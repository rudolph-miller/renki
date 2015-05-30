(in-package :cl-user)
(defpackage renki.nfa
  (:use :cl)
  (:export :<nfa>
           :<state>
           :<transition>
           :<dfa>
           :nfa-initial
           :nfa-accepting
           :nfa-transitions
           :transition-from
           :transition-to
           :transition-char
           :dfa-initial
           :dfa-acceptings
           :dfa-transitions
           :make-nfa
           :make-state
           :make-transition
           :make-dfa
           :expand-epsilon
           :nfa-dfa
           :run-nfa))
(in-package :renki.nfa)

(defclass <nfa> ()
  ((initial :initarg :initial
            :type <state>
            :reader nfa-initial)
   (accepting :initarg :accepting
              :type <state>
              :reader nfa-accepting)
   (transitions :initarg :transitions
                :type list
                :reader nfa-transitions)))

(defclass <state> () ())

(defclass <transition> ()
  ((from :initarg :from
         :type <state>
         :reader transition-from)
   (to :initarg :to
       :type <state>
       :reader transition-to)
   (char :initarg :char
         :initform nil
         :type (or null character)
         :reader transition-char)))

(defclass <dfa> ()
  ((initial :initarg :initial
            :type <state>
            :reader dfa-initial)
   (acceptings :initarg :acceptings
               :type list
               :reader dfa-acceptings)
   (transitions :initarg :transitions
                :type list
                :reader dfa-transitions)))

(defun make-nfa (initial accepting transitions)
  (make-instance '<nfa> :initial initial :accepting accepting :transitions transitions))

(defun make-state ()
  (make-instance '<state>))

(defun make-transition (from to &optional char)
  (make-instance '<transition> :from from :to to :char char))

(defun make-dfa (initial acceptings transitions)
  (make-instance '<dfa> :initial initial :acceptings acceptings :transitions transitions))

(defun expand-epsilon (nfa)
  (let* ((initial (nfa-initial nfa))
         (accepting (nfa-accepting nfa))
         (transitions (nfa-transitions nfa))
         (epsilons (remove-if #'(lambda (transition)
                                  (transition-char transition))
                              transitions)))
    (labels ((find-epsilon (state)
               (let ((result (find state epsilons
                                   :test #'(lambda (state transition)
                                             (eql state (transition-from transition))))))
                 (when result
                   (setf epsilons (remove result epsilons))
                   (let ((next (find-epsilon (transition-to result))))
                     (if next next result))))))
      (dolist (epsilon epsilons)
        (when (eql (transition-from epsilon) initial)
          (dolist (transition transitions)
            (when (eql (transition-from transition) (transition-to epsilon))
              (setf transitions (remove transition transitions))
              (push (make-transition initial (transition-to transition) (transition-char transition))
                    transitions))))
        (dolist (transition transitions)
          (when (eql (transition-to transition) (transition-from epsilon))
            (setq transitions (remove epsilon transitions))
            (let ((made (make-transition (transition-from transition) (transition-to epsilon) (transition-char transition))))
              (if (transition-char transition)
                  (push made transitions)
                  (push made epsilons))))))
      (make-nfa initial accepting transitions))))

(defun nfa-dfa (nfa)
  (let ((nfa-initial (nfa-initial nfa))
        (nfa-accepting (nfa-accepting nfa))
        (nfa-transitions (nfa-transitions nfa))
        (initial (make-state))
        (acceptings nil)
        (transitions nil))
    (labels ((add-to-map (char state map)
               (let ((found (assoc char map)))
                 (if found
                     (append (list (append found (list state))) (remove found map))
                     (append (list (list char state)) map))))
             (sub (state states)
               (let ((map nil))
                 (dolist (transition nfa-transitions)
                   (when (member (transition-from transition) states)
                     (setq map (add-to-map (transition-char transition) (transition-to transition) map))))
                 (dolist (item map)
                   (let ((to-state (make-state)))
                     (dolist (s (cdr item))
                       (dolist (trans nfa-transitions)
                         (when (eql s (transition-to trans))
                           (setf nfa-transitions (remove trans nfa-transitions)))))
                     (sub to-state (cdr item))
                     (when (member nfa-accepting (cdr item))
                       (push to-state acceptings))
                     (push (make-transition state to-state (car item)) transitions))))))
      (sub initial (list nfa-initial))
      (make-dfa initial acceptings transitions))))

(defgeneric run-nfa (nfa string))

(defmethod run-nfa ((nfa <nfa>) string)
  (run-nfa (list (nfa-initial nfa)
                 (list (nfa-accepting nfa))
                 (nfa-transitions nfa))
           string))

(defmethod run-nfa ((dfa <dfa>) string)
  (run-nfa (list (dfa-initial dfa)
                 (dfa-acceptings dfa)
                 (dfa-transitions dfa))
           string))

(defmethod run-nfa ((list list) string)
  (let ((acceptings (cadr list))
        (transitions (caddr list))
        (index 0)
        (length (length string)))
    (labels ((accept-p (state)
               (member state acceptings))
             (next-char ()
               (let ((next (incf index)))
                 (when (< next length)
                   (elt string next))))
             (sub (state char)
               (dolist (transition transitions)
                 (when (eql state (transition-from transition))
                   (if (transition-char transition)
                       (when (eql (transition-char transition) char)
                         (if (accept-p (transition-to transition))
                             (return-from run-nfa t)
                             (sub (transition-to transition) (next-char))))
                       (if (accept-p (transition-to transition))
                           (return-from run-nfa t)
                           (sub (transition-to transition) char)))))))
      (sub (car list) (elt string 0)))))
