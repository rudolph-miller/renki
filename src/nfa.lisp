(in-package :cl-user)
(defpackage renki.nfa
  (:use :cl)
  (:export :<nfa>
           :<state>
           :<transition>
           :nfa-initial
           :nfa-accepting
           :nfa-transitions
           :transition-from
           :transition-to
           :transition-char
           :make-nfa
           :make-state
           :make-transition
           :expand-epsilon
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

(defun make-nfa (initial accepting transitions)
  (make-instance '<nfa> :initial initial :accepting accepting :transitions transitions))

(defun make-state ()
  (make-instance '<state>))

(defun make-transition (from to &optional char)
  (make-instance '<transition> :from from :to to :char char))

(defun expand-epsilon (nfa)
  (let* ((initial (nfa-initial nfa))
         (accepting (nfa-accepting nfa))
         (transitions (nfa-transitions nfa))
         (epsilons (remove-if #'(lambda (transition)
                                  (transition-char transition))
                              transitions))
         (result nil))
    (labels ((find-epsilon (state)
               (let ((result (find state epsilons :test #'(lambda (state transition) (eql state (transition-from transition))))))
                 (when result
                   (setf epsilons (remove result epsilons))
                   (let ((next (find-epsilon (transition-to result))))
                     (if next next result))))))
      (dolist (transition transitions)
        (when (transition-char transition)
          (push transition result)
          (let ((found (find-epsilon (transition-to transition))))
            (when found
              (push (make-transition (transition-from transition) (transition-to found) (transition-char transition))
                    result)))))
      (make-nfa initial accepting (append epsilons result)))))

(defun run-nfa (nfa string)
  (let ((accepting (nfa-accepting nfa))
        (transitions (nfa-transitions nfa))
        (index 0)
        (length (length string)))
    (labels ((next-char ()
               (let ((next (incf index)))
                 (when (< next length)
                   (elt string next))))
             (sub (state char)
               (dolist (transition transitions)
                 (when (eql state (transition-from transition))
                   (if (transition-char transition)
                       (when (eql (transition-char transition) char)
                         (if (eql (transition-to transition) accepting)
                             (return-from run-nfa t)
                             (sub (transition-to transition) (next-char))))
                       (if (eql (transition-to transition) accepting)
                           (return-from run-nfa t)
                           (sub (transition-to transition) char)))))))
      (sub (nfa-initial nfa) (elt string 0)))))
