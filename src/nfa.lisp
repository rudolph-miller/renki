(in-package :cl-user)
(defpackage renki.nfa
  (:use :cl)
  (:export :<nfa>
           :<state>
           :<transition>
           :<dfa>
           :nfa-initials
           :nfa-acceptings
           :nfa-transitions
           :transition-from
           :transition-to
           :transition-char
           :make-nfa
           :make-state
           :make-transition
           :make-dfa
           :expand-epsilon
           :transition-table
           :nfa-dfa
           :run-nfa))
(in-package :renki.nfa)

(defclass <nfa> ()
  ((initials :initarg :initials
            :type list
            :reader nfa-initials)
   (acceptings :initarg :acceptings
              :type list
              :reader nfa-acceptings)
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

(defclass <dfa> (<nfa>) ())

(defun make-nfa (initials acceptings transitions)
  (make-instance '<nfa> :initials initials :acceptings acceptings :transitions transitions))

(defun make-state ()
  (make-instance '<state>))

(defun make-transition (from to &optional char)
  (make-instance '<transition> :from from :to to :char char))

(defun make-dfa (initials acceptings transitions)
  (make-instance '<dfa> :initials initials :acceptings acceptings :transitions transitions))

(defun expand-epsilon (nfa)
  (let* ((initials (nfa-initials nfa))
         (acceptings (nfa-acceptings nfa))
         (transitions (nfa-transitions nfa))
         (epsilons (remove-if #'(lambda (transition)
                                  (transition-char transition))
                              transitions)))
    (labels ((expand (epsilon)
               (dolist (transition transitions)
                 (cond
                   ((eql (transition-to transition) (transition-from epsilon))
                    (setq transitions (remove epsilon transitions))
                    (let ((additional (make-transition (transition-from transition) (transition-to epsilon) (transition-char transition))))
                      (if (transition-char transition)
                          (push additional transitions)
                          (expand additional))))))))
      (dolist (epsilon epsilons)
        (expand epsilon))
      (make-nfa initials acceptings transitions))))

(defun transition-table (transitions)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (transition transitions)
      (push (transition-to transition) (gethash (cons (transition-from transition) (transition-char transition)) result)))
    result))

(defun nfa-dfa (nfa)
  (let ((nfa-initial (car (nfa-initials nfa)))
        (nfa-accepting (car (nfa-acceptings nfa)))
        (nfa-transitions (nfa-transitions nfa))
        (acceptings nil)
        (transitions nil)
        (pairs nil))
    (labels ((add-to-map (char state map)
               (let ((found (assoc char map)))
                 (if found
                     (append (list (append found (list state))) (remove found map))
                     (append (list (list char state)) map))))
             (register-states (states)
               (let ((found (find states pairs
                                  :key #'cdr
                                  :test #'equal)))
                 (if found
                     (values (car found) nil)
                     (values (make-state) t))))
             (convert (state states)
               (let ((map nil))
                 (dolist (transition nfa-transitions)
                   (when (member (transition-from transition) states)
                     (setq map (add-to-map (transition-char transition) (transition-to transition) map))))
                 (dolist (item map)
                   (dolist (s (cdr item))
                     (dolist (trans nfa-transitions)
                       (when (eql s (transition-to trans))
                         (setf nfa-transitions (remove trans nfa-transitions)))))
                   (multiple-value-bind (to-state new-p) (register-states (cdr item))
                     (when new-p
                       (convert to-state (cdr item)))
                     (when (member nfa-accepting (cdr item))
                       (push to-state acceptings))
                     (push (make-transition state to-state (car item)) transitions))))))
      (let ((initial (register-states (list nfa-initial))))
        (convert initial (list nfa-initial))
        (make-dfa (list initial) acceptings transitions)))))

(defgeneric run-nfa (nfa string))

(defmethod run-nfa ((nfa <nfa>) string)
  (let ((acceptings (nfa-acceptings nfa))
        (transitions (nfa-transitions nfa))
        (index 0)
        (length (length string)))
    (labels ((accept-p (state)
               (member state acceptings))
             (next-char ()
               (let ((next (incf index)))
                 (when (< next length)
                   (elt string next))))
             (exec (state char)
               (dolist (transition transitions)
                 (when (eql state (transition-from transition))
                   (if (transition-char transition)
                       (when (eql (transition-char transition) char)
                         (if (accept-p (transition-to transition))
                             (return-from run-nfa t)
                             (exec (transition-to transition) (next-char))))
                       (if (accept-p (transition-to transition))
                           (return-from run-nfa t)
                           (exec (transition-to transition) char)))))))
      (dolist (initial (nfa-initials nfa))
        (exec initial (elt string 0))))))

(defmethod run-nfa ((dfa <dfa>) string)
  (let ((acceptings (nfa-acceptings dfa))
        (transitions (nfa-transitions dfa))
        (index 0)
        (length (length string)))
    (labels ((accept-p (state)
               (member state acceptings))
             (next-char ()
               (let ((next (incf index)))
                 (when (< next length)
                   (elt string next))))
             (exec (state char)
               (dolist (transition transitions)
                 (when (eql state (transition-from transition))
                   (if (transition-char transition)
                       (when (eql (transition-char transition) char)
                         (if (accept-p (transition-to transition))
                             (return-from run-nfa t)
                             (exec (transition-to transition) (next-char))))
                       (if (accept-p (transition-to transition))
                           (return-from run-nfa t)
                           (exec (transition-to transition) char)))))))
      (dolist (initial (nfa-initials dfa))
        (exec initial (elt string 0))))))
