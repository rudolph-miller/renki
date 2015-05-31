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
           :nfa-transition-table
           :transition-from
           :transition-to
           :transition-char
           :make-nfa
           :make-state
           :make-transition
           :make-dfa
           :expand-epsilon
           :transition-table
           :set-transition-table
           :get-availabel-states
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
                :reader nfa-transitions)
   (transition-table :initform nil
                     :type hash-table
                     :accessor nfa-transition-table)))

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
               (let ((expanded nil))
                 (cond
                   ((member (transition-from epsilon) initials)
                    (setq expanded t)
                    (push (transition-to epsilon) initials))
                   (t (dolist (transition transitions)
                        (when (eql (transition-to transition) (transition-from epsilon))
                          (setq expanded t)
                          (let ((additional (make-transition (transition-from transition)
                                                             (transition-to epsilon)
                                                             (transition-char transition))))
                            (if (transition-char transition)
                                (push additional transitions)
                                (expand additional)))))))
                 (if expanded
                     (setq transitions (remove epsilon transitions))
                     (error "Could not expand epsilon.")))))
      (dolist (epsilon epsilons)
        (expand epsilon))
      (make-nfa initials acceptings transitions))))

(defun transition-table (transitions)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (transition transitions)
      (let ((state (transition-to transition))
            (key (cons (transition-from transition) (transition-char transition))))
        (push state (gethash key result))))
    result))

(defun set-transition-table (nfa)
  (setf (nfa-transition-table nfa)
        (transition-table (nfa-transitions nfa))))

(defun get-availabel-states (state char table)
  (gethash (cons state char) table))

(defun nfa-dfa (nfa)
  (let ((nfa-initials (nfa-initials nfa))
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
      (let ((initial (register-states nfa-initials)))
        (convert initial nfa-initials)
        (make-dfa (list initial) acceptings transitions)))))

(defgeneric run-nfa (nfa string))

(defmethod run-nfa ((nfa <nfa>) string)
  (let ((acceptings (nfa-acceptings nfa))
        (transition-table (or (nfa-transition-table nfa)
                              (transition-table (nfa-transitions nfa))))
        (length (length string)))
    (labels ((accept-p (state)
               (member state acceptings))
             (current-char (i)
               (when (< i length)
                 (elt string i)))
             (exec (state i)
               (when (accept-p state)
                 (return-from run-nfa t))
               (when (current-char i)
                 (dolist (state (get-availabel-states state (current-char i) transition-table))
                   (exec state (1+ i)))
                 (dolist (state (get-availabel-states state nil transition-table))
                   (exec state i)))))
      (dolist (initial (nfa-initials nfa))
        (exec initial 0)))))

(defmethod run-nfa ((dfa <dfa>) string)
  (let* ((acceptings (nfa-acceptings dfa))
         (transition-table (or (nfa-transition-table dfa)
                               (transition-table (nfa-transitions dfa))))
         (length (length string)))
    (labels ((accept-p (state)
               (member state acceptings))
             (current-char (i)
               (when (< i length)
                 (elt string i)))
             (exec (state i)
               (when (accept-p state)
                 (return-from run-nfa t))
               (when (current-char i)
                 (dolist (state (get-availabel-states state (current-char i) transition-table))
                   (exec state (1+ i)))
                 (dolist (state (get-availabel-states state nil transition-table))
                   (exec state i)))))
      (dolist (initial (nfa-initials dfa))
        (exec initial 0)))))
