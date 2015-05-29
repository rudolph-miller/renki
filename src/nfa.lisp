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
           :run-nfa))
(in-package :renki.nfa)

(defclass <nfa> ()
  ((initial :initarg :initial
            :reader nfa-initial)
   (accepting :initarg :accepting
              :reader nfa-accepting)
   (transitions :initarg :transitions
                :reader nfa-transitions)))

(defclass <state> () ())

(defclass <transition> ()
  ((from :initarg :from
         :reader transition-from)
   (to :initarg :to
       :reader transition-to)
   (char :initarg :char
         :initform nil
         :reader transition-char)))

(defun make-nfa (initial accepting transitions)
  (make-instance '<nfa> :initial initial :accepting accepting :transitions transitions))

(defun make-state ()
  (make-instance '<state>))

(defun make-transition (from to &optional char)
  (make-instance '<transition> :from from :to to :char char))

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
               (loop for transition in transitions
                     do (when (eql state (transition-from transition))
                          (if (transition-char transition)
                              (when (eql (transition-char transition) char)
                                (if (eql (transition-to transition) accepting)
                                    (return-from run-nfa t)
                                    (sub (transition-to transition) (next-char))))
                              (if (eql (transition-to transition) accepting)
                                  (return-from run-nfa t)
                                  (sub (transition-to transition) char)))))))
      (sub (nfa-initial nfa) (elt string 0)))))
