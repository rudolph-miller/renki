(in-package :cl-user)
(defpackage renki.vm
  (:use :cl)
  (:export :*current-line*
           :<inst>
           :<empty>
           :<match>
           :<char>
           :<jmp>
           :<split>
           :inst-line
           :inst-char
           :inst-to
           :inst-to1
           :inst-to2
           :make-empty-inst
           :make-match-inst
           :make-char-inst
           :make-jmp-inst
           :make-split-inst
           :*pc*
           :*sp*
           :*insts*
           :*target*
           :*target-length*
           :*queue*
           :current-char
           :current-inst
           :make-thread
           :exec
           :with-target-string
           :run-vm))
(in-package :renki.vm)

(defparameter *current-line* 0)

(defclass <inst> ()
  ((line :initarg :line
         :type integer
         :reader inst-line)))

(defclass <empty> (<inst>) ())

(defclass <match> (<inst>) ())

(defclass <char> (<inst>)
  ((char :initarg :char
         :type character
         :reader inst-char)))

(defclass <jmp> (<inst>)
  ((to :initarg :to
       :type (or null integer)
       :accessor inst-to)))

(defclass <split> (<inst>)
  ((to1 :initarg :to1
        :type (or null integer)
        :accessor inst-to1)
   (to2 :initarg :to2
        :type (or null integer)
        :accessor inst-to2)))

(defun make-empty-inst ()
  (make-instance '<empty>))

(defun make-match-inst ()
  (make-instance '<match>))

(defun make-char-inst (char)
  (make-instance '<char> :char char))

(defun make-jmp-inst (&optional to)
  (make-instance '<jmp> :to to))

(defun make-split-inst ()
  (make-instance '<split>))

(defmethod initialize-instance :after ((obj <inst>) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value obj 'line) *current-line*)
  (incf *current-line*))

(defparameter *pc* 0)

(defparameter *sp* nil)

(defparameter *insts* nil)

(defparameter *target* nil)

(defparameter *target-length* nil)

(defparameter *queue* nil)

(defun current-char ()
  (elt *target* *sp*))

(defun current-inst ()
  (elt *insts* *pc*))

(defstruct thread
  (pc 0 :type integer)
  (sp 0 :type integer))

(defgeneric exec (obj))

(defmethod exec ((obj <empty>))
  (declare (ignore obj))
  (incf *pc*)
  t)

(defmethod exec ((obj <match>))
  (declare (ignore obj))
  :match)

(defmethod exec ((obj <char>))
  (if (and (< *sp* *target-length*)
           (char= (inst-char obj)
                  (current-char)))
      (progn (incf *pc*)
             (incf *sp*)
             t)
      :fail))

(defmethod exec ((obj <jmp>))
  (setq *pc* (inst-to obj))
  t)

(defmethod exec ((obj <split>))
  (let ((thread1 (make-thread :pc (inst-to1 obj) :sp *sp*))
        (thread2 (make-thread :pc (inst-to2 obj) :sp *sp*)))
    (push thread2 *queue*)
    (push thread1 *queue*)
    :splitted))

(defmacro with-target-string (string &body body)
  `(let ((*target* ,string)
         (*target-length* (length ,string)))
     ,@body))

(defun run-vm (insts string)
  (macrolet ((next-thread (fn)
               `(let ((thread (pop *queue*)))
                  (setq *pc* (thread-pc thread))
                  (setq *sp* (thread-sp thread))
                  (,fn))))
    (with-target-string string
      (let ((*pc* 0)
            (*sp* 0)
            (*insts* (etypecase insts
                       (list (make-array (length insts) :initial-contents insts))
                       (array insts)))
            (*queue* nil))
        (labels ((exec-loop ()
                   (case (exec (current-inst))
                     (:match (return-from run-vm t))
                     (:fail
                      (if (null *queue*)
                          (return-from run-vm nil)
                          (next-thread exec-loop)))
                     (:splitted (next-thread exec-loop))
                     (t (exec-loop)))))
          (exec-loop))))))
