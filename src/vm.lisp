(in-package :cl-user)
(defpackage renki.vm
  (:use :cl)
  (:import-from :alexandria
                :ensure-cons)
  (:export :*current-line*
           :*inst-readers*
           :<inst>
           :<empty>
           :<match>
           :<char>
           :<jmp>
           :<split>
           :<cond>
           :inst-line
           :inst-char
           :inst-to
           :inst-to1
           :inst-to2
           :inst-table
           :make-empty-inst
           :make-match-inst
           :make-char-inst
           :make-jmp-inst
           :make-split-inst
           :make-cond-inst
           :*pc*
           :*sp*
           :*insts*
           :*target*
           :*target-length*
           :*queue*
           :current-char
           :current-inst
           :make-thread
           :*exec-table*
           :expand-inst
           :exec
           :with-target-string
           :compile-insts
           :run-vm))
(in-package :renki.vm)

(defparameter *current-line* 0)

(defparameter *inst-readers* nil)

(defmacro definst (name inherits slots)
  `(progn
     (defclass ,name ,inherits
       ,slots)
     (loop for slot in (c2mop:class-direct-slots (find-class ',name))
           do (loop for reader in (c2mop:slot-definition-readers slot)
                    do (unless (member reader *inst-readers*)
                         (push reader *inst-readers*))))))

(definst <inst> ()
  ((line :initarg :line
         :type integer
         :reader inst-line)))

(definst <empty> (<inst>) ())

(definst <match> (<inst>) ())

(definst <char> (<inst>)
  ((char :initarg :char
         :type character
         :reader inst-char)))

(definst <jmp> (<inst>)
  ((to :initarg :to
       :type (or null integer)
       :accessor inst-to)))

(definst <split> (<inst>)
  ((to1 :initarg :to1
        :type (or null integer)
        :accessor inst-to1)
   (to2 :initarg :to2
        :type (or null integer)
        :accessor inst-to2)))

(definst <cond> (<inst>)
  ((table :initarg :table
          :type hash-table
          :reader inst-table)))

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

(defun make-cond-inst (table)
  (make-instance '<cond> :table table))

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

(defmacro next-line (int)
  (declare (ignore int))
  `(progn (incf *pc*) t))

(defmacro goto-line (int)
  `(progn (setq *pc* ,int) t))

(defmacro push-thread (line)
  `(push (make-thread :pc ,line :sp *sp*) *queue*))

(defmacro table-goto (table)
  `(let ((found (gethash (current-char) ,table)))
     (if found
         (progn (incf *sp*) (setq *pc* found))
         (fail))))

(defmacro match ()
  :match)

(defmacro fail ()
  :fail)

(defstruct thread
  (pc 0 :type integer)
  (sp 0 :type integer))

(defgeneric exec (obj))

(defparameter *exec-table* (make-hash-table :test #'equal))

(defun expand-inst (body)
  (flet ((reader-p (symbol)
           (member symbol *inst-readers*)))
    (mapcar #'(lambda (obj)
                (if (consp obj)
                    (if (reader-p (car obj))
                        (sb-impl::unquote obj)
                        (expand-inst obj))
                    obj))
            (ensure-cons body))))

(defmacro defexec (((obj class)) &body body)
  `(progn
     (setf (gethash (find-class ',class) *exec-table*)
           (lambda (inst)
             (remove-if #'(lambda (cons)
                            (and (consp cons)
                                 (eql (car cons) 'declare)))
                        (mapcar #'(lambda (list)
                                    (eval
                                     `(let ((,',obj ,inst))
                                        (declare (ignorable ,',obj))
                                        ,(list 'sb-int::quasiquote list))))
                                (expand-inst ',body)))))
     (defmethod exec ((,obj ,class))
       ,@body)))

(defexec ((obj <empty>))
  (declare (ignore obj))
  (next-line (inst-line obj)))

(defexec ((obj <match>))
  (declare (ignore obj))
  (match))

(defexec ((obj <char>))
  (if (and (< *sp* *target-length*)
           (char= (inst-char obj)
                  (current-char)))
      (progn (incf *sp*)
             (next-line (inst-line obj)))
      (fail)))

(defexec ((obj <jmp>))
  (goto-line (inst-to obj)))

(defexec ((obj <split>))
  (push-thread (inst-to2 obj))
  (push-thread (inst-to1 obj))
  :splitted)

(defexec ((obj <cond>))
  (table-goto (inst-table obj)))

(defmacro with-target-string (string &body body)
  `(let ((*target* ,string)
         (*target-length* (length ,string))
         (*sp* 0))
     ,@body))

(defun compile-insts (insts)
  (let ((insts (make-array (length insts) :initial-contents insts))
        (tag (gensym "tag")))
    (eval
     `(lambda (string)
        (macrolet ((next-line (int)
                     `(go ,(1+ int)))
                   (goto-line (int)
                     `(go ,int))
                   (push-thread (line)
                     (declare (ignore line))
                     `(error "Thread is not supported."))
                   (match ()
                     `(return-from ,',tag t))
                   (fail ()
                     `(return-from ,',tag nil)))
          (with-target-string string
            (block ,tag
              (tagbody
                 ,@(loop for i from 0
                         for inst across insts
                         nconc (cons i
                                     (funcall (gethash (class-of inst) *exec-table*) inst)))))))))))

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
