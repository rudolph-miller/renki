(in-package :cl-user)
(defpackage renki.ast
  (:use :cl)
  (:export :<regexp>
           :<symbol>
           :<sequence>
           :<alternative>
           :<kleene>
           :<group>
           :reg-operand
           :reg-lh
           :reg-rh
           :reg-char
           :make-sym
           :make-seq
           :make-alt
           :make-kleene
           :make-group))
(in-package :renki.ast)

(defclass <regexp> () ())

(defclass uniop (<regexp>)
  ((operand :initarg :operand
            :type <regexp>
            :reader reg-operand)))

(defclass binop (<regexp>)
  ((lh :initarg :lh
       :type <regexp>
       :reader reg-lh)
   (rh :initarg :rh
       :type <regexp>
       :reader reg-rh)))

(defclass <symbol> (<regexp>)
  ((char :initarg :char
         :type character
         :reader reg-char)))

(defclass <sequence> (binop) ())

(defclass <alternative> (binop) ())

(defclass <kleene> (uniop) ())

(defclass <group> (uniop) ())

(defun make-sym (char)
  (make-instance '<symbol> :char char))

(defun make-seq (lh rh)
  (make-instance '<sequence> :lh lh :rh rh))

(defun make-alt (lh rh)
  (make-instance '<alternative> :lh lh :rh rh))

(defun make-kleene (operand)
  (make-instance '<kleene> :operand operand))

(defun make-group (operand)
  (make-instance '<group> :operand operand))
