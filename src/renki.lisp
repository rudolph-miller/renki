(in-package :cl-user)
(defpackage renki
  (:use :cl
        :renki.parser
        :renki.compiler
        :renki.vm)
  (:export :create-scanner
           :test))
(in-package :renki)

(defun create-scanner (regex)
  (let ((insts (compile-to-bytecode (parse-string regex))))
    (make-array (length insts) :initial-contents insts)))

(defgeneric test (regex string)
  (:method ((regex string) string)
    (test (compile-to-bytecode (parse-string regex))
          string))

  (:method ((regex list) string)
    (run-vm regex string))

  (:method ((regex array) string)
    (run-vm regex string)))
