(in-package :cl-user)
(defpackage renki
  (:use :cl
        :renki.parser
        :renki.compiler
        :renki.vm)
  (:export :test))
(in-package :renki)

(defun test (regex string)
  (run (compile-to-bytecode (parse-string regex))
       string))
