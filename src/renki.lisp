(in-package :cl-user)
(defpackage renki
  (:use :cl
        :renki.parser
        :renki.compiler
        :renki.vm)
  (:export :match))
(in-package :renki)

(defun match (regex string)
  (run (compile-to-bytecode (parse-string regex))
       string))
