(in-package :cl-user)
(defpackage renki.parser
  (:use :cl
        :renki.ast
        :esrap)
  (:export :parse-string))
(in-package :renki.parser)

(defrule symbol (not (or "(" ")" "*" "|"))
  (:lambda (char)
    (make-sym char)))

(defrule sequence (and regexp regexp)
  (:destructure (lh rh)
    (make-seq lh rh)))

(defrule alternative (and regexp "|" regexp)
  (:destructure (lh bar rh)
    (declare (ignore bar))
    (make-alt lh rh)))

(defrule kleene (and regexp "*")
  (:destructure (operand star)
    (declare (ignore star))
    (make-kleene operand)))

(defrule group (and "(" regexp ")")
  (:destructure (lp operand rp)
    (declare (ignore lp rp))
    (make-group operand)))

(defrule regexp (or sequence kleene group alternative symbol))

(defun parse-string (string)
  (parse 'regexp string))
