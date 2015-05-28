(in-package :cl-user)
(defpackage renki-test.ast
  (:use :cl
        :prove
        :renki.ast))
(in-package :renki-test.ast)

(plan nil)

(subtest "<symbol>"
  (let ((sym (make-sym #\a)))
    (is-type sym
             '<symbol>
             "can make-sym.")

    (is (reg-char sym)
        #\a
        "can set char.")))

(subtest "<sequence>"
  (let ((seq (make-seq (make-sym #\a) (make-sym #\b))))
    (is-type seq
             '<sequence>
             "can make-seq.")

    (is-type (reg-lh seq)
             '<symbol>
             "can set lh.")

    (is-type (reg-rh seq)
             '<symbol>
             "can set rh.")))

(subtest "<alternative>"
  (let ((alt (make-alt (make-sym #\a) (make-sym #\b))))
    (is-type alt
             '<alternative>
             "can make-alt.")

    (is-type (reg-lh alt)
             '<symbol>
             "can set lh.")

    (is-type (reg-rh alt)
             '<symbol>
             "can set rh.")))

(subtest "<kleene>"
  (let ((kleene (make-kleene (make-sym #\a))))
    (is-type kleene
             '<kleene>
             "can make-kleene.")

    (is-type (reg-operand kleene)
             '<symbol>
             "can set operand.")))

(subtest "<group>"
  (let ((group (make-group (make-sym #\a))))
    (is-type group
             '<group>
             "can make-group.")

    (is-type (reg-operand group)
             '<symbol>
             "can set operand.")))

(finalize)
