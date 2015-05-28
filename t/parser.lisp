(in-package :cl-user)
(defpackage renki-test.parser
  (:use :cl
        :prove
        :renki.ast
        :renki.parser))
(in-package :renki-test.parser)

(plan nil)

(subtest "symbol"
  (let ((symbol (parse-string "a")))
    (is-type symbol
             '<symbol>
             "can make <symbol>.")

    (is (reg-char symbol)
        #\a
        "can set char.")))

(subtest "sequence"
  (let ((sequence (parse-string "ab")))
    (is-type sequence
             '<sequence>
             "can make <sequence>.")

    (is-type (reg-lh sequence)
             '<symbol>
             "can set lh.")

    (is-type (reg-rh sequence)
             '<symbol>
             "can set rh.")))

(subtest "alternative"
  (let ((alternative (parse-string "a|b")))
    (is-type alternative
             '<alternative>
             "can make <alternative>.")

    (is-type (reg-lh alternative)
             '<symbol>
             "can set lh.")

    (is-type (reg-rh alternative)
             '<symbol>
             "can set rh.")))

(subtest "kleene"
  (let ((kleene (parse-string "a*")))
    (is-type kleene
             '<kleene>
             "can make <kleene>.")

    (is-type (reg-operand kleene)
             '<symbol>
             "can set operand.")))

(subtest "group"
  (let ((group (parse-string "(a)")))
    (is-type group
             '<group>
             "can make <group>.")

    (is-type (reg-operand group)
             '<symbol>
             "can set operand.")))

(subtest "Priority"
  (is-type (parse-string "(a|b)*")
           '<kleene>
           "<group> > <kleene>.")

  (is-type (parse-string "ab*")
           '<sequence>
           "<kleene> > <sequence>.")

  (is-type (parse-string "a|b*")
           '<alternative>
           "<kleene> > <alternative>.")

  (is-type (parse-string "a|bc")
           '<alternative>
           "<sequence> > <alternative>."))

(finalize)

