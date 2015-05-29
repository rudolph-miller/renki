(in-package :cl-user)
(defpackage renki-test
  (:use :cl
        :renki
        :renki.vm
        :prove))
(in-package :renki-test)

(plan nil)

(subtest "create-scanner"
  (let ((scanner (create-scanner "a|b")))
    (is-type scanner
             'array
             "can return an array.")

    (is-type (elt scanner 0)
             '<inst>
             "can return <inst> in array.")))

(macrolet ((with-test (fn &body body)
             `(flet ((test (regex string)
                       (funcall (symbol-function ,fn) regex string)))
                (subtest (format nil "~a" ,fn)
                  ,@body))))
  (dolist (test (list 'test-vm 'test-nfa))
    (with-test test
      (subtest "character"
        (ok (test "a" "a")
            "T.")

        (is (test "b" "a")
            nil
            "NIL."))

      (subtest "sequence"
        (ok (test "ab" "ab")
            "T.")

        (is (test "ab" "aa")
            nil
            "NIL."))

      (subtest "alternative"
        (ok (test "a|b" "b")
            "T.")

        (is (test "a|b" "c")
            nil
            "NIL."))

      (subtest "kleene"
        (ok (test "ba*" "b")
            "T with Epsilon.")
        
        (ok (test "a*" "aaa")
            "T with repetition."))

      (subtest "group"
        (ok (test "ab|cd" "ab")
            "without grouping.")

        (is (test "a(b|c)d" "ab")
            nil
            "with grouping.")))))

(finalize)
