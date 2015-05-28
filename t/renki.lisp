(in-package :cl-user)
(defpackage renki-test
  (:use :cl
        :renki
        :prove))
(in-package :renki-test)

(plan nil)

(subtest "test"
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
        "T with Ypsilon.")
    
    (ok (test "a*" "aaa")
        "T with repetition."))

  (subtest "group"
    (ok (test "c(a|b)*c" "caabc")
        "T.")

    (skip 1 "have to test without grouping.")))

(finalize)
