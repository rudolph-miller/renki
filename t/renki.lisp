(in-package :cl-user)
(defpackage renki-test
  (:use :cl
        :renki
        :renki.vm
        :renki.nfa
        :prove)
  (:shadowing-import-from :renki.vm
                          :fail))
(in-package :renki-test)

(plan nil)

(subtest "create-scanner"
  (subtest ":vm"
    (let ((scanner (create-scanner "a|b")))
      (is-type scanner
               'array
               "can return an array.")

      (is-type (elt scanner 0)
               '<inst>
               "can return <inst>s in array.")

      (ok (test-vm scanner "b")
          "scanner can be used for test-vm.")))

  (subtest ":nfa"
    (is-type (create-scanner "a|b" :nfa)
             '<nfa>
             "can return <nfa>.")

    (let ((nfa (create-scanner "a|b" :nfa)))
      (is-type (nfa-initials nfa)
               'list
               "can set initials.")

      (is-type (nfa-acceptings nfa)
               'list
               "can set acceptings.")

      (is-type (nfa-transitions nfa)
               'list
               "can set transitions.")

      (is-type (nfa-transition-table nfa)
               'hash-table
               "can set transition-table.")))

  (subtest ":dfa"
    (is-type (create-scanner "a|b" :dfa)
             '<dfa>
             "can return <dfa>.")

    (let ((dfa (create-scanner "a|b" :nfa)))
      (is-type (nfa-initials dfa)
               'list
               "can set initials.")

      (is-type (nfa-acceptings dfa)
               'list
               "can set acceptings.")

      (is-type (nfa-transitions dfa)
               'list
               "can set transitions.")

      (is-type (nfa-transition-table dfa)
               'hash-table
               "can set transition-table."))))

(subtest "test"
  (subtest "string"
    (ok (test "a" "a")
        "ok."))

  (subtest "array"
    (let ((scanner (create-scanner "a")))
      (ok (test scanner "a")
          "ok.")))

  (subtest "<nfa>"
    (let ((scanner (create-scanner "a" :nfa)))
      (ok (test scanner "a")
          "ok.")))

  (subtest "<dfa>"
    (let ((scanner (create-scanner "a" :dfa)))
      (ok (test scanner "a")
          "ok."))))

(macrolet ((with-test (fn &body body)
             `(flet ((test (regex string)
                       (funcall (symbol-function ,fn) regex string)))
                (subtest (format nil "~a" ,fn)
                  ,@body))))
  (dolist (test (list 'test-vm 'test-nfa 'test-dfa))
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
            "alternative without grouping.")

        (is (test "a(b|c)d" "ab")
            nil
            "alternative with grouping.")

        (ok (test "(a|b)*" "abab")
            "kleene.")))))

(subtest "bench"
  (ok (bench "ab(c|d)*e" "abcdcdcdcde")
      "can return T."))

(finalize)
