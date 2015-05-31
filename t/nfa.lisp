(in-package :cl-user)
(defpackage renki-test.nfa
  (:use :cl
        :renki.parser
        :renki.compiler
        :renki.nfa
        :prove))
(in-package :renki-test.nfa)

(plan nil)

(subtest "<nfa>"
  (is-type (make-nfa (list (make-state)) (list (make-state)) (list (make-transition (make-state) (make-state))))
           '<nfa>
           "can make-nfa.")

  (let ((nfa (make-nfa (list (make-state)) (list (make-state)) (list (make-transition (make-state) (make-state))))))
    (is-type (nfa-initials nfa)
             'list
             "can set initial.")

    (is-type (nfa-acceptings nfa)
             'list
             "can set accepting.")

    (is-type (nfa-transitions nfa)
             'list
             "can set transitions.")))

(subtest "<state>"
  (is-type (make-state)
           '<state>
           "can make-state."))

(subtest "<transition>"
  (is-type (make-transition (make-state) (make-state))
           '<transition>
           "can make-transition without char.")

  (is-type (make-transition (make-state) (make-state) #\a)
           '<transition>
           "can make-transition with char.")

  (let ((transition (make-transition (make-state) (make-state) #\a)))
    (is-type (transition-from transition)
             '<state>
             "can set from.")

    (is-type (transition-to transition)
             '<state>
             "can set to.")

    (is (transition-char transition)
        #\a
        "can set char.")))

(subtest "<dfa>"
  (is-type (make-dfa (list (make-state)) (list (make-state)) (list (make-transition (make-state) (make-state))))
           '<dfa>
           "can make-dfa.")

  (let ((dfa (make-dfa (list (make-state)) (list (make-state)) (list (make-transition (make-state) (make-state))))))
    (is-type (nfa-initials dfa)
             'list
             "can set initial.")

    (is-type (nfa-acceptings dfa)
             'list
             "can set accepting.")

    (is-type (nfa-transitions dfa)
             'list
             "can set transitions.")))

(subtest "expand-epsilon"
  (flet ((count-epsilon (nfa)
           (length (remove-if #'(lambda (transition)
                                  (transition-char transition))
                              (nfa-transitions nfa)))))
    (let ((nfa (expand-epsilon (compile-to-nfa (parse-string "a|b")))))
      (is (count-epsilon nfa)
          0
          "can expand all epsilons.")))

  (let ((nfa (compile-to-nfa (parse-string "a|b"))))
    (push (make-transition (make-state) (make-state)) (slot-value nfa 'renki.nfa::transitions))
    (is-error (expand-epsilon nfa)
              'simple-error
              "can raise the error when something went wrong.")))

(subtest "transition-table"
  (let* ((nfa (compile-to-nfa (parse-string "a")))
         (transitions (nfa-transitions nfa)))
    (is-type (transition-table transitions)
             'hash-table
             "can return a hash-table.")

    (let ((table (transition-table transitions)))
      (is (hash-table-count table)
          1
          "can return a valid size table.")

      (is (gethash (cons (car (nfa-initials nfa)) #\a) table)
          (nfa-acceptings nfa)
          "can return the valid table."))))

(subtest "get-availabel-states"
  (let* ((nfa (compile-to-nfa (parse-string "a")))
         (table (transition-table (nfa-transitions nfa))))
    (is (get-availabel-states (car (nfa-initials nfa)) #\a table)
        (nfa-acceptings nfa)
        "can return the valid states.")))

(subtest "nfa-dfa"
  (is-type (nfa-dfa (expand-epsilon (compile-to-nfa (parse-string "a"))))
           '<dfa>
           "can convert <nfa> to <dfa>.")

  (let ((dfa (nfa-dfa (expand-epsilon (compile-to-nfa (parse-string "aa"))))))
    (is-type (nfa-initials dfa)
             'list
             "can set initial.")

    (is-type (nfa-acceptings dfa)
             'list
             "can set acceptings.")

    (is (length (nfa-transitions dfa))
        2
        "can set transitions.")))

(subtest "run-nfa"
  (subtest "<nfa>"
    (ok (run-nfa (compile-to-nfa (parse-string "a")) "a")
        "can return T.")

    (is (run-nfa (compile-to-nfa (parse-string "b")) "a")
        nil
        "can return NIL."))

  (subtest "<dfa>"
    (ok (run-nfa (nfa-dfa (compile-to-nfa (parse-string "a"))) "a")
        "can return T.")

    (is (run-nfa (nfa-dfa (compile-to-nfa (parse-string "b"))) "a")
        nil
        "can return NIL.")))

(finalize)
