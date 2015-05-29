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
  (is-type (make-nfa (make-state) (make-state) (list (make-transition (make-state) (make-state))))
           '<nfa>
           "can make-nfa.")

  (let ((nfa (make-nfa (make-state) (make-state) (list (make-transition (make-state) (make-state))))))
    (is-type (nfa-initial nfa)
             '<state>
             "can set initial.")

    (is-type (nfa-accepting nfa)
             '<state>
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

(subtest "expand-epsilon"
  (flet ((count-epsilon (nfa)
           (length (remove-if #'(lambda (transition)
                                  (transition-char transition))
                              (nfa-transitions nfa)))))
    (let ((nfa (compile-to-nfa (parse-string "a|b"))))
      (is (- (count-epsilon nfa) (count-epsilon (expand-epsilon nfa)))
          2
          "can remove expand-epsilon."))))

(subtest "run-nfa"
  (ok (run-nfa (compile-to-nfa (parse-string "a")) "a")
      "can return T.")

  (is (run-nfa (compile-to-nfa (parse-string "b")) "a")
      nil
      "can return NIL."))

(finalize)
