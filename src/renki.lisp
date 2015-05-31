(in-package :cl-user)
(defpackage renki
  (:use :cl
        :renki.parser
        :renki.compiler
        :renki.vm
        :renki.nfa)
  (:export :create-scanner
           :test
           :test-vm
           :test-nfa
           :test-dfa))
(in-package :renki)

(defun create-scanner (regex &optional (type :vm))
  (ecase type
    (:vm (let ((insts (compile-to-bytecode (parse-string regex))))
           (make-array (length insts) :initial-contents insts)))
    (:nfa (set-transition-table (expand-epsilon (compile-to-nfa (parse-string regex)))))
    (:dfa (set-transition-table (nfa-dfa (expand-epsilon (compile-to-nfa (parse-string regex))))))))

(defgeneric test (regex string)
  (:method ((regex string) string)
    (test-vm regex string))

  (:method ((regex array) string)
    (test-vm regex string))

  (:method ((regex <nfa>) string)
    (test-nfa regex string))

  (:method ((regex <dfa>) string)
    (test-dfa regex string)))

(defgeneric test-vm (regex string)
  (:method ((regex string) string)
    (test-vm (compile-to-bytecode (parse-string regex))
             string))

  (:method ((regex list) string)
    (run-vm regex string))

  (:method ((regex array) string)
    (run-vm regex string)))

(defgeneric test-nfa (regex string)
  (:method ((regex string) string)
    (test-nfa (expand-epsilon (compile-to-nfa (parse-string regex)))
              string))

  (:method ((regex <nfa>) string)
    (run-nfa regex string)))

(defgeneric test-dfa (regex string)
  (:method ((regex string) string)
    (test-dfa (nfa-dfa (expand-epsilon (compile-to-nfa (parse-string regex))))
              string))

  (:method ((regex <dfa>) string)
    (test-nfa regex string)))
