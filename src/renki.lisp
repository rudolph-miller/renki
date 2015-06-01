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
           :test-dfa
           :bench))
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


(defun bench (regex string &optional (times 10000))
  (let ((counter 0)
        (types (list :vm :nfa :dfa)))
    (macrolet ((type-bench (type)
                 (let ((scanner (gensym "scanner"))
                       (block (gensym "block")))
                   `(block ,block
                      (let ((,scanner (create-scanner regex ,type)))
                        (handler-case (test ,scanner string)
                          (error ()
                            (format t "Benchmark of :~(~a~) failed.~%" ,type)
                            (return-from ,block)))
                        (progn (format t "Benchmark of :~(~a~)~%" ,type)
                               (incf counter)
                               (time (loop repeat times
                                           do (test ,scanner string)))))))))
      (dolist (type types)
        (type-bench type))
      (when (= counter (length types)) t))))
