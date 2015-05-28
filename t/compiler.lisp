(in-package :cl-user)
(defpackage renki-test.compiler
  (:use :cl
        :renki.ast
        :renki.parser
        :renki.vm
        :renki.compiler
        :prove)
  (:shadowing-import-from :renki.vm
                          :run))
(in-package :renki-test.compiler)

(plan nil)

(subtest "compile-to-bytecode"
  (subtest "<symbol>"
    (let ((char (car (compile-to-bytecode (parse-string "a")))))
      (is-type char
               '<char>
               "can compile to <char>.")

      (is (inst-char char)
          #\a
          "can set char.")))

  (subtest "<sequence>"
    (let ((list (compile-to-bytecode (parse-string "ab"))))
      (is-type (car list)
               '<char>
               "can compile lh.")

      (is-type (cadr list)
               '<char>
               "can compile rh.")))

  (subtest "<alternative>"
    (let ((list (compile-to-bytecode (parse-string "a|b"))))
      (is-type (nth 0 list)
               '<split>
               "can prepend <split>.")

      (is (list (inst-to1 (nth 0 list))
                (inst-to2 (nth 0 list)))
          (list 1 3)
          "can set to1 and to2 of <split>.")

      (is-type (nth 1 list)
               '<char>
               "can compile lh.")

      (is-type (nth 2 list)
               '<jmp>
               "can insert <jmp> between lh and rh.")

      (is (inst-to (nth 2 list))
          4
          "can set to of <jmp>.")

      (is-type (nth 3 list)
               '<char>
               "can compile rh.")

      (is-type (nth 4 list)
               '<empty>
               "can append <empty>.")))

  (subtest "<kleene>"
    (let ((list (compile-to-bytecode (parse-string "a*"))))
      (is-type (nth 0 list)
               '<split>
               "can prepend <split>.")

      (is (list (inst-to1 (nth 0 list))
                (inst-to2 (nth 0 list)))
          (list 1 3)
          "can set to1 and to2 of <split>.")

      (is-type (nth 1 list)
               '<char>
               "can compile operand.")

      (is-type (nth 2 list)
               '<jmp>
               "can append <jmp>.")

      (is (inst-to (nth 2 list))
          0
          "can set to of <jmp>.")

      (is-type (nth 3 list)
               '<empty>
               "can append <empty>.")))

  (subtest "<group>"
    (is-type (car (compile-to-bytecode (parse-string "(a)")))
             '<char>
             "can compile."))

  (subtest "init line"
    (compile-to-bytecode (parse-string "a"))

    (is (inst-line (car (compile-to-bytecode (parse-string "a"))))
        0
        "can init *current-line*."))

  (subtest "<match>"
    (is-type (nth 1 (compile-to-bytecode (parse-string "a")))
             '<match>
             "can append <match>.")))

(finalize)
