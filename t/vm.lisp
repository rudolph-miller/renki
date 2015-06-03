(in-package :cl-user)
(defpackage renki-test.vm
  (:use :cl
        :renki.vm
        :prove))
(in-package :renki-test.vm)

(plan nil)

(subtest "definst"
  (is *inst-readers*
      '(inst-table inst-to2 inst-to1 inst-to inst-char inst-line)
      "can add reader in *inst-readers*."))

(subtest "<empty>"
  (is-type (make-empty-inst)
           '<empty>
           "can make-empty-inst."))

(subtest "<match>"
  (is-type (make-match-inst)
           '<match>
           "can make-match-inst."))

(subtest "<char>"
  (let ((char (make-char-inst #\a)))
    (is-type char
             '<char>
             "can make-char-inst.")

    (is (inst-char char)
        #\a
        "can set char.")))

(subtest "<jmp>"
  (let ((jmp (make-jmp-inst 1)))
    (is-type jmp
             '<jmp>
             "can make-jmp-inst.")

    (is (inst-to jmp)
        1
        "can set to.")

    (setf (inst-to jmp) 2)

    (is (inst-to jmp)
        2
        "can setf to.")))

(subtest "<split>"
  (let ((split (make-split-inst)))
    (is-type split
             '<split>
             "can make-split-inst.")

    (setf (inst-to1 split) 1)
    (setf (inst-to2 split) 2)

    (is (inst-to1 split)
        1
        "can setf to1.")

    (is (inst-to2 split)
        2
        "can setf to2.")))

(subtest "<cond>"
  (let ((table (make-hash-table)))
    (setf (gethash #\a table) 1)
    (is-type (make-cond-inst table)
             '<cond>
             "can make-cond-inst.")

    (let ((cond (make-cond-inst table)))
      (is-type (inst-table cond)
               'hash-table
               "can setf table."))))

(subtest "*current-line*"
  (let ((first (make-empty-inst))
        (second (make-empty-inst)))
    (is (- (inst-line second) (inst-line first))
        1
        "can increment *current-line*.")))

(subtest "curret-char"
  (with-target-string "a"
    (let ((*sp* 0))
      (is (current-char)
          #\a
          "can return the character *sp* indicates."))))

(subtest "curret-inst"
  (let ((*insts* (make-array 1 :initial-contents (list (make-empty-inst))))
        (*pc* 0))
    (is-type (current-inst)
             '<empty>
             "can returt the inst *pc* indicates.")))

(subtest "expand-inst"
  (let* ((form '((inst-char char))))
    (is (expand-inst form)
        (list (sb-impl::unquote '(inst-char char)))
        "can unquote."
        :test #'equalp)))

(subtest "defexec"
  (let ((empty (make-empty-inst)))
    (is (funcall (gethash (find-class '<empty>) *exec-table*) empty)
        `((renki.vm::next-line ,(inst-line empty)))
        "can setf *exec-table*.")))

(subtest "with-target-string"
  (with-target-string "ab"
    (is *target*
        "ab"
        "can let *target*.")

    (is *target-length*
        2
        "can let *target-length*.")

    (is *sp*
        0
        "can let *sp*.")))

(subtest "compile-insts"
  (let* ((*current-line* 0)
         (insts (list (make-char-inst #\a) (make-match-inst)))
         (fn (compile-insts insts)))
    (is-type fn
             'function
             "can return function.")

    (is (list (funcall fn "a") (funcall fn "b"))
        (list t nil)
        "can compile insts.")))

(subtest "exec"
  (subtest "<empty>"
    (let ((*pc* 0))
      (exec (make-empty-inst))

      (is *pc*
          1
          "can increment *pc*.")))

  (subtest "<match>"
    (is (exec (make-match-inst))
        :match
        "can returt :match."))

  (subtest "<char>"
    (with-target-string "ab"
      (let ((*pc* 0)
            (*sp* 0)
            (*queue* nil))
        (ok (exec (make-char-inst #\a))
            "can returt T when matching succeeded.")

        (is *sp*
            1
            "can increment *sp*.")

        (is *pc*
            1
            "can increment *pc*.")

        (is (exec (make-char-inst #\a))
            :fail
            "can return :fail when matching failed."))))

  (subtest "<jmp>"
    (let ((*pc* 0))
      (exec (make-jmp-inst 2))

      (is *pc*
          2
          "can set *pc* to inst-to of <jmp>.")))

  (subtest "<split>"
    (let ((*queue* nil)
          (*pc* 0)
          (*sp* 0)
          (split (make-split-inst)))
      (setf (inst-to1 split) 2)
      (setf (inst-to2 split) 3)
      (exec split)

      (is (length *queue*)
          2
          "can push 2 threads.")

      (is (car *queue*)
          (make-thread :pc 2 :sp 0)
          "can save to1 and *sp*."
          :test #'equalp)

      (is (cadr *queue*)
          (make-thread :pc 3 :sp 0)
          "can save to2 and *sp*."
          :test #'equalp)))

  (subtest "<cond>"
    (let ((table (make-hash-table)))
      (setf (gethash #\a table) 1)
      (setf (gethash #\b table) 3)
      (let ((cond (make-cond-inst table)))
        (with-target-string "abc"
          (exec cond)

          (is *sp*
              1
              "can incf *sp*.")

          (is *pc*
              1
              "can setq *pc*.")

          (exec cond)

          (is *pc*
              3
              "can dispatch with character.")

          (is (exec cond)
              :fail
              "can return fail with character not on table."))))))

(subtest "run-vm"
  (subtest ":match"
    (ok (run-vm (list (make-match-inst)) "")
        "can return T."))

  (subtest ":fail"
    (is (run-vm (list (make-char-inst #\b) (make-char-inst #\a)) "a")
        nil
        "can return NIL when *queue* is empty.")

    (let ((split (make-split-inst)))
      (setf (inst-to1 split) 1)
      (setf (inst-to2 split) 2)
      (is (run-vm (list split (make-char-inst #\b) (make-char-inst #\a) (make-match-inst)) "a")
          t
          "can continue when *queue* is not empty.")))

  (subtest ":splitted"
    (let ((*pc* 0)
          (*sp* 0)
          (*queue* 0)
          (split (make-split-inst)))
      (setf (inst-to1 split) 1)
      (setf (inst-to2 split) 2)
      (ok (run-vm (list split (make-char-inst #\a) (make-jmp-inst 4) (make-char-inst #\b) (make-match-inst)) "a")
          "can exec the first thread.")))

  (subtest "t"
    (ok (run-vm (list (make-char-inst #\a) (make-match-inst)) "a")
        "can continue.")))

(finalize)
