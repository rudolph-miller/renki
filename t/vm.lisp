(in-package :cl-user)
(defpackage renki-test.vm
  (:use :cl
        :renki.vm
        :prove)
  (:shadowing-import-from :renki.vm
                          :run))
(in-package :renki-test.vm)

(plan nil)

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

(subtest "*current-line*"
  (let ((first (make-empty-inst))
        (second (make-empty-inst)))
    (is (- (inst-line second) (inst-line first))
        1
        "can increment *current-line*.")))

(subtest "inst-list-array"
  (is-type (inst-list-array (list (make-empty-inst) (make-empty-inst)))
           'array
           "can convert list to array."))

(subtest "curret-char"
  (with-target-string "a"
    (let ((*sp* 0))
      (is (current-char)
          #\a
          "can return the character *sp* indicates."))))

(subtest "curret-inst"
  (let ((*insts* (inst-list-array (list (make-empty-inst))))
        (*pc* 0))
    (is-type (current-inst)
             '<empty>
             "can returt the inst *pc* indicates.")))

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
          :test #'equalp))))

(subtest "run"
  (subtest ":match"
    (ok (run (list (make-match-inst)) "")
        "can return T."))

  (subtest ":fail"
    (is (run (list (make-char-inst #\b) (make-char-inst #\a)) "a")
        nil
        "can return NIL when *queue* is empty.")

    (let ((split (make-split-inst)))
      (setf (inst-to1 split) 1)
      (setf (inst-to2 split) 2)
      (is (run (list split (make-char-inst #\b) (make-char-inst #\a) (make-match-inst)) "a")
          t
          "can continue when *queue* is not empty.")))

  (subtest ":splitted"
    (let ((*pc* 0)
          (*sp* 0)
          (*queue* 0)
          (split (make-split-inst)))
      (setf (inst-to1 split) 1)
      (setf (inst-to2 split) 2)
      (ok (run (list split (make-char-inst #\a) (make-jmp-inst 4) (make-char-inst #\b) (make-match-inst)) "a")
          "can exec the first thread.")))

  (subtest "t"
    (ok (run (list (make-char-inst #\a) (make-match-inst)) "a")
        "can continue.")))

(finalize)
