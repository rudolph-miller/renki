(in-package :cl-user)
(defpackage renki.compiler
  (:use :cl
        :renki.ast
        :renki.vm
        :renki.nfa)
  (:export :compile-to-bytecode
           :compile-to-nfa))
(in-package :renki.compiler)

(defparameter *line-inited* nil)

(defgeneric compile-to-bytecode (obj))

(defmethod compile-to-bytecode :around (obj)
  (if *line-inited*
      (call-next-method)
      (let ((*line-inited* t)
            (*current-line* 0))
        (append (call-next-method)
                (list (make-match-inst))))))

(defmethod compile-to-bytecode ((obj <symbol>))
  (list (make-char-inst (reg-char obj))))

(defmethod compile-to-bytecode ((obj <sequence>))
  (append (compile-to-bytecode (reg-lh obj))
          (compile-to-bytecode (reg-rh obj))))

(defmethod compile-to-bytecode ((obj <alternative>))
  (let ((split (make-split-inst))
        (lh (compile-to-bytecode (reg-lh obj)))
        (jmp (make-jmp-inst))
        (rh (compile-to-bytecode (reg-rh obj)))
        (end (make-empty-inst)))
    (setf (inst-to jmp) (inst-line end))
    (setf (inst-to1 split) (inst-line (car lh)))
    (setf (inst-to2 split) (inst-line (car rh)))
    (append (list split)
            lh
            (list jmp)
            rh
            (list end))))

(defmethod compile-to-bytecode ((obj <kleene>))
  (let* ((split (make-split-inst))
         (operand (compile-to-bytecode (reg-operand obj)))
         (jmp (make-jmp-inst (inst-line split)))
         (end (make-empty-inst)))
    (setf (inst-to1 split) (inst-line (car operand)))
    (setf (inst-to2 split) (inst-line end))
    (append (list split)
            operand
            (list jmp end))))

(defmethod compile-to-bytecode ((obj <group>))
  (compile-to-bytecode (reg-operand obj)))

(defgeneric compile-to-nfa (obj))

(defmethod compile-to-nfa ((obj <symbol>))
  (let* ((initial (make-state))
         (accepting (make-state))
         (transition (make-transition initial accepting (reg-char obj))))
    (make-nfa initial accepting (list transition))))

(defmethod compile-to-nfa ((obj <sequence>))
  (let* ((lh (compile-to-nfa (reg-lh obj)))
         (rh (compile-to-nfa (reg-rh obj)))
         (initial (nfa-initial lh))
         (accepting (nfa-accepting rh))
         (connecting-transition (make-transition (nfa-accepting lh) (nfa-initial rh)))
         (transitions (append (nfa-transitions lh)
                              (nfa-transitions rh)
                              (list connecting-transition))))
    (make-nfa initial accepting transitions)))

(defmethod compile-to-nfa ((obj <alternative>))
  (let* ((lh (compile-to-nfa (reg-lh obj)))
         (rh (compile-to-nfa (reg-rh obj)))
         (initial (make-state))
         (accepting (make-state))
         (connecting-transitions (list (make-transition initial (nfa-initial lh))
                                       (make-transition initial (nfa-initial rh))
                                       (make-transition (nfa-accepting lh) accepting)
                                       (make-transition (nfa-accepting rh) accepting)))
         (transitions (append (nfa-transitions lh)
                              (nfa-transitions rh)
                              connecting-transitions)))
    (make-nfa initial accepting transitions)))

(defmethod compile-to-nfa ((obj <kleene>))
  (let* ((operand (compile-to-nfa (reg-operand obj)))
         (initial (make-state))
         (accepting (make-state))
         (connecting-transitions (list (make-transition initial accepting)
                                       (make-transition (nfa-accepting operand) (nfa-initial operand))
                                       (make-transition initial (nfa-initial operand))
                                       (make-transition (nfa-accepting operand) accepting)))
         (transitions (append (nfa-transitions operand)
                              connecting-transitions)))
    (make-nfa initial accepting transitions)))

(defmethod compile-to-nfa ((obj <group>))
  (compile-to-nfa (reg-operand obj)))
