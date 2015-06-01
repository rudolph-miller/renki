#|
  This file is a part of renki project.
  Copyright (c) 2015 Rudolph-Miller
|#

(in-package :cl-user)
(defpackage renki-test-asd
  (:use :cl :asdf))
(in-package :renki-test-asd)

(defsystem renki-test
  :author "Rudolph-Miller"
  :license "MIT"
  :description "Tests of Renki."
  :depends-on (:renki
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "ast")
                 (:test-file "parser")
                 (:test-file "vm")
                 (:test-file "nfa")
                 (:test-file "compiler")
                 (:test-file "renki"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
