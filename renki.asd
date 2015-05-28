#|
  This file is a part of renki project.
  Copyright (c) 2015 Rudolph-Miller
|#

#|
  Regexp Engine.

  Author: Rudolph-Miller
|#

(in-package :cl-user)
(defpackage renki-asd
  (:use :cl :asdf))
(in-package :renki-asd)

(defsystem renki
  :version "0.1"
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:esrap)
  :components ((:module "src"
                :components
                ((:file "ast")
                 (:file "parser")
                 (:file "vm")
                 (:file "compiler")
                 (:file "renki"))))
  :description "Regexp Engine."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op renki-test))))
