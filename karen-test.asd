#|
  This file is a part of karen project.
  Copyright (c) 2016 Tamamu
|#

(in-package :cl-user)
(defpackage karen-test-asd
  (:use :cl :asdf))
(in-package :karen-test-asd)

(defsystem karen-test
  :author "Tamamu"
  :license "LLGPL"
  :depends-on (:karen
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "karen"))))
  :description "Test system for karen"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
