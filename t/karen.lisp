(in-package :cl-user)
(defpackage karen-test
  (:use :cl
        :karen
        :prove))
(in-package :karen-test)

;; NOTE: To run this test file, execute `(asdf:test-system :karen)' in your Lisp.

(plan nil)

(diag "Testing Gtk main...")
(isnt (main) nil)

(finalize)
