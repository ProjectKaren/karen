(in-package :cl-user)
(defpackage karen-test
  (:use :cl
        :karen
        :prove))
(in-package :karen-test)

;; NOTE: To run this test file, execute `(asdf:test-system :karen)' in your Lisp.

(plan nil)

(defvar +test-html+
  '(:html nil (
    (:head nil (
      (:meta ((:charset . "utf-8")) nil)
      (:title nil ("Document"))
    ))
    (:body nil (
      (:h1 nil ("Hello world!"))
    ))
  ))
)

(defparameter *test-element* (make-element +test-html+))
(diag "Testing Gtk main...")
(isnt (main *test-element*) nil)

(finalize)
