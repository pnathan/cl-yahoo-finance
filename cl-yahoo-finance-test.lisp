;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-yahoo-finance unit tests
;;;;
;;;; License: LLGPL
;;;;
;;;; These tests do not test the Yahoo connection, but they test what
;;;; the Yahoo connection returns and how that data is manipulated and
;;;; shaped as it goes through cl-y-f.
;;;;
;;;; This also needs to be configured to work correctly with
;;;; cl-test-grid.



;; My preferred test framework
(ql:quickload :fiveam)

(defpackage :cl-yahoo-finance-test
  (:use :common-lisp
        :cl-yahoo-finance
        :fiveam))

(in-package :cl-yahoo-finance-test)


(def-suite cl-yahoo-finance-tests
    :description "CL-Y-F Tests")

(in-suite cl-yahoo-finance-tests)

;; just a stub
(test stub
  (is (eq t t)))

(defun run-tests ()
  (run! 'cl-yahoo-finance-tests))
