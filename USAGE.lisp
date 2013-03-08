;;; This is how to use cl-yahoo-finance.


;; Load me up
(ql:quickload :cl-yahoo-finance)

;; Pull in the complete details for the big G and big Blue
(cl-yahoo-finance:read-current-data '("GOOG" "IBM"))

;; Get the historical data from TSLA
(cl-yahoo-finance:read-historical-data "TSLA" '(1 1 2011) '(6 1 2011))
