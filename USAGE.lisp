;; Load me up
(asdf:load-system :cl-yahoo-finance)

;; Pull in the complete details for the big G and big Blue
(cl-yahoo-finance:read-current-symbols '("GOOG" "IBM"))

;; Get the historical data from TSLA
(cl-yahoo-finance:read-historical-data "TSLA" '(1 1 2011) '(6 1 2011))