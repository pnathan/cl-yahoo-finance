(asdf:defsystem #:cl-yahoo-finance
  :depends-on ( #:drakma #:babel #:cl-csv)
  :components ((:file "cl-yahoo-finance")))
