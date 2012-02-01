;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-yahoo-finance
;;;; Obtains Yahoo's finance information and presents the information as a hash table.
;;;; author: Paul Nathan
;;;; Licence LLGPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :cl-yahoo-finance
  (:use :common-lisp)
  (:export
   :read-current-data
   :read-historical-data
   :read-historical-splits))
(in-package :cl-yahoo-finance)

;; TODOs
;; Refactor read-historical-*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc utility routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun concat-list(seq)
  "Concatenates a list of strings"
  (reduce #'(lambda (r s)
	      (concatenate 'string r s))
	  seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; less typing
(defun to-s (thing)
  "Converts `thing` to a string using FORMAT"
  (format nil "~a" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun strcat (&rest strings)
  (apply 'concatenate 'string strings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enquote-string (string)
  "Surround `string` with double-quotes, suitable for passing to other
systems."
  (strcat
   "\"" string "\""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ensure-list (thing)
  "Ensures that `thing` is a list. If it is an atom, it is wrapped in
a list"
  (if (not (listp thing))
      (list thing)
      thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-yql-stock-info (symbol-list)
  "Calls out to the YQL online API to get info on the list of stock
symbols"
  (let ((quoted-symbols
	 (format nil "~{~A~^, ~}"  ;join
		 (mapcar #'enquote-string
			 symbol-list))))
    (babel:octets-to-string
     (drakma:http-request
      (strcat
       "http://query.yahooapis.com/v1/public/yql?q="
       (url-rewrite:url-encode
	(strcat
	 "select * from yahoo.finance.quotes where symbol in ("
	 quoted-symbols
	 ")"))
       "&format=json&diagnostics=true&env=store://datatables.org/alltableswithkeys")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yason-stock-quotes-parse (quote-string)
  "Reads a JSON string assumed to be Yahoo data and returns a
hash-table of its data"
  (gethash
   "quote"
   (gethash
    "results"
    (gethash
     "query"
     (yason:parse quote-string)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes of operation
(defparameter *historical-modes*
  '(("daily" . "d")
    ("weekly" ."w")
    ("monthly" . "m")
    ("dividends_only" . "v"))
  "Keys into historical quotes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mooched from Ram Krishnan's public post on
;; lispservice.posterous.com
(defun safely-read-from-string (str &rest read-from-string-args)
  "Read an expression from the string STR, with *READ-EVAL* set to
NIL. Any unsafe expressions will be replaced by NIL in the resulting
S-Expression."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string str read-from-string-args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read the ratio for split to lisp ratio
;; (example, 3:1 as 3/1)
(defun read-ratio-to-lisp (str)
  (let ((pos (position #\: str)))
   (if pos
    (replace str "/" :start1 pos :end1 (+ 1 pos))))
  (safely-read-from-string str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-current-data (symbol-list)
  "Returns a list of hash tables"
  (let ((list-of-symbols (ensure-list symbol-list)))
     (ensure-list
      (yason-stock-quotes-parse
       (request-yql-stock-info list-of-symbols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Historical data URL
(defun read-historical-data (symbol-string start-date end-date &optional (historical-type "daily"))
  "Start and end dates are 3-element lists mm/dd/yy
  Returns a list of lists, ie, csv. Headers are, in order:
  Date Open High Low Close Volume Adj Close"
  (labels ((month (date-list) (1- (first date-list)))
           (day (date-list)   (second date-list))
           (year (date-list)  (third date-list)))

    (let* ((request-params
             ;;Params specified by Yahoo...
             '("s" "d" "e" "f" "g" "a" "b" "c" "ignore"))
           (param-values
             (mapcar
               #'to-s
               (list
                 symbol-string
                 (month end-date)
                 (day end-date)
                 (year end-date)
                 (cdr (assoc historical-type *historical-modes* :test #'string-equal))
                 (month start-date)
                 (day start-date)
                 (year start-date)
                 ".csv")))
           (rows
             (cl-csv:read-csv
               (drakma:http-request
                 "http://ichart.finance.yahoo.com/table.csv"
                 :parameters
                 (pairlis
                   request-params
                   param-values)))))
      ;;Parse the numbers
      (append (list (first rows))
              (loop for row in (rest rows)
                    collect
                    (cons (car row)
                          (mapcar #'safely-read-from-string
                                  (rest row))))))))

(defun read-historical-splits (symbol-string start-date end-date)
  "Start and end dates are 3-element lists mm/dd/yy
  Returns a list of lists, ie, csv. Headers are, in order:
  Date Split"
  (labels ((month (date-list) (1- (first date-list)))
           (day (date-list)   (second date-list))
           (year (date-list)  (third date-list)))

    (let* ((request-params
             ;;Params specified by Yahoo...
             '("s" "d" "e" "f" "g" "a" "b" "c" "ignore"))
           (param-values
             (mapcar
               #'to-s
               (list
                 symbol-string
                 (month end-date)
                 (day end-date)
                 (year end-date)
                 "v"
                 (month start-date)
                 (day start-date)
                 (year start-date)
                 ".csv")))
           (rows
             (cl-csv:read-csv
               (drakma:http-request
                 "http://ichart.finance.yahoo.com/x"
                 :parameters
                 (pairlis
                   request-params
                   param-values)))))
      ;;Parse the numbers
      (append '(("Date" "Split"))
            (delete nil
                    (loop for row in rows
                          collect
                          (if (string-equal (first row) "SPLIT")
			      (list (concatenate 'string
						 (subseq (second row) 0 4) "-"
						 (subseq (second row) 4 6) "-"
						 (subseq (second row) 6))
				    (read-ratio-to-lisp (third row)))
			      nil)))))))
