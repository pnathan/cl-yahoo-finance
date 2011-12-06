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
   :read-historical-data))
(in-package :cl-yahoo-finance)

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
  (reduce #'(lambda (a b)
	    (concatenate 'string a b))
	  strings))

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
  (let ((quoted-symbols
	 (reduce #'(lambda (a b)	;join
		     (strcat a  ", " b))
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
       "&format=json&diagnostics=true&env=store://datatables.org/alltableswithkeys"))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yason-stock-quotes-parse (quote-string)
  (gethash
   "quote"
   (gethash
    "results"
    (gethash
     "query"
     (yason:parse quote-string)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes of operation
(defparameter historical-modes
  '((daily . "d")
    (weekly ."w")
    (monthly . "m")
    (dividends_only . "v"))
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
(defun read-historical-data (symbol-string start-date end-date)
  "Start and end dates are 3-element lists mm/dd/yy
Returns a list of lists, ie, csv. Headers are, in order:
Date Open High Low Close Volume Adj Close"
  (let ((rows
	 (cl-csv:read-csv
	   (drakma:http-request
	    (strcat
	     "http://ichart.finance.yahoo.com/table.csv?s="
	     symbol-string
	     "&d="
	     (to-s (1- (first end-date)))
	     "&e="
	     (to-s (second end-date))
	     "&f="
	     (to-s (third end-date))
	     "&g="
	     (cdr (assoc 'daily historical-modes))
	     "&a="
	     (to-s (1- (first start-date)))
	     "&b="
	     (to-s (second start-date))
	     "&c="
	     (to-s (third start-date))
	     "&ignore=.csv")))))
    ;;Parse the numbers
    (loop for row in rows
       collect
	 (cons (car row) 
	       (mapcar #'safely-read-from-string
		       (rest row))))))
