;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-yahoo-finance
;;;; Obtains yahoo's finance information and presents the information as a hash table.
;;;; author: Paul Nathan
;;;;
;;;; Licence LLGPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :cl-yahoo-finance
  (:use :common-lisp)
  (:export
   :read-current-symbols
   :read-historical-data))
(in-package :cl-yahoo-finance)

(ql:quickload :cl-csv)
(ql:quickload :drakma)
(ql:quickload :babel)

;; column-name . yahoo-identifier
;; yahoo uses these identifiers as ways to parse out meaning. 
(defparameter columns
  '((ask . "a")
    (average_daily_volume . "a2")
    (ask_size . "a5")
    (bid . "b")
    (ask_real_time . "b2")
    (bid_real_time . "b3")
    (book_value . "b4")
    (bid_size . "b6")
    (chance_and_percent_change . "c")
    (change . "c1")
    (comission . "c3")
    (change_real_time . "c6")
    (after_hours_change_real_time . "c8")
    (dividend_per_share . "d")
    (last_trade_date . "d1")
    (trade_date . "d2")
    (earnings_per_share . "e")
    (error_indicator . "e1")
    (eps_estimate_current_year . "e7")
    (eps_estimate_next_year . "e8")
    (eps_estimate_next_quarter . "e9") 
    (float_shares . "f6") 
    (low . "g")
    (high . "h")
    (low_52_weeks . "j")
    (high_52_weeks . "k")
    (holdings_gain_percent . "g1")
    (annualized_gain . "g3")
    (holdings_gain . "g4")
    (holdings_gain_percent_realtime . "g5")
    (holdings_gain_realtime . "g6")
    (more_info . "i")
    (order_book . "i5") 
    (market_capitalization . "j1")
    (market_cap_realtime . "j3") 
    (ebitda . "j4")
    (change_From_52_week_low . "j5")
    (percent_change_from_52_week_low . "j6")
    (last_trade_realtime_withtime . "k1")
    (change_percent_realtime . "k2")
    (last_trade_size . "k3")
    (change_from_52_week_high . "k4")
    (percent_change_from_52_week_high . "k5")
    (last_trade_with_time . "l")
    (last_trade_price . "l1")
    (close . "l1")
    (high_limit . "l2")
    (low_limit . "l3")
    (days_range . "m")
    (days_range_realtime . "m2")
    (moving_average_50_day . "m3")
    (moving_average_200_day . "m4")
    (change_from_200_day_moving_average . "m5")
    (percent_change_from_200_day_moving_average . "m6")
    (change_from_50_day_moving_average . "m7")
    (percent_change_from_50_day_moving_average . "m8")
    (name . "n")
    (notes . "n4")
    (open . "o")
    (previous_close . "p")
    (price_paid . "p1")
    (change_in_percent . "p2")
    (price_per_sales . "p5")
    (price_per_book . "p6")
    (ex_dividend_date . "q")
    (pe_ratio . "p5")
    (dividend_pay_date . "r1")
    (pe_ratio_realtime . "r2")
    (peg_ratio . "r5")
    (price_eps_estimate_current_year . "r6")
    (price_eps_Estimate_next_year . "r7")
    (symbol . "s")
    (shares_owned . "s1")
    (short_ratio . "s7")
    (last_trade_time . "t1")
    ;;(trade_links . "t6")          ;; Horks up the parsing
    (ticker_trend . "t7")
    (one_year_target_price . "t8")
    (volume . "v")
    (holdings_value . "v1")
    (holdings_value_realtime . "v7")
    (weeks_range_52 . "w")
    (day_value_change . "w1")
    (day_value_change_realtime . "w4")
    (stock_exchange . "x")
    (dividend_yield . "y"))
  "This a-list serves as keys for the Yahoo stock information for a given quote")
    ;(adjusted_close . nil))) ; this one only comes in historical quotes

;; modes of operation
(defparameter historical-modes 
  '((daily . "d")
    (weekly ."w")
    (monthly . "m")
    (dividends_only . "v"))
  "Keys into historical quotes")

;; Misc utility routines
(defun concat-list(seq)
  "Concatenates a list of strings"
  (reduce #'(lambda (r s)
	      (concatenate 'string r s))
	  seq))

(defun pairup (s u)
  "s u => ((s1 . u1) (s2 . u2) ... )"
  (loop for var-s in s 
       for var-u in  u
       collect (cons var-s var-u)))
       
;; less typing
(defun to-s (thing)
  "Converts thing to a string using FORMAT"
  (format nil "~a" thing))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-current-symbols (symbol-list) 
  (let ((columnlist (concat-list 
		     (mapcar #'(lambda (pair) 
				 (cdr pair)) 
			     columns)))
	(columnnames (mapcar #'(lambda (pair) 
				 (car pair)) 
			     columns))
	;; join on + since that's how we form a request
	(gathered-symbol-list (reduce 
			       #'(lambda (a b) 
				   (concatenate 'string a "+" b))
			       symbol-list)))
    (let ((rows 
	    (cl-csv:read-csv 
	     (babel:octets-to-string 
	      (drakma:http-request
	       (concatenate 'string
			    "http://finance.yahoo.com/d/quotes.csv?s="  
			    gathered-symbol-list
			    "&f="
			    columnlist "&e=.csv"))))))
      (loop for row in rows 
	   collect (pairup columnnames row)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Historical data URL
(defun read-historical-data (symbol-string start-date end-date)
  "Start and end dates are 3-element lists mm/dd/yy "
  (drakma:http-request
   (concatenate 'string
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
		"&ignore=.csv")))
