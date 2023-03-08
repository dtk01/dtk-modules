;;; dtk-daily-light.el -- read the Daily Light
;;
;; Copyright (C) 2020-2022 David Thompson
;;
;; Author: David Thompson
;; Keywords: hypermedia
;; Package-Requires: ((dtk "0.2") (ts "0.3-pre")
;; Version: 0.1
;; URL: https://github.com/dtk01/dtk-daily-light.el

;;; Commentary:
;;
;; This package provides access to the text of the "Daily Light".
;;
;; To browse the current text, use `dtk-daily'.

;;; Code:

;;;; Requirements
(require 'ts)

;;;; Internal variables
(defvar dtk-daily-date
  nil
  "Date to use. A TS timestamp.")

;;;###autoload
(define-derived-mode dtk-daily-mode dtk-mode "dtk-daily"
  "Major mode for displaying dtk Daily Light text"
  (setq font-lock-defaults nil)
  (local-set-key "g" 'dtk-go-to))

(when dtk-module-map
  (setf (alist-get "Daily" dtk-module-map)
	'(:retriever dtk-daily-retrieve
	  :parser dtk-daily-parse
	  :inserter dtk-daily-insert
	  :mode dtk-daily-mode)))

;; Convenience wrapper
(defun dtk-daily ()
  "Read the Daily Light entry for today."
  (interactive)
  ;; if module isn't correctly set, take care of this now
  ;; options:
  ;; 1. set it permanently
  ;; (dtk-set-module module)
  ;; 2. set it temporarily
  (with-dtk-module "Daily"
    (setq dtk-daily-date (dtk-daily-today))
    (dtk-view-text t t
		   "Daily"; old way of setting the module
		   )))

(defun dtk-daily-retrieve (&optional destination)
  "Output to DESTINATION. DESTINATION can be a buffer, T (the current
buffer), NIL (discard output), or 0 (discard and don't wait for
program to terminate)."
  (interactive)
  (if (not (dtk-ts-date= dtk-daily-date (dtk-daily-today)))
      (if (y-or-n-p "DTK-DAILY-DATE is not set to the current date. Use current date?")
	  (setq dtk-daily-date (dtk-daily-today))))
  (let ((ts-date dtk-daily-date))
    ;; For the Daily Light, the key is of the form MM.DD
    (dtk-diatheke (format "%02d.%02d"
			  (ts-month ts-date)
			  (ts-day ts-date))
		  "Daily"
		  (or destination t)
		  :plain
		  nil)))

(defun dtk-daily-insert (parsed-content)
  ;; date as cached or default to today
  (let ((ts-date (or dtk-daily-date (dtk-daily-today))))
    (insert (ts-month-name ts-date)
	    " "
	    (int-to-string (ts-day ts-date))
	    #xa #xa))
  ;; morning
  (insert (propertize (cl-third (cl-second parsed-content))
		      'face 'font-lock-variable-name-face)
	  #xa)
  ;; morning text
  (insert (cl-second (cl-third parsed-content))
	  #xa #xa)
  ;; evening
  (insert (propertize (cl-third (cl-fourth parsed-content))
		      'face 'font-lock-variable-name-face)
	  #xa)
  ;; text
  (insert (cl-second (cl-fifth parsed-content))))

(defun dtk-daily-set-date-to-today ()
  "Set DTK-DAILY-DATE to today."
  (setq dtk-daily-date (dtk-daily-today)))

(defun dtk-daily-today ()
  "Return timestamp for today."
  (ts-now))

;; Match the first line of raw diatheke output for Daily.
;; Expect "MM.DDMM.DD: Morning: < morning text > Evening: < evening text >".
(defvar dtk-daily-single-line-regex
  (rx (group-n 1 digit digit) "." (group-n 2 digit digit)
      digit digit "." digit digit
      ": Morning" (group-n 3 ":")
      (+? anything)
      (group-n 4 "Evening:")))

;; Match
;; 1. first line of the form
;;   ": <i>Morning</i>:<br /> <br />   ... " or
;;   ": Morning: ..."
;; 2. "Evening" clause:
;;   "<i>Evening</i>:"
(setq dtk-daily-single-line-regex
      (rx (+? anything) "Morning"
	  ;(+? anything)
	  (group-n 3 ":")
	  (+? anything)
	  "Evening" (group-n 4 ":")))

;;;
;;; Parsing code for in-buffer parsing
;;;
(defun dtk-daily-parse (raw-string)
  ;; 1. look for
  ;;    - "03.2603.26: Morning:" <--
  ;;    - ": <i>Morning:</i>"    <-- $ diatheke -b Daily -o fmnx -k 10.25
  
  ;; 2. text following this up to "Evening:"
  ;; 3. "Evening:"
  ;; 4. remaining text
  (if (string-match dtk-daily-single-line-regex
		    raw-string
		    0)
      (dtk-daily-parse-handle-match raw-string)
      nil))

(defun dtk-daily-parse-handle-match (line)
  ;; look for
  ;; 1. MM.DDMM.DD: Morning
  ;;    - e.g., 10.2710.27: Morning:
  ;; 2. Evening:

  ;; first chars are MM.DDMM.DD: Morning:
  (let ((date-month (match-string 1 line))
	(date-day (match-string 2 line))
	(morning-colon-pos (match-end 3))
	(evening-colon-pos (match-end 4)))
    (let ((morning-text (string-trim
			 (substring line morning-colon-pos
				    (- evening-colon-pos 8))
			 " "))
	  (evening-text (string-trim (substring line evening-colon-pos)
				     " ")))
      (list
       ;; dayname monthname daynumeral
       (list :h 0 (if (and date-month date-day)
		      (format "%s %d"
			      ;; need year for this
			      ;;(calendar-day-name (list date-month date-day current-year))
			      (calendar-month-name (string-to-number date-month)
					;(string-to-number date-day)
					;2000 ; some-year
						   )
			      (string-to-number date-day))
		    "Date not specified"))
       (list :h 1 "Morning")
       (list :text morning-text)
       (list :h 1 "Evening")
       (list :text evening-text)))))

(defun dtk-ts-date= (ts1 ts2)
  "TS1 and TS2 are ts.el timestamps."
  (and ts1 ts2
       (= (ts-day ts1)
	  (ts-day ts2))
       (= (ts-month ts1)
	  (ts-month ts2))
       (= (ts-year ts1)
	  (ts-year ts2))))

(setf dtk-daily-date (dtk-daily-today))
