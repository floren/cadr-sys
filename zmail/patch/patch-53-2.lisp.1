;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 53.2
;;; Reason: Print correct year in Date fields.
;;; Written 12/05/83 23:16:01 by RMS,
;;; while running on Lisp Machine Eighteen from band 6
;;; with Bad Inconsistently updated System 98.4, CADR 3.1, Inconsistently updated ZMail 53.1, MIT-Specific 22.0, microcode 305, ZM MIT.



; From file MAIL.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFUN PRINT-HEADER (STREAM HEADER TYPE &AUX NAME)
  "Print a header item with field name TYPE and value HEADER, on STREAM.
TYPE controls the manner in which the printing is done,
and also appears in the output (usually)."
  (SETQ NAME (HEADER-TYPE-NAME TYPE))
  (COND ((EQ TYPE 'REDISTRIBUTED-HEADERS)
	 (COPY-INTERVAL-TO-STREAM STREAM HEADER))
	((MEMQ TYPE *ADDRESS-TYPE-HEADERS*)
	 (PRINT-ADDRESS-HEADER STREAM HEADER NAME))
	((MEMQ TYPE *DATE-TYPE-HEADERS*)
	 (MULTIPLE-VALUE-BIND (NIL MINUTES HOURS DAY MONTH YEAR
			       DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P)
	     (TIME:DECODE-UNIVERSAL-TIME HEADER)
	   (FORMAT STREAM "~A: ~A, ~D ~A ~D, ~2,'0D:~2,'0D-~A~%" NAME
		   (TIME:DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK) DAY (TIME:MONTH-STRING MONTH)
		   YEAR HOURS MINUTES
		   (TIME:TIMEZONE-STRING TIME:*TIMEZONE* DAYLIGHT-SAVINGS-P))))
	((CONSP HEADER)
	 (DO ((STRS HEADER (CDR STRS))
	      (STR)
	      (LEN (+ (STRING-LENGTH NAME) 2))
	      (COMMA-P (MEMQ TYPE *SINGLE-LINE-TYPE-HEADERS*))
	      (REFERENCE-P (MEMQ TYPE *REFERENCE-TYPE-HEADERS*))
	      (FIRST-P T NIL))
	     ((NULL STRS))
	   (SETQ STR (CAR STRS))
	   (COND (FIRST-P
		  (FUNCALL STREAM ':STRING-OUT NAME)
		  (FUNCALL STREAM ':STRING-OUT ": "))
		 (T
		  (DOTIMES (I LEN)
		    (FUNCALL STREAM ':TYO #\SP))))
	   (IF REFERENCE-P
	       (PRINT-REFERENCE STREAM STR)
	       (FUNCALL STREAM ':STRING-OUT STR))
	   (AND COMMA-P (CDR STRS)
		(FUNCALL STREAM ':TYO #/,))
	   (FUNCALL STREAM ':TYO #\CR)))
	(T
	 (FORMAT STREAM "~A: ~A~%" NAME HEADER))))

))
