;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.26
;;; Reason:
;;;  Source File CTS inserting tools:
;;;    com-indent-for-local-modification-cts (sets up for a $$$-style comment)
;;;    com-indent-for-systematic-modification-cts (sets up for a &&&-style comment)
;;;    com-indent-for-enhancement-suggestive-cts (sets up for a @@@-style comment)
;;;    com-indent-for-maintenance-suggestive-cts (sets up for a +++-style comment)
;;;  COM-INSERT-SHORT-DATE
;;;    just inserts into the buffer "26-Aug-1988" without the time
;;;  COM-INSERT-CURRENT-USER-ID
;;;  
;;;  plus some reordering of the arguments for insert-formatted-output 
;;;  (defined in patch 126.25)...
;;; Written 25-Oct-88 12:25:25 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.131, Experimental ZWEI 126.25, Experimental ZMail 74.13, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.




; From modified file DJ: L.ZWEI; COMF.LISP#114 at 25-Oct-88 12:25:44
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(defun insert-formatted-output (printing-function-1 &optional printing-function-2)
  ;;format output of arg1 to point if numeric arg supplied, format output of arg2 if not
  (LET ((STREAM (INTERVAL-STREAM (POINT) (POINT) T)))
    (FUNCALL (IF *NUMERIC-ARG-P* printing-function-2 printing-function-1)
	     STREAM)
    (MOVE-BP (MARK) (POINT))
    (MOVE-BP (POINT) (SEND STREAM :READ-BP))))

))

; From modified file DJ: L.ZWEI; COMF.LISP#114 at 25-Oct-88 12:25:45
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(DEFCOM COM-INSERT-DATE "Print the current date into the buffer.
Calls TIME:PRINT-CURRENT-TIME, or if given an argument TIME:PRINT-CURRENT-DATE" ()
  (insert-formatted-output #'TIME:PRINT-CURRENT-TIME #'TIME:PRINT-CURRENT-DATE)
  DIS-TEXT)

))


; From modified file DJ: SAZ; TIME.LISP#1 at 25-Oct-88 12:30:03
#10R TIME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TIME")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; TIME.#"

(DEFUN PRINT-DATE-ONLY (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME (get-universal-time))
    (FORMAT STREAM "~2,'0D-~2,'0D-~2,'0D"
	    day
	    (month-string month ':short)
	    (remainder year 100.))))

))

; From modified file DJ: L.ZWEI; COMF.LISP#114 at 25-Oct-88 12:25:46
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(DEFCOM COM-INSERT-SHORT-DATE "Print the current date into the buffer.
Calls TIME:PRINT-DATE-ONLY, which prints only  the numeric date, the month name, and the year." ()
  (insert-formatted-output #'time:print-date-only #'time:print-current-time)
  DIS-TEXT)

))

; From modified file DJ: L.ZWEI; COMF.LISP#114 at 25-Oct-88 12:25:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(defcom com-insert-current-user-id "Print the currently logged in user's name into the buffer at point." ()
  (flet ((upcase-user-id-printer (stream) (format stream "~A" (string-upcase user-id)))
	 (user-id-printer (stream) (format stream "~A" user-id)))
    (insert-formatted-output #'user-id-printer #'upcase-user-id-printer))
  dis-text)

))

; From modified file DJ: SAZ; CTS-INSERTING.LISP#1 at 25-Oct-88 12:25:38
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; CTS-INSERTING.#"


(defun indent-for-cts (symbol-string &aux already-indented)	
  ;;Insert ";;; <symbol> <date> <user> template" at *comment-start*.
  (let* ((line (bp-line (point)))
						;; $$$  <25-Oct-88 saz>
	 (despaced-line (string-trim *whitespace-chars* line))
	 ;; Does this line consist solely of 1 or more tabs
	 ;; and zero or more spaces?
	 (indented-p (and (string-equal "" despaced-line)
			  (setq already-indented t)))
	 (*comment-begin* (string-append
			    (cond ((zerop (string-length line)) ";;; ")
				  (indented-p ";; ")
				  (t "; "))
			    symbol-string)))
    (if (not already-indented)
	(indent-for-comment *point* 1 t nil t)
      (INSERT (point) *COMMENT-BEGIN*))
    (com-end-of-line)				
    (let ((*last-command-char* #\Space))
      (dotimes (i 2) (com-self-insert)))				
    (let ((*last-command-char* #\<))
      (com-self-insert))
      ;; to just print out the date, not the time
    (com-insert-short-date)			
    (let ((*last-command-char* #\Space))		
      (com-self-insert))
    (com-insert-current-user-id)
    (let ((*last-command-char* #\>))		
      (com-self-insert))
    (dotimes (i 2)
      (com-backward-sexp))
    (com-backward)))				

))

; From modified file DJ: SAZ; CTS-INSERTING.LISP#1 at 25-Oct-88 12:26:58
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; CTS-INSERTING.#"

(defcom com-indent-for-local-modification-cts ";;; $$$ <date> <user> template" ()
  (indent-for-cts "$$$")
  dis-text)		

))

; From modified file DJ: SAZ; CTS-INSERTING.LISP#1 at 25-Oct-88 12:27:02
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; CTS-INSERTING.#"

(defcom com-indent-for-systematic-modification-cts ";;; &&& <date> <user> template" ()
  (indent-for-cts "&&&")
  dis-text)		

))

; From modified file DJ: SAZ; CTS-INSERTING.LISP#1 at 25-Oct-88 12:27:06
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; CTS-INSERTING.#"

(defcom com-indent-for-enhancement-suggestive-cts ";;; @@@ <date> <user> template" ()
  (indent-for-cts "@@@")
  dis-text)		

))

; From modified file DJ: SAZ; CTS-INSERTING.LISP#1 at 25-Oct-88 12:27:09
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; CTS-INSERTING.#"

(defcom com-indent-for-maintenance-suggestive-cts ";;; +++ <date> <user> template" ()
  (indent-for-cts "+++")
  dis-text)		

))
