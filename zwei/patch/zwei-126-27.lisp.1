;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.27
;;; Reason:
;;;  Corrects the compiler::patch-defining-file property for ZWEI:INDENT-FOR-CTS and
;;;  all of its callers...
;;; Written 25-Oct-88 16:23:32 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.131, Experimental ZWEI 126.25, Experimental ZMail 74.13, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.



; From modified file DJ: L.ZWEI; COMB.LISP#106 at 25-Oct-88 16:23:32
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

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

; From modified file DJ: L.ZWEI; COMB.LISP#106 at 25-Oct-88 16:23:38
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(defcom com-indent-for-local-modification-cts ";;; $$$ <date> <user> template" ()
  (indent-for-cts "$$$")
  dis-text)		

))

; From modified file DJ: L.ZWEI; COMB.LISP#106 at 25-Oct-88 16:23:46
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(defcom com-indent-for-systematic-modification-cts ";;; &&& <date> <user> template" ()
  (indent-for-cts "&&&")
  dis-text)		

))

; From modified file DJ: L.ZWEI; COMB.LISP#106 at 25-Oct-88 16:23:51
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(defcom com-indent-for-enhancement-suggestive-cts ";;; @@@ <date> <user> template" ()
  (indent-for-cts "@@@")
  dis-text)		

))

; From modified file DJ: L.ZWEI; COMB.LISP#106 at 25-Oct-88 16:23:55
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(defcom com-indent-for-maintenance-suggestive-cts ";;; +++ <date> <user> template" ()
  (indent-for-cts "+++")
  dis-text)		

))
