;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.18
;;; Reason:
;;;  Fix COM-QUICK-DOCUMENTATION (used by Control-Shift-D) to avoid going
;;;  into the error handler when RELEVANT-FUNCTION-NAME returns a symbol that
;;;  is not a defined function.  Also, clean up logic and documentation.
;;; Written 16-Aug-88 21:46:03 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with Experimental System 126.53, ZWEI 125.17, ZMail 73.2, Local-File 75.2, File-Server 24.1, Tape 24.2, Lambda-Diag 17.0, Experimental Unix-Interface 14.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; COMF.LISP#111 at 16-Aug-88 21:46:04
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-QUICK-DOCUMENTATION "Prints documentation for the function which point is inside a call to.
At the top level of a buffer, prints documentation for the top-level form being defined.
With a numeric argument, reads the name of the function to be documented
from the mini-buffer." ()
    (LET ((NAME (RELEVANT-FUNCTION-NAME (POINT))))
      (IF *NUMERIC-ARG-P*
	  (SETQ NAME (READ-FUNCTION-NAME "Document" NAME T)))
      (IF (NULL NAME) (BARF "No function at point.")
	(flet ((print-args (stream)
		(IF (OR (FDEFINEDP NAME) (AND (SYMBOLP NAME) (FUNCTIONP NAME T)))
		    (PROGN (SEND stream :FRESH-LINE)
			   (PRINT-ARGLIST NAME stream)))))
	  (LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	    (COND ((NULL DOC)
		   (print-args *query-io*)
		   (FORMAT *QUERY-IO* "~&~S is not documented as a function." NAME))
		  (T
		   (print-args *standard-output*)
		   (FORMAT T "~%~A" DOC)))))))
    DIS-NONE)

))
