;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.14
;;; Reason:
;;;  Improved MAIL function to check type of the USER argument.  USER must be
;;;  a string containing <known user>@<known host>, or else gives an
;;;  informative error.  Averts the incomprehensible error message
;;;  encountered as a result of passing in non-strings for USER.
;;; Written 28-Sep-88 17:33:45 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.92, Experimental ZWEI 126.13, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; DIRED.LISP#340 at 28-Sep-88 17:33:45
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN MAIL (&OPTIONAL USER TEXT CALL-EDITOR-ANYWAY)
  "Mail the string TEXT to the user USER.  USER must be a string containing
 the username and//or hostname, e.g. /"TOM@HOST1/".  
If TEXT is omitted, or CALL-EDITOR-ANYWAY is T, an editor window is used.
CALL-EDITOR-ANYWAY can be a number; the cursor is initially positioned
 that many characters from the beginning of the string TEXT.
With no arguments, you specify everything with the editor window."
  (check-type user string "a string specifying a known username")
  (COND ((AND TEXT (NOT CALL-EDITOR-ANYWAY))
	 (SEND-MESSAGE-STRING USER TEXT))
	(T
	 (ED `(MAIL ,USER ,TEXT
		    ,(IF (NUMBERP CALL-EDITOR-ANYWAY) CALL-EDITOR-ANYWAY))))))

))
