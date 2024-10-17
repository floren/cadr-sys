;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.9
;;; Reason:
;;;  Now you can use the mouse to filter your ZMail messages by
;;;  sender/recipient.  Use the Select menu's "Filter" mode (i.e., click
;;;  middle on the Select menu item), click on "From/To" in the filter-making
;;;  window which appears, then click on a message sent from or to the
;;;  relevant user in the message summary (top) pane, and, finally, click on
;;;  the correct field in the typeout window... 
;;;  
;;;  [An obvious future enhancement would be to break up this From/To
;;;  selection feature into separate From and To selection features.
;;;  Then, clicking on the relevant message will be unambiguous, and
;;;  the need for the second click (in the typeout window) will go
;;;  away!
;;; Written 6-Oct-88 19:14:11 by saz at site Gigamos Cambridge
;;; while running on Johannes Brahms from band 3
;;; with Experimental System 126.100, Experimental ZWEI 126.14, Experimental ZMail 74.4, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZMAIL; FILTER.LISP#363 at 6-Oct-88 19:14:18
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; FILTER  "


(DEFUN CHOOSE-MSG-OR-READLINE (PROMPT &OPTIONAL DEFAULT &AUX RESULT)
  (let ((*throw-unknown-special-command* 'choose-msg-or-readline))
    (*CATCH *throw-unknown-special-command*
      (WITH-BACKGROUND-PROCESS-LOCKED
	(CONDITION-BIND ((UNKNOWN-SPECIAL-COMMAND
			   'CHOOSE-MSG-OR-READLINE-UNKNOWN-SPECIAL-COMMAND))
	  (SETQ RESULT (TYPEIN-LINE-READLINE
			 "~A:~@[ (Default: ~A)~]~:[ (Or select message with mouse)~]"
			 PROMPT DEFAULT
			 (NOT (SEND *SUMMARY-WINDOW* :EXPOSED-P))))))
      (AND DEFAULT (EQUAL RESULT "")
	   (SETQ RESULT DEFAULT))
      RESULT)))

))

; From modified file DJ: L.ZWEI; COMTAB.LISP#358 at 6-Oct-88 19:14:32
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "


(defvar *throw-unknown-special-command* nil) 

))

; From modified file DJ: L.ZWEI; COMTAB.LISP#358 at 6-Oct-88 19:14:49
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "


(DEFUN UNKNOWN-SPECIAL-COMMAND (TYPE &REST REST)
; (DECLARE (IGNORE REST))			;not needed
  (if (and *throw-unknown-special-command*
	   (eq type 'summary-mouse))
      (*throw *throw-unknown-special-command* (cadar rest))
    (CERROR "Ignore it" "ZMACS error: ~~S is not a valid special editor command~" TYPE)))

))
