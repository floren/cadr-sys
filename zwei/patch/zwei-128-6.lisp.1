;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 128.6
;;; Reason:
;;;  Introducing new Zwei variable *inhibit-explanation-of-completion*,
;;;  which, when non-nil, reduces HELP typeout from Zwei minibuffers to the
;;;  statement that you are typing in a minibuffer, and the possible command
;;;  completions (all mouse-sensitive) of what you have typed so far. 
;;; Written 18-Nov-88 14:50:31 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 130.6, Experimental ZWEI 128.5, Experimental ZMail 75.0, Experimental Local-File 77.0, Experimental File-Server 26.0, Experimental Unix-Interface 16.0, Experimental Tape 27.0, Experimental Lambda-Diag 19.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 11/14 Falcon System Loaded.



; From modified file DJ: L.ZWEI; COMD.LISP#180 at 18-Nov-88 14:50:38
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-DOCUMENT-COMPLETING-READ "Explain how the completing reader works.
Also tell you what you are currently doing." ()
  (LET (POSS)
    (FORMAT T "~&~A"
	    (OR *COMPLETING-HELP-MESSAGE* "You are in the completing reader."))
    (unless *inhibit-explanation-of-completion*
      (progn
	(FORMAT T
		"~%~%You are typing to a mini-buffer, with the following commands redefined:
Altmode causes as much of the string as can be determined to be inserted
into the mini-buffer (this is called command completion).  Space and )
are similar; they complete up to the next Space or ) respectively.
")
	(FORMAT T
		(IF (CDR *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
		    "
Use Control-Meta-Y to go back to editing the previous argument if you
are no longer satisfied with what you typed.
"
		  "
Use Control-Meta-Y to cancel this command and begin to re-execute
the previous command that read mini-buffer arguments.
"))
	(FORMAT T "
Control-? lists all the strings that complete what you have typed so far,
without the rest of this HELP display.  Control-// lists all the strings
that contain what you have typed anywhere within them.

End will complete as much as possible and return if that gives a unique result.
Return will complete as much as possible, and ")
	(FORMAT T
		(IF *COMPLETING-IMPOSSIBLE-IS-OK-P*
		    "return the result."
		  "if that is a valid string it
will return it."))))
    (MULTIPLE-VALUE (NIL POSS)
      (COMPLETE-STRING (BP-LINE (POINT)) *COMPLETING-ALIST* *COMPLETING-DELIMS*))
    (TERPRI) (TERPRI)
    (LIST-COMPLETIONS-INTERNAL POSS))
  DIS-NONE)

))

; From modified file DJ: L.ZWEI; MACROS.LISP#160 at 18-Nov-88 14:50:55
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; MACROS  "

(defvariable *inhibit-explanation-of-completion* nil :BOOLEAN
	     "T means don't print out all the text explaining how completion works.")

))
