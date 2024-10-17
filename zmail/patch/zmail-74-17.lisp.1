;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.17
;;; Reason:
;;;  Restrict background mail checks to times when there is a user logged in.
;;; Written 8-Nov-88 11:24:47 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.141, Experimental ZWEI 126.29, Experimental ZMail 74.16, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, ) (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: SAZ; TOP.LISP#2 at 8-Nov-88 11:24:47
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "DJ: SAZ; TOP.#"

(DEFUN ZMAIL-BACKGROUND (*ZMAIL-WINDOW*)
  (DO ((*ZMAIL-BACKGROUND-P* T)
       (TV:IO-BUFFER (SEND *ZMAIL-WINDOW* :IO-BUFFER))
       (*BACKGROUND-INBOX-BUFFER* NIL)
       (*LAST-MAIL-CHECK-TIME* (TIME))
       (*BACKGROUND-RESPONSE-QUEUE*
	 (LOCF (ZMAIL-BACKGROUND-PROCESS-RESPONSE-QUEUE CURRENT-PROCESS)))
       (LOCK (LOCF (ZMAIL-BACKGROUND-PROCESS-LOCK CURRENT-PROCESS)))
       (COMMAND-BUFFER-POINTER (LOCF (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL CURRENT-PROCESS)))
       (PRELOAD-QUEUE-POINTER (LOCF (ZMAIL-BACKGROUND-PROCESS-PRELOAD-QUEUE CURRENT-PROCESS)))
       *BACKGROUND-PRELOAD-BUFFER*)
      (NIL)
    (DECLARE (SPECIAL TV:IO-BUFFER))
    (DO () ((< TV:MOUSE-SPEED *BACKGROUND-MOUSE-SPEED-THRESHOLD*))
      (PROCESS-SLEEP 600.))			;Try not to interfere with mousing commands
    ;; Only run when main window exposed or preload requested
    (AND *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED*
	 (CONTENTS PRELOAD-QUEUE-POINTER)	;Have preload requests.
	 (NULL *BACKGROUND-PRELOAD-BUFFER*)			;In the middle of preloading
	 (ZMAIL-BACKGROUND-WAIT-FOR-EXPOSURE))
    (UNWIND-PROTECT
      (LET ((CURRENT-REQUEST (CAAR COMMAND-BUFFER-POINTER)))
	(PROCESS-LOCK LOCK)
	(ZMAIL-BACKGROUND-PERFORM CURRENT-REQUEST)	;Perform one step
	(OR *INHIBIT-BACKGROUND-MAIL-CHECKS*
	    (not *zmail-user*)			;Only do this when there's somebody to check for.
	    (ZMAIL-BACKGROUND-CHECK-FOR-NEW-MAIL)))
      (IGNORE-ERRORS (PROCESS-UNLOCK LOCK)))
    ;; Wait 1 second if there are still pending requests, or a long time if none.
    ;; Stop waiting if any new requests appear.
    (LET ((CURRENT-STATE (CDR COMMAND-BUFFER-POINTER))
	  (PRELOAD-STATE (CDR PRELOAD-QUEUE-POINTER)))
      (PROCESS-WAIT "Zmail Background"
		    #'ZMAIL-BACKGROUND-PAUSE
		    (TIME) (IF (OR CURRENT-STATE *BACKGROUND-PRELOAD-BUFFER*
				   (CDR PRELOAD-QUEUE-POINTER))
			       60. *MAIL-CHECK-PERIOD*)
		    CURRENT-STATE COMMAND-BUFFER-POINTER
		    PRELOAD-STATE PRELOAD-QUEUE-POINTER))))

))
