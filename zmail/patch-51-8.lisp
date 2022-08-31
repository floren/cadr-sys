;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 51.8
;;; Reason: Fix problems with edited messages -- by rms remote
;;; Written 10/22/83 03:29:47 by Mly,
;;; while running on Lisp Machine Eighteen from band 3
;;; with System 97.20, CADR 1.0, ZMail 51.7, MIT-Specific 21.0, microcode 257, ZM MIT.



; From file COMNDS.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-EDIT-CURRENT-MSG "Edit the current message" ()
;  (MAKE-WINDOW-CURRENT *MSG-WINDOW*)
;  (SELECT-WINDOW *MSG-WINDOW*)
  (MUST-REDISPLAY *MSG-WINDOW* DIS-BPS)
  (TV:WITH-SELECTION-SUBSTITUTE (*MSG-WINDOW* *ZMAIL-WINDOW*)
    (UNWIND-PROTECT
	(PROGN
	  (LOCK-BACKGROUND-PROCESS)
	  (SETF (NODE-UNDO-STATUS *ZMAIL-BUFFER*)
		(NODE-UNDO-STATUS (MSG-INTERVAL *MSG*)))
	  (LET ((*COMTAB* *MSG-COMTAB*)
		(*MODE-LINE-LIST* '("ZMail " "Editing message " "(" *MODE-NAME-LIST*
				    ") " *ZMAIL-FILE-NAME* *CURRENT-MSG-NAME*)))
	    (FUNCALL *MSG-WINDOW* ':EDIT)
	    DIS-NONE))
      ;; Keep undo info on a per-message basis;
      ;; however, during editing, the undo info must
      ;; be on the top-level node.
      (SETF (NODE-UNDO-STATUS (MSG-INTERVAL *MSG*))
	    (NODE-UNDO-STATUS *ZMAIL-BUFFER*))
      (SETF (NODE-UNDO-STATUS *ZMAIL-BUFFER*) ':DONT)
      ;;Make sure the message is still separated ok
      (LET* ((*INTERVAL* (WINDOW-INTERVAL *MSG-WINDOW*))
	     (BP (INTERVAL-LAST-BP *INTERVAL*)))
	(OR (BEG-LINE-P BP) (INSERT BP #\CR))
	(FUNCALL (MSG-MAIL-FILE-BUFFER *MSG*) ':UPDATE-MSG-END *MSG*))
      (SETF (MSG-TICK *MSG*) (TICK))		;Munge message
      (UNCACHE-MSG-REFERENCES *MSG*)
      (SETF (MSG-STATUS *MSG*) (SOME-PLIST (MSG-STATUS *MSG*) *INTERNAL-TYPE-PROPERTIES*))
      (SET-PARSED-MSG-HEADERS *MSG*)
      (FUNCALL *SUMMARY-WINDOW* ':NEED-TO-REDISPLAY-MSG *MSG*)
      (SETF (WINDOW-MARK-P *MSG-WINDOW*) NIL)
      (ZMAIL-SELECT-MSG *MSG*)			;May not have losing headers any more, say
      (PROCESS-UNLOCK *ZMAIL-BACKGROUND-PROCESS-LOCK*))))

))
