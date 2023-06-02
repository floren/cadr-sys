;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 53.5
;;; Reason: Undigestify bug.  Mail sending via dead hosts bug.
;;; Recover if login host down when looking for init file.
;;; Written 12/14/83 02:16:43 by RMS,
;;; while running on Lisp Machine Nine from band 4
;;; with Bad Inconsistently updated System 98.8, CADR 3.1, Experimental ZMail 53.3, MIT-Specific 22.0, microcode 305, ZM MIT.



; From file COMNDS.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFUN ZMAIL-UNDIGESTIFY-MSG-TO-MSGS
       (MSG BUFFER &AUX (INT (MSG-INTERVAL MSG)) (END-BP (INTERVAL-LAST-BP INT))
			TO-INSERT NEW-MSG (CURRENT-BP (INTERVAL-FIRST-BP INT))
			(IDX (LOCATE-MSG-IN-ZMAIL-BUFFER MSG BUFFER))
			(DIGEST-NAME (MSG-GET MSG ':SUBJECT))
			TO-INSERT-PROP
			(N-MSGS 0) FOUND) ; A BP of success
  "Returns either (), if MSG doesn't seem to be a digest, or how many messages
MSG has been split into."
  ;; Search for the end of the header and contents.
  (LOCK-ZMAIL-BUFFER (BUFFER)
    (SETQ FOUND (UM-SEARCH-FOR-SEPARATOR))
    (WHEN (AND FOUND (BP-IN-INTERVAL-P (UM-SEARCH-FOR-SEPARATOR FOUND) INT))
      (SETQ TO-INSERT (OR (MSG-GET MSG (SETQ TO-INSERT-PROP ':REPLY-TO))
			  (MSG-GET MSG (SETQ TO-INSERT-PROP ':TO))))
      ;; Now to grovel through the rest of the interval
      ;; Delete the body of text (this applies to MSG)
      (IF *CLIP-UNDIGESTIFIED-MESSAGE*  (DELETE-INTERVAL FOUND END-BP T))
      (LET ((*BATCH-UNDO-SAVE* T))
	(DO-FOREVER
	  (SETQ CURRENT-BP FOUND)
	  (SETQ FOUND (UM-SEARCH-FOR-SEPARATOR))
	  (IF (NOT FOUND) (RETURN))			; Might want to do something else later.
	  (INCF IDX) (INCF N-MSGS)
	  (SETQ NEW-MSG (MAKE-EMPTY-MSG))
	  (INSERT-INTERVAL (MSG-END-BP NEW-MSG)
			   CURRENT-BP FOUND T)
	  (SETQ NEW-MSG (SEND BUFFER ':ADD-MSG NEW-MSG IDX))
	  (ADD-HEADER-TO-MSG NEW-MSG TO-INSERT-PROP TO-INSERT)
	  (IF *INHERIT-SUBJECT-FIELD*
	      (ADD-HEADER-TO-MSG NEW-MSG ':SUBJECT
		   (FORMAT () "~A [~A]" (MSG-GET NEW-MSG ':SUBJECT) DIGEST-NAME)))
	  (SETF (MSG-TICK NEW-MSG) (TICK))
	  (SEND *SUMMARY-WINDOW* ':NEED-TO-REDISPLAY-MSG NEW-MSG)))
      (SEND *SUMMARY-WINDOW* ':NEED-FULL-REDISPLAY)
      (IF *DELETE-UNDIGESTIFIED-MESSAGE* (ZMAIL-DELETE-MSG MSG))
      (SEND *SUMMARY-WINDOW* ':NEED-TO-REDISPLAY-MSG MSG)
      N-MSGS)))

))

; From file MAIL.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFUN CHAOS-SEND-IT-1 (HOST RECIPIENTS PLIST INTERVAL TEMPLATE)
  (WITH-OPEN-STREAM (STREAM (CHAOS:OPEN-STREAM HOST "MAIL" ':ERROR NIL))
    (IF (ERRORP STREAM)
	(PROGN
	  ;; When a host can't be reached, move it to end of list.
	  (SETQ *MAIL-CHAOS-HOSTS*
		(NCONC (DELQ HOST *MAIL-CHAOS-HOSTS*)
		       (LIST HOST)))
	  STREAM)
	;; Output the recipients
	(DOLIST (RCPT RECIPIENTS)
	  (SETQ RCPT (STRING-FROM-HEADER RCPT ':HOST))
	  (FUNCALL STREAM ':STRING-OUT RCPT)
	  (FUNCALL STREAM ':TYO #\CR)
	  (CHECK-CHAOS-MAIL-RESPONSE STREAM RCPT))
	(FUNCALL STREAM ':TYO #\CR)		;Mark end of recipients
	(LET ((*QUOTE-HOSTS-FOR-XMAILR* (MEMQ (FUNCALL HOST ':SYSTEM-TYPE)
					      '(:TOPS-20 :TENEX))))
	  (OUTPUT-HEADER-AND-MSG STREAM PLIST INTERVAL TEMPLATE))
	(CHECK-CHAOS-MAIL-RESPONSE STREAM "the body of the message" T)
	(SEND STREAM ':CLOSE ':ABORT)	;Non-abort would send another EOF and wait for it.
	NIL)))

))

; From file TOP.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; TOP  "

(DEFUN LOAD-ZMAIL-INIT-FILE (&OPTIONAL SILENT)
  (UNLESS (EQUAL *ZMAIL-INIT-LOADED* USER-ID)
    (WITH-LOCK (*ZMAIL-PROFILE-LOADING-LOCK-CELL*)
      (CONDITION-CASE ()
	  (WITH-OPEN-FILE (STREAM (ZMAIL-INIT-FILE-PATHNAME)
				  ':CHARACTERS ':DEFAULT)
	    (UNLESS SILENT (FORMAT QUERY-IO "~&Loading ZMail init file ~A"
				   (FUNCALL STREAM ':TRUENAME)))
	    (FUNCALL (IF (FUNCALL STREAM ':CHARACTERS)
			 #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
		     STREAM "ZWEI" T))
	(FS:FILE-NOT-FOUND
	 NIL)
	(SYS:REMOTE-NETWORK-ERROR
	 (FORMAT QUERY-IO "~&Network trouble encountered while looking for a ZMail init file.
It is impossible to tell whether you even have one.")
	 (UNLESS (Y-OR-N-P "Proceed without loading the init file, if any? ")
	   (FORMAT QUERY-IO "~&Pausing and trying again.")
	   (SLEEP 30.)
	   (LOAD-ZMAIL-INIT-FILE SILENT))))
      (SETQ *ZMAIL-INIT-LOADED* USER-ID))))

))
