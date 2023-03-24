;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for LFS version 2.3
;;; Reason: Mail server bugs.  Initialization to load extra files.
;;; Bug in reporting error of deleting delete-protected file. - RMS.
;;; Written 7/06/83 17:51:43 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with MIT-Specific 19.5, System 94.34, ZMail 50.16, Experimental Local-File 44.2, FILE-Server 6.6, MagTape 14.4, Experimental LFS 2.1, microcode 238, FC.



; From file MAISER.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; MAISER  "

(DEFVAR MAIL-SERVER-ENABLED T)

))

; From file MAISER.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; MAISER  "

;; This is the top level function of the mail server process.
(DEFUN MAIL-SERVE (&AUX CONN STREAM CLOSING-REASON)
  "This function is called when we get connected to on contact name MAIL."
  (SETQ CONN (CHAOS:LISTEN "MAIL"))
  (IF (NOT MAIL-SERVER-ENABLED)
      (PROGN
	(CHAOS:REJECT CONN "Not accepting MAIL at the moment.")
	(CHAOS:REMOVE-CONN CONN))
    (WITHOUT-INTERRUPTS (PUSH CONN MAIL-CONNS))
    (SETQ STREAM (CHAOS:STREAM CONN))
    (WITHOUT-INTERRUPTS (PUSH STREAM MAIL-STREAMS))
    (COND ((CHAOS:WAIT CONN 'CHAOS:LISTENING-STATE MAIL-SERVER-TIMEOUT "Begin mail")
	   (CHAOS:ACCEPT CONN)
	   (SEND TV:WHO-LINE-FILE-STATE-SHEET ':ADD-SERVER CONN "MAIL")
	   ;;if you want to debug, just move this function call up here
	   ;;and punt the condition-case
	   ;;	 (MAIL-SERVE-STREAM STREAM) (FERROR 'HDT-LOSER "twit")
	   (CONDITION-CASE (PROBLEM)
	       (MAIL-SERVE-STREAM STREAM)  ;;this function does all the work
	     (SYS:BAD-CONNECTION-STATE  ;;random lossage
	      
	      (SETQ CLOSING-REASON (FORMAT NIL "Connection went into ~S after listening."
					   (CHAOS:STATE CONN))))
	     (NO-MORE-ROOM ;;disk full
	      (SEND STREAM ':LINE-OUT "~%Disk full, please try again later.")
	      (SETQ CLOSING-REASON "Disk full, can't receive mail."))
	     (SYS:CONNECTION-NO-MORE-DATA
	      (SETQ CLOSING-REASON "End of file encountered during receipt of mail."))
	     (ERROR ;;this means some random internal problems which should
	      ;;probably be debugged with mail sent to mail-maintainers
	      ;;it would be nice if there was a variable that could be set to
              ;;allow things to be debugged
	      ;;its also not clear whether to send a - or a %
	      (LET ((PROBLEMS (FORMAT NIL "-Internal problems: ~A" PROBLEM)))
		(MAIL-NOTIFY
		  "Mail server:  Problems with receipt of mail from host ~A (stream ~A)
~A" (SEND STREAM ':FOREIGN-HOST) STREAM PROBLEMS)
		(IGNORE-ERRORS
		  (SEND STREAM ':LINE-OUT PROBLEMS)))
	      (SETQ CLOSING-REASON "Internal lossage, please report to Hdt & rms"))
	     (:NO-ERROR
	      (SEND STREAM ':LINE-OUT "+") ;;success
	      (SETQ CLOSING-REASON "Mail sent successfully."))))
	  (T
	   (SETQ CLOSING-REASON "Timeout while listening for start of mail.")))
    ;;now try to close the conn, we either won or lost
    ;;debugging
    ;; (MAIL-NOTIFY "Closing connection because ~A " CLOSING-REASON)
    (WITHOUT-INTERRUPTS
      (SETQ MAIL-CONNS (DELQ CONN MAIL-CONNS))
      (SETQ MAIL-STREAMS (DELQ STREAM MAIL-STREAMS)))
    (IGNORE-ERRORS (SEND STREAM ':FINISH)
		   (CHAOS:CLOSE CONN CLOSING-REASON))))

))

; From file MAISER.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; MAISER  "

(DEFUN MAIL-SERVE-STREAM (STREAM &AUX RCPTS TEXT END)
  "Follow Chaosnet MAIL protocol for receiving mail from STREAM."
  ;;operate on a bidirectional stream
    ;;the CHAOS MAIL protocol says we get recipients on a line one at a time
    ;;and the final one is followed by a ""
    ;;we return after each recipient a line starting with a character with a special meaning
    ;;followed by text intended for human use only
    ;;a + means success, a - means failure, and a % means temporary failure
  (LOOP FOR RCPT = (READLINE STREAM)
	WITH TEM
	UNTIL (STRING-EQUAL RCPT "")
	DOING
	(COND ((SETQ TEM (LOCAL-MAIL-RCPT-EXISTS-P RCPT))
	       (SEND STREAM ':LINE-OUT "+")
	       (PUSH TEM RCPTS))
	      (T
	       (SEND STREAM ':LINE-OUT (FORMAT NIL  "-User ~A not known" RCPT))))
	;;acknowledge this transaction
	(SEND STREAM ':FINISH))
  ;;now that we have the recipients, we have to physically deliver the mail
  ;;and then return a final success or failure message
  (UNWIND-PROTECT
      ;;anybody to send to?  If so, get mail.
      (COND (RCPTS
	     (DO (EOF) (())
	       (SETQ TEXT (PUSH (GET-DISK-RQB 40) TEXT))
	       (SETF (VALUES END EOF)
		     (SEND STREAM ':STRING-IN
			   NIL (SI:RQB-8-BIT-BUFFER (CAR TEXT))))
	       (AND EOF (RETURN)))
	     ;;we now have the text, lets deliver it
	     (SETQ TEXT (REVERSE TEXT))  ;;grr
	     (DOLIST (R RCPTS)
	       (MAIL-TO-RCPT R TEXT END))))
    (DOLIST (RQB TEXT)
      (RETURN-DISK-RQB RQB)))
  ;;  (PROCESS-SLEEP 1200.) ;;I don't think this is the right thing
;We should not have generated any output so this should not be necessary.
;  (CHAOS:FINISH (SEND STREAM ':CONNECTION) "Mail Finish")
  )

))

; From file MAISER.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; MAISER  "

(DEFUN LOCAL-MAIL-RCPT-EXISTS-P (RCPT)
  "Return T if a mailbox for RCPT exists at this host.
Actually, the value is the parsed recipient name."
     ;;for now, this is true iff RCPT has a directory at this host
  (DO-FOREVER
    (LET ((TEM (STRING-REVERSE-SEARCH-CHAR #/@ RCPT)))
      (IF (AND TEM (SEND SI:LOCAL-HOST ':PATHNAME-HOST-NAMEP
			 (STRING-TRIM " " (SUBSTRING RCPT (1+ TEM)))))
	  (SETQ RCPT (STRING-TRIM " " (SUBSTRING RCPT 0 TEM)))
	(RETURN
	  (UNLESS TEM
	    (UNLESS (ERRORP (SEND (SEND (SEND (DEFAULT-PATHNAME NIL LFS-HOST)
					      ':NEW-DIRECTORY RCPT)
					':DIRECTORY-PATHNAME-AS-FILE)
				  ':PROPERTIES
				  NIL))
	      RCPT)))))))

))

; From file SERVER.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; SERVER  "

(add-initialization "Reset files-loaded flag"
		    '(setq *additional-filecomputer-server-files-loaded* ())
		    ()
		    ':before-cold)

))

; From file ANYDIR.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; ANYDIR  "

;Handle operations which open subnodes pass along to us.
;Here, we can freely send messages back to the subnode
;with no fear of deadly embraces, because the subnode
;is already locked by us.

(defmethod (dir-node delete-subnode) (entry &optional no-protection)
  (and (not no-protection)
       (memq ':delete-protect (dir-entry-flags entry))
       (cerror ':no-action nil
	       'dont-delete-flag-set
	       "Old file is delete-protected."
	       (pathlist-into-pathname
		 (send self 'subnode-standard-pathlist entry))))
  (if (dir-entry-deleted entry)
      nil
    (setf (dir-entry-deleted entry) t)
    ;; Update ref date when we delete a file
    ;; so that auto expunge can validly use the ref date
    ;; to decide when it is time to expunge a file.
    (setf (dir-entry-reference-date entry) (time:get-universal-time))
    ;; Cause our :earliest-deleted-file property
    ;; to be the date at which the least-recently-deleted file was deleted.
    (or (funcall supernode 'subnode-get supernode-info ':earliest-deleted-file)
	(funcall supernode 'subnode-putprop supernode-info ':earliest-deleted-file
		 (time:get-universal-time)))
    (dir-entry-mark-changed entry)
    (funcall-self 'update-entry entry))
  t)

))
