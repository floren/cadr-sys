;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 73.1
;;; Reason:
;;;  Tighten up use of *run-gmsgs-p*, which determines whether ZMail tries to
;;;  run GMSGS, an old Lab thing that doesn't work anywhere else in the world
;;;  (I suppose).  If the variable is :NO (the default), ZMail isn't supposed
;;;  to run GMSGS, but the logic was broken.
;;;  
;;;  We don't know why this suddenly started happening -- did something else
;;;  get fixed to cause the old logic to "work"?
;;; Written 4-Aug-88 18:48:50 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.12, ZWEI 125.10, ZMail 73.0, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, falambka.



; From modified file DJ: L.ZMAIL; MFHOST.LISP#71 at 4-Aug-88 18:49:05
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "

(defun run-gmsgs-p (&optional maybe)
  (CASE *RUN-GMSGS-P*
    (:YES T)
    (:NO NIL)
    (:ONCE-ONLY maybe)))

))

; From modified file DJ: L.ZMAIL; MFILES.LISP#334 at 4-Aug-88 18:49:16
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

(DEFUN COM-GET-NEW-MAIL-INTERNAL (FIRST-TIME-P &OPTIONAL FOR-BUFFER &AUX FROM-FILE)
  (OR FOR-BUFFER
      (SETQ FOR-BUFFER (COND ((AND *ZMAIL-BUFFER*
				   (GET (LOCF (ZMAIL-BUFFER-OPTIONS *ZMAIL-BUFFER*)) :MAIL))
			      *ZMAIL-BUFFER*)
			     ((NULL *PRIMARY-ZMAIL-BUFFER*)
			      NIL)
			     ((TYPEP *PRIMARY-ZMAIL-BUFFER* 'INBOX-BUFFER)
			      (SEND *PRIMARY-ZMAIL-BUFFER* :ASSOCIATED-MAIL-FILE-BUFFER))
			     (T
			      *PRIMARY-ZMAIL-BUFFER*))))
  (COND ((NULL FOR-BUFFER)
	 (STARTUP-ZMAIL-BUFFER))	;This will call COM-GET-NEW-MAIL-INTERNAL with FIRST-TIME-P
	((SEND FOR-BUFFER :ASSOCIATED-INBOX-BUFFER)
	 (IF (MEMQ (ZMAIL-DISK-BUFFER-STATUS FOR-BUFFER)
		   '(:LOADING :AWAITING-NEW-MAIL))
	     ;; If it's already reading in new mail, do nothing.
	     DIS-NONE
	   (IF (ZMAIL-DISK-BUFFER-STATUS FOR-BUFFER)
	       ;; Ordinary reading in of other file
	       (FOREGROUND-BACKGROUND-FINISH FOR-BUFFER NIL)
	     ;; Probably left around from an error
	     (SEND FOR-BUFFER :SET-ASSOCIATED-INBOX-BUFFER NIL))
	   (COM-GET-NEW-MAIL-INTERNAL NIL FOR-BUFFER)))
	(T
	 (AND (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
	      (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
		(SETQ FROM-FILE (READ-DEFAULTED-PATHNAME
				  "Get new mail from file"
				  (DEFAULT-ZMAIL-MOVE-PATHNAME)))))
	 (AND (EQ FOR-BUFFER *PRIMARY-ZMAIL-BUFFER*)
	      (run-gmsgs-p first-time-p)
	      (NULL FROM-FILE)		;The inbox-buffer won't look at us if this is set.
	      (GMSGS (ZMAIL-BUFFER-GMSGS-HOST FOR-BUFFER)))
	 ;; Possibly any mail file should be allowed that knows about getting new mail
	 (GET-NEW-MAIL-FOR-ZMAIL-BUFFER FOR-BUFFER FROM-FILE NIL FIRST-TIME-P))))

))

; From modified file DJ: L.ZMAIL; MFILES.LISP#334 at 4-Aug-88 18:49:58
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

(DEFMETHOD (MAIL-FILE-BUFFER :INBOX-BUFFER) (&OPTIONAL NEW-PATHNAME DELETE-P &AUX LIST)
  (IF NEW-PATHNAME
      (SETQ LIST (LIST (LIST NEW-PATHNAME NIL DELETE-P)))
    (SETQ NEW-PATHNAME (SEND PATHNAME :NEW-MAIL-PATHNAME))
    (SETQ LIST (LIST (LIST NEW-PATHNAME
			   (SEND NEW-PATHNAME :NEW-TYPE
				 (SEND NEW-PATHNAME
				       :ZMAIL-TEMP-FILE-NAME))
			   T)))
    (COND ((RUN-GMSGS-P)
	   (SETQ NEW-PATHNAME (SEND NEW-PATHNAME :NEW-TYPE "GMSGS"))
	   (PUSH (LIST NEW-PATHNAME
		       (SEND NEW-PATHNAME :NEW-TYPE
			     (SEND NEW-PATHNAME
				   :ZMAIL-TEMP-FILE-NAME))
		       T)
		 LIST))))
  (MAKE-INBOX-BUFFER (SEND NEW-PATHNAME :INBOX-BUFFER-FLAVOR) LIST SELF))

))

; From modified file DJ: L.ZMAIL; MFHOST.LISP#71 at 4-Aug-88 18:50:12
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "

(DEFMETHOD (BABYL-MAIL-FILE-BUFFER :INBOX-BUFFER) (&OPTIONAL NEW-PATHNAME DELETE-P)
  (MAKE-INBOX-BUFFER
    (FUNCALL PATHNAME :INBOX-BUFFER-FLAVOR)
    (IF NEW-PATHNAME
	(LIST (LIST NEW-PATHNAME NIL DELETE-P))
      (LOOP FOR NEW-PATHNAME
	    IN (IF (run-gmsgs-p)
		   (CONS (SEND (ZMAIL-BUFFER-GMSGS-HOST SELF) :GMSGS-PATHNAME)
			 (GET (LOCF OPTIONS) :MAIL))
		 (GET (LOCF OPTIONS) :MAIL))
	    COLLECT (LIST NEW-PATHNAME
			  (FUNCALL NEW-PATHNAME :NEW-TYPE
				   (SEND NEW-PATHNAME
					    :ZMAIL-TEMP-FILE-NAME))
			  T)))
    SELF))

))
