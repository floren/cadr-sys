;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 53.14
;;; Reason: Pathname default for O command bug.
;;; Editing init-file remembered mail files bug.
;;; Reason: Pathname default for O command bug.
;;; Editing init-file remembered mail files bug.
;;; Written 3/24/84 11:20:43 by RMS,
;;; while running on Lisp Machine One from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.39, CADR 3.6, ZMail 53.13, MIT-Specific 22.0, microcode 306.



; From file PROFIL.LISP PS:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; PROFIL  "

(DEFUN EDIT-PROFILE-REMEMBERED-MAIL-FILES (NEAR-MODE)
  (LET ((ZMAIL-BUFFER-ALIST (GET-ZMAIL-BUFFER-ALISTS T))
	(ACTIVE-LIST NIL))
    (SETQ ZMAIL-BUFFER-ALIST (DELQ (RASSQ *PRIMARY-ZMAIL-BUFFER* ZMAIL-BUFFER-ALIST)
				   ZMAIL-BUFFER-ALIST))
    ;; ACTIVE-LIST gets all the buffers (or pathnames, if file is not loaded)
    ;; that are remembered in the init file as of now.
    (DOLIST (FILE-NAME *OTHER-MAIL-FILE-NAMES*)
      (PUSHNEW (CDR (OR (RASSQ FILE-NAME ZMAIL-BUFFER-ALIST)
			(GLOBAL:FIND (SEND FILE-NAME ':NEW-VERSION NIL)
				     ZMAIL-BUFFER-ALIST
				  ':KEY #'(LAMBDA (X) (IF (TYPEP (CDR X) 'INTERVAL)
							  (BUFFER-PATHNAME (CDR X))
							(CDR X))))))
	       ACTIVE-LIST))
    (SETQ ACTIVE-LIST (DELQ NIL ACTIVE-LIST))
    (MULTIPLE-VALUE (ZMAIL-BUFFER-ALIST ACTIVE-LIST)
      (ZMAIL-MULTIPLE-MENU-CHOOSE ZMAIL-BUFFER-ALIST (NREVERSE ACTIVE-LIST)
				  'MULTIPLE-MENU-NEW-PATHNAME NEAR-MODE
				  "Mail files to be remembered in init file"))
    (SETQ ACTIVE-LIST (LOOP FOR X IN ZMAIL-BUFFER-ALIST
			    WHEN (MEMQ (CDR X) ACTIVE-LIST)
			    COLLECT (CAR X)))
    (COND ((NOT (EQUAL ACTIVE-LIST *OTHER-MAIL-FILE-NAMES*))
	   (SETQ *OTHER-MAIL-FILE-NAMES* ACTIVE-LIST)
	   (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))))))

))

; From file COMNDS.LISP PS:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFUN DEFAULT-ZMAIL-MOVE-PATHNAME ()
  (SEND (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
	  (COND ((AND *DEFAULT-MOVE-ZMAIL-BUFFER*
		      (ZMAIL-BUFFER-DISK-P *DEFAULT-MOVE-ZMAIL-BUFFER*))
		 (BUFFER-PATHNAME *DEFAULT-MOVE-ZMAIL-BUFFER*))
		(*DEFAULT-MOVE-MAIL-FILE-NAME*
		 (FS:MERGE-PATHNAME-DEFAULTS *DEFAULT-MOVE-MAIL-FILE-NAME*
					     *ZMAIL-PATHNAME-DEFAULTS*))
		(T (FS:MERGE-PATHNAME-DEFAULTS USER-ID (FS:USER-HOMEDIR) ':XMAIL))))
	;; Make version be NIL just in case it used to be :UNSPECIFIC
	;; (such as in an ITS pathname with TYPE = "BABYL")
	;; because it is going to be used with FS:*ALWAYS-MERGE-TYPE-AND-VERSION* non-NIL.
	':NEW-VERSION NIL))

))
