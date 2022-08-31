;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 53.13
;;; Reason: Profile editing of remembered mail files bug.
;;; Written 4-Mar-84 01:41:18 by Mly,
;;; while running on Lisp Machine Two from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.37, CADR 3.6, ZMail 53.12, MIT-Specific 22.0, microcode 306, gc@36.



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
				  ':KEY #'(LAMBDA (X) (BUFFER-PATHNAME (CDR X))))))
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
