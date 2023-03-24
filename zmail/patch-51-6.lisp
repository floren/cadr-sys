;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 51.6
;;; Reason: Fix Q command.
;;; Written 9/26/83 00:43:32 by RMS,
;;; while running on Lisp Machine Eighteen from band 3
;;; with System 97.6, CADR 1.0, ZMail 51.5, MIT-Specific 21.0, microcode 257, ZM MIT.



; From file COMNDS.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-QUIT "Save and exit.
Expunge deleted messages, write out changes and exit.
Right gives menu of options." (NO-ZMAIL-BUFFER-OK)
  (LET ((SAVE-MODE ':SAVE)
	(LOGOUT-MODE ':QUIT))
    (AND (EQ *ZMAIL-COMMAND-BUTTON* ':RIGHT)
	 (MULTIPLE-VALUE (SAVE-MODE LOGOUT-MODE)
	   (DEFAULTED-MULTIPLE-MENU-CHOOSE-NEAR-MENU *ZMAIL-QUIT-MENU-ALIST*
						     SAVE-MODE LOGOUT-MODE)))
    (SELECTQ SAVE-MODE
      (:NOSAVE)
      (:SAVE (ZMAIL-SAVE-ALL))
      (:ASK (ZMAIL-SAVE-MENU)))
    (TV:DESELECT-AND-MAYBE-BURY-WINDOW *ZMAIL-WINDOW*)
    (SELECTQ LOGOUT-MODE
      (:QUIT )
      (:LOGOUT (LOGOUT)
	       (TV:SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'TV:LISTENER-MIXIN 'TV:LISP-LISTENER))))
  DIS-TEXT)

))
