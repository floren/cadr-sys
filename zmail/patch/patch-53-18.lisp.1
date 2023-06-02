;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Written 6/28/84 21:39:57 by RpK,
;;; Reason: When CHOOSE-MAIL-MODE means :MAIL, it returns :MAIL.
;;; (This caused templates not to get run.)
;;; while running on Lisp Machine Eighteen from band 1
;;; with System 98.64, CADR 3.7, ZMail 53.17, MIT-Specific 22.2, microcode 309, ZM MIT.



; From file MAIL.LISP OZ:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFUN CHOOSE-MAIL-MODE (&OPTIONAL (TYPE :MAIL))
  "Ask user to choose between :LOCAL, :BUG, :FORWARD, :REDISTRIBUTE and :MAIL.
Decides based on button used, or puts up a menu."
  (IF (MEMQ *ZMAIL-COMMAND-BUTTON* '(:MIDDLE :RIGHT))
      (MULTIPLE-VALUE (TYPE *LAST-MAIL-TYPE-ITEM*)
	(ZMAIL-MENU-CHOOSE NIL *ZMAIL-MAIL-MENU-ALIST* *LAST-MAIL-TYPE-ITEM*
			   NIL *MAIL-MIDDLE-MODE*))
    TYPE))

))
