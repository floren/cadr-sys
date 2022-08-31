;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 6/15/84 03:43:03 by LMFile,
;;; Reason: For now, ignore when hosts change networks.  This patch must be loaded before the
;;; next host table is made for the MIT.
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.60, CADR 3.7, ZMail 53.17, MIT-Specific 22.1, Experimental Local-File 48.5, Experimental FILE-Server 8.5, Experimental LFS 3.3, Experimental MagTape 22.6, microcode 309, XFS/C.



; From file HOST.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; HOST  "

(DEFUN DEFINE-HOST (NAME &REST OPTIONS &AUX ELEM OLD-P)
  "Define a host.  Should be used only in SYS:SITE;HSTTBL LISP."
  (IF (NULL (SETQ ELEM (ASSOC-EQUALP NAME HOST-ALIST)))
      (SETQ ELEM (MAKE-HOST-ALIST-ELEM :NAME NAME))
      (SETQ OLD-P T)
      (SETF (HOST-ADDRESSES ELEM) NIL))
  (SETF (HOST-SITE-NAME ELEM) SITE-NAME)
  (LOOP FOR (OPTION VALUE) ON OPTIONS BY 'CDDR
	DO (SELECTQ OPTION
	     (:HOST-NAMES (SETF (HOST-NAME-LIST ELEM) VALUE))
	     (:SYSTEM-TYPE (SETF (HOST-SYSTEM-TYPE-INTERNAL ELEM) VALUE))
	     (:MACHINE-TYPE (SETF (HOST-MACHINE-TYPE-INTERNAL ELEM) VALUE))
	     (OTHERWISE (PUTPROP (LOCF (HOST-ADDRESSES ELEM)) VALUE OPTION))))
  (IF (NOT OLD-P)
      (PUSH ELEM HOST-ALIST)
    ;; It changed flavors, due to network or OS change.  Just ignore it for now.
    (LET ((OLD-INSTANCE (HOST-INSTANCE ELEM)))
      (AND OLD-INSTANCE
	   (LET ((FLAVOR (COMPUTE-HOST-FLAVOR ELEM)))
	     (AND (NEQ FLAVOR (TYPEP OLD-INSTANCE))
		  (FORMAT *ERROR-OUTPUT* "~&[Sorry, can't change ~A's flavor.]~%"
			  (HOST-NAME ELEM))))))))

))
