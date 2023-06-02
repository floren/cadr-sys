;;; -*- Mode:Lisp; Readtable:T; Package:ZWEI; Base:8.; Patch-File:T -*-
;;; Patch file for ZMail version 54.3
;;; Reason:
;;;  Bug in parsing Unix mail files.
;;; Written 11/30/84 00:36:13 by RMS,
;;; while running on Lisp Machine Four from band 3
;;; with Experimental System 99.11, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, microcode 320, GC@2.



; From file MFHOST.LISP KANSAS:<L.ZMAIL> OZ: (59)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "

(DEFMETHOD (UNIX-MAIL-FILE-MIXIN :LINE-END-OF-MSG-P) (LINE IGNORE STATE EOF IGNORE)
  (VALUES (COND ((NULL STATE) NIL)
		(EOF (LINE-LENGTH LINE))
		((STRING= LINE *UNIX-FROM-MARKER* :END1
			  (LENGTH *UNIX-FROM-MARKER*))
		 :START-NEXT))
	  T))

))
