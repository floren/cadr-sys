;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.8
;;; Reason:
;;;  MAKE-SYSTEM: Name CWARNS file with the system name when doing :DEFAULTED-BATCH.
;;; Written 10-May-23 13:27:08 by ams,
;;; while running on Lisp Machine One from band 7
;;; with Experimental System 100.7, Hacks by AMS 1.0, microcode 323, AMS.



; From file FC: /tree/sys2/maksys.lisp at 10-May-23 13:27:09
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //tree//sys2//maksys"

(DEFUN (:DEFAULTED-BATCH MAKE-SYSTEM-KEYWORD) (&AUX PATHNAME)
  (SETQ PATHNAME (OR (SYSTEM-WARNINGS-PATHNAME-DEFAULT *SYSTEM-BEING-MADE*)
		     (SEND (FS:USER-HOMEDIR) :NEW-PATHNAME
			   :NAME (FORMAT NIL "~A-CWARNS"
					 (STRING-UPCASE (OR (SI:SYSTEM-SYMBOLIC-NAME *SYSTEM-BEING-MADE*)
							    (SI:SYSTEM-SHORT-NAME *SYSTEM-BEING-MADE*))))
			   :TYPE :LISP
			   :VERSION :NEWEST)))
  (SETQ INHIBIT-FDEFINE-WARNINGS :JUST-WARN
	TV:MORE-PROCESSING-GLOBAL-ENABLE NIL
	*BATCH-MODE-P* T
	*QUERY-TYPE* :NOCONFIRM)
  (FORMAT *QUERY-IO* "~&Writing compiler warnings data base to file ~A.~%" PATHNAME)
  (SETQ *WARNINGS-STREAM* (OPEN PATHNAME :OUT))
  (FORMAT *WARNINGS-STREAM*
	  "~&;System ~A made by ~A at ~\DATIME\  -*-Mode: Lisp; Package: User; Base: 10.-*-~%"
	  (SYSTEM-NAME *SYSTEM-BEING-MADE*) USER-ID)
  (PUSH `(CLOSE *WARNINGS-STREAM*)
	*MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*))
))
