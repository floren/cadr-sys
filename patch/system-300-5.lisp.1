;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 300.3
;;; Reason:
;;;  Fix issue with unix-versions and make-system
;;; Written 17-Oct-24 15:58:00 by eswenson,
;;; while running on EJS LISPM 2 from band 4
;;; with Experimental System 300.0, Unix-Versions 95.0, Experimental Local-File 54.0, microcode 323, ejs3.



; From file CHAOS3: /sys/sys2/maksys.lisp.182 at 17-Oct-24 15:58:07
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "CHAOS3: //sys//sys2//maksys"

(DEFUN SYSTEM-GET-CREATION-DATE (FILE &OPTIONAL NO-ERROR-P)
  (LET* ((real-file (send file :truename))
         (PLIST (SYSTEM-GET-FILE-PROPERTY-LIST real-file)))
    (COND ((AND (NULL (CDR (LAST PLIST)))	;this gets around a bug which causes
			;it to bomb out on FC: when the file doesnt exist. The property
			;list manages to get a string nconced on the end.  I think it
			;may be an ancient bug in system 86 running on the filecomputer. --rg
		(GET PLIST :CREATION-DATE)))
	  ((OR NO-ERROR-P *JUST-ACCUMULATING-FILES*) -1)
	  (T (FERROR NIL "File ~A does not exist" FILE)))))

))
