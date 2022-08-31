;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.6
;;; Reason: SI::GET-NEW-SYSTEM-VERSION
;;; DESCRIBE-SYSTEM
;;; Lap bug -- outputting MISC when should have been MISC1 causing overLAP
;;; Written 9/23/84 04:10:47 by RMS,
;;; while running on Lisp Machine Twenty-five from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.4, CADR 4.0, Experimental ZMail 54.0, MIT-Specific 23.0, microcode 320, GC@2.



; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN GET-NEW-SYSTEM-VERSION (&OPTIONAL (MAXIMUM-LENGTH 16.) &KEY INCREMENTAL)
  (FORMAT T "~&This is now:")
  (DESCRIBE-SYSTEM-VERSIONS)
  (FRESH-LINE)
  (SETQ SYSTEM-ADDITIONAL-INFO
	(READLINE-TRIM *QUERY-IO* ""
		       `((:PROMPT "Additional comment for herald: ")
			 (:INITIAL-INPUT ,system-additional-info)
			 (:INITIAL-INPUT-POINTER ,(LENGTH SYSTEM-ADDITIONAL-INFO)))))
  (LET ((VERS (SYSTEM-VERSION-INFO T)))
    (IF INCREMENTAL
	(SETQ VERS (STRING-APPEND "Inc " VERS)))
    ;; If short version doesn't fit, allow user to edit it (e.g. abbreviate system names)
    (DO (SHORT)
	(( (LENGTH VERS) MAXIMUM-LENGTH))
      (SETQ SHORT (SUBSTRING VERS 0 MAXIMUM-LENGTH))
      (SETQ VERS
	    (READLINE-TRIM *QUERY-IO* ""
			   `((:PROMPT ,(FORMAT NIL "~S will not fit in disk label.~@
						    Please abbreviate to ~D character~:P: "
					       VERS MAXIMUM-LENGTH))
			     (:INITIAL-INPUT ,VERS)
			     (:INITIAL-INPUT-POINTER ,MAXIMUM-LENGTH)))))
    VERS))

))

; From file MAKSYS.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN DESCRIBE-SYSTEM (SYSTEM-NAME &KEY (SHOW-FILES T) (SHOW-TRANSFORMATIONS T) &AUX SYSTEM)
  "Print all about the system named SYSTEM-NAME.
SHOW-FILES is T to give the history of each file in the system, NIL not to,
 or :ASK meaning query the user whether to.
SHOW-TRANSFORMATIONS is similar, for whether to show the transformations
 which MAKE-SYSTEM would execute.
Note that calling DESCRIBE on a system-object prints somewhat lower level information."
  (IF (NULL (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME)))
      (FORMAT T "~&There is no system named ~A.~%" SYSTEM-NAME)
    (SETQ SYSTEM-NAME (SYSTEM-NAME SYSTEM))
    (LET* ((SYSTEM-SOURCE-FILE
	     (GET-SOURCE-FILE-NAME (SYSTEM-SYMBOLIC-NAME SYSTEM) 'DEFSYSTEM))
	   (*FORCE-PACKAGE*
	     (PKG-FIND-PACKAGE (OR (AND SYSTEM-SOURCE-FILE (GET SYSTEM-SOURCE-FILE :PACKAGE))
				   "USER"))))
      (WHEN SYSTEM-SOURCE-FILE
	(FORMAT T "~&System ~A~@[ is defined in file ~A~]~%"
		SYSTEM-NAME SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE SYSTEM-SOURCE-FILE)))
    (COND ((SYSTEM-PATCHABLE-P SYSTEM)
	   (FORMAT T "~&~%~A is patchable" SYSTEM-NAME)
	   (MULTIPLE-VALUE-BIND (MAJOR MINOR STATUS)
	       (GET-SYSTEM-VERSION SYSTEM)
	     (LET ((STATUS-NAME (OR (SECOND (ASSQ STATUS SYSTEM-STATUS-ALIST)) STATUS)))
	       (OR (EQUAL STATUS-NAME "")
		   (FORMAT T ", ~A" STATUS-NAME)))
	     (IF MAJOR (FORMAT T ", ~D.~D is loaded" MAJOR MINOR))
	     (FORMAT T ";~%  a typical patch file is ~A~%"
		     (PATCH-SYSTEM-PATHNAME SYSTEM-NAME ':PATCH-FILE (OR MAJOR 1) (OR MINOR 0)
					    ':LISP))
	     (AND MAJOR
		  (FQUERY NIL "Do you want to see the patches for ~A? " SYSTEM-NAME)
		  (PRINT-PATCHES SYSTEM)))))
    (IF (SYSTEM-PACKAGE-DEFAULT SYSTEM)
	(FORMAT T "~& Files in ~A are forcibly read in package ~A.~%"
		SYSTEM-NAME (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
    (WHEN SHOW-FILES
      (FORMAT T "~%Compilation and loading of files in this system:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':RELOAD ':DO-NOT-DO-COMPONENTS
		   ':DESCRIBE ':NO-INCREMENT-PATCH ':NO-RELOAD-SYSTEM-DECLARATION))
    (WHEN SHOW-TRANSFORMATIONS
      (FORMAT T "~%Transformations required to MAKE-SYSTEM now:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':DO-NOT-DO-COMPONENTS ':PRINT-ONLY
		   ':NO-RELOAD-SYSTEM-DECLARATION))
    (LET ((COMPONENTS (SYSTEM-COMPONENT-SYSTEMS SYSTEM)))
      (COND (COMPONENTS
	     (FORMAT T "~2&~A is made up of component system~P "
		     SYSTEM-NAME (LENGTH COMPONENTS))
	     (FORMAT:PRINT-LIST T "~A" COMPONENTS)
	     (WHEN (Y-OR-N-P "Describe the component system~P?" (LENGTH COMPONENTS))
	       (DOLIST (COMPONENT COMPONENTS)
		 (FORMAT T "~2&")
		 (DESCRIBE-SYSTEM COMPONENT ':SHOW-FILES SHOW-FILES
				  ':SHOW-TRANSFORMATIONS SHOW-TRANSFORMATIONS)))))))
  SYSTEM-NAME)

))

; From file QCP2.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN OUTI (X)
  (IF (NOT DROPTHRU)
      NIL
    (IF (AND (EQ (CADR X) 'D-RETURN)
	     (NOT (EQ (CAR X) 'CALL)))
	(SETQ DROPTHRU NIL))
    (IF (AND (EQ (CAR X) 'MISC)
	     (>= (GET (THIRD X) 'QLVAL) #o1000))
	(SETQ X (CONS 'MISC1 (CDR X))))
    (IF (MEMQ (CAR X) '(MISC MISC1))
	(OUTF X)
      (OUTS X))))

))
