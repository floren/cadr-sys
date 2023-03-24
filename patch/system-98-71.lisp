;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patches for System. From 98.5
;;; Reason: load-and-save-patches lets you specify a band as a fixnum (99.5)
;;; Written 9/21/84 00:05:09 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.2, CADR 4.0, Experimental ZMail 54.0, MIT-Specific 23.0, microcode 320, GC@2.



; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-AND-SAVE-PATCHES (&OPTIONAL BAND &REST KEYWORD-ARGS)
  "Load all patches and save a new Lisp world in a disk partition.
KEYWORD-ARGS are passed to LOAD-PATCHES.
BAND is the name or number of a LOD band to save in."
  (CHECK-TYPE BAND (OR NUMBER STRING NULL) "A specifier for a band")
  (IF (OR (MEMQ :FORCE-UNFINISHED KEYWORD-ARGS)
	  (MEMQ :UNRELEASED KEYWORD-ARGS))
      (FERROR NIL ":FORCE-UNFINISHED and :UNRELEASED are not reasonable arguments here."))
  (DOLIST (PATCH-SYSTEM PATCH-SYSTEMS-LIST)
    (WHEN (EQ (PATCH-STATUS PATCH-SYSTEM) :INCONSISTENT)
      (BEEP)
      (FORMAT *QUERY-IO* "~&You have loaded patches out of sequence,
 or loaded unreleased patches, in ~A.
As a result, the environment is probably inconsistent with the
current patches and will remain so despite attempts to update it.
Unless you understand these problems well and know how to
be sure whether they are occurring, or how to clean them up,
you should not save this environment."
	      (PATCH-NAME PATCH-SYSTEM))
      (SEND *QUERY-IO* :CLEAR-INPUT)
      (UNLESS (YES-OR-NO-P "Dump anyway? ")
	(RETURN-FROM LOAD-AND-SAVE-PATCHES NIL))))
  (DO ((BAND1 BAND (PROMPT-AND-READ :STRING "~&Save into which band? "))
       (COUNT 0 (1+ COUNT)))
      (())
    (WHEN BAND1
      (COND ((NUMBERP BAND1)
	     (SETQ BAND1 (FORMAT NIL "LOD~D" BAND1)))
	    ((PARSE-NUMBER BAND1 0 NIL NIL T)
	     (SETQ BAND1 (STRING-APPEND "LOD" BAND1))))
      (COND ((NOT (STRING-EQUAL BAND1 "LOD" :END1 3))
	     (FORMAT *QUERY-IO* "~&You must save into a LOD partition."))
	    ((NOT (FIND-DISK-PARTITION BAND1))
	     (FORMAT *QUERY-IO* "~&No such band: ~A." BAND1))
	    ((FIND-DISK-PARTITION-FOR-WRITE BAND1)
	     ;; Non-NIL means user gave confirmation.
	     (SETQ BAND BAND1)
	     (RETURN))))
    (IF (ZEROP COUNT) (PRINT-DISK-LABEL)))
  (WITH-SYS-HOST-ACCESSIBLE
    (COND ((APPLY #'LOAD-PATCHES :NOSELECTIVE KEYWORD-ARGS)
	   (DISK-SAVE BAND T))
	  (T (FORMAT *QUERY-IO* "~&No patches have been made.")))))

))
