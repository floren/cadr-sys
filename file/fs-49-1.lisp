;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for Local-File version 49.1
;;; Reason: Add :PROERTIES operation for files and streams
;;; Written 7/16/84 11:16:11 by RpK,
;;; while running on Lisp Machine Two from band 1
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.68, CADR 3.8, ZMail 53.18, MIT-Specific 22.2, Experimental Local-File 49.0, microcode 309, gc@36.



; From file FSNAME.LISP OZ:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; FSNAME  "

(DEFMETHOD (LOCAL-FILE-MIXIN :PROPERTIES) (&OPTIONAL ERROR-P)
  (IDENTIFY-FILE-OPERATION :PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (OPENING-INPUT-FILE (F (SEND SELF :DIRECTORY)
			     (SEND SELF :NAME)
			     (SEND SELF :TYPE)
			     (SEND SELF :VERSION))
	(VALUES (CONS (FILE-TRUENAME F) (LMFS-FILE-PROPERTIES F))
		LM-UNSETTABLE-PROPERTIES)))))

))

; From file FSSTR.LISP OZ:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; FSSTR  "

(DEFMETHOD (LM-STREAM-MIXIN :PROPERTIES) (&OPTIONAL ERROR-P)
  ERROR-P ; would be quite hard to get an error here
  (VALUES (CONS TRUENAME SI:PROPERTY-LIST) LM-UNSETTABLE-PROPERTIES))

))
