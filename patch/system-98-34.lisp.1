;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 2/01/84 06:46:35 by RpK,
;;; Reason: :IF-DOES-NOT-EXIST in LOAD works as documented.
;;; while running on Lisp Machine Eighteen from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.32, CADR 3.6, ZMail 53.10, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFVAR *LOAD-SET-DEFAULT-PATHNAME* T
  "Non-NIL means LOAD sets the default pathname to the name of the file loaded.
Can be overridden by the :SET-DEFAULT-PATHNAME keyword when LOAD is called.")

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN LOAD (FILE &REST KEY-OR-POSITIONAL-ARGS)
  "Load the specified text file or QFASL file or input stream.
If the specified filename has no type field, we try LISP and then QFASL.
Regardless of the filename type, we can tell QFASL files from text files.
PACKAGE specifies the package to load into; if missing or NIL,
 the package specified by the file's attribute list is used.
VERBOSE non-NIL says it's ok to print a message saying what is being loaded.
 Default comes from *LOAD-VERBOSE*, normally T.
SET-DEFAULT-PATHNAME non-NIL says set the default pathname for LOAD
 to the name of this file.  Default from *LOAD-SET-DEFAULT-PATHNAME*, normally T.
IF-DOES-NOT-EXIST NIL says just return NIL for file-not-found.  Default T.
 In all other cases the value is the truename of the loaded file, or T.
PRINT non-NIL says print all forms loaded."
  (DECLARE (ARGLIST FILE &KEY &OPTIONAL PACKAGE VERBOSE SET-DEFAULT-PATHNAME
		    (IF-DOES-NOT-EXIST T) PRINT))
  (IF (AND (CAR KEY-OR-POSITIONAL-ARGS)
	   (MEMQ (CAR KEY-OR-POSITIONAL-ARGS)
		 '(:PACKAGE :PRINT :IF-DOES-NOT-EXIST :SET-DEFAULT-PATHNAME :VERBOSE)))
      (LET ((SI:PRINT-LOADED-FORMS
	      (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':PRINT)))
	(LOAD-1 FILE (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':PACKAGE)
		(COND ((NULL (GETL (LOCF KEY-OR-POSITIONAL-ARGS) '(:IF-DOES-NOT-EXIST)))
		       ())
		      (T (NOT (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':IF-DOES-NOT-EXIST))))
		(NOT (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':SET-DEFAULT-PATHNAME
			  *LOAD-SET-DEFAULT-PATHNAME*))
		(NOT (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':VERBOSE
			  *LOAD-VERBOSE*))))
    (LEXPR-FUNCALL 'LOAD-1 FILE KEY-OR-POSITIONAL-ARGS)))

))
