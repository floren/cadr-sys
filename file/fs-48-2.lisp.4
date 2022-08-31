;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for Local-File version 48.2
;;; Reason: (neq si:pkg-user-package si:pkg-keyword-package) => t
;;; Written 17-Jan-84 07:15:54 by Mly,
;;; while running on Lisp Machine Eighteen from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.29, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.0, microcode 306, ZM MIT.

; From file FSDEFS.LISP PS:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; FSDEFS  "

(DEFUN WRITE-PROPERTY-LIST (STREAM PLIST &AUX (X (FLOOR (LENGTH PLIST) 2)))
  (PUT-BYTES STREAM 1 X)
  (DO ((I X (1- I))
       (L PLIST (CDDR L))
       (PROP))
      ((ZEROP I))
    (PUT-STRING (GET-PNAME (CAR L)) STREAM)
    (COND ((NULL (SETQ PROP (CADR L)))
	   (FUNCALL STREAM ':TYO 0))
	  ((EQ PROP T)
	   (FUNCALL STREAM ':TYO 1))
	  ((SYMBOLP PROP)
	   (COND ((MEMQ (SYMBOL-PACKAGE PROP) `(,SI:PKG-USER-PACKAGE	;for regular frobs
						,SI:PKG-GLOBAL-PACKAGE	;irregular frobs
						nil))			;(interned later)
		  (FUNCALL STREAM ':TYO 2)
		  (PUT-STRING (GET-PNAME PROP) STREAM))
		 (T (FUNCALL STREAM ':TYO 3)
		    ;; Use this odd order for ease in interning later.
		    (PUT-STRING (GET-PNAME PROP) STREAM)
		    (PUT-STRING (PACKAGE-NAME (SYMBOL-PACKAGE PROP)) STREAM))))
	  ((AND (NUMBERP PROP)
		(FIXP PROP))
	   (FUNCALL STREAM ':TYO 4)
	   (PUT-BYTES STREAM 3 PROP))
	  ((STRINGP PROP)
	   (FUNCALL STREAM ':TYO 5)
	   (PUT-STRING PROP STREAM))
	  ((LISTP PROP)
	   (FUNCALL STREAM ':TYO 6)
	   (LET ((PACKAGE SI:PKG-USER-PACKAGE)
		 (readtable si:common-lisp-readtable)
		 (BASE 10.)
		 (*NOPOINT T)
		 (SI:PRINT-READABLY T))
	     (PRIN1 PROP STREAM)))
	  ((TYPEP PROP 'PATHNAME)
	   (FUNCALL STREAM ':TYO 7)
	   (LET ((PACKAGE SI:PKG-USER-PACKAGE)
		 (readtable si:common-lisp-readtable)
		 (BASE 10.)
		 (*NOPOINT T)
		 (SI:PRINT-READABLY T))
	     ;; These are the arguments to MAKE-FASLOAD-PATHNAME.
	     (FUNCALL STREAM ':TYO #/()
	     (PRIN1-THEN-SPACE (FUNCALL (PATHNAME-HOST PROP) ':NAME-AS-FILE-COMPUTER) STREAM)
	     (PRIN1-THEN-SPACE (PATHNAME-DEVICE PROP) STREAM)
	     (PRIN1-THEN-SPACE (PATHNAME-DIRECTORY PROP) STREAM)
	     (PRIN1-THEN-SPACE (PATHNAME-NAME PROP) STREAM)
	     (PRIN1-THEN-SPACE (PATHNAME-TYPE PROP) STREAM)
	     (PRIN1 (PATHNAME-VERSION PROP) STREAM)
	     (FUNCALL STREAM ':TYO #/))))
	  (T (FERROR NIL "I don't know how to write ~S as a property." PROP)))))

))

; From file FSDEFS.LISP PS:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; FSDEFS  "

(DEFUN READ-PROPERTY-LIST (STREAM &AUX LIST (PAK SI:PKG-USER-PACKAGE))
  (SETQ LIST (MAKE-LIST (* (FUNCALL STREAM ':TYI) 2)))
  (DO ((L LIST (CDDR L)))
      ((NULL L))
    (RPLACA L (INTERN (GET-STRING STREAM) PAK))
    (SELECTQ (FUNCALL STREAM ':TYI)
      (0)
      (1 (SETF (CADR L) T))
      (2 (SETF (CADR L) (INTERN (GET-STRING STREAM) PAK)))
      (3 (SETF (CADR L) (INTERN (GET-STRING STREAM)
				(PKG-FIND-PACKAGE (GET-STRING STREAM) ':ASK))))
      (4 (SETF (CADR L) (GET-BYTES STREAM 3)))
      (5 (SETF (CADR L) (GET-STRING STREAM)))
      (6 (SETF (CADR L)
	       (LET ((IBASE 10.)
		     (PACKAGE SI:PKG-USER-PACKAGE)
		     (readtable si:common-lisp-readtable))
		   ;; This can lose pretty badly with #<'s, etc.  -- DLA
		   (READ STREAM))))
      (7 (LET* ((IBASE 10.)
		(PACKAGE SI:PKG-USER-PACKAGE)
		(readtable si:common-lisp-readtable)
		(FORM (READ STREAM))
		(DEFAULT-CONS-AREA WORKING-STORAGE-AREA))	;<-*
	   (SETF (CADR L)
		 (IF (= (LENGTH FORM) 6)
		     (APPLY #'FS:MAKE-FASLOAD-PATHNAME FORM)
		   (EVAL FORM)))))		;Obsolete form for pathnames to be written.
      (OTHERWISE (FERROR NIL "Invalid Plist property designator."))))
  LIST)

))