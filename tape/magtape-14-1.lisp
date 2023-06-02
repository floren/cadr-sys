;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for MagTape version 14.1
;;; Reason: Bug fixes.
;;; Written 3/08/83 00:54:35 by Dulcey,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with Experimental MIT-Specific 18.1, Experimental System 93.24,
;;; Experimental ZMail 49.9, Experimental Local-File 43.0, Experimental MagTape 14.0,
;;; microcode 226, gc@2.



; From file MTSTR.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR  "

;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-

;;; This file contains stream definitions for the magtape.

;;; Entrypoints are:
;;; MAKE-MT-STREAM &rest options
;;; MAKE-MT-FILE-STREAM &rest options
;;; MAKE-MAGTAPE-FILE-PROBE-STREAM &rest options

;;; Lossages:
;;; PAGE-SIZE-IN-BITS


(DEFUN GET-RQB-ARRAY (RQB BYTE-SIZE)
  (IF (> BYTE-SIZE 8)
      (SI:RQB-BUFFER RQB)
    (SI:RQB-8-BIT-BUFFER RQB)))

))

; From file MTSTR.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR  "

;; This takes keywords compatibly with OPEN
;; Probe openings are not implemented.
;; This function expects the magtape to be at the beginning of a file header.
;; Errors returned unique to magtape are:
;;  PNA F Probe openings not allowed
;;  EOT F End of Tape

(DEFUN MAKE-MT-FILE-STREAM (&OPTIONAL
			     &REST OPTIONS
			     &KEY
			    (DIRECTION ':INPUT)
			    (CHARACTERS ':DEFAULT)
			    (ERROR T)
			    (BYTE-SIZE ':DEFAULT)
			    (UNIT 0)
			    (RECORD-SIZE 10000)
			    (DENSITY 0)
			    (IBM-MODE NIL)
			    (PLIST NIL)
			    (HEADER-STYLE ':MIT)	;Default for write
			    &ALLOW-OTHER-KEYS
			    &AUX STREAM TEM)
  (COND ((EQ DIRECTION ':INPUT)
	 (MULTIPLE-VALUE (PLIST HEADER-STYLE)
	   (READ-MAGTAPE-HEADER
	     (SETQ STREAM (LEXPR-FUNCALL 'MAKE-MT-STREAM
					 ':CHARACTERS T
					 ':BYTE-SIZE 8
					 ':UNIT UNIT
					 ':DENSITY DENSITY
					 OPTIONS))))
	 (FUNCALL STREAM ':CLOSE ':RAW)
	 (IF (NULL PLIST)
	     (MT-OPEN-ERROR (MAKE-CONDITION 'END-OF-TAPE "End of tape on unit ~D." UNIT)
			    ERROR)
	     (AND (EQ CHARACTERS ':DEFAULT)
		  (IF (SETQ TEM (GETL (LOCF PLIST) '(:CHARACTERS)))
		      (SETQ CHARACTERS (CADR TEM))
		      ;; Kludge for old format tapes.
		      (SETQ CHARACTERS (= (GET (LOCF PLIST) ':BYTE-SIZE) 8))))
	     (AND (EQ BYTE-SIZE ':DEFAULT)
		  (SETQ BYTE-SIZE (GET (LOCF PLIST) ':BYTE-SIZE)))
	     (AND (NULL BYTE-SIZE)
		  (SETQ BYTE-SIZE (IF CHARACTERS 8. 16.)))
	     (MAKE-INSTANCE (IF CHARACTERS
				'MT-FILE-CHARACTER-INPUT-STREAM
				'MT-FILE-INPUT-STREAM)
			    ':BYTE-SIZE BYTE-SIZE
			    ':UNIT UNIT
			    ':RECORD-SIZE RECORD-SIZE
			    ':DENSITY DENSITY
			    ':PROPERTY-LIST PLIST
			    ':HEADER-FORMAT HEADER-STYLE
			    ':IBM-MODE IBM-MODE)))
	((EQ DIRECTION ':OUTPUT)
	 (AND (EQ BYTE-SIZE ':DEFAULT)
	      (SETQ BYTE-SIZE (GET (LOCF PLIST) ':BYTE-SIZE)))
	 (AND (NULL BYTE-SIZE)
	      (SETQ BYTE-SIZE (IF CHARACTERS 8. 16.)))
	 (WRITE-MAGTAPE-HEADER
	   (SETQ STREAM (LEXPR-FUNCALL 'MAKE-MT-STREAM
				       ':DIRECTION ':OUTPUT
				       ':CHARACTERS T
				       ':BYTE-SIZE 8
				       ':UNIT UNIT
				       ':DENSITY DENSITY
				       OPTIONS))
	   PLIST
	   HEADER-STYLE)
	 (FUNCALL STREAM ':CLOSE ':RAW)
	 (MAKE-INSTANCE (IF CHARACTERS
			    'MT-FILE-CHARACTER-OUTPUT-STREAM
			    'MT-FILE-OUTPUT-STREAM)
			':BYTE-SIZE BYTE-SIZE
			':UNIT UNIT
			':RECORD-SIZE RECORD-SIZE
			':DENSITY DENSITY
			':PROPERTY-LIST PLIST
			':HEADER-FORMAT HEADER-STYLE
			':IBM-MODE IBM-MODE))
	(T (FERROR NIL "Probe opens not allowed on magtape."))))

))

; From file MTSTR.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR  "

(DEFUN MT-OPEN-ERROR (CONDITION ERROR-P)
  (IF ERROR-P
      (SIGNAL-CONDITION CONDITION)
    CONDITION))

))

; From file MTSTR.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR  "

(DEFMETHOD (MT-FILEHANDLE :OPEN) (IGNORE &REST KEYWORD-ARGS
					 &KEY &OPTIONAL
					 (DIRECTION ':INPUT)
					 DEFAULTS-FROM-STREAM 
					 BYTE-SIZE
					 AUTHOR
					 &ALLOW-OTHER-KEYS)
  (COND ((EQ DIRECTION ':INPUT)
	 (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM KEYWORD-ARGS))
	((EQ DIRECTION ':OUTPUT)
	 (COND (DEFAULTS-FROM-STREAM
		(LET* ((TRUENAME (FUNCALL DEFAULTS-FROM-STREAM ':TRUENAME))
		       (PLIST
			 (FILTER-PLIST (FUNCALL DEFAULTS-FROM-STREAM ':PLIST)
				       (PLIST TRUENAME))))
		  (IF (AND BYTE-SIZE
			   (NULL (GET (LOCF PLIST) ':BYTE-SIZE)))
		      (SETQ PLIST (CONS ':BYTE-SIZE (CONS BYTE-SIZE PLIST))))
		  (IF (AND AUTHOR
			   (NULL (GET (LOCF PLIST) ':AUTHOR)))
		      (SETQ PLIST (CONS ':AUTHOR (CONS AUTHOR PLIST))))
   ;:UNSPECIFIC would not win when read back in, so guess.
		  (IF (EQ (GET (LOCF PLIST) ':TYPE)
			  ':UNSPECIFIC)
		      (LET ((FPLIST (FILE-READ-ATTRIBUTE-LIST NIL DEFAULTS-FROM-STREAM)))
			(PUT-ON-ALTERNATING-LIST
			  (COND ((EQ (GET (LOCF FPLIST) ':MODE)
				     ':LISP)
				 "LISP")
				((= BYTE-SIZE 10)
				 "TEXT")
				(T "UNKNOWN"))
			  PLIST
			  ':TYPE)))
		  (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM
				 ':PLIST PLIST
				 KEYWORD-ARGS)))
	       (T (FERROR NIL "MT: wins only for copying with stream default"))))
	(T (BREAK FOO T))))

))
