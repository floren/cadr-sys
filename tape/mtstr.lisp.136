;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Readtable:ZL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.

;;; This file contains stream definitions for the magtape.

;;; Entrypoints are:
;;; MAKE-MT-STREAM &rest options
;;; MAKE-MT-FILE-STREAM &rest options
;;; MAKE-MAGTAPE-FILE-PROBE-STREAM &rest options

;; Get a buffer of the correct byte size
(DEFUN MT-GET-RQB-ARRAY (RQB BYTE-SIZE &AUX TYPE)
  (COND ((= BYTE-SIZE 20) (RQB-BUFFER RQB))
	((= BYTE-SIZE 10) (RQB-8-BIT-BUFFER RQB))
	((SETQ TYPE (CDR (ASSQ BYTE-SIZE '((4 . ART-4B) (2 . ART-2B) (1 . ART-1B)))))
	 (MAKE-ARRAY (FLOOR (* (RQB-NPAGES RQB) (* PAGE-SIZE 32.)) BYTE-SIZE)
		     :AREA LOCAL-FILE-SYSTEM-AREA
		     :TYPE TYPE
		     :DISPLACED-TO RQB
		     ;; This is a system bug.  Don't try to figure it out.
		     :DISPLACED-INDEX-OFFSET (* (%P-CONTENTS-OFFSET (RQB-BUFFER RQB) 3)
						 (FLOOR 20 BYTE-SIZE))))
	(T (FERROR NIL "~D is an invalid byte size." BYTE-SIZE))))

;; This function makes a simple stream to the magtape.
;; It can be used by programs which want to read arbitrarily formated magtapes.
(DEFUN MAKE-MT-STREAM (&OPTIONAL &KEY (DIRECTION :INPUT) (BYTE-SIZE 8) (CHARACTERS T)
		       (UNIT 0) (RECORD-SIZE *default-record-size*) (DENSITY 0) (IBM-MODE NIL)
		       &ALLOW-OTHER-KEYS)
  (CHECK-ARG DIRECTION (MEMQ DIRECTION '(:INPUT :OUTPUT)) ":INPUT or :OUTPUT")
  (CHECK-TYPE BYTE-SIZE :NUMBER)
  (CHECK-ARG CHARACTERS (MEMQ CHARACTERS '(T NIL)) "T or NIL")
  (when ibm-mode
    (ferror nil "FS:MAKE-MT-STREAM called with :IBM-MODE T!!!"))
  (let ((format (tape:parse-format
		  (if (typep tape:*selected-format* 'tape:raw-format)
		      tape:*selected-format*
		    'tape:raw-format)
		  :record-size record-size)))
    (send format :initialize)
    (send format :open-file
	  (tape:parse-device tape:*selected-device* :unit unit :density density)
	  :direction direction
	  :byte-size byte-size
	  :characters characters)))


;; This takes keywords compatibly with OPEN
;; Probe openings are not implemented.
;; This function expects the magtape to be at the beginning of a file header.
;; Errors returned unique to magtape are:
;;  PNA F Probe openings not allowed
;;  EOT F End of Tape

(DEFUN MAKE-MT-FILE-STREAM (&OPTIONAL
			     &KEY
			    (DIRECTION :INPUT)
			    (CHARACTERS :DEFAULT)
			    (BYTE-SIZE :DEFAULT)
			    (UNIT 0)
			    (RECORD-SIZE *default-record-size*)
			    (DENSITY 0)
			    (IBM-MODE NIL)
			    (PLIST NIL)
			    (error t)
			    &allow-other-keys
			    &rest ignore)
  ibm-mode
  (let ((file-plist (when (setq plist (copy-list plist))
		      (cons (fs:make-pathname :host si:local-host
					      :directory (getf plist :directory)
					      :name (getf plist :name)
					      :type (getf plist :type)
					      :version (getf plist :version))
			    (dolist (elem '(:directory :name :type :version) plist)
			      (remf plist elem))))))
    (case direction
      (:output 
       (AND (EQ BYTE-SIZE :DEFAULT)
	    (SETQ BYTE-SIZE (or (tape:file-byte-size plist)
				(IF CHARACTERS 8. 16.)))))
      (:input)
      (t (FERROR NIL "Probe opens not allowed on magtape.")))
    (let ((format (tape:parse-format
		    (if (typep tape:*selected-format* 'tape:lmfl-format)
			tape:*selected-format*
		      'tape:lmfl-format)
		    :record-size record-size)))
      (send format :initialize)
      (condition-case-if (not error) (condition)
	  (send format :open-file
		(tape:parse-device tape:*selected-device* :unit unit :density density)
		:direction direction
		:byte-size byte-size
		:characters characters
		:plist file-plist)
	(tape:tape-error condition)))))


;;; RESTORE-MAGTAPE

;; This is the driver for RESTORE-MAGTAPE and friends.
;; The first value returned describes what it found on the magtape.
;; Possible values are:
;;  :EOF		An end of file block was found.
;;  :STREAM		A header block was found and sucessfully decoded.
;;			Second value is an input stream, from which can be found
;;			properties, truenames, partition-p, etc.
;;  :TAPE-HEADER	A block was found which was a tape header.
;;			The plist from this header is the second value.
;;  :HEADER		This is some sort of header which is not a tape header.
;;			The plist from this header is the second value.
;;  :ERROR		A tape header couldn't be decoded.
;;			Reason (a string) is the second value.

;This tries to be just enuf to allow opening MT: for input or output.
; It specifially tries to make no assumptions about pathnames.
(DEFFLAVOR MT-HOST
	(DEVICE-NAME)
	(SI:BASIC-HOST)
  (:GETTABLE-INSTANCE-VARIABLES DEVICE-NAME)
  (:INITABLE-INSTANCE-VARIABLES DEVICE-NAME))

(DEFMETHOD (MT-HOST :NAME) NIL DEVICE-NAME)
(DEFMETHOD (MT-HOST :NAME-AS-FILE-COMPUTER) NIL DEVICE-NAME)
(DEFMETHOD (MT-HOST :PATHNAME-HOST-NAMEP) (NAME)
  (STRING-EQUAL NAME DEVICE-NAME))

(DEFMETHOD (MT-HOST :PATHNAME-FLAVOR) ()
  (VALUES 'MT-PATHNAME NIL))

(DEFFLAVOR MT-PATHNAME () (PATHNAME))

(DEFMETHOD (MT-PATHNAME :PARSE-NAMESTRING)
	   (HOST-SPECIFIED STRING &OPTIONAL (START 0) END)
  HOST-SPECIFIED
  (VALUES :NO-INTERN (MAKE-INSTANCE 'MT-FILEHANDLE
				     :NAMESTRING (SUBSTRING STRING START END))))

;NAMESTRING is just for pseudo debugging purposes; Magtape files dont really have names.
(DEFFLAVOR MT-FILEHANDLE (NAMESTRING) ()
  (:INITABLE-INSTANCE-VARIABLES NAMESTRING))

(DEFMETHOD (MT-FILEHANDLE :STRING-FOR-PRINTING) () NAMESTRING)
(DEFMETHOD (MT-FILEHANDLE :PRINT-SELF) (STREAM PRINDEPTH SLASHIFY-P) PRINDEPTH
  (COND (SLASHIFY-P
	 (SEND STREAM :STRING-OUT "#<")
	 (PRIN1 'MT-FILEHANDLE STREAM)
	 (FORMAT STREAM " ~S ~O>" NAMESTRING (%POINTER SELF)))
	(T
	 (SEND STREAM :STRING-OUT NAMESTRING))))

;This is a kludge to make the copy-patch-files-of-system work.
(DEFMETHOD (MT-FILEHANDLE :PATCH-FILE-PATHNAME) (&REST IGNORE)
  "MT:")

(DEFUN PUTPROP-MAYBE (PLIST VALUE PROP)
  (WHEN (AND VALUE (NULL (GET PLIST PROP)))
    (PUTPROP PLIST VALUE PROP)))

(DEFMETHOD (MT-FILEHANDLE :OPEN) (IGNORE &REST KEYWORD-ARGS
					 &KEY &OPTIONAL
					 (DIRECTION :INPUT)
					 DEFAULTS-FROM-STREAM 
					 BYTE-SIZE
					 AUTHOR
					 &ALLOW-OTHER-KEYS)
  (COND ((EQ DIRECTION :INPUT)
	 (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM KEYWORD-ARGS))
	((EQ DIRECTION :OUTPUT)
	 (COND (DEFAULTS-FROM-STREAM
		(LET* ((TRUENAME (FUNCALL DEFAULTS-FROM-STREAM :TRUENAME))
		       (PLIST
			 (FILTER-PLIST (FUNCALL DEFAULTS-FROM-STREAM :PLIST)
				       (PLIST TRUENAME)))
		       (REAL-PLIST (LOCF PLIST)))
		  (PUTPROP-MAYBE REAL-PLIST (SEND DEFAULTS-FROM-STREAM :CHARACTERS) :CHARACTERS)
		  (PUTPROP-MAYBE REAL-PLIST BYTE-SIZE :BYTE-SIZE)
		  (PUTPROP-MAYBE REAL-PLIST AUTHOR :AUTHOR)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME :DIRECTORY) :DIRECTORY)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME :NAME) :NAME)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME :TYPE) :TYPE)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME :VERSION) :VERSION)
   ;:UNSPECIFIC would not win when read back in, so guess.
		  (IF (EQ (GET (LOCF PLIST) :TYPE)
			  :UNSPECIFIC)
		      (LET ((FPLIST (FILE-READ-ATTRIBUTE-LIST NIL DEFAULTS-FROM-STREAM)))
			(SI:PUT-ON-ALTERNATING-LIST
			  (COND ((EQ (GET (LOCF FPLIST) :MODE)
				     :LISP)
				 "LISP")
				((= BYTE-SIZE 10)
				 "TEXT")
				(T "UNKNOWN"))
			  PLIST
			  :TYPE)))
		  (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM
				 :PLIST PLIST
				 KEYWORD-ARGS)))
	       (T (FERROR NIL "MT: wins only for copying with stream default"))))
	(T (BREAK "FOO" T))))

(DEFMETHOD (MT-FILEHANDLE :TARGET-TRANSLATE-WILD-PATHNAME)
	   (SOURCE-PATTERN DATA-PATHNAME &OPTIONAL REVERSIBLE-P)
  (declare (ignore source-pattern reversible-p))
  data-pathname)

;below stuff from (PATHNAME :TARGET-TRANSLATE-WILD-PATHNAME)
;  (MULTIPLE-VALUE-BIND (W* W1)
;      (SEND SELF :INTERNAL-WILD-CHARACTERS)
;    (LET ((CASE-CONVERTER
;	    (SEND SELF :TRANSLATION-CASE-CONVERTER)))
;      (MULTIPLE-VALUE-BIND (DEV-SPECS DIR-SPECS NAME-SPECS TYPE-SPECS)
;	  (SEND SOURCE-PATTERN :PATHNAME-MATCH-SPECS DATA-PATHNAME)
;	(MAKE-PATHNAME :HOST HOST
;		       :RAW-DEVICE (PATHNAME-TRANSLATE-WILD-COMPONENT
;				      DEVICE
;				      (FUNCALL CASE-CONVERTER
;					       (PATHNAME-DEVICE DATA-PATHNAME))
;				      (FUNCALL CASE-CONVERTER DEV-SPECS)
;				      W* W1 REVERSIBLE-P)
;		       :RAW-DIRECTORY (PATHNAME-TRANSLATE-WILD-COMPONENT
;					 DIRECTORY
;					 (FUNCALL CASE-CONVERTER
;						  (PATHNAME-DIRECTORY DATA-PATHNAME))
;					 (FUNCALL CASE-CONVERTER DIR-SPECS)
;					 W* W1 REVERSIBLE-P)
;		       :RAW-NAME (PATHNAME-TRANSLATE-WILD-COMPONENT
;				    NAME
;				    (FUNCALL CASE-CONVERTER
;					     (PATHNAME-NAME DATA-PATHNAME))
;				    (FUNCALL CASE-CONVERTER NAME-SPECS)
;				    W* W1 REVERSIBLE-P)
;		       (IF (AND (EQ TYPE :WILD)
;				(OR (NOT REVERSIBLE-P)
;				    (EQ (PATHNAME-TYPE SOURCE-PATTERN) :WILD)))
;			   :TYPE :RAW-TYPE)
;				  (IF (AND (EQ TYPE :WILD)
;					   (OR (NOT REVERSIBLE-P)
;					       (EQ (PATHNAME-TYPE SOURCE-PATTERN) :WILD)))
;				      (SEND DATA-PATHNAME :CANONICAL-TYPE)
;				    (PATHNAME-TRANSLATE-WILD-COMPONENT
;				      TYPE
;				      (FUNCALL CASE-CONVERTER
;					       (PATHNAME-TYPE DATA-PATHNAME))	
;				      (FUNCALL CASE-CONVERTER TYPE-SPECS)
;				      W* W1 REVERSIBLE-P))
;		       :VERSION (IF (EQ VERSION :WILD) (PATHNAME-VERSION DATA-PATHNAME)
;			 VERSION)))))


;TRY TO GET RID OF ANY UNPRINTABLE THINGS FROM PLIST-TO-FILTER..
(DEFUN FILTER-PLIST (PLIST-TO-FILTER PLIST-SO-FAR)
  (COND ((NULL PLIST-TO-FILTER) PLIST-SO-FAR)
	((OR (NULL (CADR PLIST-TO-FILTER))
	     (NUMBERP (CADR PLIST-TO-FILTER))
	     (STRINGP (CADR PLIST-TO-FILTER))
	     (LISTP (CADR PLIST-TO-FILTER)))
	 (FILTER-PLIST (CDDR PLIST-TO-FILTER)
		       (CONS (CAR PLIST-TO-FILTER)
			     (CONS (CADR PLIST-TO-FILTER)
				   PLIST-SO-FAR))))
	(T (FILTER-PLIST (CDDR PLIST-TO-FILTER) PLIST-SO-FAR))))

(DEFUN ADD-MT-HOST (&OPTIONAL (NAME "MT"))
  (COND ((NULL (GET-PATHNAME-HOST NAME T))
	 (LET ((HOST (MAKE-INSTANCE 'MT-HOST :DEVICE-NAME NAME)))
	   (PUSH HOST *PATHNAME-HOST-LIST*)))))

(COMPILE-FLAVOR-METHODS MT-HOST MT-PATHNAME MT-FILEHANDLE)

(ADD-MT-HOST)

