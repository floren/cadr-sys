;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.5
;;; Reason: SUBST expansion bug (reversing order of evaluation).
;;; Put initial buffer in ZMACS window's history.
;;; Protect debugger against errors in printing.
;;; LOAD returns file's truename again.
;;; Mouse click on SUPDUP window bug.
;;; (VIEWF "just-a-directory-name") bug.
;;; PROBEF bug (in OPEN-CHAOS).
;;; :BREAK flavor operation bug.
;;; DEFSTRUCT :INCLUDE bug.
;;; Written 12/05/83 09:27:23 by RMS,
;;; while running on Lisp Machine Eighteen from band 6
;;; with Bad Inconsistently updated System 98.3, CADR 3.1, Inconsistently updated ZMail 53.1, MIT-Specific 22.0, microcode 305, ZM MIT.



; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

;; Expand a call to a SUBST function.  SUBST is the function definition to use.
;; FORM is the whole form.
;; Match the SUBST args with the expressions in the form
;; and then substitute the expressions for the args in the body of the function with SUBLIS.

(DEFUN SUBST-EXPAND (SUBST FORM &OPTIONAL SIMPLE-SUBSTITUTION-OK)
  (LET (ALIST OPTIONAL-FLAG REST-ALREADY-FLAG LAMBDA-LIST BODY FN-NAME)
    ;; Extract the lambda-list, body, and function name from the definition.
    (COND ((EQ (CAR SUBST) 'NAMED-SUBST)
	   (SETQ LAMBDA-LIST (CADDR SUBST) BODY (CDDDR SUBST))
	   (SETQ FN-NAME (COND ((SYMBOLP (CADR SUBST)) (CADR SUBST))
			       (T (CAADR SUBST)))))
	  (T (SETQ LAMBDA-LIST (CADR SUBST) BODY (CDDR SUBST)
		   FN-NAME (CAR FORM))))
    ;; Discard documentation string or declarations from front of body.
    (SETQ BODY (EXTRACT-DECLARATIONS BODY NIL T))
    ;; Provide an implicit PROGN for the body.
    (COND ((CDR BODY) (SETQ BODY `(PROGN . ,BODY)))
	  (T (SETQ BODY (CAR BODY))))
    ;;;??? Flush the implicitly generated BLOCK.
    ;; This is a kludge, indeed.
    (AND (LISTP BODY) (EQ (CAR BODY) 'BLOCK)
	 (SETQ BODY (CONS 'PROGN (CDDR BODY))))
    ;; Process the lambda list and args to make the alist.
    (DO ((VALS (CDR FORM) (CDR VALS)))
	(NIL)
      ;; We allow only &OPTIONAL and &REST.
      (DO () (())
	(SELECTQ (CAR LAMBDA-LIST)
	  (&OPTIONAL (SETQ OPTIONAL-FLAG T))
	  (&REST (OR REST-ALREADY-FLAG
		     (SETQ VALS (LIST (CONS 'LIST VALS))
			   REST-ALREADY-FLAG T)))
	  (OTHERWISE (RETURN)))
	(POP LAMBDA-LIST))
      ;; All lambda-list keywords aside from &OPTIONAL and &REST are erroneous.
      (AND (MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
	   (RETURN
	     (CONS (CERROR T NIL 'INVALID-FORM
			   "Subst-function ~S contains inappropriate keyword ~A."
			   FN-NAME (CAR LAMBDA-LIST))
		   (CDR FORM))))
      ;; Detect runout of lambda list or of args.
      (COND ((NULL VALS)
	     (COND ((NULL LAMBDA-LIST)
		    (RETURN (IF SIMPLE-SUBSTITUTION-OK
				(SUBLIS ALIST BODY)
			      (SUBLIS-EVAL-ONCE (NREVERSE ALIST) BODY))))
		   ((NOT OPTIONAL-FLAG)
		    (RETURN (CERROR T NIL 'INVALID-FORM
				    "Too few arguments for ~S."
				    FN-NAME FORM)))))
	    ((NULL LAMBDA-LIST)
	     (RETURN (CERROR T NIL 'INVALID-FORM
			     "Too many arguments for ~S."
			     FN-NAME FORM))))
      ;; Here we have one more arg.  Add it to the alist.
      (PUSH (CONS (COND ((ATOM (CAR LAMBDA-LIST)) (CAR LAMBDA-LIST))
			(T (CAAR LAMBDA-LIST)))
		  (COND (VALS (CAR VALS))
			((ATOM (CAR LAMBDA-LIST)) NIL)
			(T (CADAR LAMBDA-LIST))))
	    ALIST)
      (POP LAMBDA-LIST))))

))

(globalize "LIST-MATCH-P")

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZMACS-WINDOW :AFTER :INIT) (IGNORE)
  (PUSH-ON-HISTORY INTERVAL BUFFER-HISTORY))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN SHOW-ALL-MACRO (SG FRAME &OPTIONAL NO-DISASSEMBLED-CODE
		       &AUX N-LOCALS PC-NOW NAME REST-ARG-PRINTED NLINES WHERE LIM-PC
		       (RP (SG-REGULAR-PDL SG))
		       (FUNCTION (RP-FUNCTION-WORD RP FRAME)))
  (SETQ N-LOCALS (FEF-NUMBER-OF-LOCALS FUNCTION)
	NAME (FEF-NAME FUNCTION)
	PC-NOW (RP-EXIT-PC RP FRAME)
	LIM-PC (COMPILER:DISASSEMBLE-LIM-PC FUNCTION))
  (FORMAT T "~%~s (P.C. = ~O)~%" NAME PC-NOW)
  (WHEN (AND (LISTP NAME) (EQ (CAR NAME) ':METHOD))
    (FORMAT T "  (SELF is ")
    (PRINT-CAREFULLY "SELF"
      (PRIN1 (SYMEVAL-IN-STACK-GROUP 'SELF SG FRAME)))
    (FORMAT T ")~%"))
  (TERPRI)
  ;; Print the arguments, including the rest-arg which is the first local
  (SETQ REST-ARG-PRINTED (PRINT-FRAME-ARGS SG FRAME 0))
  (COND ((SG-FRAME-ACTIVE-P SG FRAME)
	 ;; Print the rest of the locals -- if the frame is active.
	 (DOTIMES (I N-LOCALS)
	   (COND ((NOT (AND REST-ARG-PRINTED (ZEROP I)))	;Don't show rest arg twice
		  (FORMAT T "Local ~D" I)
		  (DISPLAY-LOCAL-NAME " (~A)" FUNCTION I)
		  (LET ((PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
			(PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
		    (FORMAT T ": ")
		    (PRINT-CAREFULLY "local"
		      (PRIN1 (SG-FRAME-LOCAL-VALUE SG FRAME I)))
		    (TERPRI)))))
	 (UNLESS NO-DISASSEMBLED-CODE
	   (FORMAT T "~%Disassembled code:")
	   ;; Figure out how many instructions will fit in the stream we are using.
	   (SETQ NLINES
		 (MAX DISASSEMBLE-INSTRUCTION-COUNT	;don't show absurdly few
		      (COND ((MEMQ ':SIZE-IN-CHARACTERS (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
			     (MULTIPLE-VALUE (NIL NLINES)
			       (FUNCALL STANDARD-OUTPUT ':SIZE-IN-CHARACTERS))
			     (MULTIPLE-VALUE (NIL WHERE)
			       (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':CHARACTER))
			     (- NLINES WHERE 2))	;Leave 1 line for prompt, 1 for extra terpri
			    (T 0))))		;Don't know size of window, use default count
	   (DO ((I 0 (1+ I))
		(PC (MAX (FEF-INITIAL-PC FUNCTION) (- PC-NOW (TRUNCATE NLINES 2)))
		    (+ PC (COMPILER:DISASSEMBLE-INSTRUCTION-LENGTH FUNCTION PC))))
	       ((OR ( I NLINES) ( PC LIM-PC))
		(COND ((= PC PC-NOW)		;If arrow should point after all code,
		       (TERPRI) (PRINC "=> "))))
	     (TERPRI)
	     (PRINC (IF (= PC PC-NOW) "=> " "   "))
	     (COMPILER:DISASSEMBLE-INSTRUCTION FUNCTION PC))
	   ;; This kludge is to prevent the prompt from triggering a **MORE** when it comes out
	   ;; on the bottom line of the window
	   (IF (MEMQ ':NOTICE (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
	       (FUNCALL STANDARD-OUTPUT ':NOTICE ':INPUT-WAIT))))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
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
IF-DOES-NOT-EXIST non-NIL says just return NIL for file-not-found.  Default NIL.
 In all other cases the value is the truename of the loaded file, or T.
PRINT non-NIL says print all forms loaded."
  (DECLARE (ARGLIST FILE &KEY &OPTIONAL PACKAGE VERBOSE SET-DEFAULT-PATHNAME
		    IF-DOES-NOT-EXIST PRINT))
  (IF (AND (CAR KEY-OR-POSITIONAL-ARGS)
	   (MEMQ (CAR KEY-OR-POSITIONAL-ARGS)
		 '(:PACKAGE :PRINT :IF-DOES-NOT-EXIST :SET-DEFAULT-PATHNAME :VERBOSE)))
      (LET ((SI:PRINT-LOADED-FORMS
	      (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':PRINT)))
	(LOAD-1 FILE (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':PACKAGE)
		(GET (LOCF KEY-OR-POSITIONAL-ARGS) ':IF-DOES-NOT-EXIST)
		(NOT (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':SET-DEFAULT-PATHNAME
			  *LOAD-SET-DEFAULT-PATHNAME*))
		(NOT (GET (LOCF KEY-OR-POSITIONAL-ARGS) ':VERBOSE
			  *LOAD-VERBOSE*))))
    (LEXPR-FUNCALL 'LOAD-1 FILE KEY-OR-POSITIONAL-ARGS)))

(DEFUN LOAD-1 (FILE &OPTIONAL PKG NONEXISTENT-OK-FLAG DONT-SET-DEFAULT-P NO-MSG-P
	       &AUX PATHNAME STREAM TYPE TYPE-MAY-DEFAULT)
  ;; Merge everything, defaulting type component to NIL.
  (IF (STREAMP FILE)
      (PROGN
	;; Set the defaults from the pathname we finally opened
	(OR DONT-SET-DEFAULT-P
	    (SET-DEFAULT-PATHNAME (SEND FILE ':PATHNAME) LOAD-PATHNAME-DEFAULTS))
	(CATCH-ERROR-RESTART (ERROR "Give up on loading ~A." (SEND FILE ':PATHNAME))
	  ;; If the file was a character file, read it, else try to fasload it.
	  (FUNCALL (IF (FUNCALL FILE ':CHARACTERS)
		       #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
		   FILE PKG NO-MSG-P)
	  (OR (SEND STREAM ':SEND-IF-HANDLES ':TRUENAME) T)))
    (SETQ PATHNAME (MERGE-PATHNAME-DEFAULTS FILE LOAD-PATHNAME-DEFAULTS NIL))
    (CATCH-ERROR-RESTART (ERROR "Give up on loading ~A." PATHNAME)
      (FILE-RETRY-NEW-PATHNAME (PATHNAME FILE-ERROR)
	(CONDITION-CASE-IF NONEXISTENT-OK-FLAG ()
	    (UNWIND-PROTECT
	     (PROGN
	       (SETQ TYPE (SEND PATHNAME ':TYPE))
	       (SETQ TYPE-MAY-DEFAULT
		     (AND (EQ TYPE ':UNSPECIFIC)
			  (SEND PATHNAME ':UNSPECIFIC-TYPE-IS-DEFAULT)))
	       (OR (AND TYPE
			;; Unless file type is unspecified, try file as given, first.
			;; If type is not :unspecific, this is all we will try,
			;; so get an error on failure in that case.
			(NOT (ERRORP
			       (SETQ STREAM
				     (OPEN PATHNAME ':ERROR (NOT TYPE-MAY-DEFAULT)
					   ':CHARACTERS ':DEFAULT)))))
		   ;; Now, if it is reasonable to try varying the file type,
		   ;; try first the QFASL, then the source.
		   (AND (OR (NULL TYPE)
			    TYPE-MAY-DEFAULT)
			(OR
			  (NOT (ERRORP
				 (SETQ STREAM
				       (OPEN (SEND PATHNAME ':NEW-TYPE
						   (SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE
						     PATHNAME))
					     ':ERROR NIL ':CHARACTERS ':DEFAULT))))
			  ;; If this last one loses, it gets an error.
			  (SETQ STREAM (SEND (SEND PATHNAME ':NEW-TYPE NIL)
					     ':OPEN-CANONICAL-DEFAULT-TYPE ':LISP
					     ':CHARACTERS ':DEFAULT)))))
	       ;; Set the defaults from the pathname we finally opened
	       (OR DONT-SET-DEFAULT-P
		   (SET-DEFAULT-PATHNAME (SEND STREAM ':PATHNAME) LOAD-PATHNAME-DEFAULTS))
	       ;; If the file was a character file, read it, else try to fasload it.
	       (FUNCALL (IF (FUNCALL STREAM ':CHARACTERS)
			    #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
			STREAM PKG NO-MSG-P)
	       (SEND STREAM ':TRUENAME))
	     ;; Cleanup form
	     (AND STREAM (NOT (ERRORP STREAM)) (CLOSE STREAM)))
	  (FILE-NOT-FOUND
	   NIL))))))

))

; From file SUPDUP.LISP SRC:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

;;;Convert to NVT ASCII (except don't convert CR to two characters).
(DEFMETHOD (BASIC-TELNET :NET-OUTPUT-TRANSLATED) (CH)
  (COND ((CONSP CH)
	 (SELECTQ (FIRST CH)
	   (:MOUSE-BUTTON (IF SUPDUP-OUTPUT-FLAG
			      (MOUSE-OUT (FOURTH CH) (FIFTH CH) (SECOND CH))))))
	(T
	 (LET ((CHAR (LDB %%KBD-CHAR CH)))
	   (COND ((NOT ECHO-FLAG)
		  ;; Echo the character.
		  (IF (LDB-TEST %%KBD-CONTROL CH)
		      (FUNCALL SELF ':TYO #/))
		  (FUNCALL SELF ':TYO CHAR)))
	   (COND ((AND SUPDUP-OUTPUT-FLAG
		       (= CHAR #\END))
		  (FUNCALL-SELF ':NET-OUTPUT 30)	;control X
		  (FUNCALL-SELF ':NET-OUTPUT 23))	;control S
		 (T
		  (AND (LDB-TEST %%KBD-CONTROL CH) (SETQ CHAR (LDB 0005 CH)))	;controlify
		  (AND (> CHAR 200) (SETQ CHAR (AREF TELNET-KEYS (- CHAR 200))))
		  (AND (LDB-TEST %%KBD-META CH) (SETQ CHAR (+ CHAR 200)))
		  (FUNCALL-SELF ':NET-OUTPUT CHAR)))))))

))

; From file SUPDUP.LISP SRC:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

(DEFMETHOD (BASIC-SUPDUP :NET-OUTPUT-TRANSLATED) (CH)
  (UNLESS (CONSP CH)
    (LET ((CHAR (LDB %%KBD-CHAR CH)))
      (FUNCALL-SELF ':NET-OUTPUT
		    (LOGIOR (LSH (LDB %%KBD-CONTROL-META CH) 7)
			    (COND ((= CHAR 33) CHAR)	;(Special case)
				  ((< CHAR 40) (LOGIOR CHAR 4000))
				  ((< CHAR 200) CHAR)
				  (T (AREF SUPDUP-KEYS (- CHAR 200)))))))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

;;; Fill in slots in PATHNAME from program defaults.  This is what most
;;; programs interface to.
(DEFUN MERGE-PATHNAME-DEFAULTS (PATHNAME
				&OPTIONAL DEFAULTS
					  (DEFAULT-TYPE *NAME-SPECIFIED-DEFAULT-TYPE*)
					  (DEFAULT-VERSION ':NEWEST)
					  ALWAYS-MERGE-TYPE
				&AUX HOST DEFAULT SECONDARY-DEFAULT
				NEW-DEVICE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION
				NEW-OTYPE)
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
the type and version components, iff a name was specified
and FS:*ALWAYS-MERGE-TYPE-AND-VERSION* is NIL.
Otherwise, the type and version are obtained from DEFAULTS,
and DEFAULT-TYPE and DEFAULT-VERSION are not used.
If ALWAYS-MERGE-TYPE is non-NIL, that forces the type component
to be merged like the name, directory, etc. but has no effect on the version."
  (SETQ PATHNAME (PARSE-PATHNAME PATHNAME NIL DEFAULTS))
  (IF (NULL DEFAULTS)
      (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((NOT (TYPEP PATHNAME 'PATHNAME))
	 PATHNAME)  ;Some funny thing.  No defaulting possible.
	(T
	 ;; Host always comes from pathname
	 (SETQ HOST (PATHNAME-HOST PATHNAME))
	 ;; Setup default pathnames.  If a pathname is supplied as the defaults,
	 ;; then two levels of defaulting are needed, otherwise only one.
	 (IF (NLISTP DEFAULTS)			;if not defaults.
	     (SETQ DEFAULT (PARSE-PATHNAME DEFAULTS NIL PATHNAME)
		   DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*
		   SECONDARY-DEFAULT (DEFAULT-PATHNAME DEFAULTS HOST))
	     (SETQ DEFAULT (DEFAULT-PATHNAME DEFAULTS HOST)
		   SECONDARY-DEFAULT NIL))
	 ;; Device name DSK means the working directory and associated device if any.
	 (COND ((EQUAL (PATHNAME-DEVICE PATHNAME) "DSK")
		(LET ((WDIR (OR (CADR (ASSQ HOST HOST-WORKING-DIRECTORY-ALIST))
				(USER-HOMEDIR HOST))))
		  (SETQ NEW-DEVICE
			(OR (FUNCALL WDIR ':DEVICE)
			    (FUNCALL HOST ':PRIMARY-DEVICE)))
		  (IF (AND (NULL (PATHNAME-DIRECTORY PATHNAME))
			   ;; Don't do this when explicit directory supplied.
			   (NULL (PATHNAME-DIRECTORY DEFAULT))
			   (OR (NULL SECONDARY-DEFAULT)
			       (NULL (PATHNAME-DIRECTORY SECONDARY-DEFAULT))))
		      (SETQ NEW-DIRECTORY
			    (FUNCALL WDIR ':DIRECTORY))))))
	 ;; Merge the device, directory, and name
	 (IF (NULL (PATHNAME-DEVICE PATHNAME))
	     (SETQ NEW-DEVICE
		   (OR (PATHNAME-DEVICE DEFAULT)
		       (AND (NOT (NULL SECONDARY-DEFAULT))
			    (PATHNAME-DEVICE SECONDARY-DEFAULT)))))
	 (UNLESS NEW-DIRECTORY
	   (LET ((PDIR (PATHNAME-DIRECTORY PATHNAME))
		 (DDIR (OR (PATHNAME-DIRECTORY DEFAULT)
			   (AND (NOT (NULL SECONDARY-DEFAULT))
				(PATHNAME-DIRECTORY SECONDARY-DEFAULT)))))
	     (COND ((NULL PDIR)
		    (SETQ NEW-DIRECTORY DDIR))
		   ((AND (CONSP PDIR)
			 (EQ (CAR PDIR) ':RELATIVE))
		    (SETQ NEW-DIRECTORY
			  (MERGE-RELATIVE-DIRECTORY PDIR DDIR))))))
	 (IF (NULL (PATHNAME-NAME PATHNAME))
	     (SETQ NEW-NAME
		   (OR (PATHNAME-NAME DEFAULT)
		       (AND (NOT (NULL SECONDARY-DEFAULT))
			    (PATHNAME-NAME SECONDARY-DEFAULT))
		       ;; Never let the name of the resulting pathname be NIL.
		       "FOO")))
	 ;; Merge the type and version if the name was NIL before the above merge,
	 ;; or if the user says to always do so.
	 (IF (NULL (PATHNAME-TYPE PATHNAME))
	     (IF (OR (NULL (PATHNAME-NAME PATHNAME))
		     ALWAYS-MERGE-TYPE
		     *ALWAYS-MERGE-TYPE-AND-VERSION*)
		 (PROGN
		   (SETF (VALUES NEW-TYPE NEW-OTYPE)
			 (SEND DEFAULT ':CANONICAL-TYPE))
		   (UNLESS NEW-TYPE
		     (SETQ NEW-TYPE 
			   (OR (AND (NOT (NULL SECONDARY-DEFAULT))
				    (PATHNAME-TYPE SECONDARY-DEFAULT))
			       ;; Never let the type of the resulting pathname be NIL.
			       DEFAULT-TYPE))))
	       (SETQ NEW-TYPE DEFAULT-TYPE)))
	 (IF (NULL (PATHNAME-VERSION PATHNAME))
	     (IF (OR (NULL (PATHNAME-NAME PATHNAME))
		     *ALWAYS-MERGE-TYPE-AND-VERSION*)
		 (SETQ NEW-VERSION
		       (OR (PATHNAME-VERSION DEFAULT)
			   (AND (NOT (NULL SECONDARY-DEFAULT))
				(PATHNAME-VERSION SECONDARY-DEFAULT))
			   ;; Never let the version of the resulting pathname be NIL.
			   DEFAULT-VERSION))
	       (SETQ NEW-VERSION DEFAULT-VERSION)))
	 (SEND PATHNAME ':NEW-PATHNAME
	       (IF NEW-DEVICE ':DEVICE) NEW-DEVICE
	       (IF NEW-DIRECTORY ':DIRECTORY) NEW-DIRECTORY
	       (IF NEW-NAME ':NAME) NEW-NAME
	       (IF NEW-TYPE ':TYPE) NEW-TYPE
	       (IF NEW-OTYPE ':ORIGINAL-TYPE) NEW-OTYPE
	       (IF NEW-VERSION ':VERSION) NEW-VERSION))))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "


(DEFUN OPEN-CHAOS (HOST PATHNAME &KEY (DIRECTION ':INPUT) (CHARACTERS T)
		   (ERROR T) (ACCESS-ERROR (NOT ERROR))
		   (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
		   (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
					;; :UNSPECIFIC here is to prevent lossage
					;; writing ITS files with no version numbers.
					'(:NEWEST :UNSPECIFIC))
				  ':NEW-VERSION ':ERROR)
			      IF-EXISTS-P)
		   (IF-DOES-NOT-EXIST
		     (COND ((MEMQ DIRECTION '(:PROBE))
			    NIL)
			   ((AND (EQ DIRECTION ':OUTPUT)
				 (NOT (MEMQ IF-EXISTS '(:OVERWRITE :APPEND))))
			    ':CREATE)
			   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			   ;; for compatibility with the past.
			   ;; A Common-Lisp program would use :PROBE
			   ;; and get NIL as the default for this.
			   (T ':ERROR)))
		   TEMPORARY DELETED RAW SUPER-IMAGE (BYTE-SIZE ':DEFAULT)
		   PRESERVE-DATES IGNORE
		   &AUX HOST-UNIT DATA-CONN PKT SUCCESS STRING NOT-ABORTED
		   PHONY-CHARACTERS SIGN-EXTEND-BYTES
		   (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SI:CCASE DIRECTION
    ((:INPUT :OUTPUT))
    (:IO (FERROR NIL "Bidirectional file streams are not supported."))
    ((NIL :PROBE) (SETQ DIRECTION NIL)))
  (CHECK-TYPE IF-EXISTS (MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
				:OVERWRITE :APPEND :SUPERSEDE NIL))
  (CHECK-TYPE IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
  (WHEN ELEMENT-TYPE-P
    (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
	  (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
  (FILE-OPERATION-RETRY
    (CONDITION-CASE-IF ACCESS-ERROR (ERROR-OBJECT)
        (PROGN
	  (IF (NULL DIRECTION)
	      ;;PROBE mode implies no need for data connection
	      (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT))
	    (MULTIPLE-VALUE (DATA-CONN HOST-UNIT)
	      (FUNCALL HOST ':GET-DATA-CONNECTION DIRECTION))))
      (REMOTE-NETWORK-ERROR ERROR-OBJECT)
      (:NO-ERROR
       (UNWIND-PROTECT
	 (PROGN
	   (MULTIPLE-VALUE (PKT SUCCESS STRING)
	     (FUNCALL HOST-UNIT ':COMMAND NIL
		      (SELECTQ DIRECTION
			(:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			(:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
		      NIL
		      "OPEN " (SELECTQ DIRECTION
				((NIL) "PROBE")
				(:INPUT "READ")
				(:OUTPUT "WRITE"))
		      " " (SELECTQ CHARACTERS
			    ((NIL) "BINARY")
			    (:DEFAULT "DEFAULT")
			    (T "CHARACTER"))
		      (IF (AND (EQ DIRECTION ':OUTPUT)
			       (NEQ IF-EXISTS
				    (IF (MEMQ (PATHNAME-VERSION PATHNAME)
					      '(:NEWEST :UNSPECIFIC))
					':NEW-VERSION ':SUPERSEDE)))
			  (STRING-APPEND " IF-EXISTS "
					 (IF (EQ IF-EXISTS NIL)
					     ':ERROR
					   IF-EXISTS))
			"")
		      (IF (OR IF-EXISTS-P
			      (NEQ IF-DOES-NOT-EXIST
				   (SELECTQ DIRECTION
				     ((:INPUT NIL) ':ERROR)
				     (:OUTPUT ':CREATE))))
			  (STRING-APPEND " IF-DOES-NOT-EXIST "
					 (IF (EQ IF-DOES-NOT-EXIST NIL)
					     ':ERROR
					   IF-DOES-NOT-EXIST))
			"")
		      (FORMAT NIL "~:[ BYTE-SIZE ~D~;~*~]~:[~; TEMPORARY~]~:[~; DELETED~]~
			      ~:[~; RAW~]~:[~; SUPER~]~:[~; PRESERVE-DATES~]~%~A~%"
			      (EQ BYTE-SIZE ':DEFAULT) BYTE-SIZE
			      TEMPORARY DELETED RAW SUPER-IMAGE PRESERVE-DATES
			      (FILE-PRINT-PATHNAME SELF))))
	   (COND ((NOT SUCCESS)
		  (SETQ NOT-ABORTED T)
		  (SETQ STRING (STRING-APPEND STRING))
		  (AND PKT (CHAOS:RETURN-PKT PKT))
		  (OR (NULL DATA-CONN)
		      (SETF (DATA-STREAM DATA-CONN DIRECTION) NIL))
		  (CONDITION-CASE-IF (NOT IF-DOES-NOT-EXIST)
				     ()
		      (CONDITION-CASE-IF (NOT IF-EXISTS)
					 ()
			  (FILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR) ':OPEN)
			(FILE-ALREADY-EXISTS NIL))
		    (FILE-NOT-FOUND NIL)))
		 (T
		  (LET ((PROPERTIES (READ-FILE-PROPERTY-LIST-STRING STRING "OPEN" PATHNAME)))
		    (CHAOS:RETURN-PKT PKT)
		    (AND (EQ CHARACTERS ':DEFAULT)
			 (SETQ CHARACTERS (GET (LOCF PROPERTIES) ':CHARACTERS)))
		    (UNLESS (OR (EQ BYTE-SIZE ':DEFAULT)
				(GET (LOCF PROPERTIES) ':BYTE-SIZE))
		      (SETF (GET (LOCF PROPERTIES) ':BYTE-SIZE) BYTE-SIZE))
		    (PROG1
		      (MAKE-INSTANCE (SELECTQ DIRECTION
				       ((NIL) 'FILE-PROBE-STREAM)
				       (:INPUT
					(IF CHARACTERS
					    'FILE-INPUT-CHARACTER-STREAM
					  (COND (SIGN-EXTEND-BYTES
						 'FILE-INPUT-SIGNED-BINARY-STREAM)
						(PHONY-CHARACTERS
						 'FILE-INPUT-PHONY-CHARACTER-STREAM)
						(T
						 'FILE-INPUT-BINARY-STREAM))))
				       (:OUTPUT
					(IF CHARACTERS
					   'FILE-OUTPUT-CHARACTER-STREAM
					  (IF PHONY-CHARACTERS
					      'FILE-OUTPUT-PHONY-CHARACTER-STREAM
					    'FILE-OUTPUT-BINARY-STREAM))))
				     ':HOST-UNIT HOST-UNIT
				     ':DATA-CONNECTION DATA-CONN
				     ':PROPERTY-LIST PROPERTIES
				     ':PATHNAME PATHNAME)
		      (SETQ NOT-ABORTED T))))))
	 (UNLESS (OR NOT-ABORTED
		     (NULL DATA-CONN)
		     (NULL (SEND HOST-UNIT ':CONTROL-CONNECTION)))
	   ;; Here if aborted out of it and server may have file open.
	   (CONDITION-CASE ()
	       (PROGN
		(AND (EQ DIRECTION ':OUTPUT)
		     (FUNCALL HOST-UNIT ':COMMAND NIL
			      (DATA-OUTPUT-HANDLE DATA-CONN) NIL "DELETE"))
		(MULTIPLE-VALUE-BIND (NIL CLOSE-SUCCESS)
		    (FUNCALL HOST-UNIT ':COMMAND
			     NIL
			     (SELECTQ DIRECTION
			       (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			       (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			     NIL "CLOSE")
		  (WHEN CLOSE-SUCCESS
		    (SELECTQ DIRECTION
		      (:INPUT (READ-UNTIL-SYNCHRONOUS-MARK (DATA-CONNECTION DATA-CONN)))
		      (:OUTPUT (CHAOS:SEND-PKT (DATA-CONNECTION DATA-CONN)
					       (CHAOS:GET-PKT) %FILE-SYNCHRONOUS-MARK-OPCODE)))))
		(FUNCALL HOST-UNIT ':FREE-DATA-CONNECTION DATA-CONN DIRECTION))
	     (SYS:HOST-STOPPED-RESPONDING NIL))))))))

(DEFUN DECODE-ELEMENT-TYPE (ELEMENT-TYPE BYTE-SIZE)
  (DECLARE (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES))
  (IF (ATOM ELEMENT-TYPE)
      (SELECTQ ELEMENT-TYPE
	(:DEFAULT 
	 (VALUES ':DEFAULT BYTE-SIZE))
	(BIT
	 (VALUES NIL 1))
;No way to find out what byte size was used in this case.
;	(SIGNED-BYTE
;	 (VALUES NIL ':DEFAULT NIL T))
	(UNSIGNED-BYTE
	 (VALUES NIL ':DEFAULT))
	(STRING-CHAR
	 (VALUES T ':DEFAULT))
	(STANDARD-CHAR
	 (VALUES T ':DEFAULT))
	(CHARACTER
	 (VALUES NIL 16. T))
	(T (FERROR 'UNIMPLEMENTED-OPTION "~S is not implemented as an ELEMENT-TYPE."
		   ELEMENT-TYPE)))
    (SELECTQ (CAR ELEMENT-TYPE)
      (UNSIGNED-BYTE
       (VALUES NIL (CADR ELEMENT-TYPE)))
      (SIGNED-BYTE
       (VALUES NIL (CADR ELEMENT-TYPE) NIL T))
      (MOD
       (VALUES NIL (HAULONG (1- (CADR ELEMENT-TYPE)))))
      (T (FERROR 'UNIMPLEMENTED-OPTION "~S is not implemented as an ELEMENT-TYPE."
		 ELEMENT-TYPE)))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFMETHOD (VANILLA-FLAVOR :BREAK) ()
  (WITH-SELF-VARIABLES-BOUND (BREAK "~S" SELF)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defmacro defstruct-putprop-compile-time (sym val ind)
  `(push `(defdecl ,,sym ,,ind ,,val) returns))

(DEFUN DEFSTRUCT-1 (OPTIONS ITEMS CLIP)         ;CLIP means "common lisp, incompatible"-p
  (let* ((description (defstruct-parse-options options CLIP))
         (type-description (get (defstruct-description-type)
                                'defstruct-type-description))
         (name (defstruct-description-name))
         (DOC (AND (STRINGP (CAR ITEMS)) (POP ITEMS)))
         (new-slots (defstruct-parse-items items description))	;now all slots -- mly
         (returns nil))
    ;;Keep the returns from this as close to last as possible
    ;;Evaluate this before everything else
    (AND (defstruct-type-description-defstruct-expander)
         (setq returns (funcall (defstruct-type-description-defstruct-expander) description)))
    (SETQ RETURNS
          (APPEND RETURNS
                  ;;This must be the last returned form, since to compile it
                  ;;might require that the structure already be operable:
                  (IF (DEFSTRUCT-DESCRIPTION-PRINT)
                      (LIST (DEFSTRUCT-DEFINE-PRINTER NAME (DEFSTRUCT-DESCRIPTION-PRINT))))
                  ;;Return the name symbol as our value
                  `(',NAME)))
 #+(and LispM (NOT MIT))
    (push `(record-source-file-name ',name 'defstruct) returns)
 #+(and LispM MIT)
    (push `(eval-when (load eval) (record-source-file-name ',name 'defstruct)) returns)
 #+(AND LISPM MIT)				;not really just mit lispm, but any clisp
    (AND DOC (PUSH `(SETF (DOCUMENTATION ',NAME 'STRUCTURE) ,DOC) RETURNS))
    (let ((alterant (defstruct-description-alterant))
          (size-macro (defstruct-description-size-macro))
          (size-symbol (defstruct-description-size-symbol))
          (predicate (defstruct-description-predicate))
          (copier (defstruct-description-copier)))
      (cond (predicate
             (push (funcall (or (defstruct-type-description-predicate)
                                (defstruct-error
                                  "This DEFSTRUCT type cannot produce a predicate"
                                  (defstruct-description-type) 'in name))
                            description
                            predicate)
                   returns)))
      (cond (copier
             (push
               (let ((copy-fun (defstruct-type-description-copier)))
                 (cond (copy-fun
                        (funcall copy-fun description copier))
                       ((not (= 1 (defstruct-type-description-ref-no-args)))
                        (defstruct-error
                          "This defstruct type cannot produce a copying function"
                          (defstruct-description-type) 'in name))
                       (t (do ((i (1- (defstruct-description-size)) (1- i))
                               (l nil (cons (cons i
                                                  (funcall
                                                    (defstruct-type-description-ref-expander)
                                                    i description 'x))
                                            l)))
                              ((< i 0)
                               `(defun ,copier (x)
                                  ,(invoke-defstruct-constructor-expander
                                     description type-description l nil)))))))
               returns)))
      (cond (alterant
             (defstruct-put-macro alterant 'defstruct-expand-alter-macro)
             (defstruct-putprop alterant name 'defstruct-name)))
      (cond (size-macro
             (defstruct-put-macro size-macro 'defstruct-expand-size-macro)
             (defstruct-putprop size-macro name 'defstruct-name)))
      (cond (size-symbol
             (push `(defconst ,size-symbol
                      ,(+ (defstruct-description-size)
                          (defstruct-type-description-overhead)))
                   returns))))
    (defstruct-putprop-compile-time name description 'defstruct-description)
    ;;what defstruct returns
    #-(OR LISPM NIL)			;retain eval-when so as not to cause hidden screws
    `(eval-when ,(defstruct-description-eval-when)
       ,.(defstruct-define-ref-macros new-slots description)
       ,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,.returns)
    #+(OR LISPM NIL)			;losing eval-when flushed!! 
    `(PROGN
       ,.(defstruct-define-ref-macros new-slots description)
       ,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,.returns)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun get-defstruct-description (name)
  (let ((description (getdecl name 'defstruct-description)))
    (cond ((null description)
           (defstruct-error
             "A structure with this name has not been defined" name))
          ((not (eq (defstruct-description-version) 'one))
           (defstruct-error "The internal description of this structure is
incompatible with the currently loaded version of DEFSTRUCT,
you will need to recompile its definition"
                  name))
          (t description))))

(defun defstruct-parse-items (items description)
  (let ((name (defstruct-description-name))
	(offset (defstruct-description-initial-offset))
	(include (defstruct-description-include))
	(o-slot-alist nil)
	(conc-name (defstruct-description-conc-name))
	#+MacLisp-10 (chars (exploden conc-name)))
    (or (null include)
	(let ((d (get-defstruct-description (car include))))
	  (setq offset (+ offset (defstruct-description-size d))) 
	  (setq o-slot-alist
		(COPYTREE (defstruct-description-slot-alist d)))
	  (DOLIST (L O-SLOT-ALIST)
	    (SETF (DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR L))
		  (IF CONC-NAME #+MACLISP-10 (IMPLODE (APPEND CHARS (EXPLODEN (CAR L))))
		                #-MACLISP-10 (DEFSTRUCT-APPEND-SYMBOLS CONC-NAME (CAR L))
		      (CAR L))))
	  (DOLIST (L (CDR INCLUDE))
	    (LET* ((IT (IF (CONSP L) (CAR L) L))
		   (REST (IF (CONSP L) (CDR L) NIL))
		   (slot-description (cdr (assq it o-slot-alist))))
	      (if (null slot-description)
		(defstruct-error
		  "Unknown slot in :INCLUDEd defstruct"
		  it 'in include 'included 'by name))
	      (DEFSTRUCT-PARSE-ONE-FIELD
		IT NIL NIL REST CONC-NAME #+MACLISP-10 (EXPLODEN CONC-NAME) SLOT-DESCRIPTION)))))
    (do ((i offset (1+ i))
	 (l items (cdr l))
	 (slot-alist nil)
	 )
	((null l)
	 (setq slot-alist (nreverse slot-alist))
	 (setf (defstruct-description-size) i)
	 (setf (defstruct-description-slot-alist)
	       (nconc o-slot-alist slot-alist)))	;now returns ALL slots
      (cond ((atom (car l))
	     (push (defstruct-parse-one-field
		     (car l) i nil nil conc-name #+MacLisp-10 chars)
		   slot-alist))
	    ((atom (caar l))
	     (push (defstruct-parse-one-field
		     (caar l) i nil (cdar l) conc-name #+MacLisp-10 chars)
		   slot-alist))
	    (t
	     (do ((ll (car l) (cdr ll)))
		 ((null ll))
	       (push (defstruct-parse-one-field
		       (caar ll) i (cadar ll)
		       (cddar ll) conc-name #+MacLisp-10 chars)
		     slot-alist)))))))

))
