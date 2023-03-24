;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11/28/83 10:28:15 by rms,
;;; Reason: Connection errors in OPEN.  Package not found errors in READ.
;;; Error message for redefinition queries.
;;; Compilation of Common Lisp package functions at top level.
;;; HISTORY-ELEMENT-SET-YANK-POINTER.  C-M-Y in minibuffer.
;;; Don't record interval-stream output for undo.  Query-replace on region bug.
;;; M-Status and C-M-Status in RH.
;;; Printing arrays doesn't cons.  Reading/printing 0-rank arrays.
;;; Multiple notifications don't overwrite when in RH.
;;; Pay attention to buffers not really visiting files.  Eliminate BUFFER-MUNGED-P.
;;; Common Lisp LOOP compatibility.
;;; Clear-Screen redisplays properly when there had been typeout.
;;; Compiler blowouts with comma not in backquote.
;;; UNDEFFLAVOR.  Combination type accepted as method type.
;;; while running on Lisp Machine Eighteen from band 7
;;; with Experimental System 98.0, CADR 3.0, Experimental ZMail 53.0, MIT-Specific 22.0, microcode 301, ZM MIT.



; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

;;; Stream generating functions

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
    (IF (ATOM ELEMENT-TYPE)
	(SELECTQ ELEMENT-TYPE
	  (:DEFAULT 
	   (SETQ CHARACTERS ':DEFAULT))
	  (BIT
	   (SETQ CHARACTERS NIL BYTE-SIZE 1))
	  (SIGNED-BYTE
	   (SETQ CHARACTERS NIL BYTE-SIZE ':DEFAULT
		 SIGN-EXTEND-BYTES T))
	  (UNSIGNED-BYTE
	   (SETQ CHARACTERS NIL BYTE-SIZE ':DEFAULT))
	  (STRING-CHAR
	   (SETQ CHARACTERS T))
	  (STANDARD-CHAR
	   (SETQ CHARACTERS T))
	  (CHARACTER
	   (SETQ CHARACTERS NIL BYTE-SIZE 16. PHONY-CHARACTERS T)))
      (SELECTQ (CAR ELEMENT-TYPE)
	(UNSIGNED-BYTE
	 (SETQ CHARACTERS NIL BYTE-SIZE (CADR ELEMENT-TYPE)))
	(SIGNED-BYTE
	 (SETQ CHARACTERS NIL BYTE-SIZE (CADR ELEMENT-TYPE)
	       SIGN-EXTEND-BYTES T))
	(MOD
	 (SETQ CHARACTERS NIL BYTE-SIZE (HAULONG (1- (CADR ELEMENT-TYPE))))))))
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
				     (:INPUT ':ERROR)
				     (:OUTPUT ':CREATE)
				     ((NIL) NIL))))
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

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "


(DEFMETHOD (READ-ERROR :CASE :PROCEED-ASKING-USER :NEW-PACKAGE) (CONTINUATION READ-OBJECT-FUNCTION)
  "Proceeds, asking for a package name to use instead."
  (FUNCALL CONTINUATION ':NEW-PACKAGE
	   (FUNCALL READ-OBJECT-FUNCTION ':STRING "Package name: ")))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(globalize "READ-PACKAGE-NOT-FOUND" 'sys)

(DEFSIGNAL READ-PACKAGE-NOT-FOUND READ-ERROR (PACKAGE-NAME)
	   "Package prefix not recognized as a package name.")

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

;FOO: switches us to the package associated with the string "FOO"
;FOO:: is similar but overrides any local nicknames,
(DEFUN (PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
       LAST-CH ;ignored
       (PROG (THING TYPE PK
	      ;; Help un-screw the user if PACKAGE gets set to NIL.
	      (PACKAGE (OR PACKAGE PKG-USER-PACKAGE))
	      NO-LOCAL-NICKNAMES ENTIRE-LIST-PREFIXED)
	     ;; Gobble the second colon, if any, and set flag if found.
	     (MULTIPLE-VALUE-BIND (CH NUM REAL-CH)
		 (XR-XRTYI STREAM NIL T)
	       (IF (= CH #/:)
		   (SETQ NO-LOCAL-NICKNAMES T)
		 (IF (= CH #/()
		     (SETQ ENTIRE-LIST-PREFIXED T))
		 (XR-XRUNTYI STREAM REAL-CH NUM)))
	     ;; Try to find the package.
	     (DO ((STRING1 (OR STRING "")))
		 ;;don't try to find packages if we're not interning -- eg +slime (dis:foo)
		 ((OR READ-DONT-INTERN
		      ;; Look, counting local nicknames unless this is a double-colon.
		      (SETQ PK (FIND-PACKAGE STRING1
					     (UNLESS NO-LOCAL-NICKNAMES PACKAGE)))))
	       ;; Package not found.
	       (SIGNAL-PROCEED-CASE ((PKG) 'SYS:READ-PACKAGE-NOT-FOUND
					       "Package ~S does not exist."
					       STRING1)
		 (:NO-ACTION
		  (RETURN))
		 (:NEW-PACKAGE
		  (LET ((PACKAGE PKG-USER-PACKAGE))
		    (SETQ STRING1 (STRING (READ-FROM-STRING PKG)))))))
	     (UNLESS PK
	       (SETQ PK PKG-USER-PACKAGE))
	     (WHEN STRING (RETURN-READ-STRING STRING))
	     (LET ((PACKAGE PK)
		   (READ-INTERN-FUNCTION
		     (COND ((AND (PACKAGE-AUTO-EXPORT-P PK)
				 (PACKAGE-USED-BY-LIST PK))
			    'READ-INTERN-SOFT)
			   (T 'INTERN))
		     ;; This change may occur only in Common Lisp.
		     #| (IF (OR ENTIRE-LIST-PREFIXED (EQ PK PACKAGE))
			 ;; Here for, e.g., SI: while in SI already.
			 ;; There are things in LOOP which MUST say "SI:" even though
			 ;; loop is loaded into SI on the Lisp machine.
			 ;; Also here for ZWEI:(BP-LINE (POINT));
			 ;; such constructs are not valid Common Lisp
			 ;; so let's keep their meaning the same.
			 READ-INTERN-FUNCTION
		       (IF READ-COLON-ALLOW-INTERNALS
			   'READ-PACKAGE-PREFIX-INTERN
			 'READ-PACKAGE-PREFIX-EXTERNAL-INTERN)) |#))
	       (MULTIPLE-VALUE (THING TYPE)
		 (READ STREAM)))
	     (RETURN THING TYPE T)))   ;T means we already did RETURN-READ-STRING

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMETHOD (WARNING :PRINT-ERROR-MESSAGE-PREFIX) (IGNORE IGNORE STREAM)
  (PRINC ">>WARNING: " STREAM))

))

; From file QMISC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

;; Query about any irregularities about redefining the given function symbol now.
;; Return T to tell caller to go ahead and redefine the symbol
;; (no problems or user says ok), NIL to leave it unchanged.
(DEFUN QUERY-ABOUT-REDEFINITION (FUNCTION-SPEC NEW-PATHNAME TYPE OLD-PATHNAME)
  ;; Detect any cross-file redefinition worth complaining about.
  (COND ((OR (EQ (IF (STRINGP OLD-PATHNAME) OLD-PATHNAME
		     (AND OLD-PATHNAME (FUNCALL OLD-PATHNAME ':TRANSLATED-PATHNAME)))
		 (IF (STRINGP NEW-PATHNAME) NEW-PATHNAME
		     (AND NEW-PATHNAME (FUNCALL NEW-PATHNAME ':TRANSLATED-PATHNAME))))
	     (MEMQ OLD-PATHNAME
		   (IF NEW-PATHNAME
		       (FUNCALL NEW-PATHNAME ':GET ':REDEFINES-FILES)
		     NON-PATHNAME-REDEFINED-FILES)))
	 T)
	(T
	 ;; This redefinition deserves a warning or query.
	 ;; If it is within a file operation with warnings,
	 ;; record a warning.
	 (WHEN (AND (VARIABLE-BOUNDP FILE-WARNINGS-DATUM) FILE-WARNINGS-DATUM)
	   (RECORD-AND-PRINT-WARNING 'REDEFINITION ':PROBABLE-ERROR NIL
			   (IF NEW-PATHNAME
			       "~A ~S being redefined by file ~A.
 It was previously defined by file ~A."
			     "~A ~S being redefined;~* it was previously defined by file ~A.")
			   (OR (GET TYPE 'DEFINITION-TYPE-NAME) TYPE) FUNCTION-SPEC
			   NEW-PATHNAME OLD-PATHNAME))
	 (LET (CONDITION CHOICE)
	   (SETQ CONDITION
		 (MAKE-CONDITION 'SYS:REDEFINITION
				 (IF NEW-PATHNAME
				     "~A ~S being redefined by file ~A.
It was previously defined by file ~A."
				   "~A ~S being redefined;~* it was previously defined by file ~A.")
				 (OR (GET TYPE 'DEFINITION-TYPE-NAME) TYPE)
				 FUNCTION-SPEC
				 NEW-PATHNAME OLD-PATHNAME))
	   (SETQ CHOICE (SIGNAL CONDITION))
	   (UNLESS CHOICE
	     (UNLESS (AND INHIBIT-FDEFINE-WARNINGS
			  (NEQ INHIBIT-FDEFINE-WARNINGS ':JUST-WARN))
	       (FORMAT QUERY-IO "~&~A" CONDITION))
	     (IF INHIBIT-FDEFINE-WARNINGS
		 (SETQ CHOICE T)
	       (SETQ CHOICE
		     (FQUERY '(:CHOICES (((ERROR "Error.") #/E)
					 ((PROCEED "Proceed.") #/P)
					 . #.FORMAT:Y-OR-N-P-CHOICES)
					:HELP-FUNCTION
					(LAMBDA (STREAM &REST IGNORE)
					  (PRINC "
  Type Y to proceed to redefine the function, N to not redefine it, E to go into the
  error handler, or P to proceed and not ask in the future (for this pair of files): "
						 STREAM))
					:CLEAR-INPUT T
					:FRESH-LINE NIL
					:SELECT T)
			     " OK? "))))
	   (SELECTQ CHOICE
	     ((T :NO-ACTION) T)
	     ((NIL :INHIBIT-DEFINITION) NIL)
	     (ERROR
	      (ERROR CONDITION)
	      T)
	     (PROCEED
	      (IF NEW-PATHNAME
		  (PUSH OLD-PATHNAME (GET NEW-PATHNAME ':REDEFINES-FILES))
		(PUSH OLD-PATHNAME NON-PATHNAME-REDEFINED-FILES))
	      T))))))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

;(COMPILE-DRIVER form processing-function override-fn) should be used by anyone
;trying to do compilation of forms from source files, or any similar operation.
;It knows how to decipher DECLAREs, EVAL-WHENs, DEFUNs, macro calls, etc.
;It doesn't actually compile or evaluate anything,
;but instead calls the processing-function with two args:
; a form to process, and a flag which is one of these atoms:
;  SPECIAL  -  QC-FILE should eval this and put it in the FASL file.
;  DECLARE  -  QC-FILE should eval this.
;  DEFUN    -  QC-FILE should compile this and put the result in the FASL file.
;  MACRO    -  This defines a macro.  QC-FILE should eval this and also
;               put it in the FASL file.
;  RANDOM   -  QC-FILE should just put this in the FASL file to be evalled.
;Of course, operations other than QC-FILE will want to do different things
;in each case, but they will probably want to distinguish the same cases.
;That's why COMPILE-DRIVER will be useful to them.

;override-fn gets to look at each form just after macro expansion.
;If it returns T, nothing more is done to the form.  If it returns NIL,
;the form is processed as usual (given to process-fn, etc.).
;override-fn may be NIL.

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN)
  (PROG TOP (FN TEM TEM1 (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	((ATOM FORM))
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM TOP NIL))
      (SETQ NFORM
	    (WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S at top level"
			     (CAR FORM))
	      (MACROEXPAND-1 FORM T)))
      (IF (EQ FORM NFORM) (RETURN)
	(SETQ FORM NFORM)))
    ;; If this was a top-level macro, supply a good guess
    ;; for the function-parent for any DEFUNs inside the expansion.
    (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (COND ((ATOM FORM))
	    ((AND (NEQ FORM OFORM) (SYMBOLP (CADR OFORM)))
	     (PUSH `(FUNCTION-PARENT ,(CADR OFORM)) LOCAL-DECLARATIONS))
	    ((MEMQ (CAR OFORM) '(DEFSTRUCT :DEFSTRUCT))
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (COND ((ATOM FORM))
	    ((EQ (CAR FORM) 'EVAL-WHEN)
	     (OR (AND (OR (NOT (ATOM (CADR FORM))) (NULL (CADR FORM)))	;LISTP eventually
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE :EVAL :LOAD :COMPILE))))
		 (FERROR NIL "~S invalid EVAL-WHEN times;
	 must be a list of EVAL, LOAD, and//or COMPILE."
			     (CADR FORM)))
	     (SETQ TEM (OR (MEMQ 'COMPILE (CADR FORM)) (MEMQ ':COMPILE (CADR FORM))))
	     (SETQ TEM1 (OR (MEMQ 'LOAD (CADR FORM)) (MEMQ ':LOAD (CADR FORM))))
	     (DOLIST (FORM1 (CDDR FORM))
	       (IF (AND (CONSP FORM1) (MEMQ (CAR FORM1) '(EVAL-WHEN :EVAL-WHEN)))
		   ;; Another EVAL-WHEN within the first!
		   ;; Intersect the times at which they want to operate.
		   (LET (TIMES)
		     (AND TEM
			  (OR (MEMQ 'COMPILE (CADR FORM1)) (MEMQ ':COMPILE (CADR FORM1)))
			  (PUSH 'COMPILE TIMES))
		     (AND TEM1 (OR (MEMQ 'LOAD (CADR FORM1)) (MEMQ ':LOAD (CADR FORM1)))
			  (PUSH 'LOAD TIMES))
		     (WHEN TIMES
		       (COMPILE-DRIVER `(EVAL-WHEN ,TIMES . ,(CDDR FORM1))
				       PROCESS-FN OVERRIDE-FN)))
		 ;; An element which is not an EVAL-WHEN.
		 ;; Treat COMPILE and LOAD case independently.
		 ;; This can eval something twice in the COMPILE LOAD case,
		 ;; but the other alternative is to fail to eval some things at all.
		 (WHEN TEM
		   (FUNCALL PROCESS-FN FORM1 'DECLARE))
		 (WHEN TEM1
;For Common Lisp's sake, (EVAL-WHEN (LOAD) ...) should be treated by the compiler
;like no EVAL-WHEN at all.
		   (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN)))))
	    ((EQ (SETQ FN (CAR FORM)) 'DEFF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ FN 'DEF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDDR FORM)))
	    ((EQ FN 'WITH-SELF-ACCESSIBLE)
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDDR FORM)))
	    ((EQ FN 'PROGN)
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDR FORM)))
	    ((MEMQ FN '(MACRO DEFSUBST DEFF-MACRO))
	     (FUNCALL PROCESS-FN FORM 'MACRO))
	    ((MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
				EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT))
	     (FUNCALL PROCESS-FN FORM 'SPECIAL))
	    ((EQ FN 'DECLARE)
	     (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	    ((EQ FN 'COMMENT) NIL)
	    ((EQ FN 'PATCH-SOURCE-FILE)
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
			     PROCESS-FN OVERRIDE-FN)
	     (MAPC (FUNCTION (LAMBDA (FORM)
			       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDDR FORM))
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING NIL))
			     PROCESS-FN OVERRIDE-FN))
	    ((EQ FN 'COMPILER-LET)
	     (EVAL `(LET ,(CADR FORM) (COMPILE-DRIVER '(PROGN 'COMPILE . ,(CDDR FORM))
						      ',PROCESS-FN ',OVERRIDE-FN))))
	    ((EQ FN 'DEFUN)
	     (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed defun")
	       (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
	     (COND ((EQ (CDR TEM) (CDR FORM))
		    (IF (NULL (CDDR TEM))
			(WARN 'MALFORMED-DEFUN ':IMPOSSIBLE
			      "Malformed defun ~S" FORM)
		      (FUNCALL PROCESS-FN FORM 'DEFUN)))
		   (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN))))
	    (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))

))

; From file HISTORY.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HISTORY  "

(DEFUN HISTORY-ELEMENT-SET-YANK-POINTER (HISTORY INDEX)
  "Shift the yank pointer of HISTORY to INDEX and returning the element at that index.
If INDEX is NIL, the current yank pointer is used.
This is right for a Yank command with no argument.
A Yank command with a numeric arg can pass it right along."
  (WHEN INDEX
    (IF *HISTORY-ROTATE-IF-NUMERIC-ARG*
	(SETF (HISTORY-YANK-POINTER HISTORY)
	      (IF (MINUSP INDEX)
		  (+ (HISTORY-YANK-POINTER HISTORY) INDEX)
		(+ (HISTORY-YANK-POINTER HISTORY) INDEX -1)))
      (SETF (HISTORY-YANK-POINTER HISTORY)
	    (IF (MINUSP INDEX)
		(+ 1 (HISTORY-LENGTH HISTORY) INDEX)
	      INDEX))))
  (UNLESS (HISTORY-YANK-POINTER HISTORY)
    (SETF (HISTORY-YANK-POINTER HISTORY) 1))
  (NTH (1- (HISTORY-YANK-POINTER HISTORY))
       (HISTORY-LIST HISTORY)))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(set-comtab *mini-buffer-multi-line-comtab* '(#\C-M-Y COM-POP-MINI-BUFFER-HISTORY))
))

; From file DEFS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFFLAVOR INTERVAL-STREAM
	   (**INTERVAL** *LINE* *INDEX* *LAST-LINE* *LAST-INDEX* *STOP-INDEX* (*EOF* NIL)
	    NO-UNDO-SAVING)
	   (SI:BIDIRECTIONAL-STREAM)
  :INITABLE-INSTANCE-VARIABLES)

))

; From file METH.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

;;; Interval I/O

;;; Note: DEFFLAVORs related to this stuff are now found in SYS: ZWEI; DEFS.
;;; DEFMETHODS are found in SYS: ZWEI; METH.

;;; Edited to use flavors by Dulcey 10-Jan-83
;;; Variable used by stream renamed to **INTERVAL** to avoid compiler error messages
;;; caused by *INTERVAL* being a special variable in Zwei
;;;
;;; HACK-FONTS T means return 's for font changes
;;; HACK-FONTS :TYI means return 16 bit characters

;;; *LINE*, *INDEX* point to the next character to be returned.
;;; *STOP-INDEX* is the place on the current line at which to stop (usually the end).
;;; *LAST-LINE*, *LAST-INDEX* is where the interval ends.
;;; If *INDEX* is NIL, we are at the end-of-file.

;;; Font hacking stream
;;; Edited to use flavors by Dulcey 10-Jan-83
;;; Instance variable renamed to **FONT** to avoid compiler errors (since *FONT* is special)

;;; *FONT-FLAG* is normally NIL.  After a  which starts a font change, it is T.
;;; On input, it can also be a string of characters to read before
;;;  the next character from the interval.  This is used in describing diagram lines.
;;; On output, it can also be various things such as DIAG-1, DIAG-2 or an array
;;;  which are used in creating diagram lines.

;;; *FONT-STACK* is an art-q-list array used for doing ^F*.
;;; Numeric font changes push the previous font, and ^F*'s pop from it.
;;; It is initially empty.  If it gets full, the bottom 20. elements are flushed.

(DEFUN INTERVAL-STREAM (FROM-BP &OPTIONAL TO-BP IN-ORDER-P HACK-FONTS NO-UNDO-SAVING)
  "Return a stream that does i//o to the specified interval.
Input reads that text, and output inserts wherever input had got to.
If only output is done, it inserts at the beginning of the interval.
HACK-FONTS = T means return  prefixes if the text contains multiple fonts.
HACK-FONTS = :TYI means return characters with fonts if the text contains them.
NO-UNDO-SAVING non-NIL means do not record stream output to be undone."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (MAKE-INSTANCE (SELECTQ HACK-FONTS
		   (NIL 'INTERVAL-STREAM)
		   (:TYI 'INTERVAL-STREAM-FAT)
		   (T 'INTERVAL-STREAM-WITH-FONTS))
		 ':**INTERVAL** (CREATE-INTERVAL FROM-BP TO-BP)
		 ':NO-UNDO-SAVING NO-UNDO-SAVING
		 ':*LINE* (BP-LINE FROM-BP)
		 ':*INDEX* (BP-INDEX FROM-BP)
		 ':*LAST-LINE* (BP-LINE TO-BP)
		 ':*LAST-INDEX* (BP-INDEX TO-BP)
		 ':*STOP-INDEX* (IF (EQ (BP-LINE FROM-BP) (BP-LINE TO-BP))
				    (BP-INDEX TO-BP)
				  (LINE-LENGTH (BP-LINE FROM-BP)))))

))

; From file METH.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

;; Note: I have not yet looked at the ZMOPEN file
;; which I suspect contains the brand S code for this.

(DEFUN OPEN-EDITOR-STREAM (&KEY &OPTIONAL INTERVAL BUFFER-NAME PATHNAME WINDOW START END
			   (CREATE-P ':WARN) DEFAULTS HACK-FONTS KILL LOAD-P ORDERED-P
			   UNDO-SAVING)
  "Open and return a stream to read or write an editor interval.
The stream is bidirectional, with a single pointer into
 its interval, used for both reading and writing.
Specify the interval using :INTERVAL, :BUFFER-NAME, :PATHNAME, or :WINDOW.
:INTERVAL specifies the interval directly.
:BUFFER-NAME specifies a ZMACS buffer name.
:PATHNAME specifies a file.  The ZMACS buffer visiting it is used.
:WINDOW specifies a window.  The interval it is displaying is used.
If none of those is specified, :START must be a BP.
 The interval it points into us used.

These keywords modify the specification of the interval:
:CREATE-P says what to do if there is no ZMACS buffer
 for the specified :BUFFER-NAME or :PATHNAME.
 Possibilities are :ASK (query user), T (just create one),
 :WARN (print message on ERROR-OUTPUT and create one),
 or :ERROR (get an error).  The default is :WARN.
:LOAD-P says what to do if creating a buffer for a pathname.
 T means read in the file if it exists.  NIL means create it empty.
:DEFAULTS specifies a defaults-list for use in defaulting :PATHNAME.

These keywords specify precisely the portion of the interval
 to read or write:
:START specifies where to start.
 It can be :BEGINNING (beginning of interval), :END (end of interval),
 :POINT (the POINT of the specified :WINDOW),
 :MARK (the MARK of the specified :WINDOW),
 :REGION (same as using :POINT for :START and :MARK for :END),
 or a BP.
 :START initializes the stream's pointer, which is advanced
 over all texta read or written.
:END specifies where to stop reading (get eof).
 It can be :END, :POINT, :MARK or a BP.
:ORDERED-P non-NIL says assume that :END follows :START.
 If this is NIL, then the two are compared and whichever
 comes earlier in the interval is actually used as the start,
 the other becoming the end.  If :START is :BEGINNING or
 if :END is :END, the two are automatically known to be ordered.

:KILL if non-NIL says to delete the text of the interval
 or the portion of it between :START and :END.
:UNDO-SAVING if non-NIL says that insertions done by stream output
 should be recorded for the Undo command.
:HACK-FONTS can be NIL meaning discard font information
 of text in the buffer, or T meaning convert to epsilons
 (that is, the text you read will contain epsilons, and
 if you write epsilons they will convert into font changes)."
  (COND (INTERVAL)
	(BUFFER-NAME
	 (OR (SETQ INTERVAL (FIND-BUFFER-NAMED BUFFER-NAME))
	     (IF (SELECTQ CREATE-P
		   ((T) T)
		   (:ASK (FQUERY NIL "Create ZMACS buffer ~A? " BUFFER-NAME))
		   (:WARN
		    (FORMAT ERROR-OUTPUT "~&[Creating ZMACS buffer ~A]" BUFFER-NAME)
		    T))
		 (SETQ INTERVAL (FIND-BUFFER-NAMED BUFFER-NAME T)))
	     (LET ((TEM (CERROR '(:NO-ACTION :NEW-VALUE) NIL NIL
				"Buffer ~A does not exist." BUFFER-NAME)))
	       (IF TEM (SETQ BUFFER-NAME TEM))
	       (SETQ INTERVAL (FIND-BUFFER-NAMED BUFFER-NAME T)))))
	(PATHNAME
	 (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME DEFAULTS))
	 (OR (SETQ INTERVAL (FIND-FILE-BUFFER PATHNAME))
	     (IF (SELECTQ CREATE-P
		   ((T) T)
		   (:ASK (FQUERY NIL "Create ZMACS buffer for ~A? " PATHNAME))
		   (:WARN
		    (FORMAT ERROR-OUTPUT "~&[Creating ZMACS buffer for ~A]" PATHNAME)
		    T))
		 (SETQ INTERVAL (FIND-FILE PATHNAME NIL NIL LOAD-P)))
	     (LET ((TEM (CERROR '(:NO-ACTION :NEW-VALUE) NIL NIL
				"There is no ZMACS buffer for ~A." PATHNAME)))
	       (IF TEM (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TEM DEFAULTS)))
	       (SETQ INTERVAL
		     (OR (FIND-FILE-BUFFER PATHNAME)
			 (FIND-FILE PATHNAME NIL NIL LOAD-P))))))
	(WINDOW
	 (SETQ INTERVAL (WINDOW-INTERVAL WINDOW)))
	(START
	 (SETQ INTERVAL (BP-TOP-LEVEL-NODE START)))
	(T
	 (FERROR NIL "No interval specified.")))
  (SELECTQ START
    ((:END :APPEND NIL)
     (SETQ START (INTERVAL-LAST-BP INTERVAL)))
    (:BEGINNING
     (SETQ ORDERED-P T)
     (SETQ START (INTERVAL-FIRST-BP INTERVAL)))
    (:POINT
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR NIL "No WINDOW specified with START = :POINT."))
     (SETQ START (WINDOW-POINT WINDOW)))
    (:MARK
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR NIL "No WINDOW specified with START = :MARK."))
     (SETQ START (WINDOW-MARK WINDOW)))
    (:REGION
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR NIL "No WINDOW specified with START = :REGION."))
     (SETQ START (WINDOW-POINT WINDOW))
     (SETQ END (WINDOW-MARK WINDOW)))
    (T (UNLESS (LISTP START)
	 (FERROR NIL "START is ~S, which is not valid." START))))
  (SELECTQ END
    ((:END NIL)
     (SETQ ORDERED-P T)
     (SETQ END (INTERVAL-LAST-BP INTERVAL)))
    (:POINT
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR NIL "No WINDOW specified with END = :POINT."))
     (SETQ END (WINDOW-POINT WINDOW)))
    (:MARK
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR NIL "No WINDOW specified with END = :MARK."))
     (SETQ END (WINDOW-MARK WINDOW)))
    (T (UNLESS (LISTP END)
	 (FERROR NIL "END is not a BP or :END, :POINT or :MARK."))))
  (WHEN KILL
    (LET ((*BATCH-UNDO-SAVE* T))
      (DELETE-INTERVAL START END)))
  (INTERVAL-STREAM START END ORDERED-P HACK-FONTS (NOT UNDO-SAVING)))

))

; From file HOST.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HOST  "


(defmethod (ED-PATHNAME :REALLY-OPEN) (pathname characters error new-file)
  pathname
  "Use completing reader to find buffer name.  Never make a new file."
  (and new-file
       (neq new-file ':DEFAULT)
       (open-error "can't handle :NEW-FILE keyword, use ED-BUFFER: instead"))
  (multiple-value-bind (ignore alist) (complete-string (send self ':name)
						       *ZMACS-BUFFER-NAME-ALIST*
						       '(#\SP #/- #/. #/\ #// #/#))
    (if alist
	(if (= (length alist) 1)
	    (interval-stream (cdar alist) nil nil (if characters nil ':TYO) t)
	    (open-error "ambiguous name"))
        (open-error "not found"))))

(defmethod (ED-FILE-PATHNAME :REALLY-OPEN) (pathname characters error new-file)
  pathname error new-file
  (let ((name (fs:parse-pathname (send self ':name)))
	(*interval* nil))
    (interval-stream
      (or (find-buffer-named pathname)		; don't bash existing buffer!
	  (find-file name nil t))
      nil
      nil
      (if characters nil ':TYO)
      t)))

(defmethod (ED-BUFFER-PATHNAME :REALLY-OPEN) (pathname characters error new-file)
  pathname
  (let ((buffer (find-buffer-named (send self ':name) (not (null new-file)))))
    (if buffer
	(interval-stream buffer nil nil (if characters nil ':TYO) t)
      (open-error "ED-BUFFER: not found"))))

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-QUERY-REPLACE "Replace string, asking about each occurrence.
Prompts for each string.  If you first give it FOO, then BAR, it
finds the first FOO, displays, and
reads a character.  Space => replace it with BAR and show next FOO.
Rubout => don't replace, but show next FOO.
Comma => replace this FOO and show result, waiting for a
space, R or Altmode.
Period => replace this FOO and exit.  Altmode => just exit.
^ => return to site of previous FOO (actually, pop the point pdl).
W => kill this FOO and enter recursive edit.
R => enter editing mode recursively.  L => redisplay screen.
Exclamation mark => replace all remaining FOOs without asking.
Any other character exits and (except altmode) is read again.
If *CASE-REPLACE-P* is nonnull, BAR's initial will be capitalized
if FOO's initial had been.
If you give a numeric argument, it will not consider FOOs that are not
bounded on both sides by delimiter characters." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
      (QUERY-REPLACE (POINT) FROM TO *NUMERIC-ARG-P*)))
  DIS-TEXT)

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-MULTIPLE-QUERY-REPLACE "Query replace two sets of strings at the same time.
Strings are read in alternate mini-buffers, ended by a null string.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (MULTIPLE-QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
      (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
					  *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
	(QUERY-REPLACE-LIST (POINT) FROM-LIST TO-LIST *NUMERIC-ARG-P*))))
  DIS-TEXT)

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-QUERY-EXCHANGE "Query replace two strings with one another at the same time.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*) "exchange")
    (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
      (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
					  *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
	(QUERY-REPLACE-LIST (POINT) (LIST FROM TO) (LIST TO FROM)
			    *NUMERIC-ARG-P*))))
  DIS-TEXT)

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-REPLACE-STRING "Replace all occurrences of a given string with another.
Prompts for two string: to replace all FOO's with BAR's, type FOO and BAR.
With no numeric arg, all occurrences after point are replaced.
With numeric arg, that many occurrences are replaced.
If *CASE-REPLACE-P* is nonnull, BAR's initial will be capitalized
if FOO's initial had been (supply it in lower case)." ()
  (LET ((FROM (TYPEIN-LINE-READLINE
		"Replace ~:[all~*~;next ~D~] occurrences ~:[in the region ~]of:"
		*NUMERIC-ARG-P* *NUMERIC-ARG* (NOT (WINDOW-MARK-P *WINDOW*)))))
    (AND (ZEROP (STRING-LENGTH FROM))
	 (BARF "The string may not be null."))
    (LET ((TO (LET ((*MINI-BUFFER-DEFAULT-STRING* FROM))
		(TYPEIN-LINE-READLINE
		  "Replace ~:[all~*~;next ~D~] occurrences ~:[in the region ~]of /"~A/" with:"
		  *NUMERIC-ARG-P* *NUMERIC-ARG* (NOT (WINDOW-MARK-P *WINDOW*)) FROM))))
      (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
	(FORMAT QUERY-IO "~&~D. replacement~:P."
		(REPLACE-STRING (POINT) FROM TO (AND *NUMERIC-ARG-P*
						     *NUMERIC-ARG*))))))
  DIS-TEXT)

))

; From file METH.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "


(DEFMETHOD (INTERVAL-STREAM :TYO) (CH)
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) CH)))
      (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP)))))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :TYO) (CH)
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (COND ((EQ *FONT-FLAG* T)
	   ;; Character after a ^F.
	   (SETQ *FONT-FLAG* NIL)
	   (COND ((= CH #/)
		  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*)
				    (IN-CURRENT-FONT CH **FONT**))))
		    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
		 ((= CH #/#)
		  (SETQ *FONT-FLAG* 'DIAG-1))
		 ((= CH #/*)
		  (OR (ZEROP (ARRAY-LEADER *FONT-STACK* 0))
		      (SETQ **FONT** (ARRAY-POP *FONT-STACK*))))
		 (T 
		  (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
		  (SETQ **FONT** (- CH #/0)))))
	  ((NULL *FONT-FLAG*)
	   ;; Character in normal text state.
	   (COND ((= CH #/)
		  (SETQ *FONT-FLAG* T))
		 (T
		  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*)
				    (IN-CURRENT-FONT CH **FONT**))))
		    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))))
	  ((EQ *FONT-FLAG* 'DIAG-1)
	   ;; Character after a ^F#
	   (SETQ *FONT-FLAG* 'DIAG-2 *STOP-INDEX* 0))
	  ((EQ *FONT-FLAG* 'DIAG-2)
	   (IF (= CH #\SP)
	       (SETQ *FONT-FLAG* (MAKE-ARRAY 10.
					     ':TYPE 'ART-STRING
					     ':LEADER-LIST '(0)))
	     (SETQ *STOP-INDEX* (+ (* *STOP-INDEX* 10.) (- CH #/0)))))
	  ((STRINGP *FONT-FLAG*)
	   (IF (= CH #\CR)
	       (SETQ *INDEX* NIL
		     *FONT-FLAG* (MAKE-INSTANCE (READ-FROM-STRING *FONT-FLAG*)
						':NUMBER-OF-LINES *STOP-INDEX*))
	     (ARRAY-PUSH-EXTEND *FONT-FLAG* CH)))
	  ((TYPEP *FONT-FLAG* 'RESTORABLE-LINE-DIAGRAM-MIXIN)
	   (PROG ()
		 (OR *INDEX*
		     (COND ((< (SETQ *STOP-INDEX* (1- *STOP-INDEX*)) 0)
			    (SETQ *INDEX* 0 *FONT-FLAG* (EQ CH #/))
			    (RETURN))
			   (T
			    (SETQ *INDEX* (CREATE-LINE ART-STRING 0 NIL))
			    (INSERT-LINE-WITH-LEADER *INDEX* *LINE*))))
		 (COND ((= CH #\CR)
			(PUTPROP (LOCF (LINE-PLIST *INDEX*)) *FONT-FLAG* ':DIAGRAM)
			(FUNCALL *FONT-FLAG* ':ADD-LINE *INDEX* *INDEX*)
			(SETF (LINE-LENGTH *INDEX*) 0)
			(SETQ *INDEX* NIL))
		       (T
			(ARRAY-PUSH-EXTEND *INDEX* CH)))))
	  (T (FERROR NIL "*FONT-FLAG* has a value not understood here")))))

(DEFMETHOD (INTERVAL-STREAM :LINE-OUT) (LINE)	;Bleagh, really should take two optional args
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
   (COND ((AND (ZEROP *INDEX*) (EQ (ARRAY-LEADER-LENGTH LINE) LINE-LEADER-SIZE))
	  (INSERT-LINE-WITH-LEADER LINE *LINE*))	;Optimize case for file readin
	 ((ZEROP *INDEX*)			;Optimize case where it's not already a line
	  (LET ((NEW-LINE (CREATE-LINE 'ART-STRING (ARRAY-ACTIVE-LENGTH LINE) **INTERVAL**)))
	    (COPY-ARRAY-CONTENTS LINE NEW-LINE)
	    (INSERT-LINE-WITH-LEADER NEW-LINE *LINE*)))
	 (T
	  (LET ((BP (INSERT
		     (INSERT (CREATE-BP *LINE* *INDEX*) LINE)
		     #\CR)))
	    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))))
   LINE)

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :LINE-OUT) (LINE)	;Bleagh, really should take
						                ;two optional args
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
   (COND ((ZEROP *INDEX*)
	  ;; At start of line in buffer, we can just make a line and insert it here.
	  (IF (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
	      ;; If it's already a fat string, just make sure its leader is the right size.
	      (OR (EQ (ARRAY-LEADER-LENGTH LINE) LINE-LEADER-SIZE)
		  (LET ((NEW-LINE (CREATE-LINE 'ART-STRING
					       (ARRAY-ACTIVE-LENGTH LINE) **INTERVAL**)))
		    (COPY-ARRAY-CONTENTS LINE NEW-LINE)
		    (SETQ LINE NEW-LINE)))
	    ;; Otherwise do font processing to make a new string.
	    (SETQ LINE (MAKE-MULTI-FONT-LINE SELF LINE **INTERVAL**)))))
   ;; Now test for MAKE-MULTI-FONT-LINE returning NIL, or inserting some stuff
   ;; so that we are no longer at the start of a line in the buffer.
   (AND LINE
	(IF (ZEROP *INDEX*)
	    (INSERT-LINE-WITH-LEADER LINE *LINE*)
	  (FUNCALL-SELF ':STRING-OUT LINE)
	  (FUNCALL-SELF ':TYO #\CR)))
   (LINE-PREVIOUS *LINE*)))

(DEFMETHOD (INTERVAL-STREAM :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
   (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) STRING START END)))
     (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP)))))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :STRING-OUT)
	   (STRING &OPTIONAL (START 0) END (ORIGINAL-STRING STRING))
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
   (OR (EQ (ARRAY-TYPE STRING) 'ART-FAT-STRING)
       (SETQ STRING (MAKE-MULTI-FONT-LINE SELF STRING NIL START END) START 0 END NIL))
   (AND STRING
	(LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) STRING START END)))
	  (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
   ORIGINAL-STRING))

))

; From file HOST.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HOST  "

; This is extremely picky about keyword values, maybe it need not check so thoroughly?

(defun ED-PATHNAME-OPEN (ignored pathname
			 &key &optional (characters t)
			 (direction ':input)
			 (error t)
			 (if-exists ':append)
			 (if-does-not-exist
			   (if (eq direction ':output) ':create
			     ':error))
			 &allow-other-keys)
  "parse OPEN keywords and then call the :REALLY-OPEN method"
  (let ((stream
	  (funcall-self ':REALLY-OPEN pathname characters error
			(eq if-does-not-exist ':create))))
    (if (eq if-exists ':APPEND)
	(send stream ':READ-UNTIL-EOF))
    stream))

))

; From file RH.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "


(DEFINE-RH-COMMAND RH-COM-REST-OF-INPUT-HISTORY (#\META-STATUS) (N)
  (RH-DISPLAY-INFO
    (ZWEI:LIST-HISTORY-CONTENTS (RH-INPUT-RING) SELF (IF (= N 1) ZWEI:*HISTORY-MENU-LENGTH* N)
				(DONT-OPTIMIZE (ZWEI:HISTORY-LENGTH (RH-INPUT-RING))))))

(DEFINE-RH-COMMAND RH-COM-REST-OF-KILL-HISTORY (#\CONTROL-META-STATUS) (N)
  (RH-DISPLAY-INFO
    (ZWEI:LIST-HISTORY-CONTENTS ZWEI:*KILL-HISTORY* SELF
				(IF (= N 1) ZWEI:*HISTORY-MENU-LENGTH* N)
				(DONT-OPTIMIZE (ZWEI:HISTORY-LENGTH ZWEI:*KILL-HISTORY*)))))

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "


(DEFUN PRINT-ARRAY (EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
  (DOLIST (ELT (PTTBL-ARRAY READTABLE))
    (COND ((STRINGP ELT) (SEND STREAM ':STRING-OUT ELT))
	  ((EQ ELT ':RANK)
	   (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
	     (PRINT-FIXNUM (ARRAY-RANK EXP) STREAM)))
	  ((EQ ELT ':SEQUENCES)
	   (PRINT-ARRAY-CONTENTS EXP 0 0 I-PRINDEPTH STREAM WHICH-OPERATIONS)))))

))

; From file GRIND.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; GRIND  "


(DEFUN GRIND-ARRAY (EXP LOC)
  (IF (= (ARRAY-RANK EXP) 1)
      (IF (EQ (ARRAY-TYPE EXP) 'ART-1B)
	  (GRIND-ATOM EXP GRIND-IO LOC)
	(GRIND-AS-BLOCK (LISTARRAY EXP) NIL
			(PTTBL-OPEN-VECTOR READTABLE)
			(PTTBL-CLOSE-VECTOR READTABLE)))
    (DOLIST (ELT (PTTBL-ARRAY READTABLE))
      (COND ((STRINGP ELT)
	     (GSTRING ELT))
	    ((EQ ELT ':RANK)
	     (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
	       (GRIND-ATOM (ARRAY-RANK EXP) GRIND-IO NIL)))
	    ((EQ ELT ':SEQUENCES)
	     (OR (GRIND-TRY #'GRIND-ARRAY-CONTENTS
			    EXP 0 0 T)
		 (GRIND-ARRAY-CONTENTS EXP 0 0)))))))

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-ARRAY-CONTENTS (ARRAY DIMENSION INDEX-SO-FAR I-PRINDEPTH STREAM W-O)
  (IF (AND *PRINT-LEVEL* (>= I-PRINDEPTH *PRINT-LEVEL*))
      (SEND STREAM ':STRING-OUT (PTTBL-PRINLEVEL READTABLE))
    (IF (ZEROP (ARRAY-RANK ARRAY))
	(PROGN (SEND STREAM ':TYO (PTTBL-SPACE READTABLE))
	       (PRINT-OBJECT (AREF ARRAY) I-PRINDEPTH STREAM W-O))
      (SEND STREAM ':TYO (PTTBL-OPEN-PAREN READTABLE))
      (LET ((INDEX (* INDEX-SO-FAR (ARRAY-DIMENSION ARRAY DIMENSION))))
	(DOTIMES (I (ARRAY-DIMENSION ARRAY DIMENSION))
	  (UNLESS (ZEROP I)
	    (SEND STREAM ':TYO (PTTBL-SPACE READTABLE)))
	  (COND ((AND *PRINT-LENGTH* (= I *PRINT-LENGTH*))
		 (SEND STREAM ':STRING-OUT (PTTBL-PRINLENGTH READTABLE))
		 (RETURN))
		((= (1+ DIMENSION) (ARRAY-RANK ARRAY))
		 (PRINT-OBJECT (AR-1-FORCE ARRAY (+ INDEX I))
			       (1+ I-PRINDEPTH) STREAM W-O))
		((AND *PRINT-LEVEL*
		      (>= (1+ I-PRINDEPTH) *PRINT-LEVEL*))
		 (SEND STREAM ':STRING-OUT (PTTBL-PRINLEVEL READTABLE)))
		(T
		 (PRINT-ARRAY-CONTENTS ARRAY (1+ DIMENSION)
				       (+ INDEX I) (1+ I-PRINDEPTH)
				       STREAM W-O)))))
      (SEND STREAM ':TYO (PTTBL-CLOSE-PAREN READTABLE)))))

))

; From file GRIND.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; GRIND  "

(DEFUN GRIND-ARRAY-CONTENTS (ARRAY DIMENSION INDEX-SO-FAR &OPTIONAL LINEAR)
  (IF (AND *PRINT-LEVEL* (>= GRIND-DEPTH *PRINT-LEVEL*))
      (GRIND-ATOM (PTTBL-PRINLEVEL READTABLE) GRIND-IO NIL)
    (IF (ZEROP (ARRAY-RANK ARRAY))
	(LET ((ELT (AREF ARRAY))
	      (ELTLOC (ALOC ARRAY)))
	  (COND (LINEAR
		 (GTYO-SPACE)
		 (GRIND-LINEAR-FORM ELT ELTLOC))
		(T				;Won't fit, start another line
		 (GRIND-STANDARD-FORM ELT ELTLOC))))
      (GTYO-OPEN T)
      (GIND
	(LET ((INDEX (* INDEX-SO-FAR (ARRAY-DIMENSION ARRAY DIMENSION)))
	      (FRESHLINEP T)
	      VP)
	  (DOTIMES (I (ARRAY-DIMENSION ARRAY DIMENSION))
	    (UNLESS (ZEROP I)
	      (COND ((AND (= VP GRIND-VPOS)	;if still on same line, need a space
			  (< GRIND-HPOS GRIND-WIDTH))	;unless at end of line
		     (GTYO-SPACE)
		     (SETQ FRESHLINEP NIL))
		    (T				;If this was moby, don't put
		     (GRIND-TERPRI)		; anything else on the same line
		     (SETQ FRESHLINEP T))))
	    (SETQ VP GRIND-VPOS)
	    (COND ((AND *PRINT-LENGTH* (= I *PRINT-LENGTH*))
		   (GRIND-ATOM (PTTBL-PRINLENGTH READTABLE) GRIND-IO NIL)
		   (RETURN))
		  ((= (1+ DIMENSION) (ARRAY-RANK ARRAY))
		   (LET ((ELT (AR-1-FORCE ARRAY (+ INDEX I)))
			 (ELTLOC (AR-1-FORCE ARRAY (+ INDEX I))))
		     (COND (LINEAR (GRIND-LINEAR-FORM ELT ELTLOC))
			   ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC))
			   ((AND FRESHLINEP
				 (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM) ELT ELTLOC)))
			   (T			;Won't fit, start another line
			    (OR FRESHLINEP (GRIND-TERPRI))
			    (SETQ VP GRIND-VPOS)
			    (OR (GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC)
				(GRIND-STANDARD-FORM ELT ELTLOC))))))
		  ((AND *PRINT-LEVEL*
			(>= GRIND-DEPTH *PRINT-LEVEL*))
		   (GRIND-ATOM (PTTBL-PRINLEVEL READTABLE) GRIND-IO NIL))
		  (LINEAR
		   (GRIND-ARRAY-CONTENTS ARRAY (1+ DIMENSION) (+ INDEX I)) T)
		  (T
		   ;; Always start each row on a new line.
		   (UNLESS (OR FRESHLINEP LINEAR (ZEROP I))
		     (GRIND-TERPRI))
		   (OR (GRIND-TRY #'GRIND-ARRAY-CONTENTS
				  ARRAY (1+ DIMENSION) (+ INDEX I) T)
		       (AND (NOT FRESHLINEP)
			    (PROGN (GRIND-TERPRI)
				   (GRIND-TRY #'GRIND-ARRAY-CONTENTS
					      ARRAY (1+ DIMENSION) (+ INDEX I))))
		       (GRIND-ARRAY-CONTENTS ARRAY (1+ DIMENSION) (+ INDEX I))))))))
      (GTYO-CLOSE T))))

))

; From file GENRIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(DEFUN FILL-ARRAY-FROM-SEQUENCES (ARRAY SEQUENCE DIMENSION ARRAY-INDEX)
  (IF (= 0 (ARRAY-RANK ARRAY))
      (SETF (AREF ARRAY) SEQUENCE)
    (DO ((INDEX (SEQ-START SEQUENCE))
	 (I 0 (1+ I))
	 (LAST-DIM-FLAG (= (1+ DIMENSION) (ARRAY-RANK ARRAY)))
	 (STOP-I (ARRAY-DIMENSION ARRAY DIMENSION)))
	((= I STOP-I))
      (IF LAST-DIM-FLAG
	  ;; Cut off one level of recursion - eliminates most of the function calls.
	  (SETF (AR-1-FORCE ARRAY (+ (* ARRAY-INDEX STOP-I) I))
		(SEQ-FETCH-INC SEQUENCE INDEX))
	(FILL-ARRAY-FROM-SEQUENCES ARRAY (SEQ-FETCH-INC SEQUENCE INDEX) (1+ DIMENSION)
				   (+ (* ARRAY-INDEX STOP-I) I))))))

))

; From file RH.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFUN-RH ALTERNATE-RUBOUT-HANDLER ()
  (LET ((CH) (CH-CHAR) (CH-CONTROL-META) (COMMAND)
	(FILL-POINTER (RH-FILL-POINTER))
	(TYPEIN-POINTER (RH-TYPEIN-POINTER))
	(INITIAL-ENTRY (RHB-INITIAL-ENTRY))
	(RUBBED-OUT-SOME? NIL)
	(PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS)))
	(INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS)))
	(INITIAL-INPUT-POINTER
	  (CADR (ASSQ ':INITIAL-INPUT-POINTER-INDEX RUBOUT-HANDLER-OPTIONS)))
	(NUMERIC-ARG NIL)
	(NUMERIC-ARG-NEGATIVE NIL))

    (SETF (RHB-INITIAL-ENTRY) NIL)

    ;; Kludge #1.  If this is the first time this rubout handler has been invoked
    ;; in this stream, then we must create the input history.
    (OR (RH-INPUT-RING)
	(WHEN (FBOUNDP 'ZWEI:MAKE-HISTORY)
	  (SETF (RH-INPUT-RING) (RH-MAKE-INPUT-RING))))

    (WHEN INITIAL-ENTRY
      ;; save the previous input on the input history,
      ;; unless the previous read said not to save it.
      (COND ((AND (NOT (RH-DONT-SAVE-FLAG)) (RH-INPUT-RING)
		  TYPEIN-POINTER)
	     ;; only add the contents if it is different than the last entry, and
	     ;; the entry is at least 2 characters long.
	     (SETF (FILL-POINTER RUBOUT-HANDLER-BUFFER) TYPEIN-POINTER)
	     (WHEN (AND (> TYPEIN-POINTER 1)
			(MISMATCH RUBOUT-HANDLER-BUFFER
				  (ZWEI:HISTORY-LATEST-ELEMENT (RH-INPUT-RING))))
	       (ZWEI:PUSH-ON-HISTORY (SUBSEQ RUBOUT-HANDLER-BUFFER 0 TYPEIN-POINTER)
				     (RH-INPUT-RING)))
	     (SETF (FILL-POINTER RUBOUT-HANDLER-BUFFER) FILL-POINTER)))

      ;; Then initialize the typein pointer.
      (SETF (RH-TYPEIN-POINTER) FILL-POINTER)
      (SETQ TYPEIN-POINTER FILL-POINTER)

      ;; Gobble the initial input if any.
      (WHEN INITIAL-INPUT
	(RH-INSERT-STRING INITIAL-INPUT 0 NIL NIL NIL)
	(SETQ RUBBED-OUT-SOME? T)
	(RH-SET-POSITION (OR INITIAL-INPUT-POINTER (RH-TYPEIN-POINTER))))
    
      ;; Record whether this unit of input should be saved on the history.
      (SETF (RH-DONT-SAVE-FLAG)
	    (OR (CADR (ASSQ ':DONT-SAVE RUBOUT-HANDLER-OPTIONS))
		(CADR (ASSQ ':NO-INPUT-HISTORY RUBOUT-HANDLER-OPTIONS))))
      )

    ;; Kludge #5.  We can't echo or rub out a bucky char,
    ;; so if the last char inserted was a bucky char and it did not terminate the
    ;; input, flush its bucky bits.
    (AND (NOT (ZEROP TYPEIN-POINTER))
	 (NOT (ZEROP (LDB %%KBD-CONTROL-META
			  (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER)))))
	 (SETF (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER))
	       (LDB %%KBD-CHAR
		    (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER)))))

    ;; Kludge #4.  After resuming a Break, the stream's cursorpos is wrong.
    ;; In fact, the cursor is at the end of the string in that case.
    ;; So, if it is supposed to be elsewhere, move it.
    ;; This condition also avoids wasting time when we are reading typein
    ;; at the end of the string.
    (OR (= FILL-POINTER TYPEIN-POINTER)
	(RH-CURSOR-MOTION TYPEIN-POINTER))

    ;; Read characters.  If an ordinary character typed and nothing rubbed out,
    ;; return immediately.  Otherwise, let all editing operations complete
    ;; before returning. 
    (*CATCH 'RETURN-CHARACTER
      (DO (*LAST-COMMAND-TYPE*
	   *CURRENT-COMMAND-TYPE*
	   *RUBOUT-HANDLER-MARK*)
	  (NIL)
	;; Read a character from the stream after bypassing ourself.
	(SETQ CH (LET ((RUBOUT-HANDLER NIL)) (FUNCALL-SELF ':ANY-TYI)))
	(IF (LISTP CH)
	    (COND ((EQ (CAR CH) 'REDISPLAY-RUBOUT-HANDLER)
		   (SEND SELF ':SET-CURSORPOS
			 PROMPT-STARTING-X PROMPT-STARTING-Y)
		   (SEND SELF ':CLEAR-EOL)
		   (RH-REPRINT-INPUT NIL T))
		  ((AND (EQ (CAR CH) ':MOUSE-BUTTON)
			(EQ (CADR CH) #\MOUSE-3-1))
		   (MOUSE-CALL-SYSTEM-MENU)))
	  (SETQ CH-CHAR (LDB %%KBD-CHAR CH))
	  (SETQ CH-CONTROL-META (LDB %%KBD-CONTROL-META CH))
	  (SETQ COMMAND (ASSQ CH RH-COMMAND-ALIST))
	  (COND
	    
	    ;; Don't touch this character.  Treat it as self-inserting.
	    ((OR (MEMQ CH PASS-THROUGH)
		 (SI:ASSQ-CAREFUL CH PASS-THROUGH))
	     (RH-SET-POSITION (RH-FILL-POINTER))
	     (RH-INSERT-CHAR CH 1 RUBBED-OUT-SOME?)
	     (SETQ RUBBED-OUT-SOME? T))
	    
	    ;; An editing command of some sort.  The RUBBED-OUT-SOME bit can only be
	    ;; cleared by entering this function again.  The function is passed the
	    ;; numeric argument, and returns T if we are going to need to throw out (like
	    ;; DIS-ALL in the editor).
	    (COMMAND
	     (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
		   *CURRENT-COMMAND-TYPE* NIL)
	     (SETQ RUBBED-OUT-SOME?
		   (OR (FUNCALL (CDR COMMAND) (* (OR NUMERIC-ARG 1)
						 (IF NUMERIC-ARG-NEGATIVE -1 1)))
		       RUBBED-OUT-SOME?))
	     (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL)
	     ;; If the buffer is empty and the :FULL-RUBOUT option is active, then throw now.
	     ;; This will throw if the user types Rubout or ClearScreen immediately after
	     ;; entering the read function.  It is important that we check for this here
	     ;; and not in RH-DELETE-STRING since some commands, such as Yank-Pop, may
	     ;; temporarily empty the buffer.  It wouldn't be the right thing to throw
	     ;; if the buffer only contained whitespace since it is the responsibility
	     ;; of the caller to discard whitespace when looking for special characters.
	     (COND ((AND (ZEROP (RH-FILL-POINTER))
			 (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
		    ;; This SETF should be done in the :RUBOUT-HANDLER method loop.
		    (SETF (RH-SCAN-POINTER) 0)
		    (*THROW 'RUBOUT-HANDLER T))))
	    
	    ;;Handle Control-number and Control-U specially.
	    ((AND (NOT (ZEROP CH-CONTROL-META))
		  ( #/0 CH-CHAR #/9))
	     (SETQ NUMERIC-ARG (+ (* (OR NUMERIC-ARG 0) 10.) (- CH-CHAR #/0))))
	    ((= CH #\CONTROL-U)
	     (SETQ NUMERIC-ARG (* (OR NUMERIC-ARG 1) 4)))
	    ((AND (NOT (ZEROP CH-CONTROL-META)) (= CH-CHAR #/-))
	     (IF NUMERIC-ARG
		 (FUNCALL-SELF ':BEEP)
	       (SETQ NUMERIC-ARG-NEGATIVE (NOT NUMERIC-ARG-NEGATIVE))))
	    
	    ;; Some other random control character -- beep and ignore
	    ((NOT (ZEROP CH-CONTROL-META))
	     (FUNCALL-SELF ':BEEP)
	     (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))
	    
	    ;; Self-inserting character.  Set RUBBED-OUT-SOME since if we return,
	    ;; we were typing in the middle of the line.  Typing at the end of the
	    ;; line throws to RETURN-CHARACTER.
	    (T (UNLESS NUMERIC-ARG-NEGATIVE
		 (RH-INSERT-CHAR CH (OR NUMERIC-ARG 1) RUBBED-OUT-SOME?)
		 (SETQ RUBBED-OUT-SOME? T))
	       (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
		     *CURRENT-COMMAND-TYPE* NIL
		     *RUBOUT-HANDLER-MARK* NIL)
	       (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))))))))

))

; From file RH.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFUN-RH RH-REPRINT-INPUT (&OPTIONAL CHAR DONT-SET-PROMPT-CURSORPOS)
  "Reprint the contents of the rubout handler buffer at the current cursor position."
  (UNLESS DONT-SET-PROMPT-CURSORPOS
    (MULTIPLE-VALUE (PROMPT-STARTING-X PROMPT-STARTING-Y)
      (FUNCALL-SELF ':READ-CURSORPOS)))
  (LET ((PROMPT (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
		    (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS))))
    (IF PROMPT (RUBOUT-HANDLER-PROMPT (CADR PROMPT) SELF CHAR)))
  (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
    (FUNCALL-SELF ':READ-CURSORPOS))
  (LET ((MORE-PROCESSING-GLOBAL-ENABLE NIL))
    (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
  (RH-CURSOR-MOTION (RH-TYPEIN-POINTER))
  NIL)

))

; From file BASWIN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "


;Execute the body and then redisplay any rubout handler input that is on the window
;below what was printed by the body.
(DEFMACRO OUTPUT-BEFORE-RUBOUT-HANDLER ((WINDOW) &BODY BODY)
  `(LET (PROCESS SG RUBOUT-X RUBOUT-Y RUBOUT-X-LOC RUBOUT-Y-LOC)
      (WITHOUT-INTERRUPTS
	(AND (SETQ PROCESS (FUNCALL ,WINDOW ':PROCESS))
	     (SETQ SG (FUNCALL PROCESS ':STACK-GROUP))
	     (SYMEVAL-IN-STACK-GROUP 'RUBOUT-HANDLER-INSIDE SG)
	     (SETF (VALUES RUBOUT-X RUBOUT-X-LOC)
		   (SYMEVAL-IN-STACK-GROUP 'PROMPT-STARTING-X SG)
		   (VALUES RUBOUT-Y RUBOUT-Y-LOC)
		   (SYMEVAL-IN-STACK-GROUP 'PROMPT-STARTING-Y SG))))
      ;; If the process is in the rubout-handler, back up over the echoed input and erase it.
      (COND (RUBOUT-X (FUNCALL ,WINDOW ':SET-CURSORPOS RUBOUT-X RUBOUT-Y)
		      (FUNCALL ,WINDOW ':CLEAR-EOL)))
      (UNWIND-PROTECT
	(PROGN . ,BODY)
	;; Reprint rubout-handler buffer if necessary, and change the rubout-handler's
	;; starting cursorpos
	(COND (RUBOUT-X
	       (MULTIPLE-VALUE-BIND (NEW-X NEW-Y)
		   (SEND ,WINDOW ':READ-CURSORPOS)
		 (SETF (CONTENTS RUBOUT-X-LOC) NEW-X
		       (CONTENTS RUBOUT-Y-LOC) NEW-Y)
		 (IO-BUFFER-PUSH (SEND ,WINDOW ':IO-BUFFER)
				 `(REDISPLAY-RUBOUT-HANDLER))))))))

;Some windows that use the usual test to see whether they are able to print
;put demons on this to do hairy things when they do print.
(DEFMETHOD (NOTIFICATION-MIXIN :PRINT-NOTIFICATION-ON-SELF) (TIME STRING WINDOW-OF-INTEREST)
  WINDOW-OF-INTEREST
  (LOCK-SHEET (SELF)
    (OUTPUT-BEFORE-RUBOUT-HANDLER (SELF)
      (FUNCALL-SELF ':FRESH-LINE)
      (FUNCALL-SELF ':BEEP)
      (FUNCALL-SELF ':TYO #/[)
      (TIME:PRINT-BRIEF-UNIVERSAL-TIME TIME SELF)
      (FUNCALL-SELF ':TYO #\SP)
      (LET ((END (STRING-LENGTH STRING)))
	(OR (ZEROP END)
	    (FUNCALL-SELF ':STRING-OUT STRING 0
			  (IF (= (AREF STRING (1- END)) #\RETURN) (1- END) END))))
      (FUNCALL-SELF ':TYO #/])
      (FUNCALL-SELF ':TYO #\CR)
)))

))

; From file SUPDUP.LISP SRC:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

;This is used as the i/o buffer output function while reading a hostname.
;It intercepts Help and Network, as well as the usual things.
(DEFUN SUPDUP-IO-BUFFER-OUTPUT-FUNCTION (IGNORE CHAR)
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (COND ((NOT (NUMBERP CHAR)) CHAR)		;Blips shouldn't get here, but don't die
	((AND (EQ CHAR #\HELP)
	      (NOT INHIBIT-TOP-LEVEL-HELP))	;T if recursive, from clause below.
	 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	 (TV:OUTPUT-BEFORE-RUBOUT-HANDLER (SELF)
	   (FUNCALL SELF ':HELP-MESSAGE))
	 (VALUES CHAR T))
	((= (CHAR-UPCASE CHAR) ESCAPE-CHAR)
	 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	 (LET ((INHIBIT-TOP-LEVEL-HELP T))
	   (FUNCALL SELF ':HANDLE-ESCAPE))
	 (VALUES CHAR T))
	(T (LET ((TEM (ASSQ CHAR TV:KBD-INTERCEPTED-CHARACTERS)))
	     (IF TEM (FUNCALL (CADR TEM) CHAR)
	       CHAR)))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

;;; Give a single character, or do rubout processing, throws to RUBOUT-HANDLER on editing.
(DEFUN DEFAULT-RUBOUT-HANDLER ()
  (DECLARE (:SELF-FLAVOR STREAM-MIXIN))
  (SETF (RHB-TYPEIN-POINTER) NIL)   ;Mark that old rubout handler is in use.
  (WHEN (RHB-INITIAL-ENTRY)
    (LET ((INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS))))
      (WHEN INITIAL-INPUT
	(STRING-NCONC RUBOUT-HANDLER-BUFFER INITIAL-INPUT))))
  (SETF (RHB-INITIAL-ENTRY) NIL)
  (DO ((RUBOUT-HANDLER NIL)
       (RUBBED-OUT-SOME NIL)
       (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS)))
       CH LEN)
      (NIL)
    (SETQ CH (FUNCALL-SELF ':ANY-TYI))
    (COND ((AND (CONSP CH) (EQ (CAR CH) 'REDISPLAY-RUBOUT-HANDLER))
	   (SEND SELF ':SET-CURSORPOS PROMPT-STARTING-X PROMPT-STARTING-Y)
	   (SEND SELF ':CLEAR-EOL)
	   (AND (SETQ LEN (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
			      (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
		(RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
	   (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	     (FUNCALL-SELF ':READ-CURSORPOS))
	   (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
	  ((LISTP CH))
	  ((AND (NOT (MEMQ CH PASS-THROUGH))		;Is it an editing character?
		(NOT (SI:ASSQ-CAREFUL CH PASS-THROUGH))
		(OR (LDB-TEST %%KBD-CONTROL-META CH)
		    (MEMQ CH '(#\RUBOUT #\CLEAR-INPUT #\CLEAR-SCREEN #\VT))))
	   (COND ((MEMQ CH '(#\CLEAR-SCREEN #\VT))	;Retype buffered input
		  (FUNCALL-SELF ':TYO CH)		;Echo it
		  (IF (= CH #\CLEAR-SCREEN) (FUNCALL-SELF ':CLEAR-SCREEN)
		      (FUNCALL-SELF ':TYO #\CR))
		  (MULTIPLE-VALUE (PROMPT-STARTING-X PROMPT-STARTING-Y)
		    (FUNCALL-SELF ':READ-CURSORPOS))
		  (AND (SETQ LEN (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
				     (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
		       (RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
		  (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
		    (FUNCALL-SELF ':READ-CURSORPOS))
		  (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
		 ((MEMQ CH '(#\RUBOUT #\RUBOUT #\CLEAR-INPUT))	;Delete some characters
		  (COND ((NOT (ZEROP (SETQ LEN (RHB-FILL-POINTER))))
			 (SETF (RHB-FILL-POINTER)
			   (SETQ LEN (SELECTQ CH
				       (#\RUBOUT (1- LEN))
				       (#\RUBOUT (STRING-BACKWARD-WORD
						    RUBOUT-HANDLER-BUFFER LEN))
				       (#\CLEAR-INPUT 0))))
			 (SETQ RUBBED-OUT-SOME T)
			 (MULTIPLE-VALUE-BIND (X Y)
			     (FUNCALL-SELF ':COMPUTE-MOTION RUBOUT-HANDLER-BUFFER 0 LEN
			       RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
			   (IF RUBOUT-HANDLER-RE-ECHO-FLAG
			       (SETQ X RUBOUT-HANDLER-STARTING-X Y RUBOUT-HANDLER-STARTING-Y))
			   (MULTIPLE-VALUE-BIND (CX CY) (FUNCALL-SELF ':READ-CURSORPOS)
			     (FUNCALL-SELF ':CLEAR-BETWEEN-CURSORPOSES X Y CX CY))
			   (FUNCALL-SELF ':SET-CURSORPOS X Y)
			   (AND RUBOUT-HANDLER-RE-ECHO-FLAG
				(FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))))))
		 (T (BEEP)))				;Undefined editing character
	   (COND ((AND (ZEROP (RHB-FILL-POINTER))
		       (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
		  (SETF (RHB-SCAN-POINTER) 0)
		  (*THROW 'RUBOUT-HANDLER T))))
	  (T						;It's a self-inserting character
	   ;; If this is first character typed in, re-get starting cursorpos since while
	   ;; waiting for input a notification may have been typed out.
	   (AND (ZEROP (RHB-FILL-POINTER))
		(MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
		  (FUNCALL-SELF ':READ-CURSORPOS)))
	   (IF (NOT (MEMQ CH (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-OPTIONS))))
	       (FUNCALL-SELF ':TYO CH))
	   (ARRAY-PUSH-EXTEND RUBOUT-HANDLER-BUFFER CH)
	   (COND (RUBBED-OUT-SOME
		  (SETF (RHB-SCAN-POINTER) 0)
		  (*THROW 'RUBOUT-HANDLER T))
		 (T
		  (SETF (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
		  (RETURN CH)))))))

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN SIMILAR-BUFFER-FILES-WARNING (BUFFER &AUX SAME-NAME SAME-TYPE SAME-EVERYTHING)
  "Warn if any buffer other than BUFFER is visiting the same or a similar file."
  (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
    (AND (NEQ (CDR ELT) BUFFER)
	 (BUFFER-PATHNAME (CDR ELT))
	 (BUFFER-FILE-ID (CDR ELT))
	 (NOT (NODE-SPECIAL-TYPE (CDR ELT)))
	 (IF (EQUALP (FUNCALL (BUFFER-PATHNAME BUFFER) ':STRING-FOR-EDITOR)
		     (FUNCALL (BUFFER-PATHNAME (CDR ELT)) ':STRING-FOR-EDITOR))
	     (RETURN (SETQ SAME-EVERYTHING (CDR ELT)))
	   (IF (EQUALP (FUNCALL (BUFFER-PATHNAME BUFFER) ':NAME)
		       (FUNCALL (BUFFER-PATHNAME (CDR ELT)) ':NAME))
	       (COND ((EQUALP (FUNCALL (BUFFER-PATHNAME BUFFER) ':TYPE)
			      (FUNCALL (BUFFER-PATHNAME (CDR ELT)) ':TYPE))
		      (SETQ SAME-TYPE (CDR ELT)))
		     (T (SETQ SAME-NAME (CDR ELT))))))))
  (IF SAME-EVERYTHING
      (FORMAT QUERY-IO "~&Warning: Buffer ~A is also visiting file ~A."
	      (BUFFER-NAME SAME-EVERYTHING) (BUFFER-PATHNAME SAME-EVERYTHING))
    (LET ((LOSER (OR SAME-TYPE SAME-NAME)))
      (IF LOSER
	  (FORMAT QUERY-IO "~&Note: Another buffer ~A is visiting file ~A."
		  (BUFFER-NAME LOSER) (BUFFER-PATHNAME LOSER))))))

))

; From file LOOP.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOOP  "

(defun loop-translate-1 (loop-source-code)
  (and (memq (car loop-source-code) '(loop :loop))
       (setq loop-source-code (cdr loop-source-code)))
  (if (not (symbolp (car loop-source-code)))
      ;; This is Common Lisp's LOOP.
      `(do () (()) . ,loop-source-code)
    (do ((loop-iteration-variables ())
	 (loop-iteration-variablep ())
	 (loop-variables ())
	 (loop-nodeclare ())
	 (loop-named-variables ())
	 (loop-declarations ())
       #-System-Destructuring
	 (loop-desetq-crocks ())
	 (loop-variable-stack ())
	 (loop-declaration-stack ())
       #-System-destructuring
	 (loop-desetq-stack ())
	 (loop-prologue ())
	 (loop-wrappers ())
	 (loop-before-loop ())
	 (loop-body ())
	 (loop-emitted-body? ())
	 (loop-after-body ())
	 (loop-epilogue ())
	 (loop-after-epilogue ())
	 (loop-conditionals ())
	 (loop-when-it-variable ())
	 (loop-never-stepped-variable ())
       #-System-Destructuring
	 (loop-desetq-temporary ())
       #+Named-PROGs
	 (loop-prog-names ())
	 (loop-collect-cruft ())
	 (loop-collection-crocks ())
	 (keyword)
	 (tem)
	 (progvars))
	((null loop-source-code)
	 (and loop-conditionals
	      (loop-simple-error "Hanging conditional in loop macro"
				 (caadar loop-conditionals)))
	 (loop-optimize-duplicated-code-etc)
	 (loop-bind-block)
	 (setq progvars loop-collection-crocks)
       #-System-Destructuring
	 (and loop-desetq-temporary (push loop-desetq-temporary progvars))
	 (setq tem `(prog #+Named-PROGs ,.loop-prog-names
			  ,progvars
			#+Hairy-Collection
			  ,.(do ((l loop-collection-crocks (cddr l))
				 (v () (cons `(loop-collect-init
						  ,(cadr l) ,(car l))
					      v)))
				((null l) v))
			,.(nreverse loop-prologue)
			,.loop-before-loop
		     next-loop
			,.loop-body
			,.loop-after-body
			(go next-loop)
			; Multics complr notices when end-loop is not gone
			; to.  So we put in a dummy go.  This does not generate
			; extra code, at least in the simple example i tried,
			; but it does keep it from complaining about unused
			; go tag.
	      #+Multics (go end-loop)
		     end-loop
			,.(nreverse loop-epilogue)
			,.(nreverse loop-after-epilogue)))
	 (do ((vars) (dcls) #-System-Destructuring (crocks))
	     ((null loop-variable-stack))
	   (setq vars (car loop-variable-stack)
		 loop-variable-stack (cdr loop-variable-stack)
		 dcls (car loop-declaration-stack)
		 loop-declaration-stack (cdr loop-declaration-stack)
		 tem (ncons tem))
	   #-System-Destructuring
	     (and (setq crocks (pop loop-desetq-stack))
		  (push (loop-make-desetq crocks) tem))
	   (and dcls (push (cons 'declare dcls) tem))
	   (cond ((do ((l vars (cdr l))) ((null l) ())
		    (and (not (atom (car l)))
			 (or (null (caar l)) (not (symbolp (caar l))))
			 (return t)))
		    (setq tem `(let ,(nreverse vars) ,.tem)))
		 (t (let ((lambda-vars ()) (lambda-vals ()))
		      (do ((l vars (cdr l)) (v)) ((null l))
			(cond ((atom (setq v (car l)))
				 (push v lambda-vars)
				 (push () lambda-vals))
			      (t (push (car v) lambda-vars)
				 (push (cadr v) lambda-vals))))
		      (setq tem `((lambda ,lambda-vars ,.tem)
				  ,.lambda-vals))))))
	 (dolist (w loop-wrappers)
	   (setq tem (append w (ncons tem))))
	 tem)
      (if (symbolp (setq keyword (loop-pop-source)))
	  (if (setq tem (si:loop-tassoc keyword loop-keyword-alist))
	      (apply (cadr tem) (cddr tem))
	      (if (setq tem (si:loop-tassoc
			       keyword loop-iteration-keyword-alist))
		  (loop-hack-iteration tem)
		  (if (si:loop-tmember keyword '(and else))
		      ; Alternative is to ignore it, ie let it go around to the
		      ; next keyword...
		      (loop-simple-error
			 "secondary clause misplaced at top level in LOOP macro"
			 (list keyword (car loop-source-code)
			       (cadr loop-source-code)))
		      (loop-simple-error
			 "unknown keyword in LOOP macro" keyword))))
	  (loop-simple-error
	     "found where keyword expected in LOOP macro" keyword)))))

))

; From file COMA.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-COMPLETE-REDISPLAY "Redisplay all windows.
By default, the text is not scrolled on the screen.
However, a numeric argument specifies the screen line
to scroll point to (negative counting from the bottom)." (KM)
  (DOLIST (W *WINDOW-LIST*)
    (PREPARE-WINDOW-FOR-REDISPLAY W))
  (SEND (SEND *WINDOW* ':ALIAS-FOR-SELECTED-WINDOWS) ':REFRESH)
  (WHEN *NUMERIC-ARG-P*
    (COM-RECENTER-WINDOW))
  DIS-NONE)

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN FIND-FILE-BUFFER (PATHNAME)
  "Return the buffer visiting PATHNAME, or NIL if none."
  (SETQ PATHNAME (FUNCALL PATHNAME ':TRANSLATED-PATHNAME))
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (AND (FS:PATHNAME-EQUAL PATHNAME (BUFFER-PATHNAME BUFFER))
	 (BUFFER-FILE-ID BUFFER)  ;Make sure it's a buffer really associated with a file.
	 (NOT (NODE-SPECIAL-TYPE BUFFER))  ;Not DIRED buffers either.
	 (RETURN BUFFER))))

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

;;; This makes the redisplay mechanism forget everything it knows.
(DEFMETHOD (ZWEI :AFTER :REFRESH) (&OPTIONAL TYPE)
  (WHEN (OR (NOT TV:RESTORED-BITS-P) (EQ TYPE ':SIZE-CHANGED))
    (PREPARE-WINDOW-FOR-REDISPLAY SELF)  ;Pop down any typeout window.
    (TELL-EDITOR-TO-REDISPLAY DIS-ALL)))

))

; From file METH.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "


(DEFMETHOD (FILE-BUFFER :NEEDS-SAVING-P) ()
  (AND PATHNAME FILE-ID
       (NOT (NODE-SPECIAL-TYPE SELF))
       (SEND SELF ':MODIFIED-P)))

(DEFMETHOD (FILE-BUFFER :MODIFIED-P) ()
  (OR (AND PATHNAME (EQ FILE-ID T))
      (> TICK FILE-TICK)))

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN KILL-BUFFER (BUFFER &OPTIONAL NO-SAVE-P)
  "Kill BUFFER; remove it from the list which can be selected.
Offers to save it if it is a modified file buffer, unless NO-SAVE-P."
  ;; If the buffer is associated with a file and contains changes, offer to write it out.
  (AND (NOT NO-SAVE-P)
       (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (CONSP (BUFFER-FILE-ID BUFFER))
	   (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER))))
       (FQUERY `(:SELECT T
		 :BEEP T
		 :TYPE :READLINE
		 :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
	       "Buffer ~A has been modified, save it first? "
	       (BUFFER-NAME BUFFER))
       (SAVE-BUFFER BUFFER))
  ;; If buffer is current, select something else before killing.
  (DO () ((NOT (EQ BUFFER *INTERVAL*)))
    (MUST-REDISPLAY *WINDOW*
		    (SELECT-BUFFER
		      "Killing the current buffer, select which other buffer?"
		      'MAYBE)))
  ;; Anybody who refers to this buffer should be redirected.
  (SEND BUFFER ':KILL)
  T)

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-KILL-SOME-BUFFERS "Offer to kill each buffer.
For each buffer, ask whether to kill it, and for each one to be killed, offer to write
out any changes." ()
  (LET ((QUERY-IO STANDARD-OUTPUT))
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (FQUERY '(:SELECT T) "Buffer ~A ~A, kill it? "
		   (BUFFER-NAME BUFFER)
		   (COND ((BP-= (INTERVAL-FIRST-BP BUFFER)
				(INTERVAL-LAST-BP BUFFER))
			  "is empty")
			 ((NULL (BUFFER-FILE-ID BUFFER))
			  "has no file associated with it")
			 ((EQ (BUFFER-FILE-ID BUFFER) T)
			  "is a new file")
			 ((BUFFER-NEEDS-SAVING-P BUFFER)
			  "has been edited")
			 (T "is unmodified")))
	   (KILL-BUFFER BUFFER))))
  DIS-NONE)

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-SAVE-FILE "Write out changes to current file.
If the current buffer has no file, reads in a file name from the mini buffer." ()
  (COND ((AND (NOT (SYMBOLP (BUFFER-FILE-ID *INTERVAL*)))
	      (NOT (BUFFER-NEEDS-SAVING-P *INTERVAL*)))
	 (FORMAT QUERY-IO "~&(No changes need to be written.)")
	 DIS-NONE)
	(T
	 (SAVE-BUFFER *INTERVAL*)
	 (MAYBE-DISPLAY-DIRECTORY ':WRITE)
	 DIS-NONE)))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

;Pass 1 processing for a call to an ordinary function (ordinary, at least, for pass 1).
;FORM is the call to the function, and DESC is the GETARGDESC of the function.
;Processing consists of P1'ing all evaluated arguments, but not the quoted ones.
;DESC is used to determine which is which.
;In addition, &FUNCTIONAL arguments are broken off and separately compiled.
;We process the args by copying the arglist, and rplaca'ing each arg by P1 of itself if needed.
(DEFUN P1ARGC (FORM DESC)
       (PROG (COUNT TOKEN-LIST ARGS-LEFT DESCS-LEFT ARG-P1-RESULTS FCTN TM P1VALUE)
	     (SETQ P1VALUE T)
	     (SETQ DESCS-LEFT DESC)
	     (SETQ FCTN (CAR FORM))
	     (COND ((AND DESCS-LEFT
			 (MEMQ 'FEF-ARG-REST (SETQ TM (CADAR DESCS-LEFT)))
			 (MEMQ 'FEF-QT-QT TM))
		    (RETURN FORM))) 		;JUST FOR "EFFICIENCY"
	     (SETQ ARG-P1-RESULTS (SETQ ARGS-LEFT (COPYLIST (CDR FORM))))
             (SETQ COUNT 0)
	L3
	     ;; If all arguments processed, return.
	     (COND ((NULL ARGS-LEFT)
		    (RETURN (CONS FCTN ARG-P1-RESULTS)))
		   ((ATOM ARGS-LEFT)
		    (WARN ':IMPOSSIBLE 'NON-NIL-END-OF-FORM
			  "The form ~S ends in a non-NIL atomic cdr."
			  FORM)
		    (IF (ATOM ARG-P1-RESULTS)
			(RETURN (LIST FCTN))
		      (SETF (CDR (LAST ARG-P1-RESULTS)) NIL)
		      (RETURN (CONS FCTN ARG-P1-RESULTS)))))

	     ;; Figure out what descriptor to use for the next argument.
	     ;; TOKEN-LIST is the actual descriptor, and COUNT
	     ;; is the number of argumens left for it to apply to.
	     (COND ((ZEROP COUNT)
		    (COND ((NULL DESCS-LEFT)
			   ;; Out of descriptors => treat excess args as evalled.
			   (SETQ DESCS-LEFT '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))))
		    (SETQ COUNT (CAAR DESCS-LEFT))
		    (SETQ TOKEN-LIST (CADAR DESCS-LEFT))
		    (SETQ DESCS-LEFT (CDR DESCS-LEFT))
		    (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
			   (SETQ COUNT 1005)))))

	     ;; Process the next argument according to its descriptor.
	     (COND ((MEMQ 'FEF-QT-QT TOKEN-LIST))
		   ((OR (MEMQ 'FEF-QT-EVAL TOKEN-LIST)
			(MEMQ 'FEF-QT-DONTCARE TOKEN-LIST))
		    (RPLACA ARGS-LEFT
			  (COND ((AND (MEMQ 'FEF-FUNCTIONAL-ARG TOKEN-LIST)
				      (NOT (ATOM (SETQ TM (OPTIMIZE (CAR ARGS-LEFT) T))))
				      (EQ (CAR TM) 'QUOTE))	 ;LOOK FOR '(LAMBDA...)
				 (P1FUNCTION TM))
				(T (P1 (CAR ARGS-LEFT))))))
		   (T (BARF TOKEN-LIST 'BAD-EVAL-CODE 'BARF)))
	     (SETQ ARGS-LEFT (CDR ARGS-LEFT))
	     (SETQ COUNT (1- COUNT))
	     (GO L3)))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN UNDEFFLAVOR (FLAVOR-NAME &AUX FL)
  "Make the flavor named FLAVOR-NAME cease to be defined."
  (CHECK-ARG FLAVOR-NAME (EQ 'FLAVOR (TYPEP (SETQ FL (IF (SYMBOLP FLAVOR-NAME)
							 (GET FLAVOR-NAME 'FLAVOR)
							 FLAVOR-NAME))))
	     "a flavor or the name of one")
  (DOLIST (DEPENDENT (FLAVOR-DEPENDED-ON-BY FL))
    (PUSH (CONS (FLAVOR-NAME FL) DEPENDENT)
	  *FLAVOR-PENDING-DEPENDS*))
  (REMPROP (FLAVOR-NAME FL) 'FLAVOR))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


; :LIST combination
; No typed-methods allowed.  Returns a list of the results of all the methods.
; There will always be a combined-method, even if only one method to be called.
(DEFUN (:LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	    `(LIST
	       . ,(MAPCAR 'METHOD-CALL
			  (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':LIST '(NIL) NIL NIL)
				  (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:LIST) NIL NIL)))))))

; :INVERSE-LIST combination
; No typed-methods allowed.  Apply each method to an element of the list.  Given
; the result of a :LIST-combined method with the same ordering, and corresponding
; method definitions, the result that emerged from each component flavor gets handed
; back to that same flavor.  The combined-method returns no particular value.
(DEFUN (:INVERSE-LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	 `(LET ((.FOO. (CADR .DAEMON-CALLER-ARGS.)))
	    . ,(DO ((ML (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':INVERSE-LIST '(NIL)
						     NIL NIL)
				(GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:INVERSE-LIST)
						     NIL NIL))
			(CDR ML))
		    (R NIL))
		   ((NULL ML) (NREVERSE R))
		 (PUSH `(FUNCALL-WITH-MAPPING-TABLE-INTERNAL
			  #',(CAR ML) (METHOD-MAPPING-TABLE ,(CAR ML))
			  (CAR .DAEMON-CALLER-ARGS.) (CAR .FOO.))
		       R)
		 (AND (CDR ML) (PUSH '(SETQ .FOO. (CDR .FOO.)) R)))))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN SIMPLE-METHOD-COMBINATION (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY (CADR MAGIC-LIST-ENTRY) '(NIL)
					      NIL NIL)
			 (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
					      (LIST (CADR MAGIC-LIST-ENTRY)) NIL NIL)))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    (OR (AND (NOT WRAPPERS-P) (NULL (CDR METHODS)) (CAR METHODS))
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   (CONS (GET (CADR MAGIC-LIST-ENTRY) 'SIMPLE-METHOD-COMBINATION)
		 (MAPCAR 'METHOD-CALL
			 METHODS))))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

; :PASS-ON combination
; The values from the individual methods are the arguments to the next one;
; the values from the last method are the values returned by the combined
; method.  Format is (:METHOD-COMBINATION (:PASS-ON (ORDERING . ARGLIST) . OPERATION-NAMES)
; ORDERING is :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.  ARGLIST can have &AUX and &OPTIONAL.

(DEFUN (:PASS-ON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':PASS-ON '(NIL)
					      NIL (CAADDR MAGIC-LIST-ENTRY))
			 (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:PASS-ON)
					      NIL (CAADDR MAGIC-LIST-ENTRY))))
	(ARGLIST (CDADDR MAGIC-LIST-ENTRY))
	ARGS REST-ARG-P)
    (DO ((L ARGLIST (CDR L))
	 (ARG)
	 (NL NIL))
	((NULL L)
	 (SETQ ARGS (NREVERSE NL)))
      (SETQ ARG (CAR L))
      (AND (CONSP ARG)
	   (SETQ ARG (CAR ARG)))
      (COND ((EQ ARG '&REST)
	     (SETQ REST-ARG-P T))
	    ((EQ ARG '&AUX))
	    (T
	     (PUSH ARG NL))))      
    (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  `(DESTRUCTURING-BIND ,(CONS '.OPERATION. ARGLIST) SI:.DAEMON-CALLER-ARGS.
	     . ,(DO ((METHS METHODS (CDR METHS))
		     (LIST NIL)
		     (METH))
		    ((NULL METHS)
		     (NREVERSE LIST))
		  (SETQ METH `(,(IF REST-ARG-P
				    'LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL
				  'FUNCALL-WITH-MAPPING-TABLE-INTERNAL)
			       #',(CAR METHS) (METHOD-MAPPING-TABLE ,(CAR METHS))
			       .OPERATION. . ,ARGS))
		  (AND (CDR METHS)
		       (SETQ METH (IF (NULL (CDR ARGS))
				      `(SETQ ,(CAR ARGS) ,METH)
				    `(MULTIPLE-VALUE ,ARGS ,METH))))
		  (PUSH METH LIST)))))))

))
