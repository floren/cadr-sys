;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Written 6/11/84 06:30:36 by Mly,
;;; Reason: TV:MULTIPLE-CHOOSE does not bash user args when is supplies default implications
;;; lap bug preventing compiler rentrancy
;;; DEFLOCF
;;; (SETF (%POINTER ...) ...)
;;; STRING-APPEND uses "maximal" array type for result -- not necessarily type of first arg
;;; Define accessors for fields of floats: (SI:%SINGLE-FLOAT-EXPONENT, etc)
;;; Some fasdump operations clobbering FASD-PACKAGE
;;; SXHASH typo. Foo.
;;; describing lexical closures made by compiled code
;;; bug in compilation of (proclaim '(special ...))
;;; while running on Lisp Machine One from band 7
;;; with System 98.56, CADR 3.6, ZMail 53.17, MIT-Specific 22.1, microcode 309.


(globalize (intern "DEFLOCF" "SI") "GLOBAL")
(globalize (intern "NON-COMPLEX-NUMBER" "SI") "GLOBAL")
(globalize (intern "MULTIPLE-CERROR" "EH") "SYSTEM")


; From file CHOICE.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; CHOICE  "

(DEFUN MULTIPLE-CHOOSE (ITEM-NAME ITEM-LIST KEYWORD-ALIST
			&OPTIONAL (NEAR-MODE '(:MOUSE)) (MAXLINES 20.) SUP)
  "Ask several multiple-choice questions with a menu-like window.
ITEM-NAME is a string of the name of the type of item, e.g. /" Buffer/".
 It goes in the label, above the item names.
ITEM-LIST is an alist, in which each element is (ITEM NAME CHOICES).
 ITEM is the item itself, NAME its name (a string),
 and CHOICES a list of possible keywords, either KEYWORD or (KEYWORD DEFAULT),
 where if DEFAULT is non-NIL the KEYWORD is initially on.
 The purpose of ITEM is that it appears in the value returned,
 and allows you to identify what the returned answers apply to.
KEYWORD-ALIST is a list of the possible keywords, (KEYWORD NAME . IMPLICATIONS).
 KEYWORD is a symbol, the same as in ITEM-LIST's CHOICES.
 NAME is a string of its name.
 IMPLICATIONS is a list of on-positive, on-negative, off-positive, and off-negative
 implications for when the keyword is selected, each one either a list of (other) keywords
 or T for all other keywords.  The default for IMPLICATIONS is (NIL T NIL NIL).

We return two values: first, the list of choices chosen;
second, the reason why we exited (NIL means normal exit).
Each element of the first value is a list (ITEM SELECTED-CHOICES...)."
  (DECLARE (VALUES CHOICES EXIT-REASON))
  ;; Decide what superior to use
  (UNLESS SUP
    (SETQ SUP (IF (EQ (CAR NEAR-MODE) ':WINDOW)
		  (SHEET-SUPERIOR (CADR NEAR-MODE))
		  MOUSE-SHEET)))
  ;; avoid bashing user's code!
  (unless (loop for x in keyword-alist always (cdddr x))
    (setq keyword-alist (copylist keyword-alist))
    (DO ((L KEYWORD-ALIST (CDR L)))
	((NULL L))
      (AND (< (LENGTH (CAR L)) 3)
	   (SETF (CAR L) (APPEND (CAR L) (LIST NIL T NIL NIL))))))
  (USING-RESOURCE (WINDOW TEMPORARY-MULTIPLE-CHOICE-WINDOW SUP)
    (SEND WINDOW ':SETUP ITEM-NAME KEYWORD-ALIST
	  	 DEFAULT-FINISHING-CHOICES ITEM-LIST MAXLINES)
    (UNWIND-PROTECT
      (SEND WINDOW ':CHOOSE NEAR-MODE)
      (SEND WINDOW ':DEACTIVATE))))

))

; From file QCLAP.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLAP  "

(DEFUN QLAPP (FCTN LAP-MODE)
  (PROG (SYMTAB ADR NBR SYMPTR QLP-A-D-L-DONE SPECVARS SPECVARS-BIND-COUNT LOW-HALF-Q
	 MAX-ARGS MIN-ARGS SM-ARGS-NOT-EVALD REST-ARG HAIRY-INIT-FLAG
	 DATA-TYPE-CHECKING-FLAG LENGTH-OF-PROG PROG-ORG FCTN-NAME 
	 LAP-OUTPUT-AREA TEM LAP-NO-ADL LAP-LASTQ-MODIFIER ADL-LENGTH A-D-L-NEEDED-P
	 QUOTE-LIST QUOTE-COUNT S-V-BITMAP-ACTIVE ALLVARS FREEVARS QLP-DEBUG-INFO
	 LAP-OUTPUT-BLOCK LAP-OUTPUT-BLOCK-LENGTH LAP-STORE-POINTER LAP-MACRO-FLAG
	 BREAKOFF-FUNCTION-OFFSETS QLP-SELF-FLAVOR LAP-FASD-NIBBLE-COUNT
	 FASD-GROUP-LENGTH)
	(SETQ LAP-OUTPUT-AREA 'MACRO-COMPILED-PROGRAM)
	(SETQ MAX-ARGS (SETQ MIN-ARGS 0))
	(SETQ SYMTAB (LIST NIL))
	(SETQ QUOTE-COUNT 0)
	(SETQ ADR 0)
	(SETQ ALLVARS (CADDDR (CAR FCTN))
	      FREEVARS (CADDDR (CDAR FCTN)))
	(SETQ SPECVARS (EXTRACT-SPECVARS))
	(SCAN-ARGS)
	(COMPUTE-A-D-L-NEEDED-P)
	(QLAP-PASS1 FCTN)
	(RPLACD SYMTAB (NREVERSE (CDR SYMTAB)))
	(SETQ QUOTE-LIST (NREVERSE QUOTE-LIST))	;JUST SO FIRST ONES WILL BE FIRST
	(SETQ TEM (LAP-SYMTAB-PLACE 'QUOTE-BASE))
	(LAP-SYMTAB-RELOC (CADDAR TEM)		;VALUE OF QUOTE-BASE
			  (* 2 (LENGTH QUOTE-LIST))
			  (CDR TEM))
	(SETQ NBR (QLAP-ADJUST-SYMTAB))	;NUMBER BRANCHES TAKING EXTRA WD
	(SETQ LENGTH-OF-PROG (+ ADR (+ NBR (* 2 (LENGTH QUOTE-LIST)))))
	(SETQ SYMPTR SYMTAB)
	(SETQ QUOTE-COUNT 0)
	(SETQ ADR 0)
	(SETQ ADL-LENGTH (OR QLP-A-D-L-DONE 0))
	(SETQ QLP-A-D-L-DONE NIL)
	(QLAP-PASS2 FCTN)
	;Don't call FASD with the temporary area in effect
	(LET-IF QC-FILE-IN-PROGRESS ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
	  (COND ((OR LOW-HALF-Q 
		     (AND (OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
			  (NOT (= 0 (LOGAND ADR 1)))))
		 (LAP-OUTPUT-WORD 0)))
	  (COND ((EQ LAP-MODE 'QFASL)
		 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))
		 (COND ((NOT (= 0 LAP-FASD-NIBBLE-COUNT))
			(BARF LAP-FASD-NIBBLE-COUNT 
			      'LAP-FASD-NIBBLE-COUNT 
			      'BARF)))
		  ;; If this function is supposed to be a macro,
		  ;; dump directions to cons MACRO onto the fef.
		 (COND (LAP-MACRO-FLAG
			 (FASD-START-GROUP T 1 FASL-OP-LIST)
			 (FASD-NIBBLE 2)
			 (FASD-CONSTANT 'MACRO)
			 (FASD-START-GROUP NIL 1 FASL-OP-INDEX)
			 (FASD-NIBBLE TEM)
			 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))))
		 (FASD-STOREIN-FUNCTION-CELL FCTN-NAME TEM)
		 (FASD-FUNCTION-END)
		 (RETURN NIL))
		((EQ LAP-MODE 'QFASL-NO-FDEFINE)
		 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))
		 (COND ((NOT (= 0 LAP-FASD-NIBBLE-COUNT))
			(BARF LAP-FASD-NIBBLE-COUNT 
			      'LAP-FASD-NIBBLE-COUNT 
			      'BARF)))
		  ;; If this function is supposed to be a macro,
		  ;; dump directions to cons MACRO onto the fef.
		 (COND (LAP-MACRO-FLAG
			 (FASD-START-GROUP T 1 FASL-OP-LIST)
			 (FASD-NIBBLE 2)
			 (FASD-CONSTANT 'MACRO)
			 (FASD-START-GROUP NIL 1 FASL-OP-INDEX)
			 (FASD-NIBBLE TEM)
			 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))))
		 (RETURN TEM))
		((EQ LAP-MODE 'COMPILE-TO-CORE)
;		 (SI:FEF-CONVERT-DESTINATIONS LAP-OUTPUT-BLOCK)
		 (FSET-CAREFULLY FCTN-NAME
				 (COND (LAP-MACRO-FLAG (CONS-IN-AREA 'MACRO LAP-OUTPUT-BLOCK
								     BACKGROUND-CONS-AREA))

				       (T LAP-OUTPUT-BLOCK))))
		((EQ LAP-MODE 'REL)
;		 (SI:FEF-CONVERT-DESTINATIONS LAP-OUTPUT-BLOCK)
		 (QFASL-REL:DUMP-LAP-FSET FCTN-NAME LAP-OUTPUT-BLOCK))
	     (T (FERROR NIL "~S is a bad lap mode" LAP-MODE)) ))
)) 

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method %pointer (int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int)
    (let ((store (gensym))
	  (itemp (first stores)))
      (values temps
	      vals
	      (list store)
	      `(progn 
		 ,(sublis (list (cons itemp `(%make-pointer (%data-type ,access-form)
							    ,store)))
			  store-form)
		 ,store)
	      `(%pointer ,access-form)))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro deflocf (access-function &optional arg1 &body body)
  "Define how to LOCF forms starting with ACCESS-FUNCTION
Examples:
/(DEFLOCF CAR SI:CAR-LOCATION) or
/(DEFLOCF CAR (X) `(SI:CAR-LOCATION ,X)).
/(DEFLOCF FOO) explicitly forbids doing LOCF on (FOO ...)."
  (declare (arglist access-function &optional lambda-list-or-location-function &body body))
  (if (null body)
      `(defdecl ,access-function
		locf-method
		,(or arg1
		     '(macro . nolocf)))
    `(defmacro (:property ,access-function locf-method)
	       ,arg1 . ,body)))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defprop nolocf t :error-reporter)

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defun nolocf (form)
  (ferror 'unknown-locf-reference
	  "LOCF is explicitly forbidden on ~S." (car form)))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING-APPEND (&REST STRINGS
		      &AUX (LENGTH 0) (BITS 0) B TY (TYPE 'ART-STRING) FROB) 
  "Append any number of strings (or vectors).  The value is always a newly constructed array.
The value will have be of an array type which can contain the elements of all the STRINGS.
Symbols and numbers are coerced into strings."
  (DOLIST (S STRINGS)
    (TYPECASE S
      ((OR FIXNUM CHARACTER)
       (INCF LENGTH 1)
       (COND ((< S (^ 2 8)) (SETQ B 8 TY 'ART-STRING))
	     ((< S (^ 2 16.)) (SETQ B 16. TY 'ART-FAT-STRING))
	     (T (SETQ B %%Q-POINTER TY ART-Q))))
      (VECTOR
       (INCF LENGTH (LENGTH S))
       (SETQ B (ARRAY-ELEMENT-SIZE S) TY (ARRAY-TYPE S)))
      (SYMBOL
       (INCF LENGTH (LENGTH (SYMBOL-NAME S)))
       (SETQ B 8 TY 'ART-STRING))
      ((AND INSTANCE (SATISFIES (LAMBDA (STRING)
				(SEND STRING :OPERATION-HANDLED-P :STRING-FOR-PRINTING))))
       (PUSH (SETQ S (SEND S :STRING-FOR-PRINTING)) FROB)
       (INCF LENGTH (LENGTH S))
       (SETQ B 8 TY 'ART-STRING))
      (T 
       (FERROR NIL "Cannot convert ~S into a string." S)))
    (WHEN (> B BITS)
      (SETQ BITS B TYPE TY)))
  (SETQ FROB (NREVERSE FROB))
  (LET ((STRING (MAKE-ARRAY LENGTH :TYPE TYPE))
	(I 0)
	COERCED)
    (DOLIST (S STRINGS)
      (TYPECASE S
	((OR NUMBER CHARACTER)
	 (SETF (AREF STRING I) S)
	 (INCF I 1))
	(T (SETQ COERCED (TYPECASE S
			   (VECTOR S)
			   (SYMBOL (SYMBOL-NAME S))
			   (T (POP FROB))))
	   (COPY-ARRAY-PORTION COERCED 0 (SETQ LENGTH (ARRAY-ACTIVE-LENGTH COERCED))
			       STRING I (INCF I LENGTH)))))
    STRING))

))

; From file NUMDEF.LISP PS:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defsubst %short-float-exponent (short-float)
  (ldb (byte 7 17.) (%pointer short-float)))

(defsubst %single-float-exponent (single-float)
  (%p-ldb (byte 11. 8) single-float))

(defsubst %short-float-mantissa (short-float)
  (ldb (byte 17. 0) (%pointer short-float)))

(defsubst %single-float-mantissa (single-float)
  (dpb (%p-ldb-offset (byte 8 0) single-float 0)
       (byte 8 24.)
       (dpb (%p-ldb-offset (byte 8 16.) single-float 1) ;Extra DPB fixes negative
            (byte 8 16.)                        	;fixnum lossages
            (%p-ldb-offset (byte 16. 0) single-float 1))))

(defsetf %single-float-mantissa (single-float) (value)
  `(progn
     (setf (%p-ldb-offset (byte 8 0) ,single-float 0) (ldb (byte 8 24.) ,value))
     (setf (%p-ldb-offset (byte 24. 0) ,single-float 1) (ldb (byte 24. 0) ,value))
     ,value))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-WRITE-STRING (OBJECT GROUP-TYPE)
  (LET* ((STRING (STRING OBJECT))
	 (LENGTH (LENGTH STRING)))
    (FASD-START-GROUP NIL (CEILING LENGTH 2) GROUP-TYPE)
    (DO ((I 0 (+ I 2))
	 C0 C1)
	(( I LENGTH))
      (SETQ C0 (AREF STRING I)
	    C1 (IF (= (1+ I) LENGTH)
		   #o200
		 (AREF STRING (1+ I))))
      (FASD-NIBBLE (+ (LSH C1 8) C0)))))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-ATTRIBUTES-LIST (PLIST &OPTIONAL (ADD-FASD-DATA T))
  (WHEN ADD-FASD-DATA
    (MULTIPLE-VALUE-BIND (MAJOR MINOR)
	(SI:GET-SYSTEM-VERSION "System")
      (SETQ PLIST (LIST* ':FASD-DATA
			 `(,USER-ID
			   ,SI:LOCAL-PRETTY-HOST-NAME
			   ,(TIME:GET-UNIVERSAL-TIME)
			   ,MAJOR ,MINOR
			   (NEW-DESTINATIONS T   ;; NOT :new-destinations!!
			    :SITE ,(SHORT-SITE-NAME)))
			 PLIST))))
  (LET ((P (GETL (LOCF PLIST) '(:PACKAGE))))
    (WHEN P
      (SETQ FASD-PACKAGE (PKG-FIND-PACKAGE (CADR P)))))
  (FASD-START-GROUP NIL 0 FASL-OP-FILE-PROPERTY-LIST)
  ;; Put package prefixes on everything in the plist since it will be loaded in
  ;; the wrong package.  This way the symbols in the plist will always be loaded
  ;; into exactly the same package they were dumped from, while the rest of the
  ;; symbols in the file will be free to follow the usual rules for intern.
  (LET ((FASD-PACKAGE NIL))
    (FASD-CONSTANT PLIST)))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-SYMBOL-VALUE (FILENAME SYMBOL &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME containing SYMBOL's value.
Loading the file will set the symbol back to the same value."
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME
							   FS:LOAD-PATHNAME-DEFAULTS
							   ':QFASL)
			       ':DIRECTION ':OUTPUT
			       ':CHARACTERS NIL
			       ':BYTE-SIZE 16.)
    (LET ((FASD-PACKAGE NIL))			;in case fasd-attributes-list bashes it
      (LOCKING-RESOURCES
	(FASD-INITIALIZE)
	(FASD-START-FILE)
	(FASD-ATTRIBUTES-LIST
	  (IF (GETL (LOCF ATTRIBUTE-LIST) '(:PACKAGE))
	      ATTRIBUTE-LIST
	    (LIST* ':PACKAGE (PACKAGE-NAME (SYMBOL-PACKAGE SYMBOL)) ATTRIBUTE-LIST)))
	(FASD-FORM `(SETQ ,SYMBOL ',(SYMBOL-VALUE SYMBOL)))
	(FASD-END-WHACK)
	(FASD-END-FILE)))))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN DUMP-FORMS-TO-FILE (FILENAME FORMS-LIST &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME which, when loaded, will execute the forms in FORMS-LIST.
ATTRIBUTE-LIST is a file attribute list which controls, among other things,
what package the file is dumped and loaded in (default is USER)."
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME
							   FS:LOAD-PATHNAME-DEFAULTS
							   ':QFASL)
			       ':DIRECTION ':OUTPUT
			       ':CHARACTERS NIL
			       ':BYTE-SIZE 16.)
    (LET ((FASD-PACKAGE NIL))			;in case fasd-attributes-list bashes it
      (LOCKING-RESOURCES
	(FASD-INITIALIZE)
	(FASD-START-FILE)
	(FASD-ATTRIBUTES-LIST
	  (IF (GETL (LOCF ATTRIBUTE-LIST) '(:PACKAGE))
	      ATTRIBUTE-LIST
	    (LIST* ':PACKAGE ':USER ATTRIBUTE-LIST)))
	(DOLIST (FORM FORMS-LIST)
	  (IF ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
	      (FASD-END-WHACK))
	  (FASD-FORM FORM))
	(FASD-END-WHACK)
	(FASD-END-FILE)))))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-FONT (FONT-SYMBOL)
  "Write the font FONT into a QFASL file named SYS: FONTS; name-of-font QFASL."
  (DUMP-FORMS-TO-FILE (FS:MAKE-PATHNAME ':HOST "SYS"
					':DIRECTORY "FONTS"
					':NAME (SYMBOL-NAME FONT-SYMBOL))
		      `((SETQ ,FONT-SYMBOL ,(TV::FONT-EVALUATE FONT-SYMBOL)))
		      '(:PACKAGE :FONTS)))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-FILE-SYMBOLS-PROPERTIES (FILENAME SYMBOLS PROPERTIES
				     	      DUMP-VALUES-P DUMP-FUNCTIONS-P
                                              NEW-SYMBOL-FUNCTION
					      &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME containing data on SYMBOLS.
The data can include the symbols' values, function definitions, and properties.
PROPERTIES is a list of which propertis should be dumped.
DUMP-VALUES-P says whether to dump their values.
DUMP-FUNCTIONS-P says whether to dump their function definitions.
NEW-SYMBOL-FUNCTION is a function to call whenever a new symbol
not previously seen is found in a value being dumped.  The function
can cause the new symbol's data to be dumped like the specified symbols.
When the NEW-SYMBOL-FUNCTION is called, FASD-SYMBOL-LIST will be a list
of symbols waiting to be dumped, and FASD-ALREADY-DUMPED-SYMBOL-LIST a
list of those already dumped.  To make a new symbol be dumped, push it
on the former if it is not in either of those two."
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME
							   FS:LOAD-PATHNAME-DEFAULTS
							   ':QFASL)
			       ':DIRECTION ':OUTPUT
			       ':CHARACTERS NIL
			       ':BYTE-SIZE 16.)
    (LET ((FASD-PACKAGE NIL))			;in case fasd-attributes-list bashes it
      (LOCKING-RESOURCES
	(FASD-INITIALIZE)
	(FASD-START-FILE)
	(FASD-ATTRIBUTES-LIST
	  (IF (GETL (LOCF ATTRIBUTE-LIST) '(:PACKAGE))
	      ATTRIBUTE-LIST
	    (LIST* ':PACKAGE ':USER ATTRIBUTE-LIST)))
	(FASD-SYMBOLS-PROPERTIES SYMBOLS PROPERTIES DUMP-VALUES-P
				 DUMP-FUNCTIONS-P NEW-SYMBOL-FUNCTION)
	(FASD-END-WHACK)
	(FASD-END-FILE)))))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-SINGLE-FLOAT (N)
  (FASD-START-GROUP NIL 3 FASL-OP-FLOAT)
  (FASD-NIBBLE (%P-LDB-OFFSET #o1013 N 0))
  (FASD-NIBBLE (DPB (%P-LDB-OFFSET #o0010 N 0) #o1010 (%P-LDB-OFFSET #o2010 N 1)))
  (FASD-NIBBLE (%P-LDB-OFFSET #o0020 N 1))
  NIL)

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(defsubst %QCFASD-short-float-exponent (short-float)
  (ldb (byte 8 17.) (%pointer short-float)))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-SHORT-FLOAT (N)
  (LET ((EXP (- (%QCFASD-SHORT-FLOAT-EXPONENT N) #o200)))
    ;; If exponent is in range for FASL-OP-FLOAT, use it.
    (IF ( #o-100 EXP #o77)
	(FASD-OLD-SMALL-FLOAT N)
      (FASD-NEW-SMALL-FLOAT N))))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-OLD-SMALL-FLOAT (N)
  (SETQ N (%MAKE-POINTER DTP-FIX N))		;So that LDB's will work.
  (SETQ N (%POINTER-DIFFERENCE N #o40000000))	;Convert excess #o200 exponent to excess #o100
  (FASD-START-GROUP T 2 FASL-OP-FLOAT)
  (FASD-NIBBLE (LDB #o2010 N))
  (FASD-NIBBLE (LDB #o0020 N))
  NIL)

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-NEW-SMALL-FLOAT (N &AUX FRACTION EXPONENT)
  (FASD-START-GROUP (MINUSP N) 5 FASL-OP-NEW-FLOAT)
  (SETQ N (ABS N))
  (SETQ FRACTION (SI:%SHORT-FLOAT-MANTISSA N)
	EXPONENT (SI:%SHORT-FLOAT-EXPONENT N))
  (FASD-NIBBLE 8)				;8 bits for exponent, including sign
  (FASD-NIBBLE EXPONENT)
  (FASD-NIBBLE 17.)				;17 bits for mantissa, excluding sign
  (FASD-NIBBLE (LDB (BYTE 16. 0) FRACTION))
  (FASD-NIBBLE 1))				;implied leading digit

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-CONSTANT (S-EXP &OPTIONAL (LIST-OP FASL-OP-LIST))
  (BLOCK NIL
    (AND FASD-NEW-SYMBOL-FUNCTION			;For FASD-SYMBOLS-PROPERTIES,
	 (SYMBOLP S-EXP)				;make sure we examine all symbols in
	 (FUNCALL FASD-NEW-SYMBOL-FUNCTION S-EXP))	;the data that we dump.
    (LET ((TEM  (FASD-TABLE-LOOKUP S-EXP)))		;Check if this object already dumped
      (WHEN TEM						;Yup.
	(COND (( TEM (LSH 1 16.))
	       (FASD-START-GROUP NIL 2 FASL-OP-LARGE-INDEX)
	       (FASD-NIBBLE (LDB #o2010 TEM))
	       (FASD-NIBBLE (LDB #o0020 TEM)))
	      (T
	       (FASD-START-GROUP NIL 1 FASL-OP-INDEX)	;Just reference it in the FASL TABLE.
	       (FASD-NIBBLE TEM)))
	    (RETURN TEM)))
    (TYPECASE S-EXP
      (INTEGER (FASD-FIXED S-EXP))
      (CHARACTER (FASD-CHARACTER S-EXP))
      (SHORT-FLOAT (FASD-SMALL-FLOAT S-EXP))
      (SINGLE-FLOAT (FASD-SINGLE-FLOAT S-EXP))
      (SYMBOL (FASD-SYMBOL S-EXP))
      (STRING (RETURN (FASD-STRING S-EXP)))
      (ARRAY (RETURN (FASD-ARRAY S-EXP)))
      (COMPILED-FUNCTION (FASD-FEF S-EXP))
      (CONS (RETURN (FASD-LIST S-EXP LIST-OP)))
      (INSTANCE (FASD-EVAL-CONSTRUCT-CONSTANT
		  (OR (SEND S-EXP ':SEND-IF-HANDLES ':FASD-FORM)
		      `(APPLY 'MAKE-INSTANCE
			      '(,(TYPE-OF S-EXP)
				. ,(SEND S-EXP ':RECONSTRUCTION-INIT-PLIST))))))
      (RATIO (RETURN (FASD-RATIONAL S-EXP)))
      (COMPLEX (RETURN (FASD-COMPLEX S-EXP)))
      (T (FERROR NIL "~S is a ~S, which is not a valid data-type for FASD-CONSTANT"
		 S-EXP (TYPE-OF S-EXP))))
    (FASD-TABLE-ADD S-EXP)))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN SXHASH (X &OPTIONAL RANDOM-OBJECT-ACTION)
  "Return a hash code for object X.  EQUAL objects have the same hash code.
The hash code is always a positive fixnum.
Flavor instances and named structures may handle the :SXHASH operation
/(with one arg, passed along from RANDOM-OBJECT-ACTION) to compute their hash codes.
If RANDOM-OBJECT-ACTION is non-NIL, the ultimate default is to use the
object's address to compute a hash code.  This only happens for
objects which cannot be EQUAL unless they are EQ.
If RANDOM-OBJECT-ACTION is NIL, the hash code of an object does not
change even if it is printed out and read into a different system version."
  (COND ((SYMBOLP X) (%SXHASH-STRING (GET-PNAME X) #o337))
	((STRINGP X) (%SXHASH-STRING X #o337))	;Ignores case!
	((OR (INTEGERP X) (CHARACTERP X))
	 (IF (MINUSP X) (LOGXOR (LDB 23. X) 1) (LDB 23. X)))
	((CONSP X)		;Rotate car by 11. and cdr by 7, but do it efficiently
	 (DO ((ROT 4) (HASH 0) Y (X X))
	     ((ATOM X)
	      (OR (NULL X)
		  (SETQ HASH (LOGXOR (ROT-24-BIT (SXHASH X RANDOM-OBJECT-ACTION)
						 (- ROT 4))
				     HASH)))
	      (LOGAND #o37777777 (IF (LDB-TEST (BYTE 1 23.) HASH) (LOGXOR HASH 1) HASH)))
	   (SETQ Y (CAR X) X (CDR X))
	   (OR (< (SETQ ROT (+ ROT 7)) 24.) (SETQ ROT (- ROT 24.)))
	   (SETQ HASH (LOGXOR (ROT-24-BIT
				(COND ((SYMBOLP Y) (%SXHASH-STRING (GET-PNAME Y) #o337))
				      ((STRINGP Y) (%SXHASH-STRING Y #o337))
				      ((OR (INTEGERP Y) (CHARACTERP Y))
				       (LDB 24. Y))
				      (T (SXHASH Y RANDOM-OBJECT-ACTION)))
				ROT)
			      HASH))))
	((FLONUMP X) (LOGXOR (%P-LDB-OFFSET #o0027 X 1)
			     (%P-LDB-OFFSET #o2701 X 1)
			     (%P-LDB #o0022 X)))
	((AND (TYPEP X 'INSTANCE)
	      (SEND X :SEND-IF-HANDLES :SXHASH RANDOM-OBJECT-ACTION)))
	((AND (TYPEP X 'NAMED-STRUCTURE)
	      (MEMQ :SXHASH (NAMED-STRUCTURE-INVOKE :WHICH-OPERATIONS X)))
	      (NAMED-STRUCTURE-INVOKE :SXHASH X RANDOM-OBJECT-ACTION))
	((OR RANDOM-OBJECT-ACTION
	     (SMALL-FLOATP X))
	 (SETQ X (%POINTER X))
	 (LET ((Y (LOGXOR (LDB (- %%Q-POINTER 24.) X)
			  (LSH X (- 24. %%Q-POINTER)))))
	 (LOGAND #o37777777
		 (IF (MINUSP X) (LOGXOR Y 1) Y))))
	((ARRAYP X)
	 (ARRAY-ACTIVE-LENGTH X))
	(T 0)))					;0 for things that can't be read

))


; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-CLOSURE (CL)
  (LET ((BINDINGS (CLOSURE-BINDINGS CL))
	(SYM NIL) (OFFSET NIL))
    (FORMAT T "~%~S is a closure of ~S:~%" CL (CLOSURE-FUNCTION CL))
    (IF (NULL (CDDR BINDINGS))
	(LET ((*PRINT-CIRCLE* T))		;certain to be needed
	  (FORMAT T "Lexical environment: ~S" (CAR BINDINGS)))
      (DO ((BINDINGS BINDINGS (CDDR BINDINGS)))
	  ((NULL BINDINGS))
	(SETQ SYM (%FIND-STRUCTURE-HEADER (CAR BINDINGS))
	      OFFSET (%POINTER-DIFFERENCE (CAR BINDINGS) SYM))
	(FORMAT T
		"   ~A cell of ~S: ~40T~:[void~;~S~]~%"
		(SELECTQ OFFSET
		  (0 "Print name") (1 "Value") (2 "Function")
		  (3 "Property list") (4 "Package"))
		SYM
		(LOCATION-BOUNDP (CADR BINDINGS))
		(AND (LOCATION-BOUNDP (CADR BINDINGS))
		     (CAADR BINDINGS))))
      (DESCRIBE-1 (CLOSURE-FUNCTION CL)))))

))

; From file NUMDEF.LISP PS:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defconstant single-float-exponent-offset #o2000)
(defconstant single-float-mantissa-length 31.)
(defconstant single-float-exponent-length 11.)
(defconstant single-float-implicit-sign-bit-p nil)
;(defconstant short-float-exponent-offset #o100);will be #o200 in 99.
(defconstant short-float-mantissa-length 17.)
;(defconstant short-float-exponent-length 7)	;will be 6 in 99
(defconstant short-float-implicit-sign-bit-p nil)

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-STREAM (INPUT-STREAM GENERIC-PATHNAME FASD-FLAG PROCESS-FN
		       QC-FILE-LOAD-FLAG QC-FILE-IN-CORE-FLAG PACKAGE-SPEC
		       &OPTIONAL (FILE-LOCAL-DECLARATIONS NIL) IGNORE
		       COMPILING-WHOLE-FILE-P
		       &AUX (PACKAGE PACKAGE) (IBASE IBASE) (BASE BASE)
		       FILE-SPECIAL-LIST FILE-UNSPECIAL-LIST
		       FDEFINE-FILE-PATHNAME
		       (READ-FUNCTION (IF QC-FILE-CHECK-INDENTATION
					  'READ-CHECK-INDENTATION 'READ)))
  "This function does all the /"outer loop/" of the compiler, for file and editor compilation.
 to be compiled are read from INPUT-STREAM.
The caller is responsible for handling any file attributes.
GENERIC-PATHNAME is the file to record information for and use the attributes of.
 It may be NIL if compiling to core.
FASD-FLAG is NIL if not making a QFASL file.
PROCESS-FN is called on each form.
QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
FILE-LOCAL-DECLARATIONS is normally initialized to NIL,
but you can optionally pass in an initializations for it.
COMPILING-WHOLE-FILE-P should be T if you are processing all of the file."
  (FILE-OPERATION-WITH-WARNINGS (GENERIC-PATHNAME ':COMPILE COMPILING-WHOLE-FILE-P)
   (COMPILER-WARNINGS-CONTEXT-BIND
     ;; Override the package if required.  It has been bound in any case.
     (AND PACKAGE-SPEC (SETQ PACKAGE (PKG-FIND-PACKAGE PACKAGE-SPEC)))
     ;; Override the generic pathname
     (SETQ FDEFINE-FILE-PATHNAME
	   (LET ((PATHNAME (AND (MEMQ ':PATHNAME (SEND INPUT-STREAM ':WHICH-OPERATIONS))
				(SEND INPUT-STREAM ':PATHNAME))))
	     (AND PATHNAME (SEND PATHNAME ':GENERIC-PATHNAME))))
     ;; Having bound the variables, process the file.
     (LET ((QC-FILE-IN-PROGRESS T)
	   (UNDO-DECLARATIONS-FLAG (NOT QC-FILE-LOAD-FLAG))
	   (LOCAL-DECLARATIONS NIL)
	   (OPEN-CODE-MAP-SWITCH OPEN-CODE-MAP-SWITCH)
	   (RUN-IN-MACLISP-SWITCH RUN-IN-MACLISP-SWITCH)
	   (OBSOLETE-FUNCTION-WARNING-SWITCH OBSOLETE-FUNCTION-WARNING-SWITCH)
	   (ALL-SPECIAL-SWITCH ALL-SPECIAL-SWITCH)
	   (SOURCE-FILE-UNIQUE-ID)
	   (FASD-PACKAGE NIL))
       (COND (FASD-FLAG
	      ;; Copy all suitable file properties into the fasl file
	      ;; Suitable means those that are lambda-bound when you read in a file.
	      (LET ((PLIST (COPYLIST (SEND GENERIC-PATHNAME ':PLIST))))
		;; Remove unsuitable properties
		(DO ((L (LOCF PLIST)))
		    ((NULL (CDR L)))
		  (IF (NOT (NULL (GET (CADR L) 'FS:FILE-ATTRIBUTE-BINDINGS)))
		      (SETQ L (CDDR L))
		      (SETF (CDR L) (CDDDR L))))
		;; Make sure the package property is really the package compiled in
		;; Must load QFASL file into same package compiled in
		;; On the other hand, if we did not override it
		;; and the attribute list has a list for the package, write that list.
		(UNLESS (AND (NOT (ATOM (GETF PLIST ':PACKAGE)))
			     (STRING= (PACKAGE-NAME *PACKAGE*)
				      (CAR (GETF PLIST ':PACKAGE))))
		  (PUTPROP (LOCF PLIST)
			   (INTERN (PACKAGE-NAME *PACKAGE*) SI:PKG-KEYWORD-PACKAGE)
			   ':PACKAGE))
		(AND INPUT-STREAM
		     (MEMQ ':TRUENAME (SEND INPUT-STREAM ':WHICH-OPERATIONS))
		     (SETQ SOURCE-FILE-UNIQUE-ID (SEND INPUT-STREAM ':TRUENAME))
		     (PUTPROP (LOCF PLIST)
			      SOURCE-FILE-UNIQUE-ID
			      ':QFASL-SOURCE-FILE-UNIQUE-ID))
		;; If a file is being compiled across directories, remember where the
		;; source really came from.
		(AND FDEFINE-FILE-PATHNAME FASD-STREAM
		     (LET ((OUTFILE (AND (MEMQ ':PATHNAME
					       (SEND FASD-STREAM ':WHICH-OPERATIONS))
					 (SEND FASD-STREAM ':PATHNAME))))
		       (COND (OUTFILE
			      (SETQ OUTFILE (SEND OUTFILE ':GENERIC-PATHNAME))
			      (AND (NEQ OUTFILE FDEFINE-FILE-PATHNAME)
				   (PUTPROP (LOCF PLIST) FDEFINE-FILE-PATHNAME
					    ':SOURCE-FILE-GENERIC-PATHNAME))))))
		(MULTIPLE-VALUE-BIND (MAJOR MINOR)
		    (SI:GET-SYSTEM-VERSION "System")
		  (PUTPROP (LOCF PLIST)
			 `(,USER-ID
			   ,SI:LOCAL-PRETTY-HOST-NAME
			   ,(TIME:GET-UNIVERSAL-TIME)
			   ,MAJOR ,MINOR
			   (NEW-DESTINATIONS T    ;; NOT :new-destinations!!
			    :SITE ,(SHORT-SITE-NAME)))
			   ':COMPILE-DATA))
		;; First thing in QFASL file must be property list
		;; These properties wind up on the GENERIC-PATHNAME.
		(COND (QC-FILE-REL-FORMAT
		       (QFASL-REL#:DUMP-FILE-PROPERTY-LIST
			  GENERIC-PATHNAME
			  PLIST))
		      (T
		       (FASD-FILE-PROPERTY-LIST PLIST))))))
       (QC-PROCESS-INITIALIZE)
       (DO ((EOF (NCONS NIL))
	    (FORM))
	   (NIL)
	 ;; Detect EOF by peeking ahead, and also get an error now
	 ;; if the stream is wedged.  We really want to get an error
	 ;; in that case, not make a warning.
	 (LET ((CH (SEND INPUT-STREAM ':TYI)))
	   (OR CH (RETURN))
	   (SEND INPUT-STREAM ':UNTYI CH))
	 (setq si:premature-warnings
	       (append si:premature-warnings si:premature-warnings-this-object))
	 (let ((si:premature-warnings nil))
	   (SETQ FORM
		 (LET ((READ-AREA (IF QC-FILE-LOAD-FLAG DEFAULT-CONS-AREA
				    QCOMPILE-TEMPORARY-AREA))
		       (WARN-ON-ERRORS-STREAM INPUT-STREAM)
		       (QC-FILE-READ-IN-PROGRESS FASD-FLAG))	;looked at by XR-#,-MACRO
		   (WARN-ON-ERRORS ('READ-ERROR "Error in reading")
		     (FUNCALL READ-FUNCTION INPUT-STREAM EOF))))
	   (setq si:premature-warnings-this-object si:premature-warnings))
	 (AND (EQ FORM EOF) (RETURN))
	 ;; Start a new whack if FASD-TABLE is getting too big.
	 (AND FASD-FLAG
	      ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
	      (FASD-END-WHACK))
	 (WHEN (AND (ATOM FORM) FASD-FLAG)
	   (WARN 'ATOM-AT-TOP-LEVEL ':IMPLAUSIBLE
		 "The atom ~S appeared at top level; this would do nothing at FASLOAD time."
		 FORM))
	 (FUNCALL PROCESS-FN FORM))))))

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-PROCLAIM (DECL-LIST PROCESS-FN)
  (MAPC #'(LAMBDA (DECL &AUX X)
	    (CONDITION-CASE (ERROR)
		(SETQ X (SI::EVAL1 DECL))
	      (ERROR (WARN 'BAD-DECLARATION :IMPOSSIBLE
			   "Error evaluating argument ~S to PROCLAIM~&~A"
			   DECL (SEND ERROR :REPORT NIL)))
	      (:NO-ERROR
	       (IF (NSYMBOLP (CAR X))
		   (WARN 'BAD-DECLARATION :IMPOSSIBLE
			 "An argument of PROCLAIM evaluated to ~S, which is not a valid declaration"
			 DECL)
		 (LET ((S (SYMBOL-NAME (CAR X))))
		   (COND ((MEMQ (CAR X) '(SPECIAL UNSPECIAL))
			  (FUNCALL PROCESS-FN X 'SPECIAL))
			 ((EQUAL S "INLINE")
			  )
			 ((EQUAL S "NOTINLINE")
			  )
			 ((EQUAL S "DECLARATION")
			  (PUSHNEW `(,S IGNORE) SI:INTERPRETER-DECLARATION-TYPE-ALIST
				   :TEST 'EQUAL :KEY 'CAR))))))))
	DECL-LIST))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun setq (&quote &rest symbols-and-values)
  "Given alternating variables and value expressions, sets each variable to following value.
Each variable is set before the following variable's new value is evaluated.
See also PSETQ which computes all the new values and then sets all the variables."
  (prog (val)
     l	(cond ((null symbols-and-values) (return val))
	      ((null (cdr symbols-and-values))
	       (ferror nil "Odd number of arguments to SETQ"))
	      ;; checking for setqing defconstants would make life too hard for hacking
	      ((or (memq (car symbols-and-values) '(t nil))
		   (keywordp (car symbols-and-values)))
	       (ferror nil "Setting ~A is not allowed."
		       (if (keywordp (car symbols-and-values)) "keywords"
			 (car symbols-and-values)))))
	(if (eq interpreter-function-environment t)
	    (set (car symbols-and-values)
		 (setq val (eval1 (cadr symbols-and-values))))
	  (interpreter-set (car symbols-and-values)
			   (setq val (eval1 (cadr symbols-and-values)))))
	(setq symbols-and-values (cddr symbols-and-values))
	(go l)))

))

