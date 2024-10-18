;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.15
;;; Reason:
;;;  Errors from reading a symbol or definition name (e.g. in Edit Definition)
;;;  now print the reported error string, instead of merely "Read error."
;;;  
;;;  ZWEI's READ-FUNCTION-NAME prints the error if it receives one from
;;;  SYMBOL-FROM-STRING, which now returns an error object as its fourth
;;;  value (if applicable).
;;; Written 10-Aug-88 21:37:55 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with Experimental System 126.22, ZWEI 125.14, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, 8/9/88.



; From modified file DJ: L.ZWEI; MOUSE.LISP#107 at 10-Aug-88 21:38:30
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFUN READ-FUNCTION-NAME (PROMPT &OPTIONAL DEFAULT MUST-BE-DEFINED STRINGP
				  &AUX EXPLICIT-PACKAGE-P
				  (*MINI-BUFFER-DEFAULT-STRING* DEFAULT)
				  (READ-FUNCTION-NAME-MUST-BE-DEFINED MUST-BE-DEFINED)
				  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
				    *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*)
				  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
				    *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
				    *MOUSE-FONT-CHAR*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
				    *MOUSE-X-OFFSET*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET
				    *MOUSE-Y-OFFSET*))
  "Read a function name using mini buffer or mouse.
PROMPT is a string that goes in the mode line.
DEFAULT is a function spec to return if the user types just Return.
MUST-BE-DEFINED can be T (allow only defined functions), NIL (allow anything)
 or AARRAY-OK (allow anything either defined as a function
 or known as a section by the editor).
STRINGP can be T, NIL, ALWAYS-READ or MULTIPLE-OK.
 T means if user types text, just return a string; don't try to intern it.
 ALWAYS-READ means intern the user's string afresh now;
  don't use the symbol or list recorded in the completion aarray.
 MULTIPLE-OK means it is ok to return more than one possible function
  the user could have meant, if they differ only in their package.

The first value is a list of function specs (only one, unless STRINGP is MULTIPLE-OK).
 If STRINGP is T, this is NIL.
The second value is the string the user typed, sans package prefix.
The third value is T if the user typed a package prefix."
  (DECLARE (VALUES COMPLETIONS STRING EXPLICIT-PACKAGE-P))
  (DECLARE (SPECIAL READ-FUNCTION-NAME-MUST-BE-DEFINED
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
		    READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
		    READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
		    READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET))
  (AND (EQ MUST-BE-DEFINED T) (SETQ STRINGP 'ALWAYS-READ))
  (SETQ PROMPT (FORMAT NIL "~A~:[:~; (Default: ~S)~]" PROMPT DEFAULT DEFAULT))
  (LET ((NAME
	  (LET ((*POST-COMMAND-HOOK*
		  (APPEND *POST-COMMAND-HOOK* '(READ-FUNCTION-NAME-COMMAND-HOOK)))
		(*MINI-BUFFER-VALUE-HISTORY*
		  *DEFINITION-NAME-HISTORY*))
	    (LET ((*BATCH-UNDO-SAVE* T))
	      (DELETE-INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
	    (UNWIND-PROTECT
		(PROGN (READ-FUNCTION-NAME-COMMAND-HOOK NIL)
		       (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *ZMACS-COMPLETION-AARRAY*
							 (OR (NEQ STRINGP 'ALWAYS-READ)
							     'ALWAYS-STRING)))
	      (READ-FUNCTION-NAME-COMMAND-HOOK T))))
	SYM ERROR-P) 
    (COND ((EQUAL NAME "")
	   (OR DEFAULT (BARF))
	   (SETQ SYM DEFAULT
		 NAME (IF (SYMBOLP NAME) (STRING NAME)
			(FORMAT:OUTPUT NIL (PRINC DEFAULT)))))
	  ((CONSP NAME)
	   (SETQ SYM (CDR NAME)
		 NAME (CAR NAME))
	   (AND (CONSP SYM) (NEQ STRINGP 'MULTIPLE-OK)
		(SETQ SYM (CAR SYM))))
	  ((EQ STRINGP T)			;If returning a string, don't intern it
	   (SETQ SYM NAME))
	  (T
	   ;; If the string that was specified started with a package prefix,
	   ;; return a flag saying so.
	   ;; SYMBOL-FROM-STRING will flush the prefix from NAME.
	   (LET ((NON-LETTER-INDEX
		   (STRING-SEARCH-NOT-SET " ABCDEFGHIJKLMNOPQRSTUVWXYZ-" NAME)))
	     (AND NON-LETTER-INDEX (= (AREF NAME NON-LETTER-INDEX) #/:)
		  (SETQ EXPLICIT-PACKAGE-P T)))
	   (let (err)
	     (MULTIPLE-VALUE (SYM NAME ERROR-P err)
	       (SYMBOL-FROM-STRING NAME NIL T))
	     (AND (CONSP SYM) (EQ STRINGP 'MULTIPLE-OK)
		  (SETQ SYM (NCONS SYM)))
	     (when ERROR-P
	       (BARF (format nil "Read error~@[: ~A~]"
			     (if (errorp err) (send err :report-string))))))))
    (AND (EQ MUST-BE-DEFINED T)
	 (NOT (OR (FDEFINEDP SYM)
		  (AND (SYMBOLP SYM)
		       (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST SYM)))))
	 (OR (AND (SYMBOLP SYM)
		  (DOLIST (SPEC (PACKAGE-LOOKALIKE-SYMBOLS SYM))
		    (AND (FQUERY '(:SELECT T)
				 ;; Always print prefix
				 ;; Don't leave PACKAGE in keyword during query.
				 (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
				   (FORMAT NIL "Do you mean ~S? " SPEC)))
			 (RETURN (SETQ SYM SPEC)))))
	     (BARF "~S is not defined" SYM)))
    (PUSH-ON-HISTORY SYM *DEFINITION-NAME-HISTORY*)
    (VALUES SYM NAME EXPLICIT-PACKAGE-P)))

))

; From modified file DJ: L.ZWEI; SECTIO.LISP#316 at 10-Aug-88 21:38:36
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN SYMBOL-FROM-STRING (STR &OPTIONAL LINE OK-TO-ASK SYM &AUX ERROR-P error)
  "Given a string STR as found after DEF..., return the name of the object being defined.
LINE is the line that the string was found in.  It is used for
finding the particular defining construct used; this affects the result
since (DEFUN (FOO BAR) defines (:PROPERTY FOO BAR)
while (DEFMETHOD (FOO BAR) defines (:METHOD FOO BAR).
OK-TO-ASK means in certain circumstances
where things are not clear, ask the user.  Otherwise we guess.

The arg can also be an object; then its printed representation is used as the string.

The second value is a canonicalized string for the object
 (maybe the same string specified, maybe not).
The third value is T if there was a problem
 in parsing the string (such as unbalanced parens).
 In this case the fourth value may be an error object,
 in which case it is the error object indicating the read parsing problem.

You can pass the read-in form of the object as the fourth arg
if you already know it."
  (DECLARE (VALUES SYM STR ERROR-P error))
  (IF (ARRAYP STR)
      (UNLESS SYM
	(CONDITION-CASE (err)
			(SETQ SYM (CL:READ-FROM-STRING STR))
	  (SYS:PARSE-ERROR (SETQ ERROR-P T)
			   (setq error err))))
    (SETQ SYM STR
	  STR (FORMAT NIL "~S" STR)))
  (COND (ERROR-P
	 (VALUES NIL NIL ERROR-P error))
	((SYMBOLP SYM)
	 (VALUES SYM (SYMBOL-NAME SYM)))
	((ATOM SYM)
 	 (VALUES NIL NIL T))
	(T
	 ;; Here SYM is a list.  Certain types of function specs have two ways to
	 ;; type them, with and without the leading type keyword.  Also certain types
	 ;; of functions and other definitions do not follow the standard form
	 ;; of (DEFxxx name options...).  What we do here is to recognize and
	 ;; standardize those cases.  The variables are:
	 ;;	TYPE - the type of function spec or non-function definition
	 ;;	SYM - the function spec or definition name
	 ;;	SPEC - the variant of SYM which appears in the source code
	 ;;	STR - SPEC converted to a string
	 ;; :HANDLER doesn't appear in source files, but gets translated into
	 ;; an appropriate :METHOD here, by analyzing the combined method.
	 ;; :INTERNAL doesn't appear in source files, but might be given as the argument
	 ;; to M-X Disassemble.  The code here just tries not to destroy it.
	 (LET ((TYPE (CAR SYM))
	       DELIM-IDX SPEC)
	   (IF (GET TYPE 'SI:FUNCTION-SPEC-HANDLER)
	       (SETQ SPEC (CDR SYM)
		     STR (DEFINITION-NAME-AS-STRING TYPE SPEC))
	     (SETQ SPEC SYM
		   DELIM-IDX (AND LINE (STRING-SEARCH-SET "( " LINE 1))
		   TYPE (let ((start-idx (definition-function-start line)))
			  (COND ((NULL LINE)
				 :MAYBE-METHOD)
				((%STRING-EQUAL LINE start-idx "DEFMETHOD" 0 9.)
				 :ALWAYS-METHOD)
				((%STRING-EQUAL LINE start-idx "DEFWRAPPER" 0 10.)
				 (SETQ SPEC (LIST (CAR SPEC) :WRAPPER (SECOND SPEC)))
				 :ALWAYS-METHOD)
				((%STRING-EQUAL LINE start-idx "DEFSTRUCT" 0 9.)
				 :DEFSTRUCT)
				((%STRING-EQUAL LINE start-idx "DEFSELECT" 0 9.)
				 :DEFSELECT)
				(T :PROPERTY)))))
	   (OR (SELECTQ TYPE
		 (:INSTANCE-METHOD
		  (AND (BOUNDP (CAR SPEC))
		       (SETQ SYM (FUNCALL (SI:CLASS (SYMBOL-VALUE (CAR SPEC)))
					  :METHOD-FOR (CADR SPEC)))))
		 (:ALWAYS-METHOD
		  (SETQ SYM (CONS ':METHOD SPEC)))
		 ((:METHOD :HANDLER :MAYBE-METHOD)
		  (LET ((FLAVOR (CAR SPEC))
			(MESSAGE (IF (CDDR SPEC) (CADDR SPEC) (CADR SPEC)))
			FL)
		    (COND ((SETQ FL (GET FLAVOR 'SI:FLAVOR)))
			  ((AND (VALIDATE-2-LONG-LIST SPEC) (SI:CLASS-SYMBOLP FLAVOR))
			   (SETQ SYM (FUNCALL (SYMBOL-VALUE FLAVOR) :METHOD-FOR (CADR SPEC))
				 FL T))
			  (OK-TO-ASK
			   (DOLIST (SYMBOL (PACKAGE-LOOKALIKE-SYMBOLS FLAVOR
					      NIL '(SI:FLAVOR)))
			     (IF (FQUERY () "Do you mean ~S? "
					 `(:METHOD ,SYMBOL . ,(CDR SPEC)))
				 (RETURN (SETQ FLAVOR SYMBOL
					       SPEC (CONS FLAVOR (CDR SPEC))
					       FL (GET FLAVOR 'SI:FLAVOR)))))))
		    (COND ((SYMBOLP FL)		;T or NIL
			   (AND (EQ TYPE ':MAYBE-METHOD)
				(VALIDATE-2-LONG-LIST SPEC)
				(SETQ SYM (CONS ':PROPERTY SPEC))))
			  ((FDEFINEDP `(:METHOD . ,SPEC))
			   (SETQ SYM `(:METHOD . ,SPEC)))
			  (OK-TO-ASK
			   (DOLIST (SYMBOL (OR (FIND-COMBINED-METHODS FLAVOR MESSAGE NIL)
					       (SI:FLAVOR-ALL-INHERITABLE-METHODS
						 FLAVOR MESSAGE)))
			     (IF (FQUERY () "Do you mean ~S? " SYMBOL)
				 (RETURN (SETQ SYM SYMBOL))))))))
		 ((:DEFSTRUCT :SPECIAL-FORM)
		  (SETQ SYM (CAR SPEC)
			STR (GET-PNAME SYM)))
		 (:DEFSELECT
		  (SETQ SYM (CAR SPEC))
		  (IF (SYMBOLP SYM)
		      (SETQ STR (GET-PNAME SYM))
		      (MULTIPLE-VALUE-SETQ (SYM STR)
			(SYMBOL-FROM-STRING SYM))))
		 (:PROPERTY
		  (AND (VALIDATE-2-LONG-LIST SPEC)
		       (SETQ SYM (CONS TYPE SPEC))))
		 (:INTERNAL (SETQ SYM (CONS TYPE SPEC))
			    (SETQ STR (DEFINITION-NAME-AS-STRING NIL (CAR SPEC)))))
	       ;; Something we don't understand, make a bogus symbol to use as a property
	       ;; list to remember the location of this definition
	       (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))))
	 (IF (NOT (SYS:VALIDATE-FUNCTION-SPEC SYM))
	     (VALUES NIL NIL T)
	     (VALUES SYM STR)))))

))
