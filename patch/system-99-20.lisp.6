;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.20
;;; Reason:
;;;  Flavors: Added specvar *METHOD-TYPES-NEEDING-FEF-EQUAL-CHECK* for extensibility
;;;           Changed FDEFINE clause of METHOD-FUNCTION-SPEC-HANDLER accordingly
;;;           Added REMPROP clause to  METHOD-FUNCTION-SPEC-HANDLER
;;;  Zwei:    Unrolled and extensibilized SYMBOL-FROM-STRING
;;;           Added DEF-DEFINER special form
;;;           Added new functions for SYMBOL-FROM-STRING: (:PROPERTY <type> SYMBOL-FROM-STRING)
;;; Written 10-Feb-85 23:25:34 by TIM,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.17, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.



; From file OZ:KANSAS:<L.SYS2>FLAVOR.LISP.282 at 10-Feb-85 23:25:35
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(defconst *method-types-needing-fef-equal-check* '(:WRAPPER :INVERSE-WRAPPER))

))

; From file OZ:KANSAS:<L.SYS2>FLAVOR.LISP.282 at 10-Feb-85 23:26:32
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2 &AUX FL)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(METHOD-TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC))
	(DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (IF (NULL (CDDDR FUNCTION-SPEC))
	(SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (COND ((NOT (AND (SYMBOLP FLAVOR)
		     (SYMBOLP METHOD-TYPE)
		     (SYMBOLP MESSAGE)
		     ( 3 (LENGTH FUNCTION-SPEC) 5)))
	   (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	     (FERROR 'SYS:INVALID-FUNCTION-SPEC
		     "The function spec ~S is invalid." FUNCTION-SPEC)))
	  ((EQ T (SETQ FL (COMPILATION-FLAVOR FLAVOR)))
	   ;; Silly pseudo-flavor for cold-load stream
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       T
	     ;;The property-list operations need to work for the editor
	     (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))
	  ((OR FL		;A defined flavor
	       (NOT (CLASS-SYMBOLP FLAVOR)))	;Not defined, assume flavor
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       T
	     ;; Ignore FASLOAD-COMBINED methods if flavor methods composed already.
	     (IF (AND FL (FLAVOR-METHOD-HASH-TABLE FL)
		      (EQ (THIRD FUNCTION-SPEC) 'FASLOAD-COMBINED))
		 ;; This hair makes defining (INTERNAL (:METHOD FOO FASLOAD-COMBINED ...) ...)
		 ;; get ignored properly and not get an error.
		 (SELECTQ FUNCTION
		   (FDEFINITION LAST-FASLOAD-COMBINED-METHOD)
		   (FDEFINEDP T)
		   (FDEFINE
		    (SETQ LAST-FASLOAD-COMBINED-METHOD ARG1))
		   (FDEFINITION-LOCATION (LOCF LAST-FASLOAD-COMBINED-METHOD))
		   (T NIL))
	       ;; Otherwise refer to or define the :COMBINED method.
	       (IF (EQ METHOD-TYPE 'FASLOAD-COMBINED)
		   (SETQ FUNCTION-SPEC (LIST* (FIRST FUNCTION-SPEC) FLAVOR
					      :COMBINED (CDDDR FUNCTION-SPEC))
			 METHOD-TYPE :COMBINED))
	       (LET ((METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC
			     (CASE FUNCTION
			       ((PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE)
				NIL)		;Create.
			       (OTHERWISE T)))))	;Don't create
		 (OR (AND METH (METH-DEFINEDP METH))
		     (MEMQ FUNCTION '(FDEFINEDP COMPILER-FDEFINEDP
				      PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE
				      GET FUNCTION-PARENT DWIMIFY))
		     (IF FL
			 (FERROR NIL "~S is not a defined method; it is not possible to ~S it"
			         FUNCTION-SPEC FUNCTION)
		         (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC)))
		 (SELECTQ FUNCTION
		   (FDEFINE
		     (OR FL
			 (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC))
		     (LET ((DEFINITION-NEW (NOT (METH-DEFINEDP METH)))
			   (OLD-DEFINITION (AND (METH-DEFINEDP METH) (METH-DEFINITION METH))))
		       (SETF (METH-DEFINITION METH) ARG1)
		       ;; If we load a method compiled before system 83,
		       ;; that expects instance variables to be bound,
		       ;; make it work by forcing this flavor to bind all variables.
		       (IF (AND (TYPEP ARG1 'COMPILED-FUNCTION)
				(ZEROP (%P-LDB %%FEFH-GET-SELF-MAPPING-TABLE ARG1))
				(NOT (ASSQ 'ENCAPSULATED-DEFINITION (DEBUGGING-INFO ARG1))))
			   (MAKE-FLAVOR-ALL-SPECIAL FL))
		       ;; Incrementally recompile the flavor if this is a new method, unless
		       ;; it is a :COMBINED method, which is the result of compilation,
		       ;; not a client of it.
		       (COND ((MEMQ METHOD-TYPE *method-types-needing-fef-equal-check*)
			      (OR (AND (CONSP OLD-DEFINITION)
				       (FEF-EQUAL (CDR ARG1) (CDR OLD-DEFINITION)))
				  ;; Wrapper is really changed; must recompile flavors.
				  ;; Arrange that if we abort, the definition is set
				  ;; to the symbol ABORTED-DEFINITION.  This is a no-op,
				  ;; and redefining or undefining the wrapper will recompile.
				  (LET (SUCCESS)
				    (UNWIND-PROTECT
				      (PROGN
					(RECOMPILE-FLAVOR FLAVOR MESSAGE NIL)
					(SETQ SUCCESS T))
				      (OR SUCCESS
					  (SETF (METH-DEFINITION METH)
						'ABORTED-DEFINITION))))))
			     ((EQ METHOD-TYPE :COMBINED) NIL)
			     (DEFINITION-NEW
			      ;; This SETF, by virtue of the preceding clause,
			      ;; arranges that if we abort out before finishing recompilation
			      ;; then the recompilation will be done again if the user
			      ;; either redoes the defmethod or does undefmethod.
			      (SETF (METH-DEFINITION METH) 'ABORTED-DEFINITION)
			      (RECOMPILE-FLAVOR FLAVOR MESSAGE)
			      (SETF (METH-DEFINITION METH) ARG1))
			     ;; If method defined as a random symbol,
			     ;; must fix up hash table each time it changes.
			     ((OR (SYMBOLP OLD-DEFINITION)
				  (SYMBOLP ARG1))
			      (RECOMPILE-FLAVOR FLAVOR MESSAGE)))))
		   (FDEFINITION (METH-DEFINITION METH))
		   (FDEFINEDP (AND METH (VALUES (METH-DEFINEDP METH)
						(AND (METH-DEFINEDP METH)
						     (METH-DEFINITION METH)))))
		   (FDEFINITION-LOCATION (LOCF (METH-DEFINITION METH)))
		   (FUNDEFINE
		    (SETF (METH-DEFINITION METH) 'UNDEFINITION-IN-PROGRESS)
		    (RECOMPILE-FLAVOR (FLAVOR-NAME FL) MESSAGE)	;Propagate the change
		    (NULLIFY-METHOD-DEFINITION METH))	;Say propagation is complete.
		   (COMPILER-FDEFINEDP METH)
		   (GET (AND METH (GETF (METH-PLIST METH) ARG1 ARG2)))
		   (PUTPROP (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			      (SETF (GETF (METH-PLIST METH) ARG2) ARG1)))
		   (remprop (remf (meth-plist meth) arg1))
		   (PUSH-PROPERTY
		    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
		      (PUSH ARG1 (GETF (METH-PLIST METH) ARG2))))
		   (DWIMIFY
		    (CATCH-CONTINUATION 'DWIMIFY-PACKAGE
			#'(LAMBDA (NEW-SPEC) NEW-SPEC)
			#'(LAMBDA () NIL)
		      (DOLIST (COMPONENT
				(OR (FLAVOR-DEPENDS-ON-ALL FL)
				    (COMPOSE-FLAVOR-COMBINATION FL NIL)))
			(LET ((FLAVOR (COMPILATION-FLAVOR COMPONENT))
			      (METHS))
			  (AND FLAVOR
			       (SETQ METHS
				     (CDDDR (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FLAVOR)))))
			  (DOLIST (METH METHS)
			    (AND (METH-DEFINEDP METH)
				 (DWIMIFY-PACKAGE-2 (METH-FUNCTION-SPEC METH)
						    ARG1 ARG2 T)))))))
		   (OTHERWISE
		    (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))))
	  (T
	   (CLASS-METHOD-FUNCTION-SPEC-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; From file OZ:KANSAS:<L.ZWEI>SECTIO.LISP.271 at 14-Feb-85 03:04:54
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "



;;; Derive function spec from defining form and/or function spec shorthand
;;; This all used to be inside SYMBOL-FROM-STRING

;;; Certain types of function specs have two ways to type them, with and without the leading
;;; type keyword.  Also certain types of functions and other definitions do not follow the
;;; standard form of (DEFxxx name options...).  What we do here is to recognize and
;;; standardize those cases.

(defstruct (def-pat (:type :list*) (:conc-name def-pat-)
		    (:alterant nil)
		    (:constructor make-def-pat
				  (def-name string length type sub-type)))
  def-name
  string
  length
  type
  sub-type)

(defvar *def-patterns* nil
  "Used by SYMBOL-FROM-STRING to record patterns of special defining forms.")

(defmacro def-definer (def-name type &optional sub-type)
  "Used to tell Zwei of the existence of a defining form.  
The TYPE is used to select an appropriate (:PROPERTY ... SYMBOL-FROM-STRING) function to
parse the the name into a function spec.  The optional SUB-TYPE is needed in cases where
one parsing function is shared by several defining forms.  For instance
/(:PROPERTY :ALWAYS-METHOD SYMBOL-FROM-STRING) is used for both DEFMETHOD and DEFWRAPPER,
but DEFWRAPPER needs an implicit :WRAPPER inserted into the function spec."
  `(progn 'compile
	  (setq *def-patterns*
		(delete-if #'(lambda (x) (eq ',def-name (car x))) *def-patterns*))
	  (let ((.string. (string-append "(" (string ',def-name))))
	    (push (make-def-pat ',def-name .string. (string-length .string.) ,type ,sub-type)
		  *def-patterns*))))

;;; (DEFMETHOD (FLAVOR ...) ... => (:METHOD FLAVOR ...)
;;; (DEFWRAPPER (FLAVOR :MESSAGE) ... => (:METHOD FLAVOR :WRAPPER :MESSAGE)

(def-definer defmethod :always-method)
(def-definer defwrapper :always-method :wrapper)

(defun (:property :ALWAYS-METHOD symbol-from-string) (spec str ignore meth-type ignore)
  (values (CONS :METHOD (if meth-type
			    (list* (first SPEC) meth-type (rest SPEC))
			  SPEC))
	  str))

;;; Some ambiguous cases, etc.

;;; :HANDLER doesn't appear in source files, but gets translated into an appropriate :METHOD
;;; here, by analyzing the combined method.

(defprop :MAYBE-METHOD maybe-method-symbol-from-string symbol-from-string)
(defprop :METHOD       maybe-method-symbol-from-string symbol-from-string)
(defprop :HANDLER      maybe-method-symbol-from-string symbol-from-string)

(defun maybe-method-symbol-from-string (spec str type ignore ok-to-ask
					&aux sym)
  (LET ((FLAVOR (CAR SPEC))
	(MESSAGE (IF (CDDR SPEC) (CADDR SPEC) (CADR SPEC)))
	FL)
    (COND ((SETQ FL (GET FLAVOR 'SI:FLAVOR)))
	  ;;>> Ugh
	  ((AND (VALIDATE-2-LONG-LIST SPEC) (CLASS-SYMBOLP FLAVOR))
	   (SETQ SYM (FUNCALL (SYMBOL-VALUE FLAVOR) ':METHOD-FOR (CADR SPEC))
		 FL T))
	  (OK-TO-ASK
	   (DOLIST (SYMBOL (PACKAGE-LOOKALIKE-SYMBOLS FLAVOR
						      NIL '(SI:FLAVOR)))
	     (IF (FQUERY () "Do you mean ~S? "
			 `(:METHOD ,SYMBOL . ,(CDR SPEC)))
		 (RETURN (SETQ FLAVOR SYMBOL
			       SPEC (CONS FLAVOR (CDR SPEC))
			       FL (GET FLAVOR 'SI:FLAVOR)))))))
    (or (COND ((SYMBOLP FL)				;T or NIL
	       (AND (EQ TYPE ':MAYBE-METHOD)
		    (VALIDATE-2-LONG-LIST SPEC)
		    (values (CONS ':PROPERTY SPEC)
			    str)))
	      ((FDEFINEDP `(:METHOD . ,SPEC))
	       (values `(:METHOD . ,SPEC)
		       str))
	      (OK-TO-ASK
	       (DOLIST (SYMBOL (OR (FIND-COMBINED-METHODS FLAVOR MESSAGE NIL)
				   (SI:FLAVOR-ALL-INHERITABLE-METHODS
				     FLAVOR MESSAGE)))
		 (IF (FQUERY '() "Do you mean ~S? " SYMBOL)
		     (return (values SYMBOL
				     str))))))
	       (values nil
		       str))))

;;; (DEFSTRUCT (NAME ... => NAME

(def-definer defstruct :defstruct)

(defun (:property :DEFSTRUCT symbol-from-string) (spec ignore ignore ignore ignore
						  &aux sym)
  (values (setq sym (car spec))
	  (symbol-name sym)))

;;; (DEFSELECT (FSPEC ...) ... => FSPEC

(def-definer defselect :defselect)

(defun (:property :DEFSELECT symbol-from-string) (spec ignore ignore ignore ignore
						  &aux sym)
  (SETQ SYM (CAR SPEC))
  (IF (SYMBOLP SYM)
      (values sym
	      (SYMBOL-NAME SYM))
    (MULTIPLE-VALUE-bind (SYM STR)
	(SYMBOL-FROM-STRING SYM)
      (values sym
	      str))))

;;; (DEFUN (INDICATOR SYMBOL) ... => (:PROPERTY INDICATOR SYMBOL)

(defun (:property :PROPERTY symbol-from-string) (spec str ignore ignore ignore)
  (if (VALIDATE-2-LONG-LIST SPEC)
      (values (CONS :property SPEC)
	      str)
    (values nil
	    str)))

;;; :INTERNAL doesn't appear in source files, but might be given as the argument to
;;; m-X Disassemble.  The code here just tries not to destory it.

(defun (:property :INTERNAL symbol-from-string) (spec ignore ignore ignore ignore)
  (values (CONS :internal SPEC)
	  (DEFINITION-NAME-AS-STRING NIL (CAR SPEC))))

;;; Obsolete randomness?

(defun (:property :INSTANCE-METHOD symbol-from-string) (spec str ignore ignore ignore)
  (if (BOUNDP (CAR SPEC))
      (values (FUNCALL (CLASS (SYMBOL-VALUE (CAR SPEC)))
		       :METHOD-FOR (CADR SPEC))
	      str)
    (values nil
	    str)))


(DEFUN SYMBOL-FROM-STRING (STR &OPTIONAL LINE OK-TO-ASK SYM
			   &AUX (EOF '(())) ERRORP)
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
The third is T if there was a problem
 in parsing the string (such as unbalanced parens).

You can pass the read-in form of the object as the fourth arg
if you already know it."
  (DECLARE (VALUES SYM STR ERRORP))
  (IF (ARRAYP STR)
      (UNLESS SYM
	(MULTIPLE-VALUE-SETQ (SYM ERRORP)
	  (CATCH-ERROR (CLI:READ-FROM-STRING STR T EOF) NIL)))
    (SETQ SYM STR
	  STR (FORMAT NIL "~S" STR)))
  (COND (ERRORP
	 (VALUES NIL NIL ERRORP))
	((SYMBOLP SYM)
	 (VALUES SYM (SYMBOL-NAME SYM)))
	((OR (ATOM SYM) (EQ SYM EOF))
	 (VALUES NIL NIL T))
	(:else
	 ;; At this point we have a list to parse.  The variables are:
	 ;;	TYPE - the type of function spec or non-function definition
	 ;;	SYM - the function spec or definition name
	 ;;	SPEC - the variant of SYM which appears in the source code
	 ;;	STR - SPEC converted to a string
	 ;;     S-FROM-S-FN - a function which will return SYM and STR given
	 ;;                   these variables, SUB-TYPE, and OK-TO-ASK
	 (LET ((TYPE (CAR SYM))
	       (sub-type nil)
	       SPEC s-from-s-fn)
	   (IF (GET TYPE 'SI::FUNCTION-SPEC-HANDLER)
	       (SETQ SPEC (CDR SYM)
		     STR (DEFINITION-NAME-AS-STRING TYPE SPEC))
	     (SETQ SPEC SYM)
	     (let ((DELIM-IDX (AND LINE (STRING-SEARCH-SET "( " LINE 1))))
	       (setq type (COND ((NULL LINE)
				 :MAYBE-METHOD)
				((dolist (def-pat *def-patterns*)
				   (let ((def-pat-length (def-pat-length def-pat)))
				     (when (AND (= DELIM-IDX def-pat-length)
						(%STRING-EQUAL LINE 0
							       (def-pat-string def-pat)
							       0 def-pat-length))
				       (setq sub-type (def-pat-sub-type def-pat))
				       (return (def-pat-type def-pat))))))
				(T :PROPERTY)))))
	   (OR (and (setq s-from-s-fn (get type 'symbol-from-string))
		    ;; This function is required to pass STR through untouched if it loses
		    (multiple-value (sym str)
		      (funcall s-from-s-fn spec str type sub-type ok-to-ask)))
	       ;; Something we don't understand, make a bogus symbol to use as a property
	       ;; list to remember the location of this definition
	       (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))))
	 (IF (NOT (SYS:VALIDATE-FUNCTION-SPEC SYM))
	     (VALUES NIL NIL T)
	     (VALUES SYM STR)))))

))
