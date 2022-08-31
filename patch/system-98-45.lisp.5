;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 4/04/84 02:12:54 by RMS,
;;; Reason: Provide SELF when evalling default instance variable values.
;;; Define :SET methods for settable instance variables.
;;; Rename implicit args of methods to .OPERATION. and .SUBOPERATION.
;;; SUPDUP handles %TDCR, %TDLF, %TDBS.
;;; Lisp mode sectionization bug on stray parens.
;;; while running on Lisp Machine Eighteen from band 6
;;; with System 98.38, CADR 3.6, ZMail 53.13, MIT-Specific 22.0, microcode 309, ZM MIT.



; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN INSTANTIATE-FLAVOR (FLAVOR-NAME INIT-PLIST
		           &OPTIONAL SEND-INIT-MESSAGE-P
				     RETURN-UNHANDLED-KEYWORDS-P ;as second value
				     AREA-TO-CONS-INSTANCE-IN
			   &AUX FL UNHANDLED-KEYWORDS INSTANCE VARS
				   NEW-PLIST)
  "Create and return an instance of the specified FLAVOR-NAME, low level.
INIT-PLIST's CDR is the list of init keywords and their values.
This list will be modified destructively so that any default init plist
keywords (except those that just set instance variables) are on it.
We send a :INIT message only if SEND-INIT-MESSAGE-P is non-nil.
That may further modify the INIT-PLIST.

If RETURN-UNHANDLED-KEYWORDS-P is non-nil, our second value is an
alternating list of keywords and values for those keywords specified in
INIT-PLIST (or in the default init plist) which the flavor doesn't handle.
If RETURN-UNHANDLED-KEYWORDS-P is nil, it is an error if there are any such."
  ;; Trace any chain of alias flavors to a non-alias flavor.
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))
	     "the name of an instantiable flavor, or alias thereof")
  (LET ((TEM (FLAVOR-GET FL ':INSTANTIATION-FLAVOR-FUNCTION)))
    (WHEN TEM
      (SETQ TEM (FUNCALL TEM FL INIT-PLIST))
      (UNLESS (AND (SYMBOLP TEM)
		   (GET TEM 'FLAVOR))
	(FERROR NIL "The INSTANTIATION-FLAVOR-FUNCTION for flavor ~S
returned an invalid value, ~S, not a flavor name." FLAVOR-NAME))
      (SETQ FLAVOR-NAME TEM
	    FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))))
  (WHEN (FLAVOR-GET FL ':ABSTRACT-FLAVOR)
    (FERROR NIL "~S is an abstract flavor (or alias of one) and may not be instantiated."
	    FLAVOR-NAME))
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-TABLE FL) (COMPOSE-METHOD-COMBINATION FL))
  (UNLESS AREA-TO-CONS-INSTANCE-IN
    (SETQ AREA-TO-CONS-INSTANCE-IN
	  (FUNCALL (OR (FLAVOR-GET FL 'INSTANCE-AREA-FUNCTION) 'IGNORE)
		   INIT-PLIST)))
  (LET ((MISSING-KEYWORDS
	  (SUBSET-NOT #'(LAMBDA (KEYWORD) (GET-LOCATION-OR-NIL INIT-PLIST KEYWORD))
		      (FLAVOR-GET FL 'REQUIRED-INIT-KEYWORDS))))
    (WHEN MISSING-KEYWORDS
      (FERROR NIL "Flavor ~S requires init keywords ~S that are missing."
	      FLAVOR-NAME MISSING-KEYWORDS)))
  ;; Make the instance object, then fill in its various fields
  (SETQ INSTANCE (%ALLOCATE-AND-INITIALIZE DTP-INSTANCE DTP-INSTANCE-HEADER
			   FL NIL AREA-TO-CONS-INSTANCE-IN (FLAVOR-INSTANCE-SIZE FL)))
  (SETQ VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
  ;; Default all instance variables to unbound
  (DO ((V VARS (CDR V))
       (I 1 (1+ I)))
      ((NULL V))
    (%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE INSTANCE I)
			      DTP-NULL (CAR V)))
  (SETQ UNHANDLED-KEYWORDS (FLAVOR-UNHANDLED-INIT-KEYWORDS FL))
  (LET ((VAR-KEYWORDS (FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES FL))
	(REMAINING-KEYWORDS (FLAVOR-REMAINING-INIT-KEYWORDS FL)))
    ;; First, process any user-specified init keywords that
    ;; set instance variables.  When we process the defaults,
    ;; we will see that these are already set, and will
    ;; refrain from evaluating the default forms.
    ;; At the same time, we record any init keywords that this flavor doesn't handle.
    (DO ((PL (CDR INIT-PLIST) (CDDR PL))) ((NULL PL))
      (LET ((INDEX (FIND-POSITION-IN-LIST (CAR PL) VAR-KEYWORDS)))
	(COND (INDEX
	       (OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ INDEX)))
		   (SETF (%INSTANCE-REF INSTANCE (1+ INDEX)) (CADR PL))))
	      ((NOT (MEMQ (CAR PL) REMAINING-KEYWORDS))
	       (PUSHNEW (CAR PL) UNHANDLED-KEYWORDS)))))
    ;; Now do all the default initializations, of one sort or other,
    ;; that have not been overridden.
    (LET ((SELF INSTANCE))
      (DOLIST (D (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL))
	(OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ (CAR D))))
	    (SETF (%INSTANCE-REF INSTANCE (1+ (CAR D))) (EVAL (CADR D)))))
      ;; Now stick any default init plist items that aren't handled by that
      ;; onto the actual init plist.
      (DO ((PL (FLAVOR-REMAINING-DEFAULT-PLIST FL) (CDDR PL))) ((NULL PL))
	(OR (MEMQ-ALTERNATED (CAR PL) (CDR INIT-PLIST))
	    (PROGN
	      (UNLESS (EQ INIT-PLIST (LOCF NEW-PLIST))
		(SETQ NEW-PLIST (CDR INIT-PLIST)
		      INIT-PLIST (LOCF NEW-PLIST)))
	      (SETQ NEW-PLIST (LIST* (CAR PL) (EVAL (CADR PL)) NEW-PLIST)))))))
  ;; Complain if any keywords weren't handled, unless our caller
  ;; said it wanted to take care of this.
  (AND (NOT RETURN-UNHANDLED-KEYWORDS-P)
       UNHANDLED-KEYWORDS
       (NOT (GET INIT-PLIST ':ALLOW-OTHER-KEYS))
       (FERROR NIL "Flavor ~S does not handle the init keyword~P ~{~S~^, ~}"
	       FLAVOR-NAME
	       (LENGTH UNHANDLED-KEYWORDS)
	       UNHANDLED-KEYWORDS))
  (IF SEND-INIT-MESSAGE-P
      (SEND INSTANCE ':INIT INIT-PLIST))
  (VALUES INSTANCE UNHANDLED-KEYWORDS))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN COMPOSE-AUTOMATIC-METHODS (FL)
  ;; Avoid lossage on PROPERTY-LIST-MIXIN while reading this file into the cold load.
  (WHEN (FBOUNDP 'COMPILE-AT-APPROPRIATE-TIME)
    (DOLIST (V (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL))
      (LET* ((VV (CORRESPONDING-KEYWORD V))
	     (METH `(:METHOD ,(FLAVOR-NAME FL) ,VV)))
	(IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
		*JUST-COMPILING*)
	    (COMPILE-AT-APPROPRIATE-TIME
	      FL METH
	      `(NAMED-LAMBDA (,METH) (IGNORE)
		 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
			  (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
		 ,V))
	  (RECORD-SOURCE-FILE-NAME METH))))
    (DOLIST (V (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL))
      (LET* ((SV (INTERN1 (FORMAT NIL "SET-~A" V) PKG-KEYWORD-PACKAGE))
	     (METH `(:METHOD ,(FLAVOR-NAME FL) ,SV)))
	(IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
		*JUST-COMPILING*)
	    (COMPILE-AT-APPROPRIATE-TIME
	      FL METH
	      `(NAMED-LAMBDA (,METH) (IGNORE .NEWVALUE.)
		 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
			  (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
		 (SETQ ,V .NEWVALUE.)))
	  (RECORD-SOURCE-FILE-NAME METH)))
      (LET* ((VV (CORRESPONDING-KEYWORD V))
	     (METH `(:METHOD ,(FLAVOR-NAME FL) :CASE :SET ,VV)))
	(IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
		*JUST-COMPILING*)
	    (COMPILE-AT-APPROPRIATE-TIME
	      FL METH
	      `(NAMED-LAMBDA (,METH) (IGNORE IGNORE .NEWVALUE.)
		 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
			  (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
		 (SETQ ,V .NEWVALUE.)))
	  (RECORD-SOURCE-FILE-NAME METH))))))

;INTERN but always return-storage the print-name argument

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFFLAVOR VANILLA-FLAVOR () ()
  :NO-VANILLA-FLAVOR  ;No instance variables, no other flavors
  (:METHOD-COMBINATION (:CASE :BASE-FLAVOR-LAST :SET))
  (:DOCUMENTATION :MIXIN "The default base flavor.
This flavor provides the normal handlers for the :PRINT, :DESCRIBE, and :WHICH-OPERATIONS
operations.  Only esoteric hacks should give the :NO-VANILLA-FLAVOR option to DEFFLAVOR to
prevent this inclusion."))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method send (function arg1 &rest args)
  (let ((tempvars (list* (gensym) (mapcar #'(lambda (ignore) (gensym)) args)))
	(storevar (gensym)))
    (values tempvars (cons function args) (list storevar)
	    `(send ,(car tempvars) ':set ,arg1 ,@(cdr tempvars)
		   ,storevar)
	    `(send ,(car tempvars) ,arg1 . ,(cdr tempvars)))))

))

; From file SUPDUP.LISP PS:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "


(DEFUN SUPDUP-TDBS (SHEET)
  (SEND SHEET ':BACKWARD-CHAR))

(DEFUN SUPDUP-TDCR (SHEET)
  (TV:SHEET-SET-CURSORPOS SHEET 0 (NTH-VALUE 1 (TV:SHEET-READ-CURSORPOS SHEET))))

(DEFUN SUPDUP-TDLF (SHEET)
  (DECLARE (:SELF-FLAVOR TV:WINDOW))
  (SEND SHEET ':INCREMENT-CURSORPOS 0 TV:LINE-HEIGHT))

))

; From file SUPDUP.LISP PS:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

(FILLARRAY SUPDUP-%TD-DISPATCH
   '(SUPDUP-TDMOV SUPDUP-TDMV0 TV:SHEET-CLEAR-EOF TV:SHEET-CLEAR-EOL TV:SHEET-CLEAR-CHAR
;;;  %TDMOV       %TDMV0       %TDEOF		  %TDEOL	     %TDDLF

     SUPDUP-NOTHING SUPDUP-GT40 TV:SHEET-CRLF SUPDUP-NOTHING SUPDUP-TDBS SUPDUP-TDLF
;;;  %TDMTF	    %TDMTN      %TDCRL	      %TDNOP         %TDBS          %TDLF

     SUPDUP-TDCR SUPDUP-TDORS SUPDUP-TDQOT TV:SHEET-SPACE SUPDUP-TDMV0 SUPDUP-CLEAR
;;;  %TDCR	    %TDORS       %TDQOT       %TDFS    %TDMV0       %TDCLR

     SUPDUP-BEEP    SUPDUP-NOTHING SUPDUP-INSERT-LINE SUPDUP-DELETE-LINE
;;;  %TDBEL	    %TDINI	   %TDILP	      %TDDLP

     SUPDUP-INSERT-CHAR SUPDUP-DELETE-CHAR SUPDUP-TDBOW SUPDUP-RESET SUPDUP-GRAPHICS 
;;;  %TDICP	    	%TDDCP		   %TDBOW	  %TDRST	 %TDGRF
     SUPDUP-REGION-UP SUPDUP-REGION-DOWN
;;;  %TDRSU		%TDRSD

;;; PTV compatibility hacks (ARDS, etc.)
     SUPDUP-NOTHING SUPDUP-ARDS-SET
;;;  %TDGXT         %TDLNG

     SUPDUP-ARDS-LONG   SUPDUP-ARDS-SHORT
;;;  %TDLV              %TDSV
     ))

))

; From file SECTIO.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN (:LISP GET-SECTION-NAME) (LINE BP &AUX STR SYM ERROR-P
				 IDX END-IDX (EOF "") NON-FONT-LINE)
  (IF (NOT (AND (> (LENGTH LINE) 1) (= (LDB %%CH-CHAR (AREF LINE 0)) #/()))
      (VALUES NIL NIL T)
    (SETQ ERROR-P T)
    (WHEN (AND (%STRING-EQUAL LINE 0 "(DEF" 0 4)
	       (NOT (%STRING-EQUAL LINE 0 "(DEFPROP " 0 9))
	       (SETQ IDX (STRING-SEARCH-SET *WHITESPACE-CHARS* LINE))
	       (SETQ IDX (STRING-SEARCH-NOT-SET *WHITESPACE-CHARS* LINE IDX)))
      (SETQ ERROR-P NIL)
      (SETQ NON-FONT-LINE (STRING-REMOVE-FONTS LINE))
      (CONDITION-CASE ()
	  (SETF (VALUES SYM END-IDX)
		(READ-FROM-STRING NON-FONT-LINE EOF IDX))
	(:NO-ERROR
	 (IF (EQ SYM EOF)
	     (SETQ ERROR-P T)
	   (SETQ STR (SUBSTRING NON-FONT-LINE IDX (MIN (LENGTH LINE) END-IDX)))))
	(SYS:READ-ERROR
	 (SETQ STR (GET-DEFUN-NAME (MOVE-BP BP LINE 0)))))
      (UNLESS ERROR-P
	(MULTIPLE-VALUE (SYM NIL ERROR-P)
	  (SYMBOL-FROM-STRING STR NON-FONT-LINE NIL SYM))))
    (WHEN ERROR-P
      (SETQ SYM (CONCATENATE 'STRING
			     (LET ((BUFFER (NODE-TOP-LEVEL-NODE (LINE-NODE LINE))))
			       (IF (BUFFER-PATHNAME BUFFER)
				   (LET ((NAME
					   (PATHNAME-NAME (BUFFER-PATHNAME BUFFER))))
				     (IF (CONSP NAME)
					 (APPLY 'STRING-APPEND
						(MAPCAR #'(LAMBDA (NAME-ELT)
							    (IF (CONSP NAME-ELT)
								(CAR NAME-ELT) NAME-ELT))
							NAME))
				       (STRING NAME)))
				 (BUFFER-NAME BUFFER)))
			     "-"
			     (LET ((START-INDEX (STRING-SEARCH-NOT-CHAR #/( LINE)))
			       (SUBSTRING LINE START-INDEX
					  (AND START-INDEX
					       (STRING-SEARCH-SET *WHITESPACE-CHARS*
								  LINE START-INDEX))))
			     "-"
			     (PRIN1-TO-STRING (INCF *SECTION-COUNT*)))
	    STR SYM))
    (VALUES SYM STR NIL)))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


(DEFPROP .OPERATION. T COMPILER:IGNORABLE-VARIABLE)

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFPROP .SUBOPERATION. T COMPILER:IGNORABLE-VARIABLE)

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


(DEFUN METHOD-ARGUMENT-LIST (SPECIFIED-LAMBDA-LIST FUNCTION-SPEC)
  "Given an arglist specified in DEFMETHOD, return an arglist for the actual method.
This involves adding OPERATION to the front, and sometimes other things
depending on the method type"
  (CONS '.OPERATION.
	(APPEND (IF (CDDDR FUNCTION-SPEC)
		    (GET (CADDR FUNCTION-SPEC) 'IMPLICIT-METHOD-ARGUMENTS))
		SPECIFIED-LAMBDA-LIST)))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


(DEFPROP :CASE (.SUBOPERATION.) IMPLICIT-METHOD-ARGUMENTS)

;;; This lets you specify code to be wrapped around the invocation of the
;;; various methods for an operation.  For example,
;;; (DEFWRAPPER (FOO-FLAVOR :OPERATION) ((ARG1 ARG2) . BODY)
;;;   `(WITH-FOO-LOCKED (SELF)
;;;      (PRE-FROBULATE SELF ARG1 ARG2)
;;;      ,@BODY
;;;      (POST-FROBULATE SELF ARG2 ARG1)))
;;; Note that the wrapper needs to be defined at both compile and run times
;;; so that compiling combined methods as part of the qfasl file works.

))
