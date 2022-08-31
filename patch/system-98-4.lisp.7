;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11/30/83 17:07:55 by RMS,
;;; Reason: Flavors: :INSTANTIATION-FLAVOR-FUNCTION.  :RUN-TIME-ALTERNATIVES.
;;;  UNDEFFLAVOR bug.  Pass back any number of values from primary method.
;;;  Bug in :LIST, :PASS-ON, etc. method handling.
;;; Editor: :MOUSE-BUTTON blips.  C-X 2 etc. bugs.
;;;   Don't update attribute lists that are syntactically invalid.
;;; Every QFASL file gets a :PACKAGE attribute.  Bind FASD-PACKAGE where needed.
;;; Linearizing various things can just touch things rather than copy.
;;; &SPECIAL in &AUX variables, and in Common Lisp interpreted functions.
;;; Rubout handler discards excess characters not read.
;;; WINDOW-WITH-INSIDE-SCROLL-BAR.  :STRING-OUT-EXPLICIT bug with kerning.
;;; while running on Lisp Machine Eighteen from band 6
;;; with Experimental System 98.1, CADR 3.0, Experimental ZMail 53.0, MIT-Specific 22.0, microcode 305, ZM MIT.


; From file BASWIN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "

(DEFFLAVOR WINDOW-WITH-INSIDE-SCROLL-BAR
	() (STREAM-MIXIN BORDERS-MIXIN LABEL-MIXIN BASIC-SCROLL-BAR
	    SELECT-MIXIN DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Simple window with scroll bar inside borders."))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

;Figure out the information needed to instantiate a flavor quickly.

;We store these three properties on the flavor:
;INSTANCE-VARIABLE-INITIALIZATIONS - alist of (ivar-index . init-form)
;REMAINING-DEFAULT-PLIST - a default plist from which kwds that init ivars have been removed.
;ALL-INITTABLE-INSTANCE-VARIABLES - a list parallel to FLAVOR-ALL-INSTANCE-VARIABLES
;				   which has either the keyword to init with or NIL.
;REMAINING-INIT-KEYWORDS - the init keywords that are handled and dont just init ivars.

;We also set up the FLAVOR-DEFAULT-HANDLER of the flavor.

(DEFUN COMPOSE-FLAVOR-INITIALIZATIONS (FL &AUX ALIST
				       REMAINING-DEFAULT-PLIST ALL-INITTABLE-IVARS
				       AREA-FUNCTION REQUIRED-INIT-KEYWORDS)
  (SETQ ALL-INITTABLE-IVARS (MAKE-LIST (LENGTH (FLAVOR-ALL-INSTANCE-VARIABLES FL))
				      ':AREA (IF *JUST-COMPILING* DEFAULT-CONS-AREA
					       BACKGROUND-CONS-AREA)))
  ;; First make the mask saying which ivars can be initted by init kywords.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (LET ((FFL (COMPILATION-FLAVOR FFL)))
      (OR AREA-FUNCTION
	  (SETQ AREA-FUNCTION (FLAVOR-GET FFL ':INSTANCE-AREA-FUNCTION)))
      (SETQ REQUIRED-INIT-KEYWORDS
	    (UNION REQUIRED-INIT-KEYWORDS (FLAVOR-GET FFL ':REQUIRED-INIT-KEYWORDS)))
      (OR (FLAVOR-DEFAULT-HANDLER FL)
	  (SETF (FLAVOR-DEFAULT-HANDLER FL)
		(GET (LOCF (FLAVOR-PLIST FFL)) ':DEFAULT-HANDLER)))
      (DOLIST (IIV (FLAVOR-INITTABLE-INSTANCE-VARIABLES FFL))
	(LET ((INDEX (FIND-POSITION-IN-LIST (CDR IIV) (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	  (AND INDEX
	       (SETF (NTH INDEX ALL-INITTABLE-IVARS)
		     (CAR IIV)))))))
  (SETF (FLAVOR-REMAINING-INIT-KEYWORDS FL)    
	(SUBSET-NOT #'MEMQ (FLAVOR-ALLOWED-INIT-KEYWORDS FL)
		    (CIRCULAR-LIST ALL-INITTABLE-IVARS)))
  ;; Then look at all the default init plists, for anything there
  ;; that initializes an instance variable.  If it does, make an entry on ALIST.
  ;; Any that doesn't initialize a variable, put on the "remaining" list.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (COMPILATION-FLAVOR FFL))
    (DO ((L (GET (LOCF (FLAVOR-PLIST FFL)) ':DEFAULT-INIT-PLIST) (CDDR L))) ((NULL L))
      (LET* ((KEYWORD (CAR L)) (ARG (CADR L))
	     (INDEX (FIND-POSITION-IN-LIST KEYWORD ALL-INITTABLE-IVARS)))
	;; Remove this keyword from the list of required ones,
	;; since it is cannot ever be missing.
	(SETQ REQUIRED-INIT-KEYWORDS
	      (DELQ KEYWORD REQUIRED-INIT-KEYWORDS))
	(IF INDEX
	    (OR (ASSQ INDEX ALIST)
		(PUSH (LIST INDEX ARG)
		      ALIST))
	  ;; This keyword does not just initialize an instance variable.
	  (IF (MEMQ KEYWORD (FLAVOR-REMAINING-INIT-KEYWORDS FL))
	      (OR (GET (LOCF REMAINING-DEFAULT-PLIST) KEYWORD)
		  (PUTPROP (LOCF REMAINING-DEFAULT-PLIST) ARG KEYWORD))
	    (FERROR NIL "The flavor ~S has keyword ~S in its default init plist, but doesn't handle it"
		    (FLAVOR-NAME FL) KEYWORD))))))
  ;; Then, look for default values provided in list of instance vars.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (COMPILATION-FLAVOR FFL))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES FFL))
      (AND (NOT (ATOM V))
	   ;; When we find one, put it in if there is no init for that variable yet.
	   (LET ((INDEX (FIND-POSITION-IN-LIST (CAR V) (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	     (AND (NOT (ASSQ INDEX ALIST))
		  (PUSH (LIST INDEX
			      (CADR V))
			ALIST))))))
  (IF AREA-FUNCTION
      (PUTPROP (LOCF (FLAVOR-PLIST FL)) AREA-FUNCTION 'INSTANCE-AREA-FUNCTION)
    (REMPROP (LOCF (FLAVOR-PLIST FL)) 'INSTANCE-AREA-FUNCTION))
  (IF REQUIRED-INIT-KEYWORDS
      (PUTPROP (LOCF (FLAVOR-PLIST FL)) REQUIRED-INIT-KEYWORDS 'REQUIRED-INIT-KEYWORDS)
    (REMPROP (LOCF (FLAVOR-PLIST FL)) 'REQUIRED-INIT-KEYWORDS))
  (SETF (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL) ALIST)
  (SETF (FLAVOR-REMAINING-DEFAULT-PLIST FL) REMAINING-DEFAULT-PLIST)
  (SETF (FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES FL) ALL-INITTABLE-IVARS))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN (:SELECT-METHOD PROCESS-SPECIAL-COMMAND :MOUSE-BUTTON) (IGNORE CH WINDOW X Y)
  ;; This is sent if he clicks on the mode line window, etc.
  WINDOW X Y
  (WHEN (= CH #\MOUSE-R)
    (TV:MOUSE-CALL-SYSTEM-MENU)))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

;This function does all the "outer loop" of the compiler.  It is called
;by the editor as well as the compiler.
;INPUT-STREAM is what to compile.  GENERIC-PATHNAME is for the corresponding file.
;FASD-FLAG is NIL if not making a QFASL file.
;PROCESS-FN is called on each form.
;QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
;FILE-LOCAL-DECLARATIONS is normally initialized to NIL,
;but you can optionally pass in an initializations for it.
;READ-THEN-PROCESS-FLAG means do all reading first, thus minimizing thrashing.

(DEFUN COMPILE-STREAM (INPUT-STREAM GENERIC-PATHNAME FASD-FLAG PROCESS-FN
		       QC-FILE-LOAD-FLAG QC-FILE-IN-CORE-FLAG PACKAGE-SPEC
		       &OPTIONAL (FILE-LOCAL-DECLARATIONS NIL) IGNORE
		       COMPILING-WHOLE-FILE-P
		       &AUX (PACKAGE PACKAGE) (IBASE IBASE) (BASE BASE)
		       FILE-SPECIAL-LIST FILE-UNSPECIAL-LIST
		       FDEFINE-FILE-PATHNAME
		       (READ-FUNCTION (IF QC-FILE-CHECK-INDENTATION
					  'READ-CHECK-INDENTATION 'READ)))
  "This function does all the /"outer loop/" of the compiler, for file and editor compilation.a
Expressions to be compiled are read from INPUT-STREAM.
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
	   (LET ((PATHNAME (AND (MEMQ ':PATHNAME (FUNCALL INPUT-STREAM ':WHICH-OPERATIONS))
				(FUNCALL INPUT-STREAM ':PATHNAME))))
	     (AND PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME))))
     ;; Having bound the variables, process the file.
     (LET ((QC-FILE-IN-PROGRESS T)
	   (UNDO-DECLARATIONS-FLAG (NOT QC-FILE-LOAD-FLAG))
	   (LOCAL-DECLARATIONS NIL)
	   (OPEN-CODE-MAP-SWITCH OPEN-CODE-MAP-SWITCH)
	   (RUN-IN-MACLISP-SWITCH RUN-IN-MACLISP-SWITCH)
	   (OBSOLETE-FUNCTION-WARNING-SWITCH OBSOLETE-FUNCTION-WARNING-SWITCH)
	   (ALL-SPECIAL-SWITCH ALL-SPECIAL-SWITCH)
	   (SOURCE-FILE-UNIQUE-ID)
	   (FASD-PACKAGE NIL)
	   (FORM-LIST))
       (COND (FASD-FLAG
	      ;; Copy all suitable file properties into the fasl file
	      ;; Suitable means those that are lambda-bound when you read in a file.
	      (LET ((PLIST (COPYLIST (FUNCALL GENERIC-PATHNAME ':PLIST))))
		;; Remove unsuitable properties
		(DO ((L (LOCF PLIST)))
		    ((NULL (CDR L)))
		  (IF (NOT (NULL (GET (CADR L) 'FS:FILE-ATTRIBUTE-BINDINGS)))
		      (SETQ L (CDDR L))
		      (RPLACD L (CDDDR L))))
		;; Make sure the package property is really the package compiled in
		;; Must load QFASL file into same package compiled in
		;; On the other hand, if we did not override it
		;; and the attribute list has a list for the package, write that list.
		(OR (AND (NOT (ATOM (GET (LOCF PLIST) ':PACKAGE)))
			 (STRING-EQUAL (PACKAGE-NAME PACKAGE)
				       (CAR (GET (LOCF PLIST) ':PACKAGE))))
		    (PUTPROP (LOCF PLIST)
			     (INTERN (PACKAGE-NAME PACKAGE) SI:PKG-KEYWORD-PACKAGE)
			     ':PACKAGE))
		(AND INPUT-STREAM
		     (MEMQ ':TRUENAME (FUNCALL INPUT-STREAM ':WHICH-OPERATIONS))
		     (SETQ SOURCE-FILE-UNIQUE-ID (FUNCALL INPUT-STREAM ':TRUENAME))
		     (PUTPROP (LOCF PLIST)
			      SOURCE-FILE-UNIQUE-ID
			      ':QFASL-SOURCE-FILE-UNIQUE-ID))
		;; If a file is being compiled across directories, remember where the
		;; source really came from.
		(AND FDEFINE-FILE-PATHNAME FASD-STREAM
		     (LET ((OUTFILE (AND (MEMQ ':PATHNAME
					       (FUNCALL FASD-STREAM ':WHICH-OPERATIONS))
					 (FUNCALL FASD-STREAM ':PATHNAME))))
		       (COND (OUTFILE
			      (SETQ OUTFILE (FUNCALL OUTFILE ':GENERIC-PATHNAME))
			      (AND (NEQ OUTFILE FDEFINE-FILE-PATHNAME)
				   (PUTPROP (LOCF PLIST) FDEFINE-FILE-PATHNAME
					    ':SOURCE-FILE-GENERIC-PATHNAME))))))
		(MULTIPLE-VALUE-BIND (MAJOR MINOR)
		    (SI:GET-SYSTEM-VERSION "System")
		  (PUTPROP (LOCF PLIST)
			   (LIST USER-ID
				 SI:LOCAL-PRETTY-HOST-NAME
				 (TIME:GET-UNIVERSAL-TIME)
				 MAJOR MINOR '(NEW-DESTINATIONS T))
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
	 (FUNCALL PROCESS-FN FORM))))))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

;;; This is the heart of the M-X Fasl Update command.
;;; Reads from INPUT-STREAM using READ-FUNCTION (called with arguments like READ's)
;;; INFILE should be the name of the input file that INPUT-STREAM is reading from.
;;; OUTFILE is a pathname used to open an output file.
(DEFUN FASL-UPDATE-STREAM (INFILE OUTFILE INPUT-STREAM READ-FUNCTION
			   &AUX QC-FILE-LOAD-FLAG (QC-FILE-IN-CORE-FLAG T)
			   LAST-ERROR-FUNCTION
			   (DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  INFILE
  (UNWIND-PROTECT
    (LET ((QC-FILE-IN-PROGRESS T)
	  (LOCAL-DECLARATIONS NIL)
	  (FILE-LOCAL-DECLARATIONS NIL)
	  (FASD-PACKAGE NIL))
      (LOCKING-RESOURCES
	(WITH-OPEN-FILE (FASD-STREAM OUTFILE '(:WRITE :FIXNUM))
	  (FASD-INITIALIZE)
	  (FASD-START-FILE)
	  ;; First thing in QFASL file must be property list
	  ;; Only property supported just now is PACKAGE property
	  (FASD-ATTRIBUTES-LIST
	    (LIST ':PACKAGE (INTERN (PKG-NAME PACKAGE) SI:PKG-KEYWORD-PACKAGE)))
	  (QC-PROCESS-INITIALIZE)
	  (DO ((EOF (NCONS NIL))
	       FORM)
	      (NIL)
	    ;; Start a new whack if FASD-TABLE is getting too big.
	    (AND ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
		 (FASD-END-WHACK))
	    ;; Read and macroexpand in temp area.
	    (SETQ DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA)
	    (LET ((QC-FILE-READ-IN-PROGRESS T))
	      (SETQ FORM (FUNCALL READ-FUNCTION INPUT-STREAM EOF)))
	    (AND (EQ EOF FORM)
		 (RETURN NIL))
	    (SETQ FORM (MACROEXPAND FORM T))
	    (SETQ DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
	    ;; Output this form in the appropriate way.
	    (COMPILE-DRIVER FORM 'FASL-UPDATE-FORM NIL))
	  (FASD-END-WHACK)
	  (FASD-END-FILE))))
    (QC-FILE-RESET)))

))

; From file QCFASD.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "


(DEFUN FASD-SYMBOL-VALUE (FILENAME SYMBOL)
  "Write a QFASL file named FILENAME containing SYMBOL's value.
Loading the file will set the symbol back to the same value."
  (LET ((FASD-PACKAGE NIL))
    (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME FS:LOAD-PATHNAME-DEFAULTS
							     ':QFASL)
				 '(:WRITE :FIXNUM))
      (LOCKING-RESOURCES
	(FASD-INITIALIZE)
	(FASD-START-FILE)
	(FASD-ATTRIBUTES-LIST `(:PACKAGE :USER))
	(FASD-FORM `(SETQ ,SYMBOL ',(SYMEVAL SYMBOL)))
	(FASD-END-WHACK)
	(FASD-END-FILE)))))

(DEFUN DUMP-FORMS-TO-FILE (FILENAME FORMS-LIST &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME which, when loaded, will execute the forms in FORMS-LIST.
ATTRIBUTE-LIST is a file attribute list which controls, among other things,
what package the file is dumped and loaded in (default is USER)."
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME FS:LOAD-PATHNAME-DEFAULTS
							   ':QFASL)
			       '(:WRITE :FIXNUM))
    (LET ((FASD-PACKAGE NIL))
      (LOCKING-RESOURCES
	(FASD-INITIALIZE)
	(FASD-START-FILE)
	(FASD-ATTRIBUTES-LIST
	  (IF (GET (LOCF ATTRIBUTE-LIST) ':PACKAGE) ATTRIBUTE-LIST
	    (LIST* ':PACKAGE :USER ATTRIBUTE-LIST)))
	(DOLIST (FORM FORMS-LIST)
	  (IF ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
	      (FASD-END-WHACK))
	  (FASD-FORM FORM))
	(FASD-END-WHACK)
	(FASD-END-FILE)))))

))

; From file QCFASD.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-FILE-SYMBOLS-PROPERTIES (FILENAME SYMBOLS PROPERTIES
                                              DUMP-VALUES-P DUMP-FUNCTIONS-P
                                              NEW-SYMBOL-FUNCTION)
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
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME FS:LOAD-PATHNAME-DEFAULTS
							   ':QFASL)
			       '(:WRITE :FIXNUM))
    (LET ((FASD-PACKAGE NIL))
      (LOCKING-RESOURCES
	(FASD-INITIALIZE)
	(FASD-START-FILE)
	(FASD-ATTRIBUTES-LIST '(:PACKAGE :USER))
	(FASD-SYMBOLS-PROPERTIES SYMBOLS PROPERTIES DUMP-VALUES-P
				 DUMP-FUNCTIONS-P NEW-SYMBOL-FUNCTION)
	(FASD-END-WHACK)
	(FASD-END-FILE)))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "


  ;NIL, in the source, since have already done a gc when this patch is loaded.
(DEFVAR PATHNAME-PLISTS-LINEARIZED-ONCE T)

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFUN LINEARIZE-PATHNAME-PLISTS ()
  (IF PATHNAME-PLISTS-LINEARIZED-ONCE
      ;; If already been recopied, just reference all of them so they are
      ;; all copied into newspace together.
      (MAPHASH #'(LAMBDA (IGNORE FILE &REST IGNORE)
		   (LET ((PLIST (SEND FILE ':PLIST)))
		     (NSUBST '(NIL) '(NIL) PLIST)))
	       FS:*PATHNAME-HASH-TABLE*)
    (SETQ PATHNAME-PLISTS-LINEARIZED-ONCE T)
    (MAPHASH #'(LAMBDA (IGNORE FILE &REST IGNORE)
		 (SEND FILE ':SET-PROPERTY-LIST (COPYTREE (SEND FILE ':PLIST))))
	     FS:*PATHNAME-HASH-TABLE*)))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

;Process a Lambda-list (X), making the variables by default of kind KIND
;(FEF-ARG-REQ for the top-level lambda,
; FEF-ARG-AUX or FEF-ARG-INTERNAL-AUX for progs).
;Return a prog variable list for the same variables with their initializations if any,
;with P1 done on each initialization.
;This function gobbles down the variables and processes keywords.
;Each variable, with its appropeiate keyword info, is passed to P1LMB.
;We can do either sequential or parallel binding.
;Processing of variables is done in two steps:
;First, create the homes
;Second, if these are not FEF-ARG-INTERNAL-AUX vars,
; put the homes on VARS and ALLVARS.
;Third, process all the variables' initializations.
;Finally, put the homes on VARS and ALLVARS if not already there.

;For variables whose scope is the whole function (not FEF-ARG-INTERNAL-AUX),
;the order is designed so that variables bound inside their initializations
;all come after all the variables of the original (higher) level.
;This is needed to make sure that (DEFUN FOO (&OPTIONAL (A (LET ((C ...)) ...)) B) ...)
;does not put C into VARS before B.

;For FEF-ARG-INTERNAL-AUX variables, we want the variables bound
;inside the initializations to come first, since they are used first.
;That way, our own variables overlap with them rather than vice versa.
;As a result, the variable with the original home is always the first one used.
;This is important for deciding which variables need explicit initialization.

;The IGNORE-NIL-P argument is used by MULTIPLE-VALUE-BIND to say
; that if NIL appears as a variable, its initial value should be evaluated
; and discarded.
(DEFUN P1SBIND (X KIND PARALLEL IGNORE-NIL-P THIS-FRAME-DECLARATIONS)
  (LET (TM EVALCODE VARN MYVARS MISC-TYPES
	SPECIFIED-FLAGS (SPECIALNESS NIL) ALREADY-REST-ARG)
    ;; First look at the var specs and make homes, pushing them on MYVARS (reversed).
    (PROG ()
	  (SETQ EVALCODE 'FEF-QT-DONTCARE)
       A  (COND ((NULL X) (RETURN))
		((SETQ TM (ASSQ (CAR X)
				'((&OPTIONAL . FEF-ARG-OPT)
				  (&REST . FEF-ARG-REST) (&AUX . FEF-ARG-AUX))))
		 (COND ((OR (EQ KIND 'FEF-ARG-AUX)
			    (EQ KIND 'FEF-ARG-INTERNAL-AUX))
			(WARN 'BAD-BINDING-LIST ':IMPOSSIBLE
			      "A lambda-list keyword (~S) appears in an internal binding list."
			      (CAR X)))
		       (T (SETQ KIND (CDR TM))))
		 (GO B))
		((SETQ TM (ASSQ (CAR X)
				'((&EVAL . FEF-QT-EVAL)
				  (&QUOTE . FEF-QT-QT)
				  (&QUOTE-DONTCARE . FEF-QT-DONTCARE))))
		 (SETQ EVALCODE (CDR TM))
		 (GO B))
		((SETQ TM (ASSQ (CAR X)
				'((&FUNCTIONAL . FEF-FUNCTIONAL-ARG))))
		 (PUSH (CDR TM) MISC-TYPES)
		 (GO B))
		((EQ (CAR X) '&SPECIAL)
		 (SETQ SPECIALNESS T)
		 (GO B))
		((EQ (CAR X) '&LOCAL)
		 (SETQ SPECIALNESS NIL)
		 (GO B))
		((MEMQ (CAR X) LAMBDA-LIST-KEYWORDS)
		 (GO B)))
	  ;; LAMBDA-list keywords have jumped to B.
	  ;; Now (CAR X) should be a variable or (var init).
	  (SETQ VARN (COND ((ATOM (CAR X)) (CAR X)) (T (CAAR X))))
	  (UNLESS (SYMBOLP VARN)
	    (WARN 'VARIABLE-NOT-SYMBOL ':IMPOSSIBLE
		  "~S appears in a list of variables to be bound." VARN)
	    (GO B))
	  (AND (NOT (OR (STRING-EQUAL VARN '|IGNORE|)
			(STRING-EQUAL VARN '|IGNORED|)
			(NULL VARN)))
	       ;; Does this variable appear again later?
	       ;; An exception is made in that a function argument can be repeated
	       ;; after an &AUX.
	       (DOLIST (X1 (CDR X))
		 (COND ((EQ X1 '&AUX) (RETURN NIL))
		       ((OR (EQ X1 VARN)
			    (AND (NOT (ATOM X1)) (EQ (CAR X1) VARN)))
			(RETURN T))))
	       (WARN 'BAD-BINDING-LIST ':IMPLAUSIBLE
		     "The variable ~S appears twice in one binding list."
		     VARN))
	  (AND (= (AREF (GET-PNAME VARN) 0) #/&)
	       (WARN 'MISSPELLED-KEYWORD ':IMPLAUSIBLE
		     "~S is probably a misspelled keyword." VARN))
	  (IF ALREADY-REST-ARG
	      (WARN 'BAD-LAMBDA-LIST ':IMPOSSIBLE
		    "Argument ~S comes after the &REST argument." VARN))
	  (IF (EQ KIND 'FEF-ARG-REST)
	      (SETQ ALREADY-REST-ARG T))
	  (COND ((AND IGNORE-NIL-P (NULL VARN))
		 (P1 (CADAR X)))		;Out of order, but works in these simple cases
		((OR (NULL VARN) (EQ VARN T))
		 (WARN 'NIL-OR-T-SET ':IMPOSSIBLE "There is an attempt to bind ~S." VARN))
		(T
		 ;; Make the variable's home.
		 (IF SPECIALNESS
		     (LET ((DECL (LIST 'SPECIAL
				       (COND ((SYMBOLP (CAR X)) (CAR X))
					     ((SYMBOLP (CAAR X)) (CAAR X))
					     (T (CADAAR X))))))
		       (PUSH DECL LOCAL-DECLARATIONS)
		       (PUSH DECL THIS-FRAME-DECLARATIONS)))
		 (PUSH (P1BINDVAR (CAR X) KIND EVALCODE MISC-TYPES
				  THIS-FRAME-DECLARATIONS)
		       MYVARS)))
	  (SETQ MISC-TYPES NIL)
       B
	  (SETQ X (CDR X))
	  (GO A))

    ;; Arguments should go on ALLVARS now, so all args precede all boundvars.
    (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
	(EQ KIND 'FEF-ARG-AUX)
	(SETQ ALLVARS (APPEND SPECIFIED-FLAGS MYVARS ALLVARS)))
    (MAPC 'VAR-COMPUTE-INIT SPECIFIED-FLAGS (CIRCULAR-LIST NIL))

    ;; Now do pass 1 on the initializations for the variables.
    (DO ((ACCUM)
	 (VS (REVERSE MYVARS) (CDR VS)))
	((NULL VS)
	 ;; If parallel binding, put all var homes on VARS
	 ;; after all the inits are thru.
	 (COND (PARALLEL
		(SETQ VARS (APPEND MYVARS VARS))
		(COND ((OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
			   (EQ KIND 'FEF-ARG-AUX))
		       (MAPC 'VAR-CONSIDER-OVERLAP MYVARS)
		       (SETQ ALLVARS (APPEND MYVARS ALLVARS))))))
	 (NREVERSE ACCUM))
      (PUSH (VAR-COMPUTE-INIT (CAR VS) PARALLEL) ACCUM)
      ;; For sequential binding, put each var on VARS
      ;; after its own init.
      (OR PARALLEL
	  (PROGN (COND ((OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
			    (EQ KIND 'FEF-ARG-AUX))
			(VAR-CONSIDER-OVERLAP (CAR VS))
			(PUSH (CAR VS) ALLVARS)))
		 (PUSH (CAR VS) VARS)
		 (LET ((TEM (CDDR (VAR-INIT (CAR VS)))))
		   (AND TEM (PUSH TEM VARS))))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro apply-lambda-bindvar (var value &optional force-special)
  `(progn
     (setq this-specialp (or ,force-special
			     (interpreter-specialp (value-cell-location ,var))))
     (unless (car interpreter-environment)
       (with-stack-list (tem t)
	 (setf (car interpreter-environment) tem)))
     (%push (value-cell-location ,var))
     (%push ,value)
     ;; Modify cdr-code of last word pushed, to terminate the list.
     (with-stack-list (tem nil)
       (%p-dpb-offset cdr-next %%q-cdr-code tem -3)
       (%p-dpb-offset cdr-nil %%q-cdr-code tem -1)
       (setq thisval tem))
     ;; Bind the variable as special, if appropriate.
     (unless ,var (ferror nil "Attempt to bind NIL"))
     (when this-specialp
       (bind (value-cell-location ,var) (%p-contents-offset thisval -1))
       (%p-store-data-type (%make-pointer-offset dtp-list thisval -1)
			   dtp-one-q-forward)
       (%p-store-pointer (%make-pointer-offset dtp-list thisval -1)
			 (value-cell-location ,var)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun cl-apply-lambda (fctn a-value-list)
    (prog apply-lambda ()
       (or (consp fctn) (go bad-function))
     tail-recurse
       (cond ((memq (car fctn) '(cli:lambda cli:subst cli:named-lambda cli:named-subst))
	      (let-if (memq (car fctn) '(cli:named-lambda cli:named-subst))
		      ((interpreter-environment nil)
		       (interpreter-function-environment nil)
		       (local-declarations nil))
		;; This won't happen when standard Common Lisp code
		;; interacts with Zetalisp code, but might as well
		;; make (APPLY '(CLI:LAMBDA ...) ...) in Zetalisp work right.
		(let-if (eq interpreter-function-environment t)
			((interpreter-function-environment nil))
		  (let* (optionalf quoteflag tem restf init this-restf specialf
			 (fctn1 (cond ((eq (car fctn) 'cli:named-lambda) (cdr fctn))
				      ((eq (car fctn) 'cli:named-subst) (cdr fctn))
				      (t fctn)))
			 (lambda-list (cadr fctn1))
			 (body (cddr fctn1))
			 (value-list a-value-list)
			 (local-declarations local-declarations)
			 this-specialp thisval  ;Used by expansion of apply-lambda-bindvar
			 keynames keyvalues keyinits keykeys keyoptfs keyflags
			 key-supplied-flags
			 allow-other-keys)
		    (and (cdr body) (stringp (car body)) (pop body))	;and doc string.
		    ;; Make a binding frame to represent any SPECIAL declarations.
		    (with-stack-list* (interpreter-environment nil interpreter-environment)
		      ;; Find any declarations at the front of the function body
		      ;; and put them onto LOCAL-DECLARATIONS and into INTERPRETER-ENVIRONMENT.
		      (gobble-declarations-internal body)
		      (with-stack-list* (interpreter-environment nil interpreter-environment)
			(prog (thisvar vars-left vals-left)
			  ;; THISVAR is name of argument being processed.
      
			  ;; If SELF is an instance, and its instance vars aren't bound, bind them.
			  (when (and (typep self ':instance)
				     (neq self slots-bound-instance))
			    (tagbody
				(setq tem (self-binding-instances))
			     loop
				(unless tem (go exit))
				(apply-lambda-bindvar-1 (car tem) (cadr tem))
				(setq tem (cddr tem))
				(go loop)
			     exit)
			    (bind (locf slots-bound-instance) self))
		     l    (cond ((null value-list) (go lp1))
				((or (null lambda-list)
				     (eq (car lambda-list) '&aux)) 
				 (cond (restf (go lp1)))
				 (return-from apply-lambda
				   (signal-proceed-case
				     ((args)
				      (make-condition 'sys:too-many-arguments
						      "Function ~S called with too many arguments (~D)."
						      fctn (length a-value-list) a-value-list))
				     (:fewer-arguments
				      (apply fctn (append a-value-list args)))
				     (:return-value args)
				     (:new-argument-list (apply fctn args)))))
				((eq (car lambda-list) '&key)
				 (go key))
				((eq (car lambda-list) '&optional)
				 (setq optionalf t)
				 (go l1))		    ;Do next value.
				((memq (car lambda-list) '(&quote &eval))
				 (setq quoteflag (eq (car lambda-list) '&quote))
				 (go l1))
				((memq (car lambda-list) '(&special &local))
				 (setq specialf (eq (car lambda-list) '&special))
				 (go l1))
				((eq (car lambda-list) '&rest)
				 (setq this-restf t)
				 (go l1))		    ;Do next value.
				((memq (car lambda-list) lambda-list-keywords)
				 (go l1))
				((atom (car lambda-list))
				 (setq thisvar (car lambda-list)))
				((atom (caar lambda-list))
				 (setq thisvar (caar lambda-list))
				 ;; If it's &OPTIONAL (FOO NIL FOOP),
				 ;; bind FOOP to T since FOO was specified.
				 (cond ((and optionalf (cddar lambda-list))
					(and (null (caddar lambda-list)) (go bad-lambda-list))
					(apply-lambda-bindvar (caddar lambda-list)
							      t specialf))))
				(t (go bad-lambda-list)))
			  ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
			  ;;  It is in THISVAR.
			  (and (null thisvar) (go bad-lambda-list))
			  (cond (restf (go bad-lambda-list))	;Something follows a &REST arg???
				(this-restf		;This IS the &REST arg.
				 ;; If quoted arg, and the list of values is in a pdl, copy it.
				 (and quoteflag
				      (ldb-test %%pht2-map-access-code
						(area-region-bits (%area-number value-list)))
				      (let ((default-cons-area background-cons-area))
					(setq value-list (copylist value-list))))
				 (apply-lambda-bindvar thisvar value-list specialf)
				 ;; We don't clear out VALUE-LIST
				 ;; in case keyword args follow.
				 (setq this-restf nil restf t)
				 (go l1)))
      
			  (apply-lambda-bindvar thisvar (car value-list) specialf)
			  (pop value-list)
		     l1   (pop lambda-list)
			  (go l)
	
		     key  (setf (values nil nil lambda-list nil nil
					keykeys keynames keyoptfs keyinits keyflags
					allow-other-keys)
				(decode-keyword-arglist lambda-list))
			  ;; Make a list of all required keywords we haven't seen yet.
			  ;; Make alist of (keyword supplied-flag-var supplied-this-time-p)
			  (do ((keyl keykeys (cdr keyl))
			       (flagl keyflags (cdr flagl)))
			      ((null keyl))
			    (and (car flagl)
				 (push (list (car keyl) (car flagl) nil)
				       key-supplied-flags)))
	
			  (setq keyvalues (make-list (length keynames)))
			  ;; Now look at what keyword args were actually supplied.
			  ;; Set up KEYVALUES to contain values corresponding
			  ;; with the variable names in KEYNAMES.
			  (do ((vl value-list (cddr vl))
			       keyword)
			      ((null vl))
			    (or (cdr vl)
				(ferror 'sys:bad-keyword-arglist
					"No argument after keyword ~S"
					(car vl)))
			    (setq keyword (car vl))
			    retry
			    (let ((tem (find-position-in-list keyword keykeys)))
			      (cond (tem
				     (setf (nth tem keyvalues) (cadr vl))
				     (setf (nth tem keyinits) nil)
				     (let ((tem1 (assq keyword key-supplied-flags)))
				       (and tem1 (setf (caddr tem1) t))))
				    ((not allow-other-keys)
				     (setq keyword (cerror ':new-keyword nil
							   'sys:undefined-keyword-argument
							   "Keyword arg keyword ~S, with value ~S, is unrecognized."
							   keyword
							   (cadr vl)))
				     (and keyword (go retry))))))
			  ;; Eval the inits of any keyword args that were not supplied.
			  (do ((kvs keyvalues (cdr kvs))
			       (kis keyinits (cdr kis)))
			      ((null kvs))
			    (and (car kis)
				 (rplaca kvs (eval1 (car kis)))))
			  ;; Bind the supplied-flags of the optional keyword args.
			  ;; Can't use DO here because the bindings must stay around.
		     key1 (cond (key-supplied-flags
				 (apply-lambda-bindvar (cadar key-supplied-flags)
						       (caddar key-supplied-flags))
				 (pop key-supplied-flags)
				 (go key1)))
			  ;; If any required keyword args were not specified, barf.
			  ;; Actually bind the keyaord arg variables
			  (setq vals-left keyvalues
				vars-left keynames)
		     keybind
			  (unless vars-left (go lp1))
			  (apply-lambda-bindvar (car vars-left) (car vals-left))
			  (pop vars-left)
			  (pop vals-left)
			  (go keybind)
			  ;; Keyword args always use up all the values that are left...
	
			  ;; Here when all values used up.
		     lp1  (cond ((null lambda-list) (go ex1))
				((eq (car lambda-list) '&rest)
				 (and restf (go bad-lambda-list))
				 (setq this-restf t)
				 (go lp2))
				((eq (car lambda-list) '&key)
				 (go key))
				((memq (car lambda-list) '(&optional &aux))
				 (setq optionalf t)		;SUPPRESS TOO FEW ARGS ERROR
				 (go lp2))
				((memq (car lambda-list) '(&special &local))
				 (setq specialf (eq (car lambda-list) '&special))
				 (go lp2))
				((memq (car lambda-list) lambda-list-keywords)
				 (go lp2))
				((and (null optionalf) (null this-restf))
				 (and restf (go bad-lambda-list))
				 (return-from apply-lambda
				   (signal-proceed-case
				     ((args)
				      (make-condition 'sys:too-few-arguments
						      "Function ~S called with only ~D argument~1G~P."
						      fctn (length a-value-list) a-value-list))
				     (:additional-arguments
				      (apply fctn (append a-value-list args)))
				     (:return-value args)
				     (:new-argument-list (apply fctn args)))))
				((atom (car lambda-list)) (setq tem (car lambda-list))
				 (setq init nil))
				((atom (caar lambda-list))
				 (setq tem (caar lambda-list))
				 (setq init (eval (cadar lambda-list)))
				 ;; For (FOO NIL FOOP), bind FOOP to NIL since FOO is missing.
				 (cond ((cddar lambda-list)
					(and (null (caddar lambda-list)) (go bad-lambda-list))
					(apply-lambda-bindvar (caddar lambda-list)
							      nil specialf))))
				(t (go bad-lambda-list)))
		     lp3  (and (null tem) (go bad-lambda-list))
			  (apply-lambda-bindvar tem init specialf)
			  (and this-restf (setq restf t))
			  (setq this-restf nil)
		     lp2  (setq lambda-list (cdr lambda-list))
			  (go lp1)
    
		     ex1  ;; Here to evaluate the body.
			  (return-from apply-lambda (eval-body body))
		     bad-lambda-list
			  (setq fctn
				(cerror ':new-function nil 'sys:invalid-lambda-list
					"~S has an invalid LAMBDA list" fctn))
		     retry
			  (return-from apply-lambda (apply fctn a-value-list)))))))))
	     ((eq (car fctn) 'macro)
              (ferror 'sys:funcall-macro
		      "Funcalling the macro ~S."
		      (function-name (cdr fctn)))
	      (return-from apply-lambda
			   (eval (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list)))))
	     )

       ;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
       (cond ((lambda-macro-call-p fctn)
	      (setq fctn (lambda-macro-expand fctn))
	      (go retry)))

       bad-function
       ;; Can drop through to here for a totally unrecognized function.
       (setq fctn
	     (cerror ':new-function nil 'sys:invalid-function
		     "~S is an invalid function." fctn))
       (go retry)

       retry
       (and (consp fctn) (go tail-recurse))
       (return (apply fctn a-value-list))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :RUBOUT-HANDLER) (RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (COND ((> (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
	 (COPY-ARRAY-PORTION RUBOUT-HANDLER-BUFFER (RHB-SCAN-POINTER) (RHB-FILL-POINTER)
			     RUBOUT-HANDLER-BUFFER 0 (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER))
	 (IF (NUMBERP (RHB-TYPEIN-POINTER))
	     (DECF (RHB-TYPEIN-POINTER) (RHB-SCAN-POINTER)))
	 (SETF (RHB-FILL-POINTER) (- (RHB-FILL-POINTER) (RHB-SCAN-POINTER))))
	(T (SETF (RHB-FILL-POINTER) 0)))
  (SETF (RHB-SCAN-POINTER) 0)
  (SETF (RHB-INITIAL-ENTRY) T)
  (MULTIPLE-VALUE-BIND (PROMPT-STARTING-X PROMPT-STARTING-Y)
      (FUNCALL-SELF ':READ-CURSORPOS)
    (LET ((PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
      (AND PROMPT-OPTION			;Prompt if desired
	   (RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL)))
    (MULTIPLE-VALUE-BIND (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	(FUNCALL-SELF ':READ-CURSORPOS)
      ;; Output any "typeahead"
      (AND (PLUSP (RHB-FILL-POINTER))
	   (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
      (DO ((RUBOUT-HANDLER T)			;Establish rubout handler
	   (RUBOUT-HANDLER-INSIDE T)
	   (RUBOUT-HANDLER-RE-ECHO-FLAG NIL NIL))
	  (NIL)
	(*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
	  (CONDITION-CASE (ERROR)
	      (RETURN
	       (MULTIPLE-VALUE-PROG1
		 (APPLY FUNCTION ARGS)		;Call READ or whatever.
		 (SETF (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
		 (IF (> (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER))
		     (SETF (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER)))))
	    (SYS:READ-ERROR
	     (TERPRI SELF)
	     (PRINC ">>ERROR: " SELF)
	     (SEND ERROR ':REPORT SELF)
	     (TERPRI SELF)
	     (SETQ RUBOUT-HANDLER-RE-ECHO-FLAG T)
	     (DO () (NIL) (FUNCALL-SELF ':TYI)))))	;If error, force user to rub out
	;;Maybe return when user rubs all the way back
	(AND (ZEROP (RHB-FILL-POINTER))
	     (LET ((FULL-RUBOUT-OPTION (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS)))
	       (WHEN FULL-RUBOUT-OPTION
		 ;; Get rid of the prompt, if any.
		 (SHEET-CLEAR-BETWEEN-CURSORPOSES
		   SELF PROMPT-STARTING-X PROMPT-STARTING-Y
		   (- CURSOR-X LEFT-MARGIN-SIZE) (- CURSOR-Y TOP-MARGIN-SIZE))
		 (SHEET-SET-CURSORPOS SELF PROMPT-STARTING-X PROMPT-STARTING-Y)
		 (RETURN NIL (CADR FULL-RUBOUT-OPTION)))))))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

; This macro is used to define a flavor.  Use DEFMETHOD to define
; methods (responses to messages sent to an instance of a flavor.)
(DEFMACRO DEFFLAVOR (NAME INSTANCE-VARIABLES COMPONENT-FLAVORS &REST OPTIONS)
  (LET ((COPIED-OPTIONS (COPYLIST OPTIONS)))
   `(PROGN
      ;; Define flavor at load time.
      ;; Must come before the compile-time COMPOSE-AUTOMATIC-METHODS,
      ;; which puts methods in the QFASL file.
      (EVAL-WHEN (LOAD EVAL)
	(DEFFLAVOR2 ',NAME ',INSTANCE-VARIABLES
	  ',COMPONENT-FLAVORS ',COPIED-OPTIONS))
      ;; Define the flavor if not loading.  
      (EVAL-WHEN (COMPILE)
	(IF (JUST-COMPILING)
	    (LET ((*JUST-COMPILING* T))
	      (DEFFLAVOR2 ',NAME ',INSTANCE-VARIABLES
		',COMPONENT-FLAVORS ',COPIED-OPTIONS)
	      ;; Compile the automatic instance-variable get/set methods into QFASL file
	      (COMPOSE-AUTOMATIC-METHODS (COMPILATION-FLAVOR ',NAME)))
	  ;; Compiling in editor buffer.  Must define automatic methods for real now.
	  (COMPOSE-AUTOMATIC-METHODS (GET ',NAME 'FLAVOR))))
     (EVAL-WHEN (EVAL)
       ;; Create the instance-variable get/set methods if evaling.
       (COMPOSE-AUTOMATIC-METHODS (GET ',NAME 'FLAVOR)))
     (EVAL-WHEN (COMPILE LOAD EVAL)
       ;; Make any instance-variable accessor macros, needed at both compile and run times.
       . ,(DO ((VS (DO ((OPTS OPTIONS (CDR OPTS)))
		       ((NULL OPTS) NIL)
		     (AND (CONSP (CAR OPTS))
			  (EQ (CAAR OPTS) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			  (RETURN (CDAR OPTS)))
		     (AND (EQ (CAR OPTS) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			  (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
					  INSTANCE-VARIABLES))))
		   (CDR VS))
	       (PREFIX (OR (CADR (ASSQ-CAREFUL ':ACCESSOR-PREFIX OPTIONS))
			   (STRING-APPEND NAME "-")))
	       (ORDS (DO ((OPTS OPTIONS (CDR OPTS)))
			 ((NULL OPTS) NIL)
		       (AND (CONSP (CAR OPTS))
			    (EQ (CAAR OPTS) ':ORDERED-INSTANCE-VARIABLES)
			    (RETURN (CDAR OPTS)))
		       (AND (EQ (CAR OPTS) ':ORDERED-INSTANCE-VARIABLES)
			    (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
					    INSTANCE-VARIABLES)))))
	       (RES NIL (CONS `(DEFSUBST-WITH-PARENT ,(INTERN1 (STRING-APPEND PREFIX (CAR VS)))
						     ,NAME
						     (,NAME)
				 ,(IF (MEMQ (CAR VS) ORDS)
				      `(%INSTANCE-REF ,NAME
					   ,(1+ (FIND-POSITION-IN-LIST (CAR VS) ORDS)))
				    `(SYMEVAL-IN-INSTANCE ,NAME ',(CAR VS))))
			      RES)))
	      ((NULL VS) RES)))
     ,@(MAKE-RUN-TIME-ALTERNATIVE-DEFFLAVORS
	 NAME (OR (CDR (ASSQ-CAREFUL ':RUN-TIME-ALTERNATIVES OPTIONS))
		  (CDR (ASSQ-CAREFUL ':MIXTURE OPTIONS))))
     ',NAME)))

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
  (PERFORM-FLAVOR-REDEFINITION (FLAVOR-NAME FL) T)
  (REMPROP (FLAVOR-NAME FL) 'FLAVOR))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN LINEARIZE-FLAVOR-PLISTS ()
  "Recopy all flavor plists (and other things) so that they are linear and compact."
  (DOLIST (NAME *ALL-FLAVOR-NAMES*)
    (LET ((FL (GET NAME 'FLAVOR)))
      (UNLESS (SYMBOLP (FLAVOR-METHOD-HASH-TABLE FL))
	;; Cause rehash now if necessary.
	(GETHASH NIL (DONT-OPTIMIZE (HASH-TABLE-INSTANCE (FLAVOR-METHOD-HASH-TABLE FL)))))
      (LET ((DEFAULT-CONS-AREA FLAVOR-DATA-AREA))
	(UNLESS (= (%AREA-NUMBER (FLAVOR-BINDINGS FL))
		   FLAVOR-DATA-AREA)
	  (SETF (FLAVOR-BINDINGS FL) (COPYLIST (FLAVOR-BINDINGS FL))))
	(UNLESS (= (%AREA-NUMBER (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST FL))
		   FLAVOR-DATA-AREA)
	  (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST FL)
		(COPYALIST (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST FL))))
	(UNLESS (= (%AREA-NUMBER (FLAVOR-ALL-INSTANCE-VARIABLES FL))
		   FLAVOR-DATA-AREA)
	  (SETF (FLAVOR-ALL-INSTANCE-VARIABLES FL)
		(COPYLIST (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	(UNLESS (= (%AREA-NUMBER (FLAVOR-MAPPED-INSTANCE-VARIABLES FL))
		   FLAVOR-DATA-AREA)
	  (SETF (FLAVOR-MAPPED-INSTANCE-VARIABLES FL)
		(COPYLIST (FLAVOR-MAPPED-INSTANCE-VARIABLES FL))))
	(UNLESS (= (%AREA-NUMBER (FLAVOR-PLIST FL))
		   FLAVOR-DATA-AREA)
	  (SETF (FLAVOR-PLIST FL) (COPYTREE (FLAVOR-PLIST FL)))))
      ;; In any case force transport of all these lists to newspace now.
      (NSUBST '(NIL) '(NIL) (FLAVOR-BINDINGS FL))
      (NSUBST '(NIL) '(NIL) (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST FL))
      (NSUBST '(NIL) '(NIL) (FLAVOR-ALL-INSTANCE-VARIABLES FL))
      (NSUBST '(NIL) '(NIL) (FLAVOR-MAPPED-INSTANCE-VARIABLES FL))
      (NSUBST '(NIL) '(NIL) (FLAVOR-PLIST FL)))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

;Call here when a flavor has been changed in a way that is not compatible
;with old instances of this flavor or its dependents.
;Arranges for those old instances to keep the old flavor structures and methods.
;Return new copy of the FLAVOR defstruct, and propagate to those that depend on it.
;Note that we tell copy-method-table to discard our combined methods.
;This is because they point to METHs in our method table,
;so we must make new combined methods that point at our new method table.
(DEFUN PERFORM-FLAVOR-REDEFINITION (FLAVOR-NAME &OPTIONAL FOR-UNDEFFLAVOR-P &AUX FL NFL)
  (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
  (COND ((FLAVOR-METHOD-HASH-TABLE FL)
	 (SETQ NFL (MAKE-FLAVOR))
	 (COPY-ARRAY-CONTENTS FL NFL)
	 (COPY-METHOD-TABLE FL NFL T)			   ;Copy, but discard combined methods
	 (SETQ FL NFL)
	 (SETF (FLAVOR-PLIST FL) (COPYLIST (FLAVOR-PLIST FL) PROPERTY-LIST-AREA))
	 (SETF (FLAVOR-MAPPED-INSTANCE-VARIABLES FL)
	       (COPYLIST (FLAVOR-MAPPED-INSTANCE-VARIABLES FL)))
	 (REMPROP (LOCF (FLAVOR-PLIST FL)) 'MAPPED-COMPONENT-FLAVORS)
							   ;They are used only by the combined
							   ;methods, which we just flushed.
	 (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST FL) NIL)
	 (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL) NIL)
	 (PUTPROP FLAVOR-NAME FL 'FLAVOR)
	 (FORMAT ERROR-OUTPUT
		 (IF FOR-UNDEFFLAVOR-P
		     "~&Flavor ~S no longer instantiable; old instances are not affected.~%"
		   "~&Flavor ~S changed incompatibly; old instances will not get the new version.~%")
		 FLAVOR-NAME))
	;; Even if this flavor wasn't instantiated,
	;; probably some of its dependents were,
	;; and their hash tables and combined methods point to our method table.
	(T (COPY-METHOD-TABLE FL FL T)))
  (SETF (FLAVOR-INSTANCE-SIZE FL) NIL)	;Defuse error check
  (SETF (FLAVOR-DEPENDS-ON-ALL FL) NIL)	;Will need to be flavor-composed again
  (SETF (FLAVOR-METHOD-HASH-TABLE FL) NIL)	;Will need to be method-composed again
  (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL)
  (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
    (PERFORM-FLAVOR-REDEFINITION FN FOR-UNDEFFLAVOR-P))
  FL)

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


;; Run-time alternative flavors.

(DEFUN GET-RUN-TIME-ALTERNATIVE-FLAVOR-NAMES (FLAVOR)
  (MAPCAR 'CDR (FLAVOR-GET FLAVOR 'RUN-TIME-ALTERNATIVE-ALIST)))

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-DEFFLAVORS (FLAVOR-NAME SPECS)
  "Return a list of defflavor forms for the run-time alternatives of FLAVOR-NAME.
These are the flavors generated automatically by defining FLAVOR-NAME
and one of which you get when you instantiate FLAVOR-NAME.
SPECS should be the value of the :RUN-TIME-ALTERNATIVES option in its definition;
this function can be called before the definition is really in effect."
  (LOOP FOR ALT IN (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR-NAME SPECS)
	WHEN (AND (NOT (MEMBER-IF 'STRINGP ALT))
		  (> (LENGTH ALT) 1))
	COLLECT `(DEFFLAVOR ,(INTERN (COMBINATION-FLAVOR-NAME ALT))
			    () ,ALT)))

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-ALIST (FLAVOR-NAME SPECS)
  (MAPCAR #'(LAMBDA (COMBINATION)
	      (CONS COMBINATION (INTERN (COMBINATION-FLAVOR-NAME COMBINATION))))
	  (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR-NAME SPECS)))

(DEFUN COMBINATION-FLAVOR-NAME (FLAVOR-LIST &AUX COMBINED-NAME)
  (DOLIST (NAME (REMOVE-DUPLICATES FLAVOR-LIST))
    (IF (STRING-EQUAL NAME "-FLAVOR" (- (STRING-LENGTH NAME) 7))
	(SETQ NAME (SUBSTRING NAME 0 (- (STRING-LENGTH NAME) 7))))
    (IF (STRING-EQUAL NAME "-MIXIN" (- (STRING-LENGTH NAME) 6))
	(SETQ NAME (SUBSTRING NAME 0 (- (STRING-LENGTH NAME) 6))))
    (IF COMBINED-NAME
	(SETQ COMBINED-NAME (STRING-APPEND COMBINED-NAME "-" NAME))
      (SETQ COMBINED-NAME NAME)))
  COMBINED-NAME)
  
(DEFUN MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS (FLAVOR)
  "Return a list of flavor combinations which are run-time alternatives of FLAVOR-NAME.
Each combination is a list of the flavor names to be combined."
  (LET ((SPECS (FLAVOR-GET FLAVOR ':RUN-TIME-ALTERNATIVES)))
    (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR SPECS)))

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 (FLAVOR-NAME SPECS)
  (IF (NULL SPECS)
      (IF FLAVOR-NAME `((,FLAVOR-NAME)) '(()))
    (LET ((REMAINING-SPECS-ALTERNATIVES
	    (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR-NAME (CDR SPECS)))
	  (THIS-SPEC-ALTERNATIVES (MAKE-RUN-TIME-ALTERNATIVES (CAR SPECS))))
      (LOOP FOR THIS-SPEC IN THIS-SPEC-ALTERNATIVES
	    NCONC (LOOP FOR REMAINING IN REMAINING-SPECS-ALTERNATIVES
			COLLECT (APPEND THIS-SPEC REMAINING))))))

(DEFUN MAKE-RUN-TIME-ALTERNATIVES (SPEC)
  (IF (CONSP (CADR SPEC))
      (LOOP FOR ALTERNATIVE IN (CDR SPEC)
	    APPEND (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1
		     (CADR ALTERNATIVE) (CDDR ALTERNATIVE)))
    `(NIL . ,(MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 (CADR SPEC) (CDDR SPEC)))))

;; Note that it is vital that the combination to be used
;; be consed up in the same order as the combination was made by
;; MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS, or it will not be recognized
;; in the RUN-TIME-ALTERNATIVE-ALIST.
(DEFUN CHOOSE-RUN-TIME-ALTERNATIVE (FLAVOR INIT-PLIST)
  "This is the :INSTANTIATION-FLAVOR-FUNCTION used for run-time alternative flavors."
  (LET* ((SPECS (FLAVOR-GET FLAVOR ':RUN-TIME-ALTERNATIVES))
	 (COMBINATION (CHOOSE-RUN-TIME-ALTERNATIVE-1 SPECS INIT-PLIST (FLAVOR-NAME FLAVOR))))
    (OR (CDR (ASSOC (APPEND COMBINATION
			    (LIST (FLAVOR-NAME FLAVOR)))
		    (FLAVOR-GET FLAVOR 'RUN-TIME-ALTERNATIVE-ALIST)))
	(IF (MEMBER-IF 'STRINGP COMBINATION)
	    (FERROR NIL (CAR (MEMBER-IF 'STRINGP COMBINATION)))
	  (FERROR NIL "Bug in :RUN-TIME-ALTERNATIVE processing:~%Flavor ~S, combination ~S."
		  FLAVOR COMBINATION)))))

(DEFUN CHOOSE-RUN-TIME-ALTERNATIVE-1 (SPECS INIT-PLIST FLAVOR-NAME)
  (LOOP FOR SPEC IN SPECS
	APPEND (CHOOSE-RUN-TIME-ALTERNATIVE-2 SPEC INIT-PLIST FLAVOR-NAME)))

(DEFUN CHOOSE-RUN-TIME-ALTERNATIVE-2 (SPEC INIT-PLIST FLAVOR-NAME)
  (LET ((VALUE (GET INIT-PLIST (CAR SPEC)))
	TEM)
    (IF (CONSP (CADR SPEC))
	(SETQ TEM (ASSQ VALUE (CDR SPEC)))
      (SELECTQ VALUE
	((T) (SETQ TEM SPEC))
	((NIL) (SETQ TEM '(FOO)))))
    (UNLESS TEM (FERROR NIL "Keyword ~S with value ~S is not legitimate for flavor ~S."
			(CAR SPEC) VALUE FLAVOR-NAME))
    (WHEN (STRINGP (CADR TEM))
      (FERROR NIL (CADR TEM) (CAR SPEC) VALUE FLAVOR-NAME))
    (LET ((SUBS (CHOOSE-RUN-TIME-ALTERNATIVE-1 (CDDR TEM) INIT-PLIST FLAVOR-NAME)))
      (IF (CADR TEM) (APPEND SUBS (LIST (CADR TEM))) SUBS))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

;Make an object of a particular flavor.
;If the flavor hasn't been composed yet, must do so now.
; Delaying it until the first time it is needed aids initialization,
; e.g. up until now we haven't depended on the depended-on flavors being defined yet.
;Note that INIT-PLIST can be modified, if the :DEFAULT-INIT-PLIST option was
; used or the init methods modify it.
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
	       (PUSH (CAR PL) UNHANDLED-KEYWORDS)))))
    ;; Now do all the default initializations, of one sort or other,
    ;; that have not been overridden.
    (DOLIST (D (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL))
      (OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ (CAR D))))
	  (SETF (%INSTANCE-REF INSTANCE (1+ (CAR D))) (EVAL (CADR D)))))
    ;; Now stick any default init plist items that aren't handled by that
    ;; onto the actual init plist.
    (DO ((PL (FLAVOR-REMAINING-DEFAULT-PLIST FL) (CDDR PL))) ((NULL PL))
      (OR (MEMQ-ALTERNATED (CAR PL) (CDR INIT-PLIST))
	  (progn
	    (unless (eq init-plist (locf new-plist))
	      (setq new-plist (cdr init-plist)
		    init-plist (locf new-plist)))
	    (setq new-plist (list* (car pl) (eval (cadr pl)) new-plist))))))
  ;; Complain if any keywords weren't handled, unless our caller
  ;; said it wanted to take care of this.
  (AND (NOT RETURN-UNHANDLED-KEYWORDS-P)
       UNHANDLED-KEYWORDS
       (FERROR NIL "Flavor ~S does not handle the init keyword~P ~{~S~^, ~}"
	       FLAVOR-NAME
	       (LENGTH UNHANDLED-KEYWORDS)
	       UNHANDLED-KEYWORDS))
  (AND SEND-INIT-MESSAGE-P
       (FUNCALL INSTANCE ':INIT INIT-PLIST))
  (PROG () (RETURN INSTANCE UNHANDLED-KEYWORDS)))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN DAEMON-COMBINATION (PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
			   &OPTIONAL OR-METHODS AND-METHODS)
  (LET ((INNER-CALL (AND PRIMARY-METHOD (METHOD-CALL PRIMARY-METHOD))))
    (AND OR-METHODS (SETQ INNER-CALL
			  `(OR ,@(MAPCAR 'METHOD-CALL OR-METHODS)
			       ,INNER-CALL)))
    (AND AND-METHODS (SETQ INNER-CALL
			   `(AND ,@(MAPCAR 'METHOD-CALL AND-METHODS)
				 ,INNER-CALL)))
    `(PROGN 
       ,@(MAPCAR 'METHOD-CALL BEFORE-METHODS)
       ,(IF AFTER-METHODS
	    `(MULTIPLE-VALUE-PROG1
	       ,INNER-CALL
	       . ,(MAPCAR 'METHOD-CALL AFTER-METHODS))
	  ;; You are allowed to not have a primary method
	  INNER-CALL))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN (:CASE METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET* ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
						   '(:CASE :OR :OTHERWISE :BEFORE :AFTER) T
						   ':BASE-FLAVOR-LAST)))
	 (OTHERWISE-METHOD
	   (OR (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':OTHERWISE T T
					 ':BASE-FLAVOR-LAST))
	       PRIMARY-METHOD))
	 (BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					      ':BASE-FLAVOR-LAST))
	 (AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					     ':BASE-FLAVOR-FIRST))
	 (OR-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':OR T T
					  ':BASE-FLAVOR-LAST))
	 (METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':CASE T T NIL)))
    ;; Remove shadowed :otherwise methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ ':OTHERWISE (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST OTHERWISE-METHOD))))
    ;; Remove shadowed primary methods too.
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (IF (EQ OTHERWISE-METHOD PRIMARY-METHOD)
	  (AND (CDDR MLE)
	       (SETF (CDR MLE) (LIST PRIMARY-METHOD)))
	;; If there is a :OTHERWISE method, all the primary ones are shadowed.
	(AND MLE (DELQ MLE MAGIC-LIST-ENTRY))))
    (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  (LET ((INNER-CALL
		  `(PROGN
		     ,@(MAPCAR 'METHOD-CALL BEFORE-METHODS)
		     (SELECTQ (CADR .DAEMON-CALLER-ARGS.)
		       ,@(MAPCAR #'(LAMBDA (METHOD)
				     `(,(FIFTH METHOD)
				       ,(METHOD-CALL METHOD)))
				 METHODS)
		       ((:GET-HANDLER-FOR :OPERATION-HANDLED-P :CASE-DOCUMENTATION)
			(LEXPR-FUNCALL 'CASE-METHOD-DEFAULT-HANDLER
				       ',(FLAVOR-NAME FL)
				       ',(CAR MAGIC-LIST-ENTRY)
				       ',METHODS
				       (CDR .DAEMON-CALLER-ARGS.)))
		       (:WHICH-OPERATIONS
			;; Do not use FIFTH here; can lose at cold-load time.
			',(MAPCAR #'(LAMBDA (X) (CAR (CDDDDR X))) METHODS))
		       (T (OR ,@(MAPCAR 'METHOD-CALL OR-METHODS)
			      ,(AND OTHERWISE-METHOD (METHOD-CALL OTHERWISE-METHOD))))))))
	    ;; Copied from DAEMON-COMBINATION.
	    (IF AFTER-METHODS
		`(MULTIPLE-VALUE-PROG1
		   ,INNER-CALL
		   . ,(MAPCAR 'METHOD-CALL AFTER-METHODS))
	      ;; No :AFTER methods, hair not required
	      ;; You are allowed to not have a primary method
	      INNER-CALL))))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

;; Cause the combined-methods to get compiled.
;; Executed only from the compiler, and does something
;; only if compiling to a file.
(DEFUN COMPILE-FLAVOR-METHODS-1 (FLAVOR-NAME &AUX FL)
  (IF (JUST-COMPILING)
      (LET ((*JUST-COMPILING* T)
	    (*USE-OLD-COMBINED-METHODS* NIL))
	(COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME 'COMPILE-FLAVOR-METHODS)
	       (SETQ FL (COMPILATION-FLAVOR FLAVOR-NAME))
	       ;; Make sure we are not hacking the installed flavor object,
	       ;; in case there is no defflavor or defmethod for the flavor in this file.
	       (AND (EQ FL (GET FLAVOR-NAME 'FLAVOR))
		    (COMPILATION-DEFINE-FLAVOR
		      FLAVOR-NAME
		      (SETQ FL (FLAVOR-REDEFINITION-FOR-COMPILATION FL NIL))))
	       (OR (FLAVOR-DEPENDS-ON-ALL FL)
		   (COMPOSE-FLAVOR-COMBINATION FL))
	       (COMPOSE-METHOD-COMBINATION FL NIL)
	       (DOLIST (ALTERNATIVE (GET-RUN-TIME-ALTERNATIVE-FLAVOR-NAMES FL))
		 (COMPILE-FLAVOR-METHODS-1 ALTERNATIVE)))))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

;; Do the composition now.  This should normally just generate data-structure
;; as the methods should already all have been compiled, unless something has changed.
(DEFPROP COMPILE-FLAVOR-METHODS-2 T QFASL-DONT-RECORD)

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN DEFFLAVOR1 (FLAVOR-NAME INSTANCE-VARIABLES COMPONENT-FLAVORS OPTIONS
		   &AUX FFL ALREADY-EXISTS INSTV IDENTICAL-COMPONENTS
			GETTABLE SETTABLE INITTABLE SPECIAL-IVS
			OLD-SPECIAL-IVS OLD-DEFAULT-HANDLER
			OLD-DEFAULT-INIT-PLIST OLD-LOCAL-IVS OLD-INITTABLE-IVS
			OLD-INIT-KWDS OLD-INSTANCE-AREA-FUNCTION
			OLD-REQUIRED-INIT-KEYWORDS
			INIT-KEYWORDS INCLUDES METH-COMB
			NEW-PLIST (PL (LOCF NEW-PLIST))
			(DEFAULT-CONS-AREA
			  (IF *JUST-COMPILING* DEFAULT-CONS-AREA
			    *FLAVOR-AREA*)))
  (OR *JUST-COMPILING* (RECORD-SOURCE-FILE-NAME FLAVOR-NAME 'DEFFLAVOR))
  (WITHOUT-INTERRUPTS
    (COND ((AND (NOT *JUST-COMPILING*)
		(NOT (MEMQ FLAVOR-NAME *ALL-FLAVOR-NAMES*)))
	   (PUSH FLAVOR-NAME *ALL-FLAVOR-NAMES*)
	   ;; Push on the name without the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (CONS (GET-PNAME FLAVOR-NAME) FLAVOR-NAME))
	   ;; Push on the name with the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (LET ((PACKAGE NIL))
				(CONS (FORMAT NIL "~S" FLAVOR-NAME) FLAVOR-NAME)))
	   ;; Array is no longer sorted.
	   (STORE-ARRAY-LEADER NIL *ALL-FLAVOR-NAMES-AARRAY* 1))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
	(FERROR NIL "~S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
	(FERROR NIL "~S, which is not a symbol, was specified as a component flavor" CF)))
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (COND ((AND (SETQ ALREADY-EXISTS (COMPILATION-FLAVOR FLAVOR-NAME))
	      *USE-OLD-FLAVOR-INFO*)
	 (DOLIST (PROP DEFFLAVOR1-PRESERVED-PROPERTIES)
	   (PUTPROP PL (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS)) PROP)
		    PROP))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (DO ((L OPTIONS (CDR L))
       (OPTION) (ARGS))
      ((NULL L))
    (IF (ATOM (CAR L))
	(SETQ OPTION (CAR L) ARGS NIL)
	(SETQ OPTION (CAAR L) ARGS (CDAR L)))
    (SELECTQ OPTION
	(:GETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ GETTABLE (UNION GETTABLE (OR ARGS INSTV))))
	(:SETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ SETTABLE (UNION SETTABLE (OR ARGS INSTV))))
	((:INITTABLE-INSTANCE-VARIABLES :INITABLE-INSTANCE-VARIABLES)
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ INITTABLE (UNION INITTABLE (OR ARGS INSTV))))
	(:SPECIAL-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ SPECIAL-IVS (UNION SPECIAL-IVS (OR ARGS INSTV))))
	(:INIT-KEYWORDS
	  (SETQ INIT-KEYWORDS (UNION INIT-KEYWORDS ARGS)))
	(:INCLUDED-FLAVORS
	  (SETQ INCLUDES (UNION INCLUDES ARGS)))
	(:NO-VANILLA-FLAVOR
	  (PUTPROP PL T OPTION))
	(:ORDERED-INSTANCE-VARIABLES
	  ;Don't validate.  User may reasonably want to specify non-local instance
	  ;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
	  ;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (OR ARGS INSTV) ':ORDERED-INSTANCE-VARIABLES))
	(:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (UNION (GET PL ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			     (OR ARGS INSTV))
		   ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
	(:METHOD-COMBINATION
	  (SETQ METH-COMB (NUNION-EQUAL METH-COMB ARGS)))
	(:DEFAULT-HANDLER
	  (PUTPROP PL (CAR ARGS) OPTION))
	((:REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS
	  :REQUIRED-FLAVORS :REQUIRED-INIT-KEYWORDS)
	  (PUTPROP PL (UNION ARGS (GET PL OPTION)) OPTION))
	((:DOCUMENTATION :DEFAULT-INIT-PLIST :SELECT-METHOD-ORDER :ACCESSOR-PREFIX)
	  (PUTPROP PL ARGS OPTION))
	(:ALIAS-FLAVOR
	 (PUTPROP PL T ':ALIAS-FLAVOR))
	(:ABSTRACT-FLAVOR
	 (PUTPROP PL T ':ABSTRACT-FLAVOR))
	(:INSTANCE-AREA-FUNCTION
	 (PUTPROP PL (CAR ARGS) ':INSTANCE-AREA-FUNCTION))
	(:INSTANTIATION-FLAVOR-FUNCTION
	 (PUTPROP PL (CAR ARGS) ':INSTANTIATION-FLAVOR-FUNCTION))
	((:RUN-TIME-ALTERNATIVES :MIXTURE)
	 (PUTPROP PL ARGS ':RUN-TIME-ALTERNATIVES)
	 (PUTPROP PL 'CHOOSE-RUN-TIME-ALTERNATIVE ':INSTANTIATION-FLAVOR-FUNCTION)
	 (PUTPROP PL (MAKE-RUN-TIME-ALTERNATIVE-ALIST FLAVOR-NAME ARGS)
		  'RUN-TIME-ALTERNATIVE-ALIST))
	(OTHERWISE (FERROR NIL "~S is not a known DEFFLAVOR option." OPTION))))
  ;; All settable instance variables should also be gettable and inittable.
  (DOLIST (V SETTABLE)
    (OR (MEMQ V GETTABLE)
	(PUSH V GETTABLE))
    (OR (MEMQ V INITTABLE)
	(PUSH V INITTABLE)))
  ;; See whether there are any changes in component flavor structure from last time
  (SETQ IDENTICAL-COMPONENTS
	(AND ALREADY-EXISTS
	     *USE-OLD-FLAVOR-INFO*
	     (EQUAL COMPONENT-FLAVORS (FLAVOR-DEPENDS-ON ALREADY-EXISTS))
	     (EQUAL INCLUDES (FLAVOR-INCLUDES ALREADY-EXISTS))
	     (EQUAL (GET PL ':REQUIRED-FLAVORS)
		    (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS)) ':REQUIRED-FLAVORS))))	
  (AND ALREADY-EXISTS
       (SETQ OLD-SPECIAL-IVS (FLAVOR-SPECIAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-DEFAULT-HANDLER (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
				      ':DEFAULT-HANDLER)
	     OLD-DEFAULT-INIT-PLIST (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
					 ':DEFAULT-INIT-PLIST)
	     OLD-LOCAL-IVS (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INITTABLE-IVS (FLAVOR-INITTABLE-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET ALREADY-EXISTS ':INSTANCE-AREA-FUNCTION)
	     OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET ALREADY-EXISTS ':REQUIRED-INIT-KEYWORDS)
	     OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS ALREADY-EXISTS)))
  ;; If the flavor is being redefined, and the number or order of instance variables
  ;; is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give a warning
  ;; and disconnect from the old FLAVOR defstruct so that old instances will
  ;; retain the old information.  The instance variables can get changed either
  ;; locally or by rearrangement of the component flavors.
  (AND ALREADY-EXISTS
       (IF (AND *USE-OLD-FLAVOR-INFO*
		(EQUAL (GET PL ':ORDERED-INSTANCE-VARIABLES)
		       (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
			    ':ORDERED-INSTANCE-VARIABLES))
		(OR (EQUAL (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
			   INSTANCE-VARIABLES)
		    (EQUAL (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
				   (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS))
			   INSTV))
		(EQ (GET PL ':ALIAS-FLAVOR)
		    (FLAVOR-GET ALREADY-EXISTS ':ALIAS-FLAVOR))
		(OR IDENTICAL-COMPONENTS
		    (EQUAL (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       COMPONENT-FLAVORS INCLUDES)
			   (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       (FLAVOR-DEPENDS-ON ALREADY-EXISTS)
						       (FLAVOR-INCLUDES ALREADY-EXISTS)))))
	   (IF *JUST-COMPILING*
	       (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS NIL)))
	 (IF *JUST-COMPILING*
	     (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS T))
	   (SETQ ALREADY-EXISTS (PERFORM-FLAVOR-REDEFINITION FLAVOR-NAME)))))
  (WHEN (GET PL ':ALIAS-FLAVOR)
    (IF (CDR COMPONENT-FLAVORS)
	(FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		     "This alias flavor has more than one component."))
    (UNLESS COMPONENT-FLAVORS
      (FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		   "This alias flavor has no component to be the alias of."))
    (IF INSTANCE-VARIABLES
	(FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		     "This alias flavor has instance variables; they will be ignored.")))
  ;; Make the information structure unless the flavor already exists.
  (LET ((FL (OR ALREADY-EXISTS
		(AND (NOT *JUST-COMPILING*)
		     (GET FLAVOR-NAME 'UNDEFINED-FLAVOR))
		(MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))
    (SETF (FLAVOR-PACKAGE FL) PACKAGE)
    (SETF (FLAVOR-LOCAL-INSTANCE-VARIABLES FL) INSTANCE-VARIABLES)
    (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
    (LET ((OVEC (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL)))
      (SETF (FLAVOR-PLIST FL) NEW-PLIST)
      (IF OVEC (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL) OVEC)))
    (IF GETTABLE
	(SETF (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL) GETTABLE))
    (IF SETTABLE
	(SETF (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL) SETTABLE))
    (IF SPECIAL-IVS
	(SETF (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL) SPECIAL-IVS))
    (SETF (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL)
	  (LOOP FOR V IN INITTABLE COLLECT (CONS (CORRESPONDING-KEYWORD V) V)))
    (SETF (FLAVOR-INIT-KEYWORDS FL) INIT-KEYWORDS)
    (SETF (FLAVOR-INCLUDES FL) INCLUDES)
    ;; This can't be computed for real until flavor composition,
    ;; but this at least contains some of the right ones.
    (SETF (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FL)
	  (FLAVOR-KNOWN-UNMAPPED-INSTANCE-VARIABLES FL))
    ;; First remove old method-combination declarations, then add new ones
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
      (COND ((LOOP FOR DECL IN METH-COMB NEVER (MEMQ (CAR MTE) (CDDR DECL)))
	     (SETF (SECOND MTE) NIL)
	     (SETF (THIRD MTE) NIL))))
    (DOLIST (DECL METH-COMB)
      (LET ((TYPE (CAR DECL)) (ORDER (CADR DECL)) ELEM)
	;; Don't error-check TYPE now, its definition might not be loaded yet
	(DOLIST (MSG (CDDR DECL))
	  (OR (SETQ ELEM (ASSQ MSG (FLAVOR-METHOD-TABLE FL)))
	      (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
	  (SETF (SECOND ELEM) TYPE)
	  (SETF (THIRD ELEM) ORDER))))
    (IF *JUST-COMPILING*
	(COMPILATION-DEFINE-FLAVOR FLAVOR-NAME FL)
      ;; Make this a depended-on-by of its depends-on, or remember to do it later in
      ;; the case of depends-on's not yet defined.
      (DOLIST (COMPONENT-FLAVOR COMPONENT-FLAVORS)
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ FFL (GET COMPONENT-FLAVOR 'FLAVOR))
		 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
		(T (PUSH (CONS COMPONENT-FLAVOR FLAVOR-NAME)
			 *FLAVOR-PENDING-DEPENDS*)))))
      ;; Likewise for its includes
      (DOLIST (INCLUDED-FLAVOR (FLAVOR-INCLUDES FL))
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ FFL (GET INCLUDED-FLAVOR 'FLAVOR))
		 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
		(T (PUSH (CONS INCLUDED-FLAVOR FLAVOR-NAME)
			 *FLAVOR-PENDING-DEPENDS*)))))
      ;; If someone depends on this flavor, which wasn't defined until now, link them up.
      ;; If that flavor was flavor-composed, recompose it now.
      (WITHOUT-INTERRUPTS
	(DOLIST (X *FLAVOR-PENDING-DEPENDS*)
	  (COND ((EQ (CAR X) FLAVOR-NAME)
		 (OR (MEMQ (CDR X) (FLAVOR-DEPENDED-ON-BY FL))
		     (PUSH (CDR X) (FLAVOR-DEPENDED-ON-BY FL)))
		 (SETQ *FLAVOR-PENDING-DEPENDS*
		       (DELQ X *FLAVOR-PENDING-DEPENDS*))))))
      (PUTPROP FLAVOR-NAME FL 'FLAVOR)
      (REMPROP FLAVOR-NAME 'UNDEFINED-FLAVOR)
      ;; Now, if the flavor was redefined in a way that changes the methods but doesn't
      ;; invalidate old instances, we have to propagate some changes.
      (IF (AND ALREADY-EXISTS
	       (NOT IDENTICAL-COMPONENTS))
	  (PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FLAVOR-NAME)
	;; If the methods and instances are ok but other things have changed, notice that too.
	(OR (AND (EQUAL OLD-SPECIAL-IVS
			(FLAVOR-SPECIAL-INSTANCE-VARIABLES FL))
		 (EQUAL OLD-DEFAULT-INIT-PLIST
			(GET (LOCF (FLAVOR-PLIST FL))
			     ':DEFAULT-INIT-PLIST))
		 (EQUAL OLD-LOCAL-IVS
			(FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
		 ;; Get a warning every time, if there is a variable
		 ;; that is globally special but not in a :SPECIAL-INSTANCE-VARIABLES
		 (NOT (DOLIST (IV (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
			;; Elements can be lists (var init)
			(IF (CONSP IV) (SETQ IV (CAR IV)))
			(AND (GET IV 'SPECIAL)
			     (NOT (MEMQ IV (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL)))
			     (RETURN T))))
		 (EQUAL OLD-INITTABLE-IVS
			(FLAVOR-INITTABLE-INSTANCE-VARIABLES FL))
		 (EQUAL OLD-DEFAULT-HANDLER (GET (LOCF (FLAVOR-PLIST FL)) ':DEFAULT-HANDLER))
		 (EQUAL OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET FL ':INSTANCE-AREA-FUNCTION))
		 (EQUAL OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET FL ':REQUIRED-INIT-KEYWORDS))
		 (EQUAL OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS FL)))
	    (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR-NAME))))
    FLAVOR-NAME))

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

;;; This makes the redisplay mechanism forget everything it knows.
(DEFMETHOD (ZWEI :AFTER :REFRESH) (&OPTIONAL TYPE)
  (WHEN (OR (NOT TV:RESTORED-BITS-P) (EQ TYPE ':SIZE-CHANGED))
    (WHEN TV:EXPOSED-P
      (PREPARE-WINDOW-FOR-REDISPLAY SELF))  ;Pop down any typeout window.
    (TELL-EDITOR-TO-REDISPLAY DIS-ALL)))

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "


;;; Two window stuff, takes two windows (structures, not sheets)
;;; and makes them share the area originally occupied by the first of the two.
(DEFUN TWO-WINDOWS (ZWEI-WINDOW-1 ZWEI-WINDOW-2)
  (REDISPLAY ZWEI-WINDOW-1 ':NONE)
  (LET ((W1 (WINDOW-SHEET ZWEI-WINDOW-1))
	(W2 (WINDOW-SHEET ZWEI-WINDOW-2))
	(FRAME (WINDOW-FRAME ZWEI-WINDOW-1)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
	(FUNCALL FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
      (TV:PRESERVE-SUBSTITUTE-STATUS (SEND W1 ':SUPERIOR)
	(TV:DELAYING-SCREEN-MANAGEMENT
	  (FUNCALL W1 ':DEEXPOSE)
	  (FUNCALL W2 ':DEEXPOSE)
	  (LET ((HEIGHT (TRUNCATE (- BOTTOM TOP) 2)))
	    (FUNCALL W1 ':SET-EDGES LEFT TOP RIGHT (+ TOP HEIGHT))
	    (FUNCALL W2 ':SET-EDGES LEFT (+ TOP HEIGHT) RIGHT BOTTOM))
	  (SEND W1 ':SET-LABEL NIL)
	  (SEND W2 ':SET-LABEL NIL)
	  (FUNCALL W1 ':EXPOSE NIL ':CLEAN)		;Make sure they are both there
	  (FUNCALL W2 ':EXPOSE NIL ':CLEAN))))
    (FUNCALL FRAME ':UPDATE-LABELS)))

(DEFCOM COM-ONE-WINDOW "Go back to one-window mode.
Causes one of the editor windows in this frame to fill the whole frame.
With a numeric arg, the current window (where the cursor is) is always used.
Otherwise, it is controlled by the value of ZWEI:*ONE-WINDOW-DEFAULT*.
It can be :TOP (select the uppermost window), :BOTTOM (the lowermost),
:CURRENT (keep only the window the cursor is now in),
or :OTHER (keep the uppermost window other than the one the cursor is in).
The default is :CURRENT.
Exception: if there is no arg, windows in Warnings mode will never be used." ()
  (LET ((WINDOWS (FRAME-EXPOSED-WINDOWS))
	WINDOW)
    (UNLESS (CDR WINDOWS) (BARF "You are already in one window mode."))
    (SETQ WINDOW
	  (IF *NUMERIC-ARG-P* *WINDOW*
	    (SEND
	      (SELECTQ *ONE-WINDOW-DEFAULT*
		(:TOP (FIRST WINDOWS))
		(:BOTTOM (CAR (LAST WINDOWS)))
		(:CURRENT (WINDOW-SHEET *WINDOW*))
		(:OTHER (CAR (MEM 'NEQ (WINDOW-SHEET *WINDOW*) WINDOWS))))
	      ':ZWEI-WINDOW)))
    ;; Don't default to a window that is in Warnings mode.
    (AND (NOT *NUMERIC-ARG-P*)
	 (EQ (BUFFER-SAVED-MAJOR-MODE (WINDOW-INTERVAL *WINDOW*)) 'WARNINGS-MODE)
	 (SETQ WINDOW
	       (SEND (CAR (MEM 'NEQ (WINDOW-SHEET *WINDOW*) WINDOWS)) ':ZWEI-WINDOW)))
    (MAKE-WINDOW-FULL-SCREEN WINDOW))
  DIS-NONE)
                                               
(DEFCOM COM-OTHER-WINDOW "Move to the other window.
If there are several windows, go through them all in cyclic order.
A numeric argument specifies the window to go to, counting from 1 at the top." ()
  (LET ((WINDOW (OTHER-WINDOW)))
    (IF *NUMERIC-ARG-P*
	(SELECT-NUMBERED-WINDOW *NUMERIC-ARG*)
      (IF WINDOW
	  (MAKE-WINDOW-CURRENT WINDOW)
	(MULTIPLE-VALUE (NIL WINDOW)
	  (FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS))
	(OR (WINDOW-INTERVAL WINDOW) (BARF "Only one window"))
	(MAKE-WINDOW-FULL-SCREEN WINDOW))))
  DIS-BPS)

(DEFUN MAKE-WINDOW-FULL-SCREEN (WINDOW &AUX FRAME LEFT TOP RIGHT BOTTOM)
  (SETQ FRAME (WINDOW-FRAME WINDOW))
  (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
    (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW))
  (TV:PRESERVE-SUBSTITUTE-STATUS FRAME
    (TV:DELAYING-SCREEN-MANAGEMENT
      (SEND WINDOW ':DEEXPOSE)
      (SEND WINDOW ':SET-EDGES LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW ':SET-LABEL NIL)
      (SEND WINDOW ':EXPOSE NIL ':CLEAN)
      (SEND FRAME ':UPDATE-LABELS)
      (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
      (OR (EQ WINDOW *WINDOW*) (MAKE-WINDOW-CURRENT WINDOW)))))

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
			  (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':LIST '(NIL) T NIL)
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
						     T NIL)
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
					      T NIL)
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
					      T (CAADDR MAGIC-LIST-ENTRY))
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

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-STRING-OUT-EXPLICIT-1 (SHEET STRING START-X Y XLIM YLIM FONT ALU
				    &OPTIONAL (START 0) (END NIL)
				    MULTI-LINE-LINE-HEIGHT
				    &AUX FIT FWT LKT
				    (X START-X))
  "Output STRING on SHEET without using SHEET's cursor, font, etc.
Output starts at cursor position START-X, Y but SHEET's cursor is not moved.
Output stops if x-position XLIM or y-position YLIM is reached.
Font FONT is used, and alu-function ALU.
START and END specify a portion of STRING to be used.
MULTI-LINE-LINE-HEIGHT is how far to move down for Return characters;
 Return also moves back to x-position START-X.
 NIL means output <Return> with a lozenge.
All position arguments are relative to SHEET's outside edges."
  (DECLARE (RETURN-LIST FINAL-X FINAL-INDEX FINAL-Y))
  (COERCE-FONT FONT SHEET)
  (SETQ FIT (FONT-INDEXING-TABLE FONT)
	FWT (FONT-CHAR-WIDTH-TABLE FONT)
	LKT (FONT-LEFT-KERN-TABLE FONT))
  (OR XLIM (SETQ XLIM (SHEET-WIDTH SHEET)))
  (PREPARE-SHEET (SHEET)
    (DO ((I START (1+ I))
	 (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	 (WIDTH (FONT-CHAR-WIDTH FONT))
	 (CH))
	(( I N) (VALUES X Y I))
      (SETQ CH (AREF STRING I))
      (COND ((AND MULTI-LINE-LINE-HEIGHT (= CH #\RETURN))
	     (SETQ X START-X Y (+ Y MULTI-LINE-LINE-HEIGHT))
	     (IF (AND YLIM (> (+ Y MULTI-LINE-LINE-HEIGHT) YLIM))
		 (RETURN X Y I)))
	    (( CH 200)
	     (LET* ((STRING (STRING (OR (CAR (RASSOC CH SI:XR-SPECIAL-CHARACTER-NAMES))
					(FORMAT NIL "~O" CH))))
		    (NX (+ X (LOZENGED-STRING-WIDTH STRING))))
	       (IF (> NX XLIM) (RETURN X Y I))
	       (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL SHEET STRING
						       X (1+ Y) XLIM ALU)
	       (SETQ X NX)))
	    (T (IF FWT (SETQ WIDTH (AREF FWT CH)))
	       (IF (> (+ X WIDTH) XLIM) (RETURN X Y I))
	       (DRAW-CHAR FONT CH
			  (IF LKT (- X (AREF LKT CH)) X)
			  Y ALU SHEET)
	       (SETQ X (+ X WIDTH)))))))

))

; From file ZMNEW.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "


(DEFUN CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES (PLIST BUFFER)
  (MULTIPLE-VALUE-BIND (NIL PARSING-ERROR)
      (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM BUFFER))
    (IF PARSING-ERROR
	(PROGN
	  (FORMAT QUERY-IO "~&Invalid syntax in the -*- line of buffer ~A." BUFFER)
	  NIL)
      (WHEN (AND *UPDATE-PLIST-ON-WRITE-OK*
		 (EQ ':LISP (GET (LOCF PLIST) ':MODE))
		 (BUFFER-PATHNAME BUFFER)
		 (NULL (GET (LOCF PLIST) ':BASE))
		 ;; Don't mess with init files, since they may SETQ BASE.
		 (NEQ (SEND (BUFFER-PATHNAME BUFFER) ':CANONICAL-TYPE) ':INIT)
		 (NOT (SEND BUFFER ':GET-ATTRIBUTE ':NO-BASE-ATTRIBUTE)))
	;; insert a base attribute.
	(FORMAT QUERY-IO "~&Updating base attribute of ~A to ~D.~%" BUFFER BASE)
	(STORE-ATTRIBUTE-LIST BUFFER (SETQ PLIST (APPEND `(:BASE ,BASE) PLIST)))
	T))))

(DEFUN STORE-ATTRIBUTE-LIST (BUFFER PLIST)
  "Modify the attribute list in BUFFER's text to correspond to PLIST.
However, the buffer's current major mode is always recorded
rather than anything PLIST says."
  (LET ((START-BP (INTERVAL-FIRST-BP BUFFER))
	OLD-ATTRIBUTES PARSING-ERROR
	OLD-ATTRIBUTE-NAMES
	(*INTERVAL* BUFFER)
	LINE ALIST)
    (DECLARE (SPECIAL OLD-ATTRIBUTE-NAMES))
    (SETF (VALUES OLD-ATTRIBUTES PARSING-ERROR)
	  (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM BUFFER)))
    (WHEN PARSING-ERROR
      (FERROR NIL "Invalid syntax in the -*- line in buffer ~A." BUFFER))
    ;; Turn the plist into an alist with elements (propname-string value-string)
    (DO ((PS PLIST (CDDR PS))
	 (BASE 10.) (IBASE 10.) (*NOPOINT T))
	((NULL PS))
      (PUSH (LIST (GET-PNAME (CAR PS))
		  (FORMAT:OUTPUT NIL (PRINC (CADR PS))))
	    ALIST))
    ;; Get a list of names of attributes in order they appear in the -*- line now.
    (DO ((PS OLD-ATTRIBUTES (CDDR PS))) ((NULL PS))
      (PUSH (GET-PNAME (CAR PS)) OLD-ATTRIBUTE-NAMES))
    (SETQ OLD-ATTRIBUTE-NAMES (NREVERSE OLD-ATTRIBUTE-NAMES))
    ;; Sort the new ones into the same order.
    ;; All new ones come after all old; new ones are alphabetized.
    (SORTCAR ALIST #'(LAMBDA (AT1 AT2)
		       (LET ((TEM1 (SYS:MEMBER-EQUALP AT1 OLD-ATTRIBUTE-NAMES))
			     (TEM2 (SYS:MEMBER-EQUALP AT2 OLD-ATTRIBUTE-NAMES)))
			 (IF (AND TEM1 TEM2)
			     (SYS:MEMBER-EQUALP AT1 TEM1)
			   (OR TEM1
			       (AND (NOT TEM2)
				    (STRING-LESSP AT1 AT2)))))))
    (LET (TEM)
      ;; Put the package at the front.
      (IF (SETQ TEM (ASS 'EQUALP "PACKAGE" ALIST))
	  (SETQ ALIST (CONS TEM (DELQ TEM ALIST))))
      ;; Ignore what PLIST says for the :MODE.
      (IF (SETQ TEM (ASS 'EQUALP "MODE" ALIST))
	  (SETQ ALIST (DELQ TEM ALIST)))
      ;; Put the buffer's actual mode on, at the very front.
      (PUSH (LIST "MODE" (SYMEVAL (SEND BUFFER ':MAJOR-MODE))) ALIST))
    (SETQ LINE (BP-LINE START-BP))
    (LET (IDX END-BP)
      (COND ((SETQ IDX (STRING-SEARCH "-*-" LINE))
	     ;; Put on a comment starter if there isn't one already.
	     (WHEN *COMMENT-START*
	       (LET ((START-START (FIND-COMMENT-START LINE)))
		 (UNLESS (AND START-START (< START-START IDX))
		   (SETQ IDX
			 (BP-INDEX
			   (INSERT-MOVING (BACKWARD-OVER *BLANKS*
							 (CREATE-BP LINE IDX))
					  (IF (EQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
					      ";;; "
					    *COMMENT-BEGIN*))))
		   (WHEN (AND *COMMENT-END* (PLUSP (LENGTH *COMMENT-END*)))
		     (INSERT (END-OF-LINE LINE) *COMMENT-END*)))))
	     (SETQ START-BP (CREATE-BP LINE (SETQ IDX (+ IDX 3))))
	     (SETQ IDX (STRING-SEARCH "-*-" LINE IDX))
	     (IF IDX
		 (SETQ END-BP (CREATE-BP LINE IDX))
	       (SETQ END-BP (END-LINE LINE))
	       (INSERT END-BP " -*-"))
	     (DELETE-INTERVAL START-BP END-BP)
	     (SETQ START-BP (INSERT START-BP " "))
	     (INSERT START-BP " "))
	    (T
	     ;;special kludge for tex, since start of line 1 is sacred.
             (COND ((EQ *MAJOR-MODE* 'TEX-MODE)
		    (SETQ START-BP (INSERT (CREATE-BP LINE (LINE-LENGTH LINE))
					   "  % ")))
		   ((EQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
		    (SETQ START-BP (INSERT START-BP ";;; ")))
		   (*COMMENT-START* (SETQ START-BP (INSERT START-BP *COMMENT-BEGIN*))))
	     (SETQ START-BP (INSERT START-BP "-*- "))
	     (SETQ END-BP (INSERT START-BP " -*-"))
	     (COND ((NOT (MEMBER *COMMENT-END* '(NIL "")))
		    (INSERT-MOVING END-BP #\SP)
		    (INSERT-MOVING END-BP *COMMENT-END*)))
	     (OR (EQ *MAJOR-MODE* 'TEX-MODE) (INSERT END-BP #\CR)))))
    (DO LIST ALIST (CDR LIST) (NULL LIST)
	(LET ((BP1 (COPY-BP START-BP)))
	  (INSERT-MOVING START-BP (STRING-DOWNCASE (CAAR LIST)))
	  (UPCASE-INTERVAL BP1 (FORWARD-CHAR BP1)))
	(INSERT-MOVING START-BP #/:)
	(INSERT-MOVING START-BP (CADAR LIST))
	(AND (OR (CDR LIST))		;If more to come or some there already
	     (INSERT-MOVING START-BP "; ")))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN EXTRACT-ATTRIBUTE-LIST (STREAM &AUX WO PLIST PATH MODE ERROR)
  "Return the attribute list read from STREAM.
STREAM can be reading either a text file or a QFASL file."
  (SETQ WO (FUNCALL STREAM ':WHICH-OPERATIONS))
  (COND ((MEMQ ':SYNTAX-PLIST WO)
	 (SETQ PLIST (FUNCALL STREAM ':SYNTAX-PLIST)))
	((NOT (SEND STREAM ':CHARACTERS))
	 (SETQ PLIST (SI:QFASL-STREAM-PROPERTY-LIST STREAM)))
	;; If the file supports :READ-INPUT-BUFFER, check for absense of a plist
	;; without risk that :LINE-IN will read the whole file
	;; if the file contains no Return characters.
	((AND (MEMQ ':READ-INPUT-BUFFER WO)
	      (MULTIPLE-VALUE-BIND (BUFFER START END)
		  (FUNCALL STREAM ':READ-INPUT-BUFFER)
		(AND BUFFER
		     (NOT (STRING-SEARCH "-*-" BUFFER START END)))))
	 NIL)
	;; If stream does not support :SET-POINTER, there is no hope
	;; of parsing a plist, so give up on it.
	((NOT (MEMQ ':SET-POINTER WO))
	 NIL)
	(T (DO ((LINE) (EOF)) (NIL)
	     (MULTIPLE-VALUE (LINE EOF) (FUNCALL STREAM ':LINE-IN NIL))
	     (COND ((NULL LINE)
		    (FUNCALL STREAM ':SET-POINTER 0)
		    (RETURN NIL))
		   ((STRING-SEARCH "-*-" LINE)
		    (SETQ LINE (FILE-GRAB-WHOLE-PROPERTY-LIST LINE STREAM))
		    (FUNCALL STREAM ':SET-POINTER 0)
		    (SETF (VALUES PLIST ERROR) (FILE-PARSE-PROPERTY-LIST LINE))
		    (RETURN NIL))
		   ((OR EOF (STRING-SEARCH-NOT-SET '(#\SP #\TAB) LINE))
		    (FUNCALL STREAM ':SET-POINTER 0)
		    (RETURN NIL))))))
  (AND (NOT (GET (LOCF PLIST) ':MODE))
       (MEMQ ':PATHNAME WO)
       (SETQ PATH (FUNCALL STREAM ':PATHNAME))
       (SETQ MODE (CDR (ASSOC (FUNCALL PATH ':TYPE) *FILE-TYPE-MODE-ALIST*)))
       (PUTPROP (LOCF PLIST) MODE ':MODE))
  (VALUES PLIST ERROR))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

;;; This takes a string which probably has a property list in it, and returns the plist.
;;; If it has any trouble parsing, returns whatever plist it could find.
(DEFUN FILE-PARSE-PROPERTY-LIST (STRING &OPTIONAL (START 0) (END (ARRAY-ACTIVE-LENGTH STRING))
				 &AUX PLIST (IBASE 10.) (PACKAGE SI:PKG-KEYWORD-PACKAGE)
				 ERROR)
  (AND STRING
       (ARRAYP STRING)
       (= (ARRAY-ELEMENT-SIZE STRING) 8)
       ;; Narrow down to the stuff between the -*-'s
       (SETQ START (STRING-SEARCH "-*-" STRING START END))
       (SETQ END (STRING-SEARCH "-*-" STRING (SETQ START (+ START 3)) END))
       ;; Now parse it.
       (IF (NOT (%STRING-SEARCH-CHAR #/: STRING START END))
	   (SETQ PLIST (LIST ':MODE (READ-FROM-SUBSTRING STRING START END)))
	 (DO ((S START (1+ SEMI-IDX))
	      (COLON-IDX) (SEMI-IDX) (SYM) (ELEMENT NIL NIL) (DONE)
	      (WIN-THIS-TIME NIL NIL))
	     (NIL)
	   (OR (SETQ SEMI-IDX (%STRING-SEARCH-CHAR #/; STRING S END))
	       (SETQ DONE T SEMI-IDX END))
	   (OR (SETQ COLON-IDX (%STRING-SEARCH-CHAR #/: STRING S SEMI-IDX))
	       (RETURN NIL))
	   (IGNORE-ERRORS
	     (OR (SETQ SYM (READ-FROM-SUBSTRING STRING S COLON-IDX))
		 (RETURN NIL))
	     (IGNORE-ERRORS
	       (IF (%STRING-SEARCH-CHAR #/, STRING (SETQ S (1+ COLON-IDX)) SEMI-IDX)
		   (DO ((COMMA-IDX) (ELEMENT-DONE))
		       (NIL)
		     (OR (SETQ COMMA-IDX (%STRING-SEARCH-CHAR #/, STRING S SEMI-IDX))
			 (SETQ ELEMENT-DONE T COMMA-IDX SEMI-IDX))
		     (SETQ ELEMENT
			   (NCONC ELEMENT
				  (NCONS (READ-FROM-SUBSTRING STRING S COMMA-IDX))))
		     (AND ELEMENT-DONE (RETURN NIL))
		     (SETQ S (1+ COMMA-IDX)))
		 (SETQ ELEMENT (READ-FROM-SUBSTRING STRING S SEMI-IDX)))
	       (SETQ WIN-THIS-TIME T))
	     (SETQ PLIST (NCONC PLIST (LIST* SYM ELEMENT NIL))))	;Nicer CDR-CODEs
	   (UNLESS WIN-THIS-TIME
	     (SETQ ERROR T))
	   (AND DONE (RETURN NIL)))))
  (VALUES PLIST ERROR))

))
