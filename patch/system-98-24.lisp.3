;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.24
;;; Reason: Compilation of (SETQ X X) when value needed.
;;; WITH-LOCK when lock is the cdr of a cell.
;;; Compilation out of ZMail draft messages.
;;; DEFSYSTEM recording system pathnames bug.
;;; (MAKE-SYSTEM foo :NO-INCREMENT-PATCH :COMPILE) bug.
;;; Add Patch Buffer Changed Functions printout bug.
;;; Spurious compiler warnings from DEFMETHOD bug.
;;; Written 1/03/84 01:38:19 by RMS,
;;; while running on Lisp Machine Eighteen from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.22, CADR 3.4, ZMail 53.5, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1SETQ (FORM)
  (LET ((DO-THE-SETS
	  (CONS 'SETQ (P1SETQ-1 (CDR FORM)))))
    (IF P1VALUE
	;; If the last pair is of the form X X and was ignored,
	;; but we need X as the value of the SETQ form,
	;; put (PROGN ... X) around the actual variable setting.
	(DO ((TAIL (CDR FORM) (CDDR TAIL)))
	    ((NULL TAIL)
	     DO-THE-SETS)
	  (AND (CDR TAIL) (NULL (CDDR TAIL))
	       (EQ (CAR TAIL) (CADR TAIL))
	       (RETURN `(PROGN ,DO-THE-SETS ,(P1V (CAR TAIL))))))
      DO-THE-SETS)))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO WITH-LOCK ((LOCATOR . OPTIONS) &BODY BODY &AUX NORECURSIVE NOERROR)
  "Execute the BODY with a lock locked.
LOCATOR is an expression whose value is the lock status;
it should be suitable for use inside LOCF.
OPTIONS include :NORECURSIVE, do not allow locking a lock already locked by this process."
  ;; Ignore the old :NOERROR option -- it's always that way now.
  (KEYWORD-EXTRACT OPTIONS O () (NORECURSIVE NOERROR) (OTHERWISE NIL))
  `(LET* ((POINTER (LOCF ,LOCATOR))
	  (ALREADY-MINE (EQ (CAR POINTER) CURRENT-PROCESS)))
     (IF (CONSP POINTER)
	 (SETQ POINTER (CDR-LOCATION-FORCE POINTER)))
     (UNWIND-PROTECT
	 (PROGN (IF ALREADY-MINE
		    ,(IF NORECURSIVE `(FERROR NIL "Attempt to lock ~S recursively." ',LOCATOR))
		  ;; Redundant, but saves time if not locked.
		  (OR (%STORE-CONDITIONAL POINTER NIL CURRENT-PROCESS)
		      (PROCESS-LOCK POINTER)))
		. ,BODY)
       (UNLESS ALREADY-MINE
	 (%STORE-CONDITIONAL POINTER CURRENT-PROCESS NIL)))))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN UPDATE-INTERVAL-COMPILE-TICK (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Update the tick-of-last-compilation for all sections in an interval.
Pass either an interval or a pair of BPs."
  (TICK)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((NODE (BP-NODE BP1) (NODE-NEXT NODE))
       (FIRST T NIL)
       TEM)
      ((OR (NULL NODE)
	   (NOT (OR FIRST (BP-< (INTERVAL-FIRST-BP NODE) BP2)))))
    (WHEN (OR (NOT FIRST)
	      ;; If compiled or evaluated only part of the text in a node,
	      ;; don't set its compile tick.
	      ;; Now that there is only one form per section,
	      ;; we can be confident that if the compiled code
	      ;; started at the beginning of the form,
	      ;; it must have reached the end,
	      ;; unless either the compilation bombed out from unmatched parens
	      ;; or the section contains unmatched parens.
	      (EQ (BP-LINE BP1) (BP-LINE (INTERVAL-FIRST-BP NODE)))
	      (AND (SETQ TEM (SEND NODE ':SEND-IF-HANDLES ':DEFUN-LINE))
		   (BP-< BP1 (CREATE-BP TEM 1))))
      (SEND NODE ':UPDATE-COMPILE-TICK))))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN RECORD-SYSTEM-NAME-IN-PATHNAMES (NAME SYSTEM)
  "Record a system's name in all its files' pathnames.
NAME should be the system name and SYSTEM should be the system object."
  (LET ((DBFT (GET (LOCF (SYSTEM-PLIST SYSTEM)) 'DEFAULT-BINARY-FILE-TYPE)))
    (MAPC #'(LAMBDA (PATHNAME)
	      (LET* ((GENERIC-PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME))
		     (SYSTEMS (FUNCALL GENERIC-PATHNAME ':GET ':SYSTEMS)))
		;;if it already has a :SYSTEMS property, push onto it.
		(OR (MEMQ NAME SYSTEMS)
		    (FUNCALL GENERIC-PATHNAME
			     ':PUTPROP (CONS NAME SYSTEMS)
			     ':SYSTEMS))
		(IF DBFT
		  (FUNCALL GENERIC-PATHNAME ':PUTPROP DBFT
			   ':DEFAULT-BINARY-FILE-TYPE)
		  (FUNCALL GENERIC-PATHNAME ':REMPROP ':DEFAULT-BINARY-FILE-TYPE))))
	  (SYSTEM-SOURCE-FILES SYSTEM ':ALL NIL NIL))))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "


(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *NO-INCREMENT-PATCH* NIL)

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "


(DEFUN (:NO-INCREMENT-PATCH MAKE-SYSTEM-KEYWORD) ()
  (SETQ *NO-INCREMENT-PATCH* T))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "


(DEFUN MAKE-SYSTEM (SYSTEM &REST KEYWORDS &AUX *SOMETHING-LOADED*)
  "Operate on the files of the system SYSTEM.
Most commonly used to compile or load those files which need it.
Keywords are not followed by values.
Commonly used keywords include:
 :COMPILE - recompile source files.
 :NOLOAD - don't load compiled files.
 :RELOAD - load even files already loaded.
 :RECOMPILE - recompile files already compiled.
 :SELECTIVE - ask user about each file individually.
 :NOCONFIRM - do not ask for confirmation at all.
 :NO-INCREMENT-PATCH - don't increment the patch version number of a patchable system.
 :INCREMENT-PATCH - do increment the patch version number.
 :NO-LOAD-PATCHES - do not load patches for patchable system being loaded.
 :NO-RELOAD-SYSTEM-DECLARATION - don't reload the file that contains the DEFSYSTEM.
 :PRINT-ONLY - don't load or compile anything, just say what needs to be done.
 :DESCRIBE - say when files were compiled or loaded, etc.
 :SILENT - don't print lists of files on the terminal at all.
 :BATCH - write a file containing any warnings produced by compilation.
      Just load the file, as lisp code, to reload the warnings.
 :DO-NOT-DO-COMPONENTS - omit subsystems."
  ;; Force the system-defining file to get loaded
  ;; before we bind the variables or anything like that.
  (FIND-SYSTEM-NAMED SYSTEM)
  ;; First check whether there is a new system declaration that can be loaded
  (MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM KEYWORDS)
  (PROGW *MAKE-SYSTEM-SPECIAL-VARIABLES*
    (UNWIND-PROTECT
      (PROGN
	(SETQ *SYSTEM-BEING-MADE* (FIND-SYSTEM-NAMED SYSTEM))
	(SETQ *SYSTEM-DEFAULT-BINARY-FILE-TYPE*
	      (OR (GET (LOCF (SYSTEM-PLIST *SYSTEM-BEING-MADE*)) 'DEFAULT-BINARY-FILE-TYPE)
		  ':QFASL))
	(SETQ *TOP-LEVEL-TRANSFORMATIONS*
	      `(,@*LOAD-TYPE-TRANSFORMATIONS* DO-COMPONENTS-INTERNAL))
	;; Do all the keywords
	(DOLIST (KEYWORD KEYWORDS)
	  (LET ((FUNCTION (GET KEYWORD 'MAKE-SYSTEM-KEYWORD)))
	    (OR FUNCTION
		(FERROR NIL "~S is not a recognized option" KEYWORD))
	    (FUNCALL FUNCTION)))
	;; Make :NO-INCREMENT-PATCH override :COMPILE even if :COMPILE comes later.
	(WHEN *NO-INCREMENT-PATCH*
	  (SETQ *TOP-LEVEL-TRANSFORMATIONS*
		(DEL-IF #'(LAMBDA (X)
			    (MEMQ X '(INCREMENT-COMPILED-VERSION)))
			*TOP-LEVEL-TRANSFORMATIONS*)))
	;; Process forms with compiler context
	(DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-BEFORE*)
	  (EVAL FORM))
	;; Do the work of the transformations
	(PERFORM-TRANSFORMATIONS (COLLECT-TOP-LEVEL-TRANSFORMATIONS
				   *SYSTEM-BEING-MADE*))
	;; Finally process any forms queued by the keywords with compiler context
	(DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)
	  (EVAL FORM)))
      ;; Now forms outside of compiler context
      ;; These are done even if there was an error.
      (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*)
	(EVAL FORM))))
  *SOMETHING-LOADED*)

))

; From file PATED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "


(DEFUN ADD-PATCH-BUFFER-CHANGED-FUNCTIONS (BUFFER)
  (LET (PROCEED-FLAG)
    (RESECTIONIZE-BUFFER BUFFER)
    (DOLIST (SECTION (NODE-INFERIORS BUFFER))
      (AND (TYPEP SECTION 'SECTION-NODE)
	   (SECTION-NODE-DEFUN-LINE SECTION)
	   (LET ((PATCH-TICK (GET SECTION 'PATCH-TICK)))
	     (> (NODE-TICK SECTION) (OR PATCH-TICK (BUFFER-FILE-READ-TICK BUFFER))))
	   (LET ((NAME (SECTION-NODE-NAME SECTION)))
	     (WHEN (OR PROCEED-FLAG
		       (SELECTQ (FQUERY `(:CHOICES
					   (((:PROCEED "Proceed.") #/P)
					    . ,FORMAT:Y-OR-N-P-CHOICES))
					"Add ~S to patch? " NAME)
			 (:PROCEED (SETQ PROCEED-FLAG T))
			 ((T) T)))
	       (ADD-PATCH-INTERVAL SECTION NIL T NAME BUFFER)))))))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN QC-FILE-FORM (FORM)
  (PROG (TEM FV)
    (COND ((ATOM FORM))
	  ((MEMQ (CAR FORM) '(COMMENT :COMMENT))) ;Delete comments entirely
	  ((MEMQ (CAR FORM) '(DEFUN :DEFUN))
	   (SETQ TEM (CADR FORM))
	   (SETQ FV (SI:PROCESS-DEFUN-BODY TEM (CDDR FORM)))
	   (COND (QC-FILE-LOAD-FLAG
		     (RPLACA (FUNCTION-CELL-LOCATION TEM) FV)	;In case used interpreted
		     (COMPILE-1 TEM FV)
		     (RETURN (QC-FILE-FASD-FORM FORM T))))
           (QC-TRANSLATE-FUNCTION TEM FV
				  'MACRO-COMPILE
				  (COND (QC-FILE-REL-FORMAT 'REL)
					(T 'QFASL)))
	   (IF (AND *MICROCOMPILE-SWITCH*
		    (GETDECL TEM 'MICROCOMPILE))
	       (QC-TRANSLATE-FUNCTION
		 TEM FV				;Once more, with feeling
		 'MICRO-COMPILE
		 (COND (QC-FILE-REL-FORMAT 'REL)
		       (T 'QFASL))))
	   (RETURN NIL)
           )
	  (QC-FILE-LOAD-FLAG (EVAL FORM)))
    (RETURN (QC-FILE-FASD-FORM FORM T))))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
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
	   (FASD-PACKAGE NIL))
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
	 (WHEN (AND (ATOM FORM) FASD-FLAG)
	   (WARN 'ATOM-AT-TOP-LEVEL ':IMPLAUSIBLE
		 "The atom ~S appeared at top level; this would do nothing at FASLOAD time."
		 FORM))
	 (FUNCALL PROCESS-FN FORM))))))

))
