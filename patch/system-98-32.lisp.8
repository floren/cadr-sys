;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 1/23/84 00:19:36 by RMS,
;;; Reason: Multi-level lexical closure nesting bugs.
;;; PROCESS-MIXIN init option :PROCESS can be a function name,
;;;  or T (meaning the top level is to send a :PROCESS-TOP-LEVEL message).
;;; Help C M-X prints command and function name.
;;; M-X Find Unbalanced Paren pushes point.
;;; Debugger help alist typo.
;;; Selective MAKE-SYSTEM doesn't load depended-on files if you say No.
;;; READLINE bug affecting file error retry questions.
;;; Typeout window exposure and deexposure bugs.
;;; while running on Lisp Machine Eighteen from band 4
;;; with System 98.29, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file COMTAB.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN DOCUMENT-EXTENDED-COMMAND (COMMAND CHAR OP)
  (COND ((EQ OP ':NAME) "a prefix for extended commands")
	((MEMQ OP '(:FULL :SHORT))
	 (FORMAT T "Completing reads and executes a command from the mini buffer.~%")
	 (COND ((EQ OP ':FULL)
		(SETQ COMMAND (GET-EXTENDED-COMMAND "Type a command to document:" *COMTAB*))
		(UNLESS (EQUAL COMMAND "")
		  (FORMAT T "~:C ~A is implemented by ~S:~%" CHAR (CAR COMMAND) (CDR COMMAND))
		  (PRINT-DOC OP (CDR COMMAND) CHAR)))))))

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN BREAKOFF (X &OPTIONAL LEXICAL &AUX FNAME FNAME-TO-GIVE)
  (MULTIPLE-VALUE-BIND (VARS-NEEDED-LEXICALLY FUNCTIONS-NEEDED-LEXICALLY
			BLOCK-NAMES GO-TAGS)
      (CW-TOP-LEVEL-LAMBDA-EXPRESSION
	X
	(LET ((ACCUM
		(LOOP FOR HOME IN VARS
		      WHEN (AND (EQ (VAR-TYPE HOME) 'FEF-LOCAL)
				(EQ HOME (ASSQ (VAR-NAME HOME) VARS)))	;Omit shadowed bindings.
		      COLLECT (VAR-NAME HOME))))
	  (DOLIST (ELT COMPILER-LEXICAL-ENVIRONMENT)
	    (DOLIST (HOME ELT)
	      (PUSHNEW (VAR-NAME HOME) ACCUM)))
	  ACCUM)
	(MAPCAR 'CAR LOCAL-FUNCTIONS)
	LOCAL-MACROS)
    (DOLIST (V VARS-NEEDED-LEXICALLY)
      ;; Note: if V is not on VARS, it must come from an outer lexical level.
      ;; That is ok, and it still requires this LAMBDA to be lexical to access it.
      (SETQ LEXICAL T)
      (LET ((TEM (ASSQ V VARS)))
	(WHEN TEM
	  (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
		   (VAR-MISC TEM)))))
    (DOLIST (F FUNCTIONS-NEEDED-LEXICALLY)
      (LET ((TEM (ASSQ F LOCAL-FUNCTIONS)))
	(WHEN TEM
	  (SETQ LEXICAL T)
	  (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
		   (VAR-MISC (CADR TEM))))))
    (DOLIST (B BLOCK-NAMES)
      (LET ((TEM (IF B (ASSQ B PROGDESCS) RETPROGDESC)))
	(WHEN TEM
	  (UNLESS (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG TEM)
	    (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG TEM)
		  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
		    ;; If consed in temp area, each function would copy it separately
		    ;; and then it would not be shared by the two functions.
		    `(BLOCK ,NAME-TO-GIVE-FUNCTION ,B)))))))
    (DOLIST (G GO-TAGS)
      (LET ((TEM (ASSOC G GOTAGS)))
	(WHEN TEM
	  (SETF (GOTAG-USED-IN-LEXICAL-CLOSURES-FLAG TEM) T)
	  (LET ((TEM (GOTAG-PROGDESC TEM)))
	    (UNLESS (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG TEM)
	      (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG TEM)
		    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
		      ;; If consed in temp area, each function would copy it separately
		      ;; and then it would not be shared by the two functions.
		      `(TAGBODY ,NAME-TO-GIVE-FUNCTION ,G)))))))))
  (SETQ FNAME `(:INTERNAL ,FUNCTION-TO-BE-DEFINED ,BREAKOFF-COUNT)
	FNAME-TO-GIVE `(:INTERNAL ,NAME-TO-GIVE-FUNCTION ,BREAKOFF-COUNT))
  (SETQ BREAKOFF-COUNT (1+ BREAKOFF-COUNT))
  (WHEN LEXICAL
    (INCF LEXICAL-CLOSURE-COUNT))
  (LET ((SFD SELF-FLAVOR-DECLARATION)
	(LOCAL-DECLS LOCAL-DECLARATIONS))
    ;; Pass along the parent function's self-flavor declaration.
    (IF SFD
	(PUSH `(:SELF-FLAVOR . ,SFD)
	      LOCAL-DECLS))
    (SETQ COMPILER-QUEUE
	  (NCONC COMPILER-QUEUE
		 (NCONS
		   (MAKE-COMPILER-QUEUE-ENTRY
		     FUNCTION-SPEC FNAME
		     FUNCTION-NAME FNAME-TO-GIVE
		     DEFINITION X
		     DECLARATIONS LOCAL-DECLS
		     VARIABLES (AND LEXICAL (CONS T COMPILER-LEXICAL-ENVIRONMENT))
		     FUNCTIONS (AND LEXICAL LOCAL-FUNCTIONS)
		     PROGDESCS PROGDESCS
		     RETPROGDESC RETPROGDESC
		     GOTAGS GOTAGS
		     MACROS LOCAL-MACROS)))))
  (LET ((TEM `(BREAKOFF-FUNCTION ,FNAME)))
    (IF LEXICAL `(LEXICAL-CLOSURE ,TEM) TEM)))

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN QCOMPILE0 (EXP FUNCTION-TO-BE-DEFINED GENERATING-MICRO-COMPILER-INPUT-P
		  &OPTIONAL (NAME-TO-GIVE-FUNCTION FUNCTION-TO-BE-DEFINED))
  (PROG (VARS EXP1 DEF-TO-BE-SXHASHED BODY
	 PDLLVL MAXPDLLVL CALL-BLOCK-PDL-LEVELS WITHIN-CATCH
	 ALLGOTAGS TLEVEL P1VALUE BINDP LVCNT
	 DROPTHRU ALLVARS FREEVARS
	 (LOCAL-FUNCTIONS COMPILER-LEXICAL-FUNCTIONS)
	 (LOCAL-MACROS COMPILER-LEXICAL-MACROS)
	 (PROGDESCS COMPILER-LEXICAL-PROGDESCS)
	 (RETPROGDESC COMPILER-LEXICAL-RETPROGDESC)
	 (GOTAGS COMPILER-LEXICAL-GOTAGS)
	 LL TAGOUT TLFUNINIT SPECIALFLAG MACROFLAG
	 LOCAL-MAP ARG-MAP DOCUMENTATION EXPR-DEBUG-INFO
	 FAST-ARGS-POSSIBLE BREAKOFF-COUNT
	 (LEXICAL-CLOSURE-COUNT 0)
	 VARIABLES-USED-IN-LEXICAL-CLOSURES
	 ;; List of all macros found in this function, for the debugging info.
	 MACROS-EXPANDED
	 SELF-FLAVOR-DECLARATION
	 ;; Set to T during pass 1 if any SELF-REFs are present in the function.
	 SELF-REFERENCES-PRESENT
	 (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	 ;; T if this is a SUBST being compiled.  Always put interpreted defn in debug info.
	 SUBST-FLAG
	 INHIBIT-SPECIAL-WARNINGS
	 CLOBBER-NONSPECIAL-VARS-LISTS)
       (SETQ PDLLVL 0)		;RUNTINE LOCAL PDLLVL
       (SETQ DROPTHRU T)	;CAN DROP IN IF FALSE, FLUSH STUFF TILL TAG OR
       (SETQ MAXPDLLVL 0)	;DEEPEST LVL REACHED BY LOCAL PDL
       (SETQ TLEVEL T)
       (SETQ P1VALUE T)
       (SETQ FAST-ARGS-POSSIBLE T)
       (SETQ BREAKOFF-COUNT 0)
       (SETQ EXP1 EXP)
       (BEGIN-PROCESSING-FUNCTION FUNCTION-TO-BE-DEFINED)
       (WHEN (LIST-MATCH-P FUNCTION-TO-BE-DEFINED
			   `(:PROPERTY ,IGNORE :NAMED-STRUCTURE-INVOKE))
	 (WARN 'OBSOLETE-PROPERTY ':IMPLAUSIBLE
	       "NAMED-STRUCTURE-INVOKE, the property name, should not have a colon."))
       ;; If compiling a macro, compile its expansion function
       ;; and direct lap to construct a macro later.
       (COND ((EQ (CAR EXP1) 'MACRO)
	      (SETQ MACROFLAG T)
	      (SETQ EXP1 (CDR EXP1))
	      (SETQ DEF-TO-BE-SXHASHED EXP1)))
       (OR (MEMQ (CAR EXP1) '(LAMBDA SUBST CLI:LAMBDA
			      NAMED-LAMBDA NAMED-SUBST CLI:NAMED-LAMBDA))
	   (PROGN (WARN 'FUNCTION-NOT-VALID ':FATAL "The definition is not a function at all.")
		  (RETURN NIL)))
       (IF (MEMQ (CAR EXP1) '(SUBST NAMED-SUBST))
	   (SETQ SUBST-FLAG T INHIBIT-SPECIAL-WARNINGS T))
       ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
       (WHEN (MEMQ (CAR EXP1) '(NAMED-LAMBDA CLI:NAMED-LAMBDA NAMED-SUBST))
	 (SETQ EXPR-DEBUG-INFO
	       (AND (NOT (ATOM (CADR EXP1)))
		    (CDADR EXP1))
	       EXP1 (CDR EXP1))
	 ;; Debug info that is equivalent to declarations
	 ;; should be turned back into declarations, coming before
	 ;; declarations made outside of compilation
	 ;; but after anything coming from a DECLARE in the body.
	 (DOLIST (ELT (REVERSE EXPR-DEBUG-INFO))
	   (WHEN (ASSQ (CAR ELT) *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	     (PUSH ELT LOCAL-DECLARATIONS))))
       (SETQ LL (CADR EXP1))	;lambda list.
       (SETQ BODY (CDDR EXP1))
       ;; Record the function's arglist for warnings about recursive calls.
       (OR THIS-FUNCTION-ARGLIST-FUNCTION-NAME
	   (SETQ THIS-FUNCTION-ARGLIST-FUNCTION-NAME NAME-TO-GIVE-FUNCTION
		 THIS-FUNCTION-ARGLIST LL))
       ;; Extract documentation string and declarations from the front of the body.
       (MULTIPLE-VALUE (BODY LOCAL-DECLARATIONS DOCUMENTATION)
	 (EXTRACT-DECLARATIONS BODY LOCAL-DECLARATIONS T))
       (SETQ SELF-FLAVOR-DECLARATION
	     (CDR (ASSQ ':SELF-FLAVOR LOCAL-DECLARATIONS)))
       ;; If the user just did (declare (:self-flavor flname)),
       ;; compute the full declaration for that flavor.
       (AND SELF-FLAVOR-DECLARATION
	    (NULL (CDR SELF-FLAVOR-DECLARATION))
	    (SETQ SELF-FLAVOR-DECLARATION
		  (CDR (SI:FLAVOR-DECLARATION (CAR SELF-FLAVOR-DECLARATION)))))
       ;; Actual DEFMETHODs must always have SELF-FLAVOR, or else
       ;; the flavor system will think they are pre-system-85 methods.
       (AND (CONSP FUNCTION-TO-BE-DEFINED)
	    (EQ (CAR FUNCTION-TO-BE-DEFINED) ':METHOD)
	    (SETQ SELF-REFERENCES-PRESENT T))
       ;; Process &KEY and &AUX vars, if there are any.
       (WHEN (OR (MEMQ '&KEY LL) (MEMQ '&AUX LL))
	 ;; Put arglist together with body again.
	 (LET ((LAMEXP `(LAMBDA ,LL (DECLARE . ,LOCAL-DECLARATIONS) . ,BODY)))
	   ;; If there are keyword arguments, expand them.
	   (AND (MEMQ '&KEY LL)
		(SETQ LAMEXP (EXPAND-KEYED-LAMBDA LAMEXP)))
	   ;; Now turn any &AUX variables in the LAMBDA into a PROG in the body.
	   (SETQ LAMEXP (P1AUX LAMEXP))
	   ;; Separate lambda list and body again.
	   (SETQ LL (CADR LAMEXP) BODY (CDDR LAMEXP)))
	 (DO () ((NOT (AND (CONSP (CAR BODY)) (EQ (CAAR BODY) 'DECLARE))))
	   (POP BODY)))
       ;; Now process the variables in the lambda list, after the local declarations.
       (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL LOCAL-DECLARATIONS))
       (COND ((NOT (NULL (CDR BODY)))
	      (SETQ EXP1 (CONS 'PROGN BODY)))
	     ((SETQ EXP1 (CAR BODY))))
       (SETQ EXP1 (P1 EXP1))		;DO PASS 1 TO SINGLE-EXPRESSION BODY
       (SETQ LVCNT (ASSIGN-LAP-ADDRESSES))
       ;; Now that we know all the variables needed by lexical closures,
       ;; make a list of them and put them into the entries in COMPILER-QUEUE
       ;; for each of those lexical closures.
       (UNLESS (ZEROP LEXICAL-CLOSURE-COUNT)
	 (SETQ VARIABLES-USED-IN-LEXICAL-CLOSURES
	       (RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES)))
       (OUTF (LIST 'MFEF FUNCTION-TO-BE-DEFINED SPECIALFLAG (ELIMINATE-DUPLICATES-AND-REVERSE ALLVARS)
		   FREEVARS NAME-TO-GIVE-FUNCTION))
       (AND MACROFLAG (OUTF '(CONSTRUCT-MACRO)))
       (OUTF '(QTAG S-V-BASE))
       (OUTF '(S-V-BLOCK))
       (IF (AND SELF-FLAVOR-DECLARATION SELF-REFERENCES-PRESENT)
	   (OUTF `(SELF-FLAVOR . ,SELF-FLAVOR-DECLARATION)))
       (OUTF '(QTAG DESC-LIST-ORG))
       (OUTF (LIST 'PARAM 'LLOCBLOCK
		   (IF (ZEROP LEXICAL-CLOSURE-COUNT)
		       LVCNT
		     (+ LVCNT (* 4 LEXICAL-CLOSURE-COUNT) 3
			(LENGTH VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
       (OUTF '(A-D-L))
       (OUTF '(QTAG QUOTE-BASE))
       (OUTF '(ENDLIST))			;LAP WILL INSERT QUOTE VECTOR HERE
       (WHEN (NOT (ZEROP LEXICAL-CLOSURE-COUNT))
	 (OUTF `(VARIABLES-USED-IN-LEXICAL-CLOSURES
		  . ,(REVERSE (MAPCAR #'(LAMBDA (HOME)
					  (LET ((TEM (VAR-LAP-ADDRESS HOME)))
					    (SELECTQ (CAR TEM)
					      (ARG (CADR TEM))
					      (T (%LOGDPB 1 %%Q-BOXED-SIGN-BIT (CADR TEM))))))
				      VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
       ;; Set up the debug info from the local declarations and other things
       (LET ((DEBUG-INFO NIL) TEM)
	 (AND DOCUMENTATION (PUSH `(:DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
	 (DOLIST (DCL *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	   (IF (SETQ TEM (ASSQ (CAR DCL) LOCAL-DECLARATIONS))
	       (IF (NEQ (CAR DCL) (CDR DCL))
		   (PUSH (CONS (CDR DCL) (CDR TEM)) DEBUG-INFO)
		 (PUSH TEM DEBUG-INFO))))
	 ;; Propagate any other kinds of debug info from the expr definition.
	 (DOLIST (DCL EXPR-DEBUG-INFO)
	   (OR (ASSQ (CAR DCL) DEBUG-INFO)
	       (PUSH DCL DEBUG-INFO)))
         (AND (PLUSP BREAKOFF-COUNT)
	      (LET ((INTERNAL-OFFSETS (MAKE-LIST BREAKOFF-COUNT)))
		(OUTF `(BREAKOFFS ,INTERNAL-OFFSETS))
		(PUSH `(:INTERNAL-FEF-OFFSETS . ,INTERNAL-OFFSETS) DEBUG-INFO)))
         ;; Include the local and arg maps if we have them.
         ;; They were built by ASSIGN-LAP-ADDRESSES.
         (AND LOCAL-MAP (PUSH `(LOCAL-MAP ,LOCAL-MAP) DEBUG-INFO))
         (AND ARG-MAP (PUSH `(ARG-MAP ,ARG-MAP) DEBUG-INFO))
	 ;; Include list of macros used, if any.
	 (WHEN MACROS-EXPANDED
	   (LET ((MACROS-AND-SXHASHES
		   (MAPCAR #'(LAMBDA (MACRONAME)
			       (LET ((HASH (EXPR-SXHASH MACRONAME)))
				 (IF (OR HASH (CONSP MACRONAME))
				     (LIST MACRONAME HASH)
				   MACRONAME)))
			   MACROS-EXPANDED)))
	     (IF QC-FILE-RECORD-MACROS-EXPANDED
		 (PROGN
		   ;; If in QC-FILE, put just macro names in the function
		   ;; but put the names and sxhashes into the file's list.
		   (PUSH `(:MACROS-EXPANDED ,MACROS-EXPANDED) DEBUG-INFO)
		   (DOLIST (M MACROS-AND-SXHASHES)
		     (OR (MEMBER M QC-FILE-MACROS-EXPANDED)
			 (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			   (PUSH (COPYTREE M) QC-FILE-MACROS-EXPANDED)))))
	       (PUSH `(:MACROS-EXPANDED ,MACROS-AND-SXHASHES)
		     DEBUG-INFO))))
	 (AND (OR (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
		  SUBST-FLAG)
	      (PUSH `(INTERPRETED-DEFINITION ,EXP) DEBUG-INFO))
	 (WHEN SUBST-FLAG
	   (LET* ((ARGS-INFO (ARGS-INFO EXP))
		  (DUMMY-FORM (CONS 'FOO
				    (MAKE-LIST (+ (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)
						  (IF (LDB-TEST %ARG-DESC-EVALED-REST ARGS-INFO)
						      1 0))
					       ':INITIAL-ELEMENT '(GENSYM)))))
	     (UNLESS (EQUAL (SI:SUBST-EXPAND EXP DUMMY-FORM)
			    (SI:SUBST-EXPAND EXP DUMMY-FORM T))
	       ;; If simple and thoughtful substitution give the same result
	       ;; even with the most intractable arguments,
	       ;; we need not use thoughtful substitution for this defsubst.
	       ;; Otherwise, mark it as requiring thoughtful substitution.
	       (PUSH '(:NO-SIMPLE-SUBSTITUTION T) DEBUG-INFO))))
	 ;; Compute the sxhash now, after all displacing macros have been displaced
	 (AND MACROFLAG
	      (PUSH `(:EXPR-SXHASH ,(FUNCTION-EXPR-SXHASH DEF-TO-BE-SXHASHED)) DEBUG-INFO))
	 ;; If we aren't going to mark this function as requiring a mapping
	 ;; table, provide anyway some info that the user declared it wanted one.
	 (AND SELF-FLAVOR-DECLARATION (NOT SELF-REFERENCES-PRESENT)
	      (PUSH `(:SELF-FLAVOR ,(CAR SELF-FLAVOR-DECLARATION)) DEBUG-INFO))
	 (AND DEBUG-INFO
              (OUTF `(DEBUG-INFO . ,DEBUG-INFO))))
       (OUTF 'PROGSA)
       (P2SBIND LL VARS NIL)			;CAN COMPILE INITIALIZING CODE
       (LET ((LEXICAL-CLOSURE-COUNT 0))
	 (P2 EXP1 'D-RETURN))			;DO PASS 2
       (OUTF (LIST 'PARAM 'MXPDL (1+ MAXPDLLVL)))
       (RETURN ALLVARS)))

))

; From file BASWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "

(DEFFLAVOR PROCESS-MIXIN ((PROCESS NIL)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES PROCESS)
  (:INITABLE-INSTANCE-VARIABLES PROCESS)
  (:DOCUMENTATION :MIXIN "For a window which has its own process.
To enable this feature, specify the init keyword :PROCESS when you create the window.
The value should be either a process, a list or a symbol.

If it is a list, the cdr provides the keyword agrs to MAKE-PROCESS
and the car is then used as the top level function to run in the new process.
It will receive one arg when it is called: this window.

If PROCESS is a symbol, it is used as the top level function
and MAKE-PROCESS is called with no keyword arguments.
But, as an exception, if PROCESS is T, the top level function is
to send the window a :PROCESS-TOP-LEVEL message with no arguments.

The first time the window is exposed or selected, the process (if any)
receives the window as a run reason.
Each time the window is exposed or selected, if the process is flushed,
it gets reset and can run again."))

))

; From file BASWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "

(DEFMETHOD (PROCESS-MIXIN :AFTER :INIT) (IGNORE)
  (LET ((TEM PROCESS))
    (CTYPECASE PROCESS
      (SYMBOL (SETQ PROCESS (MAKE-PROCESS NAME))
	      (IF (EQ TEM T)
		  (PROCESS-PRESET PROCESS SELF ':PROCESS-TOP-LEVEL))
	      (PROCESS-PRESET PROCESS TEM SELF))
      (CONS (SETQ PROCESS (APPLY 'MAKE-PROCESS NAME (CDR TEM)))
	    (PROCESS-PRESET PROCESS (CAR TEM) SELF))
      (SI:PROCESS))))

))

; From file COMF.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-FIND-UNBALANCED-PARENTHESES "Find parenthesis error in buffer" ()
  (LET ((BEG-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(END-BP (INTERVAL-LAST-BP *INTERVAL*))
	(POINT (POINT))
	(*BATCH-UNDO-SAVE* T)
	(OLD-TICK (NODE-TICK *INTERVAL*))
	BEG-BP-1 END-BP-1 BP)
    (UNWIND-PROTECT
      (PROGN
	(SETQ BEG-BP-1 (COPY-BP BEG-BP ':MOVES)
	      END-BP-1 (COPY-BP END-BP ':NORMAL))
	(INSERT BEG-BP-1 "(
")
	(INSERT END-BP-1 "
)")
	(IF (SETQ BP (FORWARD-SEXP BEG-BP))
	    (IF (BP-= BP END-BP)		;All ok
		(FORMAT QUERY-IO "~&All parens appear balanced.")
	      (POINT-PDL-PUSH POINT *WINDOW*)
	      (MOVE-BP POINT BP)
	      (FORMAT QUERY-IO "~&Probably extra right-paren here."))
	  (OR (SETQ BP (FORWARD-SEXP END-BP -1))
	      (BARF "Cannot find unbalanced parenthesis"))
	  (POINT-PDL-PUSH POINT *WINDOW*)
	  (MOVE-BP POINT BP)
	  (FORMAT QUERY-IO "~&Probably no right-paren for this left-paren.")))
      (COND (BEG-BP-1
	     (DELETE-INTERVAL BEG-BP BEG-BP-1 T)
	     (FLUSH-BP BEG-BP-1)))
      (COND (END-BP-1
	     (DELETE-INTERVAL END-BP-1 END-BP T)
	     (FLUSH-BP END-BP-1)))
      (SETF (NODE-TICK *INTERVAL*) OLD-TICK)))
  DIS-BPS)

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defconst com-help-alist '(((#\G "General"
			     "General information about using the debugger") #\G #\g)
			   ((#\I "Information"
			         "Various ways of obtaining information about the current stack frame")
				 #\I #\i #\E #\e)
			   ((#\F "Stack Frames" "Selecting Stack Frames to examine") #\F #\f)
			   ((#\S "Stepping" "Stepping though through the program") #\S #\s)
			   ((#\P "Proceeding"
			         "Proceeding from this error and resuming execution")
			         #\P #\p #\X #\x)
			   ((#\T "Transferring"
			         "Transferring to other systems: Edit, Bug report, Window-based Debugger")
				 #\T #\t)
			   ((#\D "Describe"
			     "Give the documentation of the function associated with a command key")
				 #\D #\d #\C #\c)
			   ((#\abort "Abort" t) #\abort #\c-Z #\c-G)
			   ((#\help "Help" t) #\help #\?))
  "FQUERY options for used in giving help for debugger commands.")

))

; From file MAKSYS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN DO-FILE-TRANSFORMATIONS ()
  (IF (OR (EQ *QUERY-TYPE* ':NOCONFIRM)
	   (QUERY-USER-LIST))
      ;;Now actually do the work
      (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
	(LET ((STATE (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION)))
	  (SELECTQ STATE
	    ((:DONE :REFUSED :NOT-NEEDED NIL))	;Already done or user said no
	    ((:PENDING :PROBABLY)
	     (LET ((TYPE (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
		   (ARGS (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
		   (*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-TRANSFORMATION))
		   (*SYSTEM-BEING-MADE* (FILE-TRANSFORMATION-SYSTEM FILE-TRANSFORMATION)))
	       (COND ((IF (EQ STATE ':PROBABLY)	;If we suspected something would change
			  (IF (APPLY (FILE-TRANSFORMATION-CONDITION-FUNCTION	;check again
				       FILE-TRANSFORMATION)
				     ARGS)
			      T
			      (SETQ STATE ':NOT-NEEDED)	;Turned out it didn't
			      NIL)		;Don't do it
			  T)
		      ;;Otherwise perform the transformation
		      (OR *SILENT-P*
			  (FORMAT T "~&~\FILE-XFORM-TYPE\~:[ ~\FILE-XFORM-ARGS\~;~*~]~
				     ~:[~; in~:[to~] package ~A~]"
				  TYPE (NULL ARGS) FILE-TRANSFORMATION *FORCE-PACKAGE*
				  (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION)
				  *FORCE-PACKAGE*))
		      (CATCH-ERROR-RESTART
			(ERROR "Give up on all the ~(~A~)."
			       (TRANSFORMATION-TYPE-PRETTY-PRESENT-PARTICIPLE TYPE))
			(ERROR-RESTART
			  (ERROR "Retry all the ~(~A~)."
			       (TRANSFORMATION-TYPE-PRETTY-PRESENT-PARTICIPLE TYPE))
			  (APPLY (TRANSFORMATION-TYPE-FUNCTION TYPE) ARGS))
			(SETQ STATE ':DONE)
			;;That probably made new versions of the outputs files
			(DOLIST (PATHNAME (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION))
			  ;; So, forget any file info for the file.
			  (INVALIDATE-PATHNAME-INFO PATHNAME)
			  ;; Any transformation already done will need to be redone.
			  (DOLIST (FILE-XFORM *FILE-TRANSFORMATION-LIST*)
			    ;; Removed a check for :REFUSED here, 1/25/84,
			    ;; so that once a user says No, the transformation WILL NOT go.
			    (AND (MEMQ (FILE-TRANSFORMATION-STATE FILE-XFORM) '(:DONE))
				 (DO ((L (FILE-TRANSFORMATION-ARGS FILE-XFORM) (CDR L))
				      (TAIL (FILE-TRANSFORMATION-OUTPUTS FILE-XFORM)))
				     ((EQ L TAIL) NIL)
				   (AND (EQ PATHNAME (CAR L)) (RETURN T)))
				 (SETF (FILE-TRANSFORMATION-STATE FILE-XFORM) ':PROBABLY))))))))
	     (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) STATE))
	    (OTHERWISE
	     (FERROR NIL "Transformation ~S in bad state" FILE-TRANSFORMATION)))))
    ;; If user says No to the entire bunch of transformations
    (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
      (AND (MEMQ (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) '(:PENDING :PROBABLY))
	   (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) ':REFUSED)))))

))

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READLINE (&REST READ-ARGS)
  "Read a line from STREAM and return it as a string.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to :RUBOUT-HANDLER if it is used.

The second value is EOF-OPTION if we exit due to end of file.

The third value is the delimiter which ended the input, or NIL if
it ended due to EOF."
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS)
	   (VALUES STRING-OR-EOF-OPTION EOF-FLAG DELIMITER))
  (LET ((OPTIONS NIL))
    ;; This kludge is to let us take a third, optional argument.
    (COND ((> (LENGTH READ-ARGS) 2)
	   (SETQ OPTIONS (THIRD READ-ARGS))
	   (SETQ READ-ARGS (LIST (FIRST READ-ARGS) (SECOND READ-ARGS)))))
    (MULTIPLE-VALUE-BIND (STREAM EOF-OPTION)
	(DECODE-READ-ARGS READ-ARGS)
      (MULTIPLE-VALUE-BIND (STRING EOF TERMINATOR)
	  (READ-DELIMITED-STRING '(#\RETURN #\END) STREAM
				 (EQ EOF-OPTION 'NO-EOF-OPTION) OPTIONS)
	(VALUES STRING (IF EOF EOF-OPTION) TERMINATOR)))))

))

; From file BASWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "

(DEFMETHOD (ESSENTIAL-SET-EDGES :REDEFINE-MARGINS) ()
  (SETQ RESTORED-BITS-P T)
  (MULTIPLE-VALUE-BIND (LM TM RM BM)
      (SEND SELF ':COMPUTE-MARGINS 0 0 0 0)
    (UNLESS (AND (= LEFT-MARGIN-SIZE LM)
		 (= TOP-MARGIN-SIZE TM)
		 (= RIGHT-MARGIN-SIZE RM)
		 (= BOTTOM-MARGIN-SIZE BM))
      (PRESERVE-SUBSTITUTE-STATUS SELF
	(WITH-SHEET-DEEXPOSED (SELF)
	  (AND BIT-ARRAY (SI:PAGE-IN-ARRAY BIT-ARRAY))
	  (LET ((INSIDE-SIZE-CHANGED
		  (FUNCALL-SELF ':CHANGE-OF-SIZE-OR-MARGINS
				':LEFT-MARGIN-SIZE LM
				':TOP-MARGIN-SIZE TM
				':RIGHT-MARGIN-SIZE RM
				':BOTTOM-MARGIN-SIZE BM)))
	    (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	      (FUNCALL-SELF ':REFRESH (IF INSIDE-SIZE-CHANGED
					  ':SIZE-CHANGED
					':MARGINS-ONLY))))
	  (AND BIT-ARRAY (SI:PAGE-OUT-ARRAY BIT-ARRAY)))))))

))

; From file BASWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "

(DEFUN SYSTEM-SET-EDGES (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM &OPTIONAL OPTION
			 &AUX (NEW-WIDTH (- NEW-RIGHT NEW-LEFT))
			      (NEW-HEIGHT (- NEW-BOTTOM NEW-TOP))
			      ERROR WINDOW-TO-BE-DEEXPOSED)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-SET-EDGES))
  (DELAYING-SCREEN-MANAGEMENT
    (DO (DONE RESULT) (())
      (SETQ WINDOW-TO-BE-DEEXPOSED
	  (*CATCH 'SET-EDGES
	    (LOCK-SHEET (SELF)
	      (SETQ RESULT
		(COND ((SETQ ERROR (FUNCALL-SELF ':VERIFY-NEW-EDGES NEW-LEFT NEW-TOP
						 NEW-WIDTH NEW-HEIGHT))
		       ;; Can't put window there
		       (SELECTQ OPTION
			 (:VERIFY NIL)
			 (OTHERWISE
			  (FERROR NIL ERROR))))
		      ((EQ OPTION ':VERIFY)
		       ;; "Only want to know"
		       T)
		      ((AND (= NEW-WIDTH WIDTH)
			    (= NEW-HEIGHT HEIGHT)
			    (= NEW-LEFT X-OFFSET)
			    (= NEW-TOP Y-OFFSET))
		       ;;Not changing size or position, just return T (we do the verify 
		       ;; anyway in case something in the environment has made the current
		       ;; size no longer "ok", such as having the size of the
		       ;; superior change.)
		       T)
		      ((AND (= NEW-WIDTH WIDTH)
			    (= NEW-HEIGHT HEIGHT))
		       ;; Only moving the window, move it's bits behind its back
		       (LET ((CURRENT-RECTANGLE (LIST X-OFFSET Y-OFFSET
						      (+ X-OFFSET WIDTH)
						      (+ Y-OFFSET HEIGHT))))
			 (COND ((NOT EXPOSED-P)
				(SHEET-SET-DEEXPOSED-POSITION NEW-LEFT NEW-TOP)
				(LEXPR-FUNCALL #'SCREEN-AREA-HAS-CHANGED SELF
					       CURRENT-RECTANGLE)
				(SCREEN-CONFIGURATION-HAS-CHANGED SELF))
			       ((SHEET-TEMPORARY-P)
				;; For temporary windows, just deexpose and reexpose
				(LET ((SELECT-P (EQ SELF SELECTED-WINDOW)))
				  (FUNCALL-SELF ':DEEXPOSE)
				  (FUNCALL-SELF ':EXPOSE NIL NIL NEW-LEFT NEW-TOP)
				  (AND SELECT-P (FUNCALL-SELF ':SELECT))))
			       (T
				(OR (SHEET-BOUNDS-WITHIN-SHEET-P NEW-LEFT NEW-TOP
								 WIDTH HEIGHT
								 SUPERIOR)
				    (FERROR NIL
					    "Attempt to move sheet ~S outside of superior"
					    SELF))
				;; Make sure everyone under us is deexposed
				(WITHOUT-INTERRUPTS
				  (DOLIST (SISTER (SHEET-EXPOSED-INFERIORS SUPERIOR))
				    (COND ((AND (NEQ SELF SISTER)
						(SHEET-OVERLAPS-P SISTER NEW-LEFT NEW-TOP
								  WIDTH HEIGHT))
					   (*THROW 'SET-EDGES SISTER)))))
				(SHEET-SET-EXPOSED-POSITION NEW-LEFT NEW-TOP)
				(LEXPR-FUNCALL #'SCREEN-AREA-HAS-CHANGED SELF
					       CURRENT-RECTANGLE)
				(SCREEN-CONFIGURATION-HAS-CHANGED SELF)))))
		      (T
		       (LET ((CURRENT-RECTANGLE (LIST X-OFFSET Y-OFFSET
						      (+ X-OFFSET WIDTH)
						      (+ Y-OFFSET HEIGHT))))
			 (PRESERVE-SUBSTITUTE-STATUS SELF
			   (WITH-SHEET-DEEXPOSED (SELF)
			     (AND BIT-ARRAY
				  (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
			     (FUNCALL-SELF ':CHANGE-OF-SIZE-OR-MARGINS
					   ':LEFT NEW-LEFT ':TOP NEW-TOP
					   ':WIDTH NEW-WIDTH ':HEIGHT NEW-HEIGHT)
			     (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
			       (FUNCALL-SELF ':REFRESH ':SIZE-CHANGED))))
			 (AND BIT-ARRAY (SI:PAGE-OUT-ARRAY BIT-ARRAY))
			 (SETQ MOUSE-RECONSIDER T)
			 (LEXPR-FUNCALL #'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
			 (SCREEN-CONFIGURATION-HAS-CHANGED SELF)))))
	      (SETQ DONE T))))
      (IF DONE
	  (RETURN RESULT ERROR)
	  (FUNCALL WINDOW-TO-BE-DEEXPOSED ':DEEXPOSE)))))

))

; From file SHEET.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN SHEET-PREPARE-FOR-EXPOSE (SHEET INSIDE-EXPOSE-METHOD
				 &OPTIONAL TURN-ON-BLINKERS BITS-ACTION
				 	   (X X-OFFSET) (Y Y-OFFSET))
  (DECLARE (:SELF-FLAVOR SHEET))
  TURN-ON-BLINKERS
  (PROG ABORT ((OLD-INHIBIT-SCHEDULING-FLAG INHIBIT-SCHEDULING-FLAG)
	       (INHIBIT-SCHEDULING-FLAG T)
	       RESULT)
     MAIN-LOOP
	(SETQ INHIBIT-SCHEDULING-FLAG T)
	(COND ((NOT (SHEET-CAN-GET-LOCK SHEET))
	       (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	       (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET)
	       (GO MAIN-LOOP)))
	(WHEN EXPOSED-P
	  (RETURN-FROM ABORT T BITS-ACTION NIL))
	(OR (NOT INSIDE-EXPOSE-METHOD)
	    (NULL SUPERIOR)
	    (MEMQ SELF (SHEET-INFERIORS SUPERIOR))
	    ;; We can only be exposed if we are activated
	    (RETURN-FROM ABORT NIL BITS-ACTION
			 (LIST NIL "Attempt to expose deactivated sheet ~S" SELF)))
	(COND ((OR ( X-OFFSET X) ( Y-OFFSET Y))
	       (AND INSIDE-EXPOSE-METHOD (RETURN-FROM ABORT NIL BITS-ACTION NIL))
	       (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	       (SHEET-SET-DEEXPOSED-POSITION X Y)
	       (GO MAIN-LOOP)))
	(OR (NULL SUPERIOR)
	    (NOT INSIDE-EXPOSE-METHOD)
	    (SHEET-WITHIN-SHEET-P SELF SUPERIOR)
	    (RETURN-FROM ABORT NIL BITS-ACTION
			 (LIST NIL "Attempt to expose ~S outside of its superior" SELF)))
	;; If our superior is temp locked, see if we will overlap any
	;; of the temp windows.  If we will, then wait until the temp window is
	;; deexposed then try again
	(COND ((AND SUPERIOR
		    (LISTP (SHEET-LOCK SUPERIOR))
		    (SETQ RESULT
			  (DOLIST (TEMP-SHEET (SHEET-LOCK SUPERIOR))
			    (AND (SHEET-OVERLAPS-SHEET-P TEMP-SHEET SELF)
				 (RETURN TEMP-SHEET)))))
	       (AND INSIDE-EXPOSE-METHOD (RETURN-FROM ABORT NIL BITS-ACTION NIL))
	       (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	       (PROCESS-WAIT "Sheet Deexpose"
			     #'(LAMBDA (TEMP-SHEET SUP)
				 (OR (NOT (LISTP (SHEET-LOCK SUP)))
				     (NOT (MEMQ TEMP-SHEET (SHEET-LOCK SUP)))))
			     RESULT SUPERIOR)
	       (GO MAIN-LOOP)))
	(COND ((SHEET-TEMPORARY-P)
	       (SETQ RESULT
		     (*CATCH 'SHEET-EXPOSE-CANT-GET-LOCK
		       (LET ((*REQUESTOR* SELF))
			 (DECLARE (SPECIAL *REQUESTOR*))
			 ;; Check to make sure we can get all the locks at once
			 (MAP-OVER-EXPOSED-SHEET
			   #'(LAMBDA (TARGET)
			       (AND ;; Can't be us, we aren't exposed yet
				 (NEQ TARGET (SHEET-SUPERIOR *REQUESTOR*))
				 ;; Sheet may be on EXPOSED-INFERIORS, but not
				 ;; in actuality exposed
				 (SHEET-EXPOSED-P TARGET)
				 (SHEET-OVERLAPS-SHEET-P *REQUESTOR* TARGET)
				 (OR (SHEET-CAN-GET-TEMPORARY-LOCK TARGET *REQUESTOR*)
				     (*THROW 'SHEET-EXPOSE-CANT-GET-LOCK TARGET))
				 ;; If this window owns the mouse, must force
				 ;; mouse out of it
				 (EQ TARGET MOUSE-WINDOW)
				 (*THROW 'SHEET-EXPOSE-CANT-GET-LOCK TARGET)))
			   SUPERIOR)
			 ;; We can, get them all and win totally, but only do this if
			 ;; we are inside the expose method proper
			 (AND INSIDE-EXPOSE-METHOD
			      (LET ((*REQUESTOR* SELF))
				(DECLARE (SPECIAL *REQUESTOR*))
				(MAP-OVER-EXPOSED-SHEET
				  #'(LAMBDA (TARGET)
				      (COND ((AND ;; Can't be us, we aren't exposed yet
					       (NEQ TARGET (SHEET-SUPERIOR *REQUESTOR*))
					       ;; Sheet may be on EXPOSED-INFERIORS, but not
					       ;; in actuality exposed
					       (SHEET-EXPOSED-P TARGET)
					       (SHEET-OVERLAPS-SHEET-P *REQUESTOR* TARGET))
					     ;; All blinkers must get turned off on this sheet
					     (SHEET-OPEN-BLINKERS TARGET)
					     (OR (SHEET-GET-TEMPORARY-LOCK TARGET *REQUESTOR*)
						 (FERROR NIL
             "Internal error, can't get lock on ~A, but we already verified we could get lock"
	     						 TARGET))
					     (PUSH TARGET TEMPORARY-WINDOWS-LOCKED))))
				  SUPERIOR)))
			 ;; Return NIL indicating that we are winning
			 NIL)))
	       (COND ((NULL RESULT)
		      (AND INSIDE-EXPOSE-METHOD
			   ;; For temporary windows, we must open the blinkers of our
			   ;; superiors to all levels
			   (SHEET-OPEN-ALL-BLINKERS SUPERIOR)))
		     (INSIDE-EXPOSE-METHOD (RETURN-FROM ABORT NIL BITS-ACTION NIL))
		     ((EQ RESULT MOUSE-WINDOW)
		      (SETQ MOUSE-RECONSIDER T)
		      (PUSH RESULT *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
		      (SETF (SHEET-INVISIBLE-TO-MOUSE-P RESULT) T)
		      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		      (PROCESS-WAIT "Mouse Out"
				    #'(LAMBDA (SHEET) (NEQ MOUSE-WINDOW SHEET))
				    RESULT)
		      (GO MAIN-LOOP))
		     (T
		      ;; One we couldn't get: wait for it
		      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		      (PROCESS-WAIT "Temp Lock"
				    #'(LAMBDA (TARGET SHEET)
					(OR (NOT (SHEET-EXPOSED-P TARGET))
					    (NOT (SHEET-OVERLAPS-SHEET-P SHEET TARGET))
					    (SHEET-CAN-GET-TEMPORARY-LOCK TARGET SHEET)))
				    RESULT SELF)
		      (GO MAIN-LOOP))))
	      (SUPERIOR
	       ;; Deexpose all we will overlap, then loop again as the world may have
	       ;; changed out from under us
	       (LET ((FLAG NIL))
		 (DOLIST (SIBLING (SHEET-EXPOSED-INFERIORS SUPERIOR))
		   (COND ((AND (NEQ SELF SIBLING) (SHEET-OVERLAPS-SHEET-P SELF SIBLING))
			  (AND INSIDE-EXPOSE-METHOD (RETURN-FROM ABORT NIL BITS-ACTION NIL))
			  (SETQ INHIBIT-SCHEDULING-FLAG OLD-INHIBIT-SCHEDULING-FLAG
				FLAG T)
			  (FUNCALL SIBLING ':DEEXPOSE))))
		 (AND FLAG
		      ;; If had to deexpose someone, world may have changed
		      (GO MAIN-LOOP)))))
	;; We have successfully met all of the requirements, be successful
 	(RETURN T BITS-ACTION)))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFUN TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR (WINDOW)
  (LET ((HIGHEST (SEND WINDOW ':ALIAS-FOR-SELECTED-WINDOWS)))
    (DO ((W HIGHEST (SEND W ':SELECTION-SUBSTITUTE))
	 (PREV HIGHEST W))
	((OR (NULL W)
	     (EQ W WINDOW)
	     (NOT (SHEET-ME-OR-MY-KID-P SELF W)))
	 PREV))))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFWRAPPER (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :CHANGE-OF-SIZE-OR-MARGINS) (IGNORE . BODY)
  ;`(WITH-SHEET-DEEXPOSED (TYPEOUT-WINDOW) . ,BODY)
  `(LET (.STATUS. .INCOMPLETE-P.)
     (DELAYING-SCREEN-MANAGEMENT
       (UNWIND-PROTECT
	 (PROGN
	   (COND (TYPEOUT-WINDOW			;May not be present during init
		  (SETQ .STATUS. (FUNCALL TYPEOUT-WINDOW ':STATUS))
		  (SETQ .INCOMPLETE-P. (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW))))
	   . ,BODY)
	 (WHEN .STATUS.
	   (FUNCALL TYPEOUT-WINDOW ':SET-STATUS .STATUS.)
	   (SETF (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW) .INCOMPLETE-P.))))))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :DEEXPOSE)
	   (&OPTIONAL IGNORE IGNORE (REMOVE-FROM-SUPERIOR T))
  (WHEN REMOVE-FROM-SUPERIOR
    (SETQ BOTTOM-REACHED NIL)
    ;; The following used to be in a :BEFORE :DEEXPOSE method, now eliminated.
    (SETQ INCOMPLETE-P NIL)))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "


(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :EXPOSE) (IGNORE . BODY)
  `(LET ((.TYPEOUT-WAS-EXPOSABLE. (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))
	 (.OLD-INCOMPLETE-P. INCOMPLETE-P))
     (DECLARE (SPECIAL .TYPEOUT-WAS-EXPOSABLE. .OLD-INCOMPLETE-P.))
     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (SETQ MORE-VPOS (AND (SEND SUPERIOR ':MORE-P) (SHEET-DEDUCE-MORE-VPOS SELF)))
  (LOCAL-DECLARE ((SPECIAL .TYPEOUT-WAS-EXPOSABLE. .OLD-INCOMPLETE-P.))
    (IF .TYPEOUT-WAS-EXPOSABLE.
	(SETQ INCOMPLETE-P .OLD-INCOMPLETE-P.)
      (SETQ BOTTOM-REACHED (OR BOTTOM-REACHED 0)
	    INCOMPLETE-P T)
      ;;On becoming exposed, also be the selection substitute for an appropriate ancestor.
      (AND (NEQ SELF SELECTED-WINDOW)
	   (LET ((TEM (TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR SELF)))
	     (UNLESS (EQ (SEND TEM ':SELECTION-SUBSTITUTE) SELF)
	       (SETQ WINDOW-SUBSTITUTING-FOR TEM)
	       (SETQ PREVIOUS-SUBSTITUTE (SEND TEM ':SELECTION-SUBSTITUTE))
	       (SEND WINDOW-SUBSTITUTING-FOR ':SET-SELECTION-SUBSTITUTE SELF)))))))

))
