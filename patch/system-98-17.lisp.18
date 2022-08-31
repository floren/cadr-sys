;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.17
;;; Reason: CHAOS:OPEN-STREAM with host NIL does a listen.
;;; Pathname defaulting in Dired and BDired buffers.
;;; RENAME-FILE, (UN)DELETE-FILE, COPY-FILE: wildcards, and values returned.
;;;  M-X Rename File, M-X Copy File corresponding changes.
;;; Attempt to prevent window output or switching while using cold load stream.
;;; Implement PARSE-ERROR, PARSE-FERROR.  Check for freeing an error handler sg twice.
;;; NAMED-STRUCTURE-INVOKE props work with or without colon.
;;; M-X Undelete File failed to look for deleted files!
;;; Bugs in FILE-RETRY-NEW-PATHNAME; :TRUENAME on pathnames.
;;; Describe inconsistently-updated systems more nicely.
;;; WARN-ON-ERRORS-CONDITION-HANDLER bug with ~'s.
;;; Bugs in SIMPLE-MAKE-ARRAY, PRINT-RECORD-OCCURRENCES, FUNCTION-START-SYMBOLS.
;;; Anonymous sections & files with structured name components.
;;; Local declarations in interpreter bug.  :MOUSE-CLICK bug affecting ZMail's Buttons.
;;; Written 12/26/83 02:53:05 by RMS,
;;; while running on Lisp Machine Two from band 6
;;; with Bad Inconsistently updated System 98.12, CADR 3.2, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 305, ZM MIT.


(globalize "PARSE-ERROR" "SYS")
(globalize "PARSE-FERROR" "SYS")

; From file QCP1.LISP SRC:<L.SYS> OZ:
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
       (WHEN VARIABLES-USED-IN-LEXICAL-CLOSURES
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

(DEFUN P1AUX (LAMBDA)
  (BLOCK DONE
    (LET (STANDARDIZED AUXVARS AUXLIST NONAUXLIST DECLS BODY)
      (SETQ STANDARDIZED (SI:LAMBDA-EXP-ARGS-AND-BODY LAMBDA))
      (OR (SETQ AUXLIST (MEMQ '&AUX (CAR STANDARDIZED)))
	  (RETURN-FROM DONE LAMBDA))
      (SETQ AUXVARS (CDR AUXLIST))
      (SETQ NONAUXLIST (LDIFF (CAR STANDARDIZED) AUXLIST))
      (DO ((VARLIST NONAUXLIST (CDR VARLIST))
	   SPECIAL-FLAG)
	  ((NULL VARLIST)
	   (IF SPECIAL-FLAG
	       (PUSH '&SPECIAL AUXVARS)))
	(COND ((EQ (CAR VARLIST) '&SPECIAL)
	       (SETQ SPECIAL-FLAG T))
	      ((EQ (CAR VARLIST) '&LOCAL)
	       (SETQ SPECIAL-FLAG NIL))))
      (SETQ BODY (CDR STANDARDIZED))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (SETF (VALUES BODY DECLS)
	    (EXTRACT-DECLARATIONS-RECORD-MACROS BODY))
      `(LAMBDA ,NONAUXLIST
	 ,@(IF DECLS `((DECLARE . ,DECLS)))
	 (LET* ,AUXVARS
	   ,@(IF DECLS `((DECLARE . ,DECLS)))
	   . ,BODY)))))

))

; From file ADVISE.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "


(deff advise-prog #'prog)
(deff advise-setq #'setq)
(deff advise-progn #'progn)
(deff advise-multiple-value-list #'multiple-value-list)
(deff advise-return-list #'return-list)
(deff advise-apply #'apply)
(deff advise-let #'let)
(deff advise-list* #'list*)

(fset 'encapsulation-let #'let)

))


; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN FREE-SECOND-LEVEL-ERROR-HANDLER-SG (SG &AUX (DEFAULT-CONS-AREA ERROR-HANDLER-AREA))
  "Mark the second level error handler stack group SG as available for re-use."
  (WHEN (MEMQ SG FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
    (FERROR NIL "Freeing ~S, but it's already free." SG))
  (COND ((NEQ SG CURRENT-STACK-GROUP)
	 ;; Freeing the error handler, but not current stack group, so cause it to
	 ;; do a LEAVING-ERROR-HANDLER first
	 (SG-FUNCALL SG #'LEAVING-ERROR-HANDLER)))
  (WITHOUT-INTERRUPTS
    ;; If appropriate and user approves, restore the saved screen
    ;; which we clobbered by using the cold load stream.
    (WHEN (EQ SG COLD-LOAD-STREAM-DEBUGGER-SG)
      (RESTORE-SCREEN-FOR-COLD-LOAD-STREAM)
      (SETQ COLD-LOAD-STREAM-DEBUGGER-SG NIL))
    (PUSH SG FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
    (AND CURRENT-PROCESS (FUNCALL CURRENT-PROCESS ':REVOKE-RUN-REASON SG))))

))

; From file QRAND.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN NAMED-STRUCTURE-INVOKE (OPERATION STRUCTURE &REST ARGS)
  ;; This function used to take its first two arguments in the other order.
  ;; We are comitted to supporting the old argument order indefinitely.
  (IF (ARRAYP OPERATION)
      (PSETQ OPERATION STRUCTURE STRUCTURE OPERATION))
  (CHECK-ARG OPERATION SYMBOLP "a symbol")
  (CHECK-ARG STRUCTURE ARRAYP "an array")
  (LET* ((SELF STRUCTURE)
	 (C (IF (ARRAY-HAS-LEADER-P SELF)
		(ARRAY-LEADER SELF 1)
		(AREF SELF 0))))
    (IF (SYMBOLP C)
	(SETQ C (OR (GET C 'NAMED-STRUCTURE-INVOKE)
		    (GET C ':NAMED-STRUCTURE-INVOKE))))
    (COND ((NULL C) NIL)
	  ((EQ (TYPEP C) 'CLOSURE)			;If a closure, assume knows about SELF
	   (LEXPR-FUNCALL C OPERATION ARGS))
	  (T (LEXPR-FUNCALL C OPERATION SELF ARGS)))))	;flush the SELF arg

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun gobble-declarations-internal (body &optional (env interpreter-environment))
  (dolist (bodyelt body)
    (or (and (consp bodyelt)
	     (eq (car bodyelt) 'declare))
	(return nil))
    (setq local-declarations
	  (append (cdr bodyelt) local-declarations))
    (dolist (decl (cdr bodyelt))
      (when (memq (car decl) '(special unspecial))
	(dolist (var (cdr decl))
	  (setf (car env)
		(list*-in-area background-cons-area
			       (value-cell-location var)
			       nil
			       (car env)))
	  (if (eq (car decl) 'special)
	      (let ((slot (locf (cadr (car env)))))
		(%p-store-pointer slot (value-cell-location var))
		(%p-store-data-type slot dtp-one-q-forward))))))))

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFCONST FUNCTION-START-SYMBOLS
	  '(LAMBDA CLI:LAMBDA SUBST CLI:SUBST NAMED-LAMBDA CLI:NAMED-LAMBDA NAMED-SUBST
		   CLI:NAMED-SUBST
		   CURRY-BEFORE CURRY-AFTER)
  "A list starting with one of these symbols can be a function.")

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN WILDCARDED-FILE-OPERATION (STRING-OR-STREAM
				  HELPER-FUNCTION
				  DIR-LIST-OPTIONS
				  &REST ARGS)
  "Call HELPER-FUNCTION for each file which STRING-OR-STREAM refers to.
If STRING-OR-STREAM is a string (or a pathname) then it can refer to
more than one file by containing wildcards.
The arguments passed to HELPER-FUNCTION each time are
1) the truename of a file, 
2) the possibly wildcarded pathname being mapped over
 followed by ARGS.
DIR-LIST-OPTIONS are passed to FS:DIRECTORY-LIST when finding out
what files exist to be processed."
  (FORCE-USER-TO-LOGIN)
  (IF (OR (STRINGP STRING-OR-STREAM)
	  (TYPEP STRING-OR-STREAM 'PATHNAME))	;Not a stream
      (LET ((SPECIFIED-PATHNAME (MERGE-PATHNAME-DEFAULTS STRING-OR-STREAM)))
	(LEXPR-FUNCALL SPECIFIED-PATHNAME ':WILDCARD-MAP HELPER-FUNCTION
		       NIL DIR-LIST-OPTIONS SPECIFIED-PATHNAME ARGS))
    (LET ((SPECIFIED-PATHNAME (SEND STRING-OR-STREAM ':TRUENAME)))
      (LIST (LEXPR-FUNCALL HELPER-FUNCTION
			   SPECIFIED-PATHNAME SPECIFIED-PATHNAME
			   ARGS)))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-DELIMITED-STRING (&OPTIONAL (DELIMITER #/END) (STREAM STANDARD-INPUT)
			      EOF RH-OPTIONS (BUFFER-SIZE 100.))
  "Reads input from STREAM until DELIMITER is found; returns a string.
Uses the rubout handler if STREAM supports that.
DELIMITER is either a character or a list of characters.
 (Characters may be fixnums or character objects).
Values are:
 The string of characters read, not including the delimiter
 T if input ended due to end of file
 The delimiter character read (as a fixnum), or NIL if ended at EOF.
EOF if non-NIL means get error on end of file before any input is got.
RH-OPTIONS are passed to the :RUBOUT-HANDLER operation.
BUFFER-SIZE is the size to make the buffer string, initially."
  (DECLARE (VALUES STRING EOF-FLAG DELIMITER))
  (IF (AND (NOT RUBOUT-HANDLER)
	   (SEND STREAM ':OPERATION-HANDLED-P ':RUBOUT-HANDLER))
      (SEND STREAM ':RUBOUT-HANDLER
	    (CONS (LIST ':ACTIVATION 'MEMQ
			(IF (CONSP DELIMITER) DELIMITER (LIST DELIMITER)))
		  RH-OPTIONS)
	    'READ-DELIMITED-STRING DELIMITER STREAM EOF NIL BUFFER-SIZE)
    (DO ((BUFFER (MAKE-ARRAY BUFFER-SIZE ':TYPE ART-STRING ':FILL-POINTER 0)))
	(())
      (LET ((CH (SEND STREAM (IF RUBOUT-HANDLER ':ANY-TYI ':TYI)
		      (AND EOF (ZEROP (LENGTH BUFFER))))))
	(COND ((NULL CH)
	       (RETURN BUFFER T))
	      ((CONSP CH)
	       (WHEN (EQ (CAR CH) ':ACTIVATION)
		 (SEND STREAM ':TYO (CADR CH))
		 (RETURN BUFFER NIL (CADR CH))))
	      ((AND (NOT RUBOUT-HANDLER)
		    (IF (CONSP DELIMITER) (MEMQ CH DELIMITER) (EQ CH DELIMITER)))
	       (RETURN BUFFER NIL CH))
	      (T
	       (ARRAY-PUSH-EXTEND BUFFER CH)))))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "


(DEFUN COPY-FILE (PATHNAME-OR-STREAM NEW-NAME
		  &REST OPTIONS
		  &KEY (ERROR T) &ALLOW-OTHER-KEYS)
  "Copy a file, specified as a pathname, string or I//O stream.
CHARACTERS can be T, NIL, meaning the same as in OPEN.
 or it can be :ASK, meaning always ask the user,
 or :MAYBE-ASK meaning ask the user unless the answer is clear,
 or :DEFAULT meaning guess as well as possible but never ask.
Specify BYTE-SIZE to force copying in a certain byte size.
 BYTE-SIZE affects only binary mode copying.
REPORT-STREAM is a stream to output messages to as files are copied.
 If it is NIL, no messages are output.
COPY-CREATION-DATE if NIL means don't copy the file creation date;
 make now be the new file's creation date.
COPY-AUTHOR if NIL means don't copy the author; make you the new file's author.
CREATE-DIRECTORIES says whether to create a directory to be copied into.
 Values are T, NIL and :QUERY (meaning ask the user if the situation comes up).
Values returned:
1) the first value is normally the defaulted pathname to copy to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
4) the fourth value is a mode of copying, or a list of such.
 A mode of copying is a type specifier such as STRING-CHAR or (UNSIGNED-BYTE 8).
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (ARGLIST PATHNAME-OR-STREAM NEW-NAME
		    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
		    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
		    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT)))
  (FORCE-USER-TO-LOGIN)
  (LET ((RESULT
	  (IF (OR (STRINGP PATHNAME-OR-STREAM)
		  (TYPEP PATHNAME-OR-STREAM 'PATHNAME))	;Not a stream
	      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
					  (PATHNAME-OR-STREAM FILE-ERROR)
		(LET ((MERGED-PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME-OR-STREAM)))
		  (APPLY MERGED-PATHNAME
			 ':WILDCARD-MAP #'PRIMITIVE-COPY-FILE
			 ':MAYBE NIL
			 MERGED-PATHNAME (PARSE-PATHNAME NEW-NAME) OPTIONS)))
	    (LET ((TRUENAME (SEND PATHNAME-OR-STREAM ':TRUENAME)))
	      (LIST (APPLY 'PRIMITIVE-COPY-FILE
			   (FILE-PROPERTIES TRUENAME)
			   TRUENAME (PARSE-PATHNAME NEW-NAME) OPTIONS))))))
    (IF (EQ (CAAR RESULT) (CADAR RESULT))
	(VALUES (THIRD (CAR RESULT))
		(FOURTH (CAR RESULT))
		(FIFTH (CAR RESULT))
		(SIXTH (CAR RESULT)))
      (VALUES (MAPCAR 'THIRD RESULT)
	      (MAPCAR 'FOURTH RESULT)
	      (MAPCAR 'FIFTH RESULT)
	      (MAPCAR 'SIXTH RESULT)))))

(DEFUN PRIMITIVE-COPY-FILE (INPUT-PLIST-OR-PATHNAME MAPPED-PATHNAME OUTPUT-SPEC
			    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
			    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
			    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT)
			    &AUX INTYPE INPUT-PLIST INPUT-PATHNAME INPUT-TRUENAME)
  (IF (NLISTP INPUT-PLIST-OR-PATHNAME)
      (SETQ INPUT-PATHNAME INPUT-PLIST-OR-PATHNAME
	    INPUT-PLIST NIL)
      (SETQ INPUT-PATHNAME (CAR INPUT-PLIST-OR-PATHNAME)
	    INPUT-PLIST INPUT-PLIST-OR-PATHNAME))
  ;; Decide whether to copy as binary file.
  ;; Either do as told, guess from file byte size or type, or ask the user.
  (LET ((CHARACTERS?
	  (SELECTQ CHARACTERS
	    ((T NIL) CHARACTERS)
	    (:ASK (FQUERY NIL "~&Is ~A a text file? " INPUT-PATHNAME))
	    (OTHERWISE
	     ;; At this point we really need to refer to the file's property list,
	     ;; so get it if we were not given it as the first arg.
	     (IF (NULL INPUT-PLIST)
		 (SETQ INPUT-PLIST (FILE-PROPERTIES INPUT-PATHNAME)
		       INPUT-PATHNAME (CAR INPUT-PLIST)))
	     (LET ((BYTE-SIZE (GET INPUT-PLIST ':BYTE-SIZE)))
	       (COND ((MEMQ BYTE-SIZE '(7 8)) T)
		     ((EQ BYTE-SIZE 16.) NIL)
		     ((MEMBER (SETQ INTYPE (SEND INPUT-PATHNAME ':CANONICAL-TYPE))
			      *COPY-FILE-KNOWN-TEXT-TYPES*)
		      T)
		     ((MEMBER INTYPE *COPY-FILE-KNOWN-BINARY-TYPES*) NIL)
		     ((EQ CHARACTERS ':DEFAULT) ':DEFAULT)
		     (T (FQUERY '(:BEEP T) "~&Is ~A a text file? " INPUT-PATHNAME))))))))
    (IF (EQ BYTE-SIZE ':DEFAULT)
	(SETQ BYTE-SIZE (GET INPUT-PLIST ':BYTE-SIZE)))
    (IF (= BYTE-SIZE 36.)
	(SETQ BYTE-SIZE 12.))
    (CONDITION-CASE-IF (NOT ERROR) (ERROR)
	(WITH-OPEN-FILE (INSTREAM INPUT-PATHNAME
				  ':DIRECTION ':INPUT
				  ':CHARACTERS CHARACTERS?
				  ':BYTE-SIZE BYTE-SIZE)
	  (SETQ INPUT-TRUENAME (SEND INSTREAM ':TRUENAME))
	  (LET ((DEFAULTED-NEW-NAME
		  (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
		    (MERGE-PATHNAME-DEFAULTS
		      (SEND MAPPED-PATHNAME ':TRANSLATE-WILD-PATHNAME
			    OUTPUT-SPEC INPUT-TRUENAME)
		      INPUT-TRUENAME))))
	    (CONDITION-BIND ((DIRECTORY-NOT-FOUND
			       #'(LAMBDA (ERROR)
				   (WHEN (IF (EQ CREATE-DIRECTORIES ':QUERY)
					     (PROGN
					       (SEND QUERY-IO ':FRESH-LINE)
					       (SEND ERROR ':REPORT QUERY-IO)
					       (Y-OR-N-P "Create the directory? "))
					   CREATE-DIRECTORIES)
				     (CREATE-DIRECTORY (SEND ERROR ':PATHNAME) ':RECURSIVE T)
				     ':RETRY-FILE-OPERATION))))
	      (WITH-OPEN-FILE (OUTSTREAM DEFAULTED-NEW-NAME
					 ':DIRECTION ':OUTPUT
					 ':CHARACTERS CHARACTERS?
					 ':BYTE-SIZE BYTE-SIZE)
		(IF COPY-AUTHOR
		    (IF COPY-CREATION-DATE
			(SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
			      ':CREATION-DATE (SEND INSTREAM ':CREATION-DATE)
			      ':AUTHOR (OR (SEND INSTREAM ':GET ':AUTHOR)
					   (GET INPUT-PLIST ':AUTHOR)))
		      (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
			    ':AUTHOR (OR (SEND INSTREAM ':GET ':AUTHOR)
					 (GET INPUT-PLIST ':AUTHOR))))
		  (IF COPY-CREATION-DATE
		      (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
			    ':CREATION-DATE (SEND INSTREAM ':CREATION-DATE))))
		(STREAM-COPY-UNTIL-EOF INSTREAM OUTSTREAM)
		(CLOSE OUTSTREAM)
		(WHEN REPORT-STREAM
		  (FORMAT REPORT-STREAM "~&Copied ~A to ~A "
			  INPUT-TRUENAME (SEND OUTSTREAM ':TRUENAME))
		  (IF CHARACTERS?
		      (FORMAT REPORT-STREAM "in character mode.~%")
		    (FORMAT REPORT-STREAM "in byte size ~D.~%"
			    BYTE-SIZE)))
		(LIST MAPPED-PATHNAME INPUT-PATHNAME DEFAULTED-NEW-NAME
		      INPUT-TRUENAME (SEND OUTSTREAM ':TRUENAME)
		      (STREAM-ELEMENT-TYPE INSTREAM))))))
      ((FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
       (LIST MAPPED-PATHNAME INPUT-PATHNAME
	     (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
	       (MERGE-PATHNAME-DEFAULTS
		 (SEND MAPPED-PATHNAME ':TRANSLATE-WILD-PATHNAME
		       OUTPUT-SPEC (OR INPUT-TRUENAME INPUT-PATHNAME))
		 (OR INPUT-TRUENAME INPUT-PATHNAME)))
	     INPUT-PATHNAME ERROR)))))

(DEFUN RENAME-FILE (STRING-OR-STREAM NEW-NAME &KEY (ERROR T) QUERY)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR is NIL."
  (RENAMEF STRING-OR-STREAM NEW-NAME ERROR QUERY))

(DEFUN RENAMEF (STRING-OR-STREAM NEW-NAME &OPTIONAL (ERROR-P T) QUERY?)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY?, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR-P is NIL."
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
				       (TYPEP STRING-OR-STREAM 'PATHNAME))
				   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
			      (STRING-OR-STREAM FILE-ERROR)
    (LET ((RESULT (WILDCARDED-FILE-OPERATION
		    STRING-OR-STREAM
		    #'PRIMITIVE-RENAME-FILE
		    NIL
		    (PARSE-PATHNAME NEW-NAME) ERROR-P
		    (MAKE-FILE-QUERY-FUNCTION QUERY?))))
      (IF (EQ (CAAR RESULT) (CADAR RESULT))
	  (VALUES (THIRD (CAR RESULT))
		  (FOURTH (CAR RESULT))
		  (FIFTH (CAR RESULT)))
	(VALUES (MAPCAR 'THIRD RESULT)
		(MAPCAR 'FOURTH RESULT)
		(MAPCAR 'FIFTH RESULT))))))

(DEFUN PRIMITIVE-RENAME-FILE (OLD-NAME MAPPED-PATHNAME NEW-NAME &OPTIONAL (ERROR-P T) QUERYF)
  (LET ((TRUENAME (IF (EQ OLD-NAME MAPPED-PATHNAME)
		      (SEND OLD-NAME ':TRUENAME ERROR-P)
		    OLD-NAME)))
    (IF (ERRORP TRUENAME)
	(LIST MAPPED-PATHNAME OLD-NAME NIL OLD-NAME TRUENAME)
      (LET* ((DEFAULTED-NEW-NAME
	       (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
		 (MERGE-PATHNAME-DEFAULTS
		   (SEND MAPPED-PATHNAME ':TRANSLATE-WILD-PATHNAME NEW-NAME TRUENAME)
		   TRUENAME)))
	     (RENAMED? (FUNCALL QUERYF "~&Rename ~A to ~A? "
				TRUENAME DEFAULTED-NEW-NAME))
	     (RESULT (AND RENAMED? (SEND TRUENAME ':RENAME
					 DEFAULTED-NEW-NAME ERROR-P))))
	(LIST MAPPED-PATHNAME OLD-NAME DEFAULTED-NEW-NAME TRUENAME RESULT)))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "


(DEFUN DELETE-FILE (STRING-OR-STREAM &KEY (ERROR T) QUERY)
  "Delete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY, if true, means to ask the user before deleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't delete this one,
 or another non-NIL object if the file was deleted.
 OUTCOME can be an error object only if ERROR is NIL.
ERROR does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (DELETEF STRING-OR-STREAM ERROR QUERY))

(DEFUN DELETEF (STRING-OR-STREAM &OPTIONAL (ERROR-P T) QUERY?)
  "Delete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY?, if true, means to ask the user before deleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't delete this one,
 or another non-NIL object if the file was deleted.
 OUTCOME can be an error object only if ERROR-P is NIL.
ERROR-P does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
				       (TYPEP STRING-OR-STREAM 'PATHNAME))
				   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
			      (STRING-OR-STREAM FILE-ERROR)
    (WILDCARDED-FILE-OPERATION STRING-OR-STREAM
			       #'PRIMITIVE-DELETE-FILE NIL
			       ERROR-P
			       (MAKE-FILE-QUERY-FUNCTION QUERY?))))

(DEFUN PRIMITIVE-DELETE-FILE (PATHNAME MAPPED-PATHNAME &OPTIONAL (ERROR-P T) QUERYF)
  "QUERYF should be a function that takes a format-string and a pathname
and returns T or NIL saying whether to delete that file.
If you don't want any querying, pass FILE-QUERY-TRUE as QUERYF."
  (LET ((TRUENAME (IF (EQ PATHNAME MAPPED-PATHNAME)
		      (SEND PATHNAME ':TRUENAME ERROR-P)
		    PATHNAME)))
    (IF (ERRORP TRUENAME)
	(LIST PATHNAME TRUENAME)
      (LET* ((DELETE? (FUNCALL QUERYF "~&Delete ~A? " TRUENAME))
	     (RESULT (AND DELETE? (SEND TRUENAME ':DELETE ERROR-P))))
	(LIST TRUENAME (IF (ERRORP RESULT) RESULT DELETE?))))))

(DEFUN UNDELETE-FILE (STRING-OR-STREAM &KEY (ERROR T) QUERY)
  "Undelete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.  Not all file servers support undeletion.
QUERY, if true, means to ask the user before undeleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't undelete this one,
 or another non-NIL object if the file was undeleted.
 OUTCOME can be an error object only if ERROR is NIL.
ERROR does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (UNDELETEF STRING-OR-STREAM ERROR QUERY))
  
(DEFUN UNDELETEF (STRING-OR-STREAM &OPTIONAL (ERROR-P T) QUERY?)
  "Undelete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.  Not all file servers support undeletion.
QUERY?, if true, means to ask the user before undeleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't undelete this one,
 or another non-NIL object if the file was undeleted.
 OUTCOME can be an error object only if ERROR-P is NIL.
ERROR-P does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
				       (TYPEP STRING-OR-STREAM 'PATHNAME))
				   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
			      (STRING-OR-STREAM FILE-ERROR)
    (WILDCARDED-FILE-OPERATION STRING-OR-STREAM
			       #'PRIMITIVE-UNDELETE-FILE '(:DELETED)
			       ERROR-P
			       (MAKE-FILE-QUERY-FUNCTION QUERY?))))

(DEFUN PRIMITIVE-UNDELETE-FILE (PATHNAME MAPPED-PATHNAME &OPTIONAL (ERROR-P T) QUERYF)
  "QUERYF should be a function that takes a format-string and a pathname
and returns T or NIL saying whether to delete that file.
If you don't want any querying, pass FILE-QUERY-TRUE as QUERYF."
  (LET ((TRUENAME (IF (EQ PATHNAME MAPPED-PATHNAME)
		      (WITH-OPEN-FILE (STREAM PATHNAME ':ERROR ERROR-P ':DELETED T
					      ':DIRECTION NIL)
			(IF (ERRORP STREAM) STREAM
			  (SEND STREAM ':TRUENAME)))
		    PATHNAME)))
    (IF (ERRORP TRUENAME)
	(LIST PATHNAME TRUENAME)
      (LET* ((UNDELETE? (FUNCALL QUERYF "~&Undelete ~A? " TRUENAME))
	     (RESULT (AND UNDELETE? (SEND TRUENAME ':UNDELETE ERROR-P))))
	(LIST TRUENAME (IF (ERRORP RESULT) RESULT UNDELETE?))))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFMETHOD (PATHNAME :UNDELETE) (&OPTIONAL (ERROR-P T))
  (CHANGE-FILE-PROPERTIES SELF ERROR-P ':DELETED NIL))

(DEFMETHOD (PATHNAME :TRUENAME) (&OPTIONAL (ERROR-P T))
  (WITH-OPEN-FILE (STREAM SELF ':ERROR ERROR-P)
    (IF (ERRORP STREAM) STREAM
      (FUNCALL STREAM ':TRUENAME))))

))

; From file FILES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFCOM COM-UNDELETE-FILE "Undelete a file.
If wildcards are used, many files can be undeleted." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Undelete file:" (PATHNAME-DEFAULTS))))
    (IF (SEND PATHNAME ':WILD-P)
	(LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME ':DELETED))))
	  (FORMAT T "~&Files to be undeleted:~%")
	  (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
	  (WHEN (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Undelete them all? "))
	    (DOLIST (ELT DIR)
	      (CONDITION-CASE (ERROR)
		  (SEND (CAR ELT) ':UNDELETE)
		((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
		 (FORMAT T "~&Undeletion failure: ~A" ERROR))))
	    (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
	  (UNDELETE-FILE PATHNAME)
	((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
	 (BARF VALUE))
	(:NO-ERROR
	 (FORMAT QUERY-IO "~&~A undeleted." (CAAR VALUE))))))
  DIS-NONE)

))

; From file FILES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFCOM COM-RENAME-FILE "Rename a file.
If wildcards are used, many files can be renamed." ()
  (LET* ((PATHNAME (READ-DEFAULTED-PATHNAME "Rename file:" (PATHNAME-DEFAULTS)))
	 (TO-SPEC (READ-UNDEFAULTED-PATHNAME-STRING
		    (FORMAT NIL "Rename file ~A to:" PATHNAME)
		    PATHNAME))
	 BUFFERS-CONSIDERED)
    (DECLARE (SPECIAL BUFFERS-CONSIDERED))
    (IF (SEND PATHNAME ':WILD-P)
	(LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME)))
	      (TO-PATHNAME (FS:MERGE-PATHNAMES TO-SPEC PATHNAME)))
	  (FORMAT T "~&Files to be renamed:~%")
	  (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
	  (WHEN (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Rename them all? "))
	    (DOLIST (ELT DIR)
	      (CONDITION-CASE (ERROR)
		  (SEND (CAR ELT) ':RENAME
			(SEND PATHNAME ':TRANSLATE-WILD-PATHNAME TO-PATHNAME (CAR ELT)))
		((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
		 (FORMAT T "~&Rename failure: ~A" ERROR))
		(:NO-ERROR
		 (RENAME-FILE-1 PATHNAME
				(SEND PATHNAME ':TRANSLATE-WILD-PATHNAME
				      TO-PATHNAME (CAR ELT))))))
	    (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (ERROR OLD-TRUENAME NEW-TRUENAME)
	  (RENAME-FILE PATHNAME TO-SPEC)
	((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
	 (BARF ERROR))
	(:NO-ERROR
	 (FORMAT QUERY-IO "~&~A renamed~% to ~A." OLD-TRUENAME NEW-TRUENAME)
	 (RENAME-FILE-1 OLD-TRUENAME NEW-TRUENAME)))))
  DIS-NONE)

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-USE-COLD-LOAD-STREAM ()
  (LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
    (BLOCK TOP
      (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
      (DOLIST (W LOCKED-ERROR-WINDOWS)
	(LET ((QUERY-IO COLD-LOAD-STREAM))
	  (WHEN (FQUERY NIL "Handle error in ~S in cold load stream?" W)
	    ;; Deleting the window from the list wakes up the process that
	    ;; is waiting for an unlocked window.
	    (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)
	    (RETURN-FROM TOP (SETQ LOCKED-ERROR-WINDOWS (DELQ W LOCKED-ERROR-WINDOWS)))
	    )))
      (UNWIND-PROTECT
	  (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit from the cold-load-stream breakpoint.")
	    (LET ((INHIBIT-SCHEDULING-FLAG NIL)	;NIL or BREAK would complain
		  (TERMINAL-IO COLD-LOAD-STREAM))
	      (FORMAT TERMINAL-IO "~&Package ~A." (SI:PKG-NAME PACKAGE))
	      (BREAK "using cold load stream.")))
	(EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM)))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN USE-COLD-LOAD-STREAM (STRING)
  (UNLESS (EQ TERMINAL-IO TV:COLD-LOAD-STREAM)
    (SETQ TERMINAL-IO TV:COLD-LOAD-STREAM)
    (SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
    (SETQ COLD-LOAD-STREAM-DEBUGGER-SG CURRENT-STACK-GROUP))
  (SETQ TV:COLD-LOAD-STREAM-OWNS-KEYBOARD T)
  (FUNCALL TERMINAL-IO ':HOME-CURSOR)
  (FUNCALL TERMINAL-IO ':CLEAR-EOL)
  (FORMAT TERMINAL-IO "--> ~A, using the cold load stream <--~2%" STRING))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SECOND-LEVEL-ERROR-HANDLER (SG EH-ERROR &OPTIONAL (IGNORE T)
				   ;; (IGNORE T) is never passed by callers.
				   ;; It forces off the fast arg option
				   ;; which persuades the compiler to bind
				   ;; INHIBIT-SCHEDULING-FLAG at function entry,
				   ;; preventing an abort at beginning of this function.
				   &AUX MSG
				   (INHIBIT-SCHEDULING-FLAG T)
				   (SI:PRINT-READABLY NIL)
				   (PACKAGE SI:PKG-USER-PACKAGE)
				   (DEFAULT-CONS-AREA ERROR-HANDLER-AREA)
				   SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD
				   (ERROR-HANDLER-RUNNING T)
				   (ERROR-HANDLER-REPRINT-ERROR T)
				   (TERMINAL-IO (OR (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						      '*DEBUG-IO-OVERRIDE* SG)
						    (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						      'DEBUG-IO SG)
						    (SYMEVAL-IN-STACK-GROUP 'TERMINAL-IO SG)))
				   (STANDARD-INPUT SI:SYN-TERMINAL-IO)
				   (STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
				   (QUERY-IO SI:SYN-TERMINAL-IO)
				   (DEBUG-IO SI:SYN-TERMINAL-IO)
				   (*DEBUG-IO-OVERRIDE* NIL)
				   ;; In case we want to set CURRENT-PROCESS to nil.
				   (CURRENT-PROCESS CURRENT-PROCESS)
				   CURRENT-FRAME ERROR-LOCUS-FRAME
				   INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
  (UNLESS TERMINAL-IO
    (SETQ MSG "TERMINAL-IO is NIL"))
  (COND ((EQ SG SI:SCHEDULER-STACK-GROUP)
	 (SETQ MSG "Error in the scheduler"))
	((AND (BOUNDP 'TV:KBD-PROCESS)
	      (EQ CURRENT-PROCESS TV:KBD-PROCESS))
	 (SETQ MSG "Error in the keyboard process"))
	((AND (BOUNDP 'TV:MOUSE-PROCESS)
	      (EQ CURRENT-PROCESS TV:MOUSE-PROCESS))
	 (SETQ MSG "Error in the mouse process")))
  ;; Get rid of call to error-handler sg
  (LET ((RP (SG-REGULAR-PDL SG)) (AP (SG-AP SG)))
    (IF (NEQ (AREF RP AP) %ERROR-HANDLER-STACK-GROUP)
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP not found on pdl where expected"))
    (IF ( (RP-DESTINATION RP AP) 0)		;D-IGNORE
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP called with bad destination"))
    (IF ( (SG-REGULAR-PDL-POINTER SG) (1+ AP))
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP called with wrong number of args"))
    (SETF (SG-IPMARK SG) (SG-NEXT-OPEN SG AP))
    (SETF (SG-AP SG) (SETQ AP (SG-NEXT-ACTIVE SG AP)))
    (SETF (SG-FLAGS-QBBFL SG)			;Must correspond to current frame to work!
	  (RP-BINDING-BLOCK-PUSHED RP AP))
    (DOTIMES (I 5)				;Pop p3zero, function, and arg
      (SG-REGPDL-POP SG))
    ;; Now, if current frame is a foothold, restore to the previous state.  This will
    ;; normally be the case for :BREAK
    (IF (EQ (AREF RP AP) #'FOOTHOLD) (SG-RESTORE-STATE SG 0)))
  ;; Handle weird things like (BREAK): create a condition-object.
  (IF (CONSP EH-ERROR)
      (SETQ EH-ERROR (APPLY 'MAKE-CONDITION EH-ERROR)))
  (SETF (SG-TRAP-TAG SG) EH-ERROR)
  ;; Clear the SG's trap-on-call flag so that our uses of SG-APPLY will not trap.
  ;; The SG-RESTORE-STATE, above, may have restored the flag to 1.
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
  (ASSURE-DISPATCH-SET-UP)
  (ASSURE-FREE-SPACE)
  (AND MSG (USE-COLD-LOAD-STREAM MSG))
  ;; Turn on interrupts if not in cold load stream.
  (UNLESS (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL))
  ;; If not running in the scheduler, give us a run reason in case we died after
  ;; becoming inactive, before getting back to the scheduler.
  (OR (NULL CURRENT-PROCESS)
      (FUNCALL CURRENT-PROCESS ':RUN-REASON CURRENT-STACK-GROUP))
  (IF (VARIABLE-BOUNDP TV:COLD-LOAD-STREAM-OWNS-KEYBOARD)
      (SETQ SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD TV:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (LET-GLOBALLY ((TV:COLD-LOAD-STREAM-OWNS-KEYBOARD
		   (OR TV:COLD-LOAD-STREAM-OWNS-KEYBOARD
		       (EQ TERMINAL-IO TV:COLD-LOAD-STREAM))))
    ;; Try to see if TERMINAL-IO is reasonable and if not fix it.
    (LET ((WO (ERRSET (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS) NIL))
	  (ERROR-HANDLER-REPRINT-ERROR NIL))
      (IF (NULL WO) (USE-COLD-LOAD-STREAM "TERMINAL-IO clobbered")
	(COND ((MEMQ ':NOTICE (CAR WO))
	       (DO () (())
		 (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Continue entering the debugger.")
		   (LET (;; :NOTICE can change TERMINAL-IO of a background process
			 (OLD-TIO TERMINAL-IO)
			 ;; Send this message in non-erring stack
			 (WINDOW-BAD (FUNCALL TERMINAL-IO ':NOTICE ':ERROR)))
		     (IF (EQ WINDOW-BAD 'TV:COLD-LOAD-STREAM)
			 (USE-COLD-LOAD-STREAM "window-system problems")
		       (AND (NEQ TERMINAL-IO OLD-TIO)
			    (NOT WINDOW-BAD)
			    (SG-FUNCALL SG #'SET 'TERMINAL-IO TERMINAL-IO))))
		   (RETURN NIL)))))))
    ;; Turn off interrupts if switched to cold load stream.
    (IF (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
	(SETQ INHIBIT-SCHEDULING-FLAG T))
    ;; Setting this causes the previous error to be reprinted if we abort to it.
    (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)
    ;; Give these reasonable values in case of error in the :FIND-CURRENT-FRAME method.
    (SETQ ERROR-LOCUS-FRAME (SG-AP SG)
	  CURRENT-FRAME (SG-AP SG)
	  INNERMOST-VISIBLE-FRAME (SG-AP SG))
    ;; These catches are so that quitting out of the printing of the error message
    ;; leaves you in the error handler at its
    ;; normal command level rather than quitting out of the whole program.
    (*CATCH 'QUIT
      (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Abort printing error message, enter debugger.")
	(SETF (VALUES ERROR-LOCUS-FRAME CURRENT-FRAME
		      INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
	      (SEND EH-ERROR ':FIND-CURRENT-FRAME SG))
	;; Print the error message, using appropriate package, base, etc.
	(INHERITING-VARIABLES-FROM (SG)
	  (PRINT-CAREFULLY "error message"
	    (SEND STANDARD-OUTPUT ':FRESH-LINE)
	    (SEND EH-ERROR ':PRINT-ERROR-MESSAGE
		  SG NIL STANDARD-OUTPUT))
	  (PRINT-BRIEF-ERROR-BACKTRACE SG EH-ERROR)
	  (SEND EH-ERROR ':MAYBE-CLEAR-INPUT STANDARD-INPUT))))
    ;; Offer any special commands, such as wrong-package correction.
    ;; Then enter the command loop.
    (SEND EH-ERROR ':DEBUGGER-COMMAND-LOOP SG)))

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN WAIT-TILL-SAFE-FOR-ERROR (WINDOW FUNCTION &REST ARGS)
  "Wait until either (APPLY FUNCTION ARGS) is non-NIL or user does Terminal Call.
If user does Terminal Call (and picks WINDOW therein), returns
the symbol COLD-LOAD-STREAM; if FUNCTION returns non-NIL, we return NIL.
If *WINDOWS-LOCKED-ERROR-QUERY* is non-NIL, we immediately
ask user to choose to use cold load stream, unlock window locks, or do nothing.
/"Do nothing/" means we wait, as described above."
  (COND ((NOT (APPLY FUNCTION ARGS))
	 (UNWIND-PROTECT
	   (PROGN
	     (WITHOUT-INTERRUPTS (PUSH WINDOW LOCKED-ERROR-WINDOWS))
	     (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
	     (IF *WINDOWS-LOCKED-ERROR-QUERY*
		 ;; Situation has not been resolved yet; query user in cold-load stream
		 (LET (ANSWER)
		   (LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
		     (LET ((QUERY-IO COLD-LOAD-STREAM))
		       (FUNCALL COLD-LOAD-STREAM ':CLEAR-INPUT)
		       (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
		       (SETQ ANSWER
			     (FQUERY '(:CHOICES (((:C "Cold load stream") #/C)
						 ((:U "Clear all locks") #/U)
						 ((:N "Nothing now") #/N)))
				     "How do you want to handle error in process ~A?
You can handle it in the error handler by typing
        C  to use the cold-load stream (like Terminal Call),
        U  to forcibly unlock all windows so a notification can come out 
           (like Terminal Control-Clear-input)
     or N  to tell it to wait until you do some other thing. "
				     (PROCESS-NAME CURRENT-PROCESS))))
		     (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T))
		   (IF (EQ ANSWER ':C)
		       ;; Answer C means use cold load stream now.
		       (WITHOUT-INTERRUPTS
			 (SETQ LOCKED-ERROR-WINDOWS
			       (DELQ WINDOW LOCKED-ERROR-WINDOWS 1))))
		   ;; Answer U means unlock locks, allowing notification now.
		   (IF (EQ ANSWER ':U) (SHEET-CLEAR-LOCKS))))
	     ;; Wait until either the function supplied to us returns T
	     ;; or someone removes this window from the locked list
	     ;; (which means, telling us to use the cold load stream)
	     (PROCESS-WAIT
	       "Error notify"
	       #'(LAMBDA (FUNCTION ARGS WINDOW)
		   (OR (APPLY FUNCTION ARGS)
		       (NOT (MEMQ WINDOW LOCKED-ERROR-WINDOWS))))
	       FUNCTION ARGS WINDOW)
	     (IF (NOT (MEMQ WINDOW LOCKED-ERROR-WINDOWS))
		 'COLD-LOAD-STREAM))
	   (WITHOUT-INTERRUPTS (SETQ LOCKED-ERROR-WINDOWS
				     (DELQ WINDOW LOCKED-ERROR-WINDOWS 1)))))))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-ESC-OUTPUT-HOLD (IGNORE)
  (PROG (P W LOCKED ANS)
    (COND ((AND (SETQ P LAST-WHO-LINE-PROCESS)
		(MEM 'EQUALP (PROCESS-WHOSTATE P) '("Output Hold" "Lock" "Window Lock"))
		(TYPEP (SETQ W (CAR (PROCESS-WAIT-ARGUMENT-LIST P))) 'SHEET))
	   ;; Bludgeon our way past any deadlocks, e.g. due to the process P holding
	   ;; the lock on the window we are trying to expose, or on something we need
	   ;; to de-expose in order to expose it.  This code probably doesn't do a good
	   ;; enough job explaining what is going on to the user.
	   (COND ((AND (CONSP (SHEET-LOCK W))	;Only temp-locked?
		       (ZEROP (SHEET-LOCK-COUNT W))
		       (LOOP FOR TW IN (SHEET-LOCK W)
			     ALWAYS (SHEET-CAN-GET-LOCK TW)))
		  (SHEET-FREE-TEMPORARY-LOCKS W))
		 ((OR (NOT (SHEET-CAN-GET-LOCK (SETQ LOCKED W)))
		      (AND (SHEET-SUPERIOR W)
			   (LOOP FOR I IN (SHEET-EXPOSED-INFERIORS (SHEET-SUPERIOR W))
				 THEREIS (AND (SHEET-OVERLAPS-SHEET-P W I)
					      (NOT (SHEET-CAN-GET-LOCK (SETQ LOCKED I)))))))
		  (LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
		    (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
		    (LET ((QUERY-IO COLD-LOAD-STREAM))
		      (FORMAT QUERY-IO "Cannot expose ~S because~@
					~:[~S~;~*it~] is locked by ~S."
			      W (EQ W LOCKED) LOCKED (SHEET-LOCK LOCKED))
		      (WHEN (AND (TYPEP (SHEET-LOCK LOCKED) 'SI:PROCESS)
				 (NOT (SHEET-EXPOSED-P W))
				 (FQUERY NIL "Attempt to expose, pretending to be ~S? "
					 (SHEET-LOCK LOCKED)))
			(EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)
			(LET ((RESULT (EXPOSE-LOCKED-WINDOW W (SHEET-LOCK LOCKED))))
			  (COND ((EQ RESULT T) (RETURN NIL)))
			  (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM T)
			  (COND (RESULT (FORMAT QUERY-IO "~&That got an error:~%~A" RESULT))
				(T (FORMAT QUERY-IO "~&That did not finish in 10 seconds.")))))
		      (SETQ ANS (FQUERY '(:CHOICES (((T "Yes.") #/Y #\SP #/T)
						    ((NIL "No.") #/N #\RUBOUT)
						    ((EH "To debugger.") #/D #/E))
					  :BEEP T)
					"Forcibly unlock all window-system locks? "))
		      (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)))
		  (COND ((EQ ANS 'EH)
			 (SETQ DEBUG-IO COLD-LOAD-STREAM)
			 (FUNCALL P ':INTERRUPT %ERROR-HANDLER-STACK-GROUP '(:BREAK))
			 (RETURN NIL))	;Don't try to expose
			(ANS (SHEET-CLEAR-LOCKS))))
		 ((AND (SHEET-EXPOSED-P W)	;This can happen, I don't know how
		       (NOT (SHEET-LOCK W))
		       (SHEET-OUTPUT-HELD-P W))
		  (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
		  (IF (LET ((QUERY-IO COLD-LOAD-STREAM))
			(FQUERY '(:BEEP T)
				"~S is output-held for no apparent reason.~@
				 If you know the circumstances that led to this, please~@
				 mail in a bug report describing them.  ~
				 Do you want to forcibly clear output-hold? "
				W))
		      (SETF (SHEET-OUTPUT-HOLD-FLAG W) 0))
		  (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)))
	   (FUNCALL W ':EXPOSE))
	  ((BEEP)))))

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN SHEET-PREPARE-SHEET-INTERNAL (SHEET &AUX LOCK)
  "This is an internal function for PREPARE-SHEET, and must be called with
INHIBIT-SCHEDULING-FLAG bound."
  (WHEN COLD-LOAD-STREAM-OWNS-KEYBOARD
    (UNLESS (SHEET-ME-OR-MY-KID-P SHEET WHO-LINE-SCREEN)
      (PROCESS-WAIT "Screen Lock" #'(LAMBDA () (NOT COLD-LOAD-STREAM-OWNS-KEYBOARD)))))
  (DO () ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
	       (NOT (SHEET-OUTPUT-HELD-P SHEET))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
	(FUNCALL SHEET ':OUTPUT-HOLD-EXCEPTION)
	(PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET))
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (IF (SHEET-INFERIORS SHEET)
      (MAP-OVER-EXPOSED-SHEET
	#'(LAMBDA (SHEET)
	    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
	      (OPEN-BLINKER BLINKER)))
	SHEET)
      ;; No need to do full hair if no inferiors
      (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
	(OPEN-BLINKER BLINKER)))
  (AND (SHEET-EXPOSED-P SHEET)
       (SHEET-OPEN-ALL-BLINKERS (SHEET-SUPERIOR SHEET))))

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN BLINKER-CLOCK (BLINKER-DELTA-TIME)
  (DECLARE (SPECIAL BLINKER-DELTA-TIME))
  (DOLIST (S ALL-THE-SCREENS)
    (AND (SHEET-EXPOSED-P S)
	 (OR (NEQ S MAIN-SCREEN)
	     (NOT COLD-LOAD-STREAM-OWNS-KEYBOARD))
	 (BLINKER-CLOCK-INTERNAL S))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO FILE-RETRY-NEW-PATHNAME
	  ((PATHNAME-VARIABLE . CONDITION-NAMES) &BODY BODY)
  "Execute BODY with a handler for CONDITION-NAMES that reads a new pathname and tries again.
If one of those conditions is signaled within BODY, 
a new pathname is read and put in PATHNAME-VARIABLE,
and then BODY is executed again.
This is most useful when BODY is an OPEN, DELETEF, etc."
  (LET ((TAG (GENSYM)))
    `(BLOCK FILE-RETRY-NEW-PN
       (TAGBODY
	RETRY
	   (RETURN-FROM FILE-RETRY-NEW-PN
	     (CATCH-CONTINUATION ',TAG
		 #'(LAMBDA (NEW-PATHNAME) (SETQ ,PATHNAME-VARIABLE NEW-PATHNAME)
			   (GO RETRY))
		 NIL
	       (CONDITION-RESUME `(,',CONDITION-NAMES :NEW-PATHNAME T
				   ("Try again with a new pathname, not telling the callers.")
				   FILE-RETRY-RESUME-HANDLER ,',TAG)
		 (CONDITION-BIND ((,CONDITION-NAMES 'FILE-RETRY-HANDLER
				   ,PATHNAME-VARIABLE ',TAG))
		   . ,BODY))))))))

(DEFUN FILE-RETRY-RESUME-HANDLER (ERROR-OBJECT NEW-PATHNAME
				  &OPTIONAL (TAG 'FILE-RETRY-NEW-PATHNAME))
  ERROR-OBJECT
  (*THROW TAG NEW-PATHNAME))

(DEFUN FILE-RETRY-HANDLER (ERROR-OBJECT PATHNAME
			   &OPTIONAL (TAG 'FILE-RETRY-NEW-PATHNAME))
  (FORMAT QUERY-IO "~&~A" ERROR-OBJECT)
  (SETQ PATHNAME (FS:PARSE-PATHNAME PATHNAME))
  (LET* ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T)
	 (FS:*NAME-SPECIFIED-DEFAULT-TYPE* NIL)
	 (INPUT
	   (PROMPT-AND-READ `(:PATHNAME-OR-END :DEFAULTS ,PATHNAME)
			    "~&Pathname to use instead (default ~A)~%or ~C to enter debugger: "
			    PATHNAME #\END)))
    (IF (EQ INPUT #\END) NIL
      (*THROW TAG INPUT))))

(DEFMACRO FILE-RETRY-NEW-PATHNAME-IF
	  (COND-FORM (PATHNAME-VARIABLE . CONDITION-NAMES) &BODY BODY)
  "Execute BODY with a handler for CONDITION-NAMES that reads a new pathname and tries again.
If COND-FORM evaluates non-NIL, then if one of those conditions is signaled within BODY, 
a new pathname is read and put in PATHNAME-VARIABLE, and then BODY is executed again.
This is most useful when BODY is an OPEN, DELETEF, etc."
  (LET ((TAG (GENSYM)))
    `(BLOCK FILE-RETRY-NEW-PN
       (TAGBODY
	RETRY
	   (RETURN-FROM FILE-RETRY-NEW-PN
	     (CATCH-CONTINUATION ',TAG
		 #'(LAMBDA (NEW-PATHNAME) (SETQ ,PATHNAME-VARIABLE NEW-PATHNAME)
			   (GO RETRY))
		 NIL
	       (CONDITION-RESUME `(,',CONDITION-NAMES :NEW-PATHNAME T
				   ("Try again with a new pathname, not telling the callers.")
				   FILE-RETRY-RESUME-HANDLER ,',TAG)
		 (CONDITION-BIND-IF ,COND-FORM
				    ((,CONDITION-NAMES 'FILE-RETRY-HANDLER
				      ,PATHNAME-VARIABLE ',TAG))
		   . ,BODY))))))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO WITH-OPEN-FILE-SEARCH ((STREAM (OPERATION DEFAULTS AUTO-RETRY)
					 TYPE-LIST-AND-PATHNAME-FORM
					 . OPEN-OPTIONS)
				 &BODY BODY)
  "Open one of several filenames, the same except for the type component.
Binds the variable STREAM to the resulting stream, executes the BODY, then closes the stream.
OPEN-OPTIONS are alternating keywords and values, passed to OPEN.
TYPE-LIST-AND-PATHNAME-FORM is evaluated to get two values:
 a list of pathname types to try, and a base pathname.
The base pathname is merged successively with each type in the list.
This is done using FS:MERGE-PATHNAME-DEFAULTS, with DEFAULTS's value
used as the second argument and the type to be tried as the third argument.
As soon as a merged pathname succeeds in being opened, we execute BODY.
If they all fail, an error is signaled with condition FS:MULTIPLE-FILE-NOT-FOUND.
OPERATION should eval to the name of the calling function; it is used for signaling.
If AUTO-RETRY evals to non-NIL, then the user is asked to type a new
pathname to retry with."
  (LET ((BASE-PATHNAME-VAR (GENSYM))
	(TYPE-LIST-VAR (GENSYM))
	(DEFAULTS-VAR (GENSYM))
	(AUTO-RETRY-VAR (GENSYM)))
    `(LET ((,DEFAULTS-VAR ,DEFAULTS)
	   (,AUTO-RETRY-VAR ,AUTO-RETRY))
       (MULTIPLE-VALUE-BIND (,TYPE-LIST-VAR ,BASE-PATHNAME-VAR)
	   ,TYPE-LIST-AND-PATHNAME-FORM
	 (FILE-RETRY-NEW-PATHNAME-IF ,AUTO-RETRY-VAR (,BASE-PATHNAME-VAR FS:FILE-ERROR)
	   (WITH-OPEN-STREAM (,STREAM
			      (FS:OPEN-FILE-SEARCH ,BASE-PATHNAME-VAR ,TYPE-LIST-VAR
						   ,DEFAULTS-VAR ,OPERATION
						   . ,OPEN-OPTIONS))
	     . ,BODY))))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN LOAD-1 (FILE &OPTIONAL PKG NONEXISTENT-OK-FLAG DONT-SET-DEFAULT-P NO-MSG-P)
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
	  (OR (SEND FILE ':SEND-IF-HANDLES ':TRUENAME) T)))
    (LET ((PATHNAME (PARSE-PATHNAME FILE)))
      (CATCH-ERROR-RESTART (ERROR "Give up on loading ~A." FILE)
	(CONDITION-CASE-IF NONEXISTENT-OK-FLAG ()
	    (WITH-OPEN-FILE-SEARCH (STREAM ('LOAD LOAD-PATHNAME-DEFAULTS
					    (NOT NONEXISTENT-OK-FLAG))
					   (VALUES
					     (LIST (SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE
						     PATHNAME)
						   ':LISP)
					     PATHNAME)
					   ':CHARACTERS ':DEFAULT)
	      ;; Set the defaults from the pathname we finally opened
	      (OR DONT-SET-DEFAULT-P
		  (SET-DEFAULT-PATHNAME (SEND STREAM ':PATHNAME) LOAD-PATHNAME-DEFAULTS))
	      ;; If the file was a character file, read it, else try to fasload it.
	      (FUNCALL (IF (FUNCALL STREAM ':CHARACTERS)
			   #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
		       STREAM PKG NO-MSG-P)
	      (SEND STREAM ':TRUENAME))
	  (MULTIPLE-FILE-NOT-FOUND
	   NIL))))))

))

; From file PATCH.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "


(DEFVAR SYSTEM-STATUS-ALIST '((:EXPERIMENTAL "Experimental" "Exp" "experimental")
			      (:RELEASED "" "" "released")
			      (:OBSOLETE "Obsolete" "Obs" "obsolete")
			      (:INCONSISTENT "Inconsistent (unreleased patches loaded)"
					     "Bad" "inconsistent (unreleased patches loaded)")
			      (:BROKEN "Broken" "Broke" "broken")))

(DEFUN SYSTEM-VERSION-INFO (&OPTIONAL (BRIEF-P NIL) &AUX (FIRST T) TEM)
  "Return a one-line string giving the versions of all patchable systems.
Also gives the microcode version, and the loaded band's disk label comment.
With BRIEF-P, return stuff suitable for disk label comment."
  (WITH-OUTPUT-TO-STRING (S)
    (UNLESS (AND BRIEF-P (EQ (PATCH-STATUS (GET-PATCH-SYSTEM-NAMED "System")) ':INCONSISTENT))
      ;; If some system is inconsistent but System is not,
      ;; make sure "Bad" appears at the front.
      (DOLIST (SYS PATCH-SYSTEMS-LIST)
	(WHEN (EQ (PATCH-STATUS SYS) ':INCONSISTENT)
	  (FORMAT S (IF BRIEF-P "Bad " "Don't-dump-a-band! "))
	  (RETURN))))
    (DOLIST (SYS PATCH-SYSTEMS-LIST)
      (COND ((NOT (AND BRIEF-P (SYSTEM-SHOULD-NOT-APPEAR-IN-DISK-LABEL (PATCH-NAME SYS))))
	     (IF (NOT FIRST)
		 (FUNCALL S ':STRING-OUT (IF BRIEF-P " " ", ")))
	     (SETQ FIRST NIL)
	     (COND ((NULL (SETQ TEM (ASSQ (PATCH-STATUS SYS) SYSTEM-STATUS-ALIST)))
		    (SETQ TEM (STRING (PATCH-STATUS SYS))))
		   (BRIEF-P
		    (SETQ TEM (THIRD TEM)))
		   (T
		    (SETQ TEM (SECOND TEM))))
	     (COND ((NOT (EQUAL TEM ""))
		    (FUNCALL S ':STRING-OUT TEM)
		    (FUNCALL S ':TYO #\SP)))
	     (IF (NOT (AND BRIEF-P (EQUALP (PATCH-NAME SYS) "System")))
		 (FORMAT S "~A " (IF (NOT BRIEF-P) (PATCH-NAME SYS)
				     (SYSTEM-SHORT-NAME (PATCH-NAME SYS)))))
	     (FORMAT S "~D.~D"
		     (PATCH-VERSION SYS) (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST SYS)))))))
    (IF (NOT BRIEF-P)
	(FORMAT S ", microcode ~D" %MICROCODE-VERSION-NUMBER))
    (AND (PLUSP (STRING-LENGTH SYSTEM-ADDITIONAL-INFO))
	 (FORMAT S ", ~A" SYSTEM-ADDITIONAL-INFO))))

))

; From file QCDEFS.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFUN WARN-ON-ERRORS-CONDITION-HANDLER (CONDITION
					 &AUX
					 (CONDITION-NAMES (SEND CONDITION ':CONDITION-NAMES))
					 (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (IF SI:OBJECT-WARNINGS-OBJECT-NAME
      (PROGN (SI:MAYBE-PRINT-OBJECT-WARNINGS-HEADER)
	     (FORMAT T "~%Warning: ")
	     (LEXPR-FUNCALL 'FORMAT T ERROR-WARNING-ARGS))
    (PRINT-ERROR-WARNING-HEADER))
  (UNLESS EH:ERRSET-STATUS
    (COND ((AND (MEMQ 'SYS:PARSE-ERROR CONDITION-NAMES)
		(SEND CONDITION ':PROCEED-TYPE-P ':NO-ACTION))
	   (WARN 'READ-ERROR ':ERROR "~A" (SEND CONDITION ':REPORT-STRING))
	   (LET (BP REG)
	     (IF WARN-ON-ERRORS-STREAM
		 (SETQ BP (SEND WARN-ON-ERRORS-STREAM ':SEND-IF-HANDLES ':READ-BP)))
	     (AND BP
		  (SETQ REG (ZWEI:MAKE-REGISTER-NAME #/.))
		  (NOT (GET REG 'ZWEI:POINT))
		  (PROGN
		    (FORMAT T "~&Position of this error saved in ZWEI register /"./".")
		    (ZWEI:SAVE-POSITION-IN-REGISTER REG BP))))
	   ':NO-ACTION)
	  (T
	   (SI:RECORD-WARNING NIL ':ERROR NIL "~A"
			      (LEXPR-FUNCALL 'FORMAT NIL ERROR-WARNING-ARGS))
	   ;; Make a string now, in case the condition object points at data
	   ;; that is in a temporary area.
	   (WARN ERROR-WARNING-TYPE ':ERROR "~A" (SEND CONDITION ':REPORT-STRING))
	   (COND ((AND WARN-ON-ERRORS
		       (NOT (MEMQ 'SYS:PDL-OVERFLOW CONDITION-NAMES))
		       (NOT (SEND CONDITION ':DANGEROUS-CONDITION-P))
		       (NOT (SEND CONDITION ':DEBUGGING-CONDITION-P)))
		  (FORMAT T "~&TO DEBUG THIS, recompile with COMPILER:WARN-ON-ERRORS set to NIL.")
		  'WARN-ON-ERRORS))))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "


(DEFFLAVOR PARSE-ERROR (STREAM) (FERROR) :SETTABLE-INSTANCE-VARIABLES)
(DEFFLAVOR PARSE-FERROR () (PARSE-ERROR) :ALIAS-FLAVOR)

(DEFSIGNAL PARSE-ERROR-1 PARSE-ERROR ()
	   "Error in parsing input; rubout handler should handle it.")

(DEFUN PARSE-FERROR (FORMAT-STRING &REST ARGS)
  (APPLY 'CERROR ':NO-ACTION NIL 'PARSE-ERROR-1 FORMAT-STRING ARGS))

(DEFMETHOD (PARSE-ERROR :AFTER :INIT) (IGNORE)
  (SETQ STREAM (AND (VARIABLE-BOUNDP SI:READ-STREAM) SI:READ-STREAM)))

(DEFMETHOD (PARSE-ERROR :AFTER :PRINT-ERROR-MESSAGE) (IGNORE IGNORE -STREAM-)
  (WHEN STREAM
    (FORMAT -STREAM- "Error occurred in reading from ~S.~%" STREAM)))

(DEFMETHOD (PARSE-ERROR :CASE :PROCEED-ASKING-USER :NO-ACTION) (CONTINUATION IGNORE)
  "Continues reading, trying to ignore the problem."
  (FUNCALL CONTINUATION ':NO-ACTION))

(DEFSIGNAL READ-PACKAGE-NOT-FOUND (PACKAGE-NOT-FOUND PACKAGE-ERROR READ-ERROR PARSE-ERROR)
	   (NAME RELATIVE-TO)
	   "Package prefix not recognized as a package name.")

(DEFFLAVOR READ-END-OF-FILE () (END-OF-FILE PARSE-ERROR))

(DEFMETHOD (READ-END-OF-FILE :CASE :PROCEED-ASKING-USER :NO-ACTION) (CONTINUATION IGNORE)
  "Proceeds, closing off unfinished lists."
  (FUNCALL CONTINUATION ':NO-ACTION))

(DEFSIGNAL READ-END-OF-FILE (READ-END-OF-FILE READ-ERROR) (STREAM)
  "End of file within READ on STREAM.
SYS:READ-LIST-END-OF-FILE or SYS:READ-STRING-END-OF-FILE should be used
if they apply.")

(DEFSIGNAL READ-LIST-END-OF-FILE (READ-END-OF-FILE READ-ERROR READ-LIST-END-OF-FILE)
	   (STREAM LIST)
	   "End of file within READ constructing a list, on STREAM.
LIST is the list constructed so far.")

(DEFSIGNAL READ-STRING-END-OF-FILE (READ-END-OF-FILE READ-ERROR READ-STRING-END-OF-FILE)
	   (STREAM STRING)
	   "End of file within READ constructing a string, on STREAM.
STRING is the string read so far.")

(DEFSIGNAL READ-SYMBOL-END-OF-FILE (READ-END-OF-FILE READ-ERROR READ-SYMBOL-END-OF-FILE)
	   (STREAM STRING)
	   "End of file within READ constructing a symbol, on STREAM.
Occurs only within a vertical-bar construct.  STRING is the string read so far.")

(DEFSIGNAL READ-ERROR-1 (PARSE-ERROR READ-ERROR) ()
	   "Error other than end of file, within READ.")

(DEFSIGNAL MISSING-CLOSEPAREN (PARSE-ERROR READ-ERROR MISSING-CLOSEPAREN) ()
  "Error of open paren found in column 0 in middle of defun.")

(DEFSIGNAL PRINT-NOT-READABLE FERROR (OBJECT)
	   "Printing OBJECT, which cannot be printed so it can be read back.")

(compile-flavor-methods parse-error read-end-of-file)

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
  (*CATCH 'RETURN-FROM-RUBOUT-HANDLER
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
	     (RUBOUT-HANDLER-RE-ECHO-FLAG NIL NIL)
	     (RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))
	    (NIL)
	  (*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
	    (CONDITION-CASE (ERROR)
		(RETURN
		 (MULTIPLE-VALUE-PROG1
		   (APPLY FUNCTION ARGS)		;Call READ or whatever.
		   (SETF (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
		   (AND (RHB-TYPEIN-POINTER)
			(> (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER))
			(SETF (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER)))))
	      (SYS:PARSE-ERROR
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
		   (RETURN NIL (CADR FULL-RUBOUT-OPTION))))))))))

))

; From file COLD.LISP SRC:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :RUBOUT-HANDLER)
		     (RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 0)
  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
  (MULTIPLE-VALUE-BIND (PROMPT-STARTING-X PROMPT-STARTING-Y)
      (FUNCALL COLD-LOAD-STREAM ':READ-CURSORPOS)
    (LET ((PROMPT (CADR (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS))))
      (AND PROMPT				;Prompt if desired
	   (IF (STRINGP PROMPT)
	       (PRINC PROMPT SELF)
	     (FUNCALL PROMPT SELF NIL))))
    (*CATCH 'TV:RETURN-FROM-RUBOUT-HANDLER
      (DO ((RUBOUT-HANDLER T)			;Establish rubout handler
	   (INHIBIT-SCHEDULING-FLAG T)		;Make sure all chars come here
	   (COLD-LOAD-STREAM-ACTIVATION-CHARACTER NIL))
	  (NIL)
	(*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
	  (CONDITION-CASE (ERROR)
	      (RETURN (APPLY FUNCTION ARGS))	;Call read type function
	    (PARSE-ERROR
	     (TERPRI SELF)
	     (PRINC ">>ERROR: " SELF)
	     (SEND ERROR ':REPORT SELF)
	     (TERPRI SELF)
	     (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER)	;On error, retype buffered
	     (DO () (NIL) (FUNCALL-SELF ':TYI)))))		;and force user to edit it
	;;Maybe return when user rubs all the way back
	(AND (ZEROP (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0))
	     (LET ((FULL-RUBOUT-OPTION (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS)))
	       (WHEN FULL-RUBOUT-OPTION
		 ;; Get rid of the prompt, if any.
		 (FUNCALL COLD-LOAD-STREAM ':SET-CURSORPOS PROMPT-STARTING-X PROMPT-STARTING-Y)
		 (FUNCALL COLD-LOAD-STREAM ':CLEAR-EOL)
		 (RETURN NIL (CADR FULL-RUBOUT-OPTION)))))))))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :RUBOUT-HANDLER) (RUBOUT-HANDLER-ARGS FUNCTION
						  &REST ARGS &AUX TEM
						  (*WINDOW* *STREAM-SHEET*)
						  *STREAM-DEFER-OUTPUT-NOT-AT-END*
						  COMMAND-POINT)
  (IF *STREAM-COMMAND-POINT*
      (PROGN
	(MOVE-BP *STREAM-START-BP* *STREAM-BP*)
	(SETQ COMMAND-POINT *STREAM-COMMAND-POINT*)
	(FLUSH-BP COMMAND-POINT)
	(SETQ *STREAM-COMMAND-POINT* NIL))
    (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*))))
  (LET ((PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-ARGS)))
    (WHEN PROMPT-OPTION
      (IF (ARRAYP (CADR PROMPT-OPTION))
	  (PRINC (CADR PROMPT-OPTION) SELF)
	(FUNCALL (CADR PROMPT-OPTION) SELF NIL))
      (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
  (STREAM-MAYBE-REDISPLAY)
  (*CATCH 'TV:RETURN-FROM-RUBOUT-HANDLER
    (DO ((RUBOUT-HANDLER T)			;Establish rubout handler
	 (*SRE-ACTIVATION-CHARACTER* NIL)
	 (*STREAM-PASS-THROUGH* (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-ARGS)))
	 (*STREAM-COMMAND-HANDLER*
	   (ASSQ ':COMMAND RUBOUT-HANDLER-ARGS))
	 (*STREAM-ACTIVATION-HANDLER*
	   (ASSQ ':ACTIVATION RUBOUT-HANDLER-ARGS))
	 (*STREAM-DO-NOT-ECHO*
	   (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-ARGS)))
	 (*STREAM-PREEMPTABLE* (ASSQ ':PREEMPTABLE RUBOUT-HANDLER-ARGS)))
	(())
      (WITH-BP (START-OF-MSG-BP *STREAM-START-BP* ':NORMAL)
	(WITH-BP (END-OF-MSG-BP *STREAM-START-BP* ':NORMAL)
	  (*CATCH 'RUBOUT-HANDLER
	    (CONDITION-CASE (ERROR)
		(LET ((*SRE-STREAM-BP* *STREAM-BP*)
		      (*SRE-STREAM-START-BP* *STREAM-START-BP*)
		      (*SRE-WINDOW* *STREAM-SHEET*)
		      *SRE-INPUT-END-BP*
		      (*SRE-INPUT-POINT* COMMAND-POINT))
		  (CONDITION-BIND ((ERROR 'STREAM-READ-ERROR-HANDLER))
		    (RETURN (MULTIPLE-VALUE-PROG1
			      (APPLY FUNCTION ARGS)
			      (LET ((*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
				(DELETE-INTERVAL *STREAM-BP*
						 (INTERVAL-LAST-BP *INTERVAL*)))))))
	      (SYS:PARSE-ERROR
	       (LET ((*STREAM-DEFER-OUTPUT-NOT-AT-END* T))
		 (TERPRI SELF)
		 (PRINC ">>ERROR: " SELF)
		 (SEND ERROR ':REPORT SELF)
		 (TERPRI SELF))
	       (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
	       (MOVE-BP END-OF-MSG-BP *STREAM-START-BP*)
	       (MOVE-BP *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	       (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
	       (STREAM-REDISPLAY)
	       (DO () (NIL) (FUNCALL-SELF ':TYI)))))
	  ;; Here if editor throws to RUBOUT-HANDLER
	  ;; to cause the input we have to be read over again.
	  ;; First, delete any error message we got from a previous parsing.
	  (COND ((NOT (BP-= START-OF-MSG-BP END-OF-MSG-BP))
		 (DELETE-INTERVAL START-OF-MSG-BP END-OF-MSG-BP T)
		 (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
		 (STREAM-REDISPLAY T)))
	  ;; Now start over again reading from the front of the input.
	  (MOVE-BP *STREAM-BP* *STREAM-START-BP*)
	  (SETQ COMMAND-POINT NIL)
	  (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)))
      ;; When a rubout or other editing operation is done, throws back to that
      ;; catch to reread the input.  But if the :FULL-RUBOUT option was specified
      ;; and everything was rubbed out, we return NIL and the specified value.
      (AND (BP-= *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	   (SETQ TEM (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-ARGS))
	   (RETURN NIL (CADR TEM))))))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "


(DEFMETHOD (BASIC-STREAM :ACCEPT) ()
  (ACCEPT CONNECTION))

(DEFMETHOD (BASIC-STREAM :REJECT) (&OPTIONAL REASON)
  (REJECT CONNECTION (OR REASON "")))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFUN OPEN-STREAM (HOST CONTACT-NAME &KEY &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
						     (TIMEOUT (* 10. 60.))
						     (DIRECTION ':BIDIRECTIONAL)
						     (ERROR T)
						     (CHARACTERS T)
						     (ASCII-TRANSLATION NIL)
				      &AUX CONN)
  "Open a chaosnet connection and return a stream that does i//o to it.
HOST is the host to connect to; CONTACT-NAME is the contact name at that host.
The keyword arguments are:
:WINDOW-SIZE - number of packets to allow in transit to this host over the connection.
:TIMEOUT - how long to wait before assuming the host is down.
:ASCII-TRANSLATION - if non-NIL, assume the data on the connection is in ASCII
 and translate to and from the Lisp machine character set as appropriate.
:DIRECTION, :CHARACTERS, :ERROR - as in OPEN.  :DIRECTION defaults to ':BIDIRECTIONAL."
  (CONDITION-CASE-IF (NOT ERROR) (ERROR-OBJECT)
        (SETQ CONN (IF HOST
		       (CONNECT HOST CONTACT-NAME WINDOW-SIZE TIMEOUT)
		     (LISTEN CONTACT-NAME WINDOW-SIZE)))
    (SYS:REMOTE-NETWORK-ERROR ERROR-OBJECT)
    (:NO-ERROR 
      (MAKE-STREAM CONN ':DIRECTION DIRECTION ':CHARACTERS CHARACTERS
			':ASCII-TRANSLATION ASCII-TRANSLATION))))

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN PATHNAME-DEFAULTS (&OPTIONAL (DEFAULTS *PATHNAME-DEFAULTS*) (BUFFER *INTERVAL*)
			  &AUX (MAJOR-MODE (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
					     (SEND BUFFER ':MAJOR-MODE)))
			  TEM)
  "Update DEFAULTS for BUFFER, and return it.
DEFAULTS is a defaults-alist.
We update it by setting the defaults in it
based on BUFFER's visited pathname, or its name."
  (AND (TYPEP BUFFER 'FILE-BUFFER)
       (FS:SET-DEFAULT-PATHNAME
	 (OR (AND (SETQ TEM (GET MAJOR-MODE 'PATHNAME-DEFAULTING-FUNCTION))
		  (FUNCALL TEM DEFAULTS BUFFER))
	     (BUFFER-PATHNAME BUFFER)
	     (LET ((TYPE (CAR (RASSQ (INTERN-SOFT
				       (STRING-UPCASE
					 (SYMEVAL MAJOR-MODE))
				       "USER")
				     FS:*FILE-TYPE-MODE-ALIST*)))
		   (PN (FUNCALL (FS:DEFAULT-PATHNAME DEFAULTS)
				':NEW-SUGGESTED-NAME
				(BUFFER-NAME BUFFER))))
	       (IF TYPE (FUNCALL PN ':NEW-TYPE TYPE) PN)))
	 DEFAULTS))
  DEFAULTS)

))

; From file DIRED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN (DIRED-MODE PATHNAME-DEFAULTING-FUNCTION) (IGNORE BUFFER)
  (AND (EQ BUFFER *INTERVAL*)
       (DIRED-LINE-PATHNAME (BP-LINE (POINT)))))

))

; From file BDIRED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; BDIRED  "

(DEFUN (BDIRED-MODE PATHNAME-DEFAULTING-FUNCTION) (IGNORE BUFFER)
  (AND (EQ BUFFER *INTERVAL*)
       (DONT-OPTIMIZE (DIRED-LINE-PATHNAME (BP-LINE (POINT))))))

))

; From file MOUSE.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-CLICK) (BUTTONS X Y)
  X Y ;not used
  (COND ((AND (= BUTTONS #\MOUSE-1-1)
	      (NOT (SEND (SEND SELF ':ALIAS-FOR-SELECTED-WINDOWS)
			 ':SELF-OR-SUBSTITUTE-SELECTED-P))
	      (GET-HANDLER-FOR SELF ':SELECT));paper over a bug
	 (MOUSE-SELECT SELF)
	 T)
	(T
	 (OR (FUNCALL-SELF ':SEND-IF-HANDLES ':FORCE-KBD-INPUT
			   `(:MOUSE-BUTTON ,(MERGE-SHIFT-KEYS BUTTONS) ,SELF ,X ,Y))
	     (AND (= BUTTONS #\MOUSE-3-1)
		  (MOUSE-CALL-SYSTEM-MENU)
		  T)
	     (BEEP)))))				;click not handled

))

; From file QRAND.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN SIMPLE-MAKE-ARRAY (DIMENSIONS &OPTIONAL (TYPE ART-Q) AREA LEADER-LENGTH
			  (INITIAL-VALUE NIL INITIAL-VALUE-P)
			  &AUX DATA-LENGTH LONG-ARRAY-P ARRAY)
  (COND ((OR (LISTP DIMENSIONS) (NULL DIMENSIONS))
	 (IF INITIAL-VALUE-P
	     (DONT-OPTIMIZE
	       (MAKE-ARRAY DIMENSIONS ':TYPE TYPE ':AREA AREA
			   ':LEADER-LENGTH LEADER-LENGTH
			   ':INITIAL-VALUE INITIAL-VALUE))
	   (DONT-OPTIMIZE
	     (MAKE-ARRAY DIMENSIONS ':TYPE TYPE ':AREA AREA
			 ':LEADER-LENGTH LEADER-LENGTH))))
	(T
	 (CHECK-ARG DIMENSIONS FIXP "a fixnum or a list")
	 (IF (MINUSP DIMENSIONS)
	     (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	 (IF (> DIMENSIONS %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (SETQ LONG-ARRAY-P T))
	 (IF (NOT (NULL LEADER-LENGTH))
	     (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 0) "greater than zero"))
	 (COND ((FIXP TYPE)
		(IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
		    (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE))))
	       ((SYMBOLP TYPE)
		(ETYPECASE (SYMEVAL TYPE)
		  (FIXNUM
		   (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD (SYMEVAL TYPE))))
		  (SYMBOL
		   (SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS)))))
	       (T
		(CHECK-ARG TYPE (OR (FIXP TYPE) (MEMQ TYPE ARRAY-TYPES)) "an array type")
		(IF (FIXP TYPE)
		    (IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
			(SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
		  (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD (SYMEVAL TYPE))))))
	 (LET ((ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE)))
	   ;; This is positive if there are 1 or more entries per Q.  It is
	   ;; negative if there are more than one Qs per entry.
	   (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
				 (CEILING DIMENSIONS ENTRIES-PER-Q)
			       (* DIMENSIONS (MINUS ENTRIES-PER-Q))))
	   (LET ((HEADER-WORD
		   ;; Put in array type and number of dims.
		   (%LOGDPB 1 %%ARRAY-NUMBER-DIMENSIONS
			    (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
	     ;; If there is a leader, set the flag.
	     (IF LEADER-LENGTH
		 (SETQ HEADER-WORD (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-WORD)))
	     (SETQ HEADER-WORD
		   (COND (LONG-ARRAY-P
			  ;; It is local; if it is a long array, the length is not in the
			  ;; header at all; set the bit instead.
			  (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-WORD))
			 (T
			  ;; It is a short array; the length is in the header.
			  (+ DIMENSIONS HEADER-WORD))))
	     ;; Create the array.
	     (SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-WORD
							 DIMENSIONS
							 (OR LEADER-LENGTH 0)
							 AREA
							 (+ 1	;header
							    (IF LEADER-LENGTH
								(+ 2 LEADER-LENGTH)
							      0)
							    (COND (LONG-ARRAY-P
								   (1+ DATA-LENGTH))
								  (T DATA-LENGTH)))
							 )))
	   (COND (INITIAL-VALUE-P
		  (ARRAY-INITIALIZE ARRAY INITIAL-VALUE))))
	 (VALUES ARRAY DATA-LENGTH))))

))

; From file SECTIO.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN (:LISP GET-SECTION-NAME) (LINE BP &AUX STR SYM ERROR-P
				 IDX END-IDX (EOF "") NON-FONT-LINE)
  (IF (NOT (AND (PLUSP (LENGTH LINE)) (= (LDB %%CH-CHAR (AREF LINE 0)) #/()))
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
			     (SUBSTRING LINE 1 (STRING-SEARCH-SET *WHITESPACE-CHARS* LINE 1))
			     "-"
			     (PRIN1-TO-STRING (INCF *SECTION-COUNT*)))
	    STR SYM))
    (VALUES SYM STR NIL)))

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

;Main routine, to print any lisp object.
;The WHICH-OPERATIONS argument is provided as an efficiency hack.  It also used
;by streams that have a :PRINT handler, and recursively call PRINT-OBJECT, to
;prevent themselves from being called again (they pass NIL or (:STRING-OUT)).
(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM
		     &OPTIONAL
		     (WHICH-OPERATIONS (WHICH-OPERATIONS-FOR-PRINT STREAM))
		     &AUX NSS (FASTP (MEMQ ':STRING-OUT WHICH-OPERATIONS)))
  (CATCH-CONTINUATION-IF T 'PRINT-OBJECT
      #'(LAMBDA () (FORMAT STREAM "...error printing ")
		(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP :FASTP FASTP))
		(FORMAT STREAM "..."))
      NIL
    (CONDITION-RESUME '((ERROR) :ABORT-PRINTING T ("Give up trying to print this object.")
			CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
      (OR (AND (MEMQ ':PRINT WHICH-OPERATIONS)	;Allow stream to intercept print operation
	       (FUNCALL STREAM ':PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*))
	  (AND *PRINT-CIRCLE*
	       (%POINTERP EXP)
	       (OR (NOT (SYMBOLP EXP))
		   (NOT (SYMBOL-PACKAGE EXP)))
	       ;; This is a candidate for circular or shared structure printing.
	       ;; See what the hash table says about the object:
	       ;; NIL - occurs only once.
	       ;; T - occurs more than once, but no occurrences printed yet.
	       ;;  Allocate a label this time and print #label= as prefix.
	       ;; A number - that is the label.  Print only #label#.
	       (*CATCH 'LABEL-PRINTED
		 (SEND PRINT-HASH-TABLE ':MODIFY-HASH EXP
		       #'(LAMBDA (KEY VALUE KEY-FOUND-P STREAM)
			   KEY KEY-FOUND-P
			   (COND ((NULL VALUE) NIL)
				 ((EQ VALUE T)
				  (LET ((LABEL (INCF PRINT-LABEL-NUMBER))
					(BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM ':TYO #/=)
				    LABEL))
				 (T
				  (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM ':TYO #/#)
				    (*THROW 'LABEL-PRINTED T)))))
		       STREAM)
		 NIL))
	  (TYPECASE EXP
	    (:FIXNUM (PRINT-FIXNUM EXP STREAM))
	    (:SYMBOL
	     (PRINT-PNAME-STRING EXP STREAM FASTP))
	    (:LIST
	     (IF (AND PRINLEVEL (>= I-PRINDEPTH PRINLEVEL))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL READTABLE) STREAM FASTP)
	       (IF *PRINT-PRETTY*
		   (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		 (PRINT-LIST EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (:STRING
	     (IF ( (ARRAY-ACTIVE-LENGTH EXP) (ARRAY-LENGTH EXP))
		 (PRINT-QUOTED-STRING EXP STREAM FASTP)
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:INSTANCE
	      (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
	    (:ENTITY
	     (IF (MEMQ ':PRINT-SELF (FUNCALL EXP ':WHICH-OPERATIONS))
		 (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*) 
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:NAMED-STRUCTURE
	     (IGNORE-ERRORS
	       (SETQ NSS (NAMED-STRUCTURE-P EXP)))
	     (COND ((AND (SYMBOLP NSS)
			 (OR (GET NSS 'NAMED-STRUCTURE-INVOKE)
			     (GET NSS ':NAMED-STRUCTURE-INVOKE))
			 (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;NAMED STRUCTURE THAT DOESN'T PRINT ITSELF
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
;		    (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
;		      (PRINC NSS STREAM))
	    (:ARRAY
	     (IF *PRINT-ARRAY*
		 (IF (AND (= (ARRAY-RANK EXP) 1)
			  (EQ (ARRAY-TYPE EXP) 'ART-1B))
		     (PRINT-BIT-VECTOR EXP STREAM)
		   (IF *PRINT-PRETTY*
		       (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		     (IF (= (ARRAY-RANK EXP) 1)
			 (PRINT-VECTOR EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
		       (PRINT-ARRAY EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	       (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
		 (PRINT-RAW-STRING (GET-PNAME (ARRAY-TYPE EXP)) STREAM FASTP)
		 (DO ((I 0 (1+ I))
		      (RANK (ARRAY-RANK EXP))
		      (DIM))
		     ((= I RANK))
		   (SETQ DIM (ARRAY-DIMENSION EXP I))
		   (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
		   (PRINT-FIXNUM DIM STREAM)))))
	    (:SMALL-FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP T))
	    (:FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP NIL))
	    (:BIGNUM
	     (PRINT-BIGNUM EXP STREAM FASTP))
	    (:RATIONAL
	     (PRINT-RATIONAL EXP STREAM FASTP))
	    (:COMPLEX (PRINT-COMPLEX EXP STREAM FASTP))
	    (:CHARACTER
	      (IF (NOT *PRINT-ESCAPE*)
		  (SEND STREAM ':TYO (LDB %%CH-CHAR EXP))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-BEFORE-FONT READTABLE))
		(IF (LDB-TEST %%CH-FONT EXP)
		    (LET ((BASE 10.) (*NOPOINT T))
		      (PRIN1 (LDB %%CH-FONT EXP) STREAM)))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-PREFIX READTABLE))
		(IF (CHAR-BIT EXP ':CONTROL)
		    (SEND STREAM ':STRING-OUT "c-"))
		(IF (CHAR-BIT EXP ':META)
		    (SEND STREAM ':STRING-OUT "m-"))
		(IF (CHAR-BIT EXP ':SUPER)
		    (SEND STREAM ':STRING-OUT "s-"))
		(IF (CHAR-BIT EXP ':HYPER)
		    (SEND STREAM ':STRING-OUT "h-"))
		(SEND STREAM ':TYO (LDB %%CH-CHAR EXP))))
	    (:NUMBER
	     (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
	     (PRINT-RAW-STRING (GET-PNAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
	     (LET ((BASE 8))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

(DEFUN PRINT-RECORD-OCCURRENCES (OBJECT)
  (WHEN (AND (%POINTERP OBJECT)
	     (OR (NOT (SYMBOLP OBJECT))
		 (NOT (SYMBOL-PACKAGE OBJECT)))
	     (NOT (SEND PRINT-HASH-TABLE ':MODIFY-HASH OBJECT
			#'(LAMBDA (OBJECT VALUE FOUND-P)
			    OBJECT VALUE
			    FOUND-P))))
    (TYPECASE OBJECT
      (LIST
	(DO ((TAIL OBJECT (CDR TAIL))
	     (FIRST T NIL))
	    ((ATOM TAIL)
	     (WHEN TAIL (PRINT-RECORD-OCCURRENCES TAIL)))
	  (UNLESS FIRST
	    (IF (SEND PRINT-HASH-TABLE ':MODIFY-HASH TAIL
		      #'(LAMBDA (OBJECT VALUE FOUND-P)
			  OBJECT VALUE
			  FOUND-P))
		(RETURN)))
	  (PRINT-RECORD-OCCURRENCES (CAR TAIL))))
      (ARRAY
       (LET (TEM)
	 (UNLESS (IF (SETQ TEM (NAMED-STRUCTURE-P OBJECT))
		     (AND (SETQ TEM (OR (GET TEM 'NAMED-STRUCTURE-INVOKE)
					(GET TEM ':NAMED-STRUCTURE-INVOKE)))
			  (MEMQ ':PRINT-SELF (FUNCALL TEM ':WHICH-OPERATIONS OBJECT)))
		   (NULL *PRINT-ARRAY*))
	   (DOTIMES (I (ARRAY-LENGTH OBJECT))
	     (PRINT-RECORD-OCCURRENCES (AR-1-FORCE OBJECT I)))))))))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN OPEN-CHAOS (HOST PATHNAME &REST OPTIONS &KEY (DIRECTION ':INPUT) (CHARACTERS T)
		   (ERROR T) (ACCESS-ERROR (NOT ERROR))
		   (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
		   (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
					;; :UNSPECIFIC here is to prevent lossage
					;; writing ITS files with no version numbers.
					'(:NEWEST :UNSPECIFIC))
				  ':NEW-VERSION ':ERROR))
		   (IF-DOES-NOT-EXIST
		     (COND ((MEMQ DIRECTION '(:PROBE :PROBE-LINK :PROBE-DIRECTORY))
			    NIL)
			   ((AND (EQ DIRECTION ':OUTPUT)
				 (NOT (MEMQ IF-EXISTS '(:OVERWRITE :TRUNCATE :APPEND))))
			    ':CREATE)
			   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			   ;; for compatibility with the past.
			   ;; A Common-Lisp program would use :PROBE
			   ;; and get NIL as the default for this.
			   (T ':ERROR)))
		   TEMPORARY DELETED RAW SUPER-IMAGE (BYTE-SIZE ':DEFAULT)
		   PRESERVE-DATES INHIBIT-LINKS
		   &AUX HOST-UNIT DATA-CONN PKT SUCCESS STRING NOT-ABORTED
		   PHONY-CHARACTERS SIGN-EXTEND-BYTES IF-EXISTS-P
		   (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SI:CCASE DIRECTION
    ((:INPUT :OUTPUT :PROBE-DIRECTORY :PROBE-LINK))
    (:IO (FERROR NIL "Bidirectional file streams are not supported."))
    ((NIL :PROBE) (SETQ DIRECTION NIL)))
  (CHECK-TYPE IF-EXISTS (MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
				:OVERWRITE :APPEND :TRUNCATE :SUPERSEDE NIL))
  (CHECK-TYPE IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
  ;; IF-EXISTS-P is T if we need to give the IF-EXISTS to the server.
  (SETQ IF-EXISTS-P
	(NEQ IF-EXISTS
	     (IF (MEMQ (PATHNAME-VERSION PATHNAME)
		       '(:NEWEST :UNSPECIFIC))
		 ':NEW-VERSION ':SUPERSEDE)))
  (WHEN ELEMENT-TYPE-P
    (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
	  (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
  (FILE-OPERATION-RETRY
    (CONDITION-CASE-IF ACCESS-ERROR (ERROR-OBJECT)
        (PROGN
	  (IF (MEMQ DIRECTION '(NIL :PROBE-DIRECTORY :PROBE-LINK))
	      ;;PROBE mode implies no need for data connection
	      (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT))
	    (MULTIPLE-VALUE (DATA-CONN HOST-UNIT)
	      (FUNCALL HOST ':GET-DATA-CONNECTION DIRECTION))))
      (REMOTE-NETWORK-ERROR ERROR-OBJECT)
      (:NO-ERROR
       (UNWIND-PROTECT
	 (PROGN
	   (MULTIPLE-VALUE (PKT SUCCESS STRING)
	     (IF (TYPEP SELF '(OR LMFILE-PARSING-MIXIN LM-PARSING-MIXIN))
		 (FUNCALL HOST-UNIT ':COMMAND NIL
			  (SELECTQ DIRECTION
			    (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			    (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			  NIL "OPEN-FOR-LISPM " #\RETURN
			  (FILE-PRINT-PATHNAME SELF) #\RETURN
			  (LET ((BASE 10.) (*NOPOINT T) (PACKAGE SI:PKG-USER-PACKAGE)
				(READTABLE SI:INITIAL-READTABLE))
			    (AND (EQ DIRECTION ':OUTPUT) (NULL IF-EXISTS)
				 (SETQ OPTIONS (LIST* ':IF-EXISTS ':ERROR OPTIONS)))
			    (AND (NOT IF-EXISTS-P)
				 (GET-LOCATION-OR-NIL (LOCF OPTIONS) ':IF-EXISTS)
				 (PROGN
				   (SETQ OPTIONS (COPYLIST OPTIONS))
				   (REMPROP (LOCF OPTIONS) ':IF-EXISTS)))
			    (AND (NULL IF-DOES-NOT-EXIST)
				 (SETQ OPTIONS (LIST* ':IF-DOES-NOT-EXIST ':ERROR OPTIONS)))
			    (PRIN1-TO-STRING OPTIONS)))
	       (FUNCALL HOST-UNIT ':COMMAND NIL
			(SELECTQ DIRECTION
			  (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			  (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			NIL
			"OPEN " (SELECTQ DIRECTION
				  ((NIL) "PROBE")
				  (:PROBE-DIRECTORY "PROBE-DIRECTORY")
				  (:PROBE-LINK "PROBE INHIBIT-LINKS")
				  (:INPUT "READ")
				  (:OUTPUT "WRITE"))
			" " (SELECTQ CHARACTERS
			      ((NIL) "BINARY")
			      (:DEFAULT "DEFAULT")
			      (T "CHARACTER"))
			(IF (AND (EQ DIRECTION ':OUTPUT)
				 IF-EXISTS-P)
			    (STRING-APPEND " IF-EXISTS "
					   (IF (EQ IF-EXISTS NIL)
					       ':ERROR
					     IF-EXISTS))
			  "")
			(IF (OR IF-EXISTS-P
				(NEQ IF-DOES-NOT-EXIST
				     (SELECTQ DIRECTION
				       ((:INPUT NIL :PROBE-DIRECTORY :PROBE-LINK) ':ERROR)
				       (:OUTPUT ':CREATE))))
			    (STRING-APPEND " IF-DOES-NOT-EXIST "
					   (IF (EQ IF-DOES-NOT-EXIST NIL)
					       ':ERROR
					     IF-DOES-NOT-EXIST))
			  "")
			(IF INHIBIT-LINKS " INHIBIT-LINKS" "")
			(FORMAT NIL "~:[ BYTE-SIZE ~D~;~*~]~:[~; TEMPORARY~]~:[~; DELETED~]~
				~:[~; RAW~]~:[~; SUPER~]~:[~; PRESERVE-DATES~]~%~A~%"
				(EQ BYTE-SIZE ':DEFAULT) BYTE-SIZE
				TEMPORARY DELETED RAW SUPER-IMAGE PRESERVE-DATES
				(FILE-PRINT-PATHNAME SELF)))))
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
					    'FILE-OUTPUT-BINARY-STREAM)))
				       (T 'FILE-PROBE-STREAM))
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
