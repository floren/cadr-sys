;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.20
;;; Reason: Define MACROEXPAND-ALL.
;;; Fix DEFSUBSTs containing conditionals.
;;; Bug in compilation of WITH-STACK-LIST as the test in a COND.
;;; Clean up handling of environment between compiler and codewalker.
;;; Written 1/01/84 01:52:20 by RMS,
;;; while running on Lisp Machine Nine from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.17, CADR 3.4, ZMail 53.5, MIT-Specific 22.0, microcode 306, ZM MIT.


(globalize "MACROEXPAND-ALL")

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN BREAKOFF (X &OPTIONAL LEXICAL &AUX FNAME FNAME-TO-GIVE)
  (MULTIPLE-VALUE-BIND (VARS-NEEDED-LEXICALLY FUNCTIONS-NEEDED-LEXICALLY
			BLOCK-NAMES GO-TAGS)
      (CW-TOP-LEVEL-LAMBDA-EXPRESSION
	X
	(LOOP FOR HOME IN VARS
	      WHEN (AND (EQ (VAR-TYPE HOME) 'FEF-LOCAL)
			(EQ HOME (ASSQ (VAR-NAME HOME) VARS)))  ;Omit shadowed bindings.
	      COLLECT (VAR-NAME HOME))
	(MAPCAR 'CAR LOCAL-FUNCTIONS)
	LOCAL-MACROS)
    (DOLIST (V VARS-NEEDED-LEXICALLY)
      (LET ((TEM (ASSQ V VARS)))
	(WHEN TEM
	  (SETQ LEXICAL T)
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

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN (FLET-INTERNAL P1) (FORM)
  (LET ((LOCAL-FUNCTIONS
	  (NCONC (MAPCAR #'(LAMBDA (ELT)
			     ;; ELT looks like
			     ;; (local-function-name tempvar-name definition)
			     (LIST (CAR ELT)
				   (ASSQ (CADR ELT) VARS)
				   (CADDR ELT)))
			 (CADR FORM))
		 LOCAL-FUNCTIONS))
	(LOCAL-MACROS
	  ;; Defining a local function hides any local macro definition of same symbol.
	  (CONS (LOOP FOR ELT IN (CADR FORM)
		      NCONC (LIST* (LOCF (FSYMEVAL (CAR ELT))) NIL NIL))
		LOCAL-MACROS)))
    (CONS 'PROGN (P1PROGN-1 (CDDR FORM)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN (LABELS-INTERNAL P1) (FORM)
  (LET ((LOCAL-FUNCTIONS
	  (NCONC (MAPCAR #'(LAMBDA (ELT)
			     (LIST (CAR ELT)
				   (ASSQ (CADR ELT) VARS)
				   (CADDR ELT)))
			 (CADR FORM))
		 LOCAL-FUNCTIONS))
	(LOCAL-MACROS
	  ;; Defining a local function hides any local macro definition of same symbol.
	  (CONS (LOOP FOR ELT IN (CADR FORM)
		      NCONC (LIST* (LOCF (FSYMEVAL (CAR ELT))) NIL NIL))
		LOCAL-MACROS)))
    (LIST* 'PROGN
	   (P1 `(PSETQ . ,(CADDR FORM)))
	   (P1PROGN-1 (CDDDR FORM)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN (MACROLET P1) (EXP)
  (LET ((LOCAL-MACROS
	  (CONS (AND (LISTP (CADR EXP))
		     (MAPCAN #'(LAMBDA (ELT)
				 (LIST (FUNCTION-CELL-LOCATION (CAR ELT))
				       (CONS 'MACRO (SI:EXPAND-DEFMACRO ELT))))
			     (CADR EXP)))
		LOCAL-MACROS))
	;; If we define it as a local macro, that hides any local function definition.
	(LOCAL-FUNCTIONS
	  (REM-IF #'(LAMBDA (ELT) (ASSQ (CAR ELT) (CADR EXP)))
		  LOCAL-FUNCTIONS)))
    (CONS 'PROGN (P1PROGN-1 (CDDR EXP)))))

;Turn an internal lambda containing &AUX variables
;into one containing a LET* and having no &AUX variables.

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "
(defun sublis-eval-once (alist exp &optional reuse-flag sequential-flag
			 (environment *macroexpand-environment*))
  "Effectively substitute for symbols in EXP according to ALIST, preserving execution order.
Each element of ALIST describes one symbol (the car)
and what it stands for (the cdr).
We replace each symbol with the corresponding value expression,
not with straight textual substitution but so that
the value expression will be evaluated only once.

If SEQUENTIAL-FLAG is non-NIL, the value substituted for each symbol
may refer to the previous symbols substituted for.

This may require the use of temporary variables.
The first use of a symbol would be replaced by a SETQ of the tempvar
to the symbol's corresponding expression.  Later uses would be
replaced with just the tempvar.  A LET to bind the tempvars is
wrapped around the whole expression.

If REUSE-FLAG is non-NIL, the symbols themselves can be used
as their own tempvars when necessary.  Otherwise tempvars are gensymmed.

It may be necessary to expand macros in EXP in order to process it.
In this case, ENVIRONMENT is passed as the environment arg to MACROEXPAND.
It defaults to the value of *MACROEXPAND-ENVIRONMENT*, which within a macro's
expander function is bound to the environment of expansion."
  (let (constant-alist nonconstant-alist
	value-so-far)
    ;; First, divide replacements up into constant values vs nonconstants.
    ;; Order of evaluation never matters for the constants so we will
    ;; put them in with SUBLIS.
    (dolist (elt alist)
      (let ((tem (if sequential-flag
		     (sublis constant-alist (cdr elt))
		   (cdr elt))))
	(if (constantp tem)
	    (push (if (eq tem (cdr elt)) elt (cons (car elt) tem))
		  constant-alist)
	  (push (list (car elt) 0 tem nil nil)
		nonconstant-alist))))
    ;; The nonconstants must remain in the proper order!
    (setq nonconstant-alist (nreverse nonconstant-alist))
    ;; If the only things not constant are variables,
    ;; then they are ok.
    (when (loop for elt in nonconstant-alist
		always (symbolp (seo-exp elt)))
      (dolist (elt nonconstant-alist)
	(push (cons (car elt)
		    (if sequential-flag (sublis constant-alist (seo-exp elt)) (seo-exp elt)))
	      constant-alist))
      (setq nonconstant-alist nil))
    (setq value-so-far (sublis constant-alist exp))
    (when nonconstant-alist
      ;; If the expression to be substituted in
      ;; contains any kind of branching,
      ;; we must calculate all the variables at the beginning
      ;; to avoid having the calculation be skipped by a branch.
      ;; Hairier analysis might detect certain cases
      ;; such as a variable being used before the branch, or only after a join,
      ;; but that is probably not worth thinking about.
      (multiple-value-bind (nil functions-used)
	  (compiler:cw-top-level exp nil '(cond and or return return-from go
						*catch *throw cli:catch cli:throw
						do do* do-named do*-named)
				 (cadr environment))
	(if functions-used
	    (setq value-so-far `(progn ,@(mapcar 'car nonconstant-alist) ,value-so-far))))
      ;; Each nonconstant value should be inserted only once, and in correct order.
      ;; SEO-FIRST-UNINSERTED-VAR points to the first one we have not yet inserted.
      ;; All the ones before that have had temporary variables (gensyms) made.
      (let* ((seo-first-uninserted-var nonconstant-alist))
	(setq value-so-far (sublis-eval-once-1 value-so-far nonconstant-alist
					       reuse-flag sequential-flag))
	;; Now stick on evaluations of any values that weren't really used.
	(if seo-first-uninserted-var
	    (setq value-so-far
		  `(multiple-value-prog1
		     ,value-so-far
		     . ,(if sequential-flag
			    (list (sublis-eval-once-1 (caar (last nonconstant-alist))
						      nonconstant-alist
						      reuse-flag t))
			  (mapcar 'seo-exp seo-first-uninserted-var))))))
      ;; If a temp var is not used again after it is set,
      ;; flush the temp var from the code -- just use its value straight.
      (dolist (elt nonconstant-alist)
	(let ((tem (seo-first-use elt)))
	  (when (zerop (seo-count elt))
	    (do ((tail (cdr tem) (cdr tail)))
		((null tail))
	      (when (and (listp (car tail))
			 (eq (caar tail) 'setq)
			 (eq (cadar tail) (seo-tempvar elt)))
		(setf (car tail) (caddar tail))
		(return)))))))
    ;; Now see which temp vars still remain in use,
    ;; and put on a binding for them.
    (let ((tempvars-used
	    (loop for elt in nonconstant-alist
		  when (not (zerop (seo-count elt)))
		  collect (list (seo-tempvar elt) '(compiler:undefined-value)))))
      (if tempvars-used
	  `(let ,tempvars-used ,value-so-far)
	value-so-far))))

))

(load "sys:sys;qcluke")

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-MACRO-EXPAND-EXPRESSION-ALL "Print macroexpansion of next s-expression to all levels.
The result is printed on the screen with GRIND-TOP-LEVEL." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ STREAM '*EOF*)))
      (AND (EQ FORM '*EOF*) (BARF))
      (GRIND-TOP-LEVEL (COMPILER:MACROEXPAND-ALL FORM))))
  DIS-NONE)

))

; From file QCDEFS.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFVAR LOCAL-MACROS :UNBOUND
  "List of frames describing local macro definitions and shadowed functions.
The format of this object is the same as that of SI:INTERPRETER-FUNCTION-ENVIRONMENT,
except that only local macros really have definitions recorded.
Local functions that are not macros have NIL recorded as their definitions.
Such local functions are present only to record that they shadow
more global definitions of the same function names.
This turns out to be just what you want to pass to CW-TOP-LEVEL and such.")

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "
(DEFUN MACROEXPAND-1 (MACRO-CALL &OPTIONAL ENVIRONMENT
		      &AUX (LOCAL-MACROS (CADR ENVIRONMENT)))
  "Expand MACRO-CALL once and return the result.
Macro calls, uses of SUBSTs, uses of CURRY-BEFORE and CURRY-AFTER,
and uses of functions for which OPEN-CODE-P is true, are all expanded.
The second value is T if there was something to expand.
If SYS:RECORD-MACROS-EXPANDED is non-NIL,
all macro names are pushed on SYS:MACROS-EXPANDED.
The value of *MACROEXPAND-HOOK* (which should behave like FUNCALL)
is used to invoke the expander function."
  (DECLARE (VALUES EXPANSION EXPANDED-FLAG))
  (LET (TM)
    (COND ((ATOM MACRO-CALL) MACRO-CALL)
	  ((NOT (ATOM (CAR MACRO-CALL)))
	   (COND ((EQ (CAAR MACRO-CALL) 'CURRY-AFTER)
		  (VALUES `(,(CADAR MACRO-CALL) ,@(CDR MACRO-CALL) . ,(CDDAR MACRO-CALL))
			  T))
		 ((EQ (CAAR MACRO-CALL) 'CURRY-BEFORE)
		  (VALUES `(,(CADAR MACRO-CALL) ,@(CDDAR MACRO-CALL) . ,(CDR MACRO-CALL))
			  T))
		 ((OR (EQ (CAAR MACRO-CALL) 'SUBST) (EQ (CAAR MACRO-CALL) 'NAMED-SUBST))
		  (VALUES (FUNCALL *MACROEXPAND-HOOK* 'SUBST-EXPAND-1 MACRO-CALL)
			  T))
		 (T MACRO-CALL)))
	  ((NOT (SYMBOLP (CAR MACRO-CALL)))
	   MACRO-CALL)
	  ((DO ((TAIL LOCAL-MACROS (CDR TAIL)))
	       ((ATOM TAIL))
	     (LET ((FRAME (CAR TAIL)))
	       (SETQ TM
		     (GET-LOCATION-OR-NIL (LOCF FRAME) (LOCF (FSYMEVAL (CAR MACRO-CALL)))))
	       (WHEN TM (RETURN TM))))
	   (SETQ TM (CONTENTS TM))
	   (IF (AND (CONSP TM) (EQ (CAR TM) 'MACRO))
	       (LET ((*MACROEXPAND-ENVIRONMENT* ENVIRONMENT)
		     (AINF (ARGS-INFO (CDR TM))))
		 (IF (> (LDB %%ARG-DESC-MAX-ARGS AINF) 1)
		     (VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL ENVIRONMENT) T)
		   (VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL) T)))
	     MACRO-CALL))
	  ((SETQ TM (DECLARED-DEFINITION (CAR MACRO-CALL)))
	   (COND ((TYPEP TM ':COMPILED-FUNCTION)
		  ;; If function is compiled,
		  ;; see if its interpreted defn is recorded.
		  (SETQ TM (ASSQ 'INTERPRETED-DEFINITION (DEBUGGING-INFO TM)))
		  (IF (AND TM (MEMQ (CAADR TM) '(SUBST NAMED-SUBST)))
		      (PROGN
			(AND RECORD-MACROS-EXPANDED
			     (NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
			     (PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
			(VALUES (FUNCALL *MACROEXPAND-HOOK* 'SUBST-EXPAND-1 MACRO-CALL)
				T))
		    MACRO-CALL))
		 ((ATOM TM) MACRO-CALL)
		 ((EQ (CAR TM) 'MACRO)
		  (AND RECORD-MACROS-EXPANDED
		       (NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
		       (PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
		  (LET ((*MACROEXPAND-ENVIRONMENT* ENVIRONMENT)
			(AINF (ARGS-INFO (CDR TM))))
		    (IF (> (LDB %%ARG-DESC-MAX-ARGS AINF) 1)
			(VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL ENVIRONMENT) T)
		      (VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL) T))))
		 ((OR (EQ (CAR TM) 'SUBST)
		      (EQ (CAR TM) 'NAMED-SUBST))
		  (AND RECORD-MACROS-EXPANDED
		       (NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
		       (PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
		  (VALUES (FUNCALL *MACROEXPAND-HOOK* 'SUBST-EXPAND-1 MACRO-CALL) T))
		 (T MACRO-CALL)))
	  ((SETQ TM (OPEN-CODE-P (CAR MACRO-CALL)))
	   (AND RECORD-MACROS-EXPANDED
		(NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
		(PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
	   (VALUES (CONS TM (CDR MACRO-CALL)) T))
	  (T MACRO-CALL))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFPROP BLOCK-FOR-WITH-STACK-LIST P1BLOCK P1)

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1-WITH-STACK-LIST (FORM &AUX MAKER)
  (SETQ MAKER (IF (MEMQ (CAR FORM) '(WITH-STACK-LIST* :WITH-STACK-LIST*))
		  '%MAKE-EXPLICIT-STACK-LIST*
		'%MAKE-EXPLICIT-STACK-LIST))
  (P1 `(BLOCK-FOR-WITH-STACK-LIST P1-WITH-STACK-LIST
	 (CHANGE-PDLLVL ,(LENGTH (CDADR FORM))
			(%PUSH (,MAKER . ,(CDADR FORM))))
	 (LET ((,(CAADR FORM) (%POP)))
	   . ,(CDDR FORM)))))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN P2BLOCK (ARGL DEST &OPTIONAL BIND-RETPROGDESC D-INDS-LOSES)
  (LET* ((OLDGOTAGS GOTAGS)
	 (GOTAGS (CAR ARGL)) (MYPROGDESC (CADR ARGL)) (BDY (CDDR ARGL))
	 (PROGNAME (PROGDESC-NAME MYPROGDESC))
	 (RETTAG (PROGDESC-RETTAG MYPROGDESC))
	 (PROGDESCS (CONS MYPROGDESC PROGDESCS))
	 (RETPROGDESC (IF (OR (NULL PROGNAME) (AND BIND-RETPROGDESC (NEQ PROGNAME T)))
			  MYPROGDESC RETPROGDESC)))
    (PROG (IDEST NVALUES)
	  ;; Determine the immediate destination of returns in this prog.
	  (SETQ IDEST 'D-PDL)
	  (AND (MEMQ DEST '(D-IGNORE D-INDS D-RETURN))
	       (NOT (AND (EQ DEST 'D-INDS) D-INDS-LOSES))
	       (NULL M-V-TARGET)
	       (SETQ IDEST DEST))
	  ;; How many words are we supposed to leave on the stack?
	  (SETQ NVALUES
		(COND ((NUMBERP M-V-TARGET) M-V-TARGET)
		      ((EQ IDEST 'D-PDL) 1)
		      (T 0)))
	  (SETF (PROGDESC-IDEST MYPROGDESC) IDEST)
	  (SETF (PROGDESC-M-V-TARGET MYPROGDESC) M-V-TARGET)
	  (SETF (PROGDESC-PDL-LEVEL MYPROGDESC) PDLLVL)
	  (SETF (PROGDESC-NBINDS MYPROGDESC) 0)
	  ;; Set the GOTAG-PDL-LEVEL of each the rettag.
	  ;; GOTAGS contains the RETTAG and nothing else.
	  (SETF (GOTAG-PROGDESC (CAR GOTAGS)) (CAR PROGDESCS))
	  (SETF (GOTAG-PDL-LEVEL (CAR GOTAGS)) (+ PDLLVL NVALUES))
	  (SETQ GOTAGS (APPEND GOTAGS OLDGOTAGS))
	  ;; Generate code for the body.
	  (IF (NULL BDY)
	      (P2RETURN1 '('NIL) PROGNAME)
	    (DO ((TAIL BDY (CDR TAIL)))
		((NULL (CDR TAIL))
		 (P2RETURN1 (LIST (CAR TAIL)) PROGNAME))
	      (P2 (CAR TAIL) 'D-IGNORE)))
	  ;; If this is a top-level BLOCK, we just went to D-RETURN,
	  ;; and nobody will use the RETTAG, so we are done.
	  (AND (EQ DEST 'D-RETURN)
	       (RETURN NIL))
	  ;; Otherwise, this is where RETURNs jump to.
	  (MKPDLLVL (GOTAG-PDL-LEVEL (CAR GOTAGS)))
	  (OUTTAG RETTAG)
	  ;; Store away the value if
	  ;; it is not supposed to be left on the stack.
	  (AND (NEQ DEST IDEST)
	       (NULL M-V-TARGET)
	       (MOVE-RESULT-FROM-PDL DEST))
	  ;; If we were supposed to produce multiple values, we did.
	  (SETQ M-V-TARGET NIL))))

;; This differs from block only when DEST is D-INDS.
;; In that case, this one compiles the value to the PDL,
;; then moves it to D-INDS after popping off any excess pdl words
;; underneath it.  BLOCK would compile the value direct to D-INDS,
;; which loses if words must be popped off the stack on falling thru.
;; However, that is something that cannot happen for user BLOCKs.
;; It can happen only for the weird BLOCK body that WITH-STACK-LIST generates.
(DEFUN (BLOCK-FOR-WITH-STACK-LIST P2) (ARGL DEST)
  (P2BLOCK ARGL DEST NIL T))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun evalhook (form *evalhook* *applyhook*
		 &optional environment
		 &aux (interpreter-environment (car environment))
		 (interpreter-function-environment (cadr environment)))
  "Evaluate FORM, using specified *EVALHOOK* and *APPLYHOOK* except at the top level.
ENVIRONMENT is the lexical environment to eval in.  (Its car is used for
   SI:INTERPRETER-ENVIRONMENT and its cadr for SI:INTERPRETER-FUNCTION-ENVIRONMENT.)
 NIL means global Common Lisp environment, (NIL T) means traditional evaluation.
 Or use the environment argument passed to an evalhook function."
  (eval1 form t))

(defun applyhook (function args *evalhook* *applyhook*
		  &optional environment
		  &aux (interpreter-environment (car environment))
		  (interpreter-function-environment (cadr environment)))
  "Apply FUNCTION to ARGS, using specified *EVALHOOK* and *APPLYHOOK* except at the top level.
ENVIRONMENT is the lexical environment to eval in.  (Its car is used for
   SI:INTERPRETER-ENVIRONMENT and its cadr for SI:INTERPRETER-FUNCTION-ENVIRONMENT.)
 NIL means global Common Lisp environment, (NIL T) means traditional evaluation.
 Or use the environment argument passed to an evalhook function."
  (apply function args))

))
