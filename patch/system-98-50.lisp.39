;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.50
;;; Reason: LOGIN-FORMS:
;;;   ZWEI:DEFCOM, PKG-GOTO(-GLOBALLY), ADD-INITIALIATION, PROGN
;;; Some #n= #n# reader fixes
;;; FS:EN/DISABLE-CAPABILITIES hacks protocol more accurately -- vms now understands us.
;;; (ARGLIST FOO 'COMPILE)
;;; #s reader improvements
;;; DEFSTRUCT constructors take doc strings.
;;; DEFSTRUCT :PRINT and :PRINT-FUNCTION use DEFSELECT
;;; :FASD-FIXUP and :SXHASH check :WHICH-OPERATIONS first
;;;   and only are invoked if handled
;;; Terminal 1 v views any file
;;; %POINTER-PLUS
;;; ADJUSTABLE-ARRAY-P defined. Amazingly useful on a lispmachine.
;;; READLINE returns right first value on eof (ie eof option)
;;; UT timezone
;;; common lisp alternate macro definitions.
;;;   Who would believe that COND could be a macro?
;;; Disassemble *PRINT-RADIX*
;;; type-checking macros do not copy type names so gratuitously
;;; m-X Add (Buffer) Changed Sections lets you <q>uit
;;; &KEY args don't barf on :ALLOW-OTHER-KEYS NIL
;;; (TYPEP FOO 'CLI:CHARACTER) Foo.
;;; SI:XR-XRTYI strips fonts
;;; DEFSIGNAL(-EXPLICIT) indenation
;;; Bug in :SELECT-METHOD FUNCTION-SPEC-HANDLER
;;; MAKE-PACKAGE, DEFPACKAGE no longer take cretinous dotted pair :RELATIVE-NAMES args
;;; (IMPORT 'NIL FOO) works
;;; CAAR-SAFE, CDAR-SAFE
;;; Zmacs puts buffer-modified-p and more-above/below flags at start
;;;   prevent run-off-edgery
;;; Zmacs mouse clicks don't lose on mistimed shift-key merging
;;; compiler check-number-of-args error
;;; (PROMPT-AND-READ '(:CHARACTER :OR-NIL T) "Foo") lets you read a #/clear-input
;;;   by quoting with #/c-q or #/quote
;;; CONDITION-TYPEP externsions: hacks AND, OR, NOT, and combinations thereof
;;; Typo affecting :default-handler option to defflavor
;;; Locatives in TV:CHOOSE-VARIABLE-VALUES

;;; Written 28-Apr-84 07:24:56 by Mly,
;;; while running on Lisp Machine Thirty-one from band 3
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.48, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309, ZM MIT.



(eval-when (load compile eval)
  (dolist (si:foo '("ADJUSTABLE-ARRAY-P" "SIMPLE-ARRAY" "SCHAR" "WRITE-TO-STRING"
		    "ARRAY-TOTAL-SIZE-LIMIT" "%POINTER-PLUS" "INSTANCEP"
		    "CAAR-SAFE" "CDAR-SAFE" "REAL"))
    (let ((si:bar (intern si:foo si:pkg-system-internals-package)))
      (globalize si:bar si:pkg-global-package)))
  (globalize (intern "PRINTING-RANDOM-OBJECT" "SI") "SYSTEM"))

; From file LOGIN.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(DEFUN UNDOABLE-FORMS-1 (UNDO-LIST-NAME FORMS &OPTIONAL (COMPLAINT-STRING ""))
  (DOLIST (FORM FORMS)
    (IF (EQ (CAR FORM) 'PROGN)
	(UNDOABLE-FORMS-1 UNDO-LIST-NAME (CDR FORM) COMPLAINT-STRING)
      (LET ((U (UNDOABLE-EVAL FORM)))
	(IF (EQ U T)
	    (FORMAT *ERROR-OUTPUT*
		    "~&[A ~S form is supposed to be undone ~A, but this is not implemented.
The form's effects will be permanent.]~%"
		    (CAR FORM)
		    COMPLAINT-STRING)
	  (WHEN U (PUSH U (SYMEVAL UNDO-LIST-NAME))))))))

))

; From file LOGIN.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(defun (quote :undo-function) (ignore))

))

; From file LOGIN.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

zwei:
(defun (command-define :undo-function) (form)
  (let* ((command (cadr (cadr form)))		;(car (cadr form)) is QUOTE
	 (name (make-command-name command))
	 (aentry (ass 'equalp name *command-alist*))
	 )
    `(progn (setf (get ',command 'documentation) ',(get command 'documentation))
	    (setf (get ',command 'documentation-function) ',(get command 'documentation-function))
	    (setf (get ',command 'command-name) ',(get command 'command-name))
	    ,(if aentry
		 `(let ((aentry (ass 'equalp ,name *command-alist*)))
		    (if aentry
			(setf (cdr aentry) ',(cdr aentry))
		      (push (cons ,name ,(cdr aentry)) *command-list*)))
	       `(setq *command-alist* (del-if #'(lambda (foo) (equalp (car foo) ',name))
					      zwei:*command-alist*))))))

))

; From file LOGIN.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(defun (pkg-goto :undo-function) (form)
  `(progn
     ,(when (caddr form)			;globally flag
	(if (boundp-globally '*package*)
	    `(setq-globally *package* ',(symeval-globally '*package*))
	  `(setq-globally *package* pkg-user-package)))
     ,(if (boundp '*package*)
	  `(setq *package* ',*package*)
	`(setq *package* pkg-user-package))))

))

; From file LOGIN.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(defun (pkg-goto-globally :undo-function) (ignore)
  (if (boundp-globally '*package*)
      `(setq-globally *package* ',(symeval-globally '*package*))
    `(setq-globally *package* pkg-user-package)))

))

; From file LOGIN.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(defun (add-initialization :undo-function) (form)
  (let ((name (eval (cadr form)))
	(keywords (eval (cadddr form)))
	(list-name (eval (car (cddddr form))))
	tem)
    (dolist (l (if (cli:listp keywords) keywords (list keywords)))
      (if (setq tem (ass 'string-equal (symbol-name l) initialization-keywords))
	  (setq list-name (cadr tem))))
    (dolist (l (symeval list-name)
	       `(delete-initialization ',name nil ',list-name))
      (when (string-equal (init-name l) name)
	(return `(let ((foo (cli:assoc ',name ,list-name ':test 'string-equal))
		       (bar (make-init-list-entry  ',name ',(init-form l) ',(init-flag l)
						   ',(init-source-file l))))
		   (if foo
		       (setf (cdr foo) (cdr bar))
		     (setq ,list-name (nconc ,list-name (list bar))))))))))

))

; From file GC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFVAR AFTER-FLIP-INITIALIZATION-LIST NIL "Initializations performed after every flip.")

))

; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#=-MACRO (STREAM IGNORE &OPTIONAL (LABEL XR-SHARP-ARGUMENT) &AUX THING)
  (COND
    (*READ-SUPPRESS*
     (VALUES))
    ((NOT LABEL)
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "No argument (label number) to #= given."))
    ((ASSQ LABEL XR-LABEL-BINDINGS)
     ; The label is already defined, but we can't tell what it is yet.
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
	     "Label ~S already defined in this expression." LABEL))
    (T ; Go for it and BIND
     (PUSH (CONS LABEL (CAR-LOCATION (NCONS (GENSYM)))) ; Allocate a slot
	   XR-LABEL-BINDINGS)
     (LET ((LABEL-BINDING (ASSQ LABEL XR-LABEL-BINDINGS)))
       (IF (NULL LABEL-BINDING)
	   (FERROR 'SYS:READ-ERROR-1 "Internal error in #= after reading in label's value.")
	 ;; The preceding line should never happen.  By writing into the slot
	 ;; will RPLACD, we also cause other places that referred to the label
	 ;; to get the value, too.
	 (SETF (CONTENTS (CDR LABEL-BINDING)) (SETQ THING (INTERNAL-READ STREAM T NIL T)))
	 ;; If THING has no bindings in it, we can make the binding of THING itself instead of
	 ;; a locative
	 (IF (NOT (CONSP THING))
	     ;; Replace locative with object
	     (SETF (CDR LABEL-BINDING) (CONTENTS (CDR LABEL-BINDING)))
	   ;; If there are no bindings as locatives in it
;	   (IF (NOT (FIND-ANY-THINGS
;		      (REMQ (CDR LABEL-BINDING)
;			    (LET ((LOC-BINDINGS ()))
;			      (DOLIST (BINDING XR-LABEL-BINDINGS LOC-BINDINGS)
;				(IF (LOCATIVEP (CDR BINDING))
;				    (PUSH (CDR BINDING) LOC-BINDINGS)))))
;		      THING))
	   ;; Now we examine the list
	   (NSUBST-EQ-SAFE THING (CDR LABEL-BINDING) THING)	; Substitute for `self'
           (SETF (CDR LABEL-BINDING) (CONTENTS (CDR LABEL-BINDING)))
	   ;; Catch the CDR - why does this happen ?
	   (LET ((LAST-CONS (NTHCDR (1- (LENGTH THING)) THING)))
	     (IF (LOCATIVEP (CDR LAST-CONS))
		 (SETF (CDR LAST-CONS) (CONTENTS (CDR LAST-CONS))))))
	 ; Replace locative with object
	 THING))))
  )

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(defun nsubst-eq-safe (new old sexp &optional previous-sexps &aux car cdr)
  (cond ((eq sexp old) new)
	((atom sexp) sexp)
	((memq sexp previous-sexps) sexp)
	(t (with-stack-list* (previous-sexps sexp previous-sexps)
	     (setq car (nsubst-eq-safe new old (car sexp) previous-sexps))
	     (if (neq car (car sexp)) (setf (car sexp) car))
	     (with-stack-list* (previous-sexps (car sexp) previous-sexps)
	       (setq cdr (nsubst-eq-safe new old (cdr sexp) previous-sexps))
	       (if (neq cdr (cdr sexp)) (setf (cdr sexp) cdr))))
	   sexp)))

))

; From file QFILE.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN CHANGE-CAPABILITIES-CHAOS (HOST CAPABILITIES ENABLE-P
				  &OPTIONAL (HOST-UNIT NIL HOST-UNIT-SPECD)
				  &AUX PKT SUCCESS FILE-STRING COMMAND
				  CAP-STRING CURRENT-CAPABILITIES)
  (SETQ CURRENT-CAPABILITIES (GET CHAOS-HOST-CAPABILITIES-PLIST HOST))
  (SETQ COMMAND
	(IF ENABLE-P "ENABLE-CAPABILITIES" "DISABLE-CAPABILITIES"))
  (COND ((NOT CAPABILITIES)
	 (IF ENABLE-P (RETURN-FROM CHANGE-CAPABILITIES-CHAOS)
	   (SETQ CAPABILITIES CURRENT-CAPABILITIES)))
	((ATOM CAPABILITIES)
	 (SETQ CAPABILITIES (LIST CAPABILITIES))))
  (SETQ CAP-STRING "")
  (DOLIST (CAP CAPABILITIES)
    (SETQ CAP-STRING (STRING-APPEND (STRING-UPCASE CAP) " " CAP-STRING)))
  (OR HOST-UNIT-SPECD (SETQ HOST-UNIT (SEND HOST ':GET-HOST-UNIT)))
  (MULTIPLE-VALUE (PKT SUCCESS FILE-STRING)
    (SEND HOST-UNIT ':COMMAND NIL NIL NIL
	  COMMAND #/SP CAP-STRING))
  (UNWIND-PROTECT
    (WHEN SUCCESS
      ;; Succeeded on one host unit.
      ;; Record what our capabilities are for this host.
      (IF ENABLE-P
	  (SETQ CURRENT-CAPABILITIES (SI:NUNION-EQUAL CURRENT-CAPABILITIES CAPABILITIES))
	(SETQ CURRENT-CAPABILITIES
	      (SUBSET-NOT #'(LAMBDA (C) (MEMBER C CAPABILITIES))
			  CURRENT-CAPABILITIES)))
      (PUTPROP CHAOS-HOST-CAPABILITIES-PLIST CURRENT-CAPABILITIES HOST)
      ;; Also inform any other host units that are connected now.
      (OR HOST-UNIT-SPECD
	  (LET ((UNITS (SEND HOST ':HOST-UNITS)))
	    (DOLIST (UNIT UNITS)
	      (AND (NEQ UNIT HOST-UNIT)
		   (SEND UNIT ':VALIDATE-CONTROL-CONNECTION T)
		   (SEND UNIT ':COMMAND NIL NIL NIL
			 COMMAND #/SP CAP-STRING))))))
    (CHAOS:RETURN-PKT PKT)))

))

; From file COMB.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(DEFCOM COM-BEEP "Beep, and if not given a numeric arg turn off the region." ()
  (SEND *STANDARD-INPUT* ':SEND-IF-HANDLES ':MACRO-ERROR)
  (IF (OR *NUMERIC-ARG-P* (NOT (WINDOW-MARK-P *WINDOW*)))
      (BEEP))
  (IF *NUMERIC-ARG-P*
      (SETQ *MARK-STAYS* T)
    (SETQ *MARK-STAYS* NIL))
  DIS-NONE)

))

; From file QCDEFS.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFMACRO EXTRACT-DECLARATIONS-RECORD-MACROS (&REST ARGS)
  "Like EXTRACT-DECLARATIONS, but also record names of macros that expand into declarations."
  (DECLARE (ARGLIST BODY &OPTIONAL INITIAL-DECLS DOC-STRING-VALID-P))
  `(LET ((RECORD-MACROS-EXPANDED T))
     (EXTRACT-DECLARATIONS . ,ARGS)))

))

; From file QCFASD.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFASD  "

(DEFUN FASD-FONT (FONT-SYMBOL)
  "Write the font FONT into a QFASL file named SYS: FONTS; name-of-font QFASL."
  (LET ((TEM (SYMBOL-VALUE FONT-SYMBOL)))
    (UNWIND-PROTECT
	(PROGN (SETF (SYMBOL-VALUE FONT-SYMBOL) (TV::FONT-EVALUATE FONT-SYMBOL))
	       (FASD-SYMBOL-VALUE (FS:MAKE-PATHNAME ':HOST "SYS"
						    ':DIRECTORY "FONTS"
						    ':NAME (SYMBOL-NAME FONT-SYMBOL))
				  FONT-SYMBOL))
      (SETF (SYMBOL-VALUE FONT-SYMBOL) TEM)))
  NIL)

))

; From file QCDEFS.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFVAR LOCAL-FUNCTION-MAP)

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFCONST *DEBUG-INFO-LOCAL-DECLARATION-TYPES*
	  '((ARGLIST . ARGLIST)
	    (RETURN-LIST . VALUES)
	    (VALUES . VALUES)
	    (:ARGLIST . ARGLIST)
	    (:RETURN-LIST . VALUES)
	    (:VALUES . VALUES)
	    (FUNCTION-PARENT . FUNCTION-PARENT)
	    (COMPILER:COMPILER-ARGLIST . COMPILER:COMPILER-ARGLIST))
  "Local declaration types which are incorporated into the function debugging info by DEFUN
Each element is (DECLARATION . DEBUG-INFO-KEYWORD).
Note that there are many synonyms among the declarations.")

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN ARGLIST (FUNCTION &OPTIONAL REAL-FLAG &AUX TEM DEBUG-INFO ARG-MAP LOCAL-MAP)
  "Return the argument list of FUNCTION, and its value-list.
FUNCTION may be a function or a function spec.
If REAL-FLAG is T, return the actual argument list, good for compilation, calling, etc.
If REAL-FLAG is COMPILE, return the argument list generated by the compiler, 
 if FUNCTION is compiled. This arglist includes the names of the keys for &KEY arguments,
 if any, and the forms used for defaulting optional args. /"Supplied-p/" args are not included
 If the function is not compiled, this is the same as REAL-FLAG = T
Otherwise, return an argument list intended as documentation for humans.
 This will be the same as if REAL-FLAG were COMPILE, unless there was an explicit
 (DECLARE (ARGLIST ...)) in the defintiion of FUNCTION.
The second value is the value-list, only for documentation for humans.
The third value is NIL, SUBST or MACRO."
  (DECLARE (VALUES ARGLIST VALUES TYPE))
  (TYPECASE FUNCTION
    (SYMBOL
     (OR (GET FUNCTION 'ARGLIST)		;Handles names defined only in the compiler.
	 (ARGLIST (FSYMEVAL FUNCTION) REAL-FLAG)))
    (CONS
     (COND ((mEmQ (CAR FUNCTION) '(LAMBDA cli:lambda))
	    (LDIFF (CADR FUNCTION) (MEMQ '&AUX (CADR FUNCTION))))
	   ((MEMQ (CAR FUNCTION) '(SUBST CLI:SUBST))
	    (VALUES (CADR FUNCTION) NIL 'SUBST))
	   ((MEMQ (CAR FUNCTION) '(NAMED-SUBST NAMED-LAMBDA cli:named-subst cli:named-lambda))
	    (SETQ DEBUG-INFO (DEBUGGING-INFO FUNCTION))
	    (COND ((AND (MEMQ REAL-FLAG '(NIL COMPILE))
			(ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO))
		   (ARGLIST (CADR (ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO)) REAL-FLAG))
		  (T
		   (VALUES
		     (OR (IF (EQ REAL-FLAG 'NIL)
			     (CDR (ASSQ 'ARGLIST DEBUG-INFO)))
			 (IF (MEMQ REAL-FLAG '(COMPILE NIL))
			     (CDR (ASSQ 'COMPILER:COMPILER-ARGLIST DEBUG-INFO)))
			 (LDIFF (CADDR FUNCTION) (MEMQ '&AUX (CADDR FUNCTION))))
		     (CDR (ASSQ 'VALUES DEBUG-INFO))
		     (AND (EQ (CAR FUNCTION) 'NAMED-SUBST)
			  'SUBST)))))
	   ((MEMQ (CAR FUNCTION) '(CURRY-BEFORE CURRY-AFTER))
	    '(&REST ARGLIST))
	   ((EQ (CAR FUNCTION) 'MACRO)
	    ;; Look for (LOCAL-DECLARE ((ARGLIST ...))) type arglist
	    (SETQ DEBUG-INFO (DEBUGGING-INFO (CDR FUNCTION)))
	    (VALUES (OR (IF (EQ REAL-FLAG 'NIL)
			    (CDR (ASSQ 'ARGLIST DEBUG-INFO)))
			(IF (MEMQ REAL-FLAG '(COMPILE NIL))
			    (CDR (ASSQ 'COMPILER:COMPILER-ARGLIST DEBUG-INFO)))
			'MACRO)
		    (CDR (ASSQ 'VALUES (DEBUGGING-INFO (CDR FUNCTION))))
		    'MACRO))
	   ((VALIDATE-FUNCTION-SPEC FUNCTION)
	    (ARGLIST (FDEFINITION FUNCTION) REAL-FLAG))
	   (T (FERROR NIL "~S not a recognized function" FUNCTION))))
    (STACK-GROUP
     '(STACK-GROUP-ARG))
    (ARRAY
     (DO ((I (%P-LDB %%ARRAY-NUMBER-DIMENSIONS FUNCTION) (1- I))
	  (L NIL))
	 (( I 0) L)
       (SETQ L (CONS (INTERN (FORMAT NIL "DIM-~D" I) PKG-SYSTEM-INTERNALS-PACKAGE) L))))
    ((OR CLOSURE ENTITY)
     (ARGLIST (CAR (%MAKE-POINTER DTP-LIST FUNCTION)) REAL-FLAG))
    ((OR SELECT-METHOD INSTANCE)
     ;; Can't tell arglist, shouldn't give error though
     '(OP &REST SELECT-METHOD-ARGS-VARY))
    (COMPILED-FUNCTION
     (SETQ DEBUG-INFO (DEBUGGING-INFO FUNCTION))
     (SETQ ARG-MAP (CADR (ASSQ 'COMPILER:ARG-MAP DEBUG-INFO)))
     (SETQ LOCAL-MAP (CADR (ASSQ 'COMPILER:LOCAL-MAP DEBUG-INFO)))
     (VALUES
       (COND ((AND (EQ REAL-FLAG 'NIL)
		   (CDR (ASSQ 'ARGLIST DEBUG-INFO))))
	     ((AND (MEMQ REAL-FLAG '(COMPILE NIL))
		   (CDR (ASSQ 'COMPILER:COMPILER-ARGLIST DEBUG-INFO))))
	     ((SETQ TEM (GET-MACRO-ARG-DESC-POINTER FUNCTION))
	      (DO ((ADL TEM (CDR ADL))
		   (ARGNUM 0 (1+ ARGNUM))
		   (ARGNAME)
		   (OPTIONALP NIL)
		   (QUOTEP NIL)
;		   (DES-DT FEF-DT-DONTCARE)
		   (SPECIAL FEF-LOCAL)
		   (INIT)
		   (INITP T T)
		   (ADLWORD)
		   (ARGLIS NIL))
		  ((NULL ADL)
		   (NREVERSE ARGLIS))
		(SETQ ADLWORD (CAR ADL))
		(SELECT
		  (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
		  (FEF-ARG-REQ
		   (AND OPTIONALP
			(FERROR NIL "Required args after optionals in ~S" FUNCTION)))
		      (FEF-ARG-OPT (OR OPTIONALP (SETQ ARGLIS (CONS '&OPTIONAL ARGLIS)))
				   (SETQ OPTIONALP T))
		      (FEF-ARG-REST (SETQ ARGLIS (CONS '&REST ARGLIS)))
		      (OTHERWISE (RETURN (NREVERSE ARGLIS))))
		(SELECT (MASK-FIELD %%FEF-QUOTE-STATUS ADLWORD)
		  (FEF-QT-QT (OR QUOTEP (SETQ ARGLIS (CONS '&QUOTE ARGLIS)))
			     (SETQ QUOTEP T))
		  (FEF-QT-EVAL (AND QUOTEP (SETQ ARGLIS (CONS '&EVAL ARGLIS)))
			       (SETQ QUOTEP NIL)))
		(SETQ TEM (LDB %%FEF-DES-DT ADLWORD))
;		(COND ((NEQ TEM DES-DT)
;		       (SETQ DES-DT TEM)
;		       (SETQ ARGLIS (CONS (NTH TEM '(&DT-DONTCARE &DT-NUMBER 
;								  &DT-FIXNUM &DT-SYMBOL 
;								  &ATOM &LIST &DT-FRAME))
;					  ARGLIS))))
		(SETQ TEM (LDB %%FEF-SPECIAL-BIT ADLWORD))	;handle remote some time?
		(COND ((NEQ TEM SPECIAL)
		       (SETQ SPECIAL TEM)
		       (SETQ ARGLIS (CONS (NTH TEM '(&LOCAL &SPECIAL))
					  ARGLIS))))
		(SETQ ARGNAME (COND ((= (LOGAND ADLWORD %FEF-NAME-PRESENT)
					FEF-NM-YES)
				     (SETQ ADL (CDR ADL))
				     (CAR ADL))
				    (T
				     (SETQ ARGNAME (COND (( (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
							     FEF-ARG-REST)
							  (NTH ARGNUM ARG-MAP))
							 (T (CAR LOCAL-MAP))))
				     (IF (SYMBOLP ARGNAME) ARGNAME (CAR ARGNAME)))))
		(SELECT (MASK-FIELD %%FEF-INIT-OPTION ADLWORD)
		  (FEF-INI-NONE (SETQ INITP NIL))
		  (FEF-INI-NIL (SETQ INIT NIL))
		  (FEF-INI-PNTR
		   (SETQ ADL (CDR ADL))
		   (SETQ INIT (SELECTQ (%P-DATA-TYPE ADL)
				((#.DTP-EXTERNAL-VALUE-CELL-POINTER)
				 (MULTIPLE-VALUE-BIND (SYM CELL-FUNCTION)
				     (DECODE-EVCP (%P-CONTENTS-AS-LOCATIVE ADL))
				   (SELECTQ CELL-FUNCTION
				     (SYMEVAL SYM)
				     (FDEFINITION `(FUNCTION ,SYM))
				     (T `(,CELL-FUNCTION ',SYM)))))
				((#.DTP-SELF-REF-POINTER)
				 (FLAVOR-DECODE-SELF-REF-POINTER
				   (FEF-FLAVOR-NAME FUNCTION)
				   (%P-POINTER ADL)))
				(T `',(CAR ADL)))))
		  (FEF-INI-C-PNTR
		   (SETQ ADL (CDR ADL))
		   (COND ;((= (%P-DATA-TYPE ADL) DTP-EXTERNAL-VALUE-CELL-POINTER)
		         ; (SETQ INIT			;THIS IS A BIT OF A KLUDGE
		         ;       (%FIND-STRUCTURE-HEADER (%P-CONTENTS-AS-LOCATIVE ADL))))
		         ;HOPE IT'S VALUE-CELL-LOCATION
		     ((= (%DATA-TYPE (CAR ADL)) DTP-LOCATIVE)
		      (SETQ INIT (%FIND-STRUCTURE-HEADER (CAR ADL))))
		     ((SETQ INIT (CAAR ADL)))))
		  (FEF-INI-OPT-SA (SETQ ADL (CDR ADL))
				  (SETQ INIT '*HAIRY*))
		  (FEF-INI-COMP-C (SETQ INIT '*HAIRY*))
		  (FEF-INI-EFF-ADR (SETQ ADL (CDR ADL))
				   (SETQ INIT '*HAIRY*))
		  (FEF-INI-SELF (SETQ INIT ARGNAME)))
		(SETQ ARGLIS (CONS (COND (INITP
					  (LIST ARGNAME INIT))
					 (T ARGNAME)) ARGLIS))))
	     (T ;No ADL.  Use the fast-arg-option to get the general pattern
	      ;and the argmap for the names.
	      (LET ((FAST-OPT (%ARGS-INFO FUNCTION))
		    (RES NIL))
		(LET ((MIN-ARGS (LDB %%ARG-DESC-MIN-ARGS FAST-OPT))
		      (MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS FAST-OPT))
		      (EVALED-REST (LDB %%ARG-DESC-EVALED-REST FAST-OPT))
		      (QUOTED-REST (LDB %%ARG-DESC-QUOTED-REST FAST-OPT)))
		  (DOTIMES (I MIN-ARGS)
		    (PUSH (CAAR ARG-MAP) RES)
		    (SETQ ARG-MAP (CDR ARG-MAP)))
		  (OR (= MIN-ARGS MAX-ARGS)
		      (PUSH '&OPTIONAL RES))
		  (DOTIMES (I (- MAX-ARGS MIN-ARGS))
		    (PUSH (CAAR ARG-MAP) RES)
		    (SETQ ARG-MAP (CDR ARG-MAP)))
		  (OR (ZEROP QUOTED-REST)
		      (PUSH '&QUOTE RES))
		  (COND ((OR (NOT (ZEROP QUOTED-REST)) (NOT (ZEROP EVALED-REST)))
			 (PUSH '&REST RES)
			 (PUSH (CAAR LOCAL-MAP) RES)))
		  (NREVERSE RES)))))
       (DEBUG-INFO-VALUES DEBUG-INFO)))
    (MICROCODE-FUNCTION
     (MICRO-CODE-ENTRY-ARGLIST-AREA (%POINTER FUNCTION)))
    (T (FERROR NIL "~S is not a function" FUNCTION))))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN ARRAY-CANONICALIZE-TYPE (TYPE &AUX FOO)
  (COND ((MEMQ TYPE ARRAY-TYPES) TYPE)
	((SETQ FOO (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))
	 (NTH FOO ARRAY-TYPES))
	((FIXNUMP TYPE)
	 (IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
	     (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
	 (NTH TYPE ARRAY-TYPES))
	(T (SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE TYPE)))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-1 (THING)			;An internal subroutine
  (UNLESS (OR (NULL THING)			;Don't recursively describe boring things
	      (NUMBERP THING) (SYMBOLP THING) (STRINGP THING)
	      (LISTP THING))
    (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
    (LET ((*STANDARD-OUTPUT*			;Arrange for indentation by 5 spaces
	    (CLOSURE '(*STANDARD-OUTPUT*)
		     #'(LAMBDA (&REST ARGS)
			 (AND (EQ (SEND *STANDARD-OUTPUT* ':SEND-IF-HANDLES ':READ-CURSORPOS)
				  0)
			      (SEND *STANDARD-OUTPUT* ':STRING-OUT "     "))
			 (LEXPR-SEND *STANDARD-OUTPUT* ARGS)))))
      (DESCRIBE THING T))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(COMPILER:MAKE-OBSOLETE GET-FROM-ALTERNATING-LIST "use GETF instead")

))


; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFSUBST DEFSTRUCT-LISTP (X) (CLI:LISTP X))

))

; From file READ.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#S-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (LET* ((ARGS (INTERNAL-READ STREAM T NIL T))
	   (CONSTRUCTOR
	     (DOLIST (C (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS
			  (GET (CAR ARGS) 'DEFSTRUCT-DESCRIPTION)))
	       (IF (OR (NULL (CDR C))
		       (AND (STRINGP (CADR C)) (NULL (CDDR C))))
		   (RETURN (CAR C))))))
      (IF CONSTRUCTOR
	  (EVAL (CONS CONSTRUCTOR
		      (LOOP FOR (SLOT VALUE) ON (CDR ARGS) BY 'CDDR
			    APPEND `(,(INTERN (SYMBOL-NAME SLOT) PKG-KEYWORD-PACKAGE)
				     ',VALUE))))
	(CERROR ':NO-ACTION NIL 'READ-ERROR-1
		"~S is not a structure type with a standard keyword constructor." (CAR ARGS))
	NIL))))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-DEFINE-PRINTER (STRUCTURE-NAME REST &AUX (CLIP (POP REST)))
  (IF CLIP
      (LET ((STRUCTURE (MAKE-SYMBOL "STRUCTURE"))
	    (STREAM (MAKE-SYMBOL "STREAM"))
	    (DEPTH (MAKE-SYMBOL "DEPTH")))
	`(DEFSELECT ((:PROPERTY ,STRUCTURE-NAME NAMED-STRUCTURE-INVOKE) IGNORE)
	   (:PRINT-SELF (,STRUCTURE ,STREAM ,DEPTH &OPTIONAL IGNORE)
	     ;; the above "ignore" is for old callers who passed *print-escape*
	     (IF PRINT-READABLY
		 (PRINT-NOT-READABLE ,STRUCTURE)	;not always right... Sigh
	       (FUNCALL (FUNCTION ,(CAR REST)) ,STRUCTURE ,STREAM ,DEPTH)))))
    (LET ((STREAM (MAKE-SYMBOL "STREAM")))
      `(DEFSELECT ((,STRUCTURE-NAME NAMED-STRUCTURE-INVOKE) IGNORE)
	 (:PRINT-SELF (,STRUCTURE-NAME ,STREAM &REST IGNORE)
	   (IF PRINT-READABLY (PRINT-NOT-READABLE ,STRUCTURE-NAME)
	     (FORMAT ,STREAM ,@REST)))))))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-parse-options (options CLIP)
  (let ((name (if (atom options) options (car options)))
	(type nil)
	(constructors (defstruct-make-empty))
	(alterant (defstruct-make-empty))
	(included nil)
	(named-p CLIP)				;structures named by default in common lisp
	(but-first nil)
	(description (make-defstruct-description))
	(OLD))
    (setf (defstruct-description-name) name)
    (WHEN CLIP
      (SETF (DEFSTRUCT-DESCRIPTION-CONC-NAME) (DEFSTRUCT-APPEND-SYMBOLS NAME "-"))
      (SETF (DEFSTRUCT-DESCRIPTION-PREDICATE) (DEFSTRUCT-MAKE-EMPTY))
      (SETF (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS) T))
    (do ((op) (val) (vals)
	 (options (if (atom options) nil (cdr options))
		  (cdr options)))
	((null options))
      (if (atom (setq op (car options)))
	  (setq vals nil)
	  (setq op (prog1 (car op) (setq vals (cdr op)))))
      (setq val (if (null vals) (defstruct-make-empty) (car vals)))
      (SETQ OLD NIL)
      ;; If OP is not a keyword, change it to one and come back here.
    AGAIN
      (selectq op
	(:type
	 (if (defstruct-emptyp val)
	     (defstruct-error
	       "The :TYPE option to DEFSTRUCT must have a value given"
	       name))
	 ;; In Common Lisp, :TYPE implies it is not a true named structure.
	 ;; It may be a phony one (slot allocated for the type, but not
	 ;; marked as a named structure), so if NAMED-P is already :PHONY leave it alone.
	 (IF (AND CLIP (EQ NAMED-P T))
	     (SETQ NAMED-P NIL))
	 (setq type (IF (NULL (CDR VALS)) VAL VALS)))
	(:default-pointer
	 (setf (defstruct-description-default-pointer)
	       (if (defstruct-emptyp val) name val)))
	(:named
	 (or (defstruct-emptyp val)
	     (defstruct-error
	       "The :NAMED option to DEFSTRUCT doesn't take a value" name))
	 ;; In Common Lisp, :NAMED means just allocate a slot for the name,
	 ;; but not to make it a true named structure unless no :type is given
	 (setq named-p (IF CLIP ':PHONY T)))
	(:conc-name
	 (setf (defstruct-description-conc-name)
	       (if (defstruct-emptyp val)
		   (IF CLIP NIL
		     (defstruct-append-symbols name "-"))
		 val)))
	(:print
	 (if (defstruct-emptyp val)
	     (defstruct-error
	       "The :PRINT option to DEFSTRUCT requires a value"
	       name))
	 (setf (defstruct-description-print) (CONS NIL VALS)))	;commonlisp nil
	(:PRINT-FUNCTION
	 (AND (DEFSTRUCT-EMPTYP VAL)
	      (DEFSTRUCT-ERROR
		"The :PRINT-FUNCTION option to DEFSTRUCT requires a value"
		NAME))
	 (AND (CDR VALS)			;check against using :print syntax
	      (DEFSTRUCT-ERROR
		"The :PRINT-FUNCTION option to DEFSTRUCT takes only one value"))
	 (SETF (DEFSTRUCT-DESCRIPTION-PRINT) (CONS T VALS)))	;commonlisp t
	(:include
	 (if (defstruct-emptyp val)
	     (defstruct-error
	       "The :INCLUDE option to DEFSTRUCT requires a value"
	       name))
	 (setq included val)
	 (setf (defstruct-description-include) vals))
	(:predicate
	 (setf (defstruct-description-predicate)
	       (if (defstruct-emptyp val)
		   (defstruct-append-symbols name "-P")
		   val)))
	(:constructor
	 (cond ((null val)
		(setq constructors nil))
	       (t
		(and (defstruct-emptyp val)
		     (setq val (defstruct-append-symbols "MAKE-" name)))
		(setq val (cons val (cdr vals)))
		(if (defstruct-emptyp constructors)
		    (setq constructors (list val))
		  (push val constructors)))))
	(:copier
	 (setf (defstruct-description-copier)
	       (if (defstruct-emptyp val)
		   (defstruct-append-symbols "COPY-" name)
		 val)))
	#-(OR (AND LISPM MIT) NIL)
	(:eval-when
	 (and (defstruct-emptyp val)
	      (defstruct-error
	       "The :EVAL-WHEN option to DEFSTRUCT requires a value"
	       name))
	  (setf (defstruct-description-eval-when) val))
	(:alterant
	 (setq alterant val))
	(:but-first
	 (if (defstruct-emptyp val)
	     (defstruct-error
	       "The :BUT-FIRST option to DEFSTRUCT must have a value given"
	       name))
	 (setq but-first val)
	 (setf (defstruct-description-but-first) val))
	(:size-macro
	 (setf (defstruct-description-size-macro)
	       (if (defstruct-emptyp val)
		   (defstruct-append-symbols name "-SIZE")
		   val)))
	(:size-symbol
	 (setf (defstruct-description-size-symbol)
	       (if (defstruct-emptyp val)
		   (defstruct-append-symbols name "-SIZE")
		   val)))
	(:callable-accessors
	 (setf (defstruct-description-callable-accessors)
	       (if (defstruct-emptyp val) t val)))
	(:CALLABLE-CONSTRUCTORS
	 (SETF (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS)
	       (IF (DEFSTRUCT-EMPTYP VAL) T VAL)))
	(:property
	 (if (defstruct-emptyp val)
	     (defstruct-error
	       "The :PROPERTY option to DEFSTRUCT requires a value"
	       name))
	 (push (cons val (if (null (cdr vals)) t (cadr vals)))
	       (defstruct-description-property-alist)))
	(:initial-offset
	 (and (or (defstruct-emptyp val)
		  (not (fixp val)))
	      (defstruct-error
		"The :INITIAL-OFFSET option to DEFSTRUCT requires a fixnum"
		name))
	 (setf (defstruct-description-initial-offset) val))
	(t
	 (cond ((get op 'defstruct-type-description)
		(or (defstruct-emptyp val)
		    (defstruct-error
		      "DEFSTRUCT type used as an option with a value"
		      op 'in name))
		(setq type op))
	       (T
		(IF OLD (SETQ OP OLD)
		  (LET ((NEW (DEFSTRUCT-RETRY-KEYWORD OP)))
		    (UNLESS (EQ NEW OP)
		      (SETQ OLD OP OP NEW)
		      (GO AGAIN))))
		(PUSH (CONS NIL (CONS OP (IF (DEFSTRUCT-EMPTYP VAL) T VAL)))
		      ;; this  nil flags that not explicitly specified
		      (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST)))))))
    (WHEN (AND CLIP (DEFSTRUCT-EMPTYP (DEFSTRUCT-DESCRIPTION-PREDICATE)))
      (SETF (DEFSTRUCT-DESCRIPTION-PREDICATE)
	    (AND NAMED-P (DEFSTRUCT-APPEND-SYMBOLS NAME "-P"))))	    
    (if (defstruct-emptyp constructors)
	(setq constructors (list (cons (defstruct-append-symbols "MAKE-" name)
				       nil))))
    (setf (defstruct-description-constructors) constructors)
    (cond ((defstruct-emptyp alterant)
	   (SETQ ALTERANT (IF CLIP NIL
			   (defstruct-append-symbols "ALTER-" name)))))
    (setf (defstruct-description-alterant) alterant)
    (WHEN TYPE
      (WHEN (CONSP TYPE)
	(SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) (CADR TYPE))
	(PUSH (CONS T `(:SUBTYPE ,(IF (CDDR TYPE) (CDR TYPE) (CADR TYPE))))
	      (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST))
	(SETQ TYPE (CAR TYPE)))
      (UNLESS (KEYWORDP TYPE)
	(SETQ TYPE (DEFSTRUCT-RETRY-KEYWORD TYPE)))
      (LET ((TYPE-DESCRIPTION (OR (GET TYPE 'DEFSTRUCT-TYPE-DESCRIPTION)
				  (DEFSTRUCT-ERROR
				    "Unknown type in DEFSTRUCT"
				    TYPE 'IN NAME))))
	(if named-p
	    (setq type
		  (or (defstruct-type-description-named-type)
		      (defstruct-error
			"There is no way to make a :NAMED defstruct of this type"
			type 'in name))))))
    (cond (included
	     (let ((d (get-defstruct-description included)))
	       (if (null type)
		   (setq type (defstruct-description-type d))
		 (or (eq type (defstruct-description-type d))
		     (defstruct-error
		       "defstruct types must agree for :INCLUDE option"
		       included 'included 'by name)))
	       (and named-p
		    (NEQ type (defstruct-type-description-named-type
				(or (get type 'defstruct-type-description)
				    (defstruct-error
				      "Unknown type in DEFSTRUCT"
				      type 'in name 'including included))))
		    (defstruct-error
		      ":INCLUDEd defstruct's type isn't a named type"
		      included 'included 'by name))
	       (if (null but-first)
		   (setf (defstruct-description-but-first)
			 (defstruct-description-but-first d))
		 (or (equal but-first (defstruct-description-but-first d))
		     (defstruct-error
		       ":BUT-FIRST options must agree for :INCLUDE option"
		       included 'included 'by name)))))
	  ((null type)
	   (setq type
	     (cond ((EQ NAMED-P ':PHONY)
		    ':PHONY-NAMED-VECTOR)
		   (named-p
		    #+MacLisp-10 ':named-hunk
		    #+Multics ':named-list
		    #+LispM (IF CLIP ':NAMED-VECTOR ':named-array)
		    #+NIL ':extend)
		   (t
		    #+MacLisp-10 ':hunk
		    #+Multics ':list
		    #+LispM (IF CLIP ':VECTOR ':array)
		    #+NIL ':vector)))))
    (let ((type-description (or (get type 'defstruct-type-description)
				(defstruct-error
				  "Undefined defstruct type"
				  type 'in name))))
      (setf (defstruct-description-type) type)
      (setf (defstruct-description-named-p)
	    (eq (defstruct-type-description-named-type) type))
      (OR (DEFSTRUCT-DESCRIPTION-NAMED-P)
	  (NULL (DEFSTRUCT-DESCRIPTION-PRINT))
	  (DEFSTRUCT-EMPTYP (DEFSTRUCT-DESCRIPTION-PRINT))
	  (DEFSTRUCT-ERROR
	    ":PRINT or :PRINT-FUNCTION is allowed only for recognizable named structures"
	    NAME))
      (DO ((X (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST) (CDR X)))	;check validity
	  ((NULL X))
	(OR (CAAR X)				;defined explicitly via (:property foo bar)
	    (IF (OR (MEMQ (CADAR X) (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS))
		    (GET (CADAR X) ':DEFSTRUCT-OPTION))	;obsolete
		(SETF (CAR X) (CDAR X))
	      (DEFSTRUCT-ERROR
		"DEFSTRUCT doesn't understand this option"
		(CAR X) 'IN NAME))))
      (OR (MEMQ ':SUBTYPE (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS))
	  (SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) NIL)))
    description))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-DEFINE-CONSTRUCTORS (DESCRIPTION)
  (LET ((NAME (DEFSTRUCT-DESCRIPTION-NAME))
	RETURNS)
    (IF (NOT (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS))
	(DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
	  (DEFSTRUCT-PUT-MACRO (CAR CS) 'DEFSTRUCT-EXPAND-CONS-MACRO)
	  (DEFSTRUCT-PUTPROP-COMPILE-TIME (CAR CS) NAME 'DEFSTRUCT-NAME))
      ;; callable, commonlisp-style constructors
      (LET* ((SLOT-ALIST (DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
	     (SIZE (DEFSTRUCT-DESCRIPTION-SIZE))
	     (TYPE-DESCRIPTION (GET (DEFSTRUCT-DESCRIPTION-TYPE) 'DEFSTRUCT-TYPE-DESCRIPTION))
	     (CONS-KEYWORDS (DEFSTRUCT-TYPE-DESCRIPTION-CONS-KEYWORDS))
	     (FL (DEFSTRUCT-TYPE-DESCRIPTION-CONS-FLAVOR))
	     MNAME BODY SYM ARGLIST ARGS REST FROB FROBPPSS X S INIT INIT-LIST BOAP
	     OPT OPTPPSS OPT-SLOT OPTPPSS-SLOT FLAGS PPSS-FLAGS
	     NOPT NOPTPPSS NOPT-SLOT NOPTPPSS-SLOT
	     (F (MAKE-SYMBOL "FLAGS"))
	     (L (MAKE-SYMBOL "INIT-LIST"))
	     (R (GENSYM))
	     ;(D (MAKE-SYMBOL "DEFSTRUCT-DESCRIPTION"))
	     (Y (MAKE-SYMBOL "VALUE"))
	     (SL (MAKE-SYMBOL "SLOTS"))
	     (TEM (MAKE-SYMBOL "TEM"))	;rg lives!
	     CW CWN AL DOC
	     (CONS-WORDS (DO ((V CONS-KEYWORDS (CDR V))
			      R W)
			     ((NULL V) R)
			   (SETQ W (INTERN (SYMBOL-NAME (CAR V))))	;in *PACKAGE*
			   (PUSH W CW)
			   (PUSH (LIST 'QUOTE (CAR V)) CW)
			   (PUSH (MAKE-SYMBOL (STRING-APPEND (CAR V) "-SPECIFIED-P")) CW)
			   (PUSH (LIST W NIL (CAR CW)) R)
			   (PUSH W CWN)))
	     )
	(DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
	  (SETQ MNAME (CAR CS) BOAP T AL (CADR CS)
		ARGLIST () ARGS () REST NIL
		OPT () OPT-SLOT () FLAGS () OPTPPSS () OPTPPSS-SLOT () PPSS-FLAGS ()
		NOPT () NOPT-SLOT () NOPTPPSS () NOPTPPSS-SLOT ())
	  (IF (AND (CDR CS) (DEFSTRUCT-LISTP AL))
	      ;; it's a boa-constructor!
	      (IF (OR (AND (CDDR CS) (NOT (STRINGP (CADDR CS))))
		      (CDDDR CS))
		  (DEFSTRUCT-ERROR
		    "DEFSTRUCT constructors can only be specified by giving an arglist"
		    CS)
		(SETQ DOC (CADDR CS))
		(TAGBODY
		 REQUIRED
		    (SELECTQ (SETQ X (POP AL))
		      (&OPTIONAL (GO OPTIONAL))
		      (&REST (GO REST))
		      (&AUX (GO AUX))
		      (NIL (GO DONE))
		      (T (OR (SETQ S (CDR (ASSQ X SLOT-ALIST))) (GO SLOT-ERROR))
			 (PUSH X ARGS)
			 (PUSH X ARGLIST)
			 (COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				(PUSH X NOPTPPSS)
				(PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					    (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				      NOPTPPSS-SLOT))
			       (T
				(PUSH X NOPT)
				(PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				      NOPT-SLOT)))
			 (GO REQUIRED)))
		 OPTIONAL
		    (PUSH '&OPTIONAL ARGS)
		    (PUSH '&OPTIONAL ARGLIST)
		 OPT
		    (SELECTQ (SETQ X (POP AL))
		      (&OPTIONAL (GO OPT))
		      (&REST (GO REST))
		      (&AUX (GO AUX))
		      (NIL (GO DONE))
		      (T (PUSH X ARGLIST)		     
			 (IF (CONSP X)
			     (IF (CDDR X) (GO ARG-ERROR)
			       (PSETQ X (CAR X) INIT (CADR X)))
			   (SETQ INIT (DEFSTRUCT-MAKE-EMPTY)))
			 (OR (SETQ S (CDR (ASSQ X SLOT-ALIST))) (GO SLOT-ERROR))
			 (IF (DEFSTRUCT-EMPTYP INIT)
			     (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
			 (COND ((DEFSTRUCT-EMPTYP INIT)
				(SETQ SYM (GENSYM))
				(PUSH (LIST X NIL SYM) ARGS)
				(IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				    (PROGN (PUSH SYM PPSS-FLAGS)
					   (PUSH X OPTPPSS)
					   (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						       (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
						 OPTPPSS-SLOT))
				    (PUSH SYM FLAGS)
				    (PUSH X OPT)
				    (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					  OPT-SLOT)))
			       (T
				(PUSH (LIST X INIT) ARGS)
				(COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				       (PUSH X NOPTPPSS)
				       (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						   (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					     NOPTPPSS-SLOT))
				      (T
				       (PUSH X NOPT)
				       (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					     NOPT-SLOT)))))
			 (GO OPT)))
		 REST
		    (PUSH '&REST ARGS)
		    (PUSH '&REST ARGLIST)
		    (SELECTQ (SETQ X (POP AL))
		      ((&OPTIONAL &REST &AUX NIL) (GO ARG-ERROR))
		      (T (OR (SETQ S (CDR (ASSQ X SLOT-ALIST))) (GO SLOT-ERROR))
			 (SETQ REST X)
			 (PUSH X ARGS)
			 (PUSH X ARGLIST)
			 (COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				(PUSH X NOPTPPSS)
				(PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					    (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				      NOPTPPSS-SLOT))

		       (T
				(PUSH X NOPT)
				(PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				      NOPT-SLOT)))
			 (SELECTQ (SETQ X (POP AL))
			   (&AUX (GO AUX))
			   (NIL (GO DONE))
			   (T (GO ARG-ERROR)))))
		 AUX
		    (PUSH '&AUX ARGLIST)
		    (PUSH '&AUX ARGS)
		 OX
		    (SELECTQ (SETQ X (POP AL))
		      ((&OPTIONAL &REST &AUX) (GO ARG-ERROR))
		      (NIL (GO DONE))
		      (T (PUSH X ARGLIST)
			 (IF (CONSP X)
			     (IF (CDDR X) (GO ARG-ERROR)
			       (PSETQ X (CAR X) INIT (CADR X)))
			   (SETQ INIT (DEFSTRUCT-MAKE-EMPTY)))
			 (OR (SETQ S (CDR (ASSQ X SLOT-ALIST))) (GO SLOT-ERROR))
			 (IF (DEFSTRUCT-EMPTYP INIT) NIL
			   (PUSH (LIST X INIT) ARGS)
			   (COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				  (PUSH X OPTPPSS)
				  (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					      (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					NOPTPPSS-SLOT))
				 (T
				  (PUSH X NOPT)
				  (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					NOPT-SLOT))))
			 (GO OX)))
		 ARG-ERROR
		    (DEFSTRUCT-ERROR "Bad defstruct :CONSTRUCTOR argument list" AL 'FOR MNAME)
		 SLOT-ERROR
		    (DEFSTRUCT-ERROR "Invalid DEFSTRUCT slot-name" X 'WHILE 'DEFINING MNAME)
		 DONE
		    (UNLESS (NULL AL) (SETQ AL (CONS NIL AL)) (GO ARG-ERROR))))
	    ;; do this for non-boa-constructors
	    (IF (STRINGP (CADR CS)) (SETQ DOC (CADR CS)))
	    (UNLESS (NULL (IF DOC (CDDR CS) (CDR CS)))
	      (DEFSTRUCT-ERROR
		"Bad defstruct :CONSTRUCTOR specification" (CDR CS) 'FOR MNAME))
	    (SETQ BOAP NIL)
	    (PUSH '&KEY ARGLIST)
	    (PUSH '&KEY ARGS)
	    (DOLIST (S SLOT-ALIST)
	      (SETQ X (CAR S) S (CDR S)		;standardize our nomenclature
		    INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S))
	      (COND ((DEFSTRUCT-EMPTYP INIT)
		     (PUSH X ARGLIST)
		     (SETQ SYM (MAKE-SYMBOL (STRING-APPEND X "-SPECIFIED-P")))
		     (PUSH (LIST X NIL SYM) ARGS)
		     (COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			    (PUSH SYM PPSS-FLAGS)
			    (PUSH X OPTPPSS)
			    (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					(DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				  OPTPPSS-SLOT))
			   (T (PUSH SYM FLAGS)
			      (PUSH X OPT)
			      (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				    OPT-SLOT))))
		    (T
		     (PUSH (LIST X INIT) ARGS)
		     (PUSH (CAR ARGS) ARGLIST)
		     (COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			    (PUSH X NOPTPPSS)
			    (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					(DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				  NOPTPPSS-SLOT))
			   (T (PUSH X NOPT)
			      (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				    NOPT-SLOT))))))
	    (WHEN CONS-KEYWORDS
	      (PUSH '&OPTIONAL ARGLIST)
	      (LET ((PROPERTY-ALIST (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST)))
		(DOLIST (X CONS-KEYWORDS)
		  (PUSH (IF (ASSQ X PROPERTY-ALIST) `(,X ',(CDR (ASSQ X PROPERTY-ALIST))) X)
			ARGLIST))))
	    (SETQ ARGS (NCONC (COPYLIST* CONS-WORDS) ARGS)))
	  ;; crunch the args now that we've snarfed them
	  (SETQ ARGLIST (NREVERSE ARGLIST) ARGS (NREVERSE ARGS))
	  (SELECTQ FL
	      (:LIST
	       (SETQ INIT-LIST (MAKE-LIST SIZE))
	       (DO ((X INIT-LIST (CDR X))) ((NULL X))
		 (SETF (CAR X) NIL))
	       (DOLIST (X SLOT-ALIST)		;put zero inits where appropriate
		 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS (CDR X))
		     (SETF (NTH (CADR X) INIT-LIST) 0)))
	       (SETQ FROB `((SETF (CDR (NTH (CAR ,SL) ,L))) (CAR ,Y)))
	       (SETQ FROBPPSS `((SETF (LDB (CDAR ,SL) (NTH (CAAR ,SL) ,L)) (CAR ,Y)))))
	      (:ALIST
	       (SETQ INIT-LIST ())
	       (SETQ FROB `((IF (SETQ ,TEM (ASSQ (CAR ,SL) ,L))
				(SETF (CADDR ,TEM) (CAR ,Y))
			      (PUSH (CONS (CAR ,SL) (LIST 'QUOTE (CAR ,Y))) ,L))))
	       (SETQ FROBPPSS `((IF (SETQ ,TEM (ASSQ (CAAR ,SL) ,L))
				    (SETF (LDB (CDAR ,SL) (CDR ,TEM)) (CAR ,Y))
				  (PUSH (CONS (CAAR ,SL) (DPB (CAR ,Y) (CDAR ,SL) 0)) ,L)))))
	      (T (DEFSTRUCT-ERROR
		   "Unknown constructor kind"
		   FL 'IN TYPE-DESCRIPTION)))
	  (SETQ BODY
		(NCONC (IF INIT-LIST
			   `((SETQ ,L (LIST . ,INIT-LIST))))
		       #+LISPM					;needed elsewhere??
		       (IF REST
			   `((SETQ ,REST (COPYLIST ,REST))))	;can't trust stack lists
		       (IF (AND (NOT BOAP) CONS-KEYWORDS)
			   `((DO ((,F (LIST ,@CW) (CDDDR ,F)))
				 ((NULL ,F))
			       (WHEN (CAR ,F)
				 (PUSH (CONS (CADR ,F) (CADDR ,F)) ,R)))
			     ,@CWN))				;prevent compiler barfage
		       ;; optional arguments
		       (IF OPT
			   `((DO ((,F (LIST ,@FLAGS) (CDR ,F))
				  (,SL ',OPT-SLOT (CDR ,SL))
				  (,Y (LIST ,@OPT) (CDR ,Y)))
				 ((NULL ,Y))
			       (WHEN (CAR ,F) ,@FROB))))
		       ;; optional byte-slot arguments
		       (IF OPTPPSS
			   `((DO ((,F (LIST ,@PPSS-FLAGS) (CDR ,F))
				  (,SL ',OPTPPSS-SLOT (CDR ,SL))
				  (,Y (LIST ,@OPTPPSS) (CDR ,Y)))
				 ((NULL ,Y))
			       (WHEN (CAR ,F) ,@FROBPPSS))))
		       ;; required arguments
		       (IF NOPT
			   `((DO ((,SL ',NOPT-SLOT (CDR ,SL))
				  (,Y (LIST ,@NOPT) (CDR ,Y)))
				 ((NULL ,Y))
			       ,@FROB)))
		       ;; required byte-slot arguments
		       (IF NOPTPPSS
			   `((DO ((,SL ',NOPTPPSS-SLOT (CDR ,SL))
				  (,Y (LIST ,@NOPTPPSS) (CDR ,Y)))
				 ((NULL ,Y))
			       ,@FROBPPSS)))))
	  (DEFSTRUCT-PUTPROP MNAME NAME 'DEFSTRUCT-NAME)
	  (PUSH 
	    `(DEFUN ,MNAME ,ARGS
	       ,(IF (NOT DOC) `(DECLARE (ARGLIST ,ARGLIST)
					(FUNCTION-PARENT ,MNAME DEFSTRUCT))
		  DOC)
	       ,(IF DOC `(DECLARE (ARGLIST ,ARGLIST)
				  (FUNCTION-PARENT ,MNAME DEFSTRUCT)))
	       (LET (,L ,TEM ,R)
		 ,TEM
		 ,@BODY
		 ;; this is extremely kludgey. Should expand the cons function here! -- mly
		 (EVAL 
		   (FUNCALL ',(DEFSTRUCT-TYPE-DESCRIPTION-CONS-EXPANDER
			       (GET (DEFSTRUCT-DESCRIPTION-TYPE DESCRIPTION)
				    'DEFSTRUCT-TYPE-DESCRIPTION))
			    ,L
			    ',DESCRIPTION
			    ,R))))
	    RETURNS))))
    RETURNS))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DESCRIBE-DEFSTRUCT-DESCRIPTION (NAME)
  (DESCRIBE-DEFSTRUCT (GET-DEFSTRUCT-DESCRIPTION NAME) 'DEFSTRUCT-DESCRIPTION))

))


; From file QFASL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFASL  "

(DEFUN FASL-OP-INITIALIZE-ARRAY (&OPTIONAL LOAD-16BIT-MODE
				 &AUX ARRAY NUM TEM-ARRAY HACK)
  (SETQ HACK (FASL-GROUP))
  (SETQ ARRAY (AREF FASL-TABLE HACK))
  (CHECK-ARG ARRAY ARRAYP "an array")
  (SETQ NUM (FASL-NEXT-VALUE))			;Number of values to initialize with
  (SETQ TEM-ARRAY				;Indirect array used to store into it
	(MAKE-ARRAY NUM ':AREA 'FASL-TABLE-AREA 
		    	':TYPE (IF (NOT LOAD-16BIT-MODE) 
				   (%P-MASK-FIELD %%ARRAY-TYPE-FIELD ARRAY)
				 'ART-16B)
			':DISPLACED-TO ARRAY
			':FILL-POINTER 0))
  (DO ((N NUM (1- N))) ((ZEROP N))		;Initialize specified num of vals
      (LET ((N (FASL-NIBBLE-PEEK)))
	(IF (= (LOGAND %FASL-GROUP-TYPE N) FASL-OP-NULL-ARRAY-ELEMENT)
	    (PROGN
	      (FASL-NIBBLE)
	      (VECTOR-PUSH NIL TEM-ARRAY)
	      (%P-STORE-DATA-TYPE (ALOC ARRAY (1- (FILL-POINTER TEM-ARRAY)))
				  DTP-NULL))
	  (VECTOR-PUSH (FASL-NEXT-VALUE) TEM-ARRAY))))
  (RETURN-ARRAY TEM-ARRAY)
  (IF (TYPEP ARRAY ':NAMED-STRUCTURE)
      (when (memq ':fasload-fixup (named-structure-invoke ':which-operations array))
	(NAMED-STRUCTURE-INVOKE ':FASLOAD-FIXUP ARRAY)))
  HACK)

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
			   (:NEW-DESTINATIONS T
			    :SITE ,(LONG-SITE-NAME)))
			 PLIST))))
  (LET ((P (GETF PLIST ':PACKAGE)))
    (AND P (SETQ FASD-PACKAGE (PKG-FIND-PACKAGE P))))
  (FASD-START-GROUP NIL 0 FASL-OP-FILE-PROPERTY-LIST)
  ;; Put package prefixes on everything in the plist since it will be loaded in
  ;; the wrong package.  This way the symbols in the plist will always be loaded
  ;; into exactly the same package they were dumped from, while the rest of the
  ;; symbols in the file will be free to follow the usual rules for intern.
  (LET ((FASD-PACKAGE NIL))
    (FASD-CONSTANT PLIST)))

))


; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-VIEW-MAIL (ARG &AUX (LUSER USER-ID) (HOST FS:USER-LOGIN-MACHINE) DIR FILE)
  (USING-RESOURCE (WINDOW TV:POP-UP-FINGER-WINDOW)
    (SETF (TV:SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (SEND WINDOW ':SET-LABEL (IF (EQ ARG 1) "View file" "View mail"))
    (SEND WINDOW ':SET-PROCESS CURRENT-PROCESS)
    (TV:WINDOW-CALL (WINDOW :DEACTIVATE)
      (LET ((*TERMINAL-IO* WINDOW) (*QUERY-IO* WINDOW))
	(SETQ TV:KBD-ESC-TIME NIL)		;Window configuration stable.
	(SELECTQ ARG
	  (0 (SETQ LUSER (READ-LINE T NIL NIL NIL '((:PROMPT "View whose mail: "))))
	     (LET ((@-POS (STRING-SEARCH-CHAR #/@ LUSER))
		   (LUSER (STRING-UPCASE LUSER)))
	       (WHEN (NOT (NULL @-POS))		;Figure out if user wants a different machine
		 (SETQ HOST (SI:PARSE-HOST (SUBSTRING LUSER (1+ @-POS))))
		 (SETQ LUSER (SUBSTRING LUSER 0 @-POS)))))
	  (1 (SETQ FILE (PROMPT-AND-READ ':PATHNAME "View File: "))
	     (SEND WINDOW ':SET-LABEL (FORMAT NIL "Viewing ~A" FILE))))
	(UNLESS (EQ ARG 1)
	  (SEND WINDOW ':SET-LABEL (FORMAT NIL "Viewing ~A's new mail on ~A" LUSER HOST))
	  (SETQ DIR (FS:USER-HOMEDIR HOST NIL LUSER))	;this still isn't right, but...
	  (SETQ FILE (SEND DIR ':NEW-MAIL-PATHNAME)))
	(WITH-OPEN-FILE (S FILE ':ERROR NIL ':CHARACTERS T)
	  (COND ((ERRORP S)
		 (IF (EQ ARG 1)
		     (FORMAT WINDOW "~&Error opening ~A: ~A" FILE S)
		   (IF (CONDITION-TYPEP S 'FS:FILE-NOT-FOUND)
		       (FORMAT WINDOW "~&~A's mail file appears to be empty." LUSER)
		     (FORMAT WINDOW "~&Unable to view ~A's mail because ~A." LUSER S)))
		 (FORMAT WINDOW "~3%Type a space to flush: ")
		 (AWAIT-USER-TYPEAHEAD WINDOW))
		(T
		 (STREAM-COPY-UNTIL-EOF S WINDOW)
		 (SEND WINDOW ':CLEAR-INPUT)	;just to be safe
		 (FORMAT WINDOW
			 "~3&Type ~@[~C to delete ~A's mail, or ~]a space to flush: "
			 (NEQ ARG 1) #/DELETE LUSER)
		 (LET ((RESPONSE (SEND WINDOW ':TYI)))
		   (WHEN (AND (EQ RESPONSE #/DELETE)
			      (NEQ ARG 1)
			      (YES-OR-NO-P "Do you REALLY want to delete this mail ? "))
		     (SEND S ':DELETE)))))))))
  NIL)

))

; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFVAR *ESCAPE-KEYS*
     '((#/CLEAR KBD-ESC-CLEAR "Discard type-ahead" :KEYBOARD-PROCESS)
       (#/RESUME (KBD-ESC-RESUME) 
	"Allow deexposed typeout in window that Terminal-0-S would select.")
       (#/ SYSTEM-MENU-SET-MOUSE-SCREEN "Set Mouse screen")
       (#/FORM (KBD-SCREEN-REDISPLAY)
	"Clear and redisplay all windows (Page = Clear Screen)")
       (#/A KBD-ESC-ARREST
	"Arrest process in who-line (minus means unarrest)" :KEYBOARD-PROCESS)
       (#/B KBD-BURY
	"Bury the selected window" :TYPEAHEAD)
       (#/C KBD-COMPLEMENT
	'("Complement video black-on-white state"
	  "With an argument, complement the who-line documentation window")
	:KEYBOARD-PROCESS)
;      (#/D (SI:BUZZ-DOOR) (AND (SI:TECH-SQUARE-FLOOR-P 9) "Open the door"))
;      (#/E (SI:CALL-ELEVATOR) (AND (OR (SI:TECH-SQUARE-FLOOR-P 8)
;					(SI:TECH-SQUARE-FLOOR-P 9))
;				    "Call the elevator"))
       (#/F KBD-FINGER (FINGER-ARG-PROMPT)
	:TYPEAHEAD)
       (#/G (KBD-GC-STATUS) "Show the current state of all garbage collection.")
       (#/H (KBD-HOSTAT) "Show status of CHAOSnet hosts" :TYPEAHEAD)
       (#/I KBD-ESC-I
	"Selected window deexposed input notify flag (complement, or arg=1 on, 0 off)")
       (#/M KBD-ESC-MORE "Selected window **MORE** enable (complement, or arg=1 on, 0 off)"
	:KEYBOARD-PROCESS)
       (#/N KBD-ESC-NOTIFICATIONS "Allow notifications to come out."
	"Terminal 1 N  print all notifications (even old ones)"
	"Terminal 2 N  defer notifications, reset who-line"
	:TYPEAHEAD)
       (#/O KBD-OTHER-EXPOSED-WINDOW "Select another exposed window" :TYPEAHEAD)
       (#/Q KBD-ESC-Q
	(LET ((PRINTER (OR SI:*DEFAULT-BIT-ARRAY-PRINTER* SI:*DEFAULT-PRINTER*)))
	  (AND (GET (IF (CONSP PRINTER) (CAR PRINTER) PRINTER)
		    'SI:PRINT-BIT-ARRAY)
	       (FORMAT NIL "Hardcopy the screen on the ~A"
		       (IF (CONSP PRINTER)
			   (CAR PRINTER)
			 PRINTER)))))
       (#/S KBD-SWITCH-WINDOWS
	'("Select the most recently selected window.  With an argument, select the nth"
	  "previously selected window and rotate the top n windows.  (Default arg is 2)."
	  "With an arg of 1, rotate through all the windows."
	  "With a negative arg rotate in the other direction."
	  "With an argument of 0, select a window that wants to type out.")
	:TYPEAHEAD)
       (#/T KBD-ESC-T
	"Selected window deexposed typeout action.  0 - wait, 1 - notify, 2 - permit.")
       (#/V KBD-VIEW-MAIL "View new mail. Terminal 1 V - view any file."
	:TYPEAHEAD)
       (#/W KBD-ESC-W
	'("Switch which process the wholine looks at.  Default is just to refresh it"
	  " 1 means selected-window's process, 2 means freeze on this process,"
	  " 3 means rotate among all processes, 4 means rotate other direction,"
	  " 0 gives a menu of all processes"))
       (#/HOLD-OUTPUT KBD-ESC-OUTPUT-HOLD "Expose window on which we have /"Output Hold/"")
       (#/? KBD-ESC-HELP NIL :TYPEAHEAD)
       (#/HELP KBD-ESC-HELP NIL :TYPEAHEAD)
       (NIL) ;Ones after here are "for wizards"
       (#/CALL (KBD-USE-COLD-LOAD-STREAM) "Get to cold-load stream" :TYPEAHEAD)
       (#/C-T KBD-CLEAR-TEMPORARY-WINDOWS "Flush temporary windows")
       (#/C-CLEAR KBD-CLEAR-LOCKS "Clear window-system locks")
       (#/C-A KBD-ESC-ARREST-ALL "Arrest nearly all processes" :KEYBOARD-PROCESS))
  "Determines what to do with characters typed after the Terminal character.
A list of elements (CHAR FUNCTION DOCUMENTATION . OPTIONS).
CHAR is what character this element applies to.
If FUNCTION is a list, it is evaluated; otherwise, it is called with one arg,
 which is either NIL or the numeric arg (1 in Terminal 1 F).
The evaluation or calling is normally done in a separate process.
DOCUMENTATION can be a string, a function that returns a string, or NIL.
 NIL means the this entry will not appear in the help message.
 It can also be a list of strings that go on separate lines.
OPTIONS are keywords (with no values).  Defined options are:
    :TYPEAHEAD - copy the contents of the
	software buffer into the currently selected IO-BUFFER.  This has the
	effect of treating everything typed before the TERMINAL as typeahead to
	the currently selected window.  Useful for TERMINAL commands that
	change the selected window.  These commands should set KBD-ESC-TIME to NIL
	as soon as they change the selected window, unless they complete quickly
	(input should never be done with KBD-ESC-TIME non-NIL).
    :KEYBOARD-PROCESS - run the function in the keyboard process instead of starting
	a new process for it.")

))

; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFSELECT ((:PROPERTY IO-BUFFER NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (SELF *STANDARD-OUTPUT* IGNORE &OPTIONAL IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (SELF *STANDARD-OUTPUT* :NO-POINTER :TYPEP)
      (FORMAT T "~O: " (%POINTER SELF))
      (IF (= (IO-BUFFER-INPUT-POINTER SELF)
	     (IO-BUFFER-OUTPUT-POINTER SELF))
	  (PRINC "empty, ")
	(FORMAT T "~D entr~:@P, "
		(LET ((DIFF (- (IO-BUFFER-INPUT-POINTER SELF)
			       (IO-BUFFER-OUTPUT-POINTER SELF))))
		  (IF (< DIFF 0)
		      (+ DIFF (IO-BUFFER-SIZE SELF))
		    DIFF))))
      (FORMAT T "State: ~A" (IO-BUFFER-STATE SELF))))
  ((:GET :GET-LOCATION-OR-NIL :GET-LOCATION :GETL :PUTPROP :REMPROP :PUSH-PROPERTY :PLIST
    :PLIST-LOCATION :PROPERTY-LIST-LOCATION :SETPLIST :SET)
   . IO-BUFFER-PROPERTY-LIST-HANDLER))
(DEFUN IO-BUFFER-PROPERTY-LIST-HANDLER (OP SELF &REST ARGS)
  (APPLY 'SI:PROPERTY-LIST-HANDLER OP (LOCF (IO-BUFFER-PLIST SELF)) ARGS))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFSUBST %POINTER-PLUS (PTR1 PTR2)
  (%MAKE-POINTER-OFFSET DTP-FIX PTR1 PTR2))

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
	((STRINGP X) (%SXHASH-STRING X #o337))
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
				      ((OR (INTEGERP X) (CHARACTERP X))
				       (LDB 24. Y))
				      (T (SXHASH Y RANDOM-OBJECT-ACTION)))
				ROT)
			      HASH))))
	((FLONUMP X) (LOGXOR (%P-LDB-OFFSET #o0027 X 1)
			     (%P-LDB-OFFSET #o2701 X 1)
			     (%P-LDB #o0022 X)))
	((AND (TYPEP X 'INSTANCE)
	      (SEND X ':SEND-IF-HANDLES ':SXHASH RANDOM-OBJECT-ACTION)))
	;; this should use :send-if-handles also
	((AND (TYPEP X 'NAMED-STRUCTURE)
	      (MEMQ ':SXHASH (NAMED-STRUCTURE-INVOKE ':WHICH-OPERATIONS X))
	      (NAMED-STRUCTURE-INVOKE ':SXHASH X RANDOM-OBJECT-ACTION)))
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

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN ADJUSTABLE-ARRAY-P (ARRAY)
  "A Common Lisp function which returns T if ARRAY is an adjustable array (ie may have
ADJUST-ARRAY applied to it) This is true for all arrays on the Lisp Machine."
  (CHECK-TYPE ARRAY ARRAY)
  T)

))

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun readline (&rest read-args)
  "Read a line from STREAM and return it as a string.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used.

The second value is EOF-OPTION if we exit due to end of file.

The third value is the delimiter which ended the input, or NIL if
it ended due to EOF."
  (declare (arglist &optional stream eof-option options)
	   (values string-or-eof-option eof-flag delimiter))
  (let ((options nil))
    ;; This kludge is to let us take a third, optional argument.
    (cond ((> (length read-args) 2)
	   (setq options (third read-args))
	   (setq read-args (list (first read-args) (second read-args)))))
    (multiple-value-bind (stream eof-option)
	(decode-read-args read-args)
      (multiple-value-bind (string eof terminator)
	  (read-delimited-string '(#/Return #/End) stream
				 (eq eof-option 'no-eof-option) options)
	(values (if (and eof (zerop (length string))) eof-option string)
		(if eof eof-option)
		terminator)))))

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFVAR *TIMEZONES* '((0 "GMT" NIL #/Z)			;Greenwich
		      (0 "UT" NIL #/Z)
		      (1 NIL NIL #/A)
		      (2 NIL NIL #/B)
		      (3 NIL "ADT" #/C)
		      (4 "AST" "EDT" #/D)		;Atlantic
		      (5 "EST" "CDT" #/E)		;Eastern
		      (6 "CST" "MDT" #/F)		;Central
		      (7 "MST" "PDT" #/G)		;Mountain
		      (8 "PST" "YDT" #/H)		;Pacific
		      (9 "YST" "HDT" #/I)		;Yukon
		      (10. "HST" "BDT" #/K)		;Hawaiian
		      (11. "BST" NIL #/L)		;Bering
		      (12. NIL NIL #/M)
		      (-1 NIL NIL #/N)
		      (-2 NIL NIL #/O)
		      (-3 NIL NIL #/P)
		      (-4 NIL NIL #/Q)
		      (-5 NIL NIL #/R)
		      (-6 NIL NIL #/S)
		      (-7 NIL NIL #/T)
		      (-8 NIL NIL #/U)
		      (-9 NIL NIL #/V)
		      (-10. NIL NIL #/W)
		      (-11. NIL NIL #/X)
		      (-12. NIL NIL #/Y)
		      (3.5 "NST" NIL -1)		;Newfoundland
		      )
  "List of timezones: offset from gmt, name, daylight-savings-name, military character.")

))
(time:assign-zone "UT" 0)

(load "sys:sys2;clmac" :verbose nil :set-default-pathname nil)

; From file DISASS.LISP PS:<L.SYS2> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DISASS  "

(DEFUN DISASSEMBLE (FUNCTION &AUX FEF LIM-PC ILEN (DISASSEMBLE-OBJECT-OUTPUT-FUN NIL))
  "Print a disassembly of FUNCTION on STANDARD-OUTPUT.
FUNCTION can be a compiled function, an LAMBDA-expression (which will be compiled),
or a function spec (whose definition will be used)."
  (DO ((FUNCTION FUNCTION)) (())
    (COND ((TYPEP FUNCTION 'COMPILED-FUNCTION)
	   (SETQ FEF FUNCTION)
	   (RETURN))
	  ((AND (CONSP FUNCTION)
		(MEMQ (CAR FUNCTION) '(LAMBDA NAMED-LAMBDA cli:lambda cli:named-lambda SUBST CLI:SUBST NAMED-SUBST)))
	   (SETQ FEF (COMPILE-LAMBDA FUNCTION (GENSYM)))
	   (RETURN))
	  ((EQ (CAR-SAFE FUNCTION) 'MACRO)
	   (FORMAT T "~%Definition as macro")
	   (SETQ FUNCTION (CDR FUNCTION)))
	  (T
	   (SETQ FUNCTION (SI:DWIMIFY-PACKAGE FUNCTION))
	   (SETQ FUNCTION (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION))))))
  (WHEN (GET-MACRO-ARG-DESC-POINTER FEF)
    (SI:DESCRIBE-FEF-ADL FEF)
    (TERPRI))
  (SETQ LIM-PC (DISASSEMBLE-LIM-PC FEF))
  (DO ((PC (FEF-INITIAL-PC FEF) (+ PC ILEN))) (( PC LIM-PC))
    (TERPRI)
    (SETQ ILEN (DISASSEMBLE-INSTRUCTION FEF PC)))
  (TERPRI)
  FUNCTION)

(DEFUN DISASSEMBLE-INSTRUCTION (FEF PC &AUX WD OP SUBOP DEST REG DISP ILEN SECOND-WORD)
  "Print on STANDARD-OUTPUT the disassembly of the instruction at PC in FEF.
Returns the length of that instruction."
  (SETQ ILEN (DISASSEMBLE-INSTRUCTION-LENGTH FEF PC))
  (BLOCK NIL
    (SETQ WD (DISASSEMBLE-FETCH FEF PC))
    (FORMAT T "~3O" pc)
    (TYO #/SPACE)
    (SETQ OP (LDB #o1104 WD)
	  SUBOP (LDB #o1503 WD)
	  DEST (LDB #o1602 WD)
	  DISP (LDB #o0011 WD)
	  REG (LDB #o0603 WD))
    (COND ((= ILEN 2)
	   (INCF PC)
	   (SETQ SECOND-WORD (DISASSEMBLE-FETCH FEF PC))
	   ;; If a two-word insn has a source address, it must be an extended address,
	   ;; so set up REG and DISP to be right for that.
	   (UNLESS (= OP #o14)
	     (SETQ REG (LDB #o0603 SECOND-WORD)
		   DISP (DPB (LDB #o1104 SECOND-WORD)
			     #o0604 (LDB #o0006 SECOND-WORD))))))
    (WHEN (< OP #o11) (SETQ OP (LDB #o1105 WD)))
    (COND ((ZEROP WD)
	   (PRINC "0"))
	  ((< OP 11)				;DEST/ADDR 
	   (PRINC (NTH OP '(CALL CALL0 MOVE CAR CDR CADR CDDR CDAR CAAR)))
	   (TYO #/SPACE)
	   (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o11)				;ND1
	   (PRINC (NTH SUBOP '(ND1-UNUSED + - * // LOGAND LOGXOR LOGIOR)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o12)				;ND2
	   (PRINC (NTH SUBOP '(= > < EQ SETE-CDR SETE-CDDR SETE-1+ SETE-1-)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o13)				;ND3
	   (PRINC (NTH SUBOP '(BIND-OBSOLETE? BIND-NIL BIND-POP SET-NIL SET-ZERO PUSH-E MOVEM POP)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o14)				;BRANCH
	   (PRINC (NTH SUBOP '(BR BR-NIL BR-NOT-NIL BR-NIL-POP
			       BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM BR-ILL-7)))
	   (TYO #/SPACE)
	   (AND (> DISP #o400) (SETQ DISP (LOGIOR #o-400 DISP)))	;Sign-extend
	   (IF (NEQ DISP -1)
	       ;; One word
	       (FORMAT T "~O" (+ PC DISP 1))
	     ;; Long branch
	     (SETQ DISP SECOND-WORD)
	     (AND ( DISP #o100000) (SETQ DISP (LOGIOR #o-100000 DISP)))
	     (FORMAT T "LONG ~O" (+ PC DISP 1))
	     (RETURN)))
	  ((= OP #o15)				;MISC
	   (PRINC "(MISC) ")			;Moon likes to see this
	   (IF (BIT-TEST 1 SUBOP)
	       (SETQ DISP (+ DISP #o1000)))
	   (COND ((< DISP #o200)
		  (FORMAT T "~A (~D) "
			  (NTH (LDB #o0403 DISP)
			       '(AR-1 ARRAY-LEADER %INSTANCE-REF UNUSED-AREFI-3
				 AS-1 STORE-ARRAY-LEADER %INSTANCE-SET UNUSED-AREFI-7))
			  (+ (LDB #o0004 DISP)
			     (IF (= (LDB #o0402 DISP) 2) 1 0))))
		 ((< DISP #o220)
		  (FORMAT T "UNBIND ~D binding~:P " (- DISP #o177))	;200 does 1 unbind.
		  (AND (ZEROP DEST) (RETURN)))
		 ((< DISP #o240)
		  (FORMAT T "POP-PDL ~D time~:P " (- DISP #o220))	;220 does 0 pops.
		  (AND (ZEROP DEST) (RETURN)))
		 ((= DISP #o460)		;(GET 'INTERNAL-FLOOR-1 'QLVAL)
		  (PRINC (NTH (TRUNCATE DEST 2) '(FLOOR CEILING TRUNCATE ROUND)))
		  (PRINC " one value to stack")
		  (SETQ DEST NIL))
		 ((= DISP #o510)		;(GET 'INTERNAL-FLOOR-2 'QLVAL)
		  (PRINC (NTH (TRUNCATE DEST 2) '(FLOOR CEILING TRUNCATE ROUND)))
		  (PRINC " two values to stack")
		  (SETQ DEST NIL))
		 (T
                  (LET ((OP (MICRO-CODE-SYMBOL-NAME-AREA (- DISP #o200))))
                    (COND ((NULL OP) (FORMAT T "#~O " DISP))
                          (T (FORMAT T "~A " OP))))))
	   (WHEN DEST (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST)))))
	  ((= OP #o16)				;ND4
	   (SELECTQ SUBOP
	     (0 (FORMAT T "STACK-CLOSURE-DISCONNECT  local slot ~D" DISP))
	     (1 (LET ((LOCALNUM
			(LDB #o0012 (NTH DISP
					 (%P-CONTENTS-OFFSET
					   FEF (- (%P-LDB %%FEFH-PC-IN-WORDS FEF) 2))))))
		  (FORMAT T "STACK-CLOSURE-UNSHARE LOCAL|~D" LOCALNUM)
		  (LET ((TEM (DISASSEMBLE-LOCAL-NAME FEF LOCALNUM)))
		    (AND TEM (FORMAT T " ~30,8T;~A" TEM)))))
	     (2 (FORMAT T "MAKE-STACK-CLOSURE  local slot ~D" DISP))
	     (3 (FORMAT T "PUSH-NUMBER ~S" DISP))
	     (4 (FORMAT T "STACK-CLOSURE-DISCONNECT-FIRST  local slot ~D" DISP))
	     (5 (FORMAT T "PUSH-CDR-IF-CAR-EQUAL ")
		(DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD PC))
	     (6 (FORMAT T "PUSH-CDR-STORE-CAR-IF-CONS ")
		(DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD PC))
	     (T (FORMAT T "UNDEF-ND4-~D ~D" SUBOP DISP))))
	  ((= OP #o20)
	   (FORMAT T "~A (~D) "
		   (NTH REG
			'(AR-1 ARRAY-LEADER %INSTANCE-REF COMMON-LISP-AR-1
			  SET-AR-1 SET-ARRAY-LEADER SET-%INSTANCE-REF UNUSED-AREFI))
		   (+ (LDB #o0006 DISP)
		      (IF (MEMQ REG '(2 6)) 1 0)))
	   (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST))))
	  (T					;UNDEF
	   (FORMAT T "UNDEF-~O" op))))
  ILEN)

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CHECK-ARG-TYPE (ARG-NAME TYPE &OPTIONAL TYPE-STRING)
  "Generate an error unless (TYPEP ARG-NAME 'TYPE).
TYPE-STRING is a string to use in the error message, such as /"a list/".
If you omit it, it will be computed from TYPE's pname."
  (IF (NULL TYPE-STRING)
      (SETQ TYPE-STRING (TYPE-PRETTY-NAME TYPE)))
  `(DO () ((TYPEP ,ARG-NAME ',TYPE))
     (SETQ ,ARG-NAME
	   (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
		   "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
		   ',TYPE ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO ETYPECASE (OBJECT &BODY CLAUSES)
  "Execute the first clause whose type specifier OBJECT fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of OBJECT.
If the result is T, the rest of that clause is excuted and the values
 of the last form in it are the values of the ETYPECASE form.
If no clause fits, an uncorrectable error is signaled."
  (LET ((SAVE-OBJECT OBJECT))
    (ONCE-ONLY (OBJECT)
      `(COND
	 ,@(LOOP FOR (TYPE . CONSEQUENTS) IN CLAUSES
		 COLLECT `(,(PROGN
			      (MACRO-TYPE-CHECK-WARNING 'TYPECASE TYPE)
				     `(TYPEP ,OBJECT ',TYPE))
			   NIL . ,CONSEQUENTS))
	 (T (FERROR NIL
		    "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
		    '(OR . ,(MAPCAR 'CAR CLAUSES))
		    ,OBJECT ',SAVE-OBJECT
		    ,(TYPE-PRETTY-NAME `(OR . ,(MAPCAR 'CAR CLAUSES)))))))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO CTYPECASE (OBJECT &BODY CLAUSES)
  "Execute the first clause whose type specifier OBJECT fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of OBJECT.
If the result is T, the rest of that clause is excuted and the values
 of the last form in it are the values of the CTYPECASE form.
If no clause fits, a correctable error is signaled.
The user can correct with a new value for OBJECT."
  (LET ((SAVE-OBJECT OBJECT))
    `(BLOCK CTYPECASE-LOOP
       (TAGBODY
      CTYPECASE-LOOP
	 (RETURN-FROM CTYPECASE-LOOP
	   ,(ONCE-ONLY (OBJECT)
	      `(COND
		 ,@(LOOP FOR (TYPE . CONSEQUENTS) IN CLAUSES
			 COLLECT `(,(PROGN
				      (MACRO-TYPE-CHECK-WARNING 'TYPECASE TYPE)
				      `(TYPEP ,OBJECT ',TYPE))
				   NIL . ,CONSEQUENTS))
		 (T (SETF ,SAVE-OBJECT
			  (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
				  "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
				  '(OR . ,(MAPCAR 'CAR CLAUSES))
				  ,OBJECT ',SAVE-OBJECT
				  ,(TYPE-PRETTY-NAME `(OR . ,(MAPCAR 'CAR CLAUSES)))))
		    (GO CTYPECASE-LOOP)))))))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO CCASE (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the CCASE.
If no clause matches, a correctable error is signaled.
The user can correct with a new value for TEST-OBJECT."
  (LET (TEST-EXP COND-EXP TYPE-FOR-ERROR)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '.SELECTQ.ITEM.)))
    (SETQ TYPE-FOR-ERROR
	  `(MEMBER . ,(MAPCAN #'(LAMBDA (CLAUSE)
				  (LET ((MATCH (CAR CLAUSE)))
				    (IF (NOT (CONSP MATCH))
					(LIST MATCH)
				      (COPY-LIST MATCH))))
			      CLAUSES)))
    (SETQ COND-EXP
	  `(COND
	     ,@(MAPCAR #'(LAMBDA (CLAUSE)
			   (MACRO-TYPE-CHECK-WARNING 'CCASE (CAR CLAUSE))
			   (COND ((ATOM (CAR CLAUSE))
				  `((EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))
				 (T
				  `((MEMBER-EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))))
		       CLAUSES)
	     (T (SETF ,TEST-OBJECT
		      (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
			      "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
			      ',TYPE-FOR-ERROR
			      ,TEST-EXP ',TEST-OBJECT
			      ,(TYPE-PRETTY-NAME TYPE-FOR-ERROR)))
		(GO CCASE-LOOP))))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'CCASE)
    (UNLESS (EQL TEST-EXP TEST-OBJECT)
      (SETQ COND-EXP
	    `(LET ((.SELECTQ.ITEM. ,TEST-OBJECT))
	       ,COND-EXP)))
    `(BLOCK CCASE-LOOP
       (TAGBODY
      CCASE-LOOP
         (RETURN-FROM CCASE-LOOP
	   ,COND-EXP)))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO ECASE (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (LET (TEST-EXP COND-EXP TYPE-FOR-ERROR)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '.SELECTQ.ITEM.)))
    (SETQ TYPE-FOR-ERROR
	  `(MEMBER . ,(MAPCAN #'(LAMBDA (CLAUSE)
				  (LET ((MATCH (CAR CLAUSE)))
				    (IF (NOT (CONSP MATCH))
					(LIST MATCH)
				      (COPY-LIST MATCH))))
			      CLAUSES)))
    (SETQ COND-EXP
	  `(COND
	     ,@(MAPCAR #'(LAMBDA (CLAUSE)
			   (MACRO-TYPE-CHECK-WARNING 'ECASE (CAR CLAUSE))
			   (COND ((ATOM (CAR CLAUSE))
				  `((EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))
				 (T
				  `((MEMBER-EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))))
		       CLAUSES)
	     (T (FERROR NIL
			"The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
			',TYPE-FOR-ERROR
			,TEST-EXP ',TEST-OBJECT
			,(TYPE-PRETTY-NAME TYPE-FOR-ERROR)))))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'ECASE)
    (UNLESS (EQL TEST-EXP TEST-OBJECT)
      (SETQ COND-EXP
	    `(LET ((.SELECTQ.ITEM. ,TEST-OBJECT))
	       ,COND-EXP)))
    COND-EXP))

))

; From file PATED.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "


(DEFCOM COM-ADD-PATCH-CHANGED-SECTIONS "Offer to Add Patch for each changed section.
Does not ask about sections that haven't been changed
or that have been patched since they were last changed.
Type P to patch all the rest of one buffer's changed sections
with no more questions.  Questions will resume in the next buffer." ()
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (AND (EQ (SEND BUFFER ':MAJOR-MODE)
	     'LISP-MODE)
	 (NOT (SEND BUFFER ':GET-ATTRIBUTE ':PATCH-FILE))
	 (IF (EQ (ADD-PATCH-BUFFER-CHANGED-FUNCTIONS BUFFER) ':QUIT) (RETURN))))
  DIS-NONE)

))

; From file PATED.LISP PS:<L.ZWEI> OZ:
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
					    ((:QUIT "Quit") #/Q)
					    . ,FORMAT:Y-OR-N-P-CHOICES))
					"Add ~S to patch? " NAME)
			 (:PROCEED (SETQ PROCEED-FLAG T))
			 (:QUIT (RETURN-FROM ADD-PATCH-BUFFER-CHANGED-FUNCTIONS ':QUIT))
			 ((T) T)))
	       (ADD-PATCH-INTERVAL SECTION NIL T NAME BUFFER))))))
  NIL)

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN STORE-KEYWORD-ARG-VALUES (FRAME-POINTER ARGS KEYKEYS ALLOW-OTHER-KEYS
					       &OPTIONAL FIRST-KEYARG-POINTER
					       SPECVAR-LIST)
  (LET ((FIRST-KEYWORD-ARG-INDEX
	  (IF (LOCATIVEP FIRST-KEYARG-POINTER)
	      (PROGN (SETQ FRAME-POINTER FIRST-KEYARG-POINTER) -1)
	    (%P-LDB-OFFSET %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN
			   FRAME-POINTER
			   %LP-ENTRY-STATE))))
    (IF (GETF ARGS ':ALLOW-OTHER-KEYS)
	(SETQ ALLOW-OTHER-KEYS T))
    ;; First decode what was specified.
    (DO ((ARGS-LEFT ARGS (CDDR ARGS-LEFT))
	 (FOUND-FLAGS 0))
	((NULL ARGS-LEFT))
      (LET ((KEYWORD (CAR ARGS-LEFT)))
	(DO-FOREVER
	  (LET ((INDEX (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
	    (COND (INDEX
		   (WHEN (ZEROP (LOGAND 1 (ASH FOUND-FLAGS (- INDEX))))
		     (SETQ FOUND-FLAGS
			   (DPB 1 (BYTE 1 INDEX) FOUND-FLAGS))
		     (LET ((SPECVAR (NTH INDEX SPECVAR-LIST)))
		       (IF SPECVAR
			   (SET SPECVAR (CADR ARGS-LEFT)))
		       (%P-STORE-CONTENTS-OFFSET (CADR ARGS-LEFT) FRAME-POINTER
						 (+ 1 INDEX FIRST-KEYWORD-ARG-INDEX))))
		   (RETURN))
		  (ALLOW-OTHER-KEYS (RETURN))
		  ((EQ KEYWORD ':ALLOW-OTHER-KEYS) (RETURN))
		  (T
		   (SETQ KEYWORD (CERROR ':NEW-KEYWORD NIL 'SYS:UNDEFINED-KEYWORD-ARGUMENT
					 "Keyword arg keyword ~S unrecognized."
					 KEYWORD (CADR ARGS-LEFT)))
		   (OR KEYWORD (RETURN))))))))))

(DEFUN STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
       (PREV-SLOT-POINTER ARGS KEYKEYS ALLOW-OTHER-KEYS SPECVAR-LIST)
  (IF (GETF ARGS ':ALLOW-OTHER-KEYS)
      (SETQ ALLOW-OTHER-KEYS T))
  ;; First decode what was specified.
  (DO ((ARGS-LEFT ARGS (CDDR ARGS-LEFT))
       (FOUND-FLAGS 0))
      ((NULL ARGS-LEFT))
    (LET ((KEYWORD (CAR ARGS-LEFT)))
      (DO-FOREVER
	(LET ((INDEX (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
	  (COND (INDEX
		 (WHEN (ZEROP (LOGAND 1 (ASH FOUND-FLAGS (- INDEX))))
		   (SETQ FOUND-FLAGS
			 (DPB 1 (BYTE 1 INDEX) FOUND-FLAGS))
		   (LET ((SPECVAR (NTH INDEX SPECVAR-LIST)))
		     (IF SPECVAR
			 (SET SPECVAR (CADR ARGS-LEFT))
		       ;; This var is not special.
		       ;; Decrement INDEX by the number of preceding vars that are special,
		       ;; because they don't have local slots.
		       (DO ((I INDEX (1- I))
			    (TAIL SPECVAR-LIST (CDR TAIL)))
			   ((ZEROP I))
			 (WHEN (CAR TAIL)
			   (DECF INDEX)))
		       (%P-STORE-CONTENTS-OFFSET (CADR ARGS-LEFT) PREV-SLOT-POINTER
						 (+ 1 INDEX)))))
		 (RETURN))
		(ALLOW-OTHER-KEYS (RETURN))
		((EQ KEYWORD ':ALLOW-OTHER-KEYS) (RETURN))
		(T
		 (SETQ KEYWORD (CERROR ':NEW-KEYWORD NIL 'SYS:UNDEFINED-KEYWORD-ARGUMENT
				       "Keyword arg keyword ~S unrecognized."
				       KEYWORD (CADR ARGS-LEFT)))
		 (OR KEYWORD (RETURN)))))))))

))

; From file CHSAUX.LISP PS:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFUN HOST-UPTIME (HOST &OPTIONAL (STREAM *STANDARD-OUTPUT*) (TIMEOUT 240.) &AUX PKT TIME)
  "Print a human readable time onto stream STREAM.
Returns the uptime (an integer) if host up, NIL if host down."
  (CONDITION-CASE ()
      (SETQ PKT (SIMPLE HOST "UPTIME" TIMEOUT))	;packet we send out
    (SYS:REMOTE-NETWORK-ERROR
     (IF STREAM
	 (FORMAT STREAM "Host ~A is apparently not up." (SEND (SI:PARSE-HOST HOST) ':NAME)))
     NIL)
    (:NO-ERROR (SETQ TIME (// (DECODE-CANONICAL-TIME-PACKET PKT) 60.))
	       (IF STREAM (TIME:PRINT-INTERVAL-OR-NEVER TIME STREAM))
	       (RETURN-PKT PKT)
	       TIME)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defconst type-of-alist
	  '((#.dtp-symbol . symbol)
	    (#.dtp-character . character)
	    (#.dtp-character . cli:character)
	    (#.dtp-list . cons)
	    (#.dtp-fix . fixnum)
	    (#.dtp-locative . locative)
	    (#.dtp-fef-pointer . compiled-function)
	    (#.dtp-closure . closure)
	    (#.dtp-entity . entity)
	    (#.dtp-instance . instance)
	    (#.dtp-u-entry . microcode-function)
	    (#.dtp-select-method . select)
	    (#.dtp-small-flonum . short-float)
	    (#.dtp-stack-group . stack-group)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defprop cli:character character type-alias-for)

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun coerce (object result-type)
  "Coerce OBJECT to an object of type RESULT-TYPE.  Only certain coercions are allowed.
Any sequence can be coerced to any sequence type if the elements are legal.
Strings, symbols and integers can be coerced to type CHARACTER.
Any number can be coerced to type COMPLEX.
Any real number can be coerced to any floating point number type."
  (if (typep object result-type)
      object
    (prog ((canon (type-canonicalize result-type)))
      (selectq (if (atom canon) canon (car canon))
	(list
	 (cond ((typep object 'vector)
		(return (listarray object)))))
	(short-float
	 (if (realp object)
	     (return (small-float object))))
	(single-float
	 (if (realp object)
	     (return (float object))))
	(float
	 (if (realp object)
	     (return (if (small-floatp object) object (float object)))))
	((t) (return object))
	(complex (return (complex object)))
	((character cli:character)
	 (cond ((stringp object)
		(if (= (length object) 1)
		    (return (aref object 0))))
	       ((symbolp object)
		(if (= (length (get-pname object)) 1)
		    (return (aref (get-pname object) 0))))
	       ((integerp object)
		(return (int-char object)))))
	((array simple-array)
	 (when (typep object 'sequence)
	   (return (make-array (length object)
			       ':initial-contents object
			       ':element-type
			       (if (atom canon) t
				 (cadr canon)))))))
      ;; If it did not already RETURN, this coercion is not allowed.
      (ferror nil "~S cannot be coerced to a ~S." object result-type))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun coerce-optimizer (form)
  (if (not (compiler:quotep (caddr form)))
      form
    (let ((canon (type-canonicalize (cadr (caddr form))))
	  (object (cadr form)))
      (selectq (if (atom canon) canon (car canon))
	(list
	 (once-only (object)
	   `(if (consp ,object) ,object (coerce-to-list ,object))))
	(short-float `(small-float ,object))
	(single-float `(float ,object))
	(float
	 (once-only (object)
	   `(if (small-floatp ,object) ,object (float ,object))))
	((t) object)
	((or character cli:character) `(coerce-to-character ,object))
	((array simple-array)
	 `(coerce-to-array-optimized
	    ,object
	    ',(array-type-from-element-type
		(if (atom canon) t (cadr canon)))))))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defsubst flonump (object)
  "T if OBJECT is a full precision flonum."
  (and (floatp object)
       (not (small-floatp object))))

))


; From file QMISC.SAV USRD$:[MLY] OBI:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN CHANGE-INDIRECT-ARRAY (ARRAY TYPE DIMLIST DISPLACED-P INDEX-OFFSET
			      &AUX INDEX-LENGTH NDIMS INDIRECT-LENGTH TEM
				   OLD-NDIMS OLD-INDIRECT-LENGTH)
  "Change an indirect array ARRAY's type, size, or target pointed at.
TYPE specifies the new array type, DIMLIST its new dimensions,
DISPLACED-P the target it should point to (array, locative or fixnum),
INDEX-OFFSET the new offset in the new target."
  (CHECK-TYPE ARRAY ARRAY)
  (OR (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
      (FERROR NIL "~S is not a displaced array." ARRAY))
  (CHECK-ARG DISPLACED-P (OR (ARRAYP DISPLACED-P) (INTEGERP DISPLACED-P)
			     (LOCATIVEP DISPLACED-P))
	     "an array or physical address to indirect to")
  (CHECK-ARG TYPE				;TEM gets the numeric array type
	     (SETQ TEM (COND ((NUMBERP TYPE) (LDB %%ARRAY-TYPE-FIELD TYPE))
			     ((FIND-POSITION-IN-LIST TYPE ARRAY-TYPES))))
	     "an array type")
  (SETQ TYPE TEM)
  (IF (NLISTP DIMLIST)
      (SETQ NDIMS 1 INDEX-LENGTH (EVAL DIMLIST))
    (SETQ NDIMS (LENGTH DIMLIST)
	  INDEX-LENGTH (LIST-PRODUCT DIMLIST)))
  (SETQ INDIRECT-LENGTH (IF INDEX-OFFSET 3 2)
	OLD-NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)
	OLD-INDIRECT-LENGTH (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))
  (OR (= NDIMS OLD-NDIMS)
      (FERROR NIL "Attempt to change the number of dimensions from ~D to ~D."
	          OLD-NDIMS NDIMS))
  (OR (= INDIRECT-LENGTH OLD-INDIRECT-LENGTH)
      (FERROR NIL "Attempt to add or remove index-offset."))
  (%P-DPB-OFFSET TYPE %%ARRAY-TYPE-FIELD ARRAY 0)
  (AND ARRAY-INDEX-ORDER
       (CONSP DIMLIST)
       (SETQ DIMLIST (REVERSE DIMLIST)))
  (AND (CONSP DIMLIST)
       (DO ((I 1 (1+ I))
	    (N NDIMS (1- N)))
	   ((< N 2))
	 (%P-STORE-CONTENTS-OFFSET (EVAL (CAR DIMLIST)) ARRAY I)
	 (SETQ DIMLIST (CDR DIMLIST))))
  (%P-STORE-CONTENTS-OFFSET DISPLACED-P ARRAY NDIMS)
  (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ NDIMS))
  (WHEN INDEX-OFFSET
    (%P-STORE-CONTENTS-OFFSET INDEX-OFFSET ARRAY (+ NDIMS 2)))
  ARRAY)

))

; From file READ.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>READ.."

(DEFUN XR-XRTYI (STREAM &OPTIONAL IGNORE-WHITESPACE NO-CHARS-SPECIAL NO-MULTIPLE-ESCAPES)
  "Read a character from STREAM, processing escapes (// and /) and multiple-escapes (/|).
IGNORE-WHITESPACE non-NIL means skip over whitespace characters.
NO-CHARS-SPECIAL means do not process escapes specially.
NO-MULTIPLE-ESCAPES means do not process multiple-escape characters specially.

The first value is the translated character.
The second is the index for looking in READ's FSM.
The third is the original, nontranslated character.

Has a kludge for *READ-BASE* > 10. where letters that should be digits
return the readtable code for EXTENDED-DIGIT rather than their own codes."
  (DECLARE (VALUES TRANSLATED-CHAR FSM-INDEX ACTUAL-CHAR))
  (PROG TOP (CH BITS CODE CH-CHAR)
	(SETQ XR-XRTYI-PREV-CHAR XR-XRTYI-LAST-CHAR)
     L

(DO-FOREVER
	  (SETQ CH (SEND STREAM (IF RUBOUT-HANDLER ':ANY-TYI ':TYI)))
	  (if (fixnump ch) (setf (char-font ch) 0))
	  (COND ((NULL CH)
		 (RETURN-FROM TOP CH (RDTBL-EOF-CODE *READTABLE*) CH))
		((CONSP CH)
		 (AND (EQ (CAR CH) ':ACTIVATION)
		      ;; Ignore activations except in top-level context.
		      (NOT IGNORE-WHITESPACE)
		      (NOT NO-CHARS-SPECIAL)
		      (NOT NO-MULTIPLE-ESCAPES)
		      (LET ((CH1 (CAR (RDTBL-WHITESPACE *READTABLE*))))
			(RETURN-FROM TOP
			  CH1 (RDTBL-CODE *READTABLE* CH1) CH))))
		((AND READ-DISCARD-FONT-CHANGES
		      (EQ CH #/))
		 (IF (EQ #/ (SEND STREAM ':TYI))
		     (RETURN)))
		((NOT (> CH RDTBL-ARRAY-SIZE))
		 (RETURN))))
	(SETQ CH-CHAR (LDB %%CH-CHAR CH))
	(SETQ BITS (RDTBL-BITS *READTABLE* CH-CHAR))
	(SETQ CODE (RDTBL-CODE *READTABLE* CH-CHAR))
	(COND ((AND (NOT NO-CHARS-SPECIAL)
		    (NOT NO-MULTIPLE-ESCAPES)
		    (= CODE
		       (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*)))
	       ;; Vertical bar.
	       (SETQ READ-INSIDE-MULTIPLE-ESCAPE
		     (IF READ-INSIDE-MULTIPLE-ESCAPE NIL
		       CH-CHAR))
	       (GO L))
	      ((AND (NOT NO-CHARS-SPECIAL)
		    (= CODE
		       (RDTBL-ESCAPE-CODE *READTABLE*)))
	       ;; Slash
	       (SETQ XR-XRTYI-PREV-CHAR CH)
	       (DO-FOREVER
		 (SETQ CH (SEND STREAM ':TYI))
		 (COND ((AND READ-DISCARD-FONT-CHANGES
			     (EQ CH #/))
			(IF (EQ #/ (SEND STREAM ':TYI))
			    (RETURN)))
		       (T (RETURN))))
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (OR CH
			   (PROGN
			     (CERROR ':NO-ACTION NIL 'SYS:READ-END-OF-FILE
				     "EOF on ~S after a ~S." STREAM
				     (STRING XR-XRTYI-PREV-CHAR))
			     #/SPACE))
		       (RDTBL-SLASH-CODE *READTABLE*)
		       CH))
	      ((AND (NOT NO-CHARS-SPECIAL)
		    (= CODE
		       (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*)))
	       ;; circlecross
	       (SETQ XR-XRTYI-LAST-CHAR (XR-READ-CIRCLECROSS STREAM))
	       (RETURN XR-XRTYI-LAST-CHAR
		       (RDTBL-SLASH-CODE *READTABLE*)
		       XR-XRTYI-LAST-CHAR))
	      (READ-INSIDE-MULTIPLE-ESCAPE
	       ;; Ordinary character but within vertical bars.
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (OR CH
			   (PROGN
			     (CERROR ':NO-ACTION NIL 'SYS:READ-END-OF-FILE
				     "EOF on ~S inside a ~C-quoted token." STREAM
				     READ-INSIDE-MULTIPLE-ESCAPE)
			     #/SPACE))
		       (RDTBL-SLASH-CODE *READTABLE*)
		       CH))
	      (T
	       ;; Ordinary character.
	       (COND ((AND IGNORE-WHITESPACE
			   (BIT-TEST 1 BITS))
		      ;; Here if whitespace char to be ignored.
		      (SETQ XR-XRTYI-PREV-CHAR CH)
		      (GO L)))
	       ;; Here for ordinary, significant input char.
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (RDTBL-TRANS *READTABLE* CH-CHAR)
		       ;; If not doing slashes, caller must not really want the RDTBL-CODE,
		       ;; so return a value which, if passed to XR-XRUNTYI,
		       ;; will prevent barfing.
		       (IF NO-CHARS-SPECIAL 0
			 (IF (AND (NUMBERP *READ-BASE*)
				  ( #/A (CHAR-UPCASE CH) (+ *READ-BASE* #/A -11.)))
			     (CDR (GETF (RDTBL-PLIST *READTABLE*) 'EXTENDED-DIGIT))
			   (RDTBL-CODE *READTABLE* CH-CHAR)))
		       CH)))))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMACRO DEFSIGNAL-EXPLICIT (SIGNAL-NAME FLAVOR &OPTIONAL ARGS &BODY INIT-OPTIONS)
  "Define a signal name, which can be used in ERROR, SIGNAL, FERROR, CERROR.
SIGNAL-NAME is the signal name to be defined.
FLAVOR is the flavor of condition object to make,
 or a list (FLAVOR . ADDITIONAL-CONDITION-NAMES), where ADDITIONAL-CONDITION-NAMES
 is a list of condition-names to signal in addition to the flavor components.
 If you specify just a flavor name, the SIGNAL-NAME itself is used
 as the sole additional condition name.
 If you specify a one-element list (FLAVOR), there are no additional
 condition names, and you can optionally use :CONDITION-NAMES as one of
 the INIT-OPTIONS to control them dynamically.
DOCUMENTATION is a documentation string describing what this signal-name is for.
When SIGNAL-NAME is used as the first arg to MAKE-CONDITION, ERROR, SIGNAL, etc.
then you can refer to the remaining args with the arglist ARGS.
The argument names in that list may be used in the INIT-OPTIONS,
which are arguments to pass to MAKE-INSTANCE in addition to the flavor name."
  (declare (arglist signal-name flavor &optional args documentation
		    &body init-options))
  (LET ((FLAVOR (IF (CONSP FLAVOR) (CAR FLAVOR) FLAVOR))
	(documentation (if (stringp (car init-options)) (pop init-options)))
	(CONDITION-NAMES
	  (IF (CONSP FLAVOR) (CDR FLAVOR) (LIST SIGNAL-NAME))))
    `(PROGN (SI:RECORD-SOURCE-FILE-NAME ',SIGNAL-NAME 'DEFSIGNAL)
	    (DEFUN (:PROPERTY ,SIGNAL-NAME MAKE-CONDITION-FUNCTION) (IGNORE . ,ARGS)
	      ,DOCUMENTATION
	      (DECLARE (SI:FUNCTION-PARENT ,SIGNAL-NAME))
	      ,(IF CONDITION-NAMES
		   `(MAKE-INSTANCE ',FLAVOR ,@INIT-OPTIONS
				   ':CONDITION-NAMES ',CONDITION-NAMES)
		 `(MAKE-INSTANCE ',FLAVOR ,@INIT-OPTIONS))))))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMACRO DEFSIGNAL (SIGNAL-NAME FLAVOR ARGS &BODY INIT-OPTIONS)
  "Define a signal name, which can be used in ERROR, SIGNAL, FERROR, CERROR.
SIGNAL-NAME is the signal name to be defined.
FLAVOR is the flavor of condition object to make,
 or a list (FLAVOR . ADDITIONAL-CONDITION-NAMES), where ADDITIONAL-CONDITION-NAMES
 is a list of condition-names to signal in addition to the flavor components.
 If you specify just a flavor name, the SIGNAL-NAME itself is used
 as the sole additional condition name.
DOCUMENTATION is a documentation string describing what this signal-name is for.
When SIGNAL-NAME is used as the first arg to MAKE-CONDITION, ERROR, SIGNAL, etc.
then the remaining arguments are treated as a format string and format arguments.
In addition, the first few (not counting the string) can be given names,
which you specify as the list ARGS.
These names (moved into the keyword package) become messages
that can be sent to the condition object to get the corresponding values.
In addition, the values of the ARGS may be used in the INIT-OPTIONS,
which are additional arguments to pass to MAKE-INSTANCE."
  (declare arglist signal-name flavor args &optional documentation &body init-options)
  (LET ((FLAVOR (IF (CONSP FLAVOR) (CAR FLAVOR) FLAVOR))
	(documentation (if (stringp (car init-options)) (pop init-options)))
	(CONDITION-NAMES
	  (IF (CONSP FLAVOR) (CDR FLAVOR) (LIST SIGNAL-NAME)))
	(PROPERTIES (MAPCAR '(LAMBDA (SYM) `',(INTERN (GET-PNAME SYM) SI:PKG-KEYWORD-PACKAGE))
			    ARGS)))
    `(PROGN (SI:RECORD-SOURCE-FILE-NAME ',SIGNAL-NAME 'DEFSIGNAL)
	    (DEFUN (:PROPERTY ,SIGNAL-NAME MAKE-CONDITION-FUNCTION)
		   (IGNORE FORMAT-STRING &OPTIONAL ,@ARGS &REST FORMAT-ARGS)
	      ,DOCUMENTATION
	      (DECLARE (FUNCTION-PARENT ,SIGNAL-NAME))
	      (MAKE-INSTANCE ',FLAVOR
			     ,@INIT-OPTIONS
			     ':PROPERTY-LIST
			     (LIST . ,(MAPCAN 'LIST PROPERTIES ARGS))
			     ':FORMAT-STRING FORMAT-STRING
			     ':FORMAT-ARGS
			     (LIST* ,@ARGS (COPY-LIST FORMAT-ARGS))
			     ':CONDITION-NAMES ',CONDITION-NAMES)))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property real type-name-function) (type)
  (float-type-name-function "real number" type))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (real type-predicate) (object &optional low high)
  (and (realp object)
       (cond ((memq low '(nil *)) t)
	     ((numberp low) ( low object))
	     ((consp low) (< (car low) object))
	     (t (ferror nil "Invalid lower limit in REAL type specifier.")))
       (cond ((memq high '(nil *)) t)
	     ((numberp high) ( high object))
	     ((consp high) (> (car high) object))
	     (t (ferror nil "Invalid upper limit in REAL type specifier.")))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (real type-optimizer) (expression &optional low high)
  (optimize-numeric-type-test 'realp expression low high))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defvar subtypep-pairwise-disjoint-sets
  '((integer fixnum bignum)
    (rational ratio integer)
    (number rational float complex)
    (number real complex)
    (list cons null)
    (t cons symbol array number character entity locative instance closure
       stack-group select compiled-function microcode-function)
    (t list number hash-table readtable package pathname stream random-state)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property complex type-name-function) (type)
  (selectq (cadr type)
    ((nil real) "a complex number")
    (rational "a rational complex number")
    (short-float "a complex number with short-float components")
    (single-float "a complex number with single-float components")
    (long-float "a complex number with long-float components")
    (double-float "a complex number with double-float components")
    (float "a complex number with floating-point components")
    (t nil)))

))


; From file DEFSEL.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFSEL  "

(defun (:property :select-method function-spec-handler)
       (function function-spec &optional arg1 arg2)
  (let ((select-method-function-spec (second function-spec))
	(message (third function-spec))
	select-method-alist elem fn new-p)
    (if (not (and (= (length function-spec) 3)
		  (validate-function-spec select-method-function-spec)))
	(if (eq function 'validate-function-spec)
	    nil
	  (ferror 'sys:invalid-function-spec
		  "The function spec ~S is invalid." function-spec))
      (selectq function
	(validate-function-spec t)
	(function-parent (values (cadr function-spec) 'defun))
	(t (unless (or (not (memq function '(fdefine fdefinition fdefinition-location
						     fundefine fdefinedp)))
		       (and (fdefinedp select-method-function-spec)
			    (typep (setq fn (fdefinition select-method-function-spec))
				   'select-method)))
	     (ferror 'sys:invalid-function-spec
		     "The function spec ~S is invalid;~%~S is not a DEFSELECT."
		     function-spec select-method-function-spec))
	   (if fn (setq elem (assq-careful message
				   (setq select-method-alist (%make-pointer dtp-list fn)))))
	   (when (and (null elem) (memq function '(fdefine fdefinition-location)))
	     ;; cons up a select-method
	     (setq elem (cons message nil) new-p t)
	     (fdefine select-method-function-spec
		      (%make-pointer dtp-select-method (cons elem select-method-alist)))
	     (let ((closure (cdr (assq-careful ':which-operations select-method-alist))))
	       (when (closurep closure)
		 (pushnew message (symeval-in-closure closure '.defselect.which.operations.))
				  ':test 'eq)))
	   (selectq function
	     (fdefine (setf (cdr elem) arg1))
	     (fdefinition (cdr elem))
	     (fdefinition-location elem)
	     (fdefinedp (cdr elem))
	     (fundefine
	      (fdefine select-method-function-spec
		       (%make-pointer dtp-select-method (remq elem select-method-alist)))
	      (let* ((closure (cdr (assq-careful ':which-operations select-method-alist)))
		     loc)
		(when (closurep closure)
		  (setq loc (locate-in-closure closure '.defselect.which.operations.))
		  (setf (contents loc) (remq message (contents loc))))))
	     (t (function-spec-default-handler function function-spec arg1 arg2))))))))

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defmacro defpackage (name &body alist-of-options)
  "Defines (creates or alters) a package named NAME.
Each element of ALIST-OF-OPTIONS looks like (OPTION ARGS...)
Options are:
/(:NICKNAMES names...) specifies alternate names for this package.
/(:PREFIX-NAME name) specifies string to print for this package when printing prefixes.
/(:USE packages...) specifies packages for this one to inherit from.
/(:SHADOW names...) specifies names of symbols to shadow in this package.
/(:EXPORT names...) specifies names of symbols to export in this package.
/(:IMPORT symbols...) specifies symbols to import in this package.
/(:IMPORT-FROM package names...) specifies a package and names of symbols
 to import in this package from that package.
/(:SHADOWING-IMPORT symbols...) specifies symbols to import in this package,
 overriding any name conflicts.
/(:RELATIVE-NAMES (name package)...) specifies local nicknames for
 other packages to be in effect in this package.
/(:RELATIVE-NAMES-FOR-ME (package name)...) specifies local nicknames
 for this package to be in effect in other packages.
/(:AUTO-EXPORT-P t-or-nil) non-NIL specifies that all symbols placed in this package
 should be exported automatically at that time.
/(:SIZE int) specifies the number of symbols to allocate space for initially."
  `(let* ((pkg 
	    (apply (if (find-package ',name)
		       'alter-package 'make-package)
		   ',name
		   (loop for (keyword . args) in ',alist-of-options
			 nconc (list keyword (if (or (cdr args) (consp (car args)))
						 args
					       (car args))))))
	  (sym (intern ',name pkg-user-package)))
     (record-source-file-name sym 'defpackage)
     (setf (getf (pkg-plist pkg) ':source-file-name)	;kludge kluge
	   (cadr (assoc 'defpackage (get sym ':source-file-name))))))

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun alter-package (name &key &optional nicknames
		      (use '("GLOBAL")) ((:size ignore))
		      shadow export prefix-name auto-export-p
		      import shadowing-import import-from
		      relative-names relative-names-for-me
		      ((:hash-inherited-symbols ignore))
		      properties ;new-symbol-function
		      ((:include ignore)) ((:colon-mode ignore))
		      (external-only nil external-only-p))
  (setq auto-export-p (or auto-export-p external-only))	;brand s bd
  (let ((pkg (find-package name)))
    (unless pkg (ferror nil "Package ~A not found." name))
    (unless (cli:listp nicknames) (setq nicknames (list nicknames)))
    (rename-package pkg (pkg-name pkg) nicknames)
    (unless (or (null prefix-name) (string= prefix-name name)
		(mem 'string= prefix-name nicknames))
      (ferror nil "The prefix name ~A is not a name or nickname of the package." prefix-name))
    (setf (pkg-prefix-print-name pkg) prefix-name)
    (loop for (prop val) on properties by 'cddr
	  do (setf (getf (pkg-plist package) prop) val))
    (shadow shadow pkg)
    (shadowing-import shadowing-import pkg)
    (export export pkg)
    (let ((desired-use (if (cli:listp use)
			   (mapcar 'find-package use)
			 (list (find-package use)))))
      (dolist (elt (pkg-use-list pkg))
	(unless (memq elt desired-use)
	  (unuse-package elt pkg)))
      (use-package desired-use pkg))
    (import import pkg)
    (when import-from
      (dolist (name (cdr import-from))
	(import (intern (string name) (car import-from)) pkg)))
    (if (if external-only-p external-only auto-export-p)
	(pkg-mark-having-subpackages pkg)
      (setf (pkg-auto-export-p pkg) nil)
      (setf (pkg-store-function pkg) nil))
    (setf (pkg-refname-alist pkg)
	  (loop for (nick p) in relative-names
		collect (cons (string nick)
			      (find-package p))))
    ;; First delete any other local nicknames, in any package, for this one.
    (dolist (p *all-packages*)
      (setf (pkg-refname-alist p)
	    (cli:delete pkg (pkg-refname-alist p) ':key 'cdr)))
    ;; Then add the ones that are requested.
    (dolist (elt relative-names-for-me)
      (pkg-add-relative-name (car elt) (cadr elt) pkg))
    pkg))

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun import (symbols &optional (pkg *package*) &aux tem)
  "Makes SYMBOLS be present in package PKG.
They then belong to PKG directly, rather than by inheritance.
Error if this produces a name conflict
/(a distinct symbol with the same name is already available in PKG)."
  (setq pkg (pkg-find-package pkg))
  (dolist (sym (if (consp symbols) symbols (list symbols)))
    ;; using consp rather than cli:listp makes (import 'nil "FOO") work
    (unless (symbolp sym)
      (ferror nil "The argument ~S to IMPORT was not a symbol" sym))
    (when (and (setq tem (intern-local-soft sym pkg))
	       (neq tem sym))
      (cerror ':no-action nil nil
	      "A symbol named ~S already exists in package ~A."
	      (get-pname sym) pkg)
      (when (fquery nil "Discard the old symbol and import the new one?")
	(unintern tem pkg)))
    (intern sym pkg))				;Can be called before INTERN is redefined.
  t)

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST CAAR-SAFE (OBJECT)
  "Like CAAR, but treats all non-lists as NIL rather than getting an error."
  (CAR-SAFE (CAR-SAFE OBJECT)))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST CDAR-SAFE (OBJECT)
  "Like CDAR, but treats all non-lists as NIL rather than getting an error."
  (CDR-SAFE (CAR-SAFE OBJECT)))

))

; From file SCREEN.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZMACS-FRAME :BEFORE :INIT) (IGNORE)
  (SETQ STANDARD-INPUT-FOR-PANES (MAKE-MACRO-STREAM SELF))
  (LET ((*STANDARD-INPUT* STANDARD-INPUT-FOR-PANES)
	(*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
	(*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
	(*COMTAB* *ZMACS-COMTAB*)
	(*MODE-LINE-LIST* '("ZMACS " "(" *MODE-NAME-LIST*
			    ") " *BUFFER-MODIFIED-P* *MORE-ABOVE-BELOW*
			    *ZMACS-BUFFER-NAME* *ZMACS-BUFFER-VERSION-STRING*
			    (*FONT-NAME* "  Font: " *FONT-NAME*)
			    (*MACRO-LEVEL* "  Macro-level: " *MACRO-LEVEL*))))
    (SETQ EDITOR-CLOSURE
	  (MAKE-EDITOR-CLOSURE ZMACS-TOP-LEVEL-EDITOR-CLOSURE-VARIABLES NIL))))

(DEFUN (*MORE-ABOVE-BELOW* MODE-LINE-RECALCULATE) ()
  (LET ((ABOVE (NOT (BP-= (WINDOW-START-BP *WINDOW*)
			  (INTERVAL-FIRST-BP (WINDOW-INTERVAL *WINDOW*)))))
	(BELOW (PLINE-LINE *WINDOW*
			   (1- (WINDOW-N-PLINES *WINDOW*)))))
    (SETQ *MORE-ABOVE-BELOW*
	  (IF ABOVE (IF BELOW " " " ")
	    (IF BELOW " " NIL)))))

))

; From file SCREEN.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZMACS-FRAME :AFTER :INIT) (IGNORE)
  (SETQ TV:PROCESS (MAKE-PROCESS TV:NAME
				 ':FLAVOR 'SI:COROUTINING-PROCESS
				 ':INITIAL-FORM `(ZMACS-WINDOW-TOP-LEVEL ,SELF)
				 ':REGULAR-PDL-SIZE #o10000
				 ':SPECIAL-PDL-SIZE #o10000))
  (PROCESS-RESET TV:PROCESS)
  (SEND SELF ':SELECT-PANE
	(SEND SELF ':CREATE-WINDOW 'ZMACS-WINDOW-PANE
		   		   ':ACTIVATE-P T ':LABEL NIL)))

(DEFUN ZMACS-MODE-LINE-RECALCULATE-FUNCTION (&AUX INT-TICK)
  (SETQ INT-TICK (NODE-TICK *INTERVAL*))
  (SETQ *BUFFER-MODIFIED-P* (COND ((BUFFER-READ-ONLY-P *INTERVAL*) "(RO) ")
				  ((BUFFER-MODIFIED-P *INTERVAL*) "* ")
				  (T NIL))))
))

; From file SCREEN.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZWEI :MOUSE-CLICK) (BUTTON X Y &AUX HANDLED-P)
  (SETQ BUTTON (TV:MERGE-SHIFT-KEYS BUTTON))
  (COND ((NOT (SEND (SEND SELF ':TOP-OF-EDITOR-HIERARCHY)
		    ':SELF-OR-SUBSTITUTE-SELECTED-P))
	 ;; This frame or whatever is not selected.
	 (TV:MOUSE-SELECT SELF))
	((AND (NOT (EDITOR-WINDOW-SELECTED-P SELF))
	      (OR (= BUTTON #/MOUSE-1-1)
		  *MOUSE-CLICK-ALWAYS-SELECTS*))
	 ;; Frame selected but this editor window is not.  Just switch to it.
	 (COMMAND-BUFFER-PUSH `(SELECT-WINDOW ,SELF))
	 (IF *MOUSE-CLICK-ALWAYS-SELECTS*
	     ;; And maybe also do the command for the mouse button.
	     (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y)))
	 (SETQ HANDLED-P T))
	(T
	 (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y))))
  T)

))

zwei:(push '(locally 1 1) *lisp-indent-offset-alist*)
zwei:(push '(multiple-value-setq 1 1) *lisp-indent-offset-alist*)
zwei:(push '(nth-value 1 1) *lisp-indent-offset-alist*)

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN DEFMACRO-COPY-INDENTATION-FOR-ZWEI (NAME NAME1)
  (LET ((VARIABLE (IF (BOUNDP 'ZWEI:*LISP-INDENT-OFFSET-ALIST*)
		      'ZWEI:*LISP-INDENT-OFFSET-ALIST*
		    'ZWEI:*INITIAL-LISP-INDENT-OFFSET-ALIST*)))
    (LET ((X (ASSQ NAME (SYMEVAL VARIABLE)))
	  (Y (ASSQ NAME1 (SYMEVAL VARIABLE))))
      (IF Y
	  (IF (NULL X)
	      (PUSH (CONS NAME (CDR Y)) (SYMEVAL VARIABLE))
	    (SETF (CDR X) (CDR Y)))))))

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN DEFF-MACRO (&QUOTE FUNCTION &EVAL DEFINITION)
  "Define FUNCTION with definition DEFINITION, which should be a subst or macro.
If found in a file being compiled, this definition will be in effect
during compilation as well as when the compiled file is loaded.
That is how DEFF-MACRO differs from DEFF."
  (AND UNDO-DECLARATIONS-FLAG
       (COMPILER:FUNCTION-REFERENCED-P FUNCTION)
       (COMPILER:WARN 'MACRO-USED-BEFORE-DEFINED ':IMPOSSIBLE
		      "The macro ~S was used before it was defined" FUNCTION))
  ;; Put macro definition where it belongs (don't really define it if compiling)
  (COND ((AND (BOUNDP 'UNDO-DECLARATIONS-FLAG) UNDO-DECLARATIONS-FLAG)
	 (WHEN (EQ (CAR-SAFE FUNCTION) ':PROPERTY)
	   (PUTDECL (CADR FUNCTION) (CADDR FUNCTION) DEFINITION))
	 (PUSH `(DEF ,FUNCTION . ,DEFINITION) FILE-LOCAL-DECLARATIONS))
	(T
	 (FDEFINE FUNCTION DEFINITION T)
	 (IF (SYMBOLP DEFINITION)
	     (DEFMACRO-COPY-INDENTATION-FOR-ZWEI FUNCTION DEFINITION))))
  FUNCTION)

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFF-MACRO CASE 'SELECTQ)
(DEFF-MACRO CASEQ 'SELECTQ)

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN CHECK-NUMBER-OF-ARGS (FORM &OPTIONAL FUNCTION)
  (IF (NULL FUNCTION) (SETQ FUNCTION (CAR FORM)))
  (LET* (TEM
	 ARGLIST
	 NARGS
	 (MIN NIL)
	 (MAX 0)
	 (ARGS-INFO NIL)
	 (LOCALP NIL)
	 (FN FUNCTION))
    (AND (SYMBOLP FN)
	 ;; If FN is a name defined lexically by FLET or LABELS, use its definition.
	 (SETQ LOCALP (ASSQ FN LOCAL-FUNCTIONS))
	 (SETQ FN (CADDR LOCALP)))
    (FLET ((BAD-ARGUMENTS (MSG &OPTIONAL (TYPE 'WRONG-NUMBER-OF-ARGUMENTS)
			       		 (SEVERITY ':PROBABLE-ERROR))
	      (WARN TYPE SEVERITY (IF LOCALP
				      "Locally defined function ~S called with ~A"
				    "Function ~S called with ~A")
		    (CAR FORM) MSG)))
      (TAGBODY
       TOP
	  (SETQ FN (LAMBDA-MACRO-EXPAND FN))
	  (SETQ ARGLIST (IGNORE-ERRORS (LET ((TEM (ARGLIST FN T)))
					 (IF (EQ TEM 'MACRO) TEM (ARGLIST FN 'NIL)))))
	  (COND ((AND (CONSP FN) (FUNCTIONP FN T))
		 (UNLESS (CONSP ARGLIST) (RETURN-FROM CHECK-NUMBER-OF-ARGS))
		 (DOLIST (X ARGLIST)
		   (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
			 ((OR (EQ X '&REST) (EQ X '&BODY) (EQ X '&KEY))
			  (UNLESS MIN (SETQ MIN MAX))
			  (SETQ MAX MOST-POSITIVE-FIXNUM)
			  (RETURN))
			 ((EQ X '&AUX) (RETURN))
			 ((MEMQ X LAMBDA-LIST-KEYWORDS))
			 (T (INCF MAX)))))
		((NOT (SYMBOLP FN))
		 ;;Unknown type, don't check
		 (RETURN-FROM CHECK-NUMBER-OF-ARGS))
		((SETQ TEM (GET FN 'ARGDESC))
		 (DOLIST (X TEM)
		   (COND ((MEMQ 'FEF-ARG-REQ (CADR X))
			  (INCF MAX (CAR X)))
			 ((MEMQ 'FEF-ARG-OPT (CADR X))
			  (OR MIN (SETQ MIN MAX))
			  (INCF MAX (CAR X)))
			 ((MEMQ 'FEF-ARG-REST (CADR X))
			  (OR MIN (SETQ MIN MAX))
			  (SETQ MAX MOST-POSITIVE-FIXNUM)))))
		((SETQ TEM (GET FN 'QINTCMP))
		 (SETQ MAX TEM))
		((SETQ TEM (GET FN 'Q-ARGS-PROP))
		 (SETQ ARGS-INFO TEM))
		;; Take care of recursive calls to function being compiled.
		((EQ FN THIS-FUNCTION-ARGLIST-FUNCTION-NAME)
		 (DOLIST (X THIS-FUNCTION-ARGLIST)
		   (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
			 ((OR (EQ X '&REST) (EQ X '&BODY) (EQ X '&KEY))
			  (UNLESS MIN (SETQ MIN MAX))
			  (SETQ MAX MOST-POSITIVE-FIXNUM)
			  (RETURN))
			 ((EQ X '&AUX) (RETURN))
			 ((MEMQ X LAMBDA-LIST-KEYWORDS))
			 (T (INCF MAX)))))
		((FBOUNDP FN)
		 (SETQ TEM (SI:UNENCAPSULATE-FUNCTION-SPEC FN))
		 (COND ((NOT (EQ TEM FN))
			(SETQ FN TEM)
			(GO TOP)))
		 (SETQ TEM (FSYMEVAL FN))
		 (COND ((OR (SYMBOLP TEM) (CONSP TEM))
			(SETQ FN TEM)
			(GO TOP))
		       (T (SETQ ARGS-INFO (%ARGS-INFO TEM)))))
		(T ;;No information available
		 (RETURN-FROM CHECK-NUMBER-OF-ARGS))))
      (WHEN ARGS-INFO
	(SETQ MIN (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
	      MAX (IF (BIT-TEST (LOGIOR %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST)
				ARGS-INFO)
		      MOST-POSITIVE-FIXNUM
		    (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
      (SETQ NARGS (LENGTH (CDR FORM)))	;Now that we know it's not a macro
      (COND ((< NARGS (OR MIN MAX))
	     (BAD-ARGUMENTS "too few arguments."))
	    ((> NARGS MAX)
	     (BAD-ARGUMENTS "too many arguments."))
	    ((CONSP ARGLIST)
	     (LET* ((KEYARGS (MEMQ '&KEY ARGLIST))
		    (KEYFORM (NTHCDR (OR MAX MIN) (CDR FORM))))
	       (WHEN (AND KEYARGS KEYFORM)
		 (IF (ODDP (LENGTH KEYFORM))
		     (BAD-ARGUMENTS "no value supplied for some keyword argument.")
		   (LET ((ALLOW-OTHER-KEYS (OR (MEMQ '&ALLOW-OTHER-KEYS ARGLIST)
					       (GETF KEYFORM ':ALLOW-OTHER-KEYS))))
		     (LOOP FOR KEY IN KEYFORM BY #'CDDR
			   WHEN (EQ (CAR-SAFE KEY) 'QUOTE) DO (SETQ KEY (CADR KEY))
			   DOING (COND ((KEYWORDP KEY)
					(UNLESS
					  (OR ALLOW-OTHER-KEYS
					      (DOLIST (X KEYARGS)
						(IF (MEMQ X LAMBDA-LIST-KEYWORDS)
						    NIL
						  (IF 
						    (IF (CONSP X)
							(IF (CONSP (CAR X))
							    ;; ((:frob foo) bar)
							    (EQ KEY (CAAR X))
							  ;; (foo bar)
							  (STRING= KEY (CAR X)))
						      ;; foo
						      (STRING= KEY X))
						    (RETURN T)))))
					  (BAD-ARGUMENTS
					    (FORMAT NIL "the unrecognized keyword ~S"
						    KEY))))
				       ((CONSTANTP KEY)
					(BAD-ARGUMENTS
					  (FORMAT NIL "~S appearing where a keyword should" KEY))))))))))))))

))

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun trivial-form-p (form)
  "T if what FORM evaluates to is inherent in its appearance."
  (cond ((symbolp form)
	 (or (eq form 't) (null form)))
	((keywordp form))
	((eq (car-safe form) 'quote) t)
	((numberp form) t)
	((stringp form) t)))

))

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun (:character prompt-and-read-no-rubout-function) (option stream)
  (block char
    (prompt-and-read-prompt-function stream nil)
    (let ((char (send stream ':tyi))
	  (*standard-output* stream))
      (when (and (consp option) (get option ':or-nil))
	(cond ((memq char '(#/quote #/c-q))
	       (setq char (send stream ':tyi)))
	      ((= char #/Clear-input)
	       (princ "none")
	       (return-from char nil))))
      (format:ochar char ':editor)
      char)))

))

; From file DLEDIT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DLEDIT  "

(DEFUN LE-COM-CONTROL-E ()
  (SETQ LE-SOMETHING-CHANGED T)			;something probably will...
  (IF (< LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
      (LET ((ITEM (NTH LE-ITEM-NUMBER LE-STRUCTURE)))
	(LET ((NAME (FIRST ITEM))
	      (VALUE (SECOND ITEM))
	      (*READ-BASE* 10.))
	  (WITH-INPUT-EDITING (T `((:INITIAL-INPUT ,(FORMAT NIL "~D" VALUE))))
	    (SETQ VALUE (PROMPT-AND-READ (IF (NUMBERP VALUE) ':INTEGER ':STRING)
					 "Change the ~A from to:" NAME)))
	  ;; Avoid lossage in lowercase partition names.
	  (COND ((MEMQ NAME '(PARTITION-NEME CURRENT-BAND CURRENT-MICROLOAD))
		 (SETQ VALUE (STRING-UPCASE VALUE))))
	  (SELECTQ NAME
	    (PACK-NAME (PUT-DISK-STRING LE-RQB VALUE #o20 32.))
	    (DRIVE-NAME (PUT-DISK-STRING LE-RQB VALUE #o10 32.))
	    (COMMENT (PUT-DISK-STRING LE-RQB VALUE #o30 96.))
	    (N-CYLINDERS (PUT-DISK-FIXNUM LE-RQB VALUE 2))
	    (N-HEADS (PUT-DISK-FIXNUM LE-RQB VALUE 3)
		     (PUT-DISK-FIXNUM LE-RQB (* VALUE (GET-DISK-FIXNUM LE-RQB 4)) 5))
	    (N-BLOCKS-PER-TRACK (PUT-DISK-FIXNUM LE-RQB VALUE 4)
				(PUT-DISK-FIXNUM LE-RQB (* VALUE (GET-DISK-FIXNUM LE-RQB 3)) 5))
	    (CURRENT-MICROLOAD (PUT-DISK-STRING LE-RQB VALUE 6 4))
	    (CURRENT-BAND (PUT-DISK-STRING LE-RQB VALUE 7 4))
	    (N-PARTITIONS (PUT-DISK-FIXNUM LE-RQB VALUE #o200))
	    (WORDS-PER-PART (CHANGE-PARTITION-MAP LE-RQB VALUE))
	    ;; These occur in multiple instances; hair is required
	    ((PARTITION-NAME PARTITION-START PARTITION-SIZE PARTITION-COMMENT)
	     (LET ((PLOC (LE-CURRENT-PARTITION)))
	       (SELECTQ NAME
		 (PARTITION-NAME (PUT-DISK-STRING LE-RQB VALUE PLOC 4))
		 (PARTITION-START (PUT-DISK-FIXNUM LE-RQB VALUE (1+ PLOC)))
		 (PARTITION-SIZE (PUT-DISK-FIXNUM LE-RQB VALUE (+ PLOC 2)))
		 (PARTITION-COMMENT
		  (PUT-DISK-STRING LE-RQB VALUE (+ PLOC 3)
				   (* 4 (- (GET-DISK-FIXNUM LE-RQB #o201) 3)))))))
	    (OTHERWISE (FERROR NIL "No editor for ~S" NAME)))))
    (BEEP))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

))

; From file DLEDIT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DLEDIT  "

(DEFUN LE-COM-CONTROL-W ()
  (COND ((Y-OR-N-P "Do you want to write out this label? ")
	 (WRITE-DISK-LABEL LE-RQB LE-UNIT)
	 (SETQ LE-SOMETHING-CHANGED NIL)
	 (FORMAT T "~&Written.~%Type  to exit the disk-label editor."))
	(T
	 (FORMAT T "~&Not written.~%"))))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN CONDITION-TYPEP-1 (NAMES FROB)
  (IF (ATOM FROB) (MEMQ FROB NAMES)
    (SELECTQ (CAR FROB)
      (AND (DOLIST (C (CDR FROB) T)
	     (WHEN (NOT (CONDITION-TYPEP-1 NAMES C)) (RETURN NIL))))
      ((OR MEMBER) (DOLIST (C (CDR FROB) NIL)
		     (WHEN (CONDITION-TYPEP-1 NAMES C) (RETURN T))))
      (NOT (NOT (CONDITION-TYPEP-1 NAMES (CADR FROB)))))))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN CONDITION-TYPEP (CONDITION-INSTANCE CONDITION-NAME)
  "T if CONDITION-NAME is one of the condition names possessed by CONDITION-INSTANCE."
  (CONDITION-TYPEP-1 (SEND CONDITION-INSTANCE ':CONDITION-NAMES) CONDITION-NAME))

))

; From file PROCES.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(DEFUN MAKE-PROCESS (NAME &REST INIT-ARGS)
  "Create a process, with name NAME.
:FLAVOR specifies the flavor of process to make.
:SIMPLE-P if non-NIL specifies flavor SI:SIMPLE-PROCESS.
If :FLAVOR and :SIMPLE-P are NIL, the flavor SI:PROCESS is used.
:WARM-BOOT-ACTION is a function to call on warm booting,
 or :FLUSH meaning flush the process.  The default is to restart it.
 SI:PROCESS-WARM-BOOT-RESET kills the process.
 SI:PROCESS-WARM-BOOT-RESTART restarts at an earlier stage of booting.
:QUANTUM is in 60'ths and defaults to one second.
:PRIORITY defaults to 0; larger numbers run more often.
:STACK-GROUP specifies the stack group for this process to run in.
If that is omitted, the keyword arguments :SG-AREA,
:REGULAR-PDL-AREA, :SPECIAL-PDL-AREA, :REGULAR-PDL-SIZE,
and :SPECIAL-PDL-SIZE are passed on to MAKE-STACK-GROUP."
  (DECLARE (ARGLIST NAME &KEY SIMPLE-P FLAVOR STACK-GROUP WARM-BOOT-ACTION QUANTUM PRIORITY
		    	      SG-AREA REGULAR-PDL-AREA SPECIAL-PDL-AREA
			      REGULAR-PDL-SIZE SPECIAL-PDL-SIZE
			      &ALLOW-OTHER-KEYS))
  (OR (CAR INIT-ARGS) (SETQ INIT-ARGS (CDR INIT-ARGS)))	;For backward compatibility
  (SETQ INIT-ARGS (LIST* ':NAME NAME INIT-ARGS))
  (INSTANTIATE-FLAVOR (OR (GETF INIT-ARGS ':FLAVOR)
			  (AND (GETF INIT-ARGS ':SIMPLE-P) 'SIMPLE-PROCESS)
			  'PROCESS)
		      (LOCF INIT-ARGS)
		      T))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
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
			      (LET ((*PACKAGE* NIL))
				(CONS (FORMAT NIL "~S" FLAVOR-NAME) FLAVOR-NAME)))
	   ;; Array is no longer sorted.
	   (STORE-ARRAY-LEADER NIL *ALL-FLAVOR-NAMES-AARRAY* 1))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
	(FERROR NIL "~:S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
	(FERROR NIL "~:S, which is not a symbol, was specified as a component flavor" CF)))
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
       ;;Don't validate.  User may reasonably want to specify non-local instance
       ;;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
       ;;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
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
  ;; If the flavor is being redefined, and the number or order of instance$variables
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
    (SETF (FLAVOR-PACKAGE FL) *PACKAGE*)
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
	    (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR-NAME)))
      (flavor-hack-documentation flavor-name))
    FLAVOR-NAME))

))

; From file CHOICE.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; CHOICE  "

(DEFUN CHOOSE-VARIABLE-VALUES (VARIABLES &KEY &OPTIONAL FUNCTION
			       (NEAR-MODE '(:MOUSE)) (LABEL "Choose Variable Values")
			       WIDTH (EXTRA-WIDTH 10.) MARGIN-CHOICES SUPERIOR
			       REVERSE-VIDEO-P
			       &AUX OSW)
  "Invoke a temporary Choose-variable-values window to choose VARIABLES.
VARIABLES is a list of elements, each describing one line of the display
 These become text-scroll items.  Kinds of elements allowed are:
  string - just displayed
  symbol - value is printed, and if the user clicks on it
	with the mouse a new value is read.
  locative - like special-variable but value is accessed by car and written by rplaca
  list - (VAR LABEL TYPE ARGS...).  VAR is the variable (symbol or locative),
	LABEL if not NIL is a string to print instead of VAR's name,
	TYPE is a keyword saying what kinds of values are allowed (default :SEXP),
	and ARGS are args used by the TYPE's parsing functions.
Keyword args are:
:LABEL  Window label (default is /"Choose Variable Values/")
:FUNCTION  Function called if user changes anything (default is NIL)
:NEAR-MODE  Where to appear the window (default is (:MOUSE))
:WIDTH  Desired width of window.  Default is to set wide enough for items.
:EXTRA-WIDTH  Amount of extra width to allow for growing items.  Default 10 characters.
   Each of the above widths may be a number of characters or a string.
:MARGIN-CHOICES  List of elements.  A string is the label for the
   box which means /"exit/" (Default is /"Exit/"), cons of
   a string and a form means eval that form if box clicked upon.
:SUPERIOR  Window to put under, default is MOUSE-SHEET or the superior
   of the window it is supposed to be near, like MENU-CHOOSE.
:REVERSE-VIDEO-P  T means display this window reverse-video."
  ;; Decide what superior to use
  (OR SUPERIOR
      (SETQ SUPERIOR (IF (EQ (CAR NEAR-MODE) ':WINDOW) (SHEET-SUPERIOR (CADR NEAR-MODE))
		       MOUSE-SHEET)))
  ;; MARGIN-CHOICES must always contain a "exit" box so user can stop choosing
  (DO ((L MARGIN-CHOICES (CDR L)))
      ((NULL L) (PUSH "Exit" MARGIN-CHOICES))
    (COND ((STRINGP (CAR L)) (RETURN))
	  ((OR (ATOM (CAR L)) (NOT (STRINGP (CAAR L))))
	   (FERROR NIL "~S garbage in MARGIN-CHOICES" (CAR L)))))
  (SETQ MARGIN-CHOICES
	(MAPCAR #'(LAMBDA (X) (LIST (IF (ATOM X) X (CAR X))
				    NIL 'CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER NIL NIL
				    (IF (ATOM X) NIL (CADR X))))
		MARGIN-CHOICES))
  (DOLIST (ELEM VARIABLES)  ;Make sure all variables are bound, while in caller's environment
    (IF (LISTP ELEM)
	(SETQ ELEM (CAR ELEM)))
    (COND ((SYMBOLP ELEM) (SYMEVAL ELEM))
	  ((TYPEP ELEM 'LOCATIVE) (SETQ ELEM (CAR ELEM)))
	  ((STRINGP ELEM))
	  (T (FERROR NIL "~S is a ~S Bad data type for variable" ELEM (DATA-TYPE ELEM)))))
  (USING-RESOURCE (WINDOW TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW SUPERIOR)
    (SEND WINDOW ':SETUP VARIABLES LABEL FUNCTION MARGIN-CHOICES (OR WIDTH T) EXTRA-WIDTH)
    (SEND WINDOW ':SET-REVERSE-VIDEO-P REVERSE-VIDEO-P)
    (SETQ OSW SELECTED-WINDOW)
    (UNWIND-PROTECT
      (LET ((IOB (SEND WINDOW ':IO-BUFFER)))
	(IO-BUFFER-CLEAR IOB)
	(DELAYING-SCREEN-MANAGEMENT
	  (EXPOSE-WINDOW-NEAR WINDOW NEAR-MODE)
	  (SEND WINDOW ':SELECT)) ;For who-line
	(DO-FOREVER
	  (PROCESS-WAIT "Choose" #'(LAMBDA (IOB) (NOT (IO-BUFFER-EMPTY-P IOB))) IOB)
	  (AND (CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE WINDOW (SEND WINDOW ':ANY-TYI))
	       (RETURN))))
      (DELAYING-SCREEN-MANAGEMENT
        (SEND WINDOW ':DEACTIVATE)
	(AND OSW (SEND OSW ':SELECT NIL))))))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-expand-cons-macro (x)
  (let* ((description (get-defstruct-description (getdecl (car x) 'defstruct-name)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (slot-alist (defstruct-description-slot-alist))
	 (cons-keywords (defstruct-type-description-cons-keywords))
	 (kludge nil)
	 (constructor-description 
	   (cdr (or (assq (car x) (defstruct-description-constructors))
		    (defstruct-error
		      "This constructor is no longer defined for this structure"
		      (car x) 'in (defstruct-description-name)))))
	 (aux nil)
	 (aux-init nil))
     (if (null constructor-description)
	 (setq kludge (defstruct-parse-setq-style-slots (cdr x)
							slot-alist
							cons-keywords
							x))
       ;; can't do anything useful with doc strings using current scheme of defstruct
       ;; constructor macrology
       (IF (STRINGP (CAR (LAST CONSTRUCTOR-DESCRIPTION)))
	   (SETQ CONSTRUCTOR-DESCRIPTION (BUTLAST CONSTRUCTOR-DESCRIPTION)))
       (prog (args l)
	     (setq kludge (cons nil nil))
	     (setq args (cdr x))
	     (setq l (car constructor-description))
	  R  (cond ((null l)
		    (if (null args)
			(return nil)
		      (go barf-tma)))
		   ((atom l) (go barf))
		   ((eq (car l) '&optional) (go O))
		   ((eq (car l) '&rest) (go S))
		   ((eq (car l) '&aux) (go A))
		   ((null args) (go barf-tfa)))
	     (defstruct-make-init-dsc kludge
				      (pop l)
				      (pop args)
				      slot-alist
				      cons-keywords
				      x)
	     (go R)
	  O  (and (null args) (go OD))
	     (pop l)
	     (cond ((null l) (go barf-tma))
		   ((atom l) (go barf))
		   ((eq (car l) '&optional) (go barf))
		   ((eq (car l) '&rest) (go S))
		   ((eq (car l) '&aux) (go barf-tma)))
	     (defstruct-make-init-dsc kludge
				      (if (atom (car l)) (car l) (caar l))
				      (pop args)
				      slot-alist
				      cons-keywords
				      x)
	     (go O)
	  OD (pop l)
	     (cond ((null l) (return nil))
		   ((atom l) (go barf))
		   ((eq (car l) '&optional) (go barf))
		   ((eq (car l) '&rest) (go S))
		   ((eq (car l) '&aux) (go A)))
	     (or (atom (car l))
		 (defstruct-make-init-dsc kludge
					  (caar l)
					  (cadar l)
					  slot-alist
					  cons-keywords
					  x))
	     (go OD)
	  S  (and (atom (cdr l)) (go barf))
	     (defstruct-make-init-dsc kludge
				      (cadr l)
				      `(list ,@args)
				      slot-alist
				      cons-keywords
				      x)
	     (setq l (cddr l))
	     (and (null l) (return nil))
	     (and (atom l) (go barf))
	     (or (eq (car l) '&aux) (go barf))
	  A  (pop l)
	     (cond ((null l) (return nil))
		   ((atom l) (go barf))
		   ((atom (car l))
		    (push (car l) aux)
		    (push (defstruct-make-empty) aux-init))
		   (t
		    (push (caar l) aux)
		    (push (cadar l) aux-init)))
	     (go A)
	  BARF (defstruct-error
		 "Bad format for defstruct constructor arglist"
		 `(,(car x) ,@(car constructor-description)))
	  BARF-TFA (defstruct-error "Too few arguments to constructor macro" x)
	  BARF-TMA (defstruct-error "Too many arguments to constructor macro" x)))
     (do ((l slot-alist (cdr l)))
	 ((null l))
       (let* ((name (caar l))
	      (slot-description (cdar l))
	      (code (do ((aux aux (cdr aux))
			 (aux-init aux-init (cdr aux-init)))
			((null aux) (defstruct-slot-description-init-code))
		      (and (eq name (car aux)) (return (car aux-init)))))
	      (ppss (defstruct-slot-description-ppss)))
	 (or (and (defstruct-emptyp code) (null ppss))
	     (let* ((number (defstruct-slot-description-number))
		    (dsc (assoc number (car kludge))))
	       (cond ((null dsc)
		      (setq dsc (list* number nil (defstruct-make-empty) 0 0 nil))
		      (push dsc (car kludge))))
	       (cond ((defstruct-emptyp code))
		     ((eq t (cadr dsc)))
		     ((null ppss)
		      (and (defstruct-emptyp (car (cddr dsc)))
			   (setf (car (cddr dsc)) code)))
		     ((memq name (cadr dsc)))
		     ((and (numberp ppss) (numberp code))
		      (setf (ldb ppss (cadr (cddr dsc))) -1)
		      (setf (ldb ppss (caddr (cddr dsc))) code))
		     (t
		      (push (cons ppss code) (cdddr (cddr dsc)))))))))
     (do ((l (car kludge) (cdr l)))
	 ((null l))
       (rplacd (car l) (defstruct-code-from-dsc (car l))))
     (invoke-defstruct-constructor-expander
       description type-description
       (car kludge) (cdr kludge))))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-define-ref-macros (new-slots description)
  (let ((name (defstruct-description-name))
	(returns nil))
    (if (not (defstruct-description-callable-accessors))
	(do ((l new-slots (cdr l))
	     (mname))
	    ((null l))
	  (setq mname (defstruct-slot-description-ref-macro-name (cdar l)))
	  (defstruct-put-macro mname 'defstruct-expand-ref-macro)
	  (defstruct-putprop-compile-time mname (cons name (caar l)) 'defstruct-slot))
      (let* ((type-description
	       (get (defstruct-description-type)
		    'defstruct-type-description))
	     (code (defstruct-type-description-ref-expander))
	     (n (defstruct-type-description-ref-no-args))
	     (but-first (defstruct-description-but-first))
	     (default-pointer (defstruct-description-default-pointer)))
	(do ((args nil (cons (gensym) args))
	     (i n (1- i)))
	    ((< i 2)
	     ;; Last arg (if it exists) is name of structure, for documentation purposes.
	     (and (= i 1)
		  (setq args (cons name args)))
	     (let ((body (cons (if but-first
				   `(,but-first ,(car args))
				 (car args))
			       (cdr args))))
	       (and default-pointer
		    (setq args `((,(car args) ,default-pointer)
				 &optional ,@(cdr args))))
	       (setq args (reverse args))
	       (setq body (reverse body))
	       (do ((l new-slots (cdr l))
		    (mname))
		   ((null l))
		 (setq mname (defstruct-slot-description-ref-macro-name
			       (cdar l)))
		 #+(AND LISPM MIT)		;how do other people do this?
		 (IF (DEFSTRUCT-SLOT-DESCRIPTION-READ-ONLY (CDAR L))
		     (DEFSTRUCT-PUTPROP-COMPILE-TIME MNAME
						     'UNSETFABLE 'SETF-METHOD))
		 #+MacLisp
		 ;;This must come BEFORE the defun. THINK!
		 (defstruct-put-macro mname 'defstruct-expand-ref-macro)
		 (let ((ref (apply
			      code
			      (defstruct-slot-description-number (cdar l))
			      description
			      body))
		       (ppss (defstruct-slot-description-ppss (cdar l)))
		       (DOC (DEFSTRUCT-SLOT-DESCRIPTION-DOCUMENTATION (CDAR L))))
		   (push `(#+LISPM defsubst-with-parent #+NIL defsubst #-(or LispM NIL) defun 
			   ,mname #+LISPM ,name ,args
			   ,DOC
			   ,(if (null ppss) ref `(ldb ,ppss ,ref)))
			 returns))
		 (defstruct-putprop mname
				    (cons name (caar l))
		   'defstruct-slot)))))))
    returns))

))

; From file COMH.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-UNCOMMENT-OUT-REGION "Remove comment start charcaters from the start of each line in the region
which begins with one. A numeric arg specifies now many to remove." ()
  (REGION-LINES (START-LINE END-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (BP (CREATE-BP START-LINE 0))
	 (BPA (CREATE-BP START-LINE 1)))
	((EQ LINE END-LINE))
      (DOTIMES (I *NUMERIC-ARG*)
	(IF (OR (EQ (LINE-TYPE LINE) ':BLANK)
		(NOT (STRING-EQUAL LINE *COMMENT-BEGIN* 0 0
				   (LENGTH *COMMENT-BEGIN*))))
	    (RETURN)
	  (MOVE-BP BP LINE 0)
	  (MOVE-BP BPA LINE (LENGTH *COMMENT-BEGIN*))
	  (DELETE-INTERVAL BP BPA T)))))
  DIS-TEXT)

))
