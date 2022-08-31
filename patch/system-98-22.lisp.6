;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.22
;;; Reason: Define REST, same as CDR.
;;; End activates in debugger.
;;; Atoms at top level in compiled file warn gracefully.
;;; LOAD-PATHNAME-DEFAULTS now synonym for *DEFAULT-PATHNAME-DEFAULTS*.
;;; SETF of GET/GETHASH with three arguments.
;;; In-core compilation of macros temp area bug.
;;; ARGLIST declarations in DEFMACROs bug.
;;; Written 1/01/84 21:30:51 by rms,
;;; while running on Lisp Machine Eighteen from band 5
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.20, CADR 3.4, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 306, ZM MIT.


(globalize "REST")
(remob '*load-pathname-defaults*)
(remob '*load-set-default-pathname*)
(remob '*compile-file-set-default-pathname*)
(unless (eq 'global:*default-pathname-defaults* 'fs:*default-pathname-defaults*)
  (forward-value-cell 'global:*default-pathname-defaults* 'fs:*default-pathname-defaults*))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COMMAND-LOOP-READ ()
  (PROG (CHAR SEXP FLAG FUNCTION)
    RETRY
     ;; Read a character.
     (LET ((READING-COMMAND T))
       (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI)))
     ;; Now, if the char is special, echo and return it.
     (COND ((RASSQ CHAR *PROCEED-TYPE-SPECIAL-KEYS*)
	    (FORMAT T "~C" CHAR)
	    (RETURN 'COM-PROCEED-SPECIFIED-TYPE CHAR))
	   ((RASSQ CHAR *SPECIAL-COMMAND-SPECIAL-KEYS*)
	    (FORMAT T "~C" CHAR)
	    (RETURN 'COM-SPECIAL-COMMAND CHAR))
	   ((OR (LDB-TEST %%KBD-CONTROL-META CHAR)
		(COMMAND-LOOKUP CHAR))
	    (COND ((SETQ FUNCTION (COMMAND-LOOKUP CHAR))
		   (AND (EQ FUNCTION 'COM-NUMBER)
			(SETQ FUNCTION (- (LDB %%CH-CHAR CHAR) #/0)))
		   (FORMAT T "~C" CHAR)
		   (RETURN FUNCTION CHAR))))
	   ((= CHAR #\RUBOUT) (GO RETRY)))	;Ignore rubouts
     ;; Otherwise, unread it and read an s-exp instead.
     (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
     (COND ((AND (NOT RUBOUT-HANDLER)
		 (MEMQ ':RUBOUT-HANDLER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS)))
	    (MULTIPLE-VALUE (SEXP FLAG)
	      (FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT :FULL-RUBOUT)
							 (:ACTIVATION MEMQ (#\END #\RETURN))
							 (:PROMPT " Eval: "))
		       #'SI:READ-FOR-TOP-LEVEL))
	    (WHEN (EQ FLAG ':FULL-RUBOUT)
	      (GO RETRY))
	    (RETURN NIL SEXP))
	   ;; If stream has no rubout handler, degrade gracefully.
	   (T
	    (FUNCALL STANDARD-OUTPUT ':STRING-OUT " Eval: ")
	    (RETURN NIL (SI:READ-FOR-TOP-LEVEL))))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST REST (LIST)
  "Return LIST sans its first element."
  (CDR LIST))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN QC-FILE-FORM (FORM)
  (PROG (TEM FV)
    (COND ((ATOM FORM)
	   (WARN 'ATOM-AT-TOP-LEVEL ':IMPLAUSIBLE
		 "The atom ~S appeared at top level; this would do nothing at FASLOAD time."
		 FORM))
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

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(FORWARD-VALUE-CELL 'LOAD-PATHNAME-DEFAULTS '*DEFAULT-PATHNAME-DEFAULTS*)

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defsetf get (object property &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,property ,default) property)))
    `(setprop ,object ,tem ,value)))

(defsetf gethash (object hash-table &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,hash-table ,default) hash-table)))
    `(sethash ,object ,tem ,value)))

))

; From file QCLAP.LISP SRC:<L.SYS> OZ:
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
	 BREAKOFF-FUNCTION-OFFSETS QLP-SELF-FLAVOR)
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

; From file DEFMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

(DEFUN EXPAND-DEFMACRO (X)
  (LET (*VARLIST* *VALLIST* OPTIONAL-SPECIFIED-FLAGS DEFMACRO-&BODY-FLAG
	(ARGLIST (CADR X))
	WHOLE-ARG-DATA)
    (AND (LISTP ARGLIST)
	 (EQ (CAR ARGLIST) '&WHOLE)
	 (SETQ WHOLE-ARG-DATA `((,(CADR ARGLIST) *MACROARG*))
	       ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
	   (MIN-ARGS (CAR ARGS-DATA))
	   (MAX-ARGS (CADDR ARGS-DATA))
	   (BODY (CDDR X))
	   DOC-STRING DECLS)
      (MULTIPLE-VALUE (NIL DECLS DOC-STRING)
	(EXTRACT-DECLARATIONS BODY NIL T))
      (SETQ DECLS (SUBSET #'(LAMBDA (DECL)
			      (MEMQ (CAR-SAFE DECL) '(ARGLIST RETURN-LIST VALUES)))
			  DECLS))
      `(CLI:NAMED-LAMBDA ,(CAR X) (*MACROARG* &OPTIONAL *MACROENVIRONMENT*)
	,@(IF DOC-STRING (LIST DOC-STRING))
	,@(IF DECLS `((DECLARE . ,DECLS)))
	*MACROENVIRONMENT*  ;; Ok not to refer to it.
	,@(COND ((AND DEFMACRO-CHECK-ARGS
		      (NOT (AND (ZEROP MIN-ARGS) (NULL MAX-ARGS))))
		 `((AND ,(COND ((ZEROP MIN-ARGS)
				`(> (LENGTH *MACROARG*)
				    ,(1+ MAX-ARGS)))
			       ((NULL MAX-ARGS)
				`(< (LENGTH *MACROARG*)
				    ,(1+ MIN-ARGS)))
			       (T `(OR (< (LENGTH *MACROARG*)
				          ,(1+ MIN-ARGS))
				       (> (LENGTH *MACROARG*)
					  ,(1+ MAX-ARGS)))))
			(MACRO-REPORT-ARGS-ERROR *MACROARG* ,MIN-ARGS ,MAX-ARGS))))
		(T NIL))
	(LET* (,@OPTIONAL-SPECIFIED-FLAGS
	       ,@WHOLE-ARG-DATA
	       . ,(MAPCAR 'LIST (NREVERSE *VARLIST*) (NREVERSE *VALLIST*)))
	  . ,BODY)))))

))

zwei:(let ((window (funcall (window-editor-closure (tv:find-window-of-flavor 'zmacs-frame))
			    'eval '*window*)))
       (zwei:push-remove-on-history (send window ':interval)
				    (send window ':buffer-history)))

