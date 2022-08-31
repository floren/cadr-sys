;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Written 8-Feb-84 07:05:15 by Mly,
;;; Reason: DEFMACRO expansion.
;;; Make DEFPACKAGE not over-quote its args.
;;; DESCRIBE on characters.
;;; Interpreted &KEY binding evaluation order bug.
;;; ZWEI:LOAD-FILE-INTO-ZMACS sets pathname defaults by default.
;;; FS:ENABLE-CAPABILITIES, FS:DISABLE-CAPABILITIES, "*" at the beginning of passwords.
;;; EVAL-ABORT-TRIVIAL-ERRORS and error-handler recovery if DEFAULT-CONS-AREA is garbage.
;;; ROTATEF (SWAPF) bug.
;;; Make matrix operations use rationals.
;;; Evaluation of DEFFLAVOR in Zmacs.
;;; while running on Lisp Machine Thirty-two from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.34, CADR 3.6, ZMail 53.10, MIT-Specific 22.0, microcode 306.



; From file DEFMAC.LISP PS:<L.SYS2> OZ:
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
      (MULTIPLE-VALUE (BODY DECLS DOC-STRING)
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

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFMACRO DEFPACKAGE (NAME &BODY ALIST-OF-OPTIONS)
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
/(:RELATIVE-NAMES (name . package)...) specifies local nicknames for
 other packages to be in effect in this package.
/(:RELATIVE-NAMES-FOR-ME (package name)...) specifies local nicknames
 for this package to be in effect in other packages.
/(:AUTO-EXPORT-P t-or-nil) non-NIL specifies that all symbols placed in this package
 should be exported automatically at that time.
/(:SIZE int) specifies the number of symbols to allocate space for initially."
  `(PROG1
     (APPLY (IF (FIND-PACKAGE ,NAME)
		'ALTER-PACKAGE 'MAKE-PACKAGE)
	    ,NAME
	    (LOOP FOR (KEYWORD . ARGS) IN ',ALIST-OF-OPTIONS
		  NCONC (LIST KEYWORD (IF (OR (CDR ARGS) (CONSP (CAR ARGS)))
					  ARGS
					(CAR ARGS)))))
     (RECORD-SOURCE-FILE-NAME (INTERN ,NAME) 'PACKAGE)))	  

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE (ANYTHING &OPTIONAL NO-COMPLAINTS &AUX TYPE)
  "Describe the value or components of any Lisp object.
This is a good way to find out more than the printed representation says."
  (COND	((AND (NAMED-STRUCTURE-P ANYTHING)
	      (COND ((AND (GET (NAMED-STRUCTURE-P ANYTHING) 'NAMED-STRUCTURE-INVOKE)
			  (MEMQ ':DESCRIBE
				(NAMED-STRUCTURE-INVOKE ANYTHING ':WHICH-OPERATIONS)))
		     (NAMED-STRUCTURE-INVOKE ANYTHING ':DESCRIBE))
		    ((GET (SETQ TYPE (NAMED-STRUCTURE-P ANYTHING)) 'DEFSTRUCT-ITEMS)
		     (DESCRIBE-OLD-DEFSTRUCT TYPE ANYTHING))
		    ((GET (NAMED-STRUCTURE-P ANYTHING) 'DEFSTRUCT-DESCRIPTION)
		     (DESCRIBE-DEFSTRUCT ANYTHING)))))
	((OR (ENTITYP ANYTHING) (= (%DATA-TYPE ANYTHING) DTP-INSTANCE))
	 (FUNCALL ANYTHING ':DESCRIBE))
	((ARRAYP ANYTHING)
	 (DESCRIBE-ARRAY ANYTHING))
	((CLOSUREP ANYTHING)
	 (DESCRIBE-CLOSURE ANYTHING))
	((= (%DATA-TYPE ANYTHING) DTP-FEF-POINTER)
	 (DESCRIBE-FEF ANYTHING))
	((SYMBOLP ANYTHING)
	 (DESCRIBE-SYMBOL ANYTHING))
	((CONSP ANYTHING)
	 (DESCRIBE-LIST ANYTHING))
	((= (%DATA-TYPE ANYTHING) DTP-STACK-GROUP)
	 (DESCRIBE-STACK-GROUP ANYTHING))
	((SMALL-FLOATP ANYTHING)
	 (DESCRIBE-SMALL-FLONUM ANYTHING))
	((FLOATP ANYTHING)
	 (DESCRIBE-FLONUM ANYTHING))
        ((= (%DATA-TYPE ANYTHING) DTP-SELECT-METHOD)
         (DESCRIBE-SELECT-METHOD ANYTHING))
	((CHARACTERP ANYTHING)
	 (DESCRIBE-CHARACTER ANYTHING))
	((BIGP ANYTHING)
	 (DESCRIBE-BIGNUM ANYTHING))
	((FIXP ANYTHING)
	 (FORMAT T "~%~R is ~[even~;odd~]" ANYTHING (LDB 0001 ANYTHING)))
	((RATIONALP ANYTHING)
	 (DESCRIBE-RATIONAL-NUMBER ANYTHING))
	((LOCATIVEP ANYTHING)
	 (DESCRIBE-LOCATIVE ANYTHING))
	((COMPLEXP ANYTHING)
	 (DESCRIBE-COMPLEX-NUMBER ANYTHING))
	((NOT NO-COMPLAINTS)
	 (FORMAT STANDARD-OUTPUT "~%I don't know how to describe ~S" ANYTHING)))
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  ANYTHING)

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-CHARACTER (CHARACTER)
  (FORMAT T "~&~S is a character with code ~D.
Its control-bits are ~D~:[~4*~; (~@[Control~*~]~@[Meta~*~]~@[Super~*~]~@[Hyper~*~])~]. Its font is ~D"
	  CHARACTER (CHAR-CODE CHARACTER) (CHAR-BITS CHARACTER) ( 0 (CHAR-BITS CHARACTER))
	  (CHAR-BIT CHARACTER ':CONTROL) (CHAR-BIT CHARACTER ':META)
	  (CHAR-BIT CHARACTER ':SUPER) (CHAR-BIT CHARACTER ':HYPER)
	  (CHAR-FONT CHARACTER)))

))

;;;patched again in 98.39 foo. mly
;; From file READ.LISP PS:<L.IO> OZ:
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

;(DEFVAR *COMMON-LISP-SYMBOL-SUBSTITUTIONS*
;	(COPYTREE
;	  '((// . CLI://) 
;	    (AR-1 . CLI:AR-1)
;	    (AR-1-FORCE . CLI:AR-1-FORCE)
;	    (AREF . CLI:AREF)
;	    (ASSOC . CLI:ASSOC)
;	    (CATCH . CLI:CATCH)
;	    (CHARACTER . CLI:CHARACTER)
;	    (CLOSE . CLI:CLOSE)
;	    (COPY-READTABLE . CLI:COPY-READTABLE)
;	    (DEFSTRUCT . CLI:DEFSTRUCT)
;	    (DELETE . CLI:DELETE)
;	    (ERROR . CLI:ERROR)
;	    (EVAL . CLI:EVAL)
;	    (EVERY . CLI:EVERY)
;	    (FORMAT . CLI:FORMAT)
;	    (INTERSECTION . CLI:INTERSECTION)
;	    (LAMBDA . CLI:LAMBDA)
;	    (LISTP . CLI:LISTP)
;	    (MAP . CLI:MAP)
;	    (MEMBER . CLI:MEMBER)
;	    (NAMED-LAMBDA . CLI:NAMED-LAMBDA)
;	    (NAMED-SUBST . CLI:NAMED-SUBST)
;	    (NINTERSECTION . CLI:NINTERSECTION)
;	    (NLISTP . CLI:NLISTP)
;	    (NUNION . CLI:NUNION)
;	    (RASSOC . CLI:RASSOC)
;	    (READ . CLI:READ)
;	    (READ-FROM-STRING . CLI:READ-FROM-STRING)
;	    (REM . CLI:REM)
;	    (REMOVE . CLI:REMOVE)
;	    (SOME . CLI:SOME)
;	    (STRING-EQUAL . CLI:STRING-EQUAL)
;	    (STRING= . CLI:STRING=)
;	    (SUBST . CLI:SUBST)
;	    (TERPRI . CLI:TERPRI)
;	    (THROW . CLI:THROW)
;	    (UNION . CLI:UNION)
;	    (WARN . CLI:WARN))
;	  )
;  "Alist used as *READER-SYMBOL-SUBSTITUTIONS* for reading Common Lisp code.")

;))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun apply-lambda (fctn a-value-list)
    (prog apply-lambda (tem)
       (or (consp fctn) (go bad-function))
       tail-recurse
       (cond ((eq (car fctn) 'curry-after)
	      (prog ()
		  (setq tem (cddr fctn))
		  (%open-call-block (cadr fctn) 0 4)
		  (%assure-pdl-room (+ (length tem) (length a-value-list)))
		  loop1
		  (or a-value-list (go loop2))
		  (%push (car a-value-list))
		  (and (setq a-value-list (cdr a-value-list))
		       (go loop1))

		  loop2
		  (or tem (go done))
		  (%push (eval1 (car tem)))
		  (and (setq tem (cdr tem))
		       (go loop2))

		  done
		  (%activate-open-call-block)))
	     ((eq (car fctn) 'curry-before)
	      (prog ()
		  (setq tem (cddr fctn))
		  (%open-call-block (cadr fctn) 0 4)
		  (%assure-pdl-room (+ (length tem) (length a-value-list)))
		  loop1
		  (or tem (go loop2))
		  (%push (eval1 (car tem)))
		  (and (setq tem (cdr tem))
		       (go loop1))

		  loop2
		  (or a-value-list (go done))
		  (%push (car a-value-list))
		  (and (setq a-value-list (cdr a-value-list))
		       (go loop2))

		  done
		  (%activate-open-call-block)))
	     ((memq (car fctn) '(cli:lambda cli:named-lambda cli:subst cli:named-subst))
	      (return (cl-apply-lambda fctn a-value-list)))
	     ((memq (car fctn) '(lambda named-lambda subst named-subst))
	      (let* (optionalf quoteflag tem restf init this-restf
		     (fctn (cond ((eq (car fctn) 'named-lambda) (cdr fctn))
				 ((eq (car fctn) 'named-subst) (cdr fctn))
				 (t fctn)))
		     (lambda-list (cadr fctn))
		     (value-list a-value-list)
		     (local-declarations local-declarations)
		     keynames keyinits keykeys keyflags
		     keynames1 keykeys1 keyflags1 (unspecified '(()))
		     allow-other-keys)
		(setq fctn (cddr fctn))	;throw away lambda list
		(do-forever
		  (cond ((and (cdr fctn) (stringp (car fctn)))
			 (pop fctn))	;and doc string.
			;; Process any (DECLARE) at the front of the function.
			;; This does not matter for SPECIAL declarations,
			;; but for MACRO declarations it might be important
			;; even in interpreted code.
			((and (not (atom (car fctn)))
			      (memq (caar fctn) '(declare :declare)))
			 (setq local-declarations (append (cdar fctn) local-declarations))
			 (pop fctn))
			(t (return))))
		(prog ()
		  (when (memq (car fctn) '(named-lambda named-subst))
		    (bind (locf interpreter-environment) nil)
		    (bind (locf interpreter-function-environment) t))
		  ;; If SELF is an instance, and its instance vars aren't bound, bind them.
		  (and (typep self ':instance)
		       (neq self slots-bound-instance)
		       (progn (%using-binding-instances (self-binding-instances))
			      (bind (locf slots-bound-instance) self)))
	     l    (cond ((null value-list) (go lp1))
			((or (null lambda-list)
			     (eq (car lambda-list) '&aux)) 
			 (cond (restf (go lp1))
			       (t (go too-many-args))))
			((eq (car lambda-list) '&key)
			 (go key))
			((eq (car lambda-list) '&optional)
			 (setq optionalf t)
			 (go l1))		    ;Do next value.
			((memq (car lambda-list) '(&quote &eval))
			 (setq quoteflag (eq (car lambda-list) '&quote))
			 (go l1))
			((eq (car lambda-list) '&rest)
			 (setq this-restf t)
			 (go l1))		    ;Do next value.
			((memq (car lambda-list) lambda-list-keywords)
			 (go l1))
			((atom (car lambda-list)) (setq tem (car lambda-list)))
			((atom (caar lambda-list))
			 (setq tem (caar lambda-list))
			 ;; If it's &OPTIONAL (FOO NIL FOOP),
			 ;; bind FOOP to T since FOO was specified.
			 (cond ((and optionalf (cddar lambda-list))
				(and (null (caddar lambda-list)) (go bad-lambda-list))
				(bind (value-cell-location (caddar lambda-list)) t))))
			(t (go bad-lambda-list)))
		  ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
		  ;;  It is in TEM.
		  (and (null tem) (go bad-lambda-list))
		  (cond (restf (go bad-lambda-list))	;Something follows a &REST arg???
			(this-restf		;This IS the &REST arg.
			 ;; If quoted arg, and the list of values is in a pdl, copy it.
			 (and quoteflag
			      (ldb-test %%pht2-map-access-code
					(area-region-bits (%area-number value-list)))
			      (let ((default-cons-area background-cons-area))
				(setq value-list (copylist value-list))))
			 (bind (locf (symeval tem)) value-list)
			 ;; We don't clear out VALUE-LIST
			 ;; in case keyword args follow.
			 (setq this-restf nil restf t)
			 (go l1)))
		  (bind (value-cell-location tem) (car value-list))
		  (setq value-list (cdr value-list))
	     l1   (setq lambda-list (cdr lambda-list))
		  (go l)

	     key  (setf (values nil nil lambda-list nil nil
				keykeys keynames nil keyinits keyflags
				allow-other-keys)
			(decode-keyword-arglist lambda-list))
		  ;; Process the special keyword :ALLOW-OTHER-KEYS if present as an arg.
		  (if (get (locf value-list) ':allow-other-keys)
		      (setq allow-other-keys t))

		  (setq keykeys1 keykeys	;life is tough without LET...
			keynames1 keynames
			keyflags1 keyflags)
	     key1 (when keykeys1
		    (setq tem (get (locf value-list) (pop keykeys1) unspecified))
		    (bind (locf (symeval (car keynames1)))
			  (if (eq tem unspecified) (eval1 (car keyinits))
			    tem))
		    (if (car keyflags1)
			(bind (locf (symeval (car keyflags1))) (neq tem unspecified)))
		    (pop keynames1)
		    (pop keyflags1)
		    (pop keyinits)
		    (go key1))
		  (do ((x value-list (cddr x))
		       keyword)
		      ((null x))
		    (unless (cdr x)
		      (ferror 'sys:bad-keyword-arglist
			      "No argument after keyword ~S"
			      (car x)))
		    (setq keyword (car x))
		    (setq tem (find-position-in-list keyword keykeys))
		    (unless (or tem allow-other-keys)
		      (do-forever
			(setq keyword (cerror ':new-keyword nil
					      'sys:undefined-keyword-argument
					      "Keyword arg keyword ~S, with value ~S, is unrecognized."
					      keyword
					      (cadr value-list)))
			(when (and keyword (setq tem (find-position-in-list keyword keykeys)))
			  (set (nth tem keynames) (cadr x))
			  (and (setq tem (nth tem keyflags))
			       (set tem t))
			  (return)))))
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
			 (setq optionalf t)		;Suppress too few args error
			 (go lp2))
			((memq (car lambda-list) lambda-list-keywords)
			 (go lp2))
			((and (null optionalf) (null this-restf))
			 (and restf (go bad-lambda-list))
			 (go too-few-args))
			((atom (car lambda-list)) (setq tem (car lambda-list))
			 (setq init nil))
			((atom (caar lambda-list))
			 (setq tem (caar lambda-list))
			 (setq init (eval1 (cadar lambda-list)))
			 ;; For (FOO NIL FOOP), bind FOOP to NIL since FOO is missing.
			 (cond ((cddar lambda-list)
				(and (null (caddar lambda-list)) (go bad-lambda-list))
				(bind (value-cell-location (caddar lambda-list)) nil))))
			(t (go bad-lambda-list)))
	     lp3  (and (null tem) (go bad-lambda-list))
		  (bind (value-cell-location tem) init)
		  (and this-restf (setq restf t))
		  (setq this-restf nil)
	     lp2  (setq lambda-list (cdr lambda-list))
		  (go lp1)

	     ex1  (do ((l fctn (cdr l)))
		      ((null (cdr l))
		       (return-from apply-lambda (eval1 (car l))))
		    (eval1 (car l))))))
	     ((eq (car fctn) 'macro)
              (ferror 'sys:funcall-macro
		      "Funcalling the macro ~S."
		      (function-name (cdr fctn)))
	      (return-from apply-lambda
			   (eval1 (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list)))))
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

       ;; Errors jump out of the inner PROG to unbind any lambda-vars bound with BIND.

       bad-lambda-list
       (setq fctn
	     (cerror ':new-function nil 'sys:invalid-lambda-list
		     "~S has an invalid LAMBDA list" fctn))
       retry
       (and (consp fctn) (go tail-recurse))
       (return (apply fctn a-value-list))

       too-few-args
       (return (signal-proceed-case
		 ((args)
		  (make-condition 'sys:too-few-arguments
		       "Function ~S called with only ~D argument~1G~P."
		       fctn (length a-value-list) a-value-list))
		 (:additional-arguments
		   (apply fctn (append a-value-list args)))
		 (:return-value args)
		 (:new-argument-list (apply fctn args))))

       too-many-args
       (return (signal-proceed-case
		 ((args)
		  (make-condition 'sys:too-many-arguments
		       "Function ~S called with too many arguments (~D)."
		       fctn (length a-value-list) a-value-list))
		 (:fewer-arguments
		   (apply fctn (append a-value-list args)))
		 (:return-value args)
		 (:new-argument-list (apply fctn args))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
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
			 keynames keyinits keykeys keyflags
			 keynames1 keykeys1 keyflags1 (unspecified '(()))
			 allow-other-keys)
		    (and (cdr body) (stringp (car body)) (pop body))	;and doc string.
		    ;; Make a binding frame to represent any SPECIAL declarations.
		    (with-stack-list* (decls-env nil interpreter-environment)
		      ;; Find any declarations at the front of the function body
		      ;; and put them onto LOCAL-DECLARATIONS and into DECLS-ENV.
		      (gobble-declarations-internal body decls-env)
		      (with-stack-list* (interpreter-environment nil decls-env)
			(prog (thisvar)
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
							      t decls-env specialf))))
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
				 (apply-lambda-bindvar thisvar value-list decls-env specialf)
				 ;; We don't clear out VALUE-LIST
				 ;; in case keyword args follow.
				 (setq this-restf nil restf t)
				 (go l1)))
      
			  (apply-lambda-bindvar thisvar (car value-list) decls-env specialf)
			  (pop value-list)
		     l1   (pop lambda-list)
			  (go l)
	
		     key  (setf (values nil nil lambda-list nil nil
					keykeys keynames nil keyinits keyflags
					allow-other-keys)
				(decode-keyword-arglist lambda-list))
			  ;; Process the special keyword :ALLOW-OTHER-KEYS if present as an arg.
			  (if (get (locf value-list) ':allow-other-keys)
			      (setq allow-other-keys t))
			  
			  (setq keykeys1 keykeys	;life is tough without LET...
				keynames1 keynames
				keyflags1 keyflags)
		     key1 (when keykeys1
			    (setq tem (get (locf value-list) (pop keykeys1) unspecified))
			    (bind (locf (symeval (car keynames1)))
				  (if (eq tem unspecified) (eval1 (car keyinits))
				    tem))
			    (if (car keyflags1)
				(bind (locf (symeval (car keyflags1))) (neq tem unspecified)))
			    (pop keynames1)
			    (pop keyflags1)
			    (pop keyinits)
			    (go key1))
			  (do ((x value-list (cddr x))
			       keyword)
			      ((null x))
			    (unless (cdr x)
			      (ferror 'sys:bad-keyword-arglist
				      "No argument after keyword ~S"
				      (car x)))
			    (setq keyword (car x))
			    (setq tem (find-position-in-list keyword keykeys))
			    (unless (or tem allow-other-keys)
			      (do-forever
				(setq keyword (cerror ':new-keyword nil
						      'sys:undefined-keyword-argument
						      "Keyword arg keyword ~S, with value ~S, is unrecognized."
						      keyword
						      (cadr value-list)))
				(when (and keyword (setq tem (find-position-in-list keyword keykeys)))
				  (set (nth tem keynames) (cadr x))
				  (and (setq tem (nth tem keyflags))
				       (interpreter-set tem t))
				  (return)))))
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
				 (setq optionalf t)		;Suppress too few args error
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
							      nil decls-env specialf))))
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

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN LOAD-FILE-INTO-ZMACS (PATHNAME &OPTIONAL (MERGE-PATHNAME-DEFAULTS-P T))
  (LET (*WINDOW*
	(*MODE-LIST-SYNTAX-TABLE* *LIST-SYNTAX-TABLE*)
	(*PRINT-BASE* *PRINT-BASE*)
	(*READ-BASE* *READ-BASE*)
	(PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME)))
    (IF MERGE-PATHNAME-DEFAULTS-P
	(FS:SET-DEFAULT-PATHNAME PATHNAME FS:LOAD-PATHNAME-DEFAULTS))
    (FIND-FILE PATHNAME NIL)))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :INCREMENT-CURSORPOS) COMPUTE-BOTTOM-REACHED)

))


; From file QFILE.LISP PS:<MLY.L> OZ:
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
  (OR HOST-UNIT-SPECD (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT)))
  (MULTIPLE-VALUE (PKT SUCCESS FILE-STRING)
    (FUNCALL HOST-UNIT ':COMMAND NIL NIL NIL
		 COMMAND #\CR CAP-STRING))
  (UNWIND-PROTECT
    (COND (SUCCESS
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
	       (LET ((UNITS (FUNCALL HOST ':HOST-UNITS)))
		 (DOLIST (UNIT UNITS)
		   (AND (NEQ UNIT HOST-UNIT)
			(FUNCALL UNIT ':VALIDATE-CONTROL-CONNECTION T)
			(FUNCALL UNIT ':COMMAND NIL NIL NIL
				 COMMAND #\CR CAP-STRING)))))))
    (CHAOS:RETURN-PKT PKT)))

))

; From file QFILE.LISP PS:<MLY.L> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ: IO; QFILE  "

;; Fascism support. Boo Hiss etc
(DEFMETHOD (FILE-HOST-TOPS20-MIXIN :ENABLE-CAPABILITIES) (&REST CAPABILITIES)
  (CHANGE-CAPABILITIES-CHAOS SELF (OR CAPABILITIES '("OPERATOR" "WHEEL")) T))

(DEFMETHOD (FILE-HOST-TOPS20-MIXIN :DISABLE-CAPABILITIES) (&REST CAPABILITIES)
  (CHANGE-CAPABILITIES-CHAOS SELF CAPABILITIES NIL))

(DEFMETHOD (FILE-HOST-VMS-MIXIN :ENABLE-CAPABILITIES) (&REST CAPABILITIES)
  (CHANGE-CAPABILITIES-CHAOS SELF (OR CAPABILITIES '("SYSPRV")) T))

(DEFMETHOD (FILE-HOST-VMS-MIXIN :DISABLE-CAPABILITIES) (&REST CAPABILITIES)
  (CHANGE-CAPABILITIES-CHAOS SELF CAPABILITIES NIL))

))


; From file QFILE.LISP PS:<MLY.L> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN LOGIN-HOST-UNIT (UNIT LOGIN-P UNAME-HOST &AUX HOST CONN SUCCESS PKT ENABLE-CAPABILITIES
			(DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  "Log the host unit UNIT in or out.  LOGIN-P = NIL means log out, otherwise log in.
Note that logging in must be done on each host unit before it can be used,
whether or not this is the host that the user actually specified when
he said /"log in/".  UNAME-HOST should be the host that the user actually logged in on."
  (SETQ HOST (HOST-UNIT-HOST UNIT)
	CONN (HOST-UNIT-CONTROL-CONNECTION UNIT))
  ;;first thing we should do is check to see 
  ;;if the connection is in a valid state, and then logout.
  (if (null login-p)
      (and conn (eq (chaos:state conn) 'chaos:open-state)
	   (setq conn (chaos:close-conn conn "I'm loggin out.")))) ;;nil
  (AND CONN (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)
       (UNWIND-PROTECT
	 (DO ((ID (FILE-MAKE-TRANSACTION-ID))
	      (PASSWORD "")
	      (ACCOUNT "")
	      (NEED-PASSWORD NIL)
	      NEW-USER-ID)
	     (SUCCESS)
	   (SETQ PKT (CHAOS:GET-PKT)
		 ID (FILE-MAKE-TRANSACTION-ID))
	   ;; Only hack user name or password if logging in, not out.
	   (COND (LOGIN-P
		  (SETQ NEW-USER-ID (CDR (ASSQ UNAME-HOST USER-UNAMES)))
		  (COND ((EQ UNAME-HOST 'ITS)
			 (COND ((NULL NEW-USER-ID)
				;; This is an ITS; ask for the user name for all ITSes.
				(FORMAT QUERY-IO "~&ITS uname (default ~A): " USER-ID)
				(LET ((NID (READLINE-TRIM QUERY-IO)))
				  (SETQ NEW-USER-ID (IF (EQUAL NID "") USER-ID NID))))))
			;; Not an ITS: if we don't know user id or if password failed,
			;; ask for one or both.
			((OR NEED-PASSWORD
;			     (NULL NEW-USER-ID)
			     )
			 (MULTIPLE-VALUE (NEW-USER-ID PASSWORD ENABLE-CAPABILITIES)
			   (FILE-GET-PASSWORD USER-ID UNAME-HOST)))
			;; We know the user id; use remembered password if any.
			((EQUAL PASSWORD "")
			 (SETQ PASSWORD
			       (OR (CADR (ASS 'EQUALP
					   (LIST NEW-USER-ID (FUNCALL HOST ':NAME))
					   USER-HOST-PASSWORD-ALIST))
				   ;; None remembered => guess, except on Multics
				   ;; since multics would hassle the guy if it's wrong.
				   (IF (EQ (SEND HOST ':SYSTEM-TYPE) ':MULTICS) "")
				   ;; Try guessing password same as on some other host.
				   (CADR (CAR USER-HOST-PASSWORD-ALIST))
				   ;; Try guessing password same as uname or last part of it.
				   (PROG1
				     (SETQ PASSWORD
					   (SUBSTRING NEW-USER-ID
						      (1+ (OR (STRING-REVERSE-SEARCH #/. NEW-USER-ID)
							      -1))))
				     (PUSH (LIST (LIST NEW-USER-ID (SEND HOST ':NAME))
						 PASSWORD)
					   USER-HOST-PASSWORD-ALIST))))))
		  (OR NEW-USER-ID (SETQ NEW-USER-ID USER-ID))
		  (FILE-HOST-USER-ID NEW-USER-ID HOST)))
	   ;; If the connection got closed while we waited for input, reconnect.
	   (COND ((NOT (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE))
		  (CHAOS:CLOSE-CONN CONN)
		  (CONDITION-CASE (CONN1)
				  (CHAOS:CONNECT HOST *FILE-CONTACT-NAME*
						 *FILE-CONTROL-WINDOW-SIZE*)
		    (:NO-ERROR
		      (SETF (HOST-UNIT-CONTROL-CONNECTION UNIT) CONN1)
		      (SETQ CONN CONN1)
		      (SETF (CHAOS:INTERRUPT-FUNCTION CONN)
			    (LET-CLOSED ((HOST-UNIT UNIT))
			      'HOST-CHAOS-INTERRUPT-FUNCTION)))
		    (SYS:CONNECTION-REFUSED
		      (FERROR 'HOST-NOT-AVAILABLE
			      "File server ~2@*~A is refusing file connections."
			      NIL NIL HOST)))))
	   ;; Send the login command.
	   (CHAOS:SET-PKT-STRING PKT ID "  LOGIN " (OR NEW-USER-ID "") " " PASSWORD " " ACCOUNT)
	   (CHAOS:SEND-PKT CONN PKT)
	   ;; Avoid doing RETURN-PKT on a PKT that has been returned already by SEND-PKT.
	   (SETQ PKT NIL)
	   (SETQ PKT (FILE-WAIT-FOR-TRANSACTION ID CONN "Login"))
	   (IF LOGIN-P
	       (LET ((STR (CHAOS:PKT-STRING PKT))
		     IDX HSNAME-PATHNAME PERSONAL-NAME GROUP PERSONAL-NAME-1 ITEM)
		 (SETQ STR (NSUBSTRING STR (1+ (STRING-SEARCH-CHAR #\SP STR))))
		 (SETQ IDX (FILE-CHECK-COMMAND "LOGIN" STR T))
		 (COND (IDX
;This error check never goes off, except that more hosts are starting
;to return different data from the argument.  So flush it.  RMS 10/13/83.
;			(OR (STRING-EQUAL NEW-USER-ID STR 0 IDX NIL
;					  (SETQ IDX (STRING-SEARCH-CHAR #\SP STR IDX)))
;			    (Send HOST ':IGNORE-USER-ID-IN-REPLY)
;			    (FERROR NIL "File job claims to have logged in as someone else."))
			(SETQ IDX (STRING-SEARCH-CHAR #\SP STR IDX))
			(MULTIPLE-VALUE (HSNAME-PATHNAME PERSONAL-NAME GROUP PERSONAL-NAME-1)
			  (FUNCALL HOST ':HSNAME-INFORMATION UNIT STR IDX))
			;; Record info about this user
			;; only if host login name equals name given to LOGIN.
			(AND (EQUAL USER-ID NEW-USER-ID)
			     (EQUAL USER-PERSONAL-NAME "")
			     (SETQ USER-PERSONAL-NAME PERSONAL-NAME
				   USER-GROUP-AFFILIATION GROUP
				   USER-PERSONAL-NAME-FIRST-NAME-FIRST PERSONAL-NAME-1))
			(SETQ CHAOS:GIVE-FINGER-SAVED-USER-ID T)	;Clear cache
			;; If this is the user's login host
			;; but the host user id is not the one specified in LOGIN,
			;; do not accept the file server's suggested home dir
			;; since it is based on the file server login id.
			(AND (EQ HOST USER-LOGIN-MACHINE)
			     (NOT (EQUAL USER-ID NEW-USER-ID))
			     (SETQ HSNAME-PATHNAME (QUIET-USER-HOMEDIR HOST)))
			;; Record homedir for this host.
			(IF (SETQ ITEM (ASSQ HOST USER-HOMEDIRS))
			    (RPLACD ITEM HSNAME-PATHNAME)
			  (PUSH (CONS HOST HSNAME-PATHNAME) USER-HOMEDIRS))
			;; If we have done remote connect or access on this host,
			;; tell the new file server about it.
			(IF (GET CHAOS-HOST-CONNECT-PLIST HOST)
			    (CWD-CHAOS HOST (GET CHAOS-HOST-CONNECT-PLIST HOST)
				       T NIL UNIT))
			(IF (GET CHAOS-HOST-ACCESS-PLIST HOST)
			    (CWD-CHAOS HOST (GET CHAOS-HOST-ACCESS-PLIST HOST)
				       T T UNIT))
			(IF ENABLE-CAPABILITIES (SEND HOST ':ENABLE-CAPABILITIES)
			  (IF (GET CHAOS-HOST-CAPABILITIES-PLIST HOST)
			      (CHANGE-CAPABILITIES-CHAOS
				HOST (GET CHAOS-HOST-CAPABILITIES-PLIST HOST)
				T UNIT)))
			(SETQ SUCCESS T))
		       ;; If user or password is invalid, force getting it (again).
		       (T
			(CONDITION-CASE ()
					(FILE-PROCESS-ERROR-NEW STR)
			  (LOGIN-PROBLEMS
			    ;; Since this password is wrong, flush it from list of remembered ones.
			    (LET ((ALIST-ELEMENT
				    (ASS 'EQUALP
				      (LIST NEW-USER-ID (FUNCALL HOST ':NAME))
				      USER-HOST-PASSWORD-ALIST)))
			      (IF ALIST-ELEMENT
				  (SETQ USER-HOST-PASSWORD-ALIST
					(DELQ ALIST-ELEMENT USER-HOST-PASSWORD-ALIST))))
			    (SETQ NEED-PASSWORD T))))))
	     (SETQ SUCCESS T))
	   (CHAOS:RETURN-PKT PKT)
	   (SETQ PKT NIL))
	 (COND ((NOT SUCCESS)
		(AND PKT (CHAOS:RETURN-PKT PKT))
		(CHAOS:CLOSE-CONN CONN "Login failed")))))
  T)

))


; From file QFILE.LISP PS:<MLY.L> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN CWD-CHAOS (HOST PATHNAME ERROR-P ACCESS-P &OPTIONAL (HOST-UNIT NIL HOST-UNIT-SPECD)
		  &AUX PKT SUCCESS FILE-STRING COMMAND NEED-PASSWORD ENABLE-CAPABILITIES
		  (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SETQ COMMAND (IF ACCESS-P "ACCESS" "CWD"))
  (DO-FOREVER
    (LET ((DIR (FILE-PRINT-DIRECTORY PATHNAME))
	  (PASSWORD ""))
      ;; If we have failed once, ask for a new password.
      ;; The first time, if we remember a password, use it.
      (COND (NEED-PASSWORD
	     (MULTIPLE-VALUE (DIR PASSWORD ENABLE-CAPABILITIES)
	       (FILE-GET-PASSWORD DIR HOST T))
	     (WHEN ENABLE-CAPABILITIES (SEND HOST ':ENABLE-CAPABILITIES))
	     (SETQ PATHNAME (PARSE-PATHNAME DIR HOST))
	     (SETQ DIR (FILE-PRINT-DIRECTORY PATHNAME)))
	    ;; We know the user id; use remembered password if any.
	    ((EQUAL PASSWORD "")
	     (SETQ PASSWORD
		   (OR (CADR (ASS 'EQUALP (LIST DIR (FUNCALL HOST ':NAME))
				  USER-HOST-PASSWORD-ALIST))
		       ""))))
      (OR HOST-UNIT-SPECD (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT)))
      (MULTIPLE-VALUE (PKT SUCCESS FILE-STRING)
	(FUNCALL HOST-UNIT ':COMMAND NIL NIL NIL
		 COMMAND #\CR
		 DIR #\CR
		 PASSWORD #\CR))
      (UNWIND-PROTECT
	(COND (SUCCESS
	       ;; Succeeded on one host unit.
	       ;; Record what our connected or accessed directory is for this host.
	       (PUTPROP (IF ACCESS-P CHAOS-HOST-ACCESS-PLIST CHAOS-HOST-CONNECT-PLIST)
			PATHNAME HOST)
	       ;; Also inform any other host units that are connected now.
	       (OR HOST-UNIT-SPECD
		   (LET ((UNITS (FUNCALL HOST ':HOST-UNITS)))
		     (DOLIST (UNIT UNITS)
		       (AND (NEQ UNIT HOST-UNIT)
			    (FUNCALL UNIT ':VALIDATE-CONTROL-CONNECTION T)
			    (FUNCALL UNIT ':COMMAND NIL NIL NIL
				     COMMAND #\CR
				     DIR #\CR
				     PASSWORD #\CR)))))
	       (RETURN T))
	      (T
	       (CONDITION-CASE-IF (NOT ERROR-P) (ERROR-OBJECT)
		   (CONDITION-CASE ()
				   (FILE-PROCESS-ERROR-NEW FILE-STRING)
		     (LOGIN-PROBLEMS
		       ;; Since this password is wrong, flush it from list of remembered ones.
		       (LET ((ALIST-ELEMENT
			       (ASS 'EQUALP (LIST DIR (FUNCALL HOST ':NAME))
				    USER-HOST-PASSWORD-ALIST)))
			 (IF ALIST-ELEMENT
			     (SETQ USER-HOST-PASSWORD-ALIST
				   (DELQ ALIST-ELEMENT USER-HOST-PASSWORD-ALIST))))
		       (SETQ NEED-PASSWORD T)))
		 (ERROR ERROR-OBJECT))))
	(CHAOS:RETURN-PKT PKT)))))

))

;;;by kab
(setf (documentation 'mod 'function)
  "Return DIVIDEND taken in modulus MODULUS.
It is the same as the second value (the remainder) from (FLOOR DIVIDEND MODULUS).
The result will be in the range from zero (inclusive) to MODULUS (exclusive) with the same
sign as MODULUS.")


; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN EVAL-ABORT-TRIVIAL-ERRORS-HANDLER (CONDITION)
  (DECLARE (SPECIAL TOP-LEVEL-FORM))
  (WHEN (COND ((CONDITION-TYPEP CONDITION 'SYS:CELL-CONTENTS-ERROR)
	       (AND (SYMBOLP (SEND CONDITION ':CONTAINING-STRUCTURE))
		    (MEM*Q-FWD (SEND CONDITION ':CONTAINING-STRUCTURE) TOP-LEVEL-FORM)))
	      ((CONDITION-TYPEP CONDITION 'SYS:INVALID-FUNCTION-SPEC)
	       (MEM*Q (SEND CONDITION ':FUNCTION-SPEC) TOP-LEVEL-FORM))
	      ((CONDITION-TYPEP CONDITION 'SYS:UNCLAIMED-MESSAGE)
	       (MEM*Q (SEND CONDITION ':MESSAGE) TOP-LEVEL-FORM))
	      (T (MEM*Q (FUNCTION-NAME (SEND CONDITION ':FUNCTION)) TOP-LEVEL-FORM)))
    (SEND QUERY-IO ':FRESH-LINE)
    (SEND CONDITION ':PRINT-ERROR-MESSAGE CURRENT-STACK-GROUP T QUERY-IO)
    (SEND QUERY-IO ':CLEAR-INPUT)
    (LET (EVALHOOK APPLYHOOK)
      (UNLESS (FQUERY `(:CHOICES
			 ,(MAPCAR #'(LAMBDA (CHOICE)
				      (IF (EQ (CAAR CHOICE) NIL)
					  (APPEND CHOICE '(#\C-Z))
					CHOICE))
				  FORMAT:Y-OR-N-P-CHOICES))
		      "Enter the debugger (No means abort instead)? ")
	(SIGNAL-CONDITION EH:ABORT-OBJECT))))
  (VALUES))

))
; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFMETHOD (PRINT-READABLY-MIXIN :PRINT-SELF) (STREAM &REST IGNORE)
  (SEND STREAM ':STRING-OUT "#")
  (LET ((*PACKAGE* PKG-USER-PACKAGE))
    (PRIN1 (TYPEP SELF) STREAM))
  (SEND STREAM ':TYO #\SP)
  (DO ((INIT-OPTIONS (SEND SELF ':RECONSTRUCTION-INIT-PLIST) (CDDR INIT-OPTIONS)))
      ((NULL INIT-OPTIONS))
    (PRIN1 (CAR INIT-OPTIONS) STREAM)
    (SEND STREAM ':TYO #\SP)
    (PRIN1 (CADR INIT-OPTIONS) STREAM)
    (IF (CDDR INIT-OPTIONS)
	(SEND STREAM ':TYO #\SP)))
  (SEND STREAM ':TYO #/))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFMETHOD (PRINT-READABLY-MIXIN :READ-INSTANCE) (FLAVOR STREAM)
  (DO (CH INIT-OPTIONS)
      (())
    ;; Skip past spaces.
    (DO ()
	((NOT (= (SETQ CH (SEND STREAM ':TYI)) #\SP))
	 (SEND STREAM ':UNTYI CH)))
    (IF (= CH #/)
	(RETURN (LEXPR-FUNCALL 'MAKE-INSTANCE FLAVOR INIT-OPTIONS)))
    (SETQ INIT-OPTIONS
	  (LIST* (READ STREAM)
		 (READ STREAM)
		 INIT-OPTIONS))))

))

(setf (documentation 'zwei:compile-print-interval 'function)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
REGION-NAME is a string to print as the name of this whole object,
 or NIL to mention each object's name.
USE-TYPEOUT can be T, NIL, :TYPEOUT or :PROMPT.
  T prints form values and names of objects in typeout window.
 Otherwise, form values appear in the echo area, and 
  :TYPEOUT prints names of objects in typeout window.
  :PROMPT prints names of objects in prompt line.
  NIL prints names of objects in the echo area.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is either COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowercase past participle and period (/"compiled./").")

(setf (documentation 'disk-save 'function)
  "Save the current Lisp world in partition PARTITION.
PARTITION can be either a string naming a partition, or a number which signifies
 a partition whose name starts with LOD.
NO-QUERY says do not ask for confirmation (or any keyboard input at all).
INCREMENTAL means to write out only those parts of the world which have changed
 since the it was loaded from disk. (The effect of loading a world from a band
 saved incrementally is that the incremental saves /"patch/" the original full save.")

(setf (documentation 'call 'function)
  "The first argument is a function to call.
The remaining arguments are in pairs, consisting of a descriptor arg and a data arg.
The descriptor arg says what to do with the data arg.
The descriptor arg value should be either a keyword or a list of keywords or NIL.
NIL means that the data argument is to be treated as a single argument to the
 function.
The allowed keywords are :SPREAD and :OPTIONAL.
:SPREAD means that the data argument is a list of arguments
 rather than a single argument.
:OPTIONAL means that the data argument can be ignored if
 the function being called doesn't ask for it.
 After the first :OPTIONAL, all args supplied are considered optional.")

; From file EHF.LISP PS:<L.WINDOW> OZ:
;;;make matrix hackery use rationals. Online document everything.
(load "SYS:SYS2;MATRIX" :set-default-pathname nil :verbose nil)


; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN FILE-GET-PASSWORD (UID HOST &OPTIONAL DIRECTORY-FLAG)
  "Given a username and host, ask for either a password or a username and password.
If DIRECTORY-FLAG is set, we are using directory names, not passwords"
  (DECLARE (VALUES NEW-USER-ID PASSWORD ENABLE-CAPABILITIES))
  (LET* ((LINE (MAKE-STRING 30 ':FILL-POINTER 0))
	 ENABLE-CAPABILITIES CHAR UID-P
	 (HACK (AND (SEND QUERY-IO ':OPERATION-HANDLED-P ':CLEAR-BETWEEN-CURSORPOSES)
		    (SEND QUERY-IO ':OPERATION-HANDLED-P ':COMPUTE-MOTION)))
	 START-X START-Y)
    (UNLESS DIRECTORY-FLAG
      (SETQ UID (OR (CDR (ASSQ HOST USER-UNAMES)) UID)))
    (LET ((PW (CADR (ASSOC-EQUALP (LIST UID (FUNCALL HOST ':NAME))
				  USER-HOST-PASSWORD-ALIST))))
      (WHEN PW (RETURN-FROM FILE-GET-PASSWORD UID PW)))
    (TAGBODY
	(WHEN HACK (SETQ HACK (MAKE-STRING 30. ':INITIAL-VALUE #\X ':FILL-POINTER 0)))
     RESTART
	(UNLESS DIRECTORY-FLAG
	  (SETQ UID (OR (CDR (ASSQ HOST USER-UNAMES)) UID)))
	(LET ((PW (CADR (ASSOC-EQUALP (LIST UID (FUNCALL HOST ':NAME))
				      USER-HOST-PASSWORD-ALIST))))
	  (WHEN PW (RETURN-FROM FILE-GET-PASSWORD UID PW)))
	(FORMAT QUERY-IO
		(IF DIRECTORY-FLAG "~&Type the password for directory ~A on host ~A,
or a directory and password.  /"Directory/" here includes devices as well: "
		  "~&Current login name is ~A ~<~%~:;for host ~A.~>
Type either password or ~<~%~:;loginname<space>password: ~>")
		UID HOST)
	(WHEN HACK (MULTIPLE-VALUE (START-X START-Y) (SEND QUERY-IO ':READ-CURSORPOS)))
     L  (SETQ CHAR (SEND QUERY-IO ':TYI))
	(COND ((= CHAR #\\)			;quoting character.
	       (VECTOR-PUSH-EXTEND (SEND QUERY-IO ':TYI) LINE)
	       (WHEN HACK
		 (VECTOR-PUSH-EXTEND #\X HACK)
		 (SEND QUERY-IO ':TYO #\X)))
	      ((= CHAR #\RUBOUT)
	       (WHEN (ZEROP (FILL-POINTER LINE))
		 (GO FLUSH))
	       (VECTOR-POP LINE)
	       (WHEN HACK
		 (VECTOR-POP HACK)
		 (MULTIPLE-VALUE-BIND (X Y)
		     (SEND QUERY-IO ':COMPUTE-MOTION HACK 0 NIL
				    START-X START-Y)
		   (MULTIPLE-VALUE-BIND (CX CY) (SEND QUERY-IO ':READ-CURSORPOS)
		     (SEND QUERY-IO ':CLEAR-BETWEEN-CURSORPOSES X Y CX CY))
		   (SEND QUERY-IO ':SET-CURSORPOS X Y))))
	      ((= CHAR #\CLEAR-INPUT)
	       (GO FLUSH))
	      ((AND (= CHAR #\SPACE)
		    (NOT UID-P))		;allow spaces in passwords
	       (WHEN ENABLE-CAPABILITIES
		 (VECTOR-PUSH-EXTEND #\* LINE 1);make sure we have room for extra element
		 (DOTIMES (I (1- (FILL-POINTER LINE)))
		   (SETF (AREF LINE (1+ I)) (AREF LINE (1- I))))
		 (SETF (AREF LINE 0) #\*)
		 (SETQ ENABLE-CAPABILITIES NIL))
	       (SETQ UID-P T
		     UID LINE
		     LINE (MAKE-STRING 30. ':FILL-POINTER 0))
	       (WHEN HACK
		 (MULTIPLE-VALUE-BIND (CX CY) (SEND QUERY-IO ':READ-CURSORPOS)
		   (SEND QUERY-IO ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
		 (SEND QUERY-IO ':SET-CURSORPOS START-X START-Y))
	       (FORMAT QUERY-IO "~A " UID)
	       (WHEN HACK (MULTIPLE-VALUE (START-X START-Y)
			    (SEND QUERY-IO ':READ-CURSORPOS))))
	      ((= CHAR #\CR)
	       (OR DIRECTORY-FLAG (FILE-HOST-USER-ID UID HOST))
	       (FRESH-LINE QUERY-IO)
	       (AND (MEMQ ':MAKE-COMPLETE (FUNCALL QUERY-IO ':WHICH-OPERATIONS))
		    (FUNCALL QUERY-IO ':MAKE-COMPLETE))
	       (IF RECORD-PASSWORDS-FLAG
		   (PUSH `((,UID ,(FUNCALL HOST ':NAME)) ,LINE) USER-HOST-PASSWORD-ALIST))
	       (WHEN HACK (RETURN-ARRAY HACK))
	       (RETURN-FROM FILE-GET-PASSWORD UID LINE ENABLE-CAPABILITIES))
	      ((AND (= CHAR #\*)
		    (= (FILL-POINTER LINE) 0))
	       (SETQ ENABLE-CAPABILITIES T))		    
	      (T (WHEN HACK
		   (VECTOR-PUSH-EXTEND #\X HACK)
		   (SEND QUERY-IO ':TYO #\X))
		 (VECTOR-PUSH-EXTEND CHAR LINE)))
	(GO L)
     FLUSH
	(WHEN HACK
	  (MULTIPLE-VALUE-BIND (CX CY) (SEND QUERY-IO ':READ-CURSORPOS)
	    (SEND QUERY-IO ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
	  (SEND QUERY-IO ':SET-CURSORPOS START-X START-Y)
	  (SETF (FILL-POINTER HACK) 0))
	(WHEN UID-P (RETURN-ARRAY (PROG1 LINE (SETQ LINE UID UID NIL))))
	(FORMAT QUERY-IO "XXX Flushed.~&")
	(SETF (FILL-POINTER LINE) 0 UID-P NIL)
	(GO RESTART))))
))

#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN INVOKE-RESUME-HANDLER (CONDITION &OPTIONAL PROCEED-TYPE &REST ARGS)
  "Invoke a resume handler for PROCEED-TYPE on CONDITION.
Recall that each resume handler is identified by a proceed-type
and has a list of condition-names it applies to, and a predicate to actually decide.
We run the innermost resume handler for the specified PROCEED-TYPE
which applies to CONDITION, based on CONDITION's condition names
and on applying the predicate to it.
If PROCEED-TYPE is NIL, the innermost resume handler that applies
is used regardless of its proceed type; however, in this case,
a T in the list CONDITION-RESUME-HANDLERS terminates the scan."
  (LET ((H (FIND-RESUME-HANDLER CONDITION PROCEED-TYPE)))
    (WHEN H
      (CALL (FIFTH H) NIL CONDITION ':SPREAD (NTHCDR 5 H) ':SPREAD ARGS)
      (FERROR NIL "A condition resume handler for proceed-type ~S returned to its caller." PROCEED-TYPE)))
  (AND PROCEED-TYPE
       (FERROR NIL "Invalid proceed-type ~S returned by handler for ~S."
	       PROCEED-TYPE CONDITION)))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro rotatef (&rest places)
  "Rotates the values between all the specified PLACEs.
The second PLACE's value is put into the first PLACE,
the third PLACE's value into the second PLACE, and so on,
and the first PLACE's value is put into the last PLACE."
  (let ((setf-methods
	  (mapcar #'(lambda (place)
		      (multiple-value-list (get-setf-method place)))
		  places)))
    (sublis-eval-once
      (mapcan #'(lambda (setf-method)
		  (pairlis (first setf-method)
			   (second setf-method)))
	      setf-methods)
      `(let ((,(car (third (car (last setf-methods))))
	      ,(fifth (car setf-methods))))
	 ,.(loop for i from 1 below (length places)
		 collect (sublis (list (cons (car (third (nth (1- i) setf-methods)))
					     (fifth (nth i setf-methods))))
				 (fourth (nth (1- i) setf-methods))))
	 ,(fourth (car (last setf-methods)))
	 nil)
      t t)))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(deff swapf 'rotatef)

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN COMPILE-AT-APPROPRIATE-TIME (FL NAME LAMBDA-EXP &OPTIONAL FORM-TO-EVAL)
  ;; Switch to the appropriate package so gensyms get defined in that package and
  ;; and error messages about wrong package defining a function are avoided.  But
  ;; if compiling, don't mess with the package, so that symbols in the qfasl file
  ;; get interned in the proper place.
  (LET ((*PACKAGE* (IF COMPILER:QC-FILE-IN-PROGRESS PACKAGE
		     (FLAVOR-PACKAGE FL)))
	;; Declare the instance variables for the code being compiled.
	(LOCAL-DECLARATIONS (LIST* (FLAVOR-DECLARATION (FLAVOR-NAME FL))
				   LOCAL-DECLARATIONS)))
    (IF COMPILER:QC-FILE-IN-PROGRESS
	;; This case if in QC-FILE or editor-compile
	(IF *JUST-COMPILING*
	    ;; Here if QC-FILE.  If it's a combined method,
	    ;; actually FDEFINE a FASLOAD-COMBINED method when we load,
	    ;; but make the FEF's name say :COMBINED.
	    (COMPILER:QC-TRANSLATE-FUNCTION
	      (IF (AND (= 4 (LENGTH NAME)) (EQ (THIRD NAME) ':COMBINED))
		  (LIST* (FIRST NAME) (SECOND NAME) 'FASLOAD-COMBINED (CDDDR NAME))
		NAME)
	      LAMBDA-EXP 'COMPILER:MACRO-COMPILE
	      'COMPILER:QFASL NAME)
	  ;; Here for compiling from editor buffer, or QC-FILE to core.
	  (COMPILER:LOCKING-RESOURCES-NO-QFASL
	    (LET ((INHIBIT-FDEFINE-WARNINGS T))
	      (PUSH (LIST NAME FDEFINE-FILE-PATHNAME) *FLAVOR-COMPILATIONS*)
	      (COMPILER:QC-TRANSLATE-FUNCTION
		NAME LAMBDA-EXP 'COMPILER:MACRO-COMPILE 'COMPILER:COMPILE-TO-CORE))))
      ;; This case if not doing anything special
      (PUSH (LIST NAME FDEFINE-FILE-PATHNAME) *FLAVOR-COMPILATIONS*)
      (LET ((FDEFINE-FILE-PATHNAME NIL)
	    (INHIBIT-FDEFINE-WARNINGS T))
	;; If the compiler is not loaded, try to limp through with interpreted methods
	(FUNCALL (IF (FBOUNDP 'COMPILE)
		     'COMPILE
		   'FDEFINE)
		 NAME LAMBDA-EXP)))
    ;; Evaluate form now or send it over in the qfasl file
    (AND FORM-TO-EVAL
	 (IF *JUST-COMPILING*
	     (COMPILER:FASD-FORM FORM-TO-EVAL)
	   (EVAL FORM-TO-EVAL)))))

))
