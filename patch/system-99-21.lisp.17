;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Patch-File:T; Base:10; Readtable:ZL -*-
;;; Patch file for System version 99.21
;;; Reason:
;;;  hairify si:select-processor
;;;  sys:%1d-aref, etc  ar-1-force, etc
;;;  describing closures (again)
;;;  si::old-dynamic-eval for the hopelessly losing
;;;  lambda achieves special-form-hood -- (lambda . cruft)  (function (lambda . cruft))
;;;  zwei:indentation declaration
;;;  setf checks for odd number of args
;;;  zmacs m-x finish patch asks whether to finish unreleaed (unless given numeric arg)
;;;  setplist sends a :set-property-list message, not obsolete :setplist
;;;  interpreter's external-value-cell function wasn't following evcp's
;;;  si:%pointer-unsigned, si:%make-pointer-unsigned, si:%pointer-lessp
;;; Written 12-Feb-85 22:54:21 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.20, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.



(eval-when (load compile eval)
  (export (intern "INDENTATION" 'zwei) 'zwei)
  (globalize (intern "%1D-AREF" 'si) 'sys)
  (globalize (intern "%1D-ASET" 'si) 'sys)
  (globalize (intern "%1D-ALOC" 'si) 'sys)
  (export (intern "%POINTER-UNSIGNED" 'si) 'si)
  (export (intern "%MAKE-POINTER-UNSIGNED" 'si) 'si)
  (export (intern "%POINTER-LESSP" 'si) 'si)
  )

;; has to be a separate eval-when -- we haven't yet exported this symbol!
(eval-when (eval compile load)
  (putprop 'zwei:indentation t 'si::debug-info)
  (or (assq 'arglist si::*interpreter-declaration-type-alist*)
      (setq si::*interpreter-declaration-type-alist*
	    (list* '(arglist ignore) '(values ignore) '(zwei:indentation ignore)
		   si::*interpreter-declaration-type-alist*)))
  )

(DEFCONSTANT SI::EXPLORER-TYPE-CODE 3)

; From file OZ:KANSAS:<L.SYS2>LMMAC.LISP.384 at 12-Feb-85 22:54:22
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFPROP :EXPLORER 3 PROCESSOR-TYPE-CODE)

(DEFMACRO SELECT-PROCESSOR (&REST CLAUSES)
  "CLAUSES begin with :CADR, :LAMBDA or :EXPLORER (ugh!)"
  `(CASE PROCESSOR-TYPE-CODE
     ,@(MAPCAR #'(LAMBDA (CLAUSE)
		   (IF (MEMQ (CAR CLAUSE) '(T OTHERWISE))
		       CLAUSE
		     (CONS (MAPCAR #'(LAMBDA (X)
				       (OR (GET X 'PROCESSOR-TYPE-CODE)
					   (FERROR NIL "~S is an unknown processor-type" X)))
				   (IF (CLI:LISTP (CAR CLAUSE))
				       (CAR CLAUSE) (LIST (CAR CLAUSE))))
			   (CDR CLAUSE))))
	       CLAUSES)))

;; brand s compatibility
(DEFF SYS:%1D-AREF 'GLOBAL:AR-1-FORCE)
(DEFF SYS:%1D-ASET 'AS-1-FORCE)
(DEFF SYS:%1D-ALOC 'AP-1-FORCE)

(compiler:make-obsolete if-in-cadr-else-lambda "use SI:SELECT-PROCESSOR")
(compiler:make-obsolete if-in-lambda-else-cadr "use SI:SELECT-PROCESSOR")
(compiler:make-obsolete if-in-maclisp "use the #+MACLISP reader macro")
(compiler:make-obsolete if-in-maclisp "use the #+LISPM reader macro")
(compiler:make-obsolete if-for-maclisp "use the #+MACLISP reader macro")
(compiler:make-obsolete if-for-lispm "use the #+LISPM reader macro")
(compiler:make-obsolete if-for-maclisp-else-lispm "use the #+LISPM//#+MACLISP reader macro")

))

; From file OZ:KANSAS:<L.SYS2>DESCRIBE.LISP.1 at 13-Feb-85 02:14:42
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; DESCRIBE  "

(defvar *describe-print-level* 2
  "Value of *PRINT-LEVEL* to use in DESCRIBE")
(defvar *describe-print-length* 3
  "Value of *PRINT-LENGTH* to use in DESCRIBE")

))

; From file OZ:KANSAS:<L.SYS2>DESCRIBE.LISP.1 at 13-Feb-85 04:19:29
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; DESCRIBE  "

(defun describe-closure (cl)
  (if (interpreter-environment-closure-p cl)
      (describe-interpreter-closure cl)		;defined in sys; eval
    (let ((bindings (closure-bindings cl))
	  (sym nil) (offset nil))
      (format t "~%~S is a closure of ~S~%" cl (closure-function cl))
      (case (length bindings)
	(0 (format t "(No bindings)"))
	(1 (format t "Lexical environment:~%")
	   (describe-lexical-closure-bindings bindings *standard-output*))
	(t
	 (do ((bindings bindings (cddr bindings)))
	     ((null bindings))
	   (setq sym (%find-structure-header (car bindings))
		 offset (%pointer-difference (car bindings) sym))
	   (if (not (symbolp sym))
	       (format t "    ~S" (car bindings))
	     (format t "    ~[Print name~;Value~;Function~;Property list~;Package~] cell of ~S"
		     offset sym))
	   (format t ":~40T~:[void~;~S~]~%"
		   (location-boundp (cadr bindings))
		   (and (location-boundp (cadr bindings))
			(contents (cadr bindings)))))))
      (unless (consp (closure-function cl))		;don't describe interpreted functions.
	(describe-1 (closure-function cl))))))

;;>> This should be much better.
;;>> When the compiler is hacked to record the names of closure-slots to which it refers,
;;>> this function should know how to print the name of those slots
;;>> Until Mly does that this is the best we can do.
(defun describe-lexical-closure-bindings (bindings stream)
  (loop for frame in (car bindings)
	for f from 0
	do (format stream "  Context ~D~%" f)
	   (loop for x in frame
		 for i from 0
		 do (format stream "    Slot ~D: ~S~%" i x))))

))

; From file OZ:KANSAS:<L.SYS2>DESCRIBE.LISP.1 at 13-Feb-85 04:22:07
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; DESCRIBE  "

(DEFUN DESCRIBE-DEFSTRUCT (X &OPTIONAL DEFSTRUCT-TYPE &AUX DESCRIPTION)
  "Prints out a description of X, including the contents of each of its
slots.  DEFSTRUCT-TYPE should be the name of the structure so
DESCRIBE-DEFSTRUCT can figure out the names of the slots of X.  If X is
a named structure, you don't have to provide DEFSTRUCT-TYPE.  Normally
the DESCRIBE function will call DESCRIBE-DEFSTRUCT if asked to describe
a named structure; however, some named structures have their own way of
describing themselves."
  (SETQ DESCRIPTION (GET (OR DEFSTRUCT-TYPE
			     (IF (CONSP X) (CAR X) (NAMED-STRUCTURE-P X)))
			 'DEFSTRUCT-DESCRIPTION))
  (FORMAT T "~%~S is a ~S~%" X (DEFSTRUCT-DESCRIPTION-NAME))
  (DOLIST (L (DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
    (FORMAT T "   ~S:~30T~S~%"
	    (CAR L)
	    (EVAL `(,(DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR L)) ',X))))
  X)

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.93 at 13-Feb-85 04:23:07
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun interpreter-environment-closure-p (closure &aux (bindings (closure-bindings closure)))
  "T if CLOSURE is a closure over the interpreter environment variables"
  (or (getf bindings (locf (symbol-value '*interpreter-variable-environment*)))
      (getf bindings (locf (symbol-value '*interpreter-function-environment*)))
      (getf bindings (locf (symbol-value '*interpreter-frame-environment*)))))

;;; this is here rather than in sys2; describe to modularize knowledge about interpreter
;;;  internals
(defun describe-interpreter-closure (closure)
  (let ((venv (symeval-in-closure closure '*interpreter-variable-environment*))
	(fnenv (symeval-in-closure closure '*interpreter-function-environment*))
	(frenv (symeval-in-closure closure '*interpreter-frame-environment*))
	(*print-level* *describe-print-level*)
	(*print-length* *describe-print-length*)
	)
    (format t "~%~S is an interpreter closure of ~S~%  Environment is:"
	    closure (closure-function closure))
    (describe-interpreter-environment *standard-output* venv fnenv frenv))
  closure)

;;>> It would be nice to be able to say whether a given environment is stack-consed
;;>> To do this, a stack-group would be passed into this function and thence to stack-list-p
;;>> However, I don't know how to get %stack-frame-pointer out of a different stack group
;;>> sg-ap seems like the right thing, but I'm not counting on it...
(defun describe-interpreter-environment (stream venv fnenv frenv)
  (flet ((frob-bind-frames (env name &aux special-kludge)
	    (when (do ((frames env (cdr frames)))
		      ((atom frames)
		       (format stream "~&  (No ~A)" name)
		       nil)
		    (if (plusp (length frames)) (return t)))
	      (format stream "~&  ~A:" name)
	      (do ((frames env (cdr frames)))
		  ((atom frames))
		(loop for p on (car frames) by 'cddr
		      with kludge = nil
		      as slot-pointer = (locf (cadr p))
		      as slot-dtp = (%p-data-type slot-pointer)
		      as header = (%find-structure-header (car p))
		      do (cond ;; special
			       ((= slot-dtp dtp-one-q-forward)
				;; this is to get around the fact that specials occur in
				;;  in two successive bind-frames. Ugh!
				(unless (memq (car p) special-kludge)
				  (push (car p) kludge)
				  (format stream "~%    ~S:~30T(special)" header)))
			       ;; instance variable or lexical variable
			       (t
				(setq slot-pointer (follow-cell-forwarding slot-pointer t))
				(format stream "~%    ~:[~;Instance var ~]~S:~30T ~:[Void~;~S~]"
					(= slot-dtp dtp-external-value-cell-pointer)
					header
					(location-boundp slot-pointer)
					(and (location-boundp slot-pointer)
					     (contents slot-pointer)))))
		      finally (setq special-kludge kludge))
;		(format stream "~%")
		)))
	  (frob-block-frames (env name type fn)
	    (if (dolist (frame env t)
		  (when (eq (car frame) type) (return nil)))
		(format stream "~&  (No ~A)" name)
	      (format stream "~&  ~A:" name)
	      (dolist (frame env)
		(when (eq (car frame) type) (funcall fn frame))))))
    (if (eq fnenv t)
	(format stream "~& This is a old-dynamic-eval environment: ~
			All bindings and references are special"))
    (if (or (eq venv t)
	    (eq (cdr (last venv)) t))
	(format stream "~& All free variable references are special"))
    (frob-bind-frames venv "Variables")
    (unless (eq fnenv t)
      (frob-bind-frames fnenv "Functions"))
    (frob-block-frames frenv "Tagbodies" 'tagbody
		       #'(lambda (frame)
			   (let ((count (loop for x in (car (cadr frame))
					      count (not (consp x)))))
			     (if (zerop count)
				 (format stream "~%    A ~S with no ~S tags" 'tagbody 'go)
			       (format stream "~%    ~S with tag~P" 'tagbody count)
			       (loop for x in (car (cadr frame))
				     with firstp = t
				     (when (not (consp x))
				       (format stream "~:[,~] ~S" firstp x)
				       (setq firstp nil)))))))
    (frob-block-frames frenv "Blocks" 'block
		       #'(lambda (frame)
			   (format stream "~%    ~S named ~S"
				   'block (car (cadr frame)))))))

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.93 at 13-Feb-85 04:25:43
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defparameter old-dynamic-environment nil)
(defun old-dynamic-eval (form &optional nohook)
  "Evaluate FORM using the old-style dynamic evaluator
/(ie all variables and functions bound as specials,
 and free reference to non-special variables allowed)
This is a kludge: you should not be using this function for anything but to bootstrap old
code to make it work with the new winning interpreter.

This function will not be supported indefinitely: please update your code to reflect
the /"New Order/"."
  (unless old-dynamic-environment
    (setq old-dynamic-environment (make-interpreter-environment :functions t :variables t)))
  (binding-interpreter-environment (old-dynamic-environment)
    (cond ((and *evalhook* (not nohook))
	   (let ((tem *evalhook*)
		 (*evalhook* nil)
		 (*applyhook* nil))
	     (with-current-interpreter-environment (env)
	       (funcall tem form env))))
	  ((symbolp form)
	   (symbol-value form))
	  ((atom form) form)
	  (t
	   (eval1 form)))))
))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.93 at 13-Feb-85 04:27:23
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun function (&quote function)
  "Quotes FUNCTION for use as a function.
If FUNCTION is a symbol, its function definition in the current environment is returned.
If FUNCTION is a list (presumably starting with LAMBDA or some lambda-macro),
 the compiler will compile it; the interpreter will make it into a closure
 that records the lexical variables of the current lexical context."
  (cond ((symbolp function)
	 (if (eq *interpreter-function-environment* t)
	     (symbol-function function)
	     (interpreter-fsymeval function)))
	((functionp function t)
	 (if (eq *interpreter-function-environment* t)
	     function
	   (interpreter-enclose function)))
	((validate-function-spec function)	;Function spec
	 (fdefinition function))
	(t (ferror nil "~S is neither a function nor the name of a function" function))))

(defun lambda (&quote &rest cruft)
  "Same as (FUNCTION (LAMBDA . CRUFT))
Encloses a lambda-expression in the current environment"
  (declare (zwei:indentation 1 1))
  (interpreter-enclose `(lambda . ,cruft)))

(defun interpreter-enclose (function)
  "Close over FUNCTION in the interpreter's current lexical environment"
  (let ((*interpreter-variable-environment*
	  (unstackify-environment *interpreter-variable-environment*))
	(*interpreter-function-environment*
	  (unstackify-environment *interpreter-function-environment*))
	(*interpreter-frame-environment*
	  (unstackify-environment *interpreter-frame-environment*)))
    (closure '(*interpreter-variable-environment*
		*interpreter-function-environment*
		*interpreter-frame-environment*)
	     function)))

))

; From file OZ:OZ:<MLY.LL>QCLUKE.LISP.2 at 13-Feb-85 09:08:46
#10R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defun (:property lambda cw-handler) (exp)
  (if *cw-return-expansion-flag*
      `(function ,(cw-lambda-expression (cadr exp)))
    (cw-lambda-expression (cadr exp))))

))

; From file OZ:OZ:<MLY.LL>QCP1.LISP.1 at 13-Feb-85 09:12:45
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (:PROPERTY LAMBDA P1) (FORM)
  (BREAKOFF FORM))

))


; From file OZ:KANSAS:<L.SYS2>DEFMAC.LISP.79 at 13-Feb-85 05:59:13
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

(DEFVAR *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*)
(forward-value-cell 'optional-specified-flags '*defmacro-optional-specified-flags*)

(DEFUN DEFMACRO1 (X TYPE &OPTIONAL ENV)
  (LET (*VARLIST*
	*VALLIST*
	*DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
	DEFMACRO-&BODY-FLAG
	(ARGLIST (CADR X)))
    (WHEN (EQ (CAR-SAFE ARGLIST) '&WHOLE)
      (SETQ ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
	   (MIN-ARGS (CAR ARGS-DATA))
	   (OPT-ARGS (CADR ARGS-DATA)))
      `(,TYPE ,(STANDARDIZE-FUNCTION-SPEC (CAR X))
	. ,(LAMBDA-EXP-ARGS-AND-BODY
	     (EXPAND-DEFMACRO X ENV
			      `((ARGLIST . ,(LOOP FOR TAIL ON (CADR X)
						  WHEN (AND (ATOM TAIL) TAIL)
						    RETURN (CADR X)
						    UNTIL (EQ (CAR TAIL) '&AUX)
						    ;; user doesn't want to see these
						    WHEN (MEMQ (CAR TAIL)
							       '(&ENVIRONMENT &WHOLE))
						      DO (SETQ TAIL (CDR TAIL))
						      ELSE COLLECT (CAR TAIL)))
				,@(IF DEFMACRO-&BODY-FLAG
				      `((ZWEI:INDENTATION ,(+ MIN-ARGS OPT-ARGS) 1))))))))))

;;; X is the cdr of the DEFMACRO form.
;;; Return a LAMBDA expression for the expander function.
(DEFUN EXPAND-DEFMACRO (X ENV &OPTIONAL EXTRA-DECLARATIONS)
  (LET (*VARLIST*
	*VALLIST*
	*DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
	DEFMACRO-&BODY-FLAG
	(ARGLIST (CADR X))
	WHOLE-ARG-DATA)
    (WHEN (EQ (CAR-SAFE ARGLIST) '&WHOLE)
      (SETQ WHOLE-ARG-DATA `((,(CADR ARGLIST) *MACROARG*))
	    ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
	   (MIN-ARGS (CAR ARGS-DATA))
	   (MAX-ARGS (CADDR ARGS-DATA))
	   (BODY (CDDR X))
	   DOC-STRING DECLS)
      (MULTIPLE-VALUE-SETQ (BODY DECLS DOC-STRING)
	(EXTRACT-DECLARATIONS BODY NIL T ENV))
      (SETQ DECLS (NCONC DECLS EXTRA-DECLARATIONS))
;>> This is pretty bogus. Probably should be flushed.
;      (SETQ DECLS (SUBSET #'(LAMBDA (DECL)
;			      (MEMQ (CAR-SAFE DECL) '(ARGLIST
;						      VALUES RETURN-LIST
;						      ZWEI:INDENTATION
;						      IGNORE)))
;			   DECLS))
      (IF DOC-STRING (PUSH `(DOCUMENTATION ,DOC-STRING) DECLS))
      `(NAMED-LAMBDA ,(CAR X) (*MACROARG* &OPTIONAL *MACROENVIRONMENT*)
	,@(IF DECLS `((DECLARE . ,DECLS)))
	*MACROENVIRONMENT*			; Ok not to refer to it.
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
	(LET* (,@*DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
	       ,@WHOLE-ARG-DATA
	       . ,(MAPCAR #'LIST (NREVERSE *VARLIST*) (NREVERSE *VALLIST*)))
	  ,@(IF DECLS `((DECLARE . ,DECLS)))
	  . ,BODY)))))

))

; From file OZ:OZ:<MLY.LL>QCLUKE.LISP.2 at 13-Feb-85 06:49:34
#10R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defprop locally cw-eval-args cw-handler)

))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.3 at 13-Feb-85 06:51:18
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defcompiler-synonym locally	progn)

))

; From file OZ:OZ:<MLY.LL>SETF.LISP.2 at 13-Feb-85 06:52:30
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; SETF  "

(define-setf-method locally (&environment environment &rest forms)
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method-multiple-value (car (last forms)) environment)
    (values tempvars tempargs storevars
	    `(locally ,@(butlast forms) ,storeform)
	    `(locally ,@(butlast forms) ,refform))))

(deflocf locally (&rest forms)
  (let ((new-body (copylist forms)))
    (setf (car (last new-body))
	  `(locf ,(car (last new-body))))
    `(locally . ,new-body)))

))


; From file OZ:KANSAS:<L.SYS>EVAL.LISP.93 at 13-Feb-85 06:56:31
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun locally (&quote &rest body)
  "Common Lisp local declaration construct.
LOCALLY is like PROGN except that Common Lisp says that declarations
are allowed only in LOCALLY, not in PROGN, and because PROGN is treated
specially as a top-level form by the compiler."
  (declare (zwei:indentation 0 1))
  (gobble-declarations-from-body (vars body)
    (eval-body body)))

))

; From file OZ:OZ:<MLY.LL>QCFILE.LISP.2 at 13-Feb-85 08:38:07
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFVAR *EVAL-WHEN-EVAL-COMPILE-LOAD-FUNCTIONS*
       '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
	 EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT DEFF-MACRO
	 REQUIRE)
  "Any forms starting with one of these symbols acts as though it had a
/"(EVAL-WHEN (EVAL LOAD COMPILE) ...)/" wrapped around it when at top level in the compiler.
To prevent such a form from being evaluated at say time, use an additional EVAL-WHEN")

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN &OPTIONAL COMPILE-TIME-TOO (TOP-LEVEL-P T))
  (PROG (FN (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	(())
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM COMPILE-DRIVER NIL))
      (IF (ATOM FORM) (RETURN NIL))
      (SETQ NFORM
	    (WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S at top level"
			     (CAR FORM))
;>> Need current compilation macroenvironment!
;>> this presently uses the declared-definition crock implicitly within mexcrexpand-1. Bletch.
	      (MACROEXPAND-1 FORM)))
      (IF (EQ FORM NFORM) (RETURN)
	(SETQ FORM NFORM)))
    ;; If this was a top-level macro, supply a good guess
    ;; for the function-parent for any DEFUNs inside the expansion.
    (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (COND ((ATOM FORM))
	    ((AND (NEQ FORM OFORM) (SYMBOLP (CADR OFORM)))
	     (PUSH `(FUNCTION-PARENT ,(CADR OFORM)) LOCAL-DECLARATIONS))
	    ((EQ (CAR OFORM) 'DEFSTRUCT)
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (AND (CONSP FORM)
	   (NEQ (CAR FORM) 'EVAL-WHEN)
	   COMPILE-TIME-TOO
	   (FUNCALL PROCESS-FN FORM 'DECLARE))
      (COND ((ATOM FORM)
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ (SETQ FN (CAR FORM)) 'EVAL-WHEN)
	     (OR (AND (CLI:LISTP (CADR FORM))
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE))))
		 (FERROR NIL "~S invalid ~S times;
must be a list of ~S, ~S, and//or ~S."
			     (CADR FORM) 'EVAL-WHEN 'EVAL 'LOAD 'COMPILE))
	     (LET* ((COMPILE (MEMQ 'COMPILE (CADR FORM)))
		    (LOAD (MEMQ 'LOAD (CADR FORM)))
		    (EVAL (MEMQ 'EVAL (CADR FORM)))
		    (EVAL-NOW (OR COMPILE (AND COMPILE-TIME-TOO EVAL))))
	       (DOLIST (FORM1 (CDDR FORM))
		 (IF LOAD
		     (IF EVAL-NOW
			 (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN T NIL)
		       (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN NIL NIL))
		   (IF EVAL-NOW
		       (FUNCALL PROCESS-FN FORM1 'DECLARE))))))
	    ((EQ FN 'DEFF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ FN 'DEF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL))
		   (CDDR FORM)))
	    ((EQ FN 'WITH-SELF-ACCESSIBLE)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL))
		   (CDDR FORM)))
	    ((EQ FN 'PROGN)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
		   (CDR FORM)))
	    ((MEMQ FN '(MACRO DEFSUBST))
	     (FUNCALL PROCESS-FN FORM 'MACRO))
	    ((AND TOP-LEVEL-P
		  (MEMQ FN *EVAL-WHEN-EVAL-COMPILE-LOAD-FUNCTIONS*))
	     (FUNCALL PROCESS-FN FORM 'SPECIAL))
	    ((EQ FN 'DECLARE)
	     (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	    ((EQ FN 'PROCLAIM)
	     (COMPILE-PROCLAIM (CDR FORM) PROCESS-FN))
	    ((EQ FN 'COMMENT) NIL)
	    ((EQ FN 'PATCH-SOURCE-FILE)
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
		   (CDDR FORM))
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING NIL))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
	    ((EQ FN 'COMPILER-LET)
	     (PROGW (CADR FORM)
	       (COMPILE-DRIVER `(PROGN . ,(CDDR FORM))
			       PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T)))
	    ((EQ FN 'DEFUN)
	     (LET (TEM)
	       (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed DEFUN")
		 (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
	       (COND ((EQ (CDR TEM) (CDR FORM))
		      (IF (NULL (CDDR TEM))
			  (COMPILER-WARN 'MALFORMED-DEFUN ':IMPOSSIBLE
					 "Malformed defun ~S" FORM)
			(FUNCALL PROCESS-FN FORM 'DEFUN)))
		     (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL)))))
	    ((EQ FN 'LOCALLY)
	     (MULTIPLE-VALUE-BIND (BODY LOCAL-DECLARATIONS)
;>>must pass in environment here!!
		 (SI:EXTRACT-DECLARATIONS (CDR FORM) LOCAL-DECLARATIONS NIL)
	       (COMPILE-DRIVER `(PROGN . ,BODY)
			       PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T)))
	    (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))
))

; From file OZ:KANSAS:<L.ZWEI>INDENT.LISP.105 at 13-Feb-85 07:56:44
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; INDENT  "

(DEFCONST *COND-CLAUSE-SUPERIORS*
	  '(COND SELECT SELECTQ SELECTOR COND-EVERY SELECTQ-EVERY SELECT-MATCH
	    CASE CASEQ TYPECASE CTYPECASE ETYPECASE CCASE ECASE
	    CONDITION-CASE CONDITION-CASE-IF CONDITION-CALL CONDITION-CALL-IF
	    WITH-OPEN-FILE-CASE WITH-OPEN-STREAM-CASE
	    SIGNAL-PROCEED-CASE UNWIND-PROTECT-CASE FS:READING-FROM-FILE-CASE)
  "Functions whose arguments should be indented internally as clauses.")

))

; From file OZ:KANSAS:<L.ZWEI>INDENT.LISP.105 at 13-Feb-85 08:13:01
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; INDENT  "

(DEFUN INDENT-FOR-LISP (BP &OPTIONAL START-DEFUN-BP
	        	   &AUX BP1 BP2 INDENTATION OFFSET SYM
	        	   TEM SPACE-WIDTH NSEXPS LASTPAREN LASTSEXP IN-STRING)
  "Returns the indentation in pixels BP's line should have, for Lisp indent.
Second value is non-NIL if the line starts inside a string,
in which case the caller may decide not to reindent it at all.
START-DEFUN-BP is a BP to start parsing from.  Presumably before BP's line.
*LISP-INDENT-OFFSET* is the amount to offset if there isnt a complete sexp on another line
*LISP-DEFUN-INDENTION* is the amount to indent for top-level forms
*LISP-INDENT-OFFSET-ALIST* is an alist of the form ((FUNCTION . OFFSET-LIST) ...)
OFFSET-LIST is a list specifying (number-of-sexps-to-skip amount-to-change-indentation ...)
or if OFFSET-LIST is a symbol or function, it is funcall'ed and can return
the indentation, an offset, or a bp whose indentation to use."
  (DECLARE (VALUES LINE-INDENTATION-IN-PIXELS START-OF-LINE-IN-STRING-P))
  (SETQ BP (CREATE-BP (BP-LINE BP) 0)
	BP1 (OR START-DEFUN-BP (SETQ START-DEFUN-BP (FORWARD-DEFUN BP -1 T))))
  (SETQ IN-STRING (LISP-PARSE-FROM-DEFUN (BP-LINE BP) BP1))
  (IF IN-STRING
      (VALUES 0 IN-STRING)
    ;; Get BP to last unterminated paren (up one level).  Sixth argument of NIL makes
    ;; sure we get an open paren and not a single-quote (forward or backward).
    (SETQ LASTPAREN (FORWARD-SEXP BP -1 NIL 1 BP1 NIL))
    ;; Get BP to start of last complete sexp, or NIL if none at this level.
    (SETQ LASTSEXP (FORWARD-SEXP BP -1 NIL 0 BP1))
    (AND LASTPAREN LASTSEXP (BP-= LASTSEXP LASTPAREN) (SETQ LASTSEXP NIL))
    (SETQ OFFSET 0
	  SPACE-WIDTH (FONT-SPACE-WIDTH))
    (AND LASTPAREN                  ;Try to find the indentation for the current function
	 (LET ((BP2 (FORWARD-CHAR LASTPAREN)))
	   (LET ((I (BP-INDEX BP2)))
	     (SETQ SYM (DO ((J I (1+ J))
			    (LINE (BP-LINE BP2))
			    (LENGTH (LINE-LENGTH (BP-LINE BP2))))
			   ((OR ( J LENGTH)
				(AND ( (LIST-SYNTAX (CHAR LINE J)) LIST-ALPHABETIC)
				     ( (LIST-SYNTAX (CHAR LINE J)) LIST-COLON)))
			    (AND ( I J)
				 (CATCH-ERROR (CLI:READ-FROM-STRING
						(STRING-REMOVE-FONTS (NSUBSTRING LINE I J))
						NIL NIL)
					      NIL)))))
	     ;; Beware of funny read syntax, numbers, etc.
	     (OR (SYMBOLP SYM) (SETQ SYM NIL))))
	 (SETQ TEM (GET-FUNCTION-LISP-INDENTATION SYM))	 
	 ;; This function on the alist => value is either
	 ;; an indentation list or a function to call.
	 (COND ((CONSP TEM)         ;Indentation list, see how do handle this depth
		;; How many sexps at this level precede point?  Set NSEXPS.
		;; But, first, let's see how many are interesting (that's (1- MAX-I) ).
		;; Don't keep counting NSEXPS when it's already larger than is interesting.
		(DO ((BP3 (FORWARD-CHAR LASTPAREN 1) (FORWARD-SEXP BP3 1 NIL 0 BP))
		     (MAX-I (1+ (CAR (NLEFT 2 TEM))))
		     (I 0 (1+ I)))
		    ((NULL BP3) (SETQ NSEXPS (- I 2)))
		  (AND (> I MAX-I) (RETURN NIL)))
		;; Now see what the indentation lists says about that many sexps.
		(AND NSEXPS
		     (DO ((L TEM (CDDR L))
			  (I 0))
			 ((OR (NULL L) (> I NSEXPS)))
		       (AND (= (SETQ I (CAR L)) NSEXPS)
			    (SETQ OFFSET (CADR L) LASTSEXP NIL)))))
	       (T
		(MULTIPLE-VALUE-SETQ (BP2 INDENTATION OFFSET)
		  ;; funcalled with arguments:
		  ;;  bp to start of current defun
		  ;;  bp to start of line (the one we are indenting)
		  ;;  bp to last unterminated paren (up one level) (such as "(PROG ...)")
		  ;;  bp to start of last complete sexp, or NIL if none at this level.
		  ;;  space width of current font
		  ;;  symbol at start of sexp (such as PROG)
		  ;; values returned:
		  ;;  bp to which to indent, or NIL
		  ;;   (may be affected by OFFSET, third value returned)
		  ;;  explicit indentation in pixels desired, or NIL.
		  ;;   Overrides any other values
		  ;;  offset from first first value (in space-widths) to indent, or 0
		  ;;   (eg indent 3 spaces from bp to start of tag in a tagbody to get
		  ;;    indentation for start of this sexp)
		  (FUNCALL TEM BP1 BP LASTPAREN LASTSEXP SPACE-WIDTH SYM)))))
    (SETQ BP1 (DO-FOREVER
		(COND ((NULL LASTPAREN)	        ;If already balanced, nothing to do
		       (RETURN BP))
		      (BP2	        	;Specified what to indent to
		       (RETURN BP2))
		      (INDENTATION)	        ;Specified how far to indent
		      ;; If there is no complete sexp at this paren depth, line up just after
		      ;; the leftparen.
		      ((OR (NULL LASTSEXP) (BP-< LASTSEXP LASTPAREN))
		       (RETURN (FORWARD-CHAR LASTPAREN)))
		      (T
		       (SETQ BP1 (CREATE-BP (BP-LINE LASTSEXP) 0))
		       ;; If complete sexp is on different line than the unmatched leftparen,
		       ;; line up with start of sexp's line.
		       (COND ((OR (NULL LASTPAREN) (BP-< LASTPAREN BP1))
			      (SETQ BP1 (FORWARD-OVER *BLANKS* BP1))
			      ;;OK only if the first on the line or at that level.
			      ;; If LASTSEXP is first nonblank thing on its line, use it.
			      ;; Also if there are no unmatched close parens preceding it,
			      ;; use the first nonblank thing on that line
			      ;; since that must be at LASTSEXP's level.
			      (AND (OR (BP-= BP1 LASTSEXP)
				       (NOT (FORWARD-SEXP BP1 1 NIL 1 LASTSEXP)))
				   (RETURN BP1))
			      ;; LASTSEXP follows on the same line as an unmatched close.
			      ;; Back up one sexp from it.
			      ;; Eventually this moves back across that unmatched close
			      ;; and the sexp that it terminates, to another line.
			      (SETQ LASTSEXP (FORWARD-SEXP LASTSEXP -1 NIL))
;	        	            LASTPAREN (FORWARD-SEXP LASTSEXP -1 NIL 1 LASTPAREN NIL)
;	        	            LASTSEXP (FORWARD-SEXP LASTSEXP -1 NIL 0 LASTPAREN))
			      )
			     ;;Otherwise, maybe user specified how to handle this case
			     (*LISP-INDENT-OFFSET*
			      (SETQ OFFSET (+ *LISP-INDENT-OFFSET* OFFSET))
			      (RETURN (FORWARD-CHAR LASTPAREN)))
			     ;;If only one element in list so far, line up under left-paren
			     ;;also if the CAR doesnt look like the name of a function
			     ((SETQ TEM (INDENT-NOT-FUNCTION-P
					  LASTPAREN
					  (SETQ BP2 (FORWARD-CHAR LASTPAREN))
					  START-DEFUN-BP))
			      (IF (NUMBERP TEM) (SETQ OFFSET TEM))
			      (RETURN BP2))
			     ((BP-< LASTSEXP (SETQ BP1 (FORWARD-SEXP BP2)))
			      (SETQ OFFSET
				    (IF (COND-CLAUSE-SUPERIOR-P LASTPAREN START-DEFUN-BP)
					0
				      *LISP-INDENT-LONE-FUNCTION-OFFSET*))
			      (RETURN BP2))
			     ;; Otherwise line up with start of the second elt of that list
			     (T
			      (RETURN (SKIP-OVER-BLANK-LINES-AND-COMMENTS
					(SKIP-OVER-BLANK-LINES-AND-COMMENTS BP1)))))))))
    (OR INDENTATION
	(SETQ INDENTATION (MAX 0 (+ (* OFFSET SPACE-WIDTH) (BP-INDENTATION BP1)))))
    (VALUES INDENTATION IN-STRING)))

(DEFUN GET-FUNCTION-LISP-INDENTATION (SYM &AUX TEM)
  (COND ((AND (FBOUNDP SYM)
	      (SETQ TEM (CDR (ASSQ 'INDENTATION (DEBUGGING-INFO SYM)))))
	 (AND (CONSP TEM)
	      (= (LENGTH TEM) 1)
	      (SETQ TEM (CAR TEM)))
	 TEM)
	((CDR (ASSQ SYM *LISP-INDENT-OFFSET-ALIST*)))
	((STRING= SYM "DEF" :END1 3)
	 *LISP-DEFUN-INDENTATION*)
	(T
	 NIL)))

))


; From file OZ:OZ:<MLY.LL>SETF.LISP.3 at 13-Feb-85 08:43:13
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; SETF  "

(defmacro setf (&environment environment &rest places-and-values)
  "Sets the value of PLACE to be VALUE.  Allows any number of places and values, like SETQ.
For example, (SETF (AREF A I) NEWVALUE) sets the value of (AREF A I)."
  (declare (arglist place value ...))
  (if (oddp (length places-and-values)) (ferror nil "Odd number of arguments to ~S" 'setf))
  `(progn
     . ,(loop for (place value) on places-and-values by 'cddr
	      collect
	      (multiple-value-bind (tempvars tempargs storevars storeform)
		  (get-setf-method-multiple-value place environment :short-cut t)
		(if (and tempvars (symbolp tempvars))
		    ;; Handle case of simple DEFSETF as fast as possible.
		    `(,tempvars ,@(cdr tempargs) ,value)
		  (sublis-eval-once (pairlis tempvars tempargs
					     (list (cons (car storevars) value)))
				    storeform t t environment))))))

))

; From file OZ:KANSAS:<L.ZWEI>PATED.LISP.32 at 14-Feb-85 21:14:49
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFCOM COM-FINISH-PATCH "Finish off the current patch file.
Writes out the patch buffer, compiles it, and marks it as finished and released
so that LOAD-PATCHES will load it.  See also Finish Patch Unreleased." ()
  (FINISH-PATCH
    (OR *NUMERIC-ARG-P*
	(FQUERY ()
	   "Release this patch? (answer N if you have not completely sure that it works) ")))
  DIS-NONE)

))

; From file OZ:OZ:<MLY.LL>QRAND.LISP.4 at 14-Feb-85 23:42:29
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ:OZ:<MLY.LL>QRAND.."

(DEFUN SETPLIST (SYMBOL L)
  "Set the property list of SYMBOL to be L (a list of alternating properties and values).
SYMBOL may be an instance that handles the :SET-PROPERTY-LIST operation, instead of a symbol."
  (ETYPECASE SYMBOL
    (SYMBOL 
     (SETF (CONTENTS (LOCF (SYMBOL-PLIST SYMBOL))) L))
    ((OR INSTANCE NAMED-STRUCTURE)
     (SEND SYMBOL :SET-PROPERTY-LIST L)))
  L)

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.94 at 17-Feb-85 03:06:51
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro bash-to-current-interpreter-environment (env)
  "Make ENV be the current interpreter environment."
  (once-only (env)
    `(setf (interpreter-environment-variables ,env) *interpreter-variable-environment*
	   (interpreter-environment-functions ,env) *interpreter-function-environment*
	   (interpreter-environment-frames ,env) *interpreter-frame-environment*)))

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.94 at 17-Feb-85 03:07:14
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun interpreter-external-value-cell (symbol &aux mumble)
  (do ((tail *interpreter-variable-environment* (cdr tail)))
      ((atom tail)				;assume free references are special
       (or tail (getl symbol '(special system-constant))
	   (var-not-special symbol))
       (%external-value-cell symbol))
    (and (setq mumble (get-lexical-value-cell (car tail) (locf (symbol-value symbol))))
	 (return (follow-cell-forwarding mumble t)))))

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.94 at 17-Feb-85 03:07:53
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro interpreter-variable-special-in-frame-p (cell frame &optional default defaultp)
  `(let ((mumble (compiler::undefined-value)))
     (setq mumble (get-lexical-value-cell (car ,frame) ,cell))
     (cond (mumble
	    (= (%p-data-type mumble) dtp-one-q-forward))
	   (,defaultp
	    ,default)
	   (t (cadr (getl (%find-structure-header ,cell) '(special system-constant)))))))

))

; From file OZ:KANSAS:<L.SYS2>LMMAC.LISP.384 at 17-Feb-85 04:23:30
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST LEXPR-SEND (OBJECT OPERATION &REST ARGS)
  "Send a message to OBJECT, with operation OPERATION and ARGUMENTS.
The last one of ARGUMENTS actually is a list of arguments, not one argument."
  (APPLY OBJECT OPERATION ARGS))

(DEFSUBST %POINTER-LESSP (PTR1 PTR2)
  "T if PTR1 points to a lower memory address than PTR2"
  (PLUSP (%POINTER-DIFFERENCE PTR1 PTR2)))

))

; From file OZ:OZ:<MLY.LL>QMISC.LISP.2 at 18-Feb-85 01:14:08
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN %POINTER-UNSIGNED (N)
  "Convert the fixnum N, regarded as unsigned number, into number (maybe big) with same value.
If the argument is negative (if regarded as signed), it is expanded into a bignum."
  (IF (MINUSP N)
      (- N MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-FIXNUM)
      N))

(DEFUN %MAKE-POINTER-UNSIGNED (N)
  "Convert N to a fixnum which, regarded as unsigned, has same value as N.
Thus, a number just too big to be a signed fixnum
becomes a fixnum which, if regarded as signed, would be negative."
  (IF (FIXNUMP N)
      N
      (+ N MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-FIXNUM)))

))

; From file OZ:KANSAS:<L.SYS2>ADVISE.LISP.38 at 18-Feb-85 02:49:07
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defun (:property advise encapsulation-grind-function) (function def width real-io untyo-p)
  (when def					; Print the advice as calls to advise.
    (if (typep def 'compiled-function)
	(setq def (cadr (assq 'interpreted-definition (debugging-info def)))))
    (let ((body (encapsulation-body def)))
      (when (eq (car (car body)) 'si:displaced)
	(setf (car body) (cadr (car body))))
      (grind-print-advice-slot (cadr (car body)) ':before
			       function width real-io untyo-p)
      (grind-print-advice-slot (caddr (car body)) ':after
			       function width real-io untyo-p)
      (grind-print-advice-slot (cadddr (car body)) ':around
			       function width real-io untyo-p))))

))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.5 at 18-Feb-85 03:01:42
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer arith-opt-non-associative char-bits)	; a little too far...
(defoptimizer arith-opt-non-associative char-font)	;

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.95 at 18-Feb-85 06:11:15
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro parallel-function-binding-list ((varlist enclose macroflag) &body body)
  `(prog (vars-left bindframe)
	 ;; Trivial case of empty varlist would lose in code below.
	 (unless ,varlist
	   (go long))  
	 (when (nthcdr #o20 ,varlist)
	   (setq bindframe
		 (mapcan #'(lambda (var)
			     (list* (locf (symbol-function (car var)))
				    ,(cond (macroflag
					    ``(macro . ,(with-stack-list (env *interpreter-function-environment*)
							  (expand-defmacro var env))))
					   (enclose
					    `(interpreter-enclose `(lambda . ,(cdr var))))
					   (t ``(lambda . ,(cdr var))))
				    nil))
			 ,varlist))
	   (go long))
	 ;; The following code is equivalent to the above mapcar
	 ;; except that the list is constructed on the stack
	 ;; by pushing the elements one by one and fiddling with cdr codes.
	 (with-stack-list (tem nil)
	   ;; BINDFRAME gets a pointer to where the list will go.
	   (setq bindframe tem))
	 ;; Now loop over the varlist, computing and pushing initial values.
	 (setq vars-left ,varlist)
      short-nextvar
	 (when vars-left
	   (%push (locf (symbol-function (caar vars-left))))
	   (%push ,(cond (macroflag
			  ``(macro . ,(with-stack-list (env *interpreter-function-environment*)
					(expand-defmacro (car vars-left) env))))
			 (enclose
			  `(interpreter-enclose `(lambda . ,(cdar vars-left))))
			 (t
			  ``(lambda . ,(cdar vars-left)))))
	   (pop vars-left)
	   (go short-nextvar))
	 ;; Modify cdr-code of last word pushed, to terminate the list.
	 (with-stack-list (tem nil)
	   (%p-dpb-offset cdr-nil %%q-cdr-code tem -1))
      long
	 ;; Here BINDFRAME has the correct variables and values.
	 (return
	   (with-stack-list* (*interpreter-function-environment*
			       bindframe *interpreter-function-environment*)
	     . ,body))))

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.95 at 18-Feb-85 06:11:29
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun flet (&quote function-list &rest body)
  "Execute BODY with local function definitions as per FUNCTION-LIST.
Each element of FUNCTION-LIST looks like (NAME (ARGS...) BODY...).
FLET rebinds the function definition of each NAME lexically to
 (LAMBDA (ARGS...) BODY...), closed in the environment outside the FLET.
See also LABELS."
  (declare (zwei:indentation 1 1))
  (if (eq *interpreter-function-environment* t)
      (zl-parallel-function-binding-list (function-list nil nil)
	(eval-body body))
    (gobble-declarations-from-body (vars body)
      (parallel-function-binding-list (function-list t nil)
	(eval-body body)))))

(defun macrolet (&quote macro-list &rest body)
  "Execute BODY with macro function definitions as per MACRO-LIST.
Each element of MACRO-LIST looks like (NAME (ARGS...) BODY...).
MACROLET rebinds the function definition of each NAME lexically to
 a macro like the one you would get by doing
 (DEFMACRO NAME (ARGS...) BODY...)."
  (declare (zwei:indentation 1 1))
  (if (eq *interpreter-function-environment* t)
      (zl-parallel-function-binding-list (macro-list t t)
	(eval-body body))
    (gobble-declarations-from-body (vars body)
      (parallel-function-binding-list (macro-list t t)
	(eval-body body)))))

(defun labels (&quote function-list &rest body)
  "Execute BODY with local function definitions as per FUNCTION-LIST.
Each element of FUNCTION-LIST looks like (NAME (ARGS...) BODY...).
LABELS rebinds the function definition of each NAME lexically to
 (LAMBDA (ARGS...) BODY...), closed in the environment inside the LABELS.
This means that the functions defined by the LABELS can refer to
themselves and to each other.  See also FLET."
  (declare (zwei:indentation 1 1))
  (if (eq *interpreter-function-environment* t)
      (zl-parallel-function-binding-list (function-list nil nil)
	(eval-body body))
    (gobble-declarations-from-body (vars body)
      (parallel-function-binding-list (function-list nil nil)
	;; The values were not evaluated yet.
	;; The binding frame contains the expressions.
	;; Eval them now and store the values in their places.
	(do ((frametail (car *interpreter-function-environment*) (cddr frametail)))
	    ((null frametail))
	  (setf (cadr frametail) (interpreter-enclose (cadr frametail))))
	(eval-body body)))))
))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.6 at 18-Feb-85 10:10:43
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(make-obsolete get-list-pointer-into-array "this is crufty and obsolete")
(make-obsolete get-locative-pointer-into-array "this is crufty and obsolete")

))
