;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 12/08/83 17:45:32 by RMS,
;;; Reason: Fix sectionization: only one form per section.
;;; Compiling just part of a function doesn't mark it "compiled".
;;; &ENVIRONMENT in DEFMACRO.  Accompanying changes in evaluator, macroexpand, setf, step.
;;; Make DEFMACRO-DISPLACE same as DEFMACRO, etc.
;;; :NO-ERROR clauses in CONDITION-CALL.
;;; M-X Macro Expand All bug.
;;; ZWEI:READ-SYSTEM-NAME completion accepts nicknames.
;;; Crock demo bug.  CHAOS:RESET-SAVED-HOST-LISTS bug.
;;; Make FC:'s nonstandard open keywords work over chaosnet.
;;; Accept pathnames starting with colon meaning always default the host.
;;; Fix :TEXT and :MIDAS canonical types.
;;; MOUSE-SENSITIVE-ITEMS informs mouse process of changed mouse sensitivity.
;;; COLOR-DRAW-LINE bug.
;;; while running on Lisp Machine One from band 5
;;; with Experimental System 98.1, CADR 3.0, Experimental ZMail 53.0, MIT-Specific 22.0, microcode 305, ZM MIT.


(globalize 'si:&environment)
(push '&environment lambda-list-keywords)

; From file CROCK.LISP SRC:<L.DEMO> OZ:
#8R HACKS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: DEMO; CROCK  "

(defun crock ()
  (or (boundp '*crock*)
      (let ((width (tv:sheet-inside-width tv:mouse-sheet))
	    (height (tv:sheet-inside-height tv:mouse-sheet)))
	(setq *crock* (tv:make-window 'crock-window 
				      ':width (- width 40) ':left 20
				      ':height (- height 200) ':top 100))))
  (funcall *crock* ':select))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN MACRO-EXPAND-ALL (FORM)
  (IF (AND (CONSP FORM)
	   (CAR FORM)
	   (OR (NOT (SYMBOLP (CAR FORM)))
	       (FDEFINEDP (CAR FORM))))
      ;; if there is a compiler-let in the form being expanded, do that let
      (IF (MEMQ (CAR FORM) '(COMPILER-LET :COMPILER-LET))
	  (SETQ FORM `(COMPILER-LET ,(CADR FORM)
			,@(EVAL `(LET ,(CADR FORM)
				   (MACRO-EXPAND-ALL ',(CDDR FORM))))))
	(SETQ FORM (MACROEXPAND FORM))))
  (AND (CONSP FORM)
       (NOT (MEMQ (CAR FORM) '(QUOTE :QUOTE)))	; modest beginning ...
       (SETQ FORM (COPYLIST FORM))
       (DO ((L FORM (CDR L)))
	   ((ATOM L))
	 (SETF (CAR L) (MACRO-EXPAND-ALL (CAR L)))))
  FORM)

))

(globalize "UNDEFFLAVOR")

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFUN RESET-SAVED-HOST-LISTS (&OPTIONAL (DO-ALL-HOSTS T) DO-LOCAL-LISPMS DO-ALL-LISPMS)
  "Sets the magic variables that contain a list of all hosts for efficiency."
  (COND (DO-ALL-HOSTS
	 (SETQ *SAVED-ALL-CHAOS-HOSTS* NIL)  ;;reset
    ;;code is from old hostat
      ;; use SI:PARSE-HOST because it's faster on host objects
	 (DOLIST (H SI:HOST-ALIST)
	   (AND (OR (SECOND H) (SI:PARSE-HOST (FIRST H)))
		(SECOND H) ;;don't funcall NIL
		(FUNCALL (SECOND H) ':NETWORK-TYPEP ':CHAOS)
		(PUSH (SECOND H) *SAVED-ALL-CHAOS-HOSTS*)))))
  (WHEN DO-LOCAL-LISPMS
    (SETQ *SAVED-LOCAL-LISPMS* NIL)
    (DOLIST (ELEM SI:MACHINE-LOCATION-ALIST)
      (LET ((TEM (SI:PARSE-HOST (CAR ELEM) T)))
	(WHEN TEM (PUSH TEM *SAVED-LOCAL-LISPMS*))))
    (SETQ *SAVED-LOCAL-LISPMS* (NREVERSE *SAVED-LOCAL-LISPMS*)))
  (WHEN DO-ALL-LISPMS
    (SETQ *SAVED-ALL-LISPMS* (LIST-ALL-NET-MACHINES ':LISPM)))) ;;already parsed

(reset-saved-host-lists nil t nil)

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defun get-setf-method-multiple-value (form &optional short-cut &aux tem)
  "Return the canonical five values that say how to do SETF on FORM.
The values are:
* a list of symbols, gensyms, that stand for parts of FORM
* a list of the parts of FORM that they stand for
* a list of symbols, gensyms, that stand for the values to be stored
* an expression to do the storing.  It contains the gensyms described already.
* an expression to refer to the existing value of FORM.
  It differs from FORM in that it has the gensyms replacing the
  parts of FORM that they stand for.
These values give all the information needed to examine and set
 FORM repeatedly without evaluating any of its subforms more than once.

If SHORT-CUT is non-NIL, and if FORM's method of SETFing was defined
by a simple DEFSETF that just gives a function to do the setting,
then we return just two values: the setting function and a replacement FORM
/(differing from FORM by having macros expanded, CADR -> CAR (CDR ...), etc.).
The caller can tell that this case occurred because the first value
is a non-NIL symbol in this case, and is always a list in the normal case."
  (declare (values tempvars tempargs storevars storeform refform))
  (cond ((symbolp form)
	 (let ((g (gensym)))
	   (values nil nil (list g) `(setq ,form ,g) form)))
	((atom form))
	((not (symbolp (car form)))
	 (ferror nil "~S non-symbolic function in SETF." (car form)))
	((eq (getdecl (car form) 'setf) 'unsetfable)
	 (ferror 'unknown-locf-reference
		 "LOCF is explicitly forbidden on ~S." (car form)))
	((setq tem (getdecl (car form) 'setf-method))
	 (if (symbolp tem)
	     (if short-cut
		 (values tem form)
	       (let ((gs (mapcar #'(lambda (ignore) (gensym)) (cdr form)))
		     (g (gensym)))
		 (values gs (cdr form) (list g)
			 `(,tem ,@gs ,g)
			 `(,(car form) ,@gs))))
	   (call (cdr tem) () form ':optional *macroexpand-environment*)))
	((setq tem (getdecl (car form) 'setf-expand))
	 (get-setf-method-multiple-value (funcall tem form) short-cut))
	((and (fboundp (car form))
	      (arrayp (fsymeval (car form))))
	 (get-setf-method-multiple-value `(aref #',(car form) . ,(cdr form)) short-cut))
	((and (fboundp (car form))
	      (symbolp (fsymeval (car form))))
	 (get-setf-method-multiple-value `(,(fsymeval (car form)) . ,(cdr form)) short-cut))
	((not (eq form (setq form (macroexpand-1 form
						 si:*macroexpand-environment*))))
	 (get-setf-method-multiple-value form short-cut))
	(t (ferror 'sys:unknown-setf-reference
		   "No way known to do SETF of ~S." (car form)))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro locf (accessor)
  "Return a locative pointer to the place where ACCESSOR's value is stored."
  (do-forever
    (let (fcn)
      (cond ((symbolp accessor)			;SPECIAL CASE NEEDED.
	     (return `(variable-location ,accessor)))
	    ((not (symbolp (car accessor)))
	     (ferror nil "~S non-symbolic function in LOCF" (car accessor)))
	    ((eq (getdecl (car accessor) 'locf) 'unlocfable)
	     (ferror 'unknown-locf-reference
		     "LOCF is explicitly forbidden on ~S." (car accessor)))
	    ((setq fcn (getdecl (car accessor) 'locf-method))
	     (if (symbolp fcn)
		 (return (cons fcn (cdr accessor)))
	       (return (call (cdr fcn) nil accessor ':optional *macroexpand-environment*))))
	    ((setq fcn (getdecl (car accessor) 'setf-expand))
	     (setq accessor (funcall fcn accessor)))
	    ((and (fboundp (car accessor)) (arrayp (fsymeval (car accessor))))
	     (return `(aloc #',(car accessor) . ,(cdr accessor))))
	    ((and (fboundp (car accessor)) (symbolp (fsymeval (car accessor))))
	     (return `(locf (,(fsymeval (car accessor)) . ,(cdr accessor)))))
	    ((not (eq accessor (setq accessor (macroexpand-1 accessor
							     *macroexpand-environment*)))))
	    (t (ferror 'sys:unknown-locf-reference
		       "No way known to do LOCF on ~S." (car accessor)))))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


;(GET-LIST-POINTER-INTO-STRUCT (element pntr))
(defun get-list-pointer-into-struct macro (x)
  (prog (ref)
    (setq ref (macroexpand (cadr x)	;EXPAND MACROS LOOKING AT BAG-BITING MACRO LIST
			   *macroexpand-environment*))
    (cond ((eq (car ref) 'ar-1)
	   (return (list 'get-list-pointer-into-array
			 (list 'funcall (cadr ref) (caddr ref)))))
	  ((error "LOSES - GET-LIST-POINTER-INTO-STRUCT" x)))))

))

; From file STEP.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

			       ; normal printout produced unless NO-PRINT.
			       ;  User's program can turn on auto mode by
			       ;  (si:step-auto-on &optional (mode 'no-print))
			       ; and (si:step-auto-off) to reable stepping.

;Main entry point.
(defun step (form &optional step-auto &aux (step-level -1) (step-max 0)
	     (step-array (make-array 200))
	     (step-apply-p-array (make-array 200)))
  "Evaluate FORM with stepping.  It stops before and after each subexpression.
Type the Help key when you are in the stepper for a list of stepper commands."
  (with-stack-list (environment nil (if (eq interpreter-function-environment t) t))
    (step-eval form environment)))

))

; From file STEP.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "


;Main entry point.
(defun step-hook (form &optional env &aux step-auto (step-level -1) (step-max 0)
		  (step-array (make-array 200))
		  (step-apply-p-array (make-array 200)))
  (step-eval form env))

))

; From file STEP.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "


;This is evalhooked in in place of EVAL.  Works by calling step-cmdr
;to let the user see what's going on and say what to do, then continues
;evaluation using either EVAL or EVALHOOK based on what the user typed.
;Has special hair for macros and for atoms.
(defun step-eval (step-form &optional environment)
  (prog ((step-level (1+ step-level)) step-value step-values tem val)
    (when (>= step-level (ARRAY-LENGTH step-array))
      (adjust-array-size step-array (+ 100 step-level))
      (adjust-array-size step-apply-p-array (+ 100 step-level)))
 mc (as-1 step-form step-array step-level)
    (as-1 nil step-apply-p-array step-level)
    (cond ((atom step-form)
           (setq step-values (list (eval step-form)))
           (setq tem 'atom)
           (go rl))
          ((<= step-level step-max)
           (setq tem (step-cmdr step-form nil t)))
          (t (setq tem 'eval)))
    (cond ((step-macro-p step-form)
	   (setq step-form (macroexpand-1 step-form))
           (go mc))
          ((eq tem 'eval)
           (setq step-values (multiple-value-list (*eval step-form environment))))
          ((eq tem 'evalhook)
           (setq step-values (multiple-value-list (evalhook step-form #'step-eval nil
							    environment))))
	  ((eq tem 'applyhook)
	   (setq step-values (multiple-value-list (evalhook step-form nil #'step-applyhook
							    environment))))
          ((ferror nil "Unknown function ~S" tem)))
 rl (setq step-value (setq val (car step-values)))
    (cond ((<= step-level step-max)
           (setq tem (step-cmdr step-form step-values (neq tem 'eval))))
          (t (setq tem 'eval)))
    (and (neq step-value val) (return step-value))
 rt (cond ((null (cdr step-values)) (return (car step-values)))
          (t (return-next-value (car step-values))
             (setq step-values (cdr step-values))
             (go rt)))))

))

; From file STEP.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "


(defun step-applyhook (function args &optional environment
		       &aux (interpreter-environment (car environment))
		       (interpreter-function-environment (cadr environment))
		       (step-form (cons function args)))
  (prog ((step-level (1+ step-level)) step-value step-values tem val)
    (when (>= step-level (ARRAY-LENGTH step-array))
      (adjust-array-size step-array (+ 100 step-level))
      (adjust-array-size step-apply-p-array (+ 100 step-level)))
 mc (as-1 step-form step-array step-level)
    (as-1 t step-apply-p-array step-level)
    (cond ((<= step-level step-max)
           (setq tem (step-cmdr step-form nil t t)))
          (t (setq tem 'eval)))
    (cond ((eq tem 'eval)
           (setq step-values (multiple-value-list (apply (car step-form) (cdr step-form)))))
          ((eq tem 'evalhook)
           (setq step-values
		 (multiple-value-list
		   (let ((evalhook #'step-eval))
		     (apply (car step-form) (cdr step-form))))))
          ((ferror nil "Unknown function ~S" tem)))
 rl (setq step-value (setq val (car step-values)))
    (cond ((<= step-level step-max)
           (setq tem (step-cmdr step-form step-values (neq tem 'eval) t)))
          (t (setq tem 'eval)))
    (and (neq step-value val) (return step-value))
 rt (cond ((null (cdr step-values)) (return (car step-values)))
          (t (return-next-value (car step-values))
             (setq step-values (cdr step-values))
             (go rt)))))

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "


;;; Macro expansion.

;Expand any macros in top level of a form.
;MACROEXPAND-1 MACRO-CALL iteratively until it can't expand any more.
;MACROEXPAND, MACROEXPAND-1 and OPEN-CODE-P have alternate Maclisp-only versions in QCP1.
(DEFUN MACROEXPAND (MACRO-CALL &OPTIONAL ENV)
  "Expand MACRO-CALL repeatedly until the result is not a macrocall."
    (DO ((TM MACRO-CALL (MACROEXPAND-1 TM ENV))
	 (OTM NIL TM))
	((OR (EQ TM OTM) (ATOM TM)) TM)))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun cli:eval (form)
  "Evaluate FORM as Common Lisp in the global lexical environment, returning its value(s).
Free variables in FORM must be special."
  (cond (*evalhook*
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form nil)))
	((symbolp form)
	 (symeval form))
	((atom form) form)
	(t
	 (let (interpreter-environment interpreter-function-environment)
	   (eval1 form)))))

(defun eval (form &optional nohook)
  "Evaluate FORM in traditional (nonlexical) fashion, returning its value(s).
Free variables in FORM must be special."
  (cond ((and *evalhook* (not nohook))
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form '(nil t))))
	((symbolp form)
	 (symeval form))
	((atom form) form)
	(t
	 (let ((interpreter-function-environment t)
	       interpreter-environment)
	   (eval1 form)))))

(defun *eval (form &optional environment
	      &aux (interpreter-environment (car environment))
	      (interpreter-function-environment (cadr environment)))
  "Evaluate FORM as Common Lisp in the specified lexical environment, returning its value(s).
If the second environment is (NIL . T), it means to evaluate as traditional Zetalisp."
  (cond (*evalhook*
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form environment)))
	((symbolp form)
	 (if (eq interpreter-function-environment t)
	     (symeval form)
	   (if (keywordp form)
	       form
	     (interpreter-symeval form))))
	((atom form) form)
	(t (eval1 form))))

(defun eval1 (form &optional nohook)
  "Evaluate FORM in the current lexical environment, returning its value(s).
If the current environment says /"traditional Zetalisp/", we do that.
This is the function that special forms such as COND use to evaluate
their subexpressions, as it allows the subexpressions to access
lexical variables of the containing code.  Contrast with EVAL, *EVAL and CLI:EVAL."
  ;; Make sure all instances of ARGNUM, below, are local slot 0.
  (let (argnum) argnum)
  (cond ((and *evalhook* (not nohook))
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (with-stack-list (env interpreter-environment interpreter-function-environment)
	     (funcall tem form env))))
	((symbolp form)
	 (if (eq interpreter-function-environment t)
	     (symeval form)
	   (if (keywordp form)
	       form
	     (interpreter-symeval form))))
	((atom form) form)
	((eq (car form) 'quote)
	 (cadr form))
	(t (let ((fctn (car form)) arg-desc num-args closure-passed)
	     ;; Trace FCTN through symbols and closures to get the ultimate function
	     ;; which will tell us whether to evaluate the args.
	     ;; When we actually call the function, we call FCTN
	     ;; unless CLOSURE-PASSED is set.  In that case, we call (CAR FORM).
	     (do () (())
	       (typecase fctn
		 (symbol
		  (setq fctn
			(if (eq interpreter-function-environment t)
			    (fsymeval fctn)
			  (interpreter-fsymeval fctn))))
		 ((or closure entity)
		  (setq fctn (closure-function fctn)
			closure-passed t))
		 (t (return))))
	     (setq arg-desc (%args-info fctn))
	     (if (bit-test %arg-desc-interpreted arg-desc)
		 ;; Here if not a FEF.
		 (progn
		  ;; Detect ucode entry that is not actually microcoded.
		  (if (and (typep fctn ':microcode-function)
			   (not (fixp (system:micro-code-entry-area (%pointer fctn)))))
		      (setq fctn (system:micro-code-entry-area (%pointer fctn))))
		  (typecase fctn
		    (:list
		      (selectq (car fctn)
			((lambda subst named-lambda named-subst
			  cli:lambda cli:subst cli:named-lambda cli:named-subst)
			 (let ((lambda-list
				 (if (memq (car fctn)
					   '(named-lambda named-subst
					     cli:named-lambda cli:named-subst))
				     (caddr fctn) (cadr fctn))))
			   (setq num-args 0)
			   ;; Figure out whether there is a quoted rest argument,
			   ;; and open the call block with or without adi accordingly.
			   ;; Set NUM-ARGS to the number of args not counting any quoted rest arg.
			   (do ((ll lambda-list (cdr ll))
				(quote-status '&eval)
				rest-flag)
			       ((or (null ll)
				    (memq (car ll) '(&aux &key)))
				(if *applyhook*
				    (progn (%open-call-block 'applyhook1 0 4)
					   (%push (if closure-passed (car form) fctn)))
				  (%open-call-block (if closure-passed (car form) fctn) 0 4))
				(setq num-args (length (cdr form)))
				(%assure-pdl-room num-args))
			     (cond ((memq (car ll) '(&eval &quote))
				    (setq quote-status (car ll)))
				   ((eq (car ll) '&rest)
				    (setq rest-flag t))
				   ((memq (car ll) lambda-list-keywords))
				   (rest-flag
				    ;; Here if we encounter a rest arg.
				    (if ( (length (cdr form))
					   (if (eq quote-status '&quote)
					       num-args
					     63.))
					;; If there aren't enough args supplied to actually
					;; reach it, arrange to exit thru the DO's end-test.
					(setq ll nil)
				      ;; If the quoted res arg is non-nil,
				      ;; set NUM-ARGS to number of spread args,
				      ;; and call with ADI.
				      (if *applyhook*
					  (progn (%open-call-block 'applyhook2 0 4)
						 (%push (if closure-passed (car form) fctn)))
					(%push 0)
					(%push 14000000)
					(%open-call-block (if closure-passed (car form) fctn) 1 4))
				      (%assure-pdl-room (1+ num-args))
				      (return)))
				   (t (incf num-args))))
			   ;; Now push the args, evalling those that need it.
			   (do ((ll lambda-list (cdr ll))
				(argl (cdr form) (cdr argl))
				(quote-status '&eval)
				(argnum 0 (1+ argnum)))
			       (())
			     (do () ((null ll))
			       (cond ((memq (car ll) '(&eval &quote))
				      (setq quote-status (car ll)))
				     ((memq (car ll) '(&rest &aux &key))
				      (setq ll nil))
				     ((memq (car ll) lambda-list-keywords))
				     (t (return)))
			       (pop ll))
			     (if (= argnum num-args)
				 ;; Done with spread args => push the rest arg.
				 (return
				   (when argl
				     (%push
				       (if (eq quote-status '&eval)
					   (mapcar 'eval1 argl)
					 argl)))))
			     (if (eq quote-status '&eval)
				 (%push (eval1 (car argl)))
			       (%push (car argl))))
			   (%activate-open-call-block)))
			(macro (eval1 (error-restart (error "Retry macro expansion.")
					(automatic-displace (cdr fctn) form))
				      t))
			((curry-before curry-after)
			 (if *applyhook*
			     (progn (%open-call-block 'applyhook1 0 4)
				    (%push (if closure-passed (car form) fctn)))
			   (%open-call-block (if closure-passed (car form) fctn) 0 4))
			 (%assure-pdl-room (length (cdr form)))
			 (do ((argl (cdr form) (cdr argl))
			      (argnum 0 (1+ argnum)))
			     ((null argl))
			   (%push (eval1 (car argl))))
			 (%activate-open-call-block))
			(t (if (lambda-macro-call-p fctn)
			       (eval1 (cons (lambda-macro-expand fctn) (cdr form)))
			     (invalid-function form)))))
		    ((or :select-method :instance)
		     (if *applyhook*
			 (progn (%open-call-block 'applyhook1 0 4)
				(%push (if closure-passed (car form) fctn)))
		       (%open-call-block (if closure-passed (car form) fctn) 0 4))
		     (%assure-pdl-room (length (cdr form)))
		     (do ((argl (cdr form) (cdr argl))
			  (argnum 0 (1+ argnum)))
			 ((null argl))
		       (%push (eval1 (car argl))))
		     (%activate-open-call-block))
		    (t (invalid-function form))))
	       ;; FEF (or ucode entry that's microcoded or a FEF).
	       ;; Open call block accordingly to whether there's a quoted rest arg.
	       ;; Also, if more than 64 args to fn taking evaled rest arg,
	       ;; we must make an explicit rest arg to avoid lossage.
	       ;; LIST, etc., may not be called directly because the ucode versions
	       ;; do not deal with explicitly passed rest arguments.
	       (and list-etc-function-mappings
		    (memq fctn list-etc-functions)
		    (setq fctn (fsymeval
				 (cdr (assq fctn list-etc-function-mappings)))
			  arg-desc (%args-info fctn)))
	       (if (or (and (bit-test %arg-desc-quoted-rest arg-desc)
			    (> (length (cdr form)) (ldb %%arg-desc-max-args arg-desc)))
		       (and (bit-test %arg-desc-evaled-rest arg-desc)
			    (> (length (cdr form)) 63.)))
		   (progn ;; NUM-ARGS includes only the spread args.
			  (setq num-args (ldb %%arg-desc-max-args arg-desc))
			  (if *applyhook*
			      (progn (%open-call-block 'applyhook2 0 4)
				     (%push (if closure-passed (car form) fctn)))
			    ;; ADI for fexpr-call.
			    (%push 0)
			    (%push 14000000)
			    (%open-call-block (if closure-passed (car form) fctn) 1 4))
			  ;; We need room for the spread args, plus one word for the rest arg.
			  (%assure-pdl-room (1+ num-args)))
		 (setq num-args (length (cdr form)))
		 (if *applyhook*
		     (progn (%open-call-block 'applyhook1 0 4)
			    (%push (if closure-passed (car form) fctn)))
		   (%open-call-block (if closure-passed (car form) fctn) 0 4))
		 (%assure-pdl-room num-args))
	       ;; If some spread args are quoted, use the ADL to tell which.
	       (cond ((bit-test %arg-desc-fef-quote-hair arg-desc)
		      ;; Get the ADL pointer.
		      (let ((adl (get-macro-arg-desc-pointer fctn)))
			(do ((argl (cdr form) (cdr argl))
			     (argnum 0 (1+ argnum)))
			    ((= argnum num-args)
			     ;; Done with spread args => push rest arg if any.
			     (when argl
			       (%push
				 (if (bit-test %arg-desc-evaled-rest arg-desc)
				     (mapcar 'eval1 argl)
				   argl))))
			  (let ((item (or (car adl) fef-qt-eval)))
			    ;; Figure out how many extra words of ADL to skip for this arg.
			    (if (bit-test %fef-name-present item) (pop adl))
			    (selector (logand %fef-arg-syntax item) =
			      (fef-arg-opt
			       (if (memq (logand item %fef-init-option)
					 '(#.fef-ini-pntr #.fef-ini-c-pntr
					   #.fef-ini-opt-sa #.fef-ini-eff-adr))
				   (pop adl)))
			      (fef-arg-req)
			      ;; Note: does not get here for quoted rest arg.
			      ;; Gets here for evalled rest arg, or if no more args wanted
			      ;; (eval extra args supplied here; get error later).
			      (t (setq adl nil)))
			    (pop adl)
			    ;; Eval the arg if the ADL says to do so.
			    (%push (if ( (logand %fef-quote-status item) fef-qt-qt)
				       (eval1 (car argl))
				     (car argl)))))))
		     (t
		      ;; No quoted args except possibly the rest arg.  Don't look at ADL.
		      (do ((argnum 0 (1+ argnum))
			   (argl (cdr form) (cdr argl)))
			  ((= argnum num-args)
			   ;; Done with spread args => push the rest arg.
			   (when argl
			     (%push
			       (if (bit-test %arg-desc-evaled-rest arg-desc)
				   (mapcar 'eval1 argl)
				 argl))))
			(%push (eval1 (car argl))))))
	       (%activate-open-call-block))))))

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
ENVIRONMENT is the lexical environment to eval in, as in the function *EVAL."
  (eval1 form t))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun applyhook (function args *evalhook* *applyhook*
		  &optional environment
		  &aux (interpreter-environment (car environment))
		  (interpreter-function-environment (cadr environment)))
  "Apply FUNCTION to ARGS, using specified *EVALHOOK* and *APPLYHOOK* except at the top level.
ENVIRONMENT is the lexical environment to eval in, as in the function *EVAL."
  (apply function args))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


;Invoke the applyhook on a function which does not have an explicitly passed rest arg.
(defun applyhook1 (function &rest args)
  (let (evalhook applyhook
	(tem applyhook))
    (with-stack-list (environment interpreter-environment interpreter-function-environment)
      (funcall tem function args environment))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


;Invoke the applyhook for a function with an explicitly passed rest arg.
;ARGS* is like the arguments to LIST*.
(defun applyhook2 (function &rest args*)
  (let ((args (apply 'list* args*))
	evalhook applyhook
	(tem applyhook))
    (with-stack-list (environment interpreter-environment interpreter-function-environment)
      (funcall tem function args environment))))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

;;; Stream generating functions

(DEFUN OPEN-CHAOS (HOST PATHNAME &REST OPTIONS &KEY (DIRECTION ':INPUT) (CHARACTERS T)
		   (ERROR T) (ACCESS-ERROR (NOT ERROR))
		   (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
		   (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
					;; :UNSPECIFIC here is to prevent lossage
					;; writing ITS files with no version numbers.
					'(:NEWEST :UNSPECIFIC))
				  ':NEW-VERSION ':ERROR)
			      IF-EXISTS-P)
		   (IF-DOES-NOT-EXIST
		     (COND ((MEMQ DIRECTION '(:PROBE))
			    NIL)
			   ((AND (EQ DIRECTION ':OUTPUT)
				 (NOT (MEMQ IF-EXISTS '(:OVERWRITE :APPEND))))
			    ':CREATE)
			   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			   ;; for compatibility with the past.
			   ;; A Common-Lisp program would use :PROBE
			   ;; and get NIL as the default for this.
			   (T ':ERROR)))
		   TEMPORARY DELETED RAW SUPER-IMAGE (BYTE-SIZE ':DEFAULT)
		   PRESERVE-DATES IGNORE
		   &AUX HOST-UNIT DATA-CONN PKT SUCCESS STRING NOT-ABORTED
		   PHONY-CHARACTERS SIGN-EXTEND-BYTES
		   (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SI:CCASE DIRECTION
    ((:INPUT :OUTPUT))
    (:IO (FERROR NIL "Bidirectional file streams are not supported."))
    ((NIL :PROBE) (SETQ DIRECTION NIL)))
  (CHECK-TYPE IF-EXISTS (MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
				:OVERWRITE :APPEND :SUPERSEDE NIL))
  (CHECK-TYPE IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
  (WHEN ELEMENT-TYPE-P
    (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
	  (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
  (FILE-OPERATION-RETRY
    (CONDITION-CASE-IF ACCESS-ERROR (ERROR-OBJECT)
        (PROGN
	  (IF (NULL DIRECTION)
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
			    (AND IF-EXISTS-P (NULL IF-EXISTS)
				 (SETQ OPTIONS (LIST* ':IF-EXISTS ':ERROR OPTIONS)))
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
				  (:INPUT "READ")
				  (:OUTPUT "WRITE"))
			" " (SELECTQ CHARACTERS
			      ((NIL) "BINARY")
			      (:DEFAULT "DEFAULT")
			      (T "CHARACTER"))
			(IF (AND (EQ DIRECTION ':OUTPUT)
				 (NEQ IF-EXISTS
				      (IF (MEMQ (PATHNAME-VERSION PATHNAME)
						'(:NEWEST :UNSPECIFIC))
					  ':NEW-VERSION ':SUPERSEDE)))
			    (STRING-APPEND " IF-EXISTS "
					   (IF (EQ IF-EXISTS NIL)
					       ':ERROR
					     IF-EXISTS))
			  "")
			(IF (OR IF-EXISTS-P
				(NEQ IF-DOES-NOT-EXIST
				     (SELECTQ DIRECTION
				       ((:INPUT NIL) ':ERROR)
				       (:OUTPUT ':CREATE))))
			    (STRING-APPEND " IF-DOES-NOT-EXIST "
					   (IF (EQ IF-DOES-NOT-EXIST NIL)
					       ':ERROR
					     IF-DOES-NOT-EXIST))
			  "")
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
				       ((NIL) 'FILE-PROBE-STREAM)
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
					    'FILE-OUTPUT-BINARY-STREAM))))
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

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "
(DEFUN MACROEXPAND-1 (MACRO-CALL &OPTIONAL ENVIRONMENT
		      &AUX (LOCAL-MACROS (UNLESS (EQ (CADR ENVIRONMENT) T)
					   (CADR ENVIRONMENT))))
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
	  ((SETQ TM (GET (LOCF LOCAL-MACROS)
			 (FUNCTION-CELL-LOCATION (CAR MACRO-CALL))))
	   (IF (EQ (CAR TM) 'MACRO)
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

; From file DEFMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

(DEFF DEFLAMBDA-MACRO-DISPLACE 'DEFLAMBDA-MACRO)

))

; From file DEFMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "


(DEFF MACRO-DISPLACE 'MACRO)
(DEFF DEFMACRO-DISPLACE 'DEFMACRO)

))

; From file DEFMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

;; STATE is 0 for mandatory args, 1 for optional args, 2 for rest args, 3 for aux vars,
;; 4 for &key args.
;; If it is 10 or more, the 10 bit signifies &LIST-OF and the low three bits
;; are as usual.
;; If it is 20 or more, it signifies that the next arg is an &ENVIRONMENT arg;
;; the low 4 bits say what state to revert to following that arg.
;; PATH is the form which, using CAR and CDR, would extract the part of the macro arg
;; which corresponds to this arg and the following args at the same level.
;; Thus, a simple arg would be set to `(CAR ,PATH).
;; PATTERN is the rest of the arglist at this level.
;; We push arg names on *VARLIST* and their appropriate values on *VALLIST*.
;; We return a list describing how many args are wanted:
;;  its car is the minimum number of args needed,
;;  its cadr is the number of optional args accepted,
;;  its caddr is the maximum number of args accepted, or NIL if any number are allowed.
;;    If non-NIL, this is normally the sum of the car and cadr.
(DEFUN DEFMACRO-&MUMBLE-CHEVEUX (PATTERN PATH STATE)
  (COND ((NULL PATTERN) (LIST 0 0 0))
	((ATOM PATTERN)
	 (COND ((> STATE 1)
		(FERROR NIL "Non-NIL end of list, ~S, following ~S in destructuring pattern."
			PATTERN
			(SELECTQ STATE
			  (2 '&REST)
			  (3 '&AUX)
			  (4 '&KEY)
			  (T (IF (>= STATE 20) '&ENVIRONMENT '&LIST-OF)))))
	       (T (DEFMACRO-CHEVEUX PATTERN PATH)
		  (LIST 0 0 NIL))))
	((EQ (CAR PATTERN) '&OPTIONAL)
	 (COND ((> STATE 1)
		(FERROR NIL "&OPTIONAL in bad context in destructuring pattern."))
	       (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 1))))
	((MEMQ (CAR PATTERN) '(&REST &BODY))
	 (AND (EQ (CAR PATTERN) '&BODY)
	      (SETQ DEFMACRO-&BODY-FLAG T))
	 (AND (NULL (CDR PATTERN))
	      (FERROR NIL "&REST or &BODY followed by no argument, in destructuring pattern."))
	 (COND ((> STATE 1) (FERROR NIL "&REST or &BODY in bad context in destructuring pattern."))
	       (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 2))))
	((EQ (CAR PATTERN) '&AUX)
	 (COND ((>= STATE 10) (FERROR NIL "&AUX following a &LIST-OF in destructuring pattern."))
	       (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 3))))
	((EQ (CAR PATTERN) '&KEY)
	 (COND ((> STATE 2) (FERROR NIL "&KEY in bad context in destructuring pattern."))
	       (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 4))))
	((EQ (CAR PATTERN) '&ENVIRONMENT)
	 (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (+ STATE 20)))
	((EQ (CAR PATTERN) '&LIST-OF)
	 (COND ((< STATE 3)
		(DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (+ 10 STATE)))
	       (T (FERROR NIL "&LIST-OF used incorrectly in destructuring pattern."))))
	((EQ (CAR PATTERN) '&ALLOW-OTHER-KEYS)
	 (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH STATE))
	((= STATE 0)
	 (DEFMACRO-CHEVEUX (CAR PATTERN) (LIST 'CAR PATH))
	 (DEFMACRO-REQUIRED
	   (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 0)))
	((= STATE 1)
	 (COND ((ATOM (CAR PATTERN))
		(DEFMACRO-CHEVEUX (CAR PATTERN) `(CAR ,PATH)))
	       (T
		(AND (CADDAR PATTERN)
		     (PUSH (CADDAR PATTERN) OPTIONAL-SPECIFIED-FLAGS))
		(DEFMACRO-CHEVEUX (CAAR PATTERN)
				  `(COND (,PATH
					  ,(AND (CADDAR PATTERN)
						`(SETQ ,(CADDAR PATTERN) T))
					  (CAR ,PATH))
					 (T ,(CADAR PATTERN))))))
	 (DEFMACRO-OPTIONAL
	   (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 1)))
	((= STATE 2)
	 (DEFMACRO-CHEVEUX (CAR PATTERN) PATH)
	 (COND ((CDR PATTERN)
		(AND (OR (ATOM (CDR PATTERN))
			 (NOT (MEMQ (CADR PATTERN) '(&AUX &KEY))))
		     (FERROR NIL "More than one &REST argument in a macro."))
		(DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 2)))
	 (LIST 0 0 NIL))
	((= STATE 3)
	 (COND ((ATOM (CAR PATTERN))
		(DEFMACRO-CHEVEUX (CAR PATTERN) NIL))
	       (T (DEFMACRO-CHEVEUX (CAAR PATTERN) (CADAR PATTERN))))
	 (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 3))
	((= STATE 4)
	 (LET* ((SYMBOL
		  (COND ((ATOM (CAR PATTERN)) (CAR PATTERN))
			((ATOM (CAAR PATTERN)) (CAAR PATTERN))
			(T (CADAAR PATTERN))))
		(KEYWORD
		  (IF (AND (LISTP (CAR PATTERN)) (LISTP (CAAR PATTERN)))
		      (CAAAR PATTERN)
		    (INTERN (STRING SYMBOL) 'KEYWORD)))
		(DEFAULT
		  (IF (LISTP (CAR PATTERN)) (CADAR PATTERN) NIL))
		(FLAGVAR (IF (LISTP (CAR PATTERN)) (CADDAR PATTERN))))
	   (PUSH SYMBOL *VARLIST*)
	   (PUSH `(GET (LOCF ,PATH) ',KEYWORD ,DEFAULT) *VALLIST*)
	   (WHEN FLAGVAR
	     (PUSH FLAGVAR *VARLIST*)
	     (PUSH `(NOT (NULL (GET-LOCATION-OR-NIL (LOCF ,PATH) ',KEYWORD))) *VALLIST*))
	   (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 4)
	   (LIST 0 0 NIL)))
	((= STATE 10)				;&LIST-OF not optional
	 (DEFMACRO-&LIST-OF-CHEVEUX (CAR PATTERN) `(CAR ,PATH))
	 (DEFMACRO-REQUIRED
	   (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) `(CDR ,PATH) 0)))
	((= STATE 11)				;&LIST-OF optional
	 (AND (ATOM (CAR PATTERN))
	      (FERROR NIL "Incorrect use of &LIST-OF in destructuring pattern."))
	 (AND (CADDAR PATTERN)
	      (PUSH (CADDAR PATTERN) OPTIONAL-SPECIFIED-FLAGS))
	 (DEFMACRO-&LIST-OF-CHEVEUX (CAAR PATTERN)
				    `(COND (,PATH
					    ,(AND (CADDAR PATTERN)
						  `(SETQ ,(CADDAR PATTERN) T))
					    (CAR ,PATH))
					   (T ,(CADAR PATTERN))))
	 (DEFMACRO-OPTIONAL
	   (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) `(CDR ,PATH) 1)))
	((= STATE 12)
	 (DEFMACRO-&LIST-OF-CHEVEUX (CAR PATTERN) PATH)
	 (COND ((CDR PATTERN)
		(AND (OR (ATOM (CDR PATTERN))
			 (NOT (EQ (CADR PATTERN) '&AUX)))
		     (FERROR NIL "More than one &REST argument in destructuring pattern."))
		(DEFMACRO-&MUMBLE-CHEVEUX (CDDR PATTERN) PATH 3)))
	 (LIST 0 0 NIL))
	((>= STATE 20)
	 (DEFMACRO-CHEVEUX (CAR PATTERN) '*MACROENVIRONMENT*)
	 (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (LOGAND STATE 17)))
	))

))

; From file DEFMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

;; X is the cdr of the DEFMACRO form.
;; Return a LAMBDA expression for the expander function.
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
	   DOC-STRING)
      (MULTIPLE-VALUE (NIL NIL DOC-STRING)
	(EXTRACT-DECLARATIONS BODY NIL T))
      `(CLI:NAMED-LAMBDA ,(CAR X) (*MACROARG* &OPTIONAL *MACROENVIRONMENT*)
	,@(IF DOC-STRING (LIST DOC-STRING))
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

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

;; Used as the value of *MACROEXPAND-HOOK* to make all macros displace.
(defun automatic-displace (expander-function original-form)
  (let ((expanded-form
	  (with-stack-list (*macroexpand-environment* interpreter-environment
						      interpreter-function-environment)
	    (if (> (ldb %%arg-desc-max-args (args-info expander-function)) 1)
		(funcall expander-function original-form *macroexpand-environment*)
	      (funcall expander-function original-form)))))
    (if (or (eq expanded-form original-form)
	    (eq (car original-form) 'displaced)
	    inhibit-displacing-flag
	    (not (= (%area-number original-form) working-storage-area))
	    (not (= (%area-number expanded-form) working-storage-area)))
	expanded-form
      (displace original-form expanded-form))))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

;;; For things like M-X Select System as Tags Table
(DEFUN ALL-SYSTEMS-NAME-ALIST ()
  "Return an alist of all system names and system objects."
  (LOOP FOR SYSTEM IN *SYSTEMS-LIST*
	NCONC (CONS (CONS (STRING (IF (TYPEP SYSTEM 'SYSTEM) (SYSTEM-NAME SYSTEM) SYSTEM))
			  SYSTEM)
		    (MAPCAR #'(LAMBDA (NICKNAME) (CONS NICKNAME SYSTEM))
			    (IF (TYPEP SYSTEM 'SYSTEM)
				(SYSTEM-NICKNAMES SYSTEM))))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFUN PARSE-PATHNAME-FIND-COLON (STRING &OPTIONAL (ORIGINAL-START 0) END
				  &AUX HOST-SPECIFIED (START ORIGINAL-START))
  (DECLARE (RETURN-LIST HOST-SPECIFIED START END))
  (UNLESS END (SETQ END (LENGTH STRING)))
  (DO ((IDX START (1+ IDX))
       (HOST-START START)
       (ONLY-WHITESPACE-P T)
       (CHAR))
      (( IDX END))
    (COND ((= (SETQ CHAR (AREF STRING IDX)) #/:)
	   ;; The first atom ends with a colon, take the host from that, and
	   ;; parse from the end of that.
	   (SETQ HOST-SPECIFIED (SUBSTRING STRING HOST-START IDX)
		 START (1+ IDX))
	   (RETURN))
	  ((AND (= CHAR #\SP) ONLY-WHITESPACE-P)	;Skip leading spaces
	   (SETQ HOST-START (1+ IDX)))
	  (T
	   (SETQ ONLY-WHITESPACE-P NIL)
	   (OR (AND ( CHAR #/0) ( CHAR #/9))
	       (AND ( CHAR #/A) ( CHAR #/Z))
	       (AND ( CHAR #/a) ( CHAR #/z))
	       (= CHAR #/-)
	       ;; If we get to non-alphabetic or -numeric,
	       ;; then no interesting colon
	       (RETURN)))))
  (AND (NULL HOST-SPECIFIED)
       (PLUSP END) (= (AREF STRING (1- END)) #/:)
       (SETQ HOST-SPECIFIED (STRING-REVERSE-SEARCH-CHAR #\SP STRING (1- END)))
       ;; The last character is a colon, take the host from the last atom, and
       ;; parse from the beginning to the space before that.
       (PSETQ HOST-SPECIFIED (SUBSTRING STRING (1+ HOST-SPECIFIED) (1- END))
	      END HOST-SPECIFIED))
  ;; If it's just a colon with only whitespace before it,
  ;; believe there is no host name, but don't count the colon as part of the
  ;; per-host pathname.
  (AND (EQUAL HOST-SPECIFIED "")
       (SETQ HOST-SPECIFIED NIL))
  ;; If what looked like the host really wasn't, forget it and reset the indices
  (AND HOST-SPECIFIED
       (NULL (SETQ HOST-SPECIFIED (GET-PATHNAME-HOST HOST-SPECIFIED T)))
       (SETQ START ORIGINAL-START
	     END NIL))			;This will be interpreted correctly
  (VALUES HOST-SPECIFIED START END))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CONDITION-CALL-IF (COND-FORM (VARIABLE) BODY-FORM &REST CLAUSES)
  "Like CONDITION-CALL, but establishes the handlers only if COND-FORM evaluates non-NIL.
See the documentation of CONDITION-CALL for more information."
  ;; We don't use &BODY in the real arglist to avoid overriding
  ;; the special form of indentation on *INITIAL-LISP-INDENT-OFFSET-ALIST*
  (DECLARE (ARGLIST COND-FORM (VARIABLE) BODY-FORM &BODY CLAUSES))
  (LET* ((ORDINARY-CLAUSES (SUBSET #'(LAMBDA (CLAUSE) (NEQ (CAR CLAUSE) ':NO-ERROR))
				   CLAUSES))
	 (NO-ERROR-CLAUSE (ASSQ ':NO-ERROR CLAUSES))
	 (PREDICATES
	   (MAPCAR 'CAR ORDINARY-CLAUSES))
	 (VAR (OR VARIABLE (GENSYM)))
	 (TAG (GENSYM))
	 (HANDLER `#'(LAMBDA (,VAR &REST IGNORE)
		       (IF (OR . ,PREDICATES)
			   (*THROW ',TAG ,VAR)))))
    `(CATCH-CONTINUATION-IF T ',TAG
			    #'(LAMBDA (,VAR)
				(COND . ,ORDINARY-CLAUSES))
			    ,(IF NO-ERROR-CLAUSE
				 `#'(LAMBDA () . ,(CDR NO-ERROR-CLAUSE)))
       (CONDITION-BIND-IF ,COND-FORM ((NIL ,HANDLER)) ,BODY-FORM))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFINE-CANONICAL-TYPE :TEXT "TEXT"
  ((:TOPS-20 :TENEX) "TEXT" "TXT")
  (:UNIX "TX")
  (:VMS "TXT"))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFINE-CANONICAL-TYPE :MIDAS "MIDAS"
  ((:TOPS-20 :TENEX) "MID" "MIDAS")
  (:UNIX "MD")
  (:VMS "MID"))

))

; From file SECTIO.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFVAR *SECTION-COUNT* 0)


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
				   (PATHNAME-NAME (BUFFER-PATHNAME BUFFER))
				 (BUFFER-NAME BUFFER)))
			     "-"
			     (PRIN1-TO-STRING (INCF *SECTION-COUNT*)))
	    STR SYM))
    (VALUES SYM STR NIL)))


))

; From file SECTIO.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN (:LISP SECTION-P) (LINE IGNORE)
  (AND (PLUSP (LENGTH LINE))
       (= (LDB %%CH-CHAR (AREF LINE 0)) #/()))


))

; From file TYPWIN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "


;;;Type out item, either as itself or FORMAT-ARGS.  TYPE is used for indexing into
;;;ITEM-TYPE-ALIST
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM) (TYPE ITEM &REST FORMAT-ARGS)
  ;; Wrap around, if necessary, before recording the cursor.
  (SEND SELF ':INCREMENT-CURSORPOS 0 0)
  (LET ((X CURSOR-X) (Y CURSOR-Y))
    (IF FORMAT-ARGS (LEXPR-FUNCALL #'FORMAT SELF FORMAT-ARGS) (PRINC ITEM SELF))
    (DO ((LINE-Y Y (+ LINE-Y LINE-HEIGHT))
	 (LINE-X X LEFT-MARGIN-SIZE))
	(NIL)
      (IF (> (+ LINE-Y LINE-HEIGHT)
	     (SHEET-INSIDE-BOTTOM))
	  (SETQ LINE-Y TOP-MARGIN-SIZE))
      (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
	(PUSH (LIST TYPE ITEM LINE-X LINE-Y
		    (IF (= LINE-Y CURSOR-Y) CURSOR-X (SHEET-INSIDE-WIDTH))
		    (+ LINE-Y LINE-HEIGHT))
	      ITEM-LIST))
      (IF (= LINE-Y CURSOR-Y) (RETURN)))
    (TV:MOUSE-WAKEUP)
    NIL))

;;;Make an item without drawing anything (assuming the caller has drawn it already)
;;;Instead you just pass in an enclosing rectangle
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :PRIMITIVE-ITEM) (TYPE ITEM LEFT TOP RIGHT BOTTOM)
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH (LIST TYPE ITEM (+ LEFT (SHEET-INSIDE-LEFT)) (+ TOP (SHEET-INSIDE-TOP))
		(+ RIGHT (SHEET-INSIDE-LEFT)) (+ BOTTOM (SHEET-INSIDE-TOP)))
	  ITEM-LIST)
    (TV:MOUSE-WAKEUP)
    NIL))

;;;Like :PRIMITIVE-ITEM except that the edges are wrt the outside of the window.
;;;so you can use values such as CURSOR-X without subtracting (SHEET-INSIDE-TOP).
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :PRIMITIVE-ITEM-OUTSIDE)
	   (TYPE ITEM LEFT TOP RIGHT BOTTOM)
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH (LIST TYPE ITEM LEFT TOP RIGHT BOTTOM)
	  ITEM-LIST)
    (TV:MOUSE-WAKEUP)
    NIL))

;;;Type out list of item as many as will fit on each line, centered.
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM-LIST) (TYPE LIST &AUX (MAXL 0) N
						  (INSIDE-WIDTH (SHEET-INSIDE-WIDTH)))
  (FUNCALL-SELF ':FRESH-LINE)
  (COND (LIST					;Do nothing if empty list
	 ;; Compute the maximum width of any item, in dots (MAXL).
	 (DOLIST (ITEM LIST)
	   (LET ((STRING (STRING (IF (CONSP ITEM) (CAR ITEM) ITEM))))
	     (SETQ MAXL (MAX (SHEET-STRING-LENGTH SELF STRING) MAXL))))
	 ;; How many items go on each line (except the last)?
	 (SETQ N (MAX (MIN (TRUNCATE INSIDE-WIDTH (+ MAXL (FONT-CHAR-WIDTH CURRENT-FONT)))
			   (LENGTH LIST))
		      1))			;Always print something, even if continuation
	 ;; Now print the items and store the data in the table.
	 ;; Move to a new line when we exhaust a line, and at the end.
	 ;; I counts from 1 thru N on each line.
	 (DO ((I 1 (1+ I))
	      (LIST LIST (CDR LIST))
	      (WIDTH-PER (TRUNCATE INSIDE-WIDTH N)))
	     ((NULL LIST))
	   ;; Actually make this item.
	   (IF (CONSP (CAR LIST))
	       (FUNCALL SELF ':ITEM TYPE (CDAR LIST) "~A" (CAAR LIST))
	       (FUNCALL SELF ':ITEM TYPE (CAR LIST)))
	   ;; Space out for next item, or move to new line.
	   (IF (AND ( I N) (CDR LIST))
	       ;; Not end of line, space out for next item.
	       (MULTIPLE-VALUE-BIND (X Y)
		   (SHEET-READ-CURSORPOS SELF)
		 (SHEET-SET-CURSORPOS SELF
				      (* WIDTH-PER
					 (TRUNCATE (+ (1- WIDTH-PER) X)
						   WIDTH-PER))
				      Y))
	       ;; End of line.
	       (SHEET-CRLF SELF)
	       (SETQ I 0)))))
  (TV:MOUSE-WAKEUP)
  NIL)

))

; From file COLOR.LISP SRC:<L.WINDOW> OZ:
#8R COLOR#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COLOR")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLOR  "

(DEFUN COLOR-DRAW-LINE (X1 Y1 X2 Y2
			&OPTIONAL (COLOR 17) (ALU TV:ALU-SETA) (SCREEN COLOR-SCREEN))
  "Draw a line from X1, Y1 to X2, Y2 in color COLOR."
    (AND (> X1 X2) (SWAPF X1 X2) (SWAPF Y1 Y2))
    (TV:PREPARE-SHEET (SCREEN)
      (LET ((DX (- X2 X1))
	    (DY (- Y2 Y1))
	    (PIXEL-ARRAY (TV:SHEET-SCREEN-ARRAY COLOR-SCREEN)))
	(LET ((DIR-Y (IF (MINUSP DY) -1 1))
	      (DY (ABS DY)))
	  (COND ((ZEROP DY) (RECTANGLE X1 Y1 (- X2 X1) 1 COLOR ALU))
		((ZEROP DX) (RECTANGLE X1 (MIN Y1 Y2) 1 (- (MAX Y1 Y2) (MIN Y1 Y2))
				       COLOR ALU))
		((> DX DY)			;X IS LARGER STEP
		 (DO ((X1 X1 (1+ X1))
		      (REM (TRUNCATE DY 2) (+ REM DY)))
		     ((> X1 X2))
		   (IF ( REM DX) (SETQ Y1 (+ Y1 DIR-Y) REM (- REM DX)))
		   (AS-2-REVERSE (BOOLE ALU COLOR (AR-2-REVERSE PIXEL-ARRAY X1 Y1))
				 PIXEL-ARRAY X1 Y1)))
		(T				;Y IS LARGER STEP
		 (DO ((I 0 (1+ I))
		      (Y Y1 (+ Y DIR-Y))
		      (REM (TRUNCATE DX 2) (+ REM DX)))
		     ((> I DY))
		   (IF ( REM DY) (SETQ X1 (1+ X1) REM (- REM DY)))
		   (AS-2-REVERSE (BOOLE ALU COLOR (AR-2-REVERSE PIXEL-ARRAY X1 Y))
				 PIXEL-ARRAY X1 Y))))))))

))

; From file PATED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFCOM COM-ADD-PATCH "Add the current defun or the region (if any) to the patch buffer.
If there is no patch buffer, ask the user for the system to patch. Then reserve a new
version number, and create a buffer whose pathname is the source file name for the
patch creating that version number.  If there is a region, append it to the end of the
patch buffer; otherwise append the current defun to the end of the patch buffer." ()
  (LET (BP1 BP2 DEFUN-NAME)
    (COND ((WINDOW-MARK-P *WINDOW*)
	   ;; there is a region, use it.
	   (SETQ BP1 (MARK) BP2 (POINT))
	   (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	   (SETQ DEFUN-NAME "the region"))
	  ((MULTIPLE-VALUE (BP1 DEFUN-NAME) (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL T))
	   ;; No region, try to get containing defun.
	   (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	   (SETQ DEFUN-NAME (GET-DEFUN-NAME DEFUN-NAME)))
	  (T
	   (BARF "Unbalanced parentheses or no defuns.")))
    (ADD-PATCH-INTERVAL BP1 BP2 T DEFUN-NAME *INTERVAL*))
  DIS-MARK-GOES)

))

; From file SECTIO.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN GET-DEFUN-NAME (BP &AUX BP1)
  "Return the function spec defined by the defun starting at BP."
  (AND (SETQ BP (FORWARD-ATOM BP))
       (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* BP))
       (SETQ BP1 (FORWARD-SEXP BP))
       (STRING-REMOVE-FONTS (STRING-INTERVAL BP BP1))))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-DEFUN-INTERNAL (COMPILE-P MODE-NAME ECHO-NAME
			       &OPTIONAL USE-TYPEOUT DEFVAR-HACK
			       (COMPILER-PROCESSING-MODE 'COMPILER:MACRO-COMPILE)
			       &AUX BP1 BP2 DEFUN-NAME)
  "Compile or evaluate a part of the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
If there is a region, it is used; otherwise the current or following defun is used.
USE-TYPEOUT is passed to COMPILE-PRINT-INTERVAL and controls where information is printed.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (COND ((WINDOW-MARK-P *WINDOW*)
	 (SETQ BP1 (MARK) BP2 (POINT))
	 (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	 (SETQ DEFUN-NAME "Region"))
	((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
	 (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	 (SETQ DEFVAR-HACK T))
	(T
	 (BARF "Unbalanced parentheses")))
  (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P
			  DEFUN-NAME MODE-NAME ECHO-NAME USE-TYPEOUT DEFVAR-HACK
			  COMPILER-PROCESSING-MODE))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-BUFFER (COMPILE-P MODE-NAME ECHO-NAME
		       &OPTIONAL (COMPILER-PROCESSING-MODE 'COMPILER:MACRO-COMPILE)
		       &AUX BP1 BP2 NAME)
  "Compile or evaluate the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
COMPILE-PROCESSING-MODE is either COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (IF *NUMERIC-ARG-P*
      (SETQ BP1 (POINT) BP2 (INTERVAL-LAST-BP *INTERVAL*) NAME "Rest of buffer")
      (SETQ BP1 *INTERVAL* NAME "Buffer"))
  (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P NAME MODE-NAME ECHO-NAME
			  NIL ;USE-TYPEOUT
			  NIL ;DEFVAR-HACK
			  COMPILER-PROCESSING-MODE)
  DIS-NONE)

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-PRINT-INTERVAL (BP1 BP2 IN-ORDER-P COMPILE-P REGION-NAME MODE-NAME ECHO-NAME
			       &OPTIONAL USE-TYPEOUT DEFVAR-HACK
			       COMPILER-PROCESSING-MODE
			       ALREADY-RESECTIONIZED-FLAG
			       &AUX FORMAT-FUNCTION SUCCESS)
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
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (OR COMPILER-PROCESSING-MODE
      (SETQ COMPILER-PROCESSING-MODE 'COMPILER:MACRO-COMPILE))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS ALREADY-RESECTIONIZED-FLAG
    (CHECK-INTERVAL-SECTIONS BP1 BP2 T))
  (UNDO-SAVE-CURRENT-RANGE)
  (SETQ FORMAT-FUNCTION (SELECTQ USE-TYPEOUT
			  ((T :TYPEOUT) #'(LAMBDA (STRING &REST ARGS)
					    (LEXPR-FUNCALL #'FORMAT T
							   STRING ARGS)))
			  (:PROMPT #'PROMPT-LINE-MORE)
			  (OTHERWISE #'(LAMBDA (STRING &REST ARGS)
					(LEXPR-FUNCALL #'FORMAT QUERY-IO
						       STRING ARGS)))))
  (FUNCALL FORMAT-FUNCTION "~&~A ~A" MODE-NAME
	   (OR REGION-NAME (SECTION-NODE-NAME (BP-NODE BP1))))
  (UNWIND-PROTECT
    (PROGN
      (COMPILE-INTERVAL COMPILE-P
			(SELECTQ USE-TYPEOUT
			  ((T) T)
			  (T QUERY-IO))
			DEFVAR-HACK BP1 BP2 T
			COMPILER-PROCESSING-MODE)
      (SETQ SUCCESS T))
    (OR SUCCESS
	(FUNCALL FORMAT-FUNCTION " -- aborted.")))
  (FUNCALL FORMAT-FUNCTION " -- ~A" ECHO-NAME)
  (UPDATE-INTERVAL-COMPILE-TICK BP1 BP2 T))
))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-INTERVAL (COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK
			 BP1 &OPTIONAL BP2 IN-ORDER-P
			 (COMPILE-PROCESSING-MODE 'COMPILER:MACRO-COMPILE)
			 &AUX GENERIC-PATHNAME STREAM
			      WHOLE-FILE   ;T if processing the entire file.
			      SI:FDEFINE-FILE-DEFINITIONS)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
Does not print any sort of message saying what is being compiled,
does not know about sectionization.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
PRINT-RESULTS-STREAM is a stream for printing the results of evaluation, or NIL not to print.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is either COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization."
  (DECLARE (SPECIAL COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK COMPILE-PROCESSING-MODE))
  (SETQ GENERIC-PATHNAME (SEND *INTERVAL* ':GENERIC-PATHNAME))
  ;; Does not reparse the mode line; we should let the user decide whether to do that.!
  ;; Should not override the user's Set Package if he has done one.
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  ;; Decide whether the entire file is being processed or just a part.
  ;; If the whole file, we want to notice if any function present in the file previously
  ;; is now missing.  If just a part, anything we don't notice now we must assume
  ;; is elsewhere in the file.
  (SETQ WHOLE-FILE
	(AND (BP-= BP1 (INTERVAL-FIRST-BP *INTERVAL*))
	     (BP-= BP2 (INTERVAL-LAST-BP *INTERVAL*))))
  (SETQ STREAM (INTERVAL-STREAM BP1 BP2 T))
  ;; Arrange for first read-error's location to be saved in q-reg ".".
  (REMPROP (MAKE-REGISTER-NAME #/.) 'POINT)
  (LET (SI:INTERPRETER-ENVIRONMENT (SI:INTERPRETER-FUNCTION-ENVIRONMENT T))
    (MULTIPLE-VALUE-BIND (VARS VALS) (SEND *INTERVAL* ':ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	;; Bind off this flag -- our stream is not generating font changes
	;; so READ should not try to remove any.
	(LET ((SI:READ-DISCARD-FONT-CHANGES NIL))
	  (COMPILER:COMPILE-STREAM
	    STREAM
	    GENERIC-PATHNAME
	    NIL					;FASD-FLAG
	    'COMPILE-INTERVAL-PROCESS-FN
	    T					;QC-FILE-LOAD-FLAG
	    NIL					;QC-FILE-IN-CORE-FLAG
	    PACKAGE
	    NIL					;FILE-LOCAL-DECLARATIONS
	    NIL					;Unused
	    WHOLE-FILE
	    )))))
  (OR (NULL GENERIC-PATHNAME)
      (SI:RECORD-FILE-DEFINITIONS GENERIC-PATHNAME SI:FDEFINE-FILE-DEFINITIONS WHOLE-FILE)))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-INTERVAL-PROCESS-FN (FORM)
  (COMPILER:COMPILE-DRIVER FORM 'COMPILE-INTERVAL-PROCESS-BASIC-FORM
			   'COMPILE-INTERVAL-PREPROCESS-FN))

;Record the name of what we are compiling, if this form makes it clear.
;Turn DEFVAR into SETQ if appropriate.
;If we are "evaluating", look for EVAL rather than COMPILE and LOAD in any EVAL-WHEN.
;Do not affect the processing of anything but EVAL-WHENs.

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-INTERVAL-PREPROCESS-FN (FORM)
  (DECLARE (SPECIAL COMPILE-P DEFVAR-HACK))
  ;; If appropriate, turn a DEFVAR into a SETQ.
  (COND ((AND DEFVAR-HACK
	      (CONSP FORM)
	      (> (LENGTH FORM) 2)
	      (MEMQ (CAR FORM) '(DEFVAR :DEFVAR))
	      (NEQ (CADDR FORM) ':UNBOUND))
	 (OR (SYMBOLP (CADR FORM))
	     (FERROR NIL "~S not a recognized form" FORM))
	 (PUTPROP (CADR FORM) T 'SPECIAL)	;Declare it
	 (COND ((> (LENGTH FORM) 3)		;in case there is a documentation string.
		(PUTPROP (SECOND FORM) (EVAL (FOURTH FORM)) ':DOCUMENTATION)
		(SETQ FORM (NBUTLAST FORM))))	;remove documentation so that
						;hack into SETQ works properly.
	 (SETF (CAR FORM) 'SETQ)))		;then always SETQ
  (WHEN (AND (NOT COMPILE-P) (LISTP FORM) (MEMQ (CAR FORM) '(EVAL-WHEN :EVAL-WHEN)))
    (WHEN (OR (MEMQ 'EVAL (CADR FORM)) (MEMQ ':EVAL (CADR FORM)))
      (MAPC 'COMPILE-INTERVAL-PROCESS-FN (CDDR FORM)))
    T))

;We get here when COMPILER:COMPILE-DRIVER finds something it doesn't handle specially.

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
       (FIRST T NIL))
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
	      (EQ (BP-LINE BP1) (SECTION-NODE-DEFUN-LINE NODE))
	      (EQ (BP-LINE BP1) (BP-LINE (INTERVAL-FIRST-BP NODE)))
	      (BP-< BP1 (CREATE-BP (SECTION-NODE-DEFUN-LINE NODE) 0)))
      (SEND NODE ':UPDATE-COMPILE-TICK))))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-BUFFER-CHANGED-FUNCTIONS (BUFFER ASK-P
					 &OPTIONAL (COMPILE-P T)
					 (NAMES '("Compile" "Compiling" "compiled."))
					 &AUX (QUERY-IO STANDARD-OUTPUT))
  "Recompile or evaluate all changed sections in BUFFER (that contain definitions).
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
ASK-P if non-NIL means query user for each section to be processed.
NAMES has three elements, that are like (/"Compile/" /"Compiling/" /"compiled./")."
  (LET ((*INTERVAL* BUFFER))
    (RESECTIONIZE-BUFFER *INTERVAL*)
    (DOLIST (SECTION (NODE-INFERIORS *INTERVAL*))
      (IF (AND (TYPEP SECTION 'SECTION-NODE)
	       (NOT (STRINGP (SECTION-NODE-NAME SECTION)))
	       (NOT (BP-= (INTERVAL-FIRST-BP SECTION) (INTERVAL-LAST-BP SECTION)))
	       (> (NODE-TICK SECTION)
		  (SECTION-NODE-COMPILE-TICK SECTION))
	       (OR (NOT ASK-P)
		   (FQUERY '(:SELECT T) "~A ~A? " (FIRST NAMES)
			   (SECTION-NODE-NAME SECTION))))
	  (COMPILE-PRINT-INTERVAL SECTION NIL T COMPILE-P
				  NIL
				  (SECOND NAMES) (THIRD NAMES) T T
				  NIL T)))))

))

; From file NPRIM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "

(DEFUN SECTIONIZE-FILE-BUFFER (BUFFER &OPTIONAL AARRAY PROPERTY START-NODE END-NODE
			       STREAM HACK-FONTS
                                 &AUX (PACKAGE PACKAGE)
				 (NODE-LIST NIL)
				 (MODE (SEND BUFFER ':MAJOR-MODE))
				 (*INTERVAL* BUFFER) INT-STREAM
				 FIRST-BP LAST-BP ADDED-COMPLETIONS
				 BUFFER-TICK OLD-CHANGED-SECTIONS
				 NODES-TO-REUSE ACTUAL-NEW-NODES
				 START-PREDECESSOR END-FOLLOWER)
  "Compute the sectionization of all or part of BUFFER.
If START-NODE and END-NODE are NIL, the whole buffer is resectionized from scratch.
If they are non-NIL, they should be sections of the buffer;
that portion of the buffer (inclusive!) is resectionized,
reusing any existing nodes if objects with the same names are still present."
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  (SETQ FIRST-BP (INTERVAL-FIRST-BP (OR START-NODE BUFFER))
	LAST-BP (IF END-NODE (COPY-BP (INTERVAL-LAST-BP END-NODE))
		  (INTERVAL-LAST-BP BUFFER)))
  ;; If operating on a specified range of sections,
  ;; from START-NODE to END-NODE inclusive,
  ;; put all those nodes on NODES-TO-REUSE.
  (IF START-NODE
      (DO ((N START-NODE (NODE-NEXT N)))
	  (())
	(PUSH N NODES-TO-REUSE)
	(IF (EQ N END-NODE) (RETURN))))
  (SETQ END-FOLLOWER (IF END-NODE (NODE-NEXT END-NODE))
	START-PREDECESSOR (IF START-NODE (NODE-PREVIOUS START-NODE)))
  ;;Buffer must be a FILE-BUFFER, but need not be a real ZMACS BUFFER.
  (COND (AARRAY
	 (SETQ ADDED-COMPLETIONS (MAKE-ARRAY 100 ':TYPE 'ART-Q-LIST
					         ':LEADER-LENGTH 2 ':LEADER-LIST '(0)))))
  (AND STREAM
       (SETQ INT-STREAM (INTERVAL-STREAM-INTO-BP LAST-BP HACK-FONTS)))
  ;; Make sure the buffer ends with an empty line.
;  (OR (ZEROP (BP-INDEX LAST-BP))
;      (INSERT LAST-BP #\CR))
  (SETQ BUFFER-TICK (BUFFER-TICK BUFFER))
  (TICK)
;;; This is no longer needed for computing the NODE-TICK of sections,
;;; since that can be determined from the text.
;;; But it is still useful for remembering the compile-ticks.
  (OR NODES-TO-REUSE
      (DOLIST (NODE (NODE-INFERIORS BUFFER))
	(PUSH (CONS (SECTION-NODE-NAME NODE)
		    NODE)
	      OLD-CHANGED-SECTIONS)))
  ;; Now scan the buffer and record the definitions.
  (DO* ((LINE (BP-LINE FIRST-BP) (LINE-NEXT LINE))
	(LIMIT (IF (ZEROP (BP-INDEX LAST-BP))	;Line to stop at (may be NIL)
		   (BP-LINE LAST-BP)
		 (LINE-NEXT (BP-LINE LAST-BP))))
	(EOFFLG)
	(BP (COPY-BP FIRST-BP))
	(PREV-NODE-START-BP FIRST-BP)
	(PREV-NODE-DEFUN-LINE NIL)
	(FIRST-NODE-NAME (IF START-NODE "Things deleted" "Buffer header"))
	(PREVIOUS-NODE NIL)
	(ADD-SECTIONS (GET MODE 'EDITING-TYPE))
	(SECTION-P (GET ADD-SECTIONS 'SECTION-P))
	(SECTION-NAME-FUNCTION (GET ADD-SECTIONS 'GET-SECTION-NAME)))
      (NIL)
    ;; If we have a stream, and we are at the limit, read another line.
    (COND ((AND STREAM (EQ LINE LIMIT) (NOT EOFFLG))
	   (MULTIPLE-VALUE (LINE EOFFLG)
			   (FUNCALL STREAM ':LINE-IN LINE-LEADER-SIZE))
	   (IF LINE (SETQ LINE (FUNCALL INT-STREAM ':LINE-OUT LINE)))
	   (SETQ LIMIT (LINE-NEXT LINE))))
    ;; See if the line is the start of a defun.
    ;; If so, record the section that it terminates.
    (WHEN (AND ADD-SECTIONS
	       (OR EOFFLG
		   (EQ LINE LIMIT)
		   (AND LINE (FUNCALL SECTION-P LINE BP))))
      (LET ((START PREV-NODE-START-BP)
	    END OLD-NODE)
	(IF (OR EOFFLG (EQ LINE LIMIT))
	    (SETQ END LAST-BP)
	  (MOVE-BP BP LINE 0)
	  (SETQ END (COPY-BP BP))
	  ;; Include one blank line before the form in the same section with it.
	  (IF (AND (LINE-PREVIOUS (BP-LINE END))
		   (LINE-BLANK-P (LINE-PREVIOUS (BP-LINE END))))
	      (MOVE-BP END (LINE-PREVIOUS (BP-LINE END)) 0))
	  (SETQ PREV-NODE-START-BP END))
	(UNLESS (AND (NOT (OR EOFFLG (EQ LINE LIMIT)))
		     (OR (BP-= START END)
			 (AND (NOT PREV-NODE-DEFUN-LINE)
			      START-NODE
			      (NOT (EQ START-NODE (CAR (NODE-INFERIORS BUFFER)))))))
	  ;; Now we have decided for certain to create a section ending here.
	  ;; Extract the name of the section that is just being terminated.
	  ;; By now, all the lines that the name runs over must have been read in.
	  (MULTIPLE-VALUE-BIND (SYM STR ERR)
	      (IF PREV-NODE-DEFUN-LINE
		  (FUNCALL SECTION-NAME-FUNCTION PREV-NODE-DEFUN-LINE BP)
		FIRST-NODE-NAME)
	    (WHEN ERR
	      (SETQ SYM "Unknown" STR NIL))
	    (UNLESS ERR
	      (SETQ OLD-NODE (CDR (ASSOC SYM OLD-CHANGED-SECTIONS))))
	    (SETQ PREVIOUS-NODE
		  (ADD-SECTION-NODE START END
				    SYM PREV-NODE-DEFUN-LINE BUFFER PREVIOUS-NODE
				    NIL
;				    (IF OLD-NODE
;					(NODE-TICK OLD-NODE)
;				      (IF STREAM BUFFER-TICK *TICK*))
				    (IF OLD-NODE
					(SECTION-NODE-COMPILE-TICK OLD-NODE)
				      BUFFER-TICK)
				    NODES-TO-REUSE))
	    (IF (MEMQ PREVIOUS-NODE NODES-TO-REUSE)
		(SETQ NODES-TO-REUSE (DELQ PREVIOUS-NODE NODES-TO-REUSE))
	      (PUSH PREVIOUS-NODE ACTUAL-NEW-NODES)
	      (WHEN (AND ADDED-COMPLETIONS (NOT (STRINGP SYM)))
		(SECTION-COMPLETION SYM STR ADDED-COMPLETIONS)
		(UNLESS (SYMBOLP SYM)
		  (SECTION-COMPLETION SYM
				      (DEFINITION-NAME-AS-STRING NIL SYM)
				      ADDED-COMPLETIONS))))
	    (PUSH PREVIOUS-NODE NODE-LIST))))
      (SETQ PREV-NODE-DEFUN-LINE LINE))
    ;; After processing the last line, exit.
    (COND ((OR EOFFLG (EQ LINE LIMIT))
	   (RETURN))))
  ;; If reading a stream, we should not have inserted a CR
  ;; after the eof line.
  (AND STREAM
       (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP BUFFER) -1 T)
			(INTERVAL-LAST-BP BUFFER)
			T))
  ;; Splice the nodes just made in with the nodes
  ;; before START-NODE and after END-NODE
  (LET ((FIRST-NEW-NODE (CAR (LAST NODE-LIST)))
	(FLUSHED-NODES (IF START-NODE NODES-TO-REUSE (NODE-INFERIORS BUFFER))))
    (COND (NODE-LIST
	   (WHEN END-FOLLOWER
	     (SETF (NODE-PREVIOUS END-FOLLOWER) (CAR NODE-LIST))
	     (SETF (NODE-NEXT (CAR NODE-LIST)) END-FOLLOWER))
	   (WHEN START-PREDECESSOR
	     (SETF (NODE-NEXT START-PREDECESSOR) FIRST-NEW-NODE)
	     (SETF (NODE-PREVIOUS FIRST-NEW-NODE) START-PREDECESSOR)))
	  ((AND START-PREDECESSOR END-FOLLOWER)
	   (SETF (NODE-NEXT START-PREDECESSOR) END-FOLLOWER)
	   (SETF (NODE-PREVIOUS END-FOLLOWER) START-PREDECESSOR)))
    ;; Construct the new list of all inferiors of BUFFER.
    ;; Except: if all old nodes were reused, and no new ones made,
    ;; these lists are both still correct.
    (IF (OR FLUSHED-NODES ACTUAL-NEW-NODES)
	(LET (ALL-NODES)
	  (DO ((N (IF END-FOLLOWER (CAR (LAST (NODE-INFERIORS BUFFER)))
		    (CAR NODE-LIST))
		  (NODE-PREVIOUS N)))
	      ((NULL N))
	    (PUSH N ALL-NODES))
	  (SETF (NODE-INFERIORS BUFFER) ALL-NODES)))
    ;; Flush old section nodes that were not reused.
    (DOLIST (NODE FLUSHED-NODES)
      ;; Flush ZMACS-BUFFERS properties for old nodes not reused.
      (WHEN PROPERTY
	(LET ((THE-BUFFER BUFFER)
	      (SYM (SECTION-NODE-NAME NODE)))
	  (OR (STRINGP SYM)
	      (CONDITION-CASE ()
		  (SI:FUNCTION-SPEC-PUTPROP
		   SYM
		   (DEL-IF #'(LAMBDA (DEFN) (EQ (CAR DEFN) THE-BUFFER))
			   (SI:FUNCTION-SPEC-GET SYM PROPERTY))
		   PROPERTY)
		(SYS:INVALID-FUNCTION-SPEC NIL)))))
      (FLUSH-BP (INTERVAL-FIRST-BP NODE))
      (FLUSH-BP (INTERVAL-LAST-BP NODE)))
    ;; Attach ZMACS-BUFFERS properties to the symbols defined herein.
    (WHEN PROPERTY
      (DOLIST (NODE ACTUAL-NEW-NODES)
	(UNLESS (STRINGP (SECTION-NODE-NAME NODE))
	  (CONDITION-CASE ()
	      (SI:FUNCTION-SPEC-PUSH-PROPERTY 
	       (SECTION-NODE-NAME NODE)
	       (CONS BUFFER (SECTION-NODE-DEFUN-LINE NODE))
	       PROPERTY)
	    (SYS:INVALID-FUNCTION-SPEC NIL))))))
  ;; Merge new entries into the aarray.
  (COND (ADDED-COMPLETIONS
	 ;; Copy all the completion entries now, so they all go on one page.
	 (LET ((I (ARRAY-LEADER ADDED-COMPLETIONS 0)))
	   (UNLESS (ZEROP I)
	     (DOTIMES (J I)
	       (SETF (AREF ADDED-COMPLETIONS J)
		     (CONS (STRING-APPEND (CAR (AREF ADDED-COMPLETIONS J)))
			   (CDR (AREF ADDED-COMPLETIONS J)))))
	     ;; Sort them and merge them into the main list.
	     (SORT-COMPLETION-AARRAY ADDED-COMPLETIONS)
	     (MERGE-COMPLETION-AARRAY AARRAY ADDED-COMPLETIONS))))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


;Sort of a macro version of funcall, for wrappers
(DEFMACRO MACROCALL (&REST X)
  (LET ((MACRO (COND ((DECLARED-DEFINITION (CAR X)))
		     ((FDEFINEDP (CAR X))
		      (FDEFINITION (CAR X)))
		     (T (FERROR NIL "Unable to find definition of wrapper ~s at expand time"
				(CAR X))))))
    (IF (AND (CONSP MACRO) (EQ (CAR MACRO) 'MACRO))
	(CALL (CDR MACRO) NIL X ':OPTIONAL *MACROEXPAND-ENVIRONMENT*)
      ;;--- Temporary code so I can test things in the kludge environment
      (IF (AND (SYMBOLP MACRO) (CONSP (FSYMEVAL MACRO)) (EQ (CAR (FSYMEVAL MACRO)) 'MACRO))
	  (CALL (CDR (FSYMEVAL MACRO)) NIL X ':OPTIONAL *MACROEXPAND-ENVIRONMENT*)
	(FERROR NIL "~S evaluated to ~S, which is not a macro" (CAR X) MACRO)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN OPTIMIZE (FORM CHECK-STYLE
		 &AUX (MACRO-CONS-AREA
			(IF (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
			    BACKGROUND-CONS-AREA
			  DEFAULT-CONS-AREA))
		 OPTIMIZATIONS-BEGUN-FLAG)
  (DO ((TM) (FN)) ((ATOM FORM))	;Do until no more expansions possible
    (LET ((DEFAULT-CONS-AREA MACRO-CONS-AREA))
      (SETQ FN (LAMBDA-MACRO-EXPAND (CAR FORM))))
    (OR (EQ FN (CAR FORM)) (SETQ FORM (CONS FN (CDR FORM))))
    (UNLESS OPTIMIZATIONS-BEGUN-FLAG
      ;; Check for too few or too many arguments
      (CHECK-NUMBER-OF-ARGS FORM FN))
    ;; If function is redefined locally with FLET,
    ;; don't use things that reflect its global definition.
    (WHEN (ASSQ FN LOCAL-FUNCTIONS)
      (RETURN))
    (UNLESS OPTIMIZATIONS-BEGUN-FLAG
      ;; Do style checking
      (AND CHECK-STYLE (NULL INHIBIT-STYLE-WARNINGS-SWITCH)
	   (COND ((ATOM FN)
		  (AND (SYMBOLP FN)
		       (SETQ TM (GET FN 'STYLE-CHECKER))
		       (FUNCALL TM FORM)))
		 ((NOT RUN-IN-MACLISP-SWITCH))
		 ((MEMQ (CAR FN) '(LAMBDA NAMED-LAMBDA CLI:LAMBDA CLI:NAMED-LAMBDA))
		  (LAMBDA-STYLE FN))
		 ((MEMQ (CAR FN) '(CURRY-BEFORE CURRY-AFTER))
		  (WARN 'NOT-IN-MACLISP ':MACLISP "~S does not work in Maclisp." (CAR FN))))))
    ;; Apply optimizations
    (OR (AND (SYMBOLP FN)
	     (DOLIST (OPT (GET FN 'OPTIMIZERS))
	       (OR (EQ FORM (SETQ FORM (FUNCALL OPT FORM)))
		   ;; Optimizer changed something, don't do macros this pass
		   (RETURN (SETQ OPTIMIZATIONS-BEGUN-FLAG T)))))
	;; No optimizer did anything => try expanding macros.
	(WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S:" FN)
	  ;; This LET returns T if we expand something.
	  (OR (LET ((OLD-FORM FORM)
		    (DEFAULT-CONS-AREA MACRO-CONS-AREA)
		    (RECORD-MACROS-EXPANDED T))
		(WITH-STACK-LIST (TEM1 NIL LOCAL-MACROS)
		  (SETQ FORM (MACROEXPAND-1 FORM TEM1)))
		(NEQ FORM OLD-FORM))
	      ;; Stop looping, no expansions apply
	      (RETURN)))
	;; The body of the WARN-ON-ERRORS either does RETURN or returns T.
	;; So if we get here, there was an error inside it.
	(RETURN (SETQ FORM `(ERROR-MACRO-EXPANDING ',FORM))))
    ;; Only do style checking the first time around
    (SETQ CHECK-STYLE NIL))
  ;; Result is FORM
  FORM)

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

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
	    (SETQ FORM (MACROEXPAND FORM))
	    (SETQ DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
	    ;; Output this form in the appropriate way.
	    (COMPILE-DRIVER FORM 'FASL-UPDATE-FORM NIL))
	  (FASD-END-WHACK)
	  (FASD-END-FILE))))
    (QC-FILE-RESET)))

;; Process one form, for COMPILE-DRIVER.

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "


(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN)
  (PROG TOP (FN TEM TEM1 (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	((ATOM FORM))
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM TOP NIL))
      (SETQ NFORM
	    (WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S at top level"
			     (CAR FORM))
	      (MACROEXPAND-1 FORM)))
      (IF (EQ FORM NFORM) (RETURN)
	(SETQ FORM NFORM)))
    ;; If this was a top-level macro, supply a good guess
    ;; for the function-parent for any DEFUNs inside the expansion.
    (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (COND ((ATOM FORM))
	    ((AND (NEQ FORM OFORM) (SYMBOLP (CADR OFORM)))
	     (PUSH `(FUNCTION-PARENT ,(CADR OFORM)) LOCAL-DECLARATIONS))
	    ((MEMQ (CAR OFORM) '(DEFSTRUCT :DEFSTRUCT))
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (COND ((ATOM FORM))
	    ((EQ (CAR FORM) 'EVAL-WHEN)
	     (OR (AND (OR (NOT (ATOM (CADR FORM))) (NULL (CADR FORM)))	;LISTP eventually
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE :EVAL :LOAD :COMPILE))))
		 (FERROR NIL "~S invalid EVAL-WHEN times;
	 must be a list of EVAL, LOAD, and//or COMPILE."
			     (CADR FORM)))
	     (SETQ TEM (OR (MEMQ 'COMPILE (CADR FORM)) (MEMQ ':COMPILE (CADR FORM))))
	     (SETQ TEM1 (OR (MEMQ 'LOAD (CADR FORM)) (MEMQ ':LOAD (CADR FORM))))
	     (DOLIST (FORM1 (CDDR FORM))
	       (IF (AND (CONSP FORM1) (MEMQ (CAR FORM1) '(EVAL-WHEN :EVAL-WHEN)))
		   ;; Another EVAL-WHEN within the first!
		   ;; Intersect the times at which they want to operate.
		   (LET (TIMES)
		     (AND TEM
			  (OR (MEMQ 'COMPILE (CADR FORM1)) (MEMQ ':COMPILE (CADR FORM1)))
			  (PUSH 'COMPILE TIMES))
		     (AND TEM1 (OR (MEMQ 'LOAD (CADR FORM1)) (MEMQ ':LOAD (CADR FORM1)))
			  (PUSH 'LOAD TIMES))
		     (WHEN TIMES
		       (COMPILE-DRIVER `(EVAL-WHEN ,TIMES . ,(CDDR FORM1))
				       PROCESS-FN OVERRIDE-FN)))
		 ;; An element which is not an EVAL-WHEN.
		 ;; Treat COMPILE and LOAD case independently.
		 ;; This can eval something twice in the COMPILE LOAD case,
		 ;; but the other alternative is to fail to eval some things at all.
		 (WHEN TEM
		   (FUNCALL PROCESS-FN FORM1 'DECLARE))
		 (WHEN TEM1
;For Common Lisp's sake, (EVAL-WHEN (LOAD) ...) should be treated by the compiler
;like no EVAL-WHEN at all.
		   (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN)))))
	    ((EQ (SETQ FN (CAR FORM)) 'DEFF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ FN 'DEF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDDR FORM)))
	    ((EQ FN 'WITH-SELF-ACCESSIBLE)
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDDR FORM)))
	    ((EQ FN 'PROGN)
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDR FORM)))
	    ((MEMQ FN '(MACRO DEFSUBST DEFF-MACRO))
	     (FUNCALL PROCESS-FN FORM 'MACRO))
	    ((MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
				EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT))
	     (FUNCALL PROCESS-FN FORM 'SPECIAL))
	    ((EQ FN 'DECLARE)
	     (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	    ((EQ FN 'COMMENT) NIL)
	    ((EQ FN 'PATCH-SOURCE-FILE)
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
			     PROCESS-FN OVERRIDE-FN)
	     (MAPC (FUNCTION (LAMBDA (FORM)
			       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		   (CDDR FORM))
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING NIL))
			     PROCESS-FN OVERRIDE-FN))
	    ((EQ FN 'COMPILER-LET)
	     (EVAL `(LET ,(CADR FORM) (COMPILE-DRIVER '(PROGN 'COMPILE . ,(CDDR FORM))
						      ',PROCESS-FN ',OVERRIDE-FN))))
	    ((EQ FN 'DEFUN)
	     (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed defun")
	       (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
	     (COND ((EQ (CDR TEM) (CDR FORM))
		    (IF (NULL (CDDR TEM))
			(WARN 'MALFORMED-DEFUN ':IMPOSSIBLE
			      "Malformed defun ~S" FORM)
		      (FUNCALL PROCESS-FN FORM 'DEFUN)))
		   (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN))))
	    (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))

))
