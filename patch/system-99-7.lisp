;;; -*- Mode:LISP; Package:USER; Patch-File:T; Base:10; Readtable:T; Fonts:(CPTFONT) -*-
;;; Patch file for System version 99.7
;;; Reason: interpreted &key conses less
;;; make lmfile accessible
;;; (^ float negative-fixnum) bug
;;; faster numeric contagion
;;; compiler: (declare (ignore *special*)) causes barfage
;;; login-setq uses lexical env
;;; inspector uses more lenient eval
;;; typo in unfasl
;;; altering window shapes from system edit menu bug
;;; Written 9/26/84 11:10:21 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.4, CADR 4.0, Experimental ZMail 54.1, MIT-Specific 23.0, microcode 320, GC@2.



(eval-when (load compile eval)
  (export (intern "EVAL-SPECIAL-OK" 'si) 'si)
  (export (intern "SET-LOGICAL-PATHNAME-HOST" 'fs) 'fs)
  (export (intern "MAKE-LOGICAL-PATHNAME-HOST" 'fs) 'fs)
  (export (intern "MERGE-PATHNAMES-1" 'fs) 'fs))

; From file ACCESS.LISP OZ:<L.IO.FILE> OZ:
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFMETHOD (LMFILE-HOST :NETWORK-TYPEP) (TYPE)
  (EQ TYPE ':CHAOS))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	PSEUDO-KEYNAMES DECLS)
    (IF (EQ (CAR LAMBDA-EXP) 'LAMBDA)
	(SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP))
      (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP)))	;named-lambda
    (MULTIPLE-VALUE-SETQ (POSITIONAL-ARGS NIL AUXVARS
			  REST-ARG POSITIONAL-ARG-NAMES
			  KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
    (MULTIPLE-VALUE-SETQ (NIL DECLS) (EXTRACT-DECLARATIONS BODY NIL NIL))
    (DO ((D DECLS (CDR D)))
	((NULL D))
      (SETF (CAR D) `(DECLARE ,(CAR D))))
    ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
    ;; and check explicitly whether that has been overridden.
    ;; If the arg is optional
    ;; and the initial value is a constant, we can really init it to that.
    ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
    ;; after all keywords are decoded, we bind the intended variable, in sequence.
    ;; However a var that can shadow something (including any special var)
    ;; must always be replaced with a dummy.
    (DO ((KIS KEYINITS (CDR KIS))
	 (KNS KEYNAMES (CDR KNS))
	 (PKNS PSEUDO-KEYNAMES (CDR PKNS))
	 (KFS KEYFLAGS (CDR KFS)))
	((NULL KNS))
      (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
	    (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	(OR (AND (NULL KEYFLAG)
		 (CONSTANTP KEYINIT)
		 (NOT (ASSQ KEYNAME VARS))
		 (NOT (LEXICAL-VAR-P KEYNAME))
		 (NOT (SPECIALP KEYNAME)))
	    (PROGN (SETF (CAR KIS) 'SI:KEYWORD-GARBAGE)
		   (SETQ PSEUDO-KEYNAME (GENSYM))
		   (SETF (CAR PKNS) PSEUDO-KEYNAME)
		   (PUSH `(,KEYNAME
			   (COND ((EQ ,PSEUDO-KEYNAME SI:KEYWORD-GARBAGE)
				  ,KEYINIT)
				 (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
				    ,PSEUDO-KEYNAME)))
			 KEYCHECKS)))))
    (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
    (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

    ;; If the user didn't ask for a rest arg, make one for the
    ;; outer function anyway.
    (OR REST-ARG (SETQ REST-ARG (GENSYM)
		       MAYBE-REST-ARG (LIST '&REST REST-ARG)))
    `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
       (LET* (,@(MAPCAR #'(LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
	      ,@KEYFLAGS)
;       (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;	      (OR (%PERMUTE-ARGS)
;		  (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;			   (CDR ,REST-ARG)
;			   (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;			   ',KEYKEYS)
;			 (%PERMUTE-ARGS)))
;	      ;; If the function really wants the rest arg,
;	      ;; flush the permutation table and its keyword.
;	      ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;	     (T
	 (WHEN ,REST-ARG
	   (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
					,REST-ARG ',KEYKEYS
					,ALLOW-OTHER-KEYS
					(KLUDGEY-COMPILATION-VARIABLE-LOCATION
					  ,(CAR PSEUDO-KEYNAMES))))
	 (LET* ,KEYCHECKS
	   ,@decls
	   ((LAMBDA ,AUXVARS . ,BODY)))))))

(DEFUN (:PROPERTY KLUDGEY-COMPILATION-VARIABLE-LOCATION P1) (FORM &AUX TEM TEM1)
  (SETQ FORM (CADR FORM))
  (SETQ TEM (COND ((SETQ TEM1 (ASSQ FORM VARS))
		   (AND (EQ (VAR-KIND TEM1) 'FEF-ARG-FREE)
			(ZEROP (VAR-USE-COUNT TEM1))
			(PUSH (VAR-NAME TEM1) FREEVARS))
		   (VAR-LAP-ADDRESS TEM1))
		  ((SPECIALP FORM) FORM)
		  (T (BARF form "Lossage in keyed-lambda compilation" 'BARF))))
  (IF (SYMBOLP TEM)
      `(%EXTERNAL-VALUE-CELL ',TEM)
    `(VARIABLE-LOCATION ,TEM)))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun decode-keyword-arglist (lambda-list &optional for-apply-lambda)
  (declare (values positional-args keyword-args auxvars
		   rest-arg positional-arg-names
		   keykeys keynames keyinits keyflags allow-other-keys))
  (let (positional-args keyword-args auxvars
	this-rest rest-arg positional-arg-names
	keykeys keynames keyinits keyflags allow-other-keys)
    (setq auxvars (memq '&aux lambda-list))
    (when (not for-apply-lambda)
      (setq positional-args (ldiff lambda-list auxvars))
      (setq keyword-args (memq '&key positional-args))
      (setq positional-args (ldiff positional-args keyword-args))
      (setq keyword-args (ldiff keyword-args auxvars))
      ;; Get names of all positional args and their supplied-flags.
      ;; Get name of rest arg if any.  Find out whether they end optional.
      (dolist (a positional-args)
	(cond ((eq a '&rest) (setq this-rest t))
	      ((memq a lambda-list-keywords))
	      (t (cond ((symbolp a) (push a positional-arg-names))
		       (t (and (cddr a) (push (caddr a) positional-arg-names))
			  (push (car a) positional-arg-names)))
		 (and this-rest (not rest-arg)
		      (setq rest-arg (car positional-arg-names))))))
      (setq positional-arg-names (nreverse positional-arg-names)))
    ;; Decode the keyword args.  Set up keynames, keyinits, keykeys, keyflags.
    (dolist (a (cdr (memq '&key lambda-list)))
      (cond ((eq a '&aux) (return))
	    ((eq a '&allow-other-keys) (setq allow-other-keys t))
	    ((memq a lambda-list-keywords))
	    (t (let (keyname keyinit keyflag keykey)
		 (if (and (consp a) (consp (car a)))	;((:foo foo) bar)
		     ;; Key symbol specified explicitly.
		     (setq keykey (caar a) keyname (cadar a))
		   ;; Else determine it from the variable name.
		   (setq keyname (if (consp a) (car a) a))	;(foo bar)
		   (unless (setq keykey (get keyname 'keykey))
		     (setq keykey (intern (symbol-name keyname) pkg-keyword-package))
		     (putprop keyname keykey 'keykey)))
		 (if (consp a) (setq keyinit (cadr a) keyflag (caddr a)))
		 (push keyname keynames)
		 (push keyinit keyinits)
		 (push keyflag keyflags)
		 (push keykey keykeys)))))
    ;; Get everything about the keyword args back into forward order.
    (setq keynames (nreverse keynames)
	  keyinits (nreverse keyinits)
	  keykeys (nreverse keykeys)
	  keyflags (nreverse keyflags))
    (values positional-args keyword-args auxvars
	    rest-arg positional-arg-names
	    keykeys keynames keyinits keyflags allow-other-keys)))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun apply-lambda (fctn a-value-list)
  (prog (tem)
	(unless (consp fctn) (go bad-function))
   tail-recurse
	(case (car fctn)
	  (curry-after
	   (tagbody
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
	  (curry-before
	   (tagbody
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
	  ((lambda named-lambda subst cli:subst named-subst)
	   (let-if (memq (car fctn) '(named-lambda named-subst))
		   ((*interpreter-variable-environment* nil)
		    (*interpreter-function-environment* nil)
		    (*interpreter-frame-environment* nil)
;		    (local-declarations nil)
		    )
	     (let* (optionalf quoteflag tem restf init this-restf specialf
		    (fctn1 (cond ((eq (car fctn) 'named-lambda) (cdr fctn))
				 ((eq (car fctn) 'named-subst) (cdr fctn))
				 (t fctn)))
		    (lambda-list (cadr fctn1))
		    (body (cddr fctn1))
		    (value-list a-value-list)
;		    (local-declarations local-declarations)
		    this-specialp thisval  ;Used by expansion of apply-lambda-bindvar
		    keynames keyinits keykeys keyflags
		    keynames1 keykeys1 keyflags1 (unspecified '(()))
		    allow-other-keys)
	       (and (cdr body) (stringp (car body)) (pop body))	;and doc string.
	       ;; Make a binding frame to represent any SPECIAL declarations.
	       (with-stack-list* (vars-env nil *interpreter-variable-environment*)
		 ;; Find any declarations at the front of the function body
		 ;; and put them onto VARS-ENV ;;(and LOCAL-DECLARATIONS)
		 (gobble-declarations-internal body vars-env)
		 (with-stack-list* (*interpreter-variable-environment* nil vars-env)
		   (prog (thisvar)		; THISVAR is name of argument being processed.
			 ;; If SELF is an instance, and instance vars aren't bound, bind them.
			 (when (and (typep self 'instance)
				    (not (interpreter-instance-vars-boundp self)))
			   ;;??? Here should take care of special instance variables!!!
			   ;; Probably just omit them, since they were bound when
			   ;; the message was sent, weren't they?
			   (tagbody
			       (setq tem (self-binding-instances))
			    loop
			       (when tem
				 (apply-lambda-bindvar-1 (car tem) (cadr tem))
				 (setq tem (cddr tem))
				 (go loop)))
			   (apply-lambda-bindvar-1
			     ;; all this to avoid a compiler warning...
			     (locally
			       (declare (special slots-bound-instance-1))
			       (inhibit-style-warnings
				 (locf (symbol-value 'slots-bound-instance-1))))
			     self)
			 )
		      l
			 (cond ((null value-list) (go lp1))
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
				(go l1))	;Do next value.
			       ((eq (car lambda-list) '&quote)
				(setq quoteflag t)
				(go l1))
			       ((eq (car lambda-list) '&eval)
				(setq quoteflag nil)
				(go l1))
			       ((memq (car lambda-list) '(&special &local))
				(setq specialf (eq (car lambda-list) '&special))
				(go l1))
			       ((eq (car lambda-list) '&rest)
				(setq this-restf t)
				(go l1))	;Do next value.
			       ((memq (car lambda-list) lambda-list-keywords)
				(go l1))
			       ((atom (car lambda-list))
				(setq thisvar (car lambda-list)))
			       ((atom (caar lambda-list))
				(setq thisvar (caar lambda-list))
				;; If it's &OPTIONAL (FOO NIL FOOP),
				;; bind FOOP to T since FOO was specified.
				(when (and optionalf (cddar lambda-list))
				  (and (null (caddar lambda-list)) (go bad-lambda-list))
				  (apply-lambda-bindvar (caddar lambda-list)
							t vars-env specialf)))
			       (t (go bad-lambda-list)))
			 ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
			 ;;  It is in THISVAR.
			 (and (null thisvar) (go bad-lambda-list))
			 (cond (restf
				;; Something follows a &REST arg???
				(go bad-lambda-list))
			       (this-restf	;This IS the &REST arg.
				;; If quoted arg, and the list of values is in a pdl, copy it.
				(and quoteflag
				     (ldb-test %%pht2-map-access-code
					       (area-region-bits (%area-number value-list)))
				     (let ((default-cons-area background-cons-area))
				       (setq value-list (copylist value-list))))
				(apply-lambda-bindvar thisvar value-list vars-env specialf)
				;; We don't clear out VALUE-LIST
				;; in case keyword args follow.
				(setq this-restf nil restf t)
				(go l1)))

			 (apply-lambda-bindvar thisvar (car value-list) vars-env specialf)
			 (pop value-list)
		      l1 (pop lambda-list)
			 (go l)

		      key
			 (setf (values nil nil lambda-list nil nil
				       keykeys keynames keyinits keyflags
				       allow-other-keys)
			       (decode-keyword-arglist lambda-list t))
			 ;; Process the special keyword :ALLOW-OTHER-KEYS if present as arg.
			 (if (getf value-list ':allow-other-keys)
			     (setq allow-other-keys t))

			 (setq keykeys1 keykeys	;life is tough without LET...
			       keynames1 keynames
			       keyflags1 keyflags)
		      key1
			 (when keykeys1
			   (setq tem (getf value-list (pop keykeys1) unspecified))
			   (setq init (if (eq tem unspecified) (eval1 (car keyinits)) tem))
			   (apply-lambda-bindvar (car keynames1) init vars-env)
			   (if (car keyflags1)
			       (apply-lambda-bindvar (car keyflags1)
						     (neq tem unspecified)
						     vars-env))
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
			       (setq keyword (cerror :new-keyword nil
						     'sys:undefined-keyword-argument
						     "Keyword arg keyword ~S, with value ~S, is unrecognized."
						     keyword
						     (cadr value-list)))
			       (when (and keyword
					  (setq tem (find-position-in-list keyword keykeys)))
				 (interpreter-set (nth tem keynames) (cadr x))
				 (and (setq tem (nth tem keyflags))
				      (interpreter-set tem t))
				 (return)))))
			 ;; Keyword args always use up all the values that are left...

			 ;; Here when all values used up.
		      lp1
			 (cond ((null lambda-list) (go ex1))
			       ((eq (car lambda-list) '&rest)
				(and restf (go bad-lambda-list))
				(setq this-restf t)
				(go lp2))
			       ((eq (car lambda-list) '&key)
				(go key))
			       ((memq (car lambda-list) '(&optional &aux))
				(setq optionalf t)	;Suppress too few args error
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
						     "Function ~S called with only ~D argument~1@*~P."
						     fctn (length a-value-list) a-value-list))
				    (:additional-arguments
				     (apply fctn (append a-value-list args)))
				    (:return-value args)
				    (:new-argument-list (apply fctn args)))))
			       ((atom (car lambda-list)) (setq tem (car lambda-list))
							 (setq init nil))
			       ((atom (caar lambda-list))
				(setq tem (caar lambda-list))
				(setq init (eval1 (cadar lambda-list)))
				;; For (FOO NIL FOOP), bind FOOP to NIL since FOO missing.
				(when (cddar lambda-list)
				  (and (null (caddar lambda-list)) (go bad-lambda-list))
				  (apply-lambda-bindvar (caddar lambda-list)
							nil vars-env specialf)))
			       (t (go bad-lambda-list)))
		      lp3
			 (and (null tem) (go bad-lambda-list))
			 (apply-lambda-bindvar tem init vars-env specialf)
			 (and this-restf (setq restf t))
			 (setq this-restf nil)
		      lp2
			 (setq lambda-list (cdr lambda-list))
			 (go lp1)

		      ex1
			 ;; Here to evaluate the body.
			 (return-from apply-lambda (eval-body body))
		      bad-lambda-list
			 (setq fctn
			       (cerror :new-function nil 'sys:invalid-lambda-list
				       "~S has an invalid LAMBDA list" fctn))
		      retry
			 (return-from apply-lambda (apply fctn a-value-list))))))))
	  (macro
	   (ferror 'sys:funcall-macro
		   "Funcalling the macro ~S."
		   (function-name (cdr fctn)))
	   (return-from apply-lambda
	     (eval1 (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list))))))

	;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
	(when (lambda-macro-call-p fctn)
	  (setq fctn (lambda-macro-expand fctn))
	  (go retry))

   bad-function
	;; Can drop through to here for a totally unrecognized function.
	(setq fctn
	      (cerror :new-function nil 'sys:invalid-function
		      "~S is an invalid function." fctn))
	(go retry)

	;; Errors jump out of the inner PROG to unbind any lambda-vars bound with %BIND.
   bad-lambda-list
	(setq fctn
	      (cerror :new-function nil 'sys:invalid-lambda-list
		      "~S has an invalid LAMBDA list" fctn))
   retry
	(and (consp fctn) (go tail-recurse))
	(return (apply fctn a-value-list))

   too-few-args
	(return (signal-proceed-case
		  ((args)
		   (make-condition 'sys:too-few-arguments
				   "Function ~S called with only ~D argument~1@*~P."
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


; From file NUMDEF.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defsubst numeric-contage (ans influencer)
  "Return a number = ANS and of the weakest type stronger than both ANS and INFLUENCER."
  (+ ans (- influencer influencer)))

))


; From file NUMER.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(defun expt-hard (base-number power-number)
  (cond ;; ((eq power-number 0)			;integer 0
	;;  (numeric-contage 1 base-number))
	((= power-number 0)
	 ;; (if (zerop base-number)
	 ;; (ferror 'sys:illegal-expt base-number power-number
	 ;;     "An attempt was made to raise zero to the power of a non-integer zero ~*~D")
	   (numeric-contage (numeric-contage 1 power-number) base-number))
	;;)
	((zerop base-number)
	 (if (plusp (realpart power-number))
	     (numeric-contage base-number power-number)
	   (ferror 'sys:illegal-expt base-number power-number
		   "An attempt was made to raise zero to power ~*~D, whose real part is not > 0")))
	((integerp power-number)
	 (let ((minusp (minusp power-number)))
	   (setq power-number (abs power-number))
	   (do ((ans (if (oddp power-number) base-number (numeric-contage 1 base-number))
		     (if (oddp power-number) (* ans base-number) ans)))
	       ((zerop (setq power-number (ash power-number -1)))
		(if minusp (cli:// ans) ans))
	     ;; to avoid overflow, procrastinate squaring
	     (setq base-number (* base-number base-number)))))
	;; this is a truly losing algorithm ...
	(t (exp (* power-number (log base-number))))))

))

; From file NUMER.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(defun sqrt (number)
  "Square root of a number, as a float or complex.
Result is a short-float or complex short-float according to type of NUMBER"
  (let* ((n (if (complexp number)
		(if (zerop (%complex-imag-part number))
		    (float (%complex-real-part number))
		  (%complex-cons (float (%complex-real-part number))
				 (float (%complex-imag-part number))))
	      (float number)))
	 (val
	   (cond ((complexp n)
		  (let* ((abs (abs n))
			 (real (%complex-real-part n))
			 (imag (%complex-imag-part n))
			 (r (sqrt (// (+ real abs) 2))))
		    (%complex-cons r (// imag (+ r r)))))
		 ((< n 0.0)
		  (%complex-cons (- n n) (sqrt (- n))))
		 ((= n 0.0)
		  0.0)
		 (t
		  (let ((f (+ n 0.0f0))
			(i2 (%float-double 0 1))	;cons up a new one -- gets munged
			(exp (- (%single-float-exponent n) single-float-exponent-offset
				-2)))
                    (setf (%single-float-exponent f) single-float-exponent-offset)
                    (setf (%single-float-exponent i2)                            
                          (+ single-float-exponent-offset
			     (if (oddp exp)
				 (1+ (dpb (ldb #o0127 exp) #o0027 exp))
			         (dpb (ldb #o0127 exp) #o0027 exp))))
		    (do ((i 0 (1+ i))
			 (an (* i2 (+ 0.4826004 f (if (oddp exp) -0.25 0.0)))))
			((= i 4) an)
		      (setq an (* 0.5 (+ an (// n an))))))))))
    (if (complexp number)
	(if (typep (%complex-real-part number) 'short-float)
	    (%complex-cons (float (%complex-real-part val) 0s0)
			   (float (%complex-imag-part val) 0s0))
	  val)
      (if (typep number 'short-float)
	  (short-float val)
	val))))

))

; From file NUMER.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(defun log (n &optional b &aux zero)
  "Log of N base BASE, which defaults to e
/(ie by default this is the /"natural/" logarithm function. Supply BASE for an unnatural log."
  (declare (arglist n &optional (base (exp 1))))
  (unless (typep n 'float) (setq n (float n 0f0)))
  (setq zero (- n n))
  (when b
    (if (and (zerop b) (not (zerop n)))
	(return-from log (numeric-contage b n))
      (setq zero (numeric-contage zero b))))
  (setq n (log-aux n))
  (if b (setq n (// n (log-aux b))))
  (float-coerce n zero))

))

; From file LTOP.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(defun eval-abort-trivial-errors (top-level-form)
  "Evaluate TOP-LEVEL-FORM, returning the value, but aborting on trivial errors.
A trivial error is one involving a symbol present in the form itself.
Aborting is done by signaling SYS:ABORT, like the Abort key.
The user gets to choose whether to do that or to enter the debugger as usual."
  (declare (special top-level-form))
  (condition-bind (((sys:too-few-arguments sys:too-many-arguments
		     sys:cell-contents-error sys:wrong-type-argument
		     sys:invalid-function-spec sys:unclaimed-message)
		    'eval-abort-trivial-errors-handler))
    ;; Eval, making all free variable references special
    (eval-special-ok top-level-form)))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN VAR-MAKE-HOME (NAME TYPE KIND INIT-SPECS
		      EVAL-TYPE MISC-TYPES THIS-FRAME-DECLARATIONS &AUX HOME)
  (COND ((NULL (MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST
			    FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
	 (BARF KIND 'BAD-KIND 'BARF))
	((KEYWORDP NAME)
	 (WARN 'KEYWORD-BOUND :IMPOSSIBLE
	       "Binding the keyword symbol ~S." NAME))
	((CONSTANTP NAME)
	 (WARN 'SYSTEM-CONSTANT-BOUND :IMPLAUSIBLE
	       "Binding ~S, which is a constant." NAME))
	((AND (MEMQ NAME (CDDR SELF-FLAVOR-DECLARATION))
	      (EQ TYPE 'FEF-LOCAL))
	 (WARN 'INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE
	       "Rebinding the instance variable ~S.  The new binding will be local."
	       NAME)))
  ;; Rest args interfere with fast arg option except when there are no specials.
  ;; We need to look at this to
  ;;  decide how to process all the AUX variables and can't tell when processing
  ;;  the first one whether the next will be special.
  ;;  In any case, being wrong about this should not be able to produce
  ;;  incorrect code.
  (COND ((EQ KIND 'FEF-ARG-REST)
	 (SETQ FAST-ARGS-POSSIBLE NIL))
	((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
	 (AND INIT-SPECS (SETQ FAST-ARGS-POSSIBLE NIL))))
  ;; Detect vars bound to themselves which fail to be special.
  (WHEN (AND (EQ NAME (CAR INIT-SPECS))
	     (NOT (ASSQ NAME VARS))
	     ;; If variable is already accessible lexically, it need not be special.
	     (DOLIST (FRAME *OUTER-CONTEXT-VARIABLE-ENVIRONMENT* T)
	       (WHEN (ASSQ NAME FRAME) (RETURN NIL))))
    (MSPL2 NAME)
    (SETQ TYPE 'FEF-SPECIAL))
  ;; Cons up the variable descriptor.
  ;; Note that INIT-SPECS is not the final value that will go in the INIT slot.
  (SETQ HOME (MAKE-VAR :NAME NAME :KIND KIND :TYPE TYPE
		       :INIT INIT-SPECS :EVAL EVAL-TYPE :MISC MISC-TYPES
		       :DECLARATIONS (DECLARATIONS-FOR-VARIABLE NAME THIS-FRAME-DECLARATIONS)))
  (IF (AND (EQ TYPE 'FEF-SPECIAL) (GETF (VAR-DECLARATIONS HOME) 'IGNORE))
      (WARN 'NOT-IGNORED :IMPLAUSIBLE
	    "The special variable ~S was declared to be ignored" NAME))
  (SETF (VAR-LAP-ADDRESS HOME)
	;; Not the real lap address,
	;; but something for P1 to use for the value of the variable
	(IF (EQ TYPE 'FEF-SPECIAL) NAME `(LOCAL-REF ,HOME)))
  HOME)

))

; From file INSPCT.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN INSPECT-ARRAY-PRINTER (ITEM ARG STREAM ITEM-NUMBER
			      &AUX (OBJ (CAR ARG))
			      (LEADER-LENGTH-TO-MENTION
				(OR (AND (CADR ARG) (ARRAY-LEADER-LENGTH OBJ)) 0)))
  "The print-function used when inspecting an array."
  ;; (CAR ARG) is the array.  (CADR ARG) is T to display the leader.
  ;; ITEM is usually a number.  A small number is an index in the leader.
  ;; Numbers too big for that start moving through the array elements.
  (COND ((NOT (NUMBERP ITEM))
	 (INSPECT-PRINTER ITEM OBJ STREAM ITEM-NUMBER))
	((< ITEM LEADER-LENGTH-TO-MENTION)
	 (SEND STREAM ':ITEM1 ITEM 'LEADER-SLOT
	       #'(LAMBDA (ITEM STREAM)
		   (FORMAT STREAM "Leader ~D" ITEM)))
	 (FORMAT STREAM ":~12T ")
	 (SEND STREAM ':ITEM1 (ARRAY-LEADER OBJ ITEM) ':VALUE 'PRINT-ITEM-CONCISELY))
	(T
	 (LET ((ITEM (- ITEM LEADER-LENGTH-TO-MENTION))
	       (RANK (ARRAY-RANK OBJ))
	       INDICES)
	   (OR (= RANK 1) (SETQ INDICES (ARRAY-INDICES-FROM-INDEX OBJ ITEM)))
	   (SEND STREAM ':ITEM1 (CONS ITEM (IF (= RANK 1) ITEM INDICES)) 'ARRAY-SLOT
		 #'(LAMBDA (DATUM STREAM)
		     (FORMAT STREAM "Elt ~D" (CDR DATUM))))
	   (FORMAT STREAM ":~9T ")
	   (IF (OR (CDR (ASSQ (ARRAY-TYPE OBJ) ARRAY-BITS-PER-ELEMENT))
		   (%P-CONTENTS-SAFE-P (AP-1-FORCE OBJ ITEM)))
	       ;; Deal with data types that are objects, and with numeric arrays.
	       (SEND STREAM
		     ':ITEM1 (CLI:AR-1-FORCE OBJ ITEM)
		     ':VALUE 'PRINT-ITEM-CONCISELY)
	     ;; Deal with data types that aren't really objects.
	     (FORMAT STREAM "#<~A ~O>"
		     (OR (NTH (%P-DATA-TYPE (AP-1-FORCE OBJ ITEM)) Q-DATA-TYPES)
			 (%P-DATA-TYPE (AP-1-FORCE OBJ ITEM)))
		     (%P-POINTER (AP-1-FORCE OBJ ITEM))))))))

))

; From file LOGIN.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(DEFUN LOGIN-SETQ (&QUOTE &REST L)		;Undoing SETQ
  "Like SETQ, but the changes are undone by logging out."
  (DO ((L L (CDDR L)))
      ((NULL L))
    (IF (BOUNDP (CAR L))
	(PUSH `(SETQ ,(CAR L) ',(SYMBOL-VALUE (CAR L))) LOGOUT-LIST)
        (PUSH `(MAKUNBOUND ',(CAR L)) LOGOUT-LIST))
    (SET (CAR L) (EVAL1 (CADR L)))))

))

; From file QCDEFS.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(defmacro defrewrite (rewriter function-to-rewrite
		      &optional ((&rest rewrites-into)) arglist &body body)
  `(progn (add-optimizer-internal ',function-to-rewrite ',rewriter ',rewrites-into)
	  (defun ,rewriter ,arglist
	      (declare (function-parent ,rewriter defrewrite))
	      . ,body)))

))

; From file QCDEFS.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(defmacro defcompiler-synonym (function synonym-function)
  "Make the compiler substitute SYNONYM-FUNCTION for FUNCTION when compiling.
eg (defcompiler-synonym plus +)"
  `(defrewrite ,(intern (string-append function "-TO-" synonym-function)) ,function
	       (,synonym-function) (form)
     (cons ',synonym-function (cdr form))))

))

; From file QCOPT.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defrewrite break-decruftify break () (form)
  (if (not (and (symbolp (cadr-safe form)) (not (constantp (cadr-safe form)))))
      form
    (warn 'break-arg ':obsolete
	  "A symbol as the first argument to BREAK is an obsolete construct;
change it to a string before it stops working: ~S" form)
    `(break ',(cadr form))))

))
(remprop 'break 'compiler::style-checker)

; From file ACCESS.LISP OZ:<L.IO.FILE> OZ:
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFMETHOD (CASTE-FILE-HOST-MIXIN :ENABLE-CAPABILITIES) (&REST CAPABILITIES)
  (SEND SELF :ACCESS-OPERATION :ENABLE-CAPABILITIES
	(OR CAPABILITIES (SEND SELF :DEFAULT-CAPABILITIES))))

))

; From file DIRED.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-PRINTABLE-FILE-P (LINE &AUX PLIST PATHNAME TYPE BYTE)
  "T if the file on LINE seems to be one that can be hardcopied reasonably."
  (SETQ PLIST (LOCF (LINE-PLIST LINE))
	PATHNAME (GET PLIST :PATHNAME)
	TYPE (SEND PATHNAME :CANONICAL-TYPE))
  (AND (NOT (SYS:MEMBER-EQUAL TYPE '(:QFASL "BIN" "DRW" "WD" "FASL" "KST" ":EJ" :WIDTHS
				     "OUTPUT")))	;others?
       (OR (EQUAL TYPE "PLT")
	   (EQ TYPE :PRESS)
	   ;; This is probably a text file, skip open. The NIL is for VMS, which can't tell
	   (MEMQ (GET PLIST :BYTE-SIZE) '(7 8 NIL))
	   (WITH-OPEN-FILE (STREAM PATHNAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 9.)
	     (DOTIMES (I 4) (SETQ BYTE (SEND STREAM :TYI)))
	     (AND BYTE (NOT (BIT-TEST BYTE 1)))))))

))

(setf (si::pkg-prefix-print-name (find-package 'sys)) "SYS")

; From file INSPCT.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN INSPECT-COMMAND-LOOP (FRAME &AUX USER IS HISTORY)
  (SEND (SETQ USER (SEND FRAME :GET-PANE 'INTERACTOR)) :CLEAR-SCREEN)
  (SEND (CAR (SETQ IS (SEND FRAME :INSPECTORS))) :FLUSH-TYPEOUT)
  (SEND USER :SET-OLD-TYPEAHEAD NIL)
  (SETQ HISTORY (SEND FRAME :GET-PANE 'HISTORY))
  ;; Flush remnants of modify mode
  (SEND HISTORY :SET-SENSITIVE-ITEM-TYPES T)
  (DOLIST (I IS)
    (SEND I :SET-MODIFY-MODE NIL))
  (LET* ((TYPEOUT-WINDOW (SEND FRAME :TYPEOUT-WINDOW))
	 (*TERMINAL-IO* TYPEOUT-WINDOW)
	 * ** *** + ++ +++ \
	 *PRINT-ARRAY*
	 (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
	 (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
	 (TV:KBD-INTERCEPTED-CHARACTERS
	   (REMOVE (ASSQ #/BREAK TV:KBD-INTERCEPTED-CHARACTERS)
		   TV:KBD-INTERCEPTED-CHARACTERS))
	 (THING) (TOP-ITEM))
    (DECLARE (SPECIAL \))
    (DO-NAMED INSPECTOR ()
	      (())
      (LET ((ITEMS (SEND HISTORY :ITEMS))
	    (IW)
	    (IDX))
	(SETQ IDX (ARRAY-ACTIVE-LENGTH ITEMS))
	;; Make sure the inspection windows reflect the state of the history buffer
	(DOLIST (I IS)
	  ;; Update datastructure to reflect current TOP-ITEMs
	  (LET ((DISP (SEND I :CURRENT-DISPLAY)))
	    (AND DISP (SETF (FOURTH DISP) (SEND I :TOP-ITEM)))))
	(DOTIMES (I (LENGTH IS))
	  (SETQ IDX (1- IDX))
	  (SETQ IW (NTH I IS))
	  (COND ((< IDX 0)
		 (SEND IW :SET-CURRENT-DISPLAY
		       (SEND IW :SETUP
			     `(INSPECT-PRINTER NIL NIL NIL
					       (NIL NIL NIL NIL
							  ,(LABEL-FONT (SEND IW :LABEL))
							  "Empty"))))
		 (SEND IW :SET-CURRENT-OBJECT (NCONS NIL)))
		(T (SEND HISTORY :INSPECT-OBJECT (AREF ITEMS IDX) IW TOP-ITEM NIL T)
		   (SETQ TOP-ITEM NIL)))))
      
      ;; Insure last item in history is on the screen
      (SEND HISTORY :PUT-LAST-ITEM-IN-WINDOW)
      
      ;; Give *, ** and *** the right values.
      (SETQ *PRINT-ARRAY* NIL)
      (LET* ((ITEMS (SEND HISTORY :ITEMS))
	     (NITEMS (IF ITEMS (ARRAY-ACTIVE-LENGTH ITEMS) 0)))
	(AND ( NITEMS 1) (SETQ * (AREF ITEMS (- NITEMS 1))))
	(AND ( NITEMS 2) (SETQ ** (AREF ITEMS (- NITEMS 2))))
	(AND ( NITEMS 3) (SETQ *** (AREF ITEMS (- NITEMS 3)))))
      
      ;; Get input.
      ;; Keyboard commands are processed inside this loop.
      ;; Mouse commands exit the loop and go round the outer loop.
      (DO-FOREVER
	(SETQ THING -1)
	(SEND (CAR IS) :FLUSH-TYPEOUT)
	(SEND FRAME :SELECT-PANE USER)
	(SEND USER :FRESH-LINE)
	(OR (SEND USER :OLD-TYPEAHEAD)
	    (SETQ THING (SEND USER :ANY-TYI)))
	(TYPECASE THING
	  (CHARACTER (SETQ THING (CHAR-INT THING)))
	  (FIXNUM)
	  ;; Some sort of mouse command, just process
	  (T (RETURN)))
	(CASE THING
	  ((#/C-Z #/ABORT)
	   (SIGNAL EH:ABORT-OBJECT))
	  (#/C-V
	   (SEND (CAR IS) :SCROLL-TO
		 (- (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)) 2)
		 :RELATIVE))
	  (#/M-V
	   (SEND (CAR IS) :SCROLL-TO
		 (- 2 (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)))
		 :RELATIVE))
	  (#/BREAK
	   (SEND FRAME :SELECT-PANE (CAR IS))
	   (SEND *TERMINAL-IO* :EXPOSE-FOR-TYPEOUT)
	   (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to inspector command loop.")
	     (BREAK 'INSPECT))
	   (SEND *TERMINAL-IO* :MAKE-COMPLETE))
	  ;; Clear-Screen decaches.
	  (#/CLEAR-SCREEN
	   (SEND HISTORY :SET-CACHE NIL)
	   (SEND FRAME :CLEAR-SCREEN)
	   (SEND FRAME :REFRESH :COMPLETE-REDISPLAY))
	  ;; End returns *.
	  (#/END
	   (RETURN-FROM INSPECTOR *))
	  (#/HELP
	   (INSPECT-HELP)
	   (FORMAT *TERMINAL-IO* "~%Type any character to continue:")
	   (LET ((CH (SEND USER :ANY-TYI)))
	     (OR (= CH #/SP)
		 (SEND USER :UNTYI CH))))
	  (#/DELETE
	   (RETURN (SEND HISTORY :FLUSH-CONTENTS)))
	  ;;set \
	  (#/C-\
	   (FORMAT USER "~&Value to set \ to ")
	   (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
	       (INSPECT-GET-VALUE-FROM-USER USER)
	     (OR PUNT-P (SETQ \ VALUE))))
	  (#/RUBOUT)
	  (#/QUOTE
	   (LET ((*TERMINAL-IO* USER)
		 FLAG)
	     (FORMAT USER "Eval: ")
	     (MULTIPLE-VALUE-SETQ (THING FLAG)
	       (SEND USER :RUBOUT-HANDLER
			'((:FULL-RUBOUT :FULL-RUBOUT) (:ACTIVATION = #/END))
			'SI:READ-FOR-TOP-LEVEL))
	     (UNLESS (EQ FLAG :FULL-RUBOUT)
	       (SETQ +++ ++ ++ + + THING)
	       (MULTIPLE-VALUE-SETQ (THING FLAG)
		 (CATCH-ERROR (SI:EVAL-SPECIAL-OK THING)))
	       (OR FLAG
		   (LET ((*PRINT-LEVEL* 3) (*PRINT-LENGTH* 5))
		     (PRINT THING USER))))))
	  (OTHERWISE
	   (LET ((*TERMINAL-IO* USER)
		 FLAG)
	     (AND ( THING 0) (SEND USER :UNTYI THING))
	     (MULTIPLE-VALUE (THING FLAG)
	       (SEND USER :PREEMPTABLE-READ
		     '((:FULL-RUBOUT :FULL-RUBOUT) (:ACTIVATION = #/END))
		     'SI:READ-FOR-TOP-LEVEL))
	     (COND ((EQ FLAG :MOUSE-CHAR) (RETURN))
		   ((NEQ FLAG :FULL-RUBOUT)
		    (SETQ +++ ++ ++ + + THING)
		    (MULTIPLE-VALUE-SETQ (THING FLAG)
		      (CATCH-ERROR (SI:EVAL-SPECIAL-OK THING)))
		    (OR FLAG
			(RETURN (SETQ THING `(:VALUE ,THING ,HISTORY))))))))))
      (CATCH-ERROR-RESTART (SYS:ABORT "Return to inspector command loop.")
	(COND
	  ((ATOM THING))
	  ((EQ (CAR THING) :MOUSE-BUTTON))	;random rodentry
	  ((EQ (CAR THING) :MENU)
	   (SETF (SECOND THING) (SEND (FOURTH THING) :EXECUTE (SECOND THING)))
	   (SELECTQ (SECOND THING)
	     (:EXIT (RETURN *))
	     (:RETURN
	      (FORMAT USER "~&Value to return ")
	      (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
		  (INSPECT-GET-VALUE-FROM-USER USER)
		(OR PUNT-P (RETURN VALUE))))
	     (:FLUSH-CACHE
	      (SEND HISTORY :SET-CACHE NIL))
	     (:MODIFY
	      (SETQ TOP-ITEM (INSPECT-MODIFY-OBJECT USER HISTORY IS)))
	     (:CLEAR
	      (SEND HISTORY :FLUSH-CONTENTS))
	     (:SET-\
	      (FORMAT USER "~&Value to set \ to ")
	      (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
		  (INSPECT-GET-VALUE-FROM-USER USER)
		(OR PUNT-P (SETQ \ VALUE))))
	     (OTHERWISE (FORMAT USER "~&Unimplemented menu command ~A~%" (SECOND THING)))))
	  (T
	   (COND ((NULL (FIRST THING))
		  ;; Type is NIL -- nothing under mouse
		  (BEEP))
		 ((AND (EQ (FIRST THING) :LINE-AREA) (EQ (FOURTH THING) #/MOUSE-2-1))
		  ;; Delete from line area
		  (SEND HISTORY :FLUSH-OBJECT (INSPECT-REAL-VALUE THING)))
		 ((AND (EQ (FOURTH THING) #/MOUSE-2-1)
		       (MEMQ (THIRD THING) IS))
		  ;; Middle click means leave source in one of the windows
		  (LET ((1ST-THING (INSPECT-REAL-VALUE THING))
			(2ND-THING (SEND (THIRD THING) :CURRENT-OBJECT)))
		    ;; First flush item we will be inspecting
		    (INSPECT-FLUSH-FROM-HISTORY 1ST-THING HISTORY)
		    (INSPECT-FLUSH-FROM-HISTORY 2ND-THING HISTORY)
		    (SEND HISTORY :APPEND-ITEM 2ND-THING)
		    (SEND HISTORY :APPEND-ITEM 1ST-THING)))
		 ((EQ (FOURTH THING) #/MOUSE-3-1)
		  ;; Click on right button -- try to find function
		  (SETQ THING (INSPECT-FIND-FUNCTION (INSPECT-REAL-VALUE THING)))
		  (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
		  (SEND HISTORY :APPEND-ITEM THING))
		 ((CHAR-BIT (FOURTH THING) :HYPER)
		  ;; HYPER means modify the slot we are pointing at.
		  (LET ((*TERMINAL-IO* (THIRD THING)))
		    (IF (OR (NULL (FIRST THING)) (NULL (GET (FIRST THING) 'SET-FUNCTION)))
			(FORMAT *TERMINAL-IO* "~&Cannot set this component.")
		      (INSPECT-SET-SLOT THING HISTORY USER))))
		 (T
		  ;; Otherwise inspect the thing we are pointing at.
		  (SETQ THING (INSPECT-REAL-VALUE THING))
		  (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
		  (SEND HISTORY :APPEND-ITEM THING)))))))))

))

; From file INSPCT.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN INSPECT-GET-VALUE-FROM-USER (*TERMINAL-IO*)
  "Get a value either by the mouse pointing at it or by read and eval on *TERMINAL-IO*."
  (PROG ()
	(FORMAT *TERMINAL-IO* "(type a form to be evalled~%or select something with mouse):~&")
	(LET ((THING (SEND *TERMINAL-IO* :ANY-TYI)) ERROR)
	  (COND ((CONSP THING)
		 ;; Choose somthing with the mouse -- display it truncated and proceed
		 (COND ((EQ (FIRST THING) :MENU)
			(FORMAT *TERMINAL-IO* "~&Cannot set value from the menu~%")
			(RETURN NIL T)))
		 (LET ((*PRINT-LEVEL* 3) (*PRINT-LENGTH* 5))
		   (PRIN1 (SETQ THING (INSPECT-REAL-VALUE THING)) *TERMINAL-IO*)))
		(T
		 (SEND *TERMINAL-IO* :UNTYI THING)
		 (MULTIPLE-VALUE-SETQ (THING ERROR)
		   (CATCH-ERROR (SI:EVAL-SPECIAL-OK (LET ((*STANDARD-INPUT* *TERMINAL-IO*))
						      (SI:READ-FOR-TOP-LEVEL)))))
		 (IF ERROR (RETURN NIL T))))	;Failed to eval, punt
	  (TERPRI *TERMINAL-IO*)
	  (RETURN THING))))

))

; From file UNFASL.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; UNFASL  "

(DEFUN UNFASL (INPUT-FILE &OPTIONAL OUTPUT-FILE)
  "Write a description of the contents of QFASL file INPUT-FILE into OUTPUT-FILE.
The output file defaults to same name as input, with type = UNFASL."
  (SETQ INPUT-FILE (FS:MERGE-AND-SET-PATHNAME-DEFAULTS INPUT-FILE FS:LOAD-PATHNAME-DEFAULTS
						       :QFASL)
	OUTPUT-FILE (SEND (IF OUTPUT-FILE
			      (FS:MERGE-PATHNAME-DEFAULTS OUTPUT-FILE INPUT-FILE)
			      INPUT-FILE)
			  :NEW-TYPE :UNFASL))
  (OR (BOUNDP 'UNFASL-GROUP-DISPATCH) (INITIALIZE-UNFASL-ENVIRONMENT))
  (WITH-OPEN-FILE (UNFASL-FILE INPUT-FILE :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
    (OR (AND (= (UNFASL-NIBBLE) #o143150)	;Check magic ID
	     (= (UNFASL-NIBBLE) #o71660))
	(FERROR NIL "~A not a qfasl file" INPUT-FILE))
    (WITH-OPEN-FILE (*STANDARD-OUTPUT* OUTPUT-FILE :DIRECTION :OUTPUT :CHARACTERS T)
      (FORMAT T "; -*- Mode:TEXT -*-~%; This is the UNFASL for ~A~2%"
	        (SEND UNFASL-FILE :TRUENAME))
      (UNFASL-TOP-LEVEL)))
  OUTPUT-FILE)

))

; From file SCRED.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRED  "

(DEFUN MOUSE-SET-WINDOW-SIZE (WINDOW &OPTIONAL (MOVE-P T) &AUX LEFT TOP RIGHT BOTTOM ERROR)
  "Ask user for new edges for WINDOW, return them, and usually set edges of WINDOW.
WINDOW's edges are set unless MOVE-P is NIL.
The values are the new edges, or NIL if the user aborted."
  (DECLARE (VALUES LEFT TOP RIGHT BOTTOM))
  (MULTIPLE-VALUE (LEFT TOP)
    (SHEET-CALCULATE-OFFSETS WINDOW MOUSE-SHEET))
  (SETQ RIGHT (+ LEFT (SHEET-WIDTH WINDOW))
	BOTTOM (+ TOP (SHEET-HEIGHT WINDOW)))
  (DO-FOREVER
    (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
      (MOUSE-SPECIFY-RECTANGLE LEFT TOP RIGHT BOTTOM (SHEET-SUPERIOR WINDOW) 0 0 T))
    (COND ((NULL LEFT)				;Aborted
	   (BEEP)				;Leave it where it is
	   (SETQ MOVE-P NIL)
	   (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM) (SEND WINDOW :EDGES))
	   (RETURN))
	  ((NOT (MULTIPLE-VALUE-SETQ (NIL ERROR)
		  (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM :VERIFY)))
	   ;; Edges no good, try again
	   (BEEP)
	   (POP-UP-FORMAT "Illegal edges for ~S:~%~A" WINDOW ERROR))
	  (T (RETURN))))			;Good
  (AND MOVE-P (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM))
  (VALUES LEFT TOP RIGHT BOTTOM))

))

; From file HOST.LISP OZ:<L.NETWORK> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; HOST  "

(DEFUN DESCRIBE-LOGICAL-HOST (HOST &OPTIONAL (STREAM *STANDARD-OUTPUT*) &AUX OLD-HOST)
  "Print out useful things about the logical HOST onto STREAM."
  (SETQ OLD-HOST HOST)
  (SETQ HOST (FS:GET-PATHNAME-HOST HOST))
  (IF (OR (NULL HOST)
	  (NOT (SEND HOST :OPERATION-HANDLED-P :PHYSICAL-HOST)))
      (FERROR NIL "~A does not appear to be a logical host." OLD-HOST))
  (LET* ((REAL-HOST (SEND HOST :PHYSICAL-HOST))
	 (TRANSLATIONS (SEND HOST :TRANSLATIONS))
	 (INFO (SEND HOST :GET 'FS:MAKE-LOGICAL-PATHNAME-HOST)))
    (COND ((NULL TRANSLATIONS)
	   (FORMAT STREAM "~&The logical host ~A has ~A as its physical host.
There don't appear to be any translations."
		   (SEND HOST :NAME) REAL-HOST))
	  (T
	   (FORMAT STREAM
		   "~&The logical host ~A translates the following pathnames on the physical host ~A:~%"
		   (SEND HOST :NAME) REAL-HOST)
	   (DOLIST (TRANS TRANSLATIONS)
	     (FORMAT STREAM "~& ~30A  ~A" (CAR TRANS) (CADR TRANS)))))
    (if info (format stream "~%These translations were defined by the file ~A"
		     (send (send (fs:get-pathname-host "SYS") :sample-pathname)
			   :back-translated-pathname (car info)))))
  HOST)

))
