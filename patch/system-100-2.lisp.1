;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.2
;;; Reason:
;;;  APPLY-LAMBDA: Don't wrap .SLOTS.BOUND.INSTANCE. around LOCALLY.
;;; Written 4-Apr-23 16:15:16 by AMS,
;;; while running on Lisp Machine One from band 8
;;; with Experimental System 100.1, microcode 323.



; From file OZ: /home/ams/l/sys/sys/eval.lisp at 4-Apr-23 16:15:34
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//eval"

(defun apply-lambda (fctn a-value-list &optional environment &aux tem)
    (block top
      (tagbody
       tail-recurse
	  (cond ((closurep fctn)
		 (setq tem (%make-pointer dtp-list fctn))
		 (setq fctn (car tem))
		 (%using-binding-instances (cdr tem))
		 (go tail-recurse))
		((not (consp fctn))
		 (go bad-function)))
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
	     (let* (optionalf quoteflag tem restf init this-restf specialf
		    (fctn1 (cond ((eq (car fctn) 'named-lambda) (cdr fctn))
				 ((eq (car fctn) 'named-subst) (cdr fctn))
				 (t fctn)))
		    (lambda-list (cadr fctn1))
		    (body (cddr fctn1))
		    (value-list a-value-list)
		    thisval			;Used by expansion of apply-lambda-bindvar
		    keynames keyinits keykeys keyflags
		    keynames1 keykeys1 keyflags1 (unspecified '(()))
		    allow-other-keys
		    thisvar)
	       (and (cdr body) (stringp (car body)) (pop body))	;doc string.

	       ;; Make a binding frame to represent any instance variables
	       (with-stack-list* (vars-env nil *interpreter-variable-environment*)
		 ;; If SELF is an instance, and instance vars aren't bound, bind them.
		 (when (typep self 'instance)
		   (unless (do ((tail (cdr vars-env) (cdr tail)))
			       ((atom tail) nil)
			     (when (setq tem (get-lexical-value-cell
					       (car tail)
					       (locf (symbol-value '.slots.bound.instance.))))
			       (return (eq (contents tem) self))))
		     ;;??? Here should take care of special instance variables!!!
		     ;; Probably just omit them, since they were bound when
		     ;; the message was sent, weren't they?
		     (tagbody
			 (setq tem (self-binding-instances))
		      loop
			 (when tem
			   (apply-lambda-bindvar-1 (car tem) (cadr tem) vars-env)
			   (setq tem (cddr tem))
			   (go loop)))
		     ;; now bind .slots.bound.instance. nonspecial
		     (with-stack-list (tem1 nil)
		       (if (null (car vars-env))
			   ;; start new frame
			   (setf (car vars-env) tem1)
			   ;; extend previous frame
			 (%p-dpb-offset cdr-next %%q-cdr-code thisval -1)))
		     (%push (locf (symbol-value '.slots.bound.instance.)))
		     (%push self)
		     (with-stack-list (tem1 nil)
		       (%p-dpb-offset cdr-nil %%q-cdr-code tem1 -1))))

		 ;; Make a bindframe to represent and SPECIAL or UNSPECIAL declarations
		 (with-stack-list* (vars-env nil vars-env)
		   ;; Find any declarations at the front of the function body
		   ;; and put them onto VARS-ENV ;;(and LOCAL-DECLARATIONS)
		   ;; Note that any declarations will override instance bindings made
		   (gobble-declarations-internal body vars-env)

		   ;; Now this bindframe is the one actually used to bind variables...
		   (with-stack-list* (*interpreter-variable-environment* nil vars-env)
		     (tagbody
		      l
			 (cond ((null value-list) (go lp1))
			       ((or (null lambda-list)
				    (eq (car lambda-list) '&aux)) 
				(cond (restf (go lp1)))
				(return-from top
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
				(go l1))		;Do next value.
			       ((eq (car lambda-list) '&quote)
				(setq quoteflag t)
				(go l1))
			       ((eq (car lambda-list) '&eval)
				(setq quoteflag nil)
				(go l1))
			       ((memq (car lambda-list) '(&special &local))
				(setq specialf (eq (car lambda-list) '&special))
				(go l1))
			       ((memq (car lambda-list) '(&rest &body))
				(setq this-restf t)
				(go l1))		;Do next value.
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
				       (setq value-list (copy-list value-list))))
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
			 (multiple-value-setq (nil nil lambda-list nil nil
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
			       (setq keyword
				     (cerror :new-keyword nil 'sys:undefined-keyword-argument
				       "Keyword arg keyword ~S, with value ~S, is unrecognized."
				       keyword (cadr value-list)))
			       (when (setq tem (find-position-in-list keyword keykeys))
				 (interpreter-set (nth tem keynames) (cadr x))
				 (and (setq tem (nth tem keyflags))
				      (interpreter-set tem t))
				 (return)))))
			 ;; Keyword args always use up all the values that are left...

			 ;; Here when all values used up.
		      lp1
			 (cond ((null lambda-list) (go ex1))
			       ((memq (car lambda-list) '(&rest &body))
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
				(return-from top
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
			 (return-from top (eval-body body))
		      bad-lambda-list
			 (setq fctn
			       (cerror :new-function nil 'sys:invalid-lambda-list
				       "~S has an invalid lambda list" fctn))
		      retry
			 (return-from top (apply fctn a-value-list))))))))
	    (macro
	     (ferror 'sys:funcall-macro
		     "Funcalling the macro ~S."
		     (function-name (cdr fctn)))
	     (return-from top
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
			"~S has an invalid lambda list" fctn))
     retry
	  (and (consp fctn) (go tail-recurse))
	  (return-from top (apply fctn a-value-list))
  
     too-few-args
	  (return-from top (signal-proceed-case
			     ((args)
			      (make-condition
				'sys:too-few-arguments
				"Function ~S called with only ~D argument~1@*~P."
				fctn (length a-value-list) a-value-list))
			     (:additional-arguments
			      (apply fctn (append a-value-list args)))
			     (:return-value args)
			     (:new-argument-list (apply fctn args))))
  
     too-many-args
	  (return-from top (signal-proceed-case
			     ((args)
			      (make-condition
				'sys:too-many-arguments
				"Function ~S called with too many arguments (~D)."
				fctn (length a-value-list) a-value-list))
			     (:fewer-arguments
			      (apply fctn (append a-value-list args)))
			     (:return-value args)
			     (:new-argument-list (apply fctn args)))))))

))
