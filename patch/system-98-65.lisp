;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.65
;;; Reason: *Many* *hairy* evaluator changes.
;;; COMPILER-LET does The Right Thing wrt lexical environment.
;;; Reason: *Many* *hairy* evaluator changes.
;;; Right lexical environments for EVAL-WHEN
;;; Written 15-Jun-84 13:54:02 by Mly,
;;; while running on Lisp Machine One from band 2
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.62, CADR 3.7, ZMail 53.17, MIT-Specific 22.1, microcode 309, ZM MIT.

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
	((STRINGP X) (%SXHASH-STRING X #o337))	;Ignores case!
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
				      ((OR (INTEGERP Y) (CHARACTERP Y))
				       (LDB 24. Y))
				      (T (SXHASH Y RANDOM-OBJECT-ACTION)))
				ROT)
			      HASH))))
	((FLONUMP X) (LOGXOR (%P-LDB-OFFSET #o0027 X 1)
			     (%P-LDB-OFFSET #o2701 X 1)
			     (%P-LDB #o0022 X)))
	((AND (TYPEP X 'INSTANCE)
	      (SEND X :SEND-IF-HANDLES :SXHASH RANDOM-OBJECT-ACTION)))
	((AND (TYPEP X 'NAMED-STRUCTURE)
	      (MEMQ :SXHASH (NAMED-STRUCTURE-INVOKE :WHICH-OPERATIONS X)))
	      (NAMED-STRUCTURE-INVOKE :SXHASH X RANDOM-OBJECT-ACTION))
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

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro enter-block (name-exp &body body)
  `(with-stack-list (tem ,name-exp nil)
     (with-stack-list (bindframe 'block tem)
       (with-stack-list* (interpreter-environment bindframe interpreter-environment)
	 (*catch (cdr tem)
	   (with-stack-list (tem1 nil)
	     (setf (cadr tem) (%make-pointer-offset dtp-locative tem1 -1)))
	   (progn . ,body))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun block (&quote name &rest body)
  "Make nonlocal exit point named NAME for use with RETURN-FROM within BODY.
BODY is evaluated, and the value(s) of the last form in it are returned,
except that if RETURN-FROM is used with our NAME as its argument
during the execution of BODY, control immediately exits from this BLOCK
with values specified by the arguments to RETURN-FROM.
If NAME is NIL, RETURN can also be used to exit this block."
  (check-type name symbol)
  (enter-block name
    (if (eq interpreter-function-environment t)
	(eval-body body)
      (gobble-declarations-from-body (decls-env body)
	(eval-body body)))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun return-from (&quote blockname &rest vals)
  "Return from a BLOCK named BLOCKNAME, or from a named PROG or DO.
The first arg (not evaluated) is the name.
If that is the only argument, zero values are returned.
With exactly one additional argument, its value(s) are returned.
With more arguments, each argument (except the first) produces
one value to be returned."
  (check-type blockname symbol)
  (let ((values (cond ((or (null vals) (cdr vals))
		       (mapcar 'eval1 vals))
		      (t (multiple-value-list (eval1 (car vals)))))))
    (do ((tail interpreter-environment (cdr tail)))
	((atom tail))
      (let ((bindframe (car tail)))
	(and (eq (car bindframe) 'block)
	     (eq blockname (car (cadr bindframe)))
	     (*throw (cdr (cadr bindframe))
		     (values-list values)))))
    (ferror nil "There is no lexically-visible active BLOCK named ~S." blockname)))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun return (&quote &rest vals)
  "Return from a BLOCK named NIL, or from the innermost PROG or DO.
Exactly the same as RETURN-FROM with NIL as first argument.
BLOCKs are candidates for RETURN only if named NIL,
but any PROG or DO is a candidate regardless of its name.
With exactly one argument, its value(s) are returned.
With zero or multiple arguments, each argument produces
one value to be returned."
  (let ((values (cond ((or (null vals) (cdr vals))
		       (mapcar 'eval1 vals))
		      (t (multiple-value-list (eval1 (car vals)))))))
    (do ((tail interpreter-environment (cdr tail)))
	((atom tail))
      (let ((bindframe (car tail)))
	(and (eq (car bindframe) 'block)
	     (null (car (cadr bindframe)))
	     (*throw (cdr (cadr bindframe))
		     (values-list values)))))
    (ferror nil "There is no lexically-visible active BLOCK named NIL.")))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun return-list (values)
  "Return the elements of VALUES from a BLOCK named NIL, or from the innermost PROG or DO.
BLOCKs are candidates for RETURN only if named NIL,
but any PROG or DO is a candidate regardless of its name.
Each element of VALUES becomes a single returned value.
It is preferable to write (RETURN (VALUES-LIST argument))."
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail))
    (let ((bindframe (car tail)))
      (and (eq (car bindframe) 'block)
	   (null (car (cadr bindframe)))
	   (*throw (cdr (cadr bindframe))
		   (values-list values)))))
  (ferror nil "There is no lexically-visible active BLOCK named NIL."))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun tagbody-internal (body)
  (with-stack-list (tem body nil)
    (with-stack-list (bindframe 'tagbody tem)
      (with-stack-list* (interpreter-environment bindframe interpreter-environment)
	(do ((pc body))
	    (())
	  (cond ((null pc) (return nil))
		((atom pc)
		 (ferror nil "Non-NIL atomic cdr in TAGBODY form ~S." body)
		 (return nil)))
	  (let ((exp (car pc)))
	    (setq pc (cdr pc))
	    (if (atom exp) nil
	      (catch-continuation (cdr tem)
		  #'(lambda (gotag-pointer) (setq pc (cdr gotag-pointer)))
		  nil
		(with-stack-list (tem1 nil)
		  (setf (cadr tem) (%make-pointer-offset dtp-locative tem1 -1)))
		(eval1 exp)))))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun go (&quote tag &aux tem)
  "Transfer control to the tag TAG in a lexically containing TAGBODY or PROG, etc.
May be used within TAGBODY, PROG, PROG*, DO, DO*, or anything expanding into them.
TAG is not evaluated.
Control transfers instantaneously; the remainder of this statement
of the TAGBODY or PROG is not completed.
See the documentation of TAGBODY for more info."
  (check-type tag symbol)
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail))
    (let ((bindframe (car tail)))
      (and (eq (car bindframe) 'tagbody)
	   (setq tem (memq tag (car (cadr bindframe))))
	   (*throw (cdr (cadr bindframe)) tem))))
  (ferror nil "Unseen GO tag ~S." tag))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun prog (&quote &rest prog-arguments)
  "Old-fashioned form that combines a LET, a BLOCK and a TAGBODY.
Usage is (PROG name varlist body...) or (PROG varlist body...).
A non-NIL symbol is interpreted as a NAME; NIL or a cons is a VARLIST.
These two forms of usage are equivalent to
  (BLOCK name
    (BLOCK NIL
      (LET varlist
        (TAGBODY body...))))
or, in the case with no specified NAME,
  (BLOCK NIL
    (LET varlist
      (TAGBODY body...)))
BLOCK establishes the RETURN-point, LET binds the variables,
and TAGBODY executes the body and handles GO tags.
See the documentation of BLOCK, LET and TAGBODY for more information.
PROG is semi-obsolete, but too ancient to be flushed."
  (declare (arglist /[progname/] varlist &body body))
  (let* ((progname (and (atom (car prog-arguments))
			(car prog-arguments)))
	 (varlist (if progname
		      (second prog-arguments)
		    (first prog-arguments)))
	 (progbody (if progname
		       (cddr prog-arguments)
		     (cdr prog-arguments))))
    (check-type progname symbol)
    (prog ()
	  (return
	    (if (eq interpreter-function-environment t)
		(enter-block (if (eq progname t) t nil)
		  (enter-block progname
		    (zl-parallel-binding-list (varlist)
		      (tagbody-internal progbody))))
	      (gobble-declarations-from-body (decls-env progbody)
		(parallel-binding-list (varlist decls-env)
		  (enter-block (if (eq progname t) t nil)
		    (enter-block progname
		      (tagbody-internal progbody)
		      (return nil))))))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun prog* (&quote &rest prog-arguments)
  "Old fashioned form that combines a LET*, a BLOCK and a TAGBODY.
PROG* is the same as PROG except that the variables are bound sequentially,
as in LET*, whereas PROG binds them in parallel, like LET."
  (declare (arglist /[progname/] varlist &body body))
  (let* ((progname (and (atom (car prog-arguments))
			(car prog-arguments)))
	 (varlist (if progname
		      (second prog-arguments)
		    (first prog-arguments)))
	 (progbody (if progname
		       (cddr prog-arguments)
		     (cdr prog-arguments))))
    (check-type progname symbol)
    (prog ()
	  (return
	    (if (eq interpreter-function-environment t)
		(enter-block (if (eq progname t) t nil)
		  (enter-block progname
		    (zl-serial-binding-list (varlist)
		      (tagbody-internal progbody))))
	      (gobble-declarations-from-body (decls-env progbody)
		(serial-binding-list (varlist decls-env)
		  (enter-block (if (eq progname t) t nil)
		    (enter-block progname
		      (tagbody-internal progbody)
		      (return nil))))))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do-named (&quote name &rest x)
  (enter-block name
    (do-internal x)))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do*-named (&quote name &rest x)
  (enter-block name
    (do*-internal x)))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do-body (oncep endtest retvals oldp varlist body &optional serial-stepping)
  (enter-block nil
    (do ()
	((and (not oncep) (eval1 endtest))
	 ;; Now evaluate the exit actions.
	 ;; The last one should return its values out of the DO.
	 (eval-body retvals))
      ;; Now execute the body.
      (tagbody-internal body)
      
      ;; Here after finishing the body to step the DO-variables.
      (and oncep (return nil))
      (cond (oldp (if (eq interpreter-function-environment t)
		      (set (car varlist) (eval (caddr varlist)))
		    (interpreter-set (car varlist) (eval1 (caddr varlist)))))
	    (serial-stepping
	     (dolist (elt varlist)
	       (and (consp elt) (cddr elt)
		    (if (eq interpreter-function-environment t)
			(set (car elt) (eval (caddr elt)))
		      (interpreter-set (car elt) (eval1 (caddr elt)))))))
	    (t (do ((vl varlist (cdr vl))
		    (vals (do ((vl varlist (cdr vl))
			       (vals nil (cons (and (consp (car vl)) (cdar vl) (cddar vl)
						    (eval1 (caddar vl)))
					       vals)))	;******* CONS *******
			      ((null vl) (nreverse vals)))
			  (cdr vals)))
		   ((null vl))
		 (cond ((and (consp (car vl)) (cdar vl) (cddar vl))
			(if (eq interpreter-function-environment t)
			    (set (caar vl) (car vals))
			  (interpreter-set (caar vl) (car vals)))))))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun unstackify (env &aux (newenv env))
  (when (consp env)
    (when (stack-list-p env)
      (setq newenv (cons (car env) (cdr env)))
      (%p-dpb dtp-one-q-forward %%q-data-type env)
      (%p-dpb newenv %%q-pointer env)
      (%p-dpb dtp-one-q-forward %%q-data-type (1+ (%pointer env)))
      (%p-dpb (1+ (%pointer newenv)) %%q-pointer (1+ (%pointer env)))
    (let* ((frame (car newenv))
	   (newframe frame))
      (when (stack-list-p frame)
	(setq newframe (make-list (length frame)))
	;; Copy each word of the old frame to the new, then
	;; forward each word of the old frame to the new.
	;; Uses %BLT-TYPED to copy in case what's there is a DTP-ONE-Q-FORWARD.
	(do ((l newframe (cdr l))
	     (m frame (cdr m)))
	    ((null l))
	  (%blt-typed m l 1 0)
	  (%p-store-pointer m l)
	  (%p-store-data-type m dtp-one-q-forward))
	(setf (car newenv) newframe)
	(when (and (memq (car newframe) '(block tagbody))
		   (stack-list-p (cadr newframe)))
	  (let ((newtem (copylist (cadr newframe))))
	    (setf (cadr newframe) newtem)
	    (setf (car (cadr newtem)) (cdr newtem))))))
    (when (cdr newenv)
      (let ((newrest (unstackify (cdr newenv))))
	(unless (eq (cdr newenv) newrest)
	  (setf (cdr newenv) newrest)))))
    newenv))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun setq (&quote &rest symbols-and-values)
  "Given alternating variables and value expressions, sets each variable to following value.
Each variable is set before the following variable's new value is evaluated.
See also PSETQ which computes all the new values and then sets all the variables."
  (prog (val)
     l	(cond ((null symbols-and-values) (return val))
	      ((null (cdr symbols-and-values))
	       (ferror nil "Odd number of arguments to SETQ"))
	      ;; checking for setqing defconstants would make life too hard for hacking
	      ((or (memq (car symbols-and-values) '(t nil))
		   (keywordp (car symbols-and-values)))
	       (ferror nil "Setting ~A is not allowed."
		       (if (keywordp (car symbols-and-values)) "keywords"
			 (car symbols-and-values)))))
	(if (eq interpreter-function-environment t)
	    (set (car symbols-and-values)
		 (setq val (eval1 (cadr symbols-and-values))))
	  (interpreter-set (car symbols-and-values)
			   (setq val (eval1 (cadr symbols-and-values)))))
	(setq symbols-and-values (cddr symbols-and-values))
	(go l)))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun *throw (tag &quote value-expression)
  "Throw the values of VALUE-EXPRESSION to TAG.
The innermost catch for TAG will return these values to its caller."
  (*throw tag (eval1 value-expression)))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do-internal (x &aux varlist endtest retvals oncep)
  (if (and (car x) (atom (car x)))		;"OLD STYLE"
;     (if (neq interpreter-function-environment t)
;         (ferror nil "Maclisp old-style DO not allowed in Common Lisp.")
      (progn
	(bind (value-cell-location (car x)) (eval1 (cadr x)))
	(do-body nil (cadddr x) nil
		 t x (cddddr x)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq interpreter-function-environment t)
	(zl-parallel-binding-list (varlist)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x)))
      (gobble-declarations-from-body (decls-env (cddr x))
	(parallel-binding-list (varlist decls-env)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x)))))))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do*-internal (x &aux varlist endtest retvals oncep)
  (if (and (car x) (atom (car x)))		;"OLD STYLE"
;     (if (neq interpreter-function-environment t)
;         (ferror nil "Maclisp old-style DO not allowed in Common Lisp.")
      (progn
	(bind (value-cell-location (car x)) (eval1 (cadr x)))
	(do-body nil (cadddr x) nil
		 t x (cddddr x)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq interpreter-function-environment t)
	(zl-serial-binding-list (varlist)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x) t))
      (gobble-declarations-from-body (decls-env (cddr x))
	(serial-binding-list (varlist decls-env)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x) t))))))

))


; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun interpreter-instance-vars-boundp (instance &aux tem)
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail) nil)
    (and (setq tem (get-lexical-value-cell (car tail)
					   (value-cell-location 'slots-bound-instance-1)))
	 (return (eq (contents tem) instance)))))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro apply-lambda-bindvar-1 (varloc valloc)
  `(progn
     (unless (car interpreter-environment)
       (with-stack-list (tem1 t)
	 (setf (car interpreter-environment) tem1)))
     (%push ,varloc)
     (%push ,valloc)
     ;; Modify cdr-code of last word pushed, to terminate the list.
     (with-stack-list (tem1 nil)
       (%p-dpb-offset cdr-nil %%q-cdr-code tem1 -1)
       (%p-dpb-offset cdr-next %%q-cdr-code tem1 -2)
       (%p-dpb-offset cdr-next %%q-cdr-code tem1 -3))))

))

;;; FUCK ME HARDERRRRRRRRR!!!!
;;; this doesn't work at all with the old all-specials evaluator, and the bugs this
;;; was supposed to fix shouldn't happen unless the the lexical evaluator is default.
;;; (interpreted methods...) Making this work again with the old interpreter seems
;;; painful beyond description to me just now (TOO MUCH TEA!)
;;; So, I'll just punt this lossage and hope people don't notice the subtle screw
;;; and instead win and use system 99. FOO.
;; From file EVAL.LISP PS:<L.SYS> OZ:
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

;(defun apply-lambda (fctn a-value-list)
;  (prog (tem)
;	(unless (consp fctn) (go bad-function))
;   tail-recurse
;	(case (car fctn)
;	  (curry-after
;	   (tagbody
;	       (setq tem (cddr fctn))
;	       (%open-call-block (cadr fctn) 0 4)
;	       (%assure-pdl-room (+ (length tem) (length a-value-list)))

;	    loop1
;	       (or a-value-list (go loop2))
;	       (%push (car a-value-list))
;	       (and (setq a-value-list (cdr a-value-list))
;		    (go loop1))

;	    loop2
;	       (or tem (go done))
;	       (%push (eval1 (car tem)))
;	       (and (setq tem (cdr tem))
;		    (go loop2))

;	    done
;	       (%activate-open-call-block)))
;	  (curry-before
;	   (tagbody
;	       (setq tem (cddr fctn))
;	       (%open-call-block (cadr fctn) 0 4)
;	       (%assure-pdl-room (+ (length tem) (length a-value-list)))

;	    loop1
;	       (or tem (go loop2))
;	       (%push (eval1 (car tem)))
;	       (and (setq tem (cdr tem))
;		    (go loop1))

;	    loop2
;	       (or a-value-list (go done))
;	       (%push (car a-value-list))
;	       (and (setq a-value-list (cdr a-value-list))
;		    (go loop2))

;	    done
;	       (%activate-open-call-block)))
;	  ((lambda named-lambda subst cli:subst named-subst CLI:LAMBDA CLI:NAMED-LAMBDA CLI:NAMED-SUBST)
;	   (let-if (memq (car fctn) '(named-lambda named-subst CLI:NAMED-LAMBDA CLI:NAMED-SUBST))
;		   ((interpreter-environment nil)
;		    (interpreter-function-environment nil)
;		    (local-declarations nil))
;	     (let* (optionalf quoteflag tem restf init this-restf specialf
;		    (fctn1 (cond ((MeMq (car fctn) '(named-lambda CLI:NAMED-LAMBDA)) (cdr fctn))
;				 ((MeMq (car fctn) '(named-subst CLI:NAMED-SUBST)) (cdr fctn))
;				 (t fctn)))
;		    (lambda-list (cadr fctn1))
;		    (body (cddr fctn1))
;		    (value-list a-value-list)
;		    (local-declarations local-declarations)
;		    this-specialp thisval  ;Used by expansion of apply-lambda-bindvar
;		    keynames keyinits keykeys keyflags
;		    keynames1 keykeys1 keyflags1 (unspecified '(()))
;		    allow-other-keys)
;	       (and (cdr body) (stringp (car body)) (pop body))	;and doc string.
;	       ;; Make a binding frame to represent any SPECIAL declarations.
;	       (with-stack-list* (decls-env nil interpreter-environment)
;		 ;; Find any declarations at the front of the function body
;		 ;; and put them onto LOCAL-DECLARATIONS and into DECLS-ENV.
;		 (gobble-declarations-internal body decls-env)
;		 (with-stack-list* (interpreter-environment nil decls-env)
;		   (prog (thisvar)		; THISVAR is name of argument being processed.
;			 ;; If SELF is an instance, and instance vars aren't bound, bind them.
;			 (when (and (typep self 'instance)
;				    (not (interpreter-instance-vars-boundp self)))
;			   ;;??? Here should take care of special instance variables!!!
;			   ;; Probably just omit them, since they were bound when
;			   ;; the message was sent, weren't they?
;			   (tagbody
;			       (setq tem (self-binding-instances))
;			    loop
;			       (when tem
;				 (apply-lambda-bindvar-1 (car tem) (cadr tem))
;				 (setq tem (cddr tem))
;				 (go loop)))
;			   (apply-lambda-bindvar-1
;			     (value-cell-location 'slots-bound-instance-1)
;			     self)
;			 )
;		      l
;			 (cond ((null value-list) (go lp1))
;			       ((or (null lambda-list)
;				    (eq (car lambda-list) '&aux)) 
;				(cond (restf (go lp1)))
;				(return-from apply-lambda
;				  (signal-proceed-case
;				    ((args)
;				     (make-condition 'sys:too-many-arguments
;						     "Function ~S called with too many arguments (~D)."
;						     fctn (length a-value-list) a-value-list))
;				    (:fewer-arguments
;				     (apply fctn (append a-value-list args)))
;				    (:return-value args)
;				    (:new-argument-list (apply fctn args)))))
;			       ((eq (car lambda-list) '&key)
;				(go key))
;			       ((eq (car lambda-list) '&optional)
;				(setq optionalf t)
;				(go l1))	;Do next value.
;			       ((eq (car lambda-list) '&quote)
;				(setq quoteflag t)
;				(go l1))
;			       ((eq (car lambda-list) '&eval)
;				(setq quoteflag nil)
;				(go l1))
;			       ((memq (car lambda-list) '(&special &local))
;				(setq specialf (eq (car lambda-list) '&special))
;				(go l1))
;			       ((eq (car lambda-list) '&rest)
;				(setq this-restf t)
;				(go l1))	;Do next value.
;			       ((memq (car lambda-list) lambda-list-keywords)
;				(go l1))
;			       ((atom (car lambda-list))
;				(setq thisvar (car lambda-list)))
;			       ((atom (caar lambda-list))
;				(setq thisvar (caar lambda-list))
;				;; If it's &OPTIONAL (FOO NIL FOOP),
;				;; bind FOOP to T since FOO was specified.
;				(when (and optionalf (cddar lambda-list))
;				  (and (null (caddar lambda-list)) (go bad-lambda-list))
;				  (apply-lambda-bindvar (caddar lambda-list)
;							t decls-env specialf)))
;			       (t (go bad-lambda-list)))
;			 ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
;			 ;;  It is in THISVAR.
;			 (and (null thisvar) (go bad-lambda-list))
;			 (cond (restf
;				;; Something follows a &REST arg???
;				(go bad-lambda-list))
;			       (this-restf	;This IS the &REST arg.
;				;; If quoted arg, and the list of values is in a pdl, copy it.
;				(and quoteflag
;				     (ldb-test %%pht2-map-access-code
;					       (area-region-bits (%area-number value-list)))
;				     (let ((default-cons-area background-cons-area))
;				       (setq value-list (copylist value-list))))
;				(apply-lambda-bindvar thisvar value-list decls-env specialf)
;				;; We don't clear out VALUE-LIST
;				;; in case keyword args follow.
;				(setq this-restf nil restf t)
;				(go l1)))

;			 (apply-lambda-bindvar thisvar (car value-list) decls-env specialf)
;			 (pop value-list)
;		      l1 (pop lambda-list)
;			 (go l)

;		      key
;			 (setf (values nil nil lambda-list nil nil
;				       keykeys keynames nil keyinits keyflags
;				       allow-other-keys)
;			       (decode-keyword-arglist lambda-list))
;			 ;; Process the special keyword :ALLOW-OTHER-KEYS if present as arg.
;			 (if (get (locf value-list) :allow-other-keys)
;			     (setq allow-other-keys t))

;			 (setq keykeys1 keykeys	;life is tough without LET...
;			       keynames1 keynames
;			       keyflags1 keyflags)
;		      key1
;			 (when keykeys1
;			   (setq tem (getf value-list (pop keykeys1) unspecified))
;			   (setq init (if (eq tem unspecified) (eval1 (car keyinits)) tem))
;			   (apply-lambda-bindvar (car keynames1) init decls-env)
;			   (if (car keyflags1)
;			       (apply-lambda-bindvar (car keyflags1)
;						     (neq tem unspecified)
;						     decls-env))
;			   (pop keynames1)
;			   (pop keyflags1)
;			   (pop keyinits)
;			   (go key1))
;			 (do ((x value-list (cddr x))
;			      keyword)
;			     ((null x))
;			   (unless (cdr x)
;			     (ferror 'sys:bad-keyword-arglist
;				     "No argument after keyword ~S"
;				     (car x)))
;			   (setq keyword (car x))
;			   (setq tem (find-position-in-list keyword keykeys))
;			   (unless (or tem allow-other-keys)
;			     (do-forever
;			       (setq keyword (cerror :new-keyword nil
;						     'sys:undefined-keyword-argument
;						     "Keyword arg keyword ~S, with value ~S, is unrecognized."
;						     keyword
;						     (cadr value-list)))
;			       (when (and keyword
;					  (setq tem (find-position-in-list keyword keykeys)))
;				 (interpreter-set (nth tem keynames) (cadr x))
;				 (and (setq tem (nth tem keyflags))
;				      (interpreter-set tem t))
;				 (return)))))
;			 ;; Keyword args always use up all the values that are left...

;			 ;; Here when all values used up.
;		      lp1
;			 (cond ((null lambda-list) (go ex1))
;			       ((eq (car lambda-list) '&rest)
;				(and restf (go bad-lambda-list))
;				(setq this-restf t)
;				(go lp2))
;			       ((eq (car lambda-list) '&key)
;				(go key))
;			       ((memq (car lambda-list) '(&optional &aux))
;				(setq optionalf t)	;Suppress too few args error
;				(go lp2))
;			       ((memq (car lambda-list) '(&special &local))
;				(setq specialf (eq (car lambda-list) '&special))
;				(go lp2))
;			       ((memq (car lambda-list) lambda-list-keywords)
;				(go lp2))
;			       ((and (null optionalf) (null this-restf))
;				(and restf (go bad-lambda-list))
;				(return-from apply-lambda
;				  (signal-proceed-case
;				    ((args)
;				     (make-condition 'sys:too-few-arguments
;						     "Function ~S called with only ~D argument~1@*~P."
;						     fctn (length a-value-list) a-value-list))
;				    (:additional-arguments
;				     (apply fctn (append a-value-list args)))
;				    (:return-value args)
;				    (:new-argument-list (apply fctn args)))))
;			       ((atom (car lambda-list)) (setq tem (car lambda-list))
;							 (setq init nil))
;			       ((atom (caar lambda-list))
;				(setq tem (caar lambda-list))
;				(setq init (eval1 (cadar lambda-list)))
;				;; For (FOO NIL FOOP), bind FOOP to NIL since FOO missing.
;				(when (cddar lambda-list)
;				  (and (null (caddar lambda-list)) (go bad-lambda-list))
;				  (apply-lambda-bindvar (caddar lambda-list)
;							nil decls-env specialf)))
;			       (t (go bad-lambda-list)))
;		      lp3
;			 (and (null tem) (go bad-lambda-list))
;			 (apply-lambda-bindvar tem init decls-env specialf)
;			 (and this-restf (setq restf t))
;			 (setq this-restf nil)
;		      lp2
;			 (setq lambda-list (cdr lambda-list))
;			 (go lp1)

;		      ex1
;			 ;; Here to evaluate the body.
;			 (return-from apply-lambda (eval-body body))
;		      bad-lambda-list
;			 (setq fctn
;			       (cerror :new-function nil 'sys:invalid-lambda-list
;				       "~S has an invalid LAMBDA list" fctn))
;		      retry
;			 (return-from apply-lambda (apply fctn a-value-list))))))))
;	  (macro
;	   (ferror 'sys:funcall-macro
;		   "Funcalling the macro ~S."
;		   (function-name (cdr fctn)))
;	   (return-from apply-lambda
;	     (eval1 (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list))))))

;	;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
;	(when (lambda-macro-call-p fctn)
;	  (setq fctn (lambda-macro-expand fctn))
;	  (go retry))

;   bad-function
;	;; Can drop through to here for a totally unrecognized function.
;	(setq fctn
;	      (cerror :new-function nil 'sys:invalid-function
;		      "~S is an invalid function." fctn))
;	(go retry)

;	;; Errors jump out of the inner PROG to unbind any lambda-vars bound with %BIND.
;   bad-lambda-list
;	(setq fctn
;	      (cerror :new-function nil 'sys:invalid-lambda-list
;		      "~S has an invalid LAMBDA list" fctn))
;   retry
;	(and (consp fctn) (go tail-recurse))
;	(return (apply fctn a-value-list))

;   too-few-args
;	(return (signal-proceed-case
;		  ((args)
;		   (make-condition 'sys:too-few-arguments
;				   "Function ~S called with only ~D argument~1@*~P."
;				   fctn (length a-value-list) a-value-list))
;		  (:additional-arguments
;		   (apply fctn (append a-value-list args)))
;		  (:return-value args)
;		  (:new-argument-list (apply fctn args))))

;   too-many-args
;	(return (signal-proceed-case
;		  ((args)
;		   (make-condition 'sys:too-many-arguments
;				   "Function ~S called with too many arguments (~D)."
;				   fctn (length a-value-list) a-value-list))
;		  (:fewer-arguments
;		   (apply fctn (append a-value-list args)))
;		  (:return-value args)
;		  (:new-argument-list (apply fctn args))))))

;))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN COMPILER-LET (&QUOTE BINDLIST &REST BODY)
  "Perform bindings in BINDLIST at evaluation or compilation time.
In interpreted code, this is the same as LET.
When found in code being compiled, the bindings are done at compile time,
and are not done when the compiled code is run."
    (EVAL1 `(LET ,BINDLIST
	      (DECLARE (SPECIAL . ,(MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) BINDLIST)))
	      . ,BODY)))

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN &OPTIONAL COMPILE-TIME-TOO (TOP-LEVEL-P T))
  (PROG TOP (FN (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	(())
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM TOP NIL))
      (IF (ATOM FORM) (RETURN NIL))
      ;; Don't expand LOCALLY into PROGN here!
      ;; This way, we protect DECLAREs inside the LOCALLY
      ;; from being treated as top-level DECLARE, which would be erroneous.
      ;; The LOCALLY form will just be executed as a random form.
      (IF (EQ (CAR FORM) 'LOCALLY)
	  (RETURN))
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
	    ((EQ (CAR OFORM) 'DEFSTRUCT)
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (AND (CONSP FORM)
	   (NEQ (CAR FORM) 'EVAL-WHEN)
	   COMPILE-TIME-TOO
	   (FUNCALL PROCESS-FN FORM 'DECLARE))
      (COND ((ATOM FORM)
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ (CAR FORM) 'EVAL-WHEN)
	     (OR (AND (CLI:LISTP (CADR FORM))
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE cli:eval))))
		 (FERROR NIL "~S invalid EVAL-WHEN times;
must be a list of EVAL, LOAD, and//or COMPILE."
			     (CADR FORM)))
	     (LET* ((COMPILE (MEMQ 'COMPILE (CADR FORM)))
		    (LOAD (MEMQ 'LOAD (CADR FORM)))
		    (EVAL (or (MEMQ 'EVAL (CADR FORM)) (memq 'cli:eval (cadr form))))	
		    (EVAL-NOW (OR COMPILE (AND COMPILE-TIME-TOO EVAL))))
	       (DOLIST (FORM1 (CDDR FORM))
		 (IF LOAD
		     (IF EVAL-NOW
			 (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN T NIL)
		       (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN NIL NIL))
		   (IF EVAL-NOW
		       (FUNCALL PROCESS-FN FORM1 'DECLARE))))))
	    ((EQ (SETQ FN (CAR FORM)) 'DEFF)
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
		  (MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
			     EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT DEFF-MACRO
			     REQUIRE)))
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
	     (EVAL `(LET ,(CADR FORM)
		      (DECLARE (SPECIAL . ,(MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
						   (CADR FORM))))
		      (COMPILE-DRIVER '(PROGN . ,(CDDR FORM))
				      ',PROCESS-FN ',OVERRIDE-FN
				      ,COMPILE-TIME-TOO
				      T))))
	    ((EQ FN 'DEFUN)
	     (LET (TEM)
	       (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed DEFUN")
		 (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
	       (COND ((EQ (CDR TEM) (CDR FORM))
		      (IF (NULL (CDDR TEM))
			  (WARN 'MALFORMED-DEFUN ':IMPOSSIBLE
				"Malformed defun ~S" FORM)
			(FUNCALL PROCESS-FN FORM 'DEFUN)))
		     (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL)))))
	    (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))

))
; From file QCFILE.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-STREAM (INPUT-STREAM GENERIC-PATHNAME FASD-FLAG PROCESS-FN
		       QC-FILE-LOAD-FLAG QC-FILE-IN-CORE-FLAG PACKAGE-SPEC
		       &OPTIONAL (FILE-LOCAL-DECLARATIONS NIL) IGNORE
		       COMPILING-WHOLE-FILE-P
		       &AUX (PACKAGE PACKAGE) (IBASE IBASE) (BASE BASE)
		       FILE-SPECIAL-LIST FILE-UNSPECIAL-LIST
		       FDEFINE-FILE-PATHNAME
		       (READ-FUNCTION (IF QC-FILE-CHECK-INDENTATION
					  'READ-CHECK-INDENTATION 'READ)))
  "This function does all the /"outer loop/" of the compiler, for file and editor compilation.
 to be compiled are read from INPUT-STREAM.
The caller is responsible for handling any file attributes.
GENERIC-PATHNAME is the file to record information for and use the attributes of.
 It may be NIL if compiling to core.
FASD-FLAG is NIL if not making a QFASL file.
PROCESS-FN is called on each form.
QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
FILE-LOCAL-DECLARATIONS is normally initialized to NIL,
but you can optionally pass in an initializations for it.
COMPILING-WHOLE-FILE-P should be T if you are processing all of the file."
  (FILE-OPERATION-WITH-WARNINGS (GENERIC-PATHNAME ':COMPILE COMPILING-WHOLE-FILE-P)
   (COMPILER-WARNINGS-CONTEXT-BIND
     ;; Override the package if required.  It has been bound in any case.
     (AND PACKAGE-SPEC (SETQ PACKAGE (PKG-FIND-PACKAGE PACKAGE-SPEC)))
     ;; Override the generic pathname
     (SETQ FDEFINE-FILE-PATHNAME
	   (LET ((PATHNAME (AND (MEMQ ':PATHNAME (SEND INPUT-STREAM ':WHICH-OPERATIONS))
				(SEND INPUT-STREAM ':PATHNAME))))
	     (AND PATHNAME (SEND PATHNAME ':GENERIC-PATHNAME))))
     ;; Having bound the variables, process the file.
     (LET ((QC-FILE-IN-PROGRESS T)
	   (UNDO-DECLARATIONS-FLAG (NOT QC-FILE-LOAD-FLAG))
	   (LOCAL-DECLARATIONS NIL)
	   (OPEN-CODE-MAP-SWITCH OPEN-CODE-MAP-SWITCH)
	   (RUN-IN-MACLISP-SWITCH RUN-IN-MACLISP-SWITCH)
	   (OBSOLETE-FUNCTION-WARNING-SWITCH OBSOLETE-FUNCTION-WARNING-SWITCH)
	   (ALL-SPECIAL-SWITCH ALL-SPECIAL-SWITCH)
	   (SOURCE-FILE-UNIQUE-ID)
	   (FASD-PACKAGE NIL))
       (COND (FASD-FLAG
	      ;; Copy all suitable file properties into the fasl file
	      ;; Suitable means those that are lambda-bound when you read in a file.
	      (LET ((PLIST (COPYLIST (SEND GENERIC-PATHNAME ':PLIST))))
		;; Remove unsuitable properties
		(DO ((L (LOCF PLIST)))
		    ((NULL (CDR L)))
		  (IF (NOT (NULL (GET (CADR L) 'FS:FILE-ATTRIBUTE-BINDINGS)))
		      (SETQ L (CDDR L))
		      (SETF (CDR L) (CDDDR L))))
		;; Make sure the package property is really the package compiled in
		;; Must load QFASL file into same package compiled in
		;; On the other hand, if we did not override it
		;; and the attribute list has a list for the package, write that list.
		(UNLESS (AND (NOT (ATOM (GETF PLIST ':PACKAGE)))
			     (STRING= (PACKAGE-NAME *PACKAGE*)
				      (CAR (GETF PLIST ':PACKAGE))))
		  (PUTPROP (LOCF PLIST)
			   (INTERN (PACKAGE-NAME *PACKAGE*) SI:PKG-KEYWORD-PACKAGE)
			   ':PACKAGE))
		(AND INPUT-STREAM
		     (MEMQ ':TRUENAME (SEND INPUT-STREAM ':WHICH-OPERATIONS))
		     (SETQ SOURCE-FILE-UNIQUE-ID (SEND INPUT-STREAM ':TRUENAME))
		     (PUTPROP (LOCF PLIST)
			      SOURCE-FILE-UNIQUE-ID
			      ':QFASL-SOURCE-FILE-UNIQUE-ID))
		;; If a file is being compiled across directories, remember where the
		;; source really came from.
		(AND FDEFINE-FILE-PATHNAME FASD-STREAM
		     (LET ((OUTFILE (AND (MEMQ ':PATHNAME
					       (SEND FASD-STREAM ':WHICH-OPERATIONS))
					 (SEND FASD-STREAM ':PATHNAME))))
		       (COND (OUTFILE
			      (SETQ OUTFILE (SEND OUTFILE ':GENERIC-PATHNAME))
			      (AND (NEQ OUTFILE FDEFINE-FILE-PATHNAME)
				   (PUTPROP (LOCF PLIST) FDEFINE-FILE-PATHNAME
					    ':SOURCE-FILE-GENERIC-PATHNAME))))))
		(MULTIPLE-VALUE-BIND (MAJOR MINOR)
		    (SI:GET-SYSTEM-VERSION "System")
		  (PUTPROP (LOCF PLIST)
			 `(,USER-ID
			   ,SI:LOCAL-PRETTY-HOST-NAME
			   ,(TIME:GET-UNIVERSAL-TIME)
			   ,MAJOR ,MINOR
			   (NEW-DESTINATIONS T    ;; NOT :new-destinations!!
			    :SITE ,(SHORT-SITE-NAME)))
			   ':COMPILE-DATA))
		;; First thing in QFASL file must be property list
		;; These properties wind up on the GENERIC-PATHNAME.
		(COND (QC-FILE-REL-FORMAT
		       (QFASL-REL#:DUMP-FILE-PROPERTY-LIST
			  GENERIC-PATHNAME
			  PLIST))
		      (T
		       (FASD-FILE-PROPERTY-LIST PLIST))))))
       (QC-PROCESS-INITIALIZE)
       (DO ((EOF (NCONS NIL))
	    (FORM))
	   (NIL)
	 ;; Detect EOF by peeking ahead, and also get an error now
	 ;; if the stream is wedged.  We really want to get an error
	 ;; in that case, not make a warning.
	 (LET ((CH (SEND INPUT-STREAM ':TYI)))
	   (OR CH (RETURN))
	   (SEND INPUT-STREAM ':UNTYI CH))
	 (setq si:premature-warnings
	       (append si:premature-warnings si:premature-warnings-this-object))
	 (let ((si:premature-warnings nil))
	   (SETQ FORM
		 (LET ((READ-AREA (IF QC-FILE-LOAD-FLAG DEFAULT-CONS-AREA
				    QCOMPILE-TEMPORARY-AREA))
		       (WARN-ON-ERRORS-STREAM INPUT-STREAM)
		       (QC-FILE-READ-IN-PROGRESS FASD-FLAG))	;looked at by XR-#,-MACRO
		   (WARN-ON-ERRORS ('READ-ERROR "Error in reading")
		     (FUNCALL READ-FUNCTION INPUT-STREAM EOF))))
	   (setq si:premature-warnings-this-object si:premature-warnings))
	 (AND (EQ FORM EOF) (RETURN))
	 ;; Start a new whack if FASD-TABLE is getting too big.
	 (AND FASD-FLAG
	      ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
	      (FASD-END-WHACK))
	 (WHEN (AND (ATOM FORM) FASD-FLAG)
	   (WARN 'ATOM-AT-TOP-LEVEL ':IMPLAUSIBLE
		 "The atom ~S appeared at top level; this would do nothing at FASLOAD time."
		 FORM))
	 (FUNCALL PROCESS-FN FORM))))))

))
