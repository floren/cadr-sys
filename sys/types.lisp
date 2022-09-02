;-*- Mode:LISP; Package:SI; Lowercase:T; Cold-Load:T; Base:10; Readtable:T -*-

;;; Each defined type keyword can have any of these three properties:
;;; TYPE-PREDICATE - value is a function to test an object for membership in the type.
;;;   It gets the object as first arg, and any elements of the type specifier
;;;   except for the keyword itself as additional args.
;;; TYPE-OPTIMIZER - value is an optimizer function for compiling calls to TYPEP.
;;;   Its first argument is the expression which calls TYPEP.
;;;   Its remaining args are the elements of the type specifier, except the first.
;;;   It can return the original call to TYPEP if it has nothing better to optimize to.
;;; TYPE-EXPANDER - value is an expander function to compute a new type specifier.
;;;   It gets one argument, the type specifier, and returns a new type specifier.

;;; Interpreted calls to TYPEP use TYPE-PREDICATE and TYPE-EXPANDER props.
;;; Compilation uses TYPE-OPTIMIZER and TYPE-EXPANDER props.
;;; Compilation can also use the TYPE-PREDICATE prop--
;;;  compiling a call to that function rather than to TYPEP,
;;;  but only if the property is a symbol.

;;; > CAUTION: you cannot simply define any new type with a TYPE-PREDICATE
;;; > because it needs to be wired into the SUBTYPEP data structures.
;;; > Defining types with TYPE-EXPANDERs (ie, use of DEFTYPE) is ok
;;; > because they will get expanded by SUBTYPEP, so they don't really
;;; > pose a new problem.

;;; These properties are also used:
;;; TYPE-NAME - value is a string, including a definite article, which
;;;  is used as the name of this type when it appears as an atom.
;;; TYPE-NAME-FUNCTION - value is a function to compute the name
;;;  of types which are lists starting with this symbol.

(defmacro deftype (name arglist &body body)
  "Defines NAME as a data type name for use in TYPEP, etc.
A list starting with NAME, used as a type specifier,
expands by binding the args in ARGLIST and then evaluating the BODY.
The value of BODY should be another type specifier.
Any optional arguments in ARGLIST which do not have default values specified
will be bound to * by default, rather than NIL."
  (let ((argcopy (copy-list arglist))
	optionalf doc)
;; is this a good idea?
;    (if (get name 'si:flavor)
;	(warn "~S is already the name of a flavor.
;/(DEFTYPE ~S ...) will cause (TYPEP foo '~S) not to recognize existing
;instances of that flavor in new code, and cause (TYPEP foo '~S) to fail for
;objects of the new type in old compiled code.
;You may lose!" name name name))
    (if (and (stringp (car body)) (cdr body)) (setq doc (pop body)))
    (do ((tail argcopy (cdr tail))) ((null tail))
      (cond ((eq (car tail) '&optional)
	     (setq optionalf t))
	    ((memq (car tail) '(&key &rest &aux))
	     (return))
	    ((and optionalf
		  (atom (car tail))
		  (not (memq (car tail) lambda-list-keywords)))
	     (setf (car tail)
		   `(,(car tail) '*)))))
    `(progn
       (eval-when (load eval)
	 (clear-cached-subtype-info ',name)
	 (defun (:property ,name type-expander) ,argcopy
	   . ,body)
	 (setf (documentation ',name 'type) ',doc))
       (eval-when (compile)
	 (let ((sym (make-symbol (symbol-name ',name))))
	   (setf (get sym 'type-expander) '(lambda ,argcopy . ,body))
	   (putdecl ',name sym 'type-alias-for)))
       ',name)))

(defun commonp (object)
  "T if OBJECT is a kind of object which Common Lisp defines.
This is everything except locatives, stack groups, selects,
closures, entities, compiled and microcode functions,
and flavor instances (except for a few flavors which implement Common Lisp types)."
  (typecase object
    (instance
     (or (pathnamep object) (streamp object) (hash-table-p object)))
    (compiled-function
     (streamp object))
    (t
     (not (memq (%data-type object)
		'(#.dtp-locative #.dtp-stack-group #.dtp-select-method
		  #.dtp-closure #.dtp-entity #.dtp-u-entry))))))

;;;; TYPE-OF

(defconst type-of-alist
	  '((#.dtp-symbol . symbol)
	    (#.dtp-character . cli:character)
	    (#.dtp-character . global:character)
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

(defun type-of (object &aux (dtp (%data-type object)))
  "Returns a type-specifier describing the type OBJECT belongs to.
For example, (TYPE-OF 5) is FIXNUM"
  (cond ((= dtp dtp-instance)
	 (%p-contents-offset
	   (instance-flavor object)
	   %instance-descriptor-typename))
	((= dtp dtp-array-pointer)
	 (cond ((named-structure-p object))
	       ((stringp object) 'string)
	       (t 'array)))
	((= dtp dtp-entity)
	 (class-symbol object))
	((= dtp dtp-extended-number) 
	 (case (%p-ldb-offset %%header-type-field object 0)
	   (#.%header-type-flonum 'single-float)
	   (#.%header-type-bignum 'bignum)
	   (#.%header-type-rational 'ratio)
	   (#.%header-type-complex 'complex)
	   (t t)))
	((cdr (assq dtp type-of-alist)))
	(t t)))

(defconst typep-one-arg-alist
	  '((#.dtp-symbol . :symbol)
	    (#.dtp-character . :character)
	    (#.dtp-list . :cons)
	    (#.dtp-fix . :fixnum)
	    (#.dtp-locative . :locative)
	    (#.dtp-fef-pointer . :compiled-function)
	    (#.dtp-closure . :closure)
	    (#.dtp-entity . :entity)
	    (#.dtp-instance . :instance)
	    (#.dtp-u-entry . :microcode-function)
	    (#.dtp-select-method . :select)
	    (#.dtp-small-flonum . :small-flonum)
	    (#.dtp-stack-group . :stack-group)))

(defun typep (object &optional (type nil type-specified-p))
  "T if OBJECT fits the data type specifier TYPE.
An obsolete mode of use is with one argument;
then the value is a type specifier describing OBJECT."
  (declare (arglist object type))
  (let (tem structure-desc dtp
	(type1 (if (consp type) (car type) type)))
    (cond ((not type-specified-p)
	   (setq dtp (%data-type object))
	   ;; Cannot use TYPE-OF, since we must
	   ;; for back-compatibility return keywords.
	   (cond ((= dtp dtp-instance)
		  (%p-contents-offset
		    (instance-flavor object)
		    %instance-descriptor-typename))
		 ((= dtp dtp-array-pointer)
		  (cond ((named-structure-p object))
			((stringp object) :string)
			(t :array)))
		 ((= dtp dtp-entity)
		  (class-symbol object))
		 ((= dtp dtp-extended-number) 
		  (select (%p-ldb-offset %%header-type-field object 0)
		    (%header-type-flonum :flonum)
		    (%header-type-bignum :bignum)
		    (%header-type-rational :rational)
		    (%header-type-complex :complex)
		    (otherwise :random)))
		 ((cdr (assq dtp typep-one-arg-alist)))
		 (t :random)))
	  ((setq tem (get type1 'type-predicate))
	   (if (atom type)
	       (funcall tem object)
	     (apply tem object (cdr type))))
	  ((setq dtp (or (rassq type type-of-alist) (rassq type typep-one-arg-alist)))
	   (= (%data-type object) (car dtp)))
	  ((setq tem (get type1 'type-expander))
	   (typep object (apply tem (if (atom type) nil (cdr type)))))
	  ((setq tem (get type1 'type-alias-for))
	   (typep object (if (atom type) tem `(,tem . ,(cdr type)))))
	  ((get type1 'flavor)
	   (typep-structure-or-flavor
	     object
	     (dont-optimize (flavor-name (get-flavor-tracing-aliases type1)))))
	  ((or (and (setq structure-desc (get type1 'defstruct-description))
		    (defstruct-description-named-p structure-desc))
	       (get type1 'defstruct-named-p))
	   (typep-structure-or-flavor object type1))
	  ((and (symbolp type1) (fboundp 'class-symbolp) (class-symbolp type1))
	   (and (entityp object)
		(subclass-of-class-symbol-p (class object) type1)))
	  (t (typep object (cerror t nil :wrong-type-arg
				   "~1@*~S is not a type known to TYPEP" 'typep type))))))

;;; As of system 98, this is used only by old compiled expansions of TYPEP.
(defun typep-structure (x type &aux xname d)
  (cond ((setq xname (named-structure-p x))
	 (do () ((eq xname type) t)
	   (or (and (setq d (get xname 'defstruct-description))
		    (defstruct-description-named-p d)
		    (setq xname (car (defstruct-description-include d))))
	       (return nil))))
	((and (setq d (get type 'defstruct-description))
	      (defstruct-description-named-p d))
	 nil)
	(t (typep x type))))			;Optimization turned out to be wrong

(defun (:property satisfies type-predicate) (object predicate)
  (funcall predicate object))

(defun (:property satisfies type-optimizer) (expression predicate)
  `(,predicate ,(cadr expression)))

(defun (:property or type-predicate) (object &rest types)
  (dolist (disjunct types)
    (when (typep object disjunct)
      (return t))))

(defun (:property or type-optimizer) (expression &rest types)
  (let ((object (cadr expression)))
    (once-only (object)
      `(or . ,(mapcar #'(lambda (type) `(typep ,object ',type))
		      types)))))

(defun (:property and type-predicate) (object &rest types)
  (dolist (conjunct types t)
    (unless (typep object conjunct)
      (return nil))))

(defun (:property and type-optimizer) (expression &rest types)
  (let ((object (cadr expression)))
    (once-only (object)
      `(and . ,(mapcar #'(lambda (type) `(typep ,object ',type))
		       types)))))

(defun (:property not type-predicate) (object type)
  (not (typep object type)))

(defun (:property not type-optimizer) (expression type)
  `(not (typep ,(cadr expression) ',type)))

(defun (:property cli:member type-predicate) (object &rest members)
  (not (not (member-eql object members))))

(defun (:property cli:member type-optimizer) (expression &rest members)
  `(member-eql ,(cadr expression) ',(copy-list members)))

(defun (:property cli:member type-expander) (&rest members)
  (let ((length (length members)))
    (cond ((= length 0) nil)
	  ;; are we gratuitous yet?
	  ((and (= length 1)
		(typep (car members) 'non-complex-number))
	   `(,(typecase (setq members (car members))
		(integer 'integer)
		(ratio 'rational)
		(short-float 'short-float)
		(single-float 'single-float))
	     ,members ,members))
	  ;; are we even more gratuitous?
	  ((loop for x on members always (and (integerp (car x))
					      (not (memq (car x) (cdr x))))
		 maximize (car x) into max
		 minimize (car x) into min
		 finally (if (= length (- max min -1))
			     (return `(integer ,min ,max))
			   (return nil))))
	  (t `(cli:member . ,(copy-list members))))))

(defprop global:member cli:member type-alias-for)

(defun (:property array type-predicate) (object &optional (element-type '*) (dimensions '*)
					 &aux array-element-type)
  (and (arrayp object)
       (or (eq dimensions '*)
	   (if (numberp dimensions)
	       (= dimensions (array-rank object))
	     (and (= (length dimensions) (array-rank object))
		  (dotimes (i (array-rank object) t)
		    (unless
		      (or (eq (nth i dimensions) '*)
			  (= (nth i dimensions) (array-dimension object i))
			  (return nil)))))))
       (or (eq element-type '*)
	   (equal element-type (setq array-element-type (array-element-type object)))
	   ;; this is because of the declarative/descriminative type specification dichotomy
	   ;;  which means that (subtypep '(array string-char) '(array t)) => nil
	   (and (subtypep element-type array-element-type)
		(subtypep array-element-type element-type)))))

(defun (:property array type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*) (* *)))
	 `(arrayp ,(cadr expression)))
	(t expression)))

(defun (:property simple-array type-predicate) (object &optional (element-type '*)
								 (dimensions '*)
						&aux array-element-type)
  (and (simple-array-p object)
       (or (eq element-type '*)
	   (equal element-type (setq array-element-type (array-element-type object)))
	   (and (subtypep element-type array-element-type)
		(subtypep array-element-type element-type)))
       (or (eq dimensions '*)
	   (if (numberp dimensions)
	       (= dimensions (array-rank object))
	     (and (= (length dimensions) (array-rank object))
		  (dotimes (i (array-rank object) t)
		    (unless
		      (or (eq (nth i dimensions) '*)
			  (= (nth i dimensions) (array-dimension object i))
			  (return nil)))))))))

(defun (:property simple-array type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*) (* *)))
	 `(simple-array-p ,(cadr expression)))
	(t expression)))

(defun (:property vector type-predicate) (object &optional (element-type '*) (size '*)
					  &aux array-element-type)
  (and (vectorp object)
       (or (eq element-type '*)
	   (equal element-type (setq array-element-type (array-element-type object)))
	   (and (subtypep element-type array-element-type)
		(subtypep array-element-type element-type)))
       (or (eq size '*)
	   (= size (array-length object)))))

(defun (:property vector type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*) (* *)))
	 `(vectorp ,(cadr expression)))
	(t expression)))

(defun (:property vector type-expander) (&optional (element-type '*) (size '*))
  `(array ,element-type (,size)))

(defun (:property simple-vector type-predicate) (object &optional (size '*))
  (and (simple-vector-p object)
       (or (eq size '*)
	   (= size (array-length object)))))

(defun (:property simple-vector type-optimizer) (expression &optional (size '*))
  (cond ((eq size '*)
	 `(simple-vector-p ,(cadr expression)))
	(t
	 (setq expression (cadr expression))
	 (once-only (expression)
	   `(and (simple-vector-p ,expression)
		 (= (array-total-size ,expression) ,size))))))

(defun (:property simple-vector type-expander) (&optional (size '*))
  `(simple-array t (,size)))

(defun (:property string type-predicate) (object &optional (size '*))
  (and (stringp object)
       (or (eq size '*)
	   (= size (array-length object)))))

(defun (:property string type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*)))
	 `(stringp ,(cadr expression)))
	(t
	 (setq expression (cadr expression))
	 (once-only (expression)
	   `(and (stringp ,expression)
		 (= (array-total-size ,expression) ,(car args)))))))

(defun (:property string type-expander) (&optional (size '*))
  `(array string-char (,size)))

(defun (:property simple-string type-predicate) (object &optional (size '*))
  (and (simple-string-p object)
       (or (eq size '*)
	   (= size (array-length object)))))

(defun (:property simple-string type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*)))
	 `(simple-string-p ,(cadr expression)))
	(t
	 (setq expression (cadr expression))
	 (once-only (expression)
	   `(and (simple-stringp ,expression)
		 (= (array-total-size ,expression) ,(car args)))))))

(defun (:property simple-string type-expander) (&optional (size '*))
  `(simple-array string-char (,size)))

(defun (:property bit-vector type-predicate) (object &optional (size '*))
  (and (bit-vector-p object)
       (or (eq size '*)
	   (= size (array-length object)))))

(defun (:property bit-vector type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*)))
	 `(bit-vector-p ,(cadr expression)))
	(t
	 (setq expression (cadr expression))
	 (once-only (expression)
	   `(and (bit-vector-p ,expression)
		 (= (array-total-size ,expression) ,(car args)))))))

(defun (:property bit-vector type-expander) (&optional (size '*))
  `(array bit (,size)))

(defun (:property simple-bit-vector type-predicate) (object &optional (size '*))
  (and (simple-bit-vector-p object)
       (or (eq size '*)
	   (= size (array-length object)))))

(defun (:property simple-bit-vector type-optimizer) (expression &rest args)
  (cond ((member-equal args '(() (*)))
	 `(simple-bit-vector-p ,(cadr expression)))
	(t
	 (setq expression (cadr expression))
	 (once-only (expression)
	   `(and (simple-bit-vector-p ,expression)
		 (= (array-total-size ,expression) ,(car args)))))))

(defun (:property simple-bit-vector type-expander) (&optional (size '*))
  `(simple-array bit (,size)))

(defun (:property named-structure type-predicate) (object)
  (not (null (named-structure-p object))))

(defun (:property named-structure type-optimizer) (expression)
  `(not (null (named-structure-p ,(cadr expression)))))

(defun (:property named-structure type-expander) ()
  'structure)

(defun (:property structure type-predicate) (object)
  (not (null (named-structure-p object))))

(defun (:property structure type-optimizer) (expression)
  `(not (null (named-structure-p ,(cadr expression)))))

(defun (:property function type-predicate) (object &rest ignore)
  object
  (ferror nil "FUNCTION types are not meaningful for testing objects against."))

(defun (:property values type-predicate) (object &rest ignore)
  object
  (ferror nil "VALUES types are not meaningful for testing objects against."))

(defun (:property sequence type-predicate) (object)
  (or (cli:listp object) (vectorp object)))

(defun (:property sequence type-expander) ()
  '(or list vector))

(defun (:property sequence type-optimizer) (expression)
  (let ((object (cadr expression)))
    (once-only (object)
      `(or (cli:listp ,object)
	   (vectorp ,object)))))

(defun (:property nil type-predicate) (object) object nil)

(defun (:property nil type-optimizer) (expression) `(progn ,(cadr expression) nil))

(defun (:property t type-predicate) (object) object t)

(defun (:property t type-optimizer) (expression) `(progn ,(cadr expression) t))

(defun (:property string-char type-predicate) (object)
  (and (characterp object) (string-char-p object)))

(defun (:property string-char type-expander) ()
  `(and cli:character (satisfies string-char-p)))

(defun (:property string-char type-optimizer) (expression)
  (let ((object (cadr expression)))
    (once-only (object)
      `(and (characterp ,object)
	    (string-char-p ,object)))))  

(defun (:property fat-char type-predicate) (object)
  (and (characterp object)
       ( object 0)
       (< object (lsh 1 #o20))))

(defun (:property fat-char type-optimizer) (expression)
  (setq expression (cadr expression))
  (once-only (expression)
    `(and ( ,expression 0)
	  (< ,expression (lsh 1 #o20)))))

(defun (:property standard-char type-predicate) (object)
  (and (characterp object) (standard-char-p object)))

(defun (:property standard-char type-optimizer) (expression)
  (let ((object (cadr expression)))
    (once-only (object)
      `(and (characterp ,object)
	    (standard-char-p ,object)))))

(defun (:property standard-char type-expander) ()
  `(and cli:character (satisfies standard-char-p)))

(defun (:property cli:character type-optimizer) (expression)
  `(characterp ,(cadr expression)))

;;;; Numeric types.

(defun (:property complex type-predicate) (object &optional (type '*))
  (and (complexp object)
       (or (memq type '(* non-complex-number))
	   (and (typep (%complex-real-part object) type)
		(or (memq type '(float single-float short-float double-float long-float))
		    (typep (%complex-imag-part object) type))))))

(defun (:property complex type-optimizer) (expression &optional (type '*))
  (let ((object (cadr expression)))
    (if (memq type '(* non-complex-number))
	`(complexp ,object)
      (once-only (object)
	`(and (complexp ,object)
	      (and (typep (%complex-real-part ,object) ',type)
		   ,(unless (memq type
				  '(float small-float single-float double-float long-float))
		      `(typep (%complex-real-part ,object) ',type))))))))

(defsubst non-complex-number-p (object)
  (and (numberp object) (not (complexp object))))

(defun non-complex-number-in-range-p (object low high type)
  (and (cond ((eq low '*) t)
	     ((numberp low) ( low object))
	     ((consp low) (< (car low) object))
	     (t (ferror nil "Invalid lower limit ~S in ~A type specifier." low type)))
       (cond ((eq high '*) t)
	     ((numberp high) ( high object))
	     ((consp high) (> (car high) object))
	     (t (ferror nil "Invalid upper limit ~S in ~A type specifier." high type)))))

(defun (:property real type-predicate) (object &optional (low '*) (high '*))
  (and (realp object)
       (setq object (realpart object))
       (non-complex-number-in-range-p object low high 'real)))

(defun (:property real type-optimizer) (expression &optional (low '*) (high '*))
  (if (and (eq low '*) (eq high '*))
      `(realp ,(cadr expression))
    (let ((object (cadr expression))
	  (o (gensym)))
      `(let ((,o ,object))
	 (block real
	   (and (setq ,o (typecase ,o
			   (complex (%complex-real-part ,o))
			   (number ,o)
			   (t (return-from real nil))))
		,(cond ((eq low '*)
			t)
		       ((numberp low)
			`( ,o ,low))
		       ((consp low)
			`(> ,o ,(car low))))
		,(cond ((eq high '*)
			t)
		       ((numberp high)
			`( ,o ,high))
		       ((consp high)
			`(< ,o ,(car high))))))))))

(defun (:property non-complex-number type-predicate) (object &optional (low '*) (high '*))
  (and (numberp object)
       (not (complexp object))
       (non-complex-number-in-range-p object low high 'non-complex-number)))

(defun (:property non-complex-number type-optimizer) (expression &optional (low '*) (high '*))
  (optimize-numeric-type-test 'non-complex-number-p expression low high))

(defun (:property integer type-predicate) (object &optional (low '*) (high '*))
  (and (integerp object)
       (non-complex-number-in-range-p object low high 'integer)))

(defun (:property integer type-optimizer) (expression &optional (low '*) (high '*))
  (if (and (neq low '*)
	   (neq high '*)
	   (< (- (if (consp high) (1- (car high)) high)
		 (if (consp low) (1+ (car low)) low))
	      4))
      (let ((object (cadr expression)))
	`(memq ,object
	       ',(loop for i from (if (consp low) (1+ (car low)) low)
		       upto (if (consp high) (1- (car high)) high)
		       collect i)))
    (optimize-numeric-type-test 'integerp expression low high)))

(defprop fix integer alias-type)

(defun (:property bignum type-expander) ()
  `(or (integer * (,most-negative-fixnum)) (integer (,most-positive-fixnum) *)))

(defun (:property fixnum type-predicate) (object &optional (low '*) (high '*))
  (and (fixnump object)
       (non-complex-number-in-range-p object low high 'fixnum)))

(defun (:property fixnum type-expander) (&optional (low '*) (high '*))
  `(integer ,(if (eq low '*) most-negative-fixnum low)
	    ,(if (eq high '*) most-positive-fixnum high)))

(defun (:property fixnum type-optimizer) (expression &optional (low '*) (high '*))
  (if (and (neq low '*)
	   (neq high '*)
	   (< (- (if (consp high) (1- (car high)) high)
		 (if (consp low) (1+ (car low)) low))
	      4))
      (let ((object (cadr expression)))
	`(memq ,object
	       ',(loop for i from (if (consp low) (1+ (car low)) low)
		       upto (if (consp high) (1- (car high)) high)
		       collect i)))
    (optimize-numeric-type-test 'fixnump expression low high)))

(defun optimize-numeric-type-test (predicate expression low high)
  (let ((object (cadr expression)))
    (once-only (object)
      `(and (,predicate ,object)
	    ,(cond ((eq low '*)
		    t)
		   ((numberp low)
		    `( ,object ,low))
		   ((consp low)
		    `(> ,object ,(car low))))
	    ,(cond ((eq high '*)
		    t)
		   ((numberp high)
		    `( ,object ,high))
		   ((consp high)
		    `(< ,object ,(car high))))))))

(defun (:property mod type-predicate) (object &optional (limit '*))
  (and (integerp object)
       (not (minusp object))
       (cond ((eq limit '*) t)
	     ((numberp limit) (> limit object))
	     (t (ferror nil "Invalid upper limit ~S in MOD type specifier." limit)))))

(defun (:property mod type-expander) (&optional (high '*))
  (if (eq high '*)
      `(integer 0)
    `(integer 0 ,(1- high))))

(defun (:property bit type-predicate) (object)
  (memq object '(0 1)))

(defun (:property bit type-expander) ()
  '(integer 0 1))

(defun (:property unsigned-byte type-predicate) (object &optional byte-size)
  (and (integerp object)
       (not (minusp object))
       (cond ((memq byte-size '(nil *)) t)
	     ((numberp byte-size) (> (ash 1 byte-size) object))
	     (t (ferror nil "Invalid byte size in UNSIGNED-BYTE type specifier.")))))

(defun (:property unsigned-byte type-expander) (&optional (byte-size '*))
  (if (eq byte-size '*)
      '(integer 0)
    `(integer 0 ,(1- (ash 1 byte-size)))))

(defun (:property signed-byte type-predicate) (object &optional (byte-size '*))
  (and (integerp object)
       (cond ((eq byte-size '*) t)
	     ((numberp byte-size)
	      (and (< object (ash 1 (1- byte-size)))
		   ( object (- (ash 1 (1- byte-size))))))
	     (t (ferror nil "Invalid byte size in SIGNED-BYTE type specifier.")))))

(defun (:property signed-byte type-expander) (&optional (byte-size '*))
  (if (eq byte-size '*)
      `integer
    `(integer ,(- (ash 1 (1- byte-size))) ,(1- (ash 1 (1- byte-size))))))

(defun (:property rational type-predicate) (object &optional (low '*) (high '*))
  (and (rationalp object)
       (non-complex-number-in-range-p object low high 'rational)))

(defun (:property rational type-optimizer) (expression &optional (low '*) (high '*))
  (optimize-numeric-type-test 'rationalp expression low high))

(defun (:property float type-predicate) (object &optional (low '*) (high '*))
  (and (floatp object)
       (non-complex-number-in-range-p object low high 'float)))

(defun (:property float type-optimizer) (expression &optional (low '*) (high '*))
  (optimize-numeric-type-test 'floatp expression low high))

(defun (:property short-float type-predicate) (object &optional (low '*) (high '*))
  (and (small-floatp object)
       (non-complex-number-in-range-p object low high 'short-float)))
(defun (:property short-float type-optimizer) (expression &optional (low '*) (high '*))
  (optimize-numeric-type-test 'small-floatp expression low high))

(defprop small-flonum short-float type-alias-for)
(defprop small-float short-float type-alias-for)

(defun (:property single-float type-predicate) (object &optional (low '*) (high '*))
  (and (floatp object)
       (not (small-floatp object))
       (non-complex-number-in-range-p object low high 'single-float)))

(defun (:property single-float type-optimizer) (expression &optional (low '*) (high '*))
  (optimize-numeric-type-test 'flonump expression low high))

(defprop flonum single-float type-alias-for)
(defprop double-float single-float type-alias-for)
(defprop long-float single-float type-alias-for)

;;;; Data base for inclusion relation on basic types.

(defprop number (rational integer fixnum bignum ratio complex real non-complex-number
		 float short-float single-float)
	 subtypes)
(defprop real (rational integer fixnum ratio bignum
	       float short-float single-float non-complex-number)
	 subtypes)
(defprop non-complex-number (rational integer fixnum ratio bignum
			     float short-float single-float)
	 subtypes)
(defprop rational (integer ratio bignum fixnum)
	 subtypes)
(defprop integer (bignum fixnum)
	 subtypes)
(defprop float (short-float single-float)
	 subtypes)

(defprop sequence (list cons null vector bit-vector string
		   simple-vector simple-bit-vector simple-string)
	 subtypes)

(defprop symbol (null keyword)
	 subtypes)

(defprop list (cons null)
	 subtypes)

(defprop cli:character (standard-char string-char fat-char)
	 subtypes)
(defprop global:character cli:character
	 type-alias-for)
(defprop fat-char (string-char standard-char)
	 subtypes)
(defprop string-char (standard-char)
	 subtypes)

(defprop array (structure simple-array vector string bit-vector
		simple-vector simple-bit-vector simple-string)
	 subtypes)
(defprop simple-array (simple-vector simple-bit-vector simple-string)
	 subtypes)
(defprop vector (string bit-vector simple-vector simple-bit-vector simple-string)
	 subtypes)
(defprop string (simple-string)
	 subtypes)
(defprop bit-vector (simple-bit-vector)
	 subtypes)

(defprop atom (array simple-array vector string bit-vector
	       simple-vector simple-bit-vector simple-string
	       standard-char
	       symbol null
	       number rational integer bignum fixnum ratio complex real non-complex-number
	       float short-float single-float
	       hash-table readtable package pathname stream random-state
	       structure
	       closure entity instance stack-group select locative
	       compiled-function microcode-function)
	 subtypes)

(defprop common (array simple-array vector string bit-vector
		 simple-vector simple-bit-vector simple-string
		 standard-char
		 list symbol cons null
		 number rational integer bignum fixnum ratio complex real non-complex-number
		 float short-float single-float
		 hash-table readtable package pathname stream random-state
		 structure)
	 subtypes)

(defprop atom atom type-predicate)
(defprop bignum bigp type-predicate)
(defprop common commonp type-predicate)
(defprop cons consp type-predicate)
(defprop keyword keywordp type-predicate)
(defprop list cli:listp type-predicate)
(defprop null null type-predicate)
(defprop number numberp type-predicate)
(defprop ratio ratiop type-predicate)
(defprop stream streamp type-predicate)
(defprop symbol symbolp type-predicate)

(defprop select-method select type-alias-for)

;;; Pretty names for types.  This is used by the CHECK-TYPE macro.
(defprop select "a select-method" type-name)
(defprop fix "an integer" type-name)
(defprop float "a floating-point number" type-name)
(defprop real "a real number" type-name)
(defprop null "NIL" type-name)
(defprop complex "a complex number" type-name)
(defprop non-complex-number "a non-complex number" type-name)

(defun fixup-type-properties ()
  (dolist (symbol
	    '(array atom bignum bit bit-vector cli:character closure common
	      compiled-function complex cons double-float entity
	      fat-char fix fixnum flonum float hash-table instance integer keyword ;list
	      locative long-float microcode-function named-structure null number
	      package pathname random-state ratio rational readtable real
	      select select-method sequence short-float simple-array simple-bit-vector
	      simple-string simple-vector single-float small-flonum
	      standard-char stream string string-char structure symbol
	      vector))
    (putprop (intern (symbol-name symbol) pkg-keyword-package) symbol 'type-alias-for)
    (putprop :list 'cons 'type-alias-for)))

(add-initialization 'fixup-type-properties '(fixup-type-properties) '(once))

;;;; Hairy slege-hammeroid TYPE-CANONICALIZE

(defun type-canonicalize (typespec &optional record-dependencies dependencies
			  &aux canon tem)
  "Returns a typespec equivalent in meaning to TYPESPEC, but possibly simpler."
  (declare (values canonicalized-type dependencies))
  (when (eq typespec '*)
    (cerror "Proceed, returning *" "~Pinhead alert!!
Somebody tried to type-hack /"*/" Please report this bug~")
    (return-from type-canonicalize (values '* dependencies)))
  (macrolet ((record-dependency (x)
	       `(if record-dependencies
		    (if (cli:listp ,x)
			(dolist (foo ,x) (pushnew foo dependencies :test 'eq))
		      (pushnew ,x dependencies :test 'eq))))
	     (type-canonicalize-1 (x)
	       `(multiple-value-setq (nil dependencies)
		    (type-canonicalize ,x record-dependencies dependencies))))
    (flet ((find-tail-of-same-type (y list &aux x)
	     (setq x (if (consp y) (car y) y))
	     (unless (memq x '(and or not cli:member global:member satisfies))
	       (do ((z list (cdr z)))
		   ((null z))
		 (when (or (eq (car z) x) (eq (caar-safe z) x))
		   (return z))))))
      (setq canon
	    (block canon
	      (cond ((null typespec) nil)
		    ((eq typespec t) t)
		    ((symbolp typespec)
		     (cond ((setq tem (get typespec 'type-alias-for))
			    (record-dependency typespec)
			    (type-canonicalize-1 tem))
			   ((setq tem (get typespec 'type-expander))
			    (record-dependency typespec)
			    (type-canonicalize-1 (funcall tem)))
			   (t typespec)))
		    ((and (consp typespec) (symbolp (car typespec)))
		     (when (setq tem (get (car typespec) 'type-alias-for))
		       (record-dependency tem)
		       (setq typespec `(,tem . ,(cdr typespec))))
		     (case (car typespec)
		       (or (do ((tail (cdr typespec) (cdr tail))
				elt (frobs nil))
			       ((null tail)
				(cond ((cdr frobs)
				       `(or . ,(nreverse frobs)))
				      ;; (or foo) => foo
				      (frobs (car frobs))
				      ;; (or) => t
				      (t t)))
			     (setq elt (type-canonicalize-1 (car tail)))
			     (case (if (consp elt) (car elt) elt)
			       (or (setq tail (append elt (cdr tail))))
			       ((t) (setq dependencies nil)
				    (return-from canon t))
			       ((nil))		;splice out NIL's
			       (t
				(if (setq tem (find-tail-of-same-type elt frobs))
				    (cond ((atom (car tem)))
					  ;; (or (foo bar baz) foo) => foo
					  ((atom elt) (setf (car tem) elt))
					  (t (push elt frobs)))
				  (push elt frobs))))))
		       (and (do ((tail (cdr typespec) (cdr tail))
				 elt (frobs nil))
				((null tail)
				 (cond ((cdr frobs)
					`(and . ,(nreverse frobs)))
				       (t (car frobs))))
			     (setq elt (type-canonicalize-1 (car tail)))
			     (case (if (consp elt) (car elt) elt)
			       (and (setq tail (append elt (cdr tail))))
			       ((nil) (setq dependencies nil)
				      (return-from canon nil))
			       ((t))
			       (t
				(if (setq tem (find-tail-of-same-type elt frobs))
				    (cond ((atom (car tem)) (setf (car tem) elt))
					  ((atom elt))
					  (t (push elt frobs)))
				  (push elt frobs))))))
		       (not (let ((z (type-canonicalize-1 (cadr typespec))))
			      (if (eq (car-safe z) 'not)
				  (cadr z)
				`(not ,z))))
		       (t
			(cond ((dolist (elt (cdr typespec) t)
				 (unless (eq elt '*)
				   (return nil)))
			       ;; (foo * * *) => foo
			       (type-canonicalize-1 (car typespec)))
			      ((setq tem (get (car typespec) 'type-expander))
			       (record-dependency (car typespec))
			       (apply tem (cdr typespec)))
			      (t (if (memq (car typespec) '(array simple-array complex))
				     (let ((subtype (cadr typespec)))
				       (if (memq subtype '(nil t *))
					   typespec
					 (multiple-value-setq (tem dependencies)
					   (type-canonicalize subtype
							      record-dependencies
							      dependencies))
					 (if (equal tem subtype)
					     typespec
					   `(,(car typespec) ,tem . ,(cddr typespec)))))
				   typespec))))))
		    (t (multiple-value-setq (canon dependencies)
			 (type-canonicalize
			   (cerror t nil 'wrong-type-argument "~*~S invalid typespec."
				   'typespec typespec)
			   record-dependencies))))))
      (values canon dependencies))))
		       

;;;; SUBTYPEP (errrgggggggghhhhhhhhhhhh!!!!)

(defstruct (subtypep-hash-table-element (:type :list*) (:alterant nil) :conc-name
					(:callable-constructors nil))
  (subtypep nil :documentation "First value for (SUBTYPEP (CAR key) (CDR key))")
  (knownp nil :documentation "Second value for (SUBTYPEP (CAR key) (CDR key))")
  (dependencies nil :documentation "What types we expanded in making our decision."))

(defvar *subtypep-hash-table* :unbound
  "An EQUAL hash table containing cached values of (subtypep foo bar), keyed by (cons foo bar)")

(defun subtypep (x y)
  "T if any object of type X must be of type Y.
The second value is T if the first value is accurate:
if the second value is T and the first is NIL,
then there are objects of type X that are not of type Y.
If the second value is NIL, it is not known whether X is really a subtype of Y."
  (declare (values known-to-be-subtype known-whether-is-subtype))
  (unless (variable-boundp *subtypep-hash-table*)
    (let ((default-cons-area background-cons-area))
      (setq *subtypep-hash-table* (make-hash-table :test #'equal :size 400.))))
  (multiple-value-bind (known-to-be-subtype known-whether-is-subtype)
      (subtypep-1 x y nil)
    (values known-to-be-subtype known-whether-is-subtype)))

(defmacro subtypep-2 (x y)
  (declare (values known-to-be-subtype known-whether-is-subtype))
  `(let (a)
     (multiple-value (a nil dependencies)
       (subtypep-1 ,x ,y dependencies))
     a))

(defmacro subtypep-3 ((known-to-be-var known-whether-var) x y &body body)
  `(multiple-value-bind (,known-to-be-var ,known-whether-var .deps.)
       (subtypep-1 ,x ,y dependencies)
     (setq dependencies .deps.)
     . ,body))

(defun subtypep-1 (x y dependencies
		   &aux (known-to-be-subtype nil) (known-whether-is-subtype nil) tem elt)
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (when (or (eq y '*)
	    (eq x '*))
    (cerror "Proceed, returning NIL NIL" "~Pinhead alert!!
Somebody tried to type-hack /"*/" Please report this bug~")
    (return-from subtypep-1 (values nil nil)))

  (cond ((and (symbolp y) (setq tem (get y 'subtypes)))
	 (if (memq (if (atom x) x (car x)) tem)
	     (return-from subtypep-1 (values t t dependencies))))
	((memq y '(t nil)) (return-from subtypep-1 (values y t dependencies)))
	((setq tem (gethash (cons x y) *subtypep-hash-table*))
	 (return-from subtypep-1 (values (subtypep-hash-table-element-subtypep tem)
					 (subtypep-hash-table-element-knownp tem)
					 (subtypep-hash-table-element-dependencies tem)))))
  (macrolet ((record-dependency (x)
	       `(if (cli:listp ,x)
		    (dolist (x ,x) (pushnew x dependencies :test 'eq))
		  (pushnew ,x dependencies :test 'eq))))
   (labels ((record-atomic-dependency (x)
	      (if (atom x)
		  (pushnew x dependencies :test 'eq)
		(case (car x)
		  ((and or not)
		   (dolist (y (cdr x)) (record-atomic-dependencies y)))
		  ((satisfies cli:member global:member))
		  (t (pushnew (car x) dependencies :test 'eq))))))
    (let ((x x) (y y))
      (multiple-value-setq (x dependencies) (type-canonicalize x t dependencies))
      (multiple-value-setq (y dependencies) (type-canonicalize y t dependencies))
      (cond ((or (null x) (eq y t) (equal x y))
	     (setq known-to-be-subtype t
		   known-whether-is-subtype t))
	    ((eq (car-safe y) 'or)		;(subtypep foo '(or ...))
	     (let ((knownp t))
	       (dolist (y (cdr y))
		 (subtypep-3 (t1 t2) x y
		   (when t1
		     (setq known-to-be-subtype t
			   known-whether-is-subtype t)
		     (return nil))
		   (setq knownp (and knownp t2))))
	       (setq known-whether-is-subtype knownp)))
	    ((eq (car-safe y) 'and)		;(subtypep foo '(and ...))
	     (let ((val t))
	       (dolist (y (cdr y))
		 (subtypep-3 (t1 t2) x y
		   (unless t2
		     (return nil))
		   (setq val (and val t1))))
	       (setq known-to-be-subtype val
		     known-whether-is-subtype t)))
	    ((eq (car-safe y) 'not)		;(subtypep foo '(not ...))
	     (multiple-value-bind (t1 t2 tem) (disjoint-typep x (cadr y) dependencies)
	       (setq dependencies tem)
	       (setq known-to-be-subtype t1
		     known-whether-is-subtype (or t2
						  (subtypep-2 x (cadr y))
						  (subtypep-2 (cadr y) x)))))
	    ((eq (car-safe x) 'cli:member)	;(subtypep '(member ...) bar)
	     (setq known-to-be-subtype (loop for z in (cdr x) always (typep z y))
		   known-whether-is-subtype t))
	    ((eq (car-safe x) 'and)		;(subtypep '(and ...) bar)
	     (let ((knownp t))
	       (dolist (x (cdr x))
		 (subtypep-3 (t1 t2) x y
		   (when t1
		     (setq known-to-be-subtype t
			   known-whether-is-subtype t)
		     (return nil))
		   (setq knownp (and knownp t2))))
	       (setq known-whether-is-subtype knownp)))
	    ((eq (car-safe x) 'or)		;(subtypep '(or ...) bar)
	     (let ((val t))
	       (dolist (x (cdr x))
		 (subtypep-3 (t1 t2) x y
		   (unless t2
		     (return nil))
		   (setq val (and val t1))))
	       (setq known-to-be-subtype val
		     known-whether-is-subtype t)))
	    ((eq (car-safe x) 'not)		;(subtypep '(not ...) bar)
	     (multiple-value-bind (nil t2 tem) (disjoint-typep (cadr x) y dependencies)
	       (setq dependencies tem)
	       (setq known-whether-is-subtype (or t2
						  (subtypep-2 (cadr x) y)
						  (subtypep-2 y (cadr x))))))
	    ((eq (car-safe y) 'cli:member))	;(subtypep foo '(member ...))
	    ((eq (car-safe y) 'satisfies))	;(subtypep foo '(satisfies ...))
	    ((eq (car-safe x) 'satisfies))	;(subtypep '(satisfies ...) bar)
	    ((atom y)
	     (setq known-to-be-subtype (atom-subtypep (if (atom x) x (car x)) y)
		   known-whether-is-subtype t))
	    ((atom x)
	     (setq known-whether-is-subtype t))	 
	    (t
	      (unless (setq tem (atom-subtypep (car x) (car y)))
		(setq known-whether-is-subtype t))
	      (if (and tem (setq tem (get (car y) 'subtypep-predicate)))
		  (multiple-value-setq (known-to-be-subtype known-whether-is-subtype dependencies)
		    (funcall tem x y dependencies)))))
      (setq known-whether-is-subtype (not (not known-whether-is-subtype)))
      (setq known-to-be-subtype (not (not known-to-be-subtype)))
      (setq elt (let ((default-cons-area background-cons-area))
		  (make-subtypep-hash-table-element :subtypep known-to-be-subtype
						    :knownp known-whether-is-subtype
						    :dependencies (copylist dependencies))))
      (setf (gethash (cons x y) *subtypep-hash-table*) elt)))
    (setf (gethash (cons x y) *subtypep-hash-table*) elt))
  (values known-to-be-subtype known-whether-is-subtype dependencies))

;;; T if atomic type X is a subtype of atomic type Y.
;;; It is never impossible to tell, so only one value is returned.
(defun atom-subtypep (x y &aux t1 t2
		      (f1 (get-flavor-tracing-aliases x))
		      (f2 (get-flavor-tracing-aliases y)))
  (cond ((eq x y) t)
	(f1
	 (or (eq y 'atom)
	     (and (eq y 'common)
		  (subtypep x '(or pathname hash-table)))
	     (and f2
		  (memq (dont-optimize (flavor-name f2))
			(dont-optimize (flavor-depends-on-all f1)))
		  t)))
	(f2 nil)
	((class-symbolp x)
	 (or (memq y '(atom entity))
	     (and (class-symbolp y)
		  (subclass-of-class-symbol-p x y))))
	((class-symbolp y) nil)
	((or (and (setq t1 (get x 'defstruct-description))
		  (defstruct-description-named-p t1))
	     (get x 'defstruct-named-p))
	 (if (memq x '(structure atom array common))
	     t
	   (and (or (and (setq t2 (get y 'defstruct-description))
			 (defstruct-description-named-p t2))
		    (get y 'defstruct-named-p))
		(do ((symbol x
			     (and (setq t1 (get symbol 'defstruct-description))
				  (car (defstruct-description-include t1)))))
		    ((null symbol) nil)
		  (and (eq y symbol) (return t))))))
	(t (not (not (memq x (get y 'subtypes)))))))

;;;; Comparing canonicalized types 
(defprop array array-subtypep subtypep-predicate)
(defprop simple-array array-subtypep subtypep-predicate)
(defun array-subtypep (type1 type2 dependencies &aux known tem)
  (unless (or (null (cdr type2))		;.		(array)
	      (eq (cadr type2) '*)		;.		(array * ...)
	      (and (cdr type1)
		   (neq (cadr type1) '*)	;(array x ...)
		   (progn
		     (multiple-value-setq (tem known dependencies)
		       (subtypep-1 (cadr type1) (cadr type2) dependencies))
		     (setq tem (and tem (subtypep-2 (cadr type1) (cadr type2))))
		     (if known tem t))))
    (return-from array-subtypep (values nil t dependencies)))
  (if (or (null (cddr type2))			;.		(array x)
	  (eq (caddr type2) '*)			;.		(array x *)
	  (and (cddr type1)
	       (neq (caddr type1) '*)		;(array x y ...)
	       (= (if (numberp (caddr type1)) (caddr type1)
		    (length (caddr type1)))
		  (if (numberp (caddr type2)) (caddr type2)
		    (length (caddr type2))))
	       (cond
		 ((not (consp (caddr type1)))
		  (= (caddr type1) (if (consp (caddr type2)) (length type2) type2)))
		 ((not (consp (caddr type2)))
		  (= (caddr type2) (length type1)))
		 (t (do ((1tail (caddr type1) (cdr 1tail))
			 (2tail (caddr type2) (cdr 2tail)))
			((null 1tail) t)
		      (unless (or (eq (car 2tail) '*)
				  (eql (car 1tail) (car 2tail)))
			(return nil)))))))
      (if known (values t t dependencies) (values nil nil dependencies))
    nil))

(defun (:property complex subtypep-predicate) (type1 type2 dependencies)
  (multiple-value-bind (tem tem1 dependencies)
      (subtypep-1 (cadr type1) (cadr type2) dependencies)
    (values tem tem1 dependencies)))

(defun (:property integer subtypep-predicate) (type1 type2 dependencies)
  (values
    (and (or (memq (cadr type2) '(nil *))
	     (and (not (memq (cadr type1) '(nil *)))
		  ( (if (consp (cadr type1))
			 (1+ (caadr type1)) (cadr type1))
		     (if (consp (cadr type2))
			 (1+ (caadr type2)) (cadr type2)))))
	 (or (memq (caddr type2) '(nil *))
	     (and (not (memq (caddr type1) '(nil *)))
		  ( (if (consp (caddr type1))
			 (1- (caaddr type1)) (caddr type1))
		     (if (consp (caddr type2))
			 (1- (caaddr type2)) (caddr type2))))))
    t
    dependencies))

(defprop rational dense-arithmetic-subtypep subtypep-predicate)
(defprop real dense-arithmetic-subtypep subtypep-predicate)
(defprop float dense-arithmetic-subtypep subtypep-predicate)
(defprop short-float dense-arithmetic-subtypep subtypep-predicate)
(defprop single-float dense-arithmetic-subtypep subtypep-predicate)
(defprop non-complex-number dense-arithmetic-subtypep subtypep-predicate)
(defun dense-arithmetic-subtypep (type1 type2 dependencies)
  (values
    (and (or (memq (cadr type2) '(nil *))
	     (and (not (memq (cadr type1) '(nil *)))
		  (if (and (consp (cadr type2)) (not (consp (cadr type1))))
		      (> (cadr type1) (caadr type2))
		    ( (if (consp (cadr type1))
			   (caadr type1) (cadr type1))
		       (if (consp (cadr type2))
			   (caadr type2) (cadr type2))))))
	 (or (memq (caddr type2) '(nil *))
	     (and (not (memq (caddr type1) '(nil *)))
		  (if (and (consp (caddr type2)) (not (consp (caddr type1))))
		      (< (caddr type1) (caaddr type2))
		    ( (if (consp (caddr type1))
			   (caaddr type1) (caddr type1))
		       (if (consp (caddr type2))
			   (caaddr type2) (caddr type2)))))))
    t
    dependencies))

;;;; disjoint-typep

;;; this variable really isn't as much use any more,
;;;  due to rampant type-canonicalization. Sigh.
(defconst *subtypep-pairwise-disjoint-sets*
  '((integer fixnum bignum)
    (rational ratio integer)
    (number rational float complex)
    (number non-complex-number complex)
    (list cons null)
    (sequence list sequence)
    (t cons symbol array number cli:character global:character entity locative instance
       closure stack-group select compiled-function microcode-function)
    (t list number hash-table readtable package pathname stream random-state)))

(defun disjoint-typep (x y dependencies &aux t1 t2)
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (multiple-value-bind (x dependencies)
      (type-canonicalize x t dependencies)
    (multiple-value-bind (y dependencies)
	(type-canonicalize y t dependencies)
      (cond ((subtypep-2 y x)
	     (return-from disjoint-typep (values nil t dependencies)))
	    ((subtypep-2 x y)
	     (return-from disjoint-typep (values nil t dependencies)))
	    ((consp x)
	     (case (car x)
	       (or
		(loop with val = t
		      for x in (cdr x)
		      do (multiple-value-setq (t1 t2 dependencies)
			   (disjoint-typep x y dependencies))
		      when (not t2) (return-from disjoint-typep (values nil nil dependencies))
		      do (setq val (and val t1))
		      finally (return-from disjoint-typep (values val t dependencies))))
	       (and
		(loop with val = t
		      for x in (cdr x)
		      do (multiple-value-setq (t1 t2 dependencies)
			   (disjoint-typep x y dependencies))
		      when t1 (return-from disjoint-typep (values t t dependencies))
		      do (setq val (and val t2)) 
		      finally (return-from disjoint-typep (values nil val dependencies))))
	       (not
		(subtypep-1 y (cadr x) dependencies))
	       (cli:member
		(loop for x in (cdr x)
		      do (multiple-value-setq (t1 t2 dependencies)
			   (subtypep-1 x y dependencies))
		      when (null t2) return (values nil nil dependencies)
		      when t1 return (values nil t dependencies)
		      finally (return (values t t dependencies))))
	       (satisfies nil)
	       (t
		(cond ((multiple-value-setq (nil nil dependencies)
			 (disjoint-typep (car x) y dependencies))
		       (values t t dependencies))
		      ((atom y) (values nil t dependencies))
		      ((setq t1 (get (car x) 'disjoint-typep-predicate))
		       (funcall t1 x y dependencies))
		      (t (values nil nil dependencies))))))
	    ((not (atom y)) (disjoint-typep y x dependencies))
	    (t (loop for (a . b) in *subtypep-pairwise-disjoint-sets*
		     when (and (subtypep-2 x a) (subtypep-2 y a))
		     do (let ((p (loop for tt in b
				       when (subtypep-2 x tt) return tt))
			      (q (loop for tt in b
				       when (subtypep-2 y tt) return tt)))
			  (when (and p q) (return (values (not (eq p q)) t dependencies))))
		     finally (return (values nil nil dependencies))))))))

(defprop array disjoint-array-typep disjoint-typep-predicate)
(defprop simple-array disjoint-array-typep disjoint-typep-predicate)
(defun disjoint-array-typep (x y dependencies &aux (knownp t))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (if (or (and (cddr x) (not (eq (caddr x) '*))
	       (cddr y) (not (eq (caddr y) '*))
	       (not (cond ((numberp (caddr x))
			   (if (numberp (caddr y))
			       (= x y)
			     (and (= (length y) x)
				  (loop for z in y always (eq z '*)))))
			  ((numberp (caddr y))
			   (and (= (length x) y)
				(loop for z in x always (eq z '*))))
			  (t (and (= (length x) (length y))
				  (loop for z in x
					for w in y
					always (or (eq z '*) (eq w '*) (eq z w))))))))
	  (and (not (eq (cadr x) '*))
	       (not (eq (cadr y) '*))
	       (multiple-value-setq (nil knownp dependencies)
		 (disjoint-typep (multiple-value-setq (nil dependencies)
				   (type-canonicalize (cadr x) t dependencies))
				 (multiple-value-setq (nil dependencies)
				   (type-canonicalize (cadr x) t dependencies))
				 dependencies))))
      (values t t dependencies)
      (values nil knownp dependencies)))

(defprop integer real-disjoint-typep disjoint-typep-predicate)
(defprop rational real-disjoint-typep disjoint-typep-predicate)
(defprop non-complex-number real-disjoint-typep disjoint-typep-predicate)
(defprop float real-disjoint-typep disjoint-typep-predicate)
(defprop short-float real-disjoint-typep disjoint-typep-predicate)
(defprop single-float real-disjoint-typep disjoint-typep-predicate)
(defprop real real-disjoint-typep disjoint-typep-predicate)
(defun real-disjoint-typep (x y dependencies)
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (let ((low1 (if (cdr x) (cadr x) '*))
	(low2 (if (cdr y) (cadr y) '*))
	(high1 (if (cddr x) (caddr x) '*))
	(high2 (if (cddr y) (caddr y) '*)))
    (tagbody
     retry
	(return-from real-disjoint-typep
	  (if (cond ((eq low1 '*)
		     (and (neq high1 '*)
			  (neq low2 '*)
			  (cond ((consp high1)
				 ( (car high1) (if (consp low2) (car low2) low2)))
				((consp low2)
				 ( high1 (car low2)))
				(t (< high1 low2)))))
		    ((eq low2 '*) (go swap))
		    ((eq high1 '*)
		     (and (neq high2 '*)
			  (cond ((consp low1)
				 ( (if (consp high2) (car high2) high2) (car low1)))
				((consp high2)
				 ( (car high2) low1))
				(t (< high2 low1)))))
		    ((eq high2 '*) (go swap))
		    (t
		     (let ((t1 #'<) (t2 #'<))
		       (if (consp low1) (setq low1 (car low1) t1 #'))
		       (if (consp high1) (setq high1 (car high1) t2 #'))
		       (or (funcall t1 (if (consp high2) (car high2) high2) low1)
			   (funcall t2 high1 (if (consp low2) (car low2) low2))))))
	      (values t t dependencies)
	    (values nil nil dependencies)))
     swap
	(psetq low1 low2 low2 low1 high1 high2 high2 high1)
	(go retry))))

(defun (:property complex disjoint-typep-predicate) (x y dependencies &aux (knownp t))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (if (and (neq (cadr x) '*)
	   (neq (cadr y) '*)
	   (multiple-value-setq (nil knownp dependencies)
	     (disjoint-typep (multiple-value-setq (nil dependencies)
			       (type-canonicalize (cadr x) t dependencies))
			     (multiple-value-setq (nil dependencies)
			       (type-canonicalize (cadr x) t dependencies))
			     dependencies)))
      (values t t dependencies)
    (values nil knownp dependencies)))

(defvar *array-element-type-hash-table* :unbound
  "Hash table of element-type for MAKE-ARRAY keyed by the type.")

(defun array-type-from-element-type (element-type)
  "Returns a symbol, such as ART-4B"
  (unless (variable-boundp *array-element-type-hash-table*)
    (let ((default-cons-area background-cons-area))
      (setq *array-element-type-hash-table* (make-hash-table :test #'equal :size 100.))))
  (cond	((cdr (assoc-equal element-type array-element-type-alist)))
	((car (gethash element-type *array-element-type-hash-table*)))
	(t
	 (multiple-value-bind (canon dependencies)
	     (type-canonicalize element-type t nil)
	   (let ((value (or (cdr (assoc-equal canon array-element-type-alist))
			    (cond ((subtypep canon 'fixnum)
				   (cond ((subtypep canon 'bit)
					  'art-1b)	;common case
					 ((subtypep canon '(mod #o10))
					  (if (subtypep canon '(mod 4)) 'art-2b 'art-4b))
					 ((subtypep canon '(mod #o200000))
					  (if (subtypep canon '(mod #o400)) 'art-8b 'art-16B))
					 ((subtypep canon '(signed-byte #o20))
					  'art-half-fix)
					 (t 'art-q)))
				  ((subtypep canon 'cli:character)
				   (cond ((subtypep canon 'string-char)
					  'art-string)
					 ((subtypep canon 'fat-char)
					  'art-fat-string)
					 (t 'art-q)))
				  ((subtypep canon 'float) 'art-float)
				  ((subtypep canon 'complex)
				   (if (subtypep canon '(complex float))
				       'art-complex-float 'art-complex))
				  (t 'art-q)))))
	     (setq value (cons-in-area value dependencies background-cons-area))
	     (setf (gethash canon *array-element-type-hash-table*) value)
	     (setf (gethash element-type *array-element-type-hash-table*) value)
	     (car value))))))

(defun clear-cached-subtype-info (type)
  (when (variable-boundp *subtypep-hash-table*)
    (maphash #'(lambda (key entry)
		 (when (memq type (subtypep-hash-table-element-dependencies entry))
		   (remhash key *subtypep-hash-table*)))
	     *subtypep-hash-table*))
  (when (variable-boundp *array-element-type-hash-table*)
    (maphash #'(lambda (key entry)
		 (when (memq type (cdr entry))
		   (remhash key *array-element-type-hash-table*)))
	     *array-element-type-hash-table*)))

;;; ugly hairy crocks
;;; can you think of a better way?
(defun array-type-subtypep (array-type1 array-type2)
  "Returns an array-type code, such as #.art-q"
  (if (fixnump array-type1)
      (if (zerop (ldb %%array-type-field array-type1))
	  (setq array-type1 (dpb array-type1 %%array-type-field 0))
	(setq array-type1 (mask-field %%array-type-field array-type1)))
    (setq array-type1 (mask-field %%array-type-field (symeval array-type1))))
  (if (fixnump array-type2)
      (if (zerop (ldb %%array-type-field array-type2))
	  (setq array-type2 (dpb array-type2 %%array-type-field 0))
	(setq array-type2 (mask-field %%array-type-field array-type2)))
    (setq array-type2 (mask-field %%array-type-field (symeval array-type2))))
  (case array-type2
    (#.art-q
     t)
    (#.art-q-list
     (not (eq array-type1 #.art-q)))
    ((#.art-stack-group-head #.art-special-pdl #.art-reg-pdl)
     (not (memq array-type1 '(#.art-q #.art-q-list))))
    (#.art-string
     (eq array-type1 #.art-string))
    (#.art-fat-string
     (memq array-type1 '(#.art-string #.art-fat-string)))
    (#.art-32b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b #.art-16b #.art-32b)))
    (#.art-16b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b #.art-16b)))
    (#.art-half-fix
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b #.art-half-fix)))
    (#.art-8b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b)))
    (#.art-4b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b)))
    (#.art-2b
     (memq array-type1 '(#.art-1b #.art-2b)))
    (#.art-1b
     (memq array-type1 '(#.art-1b)))
    (#.art-complex
     (memq array-type1 '(#.art-complex #.art-complex-float #.art-complex-fps-float)))
    (#.art-complex-float
     (memq array-type1 '(#.art-complex-float #.art-complex-fps-float)))
    (#.art-float
     (memq array-type1 '(#.art-float #.art-fps-float)))
    (#.art-fps-float
     (memq array-type1 '(#.art-fps-float)))
    (t nil)))

;;; used by defstruct.
(defun array-type-supertype (array-type1 array-type2 &aux type)
  "Returns a symbol, such as ART-Q."
  (if (fixnump array-type1)
      (if (zerop (ldb %%array-type-field array-type1))
	  (setq array-type1 (dpb array-type1 %%array-type-field 0))
	(setq array-type1 (mask-field %%array-type-field array-type1)))
    (setq array-type1 (mask-field %%array-type-field (symeval array-type1))))
  (if (fixnump array-type2)
      (if (zerop (ldb %%array-type-field array-type2))
	  (setq array-type2 (dpb array-type2 %%array-type-field 0))
	(setq array-type2 (mask-field %%array-type-field array-type2)))
    (setq array-type2 (mask-field %%array-type-field (symeval array-type2))))
  (setq array-type1
	(or (selector nil (lambda (ignore x)
			    (setq type (cond ((eq x array-type1) array-type2)
					     ((eq x array-type2) array-type1))))
	      (#.art-error type)
	      (#.art-q-list #.art-q-list)
	      (#.art-q #.art-q)
	      (#.art-stack-group-head #.art-stack-group-head)
	      (#.art-special-pdl #.art-special-pdl)
	      (#.art-reg-pdl #.art-reg-pdl)
	      (#.art-fat-string
	       (if (memq type '(#.art-string #.art-fat-string)) #.art-fat-string))
	      (#.art-string
	       (if (eq type #.art-string) #.art-string))
	      (#.art-complex-fps-float
	       (case type
		 (#.art-complex-fps-float #.art-complex-fps-float)
		 (#.art-complex-float #.art-complex-float)
		 (#.art-complex #.art-complex)))
	      (#.art-complex-float
	       (case type
		 ((#.art-complex-float #.art-float) #.art-complex-float)
		 (#.art-complex #.art-complex)))
	      (#.art-complex
	       (case type
		 ((#.art-complex #.art-float) #.art-complex)))
	      (#.art-fps-float
	       (case type
		 ((#.art-fps-float #.art-float) art-fps-float)))
	      (#.art-float
	       (case type
		 (#.art-float art-float)))
	      (#.art-half-fix
	       (case type
		 ((#.art-half-fix #.art-1b #.art-2b #.art-4b #.art-8b) #.art-half-fix)))
	      (#.art-32b #.art-32b)
	      (#.art-16b #.art-16b)
	      (#.art-8b #.art-8b)
	      (#.art-4b #.art-4b)
	      (#.art-2b #.art-2b)
	      (#.art-1b #.art-1b))
	    #.art-q))				;rampaging dotulism
  (nth (ldb %%array-type-field array-type1) array-types))

;;;; Open coding of TYPEP.

;In QCOPT:
;(add-optimizer typep typep-two-args typep-structure typep-flavor
;	        subinstance-of-class-symbol-p)
(defun typep-two-args (form &aux opt type pred dtp)
  (cond ((and (= (length form) 3)
	      (constantp (caddr form)))
	 (setq type (if (consp (caddr form))
			(cadr (caddr form))	;(typep foo ':bar)
		      (caddr form)))		;(typep foo :bar)
	 (flet ((frob (type)
		   (if (and (symbolp type)
			    (setq opt (getdecl type 'type-alias-for)))
		       `(typep ,(cadr form) ',opt)
		     (if (and (symbolp type)
			      (setq opt (get type 'type-optimizer)))
			 (funcall opt form)
		       (cond ((symbolp type)
			      (cond ((setq opt (get type 'type-optimizer))
				     (funcall opt form))
				    ((and (setq pred (get type 'type-predicate))
					  (symbolp pred))
				     `(,pred ,(cadr form)))
				    ((setq dtp (or (rassq type type-of-alist)
						   (rassq type typep-one-arg-alist)))
				     `(= (%data-type ,(cadr form)) ,(car dtp)))
; this should also work for flavors defined in compilation
				    ((get type 'si:flavor)
				     `(typep-structure-or-flavor
					,(cadr form)
					',(dont-optimize
					    (flavor-name (get-flavor-tracing-aliases type)))))
				    ((get type 'si:defstruct-description)
				     `(typep-structure-or-flavor . ,(cdr form)))
				    ((class-symbolp type)
				     `(subinstance-of-class-symbol-p ,(cadr form) ',type))
				    (t form)))
			     (t
			      (let ((typecar (get (car type) 'type-alias-for (car type))))
				(cond ((setq opt (get typecar 'type-optimizer))
				       (apply opt form (cdr type)))
				      ((symbolp (setq pred (get typecar 'type-predicate)))
				       `(,pred ,(cadr form)))
				      (t form)))))))))
	   (let ((tem (frob type)))
	     (if (not (equal tem form))
		 tem
	       (setq form (frob (type-canonicalize type nil nil)))
	       (if (equal tem form)
		   (compiler:warn 'bad-type :implausible
				  "The type ~S does not seem to be defined" type))
	       tem))))
	(t form)))

(defun coerce (object result-type &aux canon)
  "Coerce OBJECT to an object of type RESULT-TYPE.  Only certain coercions are allowed.
Any sequence can be coerced to any sequence type if the elements are legal.
Strings, symbols and integers can be coerced to type CHARACTER.
Any number can be coerced to type COMPLEX.
Any real number can be coerced to any floating point number type."
  (setq canon (type-canonicalize result-type nil nil))
  (if (typep object canon)
      object
    (block nil
      (case (if (atom canon) canon (car canon))
	(list
	 (return (coerce-to-list object)))
	(short-float
	 (when (realp object) (return (small-float (realpart object)))))
	(single-float
	 (when (realp object) (return (float (realpart object)))))
	(float
	 (when (realp object) (return (if (small-floatp object)
					  object
					(float (realpart object))))))
	(complex
	 (when (typep object 'number)
	   (return (if (memq (cadr-safe canon) '(nil *))
		       (if (complexp object) object (complex object))
		     (if (complexp object)
			 (complex (coerce (%complex-real-part object) (cadr canon))
				  (coerce (%complex-imag-part object) (cadr canon)))
		       (complex (coerce object (cadr canon))))))))
	(cli:character
	 (return (cli:character object)))
	((array simple-array)
	 (when (typep object 'sequence)
	   (return (coerce-to-vector object
				     (if (atom canon)
					 t
				       (array-type-from-element-type (cadr canon)))
				     (eq (if (atom canon) canon (car canon))
					 'simple-array))))))
      ;; If it did not already RETURN, this coercion is not allowed.
      (ferror nil "~S cannot be coerced to type ~S" object result-type))))

(defun cli:character (x)
  "Convert X to a character if possible."
  (cond ((characterp x) x)
	((numberp x)
	 (int-char x))
	((and (stringp x) (= (length x) 1))
	 (char x 0))
	((and (symbolp x) (= (length (symbol-name x)) 1))
	 (char (symbol-name x) 0))
	(t (ferror nil "Cannot coerce ~S into a character" x))))
(deff coerce-to-character #'cli:character)		;Still used by old compiled code

(defun coerce-to-vector (object array-type &optional simplep &aux vector length)
  (etypecase object
    (vector
     (if (and (= (eval (array-type object)) (eval array-type))
	      (or (not simplep) (simple-vector-p object)))
	 object
       (setq length (length object))
       (setq vector (make-array length :type array-type))
       (dotimes (i length)
	 (setf (cli:aref vector i) (cli:aref object i)))
       vector))
    (list
     (setq length (length object))
     (setq vector (make-array length :type array-type))
     (do ((i 0 (1+ i))
	  (l object (cdr l)))
	 ((null l))
       (setf (cli:aref vector i) (car object)))
     vector)))
(deff coerce-to-array-optimized #'coerce-to-vector)  ;Still used by old compiled code.

(defun coerce-to-list (vector)
  (if (cli:listp vector) vector
    (check-type vector vector "a sequence")
    (let* ((length (length vector))
	   (list (make-list length))
	   (l list))
      (dotimes (x length)
	(setf (car l) (cli:aref vector x))
	(setq l (cdr l)))
      list)))

(defun coerce-optimizer (form)
  (let (frob type canon)
    (if (not (list-match-p form `(coerce ,frob ',type)))
	form
      (setq canon (type-canonicalize type nil nil))
      (case (if (atom canon) canon (car canon))
	(list
	 (once-only (frob)
	   `(if (consp ,frob) ,frob (coerce-to-list ,frob))))
	(short-float `(small-float ,frob))	;not strictly correct, since works on complex
	(single-float `(float ,frob))		;ditto
	(float
	 (once-only (frob)			;ditto
	   `(if (small-floatp ,frob) ,frob (float ,frob))))
	((t) frob)
	(cli:character `(cli:character ,frob))
	(complex
	 (if (memq (cadr-safe canon) '(nil *))
	     (once-only (frob)
	       `(if (complexp ,frob) ,frob
		  (complex frob)))
	   (once-only (frob)
	     `(if (complexp ,frob)
		  (%complex-cons (coerce (%complex-real-part ,frob) ',(cadr canon))
				 (coerce (%complex-imag-part ,frob) ',(cadr canon)))
		(complex (coerce ,frob ',(cadr canon)))))))
	(array
	 `(coerce-to-vector
	    ,frob
	    ',(array-type-from-element-type
		(if (atom canon) t (cadr canon)))
	    nil))
	(simple-array
	 `(coerce-to-vector
	    ,frob
	    ',(array-type-from-element-type
		(if (atom canon) t (cadr canon)))
	    t))
	(t (compiler:warn 'bad-coerce :improbable "Do not know how to coerce to type ~S" type)
	   form)))))

;;;; Pretty names for types (used in CHECK-TYPE, ETYPECASE, etc)

(defvar *type-pretty-name-hash-table* :unbound
  "A hash table containing cached pretty names for types")

(defun type-pretty-name (type)
  "Return a string containing a noun phrase describing objects of type TYPE."
  (unless (variable-boundp *type-pretty-name-hash-table*)
    (let ((default-cons-area background-cons-area))
      (setq *type-pretty-name-hash-table* (make-hash-table :test #'equal :size 400.))))
  (or (gethash type *type-pretty-name-hash-table*)
      (let (pretty-name)
	;; Prevent lossage if TYPE was consed in a temporary area.
	(setq type (copytree type background-cons-area)
	      pretty-name
	      	   (cond ((symbolp type)
			  (or (get type 'type-name)
			      (string-append-a-or-an
				(string-subst-char #/space #/-
						   (string-downcase (format nil "~a" type))
						   nil))))
			 ((and (consp type)
			  (funcall (get (car type) 'type-name-function #'ignore) type)))
			 (t (string-append (format nil "an object of type ~S" type)))))
	(let ((default-cons-area background-cons-area))
	  (setq pretty-name (copy-seq pretty-name)))
	(setf (gethash type
		       *type-pretty-name-hash-table*) pretty-name
;	      (gethash (type-canonicalize type nil nil)
;		       *type-pretty-name-hash-table*) pretty-name)
	      ))))

(defun (:property or type-name-function) (type)
  (setq type (block or
	       (mapcan #'(lambda (x)
			   (cond ((eq (car-safe x) 'or)
				  (cdr x))
				 ((eq x t)
				  (return-from or t))
				 ((eq x nil)
				  ())
				 (t (list x))))
		       (cdr type))))
  (if (cdr type)
      (string-append
	(format:output nil
	  (do ((tail type (cdr tail)))
	      ((null tail))
	    (unless (cdr tail)
	      (princ "or "))
	    (princ (type-pretty-name (car tail)))
	    (when (cdr tail)
	      (if (cddr tail)
		  (princ ", ")
		(tyo #/space))))))
    (type-pretty-name (car type))))


(defun (:property and type-name-function) (type)
  (setq type (block and
	       (mapcan #'(lambda (x)
			   (cond ((eq (car-safe x) 'and)
				  (cdr x))
				 ((eq x nil)
				  (return-from and nil))
				 ((eq x t)
				  ())
				 (t (list x))))
		       (cdr type))))
  (if (cdr type)
      (string-append
	(format:output nil
	  (princ (type-pretty-name (car type)))
	  " which is also "
	  (do ((tail (cdr type) (cdr tail)))
	      ((null tail))
	    (unless (or (cdr tail) (not (cddr type)))
	      (princ "and "))
	    (princ (type-pretty-name (car tail)))
	    (when (cdr tail)
	      (if (cddr tail)
		  (princ ", ")
		(tyo #/space))))))
    (type-pretty-name (car type))))

(defprop global:member member-type-name-function type-name-function)
(defprop cli:member member-type-name-function type-name-function)
(defun member-type-name-function (type &aux (len (length (cdr type))))
  (case len
    (0 nil)
    (1 (string-append (format nil "EQL ~S" (cadr type))))
    (2 (string-append (format nil "one of either ~S or ~S" (cadr type) (caddr type))))
    (t (string-append
	 (format:output nil
	   (princ "EQL one of ")
	   (do ((tail (cdr type) (cdr tail)))
	       ((null tail))
	     (unless (cdr tail)
	       (princ "or "))
	     (prin1 (car tail))
	     (when (cdr tail)
	       (if (cddr tail)
		   (princ ", ")
		 (tyo #/space)))))))))

(defun (:property fixnum type-name-function) (type)
  (integer-type-name type "fixnum" "a "))
(defun (:property integer type-name-function) (type)
  (integer-type-name type "integer" "an "))
(defun integer-type-name (type noun article)
  (let ((low (cond ((null (cdr type)) '*)
		   ((consp (cadr type)) (1+ (car (cadr type))))
		   ((integerp (cadr type)) (cadr type))
		   (t (cadr type))))
	(high (cond ((null (cddr type)) '*)
		    ((consp (caddr type)) (1- (car (caddr type))))
		    ((integerp (caddr type)) (caddr type))
		    (t (caddr type)))))
    (cond ((and (eq low '*) (eq high '*))
	   (string-append article noun))
	  ((and (eq low 0) (eq high '*))
	   (string-append article "positive " noun))
	  ((and (eq high 0) (eq low '*))
	   (string-append article "negative " noun))
	  ((eq high '*)
	   (if (integerp low) (format nil "~A~A greater than ~D" article noun low)))
	  ((eq low '*)
	   (if (integerp high) (format nil "~A~A less than ~D" article noun high)))
	  ((not (and (integerp low) (integerp high)))
	   nil)
	  ((= low high)
	   (format nil "the ~A ~D" noun low))
	  ((= high (1+ low))
	   (format nil "either ~D or ~D" low high))
	  ((< high low)
	   nil)
	  (t
	   (format nil "~A~A between ~D and ~D (incl)" article noun low high)))))

(defun (:property real type-name-function) (type)
  (real-type-name-function "real number" "number" type))
(defun (:property float type-name-function) (type)
  (real-type-name-function "float" "float" type))
(defun (:property short-float type-name-function) (type)
  (real-type-name-function "short float" "short-float" type))
(defun (:property non-complex-number type-name-function) (type)
  (real-type-name-function "non-complex number" "number" type))
(defun real-type-name-function (string short-string type)
  (let ((low (if (null (cdr type)) '* (cadr type)))
	(high (if (null (cddr type)) '* (caddr type)))
	lowex highex)
    (if (consp low) (setq low (car low) lowex t))
    (if (consp high) (setq high (car high) highex t))
    (cond ((and (eq low '*) (eq high '*))
	   (string-append "a " string))
	  ((and (eq low 0) (eq high '*))
	   (if lowex
	       (string-append "a positive " string)
	     (string-append "a non-negative " string)))
	  ((and (eq high 0) (eq low '*))
	   (if highex
	       (string-append "a negative " string)
	     (string-append "a non-positive " string)))
	  ((eq high '*)
	   (format nil "a ~A ~:[~;>~] ~D" string lowex low))
	  ((eq low '*)
	   (format nil "a ~A ~:[~;<~] ~D" string highex high))
	  (t (format nil "a ~A satisfying ~D ~:[~;<~] ~A ~:[~;<~] ~D"
		     string low lowex short-string highex high)))))

(defun (:property complex type-name-function) (type)
  (case (cadr type)
    ((nil * non-complex-number) "a complex number")
    (rational "a rational complex number")
    (short-float "a complex number with short-float components")
    (single-float "a complex number with single-float components")
    (long-float "a complex number with long-float components")
    (double-float "a complex number with double-float components")
    (float "a complex number with floating-point components")
    (t nil)))
