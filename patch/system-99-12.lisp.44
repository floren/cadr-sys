;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:T; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.12
;;; Reason:
;;;  Turn on printing of "::" package-internal prefixes
;;;  let-if compilation
;;;  (:method tv:sheet :bitblt) wrapping (tim)
;;;  more type system hairyness and fixes
;;;   -- fix compilation, dont' let loser wedge system types, etc etc
;;;  defresource :deinitializer option
;;;  robustify inspector against nasty data-types and locatives to weird frobs
;;;  don't cons processes in temporary areas...
;;;  qfile unsigned-byte streams remmeber their byte size
;;;  format ~t agress with doc and pleblisp
;;;  zwei:command-lookup no-indirection-p works
;;;  keep saz happy and make c-sh-e in zmacs use specials-ok eval
;;;  tv:with-mouse-grabbed-on-sheet
;;;    -- like tv:with-mouse-grabbed, but restricts mouse to be on given sheet
;;;  throw was bogously only accepting symbols as tags
;;;  eval-when doesn't throw away lexical env
;;;  random documentation fixes
;;;  default pathnames in zmacs ok even if not logged in (khs)
;;;  More macro declaration fixes
;;;  #, #. reader macros use lexical eval
;;;  LOCAL as pathname-name for local lispm
;;; Written 10/30/84 17:38:55 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.9, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, Experimental Macsyma 4.0, microcode 320, GC@2.



(eval-when (eval compile load)
  (export (intern "WITH-MOUSE-GRABBED-ON-SHEET" 'tv) 'tv)
  (globalize (intern "SELF-EVALUATING-P" 'si) 'global)
  )

;; Don't know where this octal lossage came from...
(setq-globally *read-base* 10. *print-base* 10.)

(setf (si::pttbl-package-internal-prefix si::initial-readtable) "::"
      (si::pttbl-package-internal-prefix si::standard-readtable) "::")

(eval-when (eval compile load)
; From file QCOPT.LISP OZ:<L.SYS> OZ: (136)
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defrewrite let-if-expand let-if (cond let) (form)
  (destructuring-bind (ignore cond vars-and-vals &body body) form
    (cond ((null cond) `(let () . ,body))		;Macros generate this
	  ((eq cond t) `(let ,vars-and-vals . ,body))	;and this
	  (t (multiple-value-bind (body decls)
;>> need to pass environment into extract-declarations here
		 (extract-declarations body local-declarations nil)
	       `(let ()
		  (declare . ,decls)
		  (cond (,cond ,(pbind vars-and-vals)))
		  . ,body))))))

)))


; From file SHWARM.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFMETHOD (SHEET :BITBLT) (ALU WID HEI FROM-ARRAY FROM-X FROM-Y TO-X TO-Y
				   &AUX (IL (SHEET-INSIDE-LEFT))
				        (IT (SHEET-INSIDE-TOP))
					(IW (SHEET-INSIDE-WIDTH))
					(IH (SHEET-INSIDE-HEIGHT)))
  (LET* ((ABS-WID (ABS WID))
	 (ABS-HEI (ABS HEI))
	 (LEFT-OVERRUN   (MAX 0 (- TO-X)))
	 (RIGHT-OVERRUN  (MAX 0 (- (+ TO-X ABS-WID) IW)))
	 (TOP-OVERRUN    (MAX 0 (- TO-Y)))
	 (BOTTOM-OVERRUN (MAX 0 (- (+ TO-Y ABS-HEI) IH)))
	 (CLIPPED-WID (* (IF (MINUSP WID) -1 1)
			 (MAX 0 (- ABS-WID LEFT-OVERRUN RIGHT-OVERRUN))))
	 (CLIPPED-HEI (* (IF (MINUSP HEI) -1 1)
			 (MAX 0 (- ABS-HEI TOP-OVERRUN BOTTOM-OVERRUN)))))
    (AND (NOT (ZEROP CLIPPED-WID))				;bitblt errs when w=h=0
	 (NOT (ZEROP CLIPPED-HEI))				;and dims are out of bounds
	 (PREPARE-SHEET (SELF)
	   (BITBLT ALU
		   CLIPPED-WID CLIPPED-HEI
		   FROM-ARRAY
		   (\ (+ FROM-X LEFT-OVERRUN) (PIXEL-ARRAY-WIDTH FROM-ARRAY))  ;***
		   (\ (+ FROM-Y TOP-OVERRUN) (PIXEL-ARRAY-HEIGHT FROM-ARRAY))  ;***
		   SCREEN-ARRAY
		   (+ IL (MIN (MAX 0 TO-X) IW)) (+ IT (MIN (MAX 0 TO-Y) IH)))))))

))

(setf (cdr (assq :coke-bottle si::xr-special-character-names)) (+ 128. 69.)
      (cdr (assq :cokebottle si::xr-special-character-names)) (+ 128. 69.))

(remprop 'fix 'si::alias-type)
(defprop fix integer si::type-alias-for)

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defconst *standard-system-type-specifiers*
	  '(and or not cli:member member satisfies
	    array atom bignum bit bit-vector cli:character character common compiled-function
	    complex cons double-float entity si::fat-char fix fixnum float flonum function
	    hash-table instance integer keyword list locative long-float microcode-function
	    mod named-structure nil non-complex-number null number package pathname
	    random-state ratio rational readtable real select si::select-method sequence
	    short-float signed-byte simple-array simple-bit-vector simple-string simple-vector
	    single-float small-float small-flonum standard-char stream string string-char
	    structure symbol t unsigned-byte values vector
	    :array :atom :bignum :character :closure :compiled-function :complex :cons
	    :entity :fat-char :fix :fixnum :float :flonum :integer :list :locative
	    :microcode-function :named-structure :null :number :ratio :rational :select
	    :select-method :small-flonum :string :structure :symbol))

(defmacro deftype (name arglist &body body)
  "Defines NAME as a data type name for use in TYPEP, etc.
A list starting with NAME, used as a type specifier,
expands by binding the args in ARGLIST and then evaluating the BODY.
The value of BODY should be another type specifier.
Any optional arguments in ARGLIST which do not have default values specified
will be bound to * by default, rather than NIL."
  (check-type name symbol)
  (cond ((memq name *standard-system-type-specifiers*)
	 (ferror nil "~~S is the name of a standard type specifier used by the system.
Redefining it would probably break the world.~" name))
	((or (getdecl name 'defstruct-description)
	     (let ((tem (assq 'si::flavors file-local-declarations)))
	       (and tem (get tem name)))
	     (get name 'si::flavor))
	 (cerror "Yes, please. I want to lose. ~S ~S anyway"
		 "~*~S is already the name of a ~:[flavor~;structure~]
  ~/(~S ~S ...) will cause (~S foo '~S) not to recognize existing
~:[instances of that flavor~;structures of that type~] in new code, ~
but not affect (~S foo '~S)~%in old compiled code. You may lose!"
		 'deftype name (getdecl name 'defstruct-description) 'deftype name 'typep name
		 (getdecl name 'defstruct-description) 'typep name)))
  (let ((argcopy (copy-list arglist))
	optionalf doc)
    (if (stringp (car body)) (setq doc (car body)))
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
	 (remprop ',name 'type-alias-for)
	 (setf (documentation ',name 'type) ',doc))
       (eval-when (compile)
	 (putdecl ',name #'(lambda ,argcopy . ,body) 'type-expander))
       ',name)))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun type-canonicalize (typespec record-dependencies dependencies
			  &aux tem (deps dependencies))
  "Returns a typespec equivalent in meaning to TYPESPEC, but possibly simpler."
  (declare (values canonicalized-type dependencies))
  (macrolet ((record-dependency (x)
	       `(if record-dependencies
		    (if (cli:listp ,x)
			(dolist (foo ,x) (pushnew foo dependencies :test #'eq))
		      (pushnew ,x dependencies :test #'eq))))
	     (type-canonicalize-1 (x)
	       `(multiple-value-setq (nil dependencies)
		    (type-canonicalize ,x record-dependencies dependencies)))
	     (lossage () `(return-from lossage)))
    (flet ((find-tail-of-same-type (y list &aux x)
	     (setq x (if (consp y) (car y) y))
	     (unless (memq x '(and or not cli:member global:member satisfies))
	       (do ((z list (cdr z)))
		   ((null z))
		 (when (or (eq (car z) x) (eq (caar-safe z) x))
		   (return z))))))
      (block lossage
	(condition-case ()
	  (return-from type-canonicalize
	    (values
	      (block canon
		(cond ((symbolp typespec)
		       (cond ((memq typespec *standard-system-type-specifiers*)
			      (or (get typespec 'type-alias-for)
				  (if (setq tem (get typespec 'type-expander))
				      (funcall tem)
				    typespec)))
			     ((setq tem (get typespec 'type-alias-for))
			      (record-dependency typespec)
			      (type-canonicalize-1 tem))
			     ((setq tem (getdecl typespec 'type-expander))
			      (record-dependency typespec)
			      (type-canonicalize-1 (funcall tem)))
			     ;;>> trace aliases?
			     ((and (setq tem (assq 'si::flavors file-local-declarations))
				   (get tem typespec))
			      typespec)
			     ((getdecl typespec 'defstruct-description) typespec)
			     ((get typespec 'si:flavor) typespec)
			     (t (lossage))))
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
				((setq tem (getdecl (car typespec) 'type-expander))
				 (record-dependency (car typespec))
				 (apply tem (cdr typespec)))
				((memq (car typespec) '(array simple-array complex))
				 (let ((subtype (cadr typespec)))
				   (if (memq subtype '(nil t *))
				       typespec
				     (multiple-value-setq (tem dependencies)
				       (type-canonicalize subtype
							  record-dependencies
							  dependencies))
				     (if (equal tem subtype)
					 typespec
				       `(,(car typespec) ,tem . ,(cddr typespec))))))
				((memq (car typespec) *standard-system-type-specifiers*)
				 typespec)
				(t (lossage))))))
		      (t (lossage))))
	      dependencies))			;second value to return from type-canonicalize
	    (error)))
      (type-canonicalize
	(cerror :argument-value nil 'wrong-type-argument "~*~S invalid typespec."
		'type-specification typespec 'typespec)
	record-dependencies deps))))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defvar *use-subtypep-cache-p*)
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
      (let ((*use-subtypep-cache-p* t))
	(subtypep-1 x y nil))
    (values known-to-be-subtype known-whether-is-subtype)))

(defun compilation-subtypep (x y)
  (declare (values known-to-be-subtype known-whether-is-subtype))
  (multiple-value-bind (known-to-be-subtype known-whether-is-subtype)
      (let ((*use-subtypep-cache-p* nil))
	(subtypep-1 x y nil))
    (values known-to-be-subtype known-whether-is-subtype)))

(defun subtypep-1 (x y dependencies
		   &aux (known-to-be-subtype nil) (known-whether-is-subtype nil) tem elt
		        (cachep *use-subtypep-cache-p*))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (cond ((and (symbolp y) (setq tem (get y 'subtypes)))
	 (if (memq (if (atom x) x (car x)) tem)
	     (return-from subtypep-1 (values t t dependencies))))
	((memq y '(t nil)) (return-from subtypep-1 (values y t dependencies)))
	((and cachep
	      (setq tem (gethash (cons x y) *subtypep-hash-table*))
	      (return-from subtypep-1
		(values (subtypep-hash-table-element-subtypep tem)
			(subtypep-hash-table-element-knownp tem)
			(subtypep-hash-table-element-dependencies tem))))))
  (macrolet ((record-dependency (x)
	       `(and cachep
		     (not (memq ,x *standard-system-type-specifiers*))
		     (if (cli:listp ,x)
			 (dolist (x ,x) (pushnew x dependencies :test #'eq))
		       (pushnew ,x dependencies :test #'eq)))))
   (labels ((record-atomic-dependency (x)
 	      (and cachep
		   (if (atom x)
		       (unless (memq x *standard-system-type-specifiers*)
			 (pushnew x dependencies :test #'eq))
		     (case (car x)
		       ((and or not)
			(dolist (y (cdr x)) (record-atomic-dependency y)))
		       ((satisfies cli:member global:member))
		       ((not (memq x *standard-system-type-specifiers*))
			(pushnew (car x) dependencies :test #'eq)))))))
    (let ((x x) (y y))
      (multiple-value-setq (x dependencies) (type-canonicalize x cachep dependencies))
      (multiple-value-setq (y dependencies) (type-canonicalize y cachep dependencies))
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
      (when cachep
	(setq elt (let ((default-cons-area background-cons-area))
		    (make-subtypep-hash-table-element :subtypep known-to-be-subtype
						      :knownp known-whether-is-subtype
						      :dependencies (copylist dependencies))))
	(setf (gethash (cons x y) *subtypep-hash-table*) elt))))
    (when cachep (setf (gethash (cons x y) *subtypep-hash-table*) elt)))
  (values known-to-be-subtype known-whether-is-subtype dependencies))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun atom-subtypep (x y &aux t1 t2
		      (f1 (get-flavor-tracing-aliases x))
		      (f2 (get-flavor-tracing-aliases y)))
  (cond ((eq x y) t)
	(f1
	 (or (eq y 'atom)
	     (and (eq y 'common)
		  (subtypep-1 x '(or pathname hash-table) nil))
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
	((or (and (setq t1 (getdecl x 'defstruct-description))
		  (defstruct-description-named-p t1))
	     (get x 'defstruct-named-p))
	 (if (memq x '(structure atom array common))
	     t
	   (and (or (and (setq t2 (getdecl y 'defstruct-description))
			 (defstruct-description-named-p t2))
		    (get y 'defstruct-named-p))
		(do ((symbol x
			     (and (setq t1 (getdecl symbol 'defstruct-description))
				  (car (defstruct-description-include t1)))))
		    ((null symbol) nil)
		  (and (eq y symbol) (return t))))))
	(t (not (not (memq x (get y 'subtypes)))))))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun disjoint-typep (x y dependencies &aux t1 t2)
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (multiple-value-bind (x dependencies)
      (type-canonicalize x *use-subtypep-cache-p* dependencies)
    (multiple-value-bind (y dependencies)
	(type-canonicalize y *use-subtypep-cache-p* dependencies)
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

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

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
				   (type-canonicalize (cadr x)
						      *use-subtypep-cache-p* dependencies))
				 (multiple-value-setq (nil dependencies)
				   (type-canonicalize (cadr x)
						      *use-subtypep-cache-p* dependencies))
				 dependencies))))
      (values t t dependencies)
      (values nil knownp dependencies)))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property complex disjoint-typep-predicate) (x y dependencies &aux (knownp t))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (if (and (neq (cadr x) '*)
	   (neq (cadr y) '*)
	   (multiple-value-setq (nil knownp dependencies)
	     (disjoint-typep (multiple-value-setq (nil dependencies)
			       (type-canonicalize (cadr x)
						  *use-subtypep-cache-p* dependencies))
			     (multiple-value-setq (nil dependencies)
			       (type-canonicalize (cadr x)
						  *use-subtypep-cache-p* dependencies))
			     dependencies)))
      (values t t dependencies)
    (values nil knownp dependencies)))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun array-type-from-element-type (element-type)
  "Returns a symbol, such as ART-4B"
  (unless (variable-boundp *array-element-type-hash-table*)
    (let ((default-cons-area background-cons-area))
      (setq *array-element-type-hash-table* (make-hash-table :test #'equal :size 100.))))
  (let ((*use-subtypep-cache-p* t))
    (array-type-from-element-type-1 element-type)))

(defun compilation-array-type-from-element-type (element-type)
  (let ((*use-subtypep-cache-p* nil))
    (array-type-from-element-type-1 element-type)))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun array-type-from-element-type-1 (element-type)
  (cond	((cdr (assoc-equal element-type array-element-type-alist)))
	((and *use-subtypep-cache-p*
	      (car (gethash element-type *array-element-type-hash-table*))))
	(t
	 (multiple-value-bind (canon dependencies)
	     (type-canonicalize element-type *use-subtypep-cache-p* nil)
	   (let ((value (or (cdr (assoc-equal canon array-element-type-alist))
			    (cond ((subtypep-1 canon 'fixnum dependencies)
				   (cond ((subtypep-1 canon 'bit dependencies)
					  'art-1b)	;common case
					 ((subtypep-1 canon '(mod #o10) dependencies)
					  (if (subtypep-1 canon '(mod 4) dependencies)
					      'art-2b 'art-4b))
					 ((subtypep-1 canon '(mod #o200000) dependencies)
					  (if (subtypep-1 canon '(mod #o400) dependencies)
					      'art-8b 'art-16B))
					 ((subtypep-1 canon '(signed-byte #o20) dependencies)
					  'art-half-fix)
					 (t 'art-q)))
				  ((subtypep-1 canon 'cli:character dependencies)
				   (cond ((subtypep-1 canon 'string-char dependencies)
					  'art-string)
					 ((subtypep-1 canon 'fat-char dependencies)
					  'art-fat-string)
					 (t 'art-q)))
				  ((subtypep-1 canon 'float dependencies) 'art-float)
				  ((subtypep-1 canon 'complex dependencies)
				   (if (subtypep-1 canon '(complex float) dependencies)
				       'art-complex-float 'art-complex))
				  (t 'art-q)))))
	     (prog1 value
		    (when *use-subtypep-cache-p*
		      (setq value (cons-in-area value dependencies background-cons-area))
		      (setf (gethash canon *array-element-type-hash-table*) value)
		      (setf (gethash element-type *array-element-type-hash-table*) value))))))))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun typep-two-args (form &aux type)
  (cond ((and (= (length form) 3)
	      (self-evaluating-p (caddr form)))
	 (condition-case (error)
	     (progn
	       (setq type (if (consp (caddr form))
			      (cadr (caddr form))	;(typep foo ':bar)
			      (caddr form)))		;(typep foo :bar)
	       (flet ((frob (type &aux tem)
			(if (and (symbolp type)
				 (setq tem (get type 'type-optimizer)))
			    (funcall tem form)
			  (if (and (symbolp type)
				   (setq tem (get type 'type-alias-for)))
			      `(typep ,(cadr form) ',tem)
			    (cond ((symbolp type)
				   (cond ((setq tem (get type 'type-optimizer))
					  (funcall tem form))
					 ((and (setq tem (get type 'type-predicate))
					       (symbolp tem))
					  `(,tem ,(cadr form)))
					 ((setq tem (or (rassq type type-of-alist)
							(rassq type typep-one-arg-alist)))
					  `(= (%data-type ,(cadr form)) ,(car tem)))
					 ((getdecl type 'si::defstruct-description)
					  `(typep-structure-or-flavor . ,(cdr form)))
					 ;; defflavor is so nauseating...
					 ((setq tem (or (and (setq tem (assq 'si::flavors file-local-declarations))
							     (get tem type))
							(get type 'si:flavor)))
					  ;; this is from get-flavor-tracing-aliases
					  ;; let's hear it for modularity...
					  (if (flavor-get tem :alias-flavor)
					      (setq type (car (dont-optimize
								(flavor-depends-on tem))))
					    (setq type tem))
					  `(typep-structure-or-flavor
					     ,(cadr form)
					     ',(dont-optimize (flavor-name type))))
					 ((class-symbolp type)
					  `(subinstance-of-class-symbol-p ,(cadr form)
									  ',type))
					 (t form)))
				  (t
				   (let ((typecar (get (car type) 'type-alias-for 
						       (car type))))
				     (cond ((setq tem (get typecar 'type-optimizer))
					    (apply tem form (cdr type)))
					   ((symbolp (setq tem (get typecar 'type-predicate)))
					    `(,tem ,(cadr form)))
					   (t form)))))))))
		  (let ((tem (frob type)))
		    (if (not (equal tem form))
			tem
			(setq form (frob (type-canonicalize type nil nil)))
			tem))))
	   (error (compiler::warn 'compiler::bad-type-specification :implausible
				  "Error expanding type specification ~S for ~S:~%   ~A"
				  (caddr form) 'typep error)
		   form)))
	((cdddr form)
	 (compiler::warn 'compiler::bad-type-specification :implausuble
			 "~S is a malformed type-specification" (cddr form))
	 form)
	(t form)))

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFSTRUCT (RESOURCE (:TYPE :NAMED-ARRAY-LEADER) (:ALTERANT NIL)
		     (:CONC-NAME RESOURCE-))
  NAME				;Symbol which names it
  (N-OBJECTS 0)			;Number of objects on the free list.
  PARAMETIZER			;Function which defaults the parameters and returns list
  CONSTRUCTOR			;Constructor function
  FINDER			;Optional finder function
  MATCHER			;Optional matcher function
  CHECKER			;Optional checker function
  INITIALIZER			;Optional initializer function
  DEINITIALIZER			;Optional deinitializer function
  )

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(defun resource-deinitializer (resource)
  (and (> (array-leader-length resource) 9) (array-leader resource 9.)))

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFSELECT ((:PROPERTY RESOURCE NAMED-STRUCTURE-INVOKE))
  (:DESCRIBE (RESOURCE &AUX (N-OBJECTS (RESOURCE-N-OBJECTS RESOURCE)))
    (DESCRIBE-DEFSTRUCT RESOURCE)
    (COND ((ZEROP N-OBJECTS)
	   (FORMAT T "~&There are currently no objects.~%"))
	  (T (FORMAT T "~&There ~[~;is~:;are~] currently ~:*~D object~:P:~@
			Object~40TParameters~60TIn Use"
		     N-OBJECTS)
	     (LOOP FOR I FROM 0 BELOW N-OBJECTS DOING
		   (FORMAT T "~%~S~40T~S~60T~:[No~;Yes~]"
			   (RESOURCE-OBJECT RESOURCE I)
			   (RESOURCE-PARAMETERS RESOURCE I)
			   (RESOURCE-IN-USE-P RESOURCE I)))
	     (FORMAT T "~%"))))
  (:PRINT-SELF (RESOURCE STREAM &REST IGNORE  &AUX (N-OBJECTS (RESOURCE-N-OBJECTS RESOURCE)))
    (PRINTING-RANDOM-OBJECT (RESOURCE STREAM :TYPE)
      (FORMAT STREAM "~S (~D object~:P, ~D in use)"
	      (RESOURCE-NAME RESOURCE) N-OBJECTS
	      (LOOP FOR I FROM 0 BELOW N-OBJECTS COUNT (RESOURCE-IN-USE-P RESOURCE I))))))

))



; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFMACRO DEFRESOURCE (NAME PARAMETERS &REST OPTIONS)
  "Define a resource named NAME, with parameters PARAMETERS for constructing objects.
OPTIONS can specify how to create objects and how to tell when old objects can be reused.
Options are :CONSTRUCTOR (required) :FINDER :MATCHER :CHECKER :INITIALIZER :DEINITIALIZER
 :INITIAL-COPIES :FREE-LIST-SIZE See the manual for details."
  (LET ((CONSTRUCTOR-FORM NIL) (FINDER-FORM NIL) (MATCHER-FORM NIL) (CHECKER-FORM NIL)
	(CONSTRUCTOR-FUNCTION NIL) (FINDER-FUNCTION NIL) (MATCHER-FUNCTION NIL)
	(PARAMETIZER-FUNCTION NIL) (CHECKER-FUNCTION NIL) (INITIAL-COPIES 0)
	(INITIALIZER-FORM NIL) (INITIALIZER-FUNCTION NIL)
	(DEINITIALIZER-FORM NIL) (DEINITIALIZER-FUNCTION NIL)
	(FREE-LIST-SIZE 20.) (PARAMS NIL)
	(DOCUMENTATION NIL))
    (UNLESS (CLI:LISTP PARAMETERS)
      (FERROR NIL "~S invalid parameter list" PARAMETERS))
    (SETQ PARAMS (LOOP FOR P IN PARAMETERS
		       UNLESS (MEMQ P LAMBDA-LIST-KEYWORDS)
		       COLLECT (IF (SYMBOLP P) P (CAR P))))
    ;; if first option is a string, use it as documentation instead
    (WHEN (STRINGP (CAR OPTIONS))
      (SETQ DOCUMENTATION (POP OPTIONS)))
    (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY 'CDDR
	  DO (CASE KEYWORD
	       (:CONSTRUCTOR (SETQ CONSTRUCTOR-FORM VALUE))
	       (:FINDER (SETQ FINDER-FORM VALUE))
	       (:MATCHER (SETQ MATCHER-FORM VALUE))
	       (:CHECKER (SETQ CHECKER-FORM VALUE))
	       (:INITIALIZER (SETQ INITIALIZER-FORM VALUE))
	       (:DEINITIALIZER (SETQ DEINITIALIZER-FORM VALUE))
	       (:INITIAL-COPIES
		(SETQ INITIAL-COPIES
		      (COND ((NULL VALUE) 0)
			    ((NUMBERP VALUE) VALUE)
			    (T (FERROR NIL "~S ~S - number required"
				       :INITIAL-COPIES VALUE)))))
	       (:FREE-LIST-SIZE
		(SETQ FREE-LIST-SIZE
		      (COND ((NULL VALUE) 20.)
			    ((NUMBERP VALUE) VALUE)
			    (T (FERROR NIL "~S ~S - number required"
				       :FREE-LIST-SIZE VALUE)))))
	       (OTHERWISE (FERROR NIL "~S unknown option in ~S" 'DEFRESOURCE KEYWORD))))
    (OR CONSTRUCTOR-FORM (FERROR NIL "~S requires the ~S option" 'DEFRESOURCE :CONSTRUCTOR))
    ;; Pick function names.  Note that NIL is SYMBOLP.
    (SETQ CONSTRUCTOR-FUNCTION (IF (SYMBOLP CONSTRUCTOR-FORM) CONSTRUCTOR-FORM
				 `(:PROPERTY ,NAME RESOURCE-CONSTRUCTOR)))
    (SETQ FINDER-FUNCTION (IF (SYMBOLP FINDER-FORM) FINDER-FORM
			    `(:PROPERTY ,NAME RESOURCE-FINDER)))
    (SETQ MATCHER-FUNCTION (IF (SYMBOLP MATCHER-FORM) MATCHER-FORM
			     `(:PROPERTY ,NAME RESOURCE-MATCHER)))
    (SETQ CHECKER-FUNCTION (IF (SYMBOLP CHECKER-FORM) CHECKER-FORM
			     `(:PROPERTY ,NAME RESOURCE-CHECKER)))
    (SETQ INITIALIZER-FUNCTION (IF (SYMBOLP INITIALIZER-FORM) INITIALIZER-FORM
				 `(:PROPERTY ,NAME RESOURCE-INITIALIZER)))
    (SETQ DEINITIALIZER-FUNCTION (IF (SYMBOLP DEINITIALIZER-FORM) DEINITIALIZER-FORM
				 `(:PROPERTY ,NAME RESOURCE-DEINITIALIZER)))
    (SETQ PARAMETIZER-FUNCTION (IF (AND PARAMETERS (NOT MATCHER-FORM) (NOT FINDER-FORM))
				   `(:PROPERTY ,NAME RESOURCE-PARAMETIZER)))
    `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,NAME DEFRESOURCE))
       ,(IF (NOT (SYMBOLP CONSTRUCTOR-FORM))
	    `(DEFUN ,CONSTRUCTOR-FUNCTION (IGNORE ,@PARAMETERS)
	       ,@PARAMS
	       ,CONSTRUCTOR-FORM))
       ,(IF (NOT (SYMBOLP FINDER-FORM))
	    `(DEFUN ,FINDER-FUNCTION (IGNORE ,@PARAMETERS)
	       ,@PARAMS
	       ,FINDER-FORM))
       ,(IF (NOT (SYMBOLP MATCHER-FORM))
	    `(DEFUN ,MATCHER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
	       ,@PARAMS
	       ,MATCHER-FORM))
       ,(IF (NOT (SYMBOLP CHECKER-FORM))
	    `(DEFUN ,CHECKER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
				       ,@PARAMETERS)
	       ,@PARAMS ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
	       ,CHECKER-FORM))
       ,(IF (NOT (SYMBOLP INITIALIZER-FORM))
	    `(DEFUN ,INITIALIZER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
	       ,@PARAMS ,(INTERN "OBJECT")
	       ,INITIALIZER-FORM))
       ,(IF (NOT (SYMBOLP INITIALIZER-FORM))
	    `(DEFUN ,DEINITIALIZER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
	       ,@PARAMS ,(INTERN "OBJECT")
	       ,DEINITIALIZER-FORM))
       ,(IF PARAMETIZER-FUNCTION
	    `(DEFUN ,PARAMETIZER-FUNCTION ,PARAMETERS
	       (LIST ,@PARAMS)))
       (INITIALIZE-RESOURCE ',NAME ',CONSTRUCTOR-FUNCTION ',FINDER-FUNCTION
			    ',MATCHER-FUNCTION ',CHECKER-FUNCTION
			    ',PARAMETIZER-FUNCTION ',INITIAL-COPIES ',FREE-LIST-SIZE
			    ',INITIALIZER-FUNCTION ',DEINITIALIZER-FUNCTION)
       ,(IF DOCUMENTATION
	  `(SET-DOCUMENTATION ',NAME 'RESOURCE ,DOCUMENTATION)))))

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFUN INITIALIZE-RESOURCE (NAME CONSTRUCTOR-FUNCTION FINDER-FUNCTION MATCHER-FUNCTION
			    CHECKER-FUNCTION PARAMETIZER-FUNCTION INITIAL-COPIES
			    FREE-LIST-SIZE INITIALIZER-FUNCTION
			    ;; Keep this &OPTIONAL for the time being so old QFASLs work.
			    &OPTIONAL DEINITIALIZER-FUNCTION)
  (OR (SYMBOLP CONSTRUCTOR-FUNCTION)
      (SETQ CONSTRUCTOR-FUNCTION (GET (SECOND CONSTRUCTOR-FUNCTION)
				      (THIRD CONSTRUCTOR-FUNCTION))))
  (OR (SYMBOLP FINDER-FUNCTION)
      (SETQ FINDER-FUNCTION (GET (SECOND FINDER-FUNCTION) (THIRD FINDER-FUNCTION))))
  (OR (SYMBOLP MATCHER-FUNCTION)
      (SETQ MATCHER-FUNCTION (GET (SECOND MATCHER-FUNCTION) (THIRD MATCHER-FUNCTION))))
  (OR (SYMBOLP CHECKER-FUNCTION)
      (SETQ CHECKER-FUNCTION (GET (SECOND CHECKER-FUNCTION) (THIRD CHECKER-FUNCTION))))
  (OR (SYMBOLP INITIALIZER-FUNCTION)
      (SETQ INITIALIZER-FUNCTION (GET (SECOND INITIALIZER-FUNCTION)
					(THIRD INITIALIZER-FUNCTION))))
  (OR (SYMBOLP DEINITIALIZER-FUNCTION)
      (SETQ DEINITIALIZER-FUNCTION (GET (SECOND DEINITIALIZER-FUNCTION)
					(THIRD DEINITIALIZER-FUNCTION))))
  (OR (SYMBOLP PARAMETIZER-FUNCTION)
      (SETQ PARAMETIZER-FUNCTION (GET (SECOND PARAMETIZER-FUNCTION)
				      (THIRD PARAMETIZER-FUNCTION))))
  (AND (RECORD-SOURCE-FILE-NAME NAME 'DEFRESOURCE)
       (LET ((OLD-RESOURCE (GET NAME 'DEFRESOURCE)) RESOURCE)
	 ;; Be careful that there's enough room for all objects in the old resource
	 ;; when replacing it.
	 (AND OLD-RESOURCE (NOT FINDER-FUNCTION)
	      (SETQ FREE-LIST-SIZE (MAX (RESOURCE-N-OBJECTS OLD-RESOURCE)
					FREE-LIST-SIZE)))
	 (AND FINDER-FUNCTION (SETQ FREE-LIST-SIZE 0))
	 (SETQ RESOURCE (MAKE-RESOURCE :NAME NAME
				       :MAKE-ARRAY (:LENGTH (LIST FREE-LIST-SIZE 3)
						    :AREA PERMANENT-STORAGE-AREA)
				       :PARAMETIZER PARAMETIZER-FUNCTION
				       :CONSTRUCTOR CONSTRUCTOR-FUNCTION
				       :FINDER FINDER-FUNCTION
				       :MATCHER MATCHER-FUNCTION
				       :CHECKER CHECKER-FUNCTION
				       :INITIALIZER INITIALIZER-FUNCTION
				       :DEINITIALIZER DEINITIALIZER-FUNCTION))
	 ;; Save any old objects when reloading a DEFRESOURCE
	 (WHEN (AND OLD-RESOURCE (NOT FINDER-FUNCTION))
	   (COPY-ARRAY-CONTENTS OLD-RESOURCE RESOURCE)
	   (SETF (RESOURCE-N-OBJECTS RESOURCE)
		 (RESOURCE-N-OBJECTS OLD-RESOURCE)))
	 (PUTPROP NAME RESOURCE 'DEFRESOURCE)
	 (LOOP FOR OBJECT IN (LOOP REPEAT INITIAL-COPIES COLLECT (ALLOCATE-RESOURCE NAME))
	       DO (DEALLOCATE-RESOURCE NAME OBJECT))))
  (PUSHNEW NAME *ALL-RESOURCES* :TEST #'EQ)
  NAME)

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFUN DEALLOCATE-RESOURCE (RESOURCE-NAME OBJECT &AUX RESOURCE DEINITIALIZER)
  "Return OBJECT to the free pool of resource RESOURCE-NAME.
OBJECT should have been returned by a previous call to ALLOCATE-RESOURCE."
  (CHECK-ARG RESOURCE-NAME (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
	     "the name of a resource")
  (UNLESS (RESOURCE-FINDER RESOURCE)
    (LOOP WITH N-OBJECTS = (RESOURCE-N-OBJECTS RESOURCE)
	  FOR N FROM (1- N-OBJECTS) DOWNTO 0
	  WHEN (EQ (RESOURCE-OBJECT RESOURCE N) OBJECT)
	    ;; Note that this doesn't need any locking.
	    DO (WHEN (SETQ DEINITIALIZER (dont-optimize (RESOURCE-DEINITIALIZER RESOURCE)))
		 (APPLY DEINITIALIZER RESOURCE OBJECT))
	       (RETURN (SETF (RESOURCE-IN-USE-P RESOURCE N) NIL))
	  FINALLY (FERROR NIL "~S is not an object from the ~S resource"
			  OBJECT RESOURCE-NAME))))

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFUN DEALLOCATE-WHOLE-RESOURCE (RESOURCE-NAME &AUX RESOURCE)
  "Return all objects allocated from resource RESOURCE-NAME to the free pool."
  (CHECK-ARG RESOURCE-NAME (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
	     "the name of a resource")
  (UNLESS (RESOURCE-FINDER RESOURCE)
    (LOOP WITH N-OBJECTS = (RESOURCE-N-OBJECTS RESOURCE)
	  FOR N FROM 0 BELOW N-OBJECTS
	  WITH DEINITIALIZER = (dont-optimize (RESOURCE-DEINITIALIZER RESOURCE))
	  WHEN (RESOURCE-IN-USE-P RESOURCE N)
	    DO (IF DEINITIALIZER
		   (FUNCALL DEINITIALIZER RESOURCE (RESOURCE-OBJECT RESOURCE N)))
	       (SETF (RESOURCE-IN-USE-P RESOURCE N) NIL))))

))

; From file TSCROL.LISP OZ:<L.WINDOW> OZ: (73)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TSCROL  "

(DEFMETHOD (FUNCTION-TEXT-SCROLL-WINDOW :PRINT-ITEM) (ITEM IGNORE ITEM-NO)
  (CONDITION-BIND (((SYS:CELL-CONTENTS-ERROR)
		    #'(LAMBDA (COND)
			(VALUES :NEW-VALUE
				(FORMAT NIL "#<~S ~O>"
					(Q-DATA-TYPES (SEND COND :DATA-TYPE))
					(%POINTER (SEND COND :ADDRESS)))))))
    (FUNCALL PRINT-FUNCTION ITEM PRINT-FUNCTION-ARG SELF ITEM-NO)))

))

; From file INSPCT.LISP OZ:<L.WINDOW> OZ: (158)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "


(defun print-pointer (locative stream)
  (format stream "#<~S ~O>"
	  (or (q-data-types (%p-data-type locative)) (%p-data-type locative))
	  (%pointer locative)))

(defmethod (basic-inspect :object-locative) (obj)
  `(((:item1 locative-cell "Contents : " princ)
     ,(if (%p-contents-safe-p obj)
	  `(:item1 locative-contents ,(contents obj))
	(print-pointer obj nil)))
    (" Offset " ,(format nil "~D" (%pointer-difference obj (%find-structure-header obj)))
     " into "
     (:item1 ,(data-type (%find-structure-header obj)) ,(%find-structure-header obj)))
    ("%Pointer : " ,(format nil "~O ~*  (Area ~O, ~S)" (%pointer obj)
			    (%p-pointerp obj) (%area-number obj)
			    (area-name (%area-number obj))))
    ("Cdr-code : " ,(symbol-name (nth (%p-cdr-code obj) sys:q-cdr-codes)))
    ("Data type: " ,(symbol-name (q-data-types (%p-data-type obj))))))
(defun (:property locative-cell set-function) (ignore new-value object)
  (setf (contents object) new-value))
(defprop locative-cell t only-when-modify)


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
	 (SEND STREAM :ITEM1 ITEM 'LEADER-SLOT
	       #'(LAMBDA (ITEM STREAM)
		   (FORMAT STREAM "Leader ~D" ITEM)))
	 (FORMAT STREAM ":~12T ")
	 (if (%p-contents-safe-p (locf (array-leader obj item)))
	     (send stream :item1 (array-leader obj item) :value 'print-item-concisely)
	   (print-pointer (locf (array-leader obj item)) stream)))
	(T
	 (LET ((ITEM (- ITEM LEADER-LENGTH-TO-MENTION))
	       (RANK (ARRAY-RANK OBJ))
	       INDICES)
	   (OR (= RANK 1) (SETQ INDICES (ARRAY-INDICES-FROM-INDEX OBJ ITEM)))
	   (SEND STREAM :ITEM1 (CONS ITEM (IF (= RANK 1) ITEM INDICES)) 'ARRAY-SLOT
		 #'(LAMBDA (DATUM STREAM)
		     (FORMAT STREAM "Elt ~D" (CDR DATUM))))
	   (FORMAT STREAM ":~9T ")
	   (IF (OR (CDR (ASSQ (ARRAY-TYPE OBJ) ARRAY-BITS-PER-ELEMENT))
		   (%P-CONTENTS-SAFE-P (AP-1-FORCE OBJ ITEM)))
	       ;; Deal with data types that are objects, and with numeric arrays.
	       (SEND STREAM
		     :ITEM1 (CLI:AR-1-FORCE OBJ ITEM)	;yes, I really mean cli:
		     :VALUE 'PRINT-ITEM-CONCISELY)
	     ;; Deal with data types that aren't really objects.
	     (print-pointer (locf (cli:ar-1-force obj item)) stream))))))

(defun array-indices-from-index (array index)
  "Given a single INDEX into ARRAY, compute the equivalent set of indices.
The value is a list whose elements could be used with AREF and ARRAY,
and be equivalent to (AR-1-FORCE ARRAY INDEX)."
  (let ((indicies ())
	(index1 index))
    (do ((i (1- (array-rank array)) (1- i)))
	((< i 0) indicies)
      ;; row-major-order!
      (push (\ index1 (array-dimension array i)) indicies)
      (setq index1 (truncate index1 (array-dimension array i))))))
))

; From file PROCES.LISP OZ:<L.SYS2> OZ: (157)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(compiler:make-obsolete process-preset "Use the :PRESET message")
(compiler:make-obsolete process-reset "Use the :RESET message")

))

; From file PROCES.LISP OZ:<L.SYS2> OZ: (157)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
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
  (WITH-STACK-LIST* (INIT-ARGS :NAME NAME INIT-ARGS)
    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))	;don't cons processes in temp areas!
      (INSTANTIATE-FLAVOR (OR (GETF INIT-ARGS :FLAVOR)
			      (AND (GETF INIT-ARGS :SIMPLE-P) 'SIMPLE-PROCESS)
			      'PROCESS)
			  (LOCF INIT-ARGS)
			  T))))

))

; From file QFILE.LISP OZ:<L.NETWORK.CHAOS> OZ: (354)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFFLAVOR QFILE-BINARY-STREAM-MIXIN (CURRENT-BYTE-SIZE) (QFILE-DATA-STREAM-MIXIN))

(DEFMETHOD (QFILE-BINARY-STREAM-MIXIN :AFTER :INIT) (IGNORE)
  (SETQ CURRENT-BYTE-SIZE (GETF SI:PROPERTY-LIST :BYTE-SIZE :DEFAULT)))

(DEFMETHOD (QFILE-BINARY-STREAM-MIXIN :SET-BYTE-SIZE) (NEW-BYTE-SIZE)
  (CHECK-TYPE NEW-BYTE-SIZE (INTEGER 1 16.))
  (SEND SELF :COMMAND T "Set Byte Size"
		       "SET-BYTE-SIZE "
		       (FORMAT NIL "~D ~D" NEW-BYTE-SIZE (SEND SELF :READ-POINTER)))
  (SETQ CURRENT-BYTE-SIZE NEW-BYTE-SIZE)
  NEW-BYTE-SIZE)

(DEFMETHOD (QFILE-BINARY-STREAM-MIXIN :ELEMENT-TYPE) (&AUX SIZE)
  (IF (EQ CURRENT-BYTE-SIZE :DEFAULT)
      'UNSIGNED-BYTE
    `(UNSIGNED-BYTE ,SIZE)))
(DEFFLAVOR QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN ()
	   (QFILE-INPUT-STREAM-MIXIN QFILE-BINARY-STREAM-MIXIN)
  (:REQUIRED-FLAVORS SI:BASIC-BUFFERED-INPUT-STREAM)
  :INITTABLE-INSTANCE-VARIABLES)

(AND (FDEFINEDP '(:METHOD FS::QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN :AFTER :SET-BYTE-SIZE))
     (FUNDEFINE '(:METHOD FS::QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN :AFTER :SET-BYTE-SIZE)))

))


; From file FORMAT.LISP OZ:<L.IO> OZ: (239)
#10R FORMAT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-TAB (PARAMS &AUX (DEST (OR (FIRST PARAMS) 1)) (EXTRA (OR (SECOND PARAMS) 1))
				   (OPS (SEND *STANDARD-OUTPUT* :WHICH-OPERATIONS))
				   INCR-OK)
  (COND ((OR (SETQ INCR-OK (MEMQ :INCREMENT-CURSORPOS OPS))
	     (MEMQ :SET-CURSORPOS OPS))
	 (LET ((FLAVOR (IF COLON-FLAG :PIXEL :CHARACTER)))
	   (MULTIPLE-VALUE-BIND (X Y) (SEND *STANDARD-OUTPUT* :READ-CURSORPOS FLAVOR)
	     (LET ((NEW-X (IF ATSIGN-FLAG
			      (IF ( EXTRA 1)
				  (+ DEST X)
				  (* (CEILING (+ DEST X) EXTRA) EXTRA))
			    (COND ((< X DEST)
				   DEST)
				  ((ZEROP EXTRA)
				   X)
				  (T
				   (+ X EXTRA (- (\ (- X DEST) EXTRA))))))))
	       (COND ((= NEW-X X))
		     (INCR-OK
		      ;; Use :INCREMENT-CURSORPOS preferentially
		      ;; because it will do a **MORE** if we need one.
		      (SEND *STANDARD-OUTPUT* :INCREMENT-CURSORPOS (- NEW-X X) 0 FLAVOR))
		     (T
		      (SEND *STANDARD-OUTPUT* :SET-CURSORPOS NEW-X Y FLAVOR)))))))
	(ATSIGN-FLAG
	 (DOTIMES (I DEST)
	   (SEND *STANDARD-OUTPUT* :TYO #/SPACE)))
	(T (SEND *STANDARD-OUTPUT* :STRING-OUT "  "))))

))

; From file COMTAB.LSP USRD$:[MLY.SAVE] PIG: (1)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN COMMAND-LOOKUP (CHAR COMTAB &OPTIONAL NO-ALIASES NO-INDIRECTION)
  "Return the command in COMTAB has for character CHAR.
NO-ALIASES means do not follow aliases (such as #\c-h-A => #\c-sh-A)
 Instead, return a list of the CHAR-BITS and the CHAR-CODE of the character
 for which it is an alias.
NO-INDIRECTION means only for a command associated with CHAR in COMTAB itself;
 ie the COMTAB-INDIRECT-TO of COMTAB is not followed.
The second value is the comtab the command was found in.
This will be COMTAB or a comtab that COMTAB indirects to."
  (DECLARE (VALUES COMMAND COMTAB))
;character lossage
  (IF (CHARACTERP CHAR) (SETQ CHAR (CHAR-INT CHAR)))
  (DO ((CTB COMTAB (COMTAB-INDIRECT-TO CTB))
       (CH CHAR) KEYBOARD-ARRAY COMMAND)
      ((NULL CTB) NIL)
    (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB)
	  COMMAND (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
			 (OR (CDR (ASSQ (INT-CHAR CH) KEYBOARD-ARRAY))
;character lossage (only for system 99)
			     (CDR (ASSQ CH KEYBOARD-ARRAY))))
			((TV:CHAR-MOUSE-P CH)
			 (IF (ARRAYP (COMTAB-MOUSE-ARRAY CTB))
			     (AREF (COMTAB-MOUSE-ARRAY CTB)
				   (MIN (LDB %%KBD-MOUSE-N-CLICKS CH) 1)
				   (LDB %%KBD-MOUSE-BUTTON CH)
				   (CHAR-BITS CH))))
			(T
			 (AREF KEYBOARD-ARRAY (CHAR-CODE CH) (CHAR-BITS CH)))))
    (COND ((AND (CONSP COMMAND) (NOT NO-ALIASES))
	   (RETURN (COMMAND-LOOKUP (MAKE-CHAR (CADR COMMAND) (CAR COMMAND))
				   CTB NO-ALIASES NO-INDIRECTION)))
	  (COMMAND (RETURN (VALUES COMMAND CTB)))
	  (NO-INDIRECTION (RETURN NIL)))))

(DEFUN COMMAND-STORE (COMMAND CHAR COMTAB &AUX KEYBOARD-ARRAY)
  "Store COMMAND into COMTAB for character CHAR."
;character lossage
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB))
  (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
	 (LET ((ELEMENT (OR (ASSQ CHAR KEYBOARD-ARRAY)
;character lossage (only for system 99)
			    (ASSQ (INT-CHAR CHAR) KEYBOARD-ARRAY))))
	   (IF ELEMENT
	       (SETF (CDR ELEMENT) COMMAND)
	     (PUSH (CONS CHAR COMMAND) (COMTAB-KEYBOARD-ARRAY COMTAB)))))
	((TV:CHAR-MOUSE-P CHAR)
	 (SETF (AREF (COMTAB-MOUSE-ARRAY COMTAB)
		     (MIN (LDB %%KBD-MOUSE-N-CLICKS CHAR) 1)
		     (LDB %%KBD-MOUSE-BUTTON CHAR)
		     (CHAR-BITS CHAR))
	       COMMAND))
	(T
	 (SETF (AREF KEYBOARD-ARRAY
		     (CHAR-CODE CHAR)
		     (CHAR-BITS CHAR))
	       COMMAND))))

))

; From file COME.LISP KANSAS:<L.ZWEI> OZ: (133)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COME  "

(DEFCOM COM-GRIND-EXPRESSION "Grind the evaluation of a form into the buffer.
Reads a form from the mini-buffer, evals it and inserts the result, ground, at
point." ()
  (LET ((TEM (SI:EVAL-ABORT-TRIVIAL-ERRORS
	       (TYPEIN-LINE-MULTI-LINE-READ "Lisp form: (end with )"))))
    (GRIND-INTO-BP (POINT) TEM))
  DIS-TEXT)

))

; From file EVAL.LISP KANSAS:<L.SYS> OZ: (86)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun go (&quote tag &aux tem)
  "Transfer control to the tag TAG in a lexically containing TAGBODY or PROG, etc.
May be used within TAGBODY, PROG, PROG*, DO, DO*, or anything expanding into them.
TAG is not evaluated.
Control transfers instantaneously; the remainder of this statement
of the TAGBODY or PROG is not completed.
See the documentation of TAGBODY for more info."
  (do ((tail *interpreter-frame-environment* (cdr tail)))
      ((atom tail))
    (let ((bindframe (car tail)))
      (and (eq (car bindframe) 'tagbody)
	   (setq tem (memq tag (car (cadr bindframe))))
	   (*throw (cdr (cadr bindframe)) tem))))
  (ferror nil "Unseen ~S tag ~S." 'go tag))

))

; From file COMC.LISP KANSAS:<L.ZWEI> OZ: (204)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
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
  (SETQ GENERIC-PATHNAME (SEND *INTERVAL* :GENERIC-PATHNAME))
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
  (REMPROP (MAKE-REGISTER-NAME #/.) 'POINT)
  (LET ((SI:*ALL-FREE-INTERPRETER-VARIABLE-REFERENCES-SPECIAL* T))
    (MULTIPLE-VALUE-BIND (VARS VALS) (SEND *INTERVAL* :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	(WHEN FS:THIS-IS-A-PATCH-FILE
	  ;; If compiling out of the editor buffer of a patch file,
	  ;; make sure the file itself is marked
	  ;; so that Meta-. will behave right.
	  (PUTPROP GENERIC-PATHNAME T :PATCH-FILE))
	;; Bind off this flag -- our stream is not generating font changes
	;; so READ should not try to remove any.
	(LET ((SI:READ-DISCARD-FONT-CHANGES NIL))
	  (FLET ((DO-IT ()
			(COMPILER:COMPILE-STREAM
			  STREAM
			  GENERIC-PATHNAME
			  NIL			;FASD-FLAG
			  'COMPILE-INTERVAL-PROCESS-FN
			  T			;QC-FILE-LOAD-FLAG
			  NIL			;QC-FILE-IN-CORE-FLAG
			  *PACKAGE*
			  NIL			;FILE-LOCAL-DECLARATIONS
			  NIL			;Unused
			  WHOLE-FILE)))
	    (IF COMPILE-P
		(COMPILER:LOCKING-RESOURCES-NO-QFASL (DO-IT))
	      (DO-IT)))))))
  (OR (NULL GENERIC-PATHNAME)
      (SI:RECORD-FILE-DEFINITIONS GENERIC-PATHNAME SI:FDEFINE-FILE-DEFINITIONS WHOLE-FILE)))

(DEFUN COMPILE-INTERVAL-PREPROCESS-FN (FORM)
  (DECLARE (SPECIAL COMPILE-P DEFVAR-HACK))
  ;; If appropriate, turn a DEFVAR into a SETQ.
  (WHEN (AND DEFVAR-HACK
	     (CONSP FORM)
	     (> (LENGTH FORM) 2)
	     (EQ (CAR FORM) 'DEFVAR)
	     (NEQ (CADDR FORM) ':UNBOUND))
    (UNLESS (SYMBOLP (CADR FORM))
      (FERROR NIL "~S not a recognized form" FORM))
    (PUTPROP (CADR FORM) T 'SPECIAL)		;Declare it
    (WHEN (> (LENGTH FORM) 3)			;in case there is a documentation string.
      (SETF (DOCUMENTATION (SECOND FORM) 'VARIABLE) (SI:EVAL1 (FOURTH FORM)))
      (SETQ FORM (NBUTLAST FORM)))		;remove documentation so that
						;hack into SETQ works properly.
    (SETF (CAR FORM) 'SETQ))			;then always SETQ
  (WHEN (AND (NOT COMPILE-P) (EQ (CAR-SAFE FORM) 'EVAL-WHEN))
    (WHEN (MEMQ 'EVAL (CADR FORM))
      (MAPC #'COMPILE-INTERVAL-PROCESS-FN (CDDR FORM)))
    T))

))

; From file COMA.LISP KANSAS:<L.ZWEI> OZ: (103)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-SET-GOAL-COLUMN "Sets the goal column for Up Real Line and Down Real Line.
This command takes the current horizontal position of the cursor
and makes it the /"goal column/" for the default definitions of C-N and C-P.
They try to move to the goal column in the line they move to.
This command with a numeric argument gets rid of the goal column.

Supply a numeric argument to cancel any goal column setting."
	(KM)
  (REPORT-COLUMN-SETTING "c-N//c-P goal column"
			 (SETQ *PERMANENT-REAL-LINE-GOAL-XPOS* (IF *NUMERIC-ARG-P* NIL
								   (BP-INDENTATION (POINT)))))
  DIS-NONE)

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (355)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFUN QFILE-LISPM-OPEN-OPTIONS-STRING (DIRECTION OPTIONS IF-EXISTS IF-EXISTS-P
					IF-DOES-NOT-EXIST)
  (LET ((*PRINT-BASE* 10.)
	(*NOPOINT T) (*PRINT-RADIX* NIL)
	(*PACKAGE* SI:PKG-USER-PACKAGE)
	(*READTABLE* SI:INITIAL-COMMON-LISP-READTABLE)
	(*PRINT-LENGTH* NIL) (*PRINT-LEVEL* NIL))
    (AND (EQ DIRECTION :OUTPUT)
	 (NULL IF-EXISTS)
	 (SETQ OPTIONS `(:IF-EXISTS :ERROR . ,OPTIONS)))
    (AND (NOT IF-EXISTS-P)
	 (SI:GET-LOCATION-OR-NIL (LOCF OPTIONS) :IF-EXISTS)
	 (PROGN
	   (SETQ OPTIONS (COPYLIST OPTIONS))
	   (REMF OPTIONS :IF-EXISTS)))
    (AND (NULL IF-DOES-NOT-EXIST)
	 (SETQ OPTIONS `(:IF-DOES-NOT-EXIST :ERROR . ,OPTIONS)))
    (PRIN1-TO-STRING OPTIONS)))

))

; From file TVDEFS.LISP KANSAS:<L.WINDOW> OZ: (284)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TVDEFS  "

(DEFMACRO WITH-MOUSE-GRABBED-ON-SHEET ((SHEET) &BODY BODY)
  "Like TV:WITH-MOUSE-GRABBED, but additionally restricts the mouse to moving within SHEET."
  (IF (NULL SHEET) (SETQ SHEET 'SELF))
  `(LET ((.OLD.VALUE. MOUSE-SHEET))
     (UNWIND-PROTECT
       (WITH-MOUSE-GRABBED
	 (MOUSE-SET-SHEET ,SHEET)
	 . ,BODY)
       (MOUSE-SET-SHEET .OLD.VALUE.))))

))

; From file FONT.LISP OZ:<MLY.LL> OZ: (1)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FONT  "

(DEFCOM COM-LIST-FONTS "List the loaded fonts.
With an argument, also lists the font files on the file computer." ()
  (FORMAT T "Loaded fonts: (Mouse a font name to see a sample) ~%")
  (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FONT
	   (LET ((LIST NIL))
	     (DO-LOCAL-SYMBOLS (X 'FONTS)
	       (WHEN (AND (BOUNDP X)
			  (TYPEP (SYMBOL-VALUE X) 'TV:FONT))
		 (PUSH X LIST)))
	     (SETQ LIST (SORT LIST #'STRING-LESSP))))
  (WHEN *NUMERIC-ARG-P*
    (FORMAT T "~&Plus fonts on the file computer:~%")
    (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FONT
	  (LOOP FOR FILE IN (FS:DIRECTORY-LIST (FS:MAKE-PATHNAME :HOST "SYS"
								 :DIRECTORY '("FONTS")
								 :NAME :WILD
								 :TYPE :QFASL
								 :VERSION :NEWEST)
					       :FAST)
		WHEN (CAR FILE)
		COLLECT (INTERN (STRING-UPCASE (SEND (CAR FILE) :NAME)) 'FONTS))))
  DIS-NONE)

))

;>> let me think about this...
;; From file RH.LISP KANSAS:<L.WINDOW> OZ: (162)
;#8R TV#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

;(DEFUN ADD-RH-COMMAND (NAME CHARS)
;  (DOLIST (C CHARS)
;;charcter lossage
;    (IF (CHARACTERP C) (SETQ C (CHAR-INT C)))
;    (LET ((ENTRY (ASSQ C RH-COMMAND-ALIST)))
;	(IF ENTRY
;	    (RPLACD ENTRY NAME)
;	  (SETQ RH-COMMAND-ALIST (NCONC RH-COMMAND-ALIST (LIST (CONS C NAME))))))))

;(DEFRESOURCE RH-INSERT-STRING ()
;  :CONSTRUCTOR (MAKE-STRING #o100))

;(DEFUN-RH RH-YANK-FROM-HISTORY (THING &OPTIONAL
;				   (KILL-PREVIOUS (TYPEP *LAST-COMMAND-TYPE* 'ZWEI:HISTORY)))
;  (WHEN KILL-PREVIOUS
;    (RH-DELETE-STRING (MIN (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
;		      (MAX (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
;		      NIL))
;  (LET ((POS (RH-TYPEIN-POINTER)))
;    (WHEN THING
;      (LET ((STRING (IF (STRINGP THING) THING (ZWEI:STRING-INTERVAL THING))))
;	(RH-INSERT-STRING (STRING-REMOVE-FONTS STRING) 0 NIL T NIL)))
;    (SETQ *RUBOUT-HANDLER-MARK* POS)))

;))


; From file EVAL.LISP KANSAS:<L.SYS> OZ: (87)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun throw (tag &quote &rest value-expression)
  "Throw the values of VALUE-EXPRESSION to TAG.
The innermost catch for TAG will return these values to its caller.
 For backwards compatibility, there may be multiple values-expressions:
 (throw 'foo bar baz) is equivalent to (throw 'foo (values bar baz))
 New code should always use the two-argument form."
  (declare (arglist tag &quote value-expression))
  (throw tag
	 (if (or (null value-expression) (cdr value-expression))
	     (values-list (mapcar #'eval1 value-expression))
	     (eval1 (car value-expression)))))
(deff *throw #'throw)

(defun eval-when (&quote times &rest forms)
  "Process the FORMS only at the specified TIMES.
TIMES is a list which may include COMPILE, EVAL or LOAD.
EVAL means to eval the FORMS if the EVAL-WHEN is processed by the interpreter,
 or to compile and eval them when compiling to core.
LOAD means the compiler when compiling to a file should compile the FORMS
 if appropriate and then make them be executed when the QFASL file is loaded.
COMPILE means the compiler should execute the forms
 at compile time.
/(EVAL LOAD) is equivalent to the normal state of affairs."
  (unless (and (listp times)
	       (loop for time in times always (memq time '(eval load compile))))
    (ferror nil "~S is an invalid specifier for ~S;
it should be a list consisting of ~S, ~S, and//or ~S."
	    times 'eval-when 'eval 'load 'compile))
    (when (memq 'eval times)
      (eval-body forms)))

))

(SETF (DOCUMENTATION 'VECTOR-PUSH 'FUNCTION)
  "Add NEW-ELEMENT as an element at the end of VECTOR, which must have a fill-pointer.
The fill pointer (leader element 0) is the index of the next element to be added,
If the operation is successful (ie if VECTOR is not full) returns the old value
of the fill-pointer and increments the fill-pointer.
Otherwise, returns NIL to indicate that the VECTOR-PUSH did not really happen
and does not change the fill-pointer.
Use VECTOR-PUSH-EXTEND if you want VECTOR to automatically grow if it gets filled.")
(SETF (DOCUMENTATION 'ARRAY-PUSH 'FUNCTION)
  "Same as (VECTOR-PUSH VALUE ARRAY)")
(SETF (DOCUMENTATION 'CATCH 'FUNCTION)
  "Set up a tag TAG that a THROW can throw to.
If a THROW with argument EQ to TAG is executed dynamically within FORMS,
it returns immediately from the CATCH, skipping the rest of the execution of FORMS.
The second argument of THROW is returned from the CATCH.")
(SETF (DOCUMENTATION '*CATCH 'FUNCTION)
  "Obsolete name for CATCH.")
(SETF (DOCUMENTATION '*THROW 'FUNCTION)
  "Obsolete name for THROW")
(SETF (DOCUMENTATION 'INTERNAL-GET-2 'FUNCTION)
  "Internal primitive version of GET when two arguments are supplied.
You should never need to use this, as the compiler optimizes into this as appropriate")
(SETF (DOCUMENTATION 'INTERNAL-GET-2 'FUNCTION)
  "Internal primitive version of CLI:// of exactly two arguments.
You should never need to use this, as the compiler optimizes into this as appropriate")
(SETF (DOCUMENTATION 'FIX 'FUNCTION)
  "Convert NUMBER to an integer, which is less than or equal to NUMBER.
Obsolete function; instead use the first value of TRUNCATE of one argument")
(SETF (DOCUMENTATION 'ARRAY-TO-BIGNUM 'FUNCTION)
  "Converts an array into a bignum.
ARRAY is an ART-Q array, BASE is a fixnum and SIGN the sign bit (0 or 1).
ARRAY is interpreted as a bignum expressed in base BASE is and converted
into that bignum with sign SIGN.  This is the inverse of BIGNUM-TO-ARRAY.")
(SETF (DOCUMENTATION 'BIGNUM-TO-ARRAY 'FUNCTION)
  "Converts a bignum into an array.
BIGNUM is expressed in base BASE and stuffed into an appropriate ART-Q
array.  The sign of the bignum is ignored.")
(SETF (DOCUMENTATION 'AREF 'FUNCTION)
  "Return the contents of the element of ARRAY specified by SUBSCRIPTS.
Returns a fixnum (not a character object) when applied to a string.
To get characters out of strings, use CLI:AREF or CHAR")
(SETF (DOCUMENTATION 'AR-1-FORCE 'FUNCTION)
  "Return contents of element INDEX of ARRAY, treated as one-dimensional.
ARRAY is treated as one-dimensional in that it is indexed with
a single subscript regardless of its rank.
See also CLI:AR-1-FORCE, which is the same except that if returns character
objects rather than fixnums when applied to a string")
; From file PATHNM.LISP#> QL.IO.FILE; LAM3:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(DEFUN DEFAULT-PATHNAME (&OPTIONAL DEFAULTS HOST DEFAULT-TYPE DEFAULT-VERSION INTERNAL-P
			 &AUX ELEM PATHNAME HOST-TO-USE CTYPE OTYPE)
  (AND HOST (SETQ HOST (GET-PATHNAME-HOST HOST)))
  (WHEN (OR (NULL DEFAULTS) (EQUAL DEFAULTS '(NIL)) (EQUAL DEFAULTS '((NIL))))
    (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((AND DEFAULTS (ATOM DEFAULTS))
	 (SETQ PATHNAME (PARSE-PATHNAME DEFAULTS)))
	(T
	 (SETQ ELEM (COND ((NOT *DEFAULTS-ARE-PER-HOST*) (ASSQ NIL DEFAULTS))
			  (HOST (ASSQ HOST DEFAULTS))
			  (T (DOLIST (DEFAULT DEFAULTS)	;Last host mentioned
			       (AND (CDR DEFAULT) (RETURN DEFAULT))))))
	 ;; If none better found, take the one for the login machine
	 (OR (CDR ELEM)
	     (SETQ ELEM (OR (ASSQ USER-LOGIN-MACHINE DEFAULTS)
			    (NCONS USER-LOGIN-MACHINE))))
	 ;; If there isn't one already, build a pathname from the host of this one
	 (SETQ HOST-TO-USE (OR HOST (CAR ELEM) (PATHNAME-HOST (CDR ELEM))))
	 (COND ((SETQ PATHNAME (CDR ELEM)))
	       (INTERNAL-P
		(SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST-TO-USE NIL NIL NIL NIL NIL)))
	       (T
		(SETQ PATHNAME (SEND (USER-HOMEDIR HOST-TO-USE) :NEW-PATHNAME
				     :NAME "FOO" :TYPE *NAME-SPECIFIED-DEFAULT-TYPE*
				     :VERSION :NEWEST))
		(SETF (CDR ELEM) PATHNAME)))))
  ;; If default-type or default-version was given, or the host has changed,
  ;; merge those in.
  (AND (OR (AND HOST (NEQ HOST (PATHNAME-HOST PATHNAME))) DEFAULT-TYPE DEFAULT-VERSION)
       (SETQ HOST (OR HOST (PATHNAME-HOST PATHNAME)))
       (IF INTERNAL-P
	   (AND HOST (SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST NIL NIL NIL NIL NIL)))
	 (SETF (VALUES CTYPE OTYPE) (SEND PATHNAME :CANONICAL-TYPE))
	 (SETQ PATHNAME (SEND (MAKE-PATHNAME :HOST HOST :DEFAULTS NIL)
			      :NEW-PATHNAME
			      :DIRECTORY (PATHNAME-DIRECTORY PATHNAME)
			      :DEVICE (PATHNAME-DEVICE PATHNAME)
			      :HOST (OR HOST (PATHNAME-HOST PATHNAME))
			      :NAME (PATHNAME-NAME PATHNAME)
			      :CANONICAL-TYPE CTYPE
			      :ORIGINAL-TYPE OTYPE
			      :VERSION (OR DEFAULT-VERSION (PATHNAME-VERSION PATHNAME))
			      ))))
  PATHNAME)

))


; From file LMMAC.LISP KANSAS:<L.SYS2> OZ: (380)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CONDITION-CASE-IF (&ENVIRONMENT ENV COND-FORM VARIABLES BODY-FORM &REST CLAUSES)
  "Like CONDITION-CASE, but establishes condition handlers only if COND-FORM evaluates non-NIL.
Refer to the documentation of CONDITION-CASE for more information."
  ;; We don't use &BODY in the real arglist to avoid overriding
  ;; the special form of indentation on *INITIAL-LISP-INDENT-OFFSET-ALIST*
  (DECLARE (ARGLIST COND-FORM VARIABLES BODY-FORM &BODY CLAUSES))
  (MULTIPLE-VALUE-BIND (REALCLAUSES DECLS)
      (EXTRACT-DECLARATIONS CLAUSES NIL NIL ENV)
    (LET* ((ALL-CONDITIONS
	     (MAPCAN #'(LAMBDA (CLAUSE)
			 (MACRO-TYPE-CHECK-WARNING 'CONDITION-CASE-IF (CAR CLAUSE))
			 (IF (EQ (CAR CLAUSE) ':NO-ERROR) NIL
			   (IF (CONSP (CAR CLAUSE)) (CAR CLAUSE)
			     (LIST (CAR CLAUSE)))))
		     REALCLAUSES))
	   (VAR (OR (CAR VARIABLES) (GENSYM)))
	   (NO-ERROR-CLAUSE (ASSQ ':NO-ERROR REALCLAUSES))
	   (TAG (GENSYM)))
      (IF (NULL (CDR ALL-CONDITIONS))
	  (SETQ ALL-CONDITIONS (CAR ALL-CONDITIONS)))
      (IF NO-ERROR-CLAUSE
	  `(LET ,VARIABLES
	     (DECLARE . ,DECLS)
	     (CATCH-CONTINUATION-IF T ',TAG
		 #'(LAMBDA (,VAR)
		     (DECLARE . ,DECLS)
		     (SELECT-MEMQ (SEND ,VAR :CONDITION-NAMES)
		       . ,(REMQ NO-ERROR-CLAUSE REALCLAUSES)))
		 #'(LAMBDA () . ,(CDR NO-ERROR-CLAUSE))
	       (CONDITION-BIND-IF ,COND-FORM ((,ALL-CONDITIONS 'CONDITION-CASE-THROW ',TAG))
		 (MULTIPLE-VALUE-SETQ ,VARIABLES ,BODY-FORM))))
	`(CATCH-CONTINUATION-IF T ',TAG
	     #'(LAMBDA (,VAR)
		 (DECLARE . ,DECLS)
		 (SELECT-MEMQ (SEND ,VAR :CONDITION-NAMES)
		   . ,REALCLAUSES))
	     ()
	   (CONDITION-BIND-IF ,COND-FORM ((,ALL-CONDITIONS 'CONDITION-CASE-THROW ',TAG))
	     ,BODY-FORM))))))

))

; From file LMMAC.LISP KANSAS:<L.SYS2> OZ: (380)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CONDITION-CALL-IF (&ENVIRONMENT ENV COND-FORM VARIABLES BODY-FORM &REST CLAUSES)
  "Like CONDITION-CALL, but establishes the handlers only if COND-FORM evaluates non-NIL.
See the documentation of CONDITION-CALL for more information."
  ;; We don't use &BODY in the real arglist to avoid overriding
  ;; the special form of indentation on *INITIAL-LISP-INDENT-OFFSET-ALIST*
  (DECLARE (ARGLIST COND-FORM (VARIABLE) BODY-FORM &BODY CLAUSES))
  (MULTIPLE-VALUE-BIND (REALCLAUSES DECLS)
      (EXTRACT-DECLARATIONS CLAUSES NIL NIL ENV)
    (LET* ((ORDINARY-CLAUSES (SUBSET #'(LAMBDA (CLAUSE) (NEQ (CAR CLAUSE) ':NO-ERROR))
				     REALCLAUSES))
	   (NO-ERROR-CLAUSE (ASSQ ':NO-ERROR REALCLAUSES))
	   (PREDICATES (MAPCAR #'CAR ORDINARY-CLAUSES))
	   (VAR (OR (CAR VARIABLES) (GENSYM)))
	   (TAG (GENSYM))
	   (HANDLER `#'(LAMBDA (,VAR &REST IGNORE)
			 (DECLARE . ,DECLS)
			 (IF (OR . ,PREDICATES)
			     (THROW ',TAG ,VAR)))))
      `(CATCH-CONTINUATION-IF T ',TAG
	   #'(LAMBDA (,VAR)
	       (DECLARE . ,DECLS)
	       (COND . ,ORDINARY-CLAUSES))
	   ,(IF NO-ERROR-CLAUSE
		`#'(LAMBDA ,VARIABLES
		     (DECLARE . ,DECLS)
		     . ,(CDR NO-ERROR-CLAUSE)))
       (CONDITION-BIND-IF ,COND-FORM ((NIL ,HANDLER)) ,BODY-FORM)))))

))

; From file LMMAC.LISP KANSAS:<L.SYS2> OZ: (380)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO WITH-LOCK ((LOCATOR . OPTIONS) &BODY BODY &AUX NORECURSIVE NOERROR)
  "Execute the BODY with a lock locked.
LOCATOR is an expression whose value is the lock status;
it should be suitable for use inside LOCF.
OPTIONS include :NORECURSIVE, do not allow locking a lock already locked by this process."
  ;; Ignore the old :NOERROR option -- it's always that way now.
  (KEYWORD-EXTRACT OPTIONS O () (NORECURSIVE NOERROR) (OTHERWISE NIL))
  `(LET* ((.POINTER. (LOCF ,LOCATOR))
	  (.ALREADY.MINE. (EQ (CAR .POINTER.) CURRENT-PROCESS)))
     (IF (CONSP .POINTER.)
	 (SETQ .POINTER. (CDR-LOCATION-FORCE .POINTER.)))
     (UNWIND-PROTECT
	 (PROGN (IF .ALREADY.MINE.
		    ,(IF NORECURSIVE `(FERROR NIL "Attempt to lock ~S recursively."
					      ',LOCATOR))
		  ;; Redundant, but saves time if not locked.
		  (OR (%STORE-CONDITIONAL .POINTER. NIL CURRENT-PROCESS)
		      (PROCESS-LOCK .POINTER.)))
		. ,BODY)
       (UNLESS .ALREADY.MINE.
	 (%STORE-CONDITIONAL .POINTER. CURRENT-PROCESS NIL)))))

))

;; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (533)
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

;(DEFUN GET-PATHNAME-HOST (HOST-NAME &OPTIONAL NO-ERROR-P
;			  (UNKNOWN-OK (VARIABLE-BOUNDP CHAOS:MY-ADDRESS)))
;  "Parse a host for use in a pathname.
;HOST-NAME can be a host object or a host name.
;If NO-ERROR-P is non-NIL, we return NIL if given an undefined host name."
;  (SETQ HOST-NAME (STRING HOST-NAME))
;  (IF (OR (STRING-EQUAL HOST-NAME "LM")
;	  (STRING-EQUAL HOST-NAME "LOCAL"))
;      SI:LOCAL-HOST
;    (FLET ((GET-HOST-FROM-LIST (LIST)
;	      (IF (MEMQ HOST-NAME LIST)
;		  HOST-NAME
;		(LET ((HOST NIL))
;		  (DOLIST (X LIST HOST)
;		    ;; We prefer an exact match:
;		    ;;  This is a hack for LMFILE to share LM27 (FC/FS)
;		    (WHEN (SEND X :PATHNAME-HOST-NAMEP HOST-NAME)
;		      (IF (STRING-EQUAL HOST-NAME (SEND X :NAME-AS-FILE-COMPUTER))
;			  (RETURN X)
;			(SETQ HOST X)))))))) ; Non-exact match
;      ;; And said MLY unto the Lusers ``Let logical hosts shadow physical hosts.''
;      (COND ((GET-HOST-FROM-LIST *LOGICAL-PATHNAME-HOST-LIST*))
;	    ((GET-HOST-FROM-LIST *PATHNAME-HOST-LIST*))
;	    ;; Don't let SI:PARSE-HOST check for an unknown host here when making SYS.
;	    ((LET ((HOST (SI:PARSE-HOST HOST-NAME T UNKNOWN-OK)))
;	       (WHEN (AND HOST (SEND HOST :SEND-IF-HANDLES :FILE-HOST-P))
;		 ;; Adjust the file system type, maybe.  Usually LispMs to :LMFS
;		 ;; This code is simpler than using CLI:RASSOC and CLI:MEMBER,
;		 ;; which are not loaded yet anyway.
;		 (DOLIST (ELT (GET-SITE-OPTION :SPECIAL-FILE-HOSTS))
;		   (WHEN (DOLIST (HNAME (SEND HOST :HOST-NAMES))
;			   (WHEN (MEM #'STRING-EQUAL HNAME (CDR ELT))
;			     (RETURN T)))
;		     (LET ((FILE-SYSTEM-TYPE (CAR ELT)))
;		       (SETF (GET HOST :FILE-SYSTEM-TYPE) FILE-SYSTEM-TYPE))))
;		 (PUSHNEW HOST *PATHNAME-HOST-LIST* :TEST #'EQ)
;		 HOST)))
;	    (NO-ERROR-P NIL)
;	    (T (FERROR 'UNKNOWN-PATHNAME-HOST
;		       "~S is not the name of a known file host" HOST-NAME))))))

;))

; From file HOST.LISP KANSAS:<L.NETWORK> OZ: (118)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; HOST  "

(DEFMETHOD (HOST :PATHNAME-HOST-NAMEP) (NAME)
  (OR (AND (EQ SELF SI:LOCAL-HOST)
	   (OR (STRING-EQUAL NAME "LOCAL") (STRING-EQUAL NAME "LM"))
	   self)
      ;; CLI:MEMBER is not loaded yet
      (CAR (MEM #'STRING-EQUAL NAME (HOST-NAME-LIST ALIST-ELEM)))))

))

(DEFMETHOD (fs::lispm-HOST :combined :PATHNAME-HOST-NAMEP) (NAME)
  (OR (AND (EQ SELF SI:LOCAL-HOST)
	   (OR (STRING-EQUAL NAME "LOCAL") (STRING-EQUAL NAME "LM"))
	   self)
      ;; CLI:MEMBER is not loaded yet
      (CAR (MEM #'STRING-EQUAL NAME (si::HOST-NAME-LIST si::ALIST-ELEM)))))

; From file READ.LISP KANSAS:<L.IO> OZ: (436)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#/`-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (PROG ((FLAG NIL)
	 (THING NIL)
	 (**BACKQUOTE-REPEAT-VARIABLE-LISTS** (CONS NIL **BACKQUOTE-REPEAT-VARIABLE-LISTS**)))
	(MULTIPLE-VALUE (FLAG THING) (BACKQUOTIFY (INTERNAL-READ STREAM T NIL T)))
	(AND (EQ FLAG **BACKQUOTE-/,/@-FLAG**)
	     (RETURN
	       (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
		       " /",@/" right after a /"`/": `,@~S." THING)))
	(AND (EQ FLAG **BACKQUOTE-/,/.-FLAG**)
	     (RETURN
	       (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
		       " /",./" right after a /"`/": `,.~S." THING)))
	(RETURN (CONS 'PROGN
		      (NREVERSE
			(EVAL1 `(LET (ACCUM)
				  (DO ,(CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**)
				      ((NULL ,(CAAAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**))
				       ACCUM)
				    (PUSH ,(BACKQUOTIFY-1 FLAG THING) ACCUM)))))))))

))

; From file READ.LISP KANSAS:<L.IO> OZ: (436)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#/,-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (IF FILE-IN-COLD-LOAD
      (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
	      "#, cannot be used in files in the cold load."))
  (IF (AND (BOUNDP 'COMPILER::QC-FILE-READ-IN-PROGRESS) COMPILER::QC-FILE-READ-IN-PROGRESS)
      (CONS-IN-AREA COMPILER::EVAL-AT-LOAD-TIME-MARKER (INTERNAL-READ STREAM T NIL T)
		    READ-AREA)
    (VALUES (IF *READ-SUPPRESS*
		(PROGN (INTERNAL-READ STREAM T NIL T) NIL)
	      (EVAL1 (INTERNAL-READ STREAM T NIL T))))))

))

; From file READ.LISP KANSAS:<L.IO> OZ: (436)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#.-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (VALUES (IF *READ-SUPPRESS*
	      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
	    (EVAL1 (INTERNAL-READ STREAM T NIL T)))))

))

; From file PATCH.LISP KANSAS:<L.SYS2> OZ: (165)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN WRITE-RESPONSIBILITY-COMMENT (STREAM &AUX (TIME:*DEFAULT-DATE-PRINT-MODE* :DD-MMM-YY))
  (FORMAT STREAM "~&;;; Written ~\DATIME\ by ~A,
;;; while running on ~A from band ~C
;;; with ~A.~2%"
	  USER-ID LOCAL-PRETTY-HOST-NAME
	  (LDB #o2010 CURRENT-LOADED-BAND) (SYSTEM-VERSION-INFO)))

))

; From file PATHST.LISP KANSAS:<L.IO.FILE> OZ: (178)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHST  "

(defun make-logical-pathname-host (host-name &key (warn-about-redefinition t))
  "Defines HOST-NAME to be the name of a logical host.
If this conflicts the name or nickname of any physical host,
then and error is signalled, and the new logical host may be allowed to
override that name of the physical host.
This function loads the file SYS: SITE; host-name TRANSLATIONS, which should contain
a call to FS:SET-LOGICAL-PATHNAME-HOST to set up the translations for the host."
  (setq host-name (string-upcase (string host-name)))
  (let ((old (get-pathname-host host-name t))
	new file-id loaded-id)
    (catch-error-restart ((fs:remote-network-error fs:file-not-found)
			  "Give up loading logical pathname translations for ~A" host-name)
      (when (typep old 'logical-host)
	(setq loaded-id (send old :get 'make-logical-pathname-host))
	;; if previously defined by hand, don't load translations and clobber it
	(cond ((not loaded-id)
	       (return-from make-logical-pathname-host old))
	      (warn-about-redefinition
	       (format *error-output* "~&Warning: The logical host ~A is being redefined"
		       old))))
      ;; no need to give error if redefining physical host, as set-logical-pathname-host errs
      (let ((pathname (make-pathname :host "SYS"
				     :device :unspecific
				     :directory '("SITE")
				     :name host-name
				     :canonical-type :logical-pathname-translations
				     :version :newest)))
	(setq file-id (with-open-file (stream pathname :direction :probe
					      	       :if-does-not-exist :error)
			(send stream :info)))
	(unless (equal loaded-id file-id)
	  (load pathname :verbose nil :package (symbol-package 'foo)))))
    (cond ((typep (setq new (get-pathname-host host-name nil)) 'logical-host)
	   (send new :set :get 'make-logical-pathname-host file-id)
	   new)
	  (t (format *error-output*
		     "~&Warning: The logical host ~S was not defined by ~S."
		     host-name 'make-logical-pathname-host)
	     nil))))

))

