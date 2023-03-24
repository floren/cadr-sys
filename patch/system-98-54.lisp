;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 5/13/84 13:36:57 by kab,
;;; Reason: define SI:COMBINED-METHOD-DOCUMENTATION
;;;   fancy formatting of documentation for combined methods
;;;   patch DOCUMENTATION to use it when appropriate
;;; while running on Lisp Machine Three from band 2
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.53, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, KAB-Auxiliary 1.0, 3D-Model 3.3, microcode 309, ZM MIT.



; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DOCUMENTATION (SYMBOL &OPTIONAL (DOC-TYPE 'FUNCTION))
  "Try to return the documentation string for SYMBOL, else return NIL.
Standard values of DOC-TYPE are: FUNCTION, VARIABLE, TYPE, STRUCTURE and SETF,
but you can put on and retrieve documentation for any DOC-TYPE.
Documentation strings are installed by SETFing a call to DOCUMENTATION."
  (COND ((AND (EQ DOC-TYPE 'VALUE)
	      (GET SYMBOL :DOCUMENTATION)))
	((AND (SYMBOLP SYMBOL)
	      (LET ((DOC-PROP (GET SYMBOL 'DOCUMENTATION-PROPERTY)))
		(GET (LOCF DOC-PROP) DOC-TYPE))))
	((AND (EQ DOC-TYPE 'TYPE)
	      (GET SYMBOL 'TYPE-EXPANDER)
	      (DOCUMENTATION (GET SYMBOL 'TYPE-EXPANDER) 'FUNCTION)))
	((SYMBOLP SYMBOL)
	 (AND (FBOUNDP SYMBOL)
	      (DOCUMENTATION (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC SYMBOL)))))
	((CONSP SYMBOL)
	 (IF (FUNCTIONP SYMBOL T)
	     (IF (EQ (CAR SYMBOL) 'MACRO)
		 (DOCUMENTATION (CDR SYMBOL))
	       (MULTIPLE-VALUE-BIND (NIL NIL DOC)
		   (EXTRACT-DECLARATIONS (CDR (LAMBDA-EXP-ARGS-AND-BODY SYMBOL)) NIL T)
		 DOC))
	   (AND (FDEFINEDP SYMBOL)
		(DOCUMENTATION (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC SYMBOL))))))
	((= (%DATA-TYPE SYMBOL) DTP-FEF-POINTER)
	 (IF (ASSQ 'COMBINED-METHOD-DERIVATION (DEBUGGING-INFO SYMBOL))
	     ;; its a fef for a combined method, so do special handling
	     (COMBINED-METHOD-DOCUMENTATION SYMBOL)
	   (CADR (ASSQ :DOCUMENTATION (DEBUGGING-INFO SYMBOL)))))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defun COMBINED-METHOD-DOCUMENTATION (Method &Optional Stream &Key (Fresh-Line t))
  "Returns a string which documents Method.  Method must be a :combined method.
If Stream is a stream, then instead prints documentation to the stream.
Fresh-Line is only used if Stream is a stream.
This documentation string will have the format:

  method combination is <keyword for type>, order is <keyword for order>

  :wrapper methods
    flavor component-flavor-1, arglist: args
      doc string
    flavor component-flavor-2, arglist: args
      doc string
    ...

    :around methods
      flavor component-flavor-1, arglist: args
        doc string
      flavor component-flavor-2, arglist: args
        doc string
      ...

      etc. { the rest depends on which type of method combination is used.
             see SI:DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER, SI:COMBINED-DOC-STRING, and
             (:property SI:COMBINED-METHOD-DOCUMENTATION <method combination keyword>) }

the ordering for component flavors is the order in which the components are combined to form
the combined method.  Note that the following orders always hold:
   :wrappers         :base-flavor-last
   :around methods   :base-flavor-last
   :before methods   :base-flavor-last
   :after methods    :base-flavor-first

A handler for the method-combination type used by the combined method must exist
/(see SI:DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER to define new ones)."
  (let* ((Temp (cadr (assq 'combined-method-derivation (debugging-info Method))))
	 (Type (or (cadr Temp) :daemon))	; :daemon is default type
	 (Handler (get 'combined-method-documentation Type))
	 (Order (if (eq Type :pass-on) (caaddr Temp) (caddr Temp)))
	 (Derivation (cdddr Temp))
	 (Flavors (flavor-depends-on-all (get (FLAVOR-OF-METHOD Method) 'flavor)))
	 (Indent 0)
	 String St)
    (when (null Handler)
      (ferror nil "No combined method doc handler for ~S method combination." Type))
    (if (or (streamp Stream) (eq Stream t)) (setq String Stream)
      ;; only need string if no stream
      (setq String (make-string (* 100 (length Derivation)) :fill-pointer 0))
      (setq Stream nil))
    ;; put in header
    (setq St (format Stream "~@[~&~*~]method combination is ~S~@[, order is ~S~]"
		     (and Stream Fresh-Line) Type (and (neq Type :daemon) Order)))
    (unless Stream
      (string-nconc String St))
    ;; do :wrapper and :around methods
    (COMBINED-DOC-STRING String (assq :wrapper Derivation) Flavors Indent Order)
    (when (assq :wrapper Derivation) (incf Indent 2))
    (COMBINED-DOC-STRING String (assq :around Derivation) Flavors Indent Order)
    (when (assq :around Derivation) (incf Indent 2))
    ;; call the handler appropriate to the type of method combination
    (funcall Handler String Derivation Flavors Indent Order)
    (when (and (stringp String) (plusp (length String))) String)))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defun COMBINED-DOC-STRING (String Components Flavors Indent &Optional Order)
  "Add the documentation for a component method type to String.
The type of method is in the CAR of Components.  The component methods are the CDR of
Components.  Flavors is the list of component flavors, in the order of flavor combination.
Order is taken from the method combination specifier.  The result will look roughly like:

     <indentation>
    <method type> methods, order is <order of method combination>
      flavor <flavor of first component method>, arglist: <arglist of component method>
          <documentation string for component method>
      flavor <flavor of second component method>, arglist: <arglist of component method>
          <documentation string for component method>
      ...

:CASE methods vary slightly in that they include the suboperation at the beginning of the line
giving the flavor and arglist.  If Order is nil then the header will not include mention of
the order of combination.

String can be either a string (which must have a fill pointer), in which case the modified
string is returned, or it can be a stream."
  (let ((Type (pop Components))
	(Stream (and (or (streamp String) (eq String t)) String))
	Methods St)
    (if (null Components) String
      (pkg-bind 'user				; force printing of package prefix's
	(setq St (format Stream "~2%~V,0T~:[~*:PRIMARY~;~S~] method~P~@[, order is ~S~]"
			 Indent Type Type (length Components) Order))
	(unless Stream
	  (string-nconc String St))		 
	(dolist (Flavor Flavors String)
	  (setq Methods (subset `(lambda (M) (eq ',Flavor (flavor-of-method M))) Components))
	  (dolist (Method Methods)
	    (setq St (format Stream "~%~V,0T~@[~S suboperation, ~]flavor ~S, arglist: ~S"
			     (+ Indent 2)
			     (and (eq Type :case) (car (last (NAME-OF-METHOD Method))))
			     Flavor
			     (do ((Args (arglist Method) (cdr Args)))
				 ((not (memq (car Args) '(.operation. .suboperation.)))
				  Args))))
	    (unless Stream
	      (string-nconc String St))			
	    (let ((Doc (documentation Method)))
	      (when Doc
		(setq St (format Stream "~%~V,0T~~A~" (+ Indent 4) Doc))
		(unless Stream
		  (string-nconc String St))))))))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defun FLAVOR-OF-METHOD (Method)
  "Returns the symbol which is the flavor for Method.
Method may be either a method spec (ie. (:method flavor type operation suboperation)) or an
FEF for a method.  Error if Method is not a defined method."
  (if (= (%data-type Method) dtp-fef-pointer)
      ;; get name of method from fef and extract flavor
      (cadr (NAME-OF-METHOD Method))
    (if (consp (setq Method (fdefinition (unencapsulate-function-spec Method))))
	(if (eq (car Method) 'macro) (FLAVOR-OF-METHOD (cdr Method))
	  (cadr (assq :self-flavor		; named-lambda
		      (nth-value 1 (extract-declarations
				     (cdr (lambda-exp-args-and-body Method)) nil t)))))
      (FLAVOR-OF-METHOD Method))))		; fef returned by fdefinition, try again

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defun NAME-OF-METHOD (Method)
  "Returns the list which is the function spec for the method.
Method may be either a method spec (ie. (:method flavor type operation suboperation)) or an
FEF for a method.  Error if Method is not a defined method."
  (if (= (%data-type Method) dtp-fef-pointer)
      (%p-contents-offset Method %fefhi-fctn-name)
    (if (consp (setq Method (fdefinition (unencapsulate-function-spec Method))))
	(if (eq (car Method) 'macro) (NAME-OF-METHOD (cdr Method))
	  (caadr Method))			; named-lambda
      (NAME-OF-METHOD Method))))		; fef, try again

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defmacro DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER ((Type) &Body Body)
  "Expands to define (:property si:combined-method-documentation <combination type>).
Body can reference the following variables (they are the args to the handler):
 String      the documentation string to be modified (use STRING-NCONC).
 Derivation  the list of component methods for this method.  it is an alist, with each element
             being a method type followed by the component methods of that type.
 Flavors     the list of component flavors, in the order of flavor combination.
 Indent      number of spaces to indent from the left, used for formatting.
 Order       from the method-combination declaration, a keyword.
Typically, Body consists of some number of calls to SI:COMBINED-DOC-STRING interspersed with
adjustments to the indentation."
  (declare (arglist (Method-Combination-Type) &Body Body))
  (setq Type (intern (string-upcase Type) "KEYWORD"))
  (multiple-value-bind (Body Declarations Documentation)
      (extract-declarations Body nil t)
    `(defun (COMBINED-METHOD-DOCUMENTATION ,Type) (String Derivation Flavors Indent Order)
       ,(format nil "Add documentation to string according to ~S method combination.~%~A"
		Type Documentation)
       . ,(append (mapcar #'list (circular-list 'declare) Declarations)
		  Body))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:DAEMON)
  "Format is --
  :BEFORE methods
    :PRIMARY method
  :AFTER methods"
  Order						; ignored arg
  (COMBINED-DOC-STRING String (assq :before Derivation) Flavors Indent)
  (when (or (assq :before Derivation) (assq :after Derivation)) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq nil Derivation) Flavors Indent)
  (decf Indent 2)
  (COMBINED-DOC-STRING String (assq :after Derivation) (reverse Flavors) Indent))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:DAEMON-WITH-OR)
  "Format is --
  :BEFORE methods
    :OR methods
      :PRIMARY method
  :AFTER methods"
  (COMBINED-DOC-STRING String (assq :before Derivation) Flavors Indent)
  (when (or (assq :before Derivation) (assq :after Derivation)) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq :or Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order)
  (when (assq :or Derivation) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq nil Derivation) Flavors Indent)
  (when (assq :or Derivation) (decf Indent 2))
  (decf Indent 2)
  (COMBINED-DOC-STRING String (assq :after Derivation) (reverse Flavors) Indent))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:DAEMON-WITH-AND)
  "Format is --
  :BEFORE methods
    :AND methods
      :PRIMARY method
  :AFTER methods"
  (COMBINED-DOC-STRING String (assq :before Derivation) Flavors Indent)
  (when (or (assq :before Derivation) (assq :after Derivation)) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq :and Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order)
  (when (assq :and Derivation) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq nil Derivation) Flavors Indent)
  (when (assq :and Derivation) (decf Indent 2))
  (decf Indent 2)
  (COMBINED-DOC-STRING String (assq :after Derivation) (reverse Flavors) Indent))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:DAEMON-WITH-OVERRIDE)
  "Format is --
  :OVERRIDE methods
    :BEFORE methods
      :PRIMARY method
    :AFTER methods"
  (COMBINED-DOC-STRING String (assq :override Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order)
  (when (assq :override Derivation) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq :before Derivation) Flavors Indent)
  (when (or (assq :before Derivation) (assq :after Derivation)) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq nil Derivation) Flavors Indent)
  (decf Indent 2)
  (COMBINED-DOC-STRING String (assq :after Derivation) (reverse Flavors) Indent))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:CASE)
  "Format is --
  :OR methods
    :CASE methods
      :PRIMARY method"
  (COMBINED-DOC-STRING String (assq :or Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order)
  (when (assq :or Derivation) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq :case Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order)
  (when (assq :case Derivation) (incf Indent 2))
  (COMBINED-DOC-STRING String (assq nil Derivation) Flavors Indent))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:PROGN)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:OR)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:AND)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:APPEND)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:NCONC)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:LIST)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:INVERSE-LIST)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:PASS-ON)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:MAX)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:MIN)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (:+)
  (COMBINED-DOC-STRING String (assq nil Derivation)
		       (if (eq Order :base-flavor-first) (reverse Flavors) Flavors)
		       Indent Order))

))

