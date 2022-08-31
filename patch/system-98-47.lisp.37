;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Written 13-Apr-84 11:07:02 by Mly,
;;; Reason: SPECIAL-FORM-P uses real arglist
;;; Printing characters which have names bug
;;; MAKE-OBSOLETE GLOBAL:CATCH and GLOBAL:THROW
;;; :SET methods for :PUTHASH and :GET
;;; Make select-method definition update which-operations dynamically
;;; SI:PROPERTY-LIST-HANDLER does useful things for named-structure-invoke handlers
;;; EXPORTing inherited symbols bug
;;; Package name-conflict really does checking only for external symbols of USEd packages 
;;; Trying to edit "Buffer header" and "Things deleted" sections in zmacs
;;; :COLD initializations will not run after warm boot even if never previously run
;;; Try to use short system names if necessary on vms and its
;;; COMPILER:DEFOPTIMIZER, COMPILER:DEFCOMPILER-SYNONYM
;;; Make optimization happen only once per form
;;; Perform optimizations on args first if form is not special form or macro
;;; Many random optimization improvements
;;; Make BYTE open-code
;;; Compiler arg-checking looks for bogus keywords in calls to &key functions
;;; EQL and SYS:MEMBER-EQL optimize into EQ and MEMQ (nearly always for SELECTQ)
;;; MAKE-ARRAY optimizer understands unquoted keywords, :ELEMENT-TYPE
;;; Make sure that CLI exports all its symbols
;;; SELECTQ uses EQL as test
;;; Globalize "NUMBER"
;;; Make IF a special form -- prevent macro-function  recursivness
;;; (subst nil nil foo) and (append foo nil) compiler style-checking fascism
;;;   -- tell me if this is too obnoxious.
;;; while running on Lisp Machine Eighteen from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.44, CADR 3.6, Inconsistent (unreleased patches loaded) ZMail 53.15, MIT-Specific 22.0, microcode 309, ZM MIT.


; From file QCDEFS.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(defmacro add-optimizer (target-function optimizer-name &rest optimized-into)
  "Add OPTIMIZER-NAME to TARGET-FUNCTION's list of optimizers.
Also records that TARGET-FUNCTION sometimes gets optimized into
the functions in OPTIMIZED-INTO, for the sake of WHO-CALLS.
Optimizations added latest get called first."
  `(let ((opts (get ',target-function 'optimizers)))
     (or (memq ',optimizer-name opts)
	 (putprop ',target-function (nconc opts (ncons ',optimizer-name)) 'optimizers))
     (setq opts (get ',target-function 'optimized-into))
     (dolist (into ',optimized-into)
       (pushnew into opts))
     (and opts
	  (putprop ',target-function opts 'optimized-into))
     ',target-function))

(defmacro defoptimizer (function-to-optimize optimizer-name
			&optional ((&rest optimizes-into)) arglist &body body)
  "(defoptimizer foo foo-optimizer (optfoo1 optfoo2) (form)
     (if (eq (cadr form) 'foo)
         `(and (optfoo . ,(cadr form))
               (optfoo2 . (caddr form)))
        form))
OR
/(defoptimizer foo common-foo-optimizer (optfoo1 optfoo2))"
  (unless optimizer-name
    (setq optimizer-name (string-append function-to-optimize "-OPTIMIZER"))
    (if (intern-soft optimizer-name)
	(setq optimizer-name (gentemp (string-append optimizer-name "-")))
      (setq optimizer-name (intern optimizer-name))))
  (if (null arglist)
      `(add-optimizer ,function-to-optimize ,optimizer-name . ,optimizes-into)
    `(progn (add-optimizer ,function-to-optimize ,optimizer-name . ,optimizes-into)
	    (defun ,optimizer-name ,arglist
	      (declare (function-parent ,optimizer-name defoptimzer))
	      . ,body))))


(defmacro defcompiler-synonym (function synonym-function)
  "Make the compiler substitute SYNONYM-FUNCTION for FUNCTION when compiling.
eg (defcompiler-synonym plus +)"
  `(defoptimizer ,function ,(intern (string-append function "-TO-" synonym-function))
                           (,synonym-function) (form)
     (cons ',synonym-function (cdr form))))

))

(do-local-symbols (si:foo (find-package "CLI"))
  (export si:foo "CLI"))

(eval-when (load compile eval)
  (globalize "NUMBER")
  (unintern "NUMBER" "SI")
  (globalize "MEMBER-EQL" "SYSTEM")
  (unintern "MEMBER-EQL" "SI")
  (globalize "ASSOC-EQL" "SYSTEM")
  (globalize "RASSOC-EQUALP" "SYSTEM")
  (globalize "RASSOC-EQL" "SYSTEM"))
(dolist (si:foo *all-packages*)
  (unless (or (eq si:foo pkg-global-package) (eq si:foo pkg-keyword-package))
    (unintern "NUMBER" si:foo)))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO SELECTQ (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the CASE.
T or OTHERWISE as the first element of a clause matches any test object."
  (LET (TEST-EXP COND-EXP)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '.SELECTQ.ITEM.)))
    (SETQ COND-EXP
	  (CONS 'COND
		(MAPCAR #'(LAMBDA (CLAUSE)
			    (MACRO-TYPE-CHECK-WARNING 'SELECTQ (CAR CLAUSE))
			    (COND ((MEMQ (CAR CLAUSE) '(OTHERWISE :OTHERWISE T))
				   (LIST* T NIL (CDR CLAUSE)))
				  ((ATOM (CAR CLAUSE))
				   `((EQL ,TEST-EXP ',(CAR CLAUSE)) NIL . ,(CDR CLAUSE)))
				  (T
				   `((MEMBER-EQL ,TEST-EXP ',(CAR CLAUSE)) NIL . ,(CDR CLAUSE)))))
			CLAUSES)))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'SELECTQ)
    (IF (EQL TEST-EXP TEST-OBJECT)
	COND-EXP
      `(LET ((.SELECTQ.ITEM. ,TEST-OBJECT))
	 ,COND-EXP))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CCASE (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the CCASE.
If no clause matches, a correctable error is signaled.
The user can correct with a new value for TEST-OBJECT."
  (LET (TEST-EXP COND-EXP TYPE-FOR-ERROR)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '.SELECTQ.ITEM.)))
    (SETQ TYPE-FOR-ERROR
	  `(MEMBER . ,(MAPCAN #'(LAMBDA (CLAUSE)
				  (LET ((MATCH (CAR CLAUSE)))
				    (IF (NOT (CONSP MATCH))
					(LIST MATCH)
				      (COPY-LIST MATCH))))
			      CLAUSES)))
    (SETQ COND-EXP
	  `(COND
	     ,@(MAPCAR #'(LAMBDA (CLAUSE)
			   (MACRO-TYPE-CHECK-WARNING 'CCASE (CAR CLAUSE))
			   (COND ((ATOM (CAR CLAUSE))
				  `((EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))
				 (T
				  `((MEMBER-EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))))
		       CLAUSES)
	     (T (SETF ,TEST-OBJECT
		      (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
			      "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
			      ',TYPE-FOR-ERROR
			      ,TEST-EXP ',TEST-OBJECT
			      ,(STRING-APPEND (TYPE-PRETTY-NAME TYPE-FOR-ERROR))))
		(GO CCASE-LOOP))))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'CCASE)
    (UNLESS (EQL TEST-EXP TEST-OBJECT)
      (SETQ COND-EXP
	    `(LET ((.SELECTQ.ITEM. ,TEST-OBJECT))
	       ,COND-EXP)))
    `(BLOCK CCASE-LOOP
       (TAGBODY
      CCASE-LOOP
         (RETURN-FROM CCASE-LOOP
	   ,COND-EXP)))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO ECASE (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (LET (TEST-EXP COND-EXP TYPE-FOR-ERROR)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '.SELECTQ.ITEM.)))
    (SETQ TYPE-FOR-ERROR
	  `(MEMBER . ,(MAPCAN #'(LAMBDA (CLAUSE)
				  (LET ((MATCH (CAR CLAUSE)))
				    (IF (NOT (CONSP MATCH))
					(LIST MATCH)
				      (COPY-LIST MATCH))))
			      CLAUSES)))
    (SETQ COND-EXP
	  `(COND
	     ,@(MAPCAR #'(LAMBDA (CLAUSE)
			   (MACRO-TYPE-CHECK-WARNING 'ECASE (CAR CLAUSE))
			   (COND ((ATOM (CAR CLAUSE))
				  `((EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))
				 (T
				  `((MEMBER-EQL ,TEST-EXP ',(CAR CLAUSE))
				    NIL . ,(CDR CLAUSE)))))
		       CLAUSES)
	     (T (FERROR NIL
			"The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
			',TYPE-FOR-ERROR
			,TEST-EXP ',TEST-OBJECT
			,(STRING-APPEND (TYPE-PRETTY-NAME TYPE-FOR-ERROR))))))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'ECASE)
    (UNLESS (EQL TEST-EXP TEST-OBJECT)
      (SETQ COND-EXP
	    `(LET ((.SELECTQ.ITEM. ,TEST-OBJECT))
	       ,COND-EXP)))
    COND-EXP))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCP1.."

(defun (:property if p1) (form)
  `(cond ,(p1progn-1 `(,(cadr form) ,(caddr form)))
	 ,(p1progn-1 `(t nil . ,(cdddr form)))))

))


; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defprop if cw-eval-args cw-handler)

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun if (&quote test then &rest elses)
  "Execute THEN if TEST comes out non-NIL; otherwise, execute the ELSES."
  (if (eval1 test)
      (eval1 then)
    (eval-body elses)))

))

(eval-when (load compile eval)
  (let ((si:foo 'cadr:named-structure))
    (unintern si:foo "CADR")
    (intern si:foo pkg-global-package)))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "


(DEFUN MACRO-FUNCTION (FSPEC)
  "If FSPEC has a function definition which is a macro, return the expander function; else NIL."
  (COND ((FDEFINEDP FSPEC)
	 (LET ((DEF (FDEFINITION FSPEC)))
	   (COND ((EQ (CAR-SAFE DEF) 'MACRO)
		  (CDR DEF))
		 ((AND (SYMBOLP FSPEC)
		       (CDR (GET FSPEC 'ALTERNATE-MACRO-DEFINITION))))
		 ((SYMBOLP DEF)
		  (MACRO-FUNCTION DEF))
		 (T NIL))))
	((SYMBOLP FSPEC)
	 (CDR (GET FSPEC 'ALTERNATE-MACRO-DEFINITION)))
	(T NIL)))

(DEFUN SPECIAL-FORM-P (SYMBOL &AUX arglist)
  "T if SYMBOL has a function definition taking unevaluated arguments."
  (AND (FBOUNDP SYMBOL)
	 (AND (NEQ (CAR-SAFE (FSYMEVAL SYMBOL)) 'MACRO)
	      (CONSP (SETQ ARGLIST (ARGLIST SYMBOL T)))
	      (MEMQ '&QUOTE ARGLIST)
	      T)))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFMACRO MAKE-OBSOLETE (FUNCTION REASON)
  "Mark FUNCTION as obsolete, with string REASON as the reason.
REASON should be a clause starting with a non-capitalized word.
Uses of FUNCTION will draw warnings from the compiler."
  `(PROGN (PUTPROP ',FUNCTION 'COMPILER:OBSOLETE 'COMPILER:STYLE-CHECKER)
	  (PUTPROP ',FUNCTION (STRING-APPEND "is an obsolete function; " ,REASON)
		   'COMPILER:OBSOLETE)))

))

(load "sys: sys2; defsel qfasl >" :verbose nil :set-default-pathname nil)

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM
		     &OPTIONAL
		     (WHICH-OPERATIONS (WHICH-OPERATIONS-FOR-PRINT STREAM))
		     &AUX NSS (FASTP (MEMQ ':STRING-OUT WHICH-OPERATIONS)))
  (CATCH-CONTINUATION-IF T 'PRINT-OBJECT
      #'(LAMBDA () (FORMAT STREAM "...error printing ")
		(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP :FASTP FASTP))
		(FORMAT STREAM "..."))
      NIL
    (CONDITION-RESUME '((ERROR) :ABORT-PRINTING T ("Give up trying to print this object.")
			CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
      (OR (AND (MEMQ ':PRINT WHICH-OPERATIONS)	;Allow stream to intercept print operation
	       (SEND STREAM ':PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*))
	  (AND *PRINT-CIRCLE*
	       (%POINTERP EXP)
	       (OR (NOT (SYMBOLP EXP))
		   (NOT (SYMBOL-PACKAGE EXP)))
	       ;; This is a candidate for circular or shared structure printing.
	       ;; See what the hash table says about the object:
	       ;; NIL - occurs only once.
	       ;; T - occurs more than once, but no occurrences printed yet.
	       ;;  Allocate a label this time and print #label= as prefix.
	       ;; A number - that is the label.  Print only #label#.
	       (*CATCH 'LABEL-PRINTED
		 (SEND PRINT-HASH-TABLE ':MODIFY-HASH EXP
		       #'(LAMBDA (KEY VALUE KEY-FOUND-P STREAM)
			   KEY KEY-FOUND-P
			   (COND ((NULL VALUE) NIL)
				 ((EQ VALUE T)
				  (LET ((LABEL (INCF PRINT-LABEL-NUMBER))
					(*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM ':TYO #/=)
				    LABEL))
				 (T
				  (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM ':TYO #/#)
				    (*THROW 'LABEL-PRINTED T)))))
		       STREAM)
		 NIL))
	  (TYPECASE EXP
	    (FIXNUM (PRINT-FIXNUM EXP STREAM))
	    (SYMBOL (PRINT-PNAME-STRING EXP STREAM FASTP))
	    (LIST
	     (IF (AND *PRINT-LEVEL* ( I-PRINDEPTH *PRINT-LEVEL*))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL *READTABLE*) STREAM FASTP)
	       (IF *PRINT-PRETTY*
		   (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		 (PRINT-LIST EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (STRING
	     (IF ( (ARRAY-ACTIVE-LENGTH EXP) (ARRAY-LENGTH EXP))
		 (PRINT-QUOTED-STRING EXP STREAM FASTP)
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (INSTANCE
	      (SEND EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
	    (ENTITY
	     (IF (MEMQ ':PRINT-SELF (SEND EXP ':WHICH-OPERATIONS))
		 (SEND EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*) 
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:NAMED-STRUCTURE
	     (IGNORE-ERRORS
	       (SETQ NSS (NAMED-STRUCTURE-P EXP)))
	     (COND ((AND (SYMBOLP NSS)
			 (OR (GET NSS 'NAMED-STRUCTURE-INVOKE)
			     (GET NSS ':NAMED-STRUCTURE-INVOKE))
			 (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;Named structure that doesn't print itself
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (ARRAY
	     (PRINT-ARRAY EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))
	    (SHORT-FLOAT
	     (PRINT-FLONUM EXP STREAM FASTP T))
	    (FLOAT
	     (PRINT-FLONUM EXP STREAM FASTP NIL))
	    (BIGNUM
	     (PRINT-BIGNUM EXP STREAM FASTP))
	    (RATIONAL
	     (PRINT-RATIONAL EXP STREAM FASTP))
	    (COMPLEX (PRINT-COMPLEX EXP STREAM FASTP))
	    (CHARACTER
	      (IF (NOT *PRINT-ESCAPE*)
		  (SEND STREAM ':TYO (LDB %%CH-CHAR EXP))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-BEFORE-FONT *READTABLE*))
		(IF (LDB-TEST %%CH-FONT EXP)
		    (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
		      (PRIN1 (LDB %%CH-FONT EXP) STREAM)))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-PREFIX *READTABLE*))
		(LET ((BITS (CHAR-BITS EXP))
		      (CHAR (CHAR-CODE EXP)))
		  (SEND STREAM ':STRING-OUT
			       (NTH BITS
				    '("" "c-" "m-" "c-m-"
				      "s-" "c-s-" "m-s-" "c-m-s-"
				      "h-" "c-h-" "m-h-" "c-m-h-"
				      "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")))
		  (LET ((CHNAME (FORMAT:OCHAR-GET-CHARACTER-NAME CHAR)))
		    (IF CHNAME
			(SEND STREAM ':STRING-OUT CHNAME)
		      (AND ( BITS 0)
			   (SI:CHARACTER-NEEDS-QUOTING-P CHAR)
			   (SEND STREAM ':TYO (SI:PTTBL-SLASH *READTABLE*)))
		      (SEND STREAM ':TYO CHAR))))))
	    (NUMBER
	     (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM *READTABLE*) STREAM FASTP)
	     (PRINT-RAW-STRING (GET-PNAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (SEND STREAM ':TYO (PTTBL-SPACE *READTABLE*))
	     (LET ((*PRINT-BASE* 8) (*PRINT-RADIX* NIL))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM *READTABLE*) STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

))


; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer quote unquote () (form)
  (if (= (length form) 2)
      (if (or (numberp (cadr form))
	      (characterp (cadr form))
	      (eq (cadr form) t)
	      (eq (cadr form) nil))
	  (cadr form)
	form)
    form))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(make-obsolete catch "Use CLI:CATCH or *CATCH instead")
(make-obsolete throw "Use CLI:THROW or *THROW instead")
(defcompiler-synonym cli:catch	*catch)
(defcompiler-synonym cli:throw  *throw)

))

; From file HASHFL.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; HASHFL  "

(DEFMETHOD (HASH-TABLE :CASE :SET :GET-HASH) (KEY &REST VALUES)
  (DECLARE (ARGLIST (KEY VALUE)))
  ;; use car last is to ignore optional default eg from "(push zap (send foo :get-hash bar))"
  (LEXPR-SEND SELF ':PUT-HASH KEY (CAR (LAST VALUES))))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFMETHOD (PROPERTY-LIST-MIXIN :CASE :SET :GET) (INDICATOR &REST PROPERTY)
  (DECLARE (ARGLIST INDICATOR PROPERTY))
  ;; use car last is to ignore optional default eg from "(push zap (send foo :get bar))"
  (PUTPROP (LOCF PROPERTY-LIST) (CAR (LAST PROPERTY)) INDICATOR))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN PROPERTY-LIST-HANDLER (OP PLIST &REST ARGS)
  (SELECTQ OP
    (:GET
     (GET PLIST (FIRST ARGS) (SECOND ARGS)))
    (:GET-LOCATION-OR-NIL
     (GET-LOCATION-OR-NIL PLIST (FIRST ARGS)))
    (:GET-LOCATION
      (LOCF (GET PLIST (FIRST ARGS))))
    (:GETL
     (GETL PLIST (CAR ARGS)))
    (:PUTPROP
     (PUTPROP PLIST (FIRST ARGS) (SECOND ARGS)))
    (:REMPROP
     (REMPROP PLIST (CAR ARGS)))
    (:PUSH-PROPERTY
     (PUSH (FIRST ARGS) (GET PLIST (SECOND ARGS))))
    (:PLIST
     (CONTENTS PLIST))
    ((:PLIST-LOCATION :PROPERTY-LIST-LOCATION)
     PLIST)
    (:SETPLIST
     (SETF (CONTENTS PLIST) (FIRST ARGS)))
    (:SET
     (SELECTQ (FIRST ARGS)
       (:GET
	(PUTPROP PLIST (CAR (LAST ARGS)) (SECOND ARGS)))
       (:PLIST (SETF (CONTENTS PLIST) (SECOND ARGS)))
       (:WHICH-OPERATIONS '(:GET :PLIST))
       (T (FERROR NIL "Don't know how to :SET ~S" (FIRST ARGS)))))
    (:WHICH-OPERATIONS '(:GET :GET-LOCATION :GET-LOCATION-OR-NIL :GETL :PUTPROP :REMPROP
			 :PUSH-PROPERTY :PLIST :PLIST-LOCATION :PROPERTY-LIST-LOCATION :SET
			 :SETPLIST :WHICH-OPERATIONS))
    (T (FERROR NIL "Don't know how to ~S a plist" OP))))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN EXPORT (SYMBOLS &OPTIONAL (PKG *PACKAGE*) FORCE-FLAG)
  "Makes SYMBOLS external in package PKG.
If the symbols are not already present in PKG, they are imported first.
Error if this causes a name conflict in any package that USEs PKG.
FORCE-FLAG non-NIL turns off checking for name conflicts, for speed
in the case where you know there cannot be any."
  (SETQ PKG (PKG-FIND-PACKAGE PKG))
  (UNLESS FORCE-FLAG
    (DO-FOREVER
      (LET ((CONFLICTS))
	;; Find all conflicts there are.
	;; Each element of CONFLICTS looks like
	;; (NEW-CONFLICTING-SYMBOL CONFLICT-PACKAGE
	;;   (OTHER-PACKAGE-NAME OTHER-PACKAGE-SYMBOL OTHER-PACKAGE)...)
	(DOLIST (P1 (PKG-USED-BY-LIST PKG))
	  (DOLIST (SYMBOL (IF (CLI:LISTP SYMBOLS) SYMBOLS (LIST SYMBOLS)))
	    (LET ((CANDIDATES
		    (CHECK-FOR-NAME-CONFLICT (IF (SYMBOLP SYMBOL) (GET-PNAME SYMBOL) SYMBOL)
					     P1 NIL SYMBOL PKG)))
	      (WHEN CANDIDATES
		(PUSH (LIST* SYMBOL P1 CANDIDATES) CONFLICTS)))))
	(UNLESS CONFLICTS (RETURN NIL))
	;; Now report whatever conflicts we found.
	(CERROR ':NO-ACTION NIL 'SYMBOL-NAME-CONFLICT
		"Name conflicts created by EXPORT in package ~A:
~:{~S causes a conflict in package ~A.~%~}"
		PKG CONFLICTS))))
  (DOLIST (SYM (IF (CLI:LISTP SYMBOLS) SYMBOLS (LIST SYMBOLS)))
    (UNLESS (AND (SYMBOLP SYM)
		 (EQ (SYMBOL-PACKAGE SYM) PKG))
      (SETQ SYM (INTERN-LOCAL SYM PKG)))
    (IMPORT SYM PKG)
    (MULTIPLE-VALUE-BIND (NIL INDEX)
	(PKG-INTERN-INTERNAL (GET-PNAME SYM)
			      (PKG-STRING-HASH-CODE (GET-PNAME SYM))
			      PKG)
      (SETF (PKG-SLOT-CODE PKG INDEX)
	    (PKG-MAKE-CODE 1 (PKG-SLOT-CODE PKG INDEX)))))
  T)

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; CLPACK  "

(defun package-property-list-handler (op self &rest args)
  (apply 'property-list-handler op (locf (pkg-plist self)) args))
(defselect ((:property package named-structure-invoke))
  (:describe (self)
    (describe-package self))
  (:print-self (self *standard-output* ignore &optional ignore)
    (if *print-escape*
	(si:printing-random-object (self *standard-output*)
	  (princ "Package ")
	  (princ (pkg-name self)))
      (princ (pkg-name self))))
  ((:get :get-location-or-nil :get-location :getl :putprop :remprop :push-property :plist
    :plist-location :property-list-location :setplist :set)
   . package-property-list-handler))
))

; From file POSS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN SECTION-POSSIBILITY (BP SECTION)
  BP
  (IF (NOT (MEMQ (NODE-SUPERIOR SECTION) *ZMACS-BUFFER-LIST*))
      (FORMAT *QUERY-IO* "~&The section ~A is in a buffer that has been killed." SECTION)
    (LET* ((SECTION-NODE-DEFUN-LINE (SECTION-NODE-DEFUN-LINE SECTION))
	   (SECTION-BP (AND SECTION-NODE-DEFUN-LINE
			    (IF (NEQ (LINE-TICK SECTION-NODE-DEFUN-LINE) 'DELETED)
				(CREATE-BP SECTION-NODE-DEFUN-LINE 0)
			      (INTERVAL-FIRST-BP SECTION)))))
      (IF (NOT SECTION-BP)
	  (FORMAT *QUERY-IO* "Cannot edit section ~A." SECTION)	;unreal section (deleted, eg)
	(POINT-PDL-PUSH (POINT) *WINDOW* T)
	(MAKE-BUFFER-CURRENT (NODE-SUPERIOR SECTION))
	(MOVE-BP (POINT) SECTION-BP)
	(RECENTER-WINDOW *WINDOW* ':START (BACKWARD-OVER-COMMENT-LINES (POINT) NIL))))))

))

; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFCONST TV:TV-SLOT-NUMBER #xF8 "Slot number for LAMBDA tv board.")

))

; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN LISP-REINITIALIZE (&OPTIONAL (CALLED-BY-USER T)
			  &AUX (COLD-BOOT COLD-BOOTING)
			  MUST-ENABLE-TRAPPING)
  (SETQ INHIBIT-SCHEDULING-FLAG T)		;In case called by the user
 ;  (AND (= PROCESSOR-TYPE-CODE LAMBDA-TYPE-CODE)
 ;       (SETQ TV:TV-SLOT-NUMBER (+ #XF0 (READ-A-MEMORY-LOCATION 560))))	;*****temp.
  (SETQ TV:TV-SLOT-NUMBER #xF8)
  (SETQ ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)
  ;; If these are set wrong, all sorts of things don't work.
  (SETQ LOCAL-DECLARATIONS NIL FILE-LOCAL-DECLARATIONS NIL
	UNDO-DECLARATIONS-FLAG NIL COMPILER:QC-FILE-IN-PROGRESS NIL)
  ;; Set interpreter state to Zetalisp, top level.
  (SETQ INTERPRETER-ENVIRONMENT NIL INTERPRETER-FUNCTION-ENVIRONMENT T)
  ;; Provide ucode with space to keep EVCPs stuck into a-memory locations
  ;; by closure-binding the variables that forward there.
  (OR (AND (BOUNDP 'AMEM-EVCP-VECTOR) AMEM-EVCP-VECTOR)
      (SETQ AMEM-EVCP-VECTOR
	    (MAKE-ARRAY (+ (LENGTH SYS:A-MEMORY-LOCATION-NAMES) 40 20)  ;20 in case ucode grows.
			':AREA PERMANENT-STORAGE-AREA)))
  (COND ((NOT CALLED-BY-USER)
	 (AND (FBOUNDP 'COMPILER:MA-RESET) ;Unload microcompiled defs, because they are gone!
	      (COMPILER:MA-RESET))	 ; Hopefully manage to do this before any gets called.
	 ;; Set up the TV sync program as soon as possible; until it is set up
	 ;; read references to the TV buffer can get NXM errors which cause a
	 ;; main-memory parity error halt.  Who-line updating can do this.
	 (COND ((BOUNDP 'TV:DEFAULT-SCREEN)
		(COND ((BOUNDP 'TV:SYNC-RAM-CONTENTS)
		       ;;if TV:SET-TV-SPEED has been done in this image,
		       ;;use the results from that.
		       (SETUP-CPT TV:SYNC-RAM-CONTENTS NIL T))
		      (T (SETUP-CPT)))
		(COND ((VARIABLE-BOUNDP TV:MAIN-SCREEN)
		       (SETQ %DISK-RUN-LIGHT
			     (+ (- (* TV:MAIN-SCREEN-HEIGHT
				      TV:(SHEET-LOCATIONS-PER-LINE MAIN-SCREEN))
				   15)
				(TV:SCREEN-BUFFER TV:MAIN-SCREEN)))))
		TV:(SETQ WHO-LINE-RUN-LIGHT-LOC (+ 2 (LOGAND %DISK-RUN-LIGHT 777777)))))
	 ;; Clear all the bits of the main screen after a cold boot.
	 (AND COLD-BOOT (CLEAR-SCREEN-BUFFER IO-SPACE-VIRTUAL-ADDRESS))))
  ;; Do something at least if errors occur during loading
  (OR (FBOUNDP 'FERROR) (FSET 'FERROR #'FERROR-COLD-LOAD))
  (OR (FBOUNDP 'CERROR) (FSET 'CERROR #'CERROR-COLD-LOAD))
  (OR (FBOUNDP 'UNENCAPSULATE-FUNCTION-SPEC)
      (FSET 'UNENCAPSULATE-FUNCTION-SPEC #'(LAMBDA (X) X)))
  (OR (FBOUNDP 'FS:MAKE-PATHNAME-INTERNAL) (FSET 'FS:MAKE-PATHNAME-INTERNAL #'LIST))
  (OR (FBOUNDP 'FS:MAKE-FASLOAD-PATHNAME) (FSET 'FS:MAKE-FASLOAD-PATHNAME #'LIST))
  ;; Allow streams to work before WHOLIN loaded
  (OR (BOUNDP 'TV:WHO-LINE-FILE-STATE-SHEET)
      (SETQ TV:WHO-LINE-FILE-STATE-SHEET #'IGNORE))
  (UNCLOSUREBIND '(* ** *** + ++ +++ // //// ////// *VALUES*))
  (SETQ DEFAULT-CONS-AREA WORKING-STORAGE-AREA)	;Reset default areas.
  (UNCLOSUREBIND '(READ-AREA))
  (SETQ READ-AREA NIL)
  (NUMBER-GC-ON)	;This seems to work now, make it the default
  (SETQ EH:CONDITION-HANDLERS NIL
	EH:CONDITION-DEFAULT-HANDLERS NIL
	EH:CONDITION-RESUME-HANDLERS NIL)
  (UNLESS (VARIABLE-BOUNDP *PACKAGE*)
    (PKG-INITIALIZE))

  (COND ((NOT (BOUNDP 'CURRENT-PROCESS))	;Very first time around
	 (SETQ SCHEDULER-EXISTS NIL
	       CURRENT-PROCESS NIL
	       TV:WHO-LINE-PROCESS NIL
	       TV:LAST-WHO-LINE-PROCESS NIL)
	 (OR (FBOUNDP 'TV:WHO-LINE-RUN-STATE-UPDATE)
	     (FSET 'TV:WHO-LINE-RUN-STATE-UPDATE #'(LAMBDA (&REST IGNORE) NIL)))
	 (KBD-INITIALIZE)))
  (SETQ TV:KBD-LAST-ACTIVITY-TIME (TIME))	; Booting is keyboard activity.
  (INITIALIZE-WIRED-KBD-BUFFER)
  (IF-IN-LAMBDA
    ;; now that the "unibus" channel is set up, turn on 60Hz interrupts
    (COMPILER:%NUBUS-WRITE TV:TV-SLOT-NUMBER 4
			   (LOGIOR 40 (COMPILER:%NUBUS-READ TV:TV-SLOT-NUMBER 4))))

  ;;Flush any closure binding forwarding pointers
  ;;left around from a closure we were in when we warm booted.
  (UNCLOSUREBIND '(PRIN1 *PRINT-BASE* *NOPOINT
		   FDEFINE-FILE-PATHNAME INHIBIT-FDEFINE-WARNINGS
		   SELF SI:PRINT-READABLY *PACKAGE* *READTABLE*
		   EH:ERROR-MESSAGE-HOOK EH:ERROR-DEPTH EH:ERRSET-STATUS))
  (WHEN (VARIABLE-BOUNDP ZWEI:*LOCAL-BOUND-VARIABLES*)
    (UNCLOSUREBIND ZWEI:*LOCAL-BOUND-VARIABLES*))
  (UNCLOSUREBIND '(ZWEI:*LOCAL-VARIABLES* ZWEI:*LOCAL-BOUND-VARIABLES*))
  ;Get the right readtable.
  (OR (VARIABLE-BOUNDP INITIAL-READTABLE)
      (SETQ INITIAL-READTABLE *READTABLE*
	    *READTABLE* (COPY-READTABLE *READTABLE*)
	    STANDARD-READTABLE *READTABLE*))
  (WHEN (VARIABLE-BOUNDP COMMON-LISP-READTABLE)
    (UNLESS (VARIABLE-BOUNDP INITIAL-COMMON-LISP-READTABLE)
      (SETQ INITIAL-COMMON-LISP-READTABLE COMMON-LISP-READTABLE
	    COMMON-LISP-READTABLE (COPY-READTABLE COMMON-LISP-READTABLE))))

  ;; Initialize the rubout handler.
  (SETQ	RUBOUT-HANDLER NIL)			;We're not in it now

  ;; Initialize the error handler.
  (OR (BOUNDP 'ERROR-STACK-GROUP)
      (SETQ ERROR-STACK-GROUP (MAKE-STACK-GROUP 'ERROR-STACK-GROUP ':SAFE 0)))
  (SETQ %ERROR-HANDLER-STACK-GROUP ERROR-STACK-GROUP)
  (STACK-GROUP-PRESET ERROR-STACK-GROUP 'LISP-ERROR-HANDLER)	;May not be defined yet 
  (SETF (SG-FOOTHOLD-DATA %INITIAL-STACK-GROUP) NIL)	;EH depends on this
  (COND ((AND (FBOUNDP 'LISP-ERROR-HANDLER)
	      (FBOUNDP 'EH:ENABLE-TRAPPING))
	 (IF (NOT (BOUNDP 'EH:ERROR-TABLE))
	     (SETQ MUST-ENABLE-TRAPPING T)
	   ;; Note: if error-table not loaded,
	   ;; we enable trapping after loading it.
	   (FUNCALL ERROR-STACK-GROUP '(INITIALIZE))
	   (EH:ENABLE-TRAPPING))))
  (SETQ EH:ERRSET-STATUS NIL EH:ERROR-MESSAGE-HOOK NIL)	;Turn off possible spurious errset
  (SETQ EH:ERROR-DEPTH 0)

  ;; And all kinds of randomness...

  (SETQ TRACE-LEVEL 0)
  (SETQ INSIDE-TRACE NIL)
  (SETQ + NIL * NIL - NIL ;In case of error during first read/eval/print cycle
	// NIL ++ NIL +++ NIL ;or if their values were unprintable or obscene
	** NIL *** NIL)  ;and to get global values in case of break in a non-lisp-listener
  (SETQ //// NIL ////// NIL)
  (SETQ LISP-TOP-LEVEL-INSIDE-EVAL NIL)
  (SETQ %INHIBIT-READ-ONLY NIL)
  (OR (BOUNDP 'PRIN1) (SETQ PRIN1 NIL))
  (SETQ EVALHOOK NIL APPLYHOOK NIL)
  (SETQ *READ-BASE* 8 *PRINT-BASE* 8 *NOPOINT NIL)
  (SETQ XR-CORRESPONDENCE-FLAG NIL		;Prevent the reader from doing random things
	XR-CORRESPONDENCE NIL)
  (SETQ *RSET T)				;In case any MACLISP programs look at it
  (SETQ FDEFINE-FILE-PATHNAME NIL)
  (SETQ INHIBIT-FDEFINE-WARNINGS NIL)		;Don't get screwed by warm boot
  (SETQ SELF-FLAVOR-DECLARATION NIL)
  (SETQ SELF NIL SELF-MAPPING-TABLE NIL)
  (SETQ SI:PRINT-READABLY NIL)
  (SETQ CHAOS:CHAOS-SERVERS-ENABLED NIL)	;Don't allow botherage from networks
  (IF (BOUNDP 'PKG-USER-PACKAGE)		;If package system is present
      (SETQ *PACKAGE* PKG-USER-PACKAGE))

  ;; The first time, this does top-level SETQ's from the cold-load files
  (OR (BOUNDP 'ORIGINAL-LISP-CRASH-LIST)	;Save it for possible later inspection
      (SETQ ORIGINAL-LISP-CRASH-LIST LISP-CRASH-LIST))
  (MAPC (FUNCTION EVAL) LISP-CRASH-LIST)
  (SETQ LISP-CRASH-LIST NIL)

  ;; Reattach IO streams.  Note that *TERMINAL-IO* will be fixed later to go to a window.
  (OR (BOUNDP 'SYN-TERMINAL-IO) )
  (COND ((NOT CALLED-BY-USER)
	 (UNCLOSUREBIND '(*TERMINAL-IO* *STANDARD-OUTPUT* *STANDARD-INPUT*
			  *QUERY-IO* *TRACE-OUTPUT* *ERROR-OUTPUT* *DEBUG-IO*))
	 (SETQ *TERMINAL-IO*	COLD-LOAD-STREAM
	       *STANDARD-OUTPUT* SYN-TERMINAL-IO
	       *STANDARD-INPUT*	SYN-TERMINAL-IO
	       *QUERY-IO*	SYN-TERMINAL-IO
	       *DEBUG-IO*	SYN-TERMINAL-IO
	       *TRACE-OUTPUT*	SYN-TERMINAL-IO
	       *ERROR-OUTPUT*	SYN-TERMINAL-IO)
	 (SEND *TERMINAL-IO* ':HOME-CURSOR)))

  (SETQ TV:MOUSE-WINDOW NIL)	;This gets looked at before the mouse process is turned on
  (KBD-CONVERT-NEW 1_15.)	;Reset state of shift keys

  (IF (AND (FBOUNDP 'CADR:CLEAR-UNIBUS-MAP)	;clear valid bits on unibus map.
	   (= PROCESSOR-TYPE-CODE 1))		; Prevents randomness
      (CADR:CLEAR-UNIBUS-MAP))			; and necessary if sharing Unibus with PDP11.
						; Do this before SYSTEM-INITIALIZATION-LIST to
						; avoid screwwing ETHERNET code.
  ;; These are initializations that have to be done before other initializations
  (INITIALIZATIONS 'SYSTEM-INITIALIZATION-LIST T)
  ;; At this point if the window system is loaded, it is all ready to go
  ;; and the initial Lisp listener has been exposed and selected.  So do
  ;; any future typeout on it.  But if any typeout happened on the cold-load
  ;; stream, leave it there (clobbering the Lisp listener's bits).  This does not
  ;; normally happen, but just in case we do the set-cursorpos below so that
  ;; if anything strange gets typed out it won't get erased.  Note that normally
  ;; we do not do any typeout nor erasing on the cold-load-stream, to avoid bashing
  ;; the bits of whatever window was exposed before a warm boot.
  (COND (CALLED-BY-USER)
	((FBOUNDP 'TV:WINDOW-INITIALIZE)
	 (MULTIPLE-VALUE-BIND (X Y) (SEND *TERMINAL-IO* ':READ-CURSORPOS)
	   (SEND TV:INITIAL-LISP-LISTENER ':SET-CURSORPOS X Y))
	 (SETQ *TERMINAL-IO* TV:INITIAL-LISP-LISTENER)
	 (SEND *TERMINAL-IO* ':SEND-IF-HANDLES ':SET-PACKAGE *PACKAGE*)
	 (SEND *TERMINAL-IO* ':FRESH-LINE))
	(T (SETQ TV:INITIAL-LISP-LISTENER NIL)	;Not created yet
	   (FUNCALL *TERMINAL-IO* ':CLEAR-EOL)))

  (AND CURRENT-PROCESS
       (FUNCALL CURRENT-PROCESS ':RUN-REASON 'LISP-INITIALIZE))

  ;; prevent screw from things being traced during initialization
  (if (fboundp 'untrace) (untrace))
  (if (fboundp 'breakon) (unbreakon))

  ;; Have to check explicitly for col-booting since can't just rely on initializations
  ;; to see that everything in this list has already run (ie at last cold boot)
  ;; since luser may have added own new inits
  (IF COLD-BOOTING (INITIALIZATIONS 'COLD-INITIALIZATION-LIST))
  (INITIALIZATIONS 'WARM-INITIALIZATION-LIST T)

  (COND ((AND MUST-ENABLE-TRAPPING
	      (BOUNDP 'EH:ERROR-TABLE))
	 ;; Note: this was done here if we just loaded the error table for the first time.
	 (FUNCALL ERROR-STACK-GROUP '(INITIALIZE))	; Initialize co-routining.
	 (EH:ENABLE-TRAPPING)))

  (SETQ COLD-BOOTING NIL)

  (IF (FBOUNDP 'PRINT-HERALD)
      (PRINT-HERALD)
    (SEND *STANDARD-OUTPUT* ':CLEAR-EOL)
    (PRINC "Lisp Machine cold load environment, beware!"))

  (AND (BOUNDP 'TIME:*LAST-TIME-UPDATE-TIME*)
       (NULL (CAR COLD-BOOT-HISTORY))
       (SETF (CAR COLD-BOOT-HISTORY) (CATCH-ERROR (LIST SI:LOCAL-HOST
							(GET-UNIVERSAL-TIME)))))

  ;; This process no longer needs to be able to run except for the usual reasons.
  ;; The delayed-restart processes may now be allowed to run
  (COND (CURRENT-PROCESS
	 (FUNCALL CURRENT-PROCESS ':REVOKE-RUN-REASON 'LISP-INITIALIZE)
	 (WHEN WARM-BOOTED-PROCESS
	   (FORMAT T "Warm boot while running ~S.
Its variable bindings remain in effect;
its unwind-protects have been lost.~%" WARM-BOOTED-PROCESS)
	   (WHEN (NOT (OR (EQ (PROCESS-WARM-BOOT-ACTION WARM-BOOTED-PROCESS)
			      'PROCESS-WARM-BOOT-RESTART)
			  (EQ WARM-BOOTED-PROCESS INITIAL-PROCESS)
			  (TYPEP WARM-BOOTED-PROCESS 'SI:SIMPLE-PROCESS)))
	     (IF (YES-OR-NO-P "Reset it?  Answer No if you want to debug it.  ")
		 (RESET-WARM-BOOTED-PROCESS)
	       (FORMAT T "~&Do (SI:DEBUG-WARM-BOOTED-PROCESS) to examine it, or do
/(SI:RESET-WARM-BOOTED-PROCESS) to reset it and let it run again.~%
If you examine it, you will see a state that is not quite the latest one."))))
	 (LOOP FOR (P . RR) IN DELAYED-RESTART-PROCESSES
	       DO (WITHOUT-INTERRUPTS
		    (SETF (PROCESS-RUN-REASONS P) RR)
		    (PROCESS-CONSIDER-RUNNABILITY P)))
	 (SETQ DELAYED-RESTART-PROCESSES NIL)))

  ;; The global value of *TERMINAL-IO* is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (COND ((AND (NOT CALLED-BY-USER)
	      (FBOUNDP TV:DEFAULT-BACKGROUND-STREAM))
	 (SETQ *TERMINAL-IO* TV:DEFAULT-BACKGROUND-STREAM)))

  ;; Now that -all- initialization has been completed, allow network servers
  (SETQ CHAOS:CHAOS-SERVERS-ENABLED T))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (ITS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
						      &REST ARGS)
  (SELECTQ TYP
    (:SYSTEM-DIRECTORY
     (SEND SELF ':NEW-PATHNAME ':NAME (IF SAME-DIRECTORY-P PATOM
					(IF SAME-DIRECTORY-P PATOM
					  (IF (< (STRING-LENGTH NAM) 7.)
					      NAM
					    (SI:SYSTEM-SHORT-NAME NAM))))
	   		       ':TYPE "(PDIR)"))
    (:VERSION-DIRECTORY
     (SEND SELF ':NEW-PATHNAME ':NAME (WITH-OUTPUT-TO-STRING (STREAM)
					(LET ((SNAME (IF SAME-DIRECTORY-P PATOM
						       (SI:SYSTEM-SHORT-NAME NAM))))
					  (DOTIMES (I (MIN (STRING-LENGTH SNAME) 3))
					    (FUNCALL STREAM ':TYO (AREF SNAME I))))
					(LET ((*PRINT-BASE* 10.) (*NOPOINT T))
					  (PRIN1 (\ (CAR ARGS) 1000.) STREAM)))
	   		       ':TYPE "(PDIR)"))
    (:PATCH-FILE
     (SEND SELF ':NEW-PATHNAME ':NAME (FORMAT NIL "~:[~*~;~C~]~D.~D"
					      SAME-DIRECTORY-P PATOM
					      (\ (CAR ARGS) 100.)
					      (\ (CADR ARGS)
						 (IF SAME-DIRECTORY-P 100. 1000.)))
			       ':TYPE (CADDR ARGS)))))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (VMS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
						      &REST ARGS)
  (SELECTQ TYP
    (:SYSTEM-DIRECTORY
     (SEND SELF ':NEW-PATHNAME ':NAME (IF SAME-DIRECTORY-P PATOM
					   (IF (< (STRING-LENGTH NAM) 10.)
					       NAM
					      (SI:SYSTEM-SHORT-NAME NAM)))
		   ':TYPE ':PATCH-DIRECTORY ':VERSION ':NEWEST))
    (:VERSION-DIRECTORY
     (SEND SELF ':NEW-PATHNAME ':NAME (WITH-OUTPUT-TO-STRING (STREAM)
					   (LET ((SNAME (IF SAME-DIRECTORY-P PATOM
							  (SI:SYSTEM-SHORT-NAME NAM))))
					     (DOTIMES (I (MIN (STRING-LENGTH SNAME) 6))
					       (FUNCALL STREAM ':TYO (AREF SNAME I))))
					   (LET ((*PRINT-BASE* 10.) (*NOPOINT T))
					     (PRIN1 (\ (CAR ARGS) 1000.) STREAM)))
		   ':TYPE ':PATCH-DIRECTORY ':VERSION ':NEWEST))
    (:PATCH-FILE
     (SEND SELF ':NEW-PATHNAME ':NAME (FORMAT NIL "~:[~*~;~C~]~D@T~D"
						 SAME-DIRECTORY-P PATOM
						 (\ (CAR ARGS) 100.)
						 (\ (CADR ARGS)
						    (IF SAME-DIRECTORY-P 100. 1000.)))
		   ':TYPE (CADDR ARGS) ':VERSION ':NEWEST))))

))


	   
; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer <= <=-OPTIMIZER (internal-> plusp not) (form)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   `(progn ,(car args) 't))
	  ((= N-ARGS 2)
	   (IF (eq (SECOND ARGS) 0)
	       `(NOT (PLUSP ,(FIRST ARGS)))
	     `(NOT (INTERNAL-> . ,ARGS))))
	  ((EVERY (CDR ARGS) 'TRIVIAL-FORM-P)
	   (CONS 'AND (LOOP FOR ARG IN (CDR ARGS)
			    AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			    COLLECT `(NOT (INTERNAL-> ,LAST-ARG ,ARG)))))
	  (T FORM))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer //= //=-OPTIMIZER (= zerop not) (form)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   `(progn ,(car args) 't))
	  ((= N-ARGS 2)
	   (IF (eq (SECOND ARGS) 0)
	       `(NOT (ZEROP ,(FIRST ARGS)))
	     `(NOT (= . ,ARGS))))
	  ((AND (= N-ARGS 3)
		(EVERY ARGS 'TRIVIAL-FORM-P))
	   `(NOT (OR (= ,(CAR ARGS) ,(CADR ARGS))
		     (= ,(CAR ARGS) ,(CADDR ARGS))
		     (= ,(CADR ARGS) ,(CADDR ARGS)))))
	  (T FORM))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer >= >=-OPTIMIZER (internal-< minusp not) (form)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   `(progn ,(car args) 't))
	  ((= N-ARGS 2)
	   (IF (eq (SECOND ARGS) 0)
	       `(NOT (MINUSP ,(FIRST ARGS)))
	     `(NOT (INTERNAL-< . ,ARGS))))
	  ((EVERY (CDR ARGS) 'TRIVIAL-FORM-P)
	   `(AND . ,(LOOP FOR ARG IN (CDR ARGS)
			  AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			  COLLECT `(NOT (INTERNAL-< ,LAST-ARG ,ARG)))))
	  (T FORM))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer LESSP LESSP-OPTIMIZER (internal-< minusp) (form)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   `(progn ,(car args) 't))
	  ((= N-ARGS 2)
	   (IF (eq (SECOND ARGS) '0)
	       `(MINUSP ,(FIRST ARGS))
	     `(INTERNAL-< . ,ARGS)))
	  ((EVERY (CDR ARGS) 'TRIVIAL-FORM-P)
	   `(AND . ,(LOOP FOR ARG IN (CDR ARGS)
			  AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			  COLLECT `(INTERNAL-< ,LAST-ARG ,ARG))))
	  (T FORM))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer GREATERP GREATERP-OPTIMIZER (internal-> plusp) (form)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   `(progn ,(car args) 't))
	  ((= N-ARGS 2)
	   (IF (eq (SECOND ARGS) 0)
	       `(PLUSP ,(FIRST ARGS))
	     `(INTERNAL-> . ,ARGS)))
	  ((EVERY (CDR ARGS) 'TRIVIAL-FORM-P)
	   `(AND . ,(LOOP FOR ARG IN (CDR ARGS)
			  AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			  COLLECT `(INTERNAL-> ,LAST-ARG ,ARG))))
	  (T FORM))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer CHAR-EQUAL CHAR-EQUAL-OPTIMIZER (internal-char-equal) (form)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   `(progn ,(car args) 't))
	  ((= N-ARGS 2)
	   `(INTERNAL-CHAR-EQUAL . ,ARGS))
	  ((EVERY (CDR ARGS) 'TRIVIAL-FORM-P)
	   `(AND . ,(LOOP FOR ARG IN (CDR ARGS)
			  AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			  COLLECT `(INTERNAL-CHAR-EQUAL ,LAST-ARG ,ARG))))
	  (T FORM))))

))

(load "sys:fonts;search" :verbose nil :set-default-pathname nil)
(tv:update-font-maps)

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN ASSOC (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CAR is EQUAL to ITEM."
  (PROG ()
	(IF (TYPEP ITEM '(OR SYMBOL FIXNUM SHORT-FLOAT))
	    (RETURN (ASSQ ITEM IN-LIST)))
     L	(COND ((NULL IN-LIST) (RETURN NIL))
	      ((NULL (CAR IN-LIST)))
	      ((EQUAL ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
	(SETQ IN-LIST (CDR IN-LIST))
	(GO L)))

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN ASSOC-EQUALP (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CAR is EQUALP to ITEM."
  (PROG ()
	(IF (TYPEP ITEM 'SYMBOL)
	    (RETURN (ASSQ ITEM IN-LIST)))
     L	(COND ((NULL IN-LIST) (RETURN NIL))
	      ((NULL (CAR IN-LIST)))
	      ((EQUALP ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
	(SETQ IN-LIST (CDR IN-LIST))
	(GO L)))

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN ASSOC-EQL (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CAR is EQUALP to ITEM."
  (PROG ()
	(IF (TYPEP ITEM '(OR (NOT NUMBER) FIXNUM SHORT-FLOAT))
	    (RETURN (ASSQ ITEM IN-LIST)))
     L	(COND ((NULL IN-LIST) (RETURN NIL))
	      ((NULL (CAR IN-LIST)))
	      ((EQL ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
	(SETQ IN-LIST (CDR IN-LIST))
	(GO L)))

))


; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN RASSOC-EQL (ITEM IN-LIST) 
  "Return the first element of IN-LIST whose CDR is EQUAL to ITEM."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
	 (EQL ITEM (CDAR L)) 
	 (RETURN (CAR L)))))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

(DEFUN FLAG-ALREADY-OPTIMIZED (FORM &AUX L)
  (IF (NOT (CONSP FORM))
      FORM
    (WITHOUT-INTERRUPTS
      (SETQ L (MAKE-LIST 3 ':INITIAL-ELEMENT 'ALREADY-OPTIMIZED))
      (SETF (CAR L) (CAR FORM)
	    (CADR L) (CDR FORM))
      (%P-DPB CDR-NORMAL %%Q-CDR-CODE (%POINTER L))
      (%P-DPB CDR-NORMAL %%Q-CDR-CODE (1+ (%POINTER L)))
      L)))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

(DEFUN ALREADY-OPTIMIZED-P (FORM)
  (WITHOUT-INTERRUPTS
    (OR (ATOM FORM)
	(AND (EQ (%P-LDB %%Q-CDR-CODE (%POINTER FORM)) CDR-NORMAL)
	     (EQ (%P-LDB %%Q-CDR-CODE (1+ (%POINTER FORM))) CDR-NORMAL)
	     (EQ (%P-LDB %%Q-POINTER (+ (%POINTER FORM) 2)) (%POINTER 'ALREADY-OPTIMIZED))))))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

;;; Given a form, apply optimizations and expand macros until no more is possible
;;; (at the top level).  Also apply style-checkers to the supplied input
;;; but not to generated output.  This function is also in charge of checking for
;;; too few or too many arguments so that this happens before optimizers are applied.
(DEFUN OPTIMIZE (FORM CHECK-STYLE
		 &AUX (MACRO-CONS-AREA
			(IF (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
			    BACKGROUND-CONS-AREA
			  DEFAULT-CONS-AREA))
		 OPTIMIZATIONS-BEGUN-FLAG)
  (IF (ALREADY-OPTIMIZED-P FORM)
      FORM
    (DO ((TM) (FN))
	((ATOM FORM))				;Do until no more expansions possible
      (LET ((DEFAULT-CONS-AREA MACRO-CONS-AREA))
	(SETQ FN (LAMBDA-MACRO-EXPAND (CAR FORM))))
      (UNLESS (EQ FN (CAR FORM)) (SETQ FORM (CONS FN (CDR FORM))))
      (UNLESS OPTIMIZATIONS-BEGUN-FLAG
	;; Check for too few or too many arguments
	(CHECK-NUMBER-OF-ARGS FORM FN))
      ;; If function is redefined locally with FLET,
      ;; don't use things that reflect its global definition.
      (WHEN (ASSQ FN LOCAL-FUNCTIONS)
	(RETURN))
      (UNLESS OPTIMIZATIONS-BEGUN-FLAG
	;; Do style checking
	(AND CHECK-STYLE (NOT INHIBIT-STYLE-WARNINGS-SWITCH)
	     (COND ((ATOM FN)
		    (AND (SYMBOLP FN)
			 (SETQ TM (GET FN 'STYLE-CHECKER))
			 (FUNCALL TM FORM)))
		   ((NOT RUN-IN-MACLISP-SWITCH))
		   ((MEMQ (CAR FN) '(LAMBDA NAMED-LAMBDA))
		    (LAMBDA-STYLE FN))
		   ((MEMQ (CAR FN) '(CURRY-BEFORE CURRY-AFTER))
		    (WARN 'NOT-IN-MACLISP ':MACLISP "~S does not work in Maclisp."
			  (CAR FN))))))
      ;; Apply optimizations
      (OR (AND (SYMBOLP FN)
	       (PROGN
		 ;; if not a special form, optimize arguments first
		 (IF (NOT (OR (SPECIAL-FORM-P FN) (MACRO-FUNCTION FN)))
		     (SETQ FORM (CONS (CAR FORM)
				      (MAPCAR #'(LAMBDA (X) (OPTIMIZE X CHECK-STYLE))
					      (CDR FORM)))))
		 (DOLIST (OPT (GET FN 'OPTIMIZERS))
		   (UNLESS (EQ FORM (SETQ FORM (FUNCALL OPT FORM)))
		     ;; Optimizer changed something, don't do macros this pass
		     (SETQ OPTIMIZATIONS-BEGUN-FLAG T)
		     (RETURN T)))))
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
    (FLAG-ALREADY-OPTIMIZED FORM)))

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defvar interpreter-declaration-type-alist
  '(("SPECIAL" special//unspecial-interpreter-declaration)
    ("UNSPECIAL" special//unspecial-interpreter-declaration)

    ("SELF-FLAVOR" self-flavor-interpreter-declaration)
    ("FUNCTION-PARENT" ignore)

    ("TYPE" ignore)
    ("ARRAY" ignore)
    ("ATOM" ignore)
    ("BIGNUM" ignore)
    ("BIT" ignore)
    ("BIT-VECTOR" ignore)
    ("CHARACTER" ignore)
    ("COMMON" ignore)
    ("COMPILED-FUNCTION" ignore)
    ("COMPLEX" ignore)
    ("CONS" ignore)
    ("DOUBLE-FLOAT" ignore)
    ("FIXNUM" ignore)
    ("FLOAT" ignore)
    ("FUNCTION" ignore)
    ("HASH-TABLE" ignore)
    ("INTEGER" ignore)
    ("KEYWORD" ignore)
    ("LIST" ignore)
    ("LONG-FLOAT" ignore)
    ("NIL" ignore)
    ("NULL" ignore)
    ("NUMBER" ignore)
    ("PACKAGE" ignore)
    ("PATHNAME" ignore)
    ("RANDOM-STATE" ignore)
    ("RATIO" ignore)
    ("RATIONAL" ignore)
    ("READTABLE" ignore)
    ("SEQUENCE" ignore)
    ("SHORT-FLOAT" ignore)
    ("SIMPLE-ARRAY" ignore)
    ("SIMPLE-BIT-VECTOR" ignore)
    ("SIMPLE-STRING" ignore)
    ("SIMPLE-VECTOR" ignore)
    ("SINGLE-FLOAT" ignore)
    ("STANDARD-CHAR" ignore)
    ("STREAM" ignore)
    ("STRING" ignore)
    ("STRING-CHAR" ignore)
    ("SYMBOL" ignore)
    ("T" ignore)
    ("VECTOR" ignore)

    ("FTYPE" ignore)
    ("FUNCTION" ignore)
    ("INLINE" ignore)
    ("NOTINLINE" ignore)
    ("IGNORE" ignore)
    ("OPTIMIZE" ignore)
    ("DECLARATION" define-declaration)

    ("*EXPR" ignore)
    ("*FEXPR" ignore)
    ("*LEXPR" ignore)
    )
  "Alist of elements (decl-type-string handler-function)
decl-type-string is a string such as /"SPECIAL/" or /"TYPE/".
The handler-function is called with the declaration and INTERPRETER-ENVIRONMENT as args.")

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN PROCLAIM (&REST DECLARATIONS &AUX STRING)
  "Make DECLARATIONS be in effect globally.
Only SPECIAL declarations make sense to do this way,
and they are better made using DEFVAR or DEFPARAMETER."
  (DOLIST (DECL DECLARATIONS)
    (COND ((OR (ATOM DECL) (NOT (ATOM (CAR DECL))))
	   (FERROR NIL "~S is an invalid declaration" DECL))
	  ((MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
	   (EVAL DECL))
	  ((EQUAL (SETQ STRING (STRING (CAR DECL))) "DECLARATION")
	   (DOLIST (X (CDR DECL))
	     (PUSHNEW `(,STRING IGNORE) INTERPRETER-DECLARATION-TYPE-ALIST
		      			:TEST 'EQUAL :KEY 'CAR)))
	  ((EQUAL STRING "INLINE")
	   )
	  ((EQUAL STRING "NOTINLINE")
	   )
	  ((NOT (ASSOC STRING INTERPRETER-DECLARATION-TYPE-ALIST))
	   (FERROR NIL "~S is an unknown declaration" DECL))))
  NIL)

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer byte byte-expand (dpb) (x)
  (or (when (and (= (length x) 3)
		 (numberp (cadr x))
		 (numberp (caddr x)))
	`(dpb ,(caddr x) ,%%byte-specifier-position ,(cadr x)))
      x))

))


; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(DEFUN TRIVIAL-FORM-P (X)
  (OR (constantp x)
      (SYMBOLP X)))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCP1.."

(DEFUN CHECK-NUMBER-OF-ARGS (FORM &OPTIONAL FUNCTION)
  (IF (NULL FUNCTION) (SETQ FUNCTION (CAR FORM)))
  (LET* (TEM
	 ARGLIST
	 NARGS
	 (MIN NIL)
	 (MAX 0)
	 (ARGS-INFO NIL)
	 (LOCALP NIL)
	 (FN FUNCTION))
    (AND (SYMBOLP FN)
	 ;; If FN is a name defined lexically by FLET or LABELS, use its definition.
	 (SETQ LOCALP (ASSQ FN LOCAL-FUNCTIONS))
	 (SETQ FN (CADDR LOCALP)))
    (FLET ((BAD-ARGUMENTS (MSG &OPTIONAL (TYPE 'WRONG-NUMBER-OF-ARGUMENTS)
			       		 (SEVERITY ':PROBABLE-ERROR))
	      (WARN TYPE SEVERITY (IF LOCALP
				      "Locally defined function ~S called with ~A"
				    "Function ~S called with ~A")
		    (CAR FORM) MSG)))
      (TAGBODY
       TOP
	  (SETQ FN (LAMBDA-MACRO-EXPAND FN))
	  ;; foo. a real problem here. Can't use (arglist fn t) as then we wouldn't get
	  ;; any of the keyword args (it would get "(&rest nil)")
	  (SETQ ARGLIST (IGNORE-ERRORS (ARGLIST FN)))
	  (COND ((AND (CONSP FN) (FUNCTIONP FN T))
		 (UNLESS (CONSP ARGLIST) (RETURN-FROM CHECK-NUMBER-OF-ARGS))
		 (DOLIST (X ARGLIST)
		   (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
			 ((OR (EQ X '&REST) (EQ X '&BODY) (EQ X '&KEY))
			  (UNLESS MIN (SETQ MIN MAX))
			  (SETQ MAX MOST-POSITIVE-FIXNUM)
			  (RETURN))
			 ((EQ X '&AUX) (RETURN))
			 ((MEMQ X LAMBDA-LIST-KEYWORDS))
			 (T (INCF MAX)))))
		((NOT (SYMBOLP FN))
		 ;;Unknown type, don't check
		 (RETURN-FROM CHECK-NUMBER-OF-ARGS))
		((SETQ TEM (GET FN 'ARGDESC))
		 (DOLIST (X TEM)
		   (COND ((MEMQ 'FEF-ARG-REQ (CADR X))
			  (INCF MAX (CAR X)))
			 ((MEMQ 'FEF-ARG-OPT (CADR X))
			  (OR MIN (SETQ MIN MAX))
			  (INCF MAX (CAR X)))
			 ((MEMQ 'FEF-ARG-REST (CADR X))
			  (OR MIN (SETQ MIN MAX))
			  (SETQ MAX MOST-POSITIVE-FIXNUM)))))
		((SETQ TEM (GET FN 'QINTCMP))
		 (SETQ MAX TEM))
		((SETQ TEM (GET FN 'Q-ARGS-PROP))
		 (SETQ ARGS-INFO TEM))
		;; Take care of recursive calls to function being compiled.
		((EQ FN THIS-FUNCTION-ARGLIST-FUNCTION-NAME)
		 (DOLIST (X THIS-FUNCTION-ARGLIST)
		   (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
			 ((OR (EQ X '&REST) (EQ X '&BODY) (EQ X '&KEY))
			  (UNLESS MIN (SETQ MIN MAX))
			  (SETQ MAX MOST-POSITIVE-FIXNUM)
			  (RETURN))
			 ((EQ X '&AUX) (RETURN))
			 ((MEMQ X LAMBDA-LIST-KEYWORDS))
			 (T (INCF MAX)))))
		((FBOUNDP FN)
		 (SETQ TEM (SI:UNENCAPSULATE-FUNCTION-SPEC FN))
		 (COND ((NOT (EQ TEM FN))
			(SETQ FN TEM)
			(GO TOP)))
		 (SETQ TEM (FSYMEVAL FN))
		 (COND ((OR (SYMBOLP TEM) (CONSP TEM))
			(SETQ FN TEM)
			(GO TOP))
		       (T (SETQ ARGS-INFO (%ARGS-INFO TEM)))))
		(T ;;No information available
		 (RETURN-FROM CHECK-NUMBER-OF-ARGS))))
      (WHEN ARGS-INFO
	(SETQ MIN (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
	      MAX (IF (BIT-TEST (LOGIOR %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST)
				ARGS-INFO)
		      MOST-POSITIVE-FIXNUM
		    (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
      (SETQ NARGS (LENGTH (CDR FORM)))	;Now that we know it's not a macro
      (COND ((< NARGS (OR MIN MAX))
	     (BAD-ARGUMENTS "too few arguments."))
	    ((> NARGS MAX)
	     (BAD-ARGUMENTS "too many arguments."))
	    ((CONSP ARGLIST)
	     (LET* ((KEYARGS (MEMQ '&KEY ARGLIST))
		    (KEYFORM (NTHCDR (OR MIN MAX) (CDR FORM))))
	       (WHEN (AND KEYARGS KEYFORM)
		 (COND ((ODDP (LENGTH KEYFORM))
			(BAD-ARGUMENTS "an odd number of keyword//value pairs."))
		       (T (LET ((ALLOW-OTHER-KEYS (OR (MEMQ '&ALLOW-OTHER-KEYS ARGLIST)
						      (GETF KEYFORM ':ALLOW-OTHER-KEYS))))
			    (LOOP FOR KEY IN KEYFORM BY #'CDDR
				  WHEN (EQ (CAR-SAFE KEY) 'QUOTE) DO (SETQ KEY (CADR KEY))
				  DOING (COND ((KEYWORDP KEY)
					       (UNLESS
						 (OR ALLOW-OTHER-KEYS
						     (DOLIST (X KEYARGS)
						       (IF (MEMQ X LAMBDA-LIST-KEYWORDS)
							   NIL
							 (IF 
							   (IF (CONSP X)
							       (IF (CONSP (CAR X))
								   ;; ((:frob foo) bar)
								   (EQ KEY (CAAR X))
								 ;; (foo bar)
								 (STRING= KEY (CAR X)))
							     ;; foo
							     (STRING= KEY X))
							   (RETURN T)))))
						 (BAD-ARGUMENTS
						   (FORMAT NIL "the unrecognized keyword ~S"
							   KEY))))
					      ((CONSTANTP KEY)
					       (BAD-ARGUMENTS
						 (FORMAT NIL "~S appearing where a keyword should" KEY)))))))))))))))

))


; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defoptimizer member		memq-eq)
(defoptimizer member-eql	memq-eq)
(defoptimizer member-equalp	memq-eq)
(defoptimizer memq memq-eq () (form)
  (or (when (= (length form) 3)
	(let ((item (cadr form))
	      (list (caddr form)))
	  (if (quotep list)
	      (selectq (length (cadr list))
		(0 `(progn ,item nil))
		(1 `(and (,(cdr (assq (car form) '((memq . eq)
						   (member . equal)
						   (member-eql . eql)
						   (member-equalp . equalp))))
			  ,item ',(car (cadr list)))
			 ',(cadr list)))))))
      form))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defoptimizer eql eql-eq (eq) (form)
  (or (when (and (= (length form) 3)
		 (or (and (constantp (cadr form))
			  (typep (cadr form) '(or (not number) fixnum short-float)))
		     (and (constantp (caddr form))
			  (typep (caddr form) '(or (not number) fixnum short-float)))))
	`(eq ,(cadr form) ,(caddr form)))
      form))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defoptimizer member-eql member-eql-memq (memq) (form)
  (or (when (= (length form) 3)
	(let ((item (cadr form))
	      (list (caddr form)))
	  (if (or (and (constantp item)
		       (typep item '(or (not number) fixnum short-float)))
		  (and (quotep list)
		       (global:listp (cadr list))
		       (loop for x in (cadr list)
			     always (typep x '(or (not number) fixnum short-float)))))
	      `(memq ,item ,list))))
      form))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defoptimizer MAKE-ARRAY TRY-TO-USE-SIMPLE-MAKE-ARRAY (SI:SIMPLE-MAKE-ARRAY) (form)
  (let ((LEN (LENGTH FORM))
	(DIMENSIONS-FORM NIL)
	(INITIAL-VALUE-FORM NIL)
	(INITIAL-VALUE-SPECIFIED NIL)
	(AREA-FORM NIL)
	(TYPE-FORM ''ART-Q)
	(LEADER-LENGTH-FORM NIL)
	(FILL-POINTER-FORM NIL)
	(FILL-POINTER-SPECIFIED NIL)
	(NAMED-STRUCTURE-SYMBOL-FORM NIL)
	(NAMED-STRUCTURE-SYMBOL-SPECIFIED NIL)
	OUT-OF-ORDER
	STARTFORM)
    (when (OR (< LEN 2) (ODDP LEN))
      (return-from try-to-use-simple-make-array form))
    (SETQ DIMENSIONS-FORM (SECOND FORM))
    (LOOP FOR (KEYWORD-FORM ARGUMENT-FORM) ON (REST2 FORM) BY #'CDDR
	  DO (SELECTQ (if (eq (car-safe keyword-form) 'quote)
			  (cadr keyword-form)
			keyword-form)
	       (:type
		(SETQ TYPE-FORM ARGUMENT-FORM)
		(OR (constantp type-form)
		    (AND (constantp AREA-FORM)
			 (constantp LEADER-LENGTH-FORM)
			 (constantp INITIAL-VALUE-FORM)
			 (constantp FILL-POINTER-FORM)
			 (constantp NAMED-STRUCTURE-SYMBOL-FORM))
		    (SETQ OUT-OF-ORDER T)))
	       (:element-type
		(SETQ TYPE-FORM ARGUMENT-FORM)
		(OR (constantp type-form)
		    (IF (SYMBOLP TYPE-FORM)
			(AND (TRIVIAL-FORM-P AREA-FORM)
			     (TRIVIAL-FORM-P LEADER-LENGTH-FORM)
			     (TRIVIAL-FORM-P INITIAL-VALUE-FORM)
			     (TRIVIAL-FORM-P FILL-POINTER-FORM)
			     (TRIVIAL-FORM-P NAMED-STRUCTURE-SYMBOL-FORM))
		      (AND (CONSTANTP AREA-FORM)
			   (CONSTANTP LEADER-LENGTH-FORM)
			   (CONSTANTP INITIAL-VALUE-FORM)
			   (CONSTANTP FILL-POINTER-FORM)
			   (CONSTANTP NAMED-STRUCTURE-SYMBOL-FORM)))
		    (SETQ OUT-OF-ORDER T))
		;; Note: in general can't evaluate type-form now even if constant, as
		;;  it may refer to types not yet defined by the user.
		(setq type-form `(si:array-type-from-element-type ,type-form)))
	       (:area
		(SETQ AREA-FORM ARGUMENT-FORM)
		(OR (CONSTANTP AREA-FORM)
		    (IF (SYMBOLP AREA-FORM)
			(AND (TRIVIAL-FORM-P LEADER-LENGTH-FORM)
			     (TRIVIAL-FORM-P INITIAL-VALUE-FORM)
			     (TRIVIAL-FORM-P FILL-POINTER-FORM)
			     (TRIVIAL-FORM-P NAMED-STRUCTURE-SYMBOL-FORM))
		      (AND (CONSTANTP LEADER-LENGTH-FORM)
			   (CONSTANTP INITIAL-VALUE-FORM)
			   (CONSTANTP FILL-POINTER-FORM)
			   (CONSTANTP NAMED-STRUCTURE-SYMBOL-FORM)))
		    (SETQ OUT-OF-ORDER T)))
	       (:leader-length
		(SETQ LEADER-LENGTH-FORM ARGUMENT-FORM)
		(OR (CONSTANTP LEADER-LENGTH-FORM)
		    (IF (SYMBOLP LEADER-LENGTH-FORM)
			(AND (TRIVIAL-FORM-P INITIAL-VALUE-FORM)
			     (TRIVIAL-FORM-P FILL-POINTER-FORM)
			     (TRIVIAL-FORM-P NAMED-STRUCTURE-SYMBOL-FORM))
		      (AND (CONSTANTP INITIAL-VALUE-FORM)
			   (CONSTANTP FILL-POINTER-FORM)
			   (CONSTANTP NAMED-STRUCTURE-SYMBOL-FORM)))
		    (SETQ OUT-OF-ORDER T)))
	       ((:initial-value :initial-element)
		(SETQ INITIAL-VALUE-FORM ARGUMENT-FORM INITIAL-VALUE-SPECIFIED T)
		(OR (CONSTANTP INITIAL-VALUE-FORM)
		    (IF (SYMBOLP INITIAL-VALUE-FORM)
			(AND (TRIVIAL-FORM-P FILL-POINTER-FORM)
			     (TRIVIAL-FORM-P NAMED-STRUCTURE-SYMBOL-FORM))
		      (AND (CONSTANTP FILL-POINTER-FORM)
			   (CONSTANTP NAMED-STRUCTURE-SYMBOL-FORM)))
		    (SETQ OUT-OF-ORDER T)))
	       (:fill-pointer
		(SETQ FILL-POINTER-FORM ARGUMENT-FORM FILL-POINTER-SPECIFIED T)
		(OR (CONSTANTP FILL-POINTER-FORM)
		    (IF (SYMBOLP FILL-POINTER-FORM)
			(TRIVIAL-FORM-P NAMED-STRUCTURE-SYMBOL-FORM)
		      (CONSTANTP NAMED-STRUCTURE-SYMBOL-FORM))
		    (SETQ OUT-OF-ORDER T)))
	       (:named-structure-symbol
		(SETQ NAMED-STRUCTURE-SYMBOL-FORM ARGUMENT-FORM
		      NAMED-STRUCTURE-SYMBOL-SPECIFIED T))
	       (OTHERWISE
		(RETURN-FROM try-to-use-simple-make-array FORM))))
    (if OUT-OF-ORDER
	;; Don't optimize if it means exchanging two subforms
	;; which could affect each other.
	form
      (IF FILL-POINTER-SPECIFIED
	  (SETQ LEADER-LENGTH-FORM
		(IF LEADER-LENGTH-FORM
		    `(MAX 1 ,LEADER-LENGTH-FORM)
		  1)))
      (SETQ STARTFORM
	    (COND
	      (INITIAL-VALUE-SPECIFIED
	       `(SI:SIMPLE-MAKE-ARRAY ,DIMENSIONS-FORM ,TYPE-FORM ,AREA-FORM
				      ,LEADER-LENGTH-FORM ,INITIAL-VALUE-FORM))
	      (LEADER-LENGTH-FORM
	       `(SI:SIMPLE-MAKE-ARRAY ,DIMENSIONS-FORM ,TYPE-FORM ,AREA-FORM
				      ,LEADER-LENGTH-FORM))
	      (AREA-FORM
	       `(SI:SIMPLE-MAKE-ARRAY ,DIMENSIONS-FORM ,TYPE-FORM ,AREA-FORM))
	      (T
	       `(SI:SIMPLE-MAKE-ARRAY ,DIMENSIONS-FORM ,TYPE-FORM))))
      (IF (OR FILL-POINTER-SPECIFIED NAMED-STRUCTURE-SYMBOL-SPECIFIED)
	  (LET ((ARRAY-VAR (GENSYM)))
	    `(LET ((,ARRAY-VAR ,STARTFORM))
	       ,(IF FILL-POINTER-SPECIFIED
		    `(SETF (FILL-POINTER ,ARRAY-VAR) ,FILL-POINTER-FORM))
	       ,(IF NAMED-STRUCTURE-SYMBOL-SPECIFIED
		    `(MAKE-ARRAY-INTO-NAMED-STRUCTURE
		       ,ARRAY-VAR ,NAMED-STRUCTURE-SYMBOL-FORM))
	       , ARRAY-VAR))
	STARTFORM))))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN ARRAY-TYPE-FROM-ELEMENT-TYPE (ELEMENT-TYPE &OPTIONAL KLUDGE &AUX TEM)
  (COND ((AND KLUDGE (FIXP ELEMENT-TYPE))
	 (IF (ZEROP (LDB %%ARRAY-TYPE-FIELD ELEMENT-TYPE))
	     (CDR (NTH ELEMENT-TYPE ARRAY-ELEMENT-TYPE-ALIST))
	   (CDR (NTH (LDB %%ARRAY-TYPE-FIELD ELEMENT-TYPE) ARRAY-ELEMENT-TYPE-ALIST))))
	((AND KLUDGE (SETQ TEM (MEMQ ELEMENT-TYPE ARRAY-TYPES)))
	 (CAR TEM))
	((AND KLUDGE (SETQ TEM (FIND-POSITION-IN-LIST ELEMENT-TYPE ARRAY-TYPE-KEYWORDS)))
	 (NTH TEM ARRAY-TYPES))
	(T (SETQ ELEMENT-TYPE (TYPE-CANONICALIZE ELEMENT-TYPE))
	   (OR (CDR (ASSOC ELEMENT-TYPE ARRAY-ELEMENT-TYPE-ALIST))
	       (COND ((SUBTYPEP ELEMENT-TYPE 'FIXNUM)
		      (COND ((SUBTYPEP ELEMENT-TYPE 'BIT) 'ART-1B)	;common case
			    ((SUBTYPEP ELEMENT-TYPE '(MOD #o10))
			     (IF (SUBTYPEP ELEMENT-TYPE '(MOD 4)) 'ART-2B 'ART-4B))
			    ((SUBTYPEP ELEMENT-TYPE '(MOD #o200000))
			     (IF (SUBTYPEP ELEMENT-TYPE '(MOD #o400)) 'ART-8B 'ART-16B))
			    ((SUBTYPEP ELEMENT-TYPE '(SIGNED-BYTE #o20)) 'ART-HALF-FIX)
			    (T 'ART-Q)))
		     ((SUBTYPEP ELEMENT-TYPE 'STRING-CHAR) ART-STRING)
		     ((SUBTYPEP ELEMENT-TYPE 'FLOAT) 'ART-FLOAT)
		     ((SUBTYPEP ELEMENT-TYPE 'COMPLEX)
		      (IF (SUBTYPEP ELEMENT-TYPE '(COMPLEX FLOAT))
			  'ART-COMPLEX-FLOAT
			'ART-COMPLEX))
		     (T 'ART-Q))))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defoptimizer EQ EQ-NIL (not) (form &aux x y)
  (or (when (= (length form) 3)
	(setq x (cadr form) y (caddr form))
        (cond ((null x) `(null ,y))
	      ((null y) `(null ,x))))
      form))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun check-for-name-conflict (string pkg &optional
				not-local-symbols additional-symbol additional-symbol-package
				additional-used-packages)
  (let (candidates shadowed-explicitly-flag)
    (unless not-local-symbols
      (multiple-value-bind (s foundp)
	  (intern-local-soft string pkg)
	(when foundp
	  (if (memq s (pkg-shadowing-symbols pkg))
	      (setq shadowed-explicitly-flag t)
	    (push (list (pkg-name pkg) pkg s) candidates)))))
    (unless shadowed-explicitly-flag
      (when (and additional-symbol
		 (dolist (elt candidates t)
		   (when (eq (caddr elt) additional-symbol) (return nil))))
	(push (list (pkg-name additional-symbol-package)
		    additional-symbol-package additional-symbol)
	      candidates))
      (dolist (p (pkg-use-list pkg))
	(multiple-value-bind (s foundp)
	    (intern-local-soft string p)
	  (when (eq foundp ':external)
	    (dolist (elt candidates t)
	      (when (eq (caddr elt) s) (return nil)))
	    (push (list (pkg-name p) p s) candidates))))
      (dolist (p additional-used-packages)
	(multiple-value-bind (s foundp)
	    (intern-local-soft string p)
	  (when (eq foundp ':external)
	    (dolist (elt candidates t)
	      (when (eq (caddr elt) s) (return nil)))
	    (push (list (pkg-name p) p s) candidates))))
      (and (cdr candidates)
	   candidates))))

))


; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (named-structure type-predicate) (object)
  (not (null (named-structure-p object))))

(defun (named-structure type-optimizer) (expression)
  `(not (null (named-structure-p ,(cadr expression)))))

(defun (named-structure type-expander) ()
  'structure)

))

;;;;;;; all this is for NUMBER and NAMED-STRUCTURE not being globalized. sigh. ;;;;;;;;;;;;
; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defprop atom (array simple-array vector string bit-vector
		     simple-vector simple-bit-vector simple-string
		     standard-char
		     symbol null
		     number rational integer bignum ratio complex
		     float short-float single-float
		     hash-table readtable package pathname stream random-state
		     structure
		     closure entity instance stack-group select locative
		     compiled-function microcode-function)
	 subtypes)

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defprop common (array simple-array vector string bit-vector
		       simple-vector simple-bit-vector simple-string
		       standard-char
		       list symbol cons null
		       number rational integer bignum ratio complex
		       float short-float single-float
		       hash-table readtable package pathname stream random-state
		       structure)
	 subtypes)

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun fixup-type-properties ()
  (dolist (symbol
	    '(array atom bignum bit bit-vector character closure common
	      compiled-function complex cons double-float entity
	      fat-char fix fixnum flonum float hash-table instance integer keyword ;list
	      locative long-float microcode-function null named-structure number
	      package pathname random-state ratio rational readtable real
	      select select-method sequence short-float simple-array simple-bit-vector
	      simple-string simple-vector single-float small-flonum
	      standard-char stream string string-char structure symbol
	      vector))
    (when (get symbol 'type-predicate)
      (setf (get (intern (string symbol) 'keyword) 'type-predicate)
	    (get symbol 'type-predicate)))
    (when (get symbol 'type-optimizer)
      (setf (get (intern (string symbol) 'keyword) 'type-optimizer)
	    (get symbol 'type-optimizer)))
    (when (get symbol 'subtypes)
      (let ((combined (nconc (mapcar #'(lambda (elt) (intern (string elt) 'keyword))
				     (get symbol 'subtypes))
			     (get symbol 'subtypes))))
	(setf (get (intern (string symbol) 'keyword) 'subtypes) combined)
	(setf (get symbol 'subtypes) combined)))
    (when (get symbol 'type-expander)
      (setf (get (intern (string symbol) 'keyword) 'type-expander)
	    (get symbol 'type-expander)))
    (when (get symbol 'type-name)
      (setf (get (intern (string symbol) 'keyword) 'type-name)
	    (get symbol 'type-name))))
  (putprop ':list 'cons 'type-alias-for))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(add-initialization 'fixup-type-properties '(fixup-type-properties) '(once))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defvar subtypep-pairwise-disjoint-sets
  '((integer fixnum bignum)
    (rational ratio integer)
    (number rational float complex)
    (number real complex)
    (list cons null)
    (t cons symbol array number character entity locative instance closure
       stack-group select compiled-function microcode-function)
    (t list number hash-table readtable package pathname stream random-state)))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defun (:property append style-checker) (form)
  (need-two-args form)
  (when (and (= (length form) 3)
	     (global:member (third form) '(nil 'nil)))
    (let ((*print-length* 3) (*print-level* 2))
      (warn 'obsolete ':obsolete "(APPEND ~S NIL) is an obsolete way to copy lists;
   use (COPY-LIST ~S) instead." (cadr form) (cadr form)))))

))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:PS:<MLY.L>QCOPT.."

(defun (:property subst style-checker) (form)
  (when (and (= (length form) 4)		;don't give too many warnings!
	     (global:member (cadr form) '(nil 'nil))
	     (global:member (caddr form) '(nil 'nil)))
    (let ((*print-length* 3) (*print-level* 2))
      (warn 'obsolete ':obsolete "(SUBST NIL NIL ~S) is an obsolete way to copy trees;
   use (COPY-TREE ~S) instead." (cadddr form) (cadddr form)))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defvar type-pretty-name-hash-table (make-hash-table :test 'equal :size 500.)
  "A hash table containing cached pretty names for types")

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun type-pretty-name (type)
  "Return a string containing a nun phrase describing objects of type TYPE."
  (or (gethash type type-pretty-name-hash-table)
      (setf (gethash type type-pretty-name-hash-table)
	    (cond ((symbolp type)
		   (or (get type 'type-name)
		       (string-append-a-or-an
			 (string-subst-char #/space #/-
					    (string-downcase (format nil "~a" type)) nil))))
		  ((and (consp type)
			(funcall (get (car type) 'type-name-function #'ignore) type)))
		  (t (string-append (format nil "an object of type ~S" type)))))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property or type-name-function) (type)
  (when (dolist (elt (cdr type) t)
	  (unless (type-pretty-name elt) (return nil)))
    (string-append
      (format:output nil
	(do ((tail (cdr type) (cdr tail)))
	    ((null tail))
	  (unless (cdr tail)
	    (princ "or "))
	  (princ (type-pretty-name (car tail)))
	  (when (cdr tail)
	    (if (cddr tail)
		(princ ", ")
	      (tyo #/space))))))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property member type-name-function) (type)
  (string-append
    (format:output nil
      (do ((tail (cdr type) (cdr tail)))
	  ((null tail))
	(unless (cdr tail)
	  (princ "or "))
	(prin1 (car tail))
	(when (cdr tail)
	  (if (cddr tail)
	      (princ ", ")
	    (tyo #/space)))))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property integer type-name-function) (type)
  (let ((low (cond ((null (cdr type)) '*)
		   ((consp (cadr type)) (car (cadr type)))
		   ((integerp (cadr type)) (1- (cadr type)))
		   (t (cadr type))))
	(high (cond ((null (cddr type)) '*)
		    ((consp (caddr type)) (car (caddr type)))
		    ((integerp (caddr type)) (1+ (caddr type)))
		    (t (caddr type)))))
    (cond ((and (eq low '*) (eq high '*))
	   "an integer")
	  ((and (eq low -1) (eq high '*))
	   "a positive integer")
	  ((and (eq high 1) (eq low '*))
	   "a negative integer")
	  ((eq high '*)
	   (string-append (format nil "an integer greater than ~D" low)))
	  ((eq low '*)
	   (string-append (format nil "an integer less than ~D" high)))
	  (t
	   (string-append (format nil "an integer between ~D and ~D (exclusive)" low high))))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property float type-name-function) (type)
  (float-type-name-function "float" type))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property short-float type-name-function) (type)
  (float-type-name-function "short float" type))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun float-type-name-function (string type)
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
	   (string-append (format nil "a ~A ~:[~;>~] ~D" string lowex low)))
	  ((eq low '*)
	   (string-append (format nil "a ~A ~:[~;<~] ~D" string highex high)))
	  (t (string-append (format nil "a ~A satisfying ~D ~:[~;<~] ~A ~:[~;<~] ~D"
				    string low lowex string highex high))))))
))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property complex type-name-function) (type)
  (selectq (cadr type)
    ((nil real) "a complex number")
    (rational "a rational complex number")
    (short-float "a complex number with short-float components")
    (single-float "a complex number with single-float components")
    (long-float "a complex number with long-float components")
    (double-float "a complex number with double-float components")
    (float "a complex number with floating-point components")
    (t nil)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defprop complex "a complex number" type-name)
))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN OPEN-CODE-P (SYMBOL)
  (LET ((PROP (GETDECL SYMBOL 'INLINE)))
    (IF (EQ PROP T) (FDEFINITION SYMBOL) PROP)))

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; CLPACK  "

(defun rename-package (pkg new-name &optional new-nicknames)
  "Change the name(s) of a package."
  (unless (packagep pkg)
    (setf pkg (pkg-find-package pkg)))
  (setq new-nicknames (if (cli:listp new-nicknames)
			  (mapcar 'string new-nicknames)
			(list (string new-nicknames)))
	new-name (string new-name))
  (without-interrupts
    (let ((tem (find-package new-name)))
      (when (and tem (neq tem pkg))
	(ferror nil "A package named ~A already exists." new-name)))
    (dolist (nick new-nicknames)
      (let ((tem (find-package nick)))
	(when (and tem (neq tem pkg))
	  (ferror nil "A package named ~A already exists." nick))))
    (setf (pkg-name pkg) new-name)
    (setf (pkg-nicknames pkg) new-nicknames))
  pkg)

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; CLPACK  "

(defun describe-package (pkg)
  "Describes thoroughly the package PKG (a package or the name of one).
The only thing not mentioned is what symbols are in the package.
Use MAPATOMS for that."
    (setq pkg (pkg-find-package pkg))
    (format t "~%Package ~A" (pkg-name pkg))
    (when (pkg-nicknames pkg)
      (princ " (")
      (do ((names (pkg-nicknames pkg) (cdr names))
	   (first t nil))
	  ((null names))
	(unless first (princ ", "))
	(princ (car names)))
      (princ ")"))
    (princ ".")
    (format t "~&   ~D. symbols out of ~D.  Hash modulus = ~D.~&"
	    (pkg-number-of-symbols pkg)
	    (pkg-max-number-of-symbols pkg)
	    (pkg-number-of-slots pkg))
    (when (pkg-refname-alist pkg)
      (format t "Refname alist:~%")
      (dolist (l (pkg-refname-alist pkg))
	(format t "   ~20A~S~%" (car l) (cdr l))))
    (format t "~@[Packages which USE this one:~&~{   ~A~&~}~]" (pkg-used-by-list pkg))
    (format t "~@[Super-package ~A~&~]" (pkg-super-package pkg t))
    (format t "~@[Packages which are USEd by this one:~&~{   ~A~&~}~]" (pkg-use-list pkg))
    (format t "~@[Shadowed symbols:~&~{   ~S~&~}~]" (pkg-shadowing-symbols pkg))
    (format t "~@[Symbols are interned in this package using( ~S~&~]" (pkg-store-function pkg))
    (format t "~@[Symbols interned in this package are automatically exported.~%~]"
	    (pkg-auto-export-p pkg))
    (format t "~@[Additional properties of this package:~%~{   ~S:~33T~S~%~}~]"
	    (pkg-plist pkg))
    pkg)

))

; From file CLPACK.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; CLPACK  "

(defun make-package (name &key &optional nicknames (use '("GLOBAL")) super (size 200)
		     ((nil ignore)) shadow export prefix-name auto-export-p invisible
		     import shadowing-import import-from
		     relative-names relative-names-for-me
		     ((:hash-inherited-symbols ignore))
		     (external-only nil external-only-p)
		     properties
		     ((:include ignore)) ((:colon-mode ignore)))
  (declare (arglist name &key &optional nicknames (use '("GLOBAL")) (size 200) shadow super
		    export prefix-name auto-export-p invisible
		    import shadowing-import import-from
		    relative-names relative-names-for-me properties))
  "Creates and returns a package named NAME with nicknames NICKNAMES.
If any of these names already exists as a global name, we err.
Other keywords:
:USE specifies a list of names of packages for this one to USE-PACKAGE.
:SUPER specifies a superpackage (USE that package, and all the ones it USEs).
:SHADOW specifies a list of names of symbols to shadow in this package.
:EXPORT specifies a list of names of symbols to export in this package.
:IMPORT specifies a list of symbols to import in this package.
:IMPORT-FROM specifies a list of a package and names of symbols
 to import in this package from that package.
:SHADOWING-IMPORT specifies a list of symbols to import in this package,
 overriding any name conflicts.
:RELATIVE-NAMES specifies a list of pairs (NICKNAME . PACKAGENAME)
 specifying local nicknames to be in effect in this package.
:RELATIVE-NAMES-FOR-ME specifies a list of pairs (PACKAGENAME NICKNAME)
 specifying local nicknames to be in effect in those packages, for this one.
:AUTO-EXPORT-P non-NIL specifies that all symbols placed in this package
 should be exported automatically at that time.
:PREFIX-NAME specifies string to print for this package when printing prefixes.
:SIZE specifies the number of symbols to allocate space for initially.
:INVISIBLE non-NIL means do not record this package in table of package names."
  (or (null nicknames) (listp nicknames) (setq nicknames (list nicknames)))
  (let* ((table-size (pkg-good-size size))
	 (default-cons-area working-storage-area)
	 (pkg (pkg-make-package
		:make-array (:length (list table-size 2)
				     :area pkg-area)
		:pkg-name (string name)
		:pkg-nicknames (mapcar 'string nicknames)
		:pkg-prefix-print-name prefix-name
		:pkg-all-packages-pointer '*all-packages*
		:pkg-number-of-symbols 0
		:pkg-max-number-of-symbols size
		:pkg-plist properties)))
    (when super
      (setq super (pkg-find-package super))
      (or (eq super pkg-global-package)
	  (eq super pkg-system-package)
	  (progn
	    (pkg-mark-having-subpackages super)
	    (putprop (locf (pkg-plist pkg)) super 'super-package))))
    (unless (or (null prefix-name)
		(equal prefix-name name)
		(member prefix-name nicknames))
      (ferror nil "The prefix print name ~A for package ~A is not one of its nicknames."
	      prefix-name name))
    (unless invisible
      (without-interrupts
	(when (find-package name)
	  (ferror nil "A package named ~A already exists." name))
	(dolist (nick nicknames)
	  (when (find-package nick)
	    (ferror nil "A package named ~A already exists." nick)))
	(push pkg *all-packages*)))
    ;; Now do any requested shadowing, using or exporting
    ;; and kill this package if the user aborts.
    (let (success)
      (unwind-protect
	  (progn
	    (use-package use pkg)
	    (when super
	      (use-package (list super) pkg)
	      (use-package (pkg-use-list (pkg-find-package super)) pkg))
	    (shadowing-import shadowing-import pkg)
	    (import import pkg)
	    (dolist (name (cdr import-from))
	      (import (intern (string name) (car import-from)) pkg))
	    (shadow shadow pkg)
	    (export export pkg)
	    (when relative-names
	      (setf (pkg-refname-alist pkg)
		    (loop for (nick p) in relative-names
			  collect (cons (string nick)
					(find-package p)))))
	    (dolist (elt relative-names-for-me)
	      (pkg-add-relative-name (car elt) (cadr elt) pkg))
	    (when (if external-only-p external-only auto-export-p)
	      (pkg-mark-having-subpackages pkg))
	    (setq success t))
	(unless success
	  (setq *all-packages* (delq pkg *all-packages*)))))
    pkg))

))


; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFSUBST MAKE-STRING (LENGTH &REST KEYWORD-ARGS)
  "Creates and returns a string of LENGTH elements, all set to INITIAL-VALUE.
If INITIAL-VALUE is not supplied, the elements contain the character with code 0."
  (DECLARE (ARGLIST LENGTH &KEY &OPTIONAL INITIAL-ELEMENT &ALLOW-OTHER-KEYS))
  (APPLY 'MAKE-ARRAY LENGTH ':TYPE ART-STRING KEYWORD-ARGS))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN MAKE-ARRAY (DIMENSIONS &REST OPTIONS)
  "Create an array of size DIMENSIONS (a number or list of numbers).
The keywords are as follows:
:TYPE - specify array type, controlling type of elements allowed.  Default is ART-Q.
 ART-Q (any elements), ART-Q-LIST (any elements, and the contents looks like a list),
 ART-STRING (elements 0 through 255, printed with quotes),
 ART-FAT-STRING (16 bit unsigned elements, printed with quotes),
 ART-1B (elements 0 and 1), ART-2B (elements 0 through 3), ART-4B, ART-8B, ART-16B,
 ART-32B (elements any fixnum), ART-FLOAT (elements any full-size flonum),
 ART-COMPLEX (elements any number including complex numbers),
 ART-COMPLEX-FLOAT (elements complex numbers composed of two full-size flonums),
 ART-HALF-FIX (16 bit signed fixnum elements),
 ART-FPS-FLOAT ART-COMPLEX-FPS-FLOAT (used with floating point array processor),
 ART-STACK-GROUP-HEAD, ART-REGULAR-PDL, ART-SPECIAL-PDL (parts of stack groups).
:ELEMENT-TYPE - specify array type by specifying Common Lisp
 data type of elements allowed.  For example,
 an :ELEMENT-TYPE of (MOD 4) would get an ART-2B array.
:AREA - specify area to create the array in.
:LEADER-LENGTH - specify number of elements of array leader to make.
:LEADER-LIST - list whose elements are used to initialize the leader.
:FILL-POINTER - specify initial fill pointer value (ARRAY-ACTIVE-LENGTH of the array).
 Requests a leader of length 1 and specifies the contents of the slot.
:INITIAL-ELEMENT - value used to initialize all elements of the array.
:DISPLACED-TO - array, locative or fixnum specifying address of data
 that this array should overlap.
:DISPLACED-INDEX-OFFSET - if displaced to another array, this specifies
 which element of that array should correspond to element 0 of the new one.
:NAMED-STRUCTURE-SYMBOL - if not NIL, specifies a named structure symbol
 to be stored in the array, which should have its named-structure bit set.
:INITIAL-CONTENTS - value is a sequence of sequences of sequences...
 where the leaves are the values to initialize the array from.
 The top level of sequence corresponds to the most slowly varying subscript."
  (DECLARE (ARGLIST DIMENSIONS &KEY ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
		    		    FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET
				    TYPE AREA LEADER-LENGTH LEADER-LIST NAMED-STRUCTURE-SYMBOL
				    ADJUSTABLE))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
	ENTRIES-PER-Q
	LEADER-LIST FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET NAMED-STRUCTURE-SYMBOL
	(AREA NIL) (TYPE 'ART-Q) TYPE-P ELEMENT-TYPE-P
	INITIAL-ELEMENT INITIAL-ELEMENT-P INITIAL-CONTENTS INITIAL-CONTENTS-P
	ARRAY N-DIMENSIONS INDEX-LENGTH LONG-ARRAY-P LEADER-QS DATA-LENGTH LEADER-LENGTH)
    ;; Figure out whether it is old-style.
    (COND ((AND ( LENGTH-OF-OPTIONS 2)
		(OR (NUMBERP (FIRST OPTIONS))
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPES)
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPE-KEYWORDS)))
	   ;; It is old-style.  The first arg is actually AREA.
	   (SETQ AREA DIMENSIONS)
	   (SETQ TYPE (FIRST OPTIONS))
	   (SETQ DIMENSIONS (SECOND OPTIONS))
	   (SETQ DISPLACED-TO (THIRD OPTIONS))
	   (SETQ LEADER-LIST (FOURTH OPTIONS))
	   (SETQ DISPLACED-INDEX-OFFSET (FIFTH OPTIONS))
	   (SETQ NAMED-STRUCTURE-SYMBOL (SIXTH OPTIONS))
	   (IF (NUMBERP LEADER-LIST)
	       (SETQ LEADER-LENGTH LEADER-LIST
		     LEADER-LIST NIL)
	     (SETQ LEADER-LIST (REVERSE LEADER-LIST))))
	  (T
	   ;; It is new-style.
	   (UNLESS (EVENP LENGTH-OF-OPTIONS)
	     (FERROR NIL "Odd-length options list: ~S" OPTIONS))
	   (DO ((O OPTIONS (CDDR O)))
	       ((NULL O))
	     (LET ((VALUE (CADR O)))
	       (SELECTQ (CAR O)
		 (:AREA (SETQ AREA VALUE))
		 (:TYPE (SETQ TYPE-P T)
			(SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE T)))
		 (:ELEMENT-TYPE (SETQ ELEMENT-TYPE-P T)
		  (SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE)))
		 (:DISPLACED-INDEX-OFFSET
		  (SETQ DISPLACED-INDEX-OFFSET VALUE))
		 (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
		 ((:INITIAL-ELEMENT :INITIAL-VALUE)
		  (SETQ INITIAL-ELEMENT VALUE INITIAL-ELEMENT-P T))
		 (:INITIAL-CONTENTS
		  (SETQ INITIAL-CONTENTS VALUE INITIAL-CONTENTS-P T))
		 (:FILL-POINTER (SETQ LEADER-LENGTH (MAX 1 (OR LEADER-LENGTH 1)))
				(SETQ FILL-POINTER VALUE))
		 (:ADJUSTABLE)
		 (:LEADER-LIST (SETQ LEADER-LIST VALUE))
		 (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
;		 (:OLD-LEADER-LENGTH-OR-LIST (IF (NUMBERP VALUE)
;						 (SETQ LEADER-LENGTH VALUE)
;					       (SETQ LEADER-LIST (REVERSE VALUE))))
		 (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
		 (OTHERWISE
		  (FERROR NIL "~S is not a known MAKE-ARRAY keyword." (FIRST OPTIONS))))))))
    (IF (AND TYPE-P ELEMENT-TYPE-P)
	(FERROR NIL "Both :TYPE and :ELEMENT-TYPE specified."))
    (IF (AND DISPLACED-INDEX-OFFSET (NOT DISPLACED-TO))
	(FERROR NIL "The :DISPLACED-INDEX-OFFSET option specified without :DISPLACED-TO."))
    (IF (AND INITIAL-ELEMENT-P INITIAL-CONTENTS-P)
	(FERROR NIL "Both :INITIAL-ELEMENT and :INITIAL-CONTENTS specified."))
    ;; Process the DIMENSIONS argument.
    (CHECK-ARG-TYPE DIMENSIONS (OR LIST FIXNUM)
	       "a fixnum or a list")
    (COND ((FIXNUMP DIMENSIONS)
	   (IF (MINUSP DIMENSIONS)
	       (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	   (SETQ N-DIMENSIONS 1
		 INDEX-LENGTH DIMENSIONS))
	  ((OR (NULL DIMENSIONS) (CONSP DIMENSIONS))
	   (DOLIST (DIM DIMENSIONS)
	     (UNLESS (FIXNUMP DIM)
	       (FERROR NIL "The dimension ~S is not a fixnum." DIM))
	     (IF (MINUSP DIM)
		 (FERROR NIL "The negative array dimension ~S is illegal." DIM)))
	   (SETQ N-DIMENSIONS (LENGTH DIMENSIONS))
	   (IF (> N-DIMENSIONS ARRAY-RANK-LIMIT)
	       (FERROR NIL "Arrays may only at most ~D, dimensions, not ~S"
		       ARRAY-RANK-LIMIT N-DIMENSIONS))
	   (SETQ INDEX-LENGTH (APPLY 'TIMES DIMENSIONS))))
    ;; Process the DISPLACED-TO argument.
    (CHECK-ARG DISPLACED-TO
	       (OR (NULL DISPLACED-TO) (FIXNUMP DISPLACED-TO) (ARRAYP DISPLACED-TO)
		   (LOCATIVEP DISPLACED-TO))
	       "NIL, a fixnum, a locative, or an array")
    ;; See whether this is a "short" or "long" format array.
    (IF (AND (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (NOT DISPLACED-TO))
	(SETQ LONG-ARRAY-P T))
    (OR (= (%DATA-TYPE INDEX-LENGTH) DTP-FIX)
	(FERROR NIL "Attempt to make array too large; total length ~S" INDEX-LENGTH))
    ;; Process the LEADER and NAMED-STRUCTURE-SYMBOL arguments.
    (CHECK-TYPE LEADER-LIST LIST)
    (AND (NULL LEADER-LENGTH) (NOT (NULL LEADER-LIST))
	 (SETQ LEADER-LENGTH (LENGTH LEADER-LIST)))
    (IF (AND LEADER-LENGTH (> (LENGTH LEADER-LIST) LEADER-LENGTH))
	(FERROR NIL "Length of leader initialization list is greater than leader length"))
    (COND (NAMED-STRUCTURE-SYMBOL
	   (IF LEADER-LENGTH
	       (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 1) "greater than 1")
	     (UNLESS (= N-DIMENSIONS 1)
	       (FERROR NIL "A named-structure array may not be ~S-dimensional"
		       N-DIMENSIONS))))
	  (LEADER-LENGTH
	   (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 0) "greater than zero")))
    (SETQ LEADER-QS (IF LEADER-LENGTH
			(+ 2 LEADER-LENGTH)
		      0))
    ;; Process the TYPE argument.
    (CHECK-ARG TYPE (OR (FIXNUMP TYPE)
			(MEMQ TYPE ARRAY-TYPES)
			(MEMQ TYPE ARRAY-TYPE-KEYWORDS))
	       "an array type")
    (IF (FIXNUMP TYPE)
	;may be either a small integer, which is shifted over into the type field,
	; or an already shifted over quantity (such as ART-Q, etc).
	(IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
	    (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
      (IF (FIXNUMP (SYMEVAL TYPE))
	  (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD (SYMEVAL TYPE)))
	(SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))))
    (SETQ ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE))
    ;; This is positive if there are 1 or more entries per Q.  It is
    ;; negative if there are more than one Qs per entry.
    (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			  (CEILING INDEX-LENGTH ENTRIES-PER-Q)
			(* INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
    ;; Process the DISPLACED-INDEX-OFFSET argument.
    (CHECK-ARG DISPLACED-INDEX-OFFSET
	       (OR (NULL DISPLACED-INDEX-OFFSET)
		   (AND (= (%DATA-TYPE DISPLACED-INDEX-OFFSET) DTP-FIX)
			(NOT (MINUSP DISPLACED-INDEX-OFFSET))))
	       "NIL or a non-negative fixnum")
    (LET ((HEADER-WORD
	    ;; Put in array type and number of dims.
	    (%LOGDPB N-DIMENSIONS %%ARRAY-NUMBER-DIMENSIONS
		     (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
      ;; If there is a leader, set the flag.
      (IF LEADER-LENGTH
	  (SETQ HEADER-WORD (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-WORD)))
      (SETQ HEADER-WORD
	    (COND (DISPLACED-TO
		   ;; Array is displaced; turn on the bit, and the array is 2 long
		   ;; plus one for the index-offset if any.
		   (+ (%LOGDPB 1 %%ARRAY-DISPLACED-BIT HEADER-WORD)
		      (IF DISPLACED-INDEX-OFFSET 3 2)))
		  (LONG-ARRAY-P
		   ;; It is local; if it is a long array, the length is not in the
		   ;; header at all; set the bit instead.
		   (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-WORD))
		  (T
		   ;; It is a short array; the length is in the header.
		   (+ INDEX-LENGTH HEADER-WORD))))
      ;; Create the array.
      (SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-WORD
						  INDEX-LENGTH
						  (OR LEADER-LENGTH 0)
						  AREA
						  (+ (MAX 1 N-DIMENSIONS)
						     LEADER-QS
						     (COND (DISPLACED-TO
							    (IF DISPLACED-INDEX-OFFSET 3 2))
							   (LONG-ARRAY-P
							    (1+ DATA-LENGTH))
							   (T DATA-LENGTH)))
						  )))
    (WHEN (CONSP DIMENSIONS)
      ;; It is a multi-dimensional array.  Fill in the "dope vector".
      ;; If last index varies fastest, put in all but first dimension,
      ;; and the last dimensions come last.
      (DO ((DIMLIST (CDR DIMENSIONS) (CDR DIMLIST))
	   (I (+ N-DIMENSIONS (IF LONG-ARRAY-P 0 -1)) (1- I)))
	  ((NULL DIMLIST))
	(%P-STORE-CONTENTS-OFFSET (CAR DIMLIST) ARRAY I)))
    (COND (DISPLACED-TO
	   ;; It is displaced.  Put information after the dope vector, and after
	   ;; the "long array" word if any.
	   (LET ((IDX (IF LONG-ARRAY-P (1+ N-DIMENSIONS) N-DIMENSIONS)))
	     (%P-STORE-CONTENTS-OFFSET DISPLACED-TO ARRAY IDX)
	     (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ IDX))
	     (COND (DISPLACED-INDEX-OFFSET
		    ;; Index offset feature is in use.
		    ;; Store the index offset in the next Q.
		    (%P-STORE-CONTENTS-OFFSET DISPLACED-INDEX-OFFSET ARRAY (+ IDX 2)))))))
    ;; The leader's initial values were specified.
    (DO ((I 0 (1+ I))
	 (LEADER-LIST LEADER-LIST (CDR LEADER-LIST)))
	((NULL LEADER-LIST))
      (SETF (ARRAY-LEADER ARRAY I) (CAR LEADER-LIST)))
    (AND FILL-POINTER
	 (SETF (FILL-POINTER ARRAY) FILL-POINTER))
    ;; Cretinism associated with make-array, in that the leader list can overlap
    ;; with the name-structure slot, which is how fasd dumps the named-structure-symbol
    ;; So we check for the symbol being t and not smash it in that case
    (WHEN NAMED-STRUCTURE-SYMBOL
      (IF (NULL LEADER-LENGTH)
	  ;; There is no leader; put it in element zero of the body.
	  (SETF (AREF ARRAY 0) NAMED-STRUCTURE-SYMBOL)
	;; There is a leader; use element one of the leader.
	(IF (NEQ NAMED-STRUCTURE-SYMBOL T)
	    (SETF (ARRAY-LEADER ARRAY 1) NAMED-STRUCTURE-SYMBOL)))
      ;; It is a named structure.  Set the flag.
      (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0))
    (AND INITIAL-ELEMENT-P (NOT DISPLACED-TO)
	 (ARRAY-INITIALIZE ARRAY INITIAL-ELEMENT))
    (WHEN INITIAL-CONTENTS-P
      (FILL-ARRAY-FROM-SEQUENCES ARRAY INITIAL-CONTENTS 0 0))
    ;; If there is a fill pointer on an art-q-list array, then it should control
    ;; the length of the list as well.  See array-push and array-pop.
    (WHEN (AND (= N-DIMENSIONS 1)
	       (OR FILL-POINTER (NOT (NULL LEADER-LIST)))
	       ;; The cold load generator's frame builder isn't smart enough for a #, here.
	       (= TYPE #|'#,|# (LDB %%ARRAY-TYPE-FIELD ART-Q-LIST)))
      (OR FILL-POINTER (SETQ FILL-POINTER (CAR LEADER-LIST)))
      (WHEN (AND (FIXNUMP FILL-POINTER)
		 (> FILL-POINTER 0)
		 (< FILL-POINTER (ARRAY-LENGTH ARRAY)))
	(%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- FILL-POINTER)))))
    (VALUES ARRAY DATA-LENGTH)))

))


(setf (documentation 'LOAD-PATHNAME-DEFAULTS 'variable)
  "Now the same as *default-pathname-defaults*
Used to be used as the pathname-defaults list for LOAD, COMPILE-FILE, etc.")
