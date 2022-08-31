;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 98.5
;;; Reason: LOAD-AND-SAVE-PATCHES lets you specify band to save on as a fixnum
;;; Reading uninterned symbols bug
;;; STRING-PLURALIZE bug
;;; (TYPEP foo 'COMPLEX) typo
;;; (TYPEP foo 'STRING-CHAR) optimizes in compiler
;;; New functions:
;;;   SEND-IF-HANDLES
;;;   LEXPR-SEND-IF-HANDLES
;;;   OPERATION-HANDLED-P
;;; Brand s compatible functions:
;;;   CHAR-FLIPCASE
;;;   STRING-FLIPCASE -- with bd incompatible inconsistent arguments
;;;   CHAR-STANDARD -- has NO EFFECT on system string functions.
;;;                    Takes one arg and returns T
;;; Stepper hacks lexical macro and function definitions
;;; TRACE and ADVISE encapsulation code miscompiled
;;; (PUSHNEW FOO BAR #'TEST) optimizes (used to only optimize 'TEST)
;;; Closures print more informatively
;;; LOAD-PATCHES only tries to load translations file for logical
;;;   hosts created by FS:MAKE-LOGICAL-PATHNAME-HOST
;;; Written 9/21/84 00:05:09 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.2, CADR 4.0, Experimental ZMail 54.0, MIT-Specific 23.0, microcode 320, GC@2.


(eval-when (load compile eval)
  (globalize (intern "OPTIMIZE" 'si))
  (globalize (intern "CHAR-STANDARD" 'si))
  (globalize (intern "CHAR-FLIPCASE" 'si))
  (globalize (intern "STRING-FLIPCASE" 'si))
  (globalize (intern "DEFINE-PROMPT-AND-READ-TYPE" 'si))
  (globalize (intern "LEXPR-SEND-IF-HANDLES" 'si))
  (globalize (intern "OPERATION-HANDLED-P" 'si))
  (globalize (intern "READ-OR-END" 'si))
  (globalize (intern "SEND-IF-HANDLES" 'si))
  (globalize (intern "FSYMEVAL-IN-ENVIRONMENT" 'si) 'sys)
  (globalize (intern "MACRO-IN-ENVIRONMENT-P" 'si) 'sys))

(load "sys:fonts;narrow" :set-default-pathname nil :verbose nil)

si:(setf (pttbl-prinlevel initial-readtable) "#"
	 (pttbl-prinlevel standard-readtable) "#")

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-AND-SAVE-PATCHES (&OPTIONAL BAND &REST KEYWORD-ARGS)
  "Load all patches and save a new Lisp world in a disk partition.
KEYWORD-ARGS are passed to LOAD-PATCHES.
BAND is the name or number of a LOD band to save in."
  (CHECK-TYPE BAND (OR NUMBER STRING) "A specifier for a band")
  (IF (OR (MEMQ :FORCE-UNFINISHED KEYWORD-ARGS)
	  (MEMQ :UNRELEASED KEYWORD-ARGS))
      (FERROR NIL ":FORCE-UNFINISHED and :UNRELEASED are not reasonable arguments here."))
  (DOLIST (PATCH-SYSTEM PATCH-SYSTEMS-LIST)
    (WHEN (EQ (PATCH-STATUS PATCH-SYSTEM) :INCONSISTENT)
      (BEEP)
      (FORMAT *QUERY-IO* "~&You have loaded patches out of sequence,
 or loaded unreleased patches, in ~A.
As a result, the environment is probably inconsistent with the
current patches and will remain so despite attempts to update it.
Unless you understand these problems well and know how to
be sure whether they are occurring, or how to clean them up,
you should not save this environment."
	      (PATCH-NAME PATCH-SYSTEM))
      (SEND *QUERY-IO* :CLEAR-INPUT)
      (UNLESS (YES-OR-NO-P "Dump anyway? ")
	(RETURN-FROM LOAD-AND-SAVE-PATCHES NIL))))
  (DO ((BAND1 BAND (PROMPT-AND-READ :STRING "~&Save into which band? "))
       (COUNT 0 (1+ COUNT)))
      (())
    (WHEN BAND1
      (COND ((NUMBERP BAND1)
	     (SETQ BAND1 (FORMAT NIL "LOD~D" BAND1)))
	    ((PARSE-NUMBER BAND1 0 NIL NIL T)
	     (SETQ BAND1 (STRING-APPEND "LOD" BAND1))))
      (COND ((NOT (STRING-EQUAL BAND1 "LOD" :END1 3))
	     (FORMAT *QUERY-IO* "~&You must save into a LOD partition."))
	    ((NOT (FIND-DISK-PARTITION BAND1))
	     (FORMAT *QUERY-IO* "~&No such band: ~A." BAND1))
	    ((FIND-DISK-PARTITION-FOR-WRITE BAND1)
	     ;; Non-NIL means user gave confirmation.
	     (SETQ BAND BAND1)
	     (RETURN))))
    (IF (ZEROP COUNT) (PRINT-DISK-LABEL)))
  (WITH-SYS-HOST-ACCESSIBLE
    (COND ((APPLY #'LOAD-PATCHES :NOSELECTIVE KEYWORD-ARGS)
	   (DISK-SAVE BAND T))
	  (T (FORMAT *QUERY-IO* "~&No patches have been made.")))))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN READ-UNINTERNED-SYMBOL (STRING)
  (VALUES (MAKE-SYMBOL STRING T) NIL T))	;Last value T avoids error in XR-READ-SYMBOL.

))

; From file STRING.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(defun string-pluralize (string)
  "Return a plural form of STRING.
Attempts to preserve the case-pattern in STRING."
  (coerce-string-arg string)
  (if (equal string "")
      ""
    (let* (flush add
	   (last-char-raw (char string (1- (string-length string))))
	   (last-char (char-upcase last-char-raw))
	   (last-char-lc-flag (char last-char last-char-raw))
	   (penult-char (char-upcase (if (> (string-length string) 1)
					 (char string (- (string-length string) 2))
				         0)))
	   (last-3 (substring string (max 0 (- (string-length string) 3)))))
      (cond ((and (char-equal last-char #/Y)
;character lossage assumes font=0
		  (not (memq penult-char '(#/A #/E #/I #/O #/U))))
	     (setq flush 1 add "ies"))
	    ((or (string-equal string "ox") (string-equal string "vax"))
	     (setq add "en"))
	    ((or (and (eq last-char #/H)
		      (memq penult-char '(#/C #/S)))
		 (memq last-char '(#/S #/Z #/X)))
	     (setq add "es"))
	    ((string-equal last-3 "man")
	     (setq flush 2 add "en"))
	    ((string-equal last-3 "fan")
	     (setq flush 2 add "en"))
	    ((string-equal last-3 "ife")
	     (setq flush 2 add "ves"))
	    (t (setq add "s")))
      (and flush (setq string (substring string 0 (- (string-length string) flush))))
      (cond (add (string-append string
				(cond (last-char-lc-flag add)
				      (t (string-upcase add)))))
	    (t string)))))

))

; From file QMISC.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defun describe-flonum (x)
  (format t "~%~S is a flonum.~%  " x)
  (format t "Excess-~O exponent #o~O, ~D-bit mantissa #o~O (~:[including sign bit~;with sign bit deleted~])"
	  single-float-exponent-offset
	  (%single-float-exponent x)
	  single-float-mantissa-length
	  (%single-float-mantissa x)
	  single-float-implicit-sign-bit-p))

))

; From file TYPES.LISP OZ:<L.SYS> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property complex type-predicate) (object &optional (type '*))
  (and (complexp object)
       (or (memq type '(* non-complex-number))
	   (and (typep (%complex-real-part object) type)
		(or (memq type '(float single-float short-float double-float long-float))
		    (typep (%complex-imag-part object) type))))))

))

; From file TYPES.LISP OZ:<L.SYS> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property string-char type-optimizer) (expression)
  (let ((object (cadr expression)))
    (once-only (object)
      `(and (characterp ,object)
	    (string-char-p ,object)))))  

))

; From file CHARACTER.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun char-flipcase (char)
  "If CHAR is an uppercase character, return it's lowercase conterpart, and vice-versa.
Returns CHAR unchanged if CHAR is neither upper now lower case."
  (cond ((upper-case-p char) (char-downcase char))
	((lower-case-p char) (char-upcase char))
	(t char)))

))

; From file STRING.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(defun string-flipcase (string &optional (start 0) end (copy-p t))
  "Invert the case (upperlower) of characters in STRING.
Does not affect characters which are not alphabetic.
Symbol*cs braindamage means that this does not take the same argument pattern as
STRING-UP//DOWNCASE."
  (if copy-p (setq string (string-append string)))
  (do ((len (or end (array-active-length string)))
       (char)
       (i start (1+ i)))
      ((= i len))
    (setq char (char string i))
    (cond ((upper-case-p char) (setf (char string i) (char-downcase char)))
	  ((lower-case-p char) (setf (char string i) (char-upcase char)))))
  string)

))

; From file LMMAC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defsubst send-if-handles (object operation &rest arguments)
  "Send the message OPERATION to OBJECT if OBJECT handles that message.
If it does, return the result of sending it that message on the given ARGUMENTS.
Otherwise, return NIL."
  (lexpr-funcall object :send-if-handles operation arguments))

))

; From file LMMAC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defsubst lexpr-send-if-handles (object operation &rest arguments)
  "Send the message OPERATION to OBJECT if OBJECT handles that message.
If it does, return the result of sending it that message on the given ARGUMENTS.
Otherwise, return NIL."
  (lexpr-funcall #'lexpr-funcall object :send-if-handles operation arguments))

))

; From file LMMAC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defsubst operation-handled-p (object &rest operation)
  "Non-NIL if OBJECT has a method defined for OPERATION."
  (lexpr-funcall object :opeeration-handled-p operation))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun interpreter-fsymeval (symbol &aux mumble)
  (dolist (frame *interpreter-function-environment*
		 (symbol-function symbol))
    (and (setq mumble (get-location-or-nil (locf frame) (locf (symbol-function symbol))))
	 (return (car mumble)))))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun sys:fsymeval-in-environment (symbol environment &optional (check-symbol-function t)
				    		       &aux mumble)
  "Returns SYMBOL's function or macro definition within ENVIRONMENT,
If CHECK-SYMBOL-FUNCTION is T we take SYMBOL-FUNCTION of SYMBOL if the function is not
defined by ENVIRONMENT, otherwise we return NIL if the environment doesn't define it."
  (dolist (frame (car environment) (if check-symbol-function (symbol-function symbol) nil))
    (and (setq mumble (get-location-or-nil (locf frame) (locf (symbol-function symbol))))
	 (return (car mumble)))))

))

; From file QCDEFS.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(defun fsymeval-in-function-environment (symbol &optional (fenv *function-environment*))
  "Returns SYMBOL's function or macro definition within the function environment FENV,
or NIL if it is not defined *by* the environment."
  (with-stack-list (env fenv)
    (fsymeval-in-environment symbol env nil)))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun sys:macro-in-environment-p (symbol &optional environment &aux tem)
  "Returns SYMBOL's macroexpansion function if it is defined as a macro (either
within ENVIRONMENT or gloablly), or NIL if it does not have a macro definition"
  (if (setq tem (fsymeval-in-environment symbol environment nil))
      (if (eq (car-safe tem) 'macro) (cadr tem))
    (and (fboundp symbol)
	 (eq (car-safe (setq tem (symbol-function symbol))) 'macro)
	 tem)))

))

; From file STEP.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-macro-form-p (form environment)
  (and (consp form)
       (symbolp (car form))
       (sys:macro-in-environment-p (car form) environment)))

))

; From file STEP.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-print-form (form level apply-p environment)
  (terpri)
  (do ((n (* 2 level) (1- n)))
      ((= n 0))
    (write-char #\sp))
  (write-char (cond (apply-p #\)
		    ((step-macro-form-p form environment) #\)
		    (t #\)))
  (write-char #\sp)
  (if apply-p
      (progn (print-truncated (function-name (car form)) 75.)
	     (princ ": ")
	     (print-elements-truncated (cdr form) 90. 75.))
      (print-truncated form 75.)))

))

; From file STEP.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-cmdr (form values print-form-p apply-p environment)
  (declare (special apply-p))
  (prog (ch ch1
	 (*standard-input* *query-io*)
	 (*standard-output* *query-io*))
	(if *step-auto*
	    (if (eq *step-auto* 'no-print)
		(progn (setq *step-max* (1+ *step-level*)) (return 'evalhook))))
	(and print-form-p
	     (step-print-form form *step-level* apply-p environment))
     pv (do ((l values (cdr l))
	     (ch #\ #\))
	    ((null l))
	  (terpri-if-insufficient-space 80.)
	  (write-char #\sp) (write-char ch) (write-char #\sp)
	  (print-truncated (car l) 98.))	;Several windows lose if this is 100.
     rd (setq ch1 (if *step-auto* #\c-N (read-char *standard-input*)))
	(setq ch (char-upcase ch1))
	(cond ((= ch #\call) (break "from stepper."))
	      ((= ch #\sp) (setq *step-max* *step-level*) (return 'eval))
	      ((= ch #\c-U) (setq *step-max* (max 0 (1- *step-level*))) (return 'eval))
	      ((= ch #\c-N) (setq *step-max* (1+ *step-level*)) (return 'evalhook))
	      ((= ch #\c-X) (setq *step-max* -1) (return 'eval))
	      ((and (= ch #\c-A)
		    (not apply-p))
	       (setq *step-max* (1+ *step-level*)) (return 'applyhook))
	      ((= ch #\c-B)
	       (break)
	       (setq ch 0)
	       (setf (aref *step-array* *step-level*) *step-form*)
	       (setf (aref *step-apply-p-array* *step-level*) apply-p)
	       (go redis1))
	      ((= ch #\c-E)
	       (ed)
	       (setq ch 10.)
	       (go redisplay))
	      ((or (= ch #\Clear-Screen) (= ch #\c-L))
	       (setq ch 10.)
	       (go redisplay))
	      ((= ch #\m-L)
	       (setq ch 10.)
	       (go redis1))
	      ((= ch #\c-m-L)
	       (setq ch *step-level*)
	       (go redisplay))
	      ((or (= ch #\c-G) (= ch #\c-T))
	       (setq ch (if (= ch #\c-G) #'grind-top-level #'print))
	       (cond ((null values) (funcall ch form))
		     ((do ((l values (cdr l)))
			  ((null l))
			(funcall ch (car l)))))
	       (go rd))
	      ((= (char-code ch) (char-code #\HELP))
	       (sys:with-help-stream (help-str :label "Stepper help")
		 (terpri help-str)
		 (princ
		   (if (null *step-values*)
		       (if apply-p
			   "You are about to apply the above function to the above arguments."
			   "You are about to evaluate the above form.")
		       (if apply-p
			   "You have applied a function to arguments
and are about to return the above values."
			   "You have evaluated a form and are about to return the above values."))
		   help-str)
		 (terpri help-str)
		 (princ
"Commands are single characters, usually control, which don't echo:

    C-N	    Proceed to next thing evaled.
    <space> Proceed to next thing evaled at same level.
    C-A     Eval the args without stepping; stop before applying the function.
    C-U	    Proceed to first thing up one level.
    C-X	    Continue without further stepping.
    C-E	    Escape to editor.
    C-T	    Retype current form in full.
    C-G	    Grind current form.
    C-B	    Enter breakpoint, with the following variables bound:
	        SI::*STEP-FORM* is the form, SI::*STEP-VALUES* is the list of values,
	        SI::*STEP-VALUE* is the first value.  If you change these, it wins.
    C-L
    <form>  Clear & show last 10. forms.
    M-L	    Just show last 10. forms (don't clear).
    C-M-L   Clear and show all forms.
    <any LISP form>
	    Will be read and evaluated, and values printed.

Magic flags preceding output:

       Ordinary LISP form
       About to apply a function
       Macro
       Values
       Separates multiple values
"
	help-str))
	       (setq ch 0)
	       ;; No need to redisplay if with-help-stream used a separate window.
	       (if (typep *terminal-io* 'tv:sheet) (go rd))
	       (go redis1))
	      ((zerop (char-bits ch1))
	       (unread-char ch1)
	       (catch-error-restart ((sys:abort error) "Back to STEP command level.")
		 (print
		   (eval-abort-trivial-errors 
		     (multiple-value-bind (sexp flag)
			 (with-input-editing (*standard-input* '((:full-rubout :full-rubout)
								 (:prompt " Eval: ")))
			   (si:read-for-top-level *standard-input*))
		       (when (eq flag ':full-rubout)
			 (go rd))
		       sexp))))
	       (terpri)
	       (setq ch 0)
	       (go redis1))
	      (t
	       (beep)
	       (go rd)))
     redisplay 
	(send *standard-output* :clear-screen)
     redis1 
	(do ((i (max 0 (- *step-level* ch)) (1+ i)))
	    ((> i *step-level*))
	  (step-print-form (aref *step-array* i) i (aref *step-apply-p-array* i) environment))
	(go pv)))

))

; From file STEP.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-evalhook (*step-form* &optional environment)
  (binding-interpreter-environment (environment)
    (let ((*step-level* (1+ *step-level*))
	  (*step-value*) (*step-values*)
	  tem val)
      (tagbody
	  (when ( *step-level* (array-length *step-array*))
	    (adjust-array-size *step-array* (+ #o100 *step-level*))
	    (adjust-array-size *step-apply-p-array* (+ #o100 *step-level*)))
       mc (setf (aref *step-array* *step-level*) *step-form*)
	  (setf (aref *step-apply-p-array* *step-level*) nil)
	  (cond ((atom *step-form*)
		 (setq *step-values* (list (eval1 *step-form*)))
		 (setq tem 'atom)
		 (go rl))
		(( *step-level* *step-max*)
		 (setq tem (step-cmdr *step-form* nil t nil environment)))
		(t (setq tem 'eval)))
	  (cond ((step-macro-form-p *step-form* environment)
		 (setq *step-form* (macroexpand-1 *step-form* environment))
		 (go mc))
		((eq tem 'eval)
		 (setq *step-values* (multiple-value-list
				       (evalhook *step-form* nil nil environment))))
		((eq tem 'evalhook)
		 (setq *step-values* (multiple-value-list
				       (evalhook *step-form* #'step-evalhook nil environment))))
		((eq tem 'applyhook)
		 (setq *step-values* (multiple-value-list
				       (evalhook *step-form* nil #'step-applyhook environment))))
		((ferror nil "Unknown function ~S" tem)))
       rl (setq *step-value* (setq val (car *step-values*)))
	  (if ( *step-level* *step-max*)
	      (setq tem (step-cmdr *step-form* *step-values* (neq tem 'eval) nil environment))
	    (setq tem 'eval))
	  (and (neq *step-value* val) (return-from step-evalhook *step-value*))
       rt (if (null (cdr *step-values*))
	      (return-from step-evalhook (car *step-values*))
	    (return-next-value (car *step-values*))
	    (setq *step-values* (cdr *step-values*))
	    (go rt))))))

))

; From file STEP.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-applyhook (function args &optional environment
		       &aux (*step-form* (cons function args)))
  (binding-interpreter-environment (environment)
    (let ((*step-level* (1+ *step-level*))
	  (*step-value*) (*step-values*)
	  tem val)
      (tagbody
	  (when ( *step-level* (array-length *step-array*))
	    (adjust-array-size *step-array* (+ #o100 *step-level*))
	    (adjust-array-size *step-apply-p-array* (+ #o100 *step-level*)))
       mc (setf (aref *step-array* *step-level*) *step-form*)
	  (setf (aref *step-apply-p-array* *step-level*) t)
	  (if ( *step-level* *step-max*)
	      (setq tem (step-cmdr *step-form* nil t t environment))
	      (setq tem 'eval))
	  (cond ((eq tem 'eval)
		 (setq *step-values*
		       (multiple-value-list (apply (car *step-form*) (cdr *step-form*)))))
		((eq tem 'evalhook)
		 (setq *step-values*
		       (multiple-value-list
			 (let ((*evalhook* #'step-evalhook))
			   (apply (car *step-form*) (cdr *step-form*))))))
		((ferror nil "Unknown function ~S" tem)))
       rl (setq *step-value* (setq val (car *step-values*)))
	  (if ( *step-level* *step-max*)
	      (setq tem (step-cmdr *step-form* *step-values* (neq tem 'eval) t environment))
	      (setq tem 'eval))
	  (when (neq *step-value* val)
	    (return-from step-applyhook *step-value*))
       rt (if (null (cdr *step-values*))
	      (return-from step-applyhook (car *step-values*))
	    (return-next-value (car *step-values*))
	    (setq *step-values* (cdr *step-values*))
	    (go rt))))))

))

; From file STRING.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING (X)
  "Convert X to a string if possible."
  (IF (NUMBERP X) (SETQ X (INT-CHAR X)))
  (TYPECASE X
    (STRING X)
    (SYMBOL (SYMBOL-NAME X))
    (STRING-CHAR
     (VALUES (MAKE-STRING 1 :INITIAL-ELEMENT X)))
    (INSTANCE
     (SEND X ':SEND-IF-HANDLES ':STRING-FOR-PRINTING))
    (T
     (FERROR NIL "Cannot convert ~S into a string." X))))

))

; From file QTRACE.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; QTRACE  "

(DEFUN TRACE-1 (SPEC)
  (PROG (BREAK EXITBREAK ENTRYCOND EXITCOND WHEREIN ARGPDL ENTRY EXIT (ARG T) (VALUE T) 
	     STEP (BARFP T) STEPCOND
	     ENTRYVALS EXITVALS MUMBLE FCN SPEC1 TRFCN ERROR
	     (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (COND ((ATOM SPEC)
	   (SETQ FCN SPEC))
	  (T
	   (COND ((EQ (CAR SPEC) ':FUNCTION)
		  (SETQ FCN (CADR SPEC) SPEC (CDR SPEC)))
		 ((ATOM (CAR SPEC))
		  (SETQ FCN (CAR SPEC)))
		 (T (RETURN (LOOP FOR FCN IN (CAR SPEC)
				  NCONC (TRACE-1 `(:FUNCTION ,FCN . ,(CDR SPEC)))))))
	   (DO SPECS (CDR SPEC) (CDR SPECS) (NULL SPECS)
	     (SELECTQ (CAR SPECS)
		(:BREAK (SETQ BARFP SPECS SPECS (CDR SPECS) BREAK (CAR SPECS)))
		(:EXITBREAK (SETQ BARFP SPECS SPECS (CDR SPECS) EXITBREAK (CAR SPECS)))
		(:STEPCOND (SETQ BARFP SPECS SPECS (CDR SPECS) STEPCOND (CAR SPECS)
				 STEP T))
		(:STEP (SETQ STEP T))
		(:ERROR (SETQ ERROR T))
		(:COND (SETQ BARFP SPECS SPECS (CDR SPECS))
		       (SETQ STEPCOND (SETQ EXITCOND (SETQ ENTRYCOND (CAR SPECS)))))
		(:ENTRYCOND (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRYCOND (CAR SPECS)))
		(:EXITCOND (SETQ BARFP SPECS SPECS (CDR SPECS) EXITCOND (CAR SPECS)))
		(:WHEREIN (SETQ BARFP SPECS SPECS (CDR SPECS) WHEREIN (CAR SPECS)))
		(:ARGPDL (SETQ BARFP SPECS SPECS (CDR SPECS) ARGPDL (CAR SPECS)))
		(:ENTRY (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRY (CAR SPECS)))
		(:EXIT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CAR SPECS)))
		(:PRINT (SETQ BARFP SPECS
			      SPECS (CDR SPECS)
			      ENTRY (CONS (CAR SPECS) ENTRY)
			      EXIT (CONS (CAR SPECS) EXIT)))
		(:ENTRYPRINT (SETQ BARFP SPECS SPECS (CDR SPECS)
				   ENTRY (CONS (CAR SPECS) ENTRY)))
		(:EXITPRINT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CONS (CAR SPECS) EXIT)))
		((:ARG :VALUE :BOTH NIL)
		 (AND (EQ (CAR SPECS) ':ARG) (SETQ VALUE NIL))
		 (AND (EQ (CAR SPECS) ':VALUE) (SETQ ARG NIL))
		 (AND (EQ (CAR SPECS) NIL) (SETQ ARG NIL VALUE NIL))
		 (AND ARG (SETQ ENTRYVALS (CDR SPECS)))
		 (AND VALUE (SETQ EXITVALS (CDR SPECS)))
		 (RETURN NIL))
		(OTHERWISE
		 (SETQ MUMBLE (CAR SPECS))
		 (RETURN NIL)))
	     (AND (NULL BARFP) (FERROR NIL "Parameter missing")) )))
    (SETQ FCN (DWIMIFY-ARG-PACKAGE FCN 'FCN))
    (UNTRACE-1 FCN)
    (AND MUMBLE (RETURN (FERROR NIL "Meaningless TRACE keyword: ~S" MUMBLE)))
    (CHECK-ARG ARGPDL SYMBOLP "a symbol")
    (SETQ SPEC1 (UNENCAPSULATE-FUNCTION-SPEC FCN 'TRACE))

    (SETQ TRFCN (ENCAPSULATE SPEC1 FCN 'TRACE
       `(PROG* (,@(AND ARGPDL `((,ARGPDL (CONS (LIST (1+ ,COPY) ',FCN ARGLIST)
					       ,ARGPDL))))
		(VALUES NIL)
		(,COPY (1+ ,COPY))
		(TRACE-LEVEL (1+ TRACE-LEVEL)))
	       (DECLARE (SPECIAL ,COPY VALUES))
	       ;; End of PROG var list.
	       ,(IF ERROR `(PROGN (LET ((EH:ERROR-DEPTH (1+ EH:ERROR-DEPTH))
					(EH:CONDITION-PROCEED-TYPES '(:NO-ACTION)))
				    (EH:INVOKE-DEBUGGER
				      (MAKE-CONDITION 'EH:TRACE-BREAKPOINT
						      "~S entered" ',FCN)))
				  (RETURN-LIST (MULTIPLE-VALUE-LIST
						 (APPLY ,ENCAPSULATED-FUNCTION ARGLIST))))
		  `(COND ((OR INSIDE-TRACE
			      . ,(AND WHEREIN `((NOT (FUNCTION-ACTIVE-P ',WHEREIN)))))
			  (RETURN-LIST (MULTIPLE-VALUE-LIST
					 (APPLY ,ENCAPSULATED-FUNCTION ARGLIST))))
			 (T (LET ((INSIDE-TRACE T))
			      ,(TRACE-MAYBE-CONDITIONALIZE ENTRYCOND
					 `(TRACE-PRINT ,COPY 'ENTER ',FCN ',ARG
						       ',ENTRY ',ENTRYVALS))
			      ,@(AND BREAK `((AND ,BREAK (LET (INSIDE-TRACE)
							   (BREAK "Entering ~S." ',FCN)))))
			      (SETQ VALUES
				    (LET ((INSIDE-TRACE NIL))
				      (MULTIPLE-VALUE-LIST
					,(IF (AND STEP STEPCOND)
					     ;; conditionally call the stepper.
					     `(IF ,STEPCOND
						  (TRACE-STEP-APPLY
						    ,ENCAPSULATED-FUNCTION
						    ARGLIST)
						(TRACE-APPLY
						  ,ENCAPSULATED-FUNCTION
						  ARGLIST))
					   `(,(IF STEP 'TRACE-STEP-APPLY 'TRACE-APPLY)
					        ,ENCAPSULATED-FUNCTION
					        ARGLIST)))))
			      ,(TRACE-MAYBE-CONDITIONALIZE EXITCOND
					 `(TRACE-PRINT ,COPY 'EXIT ',FCN ',VALUE
						       ',EXIT ',EXITVALS))
			      ,@(AND EXITBREAK
				     `((AND ,EXITBREAK (LET (INSIDE-TRACE)
							 (BREAK "Exiting ~S." ',FCN)))))
			      (RETURN-LIST VALUES))))))))
    (SET TRFCN 0)
    (PUSH FCN TRACED-FUNCTIONS)
    (IF (OR TRACE-COMPILE-FLAG COMPILE-ENCAPSULATIONS-FLAG)
	(COMPILE-ENCAPSULATIONS SPEC1 'TRACE))
    (RETURN (NCONS FCN))))

))

; From file ADVISE.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defun advise-init (function-spec)
  (let ((default-cons-area background-cons-area)
	(spec1 (unencapsulate-function-spec function-spec 'advise)))
    (cond ((neq spec1 (unencapsulate-function-spec spec1 '(advise)))
	   (uncompile spec1 t)
	   (let ((body (encapsulation-body (fdefinition spec1))))
	     ;; (car body) looks like:
	     ;;        (advised-function nil nil nil encapsulated-function)
	     (when (eq (car (car body)) 'si:displaced)
	       (setf (car body) (cadr (car body))))))
	  (t
	   (push function-spec advised-functions)
	   (encapsulate spec1 function-spec 'advise
			`(advised-function nil nil nil ,encapsulated-function))))))

))

; From file SETF.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defun optimize-pushnew (value place options environment)
  (if (or (not (symbolp place)) (oddp (length options)))
      ;; stupid optimizer doesn't try to win on this.
      nil
    (let (test xtest)
      (do ((x options (cddr x)))
	  ((null x))
	(cond ((or (eq (car x) ':test)
		   (equal (car x) '':test))
	       (if (not test)
		   ;; note: cannot use (function test) as this would lose inside FLET
		   ;;  redefinition of test
		   (cond ((list-match-p (cadr x) `(quote ,test)))
			 ((and (list-match-p (cadr x) `(function ,test))
			       (symbolp test)	;or else fsymeval-in-environment blows out
			       (not (sys:fsymeval-in-environment test environment nil))))
			 (t (return (setq test nil))))))
	      (t (setq test nil) (return))))
      (if (null options) (setq test 'eql))
      (cond ((and (symbolp test) (not (null test)))
	     (setq xtest (cdr (assq test *test-member-alist*)))
	     (flet ((make-test (item)
			       (if xtest
				   `(,xtest ,item ,place)
				 (let ((x (gensym)))
				   `(do ((,x ,place (cdr ,x)))
					((null ,x))
				      (if (,test (car ,x) ,item) (return ,x)))))))
	       (if (or (symbolp value) (constantp value))
		   `(progn (or ,(make-test value)
			       (push ,value ,place))
			   ,value)
		   (let ((tem (gensym)))
		     `(let ((,tem ,value))
			(or ,(make-test tem)
			    (push ,tem ,place))
			,tem)))))
	    ;; stupid optimizer doesn't try to hack this either...
	    (t nil)))))

))

(setf (documentation 'haulong 'function)
  "Returns the /"size/" of INTEGER in bits.  The size of #o777 is nine bits.")
(setf (documentation 'prinlevel 'variable)
  "If non-NIL, maximum depth for printing list structure.
Any structure nested more deeply that this amount
is replaced by /"#/".")

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun interpreter-environment-closure-p (closure &aux (bindings (closure-bindings closure)))
  "T if CLOSURE is a closure over the interpreter environment variables"
  (and (getf bindings (locf (symbol-value '*interpreter-variable-environment*)))
       (getf bindings (locf (symbol-value '*interpreter-function-environment*)))
       (getf bindings (locf (symbol-value '*interpreter-frame-environment*)))))

))

; From file MACROS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MACROS  "

(DEFMACRO BP-CH-CHARACTER (BP)
  "Return the character after BP sans font and meta-bits"
  `(INT-CHAR (CHAR-CODE (BP-CHARACTER ,BP))))

))

; From file COMH.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFUN LISP-CHANGE-CASE (UP-P &AUX START-BP BP CH SYNTAX)
  (REGION (BP1 BP2)
    (MULTIPLE-VALUE-BIND (STRING-P SLASHIFIED-P COMMENT-P) (LISP-BP-SYNTACTIC-CONTEXT BP1)
      (FLET ((DO-IT () (WITH-UNDO-SAVE ((IF UP-P "Upcase Lisp Code" "Downcase Lisp Code")
					START-BP BP T)
			 (IF UP-P (UPCASE-INTERVAL START-BP BP T)
			   (DOWNCASE-INTERVAL START-BP BP T)))))
	  (SETQ START-BP (COPY-BP BP1) BP (COPY-BP BP1))
	  (DO ()
	      ((BP-= BP BP2)
	       (UNLESS (OR COMMENT-P STRING-P SLASHIFIED-P)
		 (DO-IT)))
	    (IBP BP)
	    (SETQ CH (BP-CH-CHARACTER BP))
	    (SETQ SYNTAX (LIST-SYNTAX CH))
	    (COND (COMMENT-P
		   (WHEN (EQ CH #/RETURN)
		     (SETQ COMMENT-P NIL)
		     (MOVE-BP START-BP BP)
		     (IBP START-BP)))
		  (SLASHIFIED-P
		   (SETQ SLASHIFIED-P NIL)
		   (MOVE-BP START-BP BP)
		   (IBP START-BP))
		  ((= SYNTAX LIST-SLASH)
		   (SETQ SLASHIFIED-P BP)
		   (UNLESS STRING-P
		     (DO-IT)))
		  (STRING-P
		   (WHEN (EQ CH STRING-P)
		     (SETQ STRING-P NIL)
		     (MOVE-BP START-BP BP)
		     (IBP START-BP)))
		  ((OR (= SYNTAX LIST-COMMENT)
		       (= SYNTAX LIST-DOUBLE-QUOTE))
		   (IF (= SYNTAX LIST-COMMENT)
		       (SETQ COMMENT-P T)
		     (SETQ STRING-P CH))
		   (DO-IT))
		  ;; special kludge for Zetalisp #\ #/ equivalence. Sigh
		  ((AND (NOT (OR COMMENT-P STRING-P SLASHIFIED-P))
			(EQ CH #/\)
			(CHAR-EQUAL (BP-CHARACTER-BEFORE BP) #/#))
		   (SETQ SLASHIFIED-P T)
		   (DO-IT)))))))
  DIS-TEXT)

))

; From file PRIMIT.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PRIMIT  "

(DEFUN BP-LOOKING-AT-LIST (BP LIST)
  "T if one of the characters or strings in LIST matches the text after BP.
The strings may not contain Return."
  (DO ((LIST LIST (CDR LIST))
       (BP-CH (BP-CH-CHARACTER BP))
       (CH))
      ((NULL LIST) NIL)
    (SETQ CH (CAR LIST))
;character lossage
    (IF (NUMBERP CH) (SETQ CH (INT-CHAR CH)))
    (AND (IF (TYPEP (SETQ CH (CAR LIST)) 'CHARACTER)
	     (CHAR-EQUAL BP-CH CH)
	   (LET ((LEN (STRING-LENGTH CH))
		 (INDEX (BP-INDEX BP)))
	     (STRING-EQUAL (BP-LINE BP) CH :START1 INDEX :END1 (+ INDEX LEN))))
	 (RETURN CH))))

))

; From file COMA.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-SHOW-LIST-START "Displays the start of the list which the point lies after." (KM)
  (LET ((BP (FORWARD-LIST (POINT) -1 NIL 0 NIL T)))
    (COND ((NULL BP) (BARF "No list found before point."))
	  (T (LET* ((END-BP (FORWARD-SEXP BP 1 NIL 0))
		    (CLOSE (INT-CHAR (CHAR-CODE (BP-CHARACTER-BEFORE END-BP))))
		    (BALANCE-LENGTH (MAX 1 (- (SEND *TYPEIN-WINDOW* ':SIZE-IN-CHARACTERS)
					   25.))))
	       (DO ((INDEX (BP-INDEX BP) (1+ INDEX))
		    (OPEN))
		   ((= (LIST-SYNTAX (SETQ OPEN (INT-CHAR (CHAR-CODE
							    (CLI:AREF (BP-LINE BP) INDEX)))))
		       LIST-OPEN)
		    (TYPEIN-LINE "") ;clear line, since typein-line-more used if paren ok
		    (UNLESS (= (SECOND (ASSQ OPEN *MATCHING-DELIMITER-LIST*)) CLOSE)
		      (PROGN (BEEP) (TYPEIN-LINE "Non-matching parenthesis.~%")))))
	       (DO ((STRING (MAKE-STRING 30. :FILL-POINTER 0)
			    (STRING-APPEND STRING (IF (EQ CH #/RETURN) "   " CH)))
		    (CH (BP-CH-CHARACTER BP)
;character lossage
			(COND ((MEMQ (BP-CH-CHAR BP) *WHITESPACE-CHARS*)
			       (SELECTOR CH STRING-EQUAL
				 ((#/SP #/NEWLINE #/TAB "") "")
				 (T (BP-CH-CHARACTER BP))))
			      (T (BP-CH-CHARACTER BP))))
		    (BP (IBP BP) (IBP BP)))
		   ((OR (> (STRING-LENGTH STRING) BALANCE-LENGTH)
			(BP-= BP END-BP))
		    (SETF (FILL-POINTER STRING) (MIN (FILL-POINTER STRING) BALANCE-LENGTH))
		    (TYPEIN-LINE-MORE "~A ... balances this paren" string)))
	       (MOVE-BP (POINT) END-BP)))))
  DIS-BPS)

))

; From file COMF.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-CANONICALIZE-WHITESPACE "Try to fixup wrong spacing heuristically.
If given an argument, or called just after a yank type command, operates
at the mark, else at point." ()
  (LET ((BP (IF (OR *NUMERIC-ARG-P* (EQ *LAST-COMMAND-TYPE* 'YANK)) (MARK) (POINT)))
	BP1 CH1 CH2 SYN1 SYN2)
    (SETQ BP (BACKWARD-OVER *BLANKS* BP)
	  BP1 (FORWARD-OVER *BLANKS* BP)
	  CH1 (BP-CH-CHARACTER (OR (FORWARD-CHAR BP -1) (BARF)))
	  CH2 (BP-CH-CHARACTER BP1)
	  SYN1 (LIST-SYNTAX CH1)
	  SYN2 (LIST-SYNTAX CH2))
    (COND ((OR (EQ CH2 #/CR)			;If at the end of the line,
	       (MULTIPLE-VALUE-BIND (STRING SLASH COMMENT)
		   (LISP-BP-SYNTACTIC-CONTEXT BP)
		 (OR STRING SLASH COMMENT))))	;or any funny syntax, leave it alone
	  ((NEQ CH1 #/CR)			;If not at beginning of line,
	   (DELETE-INTERVAL BP BP1 T)
	   (IF (AND ( SYN1 LIST-OPEN) ( SYN1 LIST-SINGLE-QUOTE)
		    ( SYN2 LIST-CLOSE))
	       (INSERT BP #/SP)))
	  ((NEQ CH2 #/()			;If not start of defun
	   (INDENT-INTERVAL-FOR-LISP BP (BEG-LINE BP 1 T) T NIL T))	;run tab
	  ((DO ((LINE (LINE-PREVIOUS (BP-LINE BP)) (LINE-PREVIOUS LINE))
		(OLINE (BP-LINE BP) LINE)	;Flush blank lines, and
		(TYPE))				;unless previous non-blank is a comment
	       (NIL)
	     (SETQ TYPE (AND LINE (LINE-TYPE LINE)))
	     (UNLESS (EQ TYPE ':BLANK)
	       (DELETE-INTERVAL (CREATE-BP OLINE 0) BP T)
	       (RETURN (NEQ TYPE ':COMMENT))))
	   (INSERT BP #/NEWLINE))))		;leave just one in their place
  DIS-TEXT)

))

; From file COMH.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFUN LISP-STRING-BUFFER-MATCH (START-BP LIMIT-BP PATTERN-STRING &OPTIONAL (START 0) END)
  "Match part of a string against part of an editor buffer, comparing as Lisp code.
The string is PATTERN-STRING; START and END specify the range to use.
The buffer text starts at START-BP.  It will not match past LIMIT-BP.
If there is a match, the value is a bp to the end of the buffer text matched.
Otherwise, the value is NIL.
Differences in whitespace characters are ignored except when quoted or inside strings.
The characters ** as an atom in the PATTERN-STRING match any sexp in the buffer.
The characters ... as an atom in the PATTERN-STRING match any number of sexps."
  (UNLESS END (SETQ END (LENGTH PATTERN-STRING)))
  (DO-NAMED OUTER
	    ((I START (1+ I))
	     (BP (COPY-BP START-BP))
	     IN-STRING QUOTED IN-COMMENT IN-ATOM
	     (P-SYN -1))
	    ((= I END) BP)
    (IF (BP-= LIMIT-BP BP)
	(RETURN NIL))
    (LET* ((S-CHAR (CHAR PATTERN-STRING I))
	   (S-SYN (LIST-SYNTAX S-CHAR)))
      ;; S-SYN is this pattern character's syntax.
      ;; P-SYN is the previous significant pattern character's syntax.
      ;; It is LIST-ALPHABETIC iff the last pattern character, not counting delimiters,
      ;; was such as to be part of an atom.  This is the case in which
      ;; at least one delimiter is required in the buffer in order to match.
      (COND (IN-STRING
	     ;; First update the syntactic state.
	     (COND (QUOTED (SETQ QUOTED NIL))
		   ((= S-SYN LIST-DOUBLE-QUOTE)
		    (SETQ IN-STRING NIL))
		   ((= S-SYN LIST-SLASH)
		    (SETQ QUOTED T)))
	     ;; Now always match against buffer.
	     (UNLESS (EQ S-CHAR (BP-CH-CHARACTER BP))
	       (RETURN NIL))
	     (SETQ P-SYN -1)
	     (IBP BP))
	    (IN-COMMENT
	     (IF (EQ S-CHAR #/RETURN) (SETQ IN-COMMENT NIL))
	     ;; Now always match against buffer.
	     (UNLESS (CHAR-EQUAL S-CHAR (BP-CHARACTER BP))
	       (RETURN NIL))
	     (SETQ P-SYN -1)
	     (IBP BP))
	    (QUOTED
	     (SETQ QUOTED NIL)
	     ;; Quoted char, always match against buffer.
	     (UNLESS (EQ S-CHAR (BP-CH-CHARACTER BP))
	       (RETURN NIL))
	     (SETQ P-SYN LIST-ALPHABETIC)
	     (IBP BP))
	    ;; Not in string or comment, not slashified.
	    ((= S-SYN LIST-DELIMITER)
	     ;; Just skip all delimiters in the pattern.
	     (SETQ IN-ATOM NIL))
	    ((AND (NOT IN-ATOM)
		  ( (+ I 3) END)
		  (STRING-EQUAL PATTERN-STRING "..." I 0 (+ I 3))
		  (OR (= (+ I 3) END)
		      (NOT (MEMQ (LIST-SYNTAX (CHAR PATTERN-STRING (+ I 3)))
				 '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
	     ;; "..." has been encountered, and its an atom by itself.
	     (DO-FOREVER
	       ;; Try matching the rest of the pattern at one spot.
	       (LET ((TEM (LISP-STRING-BUFFER-MATCH BP LIMIT-BP PATTERN-STRING (+ I 3) END)))
		 (WHEN TEM
		   (RETURN-FROM OUTER TEM)))
	       ;; SKip one more sexp and try again.
	       (SETQ BP (FORWARD-SEXP BP 1 NIL 0 LIMIT-BP NIL T))
	       (UNLESS BP (RETURN NIL))))
	    ((AND (NOT IN-ATOM)
		  ( (+ I 2) END)
		  (STRING-EQUAL PATTERN-STRING "**" I 0 (+ I 2))
		  (OR (= (+ I 2) END)
		      (NOT (MEMQ (LIST-SYNTAX (CHAR PATTERN-STRING (+ I 2)))
				 '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
	     ;; "**" has been encountered as an atom in the pattern.
	     ;; SKip it, and skip one sexp in the buffer, then keep matching.
	     (INCF I)
	     (SETQ BP (FORWARD-SEXP BP 1 NIL 0 LIMIT-BP NIL T))
	     (SETQ P-SYN -1)
	     (UNLESS BP (RETURN NIL)))
	    (T
	     ;; Skip all delimiters here in the buffer, if not within an atom.
	     (UNLESS IN-ATOM
	       (DO ((COUNT 0 (1+ COUNT)))  ;Count number of delimiters skipped.
		   ((NOT (= LIST-DELIMITER (LIST-SYNTAX (BP-CHARACTER BP))))
		    (AND (ZEROP COUNT)
			 (= S-SYN LIST-ALPHABETIC)
			 (= P-SYN LIST-ALPHABETIC)
			 (RETURN-FROM OUTER NIL)))
		 (IBP BP)))
	     ;; Set up syntax context of next pattern character.
	     (SELECT S-SYN
	       (LIST-DOUBLE-QUOTE
		(SETQ IN-STRING T))
	       (LIST-SLASH
		(SETQ QUOTED T))
	       (LIST-COMMENT
		(SETQ IN-COMMENT T))
	       (LIST-ALPHABETIC
		(SETQ IN-ATOM T)))
	     (IF (EQ S-CHAR #/.) (SETQ IN-ATOM T))
	     ;; Now always match against buffer.
	     (UNLESS (CHAR-EQUAL S-CHAR (BP-CHARACTER BP))
	       (RETURN NIL))
	     (IBP BP)
	     (SETQ P-SYN S-SYN))))))

))

; From file PRINT.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM
		     &OPTIONAL
		     (WHICH-OPERATIONS (WHICH-OPERATIONS-FOR-PRINT STREAM))
		     &AUX NSS (FASTP (MEMQ :STRING-OUT WHICH-OPERATIONS)))
  (CATCH-CONTINUATION-IF T 'PRINT-OBJECT
      #'(LAMBDA () (FORMAT STREAM "...error printing ")
		(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPE :FASTP FASTP))
		(FORMAT STREAM "..."))
      NIL
    (CONDITION-RESUME '((ERROR) :ABORT-PRINTING T ("Give up trying to print this object.")
			CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
      (OR (AND (MEMQ :PRINT WHICH-OPERATIONS)	;Allow stream to intercept print operation
	       (SEND STREAM :PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*))
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
		 (SEND PRINT-HASH-TABLE :MODIFY-HASH EXP
		       #'(LAMBDA (KEY VALUE KEY-FOUND-P STREAM)
			   KEY KEY-FOUND-P
			   (COND ((NULL VALUE) NIL)
				 ((EQ VALUE T)
				  (LET ((LABEL (INCF PRINT-LABEL-NUMBER))
					(*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM :TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM :TYO #/=)
				    LABEL))
				 (T
				  (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM :TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM :TYO #/#)
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
	      (SEND EXP :PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
	    (ENTITY
	     (IF (MEMQ :PRINT-SELF (SEND EXP :WHICH-OPERARATIONS))
		 (SEND EXP :PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*) 
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (NAMED-STRUCTURE
	     (IGNORE-ERRORS
	       (SETQ NSS (NAMED-STRUCTURE-P EXP)))
	     (COND ((AND (SYMBOLP NSS)
			 (GET NSS 'NAMED-STRUCTURE-INVOKE)
			 (MEMQ :PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP :WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE :PRINT-SELF EXP STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;Named structure that doesn't print itself
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (ARRAY
	     (PRINT-ARRAY EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))
	    (SHORT-FLOAT
	     (PRINT-FLONUM EXP STREAM FASTP T))
	    (SINGLE-FLOAT
	     (PRINT-FLONUM EXP STREAM FASTP NIL))
	    (BIGNUM
	     (PRINT-BIGNUM EXP STREAM FASTP))
	    (RATIONAL
	     (PRINT-RATIONAL EXP STREAM FASTP))
	    (COMPLEX
	     (PRINT-COMPLEX EXP STREAM FASTP))
	    (CHARACTER
	     (PRINT-CHARACTER EXP STREAM FASTP))
	    (NUMBER
	     (PRINT-RAW-STRING (CAR (PTTBL-RANDOM *READTABLE*)) STREAM FASTP)
	     (PRINT-RAW-STRING (SYMBOL-NAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
	     (LET ((*PRINT-BASE* 8) (*PRINT-RADIX* NIL))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (CDR (PTTBL-RANDOM *READTABLE*)) STREAM FASTP))
	    (CLOSURE (PRINT-CLOSURE EXP STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

))

; From file PRINT.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-PNAME-STRING (SYMBOL STREAM FASTP &OPTIONAL NO-PACKAGE-PREFIXES
			   &AUX STRING LEN FSMWINS MUST// TEM)
  (DECLARE (SPECIAL XP-STREAM XP-FASTP XR-EXTENDED-IBASE-P))
  ;; Print a package prefix if appropriate.
  (WHEN (AND *PRINT-ESCAPE* (NOT NO-PACKAGE-PREFIXES)
	     (SYMBOLP SYMBOL))
    (COND ((NULL (SYMBOL-PACKAGE SYMBOL))
	   (AND *PRINT-GENSYM*
		(SEND STREAM :STRING-OUT
		      (PTTBL-UNINTERNED-SYMBOL-PREFIX *READTABLE*))))
	  (T
	   (MULTIPLE-VALUE-BIND (PKG-OR-STRING INTERNAL-FLAG)
	       (SI:PKG-PRINTING-PREFIX SYMBOL *PACKAGE*)
	     (MULTIPLE-VALUE (PKG-OR-STRING INTERNAL-FLAG SYMBOL)
	       (LET* ((TEM1 (CAR (RASSQ SYMBOL (RDTBL-SYMBOL-SUBSTITUTIONS *READTABLE*))))
		      (TEM2 (UNLESS TEM1
			      (CDR (ASSQ SYMBOL (RDTBL-SYMBOL-SUBSTITUTIONS *READTABLE*)))))
		      POS IF)
		 (COND ((AND TEM1 (MEMBER-EQUAL (MULTIPLE-VALUE (POS IF)
						  (SI:PKG-PRINTING-PREFIX TEM1 *PACKAGE*))
						'(NIL "")))
			(VALUES POS IF TEM1))
		       (TEM2 (MULTIPLE-VALUE (NIL IF POS)
			       (INTERN TEM2 *PACKAGE*))
			     (VALUES POS (EQ IF ':INTERNAL) TEM2))
		       (T (VALUES PKG-OR-STRING INTERNAL-FLAG SYMBOL)))))
	     (WHEN PKG-OR-STRING
	       (UNLESS (EQUAL PKG-OR-STRING "")
		 (PRINT-PNAME-STRING (IF (STRINGP PKG-OR-STRING) PKG-OR-STRING
				       (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING))
				     STREAM FASTP))
	       (SEND STREAM :STRING-OUT
		     (IF (AND (NOT (STRINGP PKG-OR-STRING))
			      *PACKAGE*
			      (ASSOC-EQUAL (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING)
					   (DONT-OPTIMIZE (SI:PKG-REFNAME-ALIST PACKAGE))))
			 ;; Use #: to inhibit an interfering local nickname.
			 "#:"
		       (IF INTERNAL-FLAG
			   (PTTBL-PACKAGE-INTERNAL-PREFIX *READTABLE*)
			   (PTTBL-PACKAGE-PREFIX *READTABLE*)))))))))
  (SETQ STRING (STRING SYMBOL))
  (IF (NOT *PRINT-ESCAPE*)
      (SELECTQ *PRINT-CASE*
	(:DOWNCASE (DOTIMES (I (LENGTH STRING))
		     (SEND STREAM :TYO (CHAR-DOWNCASE (AREF STRING I)))))
	(:CAPITALIZE (DO ((LENGTH (LENGTH STRING)) CHAR PREV-LETTER
			  (I 0 (1+ I)))
			 ((= I LENGTH))
		       (SETQ CHAR (AREF STRING I))
		       (COND ((UPPER-CASE-P CHAR)
			      (SEND STREAM :TYO (IF PREV-LETTER (CHAR-DOWNCASE CHAR) CHAR))
			      (SETQ PREV-LETTER T))
			     ((LOWER-CASE-P CHAR)
			      (SEND STREAM :TYO (IF PREV-LETTER CHAR (CHAR-UPCASE CHAR)))
			      (SETQ PREV-LETTER T))
			     (( #/0 CHAR #/9)
			      (SEND STREAM :TYO CHAR)
			      (SETQ PREV-LETTER T))
			     (T (SEND STREAM :TYO CHAR)
				(SETQ PREV-LETTER NIL)))))
	(T (PRINT-RAW-STRING STRING STREAM FASTP))) 
    (SETQ FSMWINS
	  (AND (PLUSP (SETQ LEN (LENGTH STRING)))
	       (DO ((I 0 (1+ I))
		    (STATE (RDTBL-STARTING-STATE *READTABLE*))
		    (FSM (RDTBL-FSM *READTABLE*))
		    (CHAR)
		    (ESCAPE-CODE (RDTBL-ESCAPE-CODE *READTABLE*))
		    (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*))
		    (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*)))
		   ((= I LEN)
		    (COND ((NOT (NUMBERP STATE))
			   (DOLIST (L (RDTBL-MAKE-SYMBOL *READTABLE*))
			     (AND (EQ (CAR STATE) (CAR L))
				  (EQ (CDR STATE) (CDR L))
				  (RETURN T))))
			  ((NOT (NUMBERP (SETQ STATE
					       (AREF FSM
						     STATE
						     (RDTBL-BREAK-CODE *READTABLE*)))))
			   (DOLIST (L (RDTBL-MAKE-SYMBOL-BUT-LAST *READTABLE*))
			     (AND (EQ (CAR STATE) (CAR L))
				  (EQ (CDR STATE) (CDR L))
				  (RETURN T))))
			  (T NIL)))
		 (SETQ CHAR (AREF STRING I))
		 (COND ((OR (NOT (NUMBERP STATE))	;FSM ran out OR
			    (NOT			;Translated char? then fsm loses
			      (= CHAR (RDTBL-TRANS *READTABLE* CHAR))))
			(OR MUST//			;Must we slash?
			    (DO ((I I (1+ I))) ((= I LEN))
			      (LET ((CODE (RDTBL-CODE *READTABLE* (AREF STRING I))))
				(WHEN (OR (= CODE ESCAPE-CODE)
					  (= CODE MULTIPLE-ESCAPE-CODE)
					  (= CODE CHARACTER-CODE-ESCAPE-CODE))
				  (SETQ MUST// T)
				  (RETURN NIL)))))
			(RETURN NIL)))
		 (SETQ STATE
		       (AREF FSM
			     STATE
			     (COND ((LET ((CODE (RDTBL-CODE *READTABLE* (AREF STRING I))))
				      (OR (= CODE ESCAPE-CODE)
					  (= CODE MULTIPLE-ESCAPE-CODE)
					  (= CODE CHARACTER-CODE-ESCAPE-CODE)))
				    (SETQ MUST// T)
				    (RDTBL-SLASH-CODE *READTABLE*))
				   ((AND (NUMBERP *PRINT-BASE*) (> *PRINT-BASE* 10.)
					 ( #/A CHAR (+ *PRINT-BASE* #/A -11.)))
				    (CDR (GETF (RDTBL-PLIST *READTABLE*) 'EXTENDED-DIGIT)))
				   (T
				    (RDTBL-CODE *READTABLE* CHAR))))))))
    (UNLESS FSMWINS (SEND STREAM :TYO (PTTBL-OPEN-QUOTE-SYMBOL *READTABLE*)))
    (COND ((OR MUST//
	       (AND FSMWINS (NEQ *PRINT-CASE* ':UPCASE)))
	   (DO ((I 0 (1+ I))
		(ESCAPE-CODE (RDTBL-ESCAPE-CODE *READTABLE*))
		(MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*))
		(CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*))
		(PREV-CHAR 0)
		CODE)
	       ((= I LEN))
	     (SETQ TEM (AREF STRING I))
	     (SETQ CODE (RDTBL-CODE *READTABLE* TEM))
	     (COND ((OR (= CODE ESCAPE-CODE)
			(= CODE MULTIPLE-ESCAPE-CODE)
			(= CODE CHARACTER-CODE-ESCAPE-CODE))
		    (SEND STREAM :TYO (PTTBL-SLASH *READTABLE*))
		    (SEND STREAM :TYO TEM))
		   ((OR (EQ *PRINT-CASE* ':DOWNCASE)
			(AND (EQ *PRINT-CASE* ':CAPITALIZE)
			     (ALPHANUMERICP PREV-CHAR)))
		    (SEND STREAM :TYO (CHAR-DOWNCASE TEM)))
		   (T
		    (SEND STREAM :TYO TEM)))
	     (SETQ PREV-CHAR TEM)))
	  (T (PRINT-RAW-STRING STRING STREAM FASTP)))
    (UNLESS FSMWINS (SEND STREAM :TYO (PTTBL-CLOSE-QUOTE-SYMBOL *READTABLE*)))
    ))

))

; From file PRINT.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(defun print-closure (closure stream fastp)
  (let ((bindings (closure-bindings closure)))
    (multiple-value-bind (function-name tem)
	(function-name (closure-function closure))
      (printing-random-object (closure stream :type :fastp fastp)
	(when tem
	  (print-object function-name 0 stream fastp)
	  (send stream :tyo #/space))
	(cond ((null bindings)
	       (send stream :tyo #/0))
	      ((null (cdr bindings))
	       (print-raw-string "(Lexical environment)" stream fastp))
	      ((interpreter-environment-closure-p closure)
	       (print-raw-string "(Interpreter)" stream fastp))
	      (t (print-fixnum (truncate (length bindings) 2) stream)))))))

))

; From file QMISC.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-CLOSURE (CL)
  (LET ((BINDINGS (CLOSURE-BINDINGS CL))
	(SYM NIL) (OFFSET NIL))
    (FORMAT T "~%~S is a closure of ~S:~%" CL (CLOSURE-FUNCTION CL))
    (CASE (LENGTH BINDINGS)
      (0 (FORMAT T "~&(No bindings)"))
      (1 (LET ((*PRINT-CIRCLE* T))		;certain to be needed
	   (FORMAT T "Lexical environment: ~S" (CAR BINDINGS))))
      (T (DO ((BINDINGS BINDINGS (CDDR BINDINGS)))
	     ((NULL BINDINGS))
	   (SETQ SYM (%FIND-STRUCTURE-HEADER (CAR BINDINGS))
		 OFFSET (%POINTER-DIFFERENCE (CAR BINDINGS) SYM))
	   (FORMAT T
		   "   ~A cell of ~S: ~40T~:[void~;~S~]~%"
		   (CASE OFFSET
		     (0 "Print name")
		     (1 "Value")
		     (2 "Function")
		     (3 "Property list")
		     (4 "Package"))
		   SYM (LOCATION-BOUNDP (CADR BINDINGS))
		   (AND (LOCATION-BOUNDP (CADR BINDINGS))
			(CAADR BINDINGS))))))
    (DESCRIBE-1 (CLOSURE-FUNCTION CL))))

))

; From file POSS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN EDIT-DEFINITIONS (DISPLAY-NAMES-FLAG DEFINITION-TYPE ITEM-LIST
			 HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "List and//or edit definitions of a bunch of objects.
DISPLAY-NAMES-FLAG says display a mouse sensitive list of the objects;
 otherwise go ahead right away and edit the first one.
DEFINITION-TYPE, if not nil, says the type of definition we should edit.
ITEM-LIST is a list of elements (printed-string . object-name),
 one for each object to be edited.  A file name can be used instead
 of an object, to mean /"some unidentified thing in that file/".
HEADING-STRING is a format control string for printing a heading for the list.
NOT-FOUND-STRING is a format control string for printing a message
 saying that the list was empty.
FORMAT-ARGS are available to either format control string when it is used."
  (AND DISPLAY-NAMES-FLAG
       (SEND *STANDARD-OUTPUT* ':FRESH-LINE))
  (COND ((NULL ITEM-LIST)
	 (IF DISPLAY-NAMES-FLAG
	     (APPLY #'FORMAT T NOT-FOUND-STRING FORMAT-ARGS)
	   (APPLY #'FORMAT *QUERY-IO* NOT-FOUND-STRING FORMAT-ARGS))
	 DIS-NONE)
	(DISPLAY-NAMES-FLAG
	 (APPLY 'FORMAT T HEADING-STRING FORMAT-ARGS)
	 (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
	 (SEND *STANDARD-OUTPUT* ':TYO #/CR)	;Blank line after heading
	 (SEND *STANDARD-OUTPUT* ':ITEM-LIST 'FUNCTION-NAME ITEM-LIST)
	 (FORMAT T
		 "~&Type ~A to ~:[start editing these~;edit this~].~%"
		 (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-H-P)
		 (= (LENGTH ITEM-LIST) 1))
	 (INSERT-DEFINITIONS-POSSIBILITIES
	   (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
	   (MAPCAR #'CDR ITEM-LIST)
	   DEFINITION-TYPE)
	 DIS-NONE)
	(T (INSERT-DEFINITIONS-POSSIBILITIES
	     (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
	     (MAPCAR #'CDR ITEM-LIST)
	     DEFINITION-TYPE)
	   (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-H-P)
	   (COM-GO-TO-NEXT-POSSIBILITY))))

))

; From file POSS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN EDIT-DEFINITIONS (DISPLAY-NAMES-FLAG DEFINITION-TYPE ITEM-LIST
			 HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "List and//or edit definitions of a bunch of objects.
DISPLAY-NAMES-FLAG says display a mouse sensitive list of the objects;
 otherwise go ahead right away and edit the first one.
DEFINITION-TYPE, if not nil, says the type of definition we should edit.
ITEM-LIST is a list of elements (printed-string . object-name),
 one for each object to be edited.  A file name can be used instead
 of an object, to mean /"some unidentified thing in that file/".
HEADING-STRING is a format control string for printing a heading for the list.
NOT-FOUND-STRING is a format control string for printing a message
 saying that the list was empty.
FORMAT-ARGS are available to either format control string when it is used."
  (AND DISPLAY-NAMES-FLAG
       (SEND *STANDARD-OUTPUT* ':FRESH-LINE))
  (COND ((NULL ITEM-LIST)
	 (IF DISPLAY-NAMES-FLAG
	     (APPLY #'FORMAT T NOT-FOUND-STRING FORMAT-ARGS)
	     (APPLY #'FORMAT *QUERY-IO* NOT-FOUND-STRING FORMAT-ARGS))
	 DIS-NONE)
	(DISPLAY-NAMES-FLAG
	 (APPLY #'FORMAT T HEADING-STRING FORMAT-ARGS)
	 (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
	 (SEND *STANDARD-OUTPUT* ':TYO #/CR)	;Blank line after heading
	 (SEND *STANDARD-OUTPUT* ':ITEM-LIST 'FUNCTION-NAME ITEM-LIST)
	 (FORMAT T
		 "~&Type ~A to ~:[start editing these~;edit this~].~%"
		 (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
		 (= (LENGTH ITEM-LIST) 1))
	 (INSERT-DEFINITIONS-POSSIBILITIES
	   (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
	   (MAPCAR #'CDR ITEM-LIST)
	   DEFINITION-TYPE)
	 DIS-NONE)
	(T (INSERT-DEFINITIONS-POSSIBILITIES
	     (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
	     (MAPCAR #'CDR ITEM-LIST)
	     DEFINITION-TYPE)
	   (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
	   (COM-GO-TO-NEXT-POSSIBILITY))))

))

; From file POSS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN EDIT-SECTIONS-DISPLAY (SECTIONS HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "Print the names of SECTIONS mouse sensitively, and also insert possibilities to visit them.
SECTIONS is a list of section-nodes.
HEADING-STRING and NOT-FOUND-STRING are two format strings, both of which
operate on FORMAT-ARGS, to make a string announcing the results
and a string saying that there are no more results.
They could be /"~A:/" and /"No more ~A./"."
  (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
  (COND ((NULL SECTIONS)
	 (APPLY #'FORMAT T NOT-FOUND-STRING FORMAT-ARGS))
	(T
	 ;; Insert the possibilities in the possibilities buffer first
	 ;; so that if the list is so long you don't want to watch it print out
	 ;; you can abort and still have it for later.
	 (INSERT-SECTIONS-POSSIBILITIES
	   (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
	   SECTIONS)
	 (APPLY #'FORMAT T HEADING-STRING FORMAT-ARGS)
	 (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
	 (SEND *STANDARD-OUTPUT* ':TYO #/CR)	;Blank line after heading
	 (SEND *STANDARD-OUTPUT* ':ITEM-LIST 'SECTION-NAME
	       (MAPCAR #'(LAMBDA (SECTION)
			   (CONS (FORMAT:OUTPUT NIL (PRIN1 (SECTION-NODE-NAME SECTION)))
				 SECTION))
		       SECTIONS))
	 (FORMAT T
		 "~&Type ~A to ~:[start editing these~;edit this~].~%"
		 (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
		 (= (LENGTH SECTIONS) 1)))))

))

; From file POSS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN EDIT-SECTIONS-NO-DISPLAY (SECTIONS HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "Insert a group of possibilities to visit SECTIONS, and visit the first one.
SECTIONS is a list of section-nodes.
HEADING-STRING and NOT-FOUND-STRING are two format strings, both of which
operate on FORMAT-ARGS, to make a string announcing the results
and a string saying that there are no more results.
They could be /"~A:/" and /"No more ~A./"."
  (COND ((NULL SECTIONS)
	 (APPLY #'FORMAT *QUERY-IO* NOT-FOUND-STRING FORMAT-ARGS)
	 DIS-NONE)
	(T (INSERT-SECTIONS-POSSIBILITIES
	     (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
	     SECTIONS)
	   (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
	   (COM-GO-TO-NEXT-POSSIBILITY))))

))

; From file POSS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN EDIT-FILES-WARNINGS (FILES)
  "Begin editing the recorded warnings of FILES.
Initializes the warnings buffer with possibilities
and visits the first one in two-window mode."
  (SET-BUFFER-CONTENTS-TO-WARNINGS
    (OR (FIND-BUFFER-NAMED "*Warnings*")
	(LET ((*INTERVAL* (FIND-BUFFER-NAMED "*Warnings*" T)))
	  (SETF (BUFFER-SAVED-MAJOR-MODE *INTERVAL*) 'WARNINGS-MODE)
	  *INTERVAL*))
    FILES)
  (FORMAT *QUERY-IO* "~&Use ~A to go to the next function that has warnings."
	  (KEY-FOR-COMMAND-SET-C-. 'COM-EDIT-NEXT-WARNING #/C-SH-W))
  (COM-EDIT-NEXT-WARNING))

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-PATCHES (&REST OPTIONS &AUX TEM SOMETHING-CHANGED)
  "Load any new patches for one or more systems.
Options can include these symbols:
 :NOSELECTIVE - don't ask about each patch.
 :SILENT or :NOWARN - don't print out any information on loading patches
   (and also don't ask).
 :VERBOSE - says to print out information about loading each patch.
   This is the default and is only turned off by :silent and :nowarn.
 :UNRELEASED - says to load or consider unreleased patches.
   Once unreleased patches have been loaded, a band may not be dumped.
 :FORCE-UNFINISHED - load all patches that have not been finished yet,
   if they have QFASL files.  This is good for testing patches.
 :NOOP - do nothing
 :SITE - load latest site configuration info.
 :NOSITE - do not load latest site configuration info.
   :SITE is the default unless systems to load patches for are specified.

Options can also include :SYSTEMS followed by a list of systems to load patches for.
One or more names of systems are also allowed.

LOAD-PATCHES returns T if any patches were loaded, otherwise NIL."
  (CATCH-ERROR-RESTART (SYS:REMOTE-NETWORK-ERROR
			 "Give up on trying to load patches.")
    (LET ((SYSTEM-NAMES NIL)			;A-list of systems to load patches for.
	  (SELECTIVE-P T)			;Ask the user.
	  (VERBOSE-P T)				;Tell the user what's going on.
	  (UNRELEASED-P NIL)
	  (SITE-SPECIFIED-P NIL)
	  (SITE-P T)
	  (FORCE-THROUGH-UNFINISHED-PATCHES-P NIL))
      (DO ((OPTS OPTIONS (CDR OPTS)))
	  ((NULL OPTS))
	(SELECTQ (CAR OPTS)
	  (:SYSTEMS
	   (SETQ OPTS (CDR OPTS))
	   (SETQ SYSTEM-NAMES
		 (IF (CONSP (CAR OPTS))
		     (MAPCAR #'GET-PATCH-SYSTEM-NAMED (CAR OPTS))
		   (LIST (GET-PATCH-SYSTEM-NAMED (CAR OPTS)))))
	   (UNLESS SITE-SPECIFIED-P
	     (SETQ SITE-P NIL)))
	  ((:SILENT :NOWARN) (SETQ VERBOSE-P NIL SELECTIVE-P NIL))
	  (:VERBOSE (SETQ VERBOSE-P T))
	  (:SELECTIVE (SETQ SELECTIVE-P T))
	  (:SITE (SETQ SITE-P T SITE-SPECIFIED-P T))
	  (:NOOP NIL)
	  (:NOSITE (SETQ SITE-P NIL SITE-SPECIFIED-P T))
	  (:UNRELEASED (SETQ UNRELEASED-P T))
	  (:NOSELECTIVE (SETQ SELECTIVE-P NIL))
	  (:FORCE-UNFINISHED (SETQ FORCE-THROUGH-UNFINISHED-PATCHES-P T))
	  (OTHERWISE
	    (COND ((AND (OR (SYMBOLP (CAR OPTS)) (STRINGP (CAR OPTS)))
			(SETQ TEM (GET-PATCH-SYSTEM-NAMED (CAR OPTS) T)))
		   (PUSH TEM SYSTEM-NAMES)
		   (UNLESS SITE-SPECIFIED-P
		     (SETQ SITE-P NIL)))
		  (T (FERROR NIL "~S is not a LOAD-PATCHES option and not a system name."
				 (CAR OPTS)))))))
      (WITH-SYS-HOST-ACCESSIBLE
	(LET-IF VERBOSE-P ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
	  (WHEN SITE-P
	    (WHEN VERBOSE-P
	      (FORMAT T "~%Checking whether site configuration has changed..."))
	    (IF (IF SELECTIVE-P
		    (MAKE-SYSTEM "SITE" :NO-RELOAD-SYSTEM-DECLARATION)
		  (IF VERBOSE-P
		      (MAKE-SYSTEM "SITE" :NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION)
		      (MAKE-SYSTEM "SITE" :NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION :SILENT)))
		(SETQ SOMETHING-CHANGED T)
	      (WHEN VERBOSE-P (FORMAT T "  it hasn't.")))
	    (LOAD-PATCHES-FOR-LOGICAL-PATHNAME-HOSTS))
	  (OR SYSTEM-NAMES (SETQ SYSTEM-NAMES PATCH-SYSTEMS-LIST))
	  (LET ((FIRST-SYSTEM T))  ; This is the first system being patched.
	    (DOLIST (PATCH SYSTEM-NAMES)
	      (CATCH-ERROR-RESTART (ERROR "Give up on patches for ~A." (CAR PATCH))
		(LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH T))
		       (NEW-VERS (PATCH-DIR-VERSION-LIST PATCH-DIR))
		       (MAJOR (PATCH-VERSION PATCH))
		       PATCHES-NOT-LOADED
		       (CHANGE-STATUS T)		;Ok to change the system status
		       (UNRELEASED-CONSIDERED NIL)	;T if considering unreleased patches.
		       (PATCH-SKIPPED NIL)	;T if considering patches after skipping one.
		       (PROCEED-FLAG (NOT SELECTIVE-P))) ; Has the user said to proceed?
		  (IF (AND (NULL PATCH-DIR) VERBOSE-P)
		      (FORMAT T "~&Skipping system ~A, whose patch directory cannot be accessed.~%"
			      (CAR PATCH))
		    ;; Get list of patches of this system not already loaded.
		    (SETQ PATCHES-NOT-LOADED
			  (CDR (MEMASSQ (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
					NEW-VERS)))
		    ;; Maybe announce the system.
		    (WHEN (AND PATCHES-NOT-LOADED VERBOSE-P)	;verbose and silent is nonsense
		      (FRESH-LINE *STANDARD-OUTPUT*)
		      (UNLESS FIRST-SYSTEM (TERPRI))
		      (FORMAT T "~&Patches for ~A (Current version is ~D.~D):"
			      (PATCH-NAME PATCH) MAJOR (CAAR (LAST NEW-VERS))))
		    (DOLIST (VERSION PATCHES-NOT-LOADED)
		      (LET* ((FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH) :PATCH-FILE
							      (PATCH-VERSION PATCH)
							      (VERSION-NUMBER VERSION)
							      :QFASL)))
			;; NIL is used to mark patches that are reserved, but not finished.
			;; We can't load any more patches without this one, in order to
			;; make sure that any two systems claiming to be version xx.yy
			;; always have exactly the same set of patches loaded.  Punt.
			;; If someone forgets to finish a patch, we assume a hacker will
			;; eventually see what is happening and fix the directory to unstick
			;; things.  We might at least say the patches are unfinished.
			(UNLESS (VERSION-EXPLANATION VERSION)
			  (WHEN VERBOSE-P
			    (FORMAT T "~&There are unfinished patches in ~A."
				    (PATCH-NAME PATCH)))
			  (UNLESS FORCE-THROUGH-UNFINISHED-PATCHES-P
			    (RETURN)))
			(WHEN (VERSION-UNRELEASED VERSION)
			  (WHEN VERBOSE-P
			    (FORMAT T "~&There are unreleased patches in ~A."
				    (PATCH-NAME PATCH)))
			  (OR
			    FORCE-THROUGH-UNFINISHED-PATCHES-P 
			    UNRELEASED-P
			    UNRELEASED-CONSIDERED
			    (EQ (PATCH-STATUS PATCH) :INCONSISTENT)
			    (AND SELECTIVE-P
				 (WITH-TIMEOUT ((* 5 60. 60.)
						(FORMAT T " -- timed out, No.")
						NIL)
				   (FORMAT T "~&Such patches are subject to change; therefore,
  you should not load them if you are going to dump a band.
  If you are not going to dump a band, it is reasonable
  to load these patches to benefit from the improvements in them.")
				   (SETQ PROCEED-FLAG NIL)
				   (Y-OR-N-P "Consider the unreleased patches? (Automatic No after 5 minutes) ")))
			    (RETURN))
			  (SETQ UNRELEASED-CONSIDERED T))
			(WHEN VERBOSE-P
			  (PRINT-PATCH (PATCH-VERSION PATCH) VERSION))
			(SELECTQ-EVERY
			  (COND (PROCEED-FLAG)
				(T (WITH-TIMEOUT ((* 5 60. 60.)
						  (FORMAT T " -- timed out, Proceed.")
						  'PROCEED)
				     (FQUERY '(:CHOICES (((T "Yes.") #/Y #/SP #/T #/HAND-UP)
							 ((NIL "No.") #/N #/RUBOUT #/HAND-DOWN)
							 ((PROCEED "Proceed.") #/P)))
					     "Load? (Automatic Proceed after 5 minutes) "))))
			  (NIL
			   ;; "No", don't load any more for this system.
			   ;; Also don't change the status.
			   ;; Except, if we are considering unreleased patches,
			   ;; loading out of order is no worse than loading unreleased
			   ;; patches in the first place, so keep on offering.
			   (SETQ CHANGE-STATUS NIL)
			   (UNLESS (OR FORCE-THROUGH-UNFINISHED-PATCHES-P
				       UNRELEASED-CONSIDERED)
			     (RETURN NIL))
			   (WHEN (EQ VERSION (CAR (LAST PATCHES-NOT-LOADED)))
			     ;; Don't give a spiel about following patches
			     ;; if there are none.
			     (RETURN NIL))
			   (UNLESS (OR PATCH-SKIPPED
				       (EQ (PATCH-STATUS PATCH) :INCONSISTENT))
			     (FORMAT T "~&If you load any following patches for this system,
  they will be out of sequence, so you must not dump a band.")
			     (SETQ PATCH-SKIPPED T)))
			  (PROCEED
			   ;; "Proceed" with the rest for this system.
			   (SETQ PROCEED-FLAG T))
			  ((T PROCEED)
			   ;; "Yes" or "Proceed", do this one.
			   (SETQ SOMETHING-CHANGED T)
			   ;; Unfinished, unreleased or out of sequence =>
			   ;;  mark system as inconsistent.
			   (WHEN (OR PATCH-SKIPPED
				     (NULL (VERSION-EXPLANATION VERSION))
				     (VERSION-UNRELEASED VERSION))
			     (UNLESS (EQ (PATCH-STATUS PATCH) :INCONSISTENT)
			       (SETF (PATCH-STATUS PATCH) :INCONSISTENT)
			       (FORMAT T "~&~A is now inconsistent; do not dump a band."
				       (PATCH-NAME PATCH))))
			   ;; Avoid error if non ex file, if patch is known to be unfinished.
			   (CONDITION-CASE-IF (NULL (VERSION-EXPLANATION VERSION)) ()
			       (LOAD FILENAME NIL NIL T (NOT VERBOSE-P))	; Don't set default,
			     (FS:FILE-NOT-FOUND
			      (WHEN VERBOSE-P
				(FORMAT T "~&File ~A does not exist, ignoring this patch."
					FILENAME))))
			   (PUSH VERSION (PATCH-VERSION-LIST PATCH))))))
		    (AND CHANGE-STATUS
			 (NEQ (PATCH-STATUS PATCH) :INCONSISTENT)
			 (LET ((NEW-STATUS (PATCH-DIR-STATUS PATCH-DIR)))
			   (COND ((NEQ (PATCH-STATUS PATCH) NEW-STATUS)
				  (SETQ SOMETHING-CHANGED T)
				  (WHEN VERBOSE-P
				    (FORMAT T "~&~A is now ~A."
					    (PATCH-NAME PATCH)
					    (FOURTH (ASSQ NEW-STATUS
							  SYSTEM-STATUS-ALIST))))
				  ;; Update the status.
				  (SETF (PATCH-STATUS PATCH) NEW-STATUS))))))))
	      (SETQ FIRST-SYSTEM NIL)))))))
  SOMETHING-CHANGED)

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(defun load-patches-for-logical-pathname-hosts ()
  (dolist (host fs::*logical-pathname-host-list*)
    (if (send host :get 'fs:make-logical-pathname-host)
	(fs:make-logical-pathname-host host :warn-about-redefinition nil))))

))

; From file PATHST.LISP OZ:<L.IO.FILE> OZ:
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
  (let ((host (get-pathname-host host-name t))
	file-id loaded-id)
    (catch-error-restart ((fs:remote-network-error fs:file-not-found)
			  "Give up loading logical pathname translations for ~A" host-name)
      (when (typep host 'logical-host)
	(setq loaded-id (send host :get 'make-logical-pathname-host))
	(if warn-about-redefinition
	    (format *error-output* "~&Warning: The logical host ~A is being redefined" host)))
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
    (cond ((typep (setq host (fs:get-pathname-host host-name nil)) 'logical-host)
	   (send host :set :get 'make-logical-pathname-host file-id)
	   host)
	  (t (format *error-output*
		     "~&Warning: The logical host ~S was not defined by FS:MAKE-LOGICAL-PATHNAME-HOST.")
	     nil))))

))

(fs:make-logical-pathname-host "SYS" :warn-about-redefinition nil)
