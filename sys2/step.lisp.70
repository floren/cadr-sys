;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Lowercase:T; Readtable:CL -*-

;Note: This is a commonlisp file!

;;; Ultra-simple stepper for lisp-machine.
;;; Wins with multiple values
;;; Does not attempt to win with editor top level

;NOTES:
; The way it decides whether it needs to reprint the form when showing
; you the values is pretty kludgey right now.  Can it check the cursorpos
; or ask itself whether it typed anything or something?
;
; Would like to be able to evaluate and/or substitute in atoms and forms
; without having to break first.
;
; Would like to be able to type c-A and have it stop after evaluating the
; args, before calling the function.
;
; Raid registers
;
; Hook up to DDT?
; 
; If an error happens, user should be able to throw back into the stepper.

(defvar *step-level* nil
  "Depth within STEP-EVALHOOK, minus one, within call to STEP")
(defvar *step-array* nil
  "Holds forms to evaluate, indexed by *STEP-LEVEL* value.")
(defvar *step-apply-p-array* nil
  "Holds the APPLY-P flag for each level, indexed by *STEP-LEVEL* value.")
(defvar *step-max* nil
  "Do not tell user about evaluations with *STEP-LEVEL* deeper than this.")
(defvar *step-form* nil
  "Form to be or just evaluated, in STEP command loop")
(defvar *step-value* nil
  "First value just computed.  May be changed in a breakpoint.")
(defvar *step-values* nil
  "List of values just computed.  May be changed in a breakpoint.")

(defvar	*step-auto* nil)
;if non NIL, simulate cntrl-n at step-cmdr.
; normal printout produced unless NO-PRINT.
;  User's program can turn on auto mode by
;  (si:*step-auto-on &optional (mode 'no-print))
; and (si:*step-auto-off) to reable stepping.

;;; Main entry point.
(defun step (form &optional *step-auto* &aux (*step-level* -1) (*step-max* 0)
	     (*step-array* (make-array #o200))
	     (*step-apply-p-array* (make-array #o200)))
  "Evaluate FORM with stepping.  It stops before and after each subexpression.
Type the Help key when you are in the stepper for a list of stepper commands."
  (binding-interpreter-environment (())
    (step-evalhook form ())))

;;; This is for TRACE, mainly.  The idea is to do an apply,
;;; stepping under it but not showing the user the apply itself.
(defun step-apply (fcn args &aux (*evalhook* #'step-hook))
  (apply fcn args))

;;; Main entry point.
(defun step-hook (form &optional environment &aux *step-auto* (*step-level* -1) (*step-max* 0)
		  (*step-array* (make-array #o200))
		  (*step-apply-p-array* (make-array #o200)))
  (step-evalhook form environment))
  
;;; Check for macros, they are treated specially.
(defun step-macro-p (form)
  (and (consp form)
       (symbolp (car form))
       (fboundp (car form))
       (eq (car-safe (symbol-function (car form))) 'macro)))

(defun step-auto-on (&optional (mode 'no-print))
  (setq *step-auto* mode))

(defun step-auto-off ()
  (setq *step-auto* nil))

;;; Print a form, suitably indented, marked, and truncated to one line.
(defun step-print-form (form level apply-p)
  (cli:terpri)
  (do ((n (* 2 level) (1- n)))
      ((= n 0))
    (write-char #\sp))
  (write-char (cond (apply-p #\)
		    ((step-macro-p form) #\)
		    (t #\)))
  (write-char #\sp)
  (if apply-p
      (progn (print-truncated (function-name (car form)) 75.)
	     (princ ": ")
	     (print-elements-truncated (cdr form) 90. 75.))
      (print-truncated form 75.)))

;;; Print whatever is necessary, read a command, set special variables
;;; and return how to proceed:  eval (just eval), evalhook (recurse), more options later.
;;; If calling for eval, *step-values* is nil, otherwise calling for return.
(defun step-cmdr (form values print-form-p &optional apply-p)
  (declare (special apply-p))
  (prog (ch ch1
	 (*standard-input* *query-io*)
	 (*standard-output* *query-io*))
	(if step-auto
	    (if (eq step-auto 'no-print)
		(progn (setq *step-max* (1+ *step-level*)) (return 'evalhook))))
	(and print-form-p
	     (step-print-form form *step-level* apply-p))
     pv (do ((l values (cdr l))
	     (ch #\ #\))
	    ((null l))
	  (terpri-if-insufficient-space 80.)
	  (write-char #\sp) (write-char ch) (write-char #\sp)
	  (print-truncated (car l) 98.))	;Several windows lose if this is 100.
     rd (setq ch1 (if step-auto #\c-N (read-char *standard-input*)))
	(setq ch (char-upcase ch1))
	(cond ((eq ch #\call) (break "for CALL key."))
	      ((eq ch #\sp) (setq *step-max* *step-level*) (return 'eval))
	      ((eq ch #\c-U) (setq *step-max* (max 0 (1- *step-level*))) (return 'eval))
	      ((eq ch #\c-N) (setq *step-max* (1+ *step-level*)) (return 'evalhook))
	      ((eq ch #\c-X) (setq *step-max* -1) (return 'eval))
	      ((and (eq ch #\c-A)
		    (not apply-p))
	       (setq *step-max* (1+ *step-level*)) (return 'applyhook))
	      ((eq ch #\c-B)
	       (break)
	       (setq ch 0)
	       (setf (cli:aref *step-array* *step-level*) *step-form*)
	       (setf (cli:aref *step-apply-p-array* *step-level*) apply-p)
	       (go redis1))
	      ((eq ch #\c-E)
	       (ed)
	       (setq ch 10.)
	       (go redisplay))
	      ((memq ch '(#\Clear-Screen #\c-L))
	       (setq ch 10.)
	       (go redisplay))
	      ((eq ch #\m-L)
	       (setq ch 10.)
	       (go redis1))
	      ((eq ch #\c-m-L)
	       (setq ch *step-level*)
	       (go redisplay))
	      ((or (eq ch #\c-G) (eq ch #\c-T))
	       (setq ch (if (eq ch #\c-G) #'grind-top-level #'print))
	       (cond ((null values) (funcall ch form))
		     ((do ((l values (cdr l)))
			  ((null l))
			(funcall ch (car l)))))
	       (go rd))
	      ((= (char-code ch) (char-code #\HELP))
	       (sys:with-help-stream (help-str :label "Stepper help")
		 (cli:terpri help-str)
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
		 (cli:terpri help-str)
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
	      ((zerop (char-bits char))
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
	       (cli:terpri)
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
	  (step-print-form (cli:aref *step-array* i) i (cli:aref *step-apply-p-array* i)))
	(go pv)))

;;; This is evalhooked in in place of EVAL.  Works by calling step-cmdr
;;; to let the user see what's going on and say what to do, then continues
;;; evaluation using either EVAL or EVALHOOK based on what the user typed.
;;; Has special hair for macros and for atoms.
(defun step-evalhook (*step-form* &optional environment)
  (binding-interpreter-environment (environment)
    (let ((*step-level* (1+ *step-level*))
	  (*step-value*) (*step-values*)
	  tem val)
      (tagbody
	  (when ( *step-level* (array-length *step-array*))
	    (adjust-array-size *step-array* (+ #o100 *step-level*))
	    (adjust-array-size *step-apply-p-array* (+ #o100 *step-level*)))
       mc (setf (cli:aref *step-array* *step-level*) *step-form*)
	  (setf (cli:aref *step-apply-p-array* *step-level*) nil)
	  (cond ((atom *step-form*)
		 (setq *step-values* (list (eval1 *step-form*)))
		 (setq tem 'atom)
		 (go rl))
		(( *step-level* *step-max*)
		 (setq tem (step-cmdr *step-form* nil t)))
		(t (setq tem 'eval)))
	  (cond ((step-macro-p *step-form*)
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
	      (setq tem (step-cmdr *step-form* *step-values* (neq tem 'eval)))
	    (setq tem 'eval))
	  (and (neq *step-value* val) (return-from step-evalhook *step-value*))
       rt (if (null (cdr *step-values*))
	      (return-from step-evalhook (car *step-values*))
	    (return-next-value (car *step-values*))
	    (setq *step-values* (cdr *step-values*))
	    (go rt))))))

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
       mc (setf (cli:aref *step-array* *step-level*) *step-form*)
	  (setf (cli:aref *step-apply-p-array* *step-level*) t)
	  (if ( *step-level* *step-max*)
	      (setq tem (step-cmdr *step-form* nil t t))
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
	      (setq tem (step-cmdr *step-form* *step-values* (neq tem 'eval) t))
	      (setq tem 'eval))
	  (when (neq *step-value* val)
	    (return-from step-applyhook *step-value*))
       rt (if (null (cdr *step-values*))
	      (return-from step-applyhook (car *step-values*))
	    (return-next-value (car *step-values*))
	    (setq *step-values* (cdr *step-values*))
	    (go rt))))))

;;;; PRINT abbreviated spacewise rather than listwise

(defvar print-truncated)			;YECH

(defun terpri-if-insufficient-space (percent-width)
  (let ((x (truncate (* percent-width (send *standard-output* :inside-size)) 100.)))
    (and ( (send *standard-output* :read-cursorpos :pixel) x)
	 (cli:terpri))))

(defun print-truncated (sexp percent-width)
  (let ((print-truncated (truncate (* percent-width (send *standard-output* :inside-size))
				   100.)))
    (*catch 'print-truncated
	    (prin1 sexp (closure '(print-truncated *standard-output*)
				 #'print-truncated-stream)))))

(defun print-elements-truncated (list truncation-percent-width terpri-percent-width)
  (dolist (element list)
    (terpri-if-insufficient-space terpri-percent-width)
    (print-truncated element truncation-percent-width)
    (write-char #\sp)))

(defun print-truncated-stream (op &optional arg1 &rest rest)
  (case op
    (:tyo
     (cond (( (send *standard-output* :read-cursorpos :pixel)
		print-truncated)
	    (*throw 'print-truncated nil))
	   (t (send *standard-output* :tyo arg1))))
    (:which-operations '(:tyo))
    (otherwise
     (stream-default-handler 'print-truncated-stream op arg1 rest))))
