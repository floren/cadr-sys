;;; -*- Mode:LISP; Package:EH; Patch-File:T; Base:8; Readtable:T -*-
;;; Patch file for System version 99.19
;;; Reason:
;;;  More eh technology
;;;   m-S sets + wrt correct SG
;;;   m-I for looking at ivars
;;;   c-m-H with arg lists all active handlers in frame
;;;   c-m-D describes *
;;;   Fix bogosity wrt ivar "specialness" in evalling, c-m-S, m-S, etc
;;;  LOCATION-CONTENTS  CONTENTS
;;;  UNWIND-PROTECT-CASE for the granoloid wimps out there.
;;;  WITHOUT-FLOATING-UNDERFLOW-TRAPS
;;;  Define the new fef header types so that tools don't lose
;;; Written 7-Feb-85 04:51:11 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.17, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.


(eval-when (load compile eval)
(defun si::xglobalize (x p)
  (setq x (intern x p))
  (globalize x 'global)
  (globalize x 'lisp)
  x)
(si::xglobalize "LOCATION-CONTENTS" 'si)
(si::xglobalize "UNWIND-PROTECT-CASE" 'si)
(si::xglobalize "WITHOUT-FLOATING-UNDERFLOW-TRAPS" 'si)
(export (intern "FUNCTION-SPEC-REMPROP" 'si) 'si)
;; got left out of 99.18
(globalize (intern "FEF-DEBUGGING-INFO-PRESENT-P" 'si) 'sys)
(globalize (intern "%HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS" 'si) 'sys)
(globalize (intern "%HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS" 'si) 'sys)
(globalize (intern "%HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS" 'si) 'sys)
(globalize (intern "%HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS" 'si) 'sys)

) ;eval-when

; From file OZ:KANSAS:<L.SYS2>DESCRIBE.LISP.1 at 4-Feb-85 01:20:57
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; DESCRIBE  "

(defun describe-array (array)
  (let ((rank (array-rank array))
	(long-length-flag (%p-ldb-offset %%array-long-length-flag array 0)))
    (format t "~%This is an ~S type array. (element-type ~S)~%"
	    (array-type array) (array-element-type array))
    (case rank
      (0
       (FORMAT T "It is of zero rank.")
       )
      (1
       (format t "It is a vector, with a total size of ~D elements" (array-total-size array))
       )
      (t
       (format T "It is ~D-dimensional, with dimensions " rank)
       (dotimes (d rank) (format t "~D " (array-dimension array d)))
       (format t ". Total size ~D elements" (array-total-size array))))
    (when (array-has-leader-p array)
      (let ((length (array-leader-length array)))
	(if (and (eq rank 1)
		 (eq length 1)
		 (fixnump (array-leader array 0)))
	    (format t "~%It has a fill-pointer: ~S" (fill-pointer array))
	  (format t "~%It has a leader, of length ~D. Contents:" length)
	  (format t "~%  Leader 0~:[~; (fill-pointer)~]: ~S"
		  (and (eq rank 1) (fixnump (array-leader array 0))) (array-leader array 0))
	  (dotimes (i (1- length))
	    (format t "~%  Leader ~D: ~S" (1+ i) (array-leader array (1+ i)))))))
    (when (array-displaced-p array)
      (cond ((array-indirect-p array)
	     (format t "~%The array is indirected to ~S"
		     (%p-contents-offset array (+ rank long-length-flag)))
	     (and (array-indexed-p array)
		  (format T ", with index-offset ~S"
			  (%p-contents-offset array (+ rank long-length-flag 2))))
	     (format t "~%Description of that array:")
	     (describe-1 (%p-contents-offset array (+ rank long-length-flag))))
	    (t (format t "~%The array is displaced to ~S"
		       (%p-contents-offset array (+ rank long-length-flag))))))))

))

; From file OZ:KANSAS:<L.SYS2>DESCRIBE.LISP.1 at 4-Feb-85 01:21:11
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; DESCRIBE  "

(defun describe-closure (cl &aux (*print-circle* t))
  (let ((bindings (closure-bindings cl))
	(sym nil) (offset nil))
    (format t "~%~S is a closure of ~S~%" cl (closure-function cl))
    (case (length bindings)
      (0 (format t "(No bindings)"))
      (1 (format t "Lexical environment:~%")
	 (loop for x in (caar bindings)
	       for i from 0
	       do (format t "  Slot ~D: ~S~%" i x)))
      (t
       (do ((bindings bindings (cddr bindings)))
	   ((null bindings))
	 (setq sym (%find-structure-header (car bindings))
	       offset (%pointer-difference (car bindings) sym))
	 (if (not (symbolp sym))
	     (format t "    ~S" (car bindings))
	   (format t "    ~[Print name~;Value~;Function~;Property list~;Package~] cell of ~S"
		   offset sym))
	 (format t ":~40T~:[void~;~S~]~%"
		 (location-boundp (cadr bindings))
		 (and (location-boundp (cadr bindings))
		      (contents (cadr bindings)))))))
    (unless (consp (closure-function cl))		;don't describe interpreted functions.
      (describe-1 (closure-function cl)))))

))

; From file OZ:OZ:<MLY.LL>EHC.LISP.7 at 7-Feb-85 09:03:47
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defconst command-dispatch-list
	  '((#/? com-help)
	    (#/help com-help)
	    (#/line com-down-stack)
	    (#/form com-clear-and-show)
	    (#/return com-up-stack)
	    (#/resume com-proceed)
	    (#/abort com-abort)
	    (#/rubout com-rubout)
	    
	    (#/c-- com-number)
	    (#/c-0 com-number 10.)		;control-digits
	    (#/c-A com-arglist)
	    (#/c-B com-short-backtrace)
	    (#/c-C com-proceed)
	    (#/c-D com-proceed-trap-on-call)
	    (#/c-E com-edit-frame-function)
	    (#/c-I com-set-macro-single-step)
	    (#/c-L com-clear-and-show)
	    (#/c-M com-bug-report)
	    (#/c-N com-down-stack)
	    (#/c-P com-up-stack)
	    (#/c-R com-return-a-value)
	    (#/c-S com-search)
	    (#/c-T com-throw)
	    (#/c-X com-toggle-frame-trap-on-exit)
	    (#/c-Z com-top-level-throw)
	    
	    (#/m-- com-number)
	    (#/m-0 com-number 10.)		;meta-digits
	    (#/m-< com-top-stack)
	    (#/m-> com-bottom-stack)
	    (#/m-B full-backtrace)
	    (#/m-D com-toggle-trap-on-call)
;>>
	    (#/m-I com-print-instance-variable)
	    (#/m-L com-clear-and-show-all)
	    (#/m-N com-down-stack-all)
	    (#/m-P com-up-stack-all)
	    (#/m-R com-reinvoke-new-args)
	    (#/m-S com-print-variable-frame-value)
	    (#/m-T com-show-stack-temporaries)
	    (#/m-X com-set-all-frames-trap-on-exit)
	    
	    (#/c-m-- com-number)
	    (#/c-m-0 com-number 10.)		;control-meta-digits
	    (#/c-m-A com-get-arg)
	    (#/c-m-B full-backtrace-uninteresting)
;>>
	    (#/c-m-C com-print-open-catch-frames)
;>>
	    (#/c-m-D com-describe-*)
;>>
	    (#/c-m-E com-describe-lexical-environment)
	    (#/c-m-F com-get-function)
	    (#/c-m-H com-print-frame-handlers)
	    (#/c-m-L com-get-local)
	    (#/c-m-N com-down-stack-uninteresting)
	    (#/c-m-P com-up-stack-uninteresting)
	    (#/c-m-Q com-describe-proceed-types)
	    (#/c-m-R com-return-reinvocation)
	    (#/c-m-S com-print-frame-bindings)
	    (#/c-m-T com-get-stack-temporary)
	    (#/c-m-U com-up-to-interesting)
	    (#/c-m-V com-get-value)
	    (#/c-m-W com-window-error-handler)
	    (#/c-m-X com-clear-all-frames-trap-on-exit)
	    
;>>
	    (#/c-sh-s com-set-breakpoint)
;>>
	    (#/c-sh-c com-clear-breakpoint)
;>>
	    (#/m-sh-c com-clear-all-breakpoints)
;>>
	    (#/c-sh-l com-list-breakpoints)
;>>
	    (#/m-sh-s com-macro-single-step)

	    (#/s-A com-proceed-specified-type 26.)
	    )
  "List of elements (character command-symbol) from which COMMAND-DISPATCH-TABLE is initialized.")

(makunbound 'command-dispatch-table)
(assure-dispatch-set-up)

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:29:04
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC"

(defvar *command-char* :unbound
  "While calling a debugger command, this is the command character.")

(defvar *special-commands* :unbound
  "List of special command keywords provided by this error.")

(defun command-loop (error-sg error-object
		     &aux function sexp 
		     (*evalhook* nil)
		     (*special-commands* nil)
		     (window-error-handler-old-window nil)
		     io-buffer
		     reading-command)
  (when error-object
    (setq *special-commands* (send error-object :special-command :which-operations))
    (send error-object :initialize-special-commands))
  (when (setq io-buffer (send *standard-input* :send-if-handles :io-buffer))
    (%bind (locf (tv:io-buffer-output-function io-buffer)) 'io-buffer-output-function)
    (%bind (locf (tv:io-buffer-input-function io-buffer)) nil))
  (inheriting-variables-from (error-sg)  ;Do this every time around the loop in case of setq
    (catch-error-restart ((sys:abort error) "Return to debugger command loop.")
      (catch 'quit
	(show-function-and-args error-sg)
	(warn-about-special-variables error-sg)
	(unless *inhibit-debugger-proceed-prompt*
	  (describe-proceed-types error-sg error-object)))))
  (error-restart (abort "Return to debugger command loop")
    (do ((numeric-arg nil nil)
	 (-)
	 (+ (symeval-in-stack-group '- error-sg))
	 (++ (symeval-in-stack-group '+ error-sg))
	 (+++ (symeval-in-stack-group '++ error-sg))
	 (* (symeval-in-stack-group '* error-sg))
	 (** (symeval-in-stack-group '** error-sg))
	 (*** (symeval-in-stack-group '*** error-sg))
	 (// (symeval-in-stack-group '// error-sg))
	 (//// (symeval-in-stack-group '//// error-sg))
	 (////// (symeval-in-stack-group '////// error-sg))
	 (*values* (symeval-in-stack-group '*values* error-sg)))
	(())
      (inheriting-variables-from (error-sg)  ;Do this every time around the loop in case of setq
	(unless error-handler-running
	  (setq error-depth 1))
	(catch-error-restart ((sys:abort error) "Return to debugger command loop.")
	  (catch 'quit
	    (fresh-line *standard-output*)
	    (dotimes (i error-depth)
	      (send *standard-output* :tyo #/))
	    (do-forever				;This loop processes numeric args
	      ;; Read the next command or sexp, with combined rubout processing.
	      (multiple-value-setq (function sexp)
		(command-loop-read))
	      ;; If it's a character, execute the definition or complain.
	      (cond ((numberp function)
		     (setq numeric-arg
			   (cond ((null numeric-arg)
				  (format t " Argument: ~C"
					  (if (eq function -1) #/- (digit-char function)))
				  function)
				 ((eq function -1)
				  (beep)
				  numeric-arg)
				 (t
				  (princ (digit-char function))
				  (+ function (* 10. numeric-arg))))))
		    (function
		     (if numeric-arg (tyo #/space))
		     (format t "~C " sexp)
		     (if (not (fdefinedp function))
			 (return (format t "~S undefined debugger function!!" function))
		       (let ((*command-char* (int-char sexp)))
			 (return (if (not numeric-arg)
				     (funcall function error-sg error-object)
				     (funcall function error-sg error-object numeric-arg))))))
		    ;; If there was no command, there was a sexp, so eval it.
		    (t
		     (catch 'quit
		       (setq +++ ++ ++ + + -)
		       (let (values)
			 (unwind-protect
			     (setq values (sg-eval-in-frame error-sg
							    (setq - sexp) current-frame t))
			   (push (unless (eq values error-flag) values) *values*))
			 (unless (eq values error-flag)
			   (setq ////// //// //// //)
			   (setq *** ** ** *)
			   (setq // values * (car //))
			   (dolist (value //)
			     (terpri)
			     (print-carefully () (funcall (or prin1 #'prin1) value))))))
		     (return))))))))))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:39:06
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun command-loop-read ()
  (prog (char sexp flag function)
     retry
	;; Read a character.
	(let ((reading-command t))
	  (setq char (send *standard-input* :tyi)))
	;; Now, if the char is special, echo and return it.
;character lossage
	(cond ((rassq char *proceed-type-special-keys*)
	       (return (values 'com-proceed-specified-type char)))
	      ((rassq char *special-command-special-keys*)
	       (return (values 'com-special-command char)))
	      ((or ( 0 (char-bits char))
		   (command-lookup char))
	       (when (setq function (command-lookup char))
		 (cond ((eq function 'com-rubout)
			(go retry))
		       ((eq function 'com-number)
			(setq function (or (digit-char-p (make-char char))
					   -1))))	;else it's a "-"
		 (return (values function char)))))
       ;; Otherwise, unread it and read an s-exp instead.
       (send *standard-input* :untyi char)
       (multiple-value (sexp flag)
	 (with-input-editing (*standard-input*  '((:full-rubout :full-rubout)
						  (:activation = #/end)
						  (:prompt " Eval: ")))
	   (si:read-for-top-level nil nil nil)))
       (when (eq flag ':full-rubout)
	 (go retry))
       (return (values nil sexp))))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:23:32
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun io-buffer-output-function (ignore char &aux tem)
  (cond ;; Blips shouldn't get here, but don't die
	((not (numberp char)) char)
	;; Don't intercept commands
	((and reading-command (command-lookup char)) char)
	((setq tem (assq char tv:kbd-intercepted-characters))
	 (multiple-value-prog1
	   (funcall (cadr tem) char)
	   (format t "~&Back to debugger.~%")))
	;; Compatibility with ancient history
;character lossage
	((eq char (char-int #/c-G))
	 (tv:kbd-intercept-abort (char-int #/c-G)))
	(t char)))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:24:50
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-print-frame-bindings (sg ignore &optional count
				 &aux start end (sp (sg-special-pdl sg)))
				 ;; self-included special-vars
  "Lists the names and values of all special variables bound by this frame."
  ;; Find range of special pdl for this frame together with
  ;; all uninteresting frames called by it.
  (do ((previous-int (sg-previous-interesting-active sg current-frame
						     innermost-visible-frame))
       (previous current-frame (sg-previous-active sg previous innermost-visible-frame)))
      ((equal previous previous-int))
    (multiple-value-bind (maybe-start maybe-end)
	(sg-frame-special-pdl-range sg previous)
      (setq start maybe-start)
      (and maybe-end (setq end (1+ maybe-end)))))
  (cond (start
	 (if (null count) (format t "~&Names and values of specials bound in this frame:~%"))
	 ;; Now look through the specials bound in this frame.
	 (do ((i start (+ i 2)))
	     (( i end))
	   (let ((sym (symbol-from-value-cell-location (aref sp (1+ i)))))
	     ;;(push tem special-vars)
	     ;;(if (eq sym 'self)
	     ;;  (setq self-included i))
	     (when (or (null count) (= count (lsh (- i start) -1)))
	       (format t "~:[~% ~S: ~;~&Value of ~S in this frame:~&   ~]" count sym)
	       (multiple-value-bind (val error)
		   (catch-error (funcall (or prin1 #'prin1) (aref sp i)) nil)
		 (cond (error (princ "void"))
		       (count (got-values val (aref sp (1+ i)) nil))))))))
	(t (format t "~&No specials bound in this frame"))))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:24:57
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-print-variable-frame-value (sg ignore &optional ignore &aux var)
  "Print the value of a special variable within the context of the current stack frame.
Prompts for the variable name."
  ;;(terpri)
  (with-input-editing (*standard-input*
			'(;(:full-rubout :full-rubout) -- too obnoxious
			  (:activation char= #/end)
			  (:prompt "Value in this frame of special variable: ")))
    (setq var (si:read-for-top-level nil nil nil))
    (multiple-value-bind (value boundflag loc)
	(symeval-in-stack-group var sg current-frame)
      (if boundflag
	  (got-values value loc nil)
	(princ "Void")))))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:25:08
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC"

(defun com-print-instance-variable (sg ignore &optional ignore)
  "Print the value of an instance VARIABLE of SELF within the context of the current frame.
Prompts for the instance variable name.
With a numeric argument, displays all instance variables of SELF."
  (let ((self-value (symeval-in-stack-group 'self sg current-frame))
	ivars var val)
    (unless (instancep self-value)
      (format t "~&~S is not an instance" 'self)
      (return-from com-print-instance-variable nil))
    (setq ivars (si::flavor-all-instance-variables-slow (si::instance-flavor self-value)))
    (with-input-editing (*standard-input*
			  '((:prompt "Value in this frame of instance variable: ")
			    (:activation char= #/end)
			    ;(:full-rubout :full-rubout)
			    ))
			  
      (do-forever
	(setq val (si:read-for-top-level nil nil nil))
	(if (setq var (or (memq val ivars) (mem #'string-equal val ivars)))
	    (let* ((pos (find-position-in-list (setq var (car var)) ivars))
		   (loc (locf (%instance-ref self-value (1+ pos)))))
	      (return-from com-print-instance-variable
		(if (location-boundp loc)
		    (got-values (contents loc) loc nil)
		  (princ "void")
		  (got-values nil loc nil))))
	  (parse-ferror "~S is not an instance variable of ~S" val self-value))))))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:25:22
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-print-frame-handlers (sg ignore &optional arg)
  "Lists the condition handlers set up in this frame. (includes resume and default handlers)
With an argument, shows all condition handlers active in this frame."
  (let ((handlers-outside (symeval-in-stack-group 'condition-handlers sg
						  (sg-next-active sg current-frame)))
	(default-handlers-outside (symeval-in-stack-group 'condition-default-handlers sg
							  (sg-next-active sg current-frame)))
	(resume-handlers-outside (symeval-in-stack-group 'condition-resume-handlers sg
							 (sg-next-active sg current-frame)))
	(handlers (symeval-in-stack-group 'condition-handlers sg current-frame))
	(default-handlers (symeval-in-stack-group 'condition-default-handlers sg
						  current-frame))
	(resume-handlers (symeval-in-stack-group 'condition-resume-handlers sg
						 current-frame)))
    (unless (and (null arg) (eq handlers handlers-outside))
      (if (and (null arg) (null handlers))
	  (format t "~&Condition handlers bound off, hidden from lower levels.")
	(format t "~&~:[No ~]Condition handlers:~%" handlers)
	(dolist (h (if arg handlers (ldiff handlers handlers-outside)))
	  (format t "~& Handler for ~S" (car h)))))
    (unless (and (null arg) (eq default-handlers default-handlers-outside))
      (if (and (null arg) (null default-handlers))
	  (format t "~&Default handlers bound off, hidden from lower levels.")
	(format t "~&~:[No ~]Default handlers:~%" default-handlers)
	(dolist (h (if arg default-handlers (ldiff default-handlers default-handlers-outside)))
	  (format t "~& Default handler for ~S" (car h)))))
    (unless (and (null arg) (eq resume-handlers resume-handlers-outside))
      (format t "~&Resume handlers:~%")
      (dolist (h (if arg resume-handlers (ldiff resume-handlers resume-handlers-outside)))
	(cond ((eq h t)
	       (format t "~& Barrier hiding all resume handlers farther out.")
	       (return nil))
	      (t
	       (format t "~& Resume handler for ~S,~%  " (car h))
	       (unless (consp (cadr h))
		 (format t "proceed type ~S,~%  " (cadr h)))
	       (apply #'format t (cadddr h))))))
    (if (and (eq resume-handlers resume-handlers-outside)
	     (eq default-handlers default-handlers-outside)
	     (eq handlers handlers-outside)
	     (null arg))
	(format t "~&No handlers set up in this frame."))))

(defun com-describe-* (ignore ignore &optional ignore)
  "Describes the value of *"
  (got-values (describe *) + nil))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:25:56
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defconst *com-help-alist* '(((com-help-general "General"
			      "General information about using the debugger") #/G #/g)
			    ((com-help-information "Information"
			      "Various ways of obtaining information about the current stack frame")
			     #/I #/i #/E #/e)
			    ((com-help-frames "Stack Frames"
			      "Selecting Stack Frames to examine") #/F #/f)
			    ((com-help-stepping "Stepping"
			      "Stepping though through the program") #/S #/s)
			    ((com-help-proceeding "Proceeding"
			      "Proceeding from this error and resuming execution")
			      #/P #/p #/X #/x)
			    ((com-help-transferring "Transferring"
			      "Transferring to other systems: Edit, Bug report, Window-based Debugger")
			     #/T #/t)
			    ((com-help-describe-command "Describe"
			      "Give the documentation of the function associated with a command key")
			     #/D #/d #/C #/c)
			    ((ignore "Abort" t) #/abort #/c-Z #/c-G)
			    ((#/help "Help" t) #/help #/?))
  "FQUERY options for used in giving help for debugger commands.")

;; ?, <help>
(defun com-help (sg error-object &optional ignore)
  "Help for using the debugger"
  (prog (command)
   loop
      (with-stack-list (options :choices *com-help-alist* :help-function nil)
	(case (setq command (fquery options "Help for debugger commands. Choose a topic: "))
	  (#/help
	   (terpri)
	   (princ "For additional help in using the debugger, type one of the following:")
	   (terpri)
	   (dolist (x *com-help-alist*)
	     (unless (eq (third (car x)) t)
	       (format t "~&  ~:C~6T~A" (cadr x) (or (third (car x)) (second (car x))))))
	   (go loop))
	  (t (funcall command sg error-object))))))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:26:28
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-help-general (ignore ignore)	       
  (format t "~2%You are in the debugger.  If you don't want to debug this error, type ~C.
Otherwise you can evaluate expressions in the context of the error, examine
the stack, and proceed//throw//return to recover.
  If you type in a Lisp form, it will be evaluated, and the results printed,
using ~a and base ~d in package ~a. This
evaluation uses the variable environment of the stack frame you are examining.
  Type ~c or ~c to get back to top level, or the previous debugger level.
While in the debugger, ~c quits back to the debugger top level.
  If you think this error indicates a bug in the Lisp machine system, use the
~:c command."
		     #/Abort *readtable* *read-base* *package*
		     #/Abort #/c-Z #/c-G #/c-M))

(defun com-help-information (ignore ignore)
  (format t "~2%~c or ~\lozenged-string\ clears screen and retypes error message.
~c clears screen and types args, locals and compiled code.
~c gives a backtrace of function names.
~c gives a backtrace of function names and argument names and values.
~c is like ~c but shows ~Ss, ~Ss, ~Ss, etc.
~c prints an argument to the current function, and sets * to be that
   argument to let you do more complicated things with it.
   + is set to a locative to that argument, should you want to modify it.
   To specify which argument, type the argument number with Control
   or Meta held down before the ~c.
~c is like ~c but works on the function's locals rather than the args.
~c is like ~c but works on the values this frame is returning.
   (This is useful when you get a trap on exit from function).
~c does likewise for the function itself.
~c prints the arglist of the function in the current frame.
~c prints the value in this frame of a special variable you specify.
~c lists all special variable bindings in this frame.

Use the functions (~S n), (~S n), (~S n) and (~S) to get the
value of an arg, local, value or function-object respectively from an
expression being evaluated. For args and locals, n can be a name or a number.
~S allows numbers only. ~S and ~S on those expressions are also allowed.
" #/c-L "Clear Screen" #/m-L #/c-B #/c-m-B #/m-B #/c-B 'eval 'prog 'cond
  #/c-m-A #/c-m-A #/c-m-L #/c-m-A #/c-m-V #/c-m-A #/c-m-F #/c-A #/m-S #/c-m-S
  'eh:arg 'eh:loc 'eh:val 'eh:fun 'eh:val 'locf 'setf))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:26:28
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-help-frames (ignore ignore)
  (format t "~%
~c or ~\lozenged-character\ goes down a frame, ~c or ~\lozenged-character\ goes up.
~c and ~c are similar but show args, locals and compiled code.
~c and ~c are similar to ~c and ~c, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
~c and ~c go to the top and bottom of the stack, respectively.
~c reads a string and searches down the stack for a frame
   calling a function whose name contains that substring.
" #/c-N #/line #/c-P #/return #/m-N #/m-P #/c-m-N #/c-m-P #/c-N #/c-P
  #/m-< #/m-> #/c-S))

(defun com-help-stepping (ignore ignore)
  (if error-handler-running
      (format t "~2%~c toggles the trap-on-exit flag for the current frame.
~c sets the trap-on-exit flag for the current frame and all outer frames.
~c clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
~c proceeds like ~C, but first sets the trap-on-next-function-call flag.
~c toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with ~c.
" #/c-X #/m-X #/c-m-X #/c-D #/resume #/m-D #/c-X)
    (format t "~2%You cannot use the stepping commands, since you are in examine-only mode.
~C exits the debugger." #/resume)))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:26:28
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-help-proceeding (sg error-object)
  (cond (error-handler-running
	 (format t "~2%~c aborts to previous debugger or other command loop, or to top level.
~c returns a value or values from the current frame.
~c offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
~c offers to reinvoke the current frame, letting you alter
   some of the arguments, or use more or fewer arguments.
~c throws to a specific tag." #/c-Z #/c-R #/c-m-R #/m-R #/c-T)
	 (describe-proceed-types sg error-object))
	(t
	 (format t "~2%You cannot continue execution, since you are in examine-only mode.
~C exits the debugger." #/resume))))

(defun com-help-transferring (ignore ignore)
  (format t "~2%~c calls the editor to edit the current function.
~c enters the editor to send a bug message, and puts the error
  message and a backtrace into the message automatically.
  A numeric argument says how many stack frames to put in the backtrace.
~c switches to the window-based debugger.
" #/c-E #/c-M #/c-m-W))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:26:28
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-help-describe-command (sg error-object &aux char)
  "Prompts for a character and describes what the current meaning of that keystoke
is as a command to the error-handler."
  (format t "~&Describe Command. Type command character: ")
  (let ((reading-command t))			;let abort, etc through
    (setq char (send *standard-input* :tyi)))
  (let* ((command (command-lookup char))
	 (resume-handlers (symeval-in-stack-group 'condition-resume-handlers sg))
	 (proceed-types (send error-object :user-proceed-types
			      (sg-condition-proceed-types sg error-object)))
	 (keywords (append proceed-types *special-commands*))
	 tem)
    (format t "~:C~&~C: " char char)
    (cond ((setq tem (rassq char *proceed-type-special-keys*))
	   (send error-object :document-proceed-type (car tem)
		 *standard-output* resume-handlers))
	  ((setq tem (rassq char *special-command-special-keys*))
	   (send error-object :document-special-command (car tem)
		 *standard-output* resume-handlers))
	  ((null command)
	   (if (zerop (char-bits char))
	       (format t "is not a special debugger command.
  Typing this will enter a read-eval-print loop,
using ~A in base ~D, package ~A, with which
you can examine information in the environment of the stack frame you are examining."
		       *readtable* *read-base* *package*)
	     (format t "is not currently a defined debugger command.")))
	  ((eq command 'com-proceed-specified-type)
	   (if (not (< -1 (- char (char-int #/s-A)) (length keywords)))
	       (format t "is not currently a defined debugger command.")
	     (send error-object (if (< (- char (char-int #/s-A)) (length proceed-types))
				    :document-proceed-type
				    :document-special-command)
		   (nth (- char (char-int #/s-A)) keywords)
		   *standard-output* resume-handlers)))
	  (t (if (documentation command)
		 (format t "~A" (documentation command))
	         (format t "~S is not documented" command))
	     (send *standard-output* :fresh-line)
	     (case command
	       (com-abort
		(let ((abort-handler (find-resume-handler abort-object nil resume-handlers)))
		  (cond ((dolist (x proceed-types)
			   (when (eq (find-resume-handler error-object x resume-handlers)
				     abort-handler)
			     (format t "~&This command is currently synonymous with ~C: "
				     (+ (char-int #/s-A) (position x proceed-types)))
			     (send error-object :document-proceed-type x
				   *standard-output* resume-handlers)
			     (return t))))
			(abort-handler
			 (send abort-object :document-proceed-type (second abort-handler)
			       *standard-output* resume-handlers))
			(t (format t "There is no way to abort from this error.")))))
	       (com-proceed
		(if proceed-types
		    (progn (format t "~&This command is currently synonymous with ~C: "
				   #/s-A)
			   (send error-object :document-proceed-type (car proceed-types)
				 *standard-output* resume-handlers))
		  (format t "There is no way to proceed from this error."))))))))

;; Rubout.  Flush numeric arg.
(defun com-rubout (ignore ignore &optional ignore)
  "Flushes the numeric arg, if any."
  (throw 'quit nil))

))

; From file OBI:USRD$:[MLY.SAVE]EHC.LSP;2 at 4-Feb-85 01:41:08
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-proceed-specified-type (error-sg error-object &rest ignore
				   &aux proceed-types proceed-type)
  "Use a user-specified proceed option for this error."
  (declare (special error-object))
  (error-handler-must-be-running)
  (setq proceed-types (append (send error-object :user-proceed-types
				    (sg-condition-proceed-types error-sg error-object))
			      *special-commands*))
;character lossage
  (cond ((rassq (char-int *command-char*) *proceed-type-special-keys*)
	 (setq proceed-type (car (rassq (char-int *command-char*)
					*proceed-type-special-keys*)))
	 (unless (memq proceed-type proceed-types)
	   (format t "Proceed type ~S is not available." proceed-type)
	   (setq proceed-type nil)))
	((> (length proceed-types) (- (char-int *command-char*) (char-int #/s-A)) -1)
	 (setq proceed-type (nth (- (char-int *command-char*) (char-int #/s-A))
				 proceed-types)))
	(t
	 (format t "There are not ~D different ways to proceed."
		 (- (char-int *command-char*) (char-int #/s-A)))))
  (if proceed-type
      (send error-object :proceed-asking-user proceed-type
	    'proceed-error-sg
	    'read-object))
  nil)

(defun com-special-command (error-sg error-object &rest ignore)
;character lossage
  (let ((special-command-type (car (rassq (char-int *command-char*)
					  *special-command-special-keys*))))
    (send error-object :special-command special-command-type)))


(defun describe-proceed-types (sg error-object)
  "Print documentation of the available proceed-types and characters to get them.
ERROR-OBJECT is the object to document.  Output goes to *STANDARD-OUTPUT*."
  (when error-handler-running
    (let* ((proceed-types  (send error-object :user-proceed-types
				 (sg-condition-proceed-types sg error-object)))
	   (resume-handlers (symeval-in-stack-group 'condition-resume-handlers sg))
	   (abort-handler (find-resume-handler abort-object nil resume-handlers)))
      (do ((keywords (append proceed-types *special-commands*)
		     (cdr keywords))
	   (proceed-types proceed-types
			  (cdr proceed-types))
	   tem
	   this-one-for-abort
	   (i 0 (1+ i)))
	  ((null keywords))
	(if (zerop i)
	    (format t "~&~%Commands available for this particular error:~2%"))
	(format t "~C" (+ (char-int #/S-A) i))
	(when proceed-types
	  (setq this-one-for-abort
		(eq (find-resume-handler error-object (car keywords) resume-handlers)
		    abort-handler))
	  (if this-one-for-abort (setq abort-handler nil)))
	(cond ((and (zerop i) (atom (car proceed-types)))
	       ;; Resume only works for proceed-types that are atomic.
	       (format t ", ~C" #/Resume))
	      ((setq tem (assq (car keywords)
			       (if proceed-types *proceed-type-special-keys*
				 *special-command-special-keys*)))
	       (format t ", ~C" (cdr tem)))
	      ;; If Abort is synonymous with this one, mention that.
	      (this-one-for-abort
	       (format t ", ~C" #/Abort)))
	(format t ":~13T")
	(send error-object
	      (if proceed-types
		  :document-proceed-type
		  :document-special-command)
	      (car keywords) *standard-output* resume-handlers)
	(send *standard-output* :fresh-line))
      (when abort-handler
	;; Abort is not currently synonymous with any of the proceed types.
	;; So document it specially.
	(format t "~C:~13T" #/Abort)
	(send abort-object :document-proceed-type (second abort-handler)
	      *standard-output* resume-handlers)
	(send *standard-output* :fresh-line)))))

))

; From file OZ:OZ:<MLY.LL>EHC.LISP.3 at 7-Feb-85 04:08:20
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun proceed-sg (sg &optional val)
  (leaving-error-handler)
  (without-interrupts
    (free-second-level-error-handler-sg %current-stack-group)
    (cond ((getf (sg-plist sg) 'single-macro-dispatch)
	   (setf (getf (sg-plist sg) 'single-macro-dispatch) nil)
	   (setf (sg-inst-disp sg) 2)))
    (stack-group-resume sg val)))

))

; From file OZ:KANSAS:<L.SYS2>EH.LISP.339 at 7-Feb-85 04:08:49
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EH  "

(DEFUN RUN-SG (SG &AUX RESULT)
  "Activate a call block in stack group SG, made with SG-OPEN-CALL-BLOCK.
The FUNCTION with which the call block was made should call the error
handler stack group with one argument.  That argument is returned from RUN-SG.
If the value is EH:LOSE, we throw to EH:QUIT.
Provide this stack group as an argument to the function to be run
so it knows who to call back."
  (%P-STORE-CDR-CODE (LOCF (AREF (SG-REGULAR-PDL SG)	;Terminate arg list assumed there
				 (SG-REGULAR-PDL-POINTER SG)))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)
  (SETF (SG-FLAGS-MAR-MODE SG) 0)			;Turn off the MAR (why??)
  (ASSURE-FREE-SPACE)
  (SETQ RESULT (STACK-GROUP-RESUME SG NIL))
  (LET ((INNER-TRAP-ON-CALL (SG-FLAGS-TRAP-ON-CALL SG))
	(INNER-PLIST (SG-PLIST SG)))
    (SG-RESTORE-STATE SG)
    ;; If the guy set trap-on-call before returning, leave it on.
    (IF (NOT (ZEROP INNER-TRAP-ON-CALL))
	(SETF (SG-FLAGS-TRAP-ON-CALL SG) 1))
    (SETF (SG-PLIST SG) INNER-PLIST))
  (WHEN (AND ERROR-HANDLER-RUNNING ERROR-HANDLER-REPRINT-ERROR)
    (UNLESS (EQ CURRENT-STACK-GROUP LAST-SECOND-LEVEL-ERROR-HANDLER-SG)
      (FORMAT T "~%Back to ")
      (PRINT-CAREFULLY "error message"
	(SEND EH-ERROR :PRINT-ERROR-MESSAGE SG T *STANDARD-OUTPUT*))
      (WARN-ABOUT-SPECIAL-VARIABLES SG))
    (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP))
  (IF (EQ RESULT 'LOSE)
      (THROW 'QUIT NIL))
  RESULT)

))

; From file OZ:KANSAS:<L.SYS2>EH.LISP.339 at 7-Feb-85 04:12:48
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EH  "

(DEFVAR EXTRA-ERROR-TABLE)		;Used in  -- defvar is to prevent cadr compiler barf

(DEFUN LISP-ERROR-HANDLER (&AUX M (INHIBIT-SCHEDULING-FLAG T)
			   (DEFAULT-CONS-AREA ERROR-HANDLER-AREA))
  ;; Return to boot code.  We are called back by the first error.
  (SETQ M (STACK-GROUP-RESUME CURRENT-STACK-GROUP-RESUMER NIL))
  (DO-FOREVER
    ;; M can be:
    ;;  NIL for a microcode error.
    ;;    We allocate a second-level stack group to create a condition object
    ;;    and cause it to be signaled.
    ;;  A condition instance, for an error being signaled.
    ;;    We allocate a second-level stack group to handle the error.
    ;;  A list, for certain weird things, such as
    ;;   (BREAK) to enter an error break, or
    ;;   (RESUME-FOOTHOLD), to resume from C-Break or C-M-Break.
    (LET (SG ETE SG2)
      (SETQ SG CURRENT-STACK-GROUP-RESUMER)
      (SETF (SG-PROCESSING-ERROR-FLAG SG) 0)	;Re-enable error trapping in that SG
      (SETF (SG-INST-DISP SG) 0)		;Turn off single-step mode (for foothold)
      (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
      (UNLESS M
	;; If microcode error, compute the ETE.
	(SETQ ETE (OR (AND (BOUNDP 'ERROR-TABLE)
			   (CDR (ASSQ (SG-TRAP-MICRO-PC SG) ERROR-TABLE)))
		      (AND (BOUNDP 'EXTRA-ERROR-TABLE)
			   (CDR (ASSQ (SG-TRAP-MICRO-PC SG) EXTRA-ERROR-TABLE)))))
	;; Clean things up after specific kinds of ucode errors.
	(LET ((TEM (GET (CAR ETE) 'ENTER-ERROR-HANDLER)))
	  (IF TEM (FUNCALL TEM SG ETE))))
      ;; All branches of this COND must end in resuming some other SG.
      (SETQ M
	    (COND ((AND (EQ (CAR ETE) 'STEP-BREAK)
			(SETQ SG2 (CDR (ASSQ SG SG-STEPPING-TABLE))))
		   (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
		   (FUNCALL SG2 SG))
		  ((AND (CONSP M) (EQ (CAR M) 'RESUME-FOOTHOLD))
		   (SG-RESTORE-STATE SG 1)
		   (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
		   (COND ((GETF (SG-PLIST SG) 'SINGLE-MACRO-DISPATCH)
			  (SETF (GETF (SG-PLIST SG) 'SINGLE-MACRO-DISPATCH) NIL)
			  (SETF (SG-INST-DISP SG) 2)))
		   (STACK-GROUP-RESUME SG NIL))
		  ((NULL M)
		   ;; Microcode error.
		   (SETQ SG2 (OR (POP FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
				 (MAKE-STACK-GROUP
				   (FORMAT NIL "SECOND-LEVEL-ERROR-HANDLER-~D"
					   (INCF SECOND-LEVEL-ERROR-HANDLER-COUNT))
				   :REGULAR-PDL-SIZE #o6000
				   :SAFE 0)))
		   (STACK-GROUP-PRESET SG2 'SIGNAL-MICROCODE-ERROR
				       SG ETE)
		   (FUNCALL SG2))
		  (T
		   ;; Condition object being signaled.
		   ;; Obtain a second level error handler sg
		   ;; and tell it what to work on.
		   (SETQ SG2 (OR (POP FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
				 (MAKE-STACK-GROUP
				   (FORMAT NIL "SECOND-LEVEL-ERROR-HANDLER-~D"
					   (INCF SECOND-LEVEL-ERROR-HANDLER-COUNT))
				   :REGULAR-PDL-SIZE #o6000
				   :SAFE 0)))
		   (STACK-GROUP-PRESET SG2 'SECOND-LEVEL-ERROR-HANDLER
				       SG M)
		   (FUNCALL SG2)))))))

))

; From file OZ:KANSAS:<L.EH>EHF.LISP.227 at 7-Feb-85 04:19:52
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHF  "

(defmethod (step-break-error :ucode-proceed-types) ()
  '(:no-action))

))

; From file OZ:KANSAS:<L.EH>EHF.LISP.227 at 7-Feb-85 06:58:16
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHF  "

(def-ucode-error number-called-as-function (invalid-function)
  :property-list `(:function ,(sg-contents sg (second ete)))
;character lossage
  :format-string "The ~:[number~;character~], ~S, was called as a function"
  :format-args (list (characterp (sg-contents sg (second ete)))
		     (sg-contents sg (second ete))))

))

; From file OZ:OZ:<MLY.LL>EHC.LISP.6 at 7-Feb-85 07:46:33
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHC  "

(defun com-proceed (error-sg error-object &rest ignore &aux proceed)
  "Proceeds from this error if possible."
  (declare (special error-object))
  (if (not error-handler-running)
      (throw 'exit t))
  (setq proceed (car (or (send error-object :user-proceed-types
			       (sg-condition-proceed-types error-sg error-object))
			 *special-commands*)))
  (if (null proceed)
      (format t "There is no way to proceed from this error.~%")
    (if (consp proceed)
	(format t "You cannot proceed; you can only restart various command loops.~%")
      (send error-object :proceed-asking-user proceed 'proceed-error-sg 'read-object)))
  nil)

(makunbound 'eh-sg)
(makunbound 'eh-frame)


))

; From file OZ:KANSAS:<L.SYS2>EH.LISP.339 at 7-Feb-85 08:49:13
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EH  "

(DEFUN FH-APPLIER (FN ARGS X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
		   X-SG X-FRAME X-ERROR SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH)))
      (CATCH TAG
	;; Note: no special variables should be bound
	;; outside this point (when we return to the error handler sg).
	(STACK-GROUP-RESUME SG
	  (LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
		(// X//) (//// X////) (////// X//////)
		(*VALUES* XVALUES)
		(EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
		(CONDITION-HANDLERS NIL)
		(CONDITION-DEFAULT-HANDLERS NIL)
		;; It's best to heap-cons this to avoid frightening the user.
		(CONDITION-RESUME-HANDLERS
		  (LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			   SI:CATCH-ERROR-RESTART-THROW ,TAG)
			 T CONDITION-RESUME-HANDLERS))
		(*EVALHOOK* NIL)
		(*APPLYHOOK* NIL)
		(ERRSET-STATUS NIL))
	    (MULTIPLE-VALUE-LIST (APPLY FN ARGS)))))
      ;; This is in case the catch catches.
      (STACK-GROUP-RESUME SG 'LOSE))
    ;; This is reached only if we throw through this frame.
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))

; From file OZ:KANSAS:<L.SYS2>EH.LISP.339 at 7-Feb-85 08:49:28
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EH  "

(DEFUN FH-EVALER (FORM X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
		  X-SG X-FRAME X-ERROR SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH)))
      (CATCH TAG
	(STACK-GROUP-RESUME
	  SG
	  ;; Note: no special variables should be bound
	  ;; outside this point (when we return to the error handler sg).
	  (LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
		(// X//) (//// X////) (////// X//////) (- FORM)
		(*VALUES* XVALUES)
		(EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
		(CONDITION-HANDLERS NIL)
		(CONDITION-DEFAULT-HANDLERS NIL)
		;; It's best to heap-cons this to avoid frightening the user.
		(CONDITION-RESUME-HANDLERS
		  (LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			   SI:CATCH-ERROR-RESTART-THROW ,TAG)
			 T CONDITION-RESUME-HANDLERS))
		(*EVALHOOK* NIL)
		(*APPLYHOOK* NIL)
		(ERRSET-STATUS NIL))
	    (MULTIPLE-VALUE-LIST (EVAL FORM)))))
      ;; This is in case the catch catches.
      (STACK-GROUP-RESUME SG 'LOSE))
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))

; From file OZ:KANSAS:<L.SYS2>EH.LISP.339 at 7-Feb-85 08:50:03
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EH  "

(DEFUN FH-STREAM-BINDING-EVALER (FORM X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
				 X-SG X-FRAME X-ERROR
				 SG EH-P PREV-FH
				 REGPDL-SEGMENT SPECPDL-SEGMENT ERROR-DEPTH
				 EH-TERMINAL-IO)
  (DECLARE (SPECIAL OLD-TERMINAL-IO OLD-STANDARD-OUTPUT OLD-STANDARD-INPUT))
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH))
;	   (OLD-TERMINAL-IO *TERMINAL-IO*) 
;	   (OLD-STANDARD-OUTPUT *STANDARD-OUTPUT*) (OLD-STANDARD-INPUT *STANDARD-INPUT*)
	   WIN-P RESULT)
      (CATCH TAG
	;; Note: no special variables should be bound
	;; outside this point (when we return to the error handler sg).
	(LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
	      (// X//) (//// X////) (////// X//////) (- FORM)
	      (*VALUES* XVALUES)
	      (EH-SG X-SG)
	      (EH-FRAME X-FRAME)
	      (EH-ERROR X-ERROR)
	      (*TERMINAL-IO* EH-TERMINAL-IO)
	      (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
	      (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
	      (*QUERY-IO* SI:SYN-TERMINAL-IO)
	      (*ERROR-OUTPUT* SI:SYN-TERMINAL-IO)
	      (*EVALHOOK* NIL)
	      (*APPLYHOOK* NIL)
	      (ERRSET-STATUS NIL)
	      (RUBOUT-HANDLER NIL)
	      (CONDITION-HANDLERS NIL)
	      (CONDITION-DEFAULT-HANDLERS NIL)
	      ;; It's best to heap-cons this to avoid frightening the user.
	      (CONDITION-RESUME-HANDLERS
		(LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			 SI:CATCH-ERROR-RESTART-THROW ,TAG)
		       T CONDITION-RESUME-HANDLERS)))
	  (SETQ RESULT (MULTIPLE-VALUE-LIST (SI:EVAL-ABORT-TRIVIAL-ERRORS FORM))
		WIN-P T)))
      (COND (WIN-P
;	     (SETQ *TERMINAL-IO* OLD-TERMINAL-IO
;		   *STANDARD-OUTPUT* OLD-STANDARD-OUTPUT
;		   *STANDARD-INPUT* OLD-STANDARD-INPUT)
	     (STACK-GROUP-RESUME SG RESULT))
	    (T (STACK-GROUP-RESUME SG 'LOSE))))
    (IF REGPDL-SEGMENT (FREE-SAVED-PDL-SEGMENT REGPDL-SEGMENT))
    (IF SPECPDL-SEGMENT (FREE-SAVED-PDL-SEGMENT SPECPDL-SEGMENT))
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))


; From file OZ:KANSAS:<L.IO>FORMAT.LISP.240 at 7-Feb-85 21:26:18
#10R FORMAT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS LOWER-CASE)
  (WHEN (EQ (CAR-SAFE ARG) :MOUSE-BUTTON) (SETQ ARG (CADR ARG)))
  (SETQ ARG (CLI:CHARACTER ARG)
	BITS (CHAR-BITS ARG))
  (FLET ((PRINT-BITS (BITS)
	   (AND (BIT-TEST CHAR-HYPER-BIT BITS)
		(SEND *STANDARD-OUTPUT* :STRING-OUT "Hyper-"))
	   (AND (BIT-TEST CHAR-SUPER-BIT BITS)
		(SEND *STANDARD-OUTPUT* :STRING-OUT "Super-"))
	   (AND (BIT-TEST CHAR-CONTROL-BIT BITS)
		(SEND *STANDARD-OUTPUT* :STRING-OUT "Control-"))
	   (AND (BIT-TEST CHAR-META-BIT BITS)
		(SEND *STANDARD-OUTPUT* :STRING-OUT "Meta-"))))
    (COND ((TV:CHAR-MOUSE-P ARG)
	   (IF (AND (NOT COLON-FLAG) ATSIGN-FLAG)
	       (PRINC "#\"))
	   (PRINT-BITS BITS)
	   (SETF (CHAR-BITS ARG) 0)
	   (IF (AND (NOT COLON-FLAG) ATSIGN-FLAG)
	       (IF (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		   (PRINC CHNAME)
		 (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
	     (SEND *STANDARD-OUTPUT* :STRING-OUT "Mouse-")
	     (SEND *STANDARD-OUTPUT* :STRING-OUT (NTH (LDB %%KBD-MOUSE-BUTTON ARG)
						      '("Left" "Middle" "Right")))
	     (IF (SETQ CHNAME (NTH (SETQ BITS (LDB %%KBD-MOUSE-N-CLICKS ARG))
				   '("" "-Twice" "-Thrice")))
		 (SEND *STANDARD-OUTPUT* :STRING-OUT CHNAME)
	       (SEND *STANDARD-OUTPUT* :TYO #/-)
	       (ENGLISH-PRINT (1+ BITS))
	       (SEND *STANDARD-OUTPUT* :STRING-OUT "-times"))))
	  ((NOT COLON-FLAG)
	   ;; If @ flag or if control bits, we want to use characters' names.
	   (IF (OR ATSIGN-FLAG (NOT (ZEROP BITS)))
	       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (CHAR-CODE ARG))))
	   ;; Print an appropriate reader macro if @C.
	   (IF ATSIGN-FLAG (PRINC "#\"))
	   (UNLESS (ZEROP BITS)
	     (SEND *STANDARD-OUTPUT*
		   :STRING-OUT (NTH BITS '("" "c-" "m-" "c-m-"
					   "s-" "c-s-" "m-s-" "c-m-s-"
					   "h-" "c-h-" "m-h-" "c-m-h-"
					   "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")))
	     (IF ( (CHAR-CODE #/a) (SETQ LOWER-CASE (CHAR-CODE ARG)) (CHAR-CODE #/z))
		 (SEND *STANDARD-OUTPUT* :STRING-OUT "sh-")
	       (SETQ LOWER-CASE NIL)))
	   (COND (CHNAME
		  (SETQ CHNAME (SYMBOL-NAME CHNAME))
		  ;; are we CONSING yet?
		  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE (CHAR CHNAME 0)))
		  (DO ((LEN (LENGTH CHNAME))
		       (I 1 (1+ I)))
		      ((= I LEN))
		    (SEND *STANDARD-OUTPUT* :TYO (CHAR-DOWNCASE (CHAR CHNAME I)))))
		 (T (IF ATSIGN-FLAG
			(IF (SI::CHARACTER-NEEDS-QUOTING-P (CHAR-CODE ARG))
			    (SEND *STANDARD-OUTPUT* :TYO (SI::PTTBL-SLASH *READTABLE*)))
		        (IF LOWER-CASE (SETQ ARG (CHAR-UPCASE (INT-CHAR LOWER-CASE)))))
		    (SEND *STANDARD-OUTPUT* :TYO (CHAR-CODE ARG)))))
	  (T
	   (PRINT-BITS BITS)
	   (SETQ ARG (INT-CHAR (CHAR-CODE ARG)))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (SETQ CHNAME (SYMBOL-NAME CHNAME))
		  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE (CHAR CHNAME 0)))
		  (DO ((LEN (LENGTH CHNAME))
		       (I 1 (1+ I)))
		      ((= I LEN))
		    (SEND *STANDARD-OUTPUT* :TYO (CHAR-DOWNCASE (CHAR CHNAME I))))
		  (AND ATSIGN-FLAG (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND ATSIGN-FLAG (< ARG #o40) ( ARG #/))
		  (SEND *STANDARD-OUTPUT* :TYO ARG)
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
		 ((AND (LOWER-CASE-P ARG)
		       (NOT (ZEROP BITS)))
		  (SEND *STANDARD-OUTPUT* :STRING-OUT "Shift-")
		  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE ARG)))
                 (T (SEND *STANDARD-OUTPUT* :TYO ARG)))))))

))

; From file OZ:KANSAS:<L.SYS2>LMMAC.LISP.382 at 8-Feb-85 21:26:16
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFF si:LOCATION-CONTENTS 'CONTENTS)

(DEFMACRO si:WITHOUT-FLOATING-UNDERFLOW-TRAPS (&BODY BODY)
  "Executes BODY with floating-point underflow traps disabled.
If a floating-point operation within body would normally underflow, zero is used instead."
  `(LET ((ZUNDERFLOW T))
     . ,BODY))

(DEFMACRO si:UNWIND-PROTECT-CASE ((&OPTIONAL ABORTED-P-VAR) BODY &REST CLAUSES)
  "Execute BODY inside an UNWIND-PROTECT form.
Each element of CLAUSES is a list of the form (<keyword> . <cruft>), where
<keyword> specifies under which condition to execute the associated <cruft>
Each keyword must be one of :ALWAYS, meaning to execute the cruft regardless
of whether BODY is exited non-locally or terminates normally,
:ABORT meaning to execute it only if BODY exits non-locally, or
:NORMAL meaning do it only if BODY returns /"normally./"
ABORTED-P-VAR, if supplied, is used to flag whether BODY exited abnormally:
 it is normally set to NIL by successful execution of BODY, but may be set to
 NIL within BODY, meaning not to execute :ABORT clauses even if BODY later exits abnormally.
The values returned are those of BODY.

This macro is for wimps who can't handle raw, manly UNWIND-PROTECT"
  (OR ABORTED-P-VAR (SETQ ABORTED-P-VAR '.ABORTED-P.))
  `(LET ((,ABORTED-P-VAR T))
     (UNWIND-PROTECT
	 (MULTIPLE-VALUE-PROG1 ,BODY (SETQ ,ABORTED-P-VAR NIL))
       ,@(LOOP FOR (KEYWORD . FORMS) IN CLAUSES
	       COLLECT (ECASE KEYWORD
			 (:NORMAL `(WHEN (NOT ,ABORTED-P-VAR) ,@FORMS))
			 (:ABORT `(WHEN ,ABORTED-P-VAR ,@FORMS))
			 (:ALWAYS `(PROGN ,@FORMS)))))))

))

; From file OZ:KANSAS:<L.COLD>QCOM.LISP.582 at 12-Feb-85 02:48:46
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: COLD; QCOM  "

(DEFCONST LINEAR-PDL-FIELDS-VALUES '(
  ;LPCLS (%LP-CALL-STATE).  Stored when this call frame is created.
  ;; Set if any of the following bits are set (used for fast check when returning from call):
  ;;   TRAP-ON-EXIT, ADI-PRESENT, MICRO-STACK-SAVED, BINDING-BLOCK-PUSHED,
  ;;   ENVIRONMENT-POINTER-POINTS-HERE, or function exit/entry metering is enabled,
  ;;   or this frame just needs to be unwound.
  %%LP-CLS-ATTENTION 3001
  ;; If set, need not compute SELF-MAPPING-TABLE
  ;;  because our caller has done so.
  %%LP-CLS-SELF-MAP-PROVIDED 2701
  ;; If set, get error before popping this frame.
  %%LP-CLS-TRAP-ON-EXIT 2601
  ;; ADI words precede this call-block
  %%LP-CLS-ADI-PRESENT 2401
  ;; Where in the caller to put this frame's value
  %%LP-CLS-DESTINATION 2004
  ;; This includes the destination field and ADI bit.
  %%LP-CLS-DESTINATION-AND-ADI 2005
  ;; Offset back to previous open or active block
  ;; An open block is one whose args are being made
  %%LP-CLS-DELTA-TO-OPEN-BLOCK 1010
  ;; Offset back to previous active block
  ;;  An active block is one that is executing
  %%LP-CLS-DELTA-TO-ACTIVE-BLOCK 0010
  ;LPEXS (%LP-EXIT-STATE).  Stored when this frame calls out.
  ; bits 22'-27' not used in LPEXS
  ;; A microstack frame exists on special pdl
  %%LP-EXS-MICRO-STACK-SAVED 2101
  ;; Same as below
  %%LP-EXS-PC-STATUS 2001
  ;; M-QBBFL STORED HERE IN MACRO EXIT OPERATION 
  %%LP-EXS-BINDING-BLOCK-PUSHED 2001
  ;; LC as offset in halfwords from FEF
  ;;  Meaningless if %LP-FEF not a fef.
  ;;  Don't change %%LP-EXS-EXIT-PC ---  the numerical value is known by UCADR
  %%LP-EXS-EXIT-PC 0017
  ;LPENS (%LP-ENTRY-STATE).  Stored when this frame entered.
  ; bits 21'-27' not used in LPENS
  ;; This is nonzero if an explicit rest arg is passed.
  %%LP-ENS-LCTYP 2001
  ;; Here are the fields that the entry state normally contains.
  ;; This is 1 if this frame has a rest arg living on the stack.
  ;; Means this frame cannot be flushed for tail recursion.
  %%LP-ENS-UNSAFE-REST-ARG 1701
  ;; This includes the number-of-args field and the unsafe field.
  %%LP-ENS-NUM-ARGS-AND-UNSAFE-FLAG 1010
  ;; This is a pointer to the unsafe flag, within the byte that goes
  ;; into the %%lp-ens-num-args-and-unsafe-flag field.
  %%LP-ENS-UNSAFE-REST-ARG-1 0701
  %%LP-ENS-ENVIRONMENT-POINTER-POINTS-HERE 1601
  %%LP-ENS-NUM-ARGS-SUPPLIED 1006
  %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN 0010
  ))
(DEFCONSTANT %%LP-CLS-ATTENTION 3001)

))

; From file OZ:KANSAS:<L.COLD>QCOM.LISP.582 at 12-Feb-85 02:51:00
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: COLD; QCOM  "

;;; Fast FEF header fields.
(DEFCONST FAST-FEFH-CONSTANT-VALUES '(
; Bits used for info are 3602 (cdr-code), 1704.  3101 is wasted because header-type is
; in the old (24-bit style) position.  The PC fields from the slow case apply here, but
; the GET-SELF-MAPPING-TABLE, SV-BIND, FAST-ARG, NO-ADL, bits do not.
  %%FEFH-ARGS-FOR-FANL 1704		;Number of args for FIXED-ARGS-NO-LOCALS.
  %%FEFH-MIN-ARGS-FOR-VANL 3602		;Minimum number of args for VAR-ARGS-NO-LOCALS.
  %%FEFH-MAX-ARGS-FOR-VANL 1704		;Maximum number of args for VAR-ARGS-NO-LOCALS.
  %%FEFH-ARGS-FOR-FAWL 3602		;Number of args for FIXED-ARGS-WITH-LOCALS.
  %%FEFH-LOCALS-FOR-FAWL 1704		;Local block length for FIXED-ARGS-WITH-LOCALS.
  %%FEFH-MIN-ARGS-FOR-VAWL 3602		;Minimum number of args for VAR-ARGS-WITH-LOCALS.
  %%FEFH-MAX-ARGS-FOR-VAWL 1702		;Maximum number of args for VAR-ARGS-WITH-LOCALS.
  %%FEFH-LOCALS-FOR-VAWL 2102		;Local block length for VAR-ARGS-WITH-LOCALS.
  %%FEFSL-NO-ADL 3701			;New NO-ADL field.
  ))
(ASSIGN-ALTERNATE FAST-FEFH-CONSTANT-VALUES)
(DEFCONST FAST-FEFH-CONSTANTS (SI:GET-ALTERNATE FAST-FEFH-CONSTANT-VALUES))

))

;; From file OZ:KANSAS:<L.COLD>QCOM.LISP.582 at 12-Feb-85 02:53:01
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER::PATCH-SOURCE-FILE "SYS: COLD; QCOM  "

;(DEFCONST SG-STATE-FIELD-VALUES '(
;  %%SG-ST-CURRENT-STATE 0006 
;  %%SG-ST-FOOTHOLD-EXECUTING 0601 
;  %%SG-ST-PROCESSING-ERROR 0701
;  %%SG-ST-PROCESSING-INTERRRUPT-REQUEST 1001 
;  %%SG-ST-SAFE 1101
;  %%SG-ST-INST-DISP 1202
;  %%SG-ST-IN-SWAPPED-STATE 2601 
;  %%SG-ST-SWAP-SV-ON-CALL-OUT 2501 
;  %%SG-ST-SWAP-SV-OF-SG-THAT-CALLS-ME 2401
;; Set if swapped out sg has saved microstack on special-pdl. Can't use %LP-EXS-MICRO-STACK-SAVED
;; because that bit can already be in use by running frame.
;  %%SG-ST-MICRO-STACK-SAVED 2301
;  ))

;))

;; From file OZ:KANSAS:<L.COLD>QCOM.LISP.582 at 12-Feb-85 03:01:57
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER::PATCH-SOURCE-FILE "SYS: COLD; QCOM  "

;(DEFCONST METER-EVENTS '(
;  %METER-PAGE-IN-EVENT
;  %METER-PAGE-OUT-EVENT
;  %METER-CONS-EVENT
;  %METER-FUNCTION-ENTRY-EVENT
;  %METER-FUNCTION-EXIT-EVENT
;  %METER-FUNCTION-UNWIND-EVENT
;  %METER-STACK-GROUP-SWITCH-EVENT
;  %%METER-MACRO-INSTRUCTION-ENABLE 0401		;Macro-instruction metering
;  ))

;))

; From file OZ:KANSAS:<L.COLD>QCOM.LISP.583 at 12-Feb-85 03:54:33
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: COLD; QCOM  "

(DEFCONST Q-HEADER-TYPES '(
  %HEADER-TYPE-ERROR
  %HEADER-TYPE-FEF
  %HEADER-TYPE-ARRAY-LEADER
  %HEADER-TYPE-unused
  %HEADER-TYPE-FLONUM
  %HEADER-TYPE-COMPLEX
  %HEADER-TYPE-BIGNUM
  %HEADER-TYPE-RATIONAL
  %HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
  %HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
  %HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
  %HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
  ))
(defconstant %HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS 8.)
(defconstant %HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS 9.)
(defconstant %HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS 10.)
(defconstant %HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS 11.)

))

; From file OZ:KANSAS:<L.COLD>DOCMIC.LISP.40 at 12-Feb-85 19:46:12
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: COLD; DOCMIC  "

(SETF (DOCUMENTATION 'EQL 'FUNCTION)
  "Like = when both arguments are numbers of the same type; like EQ otherwise.")

(setf (documentation '%pointer-plus 'function)
  "Return a fixnum which represents a pointer DISP words past PTR1.
The argumentts had better be locatives into the same object
for this operation to be meaningful;
otherwise, their relative position will be changed by GC.")

))
