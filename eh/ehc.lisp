;;; The error handler commands -*- Mode:LISP; Package:EH; Readtable:T; Base:8; Lowercase:T -*-

;;; Copyright (C) Hyperbolic Systems Inc., 1982.  All wrongs reversed.

;; Commands in the dispatch table are given the SG and the ERROR-OBJECT,
;; and a third arg which is the numeric argument may or may not be passed.

;; Any command which wants to return out of the error handler should
;; do a throw to FINISHED after restarting the erring stack group.

(defvar window-error-handler-old-window nil
  "If inside window error handler, this is either the old selected window or T if none.")

(defvar eh-command-char :unbound
  "While calling a debugger command, this is the command character.")

(defconst *proceed-type-special-keys*
	  '((:store-new-value . #/m-C))
  "Alist of proceed types vs. standard commands that proceed with those proceed types.")

(defconst *special-command-special-keys* nil
  "Alist of special-command keywords vs. standard characters that run them.")

(defvar special-commands :unbound
  "List of special command keywords provided by this error.")

(defconst *inhibit-debugger-proceed-prompt* nil
  "Non-NIL means do not list available Super-commands on entry to debugger.")

(defun command-loop (error-sg error-object
		     &aux function sexp 
		     (evalhook nil)
		     special-commands
		     (window-error-handler-old-window nil)
		     io-buffer
		     reading-command)
  (when error-object
    (setq special-commands (send error-object ':special-command ':which-operations))
    (send error-object ':initialize-special-commands))
  (when (memq ':io-buffer (send *standard-input* ':which-operations))
    (setq io-buffer (send *standard-input* ':io-buffer))
    (%bind (locf (tv:io-buffer-output-function io-buffer)) 'io-buffer-output-function)
    (%bind (locf (tv:io-buffer-input-function io-buffer)) nil))
  (inheriting-variables-from (error-sg)  ;Do this every time around the loop in case of setq
    (catch-error-restart ((sys:abort error) "Return to debugger command loop.")
      (*catch 'quit
	(show-function-and-args error-sg)
	(warn-about-special-variables error-sg)
	(unless *inhibit-debugger-proceed-prompt*
	  (describe-proceed-types error-sg error-object)))))
  (error-restart (sys:abort "Return to debugger command loop")
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
	  (*catch 'quit
	    (fresh-line *standard-output*)
	    (dotimes (i error-depth)
	      (send *standard-output* ':tyo #/))
	    (do-forever				;This loop processes numeric args
	      ;; Read the next command or sexp, with combined rubout processing.
	      (multiple-value (function sexp)
		(command-loop-read))
	      ;; If it's a character, execute the definition or complain.
	      (cond ((numberp function)
		     (setq numeric-arg
			   (if (null numeric-arg) function (+ function (* 10. numeric-arg))))
		     (tyo #/space))
		    (function
		     (tyo #/space)  ;Print a space after the echo in case it prints something
		     (let ((eh-command-char sexp))
		       (return (if (not numeric-arg)
				   (funcall function error-sg error-object)
				 (funcall function error-sg error-object numeric-arg)))))
		    ;; If there was no command, there was a sexp, so eval it.
		    (t
		     (*catch 'quit
		       (setq +++ ++ ++ + + -)
		       (let (values)
			 (unwind-protect
			     (setq values (sg-eval-in-frame error-sg (setq - sexp) current-frame t))
			   (push (unless (eq values error-flag) values) *values*))
			 (cond ((neq values error-flag)
				(setq ////// //// //// //)
				(setq *** ** ** *)
				(setq // values * (car //))
				(dolist (value //)
				  (terpri)
				  (funcall (or prin1 #'prin1) value))))))
		     (return))))))))))

;Errors in error handler commands can be reported this way.
(defun barf (format-string &rest args)
  (apply 'format t format-string args)
  (*throw 'quit nil))

(defvar reading-command nil
  "Used in the debugger. Bound to T while reading a command char, for the io-buffer function.")

;; Read from *STANDARD-INPUT* either a control-character (or ? or Help)
;; or a s-expression.  Return FUNCTION and CHAR or return NIL and the s-expression.
(defun command-loop-read ()
  (prog (char sexp flag function)
    retry
       ;; Read a character.
       (let ((reading-command t))
	 (setq char (send *standard-input* ':tyi)))
       ;; Now, if the char is special, echo and return it.
       (cond ((rassq char *proceed-type-special-keys*)
	      (format t "~C" char)
	      (return 'com-proceed-specified-type char))
	     ((rassq char *special-command-special-keys*)
	      (format t "~C" char)
	      (return 'com-special-command char))
	     ((or ( 0 (char-bits char))
		  (command-lookup char))
	      (cond ((setq function (command-lookup char))
		     (and (eq function 'com-number)
			  (setq function (digit-char-p (char-code char))))
		     (format t "~C" char)
		     (return function char))))
	     ((= char #/rubout) (go retry)))	;Ignore rubouts
       ;; Otherwise, unread it and read an s-exp instead.
       (send *standard-input* ':untyi char)
       (multiple-value (sexp flag)
	 (with-input-editing (*standard-input*  '((:full-rubout :full-rubout)
						  (:activation = #/end))
						  (:prompt " Eval: "))
	   (si:read-for-top-level)))
       (when (eq flag ':full-rubout)
	 (go retry))
       (return nil sexp)))

(defun io-buffer-output-function (ignore char &aux tem)
  (cond ;; Blips shouldn't get here, but don't die
	((not (numberp char)) char)
	;; Don't intercept commands
	((and reading-command (command-lookup char)) char)
	((setq tem (assq char tv:kbd-intercepted-characters))
	 (multiple-value-prog1 (funcall (cadr tem) char))
	 (format t "~&Back to debugger.~%"))
	;; Compatibility with ancient history
	((eq char #/c-G)			
	 (tv:kbd-intercept-abort #/c-G))
	(t char)))

(defun command-lookup (char)
  "Return the debugger command function for CHAR, or NIL."
  (aref command-dispatch-table (char-bits char)
			       (char-code (char-upcase char))))

;;;; Utility functions used by the top level, and various commands.

(defun print-brief-error-backtrace (sg error-object)
  error-object
  (format t "~&While in the function ")
  (short-backtrace sg nil error-message-backtrace-length error-locus-frame)
  (terpri)
  (let ((hook (symeval-in-stack-group 'error-message-hook sg)))
    (if hook (funcall hook))))

(defun read-object (&rest prompt-and-read-args)
  "Like PROMPT-AND-READ but executes in the erring stack group."
  (declare (arglist option format-string &rest format-args))
  (cond (window-error-handler-old-window
	 (apply 'window-read-object prompt-and-read-args))
	(t
	 (let ((otoc (sg-flags-trap-on-call error-sg)))
	   (setf (sg-flags-trap-on-call error-sg) 0)
	   (unwind-protect
	     (values-list
	       (sg-eval-in-frame error-sg `(apply 'prompt-and-read ',prompt-and-read-args)
				 current-frame t))
	     (setf (sg-flags-trap-on-call error-sg) otoc))))))

(defun leaving-error-handler ()
  "Called when resuming erring program and not coming back to the debugger.
Special hacks cause this function to be called, in the debugger SG,
if the debugger's foothold is thrown through.
Deactivates the debugger window if one was in use."
  (setq tv:cold-load-stream-owns-keyboard
	saved-cold-load-stream-owns-keyboard)
  (and window-error-handler-old-window
       (tv:delaying-screen-management
	 (if (eq window-error-handler-old-window t)
	     (send error-handler-window ':deselect t)
	     (send window-error-handler-old-window ':select))
	 ;;If this doesn't leave the window still on the screen, it is useless, so free it.
	 (or (tv:sheet-exposed-p error-handler-window)
	     (send error-handler-window ':deactivate)))))

(defun proceed-error-sg (&rest args)
  (leaving-error-handler)
  (without-interrupts
    (free-second-level-error-handler-sg %current-stack-group)
    (stack-group-resume error-sg (copylist args))))

;; Continue the stack group, returning VAL if specified
(defun proceed-sg (sg &optional val)
  (leaving-error-handler)
  (without-interrupts
    (free-second-level-error-handler-sg %current-stack-group)
    (stack-group-resume sg val)))

;;;; Backtrace commands.

;; Short backtraces contain only function names.  Full ones contain arg names and values.
;; Both versions take arguments the same way.  The first is the SG, and the
;; second is ignored so that the functions may be used as commands.
;; They will print no more than N frames if N is present.
;; If SKIP is nonzero, the first that many frames are skipped before the
;; N frames are printed.  Used to put them in curly brackets, but that was
;; fairly useless.

;; This prints out like the Maclisp BAKTRACE, and does not TERPRI at beginning
;; nor end.

(defun com-short-backtrace (sg ignore &optional (n most-positive-fixnum))
  "Prints a brief (function names only) backtrace of the stack.
Optional numeric arg determines how far back to go."
  (short-backtrace sg nil n))

(defun short-backtrace (sg ignore &optional (n most-positive-fixnum) start-frame uninteresting-flag)
  (print-backtrace sg n start-frame uninteresting-flag
		   #'(lambda (sg frame count)
		       (let ((width
			       (or (send *standard-output* ':send-if-handles
					 ':size-in-characters)
				   40.)))
			 (format:breakline width nil
			   (or (zerop count) (princ "  "))
			   (prin1 (function-name
				    (rp-function-word (sg-regular-pdl sg) frame))))))))

(defun full-backtrace (sg ignore &optional (n most-positive-fixnum) start-frame uninteresting-flag)
  "Gives a full (includes args) backtrace. Optional numeric arg determines how far back to go."
  (print-backtrace sg n start-frame uninteresting-flag
		   #'(lambda (sg frame ignore)
		       (print-function-and-args sg frame))))

(defun print-backtrace (sg n start-frame uninteresting-flag
			frame-printer-function)
  (do ((frame (or start-frame current-frame)
	      (funcall (if uninteresting-flag
			   #'sg-next-active
			   #'sg-next-interesting-active)
		       sg frame))
       (i 0 (1+ i)))
      ((or ( i n) (null frame)) nil)
    (funcall frame-printer-function sg frame i)))

(defun full-backtrace-uninteresting (sg ignore &optional (n most-positive-fixnum) start-frame)
  "Gives an exhaustive (includes args and /"uninteresting/" frames) backtrace.
Optional numeric arg determines how far back to go."
  (full-backtrace sg nil n start-frame t))

(defun print-function-and-args (sg frame &optional describe-function-source-file
				&aux function function-name (rp (sg-regular-pdl sg))
				(*print-level* function-prinlevel)
				(*print-length* function-prinlength))
  "Print the function called in FRAME, and the arguments it has."
  (setq function (rp-function-word rp frame))
  (setq function-name (function-name function))
  (catch-error (format t "~%~S:" function-name) nil)
  (when (typep function 'compiled-function)
    (format t " (P.C. = ~D)"			;Note that this displays the return-pc,
	    (rp-exit-pc rp frame)))		; which is one greater than the D-LAST.  
  (if describe-function-source-file (describe-function-source-file function))
  (terpri)
  (when (eq (car-safe function-name) ':method)
    (print-carefully "self"
      (format t "   (SELF is ~S)~%" (symeval-in-stack-group 'self sg frame))))
  (print-frame-args sg frame 3))

(defun print-frame-args (sg frame indent
			 &aux (*print-level* error-message-prinlevel)
			      (*print-length* error-message-prinlength)
			      function nargs-supplied nargs-to-print
			      (rp (sg-regular-pdl sg))
			      nargs-expected nargs-required active-flag
			      lexpr-call rest-arg-p rest-arg-value)
  "Print the arguments in FRAME, indenting lines by INDENT chars.
If the function called in FRAME wants a rest arg, we return T
/(so our caller can refrain from mentioning local 0,
if he is going to print the locals)."
  (setq function (rp-function-word rp frame)
	nargs-supplied (rp-number-args-supplied rp frame))
  (setq active-flag (sg-frame-active-p sg frame))
  (cond (active-flag
	 (when (legitimate-function-p function)
	   (setq nargs-required
		 (ldb %%arg-desc-min-args (args-info function)))
	   (setq nargs-expected
		 (ldb %%arg-desc-max-args (args-info function))))
	 (multiple-value (rest-arg-value rest-arg-p lexpr-call)
	   (sg-rest-arg-value sg frame))
	 (setq nargs-to-print (sg-number-of-spread-args sg frame)))
	(t
	 (princ "  Frame still accumulating args;
  possible args or stack temps follow:
")
	 (let ((tem (sg-previous-open sg frame)))
	   (setq nargs-to-print (- (if tem (- tem 4)
				       (sg-regular-pdl-pointer sg))
				   frame)
		 nargs-supplied nargs-to-print))))
  ;; Print the individual args.
  (dotimes (i nargs-to-print)
    (and (= i nargs-supplied)
	 (if (and nargs-required (< i nargs-required)) (format t "   --Missing args:--~%")
	     (format t "   --Defaulted args:--~%"))) ;These "args" weren't supplied
    (and nargs-required (< nargs-supplied nargs-required) (= i nargs-required)
	 (format t "   --Optional args:--~%"))	;End of truly missing args
    (and nargs-expected (= i nargs-expected)    ;Called with too many args
	 (format t "   --Extraneous args:--~%"))
    (format t "~VTArg ~D" indent i)
    (and active-flag (display-arg-name " (~A)" function i))
    ;; Print the arg value unless the arg is missing (val is garbage).
    (cond ((not (and nargs-required
		     (> nargs-required nargs-supplied)
		     ( i nargs-supplied)))
	   (princ ": ")
	   (catch-error (prin1 (sg-frame-arg-value sg frame i))) nil))
    (terpri))
  ;; Print the rest arg if any.
  (cond (rest-arg-p
	 (format t "~VTRest arg" indent)
	 (display-local-name " (~A)" function 0)
	 (princ ": "))
	(lexpr-call
	 (format t "~VTExtraneous Rest Arg: " indent)))
  (when (or rest-arg-p lexpr-call)
    (catch-error (prin1 rest-arg-value) nil)
    (terpri))
  rest-arg-p)

(defun display-value-name (format-string function valuenum &aux name)
  "Print the name of value number VALUENUM of FUNCTION, using FORMAT-STRING.
If there is no name known for such a value, prints nothing."
  (setq name (value-name function valuenum))
  (and name (format t format-string name)))

(defun display-local-name (format-string function localno &aux name)
  "Print the name of local number LOCALNO of FUNCTION, using FORMAT-STRING.
If there is no such local or no name known, prints nothing."
  (setq name (local-name function localno))
  (and name (format t format-string name)))

(defun display-arg-name (format-string function argno &aux name)
  "Print the name of arg number ARGNO of FUNCTION, using FORMAT-STRING.
If there is no such arg or no name known, prints nothing."
  (setq name (arg-name function argno))
  (and name (format t format-string name)))

;;;; Basic commands for moving between stack frames.

;; UP means closer to the top of the stack, DOWN means the base of the stack.

;; Control-P, <^>
(defun com-up-stack (sg ignore &optional count show-all-flag reverse-flag uninteresting-flag
			       &aux frame count1)
  "Goes up to the previous stack frame (inward; later function calls)"
  (setq count1 (or count 1))
  (and reverse-flag (setq count1 (- count1)))
  (if uninteresting-flag
      (setq frame (sg-previous-nth-open sg current-frame count1))
    (setq frame (sg-previous-nth-interesting-active sg current-frame count1
						    innermost-visible-frame)))
  (cond ((= frame current-frame)
	 (format t (cond (reverse-flag
			  "You are already at the bottom of the stack.~%")
			 (t "You are already at the top of the stack.~%"))))
	(t (setq current-frame frame)
	   (cond ((not show-all-flag) (show-function-and-args sg))
		 (t (show-all sg)))))
  nil)

;; Control-N, <line>
(defun com-down-stack (sg error-object &optional count)
  "Goes down to the next stack frame (outward; earlier function call)"
  (com-up-stack sg error-object count nil t))

;; Meta-P.
(defun com-up-stack-all (sg error-object &optional count)
  "Goes up to the previous stack frame (inward; later function call)
and shows the args, locals and compiled code."
  (com-up-stack sg error-object count t))

;; Meta-N.
(defun com-down-stack-all (sg error-object &optional count)
  "Goes down to the next stack frame (outward; earlier function call)
and shows the args, locals and compiled code."
  (com-up-stack sg error-object count t t))

;; Meta-<.
(defun com-top-stack (sg &rest ignore)
  "Goes to the top of the stack (innermost; most recent call)"
  (setq current-frame (sg-out-to-interesting-active sg error-locus-frame))
  (show-function-and-args sg)
  nil)

;; Meta->.
(defun com-bottom-stack (sg &rest ignore)
  "Goes to the bottom of the stack (outermost; oldest function call)"
  (setq current-frame
	(do ((frame innermost-visible-frame (sg-next-active sg frame))
	     (prev-frame nil frame))
	    ((null frame) prev-frame)))
  (show-function-and-args sg)
  nil)

;; Control-Meta-P.
(defun com-up-stack-uninteresting (sg error-object &optional count)
  "Goes up to the previous stack frame (inwards; later function call)
Includes internal /"uninteresting/" frames, such as EVAL and PROG, etc."
  (com-up-stack sg error-object count nil nil t))

;; Control-Meta-N.
(defun com-down-stack-uninteresting (sg error-object &optional count)
  "Goes down to the next stack frame (inwards; later function call)
Includes internal /"uninteresting/" frames, such as EVAL and PROG, etc."
  (com-up-stack sg error-object count nil t t))

;; Control-Meta-U.
(defun com-up-to-interesting (sg ignore &optional ignore)
  "Goes down to the previous /"interesting/" stack frame (outwards; earlier function call)"
  (setq current-frame (sg-out-to-interesting-active sg current-frame))
  (show-function-and-args sg)
  nil)

;; Control-L, form.
(defun com-clear-and-show (sg error-object &rest ignore)
  "Clears the screen and redisplays the error message."
  (send *standard-output* ':clear-screen)
  (print-carefully "error message"
    (send error-object ':print-error-message sg nil *standard-output*))
  (show-function-and-args sg)
  (warn-about-special-variables sg)
  (describe-proceed-types sg error-object)
  (unless error-handler-running
    (format t "~2&Examine-only mode; you cannot resume or alter execution.
Type ~C to exit the debugger.~%" #/resume))
  nil)

;; Control-Meta-Q
(defun com-describe-proceed-types (sg error-object &rest ignore)
  "Describes the possible proceed types."
  (describe-proceed-types sg error-object)
  nil)

;; Meta-L.
(defun com-clear-and-show-all (sg &rest ignore)
  "Clears the screen and redisplays the error message, args, locals and compiled code."
  (show-all sg)
  nil)

;; Control-S.
(defun com-search (sg ignore &optional ignore flag &aux key frame)
  "Prompts for a string and searches down the stack for a frame containing a call to a
function whose name contains that string."
  (format t "~%String to search for (end with RETURN):~%")
  (setq key (readline))
  (setq frame 
	(do ((frame current-frame (sg-next-active sg frame))
	     (rp (sg-regular-pdl sg))
	     (name)
	     )
	    ((null frame) nil)
	  (setq name (function-name (rp-function-word rp frame)))
	  (setq name
		(cond ((stringp name) name)
		      ((symbolp name) (string name))
		      (t (format nil "~S" name))))
	  (and (string-search key name)
	       (return frame))))
  (cond ((null frame)
	 (format t "Search failed.~%"))
	(t
	 (setq current-frame frame)
	 (cond ((not flag) (show-function-and-args sg))
	       (t (show-all sg))))))

(defun com-search-and-show-all (sg error-object &optional (count 1))
  "Prompts for a string and searches down the stack for a frame containing a call to a
function whose name contains that string, and then displays args, locals and compiled code."
  (com-search sg error-object count t))

(defun com-bug-report (ignore error-object &optional arg)
  "Mails a bug report containing a backtrace of the stack.
You are prompted for a message. The depth of stack backtrace included is determined
by the optional numeric argument."
  (if (eq *terminal-io* tv:cold-load-stream)
      ;; If windows are losing, don't try switching windows.
      (progn
	(format t "Please type a precise, detailed description
of what you did that led up to the bug.
")
	(bug (send error-object ':bug-report-recipient-system)
	     (string-append (zwei:qsend-get-message)
			    #/return
			    (format:output nil
			      (send error-object ':bug-report-description
				    *standard-output* arg)))))
    (format t " Mail a bug report.   Entering the editor...")
    (bug (send error-object ':bug-report-recipient-system)
	 (format:output nil
	   "Insert your description of the circumstances here:


"
	   (send error-object ':bug-report-description *standard-output* arg))
	 52.)))		;This is the length of the constant, above, minus one.

;;;; The guts of the commands on the previous page.

;; SHOW-FUNCTION-AND-ARGS is regular printing tty stuff.
;; SHOW-ALL clears the screen and then fills it up.
(defun show-function-and-args (sg)
  (print-function-and-args sg current-frame))

(defun show-all (sg &aux rp function)
  "Print everything about the current frame, including locals and disassembly."
  (setq rp (sg-regular-pdl sg)
	function (rp-function-word rp current-frame))
  (send *standard-output* ':clear-screen)
  ;; Print the header, including the function name
  (format t "Frame address ~D" current-frame)
  (if (not (zerop (rp-adi-present rp current-frame)))
      (show-adi rp (- current-frame 4)))
  (terpri)
  (if (typep function 'compiled-function)
      (show-all-macro sg current-frame)
    (show-function-and-args sg)))

(defun describe-function-source-file (function)
  (when (or (validate-function-spec function)
	    (functionp function t))
    (let* ((name (function-name function))
	   (file (cadr (assq 'defun (si:get-all-source-file-names name)))))
      (when (and file (neq (send file :host) (fs:get-pathname-host "SYS")))
	(format t " (from file ~A)" file)))))

(defun show-frame-briefly-for-bug-message (sg frame &aux rp function)
  (setq rp (sg-regular-pdl sg)
	function (rp-function-word rp frame))
  (send *standard-output* ':fresh-line)
  (if (typep function 'compiled-function)
      (format t "~S (P.C. = ~D)" (fef-name function) (rp-exit-pc rp frame))
    (prin1 (function-name function)))
  (describe-function-source-file function)
  (terpri))

(defun show-frame-for-bug-message (sg frame &aux rp function)
  (setq rp (sg-regular-pdl sg)
	function (rp-function-word rp frame))
  (if (not (zerop (rp-adi-present rp frame)))
      (show-adi rp (- frame 4)))
  (terpri)
  (if (typep function 'compiled-function)
      (show-all-macro sg frame t)
    (print-function-and-args sg frame t)))

(defun warn-about-special-variables (sg)
  "Print warnings about vital special variables whose values in SG are peculiar."
  (condition-case ()
      (let ((*print-base* (symeval-in-stack-group '*print-base* sg))
	    (*read-base* (symeval-in-stack-group '*read-base* sg)))
	(if (eq *print-base* *read-base*)
	    (unless (numberp *print-base*)
	      (format t "~&Warning: *PRINT-BASE* and *READ-BASE* are ~D." *print-base*))
	  (format t "~&Warning: *PRINT-BASE* is ~D but *READ-BASE* is ~D (both decimal).~%"
		  *print-base* *read-base*)))
    (error
      (format t "~&Warning: error while trying to find *PRINT-BASE* and *READ-BASE* in ~S."
	      sg)))
  (condition-case ()
      (let ((areaname (area-name (symeval-in-stack-group 'default-cons-area sg))))
	(or (eq areaname 'working-storage-area)
	    (format t "~&Warning: the default cons area is ~S~%" areaname)))
    (error
      (format t "~&Warning: error while trying to find DEFAULT-CONS-AREA in ~S." sg)))
  (condition-case ()
      (if (symeval-in-stack-group 'tail-recursion-flag sg)
	  (format t "~&Note: TAIL-RECURSION-FLAG is set, so some frames may no longer be on the stack.~%"))
    (error
      (format t "~&Warning: error while trying to find TAIL-RECURSION-FLAG in ~S." sg)))
  (format t "~&;Reading in base ~D in package ~A with ~A.~&"
	  *read-base* *package* *readtable*))

(defun show-all-macro (sg frame &optional no-disassembled-code
		       &aux n-locals pc-now name rest-arg-printed nlines where lim-pc
		       (rp (sg-regular-pdl sg))
		       (function (rp-function-word rp frame)))
  (setq n-locals (fef-number-of-locals function)
	name (fef-name function)
	pc-now (rp-exit-pc rp frame)
	lim-pc (compiler::disassemble-lim-pc function))
  (format t "~%~S (P.C. = ~D)" name pc-now)
  (when no-disassembled-code (describe-function-source-file name))
  (terpri)
  (when (eq (car-safe name) ':method)
    (format t "  (SELF is ")
    (print-carefully "SELF"
      (prin1 (symeval-in-stack-group 'self sg frame)))
    (format t ")~%"))
  (terpri)
  ;; Print the arguments, including the rest-arg which is the first local
  (setq rest-arg-printed (print-frame-args sg frame 0))
  (cond ((sg-frame-active-p sg frame)
	 ;; Print the rest of the locals -- if the frame is active.
	 (dotimes (i n-locals)
	   (cond ((not (and rest-arg-printed (zerop i)))	;Don't show rest arg twice
		  (format t "Local ~D" i)
		  (display-local-name " (~A)" function i)
		  (let ((*print-level* error-message-prinlevel)
			(*print-length* error-message-prinlength))
		    (format t ": ")
		    (print-carefully "local"
		      (prin1 (sg-frame-local-value sg frame i)))
		    (terpri)))))
	 (unless no-disassembled-code
	   (format t "~%Disassembled code:")
	   ;; Figure out how many instructions will fit in the stream we are using.
	   (setq nlines
		 (max disassemble-instruction-count	;don't show absurdly few
		      (cond ((send *standard-output* ':operation-handled-p
				   		     ':size-in-characters)
			     (setq nlines (nth-value 1
				            (send *standard-output* ':size-in-characters))
				   where (nth-value 1
					    (send *standard-output* ':read-cursorpos ':character)))
			     ;; Leave 1 line for prompt, 1 for extra terpri
			     (- nlines where 2))
			    (t 0))))		;Don't know size of window, use default count
	   (do ((i 0 (1+ i))
		(pc (max (fef-initial-pc function) (- pc-now (truncate nlines 2)))
		    (+ pc (compiler::disassemble-instruction-length function pc))))
	       ((or ( i nlines) ( pc lim-pc))
		(cond ((= pc pc-now)		;If arrow should point after all code,
		       (terpri) (princ "=> "))))
	     (terpri)
	     (princ (if (= pc pc-now) "=> " "   "))
	     (compiler::disassemble-instruction function pc))
	   ;; This kludge is to prevent prompt from triggering a **MORE** when it comes out
	   ;; on the bottom line of the window
	   (send *standard-output* ':send-if-handles ':notice ':input-wait)))))

(defun show-adi (rp idx)
  "Print the ADI at index IDX in the regular pdl RP"
  (format t "~2%Additional information supplied with call:")
  (do ((idx idx (- idx 2)))
      (())
    (let ((type (ldb %%adi-type (aref rp idx)))
	  (more-p (%p-ldb %%adi-previous-adi-flag (aloc rp (1- idx)))))
      (select type
	((adi-return-info adi-used-up-return-info)
	 (show-mv-specs rp idx))
	(adi-bind-stack-level
	 (format t "~% Binding stack level: ~S" (aref rp (1- idx))))
	(adi-restart-pc
	 (format t "~% Restart PC on *THROW: ~D" (aref rp (1- idx))))
	(otherwise
	 (format t "~% ~S" (nth type adi-kinds))))
      (if (zerop more-p) (return)))))

(defun show-mv-specs (rp idx)
  (let ((storing-option (nth (ldb %%adi-ret-storing-option (ar-1 rp idx))
			     adi-storing-options)))
    (cond ((eq storing-option 'adi-st-block)
	   (format t "~% Expecting ~D values"
		   (ldb %%adi-ret-num-vals-total (ar-1 rp idx)))
	   (let ((num-already
		   (- (ldb %%adi-ret-num-vals-total (ar-1 rp idx))
		      (ldb %%adi-ret-num-vals-expecting (ar-1 rp idx)))))
	     (or (zerop num-already)
		 (format t "; ~D already returned." num-already))))
	  ((eq storing-option 'adi-st-make-list)
	   (format t "~% Values to be collected for MULTIPLE-VALUE-LIST"))
	  ((eq storing-option 'adi-st-list)
	   (format t "~% Values being collected for MULTIPLE-VALUE-LIST")
	   (format t "; ~D values already returned."
		   (length (aref rp (1- idx)))))
	  ((eq storing-option 'adi-st-indirect)
	   (if (aref rp (1- idx))
	       (format t "~% Multiple values being passed to frame at ~D"
		       (+ 4 (%pointer-difference (aref rp (1- idx))
						 (aloc rp 0))))
	     (format t "~% Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them."))))))

;;;; Commands for looking at special bindings.

;; Control-Meta-S: Print names and values of specials bound by this frame.
;; If SELF is bound in this frame, its instance variables are also included.
(defun com-print-frame-bindings (sg error-object &optional count
				 &aux start end (sp (sg-special-pdl sg))
				 self-included special-vars)
  "Lists the names and values of all special variables bound by this frame."
  error-object
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
	 ;; Maybe print one or all, but in any case compute SELF-INCLUDED
	 ;; and SPECIAL-VARS.
	 (do ((i start (+ i 2)))
	     (( i end))
	   (push (symbol-from-value-cell-location (aref sp (1+ i)))
		 special-vars)
	   (if (eq (car special-vars) 'self)
	       (setq self-included i))
	   (when (or (null count) (= count (lsh (- i start) -1)))
	     (format t "~:[~% ~S: ~;~&Value of ~S in this frame:~&   ~]"
		     count (car special-vars))
	     (multiple-value-bind (val error)	;Value
		 (catch-error (funcall (or prin1 #'prin1) (aref sp i)) nil)
	       (if error (setq val (princ "void")))
	       (when count
		 (setq ////// //// //// // // (list val))
		 (setq *** ** ** * * val)
		 (setq +++ ++ ++ + + - - (value-cell-location (car special-vars)))))))
	 ;; List the instance variable of SELF also if SELF is bound in this frame.
	 (if (and self-included
		  (typep (aref sp self-included) ':instance)
		  ;; But not if want only one variable and already got it.
		  (or (null count)
		      ( count (lsh (- end start) -1))))
	     (let* ((self-value (aref sp self-included))
		    (self-flavor 
		     (si:instance-flavor self-value))
		    (self-vars (si:flavor-all-instance-variables-slow self-flavor)))
	       (unless count
		 (format t "~2&Non-special instance variables of ~S~&" self-value))
	       (do ((sv self-vars (cdr sv))
		    (count-unspecial (lsh (- end start) -1))
		    (i 1 (1+ i)))
		   ((null sv))
		 (when (and (not (memq (car sv) special-vars))
			    (or (null count)
				(= (1+ count) (incf count-unspecial))))
		   (format t "~:[~% ~S: ~;~&Value of instance variable ~S within SELF:~&   ~]"
			   count (car sv))
		   (multiple-value-bind (val error)
		       (catch-error (funcall (or prin1 #'prin1)
					     (%instance-ref self-value i)) nil)
		     (if error (setq val (princ "void")))
		     (cond (count
			    (setq ////// //// //// // // (list val))
			    (setq *** ** ** * * val)
			    (setq +++ ++ ++ + + - - (%instance-loc self-value i))))))))))
	(t (format t "~&No specials bound in this frame"))))

;; Meta-S: Print the value as seen in the current frame (at the point at which
;; it called out or erred) of a specified variable.
;; Instance variables of SELF's value are also allowed.
(defun com-print-variable-frame-value (sg error-object &optional ignore)
  "Print the value of a special variable within the context of the current stack frame.
Prompts for the variable name."
  error-object
  (terpri)
  (multiple-value-bind (var full-rubout
			self-value self-pos)
      (with-input-editing (*standard-input* '((:full-rubout :full-rubout)
					      (:activation = #/end)
					      (:prompt "Value in this frame of special variable: ")))
	(si:read-for-top-level))
    (if (eq full-rubout ':full-rubout) (return-from com-print-variable-frame-value nil))
    (multiple-value-bind (value boundflag)
	(symeval-in-stack-group var sg current-frame)
      (terpri)
      (setq ////// //// //// //)
      (setq *** ** ** *)
      (setq +++ ++ ++ + + - - var)
      (cond (boundflag (funcall (or prin1 #'prin1) (setq * value)))
	    ((progn
	      (setq self-value
		    (symeval-in-stack-group 'self sg current-frame))
	      (and (instancep self-value)
		   (setq self-pos
			 (find-position-in-list
			   var
			   (si:flavor-all-instance-variables-slow
			     (si:instance-flavor self-value))))))
	     (funcall (or prin1 #'prin1) (setq * (%instance-ref self-value (1+ self-pos)))))
	    (t (princ (setq * "Void"))))
      (setq // (list *)))))

(defun com-print-frame-handlers (sg error-object &optional ignore)
  "Lists the condition handlers set up in this frame. (includes resume and default handlers)"
  error-object
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
    (unless (eq handlers handlers-outside)
      (if (null handlers)
	  (format t "~&Condition handlers bound off, hidden from lower levels.")
	(format t "~&Condition handlers:~%")
	(dolist (h (ldiff handlers handlers-outside))
	  (format t "~& Handler for ~S" (car h)))))
    (unless (eq default-handlers default-handlers-outside)
      (if (null default-handlers)
	  (format t "~&Default handlers bound off, hidden from lower levels.")
	(format t "~&Default handlers:~%")
	(dolist (h (ldiff default-handlers default-handlers-outside))
	  (format t "~& Default handler for ~S" (car h)))))
    (unless (eq resume-handlers resume-handlers-outside)
      (format t "~&Resume handlers:~%")
      (dolist (h (ldiff resume-handlers resume-handlers-outside))
	(if (eq h t)
	    (format t "~& Barrier hiding all resume handlers farther out.")
	  (format t "~& Resume handler for ~S,~%  " (car h))
	  (unless (listp (cadr h))
	    (format t "proceed type ~S,~%  " (cadr h)))
	  (apply 'format t (cadddr h)))))
    (and (eq resume-handlers resume-handlers-outside)
	 (eq default-handlers default-handlers-outside)
	 (eq handlers handlers-outside)
	 (format t "~&No handlers set up in this frame."))))

;;;; Other informational commands.

;; Control-A.
(defun com-arglist (sg &rest ignore)
  "Returns the arglist for the current stack frame function."
  (let ((function (rp-function-word (sg-regular-pdl sg) current-frame)))
    (format t "~&Argument list for ~S is ~A.~%"
	    (function-name function)
	    (arglist function)))
  nil)

;; Control-Meta-A
(defun com-get-arg (sg ignore &optional (arg 0))
  "Sets * to the nth arg to the current function, where n is a numeric argument (default 0)
Also sets + to a locative to the arg."
  (terpri)
  (multiple-value-bind (a b barf)
     (sg-frame-arg-value sg current-frame arg nil)
    (if barf (princ barf)
      (setq ////// //// //// // // (list a))
      (setq *** ** ** * * a)
      (setq +++ ++ ++ + + - - b)
      (funcall (or prin1 #'prin1) *))))

;; Control-F
(defun com-show-foothold (sg &rest ignore)
  (let ((end-of-foothold-data (- (sg-previous-open sg current-frame) 4))
	(rp (sg-regular-pdl sg)))
    (cond ((not (eq (rp-function-word rp current-frame) #'foothold))
	   (ferror nil "This is not a FOOTHOLD frame.")))
    (do ((i (1+ current-frame) (+ i 2))
	 (sg-q si:stack-group-head-leader-qs (cdr sg-q)))
	((> i end-of-foothold-data))
      (format t "~&~s:~35t#<~s ~o>"
	      (car sg-q)
	      (or (nth (%p-ldb-offset %%q-pointer
				      rp
				      (+ 1 i (si:array-data-offset rp)))
		       q-data-types)
		  'bad-data-type)
	      (%p-ldb-offset %%q-pointer rp (+ i (si:array-data-offset rp)))))))

;; Control-Meta-L
(defun com-get-local (sg ignore &optional (arg 0))
  "Sets * to the nth local of he current function, where n is a numeric argument
/(default 0) Also sets + to a locative to the local."
  (terpri)
  (multiple-value-bind (a b barf)
      (sg-frame-local-value sg current-frame arg nil)
    (if barf (princ barf)
      (setq ////// //// //// // // (list a))
      (setq *** ** ** * * a)
      (setq +++ ++ ++ + + - - b)
      (funcall (or prin1 #'prin1) *))))

;; Control-Meta-V
(defun com-get-value (sg ignore &optional (arg 0))
  "Sets * to the nth value being returned by the current function, where n is a numeric
argument (default 0) Also sets + to a locative to the value."
  (terpri)
  (multiple-value-bind (a b barf)
      (sg-frame-value-value sg current-frame arg nil)
    (if barf (princ barf)
      (setq ////// //// //// // // (list a))
      (setq *** ** ** * * a)
      (setq +++ ++ ++ + + - - b)
      (funcall (or prin1 #'prin1) *))))

;; c-m-F
(defun com-get-function (sg ignore &optional ignore)
  "Sets * to the current function, and + to a locative to the it."
  (setq ////// //// //// //)
  (setq *** ** ** *)
  (setq +++ ++ ++ + + -)
  (setq - (locf (rp-function-word (sg-regular-pdl sg) current-frame))
	* (contents -)
	// (list *))
  (terpri t)
  (funcall (or prin1 #'prin1) *))

;; Control-E
(defun com-edit-frame-function (sg &rest ignore)
  "Edit the source code for the current function in Zmacs."
  (if (eq *terminal-io* si:cold-load-stream)
      (format t "~&The editor cannot be invoked since we are using the cold load stream.")
    (let* ((rp (sg-regular-pdl sg))
	   (fn (function-name (rp-function-word rp current-frame))))
      (if fn (ed fn)
	(format t "~&This frame's function's name cannot be determined.")))))

;;; Meta-T
;(defun com-show-frame-temporaries (sg ignore &optional ignore &aux index rp)
;  "Lists temporaries already pushed onto the stack when execution was interrupted."
;  (setq index current-frame
;	rp (sg-regular-pdl sg))
;  (unless (zerop (rp-adi-present rp index))
;    (do ((idx (- index 4) (- idx 2))
;	 more-p)
;	((zerop more-p)
;	 (setq index idx))
;      (setq more-p (%p-ldb %%adi-previous-adi-flag (aloc rp (1- idx))))))
;  (cerror "Foo!" "meta-t ~D//~D" index current-frame))


;;;; HELP!
(defconst com-help-alist '(((#/G "General"
			     "General information about using the debugger") #/G #/g)
			   ((#/I "Information"
			         "Various ways of obtaining information about the current stack frame")
				 #/I #/i #/E #/e)
			   ((#/F "Stack Frames" "Selecting Stack Frames to examine") #/F #/f)
			   ((#/S "Stepping" "Stepping though through the program") #/S #/s)
			   ((#/P "Proceeding"
			         "Proceeding from this error and resuming execution")
			         #/P #/p #/X #/x)
			   ((#/T "Transferring"
			         "Transferring to other systems: Edit, Bug report, Window-based Debugger")
				 #/T #/t)
			   ((#/D "Describe"
			     "Give the documentation of the function associated with a command key")
				 #/D #/d #/C #/c)
			   ((#/abort "Abort" t) #/abort #/c-Z #/c-G)
			   ((#/help "Help" t) #/help #/?))
  "FQUERY options for used in giving help for debugger commands.")

;; ?, <help>
(defun com-help (sg error-object &rest ignore)
  "Help for using the debugger"
  (tagbody
   again
      (selectq (fquery `(:choices ,com-help-alist
				  :help-function nil)
		       "Help for debugger commands. Choose a topic: ")
	(#/G (format t "

You are in the debugger.  If you don't want to debug this error, type ~C.
Otherwise you can evaluate expressions in the context of the error, examine
the stack, and proceed//throw//return to recover.
  If you type in a Lisp form, it will be evaluated, and the results printed,
using ~A and base ~D in package ~A. This
evaluation uses the variable environment of the stack frame you are examining.
  Type ~c or ~c to get back to top level, or the previous debugger level.
While in the debugger, ~c quits back to the debugger top level.
  If you think this error indicates a bug in the Lisp machine system, use the
~:c command."
		     #/Abort *readtable* *read-base* *package*
		     #/Abort #/c-Z #/c-G #/c-M))
	(#/I (format t "

~c or ~\lozenged-string\ clears screen and retypes error message.
~c clears screen and types args, locals and compiled code.
~c gives a backtrace of function names.
~c gives a backtrace of function names and argument names and values.
~c is line ~c but shows EVALs, PROGs, CONDs, etc.
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

Use the functions (EH-ARG n), (EH-LOC n), (EH-VAL n) and (EH-FUN) to get the
value of an arg, local, value or function-object respectively from an
expression being evaluated. For args and locals, n can be a name or a number.
EH-VAL allows numbers only. LOCF and SETF on those expressions are also allowed.
" #/c-L "Clear Screen" #/m-L #/c-B #/c-m-B #/m-B #/c-B
  #/c-m-A #/c-m-A #/c-m-L #/c-m-A #/c-m-V #/c-m-A #/c-m-F #/c-A #/m-S #/c-m-S))
	(#/F (format t "

~c or ~\lozenged-char\ goes down a frame, ~c or ~\lozenged-char\ goes up.
~c and ~c are similar but show args, locals and compiled code.
~c and ~c are similar to ~c and ~c, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
~c and ~c go to the top and bottom of the stack, respectively.
~c reads a string and searches down the stack for a frame
   calling a function whose name contains that substring.
" #/c-N #/line #/c-P #/return #/m-N #/m-P #/c-m-N #/c-m-P #/c-N #/c-P #/m-< #/m-> #/c-S))
	(#/S (if error-handler-running
		 (format t "

~c toggles the trap-on-exit flag for the current frame.
~c sets the trap-on-exit flag for the current frame and all outer frames.
~c clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
~c proceeds like ~C, but first sets the trap-on-next-function-call flag.
~c toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with ~c.
" #/c-X #/m-X #/c-m-X #/c-D #/resume #/m-D #/c-X)
	       (format t "

You cannot use the stepping commands, since you are in examine-only mode.
~C exits the debugger." #/resume)))
	(#/P (cond (error-handler-running
		    (format t "

~c aborts to previous debugger or other command loop, or to top level.
~c returns a value or values from the current frame.
~c offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
~c offers to reinvoke the current frame, letting you alter
   some of the arguments, or use more or fewer arguments.
~c throws to a specific tag." #/c-Z #/c-R #/c-m-R #/m-R #/c-T)
		    (describe-proceed-types sg error-object))
		   (t
		    (format t "

You cannot continue execution, since you are in examine-only mode.
~C exits the debugger." #/resume))))
	(#/T (format t "

~c calls the editor to edit the current function.
~c enters the editor to send a bug message, and puts the error
  message and a backtrace into the message automatically.
  A numeric argument says how many stack frames to put in the backtrace.
~c switches to the window-based debugger.
" #/c-E #/c-M #/c-m-W))
	(#/D (com-help-describe-command error-object sg))
	(#/help
	 (format t "~&For additional help in using the debugger, type one of the following:~%")
	 (dolist (x com-help-alist)
	   (unless (eq (third (car x)) t)
	     (format t "~&  ~:C~6T~A" (cadr x) (or (third (car x)) (second (car x))))))
	 (go again))
	(#/abort))))

(defun com-help-describe-command (error-object sg &aux char)
  "Prompts for a character and describes what the current meaning of that keystoke
is as a command to the error-handler."
  (format t "~&Describe Command. Type command character: ")
  (let ((reading-command t))			;let abort, etc through
    (setq char (send *standard-input* ':tyi)))
  (let* ((command (command-lookup char))
	 (resume-handlers (symeval-in-stack-group 'condition-resume-handlers sg))
	 (proceed-types (send error-object ':user-proceed-types
			      (sg-condition-proceed-types sg error-object)))
	 (keywords (append proceed-types special-commands))
	 tem)
    (format t "~:C~&~C: " char char)
    (cond ((setq tem (rassq char *proceed-type-special-keys*))
	   (send error-object ':document-proceed-type (car tem)
		 *standard-output* resume-handlers))
	  ((setq tem (rassq char *special-command-special-keys*))
	   (send error-object ':document-special-command (car tem)
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
	   (if (not (< -1 (- char (char-code #/s-A)) (length keywords)))
	       (format t "is not currently a defined debugger command.")
	     (send error-object (if (< (- char (char-code #/s-A)) (length proceed-types))
				    ':document-proceed-type
				    ':document-special-command)
		   (nth (- char (char-code #/s-A)) keywords)
		   *standard-output* resume-handlers)))
	  (t (if (documentation command)
		 (format t "~A" (documentation command))
	         (format t "~S is not documented" command))
	     (send *standard-output* ':fresh-line)
	     (case command
	       (com-abort
		(let ((abort-handler (find-resume-handler abort-object nil resume-handlers)))
		  (cond ((dolist (x proceed-types)
			   (when (eq (find-resume-handler error-object x resume-handlers)
				     abort-handler)
			     (format t "~&This command is currently synonymous with ~C: "
				     (+ (char-code #/s-A) (position x proceed-types)))
			     (send error-object ':document-proceed-type x
				   *standard-output* resume-handlers)
			     (return t))))
			(abort-handler
			 (send abort-object ':document-proceed-type (second abort-handler)
			       *standard-output* resume-handlers))
			(t (format t "There is no way to abort from this error.")))))
	       (com-proceed
		(if proceed-types
		    (progn (format t "~&This command is currently synonymous with ~C: " #/s-A)
			   (send error-object ':document-proceed-type (car proceed-types)
				 *standard-output* resume-handlers))
		  (format t "There is no way to proceed from this error."))))))))

;;;; Commands for resuming execution.

;; Abort.  If there is a numeric arg, just flush the arg.
(defun com-abort (sg error-object &optional count)
  "Aborts out of this error if possible."
  (if count (*throw 'quit nil))
  (com-top-level-throw sg error-object))

;; Rubout.  Flush numeric arg.
(defun com-flush-numeric-arg (ignore ignore &rest ignore)
  "Flushes the numeric arg, if any."
  (*throw 'quit nil))

;; Control-Z.
(defun com-top-level-throw (sg error-object &optional (count 1))
  "Throws to the top level in the current process."
  error-object count
  (leaving-error-handler)
  (error-handler-must-be-running) 
  (cond ((eq sg si:scheduler-stack-group)
	 (format t "~&Restarting the scheduler.")
	 (let ((proc (symeval-in-stack-group 'current-process sg)))
	   (when proc
	     (si:process-blast proc)
	     (format t "~%Blasting ~S so this won't happen again" proc)))
	 (stack-group-preset sg (si:appropriate-process-scheduler))
	 (sg-run-goodbye sg))
	(t
	 (let ((sg-to-abort sg)
	       (sg-innermost-frame innermost-visible-frame))
	   (cond ((and (neq sg (process-initial-stack-group current-process))
		       (not (memq sg (send current-process ':coroutine-stack-groups)))
		       (null (symeval-in-stack-group
			       'condition-resume-handlers sg)))
		  ;; Running in a random stack group, get rid of it then throw in the
		  ;; initial stack group.
		  (unwind-sg sg %current-stack-group nil nil)
		  (setq sg-to-abort (process-initial-stack-group current-process))
		  (setq sg-innermost-frame (sg-ap sg)) ))
	   ;; Prevent any trap-on-exits from this throw.
	   (do ((frame sg-innermost-frame (sg-next-active sg-to-abort frame)))
	       ((null frame))
	     (setf (rp-trap-on-exit (sg-regular-pdl sg-to-abort) frame) 0))
	   (sg-abort sg-to-abort))))
  nil)

(defun sg-abort (sg)
  (sg-apply-no-trap sg 'signal-condition (list eh:abort-object) t
		    (not error-handler-running)))

;; Control-T.
(defun com-throw (sg ignore &rest ignore &aux tag val)
  "Throw to a tag which is prompted for."
  (error-handler-must-be-running)
  (format t "Throw a value to a tag.~%")
  (setq tag (read-object ':eval-read "Form to evaluate to get the tag: ")
	val (read-object ':eval-read "~&Form to evaluate to get the value to throw: "))
  (leaving-error-handler)
  (setf (rp-trap-on-exit (sg-regular-pdl sg) innermost-visible-frame) 0)
  (sg-throw sg tag val)
  nil)

(defun com-reinvoke-new-args (sg ignore &rest ignore)
  "Reinvoke the current function with possibly altered arguments."
  (cond ((null error-handler-running)
	 (format t "You can only examine this stack group, not modify it."))
	((not (sg-frame-active-p sg current-frame))
	 (format t "This frame's args are still being computed;
it cannot be reinvoked since it was never invoked."))
	(t
	 (let* ((form (get-frame-function-and-args sg current-frame))
		(function-name (car form))
		(function (rp-function-word (sg-regular-pdl sg) current-frame))
		(argument-list (cdr form))
		(nargs (length argument-list))
		(args-info (args-info function))
		(args-wanted (ldb %%arg-desc-min-args args-info))
		(rest-flag (ldb-test %%arg-desc-any-rest args-info))
		(max-args (ldb %%arg-desc-max-args args-info))
		new-args
		(*print-level* error-message-prinlevel)
		(*print-length* error-message-prinlength))
	   (format t "~&Reinvoke ~S with possibly altered arguments." function-name)
	   (do ((i 0 (1+ i)))
	       ((unless rest-flag (eq i max-args)))
	     (multiple-value-bind (value flag)
		 (prompt-and-read
		   (let ((keyword
			   (if ( i args-wanted)
			       ':eval-read-or-end ':eval-read)))
		     (if (< i nargs)
			 (list keyword ':default (nth i argument-list))
		       keyword))
		   (if (< i nargs)
		       (if ( i args-wanted)
			   "~&Arg ~D~A, or ~\lozenged-char\ not to change it, or ~C: "
			 "~&Arg ~D~A, or ~\lozenged-char\ not to change it: ")
		     (if ( i args-wanted)
			 "~&Arg ~D~A, or ~*~C: "
		       "~&Arg ~D~A: "))
		   i
		   (format:output nil (display-arg-name " (~A)" function i))
		   #/space #/end)
	       (if (eq flag ':end) (return))
	       (if (eq flag ':default)
		   (prin1 value))
	       (setq new-args
		     (nconc new-args
			    (ncons value)))))
	   (setq form (cons function-name new-args))
	   (when (fquery nil "Reinvoking ~S, OK? " form)
	     (setf (rp-trap-on-exit (sg-regular-pdl sg) innermost-visible-frame) 0)
	     (sg-unwind-to-frame-and-reinvoke sg current-frame form)
	     (leaving-error-handler)
	     (without-interrupts
	       (and error-handler-running
		    (free-second-level-error-handler-sg %current-stack-group))
	       (stack-group-resume sg nil)))))))

;; Control-R.
(defun com-return-a-value (sg ignore &rest ignore &aux value
			   (fn (function-name (rp-function-word (sg-regular-pdl sg)
								current-frame))))
  "Specify values to return from the current frame (does not reinvoke the function)"
  (cond ((null error-handler-running)
	 (format t "You can only examine this stack group, not modify it."))
	((not (sg-frame-active-p sg current-frame))
	 (format t "This frame has not yet been activated; you cannot return from it."))
	((null (sg-next-active sg current-frame))
	 (format t "This is the bottom frame; you cannot return from it."))
	(t (multiple-value-bind (nil number-loc-or-nil) (sg-frame-value-list sg current-frame)
	     (cond ((null number-loc-or-nil)
		    (format t "Return a value from the function ~S.~%" fn)
		    (setq value (read-object ':eval-read "Form to evaluate and return: "))
		    (leaving-error-handler)
		    (setf (rp-trap-on-exit (sg-regular-pdl sg) innermost-visible-frame) 0)
		    (sg-unwind-to-frame sg current-frame t value))
		   (t
		    (format t "Return values from the function ~S " fn)
		    (if (numberp number-loc-or-nil)
			(format t "(up to ~D of them)." number-loc-or-nil)
		      (format t "(any number of them)."))
		    (let (accum)
		      (do ((i 0 (1+ i)))
			  ((eq i number-loc-or-nil))
			(multiple-value-bind (value flag)
			    (read-object ':eval-read-or-end "~&Value ~D~A, or ~C: "
					 i (format:output nil
					     (display-value-name " (~A)" fn i))
					 #/end)
			  (if flag (return))
			  (push value accum)))
		      (sg-unwind-to-frame-and-reinvoke sg current-frame
						       `(values . ,(nreverse accum)))
		      (leaving-error-handler)
		      (without-interrupts
			(and error-handler-running
			     (free-second-level-error-handler-sg %current-stack-group))
			(stack-group-resume sg nil))))))))
  nil)

;; Control-Meta-R
(defun com-return-reinvocation (sg ignore &rest ignore
				&aux form (*print-level* error-message-prinlevel)
					  (*print-length* error-message-prinlength))
  "Retries invoking the current function."
  (cond ((null error-handler-running)
	 (format t "You can only examine this stack group, not modify it."))
	((not (sg-frame-active-p sg current-frame))
	 (format t "This frame's args are still being computed;
their values are not known, to re-evaluate with."))
	((fquery '(:list-choices nil :fresh-line nil)
		 " Re-evaluating ~S, OK? "
		 (setq form (get-frame-function-and-args sg current-frame)))
	 (setf (rp-trap-on-exit (sg-regular-pdl sg) innermost-visible-frame) 0)
	 (sg-unwind-to-frame-and-reinvoke sg current-frame form)
	 (leaving-error-handler)
	 (without-interrupts
	   (and error-handler-running
		(free-second-level-error-handler-sg %current-stack-group))
	   (stack-group-resume sg nil)))))

;; Control-C. Resume.
(defun com-proceed (error-sg error-object &rest ignore &aux proceed-types)
  "Proceeds from this error if possible."
  (declare (special error-object))
  (if (not error-handler-running)
      (*throw 'exit t))
  (setq proceed-types (append (send error-object ':user-proceed-types
				    (sg-condition-proceed-types error-sg error-object))
			      special-commands))
  (if (not proceed-types)
      (format t "There is no way to proceed from this error.~%")
    (if (consp (car proceed-types))
	(format t "You cannot proceed; you can only restart various command loops.~%")
      (send error-object ':proceed-asking-user (car proceed-types)
	    'proceed-error-sg
	    'read-object)))
  nil)

;; Handles things like Super-A, and also things like Meta-C.
(defun com-proceed-specified-type (error-sg error-object &rest ignore
				   &aux proceed-types proceed-type)
  "Use a user-specified proceed option for this error."
  (declare (special error-object))
  (error-handler-must-be-running)
  (setq proceed-types (append (send error-object ':user-proceed-types
				    (sg-condition-proceed-types error-sg error-object))
			      special-commands))
  (cond ((rassq eh-command-char *proceed-type-special-keys*)
	 (setq proceed-type (car (rassq eh-command-char *proceed-type-special-keys*)))
	 (unless (memq proceed-type proceed-types)
	   (format t "Proceed type ~S is not available." proceed-type)
	   (setq proceed-type nil)))
	((> (length proceed-types) (- eh-command-char #/s-A) -1)
	 (setq proceed-type (nth (- eh-command-char #/s-A) proceed-types)))
	(t
	 (format t "There are not ~D different ways to proceed."
		 (- eh-command-char #/s-A))))
  (if proceed-type
      (send error-object ':proceed-asking-user proceed-type
	    'proceed-error-sg
	    'read-object))
  nil)

(defun com-special-command (error-sg error-object &rest ignore)
  (let ((special-command-type (car (rassq eh-command-char *special-command-special-keys*))))
    (send error-object ':special-command special-command-type)))

(defun error-handler-must-be-running ()
  (unless error-handler-running
    (format t "The process didn't get an error; this command may not be used.~%")
    (*throw 'quit nil)))

;;;; Stepping commands.

;; C-X: Control the trap-on-exit bits of frames.
(defun com-toggle-frame-trap-on-exit (sg ignore &optional ignore)
  "Toggles whether we trap on exit from this frame."
  (let ((trap-p (not (trap-on-exit-p sg current-frame))))
    (set-trap-on-exit sg current-frame trap-p)
    (terpri)
    (princ (if (trap-on-exit-p sg current-frame)
	       "Break"
	     "Do not break"))
    (princ " on exit from this frame.")))

(defun set-trap-on-exit (sg frame trap-p)
  "Set or clear trap on exit from FRAME in SG.  TRAP-P = T means set, else clear."
  (let ((rp (sg-regular-pdl sg)))
    (if (eq (rp-function-word rp frame) #'*catch)
	(setq trap-p nil))
    (setf (rp-trap-on-exit rp frame) (if trap-p 1 0)))
  trap-p)

(defun trap-on-exit-p (sg frame)
  "T if FRAME in SG is set to trap on being exited."
  (not (zerop (rp-trap-on-exit (sg-regular-pdl sg) frame))))

;; Meta-X
(defun com-set-all-frames-trap-on-exit (sg ignore &optional ignore)
  "Makes all outer frames trap on exit."
  (do ((frame current-frame (sg-next-active sg frame)))
      ((null frame))
    (set-trap-on-exit sg frame t))
  (format t "~%Break on exit from this frame and all outer active frames."))

;; Control-Meta-X
(defun com-clear-all-frames-trap-on-exit (sg ignore &optional ignore)
  "Clears the trap-on-exit flag for all outer frames."
  (do ((frame current-frame (sg-next-open sg frame)))
      ((null frame))
    (set-trap-on-exit sg frame nil))
  (format t "~%Do not break on exit from this frame and all outer frames."))

;; Control-D: Proceed, and trap next function call.
(defun com-proceed-trap-on-call (sg error-object &optional ignore)
  "Proceeds from this error (if that is possible) and traps on the next function call."
  (setf (sg-flags-trap-on-call sg) 1)
  (format t "Trap on next function call. ")
  (com-proceed sg error-object))

;; Meta-D: Toggle whether to trap on next function call.
(defun com-toggle-trap-on-call (sg ignore &optional ignore)
  "Toggle whether to trap on next function call."
  (setf (sg-flags-trap-on-call sg) (logxor 1 (sg-flags-trap-on-call sg)))
  (terpri)
  (princ (if (zerop (sg-flags-trap-on-call sg))
	     "Do not break"
	     "Break"))
  (princ " on next function call."))

(setf (documentation 'com-number 'function)
      "Used to give a numeric argument to debugger commands.")
	   
;;;; Breakon
(defvar breakon-functions nil
  "List of all function-specs that have BREAKONs.")

(defun breakon (&optional function-spec (condition t))
  "Break on entry to FUNCTION-SPEC, if CONDITION evaluates non-NIL.
If called repeatedly for one function-spec with different conditions,
a break will happen if any of the conditions evaluates non-NIL.

With no args, returns a list of function specs that have had
break-on-entry requested with BREAKON."
  (if (null function-spec)
      breakon-functions
    (setq function-spec (dwimify-arg-package function-spec 'function-spec))
    (breakon-init function-spec)
    (setq condition (si:rename-within-new-definition-maybe function-spec condition))
    (let* ((spec1 (si:unencapsulate-function-spec function-spec 'breakon)))
      (uncompile spec1 t)
      (let* ((def (fdefinition spec1))
	     (default-cons-area background-cons-area)
	     ;; Find our BREAKON-THIS-TIME.
	     ;; def looks like:
	     ;;   (named-lambda (foo debugging-info) arglist
	     ;;	    (si:encapsulation-let ((arglist (si:encapsulation-list* arglist)))
	     ;;	       (declare (special arglist))
	     ;;        (breakon-this-time conditions unencapsulated-function arglist)))
	     (defn-data (car (si:encapsulation-body def)))
	     (slot-loc (cadr defn-data)))	;Within that, find ptr to list of conditions.
	(or (member condition (cdr slot-loc)) (push condition (cdr slot-loc)))))
    (if compile-encapsulations-flag
	(compile-encapsulations function-spec 'breakon))
    function-spec))

(defun unbreakon (&optional function-spec (condition t))
  "Remove break on entry to FUNCTION-SPEC, or all functions if no arg.
If CONDITION is specified, we remove only that condition for breaking;
if other conditions have been specified with BREAKON on this function,
the other conditions remain in effect."
  (and function-spec
       (setq function-spec (dwimify-arg-package function-spec 'function-spec)))
  (let* ((spec1 (and function-spec (si:unencapsulate-function-spec function-spec 'breakon))))
    (cond ((null function-spec)
	   (mapc 'unbreakon breakon-functions))
	  ((eq condition t)
	   (fdefine spec1 (fdefinition (si:unencapsulate-function-spec spec1 '(breakon))))
	   (setq breakon-functions (delete function-spec breakon-functions))
	   function-spec)
	  ((neq spec1 (si:unencapsulate-function-spec spec1 '(breakon)))
	   (uncompile spec1 t)
	   (let* ((def (fdefinition spec1))
		  ;; Find our BREAKON-NEXT-TIME.
		  ;; def looks like:
		  ;;   (named-lambda (foo debugging-info) arglist
		  ;;	    (si:encapsulation-let ((arglist (si:encapsulation-list* arglist)))
		  ;;	       (declare (special arglist))
		  ;;        (breakon-this-time conditions unencapsulated-function arglist)))
		  (defn-data (car (si:encapsulation-body def)))
		  (slot-loc (cadr defn-data)))	;Within that, find ptr to list of conditions.
	     (setf (cdr slot-loc)
		   (delete condition (cdr slot-loc)))
	     (cond ((null (cdr slot-loc))
		    (fdefine spec1
			     (fdefinition (si:unencapsulate-function-spec spec1 '(breakon))))
		    (setq breakon-functions (delete function-spec breakon-functions)))
		   (compile-encapsulations-flag
		    (compile-encapsulations function-spec 'breakon))))
	   function-spec))))

;;; Make a specifed function into an broken-on function
;;; (with no conditions yet) if it isn't one already.
(defun breakon-init (function-spec)
  (let ((default-cons-area background-cons-area)
	(spec1 (si:unencapsulate-function-spec function-spec 'breakon)))
    (when (eq spec1 (si:unencapsulate-function-spec spec1 '(breakon)))
      (si:encapsulate spec1 function-spec 'breakon
		      ;; Must cons the (OR) afresh -- it gets RPLAC'd.
		      `(breakon-this-time ,(list 'or)
					  ,si:encapsulated-function
					  arglist))
      (push function-spec breakon-functions))))

(defun breakon-this-time (break-condition function args)
  (and break-condition (setf (ldb %%m-flags-trap-on-call %mode-flags) 1))
  ;; The next call ought to be the function the user is trying to call.
  ;; That will be so only if this function is compiled.
  (apply function args))

;;;; Initialization
(defvar command-dispatch-table :unbound
  "16. x 256. array that holds debugger command definitions.
First index is the bucky bits of the character, second index is the character code.")

(defun assure-dispatch-set-up ()
  (when (not (boundp 'command-dispatch-table))
    (setq command-dispatch-table (make-array (list char-bits-limit char-code-limit)))
    (dolist (x command-dispatch-list)
      (let ((char (car x))
	    (com (cadr x))
	    (repeat (caddr x)))
	(let ((i (char-bits char)) (j (char-code char)))
	  (dotimes (n (or repeat 1))
	    (setf (aref command-dispatch-table i j) com)
	    (incf j)))))))

;; Make sure COMMAND-DISPATCH-TABLE is recomputed
;; from the new value of COMMAND-DISPATCH-LIST if this file is reloaded.
(makunbound 'command-dispatch-table)

;; The initial dispatch table.
(defconst command-dispatch-list '(
       (#/? com-help)
       (#/help com-help)
       (#/line com-down-stack)
       (#/form com-clear-and-show)
       (#/return com-up-stack)
       (#/resume com-proceed)
       (#/abort com-abort)
       (#/rubout com-flush-numeric-arg)

       (#/c-0 com-number 10.)		;control-digits
       (#/c-A com-arglist)
       (#/c-B com-short-backtrace)
       (#/c-C com-proceed)
       (#/c-D com-proceed-trap-on-call)
       (#/c-E com-edit-frame-function)
       (#/c-F com-show-foothold)
       (#/c-L com-clear-and-show)
       (#/c-M com-bug-report)
       (#/c-N com-down-stack)
       (#/c-P com-up-stack)
       (#/c-R com-return-a-value)
       (#/c-S com-search)
       (#/c-T com-throw)
       (#/c-X com-toggle-frame-trap-on-exit)
       (#/c-Z com-top-level-throw)

       (#/m-0 com-number 10.)		;meta-digits
       (#/m-< com-top-stack)
       (#/m-> com-bottom-stack)
       (#/m-B full-backtrace)
       (#/m-D com-toggle-trap-on-call)
       (#/m-L com-clear-and-show-all)
       (#/m-N com-down-stack-all)
       (#/m-P com-up-stack-all)
       (#/m-R com-reinvoke-new-args)
       (#/m-S com-print-variable-frame-value)
;      (#/m-T com-show-frame-temporaries)
       (#/m-Y com-list-frame-temporaries)
       (#/m-X com-set-all-frames-trap-on-exit)

       (#/c-m-0 com-number 10.)		;control-meta-digits
       (#/c-m-A com-get-arg)
       (#/c-m-B full-backtrace-uninteresting)
       (#/c-m-F com-get-function)
       (#/c-m-H com-print-frame-handlers)
       (#/c-m-L com-get-local)
       (#/c-m-N com-down-stack-uninteresting)
       (#/c-m-P com-up-stack-uninteresting)
       (#/c-m-Q com-describe-proceed-types)
       (#/c-m-R com-return-reinvocation)
       (#/c-m-S com-print-frame-bindings)
       (#/c-m-U com-up-to-interesting)
       (#/c-m-V com-get-value)
       (#/c-m-W com-window-error-handler)
       (#/c-m-X com-clear-all-frames-trap-on-exit)

       (#/s-A com-proceed-specified-type 26.)
       )
  "List of elements (character command-symbol) from which EH:COMMAND-DISPATCH-TABLE is initialized.")

