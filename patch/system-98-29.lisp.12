;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11-Jan-84 01:00:22 by Mly,
;;; Reason: FORMAT ~c hacks mouse blips. FQUERY :tyi :help-function bug.
;;; EH c-m-a and c-m-l don't blow out on non-existent args/locals.
;;; New eh <help> command, including key documentation.
;;; Document all eh commands.
;;; Make c-m-a c-m-l c-m-v c-m-s set + to a locative as advertised.
;;; Repair mly brain-damage and really truly honestly fix READLINE eof.
;;; Make too-few-arguments errors on functions with optional args
;;;   proceed correctly when given additional args
;;; while running on Lisp Machine One from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.28, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306.


; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SG-FRAME-ARG-VALUE (SG FRAME ARGNUM &OPTIONAL (ERRORP T))
  "Return the value and location of arg number ARGNUM in FRAME in SG.
Checks ARGNUM for being in bounds, if the frame is active
/(for an open frame, it is not known how many args there are).
The second value is where the value is located when SG is running;
this may be a symbol value cell, etc., if the arg is special.
ERRORP non-NIL means signal an error if ARGNUM is invalid.
ERRORP NIL means retrun a third value which describes the problem, if any."
  (DECLARE (RETURN-LIST VALUE LOCATION BARF))
  (PROG* FUNCTION ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
		   (NUM-ARGS (SG-NUMBER-OF-SPREAD-ARGS SG FRAME))
		   ARG-NAME
		   (REST-ARG-P
		     (AND (LEGITIMATE-FUNCTION-P FUNCTION)
			  (LDB-TEST 2402 (ARGS-INFO FUNCTION)))))
	 (CHECK-ARG ARGNUM SYMBOL-OR-STRING-OR-NUMBERP "a symbol, string or number")
	 (IF (SYMBOLP ARGNUM)
	     (OR (DOTIMES (I NUM-ARGS)
		   (IF (STRING-EQUAL (STRING (ARG-NAME FUNCTION I)) (STRING ARGNUM))
		       (RETURN (SETQ ARGNUM I))))
		 ;; If this function takes a rest arg and we have
		 ;; specified its name, handle it (it is local number 0).
		 (AND REST-ARG-P
		      (STRING-EQUAL (STRING (LOCAL-NAME FUNCTION 0)) (STRING ARGNUM))
		      (RETURN-FROM FUNCTION
			(COND ((CONSP FUNCTION)
			       (VALUES (SG-REST-ARG-VALUE SG FRAME) T))
			      (T (SG-FRAME-LOCAL-VALUE SG FRAME 0)))))))
	 (IF (SYMBOLP ARGNUM)
	     (LET ((STRING (FORMAT NIL "No arg named ~S" ARGNUM)))
	       (IF ERRORP (FERROR NIL STRING) (RETURN NIL NIL STRING))))
	 (SETQ ARG-NAME (ARG-NAME FUNCTION ARGNUM))
	 (if (AND ( ARGNUM NUM-ARGS) (SG-FRAME-ACTIVE-P SG FRAME))
	     (LET ((LOC (NTHCDR (- ARGNUM NUM-ARGS)
				(AND REST-ARG-P (SG-REST-ARG-VALUE SG FRAME)))))
	       (IF LOC (RETURN (CAR LOC) (LOCF (CAR LOC)))
		 (LET ((STRING (FORMAT NIL "Argument number ~D is out of range in current frame" ARGNUM)))
		   (IF ERRORP (FERROR NIL STRING) (RETURN NIL NIL STRING))))))
	 ;; Is this variable bound special in THIS frame?
	 (MULTIPLE-VALUE-BIND (START END)
	     (SG-FRAME-SPECIAL-PDL-RANGE SG FRAME)
	   (COND (START
		  (DO ((SP (SG-SPECIAL-PDL SG))
		       (I START (+ 2 I)))
		      (( I END))
		    (AND (EQ (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I)))
			     ARG-NAME)
			 ;; Yes, it is, so return its special binding
			 ;; and that binding's location when the SG is running.
			 (RETURN-FROM FUNCTION
			   (MULTIPLE-VALUE-BIND (VALUE NIL LOCATION)
			       (SYMEVAL-IN-STACK-GROUP ARG-NAME SG FRAME T)
			     (VALUES VALUE LOCATION))))))))
	 (RETURN (AREF (SG-REGULAR-PDL SG) (+ FRAME ARGNUM 1))
		 (ALOC (SG-REGULAR-PDL SG) (+ FRAME ARGNUM 1)))))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SG-FRAME-LOCAL-VALUE (SG FRAME LOCALNUM &OPTIONAL (ERRORP T))
  "Return the value and location of local variable number LOCALNUM in FRAME in SG.
Checks LOCALNUM for being in bounds.
The second value is where the value is located when SG is running;
this may be a symbol value cell, etc., if the arg is special.
ERRORP non-NIL means signal an error if ARGNUM is invalid.
ERRORP NIL means retrun a third value which describes the problem, if any."
  (DECLARE (RETURN-LIST VALUE LOCATION BARF))
  (PROG* FUNCTION ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
		   (NUM-LOCALS (SG-NUMBER-OF-LOCALS SG FRAME))
		   LOCAL-NAME)
	 (CHECK-ARG LOCALNUM SYMBOL-OR-STRING-OR-NUMBERP "a symbol, string or number")
	 (IF (SYMBOLP LOCALNUM)
	     (DOTIMES (I NUM-LOCALS)
	       (IF (STRING-EQUAL (STRING (LOCAL-NAME FUNCTION I)) (STRING LOCALNUM))
		   (RETURN (SETQ LOCALNUM I)))))
	 (IF (SYMBOLP LOCALNUM)
	     (LET ((STRING (FORMAT NIL "No local named ~S" LOCALNUM)))
	       (IF ERRORP (FERROR NIL STRING) (RETURN NIL NIL STRING))))
	 (IF ( LOCALNUM NUM-LOCALS)
	     (LET ((STRING (FORMAT NIL "Local number ~D is out of range in current frame"
				   LOCALNUM)))
	       (IF ERRORP (FERROR NIL STRING) (RETURN NIL NIL STRING))))
	 (SETQ LOCAL-NAME (LOCAL-NAME FUNCTION LOCALNUM))
	 ;; Is this variable bound special in THIS frame?
	 (MULTIPLE-VALUE-BIND (START END)
	     (SG-FRAME-SPECIAL-PDL-RANGE SG FRAME)
	   (COND (START
		  (DO ((SP (SG-SPECIAL-PDL SG))
		       (I START (+ 2 I)))
		      (( I END))
		    (AND (EQ (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I)))
			     LOCAL-NAME)
			 ;; Yes, it is, so return its special binding
			 ;; and that binding's location when the SG is running.
			 (RETURN-FROM FUNCTION
			   (MULTIPLE-VALUE-BIND (VALUE NIL LOCATION)
			       (SYMEVAL-IN-STACK-GROUP LOCAL-NAME SG FRAME T)
			     (VALUES VALUE LOCATION))))))))
	 (LET* ((RP (SG-REGULAR-PDL SG))
		(RPIDX (+ LOCALNUM FRAME (RP-LOCAL-BLOCK-ORIGIN RP FRAME))))
	   (RETURN (AREF RP RPIDX) (ALOC RP RPIDX)))))

))

; From file FORMAT.LISP SRC:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS)
    (AND (CONSP ARG) (EQ (CAR ARG) ':MOUSE-BUTTON) (SETQ ARG (SECOND ARG)))
    (SETQ ARG (CHARACTER ARG))
    (COND ((TV:CHAR-MOUSE-P ARG)
	   (COND ((AND (NOT COLON-FLAG) ATSIGN-FLAG)
		  (OR (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		      (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
		  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "#\")
		  (PRIN1 CHNAME))
		 (T (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
		    (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
		    (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
		    (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
		    (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Mouse-")
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT (NTH (LDB 0003 ARG)
							       '("Left" "Middle" "Right")))
		    (IF (SETQ CHNAME (NTH (SETQ BITS (LDB 0303 ARG))
					  '("" "-Twice" "-Thrice")))
			(FUNCALL STANDARD-OUTPUT ':STRING-OUT CHNAME)
		      (FUNCALL STANDARD-OUTPUT ':TYO #/-)
		      (ENGLISH-PRINT (1+ BITS))
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT "-times")))))
          ((NOT COLON-FLAG)
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   ;; If @ flag or if control bits, we want to use characters' names.
	   (IF (OR ATSIGN-FLAG (NOT (ZEROP BITS)))
	       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (LDB %%KBD-CHAR ARG))))
	   ;; Print an appropriate reader macro if @C.
	   (IF ATSIGN-FLAG (PRINC (IF (OR (NOT (ZEROP BITS)) CHNAME) "#\" "#//")))
	   (IF (NOT (ZEROP BITS))
	       ;; For efficiency, don't send :string-out message just for null string.
	       (FUNCALL STANDARD-OUTPUT
			':STRING-OUT
			(NTH BITS
			     '("" "c-" "m-" "c-m-"
			       "s-" "c-s-" "m-s-" "c-m-s-"
			       "h-" "c-h-" "m-h-" "c-m-h-"
			       "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
	   (IF CHNAME
	       (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		 (LET ((STR (STRING-DOWNCASE CHNAME)))
		   (ASET (CHAR-UPCASE (AREF STR 0)) STR 0)
		   (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
		   (RETURN-ARRAY STR)))
	     (AND ATSIGN-FLAG
		  (NOT (ZEROP BITS))
		  (IF ( #/a (LDB %%KBD-CHAR ARG) #/z)
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT "sh-")
		    (IF (MEMQ (LDB %%KBD-CHAR ARG)
			      '(#/, #\SPACE #/( #/) #/' #// #/` #/@ #/; #/: #/" #/| #/#))
			(TYO (SI:PTTBL-SLASH READTABLE))
		      (SETQ ARG (CHAR-DOWNCASE ARG)))))
	     (FUNCALL STANDARD-OUTPUT ':TYO (LDB %%KBD-CHAR ARG))))
	  (T
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
	   (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
	   (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
	   (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
	   (SETQ ARG (LDB %%KBD-CHAR ARG))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		    (LET ((STR (STRING-DOWNCASE CHNAME)))
		      (ASET (CHAR-UPCASE (AREF STR 0)) STR 0)
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
		      (RETURN-ARRAY STR)))
		  (AND ATSIGN-FLAG (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND ATSIGN-FLAG (< ARG 40) ( ARG #/))
		  (FUNCALL STANDARD-OUTPUT ':TYO ARG)
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
		 ((AND ( #/a ARG #/z)
		       (NOT (ZEROP BITS)))
		  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Shift-")
		  (FUNCALL STANDARD-OUTPUT ':TYO (CHAR-UPCASE ARG)))
                 (T (FUNCALL STANDARD-OUTPUT ':TYO ARG))))))

))

; From file FQUERY.LISP PS:<L.IO1> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FQUERY  "

(DEFSELECT TYI-FQUERY-FUNCTION
  (:READ (STREAM)
    (DO ((CH)) (NIL)
      (FQUERY-PROMPT STREAM)
      (SETQ CH (FUNCALL STREAM ':TYI))
      (UNLESS (AND (= CH #\HELP) FQUERY-HELP-FUNCTION)
	(RETURN CH))
      (FUNCALL FQUERY-HELP-FUNCTION STREAM FQUERY-CHOICES #'TYI-FQUERY-FUNCTION)
      (FUNCALL STREAM ':FRESH-LINE)))
  (:ECHO (ECHO STREAM)
    (FUNCALL STREAM ':STRING-OUT (STRING ECHO)))
  (:MEMBER (CHAR LIST)
    (MEM #'CHAR-EQUAL CHAR LIST)))

))

eh:(setf (documentation 'com-window-error-handler 'function)
	 "Use a window-oriented error handler to debug the stack.")

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defconst com-help-alist '(((#\G "General"
			     "General information about using the debugger") #\G #\g)
			   ((#\I "Information"
			         "Various ways of obtaining information about the current stack frame")
				 #\I #\i #\E #\E)
			   ((#\F "Stack Frames" "Selecting Stack Frames to examine") #\F #\f)
			   ((#\S "Stepping" "Stepping though through the program") #\S #\s)
			   ((#\P "Proceeding"
			         "Proceeding from this error and resuming execution")
			         #\P #\p #\X #\x)
			   ((#\T "Transferring"
			         "Transferring to other systems: Edit, Bug report, Window-based Debugger")
				 #\T #\t)
			   ((#\D "Describe"
			     "Give the documentation of the function associated with a command key")
				 #\D #\d #\C #\c)
			   ((#\ABORT "Abort" t) #\ABORT #\C-Z #\C-G)
			   ((#\HELP "Help" t) #\HELP #\?))
  "FQUERY options for used in giving help for debugger commands.")

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defun com-help (sg error-object &rest ignore)
  "Help for using the debugger"
  (tagbody
   again
      (selectq (fquery `(:choices ,com-help-alist
				  :help-function nil)
		       "Help for debugger commands. Choose a topic: ")
	(#\G (format t "

You are in the debugger.  If you don't want to debug this error, type ~C.
Otherwise you can evaluate expressions in the context of the error, examine
the stack, and proceed//throw//return to recover.
  If you type in a Lisp form, it will be evaluated, and the results printed,
using ~A syntax and semantics and base ~D in package ~A. This
evaluation uses the variable environment of the stack frame you are examining.
  Type ~c or ~c to get back to top level, or the previous debugger level.
While in the debugger, ~c quits back to the debugger top level.
  If you think this error indicates a bug in the Lisp machine system, use the
~:c command."
		     #\ABORT (if *default-common-lisp* "Common Lisp" "traditional")
		     ibase (package-name package)
		     #\ABORT #\C-Z #\C-G #\C-M))
	(#\I (format t "

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
" #\C-L "Clear Screen" #\M-L #\C-B #\C-M-B #\M-B #\C-B
  #\C-M-A #\C-M-A #\C-M-L #\C-M-A #\C-M-V #\C-M-A #\C-M-F #\C-A #\M-S #\C-M-S))
	(#\F (format t "

~c or ~\lozenged-char\ goes down a frame, ~c or ~\lozenged-char\ goes up.
~c and ~c are similar but show args, locals and compiled code.
~c and ~c are similar to ~c and ~c, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
~c and ~c go to the top and bottom of the stack, respectively.
~c reads a string and searches down the stack for a frame
   calling a function whose name contains that substring.
" #\C-N #\LINE #\C-P #\RETURN #\M-N #\M-P #\C-M-N #\C-M-P #\C-N #\C-P #\M-< #\M-> #\C-S))
	(#\S (if error-handler-running
		 (format t "
~c toggles the trap-on-exit flag for the current frame.
~c sets the trap-on-exit flag for the current frame and all outer frames.
~c clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
~c proceeds like ~C, but first sets the trap-on-next-function-call flag.
~c toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with ~c.
" #\C-X #\M-X #\C-M-X #\C-D #\RESUME #\M-D #\C-X)
	       (format t "

You cannot use the stepping commands, since you are in examine-only mode.
~C exits the debugger." #\RESUME)))
	(#\P (cond (error-handler-running
		    (format t "

~c aborts to previous debugger or other command loop, or to top level.
~c returns a value or values from the current frame.
~c offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
~c offers to reinvoke the current frame, letting you alter
   some of the arguments, or use more or fewer arguments.
~c throws to a specific tag." #\C-Z #\C-R #\C-M-R #\M-R #\C-T)
		    (describe-proceed-types sg error-object))
		   (t
		    (format t "

You cannot continue execution, since you are in examine-only mode.
~C exits the debugger." #\RESUME))))
	(#\T (format t "

~c calls the editor to edit the current function.
~c enters the editor to send a bug message, and puts the error
  message and a backtrace into the message automatically.
  A numeric argument says how many stack frames to put in the backtrace.
~c switches to the window-based debugger.
" #\C-E #\C-M #\C-M-W))
	(#\D (com-help-describe-command error-object sg))
	(#\HELP
	 (format t "~&For additional help in using the debugger, type one of the following:~%")
	 (dolist (x com-help-alist)
	   (unless (eq (third (car x)) t)
	     (format t "~&  ~:C~6T~A" (cadr x) (or (third (car x)) (second (car x))))))
	 (go again))
	(#\ABORT))))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defun com-help-describe-command (error-object sg &aux char)
  "Prompts for a character and describes what the current meaning of that keystoke
is as a command to the error-handler."
  (format t "~&Describe Command. Type command character: ")
  (let ((reading-command t))			;let abort, etc through
    (setq char (send standard-input ':tyi)))
  (let* ((command (command-lookup char))
	 (resume-handlers (symeval-in-stack-group 'condition-resume-handlers sg))
	 (proceed-types (send error-object ':user-proceed-types
			      (sg-condition-proceed-types sg error-object)))
	 (keywords (append proceed-types special-commands))
	 tem)
    (format t "~:C~&~C: " char char)
    (cond ((setq tem (rassq char *proceed-type-special-keys*))
	   (send error-object ':document-proceed-type (car tem)
		 standard-output resume-handlers))
	  ((setq tem (rassq char *special-command-special-keys*))
	   (send error-object ':document-special-command (car tem)
		 standard-output resume-handlers))
	  ((null command)
	   (if (zerop (char-bits char))
	       (format t "is not a special debugger command.
  Typing this will enter a read-eval-print loop which you can use to examine information
in the enviroment of the stack frame you are examining, using ~A syntax and semantics
and base ~D in package ~A"
		       (if *default-common-lisp* "Common Lisp" "traditional")
		       ibase (package-name package))
	     (format t "is not currently a defined debugger command.")))
	  ((eq command 'com-proceed-specified-type)
	   (if (not (< -1 (- char #\s-A) (length keywords)))
	       (format t "is not currently a defined debugger command.")
	     (send error-object (if (< (- char #\s-A) (length proceed-types))
				    ':document-proceed-type
				  ':document-special-command)
		   (nth (- char #\s-A) keywords)
		   standard-output resume-handlers)))
	  (t (if (documentation command)
		 (format t "~A" (documentation command))
	       (format t "~S is not documented" command))
	     (send standard-output ':fresh-line)
	     (selectq command
	       (com-abort
		(let ((abort-handler (find-resume-handler abort-object nil resume-handlers)))
		  (cond ((dolist (x proceed-types)
			   (when (eq (find-resume-handler error-object x resume-handlers)
				     abort-handler)
			     (format t "~&This command is currently synonymous with ~C: "
				     (+ #\s-A (position x proceed-types)))
			     (send error-object ':document-proceed-type x
				   standard-output resume-handlers)
			     (return t))))
			(abort-handler
			 (send abort-object ':document-proceed-type (second abort-handler)
			       standard-output resume-handlers))
			(t (format t "There is no way to abort from this error.")))))
	       (com-proceed
		(if proceed-types
		    (progn (format t "~&This command is currently synonymous with ~C: " #\s-A)
			   (send error-object ':document-proceed-type (car proceed-types)
				 standard-output resume-handlers))
		  (format t "There is no way to proceed from this error."))))))))

))


eh:(setf (documentation 'com-down-stack 'function)
	 "Goes down to the next stack frame (outward)"
	 )
eh:(setf (documentation 'com-up-stack 'function)
	 "Goes up to the previous stack frame (inward)"
	 )
eh:(setf (documentation 'com-up-stack-all 'function)
  "Goes up to the previous stack frame (inward)
and shows the args, locals and compiled code."
	 )
eh:(setf (documentation 'com-down-stack-all 'function)
  "Goes down to the next stack frame (outward)
and shows the args, locals and compiled code."
	 )
eh:(setf (documentation 'com-top-stack 'function)
  "Goes to the top of the stack (innermost)"
	 )
eh:(setf (documentation 'com-bottom-stack 'function)
  "Goes to the bottom of the stack (outermost)"
	 )
eh:(setf (documentation 'com-up-stack-uninteresting 'function)
  "Goes up to the previous stack frame (inwards)
Includes internal /"uninteresting/" frames, such as EVAL and PROG, etc."
	 )
eh:(setf (documentation 'com-down-stack-uninteresting 'function)
  "Goes down to the next stack frame (outwards)
Includes internal /"uninteresting/" frames, such as EVAL and PROG, etc."
	 )
eh:(setf (documentation 'com-up-to-uninteresting 'function)
  "Goes up the the previous /"interesting/" stack frame (inwards)"
	 )
eh:(setf (documentation 'com-show-short-backtrace 'function)
  "Prints a brief (function names only) backtrace of the stack.
Optional numeric arg determines how far back to go."
         )
eh:(setf (documentation 'full-backtrace 'function)
  "Gives a full (includes args) backtrace. Optional numeric arg determines how far back to go."
	 )
eh:(setf (documentation 'full-backtrace-uninteresting 'function)
  "Gives an exhaustive (includes args and /"uninteresting/" frames) backtrace.
Optional numeric arg determines how far back to go."
	 )
eh:(setf (documentation 'com-clear-and-show 'function)
  "Clears the screen and redisplays the error message."
	 )
eh:(setf (documentation 'com-describe-proceed-types 'function)
  "Describes the possible proceed types."
	 )
eh:(setf (documentation 'com-clear-and-show-all 'function)
  "Clears the screen and redisplays the error message, args, locals and compiled code."
	 )
eh:(setf (documentation 'com-search 'function)
  "Prompts for a string and searches down the stack for a frame containing a call to a
function whose name contains that string."
	 )
eh:(setf (documentation 'com-search-and-show-all 'function)
  "Prompts for a string and searches down the stack for a frame containing a call to a
function whose name contains that string, and then displays args, locals and compiled code."
	 )
eh:(setf (documentation 'com-bug-report 'function)
  "Mails a bug report containing a backtrace of the stack.
You are prompted for a message. The depth of stack backtrace included is determined
by the optional numeric argument."
	 )

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-PRINT-FRAME-BINDINGS (SG ERROR-OBJECT &OPTIONAL COUNT
				 &AUX START END (SP (SG-SPECIAL-PDL SG))
				 SELF-INCLUDED SPECIAL-VARS)
  "Lists the names and values of all special variables bound by this frame."
  ERROR-OBJECT
  ;; Find range of special pdl for this frame together with
  ;; all uninteresting frames called by it.
  (DO ((PREVIOUS-INT (SG-PREVIOUS-INTERESTING-ACTIVE SG CURRENT-FRAME
						     INNERMOST-VISIBLE-FRAME))
       (PREVIOUS CURRENT-FRAME (SG-PREVIOUS-ACTIVE SG PREVIOUS INNERMOST-VISIBLE-FRAME)))
      ((EQUAL PREVIOUS PREVIOUS-INT))
    (MULTIPLE-VALUE-BIND (MAYBE-START MAYBE-END)
	(SG-FRAME-SPECIAL-PDL-RANGE SG PREVIOUS)
      (SETQ START MAYBE-START)
      (AND MAYBE-END (SETQ END (1+ MAYBE-END)))))
  (COND (START
	 (IF (NULL COUNT) (FORMAT T "~&Names and values of specials bound in this frame:~%"))
	 ;; Now look through the specials bound in this frame.
	 ;; Maybe print one or all, but in any case compute SELF-INCLUDED
	 ;; and SPECIAL-VARS.
	 (DO ((I START (+ I 2)))
	     (( I END))
	   (PUSH (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I)))
		 SPECIAL-VARS)
	   (IF (EQ (CAR SPECIAL-VARS) 'SELF)
	       (SETQ SELF-INCLUDED I))
	   (WHEN (OR (NULL COUNT) (= COUNT (LSH (- I START) -1)))
	     (FORMAT T "~:[~% ~S: ~;~&Value of ~S in this frame:~&   ~]"
		     COUNT (CAR SPECIAL-VARS))
	     (MULTIPLE-VALUE-BIND (VAL ERROR)	;Value
		 (CATCH-ERROR (FUNCALL (OR PRIN1 #'PRIN1) (AREF SP I)) NIL)
	       (IF ERROR (SETQ VAL (PRINC "unbound")))
	       (WHEN COUNT
		 (SETQ ////// //// //// // // (LIST VAL))
		 (SETQ *** ** ** * * VAL)
		 (SETQ +++ ++ ++ + + - - (VALUE-CELL-LOCATION (CAR SPECIAL-VARS)))))))
	 ;; List the instance variable of SELF also if SELF is bound in this frame.
	 (IF (AND SELF-INCLUDED
		  (TYPEP (AREF SP SELF-INCLUDED) ':INSTANCE)
		  ;; But not if want only one variable and already got it.
		  (OR (NULL COUNT)
		      ( COUNT (LSH (- END START) -1))))
	     (LET* ((SELF-VALUE (AREF SP SELF-INCLUDED))
		    (SELF-FLAVOR 
		     (SI:INSTANCE-FLAVOR SELF-VALUE))
		    (SELF-VARS (SI:FLAVOR-ALL-INSTANCE-VARIABLES-SLOW SELF-FLAVOR)))
	       (UNLESS COUNT
		 (FORMAT T "~2&Non-special instance variables of ~S~&" SELF-VALUE))
	       (DO ((SV SELF-VARS (CDR SV))
		    (COUNT-UNSPECIAL (LSH (- END START) -1))
		    (I 1 (1+ I)))
		   ((NULL SV))
		 (WHEN (AND (NOT (MEMQ (CAR SV) SPECIAL-VARS))
			    (OR (NULL COUNT)
				(= (1+ COUNT) (INCF COUNT-UNSPECIAL))))
		   (FORMAT T "~:[~% ~S: ~;~&Value of instance variable ~S within SELF:~&   ~]"
			   COUNT (CAR SV))
		   (MULTIPLE-VALUE-BIND (VAL ERROR)
		       (CATCH-ERROR (FUNCALL (OR PRIN1 #'PRIN1)
					     (%INSTANCE-REF SELF-VALUE I)) NIL)
		     (IF ERROR (SETQ VAL (PRINC "unbound")))
		     (COND (COUNT
			    (SETQ ////// //// //// // // (LIST VAL))
			    (SETQ *** ** ** * * VAL)
			    (SETQ +++ ++ ++ + + - - (%INSTANCE-LOC SELF-VALUE I))))))))))
	(T (FORMAT T "~&No specials bound in this frame"))))

))


; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-PRINT-VARIABLE-FRAME-VALUE (SG ERROR-OBJECT)
  "Print the value of a special variable within the context of the current stack frame.
Prompts for the variable name."
  ERROR-OBJECT
  (TERPRI)
  (PRINC "Value in this frame of special variable ")
  (LET ((VAR (SI:READ-FOR-TOP-LEVEL))
	SELF-VALUE SELF-POS)
    (MULTIPLE-VALUE-BIND (VALUE BOUNDFLAG)
	(SYMEVAL-IN-STACK-GROUP VAR SG CURRENT-FRAME)
      (TERPRI)
      (SETQ ////// //// //// //)
      (SETQ *** ** ** *)
      (SETQ +++ ++ ++ + + - - VAR)
      (COND (BOUNDFLAG (FUNCALL (OR PRIN1 #'PRIN1) (SETQ * VALUE)))
	    ((PROGN
	      (SETQ SELF-VALUE
		    (SYMEVAL-IN-STACK-GROUP 'SELF SG CURRENT-FRAME))
	      (AND (TYPEP SELF-VALUE ':INSTANCE)
		   (SETQ SELF-POS
			 (FIND-POSITION-IN-LIST
			   VAR
			   (SI:FLAVOR-ALL-INSTANCE-VARIABLES-SLOW
			     (SI:INSTANCE-FLAVOR SELF-VALUE))))))
	     (FUNCALL (OR PRIN1 #'PRIN1) (SETQ * (%INSTANCE-REF SELF-VALUE (1+ SELF-POS)))))
	    (T (PRINC (SETQ * "Unbound"))))
      (SETQ // (LIST *)))))

))

eh:(setf (documentation 'com-print-frame-handlers 'function)
  "Lists the names and values of all special variables bound by this frame."
	 )
eh:(setf (documentation 'com-arglist 'function)
  "Returns the arglist for the current stack frame function."
	 )


; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-GET-ARG (SG IGNORE &OPTIONAL (ARG 0))
  "Sets * to the nth arg to the current function, where n is a numeric argument (default 0)
Also sets + to a locative to the arg."
  (TERPRI)
  (MULTIPLE-VALUE-BIND (A B BARF)
     (SG-FRAME-ARG-VALUE SG CURRENT-FRAME ARG NIL)
    (IF BARF (PRINC BARF)
      (SETQ ////// //// //// // // (LIST A))
      (SETQ *** ** ** * * A)
      (SETQ +++ ++ ++ + + - - B)
      (FUNCALL (OR PRIN1 #'PRIN1) *))))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-GET-LOCAL (SG IGNORE &OPTIONAL (ARG 0))
  "Sets * to the nth local of he current function, where n is a numeric argument
(default 0) Also sets + to a locative to the local."
  (TERPRI)
  (MULTIPLE-VALUE-BIND (A B BARF)
      (SG-FRAME-LOCAL-VALUE SG CURRENT-FRAME ARG NIL)
    (IF BARF (PRINC BARF)
      (SETQ ////// //// //// // // (LIST A))
      (SETQ *** ** ** * * A)
      (SETQ +++ ++ ++ + + - - B)
      (FUNCALL (OR PRIN1 #'PRIN1) *))))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-GET-VALUE (SG IGNORE &OPTIONAL (ARG 0))
  "Sets * to the nth value being returned by the current function, where n is a numeric
argument (default 0) Also sets + to a locative to the value."
  (TERPRI)
  (MULTIPLE-VALUE-BIND (A B BARF)
      (SG-FRAME-VALUE-VALUE SG CURRENT-FRAME ARG NIL)
    (IF BARF (PRINC BARF)
      (SETQ ////// //// //// // // (LIST A))
      (SETQ *** ** ** * * A)
      (SETQ +++ ++ ++ + + - - B)
      (FUNCALL (OR PRIN1 #'PRIN1) *))))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-GET-FUNCTION (SG IGNORE &OPTIONAL IGNORE)
  "Sets * to the current function, and + to a locative to it."
  (SETQ ////// //// //// //)
  (SETQ *** ** ** *)
  (SETQ +++ ++ ++ + + -)
  (SETQ - (LOCF (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME))
	* (CONTENTS -)
	// (LIST *))
  (TERPRI T)
  (FUNCALL (OR PRIN1 #'PRIN1) *))

))

eh:(setf (documentation 'com-edit-frame-function 'function)
  "Edit the source code for the current function in Zmacs."
	 )
eh:(setf (documentation 'com-abort 'function)
  "Aborts out of this error if possible."
	 )
eh:(setf (documentation 'com-flush-numeric-arg 'function)
  "Flushes the numeric arg, if any."
	 )
eh:(setf (documentation 'com-top-level-throw 'function)
  "Throws to the top level in the current process."
	 )
eh:(setf (documentation 'com-throw 'function)
  "Throw to a tag which is prompted for."
	 )
eh:(setf (documentation 'com-reinvoke-new-args 'function)
  "Reinvoke the current function with possibly altered arguments."
	 )
eh:(setf (documentation 'com-return-a-value 'function)
  "Specify values to return from the current frame (does not reinvoke the function)"
	 )
eh:(setf (documentation 'com-return-reinvocation 'function)
  "Retries invoking the current function."
	 )
eh:(setf (documentation 'com-proceed 'function)
  "Proceeds from this error if possible."
	 )
eh:(setf (documentation 'com-proceed-specified-type 'function)
  "Use a user-specified proceed option for this error."
	 )
eh:(setf (documentation 'com-toggle-frame-trap-on-exit 'function)
  "Toggles whether we trap on exit from this frame."
	 )
eh:(setf (documentation 'com-set-all-frames-trap-on-exit 'function)
  "Makes all outer frames trap on exit."
	 )
eh:(setf (documentation 'com-clear-all-frames-trap-on-exit 'function)
  "Clears the trap-on-exit flag for all outer frames."
	 )
eh:(setf (documentation 'com-proceed-trap-on-call 'function)
  "Proceeds from this error (if that is possible) and traps on the next function call."
	 )
eh:(setf (documentation 'com-toggle-trap-on-call 'function)
  "Toggle whether to trap on next function call."
	 )
eh:(setf (documentation 'com-number 'function)
      "Used to give a numeric argument to debugger commands."
         )

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFVAR COMMAND-DISPATCH-TABLE :UNBOUND
  "16. x 256. array that holds debugger command definitions.
First index is the bucky bits of the character, second index is the low 8 bits.")

(DEFUN ASSURE-DISPATCH-SET-UP ()
  (COND ((NOT (BOUNDP 'COMMAND-DISPATCH-TABLE))
	 (SETQ COMMAND-DISPATCH-TABLE (MAKE-ARRAY '(20 400)))
	 (DOLIST (X COMMAND-DISPATCH-LIST)
	   (LET ((CHAR (CAR X))
		 (COM (CADR X))
		 (REPEAT (CADDR X)))
	     (LET ((I (CHAR-BITS CHAR)) (J (CHAR-CODE CHAR)))
	       (DOTIMES (N (OR REPEAT 1))
		 (ASET COM COMMAND-DISPATCH-TABLE I J)
		 (SETQ J (1+ J)))))))))

))

eh:(makunbound 'command-dispatch-table)
eh:(assure-dispatch-set-up)

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READLINE (&REST READ-ARGS)
  "Read a line from STREAM and return it as a string.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to :RUBOUT-HANDLER if it is used.

The second value is EOF-OPTION if we exit due to end of file."
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS)
	   (VALUES STRING-OR-EOF-OPTION EOF-FLAG))
  (LET ((OPTIONS NIL))
    ;; This kludge is to let us take a third, optional argument.
    (COND ((> (LENGTH READ-ARGS) 2)
	   (SETQ OPTIONS (THIRD READ-ARGS))
	   (SETQ READ-ARGS (LIST (FIRST READ-ARGS) (SECOND READ-ARGS)))))
    (MULTIPLE-VALUE-BIND (STREAM EOF-OPTION)
	(DECODE-READ-ARGS READ-ARGS)
      (MULTIPLE-VALUE-BIND (STRING EOF)
	  (READ-DELIMITED-STRING '(#\RETURN #\END) STREAM
				 (EQ EOF-OPTION 'NO-EOF-OPTION) OPTIONS)
	(VALUES STRING (IF EOF EOF-OPTION))))))

))

; From file EH.LISP.313 PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN EH-ARG (&OPTIONAL (NAME-OR-NUMBER 0) (ERRORP T))
  "Return the value of specified arg in current frame.
Specify either a number (origin 0) or a symbol (package does not matter)."
  (IF NAME-OR-NUMBER
      (MULTIPLE-VALUE-BIND (VAL1 NIL BARF)
	  (SG-FRAME-ARG-VALUE EH-SG EH-FRAME NAME-OR-NUMBER ERRORP)
	(IF BARF (FORMAT T "~&~A" BARF VAL1)))
    (MULTIPLE-VALUE-BIND (VAL NIL)
	(SG-LISTIFY-ARGS-AND-LOCALS EH-SG EH-FRAME)
      VAL)))

))

; From file EH.LISP.313 PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN EH-LOC (&OPTIONAL (NAME-OR-NUMBER 0) (ERRORP T))
  "Return the value of specified local variable in current frame.
Specify either a number (origin 0) or a symbol (package does not matter)."
  (IF NAME-OR-NUMBER
      (MULTIPLE-VALUE-BIND (VAL1 NIL BARF)
	  (SG-FRAME-LOCAL-VALUE EH-SG EH-FRAME NAME-OR-NUMBER ERRORP)
	(IF BARF (FORMAT T "~&~A" BARF) VAL1))
    (MULTIPLE-VALUE-BIND (NIL VAL)
	(SG-LISTIFY-ARGS-AND-LOCALS EH-SG EH-FRAME)
      VAL)))

))

; From file EH.LISP.313 PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN EH-VAL (&OPTIONAL (NUMBER 0))
  "Return the value of specified value being returned by current frame.
NUMBER is which value to return (origin 0)"
  (MULTIPLE-VALUE-BIND (VAL1 NIL BARF)
      (SG-FRAME-VALUE-VALUE EH-SG EH-FRAME NUMBER NIL)
    (IF BARF (FORMAT T "~&~A" BARF) VAL1)))

))

; From file EHF.LISP.190 PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN (FUNCTION-ENTRY MAKE-UCODE-ERROR-FUNCTION) (IGNORE SG IGNORE)
  (MAKE-INSTANCE 'FUNCTION-ENTRY-ERROR
		 ':FUNCTION (AREF (SG-REGULAR-PDL SG) (SG-AP SG))
		 ':ARGUMENT-LIST (CDR (GET-FRAME-FUNCTION-AND-ARGS SG (SG-AP SG)))
		 ':NARGS (RP-NUMBER-ARGS-SUPPLIED (SG-REGULAR-PDL SG) (SG-AP SG))
		 ':CONDITION-NAMES (LIST (FUNCTION-ENTRY-ERROR SG))))

))

; From file EH.LISP.313 PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN GET-FRAME-FUNCTION-AND-ARGS (SG FRAME &AUX FUNCTION NARGS-SUPPLIED
				    (RP (SG-REGULAR-PDL SG))
				    LEXPR-CALL REST-ARG-EXPECTED REST-ARG-VALUE ANS)
  "Return a list describing what was being executed in frame FRAME in SG.
The car of the list is a function name.
The cdr is a list of arguments (but if the arguments have
been modified, we can only get the latest values)."
      (SETQ FUNCTION (RP-FUNCTION-WORD RP FRAME)
	    nargs-supplied (rp-number-args-supplied (sg-regular-pdl sg) frame))
      ;(setq NARGS-SUPPLIED (SG-NUMBER-OF-SPREAD-ARGS SG FRAME))
      (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-EXPECTED LEXPR-CALL)
	(SG-REST-ARG-VALUE SG FRAME))
      ;; Analyze the function
      (SETQ FUNCTION (FUNCTION-NAME FUNCTION))
      (OR (SG-FRAME-ACTIVE-P SG FRAME) (BARF "This is not an active frame."))
      ;; Get the spread args.
      (DO ((I NARGS-SUPPLIED (1- I)))	;Cons them up in reverse order
	  ((ZEROP I))
	(SETQ ANS (CONS (AREF RP (+ FRAME I)) ANS)))   ;+1 -1
      ;; NCONC the rest arg if any was supplied separately from the regular args
      (AND REST-ARG-EXPECTED
	   (SETQ ANS (NCONC ANS (COPYLIST REST-ARG-VALUE))))
      (CONS FUNCTION ANS))

))
