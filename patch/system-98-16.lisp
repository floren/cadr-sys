;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.16
;;; Reason: *VALUES*.  *DEFAULT-COMMON-LISP*.  Bind *, +, etc. per process.
;;; Allow refs to all vars free at top level in listen loops.
;;; :READ/INCREMENT-CURSORPOS in characters rounds up.
;;; Written 12/26/83 02:15:39 by RMS,
;;; while running on Lisp Machine Two from band 6
;;; with Bad Inconsistently updated System 98.12, CADR 3.2, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 305, ZM MIT.



(globalize "*DEFAULT-COMMON-LISP*")
(globalize "*VALUES*")
(load "sys:io;crdtbl")
(setq si:initial-common-lisp-readtable (copy-readtable si:common-lisp-readtable))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFVAR *DEFAULT-COMMON-LISP* NIL
  "T means Lisp listen loops use Common Lisp syntax and semantics by default.")

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFVAR *BREAK-BINDINGS*
	'((RUBOUT-HANDLER NIL)			;Start new level of rubout catch
	  (READ-PRESERVE-DELIMITERS NIL)	;For normal Lisp syntax
	  (READ-CHECK-INDENTATION NIL)
	  (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)	;as opposed to compiler temp area
	  ;Next line commented out since it causes more trouble in than out
	  ;(IBASE 8) (BASE 8)
	  (OLD-STANDARD-INPUT STANDARD-INPUT)	;So user can find old stream.  BREAK, too!
	  (OLD-QUERY-IO QUERY-IO)		;..
	  (STANDARD-INPUT SYN-TERMINAL-IO)	;Rebind streams to terminal
	  (STANDARD-OUTPUT SYN-TERMINAL-IO)
	  (QUERY-IO SYN-TERMINAL-IO)
	  (EH:ERRSET-STATUS NIL)		;"Condition Wall" for errsets
	  (EH:CONDITION-HANDLERS NIL)		; and for conditions
	  (EH:CONDITION-DEFAULT-HANDLERS NIL)
	  (LOCAL-DECLARATIONS NIL)
	  (SELF-FLAVOR-DECLARATION NIL)
	  (READTABLE (IF *DEFAULT-COMMON-LISP*
			 COMMON-LISP-READTABLE STANDARD-READTABLE))
	  (*READER-SYMBOL-SUBSTITUTIONS*
	    (IF *DEFAULT-COMMON-LISP*
		SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*))
	  ;Changed 3/3/80 by Moon not to bind *, +, and -.
	  )
  "Bindings to be made by the function BREAK.
Each element is a list (VARNAME VALUE-FORM) describing one binding.
Bindings are made sequentially.")

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN VALIDATE-BASE (B)
  (IF *DEFAULT-COMMON-LISP* 10.
    (IF (MEMQ B '(8 10.)) B 8)))	;These are the only reasonable bases for debugging

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFMACRO INHERITING-VARIABLES-FROM ((SG) &BODY BODY)
  `(PROG ((.L. INHERITED-VARIABLES) .VAR. .VAL.)
     LP (SETQ .VAR. (IF (ATOM (CAR .L.)) (CAR .L.) (CAAR .L.))
	      .VAL. (SYMEVAL-IN-STACK-GROUP .VAR. ,SG))
	(BIND (VALUE-CELL-LOCATION .VAR.)
	      (IF (ATOM (CAR .L.)) .VAL. (FUNCALL (CADAR .L.) .VAL.)))
	(OR (ATOM (SETQ .L. (CDR .L.))) (GO LP))
	(LET ((READTABLE (IF *DEFAULT-COMMON-LISP*
			     SI:COMMON-LISP-READTABLE SI:STANDARD-READTABLE))
	      (SI:*READER-SYMBOL-SUBSTITUTIONS*
		(IF *DEFAULT-COMMON-LISP*
		    SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*)))
	  (RETURN (PROGN . ,BODY)))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SECOND-LEVEL-ERROR-HANDLER (SG EH-ERROR &OPTIONAL (IGNORE T)
				   ;; (IGNORE T) is never passed by callers.
				   ;; It forces off the fast arg option
				   ;; which persuades the compiler to bind
				   ;; INHIBIT-SCHEDULING-FLAG at function entry,
				   ;; preventing an abort at beginning of this function.
				   &AUX MSG
				   (INHIBIT-SCHEDULING-FLAG T)
				   (SI:PRINT-READABLY NIL)
				   (PACKAGE SI:PKG-USER-PACKAGE)
				   (DEFAULT-CONS-AREA ERROR-HANDLER-AREA)
				   SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD
				   (ERROR-HANDLER-RUNNING T)
				   (ERROR-HANDLER-REPRINT-ERROR T)
				   (TERMINAL-IO (OR (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						      '*DEBUG-IO-OVERRIDE* SG)
						    (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						      'DEBUG-IO SG)
						    (SYMEVAL-IN-STACK-GROUP 'TERMINAL-IO SG)))
				   (STANDARD-INPUT SI:SYN-TERMINAL-IO)
				   (STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
				   (QUERY-IO SI:SYN-TERMINAL-IO)
				   (DEBUG-IO SI:SYN-TERMINAL-IO)
				   (*DEBUG-IO-OVERRIDE* NIL)
				   ;; In case we want to set CURRENT-PROCESS to nil.
				   (CURRENT-PROCESS CURRENT-PROCESS)
				   CURRENT-FRAME ERROR-LOCUS-FRAME
				   INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
  (UNLESS TERMINAL-IO
    (SETQ MSG "TERMINAL-IO is NIL"))
  (COND ((EQ SG SI:SCHEDULER-STACK-GROUP)
	 (SETQ MSG "Error in the scheduler"))
	((AND (BOUNDP 'TV:KBD-PROCESS)
	      (EQ CURRENT-PROCESS TV:KBD-PROCESS))
	 (SETQ MSG "Error in the keyboard process"))
	((AND (BOUNDP 'TV:MOUSE-PROCESS)
	      (EQ CURRENT-PROCESS TV:MOUSE-PROCESS))
	 (SETQ MSG "Error in the mouse process")))
  ;; Get rid of call to error-handler sg
  (LET ((RP (SG-REGULAR-PDL SG)) (AP (SG-AP SG)))
    (IF (NEQ (AREF RP AP) %ERROR-HANDLER-STACK-GROUP)
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP not found on pdl where expected"))
    (IF ( (RP-DESTINATION RP AP) 0)		;D-IGNORE
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP called with bad destination"))
    (IF ( (SG-REGULAR-PDL-POINTER SG) (1+ AP))
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP called with wrong number of args"))
    (SETF (SG-IPMARK SG) (SG-NEXT-OPEN SG AP))
    (SETF (SG-AP SG) (SETQ AP (SG-NEXT-ACTIVE SG AP)))
    (SETF (SG-FLAGS-QBBFL SG)			;Must correspond to current frame to work!
	  (RP-BINDING-BLOCK-PUSHED RP AP))
    (DOTIMES (I 5)				;Pop p3zero, function, and arg
      (SG-REGPDL-POP SG))
    ;; Now, if current frame is a foothold, restore to the previous state.  This will
    ;; normally be the case for :BREAK
    (IF (EQ (AREF RP AP) #'FOOTHOLD) (SG-RESTORE-STATE SG 0)))
  ;; Handle weird things like (BREAK): create a condition-object.
  (IF (CONSP EH-ERROR)
      (SETQ EH-ERROR (APPLY 'MAKE-CONDITION EH-ERROR)))
  (SETF (SG-TRAP-TAG SG) EH-ERROR)
  ;; Clear the SG's trap-on-call flag so that our uses of SG-APPLY will not trap.
  ;; The SG-RESTORE-STATE, above, may have restored the flag to 1.
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
  (ASSURE-DISPATCH-SET-UP)
  (ASSURE-FREE-SPACE)
  (AND MSG (USE-COLD-LOAD-STREAM MSG))
  ;; Turn on interrupts if not in cold load stream.
  (UNLESS (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL))
  ;; If not running in the scheduler, give us a run reason in case we died after
  ;; becoming inactive, before getting back to the scheduler.
  (OR (NULL CURRENT-PROCESS)
      (FUNCALL CURRENT-PROCESS ':RUN-REASON CURRENT-STACK-GROUP))
  ;; Try to see if TERMINAL-IO is reasonable and if not fix it.
  (LET ((WO (ERRSET (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS) NIL))
	(ERROR-HANDLER-REPRINT-ERROR NIL))
    (IF (NULL WO) (USE-COLD-LOAD-STREAM "TERMINAL-IO clobbered")
      (COND ((MEMQ ':NOTICE (CAR WO))
	     (DO () (())
	       (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Continue entering the debugger.")
		 (LET (;; :NOTICE can change TERMINAL-IO of a background process
		       (OLD-TIO TERMINAL-IO)
		       ;; Send this message in non-erring stack
		       (WINDOW-BAD (FUNCALL TERMINAL-IO ':NOTICE ':ERROR)))
		   (IF (EQ WINDOW-BAD 'TV:COLD-LOAD-STREAM)
		       (USE-COLD-LOAD-STREAM "window-system problems")
		     (AND (NEQ TERMINAL-IO OLD-TIO)
			  (NOT WINDOW-BAD)
			  (SG-FUNCALL SG #'SET 'TERMINAL-IO TERMINAL-IO))))
		 (RETURN NIL)))))))
  ;; Turn off interrupts if switched to cold load stream.
  (IF (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
      (SETQ INHIBIT-SCHEDULING-FLAG T))
  (IF (VARIABLE-BOUNDP TV:COLD-LOAD-STREAM-OWNS-KEYBOARD)
      (SETQ SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD TV:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (LET-GLOBALLY-IF (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
		   ((TV:COLD-LOAD-STREAM-OWNS-KEYBOARD T))
    ;; Setting this causes the previous error to be reprinted if we abort to it.
    (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)
    ;; Give these reasonable values in case of error in the :FIND-CURRENT-FRAME method.
    (SETQ ERROR-LOCUS-FRAME (SG-AP SG)
	  CURRENT-FRAME (SG-AP SG)
	  INNERMOST-VISIBLE-FRAME (SG-AP SG))
    ;; These catches are so that quitting out of the printing of the error message
    ;; leaves you in the error handler at its
    ;; normal command level rather than quitting out of the whole program.
    (*CATCH 'QUIT
      (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Abort printing error message, enter debugger.")
	(SETF (VALUES ERROR-LOCUS-FRAME CURRENT-FRAME
		      INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
	      (SEND EH-ERROR ':FIND-CURRENT-FRAME SG))
	;; Print the error message, using appropriate package, base, etc.
	(INHERITING-VARIABLES-FROM (SG)
	  (PRINT-CAREFULLY "error message"
	    (SEND STANDARD-OUTPUT ':FRESH-LINE)
	    (SEND EH-ERROR ':PRINT-ERROR-MESSAGE
		  SG NIL STANDARD-OUTPUT))
	  (PRINT-BRIEF-ERROR-BACKTRACE SG EH-ERROR)
	  (SEND EH-ERROR ':MAYBE-CLEAR-INPUT STANDARD-INPUT))))
    ;; Offer any special commands, such as wrong-package correction.
    ;; Then enter the command loop.
    (SEND EH-ERROR ':DEBUGGER-COMMAND-LOOP SG)))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN DESCRIBE-PROCEED-TYPES (SG ERROR-OBJECT)
  "Print documentation of the available proceed-types and characters to get them.
ERROR-OBJECT is the object to document.  Output goes to STANDARD-OUTPUT."
  (WHEN ERROR-HANDLER-RUNNING
    (LET* ((PROCEED-TYPES  (SEND ERROR-OBJECT ':USER-PROCEED-TYPES
				 (SG-CONDITION-PROCEED-TYPES SG ERROR-OBJECT)))
	   (RESUME-HANDLERS (SYMEVAL-IN-STACK-GROUP 'CONDITION-RESUME-HANDLERS SG))
	   (ABORT-HANDLER (FIND-RESUME-HANDLER ABORT-OBJECT NIL RESUME-HANDLERS)))
      (DO ((KEYWORDS (APPEND PROCEED-TYPES SPECIAL-COMMANDS)
		     (CDR KEYWORDS))
	   (PROCEED-TYPES PROCEED-TYPES
			  (CDR PROCEED-TYPES))
	   TEM
	   THIS-ONE-FOR-ABORT
	   (I 0 (1+ I)))
	  ((NULL KEYWORDS))
	(IF (ZEROP I)
	    (FORMAT T "~&~%Commands available for this particular error:~2%"))
	(FORMAT T "s-~C" (+ #/A I))
	(WHEN PROCEED-TYPES
	  (SETQ THIS-ONE-FOR-ABORT
		(EQ (FIND-RESUME-HANDLER ERROR-OBJECT (CAR KEYWORDS) RESUME-HANDLERS)
		    ABORT-HANDLER))
	  (IF THIS-ONE-FOR-ABORT (SETQ ABORT-HANDLER NIL)))
	(COND ((AND (ZEROP I) (ATOM (CAR PROCEED-TYPES)))
	       ;; C-C only works for proceed-types that are atomic.
	       (FORMAT T ", ~C" #\RESUME))
	      ((SETQ TEM (ASSQ (CAR KEYWORDS)
			       (IF PROCEED-TYPES *PROCEED-TYPE-SPECIAL-KEYS*
				 *SPECIAL-COMMAND-SPECIAL-KEYS*)))
	       (FORMAT T ", ~C" (CDR TEM)))
	      ;; If Abort is synonymous with this one, mention that.
	      (THIS-ONE-FOR-ABORT
	       (FORMAT T ", ~C" #\ABORT)))
	(FORMAT T ":~13T")
	(SEND ERROR-OBJECT
	      (IF PROCEED-TYPES
		  ':DOCUMENT-PROCEED-TYPE
		':DOCUMENT-SPECIAL-COMMAND)
	      (CAR KEYWORDS) STANDARD-OUTPUT RESUME-HANDLERS)
	(SEND STANDARD-OUTPUT ':FRESH-LINE))
      (WHEN ABORT-HANDLER
	;; Abort is not currently synonymous with any of the proceed types.
	;; So document it specially.
	(FORMAT T "~C:~13T" #\ABORT)
	(SEND ABORT-OBJECT ':DOCUMENT-PROCEED-TYPE (SECOND ABORT-HANDLER)
	      STANDARD-OUTPUT RESUME-HANDLERS)
	(SEND STANDARD-OUTPUT ':FRESH-LINE)))))

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "


(DEFMETHOD (SHEET :INCREMENT-CURSORPOS) (DX DY &OPTIONAL (UNIT ':PIXEL))
  (SELECTQ UNIT
    (:PIXEL)
    (:CHARACTER
     (AND DX (SETQ DX (- (* CHAR-WIDTH DX)
			 (NTH-VALUE 1 (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)))))
     (AND DY (SETQ DY (- (* LINE-HEIGHT DY)
			 (NTH-VALUE 1 (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT))))))
    (OTHERWISE
      (FERROR NIL "~S is not a known unit." UNIT)))
  (PREPARE-SHEET (SELF)
    (OR (ZEROP (SHEET-EXCEPTIONS)) (SHEET-HANDLE-EXCEPTIONS SELF))
    (SHEET-INCREMENT-BITPOS SELF DX DY)))

(DEFMETHOD (SHEET :READ-CURSORPOS) (&OPTIONAL (UNIT ':PIXEL))
  (SELECTQ UNIT
    (:PIXEL
     (VALUES (- CURSOR-X LEFT-MARGIN-SIZE)
	     (- CURSOR-Y TOP-MARGIN-SIZE)))
    (:CHARACTER
     (VALUES (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)
	     (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT)))
    (OTHERWISE
     (FERROR NIL "~S is not a known unit." UNIT))))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-HELP (SG ERROR-OBJECT &REST IGNORE)
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  (FORMAT T
	  "You are in the debugger.  If you don't want to debug this error, type ~C.
Otherwise you can evaluate expressions in the context of the error,
examine the stack, and proceed//throw//return to recover.

If you type in a Lisp form, it will be evaluated, and the results printed,
using ~A syntax and semantics and base ~D in package ~A.
This evaluation uses the special variable environment
of the stack frame you are examining.

Type ~C or c-Z to get back to top level, or the previous debugger level.
While in the debugger, c-G quits back to the debugger top level.

If you think this error indicates a bug in the Lisp machine system,
use the control-M command.
"
	  #\ABORT
	  (IF *DEFAULT-COMMON-LISP* "Common Lisp" "traditional")
	  IBASE
	  (PACKAGE-NAME PACKAGE)
	  #\ABORT)

  (FORMAT T "For more help, type a letter to specify a topic:
  I - examining information
  F - selecting stack frames to examine
  S - stepping through the program
  X - resuming execution
  T - transfer to editor, window-oriented debugger, or bug report editor
What topic? ")
  (LET ((CH (CHAR-UPCASE (TYI))))
    (SELECTQ CH
      (#\F
       (FORMAT:OUTPUT T
	 "
Selecting stack frames:

c-N or "
	 (FORMAT:OCHAR #\LINE ':LOZENGED)
	 " goes down a frame, c-P or "
	 (FORMAT:OCHAR #\RETURN ':LOZENGED)
	 " goes up.
m-N and m-P are similar but show args, locals and compiled code.
c-m-N and c-m-P are similar to c-N and c-P, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
m-< and m-> go to the top and bottom of the stack, respectively.
c-S reads a string and searches down the stack for a frame
   calling a function whose name contains that substring.
"))
      (#\I
        (FORMAT T "
Information display:

c-L or ")
	(SEND STANDARD-OUTPUT
	      (IF (SEND STANDARD-OUTPUT ':OPERATION-HANDLED-P ':DISPLAY-LOZENGED-STRING)
		  ':DISPLAY-LOZENGED-STRING
		':STRING-OUT)
	      "Clear-screen")
	(FORMAT T " clears screen and retypes error message.
m-L clears screen and types args, locals and compiled code.
c-B gives a backtrace of function names.
m-B gives a backtrace of function names and argument names and values.
c-m-B is line m-B but shows EVALs, PROGs, CONDs, etc.
c-m-A prints an argument to the current function, and sets * to be that
   argument to let you do more complicated things with it.
   + is set to a locative to that argument, should you want to modify it.
   To specify which argument, type the argument number with Control
   or Meta held down before the c-m-A.
c-m-L is like c-m-A but works on the function's locals rather than the args.
c-m-V is like c-m-A but works on the values this frame is returning.
   (This is useful when you get a trap on exit from function).
c-m-F does likewise for the function itself.
c-A prints the arglist of the function in the current frame.
m-S prints the value in this frame of a special variable you specify.
c-m-S lists all special variable bindings in this frame.

Do (EH-ARG n), (EH-LOC n), (EH-VAL n) or (EH-FUN) to get the value of
an arg, local, value or function-object from an expression being evaluated.
For args and locals, n can be a name or a number.  EH-VAL allows numbers only.
LOCF and SETF on those expressions are also allowed.
"))
      (#\T
        (FORMAT T "
Transfer to other systems:

c-E calls the editor to edit the current function.
c-M enters the editor to send a bug message, and puts the error
  message and a backtrace into the message automatically.
  A numeric argument says how many stack frames to put in the backtrace.
c-m-W switches to the window-based debugger.
"))
      (#\S
       (IF ERROR-HANDLER-RUNNING
	   (FORMAT T "
Stepping commands:

c-X toggles the trap-on-exit flag for the current frame.
m-X sets the trap-on-exit flag for the current frame and all outer frames.
c-m-X clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
c-D proceeds like ~C, but first sets the trap-on-next-function-call flag.
m-D toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with c-X.
" #\RESUME)
	 (FORMAT T "
You cannot use the stepping commands, since you are in examine-only mode
")))
      (#\X
       (FORMAT T "
Commands to continue execution:

")
       (COND (ERROR-HANDLER-RUNNING
	      (FORMAT T "c-Z aborts to previous debugger or other command loop, or to top level.
c-R returns a value or values from the current frame.
c-m-R offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
m-R offers to reinvoke the current frame, letting you alter
   some of the arguments, or use more or fewer arguments.
c-T throws to a specific tag.")
	      (DESCRIBE-PROCEED-TYPES SG ERROR-OBJECT))
	     (T
	      (FORMAT T "You cannot continue execution, since you are in examine-only mode.
~C exits the debugger." #\RESUME))))
      (T (FORMAT T "~%~C is not a known topic" CH)))))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN WARN-ABOUT-SPECIAL-VARIABLES (SG)
  "Print warnings about vital special variables whose values in SG are peculiar."
  (CONDITION-CASE ()
      (LET ((BASE (SYMEVAL-IN-STACK-GROUP 'BASE SG))
	    (IBASE (SYMEVAL-IN-STACK-GROUP 'IBASE SG)))
	(IF (EQ BASE IBASE)
	    (UNLESS (NUMBERP BASE)
	      (FORMAT T "~&Warning: BASE and IBASE are ~D." BASE))
	  (FORMAT T "~&Warning: BASE is ~D but IBASE is ~D (both decimal).~%" BASE IBASE)))
    (ERROR
      (FORMAT T "~&Warning: error while trying to find BASE and IBASE in ~S." SG)))
  (CONDITION-CASE ()
      (LET ((AREANAME (AREA-NAME (SYMEVAL-IN-STACK-GROUP 'DEFAULT-CONS-AREA SG))))
	(OR (EQ AREANAME 'WORKING-STORAGE-AREA)
	    (FORMAT T "~&Warning: the default cons area is ~S~%" AREANAME)))
    (ERROR
      (FORMAT T "~&Warning: error while trying to find DEFAULT-CONS-AREA in ~S." SG)))
  (CONDITION-CASE ()
      (IF (SYMEVAL-IN-STACK-GROUP 'TAIL-RECURSION-FLAG SG)
	  (FORMAT T "~&Note: TAIL-RECURSION-FLAG is set, so some frames may no longer be on the stack.~%"))
    (ERROR
      (FORMAT T "~&Warning: error while trying to find TAIL-RECURSION-FLAG in ~S." SG)))
  (FORMAT T "~%Using ~A syntax and semantics and base ~D in package ~A."
	  (IF *DEFAULT-COMMON-LISP* "Common Lisp" "traditional") IBASE
	  (PACKAGE-NAME PACKAGE)))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN EVALUATE-MINI-BUFFER (&OPTIONAL INITIAL-CONTENTS INITIAL-CHAR-POS &AUX INTERVAL)
  "Read an expression with a mini buffer, and evaluate it.
INITIAL-CONTENTS is a string to initialize the contents from,
and INITIAL-CHAR-POS is where to put the cursor, as a number of
characters from the beginning."
  (MULTIPLE-VALUE (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB* INITIAL-CONTENTS INITIAL-CHAR-POS
			 '("Forms to evaluate (end with End)")))
  (LET ((FORM-STRING (STRING-INTERVAL INTERVAL)))
    (DO ((I 0)
	 (FORM)
	 (READTABLE (IF *DEFAULT-COMMON-LISP*
			SI:COMMON-LISP-READTABLE SI:STANDARD-READTABLE))
	 (SI:*READER-SYMBOL-SUBSTITUTIONS*
	   (IF *DEFAULT-COMMON-LISP*
	       SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*))
	 (BASE (IF *DEFAULT-COMMON-LISP* 10. BASE))
	 (IBASE (IF *DEFAULT-COMMON-LISP* 10. BASE))
	 (*NOPOINT (OR *DEFAULT-COMMON-LISP* *NOPOINT))
	 (EOF '(())))
	(NIL)
      (CONDITION-CASE (ERROR)
	  (MULTIPLE-VALUE (FORM I)
	    (READ-FROM-STRING FORM-STRING EOF I))
	(SYS:READ-ERROR
	 (BARF (SEND ERROR ':REPORT-STRING))
	 (RETURN NIL)))
      (COND ((EQ FORM EOF) (RETURN NIL)))
      (DO ((VALS (LET ((STANDARD-INPUT SI:SYN-TERMINAL-IO))
		   (MULTIPLE-VALUE-LIST (SI:EVAL-ABORT-TRIVIAL-ERRORS FORM)))
		 (CDR VALS))
	   (FLAG T NIL))
	  ((NULL VALS))
	(UNLESS FLAG (SEND QUERY-IO ':FRESH-LINE))
	(FORMAT QUERY-IO "~:[, ~]~S" FLAG (CAR VALS)))))
  DIS-TEXT)	;DIS-TEXT in case user manually alters the buffer with Lisp code

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFCONST INHERITED-VARIABLES
    '((PACKAGE VALIDATE-PACKAGE)
      (BASE VALIDATE-BASE)
      (IBASE VALIDATE-BASE)
      (ERROR-DEPTH VALIDATE-ERROR-DEPTH)
      (*NOPOINT VALIDATE-*NOPOINT)))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN VALIDATE-*NOPOINT (N)
  (OR *DEFAULT-COMMON-LISP* N))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN (:COMMON-LISP FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
  (VALUES (LIST* '*READTABLE* 'SI:*READER-SYMBOL-SUBSTITUTIONS*
		 'SI:INTERPRETER-FUNCTION-ENVIRONMENT '*NOPOINT
		 NIL)
	  (LIST* SI:COMMON-LISP-READTABLE
		 (IF VAL SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS* NIL)
		 NIL T
		 NIL)))

))

; From file PRODEF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PRODEF  "

(DEFSUBST PROCESS-CLOSURE (PROC)
  (PROCESS-SPARE-SLOT-1 PROC))

))

; From file PROCES.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(DEFUN PROCESS-TOP-LEVEL (&OPTIONAL IGNORE)
  (LET ((TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))
    ;; Bind these per process, and arrange that their values
    ;; stay around even if the process is reset!
    (UNLESS (LOCATION-BOUNDP (LOCF (PROCESS-CLOSURE CURRENT-PROCESS)))
      (SETF (PROCESS-CLOSURE CURRENT-PROCESS)
	    (LET-CLOSED (+ ++ +++ * ** *** // //// ////// - *VALUES* *DEFAULT-COMMON-LISP*)
	      'FUNCALL)))
    (%USING-BINDING-INSTANCES (CLOSURE-BINDINGS (PROCESS-CLOSURE CURRENT-PROCESS)))
    (DO-FOREVER
      (CATCH-ERROR-RESTART (CONDITION "Reset and arrest process ~A."
				      (SEND CURRENT-PROCESS ':NAME))
	(UNWIND-PROTECT
	    (ERROR-RESTART ((SYS:ABORT CONDITION) "Restart process ~A."
			    (SEND CURRENT-PROCESS ':NAME))
	      (APPLY (CAR (PROCESS-INITIAL-FORM CURRENT-PROCESS))
		     (CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS)))
	      (PROCESS-FLUSH-BACKGROUND-STREAM)
	      (PROCESS-WAIT-FOREVER))
	  (PROCESS-FLUSH-BACKGROUND-STREAM)))
      (SEND CURRENT-PROCESS ':ARREST-REASON ':USER)
      (PROCESS-ALLOW-SCHEDULE))))

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFVAR *VALUES* NIL
  "List of all lists-of-values produced by the expressions evaluated in this listen loop.
Most recent evaluations come first on the list.")

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "
(DEFUN BREAK (&OPTIONAL &QUOTE FORMAT-STRING &EVAL &REST ARGS
	      &AUX SAVED-BUFFER SAVED-BUFFER-POSITION)
  "Read-eval-print loop for use as subroutine.  Args are passed to FORMAT.
Many variables are rebound, as specified in SI:*BREAK-BINDINGS*."
  (SETQ FORMAT-STRING
	(IF (OR (SYMBOLP FORMAT-STRING) (AND (CONSP FORMAT-STRING)
					     (EQ (CAR FORMAT-STRING) 'QUOTE)
					     (SYMBOLP (CADR FORMAT-STRING))
					     (NULL (CDDR FORMAT-STRING))
					     (SETQ FORMAT-STRING (CADR FORMAT-STRING))))
	    (STRING FORMAT-STRING)
	  (EVAL1 FORMAT-STRING)))
  (UNLESS (OR (EQUAL FORMAT-STRING "")
	      (MEMQ (AREF FORMAT-STRING (1- (LENGTH FORMAT-STRING))) '(#/. #/? #/!)))
    (SETQ FORMAT-STRING (STRING-APPEND FORMAT-STRING #/.)))
  (LET-IF *DEFAULT-COMMON-LISP*
	  ((BASE 10.) (IBASE 10.) (*NOPOINT T))
    (PROGW *BREAK-BINDINGS*
      ;; Deal with keyboard multiplexing in a way similar to the error-handler.
      ;; If we break in the scheduler, set CURRENT-PROCESS to NIL.
      ;; If this is not the scheduler process, make sure it has a run reason
      ;; in case we broke in the middle of code manipulating process data.
      ;; If INHIBIT-SCHEDULING-FLAG is set, turn it off and print a warning.
      (COND ((EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP)
	     (SETQ CURRENT-PROCESS NIL)))
      (AND (NOT (NULL CURRENT-PROCESS))
	   (NULL (FUNCALL CURRENT-PROCESS ':RUN-REASONS))
	   (FUNCALL CURRENT-PROCESS ':RUN-REASON 'BREAK))
      (COND (INHIBIT-SCHEDULING-FLAG
	     (FORMAT T "~%---> Turning off INHIBIT-SCHEDULING-FLAG, you may lose. <---~%")
	     (SETQ INHIBIT-SCHEDULING-FLAG NIL)))
      (AND (MEMQ ':SAVE-RUBOUT-HANDLER-BUFFER (FUNCALL OLD-STANDARD-INPUT
						       ':WHICH-OPERATIONS))
	   (SETF (VALUES SAVED-BUFFER SAVED-BUFFER-POSITION)
		 (FUNCALL OLD-STANDARD-INPUT ':SAVE-RUBOUT-HANDLER-BUFFER)))
      (FORMAT T "~&;Breakpoint ~?  ~:@C to continue, ~:@C to quit.~%"
	      FORMAT-STRING ARGS #\RESUME #\ABORT)
      (FORMAT T ";Using ~A syntax and semantics and base ~D in package ~A.~%"
	      (IF *DEFAULT-COMMON-LISP* "Common Lisp" "traditional") IBASE
	      (PACKAGE-NAME PACKAGE))
      (LET ((VALUE
	      (DO ()
		  (NIL)		;Do forever (until explicit return)
		(TERPRI)
	       LOOK-FOR-SPECIAL-KEYS
	        (LET ((CHAR (FUNCALL STANDARD-INPUT ':TYI)))
		  ;; Intercept characters even if otherwise disabled in program
		  ;; broken out of.  Also treat c-Z like ABORT for convenience
		  ;; and for compatibility with the error handler.
		  (AND (= CHAR #\C-Z) (SETQ CHAR #\ABORT))
		  (COND ((AND (BOUNDP 'TV:KBD-STANDARD-INTERCEPTED-CHARACTERS)
			      (ASSQ CHAR TV:KBD-STANDARD-INTERCEPTED-CHARACTERS))
			 (FUNCALL (CADR (ASSQ CHAR TV:KBD-STANDARD-INTERCEPTED-CHARACTERS))
				  CHAR))
			((= CHAR #\RESUME)
			 (FUNCALL STANDARD-OUTPUT ':STRING-OUT "[Resume]
    ")
			 (RETURN NIL))
			(T (FUNCALL STANDARD-INPUT ':UNTYI CHAR))))
		(LET ((EH:CONDITION-RESUME-HANDLERS (CONS T EH:CONDITION-RESUME-HANDLERS))
		      (THROW-FLAG T))
		  (CATCH-ERROR-RESTART ((SYS:ABORT ERROR)
					"Return to BREAK ~?"
					FORMAT-STRING ARGS)
		    (MULTIPLE-VALUE-BIND (TEM1 TEM)
			(FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT
								     :FULL-RUBOUT)
								   (:ACTIVATION = #\END))
				 #'READ-FOR-TOP-LEVEL)
		      (COND ((EQ TEM ':FULL-RUBOUT)
			     (GO LOOK-FOR-SPECIAL-KEYS)))
		      (SHIFTF +++ ++ + - TEM1))
		    (COND ((AND (CONSP -) (EQ (CAR -) 'RETURN))
			   (RETURN (EVAL-ABORT-TRIVIAL-ERRORS (CADR -)))))	;(RETURN form) proceeds
		    (LET (VALUES)
		      (UNWIND-PROTECT
			  (SETQ VALUES
				(MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
			;; Always push SOMETHING for each form evaluated.
			(PUSH VALUES *VALUES*))
		      (SETQ ////// ////
			    //// //
			    // VALUES)
		      (SETQ *** **
			    ** *
			    * (CAR //)))
		    (DOLIST (VALUE //)
		      (TERPRI)
		      (FUNCALL (OR PRIN1 #'PRIN1) VALUE))
		    (SETQ THROW-FLAG NIL))
		  (WHEN THROW-FLAG
		    (FORMAT T "~&;Back to Breakpoint ~?  ~:@C to continue, ~:@C to quit.~%"
			    FORMAT-STRING ARGS #\RESUME #\ABORT))))))
	;; Before returning, restore and redisplay rubout handler's buffer so user
	;; gets what he sees, if we broke out of reading through the rubout handler.
	;; If we weren't inside there, the rubout handler buffer is now empty because
	;; we read from it, so leave it alone.  (Used to :CLEAR-INPUT).
	(COND (SAVED-BUFFER
	       (FUNCALL OLD-STANDARD-INPUT ':RESTORE-RUBOUT-HANDLER-BUFFER
			SAVED-BUFFER SAVED-BUFFER-POSITION)))
	VALUE))))

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN LISP-TOP-LEVEL1 (TERMINAL-IO &AUX OLD-PACKAGE W-PKG)
  "Read-eval-print loop used by lisp listeners.  TERMINAL-IO
is the stream to read and print with."
  (COND ((VARIABLE-BOUNDP PACKAGE)
	 (BIND (LOCF PACKAGE) PACKAGE)))
  (FORMAT T "~&;Reading at top level")
  (IF (SEND TERMINAL-IO ':OPERATION-HANDLED-P ':NAME)
      (FORMAT T " in ~A." (SEND TERMINAL-IO ':NAME))
    (FORMAT T "."))
  (PUSH NIL *VALUES*)
  (DO ((READTABLE STANDARD-READTABLE)
       *READER-SYMBOL-SUBSTITUTIONS*
       (BASE BASE) (IBASE IBASE) (*NOPOINT *NOPOINT)
       (LAST-TIME-DEFAULT-COMMON-LISP '(NIL))
       THROW-FLAG)	;Gets non-NIL if throw to COMMAND-LEVEL (e.g. quitting from an error)
      (NIL)		;Do forever
    ;; If PACKAGE has changed, set OLD-PACKAGE and tell our window.
    ;; Conversely, if the window's package has changed, change ours.
    ;; The first iteration, we always copy from the window.
    (COND ((NOT (VARIABLE-BOUNDP PACKAGE)))
	  ((EQ TERMINAL-IO COLD-LOAD-STREAM))
	  ;; User set the package during previous iteration of DO
	  ;; => tell the window about it.
	  ((AND OLD-PACKAGE (NEQ PACKAGE OLD-PACKAGE))
	   (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':SET-PACKAGE PACKAGE)
	   (SETQ OLD-PACKAGE PACKAGE))
	  ;; Window's package has been changed, or first iteration through DO,
	  ;; => set our package to the window's -- if the window has one.
	  ((SETQ W-PKG (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':PACKAGE))
	   (AND (NEQ W-PKG PACKAGE)
		(SETQ PACKAGE W-PKG))
	   (SETQ OLD-PACKAGE PACKAGE))
	  ;; First time ever for this window => set window's package
	  ;; to the global value of PACKAGE.
	  ((NULL OLD-PACKAGE)
	   (SETQ OLD-PACKAGE PACKAGE)
	   (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':SET-PACKAGE PACKAGE)))
    (UNLESS (EQ *DEFAULT-COMMON-LISP* LAST-TIME-DEFAULT-COMMON-LISP)
      (SETQ READTABLE (IF *DEFAULT-COMMON-LISP* COMMON-LISP-READTABLE STANDARD-READTABLE)
	    *READER-SYMBOL-SUBSTITUTIONS*
	    (IF *DEFAULT-COMMON-LISP* *COMMON-LISP-SYMBOL-SUBSTITUTIONS*)
	    LAST-TIME-DEFAULT-COMMON-LISP *DEFAULT-COMMON-LISP*
	    BASE (IF *DEFAULT-COMMON-LISP* 10. (SYMEVAL-GLOBALLY 'BASE))
	    IBASE BASE
	    *NOPOINT (OR *DEFAULT-COMMON-LISP* (SYMEVAL-GLOBALLY '*NOPOINT)))
      (FORMAT T "~%;Now using ~A syntax and semantics and base ~D in package ~A.~%"
	      (IF *DEFAULT-COMMON-LISP* "Common Lisp" "traditional") IBASE
	      (PACKAGE-NAME PACKAGE)))
    (SETQ THROW-FLAG T)
    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to top level in ~A."
			  (OR (SEND TERMINAL-IO ':SEND-IF-HANDLES ':NAME)
			      "current process."))
      (TERPRI)
      (SETQ +++ ++ ++ + + -)			;Save last three input forms
      (SETQ - (READ-FOR-TOP-LEVEL))
      (LET ((LISP-TOP-LEVEL-INSIDE-EVAL T)
	    VALUES)
	(UNWIND-PROTECT
	    (SETQ VALUES (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
	  ;; Always push SOMETHING -- NIL if evaluation is aborted.
	  (PUSH VALUES *VALUES*))
	(SETQ ////// ////
	      //// //
	      // VALUES)
	(SETQ *** **				;Save first value, propagate old saved values
	      ** *
	      * (CAR //)))
      (DOLIST (VALUE //)
	(TERPRI)
	(FUNCALL (OR PRIN1 #'PRIN1) VALUE))
      (SETQ THROW-FLAG NIL))
    (WHEN THROW-FLAG
      ;; Inform user of return to top level.
      (FORMAT T "~&;Back to top level")
      (IF (SEND TERMINAL-IO ':OPERATION-HANDLED-P ':NAME)
	  (FORMAT T " in ~A." (SEND TERMINAL-IO ':NAME))
	(TYO #/.)))))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COMMAND-LOOP (ERROR-SG ERROR-OBJECT
		     &AUX FUNCTION SEXP 
		     (EVALHOOK NIL)
		     SPECIAL-COMMANDS
		     (WINDOW-ERROR-HANDLER-OLD-WINDOW NIL)
		     IO-BUFFER
		     READING-COMMAND)
  (WHEN ERROR-OBJECT
    (SETQ SPECIAL-COMMANDS (SEND ERROR-OBJECT ':SPECIAL-COMMAND ':WHICH-OPERATIONS))
    (SEND ERROR-OBJECT ':INITIALIZE-SPECIAL-COMMANDS))
  (COND ((MEMQ ':IO-BUFFER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
	 (SETQ IO-BUFFER (FUNCALL STANDARD-INPUT ':IO-BUFFER))
	 (BIND (LOCF (TV:IO-BUFFER-OUTPUT-FUNCTION IO-BUFFER)) 'IO-BUFFER-OUTPUT-FUNCTION)
	 (BIND (LOCF (TV:IO-BUFFER-INPUT-FUNCTION IO-BUFFER)) NIL)))
  (INHERITING-VARIABLES-FROM (ERROR-SG)  ;Do this every time around the loop in case of setq
    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to debugger command loop.")
      (*CATCH 'QUIT
	(SHOW-FUNCTION-AND-ARGS ERROR-SG)
	(WARN-ABOUT-SPECIAL-VARIABLES ERROR-SG)
	(UNLESS *INHIBIT-DEBUGGER-PROCEED-PROMPT*
	  (DESCRIBE-PROCEED-TYPES ERROR-SG ERROR-OBJECT)))))
  (ERROR-RESTART (SYS:ABORT "Return to debugger command loop")
    (DO ((NUMERIC-ARG NIL NIL)
	 (-)
	 (+ (SYMEVAL-IN-STACK-GROUP '- ERROR-SG))
	 (++ (SYMEVAL-IN-STACK-GROUP '+ ERROR-SG))
	 (+++ (SYMEVAL-IN-STACK-GROUP '++ ERROR-SG))
	 (* (SYMEVAL-IN-STACK-GROUP '* ERROR-SG))
	 (** (SYMEVAL-IN-STACK-GROUP '** ERROR-SG))
	 (*** (SYMEVAL-IN-STACK-GROUP '*** ERROR-SG))
	 (// (SYMEVAL-IN-STACK-GROUP '// ERROR-SG))
	 (//// (SYMEVAL-IN-STACK-GROUP '//// ERROR-SG))
	 (////// (SYMEVAL-IN-STACK-GROUP '////// ERROR-SG))
	 (*VALUES* (SYMEVAL-IN-STACK-GROUP '*VALUES* ERROR-SG)))
	(())
      (INHERITING-VARIABLES-FROM (ERROR-SG)  ;Do this every time around the loop in case of setq
	(UNLESS ERROR-HANDLER-RUNNING
	  (SETQ ERROR-DEPTH 1))
	(CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to debugger command loop.")
	  (*CATCH 'QUIT
	    (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
	    (DOTIMES (I ERROR-DEPTH)
	      (FUNCALL STANDARD-OUTPUT ':TYO #/))
	    (DO () (NIL)				;This loop processes numeric args
	      ;; Read the next command or sexp, with combined rubout processing.
	      (MULTIPLE-VALUE (FUNCTION SEXP)
		(COMMAND-LOOP-READ))
	      ;; If it's a character, execute the definition or complain.
	      (COND ((NUMBERP FUNCTION)
		     (SETQ NUMERIC-ARG
			   (IF (NULL NUMERIC-ARG) FUNCTION (+ FUNCTION (* 10. NUMERIC-ARG))))
		     (TYO #\SPACE))
		    (FUNCTION
		     (PRINC " ")			;Print a space after the echo in case it prints something
		     (LET ((EH-COMMAND-CHAR SEXP))
		       (RETURN (IF (NOT NUMERIC-ARG)
				   (FUNCALL FUNCTION ERROR-SG ERROR-OBJECT)
				 (FUNCALL FUNCTION ERROR-SG ERROR-OBJECT NUMERIC-ARG)))))
		    ;; If there was no command, there was a sexp, so eval it.
		    (T
		     (*CATCH 'QUIT
		       (SETQ +++ ++ ++ + + -)
		       (LET (VALUES)
			 (UNWIND-PROTECT
			     (SETQ VALUES (SG-EVAL-IN-FRAME ERROR-SG (SETQ - SEXP) CURRENT-FRAME T))
			   (PUSH (UNLESS (EQ VALUES ERROR-FLAG) VALUES) *VALUES*))
			 (COND ((NEQ VALUES ERROR-FLAG)
				(SETQ ////// //// //// //)
				(SETQ *** ** ** *)
				(SETQ // VALUES * (CAR //))
				(DOLIST (VALUE //)
				  (TERPRI)
				  (FUNCALL (OR PRIN1 #'PRIN1) VALUE))))))
		     (RETURN))))))))))

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN LISP-REINITIALIZE (&OPTIONAL (CALLED-BY-USER T)
			  &AUX (COLD-BOOT COLD-BOOTING)
			  MUST-ENABLE-TRAPPING)
  ;; Needed until this file recompiled in a world with 25-bit pointers.
  (SET 'A-MEMORY-VIRTUAL-ADDRESS (LSH (ASH A-MEMORY-VIRTUAL-ADDRESS -3) 3))
  (SET 'IO-SPACE-VIRTUAL-ADDRESS (LSH (ASH IO-SPACE-VIRTUAL-ADDRESS -3) 3))
  (SET 'UNIBUS-VIRTUAL-ADDRESS (LSH (ASH UNIBUS-VIRTUAL-ADDRESS -3) 3))

  (SETQ INHIBIT-SCHEDULING-FLAG T)		;In case called by the user
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
      (SETQ TV:WHO-LINE-FILE-STATE-SHEET #'(LAMBDA (&REST IGNORE) NIL)))
  (UNCLOSUREBIND '(* ** *** + ++ +++ // //// ////// *VALUES* *DEFAULT-COMMON-LISP*))
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
  ;now that the "unibus" channel is set up, turn on 60Hz interrupts
  (AND (= PROCESSOR-TYPE-CODE 2)
       (COMPILER:%NUBUS-WRITE #XF8 4 (LOGIOR 40 (COMPILER:%NUBUS-READ #XF8 4))))

  ;;Flush any closure binding forwarding pointers
  ;;left around from a closure we were in when we warm booted.
  (UNCLOSUREBIND '(PRIN1 BASE *NOPOINT FDEFINE-FILE-PATHNAME INHIBIT-FDEFINE-WARNINGS
			 SELF SI:PRINT-READABLY PACKAGE READTABLE
			 EH:ERROR-MESSAGE-HOOK EH:ERROR-DEPTH EH:ERRSET-STATUS))

  ;Get the right readtable.
  (OR (VARIABLE-BOUNDP INITIAL-READTABLE)
      (SETQ INITIAL-READTABLE READTABLE
	    READTABLE (COPY-READTABLE READTABLE)
	    STANDARD-READTABLE READTABLE))
  (WHEN (VARIABLE-BOUNDP COMMON-LISP-READTABLE)
    (UNLESS (VARIABLE-BOUNDP INITIAL-COMMON-LISP-READTABLE)
      (SETQ INITIAL-COMMON-LISP-READTABLE COMMON-LISP-READTABLE
	    COMMON-LISP-READTABLE (COPY-READTABLE COMMON-LISP-READTABLE))))

  ;; Initialize the rubout handler.
  (SETQ	RUBOUT-HANDLER NIL TV:RUBOUT-HANDLER-INSIDE NIL)	;We're not in it now

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

  ;And all kinds of randomness...

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
  (SETQ IBASE 8 BASE 8 *NOPOINT NIL)
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
      (SETQ PACKAGE PKG-USER-PACKAGE))

  ;; The first time, this does top-level SETQ's from the cold-load files
  (OR (BOUNDP 'ORIGINAL-LISP-CRASH-LIST)	;Save it for possible later inspection
      (SETQ ORIGINAL-LISP-CRASH-LIST LISP-CRASH-LIST))
  (MAPC (FUNCTION EVAL) LISP-CRASH-LIST)
  (SETQ LISP-CRASH-LIST NIL)

  ;Reattach IO streams.  Note that TERMINAL-IO will be fixed later to go to a window.
  (OR (BOUNDP 'SYN-TERMINAL-IO) )
  (COND ((NOT CALLED-BY-USER)
	 (UNCLOSUREBIND '(TERMINAL-IO STANDARD-OUTPUT STANDARD-INPUT
			  QUERY-IO TRACE-OUTPUT ERROR-OUTPUT DEBUG-IO))
	 (SETQ TERMINAL-IO     COLD-LOAD-STREAM
	       STANDARD-OUTPUT SYN-TERMINAL-IO
	       STANDARD-INPUT  SYN-TERMINAL-IO
	       QUERY-IO        SYN-TERMINAL-IO
	       DEBUG-IO        SYN-TERMINAL-IO
	       TRACE-OUTPUT    SYN-TERMINAL-IO
	       ERROR-OUTPUT    SYN-TERMINAL-IO)
	 (FUNCALL TERMINAL-IO ':HOME-CURSOR)))

  (SETQ TV:MOUSE-WINDOW NIL)	;This gets looked at before the mouse process is turned on
  (KBD-CONVERT-NEW 1_15.)	;Reset state of shift keys

  (IF (AND (FBOUNDP 'CADR:CLEAR-UNIBUS-MAP)        ;clear valid bits on unibus map.
	   (= PROCESSOR-TYPE-CODE 1)) ; Prevents randomness
      (CADR:CLEAR-UNIBUS-MAP))		; and necessary if sharing Unibus with PDP11.
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
	 (MULTIPLE-VALUE-BIND (X Y) (FUNCALL TERMINAL-IO ':READ-CURSORPOS)
	   (FUNCALL TV:INITIAL-LISP-LISTENER ':SET-CURSORPOS X Y))
	 (SETQ TERMINAL-IO TV:INITIAL-LISP-LISTENER)
	 (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':SET-PACKAGE PACKAGE)
	 (FUNCALL TERMINAL-IO ':FRESH-LINE))
	(T (SETQ TV:INITIAL-LISP-LISTENER NIL)	;Not created yet
	   (FUNCALL TERMINAL-IO ':CLEAR-EOL)))

  (AND CURRENT-PROCESS
       (FUNCALL CURRENT-PROCESS ':RUN-REASON 'LISP-INITIALIZE))

  ; prevent screw from things being traced during initialization
  (if (fboundp 'untrace) (untrace))
  (if (fboundp 'breakon) (unbreakon))

  (INITIALIZATIONS 'COLD-INITIALIZATION-LIST)

  (INITIALIZATIONS 'WARM-INITIALIZATION-LIST T)

  (COND ((AND MUST-ENABLE-TRAPPING
	      (BOUNDP 'EH:ERROR-TABLE))
	 ;; Note: this was done here if we just loaded the error table for the first time.
	 (FUNCALL ERROR-STACK-GROUP '(INITIALIZE)) ; Initialize co-routining.
	 (EH:ENABLE-TRAPPING)))

  (SETQ COLD-BOOTING NIL)

  (IF (FBOUNDP 'PRINT-HERALD)
      (PRINT-HERALD)
    (FUNCALL STANDARD-OUTPUT ':CLEAR-EOL)
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

  ;; The global value of TERMINAL-IO is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (COND ((AND (NOT CALLED-BY-USER)
	      (FBOUNDP TV:DEFAULT-BACKGROUND-STREAM))
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM)))

  ;; Now that -all- initialization has been completed, allow network servers
  (SETQ CHAOS:CHAOS-SERVERS-ENABLED T))

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN LISP-TOP-LEVEL ()
  (LISP-REINITIALIZE NIL)			;(Re)Initialize critical variables and things
  (TERPRI (OR TV:INITIAL-LISP-LISTENER TERMINAL-IO))
  (LOOP DOING
	(IF (FBOUNDP 'PROCESS-TOP-LEVEL)
	    (PROCESS-TOP-LEVEL)
	  (LISP-TOP-LEVEL1 (OR TV:INITIAL-LISP-LISTENER TERMINAL-IO)))
	;;LISP-TOP-LEVEL1 supposedly never returns, but loop anyway in case
	;;someone forces it to return with the error-handler.
    ))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun var-not-special (symbol)
  (cerror ':no-action nil 'variable-not-special
	  "~S is referenced as a free variable but not declared special." symbol)
  t)

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defvar interpreter-environment nil
  "The current lexical environment for evaluation.
The value is a list of environment frames, each of which looks like
 (CELL VALUE CELL VALUE ...)
Each CELL is a locative (usually to a value cell),
and the following VALUE is the lexical value for that cell.
For a special binding, the VALUE is actually a DTP-ONE-Q-FORWARD to the cell.

Each place where a group of variables is bound (each LET, LAMBDA, PROG,...)
makes its own environment frame.

Other entries on the list represent BLOCKs and TAGBODYs.
These entries start with the symbol BLOCK or TAGBODY;
the second element is a list of further data:
 for a BLOCK: (NAME CATCH-TAG)
 for a TAGBODY: (BODY CATCH-TAG)
These special entries are not confusing because in a real binding frame
the first element is always a locative to a cell.

The tail of the list can be T rather than NIL.  This means
that all variables should be considered special if not found
in the entries in the environment.")


))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defsubst interpreter-symeval (symbol)
  (let (tem)
    symbol
    (do ((tail interpreter-environment (cdr tail)))
	((atom tail)
	 (or tail (getl symbol '(special system-constant))
	     (var-not-special symbol))
	 (symeval symbol))
      (and (setq tem (get-lexical-value-cell (car tail) (value-cell-location symbol)))
	   (return (car tem))))))

(defun interpreter-boundp (symbol &aux tem)
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail)
       (or tail (getl symbol '(special system-constant))
	   (var-not-special symbol))
       (boundp symbol))
    (and (setq tem (get-lexical-value-cell (car tail) (value-cell-location symbol)))
	 (return t))))

(defsubst interpreter-set (symbol value)
  (let (tem)
    symbol
    (do ((tail interpreter-environment (cdr tail)))
	((atom tail)
	 (or tail (getl symbol '(special system-constant))
	     (var-not-special symbol))
	 (set symbol value))
      (and (setq tem (get-lexical-value-cell (car tail) (value-cell-location symbol)))
	   (return (setf (car tem) value))))))

(defun interpreter-external-value-cell (symbol &aux tem)
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail)
       (or tail (getl symbol '(special system-constant))
	   (var-not-special symbol))
       (%external-value-cell symbol))
    (and (setq tem (get-lexical-value-cell (car tail) (value-cell-location symbol)))
	 (return (locf (car tem))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun eval1 (form &optional nohook)
  "Evaluate FORM in the current lexical environment, returning its value(s).
If the current environment says /"traditional Zetalisp/", we do that.
This is the function that special forms such as COND use to evaluate
their subexpressions, as it allows the subexpressions to access
lexical variables of the containing code.  Contrast with EVAL, *EVAL and CLI:EVAL."
  ;; Make sure all instances of ARGNUM, below, are local slot 0.
  (let (argnum) argnum)
  (cond ((and *evalhook* (not nohook))
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (with-stack-list (env interpreter-environment interpreter-function-environment)
	     (funcall tem form env))))
	((symbolp form)
	 (if (eq interpreter-function-environment t)
	     (symeval form)
	   (if (keywordp form)
	       form
	     (interpreter-symeval form))))
	((atom form) form)
	((eq (car form) 'quote)
	 (cadr form))
	(t (let ((fctn (car form)) arg-desc num-args closure-passed)
	     ;; Trace FCTN through symbols and closures to get the ultimate function
	     ;; which will tell us whether to evaluate the args.
	     ;; When we actually call the function, we call FCTN
	     ;; unless CLOSURE-PASSED is set.  In that case, we call (CAR FORM).
	     (do () (())
	       (typecase fctn
		 (symbol
		  (setq fctn
			(if (eq interpreter-function-environment t)
			    (fsymeval fctn)
			  (interpreter-fsymeval fctn))))
		 ((or closure entity)
		  (setq fctn (closure-function fctn)
			closure-passed t))
		 (t (return))))
	     (setq arg-desc (%args-info fctn))
	     (if (bit-test %arg-desc-interpreted arg-desc)
		 ;; Here if not a FEF.
		 (progn
		  ;; Detect ucode entry that is not actually microcoded.
		  (if (and (typep fctn ':microcode-function)
			   (not (fixp (system:micro-code-entry-area (%pointer fctn)))))
		      (setq fctn (system:micro-code-entry-area (%pointer fctn))))
		  (typecase fctn
		    (:list
		      (selectq (car fctn)
			((lambda subst named-lambda named-subst
			  cli:lambda cli:subst cli:named-lambda cli:named-subst)
			 (let ((lambda-list
				 (if (memq (car fctn)
					   '(named-lambda named-subst
					     cli:named-lambda cli:named-subst))
				     (caddr fctn) (cadr fctn))))
			   (setq num-args 0)
			   ;; Figure out whether there is a quoted rest argument,
			   ;; and open the call block with or without adi accordingly.
			   ;; Set NUM-ARGS to the number of args not counting any quoted rest arg.
			   (do ((ll lambda-list (cdr ll))
				(quote-status '&eval)
				rest-flag)
			       ((or (null ll)
				    (memq (car ll) '(&aux &key)))
				(if *applyhook*
				    (progn (%open-call-block 'applyhook1 0 4)
					   (%push (if closure-passed (car form) fctn)))
				  (%open-call-block (if closure-passed (car form) fctn) 0 4))
				(setq num-args (length (cdr form)))
				(%assure-pdl-room num-args))
			     (cond ((memq (car ll) '(&eval &quote))
				    (setq quote-status (car ll)))
				   ((eq (car ll) '&rest)
				    (setq rest-flag t))
				   ((memq (car ll) lambda-list-keywords))
				   (rest-flag
				    ;; Here if we encounter a rest arg.
				    (if ( (length (cdr form))
					   (if (eq quote-status '&quote)
					       num-args
					     63.))
					;; If there aren't enough args supplied to actually
					;; reach it, arrange to exit thru the DO's end-test.
					(setq ll nil)
				      ;; If the quoted res arg is non-nil,
				      ;; set NUM-ARGS to number of spread args,
				      ;; and call with ADI.
				      (if *applyhook*
					  (progn (%open-call-block 'applyhook2 0 4)
						 (%push (if closure-passed (car form) fctn)))
					(%push 0)
					(%push 14000000)
					(%open-call-block (if closure-passed (car form) fctn) 1 4))
				      (%assure-pdl-room (1+ num-args))
				      (return)))
				   (t (incf num-args))))
			   ;; Now push the args, evalling those that need it.
			   (do ((ll lambda-list (cdr ll))
				(argl (cdr form) (cdr argl))
				(quote-status '&eval)
				(argnum 0 (1+ argnum)))
			       (())
			     (do () ((null ll))
			       (cond ((memq (car ll) '(&eval &quote))
				      (setq quote-status (car ll)))
				     ((memq (car ll) '(&rest &aux &key))
				      (setq ll nil))
				     ((memq (car ll) lambda-list-keywords))
				     (t (return)))
			       (pop ll))
			     (if (= argnum num-args)
				 ;; Done with spread args => push the rest arg.
				 (return
				   (when argl
				     (%push
				       (if (eq quote-status '&eval)
					   (mapcar 'eval1 argl)
					 argl)))))
			     (if (eq quote-status '&eval)
				 (%push (eval1 (car argl)))
			       (%push (car argl))))
			   (%activate-open-call-block)))
			(macro (eval1 (error-restart (error "Retry macro expansion.")
					(automatic-displace (cdr fctn) form))
				      t))
			((curry-before curry-after)
			 (if *applyhook*
			     (progn (%open-call-block 'applyhook1 0 4)
				    (%push (if closure-passed (car form) fctn)))
			   (%open-call-block (if closure-passed (car form) fctn) 0 4))
			 (%assure-pdl-room (length (cdr form)))
			 (do ((argl (cdr form) (cdr argl))
			      (argnum 0 (1+ argnum)))
			     ((null argl))
			   (%push (eval1 (car argl))))
			 (%activate-open-call-block))
			(t (if (lambda-macro-call-p fctn)
			       (eval1 (cons (lambda-macro-expand fctn) (cdr form)))
			     (invalid-function form)))))
		    ((or :select-method :instance)
		     (if *applyhook*
			 (progn (%open-call-block 'applyhook1 0 4)
				(%push (if closure-passed (car form) fctn)))
		       (%open-call-block (if closure-passed (car form) fctn) 0 4))
		     (%assure-pdl-room (length (cdr form)))
		     (do ((argl (cdr form) (cdr argl))
			  (argnum 0 (1+ argnum)))
			 ((null argl))
		       (%push (eval1 (car argl))))
		     (%activate-open-call-block))
		    (t (invalid-function form))))
	       ;; FEF (or ucode entry that's microcoded or a FEF).
	       ;; Open call block accordingly to whether there's a quoted rest arg.
	       ;; Also, if more than 64 args to fn taking evaled rest arg,
	       ;; we must make an explicit rest arg to avoid lossage.
	       ;; LIST, etc., may not be called directly because the ucode versions
	       ;; do not deal with explicitly passed rest arguments.
	       (and list-etc-function-mappings
		    (memq fctn list-etc-functions)
		    (setq fctn (fsymeval
				 (cdr (assq fctn list-etc-function-mappings)))
			  arg-desc (%args-info fctn)))
	       (if (or (and (bit-test %arg-desc-quoted-rest arg-desc)
			    (> (length (cdr form)) (ldb %%arg-desc-max-args arg-desc)))
		       (and (bit-test %arg-desc-evaled-rest arg-desc)
			    (> (length (cdr form)) 63.)))
		   (progn ;; NUM-ARGS includes only the spread args.
			  (setq num-args (ldb %%arg-desc-max-args arg-desc))
			  (if *applyhook*
			      (progn (%open-call-block 'applyhook2 0 4)
				     (%push (if closure-passed (car form) fctn)))
			    ;; ADI for fexpr-call.
			    (%push 0)
			    (%push 14000000)
			    (%open-call-block (if closure-passed (car form) fctn) 1 4))
			  ;; We need room for the spread args, plus one word for the rest arg.
			  (%assure-pdl-room (1+ num-args)))
		 (setq num-args (length (cdr form)))
		 (if *applyhook*
		     (progn (%open-call-block 'applyhook1 0 4)
			    (%push (if closure-passed (car form) fctn)))
		   (%open-call-block (if closure-passed (car form) fctn) 0 4))
		 (%assure-pdl-room num-args))
	       ;; If some spread args are quoted, use the ADL to tell which.
	       (cond ((bit-test %arg-desc-fef-quote-hair arg-desc)
		      ;; Get the ADL pointer.
		      (let ((adl (get-macro-arg-desc-pointer fctn)))
			(do ((argl (cdr form) (cdr argl))
			     (argnum 0 (1+ argnum)))
			    ((= argnum num-args)
			     ;; Done with spread args => push rest arg if any.
			     (when argl
			       (%push
				 (if (bit-test %arg-desc-evaled-rest arg-desc)
				     (mapcar 'eval1 argl)
				   argl))))
			  (let ((item (or (car adl) fef-qt-eval)))
			    ;; Figure out how many extra words of ADL to skip for this arg.
			    (if (bit-test %fef-name-present item) (pop adl))
			    (selector (logand %fef-arg-syntax item) =
			      (fef-arg-opt
			       (if (memq (logand item %fef-init-option)
					 '(#.fef-ini-pntr #.fef-ini-c-pntr
					   #.fef-ini-opt-sa #.fef-ini-eff-adr))
				   (pop adl)))
			      (fef-arg-req)
			      ;; Note: does not get here for quoted rest arg.
			      ;; Gets here for evalled rest arg, or if no more args wanted
			      ;; (eval extra args supplied here; get error later).
			      (t (setq adl nil)))
			    (pop adl)
			    ;; Eval the arg if the ADL says to do so.
			    (%push (if ( (logand %fef-quote-status item) fef-qt-qt)
				       (eval1 (car argl))
				     (car argl)))))))
		     (t
		      ;; No quoted args except possibly the rest arg.  Don't look at ADL.
		      (do ((argnum 0 (1+ argnum))
			   (argl (cdr form) (cdr argl)))
			  ((= argnum num-args)
			   ;; Done with spread args => push the rest arg.
			   (when argl
			     (%push
			       (if (bit-test %arg-desc-evaled-rest arg-desc)
				   (mapcar 'eval1 argl)
				 argl))))
			(%push (eval1 (car argl))))))
	       (%activate-open-call-block))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
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
	      ((memq (car symbols-and-values) '(t nil))
	       (ferror nil "Setting ~S is not allowed." (car symbols-and-values))))
	(if (eq interpreter-function-environment t)
	    (set (car symbols-and-values)
		 (setq val (eval (cadr symbols-and-values))))
	  (interpreter-set (car symbols-and-values)
			   (setq val (eval1 (cadr symbols-and-values)))))
	(setq symbols-and-values (cddr symbols-and-values))
	(go l)))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
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
      
      ;;Now execute the body.
      (tagbody-internal body)
      
      ;; Here after finishin the body to step the DO-variables.
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

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN EVAL-ABORT-TRIVIAL-ERRORS (TOP-LEVEL-FORM)
  "Evaluate TOP-LEVEL-FORM, returning the value, but aborting on trivial errors.
A trivial error is one involving a symbol present in the form itself.
Aborting is done by signaling SYS:ABORT, like the Abort key.
The user gets to choose whether to do that or to enter the debugger as usual.
Evaluation uses Common Lisp semantics iff *DEFAULT-COMMON-LISP* is non-NIL."
  (DECLARE (SPECIAL TOP-LEVEL-FORM))
  (CONDITION-BIND (((SYS:TOO-FEW-ARGUMENTS SYS:TOO-MANY-ARGUMENTS
		     SYS:CELL-CONTENTS-ERROR SYS:WRONG-TYPE-ARGUMENT
		     SYS:INVALID-FUNCTION-SPEC SYS:UNCLAIMED-MESSAGE)
		    'EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))
    (LET ((INTERPRETER-ENVIRONMENT
	    (NOT (NOT *DEFAULT-COMMON-LISP*)))
	  (INTERPRETER-FUNCTION-ENVIRONMENT
	    (NOT *DEFAULT-COMMON-LISP*)))
      (EVAL1 TOP-LEVEL-FORM))))


))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun multiple-value (&quote var-list exp)
  "Evaluate EXP, collecting multiple values, and set the variables in VAR-LIST to them.
Returns the first value of EXP."
  (let ((val-list (multiple-value-list (eval1 exp))))
    (do ((vars var-list (cdr vars))
	 (vals val-list (cdr vals)))
	((null vars))
      (when (car vars)
	(if (eq interpreter-function-environment t)
	    (set (car vars) (car vals))
	  (interpreter-set (car vars) (car vals)))))
    (car val-list)))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
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
  (check-arg blockname symbolp "a symbol")
  (let ((values (cond ((or (null vals) (cdr vals))
		       (mapcar 'eval1 vals))
		      (t (multiple-value-list (eval1 (car vals)))))))
    (do ((tail interpreter-environment (cdr tail)))
	((atom tail))
      (let ((bindframe (car tail)))
	(and (eq (car bindframe) 'block)
	     (eq blockname (car (cadr bindframe)))
	     (*throw (cadr (cadr bindframe))
		     values))))
    (ferror nil "There is no lexically-visible active BLOCK named ~S." blockname)))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
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
	     (*throw (cadr (cadr bindframe))
		     values))))
    (ferror nil "There is no lexically-visible active BLOCK named NIL.")))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
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
	   (*throw (cadr (cadr bindframe))
		   values))))
  (ferror nil "There is no lexically-visible active BLOCK named NIL."))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
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
  (check-arg tag symbolp "a symbol")
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail))
    (let ((bindframe (car tail)))
      (and (eq (car bindframe) 'tagbody)
	   (setq tem (memq tag (car (cadr bindframe))))
	   (*throw (cadr (cadr bindframe)) tem))))
  (ferror nil "Unseen GO tag ~S." tag))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun unstackify (env &aux (newenv env))
  (when (consp env)
    (when (stack-list-p env)
      (setq newenv (cons (car env) (cdr env)))
      (%p-dpb-offset dtp-one-q-forward %%q-data-type env 1)
      (%p-dpb-offset (1+ (%pointer newenv)) %%q-pointer env 1)
      (%p-dpb dtp-one-q-forward %%q-data-type env)
      (%p-dpb newenv %%q-pointer env))
    (let* ((frame (car newenv))
	   (newframe frame))
      (when (stack-list-p frame)
	(setq newframe (make-list (length frame)))
	;; Copy each word of the old frame to the new, then
	;; forward each word of the old frame to the new.
	;; Uses %BLT to copy in case what's there is a DTP-ONE-Q-FORWARD.
	(do ((l newframe (cdr l))
	     (m frame (cdr m)))
	    ((null l))
	  (%blt-typed m l 1 0)
	  (%p-store-pointer m l)
	  (%p-store-data-type m dtp-one-q-forward))
	(setf (car newenv) newframe)))
    (when (cdr newenv)
      (let ((newrest (unstackify (cdr newenv))))
	(unless (eq (cdr newenv) newrest)
	  (setf (cdr newenv) newrest))))
    newenv))

))
