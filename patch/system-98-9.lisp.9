;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.9
;;; Reason: New PROMPT-AND-READ options; also :EVAL-FORM-OR-END now returns :END rather than #\END.
;;; READLINE, READLINE-TRIM, READLINE-OR-NIL second value.  NULL-STREAM end of file.
;;; Dwimify of method function specs.
;;; Error handler:  PRINT-CAREFULLY and callers.  Use %P-CONTENTS-SAFE-P and %P-POINTERP.
;;;  SETF of EH-ARG, etc.  SYS:PACKAGE-NOT-FOUND errors.
;;;  Disk full recovery.  Pathname parse error recovery. Non-chaos host recovery.
;;;  Allow :PROCEED methods to return NIL.  Bug in BREAK condition.
;;; :HOMEDIR op on editor pathnames.  Interpreted MULTIPLE-VALUE-LIST bug.
;;; ZWEI:RELEVANT-FUNCTION-NAME.  COMPILER:LOCKING-RESOURCES-NO-QFASL.
;;; TV:MOUSE-WAIT.  CONDITION-CALL.  PRINT-NOTIFICATIONS.  PARSE-UNIVERSAL-TIME.
;;; Answer S to MAKE-SYSTEM queries.  DESCRIBE-SYSTEM improved.
;;; Bug in circular structure printing.
;;; Written 12/13/83 23:29:06 by RMS,
;;; while running on Lisp Machine Nine from band 4
;;; with Bad Inconsistently updated System 98.8, CADR 3.1, Experimental ZMail 53.3, MIT-Specific 22.0, microcode 305, ZM MIT.


(globalize "PACKAGE-ERROR" "SYSTEM")
(globalize "PACKAGE-NOT-FOUND" "SYSTEM")
(globalize "PACKAGE-NOT-FOUND-1" "SYSTEM")

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-RECORD-OCCURRENCES (OBJECT)
  (WHEN (AND (%POINTERP OBJECT)
	     (OR (NOT (SYMBOLP OBJECT))
		 (NOT (SYMBOL-PACKAGE OBJECT)))
	     (NOT (SEND PRINT-HASH-TABLE ':MODIFY-HASH OBJECT
			#'(LAMBDA (OBJECT VALUE FOUND-P)
			    OBJECT VALUE
			    FOUND-P))))
    (TYPECASE OBJECT
      (LIST
	(DO ((TAIL OBJECT (CDR TAIL))
	     (FIRST T NIL))
	    ((ATOM TAIL)
	     (WHEN TAIL (PRINT-RECORD-OCCURRENCES TAIL)))
	  (UNLESS FIRST
	    (IF (SEND PRINT-HASH-TABLE ':MODIFY-HASH TAIL
		      #'(LAMBDA (OBJECT VALUE FOUND-P)
			  OBJECT VALUE
			  FOUND-P))
		(RETURN)))
	  (PRINT-RECORD-OCCURRENCES (CAR TAIL))))
      (ARRAY
       (LET (TEM)
	 (UNLESS (IF (SETQ TEM (NAMED-STRUCTURE-P OBJECT))
		     (AND (SETQ TEM (GET TEM 'NAMED-STRUCTURE-INVOKE))
			  (MEMQ ':PRINT-SELF (FUNCALL TEM ':WHICH-OPERATIONS)))
		   (NULL *PRINT-ARRAY*))
	   (DOTIMES (I (ARRAY-LENGTH OBJECT))
	     (PRINT-RECORD-OCCURRENCES (AR-1-FORCE OBJECT I)))))))))

))

; From file DEFS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFUN (:PROPERTY COMTAB NAMED-STRUCTURE-INVOKE) (OP &OPTIONAL SELF STREAM DEPTH SLASHIFY)
  DEPTH
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:PRINT-SELF))
    (:PRINT-SELF
     (IF SLASHIFY (FORMAT STREAM "#<COMTAB ~S ~O>" (COMTAB-NAME SELF) (%POINTER SELF))
       (PRINC (COMTAB-NAME SELF) STREAM)))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMETHOD (BREAK :CASE :PROCEED-ASKING-USER :NO-ACTION) (CONTINUATION IGNORE)
  "Proceeds."
  (FORMAT T " Continue from break.~%")
  (FUNCALL CONTINUATION ':NO-ACTION))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMETHOD (CONDITION :PROCEED-ASKING-USER) (PROCEED-TYPE CONTINUATION READ-OBJECT-FUNCTION)
  READ-OBJECT-FUNCTION
  (IF (SEND SELF ':PROCEED ':OPERATION-HANDLED-P PROCEED-TYPE)
      (LET ((VALUES (MULTIPLE-VALUE-LIST
		      (SEND SELF ':PROCEED PROCEED-TYPE))))
	(IF (CAR VALUES)
	    ;; If :PROCEED handler returns NIL, don't really proceed.
	    (APPLY CONTINUATION VALUES)))
    (FUNCALL CONTINUATION PROCEED-TYPE)))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SI:CATCH-ERROR-RESTART-THROW (IGNORE TAG)
  (*THROW TAG NIL))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFMACRO PRINT-CAREFULLY (TYPE &BODY BODY)
  "If BODY gets an error, print a message saying /"error printing/".
TYPE is used in that message if it has to be printed."
  `(CONDITION-BIND ((ERROR 'PROCEED-WITH-ABORT-PRINTING))
     . ,BODY))

(DEFUN PROCEED-WITH-ABORT-PRINTING (CONDITION)
  (WHEN (MEMQ ':ABORT-PRINTING (SEND CONDITION ':PROCEED-TYPES))
    ':ABORT-PRINTING))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN RUN-SG (SG &AUX RESULT INNER-TRAP-ON-CALL)
  "Activate a call block in stack group SG, made with SG-OPEN-CALL-BLOCK.
The FUNCTION with which the call block was made should call the error
handler stack group with one argument.  That argument is returned from RUN-SG.
If the value is EH:LOSE, we throw to EH:QUIT.
Provide this stack group as an argument to the function to be run
so it knows who to call back."
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG)	;Terminate arg list assumed there
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)
  (SETF (SG-FLAGS-MAR-MODE SG) 0)			;Turn off the MAR (why??)
  (ASSURE-FREE-SPACE)
  (SETQ RESULT (STACK-GROUP-RESUME SG NIL))
  (SETQ INNER-TRAP-ON-CALL
	(SG-FLAGS-TRAP-ON-CALL SG))
  (SG-RESTORE-STATE SG)
  ;; If the guy set trap-on-call before returning, leave it on.
  (IF (NOT (ZEROP INNER-TRAP-ON-CALL))
      (SETF (SG-FLAGS-TRAP-ON-CALL SG) 1))
  (COND ((AND ERROR-HANDLER-RUNNING ERROR-HANDLER-REPRINT-ERROR)
	 (COND ((NEQ CURRENT-STACK-GROUP LAST-SECOND-LEVEL-ERROR-HANDLER-SG)
		(FORMAT T "~%Back to ")
		(PRINT-CAREFULLY "error message"
		  (SEND EH-ERROR ':PRINT-ERROR-MESSAGE SG T STANDARD-OUTPUT))
		(WARN-ABOUT-SPECIAL-VARIABLES SG)))
	 (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)))
  (COND ((EQ RESULT 'LOSE)
	 (*THROW 'QUIT NIL)))
  RESULT)

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN DEBUG (&OPTIONAL PROCESS
	   &AUX PKG SG ARREST-REASON
	   CURRENT-FRAME INNERMOST-VISIBLE-FRAME ERROR-LOCUS-FRAME
	   INNERMOST-FRAME-IS-INTERESTING
	   EH-ERROR
	   (ERROR-HANDLER-RUNNING NIL))
  "Invoke the debugger to look at a process, window or stack group.
Supplying NIL means find a process which is waiting to be looked at for an error.
If the process is waiting to be looked at for an error,
the debugger is run in that process using our TERMINAL-IO for i//o.
Otherwise, if the process is active, it is forced to get an error
and the debugger uses our TERMINAL-IO.
Otherwise the debugger runs in the current process, not the process being examined,
and that process is arrested for the duration."
  ;; ERROR-HANDLER-RUNNING is NOT set.
  ;; The catch tag EXIT is used to return from EH.
  ;; If arg is a window or stream, extract process from it.
  (OR (NULL PROCESS) (TYPEP PROCESS ':STACK-GROUP) (TYPEP PROCESS 'SI:PROCESS)
      (SETQ PROCESS (FUNCALL PROCESS ':PROCESS)))
  (IF (OR (NULL PROCESS)
	  (AWAITING-BACKGROUND-ERROR-P PROCESS))
      (HANDLE-BACKGROUND-ERROR PROCESS)
    ;; If arg is an active non-erring process, make it get an error.
    (IF (AND (TYPEP PROCESS 'SI:PROCESS)
	     (SEND PROCESS ':ACTIVE-P)
	     (NOT (SYMEVAL-IN-STACK-GROUP 'ERROR-HANDLER-RUNNING
					  (PROCESS-STACK-GROUP PROCESS))))
	(LET ((CELL (LIST NIL)))
	  (SEND PROCESS ':INTERRUPT 'INVOKE-DEBUGGER-FOR-EH TERMINAL-IO CELL)
	  (PROCESS-WAIT "Background error" 'CAR CELL))
      ;; If arg is process or was converted to one, stop it.
      (COND ((TYPEP PROCESS 'SI:PROCESS)
	     (FUNCALL PROCESS ':ARREST-REASON CURRENT-PROCESS)
	     (SETQ ARREST-REASON CURRENT-PROCESS)
	     (SETQ SG (PROCESS-STACK-GROUP PROCESS)))
	    (T (SETQ SG PROCESS PROCESS NIL)))
      (OR (TYPEP SG ':STACK-GROUP) (FERROR NIL "~S not a stack group" SG))
      (SETQ INNERMOST-VISIBLE-FRAME (SG-AP SG))
      (SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG INNERMOST-VISIBLE-FRAME))
      (SETQ ERROR-LOCUS-FRAME CURRENT-FRAME)
      ;; Although we get the package each time around the r-e-p loop, we must get it
      ;; here as well, so that when the error message is printed it will be in the
      ;; right package.
      (SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE SG))
      (UNWIND-PROTECT
	(PROGN
	  (*CATCH 'QUIT
	    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit the debugger.")
	      (PKG-BIND (IF (EQ (TYPEP PKG) 'PACKAGE) PKG "USER")
		(PRINT-CAREFULLY "frame"
		  (FORMAT T "~&~S~%Backtrace: " SG)
		  (SHORT-BACKTRACE SG NIL 3)))))
	  (FORMAT T "~&Note: running in process ~A, not the one being debugged.
Type ~C to exit the debugger." (PROCESS-NAME CURRENT-PROCESS) #\RESUME)
	  (*CATCH 'EXIT (COMMAND-LOOP SG (SETQ EH-ERROR (SG-TRAP-TAG SG)))))
	(AND ARREST-REASON (FUNCALL PROCESS ':REVOKE-ARREST-REASON ARREST-REASON))))))

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
				   (TERMINAL-IO (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						  'DEBUG-IO SG))
				   (STANDARD-INPUT SI:SYN-TERMINAL-IO)
				   (STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
				   (QUERY-IO SI:SYN-TERMINAL-IO)
				   (DEBUG-IO SI:SYN-TERMINAL-IO)
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

(DEFMETHOD (CELL-CONTENTS-ERROR :REPORT) (STREAM &AUX CONTENTS-CHANGED VERB)
  (SETQ CONTENTS-CHANGED
	(OR ( (%P-DATA-TYPE CURRENT-ADDRESS)
	       DATA-TYPE)
	    ( (%P-POINTER CURRENT-ADDRESS)
	       POINTER)))
  (SETQ VERB (IF CONTENTS-CHANGED "was" "is"))
  (SELECTQ CELL-TYPE
    (:VALUE (FORMAT STREAM "The variable ~S ~A unbound." SYMBOL VERB))
    (:FUNCTION (FORMAT STREAM "The function ~S ~A undefined." SYMBOL VERB))
    (:CLOSURE (IF (TYPEP CONTAINING-STRUCTURE ':INSTANCE)
		  (FORMAT STREAM "The instance variable ~S ~A unbound in ~S."
			  SYMBOL VERB CONTAINING-STRUCTURE)
		(FORMAT STREAM "The variable ~S ~A unbound (in a closure value-cell)."
			SYMBOL VERB)))
    (OTHERWISE
     (FORMAT STREAM "The word #<~S ~S> was read from location ~O ~@[(in ~A)~]."
	     (Q-DATA-TYPES DATA-TYPE)
	     POINTER
	     (%POINTER ADDRESS)
	     (LET ((AREA (%AREA-NUMBER ADDRESS)))
	       (AND AREA (AREA-NAME AREA)))))))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN PRINT-FUNCTION-AND-ARGS (SG FRAME
				&AUX FUNCTION FUNCTION-NAME (RP (SG-REGULAR-PDL SG))
				(PRINLEVEL FUNCTION-PRINLEVEL)
				(PRINLENGTH FUNCTION-PRINLENGTH))
  "Print the function called in FRAME, and the arguments it has."
  (SETQ FUNCTION (RP-FUNCTION-WORD RP FRAME))
  (SETQ FUNCTION-NAME (FUNCTION-NAME FUNCTION))
  (CATCH-ERROR (FORMAT T "~%~S:" FUNCTION-NAME) NIL)
  (AND (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
       (FORMAT T " (P.C. = ~O)"		;Note that this displays the return-pc,
	       (RP-EXIT-PC RP FRAME)))	; which is one greater than the D-LAST.  
  (AND (LISTP FUNCTION-NAME) (EQ (CAR FUNCTION-NAME) ':METHOD)
       (PRINT-CAREFULLY "self"
	 (FORMAT T "~%   (SELF is ~S)" (SYMEVAL-IN-STACK-GROUP 'SELF SG FRAME))))
  (TERPRI)
  (PRINT-FRAME-ARGS SG FRAME 3))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-CLEAR-AND-SHOW (SG ERROR-OBJECT &REST IGNORE)
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  (PRINT-CAREFULLY "error message"
    (SEND ERROR-OBJECT ':PRINT-ERROR-MESSAGE SG NIL STANDARD-OUTPUT))
  (SHOW-FUNCTION-AND-ARGS SG)
  (WARN-ABOUT-SPECIAL-VARIABLES SG)
  (DESCRIBE-PROCEED-TYPES SG ERROR-OBJECT)
  (UNLESS ERROR-HANDLER-RUNNING
    (FORMAT T "~2&Examine-only mode; you cannot resume or alter execution.
Type ~C to exit the debugger.~%" #\RESUME))
  NIL)

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN SHOW-ALL-MACRO (SG FRAME &OPTIONAL NO-DISASSEMBLED-CODE
		       &AUX N-LOCALS PC-NOW NAME REST-ARG-PRINTED NLINES WHERE LIM-PC
		       (RP (SG-REGULAR-PDL SG))
		       (FUNCTION (RP-FUNCTION-WORD RP FRAME)))
  (SETQ N-LOCALS (FEF-NUMBER-OF-LOCALS FUNCTION)
	NAME (FEF-NAME FUNCTION)
	PC-NOW (RP-EXIT-PC RP FRAME)
	LIM-PC (COMPILER:DISASSEMBLE-LIM-PC FUNCTION))
  (FORMAT T "~%~s (P.C. = ~O)~%" NAME PC-NOW)
  (WHEN (AND (LISTP NAME) (EQ (CAR NAME) ':METHOD))
    (FORMAT T "  (SELF is ")
    (PRINT-CAREFULLY "SELF"
      (PRIN1 (SYMEVAL-IN-STACK-GROUP 'SELF SG FRAME)))
    (FORMAT T ")~%"))
  (TERPRI)
  ;; Print the arguments, including the rest-arg which is the first local
  (SETQ REST-ARG-PRINTED (PRINT-FRAME-ARGS SG FRAME 0))
  (COND ((SG-FRAME-ACTIVE-P SG FRAME)
	 ;; Print the rest of the locals -- if the frame is active.
	 (DOTIMES (I N-LOCALS)
	   (COND ((NOT (AND REST-ARG-PRINTED (ZEROP I)))	;Don't show rest arg twice
		  (FORMAT T "Local ~D" I)
		  (DISPLAY-LOCAL-NAME " (~A)" FUNCTION I)
		  (LET ((PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
			(PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
		    (FORMAT T ": ")
		    (PRINT-CAREFULLY "local"
		      (PRIN1 (SG-FRAME-LOCAL-VALUE SG FRAME I)))
		    (TERPRI)))))
	 (UNLESS NO-DISASSEMBLED-CODE
	   (FORMAT T "~%Disassembled code:")
	   ;; Figure out how many instructions will fit in the stream we are using.
	   (SETQ NLINES
		 (MAX DISASSEMBLE-INSTRUCTION-COUNT	;don't show absurdly few
		      (COND ((MEMQ ':SIZE-IN-CHARACTERS (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
			     (MULTIPLE-VALUE (NIL NLINES)
			       (FUNCALL STANDARD-OUTPUT ':SIZE-IN-CHARACTERS))
			     (MULTIPLE-VALUE (NIL WHERE)
			       (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':CHARACTER))
			     (- NLINES WHERE 2))	;Leave 1 line for prompt, 1 for extra terpri
			    (T 0))))		;Don't know size of window, use default count
	   (DO ((I 0 (1+ I))
		(PC (MAX (FEF-INITIAL-PC FUNCTION) (- PC-NOW (TRUNCATE NLINES 2)))
		    (+ PC (COMPILER:DISASSEMBLE-INSTRUCTION-LENGTH FUNCTION PC))))
	       ((OR ( I NLINES) ( PC LIM-PC))
		(COND ((= PC PC-NOW)		;If arrow should point after all code,
		       (TERPRI) (PRINC "=> "))))
	     (TERPRI)
	     (PRINC (IF (= PC PC-NOW) "=> " "   "))
	     (COMPILER:DISASSEMBLE-INSTRUCTION FUNCTION PC))
	   ;; This kludge is to prevent the prompt from triggering a **MORE** when it comes out
	   ;; on the bottom line of the window
	   (IF (MEMQ ':NOTICE (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
	       (FUNCALL STANDARD-OUTPUT ':NOTICE ':INPUT-WAIT))))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN P-PRIN1-CAREFUL (LOCATIVE &AUX)
  "Print the contents of LOCATIVE, catching and reporting errors in printing."
  (COND ((%P-CONTENTS-SAFE-P LOCATIVE)
	 (PRINT-CAREFULLY "printing" (PRIN1 (CAR LOCATIVE))))
	(T (FORMAT T "#<~A ~O>" (%P-DATA-TYPE LOCATIVE) (%P-POINTER LOCATIVE)))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMETHOD (CELL-CONTENTS-ERROR :CASE :PROCEED-ASKING-USER :NO-ACTION)
	   (CONTINUATION READ-OBJECT-FUNCTION)
  "Proceeds, using current contents if legal, or reading replacement value."
  (COND ((NOT (%P-CONTENTS-SAFE-P CURRENT-ADDRESS))
	 ;; Location still contains garbage, get a replacement value.
	 (FUNCALL SELF ':PROCEED-ASKING-USER ':NEW-VALUE
		  CONTINUATION READ-OBJECT-FUNCTION))
	(T (FUNCALL CONTINUATION ':NO-ACTION))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN CELL-CONTENTS-ERROR-PRINT-NEW-CONTENTS-1 (STREAM CELL-DESCRIPTION ADDRESS)
  (IF (%P-CONTENTS-SAFE-P ADDRESS)
      (FORMAT STREAM "~A ~S.~%" CELL-DESCRIPTION (%P-CONTENTS-OFFSET ADDRESS 0))
    (FORMAT STREAM "~A #<~S ~S>.~%" CELL-DESCRIPTION
	    (Q-DATA-TYPES (%P-DATA-TYPE ADDRESS))
	    (%P-POINTER ADDRESS))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN SG-POINTER-CAREFUL (SG REGISTER)
  (LET* ((LOCATION (SG-LOCATE SG REGISTER)))
    (COND ((%P-CONTENTS-SAFE-P LOCATION)
	   (CONTENTS LOCATION))
	  ((%P-POINTERP LOCATION)
	   (%P-CONTENTS-AS-LOCATIVE LOCATION))
	  (T (%P-POINTER LOCATION)))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SG-SAVE-STATE (SG &OPTIONAL SUPPRESS-PDL-GROWING &AUX P NEW-AP RP PP)
  "Save the state (accumulators, etc) of stack group SG on its stack.
SUPPRESS-PDL-GROWING inhibits making the stack bigger heuristically.
Once this function has been called, you can then push and invoke a call block
to cause the stack group to execute things without clobbering the saved
accumulators from the error."
  (OR SUPPRESS-PDL-GROWING (SG-MAYBE-GROW-PDLS SG))	;Make sure there is room to do this
  (SETQ RP (SG-REGULAR-PDL SG)
	PP (SG-REGULAR-PDL-POINTER SG))
  (SETQ NEW-AP (+ PP %LP-CALL-BLOCK-LENGTH))
  (ASET (DPB (- NEW-AP (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
	     (DPB (- NEW-AP (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
		  0))
	RP (+ NEW-AP %LP-CALL-STATE))
  (ASET (DPB (FEF-INITIAL-PC #'FOOTHOLD) %%LP-EXS-EXIT-PC 0)
	RP (+ NEW-AP %LP-EXIT-STATE))
  (ASET 0 RP (+ NEW-AP %LP-ENTRY-STATE))
  (ASET #'FOOTHOLD RP (+ NEW-AP %LP-FEF))
  (SETQ PP (1+ NEW-AP))
  (DO I 0 (1+ I) (> I SG-PDL-PHASE)
      (SETQ P (AP-LEADER SG I))
      (ASET (IF (%P-POINTERP P)
		(%P-CONTENTS-AS-LOCATIVE P)
	      (%P-POINTER P))
	    RP PP)
      (ASET (%P-LDB %%Q-ALL-BUT-POINTER P)
	    RP (1+ PP))
      (SETQ PP (+ PP 2)))
  (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))	;Index of last valid word
  (SETF (SG-FLAGS-QBBFL SG) 0)			;Clear QBBFL left over from previous frame
  (SETF (SG-IPMARK SG) NEW-AP)
  (SETF (SG-AP SG) NEW-AP))

))

; From file HOST.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HOST  "

(defmethod (ed-basic-pathname :HOMEDIR) (user)
  user
  (make-pathname ':host fs:host ':directory ':unspecific ':device ':unspecific))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFMETHOD (FILE-OPERATION-FAILURE :CASE :PROCEED-ASKING-USER :EXPUNGE-DIRECTORY) (&REST IGNORE)
  "Expunges the directory, then returns to debugger."
  (EXPUNGE-DIRECTORY (SEND (SEND SELF ':PATHNAME) ':NEW-PATHNAME
			   ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
  NIL)

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun multiple-value-list (&quote exp)
  "Evaluate the expression EXP and return a list of the values it returns."
  (multiple-value-list (eval1 exp)))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO CONDITION-CALL (VARIABLES BODY-FORM &REST CLAUSES)
  "Execute BODY-FORM with conditions handled according to CLAUSES.
Each element of CLAUSES is a clause like those used in COND.
This virtual COND is executed whenever a condition is signaled within BODY-FORM.
If the predicate at the start of a clause evaluates to non-NIL,
the rest of the clause is used to handle the condition.
The values of the last form in the clause are returned from CONDITION-CALL.
The predicate, and the rest of the clause, can find the condition object
that was signaled in the value of the first VARIABLE.

If no predicate evaluates to non-NIL, the condition is not handled
at this level.  Previously established handlers then get a chance.

The predicates may be evaluated more than once, and should have no side-effects.
They are evaluated within the context where the condition was signaled
and are evaluated again after throwing back to this level.
The rest of the clause is evaluated only after throwing back to this level.

The values of BODY-FORM are returned from the CONDITION-CALL if condition
handling does not cause something else to happen.  However, if there is
a :NO-ERROR clause (a clause whose first element is :NO-ERROR) then it
is executed and its values are returned from the CONDITION-CALL.
In this clause, the VARIABLES are bound to the values of the BODY-FORM."
  ;; We don't use &BODY in the real arglist to avoid overriding
  ;; the special form of indentation on *INITIAL-LISP-INDENT-OFFSET-ALIST*
  (DECLARE (ARGLIST VARIABLES BODY-FORM &BODY CLAUSES))
  `(CONDITION-CALL-IF T ,VARIABLES ,BODY-FORM . ,CLAUSES))

(DEFMACRO CONDITION-CALL-IF (COND-FORM VARIABLES BODY-FORM &REST CLAUSES)
  "Like CONDITION-CALL, but establishes the handlers only if COND-FORM evaluates non-NIL.
See the documentation of CONDITION-CALL for more information."
  ;; We don't use &BODY in the real arglist to avoid overriding
  ;; the special form of indentation on *INITIAL-LISP-INDENT-OFFSET-ALIST*
  (DECLARE (ARGLIST COND-FORM (VARIABLE) BODY-FORM &BODY CLAUSES))
  (LET* ((ORDINARY-CLAUSES (SUBSET #'(LAMBDA (CLAUSE) (NEQ (CAR CLAUSE) ':NO-ERROR))
				   CLAUSES))
	 (NO-ERROR-CLAUSE (ASSQ ':NO-ERROR CLAUSES))
	 (PREDICATES
	   (MAPCAR 'CAR ORDINARY-CLAUSES))
	 (VAR (OR (CAR VARIABLES) (GENSYM)))
	 (TAG (GENSYM))
	 (HANDLER `#'(LAMBDA (,VAR &REST IGNORE)
		       (IF (OR . ,PREDICATES)
			   (*THROW ',TAG ,VAR)))))
    `(CATCH-CONTINUATION-IF T ',TAG
			    #'(LAMBDA (,VAR)
				(COND . ,ORDINARY-CLAUSES))
			    ,(IF NO-ERROR-CLAUSE
				 `#'(LAMBDA ,VARIABLES . ,(CDR NO-ERROR-CLAUSE)))
       (CONDITION-BIND-IF ,COND-FORM ((NIL ,HANDLER)) ,BODY-FORM))))

))

; From file MOUSE.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFUN MOUSE-WAIT (&OPTIONAL (OLD-X MOUSE-X) (OLD-Y MOUSE-Y) (OLD-BUTTONS MOUSE-LAST-BUTTONS)
		   (WHOSTATE "MOUSE"))
  "Wait for the mouse to move or a button transition.  For processes other than MOUSE-PROCESS.
If the arguments are supplied, we wait for the mouse state to be
different from them.  To avoid lossage, save the values of
MOUSE-X and MOUSE-Y and MOUSE-LAST-BUTTONS, use the saved values,
then pass the saved values to this function.
WHOSTATE is displayed in the who line while we wait."
  (PROCESS-WAIT WHOSTATE
    (FUNCTION (LAMBDA (OLD-X OLD-Y OLD-BUTTONS)
		(OR ( MOUSE-X OLD-X)
		    ( MOUSE-Y OLD-Y)
		    ( MOUSE-LAST-BUTTONS OLD-BUTTONS))))
    OLD-X OLD-Y OLD-BUTTONS))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN PRINT-NOTIFICATIONS (&OPTIONAL (FROM 0) TO)
  "Reprint all notifications that have happened, newest first.
If FROM is nonzero, that many of the most recent notificatiosn are skipped.
If TO is non-NIL, we stop after printing the TO'th most recent notification."
  (FORMAT T "~&~:[No notifications.~;Notifications, most recent first:~]~%"
	    NOTIFICATION-HISTORY)
  (DO ((NLIST NOTIFICATION-HISTORY (CDR NLIST))
       (I 0 (1+ I)))
      ((OR (NULL NLIST)
	   (AND TO (>= I TO))))
    (UNLESS (< I FROM)
      (LET ((N (CAR NLIST)))
	(TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST N))
	(FORMAT T " ~A~%" (SECOND N))))))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN QUERY-USER-LIST ()
  (DO ((FILE-TRANSFORMATION-LIST *FILE-TRANSFORMATION-LIST* (CDR FILE-TRANSFORMATION-LIST))
       (TYPES-FOUND NIL)
       (N-FOUND 0)
       (LAST-TRANSFORMATION NIL) (LAST-TYPE T)
       (TRANSFORMATION) (TRANSFORMATION-TYPE NIL)
       (FIRST-P T))
      (NIL)
    (SETQ TRANSFORMATION-TYPE
	  (AND (NOT (NULL FILE-TRANSFORMATION-LIST))
	       (FILE-TRANSFORMATION-TRANSFORMATION-TYPE
		 (SETQ TRANSFORMATION (CAR FILE-TRANSFORMATION-LIST)))))
    (COND ((OR (NULL TRANSFORMATION-TYPE)
	       (MEMQ (FILE-TRANSFORMATION-STATE TRANSFORMATION) '(:PENDING :PROBABLY)))
	   (COND (LAST-TRANSFORMATION
		  (COND (FIRST-P
			 (FORMAT QUERY-IO "~2&")
			 (COND ((NULL (FILE-TRANSFORMATION-ARGS LAST-TRANSFORMATION))
				(FORMAT QUERY-IO "Going to ~\FILE-XFORM-ARGS\"
					LAST-TRANSFORMATION)
				(RPLACA TYPES-FOUND
					(SETQ LAST-TYPE (NCONS LAST-TRANSFORMATION))))
			       (T
				(FORMAT QUERY-IO "~2&File~:[s~] to be ~A:~%"
					(NEQ TRANSFORMATION-TYPE LAST-TYPE)
					(TRANSFORMATION-TYPE-PRETTY-PAST-PARTICIPLE
					  LAST-TYPE))
				(SETQ FIRST-P NIL)))
			 (FUNCALL QUERY-IO ':TYO #\CR)))
		  (AND (FILE-TRANSFORMATION-ARGS LAST-TRANSFORMATION)
		       (FORMAT QUERY-IO "~&~\FILE-XFORM-ARGS\" LAST-TRANSFORMATION))
		  (SETQ N-FOUND (1+ N-FOUND))))
	   (AND (NULL TRANSFORMATION-TYPE)
		(RETURN (AND (PLUSP N-FOUND)
			     (SELECTQ
			       (FQUERY `(:CHOICES
					  (((S "Selective") #/S)
					   . ,FORMAT:Y-OR-N-P-CHOICES))
				       "~2&~\XFORM-TYPES\? "
				       (NREVERSE TYPES-FOUND) N-FOUND)
			       ((T) T)
			       (S (LET ((*QUERY-TYPE* ':SELECTIVE))
				    (REQUERY-SELECTIVE *FILE-TRANSFORMATION-LIST*))
				  (QUERY-USER-LIST))))))
	   (AND (SETQ FIRST-P (NEQ TRANSFORMATION-TYPE LAST-TYPE))
		(PUSH* TRANSFORMATION-TYPE TYPES-FOUND))
	   (SETQ LAST-TRANSFORMATION TRANSFORMATION
		 LAST-TYPE TRANSFORMATION-TYPE)))))

(DEFUN REQUERY-SELECTIVE (TRANSFORMATIONS)
  (DOLIST (TRANS TRANSFORMATIONS)
    (WHEN (MEMQ (FILE-TRANSFORMATION-STATE TRANS) '(:PROBABLY :PENDING))
      (UNLESS (QUERY-USER-SELECTIVE TRANS)
	(SETF (FILE-TRANSFORMATION-STATE TRANS) ':REFUSED)))))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN DESCRIBE-SYSTEM (SYSTEM-NAME &KEY (SHOW-FILES T) (SHOW-TRANSFORMATIONS T) &AUX SYSTEM)
  "Print all about the system named SYSTEM-NAME.
SHOW-FILES is T to give the history of each file in the system, NIL not to,
 or :ASK meaning query the user whether to.
SHOW-TRANSFORMATIONS is similar, for whether to show the transformations
 which MAKE-SYSTEM would execute.
Note that calling DESCRIBE on a system-object prints somewhat lower level information."
  (IF (NULL (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME)))
      (FORMAT T "~&There is no system named ~A.~%" SYSTEM-NAME)
    (SETQ SYSTEM-NAME (SYSTEM-NAME SYSTEM))
    (LET* ((SYSTEM-SOURCE-FILE
	     (GET-SOURCE-FILE-NAME (SYSTEM-SYMBOLIC-NAME SYSTEM) 'DEFSYSTEM))
	   (*FORCE-PACKAGE*
	     (PKG-FIND-PACKAGE (OR (SEND SYSTEM-SOURCE-FILE ':GET ':PACKAGE) "USER"))))
      (WHEN SYSTEM-SOURCE-FILE
	(FORMAT T "~&System ~A~@[ is defined in file ~A~]~%"
		SYSTEM-NAME SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE SYSTEM-SOURCE-FILE)))
    (COND ((SYSTEM-PATCHABLE-P SYSTEM)
	   (FORMAT T "~&~%~A is patchable" SYSTEM-NAME)
	   (MULTIPLE-VALUE-BIND (MAJOR MINOR STATUS)
	       (GET-SYSTEM-VERSION SYSTEM)
	     (LET ((STATUS-NAME (OR (SECOND (ASSQ STATUS SYSTEM-STATUS-ALIST)) STATUS)))
	       (OR (EQUAL STATUS-NAME "")
		   (FORMAT T ", ~A" STATUS-NAME)))
	     (IF MAJOR (FORMAT T ", ~D.~D is loaded" MAJOR MINOR))
	     (FORMAT T ";~%  a typical patch file is ~A~%"
		     (PATCH-SYSTEM-PATHNAME SYSTEM-NAME ':PATCH-FILE (OR MAJOR 1) (OR MINOR 0)
					    ':LISP))
	     (AND MAJOR
		  (FQUERY NIL "Do you want to see the patches for ~A? " SYSTEM-NAME)
		  (PRINT-PATCHES SYSTEM)))))
    (IF (SYSTEM-PACKAGE-DEFAULT SYSTEM)
	(FORMAT T "~& Files in ~A are forcibly read in package ~A.~%"
		SYSTEM-NAME (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
    (WHEN SHOW-FILES
      (FORMAT T "~%Compilation and loading of files in this system:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':RELOAD ':DO-NOT-DO-COMPONENTS
		   ':DESCRIBE ':NO-INCREMENT-PATCH ':NO-RELOAD-SYSTEM-DECLARATION))
    (WHEN SHOW-TRANSFORMATIONS
      (FORMAT T "~%Transformations required to MAKE-SYSTEM now:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':DO-NOT-DO-COMPONENTS ':PRINT-ONLY
		   ':NO-RELOAD-SYSTEM-DECLARATION))
    (LET ((COMPONENTS (SYSTEM-COMPONENT-SYSTEMS SYSTEM)))
      (COND (COMPONENTS
	     (FORMAT T " ~A is made up of component system~P "
		     SYSTEM-NAME (LENGTH COMPONENTS))
	     (FORMAT:PRINT-LIST T "~A" COMPONENTS)
	     (DOLIST (COMPONENT COMPONENTS)
	       (FORMAT T "~2&")
	       (DESCRIBE-SYSTEM COMPONENT ':SHOW-FILES SHOW-FILES
				':SHOW-TRANSFORMATIONS SHOW-TRANSFORMATIONS))))))
  SYSTEM-NAME)

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-RETURN-A-VALUE (SG IGNORE &REST IGNORE &AUX VALUE
			   (FN (FUNCTION-NAME (RP-FUNCTION-WORD (SG-REGULAR-PDL SG)
								CURRENT-FRAME))))
  (COND ((NULL ERROR-HANDLER-RUNNING)
	 (FORMAT T "You can only examine this stack group, not modify it."))
	((NOT (SG-FRAME-ACTIVE-P SG CURRENT-FRAME))
	 (FORMAT T "This frame has not yet been activated; you cannot return from it."))
	((NULL (SG-NEXT-ACTIVE SG CURRENT-FRAME))
	 (FORMAT T "This is the bottom frame; you cannot return from it."))
	(T (MULTIPLE-VALUE-BIND (NIL NUMBER-LOC-OR-NIL) (SG-FRAME-VALUE-LIST SG CURRENT-FRAME)
	     (COND ((NULL NUMBER-LOC-OR-NIL)
		    (FORMAT T "Return a value from the function ~S.~%" FN)
		    (SETQ VALUE (READ-OBJECT ':EVAL-READ "Form to evaluate and return: "))
		    (LEAVING-ERROR-HANDLER)
		    (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) INNERMOST-VISIBLE-FRAME) 0)
		    (SG-UNWIND-TO-FRAME SG CURRENT-FRAME T VALUE))
		   (T
		    (FORMAT T "Return values from the function ~S " FN)
		    (IF (NUMBERP NUMBER-LOC-OR-NIL)
			(FORMAT T "(up to ~D of them)." NUMBER-LOC-OR-NIL)
		      (FORMAT T "(any number of them)."))
		    (LET (ACCUM)
		      (DO ((I 0 (1+ I)))
			  ((EQ I NUMBER-LOC-OR-NIL))
			(MULTIPLE-VALUE-BIND (VALUE FLAG)
			    (READ-OBJECT ':EVAL-READ-OR-END "~&Value ~D~A, or ~C: "
					 I (FORMAT:OUTPUT NIL
					     (DISPLAY-VALUE-NAME " (~A)" FN I))
					 #\END)
			  (IF FLAG (RETURN))
			  (PUSH VALUE ACCUM)))
		      (SG-UNWIND-TO-FRAME-AND-REINVOKE SG CURRENT-FRAME
						       `(VALUES . ,(NREVERSE ACCUM)))
		      (LEAVING-ERROR-HANDLER)
		      (WITHOUT-INTERRUPTS
			(AND ERROR-HANDLER-RUNNING
			     (FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP))
			(STACK-GROUP-RESUME SG NIL))))))))
  NIL)

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-REINVOKE-NEW-ARGS (SG IGNORE &REST IGNORE)
  (COND ((NULL ERROR-HANDLER-RUNNING)
	 (FORMAT T "You can only examine this stack group, not modify it."))
	((NOT (SG-FRAME-ACTIVE-P SG CURRENT-FRAME))
	 (FORMAT T "This frame's args are still being computed;
it cannot be reinvoked since it was never invoked."))
	(T
	 (LET* ((FORM (GET-FRAME-FUNCTION-AND-ARGS SG CURRENT-FRAME))
		(FUNCTION-NAME (CAR FORM))
		(FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME))
		(ARGUMENT-LIST (CDR FORM))
		(NARGS (LENGTH ARGUMENT-LIST))
		(ARGS-INFO (ARGS-INFO FUNCTION))
		(ARGS-WANTED (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO))
		(REST-FLAG (LDB-TEST %%ARG-DESC-ANY-REST ARGS-INFO))
		(MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))
		NEW-ARGS
		(PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
		(PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
	   (FORMAT T "~&Reinvoke ~S with possibly altered arguments." FUNCTION-NAME)
	   (DO ((I 0 (1+ I)))
	       ((UNLESS REST-FLAG (EQ I MAX-ARGS)))
	     (MULTIPLE-VALUE-BIND (VALUE FLAG)
		 (PROMPT-AND-READ
		   (LET ((KEYWORD
			   (IF (>= I ARGS-WANTED)
			       ':EVAL-READ-OR-END ':EVAL-READ)))
		     (IF (< I NARGS)
			 (LIST KEYWORD ':DEFAULT (NTH I ARGUMENT-LIST))
		       KEYWORD))
		   (IF (< I NARGS)
		       (IF (>= I ARGS-WANTED)
			   "~&Arg ~D~A, or ~\lozenged-char\ not to change it, or ~C: "
			 "~&Arg ~D~A, or ~\lozenged-char\ not to change it: ")
		     (IF (>= I ARGS-WANTED)
			 "~&Arg ~D~A, or ~*~C: "
		       "~&Arg ~D~A: "))
		   I
		   (FORMAT:OUTPUT NIL (DISPLAY-ARG-NAME " (~A)" FUNCTION I))
		   #\SPACE #\END)
	       (IF (EQ FLAG ':END) (RETURN))
	       (IF (EQ FLAG ':DEFAULT)
		   (PRIN1 VALUE))
	       (SETQ NEW-ARGS
		     (NCONC NEW-ARGS
			    (NCONS VALUE)))))
	   (SETQ FORM (CONS FUNCTION-NAME NEW-ARGS))
	   (WHEN (FQUERY NIL "Reinvoking ~S, OK? " FORM)
	     (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) INNERMOST-VISIBLE-FRAME) 0)
	     (SG-UNWIND-TO-FRAME-AND-REINVOKE SG CURRENT-FRAME FORM)
	     (LEAVING-ERROR-HANDLER)
	     (WITHOUT-INTERRUPTS
	       (AND ERROR-HANDLER-RUNNING
		    (FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP))
	       (STACK-GROUP-RESUME SG NIL)))))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMETHOD (FUNCTION-ENTRY-ERROR :CASE :PROCEED-ASKING-USER :NEW-ARGUMENT-LIST)
	   (CONTINUATION READ-OBJECT-FUNCTION)
  (LET* ((-FUNCTION- (SEND SELF ':FUNCTION))
	 (-ARGUMENT-LIST- (SEND SELF ':ARGUMENT-LIST))
	 (-NARGS- (SEND SELF ':NARGS))
	 (FORM (CONS -FUNCTION- -ARGUMENT-LIST-))
	 (ARGS-INFO (ARGS-INFO -FUNCTION-))
	 (ARGS-WANTED (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO))
	 (REST-FLAG (LDB-TEST %%ARG-DESC-ANY-REST ARGS-INFO))
	 (MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))
	 NEW-ARGS)
    ;; Function may have been redefined to take the supplied number of arguments
    ;; so don't look at the original error, but check everything again.
    (COND ((< -NARGS- ARGS-WANTED)
	   (DO ((I -NARGS- (1+ I)))
	       ((UNLESS REST-FLAG (EQ I MAX-ARGS)))
	     (MULTIPLE-VALUE-BIND (VALUE FLAG)
		 (FUNCALL READ-OBJECT-FUNCTION
			  (IF (>= I ARGS-WANTED)
			      ':EVAL-READ-OR-END ':EVAL-READ)
			  (IF (>= I ARGS-WANTED)
			      "Arg ~D~A, or ~C: "
			    "Arg ~D~A: ")
			  I
			  (FORMAT:OUTPUT NIL (DISPLAY-ARG-NAME " (~A)" -FUNCTION- I))
			  #\END)
	       (IF FLAG (RETURN))
	       (SETQ NEW-ARGS
		     (NCONC NEW-ARGS
			    (NCONS VALUE)))))
	   (FUNCALL CONTINUATION ':NEW-ARGUMENT-LIST
		    (APPEND (CDR FORM) NEW-ARGS)))
	  ((OR ( -NARGS- MAX-ARGS)
	       (LDB-TEST %%ARG-DESC-ANY-REST ARGS-INFO))
	   (IF (FUNCALL READ-OBJECT-FUNCTION '(:FQUERY) "Try ~S again? " FORM)
	       (FUNCALL CONTINUATION ':NEW-ARGUMENT-LIST (CDR FORM))))
	  ((FUNCALL READ-OBJECT-FUNCTION '(:FQUERY)
		    "Call again with the last ~[~1;~:;~:*~D ~]argument~:P dropped? "
		    (- -NARGS- MAX-ARGS))
	   (FUNCALL CONTINUATION ':NEW-ARGUMENT-LIST
		    (FIRSTN MAX-ARGS (CDR FORM)))))))

))

; From file TIMPAR.LISP SRC:<L.IO1> OZ:
#10R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIMPAR  "

(DEFUN PARSE-UNIVERSAL-TIME (STRING &OPTIONAL (START 0) END (FUTUREP T) BASE-TIME
			                MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
					TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  "Return a universal-time parsed from STRING, or the part from START to END.
FUTUREP controls the interpretation if there is just a day-of-the-week:
 T means use the next such day, NIL means use the previous.
BASE-TIME is used if the string is a relative time.
 It is what the relative time is relative to.  Default is now.
MUST-HAVE-TIME if T means error if the string is empty.
DATE-MUST-HAVE-YEAR if T means error if no year number.
TIME-MUST-HAVE-SECOND if T means error if time doesn't
 include a number of seconds.
DAY-MUST-BE-VALID if NIL means allow things like February 29
 (which equals March 1 or March 2)."
  (DECLARE (RETURN-LIST UNIVERSAL-TIME RELATIVE-P))
  (PROG KLUDGE ()				;This is needed because multiple values
   (IF (AND MUST-HAVE-TIME (EQ STRING ""))
       (FERROR 'PARSE-ERROR "The supplied time string is empty."))
   (IF (NULL END)
       (SETQ END (STRING-LENGTH STRING)))
   (LET ((TEM (PARSE-TWENEX-TIME STRING START END)))
     (IF TEM (RETURN TEM NIL)))
   (LET (*ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*
	 *ABS-DAY-OF-THE-WEEK* *ABS-TIME-ZONE*
	 (*REL-YEAR* 0) (*REL-MONTH* 0) (*REL-DATE* 0) (*REL-HOUR* 0) (*REL-MINUTE* 0)
	 (*REL-SECOND* 0) *REL-DAY-OF-THE-WEEK*
;	     *REL-TIME-ZONE*
	 *BASE-YEAR* *BASE-MONTH* *BASE-DATE* *BASE-HOUR* *BASE-MINUTE* *BASE-SECOND*
	 *RELATIVE-P*)

     ;; Compute the "base" time: the time to which the string is relative.
     (COND ((NULL BASE-TIME)
	    ;; Time is relative to right now.
	    (MULTIPLE-VALUE (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR* *BASE-DATE*
			     *BASE-MONTH* *BASE-YEAR*)
	      (GET-TIME))
	    ;; If the time is not known, assume a default base time so that we
	    ;; can still parse fully-specified date/times (e.g. in the file system)
	    (IF (NULL *BASE-SECOND*)
		(SETQ *BASE-SECOND* 0 *BASE-MINUTE* 0 *BASE-HOUR* 0
		      *BASE-DATE* 1 *BASE-MONTH* 1 *BASE-YEAR* 0)))
	   (T
	    ;; Time is relative to a specified time.
	    (MULTIPLE-VALUE (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR*
			     *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
	      (DECODE-UNIVERSAL-TIME  BASE-TIME))))
     
     ;; Do the parse, calling the action routines, which work by setting the
     ;; ABS and REL special variables bound above.
     (PARSE-1 (DELQ-ALL (LEXICALLY-ANALYZE STRING START END) *NOISE-WORDS*) 'MAIN)
     (IF (AND DATE-MUST-HAVE-YEAR (NULL *ABS-YEAR*))
	 (BARF "no year supplied"))
     (IF (AND TIME-MUST-HAVE-SECOND (NULL *ABS-SECOND*))
	 (BARF "no seconds supplied"))
     
     ;; Now apply lots of defaults.
     
     ;; There are many terms, from the lowest order (seconds) to the highest
     ;; order (years).  A legal date must specify some contiguous subsequence
     ;; of these.  The low unspecified ones get zeroed; the high unspecified
     ;; ones are either the next in the future or the previous in the past.
     ;; Time zones and days of the week are handled specially.
     
     ;; First, the following code allows a day of the week to be used to
     ;; specify a year, month, and date, when it is supposed to.
     (IF (AND (NULL *ABS-YEAR*)
	      (NULL *ABS-MONTH*)
	      (NULL *ABS-DATE*)
	      (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
	 ;; Day of week specified the year, month, and date.
	 (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)))
	   (MULTIPLE-VALUE-BIND (NIL NIL NIL NIL NIL NIL BASE-DAY-OF-THE-WEEK)
	       (DECODE-UNIVERSAL-TIME UT)
	     (LET ((DELTA-DAYS (- *ABS-DAY-OF-THE-WEEK* BASE-DAY-OF-THE-WEEK)))
	       (IF FUTUREP
		   (DO () ((> DELTA-DAYS 0))
		     (SETQ DELTA-DAYS (+ DELTA-DAYS 7)))
		   (DO () ((< DELTA-DAYS 0))
		     (SETQ DELTA-DAYS (- DELTA-DAYS 7))))
	       (MULTIPLE-VALUE (NIL NIL NIL *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
		 (COMPUTE-RELATIVE 0 0 0 (+ *BASE-DATE* DELTA-DAYS)
				   *BASE-MONTH* *BASE-YEAR*))))))
     
     ;; If everything was specified (as in a date read from a file server)
     ;; then skip worrying about defaulting.
     (OR (AND *ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*)
       ;; Non-specified low-order terms get set to zero (or the moral equivalent
       ;; of zero), up to the first speicified term.
       (DO ((TERMS '(*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR*
				  *ABS-DATE* *ABS-MONTH* *ABS-YEAR*) (CDR TERMS))
	    (BASE-TERMS '(*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR*
					*BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
			(CDR BASE-TERMS))
	    (LOWEST '(0 0 0 1 1 -100000000) (CDR LOWEST))
	    (HIGHEST '(59. 59. 23. NIL 12. 100000000) (CDR HIGHEST))
	    (STATE 'DEFAULT-LOW-TERMS)
	    (COMPARISON 'EQUAL)
	    (OPERATION NIL))
	   ((NULL TERMS)
	    (IF (EQ STATE 'DEFAULT-LOW-TERMS)
		(BARF "No time was specified.")))
	 RESTART
	 (LET ((TERM-VALUE (SYMEVAL (CAR TERMS)))
	       (BASE-TERM-VALUE (SYMEVAL (CAR BASE-TERMS))))
	   (SELECTQ STATE
	     (DEFAULT-LOW-TERMS
	      ;; Non-specified low-order terms get set to default values, which
	      ;; are zero or one depending on whether the quantity is zero-based
	      ;; or one-based.
	      (COND ((NULL TERM-VALUE)
		     ;; Term is non-specified, default it.
		     (SET (CAR TERMS) (CAR LOWEST)))
		    (T
		     ;; Term is specified: go to the next state and try again.
		     (SETQ STATE 'SKIP-OVER-SPECIFIED)
		     (GO RESTART))))
	     (SKIP-OVER-SPECIFIED
	      ;; Now we are moving over the contiguous subsequence of values
	      ;; specified by the user.
	      (COND ((NOT (NULL TERM-VALUE))
		     ;; This value was specified by the user.
		     (COND ((> TERM-VALUE BASE-TERM-VALUE)
			    ;; Specified time is later than the base time.
			    (SETQ COMPARISON 'LATER))
			   ((< TERM-VALUE BASE-TERM-VALUE)
			    ;; Specified time is earlier than the base time.
			    (SETQ COMPARISON 'EARLIER))
			   ;; If these terms are equal, use the old value of
			   ;;   COMPARISON based on the lower order terms.
			   ))
		    (T
		     ;; Term is not specified; go to the next state and try again.
		     ;; This SETQ is documented at the next state.
		     (SETQ OPERATION
			   (SELECTQ COMPARISON
			     (EQUAL
			      ;; The specified and base times are equal, do nothing.
			      'EQUAL)
			     (LATER
			      ;; Specified time is later than base time.
			      (IF FUTUREP 'EQUAL 'SUB1))
			     (EARLIER
			      ;; Specified time is earlier than base time.
			      (IF FUTUREP 'ADD1 'EQUAL))))
		     (SETQ STATE 'DEFAULT-HIGH-TERMS)
		     (GO RESTART))))
	     (DEFAULT-HIGH-TERMS
	      ;; Non-specified high-order terms come from the base time.  The
	      ;; tricky thing is that we may have to add or subtract one, depending
	      ;; on FUTUREP and COMPARISON, which requires propagating carry or
	      ;; borrow.  This information is encoded in OPERATION, which is SETQed
	      ;; above (so that we don't do it each time around the loop!).
	      (IF (NOT (NULL TERM-VALUE))
		  ;; Foo, the rest of the high-order terms have to be unspecified.
		  (BARF "Unrecognized pattern of defaulting."))
	      (SELECTQ OPERATION
		(EQUAL
		 ;; We are just copying base time into abs time.  Keep doing it.
		 (SET (CAR TERMS) BASE-TERM-VALUE))
		(ADD1
		 ;; Set this term one higher than it is in the base time.
		 (LET ((HIGHEST-VALUE
			 ;; Compute the highest legal value for this term.
			 (IF (EQ (CAR TERMS) '*ABS-DATE*)
			     ;; Highest possible value for dates depends on
			     ;; which month this is.
			     (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*)
			     ;; Other highest values are just constants.
			     (CAR HIGHEST))))
		   (COND ((< BASE-TERM-VALUE HIGHEST-VALUE)
			  ;; No carry.  Just add one, and copy the rest.
			  (SET (CAR TERMS) (1+ BASE-TERM-VALUE))
			  (SETQ OPERATION 'EQUAL))
			 (T
			  ;; Carry into next term.
			  (SET (CAR TERMS) (CAR LOWEST))))))
		(SUB1
		 ;; Set this term one lower than it is in the base time.
		 (COND ((> BASE-TERM-VALUE (CAR LOWEST))
			;; No borrow.  Just subtract one, and copy the rest.
			(SET (CAR TERMS) (1- BASE-TERM-VALUE))
			(SETQ OPERATION 'EQUAL))
		       (T
			;; Borrow from the next term.
			(SET (CAR TERMS)
			     (IF (EQ (CAR TERMS) '*ABS-DATE*)
				 ;; Highest possible value for dates depends on
				 ;; which month this is.
				 (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*)
				 ;; Other highest values are just constants.
				 (CAR HIGHEST))))))
		(OTHERWISE
		 (FERROR NIL "Bad value of OPERATION ~S" OPERATION))))
	     (OTHERWISE
	      (FERROR NIL "Bad value of STATE ~S" STATE))))))
     
     ;; Now hack other random defaults.
;	 (IF (NULL *ABS-TIME-ZONE*)
;	     (SETQ *ABS-TIME-ZONE* *TIMEZONE*))
;	 (SETQ *REL-TIME-ZONE* *ABS-TIME-ZONE*)
     
     ;; Check ranges.
     (CHECK-RANGE *ABS-SECOND* 0 59. "seconds in a minute")
     (CHECK-RANGE *ABS-MINUTE* 0 59. "minutes in an hour")
     (CHECK-RANGE *ABS-HOUR* 0 23. "hours in a day")
					    ;Check this before MONTH-STRING call!
     (CHECK-RANGE *ABS-MONTH* 1 12. "months in a year")
     (CHECK-RANGE *ABS-DATE*
		  1
		  (MONTH-LENGTH *ABS-MONTH* *ABS-YEAR*)
		  (FORMAT NIL "days in ~A" (MONTH-STRING *ABS-MONTH*)))
     (IF (AND DAY-MUST-BE-VALID (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
	 (VERIFY-DATE *ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-DAY-OF-THE-WEEK*))
     
     ;; Now put it together.
     (MULTIPLE-VALUE (*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
       (COMPUTE-RELATIVE (+ *ABS-SECOND* *REL-SECOND*)
			 (+ *ABS-MINUTE* *REL-MINUTE*)
			 (+ *ABS-HOUR* *REL-HOUR*)
			 (+ *ABS-DATE* *REL-DATE*)
			 (+ *ABS-MONTH* *REL-MONTH*)
			 (+ *ABS-YEAR* *REL-YEAR*)))
     (RETURN (ENCODE-UNIVERSAL-TIME *ABS-SECOND* *ABS-MINUTE* *ABS-HOUR*
				    *ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-TIME-ZONE*)
	     *RELATIVE-P*))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "


(DEFUN (:DELIMITED-STRING PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION STREAM)
  (READ-DELIMITED-STRING (OR (AND (CONSP OPTION) (GET OPTION ':DELIMITER)) #\END)
			 STREAM NIL NIL
			 (OR (AND (CONSP OPTION) (GET OPTION ':DELIMITER)) 100)))

(DEFUN (:DELIMITED-STRING-OR-NIL PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION STREAM)
  (LET ((STRING
	  (READ-DELIMITED-STRING (OR (AND (CONSP OPTION) (GET OPTION ':DELIMITER)) #\END)
				 STREAM NIL NIL
				 (OR (AND (CONSP OPTION) (GET OPTION ':DELIMITER)) 100))))
    (IF (EQUAL STRING "") NIL STRING)))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN (:PATHNAME-OR-NIL PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION STREAM)
  (LET ((DEFAULTS (IF (CONSP OPTION) (GET OPTION ':DEFAULTS) FS:*DEFAULT-PATHNAME-DEFAULTS*)))
    (LET ((STRING
	    (IF (SEND STREAM ':OPERATION-HANDLED-P ':RUBOUT-HANDLER)
		(SEND STREAM ':RUBOUT-HANDLER
		      '((:PROMPT PROMPT-AND-READ-PROMPT-FUNCTION))
		      'PATHNAME-OR-END-PROMPT-AND-READ-INTERNAL
		      STREAM)
	      (PATHNAME-OR-END-PROMPT-AND-READ-INTERNAL STREAM))))
      (IF (NOT (STRINGP STRING)) NIL
	(FS:MERGE-PATHNAME-DEFAULTS STRING DEFAULTS
				    FS:*NAME-SPECIFIED-DEFAULT-TYPE*
				    (OR (AND (CONSP OPTION) (GET OPTION ':DEFAULT-VERSION))
					':NEWEST))))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "


(DEFUN (:PATHNAME PROMPT-AND-READ-FUNCTION) (OPTION STREAM)
  (LET ((DEFAULTS (IF (CONSP OPTION) (GET OPTION ':DEFAULTS) FS:*DEFAULT-PATHNAME-DEFAULTS*))
	(STRING (READLINE STREAM)))
    (FS:MERGE-PATHNAME-DEFAULTS STRING DEFAULTS
				FS:*NAME-SPECIFIED-DEFAULT-TYPE*
				(OR (AND (CONSP OPTION) (GET OPTION ':DEFAULT-VERSION))
				    ':NEWEST))))

(DEFUN (:PATHNAME-OR-END PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION STREAM)
  (LET ((DEFAULTS (IF (CONSP OPTION) (GET OPTION ':DEFAULTS) FS:*DEFAULT-PATHNAME-DEFAULTS*)))
    (LET ((STRING
	    (IF (SEND STREAM ':OPERATION-HANDLED-P ':RUBOUT-HANDLER)
		(SEND STREAM ':RUBOUT-HANDLER
		      '((:PROMPT PROMPT-AND-READ-PROMPT-FUNCTION))
		      'PATHNAME-OR-END-PROMPT-AND-READ-INTERNAL
		      STREAM)
	      (PATHNAME-OR-END-PROMPT-AND-READ-INTERNAL STREAM))))
      (IF (NOT (STRINGP STRING)) STRING
	(FS:MERGE-PATHNAME-DEFAULTS STRING DEFAULTS
				    FS:*NAME-SPECIFIED-DEFAULT-TYPE*
				    (OR (AND (CONSP OPTION) (GET OPTION ':DEFAULT-VERSION))
					':NEWEST))))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "


(DEFPROP :EVAL-READ-OR-END EVAL-READ-OR-END-PROMPT-AND-READ
	 PROMPT-AND-READ-NO-RUBOUT-FUNCTION)
(DEFPROP :EVAL-SEXP-OR-END EVAL-READ-OR-END-PROMPT-AND-READ
	 PROMPT-AND-READ-NO-RUBOUT-FUNCTION)
(DEFPROP :EVAL-FORM-OR-END EVAL-READ-OR-END-PROMPT-AND-READ
	 PROMPT-AND-READ-NO-RUBOUT-FUNCTION)
(DEFUN EVAL-READ-OR-END-PROMPT-AND-READ (OPTION STREAM
					 &AUX
					 (DEFAULT (IF (CONSP OPTION) (GET OPTION ':DEFAULT))))
  (DECLARE (SPECIAL DEFAULT))
  (DO (VALUE FORM FLAG
       (DO-IT
	 (IF (AND (CONSP OPTION) (GET OPTION ':DEFAULT))
	      #'(LAMBDA (STREAM)
		  (LET ((CH (SEND STREAM ':TYI)))
		    (COND ((= CH #\SP) (VALUES DEFAULT ':DEFAULT))
			  ((= CH #\END) (VALUES NIL ':END))
			  (T (SEND STREAM ':UNTYI CH)
			     (VALUES (READ STREAM))))))
	   #'(LAMBDA (STREAM)
	       (LET ((CH (SEND STREAM ':TYI)))
		 (IF (= CH #\END) (VALUES NIL ':END)
		   (SEND STREAM ':UNTYI CH)
		   (VALUES (READ STREAM))))))))
      (())
    (ERROR-RESTART (ERROR "Try again to type this input.")
      (MULTIPLE-VALUE (FORM FLAG)
	(IF (SEND STREAM ':OPERATION-HANDLED-P ':RUBOUT-HANDLER)
	    (SEND STREAM ':RUBOUT-HANDLER
		  '((:PROMPT PROMPT-AND-READ-PROMPT-FUNCTION))
		  DO-IT
		  STREAM)
	  (FUNCALL DO-IT STREAM)))
      (IF FLAG (RETURN FORM FLAG)
	(SETQ VALUE (EVAL-ABORT-TRIVIAL-ERRORS FORM))))
    ;; If FORM was not trivial, ask for confirmation of the value it returned.
    (WHEN (OR (TRIVIAL-FORM-P FORM)
	      (LET ((PRINLEVEL EVAL-READ-PRINLEVEL)
		    (PRINLENGTH EVAL-READ-PRINLENGTH))
		(FQUERY '(:LIST-CHOICES NIL) "The object is ~S, ok? " VALUE)))
      (RETURN VALUE))
    (TERPRI STREAM)))

(DEFPROP :READ READ-PROMPT-AND-READ PROMPT-AND-READ-FUNCTION)
(DEFPROP :EXPRESSION READ-PROMPT-AND-READ PROMPT-AND-READ-FUNCTION)
(DEFUN READ-PROMPT-AND-READ (IGNORE STREAM)
  (VALUES (READ STREAM)))

(DEFPROP :EXPRESSION-OR-END EXPRESSION-OR-END-PROMPT-AND-READ PROMPT-AND-READ-FUNCTION)
(DEFUN EXPRESSION-OR-END-PROMPT-AND-READ (IGNORE STREAM)
  (LET ((CH (SEND STREAM ':TYI)))
    (IF (EQ CH #\END)
	(VALUES NIL ':END)
      (SEND STREAM ':UNTYI CH)
      (VALUES (READ STREAM)))))

(DEFUN (:CHARACTER PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (IGNORE STREAM)
  (SEND STREAM ':TYI))

(DEFUN (:NUMBER PROMPT-AND-READ-FUNCTION) (OPTION STREAM)
  (LET ((IBASE (OR (AND (CONSP OPTION) (GET OPTION ':INPUT-RADIX)) IBASE))
	(CH (SEND STREAM ':TYI)))
    (IF (AND (CONSP OPTION) (GET OPTION ':OR-NIL)
	     (MEMQ CH '(#\RETURN #\END)))
	NIL
      (SEND STREAM ':UNTYI CH)
      (LET* ((STRING (READLINE-TRIM STREAM))
	     (NUMBER (READ-FROM-STRING STRING)))
	(IF (NUMBERP NUMBER) NUMBER
	  (FERROR 'READ-ERROR-1 "~S is not a number." NUMBER))))))

(DEFUN (:DATE PROMPT-AND-READ-FUNCTION) (OPTION STREAM)
  (LET ((STRING (READLINE-TRIM STREAM)))
    (IF (EQUALP STRING "never")
	(IF (AND (CONSP OPTION)
		 (GET OPTION ':NEVER-P))
	    NIL
	  (FERROR 'READ-ERROR-1 "Never is not allowed here."))
      (LET* ((PAST-P (AND (CONSP OPTION) (GET OPTION ':PAST-P)))
	     (DATE (CONDITION-CASE (ERROR)
		       (TIME:PARSE-UNIVERSAL-TIME STRING 0 NIL (NOT PAST-P))
		     (TIME:PARSE-ERROR
		      (FERROR 'READ-ERROR-1 "~A" (SEND ERROR ':REPORT-STRING))))))
	(AND PAST-P (> DATE (TIME:GET-UNIVERSAL-TIME))
	     (FERROR 'READ-ERROR-1 "~A is not in the past."
		     (TIME:PRINT-UNIVERSAL-TIME DATE NIL)))
	DATE))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
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

The second value is T if we exit due to end of file."
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS)
	   (VALUES STRING-OR-EOF-OPTION EOF-FLAG))
  (LET ((OPTIONS NIL))
    ;; This kludge is to let us take a third, optional argument.
    (COND ((> (LENGTH READ-ARGS) 2)
	   (SETQ OPTIONS (THIRD READ-ARGS))
	   (SETQ READ-ARGS (LIST (FIRST READ-ARGS) (SECOND READ-ARGS)))))
    (MULTIPLE-VALUE-BIND (STREAM EOF-OPTION)
	(DECODE-READ-ARGS READ-ARGS)
      ;; If stream does rubout handling, get inside the rubout handler
      (COND ((AND (NOT RUBOUT-HANDLER)
		  (MEMQ ':RUBOUT-HANDLER (FUNCALL STREAM ':WHICH-OPERATIONS)))
	     ;;Stream with rubouts assumed not to have EOFs
	     (FUNCALL STREAM ':RUBOUT-HANDLER OPTIONS #'READLINE STREAM EOF-OPTION))
	    ;; All character streams are assumed to support :LINE-IN,
	    ;; by virtue of the STREAM-DEFAULT-HANDLER if for no other reason.
	    (T
	     (MULTIPLE-VALUE-BIND (STRING EOF)
		 (FUNCALL STREAM ':LINE-IN T)
	       (IF (AND EOF (EQUAL STRING ""))
		   (IF (EQ EOF-OPTION 'NO-EOF-OPTION)
		       (FERROR NIL "End of file encountered on stream ~S" STREAM)
		     (VALUES EOF-OPTION T))
		 (VALUES STRING EOF))))))))

(DEFUN READLINE-TRIM (&REST READ-ARGS)
  "Read a line from STREAM and return it as a string, sans leading and trailing whitespace.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to :RUBOUT-HANDLER if it is used.

The second value is T if we exit due to end of file."
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS)
	   (VALUES STRING EOF))
  (MULTIPLE-VALUE-BIND (STRING EOF)
      (APPLY 'READLINE READ-ARGS)
    (VALUES
      (IF EOF
	  STRING
	(STRING-TRIM '(#\SP #\TAB) STRING))
      EOF)))

(DEFUN READLINE-OR-NIL (&REST READ-ARGS)
  "Read a line from STREAM and return it as a string, or return NIL if line is empty.
The string does not include a Return character.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to :RUBOUT-HANDLER if it is used.

The second value is T if we exit due to end of file."
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS)
	   (VALUES STRING-OR-NIL EOF))
  (MULTIPLE-VALUE-BIND (STRING EOF)
      (APPLY 'READLINE READ-ARGS)
    (VALUES
      (IF EOF
	  STRING
	(SETQ STRING (STRING-TRIM '(#\SP #\TAB) STRING))
	(IF (EQUAL STRING "") NIL STRING))
      EOF)))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN NULL-STREAM (OP &REST ARGS &AUX TEM)
  "An i//o stream which ignores output and gives instant end-of-file on input."
  (SELECTQ-WITH-WHICH-OPERATIONS OP
    ;; These operations signal EOF.
    ((:TYI :TYI-NO-HANG :TYIPEEK :GET-INPUT-BUFFER :READ-INPUT-BUFFER)
     (AND (FIRST ARGS) (FERROR 'READ-END-OF-FILE "End of file on SI:NULL-STREAM.")))
    ;; Signals EOF differently.
    (:STRING-IN
     (AND (FIRST ARGS) (FERROR 'READ-END-OF-FILE "End of file on SI:NULL-STREAM."))
     (VALUES (THIRD ARGS) T))
    ;; Signals EOF still differently.
    (:LINE-IN
      (SETQ TEM (MAKE-ARRAY 0 ':TYPE 'ART-STRING
			      ':LEADER-LENGTH (AND (NUMBERP (FIRST ARGS)) (FIRST ARGS))))
      (AND (NUMBERP (FIRST ARGS))
	   (PLUSP (FIRST ARGS))
	   (STORE-ARRAY-LEADER 0 TEM 0))
      (VALUES TEM T))
    ;; These operations should all return their argument.
    ((:TYO :STRING-OUT :LINE-OUT :UNTYI)
     (FIRST ARGS))
    ((:INCREMENT-CURSORPOS :FINISH :FORCE-OUTPUT :CLEAR-OUTPUT :CLEAR-INPUT :LISTEN)
     NIL)
    ;; These operations should always return T.
    ((:CHARACTERS :BEEP) T)
    ;; Supports nothing in both directions.
    (:DIRECTION ':BIDIRECTIONAL)
    ;; Handle obscure operations.
    (OTHERWISE (STREAM-DEFAULT-HANDLER 'NULL-STREAM OP (FIRST ARGS) (REST1 ARGS)))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2 &AUX FL)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(METHOD-TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC))
	(DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (IF (NULL (CDDDR FUNCTION-SPEC))
	(SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (COND ((NOT (AND (SYMBOLP FLAVOR)
		     (SYMBOLP METHOD-TYPE)
		     (SYMBOLP MESSAGE)
		     ( 3 (LENGTH FUNCTION-SPEC) 5)))
	   (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	     (FERROR 'SYS:INVALID-FUNCTION-SPEC
		     "The function spec ~S is invalid." FUNCTION-SPEC)))
	  ((EQ T (SETQ FL (COMPILATION-FLAVOR FLAVOR)))
	   ;; Silly pseudo-flavor for cold-load stream
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       T
	     ;;The property-list operations need to work for the editor
	     (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))
	  ((OR FL		;A defined flavor
	       (NOT (CLASS-SYMBOLP FLAVOR)))	;Not defined, assume flavor
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       T
	     ;; Ignore FASLOAD-COMBINED methods if flavor methods composed already.
	     (IF (AND FL (FLAVOR-METHOD-HASH-TABLE FL)
		      (EQ (THIRD FUNCTION-SPEC) 'FASLOAD-COMBINED))
		 ;; This hair makes defining (INTERNAL (:METHOD FOO FASLOAD-COMBINED ...) ...)
		 ;; get ignored properly and not get an error.
		 (SELECTQ FUNCTION
		   (FDEFINITION LAST-FASLOAD-COMBINED-METHOD)
		   (FDEFINEDP T)
		   (FDEFINE
		    (SETQ LAST-FASLOAD-COMBINED-METHOD ARG1))
		   (FDEFINITION-LOCATION (LOCF LAST-FASLOAD-COMBINED-METHOD))
		   (T NIL))
	       ;; Otherwise refer to or define the :COMBINED method.
	       (IF (EQ METHOD-TYPE 'FASLOAD-COMBINED)
		   (SETQ FUNCTION-SPEC (LIST* (FIRST FUNCTION-SPEC) FLAVOR
					      ':COMBINED (CDDDR FUNCTION-SPEC))
			 METHOD-TYPE ':COMBINED))
	       (LET ((METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC
			     (SELECTQ FUNCTION
			       ((PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE)
				NIL)		;Create.
			       (OTHERWISE T)))))	;Don't create
		 (OR (AND METH (METH-DEFINEDP METH))
		     (MEMQ FUNCTION '(FDEFINEDP COMPILER-FDEFINEDP
				      PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE
				      GET FUNCTION-PARENT DWIMIFY))
		     (IF FL
			 (FERROR NIL "~S is not a defined method; it is not possible to ~S it"
			         FUNCTION-SPEC FUNCTION)
		         (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC)))
		 (SELECTQ FUNCTION
		   (FDEFINE
		     (OR FL
			 (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC))
		     (LET ((DEFINITION-NEW (NOT (METH-DEFINEDP METH)))
			   (OLD-DEFINITION (AND (METH-DEFINEDP METH) (METH-DEFINITION METH))))
		       (SETF (METH-DEFINITION METH) ARG1)
		       ;; If we load a method compiled before system 83,
		       ;; that expects instance variables to be bound,
		       ;; make it work by forcing this flavor to bind all variables.
		       (IF (AND (TYPEP ARG1 ':COMPILED-FUNCTION)
				(ZEROP (%P-LDB %%FEFH-GET-SELF-MAPPING-TABLE ARG1))
				(NOT (ASSQ 'ENCAPSULATED-DEFINITION (DEBUGGING-INFO ARG1))))
			   (MAKE-FLAVOR-ALL-SPECIAL FL))
		       ;; Incrementally recompile the flavor if this is a new method, unless
		       ;; it is a :COMBINED method, which is the result of compilation,
		       ;; not a client of it.
		       (COND ((MEMQ METHOD-TYPE '(:WRAPPER :INVERSE-WRAPPER))
			      (OR (AND (CONSP OLD-DEFINITION)
				       (FEF-EQUAL (CDR ARG1) (CDR OLD-DEFINITION)))
				  ;; Wrapper is really changed; must recompile flavors.
				  ;; Arrange that if we abort, the definition is set
				  ;; to the symbol ABORTED-DEFINITION.  This is a no-op,
				  ;; and redefining or undefining the wrapper will recompile.
				  (LET (SUCCESS)
				    (UNWIND-PROTECT
				      (PROGN
					(RECOMPILE-FLAVOR FLAVOR MESSAGE NIL)
					(SETQ SUCCESS T))
				      (OR SUCCESS
					  (SETF (METH-DEFINITION METH)
						'ABORTED-DEFINITION))))))
			     ((EQ METHOD-TYPE ':COMBINED) NIL)
			     (DEFINITION-NEW
			      ;; This SETF, by virtue of the preceding clause,
			      ;; arranges that if we abort out before finishing recompilation
			      ;; then the recompilation will be done again if the user
			      ;; either redoes the defmethod or does undefmethod.
			      (SETF (METH-DEFINITION METH) 'ABORTED-DEFINITION)
			      (RECOMPILE-FLAVOR FLAVOR MESSAGE)
			      (SETF (METH-DEFINITION METH) ARG1))
			     ;; If method defined as a random symbol,
			     ;; must fix up hash table each time it changes.
			     ((OR (SYMBOLP OLD-DEFINITION)
				  (SYMBOLP ARG1))
			      (RECOMPILE-FLAVOR FLAVOR MESSAGE)))))
		   (FDEFINITION (METH-DEFINITION METH))
		   (FDEFINEDP (AND METH (VALUES (METH-DEFINEDP METH)
						(AND (METH-DEFINEDP METH)
						     (METH-DEFINITION METH)))))
		   (FDEFINITION-LOCATION (LOCF (METH-DEFINITION METH)))
		   (FUNDEFINE
		    (SETF (METH-DEFINITION METH) 'UNDEFINITION-IN-PROGRESS)
		    (RECOMPILE-FLAVOR (FLAVOR-NAME FL) MESSAGE)	;Propagate the change
		    (NULLIFY-METHOD-DEFINITION METH))	;Say propagation is complete.
		   (COMPILER-FDEFINEDP METH)
		   (GET (AND METH (GET (LOCF (METH-PLIST METH)) ARG1)))
		   (PUTPROP (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			      (PUTPROP (LOCF (METH-PLIST METH)) ARG1 ARG2)))
		   (PUSH-PROPERTY
		    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
		      (PUTPROP (LOCF (METH-PLIST METH))
			       (CONS ARG1 (GET (LOCF (METH-PLIST METH)) ARG2))
			       ARG2)))
		   (DWIMIFY
		    (CATCH-CONTINUATION 'DWIMIFY-PACKAGE
			#'(LAMBDA (NEW-SPEC) NEW-SPEC)
			#'(LAMBDA () NIL)
		      (DOLIST (COMPONENT
				(OR (FLAVOR-DEPENDS-ON-ALL FL)
				    (COMPOSE-FLAVOR-COMBINATION FL NIL)))
			(LET ((FLAVOR (COMPILATION-FLAVOR COMPONENT))
			      (METHS))
			  (AND FLAVOR
			       (SETQ METHS
				     (CDDDR (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FLAVOR)))))
			  (DOLIST (METH METHS)
			    (AND (METH-DEFINEDP METH)
				 (DWIMIFY-PACKAGE-2 (METH-FUNCTION-SPEC METH)
						    ARG1 ARG2 T)))))))
		   (OTHERWISE
		    (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))))
	  (T
	   (CLASS-METHOD-FUNCTION-SPEC-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1LAMBDA (LAMBDA ARGS)
  (LET (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR QUOTEFLAG
	SPECIAL-FLAG SPECIAL-VARS UNSPECIAL-FLAG UNSPECIAL-VARS
	DECLS KEYCHECKS BORDER-VARIABLE)
    (SETQ LAMBDA (SI:LAMBDA-EXP-ARGS-AND-BODY (P1AUX LAMBDA)))
    (SETQ ARGLIST (CAR LAMBDA) BODY (CDR LAMBDA))
    (MULTIPLE-VALUE-BIND (NIL NIL NIL REST-ARG
			  NIL KEYKEYS KEYNAMES NIL KEYINITS KEYFLAGS
			  ALLOW-OTHER-KEYS)
	(DECODE-KEYWORD-ARGLIST ARGLIST)
      (WHEN (AND KEYNAMES (NOT REST-ARG))
	(SETQ REST-ARG (GENSYM)))
      (SETQ ARGS1 ARGS)
      (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
	  (NIL)
	(SETQ VAR (CAR ARGLIST1))
	(COND ((NULL ARGLIST1)
	       (RETURN T))
	      ((EQ VAR '&KEY)
	       (PUSH (LIST REST-ARG `(LIST . ,ARGS1)) PROGVARS)
	       (RETURN (SETQ ARGS1 NIL)))
	      ((EQ VAR '&REST)
	       (POP ARGLIST1)
	       (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
	       (RETURN (SETQ ARGS1 NIL)))
	      ((EQ VAR '&OPTIONAL)
	       (SETQ OPTIONAL T))
	      ((EQ VAR '&QUOTE)
	       (SETQ QUOTEFLAG T))
	      ((EQ VAR '&EVAL)
	       (SETQ QUOTEFLAG NIL))
	      ((EQ VAR '&SPECIAL)
	       (SETQ SPECIAL-FLAG T UNSPECIAL-FLAG NIL))
	      ((EQ VAR '&LOCAL)
	       (SETQ SPECIAL-FLAG NIL UNSPECIAL-FLAG T))
	      ((EQ VAR '&FUNCTIONAL))
	      ((MEMQ VAR LAMBDA-LIST-KEYWORDS)
	       (WARN 'BAD-INTERNAL-LAMBDA-KEYWORD ':IMPOSSIBLE
		     "~S is not supported in internal lambdas." VAR))
	      (T (AND SPECIAL-FLAG (PUSH VAR SPECIAL-VARS))
		 (AND UNSPECIAL-FLAG (PUSH VAR UNSPECIAL-VARS))
		 (COND ((SYMBOLP VAR)
			(PUSH (LIST VAR (IF QUOTEFLAG `',(CAR ARGS1)
					  (CAR ARGS1)))
			      PROGVARS))
		       (T
			(COND ((NOT OPTIONAL)
			       (WARN 'BAD-ARGUMENT-LIST ':IMPOSSIBLE
				     "The mandatory argument ~S of an internal lambda ~
  was given a default value."
				     (CAR VAR))))
			(PUSH (LIST (CAR VAR)
				    (COND (ARGS1 (IF QUOTEFLAG `',(CAR ARGS1)
						   (CAR ARGS1)))
					  (T (CADR VAR)))) PROGVARS)))
		 (POP ARGS1))))
      (WHEN KEYNAMES
	;; For each keyword arg, decide whether we need to nit it to KEYWORD-GARBAGE
	;; and check explicitly whether that has been overridden.
	;; If the arg is optional
	;; and the initial value is a constant, we can really init it to that.
	;; Otherwise, we change its KEYINITS element to
	;; KEYWORD-GARBAGE and push a cleanup form on KEYCHECKS.
	(DO ((KIS KEYINITS (CDR KIS))
	     (KNS KEYNAMES (CDR KNS))
	     (KFS KEYFLAGS (CDR KFS)))
	    ((NULL KNS))
	  (LET ((KEYNAME (CAR KNS)) (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	    ;; All optional now.
	    (OR (AND (NULL KEYFLAG)
		     (CONSTANTP KEYINIT))
		(PROGN (RPLACA KIS 'SI:KEYWORD-GARBAGE)
		       (PUSH `(COND ((EQ ,KEYNAME SI:KEYWORD-GARBAGE)
				     (SETQ ,KEYNAME ,KEYINIT))
				    (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))))
			     KEYCHECKS)))))
	(SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
	(SETQ KEYCHECKS (NREVERSE KEYCHECKS))

	;; BORDER-VARIABLE is a local we put in the binding list
	;; as the easiest way of being able to get a locative to the
	;; slot before the first of our keyword arg locals.
	(SETQ BORDER-VARIABLE (GENSYM))

	;; Put our list of variable names onto CLOBBER-NONSPECIAL-VARS-LISTS
	;; so that ASSIGN-LAP-ADDRESSES will clobber out the variables
	;; which are not special with NIL.
	(PUSH KEYNAMES CLOBBER-NONSPECIAL-VARS-LISTS)
	(SETQ BODY
	      `((LET* (,BORDER-VARIABLE
		       ,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) KEYNAMES KEYINITS)
		       ,@KEYFLAGS)
		  ,BORDER-VARIABLE
		  (WHEN ,REST-ARG
		    (SI:STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
		      (VARIABLE-LOCATION ,BORDER-VARIABLE)
		      ,REST-ARG ',KEYKEYS
		      ,ALLOW-OTHER-KEYS
		      ',KEYNAMES))
		  ,@KEYCHECKS
		  . ,BODY))))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (SETF (VALUES BODY DECLS)
	    (EXTRACT-DECLARATIONS-RECORD-MACROS BODY))
      (WHEN SPECIAL-VARS
	(PUSH `(SPECIAL . ,SPECIAL-VARS) DECLS))
      (WHEN UNSPECIAL-VARS
	(PUSH `(UNSPECIAL . ,UNSPECIAL-VARS) DECLS))
      (WHEN DECLS
	(PUSH `(DECLARE . ,DECLS) BODY))
      (P1 `(LET-FOR-LAMBDA ,(NRECONC PROGVARS (IF ARGS1 `((IGNORE (PROGN . ,ARGS1)))))
	     . ,BODY)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (LET-FOR-LAMBDA P1) (FORM)
  (LET ((VARS VARS)
	(P1VALUE NIL) (BINDP) (BODY) (VLIST)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(ENTRY-LEXICAL-CLOSURE-COUNT LEXICAL-CLOSURE-COUNT))
    ;; Take all DECLAREs off the body.
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS (CDDR FORM) NIL))
    (SETQ VLIST (P1SBIND (CADR FORM)
			 (COND (TLEVEL 'FEF-ARG-AUX)
			       (T 'FEF-ARG-INTERNAL-AUX))
			 T NIL THIS-FRAME-DECLARATIONS))
    (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    ;; Now P1 process what is left of the body.
    (AND (CDR BODY) (SETQ TLEVEL NIL))
    (SETQ BODY (P1PROGN-1 BODY))
    `(LET ,VLIST ,VARS ,BINDP
      ,ENTRY-LEXICAL-CLOSURE-COUNT ,LEXICAL-CLOSURE-COUNT
      . ,BODY)))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1LET (FORM &OPTIONAL FOR-AUXVARS)
  (LET ((VARS VARS)
	(FN (CAR FORM)) (P1VALUE NIL) (BINDP) (BODY) (VLIST)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(ENTRY-LEXICAL-CLOSURE-COUNT LEXICAL-CLOSURE-COUNT))
    (SETQ VLIST (CADR FORM))
    (SETQ BODY (CDDR FORM))
    (IF (EQ FN 'LET-FOR-AUXVARS) (SETQ FN 'LET*))
    ;; Take all DECLAREs off the body.
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL))
    ;; Treat parallel binding as serial if it doesn't matter.
    (OR (CDR VLIST) (SETQ FN 'LET*))
    (AND (MEMQ FN '(LET :LET))
	 (DO ((XX VLIST (CDR XX)))
	     ((NULL XX) (SETQ FN 'LET*))
	   ;; Namely, if binding each symbol to NIL, a constant, or itself.
	   (OR (ATOM (CAR XX))
	       (CONSTANTP (CADAR XX))
	       (EQ (CAAR XX) (CADAR XX))
	       (RETURN NIL))))
    ;; Flush rebinding a var to itself if it isn't special
    ;; and range of rebinding is rest of function.
    (AND TLEVEL
	 (SETQ VLIST
	       (SUBSET-NOT #'(LAMBDA (VAR)
			       (AND (NOT (ATOM VAR))
				    (EQ (CAR VAR) (CADR VAR))
				    (EQ (FIND-TYPE (CAR VAR) THIS-FRAME-DECLARATIONS)
					'FEF-LOCAL)
				    (EQ (VAR-TYPE (ASSQ (CAR VAR) VARS)) 'FEF-LOCAL)))
			   VLIST)))
    ;; &AUX vars should be allowed to inherit special declarations
    ;; since that is what it looks like when you put a DECLARE inside the body.
    (WHEN FOR-AUXVARS
      (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS)
	    THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    (SETQ VLIST (P1SBIND VLIST
			 (COND (TLEVEL 'FEF-ARG-AUX)
			       (T 'FEF-ARG-INTERNAL-AUX))
			 (MEMQ FN '(LET :LET))
			 NIL
			 THIS-FRAME-DECLARATIONS))
    (UNLESS FOR-AUXVARS
      (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS)))
    ;; Now convert initial SETQs to variable initializations.
    ;; We win only for SETQs of variables bound but with no initialization spec'd,
    ;; which set them to constant values, and only if later vars' inits didn't use them.
    ;; When we come to anything other than a SETQ we can win for, we stop.
    ;; For PROG*, we can't win for a special variable if anyone has called a function
    ;; to do initting, since that function might have referred to the special.
    ;; Even if we don't use tha ADL to init them,
    ;; we avoid redundant settings to NIL.
    (DO ((TEM) (HOME)) (NIL)
      (COND ((MEMBER (CAR BODY) '((SETQ) (:SETQ)))
	     (SETQ BODY (CDR BODY)))
	    ((OR (ATOM (CAR BODY))
		 (ATOM (SETQ TEM (OPTIMIZE (CAR BODY) NIL)))
		 (NOT (MEMQ (CAR TEM) '(SETQ :SETQ)))
		 (NOT (MEMQ (CADR TEM) VLIST))
		 (NOT (CONSTANTP (CADDR TEM)))
		 (AND (SPECIALP (CADR TEM))
		      (OR TLFUNINIT (NOT TLEVEL))
		      (MEMQ FN '(LET* :LET*)))
		 (NOT (ZEROP (VAR-USE-COUNT (SETQ HOME (ASSQ (CADR TEM) VARS))))))
	     (RETURN NIL))
	    (T (SETQ BODY (CONS (CONS 'SETQ (CDDDR TEM)) (CDR BODY)))
	       (RPLACA (MEMQ (CADR TEM) VLIST)
		       `(,(CADR TEM) ,(P1 (CADDR TEM))))
	       ;; For a variable bound at function entry, really set up its init.
	       ;; Other vars (FEF-ARG-INTERNAL-AUX) will be initted by code,
	       ;; despite our optimization, but it will be better code.
	       (AND TLEVEL (EQ (VAR-KIND HOME) 'FEF-ARG-AUX)
		    (SETF (VAR-INIT HOME) `(FEF-INI-PNTR ,(P1 (CADDR TEM))))))))
    
    ;; Now P1 process what is left of the body.
    (AND (CDR BODY) (SETQ TLEVEL NIL))
    (SETQ BODY (P1PROGN-1 BODY))
    `(,FN ,VLIST ,VARS ,BINDP
      ,ENTRY-LEXICAL-CLOSURE-COUNT ,LEXICAL-CLOSURE-COUNT
      . ,BODY)))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFSETF EH-ARG SET-EH-ARG)
(DEFSETF EH-LOC SET-EH-LOC)
(DEFSETF EH-VAL SET-EH-VAL)
(DEFSETF EH-FUN SET-EH-FUN)

(DEFPROP EH-ARG EH-ARG-LOCATION SI:LOCF-METHOD)
(DEFPROP EH-LOC EH-LOC-LOCATION SI:LOCF-METHOD)
(DEFPROP EH-VAL EH-VAL-LOCATION SI:LOCF-METHOD)
(DEFPROP EH-FUN EH-FUN-LOCATION SI:LOCF-METHOD)

))

; From file CLPACK.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN PKG-FIND-PACKAGE (THING &OPTIONAL CREATE-P USE-LOCAL-NAMES-PACKAGE)
  "Find or possibly create a package named THING.
If FIND-PACKAGE can find a package from the name THING,
we return that package.
Otherwise, we may create such a package, depending on CREATE-P.
This should only happen if THING is a string or symbol.
Possible values of CREATE-P:
 NIL means get an error,
 :FIND means return NIL,
 :ASK means create package and return it after getting confirmation,
 T means create package and return it."
  (OR (AND (PACKAGEP THING) THING)
      (FIND-PACKAGE THING USE-LOCAL-NAMES-PACKAGE)
      (SELECTQ CREATE-P
	(:FIND NIL)
	((NIL :ERROR)
	 (SIGNAL-PROCEED-CASE ((NEW-NAME) 'PACKAGE-NOT-FOUND-1
					  "Package ~A does not exist."
					  THING USE-LOCAL-NAMES-PACKAGE)
	   (:CREATE-PACKAGE (OR (FIND-PACKAGE THING)
				(MAKE-PACKAGE THING)))
	   (:NEW-NAME
	    (LET* ((PACKAGE PKG-USER-PACKAGE)
		   (STRING1 (STRING (READ-FROM-STRING NEW-NAME))))
	      (PKG-FIND-PACKAGE STRING1 CREATE-P NIL)))
	   (:RETRY (PKG-FIND-PACKAGE THING CREATE-P USE-LOCAL-NAMES-PACKAGE))))
	(:ASK
	 (IF (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
		     "~&Package ~A not found.  Create? "
		     THING)
	     (MAKE-PACKAGE THING)
	   (CERROR ':NO-ACTION NIL NIL
		   "Please load package ~A declaration file then continue." THING)
	   (PKG-FIND-PACKAGE THING CREATE-P)))
	((T)
	 (MAKE-PACKAGE THING)))))

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "


(DEFFLAVOR PACKAGE-NOT-FOUND () (FERROR))

(DEFSIGNAL PACKAGE-NOT-FOUND-1 (PACKAGE-NOT-FOUND PACKAGE-ERROR)
	   (NAME RELATIVE-TO)
	   "Package NAME doesn't exist; RELATIVE-TO is package whose local nicknames we are searching.")

(DEFMETHOD (PACKAGE-NOT-FOUND :CASE :PROCEED-ASKING-USER :RETRY)
	   (PROCEED-FUNCTION IGNORE)
  "Looks for the package again.  Use this if you create it by hand."
  (FUNCALL PROCEED-FUNCTION ':RETRY))

(DEFMETHOD (PACKAGE-NOT-FOUND :CASE :PROCEED-ASKING-USER :NEW-NAME)
	   (PROCEED-FUNCTION PROMPT-AND-READ-FUNCTION)
  "Proceeds, asking for a name of a package to use instead."
  (FUNCALL PROCEED-FUNCTION ':NEW-NAME
	   (FUNCALL PROMPT-AND-READ-FUNCTION ':STRING
		    "Name (not local nickname) of package to use instead: ")))

(DEFMETHOD (PACKAGE-NOT-FOUND :CASE :PROCEED-ASKING-USER :CREATE-PACKAGE)
	   (PROCEED-FUNCTION IGNORE)
  "Creates the package and proceeds."
  (FORMAT T "Creating package ~A." (SEND SELF ':NAME))
  (FUNCALL PROCEED-FUNCTION ':CREATE-PACKAGE))

(compile-flavor-methods package-not-found)

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN (:PACKAGE FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE PKG)
  (VALUES (NCONS 'PACKAGE) (NCONS (PKG-FIND-PACKAGE PKG ':ERROR))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN (PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
       LAST-CH ;ignored
       (PROG (THING TYPE PK
	      ;; Help un-screw the user if PACKAGE gets set to NIL.
	      (PACKAGE (OR PACKAGE PKG-USER-PACKAGE))
	      INTERNAL-OK ENTIRE-LIST-PREFIXED)
	     ;; Gobble the second colon, if any, and set flag if found.
	     ;; Note that we do not, currently, DO anything with the flag!
	     (MULTIPLE-VALUE-BIND (CH NUM REAL-CH)
		 (XR-XRTYI STREAM NIL T)
	       (IF (= CH #/:)
		   (SETQ INTERNAL-OK T)
		 (IF (= CH #/()
		     (SETQ ENTIRE-LIST-PREFIXED T))
		 (XR-XRUNTYI STREAM REAL-CH NUM)))
	     ;; Try to find the package.
	     (DO ((STRING1 (OR STRING "")))
		 ;;don't try to find packages if we're not interning -- eg +slime (dis:foo)
		 ((OR *READ-SUPPRESS*
		      (SETQ PK (FIND-PACKAGE STRING1 PACKAGE))))
	       ;; Package not found.
	       (SIGNAL-PROCEED-CASE ((PKG) 'SYS:READ-PACKAGE-NOT-FOUND
					       "Package ~S does not exist."
					       STRING1)
		 (:NO-ACTION
		  (RETURN))
		 (:NEW-NAME
		  (LET ((PACKAGE PKG-USER-PACKAGE))
		    (SETQ STRING1 (STRING (READ-FROM-STRING PKG)))))
		 (:CREATE-PACKAGE
		  (OR (FIND-PACKAGE STRING1 PACKAGE)
		      (MAKE-PACKAGE STRING1)))))
	     (UNLESS PK
	       (SETQ PK PKG-USER-PACKAGE))
	     (WHEN STRING (RETURN-READ-STRING STRING))
	     (LET ((PACKAGE PK)
		   (READ-INTERN-FUNCTION
		     (COND ((AND (PACKAGE-AUTO-EXPORT-P PK)
				 (PACKAGE-USED-BY-LIST PK))
			    'READ-INTERN-SOFT)
			   (T 'INTERN))
		     ;; This change may occur only in Common Lisp.
		     #| (IF (OR ENTIRE-LIST-PREFIXED (EQ PK PACKAGE))
			 ;; Here for, e.g., SI: while in SI already.
			 ;; There are things in LOOP which MUST say "SI:" even though
			 ;; loop is loaded into SI on the Lisp machine.
			 ;; Also here for ZWEI:(BP-LINE (POINT));
			 ;; such constructs are not valid Common Lisp
			 ;; so let's keep their meaning the same.
			 READ-INTERN-FUNCTION
		       (IF READ-COLON-ALLOW-INTERNALS
			   'READ-PACKAGE-PREFIX-INTERN
			 'READ-PACKAGE-PREFIX-EXTERNAL-INTERN)) |#))
	       (MULTIPLE-VALUE (THING TYPE)
		 (READ STREAM)))
	     (RETURN THING TYPE T)))   ;T means we already did RETURN-READ-STRING

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFSIGNAL READ-PACKAGE-NOT-FOUND (PACKAGE-NOT-FOUND PACKAGE-ERROR READ-ERROR)
	   (NAME RELATIVE-TO)
	   "Package prefix not recognized as a package name.")

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFUN PARSE-PATHNAME (THING &OPTIONAL WITH-RESPECT-TO (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
		       BREAK-CHARS (START 0) END &OPTIONAL (ERRORP T))
  "Parse THING into a pathname and return it.
THING can be a pathname already (it is just passed back),
 a string or symbol, or a Maclisp-style namelist.
WITH-RESPECT-TO can be NIL or a host or host-name;
 if it is not NIL, the pathname is parsed for that host
 and it is an error if the pathname specifies a different host.
If WITH-RESPECT-TO is NIL, then DEFAULTS is used to get the host
 if none is specified.  DEFAULTS may be a host object in this case.
START and END are indices specifying a substring of THING to be parsed.
 They default to 0 for START and NIL (meaning end of THING) for END.
BREAK-CHARS is a list of break chars; parsing stops if any of them is encountered.
 This is passed to STRING-SEARCH-SET.
The second value is the index in THING at which parsing stopped.
 If ERRORP is NIL and there was an error due to invalid syntax,
 this is the index of the invalid character."
  (DECLARE (VALUES PARSED-PATHNAME PARSE-END-INDEX))
  (AND WITH-RESPECT-TO
       (SETQ WITH-RESPECT-TO (GET-PATHNAME-HOST WITH-RESPECT-TO)))
  (WHEN BREAK-CHARS
    (SETQ END (STRING-SEARCH-SET BREAK-CHARS THING START END)))
  (CONDITION-RESUME '((PATHNAME-ERROR) :NEW-PATHNAME T ("Proceed, supplying a new pathname.")
		      PARSE-PATHNAME-THROW-NEW-PATHNAME)
    (LET ((PARSE-PATHNAME-FLAG (NOT ERRORP)))
      (CATCH-CONTINUATION 'PARSE-PATHNAME
	  #'(LAMBDA (INDEX-OR-PATHNAME) 
	      (IF (NUMBERP INDEX-OR-PATHNAME)
		  (VALUES NIL (MIN (OR END (LENGTH THING)) INDEX-OR-PATHNAME))
		(VALUES INDEX-OR-PATHNAME START)))
	  NIL
	(COND ((TYPEP THING 'PATHNAME)
	       (AND WITH-RESPECT-TO (NEQ WITH-RESPECT-TO (PATHNAME-HOST THING))
		    (FERROR 'PATHNAME-PARSE-ERROR
			    "Host ~A in ~A does not match ~A"
			    (PATHNAME-HOST THING) THING WITH-RESPECT-TO))
	       (VALUES THING START))
	      ((CONSP THING)
	       (SETQ THING (CANONICALIZE-KLUDGEY-MACLISP-PATHNAME-STRING-LIST THING))
	       (LET (DEVICE DIRECTORY NAME TYPE VERSION HOST)
		 (COND ((LISTP (CAR THING))
			(SETF `((,DEVICE ,DIRECTORY) ,NAME ,TYPE ,VERSION) THING))
		       ((NUMBERP (THIRD THING))
			(SETF `(,NAME ,TYPE ,VERSION ,DEVICE ,DIRECTORY) THING))
		       (T
			(SETF `(,NAME ,TYPE ,DEVICE ,DIRECTORY ,VERSION) THING)))
		 (SETQ HOST (COND ((GET-PATHNAME-HOST DEVICE T))
				  (WITH-RESPECT-TO)
				  ((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
				  (T (DEFAULT-HOST DEFAULTS))))
		 (AND WITH-RESPECT-TO
		      (NEQ WITH-RESPECT-TO HOST)
		      (FERROR 'PATHNAME-PARSE-ERROR
			      "Host ~A in ~A does not match ~A" HOST THING WITH-RESPECT-TO))
		 (VALUES (MAKE-PATHNAME ':HOST HOST
					':DEVICE DEVICE ':DIRECTORY DIRECTORY ':NAME NAME
					':TYPE TYPE ':VERSION VERSION)
			 START)))
	      (T
	       (SETQ THING (STRING THING))
	       (MULTIPLE-VALUE-BIND (HOST-SPECIFIED START END)
		   (PARSE-PATHNAME-FIND-COLON THING START END)
		 ;; If the thing before the colon is really a host,
		 ;; and WITH-RESPECT-TO was specified, then they had better match
		 (AND WITH-RESPECT-TO
		      HOST-SPECIFIED
		      (NEQ WITH-RESPECT-TO HOST-SPECIFIED)
		      ;; Otherwise treat it as a device name
		      (SETQ HOST-SPECIFIED NIL START 0 END NIL))
		 (LET* ((HOST
			  (COND ((GET-PATHNAME-HOST HOST-SPECIFIED T))
				(WITH-RESPECT-TO)
				((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
				(T (DEFAULT-HOST DEFAULTS)))))
		   (MULTIPLE-VALUE-BIND (DEVICE DIRECTORY NAME TYPE VERSION PARSE-END)
		       (FUNCALL (SAMPLE-PATHNAME HOST) ':PARSE-NAMESTRING
				(NOT (NULL HOST-SPECIFIED)) THING START END)
		     (VALUES
		       ;; If device is :NO-INTERN then immeditely return 2nd value, DIRECTORY.
		       ;; this provides a way to bypass as much of this lossage as possible
		       ;; in cases where it doesnt make sense.
		       (COND ((EQ DEVICE ':NO-INTERN)
			      DIRECTORY)
			     (T
			      ;; Otherwise we assume we got the raw forms of everything.
			      (MAKE-PATHNAME-INTERNAL
				HOST DEVICE DIRECTORY NAME TYPE VERSION)))
		       PARSE-END))))))))))

))

; From file PRIMIT.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PRIMIT  "

(DEFUN RELEVANT-FUNCTION-NAME (BP &OPTIONAL STRINGP (FUNCTION-ONLY T) (FUNCALL-SPECIAL T)
				  &AUX START-BP X)
  "Return a function spec obtained from the text around BP.
STRINGP = T says print the spec into a string and return that.
FUNCTION-ONLY means only consider actually defined functions (default T).
FUNCALL-SPECIAL (default T) means if BP points inside a FUNCALL or SEND,
see if it looks like sending a message and try to return
a function spec for an existing method it might call."
  ;; If we are in or next to a #' or what follows it,
  ;; set START-BP to point before it.  Otherwise START-BP is NIL.
  ;; We do not here detect #' preceding a list we are in; that is detected later.
  (COND ((LOOKING-AT BP "#'") (SETQ START-BP BP))
	((LOOKING-AT (FORWARD-CHAR BP -1 T) "#'") (SETQ START-BP (FORWARD-CHAR BP -1 T)))
	((LOOKING-AT (FORWARD-CHAR BP -2 T) "#'") (SETQ START-BP (FORWARD-CHAR BP -2 T)))
	(T (SETQ START-BP (FORWARD-SEXP BP -1 T))
	   (IF (NOT (LOOKING-AT START-BP "#'"))
	       (SETQ START-BP NIL)
	     (IF (BP-< (FORWARD-SEXP START-BP 1 T 0 (FORWARD-CHAR BP 1)) BP)
		 (SETQ START-BP NIL)))))
  ;; If START-BP was set, and the following thing is a good function, use that.
  (IF (AND START-BP
	   (LET ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT))
	     (SETQ X (BP-READ-OBJECT (FORWARD-CHAR START-BP 2))))
	   (OR (NOT FUNCTION-ONLY)
	       (ZMACS-DEFINEDP X)))
      (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)
    ;; Otherwise, look up in list structure to find either
    ;; a function we are in a call to, or a level preceded by #'.
    (SETQ START-BP (FORWARD-DEFUN BP -1 T))
    (DO ((BP1 BP)
	 (FIRST-TIME T NIL)
	 (FN-START)
	 (FN-END)
	 (X))
	((NULL (SETQ BP1 (FORWARD-SEXP BP1 -1 NIL 1 START-BP NIL)))
	 ;; If bp is at beginning of a defun, get the function it defines.
	 (AND FIRST-TIME
	      (TYPEP (BP-NODE BP) 'SECTION-NODE)
	      (SETQ X (SECTION-NODE-NAME (BP-NODE BP)))
	      (NOT (STRINGP X))
	      (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)))
      ;; If this level of list structure is preceded by #',
      ;; maybe return this whole list.
      (AND (LOOKING-AT (FORWARD-CHAR BP1 -2 T) "#'")
	   (SETQ X (BP-READ-OBJECT BP1))
	   (OR (NOT FUNCTION-ONLY)
	       (ZMACS-DEFINEDP X))
	   (RETURN (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)))
      ;; Otherwise consider the function which is the car of this list.
      (OR (SETQ FN-START (FORWARD-CHAR BP1)) (RETURN NIL))
      (OR (SETQ FN-END (FORWARD-SEXP FN-START)) (RETURN NIL))
      (COND ((AND (EQ (BP-LINE FN-START) (BP-LINE FN-END))
		  (LET ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT))
		    (SETQ X (BP-READ-OBJECT FN-START)))
		  (SYMBOLP X)
		  (OR (NOT FUNCTION-ONLY)
		      (ZMACS-DEFINEDP X)
		      (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST X))))
	     ;; If default is DEFUN, etc., get the function being defined.
	     (AND (TYPEP (BP-NODE FN-START) 'SECTION-NODE)
		  (= (BP-INDEX FN-START) 1)
		  (EQ (BP-LINE FN-START)
		      (SECTION-NODE-DEFUN-LINE (BP-NODE FN-START)))
		  (NOT (STRINGP (SECTION-NODE-NAME (BP-NODE FN-START))))
		  (SETQ X (SECTION-NODE-NAME (BP-NODE FN-START))))
	     ;; Rather than one of certain well-known functionals,
	     ;; use the function to be passed to it.
	     (AND (OR (MEMQ X '(FUNCALL LEXPR-FUNCALL APPLY SEND LEXPR-SEND
					MAP MAPC MAPCAR MAPLIST MAPCAN MAPCON)))
		  FUNCALL-SPECIAL
		  (SETQ FN-START (FORWARD-OVER *WHITESPACE-CHARS* FN-END))
		  (SETQ FN-END (FORWARD-SEXP FN-START))
		  (EQ (BP-LINE FN-START) (BP-LINE FN-END))
		  (LET* ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT)
			 (Y (BP-READ-OBJECT FN-START)))
		    (AND (CONSP Y) (MEMQ (CAR Y) '(QUOTE FUNCTION))
			 (SYMBOLP (SETQ Y (CADR Y)))
			 (OR (NOT FUNCTION-ONLY)
			     (ZMACS-DEFINEDP Y))
			 (SETQ X Y))))
	     (RETURN (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)))))))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFUN ADDRESS-PARSE (ADDRESS &AUX HOST)
  "Coerce the argument to a chaosnet address.
The argument can be a host name or host object, or an address."
  (CONDITION-CASE (ERROR)
      (COND ((FIXP ADDRESS) ADDRESS)
	    ((AND (TYPEP ADDRESS ':INSTANCE)
		  (FUNCALL ADDRESS ':SEND-IF-HANDLES ':CHAOS-ADDRESS)))
	    ((AND (SETQ HOST (SI:PARSE-HOST ADDRESS T))
		  (FUNCALL HOST ':CHAOS-ADDRESS)))
	    ((AND (STRINGP ADDRESS)
		  (PARSE-NUMBER ADDRESS 0 NIL 8))))
    (SYS:UNCLAIMED-MESSAGE NIL)))

))

; From file QCDEFS.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFMACRO LOCKING-RESOURCES-NO-QFASL (&BODY BODY)
  "Allocate a temporary area and a QCMP-OUTPUT for this process.
Use this when compiling to core.
Does not set up fasd tables, to save time."
  `(FLET ((FOO () . ,BODY))
     (IF QCOMPILE-TEMPORARY-AREA
	 (FOO)
       (USING-RESOURCE (TEMPS COMPILER-TEMPORARIES-RESOURCE)
	 (LET ((QCOMPILE-TEMPORARY-AREA (FIRST TEMPS))
	       (FASD-HASH-TABLE NIL)
	       (FASD-EVAL-HASH-TABLE NIL)
	       (QCMP-OUTPUT (FOURTH TEMPS)))
	   (RESET-TEMPORARY-AREA QCOMPILE-TEMPORARY-AREA)
	   (SETF (FILL-POINTER QCMP-OUTPUT) 0)
	   (PROG1
	     (FOO)
	     ;; Get rid of as much as possible to make saved band smaller.
	     (RESET-TEMPORARY-AREA QCOMPILE-TEMPORARY-AREA)
	     (ARRAY-INITIALIZE QCMP-OUTPUT NIL)))))))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
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
  (SETQ GENERIC-PATHNAME (SEND *INTERVAL* ':GENERIC-PATHNAME))
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
  (REMPROP (MAKE-REGISTER-NAME #/.) 'POINT)
  (LET (SI:INTERPRETER-ENVIRONMENT (SI:INTERPRETER-FUNCTION-ENVIRONMENT T))
    (MULTIPLE-VALUE-BIND (VARS VALS) (SEND *INTERVAL* ':ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
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
			  PACKAGE
			  NIL			;FILE-LOCAL-DECLARATIONS
			  NIL			;Unused
			  WHOLE-FILE)))
	    (IF COMPILE-P
		(COMPILER:LOCKING-RESOURCES-NO-QFASL (DO-IT))
	      (DO-IT)))))))
  (OR (NULL GENERIC-PATHNAME)
      (SI:RECORD-FILE-DEFINITIONS GENERIC-PATHNAME SI:FDEFINE-FILE-DEFINITIONS WHOLE-FILE)))

))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-INTERVAL-PROCESS-BASIC-FORM (FORM TYPE)
  (DECLARE (SPECIAL COMPILE-P PRINT-RESULTS-STREAM))
  ;; Really eval or compile the thing.
  (COND ((EQ COMPILE-P T)
	 (COMPILE-BUFFER-FORM FORM TYPE))
	(COMPILE-P
	 (FUNCALL COMPILE-P FORM))
	(T
	 (EVAL-PRINT FORM PRINT-RESULTS-STREAM))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN COMPILE-LAMBDA (LAMBDA-EXP &OPTIONAL NAME (PROCESSING-MODE 'MACRO-COMPILE))
  "Compile the function LAMBDA-EXP and return a compiled-function object.
That compiled function will record NAME as its name,
but we do not actually define NAME."
  (AND QC-FILE-IN-PROGRESS	;Check for condition likely to cause temporary area lossage
       (FORMAT ERROR-OUTPUT "~&COMPILE: Compiler recursively entered, you may lose.~%"))
  (LOCKING-RESOURCES-NO-QFASL
    (FILE-OPERATION-WITH-WARNINGS (T ':COMPILE)
      (COMPILER-WARNINGS-CONTEXT-BIND
	(LET (TEM FILE-SPECIAL-LIST FILE-UNSPECIAL-LIST FILE-LOCAL-DECLARATIONS
	      (INHIBIT-FDEFINE-WARNINGS T))
	  (QC-PROCESS-INITIALIZE)
	  (COMPILE-1 `(:LOCATION ,(LOCF TEM)) LAMBDA-EXP PROCESSING-MODE NAME)
	  TEM)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN COMPILE (NAME &OPTIONAL LAMBDA-EXP)
  "Compile the definition of NAME,
or its previous interpreted definition if it is already compiled.
If LAMBDA-EXP is supplied, it is compiled and made the definition of NAME.
If NAME is NIL, LAMBDA-EXP is compiled and the result is just returned."
  (AND QC-FILE-IN-PROGRESS	;Check for condition likely to cause temporary area lossage
       (FORMAT ERROR-OUTPUT "~&COMPILE: Compiler recursively entered, you may lose.~%"))
  (IF (NULL NAME)
      (COMPILE-LAMBDA LAMBDA-EXP (GENSYM))
    (LOCKING-RESOURCES-NO-QFASL
      (FILE-OPERATION-WITH-WARNINGS (T ':COMPILE)
	(COMPILER-WARNINGS-CONTEXT-BIND
	  (LET (TEM FILE-SPECIAL-LIST FILE-UNSPECIAL-LIST FILE-LOCAL-DECLARATIONS)
	    (QC-PROCESS-INITIALIZE)
	    (COND (LAMBDA-EXP)
		  ((AND (FDEFINEDP NAME)
			(CONSP (SETQ TEM (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC NAME)))))
		   (SETQ LAMBDA-EXP TEM))
		  ((AND (FDEFINEDP NAME)
			(SETQ TEM (ASSQ 'INTERPRETED-DEFINITION
					(DEBUGGING-INFO
					  (FDEFINITION
					    (SI:UNENCAPSULATE-FUNCTION-SPEC NAME))))))
		   (SETQ LAMBDA-EXP (CADR TEM)))
		  (T (FERROR NIL "Can't find LAMBDA expression for ~S" NAME)))
	    (LET ((INHIBIT-FDEFINE-WARNINGS T))
	      (COMPILE-1 NAME LAMBDA-EXP))
	    NAME))))))

))
