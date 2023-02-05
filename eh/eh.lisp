;;; New error handler.        DLW 1/5/78 -*- Mode:LISP; Package:EH; Base:8; Readtable:T -*-

(DEFVAR ERROR-MESSAGE-PRINLEVEL 2)	;These are used when printing error messages
(DEFVAR ERROR-MESSAGE-PRINLENGTH 4)	; and values of variables in frames.
(DEFVAR FUNCTION-PRINLEVEL 3)		; Used for printing LAMBDA expressions.
(DEFVAR FUNCTION-PRINLENGTH 5)

(DEFVAR ERROR-DEPTH 0
  "Depth within error handlers in this stack group.")

;;; The error table, read from SYS: UBIN; UCADR TBL nnn into MICROCODE-ERROR-TABLE,
;;; describes the symbolic meaning of certain microcode pcs.
;;; Its data is rearranged into other variables below.
;;; ERROR-TABLE relates the micro pc to a symbolic name of error,
;;; called an ETE, whose car will then have properties saying how to
;;; construct a condition instance.  The properties are defined in EHF.

(DEFVAR MICROCODE-ERROR-TABLE :UNBOUND
  "Actual error table read in from file (SYS: UBIN; UCADR TBL)")

(DEFVAR MICROCODE-ERROR-TABLE-VERSION-NUMBER 0
  "Ucode version number to which the loaded value of MICROCODE-ERROR-TABLE pertains.")

;;; ASSURE-TABLE-PROCESSED looks at MICROCODE-ERROR-TABLE
;;; and produces these lists.
(DEFVAR CALLS-SUB-LIST)			;Alist of micropcs to symbols.
(DEFVAR RESTART-LIST)			;Alist of symbols to micropcs.
(DEFVAR ARG-POPPED-LIST)		;Alist of micropcs just after where
					;misc insns pop their args.
					;The cdr of the element says where the arg went:
					;a place to find it if the error is after the pop.
(DEFVAR DEFAULT-ARG-LOCATIONS-LIST)	;Alist of microfun symbols to where there args
					;live in the absense of info to contrary.
(DEFVAR STACK-WORDS-PUSHED-LIST)	;Alist of micropcs, of error or on stack at error,
					;to how many words that subroutine had pushed
					;on the stack since it was called, up to the time
					;it called the next subroutine or got the error.
(DEFVAR ERROR-TABLE)			;List of ETEs.
(DEFVAR ERROR-TABLE-NUMBER -1)		;Microcode version number for ERROR-TABLE.
(DEFVAR BEGIN-QARYR)			;See SG-ERRING-FUNCTION
(DEFVAR END-QARYR)			;..

;;; An error immediately runs the first level error handler stack group
;;; whose job is to initialize a second level error handler stack group
;;; in which the error handler actually runs.

(DEFVAR SECOND-LEVEL-ERROR-HANDLER-COUNT 0
  "Number of second level error handler stack groups made, for making unique names.")
(DEFVAR FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST NIL
  "List of error handler stack groups that were exited and can be reused.")

(DEFVAR FREE-SAVED-PDL-SEGMENT-LIST NIL
  "List of arrays that can be used for saving stack segments.")

(DEFCONST SAVED-PDL-SEGMENT-MINIMUM-SIZE #o1000)

(DEFUN ALLOCATE-SAVED-PDL-SEGMENT (SIZE)
  (IF FREE-SAVED-PDL-SEGMENT-LIST
      (LET ((SEGMENT (WITHOUT-INTERRUPTS (POP FREE-SAVED-PDL-SEGMENT-LIST))))
	(UNLESS ( (LENGTH SEGMENT) SIZE)
	  (ADJUST-ARRAY-SIZE SEGMENT (+ SIZE #o1000)))
	SEGMENT)
    (MAKE-ARRAY (MAX SIZE SAVED-PDL-SEGMENT-MINIMUM-SIZE)
		':TYPE ART-Q ':AREA WORKING-STORAGE-AREA)))

(DEFUN FREE-SAVED-PDL-SEGMENT (SEGMENT)
  (SETF (AREF SEGMENT 0) NIL)
  (%BLT (LOCF (AREF SEGMENT 0)) (LOCF (AREF SEGMENT 1))
	(1- (LENGTH SEGMENT)) 1)
  (PUSH SEGMENT FREE-SAVED-PDL-SEGMENT-LIST))

;;; This is so that each of them can tell
;;; when it is returned to whether some other one
;;; has been running in the meanwhile.
(DEFVAR LAST-SECOND-LEVEL-ERROR-HANDLER-SG NIL
  "The last second-level error handler stack group that was running.")
(DEFVAR ERROR-HANDLER-RUNNING NIL
  "This variable is bound to T in every second-level error handler to identify them.")

(DEFVAR ERROR-HANDLER-REPRINT-ERROR T
  "Non-NIL => RUN-SG should reprint the error message on return from debugged SG.")

(DEFVAR ERROR-HANDLER-AREA
	(MAKE-AREA ':NAME 'ERROR-HANDLER-AREA)
  "The error handler conses in this area, to make sure it can operate
even if other crucial areas are full.")

(DEFVAR ERROR-HANDLER-SPACE-WARNING-GIVEN NIL
  "T if we have run out of address space and told the user about it.")

;;; Must be large enough for a second level sg's special pdl, and then some.
;;; This value is 1/4 of the minimum region size.
(DEFCONST MINIMUM-FREE-SPACE #o10000
  "Try to keep this much free space in both list and structure regions of ERROR-HANDLER-AREA.")

(DEFUN ASSURE-FREE-SPACE (&AUX STRUCTURE-SUCCESS LIST-SUCCESS)
  "Add more space to ERROR-HANDLER-AREA if needed,
to make sure we have a reserve of free space in it."
  (WITHOUT-INTERRUPTS
    (CONDITION-CASE ()
        (LET ((ARRAY (MAKE-ARRAY MINIMUM-FREE-SPACE ':AREA ERROR-HANDLER-AREA)))
	  (RETURN-STORAGE ARRAY)
	  (SETQ STRUCTURE-SUCCESS T))
      (ERROR NIL))
    (CONDITION-CASE ()
        (LET ((LIST (MAKE-LIST MINIMUM-FREE-SPACE ':AREA ERROR-HANDLER-AREA)))
	  (RETURN-STORAGE LIST)
	  (SETQ LIST-SUCCESS T))
      (ERROR NIL)))
  (OR (AND LIST-SUCCESS STRUCTURE-SUCCESS)
      ERROR-HANDLER-SPACE-WARNING-GIVEN
      (PROGN (SETQ ERROR-HANDLER-SPACE-WARNING-GIVEN T)
	     (FORMAT T "~&Warning: error handler consing space is getting low!"))))

(ADD-INITIALIZATION 'ASSURE-FREE-SPACE
		    '(ASSURE-FREE-SPACE) '(:AFTER-FLIP :NORMAL))

;;; The error handler uses the value of this variable
;;; in the stack group that got the error
;;; as the stream to do its i/o on.
(DEFVAR *DEBUG-IO* NIL
  "Stream for use by the debugger command loop.")
(DEFVAR DEBUG-IO :UNBOUND
  "Stream for use by the debugger command loop.")
(FORWARD-VALUE-CELL 'DEBUG-IO '*DEBUG-IO*)

(DEFVAR *DEBUG-IO-OVERRIDE* NIL
  "If non-NIL, this is used for debugging instead of *DEBUG-IO*")

;; ERRSET is T if the error handler should be entered despite being in an errset.
(DEFVAR ERRSET-STATUS NIL
  "T within an ERRSET.")
(DEFVAR ERRSET-PRINT-MSG NIL
  "T if the ERRSET error message should be printed anyway.")
(DEFVAR ERRSET NIL
  "Non-NIL means enter the debugger even within ERRSETs and CATCH-ERRORs.")

(DEFVAR ERROR-MESSAGE-HOOK NIL
  "Function for the debugger to call after it prints an error message.")

;;; Here are the error handler's main operating parameters.
(DEFVAR ERROR-SG :UNBOUND
  "Within the debugger, the stack group that got the error.")
;;; Note, this needs a top-level value because SG-APPLY and SG-EVAL unmodularly depend on it
(DEFVAR CURRENT-FRAME NIL
  "The SG-AP of the frame that the error handler is looking at.")
(DEFVAR ERROR-LOCUS-FRAME NIL
  "The innermost visible frame which is not part of the error signaling mechanism.")
(DEFVAR INNERMOST-VISIBLE-FRAME :UNBOUND
  "Frames on stack inside of this can't be moved to.
Also, this can point at a frame that isn't really active (is inside of SG-AP),
to allow that frame to be selected even though it isn't active.")

(DEFVAR INNERMOST-FRAME-IS-INTERESTING NIL
  "T if we should regard the innermost frame as interesting
even if it is a call to a normally uninteresting function.
This is set when we break on entry to or exit from an uninteresting function.")

(DEFVAR EH-SG NIL
  "In expressions evaluated by the debugger, this is the stack group that got the error.")

(DEFVAR EH-FRAME NIL
  "In expressions evaluated by the debugger, this is the frame pointer of the current frame.")

(DEFVAR EH-ERROR NIL
  "Inside condition handlers and the debugger, this is an error object
describing the error being handled.")

;;; This is a random gensymmed object which is returned
;;; from SG-EVAL to indicate that an error occurred within.
(DEFVAR ERROR-FLAG (NCONS NIL))

(DEFCONST ERROR-MESSAGE-BACKTRACE-LENGTH 3
  "Number of levels of backtrace to print automatically upon error.")

(DEFCONST DISASSEMBLE-INSTRUCTION-COUNT 10.
  "Number of instructions to disassemble for M-L, etc., if we
can't determine the amount of room on the screen.
This is also the minimum number of instructions to disassemble.")

;;;*** Not patched in system 98 -- functions nonexistent in 99 have been removed ***
;; Calls to these functions should not be mentioned as frames
;; when stack-censoring is going on in interpreted functions.
;; This should include all functions that have &QUOTE args and are open-compiled.
;; EVAL1 and APPLY-LAMBDA are there for peculiar reasons.
(DEFCONST UNINTERESTING-FUNCTIONS '(EVAL SI:EVAL1 SI:APPLY-LAMBDA
				    SETQ COMPILER-LET *THROW PROGN DONT-OPTIMIZE
				    WITH-STACK-LIST WITH-STACK-LIST*
				    PROG PROG* TAGBODY SI::TAGBODY-INTERNAL BLOCK
				    LET LET* RETURN RETURN-FROM
				    DO DO-NAMED SI::DO-INTERNAL
				    DO* DO*-NAMED SI::DO*-INTERNAL SI::DO-BODY
				    MULTIPLE-VALUE MULTIPLE-VALUE-LIST
				    MULTIPLE-VALUE-PROG1 MULTIPLE-VALUE-CALL
				    FUNCTION FLET LABELS MACROLET
				    PROGV PROGW LET-IF UNWIND-PROTECT
				    BREAKON-THIS-TIME COND AND OR STORE)
  "Run-time support functions for the interpreter, which the debugger should not show you.")

(DEFVAR SG-STEPPING-TABLE NIL
  "Table of stack groups being stepped and stack groups stepping them.")

;;; These logically go in SGDEFS but that isn't loaded till much later.
(DEFVAR CURRENT-STACK-GROUP :UNBOUND
  "The stack group now running.")

(DEFVAR CURRENT-STACK-GROUP-RESUMER :UNBOUND
  "The stack group that resumed the one now running.")

(FORWARD-VALUE-CELL 'CURRENT-STACK-GROUP '%CURRENT-STACK-GROUP)
(FORWARD-VALUE-CELL 'CURRENT-STACK-GROUP-RESUMER
		    '%CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)

(DEFMACRO PRINT-CAREFULLY (IGNORE &BODY BODY)
  "If BODY gets an error, print a message saying /"error printing/"."
  `(CONDITION-BIND ((ERROR 'PROCEED-WITH-ABORT-PRINTING))
     . ,BODY))

(DEFUN PROCEED-WITH-ABORT-PRINTING (CONDITION)
  (WHEN (MEMQ ':ABORT-PRINTING (SEND CONDITION ':PROCEED-TYPES))
    ':ABORT-PRINTING))

;Might as well have this loaded as soon as condition handling can run.
(DEFUN SI:CONDITION-CASE-THROW (ERROR TAG)
  (*THROW TAG ERROR))

(DEFUN SI:IGNORE-ERRORS-HANDLER (CONDITION TAG)
  (UNLESS (SEND CONDITION ':DANGEROUS-CONDITION-P)
    (*THROW TAG T)))

(DEFUN SI:ERRSET-HANDLER (CONDITION TAG PRINTFLAG)
  (UNLESS (OR ERRSET (SEND CONDITION ':DANGEROUS-CONDITION-P))
    (WHEN PRINTFLAG
      (TERPRI ERROR-OUTPUT)
      (SEND CONDITION ':PRINT-ERROR-MESSAGE CURRENT-STACK-GROUP T ERROR-OUTPUT))
    (*THROW TAG CONDITION)))

(DEFUN SI:CATCH-ERROR-RESTART-THROW (IGNORE TAG)
  (*THROW TAG NIL))

;;; Save a stack group's state on its stack so we can use it and then restore the state.
;;; The information goes on the pdl in a fake frame belonging to the function FOOTHOLD.
;;; Each Q is saved as 2 words (pointer and tag) to avoid data type problems.
;;; You must call this before pushing a call block, even if calling SG-RUN-GOODBYE,
;;; in order to clean up the QBBFL and the U-STACK Q's.
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
  (SETF (AREF RP (+ NEW-AP %LP-CALL-STATE))
	(DPB (- NEW-AP (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
	     (DPB (- NEW-AP (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
		  0)))
  (SETF (AREF RP (+ NEW-AP %LP-EXIT-STATE))
	(DPB (FEF-INITIAL-PC #'FOOTHOLD) %%LP-EXS-EXIT-PC 0))	
  (SETF (AREF RP (+ NEW-AP %LP-ENTRY-STATE)) 0)
  (SETF (AREF RP (+ NEW-AP %LP-FEF)) #'FOOTHOLD)
  (SETQ PP (1+ NEW-AP))
  (DOTIMES (I (1+ SG-PDL-PHASE))
    (SETQ P (AP-LEADER SG I))
    (SETF (AREF RP PP)
	  (IF (%P-POINTERP P)
	      (%P-CONTENTS-AS-LOCATIVE P)
	    (%P-POINTER P)))
    (SETF (AREF RP (1+ PP))
	  (%P-LDB %%Q-ALL-BUT-POINTER P))
    (SETQ PP (+ PP 2)))
  (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))	;Index of last valid word
  (SETF (SG-FLAGS-QBBFL SG) 0)			;Clear QBBFL left over from previous frame
  (SETF (SG-IPMARK SG) NEW-AP)
  (SETF (SG-AP SG) NEW-AP))

(DEFUN FOOTHOLD ()
  "This function isn't normally called, it just exists to name state-save frames.
If this function is ever returned to (see (:METHOD PROCESS :INTERRUPT))
then it will restore the saved state and resume it.
Do not trace nor redefine this function!"
 (FUNCALL %ERROR-HANDLER-STACK-GROUP '(RESUME-FOOTHOLD)))

(DEFUN SG-RESTORE-STATE (SG &OPTIONAL (N-FRAMES-BACK 1))
  "Undo SG-SAVE-STATE on SG.  Pop saved state from pdl back into saved accumulators.
N-FRAMES-BACK is the number of stack frames on the pdl to discard
before finding the data to be popped."
  (LET ((PP (SG-AP SG))
	(RP (SG-REGULAR-PDL SG)))
    (DOTIMES (I N-FRAMES-BACK) (SETQ PP (SG-NEXT-ACTIVE SG PP)))
    (AND (NULL PP)
	 (FERROR NIL "~S state not saved" SG))
    (OR (EQ (AREF RP PP) #'FOOTHOLD)
	(FERROR NIL "Saved state for ~S at ~S[~S] clobbered." SG RP PP))
    (INCF PP)
    (DOTIMES (I (1+ SG-PDL-PHASE))
      (%P-STORE-TAG-AND-POINTER (AP-LEADER SG I)
				(AREF RP (1+ PP))
				(AREF RP PP))
      (INCF PP 2))))

;;;; Low level routines for manipulating the stacks of a stack group.

;;; Call SG-SAVE-STATE before calling any of these.

(DEFUN SG-REGPDL-PUSH (X SG &AUX PP)
  "Push the value X on to the regular pdl of stack group SG."
  (SETQ PP (1+ (SG-REGULAR-PDL-POINTER SG)))
  (SETF (AREF (SG-REGULAR-PDL SG) PP) X)
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG) PP) CDR-NEXT)
  (SETF (SG-REGULAR-PDL-POINTER SG) PP)
  (INCF (SG-PDL-PHASE SG))
  X)

(DEFUN SG-REGPDL-POP (SG &AUX PP)
  "Pop and return the top word of the regular pdl of SG."
  (SETF (SG-PDL-PHASE SG) (1- (SG-PDL-PHASE SG)))
  (SETQ PP (SG-REGULAR-PDL-POINTER SG))
  (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))
  (AREF (SG-REGULAR-PDL SG) PP))

(DEFUN SG-SPECPDL-PUSH (X SG FLAG &AUX PP PDL)
  "Push the value X onto the special pdl of stack group SG.
Set the %%SPECPDL-BLOCK-START-flag bit of the word pushed according to FLAG."
  (SETQ PP (1+ (SG-SPECIAL-PDL-POINTER SG)))
  (SETF (SG-SPECIAL-PDL-POINTER SG) PP)
  (SETQ PDL (SG-SPECIAL-PDL SG))
  (SETF (AREF PDL PP) X)
  (%P-DPB FLAG %%SPECPDL-BLOCK-START-FLAG (ALOC PDL PP))
  X)

(DEFUN SG-SPECPDL-POP (SG &AUX PP)
  "Pop and return the top word of the special pdl of SG."
  (SETQ PP (SG-SPECIAL-PDL-POINTER SG))
  (SETF (SG-SPECIAL-PDL-POINTER SG) (1- PP))
  (AREF (SG-SPECIAL-PDL SG) PP))

;;; This simulates the CBM (or P3ZERO) routine in the microcode.
;;; It is what a CALL instruction does.
;;; You must call SG-SAVE-STATE before calling this.
(DEFUN SG-OPEN-CALL-BLOCK (SG DESTINATION FUNCTION &AUX PP NEW-IPMARK)
  "Push a call block for calling FUNCTION onto the regular pdl of stack group SG.
DESTINATION is the same as a destination for the CALL instruction
/(except that it must be a number).
You must do SG-SAVE-STATE before doing this."
  (SETQ PP (SG-REGULAR-PDL-POINTER SG))
  (SETQ NEW-IPMARK (+ PP %LP-CALL-BLOCK-LENGTH))
  (SG-REGPDL-PUSH (DPB (- NEW-IPMARK (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
		       (DPB (- NEW-IPMARK (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
			    (DPB DESTINATION %%LP-CLS-DESTINATION 0)))
		  SG)
  (SG-REGPDL-PUSH 0 SG)
  (SG-REGPDL-PUSH 0 SG)
  (SG-REGPDL-PUSH FUNCTION SG)
  (SETF (SG-IPMARK SG) NEW-IPMARK))

;;;; Running things in the other stack group.

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
		  (SEND EH-ERROR ':PRINT-ERROR-MESSAGE SG T *STANDARD-OUTPUT*))
		(WARN-ABOUT-SPECIAL-VARIABLES SG)))
	 (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)))
  (COND ((EQ RESULT 'LOSE)
	 (*THROW 'QUIT NIL)))
  RESULT)

(DEFUN SG-RUN-GOODBYE (SG)
  "Restart stack group SG, expecting it not to return.
If done in a second-level error handler stack group,
the error handler stack group is marked as free."
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG)	;Terminate arg list
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (ASSURE-FREE-SPACE)
  (WITHOUT-INTERRUPTS
    (AND ERROR-HANDLER-RUNNING (FREE-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP))
    (STACK-GROUP-RESUME SG NIL)))

;;; Not initialized until used, so that we don't try to create it
;;; until the window system exists.
(DEFVAR COLD-LOAD-STREAM-SAVED-SCREEN
	:UNBOUND
  "Bit array used to save the screen while the debugger uses the cold load stream.")

(DEFVAR COLD-LOAD-STREAM-DEBUGGER-SG NIL
  "If non-NIL, this is the second level error handler stack group
which had to switch to the cold load stream.")

(DEFVAR SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD NIL
  "Saves value of TV:COLD-LOAD-STREAM-OWNS-KEYBOARD on entry to debugger, to restore on exit.")

(DEFUN FREE-SECOND-LEVEL-ERROR-HANDLER-SG (SG &AUX (DEFAULT-CONS-AREA ERROR-HANDLER-AREA))
  "Mark the second level error handler stack group SG as available for re-use."
  (WHEN (MEMQ SG FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
    (FERROR NIL "Freeing ~S, but it's already free." SG))
  (COND ((NEQ SG CURRENT-STACK-GROUP)
	 ;; Freeing the error handler, but not current stack group, so cause it to
	 ;; do a LEAVING-ERROR-HANDLER first
	 (SG-FUNCALL SG #'LEAVING-ERROR-HANDLER)))
  (WITHOUT-INTERRUPTS
    ;; If appropriate and user approves, restore the saved screen
    ;; which we clobbered by using the cold load stream.
    (WHEN (EQ SG COLD-LOAD-STREAM-DEBUGGER-SG)
      (RESTORE-SCREEN-FOR-COLD-LOAD-STREAM)
      (SETQ COLD-LOAD-STREAM-DEBUGGER-SG NIL))
    (PUSH SG FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
    (AND CURRENT-PROCESS (SEND CURRENT-PROCESS ':REVOKE-RUN-REASON SG))))

;;; Set *TERMINAL-IO* to the cold load stream and print STRING as an explanation.
;;; Saves the contents of the screen so it can be restored on exit from the debugger.
(DEFUN USE-COLD-LOAD-STREAM (STRING)
  (UNLESS (EQ *TERMINAL-IO* TV:COLD-LOAD-STREAM)
    (SETQ *TERMINAL-IO* TV:COLD-LOAD-STREAM)
    (SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
    (SETQ COLD-LOAD-STREAM-DEBUGGER-SG CURRENT-STACK-GROUP))
  (SETQ TV:COLD-LOAD-STREAM-OWNS-KEYBOARD T)
  (SEND *TERMINAL-IO* ':HOME-CURSOR)
  (SEND *TERMINAL-IO* ':CLEAR-EOL)
  (FORMAT *TERMINAL-IO* "--> ~A, using the cold load stream <--~2%" STRING))

(DEFUN SAVE-SCREEN-FOR-COLD-LOAD-STREAM (&OPTIONAL DONT-HOME-CURSOR)
  (UNLESS (VARIABLE-BOUNDP COLD-LOAD-STREAM-SAVED-SCREEN)
    (SETQ COLD-LOAD-STREAM-SAVED-SCREEN
	  (MAKE-PIXEL-ARRAY
	    (PIXEL-ARRAY-WIDTH (TV:MAIN-SCREEN-AND-WHO-LINE))
	    (PIXEL-ARRAY-HEIGHT (TV:MAIN-SCREEN-AND-WHO-LINE))
	    ':TYPE ART-1B)))
  (BITBLT TV:ALU-SETA
	  (PIXEL-ARRAY-WIDTH (TV:MAIN-SCREEN-AND-WHO-LINE))
	  (PIXEL-ARRAY-HEIGHT (TV:MAIN-SCREEN-AND-WHO-LINE))
	  (TV:MAIN-SCREEN-AND-WHO-LINE) 0 0
	  COLD-LOAD-STREAM-SAVED-SCREEN 0 0)
  (UNLESS DONT-HOME-CURSOR
    (SEND TV:COLD-LOAD-STREAM ':HOME-CURSOR)
    (SEND TV:COLD-LOAD-STREAM ':CLEAR-EOL)))


(DEFUN RESTORE-SCREEN-FOR-COLD-LOAD-STREAM (&OPTIONAL DONT-ASK)
  (WHEN (OR DONT-ASK
	    (LET ((*QUERY-IO* TV:COLD-LOAD-STREAM))
	      (Y-OR-N-P "Restore the screen? ")))
    (BITBLT TV:ALU-SETA
	    (PIXEL-ARRAY-WIDTH (TV:MAIN-SCREEN-AND-WHO-LINE))
	    (PIXEL-ARRAY-HEIGHT (TV:MAIN-SCREEN-AND-WHO-LINE))
	    COLD-LOAD-STREAM-SAVED-SCREEN 0 0
	    (TV:MAIN-SCREEN-AND-WHO-LINE) 0 0)))

;;; Unwind the stack group until the M-AP is DEST-AP.
;;; If GOODBYE-P is T, it returns the specified value from that frame,
;;; otherwise it comes back to the EH.
(DEFUN SG-UNWIND-TO-FRAME (SG DEST-FRAME GOODBYE-P &OPTIONAL VALUE (LABEL T) &AUX N)
  "Unwind the stack group SG until DEST-FRAME is the innermost frame left.
GOODBYE-P means return to that frame and let SG keep running
/(and free this error handler); then VALUE is the value to return to it.
LABEL is a catch tag; if it is specified, we throw VALUE to that tag in SG
and let it keep running; DEST-FRAME is irrelevant, and GOODBYE-P MUST be T."
  (IF (> INNERMOST-VISIBLE-FRAME (SG-AP SG))
      (SETF (SG-AP SG) INNERMOST-VISIBLE-FRAME))
  (SETQ N (DO ((FRAME (SG-AP SG) (SG-NEXT-ACTIVE SG FRAME))
	       (N 1 (1+ N)))
	      ((= FRAME DEST-FRAME) N)))
  (ASSURE-FREE-SPACE)
  (SG-UNWIND SG LABEL VALUE N (IF GOODBYE-P NIL CURRENT-STACK-GROUP)
	     (IF GOODBYE-P 'FREE 'CALL))
  (COND ((NULL GOODBYE-P)			;Flush the call back to this SG
	 (LET ((RP (SG-REGULAR-PDL SG)) (FRAME (SG-AP SG)))
	   (IF (NEQ (AREF RP FRAME) CURRENT-STACK-GROUP)
	       (FERROR NIL "Second-level EH stack-group not found on pdl where expected"))
	   (IF ( (SG-REGULAR-PDL-POINTER SG) (1+ FRAME))
	       (FERROR NIL "Second-level EH stack-group called with wrong number of args"))
	   (SETF (SG-IPMARK SG) (SG-NEXT-OPEN SG FRAME))
	   (SETF (SG-AP SG)
		 (SETQ CURRENT-FRAME (SG-NEXT-ACTIVE SG FRAME)))
	   (SETQ ERROR-LOCUS-FRAME (MIN ERROR-LOCUS-FRAME CURRENT-FRAME))
	   (SETF (SG-FLAGS-QBBFL SG)		; Must correspond to current frame to work!
		 (RP-BINDING-BLOCK-PUSHED RP CURRENT-FRAME))
	   (DOTIMES (I 5)
	     (SG-REGPDL-POP SG))))))

(DEFUN SG-UNWIND-TO-FRAME-AND-REINVOKE (SG FRAME
			&OPTIONAL (FORM (GET-FRAME-FUNCTION-AND-ARGS SG FRAME))
			&AUX RP PP LEXPR-CALL)
  "Unwind stack group SG until FRAME is the innermost frame, and prepare to reinvoke it.
If FORM is specified, it is what to execute instead of what FRAME
was executing previously.
Use STACK-GROUP-RESUME to make SG take off and run."
  (ASSURE-FREE-SPACE)
  ;; Unwind back to point where frame to be retried is about to return.
  ;; This gets rid of its unwind-protects but not its special bindings
  ;; and leaves any ADI associated with calling it on the stack too.
  (LET ((OTOC (SG-FLAGS-TRAP-ON-CALL SG)))
    (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
    (SG-UNWIND-TO-FRAME SG FRAME NIL)
    (SETF (SG-FLAGS-TRAP-ON-CALL SG) OTOC))
  ;; Next line prevents total disaster if error in the code below
  (SETQ INNERMOST-VISIBLE-FRAME (SG-AP SG))
  ;; Get rid of the saved microstack for that frame.  There will at least
  ;; be an entry for XUWR1+1.
  (SETQ RP (SG-REGULAR-PDL SG)
	PP (SG-REGULAR-PDL-POINTER SG))
  (AND (ZEROP (RP-MICRO-STACK-SAVED RP FRAME))
       (FERROR NIL "Where's my saved microstack?"))
  (LET ((BOT (SG-FRAME-SPECIAL-PDL-RANGE SG FRAME)))
    (DO ((SP (SG-SPECIAL-PDL SG))
	 (SPP (SG-SPECIAL-PDL-POINTER SG) (1- SPP))
	 (P))
	(NIL)
      (SETQ P (ALOC SP SPP))
      (OR (= (%P-DATA-TYPE P) DTP-FIX) (FERROR NIL "Where's my saved microstack?"))
      (AND (NOT (ZEROP (%P-LDB %%SPECPDL-BLOCK-START-FLAG P)))
	   (RETURN (SETF (SG-SPECIAL-PDL-POINTER SG) (1- SPP)))))
    ;; Now any special bindings made within this frame,
    ;; but keep any that were made by closures, flavor instances, etc.
    ;; on entry to this frame.
    (WHEN BOT
      (DO ((SP (SG-SPECIAL-PDL SG))
	   ;; SPP is the index of the first word of a binding pair.
	   ;; So is BOT.
	   (SPP (1- (SG-SPECIAL-PDL-POINTER SG)) (- SPP 2)))
	  ((OR (< SPP BOT)
	       (NOT (ZEROP (%P-LDB %%SPECPDL-CLOSURE-BINDING (ALOC SP SPP)))))
	   ;; SPP points to a pair that doesn't belong to this frame
	   ;; or else should not be flushed.
	   (SETF (SG-SPECIAL-PDL-POINTER SG)
		 (1+ SPP))
	   ;; If there was a binding block and we flushed the whole thing,
	   ;; clear the flag saying this frame has made a binding block.
	   (IF (< SPP BOT)
	       (SETF (SG-FLAGS-QBBFL SG) 0))))))
  (SETF (RP-MICRO-STACK-SAVED RP FRAME) 0)
  ;; Now rebuild the frame as if it was an open call block about to be called
  (SETF (SG-PDL-PHASE SG)		;PP gets M-AP minus one
	(LOGAND (- (SG-PDL-PHASE SG) (- PP (SETQ PP (1- FRAME))))
		(1- SI:PDL-BUFFER-LENGTH)))
  (SETF (SG-REGULAR-PDL-POINTER SG) PP)
  ;Put back the function.  Convert from a name to a function.
  (SG-REGPDL-PUSH
    (COND ((SI:VALIDATE-FUNCTION-SPEC (CAR FORM)) (FDEFINITION (CAR FORM)))
	  (T (CAR FORM)))
    SG)
  ;; Now push back the args.  Make sure we know how many to spread.
  (SETF (VALUES NIL NIL LEXPR-CALL)
	(SG-REST-ARG-VALUE SG FRAME))
  (DO ((ARGS (CDR FORM) (CDR ARGS))
       (COUNT (SG-NUMBER-OF-SPREAD-ARGS SG FRAME)
	      (1- COUNT)))
      (())
    (COND ((AND LEXPR-CALL (ZEROP COUNT))
	   (RETURN (SG-REGPDL-PUSH ARGS SG)))
	  ((NULL ARGS)
	   (RETURN NIL)))
    (SG-REGPDL-PUSH (CAR ARGS) SG))
  ;; Clean out any values already returned by this frame.
  (SG-FRAME-VALUE-LIST SG FRAME 0)
  ;; If there is a rest arg, set cdr coding so it looks like a tail of the list of args.
  ;; Otherwise, make the list of args end with NIL.
  (COND (LEXPR-CALL
	 (%P-STORE-CDR-CODE (ALOC RP (1- (SG-REGULAR-PDL-POINTER SG))) CDR-NORMAL)
	 (%P-STORE-CDR-CODE (ALOC RP (SG-REGULAR-PDL-POINTER SG)) CDR-ERROR))
	(T
	 (%P-STORE-CDR-CODE (ALOC RP (SG-REGULAR-PDL-POINTER SG)) CDR-NIL)))
  (SETF (SG-IPMARK SG) FRAME)
  (SETF (SG-AP SG) FRAME)
  ;; Now send the SG on its way
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN))

;;; The CONTINUATION is a function called with one argument in the newly-reset
;;; stack-group.  ARGUMENT is that argument.
;;; If PROCESS-P, rather than doing it now, in this process, we simply
;;; leave the stack-group in such a state that the next time it is called,
;;; e.g. by the scheduler, it will do it.
(DEFUN UNWIND-SG (SG CONTINUATION ARGUMENT PROCESS-P)
  (SETF (SG-INST-DISP SG) 0)  ;SG-MAIN-DISPATCH
  (LET ((ST (SG-CURRENT-STATE SG)))
    (COND ((NOT (OR (= ST SG-STATE-AWAITING-INITIAL-CALL)
		    (= ST 0)))
	   (SG-UNWIND SG T ARGUMENT NIL CONTINUATION (IF PROCESS-P 'SETUP 'CALL)))
	  (T	;SG has not been run, don't unwind, but do leave in same state
	   (STACK-GROUP-PRESET SG CONTINUATION ARGUMENT)
	   (OR PROCESS-P (STACK-GROUP-RESUME SG NIL))))
    (OR PROCESS-P (SETF (SG-CURRENT-STATE SG) SG-STATE-EXHAUSTED))))

(DEFUN SG-EVAL-IN-FRAME (SG FORM FRAME &OPTIONAL REBIND-STREAMS)
  "Evaluate FORM in stack group SG in the environment within frame FRAME.
In other respects, like SG-EVAL."
  (LET ((PREV-FRAME (SG-PREVIOUS-ACTIVE SG FRAME INNERMOST-VISIBLE-FRAME)))
    (IF (NULL PREV-FRAME)
	(SG-EVAL SG FORM REBIND-STREAMS)
      (LET* ((REGPDL-POINTER (SG-REGULAR-PDL-POINTER SG))
	     (SPECPDL-POINTER (SG-SPECIAL-PDL-POINTER SG))
	     (QBBFL (SG-FLAGS-QBBFL SG))
	     (AP (SG-AP SG))
	     (IPMARK (SG-IPMARK SG))
	     (REGPDL-SEGMENT (ALLOCATE-SAVED-PDL-SEGMENT
			       (- (1+ REGPDL-POINTER) PREV-FRAME)))
	     (PREV-FRAME-SPECPDL-INDEX
	       (1+ (SG-FRAME-SPECIAL-PDL-INDEX SG PREV-FRAME)))
	     (SPECPDL-SEGMENT (ALLOCATE-SAVED-PDL-SEGMENT
				(- (1+ SPECPDL-POINTER)
				   PREV-FRAME-SPECPDL-INDEX))))
	;; Set up SG so that FRAME is the innermost frame left.
	(WITHOUT-INTERRUPTS
	  ;; Copy out the data of the frames beyond that one.
	  (%BLT-TYPED (ALOC (SG-REGULAR-PDL SG) PREV-FRAME)
		      (ALOC REGPDL-SEGMENT 0)
		      (- (1+ REGPDL-POINTER) PREV-FRAME)
		      1)
	  (%BLT-TYPED (ALOC (SG-SPECIAL-PDL SG) PREV-FRAME-SPECPDL-INDEX)
		      (ALOC SPECPDL-SEGMENT 0)
		      (- (1+ SPECPDL-POINTER) PREV-FRAME-SPECPDL-INDEX)
		      1)
	  ;; Reset M-FLAGS QBBFL to its state in that frame.
	  (SETF (SG-FLAGS-QBBFL SG) (RP-BINDING-BLOCK-PUSHED (SG-REGULAR-PDL SG) FRAME))
	  ;; Mark the inner frames as not there.
	  (SETF (SG-AP SG) FRAME)
	  (SETF (SG-IPMARK SG) FRAME)
	  (SETF (SG-REGULAR-PDL-POINTER SG) (1- PREV-FRAME))
	  (SETF (SG-SPECIAL-PDL-POINTER SG) (1- PREV-FRAME-SPECPDL-INDEX)))
	(UNWIND-PROTECT
	    ;; Do the evaluation.
	    (SG-EVAL SG FORM REBIND-STREAMS REGPDL-SEGMENT SPECPDL-SEGMENT)
	  ;; Copy back the frames we removed.
	  (WITHOUT-INTERRUPTS
	    (%BLT-TYPED (ALOC REGPDL-SEGMENT 0)
			(ALOC (SG-REGULAR-PDL SG) PREV-FRAME)
			(- (1+ REGPDL-POINTER) PREV-FRAME)
			1)
	    (%BLT-TYPED (ALOC SPECPDL-SEGMENT 0)
			(ALOC (SG-SPECIAL-PDL SG) PREV-FRAME-SPECPDL-INDEX)
			(- (1+ SPECPDL-POINTER) PREV-FRAME-SPECPDL-INDEX)
			1)
	    (SETF (SG-AP SG) AP)
	    (SETF (SG-IPMARK SG) IPMARK)
	    (SETF (SG-FLAGS-QBBFL SG) QBBFL)
	    (SETF (SG-REGULAR-PDL-POINTER SG) REGPDL-POINTER)
	    (SETF (SG-SPECIAL-PDL-POINTER SG) SPECPDL-POINTER)
	    ;; Return the save arrays for re-use.
	    (FREE-SAVED-PDL-SEGMENT REGPDL-SEGMENT)
	    (FREE-SAVED-PDL-SEGMENT SPECPDL-SEGMENT)))))))

(DEFUN SG-EVAL (SG FORM &OPTIONAL REBIND-STREAMS
		SAVED-REGPDL-SEGMENT SAVED-SPECPDL-SEGMENT
		&AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
  "Evaluate FORM in stack group SG and return a list of its values.
This is a high-level function, in that SG's state is preserved.
REBIND-STREAMS = T means execute FORM with streams such as
*STANDARD-INPUT* and *STANDARD-OUTPUT* set to the current value
of *TERMINAL-IO* in the stack group that calls SG-EVAL
instead of whatever they are in stack group SG.
SAVED-REGPDL-SEGMENT and SAVED-SPECPDL-SEGMENT are only for use by SG-EVAL-IN-FRAME."
  (SG-SAVE-STATE SG)
  (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
  (SG-OPEN-CALL-BLOCK SG 0 (IF REBIND-STREAMS 'FH-STREAM-BINDING-EVALER 'FH-EVALER))
  ;; NOTE: SG-FRAME-SINGLE-VALUE knows the position of EH-ERROR among the following
  (SG-REGPDL-PUSH FORM SG)
  (SG-REGPDL-PUSH + SG)
  (SG-REGPDL-PUSH * SG)
  (SG-REGPDL-PUSH // SG)
  (SG-REGPDL-PUSH ++ SG)
  (SG-REGPDL-PUSH ** SG)
  (SG-REGPDL-PUSH //// SG)
  (SG-REGPDL-PUSH +++ SG)
  (SG-REGPDL-PUSH *** SG) 
  (SG-REGPDL-PUSH ////// SG)
  (SG-REGPDL-PUSH *VALUES* SG)
  (SG-REGPDL-PUSH SG SG)
  (SG-REGPDL-PUSH CURRENT-FRAME SG)
  (SG-REGPDL-PUSH EH-ERROR SG)
  (SG-REGPDL-PUSH CURRENT-STACK-GROUP SG)
  (SG-REGPDL-PUSH ERROR-HANDLER-RUNNING SG)
  (SG-REGPDL-PUSH PREV-FH SG)
  (SG-REGPDL-PUSH SAVED-REGPDL-SEGMENT SG)
  (SG-REGPDL-PUSH SAVED-SPECPDL-SEGMENT SG)
  (SG-REGPDL-PUSH ERROR-DEPTH SG)
  (AND REBIND-STREAMS (SG-REGPDL-PUSH *TERMINAL-IO* SG))
  (RUN-SG SG))

(DEFUN SG-FUNCALL (SG FUNCTION &REST ARGUMENTS)
  "Apply FUNCTION to ARGUMENTS in stack group SG.
This is a high-level function, in that SG's state is preserved."
  (SG-APPLY SG FUNCTION ARGUMENTS))

(DEFUN SG-FUNCALL-NO-RESTART (SG FUNCTION &REST ARGUMENTS)
  "Apply FUNCTION to ARGUMENTS in stack group SG.
Does not do a CATCH-ERROR-RESTART around the application.
This is a high-level function, in that SG's state is preserved."
  (SG-APPLY SG FUNCTION ARGUMENTS T))

(DEFUN SG-APPLY (SG FUNCTION ARGUMENTS &OPTIONAL NO-ERROR-RESTART GOODBYE
		 &AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
  "Apply FUNCTION to ARGUMENTS in stack group SG.
This is a high-level function, in that SG's state is preserved."
  (SG-SAVE-STATE SG)
  (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
  (SG-OPEN-CALL-BLOCK SG 0
		      (IF NO-ERROR-RESTART 'FH-APPLIER-NO-RESTART 'FH-APPLIER))
  (SG-REGPDL-PUSH FUNCTION SG)
  (SG-REGPDL-PUSH ARGUMENTS SG)
  (SG-REGPDL-PUSH + SG)
  (SG-REGPDL-PUSH * SG)
  (SG-REGPDL-PUSH // SG)
  (SG-REGPDL-PUSH ++ SG)
  (SG-REGPDL-PUSH ** SG)
  (SG-REGPDL-PUSH //// SG)
  (SG-REGPDL-PUSH +++ SG)
  (SG-REGPDL-PUSH *** SG) 
  (SG-REGPDL-PUSH ////// SG)
  (SG-REGPDL-PUSH *VALUES* SG)
  (SG-REGPDL-PUSH SG SG)
  (SG-REGPDL-PUSH CURRENT-FRAME SG)
  (SG-REGPDL-PUSH EH-ERROR SG)
  (SG-REGPDL-PUSH CURRENT-STACK-GROUP SG)
  (SG-REGPDL-PUSH ERROR-HANDLER-RUNNING SG)
  (SG-REGPDL-PUSH PREV-FH SG)
  (IF GOODBYE
      (SG-RUN-GOODBYE SG)
    (RUN-SG SG)))

(DEFUN SG-THROW (SG LABEL VALUE &OPTIONAL IGNORE)
  "Resume SG by throwing VALUE to LABEL in it.  Frees the error handler."
  (LET ((PDL-GROW-RATIO 1))
    ;; Don't make the stack much bigger; we just want room to pop it.
    (SG-MAYBE-GROW-PDLS SG ALLOW-PDL-GROW-MESSAGE #o2000 0 T)
    (SG-SAVE-STATE SG T))
  (SG-OPEN-CALL-BLOCK SG 0 'FH-THROWER)
  (SG-REGPDL-PUSH LABEL SG)
  (SG-REGPDL-PUSH VALUE SG)
  (SG-REGPDL-PUSH (SG-FLAGS-TRAP-ON-CALL SG) SG)
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
  (SG-RUN-GOODBYE SG))

(DEFUN SG-UNWIND (SG LABEL VALUE COUNT ACTION DISPOSAL)
  "Unwind, or prepare to unwind, stack group SG.
LABEL, VALUE, COUNT and ACTION are like the args to *UNWIND-STACK.
DISPOSAL is SETUP just to set up the call, CALL to make the call and not free the EH,
 FREE to make the call and free the EH"
  (SG-SAVE-STATE SG)
  (AND COUNT (SETQ COUNT (1+ COUNT)))  ;Make up for the frame pushed by SG-SAVE-STATE.
  (SG-OPEN-CALL-BLOCK SG 0 'FH-UNWINDER)
  (SG-REGPDL-PUSH LABEL SG)
  (SG-REGPDL-PUSH VALUE SG)
  (SG-REGPDL-PUSH COUNT SG)
  (SG-REGPDL-PUSH ACTION SG)
  (SG-REGPDL-PUSH (SG-FLAGS-TRAP-ON-CALL SG) SG)
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG)	;Terminate arg list
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (WITHOUT-INTERRUPTS
    (AND ERROR-HANDLER-RUNNING (EQ DISPOSAL 'FREE)
	 (FREE-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP))
    (OR (EQ DISPOSAL 'SETUP) (STACK-GROUP-RESUME SG NIL))))

(DEFUN SG-EVAL-NO-TRAP (SG FORM &OPTIONAL REBIND-STREAMS)
  "Like SG-EVAL but suppress trap-on-call."
  (LET ((OTOC (SG-FLAGS-TRAP-ON-CALL SG)))
    (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
    (UNWIND-PROTECT
      (SG-EVAL SG FORM REBIND-STREAMS)
      (SETF (SG-FLAGS-TRAP-ON-CALL SG) OTOC))))

(DEFUN SG-FUNCALL-NO-TRAP (SG FUNCTION &REST ARGUMENTS)
  "Apply FUNCTION to ARGUMENTS in stack group SG, suppressing trap-on-call.
This is a high-level function, in that SG's state is preserved."
  (LET ((OTOC (SG-FLAGS-TRAP-ON-CALL SG)))
    (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
    (UNWIND-PROTECT
      (SG-APPLY SG FUNCTION ARGUMENTS)
      (SETF (SG-FLAGS-TRAP-ON-CALL SG) OTOC))))

(DEFUN SG-APPLY-NO-TRAP (SG FUNCTION ARGUMENTS &OPTIONAL NO-ERROR-RESTART GOODBYE)
  "Apply FUNCTION to ARGUMENTS in stack group SG, suppressing trap-on-call.
This is a high-level function, in that SG's state is preserved."
  (LET ((OTOC (SG-FLAGS-TRAP-ON-CALL SG)))
    (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
    (UNWIND-PROTECT
	(SG-APPLY SG FUNCTION ARGUMENTS NO-ERROR-RESTART GOODBYE)
      (SETF (SG-FLAGS-TRAP-ON-CALL SG) OTOC))))

;;; The FH- functions are those intended to run in the other stack group.
;;; Those that come back should be started up with RUN-SG.
;;; They must be given the error handler stack group as an argument
;;; so that they can call it back.  This they must do without making any other
;;; intervening active call blocks on the stack, so that the foothold data
;;; can be found from the SG-AP when it returns. They must also be given ERROR-HANDLER-RUNNING
;;; as an argument, so that if it is T they can do an unwind protect that does
;;; FREE-SECOND-LEVEL-ERROR-HANDLER-SG on the stack group that they aren't going to return
;;; to in that case.  They must also be given the previous foothold's offset so that
;;; SG-FOOTHOLD-DATA can be reset in case of a throw.

;;; Those that do not come back should be started up with SG-RUN-GOODBYE.

(DEFUN FH-APPLIER (FN ARGS X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
		   X-SG X-FRAME X-ERROR SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH)))
      (*CATCH TAG
	;; Note: no special variables should be bound
	;; outside this point (when we return to the error handler sg).
	(STACK-GROUP-RESUME SG
	  (LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
		(// X//) (//// X////) (////// X//////)
		(*VALUES* XVALUES)
		(EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
		CONDITION-HANDLERS
		CONDITION-DEFAULT-HANDLERS
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

(DEFPROP FH-APPLIER-NO-RESTART T :ERROR-REPORTER)
(DEFPROP FOOTHOLD T :ERROR-REPORTER)

(DEFUN FH-APPLIER-NO-RESTART (FN ARGS IGNORE IGNORE IGNORE IGNORE IGNORE IGNORE IGNORE
			      IGNORE IGNORE IGNORE IGNORE IGNORE IGNORE
			      SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (STACK-GROUP-RESUME SG (MULTIPLE-VALUE-LIST (APPLY FN ARGS)))
    ;; This is reached only if we throw through this frame.
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

(DEFUN FH-EVALER (FORM X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
		  X-SG X-FRAME X-ERROR SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH)))
      (*CATCH TAG
	(STACK-GROUP-RESUME
	  SG
	  ;; Note: no special variables should be bound
	  ;; outside this point (when we return to the error handler sg).
	  (LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
		(// X//) (//// X////) (////// X//////) (- FORM)
		(*VALUES* XVALUES)
		(EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
		CONDITION-HANDLERS
		CONDITION-DEFAULT-HANDLERS
		;; It's best to heap-cons this to avoid frightening the user.
		(CONDITION-RESUME-HANDLERS
		  (LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			   SI:CATCH-ERROR-RESTART-THROW ,TAG)
			 T CONDITION-RESUME-HANDLERS))
		(*EVALHOOK* NIL) (*APPLYHOOK* NIL) (ERRSET-STATUS NIL))
	    (WITH-SELF-VARIABLES-BOUND
	      (MULTIPLE-VALUE-LIST (EVAL FORM))))))
      ;; This is in case the catch catches.
      (STACK-GROUP-RESUME SG 'LOSE))
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

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
      (*CATCH TAG
	;; Note: no special variables should be bound
	;; outside this point (when we return to the error handler sg).
	(LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
	      (// X//) (//// X////) (////// X//////) (- FORM)
	      (*VALUES* XVALUES)
	      (EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
	      (*TERMINAL-IO* EH-TERMINAL-IO)
	      (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
	      (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
	      (*QUERY-IO* SI:SYN-TERMINAL-IO)
	      (*ERROR-OUTPUT* SI:SYN-TERMINAL-IO)
	      (*EVALHOOK* NIL) (*APPLYHOOK* NIL) (ERRSET-STATUS NIL)
	      (RUBOUT-HANDLER NIL)
	      CONDITION-HANDLERS
	      CONDITION-DEFAULT-HANDLERS
	      ;; It's best to heap-cons this to avoid frightening the user.
	      (CONDITION-RESUME-HANDLERS
		(LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			 SI:CATCH-ERROR-RESTART-THROW ,TAG)
		       T CONDITION-RESUME-HANDLERS)))
	  (WITH-SELF-VARIABLES-BOUND
	    (SETQ RESULT (MULTIPLE-VALUE-LIST (SI:EVAL-ABORT-TRIVIAL-ERRORS FORM))
		  WIN-P T))))
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

(DEFVAR FH-AUX-SG (MAKE-STACK-GROUP "Foothold aux SG"))

(DEFUN SET-TRAP-ON-CALL (SG)
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 1)
  (STACK-GROUP-RETURN NIL))

(DEFUN FH-THROWER (LABEL VALUE TRAP-ON-CALL-FLAG)
  (UNLESS (ZEROP TRAP-ON-CALL-FLAG)
    (WITHOUT-INTERRUPTS
      (STACK-GROUP-PRESET FH-AUX-SG 'SET-TRAP-ON-CALL CURRENT-STACK-GROUP)
      (FUNCALL FH-AUX-SG NIL)))
  (*THROW LABEL VALUE))

(DEFUN FH-UNWINDER (LABEL VALUE COUNT ACTION TRAP-ON-CALL-FLAG)
  (UNLESS (ZEROP TRAP-ON-CALL-FLAG)
    (WITHOUT-INTERRUPTS
      (STACK-GROUP-PRESET FH-AUX-SG 'SET-TRAP-ON-CALL CURRENT-STACK-GROUP)
      (FUNCALL FH-AUX-SG NIL)))
  (*UNWIND-STACK LABEL VALUE COUNT ACTION))

;;;; Various utility ANALYSIS functions.

;;; Note that "next" means "earlier frame", as in the command C-N.
;;; "Previous" means "later frame", as in the command C-P.

(DEFUN SG-INNERMOST-ACTIVE (SG)
  "Returns the currently executing frame in SG."
  (IF (EQ SG CURRENT-STACK-GROUP)
      (SG-NEXT-ACTIVE CURRENT-STACK-GROUP
		      (%POINTER-DIFFERENCE (%STACK-FRAME-POINTER)
					   (ALOC (SG-REGULAR-PDL CURRENT-STACK-GROUP) 0)))
    (SG-AP SG)))

(DEFUN SG-INNERMOST-OPEN (SG)
  "Returns the innermost open frame in SG."
  (IF (EQ SG CURRENT-STACK-GROUP)
      (SG-NEXT-OPEN CURRENT-STACK-GROUP
		    (%POINTER-DIFFERENCE (%STACK-FRAME-POINTER)
					 (ALOC (SG-REGULAR-PDL CURRENT-STACK-GROUP) 0)))
    (SG-IPMARK SG)))

(DEFUN SG-NEXT-OPEN (SG FRAME)
  "Return the frame index of next open frame out from FRAME, in SG.
Returns NIL if FRAME is the outermost frame in SG.
Open frames include those which are still accumulating args
and whose functions have not been called yet."
  (LET ((DELTA (RP-DELTA-TO-OPEN-BLOCK (SG-REGULAR-PDL SG) FRAME)))
    (IF (ZEROP DELTA) NIL (- FRAME DELTA))))

(DEFUN SG-NEXT-ACTIVE (SG FRAME)
  "Return the frame index of next active frame out from FRAME, in SG.
Returns NIL if FRAME is the outermost frame in SG."
  (LET ((DELTA (RP-DELTA-TO-ACTIVE-BLOCK (SG-REGULAR-PDL SG) FRAME)))
    (IF (ZEROP DELTA) NIL (- FRAME DELTA))))

(DEFUN SG-PREVIOUS-OPEN (SG FRAME)
  "Return the frame index of next active frame in from FRAME, in SG.
Returns NIL if FRAME is the innermost frame in SG.
If FRAME is NIL, returns the index of the outermost frame.
Open frames include those which are still accumulating args
and whose functions have not been called yet."
  (DO ((THIS-FRAME (SG-IPMARK SG) (SG-NEXT-OPEN SG THIS-FRAME))
       (PREVIOUS-FRAME NIL THIS-FRAME))
      ((COND (FRAME ( THIS-FRAME FRAME))
	     (T (NULL THIS-FRAME)))
       PREVIOUS-FRAME)))

(DEFUN SG-PREVIOUS-ACTIVE (SG FRAME &OPTIONAL (INNERMOST (SG-AP SG)))
  "Return the frame index of next active frame INWARD from FRAME, in SG.
Returns NIL if FRAME is the innermost frame in SG.
If FRAME is NIL, returns the index of the outermost frame.
If INNERMOST is specified, it should be a frame in SG;
 we pretend that it is the innermost frame."
  (DO ((THIS-FRAME INNERMOST (SG-NEXT-ACTIVE SG THIS-FRAME))
       (PREVIOUS-FRAME NIL THIS-FRAME))
      ((COND (FRAME ( THIS-FRAME FRAME))
	     (T (NULL THIS-FRAME)))
       PREVIOUS-FRAME)))

(DEFUN SG-FRAME-ACTIVE-P (SG FRAME)
  "T if FRAME in SG is active (its function has been invoked)."
  (OR (NOT (ZEROP (RP-ENTRY-WORD (SG-REGULAR-PDL SG) FRAME)))
      (DO ((THIS-FRAME (SG-AP SG) (SG-NEXT-ACTIVE SG THIS-FRAME)))
	  (( THIS-FRAME FRAME)
	   (= THIS-FRAME FRAME)))))

(DEFUN SG-PREVIOUS-NTH-OPEN (SG FRAME &OPTIONAL (COUNT 1))
  "Scans up or down from FRAME, considering all open frames.
Open frames include those which are still accumulating args
and whose functions have not been called yet.
If the top or bottom of the stack is reached, then the
innermost or outermost frame is returned, never NIL;
but the second value is T if the specified number of frames
were actually passed.
COUNT is positive to go inward (more recently created frames)."
  (DECLARE (VALUES FRAME COUNT-USED-UP-FLAG))
  (COND ((= COUNT 0) FRAME)
	((MINUSP COUNT)
	 (DO ((P FRAME (SG-NEXT-OPEN SG P))
	      (I 0 (1- I))
	      (PP NIL P))
	     (())
	   (AND (OR (NULL P) (= I COUNT))
		(RETURN (OR P PP) P))))
	(T (DO ((P FRAME (SG-PREVIOUS-OPEN SG P))
		(I 0 (1+ I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))))

(DEFUN SG-PREVIOUS-NTH-ACTIVE (SG FRAME &OPTIONAL (COUNT 1) (INNERMOST (SG-AP SG)))
  "Scans up or down from FRAME, considering only active frames.
If the top or bottom of the stack is reached, then the
innermost or outermost frame is returned, never NIL;
but the second value is T if the specified number of frames
were actually passed.
COUNT is positive to go inward (more recently created frames).
If INNERMOST is specified, it should be a frame in SG;
 we pretend that it is the innermost frame by ignoring frames inside it."
  (COND ((= COUNT 0) FRAME)
	((MINUSP COUNT)
	 (DO ((P FRAME (SG-NEXT-ACTIVE SG P))
	      (I 0 (1- I))
	      (PP NIL P))
	     (())
	   (AND (OR (NULL P) (= I COUNT))
		(RETURN (OR P PP) P))))
	(T (DO ((P FRAME (SG-PREVIOUS-ACTIVE SG P INNERMOST))
		(I 0 (1+ I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))))

(DEFUN SG-PREVIOUS-NTH-INTERESTING-ACTIVE (SG FRAME &OPTIONAL (COUNT 1) (INNERMOST (SG-AP SG)))
  "Like SG-PREVIOUS-NTH-ACTIVE but ignores certain frames.
The ignored frames do not figure in decrementing COUNT either.
The frames we ignore are those that are part of the internals
of the interpreter, and special forms such as PROG and LET.
UNINTERESTING-FUNCTIONS is a list of functions to be ignored.
If INNERMOST is specified, it should be a frame in SG;
 we pretend that it is the innermost frame by ignoring frames inside it."
  (DECLARE (VALUES FRAME COUNT-USED-UP-FLAG))
  (COND ((= COUNT 0) FRAME)
	((MINUSP COUNT)
	 (DO ((P FRAME (SG-NEXT-INTERESTING-ACTIVE SG P))
	      (I 0 (1- I))
	      (PP NIL P))
	     (())
	   (AND (OR (NULL P) (= I COUNT))
		(RETURN (OR P PP) P))))
	(T (DO ((P FRAME (SG-PREVIOUS-INTERESTING-ACTIVE SG P INNERMOST))
		(I 0 (1+ I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))))

(DEFUN SG-PREVIOUS-INTERESTING-ACTIVE (SG FRAME &OPTIONAL (INNERMOST (SG-AP SG))
				       &AUX (RP (SG-REGULAR-PDL SG)))
  "Like SG-PREVIOUS-ACTIVE but ignores certain frames.
The frames we ignore are those that are part of the internals
of the interpreter, and special forms such as PROG and LET.
UNINTERESTING-FUNCTIONS is a list of functions to be ignored.
If INNERMOST is specified, it should be a frame in SG;
 we pretend that it is the innermost frame by ignoring frames inside it."
  (COND ((ATOM (RP-FUNCTION-WORD RP FRAME))
	 (SG-PREVIOUS-ACTIVE SG FRAME))
	(T (DO ((NEW-FRAME (SG-PREVIOUS-ACTIVE SG FRAME INNERMOST)
			   (SG-PREVIOUS-ACTIVE SG NEW-FRAME INNERMOST)))
	       ((OR (NULL NEW-FRAME)
		    (NOT (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP NEW-FRAME))
			       UNINTERESTING-FUNCTIONS)))
		NEW-FRAME)
	     ;; Make provisions for showing uninteresting fns
	     ;; when we are stepping thru them.
	     (AND (= NEW-FRAME INNERMOST-VISIBLE-FRAME)
		  INNERMOST-FRAME-IS-INTERESTING
		  (RETURN (SG-PREVIOUS-ACTIVE SG FRAME INNERMOST-VISIBLE-FRAME)))))))

(DEFUN SG-NEXT-INTERESTING-ACTIVE (SG FRAME)
  "Like SG-NEXT-ACTIVE but ignores certain frames.
The frames we ignore are those that are part of the internals
of the interpreter, and special forms such as PROG and LET.
UNINTERESTING-FUNCTIONS is a list of functions to be ignored."
  (COND ((MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
	       UNINTERESTING-FUNCTIONS)
	 (SG-NEXT-ACTIVE SG FRAME))
	(T (SG-OUT-TO-INTERESTING-ACTIVE SG (SG-NEXT-ACTIVE SG FRAME)))))

(DEFUN SG-OUT-TO-INTERESTING-ACTIVE (SG FRAME &AUX (RP (SG-REGULAR-PDL SG)))
  "If frame FRAME in SG is not /"interesting/", find a frame outside it that is.
The value is either FRAME or the index of a frame outside of that one.
Uninteresting frames are those that are part of the internals
of the interpreter, and special forms such as PROG and LET.
UNINTERESTING-FUNCTIONS is a list of functions that are uninteresting."
  (COND ((NULL FRAME) NIL)
	((AND (= FRAME INNERMOST-VISIBLE-FRAME)
	      INNERMOST-FRAME-IS-INTERESTING)
	 FRAME)
	((NOT (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP FRAME)) UNINTERESTING-FUNCTIONS))
	 FRAME)
	(T (DO ((NEW-FRAME FRAME (SG-NEXT-ACTIVE SG NEW-FRAME)))
	       ((OR (NULL NEW-FRAME)
		    (NOT (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP NEW-FRAME))
			       UNINTERESTING-FUNCTIONS)))
		(COND ((NULL NEW-FRAME) FRAME)
		      ((ATOM (RP-FUNCTION-WORD RP NEW-FRAME)) FRAME)
		      (T NEW-FRAME)))))))

(defun sg-frame-for-pdl-index (sg index)
  (do ((frame (sg-innermost-open sg) (sg-next-open sg frame)))
      ((null frame) nil)
    (when ( frame index)
      (return frame))))

(defun virtual-address-to-pdl-index (sg vadr)
  (let* ((rp (sg-regular-pdl sg))
	 (offset (si::array-data-offset rp)))
    (if (or (< (%pointer vadr)
	       (%pointer-plus rp offset))
	    ( (%pointer vadr)
	       (%pointer-plus rp (+ (sg-regular-pdl-pointer sg) offset))))
	nil
      (%pointer-difference vadr (%pointer-plus rp offset)))))

(DEFUN SG-ERRING-FUNCTION (SG &OPTIONAL (FRAME (SG-AP SG)))
  "Return a /"function name/" that represents the macro instruction that erred, in SG."
  (LET ((CURRENT-UPC (SG-TRAP-MICRO-PC SG))
	(RP (SG-REGULAR-PDL SG)))
    (IF (AND ( BEGIN-QARYR CURRENT-UPC) (< CURRENT-UPC END-QARYR))
	;; Not in a function at all.  Return the array it is in.
	(RP-FUNCTION-WORD RP (SG-IPMARK SG))
      ;; Normal case.  If in a compiled function, see if it "called" an open-coded fcn.
      (LET ((FUNCTION (RP-FUNCTION-WORD RP FRAME))
	    (PC (1- (RP-EXIT-PC RP FRAME))))
	(SELECT (%DATA-TYPE FUNCTION)
	  (DTP-U-ENTRY
	   (MICRO-CODE-ENTRY-NAME-AREA (%POINTER FUNCTION)))
	  (DTP-FEF-POINTER 
	   (LET ((INST (FEF-INSTRUCTION FUNCTION PC)))
	     (LET ((OP (LDB 1105 INST))
		   (SUBOP (LDB #o1503 INST))
		   (DISP (LDB #o0011 INST)))
	       (COND ((< OP #o11)
		      (NTH OP '(FUNCALL FUNCALL MOVE-INSTRUCTION CAR
					CDR CADR CDDR CDAR CAAR)))
		     ((OR (= OP #o11) (= OP #o31))
		      (NTH SUBOP '(ND1-UNUSED + - * //
					      LOGAND LOGXOR LOGIOR)))
		     ((OR (= OP #o12) (= OP #o32))
		      (NTH SUBOP '(= > < EQ CDR CDDR 1+ 1-)))
		     ((OR (= OP #o13) (= OP #o33))
		      (NTH SUBOP '(ND3-UNUSED BIND BIND SET-NIL
					      SET-ZERO PUSH-E MOVEM POP)))
		     ((OR (= OP #o14) (= OP #o34))
		      'A-BRANCH-INSTRUCTION)
		     ((= OP #o20)
		      (COND ((< DISP #o0200) 'AR-1)
			    ((< DISP #o0400) 'ARRAY-LEADER)
			    ((< DISP #o0600) '%INSTANCE-REF)
			    ((< DISP #o1200) 'AS-1)
			    ((< DISP #o1400) 'STORE-ARRAY-LEADER)
			    ((< DISP #o1600) '%INSTANCE-SET)))
		     ((= OP #o35)
		      (MICRO-CODE-SYMBOL-NAME-AREA (+ DISP 600)))
		     (( OP #o15) NIL)
		     ((< DISP #o020) 'AR-1)
		     ((< DISP #o040) 'ARRAY-LEADER)
		     ((< DISP #o060) '%INSTANCE-REF)
		     ((< DISP #o120) 'AS-1)
		     ((< DISP #o140) 'STORE-ARRAY-LEADER)
		     ((< DISP #o160) '%INSTANCE-SET)
		     ((< DISP #o220) 'UNBIND)
		     ((< DISP #o240) 'A-POP-PDL-INSTRUCTION)
		     (T (MICRO-CODE-SYMBOL-NAME-AREA (- DISP #o200)))))))
	  (OTHERWISE FUNCTION))))))

(DEFUN VALUE-NAME (FUNCTION VALUENUM)
  "Returns the name of FUNCTION's VALUENUM'th value, or NIL."
  (NTH VALUENUM
       (MULTIPLE-VALUE-BIND (NIL TEM)
	   (ARGLIST FUNCTION)
	 TEM)))

(DEFUN LOCAL-NAME (FUNCTION LOCALNO &AUX ARGL)
  "Return the name of local variable number LOCALNO in function FUNCTION.
If LOCALNO = 0, this will get the name of the rest arg (if any)
in any function that accepts a rest arg.
Returns NIL if the function doesn't have such a local."
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (COMPILER:DISASSEMBLE-LOCAL-NAME FUNCTION LOCALNO))
	((AND (ZEROP LOCALNO)
	      (CONSP (SETQ ARGL (COND ((LEGITIMATE-FUNCTION-P FUNCTION)
				       (ARGLIST FUNCTION T))))))
	 (MEM #'(LAMBDA (LIST ELT) (NOT (MEMQ ELT LIST)))
	      LAMBDA-LIST-KEYWORDS
	      (MEMQ '&REST ARGL)))))

(DEFUN REST-ARG-NAME (FUNCTION)
  "Return the name of FUNCTION's rest argument; NIL if it has none or if name is not known."
  (LOCAL-NAME FUNCTION 0))

(DEFUN ARG-NAME (FUNCTION ARGNO &AUX ARGL)
  "Return the name of argument number ARGNO in function FUNCTION.
The first argument is ARGNO = 0.
Returns NIL if the function doesn't have such an argument, or name is unknown.
Rest arguments do not count; use (EH:REST-ARG-NAME function 0)."
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (COMPILER:DISASSEMBLE-ARG-NAME FUNCTION ARGNO))
	((CONSP (SETQ ARGL (COND ((LEGITIMATE-FUNCTION-P FUNCTION)
				  (ARGLIST FUNCTION T)))))
	 (DO ((ARGL ARGL (CDR ARGL))
	      (I ARGNO))
	     ((OR (NULL ARGL)
		  (EQ (CAR ARGL) '&AUX)
		  (EQ (CAR ARGL) '&REST)
		  (EQ (CAR ARGL) '&KEY)))
	   (OR (MEMQ (CAR ARGL) LAMBDA-LIST-KEYWORDS)
	       (COND (( I 0)
		      (RETURN (CAR ARGL)))
		     (T (DECF I))))))))

;;;; Functions to extract the argument and local variable values from a frame.

(DEFUN GET-FRAME-FUNCTION-AND-ARGS (SG FRAME &AUX FUNCTION NARGS-SPREAD
				    (RP (SG-REGULAR-PDL SG))
				    LEXPR-CALL REST-ARG-EXPECTED REST-ARG-VALUE ANS)
  "Return a list describing what was being executed in frame FRAME in SG.
The car of the list is a function name.
The cdr is a list of arguments (but if the arguments have
been modified, we can only get the latest values)."
  (SETQ FUNCTION (RP-FUNCTION-WORD RP FRAME)
	NARGS-SPREAD (MIN (RP-NUMBER-ARGS-SUPPLIED RP FRAME)
			  (SG-NUMBER-OF-SPREAD-ARGS SG FRAME)))
  ;; Note on MIN above:
  ;; If there is an explicit rest arg, then there cannot be too few spread args,
  ;; so the MIN makes no difference.  If there is no explicit rest arg,
  ;; then RP-NUMBER-ARGS-SUPPLIED returns the correct number of spread args supplied
  ;; and it is necessary because SG-NUMBER-OF-SPREAD-ARGS includes
  ;; all the optionals whether supplied or not.
  (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-EXPECTED LEXPR-CALL)
    (SG-REST-ARG-VALUE SG FRAME))
  ;; Analyze the function
  (SETQ FUNCTION (FUNCTION-NAME FUNCTION))
  (OR (SG-FRAME-ACTIVE-P SG FRAME) (BARF "This is not an active frame."))
  ;; Get the spread args.
  (DO ((I NARGS-SPREAD (1- I)))				;Cons them up in reverse order
      ((ZEROP I))
    (SETQ ANS (CONS (AREF RP (+ FRAME I)) ANS)))	;+1 -1
  ;; NCONC the rest arg if any was supplied separately from the regular args
  (AND REST-ARG-EXPECTED
       (SETQ ANS (NCONC ANS (COPY-LIST REST-ARG-VALUE))))
  (CONS FUNCTION ANS))

;Brand S compatibility names.
(DEFF ARG 'EH-ARG)
(DEFF LOC 'EH-LOC)
(DEFF VAL 'EH-VAL)
(DEFF FUN 'EH-FUN)

(DEFUN EH-ARG (&OPTIONAL (NAME-OR-NUMBER 0) (ERRORP T))
  "Return the value of specified arg in current frame.
Specify either a number (origin 0) or a symbol (package does not matter)."
  (IF NAME-OR-NUMBER
      (MULTIPLE-VALUE-BIND (VAL1 NIL BARF)
	  (SG-FRAME-ARG-VALUE EH-SG EH-FRAME NAME-OR-NUMBER ERRORP)
	(IF BARF (FORMAT T "~&~A" BARF VAL1) VAL1))
    (MULTIPLE-VALUE-BIND (VAL NIL)
	(SG-LISTIFY-ARGS-AND-LOCALS EH-SG EH-FRAME)
      VAL)))

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

(DEFUN EH-VAL (&OPTIONAL (NUMBER 0))
  "Return the value of specified value being returned by current frame.
NUMBER is which value to return (origin 0)"
  (MULTIPLE-VALUE-BIND (VAL1 NIL BARF)
      (SG-FRAME-VALUE-VALUE EH-SG EH-FRAME NUMBER NIL)
    (IF BARF (FORMAT T "~&~A" BARF) VAL1)))

(DEFUN EH-FUN (&AUX (RP (SG-REGULAR-PDL EH-SG)))
  "Return the name of the function called in the current frame."
  (FUNCTION-NAME (RP-FUNCTION-WORD RP EH-FRAME)))

(DEFSETF EH-ARG SET-EH-ARG)
(DEFSETF EH-LOC SET-EH-LOC)
(DEFSETF EH-VAL SET-EH-VAL)
(DEFSETF EH-FUN SET-EH-FUN)

(DEFPROP EH-ARG EH-ARG-LOCATION SI:LOCF-METHOD)
(DEFPROP EH-LOC EH-LOC-LOCATION SI:LOCF-METHOD)
(DEFPROP EH-VAL EH-VAL-LOCATION SI:LOCF-METHOD)
(DEFPROP EH-FUN EH-FUN-LOCATION SI:LOCF-METHOD)

(DEFUN SET-EH-ARG (NAME-OR-NUMBER VALUE)
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SG-FRAME-ARG-VALUE EH-SG EH-FRAME NAME-OR-NUMBER)
    (RPLACD LOC VALUE)
    VALUE))

(DEFUN SET-EH-LOC (NAME-OR-NUMBER VALUE)
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SG-FRAME-LOCAL-VALUE EH-SG EH-FRAME NAME-OR-NUMBER)
    (RPLACD LOC VALUE)
    VALUE))

(DEFUN SET-EH-VAL (NAME-OR-NUMBER VALUE)
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SG-FRAME-VALUE-VALUE EH-SG EH-FRAME NAME-OR-NUMBER T)
    (AND LOC (RPLACD LOC VALUE))
    VALUE))

(DEFUN SET-EH-FUN (VALUE &AUX (RP (SG-REGULAR-PDL EH-SG)))
  (SETF (RP-FUNCTION-WORD RP EH-FRAME) VALUE))

(DEFUN EH-ARG-LOCATION (NAME-OR-NUMBER)
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SG-FRAME-ARG-VALUE EH-SG EH-FRAME NAME-OR-NUMBER)
    LOC))

(DEFUN EH-LOC-LOCATION (NAME-OR-NUMBER)
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SG-FRAME-LOCAL-VALUE EH-SG EH-FRAME NAME-OR-NUMBER)
    LOC))

(DEFUN EH-VAL-LOCATION (NAME-OR-NUMBER)
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SG-FRAME-VALUE-VALUE EH-SG EH-FRAME NAME-OR-NUMBER T)
    LOC))

(DEFUN EH-FUN-LOCATION (&AUX (RP (SG-REGULAR-PDL EH-SG)))
  (LOCF (RP-FUNCTION-WORD RP EH-FRAME)))

(DEFUN SG-FRAME-ARG-VALUE (SG FRAME ARGNUM &OPTIONAL (ERRORP T))
  "Return the value and location of arg number ARGNUM in FRAME in SG.
Checks ARGNUM for being in bounds, if the frame is active
/(for an open frame, it is not known how many args there are).
The second value is where the value is located when SG is running;
this may be a symbol value cell, etc., if the arg is special.
ERRORP non-NIL means signal an error if ARGNUM is invalid.
ERRORP NIL means retrun a third value which describes the problem, if any."
  (DECLARE (VALUES VALUE LOCATION BARF))
  (CHECK-TYPE ARGNUM (OR SYMBOL STRING NUMBER))
  (BLOCK FUNCTION
    (PROG* ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
	    (NUM-ARGS (SG-NUMBER-OF-SPREAD-ARGS SG FRAME))
	    ARG-NAME
	    (REST-ARG-P
	      (AND (LEGITIMATE-FUNCTION-P FUNCTION)
		   (LDB-TEST #o2402 (ARGS-INFO FUNCTION)))))
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
	   (IF (AND ( ARGNUM NUM-ARGS) (SG-FRAME-ACTIVE-P SG FRAME))
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
		   (ALOC (SG-REGULAR-PDL SG) (+ FRAME ARGNUM 1))))))

(DEFUN SG-FRAME-LOCAL-VALUE (SG FRAME LOCALNUM &OPTIONAL (ERRORP T))
  "Return the value and location of local variable number LOCALNUM in FRAME in SG.
Checks LOCALNUM for being in bounds.
The second value is where the value is located when SG is running;
this may be a symbol value cell, etc., if the arg is special.
ERRORP non-NIL means signal an error if ARGNUM is invalid.
ERRORP NIL means retrun a third value which describes the problem, if any."
  (DECLARE (VALUES VALUE LOCATION BARF))
  (CHECK-TYPE LOCALNUM (OR SYMBOL STRING NUMBER))
  (BLOCK FUNCTION
    (PROG* ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
	    (NUM-LOCALS (SG-NUMBER-OF-LOCALS SG FRAME))
	    LOCAL-NAME)
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
	     (RETURN (AREF RP RPIDX) (ALOC RP RPIDX))))))

(defun sg-frame-stack-temporary-value (sg frame number &optional (error t))
  "Return the value of the NUMBER'th temporary pushed onto the stack in FRAME.
The second value returned is the location where the temporary is stored."
  (declare (values value location barf))
  (check-type number number)
  (let* ((rp (sg-regular-pdl sg))
	 (function (rp-function-word rp frame))
	 (n-locals 0) (nargs 0))
    (when (sg-frame-active-p sg frame)
      (when (legitimate-function-p function)
	(setq nargs (sg-number-of-spread-args sg frame))
	(setq n-locals (fef-number-of-locals function))))
    (let* ((prev-open (sg-previous-open sg frame))
	   (upper (if prev-open (- prev-open 4) (sg-regular-pdl-pointer sg))))
      ;;>> took out test for ( number 0)
      (if (< (setq number (+ frame n-locals nargs 1 number)) upper)
	  (values (aref rp number) (locf (aref rp number)))
	(let ((string "There are only ~D temporar~:@P in this stack frame"))
	  (if error
	      (ferror nil string (- upper frame n-locals nargs 1))
	    (values nil nil (format nil string (- upper frame n-locals nargs 1)))))))))

(DEFUN SG-FRAME-VALUE-LIST (SG FRAME &OPTIONAL NEW-NUMBER-OF-VALUES
			    (ORIGINAL-FRAME FRAME)
			    &AUX (RP (SG-REGULAR-PDL SG)))
  "Return a list in which live the values being or to be returned by FRAME in SG.
The second value is NIL if this frame has not been invoked to return multi values,
a number which is the number of values it has been asked for,
or a locative, meaning the frame was called with MULTIPLE-VALUE-LIST.
In the last case, the first value includes only the values FRAME
has returned already, and the locative points to a cell that points
to the tail of the list (or to the place where the list lives,
if the list is NIL).

The third value is how many values FRAME has returned so far.

If NEW-NUMBER-OF-VALUES is non-NIL, it is used to alter the
/"number of values already returned/" as recorded in the stack group.
This may alter the length of the list which is the first value.
The value you get is the altered one, in that case."
  (DECLARE (VALUES VALUE-LIST TAIL-LOCATION-OR-TOTAL-EXPECTED NUMBER-RETURNED-SO-FAR))
  (COND ((= (RP-DESTINATION RP FRAME)
	    (LDB #o1602 (OR (GET 'COMPILER:D-RETURN 'COMPILER:QLVAL) #o100000)))
	 (SG-FRAME-VALUE-LIST SG (SG-NEXT-ACTIVE SG FRAME)
			      NEW-NUMBER-OF-VALUES ORIGINAL-FRAME))
	((NOT (ZEROP (RP-ADI-PRESENT RP FRAME)))
	 (DO ((IDX (- FRAME 4) (- IDX 2)))
	     (())
	   (LET ((TYPE (LDB SI:%%ADI-TYPE (AREF RP IDX)))
		 (MORE-P (%P-LDB %%ADI-PREVIOUS-ADI-FLAG (ALOC RP (1- IDX)))))
	     (AND (OR (= TYPE ADI-RETURN-INFO)
		      (= TYPE ADI-USED-UP-RETURN-INFO))
		  (LET ((STORING-OPTION (NTH (LDB %%ADI-RET-STORING-OPTION (AREF RP IDX))
					     ADI-STORING-OPTIONS)))
		    (COND ((EQ STORING-OPTION 'ADI-ST-BLOCK)
			   (LET* ((NUM-TOTAL (LDB %%ADI-RET-NUM-VALS-TOTAL (AREF RP IDX)))
				  (NUM-ALREADY
				    (- NUM-TOTAL
				       (LDB %%ADI-RET-NUM-VALS-EXPECTING (AREF RP IDX))))
				  (POINTER
				    (%MAKE-POINTER-OFFSET DTP-LIST (AREF RP (1- IDX))
							  (- NUM-ALREADY))))
			     ;; If requested, increase or decrease number of values
			     ;; "already returned".
			     (WHEN NEW-NUMBER-OF-VALUES
			       (SETQ NEW-NUMBER-OF-VALUES
				     (MAX 0 (MIN NEW-NUMBER-OF-VALUES NUM-TOTAL)))
			       ;; Must store these in a way that preserves cdr-codes.
			       (SETF (%P-LDB %%ADI-RET-NUM-VALS-EXPECTING
					     (ALOC RP IDX))
				     (- NUM-TOTAL NEW-NUMBER-OF-VALUES))
			       (SETF (CAR (ALOC RP (1- IDX)))
				     (%MAKE-POINTER-OFFSET DTP-LOCATIVE POINTER
							   NEW-NUMBER-OF-VALUES)))
			     ;; Make all the words for storing the values in
			     ;; into a cdr-coded list so we can return a pointer to it.
			     (UNLESS (ZEROP NUM-TOTAL)
			       (DOTIMES (I NUM-TOTAL)
				 (%P-DPB-OFFSET CDR-NEXT %%Q-CDR-CODE POINTER I))
			       (%P-DPB-OFFSET CDR-NIL %%Q-CDR-CODE POINTER (1- NUM-TOTAL)))
			     (RETURN POINTER NUM-TOTAL NUM-ALREADY
				     (= TYPE ADI-USED-UP-RETURN-INFO))))
			  ((EQ STORING-OPTION 'ADI-ST-LIST)
			   (ERROR "ADI-ST-LIST is used"))
			  ((EQ STORING-OPTION 'ADI-ST-INDIRECT)
			   ;; Ask about the frame that the indirect pointer points to.
			   ;; The pointer points to the highest word of ADI
			   ;; so add %LP-CALL-BLOCK-LENGTH to get the frame pointer.
			   (SG-FRAME-VALUE-LIST SG (+ %LP-CALL-BLOCK-LENGTH
						      (%POINTER-DIFFERENCE
							(AREF RP (1- IDX))
							(ALOC RP 0)))
						NEW-NUMBER-OF-VALUES ORIGINAL-FRAME))
			  ((EQ STORING-OPTION 'ADI-ST-MAKE-LIST)
			   ;; Find the stack word that contains the pointer
			   ;; to the list being constructed.  It is the word below the ADI.
			   (LET ((LIST-SLOT-IDX
				  (DO ((IDX1 IDX (- IDX1 2)))
				      ((ZEROP (%P-LDB %%ADI-PREVIOUS-ADI-FLAG
						      (ALOC RP (1- IDX1))))
				       (- IDX1 2)))))
			     ;; If specified, add some values to the end of the list
			     ;; or flush some.
			     (COND ((NULL NEW-NUMBER-OF-VALUES))
				   ((> NEW-NUMBER-OF-VALUES
				       (LENGTH (AREF RP LIST-SLOT-IDX)))
				    (LET ((EXTRA (- NEW-NUMBER-OF-VALUES
						    (LENGTH (AREF RP LIST-SLOT-IDX)))))
				      (RPLACD (AREF RP (1- IDX)) (MAKE-LIST EXTRA))
				      (SETF (CAR (ALOC RP (1- IDX)))
					    (NTHCDR EXTRA (AREF RP (1- IDX))))))
				   (T (SETF (CAR (ALOC RP (1- IDX)))
					    (NTHCDR NEW-NUMBER-OF-VALUES
						    (LOCF (AREF RP LIST-SLOT-IDX))))
				      (RPLACD (AREF RP (1- IDX)) NIL)))
			     ;; Return the list, a flag, and the length of the list.
			     (RETURN (AREF RP LIST-SLOT-IDX)
				     (ALOC RP (1- IDX))
				     (LENGTH (AREF RP LIST-SLOT-IDX))
				     (= TYPE ADI-USED-UP-RETURN-INFO)))))))
	     (IF (ZEROP MORE-P)
		 ;; Ok, the ultimate frame being returned to is not asking for mult values.
		 ;; So, if this frame has not got a trap-on-exit, it has returned no values.
		 ;; But if it has got a trap on exit, it has returned one value.  Find it.
		 (RETURN (LIST (SG-FRAME-SINGLE-VALUE SG ORIGINAL-FRAME)))))))
	;; Likewise, in case where ultimate frame has no ADI at all.
	(T (LIST (SG-FRAME-SINGLE-VALUE SG ORIGINAL-FRAME)))))

(DEFUN SG-FRAME-SINGLE-VALUE (SG FRAME)
  (LET* (PREV
	 DOUBLEPREV
	 ;; In case called from EH-VAL inside the foothold.
	 (INNERMOST-VISIBLE-FRAME (SG-INNERMOST-ACTIVE SG))
	 (RP (SG-REGULAR-PDL SG))
	 ERROR)
    (COND ((= FRAME (SG-AP SG))
	   ;; This clause handles when we are called
	   ;; inside (:property exit-trap enter-error-handler)
	   (SG-AC-T SG))
	  ((AND (SETQ PREV (SG-PREVIOUS-ACTIVE SG FRAME INNERMOST-VISIBLE-FRAME))
		(RP-FUNCTION-WORD RP PREV)
		(EQ (RP-FUNCTION-WORD RP PREV)
		    #'FOOTHOLD)
		(SETQ DOUBLEPREV (SG-PREVIOUS-ACTIVE SG PREV INNERMOST-VISIBLE-FRAME))
		(OR (AND (EQ (RP-FUNCTION-WORD RP DOUBLEPREV)
			     #'FH-STREAM-BINDING-EVALER)
			 (SETQ ERROR (AREF RP (+ DOUBLEPREV 13.))))
		    (AND (EQ (RP-FUNCTION-WORD RP DOUBLEPREV)
			     #'FH-APPLIER-NO-RESTART)
			 (SETQ ERROR (CAR (AREF RP (+ DOUBLEPREV 2))))))
		(CONDITION-TYPEP ERROR 'EXIT-TRAP-ERROR))
	   (CAR (SEND ERROR ':VALUES))))))

(DEFUN SG-FRAME-VALUE-VALUE (SG FRAME NUMBER &OPTIONAL CREATE-SLOT)
  "Return the value of the NUMBER'th value being returned by FRAME.
The second value is the location where that value is stored.
CREATE-SLOT means make sure there is a slot for the value to live in;
this makes a difference if FRAME has been asked to return arbitrarily
many values with MULTIPLE-VALUE-LIST."
  (IF CREATE-SLOT
      (CHECK-TYPE NUMBER NUMBER)
    (CHECK-TYPE NUMBER (OR NUMBER NULL)))
  (MULTIPLE-VALUE-BIND (VALUE-LIST TAIL-LOCATION NUM-ALREADY)
      (SG-FRAME-VALUE-LIST SG FRAME)
    (IF (NULL NUMBER)
	VALUE-LIST
      (AND CREATE-SLOT ( NUM-ALREADY NUMBER)
	   (MULTIPLE-VALUE (VALUE-LIST TAIL-LOCATION NUM-ALREADY)
	     (SG-FRAME-VALUE-LIST SG FRAME (1+ NUMBER))))
      (LET ((SLOT (NTHCDR NUMBER VALUE-LIST)))
	(AND SLOT (VALUES (CAR SLOT) (LOCF (CAR SLOT))))))))

(DEFUN SG-RETURN-ADDITIONAL-VALUE (SG FRAME VALUE)
  "Add VALUE onto the list of values being returned by FRAME.
Used by trap-on-exit so that SG-FRAME-VALUE-LIST returns a list
containing ALL the values, including the last one.
Returns T if we were able to add another value."
  (MULTIPLE-VALUE-BIND (VALUE-LIST TAIL-LOCATION NUM-ALREADY USED-UP)
      (SG-FRAME-VALUE-LIST SG FRAME)
    (COND (USED-UP NIL)
	  (TAIL-LOCATION
	   ;; If frame is feeding multiple values, try to add one more.
	   (MULTIPLE-VALUE (VALUE-LIST TAIL-LOCATION)
	     (SG-FRAME-VALUE-LIST SG FRAME (1+ NUM-ALREADY)))
	   ;; Now see whether a slot exists for the additional value.
	   (LET ((SLOT (NTHCDR NUM-ALREADY VALUE-LIST)))
	     ;; If so, store in it
	     (AND SLOT (RPLACA SLOT VALUE))
	     ;; and return non-NIL if the slot exists.
	     SLOT)))))

(DEFUN SG-DISCARD-LAST-VALUE (SG FRAME)
  "Discard the last value of the multiple values FRAME is returning.
Used in proceeding from EXIT-TRAP to /"un-return/" the last value
to give it back to the system, which will go and return it again!
We return as a value the value that was un-returned.
If the frame is not feeding multiple values, this returns its only value
but changes nothing."
  (MULTIPLE-VALUE-BIND (VALUE-LIST TAIL-LOCATION NUM-ALREADY USED-UP)
      (SG-FRAME-VALUE-LIST SG FRAME)
    (COND (USED-UP NIL)
	  (TAIL-LOCATION
	   (PROG1 (NTH (MAX 0 (1- NUM-ALREADY)) VALUE-LIST)
		  ;; If frame is feeding multiple values, try to flush one.
		  (SG-FRAME-VALUE-LIST SG FRAME (1- NUM-ALREADY))))
	  (T (CAR VALUE-LIST)))))

(DEFUN SG-REST-ARG-VALUE (SG FRAME &AUX
			     (RP (SG-REGULAR-PDL SG))
			     (AP FRAME)
			     LEXPR-CALL ARGS-INFO REST-ARG
			     (FUNCTION (RP-FUNCTION-WORD RP AP))
			     (NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))
			     (NARGS-EXPECTED NARGS-SUPPLIED))
  "Get the value of the rest arg in FRAME in SG.
The first value is the value of the rest arg (nil if the frame has none).
The second value is T if the function expects to have one.
The third value indicates a rest arg that does not overlap the stack frame."
  (DECLARE (VALUES REST-ARG-VALUE REST-ARG-EXPECTED REST-ARG-EXPLICIT))
  (COND ((LEGITIMATE-FUNCTION-P FUNCTION)
	 (SETQ ARGS-INFO (ARGS-INFO FUNCTION))
	 (SETQ REST-ARG (LDB-TEST #o2402 ARGS-INFO))
	 (SETQ NARGS-EXPECTED (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
  (WHEN (AND REST-ARG
	     (TYPEP FUNCTION 'COMPILED-FUNCTION)
	     (= (1+ NARGS-EXPECTED) (RP-LOCAL-BLOCK-ORIGIN RP AP))
	     (AREF RP (+ AP 1 NARGS-EXPECTED)))	;Local 0
    (SETQ LEXPR-CALL T))
  (VALUES
    (IF (TYPEP FUNCTION 'COMPILED-FUNCTION)
	(AREF RP (+ AP (RP-LOCAL-BLOCK-ORIGIN RP AP)))
      (IF (> NARGS-SUPPLIED NARGS-EXPECTED)
	  (%MAKE-POINTER DTP-LIST (ALOC RP (+ AP NARGS-EXPECTED 1)))
	NIL))
    REST-ARG
    LEXPR-CALL))

(DEFUN SG-LISTIFY-ARGS-AND-LOCALS (SG FRAME)
  (MULTIPLE-VALUE-BIND (NIL TEM EXPLICIT-REST-ARG)
      (SG-REST-ARG-VALUE SG FRAME)
    (LET* ((RP (SG-REGULAR-PDL SG))
	   (LOCALP (RP-LOCAL-BLOCK-ORIGIN RP FRAME))
	   (NARGS (IF (ZEROP LOCALP)
		      (IF EXPLICIT-REST-ARG (1- (RP-NUMBER-ARGS-SUPPLIED RP FRAME))
			(RP-NUMBER-ARGS-SUPPLIED RP FRAME))
		    (1- LOCALP))))
      (DO ((I 1 (1+ I))
	   (END (1+ NARGS)))
	  (( I END)
	   (OR (= I 1)
	       (%P-STORE-CDR-CODE (ALOC RP (+ FRAME I -1)) CDR-NIL)))
	(%P-STORE-CDR-CODE (ALOC RP (+ FRAME I)) CDR-NEXT))
      (DO ((I 0 (1+ I))
	   (N-LOCALS (SG-NUMBER-OF-LOCALS SG FRAME)))
	  (( I N-LOCALS)
	   (OR (ZEROP I)
	       (%P-STORE-CDR-CODE (ALOC RP (+ FRAME LOCALP I -1)) CDR-NIL)))
	(%P-STORE-CDR-CODE (ALOC RP (+ FRAME LOCALP I)) CDR-NEXT))
      (WHEN (AND TEM EXPLICIT-REST-ARG)
	(%P-STORE-CDR-CODE (ALOC RP (+ FRAME LOCALP -1)) CDR-NORMAL))
      (VALUES (IF (PLUSP NARGS)
		  (%MAKE-POINTER DTP-LIST (ALOC RP (1+ FRAME)))
		(SG-REST-ARG-VALUE SG FRAME))
	      (AND (PLUSP (SG-NUMBER-OF-LOCALS SG FRAME))
		   (%MAKE-POINTER DTP-LIST (ALOC RP (+ FRAME LOCALP))))))))

(DEFUN LEGITIMATE-FUNCTION-P (FUNCTION)
  "T if FUNCTION is a reasonable argument to give to ARGS-INFO."
  (OR (TYPEP FUNCTION '(OR COMPILED-FUNCTION MICROCODE-FUNCTION))
      (MEMQ (CAR-SAFE FUNCTION) '(LAMBDA NAMED-LAMBDA SUBST NAMED-SUBST CLI:SUBST))))

(DEFF SG-FRAME-NUMBER-OF-SPREAD-ARGS 'SG-NUMBER-OF-SPREAD-ARGS)

(DEFUN SG-NUMBER-OF-SPREAD-ARGS (SG FRAME &AUX
				    (RP (SG-REGULAR-PDL SG)) (AP FRAME)
				    ARGS-INFO REST-ARG-P NARGS-EXPECTED
				    (FUNCTION (RP-FUNCTION-WORD RP AP))
				    (NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP)))
  "Returns the number of spread args present in FRAME in SG.
/"Spread/" args means that the elements of a rest arg normally do not count."
  (COND ((LEGITIMATE-FUNCTION-P FUNCTION)
	 (SETQ ARGS-INFO (ARGS-INFO FUNCTION))
	 (SETQ REST-ARG-P (LDB-TEST 2402 ARGS-INFO))
	 (SETQ NARGS-EXPECTED (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
  ;; The args that can be asked for are the ones supplied,
  ;; except that FEFs make slots for all args they expect whether supplied or not,
  ;; and if there is a rest arg it any unexpected spread args
  ;; are considered to be part of that.
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (IF REST-ARG-P NARGS-EXPECTED
	   (MAX (1- (RP-LOCAL-BLOCK-ORIGIN RP AP)) NARGS-EXPECTED NARGS-SUPPLIED)))
	(T (IF REST-ARG-P
	       (MIN NARGS-SUPPLIED NARGS-EXPECTED)
	     NARGS-SUPPLIED))))

(DEFUN SG-NUMBER-OF-LOCALS (SG FRAME &AUX RP FUNCTION)
  "Return the number of local variable slots present in FRAME in SG."
  (SETQ RP (SG-REGULAR-PDL SG)
	FUNCTION (RP-FUNCTION-WORD RP FRAME))
  (IF (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
      (FEF-NUMBER-OF-LOCALS FUNCTION)
    0))

(DEFUN FEF-NUMBER-OF-LOCALS (FUNCTION)
  (LENGTH (CADR (ASSQ 'COMPILER:LOCAL-MAP (DEBUGGING-INFO FUNCTION)))))

;;; These functions know about the location tags used in the ERROR-TABLE
;;; entries, and how to creates locatives to them, fetch from them,
;;; and store into them.
;;;   There is the issue that the contents may be illegal datatypes.
;;; Have to think about if there are screw cases, etc.

;;; Analysis
(DEFUN SG-CONTENTS (SG LOC)
  "Return the contents of state-element LOC in SG.
LOC can be an accumulator name such as M-A,
or PP meaning the word on top of the stack,
or VMA meaning the saved last memory address,
or RMD meaning the saved last memory data,
or a list (PP number) where number is negative,
 to index down the stack."
  (SELECTQ LOC
    (M-A (SG-AC-A SG))
    (M-B (SG-AC-B SG))
    (M-C (SG-AC-C SG))
    (M-D (SG-AC-C SG))
    (M-E (SG-AC-E SG))
    (M-T (SG-AC-T SG))
    (M-R (SG-AC-R SG))
    (M-Q (SG-AC-Q SG))
    (M-I (SG-AC-I SG))
    (M-J (SG-AC-J SG))
    (M-S (SG-AC-S SG))
    (M-K (SG-AC-K SG))
    (M-1 (DPB (LDB #o1010 (SG-VMA-M1-M2-TAGS SG)) #o3010	;A positive fixnum
	      (DPB (LDB #o2701 (SG-AC-1 SG)) #o2701
		   (LDB #o0027 (SG-AC-1 SG)))))
    (M-2 (DPB (LDB #o2010 (SG-VMA-M1-M2-TAGS SG)) #o3010	;A positive fixnum
	      (DPB (LDB #o2701 (SG-AC-2 SG)) #o2701
		   (LDB #o0027 (SG-AC-2 SG)))))
    (A-QCSTKG SG)
    (A-SG-PREVIOUS-STACK-GROUP (SG-PREVIOUS-STACK-GROUP SG))
    (PP (AREF (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
    (RMD (%P-CONTENTS-OFFSET (SG-SAVED-VMA SG) 0))
    (VMA (SG-SAVED-VMA SG))		;VMA without its data type
    (OTHERWISE
      (COND ((AND (CONSP LOC) (EQ (CAR LOC) 'PP))
	     (AREF (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
	    ((BAD-HACKER LOC "Unknown tag"))))))

(DEFSETF SG-CONTENTS (SG LOC) (VAL) `(SG-STORE ,VAL ,SG ,LOC))

;;; Metamorphosis
(DEFUN SG-STORE (X SG LOC)
  "Store X into state-element LOC in SG.  See SG-CONTENTS for what LOC means."
  (SELECTQ LOC
    (M-A (SETF (SG-AC-A SG) X))
    (M-B (SETF (SG-AC-B SG) X))
    (M-C (SETF (SG-AC-C SG) X))
    (M-D (SETF (SG-AC-C SG) X))
    (M-E (SETF (SG-AC-E SG) X))
    (M-T (SETF (SG-AC-T SG) X))
    (M-R (SETF (SG-AC-R SG) X))
    (M-Q (SETF (SG-AC-Q SG) X))
    (M-I (SETF (SG-AC-I SG) X))
    (M-J (SETF (SG-AC-J SG) X))
    (M-S (SETF (SG-AC-S SG) X))
    (M-K (SETF (SG-AC-K SG) X))
    (A-QCSTKG (ERROR T "You can't store in this!"))
    (A-SG-PREVIOUS-STACK-GROUP (SETF (SG-PREVIOUS-STACK-GROUP SG) X))
    (PP (ASET X (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
    (RMD (RPLACD (SG-SAVED-VMA SG) X))		;follows invisible pointers
    (VMA (SETF (SG-SAVED-VMA SG) X))		;just the pointer part of VMA
    (OTHERWISE
      (COND ((AND (CONSP LOC) (EQ (CAR LOC) 'PP))
	     (ASET X (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
	    ((BAD-HACKER LOC "Unknown tag"))))))

(DEFLOCF SG-CONTENTS SG-LOCATE)

;;; Getllocativepointersis
(DEFUN SG-LOCATE (SG LOC)
  "Return the location of state-element LOC in SG.  See SG-CONTENTS for what LOC means."
  (SELECTQ LOC
    (M-A (LOCF (SG-AC-A SG)))
    (M-B (LOCF (SG-AC-B SG)))
    (M-C (LOCF (SG-AC-C SG)))
    (M-D (LOCF (SG-AC-D SG)))
    (M-E (LOCF (SG-AC-E SG)))
    (M-T (LOCF (SG-AC-T SG)))
    (M-R (LOCF (SG-AC-R SG)))
    (M-Q (LOCF (SG-AC-Q SG)))
    (M-I (LOCF (SG-AC-I SG)))
    (M-J (LOCF (SG-AC-J SG)))
    (M-S (LOCF (SG-AC-S SG)))
    (M-K (LOCF (SG-AC-K SG)))
    (A-QCSTKG (%MAKE-POINTER DTP-LOCATIVE SG))
    (A-SG-PREVIOUS-STACK-GROUP (LOCF (SG-PREVIOUS-STACK-GROUP SG)))
    (PP (ALOC (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
    (RMD (%MAKE-POINTER DTP-LOCATIVE (SG-SAVED-VMA SG)))
    (OTHERWISE
      (COND ((AND (CONSP LOC) (EQ (CAR LOC) 'PP))
	     (ALOC (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
	    ((BAD-HACKER LOC "Unknown tag"))))))

;;; Printoutosis (is that a disease?)
(DEFUN SG-PRINT-CAREFUL (SG LOC)
  "Print the contents of state-element LOC in SG.  See SG-CONTENTS for what LOC means."
  (IF (MEMQ LOC '(M-1 M-2 VMA))
      (PRIN1 (SG-CONTENTS SG LOC))		;Can't locate, but no data type either
    (P-PRIN1-CAREFUL (SG-LOCATE SG LOC))))	;Careful of bad data types

(DEFUN SG-FIXNUM-CONTENTS (SG LOC)
  "Return the contents of status item LOC of SG, as a fixnum.
If the status item contains a Lisp datum, its address alone is returned.
See SG-CONTENTS for the meaning of LOC.
As a special feature, LOC can be a number; then LOC itself is the value."
  (COND ((NUMBERP LOC) LOC)			;Constant
	((MEMQ LOC '(M-1 M-2)) (SG-CONTENTS SG LOC))
	((EQ LOC 'VMA) (%POINTER (SG-SAVED-VMA SG)))
	(T (%P-LDB %%Q-POINTER (SG-LOCATE SG LOC)))))

(DEFUN SG-FIXNUM-STORE (X SG LOC)
  "Set the pointer field of status item LOC in SG to X."
  (%P-DPB X %%Q-POINTER (SG-LOCATE SG LOC)))

(DEFUN SG-BINDING-POSITION (SG VARIABLE)
  "Return the position in SG's specpdl of the outermost binding of VARIABLE.
Returns NIL if VARIABLE is not bound in stack group SG."
  (DO ((I 0 (1+ I))
       (LIM (1+ (SG-SPECIAL-PDL-POINTER SG)))
       (SP (SG-SPECIAL-PDL SG)))
      (( I LIM))
    (IF (AND (%P-POINTERP (ALOC SP I))
	     (EQ (%P-CONTENTS-AS-LOCATIVE (ALOC SP I))
		 (VALUE-CELL-LOCATION VARIABLE)))
	(RETURN (1- I)))))

(DEFVAR INSERT-BINDING-IN-CLOSURE-TEMP)

(DEFUN INSERT-BINDING-IN-CLOSURE (CLOSURE VARIABLE BEFORE-VARIABLE &OPTIONAL XVCELL)
  "Put a binding of VARIABLE into CLOSURE, unless there is one already.
Returns T if a new binding was inserted.
The binding's initial value is copied from the current global binding.
If we are running inside CLOSURE, a suitable binding for VARIABLE
is entered in the binding stack so that it is in effect now.
This is done by looking for an existing binding of BEFORE-VARIABLE
and inserting the new binding before it.  BEFORE-VARIABLE should be
a variable that one can assume that CLOSURE would bind and that
nothing else would bind.

If XVCELL is non-NIL, it should be a pointer to a cell
which is used as the closure value cell.  In this case,
the cell's contents are not changed, so they become VARIABLE's value."
  (WITHOUT-INTERRUPTS
    (UNLESS (GET-LOCATION-OR-NIL (%MAKE-POINTER DTP-LIST CLOSURE)
				 (VALUE-CELL-LOCATION VARIABLE))
      (LET ((POSITION (SG-BINDING-POSITION CURRENT-STACK-GROUP BEFORE-VARIABLE))
	    (OVCELL (FOLLOW-CELL-FORWARDING (VALUE-CELL-LOCATION VARIABLE) T)))
	(UNLESS XVCELL
	  (SETQ XVCELL (LIST NIL))
	  ;; Copy the current global value into the new binding cell.
	  (%P-STORE-DATA-TYPE XVCELL (%P-DATA-TYPE OVCELL))
	  (%P-STORE-POINTER XVCELL (%P-POINTER OVCELL)))
	(WHEN POSITION
	  ;; Note: SG-INSERT-SPECIAL-BINDING has no way to update
	  ;; the actual SP pointer we are running with.
	  ;; Therefore, its effect is to push this binding of INSERT-BINDING-IN-CLOSURE-TEMP
	  ;; off the top of the stack and into oblivion.
	  ;; If we did not bind INSERT-BINDING-IN-CLOSURE-TEMP,
	  ;; our binding of INHIBIT-SCHEDULING-FLAG
	  ;; would get moved into oblivion and our binding block would vanish,
	  ;; leaving the stack out of synch.
	  (%BIND (LOCF INSERT-BINDING-IN-CLOSURE-TEMP) NIL)
	  (SETF (SG-SPECIAL-PDL-POINTER CURRENT-STACK-GROUP)
		(GET-OWN-SPECIAL-PDL-POINTER))
	  (SG-INSERT-SPECIAL-BINDING CURRENT-STACK-GROUP POSITION
				     (VALUE-CELL-LOCATION VARIABLE) T)
	  (%P-DPB (%P-LDB %%SPECPDL-BLOCK-START-FLAG
			  (ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) (+ POSITION 2)))
		  %%SPECPDL-BLOCK-START-FLAG
		  (ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) POSITION))
	  (%P-DPB 0 %%SPECPDL-BLOCK-START-FLAG
		  (ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) (+ POSITION 2)))
	  (%P-STORE-DATA-TYPE (VALUE-CELL-LOCATION VARIABLE) DTP-FIX)
	  (%P-STORE-POINTER (VALUE-CELL-LOCATION VARIABLE) XVCELL)
	  (%P-STORE-DATA-TYPE (VALUE-CELL-LOCATION VARIABLE)
			      DTP-EXTERNAL-VALUE-CELL-POINTER))
	(SETF (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
	      (LIST* (VALUE-CELL-LOCATION VARIABLE)
		     (%MAKE-POINTER DTP-LOCATIVE XVCELL)
		     (CDR (%MAKE-POINTER DTP-LIST CLOSURE)))))
      T)))

(DEFUN DELETE-BINDING-FROM-CLOSURE (CLOSURE VARIABLE)
  "Remove the binding of VARIABLE from CLOSURE.
If VARIABLE is currently bound, that binding is assumed to come from CLOSURE
and is therefore removed from the binding stack."
  (LET ((POSITION (SG-BINDING-POSITION CURRENT-STACK-GROUP VARIABLE)))
    (WHEN POSITION
      (WITHOUT-INTERRUPTS
	(%P-DPB (%P-LDB %%SPECPDL-BLOCK-START-FLAG
			(ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) POSITION))
		%%SPECPDL-BLOCK-START-FLAG
		(ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) (+ POSITION 2)))
	;; Deleting the binding does not update the actual specpdl pointer
	;; we are running with.  So it has the effect of duplicating this binding
	;; of INSERT-BINDING-IN-CLOSURE-TEMP.
	;; That does no harm BECAUSE this is not the start of a binding block.
	(%BIND (LOCF INSERT-BINDING-IN-CLOSURE-TEMP) NIL)
	(SETF (SG-SPECIAL-PDL-POINTER CURRENT-STACK-GROUP)
	      (GET-OWN-SPECIAL-PDL-POINTER))
	(SG-DELETE-SPECIAL-BINDING CURRENT-STACK-GROUP POSITION))))
  (REMPROP (%MAKE-POINTER DTP-LIST CLOSURE)
	   (VALUE-CELL-LOCATION VARIABLE)))

(DEFUN SG-INSERT-SPECIAL-BINDING (SG POSITION BOUND-LOCATION &OPTIONAL CLOSURE-FLAG)
  "Insert a binding for BOUND-LOCATION into the special pdl for SG.
The binding is inserted at position POSITION in the special pdl,
and the data that was at POSITION is moved up.
The new binding is part of the binding block that precedes it.
The old value saved in the binding is the current contents of BOUND-LOCATION.
If CLOSURE-FLAG is non-NIL, the inserted binding is marked as
 /"made by closure entry/" as opposed to /"made by execution of the frame's function/".
**WARNING** unsafe to use on the current stack group
without hairy precautions; see source for EH:INSERT-BINDING-FROM-CLOSURE."
  (RELOCATE-SPECPDL-PORTION SG POSITION 2)
  (%BLT-TYPED BOUND-LOCATION (ALOC (SG-SPECIAL-PDL SG) POSITION) 1 1)
  (SETF (AREF (SG-SPECIAL-PDL SG) (1+ POSITION)) BOUND-LOCATION)
  (%P-STORE-CDR-CODE (ALOC (SG-SPECIAL-PDL SG) POSITION) 0)
  (%P-STORE-CDR-CODE (ALOC (SG-SPECIAL-PDL SG) (1+ POSITION)) 0)
  (%P-DPB (IF CLOSURE-FLAG 1 0) %%SPECPDL-CLOSURE-BINDING
	  (ALOC (SG-SPECIAL-PDL SG) POSITION))
  (%P-DPB (IF CLOSURE-FLAG 1 0) %%SPECPDL-CLOSURE-BINDING
	  (ALOC (SG-SPECIAL-PDL SG) (1+ POSITION))))

(DEFUN SG-DELETE-SPECIAL-BINDING (SG POSITION)
  "Delete one binding from the special pdl for SG.
The binding is deleted at position POSITION in the special pdl,
and the data that was at POSITION is moved down.
**WARNING** unsafe to use on the current stack group
without hairy precautions; see source for SYS:DELETE-BINDING-FROM-CLOSURE."
  ;; Restore the binding's saved old value.
  (%BLT-TYPED (ALOC (SG-SPECIAL-PDL SG) POSITION)
	      (AREF (SG-SPECIAL-PDL SG) (1+ POSITION))
	      1 1)
  (RELOCATE-SPECPDL-PORTION SG POSITION -2))

(DEFUN RELOCATE-SPECPDL-PORTION (SG START DISTANCE)
  (CHECK-ARG-TYPE DISTANCE FIXNUM)
  (CHECK-ARG-TYPE START FIXNUM)
  (CHECK-ARG-TYPE SG STACK-GROUP)
  (UNLESS (ZEROP DISTANCE)
    (LET ((SP (SG-SPECIAL-PDL SG))
	  (SPP (SG-SPECIAL-PDL-POINTER SG))
	  (RP (SG-REGULAR-PDL SG)))
      (IF (PLUSP DISTANCE)
	  (%BLT (ALOC SP SPP) (ALOC SP (+ SPP DISTANCE))
		(- SPP START -1) -1)
	(%BLT (ALOC SP (- START DISTANCE)) (ALOC SP START)
	      (- SPP START -1 (- DISTANCE)) 1))
      (LET ((SPBEG (ALOC SP 0)))
	(DOTIMES (I (IF (EQ SG CURRENT-STACK-GROUP)
			(SG-INNERMOST-OPEN SG)
		      (1+ (SG-REGULAR-PDL-POINTER SG))))
	  (IF (AND (%POINTERP (AREF RP I))
		   ( START (%POINTER-DIFFERENCE (AREF RP I) SPBEG) SPP))
	      (SETF (AREF RP I)
		    (%MAKE-POINTER-OFFSET (%DATA-TYPE (AREF RP I))
					  (AREF RP I)
					  DISTANCE)))))
      (INCF (SG-SPECIAL-PDL-POINTER SG) DISTANCE)
      (DO ((FRAME (SG-INNERMOST-OPEN SG) (SG-NEXT-OPEN SG FRAME)))
	  ((NULL FRAME))
	(IF (NOT (ZEROP (RP-ADI-PRESENT RP FRAME)))
	    (DO ((IDX (- FRAME %LP-CALL-BLOCK-LENGTH) (- IDX 2)))
		(())
	      (WHEN (= (LDB %%ADI-TYPE (AREF RP IDX)) ADI-BIND-STACK-LEVEL)
		(INCF (AREF RP (1- IDX)) DISTANCE))
	      (IF (ZEROP (%P-LDB %%ADI-PREVIOUS-ADI-FLAG (ALOC RP (1- IDX))))
		  (RETURN))))))))

(DEFUN SYMBOL-FROM-VALUE-CELL-LOCATION (LOC &AUX SYM)
  "Given LOC which is VALUE-CELL-LOCATION of some symbol, return that symbol."
  (COND ((AND ( (%POINTER LOC) A-MEMORY-VIRTUAL-ADDRESS)	;Microcode location
	      (< (%POINTER LOC) IO-SPACE-VIRTUAL-ADDRESS))	; forwarded from value cell
	 (OR (DOLIST (SYM A-MEMORY-LOCATION-NAMES)
	       (AND (= (%POINTER LOC) (%P-LDB-OFFSET %%Q-POINTER SYM 1)) (RETURN SYM)))
	     (DOLIST (SYM M-MEMORY-LOCATION-NAMES)
	       (AND (= (%POINTER LOC) (%P-LDB-OFFSET %%Q-POINTER SYM 1)) (RETURN SYM)))
	     LOC))
	((AND (SYMBOLP (SETQ SYM (%FIND-STRUCTURE-HEADER LOC)))	;Regular symbol's
	      (= (%POINTER-DIFFERENCE LOC SYM) 1))		; internal value-cell
	 SYM)
	(T LOC)))						;not a symbol

(DEFLOCF SYMEVAL-IN-STACK-GROUP (SYM SG &OPTIONAL FRAME)
  `(SYMEVAL-LOCATION-IN-STACK-GROUP ,SYM ,SG ,FRAME))

(DEFUN SYMEVAL-LOCATION-IN-STACK-GROUP (SYMBOL SG &OPTIONAL FRAME)
  (MULTIPLE-VALUE-BIND (NIL NIL LOCATION)
      (SYMEVAL-IN-STACK-GROUP SYMBOL SG FRAME)
    LOCATION))

(DEFSETF SYMEVAL-IN-STACK-GROUP (SYM SG &OPTIONAL FRAME) (VALUE)
  `(SET-IN-STACK-GROUP ,SYM ,SG ,VALUE ,FRAME))

(DEFUN SET-IN-STACK-GROUP (SYMBOL SG VALUE &OPTIONAL FRAME)
  "Set the value of SYMBOL in stack group SG to VALUE.
If FRAME is non-NIL, the value of the binding seen in that frame in SG is what is set."
  (MULTIPLE-VALUE-BIND (NIL NIL LOCATION)
      (SYMEVAL-IN-STACK-GROUP SYMBOL SG FRAME)
    (SETF (CONTENTS LOCATION) VALUE)))
  
(DEFUN SYMEVAL-IN-STACK-GROUP (SYM SG &OPTIONAL FRAME AS-IF-CURRENT)
  "Find the value of SYM in the binding environment of stack group SG's frame FRAME.
If FRAME is NIL, it means the stack group's current frame.
FRAME can be 0 to mean the global environment.
Don't call this if the stack-group could be running in another process
and thus changing its state.

If the variable's binding is unbound, the first value is NIL and so is the second.
Otherwise, the first value is the variable's value and the second is non-NIL
 (in fact, it is a copy of the third value).

The third value is the location of the binding, or NIL if there is no location.
If the variable was closure-bound, this location will contain an EVCP.
If AS-IF-CURRENT is non-nil, we return a LOCATION for where
 the value WOULD be when that SG is running.
The first value, however, is the current value -- not what is now stored in that location."
  (DECLARE (VALUES VALUE LOCATION BOUNDFLAG))
  (COND ((OR (EQ FRAME 0)
	     (AND (NEQ SG CURRENT-STACK-GROUP)
		  (NOT AS-IF-CURRENT)))
	 (PROG RESULT
	       ((VCL (FOLLOW-CELL-FORWARDING (VALUE-CELL-LOCATION SYM) NIL))
		(SP (SG-SPECIAL-PDL SG))
		(SPP (OR (AND (EQ FRAME 0) 0)
			 (AND FRAME (SG-PREVIOUS-INTERESTING-ACTIVE SG FRAME)
			      (SG-FRAME-SPECIAL-PDL-INDEX
				SG (SG-PREVIOUS-INTERESTING-ACTIVE SG FRAME)))
			 (SG-SPECIAL-PDL-POINTER SG))))
	   (OR (ZEROP (SG-IN-SWAPPED-STATE SG))	;If its bindings are swapped out
	       ( SPP 0)
	       (DO ((I SPP (1- I))		;then search through them
		    (P))
		   (( I 0))
		 (SETQ P (ALOC SP I))
		 (SELECT (%P-DATA-TYPE P)
		   (DTP-LOCATIVE
		    ;; If this is a binding pair
		    (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		    (IF (EQ (AREF SP I) VCL)
			;;and is for this variable, then return
			;;the saved value, invz'ing if necc
			(IF (LOCATION-BOUNDP (FOLLOW-CELL-FORWARDING P T))
			    (RETURN-FROM RESULT (CAR P) P P)
			  (RETURN-FROM RESULT NIL NIL P))
		      (DECF I)))		;Space over second Q of binding pair
		   (OTHERWISE ))))		;Ignore non-binding blocks
	   ;; The variable isn't bound in that stack group, so we want its global value.
	   ;; Must ignore bindings in our own stack group.
	   (SETQ SP (SG-SPECIAL-PDL CURRENT-STACK-GROUP)
		 SPP (GET-OWN-SPECIAL-PDL-POINTER SP))
	   (LET ((LOCATION (LOCF (SYMEVAL SYM))))
	     (DO ((I SPP (1- I))
		  (P))
		 (( I 0) (RETURN-FROM RESULT
			    (AND (LOCATION-BOUNDP (FOLLOW-CELL-FORWARDING LOCATION T))
				 (CONTENTS LOCATION))
			    (AND (LOCATION-BOUNDP (FOLLOW-CELL-FORWARDING LOCATION T))
				 LOCATION)
			    LOCATION))
	       (SETQ P (ALOC SP I))
	       (SELECT (%P-DATA-TYPE P)
		 (DTP-LOCATIVE
		  (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		  (COND ((EQ (AREF SP I) VCL)
			 (SETQ LOCATION P)))
		  (DECF I))
		 (OTHERWISE ))))))
	((AND (EQ SG CURRENT-STACK-GROUP)
	      (NULL FRAME))
	 (VALUES (SYMEVAL SYM) (VALUE-CELL-LOCATION SYM) (VALUE-CELL-LOCATION SYM)))
	(T
	 ;; Use specific frame in our current stack group,
	 ;; or in another stack group as if it were current.
	 (LET* ((VCL (FOLLOW-CELL-FORWARDING (VALUE-CELL-LOCATION SYM) NIL))
	        (SP (SG-SPECIAL-PDL SG))
		(INNERMOST-BINDING NIL)
		(SPP (IF (EQ SG CURRENT-STACK-GROUP)
			 (GET-OWN-SPECIAL-PDL-POINTER SP)
		       (SG-SPECIAL-PDL-POINTER SG)))
		(FRAMEP
		  (OR (LET ((INNERMOST-VISIBLE-FRAME (SG-AP SG)))
			(AND (SG-PREVIOUS-ACTIVE SG FRAME)
			     (SG-FRAME-SPECIAL-PDL-INDEX
			       SG (SG-PREVIOUS-ACTIVE SG FRAME))))
		      SPP)))
	   ;; Search through special pdl from the pointer out to specified frame
	   ;; and remember the LAST (outermost) binding we find!
	   ;; That is where the binding in the specified frame was saved.
	   (DO ((I SPP (1- I)))
	       (( I FRAMEP))
	     (LET ((P (ALOC SP I)))
	       (SELECT (%P-DATA-TYPE P)
		 (DTP-LOCATIVE				;If this is a binding pair
		  (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		  (IF (EQ (AREF SP I) VCL)		;and is for this variable, then save
		      (SETQ INNERMOST-BINDING P))	;pointer to the value-slot.
		  (DECF I)))))				;Space over second Q of binding pair
	   ;; No bindings since then -- use current one.
	   (OR INNERMOST-BINDING
	       (SETQ INNERMOST-BINDING (VALUE-CELL-LOCATION SYM)))
	   ;; Now we have the right LOCATION in INNERMOST-BINDING.
	   ;; But if not really current stack group, just pretending,
	   ;; then the current contents of that location is not the right VALUE.
	   (IF (EQ SG CURRENT-STACK-GROUP)
	       (VALUES (AND (LOCATION-BOUNDP (FOLLOW-CELL-FORWARDING INNERMOST-BINDING T))
			    (CONTENTS INNERMOST-BINDING))
		       (AND (LOCATION-BOUNDP (FOLLOW-CELL-FORWARDING INNERMOST-BINDING T))
			    INNERMOST-BINDING)
		       INNERMOST-BINDING)
	     (MULTIPLE-VALUE-BIND (VALUE NIL BOUNDP)
		 ;; So get the right value.
		 (SYMEVAL-IN-STACK-GROUP SYM SG FRAME NIL)
	       (VALUES VALUE (AND BOUNDP INNERMOST-BINDING) INNERMOST-BINDING)))))))

(DEFUN CELL-LOCATION-IN-STACK-GROUP (LOC SG &OPTIONAL FRAME)
  "Find the current location of the binding for location LOC in SG.
LOC should be the value-cell-location of a symbol, usually.
The value returned is the location of the place that saves,
when SG is not running, SG's binding for LOC.
If LOC is not bound in SG, the global binding is used."
  (DECLARE (VALUES VALUE LOCATION))
  (COND ((NEQ SG CURRENT-STACK-GROUP)
	 (BLOCK RESULT
	   (DO ((SP (SG-SPECIAL-PDL SG))
		(SPP (OR (AND FRAME (SG-PREVIOUS-INTERESTING-ACTIVE SG FRAME)
			      (SG-FRAME-SPECIAL-PDL-INDEX
				SG (SG-PREVIOUS-INTERESTING-ACTIVE SG FRAME)))
			 (SG-SPECIAL-PDL-POINTER SG))))
	       ()
	     (OR (ZEROP (SG-IN-SWAPPED-STATE SG))	;If its bindings are swapped out
		 ( SPP 0)
		 (DO ((I SPP (1- I))		;then search through them
		      (P))
		     (( I 0))
		   (SETQ P (ALOC SP I))
		   (SELECT (%P-DATA-TYPE P)
		     (DTP-LOCATIVE		;If this is a binding pair
		      (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		      (IF (EQ (AREF SP I) LOC)	;and is for this variable, then return
			  (RETURN-FROM RESULT P)
			(DECF I)))		;Space over second Q of binding pair
		     (OTHERWISE ))))		;Ignore non-binding blocks
	     ;; The cell isn't bound in that stack group, so we want its global value.
	     ;; Must ignore bindings in our own stack group.
	     (SETQ SP (SG-SPECIAL-PDL CURRENT-STACK-GROUP)
		   SPP (GET-OWN-SPECIAL-PDL-POINTER SP))
	     (DO ((LOCATION LOC)
		  (I SPP (1- I))
		  (P))
		 (( I 0) (RETURN-FROM RESULT LOCATION))
	       (SETQ P (ALOC SP I))
	       (SELECT (%P-DATA-TYPE P)
		 (DTP-LOCATIVE
		  (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		  (COND ((EQ (AREF SP I) LOC)
			 (SETQ LOCATION P)))
		  (DECF I))
		 (OTHERWISE ))))))
	((NULL FRAME)
	 LOC)
	(T
	 (FERROR NIL "FRAME-relative CELL-LOCATION in current stack group not yet implemented"))))

;;;; Various initialization routines.

(DEFUN ASSURE-TABLE-LOADED (&AUX (*READ-BASE* 8) (*PRINT-BASE* 8)
			    (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
  "Load the error table for our running microcode if it isn't already loaded."
  (UNLESS (= MICROCODE-ERROR-TABLE-VERSION-NUMBER %MICROCODE-VERSION-NUMBER)
    (LOAD-ERROR-TABLE)
    (UNLESS (= MICROCODE-ERROR-TABLE-VERSION-NUMBER %MICROCODE-VERSION-NUMBER)
      (BREAK "Cannot get error table!!"))))

(DEFUN LOAD-ERROR-TABLE ()
  (FORMAT *TERMINAL-IO* "~&[Loading error table for microcode version ~D]"
	  %MICROCODE-VERSION-NUMBER)
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-DESIRED-MICROCODE-VERSION)
	 %MICROCODE-VERSION-NUMBER)
  (WHEN (EQ SITE-NAME ':MIT)
    ;; Make this word look like a valid 24-bit fixnum with data type
    ;; so that it is possible to select this band from a brand S band.
    (%P-DPB DTP-FIX #o3005
	    (LOCF (SYSTEM-COMMUNICATION-AREA %SYS-COM-DESIRED-MICROCODE-VERSION))))
  (SI:WITH-SYS-HOST-ACCESSIBLE
    (LOAD (SELECT-PROCESSOR
	    (:CADR (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
			 :NEW-TYPE-AND-VERSION "TBL" %MICROCODE-VERSION-NUMBER))
	    (:LAMBDA (SEND (FS:PARSE-PATHNAME "SYS: UBIN; ULAMBDA")
			   :NEW-TYPE-AND-VERSION "LMC-TBL" %MICROCODE-VERSION-NUMBER)))
	  :PACKAGE "EH")))

;; Divides up MICROCODE-ERROR-TABLE into CALLS-SUB-LIST, RESTART-LIST, and ERROR-TABLE.
(DEFUN ASSURE-TABLE-PROCESSED (&AUX (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
  "Process the error table if it hasn't been processed."
  (UNLESS (= MICROCODE-ERROR-TABLE-VERSION-NUMBER ERROR-TABLE-NUMBER)
    (SETQ ERROR-TABLE NIL
	  CALLS-SUB-LIST NIL
	  RESTART-LIST NIL
	  STACK-WORDS-PUSHED-LIST NIL
	  ARG-POPPED-LIST NIL
	  DEFAULT-ARG-LOCATIONS-LIST NIL)
    (DOLIST (ET MICROCODE-ERROR-TABLE)
      (SELECTQ (CADR ET)
	(RESTART (PUSH (CONS (CADDR ET) (1+ (CAR ET))) RESTART-LIST))
	(CALLS-SUB (PUSH (CONS (CAR ET) (CADDR ET)) CALLS-SUB-LIST))
	(ARG-POPPED (PUSH (CONS (CAR ET) (CDDR ET)) ARG-POPPED-LIST))
	(DEFAULT-ARG-LOCATIONS (PUSH (CDDR ET) DEFAULT-ARG-LOCATIONS-LIST))
	(STACK-WORDS-PUSHED (PUSH (CONS (CAR ET) (CADDR ET)) STACK-WORDS-PUSHED-LIST))
	(OTHERWISE (PUSH ET ERROR-TABLE))))
    (SETQ BEGIN-QARYR (OR (CDR (ASSQ 'BEGIN-QARYR RESTART-LIST)) 0)
	  END-QARYR (OR (CDR (ASSQ 'END-QARYR RESTART-LIST)) 0)
	  ERROR-TABLE-NUMBER MICROCODE-ERROR-TABLE-VERSION-NUMBER)))

;;; Call this when it is apparent that some hacker set things up wrong.
(DEFUN BAD-HACKER (&REST ARGS)
  "Report an error in the error table."
  (FORMAT T "~2&Foo, a hacker has screwn up somewhere. Error:~%~{~S ~}~2&"
	  ARGS))

(DEFUN ENABLE-TRAPPING (&OPTIONAL (X 1))
  "Turn on invocation of the error handler for errors in the microcode."
  (ASSURE-FREE-SPACE)
  (SETQ %MODE-FLAGS (DPB X %%M-FLAGS-TRAP-ENABLE %MODE-FLAGS)))

(DEFUN TRAPPING-ENABLED-P NIL 
  (NOT (ZEROP (LDB %%M-FLAGS-TRAP-ENABLE %MODE-FLAGS))))

(DEFUN P-PRIN1-CAREFUL (LOCATIVE &AUX)
  "Print the contents of LOCATIVE, catching and reporting errors in printing."
  (COND ((%P-CONTENTS-SAFE-P LOCATIVE)
	 (PRINT-CAREFULLY "printing" (PRIN1 (CAR LOCATIVE))))
	(T (FORMAT T "#<~A ~O>" (%P-DATA-TYPE LOCATIVE) (%P-POINTER LOCATIVE)))))

(DEFUN P-PRIN1-CAREFUL-1 (LOCATIVE &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the contents of LOCATIVE, catching and reporting errors in printing.
This version does some invisible pointer following."
  (IF (%P-CONTENTS-SAFE-P LOCATIVE)
      (PRINT-CAREFULLY "printing"
	(format stream "~S ~S ~S: ~S"
		(nth (%p-cdr-code locative) q-cdr-codes)
		(nth (%p-data-type locative) q-data-types)
		(%p-pointer locative)
		(CONTENTS LOCATIVE)))
    (let ((type (q-data-types (%p-data-type locative))))
      (cond ((null type)
	     (format stream "#<~S UNKNOWN-DATA-TYPE-~S ~S>"
		     (nth (%p-cdr-code locative) q-cdr-codes)
		     (%p-data-type locative)
		     (%p-pointer locative)))
	    (t
	     (format stream "#<~S ~S ~S~@[ ~S~]>"
		     (nth (%p-cdr-code locative) q-cdr-codes)
		     type
		     (%p-pointer locative)
		     (cond ((memq (%p-data-type locative)
				  '(#.DTP-EXTERNAL-VALUE-CELL-POINTER
				    #.DTP-ONE-Q-FORWARD
				    #.DTP-HEADER-FORWARD
				    #.DTP-BODY-FORWARD))
			    (p-prin1-careful-1 (%make-pointer dtp-locative
							      (%p-pointer locative))
					       nil)))))))))

;;; Initialize the error handler at warm boot time.
(ADD-INITIALIZATION "ERROR-HANDLER-INITIALIZE" '(INITIALIZE) '(WARM))

;;; Waiting until the first error to do these things loses
;;; because they let other processes run, which could get errors and crash the machine.
(DEFUN INITIALIZE ()
  ;; So we don't lose before this variable is defined.
  (UNLESS (VARIABLE-BOUNDP TV:COLD-LOAD-STREAM-OWNS-KEYBOARD)
    (SETQ TV:COLD-LOAD-STREAM-OWNS-KEYBOARD NIL))
  (SETQ ERROR-HANDLER-RUNNING NIL)
  (SETQ ERRSET-STATUS NIL)		;Set to T if an errset exists and should be obeyed
  (ASSURE-TABLE-LOADED)			;Gets the right UCONS/UCADR TABLE file loaded.
  (ASSURE-TABLE-PROCESSED)		;Processes the contents of UCONS/UCADR TABLE.
  )

;;; This is the function that runs in the first level error handler
;;; It is called only at boot time.  From then on it just keeps coroutining.
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
      (SETF (SG-PROCESSING-ERROR-FLAG SG) 0) ;Re-enable error trapping in that SG
      (SETF (SG-INST-DISP SG) 0)	;Turn off single-step mode (for foothold)
      (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
      (UNLESS M
	;; If microcode error, compute the ETE.
	(SETQ ETE (AND (BOUNDP 'ERROR-TABLE)
		       (CDR (ASSQ (SG-TRAP-MICRO-PC SG) ERROR-TABLE))))
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
		   (STACK-GROUP-RESUME SG NIL))
		  ((NULL M)
		   ;; Microcode error.
		   (SETQ SG2 (OR (POP FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
				 (MAKE-STACK-GROUP
				   (FORMAT NIL "SECOND-LEVEL-ERROR-HANDLER-~D"
					   (INCF SECOND-LEVEL-ERROR-HANDLER-COUNT))
				   ':REGULAR-PDL-SIZE #o6000
				   ':SAFE 0)))
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
				   ':REGULAR-PDL-SIZE #o6000
				   ':SAFE 0)))
		   (STACK-GROUP-PRESET SG2 'SECOND-LEVEL-ERROR-HANDLER
				       SG M)
		   (FUNCALL SG2)))))))

(DEFVAR UCODE-ARG-VALUES)

(DEFVAR INHIBIT-SIGNAL-MICROCODE-ERROR-LOSSAGE T)

;;; This function is run, in a second-level error handler stack group,
;;; to create an error object for a microcode error
;;; and cause it to be signaled in the usual fashion.
;;; If the signal returns, meaning that a condition handler or the user has proceeded,
;;; we are responsible for restarting the microcode.
;;; Because of this function, the debugger itself does not need to distinguish
;;; between microcode and macrocode errors.
(DEFUN SIGNAL-MICROCODE-ERROR (SG ETE &OPTIONAL (IGNORE T) &AUX
			       ;; (IGNORE T) is never passed by callers.
			       ;; It forces off the fast arg option
			       ;; which persuades the compiler to bind
			       ;; INHIBIT-SCHEDULING-FLAG at function entry,
			       ;; preventing an abort at beginning of this function.
			       (INHIBIT-SCHEDULING-FLAG T)
			       (ERROR-HANDLER-RUNNING T)
			       (ERROR-HANDLER-REPRINT-ERROR NIL))
  (*CATCH 'QUIT
    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR)
			  "Abort from where a microcode error is being signaled.")
      (LET* ((DEFAULT-CONS-AREA ERROR-HANDLER-AREA)
	     (*TERMINAL-IO* (SYMEVAL-IN-STACK-GROUP '*TERMINAL-IO* SG))
	     (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
	     (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
	     (*QUERY-IO* SI:SYN-TERMINAL-IO)
	     SAVED-MICRO-PCS ERROR-OBJECT)
	(UNLESS ETE
	  (FERROR NIL "Microcode bug: no error-table entry for pc ~O." (SG-TRAP-MICRO-PC SG)))
	(SETQ ERROR-OBJECT (MAKE-UCODE-ERROR (CAR ETE) SG ETE))
	(WHEN (LDB-TEST %%LP-EXS-MICRO-STACK-SAVED
			(RP-EXIT-WORD (SG-REGULAR-PDL SG) (SG-AP SG)))
	  ;; Process the micro-stack of the active frame, if any
	  (DO ((I (SG-SPECIAL-PDL-POINTER SG) (1- I)))
	      (())
	    (LET ((PC (AREF (SG-SPECIAL-PDL SG) I)))
	      (PUSH PC SAVED-MICRO-PCS))
	    (OR (ZEROP (%P-LDB %%SPECPDL-BLOCK-START-FLAG
			       (ALOC (SG-SPECIAL-PDL SG) I)))
		(RETURN NIL)))
	  (SETQ SAVED-MICRO-PCS (NREVERSE SAVED-MICRO-PCS)))
	(UNLESS INHIBIT-SIGNAL-MICROCODE-ERROR-LOSSAGE
	  (CONDITION-CASE (ERROR)
	      (LET-GLOBALLY ((INHIBIT-SIGNAL-MICROCODE-ERROR-LOSSAGE T))
		(SETQ UCODE-ARG-VALUES
		      (MICROCODE-FUNCTION-ARG-VALUES SG SAVED-MICRO-PCS))
		(MAKE-MICROCODE-FUNCTION-FRAME SG UCODE-ARG-VALUES))
	    (NIL (SETQ UCODE-ARG-VALUES ERROR))))
	(IF (EQ 'PDL-OVERFLOW (CAR ETE))
	    ;; If pdl overflow, grow pdl without printing message
	    ;;  since streams are not set up to handle output.
	    ;; Also, don't signal another pdl-overflow error.
	    (SG-MAYBE-GROW-PDLS SG NIL NIL NIL T))
	(LET ((CONDITION-RESULT
		(SG-FUNCALL-NO-RESTART
		  SG 'SIGNAL-CONDITION
		  ERROR-OBJECT (SEND ERROR-OBJECT ':UCODE-PROCEED-TYPES)
		  NIL
		  (LIST* (SG-TRAP-MICRO-PC SG) ETE
			 (SG-AP SG) (SG-IPMARK SG)
			 SAVED-MICRO-PCS)
		  )))
	  (WHEN (CAR CONDITION-RESULT)
	    (LEXPR-SEND ERROR-OBJECT ':PROCEED-UCODE-WITH-ARGS (CAR CONDITION-RESULT)
			SG (CDR CONDITION-RESULT))
	    (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
	    (PROCEED-SG SG))
	  (FERROR NIL "Proceed-type ~S not handled~%by microcode error ~S."
		  (CAR CONDITION-RESULT) ERROR-OBJECT)))))
  ;; In case of abort within the above.
  (SG-ABORT SG))

(DEFUN HANDLE-BACKGROUND-ERROR (&OPTIONAL PROCESS)
  "Allow a process that has notified about an error to use this window.
More generally, it can use the current value of *TERMINAL-IO* for the debugger.
The current process waits until the debugger is exited in the other process.
You may specify the process to debug, or you will be offered each candidate."
  (UNLESS PROCESS (SETQ PROCESS (TV:CHOOSE-PROCESS-IN-ERROR)))
  (WHEN PROCESS
    (CHECK-ARG PROCESS AWAITING-BACKGROUND-ERROR-P
	       "a process waiting to use a background error window")
    (LET* ((STREAM-LOCATION (CELL-LOCATION-IN-STACK-GROUP (VARIABLE-LOCATION *TERMINAL-IO*)
							  (PROCESS-STACK-GROUP PROCESS)))
	   (DEALLOCATE-P (TYPEP (CONTENTS STREAM-LOCATION) 'TV:BACKGROUND-LISP-INTERACTOR))
	   (ORIGINAL-TERMINAL-IO (CONTENTS STREAM-LOCATION))
	   (SG (PROCESS-STACK-GROUP PROCESS)))
      (WHEN DEALLOCATE-P
	(DEALLOCATE-RESOURCE 'TV:BACKGROUND-LISP-INTERACTORS (CONTENTS STREAM-LOCATION)))
      (SETF (CONTENTS STREAM-LOCATION)
	    *TERMINAL-IO*)
      (UNWIND-PROTECT
	(PROGN
	  (SI:SET-PROCESS-WAIT PROCESS #'TRUE NIL)
	  (PROCESS-WAIT "Background error"
			#'(LAMBDA (SG)
			    (MEMQ SG FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST))
			SG))
	(SETF (CONTENTS STREAM-LOCATION)
	      (IF DEALLOCATE-P TV:DEFAULT-BACKGROUND-STREAM
		  ORIGINAL-TERMINAL-IO))))))

(DEFUN AWAITING-BACKGROUND-ERROR-P (PROCESS)
  (AND (TYPEP PROCESS 'SI:PROCESS)
       (SEND PROCESS ':ACTIVE-P)    ;Return NIL if process is arrested!
       (SI:PROCESS-IS-IN-ERROR-P PROCESS)))

(DEFUN EH (&OPTIONAL PROCESS)
  "Invoke the debugger to look at a process, window or stack group.
Supplying NIL means find a process which is waiting to be looked at for an error.
If the process is waiting to be looked at for an error,
the debugger is run in that process using our *TERMINAL-IO* for I//O.
Otherwise, if the process is active, it is forced to get an error
and the debugger uses our *TERMINAL-IO*.
Otherwise the debugger runs in the current process, not the process being examined,
and that process is arrested for the duration."
  (DEBUG PROCESS))
(DEFF DBG 'DEBUG)				;brand s name

(DEFUN DEBUG (&OPTIONAL PROCESS
	   &AUX PKG SG ARREST-REASON
	   CURRENT-FRAME INNERMOST-VISIBLE-FRAME ERROR-LOCUS-FRAME
	   INNERMOST-FRAME-IS-INTERESTING
	   EH-ERROR
	   (ERROR-HANDLER-RUNNING NIL))
  "Invoke the debugger to look at a process, window or stack group.
Supplying NIL means find a process which is waiting to be looked at for an error.
If the process is waiting to be looked at for an error,
the debugger is run in that process using our *TERMINAL-IO* for I//O.
Otherwise, if the process is active, it is forced to get an error
and the debugger uses our *TERMINAL-IO*.
Otherwise the debugger runs in the current process, not the process being examined,
and that process is arrested for the duration."
  ;; ERROR-HANDLER-RUNNING is NOT set.
  ;; The catch tag EXIT is used to return from EH.
  ;; If arg is a window or stream, extract process from it.
  (OR (NULL PROCESS) (TYPEP PROCESS ':STACK-GROUP) (TYPEP PROCESS 'SI:PROCESS)
      (SETQ PROCESS (SEND PROCESS ':PROCESS)))
  (IF (OR (NULL PROCESS)
	  (AWAITING-BACKGROUND-ERROR-P PROCESS))
      (HANDLE-BACKGROUND-ERROR PROCESS)
    ;; If arg is an active non-erring process, make it get an error.
    (IF (AND (TYPEP PROCESS 'SI:PROCESS)
	     (SEND PROCESS ':ACTIVE-P)
	     (NOT (SYMEVAL-IN-STACK-GROUP 'ERROR-HANDLER-RUNNING
					  (PROCESS-STACK-GROUP PROCESS))))
	(LET ((CELL (LIST NIL)))
	  (SEND PROCESS ':INTERRUPT 'INVOKE-DEBUGGER-FOR-EH *TERMINAL-IO* CELL)
	  (PROCESS-WAIT "Background error" 'CAR CELL))
      ;; If arg is process or was converted to one, stop it.
      (COND ((TYPEP PROCESS 'SI:PROCESS)
	     (SEND PROCESS ':ARREST-REASON CURRENT-PROCESS)
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
      (SETQ PKG (SYMEVAL-IN-STACK-GROUP '*PACKAGE* SG))
      (UNWIND-PROTECT
	(PROGN
	  (*CATCH 'QUIT
	    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit the debugger.")
	      (PKG-BIND (IF (EQ (TYPEP PKG) 'PACKAGE) PKG "USER")
		(PRINT-CAREFULLY "frame"
		  (FORMAT T "~&~S~%Backtrace: " SG)
		  (SHORT-BACKTRACE SG NIL 3)))))
	  (FORMAT T "~&Note: running in process ~A, not the one being debugged.
Type ~C to exit the debugger." (PROCESS-NAME CURRENT-PROCESS) #/RESUME)
	  (*CATCH 'EXIT (COMMAND-LOOP SG (SETQ EH-ERROR (SG-TRAP-TAG SG)))))
	(AND ARREST-REASON (SEND PROCESS ':REVOKE-ARREST-REASON ARREST-REASON))))))

(DEFPROP INVOKE-DEBUGGER-FOR-EH T :ERROR-REPORTER)
(DEFUN INVOKE-DEBUGGER-FOR-EH (*TERMINAL-IO* CELL)
  (LET ((*DEBUG-IO-OVERRIDE* SI:SYN-TERMINAL-IO)
	(CONDITION-PROCEED-TYPES '(:NO-ACTION)))
    (UNWIND-PROTECT
      (LET ((ERROR-DEPTH (1+ ERROR-DEPTH)))
	(INVOKE-DEBUGGER (MAKE-CONDITION 'BREAK)))
      (SETF (CAR CELL) T))))

(DEFVAR REAL-CURRENT-PROCESS :UNBOUND
  "What CURRENT-PROCESS was at entry to SECOND-LEVEL-ERROR-HANDLER,
which may have bound it to NIL.")
(DEFVAR ERRSET-INSIDE-ERROR NIL
  "Setting this to T allows debugging inside EH by disabling the error handler's own ERRSETs")
 
(DEFCONST INHERITED-VARIABLES
	  '((*PACKAGE* VALIDATE-PACKAGE)
	    (*READ-BASE* VALIDATE-BASE)
	    (*PRINT-BASE* VALIDATE-BASE)
	    (ERROR-DEPTH VALIDATE-ERROR-DEPTH)
	    (*READTABLE* VALIDATE-READTABLE))
  "This is a list of variables whose values are to be inherited from the stack group
in error by portions of the error handler inside an INHERITING-VARIABLES-FROM special
form.  Each element can be just a variable, or a list of the variable and a
validate function, which receives the value as its argument and returns either
the same value or a corrected value if it doesn't like that one.")


(DEFMACRO INHERITING-VARIABLES-FROM ((SG) &BODY BODY)
  `(PROG ((.L. INHERITED-VARIABLES) .VAR. .VAL.)
     LP (SETQ .VAR. (IF (ATOM (CAR .L.)) (CAR .L.) (CAAR .L.))
	      .VAL. (SYMEVAL-IN-STACK-GROUP .VAR. ,SG))
	(%BIND (VALUE-CELL-LOCATION .VAR.)
	       (IF (ATOM (CAR .L.)) .VAL. (FUNCALL (CADAR .L.) .VAL.)))
	(OR (ATOM (SETQ .L. (CDR .L.))) (GO LP))
	(RETURN (PROGN . ,BODY))))

(DEFUN VALIDATE-PACKAGE (P)
  (IF (TYPEP P 'PACKAGE) P SI:PKG-USER-PACKAGE))

(DEFUN VALIDATE-BASE (B)
  (IF (MEMQ B '(8 10.)) B 10.))		;These are the only reasonable bases for debugging

(DEFUN VALIDATE-ERROR-DEPTH (D)
  (IF (FIXNUMP D) D 0))
   
(DEFUN VALIDATE-READTABLE (R)
  (IF (AND (READTABLEP R) (EQ (GET *READTABLE* :SYNTAX) :COMMON-LISP))
      SI:COMMON-LISP-READTABLE SI:STANDARD-READTABLE))

;;; This is the function at the top level in each second level error handler sg.
(DEFUN SECOND-LEVEL-ERROR-HANDLER (SG EH-ERROR &OPTIONAL (IGNORE T)
				   ;; (IGNORE T) is never passed by callers.
				   ;; It forces off the fast arg option
				   ;; which persuades the compiler to bind
				   ;; INHIBIT-SCHEDULING-FLAG at function entry,
				   ;; preventing an abort at beginning of this function.
				   &AUX MSG
				   (INHIBIT-SCHEDULING-FLAG T)
				   (SI:PRINT-READABLY NIL)
				   (*PACKAGE* SI:PKG-USER-PACKAGE)
				   (DEFAULT-CONS-AREA ERROR-HANDLER-AREA)
				   SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD
				   (ERROR-HANDLER-RUNNING T)
				   (ERROR-HANDLER-REPRINT-ERROR T)
				   (*TERMINAL-IO* (OR (FOLLOW-SYN-STREAM-IN-STACK-GROUP
							'*DEBUG-IO-OVERRIDE* SG)
						      (FOLLOW-SYN-STREAM-IN-STACK-GROUP
							'*DEBUG-IO* SG)
						      (SYMEVAL-IN-STACK-GROUP '*TERMINAL-IO*
									      SG)))
				   (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
				   (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
				   (*QUERY-IO* SI:SYN-TERMINAL-IO)
				   (*DEBUG-IO* SI:SYN-TERMINAL-IO)
				   (*DEBUG-IO-OVERRIDE* NIL)
				   ;; In case we want to set CURRENT-PROCESS to nil.
				   (CURRENT-PROCESS CURRENT-PROCESS)
				   CURRENT-FRAME ERROR-LOCUS-FRAME
				   INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
  (UNLESS *TERMINAL-IO*
    (SETQ MSG "*TERMINAL-IO* is NIL"))
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
  (UNLESS (EQ SI:COLD-LOAD-STREAM *TERMINAL-IO*)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL))
  ;; If not running in the scheduler, give us a run reason in case we died after
  ;; becoming inactive, before getting back to the scheduler.
  (OR (NULL CURRENT-PROCESS)
      (SEND CURRENT-PROCESS ':RUN-REASON CURRENT-STACK-GROUP))
  (IF (VARIABLE-BOUNDP TV:COLD-LOAD-STREAM-OWNS-KEYBOARD)
      (SETQ SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD TV:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (LET-GLOBALLY ((TV:COLD-LOAD-STREAM-OWNS-KEYBOARD
		   (OR (EQ *TERMINAL-IO* TV:COLD-LOAD-STREAM)
		       TV:COLD-LOAD-STREAM-OWNS-KEYBOARD)))
    ;; Try to see if *TERMINAL-IO* is reasonable and if not fix it.
    (LET ((WO (ERRSET (SEND *TERMINAL-IO* ':WHICH-OPERATIONS) NIL))
	  (ERROR-HANDLER-REPRINT-ERROR NIL))
      (IF (NULL WO) (USE-COLD-LOAD-STREAM "*TERMINAL-IO* clobbered")
	(COND ((MEMQ ':NOTICE (CAR WO))
	       (DO-FOREVER
		 (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Continue entering the debugger.")
		   (LET (;; :NOTICE can change *TERMINAL-IO* of a background process
			 (OLD-TIO *TERMINAL-IO*)
			 ;; Send this message in non-erring stack
			 (WINDOW-BAD (SEND *TERMINAL-IO* ':NOTICE ':ERROR)))
		     (IF (EQ WINDOW-BAD 'TV:COLD-LOAD-STREAM)
			 (USE-COLD-LOAD-STREAM "window-system problems")
		       (AND (NEQ *TERMINAL-IO* OLD-TIO)
			    (NOT WINDOW-BAD)
			    (SG-FUNCALL SG #'SET '*TERMINAL-IO* *TERMINAL-IO*))))
		   (RETURN NIL)))))))
    ;; Turn off interrupts if switched to cold load stream.
    (IF (EQ SI:COLD-LOAD-STREAM *TERMINAL-IO*)
	(SETQ INHIBIT-SCHEDULING-FLAG T))
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
	    (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
	    (SEND EH-ERROR ':PRINT-ERROR-MESSAGE
		  SG NIL *STANDARD-OUTPUT*))
	  (PRINT-BRIEF-ERROR-BACKTRACE SG EH-ERROR)
	  (SEND EH-ERROR ':MAYBE-CLEAR-INPUT *STANDARD-INPUT*))))
    ;; Offer any special commands, such as wrong-package correction.
    ;; Then enter the command loop.
    (SEND EH-ERROR ':DEBUGGER-COMMAND-LOOP SG)))

(DEFUN FOLLOW-SYN-STREAM-IN-STACK-GROUP (SYM SG)
  "Evaluate SYM as an I//O stream in stack group SG, and trace synonyms.
That is, if SYM turns out to be TERMINAL-IO-SYN-STREAM,
we get the value of *TERMINAL-IO* in SG."
  (LOOP AS VAL = (SYMEVAL-IN-STACK-GROUP SYM SG) WITH (PTR) DO
    (COND ((AND (SYMBOLP VAL)
		(= (%P-DATA-TYPE (FUNCTION-CELL-LOCATION VAL))
		   DTP-EXTERNAL-VALUE-CELL-POINTER)
		(= (%POINTER-DIFFERENCE
		     (SETQ PTR (%P-CONTENTS-AS-LOCATIVE (FUNCTION-CELL-LOCATION VAL)))
		     (SETQ PTR (%FIND-STRUCTURE-HEADER PTR)))
		   1)
		(SYMBOLP PTR))
	   (SETQ SYM PTR))
	  (T (RETURN VAL)))))
