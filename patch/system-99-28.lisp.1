;;; -*- Mode:Lisp; Readtable:ZL; Package:SI; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.28
;;; Written 17-May-85 00:50:29 by ELISHA,
;;; while running on Lisp Machine Nine from band 3
;;; with System 99.24, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, Experimental Macsyma 6.0, microcode 320, GC@2.


(export (intern "PROCESS-RUN-WHOSTATE" 'si) 'si)
(export (intern "PROCESS-WAIT-WHOSTATE" 'si) 'si)

; From file OZ:KANSAS:<L.SYS2>PROCES.LISP.158 at 13-Feb-85 09:46:38
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(defmacro process-wait-whostate (process)
  "The /"Whostate/" string for the wholine, etc to be displayed when the process is
waiting to run. NIL when the process is running."
  (if (eq process 'self)
      'whostate
    `(process-whostate ,process)))
(compiler:make-obsolete process-whostate "this function is now SI:PROCESS-WAIT-WHOSTATE")

(defmacro process-run-whostate (process)
  "The /"Whostate/" string to be displayed when the process is running"
  (if (eq process 'self)
      'spare-slot-1
    `(process-spare-slot-1 ,process)))

(defmethod (process :run-whostate) () (process-run-whostate self))
(defmethod (process :wait-whostate) () (process-wait-whostate self))

(DEFMETHOD (PROCESS :AFTER :INIT) (ignore)
  (or (variable-boundp whostate)
      (setf (process-run-whostate self) "Run"))
  (WITHOUT-INTERRUPTS
    (PROCESS-ALL-PROCESSES SELF T)))

(DEFMETHOD (PROCESS :RESET) (&OPTIONAL UNWIND-OPTION KILL &AUX RESTART-FUN)
  "UNWIND-OPTION: T, never unwind; :UNLESS-CURRENT or NIL, unwinds the stack unless
the stack group is either in the current process or is the current stack group;
:ALWAYS, always unwinds the stack.  KILL is T to kill the process after optionally
unwinding it."
  (WITHOUT-INTERRUPTS
    (SETQ RESTART-FUN
	  (COND (KILL #'PROCESS-KILL-TOP-LEVEL)
		((EQ STACK-GROUP INITIAL-STACK-GROUP) #'PROCESS-TOP-LEVEL)
		(T #'(LAMBDA (&REST IGNORE)	;Unwind and switch SG's
		       (EH::UNWIND-SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS)
				      #'PROCESS-TOP-LEVEL NIL NIL)))))
    ;; Wake up
    (SETF (process-wait-whostate self) (IF KILL "Killed" "Reset")
	  (process-run-whostate self) "Run")
    (SET-PROCESS-WAIT SELF #'TRUE NIL)
    (COND ((EQ SELF CURRENT-PROCESS)
	   (IF (EQ UNWIND-OPTION ':ALWAYS)
	       (*UNWIND-STACK T NIL NIL RESTART-FUN)
	     (WHEN KILL
	       (PROCESS-DISABLE CURRENT-PROCESS)
	       (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL))))
	  (T
	   ;; Note -- the following code is not logically necessary.  However,
	   ;; it is here to make the cold-load come up when EH::UNWIND-SG
	   ;; is not loaded yet.  We avoid unwinding the stack-group if it
	   ;; has just been created.
	   (AND (SG-NEVER-RUN-P STACK-GROUP)
		(SETQ UNWIND-OPTION T))
	   ;; Cause the process, when next scheduled, to unwind itself and
	   ;; call its initial function in the right stack group.
	   (COND ((EQ %CURRENT-STACK-GROUP STACK-GROUP)
		  ;; Not current process, but our stack group is the one running.
		  ;; Respect NOUNWIND
		  (IF (EQ UNWIND-OPTION ':ALWAYS)
		      (*UNWIND-STACK T NIL NIL RESTART-FUN)
		    (WHEN KILL
		      (PROCESS-DISABLE CURRENT-PROCESS)
		      (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL))))
		 ((NEQ UNWIND-OPTION 'T)
		  (LET ((EH::ALLOW-PDL-GROW-MESSAGE NIL))
		    (EH::UNWIND-SG STACK-GROUP RESTART-FUN NIL T)))
		 (T
		  (STACK-GROUP-PRESET STACK-GROUP RESTART-FUN)))))))

))

; From file OZ:KANSAS:<L.SYS2>PROCES.LISP.158 at 13-Feb-85 09:46:42
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(DEFMETHOD (SIMPLE-PROCESS :RESET) (&OPTIONAL UNWIND-OPTION KILL)
  (DECLARE (IGNORE UNWIND-OPTION))		;ignored -- there is no stack group
  (WITHOUT-INTERRUPTS
    (SETQ STACK-GROUP (CAR INITIAL-FORM))	;Reset to initial function
    (SETF (process-wait-whostate self) (IF KILL "Killed" "Reset")
	  (process-run-whostate self) "Run")
    (SET-PROCESS-WAIT SELF #'TRUE NIL)		;and un-block
    (WHEN KILL
      (PROCESS-DISABLE SELF)			;Killing: remove from scheduler lists
      (PROCESS-ALL-PROCESSES SELF NIL))))

))

; From file OZ:KANSAS:<L.SYS2>PROCES.LISP.158 at 13-Feb-85 09:48:31
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(DEFUN PROCESS-SCHEDULER-FOR-CADR ()
  (WITHOUT-INTERRUPTS				;No seq breaks in the scheduler
    (DO ((REMAINING-QUANTUM 0 0)
	 (NEXT-PROCESS NIL NIL)
	 (OLD-CURRENT-PROCESS)
	 (THIS-TIME (TIME) (TIME))
	 (LAST-TIME (TIME) THIS-TIME)
	 (DELTA-TIME)
	 (NEXT-WHO-TIME 0))
	(())
      (SETQ DELTA-TIME (TIME-DIFFERENCE THIS-TIME LAST-TIME)
	    OLD-CURRENT-PROCESS CURRENT-PROCESS)
      (AND CURRENT-PROCESS
	   (SETF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS)
		 (SETQ REMAINING-QUANTUM
		       (- (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) DELTA-TIME))))
      (WHEN (> DELTA-TIME 0)
	;; Run clock queue no more often than every 1/60 second.
	(DOLIST (E CLOCK-FUNCTION-LIST)
	  (CATCH-ERROR (FUNCALL E DELTA-TIME) NIL))
	(WHEN (MINUSP (SETQ NEXT-WHO-TIME (- NEXT-WHO-TIME DELTA-TIME)))
	  (AND (FBOUNDP 'TV::WHO-LINE-UPDATE)
	       (CATCH-ERROR (TV::WHO-LINE-UPDATE) NIL))
	  (SETQ NEXT-WHO-TIME 60.)))
      (BLOCK FOUND-PROCESS
	(DO ((PROCS ACTIVE-PROCESSES)
	     (THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED)
	     (FIRST-OF-THIS-PRIORITY)
	     (CURRENT-PRIORITY))
	    ((NULL (FIRST (CAR PROCS))))
	  ;; Loop over all process of the current priority
	  (SETQ CURRENT-PRIORITY (FOURTH (CAR PROCS))
		FIRST-OF-THIS-PRIORITY PROCS)
	  ;; If we find a process to run return from FOUND-PROCESS.
	  ;; If we have looked at all processes of this priority, return from RAN-OUT.
	  ;; This hair is equivalent to one loop with a catch around just the APPLY,
	  ;; but it avoids entering and exiting the catch so often.
	  (BLOCK RAN-OUT
	    (DO (APE PRI PROC)
		(())
	      (CATCH 'PROCESS-WAIT-IN-SCHEDULER
		(DO-FOREVER
		  (SETQ APE (CAR PROCS))
		  (AND (OR (NULL (SETQ PROC (FIRST APE)))
			   ( (SETQ PRI (FOURTH APE)) CURRENT-PRIORITY))
		       ;; Hit next priority level, or ran out of processes
		       (RETURN-FROM RAN-OUT))
		  (AND (COND ((LET ((CURRENT-PROCESS PROC))
				(APPLY (SECOND APE) (THIRD APE)))
			      (SETQ THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED PROC)
			      T))
		       (PLUSP (PROCESS-QUANTUM-REMAINING PROC))
		       ;; It is runnable, and it has time remaining
		       (RETURN-FROM FOUND-PROCESS (SETQ NEXT-PROCESS PROC)))
		  (POP PROCS)))
	      ;; Get here only on throw.
	      (POP PROCS)))
	  ;; Ran out of all processes at current priority level.  Reset their quantums.
	  (DO ((PS FIRST-OF-THIS-PRIORITY (CDR PS)))
	      ((EQ PS PROCS))
	    (SETF (PROCESS-QUANTUM-REMAINING (FIRST (CAR PS)))
		  (PROCESS-QUANTUM (FIRST (CAR PS)))))
	  ;; If a process would have run at this priority level, but couldn't becase
	  (WHEN THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED
	    (RETURN-FROM FOUND-PROCESS
	      (SETQ NEXT-PROCESS THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED)))))
      (WHEN (NULL NEXT-PROCESS)
	;; No process to run, do idle time stuff
	(OR INHIBIT-IDLE-SCAVENGING-FLAG
	    %GC-FLIP-READY
	    TEMPORARILY-NO-IDLE-SCAVENGING
	    (%GC-SCAVENGE GC-IDLE-SCAVENGE-QUANTUM)))
      (IF (NULL NEXT-PROCESS)
	  (SETQ CURRENT-PROCESS NIL)
	(SETF (PROCESS-WAIT-WHOSTATE NEXT-PROCESS) NIL)
	(SET-PROCESS-WAIT NEXT-PROCESS #'TRUE NIL)
	(SETF (RUN-LIGHT-FOR-CADR) T)
	(LET ((SG (PROCESS-STACK-GROUP (SETQ CURRENT-PROCESS NEXT-PROCESS)))
	      (START-TIME (FIXNUM-MICROSECOND-TIME-FOR-SCHEDULER-FOR-CADR))
	      (START-DISK-TIME (FIXNUM-READ-METER-FOR-SCHEDULER %DISK-WAIT-TIME))
	      (START-PAGE-FAULTS
		(FIXNUM-READ-METER-FOR-SCHEDULER %COUNT-DISK-PAGE-READ-OPERATIONS)))
	  (IF (TYPEP SG 'STACK-GROUP)
	      (STACK-GROUP-RESUME SG NIL)
	    (APPLY SG (CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS))))
	  (SETF (RUN-LIGHT-FOR-CADR) NIL)
	  (LET ((P CURRENT-PROCESS)
		(END-TIME (FIXNUM-MICROSECOND-TIME-FOR-SCHEDULER-FOR-CADR))
		(END-DISK-TIME (FIXNUM-READ-METER-FOR-SCHEDULER %DISK-WAIT-TIME))
		(END-PAGE-FAULTS
		  (FIXNUM-READ-METER-FOR-SCHEDULER %COUNT-DISK-PAGE-READ-OPERATIONS))
		TEM TIME-USED)
	    (INCREMENT-PROCESS-TIME-METER
	      (PROCESS-TOTAL-RUN-TIME P)
	      (SETQ TIME-USED (TIME-DIFFERENCE END-TIME START-TIME)))
	    (INCREMENT-PROCESS-TIME-METER
	      (PROCESS-DISK-WAIT-TIME P)
	      (TIME-DIFFERENCE END-DISK-TIME START-DISK-TIME))
	    (INCF (PROCESS-PAGE-FAULT-COUNT P) (- END-PAGE-FAULTS START-PAGE-FAULTS))
	    (SETF (PROCESS-PERCENT-UTILIZATION P)
		  (WITHOUT-FLOATING-UNDERFLOW-TRAPS
		    (+ (IF (SETQ TEM (PROCESS-LAST-TIME-RUN P))
			   (FIX (* (PROCESS-PERCENT-UTILIZATION P)
				   (^ PERCENT-UTILIZATION-DISCOUNT-FACTOR
				      (TIME-DIFFERENCE THIS-TIME TEM))))
			 0)
		       ;; Don't use ROUND -- loses before SYS: SYS2; RAT is loaded.
		       (TRUNCATE (+ TIME-USED 500.) 1000.))))
	    ;; Above "^" typically takes a bit under a millisecond which is not bad
	    ;; compared to calling TIME a few times, so it's probably not worth
	    ;; putting in a big table of pre-computed values.
	    (SETF (PROCESS-LAST-TIME-RUN P) THIS-TIME)
	    ;; Remember stack group of process last run
	    (OR (PROCESS-SIMPLE-P P)
		(SETF (PROCESS-STACK-GROUP P)
		      %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)))))
      ;; In case we took a page fault, the microcode will turn the run light on.
      ;; So turn it back off...this is a kind of kludge, but...
      (SETF (RUN-LIGHT-FOR-CADR) NIL))))

))

; From file OZ:KANSAS:<L.SYS2>PROCES.LISP.158 at 13-Feb-85 09:48:39
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(DEFUN PROCESS-SCHEDULER-FOR-LAMBDA ()
  (WITHOUT-INTERRUPTS				;No seq breaks in the scheduler
    (DO ((REMAINING-QUANTUM 0 0)
	 (NEXT-PROCESS NIL NIL)
	 (OLD-CURRENT-PROCESS)
	 (THIS-TIME (TIME) (TIME))
	 (LAST-TIME (TIME) THIS-TIME)
	 (DELTA-TIME)
	 (NEXT-WHO-TIME 0))
	(())
      (SETQ DELTA-TIME (TIME-DIFFERENCE THIS-TIME LAST-TIME)
	    OLD-CURRENT-PROCESS CURRENT-PROCESS)
      (WHEN CURRENT-PROCESS
	(SETF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS)
	      (SETQ REMAINING-QUANTUM
		    (- (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) DELTA-TIME))))
      (WHEN (> DELTA-TIME 0)
	;; Run clock queue no more often than every 1/60 second.
	(DOLIST (E CLOCK-FUNCTION-LIST)
	  (CATCH-ERROR (FUNCALL E DELTA-TIME) NIL))
	(WHEN (MINUSP (SETQ NEXT-WHO-TIME (- NEXT-WHO-TIME DELTA-TIME)))
	  (AND (FBOUNDP 'TV::WHO-LINE-UPDATE)
	       (CATCH-ERROR (TV::WHO-LINE-UPDATE) NIL))
	  (SETQ NEXT-WHO-TIME 60.)))
      (BLOCK FOUND-PROCESS
	(DO ((PROCS ACTIVE-PROCESSES)
	     (THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED)
	     (FIRST-OF-THIS-PRIORITY)
	     (CURRENT-PRIORITY))
	    ((NULL (FIRST (CAR PROCS))))
	  ;; Loop over all process of the current priority
	  (SETQ CURRENT-PRIORITY (FOURTH (CAR PROCS))
		FIRST-OF-THIS-PRIORITY PROCS)
	  ;; If we find a process to run return from FOUND-PROCESS.
	  ;; If we have looked at all processes of this priority, return from RAN-OUT.
	  ;; This hair is equivalent to one loop with a catch around just the APPLY,
	  ;; but it avoids entering and exiting the catch so often.
	  (BLOCK RAN-OUT
	    (DO (APE PRI PROC)
		(())
	      (CATCH 'PROCESS-WAIT-IN-SCHEDULER
		(DO-FOREVER
		  (SETQ APE (CAR PROCS))
		  (AND (OR (NULL (SETQ PROC (FIRST APE)))
			   (NOT (= (SETQ PRI (FOURTH APE)) CURRENT-PRIORITY)))
		       ;; Hit next priority level, or ran out of processes
		       (RETURN-FROM RAN-OUT))
		  (AND (COND ((LET ((CURRENT-PROCESS PROC))
				(APPLY (SECOND APE) (THIRD APE)))
			      (SETQ THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED PROC)
			      T))
		       (PLUSP (PROCESS-QUANTUM-REMAINING PROC))
		       ;; It is runnable, and it has time remaining
		       (RETURN-FROM FOUND-PROCESS (SETQ NEXT-PROCESS PROC)))
		  (POP PROCS)))
	    ;; Get here only on throw.
	    (POP PROCS)))
	  ;; Ran out of all processes at current priority level.  Reset their quantums.
	  (DO ((PS FIRST-OF-THIS-PRIORITY (CDR PS)))
	      ((EQ PS PROCS))
	    (SETF (PROCESS-QUANTUM-REMAINING (FIRST (CAR PS)))
		  (PROCESS-QUANTUM (FIRST (CAR PS)))))
	  ;; If a process would have run at this priority level, but couldn't becase
	  (AND THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED
	       (RETURN-FROM FOUND-PROCESS
		 (SETQ NEXT-PROCESS THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED)))))
      (WHEN (NULL NEXT-PROCESS)
	;; No process to run, do idle time stuff
	(OR INHIBIT-IDLE-SCAVENGING-FLAG
	    (%GC-SCAVENGE GC-IDLE-SCAVENGE-QUANTUM)))
      (IF (NULL NEXT-PROCESS)
	  (SETQ CURRENT-PROCESS NIL)
	(SETF (PROCESS-WAIT-WHOSTATE NEXT-PROCESS) NIL)
	(SET-PROCESS-WAIT NEXT-PROCESS #'TRUE NIL)
	(SETF (RUN-LIGHT-FOR-LAMBDA) T)
	(LET ((SG (PROCESS-STACK-GROUP (SETQ CURRENT-PROCESS NEXT-PROCESS)))
	      (START-TIME (FIXNUM-MICROSECOND-TIME-FOR-SCHEDULER-FOR-LAMBDA))
	      (START-DISK-TIME (FIXNUM-READ-METER-FOR-SCHEDULER %DISK-WAIT-TIME))
	      (START-PAGE-FAULTS
		(FIXNUM-READ-METER-FOR-SCHEDULER %COUNT-DISK-PAGE-READ-OPERATIONS)))
	  (IF (TYPEP SG 'STACK-GROUP)
	      (STACK-GROUP-RESUME SG NIL)
	    (APPLY SG (CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS))))
	  (SETF (RUN-LIGHT-FOR-LAMBDA) NIL)
	  (LET ((P CURRENT-PROCESS)
		(END-TIME (FIXNUM-MICROSECOND-TIME-FOR-SCHEDULER-FOR-LAMBDA))
		(END-DISK-TIME (FIXNUM-READ-METER-FOR-SCHEDULER %DISK-WAIT-TIME))
		(END-PAGE-FAULTS
		  (FIXNUM-READ-METER-FOR-SCHEDULER %COUNT-DISK-PAGE-READ-OPERATIONS))
		TEM TIME-USED)
	    (INCREMENT-PROCESS-TIME-METER
	      (PROCESS-TOTAL-RUN-TIME P)
	      (SETQ TIME-USED (TIME-DIFFERENCE END-TIME START-TIME)))
	    (INCREMENT-PROCESS-TIME-METER
	      (PROCESS-DISK-WAIT-TIME P)
	      (TIME-DIFFERENCE END-DISK-TIME START-DISK-TIME))
	    (INCF (PROCESS-PAGE-FAULT-COUNT P) (- END-PAGE-FAULTS START-PAGE-FAULTS))
	    (SETF (PROCESS-PERCENT-UTILIZATION P)
		  (WITHOUT-FLOATING-UNDERFLOW-TRAPS
		    (+ (IF (SETQ TEM (PROCESS-LAST-TIME-RUN P))
			   (FIX (* (PROCESS-PERCENT-UTILIZATION P)
				   (^ PERCENT-UTILIZATION-DISCOUNT-FACTOR
				      (TIME-DIFFERENCE THIS-TIME TEM))))
			 0)
		       ;; Don't use ROUND -- loses before SYS: SYS2; RAT is loaded.
		       (TRUNCATE (+ TIME-USED 500.) 1000.))))
	    ;; Above "^" typically takes a bit under a millisecond which is not bad
	    ;; compared to calling TIME a few times, so it's probably not worth
	    ;; putting in a big table of pre-computed values.
	    (SETF (PROCESS-LAST-TIME-RUN P) THIS-TIME)
	    ;; Remember stack group of process last run
	    (OR (PROCESS-SIMPLE-P P)
		(SETF (PROCESS-STACK-GROUP P)
		      %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)))))
      ;; In case we took a page fault, the microcode will turn the run light on.
      ;; So turn it back off...this is a kind of kludge, but...
      (SETF (RUN-LIGHT-FOR-LAMBDA) NIL))))

))

;; From file OZ:KANSAS:<L.SYS2>PRODEF.LISP.48 at 13-Feb-85 00:02:09
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; PRODEF  "

;(DEFFLAVOR PROCESS
; (NAME				;Print name
;  STACK-GROUP			;Stack group currently executing on behalf of this process
;  (WAIT-FUNCTION 'FLUSHED-PROCESS) ;Predicate to determine if process is runnable
;  (WAIT-ARGUMENT-LIST NIL)	;Arguments passed to above (use an arg to avoid a closure)
;				; This will often be a rest argument in somebody's stack,
;				; but it will always be used in a safe manner.
;  (WAIT-WHOSTATE "Just Created");The "whostate" string for the who line for use when the
;				; process is waiting to run. Set to NIL by the scheduler
;				; whenever the process runs.
;; this ivar not patched in 99
;;  (RUN-WHOSTATE "Run")		;The whostate string to be used when the process is running.
;  INITIAL-STACK-GROUP		;The stack group which PROCESS-RESET (q.v.) will reset to.
;  INITIAL-FORM			;Form to preset the initial stack group to when proc is reset.
;				; Really cons of function and evaluated args.
;  (RUN-REASONS NIL)		;List of run reasons for this process.
;  (ARREST-REASONS NIL)		;List of arrest reasons for this process.
;  (QUANTUM DEFAULT-QUANTUM)	;Number of ticks process should run at most before
;				; running another process.
;  (QUANTUM-REMAINING 0)		;Amount of time remaining for this process to run.
;  (PRIORITY 0)			;Absolute priority of this process.  The larger the number,
;				; the more this process wants to run.  It will never be
;				; run for more than its quantum, though.
;  (WARM-BOOT-ACTION		;Thing to do to this process if it is active when the
;   'PROCESS-WARM-BOOT-DELAYED-RESTART)	; machine is warm-booted.
;				;  NIL means the default action
;				; (flush it).  If non-NIL, gets funcalled with the process
;				; as its argument.
;	;The default is to reset it after initializations have been completed
;	;[I'm not sure why it's this rather than to leave it alone.]
;  (SIMPLE-P NIL)		;T if the process is simple (has no stack group)
;  (LAST-TIME-RUN NIL)		;(TIME) process last woke up, NIL if never
;  (TOTAL-RUN-TIME-LOW 0)	;Low bits of total run time in microseconds
;  (TOTAL-RUN-TIME-HIGH 0)	;High bits of same
;  (DISK-WAIT-TIME-LOW 0)	;Low bits of disk wait time in microseconds
;  (DISK-WAIT-TIME-HIGH 0)	;High bits of same
;  (PAGE-FAULT-COUNT 0)		;Number of disk page waits
;  (PERCENT-UTILIZATION 0)	;Exponential average of total run time
;  CLOSURE
;; this ivar not patched in 99
;  (RUN-WHOSTATE "Run")		;The whostate string to be used when the process is running.
;;  SPARE-SLOT-1			;Allow experimentation without making new cold load
;  SPARE-SLOT-2			;..
;  )
;  ()
;  :ORDERED-INSTANCE-VARIABLES
;  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
;  (:GETTABLE-INSTANCE-VARIABLES NAME STACK-GROUP WAIT-FUNCTION WAIT-ARGUMENT-LIST
;				INITIAL-STACK-GROUP INITIAL-FORM
;				RUN-REASONS ARREST-REASONS QUANTUM QUANTUM-REMAINING
;				PRIORITY WARM-BOOT-ACTION SIMPLE-P
;				LAST-TIME-RUN PAGE-FAULT-COUNT)
;  (:SETTABLE-INSTANCE-VARIABLES WARM-BOOT-ACTION WAIT-WHOSTATE RUN-WHOSTATE)
;  (:INITABLE-INSTANCE-VARIABLES NAME STACK-GROUP WAIT-FUNCTION WAIT-ARGUMENT-LIST
;				INITIAL-STACK-GROUP INITIAL-FORM
;				RUN-REASONS ARREST-REASONS QUANTUM
;				PRIORITY WARM-BOOT-ACTION SIMPLE-P)
;  (:INIT-KEYWORDS :FLAVOR
;		  ;; Keywords for stack group
;		  :SG-AREA :REGULAR-PDL-AREA :SPECIAL-PDL-AREA :REGULAR-PDL-SIZE
;		  :SPECIAL-PDL-SIZE :CAR-SYM-MODE :CAR-NUM-MODE :CDR-SYM-MODE :CDR-NUM-MODE
;		  :SWAP-SV-ON-CALL-OUT :SWAP-SV-OF-SG-THAT-CALLS-ME :TRAP-ENABLE :SAFE
;		  :CLOSURE-VARIABLES
;		  :WHOSTATE)			;for compatibility
;  (:DEFAULT-INIT-PLIST :CLOSURE-VARIABLES *DEFAULT-PROCESS-CLOSURE-VARIABLES*))
;
;
;))

