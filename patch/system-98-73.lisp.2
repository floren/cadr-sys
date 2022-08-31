;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 10/09/84 19:39:59 by Mly,
;;; Reason: Bucky mouse click timing screw (99.8)
;;; Written 10-Oct-84 02:22:22 by Mly,
;;; while running on Lisp Machine Nine from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.7, CADR 4.0, Experimental ZMail 54.1, MIT-Specific 23.0, Experimental Macsyma 1.0, microcode 320.

; From file MOUSE.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFUN MOUSE-INPUT (&OPTIONAL (WAIT-FLAG T))
  "Wait until mouse moves or a button goes up or down, then report what happened.
The values are the delta-x, the delta-y (amounts of mouse motion),
buttons-newly-pushed, buttons-newly-raised, and the relevant mouse X and Y
/(the current position normally, but if any buttons changed, the position then)."
  ;; Await a change in hardware status from what it was last time
  (COND (WAIT-FLAG
	 ;; This sleep makes it reasonable for the mouse process to be high priority.
	 ;; It insures that moving the mouse does not lock out lower priority
	 ;; processes for a long time.  This constant may want to be tweaked.  A
	 ;; value of 1 (1/60 of a second) does not noticably affect mouse response.
	 (PROCESS-SLEEP 1.)
	 (PROCESS-WAIT "MOUSE" #'(LAMBDA () (OR MOUSE-WAKEUP MOUSE-RECONSIDER)))))
  ;; Clear wakeup flag unless there are buffered mouse button transitions, since we
  ;; might not read all of them before calling MOUSE-INPUT again.
  (SETQ MOUSE-WAKEUP ( MOUSE-BUTTONS-BUFFER-IN-INDEX MOUSE-BUTTONS-BUFFER-OUT-INDEX))
  ;; Compute delta-X and delta-Y in screen coordinates
  (LET ((DELTA-X (- MOUSE-X MOUSE-LAST-X))
	(DELTA-Y (- MOUSE-Y MOUSE-LAST-Y))
	(GLITCH-X NIL) (GLITCH-Y NIL) NEW-BUTTONS CHANGED-BUTTONS)
    (INCF MOUSE-LAST-X DELTA-X)
    (INCF MOUSE-LAST-Y DELTA-Y)
    ;; Compute change in button status
    (MULTIPLE-VALUE-SETQ
      (NEW-BUTTONS MOUSE-LAST-BUTTONS-TIME MOUSE-LAST-BUTTONS-X MOUSE-LAST-BUTTONS-Y)
      (MOUSE-BUTTONS))
    (SETQ CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
	  MOUSE-LAST-BUTTONS NEW-BUTTONS)
    ;; Force blinker to stay within mouse-sheet.  If the mouse moves during this
    ;; computation, it will glitch back.  So we only SETQ the variables 
    ;; if the mouse position actually needs to be changed, rather than using
    ;; MAX and MIN which would be more readable.
    (IF (> 0 MOUSE-X)
	(SETQ GLITCH-X 0))
    (IF ( (SHEET-WIDTH MOUSE-SHEET) MOUSE-X)
	(SETQ GLITCH-X (1- (SHEET-WIDTH MOUSE-SHEET))))
    (IF (> 0 MOUSE-Y)
	(SETQ GLITCH-Y 0))
    (IF ( (SHEET-HEIGHT MOUSE-SHEET) MOUSE-Y)
	(SETQ GLITCH-Y (1- (SHEET-HEIGHT MOUSE-SHEET))))
    ;; If mouse blinker needs to be glitched, do so
    (IF (OR GLITCH-X GLITCH-Y)
	(WITHOUT-INTERRUPTS
	   (%OPEN-MOUSE-CURSOR)
	   (IF GLITCH-X
	       (SETQ MOUSE-LAST-X (SETQ MOUSE-X GLITCH-X)))
	   (IF GLITCH-Y
	       (SETQ MOUSE-LAST-Y (SETQ MOUSE-Y GLITCH-Y)))
	   (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
		 PREPARED-SHEET NIL)))
    (VALUES DELTA-X
	    DELTA-Y
	    (LOGAND NEW-BUTTONS CHANGED-BUTTONS)
	    (BOOLE 2 NEW-BUTTONS CHANGED-BUTTONS)	;Boole 2 is ANDCA
	    (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-X MOUSE-LAST-BUTTONS-X)
	    (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-Y MOUSE-LAST-BUTTONS-Y))))

))

; From file MOUSE.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFUN MOUSE-BUTTON-ENCODE (BD &AUX BUTTON MASK CH TIME
			    (NEW-BUTTONS MOUSE-LAST-BUTTONS)
			    (NEW-TIME MOUSE-LAST-BUTTONS-TIME))
  "Look at mouse button transitions and detect double clicks.
BD is a mask of buttons that went down on the initial transition;
it presumably came from MOUSE-INPUT.
The value is NIL if no button is pushed (BD is 0),
or #o2000 + 8 N + B, where B is the bit number in the button word,
and N is one less than the number of clicks."
  (WHEN ( (SETQ BUTTON (1- (HAULONG BD))) 0)	;Pick a button that was just pushed
    (SETQ MASK (LSH 1 BUTTON)
	  CH (MERGE-SHIFT-KEYS (MAKE-MOUSE-CHAR BUTTON 0))
	  TIME MOUSE-LAST-BUTTONS-TIME)
    ;; Each incrementing key that is held down
    ;; counts as an extra click in the number of clicks.
    (DOLIST (KEY *MOUSE-INCREMENTING-KEYSTATES*)
      (IF (KEY-STATE KEY)
	  (INCF CH 8)))
    (PROG1
      (LOOP					;Do forever (until guy's finger wears out)
	UNLESS MOUSE-DOUBLE-CLICK-TIME RETURN CH
	DOING
	  ;; Ignore any clicking during the bounce delay
	  (LOOP DOING (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
		UNTIL (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-BOUNCE-TIME)
		FINALLY (SETQ TIME NEW-TIME))
	  ;; Look for button to be lifted, or for double-click timeout
	  (LOOP WHILE (BIT-TEST MASK NEW-BUTTONS)
		DO (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
		WHEN (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-DOUBLE-CLICK-TIME)
		;; Timed-out with button still down
		DO (RETURN-FROM MOUSE-BUTTON-ENCODE CH)
		FINALLY (SETQ TIME NEW-TIME))
	  ;; Button was lifted, do another bounce delay
	  (LOOP DOING (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
		UNTIL (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-BOUNCE-TIME)
		FINALLY (SETQ TIME NEW-TIME))
	  ;; Now watch for button to be pushed again
	  (LOOP UNTIL (BIT-TEST MASK NEW-BUTTONS)
		DO (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
		WHEN (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-DOUBLE-CLICK-TIME)
		;; Timed-out with button still up
		DO (RETURN-FROM MOUSE-BUTTON-ENCODE CH)
		FINALLY (SETQ CH (+ CH 8)	;Count multiplicity of clicks
			      TIME NEW-TIME))
	  ;; Continue scanning (for triple click)
	  )
      (SETQ MOUSE-LAST-BUTTONS NEW-BUTTONS
	    MOUSE-LAST-BUTTONS-TIME NEW-TIME))))

))

; From file MOUSE.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-CLICK) (BUTTONS X Y)
; (SETQ BUTTONS (MERGE-SHIFT-KEYS BUTTONS))
  (COND ((AND (= BUTTONS #/MOUSE-1-1)
	      (NOT (SEND (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS)
			 :SELF-OR-SUBSTITUTE-SELECTED-P))
	      (GET-HANDLER-FOR SELF :SELECT))	;paper over a bug
	 (MOUSE-SELECT SELF)
	 T)
	(T
	 (OR (SEND SELF :SEND-IF-HANDLES :FORCE-KBD-INPUT
		   	`(:MOUSE-BUTTON ,BUTTONS ,SELF ,X ,Y))
	     (AND (= BUTTONS #/MOUSE-3-1)
		  (MOUSE-CALL-SYSTEM-MENU)
		  T)
	     (BEEP)))))				;click not handled

))

; From file MOUSE.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFMETHOD (KBD-MOUSE-BUTTONS-MIXIN :MOUSE-CLICK) (BUTTON X Y)
  (DECLARE (IGNORE X Y))
  (AND (= BUTTON #/MOUSE-1-1) (NEQ SELF SELECTED-WINDOW)
       (MOUSE-SELECT SELF))
  (SEND SELF :FORCE-KBD-INPUT BUTTON)		;(MERGE-SHIFT-KEYS BUTTON)
  T)

))

; From file TSCROL.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TSCROL  "

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW :MOUSE-CLICK) (BUTTON X Y &AUX ITEM TYPE)
  (MULTIPLE-VALUE-SETQ (ITEM TYPE) (SEND SELF ':MOUSE-SENSITIVE-ITEM X Y))
  (WHEN TYPE
    (SEND SELF :FORCE-KBD-INPUT (LIST TYPE ITEM SELF BUTTON))
    T))

))

; From file SCREEN.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZWEI :MOUSE-CLICK) (BUTTON X Y &AUX HANDLED-P)
; (SETQ BUTTON (TV:MERGE-SHIFT-KEYS BUTTON))
  (COND ((NOT (SEND (SEND SELF ':TOP-OF-EDITOR-HIERARCHY)
		    ':SELF-OR-SUBSTITUTE-SELECTED-P))
	 ;; This frame or whatever is not selected.
	 (TV:MOUSE-SELECT SELF))
	((AND (NOT (EDITOR-WINDOW-SELECTED-P SELF))
	      (OR (= BUTTON #/MOUSE-1-1)
		  *MOUSE-CLICK-ALWAYS-SELECTS*))
	 ;; Frame selected but this editor window is not.  Just switch to it.
	 (COMMAND-BUFFER-PUSH `(SELECT-WINDOW ,SELF))
	 (IF *MOUSE-CLICK-ALWAYS-SELECTS*
	     ;; And maybe also do the command for the mouse button.
	     (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y)))
	 (SETQ HANDLED-P T))
	(T
	 (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y))))
  T)

))
