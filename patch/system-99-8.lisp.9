;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.8
;;; Reason: READ, READ-FOR-TOP-LEVEL, READ-OR-END all take kludgey third
;;;   rubout-handler-options argument.  (It is preferable to use
;;;   the WITH-INPUT-EDITING macro directly than to supply this arg.)
;;; Merge bucky bits into mouser clicks earlier to avoid timing screw.
;;; In Split Screen system menu, plain window is just that -- TV:WINDOW
;;; Patchable systems cache the the truename of their patch-directory,
;;;   and do not bother to reread it if it hasn't changed.
;;; Written 10-Oct-84 02:22:22 by Mly,
;;; while running on Lisp Machine Nine from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.7, CADR 4.0, Experimental ZMail 54.1, MIT-Specific 23.0, Experimental Macsyma 1.0, microcode 320.



; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-READ-FIXNUM (STREAM STRING &AUX NUM LEN I)
  STREAM ;ignored, doesn't do any additional reading
  (UNLESS (AND (FIXNUMP *READ-BASE*)	;If it were a flonum, confusing things would happen
	       ( 2 *READ-BASE* 36.))
    (CERROR :NO-ACTION NIL NIL "~S bad value for *READ-BASE*.  Has been reset to 10."
	    (PROG1 *READ-BASE* (SETQ *READ-BASE* 10.))))
  (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING))
  (MULTIPLE-VALUE (NUM I)
    (XR-READ-FIXNUM-INTERNAL STRING 0 LEN))
  (VALUES
    (IF (= I LEN)
	NUM
      (LET ((NUM2 (XR-READ-FIXNUM-INTERNAL STRING (1+ I) LEN)))
	(IF (= (AREF STRING I) #/_)
	    (ASH NUM NUM2)
	  (* NUM (^ *READ-BASE* NUM2)))))
    'FIXNUM))

))


; From file QIO.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun decode-kludgey-mucklisp-read-args (arglist &optional (number-of-extra-args-allowed 0))
  (declare (values stream eof-error-p eof-value other-args))
  (cond
    ((null arglist)
     (values *standard-input* t))
    ((null (cdr arglist))
     (let ((arg1 (car arglist)))
       (if (or (eq arg1 nil) (eq arg1 t) (io-stream-p arg1))
	   ;; The arg is a plausible stream.
	   (values (decode-read-arg arg1) t)
	 ;; It is not a stream and must be an EOF option.
	 (values *standard-input* nil arg1))))
    ((null (cddr arglist))
     (let ((arg1 (car arglist)) (arg2 (cadr arglist)))
       (cond ((or (eq arg1 nil) (eq arg1 t) (io-stream-p arg1))
	      (values (decode-read-arg arg1) nil arg2))
	     ((or (eq arg2 nil) (eq arg2 t) (io-stream-p arg2))
	      (values (decode-read-arg arg2) nil arg1))
	     (t (values arg1 nil arg2)))))
    ((not (null (nthcdr (+ number-of-extra-args-allowed 2) arglist)))
     (ferror nil "Too many arguments were given to one of the READ-like functions: ~S"
	     arglist))
    ;; If giving hairy options, we assume she knows not to write stream args in wrong order.
    (t
     (values (decode-read-arg (car arglist)) nil (cadr arglist) (cddr arglist)))))

))

; From file QIO.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun decode-read-args (arglist &optional (number-of-extra-args-allowed 0))
  (declare (values stream eof-option other-args))
  (multiple-value-bind (stream eof-error-p eof-value other-args)
      (decode-kludgey-mucklisp-read-args arglist number-of-extra-args-allowed)
    (if eof-error-p
	(values stream 'no-eof-option other-args)
      (values stream eof-value other-args))))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN READ-OR-END (&REST READ-ARGS &AUX CH TEM)
  "Like GLOBAL:READ, except that if the first non-blank character that the user types
is END (the interactive input activation character), we immediately return two values:
NIL and :END.  Otherwise, read and return an s-expression from STREAM.
EOF-OPTION has the same meaning as it does for GLOABL:READ."
  (DECLARE (ARGLIST STREAM EOF-OPTION RUBOUT-HANDLER-OPTIONS))
  (MULTIPLE-VALUE-BIND (STREAM EOF-ERROR-P EOF-VALUE RH-OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (SETQ TEM (ASSQ :ACTIVATION RH-OPTIONS))
    (WITH-INPUT-EDITING (STREAM (IF TEM RH-OPTIONS (CONS '(:ACTIVATION = #/END) RH-OPTIONS)))
	(SETQ TEM (IF (SEND STREAM :OPERATION-HANDLED-P :ANY-TYI) :ANY-TYI :TYI))
	;; can't use PEEK-CHAR as that wouldn't get blips...
	(DO-FOREVER
	  (SETQ CH (SEND STREAM TEM EOF-ERROR-P))
	  (COND ((EQ (CAR-SAFE CH) :ACTIVATION) (RETURN (VALUES NIL :END)))
		;; should use the same readtable-based check that peek-char uses, but wtf
		((MEMQ CH '(#/SPACE #/TAB #/RETURN)))	;do nothing
		(T (SEND STREAM :UNTYI CH)
		   (RETURN (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE NIL NIL T))))))))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN READ-FOR-TOP-LEVEL (&REST READ-ARGS)
  "Similar to READ, but ignores stray closeparens and ignores end of file.
Interactive command loops such as the lisp listener use this function."
  (DECLARE (ARGLIST STREAM EOF-OPTION (RUBOUT-HANDLER-OPTIONS '((:ACTIVATION = #/END)))))
  (MULTIPLE-VALUE-BIND (STREAM NIL EOF-VALUE OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-INPUT-EDITING (STREAM (IF OPTIONS (CAR OPTIONS) '((:ACTIVATION = #/END))))
      (INTERNAL-READ STREAM NIL EOF-VALUE NIL NIL T))))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN READ (&REST READ-ARGS)
  "Read an s-expression from a stream and return it.
EOF-OPTION, if supplied is returned if end of file is encountered.
If there is no EOF-OPTION, end of file is an error.
See the documentation for WITH-INPUT-EDITING for the format of RUBOUT-HANDLER-OPTIONS.
READ gets an error if an extraneous closeparen or dot is found.
Command loops should use READ-FOR-TOP-LEVEL instead, to discard them."
  (DECLARE (ARGLIST STREAM EOF-OPTION (RUBOUT-HANDLER-OPTIONS '((:ACTIVATION = #/END)))))
  (MULTIPLE-VALUE-BIND (STREAM EOF-ERROR-P EOF-VALUE OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-INPUT-EDITING (STREAM (IF OPTIONS (CAR OPTIONS) '((:ACTIVATION = #/END))))
      (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE NIL NIL))))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN READ-CHECK-INDENTATION (&REST READ-ARGS)
  "Read an s-expression from a stream and return it, requiring proper indentation.
We assume that all open parens in column zero are supposed
to be top-level lists, except that EVAL-WHEN's, etc, may surround them.
/(Symbols such as EVAL-WHEN should have a SI::MAY-SURROUND-DEFUN property).
If an open paren is encountered in column 0 and is not top level, sufficient
closeparens are /"imagined/" so as to close off enough pending lists to make
the data valid.  End of file closes off all pending lists.
In either case, the SYS:MISSING-CLOSEPAREN condition is signaled,
with an argument that is T for end of file, NIL for open paren encountered."
  (DECLARE (ARGLIST STREAM EOF-OPTION (RUBOUT-HANDLER-OPTIONS '((:ACTIVATION = #/END)))))
  (MULTIPLE-VALUE-BIND (STREAM EOF-ERROR-P EOF-VALUE OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-INPUT-EDITING (STREAM  (IF OPTIONS (CAR OPTIONS) '((:ACTIVATION = #/END))))
      (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE NIL NIL NIL T))))


))

; From file QIO.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun readline (&rest read-args)
  "Read a line from STREAM and return it as a string.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used.

The second value is EOF-OPTION if we exit due to end of file.

The third value is the delimiter which ended the input, or NIL if
it ended due to EOF."
  (declare (arglist &optional stream eof-option options)
	   (values string-or-eof-option eof-flag delimiter))
  (multiple-value-bind (stream eof-error-p eof-value options)
      (decode-kludgey-mucklisp-read-args read-args 1)
    (multiple-value-bind (string eof terminator)
	(read-delimited-string '(#/Newline #/End) stream
			       eof-error-p (car options))
      (values (if (and eof (zerop (length string))) eof-value string)
	      (if eof eof-value)
	      terminator))))

(defun read-from-string (string &optional (eof-option nil eof-option-p) (start 0)
			 end
			 &aux (*iolst string) (*ioch start)
			 (*ioend (or end (string-length string))))
  "Read an expression out of the characters in STRING.
If EOF-OPTION is supplied, it is returned on end of file;
otherwise, end of file is an error.  START (default 0)
is the position in STRING to start reading at; END is where to stop.

The second value is the index in the string at which reading stopped.
It stops after the first object, even if not all the input is used."
  (declare (values contents end-char-position))
  (values (internal-read 'read-from-string-stream (not eof-option-p) eof-option) *ioch))

(defun tyi (&rest read-args &aux ch)
  "Read one character from a stream.  Args are a stream and an eof-option.
The order is irrelevant; an arg which is not a reasonable stream
is taken to be the eof-option, which is returned if end of file is reached.
If there is no eof-option, end of file is an error.
If the stream supports rubout handling but we are not inside the rubout handler,
then the character read is echoed."
  (declare (arglist stream eof-option))
  (multiple-value-bind (stream eof-error-p eof-value)
      (decode-kludgey-mucklisp-read-args read-args)
    (cond ((null (setq ch (send stream :tyi)))	;Get a character, check for EOF
	   (if eof-error-p
	       (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
	     eof-value))
	  ((or rubout-handler			; If inside rubout handler, or
	       (not (memq :rubout-handler (send stream :which-operations))))
	   ch)					;  ordinary device, just return char
	  (t 
	   ;; Echo anything but blips and rubout, even control and meta charcters.
	   (if (and (fixnump ch)
		    ( ch #/Rubout))
	       (format stream "~C" ch))
	   ch))))

(defun readch (&rest read-args &aux ch (eof '(())))
  "Read one character from a stream, and return a symbol with that pname.
Otherwise the same as TYI.  This is an obsolete Maclisp function."
  (declare (arglist stream eof-option))
  (multiple-value-bind (stream eof-error-p eof-value)
      (decode-kludgey-mucklisp-read-args read-args)
    (if (eq (setq ch (tyi stream eof)) eof)
	(if eof-error-p
	    (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
	  eof-value)
	(intern (string ch)))))			;"Character objects" are in current package.

(defun tyipeek (&optional peek-type &rest read-args)
  "If PEEK-TYPE is NIL, the default, returns the next character to be
read from STREAM, part of READ-ARGS, without removing the character from
the input stream.  If PEEK-TYPE is a fixnum less than 1000 octal, this
reads characters until it gets one equal to PEEK-TYPE.  That character
is not removed from the input stream.  If PEEK-TYPE is T, it skips over
all the input characters until the start of the printed representation
of a Lisp object is reached.  Characters passed over by TYIPEEK are echo
if STREAM is interactive.

This is an obsolete function.  Use the :TYIPEEK message for STREAMs
instead."
  (declare (arglist peek-type stream eof-option))
  (multiple-value-bind (stream eof-error-p eof-value)
      (decode-kludgey-mucklisp-read-args read-args)
    (if (characterp peek-type) (setq peek-type (char-int peek-type)))
    (and (numberp peek-type) ( peek-type #o1000)
	 (ferror nil "The ~S flavor of TYIPEEK is not implemented." peek-type))
    (do ((ch))	      ;Pass over characters until termination condition reached
	(())
      (or (setq ch (send stream :tyi))
	  (if eof-error-p
	      (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
	    (return eof-value)))
      (send stream :untyi ch)			;Put it back
      (and (cond ((null peek-type))		;Break on every
		 ((eq ch peek-type))		;Break on specified character
		 ((eq peek-type t)		;Break on start-of-object
		  (and (< ch rdtbl-array-size)
		       (zerop (logand (rdtbl-bits *readtable* ch) 1)))))
	   (return ch))				;Break here
      (tyi stream))))				;Echo and eat this character

))

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

; From file SYSMEN.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFVAR DEFAULT-WINDOW-TYPES-ITEM-LIST
      '(("Supdup" :EVAL SUPDUP::SUPDUP-FLAVOR :DOCUMENTATION "Supdup to network host.")
	("Telnet" :VALUE SUPDUP::TELNET :DOCUMENTATION "TELNET to Chaos or ARPAnet host.")
	("Lisp" :VALUE LISP-LISTENER
	 :DOCUMENTATION "A READ-EVAL-PRINT loop in separate process.")
	("Edit" :VALUE ZWEI:ZMACS-FRAME
	 :DOCUMENTATION "An editor, sharing buffers with other editors.")
	("Peek" :VALUE PEEK-FRAME :DOCUMENTATION "Display status information.")
	("Inspect" :VALUE INSPECT-FRAME :DOCUMENTATION "Browse through data structure.")
	("Font Edit" :VALUE FED::FED-FRAME :DOCUMENTATION "Edit characters in fonts.")
	("Lisp (Edit)" :VALUE ZWEI::EDITOR-TOP-LEVEL
	 :DOCUMENTATION
	 "A READ-EVAL-PRINT loop in separate process with Zmacs-style editing capabilities.")
	("Any" :VALUE T :FONT :MENU-STANDOUT
	 :DOCUMENTATION "Prompts for any flavor name."))
  "Item list of the Create menu for creating inferiors of screens.")

))

; From file SYSMEN.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFVAR SPLIT-SCREEN-ITEM-LIST
	'(("Existing Lisp" :VALUE "Existing Lisp"
	   :DOCUMENTATION "An already existing LISP Listener.")
	  ("Existing Window" :VALUE "Existing Window"
	   :DOCUMENTATION "An already existing window chosen from a menu.")
	  ("Plain Window" :VALUE "Plain Window"
	   :DOCUMENTATION "A window with no special attributes, suitable for simple output.")
	  ("Trace & Error" :VALUE "Trace & Error" :DOCUMENTATION
	   "*TRACE-OUTPUT* and *DEBUG-IO* are directed to this window if it is exposed.")
	  ("Trace" :VALUE "Trace"
	   :DOCUMENTATION "*TRACE-OUTPUT* is directed to this window if it is exposed.")
	  ("Error" :VALUE "Error"
	   :DOCUMENTATION "*DEBUG-IO* is directed to this window if it is exposed.")
	  ("" :NO-SELECT T) ("" :NO-SELECT T)
	  ("Frame" :VALUE "Frame" :DOCUMENTATION "Put chosen windows together in a frame.")
	  ("Mouse Corners" :VALUE "Mouse Corners"
	   :DOCUMENTATION "Specify the area to fill from the mouse.")
	  ("" :NO-SELECT T)
	  ("Undo" :VALUE "Undo" :DOCUMENTATION "Undo last selection.")
	  ("Do It" :VALUE "Do It" :FONT :MENU-STANDOUT :DOCUMENTATION "Complete selection.")
	  ("Abort" :VALUE "Abort" :FONT :MENU-STANDOUT :DOCUMENTATION "Abort Split Screen.")
	  ))

))

; From file SYSMEN.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFUN SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW (SUP EDGES WINDOW-TYPE-LIST N-WINDOWS LAYWIN
					    &AUX N-COLUMNS N-ROWS WIDTH HEIGHT TEM WINDOW SEL)
  (DECLARE (IGNORE LAYWIN))			;ignored for now
  (IF (< N-WINDOWS 4)
      (SETQ N-COLUMNS 1 N-ROWS N-WINDOWS)
      (SETQ N-COLUMNS 2 N-ROWS (TRUNCATE (1+ N-WINDOWS) 2)))
  (SETQ WIDTH (TRUNCATE (- (THIRD EDGES) (FIRST EDGES)) N-COLUMNS)
	HEIGHT (TRUNCATE (- (FOURTH EDGES) (SECOND EDGES)) N-ROWS))
  (LOCK-SHEET (SUP)
    (DOLIST (WINDOW (SHEET-EXPOSED-INFERIORS SUP))
      (SEND WINDOW :DEEXPOSE))
    (DO ((L (NREVERSE WINDOW-TYPE-LIST) (CDR L))
	 (I 0 (1+ I)) (LEFT) (RIGHT) (TOP) (BOTTOM))
	((NULL L))
      (SETQ LEFT (+ (FIRST EDGES) (* (\ I N-COLUMNS) WIDTH))
	    RIGHT (+ LEFT WIDTH)
	    TOP (+ (SECOND EDGES) (* (TRUNCATE I N-COLUMNS) HEIGHT))
	    BOTTOM (+ TOP HEIGHT))
      ;; The bottom-most window is wider if there are an odd number of them
      (AND (NULL (CDR L))
	   (SETQ RIGHT (THIRD EDGES)))
      (COND ((EQUALP (CAR L) "Existing Lisp")
	     (SETQ WINDOW (IDLE-LISP-LISTENER SUP))
	     (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
	     (OR SEL (SETQ SEL WINDOW)))
	    ((SETQ TEM (ASSOC-EQUALP (CAR L)
				     '(("Plain Window" WINDOW)
				       ("Trace" TRACE-OR-ERROR-WINDOW *TRACE-OUTPUT*)
				       ("Error" TRACE-OR-ERROR-WINDOW *DEBUG-IO*)
				       ("Trace & Error" TRACE-OR-ERROR-WINDOW
							*TRACE-OUTPUT* *DEBUG-IO*))))
	     (SETQ WINDOW (MAKE-INSTANCE (CADR TEM)
					 :ALLOW-OTHER-KEYS T	;prevents lossage on next one
					 :STREAM-VARIABLES (CDDR TEM)
					 :SUPERIOR SUP
					 :NAME (AND (CDDR TEM) (CAR TEM))
					 :LEFT LEFT :TOP TOP :RIGHT RIGHT :BOTTOM BOTTOM)))
	    ((NOT (SYMBOLP (CAR L)))		;Window itself
	     (SETQ WINDOW (CAR L))
	     (SEND WINDOW :SET-SUPERIOR SUP)
	     (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
	     (OR SEL (SETQ SEL WINDOW)))
	    (T
	     (SETQ WINDOW (MAKE-WINDOW (CAR L)
				       :SUPERIOR SUP
				       :LEFT LEFT :TOP TOP
				       :RIGHT RIGHT :BOTTOM BOTTOM))
	     (OR SEL (SETQ SEL WINDOW))))
      (SEND WINDOW :EXPOSE))
    (AND SEL (SEND SEL :SELECT)))
  SEL)

))

; From file SYSMEN.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (WHEN STREAM-VARIABLES
    (UNLESS OLD-STREAM-VALUES
      (SETQ OLD-STREAM-VALUES (MAPCAR #'SYMBOL-VALUE STREAM-VARIABLES))
      (MAPC #'SET STREAM-VARIABLES (CIRCULAR-LIST SELF)))))

))


  
; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFSTRUCT (PATCH-SYSTEM :LIST (:CONC-NAME PATCH-) (:INCLUDE PATCH-MAJOR) (:ALTERANT NIL))
  (STATUS NIL :DOCUMENTATION "A keyword: usually one of:
:RELEASED, :EXPERIMENTAL, :OBSOLETE, :INCONSISTENT, :BROKEN")
  (VERSION-LIST NIL :DOCUMENTATION
    "List of PATCH-VERSION structures corresponding to loaded patches. MOST RECENT FIRST.")
  (DIRECTORY-LOADED-ID NIL :DOCUMENTATION
    "Cons of truename and creation-date of the file to which we last read or wrote the
patch directory to a file, or NIL")
  (PATCH-DIR NIL :DOCUMENTATION "The patch-directory, as last read from file, or NIL.")
  )

))

(dolist (foo si::patch-systems-list)
  (setf (cdddr foo) (list (cadddr foo) nil nil)))


; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN READ-PATCH-DIRECTORY (PATCH-SYSTEM &OPTIONAL NOERROR &AUX DIR)
  "Read in a patch directory file, returning the list-structure representation.
PATCH-SYSTEM is an object of type PATCH-SYSTEM.
The value is described by the defstruct PATCH-DIR.
NOERROR means return NIL rather than get error if patch directory file won't open."
  (CONDITION-CASE-IF NOERROR ()
      (WITH-OPEN-FILE (PATCH-DIR (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
							:VERSION-DIRECTORY
							(PATCH-VERSION PATCH-SYSTEM)))
	(LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE)
	      (*READTABLE* INITIAL-READTABLE)
	      (INFO (SEND PATCH-DIR :INFO)))
	  ;; don't waste time reading it in again
	  (IF (AND INFO (EQUAL INFO (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM)))
	      (SETQ DIR (PATCH-PATCH-DIR PATCH-SYSTEM))
	    (SETQ DIR (CLI:READ PATCH-DIR))
	    (SETF (PATCH-PATCH-DIR PATCH-SYSTEM) DIR
		  (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM) INFO)))
	(UNLESS (ASSQ (PATCH-DIR-STATUS DIR) SYSTEM-STATUS-ALIST)
	  (FERROR NIL "UNKNOWN PATCH SYSTEM STATUS ~S for ~A"
		  (PATCH-DIR-STATUS DIR) (PATCH-NAME PATCH-SYSTEM)))
	DIR)
    (FS:FILE-ERROR NIL)))

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN WRITE-PATCH-DIRECTORY (PATCH-SYSTEM PATCH-DIR)
  "Write out a new patch directory file for PATCH-SYSTEM.
PATCH-DIR is a list described by the defstruct PATCH-DIR,
which is the data to write into the file."
  (LET ((*PRINT-BASE* 10.) (*READ-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE)
	(*NOPOINT T) (*PRINT-RADIX* NIL)
	(*READTABLE* INITIAL-READTABLE))
    (WITH-OPEN-FILE (STREAM (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
						   :VERSION-DIRECTORY
						   (PATCH-VERSION PATCH-SYSTEM))
			       :DIRECTION :OUTPUT)
       (FORMAT STREAM
	       ";;; -*- Mode:LISP; Package:USER; Base:10; Readtable:T; Patch-File:T -*-
;;; Patch directory for ~A version ~D
"
	       (PATCH-NAME PATCH-SYSTEM) (PATCH-VERSION PATCH-SYSTEM))
       (WRITE-RESPONSIBILITY-COMMENT STREAM)
       (WRITE-CHAR #/( STREAM)
       (PRIN1 (PATCH-DIR-STATUS PATCH-DIR) STREAM)
       (SEND STREAM :STRING-OUT "
 (")
       (DOLIST (PATCH (PATCH-DIR-VERSION-LIST PATCH-DIR))
	 (PRIN1 PATCH STREAM)
	 (SEND STREAM :STRING-OUT "
  "))
       (SEND STREAM :STRING-OUT "))")
       (SETF (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM) (SEND STREAM :INFO))
       (SETF (PATCH-PATCH-DIR PATCH-SYSTEM) PATCH-DIR))))

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN ADD-PATCH-SYSTEM (NAME &AUX PATCH-SYSTEM VERSION FIRST-VERS PATCH-DIR STATUS)
  "Add a new patchable system named NAME if there is none.
Done when the system is loaded.
Read in the patch directory files to find out the version.
Returns the major version and the system status."
  ;; Flush old patch system if there is one
  (AND (SETQ PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED NAME T))
       (SETQ PATCH-SYSTEMS-LIST (DELQ PATCH-SYSTEM PATCH-SYSTEMS-LIST)))
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME)
	PATCH-SYSTEM (MAKE-PATCH-SYSTEM :NAME NAME :VERSION VERSION)
	PATCH-DIR (READ-PATCH-DIRECTORY PATCH-SYSTEM)
	FIRST-VERS (FIRST (PATCH-DIR-VERSION-LIST PATCH-DIR)))
  (OR (EQ (VERSION-NUMBER FIRST-VERS) 0)
      (FERROR NIL "Patch directory for ~A messed up: ~S" NAME FIRST-VERS))
  (SETF (PATCH-STATUS PATCH-SYSTEM) (SETQ STATUS (PATCH-DIR-STATUS PATCH-DIR)))
  (SETF (PATCH-VERSION-LIST PATCH-SYSTEM) (NCONS FIRST-VERS))
  (SETQ PATCH-SYSTEMS-LIST (NCONC PATCH-SYSTEMS-LIST (NCONS PATCH-SYSTEM)))
  (VALUES VERSION STATUS))

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN SET-SYSTEM-STATUS (SYSTEM NEW-STATUS &OPTIONAL MAJOR-VERSION &AUX PATCH PATCH-DIR)
  "Change the status of the system named SYSTEM to NEW-STATUS.
NEW-STATUS should be :EXPERIMENTAL, :BROKEN, :RELEASED or :OBSOLETE.
If MAJOR-VERSION is specified, the status of that major version is set.
Otherwise the status of the currently loaded major version is set.
This modifies the patch directory files."
  (OR (ASSQ NEW-STATUS SYSTEM-STATUS-ALIST)
      (FERROR NIL "~S is not a defined system status." NEW-STATUS))
  (SETQ PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM))
  (IF (AND MAJOR-VERSION ( MAJOR-VERSION (PATCH-VERSION PATCH)))
      (SETQ PATCH (MAKE-PATCH-SYSTEM :NAME SYSTEM :VERSION MAJOR-VERSION :STATUS NEW-STATUS))
    ;; Also change in core copy
    (SETF (PATCH-STATUS PATCH) NEW-STATUS))
  (SETQ PATCH-DIR (READ-PATCH-DIRECTORY PATCH))
  (SETF (PATCH-DIR-STATUS PATCH-DIR) NEW-STATUS)
  (WRITE-PATCH-DIRECTORY PATCH PATCH-DIR))

))

; From file OPEN.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN OPEN (FILENAME &REST KEYWORD-ARGS)
  "Open a file and return a stream.  FILENAME is a pathname or a string.
DIRECTION is :INPUT, :OUTPUT, :PROBE, :PROBE-LINK :PROBE-DIRECTORY
ELEMENT-TYPE specifies how the data of the stream file are to be interpreted.
  Possible values include :DEFAULT CHARACTER (UNSIGNED-BYTE n) (SIGNED-BYTE n) (MOD n) BIT
One may also specify the type using the following two options:
  CHARACTERS may be T, NIL or :DEFAULT.
  BYTE-SIZE specifies byte size to use for non-character files.
IF-EXISTS specifies what to do if FILENAME already exists when opening it for output.
  NIL means return NIL from OPEN if file already exists
  :ERROR Signal an error (FS:FILE-ALREADY-EXISTS) See the ERROR option
  :NEW-VERSION Defualt for when FILENAME's version is :NEWEST. Create a higher-version file
  :SUPERSEDE Create a new file which, when closed, replaces the old one
  :OVERWRITE Writes over the data of the old file.
    Sets file length to length of the data written during this open when the file is closed.
  :TRUNCATE Like :OVERWRITE, but sets length to 0 immediately upon open
  :APPEND Append new data to the end of the existing file
  :RENAME Rename the existing file to something and then create and use a new one
  :RENAME-AND-DELETE Like :RENAME, only the old (renamed) file is deleted when we close
IF-DOES-NOT-EXIST is one of :CREATE (default for most output opens, except if otherwise
  specified by IF-EXISTS), :ERROR (default for input opens, and the other output opens)
  means signal FS:FILE-NOT-FOUND, or NIL (default for :PROBE-mumble opens) meaning return NIL
ERROR specifies what to do if an error is signaled in the process of opening the file
  T (the default) means that nothing special is done; handlers are invoked if they exist
   or else the debugger is entered.
  NIL means to return the condition object itself as the value of OPEN
  :REPROMPT means to ask the user for a different filename to use instead, and retries.
   See also WITH-OPEN-FILE-RETRY and FILE-RETRY-NEW-PATHNAME, which may be The Right Thing
PRESERVE-DATES means not to alter the files reference or modification dates
ESTIMATED-SIZE informs the remote file system what thew estimated final file size will be
RAW, SUPER-IMAGE disable character set translation from ascii servers
DELETED, TEMPORARY mean to allow opening of deleted or temporary files respectively,
  on systems which support those concepts.
SUBMIT means to submit the file as a batch job on the remote host when the file is closed.

Other system-specific keywords may be supported for some file systems."
  (DECLARE (ARGLIST FILENAME &KEY (DIRECTION :INPUT) (ERROR T) (ELEMENT-TYPE :DEFAULT)
		    		  CHARACTERS BYTE-SIZE IF-EXISTS IF-DOES-NOT-EXIST ERROR
				  PRESERVE-DATES DELETED temporary submit preserve-dates
				  RAW SUPER-IMAGE INHIBIT-LINKS
			     &ALLOW-OTHER-KEYS))
  (FORCE-USER-TO-LOGIN)
  (IF (STREAMP FILENAME)
      (SETQ FILENAME (SEND FILENAME :PATHNAME)))
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME))
  (SETQ LAST-FILE-OPENED FILENAME)
  (IF (OR (NULL KEYWORD-ARGS)			;No args is good args
	  (NOT (NULL (CDR KEYWORD-ARGS))))
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ (GET (LOCF KEYWORD-ARGS) :ERROR) '(:RETRY :REPROMPT))
				  (FILENAME FILE-ERROR)
        (LEXPR-SEND FILENAME :OPEN FILENAME KEYWORD-ARGS))
    ;; Old Syntax.
    (DO ((KEYL (IF (AND (CAR KEYWORD-ARGS) (SYMBOLP (CAR KEYWORD-ARGS)))
		   (LIST (CAR KEYWORD-ARGS))
		 (CAR KEYWORD-ARGS))
	       (CDR KEYL))
	 (KEY)
	 (CHARACTERS T)
	 (DIRECTION :INPUT)
	 (BYTE-SIZE NIL)
	 (ERROR-P T)
	 (ERROR-P-SPECD NIL)
	 (DELETED-P NIL)
	 (TEMPORARY-P NIL)
	 ;; These two are really only useful for machines that do not natively store
	 ;; 8-bit characters.
	 (RAW-P NIL)
	 (SUPER-IMAGE-P NIL)
	 )
	((NULL KEYL)
	 (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR-P '(:RETRY :REPROMPT))
				     (FILENAME FILE-ERROR)
	   ;; Because we don't want to send meaningless keywords to file systems
	   ;; which don't support them, and we don't want to cons...
	   (%ASSURE-PDL-ROOM 19.)			;Worst case
	   (%OPEN-CALL-BLOCK FILENAME 0 4)	;D-RETURN
	   (%PUSH ':OPEN)       (%PUSH FILENAME)
	   (%PUSH ':CHARACTERS) (%PUSH CHARACTERS)
	   (%PUSH ':DIRECTION)  (%PUSH DIRECTION)
	   (COND (BYTE-SIZE     (%PUSH ':BYTE-SIZE)   (%PUSH BYTE-SIZE)))
	   (COND (ERROR-P-SPECD (%PUSH ':ERROR)       (%PUSH ERROR-P)))
	   (COND (DELETED-P     (%PUSH ':DELETED)     (%PUSH DELETED-P)))
	   (COND (TEMPORARY-P   (%PUSH ':TEMPORARY)   (%PUSH TEMPORARY-P)))
	   (COND (SUPER-IMAGE-P (%PUSH :SUPER-IMAGE) (%PUSH SUPER-IMAGE-P)))
	   (COND (RAW-P	      (%PUSH ':RAW)	    (%PUSH RAW-P)))
	   (%ACTIVATE-OPEN-CALL-BLOCK)))
      (SETQ KEY (CAR KEYL))
      (SELECTOR KEY STRING-EQUAL
	((:IN :READ) (SETQ DIRECTION :INPUT))
	((:OUT :WRITE :PRINT) (SETQ DIRECTION :OUTPUT))
	((:BINARY :FIXNUM) (SETQ CHARACTERS NIL))
	((:CHARACTER :ASCII) (SETQ CHARACTERS T))
	((:BYTE-SIZE) (SETQ KEYL (CDR KEYL)
			     BYTE-SIZE (CAR KEYL)))
	((:PROBE) (SETQ DIRECTION NIL
			 CHARACTERS NIL
			 ERROR-P (IF (NOT ERROR-P-SPECD) NIL ERROR-P)
			 ERROR-P-SPECD T))
	((:NOERROR) (SETQ ERROR-P NIL ERROR-P-SPECD T))
	((:ERROR) (SETQ ERROR-P T ERROR-P-SPECD T))
	((:RAW) (SETQ RAW-P T))
	((:SUPER-IMAGE) (SETQ SUPER-IMAGE-P T))
	((:DELETED) (SETQ DELETED-P T))
	((:TEMPORARY) (SETQ TEMPORARY-P T))
	((:BLOCK :SINGLE))			;Ignored for compatility with Maclisp
	(OTHERWISE (FERROR NIL "~S is not a known OPEN option" KEY))))))

))

; From file PRINT.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(defun print-character (char stream fastp
			 &aux (*print-base* 10.) (*print-radix* nil) (*nopoint t))

  (declare (ignore fastp))
  (when *print-escape*
    (send stream :string-out (car (pttbl-character *readtable*)))
    ;;>> ignore printing font of character when *print-escape* is nil for now...
    (if (not (zerop (char-font char)))
	(prin1 (char-font char) stream))
    (send stream :string-out (cdr (pttbl-character *readtable*))))
  (let ((bits (char-bits char))
	(code (char-code char)))
    (send stream :string-out
	  (nth bits
	       '("" "c-" "m-" "c-m-"
		 "s-" "c-s-" "m-s-" "c-m-s-"
		 "h-" "c-h-" "m-h-" "c-m-h-"
		 "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")))
    (cond ((tv:char-mouse-p char)
	   (send stream :string-out "Mouse-")
	   (send stream :string-out (nth (ldb %%kbd-mouse-button char)
					 '("Left-" "Middle-" "Right-")))
	   (prin1 (1+ (ldb %%kbd-mouse-n-clicks char)) stream))
	  (t
	   (let ((chname (format:ochar-get-character-name code)))
	     (if chname
		 (send stream :string-out chname)
	       (and *print-escape*
		    ( bits 0)
		    (character-needs-quoting-p code)
		    (send stream :tyo (pttbl-slash *readtable*)))
	       (send stream :tyo code)))))))
))
