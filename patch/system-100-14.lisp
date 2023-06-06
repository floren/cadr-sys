;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.14
;;; Reason:
;;;  Fix so that C-n moves properly in ZWEI.
;;;  Show all documentation for Terminal-N.
;;; Written 6-Jun-23 16:38:40 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.13, Hacks by AMS 2.0, microcode 323, WIP.



; From file FC: /sys/window/shwarm.lisp at 6-Jun-23 16:38:41
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//window//shwarm"

(DEFUN SHEET-COMPUTE-MOTION (SHEET X Y STRING
			     &OPTIONAL (START 0) (END NIL) (CR-AT-END-P NIL)
				       (STOP-X 0) (STOP-Y NIL) BOTTOM-LIMIT RIGHT-LIMIT
				       FONT
				       (LINE-HT (IF FONT (FONT-CHAR-HEIGHT FONT)
						  (SHEET-LINE-HEIGHT SHEET)))
				       (TAB-WIDTH
					 (IF FONT (* (FONT-CHAR-WIDTH FONT)
						     (SHEET-TAB-NCHARS SHEET))
					   (SHEET-TAB-WIDTH SHEET))))
  "Compute the motion that would be caused by outputing a string.
This is used by the editor and by TV:STREAM-MIXIN.
In computing the motion, it will chose the font in one of two ways:
 If given an ART-FAT-STRING array (16 bit string) like the editor uses,
  it will take the font from the character's CHAR-FONT and look in
  SHEET's font-map.
 If given an ART-STRING array (8 bit string), it will take the font from
  FONT, or the SHEET-CURRENT-FONT of the sheet.
SHEET is used to supply information such as the font map,
 and for defaulting such things as BOTTOM-LIMIT, RIGHT-LIMIT
 and LINE-HT.
STRING, with START and END, specifies what characters to process.
CR-AT-END-P if non-NIL says /"output/" a Newline after
 STRING or the portion of STRING, and count that
 in the cursor motion.
STOP-X and STOP-Y specify a cursor position at which to stop.
 Processing stops when both coordinates are  the stop points.
 The stop points default to the bottom left corner of SHEET.
 Specify a very large value for STOP-Y if you do not
 want processing to stop before the end of STRING.
BOTTOM-LIMIT and RIGHT-LIMIT are a cursor position
 at which to wrap around; these default to
 the inside-size of SHEET.
FONT specifies the font to use, if STRING is not a fat string.
LINE-HT is the line height to use for Newline characters,
 defaulting to SHEET's line height.
TAB-WIDTH is the width to use for Tab characters,
 defaulting to SHEET's SHEET-TAB-WIDTH.

Processing stops either because the string or portion has
been processed or because the stopping-point has been reached.

Returns 4 values:
FINAL-X, FINAL-Y are the cursor position
 at which processing stopped.
FINAL-STRING-INDEX is the index
 in the string at which processing stopped (could be the length
 of the string, if the stop point was passed then), T if stopped
 due to reaching the stop point after the additional Newline,
 or NIL if stopped due to finishing.
MAXIMUM-X was the largest X-position ever encountered during processing."

; *** The interface to this crock should be redesigned.  Also note that the
; *** exact treatment of STOP-X and STOP-Y does not agree with SHEET-STRING-LENGTH.
; *** This is what turning on COMPUTE-MOTION-ROUND-DOWN is going to fix.
  (DECLARE (VALUES FINAL-X FINAL-Y FINAL-STRING-INDEX MAXIMUM-X))
  (IF FONT
      (COERCE-FONT FONT SHEET)
      (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (PROG (CWA CW CH FONTX TEM I N NN II MARGIN-FLAG MAXIMUM-X OLD-X)
	(OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET)) (SETQ MARGIN-FLAG T))
	(AND (NULL X) (SETQ X (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
	(AND (NULL Y) (SETQ Y (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))
	(AND (NULL STOP-Y)
	     (SETQ STOP-Y (1+ (SHEET-INSIDE-HEIGHT SHEET))))
	;;		   ^-- THIS 1+ IS SO CAN USE  RATHER THAN >
	(OR RIGHT-LIMIT (SETQ RIGHT-LIMIT (SHEET-INSIDE-WIDTH SHEET)))
	(AND MARGIN-FLAG (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
	(AND (NULL BOTTOM-LIMIT)
	     (SETQ BOTTOM-LIMIT (- (SHEET-INSIDE-HEIGHT SHEET) LINE-HT)))
	(SETQ MAXIMUM-X X
	      I START
	      N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	      CW (FONT-CHAR-WIDTH FONT))
	;; At this point, decide whether we can use the fast version.
	(COND
	  ;; If FONTX is non-NIL, then we have a string with font changes.
	  ((NEQ 8 (CDR (ASSQ (ARRAY-TYPE STRING) ARRAY-BITS-PER-ELEMENT)))
	   (SETQ FONTX T))
	  ;; The current font is variable width.
	  ((SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))
	  ;; No font changes and the current font is fixed width.
	  ;;  We can use the fast version.
	  (T (GO FAST)))
	;;This is the slow version.
     SLOW
	(SETQ MAXIMUM-X (MAX X MAXIMUM-X))
	(COND ((AND ( Y STOP-Y) ( X STOP-X))	;Reached sticking-point
	       (RETURN (VALUES X Y
			       I MAXIMUM-X)))
	      ((NOT (< I N))			;If string exhausted
	       (WHEN CR-AT-END-P
		 (SETQ X 0 Y (+ Y LINE-HT))	;CRLF if told to
		 (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	       (RETURN (VALUES X Y
			       (AND ( X STOP-X) ( Y STOP-Y)) MAXIMUM-X))))
	;; Move quickly over the remaining characters until we reach
	;; an x-position at which something must be done.
	(UNLESS (EQ FONTX T)
	  (LET ((WIDTH-INCR)
		(LIMIT (MIN RIGHT-LIMIT (IF ( Y STOP-Y) STOP-X RIGHT-LIMIT))))
	    (SETQ WIDTH-INCR
		  (%STRING-WIDTH (OR CWA
				     PRINTING-CHARACTER-TRANSLATE-TABLE)
				 (IF FONTX (DPB FONTX %%CH-FONT 0) 0)
				 STRING I N
				 (IF CWA (- LIMIT X) (FLOOR (- LIMIT X) CW))))
	    (UNLESS CWA
	      (SETQ WIDTH-INCR (* WIDTH-INCR CW)))
	    (SETQ I (%POP))
	    ;; increment positions.
	    (INCF X WIDTH-INCR)
	    ;; At end of string, loop back, to exit.
	    (IF (= I N) (GO SLOW))
	    ;; Otherwise we stopped due to funny char or font change or reaching the X limit.
	    ))
	(SETQ MAXIMUM-X (MAX X MAXIMUM-X))
	(WHEN (AND ( Y STOP-Y) ( X STOP-X))	;If reached sticking-point, done.
	  (RETURN (VALUES X Y
			  I MAXIMUM-X)))
	(SETQ CH (CHAR-CODE (SETQ TEM (CHAR STRING I))))
	(WHEN (AND FONTX (NEQ (SETQ TEM (CHAR-FONT TEM)) FONTX))	;Changing fonts
	  (SETQ FONTX TEM
		FONT (LET ((FONT-MAP (SHEET-FONT-MAP SHEET)))
		       (AREF FONT-MAP (IF ( FONTX (ARRAY-ACTIVE-LENGTH FONT-MAP))
					  0 FONTX)))
		CWA (FONT-CHAR-WIDTH-TABLE FONT)
		CW (FONT-CHAR-WIDTH FONT)))
	(SETQ OLD-X X)
	;; Try to do this one char.
	(COND ((= CH #/NEWLINE)
	       (SETQ X 0 Y (+ Y LINE-HT))
	       (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	      ((< CH #o200)			;Printing character
	       (SETQ X (+ (COND (CWA (AREF CWA CH)) (T CW)) X)))	;do char width
	      ((= CH #/TAB)			;Tab (have to do here since x-dependent)
	       (SETQ TEM TAB-WIDTH)
	       (SETQ X (* (TRUNCATE (+ X TEM) TEM) TEM)))
	      (T				;Format effector
	       (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
	;; If this char went past the stop-point, pretend we stopped before it.
	(WHEN (AND COMPUTE-MOTION-ROUND-DOWN
		   (> X STOP-X) ( Y STOP-Y))
	  (RETURN (VALUES OLD-X Y
			  I MAXIMUM-X)))
	;; If this char went past the right margin, do a CR, then redo this char
	(COND ((> X RIGHT-LIMIT)
	       (SETQ X 0 Y (+ Y LINE-HT))
	       (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	      (T (INCF I)))
	(GO SLOW)

	;;Here is the fast loop.  The basic idea is to scan as fast as possible
	;;over printing characters, with all checking outside the loop.
     FAST 
	(SETQ MAXIMUM-X (MAX X MAXIMUM-X))
	;;First, decide the most characters we want to scan over in a whack
	(SETQ NN
	      (MIN (+ (TRUNCATE (- (IF ( Y STOP-Y)	;Stop-point is in this line
				        STOP-X
				        RIGHT-LIMIT)	;Stop for this line is margin
				    X)
				 CW)
		      I)
		   N))				;NN is limiting value of I
	;; Now, scan over printing characters.
	(WHEN ( (SETQ II I) NN)		;Save initial I, and check for null loop
	  (GO SCX))
     SCN
	(%STRING-WIDTH PRINTING-CHARACTER-TRANSLATE-TABLE
		       0 STRING II NN NIL)
	(SETQ I (%POP))
;       (SETQ TEM #o200)			;This is really a ridiculous bum
;    SCN
;	(AND (< (AREF STRING I) TEM)		;If this is a printing character
;	     (< (SETQ I (1+ I)) NN)		; and we haven't reached stop point
;	     (GO SCN))				; then continue to loop (9 instructions)
	(SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars

     SCX
	(SETQ NN X)
	(SETQ MAXIMUM-X (MAX X MAXIMUM-X))
	(COND ((AND ( Y STOP-Y) ( X STOP-X))	;If reached sticking-point, done.
	       (RETURN (VALUES X Y
			       I MAXIMUM-X)))
	      ((NOT (< I N))			;If string exhausted
	       (WHEN CR-AT-END-P		;Do Newline X off end of line
		 (SETQ X 0 Y (+ Y LINE-HT))	;crlf if told to
		 (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	       (RETURN (VALUES X Y
			       (AND ( X STOP-X) ( Y STOP-Y)) MAXIMUM-X))))
	(SETQ OLD-X X)
	;; Try to do this one char.
	(COND ((= (SETQ CH (CHAR STRING I)) #/CR)
	       (SETQ X 0 Y (+ Y LINE-HT))
	       (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	      ((< CH #o200)			;Printing character
	       (INCF X CW))
	      ((= CH #/TAB)			;Tab (have to do here since x-dependent)
	       (SETQ TEM TAB-WIDTH)
	       (SETQ X (* (TRUNCATE (+ X TEM) TEM) TEM)))
	      (T				;Format effector
	       (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
	;; If this char went past the stop-point, pretend we stopped before it.
	(WHEN (AND COMPUTE-MOTION-ROUND-DOWN
		   (> X STOP-X) ( Y STOP-Y))
	  (RETURN (VALUES OLD-X Y
			  I MAXIMUM-X)))
	;; If this char went past the right margin, do a Newline and then redo this char.
	(COND ((> X RIGHT-LIMIT)
	       (SETQ X 0 Y (+ Y LINE-HT))
	       (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	      (T (INCF I)))
	(GO FAST)))
))

; From file FC: /sys/window/basstr.lisp at 6-Jun-23 16:39:27
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//window//basstr"

(DEFVAR *ESCAPE-KEYS*
     '((#/CLEAR-INPUT KBD-ESC-CLEAR
	"Discard type-ahead" :KEYBOARD-PROCESS)
       (#/RESUME (KBD-ESC-RESUME) 
	"Allow deexposed typeout in window that Terminal-0-S would select.")
       (#/ SYSTEM-MENU-SET-MOUSE-SCREEN
	"Set Mouse screen")
       (#/FORM (KBD-SCREEN-REDISPLAY)
	"Clear and redisplay all windows (Page = Clear Screen)")
       (#/A KBD-ESC-ARREST
	"Arrest process in who-line (minus means unarrest)" :KEYBOARD-PROCESS)
       (#/B KBD-BURY
	"Bury the selected window" :TYPEAHEAD)
       (#/C KBD-COMPLEMENT
	'("Complement video black-on-white state"
	  "With an argument, complement the who-line documentation window")
	:KEYBOARD-PROCESS)
;      (#/D (SI:BUZZ-DOOR) (AND (SI:TECH-SQUARE-FLOOR-P 9) "Open the door"))
;      (#/E (SI:CALL-ELEVATOR) (AND (OR (SI:TECH-SQUARE-FLOOR-P 8)
;					(SI:TECH-SQUARE-FLOOR-P 9))
;				    "Call the elevator"))
       (#/F KBD-FINGER (FINGER-ARG-PROMPT) :TYPEAHEAD)
       (#/G (KBD-GC-STATUS) "Show the current state of all garbage collection.")
       (#/H (KBD-HOSTAT) "Show status of CHAOSnet hosts" :TYPEAHEAD)
       (#/I KBD-ESC-I
	"Selected window deexposed input notify flag (complement, or arg=1 on, 0 off)")
       (#/M KBD-ESC-MORE "Selected window **MORE** enable (complement, or arg=1 on, 0 off)"
	:KEYBOARD-PROCESS)
       (#/N KBD-ESC-NOTIFICATIONS '("Allow notifications to come out."
	"Terminal 1 N  print all notifications (even old ones)"
	"Terminal 2 N  defer notifications, reset who-line")
	:TYPEAHEAD)
       (#/O KBD-OTHER-EXPOSED-WINDOW
	"Select another exposed window" :TYPEAHEAD)
       (#/Q KBD-ESC-Q
	(LET ((PRINTER (OR SI:*DEFAULT-BIT-ARRAY-PRINTER* SI:*DEFAULT-PRINTER*)))
	  (AND (GET (IF (CONSP PRINTER) (CAR PRINTER) PRINTER) 'SI:PRINT-BIT-ARRAY)
	       (FORMAT NIL "Hardcopy the screen on the ~A"
		       (IF (CONSP PRINTER) (CAR PRINTER) PRINTER)))))
       (#/S KBD-SWITCH-WINDOWS
	'("Select the most recently selected window.  With an argument, select the nth"
	  "previously selected window and rotate the top n windows.  (Default arg is 2)."
	  "With an arg of 1, rotate through all the windows."
	  "With a negative arg rotate in the other direction."
	  "With an argument of 0, select a window that wants to type out.")
	:TYPEAHEAD)
       (#/T KBD-ESC-T
	"Selected window deexposed typeout action.  0 - wait, 1 - notify, 2 - permit.")
       (#/V KBD-VIEW-MAIL
	"View new mail. Terminal 1 V - view any file." :TYPEAHEAD)
       (#/W KBD-ESC-W
	'("Switch which process the wholine looks at.  Default is just to refresh it"
	  " 1 means selected-window's process, 2 means freeze on this process,"
	  " 3 means rotate among all processes, 4 means rotate other direction,"
	  " 0 gives a menu of all processes"))
       (#/HOLD-OUTPUT KBD-ESC-OUTPUT-HOLD
	"Expose window on which we have /"Output Hold/"")
       (#/? KBD-ESC-HELP
	NIL :TYPEAHEAD)
       (#/HELP KBD-ESC-HELP
	NIL :TYPEAHEAD)
       (NIL) ;Ones after here are "for wizards"
       (#/CALL (KBD-USE-COLD-LOAD-STREAM)
	"Get to cold-load stream" :TYPEAHEAD)
       (#/C-T KBD-CLEAR-TEMPORARY-WINDOWS
	"Flush temporary windows")
       (#/C-CLEAR KBD-CLEAR-LOCKS
	"Clear window-system locks")
       (#/C-A KBD-ESC-ARREST-ALL
	"Arrest nearly all processes" :KEYBOARD-PROCESS))
  "Determines what to do with characters typed after the Terminal character.
A list of elements (CHAR FUNCTION DOCUMENTATION . OPTIONS).
CHAR is what character this element applies to.
If FUNCTION is a list, it is evaluated; otherwise, it is called with one arg,
 which is either NIL or the numeric arg (1 in Terminal 1 F).
The evaluation or calling is normally done in a separate process.
DOCUMENTATION can be a string, a function that returns a string, or NIL.
 NIL means the this entry will not appear in the help message.
 It can also be a list of strings that go on separate lines.
OPTIONS are keywords (with no values).  Defined options are:
    :TYPEAHEAD - copy the contents of the
	software buffer into the currently selected IO-BUFFER.  This has the
	effect of treating everything typed before the TERMINAL as typeahead to
	the currently selected window.  Useful for TERMINAL commands that
	change the selected window.  These commands should set KBD-ESC-TIME to NIL
	as soon as they change the selected window, unless they complete quickly
	(input should never be done with KBD-ESC-TIME non-NIL).
    :KEYBOARD-PROCESS - run the function in the keyboard process instead of starting
	a new process for it.")
))
