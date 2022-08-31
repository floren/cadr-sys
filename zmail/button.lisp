;;; Those flashing buttons used by ZMail -*- Mode:LISP; Package:TV; Base:8 -*-
;;; This is SYS: ZMAIL; BUTTON
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

;;; Some frame and pane help
(DEFUN (WHITE-INCLUDE-WHITESPACE CONSTRAINT-MACRO) (OLD-DESC IGNORE)
  `(INTERDIGITATED-WHITESPACE :WHITE :INCLUDE
    . ,(CDDR OLD-DESC)))

(DEFUN (PANES-IN-WHITESPACE CONSTRAINT-MACRO) (OLD-DESC IGNORE &AUX SIZE PANES)
  (SETF `(NAME PANES-IN-WHITESPACE ,SIZE ,PANES) OLD-DESC)
  `(WHITE-INCLUDE-WHITESPACE
    ,SIZE (:EVEN)
    ,PANES
    ,(LOOP FOR PANE IN PANES COLLECT `(,PANE :ASK :PANE-SIZE))))

(DEFUN (SINGLE-PANE-IN-WHITESPACE CONSTRAINT-MACRO) (OLD-DESC IGNORE &AUX PANE)
  (SETF `(NAME SINGLE-PANE-IN-WHITESPACE ,PANE) OLD-DESC)
  `(PANES-IN-WHITESPACE (:ASK-WINDOW ,PANE :PANE-SIZE) (,PANE)))

(DEFUN (FLOATING-BUTTONS CONSTRAINT-MACRO) (OLD-DESC STACKING &AUX PANES CONVERSE-STACKING
								   NAME-1 NAME-2)
  (SETF `(NAME FLOATING-PANES ,PANES) OLD-DESC)
  (SETQ CONVERSE-STACKING (IF (EQ STACKING :VERTICAL) :HORIZONTAL :VERTICAL)
	NAME-1 (GENSYM) NAME-2 (GENSYM))
  `(,CONVERSE-STACKING (:ASK-WINDOW ,(CAR PANES) :PANE-SIZE-WITH-WHITESPACE)
    (,NAME-1)
    ((,NAME-1 ,STACKING (:EVEN)
      (,NAME-2)
      ((,NAME-2 PANES-IN-WHITESPACE (:ASK-WINDOW ,(CAR PANES) :PANE-SIZE)
	,PANES))))))

(DEFUN (FLOATING-MENUS CONSTRAINT-MACRO) (OLD-DESC IGNORE &AUX SIZE PANES NAMES)
  (SETF `(NAME FLOATING-MENUS ,SIZE ,PANES) OLD-DESC)
  (SETQ NAMES (LOOP FOR PANE IN PANES COLLECT (GENSYM)))
  `(WHITE-INCLUDE-WHITESPACE ,SIZE (:EVEN)
    ,NAMES
    ,(LOOP FOR PANE IN PANES
	   FOR NAME IN NAMES
	   COLLECT `(,NAME WHITE-INCLUDE-WHITESPACE
		     (:ASK-WINDOW ,PANE :PANE-SIZE) (:EVEN)
		     (,PANE)
		     ((,PANE :ASK :PANE-SIZE))))))

(DEFFLAVOR WHITESPACE-PANE-MIXIN () ())

(DEFMETHOD (WHITESPACE-PANE-MIXIN :PANE-SIZE-WITH-WHITESPACE)
	   (REM-WIDTH REM-HEIGHT MAX-WIDTH MAX-HEIGHT STACKING &AUX WITHOUT)
  (SETQ WITHOUT (SEND SELF :PANE-SIZE REM-WIDTH REM-HEIGHT MAX-WIDTH MAX-HEIGHT STACKING))
  (SETQ WITHOUT (+ WITHOUT 5))
  (SELECTQ STACKING
    (:VERTICAL (MIN REM-HEIGHT WITHOUT))
    (:HORIZONTAL (MIN REM-WIDTH WITHOUT))))

(DEFFLAVOR XOR-ACCENT-MIXIN ((ACCENT NIL)) ()
  (:GETTABLE-INSTANCE-VARIABLES ACCENT)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW))

(DEFMETHOD (XOR-ACCENT-MIXIN :SET-ACCENT) (ACCENT-P)
  (OR (EQ (NOT ACCENT-P) (NOT ACCENT))
      (SHEET-FORCE-ACCESS (SELF)
	(SEND SELF :XOR-ACCENT)))
  (SETQ ACCENT ACCENT-P))

(DEFMETHOD (XOR-ACCENT-MIXIN :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P (NOT ACCENT)
      (SEND SELF :XOR-ACCENT)))

(DEFMETHOD (XOR-ACCENT-MIXIN :XOR-ACCENT) ()
  (PREPARE-SHEET (SELF)
    (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
		     (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP) ALU-XOR SELF)))

(DEFFLAVOR BASIC-BUTTON ((DOCUMENTATION NIL)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES DOCUMENTATION))

(DEFMETHOD (BASIC-BUTTON :AFTER :REFRESH) (&REST IGNORE)
  (OR RESTORED-BITS-P (SHEET-DISPLAY-X-Y-CENTERED-STRING SELF NAME)))

(DEFMETHOD (BASIC-BUTTON :SET-NAME) (NEW-NAME)
  (SETQ NAME NEW-NAME)
  (SHEET-FORCE-ACCESS (SELF)
    (SEND SELF :REFRESH)))

(DEFMETHOD (BASIC-BUTTON :WHO-LINE-DOCUMENTATION-STRING) ()
  DOCUMENTATION)

(DEFMETHOD (BASIC-BUTTON :PANE-SIZE) (REM-WIDTH REM-HEIGHT IGNORE IGNORE STACKING)
  (SELECTQ STACKING
    (:VERTICAL (MIN REM-HEIGHT HEIGHT))
    (:HORIZONTAL (MIN REM-WIDTH
		      (LET ((INSIDE-WIDTH (+ (* CHAR-WIDTH 2)	;Allow a little whitespace
					     (SHEET-STRING-LENGTH SELF NAME))))
			(LET ((L (GET-HANDLER-FOR SELF :LABEL-SIZE)))
			  (AND L (SETQ L (SEND L :LABEL-SIZE))
			       (SETQ INSIDE-WIDTH (MAX INSIDE-WIDTH L))))
			(+ INSIDE-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))))))

(DEFFLAVOR SMALL-BUTTON-PANE () (XOR-ACCENT-MIXIN BASIC-BUTTON LIST-MOUSE-BUTTONS-MIXIN
				 WHITESPACE-PANE-MIXIN WINDOW-WITHOUT-LABEL)
  (:DEFAULT-INIT-PLIST :CHARACTER-HEIGHT 1 :BLINKER-P NIL :MORE-P NIL))

(DEFMETHOD (SMALL-BUTTON-PANE :BEFORE :FORCE-KBD-INPUT) (IGNORE)
  (SEND SELF :SET-ACCENT T))

(DEFMETHOD (SMALL-BUTTON-PANE :MOUSE-SELECT) (&REST IGNORE)
  )

(DEFFLAVOR MEDIUM-BUTTON-PANE () (SMALL-BUTTON-PANE)
  (:DEFAULT-INIT-PLIST :FONT-MAP '(FONTS:MEDFNT)))

(DEFFLAVOR BUTTON-PANE () (SMALL-BUTTON-PANE)
  (:DEFAULT-INIT-PLIST :FONT-MAP '(FONTS:HL12B) :BORDERS 2))

(DEFFLAVOR BIG-BUTTON-PANE () (SMALL-BUTTON-PANE)
  (:DEFAULT-INIT-PLIST :BORDERS 3 :FONT-MAP '(FONTS:BIGFNT)))

(DEFFLAVOR BIG-BUTTON-WITH-TOP-OUTSIDE-LABEL-PANE () (TOP-LABEL-MIXIN BUTTON-PANE))

;Define a :SET-PANES-ITEM-LIST message that changes one of our panes'
;ITEM-LIST while taking account of the fact that this may cause the pane
;(a menu, presumably) to change its size.
(DEFFLAVOR ITEM-LIST-PANE-KLUDGE () ()
  (:REQUIRED-FLAVORS BASIC-FRAME))

(DEFMETHOD (ITEM-LIST-PANE-KLUDGE :SET-PANES-ITEM-LIST) (PANE NEW-ITEM-LIST)
  (SETQ PANE (SEND SELF :GET-PANE PANE))
  (COND ((NOT (EQUAL NEW-ITEM-LIST (SEND PANE :ITEM-LIST)))
	 (WITHOUT-SCREEN-MANAGEMENT
	   (LET-GLOBALLY ((RECURSION T))
	     (SEND PANE :DEEXPOSE)))		;This is necessary because it may not fit
	 (SEND PANE :SET-ITEM-LIST NEW-ITEM-LIST)
	 T)))

(DEFFLAVOR FRAME-WITH-XOR-BUTTONS () ()
  (:REQUIRED-FLAVORS BASIC-CONSTRAINT-FRAME))

(DEFMETHOD (FRAME-WITH-XOR-BUTTONS :TURN-OFF-ACCENTS) ()
  (DO ((-PANES- INTERNAL-PANES (CDR -PANES-))
       (PANE))
      ((NULL -PANES-))
    (SETQ PANE (CDAR -PANES-))
    (SEND PANE :SEND-IF-HANDLES :SET-ACCENT NIL)))

(DEFMETHOD (FRAME-WITH-XOR-BUTTONS :SET-PANES-NAME) (PANE NEW-NAME &AUX X Y)
  (SETQ PANE (SEND SELF :GET-PANE PANE))
  (COND ((NOT (EQUAL NEW-NAME (SEND PANE :NAME)))
	 (SETQ X (+ (SHEET-X-OFFSET PANE) (TRUNCATE (SHEET-WIDTH PANE) 2))
	       Y (SHEET-Y-OFFSET PANE))
	 (DELAYING-SCREEN-MANAGEMENT
	   (LET-GLOBALLY ((RECURSION T))
	     (SEND PANE :DEEXPOSE)
	     (SEND PANE :SET-NAME NEW-NAME)
	     (LET ((NEW-WIDTH (SEND PANE :PANE-SIZE (SHEET-INSIDE-WIDTH)
				    (SHEET-INSIDE-HEIGHT) (SHEET-INSIDE-WIDTH)
				    (SHEET-INSIDE-HEIGHT) :HORIZONTAL)))
	       (DECF X (TRUNCATE NEW-WIDTH 2))
	       (SEND PANE :SET-EDGES X Y (+ X NEW-WIDTH) (+ Y (SHEET-HEIGHT PANE))))
	     ;; Otherwise Clear-screen loses afterward if button has become bigger.
	     ;; Does this slow things down too much?
	     (SEND SELF :SET-CONFIGURATION CONFIGURATION)
;	     (SEND PANE :EXPOSE)
	     )))))

(DEFFLAVOR BUTTONS-FRAME
	()
	(FRAME-WITH-XOR-BUTTONS CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER BORDERS-MIXIN))

(DEFMETHOD (BUTTONS-FRAME :BEFORE :INIT) (IGNORE &AUX PANES-NAMES)
  (SETQ PANES-NAMES (MAPCAR #'CAR PANES))
  (SETQ CONSTRAINTS `((ONLY . ((BUTTONS)
			       ((BUTTONS :HORIZONTAL (1.0) (BUTTONS-1)
			         ((BUTTONS-1 WHITE-INCLUDE-WHITESPACE (1.0) (:EVEN)
					     (BUTTONS-2)
				   ((BUTTONS-2 FLOATING-BUTTONS ,PANES-NAMES)))))))))))

(DEFMETHOD (BUTTONS-FRAME :PANE-SIZE) (&REST ARGS)
  (+ (LEXPR-SEND (CDAR INTERNAL-PANES) :PANE-SIZE ARGS) 5))

(DEFMETHOD (BUTTONS-FRAME :CHANGE-BUTTONS) (&REST PANES-AND-NAMES)
  (LOOP FOR (PANE PANE-NAME) ON PANES-AND-NAMES BY 'CDDR
	DO (SETF (SHEET-NAME PANE) PANE-NAME)
	FINALLY (SEND SELF :SET-CONFIGURATION 'ONLY)))

(COMPILE-FLAVOR-METHODS BIG-BUTTON-PANE BIG-BUTTON-WITH-TOP-OUTSIDE-LABEL-PANE BUTTON-PANE
			MEDIUM-BUTTON-PANE SMALL-BUTTON-PANE BUTTONS-FRAME)
