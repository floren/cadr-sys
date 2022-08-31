;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.33
;;; Menus:
;;;   :SET-DEFAULT-FONT does the right thing.
;;;   Geometry recomputed in a few additional places where this was necessary.
;;;   Mouse blinker uses correct height for the font of the item they are over.
;;; Sheet font-maps record the value supplied to :SET-CURRENT-FONT.
;;; FED:DISPLAY-FONT now accepts a font specifier, not just a font object.
;;;    Doesn't bash current-font of the window it uses
;;; Fix a few functions which didn't know about CLI:LAMBDA, etc.
;;; PKG-BIND knows that (eq si:pkg-keyword-package si:pkg-user-package) => nil
;;; Fix PACKAGE-DECLARE on existing packages by defining SI:ALTER-PACKAGE
;;;   (was bashed by defstruct alter macro)
;;; PKG-GOTO takes optional third arg meaning to do a PKG-GOTO-GLOBALLY as well.
;;; CLI:DEFSTRUCT accidentally left out of SI:*COMMON-LISP-SYMBOL-SUBSTITUTIIONS*
;;; Forward CLI:// value cell to GLOBAL:// to make lisp listeners work right.
;;; Make COMMON-LISP-INCOMPATIBLE auto-exporting.
;;;   Clean up symbols in it.
;;; FORMAT
;;;   Make nested use of commands which use clauses work.
;;;   Some internal gc improvements.
;;;   Use (DEFPROP [ ] FORMAT:FORMAT-MATCHING-DELIMITER) to get format parsing to
;;;     work for format commands which come in pairs.
;;; Make Luke Codewalker understand &QUOTEd arglists.
;;; Printing, reading, describing 0-rank arrays.
;;; SI:PRINT-PATCHES takes optional third arg meaning to only describe patches
;;;   made after that minor version number.
;;; Reading package prefixes in commonlisp -- function left out of 98.31. Foo.
;;; Multidimensional string and bit arrays print as
;;;   #3a(("foo" "bar") ("baz" "quux")) and #2a(#*1010 #*1111)
;;;   when *print-array* is non-nil
;;; m-x View file hacks atomic font-lists (eg -*- Fonts:Foo -*-)
;;; ZWEI:READ-FUNCTION-NAME (as used by m-., etc) really changes the mouse blinker to 
;;;   ATOM-UNDER-MOUSE knows about the ZMACS-BUFFERS property.
;;; SELECT-MATCH understands T as the car of a clause to mean OTHERWISE.
;;; New CPTFONTB.
;;; Yow! Are we DONE yet?
;;; Written 25-Jan-84 04:58:41 by Mly,
;;; while running on Lisp Machine Thirty-one from band 4
;;; with System 98.29, CADR 3.5, ZMail 53.9, MIT-Specific 22.0, microcode 306.



; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFUN MENU-ITEM-STRING-WIDTH (ITEM &OPTIONAL STOP-X)
  "Return the width in pixels of the way item ITEM is displayed on the menu.
If STOP-X is specified, we will not return a value larger than that."
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (MULTIPLE-VALUE-BIND (STRING FONT)
      (MENU-ITEM-STRING ITEM (FONT-EVALUATE DEFAULT-FONT) SELF)
    (SHEET-STRING-LENGTH SELF STRING 0 NIL STOP-X FONT)))

))

; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFUN MENU-COMPUTE-FONT-MAP (ITEMS &AUX (MAP (NCONS DEFAULT-FONT)) FONT)
  "Compute the font map to use for a menu, from an item list.
The font map we compute has all the fonts any items need."
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (DOLIST (ITEM ITEMS)
    (SETQ FONT (AND (CONSP ITEM)
		    (CONSP (CDR ITEM))
		    (GET (CDDR ITEM) ':FONT)))
    (AND FONT (NOT (MEMQ FONT MAP))
	 (PUSH FONT MAP)))
  (NREVERSE MAP))

))

; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFMETHOD (BASIC-MENU :BEFORE :INIT) (INIT-PLIST &AUX (SUP SUPERIOR) TEM)
  (SETQ SUP (OR SUP (GET INIT-PLIST ':SUPERIOR) DEFAULT-SCREEN))
  (SETQ DEFAULT-FONT (IF (VARIABLE-BOUNDP DEFAULT-FONT)
			 (SEND (SHEET-GET-SCREEN SUP) ':PARSE-FONT-SPECIFIER DEFAULT-FONT)
		       (SEND (SHEET-GET-SCREEN SUP) ':PARSE-FONT-SPECIFIER ':MENU)))
  (OR (VARIABLE-BOUNDP FONT-MAP)
      (SETQ FONT-MAP (MENU-COMPUTE-FONT-MAP (GET INIT-PLIST ':ITEM-LIST))))
  (PUTPROP INIT-PLIST NIL ':MORE-P)
  (SETQ TEM (GET INIT-PLIST ':GEOMETRY))
  (IF (> (LENGTH TEM) (LENGTH GEOMETRY))
      ;; Longer than we need, take a copy of the list
      (SETQ GEOMETRY (COPYLIST TEM))
      ;; Else copy the appropriate piece of user specified list into our list
      (DO ((TEM TEM (CDR TEM))
	   (GEO GEOMETRY (CDR GEO)))
	  ((NULL TEM))
	(SETF (CAR GEO) (CAR TEM))))
  (AND (GET INIT-PLIST ':FILL-P)
       ;(SETF (GEOMETRY-FILL-P GEOMETRY) T)  ;Compiler gives a gratuitous warning for this
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) 0))
  (AND (SETQ TEM (GET INIT-PLIST ':ROWS))
       (SETF (GEOMETRY-N-ROWS GEOMETRY) TEM))
  (AND (SETQ TEM (GET INIT-PLIST ':COLUMNS))
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) TEM))
  ;; We'll handle SAVE-BITS ourselves later
  ;; This is so the bit array doesn't get created until we know the size
  (PUTPROP INIT-PLIST (GET INIT-PLIST ':SAVE-BITS) ':MENU-SAVE-BITS)
  (PUTPROP INIT-PLIST NIL ':SAVE-BITS))

))

; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFMETHOD (BASIC-MENU :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SETQ NEW-ITEM-LIST (REMQ NIL NEW-ITEM-LIST))
  (SETQ ITEM-LIST NEW-ITEM-LIST
	LAST-ITEM NIL
	CURRENT-ITEM NIL)
  (FUNCALL-SELF ':SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST))
;set-font-map does this
;  (MENU-COMPUTE-GEOMETRY T)		;Recompute parameters, and redraw menu
  NEW-ITEM-LIST)

))

; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFMETHOD (BASIC-MENU :AFTER :SET-FONT-MAP) (&REST IGNORE)
  (MENU-COMPUTE-GEOMETRY T))

))

; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFMETHOD (BASIC-MENU :MOUSE-MOVES) (X Y
				      &AUX ITEM ITEMS ROW XREL BLINKER BLX (BLWIDTH 0)
				           COLN STOP-ITEM ITEM-BASELINE-ADJUST
					   (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (MOUSE-SET-BLINKER-CURSORPOS)
  (SETQ ROW (TRUNCATE (- Y (SHEET-INSIDE-TOP) -2) ROW-HEIGHT)
	XREL (- X (SHEET-INSIDE-LEFT) -2)
	BLINKER (CAR BLINKER-LIST))
  (COND ((AND ( X (SHEET-INSIDE-LEFT) 0)	;If inside the menu
	      (< X (SHEET-INSIDE-RIGHT))
	      ( Y (SHEET-INSIDE-TOP))
	      (< Y (SHEET-INSIDE-BOTTOM)))
	 ;;If mouse is past the last displayed row, blink item on that row.
	 (AND (OR ( (+ TOP-ROW ROW) TOTAL-ROWS) (>= ROW SCREEN-ROWS))
	      (SETQ ROW (1- (MIN SCREEN-ROWS (- TOTAL-ROWS TOP-ROW)))))
	 (IF (MINUSP ROW) (SETQ ITEMS NIL STOP-ITEM NIL)	;No items visible
	     (SETQ ITEMS (AREF ROW-MAP (+ TOP-ROW ROW))
		   STOP-ITEM (AREF ROW-MAP (+ TOP-ROW ROW 1))))
	 (COND (FILL-P				;Fill mode, cogitate
		(SETQ BLX 0)
		(DO ((L ITEMS (CDR L))
		     (ITM) (OITM NIL ITM)
		     (X 0 (+ X
			     (SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITM))
			     MENU-INTERWORD-SPACING)))
		    ((OR (NULL L)
			 (> X XREL))	    ;If this string crosses the mouse, it's the one
		     (SETQ ITEM OITM))
		  (AND (EQ L STOP-ITEM)
		       ;; The next item on next line -- punt
		       (RETURN NIL))
		  (SETQ ITM (CAR L)
			BLX X)))
	       (T				;Columnated, find which column
		(SETQ COLN (TRUNCATE XREL COLUMN-WIDTH))	;Column selected
		(SETQ ITEM (CAR (NTHCDR COLN ITEMS)))	;This may be NIL
		(SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH))
		(SETQ BLX (+ (* COLN COLUMN-WIDTH)	;Start of column
			     (MAX 0 (TRUNCATE (- COLUMN-WIDTH	;Centering
						 MENU-INTERCOLUMN-SPACING
						 BLWIDTH)
					      2))))))))
  (MULTIPLE-VALUE-BIND (NIL ITEM-FONT)
      (MENU-ITEM-STRING ITEM CURRENT-FONT SELF)
    (SETQ ITEM-BASELINE-ADJUST (- BASELINE (FONT-BASELINE ITEM-FONT)))
    ;; If this item is non-selectable, don't select it.
    (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM))) (NOT (ATOM (CDDR ITEM)))
	 (EQ (CADR ITEM) ':NO-SELECT)
	 (SETQ ITEM NIL))
    ;; Now make the blinker be where and what we have just found it should be.
    (BLINKER-SET-VISIBILITY BLINKER (NOT (NULL ITEM)))
    (SETQ CURRENT-ITEM ITEM)
    (COND (ITEM
	   (FUNCALL BLINKER ':SET-SIZE-AND-CURSORPOS 
		    (+ BLWIDTH 1)
		    (+ (FONT-CHAR-HEIGHT ITEM-FONT)
		       0)
;		       1)
		    BLX
;		    (+ BLX 1)
		    (+ (* ROW ROW-HEIGHT) ITEM-BASELINE-ADJUST)
		    )))))

))

; From file MENU.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :ADD-ITEM) (NEW-ITEM)
  (UNLESS (MEMBER NEW-ITEM ITEM-LIST)
    (SETQ ITEM-LIST (NCONC ITEM-LIST (LIST NEW-ITEM)))
    (SETQ LAST-ITEM NIL CURRENT-ITEM NIL)
    (FUNCALL-SELF ':SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST)))
  NEW-ITEM)

))

; From file SHEET.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFSTRUCT (FONT-MAP :ARRAY-LEADER :CONC-NAME (:ALTERANT NIL) (:MAKE-ARRAY (:LENGTH 26.)))
  (FILL-POINTER 26.)
  (FONT-LIST NIL :DOCUMENTATION "List of fonts or font names from this font-map was constructed.")
  (CURRENT-FONT 0 :DOCUMENTATION "What was supplied to the last call to :SET-CURRENT-FONT."))

))

; From file SHEET.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFMETHOD (SHEET :SET-CURRENT-FONT) (NEW-FONT &OPTIONAL OK-IF-NOT-IN-FONT-MAP &aux font tem)
  (WITHOUT-INTERRUPTS
    (SETF (FONT-MAP-CURRENT-FONT FONT-MAP)
	  (COND ((NUMBERP NEW-FONT) (SETQ FONT (AREF FONT-MAP NEW-FONT)) NEW-FONT)
		(T (SETQ FONT (SEND (SHEET-GET-SCREEN SELF) ':PARSE-FONT-SPECIFIER NEW-FONT)
			 TEM (POSITION FONT FONT-MAP))
		   (UNLESS (OR TEM OK-IF-NOT-IN-FONT-MAP)
		     (FERROR NIL "~A IS NOT IN THE FONT MAP OF ~S." FONT SELF))
		   (IF (SYMBOLP NEW-FONT) NEW-FONT (OR TEM FONT)))))
    (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
      (IF BL (FUNCALL BL ':SET-SIZE (FONT-CHAR-WIDTH FONT) (FONT-CHAR-HEIGHT FONT))))
    (SETQ CURRENT-FONT FONT
	  BASELINE-ADJ (- BASELINE (FONT-BASELINE FONT))
	  CHAR-WIDTH (FONT-CHAR-WIDTH FONT))))

))

; From file SHEET.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN SHEET-NEW-FONT-MAP (NEW-MAP VSP &AUX (SCREEN (SHEET-GET-SCREEN SELF)))
  "Set SELF's FONT-MAP and VSP according to NEW-MAP and VSP.
NEW-MAP is either a legitimate font-map array,
or a list or ordinary array of fonts or font descriptors.
VSP is a number of pixels."
  (DECLARE (:SELF-FLAVOR SHEET))
  (UNLESS (AND (ARRAYP NEW-MAP)
	       ( (ARRAY-LENGTH NEW-MAP) 26.)
	       (EQ (ARRAY-LEADER-LENGTH NEW-MAP) 3))
    (IF (ARRAYP NEW-MAP) (SETQ NEW-MAP (LISTARRAY NEW-MAP)))
    (LET* ((LENGTH (MAX (LENGTH NEW-MAP) 26.))
	   (FM (MAKE-FONT-MAP :FONT-LIST NEW-MAP
			      :FILL-POINTER LENGTH
			      :MAKE-ARRAY (:LENGTH LENGTH))))
      (FILLARRAY FM NEW-MAP)
      (SETQ NEW-MAP FM)))
  ;; Now that NEW-MAP contains fonts descriptors, extract the real fonts
  (DOTIMES (I (ARRAY-ACTIVE-LENGTH NEW-MAP))
    (ASET (FUNCALL SCREEN ':PARSE-FONT-SPECIFIER (AREF NEW-MAP I)) NEW-MAP I))
  (WITHOUT-INTERRUPTS
    (SETQ FONT-MAP NEW-MAP)
    ;;Now, find out the character dimensions of this set of fonts
    (LET ((FONT (AREF NEW-MAP 0)))
      (SETQ CURRENT-FONT FONT)
      (SETF (FONT-MAP-CURRENT-FONT FONT-MAP) 0)
      (SETQ CHAR-WIDTH (FONT-CHAR-WIDTH FONT))
      (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
	(IF BL (FUNCALL BL ':SET-SIZE (FONT-CHAR-WIDTH FONT) (FONT-CHAR-HEIGHT FONT)))))
    (DO ((I 0 (1+ I))
	 (LENGTH (ARRAY-ACTIVE-LENGTH NEW-MAP))
;	 (MAXWIDTH 0)
	 (MAXHEIGHT 0)
	 (MAXBASE 0)
	 (FONT))
	(( I LENGTH)
	 (SETQ BASELINE MAXBASE
	       LINE-HEIGHT (+ VSP MAXHEIGHT)))
      (SETQ FONT (AREF NEW-MAP I))
      (DO () ((NOT (SYMBOLP FONT)))
	(SETQ FONT (SYMEVAL FONT)))
      (SETQ MAXHEIGHT (MAX MAXHEIGHT (FONT-CHAR-HEIGHT FONT))
	    MAXBASE (MAX MAXBASE (FONT-BASELINE FONT)))
;     (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
;       (IF CWT
;	   (DOTIMES (J 200)
;	       (SETQ MAXWIDTH (MAX MAXWIDTH (AREF TEM J))))
;	   (SETQ MAXWIDTH (MAX MAXWIDTH (FONT-CHAR-WIDTH (AREF NEW-MAP I))))))
      )
    (SETQ BASELINE-ADJ (- BASELINE (FONT-BASELINE CURRENT-FONT)))
    ))

))

(defmethod (tv:basic-menu :default-font) () tv:default-font)

; From file SHEET.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(defun ufm (window)
  (let* ((old-font-map (send window ':font-map))
	 (font-map-font-list (font-map-font-list old-font-map))
	 (current-font (font-map-current-font-name old-font-map))
	 tem)
    (setq font-map-font-list
	  (mapcar 'update-font font-map-font-list))
    (unless current-font
      (setq current-font (send window ':current-font)))
    (setq current-font (cond ((numberp current-font)
			      current-font)
			     ((memq current-font font-map-font-list)
			      current-font)
			     ((memq (font-name current-font) font-map-font-list)
			      (font-name current-font))
			     ((setq tem (position current-font old-font-map))
			      tem)
			     (t (update-font current-font))))
    (send window ':set-font-map font-map-font-list)
    (send window ':send-if-handles ':set-default-font
		 (update-font (or (send window ':send-if-handles ':default-font)
				  (send (sheet-get-screen window) ':parse-font-specifier
								  ':default))))
    (send window ':set-current-font current-font t))
  (dolist (i (send window ':inferiors))
    (ufm i)))


))


; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFVAR *COMMON-LISP-SYMBOL-SUBSTITUTIONS*
	(COPYTREE
	  '((// . CLI://) 
	    (AR-1 . CLI:AR-1)
	    (AR-1-FORCE . CLI:AR-1-FORCE)
	    (AREF . CLI:AREF)
	    (ASSOC . CLI:ASSOC)
	    (CATCH . CLI:CATCH)
	    (CHARACTER . CLI:CHARACTER)
	    (CLOSE . CLI:CLOSE)
	    (COPY-READTABLE . CLI:COPY-READTABLE)
	    (DEFSTRUCT . CLI:DEFSTRUCT)
	    (DELETE . CLI:DELETE)
	    (ERROR . CLI:ERROR)
	    (EVAL . CLI:EVAL)
	    (EVAL . CLI:EVAL)
	    (EVERY . CLI:EVERY)
	    (FORMAT . CLI:FORMAT)
	    (INTERSECTION . CLI:INTERSECTION)
	    (LAMBDA . CLI:LAMBDA)
	    (LISTP . CLI:LISTP)
	    (MAP . CLI:MAP)
	    (MEMBER . CLI:MEMBER)
	    (NAMED-LAMBDA . CLI:NAMED-LAMBDA)
	    (NAMED-SUBST . CLI:NAMED-SUBST)
	    (NINTERSECTION . CLI:NINTERSECTION)
	    (NLISTP . CLI:NLISTP)
	    (NUNION . CLI:NUNION)
	    (RASSOC . CLI:RASSOC)
	    (READ . CLI:READ)
	    (READ-FROM-STRING . CLI:READ-FROM-STRING)
	    (REM . CLI:REM)
	    (REMOVE . CLI:REMOVE)
	    (SOME . CLI:SOME)
	    (STRING-EQUAL . CLI:STRING-EQUAL)
	    (SUBST . CLI:SUBST)
	    (TERPRI . CLI:TERPRI)
	    (THROW . CLI:THROW)
	    (UNION . CLI:UNION)
	    (WARN . CLI:WARN))
	  )
  "Alist used as *READER-SYMBOL-SUBSTITUTIONS* for reading Common Lisp code.")

))

; From file FED.LISP PS:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "


(DEFUN DISPLAY-FONT (FONT &OPTIONAL (WINDOW TERMINAL-IO) (CLEAR-FIRST-P T) (FROM-FED NIL))
  "Display the contents of font FONT on WINDOW. 
CLEAR-FIRST-P says clear the window before displaying this. 
FROM-FED is T when called from FED, NIL when called from elsewhere."
  (WHEN (SYMBOLP FONT)
    (SETQ FONT (SEND (TV:SHEET-GET-SCREEN WINDOW) ':PARSE-FONT-DESCRIPTOR FONT)))
  (IF FONT
      (LET ((FONT-MAP (SEND WINDOW ':FONT-MAP))
	    (CURRENT-FONT (SEND WINDOW ':CURRENT-FONT)))
	(UNWIND-PROTECT
	    (PROG* ((NAME (FONT-NAME FONT))
		    (FD (FONT-GET-FD NAME))
		    (DF (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN WINDOW))))
		   (SEND WINDOW ':SET-FONT-MAP (LIST DF FONT))
		   (AND CLEAR-FIRST-P (FUNCALL WINDOW ':CLEAR-SCREEN))
		   (FORMAT WINDOW "~2&Font ~A:~&" NAME)
		   (COND ((> (+ (FONT-CHAR-HEIGHT FONT)
				(TV:SHEET-LINE-HEIGHT WINDOW))
			     (- (TV:SHEET-INSIDE-BOTTOM WINDOW)
				(TV:SHEET-LINE-HEIGHT WINDOW)))
			  (FORMAT WINDOW
				  "~& This font's characters are too big to display here!")
			  (RETURN NIL)))
		   (AND FROM-FED (FORMAT WINDOW "~&Mouse any character to edit it.~%"))
		   (DO ((CH 0) (OCH) (LEN (ARRAY-LENGTH FD))) (())
		     ;; Skip any groups of 32 characters that are all missing. 
		     (AND (ZEROP (\ CH 32.))
			  (DO ((CH1 CH (1+ CH1)))
			      (( CH1 LEN)
			       (SETQ CH CH1))
			    (IF (ZEROP (\ CH1 32.))
				(SETQ CH CH1))
			    (IF (OR (AREF FD CH1)
				    (FED-CHARACTER-BEING-EDITED-P NAME CH1))
				(RETURN))))
		     (IF ( CH LEN) (RETURN NIL))
		     ;; If there is not room for a line in the default font
		     ;;   followed by a line in the font being edited
		     ;;   before we would need to **more**,
		     ;;   then **more** right now, and go to top of window afterward. 
		     (COND (( (+ (TV:SHEET-CURSOR-Y WINDOW)
				  (FONT-CHAR-HEIGHT DF)
				  (FONT-CHAR-HEIGHT DF)
				  (FONT-CHAR-HEIGHT FONT))
			       (TV:SHEET-MORE-VPOS WINDOW))
			    (SETF (TV:SHEET-MORE-FLAG WINDOW) 1)
			    (SEND WINDOW ':HANDLE-EXCEPTIONS)
			    (SETF (TV:SHEET-END-PAGE-FLAG WINDOW) 1)))
		     (SEND WINDOW ':SET-CURSORPOS (TV:SHEET-INSIDE-LEFT WINDOW)
						  (+ (TV:SHEET-CURSOR-Y WINDOW)
						     (FONT-CHAR-HEIGHT DF)))
		     (TV:PREPARE-SHEET (WINDOW)
		       (SETQ OCH CH)
		       ;; Output one line of chars in the default font,
		       ;;  spaced so that they lie above the corresponding chars in the next
		       ;;  line. Stop at margin, or when we reach a char code that's a
		       ;;   multiple of 32. 
		       (DO ()
			   ((> (+ (TV:SHEET-CURSOR-X WINDOW)
				  (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
				       (FONT-CHARACTER-WIDTH DF CH)))
			       (TV:SHEET-INSIDE-RIGHT WINDOW)))
			 (SEND WINDOW ':SET-CURRENT-FONT DF)
			 (WHEN (OR (AREF FD CH)
				   (AND FROM-FED (FED-CHARACTER-BEING-EDITED-P NAME CH)))
			   (WHEN FROM-FED
			     (SEND WINDOW ':PRIMITIVE-ITEM
				   'CHARACTER CH
				   (- (TV:SHEET-CURSOR-X WINDOW)
				      (TV:SHEET-INSIDE-LEFT WINDOW))
				   (- (TV:SHEET-CURSOR-Y WINDOW)
				      (TV:SHEET-INSIDE-TOP WINDOW)
				      (- (FONT-BASELINE DF) (TV:SHEET-BASELINE WINDOW)))
				   (- (+ (TV:SHEET-CURSOR-X WINDOW)
					 (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
					      (FONT-CHARACTER-WIDTH DF CH)))
				      (TV:SHEET-INSIDE-LEFT WINDOW))
				   (+ (- (TV:SHEET-CURSOR-Y WINDOW)
					 (TV:SHEET-INSIDE-TOP WINDOW))
				      (TV:SHEET-LINE-HEIGHT WINDOW)
				      (FONT-CHAR-HEIGHT FONT) 0)))
			   (TV:SHEET-TYO WINDOW CH)
			   (SEND WINDOW ':INCREMENT-CURSORPOS
				 (- (FED-DISPLAY-FONT-CHAR-WIDTH FD DF CH)
				    (FONT-CHARACTER-WIDTH DF CH))
				 0))
			 (SETQ CH (1+ CH))
			 (AND (= CH LEN) (RETURN))
			 (AND (ZEROP (\ CH 32.)) (RETURN)))
		       (SEND WINDOW ':SET-CURSORPOS (TV:SHEET-INSIDE-LEFT WINDOW)
						    (+ (TV:SHEET-CURSOR-Y WINDOW)
						       (FONT-CHAR-HEIGHT FONT)))
		       ;; Clear out what we will move down over with SHEET-INCREMENT-BITPOS
		       (TV:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH WINDOW)
					   (FONT-CHAR-HEIGHT FONT)
					   (TV:SHEET-INSIDE-LEFT WINDOW)
					   (+ (TV:SHEET-CURSOR-Y WINDOW)
					      (TV:SHEET-LINE-HEIGHT WINDOW))
					   TV:ALU-ANDCA WINDOW)
		       ;; Now output the corresponding chars in the font being edited. 
		       ;;  First leave space so it won't overlap if font is taller. 
		       (SEND WINDOW ':INCREMENT-CURSORPOS 0 (- (FONT-BASELINE FONT)
							       (TV:SHEET-BASELINE WINDOW)))
		       (SEND WINDOW ':SET-CURRENT-FONT FONT)
		       (DO ()
			   ((> (+ (TV:SHEET-CURSOR-X WINDOW)
				  (FED-DISPLAY-FONT-CHAR-WIDTH FD DF OCH))
			       (TV:SHEET-INSIDE-RIGHT WINDOW)))
			 (COND ((OR (AREF FD OCH)
				    (FED-CHARACTER-BEING-EDITED-P NAME OCH))
				(FED-TYO WINDOW OCH NAME)
				(TV:SHEET-INCREMENT-BITPOS WINDOW
				  (- (FED-DISPLAY-FONT-CHAR-WIDTH FD DF OCH)
				     (FONT-CHARACTER-WIDTH FONT OCH))
				  0)))
			 (SETQ OCH (1+ OCH))
			 (AND (= OCH LEN) (RETURN))
			 (AND (ZEROP (\ OCH 32.)) (RETURN))))
		     (SEND WINDOW ':SET-CURSORPOS (TV:SHEET-INSIDE-LEFT WINDOW)
						  (+ (TV:SHEET-CURSOR-Y WINDOW)
						     (FONT-CHAR-HEIGHT DF)))))
	  (SEND WINDOW ':SET-FONT-MAP FONT-MAP)
	  (SEND WINDOW ':SET-CURRENT-FONT CURRENT-FONT)))
    (FORMAT WINDOW "~&~S is not a font." FONT))
  (VALUES))

))

; From file DISPLA.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "

(DEFUN VIEW-WINDOW (WINDOW &OPTIONAL STREAM RETURN-IF-NO-MORE
		    &AUX CH ATTRIBUTE-LIST FONTSP)
  "Display WINDOW, letting user scroll with Space and Overstrike, reading data from STREAM.
This is used for the editor View File, etc, commands.
STREAM may be a stream to read more data from, and get font names from,
or may be T, meaning do not alter the fonts set up in WINDOW already.
RETURN-IF-NO-MORE non-NIL means exit immediately on attempt
to scroll past the end of the data."
  (IF (EQ STREAM T)
      (SETQ STREAM NIL)
    (AND STREAM (SETQ ATTRIBUTE-LIST (FS:EXTRACT-ATTRIBUTE-LIST STREAM)))
    (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
				       (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':BACKSPACE))
    (REDEFINE-WINDOW-TAB-NCHARS WINDOW (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':TAB-WIDTH))
    (SETQ FONTSP (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':FONTS))
    (REDEFINE-FONTS WINDOW
		    (AND STREAM
			 (SET-BUFFER-FONTS (WINDOW-INTERVAL WINDOW) FONTSP))
		    (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':VSP)))
  (UNLESS (CLI:LISTP FONTSP) (SETQ FONTSP (LIST FONTSP)))
  (SETQ FONTSP (OR (CDR FONTSP)
		   (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':DIAGRAM)))
  (DO ((N-LINES (1- (WINDOW-N-PLINES WINDOW)))
       (FIRST-P T NIL)
       (AT-END-P))
      (NIL)
    (MULTIPLE-VALUE (AT-END-P STREAM)
      (VIEW-WINDOW-DISPLAY WINDOW STREAM FIRST-P FONTSP))
    (AND FIRST-P RETURN-IF-NO-MORE AT-END-P (RETURN NIL))
    (SELECTOR (SETQ CH (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
			 (FUNCALL STANDARD-INPUT ':TYI)))
	      EQUAL
      ((#\SP #\C-V #\HAND-DOWN)
       (CONDITION-CASE ()
	   (RECENTER-WINDOW-RELATIVE WINDOW N-LINES)
	 (BARF NIL)))
      ((#\BS #\M-V #\HAND-UP)
       (RECENTER-WINDOW-RELATIVE WINDOW (- N-LINES)))
      (#\C-SPACE
       (KBD-SCROLL WINDOW
		   #'(LAMBDA (IGNORE N-LINES STREAM WINDOW)
		       (REDISPLAY WINDOW ':RELATIVE N-LINES)
		       (VIEW-WINDOW-DISPLAY WINDOW STREAM NIL FONTSP))
		   STREAM WINDOW))
      (#\HELP
       (FORMAT QUERY-IO "~&~:@C or ~:@C - scroll forward screen. ~@:C or ~:@C - scroll backward screen.
~:@C enters mode where Control scrolls slowly forward and Meta scrolls back.
~:@C or ~:@C - exit.  Anything else exits and is executed as a command."
	       #\SPACE #\C-V #\OVERSTRIKE #\M-V #\C-SPACE #\RUBOUT #\ABORT))
      (OTHERWISE
       (UNLESS (LISTP CH)
	 (OR (MEMQ CH '(#\RUBOUT #\ABORT))
	     (FUNCALL STANDARD-INPUT ':UNTYI CH))
	 (RETURN NIL)))))
  (VALUES (COPY-BP (WINDOW-POINT WINDOW)) CH))

))

; From file DISPLA.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "

(DEFUN VIEW-WINDOW-DISPLAY (WINDOW STREAM &OPTIONAL FORCE-P FONTS-P
			    &AUX AT-END-P N-PLINES SHEET
			    LAST-BP PLINE X Y Y-POS)
  (LET ((*WINDOW* WINDOW))
    (SEND *WINDOW* ':SET-BASE-TICK *TICK*)
    (SETQ N-PLINES (WINDOW-N-PLINES WINDOW)
	  LAST-BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
    (AND STREAM (SETQ PLINE (PLINE-OF-POINT NIL WINDOW LAST-BP))
	 (LET ((ISTREAM (INTERVAL-STREAM-INTO-BP LAST-BP FONTS-P)))
	   (DO ((I PLINE (1+ I))
;		(AT-LINE (BP-LINE LAST-BP))
		(LINE) (EOF))
	       (( I N-PLINES))
	     (MULTIPLE-VALUE (LINE EOF)
	       (FUNCALL STREAM ':LINE-IN LINE-LEADER-SIZE))
	     (AND LINE (SEND ISTREAM ':LINE-OUT LINE))
	     (AND EOF (RETURN (SETQ AT-END-P T
				    STREAM NIL))))))
    (MUST-REDISPLAY WINDOW DIS-TEXT)
    (REDISPLAY WINDOW ':POINT NIL NIL FORCE-P)
    (OR STREAM AT-END-P
	(SETQ AT-END-P (FIND-BP-IN-WINDOW WINDOW LAST-BP)))
    (SETQ SHEET WINDOW
	  Y (* N-PLINES (TV:SHEET-LINE-HEIGHT SHEET)))
;  (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET)	;Erase anything left over
;		       (MULTIPLE-VALUE-BIND (NIL HEIGHT)
;			   (FUNCALL SHEET ':LABEL-SIZE)
;			 (- (+ (TV:SHEET-INSIDE-BOTTOM SHEET) HEIGHT) Y))
;		       (TV:SHEET-INSIDE-LEFT SHEET) Y (TV:SHEET-ERASE-ALUF SHEET) SHEET)
    (BIND (LOCF (TV:SHEET-BOTTOM-MARGIN-SIZE SHEET)) 0)
    (AND AT-END-P
	 (MULTIPLE-VALUE (X Y-POS)
	   (FIND-BP-IN-WINDOW-COORDS LAST-BP WINDOW)))
    (COND ((OR (NOT AT-END-P) (NULL Y-POS))
	   (TV:SHEET-LINE-OUT SHEET "--More--" 0 NIL 0 Y)
	   (MULTIPLE-VALUE (X Y-POS)
	     (TV:SHEET-READ-CURSORPOS SHEET)))
	  (AT-END-P (TV:SHEET-LINE-OUT SHEET "--Bottom--" 0 NIL 0 Y)))
    (LET ((BLINKER (WINDOW-POINT-BLINKER WINDOW)))
      (TV:BLINKER-SET-CURSORPOS BLINKER X Y-POS)
      (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK))
    (VALUES AT-END-P STREAM)))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN MEXP (&OPTIONAL FORM &AUX EXP)
  "Read-macroexpand-print loop, for seeing how macros expand.
MEXP reads s-expressions and macroexpands each one, printing the expansion.
Type NIL to exit (or Abort)."
    (DO-FOREVER
      (UNLESS FORM
	(FORMAT T "~2%Macro form ")
	(FUNCALL STANDARD-INPUT ':UNTYI (FUNCALL STANDARD-INPUT ':TYI)));Allow abort to exit
      (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to MEXP input loop.")
	(SETQ EXP (OR FORM (READ-FOR-TOP-LEVEL)))
	(AND (SYMBOLP EXP) (RETURN NIL))
	(DO ((LAST NIL EXP))
	    ((EQ EXP LAST))
	  (SETQ EXP (MACROEXPAND-1 EXP))
	  (PRINC "  ")
	  (GRIND-TOP-LEVEL EXP))
	(UNLESS (EQUAL EXP (SETQ EXP (MACROEXPAND-ALL EXP)))
	  (PRINC "  ")
	  (GRIND-TOP-LEVEL EXP)))
      (WHEN FORM (RETURN '*))))

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN LAMBDA-EXP-ARGS-AND-BODY (LAMBDA-EXP)
  "Return a list containing the arglist and body of LAMBDA-EXP.
This is a list whose car is the arglist and whose cdr is the body."
  (IF (MEMQ (CAR LAMBDA-EXP) '(NAMED-LAMBDA NAMED-SUBST CLI:NAMED-LAMBDA CLI:NAMED-SUBST))
      (CDDR LAMBDA-EXP)
    (CDR LAMBDA-EXP)))

))

(setf (documentation 'digit-char-p 'function)
  "Weight of CHAR as a digit, if it is a digit in radix RADIX; else NIL.
The weights of #\0 through #\9 are 0 through 9;
the weights of letters start at ten for A.
RADIX does not affect the weight of any digit,
but it affects whether NIL is returned.")


; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFMACRO-DISPLACE PKG-BIND (PKG &BODY BODY)
  "Executes BODY with PKG as current package.  PKG is a package or the name of one."
  (IF (EQUAL PKG "USER")
      `(LET ((*PACKAGE* PKG-USER-PACKAGE))	;Optimize most common case.
	 . ,BODY)
    `(LET ((*PACKAGE* (PKG-FIND-PACKAGE ,PKG)))
       . ,BODY)))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN ALTER-PACKAGE (NAME &KEY &OPTIONAL NICKNAMES
		      (USE '("GLOBAL")) ((:SIZE IGNORE))
		      SHADOW EXPORT PREFIX-NAME AUTO-EXPORT-P
		      IMPORT SHADOWING-IMPORT IMPORT-FROM
		      RELATIVE-NAMES RELATIVE-NAMES-FOR-ME
		      ((:HASH-INHERITED-SYMBOLS IGNORE))
		      (EXTERNAL-ONLY NIL EXTERNAL-ONLY-P)
		      ((:INCLUDE IGNORE)) ((:COLON-MODE IGNORE)))
  (LET ((PKG (FIND-PACKAGE NAME)))
    (UNLESS PKG (FERROR NIL "Package ~A not found." NAME))
    (RENAME-PACKAGE PKG (PKG-NAME PKG) NICKNAMES)
    (UNLESS (OR (NULL PREFIX-NAME) (STRING= PREFIX-NAME NAME)
		(MEM 'STRING= PREFIX-NAME NICKNAMES))
      (FERROR NIL "The prefix name ~A is not a name or nickname of the package." PREFIX-NAME))
    (SETF (PKG-PREFIX-PRINT-NAME PKG) PREFIX-NAME)
    (SHADOW SHADOW PKG)
    (SHADOWING-IMPORT SHADOWING-IMPORT PKG)
    (EXPORT EXPORT PKG)
    (LET ((DESIRED-USE (IF (OR (LISTP USE) (NULL USE))
			   (MAPCAR 'FIND-PACKAGE USE)
			 (LIST (FIND-PACKAGE USE)))))
      (DOLIST (ELT (PKG-USE-LIST PKG))
	(UNLESS (MEMQ ELT DESIRED-USE)
	  (UNUSE-PACKAGE ELT PKG)))
      (USE-PACKAGE DESIRED-USE PKG))
    (IMPORT IMPORT PKG)
    (WHEN IMPORT-FROM
      (DOLIST (NAME (CDR IMPORT-FROM))
	(IMPORT (INTERN (STRING NAME) (CAR IMPORT-FROM)) PKG)))
    (IF (IF EXTERNAL-ONLY-P EXTERNAL-ONLY AUTO-EXPORT-P)
	(PKG-MARK-HAVING-SUBPACKAGES PKG)
      (SETF (PKG-AUTO-EXPORT-P PKG) NIL)
      (SETF (PKG-STORE-FUNCTION PKG) NIL))
    (SETF (PKG-REFNAME-ALIST PKG)
	  (LOOP FOR (NICK . P) IN RELATIVE-NAMES
		COLLECT (CONS (STRING NICK)
			      (FIND-PACKAGE P))))
    ;; First delete any other local nicknames, in any package, for this one.
    (DOLIST (P *ALL-PACKAGES*)
      (SETF (PKG-REFNAME-ALIST P)
	    (CLI:DELETE PKG (PKG-REFNAME-ALIST P) ':KEY 'CDR)))
    ;; Then add the ones that are requested.
    (DOLIST (ELT RELATIVE-NAMES-FOR-ME)
      (PKG-ADD-RELATIVE-NAME (CAR ELT) (CADR ELT) PKG))
    PKG))

))


; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN PKG-GOTO (&OPTIONAL (PKG PKG-USER-PACKAGE) GLOBALLY)	;Go to type-in package.
  "Set the current binding of *PACKAGE* to the package you specify (by name).
If GLOBALLY is non-NIL, then we do a PKG-GOTO-GLOBALLY as well."
  (LET ((PK (PKG-FIND-PACKAGE PKG)))
    (WHEN (PKG-AUTO-EXPORT-P PK)
      (FERROR NIL "Package ~A is auto-exporting; it should not be the current package." PK))
    (AND GLOBALLY (PKG-GOTO-GLOBALLY PK))
    (SETQ *PACKAGE* PK)))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN PKG-GOTO-GLOBALLY (&OPTIONAL (PKG PKG-USER-PACKAGE))
  "Set the global binding of *PACKAGE* used by new lisp listeners
and by random processes that don't bind *PACKAGE*."
  (LET ((*PACKAGE* *PACKAGE*))			;do error check
    (SETQ PKG (PKG-GOTO PKG)))
  (SETQ-GLOBALLY *PACKAGE* PKG))

))

;(unuse-package "GLOBAL" "FONTS")		;The Right Thing, but unfortunately
						; it breaks existing user fonts. Foo. sys99.
(setf (si:pkg-auto-export-p (pkg-find-package "CLI")) t)
;;;don't even look at this
(make-package "Package Read Lock Kludge" :invisible t :use "CLI" :size 0)
;;;ok. open your eyes again.
(setf (si:pkg-nicknames (pkg-find-package "EH")) '("DBG" "DEBUGGER"))
(unintern "NAMED-LAMDBA" "CLI")			;where did this typo come from?
(unintern "DEFUN" "CLI")			;cold-load bug
(unintern "DEFSUBST" "CLI")
(unintern "MACRO" "CLI")
(unintern "&ENVIRONMENT" "USER")
(globalize "DEFPACKAGE" "GLOBAL")


; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFCONST INITIAL-PACKAGES
  '(("GLOBAL" :NICKNAMES ("LISP") :SIZE 4000 :USE NIL)
    ("KEYWORD" :NICKNAMES ("") :SIZE 3000. :USE NIL)
    ("SYSTEM" :NICKNAMES ("SYS") :SIZE 3000)
    ("CADR" :SIZE 7000. :USE ("GLOBAL" "SYS") :NICKNAMES ("CC"))
    ("CHAOS" :SIZE 1200. :USE ("GLOBAL" "SYS")
     :SHADOW ("OPEN" "STATUS" "CLOSE" "LISTEN" "FINISH"))
    ("COLOR" :SIZE 400 :USE ("GLOBAL" "SYS"))
    ("COMPILER" :SIZE 2800. :USE ("GLOBAL" "SYS"))
    ("FILE-SYSTEM" :SIZE 1800. :USE ("GLOBAL" "SYS") :NICKNAMES ("FS") :PREFIX-NAME "FS")
    ("QFASL-REL" :SIZE 300. :USE ("GLOBAL" "SYS")
     :SHADOW ("READ-BYTE" "WRITE-BYTE" "WRITE-STRING"))
    ("METER" :SIZE 300. :USE ("GLOBAL" "SYS"))
    ("TV" :SIZE 4000. :USE ("GLOBAL" "SYS"))
    ("EH" :SIZE 1200. :USE ("GLOBAL" "SYS") :NICKNAMES ("DBG" "DEBUGGER")
     :SHADOW ("ARG"))
    ("FED" :SIZE 1000. :USE ("GLOBAL" "SYS"))
    ("SYSTEM-INTERNALS" :SIZE 7000. :USE ("GLOBAL" "SYS") :NICKNAMES ("SI")
     :PREFIX-NAME "SI")
    ("FONTS" :SIZE 200 :AUTO-EXPORT-P T :USE "GLOBAL")
    ("TIME" :SIZE 1400)
    ("SUPDUP" :SIZE 600.)
    ("PRESS" :SIZE 500.)
    ("FORMAT" :SIZE 400.)
    ("ZWEI" :SIZE 7000. :SHADOW ("SEARCH" "FIND"))
    ("MICRO-ASSEMBLER" :SIZE 6000. :NICKNAMES ("UA") :PREFIX-NAME "UA"
     :SHADOW ("FIXNUM" "INCLUDE" "MERGE" "AREA-LIST"))
    ("MATH" :SIZE 200.)
    ("HACKS" :SIZE 2000.)
    ("SRCCOM" :SIZE 100. :SHADOW ("FILE-LENGTH"))
    ("USER" :SIZE 2000.)
    ("COMMON-LISP-INCOMPATIBLE" :NICKNAMES ("CLI") :PREFIX-NAME "CLI"
				:USE NIL :AUTO-EXPORT-P T)
    ("Package Read Lock Kludge" :INVISIBLE T :USE "CLI" :SIZE 0))
  "List of specifications of all the packages in the system.
Each element is an argument list to which MAKE-PACKAGE is applied.")

))

; From file FORMAT.LISP PS:<MLY.L> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; FORMAT  "

(DEFVAR FORMAT-CLAUSES-ARRAY NIL)	;Internal "pseudo-resource"s
(DEFVAR FORMAT-STACK-ARRAY NIL)

))


; From file FORMAT.LISP PS:<MLY.L> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; FORMAT  "

(defun format-parse-clauses (closechar semip &aux (start (+ 3 ctl-index)))
  (let ((clauses (let ((tem format-clauses-array))
		   (if (and tem (%store-conditional (locf format-clauses-array) tem nil))
		       tem
		     (make-array 30. ':area format-temporary-area ':type art-q-list
				     ':fill-pointer 0))))
	(stack (let ((tem format-stack-array))
		 (if (and tem (%store-conditional (locf format-stack-array) tem nil))
		     tem
		   (make-array 10. ':area format-temporary-area ':type art-q-list
				   ':fill-pointer 0))))
	i j tem atsign-flag colon-flag command)
    (setf (fill-pointer clauses) 0 (fill-pointer stack) 0)
    (setq i ctl-index)
    (do-forever
      (unless (setq ctl-index (%string-search-char #/~ ctl-string ctl-index ctl-length))
	(ferror nil
		"Missing ~{~*~~~A and ~} ~~~A in format string:~%~{~VT~*~}~VT~%~3@T/"~A/"~%"
		(g-l-p stack) closechar (g-l-p stack) start ctl-string))
      (setq j ctl-index)
      (setq atsign-flag nil colon-flag nil)
      (let ((format-params (allocate-resource 'format-params)))
	(setf (fill-pointer format-params) 0)
	(setq command (format-parse-command nil nil))
	;; Now I points to beginning of clause, J to ~, and CTL-INDEX after command.
	(cond ((setq tem (get command 'format-matching-delimiter))
	       (vector-push-extend start stack)
	       (vector-push-extend closechar stack)
	       (setq closechar tem start (+ 3 ctl-index)))
	      ((< (fill-pointer stack) 2)			;at top level
	       (when (or (eq command closechar) (and (eq command '/;) semip))
		 (vector-push-extend (nsubstring ctl-string i j format-temporary-area)
				     clauses)
		 (vector-push-extend (+ (if colon-flag 1 0) (if atsign-flag 2 0))
				     clauses)
		 (vector-push-extend (when (g-l-p format-params)
				       (prog1 format-params (setq format-params nil)))
				     clauses)
		 (setq i ctl-index)
		 (when (eq command closechar)
		   (unless (%store-conditional (locf format-stack-array) nil stack)
		     (return-array stack))
		   (when format-params (deallocate-resource 'format-params format-params))
		   (return clauses))))
	      ((eq command closechar)				;pop off a level
	       (setq closechar (vector-pop stack))
	       (setq start (vector-pop stack))))
	;; Unless the parameters were saved away in the clauses table, free them
	(if format-params (deallocate-resource 'format-params format-params))))))

))

; From file FORMAT.LISP PS:<MLY.L> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; FORMAT  "

(defun format-reclaim-clauses (clauses)
       (do ((i (fill-pointer clauses) (- i 3)))
	   ((= i 0)
	    (unless (%store-conditional (locf format-clauses-array) nil clauses)
	      (return-array clauses)))
	 (return-array (aref clauses (- i 3)))
	 (and (aref clauses (1- i))
	      (deallocate-resource 'format-params (aref clauses (1- i))))))

))

; From file FORMAT.LISP PS:<MLY.L> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; FORMAT  "

(defprop /( /) format-matching-delimiter)
(defprop   format-matching-delimiter)
(defprop [ ] format-matching-delimiter)
(defprop { } format-matching-delimiter)
(defprop < > format-matching-delimiter)

))

(setf (documentation 'format 'function)
  "Format arguments according to a control string and print to a stream.
/(If the stream is T, STANDARD-OUTPUT is used;
 if NIL, a string is returned containing the formatted text.)
The control string is copied to the stream, but ~ indicates special formatting commands.
Note commands X, E, F, G are incompatible between this FORMAT and Common Lisp FORMAT.
~D  ~mincol,padchar,commacharD   Print number as a decimal integer.
    ~:D  Print the comma character every three digits.
    ~@D  Always print the sign.   ~:@D  Both.
~O  Analogous to ~D, but prints in octal.
~B  Analogous to ~B, but prints in binary.
~F  ~F  Print a floating point number.   ~nF  Round it to n digits.
~E  ~E  Print a floating-point number in exponential notation.   ~nE  Round to n digits.
~$  ~w,x,y,z$ prints a floating-point number with exactly w (default 2) digits to right of
     decimal, at least x (default 1) to left of decimal, right-justified in field y wide
     padded with z.  @ print + sign.  : sign to left of padding.
~R  ~R  Print number as an English cardinal number.
    ~:R  English ordinal number.   ~@R  Roman numeral.   ~:@R  Old Roman numeral.
    ~nR  Print number in radix n.  Thus ~8R = ~O, and ~10R = ~D.
    Extra parameters are as for ~D (~n,mincol,padchar,commacharR).
~A  Ascii output (PRINC).  Good for printing strings.  ~mincol,colinc,minpad,padcharA.
    ~@A  Right-justify the string.   ~:A  Make NIL print as ().  ~:@A  Both.
~S  Analogous to ~A, but uses PRIN1, not PRINC.
~C  Print a character.  Mouse characters print in standard format.
    ~C  Actual character, preceded by /"c-/", /"m-/", /"s-/" or /"h-/" if necessary.
    ~:C  Format effectors print as names.  Names of control bits (/"Control-/") precede.
    ~@C  Prints the character in READ format, using #// or #\.
    ~:@C  Like ~:C, but top/front/greek characters are followed by remark, e.g. /" (Top-S)/".
~*  Ignore an argument.   ~n*  Ignore n arguments.   ~:n*  Back up n arguments (default 1).
~G  Goto.  ~nG goes to the nth argument (0-origin).  Operates relative to ~{...~} lists.
~%  Insert a newline.     ~n%  Insert n newlines.
~X  Insert a space.       ~nX  Insert n spaces.
~~  Insert a tilde.       ~n~  Insert n tildes.
~|  Insert a form.        ~n|  Insert n forms.
    ~:|  Do :CLEAR-SCREEN if the stream supports it, otherwise insert a form.   ~:n|  Similar.
~<cr>  Ignore a CR and following whitespace in the control string.
    ~:<cr> Ignore the CR, retain the whitespace.  ~@<cr> Retain the CR, ignore the whitespace.
~&  Do a :FRESH-LINE.     ~n&  Do a FRESH-LINE, then insert n-1 newlines.
~^  Terminate processing if no more arguments.  Within ~{...~}, just terminate the loop.
    ~n;  Terminate if n is zero.  ~n,m;  Terminate if n=m.  ~n,m,p;  Terminate if nmp.
    ~:^  When within ~:{...~}, ~^ terminates this iteration.  Use ~:^ to exit the loop.
~T  ~mincol,colincT  Tab to column mincol+p*colinc, for the smallest integer p possible.
    ~mincol,colinc:T  Same, but tabs in TV pixels rather than characters.
    ~n@T  Insert n spaces.
    ~n,colinc@T   Insert n spaces, then move 0 or more up to multiple of colinc.
~Q  Apply next argument to no arguments.  ~a,b,c,...,zQ  Apply next argument to parameters
    a,b,c,...z.  In (Q ...) form, apply argument to unevaled parameters.
~P  Pluralize.  Insert /"s/", unless argument is 1.
    ~:P  Use previous argument, not next one (i.e. do ~:* first).
    ~@P  Insert /"y/" if argument is 1, otherwise insert /"ies/".   ~:@P  Both.
~(  ~(...~)  Force lower case for the output generated within.
    ~:(...~)  Similar but capitalize each word.
    ~@(...~)  Similar but capitalize the first word.
    ~:@(...~)  Similar but force all upper case.
~?  Indirect.  Uses up two args; first is a format string, second is args for it.
~<  ~mincol,colinc,minpad,padchar<str0~;str1~;...~;strn~>  Do formatting for all formatting
    strings strj; then output all strings with padding between them at the ~; points.
    Each padding point must have at least minpad padding characters.  Subject to that,
    the total width must be at least mincol, and must be mincol+p*colinc for some p.
    If str0 is followed by ~:; instead of ~;, then str0 is not normally output, and the
    ~:; is not a padding point.  Instead, after the total width has been determined,
    if the text will not fit into the current line of output, then str0 is output before
    outputting the rest.  (Doesn't work when producing a string.)  An argument n (~:n;)
    means that the text plus n more columns must fit to avoid outputting str0.  A second
    argument m (~n,m:;) provides the line width to use instead of the stream's width.
    ~:<  Also have a padding point at the left.  Hence ~n:<x~> right-justifies x in n columns.
    ~@<  Also have a padding point at the right.   ~:@<  Both.   Hence ~n:@<x~> centers x.
~[  ~[str0~;str1~;...~;strn~]  Select.  Argument selects one clause to do.  If argument is not
    between 0 and n inclusive, then no alternative is performed.  If a parameter is given,
    then use the parameter instead of an argument.  (The only useful one is /"#/".)
    If the last string is preceded by ~:;, it is an /"else/" clause, and is processed if
    no other string is selected.
    One can also tag the clauses explicitly by giving arguments to ~;.  In this case the
    first string must be null, and arguments to ~; tag the following string.  The
    argument is matched against the list of parameters for each ~;.  One can get ranges
    of tags by using ~:;.  Pairs of parameters serve as inclusive range limits.
    A ~:; with no parameters is still an /"else/" clause.
    Example:  ~[~'+,'-,'*,'////;operator~:'A,'Z,'a,'z;letter~:'0,'9;digit~:;other~]
    will produce /"operator/", /"letter/", /"digit/", or /"other/" as appropriate.
    ~:[iffalse~;iftrue~]  The argument selects the first clause if nil, the second if non-nil.
    ~@[str~]  If the argument is non-nil, then it is not swallowed, and str is processed.
    Otherwise, the nil is swallowed and str is ignored.  Thus ~@[~S~] will PRIN1 a
    non-null thing.
~{  ~{str~}  Use str as a format string for each element in the argument.  More generally,
    the argument is a list of things to be used as successive arguments, and str is used
    repeatedly as a format string until the arguments are exhausted (or ~^ is used).
    Within the iteration the commands ~* and ~G move among the iteration arguments,
    not among all the arguments given to FORMAT.
    ~n{str~} repeats the string at most n times.
    Terminating with ~:} forces str to be processed at least once.
    ~:{str}  The argument is a list of lists, and each repetition sees one sublist.
    ~@{str}  All remaining arguments are used as the list.
    ~:@{str}  Each remaining argument is a list.
    If the str within a ~{ is empty, then an argument (which must be a string) is used.
    This argument precedes any that are iterated over as loop arguments.
~  ~str~ Successive lines within str are indented to align themselves with the column
    at which str began. ie all text within str will lie to the right of the beginning of str
In place of a numeric parameter, one may use V, which uses an argument to supply the number;
or one may use #, which represents the number of arguments remaining to be processed;
or one may use 'x, which uses the ascii value of x (good for pad characters).
The control string may actually be a list of intermixed strings and sublists.
In that case, the strings are printed literally.  The first atom in a sublist should be
the name of a command, and remaining elements are parameters.")

(setf (documentation 'cli:format 'function)
  "Format arguments according to a control string and print to a stream; Common Lisp version.
/(If the stream is T, STANDARD-OUTPUT is used;
 if NIL, a string is returned containing the formatted text.)
The control string is copied to the stream, but ~ indicates special formatting commands.
Note commands X, E, F, G are incompatible between Common Lisp FORMAT and regular FORMAT.
~D  ~mincol,padchar,commacharD   Print number as a decimal integer.
    ~:D  Print the comma character every three digits.
    ~@D  Always print the sign.   ~:@D  Both.
~O  Analogous to ~D, but prints in octal.
~X  Analogous to ~D, but prints in hex.
~B  Analogous to ~B, but prints in binary.
~F  ~w,d,s,overflowchar,padcharF  Print float in nonexponential notation.
    Multiplies by 10^s before printing if s is specified.
    Prints in w positions, with d digits after the decimal point.
    Pads on left with padchar if nec.  If number doesn't fit in w positions,
    and overflowchar is specified, just fills the w positions with that character.
~E  ~w,d,e,s,overflowchar,padchar,exptcharE   Print float in exponential notation.
    Prints in w positions, with e digits of exponent.
    If s (default is 1) is positive, prints s digits before point, d-s+1 after.
    If s is zero, prints d digits after the point, and a zero before if there's room.
    If s is negative, prints d digits after the point, of which the first -s are zeros.
    If exptchar is specified, it is used to delimit the exponent
    (instead of /"e/" or whatever.)
    If overflowchar is specified, then if number doesn't fit in specified width,
    or if exponent doesn't fit in e positions, field is filled with overflowchar instead.
~G  Like ~E, but if number fits without exponent, prints without one.
~$  ~w,x,y,z$ prints a floating-point number with exactly w (default 2) digits to right of
     decimal, at least x (default 1) to left of decimal, right-justified in field y wide
     padded with z.  @ print + sign.  : sign to left of padding.
~R  ~R  Print number as an English cardinal number.
    ~:R  English ordinal number.   ~@R  Roman numeral.   ~:@R  Old Roman numeral.
    ~nR  Print number in radix n.  Thus ~8R = ~O, and ~10R = ~D.
    Extra parameters are as for ~D (~n,mincol,padchar,commacharR).
~A  Ascii output (PRINC).  Good for printing strings.  ~mincol,colinc,minpad,padcharA.
    ~@A  Right-justify the string.   ~:A  Make NIL print as ().  ~:@A  Both.
~S  Analogous to ~A, but uses PRIN1, not PRINC.
~C  Print a character.  Mouse characters print in standard format.
    ~C  Actual character, preceded by /"c-/", /"m-/", /"s-/" or /"h-/" if necessary.
    ~:C  Format effectors print as names.  Names of control bits (/"Control-/") precede.
    ~@C  Prints the character in READ format, using #\ or #//.
    ~:@C  Like ~:C, but top/front/greek characters are followed by remark, e.g. /" (Top-S)/".
~*  Ignore an argument.   ~n*  Ignore n arguments.   ~:n*  Back up n arguments (default 1).
    ~n@* goes to the nth argument (0-origin).  Operates relative to ~{...~} lists.
~%  Insert a newline.     ~n%  Insert n newlines.
~~  Insert a tilde.       ~n~  Insert n tildes.
~|  Insert a form.        ~n|  Insert n forms.
    ~:|  Do :CLEAR-SCREEN if the stream supports it, otherwise insert a form.   ~:n|  Similar.
~<cr>  Ignore a CR and following whitespace in the control string.
    ~:<cr> Ignore the CR, retain the whitespace.  ~@<cr> Retain the CR, ignore the whitespace.
~&  Do a :FRESH-LINE.     ~n&  Do a FRESH-LINE, then insert n-1 newlines.
~^  Terminate processing if no more arguments.  Within ~{...~}, just terminate the loop.
    ~n;  Terminate if n is zero.  ~n,m;  Terminate if n=m.  ~n,m,p;  Terminate if nmp.
    ~:^  When within ~:{...~}, ~^ terminates this iteration.  Use ~:^ to exit the loop.
~T  ~mincol,colincT  Tab to column mincol+p*colinc, for the smallest possible integer p > 0.
    ~mincol,colinc:T  Same, but tabs in TV pixels rather than characters.
    ~n@T  Insert n spaces.
    ~n,colinc@T   Insert n spaces, then move 0 or more up to multiple of colinc.
~Q  Apply next argument to no arguments.  ~a,b,c,...,zQ  Apply next argument to parameters
    a,b,c,...z.  In (Q ...) form, apply argument to unevaled parameters.
~P  Pluralize.  Insert /"s/", unless argument is 1.
    ~:P  Use previous argument, not next one (i.e. do ~:* first).
    ~@P  Insert /"y/" if argument is 1, otherwise insert /"ies/".   ~:@P  Both.
~(  ~(...~)  Force lower case for the output generated within.
    ~:(...~)  Similar but capitalize each word.
    ~@(...~)  Similar but capitalize the first word.
    ~:@(...~)  Similar but force all upper case.
~?  Indirect.  Uses up two args; first is a format string, second is args for it.
~<  ~mincol,colinc,minpad,padchar<str0~;str1~;...~;strn~>  Do formatting for all formatting
    strings strj; then output all strings with padding between them at the ~; points.
    Each padding point must have at least minpad padding characters.  Subject to that,
    the total width must be at least mincol, and must be mincol+p*colinc for some p.
    If str0 is followed by ~:; instead of ~;, then str0 is not normally output, and the
    ~:; is not a padding point.  Instead, after the total width has been determined,
    if the text will not fit into the current line of output, then str0 is output before
    outputting the rest.  (Doesn't work when producing a string.)  An argument n (~:n;)
    means that the text plus n more columns must fit to avoid outputting str0.  A second
    argument m (~n,m:;) provides the line width to use instead of the stream's width.
    ~:<  Also have a padding point at the left.  Hence ~n:<x~> right-justifies x in n columns.
    ~@<  Also have a padding point at the right.   ~:@<  Both.   Hence ~n:@<x~> centers x.
~[  ~[str0~;str1~;...~;strn~]  Select.  Argument selects one clause to do.  If argument is not
    between 0 and n inclusive, then no alternative is performed.  If a parameter is given,
    then use the parameter instead of an argument.  (The only useful one is /"#/".)
    If the last string is preceded by ~:;, it is an /"else/" clause, and is processed if
    no other string is selected.
    One can also tag the clauses explicitly by giving arguments to ~;.  In this case the
    first string must be null, and arguments to ~; tag the following string.  The
    argument is matched against the list of parameters for each ~;.  One can get ranges
    of tags by using ~:;.  Pairs of parameters serve as inclusive range limits.
    A ~:; with no parameters is still an /"else/" clause.
    Example:  ~[~'+,'-,'*,'//;operator~:'A,'Z,'a,'z;letter~:'0,'9;digit~:;other~]
    will produce /"operator/", /"letter/", /"digit/", or /"other/" as appropriate.
    ~:[iffalse~;iftrue~]  The argument selects the first clause if nil, the second if non-nil.
    ~@[str~]  If the argument is non-nil, then it is not swallowed, and str is processed.
    Otherwise, the nil is swallowed and str is ignored.  Thus ~@[~S~] will PRIN1 a
    non-null thing.
~{  ~{str~}  Use str as a format string for each element in the argument.  More generally,
    the argument is a list of things to be used as successive arguments, and str is used
    repeatedly as a format string until the arguments are exhausted (or ~^ is used).
    Within the iteration the commands ~* and ~G move among the iteration arguments,
    not among all the arguments given to FORMAT.
    ~n{str~} repeats the string at most n times.
    Terminating with ~:} forces str to be processed at least once.
    ~:{str}  The argument is a list of lists, and each repetition sees one sublist.
    ~@{str}  All remaining arguments are used as the list.
    ~:@{str}  Each remaining argument is a list.
    If the str within a ~{ is empty, then an argument (which must be a string) is used.
    This argument precedes any that are iterated over as loop arguments.
~  ~str~ Successive lines within str are indented to align themselves with the column
    at which str began. ie all text within str will lie to the right of the beginning of str
In place of a numeric parameter, one may use V, which uses an argument to supply the number;
or one may use #, which represents the number of arguments remaining to be processed;
or one may use 'x, which uses the ascii value of x (good for pad characters).
The control string may actually be a list of intermixed strings and sublists.
In that case, the strings are printed literally.  The first atom in a sublist should be
the name of a command, and remaining elements are parameters.")

; From file FORMAT.LISP PS:<MLY.L> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; FORMAT  "

(DEFUN FORMAT-PARSE-COMMAND (ARGS SWALLOW-ARGS)
  (DO ((PARAM-FLAG NIL)		;If T, a parameter has been started in PARAM
       (START CTL-INDEX)	;for error message
       (CH)
       (TEM)
       (SYM)
       (SIGN NIL)		;Sign of parameter currently being constructed.
       (PARAM NIL))		;PARAM is the parameter currently being constructed
      (( (SETQ CTL-INDEX (1+ CTL-INDEX)) CTL-LENGTH)
       (SETQ CTL-INDEX (1+ START))
       (FORMAT-ERROR "Command fell off end of control string"))
    (SETQ CH (CHAR-UPCASE (AREF CTL-STRING CTL-INDEX)))
    (COND ((SETQ TEM (DIGIT-CHAR-P CH))
	   (SETQ PARAM (+ (* (OR PARAM 0) 10.) TEM)
		 PARAM-FLAG T))
	  ((= CH #/-)
	   (SETQ SIGN (NOT SIGN)))
	  ((= CH #/+) NIL)
	  ((= CH #/@)
	   (SETQ ATSIGN-FLAG T))
	  ((= CH #/:)
	   (SETQ COLON-FLAG T))
	  ((= CH #/V)
	   (COND ((AND (NULL ARGS) SWALLOW-ARGS)
		  (SETQ CTL-INDEX (1+ CTL-INDEX))
		  (FORMAT-ERROR "No argument for V parameter to use")))
	   (SETQ PARAM (POP ARGS) PARAM-FLAG T))
	  ((= CH #/#)
	   (SETQ PARAM (LENGTH ARGS) PARAM-FLAG T))
	  ((= CH #/')
	   (SETQ PARAM (AREF CTL-STRING (SETQ CTL-INDEX (1+ CTL-INDEX))) PARAM-FLAG T))
	  ((= CH #/,)	;comma, begin another parameter
	   (AND SIGN PARAM (SETQ PARAM (- PARAM)))
	   (VECTOR-PUSH PARAM FORMAT-PARAMS)
	   (SETQ PARAM NIL PARAM-FLAG T SIGN NIL))  ;omitted arguments made manifest by the
						    ; presence of a comma come through as NIL
	  ((= CH #\RETURN)			;No command, just ignoring a CR
	   (SETQ CTL-INDEX (1+ CTL-INDEX))	;Skip the newline
	   (OR COLON-FLAG	;Unless colon, skip whitespace on the next line
	       (DO () ((OR ( CTL-INDEX CTL-LENGTH)
			   (NOT (MEMQ (AREF CTL-STRING CTL-INDEX) '(#\SP #\TAB)))))
		 (SETQ CTL-INDEX (1+ CTL-INDEX))))
	   (RETURN 'CRLF ARGS))
	  (T					;Must be a command character
	    (SETQ CTL-INDEX (1+ CTL-INDEX))	;Advance past command character
	    (AND SIGN PARAM (SETQ PARAM (- PARAM)))
	    (AND PARAM-FLAG (VECTOR-PUSH PARAM FORMAT-PARAMS))
	    (SETQ PARAM-FLAG NIL PARAM NIL TEM NIL)
	    ;; SYM gets the symbol for the operation to be performed.
	    ;; If SYM is NIL (and maybe otherwise), TEM gets a string
	    ;; which is the operationn name as found in the control string.
	    (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
	      (IF (= CH #/\)
		  (LET ((I (STRING-SEARCH-CHAR #/\ CTL-STRING (1+ CTL-INDEX))))
		    (AND (NULL I)
			 (FORMAT-ERROR "Unmatched \ in control string."))
		    (SETQ TEM (NSUBSTRING CTL-STRING CTL-INDEX I))
		    (SETQ CTL-INDEX (1+ I))
		    (LET ((*PACKAGE* FORMAT-PACKAGE))
		      (SETQ SYM (READ-FROM-STRING TEM))))
#| This cannot work properly due to variablity of readtable.
   So, we have to use slow read every time to win.
   Of course, this format op is not commonlisp, but still...
		         (let l ((length tem))
			   (IF (OR (%STRING-SEARCH-CHAR #/: TEM 0 L)
				   (%STRING-SEARCH-CHAR #/| TEM 0 L)
				   (%STRING-SEARCH-CHAR #// TEM 0 L))
			     (SETQ SYM (READ-FROM-STRING TEM))
			   (SETQ SYM (INTERN-SOFT (STRING-UPCASE TEM)
						  FORMAT-PACKAGE))))
|#
		(SETQ SYM (OR (AREF FORMAT-CHAR-TABLE CH)
			      (PROG2 (SETQ TEM (STRING CH))
				     (INTERN-SOFT TEM FORMAT-PACKAGE)
				     (RETURN-ARRAY TEM))))))
	    (RETURN SYM ARGS)))))

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defun cw-expression (exp &aux tem)
  (when (and (consp exp)
	     (memq (car exp) all-functions-to-check-for))
    (pushnew (car exp) all-functions))
  (cond ((symbolp exp)
	 (when (or (eq all-variables-to-check-for t)
		   (memq exp all-variables-to-check-for))
	   (pushnew exp all-variables))
	 exp)
	((atom exp) exp)
	((listp (car exp))
	 ;; Explicit lambda-expression
	 (if cw-return-expansion-flag
	     (cons (cw-lambda-expression (car exp))
		   (mapcar 'cw-expression (cdr exp)))
	   (cw-lambda-expression (car exp))
	   (mapc 'cw-expression (cdr exp))))
	((nsymbolp (car exp))
	 (cw-eval-args exp))
	((do ((tail cw-function-environment (cdr tail)))
	     ((atom tail))
	   (let ((frame (car tail)))
	     (setq tem
		   (get-location-or-nil (locf frame) (locf (fsymeval (car exp)))))
	     (when tem (return tem))))
	 (if (eq (car-safe (contents tem)) 'macro)
	     ;; Local definition is a macro.  Call its expander.
	     (let ((cw-function-environment nil)
		   (si:*macroexpand-environment* (list nil cw-function-environment)))
	       (cw-expression (funcall (cdr (contents tem)) exp
				       si:*macroexpand-environment*)))
	   ;; Local definition is not a macro.  Assume it evals its args.
	   (cw-eval-args exp)))
	((setq tem (get (car exp) 'cw-handler))
	 ;; special form with its own way of doing this.
	 (funcall tem exp))
	;;kludge to deal with &quote. Blech
	((and (fboundp (car exp)) (listp (setq tem (arglist (car exp) t))) (memq '&quote tem))
	 (let ((quoted))
	   (flet ((frob (arg) (do ((x (pop tem) (pop tem)))
				  ((not (memq x lambda-list-keywords))
				   (if quoted arg (cw-expression arg)))
				(cond ((eq x '&quote) (setq quoted t))
				      ((eq x '&eval) (setq quoted nil))))))
	     (if cw-return-expansion-flag
		 (cons (car exp) (mapcar 'frob (cdr exp)))
	       (mapc 'frob (cdr exp))))))
	((multiple-value-bind (v1 v2)
	     (with-stack-list (env nil cw-function-environment)
	       (macroexpand-1 exp env))
	   (setq tem v1)
	   v2)
	 ;; Macro call.
	 (cw-expression tem))
	(t
	 (cw-eval-args exp))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-ARRAY (ARRAY &AUX ARRAYDIMS NDIMS LONG-LENGTH-FLAG)
    (COND ((SYMBOLP ARRAY)
	   (COND ((AND (BOUNDP ARRAY)
		       (ARRAYP (SYMEVAL ARRAY)))
		  (DESCRIBE-ARRAY (SYMEVAL ARRAY)))
		 ((AND (FBOUNDP ARRAY)
		       (ARRAYP (FSYMEVAL ARRAY)))
		  (DESCRIBE-ARRAY (FSYMEVAL ARRAY)))
		 (T NIL)))
	  ((ARRAYP ARRAY)
	   (FORMAT STANDARD-OUTPUT "~%This is an ~S type array." (ARRAY-TYPE ARRAY))
	   (SETQ ARRAYDIMS (ARRAY-DIMENSIONS ARRAY))
	   (SETQ NDIMS (LENGTH ARRAYDIMS))
	   (SETQ LONG-LENGTH-FLAG (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
	   (COND ((> NDIMS 1)
		  (FORMAT STANDARD-OUTPUT "~%It is ~D-dimensional, with dimensions "
			  NDIMS)
		  (DO L ARRAYDIMS (CDR L) (NULL L)
		    (FORMAT STANDARD-OUTPUT "~S " (CAR L))))
		 (T (FORMAT STANDARD-OUTPUT "~%It is ~S long." (ARRAY-LENGTH ARRAY))
		    (IF ARRAYDIMS		;don't barf on 0-rank arrays
			(AND (< (ARRAY-ACTIVE-LENGTH ARRAY) (CAR ARRAYDIMS))
			     (FORMAT STANDARD-OUTPUT "  Active length is ~S"
				     (ARRAY-ACTIVE-LENGTH ARRAY)))
		      (FORMAT T " It is of zero rank."))))
	   (AND (ARRAY-HAS-LEADER-P ARRAY)
		(FORMAT STANDARD-OUTPUT "~%It has a leader, of length ~S"
			(ARRAY-LEADER-LENGTH ARRAY)))
	   (COND ((ARRAY-DISPLACED-P ARRAY)
		  (COND ((ARRAY-INDIRECT-P ARRAY)
			 (FORMAT STANDARD-OUTPUT "~%The array is indirected to ~S"
				 (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG)))
			 (AND (ARRAY-INDEXED-P ARRAY)
			      (FORMAT STANDARD-OUTPUT ", with index-offset ~S"
				    (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG 2))))
			 (FORMAT STANDARD-OUTPUT "~%Description:")
			 (DESCRIBE-ARRAY (%P-CONTENTS-OFFSET ARRAY
							     (+ NDIMS LONG-LENGTH-FLAG))))
			(T (FORMAT STANDARD-OUTPUT "~%The array is displaced to ~S"
				   (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG))))))))
	  (T (FERROR NIL "~S is not an array" ARRAY))))

))

; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#A-MACRO (STREAM IGNORE &OPTIONAL (RANK XR-SHARP-ARGUMENT))
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (IF (AND (FIXNUMP RANK) (PLUSP RANK))
	(LET (DIMENSIONS (SEQUENCES (INTERNAL-READ STREAM T NIL T)))
	  (DO ((DIM 0 (1+ DIM))
	       (STUFF SEQUENCES (ELT STUFF 0)))
	      ((= DIM RANK))
	    (PUSH (LENGTH STUFF) DIMENSIONS))
	  (VALUES (MAKE-ARRAY (NREVERSE DIMENSIONS) ':INITIAL-CONTENTS SEQUENCES)))
      (IF (EQ RANK 0)
	  (VALUES (MAKE-ARRAY NIL ':INITIAL-ELEMENT (INTERNAL-READ STREAM T NIL T)))
	(CERROR ':NO-ACTION NIL 'READ-ERROR-1
		"~S is not a valid array rank." RANK)
	(INTERNAL-READ STREAM T NIL T)
	NIL))))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN MAKE-ARRAY (DIMENSIONS &REST OPTIONS)
  "Create an array of size DIMENSIONS (a number or list of numbers).
The keywords are as follows:
:TYPE - specify array type, controlling type of elements allowed.  Default is ART-Q.
 ART-Q (any elements), ART-Q-LIST (any elements, and the contents looks like a list),
 ART-STRING (elements 0 through 377, printed with quotes),
 ART-FAT-STRING (16 bit unsigned elements, printed with quotes),
 ART-1B (elements 0 and 1), ART-2B (elements 0 through 3), ART-4B, ART-8B, ART-16B,
 ART-32B (elements any fixnum), ART-FLOAT (elements any full-size flonum),
 ART-COMPLEX (elements any number including complex numbers),
 ART-COMPLEX-FLOAT (elements complex numbers composed of two full-size flonums),
 ART-HALF-FIX (16 bit signed fixnum elements),
 ART-FPS-FLOAT ART-COMPLEX-FPS-FLOAT (used with floating point array processor),
 ART-STACK-GROUP-HEAD, ART-REGULAR-PDL, ART-SPECIAL-PDL (parts of stack groups).
:ELEMENT-TYPE - specify array type by specifying Common Lisp
 data type of elements allowed.  For example,
 an :ELEMENT-TYPE of (MOD 4) would get an ART-2B array.
:AREA - specify area to create the array in.
:LEADER-LENGTH - specify number of elements of array leader to make.
:LEADER-LIST - list whose elements are used to initialize the leader.
:FILL-POINTER - specify initial fill pointer value (ARRAY-ACTIVE-LENGTH of the array).
 Requests a leader of length 1 and specifies the contents of the slot.
:INITIAL-ELEMENT - value used to initialize all elements of the array.
:DISPLACED-TO - array, locative or fixnum specifying address of data
 that this array should overlap.
:DISPLACED-INDEX-OFFSET - if displaced to another array, this specifies
 which element of that array should correspond to element 0 of the new one.
:NAMED-STRUCTURE-SYMBOL - if not NIL, specifies a named structure symbol
 to be stored in the array, which should have its named-structure bit set.
:INITIAL-CONTENTS - value is a sequence of sequences of sequences...
 where the leaves are the values to initialize the array from.
 The top level of sequence corresponds to the most slowly varying subscript."
  (DECLARE (ARGLIST DIMENSIONS &KEY TYPE ELEMENT-TYPE AREA LEADER-LENGTH LEADER-LIST
		    INITIAL-ELEMENT DISPLACED-TO DISPLACED-INDEX-OFFSET
		    NAMED-STRUCTURE-SYMBOL))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
	ENTRIES-PER-Q
	LEADER-LIST FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET NAMED-STRUCTURE-SYMBOL
	(AREA NIL) (TYPE 'ART-Q)
	INITIAL-VALUE INITIAL-VALUE-P INITIAL-CONTENTS INITIAL-CONTENTS-P
	ARRAY N-DIMENSIONS INDEX-LENGTH LONG-ARRAY-P LEADER-QS DATA-LENGTH LEADER-LENGTH)
    ;; Figure out whether it is old-style.
    (COND ((AND ( LENGTH-OF-OPTIONS 2)
		(OR (NUMBERP (FIRST OPTIONS))
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPES)
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPE-KEYWORDS)))
	   ;; It is old-style.  The first arg is actually AREA.
	   (SETQ AREA DIMENSIONS)
	   (SETQ TYPE (FIRST OPTIONS))
	   (SETQ DIMENSIONS (SECOND OPTIONS))
	   (SETQ DISPLACED-TO (THIRD OPTIONS))
	   (SETQ LEADER-LIST (FOURTH OPTIONS))
	   (SETQ DISPLACED-INDEX-OFFSET (FIFTH OPTIONS))
	   (SETQ NAMED-STRUCTURE-SYMBOL (SIXTH OPTIONS))
	   (IF (NUMBERP LEADER-LIST)
	       (SETQ LEADER-LENGTH LEADER-LIST
		     LEADER-LIST NIL)
	       (SETQ LEADER-LIST (REVERSE LEADER-LIST))))
	  (T
	   ;; It is new-style.
	   (IF (NOT (EVENP LENGTH-OF-OPTIONS))
	       (FERROR NIL "Odd-length options list: ~S" OPTIONS))
	   (DO ((OPTIONS OPTIONS (CDDR OPTIONS)))
	       ((NULL OPTIONS))
	     (LET ((VALUE (SECOND OPTIONS)))
	       (SELECTQ (FIRST OPTIONS)
		 (:AREA (SETQ AREA VALUE))
		 (:TYPE (SETQ TYPE VALUE))
		 (:ELEMENT-TYPE
		  (SETQ TYPE
			(ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE)))
		 (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
		 (:DISPLACED-INDEX-OFFSET (SETQ DISPLACED-INDEX-OFFSET VALUE))
		 ((:INITIAL-VALUE :INITIAL-ELEMENT)
		  (SETQ INITIAL-VALUE VALUE INITIAL-VALUE-P T))
		 (:INITIAL-CONTENTS
		  (SETQ INITIAL-CONTENTS VALUE INITIAL-CONTENTS-P T))
		 (:FILL-POINTER (SETQ LEADER-LENGTH (MAX 1 (OR LEADER-LENGTH 1)))
				(SETQ FILL-POINTER VALUE))
		 (:ADJUSTABLE)
		 (:LEADER-LIST (SETQ LEADER-LIST VALUE))
		 (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
		 (:OLD-LEADER-LENGTH-OR-LIST (IF (NUMBERP VALUE)
						 (SETQ LEADER-LENGTH VALUE)
						 (SETQ LEADER-LIST (REVERSE VALUE))))
		 (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
		 (OTHERWISE
		  (FERROR NIL "~S is not a known MAKE-ARRAY keyword." (FIRST OPTIONS))))))))
    ;; Process the DIMENSIONS argument.
    (CHECK-ARG DIMENSIONS (OR (NULL DIMENSIONS) (FIXP DIMENSIONS) (LISTP DIMENSIONS))
	       "a fixnum or a list")
    (COND ((FIXP DIMENSIONS)
	   (IF (MINUSP DIMENSIONS)
	       (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	   (SETQ N-DIMENSIONS 1
		 INDEX-LENGTH DIMENSIONS))
	  ((OR (NULL DIMENSIONS) (LISTP DIMENSIONS))
	   (DOLIST (DIM DIMENSIONS)
	     (IF (NOT (FIXP DIM))
		 (FERROR NIL "The dimension ~S is not a fixnum." DIM))
	     (IF (MINUSP DIM)
		 (FERROR NIL "The negative array dimension ~S is illegal." DIM)))
	   (SETQ N-DIMENSIONS (LENGTH DIMENSIONS))
	   (IF (> N-DIMENSIONS 7)
	       (FERROR NIL "Arrays may only have 7 dimensions, not ~S" N-DIMENSIONS))
	   (SETQ INDEX-LENGTH (APPLY 'TIMES DIMENSIONS))))
    ;; Process the DISPLACED-TO argument.
    (CHECK-ARG DISPLACED-TO
	       (OR (NULL DISPLACED-TO) (FIXP DISPLACED-TO) (ARRAYP DISPLACED-TO)
		   (LOCATIVEP DISPLACED-TO))
	       "NIL, a fixnum, a locative, or an array")
    ;; See whether this is a "short" or "long" format array.
    (IF (AND (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (NOT DISPLACED-TO))
	(SETQ LONG-ARRAY-P T))
    (OR (= (%DATA-TYPE INDEX-LENGTH) DTP-FIX)
	(FERROR NIL "Attempt to make array too large; total length ~S" INDEX-LENGTH))
    ;; Process the LEADER and NAMED-STRUCTURE-SYMBOL arguments.
    (CHECK-ARG LEADER-LIST (OR (NULL LEADER-LIST) (LISTP LEADER-LIST))
	       "NIL, or a list")
    (AND (NULL LEADER-LENGTH) (NOT (NULL LEADER-LIST))
	 (SETQ LEADER-LENGTH (LENGTH LEADER-LIST)))
    (IF (AND LEADER-LENGTH (> (LENGTH LEADER-LIST) LEADER-LENGTH))
	(FERROR NIL "Length of leader initialization list is greater than leader length"))
    (COND (NAMED-STRUCTURE-SYMBOL
	   (COND (LEADER-LENGTH
		  (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 1) "greater than 1"))
		 (T (OR (= N-DIMENSIONS 1)
			(FERROR NIL "A named-structure array may not be ~S-dimensional"
				    N-DIMENSIONS)))))
	  (LEADER-LENGTH
	   (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 0) "greater than zero")))
    (SETQ LEADER-QS (IF LEADER-LENGTH
			(+ 2 LEADER-LENGTH)
			0))
    ;; Process the TYPE argument.
    (CHECK-ARG TYPE (OR (FIXP TYPE) (MEMQ TYPE ARRAY-TYPES) (MEMQ TYPE ARRAY-TYPE-KEYWORDS))
	       "an array type")
    (IF (FIXP TYPE)
	;may be either a small integer, which is shifted over into the type field,
	; or an already shifted over quantity (such as ART-Q, etc).
	(IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
	    (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
      (IF (FIXP (SYMEVAL TYPE))
	  (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD (SYMEVAL TYPE)))
	(SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))))
    (SETQ ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE))
    ;; This is positive if there are 1 or more entries per Q.  It is
    ;; negative if there are more than one Qs per entry.
    (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			  (CEILING INDEX-LENGTH ENTRIES-PER-Q)
			(* INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
    ;; Process the DISPLACED-INDEX-OFFSET argument.
    (CHECK-ARG DISPLACED-INDEX-OFFSET
	       (OR (NULL DISPLACED-INDEX-OFFSET)
		   (AND (= (%DATA-TYPE DISPLACED-INDEX-OFFSET) DTP-FIX)
			(NOT (MINUSP DISPLACED-INDEX-OFFSET))))
	       "NIL or a non-negative fixnum")
    (LET ((HEADER-WORD
	    ;; Put in array type and number of dims.
	    (%LOGDPB N-DIMENSIONS %%ARRAY-NUMBER-DIMENSIONS
		     (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
      ;; If there is a leader, set the flag.
      (IF LEADER-LENGTH
	  (SETQ HEADER-WORD (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-WORD)))
      (SETQ HEADER-WORD
	    (COND (DISPLACED-TO
		   ;; Array is displaced; turn on the bit, and the array is 2 long
		   ;; plus one for the index-offset if any.
		   (+ (%LOGDPB 1 %%ARRAY-DISPLACED-BIT HEADER-WORD)
		      (IF DISPLACED-INDEX-OFFSET 3 2)))
		  (LONG-ARRAY-P
		   ;; It is local; if it is a long array, the length is not in the
		   ;; header at all; set the bit instead.
		   (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-WORD))
		  (T
		   ;; It is a short array; the length is in the header.
		   (+ INDEX-LENGTH HEADER-WORD))))
      ;; Create the array.
      (SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-WORD
						  INDEX-LENGTH
						  (OR LEADER-LENGTH 0)
						  AREA
						  (+ (MAX 1 N-DIMENSIONS)
						     LEADER-QS
						     (COND (DISPLACED-TO
							    (IF DISPLACED-INDEX-OFFSET 3 2))
							   (LONG-ARRAY-P
							    (1+ DATA-LENGTH))
							   (T DATA-LENGTH)))
						  )))
    (WHEN (CONSP DIMENSIONS)
      ;; It is a multi-dimensional array.  Fill in the "dope vector".
      (IF ARRAY-INDEX-ORDER
	  ;; If last index varies fastest, put in all but first dimension,
	  ;; and the last dimensions come last.
	  (DO ((DIMLIST (CDR DIMENSIONS) (CDR DIMLIST))
	       (I (+ N-DIMENSIONS (IF LONG-ARRAY-P 0 -1)) (1- I)))
	      ((NULL DIMLIST))
	    (%P-STORE-CONTENTS-OFFSET (CAR DIMLIST) ARRAY I))
	;; If first index varies fastest, put in all but last dimension,
	;; and the first dimensions come last.
	(DO ((I (IF LONG-ARRAY-P 2 1) (1+ I))
	     (DIMLIST DIMENSIONS (CDR DIMLIST))
	     (N N-DIMENSIONS (1- N)))
	    ((< N 2))
	  (%P-STORE-CONTENTS-OFFSET (CAR DIMLIST) ARRAY I))))
    (COND (DISPLACED-TO
	   ;; It is displaced.  Put information after the dope vector, and after
	   ;; the "long array" word if any.
	   (LET ((IDX (IF LONG-ARRAY-P (1+ N-DIMENSIONS) N-DIMENSIONS)))
	     (%P-STORE-CONTENTS-OFFSET DISPLACED-TO ARRAY IDX)
	     (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ IDX))
	     (COND (DISPLACED-INDEX-OFFSET
		    ;; Index offset feature is in use.
		    ;; Store the index offset in the next Q.
		    (%P-STORE-CONTENTS-OFFSET DISPLACED-INDEX-OFFSET ARRAY (+ IDX 2)))))))
    ;; The leader's initial values were specified.
    (DO ((I 0 (1+ I))
	 (LEADER-LIST LEADER-LIST (CDR LEADER-LIST)))
	((NULL LEADER-LIST))
      (STORE-ARRAY-LEADER (CAR LEADER-LIST) ARRAY I))
    (AND FILL-POINTER
	 (SETF (ARRAY-LEADER ARRAY 0) FILL-POINTER))
;;; Cretinism associated with make-array, in that the leader list can overlap
;;; with the name-structure slot, which is how fasd dumps the named-structure-symbol
;;; So we check for the symbol being t and not smash it in that case
    (COND (NAMED-STRUCTURE-SYMBOL
	   (COND ((NULL LEADER-LENGTH)
		  ;; There is no leader; put it in element zero of the body.
		  (AS-1 NAMED-STRUCTURE-SYMBOL ARRAY 0))
		 (T
		  ;; There is a leader; use element one of the leader.
		  (IF (NEQ NAMED-STRUCTURE-SYMBOL T)
		      (STORE-ARRAY-LEADER NAMED-STRUCTURE-SYMBOL ARRAY 1))))
	   ;; It is a named structure.  Set the flag.
	   (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0)))
    (AND INITIAL-VALUE-P (NOT DISPLACED-TO)
	 (ARRAY-INITIALIZE ARRAY INITIAL-VALUE))
    (WHEN INITIAL-CONTENTS-P
      (FILL-ARRAY-FROM-SEQUENCES ARRAY INITIAL-CONTENTS 0 0))
    ;; If there is a fill pointer on an art-q-list array, then it should control
    ;; the length of the list as well.  See array-push and array-pop.
    (COND ((AND (= N-DIMENSIONS 1)
		(OR FILL-POINTER (NOT (NULL LEADER-LIST)))
		;; The cold load generator's frame builder isn't smart enough for a #, here.
		(= TYPE #|'#,|# (LDB %%ARRAY-TYPE-FIELD ART-Q-LIST)))
	   (OR FILL-POINTER (SETQ FILL-POINTER (CAR LEADER-LIST)))
	   (COND ((AND (FIXP FILL-POINTER)
		       (> FILL-POINTER 0)
		       (< FILL-POINTER (ARRAY-LENGTH ARRAY)))
		  (%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- FILL-POINTER)))))))
    (VALUES ARRAY DATA-LENGTH)))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFCONST ARRAY-ELEMENT-TYPE-ALIST
	  '((NIL . ART-ERROR)
	    (BIT . ART-1B)
	    ((MOD 4) . ART-2B)
	    ((MOD 8) . ART-4B)
	    ((MOD 400) . ART-8B)
	    ((MOD 200000) . ART-16B)
	    (FIXNUM . ART-32B)
	    (* . ART-Q)
	    (* . ART-Q-LIST)
	    (STRING-CHAR . ART-STRING)
	    (* . ART-STACK-GROUP-HEAD)
	    (* . ART-SPECIAL-PDL)
	    ((SIGNED-BYTE 20) . ART-HALF-FIX)
	    (* . ART-REG-PDL)
	    (FLOAT . ART-FLOAT)
	    (FLOAT . ART-FPS-FLOAT)
	    (FAT-CHAR . ART-FAT-STRING)
	    ((COMPLEX FLOAT) . ART-COMPLEX-FLOAT)
	    (COMPLEX . ART-COMPLEX)
	    ((COMPLEX FLOAT) . ART-COMPLEX-FPS-FLOAT)
	    ((UNSIGNED-BYTE 1) . ART-1B)
	    ((UNSIGNED-BYTE 2) . ART-2B)
	    ((UNSIGNED-BYTE 4) . ART-4B)
	    ((UNSIGNED-BYTE 10) . ART-8B)
	    ((UNSIGNED-BYTE 20) . ART-16B)
	    ((SIGNED-BYTE 20) . ART-HALF-FIX)))

))

(si:defrepinfix cli:// 120.)
(defvar cli:// nil "All values of last expression evaluated by read-eval-print loop.")
(forward-value-cell 'cli:// '//)



; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN INCREMENT-PATCH-SYSTEM-MAJOR-VERSION (NAME STATUS &AUX VERSION PATCH-MAJOR)
  "Increment the major version of the patchable system NAME, and set status to STATUS.
This modifies the patch directory files of the system."
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME T))
  (COND ((NULL VERSION)
	 (FORMAT T "~&No master directory for system ~A, creating one." NAME)
	 (SETQ VERSION 0)))
  (SETQ VERSION (1+ VERSION)
	PATCH-MAJOR (MAKE-PATCH-MAJOR NAME NAME VERSION VERSION))
  (WITH-OPEN-FILE (FILE (PATCH-SYSTEM-PATHNAME NAME ':SYSTEM-DIRECTORY) '(:WRITE))
    (FORMAT FILE
	    ";;; -*- Mode: Lisp; Package: User; Base: 10.; Patch-File: T -*-
")
    (WRITE-RESPONSIBILITY-COMMENT FILE)
    (LET ((*PRINT-BASE* 10.))
      (PRINT PATCH-MAJOR FILE)))
  (LET ((FIRST-VERS (MAKE-PATCH-VERSION NUMBER 0
					EXPLANATION (FORMAT NIL "~A Loaded" NAME))))
    (WRITE-PATCH-DIRECTORY PATCH-MAJOR (MAKE-PATCH-DIR STATUS STATUS
						       VERSION-LIST (NCONS FIRST-VERS))))
  VERSION)

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN PRINT-PATCHES (&OPTIONAL (SYSTEM "System") (AFTER 0))
  "Print the patches of the system SYSTEM after minor version AFTER."
  (LET* ((PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED SYSTEM T T))
	 (VERSION (PATCH-VERSION PATCH-SYSTEM))	;efficiency
	 (LATEST (VERSION-NUMBER (CAR (PATCH-VERSION-LIST PATCH-SYSTEM)))))
    (IF (NULL PATCH-SYSTEM)
	(FORMAT T "~%No ~A system loaded~%" SYSTEM)
      (FORMAT T "~%~A ~8TModification:~%" (PATCH-NAME PATCH-SYSTEM))
      (IF (> AFTER LATEST) (FORMAT T "Most recent patch loaded is ~D." LATEST)
	(DOLIST (V (REVERSE (PATCH-VERSION-LIST PATCH-SYSTEM)))
	  (WHEN ( AFTER (VERSION-NUMBER V)) (PRINT-PATCH VERSION V)))))))

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN PRINT-PATCH (MAJOR-VERSION-NUMBER PATCH-VERSION-DESC)
  (FORMAT T "~&~D.~D ~8T~A:~:[~; (unreleased)~]~&~10T~~A~"
	  MAJOR-VERSION-NUMBER
	  (VERSION-NUMBER PATCH-VERSION-DESC)
	  (VERSION-AUTHOR PATCH-VERSION-DESC)
	  (VERSION-UNRELEASED PATCH-VERSION-DESC)
	  (VERSION-EXPLANATION PATCH-VERSION-DESC)))

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-PATCHES (&REST OPTIONS &AUX TEM SOMETHING-CHANGED)
  "Load any new patches for one or more systems.
Options can include these symbols:
 :NOSELECTIVE - don't ask about each patch.
 :SILENT or :NOWARN - don't print out any information on loading patches
   (and also don't ask).
 :VERBOSE - says to print out information about loading each patch.
   This is the default and is only turned off by :silent and :nowarn.
 :UNRELEASED - says to load or consider unreleased patches.
   Once unreleased patches have been loaded, a band may not be dumped.
 :FORCE-UNFINISHED - load all patches that have not been finished yet,
   if they have QFASL files.  This is good for testing patches.
 :NOOP - do nothing
 :SITE - load latest site configuration info.
 :NOSITE - do not load latest site configuration info.
   :SITE is the default unless systems to load patches for are specified.

Options can also include :SYSTEMS followed by a list of systems to load patches for.
One or more names of systems are also allowed.

LOAD-PATCHES returns T if any patches were loaded, otherwise NIL."
  (CATCH-ERROR-RESTART (SYS:REMOTE-NETWORK-ERROR
			 "Give up on trying to load patches.")
    (LET ((SYSTEM-NAMES NIL)			;A-list of systems to load patches for.
	  (SELECTIVE-P T)			;Ask the user.
	  (VERBOSE-P T)				;Tell the user what's going on.
	  (UNRELEASED-P NIL)
	  (SITE-SPECIFIED-P NIL)
	  (SITE-P T)
	  (FORCE-THROUGH-UNFINISHED-PATCHES-P NIL))
      (DO ((OPTS OPTIONS (CDR OPTS)))
	  ((NULL OPTS))
	(SELECTQ (CAR OPTS)
	  (:SYSTEMS
	   (SETQ OPTS (CDR OPTS))
	   (SETQ SYSTEM-NAMES
		 (IF (CONSP (CAR OPTS))
		     (MAPCAR 'GET-PATCH-SYSTEM-NAMED (CAR OPTS))
		   (LIST (GET-PATCH-SYSTEM-NAMED (CAR OPTS)))))
	   (UNLESS SITE-SPECIFIED-P
	     (SETQ SITE-P NIL)))
	  ((:SILENT :NOWARN) (SETQ VERBOSE-P NIL SELECTIVE-P NIL))
	  (:VERBOSE (SETQ VERBOSE-P T))
	  (:SELECTIVE (SETQ SELECTIVE-P T))
	  (:SITE (SETQ SITE-P T SITE-SPECIFIED-P T))
	  (:NOOP NIL)
	  (:NOSITE (SETQ SITE-P NIL SITE-SPECIFIED-P T))
	  (:UNRELEASED (SETQ UNRELEASED-P T))
	  (:NOSELECTIVE (SETQ SELECTIVE-P NIL))
	  (:FORCE-UNFINISHED (SETQ FORCE-THROUGH-UNFINISHED-PATCHES-P T))
	  (OTHERWISE
	    (COND ((AND (OR (SYMBOLP (CAR OPTS)) (STRINGP (CAR OPTS)))
			(SETQ TEM (GET-PATCH-SYSTEM-NAMED (CAR OPTS) T)))
		   (PUSH TEM SYSTEM-NAMES)
		   (UNLESS SITE-SPECIFIED-P
		     (SETQ SITE-P NIL)))
		  (T (FERROR NIL "~S is not a LOAD-PATCHES option and not a system name."
				 (CAR OPTS)))))))
      (LET-IF VERBOSE-P ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
	(WHEN SITE-P
	  (AND VERBOSE-P
	       (FORMAT T "~%Checking whether site configuration has changed..."))
	  (IF (IF SELECTIVE-P
		  (MAKE-SYSTEM ':SITE ':NO-RELOAD-SYSTEM-DECLARATION)
		(IF VERBOSE-P
		    (MAKE-SYSTEM ':SITE ':NOCONFIRM ':NO-RELOAD-SYSTEM-DECLARATION)
		  (MAKE-SYSTEM ':SITE ':NOCONFIRM ':NO-RELOAD-SYSTEM-DECLARATION
			       ':SILENT)))
	      (SETQ SOMETHING-CHANGED T)
	    (FORMAT T "  it hasn't.")))
	(OR SYSTEM-NAMES (SETQ SYSTEM-NAMES PATCH-SYSTEMS-LIST))
	(LET ((FIRST-SYSTEM T))			; This is the first system being patched.
	  (DOLIST (PATCH SYSTEM-NAMES)
	    (CATCH-ERROR-RESTART (ERROR "Give up on patches for ~A." (CAR PATCH))
	      (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH T))
		     (NEW-VERS (PATCH-DIR-VERSION-LIST PATCH-DIR))
		     (MAJOR (PATCH-VERSION PATCH))
		     PATCHES-NOT-LOADED
		     (CHANGE-STATUS T)		;Ok to change the system status
		     (UNRELEASED-CONSIDERED NIL);T if considering unreleased patches.
		     (PATCH-SKIPPED NIL)	;T if considering patches after skipping one.
		     (PROCEED-FLAG (NOT SELECTIVE-P)))	; Has the user said to proceed?
		(IF (AND (NULL PATCH-DIR) VERBOSE-P)
		    (FORMAT T "~&Skipping system ~A, whose patch directory cannot be accessed.~%"
			    PATCH)
		  ;; Get list of patches of this system not already loaded.
		  (SETQ PATCHES-NOT-LOADED
			(CDR (MEMASSQ (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
				      NEW-VERS)))
		  ;; Maybe announce the system.
		  (COND ((AND PATCHES-NOT-LOADED VERBOSE-P) ;;verbose and silent no sense
			 (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
			 (UNLESS FIRST-SYSTEM (TERPRI))
			 (FORMAT T "~&Patches for ~A (Current version is ~D.~D):"
				 (PATCH-NAME PATCH) MAJOR (CAAR (LAST NEW-VERS)))))
		  (DOLIST (VERSION PATCHES-NOT-LOADED)
		    (LET* ((FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH)
							    ':PATCH-FILE
							    (PATCH-VERSION PATCH)
							    (VERSION-NUMBER VERSION)
							    ':QFASL)))
		      ;; NIL is used to mark patches that are reserved, but not finished.
		      ;; We can't load any more patches without this one, in order to
		      ;; make sure that any two systems claiming to be version xx.yy
		      ;; always have exactly the same set of patches loaded.  Punt.
		      ;; If someone forgets to finish a patch, we assume a hacker will
		      ;; eventually see what is happening and fix the directory to unstick
		      ;; things.  We might at least say the patches are unfinished.
		      (UNLESS (VERSION-EXPLANATION VERSION)
			(COND (VERBOSE-P
			     (FORMAT T "~&There are unfinished patches in ~A."
				     (PATCH-NAME PATCH))))
			(UNLESS FORCE-THROUGH-UNFINISHED-PATCHES-P
			  (RETURN)))
		      (WHEN (VERSION-UNRELEASED VERSION)
			(COND (VERBOSE-P
			       (FORMAT T "~&There are unreleased patches in ~A."
				       (PATCH-NAME PATCH))))
			(OR
			  FORCE-THROUGH-UNFINISHED-PATCHES-P 
			  UNRELEASED-P
			  UNRELEASED-CONSIDERED
			  (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			  (AND SELECTIVE-P
			       (WITH-TIMEOUT ((* 5 60. 60.)
					      (FORMAT T " -- timed out, No.")
					      NIL)
				 (FORMAT T "~&Such patches are subject to change; therefore,
you should not load them if you are going to dump a band.
If you are not going to dump a band, it is reasonable
to load these patches to benefit from the improvements in them.")
				 (SETQ PROCEED-FLAG NIL)
				 (Y-OR-N-P "Consider the unreleased patches? (Automatic No after 5 minutes) ")))
			  (RETURN))
			(SETQ UNRELEASED-CONSIDERED T))
		      (IF VERBOSE-P
			  (PRINT-PATCH (PATCH-VERSION PATCH) VERSION))
		      (SELECTQ-EVERY
			(COND (PROCEED-FLAG)
			      (T (WITH-TIMEOUT ((* 5 60. 60.)
						(FORMAT T " -- timed out, Proceed.")
						'PROCEED)
				   (FQUERY '(:CHOICES (((T "Yes.") #/Y #\SP #/T #\HAND-UP)
						       ((NIL "No.") #/N #\RUBOUT #\HAND-DOWN)
						       ((PROCEED "Proceed.") #/P)))
					   "Load? (Automatic Proceed after 5 minutes) "))))
			(NIL
			 ;; "No", don't load any more for this system.
			 ;; Also don't change the status.
			 ;; Except, if we are considering unreleased patches,
			 ;; loading out of order is no worse than loading unreleased
			 ;; patches in the first place, so keep on offering.
			 (SETQ CHANGE-STATUS NIL)
			 (UNLESS (OR FORCE-THROUGH-UNFINISHED-PATCHES-P
				     UNRELEASED-CONSIDERED)
			   (RETURN NIL))
			 (WHEN (EQ VERSION (CAR (LAST PATCHES-NOT-LOADED)))
			   ;; Don't give a spiel about following patches
			   ;; if there are none.
			   (RETURN NIL))
			 (UNLESS (OR PATCH-SKIPPED
				     (EQ (PATCH-STATUS PATCH) ':INCONSISTENT))
			   (FORMAT T "~&If you load any following patches for this system,
they will be out of sequence, so you must not dump a band.")
			   (SETQ PATCH-SKIPPED T)))
			(PROCEED
			 ;; "Proceed" with the rest for this system.
			 (SETQ PROCEED-FLAG T))
			((T PROCEED)
			 ;; "Yes" or "Proceed", do this one.
			 (SETQ SOMETHING-CHANGED T)
			 ;; Unfinished, unreleased or out of sequence =>
			 ;;  mark system as inconsistent.
			 (WHEN (OR PATCH-SKIPPED
				   (NULL (VERSION-EXPLANATION VERSION))
				   (VERSION-UNRELEASED VERSION))
			   (UNLESS (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			     (SETF (PATCH-STATUS PATCH) ':INCONSISTENT)
			     (FORMAT T "~&~A is now inconsistent; do not dump a band."
				     (PATCH-NAME PATCH))))
			 ;; Avoid error if non ex file, if patch is known to be unfinished.
			 (CONDITION-CASE-IF (NULL (VERSION-EXPLANATION VERSION)) ()
			     (LOAD FILENAME NIL NIL T (NOT VERBOSE-P))	; Don't set default,
			   (FS:FILE-NOT-FOUND
			    (IF VERBOSE-P
				(FORMAT T "~&File ~A does not exist, ignoring this patch."
					FILENAME))))
			 (PUSH VERSION (PATCH-VERSION-LIST PATCH))))))
		  (AND CHANGE-STATUS
		       (NEQ (PATCH-STATUS PATCH) ':INCONSISTENT)
		       (LET ((NEW-STATUS (PATCH-DIR-STATUS PATCH-DIR)))
			 (COND ((NEQ (PATCH-STATUS PATCH) NEW-STATUS)
				(SETQ SOMETHING-CHANGED T)
				(AND VERBOSE-P
				     (FORMAT T "~&~A is now ~A."
					     (PATCH-NAME PATCH)
					     (FOURTH (ASSQ NEW-STATUS
							   SYSTEM-STATUS-ALIST))))
				;; Update the status.
				(SETF (PATCH-STATUS PATCH) NEW-STATUS))))))))
	    (SETQ FIRST-SYSTEM NIL))))))
  SOMETHING-CHANGED)

))

; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN (PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
       LAST-CH ;ignored
       (PROG (THING PK
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
		 ;;don't try to find packages if we're not interning -- eg #+slime (dis:foo)
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
	       (SETQ THING (READ STREAM))
	       ;; Don't use symbol substitution if a package is explicitly specified!
	       (AND (SYMBOLP THING) (SETQ THING (INTERN THING PACKAGE))))
	     (RETURN THING (TYPE-OF THING) T)))   ;T means we already did RETURN-READ-STRING

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defsetf cli:ar-1 set-ar-1)
(defsetf cli:ar-1-force set-ar-1-force)
(defprop cli:ar-1 ap-1 locf-method)
(defprop cli:ar-1-force ap-1-force locf-method)

))

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-ARRAY (EXP STREAM FASTP I-PRINDEPTH
		    &OPTIONAL (WHICH-OPERATIONS (WHICH-OPERATIONS-FOR-PRINT STREAM))
		    &AUX (RANK (ARRAY-RANK EXP)))
  (IF *PRINT-ARRAY*
      (IF (AND (= RANK 1)
	       (EQ (ARRAY-TYPE EXP) 'ART-1B))
	  (PRINT-BIT-VECTOR EXP STREAM)
	(IF *PRINT-PRETTY*
	    (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
	  (IF (= RANK 1)
	      (PRINT-VECTOR EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
	    (PRINT-MULTIDIMENSIONAL-ARRAY EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
    (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
      (PRINT-RAW-STRING (GET-PNAME (ARRAY-TYPE EXP)) STREAM FASTP)
      (DOTIMES (I RANK)
	(SEND STREAM ':TYO #\-)
	(PRINT-FIXNUM (ARRAY-DIMENSION EXP I) STREAM)))))

(DEFUN PRINT-MULTIDIMENSIONAL-ARRAY (EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
  (DOLIST (ELT (PTTBL-ARRAY *READTABLE*))
    (COND ((STRINGP ELT) (SEND STREAM ':STRING-OUT ELT))
	  ((EQ ELT ':RANK)
	   (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
	     (PRINT-FIXNUM (ARRAY-RANK EXP) STREAM)))
	  ((EQ ELT ':SEQUENCES)
	   (PRINT-ARRAY-CONTENTS EXP 0 0 I-PRINDEPTH STREAM WHICH-OPERATIONS)))))

(DEFVAR ARRAY-CONTENTS-ARRAY NIL)
(DEFUN PRINT-ARRAY-CONTENTS (ARRAY DIMENSION INDEX-SO-FAR I-PRINDEPTH STREAM W-O &AUX TEM)
  (IF (AND *PRINT-LEVEL* ( I-PRINDEPTH *PRINT-LEVEL*))
      (SEND STREAM ':STRING-OUT (PTTBL-PRINLEVEL *READTABLE*))
    (IF (ZEROP (ARRAY-RANK ARRAY))
	(PRINT-OBJECT (AREF ARRAY) I-PRINDEPTH STREAM W-O)
      (LET ((INDEX (* INDEX-SO-FAR (ARRAY-DIMENSION ARRAY DIMENSION)))
	    (MODE (CAR (MEMQ (ARRAY-TYPE ARRAY) '(ART-1B ART-STRING ART-FAT-STRING))))
	    (LENGTH (ARRAY-DIMENSION ARRAY DIMENSION))
	    (RANK (ARRAY-RANK ARRAY)))
	(COND ((AND MODE (= (1+ DIMENSION) RANK))
	       (LET ((KLUDGE (IF (AND (%STORE-CONDITIONAL (LOCF ARRAY-CONTENTS-ARRAY)
							  (SETQ TEM ARRAY-CONTENTS-ARRAY)
							  NIL)
				      TEM)
				 (CHANGE-INDIRECT-ARRAY TEM MODE LENGTH ARRAY INDEX)
			       (MAKE-ARRAY LENGTH ':TYPE MODE ':DISPLACED-TO ARRAY
					   ':DISPLACED-INDEX-OFFSET INDEX))))
		 (IF (EQ MODE 'ART-1B) (PRINT-BIT-VECTOR KLUDGE STREAM)
		   (PRINT-QUOTED-STRING KLUDGE STREAM (MEMQ ':STRING-OUT W-O)))
		 (SETQ ARRAY-CONTENTS-ARRAY KLUDGE)))	;massachusetts
	      (T (SEND STREAM ':TYO (PTTBL-OPEN-PAREN *READTABLE*))
		 (DOTIMES (I (ARRAY-DIMENSION ARRAY DIMENSION))
		   (OR (ZEROP I) (SEND STREAM ':TYO (PTTBL-SPACE *READTABLE*)))
		   (COND ((AND *PRINT-LENGTH* (= I *PRINT-LENGTH*))
			  (SEND STREAM ':STRING-OUT (PTTBL-PRINLENGTH *READTABLE*))
			  (RETURN))
			 ((= (1+ DIMENSION) (ARRAY-RANK ARRAY))
			  (PRINT-OBJECT (AR-1-FORCE ARRAY (+ INDEX I))
					(1+ I-PRINDEPTH) STREAM W-O))
			 ((AND *PRINT-LEVEL*
			       ( (1+ I-PRINDEPTH) *PRINT-LEVEL*))
			  (SEND STREAM ':STRING-OUT (PTTBL-PRINLEVEL *READTABLE*)))
			 (T
			  (PRINT-ARRAY-CONTENTS ARRAY (1+ DIMENSION)
						(+ INDEX I) (1+ I-PRINDEPTH)
						STREAM W-O))))
		 (SEND STREAM ':TYO (PTTBL-CLOSE-PAREN *READTABLE*))))))))

))

; From file MOUSE.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFUN READ-FUNCTION-NAME (PROMPT &OPTIONAL DEFAULT MUST-BE-DEFINED STRINGP
				  &AUX EXPLICIT-PACKAGE-P
				  (*MINI-BUFFER-DEFAULT-STRING* DEFAULT)
				  (READ-FUNCTION-NAME-MUST-BE-DEFINED MUST-BE-DEFINED)
				  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
				    *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*)
				  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
				    *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
				    *MOUSE-FONT-CHAR*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
				    *MOUSE-X-OFFSET*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET
				    *MOUSE-Y-OFFSET*))
  "Read a function name using mini buffer or mouse.
PROMPT is a string that goes in the mode line.
DEFAULT is a function spec to return if the user types just Return.
MUST-BE-DEFINED can be T (allow only defined functions), NIL (allow anything)
 or AARRAY-OK (allow anything either defined as a function
 or known as a section by the editor).
STRINGP can be T, NIL, ALWAYS-READ or MULTIPLE-OK.
 T means if user types text, just return a string; don't try to intern it.
 ALWAYS-READ means intern the user's string afresh now;
  don't use the symbol or list recorded in the completion aarray.
 MULTIPLE-OK means it is ok to return more than one possible function
  the user could have meant, if they differ only in their package.

The first value is a list of function specs (only one, unless STRINGP is MULTIPLE-OK).
 If STRINGP is T, this is NIL.
The second value is the string the user typed, sans package prefix.
The third value is T if the user typed a package prefix."
  (DECLARE (RETURN-LIST COMPLETIONS STRING EXPLICIT-PACKAGE-P))
  (DECLARE (SPECIAL READ-FUNCTION-NAME-MUST-BE-DEFINED
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
		    READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
		    READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
		    READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET))
  (AND (EQ MUST-BE-DEFINED T) (SETQ STRINGP 'ALWAYS-READ))
  (SETQ PROMPT (FORMAT NIL "~A~:[:~; (Default: ~S)~]" PROMPT DEFAULT DEFAULT))
  (LET ((NAME
	  (LET ((*POST-COMMAND-HOOK*
		  (APPEND *POST-COMMAND-HOOK* '(READ-FUNCTION-NAME-COMMAND-HOOK)))
		(*MINI-BUFFER-VALUE-HISTORY*
		  *DEFINITION-NAME-HISTORY*))
	    (LET ((*BATCH-UNDO-SAVE* T))
	      (DELETE-INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
	    (READ-FUNCTION-NAME-COMMAND-HOOK NIL)
	    (UNWIND-PROTECT
	      (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *ZMACS-COMPLETION-AARRAY*
						(OR (NEQ STRINGP 'ALWAYS-READ)
						    'ALWAYS-STRING))
	      (READ-FUNCTION-NAME-COMMAND-HOOK T))))
	SYM ERROR-P) 
    (COND ((EQUAL NAME "")
	   (OR DEFAULT (BARF))
	   (SETQ SYM DEFAULT
		 NAME (IF (SYMBOLP NAME) (STRING NAME)
			(FORMAT:OUTPUT NIL (PRINC DEFAULT)))))
	  ((CONSP NAME)
	   (SETQ SYM (CDR NAME)
		 NAME (CAR NAME))
	   (AND (CONSP SYM) (NEQ STRINGP 'MULTIPLE-OK)
		(SETQ SYM (CAR SYM))))
	  ((EQ STRINGP T)			;If returning a string, don't intern it
	   (SETQ SYM NAME))
	  (T
	   ;; If the string that was specified started with a package prefix,
	   ;; return a flag saying so.
	   ;; SYMBOL-FROM-STRING will flush the prefix from NAME.
	   (LET ((NON-LETTER-INDEX
		   (STRING-SEARCH-NOT-SET " ABCDEFGHIJKLMNOPQRSTUVWXYZ-" NAME)))
	     (AND NON-LETTER-INDEX (= (AREF NAME NON-LETTER-INDEX) #/:)
		  (SETQ EXPLICIT-PACKAGE-P T)))
	   (MULTIPLE-VALUE (SYM NAME ERROR-P)
	     (SYMBOL-FROM-STRING NAME NIL T))
	   (AND (CONSP SYM) (EQ STRINGP 'MULTIPLE-OK)
		(SETQ SYM (NCONS SYM)))
	   (AND ERROR-P (BARF "Read error"))))
    (AND (EQ MUST-BE-DEFINED T)
	 (NOT (OR (FDEFINEDP SYM)
		  (AND (SYMBOLP SYM)
		       (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST SYM)))))
	 (OR (DOLIST (SPEC (PACKAGE-LOOKALIKE-SYMBOLS SYM))
	       (AND (FQUERY '(:SELECT T)
			    ;; Always print prefix
			    ;; Don't leave PACKAGE in keyword during query.
			    (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
			      (FORMAT NIL "Do you mean ~S? " SPEC)))
		    (RETURN (SETQ SYM SPEC))))
	     (BARF "~S is not defined" SYM)))
    (PUSH-ON-HISTORY SYM *DEFINITION-NAME-HISTORY*)
    (VALUES SYM NAME EXPLICIT-PACKAGE-P)))

))

; From file MOUSE.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFUN ATOM-UNDER-MOUSE (WINDOW &OPTIONAL CHAR X Y LINE INDEX &AUX SYMBOL END)
  "Returns the symbol which the mouse is pointing at in WINDOW.  NIL if not pointing at one.
Normally, CHAR, X, Y, LINE, and INDEX are set from the mouse position.
If you pass them, then the mouse position is irrelevant.
Actually, X and Y are irrelevant in any case.
All that matters is LINE and INDEX, and CHAR which would be the character there.
The values are the symbol pointed at, the line it is in,
and the start and end indices of the symbol as a substring in that line.
All values are NIL if the position is not on a valid symbol."
  (DECLARE (VALUES SYMBOL LINE START END))
  (OR CHAR (MULTIPLE-VALUE (CHAR X Y LINE INDEX)
	       (MOUSE-CHAR WINDOW)))
  (AND CHAR
       ( CHAR #\CR)
       (DO ((I INDEX (1- I)))
	   ((OR (ZEROP I)
		( (ATOM-WORD-SYNTAX (AREF LINE I)) WORD-ALPHABETIC))
	    (AND ( I INDEX)
		 (CATCH-ERROR (LET ((*PACKAGE* *PACKAGE*)
				    (READ-PRESERVE-DELIMITERS T)
				    (INTERVAL (WINDOW-INTERVAL WINDOW)))
				(COMPUTE-BUFFER-PACKAGE INTERVAL)
				(MULTIPLE-VALUE (SYMBOL END)
				  (CLI:READ-FROM-STRING LINE NIL NIL
							':START (SETQ I (1+ I))
							':PRESERVE-WHITESPACE T))
				(SETQ END (MIN (ARRAY-ACTIVE-LENGTH LINE) END)))
			      NIL)
		 (SYMBOLP SYMBOL)
		 (VALUES SYMBOL LINE I END))))))

))

; From file MOUSE.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFUN READ-FUNCTION-NAME-COMMAND-HOOK (CHAR-OR-T)
  (DECLARE (SPECIAL READ-FUNCTION-NAME-MUST-BE-DEFINED
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
		    READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
		    READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
		    READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET))
  (COND ((AND (NEQ CHAR-OR-T T)
	      (BP-= (INTERVAL-FIRST-BP (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
		    (INTERVAL-LAST-BP (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))))
	 (SETQ *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*
	       (IF READ-FUNCTION-NAME-MUST-BE-DEFINED
		   #'BLINK-FUNCTION
		 #'BLINK-ATOM)
	       *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
	       "Click left on highlighted name to select it."
	       *MOUSE-HOOK*
	       #'(LAMBDA (WINDOW CHAR IGNORE IGNORE &AUX TEM)
		   (AND (= CHAR #\MOUSE-1-1)
			(MULTIPLE-VALUE-BIND (FCTN LINE START END)
			    (ATOM-UNDER-MOUSE WINDOW)
			  (WHEN (AND LINE
				     (OR (FBOUNDP (SETQ TEM FCTN))
					 (GET TEM ':SOURCE-FILE-NAME)
					 (GET TEM 'ZMACS-BUFFERS)
					 (STRING-IN-AARRAY-P TEM *ZMACS-COMPLETION-AARRAY*)
					 (AND (NOT READ-FUNCTION-NAME-MUST-BE-DEFINED) TEM)))
			    (LET ((INT (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
			      (DELETE-INTERVAL INT)
			      (INSERT (INTERVAL-FIRST-BP INT) LINE START END))
			    (*THROW 'RETURN-FROM-COMMAND-LOOP
				    (SUBSTRING LINE START END))))))
	       *MOUSE-FONT-CHAR* 0
	       *MOUSE-X-OFFSET* 4
	       *MOUSE-Y-OFFSET* 0)
	 (TV:MOUSE-SET-BLINKER-DEFINITION ':CHARACTER *MOUSE-X-OFFSET* *MOUSE-Y-OFFSET*
					  ':ON ':SET-CHARACTER *MOUSE-FONT-CHAR*))
	(T (SEND *GLOBAL-MOUSE-CHAR-BLINKER* ':SET-VISIBILITY NIL)
	   (SETQ *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*
		 READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
		 *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
		 READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
		 *MOUSE-HOOK* NIL
		 *MOUSE-FONT-CHAR*
		 READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
		 *MOUSE-X-OFFSET*
		 READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
		 *MOUSE-Y-OFFSET*
		 READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET)
	   (TV:MOUSE-SET-BLINKER-DEFINITION ':CHARACTER *MOUSE-X-OFFSET* *MOUSE-Y-OFFSET*
					    ':ON ':SET-CHARACTER *MOUSE-FONT-CHAR*)))
  (TV:MOUSE-WAKEUP))

))

; From file MOUSE.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFUN BLINK-FUNCTION (BLINKER WINDOW CHAR X Y LINE INDEX &OPTIONAL NOT-DEFINED-OK
							  &AUX SYMBOL BEG END SHEET)
  (COND ((> TV:MOUSE-SPEED *BLINKING-FUNCTION-MAXIMUM-MOUSE-SPEED*)
	 (TV:BLINKER-SET-VISIBILITY BLINKER NIL))	;Moving too fast, forget it
	(T
	 (MULTIPLE-VALUE (SYMBOL NIL BEG END)
	   (ATOM-UNDER-MOUSE WINDOW CHAR X Y LINE INDEX))
	 (COND ((AND (NOT (NULL BEG))
		     (OR NOT-DEFINED-OK
			 (FBOUNDP SYMBOL)
			 (GET SYMBOL 'ZMACS-BUFFERS)
			 (GET SYMBOL ':SOURCE-FILE-NAME)
			 (STRING-IN-AARRAY-P SYMBOL *ZMACS-COMPLETION-AARRAY*)))
		(SETQ SHEET (WINDOW-SHEET WINDOW))
		(TV:BLINKER-SET-SHEET BLINKER SHEET)
		(SHEET-SET-BLINKER-CURSORPOS SHEET BLINKER
					     (- X (TV:SHEET-STRING-LENGTH SHEET LINE BEG
									  INDEX))
					     Y)
		(TV:BLINKER-SET-SIZE BLINKER
				     (TV:SHEET-STRING-LENGTH SHEET LINE BEG END)
				     (FONT-CHAR-HEIGHT (AREF (TV:SHEET-FONT-MAP SHEET)
							     (LDB %%CH-FONT CHAR))))
		(TV:BLINKER-SET-VISIBILITY BLINKER T))
	       (T
		(TV:BLINKER-SET-VISIBILITY BLINKER NIL))))))

))

(load "sys:fonts;cptfontb" :verbose nil)
(tv:update-font-maps)

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(DEFUN CALL-TO-MULTIPLE-VALUE-LIST (FORM)
  (IF (NOT (AND (= (LENGTH FORM) 4)
		(MEMBER (THIRD FORM) '('(:OPTIONAL :SPREAD) '(:SPREAD :OPTIONAL)))))
      FORM
    (LET ((ARGFORM (OPTIMIZE (FOURTH FORM) NIL))
	  (FIRSTARG (OPTIMIZE (SECOND FORM) NIL)))
      (COND ((ATOM ARGFORM)
	     FORM)
	    ((AND (MEMQ (CAR ARGFORM) '(MULTIPLE-VALUE-LIST :MULTIPLE-VALUE-LIST))
		  (CONSP FIRSTARG)
		  (EQ (CAR FIRSTARG) 'FUNCTION)
		  (CONSP (CADR FIRSTARG))
		  (MEMQ (CAADR FIRSTARG) '(LAMBDA CLI:LAMBDA))
		  (NOT (MEMQ '&REST (CADADR FIRSTARG)))
		  (NOT (MEMQ '&KEY (CADADR FIRSTARG))))
	     ;;(call #'(lambda (x y z) ..) '(:spread :optional) (multiple-value-list ...))
	     ;;and the lambda does not have a rest arg.
	     ;;since we know how many args it wants, we can avoid consing the list of vals.
	     ;;This weird optimization is for the sake of code made by CATCH-CONTINUATION.
	     (LET ((NARGS (LDB %%ARG-DESC-MAX-ARGS (ARGS-INFO (CADR FIRSTARG)))))
	       (IF (= NARGS 1)
		   `(,(CADR FIRSTARG) ,(CADR ARGFORM))
		 `(PROGN (MULTIPLE-VALUE-PUSH ,NARGS ,(CADR ARGFORM))
			 (,(CADR FIRSTARG)
			  . ,(MAKE-LIST NARGS ':INITIAL-VALUE '(%POP)))))))
	    ;; The optimizations done on APPLY are not correct to do here,
	    ;; because they would cause the function to get an error
	    ;; if it does not want all the arguments.
	    (T FORM)))))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

(DEFUN COMPILE-1 (NAME LAMBDA-EXP &OPTIONAL (PROCESSING-MODE 'MACRO-COMPILE)
		  (NAME-FOR-FUNCTION NAME))
  "Compile LAMBDA-EXP and define NAME, while already inside the compiler environment.
NAME-FOR-FUNCTION is recorded as the name of the compiled function 
 (the default is NAME).
PROCESSING-MODE is how to compile: COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE."
  (SETQ LAMBDA-EXP (LAMBDA-MACRO-EXPAND LAMBDA-EXP))
  (COND ((ATOM LAMBDA-EXP)
	 (FDEFINE NAME LAMBDA-EXP T))
	((OR (MEMQ (CAR LAMBDA-EXP) '(LAMBDA NAMED-LAMBDA SUBST NAMED-SUBST
				      CLI:LAMBDA CLI:NAMED-LAMBDA
				      CLI:SUBST CLI:NAMED-SUBST))
	     (AND (EQ (CAR LAMBDA-EXP) 'MACRO)
		  (CONSP (CDR LAMBDA-EXP))
		  (MEMQ (CADR LAMBDA-EXP)
			'(LAMBDA NAMED-LAMBDA CLI:LAMBDA CLI:NAMED-LAMBDA))))
	 (QC-TRANSLATE-FUNCTION NAME LAMBDA-EXP PROCESSING-MODE 'COMPILE-TO-CORE
				NAME-FOR-FUNCTION))
	(T (FDEFINE NAME LAMBDA-EXP T))))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

(DEFUN MAYBE-BREAKOFF (FUNCTION &OPTIONAL LEXICAL)
    (SETQ FUNCTION (LAMBDA-MACRO-EXPAND FUNCTION))
    (COND ((ATOM FUNCTION) NIL)
	  ((MEMQ (CAR FUNCTION) '(LAMBDA CLI:LAMBDA))
	   (BREAKOFF FUNCTION LEXICAL))))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

(DEFUN QCOMPILE0 (EXP FUNCTION-TO-BE-DEFINED GENERATING-MICRO-COMPILER-INPUT-P
		  &OPTIONAL (NAME-TO-GIVE-FUNCTION FUNCTION-TO-BE-DEFINED))
  (PROG (VARS EXP1 DEF-TO-BE-SXHASHED BODY
	 PDLLVL MAXPDLLVL CALL-BLOCK-PDL-LEVELS WITHIN-CATCH
	 ALLGOTAGS TLEVEL P1VALUE BINDP LVCNT
	 DROPTHRU ALLVARS FREEVARS
	 (LOCAL-FUNCTIONS COMPILER-LEXICAL-FUNCTIONS)
	 (LOCAL-MACROS COMPILER-LEXICAL-MACROS)
	 (PROGDESCS COMPILER-LEXICAL-PROGDESCS)
	 (RETPROGDESC COMPILER-LEXICAL-RETPROGDESC)
	 (GOTAGS COMPILER-LEXICAL-GOTAGS)
	 LL TAGOUT TLFUNINIT SPECIALFLAG MACROFLAG
	 LOCAL-MAP ARG-MAP DOCUMENTATION EXPR-DEBUG-INFO
	 FAST-ARGS-POSSIBLE BREAKOFF-COUNT
	 (LEXICAL-CLOSURE-COUNT 0)
	 VARIABLES-USED-IN-LEXICAL-CLOSURES
	 ;; List of all macros found in this function, for the debugging info.
	 MACROS-EXPANDED
	 SELF-FLAVOR-DECLARATION
	 ;; Set to T during pass 1 if any SELF-REFs are present in the function.
	 SELF-REFERENCES-PRESENT
	 (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	 ;; T if this is a SUBST being compiled.  Always put interpreted defn in debug info.
	 SUBST-FLAG
	 INHIBIT-SPECIAL-WARNINGS
	 CLOBBER-NONSPECIAL-VARS-LISTS)
       (SETQ PDLLVL 0)		;RUNTINE LOCAL PDLLVL
       (SETQ DROPTHRU T)	;CAN DROP IN IF FALSE, FLUSH STUFF TILL TAG OR
       (SETQ MAXPDLLVL 0)	;DEEPEST LVL REACHED BY LOCAL PDL
       (SETQ TLEVEL T)
       (SETQ P1VALUE T)
       (SETQ FAST-ARGS-POSSIBLE T)
       (SETQ BREAKOFF-COUNT 0)
       (SETQ EXP1 EXP)
       (BEGIN-PROCESSING-FUNCTION FUNCTION-TO-BE-DEFINED)
       (WHEN (LIST-MATCH-P FUNCTION-TO-BE-DEFINED
			   `(:PROPERTY ,IGNORE :NAMED-STRUCTURE-INVOKE))
	 (WARN 'OBSOLETE-PROPERTY ':IMPLAUSIBLE
	       "NAMED-STRUCTURE-INVOKE, the property name, should not have a colon."))
       ;; If compiling a macro, compile its expansion function
       ;; and direct lap to construct a macro later.
       (COND ((EQ (CAR EXP1) 'MACRO)
	      (SETQ MACROFLAG T)
	      (SETQ EXP1 (CDR EXP1))
	      (SETQ DEF-TO-BE-SXHASHED EXP1)))
       (OR (MEMQ (CAR EXP1) '(LAMBDA SUBST CLI:LAMBDA CLI:SUBST CLI:NAMED-SUBST
			      NAMED-LAMBDA NAMED-SUBST CLI:NAMED-LAMBDA))
	   (PROGN (WARN 'FUNCTION-NOT-VALID ':FATAL "The definition is not a function at all.")
		  (RETURN NIL)))
       (IF (MEMQ (CAR EXP1) '(SUBST NAMED-SUBST CLI:SUBST CLI:NAMED-SUBST))
	   (SETQ SUBST-FLAG T INHIBIT-SPECIAL-WARNINGS T))
       ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
       (WHEN (MEMQ (CAR EXP1) '(NAMED-LAMBDA CLI:NAMED-LAMBDA NAMED-SUBST CLI:NAMED-SUBST))
	 (SETQ EXPR-DEBUG-INFO
	       (AND (NOT (ATOM (CADR EXP1)))
		    (CDADR EXP1))
	       EXP1 (CDR EXP1))
	 ;; Debug info that is equivalent to declarations
	 ;; should be turned back into declarations, coming before
	 ;; declarations made outside of compilation
	 ;; but after anything coming from a DECLARE in the body.
	 (DOLIST (ELT (REVERSE EXPR-DEBUG-INFO))
	   (WHEN (ASSQ (CAR ELT) *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	     (PUSH ELT LOCAL-DECLARATIONS))))
       (SETQ LL (CADR EXP1))	;lambda list.
       (SETQ BODY (CDDR EXP1))
       ;; Record the function's arglist for warnings about recursive calls.
       (OR THIS-FUNCTION-ARGLIST-FUNCTION-NAME
	   (SETQ THIS-FUNCTION-ARGLIST-FUNCTION-NAME NAME-TO-GIVE-FUNCTION
		 THIS-FUNCTION-ARGLIST LL))
       ;; Extract documentation string and declarations from the front of the body.
       (MULTIPLE-VALUE (BODY LOCAL-DECLARATIONS DOCUMENTATION)
	 (EXTRACT-DECLARATIONS BODY LOCAL-DECLARATIONS T))
       (SETQ SELF-FLAVOR-DECLARATION
	     (CDR (ASSQ ':SELF-FLAVOR LOCAL-DECLARATIONS)))
       ;; If the user just did (declare (:self-flavor flname)),
       ;; compute the full declaration for that flavor.
       (AND SELF-FLAVOR-DECLARATION
	    (NULL (CDR SELF-FLAVOR-DECLARATION))
	    (SETQ SELF-FLAVOR-DECLARATION
		  (CDR (SI:FLAVOR-DECLARATION (CAR SELF-FLAVOR-DECLARATION)))))
       ;; Actual DEFMETHODs must always have SELF-FLAVOR
       (AND (CONSP FUNCTION-TO-BE-DEFINED)
	    (EQ (CAR FUNCTION-TO-BE-DEFINED) ':METHOD)
	    (SETQ SELF-REFERENCES-PRESENT T))
       ;; Process &KEY and &AUX vars, if there are any.
       (WHEN (OR (MEMQ '&KEY LL) (MEMQ '&AUX LL))
	 ;; Put arglist together with body again.
	 (LET ((LAMEXP `(LAMBDA ,LL (DECLARE . ,LOCAL-DECLARATIONS) . ,BODY)))
	   ;; If there are keyword arguments, expand them.
	   (AND (MEMQ '&KEY LL)
		(SETQ LAMEXP (EXPAND-KEYED-LAMBDA LAMEXP)))
	   ;; Now turn any &AUX variables in the LAMBDA into a PROG in the body.
	   (SETQ LAMEXP (P1AUX LAMEXP))
	   ;; Separate lambda list and body again.
	   (SETQ LL (CADR LAMEXP) BODY (CDDR LAMEXP)))
	 (DO () ((NOT (AND (CONSP (CAR BODY)) (EQ (CAAR BODY) 'DECLARE))))
	   (POP BODY)))
       ;; Now process the variables in the lambda list, after the local declarations.
       (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL LOCAL-DECLARATIONS))
       (COND ((NOT (NULL (CDR BODY)))
	      (SETQ EXP1 (CONS 'PROGN BODY)))
	     ((SETQ EXP1 (CAR BODY))))
       (SETQ EXP1 (P1 EXP1))			;Do pass 1 to single-expression body
       (SETQ LVCNT (ASSIGN-LAP-ADDRESSES))
       ;; Now that we know all the variables needed by lexical closures,
       ;; make a list of them and put them into the entries in COMPILER-QUEUE
       ;; for each of those lexical closures.
       (UNLESS (ZEROP LEXICAL-CLOSURE-COUNT)
	 (SETQ VARIABLES-USED-IN-LEXICAL-CLOSURES
	       (RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES)))
       (OUTF (LIST 'MFEF FUNCTION-TO-BE-DEFINED SPECIALFLAG (ELIMINATE-DUPLICATES-AND-REVERSE ALLVARS)
		   FREEVARS NAME-TO-GIVE-FUNCTION))
       (AND MACROFLAG (OUTF '(CONSTRUCT-MACRO)))
       (OUTF '(QTAG S-V-BASE))
       (OUTF '(S-V-BLOCK))
       (IF (AND SELF-FLAVOR-DECLARATION SELF-REFERENCES-PRESENT)
	   (OUTF `(SELF-FLAVOR . ,SELF-FLAVOR-DECLARATION)))
       (OUTF '(QTAG DESC-LIST-ORG))
       (OUTF (LIST 'PARAM 'LLOCBLOCK
		   (IF (ZEROP LEXICAL-CLOSURE-COUNT)
		       LVCNT
		     (+ LVCNT (* 4 LEXICAL-CLOSURE-COUNT) 3
			(LENGTH VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
       (OUTF '(A-D-L))
       (OUTF '(QTAG QUOTE-BASE))
       (OUTF '(ENDLIST))			;Lap will insert quote vector here
       (WHEN (NOT (ZEROP LEXICAL-CLOSURE-COUNT))
	 (OUTF `(VARIABLES-USED-IN-LEXICAL-CLOSURES
		  . ,(REVERSE (MAPCAR #'(LAMBDA (HOME)
					  (LET ((TEM (VAR-LAP-ADDRESS HOME)))
					    (SELECTQ (CAR TEM)
					      (ARG (CADR TEM))
					      (T (%LOGDPB 1 %%Q-BOXED-SIGN-BIT (CADR TEM))))))
				      VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
       ;; Set up the debug info from the local declarations and other things
       (LET ((DEBUG-INFO NIL) TEM)
	 (AND DOCUMENTATION (PUSH `(:DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
	 (DOLIST (DCL *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	   (IF (SETQ TEM (ASSQ (CAR DCL) LOCAL-DECLARATIONS))
	       (IF (NEQ (CAR DCL) (CDR DCL))
		   (PUSH (CONS (CDR DCL) (CDR TEM)) DEBUG-INFO)
		 (PUSH TEM DEBUG-INFO))))
	 ;; Propagate any other kinds of debug info from the expr definition.
	 (DOLIST (DCL EXPR-DEBUG-INFO)
	   (OR (ASSQ (CAR DCL) DEBUG-INFO)
	       (PUSH DCL DEBUG-INFO)))
         (AND (PLUSP BREAKOFF-COUNT)
	      (LET ((INTERNAL-OFFSETS (MAKE-LIST BREAKOFF-COUNT)))
		(OUTF `(BREAKOFFS ,INTERNAL-OFFSETS))
		(PUSH `(:INTERNAL-FEF-OFFSETS . ,INTERNAL-OFFSETS) DEBUG-INFO)))
         ;; Include the local and arg maps if we have them.
         ;; They were built by ASSIGN-LAP-ADDRESSES.
         (AND LOCAL-MAP (PUSH `(LOCAL-MAP ,LOCAL-MAP) DEBUG-INFO))
         (AND ARG-MAP (PUSH `(ARG-MAP ,ARG-MAP) DEBUG-INFO))
	 ;; Include list of macros used, if any.
	 (WHEN MACROS-EXPANDED
	   (LET ((MACROS-AND-SXHASHES
		   (MAPCAR #'(LAMBDA (MACRONAME)
			       (LET ((HASH (EXPR-SXHASH MACRONAME)))
				 (IF (OR HASH (CONSP MACRONAME))
				     (LIST MACRONAME HASH)
				   MACRONAME)))
			   MACROS-EXPANDED)))
	     (IF QC-FILE-RECORD-MACROS-EXPANDED
		 (PROGN
		   ;; If in QC-FILE, put just macro names in the function
		   ;; but put the names and sxhashes into the file's list.
		   (PUSH `(:MACROS-EXPANDED ,MACROS-EXPANDED) DEBUG-INFO)
		   (DOLIST (M MACROS-AND-SXHASHES)
		     (OR (MEMBER M QC-FILE-MACROS-EXPANDED)
			 (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			   (PUSH (COPYTREE M) QC-FILE-MACROS-EXPANDED)))))
	       (PUSH `(:MACROS-EXPANDED ,MACROS-AND-SXHASHES)
		     DEBUG-INFO))))
	 (AND (OR (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
		  SUBST-FLAG)
	      (PUSH `(INTERPRETED-DEFINITION ,EXP) DEBUG-INFO))
	 (WHEN SUBST-FLAG
	   (LET* ((ARGS-INFO (ARGS-INFO EXP))
		  (DUMMY-FORM (CONS 'FOO
				    (MAKE-LIST (+ (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)
						  (IF (LDB-TEST %ARG-DESC-EVALED-REST ARGS-INFO)
						      1 0))
					       ':INITIAL-ELEMENT '(GENSYM)))))
	     (UNLESS (EQUAL (SI:SUBST-EXPAND EXP DUMMY-FORM)
			    (SI:SUBST-EXPAND EXP DUMMY-FORM T))
	       ;; If simple and thoughtful substitution give the same result
	       ;; even with the most intractable arguments,
	       ;; we need not use thoughtful substitution for this defsubst.
	       ;; Otherwise, mark it as requiring thoughtful substitution.
	       (PUSH '(:NO-SIMPLE-SUBSTITUTION T) DEBUG-INFO))))
	 ;; Compute the sxhash now, after all displacing macros have been displaced
	 (AND MACROFLAG
	      (PUSH `(:EXPR-SXHASH ,(FUNCTION-EXPR-SXHASH DEF-TO-BE-SXHASHED)) DEBUG-INFO))
	 ;; If we aren't going to mark this function as requiring a mapping
	 ;; table, provide anyway some info that the user declared it wanted one.
	 (AND SELF-FLAVOR-DECLARATION (NOT SELF-REFERENCES-PRESENT)
	      (PUSH `(:SELF-FLAVOR ,(CAR SELF-FLAVOR-DECLARATION)) DEBUG-INFO))
	 (AND DEBUG-INFO
              (OUTF `(DEBUG-INFO . ,DEBUG-INFO))))
       (OUTF 'PROGSA)
       (P2SBIND LL VARS NIL)			;Can compile initializing code
       (LET ((LEXICAL-CLOSURE-COUNT 0))
	 (P2 EXP1 'D-RETURN))			;Do pass 2
       (OUTF (LIST 'PARAM 'MXPDL (1+ MAXPDLLVL)))
       (RETURN ALLVARS)))

))

; From file QCP1.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCP1  "

(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	(NUMBER-OF-REQUIRED-KEYWORDS 0))
    (COND ((MEMQ (CAR LAMBDA-EXP) '(LAMBDA CLI:LAMBDA))
	   (SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP)))
	  (T
	   (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP))))
    (MULTIPLE-VALUE (POSITIONAL-ARGS NIL AUXVARS
		     REST-ARG POSITIONAL-ARG-NAMES
		     KEYKEYS KEYNAMES NIL KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    ;; For each keyword arg, decide whether we need to nit it to KEYWORD-GARBAGE
    ;; and check explicitly whether that has been overridden.
    ;; If the arg is optional
    ;; and the initial value is a constant, we can really init it to that.
    ;; Otherwise, we change its KEYINITS element to
    ;; KEYWORD-GARBAGE and push a cleanup form on KEYCHECKS.
    (DO ((KIS KEYINITS (CDR KIS))
	 (KNS KEYNAMES (CDR KNS))
	 (KKS KEYKEYS (CDR KKS))
	 (KFS KEYFLAGS (CDR KFS))
	 (MAP-BIT-NUMBER 0 (1+ MAP-BIT-NUMBER)))
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
    
    ;; Add some declarations to inform user of the fact that this compiled
    ;; function still logically wants keyword args.
    ;; The arglist info can be overridden by the user's explicit declaration.
    (COND ((ASSQ 'ARGLIST LOCAL-DECLARATIONS))
	  ((ASSQ ':ARGLIST LOCAL-DECLARATIONS))
	  (T
	   (PUSH `(ARGLIST . ,(LDIFF LAMBDA-LIST AUXVARS)) LOCAL-DECLARATIONS)))

    ;; If the user didn't ask for a rest arg, make one for the
    ;; outer function anyway.
    (OR REST-ARG (SETQ REST-ARG (GENSYM)
		       MAYBE-REST-ARG (LIST '&REST REST-ARG)))

    ;; Put our list of variable names onto CLOBBER-NONSPECIAL-VARS-LISTS
    ;; so that ASSIGN-LAP-ADDRESSES will clobber out the variables
    ;; which are not special with NIL.
    (PUSH KEYNAMES CLOBBER-NONSPECIAL-VARS-LISTS)
    `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG
	      &AUX ,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) KEYNAMES KEYINITS)
	      ,@KEYFLAGS)
;       (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;	      (OR (%PERMUTE-ARGS)
;		  (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;			   (CDR ,REST-ARG)
;			   (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;			   ',KEYKEYS)
;			 (%PERMUTE-ARGS)))
;	      ;; If the function really wants the rest arg,
;	      ;; flush the permutation table and its keyword.
;	      ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;	     (T
       (WHEN ,(IF (PLUSP NUMBER-OF-REQUIRED-KEYWORDS) T REST-ARG)
	 (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
				      ,REST-ARG ',KEYKEYS
				      ,ALLOW-OTHER-KEYS
				      ,NUMBER-OF-REQUIRED-KEYWORDS
				      ',KEYNAMES))
;	      ))
       ,@KEYCHECKS
       ((LAMBDA ,AUXVARS . ,BODY)))))

))

; From file QCLUKE.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "Z: L; QCLUKE  "

(defun (let-if cw-handler) (exp)
  (let ((cond (cw-expression (cadr exp))))
    (multiple-value-bind (nil bindlist)
	(cw-parallel-binding-list (caddr exp))
      (let ((body (cw-clause (cdddr exp))))
	(if cw-return-expansion-flag
	    `(let-if ,cond ,bindlist . ,body))))))

(defprop and cw-eval-args cw-handler)
(defprop or cw-eval-args cw-handler)
(defprop setq cw-eval-args cw-handler)
(defprop login-setq cw-eval-args cw-handler)
(defprop progn cw-eval-args cw-handler)
(defprop progv cw-eval-args cw-handler)
(defprop progw cw-eval-args cw-handler)
(defprop unwind-protect cw-eval-args cw-handler)
(defprop dont-optimize cw-eval-args cw-handler)
(defprop eval-when cw-first-arg-quoted cw-handler)
(defprop multiple-value-list cw-eval-args cw-handler)
(defprop nth-value cw-eval-args cw-handler)
(defprop cli:throw cw-eval-args cw-handler)
(defprop si:advise-progn cw-eval-args cw-handler)
(putprop 'si:advise-let #'(:property let cw-handler) 'cw-handler)
(defprop si:advise-setq cw-eval-args cw-handler)
(defprop si:setq-if-unbound cw-eval-args cw-handler)
(defprop si:advise-multiple-value-list cw-eval-args cw-handler)
(defprop patch-source-file cw-first-arg-quoted cw-handler)
(defprop si:defvar-1 cw-first-arg-quoted cw-handler)
(defun (si:advise-prog cw-handler) (exp)
  (cw-prog-form exp 'cw-parallel-binding-list))
(putprop 'si:encapsulation-let #'(:property let cw-handler) 'cw-handler)
(defprop si:*catch-for-eval cw-eval-args cw-handler)
(defun (si:matchcarcdr cw-handler) (exp)
  (let ((arg (cw-expression (cadr exp)))
	(car (cw-lambda-expression (caddr exp)))
	(cdr (cw-lambda-expression (cadddr exp))))
    (if cw-return-expansion-flag `(si:matchcarcdr ,arg ,car ,cdr))))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN DESCRIBE-PACKAGE (PKG)
  "Describes thoroughly the package PKG (a package or the name of one).
The only thing not mentioned is what symbols are in the package.
Use MAPATOMS for that."
    (SETQ PKG (PKG-FIND-PACKAGE PKG))
    (FORMAT T "~%Package ~A" (PKG-NAME PKG))
    (WHEN (PKG-NICKNAMES PKG)
      (PRINC " (")
      (DO ((NAMES (PKG-NICKNAMES PKG) (CDR NAMES))
	   (FIRST T NIL))
	  ((NULL NAMES))
	(UNLESS FIRST (PRINC ", "))
	(PRINC (CAR NAMES)))
      (PRINC ")"))
    (PRINC ".")
    (FORMAT T "~&  ~D. symbols out of ~D.  Hash modulus = ~D.~&"
	    (PKG-NUMBER-OF-SYMBOLS PKG)
	    (PKG-MAX-NUMBER-OF-SYMBOLS PKG)
	    (PKG-NUMBER-OF-SLOTS PKG))
    (WHEN (PKG-REFNAME-ALIST PKG)
      (FORMAT T "Refname alist:~%")
      (DOLIST (L (PKG-REFNAME-ALIST PKG))
	(FORMAT T "    ~20A~S~%" (CAR L) (CDR L))))
    (FORMAT T "~@[Packages which USE this one:~&~{    ~A~&~}~]" (PKG-USED-BY-LIST PKG))
    (FORMAT T "~@[Super-package ~A~&~]" (PKG-SUPER-PACKAGE PKG T))
    (FORMAT T "~@[Packages which are USEd by this one:~&~{    ~A~&~}~]" (PKG-USE-LIST PKG))
    (FORMAT T "~@[Shadowed symbols:~&~{    ~S~&~}~]" (PKG-SHADOWING-SYMBOLS PKG))
    (FORMAT T "~@[Symbols are interned in this package using ~S~]" (PKG-STORE-FUNCTION PKG))
    (FORMAT T "~@[Symbols interned in this package are automatically exported.~%~]"
	    (PKG-AUTO-EXPORT-P PKG))
    PKG)

))

(setf (documentation 'select-match 'function)
  "Execute the first clause whose pattern matches the value of OBJECT.
The syntax is 

   (SELECT-MATCH OBJECT
     (`PATTERN CONDITION CLAUSE-BODY...)
     (`PATTERN CONDITION CLAUSE-BODY...)
     ...
     (`PATTERN CONDITION CLAUSE-BODY...)
     (OTHERWISE CLAUSE-BODY...)

The value of OBJECT is matched against the PATTERNs one at a time until a
match succeeds and the accompanying CONDITION evaluates to non-NIL.
Then the CLAUSE-BODY of that clause is executed and its last expression's
value is returned.

,VARIABLE can appear in a pattern; it matches anything, and the variable
is bound to what it matched for the execution of the CONDITION and CLAUSE-BODY.
If one variable appears twice in a pattern, it must match EQUAL objects
in both occurrences:
    (SELECT-MATCH '(A B C) 
      (`(,X B ,X) T 'LOSE)
      (`(,X B ,Y) T 'WIN)
      (OTHERWISE 'LOSE-BIG))
returns WIN.  Use ,IGNORE to match anything and not use it.")

; From file SELEV.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SELEV  "

(DEFUN SELECT-MATCH-CLAUSE (CLAUSE OBJECTVAR)
  (IF (MEMQ (CAR CLAUSE) '(OTHERWISE :OTHERWISE T))
      (CONS 'T (CDR CLAUSE))
    (LET ((PATCOND (SELECT-MATCH-MATCHITEMS (CAR CLAUSE) OBJECTVAR)))
      (CONS (CONS 'AND (IF (EQ (CADR CLAUSE) 'T)
			   (LIST PATCOND)
			 (LIST PATCOND (CADR CLAUSE))))
	    (CDDR CLAUSE)))))

))

(setf (documentation 'compiler:cw-top-level 'function)
  "Return a list of free variables, block names and go tags used by expression EXP.
CW-FUNCTION-ENVIRONMENT has the same format as SI:INTERPRETER-FUNCTION-ENVIRONMENT.
 It is used to record local macros available
 and local function definitions that may be shadowing global macro definitions.
CW-RETURN-EXPANSION-FLAG if non-NIL says expand macros to all levels
 and construct a macro-free form, returned as the fifth value.
The first value lists the free variables,
 (but only symbols present in the argument ALL-VARIABLES-TO-CHECK-FOR are mentioned),
the second lists function symbols used free
 (but only symbols present in the argument ALL-FUNCTIONS-TO-CHECK-FOR are mentioned),
the third value lists the free block names (including possibly NIL),
the fourth lists the free go tags.
the fifth is the macroexpanded form, but only if CW-RETURN-EXPANSION-FLAG is non-NIL.
ALL-VARIABLES-to-CHECK-FOR may also be T, meaning return all variables used free.")

(setf (documentation 'compiler:cw-top-level-lambda-expression 'function)
  "Return a list of free variables, block names and go tags used by expression EXP.
CW-FUNCTION-ENVIRONMENT has the same format as SI:INTERPRETER-FUNCTION-ENVIRONMENT.
 It is used to record local macros available
 and local function definitions that may be shadowing global macro definitions.
CW-RETURN-EXPANSION-FLAG if non-NIL says expand macros to all levels
 and construct a macro-free form, returned as the fifth value.
The first value lists the free variables,
 (but only symbols present in the argument ALL-VARIABLES-TO-CHECK-FOR are mentioned),
the second lists function symbols used free
 (but only symbols present in the argument ALL-FUNCTIONS-TO-CHECK-FOR are mentioned),
the third value lists the free block names (including possibly NIL),
the fourth lists the free go tags.
the fifth is the macroexpanded form, but only if CW-RETURN-EXPANSION-FLAG is non-NIL.
ALL-VARIABLES-to-CHECK-FOR may also be T, meaning return all variables used free.")

