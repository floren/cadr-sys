;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for System version 100.3
;;; Reason:
;;;  Fix (BASIC-MENU :MOUSE-MOVES) crash.
;;; Written 10-Apr-23 02:00:57 by AMS,
;;; while running on Lisp Machine One from band 8
;;; with Experimental System 100.2, Hacks by AMS 1.0, Experimental Local-File 54.0, Experimental MagTape 23.0, Experimental FILE-Server 11.0, microcode 323.



; From file OZ: /home/ams/l/sys/window/menu.lisp at 10-Apr-23 02:01:04
#10R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//window//menu"

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
	       (T						;Columnated, find which column
		(SETQ COLN (TRUNCATE XREL COLUMN-WIDTH))	;Column selected
		(SETQ ITEM (CAR (NTHCDR COLN ITEMS)))		;This may be NIL
		(SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH))
		(SETQ BLX (+ (* COLN COLUMN-WIDTH)		;Start of column
			     (MAX 0 (TRUNCATE (- COLUMN-WIDTH	;Centering
						 MENU-INTERCOLUMN-SPACING
						 BLWIDTH)
					      2))))))))
  (MULTIPLE-VALUE-BIND (NIL ITEM-FONT)
      (MENU-ITEM-STRING ITEM CURRENT-FONT SELF)
    (SETQ ITEM-BASELINE-ADJUST (- BASELINE (FONT-BASELINE ITEM-FONT)))
    ;; If this item is non-selectable, don't select it.
    (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM)))
	 (NOT (ATOM (CDDR ITEM)))
	 (EQ (CADR ITEM) ':NO-SELECT)
	 (SETQ ITEM NIL))
    ;; Now make the blinker be where and what we have just found it should be.
    (BLINKER-SET-VISIBILITY BLINKER (NOT (NULL ITEM)))
    (SETQ CURRENT-ITEM ITEM)
    (COND (ITEM
	   (SEND BLINKER :SET-SIZE-AND-CURSORPOS 
			 (+ BLWIDTH 1)
			 (+ (FONT-CHAR-HEIGHT ITEM-FONT)
			    0)
;		         1)
			 BLX
;		         (+ BLX 1)
			 (+ (* ROW ROW-HEIGHT) ITEM-BASELINE-ADJUST))))))
))
