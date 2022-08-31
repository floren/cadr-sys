;;; -*- Mode:LISP; Package:TV; Readtable:ZL; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.24
;;; Reason:
;;;  symbol-plist works only on symbols; use plist to hack instances as well
;;;  (locf (plist ...)) works on instances (sends :property-list-location)
;;;  (setf (the ...) ...)
;;;  Speedups to tv:sheet-line-out and to zmacs redisplay by KHS. Yow!
;;;  Buglet in "Just lisp" layout configarations (for what they're worth)
;;; Written 2-Mar-85 11:37:27 by Mly,
;;; while running on Sarah Bernhardt from band 2
;;; with System 99.21, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.


; From file OZ:OZ:<MLY.LL>QRAND.LISP.7 at 6-Mar-85 00:53:07
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(remprop 'symbol-plist 'si::setf-method)
(remprop 'symbol-plist 'si::locf-method)

(DEFSUBST SYMBOL-PLIST (SYMBOL)
  "Return the contents of the property list of SYMBOL."
  (CONTENTS (PROPERTY-CELL-LOCATION SYMBOL)))

(DEFUN PLIST (OBJECT-WITH-PROPERTY-LIST)
  "Return the contents of the property list of OBJECT-WITH-PROPERTY-LIST,
which may be a symbol, an instance supporting the :PROPERTY-LIST operation,
or a locative or cons cell whose cdr is the property list."
 (ETYPECASE OBJECT-WITH-PROPERTY-LIST
   (SYMBOL (CONTENTS (LOCF (SYMBOL-PLIST OBJECT-WITH-PROPERTY-LIST))))
   ((OR CONS LOCATIVE) (CDR OBJECT-WITH-PROPERTY-LIST))
   ((OR INSTANCE NAMED-STRUCTURE) (SEND OBJECT-WITH-PROPERTY-LIST :PROPERTY-LIST))))

;; used by (locf (plist ...))
(DEFUN PLIST-LOCATION (OBJECT-WITH-PROPERTY-LIST)
  (ETYPECASE OBJECT-WITH-PROPERTY-LIST
    (SYMBOL (PROPERTY-CELL-LOCATION OBJECT-WITH-PROPERTY-LIST))
    ((OR CONS LOCATIVE) (LOCF (CDR OBJECT-WITH-PROPERTY-LIST)))
    ((OR INSTANCE NAMED-STRUCTURE) (SEND OBJECT-WITH-PROPERTY-LIST :PROPERTY-LIST-LOCATION))))

(DEFUN SETPLIST (OBJECT-WITH-PROPERTY-LIST VALUE)
  (ETYPECASE OBJECT-WITH-PROPERTY-LIST
    (SYMBOL (SETF (SYMBOL-PLIST OBJECT-WITH-PROPERTY-LIST) VALUE))
    ((OR CONS LOCATIVE) (SETF (CDR OBJECT-WITH-PROPERTY-LIST) VALUE))
    ((OR INSTANCE NAMED-STRUCTURE)
     (SEND OBJECT-WITH-PROPERTY-LIST :SET-PROPERTY-LIST VALUE))))


))

; From file OZ:OZ:<MLY.LL>SETF.LISP.5 at 6-Mar-85 00:56:03
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(deflocf plist plist-location)

))

; From file OZ:OZ:<MLY.LL>SETF.LISP.6 at 11-Mar-85 19:19:11
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ:OZ:<MLY.LL>SETF.."

(define-setf-method the (type place)
  ;; Handle (values t1 t2 .. tn), but we can't do anything interesting with
  ;; the other values for now, though this method could handle it.
  ;; By the way, this is mostly for the benefit of the microcompiler.
  (when (eq (car-safe type) 'values)
    (setq type (second type)))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method place)
    (values tempvars tempargs storevars
	    (sublis (mapcar #'(lambda (var) (cons var `(the ,type ,var)))
			    storevars)
		    storeform)
	    `(the ,type ,refform))))

))

; From file OZ:OZ:<MLY.LL>ZWSCREEN.LISP.1 at 11-Mar-85 21:53:42
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(defmethod (editor-typeout-window :before :deactivate) ()
  (tv:prepare-sheet (self)
    (let ((bottom (min (tv:sheet-inside-bottom) (1+ (send self :bottom-reached)))))
      (tv:%draw-rectangle (tv:sheet-inside-width)
			  (- bottom (tv:sheet-inside-top))
			  (tv:sheet-inside-left)
			  (tv:sheet-inside-top)
			  (tv:sheet-erase-aluf self)
			  self))))

))

; From file OZ:OZ:<MLY.LL>SHWARM.LISP.1 at 11-Mar-85 21:54:03
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(defun %draw-string (sheet alu xpos ypos string font start stop xlim)
  "Draw STRING on SHEET starting with the character at index START and stopping after
drawing the character at index STOP, presuming it all fits.  Output starts at XPOS,
YPOS on the sheet and continues until all appropriate characters are drawn, or until
the next character to be drawn would extend past XLIM.  The index of the next character
to be drawn, and the xpos where it would go are returned.  If a NEWLINE is encountered,
returns its index and xpos immediately.  The sheet's cursor position is ignored and
left unchanged."
  ;; Who says you can't write assembly code on the Lisp Machine?
  (declare (values index xpos))
  (prog (c (i start) width (tab-width (sheet-tab-width sheet)) (base-ypos ypos) (npos xpos)
	 font-index font-next (font-map (sheet-font-map sheet))
	 font-index-table font-width-table font-kern-table lozenge-string
	 (inside-left (sheet-inside-left sheet)))

	(cond (( i stop)
	       (return (values (1+ stop) xpos)))
	      ((neq (%p-mask-field-offset #.%%array-type-field string 0) art-string)
	       (go multiple-font)))

   single-font
	(when (fixnump font) (setq font (aref font-map font)))
	(setq ypos (+ ypos (- (sheet-baseline sheet) (font-baseline font))))
	(cond ((setq font-index-table (font-indexing-table font))
	       (setf (sheet-cursor-x sheet) xpos)
	       (go single-hairy-font-loop))
	      ((or (setq font-width-table (font-char-width-table font))
		   (setq font-kern-table (font-left-kern-table font)))
	       (setq width (font-char-width font))
	       (go single-variable-width-font-loop))
	      (t
	       (setq width (font-char-width font))
	       (go single-fixed-width-font-loop)))

   single-fixed-width-font-loop
	(when ( (setq c (char-int (char string i))) #o200)
	  (case c
	    (#.(char-int #/tab)     (go single-fixed-width-font-tab))
	    (#.(char-int #/newline) (return (values i npos)))
	    (otherwise (go single-fixed-width-font-hard))))
   single-fixed-width-font-char
	(when (> (setq npos (+ (setq xpos npos) width)) xlim)
	  (return (values i xpos)))
	(%draw-char font c xpos ypos alu sheet)
	(when (eq (incf i) stop)
	  (return (values (1+ stop) npos)))
	(if (< (setq c (char-int (char string i))) #o200)
	    (go single-fixed-width-font-char)
	  (case c
	    (#.(char-int #/tab)     (go single-fixed-width-font-tab))
	    (#.(char-int #/newline) (return (values i npos)))
	    (otherwise (go single-fixed-width-font-hard))))
   single-fixed-width-font-tab
	(setq npos (+ (* (truncate (+ (setq xpos npos) tab-width) tab-width)
			 tab-width)
		      inside-left))
	(cond ((> npos xlim)
	       (return (values i xpos)))
	      ((eq (incf i) stop)
	       (return (values (1+ stop) npos)))
	      (t (go single-fixed-width-font-loop)))
   single-fixed-width-font-hard
	(setq lozenge-string (or (car (rassq c si::xr-special-character-names))
				 (format nil "~O" c)))
	(setq npos (+ (setq xpos npos) (lozenged-string-width lozenge-string)))
	(when (> npos xlim)
	  (return (values i xpos)))
	(setf (sheet-cursor-x sheet) xpos)
	(sheet-display-lozenged-string sheet lozenge-string)
	(if (eq (incf i) stop)
	    (return (values (1+ stop) npos))
	  (go single-fixed-width-font-loop))

   single-variable-width-font-loop
	(when ( (setq c (char-int (char string i))) #o200)
	  (case c
	    (#.(char-int #/tab)     (go single-variable-width-font-tab))
	    (#.(char-int #/newline) (return (values i npos)))
	    (otherwise (go single-variable-width-font-hard))))
   single-variable-width-font-char
        (when (> (setq npos (+ (setq xpos npos)
			       (if font-width-table (aref font-width-table c) width))) xlim)
	  (return (values i xpos)))
	(if font-kern-table
	    (%draw-char font c (- xpos (aref font-kern-table c)) ypos alu sheet)
	  (%draw-char font c xpos ypos alu sheet))
	(if (eq (incf i) stop)
	    (return (values (1+ stop) npos))
	  (go single-variable-width-font-loop))
   single-variable-width-font-tab
	(setq npos (+ (* (truncate (+ (setq xpos npos) tab-width) tab-width)
			 tab-width)
		      inside-left))
	(cond ((> npos xlim)
	       (return (values i xpos)))
	      ((eq (incf i) stop)
	       (return (values (1+ stop) npos)))
	      (t (go single-variable-width-font-loop)))
   single-variable-width-font-hard
	(setq lozenge-string (or (car (rassq c si:xr-special-character-names))
				 (format nil "~O" c)))
	(setq npos (+ (setq xpos npos) (lozenged-string-width lozenge-string)))
	(when (> npos xlim)
	  (return (values i xpos)))
	(setf (sheet-cursor-x sheet) xpos)
	(sheet-display-lozenged-string sheet lozenge-string)
	(if (eq (incf i) stop)
	    (return (values (1+ stop) npos))
	  (go single-variable-width-font-loop))

   single-hairy-font-loop
	(setq width (sheet-character-width sheet (setq c (char-int (char string i))) font))
	(when (> (+ (sheet-cursor-x sheet) width) xlim)
	  (return (values i (sheet-cursor-x sheet))))
	(sheet-tyo sheet c font)
	(if (eq (incf i) stop)
	    (return (values (1+ stop) (sheet-cursor-x sheet)))
	  (go single-hairy-font-loop))

   multiple-font
	(setq font-next (char-font (setq c (char string i))))
   multiple-font-main-loop-font-changed
	(setq font (aref font-map (setq font-index font-next)))
	(setq font-index-table (font-indexing-table font))
	(setq font-width-table (font-char-width-table font))
	(setq font-kern-table (font-left-kern-table font))
	(setq width (font-char-width font))
	(setq ypos (+ base-ypos (- (sheet-baseline sheet) (font-baseline font))))
   multiple-font-main-loop-font-unchanged
	(when ( (setq c (char-code c)) #o200)
	  (go multiple-font-special-character))

   multiple-font-graphic-character
	(when (> (setq npos (+ (setq xpos npos)
			       (if font-width-table (aref font-width-table c) width)))
		 xlim)
	  (return (values i xpos)))
	(when (null font-index-table)
	  (if font-width-table
	      (if font-kern-table
		  (go multiple-font-variable-width-with-kerning-loop-short-cut)
		(go multiple-font-variable-width-loop-short-cut))
	    (go multiple-font-fixed-width-loop-short-cut)))
	(setf (sheet-cursor-x sheet) xpos)	;Must transmit latest position.
	(sheet-tyo sheet c font)		;Indexed character (wider than 32 bits).
	(go multiple-font-loop-tail)

   multiple-font-special-character
	(case c
	  (#.(char-int #/tab)
	   (when (> (setq npos (+ (setq xpos npos)
				  (- tab-width
				     (\ (- xpos inside-left)
					tab-width))))
		    xlim)
	     (return (values i xpos))))
	  (#.(char-int #/newline)
	   (return (values i (setq xpos npos))))
	  (otherwise
	   (setq lozenge-string (or (car (rassq c si::xr-special-character-names))
				    (format nil "~O" c)))
	   (when (> (setq npos (+ (setq xpos npos)
				  (lozenged-string-width lozenge-string)))
		    xlim)
	     (return (values i xpos)))
	   (setf (sheet-cursor-x sheet) xpos)
	   (sheet-display-lozenged-string sheet lozenge-string)))

   multiple-font-loop-tail
	(cond ((eq (incf i) stop)
	       (return (values (1+ stop) npos)))
	      ((eq (setq font-next (char-font (setq c (char string i)))) font-index)
	       (go multiple-font-main-loop-font-unchanged))
	      (t
	       (go multiple-font-main-loop-font-changed)))
	
   multiple-font-fixed-width-loop
	(cond (( (setq c (char-code c)) #o200)
	       (go multiple-font-special-character))
	      ((> (setq npos (+ (setq xpos npos) width))
		  xlim)
	       (return (values i xpos))))
   multiple-font-fixed-width-loop-short-cut
	(%draw-char font c xpos ypos alu sheet)
	(cond ((eq (incf i) stop)
	       (return (values (1+ stop) npos)))
	      ((eq (setq font-next (char-font (setq c (char string i)))) font-index)
	       (go multiple-font-fixed-width-loop))
	      (t
	       (go multiple-font-main-loop-font-changed)))

   multiple-font-variable-width-loop
	(cond (( (setq c (char-code c)) #o200)
	       (go multiple-font-special-character))
	      ((> (setq npos (+ (setq xpos npos)
				(if font-width-table (aref font-width-table c) width)))
		  xlim)
	       (return (values i xpos))))
   multiple-font-variable-width-loop-short-cut
	(%draw-char font c xpos ypos alu sheet)
	(cond ((eq (incf i) stop)
	       (return (values (1+ stop) npos)))
	      ((eq (setq font-next (char-font (setq c (char string i)))) font-index)
	       (go multiple-font-variable-width-loop))
	      (t
	       (go multiple-font-main-loop-font-changed)))

   multiple-font-variable-width-with-kerning-loop
	(cond (( (setq c (char-code c)) #o200)
	       (go multiple-font-special-character))
	      ((> (setq npos (+ (setq xpos npos)
				(if font-width-table (aref font-width-table c) width)))
		  xlim)
	       (return (values i xpos))))
   multiple-font-variable-width-with-kerning-loop-short-cut
	(%draw-char font c (- xpos (aref font-kern-table c)) ypos alu sheet)
	(cond ((eq (incf i) stop)
	       (return (values (1+ stop) npos)))
	      ((eq (setq font-next (char-font (setq c (char string i)))) font-index)
	       (go multiple-font-variable-width-with-kerning-loop))
	      (t
	       (go multiple-font-main-loop-font-changed)))))

))

; From file OZ:OZ:<MLY.LL>SHWARM.LISP.1 at 11-Mar-85 21:54:07
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(defun sheet-line-out (sheet string &optional (start 0) (stop nil)
					      set-xpos set-ypos dwidth (clear t))
  ;; Returns index of next character to do and where cursor got to except the first value can
  ;; be incremented, to show that the line was completed (as though it counted an implicit
  ;; carriage return).
  (declare (values index xpos))
  (let* ((inside-right (sheet-inside-right sheet))
	 (inside-left (sheet-inside-left sheet))
	 (margin-flag (not (zerop (sheet-right-margin-character-flag sheet))))
	 (xpos (if set-xpos (+ set-xpos inside-left) (sheet-cursor-x sheet)))
	 (ypos (if set-ypos (+ set-ypos (sheet-inside-top sheet))
		 (sheet-cursor-y sheet)))
	 (stop-index)
	 (stop-xpos))
  (prepare-sheet (sheet)
    (setq stop (or stop (array-active-length string)))
    (setf (sheet-cursor-y sheet) ypos)		;%DRAW-STRING depends on this in some cases.
;    (%draw-rectangle
;      (- inside-right xpos)
;      (sheet-line-height sheet)
;      xpos
;      ypos
;      (sheet-erase-aluf sheet)
;      sheet)
    (cond ((fixnump clear)
	   (if ( (+ inside-left clear) (- inside-right #o20))
	       (%draw-rectangle (- inside-right xpos) (sheet-line-height sheet)
				xpos ypos
				(sheet-erase-aluf sheet) sheet)
	       (%draw-rectangle (- (+ inside-left clear) xpos) (sheet-line-height sheet)
				xpos ypos
				(sheet-erase-aluf sheet) sheet)))
	  ((null clear))
	  (t
	   (%draw-rectangle (- inside-right xpos) (sheet-line-height sheet)
			    xpos ypos
			    (sheet-erase-aluf sheet) sheet)))
    (when dwidth				;Italic correction, back up one character.
      (setq xpos (- xpos dwidth))
      (decf start))
    (multiple-value-setq (stop-index stop-xpos)
      (%draw-string sheet (sheet-char-aluf sheet) xpos ypos string 0 start stop
		    (if margin-flag (- inside-right (sheet-char-width sheet)) inside-right)))
    (when (< stop-index stop)
      (when margin-flag (send sheet :tyo-right-margin-character stop-xpos ypos #/!)))
    (values stop-index (- stop-xpos inside-left)))))

))

; From file OZ:KANSAS:<L.ZWEI>DISPLA.LISP.157 at 11-Mar-85 21:53:12
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "

(DEFMETHOD (WINDOW :REDISPLAY) (RECENTER-TYPE RC1 RC2 FORCE-TO-COMPLETION-P)
  (PREPARE-WINDOW-FOR-REDISPLAY SELF)
  (BLOCK ABORT-REDISPLAY
    (LET ((LH TV:LINE-HEIGHT)
	   (NOW (TICK))
	   POINT-PLINE
	   (POINT-LINE (BP-LINE POINT))
	   (POINT-INDEX (BP-INDEX POINT))
	   (TOP-LINE (BP-LINE START-BP))
	   (TOP-INDEX (BP-INDEX START-BP))
	   (LAST-BP (INTERVAL-LAST-BP INTERVAL))
	   (INITIAL-DEGREE REDISPLAY-DEGREE)
	   ;; Bind *INTERVAL* in case we decide to call any primitives, e.g. inside the
	   ;; special-blinker which blinks matching parens.  This is an implicit argument.
	   (*INTERVAL* INTERVAL)
	   ;; These are for debugging only.
	   POINT-NODE START-BP-NODE BUF)
      ;; We prefer not to start redisplay in the middle of a line.
      ;; The start-bp of the window may have ended up there via a command like rubout at
      ;; the beginning of the window or may have been scrolled there explicitly.  If the
      ;; top line has changed so that continuation may not be necessary any more, attempt
      ;; recentering.
      (AND (EQ RECENTER-TYPE ':POINT)
	   (NOT (ZEROP TOP-INDEX))
	   (> N-PLINES 1)
	   (> (LINE-TICK TOP-LINE) (OR (PLINE-TICK SELF 0) 0))
	   (LET ((NEW-TOP-INDEX (IF (EQ TOP-LINE (BP-LINE (INTERVAL-FIRST-BP INTERVAL)))
				    (BP-INDEX (INTERVAL-FIRST-BP INTERVAL))
				  0)))
	     (AND ( TOP-INDEX NEW-TOP-INDEX)
		  (< (NTH-VALUE 1
		       (TV:SHEET-COMPUTE-MOTION SELF 0 0
						TOP-LINE NEW-TOP-INDEX POINT-INDEX
						NIL 0
						MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM))
		     (* LH N-PLINES))
		  (SETQ RECENTER-TYPE ':ABSOLUTE))))
      ;; :POINT recentering is a conditional sort of :ABSOLUTE recentering.
      ;; So decide here whether :ABSOLUTE recentering should be done.
      (WHEN (EQ RECENTER-TYPE ':POINT)
	(COND (( REDISPLAY-DEGREE DIS-MARK-GOES))
	      ((AND (BP-= POINT (INTERVAL-LAST-BP *INTERVAL*))
		    (BP-= POINT START-BP)
		    (NOT (BP-= POINT (INTERVAL-FIRST-BP *INTERVAL*))))
	       ;; Don't let display be empty at end of buffer.
	       (SETQ RECENTER-TYPE ':ABSOLUTE))
	      ;; When typing at the end of the line, don't try to compute POINT-PLINE
	      ;; yet, but wait till after we have faked out the pline-text-width
	      ;; correctly. Otherwise it will be much, much slower
	      ((AND (= REDISPLAY-DEGREE DIS-LINE)
		    (EQ REDISPLAY-LINE POINT-LINE)
		    (NEQ POINT-LINE (PLINE-LINE SELF (1- N-PLINES)))
		    (OR ( (1+ REDISPLAY-INDEX) POINT-INDEX)
			(= (NTH-VALUE 1
			     (TV:SHEET-COMPUTE-MOTION SELF 0 0
						      POINT-LINE 0 POINT-INDEX T))
			   0))))
	      ((SETQ POINT-PLINE (PLINE-OF-POINT T SELF POINT)))
	      (T
	       (SETQ RECENTER-TYPE ':ABSOLUTE))))
      ;; If recentering is needed, do it, and see what changes it made.
      (UNLESS (MEMQ RECENTER-TYPE '(:NONE :POINT))
	(RECENTER-WINDOW SELF RECENTER-TYPE RC1 RC2)
	(SETQ TOP-LINE (BP-LINE START-BP)
	      TOP-INDEX (BP-INDEX START-BP)
	      POINT-LINE (BP-LINE POINT)
	      POINT-INDEX (BP-INDEX POINT))
	;; Gobble point-pline as computed by recenter-window
	;; if it is accurate.
	(SETQ POINT-PLINE LAST-POINT-PLINE)
	(OR (AND (EQ POINT-LINE (PLINE-LINE SELF POINT-PLINE))
		 ( (PLINE-FROM-INDEX SELF POINT-PLINE) POINT-INDEX)
		 (< POINT-INDEX (PLINE-TO-INDEX SELF POINT-PLINE)))
	    (SETQ POINT-PLINE NIL)))
      ;; Now we have TOP-LINE and TOP-INDEX, and possibly POINT-PLINE.

      ;; First, handle the case where just one line needs to be updated.
      (WHEN (= REDISPLAY-DEGREE DIS-LINE)
	(LET ((LINE REDISPLAY-LINE)
	      (INDEX REDISPLAY-INDEX))
	  (LET ((P (FIND-BP-IN-WINDOW SELF LINE INDEX))
		(LINE-LENGTH (IF (EQ LINE (BP-LINE LAST-BP))
				 (BP-INDEX LAST-BP)
			       (LINE-LENGTH LINE)))
		LEN DWID)
	    ;; LEN gets the raster position in the pline P
	    ;; of the character in LINE at position INDEX.
	    (WHEN P
	      (IF (AND P (EQ (PLINE-LINE SELF P) *LAST-REDISPLAY-LINE*)
		       (= INDEX *LAST-REDISPLAY-INDEX*)
		       (EQ SELF *LAST-REDISPLAY-SHEET*))
		  (SETQ LEN *LAST-REDISPLAY-CURSOR-X*)
		(SETQ LEN (STRING-WIDTH LINE (PLINE-FROM-INDEX SELF P) INDEX SELF))))
	    (COND ((AND P
			;; If P and LEN say we are at the start of a continuation line,
			;; then maybe they are wrong
			;; (if the contin line has been exactly deleted).
			(OR (NOT (ZEROP LEN))
			    (ZEROP INDEX)))
		   ;; Reverse-video region marking must be removed before updating.
		   (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
			(OR (PLINE-MARKING-LEFT SELF P)
			    (PLINE-MARKING-LEFT SELF (MIN (1+ P) (1- N-PLINES))))
			(REGION-UNMARK-PLINE P))
		   ;; Go to the place in the line where changes start.
		   ;; Clear from there.
		   ;; This means that any region marking from there on is gone now.
		   (COND ((AND (PLINE-MARKING-LEFT SELF P)
			       (< (PLINE-MARKING-LEFT SELF P) LEN))
			  (SETF (PLINE-MARKING-WIDTH SELF P)
				(MIN (- LEN (PLINE-MARKING-LEFT SELF P))
				     (PLINE-MARKING-WIDTH SELF P))))
			 (T (SETF (PLINE-MARKING-LEFT SELF P) NIL)
			    (SETF (PLINE-MARKING-WIDTH SELF P) NIL)))
		   ;; If the character is wider than it claims to be, draw an extra
		   ;; character, since the clear-eol will erase data.
		   (OR (ZEROP INDEX)
		       (LET ((CH (AREF LINE (1- INDEX))))
			 (AND (< (CHAR-CODE CH) #o200)
			      (LET ((FONT (AREF (TV:SHEET-FONT-MAP SELF)
						(CHAR-FONT CH)))
				    CWT)
				(AND (SETQ CWT (FONT-CHAR-WIDTH-TABLE FONT))
				     (LET ((CWID (AREF CWT (SETQ CH (CHAR-CODE CH))))
					   (RWID (FED:FONT-CHAR-MIN-RASTER-WIDTH FONT CH)))
				       (AND (> RWID CWID) (SETQ DWID CWID))))))))
		   (MULTIPLE-VALUE-BIND (I TW)
		       (TV:SHEET-LINE-OUT SELF LINE INDEX LINE-LENGTH LEN (* LH P) DWID)
		     ;; Save cursor x to avoid calls to STRING-WIDTH
		     ;;  while inserting text.
		     (SETQ *LAST-REDISPLAY-SHEET* SELF
			   *LAST-REDISPLAY-LINE* LINE
			   *LAST-REDISPLAY-INDEX* LINE-LENGTH
			   *LAST-REDISPLAY-CURSOR-X* TW)
		     ;; We have output the first PLINE of this line
		     (SETF (PLINE-TO-INDEX SELF P) I)
		     (SETF (PLINE-TEXT-WIDTH SELF P)
			   (IF ( I LINE-LENGTH) TW	;Continuation needed
			     (+ TW (TV:SHEET-CHAR-WIDTH SELF)))) ;Allow for CR
		     (SETF (PLINE-TICK SELF P) NOW)
		     ;; See if plines below this need to be redisplayed, due
		     ;; to line-continuation issues
		     (COND ((AND (< (1+ P) N-PLINES)
				 (OR ( I LINE-LENGTH)
				     ( (+ TW (TV:SHEET-INSIDE-LEFT SELF))
					(TV:SHEET-INSIDE-RIGHT SELF))
				     (EQ (PLINE-LINE SELF (1+ P)) LINE)))
			    (SETQ REDISPLAY-DEGREE DIS-TEXT POINT-PLINE NIL)
			    ;; If we are just creating a new continuation line, make it
			    ;; still look munged, so REDISPLAY-BLT can understand.
			    (OR (EQ (PLINE-LINE SELF (1+ P)) LINE)
				(SETF (PLINE-TICK SELF P) -1))))))
		  (T
		   (SETQ REDISPLAY-DEGREE DIS-TEXT POINT-PLINE NIL))))))
      ;; If all the window should be redisplayed, mark each pline as unknown.
      (WHEN ( REDISPLAY-DEGREE DIS-ALL)
	(TV:SHEET-CLEAR SELF T)
	(SEND SELF :REFRESH-MARGINS)
	(DO ((I 0 (1+ I)))
	    ((= I N-PLINES))
	  (SETF (PLINE-TICK SELF I) -1)
	  (SETF (PLINE-MARKING-LEFT SELF I) NIL)))
      (WHEN ( REDISPLAY-DEGREE DIS-TEXT)
	(SETQ *LAST-REDISPLAY-LINE* NIL)
	;; In case we abort before we are done, don't forget what's needed.
	(SETF REDISPLAY-DEGREE DIS-TEXT)
	(SETF LAST-BP-DISPLAYED-P NIL)
	(DO ((L SPECIAL-BLINKER-LIST (CDR L)))
	    ((NULL L))
	  (SEND (CDAR L) :SET-VISIBILITY NIL))
	;; Abort now if input available
	(AND (NOT FORCE-TO-COMPLETION-P)
	     (SEND *STANDARD-INPUT* :LISTEN)
	     (RETURN-FROM ABORT-REDISPLAY NIL))
	;; Attempt to do insert and delete line cleverness.
	(REDISPLAY-BLT)
	;; This might have invalidated the value of POINT-PLINE.
	;; It won't be hard to recompute, so do so.
	(SETQ POINT-PLINE NIL)
	;; First loop over actual lines.
	(BLOCK LINES
	  (DO ((LINE TOP-LINE (LINE-NEXT LINE))
	       (FROM-INDEX TOP-INDEX 0)
	       (TO-INDEX)
	       (PLINE 0)
	       (check-input-count 0)
	       (STOP-LINE (BP-LINE LAST-BP)))
	      (NIL)
	    ;; Between lines, check for input available and abort if so.
	    (incf check-input-count)
	    (unless force-to-completion-p
	      (when (> check-input-count 20.)
		(setq check-input-count 0)
		(if (send *standard-input* :listen) (return-from abort-redisplay nil))))
;		 (AND (NOT FORCE-TO-COMPLETION-P)
;		      (ZEROP (\ PLINE 30.))
;		      (SEND *STANDARD-INPUT* ':LISTEN)
;		      (RETURN-FROM ABORT-REDISPLAY NIL))

	    (SETQ TO-INDEX (IF (EQ LINE STOP-LINE) (BP-INDEX LAST-BP)
			       (LINE-LENGTH LINE)))
	    ;; Now loop over the plines of this line.
	    (DO-FOREVER
	      (AND ( PLINE N-PLINES) (RETURN-FROM LINES))
	      ;; Check for a line that has not been changed.
	      (COND ((AND (EQ LINE (PLINE-LINE SELF PLINE))
			  (> (PLINE-TICK SELF PLINE) (LINE-TICK LINE))
			  (= (PLINE-FROM-INDEX SELF PLINE) FROM-INDEX))
		     (SETQ FROM-INDEX (PLINE-TO-INDEX SELF PLINE)))
		    (T
		     ;; Reverse-video region marking must be removed before updating.
		     (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
			  (OR (PLINE-MARKING-LEFT SELF PLINE)
			      (PLINE-MARKING-LEFT SELF
						  (MIN (1+ PLINE) (1- N-PLINES))))
			  (REGION-UNMARK-PLINE PLINE))
		     ;; This should work differently
		     (LET ((FROB (GETF (LINE-PLIST LINE) ':DIAGRAM)) I TW)
		       (COND (FROB
			      (TV:SHEET-SET-CURSORPOS SELF 0 (* LH PLINE))
			      (TV:SHEET-CLEAR-EOL SELF)
			      (SEND FROB :DRAW LINE SELF)
			      (SETQ I 1 TW 0))
			     (T
			      (MULTIPLE-VALUE (I TW)
				(TV:SHEET-LINE-OUT
				  SELF LINE FROM-INDEX TO-INDEX
				  0 (* LH PLINE)
				  nil
				  (and (pline-line self pline)
				       (pline-text-width self pline))))))
		       (SETF (PLINE-LINE SELF PLINE) LINE)
		       (SETF (PLINE-FROM-INDEX SELF PLINE) FROM-INDEX)
		       (SETF (PLINE-TO-INDEX SELF PLINE) I)
		       (SETF (PLINE-TICK SELF PLINE) NOW)
		       (SETF (PLINE-MARKING-LEFT SELF PLINE) NIL)
		       (SETF (PLINE-TEXT-WIDTH SELF PLINE)
			     (IF ( I (LINE-LENGTH LINE)) TW	;Continuation needed
				 (+ TW (TV:SHEET-CHAR-WIDTH SELF))))	;Allow for CR
		       (SETQ FROM-INDEX I))))
	      (INCF PLINE)
	      ;; This is >, not , because if line isn't cont'd then PLINE-TO-PLINE
	      ;; counts the phony CR which is output by SHEET-LINE-OUT.
	      (WHEN (> FROM-INDEX TO-INDEX)
		(RETURN)))
	    ;; Check for the last line in the interval.
	    (COND ((EQ LINE STOP-LINE)
		   (SETF LAST-BP-DISPLAYED-P T)
		   (OR (< PLINE N-PLINES) (RETURN-FROM LINES))
		   (AND (NULL (PLINE-LINE SELF PLINE))
			(PLINE-TICK SELF PLINE) (> (PLINE-TICK SELF PLINE) 0)
			(RETURN-FROM LINES)) ;Return if screen already blanked
		   ;; Reverse-video region marking must be removed before updating.
		   (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
			(OR (PLINE-MARKING-LEFT SELF PLINE)
			    (PLINE-MARKING-LEFT SELF
						(MIN (1+ PLINE) (1- N-PLINES))))
			(REGION-UNMARK-PLINE PLINE))
		   ;; Clean out the rest of the window beneath it.  Then exit.
		   (TV:SHEET-SET-CURSORPOS SELF 0 (* LH PLINE))
		   (TV:SHEET-CLEAR-EOF SELF)
		   (DO ((PLINE PLINE (1+ PLINE)))
		       (( PLINE N-PLINES))
		     (SETF (PLINE-LINE SELF PLINE) NIL)
		     (SETF (PLINE-TICK SELF PLINE) NOW)
		     (SETF (PLINE-MARKING-LEFT SELF PLINE) NIL))
		   (RETURN-FROM LINES))))))
      ;; These are just for debugging the errors reported below.
      (SETQ POINT-NODE (BP-TOP-LEVEL-NODE POINT)
	    START-BP-NODE (BP-TOP-LEVEL-NODE START-BP)
	    BUF *INTERVAL*)
      (WHEN ( REDISPLAY-DEGREE DIS-BPS)
	;; BPs have moved.  Reposition the POINT blinker.
	(OR POINT-PLINE
	    (SETQ POINT-PLINE (FIND-BP-IN-WINDOW SELF POINT-LINE POINT-INDEX))
	    (EQ RECENTER-TYPE ':NONE)
	    (IF (AND (= INITIAL-DEGREE DIS-LINE) (= REDISPLAY-DEGREE DIS-TEXT))
		;; Somewhat anomalous case, try again with greater redisplay degree
		(RETURN-FROM ABORT-REDISPLAY
		  (REDISPLAY SELF RECENTER-TYPE RC1 RC2 FORCE-TO-COMPLETION-P))
	      (UNWIND-PROTECT
		  (CERROR "Try to fix things up"
			  "Recenter type ~S left point outside the window."
			  RECENTER-TYPE)
		;; Try to clean things up so error won't repeat.
		(MOVE-BP POINT (INTERVAL-FIRST-BP INTERVAL))
		(MOVE-BP START-BP (INTERVAL-FIRST-BP INTERVAL)))))
	(COND ((NULL POINT-PLINE)
	       ;; POINT is not on the window, so make it go away.
	       (SEND POINT-BLINKER :SET-VISIBILITY NIL))
	      (T
	       ;; POINT is on the window, find its Y position.
	       (SEND POINT-BLINKER :SET-VISIBILITY (IF (EQ SELF TV:SELECTED-WINDOW)
						       ':BLINK
						     (TV:BLINKER-DESELECTED-VISIBILITY
						       POINT-BLINKER)))
	       (UNLESS (EQ POINT-LINE (PLINE-LINE SELF POINT-PLINE))
		 (DPRINT POINT-LINE POINT-PLINE (PLINE-LINE SELF POINT-PLINE))
		 (UNWIND-PROTECT
		     (CERROR "Try to fix things up"
			     "Position of POINT on window is screwed up.")
		   ;; Try to clean things up so error won't repeat.
		   (MOVE-BP POINT (INTERVAL-FIRST-BP INTERVAL))
		   (MOVE-BP START-BP (INTERVAL-FIRST-BP INTERVAL))))
	       (SET-BLINKER-SIZE POINT POINT-BLINKER
				 (TV:SHEET-COMPUTE-MOTION SELF 0 0 POINT-LINE
							  (PLINE-FROM-INDEX SELF POINT-PLINE)
							  POINT-INDEX)
				 (* LH POINT-PLINE))
	       (SETF LAST-POINT-PLINE POINT-PLINE)))
	;; Blink the parens, etc.
	(DOLIST (BL SPECIAL-BLINKER-LIST)
	  (FUNCALL (CAR BL) (CDR BL) SELF POINT START-BP)))
      (WHEN ( REDISPLAY-DEGREE DIS-MARK-GOES)
	;; The region marking may have changed.
	(UPDATE-REGION-MARKING))
      ;; The character under the mouse also
      (WHEN ( REDISPLAY-DEGREE DIS-BPS)
	(MOUSE-RETHINK SELF))
      (WHEN ( REDISPLAY-DEGREE DIS-TEXT)
	(SEND SELF :NEW-SCROLL-POSITION))
      (SETF REDISPLAY-DEGREE DIS-NONE)
      )))

))


; From file OZ:KANSAS:<L.WINDOW>SYSMEN.LISP.178 at 11-Mar-85 22:20:17
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFWINDOW-RESOURCE SCREEN-LAYOUT-MENU ()
  :MAKE-WINDOW (MOMENTARY-MENU
		 :NAME "Screen Layout Menu"
		 :LABEL "Screen Layouts"
		 :ITEM-LIST `(("Just Lisp" :EVAL
			       (LET ((SUPERIOR (SEND TV:SELECTED-WINDOW :SUPERIOR)))
				 `((,(IDLE-LISP-LISTENER SUPERIOR)
				    ,(SEND TV:SELECTED-WINDOW :STATUS)
				    ,(SHEET-INSIDE-LEFT SUPERIOR)
				    ,(SHEET-INSIDE-TOP SUPERIOR)
				    ,(SHEET-INSIDE-RIGHT SUPERIOR)
				    ,(SHEET-INSIDE-BOTTOM SUPERIOR)))))
			      ("Save This" :EVAL
			       (PROGN (SAVE-THIS-SCREEN-LAYOUT) NIL))))
    :REUSABLE-WHEN :DEEXPOSED)

(CLEAR-RESOURCE 'SCREEN-LAYOUT-MENU)
))
