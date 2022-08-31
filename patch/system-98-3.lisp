;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11/28/83 20:23:58 by Mly,
;;; Reason: fed doesn't make unnecessary kern tables
;;; defstruct documentation; macros permanently loaded; :eval-when flushed;
;;;           subtype determined at defstruct time
;;; structures print with #s(...) syntax by default.
;;; #s(...) #a(...) reader error recovery
;;; *PRINT-ARRAY* bound to NIL in inspector
;;; *EVALHOOK* lossage
;;; New font TR12BI. Color fonts.
;;; while running on Lisp Machine Eighteen from band 6
;;; with Experimental System 98.1, CADR 3.0, Experimental ZMail 53.0, MIT-Specific 22.0, microcode 305, ZM MIT.

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFUN FONT-STORE-CD (FONTNAME CHAR &OPTIONAL (UPDATE-FONT-FLAG T)
		      &AUX FD CD YSTART XSTART YWIDTH XWIDTH KERN
		      PLANE-X1 PLANE-Y1 PLANE-WIDTH PLANE-HEIGHT)
  "Store the current FED data into character CHAR of FONTNAME.
If UPDATE-FONT-FLAG is NIL, only the font-descriptor for that font
is updated, not the actual font."
  (DECLARE (:SELF-FLAVOR FED))
  (PROG FONT-STORE-CD ()
	;; Find the FD format array for this font.
	(SETQ FD (FONT-GET-FD FONTNAME))
	;; Warn if char box now displayed is incompatible with the font.
	(COND ((OR ( (- CHAR-BOX-Y2 CHAR-BOX-Y1) (FD-BASELINE FD))
		   ( (- CHAR-BOX-Y3 CHAR-BOX-Y1) (FD-LINE-SPACING FD)))
	       (OR (Y-OR-N-P
		     "Character height and baseline are incompatible with font.
If actually stored, the character will be aligned by the
top of its box.
Proceed to store anyway?")
		   (RETURN-FROM FONT-STORE-CD NIL))))
	;; What are the regions of the fed data plane which actually are stored?
	(SETQ PLANE-X1 (FIRST (PLANE-ORIGIN PLANE)))
	(SETQ PLANE-Y1 (SECOND (PLANE-ORIGIN PLANE)))
	(SETQ PLANE-WIDTH (FIRST (ARRAY-DIMENSIONS PLANE)))
	(SETQ PLANE-HEIGHT (SECOND (ARRAY-DIMENSIONS PLANE)))
	;; Figure out what portion of the plane holding the fed data is really nonzero.
	;;   XSTART and YSTART get the indices in PLANE (as an array, not as a plane!)
	;;   of what is going to go into the upper left corner of the CD.
	;;   XWIDTH and YWIDTH get the dimensions which the CD will need to hold all nonzero data.
	;;   XSTART is determined by the leftmost nonzero data, and its distance from
	;;   CHAR-BOX-X1 determines the left kern.  YSTART has to correspond to CHAR-BOX-Y1
	;;   because that is not a per-character parameter.
	(SETQ YSTART (- CHAR-BOX-Y1 PLANE-Y1) YWIDTH 0)
	(DO J (MAX YSTART 0) (1+ J) (= J PLANE-HEIGHT)
	    (DO I 0 (1+ I) (= I PLANE-WIDTH)
		(OR (ZEROP (AREF PLANE I J))
		    (SETQ YWIDTH (1+ (- J YSTART))))))
	(SETQ XSTART NIL XWIDTH 0)
	(DO I 0 (1+ I) (= I PLANE-WIDTH)
	    (DO J (MAX YSTART 0) (1+ J) (= J (+ YSTART YWIDTH))
		(COND ((NOT (ZEROP (AREF PLANE I J)))
		       (OR XSTART (SETQ XSTART I))
		       (SETQ XWIDTH (1+ (- I XSTART)))))))
	;; Make sure XSTART isn't NIL, and neither width is zero.
	(COND ((NULL XSTART)
	       (SETQ XSTART 0 XWIDTH 1)))
	(AND (ZEROP YWIDTH) (SETQ YWIDTH 1))
	;; Warn about dots to be lost above YSTART.
	(PROG FOO ()
	      (DO I 0 (1+ I) (= I PLANE-WIDTH)
		  (DO J 0 (1+ J) (>= J YSTART)
		      (OR (ZEROP (AREF PLANE I J))
			  (COND ((Y-OR-N-P
				   "Dots above character top will be lost. Store anyway? ")
				 (RETURN-FROM FOO NIL))
				(T (RETURN-FROM FONT-STORE-CD NIL)))))
		  (DO J (+ YSTART YWIDTH) (1+ J) (>= J PLANE-HEIGHT)
		      (OR (ZEROP (AREF PLANE I J))
			  (COND ((Y-OR-N-P
				   "Dots below character bottom will be lost. Store anyway? ")
				 (RETURN-FROM FOO NIL))

				(T (RETURN-FROM FONT-STORE-CD NIL)))))))
	(SETQ KERN (- CHAR-BOX-X1 XSTART PLANE-X1))
	;; Copy the data in the FED buffer into a CD.
	(SETQ CD (MAKE-CHAR-DESCRIPTOR
		   :MAKE-ARRAY (:TYPE 'ART-4B :DIMENSIONS (LIST YWIDTH XWIDTH))
		   CD-CHAR-WIDTH (- CHAR-BOX-X2 CHAR-BOX-X1)
		   CD-CHAR-LEFT-KERN KERN))
	(DO J 0 (1+ J) (= J YWIDTH)
	    (IF (OR (MINUSP (+ J YSTART))
		    (> (+ J YSTART) PLANE-HEIGHT))
		;; If we are outside the existing plane,
		;; fetch zeros.
		(DOTIMES (I XWIDTH)
		  (SETF (AREF CD J I) 0))
	      (DOTIMES (I XWIDTH)
		(SETF (AREF CD J I)
		      (AREF PLANE (+ I XSTART) (+ J YSTART))))))
	(COND (UPDATE-FONT-FLAG
	       ;; Use the CD just made to update the font itself, or make a new font.
	       (FONT-NAME-STORE-CD FONTNAME CD CHAR))
	      (T
	       ;; Store the CD in the FD.
	       (AND (>= CHAR (ARRAY-LENGTH FD))
		    (ADJUST-ARRAY-SIZE FD (1+ CHAR)))
	       (AS-1 CD FD CHAR)
	       (AND (= CHAR #\SP)
		    (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))))))

))

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFUN COM-READ-FILE (&AUX FD FILENAME TYPE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ TYPE (FED-CHOOSE '(("KST") (:QFASL) ("AC") ("AL") ("KS") ("AST"))
			 "Read which format of font file"))
  (IF (NULL TYPE)
      NIL
    (SETQ FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Read" TYPE))
    (SETQ CURRENT-FONT (INTERN (FUNCALL FILENAME ':NAME) "FONTS")))
  (COND ((STRING-EQUAL TYPE "KST")
	 (SETQ FD (READ-KST-INTO-FONT-DESCRIPTOR FILENAME CURRENT-FONT))
	 (PUTPROP CURRENT-FONT FILENAME 'KST-FILE)
	 (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT FD))
	((STRING-EQUAL TYPE "AST")
	 (SETQ FD (READ-AST-INTO-FONT-DESCRIPTOR FILENAME CURRENT-FONT))
	 (PUTPROP CURRENT-FONT FILENAME 'AST-FILE)
	 (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT FD))
	((STRING-EQUAL TYPE "QFASL")
	 (LOAD FILENAME "FONTS"))
	((STRING-EQUAL TYPE "AC")
	 (READ-AC-INTO-FONT FILENAME CURRENT-FONT))
	((STRING-EQUAL TYPE "KS")
	 (READ-KS-INTO-FONT FILENAME CURRENT-FONT))
	((STRING-EQUAL TYPE "AL")
	 (READ-AL-INTO-FONT FILENAME CURRENT-FONT)))
  (FUNCALL-SELF ':REDEFINE-MARGINS)
  (SELECT-FONT CURRENT-FONT))

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "


;;; Memoizing version of FONT-INTO-FONT-DESCRIPTOR
;;; that wants a font name (symbol in FONTS:) rather than the font itself.
;;; The FONT-DESCRIPTOR property of the symbol holds the descriptor.
;;; The FONT-DESCRIBED property holds the font itself which the descriptor matches.
;;; If anyone changes the font, we can see that the old descriptor is no good.
(DEFUN FONT-NAME-FONT-DESCRIPTOR (FONTNAME &AUX FD)
  "Return a font-descriptor whose data is equivalent to font FONTNAME.
If we computed one previously, we return it; otherwise, we create one.
This assumes that anyone who alters the contents of the font
also alters the corresponding font descriptor."
  (OR (SYMBOLP FONTNAME)
      (SETQ FONTNAME (FONT-NAME FONTNAME)))
  (SETQ FD (GET FONTNAME 'FONT-DESCRIPTOR))
  (COND ((AND FD (EQ (GET FONTNAME 'FONT-DESCRIBED) (SYMEVAL FONTNAME))))
	(T (SETQ FD (FONT-INTO-FONT-DESCRIPTOR (SYMEVAL FONTNAME)))
	   (PUTPROP FONTNAME (SYMEVAL FONTNAME) 'FONT-DESCRIBED)
	   (PUTPROP FONTNAME FD 'FONT-DESCRIPTOR)))
  FD)

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "


(DEFUN FONT-NAME-STORE-CD (FONTNAME CD CHAR-CODE &AUX FONT)
  "Store character CHAR-CODE in font named FONTNAME from data in CD.
CD should be a CHARACTER-DESCRIPTOR.  Both the font itself
and any remembered font-descriptor are modified.
A new font array is constructed if the new character's raster size
is too big for the old one."
  (LET ((WIDTH (ARRAY-DIMENSION CD 1))
	(HEIGHT (ARRAY-DIMENSION CD 0))
	TEM FD)
    (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME))
    (FD-STORE-CD FD CD CHAR-CODE)
    (AND (= CHAR-CODE #\SP)
	 (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))
    (COND ((OR (NOT (BOUNDP FONTNAME))
	       (NULL (SETQ FONT (SYMEVAL FONTNAME)))
	       ( CHAR-CODE (MAX (OR (FONT-FILL-POINTER FONT) 200) 200))
	       (> WIDTH
		  (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
			 (* (FONT-RASTER-WIDTH FONT)
			    (- (AREF TEM (1+ CHAR-CODE))
			       (AREF TEM CHAR-CODE))))
			(T (FONT-RASTER-WIDTH FONT))))
	       (> HEIGHT (FONT-RASTER-HEIGHT FONT)))
	   (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD))
	  (T (STORE-CD-IN-FONT CD FONT CHAR-CODE NIL)))))

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "


;Functions for referring to specified pixels of characters in an internal format font.

;ROW and COL are measured from top/left as usual.  An alternative would be:
;	COL is measured from the left, with Kerning hacked.
;	ROW is positive above the baseline and negative below.
;  (SETQ ROW (- (FONT-BASELINE FONT) ROW))
;  (AND (SETQ TEM (FONT-LEFT-KERN-TABLE FONT))
;       (SETQ COL (+ COL (AREF TEM CHAR))))
;However it looks like this would cause more trouble than it would save.
;Attempts to reference outside of the raster return 0, or barf if storing.
;Conceivably it might be good to not barf at attempts to store 0 out of bounds?

(DEFUN FONT-GET-PIXEL (FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  "Get the pixel at position ROW, COL in character CHAR of FONT.
FONT should be a font array, not a name."
  (COND ((OR (< ROW 0)
	     ( ROW (FONT-RASTER-HEIGHT FONT))
	     (< COL 0)
	     (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
		    (SETQ CHAR (+ (AREF TEM CHAR) (TRUNCATE COL (FONT-RASTER-WIDTH FONT))))
		    (SETQ COL (\ COL (FONT-RASTER-WIDTH FONT)))
		    ( CHAR (AREF TEM NEXTCHAR)))
		   (( COL (FONT-RASTER-WIDTH FONT)))))
	 0)	;out of bounds, return 0
	(T
	 (DO ((FONT FONT (FONT-NEXT-PLANE FONT))
	      (PIXEL 0)
	      (PLANENUM 0 (1+ PLANENUM)))
	     ((NULL FONT) PIXEL)
	   (SETQ PIXEL
		 (+ PIXEL (LSH (AREF FONT
				     (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT) CHAR)
						  (TRUNCATE ROW (FONT-RASTERS-PER-WORD FONT))))
                                        (+ (* (FONT-RASTER-WIDTH FONT)
                                              (\ ROW (FONT-RASTERS-PER-WORD FONT)))
                                           COL)))
			       PLANENUM)))))))

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "


;This function takes an FD format font and creates an internal format
;	font from it.  All of the hairy formats of the stored font
;	are taken care of by this function so the user doesn't have
;	to worry about them.

(DEFUN FONT-DESCRIPTOR-INTO-FONT
       (FONT-DESCRIPTOR
	&OPTIONAL (NBR-PLANES-OUT NIL)
	&AUX (FONT-OUT NIL)
	(FONT-DESCRIPTOR-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR))
	(FONT-LENGTH (MAX FONT-DESCRIPTOR-LENGTH 200))
	(COL-INCR (COND ((FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) 2)
			(T 1)))
	(SPACE-WIDTH (OR (FIX (+ (FD-SPACE-WIDTH FONT-DESCRIPTOR) 0.5)) 0))
	(WIDTH (TRUNCATE SPACE-WIDTH COL-INCR))
	(HEIGHT (FD-LINE-SPACING FONT-DESCRIPTOR))
	(BASELINE (FD-BASELINE FONT-DESCRIPTOR))
	(RASTER-WIDTH (MAX 1 (CEILING (MAX-RASTER-WIDTH FONT-DESCRIPTOR)
				      COL-INCR)))
	(RASTER-HEIGHT (MAX-RASTER-HEIGHT FONT-DESCRIPTOR))
	(RASTERS-PER-WORD (TRUNCATE 32. (MIN 32. RASTER-WIDTH)))
	(WORDS-PER-RASTER-ELEMENT (CEILING RASTER-HEIGHT RASTERS-PER-WORD))
	(TOTAL-RASTER-ELEMENTS FONT-LENGTH)
	(BLINKER-WIDTH (FLOOR (FD-BLINKER-WIDTH FONT-DESCRIPTOR) COL-INCR))
	(BLINKER-HEIGHT (FD-BLINKER-HEIGHT FONT-DESCRIPTOR))
	(INDEXING-TABLE NIL)
	(CHARS-EXIST-TABLE (MAKE-ARRAY FONT-LENGTH ':TYPE 'ART-1B))
	TEMP					;General temporary
	)
  "Create a font-array from font-descriptor FONT-DESCRIPTOR.
Its name is set from that in FONT-DESCRIPTOR."
  ;;Set up NBR-PLANES-OUT if defaulted
  (COND ((NULL NBR-PLANES-OUT)
	 (SETQ NBR-PLANES-OUT COL-INCR)))
  ;;Create INDEXING-TABLE if needed
  (COND ((> RASTER-WIDTH 32.)
	 (SETQ INDEXING-TABLE (MAKE-ARRAY (1+ FONT-LENGTH) ':TYPE 'ART-16B))
	 (ASET 0 INDEXING-TABLE 0)
	 (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	     (( CHAR-CODE FONT-LENGTH)
	      (SETQ TOTAL-RASTER-ELEMENTS (AREF INDEXING-TABLE FONT-LENGTH)))
	   (SETQ TEMP (AND (< CHAR-CODE FONT-DESCRIPTOR-LENGTH)
			   (AREF FONT-DESCRIPTOR CHAR-CODE)))
	   (ASET (+ (AREF INDEXING-TABLE CHAR-CODE)
		    (COND ((NULL TEMP) 0)
			  (T (CEILING (ARRAY-DIMENSION TEMP 1) 32.))))
		 INDEXING-TABLE (1+ CHAR-CODE)))
	 (SETQ RASTER-WIDTH 32.)))
  ;;set up all the planes of the font
  (DO ((I NBR-PLANES-OUT (1- I)))
      ((ZEROP I))
    ;;Make up a (one-plane) font and make it's next plane be the last one we made
    (SETQ TEMP (TV:MAKE-FONT :MAKE-ARRAY (:TYPE 'ART-1B
					       :LENGTH (* TOTAL-RASTER-ELEMENTS
							  WORDS-PER-RASTER-ELEMENT 32.))))
    (SETF (FONT-NEXT-PLANE TEMP) FONT-OUT)
    (SETQ FONT-OUT TEMP)
    ;;Now set all the other fields in the leader
    (SETF (FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
    (SETF (FONT-CHAR-WIDTH FONT-OUT) WIDTH)
    (SETF (FONT-CHAR-HEIGHT FONT-OUT) HEIGHT)
    (SETF (FONT-RASTER-WIDTH FONT-OUT) RASTER-WIDTH)
    (SETF (FONT-RASTER-HEIGHT FONT-OUT) RASTER-HEIGHT)
    (SETF (FONT-RASTERS-PER-WORD FONT-OUT) RASTERS-PER-WORD)
    (SETF (FONT-WORDS-PER-CHAR FONT-OUT) WORDS-PER-RASTER-ELEMENT)
    (SETF (FONT-BASELINE FONT-OUT) BASELINE)
    (SETF (FONT-BLINKER-WIDTH FONT-OUT) BLINKER-WIDTH)
    (SETF (FONT-BLINKER-HEIGHT FONT-OUT) BLINKER-HEIGHT)
    (SETF (FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
    (SETF (FONT-CHARS-EXIST-TABLE FONT-OUT) CHARS-EXIST-TABLE)
    (SETF (FONT-INDEXING-TABLE FONT-OUT) INDEXING-TABLE)
    (SETF (FONT-FILL-POINTER FONT-OUT) FONT-LENGTH))
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
      (( CHAR-CODE FONT-LENGTH))
    (SETQ TEMP (AND (< CHAR-CODE FONT-DESCRIPTOR-LENGTH)
		    (AREF FONT-DESCRIPTOR CHAR-CODE)))
    (COND (TEMP
	   (STORE-CD-IN-FONT TEMP FONT-OUT CHAR-CODE
			     (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR)))))
  FONT-OUT)

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "


(DEFUN READ-AST-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME &AUX FD)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:IN))
    (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME :MAKE-ARRAY (:LENGTH 200)))
    (READ-AST-DN STREAM)			;DISCARD KSTID
    (SETF (FD-LINE-SPACING FD) (READ-AST-DN STREAM))
    (SETF (FD-BASELINE FD) (READ-AST-DN STREAM))
    (READ-AST-DN STREAM)			;COLUMN POSITION ADJUSTMENT
    (SETF (FD-SPACE-WIDTH FD) 0)		;Just in case no space character.
    (SETF (FD-BLINKER-HEIGHT FD)
	  (FD-LINE-SPACING FD))
    (SETF (FD-NAME FD) FONTNAME)
    (LET (KERN CHAR-CODE RASTER-WIDTH INPUT-RASTER-WIDTH CHAR-WIDTH
	  CD CH (LINE-HEIGHT (FD-LINE-SPACING FD)))
      (DO ()
	  ((NULL (READ-AST-NEXT-PAGE STREAM)))
	(SETQ CHAR-CODE (READ-AST-ON STREAM))
	(SETQ INPUT-RASTER-WIDTH (READ-AST-DN STREAM) RASTER-WIDTH INPUT-RASTER-WIDTH)
	(SETQ CHAR-WIDTH (READ-AST-DN STREAM))
	(SETQ KERN (READ-AST-DN STREAM))
	(COND ((< KERN 0)			;FED compact raster lossage
	       (SETQ RASTER-WIDTH (+ RASTER-WIDTH (ABS KERN)))
	       (SETQ KERN 0)))
	(SETQ CD (MAKE-CHAR-DESCRIPTOR
		   :MAKE-ARRAY (:TYPE ART-1B :LENGTH (LIST LINE-HEIGHT RASTER-WIDTH))))
	(SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
	(SETF (CD-CHAR-LEFT-KERN CD) KERN)
	(FD-STORE-CD FD CD CHAR-CODE)
	(AND (= CHAR-CODE #\SP)
	     (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
	(DO-NAMED TOP ((VPOS 0 (1+ VPOS)))
		  ((= VPOS LINE-HEIGHT))
	  (DO ((HCNT 0 (1+ HCNT)))
	      ((= HCNT INPUT-RASTER-WIDTH)
	       (DO ((CH)) ()
		 (COND ((OR (NULL (SETQ CH (FUNCALL STREAM ':TYI)))
			    (= CH #\RETURN))
			(RETURN NIL))
		       ((= CH #\FORM)
			(FUNCALL STREAM ':UNTYI CH)
			(RETURN-FROM TOP NIL))
		       ((NOT (= CH #\SPACE))
			(FERROR NIL "non space seen past raster width")))))
	    (SETQ CH (FUNCALL STREAM ':TYI))
	    (COND ((NULL CH)
		   (RETURN-FROM TOP NIL))
		  ((= CH #\FORM)
		   (FUNCALL STREAM ':UNTYI CH)
		   (RETURN-FROM TOP NIL))
		  ((OR (< CH 40) (> CH 200))
		   (DO () ((= CH #\RETURN)) (SETQ CH (FUNCALL STREAM ':TYI)))
		   (RETURN NIL))
		  ((> CH 40)
		   (ASET 1 CD VPOS (+ HCNT (- RASTER-WIDTH INPUT-RASTER-WIDTH)))))))
     ; (COND ((> CHAR-CODE 37) (TYO CHAR-CODE))
     ;	      (T (PRINC '^) (TYO (+ 100 CHAR-CODE))))
	)
      ;; Truncate fd to discard unused elements at the end.
      (DO ((I (1- (ARRAY-LENGTH FD)) (1- I)))
	  ((OR (MINUSP I)
	       (AREF FD I))
	   (ADJUST-ARRAY-SIZE FD (1+ I))))
      (SETF (FD-FILL-POINTER FD) (ARRAY-LENGTH FD))
      ;; Set width of blinker and space fields from the space character.
      (SETF (FD-BLINKER-WIDTH FD)
	    (FD-SPACE-WIDTH FD))
      FD)))

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "


(DEFUN WRITE-FONT-DESCRIPTOR-INTO-AST (FD &OPTIONAL FILENAME
					  &AUX (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FD)))
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "AST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:OUT :ASCII))
    (FORMAT STREAM "0 KSTID ~A"  FILENAME)
    (FORMAT STREAM "~%~D HEIGHT" (FD-LINE-SPACING FD))
    (FORMAT STREAM "~%~D BASE LINE" (FD-BASELINE FD))
    (FORMAT STREAM "~%0 COLUMN POSITION ADJUSTMENT~%")
    ;; Then write out all the characters.
    (LET (CD CHAR-HEIGHT LOSING-KERN KERN)
      (DOTIMES (CHAR-CODE FONT-LENGTH)
	(COND ((AND (SETQ CD (AREF FD CHAR-CODE))
		    ;; Wide fonts without chars-exist-tables can have 0-width chars.
		    (OR (NOT (ZEROP (ARRAY-DIMENSION CD 1)))
			(NOT (ZEROP (CD-CHAR-WIDTH CD)))))
	       (FUNCALL STREAM ':TYO #\FORM)
	       (SETQ KERN (CD-CHAR-LEFT-KERN CD)
		     LOSING-KERN (AND (MINUSP KERN)
				      ( (CD-CHAR-WIDTH CD)
					 (- (ARRAY-DIMENSION CD 1) KERN))
				      (- KERN)))
	       (FORMAT STREAM "~O CHARACTER CODE ~A" CHAR-CODE FILENAME)
	       (FORMAT STREAM "~%~D RASTER WIDTH" (IF LOSING-KERN (CD-CHAR-WIDTH CD)
						    (ARRAY-DIMENSION CD 1)))
	       (FORMAT STREAM "~%~D CHARACTER WIDTH" (CD-CHAR-WIDTH CD))
	       (FORMAT STREAM "~%~D LEFT KERN~%" (IF LOSING-KERN 0 KERN))
	       (SETQ CHAR-HEIGHT (ARRAY-DIMENSION CD 0))
	       (DOTIMES (VPOS CHAR-HEIGHT)
		 (IF LOSING-KERN (DOTIMES (HPOS LOSING-KERN)
				   (SEND STREAM ':TYO #\SPACE)))
		 (DOTIMES (HPOS (ARRAY-DIMENSION CD 1))
		   (SEND STREAM ':TYO (IF (ZEROP (AREF CD VPOS HPOS))
					     #\SPACE
					     #/*)))
		 (SEND STREAM ':TYO #\RETURN))))))
    (CLOSE STREAM)
    (SEND STREAM ':TRUENAME)))

))

; From file FNTCNV.LISP SRC:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "

;Store the data in CD into character number CHAR-CODE of FONT.
;It is assumed that the dimensions of the CD fit within the raster dimensions of the font.
;This is not recommended for users to call.
(DEFUN STORE-CD-IN-FONT (CD FONT CHAR-CODE &OPTIONAL (DOUBLE-WIDTH-P NIL) &AUX
			 (FONT-LENGTH (FONT-FILL-POINTER FONT))
			 (WIDTH (ARRAY-DIMENSION CD 1))
			 (HEIGHT (ARRAY-DIMENSION CD 0))
			 (FONT-HEIGHT (FONT-RASTER-HEIGHT FONT))
			 (FONT-WIDTH (FONT-RASTER-WIDTH FONT))
			 PIXEL (LOSING-KERN 0)
			 (COL-INCR (COND (DOUBLE-WIDTH-P 2) (T 1))))
  (OR (AND FONT-LENGTH ( FONT-LENGTH 200))
      (SETQ FONT-LENGTH 200))
  ;; Update the font's char-width-table, creating one if necessary.
  (LET ((CW (CEILING (ROUND (CD-CHAR-WIDTH CD)) COL-INCR))
	(FCW (FONT-CHAR-WIDTH FONT))
	(FCWT (FONT-CHAR-WIDTH-TABLE FONT)))
    (COND (FCWT
	   (ASET CW FCWT CHAR-CODE))
	  ((NOT (= CW FCW))
	   (SETF (FONT-CHAR-WIDTH-TABLE FONT)
		 (SETQ FCWT (MAKE-ARRAY FONT-LENGTH)))
	   (AND DOUBLE-WIDTH-P
		(SETF (FONT-CHAR-WIDTH-TABLE (FONT-NEXT-PLANE FONT))
		      FCWT))
	   (DO I 0 (1+ I) (= I FONT-LENGTH)
	       (ASET FCW FCWT I))
	   (ASET CW FCWT CHAR-CODE)))
    (AND (= CHAR-CODE #\SP)
	 (SETF (FONT-CHAR-WIDTH FONT) CW)))
  ;; Update the font's left-kern table, creating one if necessary.
  (LET ((CK (CD-CHAR-LEFT-KERN CD))
	(FCKT (FONT-LEFT-KERN-TABLE FONT)))
    ;;this is to prevent lossage with this function making left kern tables unnecessarily.
    ;; the problem is that a character with a blank left column gets a kern of -1, regardless
    ;; of whether it would fit into the raster regardless.
    (AND (MINUSP CK)
	 ( (CD-CHAR-WIDTH CD) (- WIDTH CK))
	 (SETQ LOSING-KERN (- CK) CK 0))
    (COND (FCKT (ASET CK FCKT CHAR-CODE))
	  ((NOT (ZEROP CK))
	   (SETF (FONT-LEFT-KERN-TABLE FONT)	;Must be ART-32B since left-kern can be -ve
		 (SETQ FCKT (MAKE-ARRAY FONT-LENGTH ':TYPE ART-32B)))
	   (AND DOUBLE-WIDTH-P
		(SETF (FONT-LEFT-KERN-TABLE (FONT-NEXT-PLANE FONT))
		      FCKT))
	   (ASET CK FCKT CHAR-CODE))))
  ;; Tell the font this char exists.
  (ERRSET (ASET 1 (FONT-CHARS-EXIST-TABLE FONT) CHAR-CODE) NIL)
  ;; In wide fonts, the raster width depends on the character, and is a multiple of 32.
  (COND ((FONT-INDEXING-TABLE FONT)
	 (SETQ FONT-WIDTH (* (CEILING (ARRAY-DIMENSION CD 1) 32.) 32.))))
  ;; Now copy the data.
  (DO ((ROW 0 (1+ ROW))
       (ONE-BIT-FONT (NULL (FONT-NEXT-PLANE FONT)))
       (RASTER-WIDTH (FONT-RASTER-WIDTH FONT)))
      (( ROW FONT-HEIGHT))
    (DO (
	 ;; Count columns in font descriptor.
	 (COL 0 (IF (< PIXEL-COL LOSING-KERN) COL
		  (+ COL COL-INCR)))
	 ;; Count columns in font.
	 (PIXEL-COL 0 (1+ PIXEL-COL))
	 ;; for one-bit fonts this is index in font itself of start of row.
	 ;; For multi-bit fonts it is not used.
	 (NEXT-BIT-FONT-INDEX
	   (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT)
			   (IF (FONT-INDEXING-TABLE FONT)
			       (AREF (FONT-INDEXING-TABLE FONT) CHAR-CODE)
			     CHAR-CODE))
			(FLOOR ROW (FONT-RASTERS-PER-WORD FONT))))
	      (* RASTER-WIDTH
		 (\ ROW (FONT-RASTERS-PER-WORD FONT))))
	   (1+ NEXT-BIT-FONT-INDEX)))
	(( PIXEL-COL FONT-WIDTH))
      ;; Get pixel out of font descriptor.
      ;; If font is "double width", two pixels of font descriptor
      ;; are combined into one pixel for the font itself.
      (SETQ PIXEL (COND ((< PIXEL-COL LOSING-KERN) 0)
			((OR ( COL WIDTH) ( ROW HEIGHT)) 0)
			(DOUBLE-WIDTH-P
			 (+ (COND (( (1+ COL) WIDTH) 0)
				  (T (AREF CD ROW (1+ COL))))
			    (* 2 (AREF CD ROW COL))))
			(T (AREF CD ROW COL))))
      ;; Store pixel into font.
      ;; If pixels are only one bit and chars not too wide, use a short cut.
      (COND (ONE-BIT-FONT
	     ;; In wide font, notice when our horizontal advance
	     ;; carries us into the "next character" of the many characters
	     ;; in the font which actually represent vertical strips of one character.
	     (AND (ZEROP (\ PIXEL-COL RASTER-WIDTH))
		  (NOT (ZEROP PIXEL-COL))
		  (SETQ NEXT-BIT-FONT-INDEX
			(+ NEXT-BIT-FONT-INDEX
			   (* 32. (FONT-WORDS-PER-CHAR FONT))
			   (- RASTER-WIDTH))))
	     (ASET PIXEL FONT NEXT-BIT-FONT-INDEX))
	    (T
	     (FONT-SET-PIXEL PIXEL FONT CHAR-CODE
			     ROW PIXEL-COL))))))

))

; From file COMF.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-LONG-DOCUMENTATION "Prints long documentation for the specified symbol or function.
Reads the name of the function or symbol from the mini-buffer
/(the default is the /"current/" function from the buffer).
First comes the arglist of the name as a function, if it is defined,
and then the documentation of the function.
Then, if the name is a symbol, comes the documentation for the name in its other roles." ()
  (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)))))
    (LET ((DOC (DOCUMENTATION NAME 'FUNCTION))
	  (ALL-DOC (AND (SYMBOLP NAME) (GET NAME 'SI:DOCUMENTATION-PROPERTY))))
      (IF (NOT (FDEFINEDP NAME))
	  (FORMAT T "~S:" NAME)
	(SEND STANDARD-OUTPUT ':FRESH-LINE)
	(PRINT-ARGLIST NAME STANDARD-OUTPUT))
      (WHEN DOC
	(FORMAT T "~%~A" DOC))
      (IF ALL-DOC
	  (LOOP FOR (KIND STRING) ON ALL-DOC BY 'CDDR
		UNLESS (EQ KIND 'FUNCTION)
		DO (FORMAT T "~&~%Documentation of ~S as a ~A:~%~A~%"
			   NAME (STRING-DOWNCASE KIND) STRING))
	(FORMAT QUERY-IO "~&~S is not documented" NAME))))
  DIS-NONE)

))

; From file SYSMEN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

;;; This goes in QTRACE

;;; Display Features

;; Items in this menu are lists of the form:
;;  ("name" :VALUE (S-expr-arg-p . what-to-append-into-trace-options))
;;			^-- if this is UNTRACE, QUIT, or DO-IT, that special function
;;			if NIL, nothing special
;;			otherwise is prompt for reading what goes into trace options
;; Try to keep this so it comes out in 3 columns
(DEFVAR TRACE-ITEM-LIST
	`(("Break before" :VALUE (NIL :BREAK T)
	   :DOCUMENTATION "Call BREAK before entering the function, binding ARGLIST.")
	  ("Break after" :VALUE (NIL :EXITBREAK T)
	   :DOCUMENTATION "Call BREAK after leaving the function, binding VALUES.")
	  ("Step" :VALUE (NIL :STEP)
	   :DOCUMENTATION "Single-step through the body of the (interpreted) function.")
	  ("Cond Step" :VALUE ("Predicate for stepping calls" :STEPCOND)
	   :DOCUMENTATION "Asks for a predicate which controls whether stepping happens.")
	  ("Error" :VALUE (NIL :ERROR)
	   :DOCUMENTATION "Enter the error-handler when the function is called.")
	  ("Print" :VALUE ("Form to evaluate and print in trace messages" :PRINT)
	   :DOCUMENTATION "Asks for a form to be evaluated and printed in trace messages.")
	  ("Print before" :VALUE ("Form to evaluate and print before calling" :ENTRYPRINT)
	   :DOCUMENTATION "Asks for a form to be evaluated and printed upon function entry.")
	  ("Print after" :VALUE ("Form to evaluate and print after returning" :EXITPRINT)
	   :DOCUMENTATION "Asks for a form to be evaluated and printed upon function exit.")
	  ("Conditional" :VALUE ("Predicate for tracing" :COND)
	   :DOCUMENTATION "Asks for a predicate which controls whether tracing happens.")
	  ("Cond before" :VALUE ("Predicate for tracing calls" :ENTRYCOND)
	   :DOCUMENTATION "Asks for a predicate which controls tracing of function entry.")
	  ("Cond after" :VALUE ("Predicate for tracing returns" :EXITCOND)
	   :DOCUMENTATION "Asks for a predicate which controls tracing of function exit.")
	  ("Cond break before" :VALUE ("Predicate for breaking before" :BREAK)
	   :DOCUMENTATION "Asks for a predicate which controls BREAKing on function entry.")
	  ("Cond break after" :VALUE ("Predicate for breaking after" :EXITBREAK)
	   :DOCUMENTATION "Asks for a predicate which controls BREAKing on function exit.")
	  ("ARGPDL" :VALUE ("Arg pdl variable" :ARGPDL)
	   :DOCUMENTATION "Asks for a variable which gets list of recursive argument lists.")
	  ("Wherein" :VALUE ("Function within which to trace" :WHEREIN)
	   :DOCUMENTATION "Asks for a function, within which tracing will be active.")
	  ("Untrace" :VALUE (UNTRACE) :FONT :MENU-STANDOUT
	   :DOCUMENTATION "Instead of tracing this function, stop tracing it.")
	  ("Abort" :VALUE (QUIT) :FONT :MENU-STANDOUT
	   :DOCUMENTATION "Click here to get out of this without doing anything.")
	  ("Do It" :VALUE (DO-IT) :FONT :MENU-STANDOUT
	   :DOCUMENTATION "Click here to do the tracing with the selected options.")))

))

; From file QRAND.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN ARRAY-CANONICALIZE-TYPE (TYPE &AUX FOO)
  (COND ((MEMQ TYPE ARRAY-TYPES) TYPE)
	((SETQ FOO (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))
	 (NTH FOO ARRAY-TYPES))
	((FIXNUMP TYPE)
	 (IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
	     (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
	 (NTH TYPE ARRAY-TYPES))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-append-symbols (&rest args)
  (intern (apply 'string-append args)))

(defmacro defstruct-putprop (sym val ind)
  `(push `(defprop ,,sym ,,val ,,ind) returns))

(defmacro defstruct-put-macro (sym fcn)
  (setq fcn (if (and (not (atom fcn))
                     (eq (car fcn) 'quote))
                `'(macro . ,(cadr fcn))
              `(cons 'macro ,fcn)))
  `(push `(deff-macro ,,sym ',,fcn) returns))

(defmacro make-empty () `'%%defstruct-empty%%)

(defmacro emptyp (x) `(eq ,x '%%defstruct-empty%%))

(defmacro defstruct-error (message &rest args)
  (do ((l args (cdr l))
       (fs "")
       (na nil))
      ((null l)
      `(ferror nil
               ,(string-append message
                               (if (null args)
                                   "."
                                   (string-append ":" fs)))
               ,.(nreverse na)))
    (cond ((and (not (atom (car l)))
                (eq (caar l) 'quote)
                (symbolp (cadar l)))
           (setq fs (string-append fs " " (string-downcase (cadar l)))))
          (t
           (push (car l) na)
           (setq fs (string-append fs " ~S"))))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFMACRO DEFSTRUCT (OPTIONS &BODY ITEMS)
  "(DEFSTRUCT (<name> . <options>) . <slots>) or (DEFSTRUCT <name> . <slots>)
Options:
  :TYPE defaults to :ARRAY
    other useful types include :ARRAY-LEADER :TYPED-ARRAY :FIXNUM-ARRAY :FLONUM-ARRAY
    :LIST :LIST* :TREE
  :CONSTRUCTOR defaults to /"MAKE-<name>/"
    More than one constructor may be specified. The syntax for defining a constructor is
    either: (:CONSTRUCTOR <name>) or (:CONSTRUCTOR <name> <arglist>)
    If no arglist is supplied, a constructor is defined which takes alternating slotnames
     and values as arguments and initializes those slots to those values.
    If an arglist is supplied, then the constructor defined will have this as its arglist.
     Meaningful lambda-list-keywords are &OPTIONAL &REST and &AUX.
     Use &AUX to initialize a slot to a value other then the usual default value.
    The first type of constructor behaves differently if :CALLABLE-CONSTRUCTORS is non-NIL.
    See below.
  :DEFAULT-POINTER defaults to empty (if no value given defaults to /"<name>/")
  :CONC-NAME defaults to empty (if no value given defaults to /"<name>-/")
    This what to prepend to the names of slots to obtain the names of the slot-accessor
    functions.
  :SIZE-SYMBOL defaults to empty (if no value given defaults to /"<name>-SIZE/")
  :SIZE-MACRO defaults to empty (if no value given defaults to /"<name>-SIZE/")
  :ALTERANT defaults to /"ALTER-<name>/"
  :BUT-FIRST see the manual
  :INCLUDE specifies a structure to include as a part of this structure.
  :PROPERTY (:property foo bar) gives the structure a foo property of bar.
            (:property foo) gives a foo property of T.
  :INITIAL-OFFSET can cause defstruct to skip over that many slots.
  :NAMED takes no value.  Tries to make the structure a named type.
  :CALLABLE-ACCESSORS defaults to T, which means that the slot-accessors may be called (they
   are defsubsts in this case) If NIL, the accessors are defined as macros.
  :CALLABLE-CONSTRUCTORS defaults to NIL -- the constructors are defined as macros.
    If non-NIL, they are functions, which is what Common Lisp wants.
    This will affect /"SETQ-style/" constructors, ie those which take alternating slotnames
     and values.
     If :CALLABLE-CONSTRUCTORS is T, those slotnames must be keywords, as the constructor
     is implemented as a function with an &KEY arglist. Keyword slotnames will ALSO work
     even if this is NIL, though non-keyword slotnames WILL NOT work if T.
     This also affects the quoting of arguments to the additional cons-keywords for these
     constructors. See SYS:DOC;SYS98 DEFSTRUCT for details.
  :PREDICATE defaults to empty (if no value given defaults to /"<name>-P/").
    Generates a predicate if possible.
  :COPIER defaults to empty (if no value given defaults to /"COPY-<name>/").
    Generates a function to copy this structure.
  :PRINT (:print /"#<spaceship at ~S by ~S>/" (x-pos spaceship) (y-pos spaceship))
    <name> is bound to the instance of the structure being printed. (/"spaceship/" in this
    example)
  :PRINT-FUNCTION The value should be a function of three arguments, the structure to be
    printed, the stream to print it on, and the current print-depth.
  <type> any type name can be used without a <val> instead of
    saying (:TYPE <type>) 
  <other> any symbol which is specified as a :DEFSTRUCT-KEYWORD for the type of structure
    which we are creating."
  (DEFSTRUCT-1 OPTIONS ITEMS NIL))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFMACRO CLI:DEFSTRUCT (OPTIONS &BODY ITEMS)
  "(DEFSTRUCT (<name> . <options>) . <slots>) or (DEFSTRUCT <name> . <slots>)
Options:
  :TYPE defaults to VECTOR
    Possible values are VECTOR, (VECTOR <vector-type>) and LIST, as well as the types
    available for non-common-lisp defstruct (GLOBAL:DEFSTRUCT)
  :CONSTRUCTOR defaults to /"MAKE-<name>/"
    More than one constructor may be specified. The syntax for defining a constructor
    is either: (:CONSTRUCTOR <name>) or (:CONSTRUCTOR <name> <arglist>)
    If no arglist is supplied, a constructor is defined which takes alternating
     slotnames and values as arguments and initializes those slots to those values.
    If an arglist is supplied, then the constructor defined will have this as its
     arglist. Meaningful lambda-list-keywords are &OPTIONAL &REST and &AUX.
     Use &AUX to initialize a slot to a value other then the usual default value.
  :CONC-NAME defaults to <name>. This what to prepend to the names of slots to obtain
    the names of the slot-accessor functions
  :INCLUDE specifies a structure to include as a part of this structure.
  :INITIAL-OFFSET can cause defstruct to skip over that many slots.
  :UNNAMED takes no value.  Tries to make the structure an unnamed type.
  :PREDICATE defaults to <name>-p.  Generates a predicate if possible. Give this option
    a value of NIL if you don't want a predicate.
  :PRINT-FUNCTION The value should be a function of three arguments, the structure to be
    printed, the stream to print it on, and the current print-depth.
  Many other (non-common-lisp, perhaps non-portable) options are available.
    See the documentation for GLOBAL:DEFSTRUCT."
  (DEFSTRUCT-1 OPTIONS ITEMS T))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "


(DEFUN DEFSTRUCT-1 (OPTIONS ITEMS CLIP)         ;CLIP means "common lisp, incompatible"-p
  (let* ((description (defstruct-parse-options options CLIP))
         (type-description (get (defstruct-description-type)
                                'defstruct-type-description))
         (name (defstruct-description-name))
         (DOC (AND (STRINGP (CAR ITEMS)) (POP ITEMS)))
         (new-slots (defstruct-parse-items items description))	;now all slots -- mly
         (returns nil))
    ;;Keep the returns from this as close to last as possible
    ;;Evaluate this before everything else
    (AND (defstruct-type-description-defstruct-expander)
         (setq returns (funcall (defstruct-type-description-defstruct-expander) description)))
    (SETQ RETURNS
          (APPEND RETURNS
                  ;;This must be the last returned form, since to compile it
                  ;;might require that the structure already be operable:
                  (IF (DEFSTRUCT-DESCRIPTION-PRINT)
                      (LIST (DEFSTRUCT-DEFINE-PRINTER NAME (DEFSTRUCT-DESCRIPTION-PRINT))))
                  ;;Return the name symbol as our value
                  `(',NAME)))
 #+(and LispM (NOT MIT))
    (push `(record-source-file-name ',name 'defstruct) returns)
 #+(and LispM MIT)
    (push `(eval-when (load eval) (record-source-file-name ',name 'defstruct)) returns)
 #+(AND LISPM MIT)				;not really just mit lispm, but any clisp
    (AND DOC (PUSH `(SETF (DOCUMENTATION ',NAME 'STRUCTURE) ,DOC) RETURNS))
    (let ((alterant (defstruct-description-alterant))
          (size-macro (defstruct-description-size-macro))
          (size-symbol (defstruct-description-size-symbol))
          (predicate (defstruct-description-predicate))
          (copier (defstruct-description-copier)))
      (cond (predicate
             (push (funcall (or (defstruct-type-description-predicate)
                                (defstruct-error
                                  "This DEFSTRUCT type cannot produce a predicate"
                                  (defstruct-description-type) 'in name))
                            description
                            predicate)
                   returns)))
      (cond (copier
             (push
               (let ((copy-fun (defstruct-type-description-copier)))
                 (cond (copy-fun
                        (funcall copy-fun description copier))
                       ((not (= 1 (defstruct-type-description-ref-no-args)))
                        (defstruct-error
                          "This defstruct type cannot produce a copying function"
                          (defstruct-description-type) 'in name))
                       (t (do ((i (1- (defstruct-description-size)) (1- i))
                               (l nil (cons (cons i
                                                  (funcall
                                                    (defstruct-type-description-ref-expander)
                                                    i description 'x))
                                            l)))
                              ((< i 0)
                               `(defun ,copier (x)
                                  ,(invoke-defstruct-constructor-expander
                                     description type-description l nil)))))))
               returns)))
      (cond (alterant
             (defstruct-put-macro alterant 'defstruct-expand-alter-macro)
             (defstruct-putprop alterant name 'defstruct-name)))
      (cond (size-macro
             (defstruct-put-macro size-macro 'defstruct-expand-size-macro)
             (defstruct-putprop size-macro name 'defstruct-name)))
      (cond (size-symbol
             (push `(defconst ,size-symbol
                      ,(+ (defstruct-description-size)
                          (defstruct-type-description-overhead)))
                   returns))))
    (defstruct-putprop name description 'defstruct-description)
    ;;what defstruct returns
    #-(OR LISPM NIL)			;retain eval-when so as not to cause hidden screws
    `(eval-when ,(defstruct-description-eval-when)
       ,.(defstruct-define-ref-macros new-slots description)
       ,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,.returns)
    #+(OR LISPM NIL)			;losing eval-when flushed!! 
    `(PROGN
       ,.(defstruct-define-ref-macros new-slots description)
       ,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,.returns)))
))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-DEFINE-PRINTER (NAME REST &AUX (CLIP (POP REST)))
  (LET ((OP (GENSYM))
        (ARGS (GENSYM)))
    (IF CLIP
        `(DEFUN (,NAME NAMED-STRUCTURE-INVOKE) (,OP ,NAME &REST ,ARGS)
           (SELECTQ ,OP
             (:PRINT-SELF
              (IF PRINT-READABLY (PRINT-NOT-READABLE ,NAME))
              (FUNCALL ,(CAR REST) ,NAME (CAR ,ARGS) (CADR ,ARGS)))
             (:WHICH-OPERATIONS '(:PRINT-SELF :WHICH-OPERATIONS))
             (T NIL)))                          ;don't barf on weird operations
       `(DEFUN (,NAME NAMED-STRUCTURE-INVOKE) (,OP ,NAME &REST ,ARGS)
          (SELECTQ ,OP
            (:PRINT-SELF
             (IF PRINT-READABLY (PRINT-NOT-READABLE ,NAME))
             (FORMAT (CAR ,ARGS) ,@REST))
            (:WHICH-OPERATIONS '(:PRINT-SELF :WHICH-OPERATIONS))
            (T NIL))))))
  

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "


(defun defstruct-make-init-dsc (kludge name code slots others x)
  (let ((p (OR (assq name slots)
	       ;;clisp uses keywords -- this is if (:callable-constructors nil) specified
	       (ASS #'STRING= (GET-PNAME NAME) SLOTS))))
    (if (null p)
	(if (memq name others)
	    (push (cons name code) (cdr kludge))
	  (let ((new (defstruct-retry-keyword name)))
	    (if (memq new others)
		(push (cons new code) (cdr kludge))
	      (defstruct-error
		"Unknown slot to constructor or alterant macro"
		name 'in x))))
      (let* ((slot-description (cdr p))
	     (number (defstruct-slot-description-number))
	     (ppss (defstruct-slot-description-ppss))
	     (dsc (assoc number (car kludge))))
	(cond ((null dsc)
	       (setq dsc (list* number nil (make-empty) 0 0 nil))
	       (push dsc (car kludge))))
	(cond ((null ppss)
	       (setf (car (cddr dsc)) code)
	       (setf (cadr dsc) t))
	      (t (cond ((and (numberp ppss) (numberp code))
			(setf (ldb ppss (cadr (cddr dsc))) -1)
			(setf (ldb ppss (caddr (cddr dsc))) code))
		       (t
			(push (cons ppss code) (cdddr (cddr dsc)))))
		 (or (eq t (cadr dsc))
		     (push name (cadr dsc)))))))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defstruct-define-type :array
  (:named :named-array)
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
			       description etc nil nil nil 1 NIL))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,n)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defstruct-define-type :named-array
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :named (:overhead 1)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,(1+ i)))
			       description etc nil t nil 1 NIL))
  (:ref (n description arg)
    description		;ignored
    `(aref ,arg ,(1+ n)))
  (:predicate (description name)
    `(defsubst ,name (x)
       (typep x ',(defstruct-description-name)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFSTRUCT-DEFINE-TYPE :TYPED-ARRAY		;an array with the named-structure-symbol
  (:NAMED :NAMED-TYPED-ARRAY)			;(if any) in the leader
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
			       DESCRIPTION ETC NIL NIL NIL 1 NIL))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION		;ignored
    `(AREF ,ARG ,N)))


))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFSTRUCT-DEFINE-TYPE :NAMED-TYPED-ARRAY	;type in leader -- data in array
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :NAMED
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
			       DESCRIPTION ETC NIL T NIL 1 T))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION		;ignored
    `(AREF ,ARG ,N))
  (:PREDICATE (DESCRIPTION NAME)
    `(DEFSUBST ,NAME (X)
       (TYPEP X ',(DEFSTRUCT-DESCRIPTION-NAME)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFSTRUCT-DEFINE-TYPE :VECTOR			;same as :TYPED-ARRAY
  (:NAMED :NAMED-VECTOR)
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
			       DESCRIPTION ETC NIL NIL NIL 1 NIL))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION		;ignored
    `(AREF ,ARG ,N)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFSTRUCT-DEFINE-TYPE :NAMED-VECTOR		;same as :NAMED-TYPED-ARRAY
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :NAMED
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
			       DESCRIPTION ETC NIL T NIL 1 T))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION		;ignored
    `(AREF ,ARG ,N))
  (:PREDICATE (DESCRIPTION NAME)
    `(DEFSUBST ,NAME (X)
       (TYPEP X ',(DEFSTRUCT-DESCRIPTION-NAME)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defstruct-define-type :array-leader
  (:named :named-array-leader)
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i)
				       `(store-array-leader ,v ,a ,i))
			       description etc nil nil t 1 NIL))
  (:ref (n description arg)
    description		;ignored
    `(array-leader ,arg ,n)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defstruct-define-type :named-array-leader
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :named (:overhead 1)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct
      arg
      #'(lambda (v a i)
	  `(store-array-leader ,v ,a ,(if (zerop i)
					  0
					  (1+ i))))
      description etc nil t t 1 T))
  (:ref (n description arg)
    description		;ignored
    (if (zerop n)
	`(array-leader ,arg 0)
	`(array-leader ,arg ,(1+ n))))
  (:predicate (description name)
    `(defsubst ,name (x)
       (typep x ',(defstruct-description-name)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defstruct-define-type :grouped-array
  (:CONS-KEYWORDS :make-array :times :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :TIMES :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct
      arg
      #'(lambda (v a i) `(aset ,v ,a ,i))
      description etc nil nil nil
      (or (cdr (or (assq ':times etc)
		   (assq ':times (defstruct-description-property-alist))))
	  1)
      NIL))
  (:ref (n description index arg)
    description		;ignored
    (cond ((numberp index)
	   `(aref ,arg ,(+ n index)))
	  ((zerop n)
	   `(aref ,arg ,index))
	  (t `(aref ,arg (+ ,n ,index))))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun lispm-array-for-defstruct (arg
				  cons-init
				  description
				  etc
				  type
				  &OPTIONAL (NAMED-P NIL)
					    (LEADER-P NIL)
					    (TIMES 1)
					    (TYPE-IN-LEADER NIL)
				  &AUX (P (CONS NIL NIL))
				       NO-OP
				       ARRAY-TYPE)
;arg is slot arg
;cons-init is code to initialize the structure per-slot
;description is a structure description
;etc is cons-keyword args/values
;type is the array-type to make
;named-p is t if to make a named structure
;leader-p is t if the data is to be stored in the leader (as in :{named-}array-leader)
;times if the #times for :grouped-array
;type-in-leader is t if the structure-type is to be put in array-leader 1 rather than
; in aref 0
  (defstruct-grok-make-array-args
    (cdr (assq ':make-array (defstruct-description-property-alist)))
    p)
  (defstruct-grok-make-array-args
    (cdr (assq ':make-array etc))
    p)
  (COND (TYPE
	 (PUTPROP P TYPE ':TYPE))
	((SETQ TYPE (CDR (ASSQ ':SUBTYPE ETC)))
	 (PUTPROP P `',(SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE TYPE T)) ':TYPE))
	((SETQ TYPE (DEFSTRUCT-DESCRIPTION-SUBTYPE))
	 (PUTPROP P `',(SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE TYPE T)) ':TYPE)))
  (and named-p (putprop p `',(defstruct-description-name) ':named-structure-symbol))
  (LET* ((S (OR (GET P (IF LEADER-P ':LEADER-LENGTH ':DIMENSIONS)) 0))
	 (SIZE (let ((size (if named-p
			       (1+ (defstruct-description-size))
			     (defstruct-description-size))))
		 (if (numberp times)
		     (MAX S (* size times))
		   `(MAX ,S (* ,size ,times))))))
    (putprop p SIZE (if leader-p ':leader-length ':dimensions)))
  (AND TYPE-IN-LEADER (OR (NOT (GET P ':LEADER-LENGTH))
			  (< (GET P ':LEADER-LENGTH) 2))
       (PUTPROP P 2 ':LEADER-LENGTH))
  (SETQ ARRAY-TYPE (OR (LET ((TYPE (GET P ':TYPE)))
			 (OR (ATOM TYPE)
			     (NEQ (CAR TYPE) 'QUOTE)
			     (SETQ TYPE (CADR TYPE)))
			 (ARRAY-CANONICALIZE-TYPE TYPE))
		       'ART-Q))
  (OR LEADER-P
      (IF (OR (GET P ':INITIAL-ELEMENT)
	      (GET P ':INITIAL-VALUE))
	  (SETQ NO-OP (MAKE-EMPTY))
	(SETQ NO-OP (SELECTQ ARRAY-TYPE
		      ((NIL ART-Q ART-Q-LIST) NIL)
		      ((ART-32B ART-16B ART-8B ART-4B ART-2B ART-1B ART-HALF-FIX
				ART-STRING ART-FAT-STRING)
		       0)
		      ((ART-FLOAT ART-FPS-FLOAT)
		       0.0)
		      (ART-COMPLEX
		       (COMPLEX 0 0))
		      ((ART-COMPLEX-FLOAT ART-COMPLEX-FPS-FLOAT)
		       (COMPLEX 0.0 0.0))
		      (T (MAKE-EMPTY))))))
  ;;make sure that we can store tha named-structure-symbol safely
  (OR (NOT NAMED-P)
      (MEMQ ARRAY-TYPE '(ART-Q ART-Q-LIST ART-SPECIAL-PDL ART-REG-PDL ART-STACK-GROUP-HEAD))
      (GET P ':LEADER-LENGTH)
      (SETQ ARRAY-TYPE 'ART-Q)
      (PUTPROP P 'ART-Q ':TYPE))
  (do ((creator
	 (let ((dims (remprop p ':dimensions)))
	   (do ((l (cdr p) (cddr l)))
	       ((null l))
	     (rplaca l `',(car l)))
	   `(make-array ,(if (null dims) 0 (car dims)) ,@(cdr p))))
       (var (gensym))
       (set-ups nil (if (equal (cdar l) no-op)
			set-ups
		      (PUSH (funcall cons-init (cdar l) var (caar l)) SET-UPS)))
       (l arg (cdr l)))
      ((null l)
       (if set-ups
	   `((lambda (,var)
	       ,@(nreverse set-ups)
	       ,var)
	     ,creator)
	 creator))))
))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-GROK-MAKE-ARRAY-ARGS (ARGS P)
  (DO ((L ARGS (CDDR L)))
      ((NULL L) P)
    (UNLESS (AND (CDR L)
		 (MEMQ (CAR L) '(:AREA :TYPE :DISPLACED-TO :LEADER-LIST
				 :LEADER-LENGTH :DISPLACED-INDEX-OFFSET
				 :NAMED-STRUCTURE-SYMBOL :DIMENSIONS
				 :LENGTH :INITIAL-VALUE :INITIAL-ELEMENT :FILL-POINTER
				 :ELEMENT-TYPE)))
      (DEFSTRUCT-ERROR
	"DEFSTRUCT can't grok these MAKE-ARRAY arguments"
	ARGS))
    (PUTPROP P
	     (CADR L)
	     (IF (EQ (CAR L) ':LENGTH)
		 ':DIMENSIONS
		 (CAR L)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-HACK-ARRAY-SUPERTYPE (DESCRIPTION)
  (OR (DEFSTRUCT-DESCRIPTION-SUBTYPE)
      (DO* ((SL (DEFSTRUCT-DESCRIPTION-SLOT-ALIST) (CDR SL))
	    (SLOT-TYPE)
	    (TY ART-ERROR))
	   ((OR (NULL SL) (MEMQ TY '(ART-Q ART-ERROR)))
	    (IF (EQ TY 'ART-ERROR) (SETQ TY 'ART-Q))
	    (SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) TY))
	(SETQ SLOT-TYPE (DEFSTRUCT-SLOT-DESCRIPTION-TYPE (CDAR SL)))
	(IF (MEMQ SLOT-TYPE '(NIL NOTYPE)) (SETQ SLOT-TYPE T))
	(SETQ TY (ARRAY-SUPERTYPE T TY (ARRAY-TYPE-FROM-ELEMENT-TYPE SLOT-TYPE T)))))
  NIL)

))

; From file METH.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (ZMACS-WINDOW :EXIT-SPECIAL-BUFFER) (&OPTIONAL MARK-CLEAN BUFFER-BEING-EXITED)
  (LET ((SPECIAL-BUFFER (OR BUFFER-BEING-EXITED *INTERVAL*)))
    (AND MARK-CLEAN (NOT-MODIFIED SPECIAL-BUFFER))
    (IF (EQ SPECIAL-BUFFER *INTERVAL*)
	(MAKE-BUFFER-CURRENT (OR (CAR (MEM 'NEQ SPECIAL-BUFFER (HISTORY-LIST BUFFER-HISTORY)))
				 *INTERVAL*)))
    (WITHOUT-INTERRUPTS
      (DELETE-FROM-HISTORY SPECIAL-BUFFER BUFFER-HISTORY)
      (SETQ *ZMACS-BUFFER-LIST* (APPEND (REMQ SPECIAL-BUFFER *ZMACS-BUFFER-LIST*)
					(LIST SPECIAL-BUFFER))))
    (POINT-PDL-PURGE SPECIAL-BUFFER))
  DIS-TEXT)

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

;MAIN ROUTINE, TO PRINT ANY LISP OBJECT.
;THE WHICH-OPERATIONS ARGUMENT IS PROVIDED AS AN EFFICIENCY HACK.  IT ALSO USED
;BY STREAMS THAT HAVE A :PRINT HANDLER, AND RECURSIVELY CALL PRINT-OBJECT, TO
;PREVENT THEMSELVES FROM BEING CALLED AGAIN (THEY PASS NIL OR (:STRING-OUT)).
(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM
		     &OPTIONAL
		     (WHICH-OPERATIONS
		       ;; This arg is only used to MEMQ for :PRINT or :STRING-OUT,
		       ;; so eliminate all other elements to make that faster.
		       (LET ((TEM (FUNCALL STREAM ':WHICH-OPERATIONS)))
			 (IF (MEMQ ':PRINT TEM)
			     (IF (MEMQ ':STRING-OUT TEM)
				 '(:PRINT :STRING-OUT) '(:PRINT))
			   (IF (MEMQ ':STRING-OUT TEM)
			       '(:STRING-OUT) '()))))
		     &AUX NSS (FASTP (MEMQ ':STRING-OUT WHICH-OPERATIONS)))
  (CATCH-CONTINUATION-IF T 'PRINT-OBJECT
      #'(LAMBDA () (FORMAT STREAM "...error printing ")
		(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP :FASTP FASTP))
		(FORMAT STREAM "..."))
      NIL
    (CONDITION-RESUME '((ERROR) :ABORT-PRINTING T ("Give up trying to print this object.")
			CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
      (OR (AND (MEMQ ':PRINT WHICH-OPERATIONS)	;Allow stream to intercept print operation
	       (FUNCALL STREAM ':PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*))
	  (AND *PRINT-CIRCLE*
	       (%POINTERP EXP)
	       (OR (NOT (SYMBOLP EXP))
		   (NOT (SYMBOL-PACKAGE EXP)))
	       ;; This is a candidate for circular or shared structure printing.
	       ;; See what the hash table says about the object:
	       ;; NIL - occurs only once.
	       ;; T - occurs more than once, but no occurrences printed yet.
	       ;;  Allocate a label this time and print #label= as prefix.
	       ;; A number - that is the label.  Print only #label#.
	       (*CATCH 'LABEL-PRINTED
		 (SEND PRINT-HASH-TABLE ':MODIFY-HASH EXP
		       #'(LAMBDA (KEY VALUE KEY-FOUND-P STREAM)
			   KEY KEY-FOUND-P
			   (COND ((NULL VALUE) NIL)
				 ((EQ VALUE T)
				  (LET ((LABEL (INCF PRINT-LABEL-NUMBER))
					(BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM ':TYO #/=)
				    LABEL))
				 (T
				  (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM ':TYO #/#)
				    (*THROW 'LABEL-PRINTED T)))))
		       STREAM)
		 NIL))
	  (TYPECASE EXP
	    (:FIXNUM (PRINT-FIXNUM EXP STREAM))
	    (:SYMBOL
	     (PRINT-PNAME-STRING EXP STREAM FASTP))
	    (:LIST
	     (IF (AND PRINLEVEL (>= I-PRINDEPTH PRINLEVEL))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL READTABLE) STREAM FASTP)
	       (IF *PRINT-PRETTY*
		   (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		 (PRINT-LIST EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (:STRING
	     (IF ( (ARRAY-ACTIVE-LENGTH EXP) (ARRAY-LENGTH EXP))
		 (PRINT-QUOTED-STRING EXP STREAM FASTP)
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:INSTANCE
	      (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
	    (:ENTITY
	     (IF (MEMQ ':PRINT-SELF (FUNCALL EXP ':WHICH-OPERATIONS))
		 (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*) 
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:NAMED-STRUCTURE
	     (IGNORE-ERRORS
	       (SETQ NSS (NAMED-STRUCTURE-P EXP)))
	     (COND ((AND (SYMBOLP NSS)
			 (GET NSS 'NAMED-STRUCTURE-INVOKE)
			 (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;NAMED STRUCTURE THAT DOESN'T PRINT ITSELF
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
;		    (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
;		      (PRINC NSS STREAM))
	    (:ARRAY
	     (IF *PRINT-ARRAY*
		 (IF (AND (= (ARRAY-RANK EXP) 1)
			  (EQ (ARRAY-TYPE EXP) 'ART-1B))
		     (PRINT-BIT-VECTOR EXP STREAM)
		   (IF *PRINT-PRETTY*
		       (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		     (IF (= (ARRAY-RANK EXP) 1)
			 (PRINT-VECTOR EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
		       (PRINT-ARRAY EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	       (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
		 (PRINT-RAW-STRING (GET-PNAME (ARRAY-TYPE EXP)) STREAM FASTP)
		 (DO ((I 0 (1+ I))
		      (RANK (ARRAY-RANK EXP))
		      (DIM))
		     ((= I RANK))
		   (SETQ DIM (ARRAY-DIMENSION EXP I))
		   (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
		   (PRINT-FIXNUM DIM STREAM)))))
	    (:SMALL-FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP T))
	    (:FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP NIL))
	    (:BIGNUM
	     (PRINT-BIGNUM EXP STREAM FASTP))
	    (:RATIONAL
	     (PRINT-RATIONAL EXP STREAM FASTP))
	    (:COMPLEX (PRINT-COMPLEX EXP STREAM FASTP))
	    (:CHARACTER
	      (IF (NOT *PRINT-ESCAPE*)
		  (SEND STREAM ':TYO (LDB %%CH-CHAR EXP))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-BEFORE-FONT READTABLE))
		(IF (LDB-TEST %%CH-FONT EXP)
		    (LET ((BASE 10.) (*NOPOINT T))
		      (PRIN1 (LDB %%CH-FONT EXP) STREAM)))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-PREFIX READTABLE))
		(SEND STREAM ':TYO (LDB %%CH-CHAR EXP))))
	    (:NUMBER
	     (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
	     (PRINT-RAW-STRING (GET-PNAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
	     (LET ((BASE 8))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-NAMED-STRUCTURE (NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
  (FUNCALL STREAM ':STRING-OUT "#S")		;should use printtable
  (LET* ((DESCRIPTION (GET-DEFSTRUCT-DESCRIPTION NSS))
	 (SLOT-ALIST (DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
	 (L (LIST NSS)))
    (DOLIST (S SLOT-ALIST)
      (LET* ((KWD (INTERN (GET-PNAME (CAR S)) PKG-KEYWORD-PACKAGE))
	     (FUN (DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR S)))
	     (INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE (CDR S)))
	     (VAL (EVAL `(,FUN ,EXP))))		;watch out for macros!
	(UNLESS (EQUAL VAL INIT)
	  (PUSH KWD L) (PUSH VAL L))))
    (PRINT-LIST (NREVERSE L) I-PRINDEPTH STREAM WHICH-OPERATIONS)))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (BASIC-INSPECT :OBJECT-LIST) (LIST)
  (MULTIPLE-VALUE-BIND (STRING-LIST ATOMIC-ITEMS LIST-ITEMS)
      (LET ((PRINLENGTH INSPECT-PRINLENGTH)
	    (PRINLEVEL INSPECT-PRINLEVEL)
	    (*PRINT-ARRAY* NIL))
	(GRIND-INTO-LIST LIST (TRUNCATE (SHEET-INSIDE-WIDTH) CHAR-WIDTH) T))
    ;; Turn STRING-LIST into a list of elements, one for each line, of the form
    ;; (NIL contents-string atom-item-list line-contains-lozenged-characters-p).
    (DO ((L STRING-LIST (CDR L))
	 (AIS ATOMIC-ITEMS (CDR AIS)))
	((NULL L))
      (LET ((LOZENGED-CHARACTERS
	      (DOTIMES (I (STRING-LENGTH (CAR L)))
		(IF ( (AREF (CAR L) I) 200)
		    (RETURN T)))))
	;; Convert the start and end indices for each atom-item from characters to pixels.
	;; If this line contains no lozenged characters,
	;; this can be done by multiplying.  Otherwise, SHEET-STRING-LENGTH must be used.
	(DOLIST (I (CAR AIS))
	  (SETF (THIRD I) (+ (SHEET-INSIDE-LEFT)
			     (IF LOZENGED-CHARACTERS
				 (SHEET-STRING-LENGTH SELF (CAR L) 0 (THIRD I))
			       (* (THIRD I) CHAR-WIDTH))))
	  (SETF (FOURTH I) (+ (SHEET-INSIDE-LEFT)
			      (IF LOZENGED-CHARACTERS
				  (SHEET-STRING-LENGTH SELF (CAR L) 0 (FOURTH I))
				(* (FOURTH I) CHAR-WIDTH)))))
	(RPLACA L (LIST NIL (CAR L) (CAR AIS) LOZENGED-CHARACTERS))))
    ;; Convert the starting and ending hpos of each list-item from characters to pixels
    ;; Must find the line which the start or end appears on
    ;; and see whether that line had any lozenged characters
    ;; to decide whether a multiplication is sufficient.
    (DOLIST (I LIST-ITEMS)
      (SETF (SECOND I)
	    (+ (SHEET-INSIDE-LEFT)
	       (LET ((LINE-DESC (NTH (THIRD I) STRING-LIST)))
		 (IF (FOURTH LINE-DESC)
		     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (SECOND I))
		   (* (SECOND I) CHAR-WIDTH)))))
      (SETF (FOURTH I)
	    (+ (SHEET-INSIDE-LEFT)
	       (LET ((LINE-DESC (NTH (FIFTH I) STRING-LIST)))
		 (IF (FOURTH LINE-DESC)
		     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (FOURTH I))
		   (* (FOURTH I) CHAR-WIDTH))))))
    (SETQ LIST-ITEMS (SORT LIST-ITEMS
			   #'(LAMBDA (X Y)
			       (COND ((< (THIRD Y) (THIRD X)) T)
				     ((> (THIRD Y) (THIRD X)) NIL)
				     (T (> (SECOND X) (SECOND Y)))))))
    (DO ((LINE (1- (LENGTH STRING-LIST)) (1- LINE))
	 (CURRENT LIST-ITEMS))
	((< LINE 0))
      (DO ()
	  ((OR (NULL CURRENT)
	       ( (THIRD (CAR CURRENT)) LINE)))
	(SETQ CURRENT (CDR CURRENT)))
      (RPLACA (CAR (NTHCDR LINE STRING-LIST)) CURRENT))
    (PROG () (RETURN STRING-LIST ':LIST-STRUCTURE 'INSPECT-LIST-PRINTER))))

))

; From file MOUSE.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFUN MOUSE-SPEED-HACK (&REST SPECS)
"Specs are SCALE-FACTOR SPEED-BREAK SCALE-FACTOR SPEED-BREAK ... SCALE-FACTOR
Each SCALE-FACTOR applies to speeds up to the following SPEED-BREAK.
The last SCALE-FACTOR applies to all higher speeds.
Args of (.6 120 1 200 1.5 400 2.2 700 3.3) are standardly used.
These apply to both X and Y."
  (LOOP FOR (SCALE SPEED) ON SPECS BY 'CDDR
	FOR I FROM 0 BY 2
	DO (ASET (OR SPEED 37777777) TV:MOUSE-X-SCALE-ARRAY I)
	   (ASET (OR SPEED 37777777) TV:MOUSE-Y-SCALE-ARRAY I)
	   (ASET (// (FIX (* 2 SCALE 1024.)) 3) TV:MOUSE-X-SCALE-ARRAY (1+ I))
	   (ASET (// (FIX (* 3 SCALE 1024.)) 5) TV:MOUSE-Y-SCALE-ARRAY (1+ I))))

))


; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

;; The standard externally called forms of EVAL are here.
;; They handle all kinds of atoms themselves,
;; to save the extra function call.
;; They use EVAL1 to handle combinations.

(defun cli:eval (form)
  "Evaluate FORM as Common Lisp in the global lexical environment, returning its value(s).
Free variables in FORM must be special."
  (cond (*evalhook*
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form nil nil)))
	((symbolp form)
	 (symeval form))
	((atom form) form)
	(t
	 (let (interpreter-environment interpreter-function-environment)
	   (eval1 form)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun eval (form &optional nohook)
  "Evaluate FORM as Zetalisp in the global lexical environment, returning its value(s).
Free variables in FORM must be special."
  (cond ((and *evalhook* (not nohook))
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form nil t)))
	((symbolp form)
	 (symeval form))
	((atom form) form)
	(t
	 (let ((interpreter-function-environment t)
	       interpreter-environment)
	   (eval1 form)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun *eval (form &optional interpreter-environment interpreter-function-environment)
  "Evaluate FORM as Common Lisp in the specified lexical environment, returning its value(s).
If the second arg is T, it means to use the Zetalisp evaluator."
  (cond (*evalhook*
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form
		    interpreter-environment interpreter-function-environment)))
	((symbolp form)
	 (if (eq interpreter-function-environment t)
	     (symeval form)
	   (if (keywordp form)
	       form
	     (interpreter-symeval form))))
	((atom form) form)
	(t (eval1 form))))

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFCONST *SPLIT-SCREEN-AMONG-BUFFERS-FIXED-ITEMS*
     '(("New buffer" :VALUE "New buffer"
	:DOCUMENTATION "Create a new, empty buffer.  Prompt for its name.")
       ("Find file" :VALUE "Find file" 
	:DOCUMENTATION "Do a Find File command and put the resulting buffer in a window.")
       ("Undo" :VALUE "Undo"
	:FONT :MENU-STANDOUT
	:DOCUMENTATION "Undo last selection.")
       ("Do It" :VALUE "Do It"
	:FONT :MENU-STANDOUT
	:DOCUMENTATION "Complete the selection and set up the windows as specified.")
       ("Abort" :VALUE "Abort"
	:FONT :MENU-STANDOUT
	:DOCUMENTATION "Abort the Split Screen command.")))

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-COUNT-WORDS "Counts the number of words in the region or buffer." (KM)
  (WITH-REGION-OR-WHOLE-INTERVAL (TEM)
    (FORMAT QUERY-IO "~&There are ~D. words in the ~A.~%"
		 (COUNT-OBJECTS 'FORWARD-WORD *INTERVAL*)
		 (IF TEM "region" "buffer")))
  DIS-NONE)

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-COUNT-LINES "Counts the number of lines in the region or buffer." (KM)
  (WITH-REGION-OR-WHOLE-INTERVAL (TEM)
    (FORMAT QUERY-IO "~&There are ~D. lines in the ~A.~%"
		 (1- (COUNT-LINES *INTERVAL*))
		 (IF TEM "region" "buffer")))
  DIS-NONE)

))

; From file COMS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

;;; General query replace.  Note: BP itself is moved around.  It is usually POINT.
;;; The caller must bind *QUERY-FROM* and *QUERY-TO* to strings for the mode line to use.
;;; BREAKS means only consider things surrounded by delimiters.
;;; FUNCTION is called on with BP and QUERY-FROM and QUERY-to, it should return two bps to
;;; the area of the thing found or NIL.
;;; FLAG-1 and FLAG-2 implement the hairy COMMA command.
(DEFUN QUERY-REPLACE-INTERNAL (BP QUERY-FROM QUERY-TO FUNCTION BREAKS
			       &AUX BP1 BP2 DO-THE-REST CHAR UCHAR FLAG-1 FLAG-2)
  (BIND-MODE-LINE '("Query Replacing " *QUERY-FROM* " => " *QUERY-TO*)
    (SETQ BP1 (COPY-BP BP)
	  BP2 (COPY-BP BP))
    (DO () (NIL)
      (SETQ FLAG-2 FLAG-1 FLAG-1 NIL)
      (COND ((NOT FLAG-2)
	     (MULTIPLE-VALUE (BP2 BP1)
	       (FUNCALL FUNCTION BP2 QUERY-FROM QUERY-TO))
	     (UNLESS BP2
	       (FORMAT QUERY-IO "~&No more occurences.")
	       (RETURN NIL))))
      (COND ((OR FLAG-2
		 (NOT BREAKS)			; If we don't care about breaks, go ahead.
		 (AND				; Both beginning and end must be breaks.
		   (OR (BP-= BP2 (INTERVAL-LAST-BP *INTERVAL*))	; EOB counts as a break.
		       (= (WORD-SYNTAX (BP-CHAR BP2)) WORD-DELIMITER))
		   (OR (BP-= BP1 (INTERVAL-FIRST-BP *INTERVAL*))
		       (= (WORD-SYNTAX (BP-CHAR-BEFORE BP1)) WORD-DELIMITER))))
	     ;; Move point after checking delimiters
	     (COND ((NOT FLAG-2)
		    (MOVE-BP BP BP2)
		    (MUST-REDISPLAY *WINDOW* DIS-BPS)))
	     ;; We want to offer this string for replacement.
	     (COND (DO-THE-REST (QREP))
		   (T
		    (REDISPLAY *WINDOW* ':POINT)
		    (REDISPLAY-MODE-LINE)
		    (POINT-PDL-PUSH BP *WINDOW*)
		    (PROG ()
		       GETCHAR
			  (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI))
			  (OR (NUMBERP CHAR) (GO GETCHAR))	;Ignore special request
			  (SETQ UCHAR (CHAR-UPCASE CHAR))
			  (COND ((= UCHAR #/^)
				 (POINT-PDL-POP *WINDOW*)	;Already done once
				 (MULTIPLE-VALUE-BIND (BP1 PLINE)
				     (POINT-PDL-POP *WINDOW*)
				   (MOVE-BP BP BP1)
				   (REDISPLAY-POINT-ON-PLINE BP *WINDOW* PLINE))
				 (MUST-REDISPLAY *WINDOW* DIS-BPS)
				 (REDISPLAY *WINDOW* ':POINT)
				 (GO GETCHAR))
				((= CHAR #\C-R)	;C-R: Recurse.
				 (WITH-BP (BP1-COPY BP1 ':MOVES)
				   (WITH-BP (BP2-COPY BP2 ':NORMAL)
				     (CONTROL-R)
				     (MOVE-BP (POINT) BP2-COPY)
				     (MOVE-BP BP1 BP1-COPY)
				     (IF (BP-< BP2-COPY BP1-COPY)
					 (MOVE-BP BP2 BP1-COPY)
				       (MOVE-BP BP2 BP2-COPY))))
				 (MUST-REDISPLAY *WINDOW* DIS-BPS)
				 (REDISPLAY *WINDOW* ':POINT)
				 (REDISPLAY-MODE-LINE)
				 (GO GETCHAR))
				((MEMQ UCHAR '(#\FF #\C-L))
				 (MUST-REDISPLAY *WINDOW*
						 (IF (= UCHAR #\FF) DIS-ALL
						     (COM-RECENTER-WINDOW)))
				 (REDISPLAY *WINDOW* ':POINT)
				 (GO GETCHAR))
				((MEMQ UCHAR '(#/? #\HELP))
				 (PRINT-DOC ':FULL *CURRENT-COMMAND*)
				 (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT)
				 (REDISPLAY-ALL-WINDOWS)
				 (GO GETCHAR))))
		    (SELECTQ UCHAR
		      (#\SP (QREP))		;Space: Replace and continue.
		      (#\RUBOUT NIL)		;Rubout: Continue.
		      (#/,			;Comma:
		       (QREP)
		       (SETQ FLAG-1 T))
		      ((#/ #\END) (RETURN NIL))	;Altmode: Quit.
		      (#/. (QREP)		;Point: Replace and quit.
		       (RETURN NIL))
		      (#\C-W			;C-W: Delete, and recurse.
		       (DELETE-INTERVAL BP BP1)
		       (MUST-REDISPLAY *WINDOW* DIS-TEXT)
		       (CONTROL-R))
		      (#/! (QREP)		;!: Do this and rest.
		       (SETQ DO-THE-REST T))
		      (OTHERWISE
		       (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
		       (RETURN 'ABORTED))))))))))

))

; From file WINDOW.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(DEFMETHOD (ZMAIL-SUMMARY-SCROLL-WINDOW :WHO-LINE-DOCUMENTATION-STRING) (&AUX X Y)
  (MULTIPLE-VALUE (X Y)
    (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET))
  (SETQ X (- TV:MOUSE-X X)
	Y (- TV:MOUSE-Y Y))
  (COND ((< Y (TV:SHEET-INSIDE-TOP))
	 (IF ( (TRUNCATE TV:WIDTH 4) X (TRUNCATE (* 3 TV:WIDTH) 4))
	     (IF (EQ (SYMEVAL-IN-CLOSURE (SEND TV:SUPERIOR ':EDITOR-CLOSURE)
					 '*WINDOW-CONFIGURATION*)
		     ':BOTH)
		 "Change layout to display just summary window."
		 "Change layout to display both summary and message.")
	     "Change layout to display just message."))
	(TV:CURRENT-ITEM
	 (GET 'ZMAIL-SUMMARY-MOUSE ':WHO-LINE-DOCUMENTATION))))

))

; From file DOCMIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'EQUAL 'FUNCTION)
  "T if X and Y are EQ, or if they are lists whose elements are EQUAL.
Numbers are compared with EQL, so the answer is T if they have the same type and value.
Strings are compared by their contents, using STRING=.
Other kinds of arrays, however, are compared with EQ.")

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFMETHOD (SHEET :CHARACTER-WIDTH) (CHAR &OPTIONAL (FONT CURRENT-FONT))
  (SHEET-CHARACTER-WIDTH SELF CHAR
			 (IF (TYPEP FONT 'FONT)
			     FONT
			   (FUNCALL (SHEET-GET-SCREEN SELF)
				    ':PARSE-FONT-DESCRIPTOR FONT))))

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

;;; -*- Mode: LISP; Package: TV; Base: 8 -*-
;;;	** (c) Copyright 1980, 1981 Massachusetts Institute of Technology **

(DEFMACRO COERCE-FONT (FONT-VARIABLE SHEET)
  `(UNLESS (TYPEP ,FONT-VARIABLE 'FONT)
     (SETQ ,FONT-VARIABLE (SEND (SHEET-GET-SCREEN ,SHEET) ':PARSE-FONT-SPECIFIER
				,FONT-VARIABLE))))

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

;;;kludgiferous stuff for dealing with the kludges involved in the font-map system.
;;;these functions will make a change in a font (eg by loading or fed) propagate to all the
;;;places that the font is in use.
;;;the reason why the changes don't propagate normally is that the font maps are consist of
;;;the actual font structures rather than symbols like 'fonts:cptfont.
(DEFUN UPDATE-FONT (FONT)
  (IF (TYPEP FONT 'FONT)
      (FONT-EVALUATE (FONT-NAME FONT))
    FONT))

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN UPDATE-FONT-MAPS (&OPTIONAL FONTS)
  "Update all the font maps in the world to reflect changes made in FONTS.
FONTS should either be a list of fonts to check or NIL, meaning to check all fonts."
  (SETQ FONTS (MAPCAR #'FONT-EVALUATE FONTS))
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (LET ((INFERIORS (SEND SCREEN ':INFERIORS))
	  (FONT-ALIST (SEND SCREEN ':FONT-ALIST))
	  )
      (DOLIST (TEM FONT-ALIST)
	(SET (CDR TEM) (UPDATE-FONT (SYMEVAL (CDR TEM)))))
      (UPDATE-FONT-MAP SCREEN FONTS)
      (DOLIST (WINDOW INFERIORS)
	(UPDATE-FONT-MAP WINDOW FONTS)))))

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN UPDATE-FONT-MAP (WINDOW &OPTIONAL FONTS)
  (LET* ((OLD-FONT-MAP (SEND WINDOW ':FONT-MAP))
	 (FONT-MAP (FONT-MAP-FONT-LIST OLD-FONT-MAP))
	 (CURRENT-FONT (FONT-MAP-CURRENT-FONT-NAME OLD-FONT-MAP))
	 MOD)
    (SETQ FONT-MAP
	  (MAPCAR #'(LAMBDA (F)
		      (IF (AND (TYPEP F 'FONT)
			       (OR (NULL FONTS) (MEMQ F FONTS))
			       (SETQ MOD T))
			  (UPDATE-FONT F)
			F))
		  FONT-MAP))
    (OR CURRENT-FONT (SETQ CURRENT-FONT (SEND WINDOW :CURRENT-FONT)))
    (SETQ MOD (OR (NEQ CURRENT-FONT (SETQ CURRENT-FONT (UPDATE-FONT CURRENT-FONT))) MOD))
    (WHEN MOD
      (SEND WINDOW ':SET-FONT-MAP FONT-MAP)
      (SEND WINDOW ':SET-CURRENT-FONT CURRENT-FONT)))
  (DOLIST (I (SEND WINDOW ':INFERIORS))
    (UPDATE-FONT-MAP I FONTS)))

))

(DOLIST (F 'FONTS:(CPTFONT TR12BI
;;MEDFNT MEDFNB TR10 TR10B TR10BI TR8 TR8B HL6 HL7 TR12 TR12B TR12B TR12B1 HL12 HL12B HL12I HL12BI HL10 HL10B METS METSI NARROW  TVFONT  43VXMS  BIGFNT
			   ))
  (LOAD (SEND '#FS#:LOGICAL-PATHNAME "SYS: FONTS; CPTFONT QFASL >"
	      :NEW-NAME (GET-PNAME F))
	"FONTS" NIL))

(DOLIST (F FONTS:(LIST MOUSE CPTFONT MEDFNT 5X5))
  (COLOR:MAKE-COLOR-FONT F))

(TV:UPDATE-FONT-MAPS)
; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#A-MACRO (STREAM IGNORE &OPTIONAL (RANK XR-SHARP-ARGUMENT))
  (IF (AND (FIXNUMP RANK) (PLUSP RANK))
      (LET (DIMENSIONS (SEQUENCES (INTERNAL-READ STREAM T NIL T)))
	(DO ((DIM 0 (1+ DIM))
	     (STUFF SEQUENCES (ELT STUFF 0)))
	    ((= DIM RANK))
	  (PUSH (LENGTH STUFF) DIMENSIONS))
	(VALUES (MAKE-ARRAY (NREVERSE DIMENSIONS) ':INITIAL-CONTENTS SEQUENCES)))
    (CERROR ':NO-ACTION NIL 'READ-ERROR-1
	    "~S is not a valid array rank.")
    (INTERNAL-READ STREAM T NIL T)
    NIL))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#S-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (LET* ((ARGS (INTERNAL-READ STREAM T NIL T))
	 (CONSTRUCTOR
	   (DOLIST (C (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS
			(GET (CAR ARGS) 'DEFSTRUCT-DESCRIPTION)))
	     (IF (NULL (CDR C)) (RETURN (CAR C))))))
    (IF CONSTRUCTOR
	(EVAL (CONS CONSTRUCTOR
		    (LOOP FOR (SLOT VALUE) ON (CDR ARGS) BY 'CDDR
			  APPEND `(,SLOT ',VALUE))))
      (CERROR ':NO-ACTION NIL 'READ-ERROR-1
	      "~S is not a structure type with a standard keyword constructor." (CAR ARGS))
      NIL)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

;;; callable, clisp-style constructors
(DEFUN DEFSTRUCT-DEFINE-CONSTRUCTORS (DESCRIPTION)
  (LET ((NAME (DEFSTRUCT-DESCRIPTION-NAME))
	RETURNS)
    (IF (NOT (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS))
	(DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
	  (DEFSTRUCT-PUT-MACRO (CAR CS) 'DEFSTRUCT-EXPAND-CONS-MACRO)
	  (DEFSTRUCT-PUTPROP (CAR CS) NAME 'DEFSTRUCT-NAME))
      (LET* ((SLOT-ALIST (DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
	     (SIZE (DEFSTRUCT-DESCRIPTION-SIZE))
	     (TYPE-DESCRIPTION (GET (DEFSTRUCT-DESCRIPTION-TYPE) 'DEFSTRUCT-TYPE-DESCRIPTION))
	     (CONS-KEYWORDS (DEFSTRUCT-TYPE-DESCRIPTION-CONS-KEYWORDS))
	     (FL (DEFSTRUCT-TYPE-DESCRIPTION-CONS-FLAVOR))
	     MNAME BODY SYM ARGLIST ARGS REST FROB FROBPPSS X S INIT INIT-LIST BOAP
	     OPT OPTPPSS OPT-SLOT OPTPPSS-SLOT FLAGS PPSS-FLAGS
	     NOPT NOPTPPSS NOPT-SLOT NOPTPPSS-SLOT
	     (F (GENSYM)) (L (GENSYM)) (R (GENSYM)) (D (GENSYM)) (Y (GENSYM))
	     (SL (GENSYM)) (TEM (GENSYM))
	     CW CWN
	     (CONS-WORDS (DO ((V CONS-KEYWORDS (CDR V))
			      R W)
			     ((NULL V) R)
			   ;;will this win everywhere?
			   (SETQ W (INTERN (GET-PNAME (CAR V))))	;in *package*
			   (PUSH W CW)
			   (PUSH (LIST 'QUOTE (CAR V)) CW)
			   (PUSH (GENSYM) CW)
			   (PUSH (LIST W NIL (CAR CW)) R)
			   (PUSH W CWN)))
	     )
	(DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
	  (SETQ MNAME (CAR CS) BOAP T
		ARGLIST () ARGS () REST NIL
		OPT () OPT-SLOT () FLAGS () OPTPPSS () OPTPPSS-SLOT () PPSS-FLAGS ()
		NOPT () NOPT-SLOT () NOPTPPSS () NOPTPPSS-SLOT ())
	  (IF (CDR CS)
	      ;;it's a boa-constructor!
	      (IF (CDDR CS) (DEFSTRUCT-ERROR
			      "DEFSTRUCT constructors can only be specified by arglist"
			      CS)
		(DO ((AL (CADR CS)))
		    (NIL)
		 REQUIRED
		  (SELECTQ (SETQ X (POP AL))
		    (&OPTIONAL (GO OPTIONAL))
		    (&REST (GO REST))
		    (&AUX (GO AUX))
		    (NIL (RETURN))
		    (T (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (PUSH X ARGS)
		       (PUSH X ARGLIST)
		       (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			   (PROGN (PUSH X NOPTPPSS)
				  (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					      (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					NOPTPPSS-SLOT))
			 (PUSH X NOPT)
			 (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			       NOPT-SLOT))
		       (GO REQUIRED)))
		 OPTIONAL
		  (PUSH '&OPTIONAL ARGS)
		  (PUSH '&OPTIONAL ARGLIST)
		 OPT
		  (SELECTQ (SETQ X (POP AL))
		    (&OPTIONAL (GO OPT))
		    (&REST (GO REST))
		    (&AUX (GO AUX))
		    (NIL (RETURN))
		    (T (PUSH X ARGLIST)		     
		       (IF (CONSP X)
			   (IF (CDDR X) (GO ARG-ERROR)
			     (PSETQ X (CAR X) INIT (CADR X)))
			 (SETQ INIT (MAKE-EMPTY)))
		       (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (IF (EMPTYP INIT) (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
		       (IF (EMPTYP INIT)
			   (PROGN
			     (SETQ SYM (GENSYM))
			     (PUSH (LIST X NIL SYM) ARGS)
			     (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				 (PROGN (PUSH SYM PPSS-FLAGS)
					(PUSH X OPTPPSS)
					(PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						    (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					      OPTPPSS-SLOT))
			       (PUSH SYM FLAGS)
			       (PUSH X OPT)
			       (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				     OPT-SLOT)))
			 (PUSH (LIST X INIT) ARGS)
			 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			     (PROGN (PUSH X NOPTPPSS)
				    (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						(DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					  NOPTPPSS-SLOT))
			   (PUSH X NOPT)
			   (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				 NOPT-SLOT)))
		       (GO OPT)))
		 REST
		  (PUSH '&REST ARGS)
		  (PUSH '&REST ARGLIST)
		  (SELECTQ (SETQ X (POP AL))
		    ((&OPTIONAL &REST &AUX NIL) (GO ARG-ERROR))
		    (T (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (SETQ REST X)
		       (PUSH X ARGS)
		       (PUSH X ARGLIST)
		       (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			   (PROGN (PUSH X NOPTPPSS)
				  (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					      (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					NOPTPPSS-SLOT))
			 (PUSH X NOPT)
			 (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			       NOPT-SLOT))
		       (SELECTQ (SETQ X (POP AL))
			 (&AUX (GO AUX))
			 (NIL (RETURN))
			 (T (GO ARG-ERROR)))))
		 AUX
		  (PUSH '&AUX ARGLIST)
		  (PUSH '&AUX ARGS)
		 OX
		  (SELECTQ (SETQ X (POP AL))
		    ((&OPTIONAL &REST &AUX) (GO ARG-ERROR))
		    (NIL (RETURN))
		    (T (PUSH X ARGLIST)
		       (IF (CONSP X)
			   (IF (CDDR X) (GO ARG-ERROR)
			     (PSETQ X (CAR X) INIT (CADR X)))
			 (SETQ INIT (MAKE-EMPTY)))
		       (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (IF (EMPTYP INIT) (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
		       (IF (EMPTYP INIT) NIL
			 (PUSH (LIST X INIT) ARGS)
			 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			     (PROGN (PUSH X OPTPPSS)
				    (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						(DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					  NOPTPPSS-SLOT))
			   (PUSH X NOPT)
			   (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				 NOPT-SLOT)))
		       (GO OX)))
		 ARG-ERROR
		  (DEFSTRUCT-ERROR "Bad defstruct :CONSTRUCTOR argument list" AL 'FOR MNAME)
		 SLOT-ERROR
		  (DEFSTRUCT-ERROR "Invalid defstruct slot-name" X 'WHILE 'DEFINING MNAME)))
	    ;;do this for non-boa-constructors
	    (SETQ BOAP NIL)
	    (PUSH '&KEY ARGLIST)
	    (PUSH '&KEY ARGS)
	    (DOLIST (S SLOT-ALIST)
	      (SETQ X (CAR S) S (CDR S)		;standardize our nomenclature
		    INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S))
	      (IF (EMPTYP INIT)
		  (PROGN
		    (PUSH X ARGLIST)
		    (SETQ SYM (GENSYM))
		    (PUSH (LIST X NIL SYM) ARGS)
		    (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			(PROGN (PUSH SYM PPSS-FLAGS)
			       (PUSH X OPTPPSS)
			       (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					   (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				     OPTPPSS-SLOT))
		      (PUSH SYM FLAGS)
		      (PUSH X OPT)
		      (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			    OPT-SLOT)))
		(PUSH (LIST X INIT) ARGS)
		(PUSH (CAR ARGS) ARGLIST)
		(IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
		    (PROGN (PUSH X NOPTPPSS)
			   (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				       (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				 NOPTPPSS-SLOT))
		  (PUSH X NOPT)
		  (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			NOPT-SLOT))))
	    (WHEN CONS-KEYWORDS
	      (SETQ ARGLIST (NCONC (REVERSE CONS-KEYWORDS) (LIST* '&OPTIONAL NIL) ARGLIST))
	      (SETQ ARGS (NCONC (COPYLIST* CONS-WORDS) ARGS))))
	  ;;crunch the args now that we've snarfed them
	  (SETQ ARGLIST (NREVERSE ARGLIST) ARGS (NREVERSE ARGS))
          (SELECTQ FL
	      (:LIST
	       (SETQ INIT-LIST (MAKE-LIST SIZE))
	       (DO ((X INIT-LIST (CDR X))) ((NULL X))
		 (SETF (CAR X) (LIST 'QUOTE NIL)))
	       (DOLIST (X SLOT-ALIST)		;put zero inits where appropriate
		 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS (CDR X)) (SETF (CADR (NTH (CAR X) INIT-LIST)) 0)))
	       (SETQ FROB `((SETF (CADR (NTH (CAR ,SL) ,L))) (CAR ,Y))))
	       (SETQ FROBPPSS `((SETF (LDB (CDAR ,SL) (CADR (NTH (CAAR ,SL) ,L))) (CAR ,Y))))
	      (:ALIST
	       (SETQ INIT-LIST ())
	       (SETQ FROB `((IF (SETQ ,TEM (ASSOC (CAR ,SL) ,L))
				(SETF (CADDR ,TEM) (CAR ,Y))
			      (PUSH (CONS (CAR ,SL) (LIST 'QUOTE (CAR ,Y))) ,L))))
	       (SETQ FROBPPSS `((IF (SETQ ,TEM (ASSOC (CAAR ,SL) ,L))
				    (SETF (LDB (CDAR ,SL) (CADR ,TEM)) (CAR ,Y))
				  (PUSH (CONS (CAAR ,SL) (DPB (CAR ,Y) (CDAR ,SL) 0)) ,L)))))
	      (T (DEFSTRUCT-ERROR
		   "Unknown constructor kind"
		   FL 'IN TYPE-DESCRIPTION)))
	    (SETQ BODY
		  (NCONC (IF INIT-LIST
			     `((SETQ ,L ,INIT-LIST)))
			 #+LISPM				;needed elsewhere??
			 (IF REST
			     `((SETQ ,REST (COPYLIST ,REST))))	;can't trust stack lists
			 (IF (AND (NOT BOAP) CONS-KEYWORDS)
			     `((DO ((,F (LIST ,@CW) (CDDDR ,F)))
				   ((NULL ,F))
				 (WHEN (CAR ,F)
				   (PUSH (CONS (CADR ,F) (CADDR ,F)) ,R)))
			       ,@CWN))				;prevent compiler barfage
			 (IF OPT
			     `((DO ((,F (LIST ,@FLAGS) (CDR ,F))
				    (,SL ',OPT-SLOT (CDR ,SL))
				    (,Y (LIST ,@OPT) (CDR ,Y)))
				   ((NULL ,Y))
				 (WHEN (CAR ,F) ,@FROB))))
			 (IF OPTPPSS
			     `((DO ((,F (LIST ,@PPSS-FLAGS) (CDR ,F))
				    (,SL ',OPTPPSS-SLOT (CDR ,SL))
				    (,Y (LIST ,@OPTPPSS) (CDR ,Y)))
				   ((NULL ,Y))
				 (WHEN (CAR ,F) ,@FROBPPSS))))
			 (IF NOPT
			     `((DO ((,SL ',NOPT-SLOT (CDR ,SL))
				    (,Y (LIST ,@NOPT) (CDR ,Y)))
				   ((NULL ,Y))
				 ,@FROB)))
			 (IF NOPTPPSS
			     `((DO ((,SL ',NOPTPPSS-SLOT (CDR ,SL))
				    (,Y (LIST ,@NOPTPPSS) (CDR ,Y)))
				   ((NULL ,Y))
				 ,@FROBPPSS)))))
	    (DEFSTRUCT-PUTPROP MNAME NAME 'DEFSTRUCT-NAME)
	    (PUSH 
	      `(DEFUN ,MNAME ,ARGS
		 (DECLARE (ARGLIST ,ARGLIST))
		 (LET ((,D (GET-DEFSTRUCT-DESCRIPTION ',NAME))
		       ,L ,TEM ,R)
		   ,@BODY
		   (EVAL 
		     (FUNCALL (DEFSTRUCT-TYPE-DESCRIPTION-CONS-EXPANDER
				(GET (DEFSTRUCT-DESCRIPTION-TYPE ,D) 'DEFSTRUCT-TYPE-DESCRIPTION))
			      ,L
			      ,D
			      ,R))))
	      RETURNS))))
    RETURNS))

))
