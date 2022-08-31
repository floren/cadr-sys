;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patches for System 98.76 (from 99.9)
;;; Reason: (NEQ 'WHEN 'AND) idiot. (Really fix COLOR:COLOR-DRAW-LINE) (99.9)
;;; Written 14-Oct-84 15:56:53 by Mly,
;;; while running on Lisp Machine Sixteen from band 5
;;; with System 98.75, CADR 3.10, ZMail 53.18, MIT-Specific 22.5, microcode 309, gc@36.


; From file COLOR.LISP OZ:<L.WINDOW> OZ:
#8R COLOR#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COLOR")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLOR  "

(DEFUN COLOR-DRAW-LINE (X1 Y1 X2 Y2
			&OPTIONAL (COLOR 17) (ALU TV:ALU-SETA) (SCREEN COLOR-SCREEN))
  "Draw a line from X1, Y1 to X2, Y2 in color COLOR."
    (WHEN (> X1 X2) (SWAPF X1 X2) (SWAPF Y1 Y2))
    (TV:PREPARE-SHEET (SCREEN)
      (LET ((DX (- X2 X1))
	    (DY (- Y2 Y1))
	    (PIXEL-ARRAY (TV:SHEET-SCREEN-ARRAY COLOR-SCREEN)))
	(LET ((DIR-Y (IF (MINUSP DY) -1 1))
	      (DY (ABS DY)))
	  (COND ((ZEROP DY) (RECTANGLE X1 Y1 (- X2 X1) 1 COLOR ALU SCREEN))
		((ZEROP DX) (RECTANGLE X1 (MIN Y1 Y2) 1 (- (MAX Y1 Y2) (MIN Y1 Y2))
				       COLOR ALU SCREEN))
		((> DX DY)			;X IS LARGER STEP
		 (DO ((X1 X1 (1+ X1))
		      (REM (TRUNCATE DY 2) (+ REM DY)))
		     ((> X1 X2))
		   (IF ( REM DX) (SETQ Y1 (+ Y1 DIR-Y) REM (- REM DX)))
		   (AS-2-REVERSE (BOOLE ALU COLOR (AR-2-REVERSE PIXEL-ARRAY X1 Y1))
				 PIXEL-ARRAY X1 Y1)))
		(T				;Y IS LARGER STEP
		 (DO ((I 0 (1+ I))
		      (Y Y1 (+ Y DIR-Y))
		      (REM (TRUNCATE DX 2) (+ REM DX)))
		     ((> I DY))
		   (IF ( REM DY) (SETQ X1 (1+ X1) REM (- REM DY)))
		   (AS-2-REVERSE (BOOLE ALU COLOR (AR-2-REVERSE PIXEL-ARRAY X1 Y))
				 PIXEL-ARRAY X1 Y))))))))

))