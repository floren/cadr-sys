;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by BV
;;; Reason:
;;;  Copy DG patch from LMI: 
;;;  allow :EVEN constraints with other constraints in TV:PARSE-CONSTRAINT.
;;; Written 13-Feb-24 16:05:57 by BV,
;;; while running on CDR (CADR) from band 3
;;; with Experimental System 100.22, Experimental Local-File 54.0, Experimental FILE-Server 11.0, Experimental Tape 27.0, microcode 323, 230816.



; From file OZ: /tree/window/frame.lisp at 13-Feb-24 16:06:06
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//window//frame"

(DEFUNP PARSE-CONSTRAINT (CONSTR PANES WINDOW LG-P EVEN-P &AUX (MIN -1) (MAX 1_15.))
  "Verify correctness of the specified constraint.  Returns the constraint part
of the constraint, as well as the limits if specified."
  (COND ((EQ (FIRST CONSTR) ':LIMIT)
	 (LET ((LIMITS (SECOND CONSTR))
	       (ROUND) (OFFSET))
	   (COND ((> (LENGTH LIMITS) 2)
		  (OR (SETQ WINDOW (OR (PARSE-CONSTRAINT-GET-PANE (FOURTH LIMITS) PANES)
				       WINDOW))
		      (FERROR NIL "Illegal format :LIMIT (no window specified)"))
		  (SELECTQ (THIRD LIMITS)
		    (:CHARACTERS (SETQ ROUND (SHEET-CHAR-WIDTH WINDOW)
				       OFFSET (+ (SHEET-LEFT-MARGIN-SIZE WINDOW)
						 (SHEET-RIGHT-MARGIN-SIZE WINDOW))))
		    (:LINES (SETQ ROUND (SHEET-LINE-HEIGHT WINDOW)
				  OFFSET (+ (SHEET-TOP-MARGIN-SIZE WINDOW)
					    (SHEET-BOTTOM-MARGIN-SIZE WINDOW))))
		    (OTHERWISE (FERROR NIL "~A is illegal rounding specification"
				       (THIRD LIMITS))))))
	   (SETQ MIN (OR (FIRST LIMITS) MIN)
		 MAX (OR (SECOND LIMITS) MAX))
	   (SETQ MIN (IF ROUND (+ (* MIN ROUND) OFFSET) MIN)
		 MAX (IF ROUND (+ (* MAX ROUND) OFFSET) MAX)))
	 (SETQ CONSTR (CDDR CONSTR))))
  (COND ((OR (IF (NUMBERP (FIRST CONSTR))
		 (OR (NULL EVEN-P) (EQ EVEN-P ':NO)
		     (FERROR NIL "Cannot mix :EVEN constraints and other constraints"))
		 NIL)
	     (COND ((EQ (FIRST CONSTR) ':EVEN)
		    (OR LG-P (FERROR NIL ":EVEN not in last descriptor group"))
;;; This was causing incredible problems... looks useless -dg
;		    (OR (NULL EVEN-P) (EQ EVEN-P ':YES)
;			(FERROR NIL "Cannot mix :EVEN constraints and other constraints"))
		    (SETQ EVEN-P ':YES)
		    T)))
	 (COND ((> (LENGTH CONSTR) 1)
		(LET ((W (PARSE-CONSTRAINT-GET-PANE (THIRD CONSTR) PANES)))
		  (IF W
		      (SETQ WINDOW W
			    CONSTR (LIST (FIRST CONSTR) (SECOND CONSTR) W))
		      (OR WINDOW
			  (FERROR NIL "Illegal format constraint -- no window specified"))))
		(OR (MEMQ (SECOND CONSTR) '(:LINES :CHARACTERS))
		    (FERROR NIL "Illegal rounding specifier ~A" (SECOND CONSTR)))
		(AND (FIXP (FIRST CONSTR))
		     (SETQ CONSTR (LIST (* (FIRST CONSTR)
					   (SELECTQ (SECOND CONSTR)
					     (:LINES (SHEET-LINE-HEIGHT WINDOW))
					     (:CHARACTERS (SHEET-CHAR-WIDTH WINDOW))))
					(SECOND CONSTR)
					WINDOW))))))
	((NOT (OR (NULL EVEN-P) (EQ EVEN-P ':NO)))
	 (FERROR NIL "Cannot mix :EVEN constraints and other constraints"))
	((MEMQ (FIRST CONSTR) '(:ASK :FUNCALL :EVAL :FIXED)))
	((EQ (FIRST CONSTR) ':ASK-WINDOW)
	 (LET ((W (IF (EQ (SECOND CONSTR) 'SELF)
		      SELF
		      (CDR (ASSQ (SECOND CONSTR) PANES)))))
	   (OR W (FERROR NIL "Unknown pane ~A is :ASK-WINDOW constraint"
			 (SECOND CONSTR)))
	   (SETF (SECOND (SETQ CONSTR (COPYLIST CONSTR))) W))))
  (RETURN CONSTR MIN MAX (OR EVEN-P ':NO)))

))
