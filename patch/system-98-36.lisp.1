;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 2/03/84 03:26:44 by RpK,
;;; Reason: MATH:MULTIPLY-MATRICES bugs (barfed on vectors, played fast and loose with returned
;;;    matrix array type)
;;; while running on Lisp Machine Eighteen from band 6
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.32, CADR 3.6, ZMail 53.10, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file MATRIX.LISP PS:<L.SYS2> OZ:
#8R MATH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "MATH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MATRIX  "

(DEFUN MULTIPLY-MATRICES (MATRIX-1 MATRIX-2 &OPTIONAL MATRIX-3
			  &AUX MAT-1 MAT-2 MAT-3 DIM-1 DIM-2 COM-DIM
			  SAVED-MATRIX-3)
  "Multiply matrices MATRIX-1 and MATRIX-2, storing into MATRIX-3 if supplied.
MATRIX-2 can be a vector; MATRIX-1 must be two dimensional.  If MATRIX-3 (which can 
also be a vector) is not supplied, then a new (ART-Q type) array is returned.
The first dimension of MATRIX-2 must equal the second dimension of MATRIX-1."
  (SETQ DIM-1 (ARRAY-DIMENSION MATRIX-1 0)
	DIM-2 (IF (= (ARRAY-RANK MATRIX-2) 1) 1 (ARRAY-DIMENSION MATRIX-2 1))
	COM-DIM (ARRAY-DIMENSION MATRIX-2 0))
  (OR (= COM-DIM (OR (ARRAY-DIMENSION MATRIX-1 1) 1))
      (FERROR NIL "The matrices ~S and ~S are not compatible for multiplication"
	      MATRIX-1 MATRIX-2))
  (IF MATRIX-3
      (IF (NOT (AND (= DIM-1 (ARRAY-DIMENSION MATRIX-3 0))
		    (= DIM-2 (IF (= (ARRAY-RANK MATRIX-3) 1)
				 1
			       (ARRAY-DIMENSION MATRIX-3 1)))))
	  (FERROR NIL "The matrix ~S is not the right size for multiplying ~S and ~S"
		  MATRIX-3 MATRIX-1 MATRIX-2)
	;; We have a destination; see if it's the same as one of the sources,
	;; If it is, substitute a temporary for the destination.  We only check
	;; eqness, not displacements.
	(IF (LET ((FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-3)))
	      (OR (EQ FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-1))
		  (EQ FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-2))))
	    (SETQ SAVED-MATRIX-3 MATRIX-3
		  MATRIX-3 (MAKE-ARRAY (ARRAY-DIMENSIONS MATRIX-3)
				       ':TYPE (ARRAY-TYPE MATRIX-3)))))
    (SETQ MATRIX-3 (MAKE-ARRAY (IF (= DIM-2 1) DIM-1 (LIST DIM-1 DIM-2)))))
  ;; Make indirect arrays to any vectors, so can use ar-2 everywhere below
  (SETQ MAT-1 (IF (= 2 (ARRAY-RANK MATRIX-1)) MATRIX-1
		(MAKE-ARRAY (LIST DIM-1 COM-DIM) ':TYPE (ARRAY-TYPE MATRIX-1)
			    ':DISPLACED-TO MATRIX-1)))
  (SETQ MAT-2 (IF (= 2 (ARRAY-RANK MATRIX-2)) MATRIX-2
		(MAKE-ARRAY (LIST COM-DIM DIM-2) ':TYPE (ARRAY-TYPE MATRIX-2)
			    ':DISPLACED-TO MATRIX-2)))
  (SETQ MAT-3 (IF (= 2 (ARRAY-RANK MATRIX-3)) MATRIX-3
		(MAKE-ARRAY (LIST DIM-1 DIM-2) ':TYPE (ARRAY-TYPE MATRIX-3)
			    ':DISPLACED-TO MATRIX-3)))
  ;; Do the actual multiplication
  (DOTIMES (I DIM-1)
    (DOTIMES (J DIM-2)
      (ASET (DO ((K 0 (1+ K))
		 (SUM 0 (+ SUM (* (AREF MAT-1 I K) (AREF MAT-2 K J)))))
		(( K COM-DIM) SUM))
	    MAT-3 I J)))
  ;; Try to get rid of any temporary arrays we made
  (AND (NEQ MATRIX-3 MAT-3) (RETURN-ARRAY (PROG1 MAT-3 (SETQ MAT-3 NIL))))
  (AND (NEQ MATRIX-2 MAT-2) (RETURN-ARRAY (PROG1 MAT-2 (SETQ MAT-2 NIL))))
  (AND (NEQ MATRIX-1 MAT-1) (RETURN-ARRAY (PROG1 MAT-1 (SETQ MAT-1 NIL))))
  ;; if we substituted a temporary above, copy the result into the saved
  ;; destination and return the temporary to free storage.
  (COND (SAVED-MATRIX-3 (COPY-ARRAY-CONTENTS MATRIX-3 SAVED-MATRIX-3)
			(RETURN-ARRAY (PROG1 MATRIX-3
					     (SETQ MATRIX-3 NIL)))
			SAVED-MATRIX-3)
	(T MATRIX-3)))

))
