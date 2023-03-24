;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 3/15/84 01:14:05 by kab,
;;; Reason: DEFRESOURCE interprets a string as first option to instead be documentation
;;; MATH:MULTIPLY-MATRICES fixed improper handling of vector x matrix case
;;; while running on Lisp Machine Eighteen from band 6
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.39, CADR 3.6, ZMail 53.13, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file RESOUR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFMACRO DEFRESOURCE (NAME PARAMETERS &REST OPTIONS)
  "Define a resource named NAME, with parameters PARAMETERS for constructing objects.
OPTIONS can specify how to create objects and how to tell when old objects
can be reused."
  ;; Old format?
  (IF (OR (CONSP NAME) (NULL OPTIONS) (LISTP (CAR OPTIONS)))
      ;; In system 88, crack down on these; make sure they finally get fixed.
      (FERROR NIL "Obsolete form of DEFRESOURCE for ~S~%" NAME)
    (LET ((CONSTRUCTOR-FORM NIL) (FINDER-FORM NIL) (MATCHER-FORM NIL) (CHECKER-FORM NIL)
	  (CONSTRUCTOR-FUNCTION NIL) (FINDER-FUNCTION NIL) (MATCHER-FUNCTION NIL)
	  (PARAMETIZER-FUNCTION NIL) (CHECKER-FUNCTION NIL) (INITIAL-COPIES 0)
	  (INITIALIZER-FORM NIL) (INITIALIZER-FUNCTION NIL) (FREE-LIST-SIZE 20.) (PARAMS NIL)
	  (DOCUMENTATION NIL))
      (OR (LISTP PARAMETERS) (NULL PARAMETERS)
	  (FERROR NIL "~S invalid parameter list" PARAMETERS))
      (SETQ PARAMS (LOOP FOR P IN PARAMETERS
			 UNLESS (MEMQ P LAMBDA-LIST-KEYWORDS)
			 COLLECT (IF (SYMBOLP P) P (CAR P))))
      ;; if first option is a string, use it as documentation instead
      (WHEN (STRINGP (CAR OPTIONS))
	(SETQ DOCUMENTATION (POP OPTIONS)))
      (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY #'CDDR
	    DO (SELECTQ KEYWORD
		 (:CONSTRUCTOR (SETQ CONSTRUCTOR-FORM VALUE))
		 (:FINDER (SETQ FINDER-FORM VALUE))
		 (:MATCHER (SETQ MATCHER-FORM VALUE))
		 (:CHECKER (SETQ CHECKER-FORM VALUE))
		 (:INITIALIZER (SETQ INITIALIZER-FORM VALUE))
		 (:INITIAL-COPIES
		  (SETQ INITIAL-COPIES
			(COND ((NULL VALUE) 0)
			      ((NUMBERP VALUE) VALUE)
			      (T (FERROR NIL ":INITIAL-COPIES ~S - number required"
					 VALUE)))))
		 (:FREE-LIST-SIZE
		  (SETQ FREE-LIST-SIZE
			(COND ((NULL VALUE) 20.)
			      ((NUMBERP VALUE) VALUE)
			      (T (FERROR NIL ":FREE-LIST-SIZE ~S - number required")))))
		 (OTHERWISE (FERROR NIL "~S illegal option in DEFRESOURCE" KEYWORD))))
      (OR CONSTRUCTOR-FORM (FERROR NIL "DEFRESOURCE requires the :CONSTRUCTOR option"))
      ;;Pick function names.  Note that NIL is SYMBOLP.
      (SETQ CONSTRUCTOR-FUNCTION (IF (SYMBOLP CONSTRUCTOR-FORM) CONSTRUCTOR-FORM
				   `(:PROPERTY ,NAME RESOURCE-CONSTRUCTOR)))
      (SETQ FINDER-FUNCTION (IF (SYMBOLP FINDER-FORM) FINDER-FORM
			      `(:PROPERTY ,NAME RESOURCE-FINDER)))
      (SETQ MATCHER-FUNCTION (IF (SYMBOLP MATCHER-FORM) MATCHER-FORM
			       `(:PROPERTY ,NAME RESOURCE-MATCHER)))
      (SETQ CHECKER-FUNCTION (IF (SYMBOLP CHECKER-FORM) CHECKER-FORM
			       `(:PROPERTY ,NAME RESOURCE-CHECKER)))
      (SETQ INITIALIZER-FUNCTION (IF (SYMBOLP INITIALIZER-FORM) INITIALIZER-FORM
				   `(:PROPERTY ,NAME RESOURCE-INITIALIZER)))
      (SETQ PARAMETIZER-FUNCTION (IF (AND PARAMETERS (NOT MATCHER-FORM) (NOT FINDER-FORM))
				     `(:PROPERTY ,NAME RESOURCE-PARAMETIZER)))
      `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,NAME DEFRESOURCE))
	 ,(IF (NOT (SYMBOLP CONSTRUCTOR-FORM))
	      `(DEFUN ,CONSTRUCTOR-FUNCTION (IGNORE ,@PARAMETERS)
		 ,@PARAMS
		 ,CONSTRUCTOR-FORM))
	 ,(IF (NOT (SYMBOLP FINDER-FORM))
	      `(DEFUN ,FINDER-FUNCTION (IGNORE ,@PARAMETERS)
		 ,@PARAMS
		 ,FINDER-FORM))
	 ,(IF (NOT (SYMBOLP MATCHER-FORM))
	      `(DEFUN ,MATCHER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
		 ,@PARAMS
		 ,MATCHER-FORM))
	 ,(IF (NOT (SYMBOLP CHECKER-FORM))
	      `(DEFUN ,CHECKER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
					 ,@PARAMETERS)
		 ,@PARAMS ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
		 ,CHECKER-FORM))
	 ,(IF (NOT (SYMBOLP INITIALIZER-FORM))
	      `(DEFUN ,INITIALIZER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
		 ,@PARAMS ,(INTERN "OBJECT")
		 ,INITIALIZER-FORM))
	 ,(IF PARAMETIZER-FUNCTION
	      `(DEFUN ,PARAMETIZER-FUNCTION ,PARAMETERS
		 (LIST ,@PARAMS)))
	 (INITIALIZE-RESOURCE ',NAME ',CONSTRUCTOR-FUNCTION ',FINDER-FUNCTION
			      ',MATCHER-FUNCTION ',CHECKER-FUNCTION
			      ',PARAMETIZER-FUNCTION ',INITIAL-COPIES ',FREE-LIST-SIZE
			      ',INITIALIZER-FUNCTION)
	 ;; update documentation
	 (WHEN ,DOCUMENTATION
	   (SETF (DOCUMENTATION ',NAME 'DEFRESOURCE) ,DOCUMENTATION))))))

))

; From file MATRIX.LISP PS:<L.SYS2> OZ:
#8R MATH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "MATH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MATRIX  "

(DEFUN MULTIPLY-MATRICES (MATRIX-1 MATRIX-2 &OPTIONAL MATRIX-3 &AUX SAVED-MATRIX-3)
  "Multiply matrices MATRIX-1 and MATRIX-2, storing into MATRIX-3 if supplied.
If MATRIX-3 is not supplied, then a new (ART-Q type) array is returned, else
MATRIX-3 must have exactly the right dimensions for holding the result of the multiplication.
Both MATRIX-1 and MATRIX-2 must be either one- or two-diimensional.
The first dimension of MATRIX-2 must equal the second dimension of MATRIX-1, unless MATRIX-1
is one-dimensional, when the first dimensions must match (thus allowing multiplications of the
form VECTOR x MATRIX)"
  (CHECK-ARG MATRIX-1 1-OR-2D-ARRAYP "A one- or two-dimensional array")
  (CHECK-ARG MATRIX-2 1-OR-2D-ARRAYP "A one- or two-dimensional array")
  (CHECK-ARG MATRIX-3 (OR (NULL MATRIX-3) (1-OR-2D-ARRAYP MATRIX-3))
	     "A one- or two-dimensional array or NIL")
  (LET ((DIM-1-1 (IF (= (ARRAY-RANK MATRIX-1) 1) 1 (ARRAY-DIMENSION MATRIX-1 0)))
	(DIM-1-2 (IF (= (ARRAY-RANK MATRIX-1) 1) (ARRAY-DIMENSION MATRIX-1 0)
		   (ARRAY-DIMENSION MATRIX-1 1)))
	(DIM-2-1 (ARRAY-DIMENSION MATRIX-2 0))
	(DIM-2-2 (IF (= (ARRAY-RANK MATRIX-2) 1) 1 (ARRAY-DIMENSION MATRIX-2 1)))
	(DIM-3-1 (WHEN MATRIX-3
		   (IF (= (ARRAY-RANK MATRIX-3) 1) 1 (ARRAY-DIMENSION MATRIX-3 0))))
	(DIM-3-2 (WHEN MATRIX-3
		   (IF (= (ARRAY-RANK MATRIX-3) 1) (ARRAY-DIMENSION MATRIX-3 0)
		     (ARRAY-DIMENSION MATRIX-3 1)))))
    (UNLESS (= DIM-2-1 DIM-1-2)
      (FERROR NIL "The ~~Dx~D matrix ~S and the
~Dx~D matrix ~S cannot be multiplied~"
	      DIM-1-1 DIM-1-2 MATRIX-1 DIM-2-1 DIM-2-2 MATRIX-2))
    (IF MATRIX-3
	(IF (AND (= DIM-1-1 DIM-3-1)
		 (= DIM-2-2 DIM-3-2))
	    ;; We have a destination; see if it's the same as one of the sources,
	    ;; If it is, substitute a temporary for the destination.  We only check
	    ;; EQness, not displacements.
	    (WHEN (LET ((FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-3)))
		    (OR (EQ FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-1))
			(EQ FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-2))))
	      (SETQ SAVED-MATRIX-3 MATRIX-3
		    MATRIX-3 (MAKE-ARRAY (ARRAY-DIMENSIONS MATRIX-3)
					 ':TYPE (ARRAY-TYPE MATRIX-3))))
	  (FERROR NIL "The ~~Dx~D matrix ~S is not the right size for multiplying the
~Dx~D matrix ~S and the
~Dx~D matrix ~S.~"
		  DIM-3-1 DIM-3-2 MATRIX-3 DIM-1-1 DIM-1-2 MATRIX-1
		  DIM-2-1 DIM-2-2 MATRIX-2))
      ;; we don't make a 1xn matrix here since the user probably wants a vector result.
      (SETQ MATRIX-3 (MAKE-ARRAY (IF (= (ARRAY-RANK MATRIX-1) 1) DIM-1-2 (LIST DIM-1-1 DIM-2-2)))))
    ;; Make indirect arrays to any vectors, so can use ar-2 everywhere below
    (LET ((MAT-1 (IF (= 2 (ARRAY-RANK MATRIX-1)) MATRIX-1
		   (MAKE-ARRAY (LIST DIM-1-1 DIM-2-1) ':TYPE (ARRAY-TYPE MATRIX-1)
			       ':DISPLACED-TO MATRIX-1)))
	  (MAT-2 (IF (= 2 (ARRAY-RANK MATRIX-2)) MATRIX-2
		   (MAKE-ARRAY (LIST DIM-2-1 DIM-2-2) ':TYPE (ARRAY-TYPE MATRIX-2)
			       ':DISPLACED-TO MATRIX-2)))
	  (MAT-3 (IF (= 2 (ARRAY-RANK MATRIX-3)) MATRIX-3
		   (MAKE-ARRAY (LIST DIM-1-1 DIM-2-2) ':TYPE (ARRAY-TYPE MATRIX-3)
			       ':DISPLACED-TO MATRIX-3))))
      ;; Do the actual multiplication
      (DOTIMES (I DIM-1-1)
	(DOTIMES (J DIM-2-2)
	  (SETF (AREF MAT-3 I J)
		(DO ((K 0 (1+ K))
		     (SUM 0 (+ SUM (* (AREF MAT-1 I K) (AREF MAT-2 K J)))))
		    (( K DIM-2-1) SUM)))))
      ;; Try to get rid of any temporary arrays we made
      (WHEN (NEQ MATRIX-3 MAT-3) (RETURN-ARRAY MAT-3))
      (WHEN (NEQ MATRIX-2 MAT-2) (RETURN-ARRAY MAT-2))
      (WHEN (NEQ MATRIX-1 MAT-1) (RETURN-ARRAY MAT-1))
      ;; if we substituted a temporary above, copy the result into the saved
      ;; destination and return the temporary to free storage.
      (WHEN SAVED-MATRIX-3
	(COPY-ARRAY-CONTENTS MATRIX-3 SAVED-MATRIX-3)
	(RETURN-ARRAY MATRIX-3)
	(SETQ MATRIX-3 SAVED-MATRIX-3))))
  MATRIX-3)

))
