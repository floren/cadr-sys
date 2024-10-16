;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 300.2
;;; Reason:
;;;  MAKE-ARRAY: Properly error out for array ranks strictly larger or equal than 8.
;;; Written 16-Oct-24 23:59:25 by ams,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 300.0, microcode 323.



; From file OZ: /sys/sys/qrand.lisp.414 at 16-Oct-24 12:17:08
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //sys//sys//qrand.lisp.414"

;;; This is the new version of MAKE-ARRAY.  It takes the old argument
;;; format as well as the new one, with heuristics to disambiguate whether
;;; a given call is old-style or new-style.
(DEFUN MAKE-ARRAY (DIMENSIONS &REST OPTIONS)
  "Create an array of size DIMENSIONS (a number or list of numbers).
The keywords are as follows:
:TYPE - specify array type, controlling type of elements allowed.  Default is ART-Q.
 ART-Q (any elements), ART-Q-LIST (any elements, and the contents looks like a list),
 ART-STRING (elements 0 through 255, printed with quotes),
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
  (DECLARE (ARGLIST DIMENSIONS &KEY ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
		    		    FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET
				    TYPE AREA LEADER-LENGTH LEADER-LIST NAMED-STRUCTURE-SYMBOL
				    ADJUSTABLE))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
	ENTRIES-PER-Q
	LEADER-LIST FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET NAMED-STRUCTURE-SYMBOL
	(AREA NIL) (TYPE 'ART-Q) TYPE-P ELEMENT-TYPE-P
	INITIAL-ELEMENT INITIAL-ELEMENT-P INITIAL-CONTENTS INITIAL-CONTENTS-P
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
	   (UNLESS (EVENP LENGTH-OF-OPTIONS)
	     (FERROR NIL "Odd-length options list: ~S" OPTIONS))
	   (DO ((O OPTIONS (CDDR O)))
	       ((NULL O))
	     (LET ((VALUE (CADR O)))
	       (CASE (CAR O)
		 (:AREA (SETQ AREA VALUE))
		 (:TYPE (SETQ TYPE-P T)
			(SETQ TYPE (ARRAY-CANONICALIZE-TYPE VALUE)))
		 (:ELEMENT-TYPE (SETQ ELEMENT-TYPE-P T)
		  (SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE)))
		 (:DISPLACED-INDEX-OFFSET
		  (SETQ DISPLACED-INDEX-OFFSET VALUE))
		 (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
		 ((:INITIAL-ELEMENT :INITIAL-VALUE)
		  (SETQ INITIAL-ELEMENT VALUE INITIAL-ELEMENT-P T))
		 (:INITIAL-CONTENTS
		  (SETQ INITIAL-CONTENTS VALUE INITIAL-CONTENTS-P T))
		 (:FILL-POINTER (SETQ LEADER-LENGTH (MAX 1 (OR LEADER-LENGTH 1)))
				(SETQ FILL-POINTER VALUE))
		 (:ADJUSTABLE)
		 (:LEADER-LIST (SETQ LEADER-LIST VALUE))
		 (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
;		 (:OLD-LEADER-LENGTH-OR-LIST (IF (NUMBERP VALUE)
;						 (SETQ LEADER-LENGTH VALUE)
;					       (SETQ LEADER-LIST (REVERSE VALUE))))
		 (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
		 (OTHERWISE
		  (FERROR NIL "~S is not a known MAKE-ARRAY keyword." (FIRST OPTIONS))))))))
    (IF (AND TYPE-P ELEMENT-TYPE-P)
	(FERROR NIL "Both :TYPE and :ELEMENT-TYPE specified."))
    (IF (AND DISPLACED-INDEX-OFFSET (NOT DISPLACED-TO))
	(FERROR NIL "The :DISPLACED-INDEX-OFFSET option specified without :DISPLACED-TO."))
    (IF (AND INITIAL-ELEMENT-P INITIAL-CONTENTS-P)
	(FERROR NIL "Both :INITIAL-ELEMENT and :INITIAL-CONTENTS specified."))
    ;; Process the DIMENSIONS argument.
    (CHECK-TYPE DIMENSIONS (OR FIXNUM LIST))
    (COND ((FIXNUMP DIMENSIONS)
	   (IF (MINUSP DIMENSIONS)
	       (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	   (SETQ N-DIMENSIONS 1
		 INDEX-LENGTH DIMENSIONS))
	  ((OR (NULL DIMENSIONS) (CONSP DIMENSIONS))
	   (DOLIST (DIM DIMENSIONS)
	     (UNLESS (FIXNUMP DIM)
	       (FERROR NIL "The dimension ~S is not a fixnum." DIM))
	     (IF (MINUSP DIM)
		 (FERROR NIL "The negative array dimension ~S is illegal." DIM)))
	   (SETQ N-DIMENSIONS (LENGTH DIMENSIONS))
	   (UNLESS (< N-DIMENSIONS ARRAY-RANK-LIMIT)
	       (FERROR NIL "Arrays may have at most ~D dimensions, not ~S"
		       (1- ARRAY-RANK-LIMIT) N-DIMENSIONS))
	   (SETQ INDEX-LENGTH (APPLY #'TIMES DIMENSIONS))))	;* loses in cold load, TIMES wins.
    ;; Process the DISPLACED-TO argument.
    (CHECK-TYPE DISPLACED-TO (OR NULL FIXNUM ARRAY LOCATIVE))
    ;; See whether this is a "short" or "long" format array.
    (IF (AND (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (NOT DISPLACED-TO))
	(SETQ LONG-ARRAY-P T))
    (OR (FIXNUMP INDEX-LENGTH)
	(FERROR NIL "Attempt to make array too large; total length ~S" INDEX-LENGTH))
    ;; Process the LEADER and NAMED-STRUCTURE-SYMBOL arguments.
    (CHECK-TYPE LEADER-LIST LIST)
    (AND (NULL LEADER-LENGTH) (NOT (NULL LEADER-LIST))
	 (SETQ LEADER-LENGTH (LENGTH LEADER-LIST)))
    (IF (AND LEADER-LENGTH (> (LENGTH LEADER-LIST) LEADER-LENGTH))
	(FERROR NIL "Length of leader initialization list is greater than leader length"))
    (COND (NAMED-STRUCTURE-SYMBOL
	   (IF LEADER-LENGTH
	       (SETQ LEADER-LENGTH (MAX LEADER-LENGTH 2))
	     (UNLESS (= N-DIMENSIONS 1)
	       (FERROR NIL "A named-structure array may not be ~D-dimensional"
		       N-DIMENSIONS))))
	  (LEADER-LENGTH
	   (CHECK-TYPE LEADER-LENGTH (INTEGER 0))))
    (SETQ LEADER-QS (IF LEADER-LENGTH
			(+ 2 LEADER-LENGTH)
		        0))
    ;; Process the TYPE argument.
    (CHECK-ARG TYPE (OR (FIXNUMP TYPE)
			(MEMQ TYPE ARRAY-TYPES)
			(MEMQ TYPE ARRAY-TYPE-KEYWORDS))
	       "an array type")
    (IF (FIXNUMP TYPE)
	;; may be either a small integer, which is shifted over into the type field,
	;; or an already shifted over quantity (such as ART-Q, etc).
	(IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
	    (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
      (IF (FIXNUMP (SYMBOL-VALUE TYPE))
	  (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD (SYMBOL-VALUE TYPE)))
	(SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))))
    (SETQ ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE))
    ;; This is positive if there are 1 or more entries per Q.  It is
    ;; negative if there are more than one Qs per entry.
    (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			  (CEILING INDEX-LENGTH ENTRIES-PER-Q)
			(* INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
    ;; Process the DISPLACED-INDEX-OFFSET argument.
    (CHECK-TYPE DISPLACED-INDEX-OFFSET (OR NULL (FIXNUM 0)))
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
      ;; If last index varies fastest, put in all but first dimension,
      ;; and the last dimensions come last.
      (DO ((DIMLIST (CDR DIMENSIONS) (CDR DIMLIST))
	   (I (+ N-DIMENSIONS (IF LONG-ARRAY-P 0 -1)) (1- I)))
	  ((NULL DIMLIST))
	(%P-STORE-CONTENTS-OFFSET (CAR DIMLIST) ARRAY I)))
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
      (SETF (ARRAY-LEADER ARRAY I) (CAR LEADER-LIST)))
    (AND FILL-POINTER
	 (SETF (FILL-POINTER ARRAY) FILL-POINTER))
    ;; Cretinism associated with make-array, in that the leader list can overlap
    ;; with the name-structure slot, which is how fasd dumps the named-structure-symbol
    ;; So we check for the symbol being t and not smash it in that case
    (WHEN NAMED-STRUCTURE-SYMBOL
      (IF (NULL LEADER-LENGTH)
	  ;; There is no leader; put it in element zero of the body.
	  (SETF (AREF ARRAY 0) NAMED-STRUCTURE-SYMBOL)
	;; There is a leader; use element one of the leader.
	(IF (NEQ NAMED-STRUCTURE-SYMBOL T)
	    (SETF (ARRAY-LEADER ARRAY 1) NAMED-STRUCTURE-SYMBOL)))
      ;; It is a named structure.  Set the flag.
      (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0))
    (AND INITIAL-ELEMENT-P (NOT DISPLACED-TO)
	 (ARRAY-INITIALIZE ARRAY INITIAL-ELEMENT))
    (WHEN INITIAL-CONTENTS-P
      (FILL-ARRAY-FROM-SEQUENCES ARRAY INITIAL-CONTENTS 0 0))
    ;; If there is a fill pointer on an art-q-list array, then it should control
    ;; the length of the list as well.  See array-push and array-pop.
    (WHEN (AND (= N-DIMENSIONS 1)
	       (OR FILL-POINTER (NOT (NULL LEADER-LIST)))
	       ;; The cold load generator's frame builder isn't smart enough for a #, here.
	       (= TYPE #|'#,|# (LDB %%ARRAY-TYPE-FIELD ART-Q-LIST)))
      (OR FILL-POINTER (SETQ FILL-POINTER (CAR LEADER-LIST)))
      (WHEN (AND (FIXNUMP FILL-POINTER)
		 (> FILL-POINTER 0)
		 (< FILL-POINTER (ARRAY-LENGTH ARRAY)))
	(%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- FILL-POINTER)))))
    (VALUES ARRAY DATA-LENGTH)))

))
