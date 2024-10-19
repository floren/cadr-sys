;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 300.3
;;; Reason:
;;;  MAKE-AREA: Allow modifying the existing area instead of erroring out.
;;; Written 19-Oct-24 09:05:36 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 300.0, microcode 323.



; From file OZ: /sys/sys/qfctns.lisp.776 at 19-Oct-24 09:05:47
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //sys//sys//qfctns"

(DEFUN MAKE-AREA (&REST KEYWORDS
		  &AUX (NAME NIL) (SIZE (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1))
		       (THE-REGION-SIZE NIL)
		       (REPRESENTATION %REGION-REPRESENTATION-TYPE-STRUCTURE)
		       (GC :DYNAMIC) (READ-ONLY NIL) (PDL NIL)
		       (MAP-BITS NIL) (SPACE-TYPE NIL) (SCAV-ENB NIL) (RM NIL)
		       (SWAP-RECOMMENDATIONS 0)
		       AREA-NUMBER REGION-NUMBER ARG THE-REGION-BITS)
  "Create a new area, or modify an existing one.  Returns the area number.
Takes keyword argument pairs as follows:
:NAME - symbol which provides name of area. this is required. This symbol is SET to the area.
:SIZE - maximum size (default=infinite)
:REGION-SIZE - size for regions, defaults to :SIZE if specified else medium-size.
:REPRESENTATION (:LIST, :STRUCTURE, number) - just for the initial region, default=struc
:GC (:STATIC, :TEMPORARY, :DYNAMIC) - default = dynamic
:READ-ONLY, :PDL - attributes
:ROOM - if specified, push this area onto ROOM, so that (ROOM) will list it.
:SWAP-RECOMMENDATIONS - swapin quantum size for area minus 1.
SYS:%%REGION-MAP-BITS - over-ride on map bits
SYS:%%REGION-SPACE-TYPE - over-ride on space type 
SYS:%%REGION-SCAVENGE-ENABLE - default 1, 0 disables scavenger."
  (DECLARE (ARGLIST &KEY NAME SIZE REGION-SIZE
		    REPRESENTATION GC READ-ONLY ROOM SWAP-RECOMMENDATIONS
		    SYS:%%REGION-MAP-BITS SYS:%%REGION-SPACE-TYPE
		    SYS:%%REGION-SCAVENGE-ENABLE))
  ;; Don't use a DEFVAR to initialize this variable, for then someone
  ;; else may call MAKE-AREA before it gets done.
  (OR (VARIABLE-BOUNDP AREA-TEMPORARY-FLAG-ARRAY)
      (SETQ AREA-TEMPORARY-FLAG-ARRAY
	    (MAKE-ARRAY SIZE-OF-AREA-ARRAYS :TYPE ART-1B)))
  ;; Process keyword arguments
  (DO ((L KEYWORDS (CDDR L))) ((NULL L))
    (SETQ ARG (CADR L))
    (CASE (CAR L)
      (:NAME (CHECK-TYPE ARG SYMBOL)
	     (SETQ NAME ARG))
      (:SIZE (CHECK-TYPE ARG (INTEGER 0))
	     (SETQ SIZE ARG))
      (:REGION-SIZE (CHECK-TYPE ARG (INTEGER 0))
		    (SETQ THE-REGION-SIZE ARG))
      (:REPRESENTATION
	(CHECK-TYPE ARG (OR NUMBER (MEMBER :LIST :STRUCTURE))
		    "a valid representation-type (:LIST, :STRUCTURE, or a number)")
	(SETQ REPRESENTATION
	      (COND ((EQ ARG ':LIST) %REGION-REPRESENTATION-TYPE-LIST)
		    ((EQ ARG ':STRUCTURE) %REGION-REPRESENTATION-TYPE-STRUCTURE)
		    (T ARG))))
      (:GC (CHECK-TYPE ARG (MEMBER :STATIC :TEMPORARY :DYNAMIC)
		       "a GC mode (:STATIC, :TEMPORARY, or :DYNAMIC)")
	   (SETQ GC ARG))
      (:READ-ONLY (SETQ READ-ONLY ARG))
      (:PDL (SETQ PDL ARG))
      (:ROOM (SETQ RM ARG))
      (:SWAP-RECOMMENDATIONS (SETQ SWAP-RECOMMENDATIONS ARG))
      (%%REGION-MAP-BITS (SETQ MAP-BITS ARG))
      (%%REGION-SPACE-TYPE (SETQ SPACE-TYPE ARG))
      (%%REGION-SCAVENGE-ENABLE (SETQ SCAV-ENB ARG))
      (OTHERWISE (FERROR NIL "~S is not a valid keyword for ~S" (CAR L) 'MAKE-AREA))))
  ;; Perform defaulting and concordance
  (CHECK-TYPE NAME (NOT (NULL NAME)) "specified explicitly")
  (AND (NULL THE-REGION-SIZE)
       (SETQ THE-REGION-SIZE (IF (= SIZE (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1))
				 #o40000	;Size unspecified
			         SIZE)))	;If specified, assume user wants single region
  (AND (NULL SPACE-TYPE)
       (SETQ SPACE-TYPE (IF (EQ GC ':DYNAMIC) %REGION-SPACE-NEW %REGION-SPACE-STATIC)))
  (AND (NULL SCAV-ENB)
       (SETQ SCAV-ENB (SELECT SPACE-TYPE
			;; What happens if the user specifies a random space type?
			((%REGION-SPACE-STATIC %REGION-SPACE-FIXED %REGION-SPACE-COPY) 1)
			(OTHERWISE 0))))
  (AND (NULL MAP-BITS)
       (SETQ MAP-BITS
	     (LDB %%REGION-MAP-BITS
		  ;;Meta bits
		  (%LOGDPB 1 %%REGION-OLDSPACE-META-BIT
		    (%LOGDPB 1 %%REGION-EXTRA-PDL-META-BIT
		      (%LOGDPB REPRESENTATION %%REGION-REPRESENTATION-TYPE
		  ;;Map status code
			(%LOGDPB (COND (PDL %PHT-MAP-STATUS-PDL-BUFFER)
				       (READ-ONLY %PHT-MAP-STATUS-READ-ONLY)
				       (T %PHT-MAP-STATUS-READ-WRITE-FIRST))
			     %%PHT2-MAP-STATUS-CODE
		  ;;Hardware access code
			  (%LOGDPB (COND (PDL 0)
					 (READ-ONLY 2)
					 (T 3))
				   %%PHT2-MAP-ACCESS-CODE 0))))))))
  (SETQ THE-REGION-BITS
	(%LOGDPB MAP-BITS %%REGION-MAP-BITS
		 (%LOGDPB SPACE-TYPE %%REGION-SPACE-TYPE
			  (%LOGDPB SCAV-ENB %%REGION-SCAVENGE-ENABLE
				   (%LOGDPB SWAP-RECOMMENDATIONS
					    %%REGION-SWAPIN-QUANTUM
					    0)))))
  (LET ((INHIBIT-SCHEDULING-FLAG T)		;Lock the area data-structure
	(INHIBIT-SCAVENGING-FLAG T))
    (COND ((MEMQ NAME AREA-LIST)
	   (SETQ AREA-NUMBER (SYMBOL-VALUE NAME)))
	  (T
	   (SETQ AREA-NUMBER (SYSTEM-COMMUNICATION-AREA %SYS-COM-FREE-AREA/#-LIST))
	   (WHEN (ZEROP AREA-NUMBER)
	     (FERROR NIL "Out of area numbers, cannot create ~S" NAME))
	   (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-FREE-AREA/#-LIST)
		  (AREA-REGION-LIST AREA-NUMBER))
	   ;; Next two lines set up cdr codes correctly and concs onto end of area-list
	   ;; Array leader should be right, but store anyway just in case
	   (SETF (FILL-POINTER #'AREA-NAME) AREA-NUMBER)
	   (VECTOR-PUSH NAME #'AREA-NAME)
	   (SET NAME AREA-NUMBER)))
    (SETF (AREF AREA-TEMPORARY-FLAG-ARRAY AREA-NUMBER)
	  (IF (EQ GC :TEMPORARY) 1 0))
    ;; try to leave in consistent state if bomb out.
    (STORE (AREA-REGION-LIST AREA-NUMBER) (%LOGDPB 1 %%Q-BOXED-SIGN-BIT AREA-NUMBER))
    (STORE (AREA-REGION-SIZE AREA-NUMBER) THE-REGION-SIZE)
    (STORE (AREA-MAXIMUM-SIZE AREA-NUMBER) SIZE)
    (STORE (AREA-REGION-BITS AREA-NUMBER) THE-REGION-BITS)
    (SETQ REGION-NUMBER (%MAKE-REGION THE-REGION-BITS THE-REGION-SIZE))
    (STORE (AREA-REGION-LIST AREA-NUMBER) REGION-NUMBER)
    (STORE (REGION-LIST-THREAD REGION-NUMBER) (%LOGDPB 1 %%Q-BOXED-SIGN-BIT AREA-NUMBER))
    (AND RM (PUSH NAME ROOM))
    AREA-NUMBER))		;Assign area as requested, return number

))
