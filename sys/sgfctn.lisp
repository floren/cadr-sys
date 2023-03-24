;; -*- Package:SYSTEM-INTERNALS; Mode:LISP; Base:8; Cold-load: T -*-
;; Stack Group Functions.				Recoded 1/5/78 by DLW.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFMACRO COERCE-BOOLEAN-TO-BIT (VARIABLE)
  `(OR (NUMBERP ,VARIABLE) (SETQ ,VARIABLE (IF ,VARIABLE 1 0))))

(DEFUN MAKE-STACK-GROUP (NAME &REST OPTIONS &KEY &OPTIONAL
			 (SG-AREA DEFAULT-CONS-AREA)
			 (REGULAR-PDL-AREA PDL-AREA)
			 (SPECIAL-PDL-AREA DEFAULT-CONS-AREA)
			 (REGULAR-PDL-SIZE 3000)
			 (SPECIAL-PDL-SIZE 2000)  ;big for flavors
			 (CAR-SYM-MODE 1)
			 (CAR-NUM-MODE 0)
			 (CDR-SYM-MODE 1)
			 (CDR-NUM-MODE 0)
			 (SWAP-SV-ON-CALL-OUT 1)
			 (SWAP-SV-OF-SG-THAT-CALLS-ME 1)
			 (TRAP-ENABLE 1)
			 (SAFE 1)
			 &ALLOW-OTHER-KEYS
			 &AUX SG REGULAR-PDL SPECIAL-PDL)
  "Create a stack group.  NAME, a string, is the name.  There are also keyword args.
Keywords allowed are:
:SG-AREA - specify area to cons in; default is DEFAULT-CONS-AREA.
:REGULAR-PDL-AREA - area to cons the regular pdl array in; must be a pdl area.
:SPECIAL-PDL-AREA - area to cons the special pdl array in; default is DEFAULT-CONS-AREA.
:REGULAR-PDL-SIZE - size of regular pdl in Qs; default is 3000 (octal).
:SPECIAL-PDL-SIZE - size of special pdl in Qs; default is 2000 (octal).
:TRAP-ENABLE - NIL or 0 means halt on error in this stack group.  Default is T!
:SAFE - NIL or 0 means allow stack group switching in any order.
The last two keywords can be either 1 vs 0 or T vs NIL.
Other keywords are obscure and not needed."
  (COERCE-BOOLEAN-TO-BIT CAR-SYM-MODE)
  (COERCE-BOOLEAN-TO-BIT CAR-NUM-MODE)
  (COERCE-BOOLEAN-TO-BIT CDR-SYM-MODE)
  (COERCE-BOOLEAN-TO-BIT CDR-NUM-MODE)
  (COERCE-BOOLEAN-TO-BIT SWAP-SV-ON-CALL-OUT)
  (COERCE-BOOLEAN-TO-BIT SWAP-SV-OF-SG-THAT-CALLS-ME)
  (COERCE-BOOLEAN-TO-BIT TRAP-ENABLE)
  (COERCE-BOOLEAN-TO-BIT SAFE)
    (AND (< REGULAR-PDL-SIZE 400)
	(FERROR NIL "Regular PDL size ~O not at least 400" REGULAR-PDL-SIZE))
    (SETQ SG (MAKE-ARRAY 0 ':AREA SG-AREA ':TYPE 'ART-STACK-GROUP-HEAD
			   ':LEADER-LENGTH (LENGTH STACK-GROUP-HEAD-LEADER-QS)))
    (SETQ SPECIAL-PDL (MAKE-ARRAY SPECIAL-PDL-SIZE
				  ':AREA SPECIAL-PDL-AREA
				  ':TYPE 'ART-SPECIAL-PDL 
				  ':LEADER-LENGTH (LENGTH SPECIAL-PDL-LEADER-QS)))
    (SETQ REGULAR-PDL (MAKE-ARRAY REGULAR-PDL-SIZE
				  ':AREA REGULAR-PDL-AREA
				  ':TYPE 'ART-REG-PDL
				  ':LEADER-LENGTH (LENGTH REG-PDL-LEADER-QS)))
    (SETF (REGULAR-PDL-SG REGULAR-PDL) (%MAKE-POINTER DTP-STACK-GROUP SG))
    (SETF (SPECIAL-PDL-SG SPECIAL-PDL) (%MAKE-POINTER DTP-STACK-GROUP SG))
    (SETF (SG-NAME SG) NAME)
    (SETF (SG-REGULAR-PDL SG) REGULAR-PDL)
    (SETF (SG-REGULAR-PDL-LIMIT SG) (- REGULAR-PDL-SIZE 100))
    (SETF (SG-SPECIAL-PDL SG) SPECIAL-PDL)
    (SETF (SG-SPECIAL-PDL-LIMIT SG) (- SPECIAL-PDL-SIZE 40))
    (SETF (SG-SAVED-M-FLAGS SG) 0)
    (SETF (SG-FLAGS-CAR-SYM-MODE SG) CAR-SYM-MODE)
    (SETF (SG-FLAGS-CAR-NUM-MODE SG) CAR-NUM-MODE)
    (SETF (SG-FLAGS-CDR-SYM-MODE SG) CDR-SYM-MODE)
    (SETF (SG-FLAGS-CDR-NUM-MODE SG) CDR-NUM-MODE)
    (SETF (SG-STATE SG) 0)
    (SETF (SG-SWAP-SV-ON-CALL-OUT SG) SWAP-SV-ON-CALL-OUT)
    (SETF (SG-SWAP-SV-OF-SG-THAT-CALLS-ME SG) SWAP-SV-OF-SG-THAT-CALLS-ME)
    (SETF (SG-FLAGS-TRAP-ENABLE SG) TRAP-ENABLE)
    (SETF (SG-SAFE SG) SAFE)
    (%MAKE-POINTER DTP-STACK-GROUP SG))

(DEFUN STACK-GROUP-PRESET (SG FUNCTION &REST ARGUMENTS
			      &AUX REGULAR-PDL IDX)
  "Make stack group SG apply FUNCTION to ARGUMENTS when next resumed."
    (CHECK-ARG SG (= (%DATA-TYPE SG) DTP-STACK-GROUP) "a stack group")
    (SETQ REGULAR-PDL (SG-REGULAR-PDL SG))
    (AS-1 0 REGULAR-PDL 0)
    (AS-1 0 REGULAR-PDL 1)
    (AS-1 0 REGULAR-PDL 2)
    (AS-1 FUNCTION REGULAR-PDL 3)
    (SETF (SG-INITIAL-FUNCTION-INDEX SG) 3)
    (SETF (SG-AP SG) 3)
    (SETF (SG-IPMARK SG) 3)
    (SETQ IDX (DO ((ARGL ARGUMENTS (CDR ARGL))
		   (I 4 (1+ I)))
		  ((NULL ARGL)
		   (1- I))				;Undo the last 1+
		(AS-1 (CAR ARGL) REGULAR-PDL I)
		(%P-STORE-CDR-CODE (AP-1 REGULAR-PDL I)
				   (COND ((NULL (CDR ARGL)) CDR-NIL)
					 (T CDR-NEXT)))))
    (SETF (SG-REGULAR-PDL-POINTER SG) IDX)
    (SETF (SG-PDL-PHASE SG) IDX)
    (SETF (SG-SPECIAL-PDL-POINTER SG) -1)
    (SETF (SG-CURRENT-STATE SG) SG-STATE-AWAITING-INITIAL-CALL)
    (SETF (SG-FOOTHOLD-EXECUTING-FLAG SG) 0)
    (SETF (SG-FOOTHOLD-DATA SG) NIL)			;EH depends on this
    (SETF (SG-FLAGS-QBBFL SG) 0)
    (SETF (SG-PROCESSING-ERROR-FLAG SG) 0)
    (SETF (SG-PROCESSING-INTERRUPT-FLAG SG) 0)
    (SETF (SG-IN-SWAPPED-STATE SG) 0)
    SG)

(DEFUN SG-NEVER-RUN-P (STACK-GROUP)
  "T if stack group has not been run since it was last reset or preset."
  (LET ((ST (SG-CURRENT-STATE STACK-GROUP)))
    (OR (= ST SG-STATE-AWAITING-INITIAL-CALL) (= ST 0))))

(DEFUN SG-RESUMABLE-P (STACK-GROUP)
  "T if it makes sense to resume this stack group."
  (NOT (LET ((STATE (SG-CURRENT-STATE STACK-GROUP)))
	 (OR (= STATE SG-STATE-ERROR)
	     (= STATE SG-STATE-ACTIVE)
	     (= STATE SG-STATE-EXHAUSTED)))))
