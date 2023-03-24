;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Patch-File:T; Base:8; Readtable:T -*-
;;; Patch file for System version 99.9
;;; Reason: Disassembly works better with different *print-case*'s
;;; load-and-save-patches spazz
;;; Zmacs documentation functions hack new-style documentation
;;; Zmacs c-x j doesn't cause cerebral haemorrhage when jumping to another buffer
;;; read-delimited-string understands characters (but copies... sigh)
;;; si:function-spec-get takes third optional default argument
;;; (replace foo foo ...) bug
;;; Zmacs pathname history records the result of parsing what you typed to it
;;;  Is this a good idea?
;;; Directory lister ~X Foo.
;;; cerror (commonlisp style)'s first arg is a format string, (not just a
;;;  literal string) and gets used on the same format args as the second arg.
;;; color:color-draw-line
;;; macro-function, special-form-p take environment arg.
;;; Make multiple-value-call a special form, for what that's worth.
;;; Zmacs key-for-command (as used by apropos) had typo.
;;; Compiler makes available *check-style-p* to optimizers and p1 handlers
;;; Compiler wasn't optimizing args to lexical functions
;;; Creation of lisp patch files (m-x add patch & friends) uses generic type
;;; Written 11-Oct-84 09:29:59 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.7, CADR 4.0, Experimental ZMail 54.1, MIT-Specific 23.0, Experimental Macsyma 4.0, microcode 320, GC@2.



; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	PSEUDO-KEYNAMES DECLS)
    (IF (EQ (CAR LAMBDA-EXP) 'LAMBDA)
	(SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP))
      (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP)))	;named-lambda
    (MULTIPLE-VALUE-SETQ (POSITIONAL-ARGS NIL AUXVARS
			  REST-ARG POSITIONAL-ARG-NAMES
			  KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
    (MULTIPLE-VALUE-SETQ (NIL DECLS) (EXTRACT-DECLARATIONS BODY NIL NIL))
    (DO ((D DECLS (CDR D)))
	((NULL D))
      (SETF (CAR D) `(DECLARE ,(CAR D))))
    ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
    ;; and check explicitly whether that has been overridden.
    ;; If the arg is optional
    ;; and the initial value is a constant, we can really init it to that.
    ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
    ;; after all keywords are decoded, we bind the intended variable, in sequence.
    ;; However a var that can shadow something (including any special var)
    ;; must always be replaced with a dummy.
    (DO ((KIS KEYINITS (CDR KIS))
	 (KNS KEYNAMES (CDR KNS))
	 (PKNS PSEUDO-KEYNAMES (CDR PKNS))
	 (KFS KEYFLAGS (CDR KFS)))
	((NULL KNS))
      (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
	    (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	(OR (AND (NULL KEYFLAG)
		 (CONSTANTP KEYINIT)
		 (NOT (ASSQ KEYNAME VARS))
		 (NOT (LEXICAL-VAR-P KEYNAME))
		 (NOT (SPECIALP KEYNAME)))
	    (PROGN (SETF (CAR KIS) 'SI:KEYWORD-GARBAGE)
		   (SETQ PSEUDO-KEYNAME (GENSYM))
		   (SETF (CAR PKNS) PSEUDO-KEYNAME)
		   (PUSH `(,KEYNAME
			   (COND ((EQ ,PSEUDO-KEYNAME SI:KEYWORD-GARBAGE)
				  ,KEYINIT)
				 (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
				    ,PSEUDO-KEYNAME)))
			 KEYCHECKS)))))
    (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
    (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

    ;; If the user didn't ask for a rest arg, make one for the
    ;; outer function anyway.
    (OR REST-ARG (SETQ REST-ARG (GENSYM)
		       MAYBE-REST-ARG (LIST '&REST REST-ARG)))
    `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
       (LET* (,@(MAPCAR #'(LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
	      ,@KEYFLAGS)
;       (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;	      (OR (%PERMUTE-ARGS)
;		  (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;			   (CDR ,REST-ARG)
;			   (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;			   ',KEYKEYS)
;			 (%PERMUTE-ARGS)))
;	      ;; If the function really wants the rest arg,
;	      ;; flush the permutation table and its keyword.
;	      ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;	     (T
	 (WHEN ,REST-ARG
	   (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
					,REST-ARG ',KEYKEYS
					,ALLOW-OTHER-KEYS
					(KLUDGEY-COMPILATION-VARIABLE-LOCATION
					  ,(CAR PSEUDO-KEYNAMES))))
	 (LET* ,KEYCHECKS
	   ,@decls
	   ((LAMBDA ,AUXVARS . ,BODY)))))))

(DEFUN (:PROPERTY KLUDGEY-COMPILATION-VARIABLE-LOCATION P1) (FORM &AUX TEM TEM1)
  (SETQ FORM (CADR FORM))
  (SETQ TEM (COND ((SETQ TEM1 (ASSQ FORM VARS))
		   (AND (EQ (VAR-KIND TEM1) 'FEF-ARG-FREE)
			(ZEROP (VAR-USE-COUNT TEM1))
			(PUSH (VAR-NAME TEM1) FREEVARS))
		   (VAR-LAP-ADDRESS TEM1))
		  ((SPECIALP FORM) FORM)
		  (T (BARF form "Lossage in keyed-lambda compilation" 'BARF))))
  (IF (SYMBOLP TEM)
      `(%EXTERNAL-VALUE-CELL ',TEM)
    `(VARIABLE-LOCATION ,TEM)))

))

; From file DISASS.LISP OZ:<L.SYS2> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DISASS  "

(DEFUN DISASSEMBLE (FUNCTION &AUX FEF LIM-PC ILEN (DISASSEMBLE-OBJECT-OUTPUT-FUN NIL))
  "Print a disassembly of FUNCTION on STANDARD-OUTPUT.
FUNCTION can be a compiled function, an LAMBDA-expression (which will be compiled),
or a function spec (whose definition will be used)."
  (DO ((FUNCTION FUNCTION)) (())
    (COND ((TYPEP FUNCTION 'COMPILED-FUNCTION)
	   (SETQ FEF FUNCTION)
	   (RETURN))
	  ((MEMQ (CAR-SAFE FUNCTION) '(LAMBDA NAMED-LAMBDA SUBST CLI:SUBST NAMED-SUBST))
	   (SETQ FEF (COMPILE-LAMBDA FUNCTION (GENSYM)))
	   (RETURN))
	  ((EQ (CAR-SAFE FUNCTION) 'MACRO)
	   (FORMAT T "~%Definition as macro")
	   (SETQ FUNCTION (CDR FUNCTION)))
	  ((TYPEP FUNCTION 'CLOSURE)
	   (FORMAT T "~%Definition of closed-over function")
	   (SETQ FUNCTION (CLOSURE-FUNCTION FUNCTION)))
	  (T
	   (SETQ FUNCTION (SI:DWIMIFY-PACKAGE FUNCTION))
	   (SETQ FUNCTION (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION))))))
  (WHEN (GET-MACRO-ARG-DESC-POINTER FEF)
    (SI:DESCRIBE-FEF-ADL FEF)
    (TERPRI))
  (SETQ LIM-PC (DISASSEMBLE-LIM-PC FEF))
  (DO ((PC (FEF-INITIAL-PC FEF) (+ PC ILEN))) (( PC LIM-PC))
    (TERPRI)
    (SETQ ILEN (DISASSEMBLE-INSTRUCTION FEF PC)))
  (TERPRI)
  FUNCTION)

))

; From file DISASS.LISP OZ:<L.SYS2> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DISASS  "

(DEFUN DISASSEMBLE-INSTRUCTION (FEF PC &AUX WD OP SUBOP DEST REG DISP ILEN SECOND-WORD)
  "Print on STANDARD-OUTPUT the disassembly of the instruction at PC in FEF.
Returns the length of that instruction."
  (SETQ ILEN (DISASSEMBLE-INSTRUCTION-LENGTH FEF PC))
  (BLOCK NIL
    (SETQ WD (DISASSEMBLE-FETCH FEF PC))
    (FORMAT T "~3D " pc)
    (SETQ OP (LDB #o1104 WD)
	  SUBOP (LDB #o1503 WD)
	  DEST (LDB #o1602 WD)
	  DISP (LDB #o0011 WD)
	  REG (LDB #o0603 WD))
    (COND ((= ILEN 2)
	   (INCF PC)
	   (SETQ SECOND-WORD (DISASSEMBLE-FETCH FEF PC))
	   ;; If a two-word insn has a source address, it must be an extended address,
	   ;; so set up REG and DISP to be right for that.
	   (UNLESS (= OP #o14)
	     (SETQ REG (LDB #o0603 SECOND-WORD)
		   DISP (DPB (LDB #o1104 SECOND-WORD)
			     #o0604 (LDB #o0006 SECOND-WORD))))))
    (WHEN (< OP #o11) (SETQ OP (LDB #o1105 WD)))
    (COND ((ZEROP WD)
	   (PRINC 0))
	  ((< OP #o11)				;DEST/ADDR 
	   (FORMAT T "~A ~A" (NTH OP '(CALL CALL0 MOVE CAR CDR CADR CDDR CDAR CAAR))
		   	     (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o11)				;ND1
	   (PRINC (NTH SUBOP '(ND1-UNUSED + - * // LOGAND LOGXOR LOGIOR)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o12)				;ND2
	   (PRINC (NTH SUBOP '(= > < EQ SETE-CDR SETE-CDDR SETE-1+ SETE-1-)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o13)				;ND3
	   (PRINC (NTH SUBOP '(BIND-OBSOLETE? BIND-NIL BIND-POP
			       SET-NIL SET-ZERO PUSH-E MOVEM POP)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
	  ((= OP #o14)				;BRANCH
	   (PRINC (NTH SUBOP '(BR BR-NIL BR-NOT-NIL BR-NIL-POP
			       BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM BR-ILL-7)))
	   (IF (> DISP #o400) (SETQ DISP (LOGIOR #o-400 DISP)))	;Sign-extend
	   (IF (NEQ DISP -1)
	       ;; One word
	       (FORMAT T " ~D" (+ PC DISP 1))
	     ;; Long branch
	     (SETQ DISP SECOND-WORD)
	     (IF ( DISP #o100000) (SETQ DISP (LOGIOR #o-100000 DISP)))
	     (FORMAT T " ~A ~D" 'LONG (+ PC DISP 1))
	     (RETURN)))
	  ((= OP #o15)				;MISC
	   (FORMAT T "(~A) " 'MISC)		;Moon likes to see this
	   (IF (BIT-TEST 1 SUBOP)
	       (SETQ DISP (+ DISP #o1000)))
	   (COND ((< DISP #o200)
		  (FORMAT T "~A (~D) "
			  (NTH (LDB #o0403 DISP)
			       '(AR-1 ARRAY-LEADER %INSTANCE-REF UNUSED-AREFI-3
				 AS-1 STORE-ARRAY-LEADER %INSTANCE-SET UNUSED-AREFI-7))
			  (+ (LDB #o0004 DISP)
			     (IF (= (LDB #o0402 DISP) 2) 1 0))))
		 ((< DISP #o220)
		  (FORMAT T "~A ~D binding~:P " 'UNBIND (- DISP #o177))	;200 does 1 unbind.
		  (AND (ZEROP DEST) (RETURN)))
		 ((< DISP #o240)
		  (FORMAT T "~A ~D time~:P " 'PDL-POP (- DISP #o220))	;220 does 0 pops.
		  (AND (ZEROP DEST) (RETURN)))
		 ((= DISP #o460)		;(GET 'INTERNAL-FLOOR-1 'QLVAL)
		  (PRINC (NTH DEST '(FLOOR CEILING TRUNCATE ROUND)))
		  (PRINC " one value to stack")
		  (SETQ DEST NIL))
		 ((= DISP #o510)		;(GET 'INTERNAL-FLOOR-2 'QLVAL)
		  (PRINC (NTH DEST '(FLOOR CEILING TRUNCATE ROUND)))
		  (PRINC " two values to stack")
		  (SETQ DEST NIL))
		 (T
                  (LET ((OP (MICRO-CODE-SYMBOL-NAME-AREA (- DISP #o200))))
                    (IF (NULL OP) (FORMAT T "#~O " DISP) (FORMAT T "~A " OP)))))
	   (WHEN DEST (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST)))))
	  ((= OP #o16)				;ND4
	   (CASE SUBOP
	     (0 (FORMAT T "STACK-CLOSURE-DISCONNECT  local slot ~D" DISP))
	     (1 (LET ((LOCALNUM
			(LDB #o0012 (NTH DISP
					 (%P-CONTENTS-OFFSET
					   FEF (- (%P-LDB %%FEFH-PC-IN-WORDS FEF) 2))))))
		  (PRINC 'STACK-CLOSURE-UNSHARE)
		  (FORMAT T " ~A|~D" 'LOCAL LOCALNUM)
		  (LET ((TEM (DISASSEMBLE-LOCAL-NAME FEF LOCALNUM)))
		    (AND TEM (FORMAT T " ~30,8T;~A" TEM)))))
	     (2 (FORMAT T "~A  local slot ~D" 'MAKE-STACK-CLOSURE DISP))
	     (3 (FORMAT T "~A ~S" 'push-number DISP))
	     (4 (FORMAT T "~A  local slot ~D" 'STACK-CLOSURE-DISCONNECT-FIRST DISP))
	     (5 (PRINC 'PUSH-CDR-IF-CAR-EQUAL)
		(TYO #/SPACE)
		(DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD PC))
	     (6 (PRINC 'PUSH-CDR-STORE-CAR-IF-CONS)
		(TYO #/SPACE)
		(DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD PC))
	     (T (FORMAT T "UNDEF-ND4-~D ~D" SUBOP DISP))))
	  ((= OP #o20)
	   (FORMAT T "~A (~D) "
		   (NTH REG
			'(AR-1 ARRAY-LEADER %INSTANCE-REF COMMON-LISP-AR-1
			  SET-AR-1 SET-ARRAY-LEADER SET-%INSTANCE-REF UNUSED-AREFI))
		   (+ (LDB #o0006 DISP)
		      (IF (MEMQ REG '(2 6)) 1 0)))
	   (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST))))
	  (T					;UNDEF
	   (FORMAT T "UNDEF-~O" op))))
  ILEN)

))

; From file DISASS.LISP OZ:<L.SYS2> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DISASS  "

(DEFUN DISASSEMBLE-ADDRESS (FEF REG DISP &OPTIONAL SECOND-WORD PC &AUX TEM)
  "Print out the disassembly of an instruction source address.
REG is the register number of the address, and DISP is the displacement.
SECOND-WORD should be the instruction's second word if it has two.
PC should be where the instruction was found in the FEF."
  (TYO #/SPACE)
  ;; In a one-word instruction, the displacement for types 4 thru 7 is only 6 bits,
  ;; so ignore the rest.  In a two word insn, we have been fed the full disp from word 2.
  (IF (AND ( REG 4) (NOT SECOND-WORD))
      (SETQ DISP (LOGAND #o77 DISP)))
  (COND ((< REG 4)
	  (FORMAT T "~A|~D ~30,8T;" 'FEF DISP)
	  (DISASSEMBLE-POINTER FEF DISP PC))
	((= REG 4)
	 (FORMAT T "'~S" (CONSTANTS-AREA DISP)))
        ((= REG 5)
         (FORMAT T "~A|~D" 'LOCAL DISP)
         (SETQ TEM (DISASSEMBLE-LOCAL-NAME FEF DISP))
         (AND TEM (FORMAT T " ~30,8T;~A" TEM)))
        ((= REG 6)
         (FORMAT T "~A|~D" 'ARG DISP)
         (SETQ TEM (DISASSEMBLE-ARG-NAME FEF DISP))
         (AND TEM (FORMAT T " ~30,8T;~A" TEM)))
	((AND (NOT SECOND-WORD) (= DISP #o77))
	 (PRINC 'PDL-POP))
	((< DISP #o40)
         (FORMAT T "~A|~D" SELF DISP)
         (SETQ TEM (DISASSEMBLE-INSTANCE-VAR-NAME FEF DISP))
         (AND TEM (FORMAT T " ~30,8T;~A" TEM)))
	((< DISP #o70)
         (FORMAT T "~A|~D" 'SELF-MAP (- DISP #o40))
         (SETQ TEM (DISASSEMBLE-MAPPED-INSTANCE-VAR-NAME FEF (- DISP #o40)))
         (AND TEM (FORMAT T " ~30,8T;~A" TEM)))
	(T (FORMAT T "PDL|~D (undefined)" DISP))))

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-AND-SAVE-PATCHES (&OPTIONAL BAND &REST KEYWORD-ARGS)
  "Load all patches and save a new Lisp world in a disk partition.
KEYWORD-ARGS are passed to LOAD-PATCHES.
BAND is the name or number of a LOD band to save in."
  (CHECK-TYPE BAND (OR NUMBER STRING NULL) "A specifier for a band")
  (IF (OR (MEMQ :FORCE-UNFINISHED KEYWORD-ARGS)
	  (MEMQ :UNRELEASED KEYWORD-ARGS))
      (FERROR NIL ":FORCE-UNFINISHED and :UNRELEASED are not reasonable arguments here."))
  (DOLIST (PATCH-SYSTEM PATCH-SYSTEMS-LIST)
    (WHEN (EQ (PATCH-STATUS PATCH-SYSTEM) :INCONSISTENT)
      (BEEP)
      (FORMAT *QUERY-IO* "~&You have loaded patches out of sequence,
 or loaded unreleased patches, in ~A.
As a result, the environment is probably inconsistent with the
current patches and will remain so despite attempts to update it.
Unless you understand these problems well and know how to
be sure whether they are occurring, or how to clean them up,
you should not save this environment."
	      (PATCH-NAME PATCH-SYSTEM))
      (SEND *QUERY-IO* :CLEAR-INPUT)
      (UNLESS (YES-OR-NO-P "Dump anyway? ")
	(RETURN-FROM LOAD-AND-SAVE-PATCHES NIL))))
  (DO ((BAND1 BAND (PROMPT-AND-READ :STRING "~&Save into which band? "))
       (COUNT 0 (1+ COUNT)))
      (())
    (WHEN BAND1
      (COND ((NUMBERP BAND1)
	     (SETQ BAND1 (FORMAT NIL "LOD~D" BAND1)))
	    ((PARSE-NUMBER BAND1 0 NIL NIL T)
	     (SETQ BAND1 (STRING-APPEND "LOD" BAND1))))
      (COND ((NOT (STRING-EQUAL BAND1 "LOD" :END1 3))
	     (FORMAT *QUERY-IO* "~&You must save into a LOD partition."))
	    ((NOT (FIND-DISK-PARTITION BAND1))
	     (FORMAT *QUERY-IO* "~&No such band: ~A." BAND1))
	    ((FIND-DISK-PARTITION-FOR-WRITE BAND1)
	     ;; Non-NIL means user gave confirmation.
	     (SETQ BAND BAND1)
	     (RETURN))))
    (IF (ZEROP COUNT) (PRINT-DISK-LABEL)))
  (WITH-SYS-HOST-ACCESSIBLE
    (COND ((APPLY #'LOAD-PATCHES :NOSELECTIVE KEYWORD-ARGS)
	   (DISK-SAVE BAND T))
	  (T (FORMAT *QUERY-IO* "~&No patches have been made.")))))

))


; From file QIO.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(compiler:make-obsolete decode-read-args "you probably want to be using DECODE-KLUDGEY-MUCKLISP-READ-ARGS instead")

))

; From file PRESS.LISP OZ:<L.IO1> OZ:
#8R PRESS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "PRESS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; PRESS  "

(DEFVAR ALL-DOVER-FONTS '(

  (APL || 8.) (APL || 10.)
  
  (ARROWS || 10.)

  (BOX || 10.)

  (CMU || 10.)

  (CREAM || 10.) (CREAM || 12.)
  (CREAM B 10.) (CREAM B 12.)
  (CREAM I 10.) (CREAM I 12.)
  (CREAM BI 10.) (CREAM BI 12.)

  (DOTS || 7.)

  (ELITE || 10.)

  (GACHA || 5.) (GACHA || 6.) (GACHA || 7.)
  (GACHA || 8.) (GACHA || 9.) (GACHA || 10.)
  (GACHA || 12.)

  (GACHA B 5.) (GACHA B 6.) (GACHA B 7.)
  (GACHA B 8.) (GACHA B 9.) (GACHA B 10.)
  (GACHA B 12.)

  (GACHA I 5.) (GACHA I 6.) (GACHA I 7.)
  (GACHA I 8.) (GACHA I 9.) (GACHA I 10.)
  (GACHA I 12.)

  (GACHA BI 5.) (GACHA BI 6.) (GACHA BI 7.)
  (GACHA BI 8.) (GACHA BI 9.) (GACHA BI 10.)
  (GACHA BI 12.)
  
  (GATES || 10.) (GATES || 12.) (GATES || 18.)
  (GATES || 32.)

  (HELVETICA || 3.) (HELVETICA || 4.) (HELVETICA || 5.)
  (HELVETICA || 6.) (HELVETICA || 7.) (HELVETICA || 8.)
  (HELVETICA || 9.) (HELVETICA || 10.) (HELVETICA || 11.)
  (HELVETICA || 12.) (HELVETICA || 14.) (HELVETICA || 18.)

  (HELVETICA B 6.) (HELVETICA B 7.) (HELVETICA B 8.)
  (HELVETICA B 9.) (HELVETICA B 10.) (HELVETICA B 11.)
  (HELVETICA B 12.) (HELVETICA B 14.) (HELVETICA B 18.)

  (HELVETICA I 6.) (HELVETICA I 7.) (HELVETICA I 8.)
  (HELVETICA I 9.) (HELVETICA I 10.) (HELVETICA I 11.)
  (HELVETICA I 12.) (HELVETICA I 14.) (HELVETICA I 18.)

  (HELVETICA BI 6.) (HELVETICA BI 7.) (HELVETICA BI 8.)
  (HELVETICA BI 9.) (HELVETICA BI 10.) (HELVETICA BI 11.)
  (HELVETICA BI 12.) (HELVETICA BI 14.) (HELVETICA BI 18.)

  (HELVETICAD || 24.) (HELVETICAD || 30.) (HELVETICAD || 36.)

  (HELVETICAMIT || 10.)

  (HELVETICASC || 9.) (HELVETICASC || 10.)
  (HELVETICASC B 9.) (HELVETICASC B 10.)

  (HIPPO || 6.) (HIPPO || 8.) (HIPPO || 10.)
  (HIPPO || 12.) (HIPPO || 14.) (HIPPO || 18.)

  (LPT || 6.) (LPT || 8.) (LPT || 10.)
  (LPT B 10.)

  (MATH || 6.) (MATH || 8.) (MATH || 10.)
  (MATH || 12.) (MATH || 14.) (MATH || 18.)

  (OLDENGLISH || 10.) (OLDENGLISH || 12.) (OLDENGLISH || 18.)
  (OLDENGLISH || 24.) (OLDENGLISH || 36.) (OLDENGLISH || 48.)

  (SAIL || 6.) (SAIL || 8.) (SAIL || 10.)

  (SIGMA || 20.)

  (SYMBOL || 10.)

  (TEMPLATE || 10.) (TEMPLATE || 12.) (TEMPLATE || 18.)
  (TEMPLATE || 64.)

  (TIMESROMAN || 4.) (TIMESROMAN || 6.) (TIMESROMAN || 7.)
  (TIMESROMAN || 8.) (TIMESROMAN || 9.) (TIMESROMAN || 10.)
  (TIMESROMAN || 11.) (TIMESROMAN || 12.) (TIMESROMAN || 14.)
  (TIMESROMAN || 17.) (TIMESROMAN || 18.)

  (TIMESROMAN B 6.) (TIMESROMAN B 7.)
  (TIMESROMAN B 8.) (TIMESROMAN B 9.) (TIMESROMAN B 10.)
  (TIMESROMAN B 11.) (TIMESROMAN B 12.) (TIMESROMAN B 14.)
  (TIMESROMAN B 17.) (TIMESROMAN B 18.)

  (TIMESROMAN I 6.) (TIMESROMAN I 7.)
  (TIMESROMAN I 8.) (TIMESROMAN I 9.) (TIMESROMAN I 10.)
  (TIMESROMAN I 11.) (TIMESROMAN I 12.) (TIMESROMAN I 14.)
  (TIMESROMAN I 17.) (TIMESROMAN I 18.)

  (TIMESROMAN BI 6.) (TIMESROMAN BI 7.)
  (TIMESROMAN BI 8.) (TIMESROMAN BI 9.) (TIMESROMAN BI 10.)
  (TIMESROMAN BI 11.) (TIMESROMAN BI 12.) (TIMESROMAN BI 14.)
  (TIMESROMAN BI 17.)

  (TIMESROMAND || 24.) (TIMESROMAND || 30.) (TIMESROMAND || 36.)

  (TIMESROMANMIT || 10.)

  (TIMESROMANSC || 9.) (TIMESROMANSC || 10.)

  ))

))

; From file HASH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; HASH  "

(defselect ((:property hash-array named-structure-invoke))
  (:fasload-fixup (self)
    ;; Force rehash as if due to gc, because hash codes are all wrong now.
    ;; Also fix up cdr codes.
    (setf (hash-table-gc-generation-number self) -1)
    (do ((i 0 (+ i blen))
	 (blen (hash-table-block-length self))
	 (length (array-length self)))
	(( i length))
      (%p-store-cdr-code (locf (cli:aref self (+ i blen -1))) cdr-nil)))
  (:print-self (self stream &optional ignore ignore)
    (printing-random-object (self stream)
      (format stream "~S-~S~A"
	      (function-name (hash-table-compare-function self)) 'hash-array
	      (if (hash-table-funcallable-p self)
		  " (Funcallable)" "")))))

))

; From file HASHFL.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; HASHFL  "

(defun clrhash (hash-table &optional ignore)
  "Clear out a hash table; leave it with no entries."
  (send hash-table :clear-hash)
  hash-table)

(defun maphash (function hash-table &rest extra-args)
  "Apply FUNCTION to each item in HASH-TABLE; ignore values.
FUNCTION's arguments are the key followed by the values associated with it,
 followed by the EXTRA-ARGS."
  (lexpr-send hash-table :map-hash function extra-args)
  nil)

))

; From file COMF.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-LONG-DOCUMENTATION "Prints long documentation for the specified symbol or function.
Reads the name of the function or symbol from the mini-buffer
/(the default is the /"current/" function from the buffer).
First comes the arglist of the name as a function, if it is defined,
and then the documentation of the function.
Then, if the name is a symbol, comes the documentation for the name in its other roles." ()
  (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)))))
    (LET ((DOC (DOCUMENTATION NAME 'FUNCTION))
	  (ALL-DOC (AND (SYMBOLP NAME) (GET NAME 'SI::DOCUMENTATION-PROPERTY))))
      (IF (NOT (FDEFINEDP NAME))
	  (FORMAT T "~S:" NAME)
	(SEND *STANDARD-OUTPUT* :FRESH-LINE)
	(PRINT-ARGLIST NAME *STANDARD-OUTPUT*))
      (COND (ALL-DOC
	     (LOOP FOR (KIND STRING) ON ALL-DOC BY 'CDDR
		   UNLESS (STRING= STRING KIND 'FUNCTION)
		   DO (FORMAT T "~&~%Documentation of ~S as a ~A:~%~A~%"
			      NAME (STRING-DOWNCASE KIND) STRING)))
	    (DOC
	     (FORMAT T "~&~A" DOC))
	    (T
	     (FORMAT *QUERY-IO* "~&~S is not documented." NAME)))))
  DIS-NONE)

))

; From file COMF.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-BRIEF-DOCUMENTATION "Prints brief documentation for the specified function.
Reads the name of the function from the mini-buffer (the default is
the /"current/" function from the buffer) and prints the first
line of its documentation in the echo area." ()
    (LET ((NAME (READ-FUNCTION-NAME "Brief Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (IF (NULL NAME) (BARF)
	(LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	  (COND ((NULL DOC) (FORMAT *QUERY-IO* "~&~S is not documented as a function" NAME))
		(T (FORMAT *QUERY-IO* "~&~S: ~A" NAME
			   (SUBSTRING DOC 0 (STRING-SEARCH-CHAR #/CR DOC))))))))
    DIS-NONE)

))

; From file COMF.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-QUICK-DOCUMENTATION "Prints documentation for the function point is at.
Prints the documentation string of the function which point is inside a call to.
With a numeric argument, reads the name of the function to document
from the mini buffer." ()
    (LET ((NAME (RELEVANT-FUNCTION-NAME (POINT))))
      (IF *NUMERIC-ARG-P*
	  (SETQ NAME (READ-FUNCTION-NAME "Brief Document" NAME T)))
      (IF (NULL NAME) (BARF)
	(LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	  (COND ((NULL DOC)
		 (FORMAT *QUERY-IO* "~&~S is not documented as a function" NAME))
		(T (IF (FDEFINEDP NAME)
		       (PROGN (SEND *STANDARD-OUTPUT* :FRESH-LINE)
			      (PRINT-ARGLIST NAME *STANDARD-OUTPUT*))
		       (FORMAT T "~S:" NAME))
		   (FORMAT T "~%~A" DOC))))))
    DIS-NONE)

))

; From file COMD.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-JUMP-TO-SAVED-POSITION "Restore a saved position from a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
    (LET ((PT (GET Q-REG 'POINT)))
      (WHEN (NULL PT)
	(BARF "The register ~A doesn't point anywhere." Q-REG))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
      (MAKE-BUFFER-CURRENT (CDR PT))
      (MOVE-BP (POINT) (CAR PT))))
  DIS-BPS)

))

; From file POSS.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFCOM COM-EDIT-DEFINITION "Go to the definition of a specified function.
The name of the function is read from the mini-buffer." ()
  (IF *NUMERIC-ARG-P*
      (EDIT-NEXT-DEFINITION)
    (LET (SPEC STRING EXPLICIT-PACKAGE-P)
      (SETF (VALUES SPEC STRING EXPLICIT-PACKAGE-P)
	    (READ-FUNCTION-NAME "Edit definition" (RELEVANT-FUNCTION-NAME (POINT))
				'AARRAY-OK))
      (SETQ SPEC (LIST SPEC))
      ;; If there's only one entry in the aarray, and its for a different package,
      ;; but the symbol in the current package has some sort of definition in a file,
      ;; include them both.
      (IF (AND (NOT EXPLICIT-PACKAGE-P) (SYMBOLP (CAR SPEC)))
	  (MULTIPLE-VALUE-BIND (THIS-PKG-SYMBOL FOUNDP)
	      (INTERN-SOFT (STRING-UPCASE (STRING (CAR SPEC))))
	    (IF (AND FOUNDP
		     (NEQ THIS-PKG-SYMBOL (CAR SPEC))
		     (GET THIS-PKG-SYMBOL :SOURCE-FILE-NAME))
		(PUSH THIS-PKG-SYMBOL SPEC))))
      (EDIT-DEFINITION-1 (CAR SPEC) (IF EXPLICIT-PACKAGE-P T SPEC) STRING)))
  DIS-TEXT)

))

; From file QIO.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(defun read-delimited-string (&optional (delimiter #/End) (stream *standard-input*)
			      eof-error-p rh-options (buffer-size 100.))
  "Reads input from STREAM until DELIMITER is found; returns a string.
Uses the rubout handler if STREAM supports that.
DELIMITER is either a character or a list of characters.
 (Characters may be fixnums or character objects).
Values are:
 The string of characters read, not including the delimiter
 T if input ended due to end of file
 The delimiter character read (as a fixnum), or NIL if ended at EOF.
EOF-ERROR-P if non-NIL means get error on end of file before any input is got.
RH-OPTIONS are passed to WITH-INPUT-EDITING.
BUFFER-SIZE is the size to make the buffer string, initially."
  (declare (values string eof-flag delimiter))
  (setq stream (decode-read-arg stream))
  (typecase delimiter
    (character (setq delimiter (char-int delimiter)))
    (cons (loop for x in delimiter
		when (characterp delimiter)
		return (setq delimiter (mapcar #'(lambda (x)
						   (if (characterp x) (char-int x) x))
					       delimiter)))))
  (with-stack-list (activation :activation
			       (if (consp delimiter) 'memq 'eq)
			       delimiter)
    (with-stack-list* (options activation rh-options)
      (with-input-editing (stream options)
	(do ((buffer (make-array buffer-size :type art-string :fill-pointer 0)))
	    (())
	  (let ((ch (send stream (if rubout-handler :any-tyi :tyi)
			  (and (zerop (length buffer)) eof-error-p))))
	    (cond ((null ch)
		   (return buffer t))
		  ((consp ch)
		   (when (eq (car ch) :activation)
		     (send stream :tyo (cadr ch))
		     (return buffer nil (cadr ch))))
		  ((and (not rubout-handler)
			(if (consp delimiter) (memq ch delimiter) (eq ch delimiter)))
		   (return buffer nil ch))
		  (t
		   (vector-push-extend ch buffer)))))))))

))

; From file FLAVOR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2 &AUX FL)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(METHOD-TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC))
	(DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (IF (NULL (CDDDR FUNCTION-SPEC))
	(SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (COND ((NOT (AND (SYMBOLP FLAVOR)
		     (SYMBOLP METHOD-TYPE)
		     (SYMBOLP MESSAGE)
		     ( 3 (LENGTH FUNCTION-SPEC) 5)))
	   (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	     (FERROR 'SYS:INVALID-FUNCTION-SPEC
		     "The function spec ~S is invalid." FUNCTION-SPEC)))
	  ((EQ T (SETQ FL (COMPILATION-FLAVOR FLAVOR)))
	   ;; Silly pseudo-flavor for cold-load stream
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       T
	     ;;The property-list operations need to work for the editor
	     (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))
	  ((OR FL		;A defined flavor
	       (NOT (CLASS-SYMBOLP FLAVOR)))	;Not defined, assume flavor
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       T
	     ;; Ignore FASLOAD-COMBINED methods if flavor methods composed already.
	     (IF (AND FL (FLAVOR-METHOD-HASH-TABLE FL)
		      (EQ (THIRD FUNCTION-SPEC) 'FASLOAD-COMBINED))
		 ;; This hair makes defining (INTERNAL (:METHOD FOO FASLOAD-COMBINED ...) ...)
		 ;; get ignored properly and not get an error.
		 (SELECTQ FUNCTION
		   (FDEFINITION LAST-FASLOAD-COMBINED-METHOD)
		   (FDEFINEDP T)
		   (FDEFINE
		    (SETQ LAST-FASLOAD-COMBINED-METHOD ARG1))
		   (FDEFINITION-LOCATION (LOCF LAST-FASLOAD-COMBINED-METHOD))
		   (T NIL))
	       ;; Otherwise refer to or define the :COMBINED method.
	       (IF (EQ METHOD-TYPE 'FASLOAD-COMBINED)
		   (SETQ FUNCTION-SPEC (LIST* (FIRST FUNCTION-SPEC) FLAVOR
					      :COMBINED (CDDDR FUNCTION-SPEC))
			 METHOD-TYPE :COMBINED))
	       (LET ((METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC
			     (CASE FUNCTION
			       ((PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE)
				NIL)		;Create.
			       (OTHERWISE T)))))	;Don't create
		 (OR (AND METH (METH-DEFINEDP METH))
		     (MEMQ FUNCTION '(FDEFINEDP COMPILER-FDEFINEDP
				      PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE
				      GET FUNCTION-PARENT DWIMIFY))
		     (IF FL
			 (FERROR NIL "~S is not a defined method; it is not possible to ~S it"
			         FUNCTION-SPEC FUNCTION)
		         (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC)))
		 (SELECTQ FUNCTION
		   (FDEFINE
		     (OR FL
			 (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC))
		     (LET ((DEFINITION-NEW (NOT (METH-DEFINEDP METH)))
			   (OLD-DEFINITION (AND (METH-DEFINEDP METH) (METH-DEFINITION METH))))
		       (SETF (METH-DEFINITION METH) ARG1)
		       ;; If we load a method compiled before system 83,
		       ;; that expects instance variables to be bound,
		       ;; make it work by forcing this flavor to bind all variables.
		       (IF (AND (TYPEP ARG1 'COMPILED-FUNCTION)
				(ZEROP (%P-LDB %%FEFH-GET-SELF-MAPPING-TABLE ARG1))
				(NOT (ASSQ 'ENCAPSULATED-DEFINITION (DEBUGGING-INFO ARG1))))
			   (MAKE-FLAVOR-ALL-SPECIAL FL))
		       ;; Incrementally recompile the flavor if this is a new method, unless
		       ;; it is a :COMBINED method, which is the result of compilation,
		       ;; not a client of it.
		       (COND ((MEMQ METHOD-TYPE '(:WRAPPER :INVERSE-WRAPPER))
			      (OR (AND (CONSP OLD-DEFINITION)
				       (FEF-EQUAL (CDR ARG1) (CDR OLD-DEFINITION)))
				  ;; Wrapper is really changed; must recompile flavors.
				  ;; Arrange that if we abort, the definition is set
				  ;; to the symbol ABORTED-DEFINITION.  This is a no-op,
				  ;; and redefining or undefining the wrapper will recompile.
				  (LET (SUCCESS)
				    (UNWIND-PROTECT
				      (PROGN
					(RECOMPILE-FLAVOR FLAVOR MESSAGE NIL)
					(SETQ SUCCESS T))
				      (OR SUCCESS
					  (SETF (METH-DEFINITION METH)
						'ABORTED-DEFINITION))))))
			     ((EQ METHOD-TYPE :COMBINED) NIL)
			     (DEFINITION-NEW
			      ;; This SETF, by virtue of the preceding clause,
			      ;; arranges that if we abort out before finishing recompilation
			      ;; then the recompilation will be done again if the user
			      ;; either redoes the defmethod or does undefmethod.
			      (SETF (METH-DEFINITION METH) 'ABORTED-DEFINITION)
			      (RECOMPILE-FLAVOR FLAVOR MESSAGE)
			      (SETF (METH-DEFINITION METH) ARG1))
			     ;; If method defined as a random symbol,
			     ;; must fix up hash table each time it changes.
			     ((OR (SYMBOLP OLD-DEFINITION)
				  (SYMBOLP ARG1))
			      (RECOMPILE-FLAVOR FLAVOR MESSAGE)))))
		   (FDEFINITION (METH-DEFINITION METH))
		   (FDEFINEDP (AND METH (VALUES (METH-DEFINEDP METH)
						(AND (METH-DEFINEDP METH)
						     (METH-DEFINITION METH)))))
		   (FDEFINITION-LOCATION (LOCF (METH-DEFINITION METH)))
		   (FUNDEFINE
		    (SETF (METH-DEFINITION METH) 'UNDEFINITION-IN-PROGRESS)
		    (RECOMPILE-FLAVOR (FLAVOR-NAME FL) MESSAGE)	;Propagate the change
		    (NULLIFY-METHOD-DEFINITION METH))	;Say propagation is complete.
		   (COMPILER-FDEFINEDP METH)
		   (GET (AND METH (GETF (METH-PLIST METH) ARG1 ARG2)))
		   (PUTPROP (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			      (SETF (GETF (METH-PLIST METH) ARG2) ARG1)))
		   (PUSH-PROPERTY
		    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
		      (PUSH ARG1 (GETF (METH-PLIST METH) ARG2))))
		   (DWIMIFY
		    (CATCH-CONTINUATION 'DWIMIFY-PACKAGE
			#'(LAMBDA (NEW-SPEC) NEW-SPEC)
			#'(LAMBDA () NIL)
		      (DOLIST (COMPONENT
				(OR (FLAVOR-DEPENDS-ON-ALL FL)
				    (COMPOSE-FLAVOR-COMBINATION FL NIL)))
			(LET ((FLAVOR (COMPILATION-FLAVOR COMPONENT))
			      (METHS))
			  (AND FLAVOR
			       (SETQ METHS
				     (CDDDR (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FLAVOR)))))
			  (DOLIST (METH METHS)
			    (AND (METH-DEFINEDP METH)
				 (DWIMIFY-PACKAGE-2 (METH-FUNCTION-SPEC METH)
						    ARG1 ARG2 T)))))))
		   (OTHERWISE
		    (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))))
	  (T
	   (CLASS-METHOD-FUNCTION-SPEC-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

))

; From file QRAND.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN FUNCTION-SPEC-DEFAULT-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  "This subroutine handles various operations for other function spec handlers."
  (CASE FUNCTION
    (VALIDATE-FUNCTION-SPEC T)	;Used only during system build, via FUNCTION-SPEC-GET
    (FUNCTION-PARENT NIL)		;Default is no embedding in other definitions
    (COMPILER-FDEFINEDP NIL)		;Default is no remembering of compiled definitions
    (DWIMIFY NIL)
    (GET (IF FUNCTION-SPEC-HASH-TABLE
	     ;; Default is to use plist hash table
	     (WITH-STACK-LIST (KEY FUNCTION-SPEC ARG1)
	       (GETHASH KEY FUNCTION-SPEC-HASH-TABLE ARG2))
	   (LOOP FOR (FS IND PROP) IN COLD-LOAD-FUNCTION-PROPERTY-LISTS
		 WHEN (AND (EQUAL FS FUNCTION-SPEC) (EQ IND ARG1))
		 RETURN PROP))
	 ARG2)
    (PUTPROP (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
		   (AREA (%AREA-NUMBER FUNCTION-SPEC)))
	       (IF (OR (AREA-TEMPORARY-P AREA)
		       (= AREA PDL-AREA))
		   (SETQ FUNCTION-SPEC (COPYTREE FUNCTION-SPEC)))
	       (IF FUNCTION-SPEC-HASH-TABLE
		   (SETF (GETHASH (LIST FUNCTION-SPEC ARG2) FUNCTION-SPEC-HASH-TABLE) ARG1)
		 (PUSH (LIST FUNCTION-SPEC ARG2 ARG1) COLD-LOAD-FUNCTION-PROPERTY-LISTS))))
    (PUSH-PROPERTY
     (WITH-STACK-LIST (KEY FUNCTION-SPEC ARG2)
       (PUSH ARG1 (GETHASH KEY FUNCTION-SPEC-HASH-TABLE))))
    (OTHERWISE (FERROR NIL "~S is not implemented by the function spec ~S"
		           FUNCTION FUNCTION-SPEC))))

))

; From file QRAND.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN FUNCTION-SPEC-GET (FUNCTION-SPEC PROPERTY &OPTIONAL DEFAULT)
  "Get the PROPERTY property of FUNCTION-SPEC.
For symbols, this is just GET, but it works on any function spec."
  (IF (SYMBOLP FUNCTION-SPEC)
      (GET FUNCTION-SPEC PROPERTY DEFAULT)
    ;; Look for a handler for this type of function spec.
    (LET ((HFUN
	    (IF (NULL FUNCTION-SPEC-HASH-TABLE)
		;; While loading files with MINI during system build,
		;; always use the default handler,
		;; which stores on COLD-LOAD-FUNCTION-PROPERTY-LISTS.
		;; This is so that the property's pathnames will be canonicalized later.
		'FUNCTION-SPEC-DEFAULT-HANDLER
	      (AND (CONSP FUNCTION-SPEC)
		   (SYMBOLP (CAR FUNCTION-SPEC))
		   (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))))
      (IF HFUN
	  (AND (FUNCALL HFUN 'VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
	       ;;previous line avoids lossage when compiling defselects which aren't present
	       ;; in run time environment (yet), for example.		
	       (FUNCALL HFUN 'GET FUNCTION-SPEC PROPERTY DEFAULT))
	(FERROR 'SYS:INVALID-FUNCTION-SPEC "The function spec ~S is invalid."
		FUNCTION-SPEC)))))

))

; From file GENRIC.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun replace (into-sequence-1 from-sequence-2 &key (start1 0) end1 (start2 0) end2)
  "Copy all or part of FROM-SEQUENCE-2 into INTO-SEQUENCE-1.
A sequence is either a list or a vector.
START1 and END1 specify the part of FROM-SEQUENCE-2 to be copied.
 They default to 0 and NIL (which means the end of the sequence).
START2 and END2 specify the part of INTO-SEQUENCE-1 to be copied into.
If the subsequence to be copied into is longer than the one to be copied,
 the extra elements of the to-subsequence are left unchanged.
If the two sequences are the same, the data is first copied to a
 intermediate location and then copied back in.
The value is INTO-SEQUENCE-1."
  (if (eq into-sequence-1 from-sequence-2)
      (let* ((n-copy (min (- (or end2 (length from-sequence-2)) start2)
			  (- (or end1 (length into-sequence-1)) start1)))
	     (temp (get-temp-vector n-copy)))
	(replace temp from-sequence-2 :start2 start2 :end2 end2 :end1 n-copy)
	(replace into-sequence-1 temp :end2 n-copy :start1 start1 :end1 end1)
	(setq temp-vector temp))
    (if (and (arrayp into-sequence-1) (arrayp from-sequence-2))
	(let ((n-copy (min (- (or end2 (length from-sequence-2)) start2)
			   (- (or end1 (length into-sequence-1)) start1))))
	  (copy-array-portion from-sequence-2 start2 (+ start2 n-copy)
			      into-sequence-1 start1 (+ start1 n-copy)))
      (let ((store-index (if (arrayp into-sequence-1) start1 (nthcdr start1 into-sequence-1)))
	    (store-end (if end1
			   (if (arrayp into-sequence-1) end1
			     (nthcdr into-sequence-1 end1))
			 (seq-end into-sequence-1)))
	    (fetch-end (seq-end from-sequence-2 end2))
	    (fetch-index (seq-start from-sequence-2 start2)))
	(do ()
	    ((or (eq store-index store-end)
		 (eq fetch-index fetch-end)))
	  (seq-store into-sequence-1 store-index
		     (seq-fetch-inc from-sequence-2 fetch-index))
	  (seq-inc store-index)))))
  into-sequence-1)

))

; From file FILES.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN READ-DEFAULTED-PATHNAME (PROMPT DEFAULTS
				&OPTIONAL SPECIAL-TYPE SPECIAL-VERSION (DIRECTION :READ)
					  (MERGE-IN-SPECIAL-VERSION T) (MERGE-TYPE T)
				MINI-BUFFER-INITIAL-CONTENTS
				MINI-BUFFER-INITIAL-CHAR-POS)
  "Read and default a pathname, prompting with PROMPT, which should end with a colon.
DEFAULTS is a defaults-alist used as the defaults, and modified.
DIRECTION should be :READ or :WRITE; used for completion.
The remaining arguments are passed along to MAKE-DEFAULTED-PATHNAME (which see)
MINI-BUFFER-INITIAL-CONTENTS is a string to initialize the mini buffer with,
 MINI-BUFFER-INITIAL-CHAR-POS an index in that string to start the cursor out at."
  (LET* ((STRING (READ-UNDEFAULTED-PATHNAME-STRING PROMPT
						   DEFAULTS
						   SPECIAL-TYPE
						   SPECIAL-VERSION
						   DIRECTION
						   MERGE-IN-SPECIAL-VERSION
						   MINI-BUFFER-INITIAL-CONTENTS
						   MINI-BUFFER-INITIAL-CHAR-POS))
	 (PATHNAME (MAKE-DEFAULTED-PATHNAME STRING
					    DEFAULTS
					    SPECIAL-TYPE
					    SPECIAL-VERSION
					    MERGE-IN-SPECIAL-VERSION
					    MERGE-TYPE)))
    (PUSH-ON-HISTORY (SEND PATHNAME :STRING-FOR-PRINTING) *PATHNAME-ARGUMENT-HISTORY*)
    PATHNAME))

))

; From file FILES.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN READ-UNDEFAULTED-PATHNAME-STRING
       (PROMPT *READING-PATHNAME-DEFAULTS*
	&OPTIONAL *READING-PATHNAME-SPECIAL-TYPE*
	*READING-PATHNAME-SPECIAL-VERSION*
	(*READING-PATHNAME-DIRECTION* :READ)
	(MERGE-IN-SPECIAL-VERSION T)
	MINI-BUFFER-INITIAL-CONTENTS
	MINI-BUFFER-INITIAL-CHAR-POS
	&AUX
	(*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
  ;; MERGE-IN-SPECIAL-VERSION is for the case of wanting the default to have :OLDEST, but
  ;; not having pathnames typed in keeping to this.
  (IF (NOT MERGE-IN-SPECIAL-VERSION)
      (SETQ *READING-PATHNAME-SPECIAL-VERSION* NIL))	;Don't complete from this
  (WITH-STACK-LIST (PROMPT (FORMAT NIL "~A (Default is ~A" prompt
				   (FS:DEFAULT-PATHNAME *READING-PATHNAME-DEFAULTS*
							NIL
				     			*READING-PATHNAME-SPECIAL-TYPE*
							*READING-PATHNAME-SPECIAL-VERSION*))
			   '(:RIGHT-FLUSH " (Completion)"))
    (LET ((STRING (STRING-INTERVAL
		    (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB*
						      MINI-BUFFER-INITIAL-CONTENTS
						      MINI-BUFFER-INITIAL-CHAR-POS
						      PROMPT)))))
      ;; it is now the responsibility of callers to do this
      ;(PUSH-ON-HISTORY STRING *PATHNAME-ARGUMENT-HISTORY*)
      STRING)))					;Return the correct pathname string

))

; From file FILES.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN DEFAULT-LIST-ONE-FILE (FILE &OPTIONAL (STREAM *STANDARD-OUTPUT*) &AUX PATHNAME)
  (COND ((NULL (SETQ PATHNAME (CAR FILE)))
	 (COND ((GET FILE :DISK-SPACE-DESCRIPTION)
		(SEND STREAM :LINE-OUT (GET FILE :DISK-SPACE-DESCRIPTION)))
	       ((GET FILE :PHYSICAL-VOLUME-FREE-BLOCKS)
		(DO ((FREE (GET FILE :PHYSICAL-VOLUME-FREE-BLOCKS) (CDR FREE))
		     (FLAG T NIL))
		    ((NULL FREE) (SEND STREAM :TYO #/NEWLINE))
		 (FORMAT STREAM "~A #~A=~D" (IF FLAG "Free:" ",") (CAAR FREE) (CDAR FREE))))
	       (T
		(SEND STREAM :TYO #/NEWLINE))))
	((TYPEP STREAM 'INTERVAL-STREAM)
	 (LET ((STRING (CREATE-LINE 'ART-STRING 128. NIL)))
	   (DEFAULT-LIST-ONE-FILE FILE STRING)
	   (SEND STREAM :LINE-OUT STRING)))
	((OR (NULL STREAM) (STRINGP STREAM))
	 (LET ((STRING
		 (OR STREAM (MAKE-ARRAY 128. :TYPE 'ART-STRING :LEADER-LENGTH 1))))
	   (SETF (FILL-POINTER STRING) 0)
	   (ARRAY-INITIALIZE STRING #/SP 0 (ARRAY-LENGTH STRING))
	   (VECTOR-PUSH (IF (GET FILE :DELETED) #/D #/SP) STRING)
	   (VECTOR-PUSH #/SP STRING)
	   (STRING-NCONC STRING (OR (GET FILE :PHYSICAL-VOLUME) ""))
	   (SETF (FILL-POINTER STRING) (1+ (MAX 5 (FILL-POINTER STRING))))
	   (STRING-NCONC STRING (SEND PATHNAME :STRING-FOR-DIRED))
	   (VECTOR-PUSH #/SP STRING)
	   (SETF (FILL-POINTER STRING)
		 (MAX 20. (FILL-POINTER STRING)))
	   (LET ((LINK-TO (GET FILE :LINK-TO)))
	     (IF LINK-TO
		 (PROGN (STRING-NCONC STRING "=> " LINK-TO " ")
			(SETF (FILL-POINTER STRING)
			      (MAX 40. (FILL-POINTER STRING))))
	       (LET ((LENGTH (GET FILE :LENGTH-IN-BLOCKS)))
		 (SETF (FILL-POINTER STRING)
		       (MAX 23. (FILL-POINTER STRING)))
		 (COND ((NULL LENGTH)
			(STRING-NCONC STRING "     "))
		       ((> LENGTH 999.)
			(SETF (FILL-POINTER STRING)
			      (NUMBER-INTO-ARRAY STRING LENGTH 10.
						 (FILL-POINTER STRING) 4))
			(VECTOR-PUSH #/SP STRING))
		       (T
			(SETF (FILL-POINTER STRING)
			      (MAX 24. (FILL-POINTER STRING)))
			(SETF (FILL-POINTER STRING)
			      (NUMBER-INTO-ARRAY STRING LENGTH 10.
						 (FILL-POINTER STRING) 3))
			(VECTOR-PUSH #/SP STRING))))
	       (LET ((LENGTH (GET FILE :LENGTH-IN-BYTES)))
		 (IF (GET FILE :DIRECTORY)
		     (STRING-NCONC STRING "  DIRECTORY")
		   (WHEN LENGTH
		     (SETF (FILL-POINTER STRING)
			   (NUMBER-INTO-ARRAY STRING LENGTH 10.
					      (FILL-POINTER STRING) 6))
		     (VECTOR-PUSH #/( STRING)
		     (SETF (FILL-POINTER STRING)
			   (NUMBER-INTO-ARRAY STRING (GET FILE :BYTE-SIZE) 10.
					      (FILL-POINTER STRING)))
		     (VECTOR-PUSH #/) STRING))))
	       (SETF (FILL-POINTER STRING)
		     (MAX 39. (FILL-POINTER STRING)))
	       (VECTOR-PUSH (COND ((GET FILE :OFFLINE) #/O)
				  ((GET FILE :NOT-BACKED-UP) #/!)
				  (T #/SP))
			    STRING)))
	   (VECTOR-PUSH (IF (GET FILE :DONT-DELETE) #/@ #/SP) STRING)
	   (VECTOR-PUSH (IF (GET FILE :DONT-SUPERSEDE) #/# #/SP) STRING)
	   (VECTOR-PUSH (IF (GET FILE :DONT-REAP) #/$ #/SP) STRING)
	   (TIME-INTO-ARRAY STRING (GET FILE :CREATION-DATE))
	   (LET* ((DATE-LAST-EXPUNGE (GET FILE :DATE-LAST-EXPUNGE))
		  (REFERENCE-DATE (OR DATE-LAST-EXPUNGE (GET FILE :REFERENCE-DATE))))
	     (WHEN REFERENCE-DATE
	       (STRING-NCONC STRING (IF DATE-LAST-EXPUNGE " X=" " ("))
	       (TIME-INTO-ARRAY STRING REFERENCE-DATE NIL)
	       (OR DATE-LAST-EXPUNGE (STRING-NCONC STRING ")"))))
	   (LET ((AUTHOR (GET FILE :AUTHOR)))
	     (WHEN (AND AUTHOR (NOT (EQUAL AUTHOR (SEND PATHNAME :DIRECTORY))))
	       (SETF (FILL-POINTER STRING)
		     (MAX 74. (FILL-POINTER STRING)))
	       (STRING-NCONC STRING AUTHOR)))
	   (LET ((READER (GET FILE :READER)))
	     (WHEN (AND READER (NOT (EQUAL READER (SEND PATHNAME :DIRECTORY))))
	       (SETF (FILL-POINTER STRING)
		     (MAX 84. (FILL-POINTER STRING)))
	       (STRING-NCONC STRING READER)))
	   STRING))
	(T (FORMAT STREAM "~C ~3A "
		   (IF (GET FILE :DELETED) #/D #/SP)
		   (OR (GET FILE :PHYSICAL-VOLUME) ""))
	   (IF (SEND STREAM :OPERATION-HANDLED-P :ITEM)
	       (SEND STREAM :ITEM 'FILE PATHNAME "~A"
			(SEND PATHNAME :STRING-FOR-DIRED))
	       (SEND STREAM :STRING-OUT (SEND PATHNAME :STRING-FOR-DIRED)))
	   (FORMAT STREAM "~20T")
	   (LET ((LINK-TO (GET FILE :LINK-TO)))
	     (IF LINK-TO
		 (FORMAT STREAM "=> ~A ~40T" LINK-TO)
	       (LET ((LENGTH (GET FILE :LENGTH-IN-BLOCKS)))
		 (LET ((*STANDARD-OUTPUT* STREAM))
		   (FORMAT:TAB 23.))
		 (COND ((NULL LENGTH)
			(LET ((*STANDARD-OUTPUT* STREAM))
			  (FORMAT:TAB 28.)))
		       ((> LENGTH 999.)
			(FORMAT STREAM "~4D " LENGTH))
		       (T
			(LET ((*STANDARD-OUTPUT* STREAM))
			  (FORMAT:TAB 24.))
			(FORMAT STREAM "~3D " LENGTH))))
	       (LET ((LENGTH (GET FILE :LENGTH-IN-BYTES)))
		 (IF (GET FILE :DIRECTORY)
		     (PRINC "  DIRECTORY" STREAM)
		   (AND LENGTH
			(FORMAT STREAM "~6D(~D)" LENGTH (GET FILE :BYTE-SIZE)))))
	       (FORMAT STREAM "~39T")
	       (SEND STREAM :TYO
		     (COND ((GET FILE :OFFLINE) #/O)
			   ((GET FILE :NOT-BACKED-UP) #/!)
			   (T #/SP)))))
	   (SEND STREAM :TYO (IF (GET FILE :DONT-DELETE) #/@ #/SP))
	   (SEND STREAM :TYO (IF (GET FILE :DONT-SUPERSEDE) #/# #/SP))
	   (SEND STREAM :TYO (IF (GET FILE :DONT-REAP) #/$ #/SP))
	   (LET ((CREATION-DATE (GET FILE :CREATION-DATE)))
	     (IF CREATION-DATE
		 (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
		     (TIME:DECODE-UNIVERSAL-TIME CREATION-DATE)
		   (FORMAT STREAM "~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D"
			   MONTH DAY (MOD YEAR 100.) HOURS MINUTES SECONDS))
		 (FORMAT STREAM "~17@T")))
	   (LET* ((DATE-LAST-EXPUNGE (GET FILE :DATE-LAST-EXPUNGE))
		  (REFERENCE-DATE (OR DATE-LAST-EXPUNGE (GET FILE :REFERENCE-DATE))))
	     (AND REFERENCE-DATE
		  (MULTIPLE-VALUE-BIND (NIL NIL NIL DAY MONTH YEAR)
		      (TIME:DECODE-UNIVERSAL-TIME REFERENCE-DATE)
		    (PRINC (IF DATE-LAST-EXPUNGE " X=" " (")
			   STREAM)
		    (FORMAT STREAM "~2,'0D//~2,'0D//~2,'0D" MONTH DAY (MOD YEAR 100.))
		    (OR DATE-LAST-EXPUNGE (PRINC ")" STREAM))))) 
	   (LET ((AUTHOR (GET FILE :AUTHOR)))
	     (AND AUTHOR (NOT (EQUAL AUTHOR (SEND PATHNAME :DIRECTORY)))
		  (FORMAT STREAM "~74T~A" AUTHOR)))
	   (LET ((READER (GET FILE :READER)))
	     (AND READER (NOT (EQUAL READER (SEND PATHNAME :DIRECTORY)))
		  (FORMAT STREAM "~84T~A" READER)))
	   (SEND STREAM :TYO #/NEWLINE))))

))

; From file FILES.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN READ-DIRECTORY-NAME (PROMPT PATHNAME &OPTIONAL (WILDP :WILD) &AUX TYPEIN)
  "Read a pathname to pass to FS:DIRECTORY-LIST.
Prompt with PROMPT, a string probably ending in a colon.
PATHNAME gives the defaults for host, device, directory.
WILDP gives the default used for the other components;
 normally :WILD, but could be NIL."
  (SETQ PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME WILDP
		       			      :TYPE WILDP
					      :VERSION WILDP)
	PROMPT (FORMAT NIL "~A (Default is ~A)" PROMPT PATHNAME))
  (LET ((*READING-PATHNAME-DEFAULTS* PATHNAME)
	(*READING-PATHNAME-SPECIAL-TYPE* :WILD)
	(*READING-PATHNAME-SPECIAL-VERSION* :WILD)
	(*READING-PATHNAME-DIRECTION* :READ)
	(*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
    (WITH-STACK-LIST (PROMPT PROMPT '(:RIGHT-FLUSH " (Completion)"))
      (SETQ TYPEIN (STRING-INTERVAL
		     (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB* NIL NIL 
						       PROMPT))))))
  (COND ((EQUAL TYPEIN "")
	 (PUSH-ON-HISTORY PATHNAME *PATHNAME-ARGUMENT-HISTORY*)
	 PATHNAME)
	(T
	 (LET ((PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TYPEIN PATHNAME :WILD :WILD)))
	   (PUSH-ON-HISTORY PATHNAME *PATHNAME-ARGUMENT-HISTORY*)
	   PATHNAME))))

))

; From file FILES.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN READ-UNDEFAULTED-DIRECTORY-STRING (PROMPT PATHNAME &OPTIONAL (WILDP :WILD))
  "Read a string specifying a pathname to pass to FS:DIRECTORY-LIST.
Prompt with PROMPT, a string probably ending in a colon.
PATHNAME gives the defaults for host, device, directory.
WILDP gives the default used for the other components;
 normally :WILD, but could be NIL.
These defaults are used only for completion."
  (SETQ PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME WILDP
					      :TYPE WILDP
					      :VERSION WILDP)
	PROMPT (FORMAT NIL "~A (Default is ~A)" PROMPT PATHNAME))
  (LET ((*READING-PATHNAME-DEFAULTS* PATHNAME)
	(*READING-PATHNAME-SPECIAL-TYPE* :WILD)
	(*READING-PATHNAME-SPECIAL-VERSION* :WILD)
	(*READING-PATHNAME-DIRECTION* :READ)
	(*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
    ;; no longer pushes result on *pathname-argument-history*
    ;;  -- that is now caller's responsibility
    (WITH-STACK-LIST (PROMPT PROMPT '(:RIGHT-FLUSH " (Completion)"))
      (STRING-INTERVAL (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB* NIL NIL 
							 PROMPT))))))

))

; From file EHF.LISP OZ:<L.SYS2> OZ:
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHF  "

(defun cerror (proceedable-flag unused &optional signal-name format-string &rest args)
  "Report a simple correctable error, using FORMAT and ARGS to print the message.
SIGNAL-NAME is a signal name or condition flavor name to be signalled,
or else a condition name to include in the signal, or NIL for none in particular.
 Actually, FORMAT-STRING and ARGS are just passed along to MAKE-CONDITION
 and SIGNAL-NAME controls how they are interpreted there.
PROCEEDABLE-FLAG = :YES means allow proceed-type :NO-ACTION,
 which returns NIL from CERROR.
PROCEEDABLE-FLAG = T means allow proceed-type :NEW-VALUE,
 and the value is returned from CERROR.
Any other non-NIL value for PROCEEDABLE-FLAG is either a proceed-type
 or a list of proceed-types.

For common-lisp compatibility, PROCEEDABLE-FLAG is a string describing the action to be
taken if the error is proceeded, UNUSED is a format string describing the error, and
the other arguments are used as other arguments to FORMAT.
In this case, NIL is always returned."
  (if (stringp proceedable-flag)
      ;; common-lisp cerror
      (with-stack-list* (format-args signal-name format-string args)
	(multiple-cerror 'common-lisp-cerror ()	;for compatabilty with manual.
			 ("~1{~:}" unused format-args)
	  ((apply #'format nil proceedable-flag format-args) nil)))	;returns nil
    (nth-value 1 (signal-condition
		   (apply #'make-condition signal-name format-string args)
		   (case proceedable-flag
		     ((t) '(:new-value))
		     ((nil) nil)
		     (:yes '(:no-action))
		     (t (if (atom proceedable-flag)
			    (list proceedable-flag) proceedable-flag)))))))

))

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
; From file QFCTNS.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN MACRO-FUNCTION (FSPEC &OPTIONAL ENVIRONMENT &AUX DEF)
  "If FSPEC has a function definition which is a macro, return the expander function; else NIL."
  (COND ((AND (SYMBOLP FSPEC) (SETQ DEF (FSYMEVAL-IN-ENVIRONMENT FSPEC ENVIRONMENT NIL)))
	 (IF (SYMBOLP DEF) (MACRO-FUNCTION DEF ENVIRONMENT)
	   (IF (EQ (CAR-SAFE DEF) 'MACRO)
	       (CDR DEF))))
	((FDEFINEDP FSPEC)
	 (SETQ DEF (FDEFINITION FSPEC))
	 (COND ((EQ (CAR-SAFE DEF) 'MACRO)
		(CDR DEF))
	       ((AND (SYMBOLP FSPEC)
		     (CDR (GET FSPEC 'ALTERNATE-MACRO-DEFINITION))))
	       ((SYMBOLP DEF)
		(MACRO-FUNCTION DEF))
	       (T NIL)))
	((SYMBOLP FSPEC)
	 (CDR (GET FSPEC 'ALTERNATE-MACRO-DEFINITION)))
	(T NIL)))

))

; From file QFCTNS.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN SET-MACRO-FUNCTION (FSPEC DEFINITION)
  (FDEFINE (IF (SPECIAL-FORM-P FSPEC)
	       `(:PROPERTY ,FSPEC ALTERNATE-MACRO-DEFINITION)
	       FSPEC)
	   (CONS 'MACRO DEFINITION) NIL))

))

; From file QFCTNS.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFPARAMETER *COMMON-LISP-ONE-TRUE-AND-ONLY-OFFICIAL-SPECIAL-FORMS*
	      '(BLOCK CATCH COMPILER-LET DECLARE EVAL-WHEN FLET FUNCTION GO
	        IF LABELS LET LET* MACROLET MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 PROGN
		PROGV QUOTE RETURN-FROM SETQ TAGBODY THE THROW UNWIND-PROTECT)
  "So decree the Gang of Five")

;;; This can't do the right thing all the time. -- it only checks for &QUOTE and macros
(DEFUN SPECIAL-FORM-P (SYMBOL &OPTIONAL ENVIRONMENT &AUX ARGLIST)
  "T if SYMBOL has a function definition taking unevaluated arguments.
This does not include macros. To test for them, use MACRO-FUNCTION."
  (IF (FSYMEVAL-IN-ENVIRONMENT SYMBOL ENVIRONMENT NIL)
      NIL					;we don't allow (flet ((foo (... &quote ...)
    (OR (MEMQ SYMBOL *COMMON-LISP-ONE-TRUE-AND-ONLY-OFFICIAL-SPECIAL-FORMS*)
	(AND (FBOUNDP SYMBOL)
	     (NEQ (CAR-SAFE (SYMBOL-FUNCTION SYMBOL)) 'MACRO)
	     (CONSP (SETQ ARGLIST (ARGLIST SYMBOL 'COMPILE)))
	     (MEMQ '&QUOTE ARGLIST)
	     T))))

(defun fsymeval-in-environment (symbol environment check-symbol-function &aux mumble)
  "Returns SYMBOL's function or macro definition within ENVIRONMENT,
If CHECK-SYMBOL-FUNCTION is T we take SYMBOL-FUNCTION of SYMBOL if the function is not
defined by ENVIRONMENT, otherwise we return NIL if the environment doesn't define it."
  (dolist (frame (car environment) (if check-symbol-function (symbol-function symbol) nil))
    (and (setq mumble (get-location-or-nil (locf frame) (locf (symbol-function symbol))))
	 (return (car mumble)))))

;;; Note: this is different from (macro-function symbol environment),
;;;  as this doesn't look at alternate-macro-definitions
(defun macro-in-environment-p (symbol environment &aux tem)
  "Returns SYMBOL's macroexpansion function if it is defined as a macro (either
within ENVIRONMENT or gloablly), or NIL if it does not have a macro definition"
  (if (setq tem (fsymeval-in-environment symbol environment nil))
      (if (eq (car-safe tem) 'macro) (cadr tem))
    (and (fboundp symbol)
	 (eq (car-safe (setq tem (symbol-function symbol))) 'macro)
	 tem)))

))


; From file EVAL.LSP USRD$:[MLY.SAVE] OBI:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun multiple-value-call (function &quote &rest forms)
  "Call FUNCTION like FUNCALL, but use all values returned by each of FORMS.
FUNCALL would use only the first value returned by each of them.
This conses, alas."
  (let ((args (mapcan #'(lambda (form)
			  `(:spread ,(multiple-value-list (eval1 form))))
		      forms)))
    (apply #'call function args)))

))

; From file COMTAB.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN KEY-FOR-COMMAND (COMMAND &OPTIONAL (COMTAB *COMTAB*)
			STARTING-CHAR STARTING-COMTAB
			SUGGESTED-CHAR &AUX TEM)
  "Return a string describing the character to invoke COMMAND in COMTAB.
Returns NIL if there is no way.
The second value is the comtab that COMMAND was actually found in;
this is COMTAB or one of the comtabs it indirects to.
STARTING-CHAR and STARTING-COMTAB say where, in the sequence
to be searched for COMTAB, to start looking.  This is so you
can use the character and comtab values to resume the search. 
You can use the SUGGESTED-CHAR to save time
by suggesting the place where the command standardly goes."
 (DECLARE (VALUES STRING CHARACTER COMTAB))
 (OR STARTING-CHAR (SETQ STARTING-CHAR 0))
 (OR STARTING-COMTAB (SETQ STARTING-COMTAB COMTAB))
 (BLOCK FOUND
   (IF SUGGESTED-CHAR
       (MULTIPLE-VALUE-BIND (COMMAND1 COMTAB1)
	   (COMMAND-LOOKUP SUGGESTED-CHAR COMTAB)
	 (IF (EQ COMMAND1 COMMAND)
	     (RETURN-FROM FOUND (FORMAT NIL "~@:C" SUGGESTED-CHAR)
			  	SUGGESTED-CHAR
				COMTAB1))))
   (DO ((CTB STARTING-COMTAB (COMTAB-INDIRECT-TO CTB))
	(STARTING-CHAR STARTING-CHAR 0)
	KEYBOARD-ARRAY LENGTH MOUSE-ARRAY)
       ((NULL CTB))
     (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB)
	   MOUSE-ARRAY (COMTAB-MOUSE-ARRAY CTB))
     (IF (NOT (ARRAYP KEYBOARD-ARRAY))
	 (DOLIST (ELT KEYBOARD-ARRAY)
	   (COND ((< (CAR ELT) STARTING-CHAR))
		 ((AND (EQ (CDR ELT) COMMAND)
		       (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB))
		  (RETURN-FROM FOUND
		    (FORMAT NIL "~@:C" (CAR ELT))
		    (CAR ELT)
		    CTB))
		 ((AND (EQ (CDR ELT) 'COM-DOCUMENTATION)
		       (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB)
		       (SETQ TEM (RASSQ COMMAND *COM-DOCUMENTATION-ALIST*)))
		  (RETURN-FROM FOUND
		    (FORMAT NIL "~:@C ~:@C" (CAR ELT) (CAR TEM))
		    (CAR ELT)
		    CTB))
		 ((AND (TYPEP (CDR ELT) 'CLOSURE)	;Redundant but should speed things up.
		       (PREFIX-COMMAND-P (CDR ELT))
		       (SETQ TEM
			     (KEY-FOR-COMMAND COMMAND
					      (GET-PREFIX-COMMAND-COMTAB (CDR ELT))))
		       (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB))
		  (RETURN-FROM FOUND
		    (FORMAT NIL "~:@C ~A" (CAR ELT) TEM)
		    (CAR ELT)
		    CTB))))
       (SETQ LENGTH (ARRAY-DIMENSION KEYBOARD-ARRAY 0))
       (DO ((BITS (CHAR-BITS STARTING-CHAR) (1+ BITS))
	    (INCREMENT (ARRAY-DIMENSION KEYBOARD-ARRAY 1)))
	   ((= BITS CHAR-BITS-LIMIT))
	 (DO ((CH (IF (= BITS (CHAR-BITS STARTING-CHAR))
		      (CHAR-CODE STARTING-CHAR)
		    0)
		  (+ 1 CH))
	      (OFFSET (IF (= BITS (CHAR-BITS STARTING-CHAR))
			  (* INCREMENT (CHAR-CODE STARTING-CHAR))
			  0)
		      (+ OFFSET INCREMENT))
	      (PTR (ALOC KEYBOARD-ARRAY 0 BITS)))
	     ((= CH LENGTH))
	   (LET ((THIS-COM (%P-CONTENTS-OFFSET PTR OFFSET)))	;Faster than AREF on 2d array!
	     (COND ((AND (EQ THIS-COM COMMAND)
			 (IN-THIS-COMTAB-P COMTAB (MAKE-CHAR CH BITS) CTB))
		    (SETQ CH (MAKE-CHAR CH BITS))
		    (RETURN-FROM FOUND
		      (FORMAT NIL "~@:C" CH)
		      CH
		      CTB))
		   ((AND (EQ THIS-COM 'COM-DOCUMENTATION)
			 (IN-THIS-COMTAB-P COMTAB (MAKE-CHAR CH BITS) CTB)
			 (SETQ TEM (RASSQ COMMAND *COM-DOCUMENTATION-ALIST*)))
		    (SETQ CH (MAKE-CHAR CH BITS))
		    (RETURN-FROM FOUND
		      (FORMAT NIL "~:@C ~:@C" CH (CAR TEM))
		      CH
		      CTB))
		   ((AND (TYPEP THIS-COM 'CLOSURE)	;Redundant but should speed things up.
			 (PREFIX-COMMAND-P THIS-COM)
			 (SETQ TEM
			       (KEY-FOR-COMMAND COMMAND
						(GET-PREFIX-COMMAND-COMTAB THIS-COM)))
			 (IN-THIS-COMTAB-P COMTAB (MAKE-CHAR CH BITS) CTB))
		    (SETQ CH (MAKE-CHAR CH BITS))
		    (RETURN-FROM FOUND
		      (FORMAT NIL "~:@C ~A" CH TEM)
		      CH
		      CTB))))))))))

))

; From file QCOPT.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer multiple-value-call-list multiple-value-call (call multiple-value-list) (form)
  (let ((args (mapcan #'(lambda (form) `(:spread (multiple-value-list ,form))) (cddr form))))
    `(funcall #'call ,(cadr form) . ,args)))

))

; From file QCDEFS.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFVAR *CHECK-STYLE-P* T
  "During pass 1, means to perform style checking on the current outermost form being
compiler. Inner forms will have their style checked regardless of this flag, unless
somebody arranges otherwise.")

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1 (FORM &OPTIONAL DONT-OPTIMIZE &AUX TM)
  (UNLESS DONT-OPTIMIZE
    (SETQ FORM (COMPILER-OPTIMIZE FORM)))
  (COND
    ((ATOM FORM)
     (COND ((AND (CONSTANTP FORM)
		 (OR (NOT (SYMBOLP FORM)) (BOUNDP FORM))
		 (EQ FORM (EVAL FORM)))
	    `',FORM)
	   ((SETQ TM (ASSQ FORM VARS))
	    (AND (EQ (VAR-KIND TM) 'FEF-ARG-FREE)
		 (ZEROP (VAR-USE-COUNT TM))
		 (PUSH (VAR-NAME TM) FREEVARS))
	    (INCF (VAR-USE-COUNT TM))
	    (VAR-LAP-ADDRESS TM))
	   ((TRY-REF-SELF FORM))
	   ((SPECIALP FORM)
	    (MAKESPECIAL FORM) FORM)
	   ((TRY-REF-LEXICAL-VAR FORM))
	   (T (MAKESPECIAL FORM) FORM)))
    ((EQ (CAR FORM) 'QUOTE) FORM)
    ;; Certain constructs must be checked for here
    ;; so we can call P1 recursively without setting TLEVEL to NIL.
    ((NOT (ATOM (CAR FORM)))
     ;; Expand any lambda macros -- just returns old function if none found
     (LET ((FCTN (CAR FORM)))
       (OR (SYMBOLP (CAR FCTN))
	   (WARN 'BAD-FUNCTION-CALLED :IMPOSSIBLE
		 "There appears to be a call to a function whose CAR is ~S."
		 (CAR FCTN)))
       (IF (MEMQ (CAR FCTN) '(LAMBDA NAMED-LAMBDA))
	   (P1LAMBDA FCTN (CDR FORM))
	 ;; Old Maclisp evaluated functions.
	 (WARN 'EXPRESSION-AS-FUNCTION :VERY-OBSOLETE
	       "The expression ~S is used as a function; use FUNCALL."
	       (CAR FORM))
	 (P1 `(FUNCALL . ,FORM)))))
    ((NOT (SYMBOLP (CAR FORM)))
     (WARN 'BAD-FUNCTION-CALLED :IMPOSSIBLE
	   "~S is used as a function to be called." (CAR FORM))
     (P1 `(PROGN . ,(CDR FORM))))
    ((SETQ TM (ASSQ (CAR FORM) *LOCAL-FUNCTIONS*))
     (INCF (VAR-USE-COUNT (CADR TM)))
     `(FUNCALL ,(TRY-REF-LEXICAL-HOME (CADR TM))
	       . ,(P1PROGN-1 (CDR FORM))))
    ((MEMQ (CAR FORM) '(PROG PROG*))
     (P1PROG FORM))
    ((MEMQ (CAR FORM) '(LET LET*))
     (P1LET FORM))
    ((EQ (CAR FORM) 'BLOCK)
     (P1BLOCK FORM))
    ((EQ (CAR FORM) 'TAGBODY)
     (P1TAGBODY FORM))
    ((EQ (CAR FORM) '%POP)			;P2 specially checks for this
      FORM)
    (T (SETQ TLEVEL NIL)
       ;; Check for functions with special P1 handlers.
       (IF (SETQ TM (GET (CAR FORM) 'P1))
	   (FUNCALL TM FORM)
	 (IF (NOT (AND ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
		       (ASSQ (CAR FORM) VARS)
		       (NULL (FUNCTION-P (CAR FORM)))))
	     (P1ARGC FORM (GETARGDESC (CAR FORM)))
	   (WARN 'EXPRESSION-AS-FUNCTION :VERY-OBSOLETE
		 "The variable ~S is used in function position; use FUNCALL."
		 (CAR FORM))
	   (P1 `(FUNCALL . ,FORM)))))))

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS:SYS;QCP1  "

(defun compiler-optimize (form &optional ignore &aux (macro-cons-area
				      (if (eq qc-tf-output-mode 'compile-to-core)
					  background-cons-area
					  default-cons-area))
			  	    (*check-style-p* *check-style-p*))
  (if (already-optimized-p form)
      form
    (do (tm fn local-definition optimizations-begun-flag)
	((atom form))				;Do until no more expansions possible
      (let ((default-cons-area macro-cons-area))
	(setq fn (lambda-macro-expand (car form))))
      (unless (eq fn (car form)) (setq form (cons fn (cdr form))))
      (unless optimizations-begun-flag
	;; Check for too few or too many arguments
	(check-number-of-args form fn))
      (setq local-definition
	    (and (symbolp fn) (fsymeval-in-function-environment fn *function-environment*)))
      (unless (or optimizations-begun-flag
		  local-definition
		  (not *check-style-p*)
		  inhibit-style-warnings-switch)
	;; Do style checking
	(cond ((atom fn)
	       (and (symbolp fn)
		    (setq tm (get fn 'style-checker))
		    (funcall tm form)))
	      ((not run-in-maclisp-switch))
	      ((memq (car fn) '(lambda named-lambda))
	       (lambda-style fn))
	      ((memq (car fn) '(curry-before curry-after))
	       (warn 'not-in-maclisp :maclisp "~S does not work in Maclisp."
		     (car fn)))))
      ;; Optimize args to vanilla functions
      (when (symbolp fn)
	;; don't optimize args to macros of special forms, or to frobs with p1 handlers
	(unless (if local-definition
		    (eq (car-safe local-definition) 'macro)
		  (or (get fn 'p1)
		      (macro-function fn)
		      (special-form-p fn)))
	  (setq form `(,(car form) . ,(mapcar #'compiler-optimize (cdr form))))))
      (or (unless (or local-definition (not (symbolp fn)))
	    (dolist (opt (get fn 'optimizers))
	      (unless (eq form (setq form (funcall opt form)))
		;; Optimizer changed something, don't do macros this pass
		(setq optimizations-begun-flag t)
		(return t))))
	  ;; No optimizer did anything => try expanding macros.
	  (warn-on-errors ('macro-expansion-error "Error expanding macro ~S:" fn)
	    ;; This LET returns T if we expand something.
	    (or (let ((default-cons-area macro-cons-area)
		      (record-macros-expanded t))
		  (multiple-value-setq (form tm)
		    (compiler-macroexpand-1 form))
		  tm)				;non-nil if macroexpansion happened
		;; Stop looping, no expansions apply
		(return)))
	  ;; The body of the WARN-ON-ERRORS either does RETURN or returns T.
	  ;; So if we get here, there was an error inside it.
	  (return (setq form `(error-macro-expanding ',form))))
      ;; Only do style checking the first time around
      (setq *check-style-p* nil))
    ;; Result is FORM
    (flag-already-optimized form)))

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

;;>> this should eventually be different from macroexpand-1, in that it supply a
;;   *macroexpand-hook* which will not do mogus things with things we know about
;;   specially (such as (common-lisp-only) "macros" which we really implement as
;;   special forms, or things with p1 handlers or optimizers)
;;   For now, macroexpand-1 doesn't hack the things with si::alternate-macro-definitions,
;;   so this will do.
;;   Also, we really should pass more than just the function env -- also need declarations,
;;   perhaps.
(defun compiler-macroexpand-1 (form)
  ;; CAR of environment is local macros (and functions)
  (with-stack-list (env *function-environment*)
    (macroexpand-1 form env)))

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1ARGC (FORM DESC)
  (IF (AND (MEMQ 'FEF-ARG-REST (CADAR DESC))
	   (MEMQ 'FEF-QT-QT (CADAR DESC)))
      FORM
    (DO* ((COUNT 0 (1- COUNT))
	  (ARGS-LEFT (COPY-LIST (CDR FORM)) (CDR ARGS-LEFT))
	  (ARG-P1-RESULTS ARGS-LEFT)
	  (FCTN (CAR FORM))
	  (P1VALUE 1)				;function calling uses only first value
	  (DESCS-LEFT DESC)
	  TOKEN-LIST
	  TM)
	 (())
      ;; If all arguments processed, return.
      (COND ((NULL ARGS-LEFT)
	     (RETURN (CONS FCTN ARG-P1-RESULTS)))
	    ((ATOM ARGS-LEFT)
	     (WARN :IMPOSSIBLE 'NON-NIL-END-OF-FORM
		   "The form ~S ends in a non-NIL atomic cdr."
		   FORM)
	     (IF (ATOM ARG-P1-RESULTS)
		 (RETURN (LIST FCTN))
	       (SETF (CDR (LAST ARG-P1-RESULTS)) NIL)
	       (RETURN (CONS FCTN ARG-P1-RESULTS)))))
	
      ;; Figure out what descriptor to use for the next argument.
      ;; TOKEN-LIST is the actual descriptor, and COUNT
      ;; is the number of arguments left for it to apply to.
      (WHEN (ZEROP COUNT)
	(COND ((NULL DESCS-LEFT)
	       ;; Out of descriptors => treat excess args as evalled.
	       (SETQ DESCS-LEFT '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL))))))
	(SETQ COUNT (CAAR DESCS-LEFT))
	(SETQ TOKEN-LIST (CADAR DESCS-LEFT))
	(SETQ DESCS-LEFT (CDR DESCS-LEFT))
	(IF (MEMQ 'FEF-ARG-REST TOKEN-LIST)
	    (SETQ COUNT #o1005)))

      ;; Process the next argument according to its descriptor.
      (COND ((MEMQ 'FEF-QT-QT TOKEN-LIST))
	    ((OR (MEMQ 'FEF-QT-EVAL TOKEN-LIST)
		 (MEMQ 'FEF-QT-DONTCARE TOKEN-LIST))
	     (RPLACA ARGS-LEFT
		     (IF (AND (MEMQ 'FEF-FUNCTIONAL-ARG TOKEN-LIST)
			      (NOT (ATOM (SETQ TM (COMPILER-OPTIMIZE (CAR ARGS-LEFT)))))
			      (EQ (CAR TM) 'QUOTE))	;Look for '(LAMBDA...)
			 (P1FUNCTION TM)
		       (P1 (CAR ARGS-LEFT)))))
	    (T (BARF TOKEN-LIST 'BAD-EVAL-CODE 'BARF))))))

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1LET (FORM &OPTIONAL FOR-AUXVARS)
  (LET ((VARS VARS)
	OUTER-VARS
	(FN (CAR FORM))
	(BINDP)					;%bind not used
	(BODY)
	(VLIST)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*))
    (SETQ VLIST (CADR FORM))
    (SETQ BODY (CDDR FORM))
    (IF (EQ FN 'LET-FOR-AUXVARS) (SETQ FN 'LET*))
    ;; Take all DECLAREs off the body.
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL NIL))
    (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
    (SETQ OUTER-VARS VARS)
    ;; Treat parallel binding as serial if it doesn't matter.
    (WHEN (OR ;; ie if only 1 symbol
	      (NULL (CDR VLIST))	  
	      (AND (EQ FN 'LET)
		   (DOLIST (XX VLIST)
		     ;; or if binding each symbol to NIL, a constant, or itself.
		     (OR (ATOM XX)		;(let (x) ...)
			 (CONSTANTP (CADR XX))	;(let ((x 'foo)) ...)
			 (EQ (CAR XX) (CADR XX));(let ((x x)) ...)
			 (RETURN NIL)))))
      (SETQ FN 'LET*))
    ;; Flush rebinding a var to itself if it isn't special
    ;; and range of rebinding is rest of function.
    (IF TLEVEL
	(SETQ VLIST (SUBSET-NOT #'(LAMBDA (VAR)
				    (AND (CONSP VAR)
					 (EQ (CAR VAR) (CADR VAR))
					 (EQ (FIND-TYPE (CAR VAR) THIS-FRAME-DECLARATIONS)
					     'FEF-LOCAL)
					 (EQ (VAR-TYPE (ASSQ (CAR VAR) VARS)) 'FEF-LOCAL)))
				VLIST)))
    ;; All the local declarations should be in effect for the init forms.
    (SETQ LOCAL-DECLARATIONS (APPEND THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    ;; &AUX vars should be allowed to inherit special declarations
    ;; since that is what it looks like when you put a DECLARE inside the body.
    (IF FOR-AUXVARS
	(SETQ THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    (SETQ VLIST (P1SBIND VLIST
			 (IF TLEVEL 'FEF-ARG-AUX 'FEF-ARG-INTERNAL-AUX)
			 (EQ FN 'LET)
			 NIL
			 THIS-FRAME-DECLARATIONS))
    ;; Now convert initial SETQs to variable initializations.
    ;; We win only for SETQs of variables bound but with no initialization spec'd,
    ;; which set them to constant values, and only if later vars' inits didn't use them.
    ;; When we come to anything other than a SETQ we can win for, we stop.
    ;; For LET*, we can't win for a special variable if anyone has called a function
    ;; to do initting, since that function might have referred to the special.
    ;; Even if we don't use tha ADL to init them,
    ;; we avoid redundant settings to NIL.
    (DO ((P1VALUE 1)				;setq only wants one value
	 TEM HOME)
	(())
      (COND ((EQUAL (CAR BODY) '((SETQ)))
	     (POP BODY))
	    ((OR (ATOM (CAR BODY))
		 (ATOM (SETQ TEM (COMPILER-OPTIMIZE (CAR BODY))))
		 (NOT (EQ (CAR TEM) 'SETQ))
		 (NOT (MEMQ (CADR TEM) VLIST))	;we're binding it
		 (NOT (CONSTANTP (CADDR TEM)))	;initializing to constant
		 (AND (SPECIALP (CADR TEM))
		      (OR TLFUNINIT (NOT TLEVEL))
		      (EQ FN 'LET*))
		 (NOT (ZEROP (VAR-USE-COUNT (SETQ HOME (ASSQ (CADR TEM) VARS))))))
	     (RETURN NIL))
	    (T (SETQ BODY (CONS `(SETQ . ,(CDDDR TEM)) (CDR BODY)))
	       (SETF (CAR (MEMQ (CADR TEM) VLIST))
		     `(,(CADR TEM) ,(P1 (CADDR TEM))))
	       ;; For a variable bound at function entry, really set up its init.
	       ;; Other vars (FEF-ARG-INTERNAL-AUX) will be initted by code,
	       ;; despite our optimization, but it will be better code.
	       (AND TLEVEL (EQ (VAR-KIND HOME) 'FEF-ARG-AUX)
		    (SETF (VAR-INIT HOME) `(FEF-INI-PNTR ,(P1 (CADDR TEM))))))))
    ;; Now P1 process what is left of the body.
    (WHEN (CDR BODY) (SETQ TLEVEL NIL))
    (SETQ BODY (P1PROGN-1 BODY))
    `(,FN ,VLIST ,OUTER-VARS ,VARS ,BINDP
      ,ENTRY-LEXICAL-CLOSURE-COUNT ,*LEXICAL-CLOSURE-COUNT*
      . ,BODY)))

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (:PROPERTY INHIBIT-STYLE-WARNINGS P1) (FORM &REST (FORMS (CDR FORM)))
  (DO ((FORMS-LEFT (SETQ FORMS (COPY-LIST FORMS)) (CDR FORMS-LEFT))
       (*CHECK-STYLE-P* NIL))
      ((NULL FORMS-LEFT) `(PROGN . ,FORMS))
    (SETF (CAR FORMS-LEFT)
	  (P1V (CAR FORMS-LEFT) (IF (CDR FORMS) NIL P1VALUE)))))

))

; From file QCP1.LSP USRD$:[MLY.SAVE] OBI:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN QC-PROCESS-INITIALIZE ()
  (SETQ HOLDPROG T)
  (SETQ MC-HOLDPROG T)
  (SETQ ULAP-DEBUG NIL)
  (SETQ LAP-DEBUG NIL)
  (SETQ FUNCTION-BEING-PROCESSED NIL)	;For error printouts.  Avoid any unbound problems
  (SETQ OPEN-CODE-MAP-SWITCH T)
  (SETQ ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH NIL)
  (SETQ ALL-SPECIAL-SWITCH NIL)
  (SETQ OBSOLETE-FUNCTION-WARNING-SWITCH T)
  (SETQ RUN-IN-MACLISP-SWITCH NIL)
  (SETQ INHIBIT-STYLE-WARNINGS-SWITCH NIL)
  (SETQ *CHECK-STYLE-P* T))

))
(remprop '// 'compiler::style-checker)
(remprop 'cli:// 'compiler::style-checker)
(remprop '- 'compiler::style-checker)
(remprop 'prog1 'compiler::style-checker)
(remprop 'prog2 'compiler::style-checker)
(remprop '+ 'compiler::style-checker)
(remprop 'plus 'compiler::style-checker)
(remprop '* 'compiler::style-checker)
(remprop 'times 'compiler::style-checker)
(remprop 'quotient 'compiler::style-checker)
(remprop 'difference 'compiler::style-checker)
(remprop 'nconc 'compiler::style-checker)

; From file COMTAB.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFMETHOD (WINDOW :PROCESS-COMMAND-CHAR) (CH)
  (PROCESS-COMMAND-CHAR CH))

))

; From file COMTAB.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFMETHOD (WINDOW :PROCESS-SPECIAL-COMMAND) (&REST ARGS)
  (APPLY #'PROCESS-SPECIAL-COMMAND ARGS))

))

; From file COMTAB.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFMETHOD (WINDOW :EDIT) (&OPTIONAL IGNORE (*COMTAB* *COMTAB*)
			   (*MODE-LINE-LIST* *MODE-LINE-LIST*)
			   &OPTIONAL (TOP-LEVEL-P (SEND SELF :TOP-LEVEL-P))
			   &AUX
			   (PROMPT-ARRAY (MAKE-PROMPT-ARRAY))
			   (TV:KBD-INTERCEPTED-CHARACTERS EDITOR-INTERCEPTED-CHARACTERS))
  (TV:PROCESS-TYPEAHEAD (SEND SELF :IO-BUFFER)
			#'(LAMBDA (CH)
			    (COND ((ATOM CH) CH)
				  ((EQ (CAR CH) 'SELECT-WINDOW)
				   (LEXPR-SEND SELF :PROCESS-SPECIAL-COMMAND CH)
				   NIL)
				  ((MEMQ (CAR CH) '(CONFIGURATION-CHANGED REDISPLAY))
				   NIL)
				  (T CH))))
  (UNWIND-PROTECT
    (PROGN
      (SEND (WINDOW-SHEET *WINDOW*)	;Don't expose yet, but on first redisplay
	    :START-DELAYED-SELECT)
      (SETQ *WINDOW-LIST* (FRAME-EXPOSED-WINDOWS))
      (REDISPLAY-MODE-LINE)		;Do this once since may change size
      ;; Flush any typeout hiding this window.
      (IF (SEND (WINDOW-SHEET *WINDOW*) :EXPOSED-P)
	  (PREPARE-WINDOW-FOR-REDISPLAY *WINDOW*))
      (*CATCH 'RETURN-FROM-COMMAND-LOOP
	(*CATCH (IF (EQ TOP-LEVEL-P T) 'EXIT-TOP-LEVEL 'EXIT-CONTROL-R)
	  (DO-FOREVER
	    (UNLESS
	      (CATCH-ERROR-RESTART ((SYS:ABORT ERROR)
				    (IF TOP-LEVEL-P
					"Return to top level editor command loop."
				      "Return to editor command loop."))
		(*CATCH 'ZWEI-COMMAND-LOOP
		  (*CATCH (IF (EQ TOP-LEVEL-P T) 'TOP-LEVEL 'DUMMY-TAG)
		    (PROG (CH)
			  (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
				*CURRENT-COMMAND-TYPE* NIL
				*NUMERIC-ARG* 1
				*NUMERIC-ARG-P* NIL
				*NUMERIC-ARG-N-DIGITS* 0
				*MARK-STAYS* NIL
				*MINI-BUFFER-COMMAND* NIL)
			  (CLEAR-PROMPTS)
			  (REDISPLAY-ALL-WINDOWS)
			  (SEND *TYPEIN-WINDOW* :COMMAND-LOOP-REDISPLAY)
			  (SETQ *CENTERING-FRACTION* *CENTER-FRACTION*)
		       UNREAL-COMMAND		;arguments loop back here
			  (LET ((*EDITOR-IDLE* TOP-LEVEL-P))
			    (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
			      (SETQ CH (INPUT-WITH-PROMPTS STANDARD-INPUT :ANY-TYI))))
			  (TYPECASE CH
			    (NULL		;If EOF, return
			     (RETURN NIL))
			    (CONS		;Handle mouse, etc
			     (SETQ *LAST-COMMAND-CHAR* CH)
			     (COND ((NOT (LEXPR-SEND SELF :PROCESS-SPECIAL-COMMAND CH))
				    ;; Here to NOT flush typeout!
				    ;; First redisplay any windows not covered by typeout.
				    (REDISPLAY-ALL-WINDOWS NIL NIL)
				    (IF *NUMERIC-ARG-P* (GO UNREAL-COMMAND))
				    ;; If no numeric arg, must flush next char if space.
				    ;; Now wait for a char if there is any typeout
				    ;;  even if it is "complete".
				    ;; Aside from that, we do the same redisplay-related
				    ;; things that real commands do.
				    (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT T)
				    (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
				    (REDISPLAY-ALL-WINDOWS)
				    (GO UNREAL-COMMAND))))
			    ((OR NUMBER CHARACTER)	;Keyboard or mouse character
			     (WHEN (EQ :ARGUMENT (SEND SELF :PROCESS-COMMAND-CHAR CH))
			       (INCF *NUMERIC-ARG-N-DIGITS*)
			       (REDISPLAY-ALL-WINDOWS NIL NIL)
			       ;; If there is typeout, the windows under it
			       ;; have not been redisplayed.
			       ;; Now redisplay will not happen until (at least)
			       ;; the next input char, but Space will not be ignored
			       ;; since it ought to execute with this numeric arg.
			       (GO UNREAL-COMMAND))))
			  ;; If there is typeout (window-typeout-stream style) that the user
			  ;; hasn't finished reading, wait for a character, and unread it
			  ;; unless it is a space.
			  (LET ((*EDITOR-IDLE* TOP-LEVEL-P))
			    (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT))))
		  T))
	      (SEND *STANDARD-INPUT* :SEND-IF-HANDLES :MACRO-ERROR))
	    ;; If we Z from BREAK or an error, and after every command,
	    ;; say it is ok to flush the typeout window.
	    (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
	    (WHEN *MINI-BUFFER-COMMAND*
	      (MINI-BUFFER-RING-PUSH *MINI-BUFFER-COMMAND*))))))
    (SEND (WINDOW-SHEET *WINDOW*) :FLUSH-DELAYED-SELECT)))

))

; From file PATED.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN CREATE-PATCH-BUFFER (PATCH-STRUCTURE NUMBER)
  (SETQ *PATCH-NUMBER* NUMBER)
  (LET* ((FILENAME (SI::PATCH-SYSTEM-PATHNAME (SI:PATCH-NAME PATCH-STRUCTURE) :PATCH-FILE
					      (SI:PATCH-VERSION PATCH-STRUCTURE)
					      *PATCH-NUMBER*
					      :LISP))
	 (*FIND-FILE-NOT-FOUND-IS-AN-ERROR* NIL))
    (SETQ *PATCH-BUFFER*
	  (FIND-FILE FILENAME NIL)))	; The NIL means "don't select this buffer".
  (LET ((EMPTY-P (BP-= (INTERVAL-FIRST-BP *PATCH-BUFFER*)
		       (INTERVAL-LAST-BP *PATCH-BUFFER*))))
    (INITIALIZE-PATCH-BUFFER *PATCH-BUFFER* PATCH-STRUCTURE)
    (SETQ *PATCH-SYSTEM* PATCH-STRUCTURE)
    (UNLESS EMPTY-P
      (MAKE-BUFFER-CURRENT *PATCH-BUFFER*)
      (BARF "~A was supposed to be a new file.  It isn't.  Look at it!"
	    (BUFFER-PATHNAME *PATCH-BUFFER*)))))

))

; From file PATED.LISP OZ:<L.ZWEI> OZ:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN CREATE-OLD-PATCH-BUFFER (PATCH-STRUCTURE NUMBER)
  (SETQ *PATCH-NUMBER* NUMBER)
  (LET* ((FILENAME (SI::PATCH-SYSTEM-PATHNAME (SI:PATCH-NAME PATCH-STRUCTURE) :PATCH-FILE
					      (SI:PATCH-VERSION PATCH-STRUCTURE)
					      *PATCH-NUMBER*
					      :LISP)))
    (SETQ *PATCH-BUFFER*
	  (FIND-FILE FILENAME NIL)))		; The NIL means "don't select this buffer".
  (INITIALIZE-PATCH-BUFFER *PATCH-BUFFER* PATCH-STRUCTURE)
  (SETQ *PATCH-SYSTEM* PATCH-STRUCTURE))

))


; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN INCREMENT-PATCH-SYSTEM-MAJOR-VERSION (NAME STATUS &AUX VERSION PATCH-MAJOR)
  "Increment the major version of the patchable system NAME, and set status to STATUS.
This modifies the patch directory files of the system."
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME T))
  (WHEN (NULL VERSION)
    (FORMAT T "~&No master directory for system ~A, creating one." NAME)
    (SETQ VERSION 0))
  (INCF VERSION)
  (SETQ PATCH-MAJOR (MAKE-PATCH-MAJOR :NAME NAME :VERSION VERSION))
  (WITH-OPEN-FILE (FILE (PATCH-SYSTEM-PATHNAME NAME :SYSTEM-DIRECTORY) :DIRECTION :OUTPUT)
    (FORMAT FILE
	    ";;; -*- Mode:LISP; Package:USER; Base:10; Readtable:T; Patch-File:T -*-~%")
    (WRITE-RESPONSIBILITY-COMMENT FILE)
    (LET ((*PRINT-BASE* 10.))
      (PRINT PATCH-MAJOR FILE)))
  (LET ((FIRST-VERS (MAKE-PATCH-VERSION :NUMBER 0
					:EXPLANATION (FORMAT NIL "~A Loaded" NAME))))
    (WRITE-PATCH-DIRECTORY PATCH-MAJOR (MAKE-PATCH-DIR :STATUS STATUS
						       :VERSION-LIST (NCONS FIRST-VERS))
			   T))
  VERSION)

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN WRITE-PATCH-DIRECTORY (PATCH-SYSTEM PATCH-DIR &OPTIONAL MAJORP)
  "Write out a new patch directory file for PATCH-SYSTEM.
PATCH-DIR is a list described by the defstruct PATCH-DIR,
which is the data to write into the file."
  (LET ((*PRINT-BASE* 10.) (*READ-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE)
	(*NOPOINT T) (*PRINT-RADIX* NIL)
	(*READTABLE* INITIAL-READTABLE))
    (WITH-OPEN-FILE (STREAM (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
						   :VERSION-DIRECTORY
						   (PATCH-VERSION PATCH-SYSTEM))
			       :DIRECTION :OUTPUT)
       (FORMAT STREAM
	       ";;; -*- Mode:LISP; Package:USER; Base:10; Readtable:T; Patch-File:T -*-
;;; Patch directory for ~A version ~D
"
	       (PATCH-NAME PATCH-SYSTEM) (PATCH-VERSION PATCH-SYSTEM))
       (WRITE-RESPONSIBILITY-COMMENT STREAM)
       (WRITE-CHAR #/( STREAM)
       (PRIN1 (PATCH-DIR-STATUS PATCH-DIR) STREAM)
       (SEND STREAM :STRING-OUT "
 (")
       (DOLIST (PATCH (PATCH-DIR-VERSION-LIST PATCH-DIR))
	 (PRIN1 PATCH STREAM)
	 (SEND STREAM :STRING-OUT "
  "))
       (SEND STREAM :STRING-OUT "))")
       (UNLESS MAJORP
	 (SETF (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM) (SEND STREAM :INFO))
	 (SETF (PATCH-PATCH-DIR PATCH-SYSTEM) PATCH-DIR)))))

))


; From file PATHST.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHST  "

(DEFMETHOD (VMS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
						      &REST ARGS)
  (SELECTQ TYP
    (:SYSTEM-DIRECTORY
     (SEND SELF :NEW-PATHNAME :NAME (IF SAME-DIRECTORY-P PATOM
					   (IF (< (STRING-LENGTH NAM) 10.)
					       NAM
					      (SI:SYSTEM-SHORT-NAME NAM)))
			      :TYPE :PATCH-DIRECTORY
			      :VERSION :NEWEST))
    (:VERSION-DIRECTORY
     (SEND SELF :NEW-PATHNAME :NAME (WITH-OUTPUT-TO-STRING (STREAM)
					   (LET ((SNAME (IF SAME-DIRECTORY-P PATOM
							  (SI:SYSTEM-SHORT-NAME NAM))))
					     (DOTIMES (I (MIN (STRING-LENGTH SNAME) 6))
					       (SEND STREAM :TYO (AREF SNAME I))))
					   (LET ((*PRINT-BASE* 10.) (*NOPOINT T))
					     (PRIN1 (\ (CAR ARGS) 1000.) STREAM)))
			      :TYPE :PATCH-DIRECTORY :VERSION :NEWEST))
    (:PATCH-FILE
     (SEND SELF :NEW-PATHNAME :NAME (FORMAT NIL "~:[~*~;~C~]~DP~D"
					      SAME-DIRECTORY-P PATOM
					      (\ (CAR ARGS) 100.)
					      (\ (CADR ARGS)
						 (IF SAME-DIRECTORY-P 100. 1000.)))
		   	      :TYPE (CADDR ARGS) :VERSION :NEWEST))))

))
