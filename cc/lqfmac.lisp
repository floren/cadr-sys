;;; -*- Mode:Lisp; Package:CADR; Base:8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; ***CAUTION!! This file runs only on LISPM.  The MACLISP version is LMCONS;QFMAC***
;;;    macros for QF, CC: version of console program that runs on machine


;SPECIAL VARIABLES FOR ARRAY STUFF

(DECLARE (SPECIAL QF-ARRAY-HEADER QF-ARRAY-DISPLACED-P QF-ARRAY-HAS-LEADER-P 
  QF-ARRAY-NUMBER-DIMS QF-ARRAY-HEADER-ADDRESS QF-ARRAY-DATA-ORIGIN QF-ARRAY-LENGTH))

;FUNCTIONS TO EXAMINE AND DEPOSIT FIELDS OF A Q

;BUILD A Q, GIVEN THE CONTENTS OF ITS FIELDS.
;THE CDR-CODE DEFAULTS TO CDR-ERROR.
(DEFMACRO QF-MAKE-Q (POINTER DATA-TYPE &OPTIONAL CDR-CODE)
     (COND (CDR-CODE
	    `(QF-SMASH-CDR-CODE (QF-SMASH-DATA-TYPE ,POINTER ,DATA-TYPE) ,CDR-CODE))
	   (T `(QF-SMASH-DATA-TYPE ,POINTER ,DATA-TYPE))))

;; Many of these are set up to values that depend on
;; how big the pointer field is in the machine being debugged.
(DEFVAR %%QF-POINTER)
(DEFVAR %%QF-DATA-TYPE)
(DEFVAR %%QF-TYPED-POINTER)
(DEFVAR %%QF-CDR-CODE)
(DEFVAR %QF-POINTER-MASK)
(DEFVAR %%QF-BOXED-SIGN-BIT)
(DEFVAR %%QF-PHT1-VIRTUAL-PAGE-NUMBER)
(DEFVAR %QF-PAGE-NUMBER-MASK)
(DEFVAR %QF-POINTER-SANS-BOXED-SIGN-BIT-MASK)
(DEFVAR %QF-TYPED-POINTER-MASK)

(DEFVAR QF-NIL :UNBOUND
  "Bignum representing NIL on debugged machine.
Must be set up again when cache is cleared.")

(DEFSUBST QF-DATA-TYPE (Q) (LDB %%QF-DATA-TYPE Q))

(DEFSUBST QF-POINTER (Q) (LOGAND %QF-POINTER-MASK Q))	;Can't use LDB, byte too wide

(DEFSUBST QF-MASK-PAGE-NUMBER (Q) (LOGAND %QF-PAGE-NUMBER-MASK Q))

(DEFSUBST QF-POINTER-SANS-BOXED-SIGN-BIT (Q) (LOGAND %QF-POINTER-SANS-BOXED-SIGN-BIT-MASK Q))

(DEFSUBST QF-BOXED-SIGN-BIT (Q) (LDB %%QF-BOXED-SIGN-BIT Q))

(DEFSUBST QF-CDR-CODE (Q) (LDB %%QF-CDR-CODE Q))

(DEFSUBST QF-TYPED-POINTER (Q) (LOGAND %QF-TYPED-POINTER-MASK Q))

;SMASH VAL INTO POINTER AND DATA-TYPE OF Q
(DEFUN QF-SMASH-TYPED-POINTER (Q VAL)
  (+ (QF-TYPED-POINTER VAL)
     (- Q (QF-TYPED-POINTER Q))))

(DEFSUBST QF-SMASH-CDR-CODE (Q VAL) (DPB VAL %%QF-CDR-CODE Q))

(DEFSUBST QF-SMASH-POINTER (Q VAL) (DPB VAL %%QF-POINTER Q))

(DEFSUBST QF-SMASH-DATA-TYPE (Q VAL) (DPB VAL %%QF-DATA-TYPE Q))

;;;; ANALOGUES OF %P-POINTER, %P-STORE-POINTER, ETC.

(DEFMACRO QF-P-POINTER (LOC) `(QF-POINTER (QF-MEM-READ ,LOC)))

(DEFMACRO QF-P-DATA-TYPE (LOC) `(QF-DATA-TYPE (QF-MEM-READ ,LOC)))

(DEFMACRO QF-P-CDR-CODE (LOC) `(QF-CDR-CODE (QF-MEM-READ ,LOC)))

(DEFMACRO QF-P-CONTENTS (LOC) `(QF-TYPED-POINTER (QF-MEM-READ ,LOC)))

(DEFMACRO QF-P-STORE-POINTER (LOC VAL)
     `(LET ((ADDR* ,LOC))
	   (QF-MEM-WRITE (QF-SMASH-POINTER (QF-MEM-READ ADDR*)
					   ,VAL)
			 ADDR*)))

(DEFMACRO QF-P-STORE-CONTENTS (LOC VAL)
     `(LET ((ADDR* ,LOC))
	   (QF-MEM-WRITE (QF-SMASH-TYPED-POINTER (QF-MEM-READ ADDR*)
						 ,VAL)
			 ADDR*)))

(DEFMACRO QF-P-STORE-DATA-TYPE (LOC VAL)
     `(LET ((ADDR* ,LOC))
	   (QF-MEM-WRITE (QF-SMASH-DATA-TYPE (QF-MEM-READ ADDR*)
					     ,VAL)
			 ADDR*)))

(DEFMACRO QF-P-STORE-CDR-CODE (LOC VAL)
     `(LET ((ADDR* ,LOC))
	   (QF-MEM-WRITE (QF-SMASH-CDR-CODE (QF-MEM-READ ADDR*)
					    ,VAL)
			 ADDR*)))

(DEFMACRO QF-TRANSPORT-HEADER (HEADER-ADDRESS)
  `(DO-FOREVER
     (LET ((CONTENTS (QF-MEM-READ ,HEADER-ADDRESS)))
       (UNLESS (OR (= (QF-DATA-TYPE CONTENTS) DTP-HEADER-FORWARD)
		   (= (QF-DATA-TYPE CONTENTS) DTP-GC-FORWARD))
	 (RETURN NIL))
       (SETQ ,HEADER-ADDRESS CONTENTS))))

(DEFSUBST QF-NULL (X) (= X QF-NIL))

(DEFMACRO SELECTN (ITEM . BODY)
   `((LAMBDA (*SELECTN-ITEM*)
	(COND . ,(MAPCAR
		  '(LAMBDA (CLAUSE)
		       (COND ((EQ (CAR CLAUSE) 'OTHERWISE)
			      `(T . ,(CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((= *SELECTN-ITEM* ,(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T `((OR . ,(MAPCAR '(LAMBDA (ITEM) `(= *SELECTN-ITEM* ,ITEM))
						 (CAR CLAUSE))) . ,(CDR CLAUSE)))))
			 BODY)))
     ,ITEM))


;Really wants to be a bignum LSH.  On LISPM, LSH doesnt win for bignums, ASH does.
; In MACLISP, LSH wins sufficiently.
(DEFSUBST CC-SHIFT (QUAN AMT)
  (ASH QUAN AMT))

(DEFSUBST LOGLDB-FROM-FIXNUM (PPSS WORD) (LDB PPSS WORD))

(DEFSUBST LOGDPB-INTO-FIXNUM (BYTE PPSS WORD) (DPB BYTE PPSS WORD))

(DEFSUBST LOGDPB (BYTE PPSS WORD) (DPB BYTE PPSS WORD))

(DEFMACRO 32-LOGAND (X Y) `(LOGAND ,X ,Y))

(DEFUN MASK-FIELD-FROM-FIXNUM (PPSS WORD)
   (LOGAND WORD (DPB -1 PPSS 0)))

(DEFUN LOGLDB (PPSS WORD) (COND ((>= (LOGAND PPSS 77) 30)
                                 (DPB (LDB (+ PPSS 2700 -27) WORD)
                                      2727
                                      (LDB (+ (LOGAND PPSS 7700) 27) WORD)))
                                (T (LDB PPSS WORD))))
