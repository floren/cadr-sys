;;; -*- Mode: Lisp; Package: CADR; Base: 8.; Patch-File: T -*-
;;; Written 9/06/84 14:30:37 by RMS,
;;; Reason: Fix writing of ucode symbol files.
;;; while running on Lisp Machine Twenty-four from band 7
;;; with System 98.70, CADR 3.8, ZMail 53.18, MIT-Specific 22.4, microcode 309, ZM MIT.



; From file CDMP.LISP OZ:<L.SYS> OZ:
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CDMP  "

(DEFUN CONS-LAP-DUMP-SYMTAB-ELEMENT (SYM)
  (PROG (VAL DMP-TYPE TEM
;; not needed after building 99
	 (*PRINT-GENSYM* NIL))	;Somehow DMP-TYPE can be an uninterned symbol named NUMBER.
	(SETQ VAL (GET SYM 'CONS-LAP-USER-SYMBOL))
    L	(COND ((NULL VAL) (RETURN NIL))
	      ((NUMBERP VAL)
		(SETQ DMP-TYPE 'NUMBER))
	      ((ATOM VAL)
		(SETQ VAL (CONS-LAP-SYMEVAL VAL))
		(GO L))
             ((AND (SETQ TEM (ASSQ (CAR VAL) 
			'( (I-MEM JUMP-ADDRESS-MULTIPLIER)
                           (D-MEM DISPATCH-ADDRESS-MULTIPLIER)
                           (A-MEM A-SOURCE-MULTIPLIER)
                           (M-MEM M-SOURCE-MULTIPLIER))))
                   (EQ (CAADR VAL) 'FIELD)
                   (EQ (CADADR VAL) (CADR TEM)))
              (SETQ DMP-TYPE (CAR VAL) VAL (CADDR (CADR VAL))))
	     (T (RETURN NIL)))
	(PRIN1 SYM SPECIAL-OUT-FILE)
	(PRINC " "  SPECIAL-OUT-FILE)
        (PRIN1 DMP-TYPE SPECIAL-OUT-FILE)
        (PRINC " "  SPECIAL-OUT-FILE)
	(PRIN1 VAL SPECIAL-OUT-FILE)
	(PRINC " "  SPECIAL-OUT-FILE)
	(TERPRI SPECIAL-OUT-FILE)
	(RETURN T)))

))
