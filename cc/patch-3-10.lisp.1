;;; -*- Mode: Lisp; Package: CADR; Base: 8.; Patch-File: T -*-
;;; Written 9/07/84 20:21:38 by JGB,
;;; Reason: UA-DEFINE-SYMS wrong symbol NUMBER.
;;; while running on Lisp Machine Three from band 1
;;; with System 98.70, CADR 3.9, ZMail 53.18, MIT-Specific 22.4, microcode 309, ZM MIT.



; From file USYMLD.LISP PS:<L.SYS2> OZ:
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; USYMLD  "

(DEFUN UA-DEFINE-SYMS (&OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  ;;Cause symbols to exist. Temporarily CONS-LAP-SYM.
  (LET ((SYM-ARRAY (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
    (COND (T 
	 ; (NULL (GET (AREF SYM-ARRAY 0) 'CONS-LAP-SYM)) ;Save time if looks like it's there
	   (DO ((ADR 0 (+ ADR 3))
		(LIM (ARRAY-ACTIVE-LENGTH SYM-ARRAY)))
	       (( ADR LIM))
	     (LET ((SYM (AREF SYM-ARRAY ADR))
		   (TYPE (AREF SYM-ARRAY (1+ ADR)))
		   (VAL (AREF SYM-ARRAY (+ 2 ADR))))
	       (PUTPROP SYM
			(COND ((EQ TYPE 'NUMBER)
			       VAL)
			      (T 
			       (LIST TYPE 
				(CONS 'FIELD 
				  (COND ((EQ TYPE 'I-MEM)
					 (LIST 'JUMP-ADDRESS-MULTIPLIER VAL))
					((EQ TYPE 'A-MEM) 
					 (LIST 'A-SOURCE-MULTIPLIER VAL))
					((EQ TYPE 'M-MEM) 
					 (LIST 'M-SOURCE-MULTIPLIER VAL))
					((EQ TYPE 'D-MEM) 
					 (LIST 'DISPATCH-ADDRESS-MULTIPLIER VAL))
					(T (FERROR NIL 
"~%The symbol ~S has bad type ~S. Its value is ~S" SYM TYPE VAL)) )))))
			'CONS-LAP-SYM)))))))

))
