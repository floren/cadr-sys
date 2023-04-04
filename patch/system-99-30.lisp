;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.34
;;; Reason:
;;;  Fix window label spec parsing.
;;;  String now defaults to NAME when given a list spec with no :string key.
;;;  Fixed obsolete MAKE-LIST form.
;;; Written 18-Jul-86 15:20:16 by MUSE,
;;; while running on Amoeba from band 2
;;; with System 99.27, CADR 4.3, Experimental ZMail 54.4, MIT-Specific 23.0, Experimental Macsyma 6.1, Experimental Local-File 53.0, Experimental FILE-Server 10.0, microcode 320, GC@2.



; From file OZ:KANSAS:<L.WINDOW>BASWIN.LISP.562 at 18-Jul-86 15:20:17
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "


(DEFMETHOD (LABEL-MIXIN :PARSE-LABEL-SPEC) (SPEC LM TM RM BM
					    &OPTIONAL TOP-P
					    &AUX FONT NSPEC LSTRING)
  (COND (SPEC
	 (AND (LISTP SPEC) (MEMQ (CAR SPEC) '(:STRING :FONT :TOP :BOTTOM :VSP :CENTERED))
	      (DO ((LIST SPEC (CDR LIST))
		   (STRING NIL)
		   VSP
		   CENTERED)
		  ((NULL LIST)
		   (SETQ SPEC (LIST NIL NIL NIL NIL FONT STRING VSP CENTERED)))
		(SELECTQ (CAR LIST)
		  (:STRING (SETQ STRING (CADR LIST)
				 LIST (CDR LIST)))
		  (:FONT (SETQ FONT (CADR LIST)
			       LIST (CDR LIST)))
		  (:VSP (SETQ VSP (CADR LIST) LIST (CDR LIST)))
		  (:CENTERED (SETQ CENTERED T))
		  (:TOP (SETQ TOP-P T))
		  (:BOTTOM (SETQ TOP-P NIL))
		  (OTHERWISE (FERROR NIL "~S is not a recognized keyword" (CAR LIST))))))
	 (SETQ FONT (OR (AND (EQ (TYPEP SPEC) 'FONT) (PROG1 SPEC (SETQ SPEC T)))
			(AND (LISTP SPEC) (LABEL-FONT SPEC))
			(SEND  (SHEET-GET-SCREEN SELF) ':FONT-NAME-FOR ':LABEL)))
	 (SETQ FONT (SEND (SHEET-GET-SCREEN SELF) ':PARSE-FONT-NAME FONT))
	 (SETQ LSTRING (COND ((STRINGP SPEC) SPEC)
			     ((AND (LISTP SPEC) (LABEL-STRING SPEC))
			      (LABEL-STRING SPEC))
			     ((LISTP SPEC) NAME)
			     ((NEQ SPEC T) (STRING SPEC))
			     (T NAME)))
	 (AND (LISTP SPEC) (LABEL-TOP SPEC) (SETQ TOP-P (NOT (MINUSP (LABEL-TOP SPEC)))))
	 (MULTIPLE-VALUE (NSPEC LM TM RM BM)
	   (PARSE-LABEL-SPEC-1 SPEC LM TM RM BM
			       (LABEL-HEIGHT SELF LSTRING FONT
					     (AND (LISTP SPEC) (LABEL-VSP SPEC)))
			       TOP-P))
	 (LET ((TEM (- LABEL-DEFSTRUCT-SIZE (LENGTH NSPEC))))
	   (AND (> TEM 0) (RPLACD (LAST NSPEC) (MAKE-LIST TEM))))
	 (SETF (LABEL-FONT NSPEC) FONT)
	 (OR (LABEL-STRING NSPEC)
	     (SETF (LABEL-STRING NSPEC) LSTRING))
	 (SETQ SPEC NSPEC)))
  (VALUES SPEC LM TM RM BM))

))