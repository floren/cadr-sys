;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for System version 100.4
;;; Reason:
;;;  ADVISE-FIND-SLOT: Fix typo.
;;   ARRAY-POP: Fix typo.
;;; Written 11-Apr-23 02:09:41 by AMS,
;;; while running on Lisp Machine One from band 8
;;; with Experimental System 100.3, Experimental Local-File 54.0, Hacks by AMS 1.0, microcode 323, AMS.



; From file OZ: /home/amsl/sys/sys2/advise.lisp at 11-Apr-23 02:09:47
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys2//advise"

(defun advise-find-slot (function-spec class &aux body)
  (uncompile function-spec t)
  (setq body (encapsulation-body (fdefinition function-spec)))
  (nthcdr (ecase class
	    (:before 1)
	    (:after 2)
	    (:around 3))
	  (car body)))
))



; From file OZ: /home/ams/l/sys/sys/qmisc.lisp at 11-Apr-23 05:09:11
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//qmisc"

(DEFUN ARRAY-POP (ARRAY)
  "Returns the last used element of ARRAY, and decrements the fill pointer.
For an ART-Q-LIST array, the cdr codes are updated so that the overlayed list
no longer contains the element removed. Signals an error if ARRAY is empty
/(has fill-pointer 0)
Uses GLOBAL:AREF, so will pop fixnums out of strings."
  (WITHOUT-INTERRUPTS
    (LET* ((INDEX (1- (FILL-POINTER ARRAY)))	;1- because fill-pointer is # active elements
	   (ARRAY-TYPE (AREF (FUNCTION ARRAY-TYPES)
			     (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0)))
	   VAL)
      (WHEN (MINUSP INDEX)
	(FERROR NIL "~S Overpopped" ARRAY))
      (SETQ VAL (GLOBAL:AREF ARRAY INDEX))
      (SETF (FILL-POINTER ARRAY) INDEX)
      (WHEN (AND (EQ ARRAY-TYPE 'ART-Q-LIST)
		 (NOT (ZEROP INDEX)))
	(%P-DPB CDR-NIL %%Q-CDR-CODE (LOCF (AREF ARRAY (1- INDEX)))))
      VAL)))

))
