;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.6
;;; Reason:
;;;  Fix character lossage in ZWEI.
;;;  Increase Chaosnet routing table size to 256. so that we can handle all subnets.
;;; Written 25-Apr-23 09:59:37 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.5, microcode 323.



; From file OZ: /home/ams/l/sys/zwei/comtab.lisp at 25-Apr-23 09:59:37
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//zwei//comtab"

(DEFUN COMMAND-LOOKUP (CHAR COMTAB &OPTIONAL NO-ALIASES NO-INDIRECTION)
  "Return the command in COMTAB has for character CHAR.
NO-ALIASES means do not follow aliases (such as #\c-h-A => #\c-sh-A)
 Instead, return a list of the CHAR-BITS and the CHAR-CODE of the character
 for which it is an alias.
NO-INDIRECTION means only for a command associated with CHAR in COMTAB itself;
 ie the COMTAB-INDIRECT-TO of COMTAB is not followed.
The second value is the comtab the command was found in.
This will be COMTAB or a comtab that COMTAB indirects to."
  (DECLARE (VALUES COMMAND COMTAB))
;character lossage
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (DO ((CTB COMTAB (COMTAB-INDIRECT-TO CTB))
       (CH CHAR) KEYBOARD-ARRAY COMMAND)
      ((NULL CTB) NIL)
    (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB)
	  COMMAND (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
			 (CDR (ASSQ CH KEYBOARD-ARRAY)))
			((TV:CHAR-MOUSE-P CH)
			 (IF (ARRAYP (COMTAB-MOUSE-ARRAY CTB))
			     (AREF (COMTAB-MOUSE-ARRAY CTB)
				   (MIN (LDB %%KBD-MOUSE-N-CLICKS CH) 1)
				   (LDB %%KBD-MOUSE-BUTTON CH)
				   (CHAR-BITS CH))))
			(T
			 (AREF KEYBOARD-ARRAY (CHAR-CODE CH) (CHAR-BITS CH)))))
    (COND ((AND (CONSP COMMAND) (NOT NO-ALIASES))
	   (RETURN (COMMAND-LOOKUP (MAKE-CHAR (CADR COMMAND) (CAR COMMAND))
				   CTB NO-ALIASES NO-INDIRECTION)))
	  (COMMAND (RETURN (VALUES COMMAND CTB)))
	  (NO-INDIRECTION (RETURN NIL)))))
))

; From file OZ: /home/ams/l/sys/network/chaos/chsncp.lisp at 25-Apr-23 10:00:55
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//network//chaos//chsncp"

(DEFCONST ROUTING-TABLE-SIZE 256. "the number of subnets in the routing table")

(without-interrupts
  (unless (and (variable-boundp routing-table-size)
	       (= (length routing-table) routing-table-size))
    (setq routing-table
	  (MAKE-ARRAY ROUTING-TABLE-SIZE :TYPE 'ART-16B :AREA PERMANENT-STORAGE-AREA)))
  (unless (and (variable-boundp routing-table-cost)
	       (= (length routing-table-cost) routing-table-size))
    (setq routing-table-cost
	  (MAKE-ARRAY ROUTING-TABLE-SIZE :TYPE 'ART-16B :AREA PERMANENT-STORAGE-AREA)))
  (unless (and (variable-boundp routing-table-type)
	       (= (length routing-table-type) routing-table-size))
    (setq routing-table-type
	  (MAKE-ARRAY routing-table-size :TYPE ART-Q :AREA PERMANENT-STORAGE-AREA)))
  (reset-routing-table))

))
