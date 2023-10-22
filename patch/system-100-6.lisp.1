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

;; not patched in 99 -- see 99.12
(DEFUN COMMAND-STORE (COMMAND CHAR COMTAB &AUX KEYBOARD-ARRAY)
  "Store COMMAND into COMTAB for character CHAR."
;character lossage
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB))
  (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
	 (LET ((ELEMENT (ASSQ CHAR KEYBOARD-ARRAY)))
	   (IF ELEMENT
	       (SETF (CDR ELEMENT) COMMAND)
	     (PUSH (CONS CHAR COMMAND) (COMTAB-KEYBOARD-ARRAY COMTAB)))))
	((TV:CHAR-MOUSE-P CHAR)
	 (SETF (AREF (COMTAB-MOUSE-ARRAY COMTAB)
		     (MIN (LDB %%KBD-MOUSE-N-CLICKS CHAR) 1)
		     (LDB %%KBD-MOUSE-BUTTON CHAR)
		     (CHAR-BITS CHAR))
	       COMMAND))
	(T
	 (SETF (AREF KEYBOARD-ARRAY
		     (CHAR-CODE CHAR)
		     (CHAR-BITS CHAR))
	       COMMAND))))

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
