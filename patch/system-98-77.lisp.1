;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Patch-File:T; Base:8; Readtable:T -*-
;;; Patch file for System version 98.77 (from 99.9)
;;; Reason: Zmacs c-x j doesn't cause cerebral haemorrhage when jumping to another buffer

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