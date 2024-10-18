;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.23
;;; Reason:
;;;  Fixes screw case in COM-JUST-ONE-SPACE [c-\] where
;;;  (point) is on beginning of a line when command is
;;;  invoked.
;;; Written 20-Oct-88 18:32:51 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.118, Experimental ZWEI 126.22, Experimental ZMail 74.10, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.



; From modified file DJ: L.ZWEI; COMF.LISP#112 at 20-Oct-88 18:32:51
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-JUST-ONE-SPACE "Replace all whitespace around point with arg spaces" ()
  (LET ((SPACES-BEFORE
	  (max 0
	       (- (BP-INDEX (POINT))
			  (BP-INDEX (BACKWARD-OVER *WHITESPACE-CHARS* (POINT)))))))
    (DELETE-AROUND *WHITESPACE-CHARS* (POINT))
    (DOTIMES (I *NUMERIC-ARG*)
      (INSERT (POINT) #/SP))
    (MOVE-BP (POINT) (FORWARD-CHAR (POINT) (MIN SPACES-BEFORE *NUMERIC-ARG*))))
  DIS-TEXT)

))
