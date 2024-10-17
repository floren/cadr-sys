;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 70.1
;;; Reason:
;;;  Add defensiveness to PRINT-ADDRESS-HEADER.
;;; Written 15-Jan-87 01:02:20 by RG at site LMI Cambridge
;;; while running on Moe from band 2
;;; with Experimental System 121.5, Experimental Lambda-Diag 15.0, Experimental ZMail 70.0, Experimental KERMIT 32.0, Experimental Unix-Interface 10.0, Experimental Local-File 72.0, Experimental FILE-Server 21.0, Experimental Tape 13.0, Experimental DOE-Macsyma 22.0, Experimental FORTRAN 9.0, Experimental Site Data Editor 4.0, microcode 1730, SDU Boot Tape 3.14, SDU ROM 103.



; From file DJ: L.ZMAIL; MAIL.LISP#322 at 15-Jan-87 01:02:21
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFUN PRINT-ADDRESS-HEADER (STREAM LIST NAME &OPTIONAL (MAX-COL 72.) &AUX PADLEN)
  (SETQ PADLEN (+ (STRING-LENGTH NAME) 2))	;Account for ": "
  (SEND STREAM :STRING-OUT NAME)
  (SEND STREAM :STRING-OUT ": ")
  (cond ((listp list)
	 (DO ((L LIST (CDR L))
	      (X PADLEN (+ X LEN))
	      (LEN) (STR)
	      (EOL-P T NIL))
	     ((NULL L))
	   (SETQ STR (STRING-FROM-HEADER (CAR L) *SEND-HEADER-FORMAT*)
		 LEN (STRING-LENGTH STR))
	   (COND ((NOT EOL-P)
		  (SEND STREAM :TYO #/,)
		  (COND ((OR *QUOTE-HOSTS-FOR-XMAILR* (> (+ X LEN 2) MAX-COL))
			 (SEND STREAM :TYO #/CR)
			 (DOTIMES (I PADLEN)
			   (SEND STREAM :TYO #/SP))
			 (SETQ X PADLEN
			       EOL-P T)))
		  (COND ((NOT EOL-P)
			 (SEND STREAM :TYO #/SP)
			 (INCF X 2)))))
	   (SEND STREAM :STRING-OUT STR)))
	(t (format stream "Bad List: ~S" list)))	;defensive, in case things screwwed.
  (SEND STREAM :TYO #/CR))

))
