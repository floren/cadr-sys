;;; -*- Mode:Lisp; Readtable:T; Package:CADR; Base:8.; Patch-File:T -*-
;;; Patch file for CADR version 4.2
;;; Reason:
;;;  Load MEMD ULOAD.
;;;  Also arrange for code to load it if needed and not loaded.
;;; Written 1/03/85 14:09:26 by RAMESH,
;;; while running on Lisp Machine Sixteen from band 7
;;; with Experimental System 99.11, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, Experimental Local-File 51.0, microcode 320, GC@2.



; From file DMON.LISP KANSAS:<L.CC> OZ: (57)
#8R CADR#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CADR")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CC; DMON  "

(DEFUN CC-RUN-MTEST (&OPTIONAL ALREADY-LOADED RANGE (MAP-OFFSET 0) AUTO-P (RESET-P T)
		     &AUX PC SYMBOLIC-PC CHAR 
		     ERRORS OTHER-ERRORS ADRAND ADRIOR DATAAND DATAIOR)
  (CC-DISCOVER-MAIN-MEMORY-SIZE)
  (dotimes (i (TRUNCATE cc-main-memory-size 200000))
    (format t "~% Memory board ~D" i)
    (cc-fast-address-test-mem i))
  (COND ((NULL ALREADY-LOADED)
	 (CC-ZERO-ENTIRE-MACHINE RESET-P)
	 ;; Read in MEMD ULOAD if have not already done so.
	 (IF (ZEROP (LENGTH MTEST-UCODE))
	     (MTEST-UCODE-STREAM :RESET))
	 (SETQ UCODE-COUNTER 0)
	 (CC-UCODE-LOADER NIL "SYS: UBIN; MEMD ULOAD >" NIL 'mtest-ucode-stream)))
  (COND ((ZEROP MAP-OFFSET) (CC-FAST-LOAD-STRAIGHT-MAP))
	(T (CC-LOAD-STRAIGHT-MAP MAP-OFFSET)))
  (COND ((NULL RANGE)
	 (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-START 0)
	 (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-SIZE CC-MAIN-MEMORY-SIZE))
	(T (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-START (CAR RANGE))
	   (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-SIZE (CADR RANGE))))
  (LET ((CC-MODE-REG (+ 44 (LOGAND CC-MODE-REG 3))))  ;SAME SPEED, DISABLE PROM, ENABLE
    (IF RESET-P (CC-RESET-MACH))				   ; ERROR STOPS.
    (DO TEST 0 (1+ TEST) (= TEST 10)
	(SETQ ERRORS 0 OTHER-ERRORS 0
	      ADRAND 77777777 ADRIOR 0 DATAAND 37777777777 DATAIOR 0)
	(SETQ CC-UPDATE-DISPLAY-FLAG T)
	(CC-SYMBOLIC-DEPOSIT-REGISTER 'M-TEST TEST)
	(CC-REGISTER-DEPOSIT RASA (CC-SYMBOLIC-CMEM-ADR  'MEMORY-DATA-TEST))
			 CONT (CC-REGISTER-DEPOSIT RAGO 0)
			 L  (COND ((SETQ CHAR (KBD-TYI-NO-HANG))
				   (GO X1))
				  ((ZEROP (CC-REGISTER-EXAMINE RAGO)) (GO X)))
			 (PROCESS-SLEEP 30. "MTest Wait")         ;WHY WAIT AS LONG?
			 (GO L)      
			 X1 (COND ((= CHAR #/ )
				   (FORMAT T "~%Aborting test ~D" TEST)
				   (CC-REGISTER-DEPOSIT RASTOP 0)
				   (GO E)))
			 X  (CC-REGISTER-DEPOSIT RASTOP 0)
			 (COND ((NOT (OR (= (SETQ PC (CC-REGISTER-EXAMINE RAPC))
					    (CC-SYMBOLIC-CMEM-ADR 'MEMORY-TEST-OK))
					 (> ERRORS 100.)))   ;give up after 100. errors.
				(SETQ SYMBOLIC-PC (CC-FIND-CLOSEST-SYM (+ RACMO PC)))
				(IF (NULL AUTO-P)
				    (FORMAT T "~%Test ~D halted at ~S (= ~O)
 " TEST SYMBOLIC-PC PC)
				    (SETQ ERRORS (1+ ERRORS))
				    (LET* ((CORRECT-DATA
					     (CC-SYMBOLIC-EXAMINE-REGISTER
					       'A-CURRENT-MEMORY-DATA))
					   (WRONG-BITS (LOGXOR CC-SAVED-MD CORRECT-DATA)))
				      (IF (NOT (MEMBER SYMBOLIC-PC '( ERROR-WRONG-DATA
								     (ERROR-WRONG-DATA 2)
								     (MEMORY-CHECK 2))))
					  (PROGN (SETQ OTHER-ERRORS (1+ OTHER-ERRORS))
					   (FORMAT T "~%unexpected stop!!")
					   (CC))
					  (SETQ ADRAND (LOGAND ADRAND CC-SAVED-VMA)
						ADRIOR (LOGIOR ADRIOR CC-SAVED-VMA)
						DATAAND (LOGAND WRONG-BITS DATAAND)
						DATAIOR (LOGIOR WRONG-BITS DATAIOR))))
				    (GO CONT)))
			       ((NOT (ZEROP ERRORS))
				(FORMAT T "~%Test ~D, ~D errors, ADRAND ~S,
 ADRIOR ~S, DATAAND ~S, DATAIOR ~S, other errors ~D"
					TEST ERRORS ADRAND ADRIOR DATAAND DATAIOR
					OTHER-ERRORS)
				(FORMAT T "~%DATAIOR bits ")
				(CC-PRINT-BITS DATAIOR))
			       (T (FORMAT T "~%Test ~D OK" TEST)))
			 E  )))

))

; From file DMON.LISP KANSAS:<L.CC> OZ: (57)
#8R CADR#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CADR")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CC; DMON  "

(defvar ucode-dummy (funcall 'mtest-ucode-stream ':reset))

))