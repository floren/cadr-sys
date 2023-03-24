;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for MagTape version 14.2
;;; Reason: Fix bug in reporting end of tape.
;;; Written 3/29/83 01:21:27 by LMFILE,
;;; while running on Lisp Machine Filecomputer from band 2
;;; with MIT-Specific 18.1, System 93.36, ZMail 49.15, Local-File 43.2, MagTape 14.1, FILE-Server 6.2, microcode 226, FC.



; From file MTDEFS.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTDEFS  "

(DEFUN MT-RUN (RQB COMMAND &OPTIONAL MINUS-BYTE-COUNT (UNIT 0) (DENSITY 0) IBM-MODE)
  (PROG ((RETRIES MT-RETRY-COUNT))
   AGAIN
    	(SETF (AREF RQB %MT-RQ-COMMAND) 0)
	(SETF (MT-COMMAND-UNIT) UNIT)
	(SETF (MT-COMMAND-DENSITY) DENSITY)
	(SETF (MT-COMMAND) COMMAND)
	(IF (NULL MINUS-BYTE-COUNT)
	    (SETQ MINUS-BYTE-COUNT
		  (MINUS (ARRAY-LENGTH (RQB-8-BIT-BUFFER RQB)))))
	(AS-1 MINUS-BYTE-COUNT RQB %MT-BYTE-COUNT)
	(AS-1 (IF IBM-MODE 1_10. 0)
	      RQB %MT-READ)
	(EXECUTE-MT-RQB RQB (= COMMAND %MT-COMMAND-READ))
	(COND ((MT-STATUS-EOT)
	       (CERROR ':NO-ACTION NIL 'END-OF-TAPE
		       "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
		       UNIT (NTH COMMAND MT-COMMAND-NAMES)
		       (- MINUS-BYTE-COUNT) DENSITY IBM-MODE RQB)))
	(COND ((NOT (MT-STATUS-ERROR))
	       (RETURN T))
	      (T (FORMAT T "~%MAGTAPE ERROR!")
		 (PRINT-MT-RQB RQB)
		 (MT-RUN-SIMPLE %MT-COMMAND-SPACE-REV UNIT 1)
		 (AND (= COMMAND %MT-COMMAND-WRITE)
		      ( RETRIES MT-ATTEMPT-TO-WRITE-WITH-EXTENDED-GAP-COUNT)
		      (SETQ COMMAND %MT-COMMAND-WRITE-WITH-EXTENDED-GAP))
		 (IF (>= (SETQ RETRIES (1- RETRIES)) 0)
		     (GO AGAIN)
		   (CATCH-ERROR-RESTART-EXPLICIT-IF T (MT-ERROR :RETRY "Retry magtape operation.")
		     (FERROR 'MT-ERROR "MagTape operation failed."))
		   (GO AGAIN))))))

))
