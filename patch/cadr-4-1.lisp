;;; -*- Mode:Lisp; Readtable:T; Package:CADR; Base:8; Patch-File:T -*-
;;; Patch file for CADR version 4.1
;;; Reason:
;;;  some ua fixes:
;;;   Probably better to do a (make-system 'ua :compile) than rely on
;;;   this patch to fix everything.
;;; Written 10-Dec-84 02:54:58 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Experimental System 99.13, CADR 4.0, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.



; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN ASSEMBLE-READ-FILE (FILE-TRUENAME USE-FAST-READER RE-READ)
  (COND ((OR RE-READ
	     (NULL (SEND FILE-TRUENAME :GET 'UA-SEXP)))
	 (LET ((CURRENT-ASSEMBLY-DEFMICS NIL)
	       (NAME (INTERN (STRING-UPCASE (SEND FILE-TRUENAME ':NAME)))))
	   (MAKUNBOUND NAME)
	   (COND (USE-FAST-READER
		  (FORMAT T "~%Reading ~A with fast reader" FILE-TRUENAME)
		  (READ-UCODE FILE-TRUENAME))
		 (T
		  (FORMAT T "~%Reading ~A" FILE-TRUENAME)
		  (READFILE FILE-TRUENAME "UA")))
	   (IF (NOT (BOUNDP NAME))
	       (FERROR NIL "~%Reading ~A failed to set the symbol ~S" FILE-TRUENAME NAME))
	   (SEND FILE-TRUENAME :PUTPROP (SYMEVAL NAME) 'UA-SEXP)
	   (SEND FILE-TRUENAME :PUTPROP CURRENT-ASSEMBLY-DEFMICS 'DEFMICS)))
	(T (FORMAT T "~%Already read ~A" FILE-TRUENAME))))

(DEFUN ASSEMBLE (&OPTIONAL FN INIT-STATE DONT-RE-READ &AUX INPUT-FILE INPUT-TRUENAME)
  (let ((*read-base* 8) (*print-base* 8))
    (PKG-BIND "UA"				;Put user typein into our package during assembly
      (COND ((NOT (BOUNDP 'PATHNAME-DEFAULTS))
	     (SETQ PATHNAME-DEFAULTS (FS:MAKE-PATHNAME-DEFAULTS))
	     (FS:SET-DEFAULT-PATHNAME "SYS: UCADR; UCADR LISP >" PATHNAME-DEFAULTS)))
      (COND ((NULL FN)
	     (FORMAT T "~&Enter input file name (default ~A): "
		     (FS:DEFAULT-PATHNAME PATHNAME-DEFAULTS))
	     (SETQ FN (READLINE))))
      (SETQ INPUT-FILE (FS:MERGE-AND-SET-PATHNAME-DEFAULTS FN PATHNAME-DEFAULTS))
      (SETQ CONSLP-INPUT
	    (SETQ CONSLP-OUTPUT (INTERN (STRING-UPCASE (SEND INPUT-FILE ':NAME)))))
      (SETQ INPUT-TRUENAME (SEND INPUT-FILE ':TRUENAME)
	    VERSION-NUMBER (SEND INPUT-TRUENAME ':VERSION))
      (LET ((TIME (TIME))
	    (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
	    (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
	(COND ((AND DONT-RE-READ (BOUNDP CONSLP-INPUT))
	       (FORMAT T "~&Ucode already read in.~%"))
	      ((OR INIT-STATE			;Use regular reader for incremental assembly
		   (NOT (FBOUNDP 'READ-UCODE)))
	       (FORMAT T "Reading ~A~%" INPUT-TRUENAME)
	       (SETQ CURRENT-ASSEMBLY-DEFMICS NIL)
	       (READFILE INPUT-FILE "UA"))
	      (T
	       (FORMAT T "Reading ~A with fast reader~%" INPUT-TRUENAME)
	       (SETQ CURRENT-ASSEMBLY-DEFMICS NIL)
	       (READ-UCODE INPUT-FILE)))
	(SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
	(FORMAT T "~&Read-in time ~D:~2,'0D, ~D disk reads, ~D disk writes~%"
		(TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW)))
      (DOLIST (X CURRENT-ASSEMBLY-DEFMICS)	;process UA-DEFMICs read
	(APPLY #'UA-DO-DEFMIC X))
      (LET ((TIME (TIME))
	    (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
	    (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
	(FORMAT T "~&Begin Assembly~%")
	(CONS-LAP (SYMEVAL CONSLP-INPUT) INIT-STATE)
	(SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
	(COND ((NULL INIT-STATE)		;dont write on incremental assembly
	       (WRITE-VARIOUS-OUTPUTS INPUT-FILE)))
	(FORMAT T "~&Assembly time ~D:~D, ~D disk reads, ~D disk writes~%"
		(TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW))))))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN WRITE-VARIOUS-OUTPUTS-SYSTEM (OUTPUT-GENERIC-PATHNAME)
  (WHEN	(Y-OR-N-P "WRITE-MCR? ")
    (WRITE-MCR-FILE (SEND OUTPUT-GENERIC-PATHNAME
			  :NEW-CANONICAL-TYPE :CADR-MICROCODE) BASE-VERSION-NUMBER)
    (WRITE-TBL-FILE (SEND OUTPUT-GENERIC-PATHNAME
			  :NEW-CANONICAL-TYPE :CADR-MICROCODE-LOCATIONS))
    (WRITE-ERROR-TABLE (SEND OUTPUT-GENERIC-PATHNAME
			     :NEW-CANONICAL-TYPE :CADR-MICROCODE-ERROR-TABLE))))

;obsolete now.  see write-various-outputs-system
(DEFUN WRITE-VARIOUS-OUTPUTS (INPUT-FILE)
  ;; Binary for the main microcode lives on another directory.
  ;; Allow the user to type the name of the translated file explicitly.
  (LET ((INPUT-FILE-1 INPUT-FILE))
    (OR (EQUAL (SEND INPUT-FILE-1 :HOST) (FS:GET-PATHNAME-HOST "SYS"))
	(SETQ INPUT-FILE-1 (SEND (FS:DEFAULT-PATHNAME PATHNAME-DEFAULTS "SYS")
				 :BACK-TRANSLATED-PATHNAME INPUT-FILE-1)))
    (AND (EQUAL (SEND INPUT-FILE-1 :DIRECTORY) '("UCADR"))
	 (SETQ INPUT-FILE (SEND INPUT-FILE-1 :NEW-DIRECTORY '("UBIN")))))
  (SETQ CONSLP-OUTPUT-PATHNAME (SEND INPUT-FILE
				     :NEW-PATHNAME :NAME (STRING CONSLP-OUTPUT)
						   :TYPE :UNSPECIFIC :VERSION :UNSPECIFIC))
  (WHEN	(Y-OR-N-P "WRITE-MCR? ")
    (WRITE-MCR BASE-VERSION-NUMBER)
    (WRITE-TBL-FILE (SEND CONSLP-OUTPUT-PATHNAME :NEW-PATHNAME
			  :CANONICAL-TYPE :cadr-MICROCODE-LOCATIONS
			  :VERSION VERSION-NUMBER))
    (WRITE-ERROR-TABLE (SEND CONSLP-OUTPUT-PATHNAME :NEW-PATHNAME
			     :CANONICAL-TYPE :cadr-MICROCODE-ERROR-TABLE
			     :VERSION VERSION-NUMBER))))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN MICRO-ASSEMBLE-SYSTEM-DO-IT ()
  (LET* ((FILE-TRUENAMES (MAPCAR #'(LAMBDA (X) (SEND X ':TRUENAME))
				 SI::*FILES-TO-MICRO-ASSEMBLE*))
	 (FILE-TRUENAMES-LISTIFIED (MAPCAR #'(LAMBDA (X) (LISTIFY-PATHNAME X))
					   FILE-TRUENAMES))
	 (OUTPUT-PATHSTRING (GETF (SI::SYSTEM-PLIST SI::*SYSTEM-BEING-MADE*)
				  'OUTPUT-PATHSTRING))
	 (OUTPUT-GENERIC-PATHNAME (FS:PARSE-PATHNAME OUTPUT-PATHSTRING))
	 (OUTPUT-MCR-PATHNAME (SEND OUTPUT-GENERIC-PATHNAME :NEW-CANONICAL-TYPE :cadr-MICROCODE))
	 (PROBE (PROBEF OUTPUT-MCR-PATHNAME))
	 (OUTPUT-OLD-TRUENAME (IF PROBE (SEND PROBE :TRUENAME)))
	 (OLD-VERSION-NUMBER (IF PROBE (SEND OUTPUT-OLD-TRUENAME :VERSION) 0)))
    (SETQ VERSION-NUMBER (1+ OLD-VERSION-NUMBER))
    (SETQ OUTPUT-GENERIC-PATHNAME (SEND OUTPUT-GENERIC-PATHNAME :NEW-VERSION VERSION-NUMBER))
    (FORMAT T "~%Output will be version ~D" VERSION-NUMBER)
    (ASSEMBLE-SYSTEM OUTPUT-GENERIC-PATHNAME
		     FILE-TRUENAMES NIL NIL
		     (GETF (SI::SYSTEM-PLIST SI::*SYSTEM-BEING-MADE*) 'FAST-READ-SWITCH))))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN ASSEMBLE-SYSTEM (OUTPUT-GENERIC-PATHNAME
			FILE-TRUENAMES INIT-STATE RE-READ USE-FAST-READER)
  (let ((*read-base* 8) (*print-base* 8))
    (PKG-BIND "UA"			;Put user typein into our package during assembly
      (LET ((TIME (TIME))
	    (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
	    (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
	(DOLIST (FILE-TRUENAME FILE-TRUENAMES)
	  ;; read in S-exp if necessary.  Also save DEFMICSs on property list of TRUENAME.
	  (ASSEMBLE-READ-FILE FILE-TRUENAME USE-FAST-READER RE-READ))
	(SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
	(FORMAT T "~&Read-in time ~D:~D, ~D disk reads, ~D disk writes~%"
		(TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW)))
      (DOLIST (FILE-TRUENAME FILE-TRUENAMES)
	(DOLIST (X (SEND FILE-TRUENAME :GET 'DEFMICS))	;process UA-DEFMICs
	  (APPLY #'UA-DO-DEFMIC X)))
      (LET ((TIME (TIME))
	    (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
	    (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
	(FORMAT T "~&Begin Assembly~%")
	(CONS-LAP-INITIALIZE INIT-STATE)
	(CONS-LAP-SYSTEM FILE-TRUENAMES INIT-STATE)
	(SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
	(COND ((NULL INIT-STATE)		;dont write on incremental assembly
	       (WRITE-VARIOUS-OUTPUTS-SYSTEM OUTPUT-GENERIC-PATHNAME)))
	(FORMAT T "~&Assembly time ~D:~D, ~D disk reads, ~D disk writes~%"
		(TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
		(- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW))))))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN CONS-LAP-PASS1 (WD) 
  (PROG (CURRENT-WORD)
	(SETQ CURRENT-WORD WD)			;FOR DEBUGGING
	(COND (CONS-LAP-INSIDE-COMMENT
	       (WHEN (EQUAL WD '(END-COMMENT))
		 (SETQ CONS-LAP-INSIDE-COMMENT NIL))
	       (RETURN NIL))
	      ((ATOM WD)
	       (SETQ CONS-LAP-LAST-SYM WD)
	       (SETQ CONS-LAP-WDS-SINCE-LAST-SYM 0)
	       (CONS-LAP-DEFSYM 
		 WD 
		 (LIST LOCALITY 
		       (CONS 'FIELD 
			     (COND ((EQ LOCALITY 'I-MEM)
				    (LIST 'JUMP-ADDRESS-MULTIPLIER I-MEM-LOC))
				   ((EQ LOCALITY 'A-MEM) 
				    (LIST 'A-SOURCE-MULTIPLIER A-MEM-LOC))
				   ((EQ LOCALITY 'M-MEM) 
				    (LIST 'M-SOURCE-MULTIPLIER M-MEM-LOC))
				   ((EQ LOCALITY 'D-MEM) 
				    (LIST 'DISPATCH-ADDRESS-MULTIPLIER D-MEM-LOC))
				   (T (CONS-LAP-BARF LOCALITY 
						     'BAD-LOCALITY 
						     'BARF))) )) )
	       (COND ((OR (EQ LOCALITY 'M-MEM)		;automatically MC-LINKAGEify
			  (AND (EQ LOCALITY 'A-MEM)	; accumulator type frobs.
			       (< A-MEM-LOC 40)))
		      (CONS-LAP-MC-LINKAGE-STORE WD))))
	      ((EQ (CAR WD) 'BEGIN-COMMENT)
	       (SETQ CONS-LAP-INSIDE-COMMENT T))
	      ((EQ (CAR WD) 'DEF-DATA-FIELD)
		(DEF-DATA-FIELD (CADR WD) 
				(CONS-LAP-ARG-EVAL (CADDR WD))
				(CONS-LAP-ARG-EVAL (CADDDR WD))))
	      ((EQ (CAR WD) 'DEF-BIT-FIELD-IN-REG)
		(DEF-BIT-FIELD-IN-REG (CADR WD)
				      (CONS-LAP-ARG-EVAL (CADDR WD))
				      (CONS-LAP-ARG-EVAL (CADDDR WD))
				      (CAR (CDDDDR WD))))
	      ((EQ (CAR WD) 'ASSIGN)
		(CONS-LAP-DEFSYM (CADR WD)
				 (CADDR WD)))
	      ((EQ (CAR WD) 'ASSIGN-EVAL)
		(CONS-LAP-DEFSYM (CADR WD)
				 (CONS-LAP-ARG-EVAL (CADDR WD))))
	      ((EQ (CAR WD) 'DEF-NEXT-BIT)
		(DEF-NEXT-FIELD (CADR WD) 1 (CADDR WD)))
	      ((EQ (CAR WD) 'RESET-BIT-POINTER)
		(RESET-BIT-POINTER (CADR WD)))
	      ((EQ (CAR WD) 'DEF-NEXT-FIELD)
		(DEF-NEXT-FIELD (CADR WD) 
				(CONS-LAP-ARG-EVAL (CADDR WD))
				(CADDDR WD)))
	      ((EQ (CAR WD) 'LOCALITY)
		(SETQ LOCALITY (CADR WD))
		(COND ((NOT (MEMQ LOCALITY '(M-MEM A-MEM D-MEM I-MEM)))
			(CONS-LAP-BARF LOCALITY 'BAD-LOCALITY 'BARF))))
	      ((EQ (CAR WD) 'START-DISPATCH)
		(COND ((NOT (EQ LOCALITY 'D-MEM))
			(CONS-LAP-BARF LOCALITY 'BAD-START-DISPATCH 'BARF)))
		(COND (IN-DISPATCH-BLOCK 
			(CONS-LAP-BARF WD 'ALREADY-IN-DISPATCH 'DATA)))
		(SETQ D-MEM-LOC (FIND-D-MEM-SPACE (EXPT 2 (CADR WD))))
		(SETQ IN-DISPATCH-BLOCK T))
	      ((EQ (CAR WD) 'END-DISPATCH)
		(COND ((NULL IN-DISPATCH-BLOCK)
			(CONS-LAP-BARF WD 'NOT-IN-DISPATCH-BLOCK 'DATA)))
		(COND ((> D-MEM-LOC DISPATCH-BLOCK-LIMIT)
			(CONS-LAP-BARF D-MEM-LOC  
				       'DISPATCH-BLOCK-OVERFLOW 
				       'DATA))
		      ((NOT (= D-MEM-LOC DISPATCH-BLOCK-LIMIT))
			(CONS-LAP-BARF (LIST D-MEM-LOC DISPATCH-BLOCK-LIMIT)
			      'DISPATCH-BLOCK-UNDERFLOW 
			      'WARN)))
		(SETQ IN-DISPATCH-BLOCK NIL))
	      ((MEMQ (CAR WD) '(LOC MODULO))
		(CONS-LAP-LOC-MODULO WD))
	      ((EQ (CAR WD) 'REPEAT)
		(CONS-LAP-REPEAT-1 (CONS-LAP-ARG-EVAL (CADR WD))
				   (CDDR WD)))
	      ((MEMQ (CAR WD) '(MISC-INST-ENTRY MC-LINKAGE MC-LINKAGE-VALUE
				MICRO-CODE-ILLEGAL-ENTRY-HERE ERROR-TABLE
				MC-ENTRY-ADR MISC-ENTRY-ADR))
		(GO X))
	      ((EQ (CAR WD) 'COMMENT))
	      ((EQ (CAR WD) 'IF)
	       (COND ((si:EVAL-special-ok (CADR WD))
		      (CONS-LAP-PASS1 (CADDR WD)))
		     (T (MAPC (FUNCTION CONS-LAP-PASS1) (CDDDR WD)))))
	      (T (CONS-LAP-PASS1-WD WD)
		 (GO W1)))
     X	(RETURN NIL)
     W1	(SETQ CONS-LAP-WDS-SINCE-LAST-SYM (1+ CONS-LAP-WDS-SINCE-LAST-SYM))
	(COND ((EQ LOCALITY 'A-MEM)
	       (SETQ A-MEM-LOC (1+ A-MEM-LOC)))
	      ((EQ LOCALITY 'M-MEM)
	       (SETQ M-MEM-LOC (1+ M-MEM-LOC)))
	      ((EQ LOCALITY 'D-MEM)
	       (COND ((NOT IN-DISPATCH-BLOCK)
		      (CONS-LAP-BARF WD 'STORAGE-WD-NOT-IN-DISPATCH-BLOCK 'DATA)))
	       (SETQ D-MEM-LOC (1+ D-MEM-LOC)))
	      ((EQ LOCALITY 'I-MEM)
	       (SETQ I-MEM-LOC (1+ I-MEM-LOC)))
	 (T (CONS-LAP-BARF WD 'STORAGE-WD-IN-BAD-LOCALITY 'DATA)))
	(RETURN NIL)))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN CONS-LAP-PASS2 (WD)
  (PROG (V)
	(COND (CONS-LAP-INSIDE-COMMENT
	       (WHEN (EQUAL WD '(END-COMMENT))
		 (SETQ CONS-LAP-INSIDE-COMMENT NIL))
	       (RETURN NIL))
	      ((ATOM WD)
	       (SETQ CONS-LAP-LAST-SYM WD)
	       (SETQ CONS-LAP-WDS-SINCE-LAST-SYM 0)
	       (COND ((AND DISPATCH-ARM 
			   (EQ LOCALITY 'D-MEM))
		      (SETQ D-MEM-LOC (LDB 1413 (CONS-LAP-ARG-EVAL WD)))
		      (SETQ DISPATCH-ARM NIL))
		     ((NOT (EQUAL 
			    (CONS-LAP-SYMEVAL WD)
			    (LIST LOCALITY 
				  (CONS 'FIELD 
					(COND ((EQ LOCALITY 'I-MEM)
					       (LIST 'JUMP-ADDRESS-MULTIPLIER I-MEM-LOC))
					      ((EQ LOCALITY 'A-MEM) 
					       (LIST 'A-SOURCE-MULTIPLIER A-MEM-LOC))
					      ((EQ LOCALITY 'M-MEM) 
					       (LIST 'M-SOURCE-MULTIPLIER M-MEM-LOC))
					      ((EQ LOCALITY 'D-MEM) 
					       (LIST 'DISPATCH-ADDRESS-MULTIPLIER D-MEM-LOC))
					      (T (CONS-LAP-BARF LOCALITY 
								'BAD-LOCALITY 
								'BARF))) )) ))
		      (CONS-LAP-BARF WD 'DEF-DFRS-ON-PASS2 'BARF))))
	      ((EQ (CAR WD) 'BEGIN-COMMENT)
	       (SETQ CONS-LAP-INSIDE-COMMENT T))
	      ((MEMQ (CAR WD) '(DEF-DATA-FIELD ASSIGN ASSIGN-EVAL DEF-NEXT-BIT 
					       RESET-BIT-POINTER 
					       DEF-NEXT-FIELD END-DISPATCH 
					       DEF-BIT-FIELD-IN-REG)))
	      ((EQ (CAR WD) 'LOCALITY)
	       (SETQ LOCALITY (CADR WD)))
	      ((EQ (CAR WD) 'START-DISPATCH)
	       (SETQ DISPATCH-CONSTANT (COND ((CONS-LAP-ARG-EVAL (CADDR WD)))
					     (T 0)))
	       (SETQ DISPATCH-ARM T))	;SET D-MEM-LOC TO NEXT D-MEM SYMBOL ENCOUNTERED
	      				;ERROR IF STORAGE WORD BEFORE THAT.
	      ((MEMQ (CAR WD) '(LOC MODULO))
	       (CONS-LAP-LOC-MODULO WD))
	      ((EQ (CAR WD) 'REPEAT)
	       (CONS-LAP-REPEAT-2 (CONS-LAP-ARG-EVAL (CADR WD))
				  (CDDR WD)))
	      ((EQ (CAR WD) 'MISC-INST-ENTRY)
	       (LET ((OPCODE (GET (CADR WD) 'QLVAL)))
		 (COND ((NULL OPCODE)
			(CONS-LAP-BARF (CADR WD) 'NO-UCODE-ENTRY-INDEX 'WARN))
		       (T
			 (SETQ CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY
			       (MAX OPCODE CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY))
			 (COND ((NULL CONS-LAP-INIT-STATE)
				(SETF (AREF MICRO-CODE-SYMBOL-IMAGE (- OPCODE 200))
				      I-MEM-LOC))
			       (T (SETQ CURRENT-ASSEMBLY-MICRO-ENTRIES;in incremental assembly
					(CONS (LIST 'MISC-INST-ENTRY (CADR WD) I-MEM-LOC)
					      CURRENT-ASSEMBLY-MICRO-ENTRIES))))))))
	      ((EQ (CAR WD) 'MICRO-CODE-ILLEGAL-ENTRY-HERE)
	       (SETQ MICRO-CODE-SYMBOL-TABLE-FILL-VALUE I-MEM-LOC)
	       (CONS-LAP-WIPE-SYMBOL-VECTOR I-MEM-LOC))
	      ((AND (EQ (CAR WD) 'MC-LINKAGE)
		    (LISTP (CADR WD)))
	       (MAPC (FUNCTION CONS-LAP-MC-LINKAGE-STORE) (CADR WD)))
	      ((EQ (CAR WD) 'ERROR-TABLE)
	       (SETQ CURRENT-ASSEMBLY-TABLE
		     (NCONC CURRENT-ASSEMBLY-TABLE
			    (LIST (CONS (1- I-MEM-LOC) (CDR WD))))))
	      ((EQ (CAR WD) 'COMMENT))
	      ((EQ (CAR WD) 'IF)
	       (COND ((si:eval-special-ok (CADR WD))
		      (CONS-LAP-PASS2 (CADDR WD)))
		     (T (MAPC (FUNCTION CONS-LAP-PASS2) (CDDDR WD)))))
	      (T (GO W1)))
     X	(RETURN NIL)
     W1	(SETQ CONS-LAP-WDS-SINCE-LAST-SYM (1+ CONS-LAP-WDS-SINCE-LAST-SYM))
	(COND (DISPATCH-ARM 
	       (CONS-LAP-BARF WD 'STORAGE-WD-IN-UNLOCATED-DISPATCH-BLOCK 'DATA)))
	(SETQ V (CONS-WORD-EVAL WD))
	(COND ((EQ LOCALITY 'A-MEM)
	       (COND ((>= A-MEM-LOC (ARRAY-ACTIVE-LENGTH A-MEM))
		      (CONS-LAP-BARF A-MEM-LOC 'A-MEM-OVERFLOW 'DATA))
		     ((>= A-MEM-LOC 40)		;The rest is really m-memory.
		      (SETF (AREF A-MEM A-MEM-LOC) V)))
	       (SETQ A-MEM-LOC (1+ A-MEM-LOC)))
	      ((EQ LOCALITY 'M-MEM)
	       (COND ((< M-MEM-LOC 40)
		      (SETF (AREF A-MEM M-MEM-LOC) V))
		     (T (CONS-LAP-BARF M-MEM-LOC 'M-MEM-OVERFLOW 'DATA)))
	       (SETQ M-MEM-LOC (1+ M-MEM-LOC)))
	      ((EQ LOCALITY 'D-MEM)
	       (SETQ V (+ V DISPATCH-CONSTANT))	;CONSTANT FOR ENTIRE BLOCK
	       (SETQ V (+ (LSH (LDB 703 V) 14.)	;RPN BITS FROM JUMP
			  (LDB 1416 V)))		;PC FROM JUMP
	       (SETF (AREF D-MEM D-MEM-LOC) V)
	       (SETQ D-MEM-LOC (1+ D-MEM-LOC)))
	      ((EQ LOCALITY 'I-MEM)
	       (IF ( I-MEM-LOC (ARRAY-ACTIVE-LENGTH I-MEM))
		   (CONS-LAP-BARF I-MEM-LOC 'I-MEM-OVERFLOW 'DATA)
		 (SETF (AREF I-MEM I-MEM-LOC) V))
	       (SETQ I-MEM-LOC (1+ I-MEM-LOC)))
	      (T (CONS-LAP-BARF WD 'STORAGE-WD-IN-BAD-LOCALITY 'DATA)))
	(RETURN NIL)
	))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN CONS-LAP-EVAL (EXP)      ;EXP A SYMBOL "PROGRAM".
				;RETURNS EITHER A NUMBERIC VALUE OR NIL, AND
				;MAY HAVE THE SIDE EFFECT OF MODIFING 
				;INSTRUCTION-CONTEXT AND/OR FIELD-INDICATORS

  (PROG (VAL V V1 V2 TEM)
     L	(COND ((NULL EXP) (GO X))
	      ((NUMBERP EXP)
		(SETQ V EXP)
		(GO C-V))
	      ((ATOM EXP) 
		(SETQ V (CONS-LAP-SYM-RUN EXP))
		(GO C-V))
	      ((MEMQ (CAR EXP) '(A-MEM M-MEM I-MEM D-MEM))
		(GO L2))
	      ((EQ (CAR EXP) 'SOURCE-P) (GO S-P))
	      ((EQ (CAR EXP) 'DESTINATION-P) (GO D-P))
	      ((MEMQ (CAR EXP) '(FORCE-DISPATCH FORCE-JUMP FORCE-ALU FORCE-BYTE 
			FORCE-DISPATCH-OR-BYTE FORCE-ALU-OR-BYTE))
		(CONS-GET-NEW-CONTEXT (CAR EXP))
		(GO L2))
	      ((SETQ TEM (ASSQ (CAR EXP) '( (DISPATCH-INSTRUCTION-P . FORCE-DISPATCH)
		(BYTE-INSTRUCTION-P . FORCE-BYTE) (JUMP-INSTRUCTION-P . FORCE-JUMP)
		(ALU-INSTRUCTION-P . FORCE-ALU))))
		(GO I-P))
	      ((EQ (CAR EXP) 'NOT)
		(GO N1))
	      ((EQ (CAR EXP) 'OR)
		(GO OR-1))
	      ((SETQ V (ASSQ (CAR EXP)
			     '((I-MEM-LOC . I-MEM) (D-MEM-LOC . D-MEM)
			       (A-MEM-LOC . A-MEM) (M-MEM-LOC . M-MEM))))
		(SETQ TEM (CONS-LAP-SYMEVAL (CADR EXP)))
		(OR (EQ (CAR TEM) (CDR V))
		    (CONS-LAP-BARF EXP 'LOSES 'DATA))
		(SETQ V (CADDR (CADR TEM)))
		(GO C-V))
	      ((EQ (CAR EXP) 'FIELD)
		(SETQ TEM (CONS-LAP-SYM-RUN (CADR EXP)))
		(SETQ V (TIMES (CONS-LAP-EVAL (CADDR EXP)) TEM))
		(COND ((SETQ TEM (GET (CADR EXP) 'CONS-LAP-ADDITIVE-CONSTANT))
			(SETQ V (PLUS V TEM))))
		(ADD-FIELD-INDICATORS (CADR EXP))
		(GO C-V))
	      ((EQ (CAR EXP) 'PLUS)
		(SETQ V (CONS-LAP-EVAL (CADR EXP)))
		(DO L (CDDR EXP) (CDR L) (NULL L)
		  (SETQ V (PLUS V (CONS-LAP-EVAL (CAR L)))))
		(GO C-V))
	      ((EQ (CAR EXP) 'DIFFERENCE)
		(SETQ V (DIFFERENCE (CONS-LAP-EVAL (CADR EXP))
				    (CONS-LAP-EVAL (CADDR EXP))))
		(GO C-V))
	      ((EQ (CAR EXP) 'BYTE-FIELD)
		(COND ((MEMQ INSTRUCTION-CONTEXT '(INSTRUCTION FORCE-DISPATCH-OR-BYTE 
							FORCE-ALU-OR-BYTE))
			(CONS-GET-NEW-CONTEXT 'FORCE-BYTE)))
		(SETQ V1 (CONS-LAP-EVAL (CADR EXP)) V2 (CONS-LAP-EVAL (CADDR EXP)))
		(COND ((EQ INSTRUCTION-CONTEXT 'FORCE-BYTE)
		       (AND (> V1 32.) (CONS-LAP-BARF (CADR EXP)
						      'BYTE-SIZE-GREATER-THAN-32
						      'DATA))
		       (AND (ZEROP V1) (SETQ V1 1))	;Byte size 0, doing OA hackery, use 1-1
		       (SETQ V (+ (* 1_5. (1- V1)) V2))) ;1- byte size, MROT not buggered yet
		      ((EQ INSTRUCTION-CONTEXT 'FORCE-DISPATCH)
			(AND (> V1 7) (CONS-LAP-BARF (CADR EXP)
						     'DISPATCH-BYTE-SIZE-GREATER-THAN-7
						     'DATA))
			(SETQ V (+ (* 1_5. V1) V2)))
		      ((EQ INSTRUCTION-CONTEXT 'FORCE-JUMP)
			(COND ((NOT (= 1 V1))
				(CONS-LAP-BARF (CADR EXP) 
						'CAN-ONLY-TEST-ONE-BIT-FIELD-WITH-JUMP 
						 'DATA)))
			(SETQ V V2))
		      (T (CONS-LAP-BARF INSTRUCTION-CONTEXT 
					'BYTE-FIELD-IN-BAD-CONTEXT 
					'DATA)))
		(GO C-V))
	      ((EQ (CAR EXP) 'LISP-BYTE)
		(SETQ V (CONS-LAP-EVAL (CONVERT-LISP-BYTE (CADR EXP))))
		(GO C-V))
	      ((EQ (CAR EXP) 'ALL-BUT-LISP-BYTE)
		(SETQ V (CONS-LAP-EVAL (CONVERT-ALL-BUT-LISP-BYTE (CADR EXP))))
		(GO C-V))
	      ((EQ (CAR EXP) 'BYTE-MASK)
		(SETQ V (CONS-LAP-GET-BYTE-VALUE (CADR EXP) -1))
		(GO C-V))
	      ((EQ (CAR EXP) 'BYTE-VALUE)
		(SETQ V (CONS-LAP-GET-BYTE-VALUE (CADR EXP) (CADDR EXP)))
		(GO C-V))
	      ((EQ (CAR EXP) 'EVAL)
		(SETQ V (si:eval-special-ok (CADR EXP)))
		(GO C-V))
	      ((EQ (CAR EXP) 'I-ARG)
		(SETQ V (DPB (CONS-LAP-EVAL (CADR EXP))
				4012 
				0))
		(GO C-V))
	      ((EQ (CAR EXP) 'OA-HIGH-CONTEXT)
		(SETQ V (LDB 3226 (CONS-WORD-EVAL (CADR EXP))))	;All above 26. bits
		(GO C-V))
	      ((EQ (CAR EXP) 'OA-LOW-CONTEXT)
		;  (SETQ V (LDB 0032 (CONS-WORD-EVAL (CADR EXP)))) ;LOW 26. BITS
		   (SETQ V (LET ((TEM-V (CONS-WORD-EVAL (CADR EXP))))  ;Result of LDB cant be
			     (DPB (LDB 2703 TEM-V) 2703 (LDB 0027 TEM-V)))) ;Bignum for now.
		(GO C-V))
	      ((AND (EQ (CAR EXP) 'MC-LINKAGE)
		    (SYMBOLP (CADR EXP)))
	       (SETQ V (CONS-LAP-EVAL (CONS-LAP-MC-LINKAGE (CADR EXP))))
	       (GO C-V))
	      ((EQ (CAR EXP) 'MC-LINKAGE-VALUE)
	       (SETQ V (CONS-LAP-EVAL (CONS-LAP-MC-LINKAGE-VALUE (CADR EXP) (CADDR EXP))))
	       (GO C-V))
	      ((AND CONS-LAP-INIT-STATE		;incremental assembly
		    (EQ (CAR EXP) 'MC-ENTRY-ADR))
	       (COND ((NOT (= (%DATA-TYPE
				(SETQ TEM (CAR (FUNCTION-CELL-LOCATION (CADR EXP)))))
			      DTP-U-ENTRY))
		(FERROR NIL "mc-entry-adr not DTP-U-ENTRY")))
	       (SETQ V (CONS-LAP-EVAL
			 `(I-MEM (FIELD JUMP-ADDRESS-MULTIPLIER
					,(AREF (SYMBOL-FUNCTION SYS:MICRO-CODE-SYMBOL-AREA)
					       (AREF (SYMBOL-FUNCTION SYS:MICRO-CODE-ENTRY-AREA)
						     (%POINTER TEM)))))))
	       (GO C-V))
	      ((AND CONS-LAP-INIT-STATE		;incremental assembly
		    (EQ (CAR EXP) 'MISC-ENTRY-ADR))
	       (SETQ V (CONS-LAP-EVAL
			 `(I-MEM (FIELD JUMP-ADDRESS-MULTIPLIER
					,(AREF (SYMBOL-FUNCTION SYS:MICRO-CODE-SYMBOL-AREA)
					       (- (GET (CADR EXP) 'QLVAL) #o200))))))
	       (GO C-V))
	      (T (CONS-LAP-BARF EXP 'UNRECGONIZED-OP 'DATA)
		 (GO X)))
     OR-2
	(COND ((NULL (CDR (SETQ EXP (CDR EXP))))
	       (GO X)))				;All NIL
     OR-1
	(SETQ TEM (CONS-LAP-EVAL (CADR EXP)))
	(COND ((NULL TEM) (GO OR-2)))		;That one evaluated to NIL
     MERGE-V
	(COND ((NULL VAL) (SETQ VAL TEM))
	      (T (SETQ VAL (PLUS VAL TEM))))
	(GO X)
     N1
	(SETQ TEM (CONS-LAP-EVAL (LIST (CAADR EXP) 1)))
	(COND ((= TEM 1) (GO X))		;That condition true, this false
	      (T (SETQ EXP (CADR EXP))		;That condition false, this true
		 (GO L1)))
     D-P
	(COND (DESTINATION-CONTEXT (GO L1)))
	(GO X)
     S-P
	(COND (DESTINATION-CONTEXT (GO X)))
	(GO L1)
     L2
	(ADD-FIELD-INDICATORS (CAR EXP))
     L1
	(SETQ EXP (CADR EXP))
	(GO L)
     I-P
	(COND ((EQ (CDR TEM) INSTRUCTION-CONTEXT)
	       (GO L1))				;Condition true
	      ((EQ INSTRUCTION-CONTEXT 'INSTRUCTION)
	       (CONS-LAP-BARF EXP 'UNDETERMINED-CONDITION 'WARN)))
	(GO X)					;Condition false
     C-V
	(COND ((NULL VAL) (SETQ VAL 0)))
	(COND ((NULL V)
	       (CONS-LAP-BARF EXP 'EVALUATED-TO-NIL 'DATA))
	      (T (SETQ VAL (PLUS VAL V))))
     X
	(RETURN VAL) ))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN CONVERT-LISP-BYTE (X)  ;CONVERT LISP-BYTE TO CORRESPONDING BYTE-FIELD
  (PROG (TEM)
	(SETQ TEM (si:eval-special-ok X))
	(RETURN (LIST 'BYTE-FIELD (LOGAND TEM 77) 
				  (LDB 0606 TEM)))))

(DEFUN CONVERT-ALL-BUT-LISP-BYTE (X)	;ADDRESS ALL BITS NOT IN BYTE. BYTE MUST BE
  (PROG (TEM BITS OVER)			;LEFT OR RIGHT ADJUSTED IN 32. BITS
	(SETQ TEM (si:eval-special-ok X))
	(SETQ BITS (LOGAND TEM 77) OVER (LDB 0606 TEM))
	(COND ((= 0 OVER)
		(SETQ OVER BITS)
		(SETQ BITS (- 32. BITS)))
	      ((= 32. (+ BITS OVER))
		(SETQ BITS (- 32. BITS))
		(SETQ OVER 0))
	      (T (CONS-LAP-BARF X 'ALL-BUT-BYTE-NOT-LEFT-OR-RIGHT-ADJUSTED 'DATA)))
	(RETURN (LIST 'BYTE-FIELD BITS OVER))))

))

; From file CADRLP.LISP KANSAS:<L.SYS> OZ: (149)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CADRLP  "

(DEFUN CONS-LAP-D-MEM-LOC (L)
  (OR CONS-LAP-PASS2
      (DO ((BL D-MEM-FREE-BLOCKS (CDR BL))
	   (TEM))
	  ((NULL (CDR BL)) (CERROR :YES NIL NIL 'CONS-LAP-D-MEM-LOC))
	(SETQ TEM (CADR BL))				;A BLOCK
	(COND ((AND (NOT (< L (CDR TEM)))		;IF LOC IS IN THIS BLOCK
		    (< L (+ (CDR TEM) (CAR TEM))))
	       (RPLACD BL (CDDR BL))			;PATCH OUT THIS BLOCK
	       (CONS-LAP-D-MEM-LOC-SPLITUP BL (CDR TEM) L)	;INSTALL BLOCKS BEFORE LOC
	       (CONS-LAP-D-MEM-LOC-SPLITUP BL (1+ L)	;INSTALL BLOCKS AFTER LOC
					   (+ (CAR TEM) (CDR TEM)))
	       (RETURN NIL)))))
  (SETQ D-MEM-LOC L
	IN-DISPATCH-BLOCK T
        DISPATCH-CONSTANT 0	;DONT ADD ANYTHING TO THIS ONE.
	DISPATCH-BLOCK-LIMIT (1+ L)))

))

; From file FREAD.LISP KANSAS:<L.IO> OZ: (29)
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FREAD  "

(defun read-ucode (file)
  (with-open-file (stream file :direction :input :characters t)
    ;; since these files have a way of saying (setq random-symbol ...)
    (si:eval-special-ok (fread stream))))

(defprop fread-stream t si:io-stream-p)
(defun fread-stream (operation &optional arg1 &rest rest &aux eof-flag)
  (case operation
    ((:tyi :read-char)
     (let ((ch (fread-tyi t)))			;preserve cr's so comments terminate!
       (if (eq operation :tyi) ch (int-char ch))))
    ((:untyi :unread-char)
     (setq *unrchf* (if (characterp arg1) (char-int arg1) arg1)))
    (:which-operations
     '(:tyi :read-char :untyi :unread-char))
    (t (stream-default-handler 'fread-stream operation arg1 rest))))
))

