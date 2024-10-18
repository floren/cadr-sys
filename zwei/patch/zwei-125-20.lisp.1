;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.20
;;; Reason:
;;;  make sure the *fasd-interface* argument to COMPILE-PRINT-INTERVAL is REALLY gone 
;;;  --pfc
;;; Written 2-Sep-88 00:53:30 by wkf (William K. Foster) at site Gigamos Cambridge
;;; while running on Love from band 2
;;; with Experimental System 126.67, ZWEI 125.19, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.13, SDU ROM 102, Kenv 8/19.



; From file DJ: L.ZWEI; COMC.LISP#227 at 2-Sep-88 00:53:32
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-BUFFER-CHANGED-FUNCTIONS (BUFFER ASK-P
					 &OPTIONAL (COMPILE-P T)
					 (NAMES '("Compile" "Compiling" "compiled."))
					 (*target-computer* 'compiler:lambda-interface)
					 &AUX (*QUERY-IO* *STANDARD-OUTPUT*))
  "Recompile or evaluate all changed sections in BUFFER (that contain definitions).
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
ASK-P if non-NIL means query user for each section to be processed.
NAMES has three elements, that are like (/"Compile/" /"Compiling/" /"compiled./")."
  (LET ((*INTERVAL* BUFFER))
    (RESECTIONIZE-BUFFER *INTERVAL*)
    (DOLIST (SECTION (NODE-INFERIORS *INTERVAL*))
      (IF (AND (TYPEP SECTION 'SECTION-NODE)
	       (NOT (STRINGP (SECTION-NODE-NAME SECTION)))
	       (NOT (BP-= (INTERVAL-FIRST-BP SECTION) (INTERVAL-LAST-BP SECTION)))
	       (> (NODE-TICK SECTION)
		  (SECTION-NODE-COMPILE-TICK SECTION))
	       (OR (NOT ASK-P)
		   (FQUERY () "~A ~A? " (FIRST NAMES)
			   (SECTION-NODE-NAME SECTION))))
	  (COMPILE-PRINT-INTERVAL SECTION NIL T COMPILE-P
				  NIL			;region-name
				  (SECOND NAMES)	;mode-name
				  (THIRD NAMES)		;echo-name
				  T			;use-typeout
				  T			;defvar-hack
				  NIL			;compiler-processing-mode
				  T			;already-resectionized-flag
				  *target-computer*
				  )))))

))
