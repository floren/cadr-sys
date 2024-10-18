;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.19
;;; Reason:
;;;  Another optional arg to ZWEI:COMPILE-DEFUN-INTERNAL
;;;  An explicit COMPILATION-ENVIRONMENT may be passed in.
;;;  This is used when called to cross compile for the Falcon.
;;; Written 14-Oct-88 15:54:04 by smh at site Gigamos Cambridge
;;; while running on Alex from band 4
;;; with Experimental System 126.103, Experimental ZWEI 126.17, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 10/4.



; From modified file DJ: L.ZWEI; COMC.LISP#227 at 14-Oct-88 15:54:24
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-DEFUN-INTERNAL (COMPILE-P MODE-NAME ECHO-NAME
			       &OPTIONAL USE-TYPEOUT DEFVAR-HACK
			       (COMPILER-PROCESSING-MODE
				 '(:MODE COMPILER:MACRO-COMPILE))
			       (*target-computer* 'compiler:lambda-interface)
			       (compilation-environment nil compilation-environment-p)	;||| 14oct smh
			       &AUX BP1 BP2 DEFUN-NAME)
  "Compile or evaluate a part of the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
If there is a region, it is used; otherwise the current or following defun is used.
USE-TYPEOUT is passed to COMPILE-PRINT-INTERVAL and controls where information is printed.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (COND ((WINDOW-MARK-P *WINDOW*)
	 (SETQ BP1 (MARK) BP2 (POINT))
	 (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	 (SETQ DEFUN-NAME "Region"))
	((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
	 (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	 (SETQ DEFVAR-HACK T))
	(T
	 (BARF "Cannot find a defun near point.")))
  (progv (and compilation-environment-p '(compiler:*compilation-environment*))	;||| 14oct smh
	 (list compilation-environment)
    ;; Add this binding to allow c-m from the debugger to do the correct thing.
    ;; ||| JIM 10/19/88
    (let ((eh:*bug-report-recipient-system* (if (eq *target-computer* 'compiler:k)
					     "FALCON-COMPILER"
					   "LISPM")))
      (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P
			      DEFUN-NAME MODE-NAME ECHO-NAME USE-TYPEOUT DEFVAR-HACK
			      COMPILER-PROCESSING-MODE
			      nil			;already-resectionized-flag
			      *target-computer*))))

))
