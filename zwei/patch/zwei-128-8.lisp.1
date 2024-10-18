;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 128.8
;;; Reason:
;;;  Zwei support for cross compiling a DEFAFUN from an editor buffer.
;;; Written 22-Nov-88 18:58:07 by smh (Steve Haflich) at site Gigamos Cambridge
;;; while running on Alex from band 1
;;; with Experimental System 130.9, Experimental ZWEI 128.7, Experimental ZMail 75.0, Experimental Local-File 77.0, Experimental File-Server 26.0, Experimental Unix-Interface 16.0, Experimental Tape 27.0, Experimental Lambda-Diag 19.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 11/18 Falcon System Loaded.



; From modified file DJ: L.ZWEI; COMC.LISP#231 at 22-Nov-88 18:58:12
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-BUFFER-FORM (FORM TYPE)
  (DECLARE (SPECIAL COMPILE-PROCESSING-MODE))
  ;; $$$ Cosmetic changes, and added DEFAFUN <22-Nov-88 smh>
  (cond ((MEMQ TYPE '(DECLARE RANDOM SPECIAL proclaim))
	 (EVAL FORM))
	((eq type 'compiler:defafun)
	 (compiler::compile-defafun form))
	(t (let-if (get-from-alternating-list compile-processing-mode :CAREFUL-MODE)
		   ((compiler:peep-enable nil)
		    (compiler:*inhibit-optimizers* t))
	     (COMPILER:COMPILE-1 (CADR FORM)
				 (APPEND
				   (CASE (CAR FORM)
				     (DEFSUBST '(NAMED-SUBST))
				     (MACRO '(MACRO NAMED-LAMBDA))
				     (DEFUN '(NAMED-LAMBDA)))
				   (CDR (SI:PROCESS-DEFUN-BODY (CADR FORM) (CDDR FORM))))
				 (IF (MEMQ (CAR FORM) '(MACRO DEFSUBST))
				     'COMPILER:MACRO-COMPILE
				   (GET-FROM-ALTERNATING-LIST COMPILE-PROCESSING-MODE :MODE)))))))

))
