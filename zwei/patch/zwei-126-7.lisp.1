;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.7
;;; Reason:
;;;  Meta-X View Keyboard Macro now prints out a readable message
;;;  upon encountering embedded keyboard macros (macros within 
;;;  macros) instead of generating an error.
;;;  
;;;  saz
;;; Written 30-Aug-88 19:06:39 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.67, Experimental ZWEI 126.4, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; COMG.LISP#51 at 30-Aug-88 19:06:39
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "

(DEFCOM COM-VIEW-KEYBOARD-MACRO "Typeout the specified keyboard macro.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer.
Just Return means the last one defined, even if temporary." ()
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
	NAME MAC)
    (SETQ NAME (completing-read-from-mini-buffer "Name of macro to view (Press <Return> for last macro defined):"
						 macro-alist))
    (typecase name
      (symbol (setq name (string name)))
      (string
       (setq mac (cdr (lisp:assoc name macro-alist :test #'string-equal))))
      (list
       (psetq name (car name)
	      mac (cdr name))))
    (COND ((string-equal NAME "")
	   (SETQ MAC (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY))
	   (setq name "last macro defined"))
	  ((null mac)
	   (BARF "~A is not a defined macro." NAME)))
    (let ((len (macro-length mac)))
      (cond
	((minusp len)
	 (beep)
	 (format *query-io* "~1(~A~) contains no keystrokes." name))
	(t
	 (format t "~&~1(~A~) consists of: ~2%" name)
	 (DO ((I 0 (1+ I))
	     (CH))
	    ((> I LEN))
	  (FORMAT T (typecase (SETQ CH (AREF MAC I))
		      (array "<an embedded keyboard macro, which cannot be displayed> ~*")
		      (t (selectq ch
			   (*MOUSE* "Mouse command ~*")
			   (*SPACE* "Macro query ~*")
			   (*RUN* "Repeat ~*")
			   (NIL "Input ~*")
			   (OTHERWISE "~:C "))))
		  CH))
	 (format t "~2&")))))
  DIS-NONE)

))
