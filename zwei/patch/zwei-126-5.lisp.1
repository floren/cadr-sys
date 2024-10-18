;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.5
;;; Reason:
;;;  More macro fixups -- among other things, this patch corrects
;;;  misleading macro installation and deinstallation messages. 
;;;  
;;;  saz
;;; Written 30-Aug-88 18:01:59 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.67, Experimental ZWEI 126.4, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; COMG.LISP#50 at 30-Aug-88 18:40:56
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "

(DEFUN COM-INSTALL-MACRO-INTERNAL (MOUSE-P)
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
	name-mac-pair NAME MAC)
    (setq name-mac-pair (completing-read-from-mini-buffer
			  "Name of macro to install (<Return> for last macro defined):"
			  macro-alist))
    (typecase name-mac-pair
      (cons
       ;;found an elt in macro-alist matching the input
        (setq name (car name-mac-pair)
	      mac (cdr name-mac-pair)))
      (string
       ;;either non-matching input or user typed carraige return, indicating last macro defined.
	(if (string-equal NAME-MAC-PAIR "")
	    (SETQ MAC (cdr (first macro-alist))
		  name (car (first macro-alist)))
	  (BARF "~A is not a defined macro." NAME-MAC-PAIR)))
      ;; this line should never be evaluated
      (otherwise (barf "Unrecognized alist entry in MACRO-ALIST: ~A" name-mac-pair)))
    (putprop (intern-soft (read-from-string name)) mac 'macro-stream-macro)
    ;; Kind of a shame that MAKE-MACRO-COMMAND can't return something from which
    ;; name is extractable...
    (INSTALL-COMMAND-INTERNAL (MAKE-MACRO-COMMAND NAME MOUSE-P) T)))

))

; From modified file DJ: L.ZWEI; COMG.LISP#50 at 30-Aug-88 18:38:07
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "


(DEFUN INSTALL-COMMAND-INTERNAL (COMMAND &OPTIONAL REMEMBER-OLD-P DEINSTALL-P
				 &AUX KEY-LIST)
  (PROMPT-LINE (IF DEINSTALL-P "Key to deinstall:" "Key to get it:"))
  (CLEAR-PROMPTS)
  (ALWAYS-DISPLAY-PROMPTS)
  (DO ((COMTAB *COMTAB*)
       (KEY (INPUT-WITH-PROMPTS *STANDARD-INPUT* :MOUSE-OR-KBD-TYI)
	    (INPUT-WITH-PROMPTS *STANDARD-INPUT* :TYI)))
      (NIL)
    (PUSH KEY KEY-LIST)
    (RECORD-MINI-BUFFER-VALUE NIL)
    (LET ((OLD-COMMAND (COMMAND-LOOKUP KEY COMTAB))
	  (yornp-string (if deinstall-p
			    (format nil "Remove existing command from ~:@C?" (int-char key))
			  (format nil "Install ~A on~{ ~:@C~}? "
				  (typecase COMMAND
				    (closure (format nil "macro ~A" (symeval-in-closure command 'symbol)))
				    (symbol (format nil "command ~A" command)))
				  (REVERSE KEY-LIST))))
	  (confirmation-string (if deinstall-p "~&Removed." "~&Installed.")))
      (COND ((AND (PREFIX-COMMAND-P OLD-COMMAND)
		  (NOT *NUMERIC-ARG-P*))
	     (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
	    ((Y-OR-N-P yornp-string)
	     ;; >>This change seems to be broken, and I don't think it can
	     ;; actually do the right thing anyway. (RpK, 9-May-86 16:17:20) Now
	     ;; we know the key sequence, so decide which comtab to really start
	     ;; from.
	     #|
	     (DO ((COMTAB (SYMEVAL (FQUERY '(:CHOICES (((*ZMACS-COMTAB* "Zmacs") #\Z)
						       ((*STANDARD-COMTAB* "All Zwei editors") #\A)
						       ((*COMTAB* "Just this window") #\W #\T))
						      :TYPE :TYI)
					   "Define for Zmacs, all editors, or just this window? ")))
		  (KEYS KEY-LIST (CDR KEY-LIST)))
		 ((NULL (CDR KEY-LIST)) ... clauses below)
	       (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB))) |#
	     (AND DEINSTALL-P (typep command :closure)
		  (SETQ COMMAND (MOUSE-MACRO-COMMAND-LAST-COMMAND
				  (COMMAND-LOOKUP KEY COMTAB))))
	     (AND REMEMBER-OLD-P (typep command :closure)
		  (SET-MOUSE-MACRO-COMMAND-LAST-COMMAND COMMAND
							(COMMAND-LOOKUP KEY COMTAB)))
	     (COMMAND-STORE COMMAND KEY COMTAB)
	     (FORMAT *QUERY-IO* confirmation-string)
	     (RETURN NIL))
	    (T (FORMAT *QUERY-IO* "~&Not installed.")
	       (RETURN NIL)))))
  DIS-NONE)

;;; EMACS compatible macro commands

))

; From modified file DJ: L.ZWEI; COMTAB.LISP#357 at 30-Aug-88 18:39:37
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN MAKE-MACRO-COMMAND (THE-SYMBOL &OPTIONAL MOUSE-P)
  (LET-CLOSED ((SYMBOL (typecase the-symbol
			 (string (read-from-string THE-SYMBOL))
			 (otherwise the-symbol)))
	       (MOVE-TO-MOUSE-P MOUSE-P)
	       (PREVIOUS-COMMAND NIL))
    (LAMBDA ()
      (LET ((MAC (GET SYMBOL 'MACRO-STREAM-MACRO)))
	(OR MAC (BARF "The macro ~A is not defined." SYMBOL))
	(OR (OPERATION-HANDLED-P *STANDARD-INPUT* :MACRO-EXECUTE)
	    (BARF "The input stream does not support macros."))
	(AND MOVE-TO-MOUSE-P
	     (MOVE-BP (POINT) (MOUSE-BP *WINDOW* *MOUSE-X* *MOUSE-Y*)))
	(SEND *STANDARD-INPUT* :MACRO-EXECUTE MAC (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))
	DIS-NONE))))

))

