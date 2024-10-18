;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.4
;;; Reason:
;;;  Zwei now implements a new alist mechanism for remembering
;;;  user-defined keyboard macros.  Upon completion of a keyboard
;;;  macro at any macro level, via Meta-X End Keyboard Macro or
;;;  its equivalent, control-x ) , the user is now asked to supply
;;;  a name for the macro.  If they wish to leave it unnamed
;;;  ("temporary"), typing a carraige return will cause Zwei to
;;;  generate a name using (gensym).  This query includes a HELP
;;;  keystroke which displays all previously defined macros' names
;;;  (mouse-sensitively).  If the user clicks or spells out an
;;;  existing name, Zwei asks if it should replace the existing
;;;  definition.  If the user says no, then Zwei reprompts.  When
;;;  a name has been settled upon, both it and the array
;;;  containing the macro keystrokes are pushed onto
;;;  ZWEI:MACRO-ALIST as a new element.
;;;  
;;;  Meta-X View Keyboard Macro now looks up macros in
;;;  ZWEI:MACRO-ALIST, using the same (mouse sensitive) HELP key
;;;  display as the query sequence which appears when finishing
;;;  macro definitions.
;;;  
;;;  One of the advantages of this scheme is that through
;;;  judicious use of init files, ZWEI:MACRO-ALIST and Meta-X
;;;  Install Macro one can now easily use one's favorite macros
;;;  across cold boots.
;;;  
;;;  Thanks to Keith for many useful suggestions and improvements.
;;; Written 30-Aug-88 15:02:35 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.66, Experimental ZWEI 126.3, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; KBDMAC.LISP#49 at 30-Aug-88 15:06:18
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; KBDMAC  "

(defvar macro-alist nil)

))

; From modified file DJ: L.ZWEI; KBDMAC.LISP#49 at 30-Aug-88 15:06:22
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; KBDMAC  "

(defun alist-member (elt alist)
  ;; Necessary because of completing-read-from-mini-buffer's lack of return value type...
  (if (consp elt)
      (assoc (car elt) alist)
      (assoc elt alist)))

))

; From modified file DJ: L.ZWEI; KBDMAC.LISP#49 at 30-Aug-88 15:06:26
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; KBDMAC  "

(defun read-macro-name-from-mini-buffer (macro-level)
  (let ((name-or-pair
	  (completing-read-from-mini-buffer
	    (if (= macro-level 0)
		"Name this macro (RETURN generates an internal name, HELP shows existing keyboard macro names)"
	      "Name this nested macro (RETURN generates an internal name, HELP shows existing keyboard macro names)")
	    macro-alist
	    ;;no completions exist? ==> ok
	    t)))
    (cond ((consp name-or-pair)
	   ;; user typed an existing kbd macro name -- replace or recurse!
	   (if (y-or-n-p "Replace existing version of ~A? " (car name-or-pair))
	       ;;return only the string in the car slot
	       (car name-or-pair)
	     (read-macro-name-from-mini-buffer macro-level)))
	  ((string-equal (string name-or-pair) "")
	   ;; user typed carraige return -- (string (gensym)) allows
	   ;; completing read to compare this name with future
	   ;; additions...
	   (string (gensym)))
	  (t
	   ;; new name entered -- it's a string already.
	   name-or-pair))))

))

; From modified file DJ: L.ZWEI; KBDMAC.LISP#49 at 30-Aug-88 15:06:31
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; KBDMAC  "

(defun remember-last-kbd-macro ()
  (let* ((macro-level (send *standard-input* :macro-level))
	 (name (read-macro-name-from-mini-buffer macro-level))
	 (alist macro-alist)
	 (name-mac-pair (cons name
			      (send *standard-input* :macro-previous-array))))
    (if (equal alist macro-alist)
	;; Replace the old contents of MACRO-ALIST
	(setq macro-alist
	      (delq (alist-member name-mac-pair macro-alist) macro-alist)))
    (pushnew name-mac-pair macro-alist)
    (format *query-io* "Last macro named /"~A/".  Use View Keyboard Macro to see macro definitions." name)))

))

; From modified file DJ: L.ZWEI; KBDMAC.LISP#49 at 30-Aug-88 15:06:53
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; KBDMAC  "

(defvar MACRO-POP-HOOK 'remember-last-kbd-macro
  "If non-NIL, funcalled by macro stream after :MACRO-POP operation.")

))

; From modified file DJ: L.ZWEI; COMG.LISP#49 at 30-Aug-88 15:07:37
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
	 (format t "~&~1(~A~) consists of:~2&" name)
	 (DO ((I 0 (1+ I))
	     (CH))
	    ((> I LEN))
	  (FORMAT T (SELECTQ (SETQ CH (AREF MAC I))
		      (*MOUSE* "Mouse command ~*")
		      (*SPACE* "Macro query ~*")
		      (*RUN* "Repeat ~*")
		      (NIL "Input ~*")
		      (OTHERWISE "~:C "))
		  CH))
	 (format t "~2&")))))
  DIS-NONE)

))
