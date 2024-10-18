;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.6
;;; Reason:
;;;  User is now informed of new macro's name, along with the sentence,
;;;  "Use Meta-X Install Macro to associate a macro with a key."
;;;  
;;;  saz
;;; Written 30-Aug-88 18:48:49 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.67, Experimental ZWEI 126.4, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; KBDMAC.LISP#51 at 30-Aug-88 18:48:50
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
    (format *query-io* "Last macro named /"~A/".  Use Meta-X Install Macro to associate a macro with a key." name)))
))
