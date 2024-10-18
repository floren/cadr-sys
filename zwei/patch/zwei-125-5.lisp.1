;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.5
;;; Reason:
;;;  zwei:definition-function-start should not blow up if handed a null line.
;;; Written 27-Jul-88 15:53:07 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 1
;;; with Experimental System 126.2, ZWEI 125.4, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.ZWEI; SECTIO.LISP#314 at 27-Jul-88 15:53:12
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(defun definition-function-start (line &aux idx end-idx)
  (when line
    (if (not (char-equal (char line 0) #/())
	0
      (setq end-idx (string-search-set *whitespace-chars* line 1))
      (if (setq idx (string-search-char #/: line 1 end-idx))
	  (do ()
	      ((char-not-equal (char line idx) #/:) idx)
	    (incf idx))
	1))))

))
