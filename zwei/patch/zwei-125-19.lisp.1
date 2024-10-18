;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.19
;;; Reason:
;;;  Fix COM-YANK-DEFAULT-STRING to use FORMAT ~S, so defaults that are lists
;;;  print correctly.
;;; Written 17-Aug-88 12:42:52 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with Experimental System 126.55, ZWEI 125.18, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; COMD.LISP#178 at 17-Aug-88 12:42:54
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-YANK-DEFAULT-STRING "Insert the default string into the mini buffer." ()
  (IF (NULL *MINI-BUFFER-DEFAULT-STRING*)
      (BARF "There is no default in this context.")
    (INSERT-MOVING (POINT)
		   (typecase *MINI-BUFFER-DEFAULT-STRING*
		     ((or character string) *mini-buffer-default-string*)
		     (symbol (string *mini-buffer-default-string*))
		     (list (format nil "~S" *mini-buffer-default-string*))
		     (t (format nil "~A" *mini-buffer-default-string*)))))
  DIS-TEXT)

))
