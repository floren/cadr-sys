;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.1
;;; Reason:
;;;  DIRED-PATHNAME-EQUAL doesn't blow up on arguments that aren't pathnames.
;;;  Also, it coerces strings into pathnames.
;;; Written 19-Aug-88 12:33:31 by saz at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with Experimental System 126.55, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, Experimental ZWEI 126.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; DIRED.LISP#338 at 19-Aug-88 12:33:31
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(defun dired-pathname-equal (path1 path2)
  (typecase path1
    (string (setq path1 (pathname path1))))
  (typecase path2
    (string (setq path2 (pathname path2))))
  (when (and (typep path1 'pathname) (typep path2 'pathname))
    (let ((lm (fs:get-pathname-host "LOCAL" :maybe-it-is-not-defined?)))
      (cond
	((null lm))
	((eq (pathname-host path1) lm)
	 (setq path1 (send path1 :new-pathname :host si:local-host)))
	((eq (pathname-host path2) lm)
	 (setq path2 (send path2 :new-pathname :host si:local-host))))
      (fs:pathname-equal path1 path2))))

))
