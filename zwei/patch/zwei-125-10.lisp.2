;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.10
;;; Reason:
;;;  DIRED gets confused between pathnames on the host LOCAL: and the real local host.
;;;  This messes up renames when the host for the DIRED buffer is LOCAL -- the file
;;;  getting renamed may have the "real" host as its host component, and then DIRED would
;;;  think the new name was in a different directory, "not in this display".
;;;  
;;;  Solve this by creating and using a new function, DIRED-PATHNAME-EQUAL.
;;;  It considers hosts "LOCAL" and SI:LOCAL-HOST to be equivalent for its
;;;  purposes.  Also, always compare pathnames with FS:PATHNAME-EQUAL, not EQ.
;;; Written 2-Aug-88 22:07:29 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.7, ZWEI 125.9, ZMail 73.0, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, falambka.



; From modified file DJ: L.ZWEI; DIRED.LISP#337 at 2-Aug-88 22:07:40
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(defun dired-pathname-equal (path1 path2)
  (let ((lm (fs:get-pathname-host "LOCAL" :maybe-it-is-not-defined?)))
    (cond
      ((null lm))
      ((eq (pathname-host path1) lm)
       (setq path1 (send path1 :new-pathname :host si:local-host)))
      ((eq (pathname-host path2) lm)
       (setq path2 (send path2 :new-pathname :host si:local-host))))
    (fs:pathname-equal path1 path2)))


))

; From modified file DJ: L.ZWEI; DIRED.LISP#337 at 2-Aug-88 22:07:43
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-PATHNAME-DIRECTORY-LINE (PATHNAME &AUX DIR-PATHNAME buffer-pathname)
  "Return the line in the DIRED buffer for PATHNAME's directory."
  (SETQ DIR-PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME NIL
			   			  :TYPE NIL
						  :VERSION NIL))
  (setq buffer-pathname
	(SEND (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*)
	      :NEW-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL))
  (IF (dired-pathname-equal DIR-PATHNAME buffer-pathname)
      (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
    (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE)))
	((NULL LINE))
      (AND (GETF (LINE-PLIST LINE) :DIRECTORY)
	   (DIRED-LINE-PATHNAME LINE)
	   (EQ (SEND (DIRED-LINE-PATHNAME LINE) :PATHNAME-AS-DIRECTORY)
	       DIR-PATHNAME)
	   (RETURN (AND (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT)
			LINE))))))


))
