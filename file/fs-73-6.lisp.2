;;; -*- Mode:Lisp; Readtable:T; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 73.6
;;; Reason:
;;;  Offer to dismount the file system before a :system-release garbage collection.
;;; Written 11-May-88 20:54:53 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 3
;;; with Experimental System 123.276, Experimental Local-File 73.5, Experimental FILE-Server 22.5, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.2, Experimental Lambda-Diag 15.0, Experimental Tape 22.4, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8, the old ones.



; From modified file DJ: L.FILE; FSGUTS.LISP#439 at 11-May-88 20:54:59
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "


(defun maybe-dismount-file-system ()
  (when (global:with-timeout ((* 30 60.) (progn (write-string "Yes (after timeout)" *query-io*) t))
	    (yes-or-no-p "    Dismount file system (Yes after thirty seconds) ? "))
    (dismount-file-system)))

))

; From modified file DJ: L.FILE; FSGUTS.LISP#439 at 11-May-88 20:55:01
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "


(add-initialization "Maybe Dismount File System" '(maybe-dismount-file-system) '(:gc-system-release))


))
