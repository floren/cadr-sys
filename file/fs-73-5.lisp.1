;;; -*- Mode:Lisp; Readtable:T; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 73.5
;;; Reason:
;;;  (fs:lookup-subdirectory) with OK-IF-NOT-THERE == T now returns
;;;  NIL if any directory step does not exist, rather than throwing
;;;  you into error handler if any intermediate step doesn't exist.
;;; Written 3-May-88 14:06:33 by keith at site Gigamos Cambridge
;;; while running on Azathoth from band 3
;;; with Experimental System 123.247, Experimental Local-File 73.4, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 22.3, Experimental Serial-IP 1.1, Experimental Tiger 27.1, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8, the old ones.



; From file DJ: L.FILE; FSGUTS.LISP#439 at 3-May-88 14:06:34
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN LOOKUP-SUBDIRECTORY (NODE SUBPATH OK-IF-NOT-THERE)
  (IF (NULL (CDR SUBPATH))
      (LOOKUP-SUBDIRECTORY-STEP NODE
				(CAR SUBPATH)
				OK-IF-NOT-THERE)
    (let ((step (LOOKUP-SUBDIRECTORY-STEP NODE
					  (CAR SUBPATH)
					  OK-IF-NOT-THERE)))
      (and step
	   (LOOKUP-SUBDIRECTORY step
				(CDR SUBPATH)
				OK-IF-NOT-THERE)))))

))
