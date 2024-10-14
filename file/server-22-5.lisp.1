;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 22.5
;;; Reason:
;;;  It's ok if you don't give any properties to change.  Used to return
;;;  an error "No pathname given", which was a complete lie.
;;; Written 9-May-88 15:54:33 by pld at site Gigamos Cambridge
;;; while running on Azathoth from band 3
;;; with Experimental System 123.264, Experimental Local-File 73.5, Experimental FILE-Server 22.4, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.2, Experimental Lambda-Diag 15.0, Experimental Tape 22.4, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8, the old ones.



; From modified file DJ: L.FILE; SERVER.LISP#193 at 9-May-88 15:54:33
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-change-props (fh strings)
  (trap-lossage (error "Change properties toplevel")
      (cond ((null fh)
	     (if (not (> (length strings) 0))
		 (format conn-stream "~A  ERROR IRF F No pathname given." tid)
	       (let ((path (lmfs-parse-for-server (car strings))))
		 (if (errorp path)
		     (format conn-stream "~A  ERROR IPS F Syntax error in supplied path: ~A"
			     tid)
		   (change-props-1 path "" (cdr strings))))))
	    (t (let ((data (get fh server-instance)))
		 (if (or (null data) (null (server-dataproc-comm-opening data)))
		     (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
			     tid fh fh)
		   (change-props-1 (server-dataproc-comm-opening data) fh strings)))))
    (format conn-stream "~A ~A ERROR SYS F Internal error:~% ~A" tid (or fh "") trap-error)))

))
