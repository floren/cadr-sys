;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 22.3
;;; Reason:
;;;  Don't bomb if pathname's :complete-string method gets an error.
;;; Written 4-May-88 13:23:07 by pld at site Gigamos Cambridge
;;; while running on Azathoth from band 3
;;; with Experimental System 123.247, Experimental Local-File 73.4, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 22.3, Experimental Serial-IP 1.1, Experimental Tiger 27.1, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8, the old ones.



; From file DJ: L.FILE; SERVER.LISP#192 at 4-May-88 13:23:08
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-complete (fh args strings &aux path result success)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given to COMPLETE command." tid fh))
	((not (= (length strings) 2))
	 (format conn-stream "~A  ERROR IRF Wrong number of strings given in COMPLETE command."
		 tid))
;	((errorp (setq path (lmfs-parse-for-server (first strings))))
;	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname." tid))
	(t (setq path (lmfs-parse-for-server (first strings)))
	   ;;string result means an error
	   (if (errorp path)			;ZMACS will supply semibogus paths....!!
	       (setq path (fs:user-homedir si:local-host)))
	   (multiple-value (result success)
	     (condition-case (result)
		 (send path ':complete-string (second strings)
		       (list*
			 (if (memq ':write args) ':write ':read)
			 (if (memq ':new-ok args) ':new-ok ':old)
			 (if (memq ':deleted args) '(:deleted))))
	       (error
		(second strings))))
	   (let ((x (and result (string-search-char #/: result))))	;strip out host
	     (if x (setq result (substring result (1+ x)))))
	   (format conn-stream "~A  COMPLETE ~A~%~A~%" tid success result))))

))
