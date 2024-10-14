;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 20.1
;;; Reason:
;;;  *lmfs-parse-for-server-translations* a kludge for fooling clients
;;; Written 20-Nov-86 13:27:50 by rg (Richard Greenblatt) at site LMI Cambridge
;;; while running on Guinea Pig from band 1
;;; with Experimental System 119.4, Experimental Local-File 71.0, Experimental FILE-Server 20.0, Experimental Lambda-Diag 11.0, Experimental Tape 12.0, Experimental ZMail 68.0, Experimental KERMIT 32.0, Experimental Unix-Interface 10.0, Experimental NVRAM 3.0, microcode 1715, SDU Boot Tape 3.12, SDU ROM 102.



; From file DJ: L.FILE; SERVER.LISP#185 at 20-Nov-86 13:27:53
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "


(defvar *lmfs-parse-for-server-translations* nil)
;; if not NIL this is an alist of ("foreign-hostname" "logical-hostname")
;; any pathnames opened from the host become logical pathnames.
	
(defun lmfs-parse-for-server (string)
  (condition-case (result)
      (lmfs-parse-for-server-translate 
       (fs:merge-pathname-defaults
	 string
	 local-host-pathname
	 ':unspecific ':newest))
    (pathname-error
     result)))



(defun lmfs-parse-for-server-translate (pathname)
  (cond ((not *lmfs-parse-for-server-translations*)
	 pathname)
	('else
	 (let ((tr (ass #'(lambda (addr host)
			    (let ((addrs (send (si:parse-host host) :network-addresses)))
			      (member addr (getf addrs :chaos))))
			(and (boundp 'conn) conn (chaos:foreign-address conn))
			*lmfs-parse-for-server-translations*)))
	   (cond ((not tr)
		  pathname)
		 ('else
		  (let ((new (condition-case (r)
				 (send (send pathname :new-pathname
					     :host (cadr tr)
					     :device :unspecific)
				       :translated-pathname)
			       (fs:unknown-logical-pathname-translation
				r))))
		    (cond ((errorp new)
			   (tv:notify nil "File serving ~S ~A"
				      (car tr)
				      (send new :report-string))
			   pathname)
			  ((eq new pathname)
			   pathname)
			  ('else
			   (tv:notify nil "File serving ~S translating ~A => ~A"
				      (car tr) pathname new)
			   new)))))))))
))
