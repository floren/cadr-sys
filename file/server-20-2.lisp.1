;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 20.2
;;; Reason:
;;;  *lmfs-parse-for-server-translations* again.
;;; Written 1-Dec-86 15:07:56 by rg at site LMI Cambridge
;;; while running on Moe from band 3
;;; with Experimental System 119.3, Experimental Local-File 71.0, Experimental FILE-Server 20.0, Experimental Lambda-Diag 11.0, Experimental Tape 12.0, Experimental ZMail 68.0, Experimental KERMIT 32.0, Experimental Unix-Interface 10.0, microcode 1715, SDU Boot Tape 3.14, SDU ROM 103.



; From file DJ: L.FILE; SERVER.LISP#186 at 1-Dec-86 15:07:57
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defvar *lmfs-parse-for-server-translations* nil)
;; if not NIL this is an alist of ("foreign-hostname" . t-alist)
;; where t-alist is ("from" "to")
	
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
		  (let ((new (condition-case (x)
				 (lmfs-parse-for-server-translate-1
				  pathname (cdr tr))
			       (pathname-error x))))
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


(defun lmfs-parse-for-server-translate-1 (path alist)
  (dolist (x alist path)
    (let ((pat (parse-pathname (car x) si:local-host)))
      (when (send pat :pathname-match path)
	(return (send pat :translate-wild-pathname
		      (parse-pathname (cadr x) si:local-host)
		      path))))))
))
