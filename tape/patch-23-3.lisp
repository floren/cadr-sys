;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 23.3
;;; Reason:
;;;  It appears that TI tapes store some unusual file properties:  :host :directory
;;;  :device :name :type :version.  The tape system has code to filter these out,
;;;  but apparently an older version didn't and we have some files on disk with
;;;  those properties.  We should filter them out when we write tapes to avoid
;;;  a silly warning when reading the tape again....
;;; Written 31-May-88 16:14:03 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 2
;;; with Experimental System 124.13, Experimental Local-File 74.0, Experimental File-Server 23.1, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.2, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.TAPE; LMFL-FORMAT.LISP#200 at 31-May-88 16:14:03
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; LMFL-FORMAT  "

(defmethod (lmfl-format :read-file-header) (device &optional (host-for-parsing si:local-host))
  (check-device device)
  (check-host host-for-parsing)
  (If (Not (Null current-plist))
      current-plist
    (let ((*read-base* 10.))
      (using-resource (header-block si:dma-buffer (/ record-size *bytes-per-page*))
	(condition-case ()
	    (send device :read-block header-block record-size)
	  ((filemark-encountered physical-end-of-tape)
	   (signal 'logical-end-of-tape :device-object device))
	  (:no-error
	   (let ((string (si:dma-buffer-string header-block))
		 plist)
	     (unless (string-equal string "LMFL" :end1 4)
	       (signal 'bad-file-header
		       :format-type 'lmfl
		       :header string))
	     (cond ((string-equal string "#!C" :start1 4 :end1 7)
		    (print 'ti-format)
		    (setq plist (read-from-string string nil :no-plist :start 7)))
		   (t
		    (setq plist (read-from-string string nil :no-plist :start 4))))
	     (cond ((atom (cdr plist))
		    (signal 'bad-file-header
			    :format-type 'lmfl
			    :header string))
		   (t
		    (setq plist (check-plist-validity plist))
		    (setq current-plist
			  (cons (when host-for-parsing
				  (fs:make-pathname
				    :host      host-for-parsing
				    :device    (getf plist :device)
				    :directory (getf plist :directory)
				    :name      (getf plist :name)
				    :type      (getf plist :type)
				    :version   (getf plist :version)))
				(dolist (elem '(:host :directory :device :name :type :version) plist)
				  (remf plist elem))))))))))))
  )

))

; From modified file DJ: L.TAPE; LMFL-FORMAT.LISP#200 at 31-May-88 16:39:51
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; LMFL-FORMAT  "

(defmethod (lmfl-format :write-file-header) (device truename attribute-list)
  (check-device device)
  (check-type truename pathname)
  (check-attribute-list attribute-list)
  (let* ((*print-base* 10.)
	 (plist (cond ((getf attribute-list :partition)
		       attribute-list)
		      (t
		       (nconc (list :device (pathname-device truename)
				    :directory (pathname-directory truename)
				    :name (pathname-name truename)
				    :type (pathname-type truename)
				    :version (pathname-version truename))
			      (let ((x (copy-list attribute-list)))
				(dolist (elem '(:host :directory :device :name :type :version) x)
				  (remf x elem))
				x)))))
	 (string (format nil "LMFL~S" plist)))
    (using-resource (header-block si:dma-buffer (/ record-size *bytes-per-page*))
      (copy-array-contents string (si:dma-buffer-string header-block))
      (Setq current-plist nil)
      (setq tape-modified t)
      (send device :write-block header-block record-size))))

))
