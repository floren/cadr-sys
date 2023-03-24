;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for FILE-Server version 8.1
;;; Reason: FILE-SERVER-OPEN defaulting of :BYTE-SIZE.
;;; Written 1/04/84 00:48:42 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.22, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.0, Experimental FILE-Server 8.0, Experimental LFS 3.0, Experimental MagTape 22.0, microcode 306, Xmntl FS.



; From file SERVER.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-open (fh rest filename &aux answer binp
			 (characters t) direction directionp
			 if-exists if-does-not-exist (byte-size ':default)
			 deleted preserve-dates inhibit-links)
  (let ((losep
	  (*catch 'open-opt-lost
	    (progn
	      (loop for olist on rest
		    do
		    (let ((opt (car olist)))
		      (selectq opt
			(:binary (setq characters nil))
			(:character (setq characters t))
			(:default (setq characters ':default))
			(:read (setq direction ':input directionp t))
			(:write (setq direction ':output directionp t))
			(:probe (setq direction nil directionp t))
			(:probe-directory
			 (setq direction ':probe-directory directionp t))
			(:inhibit-links
			 (setq inhibit-links t))
			((:temporary :raw :super-image))
			(:deleted (setq deleted t))
			(:preserve-dates (setq preserve-dates t))
			(:byte-size
			 (setq byte-size (cadr olist))
			 (pop olist))
			(:if-exists (setq if-exists (cadr olist))
				    (pop olist))
			(:if-does-not-exist (setq if-does-not-exist (cadr olist))
					    (pop olist))
			(t (open-err "UOO F Unknown option: " opt)))))
	      (if (null fh)
		  (if (memq direction '(:input :output))
		      (open-err "ICO F Inconsistent open options for probe opening"))
		;; FHN given. must be real read or write.
		(let* ((comdata (get fh server-instance))
		       (type (selectq (server-dataproc-comm-iotype comdata)
			       (input ':input)
			       (output ':output))))
		    (if (null comdata)
			(open-err "UFH F No open data channel for this file handle: " fh))
		    (if directionp
			(unless (eq direction type)
			  (open-err "ICO F File handle type inconsistent with open mode."))
		      (setq direction type))))
	      (let ((pathname (lmfs-parse-for-server filename)))
		(if (errorp pathname) (open-err "IPS F Bad filename syntax: " pathname))
		(let ((opening
			(open pathname
			      ':direction direction			      
			      ':characters characters
			      ':if-does-not-exist (or if-does-not-exist
						      (selectq direction
							((:input nil) ':error)
							(:output ':create)))
			      ':if-exists (or if-exists
					      (if (memq (pathname-version pathname)
							'(:unspecific :newest))
						  ':new-version ':supersede))
			      ':error nil
			      ':inhibit-links inhibit-links
			      ':deleted deleted
			      ':preserve-dates preserve-dates
			      ':byte-size byte-size)))
		  (if (errorp opening) (*throw 'open-opt-lost (lmfs-error-string opening)))
		  (setq binp
			(selectq characters
			  (:default (not (funcall opening ':characters)))
			  (t (not characters))))
		  (setq answer
			(selectq server-protocol-version
			  (0  
			   (format nil
				   "~D ~A ~D ~S~%~A~%"
				   (funcall (funcall opening ':truename) ':version)
				   (cv-time (funcall opening ':creation-date))
				   (funcall opening ':length)
				   (funcall opening ':send-if-handles ':qfaslp)
				   (server-print-pathname (funcall opening ':truename))))
			  (1
			   (format nil
				   "~A ~D ~S ~S~%~A~%"
				   (time:print-universal-time
				     (funcall opening ':creation-date) nil)
				   (funcall opening ':length)
				   binp		;qfaslp, needed for compatibility
				   (not binp)
				   (server-print-pathname
				     (funcall opening ':truename))))))
		  (if (null direction)
		      (funcall opening ':close)
		    (let ((servi (get fh server-instance)))
		      (push opening server-openings)
		      (setf (server-dataproc-comm-binp servi) binp)
		      (setf (server-dataproc-comm-tid servi) tid)
		      (setf (server-dataproc-comm-opening servi) opening)
		      (rplaca (server-dataproc-comm-cell servi)
			      (if (eq direction ':input) 'read 'write))))
		  nil))))))
    (if (null losep)
	(format conn-stream  "~A ~A OPEN ~A" tid (or fh "") answer)
	(format conn-stream  "~A ~A ERROR ~A" tid (or fh "") losep))))

))
