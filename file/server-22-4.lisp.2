;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 22.4
;;; Reason:
;;;  (Patch is from Jim O'Dell when he was at Los Alamos:)
;;;  
;;;     When communicating from a symbolics to a lambda, the symbolics side
;;;     often does a  CHAOS:OPEN and passes an :ESTIMATED-SIZE keyword to 
;;;     the lambda, which is not recognized.
;;;     The lambda side should parse this in FS:FILE-SERVER-OPEN, along with the
;;;     other options,  and just ignore it.
;;;  Done.
;;; Written 4-May-88 17:47:23 by pld at site Gigamos Cambridge
;;; while running on Azathoth from band 3
;;; with Experimental System 123.249, Experimental Local-File 73.5, Experimental FILE-Server 22.3, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 22.4, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8, the old ones.



; From modified file DJ: L.FILE; SERVER.LISP#192 at 4-May-88 17:47:23
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

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
			(:probe-link
			 (setq direction nil directionp t inhibit-links t))
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
			(:ESTIMATED-SIZE (pop olist))	;COMES FROM SYMBOLICS
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
			  (:default (not (send opening ':characters)))
			  (t (not characters))))
		  (setq answer
			(selectq server-protocol-version
			  (0  
			   (format nil
				   "~D ~A ~D ~S~%~A~%"
				   (send (send opening ':truename) ':version)
				   (cv-time (send opening ':creation-date))
				   (send opening ':length)
				   (send opening ':send-if-handles ':qfaslp)
				   (server-print-pathname (send opening ':truename))))
			  (1
			   (format nil
				   "~A ~D ~S ~S~%~A~%"
				   (cv-time (send opening ':creation-date))
				   (send opening ':length)
				   binp		;qfaslp, needed for compatibility
				   (not binp)
				   (server-print-pathname
				     (send opening ':truename))))))
		  (if (null direction)
		      (send opening ':close)
		    (let ((servi (get fh server-instance)))
		      (push opening server-openings)
		      (setf (server-dataproc-comm-binp servi)
			    ;; BINP used to mean QFASLP, so people did not expect
			    ;; it to work for :BYTE-SIZE 8 files. But now it means binaryp
			    ;; but is doing the wrong thing for non-character binary files.
			    ;; So we arrange to set keep this BINP flag NIL here if our opening is
			    ;; an 8-bit file.
			    (COND ((NOT BINP) NIL)
				  ((OR (EQ 8 (SEND-IF-HANDLES OPENING :BYTE-SIZE))
				       (EQ 8 BYTE-SIZE))
				   NIL)
				  ('ELSE
				   T)))
		      (setf (server-dataproc-comm-tid servi) tid)
		      (setf (server-dataproc-comm-opening servi) opening)
		      (rplaca (server-dataproc-comm-cell servi)
			      (if (eq direction ':input) 'read 'write))))
		  nil))))))
    (if (null losep)
	(format conn-stream  "~A ~A OPEN ~A" tid (or fh "") answer)
	(format conn-stream  "~A ~A ERROR ~A" tid (or fh "") losep))))

))
