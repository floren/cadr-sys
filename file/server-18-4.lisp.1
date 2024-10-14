;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 18.4
;;; Reason:
;;;  fix file server :characters nil :byte-size 8.
;;; Written 14-Apr-86 13:04:38 by GJC at site LMI Cambridge
;;; while running on Explorer One from band 2
;;; with Experimental System 110.198, Experimental Lambda-Diag 7.5, Experimental Local-File 68.7, Experimental FILE-Server 18.3, Experimental Unix-Interface 9.1, Experimental Object Lisp 3.1, microcode 1482, Lambda E for LMI.



; From file DJ: L.FILE; SERVER.LISP#183 at 14-Apr-86 13:04:39
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

; From file DJ: L.FILE; SERVER.LISP#183 at 14-Apr-86 13:04:50
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-open-for-lispm (fh filename &rest modes
				   &key &optional (direction ':input)
				   moby-mapped
				   &allow-other-keys)
  (let ((losep
	  (*catch 'open-opt-lost
	    (progn
	      (if (null fh)
		  (unless (or (eq direction ':probe) (null direction) moby-mapped)
		    ;; :direction missing or not nil.
		    (open-err "ICO F Inconsistent open options for probe opening"))

		;; FHN given. must be real read or write.
		(let* ((comdata (get fh server-instance))
		       (type (server-dataproc-comm-iotype comdata)))
		    (if (null comdata)
			(open-err "UFH F No open data channel for this file handle: " fh))
		    (unless (eq type
				(selectq direction
				  (:input 'input)
				  (:output 'output)))
		      (open-err "ICO F File handle type inconsistent with open mode"))))
	      (let ((pathname (lmfs-parse-for-server filename)))
		(if (errorp pathname) (open-err "IPS F Bad filename syntax: " pathname))
		(let ((opening
			(if moby-mapped (apply 'open pathname :error nil :direction nil modes)
			  (apply 'open pathname ':error nil modes))))
		  (if (errorp opening) (*throw 'open-opt-lost (lmfs-error-string opening)))
		  (format conn-stream "~A ~A OPEN " tid (or fh ""))
		  (format conn-stream
			  "~A ~D ~S ~S ~S ~S~%~A~%"
			  (cv-time (send opening ':creation-date))
			  (send opening ':length)
			  (send opening ':send-if-handles ':qfaslp)
			  (send opening ':characters)
			  (send opening ':get ':author)
			  (send opening ':byte-size)
			  (server-print-pathname
			    (send opening ':truename)))
		  (let ((*read-base* 10.) (*print-base* 10.)
			(*readtable* si:initial-readtable)
			(*package* si:pkg-user-package))
		    (send conn-stream ':tyo #/()
		    (dolist (op (send opening ':which-operations))
		      (unless (memq op unmentioned-stream-ops)
			(prin1 op conn-stream)
			(send conn-stream ':tyo #/sp)))
		    (send conn-stream ':tyo #/))
		    (print (or (send opening ':send-if-handles ':file-contents-plist)
			       (send opening ':send-if-handles ':file-plist))
			   conn-stream))
		  (cond (moby-mapped
			 (let ((root (apply 'open pathname :error nil modes)))
			   (cond ((not (or (consp root) (arrayp root)))
				  (open-err "ICO F MOBY-MAPPED open failed"))
				 (t (moby-mapped-open-response-5th-line conn-stream root)))))
			((or (null direction) (eq direction ':probe))
			 (send opening ':close))
			(t
			 (let ((servi (get fh server-instance)))
			   (push opening server-openings)
			   (setf (server-dataproc-comm-binp servi)
				 (COND ((send opening ':characters)
					NIL)
				       ((EQ 8 (SEND-if-handles opening :byte-size))
					NIL)
				       ('ELSE
					T)))
			   (setf (server-dataproc-comm-tid servi) tid)
			   (setf (server-dataproc-comm-opening servi) opening)
			   (rplaca (server-dataproc-comm-cell servi)
				   (selectq direction
				     (:input 'read)
				     (:output 'write)
				     (t (ferror "direction is not :input or :output")))))))
		  nil))))))
    (if losep
	(format conn-stream  "~A ~A ERROR ~A" tid (or fh "") losep))))

))
