;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 22.2
;;; Reason:
;;;  In OPEN response, always print the Byte Size in decimal.
;;; Written 22-Mar-88 17:13:26 by pld at site Gigamos Cambridge
;;; while running on Djinn from band 2
;;; with Experimental System 123.216, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 21.1, microcode 1754, SDU ROM 8.



; From modified file DJ: L.FILE; SERVER.LISP#188 at 22-Mar-88 17:13:26
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
			  "~A ~D ~S ~S ~S ~D~%~A~%"
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
