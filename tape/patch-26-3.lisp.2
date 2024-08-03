;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 26.3
;;; Reason:
;;;  LMFL tape format file property lists were being written and read using
;;;  whatever readtable happened to be bound at the moment.  This could cause
;;;  possible READ-FROM-STRING errors. Fix is:
;;;  
;;;  1) In :READ-FILE-HEADER, to win at reading tape plists written in the
;;;  past, try ZL and then CL readtables (if a read error occurs).  If we
;;;  still get a read error, say so before printing the error.
;;;  
;;;  2) Always, from now on, write the plist in "standard" (currently ZL)
;;;  format.  This is best for downward-compatibility.
;;; Written 21-Sep-88 12:51:09 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Johannes Brahms from band 3
;;; with Experimental System 126.86, Experimental ZWEI 126.10, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.TAPE; LMFL-FORMAT.LISP#201 at 21-Sep-88 13:03:35
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
	     ;;All this is because file plists may have been written out in "random"
	     ;;(not carefully set) readtable:
	     (flet ((read-plist ()
		     (cond ((string-equal string "#!C" :start1 4 :end1 7)
			    (format t "[File header in T.I. format]")
			    (setq plist (read-from-string string nil :no-plist :start 7)))
			   (t
			    (setq plist (read-from-string string nil :no-plist :start 4))))))
	       (let ((*readtable* si:standard-readtable))
		 (condition-case (condition)
		     (read-plist)
		   (si:parse-error
		    (let ((*readtable* si:common-lisp-readtable))
		      (condition-case (condition)
			  (read-plist)
			(si:parse-error
			 (error "Unable to read file property list -- condition was:~&  ~S" condition)))))
		   (t))))
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

; From modified file DJ: L.TAPE; LMFL-FORMAT.LISP#201 at 21-Sep-88 13:03:38
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; LMFL-FORMAT  "


(defmethod (lmfl-format :write-file-header) (device truename attribute-list)
  (check-device device)
  (check-type truename pathname)
  (check-attribute-list attribute-list)
  (let* ((*print-base* 10.)
	 (*readtable* si:standard-readtable))
    (let* ((plist (cond ((getf attribute-list :partition)
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
	(send device :write-block header-block record-size)))))

;;;fix error messages:

(defmethod (lmfl-format :write-partition) (partition-name device unit-arg &key
					   silent end start)
  (check-type partition-name string)
  (check-type start (or null (integer 0)))
  (check-type end (or (integer 0) (member t nil)))
  (check-device device)
  (check-type unit-arg (or (integer 0) string closure))
  (si:with-decoded-disk-unit (unit unit-arg "for writing partition")
    (multiple-value-bind (beg length nil name)
	(si:find-disk-partition partition-name nil unit)
      (unless beg
	(ferror 'no-such-partition
		:host (unit-host unit)
		:disk-unit (unit-number unit)
		:partition partition-name))
      (setq start (or start beg)
	    end (cond ((null end)
		       (+ (or (si:measured-from-part-size unit name beg length) length) start))
		      ((integerp end) (+ start end))
		      (t (+ beg length))))
      (unless (and (< start end)
		   (>= start beg)
		   (<= end (+ beg length)))
	(ferror nil "Partition start or end specifications out of bounds."))
      (Setq current-plist nil)
      (using-resource (buffer si:dma-buffer (/ record-size *bytes-per-page*))
	(let ((*print-base* 10.)
	      (plist (list :partition t :name name :size (- end start)
			   :comment (si:partition-comment name unit)
			   :byte-size 16.
			   :host (send (unit-host unit) :name)
			   :host-unit (unit-number unit)
			   :creation-date (time:get-universal-time))))
	  (copy-array-contents
	    (format nil "LMFL~s" plist)
	    (si:dma-buffer-string buffer))
	  (with-device-locked device
	    (condition-case (condition)
		(send device :write-block buffer record-size)
	      (physical-end-of-tape
	       (setq tape-modified nil)
	       (Get-Next-Tape "End of tape while writing partition header. Unloading tape..." self device)
	       (send device :write-block buffer record-size)))))
	(do ((addr start)
	     (blocks-to-write (* (ceiling (- end start) 4.) 4)))
	    ((zerop blocks-to-write))
	  (with-device-locked device
	    (condition-case (condition)
		(progn
		 (setq tape-modified t)
		 (send device :write-from-disk
		       unit addr blocks-to-write record-size :silent silent))
	      (physical-end-of-tape
	       (setq tape-modified nil)
	       (Get-Next-Tape "End of tape while writing partition. Unloading tape..." self device)
	       (Let ((data-transferred (send condition :data-transferred)))
		 (Incf addr data-transferred)
		 (Decf blocks-to-write data-transferred)))
	      (:no-error
	       (setq blocks-to-write 0)
	       (condition-case ()
		   (send device :write-filemark)
		 (physical-end-of-tape))))))))))

))
