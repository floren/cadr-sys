;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 22.2
;;; Reason:
;;;  Fix error-reporting problems with tape comparison functions.
;;;  
;;;  1. make sure all flavors of comparison errors can handle :source-file method.
;;;  2. fix up error :report method messages
;;;  3. where tape s/w was constructing error message by sending :source-file message to
;;;  condition instance, use :report method instead
;;; Written 26-Apr-88 13:41:41 by keith (Keith M. Corbett) at site LMI
;;; while running on Opus from band 3
;;; with Experimental System 123.235, Experimental Local-File 73.4, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 9.0, Experimental Tape 22.1, Experimental Serial Internet Protocol (TCP over RS232) 1.0, Experimental Serial Internet Protocol 2.0, microcode 1755, SDU Boot Tape 3.14, SDU ROM 8, Beta II/site/patch.



; From modified file OPUS: L.TAPE; ERROR.LISP#48 at 26-Apr-88 13:52:53
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (compare-error :report) (stream)
  (format stream "File comparison error for file: \"~A\"" source-file))

))

; From modified file OPUS: L.TAPE; ERROR.LISP#48 at 26-Apr-88 13:52:55
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (compare-source-not-found :report) (stream)
  (format stream "File \"~A\" not found for comparison." source-file))

))

; From modified file OPUS: L.TAPE; ERROR.LISP#48 at 26-Apr-88 13:52:56
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (compare-source-changed :source-file) ()
  (car source-plist))

))

; From modified file OPUS: L.TAPE; ERROR.LISP#48 at 26-Apr-88 13:52:58
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (compare-source-changed :report) (stream)
  (format stream
	  "File \"~A\" for comparing seems to have changed since writing."
	  (send self :source-file)))

))

; From modified file OPUS: L.TAPE; USER.LISP#97 at 26-Apr-88 13:53:10
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; USER  "


(defun backup-files (file-list host &key
		     (set-backup-bits t)
		     (compare t)
		     (tape-info-function 'prompt-for-tape-info)
		     (device-spec *selected-device*)
		     (format-spec *selected-format*))
  "This backs up the files in FILE-LIST to tape.  Each element in
the list must be a file property list.  The files backed up will
be compared and/or have their backup bits set as specified by the
arguments.  TAPE-INFO-FUNCTION takes no arguments and should return
two values for each tape mounted: the tape name and the pathname 
for the log.  It is called for each tape in the dump."
  (using-device (device device-spec)
    (using-format (format format-spec)
      (with-device-locked device
	(format t "~&Backing-up ~D files: ~:D total bytes"
		(length file-list)
		(let ((num 0))
		  (dolist (f file-list num)
		    (incf num (* (get f :length-in-bytes) (/ (file-byte-size f) 8))))))
	(do ((time (time:get-universal-time))
	     (files-to-backup file-list)
	     failed-files)
	    ((null files-to-backup)
	     (when failed-files
	       (format t "~&*** ~D files failed during access ***~%" (length failed-files)))
	     (format t "~&~%*** Backup Finished ***~%")
	     failed-files)
	  (multiple-value-bind (tape-name log-file)
	      (funcall (or tape-info-function 'prompt-for-tape-info) host t)
	    (do* ((files files-to-backup (cdr files))
		  (file (car files) (car files))
		  bad-files
		  files-to-log
		  new-tape)
		 ((or (null file) new-tape)
		  (if (not new-tape)
		      (format t "~&Last file written to tape.~%")
		    (let ((condition (first new-tape))
			  (file-to-retry (second new-tape)))
		      (format t "~&End of tape encountered writing ~A.  ~%~
                                   Fixing last file on tape - "
			      (car file-to-retry))
		      (push file-to-retry files) ;put this file back on list
		      (typecase condition
			(end-of-tape-writing-file
			 (send format :beginning-of-file device)
			 (send format :finish-tape device))
			(end-of-tape-writing-header))
		      (format t "done.~%")))
		  (send format :finish-tape device)
		  (when compare
		    (format t "~&Rewinding to compare ... ")
		    (send format :rewind device)
		    (format t "done.~2%Comparing files:~%")
		    (do* ((vl (compare-files :format-spec format
					     :device-spec device)
			      (cdr vl))
			  (val (car vl) (car vl))
			  (count 0 (add1 count)))
			 ((null vl))
		      (when (errorp val)
			(push val bad-files)
			(delq (nth count files-to-log) files-to-log)))
		    (if (null bad-files)
			(format t "~&All files compared were equal.")
		      (progn
			(format t "~&*** Not all files were equal (bad files follow) ***")
			;; +++ compare returns condition objects (at least in the case of "file not found" +++
			(dolist (condition bad-files)
			  (format t "~&~10@t~A~%" (send condition :report nil)))
			(format t "~&Make a note of these files and dump them again.~%")
			(y-or-n-p "Continue? "))))
		  (setq files-to-backup files)
		  (when files-to-log
		    (format t "~&Logging files - ")
		    (log-files files-to-log
			       host log-file `(:tape ,tape-name)
			       (type-of format) user-id time)
		    (format t "done.~%"))
		  (when (and set-backup-bits files-to-log)
		    (format t "~&Setting backup bits ... ")
		    (set-backup-bits files-to-log)
		    (format t "done.~%"))
		  (when files-to-backup
		    (format t "~&Unloading tape ... ")
		    (unload)
		    (prompt-for-new-tape format device))
		  t)
	      (condition-case (condition)
		  (send format :write-file device file :end-of-tape-action :error)
		((end-of-tape-writing-header end-of-tape-writing-file)
		 (setq new-tape (list condition file)))
		(fs:file-operation-failure
		 (format t "~&*** Failed writing file: \"~s\". ***" (car file))
		 (push (cons (car file) condition) failed-files))
		(:no-error (push file files-to-log))))))))))


))

; From modified file OPUS: L.TAPE; LMFL-FORMAT.LISP#198 at 26-Apr-88 13:53:16
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; LMFL-FORMAT  "


(defmethod (lmfl-format :compare-file) (device &key transform silent (error-action :return))
  (check-device device)
  (let* ((pl (send self :read-file-header device))
	 (max-chunk (send device :optimal-chunk-size record-size))
	 (pathname (if transform
		       (process-transform transform pl)
		     (car pl)))
	 (length-in-bytes (or (get pl :length-in-bytes)
			      (get pl :length)
			      (get pl :size)	; for partitions
			      (ferror nil "length in bytes is NIL!")))
	 (byte-factor (/ (file-byte-size pl) 8))
	 number-of-chunks
	 last-chunk-size)
    (setq current-plist nil)
    (if (get pl :partition)
	(send self :compare-partition device pl silent)
      (if (not (condition-case (cond)
		   (probef pathname)
		 (fs:directory-not-found)))
	  (let ((cond (make-condition 'compare-source-not-found :source-file pathname)))
	    (send self :space-to-end-of-this-file device pl 0)
	    (case error-action
	      (:return
	       (unless silent
		 (warn (send cond :report nil)))
	       cond)
	      (:error (signal-condition cond))))
	(block really-compare
	  (multiple-value-bind (a b)
	      (floor (* length-in-bytes byte-factor) max-chunk)
	    (setq number-of-chunks (if (zerop b) a (add1 a))
		  last-chunk-size (if (zerop b) max-chunk b))
	    (with-open-file (f pathname
			       :direction :input
			       :characters :default)
	      (using-resource (fbuffer si:dma-buffer (/ max-chunk *bytes-per-page*))
		(using-resource (tbuffer si:dma-buffer (/ max-chunk *bytes-per-page*))
		  (unless silent
		    (format t "~&Comparing \"~a\" ... " pathname))
		  (unless (and (= length-in-bytes
				  (or (get f :length-in-bytes)
				      (get f :length)
				      (ferror nil "file's length in bytes is NIL!")))
			       (= (file-byte-size pl) (file-byte-size f))
			       (= (get pl :creation-date) (get f :creation-date))
			       (eq (get pl :characters) (get f :characters)))
		    (let ((cond (make-condition 'compare-source-changed
						:source-plist (cons (send f :truename)
								    (plist f))
						:file-plist pl)))
		      (unless silent
			(format t "[*** Not Compared ***]"))

		      (send self :space-to-end-of-this-file device pl 0)
		      (case error-action
			(:return (return-from really-compare cond))
			(:error (signal-condition cond)))))
		  (when (zerop length-in-bytes)
		    (Setq current-plist nil)
		    (condition-case (condition)
			(send device :space 1)
		      ((filemark-encountered physical-end-of-tape)))
		    (format t "[Zero Length]")
		    (return-from really-compare pl))
		  (do* ((count 0 (add1 count))
			(records-compared 0)
			(bytes-this-time	;note these are 8-bit bytes
			  (if (= count (sub1 number-of-chunks)) last-chunk-size max-chunk)
			  (if (= count (sub1 number-of-chunks)) last-chunk-size max-chunk))
			(farray (case byte-factor
				  (1 (si:dma-buffer-8b fbuffer))
				  (2 (si:dma-buffer-16b fbuffer))))
			(fstring (si:dma-buffer-string fbuffer))
			(tstring (si:dma-buffer-string tbuffer))
			unequalp)
		       ((or (= count number-of-chunks) unequalp)
			(ZL:if unequalp
			    (let ((cond (make-condition 'compare-error
							:source-file (send f :truename)
							:file-plist pl)))
			      (unless silent
				(format t "[*** Unequal ***]"))
			      (ecase error-action
				(:return
				 (send self :space-to-end-of-this-file device pl records-compared)
				 cond)
				(:error (signal-condition cond))))
			  (unless silent
			    (format t "[Equal]"))
			  (Setq current-plist nil)
			  (condition-case (condition)
			      (send device :space 1)
			    ((filemark-encountered physical-end-of-tape)))
			  pl))
		    (send f :string-in nil farray 0 (/ bytes-this-time byte-factor))
		    (Setq current-plist nil)
		    (condition-case (condition)
			(send device :read-array
			      tstring (ceiling bytes-this-time record-size) record-size)
		      (physical-end-of-tape
		       (let* ((records-read (send condition :data-transferred))
			      (bytes-left (- bytes-this-time (* records-read record-size))))
			 (send self :find-continuation-tape device pl)
			 (ZL:if (string-not-equal fstring tstring
					       :end1 (- bytes-this-time bytes-left)
					       :end2 (- bytes-this-time bytes-left))
			     (setq unequalp t
				   records-compared (+ records-compared records-read))
			   (send device :read-array
				 tstring (ceiling bytes-left record-size) record-size)
			   (unless (string-equal fstring tstring
						 :Start1 (- bytes-this-time bytes-left)
						 :end1 bytes-this-time
						 :end2 bytes-left)
			     (setq unequalp t))
			   (incf records-compared (ceiling bytes-this-time record-size)))))
		      (:no-error
		       (unless (string-equal fstring tstring
					     :start1 0
					     :end1 bytes-this-time
					     :Start2 0
					     :end2 bytes-this-time)
			 (setq unequalp t
			       records-compared
			       (ceiling bytes-this-time record-size)))))))))))))))


))
