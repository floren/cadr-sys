;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 25.1
;;; Reason:
;;;  Fix problem that could mess up a backup; if the problem occurred on the
;;;  Nth tape, user had to redo N tapes.
;;;  
;;;  When we get physical EOT (end-of-tape) while writing files, we attempt
;;;  to reverse position to beginning of last file and mark logical EOT.  If
;;;  we get another EOT error here (not while finishing the tape, but while
;;;  reversing position and spacking forward), there isn't room for logical
;;;  EOT, but the tape is still good!  User might get a warning when
;;;  restoring from this tape that physical EOT was encountered.
;;; Written 26-Aug-88 12:19:11 by fileserver at site Gigamos Cambridge
;;; while running on Djinn from band 2
;;; with Experimental System 126.66, ZWEI 125.19, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Lambda-Diag 17.0, Experimental Tape 25.0, microcode 1762, SDU ROM 102, don't patch outer systems!!!.



; From modified file DJ: L.TAPE; USER.LISP#101 at 26-Aug-88 12:19:14
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
			 ;;;Got physical EOT (end-of-tape).  Attempt to mark logical EOT.
			 ;;;If we get another EOT error here, there wasn't room for logical EOT,
			 ;;;but the tape is still good!
			 (condition-case (condition)
			     (progn
			      (send format :beginning-of-file device)
			      (send format :finish-tape device))
			   (physical-end-of-tape
			    (format t "~&Unable to mark logical end of tape - proceeding anyway."))))
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
