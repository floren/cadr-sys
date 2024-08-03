;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 26.4
;;; Reason:
;;;    This should solve the problem that's been plaguing our backups.
;;;    
;;;    Sometimes the last file written on the tape is finished with one EOF, but
;;;    the next file fails to fit, and the double EOF is (for unknown reasons) not
;;;    being written to indicate logical EOT.  On a new tape, the remainder of the
;;;    tape appears to be blank, which you find out next time you try to verify or
;;;    restore the tape.  On COMPARE-FILES during a backup, this blows out the
;;;    entire backup.  I added error handling for that case, but not
;;;    RESTORE-FILES, because the integrity of a restoration is not affected.
;;; Reason:
;;;  This should solve the problem that's been plaguing our backups.
;;;  
;;;  Sometimes the last file written on the tape is finished with one EOF, but
;;;  the next file fails to fit, and the double EOF is (for unknown reasons) not
;;;  being written to indicate logical EOT.  On a new tape, the remainder of the
;;;  tape appears to be blank, which you find out next time you try to verify or
;;;  restore the tape.  On COMPARE-FILES during a backup, this blows out the
;;;  entire backup.  I added error handling for that case, but not
;;;  RESTORE-FILES, because the integrity of a restoration is not affected.
;;; Written 21-Sep-88 18:23:19 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Johannes Brahms from band 3
;;; with Experimental System 126.86, Experimental ZWEI 126.10, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.3, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.TAPE; USER.LISP#102 at 21-Sep-88 18:23:19
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; USER  "

(defun compare-files (&key
		      number-of-files
		      transform
		      silent
		      (error-action :return)
		      (device-spec *selected-device*)
		      (format-spec *selected-format*))
  "Compares the files on tape against files on disk.  TRANSFORM,
as in RESTORE-FILES, can be used to determine the pathnames for the files
on disk.  NUMBER-OF-FILES should be the number of files to compare
or NIL, meaning all files on the tape.  If SILENT is nil, each time a file
is compared, a message concerning the success or failure of the comparison is
printed.  ERROR-ACTION determines what to do if the comparison is unsuccessful
and must be either :RETURN or :ERROR.  If it is :ERROR, an error is signalled, 
otherwise the compare-error condition is the returned value for the file.
The return value of this function is a list each of whose elements is the
file property list of the file (successful) or the compare-error condition
\(unsuccessful)."
  (check-type error-action (member :prompt :rewrite :return))
  (using-device (device device-spec)
    (using-format (format format-spec)
      (with-device-locked device
	(let ((vals))
	  (condition-case (condition)
	      (do* ((count 1 (add1 count))
		    (val (send format
			       :compare-file device
			       :silent silent
			       :transform transform
			       :error-action error-action)
			 (send format
			       :compare-file device
			       :silent silent
			       :transform transform
			       :error-action error-action)))
		   ((and number-of-files (= count number-of-files))
		    (push val vals)
		    (reverse vals))
		(when val (push val vals)))
	    ;;Handle "blank tape" error: if no files, signal error, otherwise user may proceed.
	    (blank-tape
	     (format *standard-output* "~&** Blank Tape **~%")
	     (if (null vals)
		 (signal condition)
	       (progn
		 (cerror "Simply proceed"
			 "~A~&~
			  This may be not be an error; if this tape was made with a multi-volume set,~&~
                          or if this tape was finished with (at least one) logical EOF marker,~&~
                          it can still be used.~&"
			 (send condition :report-string))
		 (reverse vals))))
	    (logical-end-of-tape
	     (format *standard-output* "~&** End of Tape **~%")
	     (reverse vals))))))))

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#211 at 21-Sep-88 18:31:28
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "

(defun direct-read (dma-buffer record-size unit density
		    &optional function &rest args)
  (let ((max-pages (ceiling record-size 1024)))
    (tape:with-buffer-wired (dma-buffer max-pages)
      (with-buffer-mapped (dma-buffer (data-first-page) max-pages)
	(do ((count 0 (add1 count))
	     finished?)
	    (finished? (tpb-records *tpb*))
	  (execute-command %direct-read
			   unit
			   0
			   density
			   0
			   record-size
			   0)
	  (when function 
	    (lexpr-funcall function args))
	  (wait-for-command-complete)
	  (condition-case (condition)
	      (check-for-error :error-trace 'direct-read)
	    (hard-or-soft-error
	     (signal 'tape:bad-tape
		     :device-type 'tapemaster
		     :unit (get-stupid-unit *tpb*)
		     :proceed-types nil))
	    (interface-timeout
	     (incf *interface-timeout-count*)
	     (when *interface-timeout-notify*
	       (format *error-output* "~&Tapemaster: got an interface timeout"))
	     (when (= count *timeout-retry-limit*)
	       (ferror 'tape:hardware-error
		       :device-type 'tapemaster
		       :error-code (send condition :error-code)
		       :error-message
		       (format nil
			       "Tapemaster timed out ~D times in a row during DIRECT-READ."
			       count)))
	     (space unit 1 1 0))
	    ((eot filemark-encountered)
	     (signal (typecase condition
		       (eot 'tape:physical-end-of-tape)
		       (filemark-encountered 'tape:filemark-encountered))
		     :device-type 'tapemaster
		     :unit unit
		     :data-transferred 0))
	    (blank-tape
	     (signal 'tape:blank-tape :device-type 'tapemaster :unit 0))
	    (:no-error (setq finished? t))))))))

))
