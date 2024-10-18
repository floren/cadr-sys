;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.29
;;; Reason:
;;;  Small bug in order of response strings for Meta-X Refind File;
;;;  user now sees the string corresponding to his choice of action
;;;  in all cases.
;;; Written 8-Nov-88 11:00:48 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.141, Experimental ZWEI 126.28, Experimental ZMail 74.15, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, ) (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: L.ZWEI; ZMACS.LISP#593 at 8-Nov-88 11:00:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defun REFIND-FILE (&optional (buffer (read-buffer-name "File to refind: "
						        *interval*))
		    &aux buffer-pathname)
  ;;  This will give users choices unless they specify (by using com-refind-file with
  ;; an explicit numeric arg) that they want REFIND-FILE to act independently of user
  ;; input.
  (if buffer (setq buffer-pathname (buffer-pathname buffer)))
  (cond ((null buffer)
	 (load-file-into-zmacs
	   (READ-DEFAULTED-PATHNAME "Find file:"
				    (PATHNAME-DEFAULTS) NIL NIL :NEW-OK)))
	((not buffer-pathname)
	 (format t "~%The buffer ~A is not associated with a currently existing file." buffer))
	((eq (buffer-saved-major-mode buffer) 'dired-mode)
	 (without-interrupts
	   (format t "~%Updating Dired display of ~A..." buffer-pathname)
	   (directory-edit-revert buffer)
	   (setq *zmacs-buffer-name-alist*
		 (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
	   (zl:format t "Dired display of ~A updated." buffer-pathname)))
	(t 
	 (with-version-and-modification-info buffer
	   (cond (buffer-modified
		  (cond ((numberp file-version)
			 (selectq (fquery-unless-arg
				   '(:FIND-LATEST :REVERT :NO-ACTION :WRITE)
				   '("Replacing buffer contents with #> version" "Reverting buffer..."
				     "Write buffer contents to disk." "No action taken.")
				   '(#\F #\R #\D #\W) 
				   "~%The buffer ~S contains modifications to version ~S of ~S.~
			     ~%Choose an option:~
 			     ~%~% (F) Find and read into ZMACS the current > version of ~:*~S,~
			       ~%     (changing the buffer name to end with /">/" as well),~
			     ~%~% (R) Revert buffer to version ~2:*~S,~
			     ~%~% (S) Write buffer contents to disk,~
			     ~%~% (D) Do nothing.
			     ~%~% "
			       buffer-namestring file-version generic-pathname)
			   (:FIND-LATEST
			    ;; A defun and not a defcom; no automatic redisplay makes sure user reads sees
			    ;; the echo string returned by fquery choice...
			    (replace-current-buffer-with-file buffer (merge-pathnames "#>" buffer-pathname)))
			   (:REVERT (revert-buffer buffer))
			   (:WRITE (com-write-file))
			   (:NO-ACTION nil)
			   (:GO-AHEAD (barf "~S is explicitly numeric.  Use Meta-x Revert Buffer or Meta-x Not Modified"))))
			;; Non-numeric version (unix file, #> file, etc...)
			;; Too bad -- except for the disk status user message, this clause and the
			;; (t ... ) clause are IDENTICAL, thanks to revert-buffer's behaviour in cases
			;; of non-numeric-version-numbered files.  The programmer who feels that telling the
			;; user about whether or not a new version exists on disk is unimportant
			;; could merge these two clauses (after unifying the text displayed).   -ds
			(newer-version-exists
			 (selectq (fquery-unless-arg
				   '(:FIND-LATEST :WRITE :SRCCOM :SRCCOM-MERGE :NO-ACTION)
				   '("Replace buffer's contents with #> version" "Write buffer contents to disk."
				     "Do a Source Compare" "Do a Source Compare Merge" "No action taken.")
				   '(#\F #\W #\S #\M #\D)
				   "The buffer ~S contains modifications, but a newer version of ~:*~S~
			   ~%now exists on disk.  Choose an option:~
			   ~%~%(F) Find and read into ZMACS the current > version of ~:*~S,~
			   ~%~%(W) Write buffer contents to disk,~
			   ~%~%(S) Source Compare the current contents of this buffer with the contents of the current~
			   ~%disk file named ~:*~S,~
			   ~%~%(M) Source Compare Merge the current contents of this buffer with the contents of the current~
			   ~%disk file named ~:*~S, or~
			   ~%~%(D) Do nothing~%~%" buffer-namestring)
			   ((:GO-AHEAD :FIND-LATEST)
			    (format t "~%Updating buffer for ~S to more recent version..." buffer-pathname)
			    (revert-buffer buffer)
			    (format t "Buffer for ~S updated." buffer-pathname))
			   (:WRITE (com-write-file))
			   (:SRCCOM (let ((*numeric-arg-p* t))
				      (source-compare-changes buffer)))
			   (:SRCCOM-MERGE (com-source-compare-merge-changes))
			   (:NO-ACTION nil)))
			;;modified version of the real #> version (i.e., no newer version exists)
			(t (selectq (fquery-unless-arg
				     '(:REVERT :WRITE :SRCCOM :SRCCOM-MERGE :NO-ACTION)
				     '("Revert buffer" "Write buffer contents to disk" "Do a Source Compare"
				       "Do a Source Compare Merge" "No action taken.")
				     '(#\R #\W #\S #\M #\D)
				     "The buffer ~S contains modifications, and no newer version of~
			     ~%~:*~S exists on disk.  Choose an option:~
			   ~%~%  (R) Revert the buffer to contain disk's latest version,~
			   ~%~%  (W) Write buffer contents to disk,~
			   ~%~%  (S) Source Compare the buffer with the contents of~
			     ~%      the file currently named ~:*~S,~
			   ~%~%  (M) Source Compare Merge the buffer with the contents of~
			     ~%      the file currently named ~:*~S, or~
			   ~%~%  (D) Do nothing.~%~%" buffer-namestring)
			     ((:GO-AHEAD :REVERT)
			      (format t "~%Reverting buffer ~S to its original state..." buffer-pathname)
			      (revert-buffer buffer)
			      (format t "Buffer for ~S reverted." buffer-pathname))
			     (:WRITE (com-write-file))
			     (:SRCCOM (let ((*numeric-arg-p* t))
					(source-compare-changes buffer)))
			     (:SRCCOM-MERGE (com-source-compare-merge-changes))
			     (:NO-ACTION nil)))))
		 ;;if we get here, then buffer not modified.
		 (t (cond ((numberp file-version)
			   (selectq (fquery-unless-arg
				     '(:FIND-LATEST :WRITE :REVERT :NO-ACTION)
				     '("Replacing buffer contents with #> version" "Writing buffer contents to disk..."
				       "Reverting buffer..." "No action taken.")
				     '(#\F #\W #\R #\D) 
				     "The buffer ~S contains modifications to version ~S of ~S, but a newer version of~
			   ~%~S now exists on disk.  Choose an option:~
 			   ~%~% (F) Find and read into ZMACS the current > version of ~:*~S,~
			     ~%     (changing the buffer name to end with /">/" as well),~
			   ~%~% (W) Write buffer contents to disk,~
			   ~%~% (R) Revert buffer to version ~2:*~S, or~
			   ~%~% (D) Do nothing.~%~% "
			     buffer-namestring file-version generic-pathname)
			     ((:FIND-LATEST)
			      (replace-current-buffer-with-file buffer (merge-pathnames "#>" buffer-pathname)))
			     (:WRITE (com-write-file))
			     (:REVERT (revert-buffer buffer))
			     (:NO-ACTION nil)
			     (:GO-AHEAD (barf "~S is explicitly numeric.  Use Meta-x Revert Buffer or Meta-x Not Modified"
					      buffer-pathname))))
			  ;; Non-numeric version (unix file, #> file, etc...)
			  (newer-version-exists
			   (format t "~%Reverting buffer ~S" buffer-namestring)
			   (revert-buffer buffer))
			  (t (format t "~%No updating required for ~S." buffer-namestring)))))))))

))
