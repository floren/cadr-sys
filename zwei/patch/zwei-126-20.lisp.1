;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.20
;;; Reason:
;;;  Introducing new Zwei REFIND FILE commands:
;;;  
;;;  Control-Meta-X Refind File
;;;  
;;;    Updates a buffer to contain current file contents, based
;;;    on modification, filename specification, and disk status
;;;    information.  Queries the user in most cases, unless nu-
;;;    meric argument is specified, in which case no querying
;;;    takes place; each buffer is updated w.r.t. disk if needed.
;;;    For Dired buffers, update the display to represent current
;;;    status of the Dired buffer's file specification.
;;;    (see Specification for more details).
;;;  
;;;  Control-Meta-X Refind All Files
;;;  
;;;    Updates all buffers as above; with numeric argument, omits
;;;    queries.
;;;  
;;;  Control-Meta-X Refind Files In System
;;;  
;;;    Updates all buffers in a specified system.
;;;  
;;;  Control-Meta-X Refind Tag Table Files
;;;  
;;;    Update all buffers in a specified tag table.
;;;  
;;;  Control-Meta-X Refind Files For Modified Buffers
;;;  
;;;    Update every buffer which has been modified.
;;;  
;;;  Control-Meta-X Refind Files For Unmodified Buffers
;;;  
;;;    Update every buffer which has not been modified.
;;;  
;;;  Control-Meta-X Refind Specified Files
;;;  
;;;    Update all buffers selected by user from a pop-up
;;;    choice menu containing all file-associated buffers.
;;;  
;;;  Control-Meta-X Revert All Buffers
;;;  
;;;    Sends a :revert message to all active buffers.
;;;  
;;;  Control-Meta-X Revert Buffer If Needed
;;;  
;;;    Sends a :revert message to the current buffer if
;;;    it has been modified.
;;;  
;;;  Control-Meta-X Revert All Buffers If Needed
;;;  
;;;    Sends :revert messages to all modified, active
;;;    buffers.
;;;  
;;;  
;;;  
;;;  Also, this patch contains numerous modularizations 
;;;  and corrections to the Source Compare commands'
;;;  interface, along with some new functions, used
;;;  in implementing the Refind commands:
;;;  
;;;    WITH-VERSION-AND-MODIFICATION-INFO
;;;    FQUERY-UNLESS-ARG
;;;    REPLACE-CURRENT-BUFFER-WITH-FILE
;;;  
;;;  (See file itself for more details on these.)
;;;  
;;;  ---saz
;;;  
;;;  ps: The new Refind commands have been extensively tested,
;;;  but will no doubt be called upon to operate in all sorts
;;;  of anomalous disk/machine/user environments.  Please re-
;;;  port carefully all pertinent details of any incorrect or
;;;  undesirable behavior witnessed as a result of using these 
;;;  commands to BUG-LISPM in the usual fashion.	      -ds
;;; Written 17-Oct-88 00:27:32 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.111, Experimental ZWEI 126.19, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.




; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:27:51
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defmacro WITH-VERSION-AND-MODIFICATION-INFO (buffer body)
  ;; binds some variables to interesting properties of a buffer and its associated file.
  ;; Used in REFIND-FILE, below.
  (declare (zwei:indentation 1 1))
  `(block no-file
     (let* ((buffer-pathname (zwei:buffer-pathname ,buffer))
	  (FILE-TRUENAME (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
     (when (NOT FILE-TRUENAME)
       (return-from no-file
	 (format t "~%No versions of ~S found on disk."
		 BUFFER-PATHNAME)))
	 ;;without the numeric version number if exists in buffer name
     (and buffer-pathname
	  ;; none of these have any relevance if there is no file associated with
	  ;; <buffer>.
	  (let* ((generic-pathname (buffer-generic-pathname ,buffer))
		 (buffer-namestring (namestring buffer-pathname))
		 (buffer-version (or (buffer-file-version-if-known ,buffer)
				     ;;this is a universal-time, given to all files, whether they have
				     ;;version numbers or not.  Here it becomes a quasi-version number.
				     (get (cadr (send buffer-pathname :directory-list nil)) :creation-date)))
		 (file-version (pathname-version buffer-pathname))
		 (file-number (if (numberp file-version)
				  file-version
				(pathname-version file-truename)))
		 ;;anything is newer than a non-existent buffer version, and
		 ;;file version numbers are assumed to be positive integers
		 (newer-version-exists (and file-number (< (or buffer-version -1) file-number)))
		 ;;(this-version-is-newest (= buffer-version file-number))
		 (buffer-modified (buffer-needs-saving-p ,buffer)))
	    ,body)))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:28:37
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defun fquery-unless-arg (return-keywords echo-strings activators fquery-string-for-user &rest string-args)
  ;;  This function returns a special symbol if *numeric-arg* has been
  ;; provided to its caller; if not, it calls FQUERY with the lists provided.
  ;; A surrounding SELECTQ can catch :GO-AHEAD or any of the <RETURN-KEYWORDS>
  ;; provided.
  ;;  Caveat: This does no checking to see if args 1-3 are of equal length --
  ;; make sure that (= (length return-keywords) (,echo
  (let ((*query-io* *standard-output*))
    (if *numeric-arg-p*
	:GO-AHEAD
      (loop for each-key in return-keywords
	    and for each-string in echo-strings
	    and for each-char in activators
	    with choice-list initially nil
	    do
	    (let* ((this-choice (list (list each-key each-string) each-char)))
	      (setq choice-list (append choice-list (list this-choice))))
	    finally
	    (return
	      (apply
		#'fquery
		(append 
		  (list (list :type :tyi :choices choice-list))
		  (list fquery-string-for-user)
		  string-args)))))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:28:54
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
			 (if (let ((*query-io* *standard-output*))
			       (yes-or-no-p "The buffer associated with file ~S,~
			   ~%read in with explicit numeric version, has been modified.~
		     	   ~%Write out the changed version to a file?~% " buffer-namestring))
			     (com-write-file))
			 (selectq (fquery-unless-arg
				   '(:FIND-LATEST :REVERT :NO-ACTION)
				   '("Replacing buffer contents with #> version" "Reverting buffer..." "No action taken.")
				   '(#\F #\R #\D) 
				   "~%The buffer ~S contains modifications to version ~S of ~S.~
			     ~%Choose an option:~
 			     ~%~% (F) Find and read into ZMACS the current > version of ~:*~S,~
			       ~%     (changing the buffer name to end with /">/" as well),~
			     ~%~% (R) Revert buffer to version ~2:*~S,~
			     ~%~% (D) Do nothing.
			     ~%~% "
			       buffer-namestring file-version generic-pathname)
			   (:FIND-LATEST
			    ;; A defun and not a defcom; no automatic redisplay makes sure user reads sees
			    ;; the echo string returned by fquery choice...
			    (replace-current-buffer-with-file buffer (merge-pathnames "#>" buffer-pathname)))
			   (:REVERT (revert-buffer buffer))
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
				   '(:FIND-LATEST :SRCCOM :SRCCOM-MERGE :NO-ACTION)
				   '("Replac buffer's contents with #> version" "Do a Source Compare"
				     "Do a Source Compare Merge" "No action taken.")
				   '(#\F #\S #\M #\D)
				   "The buffer ~S contains modifications, but a newer version of ~:*~S~
			   ~%now exists on disk.  Choose an option:~
			   ~%~%(F) Find and read into ZMACS the current > version of ~:*~S,~
			   ~%~%(S) Source Compare the current contents of this buffer with the contents of the current~
			   ~%disk file named ~:*~S,~
			   ~%~%(M) Source Compare Merge the current contents of this buffer with the contents of the current~
			   ~%disk file named ~:*~S, or~
			   ~%~%(D) Do nothing~%~%" buffer-namestring)
			   ((:GO-AHEAD :FIND-LATEST)
			    (format t "~%Updating buffer for ~S to more recent version..." buffer-pathname)
			    (revert-buffer buffer)
			    (format t "Buffer for ~S updated." buffer-pathname))
			   (:SRCCOM (let ((*numeric-arg-p* t))
				      (source-compare-changes buffer)))
			   (:SRCCOM-MERGE (com-source-compare-merge-changes))
			   (:NO-ACTION nil)))
			;;modified version of the real #> version (i.e., no newer version exists)
			(t (selectq (fquery-unless-arg
				     '(:REVERT :SRCCOM :SRCCOM-MERGE :NO-ACTION)
				     '("Revert buffer" "Do a Source Compare" "Do a Source Compare Merge" "No action taken.")
				     '(#\R #\S #\M #\D)
				     "The buffer ~S contains modifications, and no newer version of~
			     ~%~:*~S exists on disk.  Choose an option:~
			   ~%~%  (R) Revert the buffer to contain disk's latest version.~
			   ~%~%  (S) Source Compare the buffer with the contents of~
			     ~%      the file currently named ~:*~S~
			   ~%~%  (M) Source Compare Merge the buffer with the contents of~
			     ~%      the file currently named ~:*~S~
			   ~%~%  (D) Do nothing~%~%" buffer-namestring)
			     ((:GO-AHEAD :REVERT)
			      (format t "~%Reverting buffer ~S to its original state..." buffer-pathname)
			      (revert-buffer buffer)
			      (format t "Buffer for ~S reverted." buffer-pathname))
			     (:SRCCOM (let ((*numeric-arg-p* t))
					(source-compare-changes buffer)))
			     (:SRCCOM-MERGE (com-source-compare-merge-changes))
			     (:NO-ACTION nil)))))
		 ;;if we get here, then buffer not modified.
		 (t (cond ((numberp file-version)
			   (selectq (fquery-unless-arg
				     '(:FIND-LATEST :REVERT :NO-ACTION)
				     '("Replacing buffer contents with #> version" "Reverting buffer..." "No action taken.")
				     '(#\F #\R #\D) 
				     "The buffer ~S contains modifications to version ~S of ~S, but a newer version of~
			   ~%~S now exists on disk.  Choose an option:~
 			   ~%~% (F) Find and read into ZMACS the current > version of ~:*~S,~
			     ~%     (changing the buffer name to end with /">/" as well),~
			   ~%~% (R) Revert buffer to version ~2:*~S,~
			   ~%~% (D) Do nothing.~%~% "
			     buffer-namestring file-version generic-pathname)
			     ((:FIND-LATEST)
			      (replace-current-buffer-with-file buffer (merge-pathnames "#>" buffer-pathname)))
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

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:12
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-file "Revert or update the current buffer to contain the most recent version." ()
  (refind-file *interval*)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:19
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defun refind-buffer-subset (keyword)
  (mapcar
    'refind-file
    (mapcar
      #'(lambda (x)
	  (if (typep x 'zmacs-buffer)
	      x
	    (find-file-buffer x)))
      (selectq keyword
	(:TAG-TABLE
	 (cadr
	   (memq
	     'zmacs-tag-table-file-symbols
	     (plist
	       (let ((tt
		       (completing-read-from-mini-buffer
			 "Tag table whose files to refind: (Current tag table is the default)"
			 *zmacs-tag-table-alist*)))
		 (cond ((or (and (consp tt) (not (string-equal (car tt) "")))
			    (string-equal tt ""))
			(or *zmacs-current-tag-table* (barf "No tag table selected")))
		       (t (cdr tt))))))))
	(:SYSTEM
	 (si:system-source-files
	   (let ((sys (completing-read-from-mini-buffer "System whose files to refind"
							(si:all-systems-name-alist))))
	     (cond ((and (stringp sys) (string-equal sys ""))
		    (barf "No system selected"))
		   (t (cdr sys))))))
	(:SPECIFIED
	 (multiple-value-bind (buffers-selected do-it?)
	     (tv:multiple-menu-choose (subset #'(lambda (x) (buffer-pathname (cdr x)))
					      *zmacs-buffer-name-alist*)
				      "                 FILES TO REFIND: 
 (Click on files to include them in the Refind list,
  Click Do It when finished. Move off menu to abort.) ")
	   (if (null do-it?)
	       ;;user moved off menu -- refind nothing
	       (*throw 'REFIND-NOTHING dis-text)
	     ;;those buffers the user selects
	     (or buffers-selected (barf "No buffers selected")))))
	(:ALL *zmacs-buffer-list*)
	(:MODIFIED
	  (subset #'(lambda (x) (buffer-modified-p x))
		  *zmacs-buffer-list*))
	(:NOT-MODIFIED
	  (subset #'(lambda (x) (not (buffer-modified-p x)))
		  *zmacs-buffer-list*))
	(otherwise (ferror "Unknown buffer subset operation keyword: ~A" keyword))))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:40
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-all-files "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :ALL)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:50
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-files-in-system "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :SYSTEM)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:53
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-tag-table-files "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :TAG-TABLE)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:55
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-files-for-modified-buffers "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :MODIFIED)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:29:58
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-files-for-unmodified-buffers "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :NOT-MODIFIED)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:30:02
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-refind-specified-files "Revert or update all buffers to contain their most recent versions." ()
  (*catch 'REFIND-NOTHING
    (refind-buffer-subset :SPECIFIED)
    dis-all))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:32:42
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-revert-all-buffers "Send a :REVERT message to all active buffers." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (send-if-handles each-buffer :REVERT))
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:32:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defun revert-buffer-if-needed (buffer)
  (LET* ((buffer-pathname (buffer-pathname buffer))
	 (probe-stream (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
    (IF (NOT PROBE-STREAM)
        (format t "The buffer ~S is not associated with an existing file."
		buffer-pathname)
      (if (buffer-modified-p buffer)
	  (send-if-handles buffer :REVERT)))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:32:56
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-revert-buffer-if-needed "Revert any buffer with a handler the :REVERT message, if modified." ()
  (revert-buffer-if-needed *interval*)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:33:01
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defcom com-revert-all-buffers-if-needed "Revert all buffers with handlers for the :REVERT message, if modified." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (revert-buffer-if-needed each-buffer)
    dis-all))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:33:30
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defun replace-current-buffer-with-file (buffer pathname)
  (without-interrupts
      (setq *zmacs-buffer-name-alist*
	    (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
      (send buffer :kill)
      (let* ((new-buffer 
	       (load-file-into-zmacs pathname)))
	(send new-buffer :select))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:34:11
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-SOURCE-COMPARE-CHANGES 
  "Compare a buffer vs the buffer's files.  If no changes, clear buffer's modified bit.
The output goes on the screen, and also into a buffer named *Source Compare ...*.
With a numeric argument, perform source compare on the current buffer without query." ()
  (source-compare-changes
    (if *numeric-arg-p*
	*interval*
      (READ-BUFFER-NAME "Compare changes of buffer" *INTERVAL*)))
  dis-none)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:34:15
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(defun source-compare-changes (buffer)
  (LET* ((NAME (BUFFER-NAME BUFFER))
	 FILE-1 FILE-2)
    (UNWIND-PROTECT
      (PROGN
	(SETQ FILE-1 (SRCCOM::MAKE-FILE
		       :FILE-NAME NAME
		       :FILE-TYPE "Buffer"
		       :FILE-STREAM (INTERVAL-STREAM BUFFER)
		       :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
						  (SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE BUFFER)))
						SI:PKG-KEYWORD-PACKAGE)))
	(SETQ FILE-2 (SRCCOM::CREATE-FILE (BUFFER-PATHNAME BUFFER)))
	(LET* ((OUTPUT-NAME (FORMAT NIL "*Source Compare Changes of ~A*" NAME))
	       (*STANDARD-OUTPUT*
		 (MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM
		   (FORMAT NIL "*Source Compare Changes of ~A*" NAME) NIL T)))
	  (with-source-compare-parameter-handling file-1 file-2
	    (SRCCOM:DESCRIBE-SRCCOM-SOURCES FILE-1 FILE-2 *STANDARD-OUTPUT*)
	    (COND ((SRCCOM:SOURCE-COMPARE-FILES FILE-1 FILE-2 *STANDARD-OUTPUT*
						(SRCCOM::QUERY-TYPE))
		   (SETF (BUFFER-TICK BUFFER) (TICK))))	;No changes, unmodify buffer.
	    (LET ((OUTBUF (FIND-BUFFER-NAMED OUTPUT-NAME)))
	      (AND OUTBUF (SETF (GET OUTBUF 'SPECIAL-PURPOSE) :SRCCOM-OUTPUT))))
	(FORMAT T "~&Done.")))
      (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE)))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:27:37
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-SOURCE-COMPARE-MERGE-CHANGES 
       "Compare current buffer and its associated file, merging differences into the specified buffer" ()
       (LET (FILE-1 FILE-2 NAME-1 NAME-2 TYPE-1 TYPE-2 BUF-1 BUF-2 DEFAULT OUTPUT-BUFFER)
	 (UNWIND-PROTECT
	     (PROGN
	       (MULTIPLE-VALUE (FILE-1 NAME-1 TYPE-1 DEFAULT BUF-1)
		 (let ((bp (buffer-pathname *interval*)))
		   (VALUES (SRCCOM::CREATE-FILE bP)
			   bp bp "File" bp)))
	       (MULTIPLE-VALUE (FILE-2 NAME-2 TYPE-2 NIL BUF-2)
		 (LET* ((NAME (BUFFER-NAME *INTERVAL*)))
		   (VALUES (SRCCOM::MAKE-FILE
			     :FILE-NAME NAME
			     :FILE-TYPE "Buffer"
			     :FILE-STREAM (INTERVAL-STREAM *INTERVAL*)
			     :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
							(SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE *INTERVAL*)))
						      SI:PKG-KEYWORD-PACKAGE))
			   NAME
			   "BUFFER"
			   (AND (BUFFER-FILE-ID *INTERVAL*) (BUFFER-PATHNAME *INTERVAL*))
			   *INTERVAL*)))
	       (SETQ OUTPUT-BUFFER (READ-BUFFER-NAME "Put merged version into buffer"
						     (OR BUF-1 BUF-2) T))
	       (MAKE-BUFFER-CURRENT OUTPUT-BUFFER)
	       (LET ((INTERVAL (CREATE-INTERVAL))
		     (*BATCH-UNDO-SAVE* T)
		     MARKS)
		 (SETQ MARKS (SRCCOM::SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING
			       FILE-1 FILE-2 (INTERVAL-STREAM INTERVAL)))
		 (REPLACE-INTERVALS OUTPUT-BUFFER INTERVAL)
		 (BIND-MODE-LINE `("Source Compare Merge " ,NAME-1 " vs " ,NAME-2)
		   (SOURCE-COMPARE-MERGE-QUERY MARKS))
		 (FORMAT *QUERY-IO* "~&Done.  Resectionizing the buffer."))
	       (COM-REPARSE-ATTRIBUTE-LIST)
	       (SECTIONIZE-BUFFER OUTPUT-BUFFER)
	       ;; If one input is a file, and the other is the output buffer, and
	       ;; the file is the one in the buffer, then update the buffer's file-id
	       ;; as if it had been read in from the file.
	       (IF BUF-1 (PSETQ BUF-1 BUF-2 BUF-2 BUF-1
				NAME-1 NAME-2 NAME-2 NAME-1
				FILE-1 FILE-2 FILE-2 FILE-1))
	       (AND (NULL BUF-1) (EQ BUF-2 OUTPUT-BUFFER)
		    (EQ NAME-1 (BUFFER-PATHNAME BUF-2))
		    (SET-BUFFER-FILE-ID BUF-2 (SEND (SRCCOM::FILE-STREAM FILE-1) :INFO))))
	   (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
	   (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE))))
       DIS-NONE)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:37:40
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-SOURCE-COMPARE-MERGE
  "Compare two files or buffers and merge the differences into the specified buffer" ()
  (LET (FILE-1 FILE-2 NAME-1 NAME-2 TYPE-1 TYPE-2 BUF-1 BUF-2 DEFAULT OUTPUT-BUFFER)
    (UNWIND-PROTECT
      (PROGN
	(MULTIPLE-VALUE (FILE-1 NAME-1 TYPE-1 DEFAULT BUF-1)
	  (GET-BUFFER-OR-FILE-FILE "Merge"))
	(MULTIPLE-VALUE (FILE-2 NAME-2 TYPE-2 NIL BUF-2)
	  (GET-BUFFER-OR-FILE-FILE (FORMAT NIL "Merge ~A ~A with" TYPE-1 NAME-1) DEFAULT))
	(SETQ OUTPUT-BUFFER (READ-BUFFER-NAME "Put merged version into buffer"
					      (OR BUF-1 BUF-2) T))
	(MAKE-BUFFER-CURRENT OUTPUT-BUFFER)
	(LET ((INTERVAL (CREATE-INTERVAL))
	      (*BATCH-UNDO-SAVE* T)
	      MARKS)
	  (SETQ MARKS (SRCCOM::SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING
			FILE-1 FILE-2 (INTERVAL-STREAM INTERVAL)))
	  (REPLACE-INTERVALS OUTPUT-BUFFER INTERVAL)
	  (BIND-MODE-LINE `("Source Compare Merge " ,NAME-1 " vs " ,NAME-2)
	    (SOURCE-COMPARE-MERGE-QUERY MARKS))
	  (send *query-io* :clear-window)
	  (FORMAT *QUERY-IO* "~&Done.  Resectionizing the buffer."))
	(COM-REPARSE-ATTRIBUTE-LIST)
	(SECTIONIZE-BUFFER OUTPUT-BUFFER)
	;; If one input is a file, and the other is the output buffer, and
	;; the file is the one in the buffer, then update the buffer's file-id
	;; as if it had been read in from the file.
	(IF BUF-1 (PSETQ BUF-1 BUF-2 BUF-2 BUF-1
			 NAME-1 NAME-2 NAME-2 NAME-1
			 FILE-1 FILE-2 FILE-2 FILE-1))
	(AND (NULL BUF-1) (EQ BUF-2 OUTPUT-BUFFER)
	     (EQ NAME-1 (BUFFER-PATHNAME BUF-2))
	     (SET-BUFFER-FILE-ID BUF-2 (SEND (SRCCOM::FILE-STREAM FILE-1) :INFO))))
      (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE))))
  DIS-NONE)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:38:53
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN SOURCE-COMPARE-MERGE-QUERY (MARKS)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (UNWIND-PROTECT
    (DO (MARK DO-THE-REST PRESERVE-HEADINGS)
	((NULL MARKS))
      (POP MARKS MARK)
      (SETQ PRESERVE-HEADINGS NIL)
      (UNWIND-PROTECT
	(LET ((BP1 (FIRST MARK)) (BP2 (SECOND MARK)) (BP3 (THIRD MARK)) (BP4 (FOURTH MARK))
	      (BP5 (FIFTH MARK)) (BP6 (SIXTH MARK)))
	  (UNLESS DO-THE-REST
	    (SEND *QUERY-IO* :CLEAR-WINDOW)
	    (DO-NAMED ONE-MARK
		      ((REDISPLAY-P T REDISPLAY-NEXT-P)
		       (REDISPLAY-NEXT-P NIL NIL)
		       (DO-IT NIL NIL))
		      (NIL)
	      (WHEN REDISPLAY-P
		(MOVE-BP (POINT) BP1)
		(MUST-REDISPLAY *WINDOW* DIS-BPS)
		(LET ((*CENTERING-FRACTION* 0.10s0))
		  (RECENTER-WINDOW *WINDOW* :ABSOLUTE)))
	      (REDISPLAY *WINDOW* :POINT)
	      (SELECTOR
		(TYPEIN-LINE-ACTIVATE
		  (FORMAT *QUERY-IO* "~&1, 2, *, I, ~\LOZENGED-CHAR\, ~\LOZENGED-CHAR\, !, c-R or ~\LOZENGED-CHAR\: "
			  #/Space #/Rubout #/Help)
		  (CHAR-UPCASE (TYI-WITH-SCROLLING T)))
		CHAR=
		(#/C-G (BARF))
		(#/1 (SETQ DO-IT 1))
		(#/2 (SETQ DO-IT 2))
		(#/* (SETQ DO-IT '*))
		(#/I (SETQ DO-IT 'I))
		(#/MOUSE-1-1
		 (OR (LET ((BP (MOUSE-BP *WINDOW*)))
		       (SETQ DO-IT (COND ((BP-< BP BP2) NIL)
					 ((BP-< BP BP3) 1)
					 ((BP-< BP BP4) '*)
					 ((BP-< BP BP5) 2)
					 (T NIL))))
		     (BEEP)))
		(#/Space (RETURN NIL))
		(#/Rubout
		 (DELETE-INTERVAL BP2 BP5 T)
		 (RETURN))
		(#/!
		 (SEND *QUERY-IO* :CLEAR-WINDOW)
		 (DO-FOREVER
		   (SELECTOR (TYPEIN-LINE-ACTIVATE
			       (FORMAT *QUERY-IO* "~&Type 1, 2, *, ~\LOZENGED-CHAR\, or ~\LOZENGED-CHAR\: "
				       #/Rubout #/Help)
			       (CHAR-UPCASE (SEND *STANDARD-INPUT* :TYI)))
		     CHAR=
		     (#/C-G (BARF))
		     (#/1 (SETQ DO-THE-REST 1) (RETURN-FROM ONE-MARK))
		     (#/2 (SETQ DO-THE-REST 2) (RETURN-FROM ONE-MARK))
		     (#/I (SETQ DO-THE-REST 'I) (RETURN-FROM ONE-MARK))
		     (#/* (SETQ DO-THE-REST '*) (RETURN-FROM ONE-MARK))
		     (#/Rubout (progn (send *query-io* :clear-window) (RETURN)))
		     ;;Caution!  This format statement works out to exactly two lines of mini-buffer space.  Altering its
		     ;;control directives could have fatal consequences for display of this help message!
		     (#/Help
		      (FORMAT *QUERY-IO* "~&1: Insert all differences from first source only; ~
					    2: Insert all differences from second source only; ~
	 				  ~%*: Insert all differences; ~
					    I: Insert all differences, retaining source headings; ~
			     ~\LOZENGED-CHAR\: Abort"
			      #/Rubout)))))
		(#/C-R (CONTROL-R) (SETQ REDISPLAY-NEXT-P T))
		(#/Page (MUST-REDISPLAY *WINDOW* DIS-ALL))
		(#/C-L (MUST-REDISPLAY *WINDOW* (COM-RECENTER-WINDOW)))
		(#/HELP
		 ;;Caution!  This format statement works out to exactly two lines of mini-buffer space.  Altering its
		 ;;control directives could have fatal consequences for display of this help message!
		 (FORMAT *query-io* "~&1: First source; ~
				       2: Second source; ~
				       *: Both sources; ~
				       I: Both versions, with headers; ~
			~\LOZENGED-CHAR\: Use both versions w//o confirmation; ~
   		      ~%~\LOZENGED-CHAR\: Use neither version; !: Use one or both versions from now on; ~
			       control-R: Edit this buffer."
			 #/Space #/Rubout))
		(OTHERWISE (BEEP)))
	      ;; If the command specified which one we want this time, flush the rest.
	      (AND DO-IT
		   (LET (OK CONTROL-R-P)
		     (CASE DO-IT
		       (* (MULTIPLE-VALUE (OK CONTROL-R-P)
			    (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP3 BP4 BP5 BP6)))
		       (I (MULTIPLE-VALUE (OK CONTROL-R-P)
			    (SOURCE-COMPARE-MERGE-QUERY-1))
			  (AND OK (SETQ PRESERVE-HEADINGS T)))
		       (1 (MULTIPLE-VALUE (OK CONTROL-R-P)
			    (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP3 BP4
							  BP4 BP5 BP5 BP6)))
		       (2 (MULTIPLE-VALUE (OK CONTROL-R-P)
			    (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP2 BP3
							  BP3 BP4 BP5 BP6))))
		     (SETQ REDISPLAY-NEXT-P CONTROL-R-P)
		     OK)
		   (RETURN))))
	  ;; If ! was specified this time or a previous time,
	  ;; maybe flush one file's stuff.
	  (WHEN DO-THE-REST
	    (CASE DO-THE-REST
	      (* )
	      (I (SETQ PRESERVE-HEADINGS T))  ;I => don't flush the *** lines.
	      (1 (DELETE-INTERVAL BP4 BP5 T))
	      (2 (DELETE-INTERVAL BP2 BP3 T)))
	    (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
	;; Flush the *** lines (unless user typed I) and the permanent BPs.
	(FLUSH-SOURCE-COMPARE-MARK MARK PRESERVE-HEADINGS)))
    ;; Flush all remaining *** lines (unless user typed !I, which sets MARKS to NIL).
    (MAPCAR #'FLUSH-SOURCE-COMPARE-MARK MARKS)))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 17-Oct-88 00:39:16
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN SOURCE-COMPARE-MERGE-QUERY-1 (&REST START-AND-END-BPS &AUX INTS FLAG)
  ;; For each bp pair, get a copy of the text between them.  Make a list of intervals.
  (SETQ INTS (DO ((BPS START-AND-END-BPS (CDDR BPS))
		  (LIST NIL))
		 ((NULL BPS) (NREVERSE LIST))
	       (PUSH (COPY-INTERVAL (CAR BPS) (CADR BPS) T) LIST)))
  (UNWIND-PROTECT
    (PROGN
      ;; Delete the text within the bp pairs.
      (DO ((BPS START-AND-END-BPS (CDDR BPS)))
	  ((NULL BPS))
	(DELETE-INTERVAL (CAR BPS) (CADR BPS) T))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT)
      (REDISPLAY *WINDOW* :POINT)
      (SEND *QUERY-IO* :CLEAR-WINDOW)
      (DO ((CONTROL-R-P NIL)) (NIL)
	(CASE (TYPEIN-LINE-ACTIVATE
		(FORMAT *QUERY-IO* "~&Type ~\LOZENGED-CHAR\ to confirm, ~\LOZENGED-CHAR\ to abort, Control-R to edit."
			#\Space #\Rubout)
		(CHAR-UPCASE (TYI-WITH-SCROLLING T)))
	  (#/C-G (BARF))
	  ((#/SP #/MOUSE-1-1) (SETQ FLAG T) (RETURN T))
	  ((#/RUBOUT #/MOUSE-2-1) (RETURN (VALUES NIL CONTROL-R-P)))
	  (#/C-R (CONTROL-R) (SETQ CONTROL-R-P T))
	  (#/HELP (FORMAT *QUERY-IO* "~&~\LOZENGED-CHAR\ confirms this choice, ~\LOZENGED-CHAR\ aborts this choice, ~
				      ~%Control-R lets you edit this buffer." #/Space #/Rubout))
	  (OTHERWISE (BEEP)))))
    (OR FLAG
	;; If user did not confirm, reinsert the deleted text
	;; from the copies we made.
	(DO ((BPS START-AND-END-BPS (CDDR BPS))
	     (INTS INTS (CDR INTS))
	     (BP1) (BP2))
	    ((NULL BPS)
	     (MUST-REDISPLAY *WINDOW* DIS-TEXT))
	  (SETQ BP1 (CAR BPS)
		BP2 (CADR BPS))
	  (MOVE-BP BP2 BP1)
	  (LET ((TEMP-BP (COPY-BP BP1 :NORMAL)))
	    (INSERT-INTERVAL-MOVING BP2 (CAR INTS))
	    (MOVE-BP BP1 TEMP-BP)
	    (FLUSH-BP TEMP-BP))))))

))
