;;;-*- Mode:LISP; Package:ZWEI; Base:8; Readtable:T -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Directory editor

;;; A line starting with "D" means a file to be deleted.
;;; A line starting with "d" means a file ALREADY deleted.
;;; A line starting with "U" means a deleted file to be undeleted.
;;; Lines can also start with P, F or A
;;;   to print, read into ZWEI or apply a function on the file.
;;; But this can only be so for files that are not already deleted.

(DEFVAR *DIRED-PATHNAME-NAME* NIL
  "In ZMACS, when a DIRED buffer is current, this is set to a string
describing what directory that buffer is editing.")

(DEFSUBST DIRED-LINE-PATHNAME (LINE)
  "Return the pathname of the file that LINE describes, or NIL if non-file line."
  (GETF (LINE-PLIST LINE) :PATHNAME))

(DEFUN DIRED-LINE-PATHNAME-OR-BARF (LINE)
  "Return the pathname of the file that LINE describes, or get an error."
  (OR (DIRED-LINE-PATHNAME LINE)
      (BARF "The current line does not describe a file.")))

(DEFSUBST DIRED-LINE-LEVEL (LINE)
  "Return the level in subdirectories of the file that LINE describes.
The level is zero for the topmost files in the buffer
and increases as we go down the directory tree structure."
  (GETF (LINE-PLIST LINE) 'LEVEL))

(DEFUN DIRED-MAP-OVER-LINES (N-TIMES FUNCTION)
  "Operate on N-TIMES successive file-describing lines with FUNCTION.
FUNCTION receives one arg, the line to operate on.
N-TIMES may be negative, to move upward."
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (LET ((BP (BEG-LINE (POINT)))
	  (BOTTOM (INTERVAL-LAST-BP *INTERVAL*)))
      (DOTIMES (I (ABS N-TIMES))
	(COND ((MINUSP N-TIMES)
	       (IF (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
		   (RETURN))
	       (SETQ BP (BEG-LINE BP -1 T))))
	(AND (BP-= BP BOTTOM) (RETURN))
	(IF (DIRED-LINE-PATHNAME (BP-LINE BP))
	    (FUNCALL FUNCTION (BP-LINE BP)))
	(AND (PLUSP N-TIMES) (SETQ BP (BEG-LINE BP +1 T))))
      (MOVE-BP (POINT) BP))
    DIS-TEXT))

(DEFUN DIRED-PATHNAME-INSERTION-BP (PATHNAME)
  "Return the BP at which a line about PATHNAME would be inserted.
We take account of alphabetical order of name, type and version,
and find the correct directory.  Value is NIL if PATHNAME's directory
does not have its contents listed in the DIRED buffer.
Second value is the proper level for a line inserted for this pathname
/(or NIL if the first value is NIL)."
  (DECLARE (VALUES BP LEVEL))
  (LET ((DIR-LINE (DIRED-PATHNAME-DIRECTORY-LINE PATHNAME)))
    (AND DIR-LINE
	 (LET ((LEVEL (OR (DIRED-LINE-LEVEL DIR-LINE) -1)))
	   (DO ((LINE (LINE-NEXT DIR-LINE) (LINE-NEXT LINE)))
	       ((NULL LINE))
	     (IF (OR (NULL (DIRED-LINE-PATHNAME LINE))
		     (< (DIRED-LINE-LEVEL LINE) LEVEL)
		     (NOT (FS:PATHNAME-LESSP (DIRED-LINE-PATHNAME LINE) PATHNAME)))
		 (RETURN (CREATE-BP LINE 0) (1+ LEVEL))))))))

(DEFSUBST DIRED-BUFFER-DIRECTORY-PATHNAME (BUFFER)
  "Return the pathname specifying the directory on which DIRED was invoked in this buffer."
  (GETF (LINE-PLIST (BP-LINE (INTERVAL-FIRST-BP BUFFER))) :DIRECTORY))

(DEFUN DIRED-PATHNAME-DIRECTORY-LINE (PATHNAME &AUX DIR-PATHNAME)
  "Return the line in the dired buffer for PATHNAME's directory."
  (SETQ DIR-PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME NIL
			   			  :TYPE NIL
						  :VERSION NIL))
  (IF (EQ (SEND (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*) :NEW-PATHNAME :NAME NIL
									   :TYPE NIL
									   :VERSION NIL)
	  DIR-PATHNAME)
      (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
    (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE)))
	((NULL LINE))
      (AND (GETF (LINE-PLIST LINE) :DIRECTORY)
	   (DIRED-LINE-PATHNAME LINE)
	   (EQ (SEND (DIRED-LINE-PATHNAME LINE) :PATHNAME-AS-DIRECTORY)
	       DIR-PATHNAME)
	   (RETURN (AND (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT)
			LINE))))))

(DEFMAJOR COM-DIRED-MODE DIRED-MODE "Dired" "Setup for editing a directory" ()
  (PROGN (LET ((PATHNAME (SEND *INTERVAL* :PATHNAME)))
	   (SETQ *DIRED-PATHNAME-NAME* (AND PATHNAME (STRING PATHNAME)))))
  (SET-COMTAB *MODE-COMTAB* '(#/SP COM-DOWN-REAL-LINE
			      #/! COM-DIRED-NEXT-UNDUMPED
			      #/@ COM-DIRED-COMPLEMENT-DONT-DELETE
			      #/# COM-DIRED-COMPLEMENT-DONT-SUPERSEDE
			      #/$ COM-DIRED-COMPLEMENT-NO-REAP-FLAG
			      #/. COM-DIRED-CHANGE-FILE-PROPERTIES
			      #/, COM-DIRED-PRINT-FILE-ATTRIBUTES
			      #/= COM-DIRED-SRCCOM
			      #/? COM-DIRED-DOCUMENTATION
			      #/HELP COM-DIRED-DOCUMENTATION
			      #/A COM-DIRED-APPLY-FUNCTION
			      #/a (0 #/A)
			      #/C COM-DIRED-COPY
			      #/c (0 #/C)
			      #/D COM-DIRED-DELETE
			      #/d (0 #/D)
			      #/C-D COM-DIRED-DELETE
			      #/E COM-DIRED-EDIT-FILE
			      #/e (0 #/E)
			      #/C-SH-E COM-DIRED-EDIT-FILE-TWO-WINDOWS
			      #/F COM-DIRED-FIND-FILE
			      #/f (0 #/F)
			      #/H COM-DIRED-AUTOMATIC
			      #/h (0 #/H)
			      #/K COM-DIRED-DELETE
			      #/k (0 #/K)
			      #/C-K COM-DIRED-DELETE
			      #/L COM-DIRED-LOAD-FILE
			      #/l (0 #/L)
			      #/N COM-DIRED-NEXT-HOG
			      #/n (0 #/N)
			      #/P COM-DIRED-PRINT-FILE
			      #/p (0 #/P)
			      #/Q COM-DIRED-EXIT
			      #/q (0 #/Q)
			      #/R COM-DIRED-RENAME
			      #/r (0 #/R)
			      #/S COM-DIRED-SUBDIRECTORY
			      #/s (0 #/S)
			      #/U COM-DIRED-UNDELETE
			      #/u (0 #/U)
			      #/V COM-DIRED-VIEW-FILE
			      #/v (0 #/V)
			      #/X COM-DIRED-EXECUTE
			      #/x (0 #/X)
			      #/1 COM-NUMBERS
			      #/2 COM-NUMBERS
			      #/3 COM-NUMBERS
			      #/4 COM-NUMBERS
			      #/5 COM-NUMBERS
			      #/6 COM-NUMBERS
			      #/7 COM-NUMBERS
			      #/8 COM-NUMBERS
			      #/9 COM-NUMBERS
			      #/0 COM-NUMBERS
			      #/< COM-DIRED-EDIT-SUPERIOR-DIRECTORY
			      #/RUBOUT COM-DIRED-REVERSE-UNDELETE
			      #/ABORT COM-DIRED-ABORT
			      #/END COM-DIRED-EXIT
			      #/MOUSE-3-1 COM-DIRED-MOUSE-MENU)
	      '(("Automatic" . COM-DIRED-AUTOMATIC)
		("Automatic All Files" . COM-DIRED-AUTOMATIC-ALL)
		("Sort Increasing Reference Date"
		 . COM-DIRED-SORT-BY-INCREASING-REFERENCE-DATE)
		("Sort Decreasing Reference Date"
		 . COM-DIRED-SORT-BY-DECREASING-REFERENCE-DATE)
		("Sort Increasing Creation Date"
		 . COM-DIRED-SORT-BY-INCREASING-CREATION-DATE)
		("Sort Decreasing Creation Date"
		 . COM-DIRED-SORT-BY-DECREASING-CREATION-DATE)
		("Sort Increasing File Name"
		 . COM-DIRED-SORT-BY-INCREASING-FILE-NAME)
		("Sort Decreasing File Name"
		 . COM-DIRED-SORT-BY-DECREASING-FILE-NAME)
		("Sort Increasing Size"
		 . COM-DIRED-SORT-BY-INCREASING-SIZE)
		("Sort Decreasing Size"
		 . COM-DIRED-SORT-BY-DECREASING-SIZE)))
  (SET-MODE-LINE-LIST (APPEND (MODE-LINE-LIST) '("  " *DIRED-PATHNAME-NAME*
						 "     (Q to exit)"))))

(DEFUN (DIRED-MODE PATHNAME-DEFAULTING-FUNCTION) (IGNORE BUFFER)
  (AND (EQ BUFFER *INTERVAL*)
       (DIRED-LINE-PATHNAME (BP-LINE (POINT)))))

(DEFCOM COM-DIRED "Edit a directory.
For documentation on the Dired commands, enter Dired and type question-mark." ()
  (KILL-NEW-BUFFER-ON-ABORT (*INTERVAL*)
    (DIRECTORY-EDIT (READ-DIRECTORY-NAME "Edit directory" (DEFAULT-PATHNAME)))))

(DEFCOM COM-R-DIRED "Edit directory for current file.
With no argument, edits the directory containing the file in the current buffer.
With an argument of 1, shows only files with the same first name as the current file.
With an argument of 4, asks for a directory name.
For documentation on the Dired commands, enter Dired and type question-mark." ()
  (LET ((PATHNAME (SEND (DEFAULT-PATHNAME) :NEW-PATHNAME :TYPE :WILD
							 :VERSION :WILD)))
    (KILL-NEW-BUFFER-ON-ABORT (*INTERVAL*)
      (COND ((NOT *NUMERIC-ARG-P*)
	     (DIRECTORY-EDIT (SEND PATHNAME :NEW-NAME :WILD)))
	    ((= *NUMERIC-ARG* 1)
	     (DIRECTORY-EDIT PATHNAME))
	    (T (COM-DIRED))))))

;;; Here is the actual directory editor
(DEFUN DIRECTORY-EDIT (PATHNAME &OPTIONAL (SELECT-P T))
  "Create a ZMACS buffer editing the directory PATHNAME, and select it unless inhibited.
The buffer is selected unless SELECT-P is NIL."
  (LET* ((DIRNAME (SEND PATHNAME :STRING-FOR-DIRECTORY))
	 (INTERVAL
	   ;; We do not use :FIND-SPECIAL-BUFFER because we can be called
	   ;; while not inside ZMACS, and there may not even be a good way to
	   ;; pick which ZMACS window to call.
	   (MAKE-INSTANCE 'ZMACS-BUFFER :NAME (LOOP FOR I FROM 1
						    AS BUFNAM = (FORMAT NIL "*Dired-~A-~D*"
									DIRNAME
									I)
						    UNLESS (FIND-BUFFER-NAMED BUFNAM)
						    RETURN BUFNAM))))
    (MAKE-BUFFER-READ-ONLY INTERVAL)
    (SETF (NODE-SPECIAL-TYPE INTERVAL) :DIRED)
    (SETF (BUFFER-SAVED-MAJOR-MODE INTERVAL) 'DIRED-MODE)
    (IF SELECT-P
	(SEND INTERVAL :SELECT)
      (SEND INTERVAL :ACTIVATE))
    (PUTPROP INTERVAL (LIST PATHNAME) 'PATHNAME-LIST)
    (LET ((*INTERVAL* NIL))
      (DIRECTORY-EDIT-REVERT INTERVAL))
    (IF SELECT-P
	(SETQ *DIRED-PATHNAME-NAME* (SEND (BUFFER-PATHNAME INTERVAL) :STRING-FOR-PRINTING)))
    DIS-TEXT))

(DEFUN DIRECTORY-EDIT-MULTIPLE (BUFFER-NAME PATHNAME-LIST &OPTIONAL (SELECT-P T))
  "Create buffer editing the directories in PATHNAME-LIST, and select it unless inhibited.
The buffer is selected unless SELECT-P is NIL."
  (LET* ((INTERVAL (MAKE-INSTANCE 'ZMACS-BUFFER
				  :NAME (OR BUFFER-NAME
					    (LOOP FOR I FROM 1
						  AS BUFNAM = (FORMAT NIL "*Dired-~D*" I)
						  UNLESS (FIND-BUFFER-NAMED BUFNAM)
						  RETURN BUFNAM)))))
    (MAKE-BUFFER-READ-ONLY INTERVAL)
    (SETF (NODE-SPECIAL-TYPE INTERVAL) :DIRED)
    (SETF (BUFFER-SAVED-MAJOR-MODE INTERVAL) 'DIRED-MODE)
    (IF SELECT-P
	(SEND INTERVAL :SELECT)
      (SEND INTERVAL :ACTIVATE))
    (PUTPROP INTERVAL PATHNAME-LIST 'PATHNAME-LIST)
    (LET ((*INTERVAL* NIL))
      (DIRECTORY-EDIT-REVERT INTERVAL))
    DIS-TEXT))

(DEFUN DIRED-ALL-OPEN-SUBDIRECTORIES (BUFFER)
  "Return a list of pathnames of all subdirectories whose files have been brought into BUFFER."
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP BUFFER)) (LINE-NEXT LINE))
       (END-LINE (BP-LINE (INTERVAL-LAST-BP BUFFER)))
       SUBDIRS)
      ((EQ LINE END-LINE)
       (NREVERSE SUBDIRS))
    (AND (DIRED-LINE-PATHNAME LINE)
	 (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT)
	 (PUSH (DIRED-LINE-PATHNAME LINE) SUBDIRS))))

(DEFPROP DIRED-MODE DIRECTORY-EDIT-REVERT MAJOR-MODE-REVERT-FUNCTION)

;;; This function handles Revert Buffer for DIRED buffers,
;;; and is also used to give them the correct contents when they are created.
(DEFUN DIRECTORY-EDIT-REVERT (BUFFER &OPTIONAL IGNORE
			      IGNORE SELECT-FLAG QUIETLY-FLAG
			      &AUX DIRECTORY
			      (PATHNAME-LIST
				(OR (GET BUFFER 'PATHNAME-LIST)
				    (LIST (BUFFER-PATHNAME BUFFER)))))
  (DECLARE (IGNORE QUIETLY-FLAG))
  (WITH-READ-ONLY-SUPPRESSED (BUFFER)
    (LET ((*BATCH-UNDO-SAVE* T)
	  OLD-POSITION-PATHNAME
	  OLD-POSITION-INDEX
	  (SELECTED-P (EQ BUFFER *INTERVAL*))
	  (OPEN-SUBDIRS (DIRED-ALL-OPEN-SUBDIRECTORIES BUFFER))
	  (*INTERVAL* BUFFER))
      (WHEN (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER)))
	(SETQ OLD-POSITION-PATHNAME (DIRED-LINE-PATHNAME (BP-LINE (POINT))))
	(SETQ OLD-POSITION-INDEX (BP-INDEX (POINT))))
      (DELETE-INTERVAL BUFFER)
      (DISCARD-UNDO-INFORMATION BUFFER)
      (UNLESS (= (LENGTH PATHNAME-LIST) 1)
	(SETF (BUFFER-PATHNAME BUFFER) NIL)
	(SETQ *DIRED-PATHNAME-NAME* NIL))
      (DO ((REST PATHNAME-LIST (CDR REST))
	   (FIRST T NIL))
	  ((NULL REST))
	(LET ((PATHNAME (CAR REST)))
	  (FILE-RETRY-NEW-PATHNAME (PATHNAME FS:FILE-ERROR)
	    (SETQ DIRECTORY (FS:DIRECTORY-LIST PATHNAME :DELETED :SORTED)))
	  (SETQ PATHNAME (SEND PATHNAME :TRANSLATED-PATHNAME))
	  (WHEN (= (LENGTH PATHNAME-LIST) 1)
	    (AND SELECTED-P
		 (SETQ *DIRED-PATHNAME-NAME* (SEND PATHNAME :STRING-FOR-PRINTING)))
	    (SEND BUFFER :SEND-IF-HANDLES :SET-PATHNAME PATHNAME)
	    (SEND BUFFER :SEND-IF-HANDLES :SET-FILE-ID (LIST PATHNAME)))
	  (LET ((STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-LAST-BP BUFFER))))
	    (UNLESS FIRST (CLI:TERPRI STREAM))
	    (SEND STREAM :STRING-OUT (SEND PATHNAME :STRING-FOR-PRINTING))
	    (SEND STREAM :LINE-PUT :DIRECTORY PATHNAME)
	    (SEND STREAM :TYO #/CR)
	    (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* (ASSQ NIL DIRECTORY) STREAM)
	    ;; Insert the subfiles, and maybe move point
	    (LET ((FIRST-FILE-LINE
		    (DIRED-INSERT-DIRECTORY DIRECTORY STREAM 0)))
	      (AND FIRST FIRST-FILE-LINE
		   (MOVE-BP (SEND BUFFER :POINT) FIRST-FILE-LINE 0))))))
      ;; Bring back the files of any subdirs whose files were included before.
      (MAPCAR 'DIRED-OPEN-SUBDIRECTORY OPEN-SUBDIRS)
      (SEND BUFFER :SEND-IF-HANDLES :SET-FILE-READ-TICK *TICK*)
      (SEND BUFFER :SEND-IF-HANDLES :SET-FILE-TICK *TICK*)
      ;; "Restore" buffer position by finding where the same pathname would go now.
      (WHEN OLD-POSITION-PATHNAME
	(LET ((BP (DIRED-PATHNAME-INSERTION-BP OLD-POSITION-PATHNAME)))
	  (WHEN BP
	    (MOVE-BP (POINT) BP)
	    (IF (EQ (DIRED-LINE-PATHNAME (BP-LINE (POINT)))
		    OLD-POSITION-PATHNAME)
		(SETF (BP-INDEX (POINT)) OLD-POSITION-INDEX)))))))
  (IF SELECT-FLAG (MAKE-BUFFER-CURRENT BUFFER)))

(DEFCOM COM-DIRED-SUBDIRECTORY "Insert or remove the files of this subdirectory.
The files in the subdirectory mentioned on this line
are inserted into the DIRED buffer underneath this line.
You can then delete them, rename them, etc.
The subdirectory files are indented one additional space.
If the subdirectory contents are already present in the DIRED buffer,
this command offers to remove them from the buffer.
Removing them from the buffer does not delete the files!
It only makes DIRED stop operating on them.

With an argument, prompts for a wildcarded specification of files in the
directory to insert, rather than inserting them all, which is the default. " ()
  (let* ((line (bp-line (point)))
	 (wild-pathname (getf (line-plist line) 'contents-present))
	 (pathname (getf (line-plist line) ':pathname))
	 directory wild-directory)
    (unless (getf (line-plist line) ':directory)
      (barf "~A is not a directory" pathname))
    (setq directory (send pathname :pathname-as-directory)
	  wild-directory (send directory :new-pathname :name :wild :type :wild :version :wild))
    (cond ;; no arg and presently there => close subdir
	  ((and wild-pathname
		(not *numeric-arg-p*)
		(fquery () "Remove subfiles ~A ?" wild-pathname))
	   (dired-close-line-subdirectory line))
	  ;; arg means selective insert of subdir, deleting old contents if present
	  (*numeric-arg-p*
	   (or wild-pathname (setq wild-pathname wild-directory))
	   (setq wild-pathname (read-directory-name
				 (format nil "Edit which subfiles of directory ~A"
					 wild-directory)
				 wild-pathname))
	   (unless (send wild-directory :pathname-match wild-pathname)
	     (format *query-io* "~&~A does not seem to specify any subfile of ~A"
		     wild-pathname wild-directory)
	     (beep)
	     (if (y-or-n-p "Start a separate dired of ~A ?")
		 (directory-edit wild-pathname t)
	       (return-from com-dired-subdirectory dis-none)))	       
	   (if (getf (line-plist line) 'contents-present)
	       (dired-close-line-subdirectory line))
	   (dired-open-line-subdirectory line wild-pathname))
	  ;; no arg and not there => insert *.*.* subdir
	  (t
	   (dired-open-line-subdirectory line wild-directory))))
  dis-text)

(DEFUN DIRED-OPEN-SUBDIRECTORY (PATHNAME)
  "Add the files in the subdirectory PATHNAME to the dired buffer.
Does nothing if that subdirectory is not itself present.
PATHNAME should be the pathname of the file which is the subdirectory."
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (END-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))) WILD-PATHNAME)
      ((EQ LINE END-LINE) NIL)
    (WHEN (EQ PATHNAME (DIRED-LINE-PATHNAME LINE))
      (UNLESS (SETQ WILD-PATHNAME (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
	(DIRED-OPEN-LINE-SUBDIRECTORY LINE WILD-PATHNAME))
      (RETURN T))))

(DEFUN DIRED-OPEN-LINE-SUBDIRECTORY (LINE WILD-PATHNAME &AUX DIRECTORY)
  (IF (SETQ DIRECTORY (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
      (BARF "Subfiles ~A are already present" DIRECTORY)
    (UNLESS (PATHNAMEP WILD-PATHNAME)
      (SETQ WILD-PATHNAME (SEND (SEND (DIRED-LINE-PATHNAME-OR-BARF LINE)
				      :PATHNAME-AS-DIRECTORY)
				:NEW-PATHNAME :NAME :WILD :TYPE :WILD :VERSION :WILD)))
    (SETQ DIRECTORY (FS:DIRECTORY-LIST WILD-PATHNAME :DELETED :SORTED))
    (LET* ((*BATCH-UNDO-SAVE* T))
      (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	(SETF (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT) WILD-PATHNAME)
	(LET ((NEXT-PLIST (LINE-PLIST (LINE-NEXT LINE)))
	      (STREAM (INTERVAL-STREAM-INTO-BP (CREATE-BP (LINE-NEXT LINE) 0))))
	  (DIRED-INSERT-DIRECTORY DIRECTORY STREAM
				  (1+ (DIRED-LINE-LEVEL LINE)))
	  ;; Restore the plist, now clobbered, of the following line.
	  (SETF (LINE-PLIST (BP-LINE (SEND STREAM :READ-BP))) NEXT-PLIST))))))

(defun dired-close-line-subdirectory (line)
  (let* ((*batch-undo-save* t)
	 (wild-pathname (getf (line-plist line) 'contents-present)))
    (if (null wild-pathname)
	(barf "No subfiles are present")
      (with-read-only-suppressed (*interval*)
	(setf (getf (line-plist line) 'contents-present) nil)
	(do ((line2 (line-next line) (line-next line2))
	     (thislevel (dired-line-level line)))
	    ((let ((linelevel (dired-line-level line2)))
	       (or (null linelevel)
		   ( linelevel thislevel)))
	     (delete-interval (create-bp (line-next line) 0)
			      (create-bp line2 0)
			      t)))))))


(DEFVAR *DIRED-SUBDIRECTORY-INDENTATION* 2
  "The number of spaces inserted in front of the files of a subdirectory in dired.")

(DEFUN DIRED-INSERT-DIRECTORY (DIRECTORY STREAM LEVEL)
  "Insert into a DIRED buffer lines describing the files in DIRECTORY.
DIRECTORY is a value returned by FS:DIRECTORY-LIST.
STREAM is a stream outputting into the DIRED buffer.
LEVEL is the depth in subdirectories of these files.
Returns the first inserted line that describes a file."
  ;; Mark all files that are the newest
  (DIRED-COMPUTE-GREATER-THANS (CDR DIRECTORY))
  (DO ((FILES DIRECTORY (CDR FILES))
       (FILE)
       (LINE) (FIRST-FILE-LINE))
      ((NULL FILES)
       FIRST-FILE-LINE)
    (SETQ FILE (CAR FILES))
    (UNLESS (NULL (CAR FILE))
      (IF (GET FILE :DIRECTORY)
	  (LET ((STR (SEND (SEND (SEND (CAR FILE)
				       :NEW-PATHNAME :DEVICE NIL
				       		     ;; Get rid of the version iff this is the newest one.
						     :VERSION (IF (GET FILE ':NEWEST) NIL
								(SEND (CAR FILE) :VERSION)))
				 :PATHNAME-AS-DIRECTORY)
			   :STRING-FOR-DIRECTORY)))
	    ;; STR has the string we want to print instead of the filename.
	    ;; Replace (CAR FILE) with a phony "pathname" that will print as that string.
	    (WITH-STACK-LIST* (FILE1 #'(LAMBDA (&REST IGNORE) STR) (CDR FILE))
	      (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE1 STREAM)))
	(FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE STREAM))
      (SETQ LINE (LINE-PREVIOUS (BP-LINE (SEND STREAM :READ-BP))))
      (INSERT-CHARS (CREATE-BP LINE 5) #/SPACE (* *DIRED-SUBDIRECTORY-INDENTATION* LEVEL))
      ;; Use lower-case "d" to mark already-deleted files.
      (IF (GET FILE ':DELETED)
	  (SETF (CHAR LINE 0) #/d))
      (OR FIRST-FILE-LINE
	  (SETQ FIRST-FILE-LINE LINE))
      (SETF (GETF (LINE-PLIST LINE) 'LEVEL) LEVEL)
      (LOOP FOR (PROP VAL) ON (CDR FILE) BY 'CDDR
	 DO (SETF (GETF (LINE-PLIST LINE) PROP) VAL))
      (SETF (GETF (LINE-PLIST LINE) ':PATHNAME) (CAR FILE)))))

(DEFUN DIRED-COMPUTE-GREATER-THANS (DIRECTORY)
  "This goes through a sorted list of files and puts :NEWEST properties on files."
  (DO ((FILES DIRECTORY (CDR FILES))
       (FILE NIL NEXT-FILE)
       (NEXT-FILE))
      (NIL)
    (SETQ NEXT-FILE (CAR FILES))
    (AND FILE
	 (OR (NULL NEXT-FILE)
	     (NOT (AND (EQUAL (SEND (CAR FILE) :NAME) (SEND (CAR NEXT-FILE) :NAME))
		       (EQUAL (SEND (CAR FILE) :TYPE) (SEND (CAR NEXT-FILE) :TYPE)))))
	 (NOT (MEMQ (SEND (CAR FILE) :VERSION) '(:NEWEST :UNSPECIFIC)))
	 (PUTPROP FILE T :NEWEST))
    (OR FILES (RETURN NIL))))

(DEFUN DIRED-REGENERATE-LINE (LINE &AUX (PLIST (LOCF (LINE-PLIST LINE)))
			      (PATHNAME (GET PLIST :PATHNAME)))
  "Restore the contents of LINE from the data in its properties."
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (LET ((CH (IF (ZEROP (STRING-LENGTH LINE))
		  #/SP
		  (CHAR LINE 0)))
	  (FILE (CONS PATHNAME (CDR PLIST))))
      (SETF (LINE-LENGTH LINE) 0)
      (WITH-OUTPUT-TO-STRING (S LINE)
;	(IF (GET FILE :DIRECTORY)
;	    (LET ((STR (SEND (SEND (SEND (CAR FILE) :PATHNAME-AS-DIRECTORY)
;				   :NEW-PATHNAME :NAME NIL :TYPE NIL :DEVICE NIL)
;			     :STRING-FOR-PRINTING)))
;	      (SEND S :STRING-OUT "      ")
;	      (SEND S :STRING-OUT STR (1+ (STRING-SEARCH-CHAR #/: STR))))
	  (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE S))
;     )
      (OR (GET FILE ':DIRECTORY)
	  ;; Eliminate the Newline which the lister writes.
	  (DECF (LINE-LENGTH LINE)))
      (INSERT-CHARS (CREATE-BP LINE 5) #/SPACE
		    (* *DIRED-SUBDIRECTORY-INDENTATION* (GET FILE 'LEVEL)))
      (SETF (CHAR LINE 0) CH))
    (MUNG-LINE LINE)))


(DEFCOM COM-DIRED-HELP "Explain use of DIRED commands" ()
  (FORMAT T "You are in the directory editor.  The commands are:
	D (or K, c-D, c-K)  Mark the current file for deletion.
	P	Print the current file on the standard hardcopy device.
	A	Queue this file for function application.
	U	Undelete the current file, or else the file just above the cursor.
		Also used to cancel a Print or Apply function request.
	R	Rename this file.  You type the new filename in a mini buffer.
	C	Copy this file.  You type the new filename in a mini buffer.
	L	Load this file (lisp code or QFASL file).
	Rubout	Undelete file above the cursor.
	Space	Move to the next line.
	  Above commands repeat with a numeric argument,
	  backwards if the argument is negative.	  
	S	Insert the contents of this Subdirectory.
	        The files in the subdirectory are indented ~R additional space~:P.
		By default it inserts all the files of the subdirectory; however
		 by giving this command a numeric argument you will be prompted
		 for a wildcarded pathname specifying a subset of the subdirectory's
		 contents.
		If the subdirectory files are already inserted, then S with no
		 argument command offers to remove them from the display.
		Removing them from the display does NOT delete the files!
	N	Move to the next file with more than ~D versions.
		 (This number /"~:*~D/" is the value of ~S)
	H	Mark excess versions of the current file for deletion.
	Q	Exit.  You will be shown the files to be deleted and asked for
		confirmation.  In this display /":/" means a link, /">/" means
		this is the highest version-number of this file, /"!/" means
		not backed-up, and /"$/" means not to be reaped, please.
	X	Execute.  Perform requested file deletions, etc.,
		but stay in the DIRED buffer afterwards.
	!	Move to the next file that is not backed up on tape.
	@	Complement @ flag (dont-delete)
	#	Complement # flag (dont-supersede)
	$	Complement $ flag (dont-reap)
	,	Print the attributes of a file.  For a source file, the -*- line.
		For a QFASL file, the compilation data and what is recorded
		 of the source file's -*- line.
	.	Change properties of current file.
	E	Edit the current file, or DIRED on subdirectory.
	F	Edit current file or subdirectory, not now, but when you exit.
	C-Sh-E	Edit the current file in another window.  The DIRED remains visible.
		 Enters two window mode if you are in one window mode.
        <       DIRED on the superior directory of this directory.
	V	View the current file (doesn't read it all in).
	=	SRCCOM this file with the > version.

Clicking the right-hand button on the mouse will give you a menu
of commands to operate on the line the mouse is pointing at.

Sorting commands which sort the DIRED buffer:

M-X Sort Increasing File Name
M-X Sort Increasing Creation Date
M-X Sort Increasing Reference Date
M-X Sort Increasing Size
and their counterparts with Decreasing instead of Increasing.
"
	  *DIRED-SUBDIRECTORY-INDENTATION*
	  *FILE-VERSIONS-KEPT* '*FILE-VERSIONS-KEPT*
	  )
  DIS-NONE)


;;;; Random dired commands that operate on one or n files.

(DEFCONST DIRED-MOUSE-MENU-ALIST
	   '(("Delete" . COM-DIRED-DELETE)
	     ("Rename" . COM-DIRED-RENAME)
	     ("Copy" . COM-DIRED-COPY)
	     ("Subdirectory" . COM-DIRED-SUBDIRECTORY)
	     ("Undelete//Cancel" . COM-DIRED-UNDELETE-FORWARD)
	     ("Change Properties" . COM-DIRED-CHANGE-FILE-PROPERTIES)
	     ("Edit File" . COM-DIRED-EDIT-FILE)
	     ("View File" . COM-DIRED-VIEW-FILE)
	     ("Compare" . COM-DIRED-SRCCOM)
	     ("Find File on Exit" . COM-DIRED-FIND-FILE)
	     ("Load File" . COM-DIRED-LOAD-FILE)
	     ("Hardcopy" . COM-DIRED-PRINT-FILE)))

(DEFCOM COM-DIRED-MOUSE-MENU "Offer a menu to operate on file mouse points at." ()
  (LET ((BP (MOUSE-BP *WINDOW*))
	COMMAND)
    (USING-RESOURCE (MENU MENU-COMMAND-MENU DIRED-MOUSE-MENU-ALIST)
      (SEND MENU :SET-LABEL
	    (SEND (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE BP)) :STRING-FOR-DIRED))
      (SETQ COMMAND (SEND MENU :CHOOSE)))
    (IF COMMAND
	(PROGN (MOVE-BP (POINT) BP)
	       (FUNCALL COMMAND))
      DIS-NONE)))

(DEFCOM COM-DIRED-PRINT-FILE-ATTRIBUTES "Print the attributes and compilation data of this file." ()
  (LET ((PN (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT))))
	PLIST CD)
    (SETQ PLIST (FS:FILE-ATTRIBUTE-LIST PN))
    (WHEN (SETQ CD (GETF PLIST ':COMPILE-DATA))
      (FORMAT T "The file ~A was compiled by ~A on ~A ~@[(at ~A)~]~% at "
	      PN (CAR CD) (CADR CD) (GETF (SIXTH CD) ':SITE))
      (TIME:PRINT-UNIVERSAL-TIME (CADDR CD))
      (FORMAT T " in system version ~D.~D from ~A~%"
	      (FOURTH CD) (FIFTH CD) (GETF PLIST ':QFASL-SOURCE-FILE-UNIQUE-ID))
      (REMF PLIST ':COMPILE-DATA)
      (REMF PLIST ':QFASL-SOURCE-FILE-UNIQUE-ID))
    (WHEN PLIST
      (FORMAT T "The attributes line of ~A looks like:~% -*-" PN)
      (DO ((PS PLIST (CDDR PS))) ((NULL PS))
	(LET ((PROP (CAR PS)) (VAL (CADR PS))
	      (*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))	      
	  (FORMAT T "~A: ~A; " PROP VAL)))
      (FORMAT T "-*-~%"))
    DIS-NONE))

(DEFCOM COM-DIRED-DOCUMENTATION "Print various sorts of editor documentation" ()
  (LET ((*COM-DOCUMENTATION-ALIST*
	  (CONS '(#/M COM-DIRED-HELP) *COM-DOCUMENTATION-ALIST*)))
    (COM-DOCUMENTATION)))
  
(DEFCOM COM-DIRED-DELETE "Mark file(s) for deletion" ()
  (IF (NOT (GETF (LINE-PLIST (BP-LINE (POINT))) ':DONT-DELETE))
      (DIRED-MAP-OVER-LINES *NUMERIC-ARG* 
			    #'(LAMBDA (LINE)
				(MUNG-LINE LINE)
				(SETF (CHAR LINE 0)
				      (IF (GETF (LINE-PLIST LINE) :DELETED) #/d #/D))))
    (BARF "This file is delete protected.  Use @ to turn of delete protection.")))


(DEFCOM COM-DIRED-UNDELETE-FORWARD "Un-mark file(s) for action.
Can also be used to mark a deleted file for undeletion." ()
  (DIRED-MAP-OVER-LINES *NUMERIC-ARG*
			#'(LAMBDA (LINE)
			    (MUNG-LINE LINE)
			    (SETF (CHAR LINE 0)
				  (IF (GETF (LINE-PLIST LINE) :DELETED) #/U #/SP)))))

(DEFCOM COM-DIRED-UNDELETE "Un-mark next or previous file(s) for action.
With a numeric argument, operates that many lines downward (upward for negative arg).
But with no arg, operates on the previous line's file
 if it is deleted or marked for action; otherwise operates on the current line.
Can also be used to mark a deleted file for undeletion." ()
  (DIRED-MAP-OVER-LINES (IF (AND (NOT *NUMERIC-ARG-P*)
				 (OR (NOT (DIRED-LINE-PATHNAME (BP-LINE (POINT))))
				     (CHAR= (BP-CHARACTER (BEG-LINE (POINT))) #/SPACE)))
			    -1
			    *NUMERIC-ARG*)
			#'(LAMBDA (LINE)
			    (MUNG-LINE LINE)
			    (SETF (CHAR LINE 0)
				  (IF (GETF (LINE-PLIST LINE) :DELETED) #/U #/SP)))))

(DEFCOM COM-DIRED-REVERSE-UNDELETE "Un-mark previous file(s) for action." ()
  (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
  (COM-DIRED-UNDELETE))

(DEFCOM COM-DIRED-FIND-FILE "Mark a file to be read into the editor." ()
   (DIRED-MAP-OVER-LINES *NUMERIC-ARG*
			 #'(LAMBDA(LINE)
			     (UNLESS (GETF (LINE-PLIST LINE) :DELETED)
			       (MUNG-LINE LINE)
			       (SETF (CHAR LINE 0) #/F)))))

(DEFCOM COM-DIRED-PRINT-FILE "Mark a file to be printed" ()
   (DIRED-MAP-OVER-LINES *NUMERIC-ARG*
			 #'(LAMBDA (LINE)
			     (MUNG-LINE LINE)
			     (UNLESS (GETF (LINE-PLIST LINE) :DELETED)
			       (IF (DIRED-PRINTABLE-FILE-P LINE)
				   (SETF (CHAR LINE 0) #/P)
				 (BARF "Don't know how to print this type of file!"))))))

(DEFCOM COM-DIRED-COPY "Copy the file on this line" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET ((FILE (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT)))))
    (LET ((NEWFILE (READ-DEFAULTED-PATHNAME (FORMAT NIL "Pathname to copy ~A to" FILE)
					    FILE))
	  RESULT FILE-PLIST)
      (SETQ RESULT (MULTIPLE-VALUE-LIST (COPY-FILE FILE NEWFILE :ERROR NIL)))
      (COND ((ERRORP (THIRD RESULT))
	     (FORMAT *QUERY-IO* "~&Not copied: ~A" (THIRD RESULT)))
	    (T
	     (FORMAT *QUERY-IO* "~&File copied to ~A" (THIRD RESULT))
	     ;; Save a copy of this file's directory list entry.
	     (SETQ FILE-PLIST (COPYLIST (LINE-PLIST (BP-LINE (POINT)))))
	     (SETF (GETF FILE-PLIST :PATHNAME) (THIRD RESULT))
	     ;; insert a line for the new file.
	     (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	       (MULTIPLE-VALUE-BIND (BP LEVEL)
		   (DIRED-PATHNAME-INSERTION-BP (THIRD RESULT))
		 (COND (BP
			(WITH-BP (SAVE-BP BP :NORMAL)
			  (INSERT BP #/NEWLINE)
			  (SETF (LINE-PLIST (BP-LINE SAVE-BP)) FILE-PLIST)
			  (SETF (DIRED-LINE-LEVEL (BP-LINE SAVE-BP))
				(OR LEVEL 0))
			  (DIRED-REGENERATE-LINE (BP-LINE SAVE-BP))))
		       (T
			(FORMAT *QUERY-IO* ", in a directory not in this display.")))))))))
  DIS-TEXT)

(DEFCOM COM-DIRED-RENAME "Rename the file on this line" () 
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET ((FILE (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT)))))
    (LET ((NEWFILE (READ-DEFAULTED-PATHNAME (FORMAT NIL "Pathname to rename ~A to" FILE)
					    FILE))
	  FILE-PLIST)
      (WITH-OPEN-FILE (STREAM FILE)
	(FILE-RETRY-NEW-PATHNAME (NEWFILE FS:RENAME-FAILURE)
	  (SEND STREAM :RENAME NEWFILE))
	(CLOSE STREAM)
	(SETQ NEWFILE (SEND STREAM :TRUENAME))
	(SETF (GETF (LINE-PLIST (BP-LINE (POINT))) :PATHNAME) NEWFILE)
	(FORMAT *QUERY-IO* "~&File renamed to ~A" NEWFILE)
	;; Save a copy of this file's directory list entry.
	(SETQ FILE-PLIST (LINE-PLIST (BP-LINE (POINT))))
	;; Delete this line.
	(WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	  (DELETE-INTERVAL (BEG-LINE (POINT)) (BEG-LINE (POINT) 1) T)
	  (MULTIPLE-VALUE-BIND (BP LEVEL)
	      (DIRED-PATHNAME-INSERTION-BP NEWFILE)
	    (COND (BP
		   (WITH-BP (SAVE-BP BP :NORMAL)
		     (INSERT BP #/NEWLINE)
		     (SETF (LINE-PLIST (BP-LINE SAVE-BP)) FILE-PLIST)
		     (SETF (DIRED-LINE-LEVEL (BP-LINE SAVE-BP))
			   (OR LEVEL 0))
		     (DIRED-REGENERATE-LINE (BP-LINE SAVE-BP))))
		  (T
		   (FORMAT *QUERY-IO* ", in a directory not in this display."))))))))
  DIS-TEXT)
	   
(DEFCOM COM-DIRED-APPLY-FUNCTION "Mark file(s) for having a function applied to them" ()
  (DIRED-MAP-OVER-LINES *NUMERIC-ARG* 
			#'(LAMBDA (LINE)
			    (UNLESS (GETF (LINE-PLIST LINE) :DELETED)
			      (MUNG-LINE LINE)
			      (SETF (CHAR LINE 0) #/A)))))

(DEFUN DIRED-PRINTABLE-FILE-P (LINE &AUX PLIST PATHNAME TYPE BYTE)
  "T if the file on LINE seems to be one that can be hardcopied reasonably."
  (SETQ PLIST (LOCF (LINE-PLIST LINE))
	PATHNAME (GET PLIST :PATHNAME)
	TYPE (SEND PATHNAME :CANONICAL-TYPE))
  (AND (NOT (SYS:MEMBER-EQUAL TYPE '(:QFASL "BIN" "DRW" "WD" "FASL" "KST" ":EJ" :WIDTHS
				     "OUTPUT")))	;others?
       (OR (EQUAL TYPE "PLT")
	   (EQ TYPE :PRESS)
	   ;; This is probably a text file, skip open. The NIL is for VMS, which can't tell
	   (MEMQ (GET PLIST :BYTE-SIZE) '(7 8 NIL))
	   (WITH-OPEN-FILE (STREAM PATHNAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 9.)
	     (DOTIMES (I 4) (SETQ BYTE (SEND STREAM :TYI)))
	     (AND BYTE (NOT (BIT-TEST BYTE 1)))))))

(DEFCOM COM-DIRED-NEXT-UNDUMPED "Find next file that is not backed up" ()
  (DO ((BP (BEG-LINE (POINT) +1 NIL) (BEG-LINE BP +1 NIL)))
      ((NULL BP) (BARF))
    (AND (GETF (LINE-PLIST (BP-LINE BP)) :NOT-BACKED-UP)
	 (RETURN (MOVE-BP (POINT) BP))))
  DIS-BPS)

(DEFCOM COM-DIRED-COMPLEMENT-DONT-DELETE "Change the @ flag (dont-delete)" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (PLIST (LOCF (LINE-PLIST LINE)))
	 (PATHNAME (GET PLIST :PATHNAME)))
    (FS:CHANGE-FILE-PROPERTIES PATHNAME T :DONT-DELETE
			       (PUTPROP PLIST (NOT (GET PLIST :DONT-DELETE)) :DONT-DELETE))
    (DIRED-REGENERATE-LINE LINE))
  DIS-TEXT)

(DEFCOM COM-DIRED-COMPLEMENT-DONT-SUPERSEDE "Change the # flag (dont-supersede)" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (PLIST (LOCF (LINE-PLIST LINE)))
	 (PATHNAME (GET PLIST :PATHNAME)))
    (FS:CHANGE-FILE-PROPERTIES PATHNAME T :DONT-SUPERSEDE
			       (PUTPROP PLIST (NOT (GET PLIST :DONT-SUPERSEDE)) :DONT-SUPERSEDE))
    (DIRED-REGENERATE-LINE LINE))
  DIS-TEXT)

(DEFCOM COM-DIRED-COMPLEMENT-NO-REAP-FLAG "Change the $ flag" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (PLIST (LOCF (LINE-PLIST LINE)))
	 (PATHNAME (GET PLIST :PATHNAME)))
    (FS:CHANGE-FILE-PROPERTIES PATHNAME T :DONT-REAP
			       (PUTPROP PLIST (NOT (GET PLIST :DONT-REAP)) :DONT-REAP))
    (DIRED-REGENERATE-LINE LINE))
  DIS-TEXT)

(DEFCOM COM-DIRED-NEXT-HOG "Find the next file with superfluous versions.
This is a file with more numbered versions than the value of *FILE-VERSIONS-KEPT*,
or the numeric argument if one is supplied." ()
  (LET* ((HOG (IF *NUMERIC-ARG-P* *NUMERIC-ARG* *FILE-VERSIONS-KEPT*))
	 (LINE (BP-LINE (POINT)))
	 PATHNAME)
    (DO () ((SETQ PATHNAME (DIRED-LINE-PATHNAME LINE)))
      (SETQ LINE (LINE-NEXT LINE)))
    (OR LINE (BARF "No more hogs"))
    (DO ((LINE LINE (LINE-NEXT LINE))
	 (STOP-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	 (NAME (SEND PATHNAME :NAME))
	 (TYPE (SEND PATHNAME :TYPE))
	 (SKIP-P T)	;Skipping current file
	 (FIRST-LINE)	;Save first line in this group
	 (N-VERSIONS))	;Number of versions of current file so far
	((EQ LINE STOP-LINE) (BARF "No more hogs"))
      (SETQ PATHNAME (DIRED-LINE-PATHNAME LINE))
     CHECK-AGAIN
      (AND PATHNAME
	   (COND ((AND (EQUAL NAME (SEND PATHNAME :NAME))
		       (EQUAL TYPE (SEND PATHNAME :TYPE)))
		  (COND ((AND (NOT SKIP-P)
			      (> (SETQ N-VERSIONS (1+ N-VERSIONS)) HOG))
			 (MOVE-BP (POINT) FIRST-LINE 0)
			 (RETURN (NEXT-HOG-REDISPLAY LINE STOP-LINE NAME TYPE)))))
		 (T (SETQ SKIP-P NIL
			  NAME (SEND PATHNAME :NAME)
			  TYPE (SEND PATHNAME :TYPE)
			  N-VERSIONS 0
			  FIRST-LINE LINE)
		    (GO CHECK-AGAIN)))))))


(DEFUN NEXT-HOG-REDISPLAY (LINE STOP-LINE NAME TYPE
			   &AUX LAST-LINE LAST-LINE-BP
			   (START-BP (POINT))
			   (N-PLINES (WINDOW-N-PLINES *WINDOW*)))
  ;; Find first line that doesn't match.
  (DO ((L (LINE-NEXT LINE)(LINE-NEXT L)) (PATHNAME))
      ((EQ L STOP-LINE) (SETQ LAST-LINE STOP-LINE
			      LAST-LINE-BP (CREATE-BP LAST-LINE 0)))
    (SETQ PATHNAME (DIRED-LINE-PATHNAME L))
    (UNLESS (AND (EQUAL NAME (SEND PATHNAME :NAME))
		 (EQUAL TYPE (SEND PATHNAME :TYPE)))
      (RETURN (SETQ LAST-LINE L
		    LAST-LINE-BP (CREATE-BP LAST-LINE 0)))))
  ;; Now figure out displaying.
  ;; IF THE WHOLE THING IS NOT DISPLAYED, FIRST TRY DISPLAYING IT IN THE MIDDLE
  (COND ((AND (< (FLOOR N-PLINES 2) (COUNT-LINES START-BP LAST-LINE-BP))
	      (NULL (PLINE-OF-POINT T *WINDOW* LAST-LINE-BP)))
	 ;; Redisplay at top
	 (RECENTER-WINDOW *WINDOW* :START START-BP))
	((NULL (PLINE-OF-POINT T *WINDOW* LAST-LINE-BP))
	 (RECENTER-WINDOW *WINDOW* :ABSOLUTE 0.2S0)))
  DIS-BPS)


(DEFCOM COM-DIRED-SRCCOM "Compare the current file against the > version" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (SRCCOM-FILE (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT))))
  DIS-NONE)

(DEFCOM COM-DIRED-LOAD-FILE "Load the current Lisp or QFASL file." ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LOAD (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT))))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FILE "Compare" SRCCOM-FILE NIL
			  "Compare this file with the newest version.")

(DEFUN SRCCOM-FILE (PATHNAME-1 &AUX PATHNAME-2)
  (SETQ PATHNAME-2 (SEND PATHNAME-1 :NEW-VERSION :NEWEST))
  (PROMPT-LINE "Source comparing ~A" PATHNAME-1)
  (LET ((*STANDARD-OUTPUT*
	  (MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM
	    (FORMAT NIL "*Source Compare ~A // ~A*"
		    (SEND PATHNAME-1 :STRING-FOR-EDITOR)
		    (SEND PATHNAME-2 :STRING-FOR-EDITOR))
	    NIL T)))
    (SRCCOM:SOURCE-COMPARE PATHNAME-1 PATHNAME-2)))

(DEFCOM COM-DIRED-VIEW-FILE "View the current file" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (DIR-P (GETF (LINE-PLIST LINE) :DIRECTORY))
	 (PATHNAME (DIRED-LINE-PATHNAME-OR-BARF LINE)))
    (IF DIR-P
	(VIEW-DIRECTORY (SEND (FUNCALL PATHNAME :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
			      						 :NAME :WILD
									 :TYPE :WILD
									 :VERSION :WILD))
     (VIEW-FILE PATHNAME (GETF (LINE-PLIST LINE) :DELETED))))
   DIS-NONE)

(DEFUN NEED-TO-REVERT-BUFFER (PATHNAME)
  (LET ((PROBE-STREAM (PROBEF (SEND PATHNAME :NEW-VERSION :NEWEST))))
    (IF (NOT PROBE-STREAM)
	(BARF "Cannot find any version of this file. Perhaps it has been deleted.")
      (IF (GREATERP (SEND PROBE-STREAM :VERSION)
		    (SEND PATHNAME :VERSION))
	  (IF (EQUAL (COMPLETING-READ-FROM-MINI-BUFFER 
		       "A newer version of this file exists now.  Revert this dired buffer? (default is no)" 
		       '(("yes") ("no")))
		     '("yes"))
	      (REVERT-BUFFER *INTERVAL*)
	    (IF (EQUAL (COMPLETING-READ-FROM-MINI-BUFFER
			 "Do you want to see the newest version? (default is no)"
			 '(("yes") ("no")))
		       '("yes"))
		(SEND PATHNAME :NEW-VERSION :NEWEST)
	      PATHNAME))
	(SEND PATHNAME :NEW-VERSION :NEWEST)))))

(DEFCOM COM-DIRED-EDIT-FILE "Edit the current file; or DIRED it if it's a directory" ()
  (OR (TYPEP *WINDOW* 'ZMACS-WINDOW) (BARF))
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (DIR-P (GETF (LINE-PLIST LINE) :DIRECTORY))
	 (PATHNAME (DIRED-LINE-PATHNAME-OR-BARF LINE)))
    (AND (NOT DIR-P) ; Reversion doesn't make sense for recursive Dired
	 (GETF (LINE-PLIST LINE) :NEWEST)
	 (IF (NULL (SETQ PATHNAME (NEED-TO-REVERT-BUFFER PATHNAME)))
	     (BARF "Re-enter edit command")))
    (IF DIR-P
	(DIRECTORY-EDIT (SEND (SEND PATHNAME :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
			      					      :NAME :WILD
								      :TYPE :WILD
								      :VERSION :WILD))
      (LET ((BUFFER (FIND-FILE-BUFFER PATHNAME)))
	(IF BUFFER (MAKE-BUFFER-CURRENT BUFFER)
	  (FIND-FILE PATHNAME)))
      (LET ((BLURB (KEY-FOR-COMMAND 'COM-SELECT-PREVIOUS-BUFFER
				    *COMTAB* NIL NIL #/C-M-L)))
	(AND (NULL BLURB) (SETQ BLURB (KEY-FOR-COMMAND 'COM-SELECT-BUFFER))
	     (SETQ BLURB (STRING-APPEND BLURB " Return")))
	(AND BLURB
	     (FORMAT *QUERY-IO* "~&Type ~A to return to DIRED" BLURB))
	DIS-TEXT))))


(DEFCOM COM-DIRED-EDIT-SUPERIOR-DIRECTORY
  "Edit the superior directory of the current buffer's directory." ()
  (OR (TYPEP *WINDOW* 'ZMACS-WINDOW) (BARF))
  (LET* ((PATHNAME (SEND *INTERVAL* :PATHNAME))
	 (DIRECTORY (SEND PATHNAME :DIRECTORY)))
    (IF (OR (NOT (CONSP DIRECTORY)) (= 1 (LENGTH DIRECTORY)))
	(BARF "There isn't a superior for this directory.")
      	(DIRECTORY-EDIT
	  (SEND (SEND PATHNAME :DIRECTORY-PATHNAME-AS-FILE) :NEW-PATHNAME
		:DIRECTORY (BUTLAST DIRECTORY)
		:NAME :WILD :TYPE :WILD :VERSION :WILD)))))

(DEFCOM COM-DIRED-EDIT-FILE-TWO-WINDOWS
	"Edit the current file; or DIRED it if it's a directory" ()
  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) :DELETED)
    (BARF))
  (OR (TYPEP *WINDOW* 'ZMACS-WINDOW) (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (DIR-P (GETF (LINE-PLIST LINE) :DIRECTORY))
	 (PATHNAME (DIRED-LINE-PATHNAME-OR-BARF LINE)))
    (AND (GETF (LINE-PLIST LINE) :NEWEST)
	 (IF (NULL (SETQ PATHNAME (NEED-TO-REVERT-BUFFER PATHNAME)))
	     (BARF "Re-enter edit command")))
    (SWITCH-WINDOWS)
    (IF DIR-P
	(DIRECTORY-EDIT (SEND (SEND PATHNAME :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
								     :NAME :WILD
								     :TYPE :WILD
								     :VERSION :WILD))
      (LET ((BUFFER (FIND-FILE-BUFFER PATHNAME)))
	(IF BUFFER (MAKE-BUFFER-CURRENT BUFFER)
	  (FIND-FILE PATHNAME)))
      (LET ((BLURB (KEY-FOR-COMMAND 'COM-SELECT-PREVIOUS-BUFFER
				    *COMTAB* NIL NIL #/C-M-L)))
	(AND (NULL BLURB) (SETQ BLURB (KEY-FOR-COMMAND 'COM-SELECT-BUFFER))
	     (SETQ BLURB (STRING-APPEND BLURB " Return")))
	(AND BLURB
	     (FORMAT *QUERY-IO* "~&Type ~A to return to DIRED" BLURB))
	DIS-TEXT))))


(DEFCOM COM-DIRED-ABORT "Abort dired" ()
  (SEND *WINDOW* :EXIT-SPECIAL-BUFFER))

(DEFCOM COM-DIRED-EXIT "Leave DIRED, performing deletions//visiting//printing.
Displays the files to be deleted and/or printed, then asks you to confirm." ()
  (IF (DIRED-PROCESS-FILES)
      (SEND *WINDOW* :EXIT-SPECIAL-BUFFER NIL *INTERVAL*))
  DIS-BPS)

(DEFCOM COM-DIRED-EXECUTE "Perform requested deletions//visiting//printing.
Displays the files to be deleted and/or printed, then asks you to confirm." ()
  (DIRED-PROCESS-FILES)
  DIS-BPS)

(DEFVAR *DIRED-FUNCTION-TO-APPLY* :UNBOUND
  "While processing A commands, holds the function to apply as read from the minibuffer.")

(DEFUN DIRED-PROCESS-FILES ()
  "Perform all the operations requested on files in the DIRED buffer.
Returns T if user typed E or Y or Q, NIL if user typed N."
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (UNDELETEABLE (SEND (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*) :UNDELETABLE-P))
       DELETE-FILES
       UNDELETE-FILES
       FIND-FILES
       PRINT-FILES
       APPLY-FILES
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
       QUERY-RESULT)
      ((EQ LINE LAST-LINE)
       (SETQ DELETE-FILES (NREVERSE DELETE-FILES)
	     UNDELETE-FILES (NREVERSE UNDELETE-FILES)
	     FIND-FILES (NREVERSE FIND-FILES)
	     PRINT-FILES (NREVERSE PRINT-FILES)
	     APPLY-FILES (NREVERSE APPLY-FILES))
       (CATCH 'RETURN-TO-DIRED (PROGN	 
	 (COND ((OR DELETE-FILES UNDELETE-FILES FIND-FILES PRINT-FILES APPLY-FILES)
		(AND DELETE-FILES (DIRED-PRINT-FILE-LIST DELETE-FILES "deleted"))
		(AND UNDELETE-FILES (DIRED-PRINT-FILE-LIST UNDELETE-FILES "undeleted"))
		(AND FIND-FILES (DIRED-PRINT-FILE-LIST FIND-FILES "visited"))
		(AND PRINT-FILES (DIRED-PRINT-FILE-LIST PRINT-FILES "printed"))
		(AND APPLY-FILES (DIRED-PRINT-FILE-LIST APPLY-FILES "processed by function"))
		(COND ((SETQ QUERY-RESULT
			     (DIRED-FILE-QUERY UNDELETEABLE
					       (AND DELETE-FILES "Delete")
					       (AND UNDELETE-FILES "Undelete")
					       (AND FIND-FILES "Visit")
					       (AND PRINT-FILES "Print")
					       (AND APPLY-FILES "Apply function")))
		       (COND (APPLY-FILES
			      ;This crock to fake out read-function-name.
			      ;Mouse would not win particularily.
			      (LET* ((*MINI-BUFFER-REPEATED-COMMAND* '())
				     *DIRED-FUNCTION-TO-APPLY*)
				(MULTIPLE-VALUE-BIND (FNSPEC STRING)
				    (READ-FUNCTION-NAME "Function to apply:" 'COMPILE-FILE)
				  (SETQ *DIRED-FUNCTION-TO-APPLY*
					(COND ((FDEFINEDP FNSPEC) FNSPEC)
					      (T (CONDITION-CASE ()
						     (CLI:READ-FROM-STRING STRING)
						   (SYS:END-OF-FILE
						    (BARF)))))))
				(DIRED-DO-FILE-LIST APPLY-FILES
						    'DIRED-APPLY-FUNCTION NIL))))
		       (AND DELETE-FILES
			    (DIRED-DO-FILE-LIST DELETE-FILES 'DIRED-DELETE-FILE "delete"
						:DELETE-MULTIPLE-FILES
						#'(LAMBDA (LINE)
						    (SETF (GETF (LINE-PLIST LINE) ':DELETED) T))))
		       (AND UNDELETE-FILES
			    (DIRED-DO-FILE-LIST UNDELETE-FILES 'DIRED-UNDELETE-FILE
						"undelete"
						:UNDELETE-MULTIPLE-FILES
						#'(LAMBDA (LINE)
						    (SETF (GETF (LINE-PLIST LINE) ':DELETED) NIL))))
		       (AND FIND-FILES
			    (DIRED-DO-FILE-LIST FIND-FILES 'DIRED-FIND-FILE "visit"))
		       (AND PRINT-FILES
			    (DIRED-DO-FILE-LIST PRINT-FILES 'DIRED-PRINT-FILE "print"))
		       ;; Expunge if desired.
		       (WHEN (EQ QUERY-RESULT :EXPUNGE)
			 (LET ((BLOCKS-FREED 0))
			   ;; Expunge the directory we did DIRED on.
			   (INCF BLOCKS-FREED
				 (FS:EXPUNGE-DIRECTORY
				   (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*)))
			   ;; Expunge any subdirectories whose contents are listed.
			   (DO ((LINE (LINE-NEXT (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
				      (LINE-NEXT LINE)))
			       ((NULL (LINE-NEXT LINE)))
			     (WHEN (AND (GETF (LINE-PLIST LINE) :DIRECTORY)
					(GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
			       (INCF BLOCKS-FREED
				     (FS:EXPUNGE-DIRECTORY
				       (SEND (DIRED-LINE-PATHNAME LINE)
					     :PATHNAME-AS-DIRECTORY)))))
			   (FORMAT *QUERY-IO* "~&~D blocks freed." BLOCKS-FREED)))
		       ;; If the deleted files are now gone for good,
		       ;; delete their lines from the buffer.
		       ;; Also, flush any U's, A's, F's, or P's.
		       (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
			 (DO ((LINE (LINE-NEXT (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
				    (LINE-NEXT LINE)))
			     ((NULL (LINE-NEXT LINE)))
			   (COND ((= (LENGTH LINE) 0))
				 ((CHAR-EQUAL (CHAR LINE 0) #/D)
				  (IF (OR (EQ QUERY-RESULT :EXPUNGE)
					  (NOT UNDELETEABLE))
				      (DELETE-INTERVAL (BEG-OF-LINE LINE)
						       (BEG-OF-LINE (LINE-NEXT LINE))
						       T)
				    (MUNG-LINE LINE)
				    (SETF (CHAR LINE 0) #/d)))
				 ((CHAR (CHAR LINE 0) #/SP)
				  (MUNG-LINE LINE)
				  (SETF (CHAR LINE 0) #/SP)))))))))
	 (RETURN-FROM DIRED-PROCESS-FILES T))))
    (WHEN (DIRED-LINE-PATHNAME LINE)
      (CASE (CHAR LINE 0)
	(#/D (PUSH LINE DELETE-FILES))
	(#/U (PUSH LINE UNDELETE-FILES))
	(#/F (PUSH LINE FIND-FILES))
	(#/P (PUSH LINE PRINT-FILES))
	(#/A (PUSH LINE APPLY-FILES))))))

(DEFUN DIRED-PRINT-FILE-LIST (FILES NAME)
  (FORMAT T "~&Files to be ~A" NAME)
  (WHEN *DIRED-PATHNAME-NAME* (FORMAT T  " in ~A" *DIRED-PATHNAME-NAME*))
  (TERPRI) (TERPRI)
  (SEND *STANDARD-OUTPUT* :ITEM-LIST NIL
	   (MAPCAR #'(LAMBDA (LINE)
		       (LET ((PLIST (LOCF (LINE-PLIST LINE))))
			 (STRING-APPEND
			   (IF (GET PLIST :DONT-REAP) #/$ #/SP)
			   (IF (GET PLIST :NOT-BACKED-UP) #/! #/SP)
			   (IF (GET PLIST :LINK-TO) #/: #/SP)
			   (IF (GET PLIST :NEWEST) #/> #/SP)
			   #/SP
			   ;; Mention the file's directory if not the normal one.
			   (IF (EQUAL (SEND (GET PLIST :PATHNAME) :DIRECTORY)
				      (SEND (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*)
					    :DIRECTORY))
			       (SEND (GET PLIST :PATHNAME) :STRING-FOR-DIRED)
			       (SEND (GET PLIST :PATHNAME) :STRING-FOR-PRINTING)))))
		   FILES)))

(DEFUN DIRED-FILE-QUERY (UNDELETEABLE &REST NAMES &AUX (N 0) STRING)
  (SETQ STRING
	(WITH-OUTPUT-TO-STRING (STREAM)
	  (DO ((L NAMES (CDR L))) ((NULL L))
	      (AND (CAR L) (SETQ N (1+ N))))
	  (DO ((L NAMES (CDR L))
	       (FLAG NIL))
	      ((NULL L))
	    (COND ((CAR L)
		   (IF FLAG
		       (COND ((> N 2)
			      (SEND STREAM :STRING-OUT ", ")
			      (SETQ N (1- N)))
			     ((= N 2)
			      (SEND STREAM :STRING-OUT " or ")))
		     (SETQ FLAG T))
		   (SEND STREAM :STRING-OUT (CAR L)))))
	  (SEND STREAM :STRING-OUT "? ")))
  (SELECTQ (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
	     (FQUERY (IF UNDELETEABLE
			 '(:CHOICES (((:ABORT "Abort.") #/Q #/X)
				     ((:EXPUNGE "Yes, then expunge.") #/E)
				     . #,FORMAT:Y-OR-N-P-CHOICES)
				    :FRESH-LINE T
				    :HELP-FUNCTION DIRED-FILE-QUERY-HELP)
		       '(:CHOICES (((:ABORT "Abort.") #/Q #/X)
				   . #,FORMAT:Y-OR-N-P-CHOICES)
				  :FRESH-LINE T
				  :HELP-FUNCTION DIRED-FILE-QUERY-HELP-NO-EXPUNGE))
		     STRING))
    ((T) T)
    ((NIL) (THROW 'RETURN-TO-DIRED NIL))
    (:EXPUNGE :EXPUNGE)
    (:ABORT NIL)))

(DEFUN DIRED-FILE-QUERY-HELP (STREAM IGNORE IGNORE)
  (FORMAT STREAM
	  "~%Type Y to go ahead, E to go ahead and afterward expunge the directory,
 N to return to DIRED, or Q or X to abort out of DIRED.~%"))

(DEFUN DIRED-FILE-QUERY-HELP-NO-EXPUNGE (STREAM IGNORE IGNORE)
  (FORMAT STREAM
	  "~%Type Y to go ahead, N to return to DIRED, or Q or X to abort out of DIRED.~%"))

;;; A MULTIPLE-FILE-MESSAGE is assumed to take a first argument of ERROR-P
;;; and a second of FILES.  It should return either an error object (entire operation failed),
;;; NIL (entire operation successful),
;;; or a list of values corresponding to individual message values.
(DEFUN DIRED-DO-FILE-LIST (FILES FUNCTION NAME &OPTIONAL MULTIPLE-FILE-MESSAGE AUXILIARY-FUNCTION
			   &AUX ERR PATHS)
;; Added AUXILIARY-FUNCTION which is called for each file in FILES when the multiple-file
;; path is used.  This is so delete/undelete can pass in a function to update the plist
;; on each line.  1/2/85 KHS.
  (COND ((AND MULTIPLE-FILE-MESSAGE
	      (SEND (DIRED-LINE-PATHNAME (CAR FILES)) :OPERATION-HANDLED-P MULTIPLE-FILE-MESSAGE))
	 (SETQ PATHS (MAPCAR #'DIRED-LINE-PATHNAME FILES))
	 (SETQ ERR (SEND (CAR PATHS) MULTIPLE-FILE-MESSAGE
			 NIL			;error-p
			 PATHS))
	 (AND AUXILIARY-FUNCTION
	      (NOT (ERRORP ERR))
	      (MAPC AUXILIARY-FUNCTION FILES))
	 (AND NAME (ERRORP ERR)
	      (DIRED-REPORT-ERROR NAME "files" ERR))
	 (AND NAME (CONSP ERR)
	      (MAPC #'(LAMBDA (PATHNAME ERROR)
			(AND (ERRORP ERROR)
			     (DIRED-REPORT-ERROR NAME PATHNAME ERROR)))
		    PATHS ERR)))
	(T (DOLIST (LINE FILES)
	     (SETQ ERR (FUNCALL FUNCTION LINE))
	     (AND NAME
		  (ERRORP ERR)
		  (DIRED-REPORT-ERROR NAME (DIRED-LINE-PATHNAME LINE) ERR))))))

(DEFUN DIRED-REPORT-ERROR (NAME PATH ERR)
  (FORMAT T "~&Cannot ~A ~A because ~A"
	  NAME PATH ERR))

(DEFUN DIRED-DELETE-FILE (LINE)
  (LET ((ERROR (SEND (DIRED-LINE-PATHNAME LINE) :DELETE NIL)))
    (UNLESS (ERRORP ERROR)
      (SETF (GETF (LINE-PLIST LINE) :DELETED) T))
    ERROR))

(DEFUN DIRED-UNDELETE-FILE (LINE)
  (LET ((ERROR (SEND (DIRED-LINE-PATHNAME LINE) :UNDELETE NIL)))
    (UNLESS (ERRORP ERROR)
      (SETF (GETF (LINE-PLIST LINE) :DELETED) NIL))
    ERROR))

(DEFUN DIRED-APPLY-FUNCTION (LINE)
  (FUNCALL *DIRED-FUNCTION-TO-APPLY* (DIRED-LINE-PATHNAME LINE)))

(DEFCONST *DIRED-PRINT-OPTIONS* NIL
  "Options given to HARDCOPY-FILE for printing files in DIRED.")

(DEFUN DIRED-PRINT-FILE (LINE)
  (APPLY 'HARDCOPY-FILE (DIRED-LINE-PATHNAME LINE) *DIRED-PRINT-OPTIONS*))

;;; Read the file mentioned by LINE into the editor but do not select its buffer.
;;; Note that this works even if the DIRED is not inside ZMACS.
(DEFUN DIRED-FIND-FILE (LINE)
  (LET* ((DIR-P (GET (LINE-PLIST LINE) :DIRECTORY))
	 (PATHNAME (DIRED-LINE-PATHNAME LINE)))
    (AND (GETF (LINE-PLIST LINE) :NEWEST)
	 (IF (NULL (SETQ PATHNAME (NEED-TO-REVERT-BUFFER PATHNAME)))
	     (BARF "Re-enter command")))
    (IF DIR-P
	(LOAD-DIRECTORY-INTO-ZMACS
	  (SEND (SEND PATHNAME :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
						       :NAME :WILD
						       :TYPE :WILD
						       :VERSION :WILD))
	(LOAD-FILE-INTO-ZMACS PATHNAME))))

;;;; Dired sorting commands.

(DEFCOM COM-DIRED-SORT-BY-INCREASING-FILE-NAME "Sort by file name (up)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (FS:PATHNAME-LESSP (DIRED-LINE-PATHNAME L1)
				       (DIRED-LINE-PATHNAME L2))))))

(DEFCOM COM-DIRED-SORT-BY-DECREASING-FILE-NAME "Sort by file name (down)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (FS:PATHNAME-LESSP (DIRED-LINE-PATHNAME L2)
				       (DIRED-LINE-PATHNAME L1))))))


(DEFCOM COM-DIRED-SORT-BY-INCREASING-REFERENCE-DATE "Sort by reference date (up)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (LET ((P1 (LOCF (LINE-PLIST L1)))
			  (P2 (LOCF (LINE-PLIST L2))))
		      (< (OR (GET P1 :REFERENCE-DATE) -1)
			 (OR (GET P2 :REFERENCE-DATE) -1)))))))

(DEFCOM COM-DIRED-SORT-BY-DECREASING-REFERENCE-DATE "Sort by reference date (down)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (LET ((P1 (LOCF (LINE-PLIST L1)))
			  (P2 (LOCF (LINE-PLIST L2))))
		      (< (OR (GET P2 :REFERENCE-DATE) -1)
			 (OR (GET P1 :REFERENCE-DATE) -1)))))))

(DEFCOM COM-DIRED-SORT-BY-INCREASING-CREATION-DATE "Sort by creation date (up)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (LET ((P1 (LOCF (LINE-PLIST L1)))
			  (P2 (LOCF (LINE-PLIST L2))))
		      (< (GET P1 :CREATION-DATE) (GET P2 :CREATION-DATE)))))))

(DEFCOM COM-DIRED-SORT-BY-DECREASING-CREATION-DATE "Sort by creation date (down)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (LET ((P1 (LOCF (LINE-PLIST L1)))
			  (P2 (LOCF (LINE-PLIST L2))))
		      (< (GET P2 :CREATION-DATE) (GET P1 :CREATION-DATE)))))))

(DEFCOM COM-DIRED-SORT-BY-INCREASING-SIZE "Sort by file size (up)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (< (DIRED-LINE-FILE-SIZE L1) (DIRED-LINE-FILE-SIZE L2))))))

(DEFCOM COM-DIRED-SORT-BY-DECREASING-SIZE  "Sort by file size (down)" ()
  (DIRED-SORT #'(LAMBDA (I1 I2)
		  (LET ((L1 (INTERVAL-SORT-KEY I1)) (L2 (INTERVAL-SORT-KEY I2)))
		    (< (DIRED-LINE-FILE-SIZE L2) (DIRED-LINE-FILE-SIZE L1))))))

(DEFUN DIRED-LINE-FILE-SIZE (LINE &AUX PLIST BYTE-SIZE)
  "Return size, in bits, of file described by LINE."
  (SETQ PLIST (LOCF (LINE-PLIST LINE)))
  (COND ((GET PLIST :LINK-TO) -1)		;Sort links together in this mode
	((SETQ BYTE-SIZE (GET PLIST :BYTE-SIZE))
	 (* BYTE-SIZE (GET PLIST :LENGTH-IN-BYTES)))
	(T
	 (* (GET PLIST :BLOCK-SIZE)
	    (GET PLIST :LENGTH-IN-BLOCKS)))))

(DEFUN DIRED-SORT (PREDICATE)
  "Sort the DIRED buffer using PREDICATE.
PREDICATE should be a function of two arguments.
It should apply INTERVAL-SORT-KEY to each argument to get a line.
These lines will describe two files, and their plists can be used
to get the data for the comparison."
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    ;; Sort the top level, keeping each subdirectory's sublevels as a group.
    (DIRED-SORT-1 PREDICATE
		  (BEG-LINE (INTERVAL-FIRST-BP *INTERVAL*) 2)
		  (INTERVAL-LAST-BP *INTERVAL*)))
  DIS-TEXT)

(DEFUN DIRED-SORT-1 (PREDICATE START-BP END-BP)
  "Sort the part of the DIRED buffer from START-BP to END-BP, at top level and lower levels.
First, the highest level present in that range is sorted,
keeping each subdirectory's contents after the subdirectory.
Then, each bunch of files at a deeper level is individually sorted, recursively."
  (WITH-BP (SAVE-START-BP START-BP :NORMAL)
    (WITH-BP (SAVE-END-BP END-BP :MOVES)
      (SORT-INTERVAL-FUNCTIONS-WITH-KEY 
	'COPY-BP
	#'(LAMBDA (BP) (VALUES BP (BP-LINE BP)))
	'DIRED-NEXT-FILE-SAME-LEVEL
	PREDICATE
	START-BP END-BP)
      (DO ((BP SAVE-START-BP END)
	   END)
	  ((BP-= BP SAVE-END-BP))
	(SETQ END (DIRED-NEXT-FILE-SAME-LEVEL BP))
	(UNLESS (BP-= END (BEG-LINE BP 1))
	  (DIRED-SORT-1 PREDICATE (BEG-LINE BP 1) END))))))

(DEFUN DIRED-NEXT-FILE-SAME-LEVEL (BP)
  "Return a BP to the beginning of the next line whose level is <= BP's line's level."
  (DO ((LINE (LINE-NEXT (BP-LINE BP)) (LINE-NEXT LINE))
       (LEVEL (DIRED-LINE-LEVEL (BP-LINE BP))))
      ((OR (NOT (DIRED-LINE-LEVEL LINE))
	   ( (DIRED-LINE-LEVEL LINE)
	      LEVEL))
       (CREATE-BP LINE 0))))
    
(DEFCOM COM-DIRED-AUTOMATIC "Mark superfluous versions of current file for deletion
Superfluous files are those with more numbered versions than the value
of *FILE-VERSIONS-KEPT* (not counting noncontiguous versions),
and files with type in the list *TEMP-FILE-TYPE-LIST*.
Files marked with a $ are always exempted.
With numeric argument, processes whole directory." ()
  (IF *NUMERIC-ARG-P* (COM-DIRED-AUTOMATIC-ALL)
      ;; Start by making FIRST-LINE and LAST-LINE bracket all of this file,
      ;; and make VERSIONS be a list of the numeric versions of it
      (LET ((FIRST-LINE (BP-LINE (POINT)))
	    (LAST-LINE)
	    (STOP-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	    VERSIONS)
	(DO ((LINE FIRST-LINE (LINE-NEXT LINE))
	     (NAME (SEND (DIRED-LINE-PATHNAME-OR-BARF FIRST-LINE) :NAME))
	     (TYPE (SEND (DIRED-LINE-PATHNAME FIRST-LINE) :TYPE))
	     (PATHNAME))
	    ((EQ LINE STOP-LINE) (SETQ LAST-LINE LINE))
	  (SETQ PATHNAME (DIRED-LINE-PATHNAME LINE))
	  (OR (AND (EQUAL (SEND PATHNAME :NAME) NAME)
		   (OR (EQUAL (SEND PATHNAME :TYPE) TYPE)
		       (MEMQ (SEND PATHNAME :VERSION) '(:NEWEST :UNSPECIFIC))))
	      (RETURN (SETQ LAST-LINE LINE)))
	  (LET ((VERS (SEND PATHNAME :VERSION)))
	    (AND (NOT (MEMQ VERS '(:NEWEST :UNSPECIFIC)))
		 (PUSH VERS VERSIONS))))
	;; Now sort the versions into decreasing order and drop any nonconsecutive old ones.
	(SETQ VERSIONS (SORT VERSIONS #'>))
	(DO ((V VERSIONS (CDR V)))
	    ((NULL (CDR V)))
	  (IF ( (CAR V) (1+ (CADR V)))
	      (RETURN (SETF (CDR V) NIL))))
	;; Now remove the last N of them from the list to be flushed.
	(SETQ VERSIONS (NTHCDR *FILE-VERSIONS-KEPT* VERSIONS))
	;; Now scan through, and mark for deletion all the versions still in VERSIONS.
	;; Also mark temp types.
	(DO ((LINE FIRST-LINE (LINE-NEXT LINE))
	     PATHNAME TYPE VERS)
	    ((EQ LINE LAST-LINE))
	  (SETQ PATHNAME (DIRED-LINE-PATHNAME LINE)
		VERS (SEND PATHNAME :VERSION)
		TYPE (SEND PATHNAME :TYPE))
	  (COND ((OR (MEMQ VERS VERSIONS)
		     (SYS:MEMBER-EQUAL TYPE *TEMP-FILE-TYPE-LIST*))
		 (OR (GET (LOCF (LINE-PLIST LINE)) :DONT-REAP)
		     (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
		        (MUNG-LINE LINE)
			(SETF (CHAR LINE 0) #/D))))))))
  DIS-TEXT)

(DEFCOM COM-DIRED-AUTOMATIC-ALL "Mark all superfluous files for deletion." ()
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (STOP-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
       (NAME NIL)	;If non-NIL is TYPE being skipped
       (TYPE)
       (*NUMERIC-ARG-P* NIL)
       (PATHNAME)
       (FIRST-FILE-LINE NIL))
      ((EQ LINE STOP-LINE)
       (MOVE-BP (POINT) FIRST-FILE-LINE 0))
    (SETQ PATHNAME (DIRED-LINE-PATHNAME LINE))
   CHECK-THIS
    (COND (PATHNAME
	   (OR FIRST-FILE-LINE (SETQ FIRST-FILE-LINE LINE))
	   (COND ((NULL NAME)
		  (MOVE-BP (POINT) LINE 0)
		  (COM-DIRED-AUTOMATIC)
		  (SETQ NAME (SEND PATHNAME :NAME)
			TYPE (SEND PATHNAME :TYPE)))
		 ((AND (EQUAL (SEND PATHNAME :NAME) NAME)
		       (OR (EQUAL (SEND PATHNAME :TYPE) TYPE)
			   (MEMQ (SEND PATHNAME :VERSION) '(:NEWEST :UNSPECIFIC)))))
		 (T (SETQ NAME NIL)
		    (GO CHECK-THIS))))))
 DIS-TEXT)

(DEFCOM COM-DIRED-CHANGE-FILE-PROPERTIES "Change the properties of this file." ()
  (LET ((NEW-PROPS
	  (CHANGE-FILE-PROPERTIES (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT))))))
    (OR (EQ NEW-PROPS T)
	(DO ((L NEW-PROPS (CDDR L)))
	    ((NULL L))
	  (PUTPROP (LOCF (LINE-PLIST (BP-LINE (POINT)))) (CADR L) (CAR L))))
    (DIRED-REGENERATE-LINE (BP-LINE (POINT))))
  DIS-TEXT)

(DEFCOM COM-REAP-FILE "Delete multiple versions of the specified file." ()
  (LET ((PATHNAME (READ-DEFAULTED-WILD-PATHNAME "Reap file" (DEFAULT-PATHNAME))))
    (PROMPT-LINE "")
    (REAP-FILE PATHNAME
	       (IF *NUMERIC-ARG-P* *NUMERIC-ARG* *FILE-VERSIONS-KEPT*)
	       *MODE-LINE-WINDOW*))
  (SEND *STANDARD-OUTPUT* :MAKE-COMPLETE)
  DIS-NONE)

(DEFUN REAP-FILE (&OPTIONAL (PATHNAME "")
			    (N-TO-KEEP *FILE-VERSIONS-KEPT*)
			    (PROMPT-STREAM *STANDARD-OUTPUT*))
  "Delete all but the last N-TO-KEEP versions of PATHNAME.
It tells you which versions there are and which it will delete,
then asks for confirmation."
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME
					     (SEND FS:LAST-FILE-OPENED :NEW-PATHNAME
						   		       :TYPE :WILD
								       :VERSION :WILD)
					     :WILD :WILD))
  (FORMAT PROMPT-STREAM "~&Reaping ~A" PATHNAME)
  (REAP-DIRECTORY PATHNAME N-TO-KEEP *STANDARD-OUTPUT*))

(DEFCOM COM-CLEAN-DIRECTORY "Delete multiple versions in the specified directory." ()
  (LET ((PATHNAME (READ-DIRECTORY-NAME "Clean directory" (DEFAULT-PATHNAME))))
    (PROMPT-LINE "")
    (CLEAN-DIRECTORY PATHNAME
		     (IF *NUMERIC-ARG-P* *NUMERIC-ARG* *FILE-VERSIONS-KEPT*)
		     *MODE-LINE-WINDOW*))
  (SEND *STANDARD-OUTPUT* :MAKE-COMPLETE)
  DIS-NONE)

(DEFUN CLEAN-DIRECTORY (&OPTIONAL (PATHNAME FS:LAST-FILE-OPENED)
				  (N-TO-KEEP *FILE-VERSIONS-KEPT*)
				  (PROMPT-STREAM *STANDARD-OUTPUT*))
  "Delete all but the last N-TO-KEEP versions of each file in PATHNAME.
PATHNAME may (and does by default) contain wildcards
so you can process all the files in a directory.
It tells you which versions there are and which it will delete,
then asks for confirmation, for each filename individually."
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME
					     (SEND FS:LAST-FILE-OPENED :NEW-PATHNAME
						   		       :NAME :WILD
								       :TYPE :WILD
								       :VERSION :WILD)
					     :WILD :WILD))
  (FORMAT PROMPT-STREAM "~&Cleaning ~A for >~D versions"
	  PATHNAME N-TO-KEEP)
  (REAP-DIRECTORY PATHNAME N-TO-KEEP *STANDARD-OUTPUT*))

;(COMPILER:MAKE-OBSOLETE DIRED-PATHNAME-LESSP "use FS:PATHNAME-LESSP")
;(DEFF DIRED-PATHNAME-LESSP 'FS:PATHNAME-LESSP)

(DEFUN REAP-DIRECTORY (PATHNAME N-TO-KEEP STREAM &AUX DIRECTORY-LIST SOMETHING-DELETED)
  (SETQ DIRECTORY-LIST (FS:DIRECTORY-LIST PATHNAME :SORTED))
  (SETQ DIRECTORY-LIST (DELQ (ASSQ NIL DIRECTORY-LIST) DIRECTORY-LIST))
  (DO ((LIST DIRECTORY-LIST (CDR LIST))
       (HEAD NIL)
       (PREV-NAME NIL NAME) (NAME)
       (PREV-TYPE NIL TYPE) (TYPE)
       (PATHNAME))
      (NIL)
    (AND LIST
	 (SETQ PATHNAME (CAAR LIST)
	       NAME (SEND PATHNAME :NAME)
	       TYPE (SEND PATHNAME :TYPE)))
    (COND ((OR (NULL LIST)
	       (NOT (EQUAL PREV-NAME NAME))
	       (AND (NOT (EQUAL PREV-TYPE TYPE))
		    (NEQ (SEND PATHNAME :VERSION) :UNSPECIFIC)))
	   (AND HEAD (REAP-ONE-FILE HEAD LIST N-TO-KEEP STREAM)
		(SETQ SOMETHING-DELETED T))
	   (OR (SETQ HEAD LIST) (RETURN NIL)))))
  (AND SOMETHING-DELETED
       (SEND PATHNAME :UNDELETABLE-P)
       (LET ((*QUERY-IO* *TERMINAL-IO*))
	 (FQUERY NIL "Expunge ~A? " (SEND PATHNAME :STRING-FOR-DIRECTORY)))
       (FORMAT T "~&~D blocks reclaimed." (FS:EXPUNGE-DIRECTORY PATHNAME))))

;;; Returns T if something was deleted.
(DEFUN REAP-ONE-FILE (HEAD TAIL N-TO-KEEP STREAM
		      &AUX LAST-VERSION FIRST-DELETION-VERSION (N-VERSIONS 0) THIS-VERSION
		      DELETE-LIST KEEP-LIST)
  (DO LIST HEAD (CDR LIST) (EQ LIST TAIL)
      (SETQ THIS-VERSION (SEND (CAAR LIST) :VERSION))
      (WHEN (NUMBERP THIS-VERSION)
	(IF (AND LAST-VERSION ( (1+ LAST-VERSION) THIS-VERSION))
	    (SETQ LAST-VERSION NIL N-VERSIONS 0))
	(UNLESS LAST-VERSION (SETQ FIRST-DELETION-VERSION THIS-VERSION))
	(SETQ N-VERSIONS (1+ N-VERSIONS)
	      LAST-VERSION THIS-VERSION)))
  ;; FIRST-DELETION-VERSION is lowest version number to delete.
  ;; That is the bottom of the sequence of consecutive versions
  ;; that ends with the most recent version.
  ;; N-VERSIONS is number of versions that exist, starting with that version.
  (DO ((LIST HEAD (CDR LIST))
       (N-TO-DELETE -1)
       (FILE) (PATHNAME) (VERSION))
      ((EQ LIST TAIL)
       (SETQ DELETE-LIST (NREVERSE DELETE-LIST)
	     KEEP-LIST (NREVERSE KEEP-LIST)))
    (SETQ FILE (CAR LIST)
	  PATHNAME (CAR FILE)
	  VERSION (SEND PATHNAME :VERSION))
    (IF (EQ VERSION FIRST-DELETION-VERSION)
	(SETQ N-TO-DELETE (- N-VERSIONS N-TO-KEEP)))
    (IF (AND (OR (AND (NUMBERP VERSION) (PLUSP N-TO-DELETE))
		 (SYS:MEMBER-EQUAL (SEND PATHNAME :TYPE) *TEMP-FILE-TYPE-LIST*))
	     (NOT (GET FILE :DONT-REAP)))
	(PUSH FILE DELETE-LIST)
	(PUSH FILE KEEP-LIST))
    (AND (NUMBERP VERSION)
	 (SETQ N-TO-DELETE (1- N-TO-DELETE))))
  (COND (DELETE-LIST
	 (COND (KEEP-LIST
		(FORMAT STREAM "~&Keeping the following file~P:  (in ~A)~%"
			(LENGTH KEEP-LIST) (SEND (CAAR KEEP-LIST) :STRING-FOR-DIRECTORY))
		(DOLIST (FILE KEEP-LIST)
		  (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE))))
	 (FORMAT STREAM "~&Deleting the following file~P:~:[ (in ~A)~]~%"
		 (LENGTH DELETE-LIST) KEEP-LIST
		 (SEND (CAAR DELETE-LIST) :STRING-FOR-DIRECTORY))
	 (DOLIST (FILE DELETE-LIST)
	   (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE))
	 (AND (LET ((*QUERY-IO* STREAM))
		(Y-OR-N-P "Ok? "))
	      (DOLIST (L DELETE-LIST T)
		(LET ((PATHNAME (CAR L)))
		  (CONDITION-CASE (ERROR)
		      (SEND PATHNAME :DELETE)
		    (FS:FILE-ERROR
		     (FORMAT STREAM "~&Cannot delete ~A because ~A.~%" PATHNAME ERROR)))))))))

(DEFCOM COM-CHANGE-FILE-PROPERTIES "Change properties on a file" ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Change properties for" (PATHNAME-DEFAULTS)
					   NIL NIL :DELETED)))
    (CHANGE-FILE-PROPERTIES PATHNAME))
  DIS-NONE)

;;; Really nice printing for ZWEI's Change File Properties
(DEFPROP :DONT-DELETE "Don't Delete" PRETTY-NAME)
(DEFPROP :DONT-REAP "Don't Reap" PRETTY-NAME)

(DEFUN CHANGE-FILE-PROPERTIES (PATHNAME &AUX DIRECTORY INDICATORS VALUES CHOICES CHANGES)
  (MULTIPLE-VALUE (DIRECTORY INDICATORS)
    (FS:FILE-PROPERTIES PATHNAME NIL))
  (AND (ERRORP DIRECTORY) (BARF "Err: ~A" DIRECTORY))
  (OR (SETQ PATHNAME (CAR DIRECTORY))
      (BARF "Err:  File not found"))
  (OR INDICATORS (BARF "File has no settable properties."))
  (SETQ VALUES (LOOP FOR IND IN INDICATORS
		     COLLECT (GET DIRECTORY IND)))
  (SETQ CHOICES (LOOP FOR IND IN INDICATORS
		      COLLECT (LIST IND
				    (OR (GET IND 'PRETTY-NAME)
					(SETF (GET IND 'PRETTY-NAME)
					      (STRING-CAPITALIZE-WORDS (STRING-APPEND IND))))
				    (DOLIST (L FS:*KNOWN-DIRECTORY-PROPERTIES* :SEXP)
				      (AND (MEMQ IND (CDR L))
					   (RETURN (CADDR (CAR L))))))))
  (LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.) (*NOPOINT T) (*PRINT-RADIX* NIL))
    (CATCH 'ABORT
      (PROGV INDICATORS VALUES
	(TV:CHOOSE-VARIABLE-VALUES CHOICES
				   :LABEL (FORMAT NIL "Change properties for ~A" PATHNAME)
				   :MARGIN-CHOICES '("Do It"
						     ("Abort" (THROW 'ABORT T))))
	(SETQ CHANGES (LOOP FOR IND IN INDICATORS
			    FOR VAL IN VALUES
			    AS NEW = (SYMBOL-VALUE IND)
			    WHEN (NOT (EQUAL NEW VAL))
			    NCONC (LIST IND NEW))))
      (APPLY 'FS:CHANGE-FILE-PROPERTIES PATHNAME T CHANGES)
      CHANGES)))

(DEFVAR *EXITING-MAIL-EXITS-ZMACS* NIL)
(DEFPROP *EXITING-MAIL-EXITS-ZMACS* T MODE-SETTABLE-P)

(DEFVAR *MAIL-CONTROL-X-COMTAB*)
(DEFUN INITIALIZE-MAIL-CONTROL-X-COMTAB ()
  (SETQ *MAIL-CONTROL-X-COMTAB*
	(SET-COMTAB '*MAIL-CONTROL-X-COMTAB*
		    '(#/A COM-ADD-MORE-TEXT
		      #/C COM-ADD-CC-FIELD
		      #/S COM-ADD-SUBJECT-FIELD
		      #/T COM-ADD-TO-FIELD)
		    (MAKE-COMMAND-ALIST
		      '(COM-ADD-TO-FIELD
			COM-ADD-CC-FIELD
			COM-ADD-SUBJECT-FIELD
			COM-ADD-IN-REPLY-TO-FIELD
			COM-ADD-MORE-TEXT COM-ADD-FROM-FIELD
			COM-CHANGE-SUBJECT-PRONOUNS))))
  (SET-COMTAB-INDIRECTION *MAIL-CONTROL-X-COMTAB* *ZMACS-CONTROL-X-COMTAB*))

;;; Define command names now for MAKE-COMMAND-ALIST's sake.
;;; Since the commands are really in ZMAIL, these command names would
;;; not otherwise exist when ZMACS is initialized and the above function is called.
(DEFPROP COM-ADD-TO-FIELD "Add To Field" COMMAND-NAME)
(DEFPROP COM-ADD-CC-FIELD "Add CC Field" COMMAND-NAME)
(DEFPROP COM-ADD-SUBJECT-FIELD "Add Subject Field" COMMAND-NAME)
(DEFPROP COM-ADD-IN-REPLY-TO-FIELD "Add In-Reply-To Field" COMMAND-NAME)
(DEFPROP COM-ADD-MORE-TEXT "Add More Text" COMMAND-NAME)
(DEFPROP COM-ADD-FROM-FIELD "Add From Field" COMMAND-NAME)
(DEFPROP COM-CHANGE-SUBJECT-PRONOUNS "Change Subject Pronouns" COMMAND-NAME)

;;;; Send mail
(DEFMINOR COM-MAIL-MODE MAIL-MODE "Mail" 1 "Setup for mailing" ()
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/')
  (SET-CHAR-SYNTAX WORD-DELIMITER *MODE-WORD-SYNTAX-TABLE* #/.)
  (SET-COMTAB *MODE-COMTAB* '(#/C- COM-EXIT-COM-MAIL
			      #/END COM-EXIT-COM-MAIL
			      #/ABORT COM-QUIT-COM-MAIL
			      #/C-] COM-QUIT-COM-MAIL
			      #/TAB COM-TAB-TO-TAB-STOP
			      #/H-F COM-FORWARD-ADDRESS
			      #/H-B COM-BACKWARD-ADDRESS
			      #/H-K COM-KILL-ADDRESS
			      #/H-RUBOUT COM-BACKWARD-KILL-ADDRESS
			      #/H-T COM-EXCHANGE-ADDRESSES)
	      (IF (VARIABLE-BOUNDP *TEMPLATE-COMMAND-ALIST*)
		  *TEMPLATE-COMMAND-ALIST*))
  (SET-COMTAB *MODE-COMTAB*
	      (LIST #/C-X (MAKE-EXTENDED-COMMAND *MAIL-CONTROL-X-COMTAB*)))
  (SETQ *COMMENT-START* NIL)		;Be like Text mode
  ;; This FORMAT is here to dynamically figure out how to type the character
  (SET-MODE-LINE-LIST `(,@(MODE-LINE-LIST) ,(FORMAT NIL "     ~:@C mails, ~:@C aborts"
						    #/END #/ABORT)))
	;;This makes M-Q and M-[ understand the --Text follows this line-- line
; character lossage
  (SETQ *PARAGRAPH-DELIMITER-LIST* (CONS #/- *PARAGRAPH-DELIMITER-LIST*))
  ;; The default value of this is NIL already, but this way,
  ;; it can be set to T and will become NIL again if we switch buffers.
  (SETQ *EXITING-MAIL-EXITS-ZMACS* NIL))

(DEFCOM COM-MAIL "Send mail.
Puts you into the buffer *MAIL*.  With a numeric argument
retains the previous contents of the buffer.  Above the funny
line you can put TO:, CC:, SUBJECT: (or S:), and FROM: lines to
control the mailing process.  Below the funny line you put the
text of the message.  End causes the mail to be transmitted.
Abort quits out." ()
  (COM-MAIL-INTERNAL (IF (NOT *NUMERIC-ARG-P*) '*DEFAULT-ZMACS-MAIL-TEMPLATE*)))

(DEFINE-SITE-VARIABLE *HOST-FOR-BUG-REPORTS* :HOST-FOR-BUG-REPORTS)

(DEFUN PARSE-BUG-ARG (WHO)
  (VALUES (STRING-APPEND "BUG-" WHO #/@ *HOST-FOR-BUG-REPORTS*)
	  (LET ((S (FORMAT NIL "In~:[ ~A in~;~*~] ~A, on ~A (~A):~2%"
			       (STRING-EQUAL WHO "CADR") WHO	;used to be BUG-LISPM
			       (SI:SYSTEM-VERSION-INFO)
			       SI:LOCAL-PRETTY-HOST-NAME
			       (machine-type))))
	    ;; Fill to fit within a 75-column line
	    (LOOP WITH LINE-START = 0
		  FOR START = 0 THEN (+ COMMA-POS 2)
		  AS PREV-COMMA-POS = NIL THEN COMMA-POS
		  AS COMMA-POS = (STRING-SEARCH ", " S START)
	       WHEN (> (- (OR COMMA-POS (STRING-LENGTH S)) LINE-START) 72.)
	         UNLESS (NULL PREV-COMMA-POS)
		   DO (SETF (CHAR S (1+ PREV-COMMA-POS)) #/NEWLINE)
		      (when (> prev-comma-pos line-start)
			(SETQ LINE-START (+ PREV-COMMA-POS 2))
			(SETQ COMMA-POS PREV-COMMA-POS))
	       UNTIL (NULL COMMA-POS))
	    S)))

(DEFCOM COM-BUG "Setup mail buffer for sending a bug report, arg prompts for type" ()
  (LET (WHO WHAT)
    (SETQ WHO (COMPLETING-READ-FROM-MINI-BUFFER
		"Report bug to BUG- (default CADR)"	;used to be BUG-LISPM
		(SUBSET #'(LAMBDA (ELT) (NOT (EQUAL (CAR ELT) "Other")))
			*ZMAIL-BUG-LIST*)
		T NIL))
    (IF (CONSP WHO) (SETQ WHO (CAR WHO)))
    (AND (EQUAL WHO "") (SETQ WHO 'CADR))	;used to be BUG-LISPM
    (MULTIPLE-VALUE (WHO WHAT)
      (PARSE-BUG-ARG WHO))
    (COM-MAIL-INTERNAL '*DEFAULT-ZMACS-BUG-TEMPLATE* WHO WHAT)))

;;; Create a buffer, put it in text mode, initialize to the right thing, and return.
;;; RE-INIT-P may be T meaning just initialize,
;;; or it can be a variable whose value (if non nil) is a DEFINE-MAIL-TEMPLATE template.
(DEFUN COM-MAIL-INTERNAL (RE-INIT-P &OPTIONAL WHO WHAT INITIAL-POSITION)
  (SEND *WINDOW* :FIND-SPECIAL-BUFFER :MAIL RE-INIT-P "Mail" T :TEXT)
  (WHEN RE-INIT-P				;With no numeric arg, re-initialize the buffer
    (COM-TEXT-MODE)
    (TURN-ON-MODE 'MAIL-MODE)
    (DELETE-INTERVAL *INTERVAL*)
    (INSERT-MOVING (POINT) "To: ")
    (AND WHO (INSERT-MOVING (POINT) WHO))
    (LET ((BP (INSERT (POINT) #/NEWLINE)))
      (SETQ BP (INSERT BP *MAIL-HEADER-DELIMITER*))
      (SETQ BP (INSERT BP #/NEWLINE))
      (WHEN WHAT
	(INSERT-MOVING BP WHAT)
	(IF INITIAL-POSITION
	    (SETQ BP (FORWARD-CHAR BP (- INITIAL-POSITION (STRING-LENGTH WHAT)) T))))
      (AND WHO (MOVE-BP (POINT) BP)))
    ;; RE-INIT-P can be a variable whose value may be a template to use.
    ;; If so, invoke the template.
    (AND (NEQ RE-INIT-P T)
	 (BOUNDP RE-INIT-P)
	 (SYMBOL-VALUE RE-INIT-P)
	 (FUNCALL (SYMBOL-VALUE RE-INIT-P) *INTERVAL* NIL))
    (DISCARD-UNDO-INFORMATION *INTERVAL*)
    (NOT-MODIFIED *INTERVAL*))
  DIS-TEXT)

;(DEFUN SELECT-MOST-RECENT-BUFFER ()
;  (MAKE-BUFFER-CURRENT (DOLIST (BUF *ZMACS-BUFFER-HISTORY*)
;		       (OR (EQ BUF *INTERVAL*)
;			   (RETURN BUF)))))

(DEFCOM COM-QUIT-COM-MAIL "Abort sending mail, but announce how to continue" ()
  (LET ((EXIT-FLAG *EXITING-MAIL-EXITS-ZMACS*))
    (COND ((NOT (BUFFER-MODIFIED-P *INTERVAL*))
	   (LET ((BUFFER *INTERVAL*))
	     (SEND *WINDOW* :EXIT-SPECIAL-BUFFER NIL BUFFER)
	     (KILL-BUFFER BUFFER)
	     (IF EXIT-FLAG
		 (THROW 'EXIT-TOP-LEVEL NIL))
	     DIS-TEXT))
	  (T (FORMAT *QUERY-IO* "~&Quitting, you may continue")
	     (IF (OR *EXITING-MAIL-EXITS-ZMACS*
		     (AND (GET 'STANDALONE-MAIL-OR-DIRED-FRAME 'SI:FLAVOR)
			  (TYPEP (SEND *WINDOW* :SUPERIOR) 'STANDALONE-MAIL-OR-DIRED-FRAME)))
		 (FORMAT *QUERY-IO* " with (MAIL T)")
	       (LET ((*STANDARD-OUTPUT* *QUERY-IO*))
		 (FIND-COMMAND-ON-KEYS 'COM-MAIL 1 " by giving a numeric arg to ")))
	     (PROG1 (SEND *WINDOW* :EXIT-SPECIAL-BUFFER)
		    (IF EXIT-FLAG
			(THROW 'EXIT-TOP-LEVEL NIL)))))))

(DEFCOM COM-EXIT-COM-MAIL "Actually transmits the mail." ()
  (LET* ((BP1 (INTERVAL-FIRST-BP *INTERVAL*))
	 (BP2 (OR (ZWEI-SEARCH BP1 (STRING-APPEND #/NEWLINE
						  *MAIL-HEADER-DELIMITER*
						  #/NEWLINE))
		  (BARF "You've messed up the buffer"))))
    ;; Call ZMail to do the actual sending in the appropriate manner for this host
    (SEND-MESSAGE BP1 (BEG-LINE BP2 -1 T) T BP2 (INTERVAL-LAST-BP *INTERVAL*) T))
  (LET ((EXIT-FLAG *EXITING-MAIL-EXITS-ZMACS*))
    (PROG1 (SEND *WINDOW* :EXIT-SPECIAL-BUFFER T)
	   (IF EXIT-FLAG
	       (THROW 'EXIT-TOP-LEVEL NIL)))))

(DEFUN ZMACS-COMPOSE-MESSAGE (WHO WHAT &OPTIONAL INITIAL-POSITION &AUX (RE-INIT-P T))
  (AND (EQ WHO T) (SETQ RE-INIT-P NIL WHO NIL))
  (COM-MAIL-INTERNAL RE-INIT-P (AND WHO (STRING WHO)) (AND WHAT (STRING WHAT))
		     INITIAL-POSITION)
  (SETQ *EXITING-MAIL-EXITS-ZMACS* T)
  DIS-TEXT)

(DEFUN BUG (&OPTIONAL (PROGRAM 'CADR) TEXT CALL-EDITOR-ANYWAY)	;used to be BUG-LISPM
  "Record a bug in PROGRAM.
If TEXT is omitted, or CALL-EDITOR-ANYWAY is T, an editor window is used.
CALL-EDITOR-ANYWAY can be a number; the cursor is initially positioned
that many characters from the beginning of the string TEXT.
With no arguments, you specify everything with the editor window."
  (MULTIPLE-VALUE-BIND (WHOM WHAT0)
      (PARSE-BUG-ARG PROGRAM)
    (WHEN TEXT
      (IF (NUMBERP CALL-EDITOR-ANYWAY)
	  (SETQ CALL-EDITOR-ANYWAY
		(+ CALL-EDITOR-ANYWAY
		   1 (STRING-LENGTH WHAT0))))
      (SETQ WHAT0 (STRING-APPEND WHAT0 #/NEWLINE TEXT)))
    (MAIL WHOM WHAT0 (OR (NULL TEXT) CALL-EDITOR-ANYWAY))))

(DEFUN DIRED (&OPTIONAL (PATHNAME ""))
   "Edit the directory specified in PATHNAME.
The default is to edit the directory of the last file you tried to open.
You use the editor to specify files to delete, rename, etc.,
then when you exit the operations are performed.
Type Help when inside DIRED for more info."
   (ED `(DIRECTORY
	  ,(FS:MERGE-PATHNAME-DEFAULTS PATHNAME
				       (SEND FS:LAST-FILE-OPENED :NEW-PATHNAME
					     			 :NAME :WILD
								 :TYPE :WILD
								 :VERSION :WILD)
				       :WILD :WILD))))

;;; Top level functions for mailing
(DEFUN MAIL (&OPTIONAL USER TEXT CALL-EDITOR-ANYWAY)
  "Mail the string TEXT to the user USER. 
If TEXT is omitted, or CALL-EDITOR-ANYWAY is T, an editor window is used.
CALL-EDITOR-ANYWAY can be a number; the cursor is initially positioned
that many characters from the beginning of the string TEXT.
With no arguments, you specify everything with the editor window."
  (COND ((AND TEXT (NOT CALL-EDITOR-ANYWAY))
	 (SEND-MESSAGE-STRING USER TEXT))
	(T
	 (ED `(MAIL ,USER ,TEXT
		    ,(IF (NUMBERP CALL-EDITOR-ANYWAY) CALL-EDITOR-ANYWAY))))))

; ZMACS frames are now used for the MAIL and DIRED functions.

;(DEFVAR *MAIL-AND-DIRED-USE-ZMACS* T
;  "Non-NIL means use a ZMACS frame for the functions MAIL and DIRED.
;NIL means use a standalone mail-or-dired frame.")

;(DEFUN DIRED (&OPTIONAL (PATHNAME ""))
;   "Edit the directory specified in PATHNAME.
;The default is to edit the directory of the last file you tried to open.
;You use the editor to specify files to delete, rename, etc.,
;then when you exit the operations are performed.
;Type Help when inside DIRED for more info."
;  (IF *MAIL-AND-DIRED-USE-ZMACS*
;      (ED `(DIRECTORY
;	     ,(FS:MERGE-PATHNAME-DEFAULTS PATHNAME
;					  (SEND FS:LAST-FILE-OPENED :NEW-PATHNAME
;								    :NAME :WILD
;								    :TYPE :WILD
;								    :VERSION :WILD)
;					  :WILD :WILD)))
;    (USING-RESOURCE (DIRED STANDALONE-MAIL-OR-DIRED-FRAME)
;      (SEND DIRED :DIRED PATHNAME))))

;;;; Top level functions for mailing
;(DEFUN MAIL (&OPTIONAL USER TEXT CALL-EDITOR-ANYWAY)
;  "Mail the string TEXT to the user USER. 
;If TEXT is omitted, or CALL-EDITOR-ANYWAY is T, an editor window is used.
;CALL-EDITOR-ANYWAY can be a number; the cursor is initially positioned
;that many characters from the beginning of the string TEXT.
;With no arguments, you specify everything with the editor window."
;  (COND ((AND TEXT (NOT CALL-EDITOR-ANYWAY))
;	 (SEND-MESSAGE-STRING USER TEXT))
;	(*MAIL-AND-DIRED-USE-ZMACS*
;	 (ED `(MAIL ,USER ,TEXT
;		    ,(IF (NUMBERP CALL-EDITOR-ANYWAY) CALL-EDITOR-ANYWAY))))
;	(T
;	 (USING-RESOURCE (WINDOW STANDALONE-MAIL-OR-DIRED-FRAME)
;	   (SEND WINDOW :MAIL USER TEXT
;		 (IF (NUMBERP CALL-EDITOR-ANYWAY) CALL-EDITOR-ANYWAY))))))

;;;; The :BASE-TICK operation is called by the :MODIFIED-P operation on the interval.
;(DEFFLAVOR STANDALONE-MAIL-OR-DIRED-FRAME ((*DIRED-PATHNAME-NAME* NIL)
;					   BASE-TICK)
;	   (STANDALONE-EDITOR-FRAME)
;  (:SPECIAL-INSTANCE-VARIABLES *DIRED-PATHNAME-NAME*)
;  :GETTABLE-INSTANCE-VARIABLES
;  (:DOCUMENTATION :SPECIAL-PURPOSE "The editor window for the (DIRED) and (MAIL) functions"))

;(DEFMETHOD (STANDALONE-MAIL-OR-DIRED-FRAME :EXIT-SPECIAL-BUFFER) (&REST IGNORE)
;  (THROW 'EXIT-TOP-LEVEL T))

;(DEFMETHOD (STANDALONE-MAIL-OR-DIRED-FRAME :FIND-SPECIAL-BUFFER) (&REST IGNORE))

;(DEFMETHOD (STANDALONE-MAIL-OR-DIRED-FRAME :NAME-FOR-SELECTION) ()
;  (IF *DIRED-PATHNAME-NAME*
;      (FORMAT NIL "Dired: ~A" *DIRED-PATHNAME-NAME*)
;    (FORMAT NIL "Mail: ~A"
;	    (BP-LINE (INTERVAL-FIRST-BP (WINDOW-INTERVAL TV:SELECTION-SUBSTITUTE))))))

;(DEFMETHOD (STANDALONE-MAIL-OR-DIRED-FRAME :SELECTABLE-WINDOWS) ()
;  (LIST (LIST (SEND SELF :NAME-FOR-SELECTION) SELF)))

;(DEFMETHOD (STANDALONE-MAIL-OR-DIRED-FRAME :DIRED) (PATHNAME)
;  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME
;					     (SEND FS:LAST-FILE-OPENED :NEW-PATHNAME
;								       :NAME :WILD
;								       :TYPE :WILD
;								       :VERSION :WILD)
;					     :WILD :WILD))
;  (SETQ BASE-TICK *TICK*)
;  (LET ()
;    (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
;    (COM-DIRED-MODE)
;    (MAKE-BUFFER-READ-ONLY *INTERVAL*)
;    (DIRECTORY-EDIT-REVERT *INTERVAL* PATHNAME)
;    (MUST-REDISPLAY *WINDOW* DIS-ALL))
;  (TV:WINDOW-CALL (SELF :DEACTIVATE)
;    ;; Make sure typeout window does not come up
;    (SEND (SEND TV:SELECTION-SUBSTITUTE :TYPEOUT-WINDOW) :MAKE-COMPLETE)
;    (SEND TV:SELECTION-SUBSTITUTE :EDIT)))

;(DEFMETHOD (STANDALONE-MAIL-OR-DIRED-FRAME :MAIL)
;	   (WHO WHAT &OPTIONAL INITIAL-POSITION &AUX (RE-INIT-P T))
;  (AND (EQ WHO T) (SETQ RE-INIT-P NIL WHO NIL))
;  (IF RE-INIT-P (SETQ BASE-TICK *TICK*))
;  (SETQ *DIRED-PATHNAME-NAME* NIL)
;  (FUNCALL EDITOR-CLOSURE
;	   'COM-MAIL-INTERNAL RE-INIT-P (AND WHO (STRING WHO)) (AND WHAT (STRING WHAT))
;	   INITIAL-POSITION)
;  (SETF (WINDOW-REDISPLAY-DEGREE TV:SELECTION-SUBSTITUTE) DIS-ALL)
;  (TV:WINDOW-CALL (SELF :DEACTIVATE)
;    (SEND TV:SELECTION-SUBSTITUTE :EDIT)))

;(DEFWINDOW-RESOURCE STANDALONE-MAIL-OR-DIRED-FRAME ()
;	:MAKE-WINDOW (STANDALONE-MAIL-OR-DIRED-FRAME)
;	:REUSABLE-WHEN :DEACTIVATED
;	:INITIAL-COPIES 0)

;(DEFUN SOURCE-COMPARE-MERGE (PATHNAME-1 PATHNAME-2 OUTPUT-PATHNAME)
;  "Merge files PATHNAME-1 and PATHNAME-2, putting output in OUTPUT-PATHNAME.
;Merging finds those sections of the files which match and those which differ,
;just like regular SOURCE-COMPARE.  The output is generated by copying the
;matching sections automatically, and asking the user which version to use
;for the differing sections.
;Type Help at the query to get an explanation of the command interface."
;  (IF *MAIL-AND-DIRED-USE-ZMACS*
;      (ED `(SOURCE-COMPARE-MERGE
;	     ,(FS:MERGE-PATHNAME-DEFAULTS PATHNAME-1)
;	     ,(FS:MERGE-PATHNAME-DEFAULTS PATHNAME-2 PATHNAME-1)
;	     ,(FS:MERGE-PATHNAME-DEFAULTS OUTPUT-PATHNAME PATHNAME-2)))
;    (USING-RESOURCE (WINDOW STANDALONE-MAIL-OR-DIRED-FRAME)
;      (TV:WINDOW-CALL ((WINDOW-FRAME WINDOW) :DEACTIVATE)
;	(SEND WINDOW :FUNCALL-EDITOR-CLOSURE
;	      'SOURCE-COMPARE-MERGE-1
;	      (FS:MERGE-PATHNAME-DEFAULTS PATHNAME-1)
;	      (FS:MERGE-PATHNAME-DEFAULTS PATHNAME-2 PATHNAME-1)
;	      (FS:MERGE-PATHNAME-DEFAULTS OUTPUT-PATHNAME PATHNAME-2))))))

(DEFUN SOURCE-COMPARE-MERGE (PATHNAME-1 PATHNAME-2 OUTPUT-PATHNAME)
  "Merge files PATHNAME-1 and PATHNAME-2, putting output in OUTPUT-PATHNAME.
Merging finds those sections of the files which match and those which differ,
just like regular SOURCE-COMPARE.  The output is generated by copying the
matching sections automatically, and asking the user which version to use
for the differing sections.
Type Help at the query to get an explanation of the command interface."
  (ED `(SOURCE-COMPARE-MERGE
	 ,(FS:MERGE-PATHNAME-DEFAULTS PATHNAME-1)
	 ,(FS:MERGE-PATHNAME-DEFAULTS PATHNAME-2 PATHNAME-1)
	 ,(FS:MERGE-PATHNAME-DEFAULTS OUTPUT-PATHNAME PATHNAME-2))))

(DEFUN SOURCE-COMPARE-MERGE-1 (PATHNAME-1 PATHNAME-2 OUTPUT-PATHNAME &AUX FILE-1 FILE-2)
  (LET ((*BATCH-UNDO-SAVE* T))
    (DELETE-INTERVAL *INTERVAL*)
    (DISCARD-UNDO-INFORMATION *INTERVAL*)
    (MUST-REDISPLAY *WINDOW* DIS-ALL)
    (SEND *STANDARD-OUTPUT* :MAKE-COMPLETE)
    (SELECT-WINDOW *WINDOW*)
    (TV:PROCESS-TYPEAHEAD (SEND *WINDOW* :IO-BUFFER)
			  #'(LAMBDA (CH)
			      (COND ((ATOM CH) CH)
				    ((EQ (CAR CH) 'SELECT-WINDOW)
				     (APPLY 'PROCESS-SPECIAL-COMMAND CH)
				     NIL)
				    ((MEMQ (CAR CH) '(CONFIGURATION-CHANGED REDISPLAY))
				     NIL)
				    (T CH))))
    (UNWIND-PROTECT
	(PROGN
	  (SETQ FILE-1 (SRCCOM:CREATE-FILE PATHNAME-1)
		FILE-2 (SRCCOM:CREATE-FILE PATHNAME-2))
	  (LET ((MARKS (SRCCOM:SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING
			 FILE-1 FILE-2 (INTERVAL-STREAM *INTERVAL*))))
	    (SOURCE-COMPARE-MERGE-QUERY MARKS))
	  (WITH-OPEN-FILE (STREAM OUTPUT-PATHNAME '(:OUT))
	    (STREAM-OUT-INTERVAL STREAM *INTERVAL*)
	    (CLOSE STREAM)
	    (SEND STREAM :TRUENAME)))
      (AND FILE-1 (SEND (SRCCOM:FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM:FILE-STREAM FILE-2) :CLOSE)))))

;;;; Buffer editor.

(DEFMAJOR COM-EDIT-BUFFERS-MODE EDIT-BUFFERS-MODE "Edit-Buffers"
	  "Setup for editing the list of ZMACS buffers" ()
  (SET-COMTAB *MODE-COMTAB* '(#/SP COM-DOWN-REAL-LINE
			      #/S COM-EDIT-BUFFERS-SAVE
			      #/s (0 #/S)
			      #/W COM-EDIT-BUFFERS-WRITE
			      #/w (0 #/W)
			      #/R COM-EDIT-BUFFERS-REVERT
			      #/r (0 #/R)
			      #/~ COM-EDIT-BUFFERS-UNMODIFY
			      #/K COM-EDIT-BUFFERS-DELETE
			      #/k (0 #/K)
			      #/D COM-EDIT-BUFFERS-DELETE
			      #/d (0 #/D)
			      #/. COM-EDIT-BUFFERS-SELECT
			      #/C-K COM-EDIT-BUFFERS-DELETE
			      #/C-D COM-EDIT-BUFFERS-DELETE
			      #/U COM-EDIT-BUFFERS-UNDELETE
			      #/u (0 #/U)
			      #/N COM-EDIT-BUFFERS-NO-FILE-IO
			      #/n (0 #/N)
			      #/P COM-EDIT-BUFFERS-PRINT
			      #/p (0 #/P)
			      #/HELP COM-EDIT-BUFFERS-HELP
			      #/RUBOUT COM-EDIT-BUFFERS-REVERSE-UNDELETE
			      #/ABORT COM-EDIT-BUFFERS-ABORT
			      #/END COM-EDIT-BUFFERS-EXIT
			      #/Q COM-EDIT-BUFFERS-EXIT
			      #/q (0 #/Q)))
  (SET-MODE-LINE-LIST (APPEND (MODE-LINE-LIST) '("   End to exit, Abort to cancel"))))

(DEFCOM COM-BUFFER-EDIT "Edit the list of buffers; save, kill, etc." ()
  (KILL-NEW-BUFFER-ON-ABORT (*INTERVAL*)
    (EDIT-BUFFERS))
  DIS-NONE)

(DEFCOM COM-EDIT-BUFFERS "Edit the list of buffers; save, kill, etc." ()
  (KILL-NEW-BUFFER-ON-ABORT (*INTERVAL*)
    (EDIT-BUFFERS))
  DIS-NONE)

(DEFUN EDIT-BUFFERS ()
  (LET ((*INTERVAL*
	  (OR (SEND SELF :FIND-SPECIAL-BUFFER :EDIT-BUFFERS T "Edit-Buffers" T)
	      *INTERVAL*)))
    (MAKE-BUFFER-READ-ONLY *INTERVAL*)
    (COM-EDIT-BUFFERS-MODE)
    (EDIT-BUFFERS-REVERT *INTERVAL*)
    DIS-TEXT))

(DEFPROP EDIT-BUFFERS-MODE EDIT-BUFFERS-REVERT MAJOR-MODE-REVERT-FUNCTION)

(DEFUN EDIT-BUFFERS-REVERT (BUFFER &OPTIONAL IGNORE IGNORE SELECT-P)
  (WITH-READ-ONLY-SUPPRESSED (BUFFER)
    (LET ((*INTERVAL* BUFFER)
	  (OLD-BUFFER)
	  (*BATCH-UNDO-SAVE* T))
      (DOLIST (BUF (HISTORY-LIST (SEND *WINDOW* :BUFFER-HISTORY)))
	(OR (EQ BUF *INTERVAL*) (RETURN (SETQ OLD-BUFFER BUF))))
      (DELETE-INTERVAL *INTERVAL*)
      (DISCARD-UNDO-INFORMATION *INTERVAL*)
      (LET ((STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-FIRST-BP *INTERVAL*))))
	(FORMAT STREAM "Buffers in ZMACS:~2%")
	(DOLIST (B *ZMACS-BUFFER-LIST*)
	  (COND ((NEQ B *INTERVAL*)
		 (SEND STREAM :STRING-OUT
		       (IF (BUFFER-NEEDS-SAVING-P B)
			   (IF (EQ B OLD-BUFFER) " S . " " S   ")
			 (IF (EQ B OLD-BUFFER) "   . " "     ")))
		 (SEND STREAM :STRING-OUT
		       (IF (BUFFER-MODIFIED-P B) " * " "   "))
		 (SEND STREAM :STRING-OUT (BUFFER-NAME B))
		 (SEND STREAM :LINE-PUT 'BUFFER B)
		 (SEND STREAM :TYO #/CR)))))
      (MOVE-BP (POINT) (BEG-LINE (INTERVAL-FIRST-BP *INTERVAL*) 2 T))))
  (IF SELECT-P (MAKE-BUFFER-CURRENT BUFFER)))

(DEFCOM COM-EDIT-BUFFERS-HELP "Explain Edit Buffers commands" ()
  (FORMAT T "You are inside Edit Buffers.  You are editing a list of all ZMACS buffers.
You can move around in the list with the usual cursor motion commands.
Also, you can request to save, write, kill or unmodify buffers.
	D or K	Mark the buffer to be killed.
		Also requests saving, if buffer contains changes.
		Use N to cancel the saving but not cancel the killing.
	U	Cancel all operations on the buffer.
	Rubout	Cancel all operations on previous line, moving up.
	.	Mark this buffer to be selected.
	P	Mark the buffer to be printed.
	S	Mark the buffer to be saved.
	W	Mark the buffer to be written.
	~~	Mark the buffer to be unmodified.
	R	Mark the buffer to be reverted.
	N	Cancel any request for file I//O on the buffer.
	Q	Exit.  Kill//save//revert as requested.
")
  DIS-NONE)

(DEFSUBST EDIT-BUFFERS-LINE-BUFFER (LINE)
  (GETF (LINE-PLIST LINE) 'BUFFER))

(DEFUN EDIT-BUFFERS-MAP-OVER-LINES (N-TIMES FUNCTION)
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (LET ((BP (BEG-LINE (POINT)))
	  (BOTTOM (INTERVAL-LAST-BP *INTERVAL*)))
      (DOTIMES (I (ABS N-TIMES))
	(COND ((MINUSP N-TIMES)
	       (IF (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
		   (RETURN))
	       (SETQ BP (BEG-LINE BP -1 T))))
	(AND (BP-= BP BOTTOM) (RETURN))
	(IF (EDIT-BUFFERS-LINE-BUFFER (BP-LINE BP))
	    (FUNCALL FUNCTION (BP-LINE BP)))
	(AND (PLUSP N-TIMES) (SETQ BP (BEG-LINE BP +1 T))))
      (MOVE-BP (POINT) BP))
    DIS-TEXT))

(DEFCOM COM-EDIT-BUFFERS-SELECT "Mark buffer for selection" ()
  (IF (AND (EDIT-BUFFERS-LINE-BUFFER (BP-LINE (POINT)))
	   (CHAR (CHAR (BP-LINE (POINT)) 0) #/K))
      (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	(MUNG-LINE (BP-LINE (POINT)))
	(SETF (CHAR (BP-LINE (POINT)) 3) #/.)
	(DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
	     (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
	    ((EQ LINE LAST-LINE))
	  (AND (> (LINE-LENGTH LINE) 2)
	       (CHAR= (CHAR LINE 3) #/.)
	       (NEQ LINE (BP-LINE (POINT)))
	       (PROGN (MUNG-LINE LINE)
		      (SETF (CHAR LINE 3) #/SPACE)))))
    (BEEP))
  DIS-TEXT)

;;; Find the most recently selected buffer which is not marked for killing,
;;; and mark it for selection.
(DEFUN EDIT-BUFFERS-DEFAULT-SELECT ()
  (DO ((BUFFERS *ZMACS-BUFFER-LIST* (CDR BUFFERS)))
      ((NULL BUFFERS))
    (LET ((BUFFER (CAR BUFFERS)))
      (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
	   (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
	  ((EQ LINE LAST-LINE))
	(IF (EQ (EDIT-BUFFERS-LINE-BUFFER LINE) BUFFER)
	    (IF (CHAR= (CHAR LINE 0) #/K)
		(RETURN)
	      (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
		(MUNG-LINE LINE)
		(SETF (CHAR LINE 3) #/.))
	      (RETURN-FROM EDIT-BUFFERS-DEFAULT-SELECT DIS-TEXT)))))))


(DEFCOM COM-EDIT-BUFFERS-DELETE "Mark buffer(s) for deletion" ()
  (EDIT-BUFFERS-MAP-OVER-LINES *NUMERIC-ARG* 
			#'(LAMBDA (LINE)
			    (MUNG-LINE LINE)
			    (SETF (CHAR LINE 0) #/K)
			    ;; If this buffer was due to be selected,
			    ;; find some other one instead.
			    (WHEN (CHAR= (CHAR LINE 3) #/.)
			      (SETF (CHAR LINE 3) #/SP)
			      (EDIT-BUFFERS-DEFAULT-SELECT))
			    ;; If buffer modified, assume save it as well as kill.
			    (IF (BUFFER-NEEDS-SAVING-P (EDIT-BUFFERS-LINE-BUFFER LINE))
				(SETF (CHAR LINE 1) #/S)))))

(DEFCOM COM-EDIT-BUFFERS-UNDELETE "Un-mark buffer(s) for deletion.
Also cancels any other operation requested on the buffers" ()
  (EDIT-BUFFERS-MAP-OVER-LINES (IF (AND (NOT *NUMERIC-ARG-P*)
					(> (STRING-LENGTH (BP-LINE (POINT))) 3)
					(CHAR= #/SP (CHAR (BP-LINE (POINT)) 0))
					(CHAR= #/SP (CHAR (BP-LINE (POINT)) 1))
					(CHAR= #/SP (CHAR (BP-LINE (POINT)) 2)))
				   -1
				 *NUMERIC-ARG*)
			#'(LAMBDA (LINE)
			    (MUNG-LINE LINE)
			    (SETF (CHAR LINE 0) #/SPACE)
			    (SETF (CHAR LINE 0) #/SPACE)
			    (SETF (CHAR LINE 0) #/SPACE))))

(DEFCOM COM-EDIT-BUFFERS-REVERSE-UNDELETE "Un-mark buffer(s) upwards for deletion" ()
  (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
  (COM-EDIT-BUFFERS-UNDELETE))

(DEFCOM COM-EDIT-BUFFERS-PRINT "Mark buffer(s) for printing" ()
  (EDIT-BUFFERS-MAP-OVER-LINES *NUMERIC-ARG*
			       #'(LAMBDA (LINE)
				   (MUNG-LINE LINE)
				   (SETF (CHAR LINE 2) #/P))))

(DEFUN SET-LINE-PATHNAME (LINE OPERATION)
  (MUNG-LINE LINE)
  (LET* ((BUFFER (EDIT-BUFFERS-LINE-BUFFER LINE))
	 (PATHNAME (READ-DEFAULTED-PATHNAME (FORMAT NIL "Write buffer ~A to File:"
						   (BUFFER-NAME BUFFER))
					    (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* BUFFER)
					    NIL NIL :WRITE)))
    (SETF (LINE-LENGTH LINE) 5)
    (STRING-NCONC LINE
		  (FORMAT NIL "~A~A into ~A"
			  (IF (BUFFER-MODIFIED-P (EDIT-BUFFERS-LINE-BUFFER LINE))
			      " * " "   ")
			  (EDIT-BUFFERS-LINE-BUFFER LINE)
			  PATHNAME))
    (SETF (GETF (LINE-PLIST LINE) :PATHNAME)
	  PATHNAME))
  (SETF (CHAR LINE 1) OPERATION))

(DEFUN CLEAR-LINE-PATHNAME (LINE OPERATION)
  (MUNG-LINE LINE)
  (SETF (CHAR LINE 1) OPERATION)
  (SETF (LINE-LENGTH LINE) 5)
  (STRING-NCONC LINE
		(IF (BUFFER-MODIFIED-P (EDIT-BUFFERS-LINE-BUFFER LINE))
		    " * " "   ")
		(BUFFER-NAME (EDIT-BUFFERS-LINE-BUFFER LINE)))
  (REMF (LINE-PLIST LINE) ':PATHNAME))
 
(DEFCOM COM-EDIT-BUFFERS-SAVE "Mark buffer(s) for saving" ()
  (EDIT-BUFFERS-MAP-OVER-LINES *NUMERIC-ARG* 
			#'(LAMBDA (LINE)
			    (IF (BUFFER-PATHNAME (EDIT-BUFFERS-LINE-BUFFER LINE))
				(CLEAR-LINE-PATHNAME LINE #/S)
			      (SET-LINE-PATHNAME LINE #/W)))))
 
(DEFCOM COM-EDIT-BUFFERS-NO-FILE-IO "Mark buffer(s) not to be saved, reverted, etc." ()
  (EDIT-BUFFERS-MAP-OVER-LINES (IF (AND (NOT *NUMERIC-ARG-P*)
					(> (STRING-LENGTH (BP-LINE (POINT))) 3)
					(CHAR= #/SP (CHAR (BP-LINE (POINT)) 1)))
				   -1
				 *NUMERIC-ARG*)
			#'(LAMBDA (LINE)
			    (CLEAR-LINE-PATHNAME LINE #/SP))))

(DEFCOM COM-EDIT-BUFFERS-REVERT "Mark buffer(s) to be reverted" ()
  (EDIT-BUFFERS-MAP-OVER-LINES *NUMERIC-ARG* 
			#'(LAMBDA (LINE)
			    ;; Only buffers with files can be reverted.
			    (IF (BUFFER-PATHNAME (EDIT-BUFFERS-LINE-BUFFER LINE))
				(CLEAR-LINE-PATHNAME LINE #/R)))))
 
(DEFCOM COM-EDIT-BUFFERS-WRITE "Mark buffer(s) to be written" ()
  (EDIT-BUFFERS-MAP-OVER-LINES *NUMERIC-ARG* 
			#'(LAMBDA (LINE)
			    (SET-LINE-PATHNAME LINE #/W))))
 
(DEFCOM COM-EDIT-BUFFERS-UNMODIFY "Mark buffer(s) to be marked as unmodified" ()
  (EDIT-BUFFERS-MAP-OVER-LINES *NUMERIC-ARG* 
			#'(LAMBDA (LINE)
			    (CLEAR-LINE-PATHNAME LINE #/~))))

(DEFCOM COM-EDIT-BUFFERS-ABORT "Abort out of Edit Buffers" ()
  (SEND SELF :EXIT-SPECIAL-BUFFER))

(DEFCOM COM-EDIT-BUFFERS-EXIT "Leave Edit Buffers, killing and saving as requested.
Displays the files to be deleted and/or printed, then asks you to confirm." ()
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
       BUFFER-TO-SELECT)
      ((EQ LINE LAST-LINE)
       (SEND SELF :EXIT-SPECIAL-BUFFER T *INTERVAL*)
       (IF BUFFER-TO-SELECT
	   (MAKE-BUFFER-CURRENT BUFFER-TO-SELECT)))
    (LET ((BUFFER (EDIT-BUFFERS-LINE-BUFFER LINE))
	  (PATHNAME (GETF (LINE-PLIST LINE) :PATHNAME)))
      (COND (BUFFER
	     (CASE (CHAR LINE 1)
	       (#/S (SAVE-BUFFER BUFFER))
	       (#/W
		(FILE-RETRY-NEW-PATHNAME (PATHNAME FS:FILE-ERROR)
		  (SET-BUFFER-PATHNAME PATHNAME BUFFER)
		  (SET-BUFFER-FILE-ID BUFFER NIL)
		  (WRITE-FILE-INTERNAL PATHNAME BUFFER)))
	       (#/R (REVERT-BUFFER BUFFER))
	       (#/~ (NOT-MODIFIED BUFFER)))
	     (WHEN (CHAR= (CHAR LINE 2) #/P)
	       (FORMAT *QUERY-IO* "~&Attempting transmission of ~A: " (BUFFER-NAME BUFFER))
	       (PRINT-BUFFER-1 BUFFER))
	     (IF (CHAR= (CHAR LINE 3) #/.)
		 (SETQ BUFFER-TO-SELECT BUFFER))
	     (IF (CHAR= (CHAR LINE 0) #/K)
		 (KILL-BUFFER BUFFER T))))))
  DIS-BPS)



