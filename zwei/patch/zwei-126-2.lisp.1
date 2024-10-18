;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.2
;;; Reason:
;;;  DIRED.LISP modified extensively -- here are the major changes:
;;;  
;;;  DIRED-PATHNAME-EQUAL accepts more types of arguments;
;;;  COM-DIRED-SUBDIRECTORY has added functionality for 
;;;    inserting and removing subdirectory contents from a 
;;;    Dired display;
;;;  New command `>' introduced, which moves cursor to 
;;;    most recent version of current file;
;;;  HELP documentation updated and reformatted 
;;;    so as to arrive in semantic screenfuls;
;;;  DIRED-PROCESS-FILES now makes sure that it has
;;;    a file to process before doing anything.
;;;    
;;;  ---saz
;;; Written 19-Aug-88 18:09:17 by saz at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with Experimental System 126.55, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, Experimental ZWEI 126.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:12:05
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

;;;For DIRED's purposes, the host called "LOCAL" and the "local host"
;;;are equivalent. The user may ask for DIRED of "LM:FOO;", and later
;;;give DIRED a pathname on his local host, specified by name.  In this
;;;situation, for example, the Copy-File command used to say "copied to
;;;a file not in this display".  Solution: use the following function to
;;;compare pathnames.
(defun dired-pathname-equal (path1 path2)
  (typecase path1
    (string (setq path1 (pathname path1))))
  (typecase path2
    (string (setq path2 (pathname path2))))
  (when (and (typep path1 'pathname) (typep path2 'pathname))
    (let ((lm (fs:get-pathname-host "LOCAL" :maybe-it-is-not-defined?)))
      (cond
	((null lm))
	((eq (pathname-host path1) lm)
	 (setq path1 (send path1 :new-pathname :host si:local-host)))
	((eq (pathname-host path2) lm)
	 (setq path2 (send path2 :new-pathname :host si:local-host))))
      (fs:pathname-equal path1 path2))))

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:12:25
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

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
			      #/ COM-DIRED-SRCCOM-FILE
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
			      #/> com-dired-go-to-most-recent-version
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

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:13:34
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(defcom com-dired-subdirectory "Insert or remove the display of files in this subdirectory.
The files of the subdirectory mentioned on this line are inserted into the DIRED buffer,
underneath this line and indented one space in, as fully-functional DIRED file lines. 
If the subdirectory contents are already present in the DIRED buffer, this command removes
them from the display (but does not delete them).
/
With an argument, prompts for a wildcarded specification of files in the directory to 
insert, as opposed to inserting all files in the subdirectory (the default). " ()
  (let* ((line (bp-line (point)))
	 (line-plist (line-plist line)))
    (cond ((null line-plist)
	   (barf "This line does not contain a legal file listing."))	;test to see if on second line of Dired buffer
	  ((null (getf line-plist 'level))	;test to see if on first line of Dired buffer
	   (barf "Directory headers cannot be opened in this way -- use a new Dired."))
	  (t
	   (let*
	     ((subfiles-displayed-currently	; nil or a pathname w/ some
		(getf line-plist 'contents-present))	; (or all) wildcard components
	      (pathname (getf line-plist ':pathname))
	      (current-directory (send pathname :pathname-as-directory))
	      (directory-p (getf line-plist ':directory))
	      directory directory-on-current-line)
	     (if directory-p			; User typed `S' on a directory line -- open it up
		 (progn
		   (setq directory current-directory
			 directory-on-current-line (send directory :new-pathname :name :wild :type :wild :version :wild))
		   (cond ((not *numeric-arg-p*)		
			  (cond ((dired-pathname-equal subfiles-displayed-currently directory-on-current-line)
				 ;;Contents are already displayed -- user wants to close it.
				 (dired-close-line-subdirectory line))
				((null subfiles-displayed-currently)
				 ;;No subfiles currently displayed -- ok to proceed normally
				 (dired-open-line-subdirectory line directory-on-current-line))
				(t (if (y-or-n-p "~A already has some subfiles displayed.  ~
  						    Replace display with full listing? "
						 directory-on-current-line)
				       (progn
					 ;; first get rid of current contents
					 (dired-close-line-subdirectory line)	
					 ;; and then replace with new contents
					 (dired-open-line-subdirectory line directory-on-current-line))
				     ;; Otherwise, check to see if user wanted to close up the partial listing
				     (if (y-or-n-p "Remove displayed subfiles of ~A ? " directory-on-current-line)
					 (dired-close-line-subdirectory line)
				       (return-from com-dired-subdirectory dis-none))))))
			 (t			; numeric arg was supplied
			  (if subfiles-displayed-currently	; some or all contents already being displayed
			      (if (not (y-or-n-p "~A already has some subfiles displayed.  ~
  							Replace display with another listing? "
						 directory-on-current-line))
				  ;;If user reneges, do nothing
				  (return-from com-dired-subdirectory dis-none)))
				;;Close up, prepare for new display
			  (setq subfiles-displayed-currently	; user specifies replacement listing (default to all)
				(read-directory-name
				  (format nil "Subfiles of ~A to display: "
					  directory-on-current-line)
				  directory-on-current-line))
			  (if (not (send directory-on-current-line :pathname-match
					 subfiles-displayed-currently))
			      ;; I.e., if user explicitly specifies a file not in the directory
			      (progn
				(beep)
				(if (y-or-n-p "~&~A is not a file in ~A; do you want a separate DIRED of ~A ?"
					      subfiles-displayed-currently
					      directory-on-current-line
					      subfiles-displayed-currently)
				    (directory-edit subfiles-displayed-currently t)	;new call to Dired
				  ;;Otherwise no-op -- user mistyped
				  (return-from com-dired-subdirectory dis-none)))
			    (progn (dired-close-line-subdirectory line t)	;t means a reopen is pending.
				   (dired-open-line-subdirectory line subfiles-displayed-currently))))))
	       ;;If we get here, then we cannot be on a line representing a directory -- user wants to close up
	       (cond (*numeric-arg-p*
		      (barf "Invalid numeric argument -- not on a subdirectory line."))
		     ((zerop (dired-line-level line))
		      ;;Can't close up 
		      (barf "~A: ~A is the top level display and cannot be removed."
			    (pathname-host pathname)
			    (send pathname :string-for-directory)))
		     (t
		      ;;Hence, this must be a legal request to close up; move
		      ;;to parent line and do it.  (Note we know from the fact
		      ;;that we got here that we cannot be on the directory
		      ;;line itself!)
		      (dired-close-line-subdirectory (parent-line line))
		      (com-up-real-line))))))))
  dis-text)

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:13:37
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(defun parent-line (line)
  ;; Find and return the line in the display representing the directory
  ;; to which this line's file or directory belongs.
  (let ((child-level (dired-line-level line)))
    (do ((parent-candidate (line-previous line) (line-previous parent-candidate)))
       ((null parent-candidate)
	(barf "Top of display."))
    (let ((parent-level (dired-line-level parent-candidate)))
      (if (< parent-level
	     child-level)
	  (return parent-candidate))))))


))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:14:35
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(DEFUN DIRED-OPEN-LINE-SUBDIRECTORY (LINE WILD-PATHNAME &AUX DIRECTORY)
  (IF (SETQ DIRECTORY (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
      (BARF "Files for subdirectory ~A are already in display." directory))
  (UNLESS (PATHNAMEP WILD-PATHNAME)
    (SETQ WILD-PATHNAME (SEND (SEND (DIRED-LINE-PATHNAME-OR-BARF LINE)
				    :PATHNAME-AS-DIRECTORY)
			      :NEW-PATHNAME :NAME :WILD :TYPE :WILD :VERSION :WILD)))
  (SETQ DIRECTORY (FS:DIRECTORY-LIST WILD-PATHNAME :DELETED :SORTED))
  (if (not (find-if #'car directory))
      ;;FS:DIRECTORY-LIST returned only the directory pathname -- no files!
      (progn (format *query-io* "No files matching ~A." wild-pathname)
	     (setf (getf (line-plist line) 'contents-present) nil))
    (LET* ((*BATCH-UNDO-SAVE* T))		; otherwise, there are files to insert.
      (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	(SETF (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT) WILD-PATHNAME)
	(LET ((NEXT-PLIST (LINE-PLIST (LINE-NEXT LINE)))
	      (STREAM (INTERVAL-STREAM-INTO-BP (CREATE-BP (LINE-NEXT LINE) 0))))
	  (DIRED-INSERT-DIRECTORY DIRECTORY STREAM
				  (1+ (DIRED-LINE-LEVEL LINE)))
	  ;; Restore the plist, now clobbered, of the following line.
	  (SETF (LINE-PLIST (BP-LINE (SEND STREAM :READ-BP))) NEXT-PLIST))))))

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:14:38
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(defun dired-close-line-subdirectory (line &optional pending-reopen)
  (let* ((*batch-undo-save* t)
	 (wild-pathname (getf (line-plist line) 'contents-present)))
    (if (and (null pending-reopen)		;t if we are removing contents in
						;preparation for a pending open-line-subdirectory --
						;this way, we won't barf in this way when user types
						;bad subfile spec...
	     (null wild-pathname))
	(barf "No subfiles are present for ~A."
      	      (getf (line-plist line) ':pathname))
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



))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:14:52
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(DEFCOM COM-DIRED-HELP "Explain use of DIRED commands." ()
  (FORMAT T "You are in the directory editor.  The commands are:

        D       Mark current file for deletion.
                 (Also invoked by K, Control-D, and Control-K.)
        P       Print current file on the standard hardcopy device.
        A       Apply function to current file on exit.
        U       Undelete current file, or file just above the cursor.
                 Also used to cancel a Print or Apply function request.
        R       Rename current file.  You type the new filename in a mini buffer.
        C       Copy current file.  You type the new filename in a mini buffer.
        L       Load current file (LISP code or QFASL file).
        Rubout  Undelete file above the cursor.
        Space   Move to the next line.
        ----- Above commands repeat with a numeric argument,   -----
        ----- repeating backwards if the argument is negative. -----
        S       Insert or remove the display of this subdirectory.
                 The files in the subdirectory are indented ~R additional space~:P.
                 By default it inserts all the files of the subdirectory; however
                  by giving this command a numeric argument you will be prompted
                  for a wildcarded pathname specifying a subset of the subdirectory's
                  contents.
                 If the subdirectory files are already inserted, then S with no
                  argument command offers to remove them from the display.
                 Removing them from the display does NOT delete the files!
        N       Move to the next file with more than ~D versions.
                 (This number /"~:*~D/" is the value of ~S.)
        H       Mark excess versions (/"Hogs/") of current file for deletion.
        Q       Quit.  You will be shown the files to be deleted and asked for
                 confirmation.  In this display, /":/" means this file is a link,
                 /">/" means this is the newest version of this file, /"!/" means this file is
                 not backed-up, and /"$/" means do not reap this file.
        X       Execute.  Like Quit, but stay in the DIRED buffer afterwards.
        !       Move to the next file that is not backed up on tape.
        @       Complement @ (dont-delete) flag
        #       Complement # (dont-supersede) flag
        $       Complement $ (dont-reap) flag
        ,       Display the file attributes.  For a source file, display the -*- line.
                 For a QFASL file, display the source -*- line and compilation data.
        .       Change properties of current file.
        E       Edit current file, or run DIRED on a subdirectory.
        F       Edit current file, or run DIRED on a subdirectory, not now, but when you exit.
        C-Sh-E  Edit current file in Two Window mode (DIRED remains visible).
        <       Edit (run DIRED on) the superior directory of this directory.
        >       Go to newest version of current file.
        V       View current file (doesn't read it all in).
        =       Source Compare current file with the newest version.
               Source Compare current file with a specified file.

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


;;;; Random DIRED commands that operate on one or n files.

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:15:33
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(defcom com-dired-go-to-most-recent-version 
	"Find the highest version of current file." ()
  (let ((line-plist (line-plist (bp-line (point)))))
    (if (null line-plist)
	(barf "The current line does not specify a file.")
      (if (getf (line-plist (bp-line (point))) ':directory)
	  (barf "Directories do not have versions.")
	(do* ((line (bp-line (point)) (line-next line))
	      (line-plist (line-plist line) (line-plist line))
	      (pathname (getf line-plist ':pathname) (getf line-plist ':pathname))
	      (name-component (send pathname ':name) (send pathname ':name))
	      (suffix (send pathname ':type) (send pathname ':type))
	      (next-line-pathname (send (getf (line-plist (line-next line)) ':pathname) ':name)
				  (send (getf (line-plist (line-next line)) ':pathname) ':name))
	      (next-line-suffix (send (getf (line-plist (line-next line)) ':pathname) ':type)
				(send (getf (line-plist (line-next line)) ':pathname) ':type)))
	     ((cond ((null (getf line-plist 'level))
		     (barf "End of display reached."))
		    ((not (and (string-equal name-component
					     next-line-pathname)
			       (string-equal suffix
					     next-line-suffix)))
		     (format *query-io* "Latest version of file: ~A." pathname)
		     (move-bp (point) line 0))
		    (t nil)))))))
  dis-bps)					
	

;;;Source Compare interface

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:16:40
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(DEFUN DIRED-PROCESS-FILES ()
  "Perform all the operations requested on files in the DIRED buffer.
Returns T if user typed E or Y or Q, NIL if user typed N."
  (let ((buffer-pathname (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*)))
    ;;A messed-up DIRED buffer (or other type of buffer) will not have an
    ;;associated DIRED pathname; simply return.
    (when buffer-pathname
      (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
	   (UNDELETEABLE (SEND buffer-pathname :UNDELETABLE-P))
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
										(BARF "End of file encountered.")))))))
							    (DIRED-DO-FILE-LIST APPLY-FILES
										'DIRED-APPLY-FUNCTION NIL))))
						   (AND DELETE-FILES
							(DIRED-DO-FILE-LIST DELETE-FILES 'DIRED-DELETE-FILE "delete"
									    :DELETE-MULTIPLE-FILES
									    #'(LAMBDA (LINE)
										(SETF (GETF (LINE-PLIST LINE) ':DELETED) T))
									    #'(lambda (line)
										(setf (char line 0) #/SP))))
						   (AND UNDELETE-FILES
							(DIRED-DO-FILE-LIST UNDELETE-FILES 'DIRED-UNDELETE-FILE
									    "undelete"
									    :UNDELETE-MULTIPLE-FILES
									    (LAMBDA (LINE)
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
							 (WHEN (AND (GETF (LINE-PLIST LINE) ':DIRECTORY)
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
	    (#/A (PUSH LINE APPLY-FILES))))))))

))

; From modified file DJ: L.ZWEI; DIRED.LISP#339 at 19-Aug-88 18:20:14
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-EXIT "Leave DIRED, performing deletions//visiting//printing etc.
Displays the files to be deleted and/or otherwise acted upon, then asks you to confirm." ()
  (if (and (boundp '*menu-driven-dired*)
	   *menu-driven-dired*)
      (com-menu-dired-exit)
    (IF (DIRED-PROCESS-FILES)
	(SEND *WINDOW* :EXIT-SPECIAL-BUFFER NIL *INTERVAL*)))
  DIS-BPS)

))
