;;; -*- Mode:Lisp;Package:FS;Base:10.;Fonts:CPTFONT -*-
;;; Restore functions for the dump system
;;; Robert P. Krajewski (c) 1984 Lisp Machine Incorporated
;;;

;;; Because of the file iterator, ordering of the files as they are dumped is basically
;;; alphabetical.  However, a directory of depth n always comes before a directory of
;;; depth n+1, n components being otherwise the same.
(DEFUN DIRECTORY-DEPTH (DIR)
  "Does NOT work for the root directory.  DIR is a string or a list."
  (IF (LISTP DIR) (LENGTH DIR) 1))

(DEFUN FILE-DIRECTORY-EQUAL (DIR1 DIR2)
  "Returns non-() iff DIR1 and DIR2 are the same directory."
  (COND ((NULL DIR1) (NULL DIR2))
	((NULL DIR2) ())
	((STRINGP DIR1)
	 (COND ((STRINGP DIR2) (STRING-EQUAL DIR1 DIR2))
	       ((NULL (CDR DIR2)) (STRING-EQUAL DIR1 (FIRST DIR2)))
	       (T ())))
	((STRINGP DIR2)
	 (AND (NULL (CDR DIR1)) (STRING-EQUAL DIR2 (FIRST DIR1))))
	((= (LENGTH DIR1) (LENGTH DIR2))
	 (AND (STRING-EQUAL (CAR DIR1) (CAR DIR2))
	      (FILE-DIRECTORY-EQUAL (CDR DIR2) (CDR DIR2))))
	(T ())))
	      
(DEFUN FILE-DIRECTORY-LESSP (DIR1 DIR2)
  "Returns non-() iff DIR1 would come before DIR2 in a dump.
The ordering of files is as follows: At any level, files in alpha order (they are kept this
way by the local file system), then subdirectories."
  (COND ((NULL DIR1)
	 (NOT (NULL DIR2)))
	((NULL DIR2) ())
	((STRINGP DIR1)
	 (STRING-LESSP DIR1 (IF (STRINGP DIR2) DIR2 (FIRST DIR2))))
	((STRINGP DIR2)
	 (STRING-LESSP (FIRST DIR1) DIR2))
	((STRING-LESSP (FIRST DIR1) (FIRST DIR2)) T)
	((STRING-EQUAL (FIRST DIR1) (FIRST DIR2))
	 (FILE-DIRECTORY-LESSP (CDR DIR1) (CDR DIR2)))
	(T ())))

;;; These function names have the _ character in front of them so that they will not
;;; be confused with internal file system functions.
(DEFUN _FILE-LESSP (FILE1 FILE2 IGNORE-VERSION)
  "Both pathnames must be from the same host, since this function doesn't check it.
Also NAMEs are assumed to be unstructured (strings), and the versions, if compared, have to
be numeric."
  (OR ; Try the simple things first
    (FILE-DIRECTORY-LESSP (SEND FILE1 ':DIRECTORY) (SEND FILE2 ':DIRECTORY))
    (STRING-LESSP (SEND FILE1 ':NAME) (SEND FILE2 ':NAME))
    (STRING-LESSP (SEND FILE1 ':TYPE) (SEND FILE2 ':TYPE))
    IGNORE-VERSION
    (< (SEND FILE1 ':VERSION) (SEND FILE2 ':VERSION))))

(DEFUN _FILE-EQUAL (FILE1 FILE2 IGNORE-VERSION)
  "Both pathnames must be from the same host, since this function doesn't check it.
Also NAMEs are assumed to be unstructured (strings), and the versions, if compared, have to
be numeric."
  (AND ; Try the simple things first
    (STRING-EQUAL (SEND FILE1 ':NAME) (SEND FILE2 ':NAME))
    (STRING-EQUAL (SEND FILE1 ':TYPE) (SEND FILE2 ':TYPE))
    (FILE-DIRECTORY-EQUAL (SEND FILE1 ':DIRECTORY) (SEND FILE2 ':DIRECTORY))
    IGNORE-VERSION
    (= (SEND FILE1 ':VERSION) (SEND FILE2 ':VERSION))))

;;; File Retrieval structures
;;; To speed up retrieval a bit, the information about what files live on what tapes is cached
;;; in a hash table, where the key is a pathname.
;;; ``Appearance'' below means a tape.
;;; As the searching functions go back over older and older dump records, they deposit all
;;; the dumped pathnames in the table, and update information accordingly if they are
;;; actually reading new data from the dump record files.
;;; The key is a pathname:
;;;  If the pathname has a numeric version, a tape-entry is returned whose :TAPE-NAME
;;;    property is the tape on which it lives
;;;  If the pathname has the :NEWEST version, the version number which is latest (in all
;;;    the dumps gobbled up so far) is returned
;;;  If the pathname has the name, type and version NIL, a cons of the latest appearances of
;;;    the directory in full dump and a list of all the incremental dumps after it (in
;;;    chronological order).
;;;
;;; Here is the algorithm for reading in a tape file (of the form MAGTAPE;DUMP-X.TEXT):
;;;  Open the file up, get the attributes;
;;;  For every tape entry TE in the file:
;;;    TE-PUT the tape name on TE's :TAPE-NAME property
;;;    If it's the first one and this tape isn't the first of the dump, link the last tape's
;;;      previous entry to TE.
;;;     otherwise link the previous to TE.
;;;    If TE is the first of its directory, associate the directory with the dump.
;;;    If TE's :NEWEST counterpart isn't the tape in file table, put it there (call it P).
;;;      If the numeric version of TE is greater than P's (doing gethash), replace it.
;;;  Update the tape-directory.

(DEFVAR *TAPE-DIRECTORY* ()
  "A list of (tape-name dump-attributes first-tape-entry last-tape-entry recdump). 
Any tape that appears on this list has been read into the *TAPE-FILE-TABLE*
The tape-entry, of course, may be pointed to, and most likely points to the next in
this particular file.
The RECDUMP is an element of the tape directory.
If FIRST-TAPE-ENTRY is (), then the tape file really hasn't been gobbled.")

(DEFMACRO TDIR-ATTRIBUTES (TDIR-ELEMENT) `(SECOND ,TDIR-ELEMENT))
(DEFMACRO TDIR-TE1 (TDIR-ELEMENT) `(THIRD ,TDIR-ELEMENT))
(DEFMACRO TDIR-LAST-TE (TDIR-ELEMENT) `(FOURTH ,TDIR-ELEMENT))
(DEFMACRO TDIR-DR (TDIR-ELEMENT) `(FIFTH ,TDIR-ELEMENT))

(DEFVAR *TAPE-FILE-TABLE* (MAKE-HASH-TABLE ':SIZE 300.))

(DEFVAR *DUMP-RECORDS* () "A list of dump records (RECDUMP-*).")

(DEFUN READ-DUMP-RECORDS (&OPTIONAL &KEY (RECORD-HOST *DEFAULT-RECORD-HOST*) FORCE-P)
  (AND (OR FORCE-P (NULL *DUMP-RECORDS*))
       (SETQ *DUMP-RECORDS* (READ-TAPE-DIRECTORY RECORD-HOST))))

(DEFUN FIND-DUMP-RECORD-CONTAINING-TAPE (TAPE)
  (FIRST (MEM #'(LAMBDA (TAPE RD) (MEM #'STRING-EQUAL TAPE (RECDUMP-TAPES RD)))
	      TAPE *DUMP-RECORDS*)))

(DEFMACRO DO-DUMP-RECORDS ((DUMP-RECORD) &BODY BODY)
  "Do BODY with each completed DUMP-RECORD bound to each element of *DUMP-RECORDS*.
It's implemented by DOLIST, so you can RETURN out of it."
  `(DOLIST (,DUMP-RECORD *DUMP-RECORDS*) (WHEN (RECDUMP-COMPLETED ,DUMP-RECORD) ,@BODY)))

(DEFUN ASSURE-TAPE-INFO-LOADED (TAPE)
  (IF (NOT (TDIR-TE1 (ASS #'STRING-EQUAL TAPE *TAPE-DIRECTORY*)))
      (GOBBLE-TAPE-INFORMATION TAPE *DEFAULT-RECORD-HOST*)))

(DEFUN TABLE-PUT-NEWEST-VERSION (PATHNAME)
  (PUTHASH (SEND PATHNAME ':NEW-VERSION ':NEWEST) (SEND PATHNAME ':VERSION)
	   *TAPE-FILE-TABLE*))

(DEFSUBST DIRECTORY-PATHNAME-FOR-TABLE (FILE)
  (SEND FILE ':NEW-PATHNAME ':NAME () ':TYPE () ':VERSION ()))

(DEFUN DIRECTORY-DUMP-TAPES (FILE)
  (ASSURE-FILE-DUMP-INFO-LOADED FILE)
  (GETHASH (DIRECTORY-PATHNAME-FOR-TABLE FILE) *TAPE-FILE-TABLE*))

(DEFUN DUMP-TAPE-INFORMATION (TAPE-NAME)
  (ASSURE-TAPE-INFO-LOADED TAPE-NAME)
  (ASS #'STRING-EQUAL TAPE-NAME *TAPE-DIRECTORY*))

(DEFUN DUMP-TAPE-TIME (TAPE)
  (GET-DUMP-ATTRIBUTE ':DUMP-STARTED (SECOND (DUMP-TAPE-INFORMATION TAPE))))

(DEFUN DUMP-TAPE-FIRST-PATHNAME (TAPE)
  (GET-DUMP-ATTRIBUTE ':FIRST-PATHNAME (TDIR-ATTRIBUTES (DUMP-TAPE-INFORMATION TAPE))))

(DEFUN DUMP-TAPE-LAST-PATHNAME (TAPE)
  (GET-DUMP-ATTRIBUTE ':LAST-PATHNAME (TDIR-ATTRIBUTES (DUMP-TAPE-INFORMATION TAPE))))

(DEFSUBST DIRECTORY-FULL-DUMPS (FILE) (FIRST (DIRECTORY-DUMP-TAPES FILE)))

(DEFSUBST DIRECTORY-INcREMENTAL-DUMPS (FILE) (REST1 (DIRECTORY-DUMP-TAPES FILE)))

(DEFUN GOBBLE-TAPE-INFORMATION (TAPE-NAME RECORD-HOST
				&AUX VERSION INFO-ALIST FIRST-TE last-te DUMP)
  "Add the appropriate info to *TAPE-DIRECTORY* and the file table."
  (SETQ DUMP (FIND-DUMP-RECORD-CONTAINING-TAPE TAPE-NAME))
  (WITH-OPEN-FILE (S (TAPE-ENTRY-FILE RECORD-HOST TAPE-NAME)
		     ':DIRECTION ':INPUT) ; FNF error should cause errors...
    (FORMAT T "~&Reading information for tape ~A ..." TAPE-NAME)
    (MULTIPLE-VALUE (VERSION INFO-ALIST) (READ-TAPE-ENTRY-STREAM-BEGINNING S))
    (IF ( VERSION 4) (FERROR () "Can't read version ~D tape entry files." VERSION)
      (SETQ FIRST-TE (READ-SINGLE-TAPE-ENTRY S))
      (PROG1 ; return value of this DO
	(DO* ((<TE () TE)	; previous tape entry (from this file, n.b.)
	      (TE FIRST-TE (READ-SINGLE-TAPE-ENTRY S))
	      (TE-PATHNAME (TE-PATHNAME TE) (TE-PATHNAME TE))
	      (TE-P-DIRECTORY (SEND TE-PATHNAME ':DIRECTORY) (SEND TE-PATHNAME ':DIRECTORY))
	      (TE-P-NAME (SEND TE-PATHNAME ':NAME) (SEND TE-PATHNAME ':NAME))
	      (TE-P-TYPE (SEND TE-PATHNAME ':TYPE) (SEND TE-PATHNAME ':TYPE))
;	      (TE-P-VERSION (SEND TE-PATHNAME ':VERSION) (SEND TE-PATHNAME ':VERSION))
	      RUNNING-DIRECTORY RUNNING-NAME RUNNING-TYPE) ; to do version and directory hacking
	     ((NULL TE)
	      (TABLE-PUT-NEWEST-VERSION (TE-PATHNAME <TE))
	      (setq last-te <TE))			; ends up being the last tape entry
	  (IF <TE (SETF (TE-NEXT <TE) TE))	; link up
	  (TE-PUT TE ':TAPE-NAME TAPE-NAME)
	  (PUTHASH TE-PATHNAME TE *TAPE-FILE-TABLE*)
	  (WHEN (OR (NOT (EQUAL RUNNING-DIRECTORY TE-P-DIRECTORY))
		    (NOT (EQUAL RUNNING-NAME TE-P-NAME))
		    (NOT (EQUAL RUNNING-TYPE TE-P-TYPE)))
	    (TABLE-PUT-NEWEST-VERSION TE-PATHNAME)
	    (SETQ RUNNING-NAME () RUNNING-TYPE ()))
	  (UNLESS (EQUAL RUNNING-DIRECTORY TE-P-DIRECTORY)
	    ;; Directory changed; add info
	    (SETQ RUNNING-NAME () RUNNING-TYPE ())
	    (AND <TE (TABLE-PUT-NEWEST-VERSION (TE-PATHNAME <TE)))
	    (LET* ((DPN (DIRECTORY-PATHNAME-FOR-TABLE TE-PATHNAME))
		   (DTAPES (GETHASH DPN *TAPE-FILE-TABLE*)))
	      (cond ((EQ (RECDUMP-TYPE DUMP) ':FULL)
		     (IF (NULL DTAPES) (PUTHASH DPN (NCONS (NCONS TAPE-NAME))
						*TAPE-FILE-TABLE*)
		       (SETF (CAR DTAPES) (NCONC (car dtapes) (NCONS TAPE-NAME)))))
		    (t
		     (IF (NULL DTAPES) (PUTHASH DPN (LIST () TAPE-NAME) *TAPE-FILE-TABLE*)
		       (SETF (CDR DTAPES) (NCONC (CDR DTAPES) (NCONS TAPE-NAME)))))))
	    (SETQ RUNNING-DIRECTORY TE-P-DIRECTORY)))
	(PUSH (LIST TAPE-NAME INFO-ALIST FIRST-TE LAST-TE DUMP) *TAPE-DIRECTORY*)))))

(DEFUN ASSURE-FILE-DUMP-INFO-LOADED (FILE)
  "Bring all dump info (back to the last full dump) in core for FILE's host.
Actually, only the HOST component of the file has to be non-NIL."
  (DO-DUMP-RECORDS (DR)
    (WHEN (AND (EQ (SEND FILE ':HOST) (RECDUMP-HOST DR)))
      (DOLIST (TAPE (RECDUMP-TAPES DR))
	(ASSURE-TAPE-INFO-LOADED TAPE)
	(IF (FILE-DIRECTORY-LESSP (SEND FILE ':DIRECTORY)
				  (SEND (DUMP-TAPE-LAST-PATHNAME TAPE) ':DIRECTORY))
	    (RETURN))) ; from DOLIST, but not from DO-DUMP-RECORDS
      (IF (EQ ':FULL (RECDUMP-TYPE DR)) (RETURN)))))

(defun file-last-dump (file) ; all fields must be specified
  "Returns the tape on which this file was dumped last (or NIL)."
  (ASSURE-FILE-DUMP-INFO-LOADED FILE)
  (TE-GET (GETHASH FILE *TAPE-FILE-TABLE*) ':TAPE-NAME))

(DEFUN DIRECTORY-LAST-FULL-DUMP (FILE)
  "Returns the tape on which this file's directory was dumped in full most recently (or NIL)."
  (ASSURE-FILE-DUMP-INFO-LOADED file)
  (FIRST (DIRECTORY-full-DUMPs FILE)))

(DEFUN DIRECTORY-ALL-RECENT-DUMPS (&special DIR) ;; grrr
  "Actually returns SPECS"
  (ASSURE-FILE-DUMP-INFO-LOADED DIR)
  (mapcar #'(lambda (tape) (cons tape (ncons dir)))
	  (APPEND (DIRECTORY-INCREMENTAL-DUMPS DIR) (NCONS (DIRECTORY-LAST-FULL-DUMP DIR)))))
  
(DEFSUBST PATHNAME-COMPONENT-EQUAL (OBJECT PATHNAME COMPONENT)
  "Returns T if OBJECT and the pathname component are equal."
  (EQUAL OBJECT (SEND PATHNAME COMPONENT)))

(DEFUN RETRIEVE-FROM-TAPE (HOST PATHNAMES DIRECTORIES
			  &AUX CURRENT-DIR T-DIR POSSIBILITIES)
  (LOOP AS stream = (lexpr-funcall 'MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL)
	UNTIL (or (and (null directories) (null pathnames)) (errorp stream))
	DO (setq t-dir (send stream ':directory))
	DO (unless (equal t-dir current-dir)
	     (and current-dir (setq directories (delete current-dir directories)))
	     (setq current-dir t-dir))
	DO (cond ((member t-dir directories)
		  (FS-COPY-FILE STREAM HOST))
		 ((setq possibilities
			(mem #'(lambda (dir pn) (pathname-component-equal dir pn ':directory))
			     t-dir possibilities))
		  (and (setq possibilities	;
			     (mem #'(lambda (n pn) (pathname-component-equal n pn ':name))
				  (send stream ':name) possibilities))
		       (setq possibilities
			     (mem #'(lambda (ty pn) (pathname-component-equal ty pn ':type))
				  (send stream ':type) possibilities))
		       (setq possibilities
			     (mem #'(lambda (v pn) (pathname-component-equal v pn ':version))
				  (send stream ':version) possibilities))
		       (progn
			 (fs-copy-file stream host)
			 (setq pathnames (delq (car possibilities) pathnames))))))
	DO (send stream ':CLOSE))
  (when (or pathnames directories)
    (tv:beep)
    (format t "~%Bug !! End of tape reached and yet there are still things to retrieve:~2%")
    (format t "Files: ") (format:print-list t "~A" pathnames)
    (format t "~%Directories: ") (format:print-list t "~A" directories)
    (format t "~2%Please tell RpK@LMI-East about this.~%"))
  (format t "~&Retrieval from this tape finished.~%")
  (mt-rewind)
  (mt-offline))

(defun ask-pathname-or-nil (defaults format-string &rest format-args)
  (let ((string (lexpr-funcall #'prompt-and-read ':string-or-nil format-string format-args)))
    (and string
	 (merge-pathname-defaults (parse-pathname string () defaults) defaults))))

(defun ask-directory-or-nil (defaults extra-message)
  (let ((pn (ask-pathname-or-nil defaults "Directory ~A: " extra-message)))
    (and pn (directory-pathname-for-table pn))))

(defun merge-specs (new old &aux tape)
  "Merge the NEW list of specs with the OLD, and return it."
  (dolist (nspec new)
    (setq tape (car nspec))
    (let ((ospec (ass #'string-equal tape old)))
      (if ospec (nconc Ospec (cdr nspec))
	(push nspec old))))
  old)

(defun retrieval-loop (specs &aux files dirs host)
  "SPECS is a list of (tape . pathnames)
The caller should make sure that no tape is STRING-EQUAL to any other for efficiency."
  (format t "~2%Get the following tapes ready: ~3%")
  (format:print-list t "~A" (mapcar #'car specs))
  (format t "~3%")
  (dolist (spec specs)
    (format t "~&Please mount tape ~A, and put it on line." (car spec))
    (setq files () dirs () host (send (first specs) ':host))
    (dolist (pathname (cdr spec))
      (if (send pathname ':name) ; it's really a filename and not a directory
	  (setq files (nconc files (ncons pathname)))
	(setq dirs (nconc dirs (ncons (send pathname ':directory))))))
    (await-carriage-return)
    (retRIEVe-from-tape host files dirs))
  (format t "~%2Retrieval session complete.~%"))
    
(defun retrieve-things-internal (tape-function things &aux tape specs)
  "THINGS are in reverse order (just in case it makes a difference)
TAPE-FUNCTION returns either a tape or a list, assumed to be a list of specs.
A spec is, of course, a list of (tape . pathnames)."
  (READ-DUMP-RECORDS)
  (dolist (thing things)
    (setq tape (funcall tape-function thing))
    (if tape
	(if (listp tape) (setq specs (merge-specs tape specs))
	  (let ((spec (asS #'string-equal tape specs)))
	    (if (null spec) (push (list* tape thing ()) specs)
	      (nconc spec (ncons thing))))
	  (format t "~&[Warning: no tape found for ~A.]~%" thing))))
  (retrieval-loop specs))

(defun retrieve-files (&aux files)
  (send query-io ':fresh-line)
  (do ((file (ask-pathname-or-nil (make-pathname ':host si:associated-machine)
				  "File to retrieve (or [Return] to go): ")
	     (ASK-PATHNAME-OR-NIL FILE "File to retrieve (or [Return] to go): " file)))
      ((NULL FILE))
    (OR (MEMQ FILE FILES) (PUSH FILE FILES)))
  (RETRIEVE-THINGS-INTERNAL #'FILE-LAST-DUMP FILES)) ; Reverse order

(defun retrieve-directories (&optional (mode ':simple) &aux dirs) mode
  ":SIMPLE mode, the default, means read the directories from the last full dump."
  (send query-io ':fresh-line)
  (do ((dir (ask-directory-or-nil (make-pathname ':host si:associated-machine)
			  "(or [Return] to end)")
	    (ask-directory-or-nil dir "(or [Return] to end)")))
      ((null dir))
    (or (memq dir dirs) (push dir dirs)))
  (retrieve-things-internal #'directory-last-full-dump dirs)) ; reverse order