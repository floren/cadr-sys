;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-

;;; Entry functions which deal with dribble files as well as magtape...
;;; LM-FULL-DUMP -- do a full dump of the file system.
;;; LM-INCREMENTAL-DUMP -- do an incremental dump of the file system.
;;; LM-FULL-DUMP-DIRECTORIES -- do a full dump of specific directories
;;; LM-DUMP -- do a dump of specific files.
;;; DUMP-RESTORE -- look up and restore file(s) which has been DUMPed.
;;; DUMP-LIST-FILES -- given a tape name, list the files on it.
;;; RESTORE-FILES -- given a number of pathnames, restore them selectively
;;; RESTORE -- more general form of RESTORE-FILES
;;;
;;; Entry functions which deal only with magtape
;;; MAGTAPE-LIST-FILES -- List files on a magtape.
;;; RESTORE-MAGTAPE -- Restore whatever's on the mounted magtape.
;;; MT-WRITE-FILES -- Write files to magtape.
;;; MT-WRITE-DIRECTORY -- Write an entire directory to magtape.
;;; MT-WRITE-PARTITION -- Write a partition onto a magtape.
;;;
;;; Other functions:
;;; PRINT-MAGTAPE -- Directly print contents of the magtape (for debugging).
;;; LOAD-PDP10-DUMP-MAGTAPE -- Load a tape made on DM.
;;; MT-WRITE-PDP10-FILES -- Write files to be read at DM.

;;; Info on dumps is kept in a file in the FS: MAGTAPE; directory.
;;; Name is DUMP-<tape name>.TEXT.0
;;; Format is (pardon notation):
;;;  entry ::= <truename><cr><creation-date><cr><author><cr><plist><cr>
;;;  plist ::= <prop><sp><val><cr>...
;;; So the end of an entry is signified by two CRs.
;;; Defined properties are:
;;;  :NSC -- The file has not been source compared.

;;; Modification history
;;;
;;; dumping stuff
;;; started by DLA sometime ago
;;; hacked by ALR summer of 1982? so that dumps would work somewhat reasonably
;;; improved by HGA starting 11/18/82


(DEFVAR DUMP-HOST "LM")
(DEFVAR DUMP-VERSION 1)

;;; In core, the format is:
(DEFSTRUCT (TAPE-FILE :LIST* (:DEFAULT-POINTER TAPE-FILE))
  TAPE-NAME					;A string
  TAPE-FILE-NAME				;A pathname
  TAPE-FILE-PROPERTIES				;File properties of the file
  TAPE-FILE-TAIL-POINTER			;For efficiently adding entries
  TAPE-FILE-ENTRIES)				;A list or NIL.

(DEFSTRUCT (TAPE-FILE-ENTRY :LIST)
  ENTRY-TRUENAME				;A pathname
  ENTRY-CREATION-DATE				;A universal time
  ENTRY-AUTHOR					;A string
  ENTRY-PLIST)					;A list or NIL.

(DEFVAR TAPE-FILE)

(defun setup-tape-file (tape-name
			&OPTIONAL tape-file-name new-p
			&AUX entries properties)
  (or tape-file-name
      (setq tape-file-name (tape-file-name-from-tape-name tape-name)))
  (or new-p
      (multiple-value (entries properties)
	(read-tape-file tape-file-name)))
  (setq tape-file (make-tape-file tape-name tape-name
				  tape-file-name tape-file-name
				  tape-file-properties properties
				  tape-file-entries entries))
  (setf (tape-file-tail-pointer tape-file)
	(cond ((last (tape-file-entries tape-file)))
	      (t (locf (tape-file-entries tape-file))))))

(defun read-tape-file (tape-file)
  (with-open-file (s tape-file '(:READ :NOERROR))
    (cond ((errorp s) NIL)
	  (T (values (LOOP FOR entry = (read-tape-entry s)
			   UNTIL (null entry)
			   COLLECTING entry)
		     NIL;(funcall s ':FILE-PLIST)
		     )))))



(defun read-tape-entry (stream &AUX line eof pathname creation-date author plist prop val tem
			(package si:pkg-user-package) (ibase 10.))
  (prog NIL
	(multiple-value (line eof)
	  (funcall stream ':LINE-IN T))
	(and eof (return nil))
	(setq pathname (merge-pathname-defaults line))
	(setq line (funcall stream ':LINE-IN T))
	(setq creation-date (time:parse-universal-time creation-date 0 NIL NIL))
	(setq author (funcall stream ':LINE-IN T))
     l  (setq line (funcall stream ':LINE-IN T))
	(and (equal line "")
	     (return (make-tape-file-entry entry-truename pathname
					   entry-creation-date creation-date
					   entry-author author
					   entry-plist plist)))
	(setq tem (string-search-char #\SP line))
	(setq prop (intern (substring line 0 tem) si:pkg-user-package))
	(setq val (read-from-string line 'SI:NO-EOF-OPTION tem))
	(setq plist (nconc plist (list* prop val nil)))
	(go l)))



(defun dump-list-files (tape-name &AUX tape-file)
  (setup-tape-file tape-name)
  (format T "~%Tape ~A~:[ not found.~;~%~]" (tape-name) (tape-file-entries))
  (mapc 'LIST-ONE-ENTRY (tape-file-entries))
  T)

(defun tape-name-from-tape-file-name (file-name)
  (substring (funcall file-name ':NAME) 5))

(defun list-one-entry (entry)
  (format T "~% ~A~40T" (entry-truename entry))
  (time:print-universal-time (entry-creation-date entry))
  (format T "~60T~A" (entry-author entry)))

(defun write-tape-file ()
  (with-open-file (s (tape-file-name) '(:WRITE))
    (dolist (e (tape-file-entries))
      (write-tape-file-entry e s))
    ;(tv:doplist ((tape-file-properties) prop ind)
    ;(funcall s ':PROPERTY-LIST prop ind))))
    ))

(defun write-tape-file-entry (entry stream
			      &AUX (base 10.) (*Nopoint T)
			           (package si:pkg-user-package))
  (funcall stream ':LINE-OUT (funcall (entry-truename entry) ':STRING-FOR-PRINTING))
  (time:print-universal-time (entry-creation-date entry) stream)
  (funcall stream ':TYO #\CR)
  (funcall stream ':LINE-OUT (entry-author entry))
  (LOOP FOR p ON (entry-plist entry) BY #'CDDR
	DO (format stream "~A ~S~%" (first p) (second p)))
  (funcall stream ':TYO #\CR))
    
(defun tape-file-name-from-tape-name (tape-name)
  (merge-pathname-defaults
    (make-pathname ':HOST "FS"
		   ':DIRECTORY "Magtape"
		   ':NAME (format NIL "DUMP-~A" tape-name)
		   ':TYPE "TEXT"
		   ':VERSION dump-version)))

(DEFVAR DUMP-FILE-LIST)
(DEFVAR DUMP-TYPE)
(DEFVAR DUMP-GET-MORE-FILES)



(defun file-to-pathname (file)
  (merge-pathname-defaults
    (make-pathname ':Host "fs"
		   ':DIRECTORY (directory-name (file-directory file))
		   ':Name (file-name file)
		   ':TYPE (file-type file)
		   ':VERSION (file-version file))))


(DEFUN LM-FULL-DUMP (&optional (HOST DUMP-HOST))
    (DUMP-1 HOST "Full" (dump-get-all-lm-files)))

(defun lm-incremental-dump (&OPTIONAL (host DUMP-HOST))
    (dump-1 host "Incremental"
	    (dump-get-undumped-lm-files)))

(defun lm-full-dump-directories (dirs &OPTIONAL (host DUMP-HOST))
  (dump-1 host "Random directories"
	  (dump-get-directories dirs host)))

(DEFUN LM-DUMP (&OPTIONAL (host DUMP-HOST))
  (LET ((FILES (DUMP-PROMPT-AND-READ-FILES host)))
    (AND FILES
	 (DUMP-1 (FUNCALL (CAR FILES) ':HOST) "Random files" FILES))))


;;; this doesn't quite work yet for local file systems because
;;; because read-directory-files wants raw directories

(defun dump-get-directories (dirs host &AUX result tem)
  (cond ((equal host DUMP-HOST)
	 (ferror nil
		 "Sorry, you can't dump just a few of the local file system's files yet"))
	(T
	 (dolist (dir dirs)
	   (setq dir (make-pathname ':host host
				    ':device ':wild
				    ':directory dir
				    ':name ':wild
				    ':type ':wild
				    ':version ':wild))
	   (setq tem (cdr (funcall dir ':directory-list '(:fast))))
	   (setq result (nconc result (mapcar 'car tem))))
	 result)))

(defun dump-get-all-lm-files (&OPTIONAL dir continue &AUX dirs result)
  (cond (dir
	 (setq dirs
	       (LOOP FOR d ON (dc-directories)
		     WHEN (equal dir (directory-name (first d)))
		     DO
		     (return (if continue d (ncons (first d))))
		     FINALLY (return NIL))))
	(T (setq dirs (dc-directories))))
  (dolist (dir dirs)
    (setq result (nconc result (copylist* (read-directory-files dir)))))
  result)

(defun mark-all-undumped-files-dumped (&OPTIONAL dir continue)
  (LOOP FOR file IN (dump-get-all-lm-files dir continue)
	UNLESS (file-attribute file ':DUMPED)
	DO
	(format T "~%Marking file ~A dumped." file)
	(change-file-properties (file-truename file) NIL ':NOT-BACKED-UP NIL)))

(defun dump-get-undumped-lm-files (&AUX (result nil))
  (LOOP FOR dir IN (dc-directories)
	DO
	(LOOP FOR file IN (read-directory-files dir)
	      UNLESS (file-attribute file ':DUMPED)
	      DO (push file result)))
  (nreverse result))

;;; this is broken for lists of remote dirs.  individual files work.  - hga

(DEFUN DUMP-PROMPT-AND-READ-FILES (HOST &AUX TEM LINES RESULT)
  (DO () (NIL)
    (FORMAT T "~% FILE= ")
    (SETQ TEM (READLINE))
    (AND (EQUAL TEM "")
	 (RETURN NIL))
    (PUSH TEM LINES))
  (DOLIST (LINE LINES)
    (SETQ LINE (MERGE-PATHNAME-DEFAULTS
		 (PARSE-PATHNAME LINE HOST *DEFAULT-PATHNAME-DEFAULTS*)))
    (SETQ TEM (FUNCALL LINE ':DIRECTORY-LIST '(:FAST)))
    (SETQ RESULT (NCONC RESULT (MAPCAR 'CAR TEM))))
  (SI:ELIMINATE-DUPLICATES RESULT))

;(DEFUN FILE-DUMPED-P (FILE)
;  (FILE-GETPROP FILE ':DUMPED))

(defun dump-1 (host dump-type dump-file-list
	       &AUX tape-file tape-name tape-file-name probe)
  (*catch 'DUMP-1
    (format T "~%Enter Tape Name: ")
    (setq tape-name (readline))
    (setq tape-file-name (tape-file-name-from-tape-name tape-name))
    (setq probe (open tape-file-name '(:PROBE)))
    (setup-tape-file tape-name tape-file-name T))
    (format T "~&~A dump ~:[starting on~;appending to~] tape ~A, "
	    dump-type
	    (tape-file-entries)
	    (tape-name))
    (time:print-current-time)
    (dump-2 host)
    (write-tape-file)
    (format T "~&~A dump completed on tape ~A." dump-type (tape-name)))

(defun dump-2 (host &AUX current-directory this-host
			 current-directory-start
			 (current-tape-start dump-file-list)
			 (current-file-list dump-file-list)
			 #|compare-end|#)
  (SETQ THIS-HOST (EQUAL HOST "LM"))
  (*Catch 'DUMP-2
    (do () (NIL)
      (condition-case ()
	  (do () (NIL)
	    (if (null current-file-list) (*THROW 'DUMP-2 T))
	    (cond ((not (equal current-directory
			   (if this-host
			       (file-directory (first current-file-list))
			       (funcall (first current-file-list) ':directory))))
		   (setq current-directory
			 (if this-host
			     (file-directory (first current-file-list))
			     (funcall (first current-file-list) ':directory)))
		   (setq current-directory-start current-file-list)
		   (format T "~% Dumping ~A" current-directory)))
	    (dump-one-file (pop current-file-list) this-host))
	(end-of-tape nil))
      ;; Here reached EOT.  Space back over files until beginning of directory, and reset.
      (mt-space-rev-to-bof 0 (find-position-in-list (first current-file-list)
						    current-directory-start))
      (mt-write-eof)
      (mt-rewind)
      ;(setq compare-end (dump-compare-files current-tape-start NIL))
	   ;(cond ((neq compare-end current-directory-start)
      ;(ferror NIL "Number of files on tape does not match.  (Bug in DUMP)")))
      ;(mt-rewind)
      (mt-offline)
      (dump-get-new-tape T)
      (setq current-file-list current-directory-start
	    current-tape-start current-directory-start)))
  ;; Here we've reached the end of the dump...
  (mt-write-eof)
  (mt-rewind)
;  (setq compare-end (dump-compare-files current-tape-start NIL))
;  (cond (compare-end
;	 (ferror NIL "All files evidently didn't get dumped. (Bug in DUMP)")))
  (mt-rewind))


;; Cuts a lot of unneccessary crap from COPY-FILE, losing generality.

(defun dump-one-file (file this-host)
  (let* ((truename (if this-host (file-truename file)
				 (funcall file ':truename)))
	 (plist (if this-host (lm-construct-plist file)
			      (cdr (file-properties file))))
	 (author (second (memq ':AUTHOR plist)))
	 (creation-date (second (memq ':CREATION-DATE plist))))
    (copy-file truename "MT:" ':default-byte-size (second (memq ':byte-size plist)))
    (change-file-properties truename NIL ':NOT-BACKED-UP NIL)
    (append-tape-file-entry truename creation-date author
			    '(:NSC T))))  ;; Not Source Compared

(DEFUN REVERSE-PLIST (PLIST &AUX ANS)
  (TV:DOPLIST (PLIST VALUE IND)
    (SETQ ANS (CONS IND (CONS VALUE ANS))))
  ANS)
  

;build list in reverse order to eliminate gross paging every time.

(defun append-tape-file-entry (name creation-date author plist)
	(LET ((FOO (ncons (make-tape-file-entry entry-truename name
					    entry-creation-date creation-date
					    entry-author author
					    entry-plist (copylist plist)))))
	  (RPLACD (TAPE-FILE-TAIL-POINTER) FOO)
	  (SETF (TAPE-FILE-TAIL-POINTER) FOO)))

; save until we're sure the above works
;(defun append-tape-file-entry (name creation-date author plist)
;  (setf (tape-file-entries)
;	(nconc (tape-file-entries)
;	       (ncons (make-tape-file-entry entry-truename name
;					    entry-creation-date creation-date
;					    entry-author author
;       				    entry-plist (copylist plist))))))


;this guy isn't used by anyone

(defun find-file (filename)
  (let* ((pathname (fs:parse-pathname filename))
	 (directory (funcall pathname ':DIRECTORY))
	 (name (funcall pathname ':NAME))
	 (type (funcall pathname ':TYPE))
	 (version (funcall pathname ':VERSION)))
    (LOOP FOR dir IN (dc-directories)
	  AS directory-name = (directory-name dir)
	  AS printed-flag = NIL
	  WHEN (or (not (stringp directory))
		   (string-search directory directory-name))
	  DO
	  (LOOP FOR file IN (read-directory-files dir)
		WHEN (or (not (stringp name))
			 (string-search name (file-name file)))
		WHEN (or (not (stringp type))
			 (string-search type (file-type file)))
		WHEN (or (not (numberp version))
			 (= version (file-version file)))
		DO
		(if (null printed-flag)
		    (progn
		      (setq printed-flag T)
		      (format T "~2%~A;" directory-name)))
		(format T "~%~A.~A#~A"
			(file-name file)(file-type file)(file-version file))))))


(defun find-multiple-files ()
  (LOOP FOR dir IN (dc-directories)
	AS directory-name = (directory-name dir)
	AS printed-flag = NIL
	DO
	(print-directory-multiple-versions dir)))

(defun find-multiple-files-in-dir (directory)
  (if (not (typep directory 'FS:DIRECTORY))
      (setq directory (lookup-directory directory)))
  (LOOP FOR file IN (read-directory-files directory)
	WITH directory-results = NIL
	AS file-entry = (assoc (file-name file) directory-results)
	AS type-entry = (assoc (file-type file) (cdr file-entry))
	DO
	(cond ((and (null file-entry)
		    (null type-entry))
	       (push (list (file-name file)
			   (list (file-type file) file))
		     directory-results))
	      ((null type-entry)
	       (push (list (file-type file) file)
		     (cdr file-entry)))
	      (T (push file (cdr type-entry))))
	FINALLY
	(return directory-results)))

(defvar type-versions
	'(("LISP" . 2)
	  ("TEXT" . 2)
	  ("INIT" . 2)
	  ("RMAIL" . 2)
	  ("QFASL" . 1)
	  ("DRAW" . 2)))

(defun print-directory-multiple-versions (directory)
  (format T "~&~%Directory ~A~2%"
	  (if (typep directory ':STRING) directory
	    (directory-name directory)))
  (LOOP FOR (filename . types) IN (nreverse (find-multiple-files-in-dir directory))
	DO
	(LOOP FOR (type . files) IN types
	      AS versions = (cdr (assoc type type-versions))
	      WHEN (or (and versions
			    (> (length files) versions))
		       (and (null versions)
			    (> (length files) 1)))
	      DO
	      (LOOP FOR f IN files
		    DO (format T "~%~A;~A.~A#~A"
			       (directory-name (file-directory f))
			       (file-name f)
			       (file-type f)
			       (file-version f))))))



(defun find-directory-files (directory-name)
  (let* ((directory-pathname
	   (fs:parse-pathname (format NIL "~A;*.*" directory-name)))
	 (directory-file-list (funcall directory-pathname ':DIRECTORY-LIST '(:SORTED))))
    (LOOP FOR file-entry IN (cdr directory-file-list)
	  WITH result = NIL
	  AS pathname = (first file-entry)
	  AS name = (funcall pathname ':NAME)
	  AS type = (funcall pathname ':TYPE)
	  AS string = (fs:make-pathname ':DIRECTORY directory-name ':NAME name ':TYPE type)
	  AS search = (assoc string result)
	  UNLESS search
	  DO (push (list string file-entry) result)
	  ELSE
	  DO (setf (second search) file-entry)
	  FINALLY (return result))))

(defun find-all-single-files (&OPTIONAL (dirs *))
  (LOOP FOR dir IN dirs
	APPEND (find-directory-files dir)))

(defun make-distribution (&OPTIONAL (host "LM:") (dirs si:*LMI-Standard-Directories*))
  (let ((FS:LM-AUTOMATICALLY-CREATE-DIRECTORIES T))
    (LOOP FOR dir IN dirs
	  AS dir-files = (find-directory-files dir)
	  DO
	  (LOOP FOR (file directory-entry) IN dir-files
		DO (copy-file file host ':DIRECTORY-LIST (cdr directory-entry))))))


;;;??? :PLISTs will disagree some, I fear.
(DEFUN DUMP-COMPARE-FILES (FILE-LIST FILES-NOT-DUMPED
			   &AUX MUST-BE-WRITTEN BS PATH COMPARE-END)
  (*CATCH 'DUMP-COMPARE-FILES
    (DO ((F FILE-LIST (CDR F))
	 (E (TAPE-FILE-ENTRIES) (CDR E)))
	((NULL F))
     (WITH-OPEN-FILE (FILE-STREAM (CAR F)
				  ':CHARACTERS ':DEFAULT
				  ':BYTE-SIZE ':DEFAULT
				  ':PDP10-FORMAT ':DEFAULT)
       (SETQ PATH (FUNCALL (CAR F) ':TRUENAME))
       (WITH-OPEN-FILE (MT-STREAM "MT: *"
				  ':CHARACTERS (FUNCALL FILE-STREAM ':CHARACTERS)
				  ':BYTE-SIZE (FUNCALL FILE-STREAM ':BYTE-SIZE))
	 (IF (ERRORP MT-STREAM)
	     (PROGN (SETQ COMPARE-END F)
		    (*THROW 'DUMP-COMPARE-FILES NIL))
	    (LET ((CD-CORRECT (EQUAL (FUNCALL MT-STREAM ':CREATION-DATE)
				     (FUNCALL FILE-STREAM ':CREATION-DATE)))
		  (AUTHOR-CORRECT (EQUAL (FUNCALL MT-STREAM ':AUTHOR)
					 (FUNCALL FILE-STREAM ':AUTHOR)))
		  (PLIST-CORRECT (EQUAL (FUNCALL MT-STREAM ':PLIST)
					(FUNCALL FILE-STREAM ':PLIST))))
	      (COND ((AND CD-CORRECT AUTHOR-CORRECT PLIST-CORRECT
			  (DUMP-SOURCE-COMPARE-STREAMS MT-STREAM FILE-STREAM))
		     (OR (EQUAL PATH (ENTRY-TRUENAME (CAR E)))
			 (FERROR NIL "Shouldn't happen."))
		     (REMPROP (LOCF (ENTRY-PLIST (CAR E))) ':NOT-SOURCE-COMPARED)
		     (FUNCALL (CAR F) ':PUTPROP ':DUMPED T))
		    (T (FORMAT T "~% Compare error ~A: ~
				    ~:[Creation date ~]~:[Author ~]~:[PLIST~]"
			       PATH CD-CORRECT AUTHOR-CORRECT PLIST-CORRECT)
		       (COND ((NULL PLIST-CORRECT)
			      (FORMAT T "~%  Magtape PLIST ~S, Disk PLIST ~S"
				      (FUNCALL MT-STREAM ':PLIST)
				      (FUNCALL FILE-STREAM ':PLIST))))))))))))
  COMPARE-END)

(DEFUN DUMP-SOURCE-COMPARE-STREAMS (S1 S2 &AUX (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
  (PROG (BUF1 IDX1 CNT1 BUF2 IDX2 CNT2 CC (BUF-START 0))
     L  (MULTIPLE-VALUE (BUF1 IDX1 CNT1)
	  (FUNCALL S1 ':GET-INPUT-BUFFER))
        (MULTIPLE-VALUE (BUF2 IDX2 CNT2)
	  (FUNCALL S2 ':GET-INPUT-BUFFER))
	(COND ((NOT (AND BUF1 BUF2))
	       (IF BUF1 (FORMAT T "~%Stream 1 longer"))
	       (IF BUF2 (FORMAT T "~%Stream 2 longer"))
	       (RETURN (NOT (OR BUF1 BUF2)))))
	(SETQ CC (MIN CNT1 CNT2))
	(COND ((NULL (%STRING-EQUAL BUF1 IDX1 BUF2 IDX2 CC))
	       (FORMAT T "~%Compare fails in buffer starting at ~D" BUF-START)
	       (RETURN NIL)))
	(FUNCALL S1 ':ADVANCE-INPUT-BUFFER (+ IDX1 CC))
	(FUNCALL S2 ':ADVANCE-INPUT-BUFFER (+ IDX2 CC))
	(SETQ BUF-START (+ BUF-START CC))
	(GO L)))

(DEFUNP DUMP-GET-NEW-TAPE (&OPTIONAL OLD-FULL-P &AUX TAPE-NAME TAPE-FILE-NAME PROBE)
  (PUTPROP (LOCF (TAPE-FILE-PROPERTIES)) (NOT OLD-FULL-P) 'TAPE-MAY-APPEND)
  (WRITE-TAPE-FILE)
  (BEEP)
  (FORMAT T "~&>>> Time to mount a new tape <<<")
  FOO
  (FORMAT T "~%Continue ~A dump on tape: " DUMP-TYPE)
  (SETQ TAPE-NAME (READLINE))
  (SETQ TAPE-FILE-NAME (TAPE-FILE-NAME-FROM-TAPE-NAME (FUNCALL (TAPE-FILE-NAME) ':HOST)
						      TAPE-NAME))
  (SETQ PROBE (OPEN TAPE-FILE-NAME '(:PROBE)))
  (COND ((NOT (ERRORP PROBE))
	 (FORMAT T "~&Tape ~A has been used.  Are you *SURE* you want to overwrite it? ")
	 (OR (Y-OR-N-P) (GO FOO))))
  (MT-REWIND)
  (SETUP-TAPE-FILE TAPE-NAME TAPE-FILE-NAME T)
  (FORMAT T "~&Continuing ~A dump on tape ~A ..."
	  DUMP-TYPE TAPE-NAME))


;; RESTORE
(DEFVAR RESTORE-PATHNAMES)
(DEFVAR RESTORE-PATHNAME-TAPENAME-ALIST)
(DEFUN RESTORE-FILES (&REST PATHNAMES)
  (LET ((RESTORE-PATHNAMES (MAPCAR 'MERGE-PATHNAME-DEFAULTS PATHNAMES)))
    (RESTORE-1)))

(DEFUN RESTORE ()
  (LET ((RESTORE-PATHNAMES (LOOP FOR PATH = (FORMAT T "~& FILE= ") (READLINE)
				 WHEN (EQUAL PATH "") RETURN PATHLIST
				 COLLECT (MERGE-PATHNAME-DEFAULTS PATH) INTO PATHLIST)))
    (RESTORE-1)))

(DEFUN RESTORE-1 (&AUX MAGTAPE-DIRECTORY RESTORE-PATHNAME-TAPENAME-ALIST TAPE-NAME TAPE-FILE
		  TAPE-SHOWN TEM HOST)
  (SETQ HOST (FUNCALL (CAR RESTORE-PATHNAMES) ':HOST))
  (FORMAT T "~%(Type Abort to stop searching for files)")
  (SETQ MAGTAPE-DIRECTORY
	(MAPCAR 'CAR
		(SORT (DIRECTORY-LIST (FUNCALL (DEFAULT-PATHNAME NIL HOST)
					       ':NEW-PATHNAME ':DIRECTORY "MAGTAPE"
					       ':NAME ':WILD ':TYPE ':WILD))
		      #'(LAMBDA (X Y)
			  (NOT (< (GET X ':CREATION-DATE) (GET Y ':CREATION-DATE)))))))
  (*CATCH 'SYS:COMMAND-LEVEL
    (DOLIST (TAPE-FILE-NAME MAGTAPE-DIRECTORY)
      (SETQ TAPE-NAME (TAPE-NAME-FROM-TAPE-FILE-NAME TAPE-FILE-NAME))
      (SETUP-TAPE-FILE TAPE-NAME)
      (SETQ TAPE-SHOWN NIL)
      (DOLIST (ENTRY (TAPE-FILE-ENTRIES))
	(COND ((RESTORE-PATHNAMES-MATCH (ENTRY-TRUENAME ENTRY))
	       (COND ((NULL TAPE-SHOWN)
		      (FORMAT T "~%~%Tape ~A:" (TAPE-NAME))
		      (SETQ TAPE-SHOWN (LIST* (TAPE-NAME) TAPE-FILE NIL))))
	       (LIST-ONE-ENTRY ENTRY)
	       (RPLACD (LAST TAPE-SHOWN) (NCONS ENTRY)))))
      (AND TAPE-SHOWN (PUSH TAPE-SHOWN RESTORE-PATHNAME-TAPENAME-ALIST))
      ;; Allow abort to throw now.
      (AND (SETQ TEM (FUNCALL STANDARD-INPUT ':TYI-NO-HANG))
	   (FUNCALL STANDARD-INPUT ':UNTYI TEM))))
  (RESTORE-2))

(DEFUN RESTORE-PATHNAMES-MATCH (PATHNAME)
  (DOLIST (P RESTORE-PATHNAMES)
    (RESTORE-PATHNAME-MATCH PATHNAME P)))

;; P2 shouldn't contain wildcards...
(DEFUN RESTORE-PATHNAME-MATCH (P1 P2 &AUX T1 T2)
  (AND (OR (EQUAL (SETQ T1 (PATHNAME-DIRECTORY P1))
		  (SETQ T2 (PATHNAME-DIRECTORY P2)))
	   (AND (STRINGP T1)
		(STRINGP T2)
		(STRING-SEARCH-CHAR #\* T1)
		(WILD-STRING-MATCH T1 T2)))
       (OR (EQUAL (SETQ T1 (PATHNAME-NAME P1))
		  (SETQ T2 (PATHNAME-NAME P2)))
	   (EQ T1 ':WILD)
	   (AND (STRINGP T1)
		(STRINGP T2)
		(STRING-SEARCH-CHAR #\* T1)
		(WILD-STRING-MATCH T1 T2)))
       (OR (EQUAL (SETQ T1 (PATHNAME-TYPE P1))
		  (SETQ T2 (PATHNAME-TYPE P2)))
	   (EQ T1 ':WILD)
	   (AND (STRINGP T1)
		(STRINGP T2)
		(STRING-SEARCH-CHAR #\* T1)
		(WILD-STRING-MATCH T1 T2)))
       (OR (SYMBOLP (SETQ T1 (PATHNAME-VERSION P1)))
	   (EQUAL T1 (PATHNAME-VERSION P2)))))

(DEFUN RESTORE-2 (&AUX TAPE-NAME TEM)
  (DO () (NIL)
    TOP
    (FORMAT T "~% Enter Tape name to restore or [Return]: ")
    (AND (EQUAL (SETQ TAPE-NAME (READLINE)) "") (RETURN NIL))
    (COND ((SETQ TEM (ASSOC TAPE-NAME RESTORE-PATHNAME-TAPENAME-ALIST))
	   (SETQ TAPE-FILE (SECOND TEM))
	   (FORMAT T "~% Files to be restored on ~A:")
	   (MAPC 'LIST-ONE-ENTRY (CDDR TEM))
	   (SELECTQ (RESTORE-TAPE-OPTION)
	     (ALL (MAPC 'RESTORE-TAPE-ENTRIES (CDDR TEM)))
	     (SELECTIVE (MAPC 'RESTORE-TAPE-ENTRIES
			      (SELECT-FILES-TO-RESTORE (CDDR TEM))))))
	  (T (FUNCALL T "~% No files were found on ~A.")))))

(DEFUN RESTORE-TAPE-OPTION ()
  ;; Returns NIL if user types abort.
  (*CATCH 'SYS:COMMAND-LEVEL
    (FQUERY '(:CHOICES (((ALL "All.") #/A)
			((SELECTIVE "Selective.") #/S)))
	    "Restore all these or be selective? ")))

(DEFUN RESTORE-TAPE-ENTRIES (ENTRIES &AUX FILE-POS SKIP)
  (MT-REWIND)
  (DO ((E ENTRIES (CDR E))
       (POS 0))
      ((NULL E))
    (OR (SETQ FILE-POS (FIND-POSITION-IN-LIST (CAR E) (TAPE-FILE-ENTRIES)))
	(FERROR NIL "Tape entry not found in tape file on second pass."))
    (AND (MINUSP (SETQ SKIP (- POS FILE-POS)))
	 (FERROR NIL "Tape entries out of order on second pass."))
    (MT-SPACE-TO-EOF 0 SKIP)
    (RESTORE-FILE-FROM-TAPE (CAR E))
    (SETQ POS (1+ FILE-POS))))

(DEFUN RESTORE-FILE-FROM-TAPE (ENTRY &AUX PROBE TRUENAME AUTHOR CREATION-DATE
				     BYTE-SIZE CHARACTERS HOST)
  (SETQ PROBE (OPEN "MT: *" '(:PROBE)))
  (SETQ TRUENAME (FUNCALL PROBE ':TRUENAME)
	AUTHOR (FUNCALL PROBE ':AUTHOR)
	CREATION-DATE (FUNCALL PROBE ':CREATION-DATE)
	CHARACTERS (FUNCALL PROBE ':CHARACTERS)
	BYTE-SIZE (FUNCALL PROBE ':BYTE-SIZE)
	FLAVOR (FUNCALL PROBE ':GET ':FLAVOR))
  (OR (AND (EQ TRUENAME (ENTRY-TRUENAME ENTRY))
	   (EQUAL AUTHOR (ENTRY-AUTHOR ENTRY))
	   (EQUAL CREATION-DATE (ENTRY-CREATION-DATE ENTRY)))
      (FERROR NIL "Incorrect file found on tape."))
  (WITH-OPEN-FILE (I "MT: *#0" ':CHARACTERS CHARACTERS
		     ':BYTE-SIZE BYTE-SIZE)
    (WITH-OPEN-FILE (O (FUNCALL TRUENAME ':NEW-PATHNAME ':HOST HOST ':DEVICE "DSK")
		       ':CHARACTERS CHARACTERS ':BYTE-SIZE BYTE-SIZE
		       ':DIRECTION ':OUTPUT ':FLAVOR FLAVOR)
      (STREAM-COPY-UNTIL-EOF I O NIL)
      (FUNCALL O ':SET-AUTHOR AUTHOR)
      (FUNCALL O ':SET-CREATION-DATE CREATION-DATE)
      (LEXPR-FUNCALL O ':CHANGE-PROPERTIES NIL (FUNCALL I ':PLIST))
      (FUNCALL O ':CHANGE-PROPERTIES NIL
	       ':DUMPED (NOT (GET (LOCF (ENTRY-PLIST ENTRY)) ':NOT-SOURCE-COMPARED))))))

(DEFUN SELECT-FILES-TO-RESTORE (ENTRIES)
 (get (locf hdr) 'truename)
  (LOOP FOR ENTRY IN ENTRIES
	DOING (LIST-ONE-ENTRY ENTRY) (FORMAT T " Restore? ")
	WHEN (Y-OR-N-P) COLLECT ENTRY))

(DEFUN RESTORE-MAGTAPE (&OPTIONAL &KEY
			(HOST SI:LOCAL-HOST)
			(QUERY T)
			DIRECTORIES
			TAPE-OPTIONS
			COPY-OPTIONS
			FILES
			&AUX COPY-VALUE file last-time-p TEM)
  (if files
      (setq files
	    (LOOP FOR file IN files
		  AS path = (parse-pathname file)
		  COLLECT (string-append
			    (send path ':directory)
			    ";" (send path ':name)))))
  (LOOP AS stream = (lexpr-funcall 'MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL
				   TAPE-OPTIONS)
	UNTIL (errorp stream) 
	WHEN (send stream ':GET ':PARTITION)	;If this is a partition, allow loading
	DO
	(format T "~%Tape partition ~S, length ~D.  Load? "
		(send stream ':GET ':COMMENT)
		(send stream ':GET ':SIZE))
	(cond ((yes-or-no-p)
	       (let ((disk-device
		       (if (yes-or-no-p
			     "Do you want to load the partition onto the local disk?")
			   0
			 (format T "~&Enter the machine name (i.e. cadr2)")
			 (readline))))
		 (format T "~&Copy into partition: ")
		 (mt-space-rev-to-bof)
		 (si:copy-disk-partition "MT" (send stream ':GET ':NAME)
					 disk-device (readline)))))
	ELSE
	DO (cond (last-time-p (send stream ':CLOSE) (return t))
		 ((and directories
		       (not (member (send stream ':DIRECTORY) directories))))
		 ((and files
		       (cond ((member
				(setq file
				      (string-append
					(send stream ':directory)
					";" (send stream ':name)))
				files)
			      (setq files (delete file files))
			      (if (null files) (setq last-time-p T))
			      NIL)
			     (T T))))
		 ((if query
		      (progn (format T "~%~S;~S ~S #~D"
				     (send stream ':DIRECTORY)
				     (send stream ':NAME)
				     (send stream ':TYPE)
				     (send stream ':VERSION))
			     (not (y-or-n-p "Restore? ")))))
		 (T
  ;until we get structured directories...
     ;		  (COND ((LISTP (SETQ TEM (SEND STREAM ':DIRECTORY)))
     ;			 (SEND STREAM ':PUTPROP  (FN-CONCATENATE TEM) ':DIRECTORY)))
		  ;; should it NOT DO a next file if copy-value is NIL?
		  (setq copy-value
			(lexpr-funcall #'FS-COPY-FILE STREAM HOST
				       copy-options))
		  ;(if (not (eq copy-value T))
		  ;    (send stream ':NEXT-FILE))    ;No good to do :NEXT-FILE because  
		  ))					;the :CLOSE operation does an 
	DO (send stream ':CLOSE)))			;an :ADVANCE-FILE 

(DEFUN FN-CONCATENATE (LIST)
  (PROG (NAME)
	(SETQ NAME (CAR LIST) LIST (CDR LIST))
     L  (COND ((NULL LIST) (RETURN NAME)))
	(SETQ NAME (STRING-APPEND NAME "-" (CAR LIST))
	      LIST (CDR LIST))
	(GO L)))
	
;; Allow for partitions also.
(DEFUN MAGTAPE-LIST-FILES (&OPTIONAL (OUT-STREAM STANDARD-OUTPUT))
  (DO ((STREAM))
      ((ERRORP (SETQ STREAM (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL))))
    (IF (FUNCALL STREAM ':GET ':PARTITION)
	(FORMAT OUT-STREAM "~%Partition ~A: ~S, Size ~D"
		(FUNCALL STREAM ':GET ':NAME)
		(FUNCALL STREAM ':GET ':COMMENT)
		(FUNCALL STREAM ':GET ':SIZE))
	(FORMAT OUT-STREAM "~%~A; ~A ~A #~D~35TByte Size ~2D, Created "
		(FUNCALL STREAM ':DIRECTORY)
		(FUNCALL STREAM ':NAME)
		(FUNCALL STREAM ':TYPE)
		(FUNCALL STREAM ':VERSION)
		(FUNCALL STREAM ':BYTE-SIZE))
	(TIME:PRINT-UNIVERSAL-TIME (FUNCALL STREAM ':CREATION-DATE) OUT-STREAM)
;	(FORMAT T "~%PLIST: ~S" (FUNCALL STREAM ':FILE-PLIST))
	)
    (FUNCALL STREAM ':CLOSE)))

;; Tape creation functions which hack EOT.

(DEFUN MT-EOT-HANDLER (&REST IGNORE)
  (*THROW 'EOT NIL))

(DEFUN MT-WRITE-FILES (&REST FILES &AUX (*MT-EOT-HANDLER* 'MT-EOT-HANDLER))
  (LOOP FOR FILE IN FILES DOING
	(LOOP UNTIL (*CATCH 'EOT (COPY-FILE FILE "MT") T)
	      DO (MT-SPACE-REV-TO-BOF)
		 (MT-WRITE-EOF)
		 (MT-REWIND)
		 (BEEP)
		 (FORMAT ERROR-OUTPUT "~&>>> MagTape reached end of tape <<<")
		 (MT-OFFLINE)
		 (LOOP DOING (FORMAT ERROR-OUTPUT "~%Type [Resume] when new tape is mounted:")
		       UNTIL (FUNCALL STANDARD-INPUT ':CLEAR-INPUT)
			     (EQ (FUNCALL STANDARD-INPUT ':TYI) #\RESUME)
		       DOING (BEEP)))
	FINALLY (MT-WRITE-EOF) (MT-SPACE-REV)))

;; Old name
(DEFF MAGTAPE-WRITE-FILE 'MT-WRITE-FILES)

(DEFUN MT-WRITE-DIRECTORIES (&OPTIONAL (HOST SI:LOCAL-HOST) &REST DIRECTORIES)
  (SETQ DIRECTORIES
	(LOOP FOR D IN (OR DIRECTORIES
			   (LOOP FOR D IN (ALL-DIRECTORIES HOST)
				 COLLECTING (FUNCALL (CAR D) ':DIRECTORY)))
	      WHEN D COLLECT (MAKE-PATHNAME ':HOST HOST ':DIRECTORY D ':NAME NIL)))
  (LOOP FOR DIRECTORY IN DIRECTORIES
	DO (FORMAT T "~%Dumping ~A" DIRECTORY)
	   (MT-WRITE-DIRECTORY DIRECTORY)
	   (FORMAT T "~%~%")))
  
(DEFUN MT-WRITE-DIRECTORY (PATH &AUX (*MT-EOT-HANDLER* 'MT-EOT-HANDLER))
  (SETQ PATH (PARSE-PATHNAME PATH))
  (SETQ PATH (FUNCALL PATH ':NEW-PATHNAME
		      ':NAME (OR (PATHNAME-NAME PATH) ':WILD)
		      ':TYPE (OR (PATHNAME-TYPE PATH) ':WILD)
		      ':VERSION (OR (PATHNAME-VERSION PATH) ':WILD)))
  (LOOP AS COUNT = 0 UNTIL
	(*CATCH 'EOT
	  (LOOP FOR (FILE . DIR-LIST)
		IN (DIRECTORY-LIST PATH)
		WHEN (AND FILE (NOT (GET (LOCF DIR-LIST) ':LINK-TO)))
		DO (COPY-FILE FILE "MT" ':DIRECTORY-LIST DIR-LIST)
		   (INCF COUNT)
		FINALLY (RETURN T)))
	DO (LOOP REPEAT COUNT DO (MT-SPACE-REV 0))
	   (MT-SPACE-REV-TO-BOF)
	   (MT-WRITE-EOF)
	   (MT-REWIND)
	   (BEEP)
	   (FORMAT ERROR-OUTPUT "~&>>> MagTape reached end of tape <<<")
	   (MT-OFFLINE)
	   (LOOP DOING (FORMAT ERROR-OUTPUT "~%Type [Resume] when new tape is mounted:")
		 UNTIL (FUNCALL STANDARD-INPUT ':CLEAR-INPUT)
		       (EQ (FUNCALL STANDARD-INPUT ':TYI) #\RESUME)
		 DOING (BEEP))
	FINALLY (MT-WRITE-EOF) (MT-SPACE-REV)))

(DEFUN MT-WRITE-PARTITION (PARTITION &OPTIONAL (UNIT 0))
  (SI:COPY-DISK-PARTITION UNIT PARTITION "MT" PARTITION)
  (MT-WRITE-EOF)
  (MT-SPACE-REV))


;; MagTape Band transfer handler.
(DECLARE (SPECIAL *BAND-WRITE* *BAND-PLIST* *BAND-STREAM*))

(DEFUN MAKE-BAND-MAGTAPE-HANDLER (*BAND-WRITE*)
  (LET ((*BAND-PLIST* `(:PARTITION T :BYTE-SIZE 20 :AUTHOR ,USER-ID))
	(*BAND-STREAM* NIL))
    (COND ((NULL *BAND-WRITE*)
	   (SETQ *BAND-STREAM* (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':CHARACTERS NIL)
		 *BAND-PLIST* (FUNCALL *BAND-STREAM* ':PLIST))))
    (CLOSURE '(*BAND-WRITE* *BAND-PLIST* *BAND-STREAM*)
	     'BAND-MAGTAPE-HANDLER)))

(DEFSELECT (BAND-MAGTAPE-HANDLER IGNORE)
  (:READ (RQB BLOCK)
    BLOCK
    (FUNCALL *BAND-STREAM* ':STRING-IN "unexpected EOF"
	     (RQB-BUFFER RQB) 0 (* (RQB-NPAGES RQB) 1000)))
  (:WRITE (RQB BLOCK &AUX
	   (N-BLOCKS (RQB-NPAGES RQB))
	   (N-HWDS (* N-BLOCKS 1000))
	   (BUF (RQB-BUFFER RQB)))
    BLOCK
    (OR *BAND-STREAM*
	(SETQ *BAND-STREAM*
	      (MAKE-MT-FILE-STREAM ':DIRECTION ':OUTPUT
				   ':PLIST *BAND-PLIST*
				   ':CHARACTERS NIL)))
    (FUNCALL *BAND-STREAM* ':STRING-OUT BUF 0 N-HWDS))
  (:DISPOSE ()
    (COND (*BAND-STREAM*
	   (FUNCALL *BAND-STREAM* ':CLOSE)
	   (SETQ *BAND-STREAM* NIL))))
  (:HANDLES-LABEL () T)
  (:GET (IND) (GET (LOCF *BAND-PLIST*) IND))
  (:PUT (PROP IND) (PUTPROP (LOCF *BAND-PLIST*) PROP IND))
  (:FIND-DISK-PARTITION (NAME &AUX TEM)
    (IF (SETQ TEM (GET-FROM-ALTERNATING-LIST *BAND-PLIST* ':NAME))
	(IF (NOT (EQUAL NAME TEM))
	    (IF (NULL (Y-OR-N-P (FORMAT NIL "~%Tape partition ~s, OK?" *BAND-PLIST*)))
		(BREAK FOO T)))
	(PUTPROP (LOCF *BAND-PLIST*) NAME ':NAME))
    (VALUES  0
	     (OR (GET (LOCF *BAND-PLIST*) ':SIZE) 3777777)
	     NIL))
  (:PARTITION-COMMENT (IGNORE)
    (GET (LOCF *BAND-PLIST*) ':COMMENT)))


;; All stuff hereafter is untested kruft left over from the days of RG;MT.

;; Mode can be :LIST, :PRINT, or :LOAD
(DEFUN LOAD-PDP10-DUMP-MAGTAPE (&OPTIONAL (MODE ':LOAD) HOST &REST OPTIONS &AUX IS)
  (UNWIND-PROTECT 
    (PROG (PLIST TEM (FIRST-TIME T) ITS-DIR ITS-N1 ITS-N2)
       L  (SETQ IS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':INPUT ':DENSITY 2
				   ':RECORD-SIZE (* 1024. 5) OPTIONS))
	  (COND (FIRST-TIME
		 (STREAM-READ-PDP10-TAPE-HEADER IS)
		 (SETQ FIRST-TIME NIL)))
	  (IF (NULL (MULTIPLE-VALUE (ITS-DIR ITS-N1 ITS-N2)
		      (STREAM-READ-PDP10-DUMP-HEADER IS)))
	      (RETURN T))
	  (SELECTQ MODE
	    (:LIST
	     (FORMAT T "~%~S; ~S ~S" ITS-DIR ITS-N1 ITS-N2)
	     (MT-SPACE-TO-EOF)
	     (GO E0))
	    (:PRINT
	     (FORMAT T "~%~S; ~S ~S" ITS-DIR ITS-N1 ITS-N2)
	     (COND ((EQUAL ITS-N2 "QFASL")
		    (MT-SPACE-TO-EOF)
		    (GO E0))
		   (T (STREAM-COPY-PDP10-ASCII-FILE-FROM-TAPE IS TERMINAL-IO)
		      (GO E0))))
	    (:LOAD
	     (SETQ PLIST (ITS-FILENAMES-TO-LM-PLIST ITS-DIR ITS-N1 ITS-N2))
	     (FORMAT T "~%FILE ~S on tape, type disk file name or CR: " PLIST))
	    (OTHERWISE
	     (FERROR T "~%~S is an unknown mode" MODE)))
	  (IF (> (ARRAY-ACTIVE-LENGTH (SETQ TEM (READLINE))) 0)
	      (SETQ PLIST (PARSE-PATHNAME TEM)))
	  (LET* ((OUTPATH (PATHNAME-FROM-PLIST HOST NIL PLIST)))
	    (WITH-OPEN-FILE (OS OUTPATH ':DIRECTION ':OUTPUT
				':FLAVOR ':PDP10
				':PDP10-FORMAT T
				':CHARACTERS (NOT (EQUAL (FUNCALL OUTPATH ':TYPE) "QFASL")))
	      (COND ((FUNCALL OS ':SEND-IF-HANDLES ':PDP10-FORMAT)
		     (STREAM-COPY-PDP10-FMT-FILE-FROM-TAPE IS OS))
		    ((FUNCALL OS ':CHARACTERS)
		     (STREAM-COPY-PDP10-ASCII-FILE-FROM-TAPE IS OS))
		    (T (STREAM-COPY-PDP10-QFASL-FILE-TO-PDP10-TAPE IS OS)))))
      E0  (CLOSE IS)
	  (GO L))
    (CLOSE IS)))

(DEFUN MT-WRITE-PDP10-FILES (FILES &REST OPTIONS &AUX OS)
  (UNWIND-PROTECT
    (PROG (PLIST (FIRST-TIME T))
	  (SETQ FILES (MAPCAR #'MERGE-PATHNAME-DEFAULTS FILES))
      L	  (COND ((NULL FILES)
		 (MT-WRITE-EOF)
		 (RETURN T)))
	  (SETQ OS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':OUTPUT ':DENSITY 2
				  ':RECORD-SIZE (* 1024. 5) OPTIONS))
	  (COND (FIRST-TIME
		 (STREAM-WRITE-PDP10-TAPE-HEADER OS)
		 (SETQ FIRST-TIME NIL)))
	  (SETQ PLIST (PLIST-VIA-PATHNAME (CAR FILES)))
	  (MULTIPLE-VALUE-BIND (ITS-DIR ITS-N1 ITS-N2)
	      (LM-PLIST-TO-ITS-FILENAMES PLIST)
	    (SETQ ITS-DIR "USERS1")		;Hack for DMS.
	    (STREAM-WRITE-PDP10-DUMP-HEADER OS ITS-DIR ITS-N1 ITS-N2))
	  (WITH-OPEN-FILE (IS (CAR FILES) ':CHARACTERS ':DEFAULT ':PDP10-FORMAT T)
	    (IF (FUNCALL IS ':SEND-IF-HANDLES ':PDP10-FORMAT)
		(STREAM-COPY-PDP10-FMT-FILE-TO-PDP10-TAPE IS OS)
	      (IF (FUNCALL IS ':CHARACTERS)
		  (STREAM-COPY-ASCII-FILE-TO-PDP10-TAPE IS OS)
		(STREAM-COPY-QFASL-FILE-TO-PDP10-TAPE IS OS))))
	  (CLOSE OS)
	  (SETQ FILES (CDR FILES))
	  (GO L))
    (CLOSE OS)))

(DEFUN PLIST-VIA-PATHNAME (FILE &AUX ANS)
  (IF (OR (STRINGP FILE)
	  (TYPEP FILE 'PATHNAME))
      (SETQ FILE (OPEN FILE '(:PROBE))))
  (IF (STRINGP FILE)
      (FERROR NIL FILE)
      (LET ((INPATH (FUNCALL FILE ':TRUENAME))
	    (WO  (FUNCALL FILE ':WHICH-OPERATIONS)))
	(LET ((DIRECTORY (FUNCALL INPATH ':DIRECTORY))
	      (NAME (FUNCALL INPATH ':NAME))
	      (TYPE (FUNCALL INPATH ':TYPE))
	      (VERSION (FUNCALL INPATH ':VERSION)))
	  (SETQ ANS `(:DIRECTORY ,DIRECTORY :NAME ,NAME :TYPE ,TYPE :VERSION ,VERSION))
	  (LET ((BYTE-SIZE (COND ((MEMQ ':BYTE-SIZE WO) (FUNCALL FILE ':BYTE-SIZE))
				 ((EQUAL TYPE "QFASL") 16.)
				 (T 8.)))
		(CREATION-DATE (IF (MEMQ ':CREATION-DATE WO)
				   (FUNCALL FILE ':CREATION-DATE)))
		(AUTHOR  (IF (MEMQ ':AUTHOR WO)
			     (FUNCALL FILE ':AUTHOR))))
	    (SETQ ANS (NCONC ANS `(:BYTE-SIZE ,BYTE-SIZE)))
	    (IF CREATION-DATE (SETQ ANS (NCONC ANS `(:CREATION-DATE ,CREATION-DATE))))
	    (IF AUTHOR (SETQ ANS (NCONC ANS `(:AUTHOR ,AUTHOR))))
	    (IF (MEMQ ':PLIST WO)
		(TV:DOPLIST ((FUNCALL FILE ':PLIST) VAL IND)
		  (IF (GET-FROM-ALTERNATING-LIST ANS IND)
		      NIL
		      (SETQ ANS (NCONC ANS (LIST IND VAL))))))
	    ANS)))))

(DEFUN PLIST-FROM-PATHNAME (PATHNAME)
  `(:DIRECTORY ,(FUNCALL PATHNAME ':DIRECTORY)
    :NAME ,(FUNCALL PATHNAME ':NAME)
    :VERSION ,(FUNCALL PATHNAME ':VERSION)
    :TYPE ,(FUNCALL PATHNAME ':TYPE)))

(DEFUN PATHNAME-FROM-PLIST (HOST DIRECTORY PLIST)
  (IF (TYPEP PLIST 'PATHNAME)
      PLIST
      (MAKE-PATHNAME ':HOST HOST
		     ':DIRECTORY (IF DIRECTORY DIRECTORY
				     (GET-FROM-ALTERNATING-LIST PLIST ':DIRECTORY))
		     ':NAME (GET-FROM-ALTERNATING-LIST PLIST ':NAME)
		     ':TYPE (GET-FROM-ALTERNATING-LIST PLIST ':TYPE)
		     ':VERSION (GET-FROM-ALTERNATING-LIST PLIST ':VERSION))))

(DEFUN STREAM-COPY-PDP10-QFASL-FILE-FROM-TAPE (IS OS)
  (PROG (C1 C2 C3 C4 C5)
   L   (SETQ C1 (FUNCALL IS ':TYI)	;JUST GOBBLE HIGH 32 BITS FROM PDP10 WORD.
	     C2 (FUNCALL IS ':TYI)
	     C3 (FUNCALL IS ':TYI)
	     C4 (FUNCALL IS ':TYI)
	     C5 (FUNCALL IS ':TYI))
       (IF (NULL C5) (RETURN NIL))
       (FUNCALL OS ':TYO (DPB C1 1010 C2))
       (FUNCALL OS ':TYO (DPB C3 1010 C4))
       (GO L)))

(DEFUN STREAM-COPY-QFASL-FILE-TO-PDP10-TAPE (IS OS)
  (PROG (C1 C2)
    L	(SETQ C1 (FUNCALL IS ':TYI)
	      C2 (FUNCALL IS ':TYI))
       (IF (NULL C1) (RETURN NIL))
       (IF (NULL C2) (SETQ C2 0))
       (FUNCALL OS ':TYO (LDB 1010 C1))
       (FUNCALL OS ':TYO (LDB 0010 C1))
       (FUNCALL OS ':TYO (LDB 1010 C2))
       (FUNCALL OS ':TYO (LDB 0010 C2))
       (GO L)))

(DEFUN STREAM-COPY-PDP10-FMT-FILE-FROM-TAPE (IS OS &OPTIONAL PDP10WDS)
  (PROG (C1 C2 C3 C4 C5 CH)
    L  (COND ((AND PDP10WDS (ZEROP (SETQ PDP10WDS (1- PDP10WDS))))
	      (RETURN T)))
       (SETQ C1 (FUNCALL IS ':TYI)
	     C2 (FUNCALL IS ':TYI)
	     C3 (FUNCALL IS ':TYI)
	     C4 (FUNCALL IS ':TYI)
	     C5 (FUNCALL IS ':TYI))
       (IF (NULL C5) (RETURN NIL))
       (FUNCALL OS ':TYO (LSH C1 -1))
       (FUNCALL OS ':TYO (DPB C1 0601 (LSH C2 -2)))
       (FUNCALL OS ':TYO (DPB C2 0502 (LSH C3 -3)))
       (FUNCALL OS ':TYO (DPB C3 0403 (LSH C4 -4)))
       (FUNCALL OS ':TYO (DPB C5 0701 (DPB C4 0304 (LSH (LOGAND 77 C5) -1))))
       (GO L)))

(DEFUN STREAM-COPY-PDP10-FMT-FILE-TO-PDP10-TAPE (IS OS)
  (PROG (CH BP WD)
	(SETQ BP 3507 WD 0)
    L	(SETQ CH (FUNCALL IS ':TYI))
        (COND ((NULL CH)
	       (IF (NOT (ZEROP WD)) (STREAM-WRITE-PDP10-WORD OS WD))
	       (RETURN NIL)))
	(SETQ WD (DPB CH BP WD))
	(IF (= BP 0107)
	    (DPB (LSH CH -7) 0001 WD))
	(SETQ BP (- BP 700))
	(COND ((MINUSP BP)
	       (STREAM-WRITE-PDP10-WORD OS WD)
	       (SETQ BP 3507 WD 0)))
	(GO L)))

(DEFUN STREAM-COPY-PDP10-ASCII-FILE-FROM-TAPE (IS OS &OPTIONAL PDP10WDS)
  (PROG (C1 C2 C3 C4 C5 CH)
    L  (COND ((AND PDP10WDS (ZEROP (SETQ PDP10WDS (1- PDP10WDS))))
	      (RETURN T)))
       (SETQ C1 (FUNCALL IS ':TYI)
	     C2 (FUNCALL IS ':TYI)
	     C3 (FUNCALL IS ':TYI)
	     C4 (FUNCALL IS ':TYI)
	     C5 (FUNCALL IS ':TYI))
       (IF (NULL C5) (RETURN NIL))
       (SETQ CH (ASCII-TO-LISPM (LSH C1 -1)))
       (IF CH (FUNCALL OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C1 0601 (LSH C2 -2))))
       (IF CH (FUNCALL OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C2 0502 (LSH C3 -3))))
       (IF CH (FUNCALL OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C3 0403 (LSH C4 -4))))
       (IF CH (FUNCALL OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C4 0304 (LSH (LOGAND 77 C5) -1))))
       (IF CH (FUNCALL OS ':TYO CH))
       (GO L)))

(DEFUN STREAM-COPY-ASCII-FILE-TO-PDP10-TAPE (IS OS)
  (PROG (CH BP WD)
	(SETQ BP 3507 WD 0)
    L	(SETQ CH (FUNCALL IS ':TYI))
        (COND ((NULL CH)
	       (IF (NOT (ZEROP WD)) (STREAM-WRITE-PDP10-WORD OS WD))
	       (RETURN NIL)))
	(SETQ CH (LISPM-TO-ASCII CH))
	(SETQ WD (DPB CH BP WD)
	      BP (- BP 700))
	(COND ((MINUSP BP)
	       (STREAM-WRITE-PDP10-WORD OS WD)
	       (SETQ BP 3507 WD 0)))
	(COND ((= CH 15)
	       (SETQ WD (DPB 12 BP WD)
		     BP (- BP 700))
	       (COND ((MINUSP BP)
		      (STREAM-WRITE-PDP10-WORD OS WD)
		      (SETQ BP 3507 WD 0)))))
	(GO L)))

(DEFUN ASCII-TO-LISPM (CH)
  (COND ((EQ CH 12) NIL)
	((MEMQ CH '(10 11 14 15)) (+ CH 200))
	(T CH)))

(DEFUN LISPM-TO-ASCII (CH)
  (COND ((>= CH 200)
	 (- CH 200))
	(T CH)))

(DEFUN STREAM-READ-PDP10-TAPE-HEADER (IS)
  (PROG (HEADER COUNT)
	(SETQ HEADER (STREAM-READ-PDP10-WORD IS))
	(SETQ COUNT (MIN 100 (1- (MINUS (LOGIOR -400000 (LDB 2222 HEADER))))))
	(FORMAT T "~%TAPE HEADER: ~D WORDS" COUNT)
	(DOTIMES (C COUNT)
	  (PRINT (STREAM-READ-PDP10-WORD IS)))
   ;TAPE NUMBER,  TAPE CREATION DATE (SIXBIT), TYPE-DUMP
	))

(DEFUN STREAM-WRITE-PDP10-TAPE-HEADER (OS)
  (STREAM-WRITE-PDP10-WORD OS (PDP10-HALF-WORDS -4 0))
  (DOTIMES (C 3) (STREAM-WRITE-PDP10-WORD OS 0)))

(DEFUN PDP10-HALF-WORDS (LH RH)
  (DPB LH 2222 RH))

(DEFUN STREAM-READ-PDP10-DUMP-HEADER (IS)
  (PROG (HEADER COUNT DIR N1 N2)
	(IF (NULL (SETQ HEADER (STREAM-READ-PDP10-WORD IS)))
	    (RETURN NIL))
	(SETQ COUNT (MIN 100 (1- (MINUS (LOGIOR -400000 (LDB 2222 HEADER))))))
	(FORMAT T "~%FILE HEADER: ~D WORDS" COUNT)
	(SETQ DIR (SIXBIT-TO-LISPM (STREAM-READ-PDP10-WORD IS)))
	(SETQ N1 (SIXBIT-TO-LISPM (STREAM-READ-PDP10-WORD IS)))
	(SETQ N2 (SIXBIT-TO-LISPM (STREAM-READ-PDP10-WORD IS)))
	(DOTIMES (C (- COUNT 3))
	  (STREAM-READ-PDP10-WORD IS))
   ;DIRECTORY, FN1, FN2, PACK NUMBER, CREATION-DATE (DISK FORMAT)
   	(RETURN DIR N1 N2)))

(DEFUN STREAM-WRITE-PDP10-DUMP-HEADER (OS DIR N1 N2)
  (STREAM-WRITE-PDP10-WORD OS (PDP10-HALF-WORDS -6 0))
  (STREAM-WRITE-PDP10-WORD OS (LISPM-TO-SIXBIT DIR))
  (STREAM-WRITE-PDP10-WORD OS (LISPM-TO-SIXBIT N1))
  (STREAM-WRITE-PDP10-WORD OS (LISPM-TO-SIXBIT N2))
  (STREAM-WRITE-PDP10-WORD OS 0)
  (STREAM-WRITE-PDP10-WORD OS -1))

(DEFCONST *ITS-NAME2-TYPES* '("QFASL" "DRW" "PRESS" "BIN"))

(DEFUN ITS-FILENAMES-TO-LM-PLIST (DIR N1 N2)
  (LET ((N2-IS-TYPE (MEMBER N2 *ITS-NAME2-TYPES*))
	(N2-IS-VERSION (NUMERIC-P N2)))
  `(:DIRECTORY ,DIR
    :NAME ,N1
    :TYPE ,(IF N2-IS-TYPE N2 ':UNSPECIFIC)
    :VERSION ,(COND (N2-IS-VERSION) (T ':NEWEST)))))

(DEFUN LM-PLIST-TO-ITS-FILENAMES (PLIST)
  (LET ((DIRECTORY (GET-FROM-ALTERNATING-LIST PLIST ':DIRECTORY))
	(N (GET-FROM-ALTERNATING-LIST PLIST ':NAME))
	(TYPE (GET-FROM-ALTERNATING-LIST PLIST ':TYPE))
	(VERSION (GET-FROM-ALTERNATING-LIST PLIST ':VERSION)))
    (VALUES DIRECTORY N (IF (MEMBER TYPE *ITS-NAME2-TYPES*)
			    TYPE
			    (FORMAT NIL "~D" VERSION)))))

(DEFUN SIXBIT-TO-LISPM (SIX)
  (DO ((F 3606 (- F 600))
       (ANS (MAKE-ARRAY 6 ':TYPE 'ART-STRING ':LEADER-LIST '(0)))
       (CH))
      ((MINUSP F) ANS)
    (IF (ZEROP (SETQ CH (LDB F SIX)))
	(RETURN ANS)
	(ARRAY-PUSH ANS (+ 40 CH)))))

(DEFUN LISPM-TO-SIXBIT (STR)
  (DO ((F 3606 (- F 600))
       (I 0 (1+ I))
       (ANS 0))
      ((MINUSP F) ANS)
    (IF (< I (ARRAY-ACTIVE-LENGTH STR))
	(SETQ ANS (DPB (- (CHAR-UPCASE (AR-1 STR I)) 40) F ANS)))))

(DEFUN STREAM-READ-PDP10-WORD (IS)
  (PROG (C1 C2 C3 C4 C5)
       (SETQ C1 (FUNCALL IS ':TYI)
	     C2 (FUNCALL IS ':TYI)
	     C3 (FUNCALL IS ':TYI)
	     C4 (FUNCALL IS ':TYI)
	     C5 (FUNCALL IS ':TYI))
       (OR C1 C2 C3 C4 C5 (RETURN NIL))
       (RETURN (DPB C1 3410 (DPB C2 2410 (DPB C3 1410 (DPB C4 0410 C5)))))))

(DEFUN STREAM-WRITE-PDP10-WORD (OS WD)
  (FUNCALL OS ':TYO (LDB 3410 WD))
  (FUNCALL OS ':TYO (LDB 2410 WD))
  (FUNCALL OS ':TYO (LDB 1410 WD))
  (FUNCALL OS ':TYO (LDB 0410 WD))
  (FUNCALL OS ':TYO (LDB 0010 WD)))

;SPECIAL FUNCTION TO DUMP TAMI'S PICTURE DATA ARRAY IN IBM MODE.
(DEFUN DUMP-2D-BYTE-ARRAY-RAW (ARRAY)
  (LET ((XLIM (ARRAY-DIMENSION-N 1 ARRAY))
	(YLIM (ARRAY-DIMENSION-N 2 ARRAY)))
    (LET ((STREAM (MAKE-MT-STREAM ':DIRECTION ':OUTPUT
				  ':IBM-MODE T ':RECORD-SIZE (+ 100 YLIM))))
      (DOTIMES (Y YLIM)
	(DOTIMES (X XLIM)
	  (FUNCALL STREAM ':TYO (AR-2 ARRAY X Y)))
	(FUNCALL STREAM ':ADVANCE-OUTPUT-BUFFER))
      (FUNCALL STREAM ':CLOSE))))

;; For Debugging.
(DEFUN PRINT-MAGTAPE (&REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (UNWIND-PROTECT
      (STREAM-COPY-UNTIL-EOF IS STANDARD-OUTPUT NIL)
      (FUNCALL IS ':CLOSE ':RAW)))		;Avoid searching for EOF
						;This should not normally be done.
  T)


(DEFUN COPY-MAGTAPE-FILE (FN &REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (WITH-OPEN-FILE (OS FN ':OUT)
      (UNWIND-PROTECT 
	(STREAM-COPY-UNTIL-EOF IS OS NIL)
	(FUNCALL IS ':CLOSE ':RAW))))		;Avoid searching for EOF
						;This should not normally be done.
  T)

(DEFUN COPY-INSERTING-CR-EVERY-N (N FROM TO)
  (WITH-OPEN-FILE (OS TO ':OUT)
    (WITH-OPEN-FILE (IS FROM ':IN)
      (DO ((CH T))
	  ((NULL CH))
	(DOTIMES (C N)
	  (SETQ CH  (FUNCALL IS ':TYI))
	  (IF CH (FUNCALL OS ':TYO CH)))
	(FUNCALL OS ':TYO #\CR)))))

(DEFUN PRINT-ASCII-MAGTAPE (&REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (UNWIND-PROTECT
      (do ((ch (funcall is ':tyi) (funcall is ':tyi)))
	  ((null ch))
	(setq ch (ascii-to-lispm ch))
	(if ch (funcall standard-output ':tyo ch)))
      (FUNCALL IS ':CLOSE ':RAW)))		;Avoid searching for EOF
						;This should not normally be done.
  T)


(DEFUN WRITE-ASCII-MAGTAPE (FN &REST OPTIONS)
  (LET ((IS (OPEN FN))
	(OS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':OUTPUT OPTIONS)))
    (DO ((CH (FUNCALL IS ':TYI) (FUNCALL IS ':TYI)))
	((NULL CH)
	 (FUNCALL OS ':CLOSE)
	 (FUNCALL IS ':CLOSE))
      (SETQ CH (LISPM-TO-ASCII CH))
      (FUNCALL OS ':TYO CH))))

(comment 
(defun tape-xfer nil
  (do ((fn 0 (1+ fn)))
      (())
    (let ((is (make-mt-stream))
	  (os (open (format nil "fs:poggio;magf~D.text" fn) '(:write))))
      (do ((ch (funcall is ':tyi) (funcall is ':tyi)))
	  ((null ch))
	(setq ch (ascii-to-lispm ch))
	(if ch (funcall os ':tyo ch)))
      (funcall is ':close ':raw)
      (funcall os ':close)))) )           


(defun restore-vax-tape (&OPTIONAL directory ufilename &REST options &AUX in-stream filename)
  (or directory
      (setq directory
	    (progn (format T "~%Please type the directory name for these files: ")
		   (readline))))
  (do ()
      ((eq ':EOT (setq filename (or ufilename
				    (read-vax-filename-from-header options)))))
    (setq in-stream (lexpr-funcall #'MAKE-MT-STREAM ':ERROR NIL options))
    (with-open-file (os (make-pathname ':DIRECTORY directory
				       ':NAME filename
				       ':TYPE "TEXT")
			'(:write))
      (format T "~%Restoring vax file ~A into file ~A "
	      filename (funcall (funcall os ':TRUENAME) ':STRING-FOR-PRINTING))
      (do ((ch (funcall in-stream ':tyi) (funcall in-stream ':tyi))
	   (base 10.) (ibase 10.) (line-length 0)
	   (counter 1 (1+ counter)))
	  ((null ch))
	(setq ch (ascii-to-lispm ch))
	(cond (( 0 counter 4)
	       (if ( #/0 ch #/9)
		   (setq line-length (+ (* 10. line-length) (- ch #/0)))
		 (format T "~%Ignoring file due to bad record header")
		 (funcall os ':delete)
		 (return)))
	      ((> counter line-length)
	       (cond (( #/0 ch #/9)
		      (funcall in-stream ':untyi ch)
		      (setq counter 0
			    line-length 0))
		     ((string-equal ch "^"))
		     (T (ferror nil "Bad line header encountered."))))
	      (ch (funcall os ':tyo ch)))
	(cond ((and (= line-length counter)
		    ( counter 4))
	       (funcall os ':tyo #\cr))))
      (funcall in-stream ':NEXT-FILE)
      (funcall in-stream ':CLOSE ':RAW))
    (read-eof-file)))

(defun read-vax-filename-from-header (options &AUX in-stream line start)
  (setq in-stream (lexpr-funcall #'MAKE-MT-STREAM ':ERROR NIL options))
  (setq line (funcall in-stream ':LINE-IN))
  (funcall in-stream ':NEXT-FILE)
  (funcall in-stream ':CLOSE ':RAW)
  (if (equal line "") ':EOT
    (substring line (setq start (+ 4 (string-search "HDR1" line)))
	       (string-search-char #\SPACE line start))))


(defun read-eof-file (&AUX in-stream)
  (setq in-stream (lexpr-funcall #'MAKE-MT-STREAM ':ERROR NIL))
  (funcall in-stream ':NEXT-FILE)
  (funcall in-stream ':CLOSE ':RAW))


  
(defun write-vax-tape (filename &OPTIONAL (record-size 120.) &AUX out-stream)
  (setq out-stream (make-mt-stream ':DIRECTION ':OUTPUT ':RECORD-SIZE record-size))
  (with-open-file (in-stream filename)
    (LOOP WITH buff-length 
	  DO
	  (multiple-value-bind (temp-buffer eof)
	      (funcall in-stream ':LINE-IN)
	    (cond ((< (setq buff-length (string-length temp-buffer))
		      record-size)
		   (setq temp-buffer (string-append temp-buffer
						    (make-array (- record-size buff-length)
								':type 'art-string
								':initial-value #\SPACE)))))
	    (funcall out-stream ':STRING-OUT TEMP-BUFFER)
	    (if eof (loop-finish)))))
  (funcall out-stream ':close)
  (mt-write-eof))