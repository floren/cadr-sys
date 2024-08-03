;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Readtable:ZL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.

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


;;; If TRANSFORM is supplied, it should be a function of five arguments
;;; (host, directory, name, type, version) that returns a pathname that actually gets the file
;;; or NIL if the file should not be copied, or a symbol with a FS:TAPE-RESTORE-TRANSFORM
;;; property that is such a function.  The host argument is a host object.
;;;
;;; If DIRECTORY-TRANSLATIONS, a list of translations, each of which is a 2-list, (LHS RHS),
;;; the following happens:  If the file's directory list is equal to LHS, it is replaced by RHS.
;;; * may appear as the last element in LHS, which matches any list segment.  RHS is then
;;; appended to the portion of the list matched by *.
;;;
;;; > Note that these two ways of translation do not work together; TRANSFORM takes precedence
;;; if both are given.
(DEFUN RESTORE-MAGTAPE (&OPTIONAL &KEY (HOST SI:LOCAL-HOST) (QUERY T) DIRECTORIES (UNIT 0)
			TAPE-OPTIONS COPY-OPTIONS FILES TRANSFORM DIRECTORY-TRANSLATIONS
			&AUX file last-time-p DIRECTORY NAME TYPE VERSION)
  (SETQ HOST (FS:GET-PATHNAME-HOST HOST))
  (WHEN (AND TRANSFORM (NOT (FUNCTIONP TRANSFORM)))
    (SETQ TRANSFORM (GET TRANSFORM 'TAPE-RESTORE-TRANSFORM))
    (IF (NOT (FUNCTIONP TRANSFORM)) (FERROR () "The transform supplied is bogus.")))
  (WHEN files
    (setq files
	  (LOOP FOR file IN files
		AS path = (parse-pathname file)
		COLLECT (string-append (send path :directory) ";" (send path :name)))))
  (LOOP AS stream = (condition-case ()
			(LEXPR-FUNCALL #'MAKE-MT-FILE-STREAM
				       :DIRECTION :INPUT
				       :UNIT UNIT
				       TAPE-OPTIONS)
		      (tape:logical-end-of-tape))
	UNTIL (null stream)

	WHEN (send stream :GET :PARTITION) ; If this is a partition, allow loading
	DO
	(when (FQUERY FORMAT:YES-OR-NO-P-OPTIONS "Tape partition ``~A'', length ~D.  Load ? "
		      (send stream :GET :COMMENT) (send stream :GET :SIZE))
	  (let ((disk-device
		  (if (yes-or-no-p "Do you want to load the partition onto the local disk ? ")
		      0
		    (format *QUERY-IO* "~&Enter the machine name (i.e. cadr2): ")
		    (READLINE-TRIM *QUERY-IO*))))
	    (format T "~&Copy into partition: ")
	    (MT-SPACE-REV-TO-BOF UNIT)
	    (SI:COPY-DISK-PARTITION "MT" (SEND STREAM :GET :NAME)
				    DISK-DEVICE (readline-TRIM *QUERY-IO*))
	    (setq stream nil)))
	ELSE
	DO (cond (last-time-p (send stream :CLOSE) (return t))
		 ((and directories (not (member (send stream :DIRECTORY) directories))))
		 ((and files
		       (cond ((member
				(setq file (string-append (send stream :directory)
							  ";" (send stream :name)))
				files)
			      (setq files (delete file files))
			      (if (null files) (setq last-time-p T))
			      NIL)
			     (T T))))
		 (T
		  (SETQ DIRECTORY (SEND STREAM :DIRECTORY) NAME (SEND STREAM :NAME)
			TYPE (SEND STREAM :TYPE) VERSION (SEND STREAM :VERSION))
		  (LET ((TRANS (RESTORE-DIRECTORY-TRANSLATE DIRECTORY DIRECTORY-TRANSLATIONS)))
		    (IF TRANS (SETQ DIRECTORY TRANS)))
		  (LET ((TO (IF TRANSFORM (FUNCALL TRANSFORM HOST DIRECTORY NAME TYPE VERSION)
			      HOST)))
		    (WHEN TO
		      (LET ((ANS
			      (OR (NOT QUERY)
				  (FQUERY '(:CHOICES (((T "Yes.") #/y #/t #\SP #\HAND-UP)
						      ((() "No.") #/n #\RUBOUT #\HAND-DOWN)
						      ((:PROCEED "Proceed.") #/p #\RESUME)))
					  "~A : Restore ? "
					  (IF TRANSFORM TO
					    (format () "~&~S;~S ~S #~D "
						    DIRECTORY NAME TYPE VERSION))))))
			(AND (EQ ANS :PROCEED) (SETQ QUERY ()))
			(AND ANS (LEXPR-FUNCALL #'FS-COPY-FILE STREAM TO COPY-OPTIONS)))))))
		 ;No good to do :NEXT-FILE because the :CLOSE operation does an :ADVANCE-FILE 
	DO (when stream (send stream :CLOSE))))


;;; Asssumes standard SYS:FOO; ==> <L.FOO> translations.
(DEFUN (:PROPERTY :STANDARD-SYS TAPE-RESTORE-TRANSFORM) (HOST DIRECTORY NAME TYPE VERSION)
  HOST ; ignored, using standard sys host
  (FS:MAKE-PATHNAME ':HOST (FS:GET-PATHNAME-HOST "SYS") ':DIRECTORY (CDR DIRECTORY)
		    ':NAME NAME ':TYPE TYPE ':VERSION VERSION))

(defvar *ask-per-directory-defaults* ()
  "Alist whose key is a pathname with just host, device, and directory components.
The associated value is another pathname of the same form for translation.")

(defun (:property :ask-and-default tape-restore-transform) (host directory name type version)
  (let* ((dir (fs:make-pathname ':host host ':directory directory))
	 (tdir (cdr (assq dir *ask-per-directory-defaults*))))
    (if tdir
	(fs:merge-pathname-defaults
	  tdir (fs:make-pathname ':name name ':type type ':version version))
      (setq tdir (progn
		   (format t "~&Translation for the directory ~A ? " dir)
		   (send 
		     (fs:parse-pathname (readline-trim) () dir)
		     ':new-pathname ':name () ':type () ':version ())))
      (push (cons dir tdir) *ask-per-directory-defaults*)
      (fs:merge-pathname-defaults
	tdir (fs:make-pathname ':name name ':type type ':version version)))))

(DEFUN RESTORE-DIRECTORY-TRANSLATE (NAME-FROM-TAPE TRANSLATIONS)
  (DOLIST (TRANS TRANSLATIONS)
    (MULTIPLE-VALUE-BIND (MATCH-P *-BINDING)
	(RESTORE-DIRECTORY-MATCH NAME-FROM-TAPE (CAR TRANS))
      (COND (MATCH-P
	     (RETURN (RESTORE-DIRECTORY-SUBSTITUTE NAME-FROM-TAPE
						   (CADR TRANS)
						   *-BINDING)))))))

(DEFUN RESTORE-DIRECTORY-MATCH (NAME-FROM-TAPE LHS)
  (COND ((NOT (LISTP NAME-FROM-TAPE)) NIL)
	((EQUAL NAME-FROM-TAPE LHS) T)
	((AND (EQ (CAR (LAST LHS)) '*)
	      (RESTORE-MATCHOFF (BUTLAST LHS) NAME-FROM-TAPE))
	 (VALUES T (NTHCDR (1- (LENGTH LHS)) NAME-FROM-TAPE)))
	(T NIL)))

(DEFUN RESTORE-MATCHOFF (LHS NAME-FROM-TAPE)
  (COND ((NULL LHS) T)
	((NOT (EQUAL (CAR LHS) (CAR NAME-FROM-TAPE)))
	 NIL)
	(T (RESTORE-MATCHOFF (CDR LHS) (CDR NAME-FROM-TAPE)))))

(DEFUN RESTORE-DIRECTORY-SUBSTITUTE (NAME-FROM-TAPE RHS *-BINDING)
  NAME-FROM-TAPE
  (COND ((NULL *-BINDING) RHS)
	(T (APPEND RHS *-BINDING))))

(DEFUN FN-CONCATENATE (LIST)
  (PROG (NAME)
	(SETQ NAME (CAR LIST) LIST (CDR LIST))
     L  (COND ((NULL LIST) (RETURN NAME)))
	(SETQ NAME (STRING-APPEND NAME "-" (CAR LIST))
	      LIST (CDR LIST))
	(GO L)))

;; Allow for partitions also.
(DEFUN MAGTAPE-LIST-FILES (&OPTIONAL (OUT-STREAM STANDARD-OUTPUT) (UNIT 0))
  (DO ((STREAM))
      ((ERRORP (SETQ STREAM (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL :UNIT UNIT))))
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
(DECLARE (SPECIAL *BAND-WRITE* *BAND-PLIST* *BAND-STREAM* *band-tape-number*))

;(DEFUN MAKE-BAND-MAGTAPE-HANDLER (*BAND-WRITE*)
;  (LET ((*BAND-PLIST* `(:PARTITION T :BYTE-SIZE 20 :AUTHOR ,USER-ID))
;	(*BAND-STREAM* NIL))
;    (COND ((NULL *BAND-WRITE*)
;	   (SETQ *BAND-STREAM* (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':CHARACTERS NIL)
;		 *BAND-PLIST* (FUNCALL *BAND-STREAM* ':PLIST))))
;    (CLOSURE '(*BAND-WRITE* *BAND-PLIST* *BAND-STREAM*)
;	     'BAND-MAGTAPE-HANDLER)))
;
;(DEFSELECT (BAND-MAGTAPE-HANDLER IGNORE)
;  (:READ (RQB BLOCK)
;    BLOCK
;    (FUNCALL *BAND-STREAM* ':STRING-IN "unexpected EOF"
;	     (RQB-BUFFER RQB) 0 (* (RQB-NPAGES RQB) 1000)))
;  (:WRITE (RQB BLOCK &AUX
;	   (N-BLOCKS (RQB-NPAGES RQB))
;	   (N-HWDS (* N-BLOCKS 1000))
;	   (BUF (RQB-BUFFER RQB)))
;    BLOCK
;    (OR *BAND-STREAM*
;	(SETQ *BAND-STREAM*
;	      (MAKE-MT-FILE-STREAM ':DIRECTION ':OUTPUT
;				   ':PLIST *BAND-PLIST*
;				   ':CHARACTERS NIL)))
;    (FUNCALL *BAND-STREAM* ':STRING-OUT BUF 0 N-HWDS))
;  (:DISPOSE ()
;    (COND (*BAND-STREAM*
;	   (FUNCALL *BAND-STREAM* ':CLOSE)
;	   (SETQ *BAND-STREAM* NIL))))
;  (:HANDLES-LABEL () T)
;  (:GET (IND) (GET (LOCF *BAND-PLIST*) IND))
;  (:PUT (PROP IND) (PUTPROP (LOCF *BAND-PLIST*) PROP IND))
;  (:FIND-DISK-PARTITION (NAME &AUX TEM)
;    (IF (SETQ TEM (GET-FROM-ALTERNATING-LIST *BAND-PLIST* ':NAME))
;	(IF (NOT (EQUAL NAME TEM))
;	    (IF (NULL (Y-OR-N-P (FORMAT NIL "~%Tape partition ~s, OK?" *BAND-PLIST*)))
;		(BREAK FOO T)))
;	(PUTPROP (LOCF *BAND-PLIST*) NAME ':NAME))
;    (VALUES  0
;	     (OR (GET (LOCF *BAND-PLIST*) ':SIZE) 3777777)
;	     NIL))
;  (:PARTITION-COMMENT (IGNORE)
;    (GET (LOCF *BAND-PLIST*) ':COMMENT)))

(defun generate-volume-id-string (&optional (string ""))
  "Returns a string to be used to identify several tapes as members od a multi-volume set.
The string contains the site and name of the machine which wrote the tape, the user who
was logged in then, the time that the tape set name was generated and a randomly
generated integer.  The optional string argument is appended."
  (format nil "~a ~a ~a ~a ~o ~a"
	  si:site-name
	  si:local-pretty-host-name
	  user-id
	  (time:print-current-time nil)
	  (random 17777777)
	  string))

(DEFUN MAKE-BAND-MAGTAPE-HANDLER (*BAND-WRITE*)
  (LET* ((*band-tape-number* 1)
	 (*BAND-PLIST* `(:PARTITION T :BYTE-SIZE 20 :tape-number ,*band-tape-number*
				   :AUTHOR ,USER-ID :tape-id ,(generate-volume-id-string)))
	(*BAND-STREAM* NIL))
    (COND ((NULL *BAND-WRITE*)
	   (SETQ *BAND-STREAM* (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':CHARACTERS NIL)
		 *BAND-PLIST* (FUNCALL *BAND-STREAM* ':PLIST))))
    (CLOSURE '(*BAND-WRITE* *BAND-PLIST* *BAND-STREAM* *band-tape-number*)
	     'BAND-MAGTAPE-HANDLER)))

;;; The :prepare-next-volume-for-read and :prepare-next-volume-for-write messages are used
;;; by si:copy-disk-partition when a band doesn't fit on a single tape.
(DEFSELECT (BAND-MAGTAPE-HANDLER IGNORE)
  (:prepare-next-volume-for-read (&aux preveous-volume-id-string)
    (if *band-write*
	(ferror nil
		"A writing BAND-MAGTAPE-HANDLER got a :prepare-next-volume-for-read message."))
    (setq preveous-volume-id-string (get (locf *band-plist*) ':tape-id))
    (send *band-stream* ':close ':raw)
    (mt-rewind)
    (setq *band-stream* (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':CHARACTERS NIL)
	  *band-plist* (send *band-stream* ':plist))
    (if (not (string-equal preveous-volume-id-string
			   (get (locf *band-plist*) ':tape-id)))
	(cerror ':no-action nil nil
		"The tape id of this tape does not compare with that of the preveous tape."))
    (if (not (= (incf *band-tape-number*) (get (locf *band-plist*) ':tape-number)))
	(cerror ':no-action nil nil "Tape number ~d expected.  Tape number ~d provided instead."
		*band-tape-number* (get (locf *band-plist*) ':tape-number)))
    (format t "~&Now reading tape number ~d." (get (locf *band-plist*) ':tape-number))
    (get (locf *band-plist*) ':starting-block-number)	;return the starting block number.
    )
  (:prepare-next-volume-for-write (starting-block-number)
    (if (not *band-write*)
	(ferror nil
		"A reading BAND-MAGTAPE-HANDLER got a :prepare-next-volume-for-write message."))
    (send *band-stream* ':close ':raw)
    (setq *band-stream* nil)
    (putprop (locf *band-plist*) (incf *band-tape-number*) ':tape-number)
    (putprop (locf *band-plist*) starting-block-number ':starting-block-number)
    ;;; tape stream gets opened on next write.
    )
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
		(BREAK "FOO")))
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
  (PROG (C1 C2 C3 C4 C5)
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
   	(RETURN (values DIR N1 N2))))

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
  (LET ((XLIM (ARRAY-DIMENSION ARRAY 2))
	(YLIM (ARRAY-DIMENSION ARRAY 1)))
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


(defun copy-ascii-magtape-to-file (file)
  (with-open-file (standard-output file ':out)
    (print-ascii-magtape)))

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



(DEFUN PRINT-ASCII-MAGTAPE-record-headers (&REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (UNWIND-PROTECT
      (do ((ch (funcall is ':tyi) (funcall is ':tyi))
	   (c 0 (1+ c))
	   (record-count 0))
	  ((null ch))
	(setq ch (ascii-to-lispm ch))
	(if ch (funcall standard-output ':tyo ch))
	(when (> c 50.)
	  (funcall is ':next-input-buffer)
	  (setq c 0
		record-count (1+ record-count))
	  (format t "~%~%record ~s ~%~%" record-count)))
      (FUNCALL IS ':CLOSE ':RAW)))		;Avoid searching for EOF
						;This should not normally be done.
  T)

(DEFUN PRINT-PDP10-BINARY-MAGTAPE (&REST OPTIONS &AUX IS)
  (UNWIND-PROTECT 
    (PROG ()
         (SETQ IS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':INPUT :characters nil
				   ':RECORD-SIZE (* 1024. 5 5) OPTIONS))
      L   (let* ((idx (funcall is :stream-input-index))
		 (lim (if idx (funcall is :stream-input-limit)))
		 (wd (stream-read-pdp10-word is)))
	    (format t "~%~d of ~d:~s		" idx lim wd)
	    (print-pdp10-ascii-word wd standard-output))
	  (go l)
      E0  (CLOSE IS)
	  (GO L))
    (CLOSE IS)))

;This restores a format observed to have been written by TOPS-20.  Is it "interchange format"?
;  records are
;   32. word header.
;        header word 0 ->  2 tape header, 4 data block
;        header word 1 ->  ascending block count, increments starting with 1.
;        header word 2 ->  always 1.
;
;  512. word data.  = 2720. characters total.

(defun print-pdp10-??-magtape (&rest options &aux is)
  (prog (header-ary data-ary record-count data-index)
	(setq header-ary (make-array 32.)
	      data-ary (make-array 512.))
        (setq is (lexpr-funcall #'make-mt-stream :direction :input
				:record-size (* (+ 512. 32.) 5) :characters nil options))
	(setq record-count 1)
    L   (dotimes (c 32.)
	  (aset (stream-read-pdp10-word is) header-ary c))
	(dotimes (c 512.)
	  (aset (stream-read-pdp10-word is) data-ary c))
	(cond ((not (= (aref header-ary 1) record-count))
	       (format t "~%Record count differs, is ~d should be ~d" (aref header-ary 1) record-count)))
	(cond ((not (= (aref header-ary 2) 1))
	       (format t "~%Header word 2 is ~d, not 1" (aref header-ary 2))))
	(format t "~%header word 0 = ~o, Header word 3 = ~o" (aref header-ary 0) (aref header-ary 3))
	(dotimes (c 4.)
	  (format t "~%	~o ~o ~o ~o ~o ~o ~o"
		  (aref header-ary (+ (* c 7) 0 4))
		  (aref header-ary (+ (* c 7) 1 4))
		  (aref header-ary (+ (* c 7) 2 4))
		  (aref header-ary (+ (* c 7) 3 4))
		  (aref header-ary (+ (* c 7) 4 4))
		  (aref header-ary (+ (* c 7) 5 4))
		  (aref header-ary (+ (* c 7) 6 4))))
	(cond ((= (aref header-ary 6) 400)
	       (format t "~%**file header block: ~s"
		       (pdp10-??-file-header data-ary 1))  ;wd0 seems to be 1,,200 always.
	       (setq data-index 0))
	      (t
	       (cond ((and data-index
			   (not (= data-index (aref header-ary 13.))))
		      (format t "~%Data index wrong, is ~d should be ~d" data-index (aref header-ary 13.))))
	       (let ((active-wds (aref header-ary 5)))
		 (format t "~%***** ~D active wds ~%" active-wds)
		 (dotimes (c active-wds)
		   (print-pdp10-ascii-word (aref data-ary c) standard-output))
		 (if data-index (setq data-index (+ data-index active-wds))))))
	(setq record-count (1+ record-count))
	(go l)))


(defun restore-pdp10-??-magtape (&rest options &aux is)
  (prog (header-ary data-ary record-count data-index file-stream)
	(setq header-ary (make-array 32.)
	      data-ary (make-array 512.))
        (setq is (lexpr-funcall #'make-mt-stream :direction :input
				:record-size (* (+ 512. 32.) 5) :characters nil options))
	(setq record-count 1)
    L   (dotimes (c 32.)
	  (aset (stream-read-pdp10-word is) header-ary c))
	(dotimes (c 512.)
	  (aset (stream-read-pdp10-word is) data-ary c))
	(cond ((not (= (aref header-ary 1) record-count))
	       (format t "~%Record count differs, is ~d should be ~d" (aref header-ary 1) record-count)))
	(cond ((not (= (aref header-ary 2) 1))
	       (format t "~%Header word 2 is ~d, not 1" (aref header-ary 2))))
	(cond ((= (aref header-ary 6) 400)
	       (if file-stream (funcall file-stream :close))
	       (let ((file-namelist
		       (pdp10-??-file-header data-ary 1)))  ;wd0 seems to be 1,,200 always.
		 (setq file-stream
		       (open-file-from-tops20-dumper-spec (string-append "<" (car file-namelist) ">"
								   (cadr file-namelist) "."
								   (caddr file-namelist) "."
								   (cadddr file-namelist)))))
	       (setq data-index 0))
	      (t
	       (cond ((and data-index
			   (not (= data-index (aref header-ary 13.))))
		      (format t "~%Data index wrong, is ~d should be ~d" data-index (aref header-ary 13.))))
	       (let ((active-wds (aref header-ary 5)))
		 (if file-stream
		     (dotimes (c active-wds)
		       (print-pdp10-ascii-word (aref data-ary c) file-stream)))
		 (if data-index (setq data-index (+ data-index active-wds))))))
	(setq record-count (1+ record-count))
	(go l)))

(defun pdp10-??-file-header (data-ary base-wd)
  (let ((ans nil)
	(pntr base-wd))
    (dotimes (c 4)
      (let* ((c-base-wd (aref data-ary pntr))
	     (wds-to-next-base (logand 777777 c-base-wd)))
	(setq ans (nconc ans (list (pdp10-get-ascii-from-array data-ary (1+ pntr) (1- wds-to-next-base))))
	      pntr (+ pntr wds-to-next-base))))
    ans))
  
(defun pdp10-get-ascii-from-array (ary offset n-words)
  (let ((ans (make-array (* n-words 5) :type :art-string :fill-pointer 0))
	(ch))
    (dotimes (c n-words)
      (let ((wd (aref ary (+ offset c))))
	(setq ch (ascii-to-lispm (ldb 3507 wd)))
	(if (and ch (not (zerop ch))) (array-push ans ch))
	(setq ch (ascii-to-lispm (ldb 2607 wd)))
	(if (and ch (not (zerop ch))) (array-push ans ch))
	(setq ch (ascii-to-lispm (ldb 1707 wd)))
	(if (and ch (not (zerop ch))) (array-push ans ch))
	(setq ch (ascii-to-lispm (ldb 1007 wd)))
	(if (and ch (not (zerop ch))) (array-push ans ch))
	(setq ch (ascii-to-lispm (ldb 0107 wd)))
	(if (and ch (not (zerop ch))) (array-push ans ch))))
    ans))

(DEFUN PRINT-PDP10-DUMPER-MAGTAPE (&REST OPTIONS &AUX IS)
  (UNWIND-PROTECT 
    (PROG (ary tem)
         (SETQ IS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':INPUT
				   ':RECORD-SIZE (* 1024. 5 5) ':CHARACTERS NIL OPTIONS))
	 (setq ary (make-array 518.))
    L	 (setq tem (funcall is ':stream-input-index))
    	 (if (null tem) (setq tem 0))
	 (cond ((not (zerop (\ tem (* 5 518.))))
		(format t "~%Stream input index out of phase..")))
	 (if (> tem 0)
	     (let ((lim (send is :stream-input-limit)))
	       (cond ((< (- lim tem)
			 (* 5 518.))
		      (format t "~%record not big enuf for another block, flushing partial block")
		      (funcall is :setup-next-input-buffer)))))
	 (read-dumper-logical-record is ary)
	 (setq tem (funcall is ':stream-input-index))
    	 (if (null tem) (setq tem 0))
	 (cond ((not (zerop (\ tem (* 5 518.))))
		(format t "~%Stream input index out of phase after reading logical record..")))
    	 (print-dumper-logical-record ary standard-output)
	 (go l))
    (close is)))

(defconst *directories-we-want* '(
	;("RG" "CHESS")
	;("L" "PATCH")
	;("L" "UCADR")
	;("LM-PROLOG" "KERNEL")
	;("LM-PROLOG" "LISP-LIBRARY")
        ;("LUCIA" "3D-MODEL")
	)
)

(defconst *dummy-tops20-host* (make-instance 'tops20-host :alist-elem (si:make-host-alist-elem :name "DUMMY")))

(defun open-file-from-tops20-dumper-spec (x)
 ;  (format t "~%Tape pathname= ~s" x)
  (let ((p (parse-pathname x *dummy-tops20-host*)))
    (format t "~%tape file = ~s, directory ~s" p (send p :directory))
    (and (or (null *directories-we-want*)
	     (member (send p ':directory) *directories-we-want*))
	 (print (open (send p ':new-pathname ':host "LM" ':device nil)
		      'out)))))

(defun restore-pdp10-dumper-magtape (&optional &key (OPENF 'open-file-from-tops20-dumper-spec))
  (let (ary is file-stream file-name last-record-type record-type tem)
    (UNWIND-PROTECT 
	(PROGN (SETQ IS (MAKE-MT-STREAM ':DIRECTION ':INPUT
					':RECORD-SIZE (* 1024. 5 5) ':CHARACTERS NIL))
		(setq ary (make-array 518.))
		(setq file-stream "not reading a file")
		(do ()(nil)
		  (setq tem (funcall is ':stream-input-index))
		  (if (null tem) (setq tem 0))
		  (cond ((not (zerop (\ tem (* 5 518.))))
			 (format t "~%Stream input index out of phase..")))
		  (if (> tem 0)
		      (let ((lim (send is :stream-input-limit)))
			(cond ((< (- lim tem)
				  (* 5 518.))
			       (format t "~%record not big enuf for another block, flushing partial block")
			       (funcall is :setup-next-input-buffer)))))
		  (read-dumper-logical-record-header is ary)
		  (selectq (setq record-type (pdp10-dumper-logical-record-type ary))
		    (data
		     (cond ((and file-stream (not (stringp file-stream)))
			    (read-dumper-logical-record-rest is ary)
			    (dotimes (c 512.)
			      (let ((n (aref ary (+ c 6))))
				;; possibly bogus, but no ascii source files will have
			      ;; words of nulls in them anyway.
			      (or (zerop n) (print-pdp10-ascii-word n file-stream)))))
			   ('else
			    (throw-away-dumper-logical-record-rest is))))
		    (tphd
		     (print "save-set-header")
		     (throw-away-dumper-logical-record-rest is))
		    (flhd
		     (print "file header")
		     (read-dumper-logical-record-rest is ary)
		     (setq file-name
			   (with-output-to-string (s)
			     (dotimes (c 30.)	; no filenames longer than 150 chars i bet.
			       (print-pdp10-ascii-word (aref ary (+ c 6)) s))))
		     (setq file-name (substring file-name
						0
						(string-search #/; file-name)))
		     (setq file-stream (funcall openf file-name)))
		    (fltr
		     (print "file trailer")
		     (throw-away-dumper-logical-record-rest is)
		     (and file-stream (close file-stream))
		     (setq file-stream "not reading a file"))
		    (tptr
		     (return "done"))
		    (usr
		     (print "user directory information")
		     (throw-away-dumper-logical-record-rest is))
		    (ctph
		     (error "foo on continued saveset headers"))
		    (fill
		     (print "fill record")
		     (throw-away-dumper-logical-record-rest is))
		    (otherwise
       ;	     (ferror nil "unknown logical record type ~s" record-type)
		     (throw-away-dumper-logical-record-rest is)))
		  (setq last-record-type record-type)))
      (and is (close is))
      (and file-stream (not (stringp file-stream)) (close file-stream)))))
  
(defun read-dumper-logical-record (is ary)
  (dotimes (c 518.)
    (aset (stream-read-pdp10-word is) ary c)
    ;(format t "~%count ~D, index ~d" c (funcall is ':stream-input-index))
    ))

(defun read-dumper-logical-record-header (is ary)
  (dotimes (c 6.)
    (aset (stream-read-pdp10-word is) ary c)))

(defun read-dumper-logical-record-rest (is ary)
  (dotimes (c 512.)
    (aset (stream-read-pdp10-word is) ary (+ c 6.))))

(defun throw-away-dumper-logical-record-rest (is)
  (dotimes (c 512.)
    (stream-throw-away-pdp10-word is)))

(DEFUN STREAM-throw-away-PDP10-WORD (IS)
  (FUNCALL IS ':TYI)
  (FUNCALL IS ':TYI)
  (FUNCALL IS ':TYI)
  (FUNCALL IS ':TYI)
  (FUNCALL IS ':TYI))
    
(defun pdp10-dumper-logical-record-type (ary)
  (let* ((code (if (zerop (aref ary 4))
		  0
		(minus (logior -1000 (aref ary 4)))))
	 (ans (nth code '(DATA TPHD FLHD FLTR TPTR USR CTPH FILL))))
    (if (null ans)
	(format t "~%unknown error type ~s" code))
    ans))
	
(defun print-dumper-logical-record (ary stream)
  (let ((code (if (zerop (aref ary 4))
		  0
		(minus (logior -1000 (aref ary 4))))))
    (format stream "~%TYPE ~S (~S)"
	    (nth code '(DATA TPHD FLHD FLTR TPTR USR CTPH FILL))
	    code)
    (format stream "~%SEQ ~D" (aref ary 5))
    (selectq code
      (0 (dotimes (c 512.)
	   (print-pdp10-ascii-word (aref ary (+ c 6)) stream)))
      ((1 2 3 4 5 6)
       (dotimes (c 512.)
	 (let ((wd (aref ary (+ c 6))))
	   (cond ((not (zerop wd))
		  (format t "~%~S:	~s	"
			  (+ c 6)
			  wd)
		  (print-pdp10-ascii-word wd stream)))))))))

(defun print-pdp10-ascii-word (wd stream &aux ch)
  (setq ch (ascii-to-lispm (ldb 3507 wd)))
  (if ch (funcall stream ':tyo ch))
  (setq ch (ascii-to-lispm (ldb 2607 wd)))
  (if ch (funcall stream ':tyo ch))
  (setq ch (ascii-to-lispm (ldb 1707 wd)))
  (if ch (funcall stream ':tyo ch))
  (setq ch (ascii-to-lispm (ldb 1007 wd)))
  (if ch (funcall stream ':tyo ch))
  (setq ch (ascii-to-lispm (ldb 0107 wd)))
  (if ch (funcall stream ':tyo ch)))

;;; if record-size = 0., then ignores record-size.  Otherwise outputs a cr after
;;; each record.  The &REST args are for the magtape stream...

(DEFUN PRINT-ASCII-MAGTAPE-TO-FILE (filename &OPTIONAL (record-size 80.) &REST OPTIONS)
  (with-open-file (output-stream (fs:merge-pathname-defaults filename) ':direction ':output)
		   (LET ((input-stream (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
		     (UNWIND-PROTECT
			 (do ((character (funcall input-stream ':tyi)
					 (funcall input-stream ':tyi))
			      (limit (1- record-size))
			      (index 0 (if (>= index limit) 0. (1+ index))))
			     ((null character))
			   (setq character (ascii-to-lispm character))
			   (if character (funcall output-stream ':tyo character))
			   (if (not (equal record-size 0.))
			       (if (= index limit)
				   (funcall output-stream ':tyo #\cr))))
		       (FUNCALL input-stream ':CLOSE ':RAW)))) ;;Avoid searching for EOF
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