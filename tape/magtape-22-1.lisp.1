;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Written 1/07/84 16:38:06 by LMFile,
;;; Reason: Make sure :BYTE-SIZE and :DIRECTORY are correct.
;;; :TRANSFORM argument for FS:RESTORE-MAGTAPE.
;;; while running on Lisp Machine Filecomputer from band 4
;;; with System 98.26, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.1, Experimental FILE-Server 8.2, Experimental LFS 3.1, Experimental MagTape 22.0, microcode 306, Xmntl FS.



; From file MTSTR.LISP PS:<L.TAPE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: TAPE; MTSTR  "

(DEFMETHOD (MT-FILE-MIXIN :DIRECTORY) ()
  (OR (GET (LOCF SI:PROPERTY-LIST) ':DIRECTORY) (GET (LOCF SI:PROPERTY-LIST) 'DIRECTORY)))

))

; From file MTSTR.LISP PS:<L.TAPE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: TAPE; MTSTR  "

(DEFUN MAKE-MT-FILE-STREAM (&OPTIONAL
			     &REST OPTIONS
			     &KEY
			    (DIRECTION ':INPUT)
			    (CHARACTERS ':DEFAULT)
			    (ERROR T)
			    (BYTE-SIZE ':DEFAULT)
			    (UNIT 0)
			    (RECORD-SIZE *DEFAULT-RECORD-SIZE*)
			    (DENSITY 0)
			    (IBM-MODE NIL)
			    (PLIST NIL)
			    (HEADER-STYLE ':MIT)	;Default for write
			    &ALLOW-OTHER-KEYS
			    &AUX STREAM TEM)
  (COND ((EQ DIRECTION ':INPUT)
	 (MULTIPLE-VALUE (PLIST HEADER-STYLE)
	   (READ-MAGTAPE-HEADER
	     (SETQ STREAM (LEXPR-FUNCALL 'MAKE-MT-STREAM
					 ':CHARACTERS T
					 ':BYTE-SIZE 8
					 ':UNIT UNIT
					 ':DENSITY DENSITY
					 OPTIONS))))
	 (FUNCALL STREAM ':CLOSE ':RAW)
	 (IF (NULL PLIST)
	     (MT-OPEN-ERROR (MAKE-CONDITION 'END-OF-TAPE "End of tape on unit ~D." UNIT)
			    ERROR)
	     (AND (EQ CHARACTERS ':DEFAULT)
		  (IF (SETQ TEM (GETL (LOCF PLIST) '(:CHARACTERS)))
		      (SETQ CHARACTERS (CADR TEM))
		      ;; Kludge for old format tapes.
		      (SETQ CHARACTERS (= (OR (GET (LOCF PLIST) ':BYTE-SIZE)
					      (GET (LOCF PLIST) 'BYTE-SIZE)) 8))))
	     (AND (EQ BYTE-SIZE ':DEFAULT)
		  (SETQ BYTE-SIZE (OR (GET (LOCF PLIST) ':BYTE-SIZE)
				      (GET (LOCF PLIST) 'BYTE-SIZE))))
	     (AND (NULL BYTE-SIZE)
		  (SETQ BYTE-SIZE (IF CHARACTERS 8. 16.)))
	     (MAKE-INSTANCE (IF CHARACTERS
				'MT-FILE-CHARACTER-INPUT-STREAM
				'MT-FILE-INPUT-STREAM)
			    ':BYTE-SIZE BYTE-SIZE
			    ':UNIT UNIT
			    ':RECORD-SIZE RECORD-SIZE
			    ':DENSITY DENSITY
			    ':PROPERTY-LIST PLIST
			    ':HEADER-FORMAT HEADER-STYLE
			    ':IBM-MODE IBM-MODE)))
	((EQ DIRECTION ':OUTPUT)
	 (AND (EQ BYTE-SIZE ':DEFAULT)
	      (SETQ BYTE-SIZE (GET (LOCF PLIST) ':BYTE-SIZE)))
	 (AND (NULL BYTE-SIZE)
	      (SETQ BYTE-SIZE (IF CHARACTERS 8. 16.)))
	 (WRITE-MAGTAPE-HEADER
	   (SETQ STREAM (LEXPR-FUNCALL 'MAKE-MT-STREAM
				       ':DIRECTION ':OUTPUT
				       ':CHARACTERS T
				       ':BYTE-SIZE 8
				       ':UNIT UNIT
				       ':DENSITY DENSITY
				       OPTIONS))
	   PLIST
	   HEADER-STYLE)
	 (FUNCALL STREAM ':CLOSE ':RAW)
	 (MAKE-INSTANCE (IF CHARACTERS
			    'MT-FILE-CHARACTER-OUTPUT-STREAM
			    'MT-FILE-OUTPUT-STREAM)
			':BYTE-SIZE BYTE-SIZE
			':UNIT UNIT
			':RECORD-SIZE RECORD-SIZE
			':DENSITY DENSITY
			':PROPERTY-LIST PLIST
			':HEADER-FORMAT HEADER-STYLE
			':IBM-MODE IBM-MODE))
	(T (FERROR NIL "Probe opens not allowed on magtape."))))

))

; From file MTAUX.LISP PS:<L.TAPE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: TAPE; MTAUX  "

(DEFUN RESTORE-MAGTAPE (&OPTIONAL &KEY (HOST SI:LOCAL-HOST) (QUERY T) DIRECTORIES
			TAPE-OPTIONS COPY-OPTIONS FILES TRANSFORM
			&AUX file last-time-p DIRECTORY NAME TYPE VERSION)
  (SETQ HOST (FS:GET-PATHNAME-HOST HOST))
  (WHEN (AND TRANSFORM (NOT (FUNCTIONP TRANSFORM)))
    (SETQ TRANSFORM (GET TRANSFORM 'TAPE-RESTORE-TRANSFORM))
    (IF (NOT (FUNCTIONP TRANSFORM))
	(FERROR () "The transform supplied is bogus.")))
  (if files (setq files
		  (LOOP FOR file IN files
			AS path = (parse-pathname file)
			COLLECT (string-append
				  (send path ':directory)
				  ";" (send path ':name)))))
  (LOOP AS stream = (LEXPR-FUNCALL #'MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL
				   TAPE-OPTIONS)
	UNTIL (errorp stream) 
	WHEN (send stream ':GET ':PARTITION) ; If this is a partition, allow loading
	DO
	(format T "~%Tape partition ~S, length ~D.  Load ? "
		(send stream ':GET ':COMMENT) (send stream ':GET ':SIZE))
	(cond ((yes-or-no-p)
	       (let ((disk-device
		       (if (yes-or-no-p
			     "Do you want to load the partition onto the local disk ? ")
			   0
			 (format T "~&Enter the machine name (i.e. cadr2): ")
			 (READLINE-TRIM))))
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
		 ((if (PROGN
			(SETQ DIRECTORY (SEND STREAM :DIRECTORY) NAME (SEND STREAM :NAME)
			      TYPE (SEND STREAM :TYPE) VERSION (SEND STREAM :VERSION))
			QUERY)
		      (PROGN
			(format T "~%~S;~S ~S #~D " DIRECTORY NAME TYPE VERSION)
			(not (y-or-n-p "Restore ? ")))))
		 (T
		  (LEXPR-FUNCALL #'FS-COPY-FILE STREAM
				 (IF TRANSFORM (FUNCALL TRANSFORM
							HOST DIRECTORY NAME TYPE VERSION)
				   HOST)
				 copy-options)))
		 ;No good to do :NEXT-FILE because the :CLOSE operation does an :ADVANCE-FILE 
	DO (send stream ':CLOSE)))

))
