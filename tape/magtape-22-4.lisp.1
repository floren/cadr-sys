;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Written 1/13/84 06:58:34 by LMFile,
;;; Reason: RESTORE-MAGTAPE
;;;  Queries formatted more nicely.
;;;  If the :TRANSFORM function returns (), the file is skipped.
;;;  If :QUERYing, allow P (proceed) as a response.
;;; while running on Lisp Machine Filecomputer from band 4
;;; with System 98.27, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.1, Experimental FILE-Server 8.2, Experimental LFS 3.1, Experimental MagTape 22.3, microcode 306, Xmntl FS.



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
			COLLECT (string-append (send path ':directory)
					       ";" (send path ':name)))))
  (LOOP AS stream = (LEXPR-FUNCALL #'MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL
				   TAPE-OPTIONS)
	UNTIL (errorp stream) 
	WHEN (send stream ':GET ':PARTITION) ; If this is a partition, allow loading
	DO
	(IF (FQUERY FORMAT:YES-OR-NO-P-OPTIONS "Tape partition ``~A'', length ~D.  Load ? "
		    (send stream ':GET ':COMMENT) (send stream ':GET ':SIZE))
	    (let ((disk-device
		    (if (yes-or-no-p "Do you want to load the partition onto the local disk ? ")
			0
		      (format T "~&Enter the machine name (i.e. cadr2): ")
		      (READLINE-TRIM))))
	      (format T "~&Copy into partition: ")
	      (MT-SPACE-REV-TO-BOF)
	      (SI:COPY-DISK-PARTITION "MT" (SEND STREAM ':GET ':NAME)
				      DISK-DEVICE (readline-TRIM))))
	ELSE
	DO (cond (last-time-p (send stream ':CLOSE) (return t))
		 ((and directories (not (member (send stream ':DIRECTORY) directories))))
		 ((and files
		       (cond ((member
				(setq file (string-append (send stream ':directory)
							  ";" (send stream ':name)))
				files)
			      (setq files (delete file files))
			      (if (null files) (setq last-time-p T))
			      NIL)
			     (T T))))
		 (T
		  (SETQ DIRECTORY (SEND STREAM ':DIRECTORY) NAME (SEND STREAM ':NAME)
			TYPE (SEND STREAM ':TYPE) VERSION (SEND STREAM ':VERSION))
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
			(AND (EQ ANS ':PROCEED) (SETQ QUERY ()))
			(AND ANS (LEXPR-FUNCALL #'FS-COPY-FILE STREAM TO COPY-OPTIONS)))))))
		 ;No good to do :NEXT-FILE because the :CLOSE operation does an :ADVANCE-FILE 
	DO (send stream ':CLOSE)))

))
