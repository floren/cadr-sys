;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for MagTape version 14.4
;;; Reason: Writing files to MagTape now writes filenames correctly.
;;; Fixed bugs in RESTORE-MAGTAPE.
;;; Written 5/18/83 19:36:19 by HGA,
;;; while running on Lisp Machine One from band 1
;;; with System 93.30, ZMail 49.10, Experimental Local-File 43.4, Experimental MagTape 14.1,
;;; Experimental FILE-Server 6.2, Experimental LMI 3.5, Experimental NewDraw 20.5,
;;; microcode 226, CHIPS loaded.



; From file MTAUX.LISP#> FILE; SRC:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTAUX LISP 66"

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
		  (COND ((LISTP (SETQ TEM (SEND STREAM ':DIRECTORY)))
			 (SEND STREAM ':PUTPROP  (FN-CONCATENATE TEM) ':DIRECTORY)))
		  ;; should it NOT DO a next file if copy-value is NIL?
		  (setq copy-value
			(lexpr-funcall #'FS-COPY-FILE STREAM HOST
				       copy-options))
		  ;(if (not (eq copy-value T))
		  ;    (send stream ':NEXT-FILE))    ;No good to do :NEXT-FILE because  
		  ))					;the :CLOSE operation does an 
	DO (send stream ':CLOSE)))			;an :ADVANCE-FILE 

))

; From file MTSTR.LISP#> FILE; SRC:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR LISP 72"

(DEFMETHOD (MT-FILE-MIXIN :DEVICE) ()
  NIL)

))

; From file MTSTR.LISP#> FILE; SRC:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR LISP 72"

(DEFUN PUTPROP-MAYBE (PLIST VALUE PROP)
  (WHEN (AND VALUE (NULL (GET PLIST PROP)))
    (PUTPROP PLIST VALUE PROP)))

))

; From file MTSTR.LISP#> FILE; SRC:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTSTR LISP 72"

(DEFMETHOD (MT-FILEHANDLE :OPEN) (IGNORE &REST KEYWORD-ARGS
					 &KEY &OPTIONAL
					 (DIRECTION ':INPUT)
					 DEFAULTS-FROM-STREAM 
					 BYTE-SIZE
					 AUTHOR
					 &ALLOW-OTHER-KEYS)
  (COND ((EQ DIRECTION ':INPUT)
	 (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM KEYWORD-ARGS))
	((EQ DIRECTION ':OUTPUT)
	 (COND (DEFAULTS-FROM-STREAM
		(LET* ((TRUENAME (FUNCALL DEFAULTS-FROM-STREAM ':TRUENAME))
		       (PLIST
			 (FILTER-PLIST (FUNCALL DEFAULTS-FROM-STREAM ':PLIST)
				       (PLIST TRUENAME)))
		       (REAL-PLIST (LOCF PLIST)))
		  (PUTPROP-MAYBE REAL-PLIST BYTE-SIZE ':BYTE-SIZE)
		  (PUTPROP-MAYBE REAL-PLIST AUTHOR ':AUTHOR)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':DIRECTORY) ':DIRECTORY)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':NAME) ':NAME)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':TYPE) ':TYPE)
		  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':VERSION) ':VERSION)
   ;:UNSPECIFIC would not win when read back in, so guess.
		  (IF (EQ (GET (LOCF PLIST) ':TYPE)
			  ':UNSPECIFIC)
		      (LET ((FPLIST (FILE-READ-ATTRIBUTE-LIST NIL DEFAULTS-FROM-STREAM)))
			(PUT-ON-ALTERNATING-LIST
			  (COND ((EQ (GET (LOCF FPLIST) ':MODE)
				     ':LISP)
				 "LISP")
				((= BYTE-SIZE 10)
				 "TEXT")
				(T "UNKNOWN"))
			  PLIST
			  ':TYPE)))
		  (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM
				 ':PLIST PLIST
				 KEYWORD-ARGS)))
	       (T (FERROR NIL "MT: wins only for copying with stream default"))))
	(T (BREAK FOO T))))

))
