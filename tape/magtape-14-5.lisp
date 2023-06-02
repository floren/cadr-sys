;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for MagTape version 14.5
;;; Reason: Restore files with structured directories correctly.
;;; Written 10/26/83 14:36:51 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 4
;;; with MIT-Specific 19.5, System 94.41, ZMail 50.17, Experimental Local-File 44.3, FILE-Server 6.6, MagTape 14.4, Experimental LFS 2.4, microcode 238, FC.



; From file MTAUX.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; MTAUX  "

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
			     (not (y-or-n-p "Restore ? ")))))
		 (T
;;  ;until we get structured directories...
;;		  (COND ((CONSP (SETQ TEM (SEND STREAM ':DIRECTORY)))
;;			 (SEND STREAM ':PUTPROP  (FN-CONCATENATE TEM) ':DIRECTORY)))
		  ;; should it NOT DO a next file if copy-value is NIL?
		  (setq copy-value
			(lexpr-funcall #'FS-COPY-FILE STREAM HOST
				       copy-options))
		  ;(if (not (eq copy-value T))
		  ;    (send stream ':NEXT-FILE))    ;No good to do :NEXT-FILE because  
		  ))					;the :CLOSE operation does an 
	DO (send stream ':CLOSE)))			;an :ADVANCE-FILE 

))
