;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Written 2/16/84 08:09:43 by LMFile,
;;; Reason: :COPY-SUBDIRECTORIES and :FILTER for FS:COPY-DIRECTORY
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.34, CADR 3.6, ZMail 53.10, MIT-Specific 22.0, Experimental Local-File 48.3, Experimental FILE-Server 8.2, Experimental LFS 3.3, Experimental MagTape 22.5, microcode 306, XFS/C.



; From file COPY.LISP PS:<L.TAPE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: TAPE; COPY  "


(DEFUN COPY-DIRECTORY (DIR TO
		       &OPTIONAL &REST OPTIONS
		       &KEY
		       COPY-ONLY
		       SELECTIVE
		       (RECOPY-FILE-ON-EOT T)
		       (ONLY-LATEST NIL)
		       (copy-subdirectories t)
		       (SINCE NIL)
		       FILTER
		       &ALLOW-OTHER-KEYS
		       &AUX WHOLE-DIR-LIST OUTPUT-DIR-LIST ODIR TAPE-DUMP)
  "Copy the DIR to TO."
  (SETQ DIR (PARSE-PATHNAME DIR))
  (SETQ DIR (FUNCALL DIR ':NEW-PATHNAME
		     ':NAME (IF (MEMQ (FUNCALL DIR ':NAME) '(NIL :UNSPECIFIC))
				':WILD
				(FUNCALL DIR ':NAME))
		     ':TYPE (IF (MEMQ (FUNCALL DIR ':TYPE) '(NIL :UNSPECIFIC))
				':WILD
				(FUNCALL DIR ':TYPE))
		     ':VERSION (IF ONLY-LATEST ':NEWEST
				 (IF (MEMQ (FUNCALL DIR ':VERSION) '(NIL :UNSPECIFIC))
				     ':WILD
				   (FUNCALL DIR ':VERSION)))))
  (SETQ ODIR (PARSE-PATHNAME TO))
  (AND FILTER
       (OR (FUNCTIONP FILTER) (SETQ FILTER (GET FILTER 'COPY-FILTER))))
  (COND ((TYPEP ODIR 'PATHNAME)
	 (SETQ ODIR (FUNCALL ODIR ':NEW-PATHNAME
		      ':NAME (IF (MEMQ (FUNCALL ODIR ':NAME) '(NIL :UNSPECIFIC))
				 ':WILD
			       (FUNCALL ODIR ':NAME))
		      ':TYPE (IF (MEMQ (FUNCALL ODIR ':TYPE) '(NIL :UNSPECIFIC))
				 ':WILD
			       (FUNCALL ODIR ':TYPE))
		      ':VERSION (IF (MEMQ (FUNCALL ODIR ':VERSION) '(NIL :UNSPECIFIC :NEWEST))
				    ':WILD
				  (FUNCALL ODIR ':VERSION)))))
	((TYPEP ODIR 'MT-FILEHANDLE)
	 (SETQ TAPE-DUMP T)))
  (SETQ WHOLE-DIR-LIST (DIRECTORY-LIST DIR))
  (COND ((TYPEP ODIR 'PATHNAME)
	 (ERRSET (SETQ OUTPUT-DIR-LIST (DIRECTORY-LIST ODIR))
		 NIL)))
  (DOLIST (F (IF SINCE (ELIMINATE-DATED-FILES (TIME:PARSE-UNIVERSAL-TIME SINCE)
					      WHOLE-DIR-LIST) WHOLE-DIR-LIST))
    (AND (CAR F)
	 (NOT (GET F ':LINK-TO))
	 (OR (NOT SELECTIVE)
	     (PROGN (FORMAT QUERY-IO "Copy ~A ?" (CAR F))
		    (Y-OR-N-P)))
	 (OR (NULL COPY-ONLY)
	     (CHECK-COPY-ONLY (CAR F) COPY-ONLY WHOLE-DIR-LIST))
	 (cond ((get f ':directory)
		(cond (copy-subdirectories
		       (let ((d (funcall (car f) ':directory)))
			 (cond ((not (listp d)) (setq d (list d))))
			 (lexpr-funcall #'copy-directory
					(funcall (car f) ':new-pathname
						 ':directory (append d
								     (list (funcall (car f)
										    ':name)))
						 ':name ':wild
						 ':type ':wild
						 ':version ':wild)
					to
					options)))))
	       (t						     
		(PROGV (IF RECOPY-FILE-ON-EOT '(*MT-EOT-HANDLER*) NIL)
		       '(COPY-EOT-HANDLER)
		  (DO ((V) (EOT))
		      (())
		    (MULTIPLE-VALUE (V EOT)
		      (*CATCH (IF RECOPY-FILE-ON-EOT ':EOT ':NEVER)
			(IF (OR (NULL FILTER) (FUNCALL FILTER F))
			    (LEXPR-FUNCALL #'FS-COPY-FILE (CAR F) TO ':DIRECTORY-LIST (CDR F)
					   ':OUTPUT-DIRECTORY-LIST OUTPUT-DIR-LIST OPTIONS))))
		    (IF (NULL EOT)
			(RETURN)
		      (COPY-MOUNT-NEXT-TAPE TAPE-DUMP)))))))))

(DEFVAR *DIR*)
(DEFVAR *SWITCH*)
(DEFUN DUMP-AFTER-DIRECTORY-FILTER (*DIR*)
  (LET ((*SWITCH* NIL))
    (CLOSURE '(*DIR* *SWITCH*) (FUNCTION DUMP-AFTER-DIRECTORY-FILTER-INTERNAL))))

(DEFUN DUMP-AFTER-DIRECTORY-FILTER-INTERNAL (F)
  (COND (*SWITCH* T)
	((EQUAL *DIR* (FUNCALL (CAR F) ':DIRECTORY))
	 (SETQ *SWITCH* T))
	(T NIL)))

))
