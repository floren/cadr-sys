;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for Tape version 21.2
;;; Reason:
;;;  (fs:copy-directory "A:~;" "B:~;" :copy-subdirectories t) copied the root
;;;  and subdirectories of host A: into directory "FOO" on host B:.  This is
;;;  almost exactly what I did NOT want to happen.
;;; Written 30-Mar-88 13:38:24 by pld at site Gigamos Cambridge
;;; while running on Jack Flanders from band 1
;;; with Experimental System 123.218, Experimental Local-File 73.4, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 21.1, microcode 1755, SDU Boot Tape 3.14, SDU ROM 8.



; From file DJ: L.TAPE; COPY.LISP#169 at 30-Mar-88 13:38:24
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; COPY  "

(DEFUN COPY-DIRECTORY (DIR TO &OPTIONAL &REST OPTIONS
		       &KEY COPY-ONLY SELECTIVE (RECOPY-FILE-ON-EOT T) (copy-subdirectories t)
		       FILTER ONLY-LATEST (SINCE 0) (FUNCTION #'FS-COPY-FILE)
		       &ALLOW-OTHER-KEYS
		       &AUX WHOLE-DIR-LIST OUTPUT-DIR-LIST TAPE-DUMP FILE)
  "A utility for copying directories.
Options:
 :COPY-ONLY     One of NIL (default), :SOURCE, :NEWEST, or :QFASL.
 :SELECTIVE     One of NIL (default) or T. If not NIL, asks whether to copy each element.
 :COPY-SUBDIRECTORIES 
                One of NIL, T (default), or :WILD meaning to use wild name, type, and version.
 :FILTER        One of NIL (default) or a function to be called with the directory
                  element as its single argument.  If value returned by function is 
                  non-NIL, the element is copied.
 :ONLY-LATEST   One of NIL (default) or T. If not NIL, force :NEWEST from the source directory.
 :SINCE date    Copy only files created after date, a string or Universal Time (an integer)."
  (SETQ DIR (PARSE-PATHNAME DIR))
  (SETQ DIR (SEND DIR :NEW-PATHNAME
		  :NAME (IF (MEMQ (SEND DIR :NAME) '(NIL :UNSPECIFIC))
			    :WILD
			  (SEND DIR :NAME))
		  :TYPE (IF (MEMQ (SEND DIR :TYPE) '(NIL :UNSPECIFIC))
			    :WILD
			  (SEND DIR :TYPE))
		  :VERSION (IF ONLY-LATEST :NEWEST
			     (IF (MEMQ (SEND DIR :VERSION) '(NIL :UNSPECIFIC))
				 :WILD
			       (SEND DIR :VERSION)))))
  (UNLESS (TYPEP TO 'MT-FILEHANDLE) (SETQ TO (PARSE-PATHNAME TO)))
  (UNLESS (TYPEP TO '(OR PATHNAME MT-FILEHANDLE))
    (SETQ TO (MERGE-PATHNAME-DEFAULTS DIR :WILD :WILD)))
  (UNLESS (NUMBERP SINCE) (SETQ SINCE (TIME:PARSE-UNIVERSAL-TIME SINCE)))
  (TYPECASE TO
    (MT-FILEHANDLE (SETQ TAPE-DUMP T))  ;must test first, since is a subtype of pathname.
    (PATHNAME
     (DOLIST (PART '(:DIRECTORY :NAME :TYPE))
       (LET ((COMPONENT (send TO PART))) ; Yow -- sending a non-constant message !
	 (IF (MEMQ COMPONENT '(nil :unspecific))
	     (SETQ TO (SEND TO :NEW-PATHNAME PART :WILD)))))
     (SETQ TO (SEND TO :NEW-VERSION :WILD))) ; must always be :WILD for accuracy in dir list
    )
  (SETQ WHOLE-DIR-LIST (DIRECTORY-LIST DIR))
  (IF (AND (NOT TAPE-DUMP)
	   (TYPEP TO 'PATHNAME))
      (ERRSET (SETQ OUTPUT-DIR-LIST (DIRECTORY-LIST TO))
	      NIL))
  (DOLIST (F WHOLE-DIR-LIST)
    (AND (SETQ FILE (CAR F))
	 (NOT (GET F :LINK-TO))
	 (OR (NOT SELECTIVE) (Y-OR-N-P "Copy ~A ?" FILE))
	 (IF (get f :directory)
	     (WHEN copy-subdirectories
	       (let* ((d (send file :directory))
		      (SUBDIRECTORY (cond ((CLI:LISTP D)
					   (APPEND D (NCONS (SEND file :NAME))))
					  ((eq d ':root)
					   (send file :name))
					  (t (LIST D (send file :NAME))))))
		 (APPLY 'copy-directory
			(IF (EQ COPY-SUBDIRECTORIES :WILD)
			    (SEND file :new-pathname
				  :directory SUBDIRECTORY :name :wild :type :wild
				  :version (SEND DIR :VERSION))
			  (SEND DIR :NEW-DIRECTORY SUBDIRECTORY))
			(if (and (not tape-dump) (typep to 'pathname))
			    (SEND TO :NEW-DIRECTORY
				  (let ((olddir (send to :directory))
					(newdir (send file :name)))
				    (cond ((consp olddir)
					   (append olddir (ncons newdir)))
					  ((eq olddir :root)
					   newdir)
					  (t
					   (list olddir newdir)))))
			  TO)
			options)))
	   (WHEN (AND (OR (NULL COPY-ONLY)
			  (CHECK-COPY-ONLY FILE COPY-ONLY WHOLE-DIR-LIST))
		      (> (GET F :CREATION-DATE) SINCE)
		      (OR (NULL FILTER) (FUNCALL FILTER F)))
	     (PROGV (IF RECOPY-FILE-ON-EOT '(*MT-EOT-HANDLER*) NIL)
		    '(COPY-EOT-HANDLER)
	       (DO ((V) (EOT))
		   (())
		 (MULTIPLE-VALUE (V EOT)
		   (*CATCH (IF RECOPY-FILE-ON-EOT :EOT :NEVER)
		     (APPLY FUNCTION FILE TO
			    :DIRECTORY-LIST (CDR F) :OUTPUT-DIRECTORY-LIST OUTPUT-DIR-LIST
			    OPTIONS)))
		 (IF (NULL EOT)
		     (RETURN)
		   (COPY-MOUNT-NEXT-TAPE TAPE-DUMP)))))))))

))
