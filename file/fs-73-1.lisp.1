;;; -*- Mode:Lisp; Readtable:T; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 73.1
;;; Reason:
;;;  Somehow, directories are being created with the name "*".  This is bad.
;;;  The parent directory becomes un-listable.  (fs:lookup-subdirectories)
;;;  now ignores such directories.
;;; Written 17-Nov-87 14:00:34 by pld at site LMI Cambridge
;;; while running on Djinn from band 2
;;; with Experimental System 123.110, Experimental Local-File 73.0, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental Tape 18.0, Experimental KERMIT 34.0, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, microcode 1754, SDU ROM 8.



; From modified file DJ: L.FILE; FSGUTS.LISP#434 at 17-Nov-87 14:00:38
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN LOOKUP-SUBDIRECTORIES (NODE PATH)
  (MAPCAN #'(LAMBDA (FILE)
	      (COND ((NOT (DIRECTORY? FILE)) '())
		    ((EQ PATH ':WILD) (LIST FILE))
		    ((eq (directory-name file) :wild) '())
		    ((WILDCARD-MATCH (CAR PATH) (DIRECTORY-NAME FILE))
		     (IF (NULL (CDR PATH))
			 (LIST FILE)
			 (LOOKUP-SUBDIRECTORIES FILE (CDR PATH))))
		    (T '())))
	  (READ-DIRECTORY-FILES NODE)))

))
