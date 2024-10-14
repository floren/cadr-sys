;;; -*- Mode:Lisp; Readtable:T; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 73.3
;;; Reason:
;;;  There seems to be a bug in the compiler such that if you GO to a tag that is outside an
;;;  unwind-protect that the cleanup forms don't get executed.  Someone should track that
;;;  down, but for now, fix (FS:LOOKUP-SUBDIRECTORY-STEP) such that it doesn't require
;;;  that to work, so that directory locks don't get left set.
;;; Written 24-Nov-87 15:21:21 by pld at site Gigamos Cambridge
;;; while running on Jack Flanders from band 2
;;; with Experimental System 123.124, Experimental Local-File 73.2, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental Tape 18.0, Experimental KERMIT 34.0, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, microcode 1754, SDU Boot Tape 3.12, SDU ROM 8.



; From modified file DJ: L.FILE; FSGUTS.LISP#435 at 24-Nov-87 15:21:32
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN LOOKUP-SUBDIRECTORY-STEP (NODE STEP OK-IF-NOT-THERE)
  (READ-DIRECTORY-FILES NODE)
  (case (catch 'lookup-subdirectory-step-error
	  (RETURN-from lookup-subdirectory-step
	    (LOCKING-recursively (DIRECTORY-LOCK NODE)
	      (MULTIPLE-VALUE-BIND (FILE LOC)
		  (LOOKUP-NAMED-FILE NODE
				     STEP
				     LMFS-DIRECTORY-TYPE
				     LMFS-DIRECTORY-VERSION)
		(COND ((NOT (NULL FILE))
		       (LOCKING-recursively (FILE-LOCK FILE)
			 (IF (NOT (DIRECTORY? FILE))
			     (throw 'lookup-subdirectory-step-error 'expected-a-directory))
			 (IF (LMFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? FILE)
			     (THROW 'FILE-BEING-WRITTEN-OR-SUPERSEDED FILE)
						;	   (LM-LOOKUP-ERROR 'OPEN-UNFINISHED-DIRECTORY
						;			    NODE
						;			    STEP
						;			    LMFS-DIRECTORY-TYPE
						;			    LMFS-DIRECTORY-VERSION)
			   )
			 (IF (FILE-DELETED? FILE)
			     (LM-LOOKUP-ERROR 'OPEN-DELETED-DIRECTORY
					      NODE
					      STEP
					      LMFS-DIRECTORY-TYPE
					      LMFS-DIRECTORY-VERSION)))
		       (READ-DIRECTORY-FILES FILE)	;make sure files are read in.
		       FILE)
		      ((eq step :wild)
		       (throw 'lookup-subdirectory-step-error 'bad-directory-name))
		      (LM-AUTOMATICALLY-CREATE-DIRECTORIES
		       (LET ((DIRECTORY (CREATE-NEW-DIRECTORY NODE STEP)))
			 (PUSH DIRECTORY (CDR LOC))
			 (PROCESS-UNLOCK (LOCF (DIRECTORY-LOCK NODE)))
			 (WRITE-DIRECTORY-FILES NODE)
			 (IF (FBOUNDP 'GIVE-FILE-GENERATION-NUMBER)
			     (GIVE-FILE-GENERATION-NUMBER DIRECTORY))
			 DIRECTORY))
		      ((NOT OK-IF-NOT-THERE)
		       (throw 'lookup-subdirectory-step-error 'DIRECTORY-NOT-FOUND)))))))
    ('DIRECTORY-NOT-FOUND
     (LM-SIGNAL-ERROR 'DIRECTORY-NOT-FOUND))
    ('EXPECTED-A-DIRECTORY
     (lm-signal-error 'wrong-kind-of-file))
    ('bad-directory-name
     (lm-signal-error 'invalid-wildcard))))

))
