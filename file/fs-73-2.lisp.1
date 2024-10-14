;;; -*- Mode:Lisp; Readtable:T; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 73.2
;;; Reason:
;;;  Do not allow a directory to be created with the name "*" -- too many other places
;;;  are unable to deal properly with that name, treating it as a wildcard directory.
;;; Written 17-Nov-87 14:59:07 by pld at site LMI Cambridge
;;; while running on Djinn from band 2
;;; with Experimental System 123.110, Experimental Local-File 73.0, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental Tape 18.0, Experimental KERMIT 34.0, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, microcode 1754, SDU ROM 8.



; From modified file DJ: L.FILE; FSDEFS.LISP#210 at 17-Nov-87 14:59:10
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSDEFS  "

(DEFINE-ERRORS
  INVALID-WILDCARD              "Wildcard that got through pathname parsing but file system didn't like it."
  )

))

; From modified file DJ: L.FILE; FSGUTS.LISP#434 at 17-Nov-87 14:59:56
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN LOOKUP-SUBDIRECTORY-STEP (NODE STEP OK-IF-NOT-THERE)
  (READ-DIRECTORY-FILES NODE)
  (PROG ()
	;; ISNT LISP WONDERFULL.
	;; I REWROTE THIS AS A PROG SO THAT
	;; THE LM-SIGNAL-ERROR WOULD TAKE PLACE OUT SIDE
	;; OF THE SCOPE OF THE LOCKING-RECURSIVELY CALL.
	;; 5-Feb-87 18:14:39 -GJC
	(RETURN (LOCKING-recursively (DIRECTORY-LOCK NODE)
		  (MULTIPLE-VALUE-BIND (FILE LOC)
		      (LOOKUP-NAMED-FILE NODE
					 STEP
					 LMFS-DIRECTORY-TYPE
					 LMFS-DIRECTORY-VERSION)
		    (COND ((NOT (NULL FILE))
			   (LOCKING-recursively (FILE-LOCK FILE)
			     (IF (NOT (DIRECTORY? FILE))
				 (go expected-a-directory))
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
			   (go bad-directory-name))
			  (LM-AUTOMATICALLY-CREATE-DIRECTORIES
			   (LET ((DIRECTORY (CREATE-NEW-DIRECTORY NODE STEP)))
			     (PUSH DIRECTORY (CDR LOC))
			     (PROCESS-UNLOCK (LOCF (DIRECTORY-LOCK NODE)))
			     (WRITE-DIRECTORY-FILES NODE)
			     (IF (FBOUNDP 'GIVE-FILE-GENERATION-NUMBER)
				 (GIVE-FILE-GENERATION-NUMBER DIRECTORY))
			     DIRECTORY))
			  ((NOT OK-IF-NOT-THERE)
			   (GO DIRECTORY-NOT-FOUND))))))
     DIRECTORY-NOT-FOUND
	(LM-SIGNAL-ERROR 'DIRECTORY-NOT-FOUND)
     EXPECTED-A-DIRECTORY
	(lm-signal-error 'wrong-kind-of-file)
     bad-directory-name
	(lm-signal-error 'invalid-wildcard)
	))

))
