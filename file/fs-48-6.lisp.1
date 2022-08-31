;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11/21/84 02:56:44 by LMFile, -----really 4PM on 11/21
;;; Reason: Fix fs:Save-directory-subtree so as not to blow out when given NIL.
;;; filecomputer should boot OK now.
;;; turn on normal servers on filecomputer
;;; Reason: fix initial lossage
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.76, CADR 3.10, ZMail 53.19, MIT-Specific 22.5, Experimental Local-File 48.5, Experimental FILE-Server 8.5, Experimental LFS 3.3, Experimental MagTape 22.6, microcode 309, Really Kludged 98.76 but it should boot.


; From file FSGUTS.LISP KANSAS:<L.FILE> OZ:
#10R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN SAVE-DIRECTORY-SUBTREE (DIRECTORY DO-ALL?)
  "Save all of the files in the subtree DIRECTORY.  Save all subdir subdirs if DO-ALL?"
  (and DIRECTORY
       (LET ((FILES (DIRECTORY-FILES DIRECTORY)))
	 (LET ((SUBDIRECTORY-RESULTS
		 (MAPCAN #'(LAMBDA (FILE)
			     (AND (DIRECTORY? FILE)
				  (OR DO-ALL? (NOT (FILE-CLOSED? FILE)))
				  (SAVE-DIRECTORY-SUBTREE FILE DO-ALL?)))
			 (COND ((NOT (EQ FILES ':DISK))
				FILES)
			       ((EQ DO-ALL? ':SAVE-ALL)
				(READ-DIRECTORY-FILES DIRECTORY))
			       (T '())))))
	   (IF (OR (EQ DO-ALL? ':SAVE-ALL)
		   (NOT (FILE-CLOSED? DIRECTORY)))
	       (CONS (LMFS-WRITE-DIRECTORY DIRECTORY) SUBDIRECTORY-RESULTS)
	     SUBDIRECTORY-RESULTS)))))

))

; From file SERVER.LISP KANSAS:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; SERVER  "

(ADD-INITIALIZATION "Turn on LM Servers" '(SETQ CHAOS:CHAOS-SERVERS-ENABLED T ) '(WARM))

))
