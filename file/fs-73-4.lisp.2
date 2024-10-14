;;; -*- Mode:Lisp; Readtable:T; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 73.4
;;; Reason:
;;;  (fs:initialize-file-system) needs to write the :header-block of the root directory
;;;  if the file system version is > 4.
;;; Written 24-Mar-88 16:36:10 by pld at site Gigamos Cambridge
;;; while running on Jack Flanders from band 2
;;; with Experimental System 123.218, Experimental Local-File 73.3, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 21.1, microcode 1755, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.FILE; FSGUTS.LISP#436 at 24-Mar-88 16:36:10
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN INITIALIZE-FILE-SYSTEM (&AUX PBASE PSIZE NECESSARY-PUT-SIZE)
  (*CATCH 'BOOT-FILE-SYSTEM
    ;; Attempt to find the LM band first; if can't,
    ;; will try current LM-UNIT/LM-PARTITION.
    (FIND-LM-BAND)
    ;; Now open up the partition for use.
    (MULTIPLE-VALUE (PBASE PSIZE)
      (FIND-DISK-PARTITION-FOR-WRITE LM-PARTITION NIL LM-UNIT))
    ;; User requests abort.
    (WHEN (NULL PBASE)
      (NOTIFY "Aborting initialization of file system in partition ~A." LM-PARTITION)
      (*THROW 'BOOT-FILE-SYSTEM NIL))
    (DISMOUNT-FILE-SYSTEM)
    ;; Setup the DISK-CONFIGURATION variables.
    (SETQ DISK-CONFIGURATION (MAKE-DISK-CONFIGURATION)
	  DISK-CONFIGURATION-LOCK NIL
	  DISK-CONFIGURATION-RQB (GET-DISK-RQB)
	  DISK-CONFIGURATION-BUFFER (GET-RQB-ARRAY DISK-CONFIGURATION-RQB 8))
    ;; From the size of the partition, compute the size of the PUT
    (SETQ NECESSARY-PUT-SIZE (CEILING (* 2 PSIZE) PAGE-SIZE-IN-BITS))
    ;; Setup the PUT variables.
    (SETQ PUT-RQB (CONDITION-BIND ((RQB-TOO-LARGE #'LM-PUT-RQB-TOO-LARGE))
		    (GET-DISK-RQB NECESSARY-PUT-SIZE))
	  PAGE-USAGE-TABLE (GET-RQB-ARRAY PUT-RQB 2)
	  PUT-LOCK NIL
	  PUT-MODIFIED NIL
	  PUT-SCANNING-INDEX 1)
    ;; Setup the DISK-CONFIGURATION structure.
    (SETF (DC-VERSION) LM-VERSION)
    (SETQ LM-PARTITION-BASE PBASE)
    (SETF (DC-PARTITION-SIZE) PSIZE)
    ;; Put the PUT at the beginning of the partition for now.
    (SETF (DC-PUT-BASE) 1)
    (SETF (DC-PUT-SIZE) NECESSARY-PUT-SIZE)
    ;; Make an root directory.
    (SETF (DC-ROOT-DIRECTORY)
	  (CREATE-NEW-DIRECTORY NIL LMFS-ROOT-DIRECTORY-NAME))
    ;; Now zero the PUT
    (COPY-ARRAY-CONTENTS "" (RQB-BUFFER PUT-RQB))
    ;; Set it to PUT-CONSISTENT before entering USING-PUT
    (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
    (REINITIALIZE-PUT-USAGE-ARRAY)
    (USING-PUT
      (CHANGE-BLOCK-DISK-SPACE 1 NECESSARY-PUT-SIZE PUT-FREE PUT-USED)
      (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
      (when (> (dc-version) 4)
	;;If we think the root directory has a :header-block, make sure it exists on disk
	(lmfs-write-new-format-directory (dc-root-directory) ""))
      (LOCKING DISK-CONFIGURATION-LOCK
	(LM-WRITE-CONFIGURATION)
	;; Update the comment to say what's in the partition.
	(UPDATE-PARTITION-COMMENT LM-PARTITION "LM File System" LM-UNIT)))))

))

; From modified file DJ: L.FILE; FSGUTS.LISP#436 at 24-Mar-88 16:50:01
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN READ-PAGE-USAGE-TABLE (&AUX MUST-SALVAGE)
  (LM-DISK-READ PUT-RQB (DC-PUT-BASE))
  (SELECT (AREF PAGE-USAGE-TABLE 0)
    (PUT-CONSISTENT)
    (PUT-INCONSISTENT
     ;; Don't salvage here!!  No directories exist yet.
     (SETQ MUST-SALVAGE T))
    (OTHERWISE
     (CERROR T NIL NIL "Invalid state designator in page usage table")
     (SETQ MUST-SALVAGE T)))
  (REINITIALIZE-PUT-USAGE-ARRAY)
  MUST-SALVAGE)

))
