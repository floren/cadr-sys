;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Written 1/19/84 11:37:32 by LMFile,
;;; Reason: Define (for FS:RESTORE-MAGTAPE) the :ASK-AND-DEFAULT transform that queries for a
;;; a translation from one directory to another each time it encounters a new directory
;;; on tape.  The translations are stored as an alist in *ASK-PER-DIRECTORY-DEFAULTS*.
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.29, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.1, Experimental FILE-Server 8.2, Experimental LFS 3.1, Experimental MagTape 22.4, microcode 306, Xmntl FS.



; From file MTAUX.LISP PS:<L.TAPE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: TAPE; MTAUX  "

(defun (:property :ask-and-default tape-restore-transform) (host directory name type version)
  (let* ((dir (fs:make-pathname ':host host ':directory directory))
	 (tdir (cdr (assq dir *ask-per-directory-defaults*))))
    (if tdir
	(fs:merge-pathname-defaults
	  tdir (fs:make-pathname ':name name ':type type ':version version))
      (setq tdir (progn
		   (format t "~&Translation for the directory ~A ? " dir)
		   (send 
		     (fs:parse-pathname (readline-trim) () dir)
		     ':new-pathname ':name () ':type () ':version ())))
      (push (cons dir tdir) *ask-per-directory-defaults*)
      (fs:merge-pathname-defaults
	tdir (fs:make-pathname ':name name ':type type ':version version)))))

))

; From file MTAUX.LISP PS:<L.TAPE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: TAPE; MTAUX  "

(defvar *ask-per-directory-defaults* ()
  "Alist whose key is a pathname with just host, device, and directory components.
The associated value is another pathname of the same form for translation.")

))
