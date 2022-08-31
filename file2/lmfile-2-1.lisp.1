;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for LFS version 2.1
;;; Reason: Load SYS: FILE2; EXTRA-FILES LISP (if it exists) when booting
;;;  server.
;;; Check to see if SYS host, not the associated machine, is up before
;;;  loading patches.
;;; Written 6/16/83 17:31:11 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with MIT-Specific 19.5, System 94.28, ZMail 50.10, Experimental Local-File 44.1, FILE-Server 6.6, MagTape 14.4, Experimental LFS 2.0, microcode 238, FC.



; From file SERVER.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; SERVER  "


(defvar *additional-filecomputer-server-files-loaded* ()
  "Hook to load up additional modules (more servers, etc.)")

(DEFUN BRING-UP-SERVER (&OPTIONAL FOR-WARM-BOOT)
  (DISABLE-FILE-SERVER)
  (DISABLE-MAIL-SERVER)
  (AND FILE-SYSTEM-RUNNING (NOT FOR-WARM-BOOT)
       (STOP-FILE-SYSTEM))
  (COND ((AND (NOT FILE-SYSTEM-RUNNING)
	      (OR (NOT FOR-WARM-BOOT)
		  (SI:WITH-TIMEOUT
		    (user-timeout #'(lambda ()
				      (format t "~&No answer after 5 minutes, Bring up Server.~&")
				      t)) ;;return t
		    (y-or-n-p "Bring up LMFile? "))))
	 (COND ((CHAOS:HOST-UP-P (or (si:get-site-option ':sys-host) si:associated-machine))
		(SI:WITH-SYS-HOST-ACCESSIBLE
		  (LOAD-PATCHES ':NOSELECTIVE)
		  (or *additional-filecomputer-server-files-loaded*
		      (condition-case (result) (probef "SYS: FILE2; EXTRA-FILES LISP")
			(fs:file-error t)
			(:no-error (not result)))
		      (format t "~&Loading more modules into server.~%") ; Returns ()
		      (progn (load "SYS: FILE2; EXTRA-FILES LISP" "USER" () T T)
			     (setq *additional-filecomputer-server-files-loaded* t))))
		)
	       (T (FORMAT T "~%Cannot connect to SYS host ~A to load the latest patches.
When it is up again, please call BRING-UP-SERVER."  (si:get-site-option ':sys-host))))

	 ;;Note: at a WARM boot, as opposed to a cold boot,
	 ;;this does not run, but LMFILE-WARM-BOOT handles everything.
	 (FORMAT T "~%Booting LMFILE on partition SRVR...")
	 (FS:CONSIDER-UNIT 2 "SRVR")
	 (FS:START-FILE-SYSTEM)
	 (UNLESS (GET 'LMFILE-CHAOS-HOST 'SI:FLAVOR)
	   (FS:ADD-LFS-HOST "FC"))
	 (FORMAT T "~%LMFILE booted and running.")))

  (COND (FILE-SYSTEM-RUNNING
	 (ENABLE-FILE-SERVER)
	 (ENABLE-MAIL-SERVER)))
	 
  ;; Random initializations.
  ;; Allow typeout to continue past the end of screen without hanging.
  (SETQ TV:MORE-PROCESSING-GLOBAL-ENABLE NIL)
  (SETQ USER-ID "LMFile")
  (fs:file-host-user-id "LMFILE" (si:parse-host "mc"))
  (setq fs:user-personal-name-first-name-first "The AI File Server")

  (PKG-GOTO 'FS))

))
