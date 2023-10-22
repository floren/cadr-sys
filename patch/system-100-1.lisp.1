;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.1
;;; Reason:
;;;  Fix FS:DEFAULT-PATHNAME so that it works with ZWEI when user is not logged in.
;;; Written 3-Apr-23 03:02:32 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.0, microcode 323.



; From file OZ: /home/ams/l/sys/io/file/pathnm.lisp at 3-Apr-23 03:02:41
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//io//file//pathnm"

;;; Returns the default for the given host from defaults.
;;; INTERNAL-P means this function is being called from inside the parsing function and
;;; cannot do any parsing itself, but must just return something to accept messages.
;;; DEFAULTS can also be an atom, which is used as a default.
(DEFUN DEFAULT-PATHNAME (&OPTIONAL DEFAULTS HOST DEFAULT-TYPE DEFAULT-VERSION INTERNAL-P
			 &AUX ELEM PATHNAME HOST-TO-USE CTYPE OTYPE)
  (AND HOST (SETQ HOST (GET-PATHNAME-HOST HOST)))
  ;; Defaults '(NIL) '((NIL)) have been seen prior to login.
  (WHEN (OR (NULL DEFAULTS) (EQUAL DEFAULTS '(NIL)) (EQUAL DEFAULTS '((NIL))))
    (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((AND DEFAULTS (ATOM DEFAULTS))
	 (SETQ PATHNAME (PARSE-PATHNAME DEFAULTS)))
	(T
	 (SETQ ELEM (COND ((NOT *DEFAULTS-ARE-PER-HOST*) (ASSQ NIL DEFAULTS))
			  (HOST (ASSQ HOST DEFAULTS))
			  (T (DOLIST (DEFAULT DEFAULTS)	;Last host mentioned
			       (AND (CDR DEFAULT) (RETURN DEFAULT))))))
	 ;; If none better found, take the one for the login machine
	 (OR (CDR ELEM)
	     (SETQ ELEM (OR (AND USER-LOGIN-MACHINE (ASSQ USER-LOGIN-MACHINE DEFAULTS))
			    (IF (NULL USER-LOGIN-MACHINE)
				(NCONS SI:ASSOCIATED-MACHINE)
			      (NCONS USER-LOGIN-MACHINE)))))
	 ;; If there isn't one already, build a pathname from the host of this one
	 (SETQ HOST-TO-USE (OR HOST (CAR ELEM) (PATHNAME-HOST (CDR ELEM))))
	 (COND ((SETQ PATHNAME (CDR ELEM)))
	       (INTERNAL-P
		(SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST-TO-USE NIL NIL NIL NIL NIL)))
	       (T
		(SETQ PATHNAME (SEND (USER-HOMEDIR HOST-TO-USE) :NEW-PATHNAME
				     :NAME "FOO" :TYPE *NAME-SPECIFIED-DEFAULT-TYPE*
				     :VERSION :NEWEST))
		(SETF (CDR ELEM) PATHNAME)))))
  ;; If default-type or default-version was given, or the host has changed,
  ;; merge those in.
  (AND (OR (AND HOST (NEQ HOST (PATHNAME-HOST PATHNAME))) DEFAULT-TYPE DEFAULT-VERSION)
       (SETQ HOST (OR HOST (PATHNAME-HOST PATHNAME)))
       (IF INTERNAL-P
	   (AND HOST (SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST NIL NIL NIL NIL NIL)))
	 (SETF (VALUES CTYPE OTYPE) (SEND PATHNAME :CANONICAL-TYPE))
	 (SETQ PATHNAME (SEND (MAKE-PATHNAME :HOST HOST :DEFAULTS NIL)
			      :NEW-PATHNAME
			      :DIRECTORY (PATHNAME-DIRECTORY PATHNAME)
			      :DEVICE (PATHNAME-DEVICE PATHNAME)
			      :HOST (OR HOST (PATHNAME-HOST PATHNAME))
			      :NAME (PATHNAME-NAME PATHNAME)
			      :CANONICAL-TYPE CTYPE
			      :ORIGINAL-TYPE OTYPE
			      :VERSION (OR DEFAULT-VERSION (PATHNAME-VERSION PATHNAME))
			      ))))
  PATHNAME)
))
