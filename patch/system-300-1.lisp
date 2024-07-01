;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 300.1
;;; Reason:
;;;  Don't trap to debugger when trying to LOGIN if home directory doesn't exist.
;;; Written 1-Jul-24 16:07:36 by ams,
;;; while running on Lisp Machine One from band 3
;;; with Experimental System 300.0, microcode 323.



; From file OZ: /sys/sys2/login.lisp at 1-Jul-24 16:08:09
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //sys//sys2//login"

(DEFUN LOGIN (USER-NAME &OPTIONAL (HOST ASSOCIATED-MACHINE) INHIBIT-INIT-FILE-P)
  "Log in, specifying user name and host.
This identifies you, for the sake of other users running FINGER.
You cannot access files until you have logged in.
You can log in on any host that files can be read from, including
the local file system if one is loaded."
  ;; Do this so LOGIN init list has the correct enviroment.
  (DECLARE (SPECIAL USER-ID HOST))
  (LET ((WIN-P NIL)
	(LOAD-INIT-FILE-P (NOT INHIBIT-INIT-FILE-P)))
    (DECLARE (SPECIAL LOAD-INIT-FILE-P))
    (UNWIND-PROTECT
      (PROGN
	(LOGOUT)
	(AND (EQ HOST T)			;For compatibility
	     (SETQ HOST ASSOCIATED-MACHINE LOAD-INIT-FILE-P NIL))
	(SETQ USER-ID (STRING-TRIM '(#/SP) (STRING USER-NAME)))
	(SETQ HOST (FS:GET-PATHNAME-HOST HOST))
	(SETQ FS:USER-LOGIN-MACHINE HOST)
	(INITIALIZATIONS 'LOGIN-INITIALIZATION-LIST)
	(RESET-INITIALIZATIONS 'LOGOUT-INITIALIZATION-LIST)
	(PUSH (LIST USER-ID HOST
		    (AND (BOUNDP 'LOCAL-PRETTY-HOST-NAME) LOCAL-PRETTY-HOST-NAME)
		    (AND (FBOUNDP 'TIME:PRINT-CURRENT-TIME) (TIME:PRINT-CURRENT-TIME NIL)))
	      LOGIN-HISTORY)
	(PUSH (CONS HOST USER-ID) FS:USER-UNAMES)
	(FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) FS:*DEFAULT-PATHNAME-DEFAULTS*)
	;;(FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) FS:LOAD-PATHNAME-DEFAULTS)
	(SETQ WIN-P T)
	(WHEN LOAD-INIT-FILE-P
	  (CONDITION-CASE ()
	      (LOAD (FS:INIT-FILE-PATHNAME "LISPM" HOST)
		    :PACKAGE "USER"
		    :IF-DOES-NOT-EXIST NIL
		    :SET-DEFAULT-PATHNAME NIL) ; already done explicity above
	    ((FS:DIRECTORY-NOT-FOUND
		 (FORMAT *TERMINAL-IO*
			 "~&There does not seem to be directory for you on ~A." HOST))))))
      (UNLESS WIN-P
	;; If user aborts during login, particularly if he types Abort when
	;; being asked for his password, log him out so he can try again.  But
	;; if he aborts about of loading the init file, leave him logged in.
	(LOGOUT))))
  T)

))
