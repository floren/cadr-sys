;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.5
;;; Reason:
;;;  FS:*MERGE-PATHNAME-ALLOW-UNSPECIFIED-TYPE*: New variable to control if object components can be without a LISP type in DEFSYSTEM.
;;;  EDIT-DISK-lABEL: Reads the label when invokes, no need to C-r.
;;;  COMPILE-FILE: Arguments now match Common Lisp.
;;; Written 16-Apr-23 11:16:00 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.4, microcode 323.



; From file OZ: /home/ams/l/sys/sys/qcfile.lisp at 16-Apr-23 11:16:30
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "OZ: //tree//sys//qcfile"

(DEFUN COMPILE-FILE (&OPTIONAL INPUT-FILE
		     &KEY OUTPUT-FILE
		     (SET-DEFAULT-PATHNAME T)
		     ((:PACKAGE PACKAGE-SPEC)) LOAD)
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the standard defaults.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE if non-NIL is the package to compile in.
LOAD means to load the compiled file."
  (QC-FILE (OR INPUT-FILE "") OUTPUT-FILE
	   LOAD NIL PACKAGE-SPEC NIL
	   (NOT SET-DEFAULT-PATHNAME)))
))

; From file OZ: /home/ams/l/sys/io/dledit.lisp at 16-Apr-23 11:16:58
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//io//dledit"

(DEFUN EDIT-DISK-LABEL (&OPTIONAL (LE-UNIT 0) (INIT-P NIL)
					      ;If t, dont try to save page 1 since current
					      ; label is garbage.  It can bomb setting
					      ; blocks-per-track to 0, etc.
			&AUX LE-RQB LE-STRUCTURE (LE-ITEM-NUMBER 0) CH COM ABORT)
  "Edit the label of a disk pack.
LE-UNIT is the disk drive number, or a name of a machine (the chaosnet is used),
or /"CC/" which refers to the machine being debugged by this one."
  (SETQ LE-SOMETHING-CHANGED NIL)		;restart
  (SETQ LE-UNIT (DECODE-UNIT-ARGUMENT LE-UNIT "editing label" INIT-P))
  (UNWIND-PROTECT
     (PROGN (WITHOUT-INTERRUPTS
	     (SETQ LE-RQB (GET-DISK-LABEL-RQB)))
	    (LE-INITIALIZE-LABEL LE-RQB (CAR PACK-TYPES))
	    (IF (NULL INIT-P)
		(READ-DISK-LABEL LE-RQB LE-UNIT))
	    (LE-DISPLAY-LABEL LE-RQB LE-UNIT T)
	    (FORMAT T "Use Control-R to read and edit existing label; hit HELP for help.~%")
	    (PRINC "Label Edit Command: ")
	    (*CATCH 'LE-EXIT
		    (DO-FOREVER
		      (SETQ CH (SEND *TERMINAL-IO* ':TYI))
		      (SETQ COM (INTERN-SOFT (STRING-UPCASE (FORMAT NIL "LE-COM-~:C" CH))
					     "SI"))
		      (COND ((OR (NULL COM)	;nothing typed
				 (NOT (FBOUNDP COM)))	;command not defined
			     (BEEP)
			     (FORMAT T "~%~:C is not a known edit-disk-label command.  Type ~:C for help, or ~:C to exit." CH #/HELP #/END))
			    (T (MULTIPLE-VALUE (NIL ABORT)
				 (CATCH-ERROR-RESTART ((ERROR SYS:ABORT)
						       "Return to EDIT-DISK-LABEL.")
				   (FUNCALL COM)))
			       (AND ABORT (LE-DISPLAY-LABEL LE-RQB LE-UNIT)))))))
     (RETURN-DISK-RQB LE-RQB)
     (DISPOSE-OF-UNIT LE-UNIT)))
))

; From file OZ: /home/ams/l/sys/io/file/pathnm.lisp at 16-Apr-23 11:18:14
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//io//file//pathnm"

(DEFCONST *MERGE-PATHNAME-ALLOW-UNSPECIFIED-TYPE* NIL
  "This is mainly for DEFSYSTEM to allow modules with distinct
source/object components to not have object components include the
LISP type in their pathnames.")

))

; From file OZ: /home/ams/l/sys/io/file/pathnm.lisp at 16-Apr-23 11:18:57
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//io//file//pathnm"

;;; What a crock.
;;; Fill in slots in PATHNAME from program defaults.  This is what most programs interface to.
;;; They should often be interfacing to merge-pathname-components instead!!
(DEFUN MERGE-PATHNAME-DEFAULTS (PATHNAME
				&OPTIONAL DEFAULTS
					  (DEFAULT-TYPE *NAME-SPECIFIED-DEFAULT-TYPE*)
					  (DEFAULT-VERSION :NEWEST)
					  ALWAYS-MERGE-TYPE
				&AUX HOST DEFAULT SECONDARY-DEFAULT
				     NEW-DEVICE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION
				     NEW-OTYPE)
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
the type and version components, iff a name was specified
and FS:*ALWAYS-MERGE-TYPE-AND-VERSION* is NIL.
Otherwise, the type and version are obtained from DEFAULTS,
and DEFAULT-TYPE and DEFAULT-VERSION are not used.
If ALWAYS-MERGE-TYPE is non-NIL, that forces the type component
to be merged like the name, directory, etc. but has no effect on the version."
  (SETQ PATHNAME (PARSE-PATHNAME PATHNAME NIL DEFAULTS))
  (IF (NULL DEFAULTS)
      (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((NOT (TYPEP PATHNAME 'PATHNAME))
	 PATHNAME)				;Some funny thing.  No defaulting possible.
	(T
	 ;; Host always comes from pathname
	 (SETQ HOST (PATHNAME-HOST PATHNAME))
	 ;; Setup default pathnames.  If a pathname is supplied as the defaults,
	 ;; then two levels of defaulting are needed, otherwise only one.
	 (IF (ATOM DEFAULTS)			;if not defaults.
	     (SETQ DEFAULT (PARSE-PATHNAME DEFAULTS NIL PATHNAME)
		   DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*
		   SECONDARY-DEFAULT (DEFAULT-PATHNAME DEFAULTS HOST)
		   )
	     (SETQ DEFAULT (DEFAULT-PATHNAME DEFAULTS HOST)
		   SECONDARY-DEFAULT NIL)
		   )
	 ;; Device name DSK means the working directory and associated device if any.
	 (COND ((EQUAL (PATHNAME-DEVICE PATHNAME) "DSK")
		(LET ((WDIR (OR (GET HOST 'WORKING-DIRECTORY) (USER-HOMEDIR HOST))))
		  (SETQ NEW-DEVICE
			(OR (SEND WDIR :DEVICE)
			    (SEND HOST :PRIMARY-DEVICE)))
		  (IF (AND (NULL (PATHNAME-DIRECTORY PATHNAME))
			   ;; Don't do this when explicit directory supplied.
			   (NULL (PATHNAME-DIRECTORY DEFAULT))
			   (OR (NULL SECONDARY-DEFAULT)
			       (NULL (PATHNAME-DIRECTORY SECONDARY-DEFAULT))))
		      (SETQ NEW-DIRECTORY
			    (SEND WDIR :DIRECTORY))))))
	 ;; Merge the device, directory, and name
	 (IF (NULL (PATHNAME-DEVICE PATHNAME))
	     (SETQ NEW-DEVICE
		   (OR (PATHNAME-DEVICE DEFAULT)
		       (AND (NOT (NULL SECONDARY-DEFAULT))
			    (PATHNAME-DEVICE SECONDARY-DEFAULT))
		       )))
	 (UNLESS NEW-DIRECTORY
	   (LET ((PDIR (PATHNAME-DIRECTORY PATHNAME))
		 (DDIR (OR (PATHNAME-DIRECTORY DEFAULT)
			   (AND (NOT (NULL SECONDARY-DEFAULT))
				(PATHNAME-DIRECTORY SECONDARY-DEFAULT))
			   )))
	     (COND ((NULL PDIR)
		    (SETQ NEW-DIRECTORY DDIR))
		   ((EQ (CAR-SAFE PDIR) :RELATIVE)
		    (SETQ NEW-DIRECTORY
			  (MERGE-RELATIVE-DIRECTORY PDIR DDIR))))))
	 (IF (NULL (PATHNAME-NAME PATHNAME))
	     (SETQ NEW-NAME
		   (OR (PATHNAME-NAME DEFAULT)
		       (AND (NOT (NULL SECONDARY-DEFAULT))
			    (PATHNAME-NAME SECONDARY-DEFAULT))
		       ;; Never let the name of the resulting pathname be NIL.
		       "FOO")))
		       
	 ;; Merge the type and version if the name was NIL before the above merge,
	 ;; or if the user says to always do so.
	 (IF (NULL (PATHNAME-TYPE PATHNAME))
	     (IF (OR (NULL (PATHNAME-NAME PATHNAME))
		     ALWAYS-MERGE-TYPE
		     *ALWAYS-MERGE-TYPE-AND-VERSION*)
		 (PROGN
		   (SETF (VALUES NEW-TYPE NEW-OTYPE)
			 (SEND DEFAULT :CANONICAL-TYPE))
                   (IF (AND *MERGE-PATHNAME-ALLOW-UNSPECIFIED-TYPE*
                            (NULL DEFAULT-TYPE))
                       (SETQ NEW-TYPE DEFAULT-TYPE)
		       (UNLESS NEW-TYPE
    		         (SETQ NEW-TYPE
                           (IF (AND *MERGE-PATHNAME-ALLOW-UNSPECIFIED-TYPE*
				    (NULL DEFAULT-TYPE))
                               DEFAULT-TYPE
			       (OR (AND (NOT (NULL SECONDARY-DEFAULT))
				        (PATHNAME-TYPE SECONDARY-DEFAULT))
			           ;; Never let the type of the resulting pathname be NIL.
			           DEFAULT-TYPE)))))
	       )
	       (SETQ NEW-TYPE DEFAULT-TYPE)))
	 (IF (NULL (PATHNAME-VERSION PATHNAME))
	     (IF (OR (NULL (PATHNAME-NAME PATHNAME))
		     *ALWAYS-MERGE-TYPE-AND-VERSION*)
		 (SETQ NEW-VERSION
		       (OR (PATHNAME-VERSION DEFAULT)
			   (AND (NOT (NULL SECONDARY-DEFAULT))
				(PATHNAME-VERSION SECONDARY-DEFAULT))
			   ;; Never let the version of the resulting pathname be NIL.
			   DEFAULT-VERSION))
	       (SETQ NEW-VERSION DEFAULT-VERSION)))
	 (SEND PATHNAME :NEW-PATHNAME
	       		(IF NEW-DEVICE :DEVICE) NEW-DEVICE
			(IF NEW-DIRECTORY :DIRECTORY) NEW-DIRECTORY
			(IF NEW-NAME :NAME) NEW-NAME
			(IF NEW-TYPE :TYPE) NEW-TYPE
			(IF NEW-OTYPE :ORIGINAL-TYPE) NEW-OTYPE
			(IF NEW-VERSION :VERSION) NEW-VERSION))))
))

; From file OZ: /home/ams/l/sys/sys2/maksys.lisp at 16-Apr-23 11:19:21
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys2//maksys"

(DEFUN CANONICALIZE-PATHNAME
       (PATHNAME &OPTIONAL (DEFAULT *SYSTEM-PATHNAME-DEFAULT*))
  (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* NIL)
	(FS:*MERGE-PATHNAME-ALLOW-UNSPECIFIED-TYPE* T))
    (FS:MERGE-PATHNAME-DEFAULTS PATHNAME DEFAULT NIL)))
))
