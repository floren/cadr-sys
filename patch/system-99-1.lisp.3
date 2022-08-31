;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Private patches made by RMS
;;; Reason: COPY-FILE, RENAMEF parsing new file name.
;;; Make FASL-TEMP-AREA, FASL-TABLE-AREA dynamic.
;;; :DELETE-MULTIPLE-FILES pathname operation exists.
;;; MERGE-PATHNAME-DEFAULTS bug.
;;; :REMOTE-CONNECT bug.  CHAOS:RETRANSMISSION bug.
;;; Written 9/12/84 00:51:36 by RMS,
;;; while running on Lisp Machine Twenty-five from band 5
;;; with Experimental System 99.0, CADR 4.0, Experimental ZMail 54.0, MIT-Specific 23.0, microcode 320, MIT GC@0.



; From file QFILE.LISP OZ:<L.NETWORK.CHAOS> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFMETHOD (QFILE-ACCESS :REMOTE-CONNECT) (&REST ARGS)
  (DECLARE (ARGLIST FILE ERROR ACCESS-MODE &OPTIONAL UNIT)) 
  (APPLY #'CWD-CHAOS SELF ARGS))

))

; From file OPEN.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN COPY-FILE (PATHNAME-OR-STREAM NEW-NAME
		  &REST OPTIONS
		  &KEY (ERROR T)
		  &ALLOW-OTHER-KEYS)
  "Copy a file, specified as a pathname, string or I//O stream.
CHARACTERS can be T, NIL, meaning the same as in OPEN.
 or it can be :ASK, meaning always ask the user,
 or :MAYBE-ASK meaning ask the user unless the answer is clear,
 or :DEFAULT meaning guess as well as possible but never ask.
Specify BYTE-SIZE to force copying in a certain byte size.
 BYTE-SIZE affects only binary mode copying.
REPORT-STREAM is a stream to output messages to as files are copied.
 If it is NIL, no messages are output.
COPY-CREATION-DATE if NIL means don't copy the file creation date;
 make now be the new file's creation date.
COPY-AUTHOR if NIL means don't copy the author; make you the new file's author.
CREATE-DIRECTORIES says whether to create a directory to be copied into.
 Values are T, NIL and :QUERY (meaning ask the user if the situation comes up).
Values returned:
1) the first value is normally the defaulted pathname to copy to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
4) the fourth value is a mode of copying, or a list of such.
 A mode of copying is a type specifier such as STRING-CHAR or (UNSIGNED-BYTE 8).
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (ARGLIST PATHNAME-OR-STREAM NEW-NAME
		    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
		    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
		    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT))
	   (VALUES TARGET-PATHNAME TARGET-TRUENAME RESULT-PATHNAME COPY-MODE))
  (FORCE-USER-TO-LOGIN)
  (LET ((RESULT
	  (IF (OR (STRINGP PATHNAME-OR-STREAM)
		  (TYPEP PATHNAME-OR-STREAM 'PATHNAME))	;Not a stream
	      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
					  (PATHNAME-OR-STREAM FILE-ERROR)
		(LET ((MERGED-PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME-OR-STREAM)))
		  (APPLY MERGED-PATHNAME
			 ':WILDCARD-MAP #'PRIMITIVE-COPY-FILE
			 ':MAYBE NIL
			 MERGED-PATHNAME (PARSE-PATHNAME NEW-NAME NIL MERGED-PATHNAME) OPTIONS)))
	    (LET ((TRUENAME (SEND PATHNAME-OR-STREAM ':TRUENAME)))
	      (LIST (APPLY 'PRIMITIVE-COPY-FILE
			   (FILE-PROPERTIES TRUENAME)
			   TRUENAME (PARSE-PATHNAME NEW-NAME NIL TRUENAME) OPTIONS))))))
    (IF (EQ (CAAR RESULT) (CADAR RESULT))
	(VALUES (THIRD (CAR RESULT))
		(FOURTH (CAR RESULT))
		(FIFTH (CAR RESULT))
		(SIXTH (CAR RESULT)))
      (VALUES (MAPCAR 'THIRD RESULT)
	      (MAPCAR 'FOURTH RESULT)
	      (MAPCAR 'FIFTH RESULT)
	      (MAPCAR 'SIXTH RESULT)))))

))

; From file OPEN.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN RENAMEF (STRING-OR-STREAM NEW-NAME &OPTIONAL (ERROR-P T) QUERY?)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY?, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR-P is NIL."
  (DECLARE (VALUES OLD-NAME OLD-TRUENAME NEW-TRUENAME))
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
				       (TYPEP STRING-OR-STREAM 'PATHNAME))
				   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
			      (STRING-OR-STREAM FILE-ERROR)
    (LET* ((FROM-PATHNAME (PATHNAME STRING-OR-STREAM))
	   (RESULT (WILDCARDED-FILE-OPERATION
		     STRING-OR-STREAM
		     #'PRIMITIVE-RENAME-FILE
		     NIL
		     (PARSE-PATHNAME NEW-NAME NIL FROM-PATHNAME) ERROR-P
		     (MAKE-FILE-QUERY-FUNCTION QUERY?))))
      (IF (EQ (CAAR RESULT) (CADAR RESULT))
	  (VALUES (THIRD (CAR RESULT))
		  (FOURTH (CAR RESULT))
		  (FIFTH (CAR RESULT)))
	(VALUES (MAPCAR 'THIRD RESULT)
		(MAPCAR 'FOURTH RESULT)
		(MAPCAR 'FIFTH RESULT))))))

))

; From file PATHNM.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(DEFMETHOD (HOST-PATHNAME :DELETE-MULTIPLE-FILES) (ERROR-P FILES)
  (SEND HOST :ACCESS-OPERATION :DELETE-MULTIPLE-FILES ERROR-P FILES))

))

; From file PATHNM.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

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
		(LET ((WDIR (OR (CADR (ASSQ HOST HOST-WORKING-DIRECTORY-ALIST))
				(USER-HOMEDIR HOST))))
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
		   (UNLESS NEW-TYPE
		     (SETQ NEW-TYPE
			   (OR (AND (NOT (NULL SECONDARY-DEFAULT))
				    (PATHNAME-TYPE SECONDARY-DEFAULT))
			       ;; Never let the type of the resulting pathname be NIL.
			       DEFAULT-TYPE)))
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

; From file ACCESS.LISP OZ:<L.IO.FILE> OZ:
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFFLAVOR BASIC-ACCESS
	   (HOST)
	   ()
  (:INITABLE-INSTANCE-VARIABLES HOST)
  (:GETTABLE-INSTANCE-VARIABLES HOST)
  (:REQUIRED-METHODS
    :RESET		; () Close all streams, reset host units
    :OPEN-STREAMS	; () List of open streams
    :CLOSE-ALL-FILES	; ()
    :OPEN 		; (FILE PATHNAME &REST OPTIONS) The PATHNAME argument is for the
			; the PATHNAME which was originally requested; the usual ``different''
			; thing for this to be is a logical pathname.
			; FILE seems to be ignored, but it's being kept in for now
    :RENAME		; (FILE NEW-PATHNAME ERROR-P)
    :DELETE		; (FILE ERROR-P)
    :COMPLETE-STRING	; (FILE STRING OPTIONS) FILE is mostly for defaulting
    :CHANGE-PROPERTIES	; (FILE ERROR-P &REST PROPERTIES)
;Is :HOMEDIR really used??
    :HOMEDIR		; (USER) Ignored most of the time
    :CREATE-LINK	; (FILE LINK-TO ERROR)
    :EXPUNGE 		; (FILE ERROR)
    :REMOTE-CONNECT	; (FILE ERROR ACCESS-MODE &OPTIONAL UNIT) Connect to the directory
			; FILE.  If ACCESS-MODE is T, then do TOPS-20 access.  If UNIT
			; is given, connect for just that unit.  (This argument should be
			; be ignored if it does not make sense the access object.)
    :CREATE-DIRECTORY	; (FILE ERROR)
    :DIRECTORY-LIST	; (FILE OPTIONS)
    :DIRECTORY-LIST-STREAM ; (FILE OPTIONS)
;Required only by DIRECTORY-STREAM-ACCESS-MIXIN
;   :DIRECTORY-STREAM	; (FILE OPTIONS)
    :ACCESS-DESCRIPTION	; () Returns a string describing the access method (for :PRINT-SELF)
    ))

))

; From file ACCESS.LISP OZ:<L.IO.FILE> OZ:
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFMETHOD (BASIC-ACCESS :DELETE-MULTIPLE-FILES) (ERROR-P FILES)
  (MAPCAR 'FUNCALL FILES (CIRCULAR-LIST :DELETE) ERROR-P))

))

(si:make-area-dynamic si:fasl-temp-area)
(si:make-area-dynamic si:fasl-table-area)

; From file CHSNCP.LISP OZ:<L.NETWORK.CHAOS> OZ:
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSNCP  "

(DEFUN RETRANSMISSION (CONN &AUX TIME (INHIBIT-SCHEDULING-FLAG T))
  (COND ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
	 ;Only if it is open or awaiting a response from RFC
         (DO-NAMED CONN-DONE
		   () (NIL)
	   ;; Doing this outside the loop can lose
	   ;; because then TIME can be less than (PKT-TIME-TRANSMITTED PKT).
	   (SETQ TIME (TIME))
	   (LET ((INHIBIT-SCHEDULING-FLAG T))
             (DO ((PKT (SEND-PKTS CONN) (PKT-LINK PKT)))
                 ((NULL PKT) (RETURN-FROM CONN-DONE NIL))
	       (COND ((NOT (EQ CONN (PKT-SOURCE-CONN PKT)))
		      (FERROR NIL "~S in SEND-PKTS list for incorrect CONN:
CONN ~S, (PKT-SOURCE-CONN PKT) ~S." PKT CONN (PKT-SOURCE-CONN PKT))))
	       (SETQ MORE-RETRANSMISSION-NEEDED T)
	       (COND (( (TIME-DIFFERENCE TIME (PKT-TIME-TRANSMITTED PKT))
			 (LSH (CONN-RETRANSMISSION-INTERVAL CONN)
			      ;; Retransmit the lowest numbered packet most often
			      (MAX 0 (MIN 5 (1- (%POINTER-DIFFERENCE (PKT-NUM PKT)
								     (SEND-PKT-ACKED CONN)))))))
		      (SETF (PKT-BEING-RETRANSMITTED PKT) T)
		      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		      (TRANSMIT-PKT PKT T)
		      (INCF PKTS-RETRANSMITTED)
		      (SETQ INHIBIT-SCHEDULING-FLAG T)
		      (COND ((EQ (PKT-BEING-RETRANSMITTED PKT) 'FREE)
			     (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)
			     (FREE-PKT PKT))
			    (T (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)))
		      (RETURN NIL)))))  ;Must always start from beginning of chain if
					; turned on scheduling, since chain could be invalid
           (PROCESS-ALLOW-SCHEDULE)))))

))

; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN GET-NEW-SYSTEM-VERSION (&OPTIONAL (MAXIMUM-LENGTH 16.) &KEY INCREMENTAL)
  (FORMAT T "~&This is now:")
  (DESCRIBE-SYSTEM-VERSIONS)
  (FRESH-LINE)
  (SETQ SYSTEM-ADDITIONAL-INFO
	(READLINE-TRIM *QUERY-IO* ""
		       `((:PROMPT "Additional comment for herald: ")
			 (:INITIAL-INPUT ,system-additional-info)
			 (:INITIAL-INPUT-POINTER ,(LENGTH SYSTEM-ADDITIONAL-INFO)))))
  (LET ((VERS (SYSTEM-VERSION-INFO T)))
    (IF INCREMENTAL
	(SETQ VERS (STRING-APPEND "Inc " VERS)))
    ;; If short version doesn't fit, allow user to edit it (e.g. abbreviate system names)
    (DO (SHORT)
	(( (LENGTH VERS) MAXIMUM-LENGTH))
      (SETQ SHORT (SUBSTRING VERS 0 MAXIMUM-LENGTH))
      (SETQ VERS
	    (READLINE-TRIM *QUERY-IO* ""
			   `((:PROMPT ,(FORMAT NIL "~S will not fit in disk label.~@
						    Please abbreviate to ~D character~:P: "
					       VERS MAXIMUM-LENGTH))
			     (:INITIAL-INPUT ,VERS)
			     (:INITIAL-INPUT-POINTER ,MAXIMUM-LENGTH)))))
    VERS))

))
