;;; -*- Mode:LISP; Package:FILE-SYSTEM; Patch-File:T; Base:10; Readtable:T -*-
;;; Patch file for System version 99.13
;;; Reason:
;;;  Improve chaos-simple polling
;;;  qfile rename when server doesn't return all the truenames
;;;  Better pathname parsing errors
;;;  In fs:parse-pathname, if second arg (host) is non-nil, then pathname is
;;;   ALWAYS parsed wrt that host, even if it contains leading "foo:"
;;;  Some random pathname parsing bugs
;;;  fix compilation of (cond)
;;;  compiler p1 handlers ay returns self-evaluating-p frobs
;;;  fs:merge-pathname-conponents extended args
;;;   (old name fs:merge-pathnames-1 flushed)
;;;  -*- Syntax:Common-Lisp -*- for NIL compatibility (same as Readtable:)
;;;  copy-seq on vector always returns a simple-vector
;;;  :set-primary-device for vms hosts
;;;  Zwei problems with changed format mouse blips
;;;  fs:close-all-files optional arg for rg
;;;  hack qfile enable/disable-capabilities returns, and record current
;;;   capabilities correctly
;;;  parsing of arg to fs:expunge-directory
;;;   (fs:merge-pathname-defaults bites the big one)
;;;  Better restoring of qfile user state on relogin
;;;  rename-within encapsulation when within function spec wasn't a symbol
;;; Written 18-Nov-84 06:33:52 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Experimental System 99.11, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, microcode 320, GC@2.



(eval-when (eval compile load)
  (export (intern "MERGE-PATHNAME-COMPONENTS" 'fs) 'fs)
  (unexport 'fs::merge-pathnames-1 'fs)
  )
(undefun 'fs::merge-pathnames-1)

; From file CHUSE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (12)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHUSE  "

(DEFUN ADDRESS-PARSE (ADDRESS &AUX HOST)
  "Coerce the argument to a chaosnet address.
The argument can be a host name or host object, or an address."
  (DECLARE (VALUES ADDRESS HOST-OBJECT))
  (CONDITION-CASE (ERROR)
      (LET ((ADDRESS (COND ((INTEGERP ADDRESS)
			    ADDRESS)
			   ((AND (TYPEP ADDRESS 'INSTANCE)
				 (SEND (SETQ HOST ADDRESS) :SEND-IF-HANDLES :CHAOS-ADDRESS)))
			   ((AND (SETQ HOST (SI:PARSE-HOST ADDRESS T))
				 (SEND HOST :CHAOS-ADDRESS)))
			   ((AND (STRINGP ADDRESS)
				 (PARSE-NUMBER ADDRESS 0 NIL 8))))))
	(IF ADDRESS (VALUES ADDRESS (OR HOST (SI:GET-HOST-FROM-ADDRESS ADDRESS :CHAOS)))))
    (SYS:UNCLAIMED-MESSAGE NIL)))

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN POLL-HOSTS (HOSTS CONTACT-NAME STREAM HEADER-FUNCTION FORMAT-FUNCTION
		   &KEY IGNORE-STATES (WINDOW-SIZE 1) (TIMEOUT 600.) (WHOSTATE "Poll Hosts")
		   &AUX CONNECTIONS (OPEN-CONNECTIONS 0))
  "Print the status of chaosnet hosts in HOSTS, or all known chaosnet hosts.
STREAM is where all of the information is printed.  
HOSTS is a list of hosts to report about.  If NIL, then all chaonset hosts are used.
HEADER-FUNCTION is a function called with one arg, STREAM, to print out the intial header.
FORMAT-FUNCTION is called on STREAM chaos address and response packet for successful
 connections.
WINDOW-SIZE defaults to 1 and the TIMEOUT defaults to 10 seconds (600.)
IGNORE-STATES is either NIL or a list of states for which nothing is printed if the
 connection goes into that state.
 The states can be any of CHAOS:RFC-SENT-STATE CHAOS:ANSWERED-STATE CHAOS:CLS-RECEIVED-STATE
 CHAOS:OPEN-STATE CHAOS:LOS-RECEIVED-STATE or OTHERWISE (any other state)"
  (UNWIND-PROTECT
      (PROGN
	(SETQ CONNECTIONS (IF HOSTS (LOOP FOR HOST IN HOSTS
					  COLLECT (IF (NUMBERP HOST)
						      (LIST NIL HOST NIL)
						    (MULTIPLE-VALUE-BIND (ADDRESS HOST)
							(ADDRESS-PARSE HOST)
						      (LIST HOST ADDRESS NIL))))
			            (CREATE-HOSTAT-CONNECTION-LIST HOSTS)))
	(FUNCALL HEADER-FUNCTION STREAM)
	(DO () ((NULL CONNECTIONS))		;loop until there are no more
	  ;; Handle any replies that have come in.
	  ;; Note host-name truncated to 27. characters to make more room for statistics
	  ;; Only have up to 20. outstanding connections at a time
	  (LOOP FOR ELEM IN CONNECTIONS
		WHILE (< OPEN-CONNECTIONS 20.)
		WITH NEW-CONN
		DO (WHEN (AND (NULL (THIRD ELEM))
			      (SECOND ELEM)
			      (SETQ NEW-CONN (CONDITION-CASE ()
						 (OPEN-CONNECTION (CADR ELEM)
								  CONTACT-NAME WINDOW-SIZE)
					       (SYS:NETWORK-RESOURCES-EXHAUSTED NIL))))
		     (INCF OPEN-CONNECTIONS)
		     (SETF (THIRD ELEM) NEW-CONN)))
	  ;; tell user that something is happening...
	  (PROCESS-WAIT-WITH-TIMEOUT WHOSTATE 120. #'(LAMBDA ()
						       (DOLIST (ELEM CONNECTIONS)
							 (WHEN (AND (THIRD ELEM)
								    (NEQ (STATE (THIRD ELEM))
									 'RFC-SENT-STATE))
							   (RETURN T)))))
	  (DOLIST (ELEM CONNECTIONS)
	    (LET ((HOST (CAR ELEM))
		  (ADDRESS (CADR ELEM))
		  (CONN (CADDR ELEM))
		  (PUNT 'CONN)
		  (PKT NIL))
	      (WHEN CONN
		(CASE (STATE CONN)
		  (RFC-SENT-STATE
		    (IF (< (TIME-DIFFERENCE (TIME) (TIME-LAST-RECEIVED CONN))
			   TIMEOUT)
			(SETQ PUNT NIL)
		      (UNLESS (MEMQ 'RFC-SENT-STATE IGNORE-STATES)
			(FORMAT STREAM "~O~7T~@[~A   ~]Host not responding~%" ADDRESS HOST))))
		  (ANSWERED-STATE
		    (UNWIND-PROTECT
			(PROGN (SETQ PKT (GET-NEXT-PKT CONN))
			       (UNLESS (MEMQ 'ANSWERED-STATE IGNORE-STATES)
				 (FUNCALL FORMAT-FUNCTION STREAM ADDRESS PKT)))
		      (AND PKT (RETURN-PKT PKT)))
		    ;; Delete not only this connection, but every one to this same host, in
		    ;; case it has multiple addresses.  One copy of the answer is enough, but
		    ;; if it fails we would like to see all paths.
		    (SETQ PUNT 'HOST))
		  (CLS-RECEIVED-STATE
		    (UNWIND-PROTECT
			(PROGN (SETQ PKT (GET-NEXT-PKT CONN))
			       (UNLESS (MEMQ 'CLS-RECEIVED-STATE IGNORE-STATES)
				 (FORMAT STREAM "~O~7T~@[~A   ~]returned a CLS:~A~%"
					 ADDRESS HOST (PKT-STRING PKT))))
		      (AND PKT (RETURN-PKT PKT))))
		  (OPEN-STATE
		    (UNLESS (MEMQ 'OPEN-STATE IGNORE-STATES)
		      (FORMAT STREAM "~#oO~7T~@[~A   ~]returned an OPN~%" ADDRESS HOST)))
		  (LOS-RECEIVED-STATE
		    (SETQ PKT (READ-PKTS-LAST CONN))
		    (UNLESS (MEMQ 'LOS-RECEIVED-STATE IGNORE-STATES)
		      (FORMAT STREAM "~O~7T~@[~A   ~]returned a LOS:~A~%"
			      ADDRESS HOST (PKT-STRING PKT))))
		  (OTHERWISE
		    (UNLESS (MEMQ 'OTHERWISE IGNORE-STATES)
		      (FORMAT STREAM "~O~7T~@[~A   ~]connection entered bad state: ~A~%"
			      ADDRESS HOST (STATE CONN)))))
		(CASE PUNT
		  (CONN (REMOVE-CONN CONN)
			(SETQ CONNECTIONS (DELQ ELEM CONNECTIONS))
			(DECF OPEN-CONNECTIONS))
		  (HOST (WHEN HOST
			  (DOLIST (C CONNECTIONS)
			    (WHEN (EQ (CAR C) HOST)
			      (WHEN (THIRD C) (REMOVE-CONN (THIRD C)))
			      (SETQ CONNECTIONS (DELQ C CONNECTIONS))
			      (DECF OPEN-CONNECTIONS)))))))))))
    ;; Remove host objects with no connections attached to them
    ;; Unwind-protect cleanup -- Flush any connections that remain
    (LOOP FOR (HOST ADDRESS CONN) IN CONNECTIONS
	  WHEN CONN DO (CLOSE-CONN CONN)))) 

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN HOSTAT (&REST HOSTS)
  "Prints out information on STREAM on the status of all of the hosts specified by HOSTS."
  (POLL-HOSTS HOSTS "STATUS" *STANDARD-OUTPUT*
	      (IF (CDR HOSTS)
		  #'HOSTAT-HEADING
		  #'(LAMBDA (STREAM) (HOSTAT-HEADING STREAM NIL)))
	      #'HOSTAT-FORMAT-ANS
	      :WHOSTATE "Hostat Reply"))

(DEFUN HOSTAT-HEADING (STREAM &OPTIONAL (VERBOSE T))
  (FORMAT STREAM "~&Chaosnet host status report.  ~:[~;Type Control-Abort to quit.~]" VERBOSE)
  (FORMAT STREAM "~%~7A~25A" "Site" "Name//Status")
  (DO ((HEADS '("Subnet" "#-in" "#-out" "abort" "lost" "crc" "ram" "bitc" "other")
	      (CDR HEADS))
       (WIDTHS '(6 9 9 8 8 8 4 5 6) (CDR WIDTHS)))
      ((NULL HEADS) (WRITE-CHAR #/NEWLINE STREAM))
    (FORMAT STREAM "~V@A" (CAR WIDTHS) (CAR HEADS))))

(DEFUN HOSTAT-FORMAT-ANS (STREAM HOST PKT &AUX (NBYTES (PKT-NBYTES PKT)))
  (FORMAT STREAM "~7@<~O ~>~27A"		;Print host number and name as returned
	  HOST
	  (NSUBSTRING (PKT-STRING PKT) 0
		      (MIN NBYTES 27. (OR (STRING-SEARCH-CHAR 0 (PKT-STRING PKT) 0 32.)
					  ;; This line is temporary! *******
					  (STRING-SEARCH-CHAR #o200 (PKT-STRING PKT) 0 32.)
					  32.))))
  (HOSTAT-FORMAT-ANS-1 PKT 34. '(4 9 9 8 8 8 4 5 6) STREAM))

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN PRINT-HOST-TIMES (&OPTIONAL (HOSTS TIME-SERVER-HOSTS) (STREAM *STANDARD-OUTPUT*))
  (LET ((BEGIN-TIME (TIME:TIME))
	(TOTAL-TIME 0)
	(RESPONSES 0))
    (POLL-HOSTS HOSTS "TIME" STREAM
		#'(LAMBDA (STREAM) (FORMAT STREAM "~&Host~26TTime"))
		#'(LAMBDA (STREAM ADDRESS PKT)
		    (LET ((TIME (DECODE-CANONICAL-TIME-PACKET PKT)))
		      (FORMAT STREAM "~&~:[~;! ~]~A~22,2T~\TIME\~%"
			      (> (ABS (- (TIME:GET-UNIVERSAL-TIME) TIME)) 180.)
			      (SI:GET-HOST-FROM-ADDRESS ADDRESS :CHAOS)
			      TIME)
		      (INCF TOTAL-TIME TIME)
		      (INCF RESPONSES)))
		:WHOSTATE "Host time")
    (FORMAT STREAM "~2%Average time: ~\TIME\; total time elapsed: ~D seconds.~%"
	    (ROUND TOTAL-TIME RESPONSES) (TRUNCATE (- (TIME:TIME) BEGIN-TIME) 60.))))

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN UPTIME (&OPTIONAL (STREAM *STANDARD-OUTPUT*) &REST HOSTS)
  "Print onto STREAM a listing of the uptimes of HOSTS, or all chaosnet hosts if HOSTS is ()."
  (POLL-HOSTS HOSTS "UPTIME" STREAM
	      #'(LAMBDA (STREAM)
		  (FORMAT STREAM "~%~8A~25A~25A" "Address" "Host name" "Uptime"))
	      #'(LAMBDA (STREAM HOST PKT)
		  (FORMAT STREAM "~&~8@<~O ~>~25A~A~%" HOST (CHAOS:HOST-DATA HOST)
			  (TIME:PRINT-INTERVAL-OR-NEVER
			    (// (DECODE-CANONICAL-TIME-PACKET PKT) 60.) NIL)))
	      :IGNORE-STATES '(CLS-RECEIVED-STATE)
	      :WHOSTATE "Uptime reply" ))

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (357)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFUN RENAME-CHAOS (ACCESS OLD-PATHNAME NEW-PATHNAME ERROR &AUX PKT SUCCESS STRING)
  (DECLARE (VALUES TRUENAME OLD-TRUENAME))
  (FILE-OPERATION-RETRY
    (let ((HOST-UNIT (SEND ACCESS :GET-HOST-UNIT)))
      (UNWIND-PROTECT
	  (PROGN (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
		   (SEND HOST-UNIT :COMMAND NIL NIL NIL "Rename" "RENAME" #/NEWLINE
			 (FILE-PRINT-PATHNAME OLD-PATHNAME) #/NEWLINE
			 (FILE-PRINT-PATHNAME NEW-PATHNAME) #/NEWLINE))
		 (IF SUCCESS
		     ;; If there is a second line coming from the file server,
		     ;; it is the new truename.
		     (let* ((from (string-search-char #/newline string)) truename)
		       (if (null from) (values new-pathname old-pathname)
			 (let* ((old (string-search-char #/newline string from))
				(host (send old-pathname :host)))
			   (setq truename (parse-pathname string host nil (1+ from) old))
			   (if (null old) (values truename old-pathname)
			     (values truename
				     (fs:parse-pathname string host nil (1+ old)))))))
		   (QFILE-PROCESS-ERROR-NEW STRING OLD-PATHNAME NIL (NOT ERROR) :RENAME)))
	(AND PKT (CHAOS:RETURN-PKT PKT))))))

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (357)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :RENAME) (NEW-NAME &OPTIONAL (ERROR-P T)
					     &AUX SUCCESS STRING)
  (FILE-OPERATION-RETRY
    (CASE STATUS
      ((:OPEN :EOF :SYNC-MARKED :ASYNC-MARKED)
       (MULTIPLE-VALUE (STRING SUCCESS)
	 (SEND SELF :COMMAND NIL "Rename"
				 "RENAME" #/NEWLINE
				  (FILE-PRINT-PATHNAME NEW-NAME) #/NEWLINE))
       (COND (SUCCESS
	      ;; If there is a second line coming from the file server,
	      ;; it is the new truename.
	      (LET* ((FROM (STRING-SEARCH #/NEWLINE STRING)))
		(WHEN FROM
		  (SEND SELF :PUTPROP (FS:PARSE-PATHNAME
					STRING
					(SEND (GET SELF :TRUENAME) :HOST) NIL
					(1+ FROM) (STRING-SEARCH #/NEWLINE STRING (1+ FROM)))
			:TRUENAME)))
	      (SETQ PATHNAME NEW-NAME)
	      (SEND TV::WHO-LINE-FILE-STATE-SHEET :CLOBBERED)
	      T)
	     (T (QFILE-PROCESS-ERROR-NEW STRING SELF NIL (NOT ERROR-P) :RENAME))))
      (OTHERWISE (FERROR NIL "~S in illegal state for rename." SELF)))))

))

; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (533)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(DEFSIGNAL PATHNAME-PARSE-ERROR (PATHNAME-ERROR PATHNAME-PARSE-ERROR)
	   (PARSE-END-INDEX REPORT-STRING REPORT-ARGS)
  "Any error that makes it impossible to parse a string into a pathname.")

(DEFPROP PATHNAME-ERROR T :ERROR-REPORTER)
(DEFUN PATHNAME-ERROR (INDEX LOSING-STRING REPORT-STRING &REST ARGS)
  (IF PARSE-PATHNAME-FLAG
      (THROW 'PARSE-PATHNAME INDEX)
    (FERROR 'PATHNAME-PARSE-ERROR
	    "~?~%~VT~%   /"~A/"~%" 
	    REPORT-STRING ARGS
	    (- INDEX
	       1
	       (OR (STRING-REVERSE-SEARCH-CHAR #/NEWLINE LOSING-STRING INDEX) -4))
	    LOSING-STRING)))

(DEFUN PARSE-NAMESTRING (THING &OPTIONAL WITH-RESPECT-TO
			 		 (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
					 &KEY (START 0) END JUNK-ALLOWED)
  "Parse THING into a pathname and return it.
The same as FS:PARSE-PATHNAME except that that function's args are all positional."
  (PARSE-PATHNAME THING WITH-RESPECT-TO DEFAULTS START END JUNK-ALLOWED))

(DEFUN PARSE-PATHNAME (THING &OPTIONAL WITH-RESPECT-TO (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
		       (START 0) END JUNK-ALLOWED)
  "Parse THING into a pathname and return it.
THING can be a pathname already (it is just passed back),
 a string or symbol, or a Maclisp-style namelist.
WITH-RESPECT-TO can be NIL or a host or host-name;
 if it is not NIL, the pathname is parsed for that host.
If WITH-RESPECT-TO is NIL, then DEFAULTS is used to get the host
 if none is specified.  DEFAULTS may be a host object in this case.
START and END are indices specifying a substring of THING to be parsed.
 They default to 0 for START and NIL (meaning end of THING) for END.
If JUNK-ALLOWED is non-NIL, parsing stops without error if
 the syntax is invalid, and this function returns NIL.
The second value is the index in THING at which parsing stopped.
 If JUNK-ALLOWED is T and there was invalid syntax,
 this is the index of the invalid character."
  (DECLARE (VALUES PARSED-PATHNAME PARSE-END-INDEX))
  (AND WITH-RESPECT-TO
       (SETQ WITH-RESPECT-TO (GET-PATHNAME-HOST WITH-RESPECT-TO)))
  (CONDITION-RESUME '((PATHNAME-ERROR) :NEW-PATHNAME T ("Proceed, supplying a new pathname.")
		      PARSE-PATHNAME-THROW-NEW-PATHNAME)
    (LET ((PARSE-PATHNAME-FLAG JUNK-ALLOWED))
      (CATCH-CONTINUATION 'PARSE-PATHNAME
	  #'(LAMBDA (INDEX-OR-PATHNAME) 
	      (IF (NUMBERP INDEX-OR-PATHNAME)
		  (VALUES NIL (MIN (OR END (STRING-LENGTH THING)) INDEX-OR-PATHNAME))
		(VALUES INDEX-OR-PATHNAME START)))
	  NIL
	(COND ((TYPEP THING 'PATHNAME)
	       (AND WITH-RESPECT-TO (NEQ WITH-RESPECT-TO (PATHNAME-HOST THING))
		    (FERROR 'PATHNAME-PARSE-ERROR
			    "Host ~A in ~A does not match ~A"
			    (PATHNAME-HOST THING) THING WITH-RESPECT-TO))
	       (VALUES THING START))
	      ((CONSP THING)
	       (SETQ THING (CANONICALIZE-KLUDGEY-MACLISP-PATHNAME-STRING-LIST THING))
	       (LET (DEVICE DIRECTORY NAME TYPE VERSION HOST)
		 (COND ((CONSP (CAR THING))
			(SETF `((,DEVICE ,DIRECTORY) ,NAME ,TYPE ,VERSION) THING))
		       ((NUMBERP (THIRD THING))
			(SETF `(,NAME ,TYPE ,VERSION ,DEVICE ,DIRECTORY) THING))
		       (T
			(SETF `(,NAME ,TYPE ,DEVICE ,DIRECTORY ,VERSION) THING)))
		 (SETQ HOST (COND ((GET-PATHNAME-HOST DEVICE T))
				  (WITH-RESPECT-TO)
				  ((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
				  (T (DEFAULT-HOST DEFAULTS))))
		 (AND WITH-RESPECT-TO
		      (NEQ WITH-RESPECT-TO HOST)
		      (FERROR 'PATHNAME-PARSE-ERROR
			      "Host ~A in ~A does not match ~A" HOST THING WITH-RESPECT-TO))
		 (VALUES (MAKE-PATHNAME :HOST HOST
					:DEVICE DEVICE :DIRECTORY DIRECTORY :NAME NAME
					:TYPE TYPE :VERSION VERSION)
			 START)))
	      (T
	       (SETQ THING (STRING THING) END (OR END (LENGTH THING)))
	       (LET ((HOST-SPECIFIED NIL))
		 (OR WITH-RESPECT-TO
		     (MULTIPLE-VALUE-SETQ (HOST-SPECIFIED START END)
		       (PARSE-PATHNAME-FIND-COLON THING START END)))
;		 ;; If the thing before the colon is really a host,
;		 ;; and WITH-RESPECT-TO was specified, then they had better match
;		 (AND WITH-RESPECT-TO
;		      HOST-SPECIFIED
;		      (NEQ WITH-RESPECT-TO HOST-SPECIFIED)
;		      ;; Otherwise treat it as a device name
;		      (SETQ HOST-SPECIFIED NIL START 0 END NIL))
		 (LET ((HOST (COND (WITH-RESPECT-TO)
				   ((AND HOST-SPECIFIED (GET-PATHNAME-HOST HOST-SPECIFIED T)))
				   ((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
				   (T (DEFAULT-HOST DEFAULTS)))))
		   (MULTIPLE-VALUE-BIND (DEVICE DIRECTORY NAME TYPE VERSION PARSE-END)
		       (SEND (SAMPLE-PATHNAME HOST) :PARSE-NAMESTRING
						    (NOT (NULL HOST-SPECIFIED))
						    THING START END)
		     (VALUES
		       ;; If device is :NO-INTERN then immeditely return 2nd value, DIRECTORY.
		       ;; this provides a way to bypass as much of this lossage as possible
		       ;; in cases where it doesn't make sense.
		       (COND ((EQ DEVICE :NO-INTERN)
			      DIRECTORY)
			     (T
			      ;; Otherwise we assume we got the raw forms of everything.
			      (MAKE-PATHNAME-INTERNAL
				HOST DEVICE DIRECTORY NAME TYPE VERSION)))
		       PARSE-END))))))))))

))

; From file PATHNM.LISP KANSAS:<L.FILE2> OZ: (162)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

(defun expand-pathstring (pathstring &optional (name-start-index 0))
  (cond ((or (consp pathstring) (null pathstring)) pathstring)
	(t
	 (setq pathstring (string pathstring))
	 (do (pathlist
	       previous-step
	       (len (string-length pathstring))
	       nextname nextversion propflag
	       after-root-or-supernode
	       name-end-index last-nonspace)
	     (( name-start-index len)
	      pathlist)
	   (setq nextname nil nextversion nil)
	   (cond ((= (aref pathstring name-start-index) #/^)
		  (setq previous-step 'supernode)
		  (setq pathlist (nconc pathlist (list previous-step)))
		  (setq after-root-or-supernode "^")
		  (setq name-start-index (1+ name-start-index)))
		 ((= (aref pathstring name-start-index) #/~)
		  (setq previous-step 'root)
		  (setq pathlist (nconc pathlist (list previous-step)))
		  (setq after-root-or-supernode "~")
		  (setq name-start-index (1+ name-start-index)))
		 ((and (memq (aref pathstring name-start-index) '(#// #/\))
		       after-root-or-supernode)
		  (setq name-start-index (1+ name-start-index))
		  (setq after-root-or-supernode nil))
		 ((and (= (aref pathstring name-start-index) #/|)
		       after-root-or-supernode)
		  (setq after-root-or-supernode nil))
		 ((= (aref pathstring name-start-index) #/ )
		  (setq name-start-index (1+ name-start-index)))
		 ((memq (aref pathstring name-start-index) '(#/ #/ #/; #/:))
		  (return pathlist name-start-index))
		 (t
		   (and after-root-or-supernode
			(ferror 'pathname-parse-error
				"garbage following ~A in pathstring"
				after-root-or-supernode))
		   ;; Read a name of subnode or property.
		   (setq propflag nil)
		   (cond ((= (aref pathstring name-start-index) #/|)
			  (setq name-start-index (1+ name-start-index))
			  (setq propflag t)))
		   (setq last-nonspace (1- name-start-index))
		   ;; Find end of name.  Skip over quoted characters.
		   (do ((i name-start-index (1+ i)))
		       (( i len)
			(setq name-end-index len))
		     (let ((ch (aref pathstring i)))
		       (cond ((= ch #/)
			      (setq i (1+ i))
			      (setq last-nonspace i)
			      (cond ((= i len)
				     (pathname-error (1- i) pathstring
					     "Pathstring ends with quote character ~C"
					     #/))))
			     ((memq ch pathstring-special-chars)
			      (return (setq name-end-index i)))
			     ((= ch #/ ))
			     (t (setq last-nonspace i)))))
		   ;; Extract the name.
		   (setq nextname
			 (string-left-trim " "
					   (substring pathstring
						      name-start-index (1+ last-nonspace))))
		   (setq name-start-index name-end-index)
		   (setq nextname (parse-name-and-version nextname
							  (cond (propflag :property)
								(t :version))))
		   ;; Don't keep a list starting with :version
		   ;; if there isn't really a version number.
		   (and (not propflag) (null (caddr nextname))
			(setq nextname (cadr nextname)))
		   ;; Skip over any "/" or "\" separating it from the following name.
		   (and (< name-start-index len)
			(or (= (aref pathstring name-start-index) #//)
			    (= (aref pathstring name-start-index) #/\))
			(setq name-start-index (1+ name-start-index)))
		   ;; Add the new step onto the pathlist.
		   (if (and propflag
			    (not (symbolp previous-step))
			    (not (and (consp previous-step)
				      (eq (car previous-step) :property))))
		       (rplaca (last pathlist)
			       (setq previous-step
				     `(:subnode-property ,(car (last pathlist))
							 . ,(cdr nextname))))
		       (setq pathlist
			     (nconc pathlist (list (setq previous-step nextname)))))))))))

))

; From file PATHST.LISP KANSAS:<L.IO.FILE> OZ: (179)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHST  "

(DEFMETHOD (LOGICAL-PATHNAME :PARSE-NAMESTRING) (IGNORE NAMESTRING &OPTIONAL (START 0) END)
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (DO ((I START)
       (J START (1+ J))
       CH TEM Q
       DIR NAM NAMP TYP TYPP VERS)
      ((> J END)
       (SETQ DIR (NREVERSE DIR))
       (VALUES :UNSPECIFIC DIR NAM TYP VERS))
    (SETQ CH (IF (= J END) #/SP (AREF NAMESTRING J)))
    (COND ((= CH '#/)
	   (SETQ J (1+ J)))
	  ((MEMQ CH '(#/; #/: #/ #/ #/SP #/TAB #/.))
	   (COND ((OR ( I J) (= CH #/) (= CH #/))
		  (AND (MEM #'= CH '(#/ #/))
		       (OR ( I J)
			   (AND ( (1+ J) END)
				( (AREF NAMESTRING (1+ J)) #/SP)))
		       (PATHNAME-ERROR (1+ J) NAMESTRING
				       "An unquoted ~C must be a component unto itself." CH))
		  (MULTIPLE-VALUE (TEM Q)
		    (SELECTQ CH
		      (#/ (VALUES :UNSPECIFIC NIL))
		      (#/ (VALUES NIL NIL))
		      (T (UNQUOTE-LOGICAL-STRING NAMESTRING I J))))
		  (IF (AND (NOT Q) (STRING= TEM "*"))
		      (SETQ TEM :WILD))
		  (SELECTQ CH
		    (#/: NIL)			;Ignore "devices"
		    (#/; (PUSH TEM DIR))
		    (OTHERWISE
		     (COND (VERS)
			   (TYPP (SETQ VERS (COND ((MEMQ TEM '(:UNSPECIFIC :WILD)) TEM)
						  ((AND (NOT Q)
							(COND ((STRING= TEM ">") :NEWEST)
							      ((STRING= TEM "<") :OLDEST)
							      ((NUMERIC-P TEM)))))
						  (T (PATHNAME-ERROR J NAMESTRING
						       "Version not numeric")))))
			   (NAMP (SETQ TYP TEM TYPP T))
			   (T (SETQ NAM TEM NAMP T)))))))
	   (SETQ I (1+ J))))))

))

; From file PATHST.LISP KANSAS:<L.IO.FILE> OZ: (179)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHST  "

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-NAMESTRING)
	   (HOST-SPECIFIED-P NAMESTRING &OPTIONAL (START 0) END
	    &AUX (WILD-STRINGS '(#.(STRING #/BREAK))))
  (DECLARE (VALUES DEVICE DIRECTORY NAME TYPE VERSION
		   DEVICE-SPECIFIED-P DIRECTORY-SPECIFIED-P NAME-SPECIFIED-P TYPE-SPECIFIED-P
		   VERSION-SPECIFIED-P))
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (LET* ((DIR-DELIM-ALIST (SEND SELF :DIRECTORY-DELIMITERS))
	 (ALL-DELIMS (NCONC (MAPCAR #'CAR DIR-DELIM-ALIST) '(#/: #/. #/; #/SP))))
    (DO ((IDX (OR (STRING-SEARCH-NOT-CHAR #/SP NAMESTRING START END) END))
	 (TEM) (TEM1) (DELIM)
	 (DIR-DELIM)
	 (DEV)
	 (DIR) (NAM) (TYP) (VER)
	 DEV-SPECIFIED-P NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P)
	(( IDX END)
	 (IF (EQUAL TYP "") (SETQ TYP :UNSPECIFIC))
	 (IF (EQUAL NAM "") (SETQ NAM NIL))
	 (SETQ DEV (OR DEV (IF HOST-SPECIFIED-P (SEND SELF :PRIMARY-DEVICE))))
	 (VALUES DEV DIR NAM TYP VER
		 DEV-SPECIFIED-P DIR NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P))
      (COND ((SETQ DIR-DELIM (CDR (ASSQ (AREF NAMESTRING IDX) DIR-DELIM-ALIST)))
	     (AND DIR
		  (PATHNAME-ERROR IDX NAMESTRING "Directory occurs twice"))
	     (INCF IDX)
	     (DO-FOREVER
	       (MULTIPLE-VALUE (TEM IDX DELIM)
		 (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING
						  (LIST #/. DIR-DELIM) IDX END NIL T))
	       (IF (SYS:MEMBER-EQUAL TEM WILD-STRINGS) (SETQ TEM :WILD))
	       (SETQ DIR (IF (AND (= DELIM DIR-DELIM) (NULL DIR))
			     (LIST TEM)
			   (NCONC DIR (NCONS TEM))))
	       (AND (= DELIM DIR-DELIM) (RETURN))))
	    (T
	     (MULTIPLE-VALUE (TEM IDX DELIM)
	       (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING ALL-DELIMS IDX END T T))
	     (COND ((ASSQ DELIM DIR-DELIM-ALIST)
		    (SETQ IDX (1- IDX)))
		   ((AND (= DELIM #/;) VER)	;Protect against twenex attribute usage
		    (SETQ IDX END)))
	     (IF (SYS:MEMBER-EQUAL TEM WILD-STRINGS) (SETQ TEM :WILD))
	     (COND ((= DELIM #/:)
		    (AND DEV
			 (PATHNAME-ERROR IDX NAMESTRING "Device occurs twice" NAMESTRING))
		    (SETQ DEV TEM DEV-SPECIFIED-P (1- IDX)))
		   ((= DELIM #/;)
		    (COND ((NULL NAM-SPECIFIED-P)
			   (SETQ NAM TEM TYP ""
				 NAM-SPECIFIED-P (1- IDX) TYP-SPECIFIED-P (1- IDX)))
			  ((NULL TYP-SPECIFIED-P)
			   (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX)))))
		   ((NULL NAM-SPECIFIED-P)
		    (SETQ NAM TEM NAM-SPECIFIED-P (1- IDX))
		    (IF (= DELIM #/.) (SETQ TYP :UNSPECIFIC)))
		   ((NULL TYP-SPECIFIED-P)
		    (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX))
		    (IF (EQ DELIM #/.) (SETQ VER :UNSPECIFIC)))
		   ((NULL VER-SPECIFIED-P)
		    (SETQ VER-SPECIFIED-P (1- IDX))
		    (COND ((NULL TEM)
			   (SETQ VER NIL))
			  ((EQUAL TEM "")
			   (SETQ VER :UNSPECIFIC))
			  ((SETQ TEM1 (NUMERIC-P TEM))
			   (SETQ VER TEM1))
			  ((EQ TEM :WILD)
			   (SETQ VER :WILD))
			  ((SEND SELF :OLDEST-CHECK TEM)
			   (SETQ VER :OLDEST))
			  (T (PATHNAME-ERROR IDX NAMESTRING "Version must be numeric"))))))))))

))

; From file PATHST.LISP KANSAS:<L.IO.FILE> OZ: (179)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHST  "

(DEFUN TENEX-FAMILY-STRING-UNTIL-DELIM (STRING DELIMS &OPTIONAL (START 0) END EOS-OK QUOTE-P
					&AUX IDX DELIM (NCH 0) (NEED-COPY NIL))
  (DECLARE (VALUES SUBSTRING END DELIM))
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (DO ((I START (1+ I))
       (CHAR))
      (( I END)
       (OR EOS-OK (PATHNAME-ERROR I STRING "Illegal end of string"))
       (SETQ IDX END DELIM -1))
    (SETQ CHAR (AREF STRING I))
    (COND (( #/A CHAR #/Z))
	  ((AND QUOTE-P (= CHAR #/))
	   ;; TOPS-20 quoting character
	   (AND ( (SETQ I (1+ I)) END)
		(PATHNAME-ERROR I STRING "End of string after quoting character ~C" #/))
	   (SETQ NEED-COPY T
		 NCH (1+ NCH)))
	  ((MEMQ CHAR DELIMS)
	   (SETQ IDX I DELIM CHAR)
	   (RETURN))
	  (QUOTE-P
	   (IF (MEMQ CHAR '(#/* #/%))
	       (SETQ NEED-COPY T))
	   (AND ( CHAR #/a) ( CHAR #/z)
		(SETQ NEED-COPY T)))))
  ;; NCH is number of characters that we will discard.
  ;; NEED-COPY is T if either we will discard some chars or we must upcase some.
  (VALUES (COND ((AND QUOTE-P (= IDX (1+ START))
		      (STRING-EQUAL STRING "" START 0 IDX 1))
		 NIL)
		((AND (= START 0) (= IDX (STRING-LENGTH STRING)) (NOT NEED-COPY))
		 STRING)			;Avoid consing
		((NOT NEED-COPY)
		 (SUBSTRING STRING START IDX))
		(T
		 (DO ((SUBSTRING (MAKE-ARRAY (- IDX START NCH) :TYPE 'ART-STRING))
		      (I 0)
		      (J START (1+ J))
		      (QUOTE-P NIL)
		      (CHAR))
		     (( J IDX)
		      SUBSTRING)
		   (SETQ CHAR (LOGAND #o177 (AREF STRING J)))
		   (IF (AND (NOT QUOTE-P)
			    (MEMQ CHAR '(#/% #/*)))
		       (SETQ CHAR (IF (= CHAR #/%) #/RESUME #/BREAK)))
		   (IF (AND (NOT QUOTE-P) (= CHAR #/))
		       (SETQ QUOTE-P T)
		     (IF QUOTE-P
			 (SETQ QUOTE-P NIL)
		       (SETQ CHAR (CHAR-UPCASE CHAR)))
		     (SETF (AREF SUBSTRING I) CHAR)
		     (INCF I)))))
	  (1+ IDX) DELIM))

))

; From file HOST.LISP KANSAS:<L.NETWORK> OZ: (119)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; HOST  "

(DEFMETHOD (HOST-TOPS20-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (LET ((PN (FS:PARSE-PATHNAME STRING HOST)))
    (IF (NULL (SEND PN :DEVICE))
	(SEND PN :NEW-DEVICE (SEND HOST :PRIMARY-DEVICE))
      PN)))

(DEFMETHOD (HOST-VMS-MIXIN :HSNAME-PATHNAME) (STRING HOST) 
  (FS:PARSE-PATHNAME STRING HOST))

))

; From file QCP1.LISP OZ:<MLY.LL> OZ: (1)
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (:PROPERTY COND P1) (X)
  (COND ((NULL (CDR X))
	 ''NIL)
	((ATOM (CDR X))
	 (WARN 'BAD-COND :IMPOSSIBLE
	       "The atom ~S appears as the body of a ~S." (CDR X) 'COND)
	 ''NIL)
	(T `(COND . ,(MAPCAR #'(LAMBDA (CLAUSE)
				 (COND ((ATOM CLAUSE)
					(WARN 'BAD-COND :IMPOSSIBLE
					      "The atom ~S appears as a ~S-clause."
					      CLAUSE 'COND)
					NIL)
				       (T (P1COND-CLAUSE CLAUSE))))
			     (CDR X))))))

(DEFUN P1 (FORM &OPTIONAL DONT-OPTIMIZE &AUX TM)
  (UNLESS DONT-OPTIMIZE
    (SETQ FORM (COMPILER-OPTIMIZE FORM)))
  (SETQ FORM (COND ((ATOM FORM)
		    (COND ((SELF-EVALUATING-P FORM)
			   `',FORM)
			  ((SETQ TM (ASSQ FORM VARS))
			   (AND (EQ (VAR-KIND TM) 'FEF-ARG-FREE)
				(ZEROP (VAR-USE-COUNT TM))
				(PUSH (VAR-NAME TM) FREEVARS))
			   (INCF (VAR-USE-COUNT TM))
			   (VAR-LAP-ADDRESS TM))
			  ((TRY-REF-SELF FORM))
			  ((SPECIALP FORM)
			   (MAKESPECIAL FORM) FORM)
			  ((TRY-REF-LEXICAL-VAR FORM))
			  (T (MAKESPECIAL FORM) FORM)))
		   ((EQ (CAR FORM) 'QUOTE) FORM)
		   ;; Certain constructs must be checked for here
		   ;; so we can call P1 recursively without setting TLEVEL to NIL.
		   ((NOT (ATOM (CAR FORM)))
		    ;; Expand any lambda macros -- just returns old function if none found
		    (LET ((FCTN (CAR FORM)))
		      (OR (SYMBOLP (CAR FCTN))
			  (WARN 'BAD-FUNCTION-CALLED :IMPOSSIBLE
				"There appears to be a call to a function whose CAR is ~S."
				(CAR FCTN)))
		      (IF (MEMQ (CAR FCTN) '(LAMBDA NAMED-LAMBDA))
			  (P1LAMBDA FCTN (CDR FORM))
			;; Old Maclisp evaluated functions.
			(WARN 'EXPRESSION-AS-FUNCTION :VERY-OBSOLETE
			      "The expression ~S is used as a function; use ~S."
			      (CAR FORM) 'FUNCALL)
			(P1 `(FUNCALL . ,FORM)))))
		   ((NOT (SYMBOLP (CAR FORM)))
		    (WARN 'BAD-FUNCTION-CALLED :IMPOSSIBLE
			  "~S is used as a function to be called." (CAR FORM))
		    (P1 `(PROGN . ,(CDR FORM))))
		   ((SETQ TM (ASSQ (CAR FORM) *LOCAL-FUNCTIONS*))
		    (INCF (VAR-USE-COUNT (CADR TM)))
		    `(FUNCALL ,(TRY-REF-LEXICAL-HOME (CADR TM))
			      . ,(P1PROGN-1 (CDR FORM))))
		   ((MEMQ (CAR FORM) '(PROG PROG*))
		    (P1PROG FORM))
		   ((MEMQ (CAR FORM) '(LET LET*))
		    (P1LET FORM))
		   ((EQ (CAR FORM) 'BLOCK)
		    (P1BLOCK FORM))
		   ((EQ (CAR FORM) 'TAGBODY)
		    (P1TAGBODY FORM))
		   ((EQ (CAR FORM) '%POP)	;P2 specially checks for this
		    FORM)
		   (T (SETQ TLEVEL NIL)
		      ;; Check for functions with special P1 handlers.
		      (IF (SETQ TM (GET (CAR FORM) 'P1))
			  (FUNCALL TM FORM)
			(IF (NOT (AND ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
				      (ASSQ (CAR FORM) VARS)
				      (NULL (FUNCTION-P (CAR FORM)))))
			    (P1ARGC FORM (GETARGDESC (CAR FORM)))
			  (WARN 'EXPRESSION-AS-FUNCTION :VERY-OBSOLETE
				"The variable ~S is used in function position; use FUNCALL."
				(CAR FORM))
			  (P1 `(FUNCALL . ,FORM)))))))
  (IF (AND (ATOM FORM) (SELF-EVALUATING-P FORM))
      ;; a p1 handler may return :foo, for example
      `',FORM
      FORM))

))

; From file QFCTNS.LISP KANSAS:<L.SYS> OZ: (774)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(compiler:make-obsolete function-debugging-info "Use DEBUGGING-INFO")

))

; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (534)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(DEFMETHOD (PATHNAME :PARSE-TRUENAME) (STRING)
  (PARSE-PATHNAME STRING HOST))


(DEFMETHOD (PATHNAME :PRINT-SELF) (STREAM IGNORE SLASHIFY-P)
  (COND (SLASHIFY-P
	 (PRINC "#" STREAM)
	 (PRIN1 (TYPE-OF SELF) STREAM)
	 (TYO #/SP STREAM)
	 (PRIN1 (SEND SELF :STRING-FOR-PRINTING) STREAM)
	 (TYO #/ STREAM))
	(T (SEND STREAM :STRING-OUT (SEND SELF :STRING-FOR-PRINTING)))))

(DEFMETHOD (PATHNAME :READ-INSTANCE) (IGNORE STREAM)
  (PARSE-PATHNAME (CLI:READ STREAM T NIL T)))

))

; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (534)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(defun merge-pathnames (pathname &optional defaults (default-version :newest))
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
If non-NIL, DEFAULT-VERSION specifies the version of the resulting pathname,
else the version is defaulted from the corresponging component DEFAULTS in the usual manner."
  (merge-pathname-components pathname defaults :default-version default-version
						:always-merge-version t))

(defun merge-pathname-components
       (pathname &optional defaults
		 &key (default-version nil default-version-specified-p)
		      (default-type nil default-type-specified-p)
		      (default-name nil default-name-specified-p)
		      always-merge-name always-merge-type always-merge-version
		 &aux default new-device new-directory new-name new-type new-version
		      new-otype merge-name-p merge-type-p merge-version-p)
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
If supplied, DEFAULT-NAME, DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
their components if those components are not supplied by PATHNAME.
Otherwise, these components are defaulted from DEFAULTS in the usual manner.
ALWAYS-MERGE-xxx mean that the the xxx components should *always* be merged in
/(from either DEFAULT-xxx or from DEFAULTS) even if the relevant component is already
specified by PATHNAME."
  (setq pathname (parse-pathname pathname nil defaults))
  (if (null defaults) (setq defaults *default-pathname-defaults*))
  (if (not (typep pathname 'pathname))
      pathname					;Some funny thing.  No defaulting possible.
    (setq default (if (atom defaults)
		      (parse-pathname defaults nil pathname)
		      (default-pathname defaults (pathname-host pathname) nil nil t)))
    ;; Merge the and device and directory in vanilla fashion
    (when (null (pathname-device pathname))
      (setq new-device (pathname-device default)))
    (let ((pdir (pathname-directory pathname))
	  (ddir (pathname-directory default)))
      (cond ((null pdir)
	     (setq new-directory ddir))
	    ((eq (car-safe pdir) :relative)
	     (setq new-directory
		   (merge-relative-directory pdir ddir)))))
    ;; merge name type and version hirsutely
    (when (or (null (pathname-name pathname))
	      always-merge-name)
      (setq new-version (if default-name-specified-p
			    default-name
			    (pathname-name default))
	    merge-name-p t))
    (when (or (null (pathname-type pathname))
	      always-merge-type)
      (setq merge-type-p t)
      (if default-type-specified-p
	  (setq new-type default-type)
	  (multiple-value-setq (new-type new-otype) (send default :canonical-type))))
    (when (or (null (pathname-version pathname))
	      always-merge-version)
      (setq new-version (if default-version-specified-p
			    default-version
			    (pathname-version default))
	    merge-version-p t))
    (send pathname :new-pathname
		   (if new-device :device) new-device
		   (if new-directory :directory) new-directory
		   (if merge-name-p :name) new-name
		   (if merge-type-p :type) new-type
		   (if new-otype :original-type) new-otype
		   (if merge-version-p :version) new-version)))
))

; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (534)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(DEFUN SET-HOST-WORKING-DIRECTORY (HOST PATHNAME)
  "Set the working device//directory for HOST to that in PATHNAME.
When a pathname containing device component DSK is defaulted,
its device is replaced by the working device, and its directory
defaulted (if not explicitly specified) to the working directory."
  (LET* ((HOST1 (GET-PATHNAME-HOST HOST))
	 (DIR (PARSE-PATHNAME PATHNAME HOST1)))
    (SEND HOST :SET :GET 'WORKING-DIRECTORY (SEND DIR :PATHNAME-AS-DIRECTORY))))

))

(setf (documentation 'enough-namestring 'function)
  "Return enough namestring to produce whatever OBJECT produced
when merged with DEFAULTS using MERGE-PATHNAMES.
OBJECT is converted to a pathname, and that is made into a string
from which components may be omitted if their values are the same as
what would result from defaulting whatever is left with the specified defaults.")

; From file OPEN.LISP KANSAS:<L.IO.FILE> OZ: (178)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN (:SYNTAX FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
  (VALUES (NCONS '*READTABLE*) (NCONS (SI:FIND-READTABLE-NAMED VAL :ERROR))))

))

; From file QFCTNS.LISP KANSAS:<L.SYS> OZ: (774)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN COPY-SEQ (SEQUENCE)
  "Return a new sequence with the same elements as SEQUENCE, and of the same type.
SEQUENCE may be a list or an array."
  (ETYPECASE SEQUENCE
    (LIST (COPY-LIST SEQUENCE))
    (VECTOR
     (LET* ((LEN (LENGTH SEQUENCE))
	    (NEW (SIMPLE-MAKE-ARRAY LEN (ARRAY-TYPE SEQUENCE))))
       (COPY-ARRAY-PORTION SEQUENCE 0 LEN NEW 0 LEN)
       NEW))))

))

; From file HOST.LISP KANSAS:<L.NETWORK> OZ: (120)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; HOST  "

(DEFFLAVOR HOST-TOPS20-MIXIN ((PRIMARY-DEVICE "PS")) ()
  (:REQUIRED-FLAVORS HOST)
  :SETTABLE-INSTANCE-VARIABLES)

))

; From file HOST.LISP KANSAS:<L.NETWORK> OZ: (120)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; HOST  "

(DEFMETHOD (HOST-VMS-MIXIN :PRIMARY-DEVICE) ()
  (or (get si:property-list :primary-device)
      "USRD$"))

(defmethod (host-vms-mixin :set-primary-device) (device)
  (setf (getf si:property-list :primary-device) device))

))


; From file DOC.LISP KANSAS:<L.ZWEI> OZ: (74)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFCOM COM-SELF-DOCUMENT "Print out documentation for the command on a given key." (KM)
  (LET (CHAR)
    (FORMAT *QUERY-IO* "~&Document command: ")
    (TYPEIN-LINE-ACTIVATE
      (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
	(SETQ CHAR (SEND *QUERY-IO* :ANY-TYI))))
    (DO-FOREVER
      (IF (EQ (CAR-SAFE CHAR) :MOUSE-BUTTON)
	  (SETQ CHAR (CADR CHAR)))
      (COND ((ATOM CHAR)
	     (FORMAT *QUERY-IO* "~:@C" CHAR)
	     (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)
	     (DOCUMENT-KEY CHAR *COMTAB*)
	     (RETURN))
	    ((EQ (CAR CHAR) 'SCROLL)
	     (FORMAT *QUERY-IO* "Mouse-Scroll")
	     (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)
	     (FORMAT T
		     "Mouse Scrolling:
  When the mouse cursor is an up-and-down arrow, near the left edge,
it is in the /"scroll bar/".  Clicking the mouse in the scroll bar
scrolls the text in the window.

When the mouse is near the top or bottom edge and the cursor is a thick arrow,
that too is a place you can scroll, by pushing the mouse against the edge.

In the scroll bar, click left to scroll the line the mouse is on to the
top of the window.  Click right scrolls the same amount in the opposite
direction; the line at the top of the window moves down to the mouse.
Click middle uses the position of the mouse along the edge to choose
a portion of the buffer to view, so that if the mouse is near the bottom
you see something near the end of the file.

A portion of the left edge is thickened to show you what part of the
buffer is currently on the screen.")
	     (RETURN)))))
  DIS-NONE)

))

; From file DOC.LISP KANSAS:<L.ZWEI> OZ: (74)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN DOCUMENT-KEY (CHAR COMTAB)
  "Print full documentation of character CHAR's definition in COMTAB, on *STANDARD-OUTPUT*."
  (IF (OR (ATOM CHAR)
	  (AND (EQ (CAR-SAFE CHAR) :MOUSE-BUTTON)
	       (SETQ CHAR (CADR CHAR))))
      (FORMAT T "~&~:@C" CHAR)
      (FORMAT T "~&~S" CHAR))
  (PROG (TEM PREFIX)
     L  (SETQ TEM (COMMAND-LOOKUP CHAR COMTAB T))
	(COND ((NULL TEM)
	       (FORMAT T " is undefined.~%"))
	      ((SYMBOLP TEM)
	       (IF (NOT (GET TEM 'COMMAND-NAME))
		   (FORMAT T " is ~A, which is not implemented.~%" TEM)
		   (FORMAT T " is ~A, implemented by " (COMMAND-NAME TEM))
		   (SEND *STANDARD-OUTPUT* :ITEM 'FUNCTION-NAME TEM)
		   (FORMAT T ":~%")
		   (DO L *COMMAND-HOOK* (CDR L) (NULL L)
		       (LET ((DOCFN (GET (CAR L) 'HOOK-DOCUMENTATION-FUNCTION)))
			 (AND DOCFN
			      (FUNCALL DOCFN TEM CHAR))))
		   (PRINT-DOC :FULL TEM CHAR)))
	      ((CONSP TEM)
	       (FORMAT T " is an alias for ~@[~:@C ~]~:@C.~%~@[~:@C ~]~:@C"
		       PREFIX
		       (SETQ CHAR (%LOGDPB (FIRST TEM) %%KBD-CONTROL-META (SECOND TEM)))
		       PREFIX CHAR)
	       (GO L))
	      ((MACRO-COMMAND-P TEM)
	       (FORMAT T " is a user defined macro named ~A.
With no argument, run the macro with the repeat count in its definition.
With an argument, ignore the repeat count in its definition and use
the argument instead.~%"
		       (SYMEVAL-IN-CLOSURE TEM 'SYMBOL)))
	      ((PREFIX-COMMAND-P TEM)
	       (FORMAT T " is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
Type a subcommand to document (or * for all):~%")
	       (SETQ PREFIX CHAR
		     CHAR (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
			    (READ-CHAR *STANDARD-INPUT*)))
	       (FORMAT T "~%~:@C" PREFIX)
	       (COND ((CHAR= CHAR #/*)
		      (FORMAT T " has these subcommands:~%")
		      (DOCUMENT-PREFIX-CHAR-TABLE (GET-PREFIX-COMMAND-COMTAB TEM)))
		     (T
		      (FORMAT T " ~:@C" CHAR)
		      (SETQ COMTAB (GET-PREFIX-COMMAND-COMTAB TEM))
		      (GO L))))
	      ((MENU-COMMAND-P TEM)
	       (FORMAT T " is a menu command with the following subcommands:~%")
	       (DO ((L (GET-MENU-COMMAND-COMMANDS TEM) (CDR L))
		    (FLAG T NIL))
		   ((NULL L) (TERPRI))
;>> should makes these items
		 (FORMAT T "~:[, ~]~A" FLAG (CAAR L))))
	      (T (FORMAT T " is garbage!?~%")))))

))

; From file ACCESS.LISP KANSAS:<L.IO.FILE> OZ: (12)
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFFLAVOR BASIC-HOST-UNIT
	(HOST				;Host object
	 ACCESS				;Access object
	 CONTROL-CONNECTION		;May be the only connection if things are multiplexed
	 (LOCK NIL)			;Lock to insure no timing screws
	 (LAST-USE-TIME (TIME)))
	()
  (:INITABLE-INSTANCE-VARIABLES HOST ACCESS)
  (:GETTABLE-INSTANCE-VARIABLES HOST ACCESS CONTROL-CONNECTION LAST-USE-TIME)
  (:REQUIRED-METHODS
    :RESET	; (&optional dont-unlock-lock-p) close all data streams in abort mode,
		; close control connection
    :DORMANT-RESET ; () Reset self if dormant
    :DORMANT-P	; () Return T if dormant.
    :CLOSE-ALL-FILES ; () close all files in this host unit, reporting to *ERROR-OUTPUT*
    		; and returning a list of closed streams
    :VALIDATE-CONTROL-CONNECTION ; (&optional no-error-p) Check that connection hasn't
		; gone away, making a new one if necessary.  Return NIL when failure
		; and NO-ERROR-P equal to T, otherwise barf on failure
    :OPEN-STREAMS ; () Return a list of open streams
    :OPEN-CONTROL-CONNECTION-P ; () Returns T if the connection is in an open state
    :CLOSE-CONTROL-CONNECTION ; () Close connection, for logging out
    )
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES LOCK LAST-USE-TIME))

(DEFMACRO LOCK-HOST-UNIT ((HOST-UNIT) &BODY BODY)
  (LET ((LOCK (GENSYM)) (LOCKED-P (GENSYM)) (HU (GENSYM)))
    `(LET* ((,HU ,HOST-UNIT)
	    (,LOCK (LOCF (BASIC-HOST-UNIT-LOCK ,HU)))
	    (,LOCKED-P NIL))
       (UNWIND-PROTECT
	 (PROGN
	   (COND ((NEQ (CAR ,LOCK) CURRENT-PROCESS)
		  (PROCESS-LOCK ,LOCK)
		  (SETQ ,LOCKED-P T)))
	   . ,BODY)
	 (SETF (BASIC-HOST-UNIT-LAST-USE-TIME ,HU) (TIME))
	 (AND ,LOCKED-P (PROCESS-UNLOCK ,LOCK))))))
))

; From file ACCESS.LISP KANSAS:<L.IO.FILE> OZ: (12)
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFUN FILE-LOGIN (LOGINP &AUX TEM)
  "Log all open host units in or out.  LOGINP = NIL means log out, otherwise log in."
  (DOLIST (HOST *PATHNAME-HOST-LIST*)
    (DOLIST (PROP '(QFILE-CONNECTED-DIRECTORY QFILE-ACCESSED-DIRECTORY CAPABILITIES-ALIST))
      (AND (SETQ TEM (GET-LOCATION-OR-NIL HOST PROP)) (SETF (CONTENTS TEM) NIL)))
    (DOLIST (UNIT (SEND HOST :SEND-IF-HANDLES :HOST-UNITS))
      (SEND HOST :LOGIN-UNIT UNIT LOGINP))))

(DEFMETHOD (FILE-HOST-MIXIN :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT))
  (AND ACCESS (SEND ACCESS :CLOSE-ALL-FILES MODE)))


(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT) &AUX CLOSED)
  (DOLIST (UNIT HOST-UNITS)
    (SETQ CLOSED (NCONC CLOSED (SEND UNIT :CLOSE-ALL-FILES MODE))))
  CLOSED)

))

; From file ACCESS.LISP KANSAS:<L.IO.FILE> OZ: (12)
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFMETHOD (DATA-CONNECTION-MIXIN :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT) &AUX CLOSED)
  (DOLIST (DATA-CONN DATA-CONNECTIONS)
    (DO ((LIST (DATA-STREAM-LIST DATA-CONN) (CDDR LIST)))
	((NULL LIST))
      (LET ((STREAM (CADR LIST)))
	(COND ((NULL STREAM))
	      ((EQ STREAM T)
	       (SETF (CADR LIST) NIL))
	      (T
	       (FORMAT *ERROR-OUTPUT* "~%Closing ~S" STREAM)
	       (PUSH STREAM CLOSED)
	       (SEND STREAM :CLOSE MODE))))))
  CLOSED)


(DEFMETHOD (si::BASIC-HOST :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT))
  (LOOP FOR STREAM IN (SEND SELF :OPEN-STREAMS)
	DO (FORMAT *ERROR-OUTPUT* "~%Closing ~S" STREAM)
	   (SEND STREAM :CLOSE MODE)
	COLLECT STREAM))

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (357)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFMETHOD (QFILE-ACCESS :CHANGE-CAPABILITIES)
	   (ENABLEP CAPABILITIES &OPTIONAL HOST-UNIT
	    &AUX WHOSTATE COMMAND (ALIST (GET HOST 'CAPABILITIES-ALIST)) LOSERS
	    (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (IF ENABLEP
      (SETQ WHOSTATE "Enable" COMMAND 'ENABLE-CAPABILITIES)
      (SETQ WHOSTATE "Disable" COMMAND 'DISABLE-CAPABILITIES))
  (WITH-STACK-LIST (HU HOST-UNIT)		;Are we CONNSING yet?
    (SEND SELF :GET-HOST-UNIT T)			;need at least one
    (IF (EQUAL HU '(NIL)) (SETQ HU (SEND SELF :HOST-UNITS)))
    ;; have to do them one at a time so that can get success/failure on individual capability
    ;;  basis, rather than error for whole transaction if once capability is losing.
    (DOLIST (CAP CAPABILITIES)
      (SETQ CAP (STRING-UPCASE CAP))
      (LET ((ELEM (ASSOC-EQUAL CAP ALIST))
	    (FLAG NIL))
	(DO ((HU HU (CDR HU))
	     PKT SUCCESS STRING)
	    ((NULL HU))
	  (LOCK-HOST-UNIT ((CAR HU))
	    (IF (SEND (CAR HU) :VALIDATE-CONTROL-CONNECTION)
		(UNWIND-PROTECT
		  (PROGN
		    (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
		      (SEND (CAR HU) :COMMAND NIL NIL NIL WHOSTATE
			    COMMAND " " CAP))
		    (COND (FLAG)
			  ((NOT SUCCESS)
			   (WHEN (NULL (CDR HU))
			     (SETQ ALIST (DELQ ELEM ALIST))
			     (PUSHNEW CAP LOSERS :TEST #'EQUAL)))
			  (T
			   (LOOP WITH I = 0
				 WHILE (SETQ I (STRING-SEARCH-CHAR #/SPACE STRING (1+ I)))
			      WHEN (STRING-EQUAL CAP STRING :START2 (1+ I)
							    :END2 (+ I 1 (LENGTH CAP)))
			        DO (SETQ FLAG T) 
				   (UNLESS ELEM
				     (SETQ ELEM (CONS CAP NIL))
				     (PUSH ELEM ALIST))
				   (SETF (CDR ELEM)
					 (CHAR-EQUAL
					   (CHAR STRING (SETQ I (+ I 2 (LENGTH CAP))))
					   ;; will be either T or NIL
					   (IF ENABLEP #/T #/N)))))))
		  (CHAOS:RETURN-PKT PKT))))))))
  (VALUES (SETF (GET HOST 'CAPABILITIES-ALIST) ALIST)
	  LOSERS))

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (357)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFUN DIRECTORY-OPERATION-CHAOS (OPERATION ACCESS PATHNAME ERRORP WHOSTATE
				  &AUX HOST-UNIT PKT SUCCESS FILE-STRING)
  (FILE-OPERATION-RETRY
    (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
    (UNWIND-PROTECT
	(PROGN
	  (MULTIPLE-VALUE-SETQ (PKT SUCCESS FILE-STRING)
	    (SEND HOST-UNIT :COMMAND NIL NIL NIL WHOSTATE
		  (STRING OPERATION) #/NEWLINE
		  (FILE-PRINT-DIRECTORY PATHNAME) #/NEWLINE))
	  (COND (SUCCESS
		 (LET ((START (QFILE-CHECK-COMMAND (STRING OPERATION) FILE-STRING)))
		   (VALUES (PARSE-NUMBER FILE-STRING START))))
		(T
		 (QFILE-PROCESS-ERROR-NEW FILE-STRING PATHNAME NIL (NOT ERRORP) OPERATION))))
      (AND PKT (CHAOS:RETURN-PKT PKT)))))

(DEFMETHOD (QFILE-ACCESS :EXPUNGE) (FILE ERROR)
  (DIRECTORY-OPERATION-CHAOS :EXPUNGE SELF FILE ERROR "Expunge Directory"))

(DEFMETHOD (QFILE-ACCESS :CREATE-DIRECTORY) (FILE ERROR)
  (DIRECTORY-OPERATION-CHAOS :CREATE-DIRECTORY-CHAOS SELF FILE ERROR "Create Directory"))


))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (357)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFUN CWD-CHAOS (ACCESS PATHNAME ERROR-P ACCESSP &OPTIONAL (HOST-UNIT NIL HOST-UNIT-SPECD)
		  &AUX PKT SUCCESS FILE-STRING COMMAND NEED-PASSWORD ENABLE-CAPABILITIES
		  (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA) (HOST (SEND ACCESS :HOST)))
  (SETQ COMMAND (IF ACCESSP "ACCESS" "CWD"))
  (DO-FOREVER
    (LET ((DIR (FILE-PRINT-DIRECTORY PATHNAME))
	  (PASSWORD ""))
      ;; If we have failed once, ask for a new password.
      ;; The first time, if we remember a password, use it.
      (COND (NEED-PASSWORD
	     (MULTIPLE-VALUE (DIR PASSWORD ENABLE-CAPABILITIES)
	       (FS:FILE-GET-PASSWORD DIR HOST T))
	     (WHEN ENABLE-CAPABILITIES (SEND ACCESS :ENABLE-CAPABILITIES))
	     (SETQ PATHNAME (FS:PARSE-PATHNAME DIR HOST))
	     (SETQ DIR (FILE-PRINT-DIRECTORY PATHNAME)))
	    ;; We know the user id; use remembered password if any.
	    ((EQUAL PASSWORD "")
	     (SETQ PASSWORD
		   (OR (CADR (SI:ASSOC-EQUALP (LIST DIR (SEND HOST :NAME))
					      FS:USER-HOST-PASSWORD-ALIST))
		       ""))))
      (OR HOST-UNIT-SPECD (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT)))
      (UNWIND-PROTECT
	  (PROGN
	    (MULTIPLE-VALUE-SETQ (PKT SUCCESS FILE-STRING)
	      (SEND HOST-UNIT :COMMAND NIL NIL NIL COMMAND
		    COMMAND #/NEWLINE
		    DIR #/NEWLINE
		    PASSWORD #/NEWLINE))
	    (COND (SUCCESS
		   ;; Succeeded on one host unit.
		   ;; Record what our connected or accessed directory is for this host.
		   (IF ACCESSP
		       (PUSHNEW PATHNAME (GET HOST 'QFILE-ACCESSED-DIRECTORIES))
		       (SEND HOST :SET :GET 'QFILE-CONNECTED-DIRECTORY PATHNAME))
		   ;; Also inform any other host units that are connected now.
		   (OR HOST-UNIT-SPECD
		       (LET ((UNITS (SEND ACCESS :HOST-UNITS)))
			 (DOLIST (UNIT UNITS)
			   (AND (NEQ UNIT HOST-UNIT)
				(SEND UNIT :VALIDATE-CONTROL-CONNECTION T)
				(SEND UNIT :COMMAND NIL NIL NIL COMMAND
				      COMMAND #/NEWLINE
				      DIR #/NEWLINE
				      PASSWORD #/NEWLINE)))))
		   (RETURN T))
		  (T
		   (CONDITION-CASE-IF (NOT ERROR-P) (ERROR-OBJECT)
		       (CONDITION-CASE ()
			   (QFILE-PROCESS-ERROR-NEW FILE-STRING)
			 (LOGIN-PROBLEMS
			  ;; Since this password is wrong,
			  ;;  flush it from list of remembered ones.
			  (LET ((ALIST-ELEMENT
				  (SI:ASSOC-EQUALP (LIST DIR (SEND HOST :NAME))
						   FS:USER-HOST-PASSWORD-ALIST)))
			    (IF ALIST-ELEMENT
				(SETQ FS:USER-HOST-PASSWORD-ALIST
				      (DELQ ALIST-ELEMENT FS:USER-HOST-PASSWORD-ALIST))))
			  (SETQ NEED-PASSWORD T)))
		     (ERROR ERROR-OBJECT)))))
	(AND PKT (CHAOS:RETURN-PKT PKT))))))

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (357)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

;;;; Fascist caste system support. Boo Hiss etc
(DEFMETHOD (QFILE-ACCESS :ENABLE-CAPABILITIES) (CAPABILITIES &OPTIONAL UNIT)
  (SEND SELF :CHANGE-CAPABILITIES T CAPABILITIES UNIT))

(DEFMETHOD (QFILE-ACCESS :DISABLE-CAPABILITIES) (CAPABILITIES &OPTIONAL UNIT)
  (SEND SELF :CHANGE-CAPABILITIES NIL CAPABILITIES UNIT))


))

; From file OPEN.LISP KANSAS:<L.IO.FILE> OZ: (178)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN CLOSE-ALL-FILES (&OPTIONAL (MODE :ABORT))
  "Close all file streams that are open."
  (NCONC (AND (BOUNDP 'TV::WHO-LINE-FILE-STATE-SHEET)
	      TV::WHO-LINE-FILE-STATE-SHEET
	      (DO ((F (SEND TV::WHO-LINE-FILE-STATE-SHEET :OPEN-STREAMS)
		      (CDR F))
		   (THINGS-CLOSED NIL))
		  ((NULL F)
		   (SEND TV::WHO-LINE-FILE-STATE-SHEET :DELETE-ALL-STREAMS)
		   (NREVERSE THINGS-CLOSED))
		(FORMAT *ERROR-OUTPUT* "~%Closing ~S" (CAR F))
		(PUSH (CAR F) THINGS-CLOSED)
		(SEND (CAR F) :CLOSE MODE)))
	 (LOOP FOR HOST IN *PATHNAME-HOST-LIST*
	       NCONC (SEND HOST :CLOSE-ALL-FILES MODE))))

))

; From file OPEN.LISP KANSAS:<L.IO.FILE> OZ: (178)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN EXPUNGE-DIRECTORY (PATHNAME &REST OPTIONS &KEY (ERROR T))
  "Expunge all deleted files in the directory specified in PATHNAME.
PATHNAME can be a pathname or a namestring."
  (DECLARE (VALUES BLOCKS-FREED))
  (FORCE-USER-TO-LOGIN)
  ;; avoid merge-pathname-defaults braindeath
  (OR (SEND (SETQ PATHNAME (FS:PARSE-PATHNAME PATHNAME)) :NAME)
      (SETQ PATHNAME (SEND PATHNAME :NEW-NAME :WILD)))
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME NIL :WILD :WILD))
  (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
			      (PATHNAME FILE-ERROR)
    (LEXPR-SEND PATHNAME :EXPUNGE OPTIONS)))

))

; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (534)
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

; From file ACCESS.LISP KANSAS:<L.IO.FILE> OZ: (13)
#10R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; ACCESS  "

(DEFMETHOD (BASIC-HOST-UNIT :RESTORE-SERVER-STATE) (ENABLE-CAPABILITIES &AUX TEM)
  (IF (SETQ TEM (GET HOST 'CONNECTED-DIRECTORY))
      (SEND ACCESS :REMOTE-CONNECT TEM T NIL SELF))
  (IF (SETQ TEM (GET HOST 'ACCESSED-DIRECTORIES))
      (DOLIST (X TEM)
	(SEND ACCESS :REMOTE-CONNECT X T T SELF)))
  (IF (SETQ TEM (GET HOST 'CAPABILITIES-ALIST))
      (DOLIST (X TEM)
	(WITH-STACK-LIST (CAP (CAR X))
	  (SEND ACCESS (IF (CDR X) :ENABLE-CAPABILITIES :DISABLE-CAPABILITIES) CAP))))
  (IF ENABLE-CAPABILITIES (SEND HOST :ENABLE-CAPABILITIES)))

))

; From file QTRACE.LISP KANSAS:<L.SYS2> OZ: (151)
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; QTRACE  "


;; foo called from lisp-reinitialize. No point recompiling that.
;(DEFMACRO TRACE (&REST SPECS)
;  "Trace one or more functions. With no args returns a list of all funnction-specs traced.
;For anything but the simplest case of tracing symbol-functions this function
;has such a bletcherous interface that you'd probably be better off trying to overcome
;your fear of rodents and calling TV::TRACE-VIA-MENUS instead."
;  (IF (NULL SPECS)
;      `',TRACED-FUNCTIONS
;    `(MAPCAN #'TRACE-1 ',SPECS)))

;(DEFMACRO UNTRACE (&REST FNS)
;  "Untrace one or more functions.  With no arg, untrace all traced functions."
;  (LET ((INSIDE-TRACE T))
;    `(MAPCAR #'UNTRACE-1 ',(OR FNS TRACED-FUNCTIONS))))

(deff trace-step-apply #'step-apply)
))

; From file ENCAPS.LISP KANSAS:<L.SYS2> OZ: (27)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ENCAPS  "

(defun rename-within-add (within-function function-to-rename
			  &aux (default-cons-area background-cons-area))
  "Make FUNCTION-TO-RENAME be renamed for calls inside WITHIN-FUNCTION.
A new uninterned symbol will named ALTERED-function-to-rename-WITHIN-within-function
will be created, defined to call FUNCTION-TO-RENAME, and put in
place of FUNCTION-TO-RENAME wherever it is called inside WITHIN-FUCTION.
The uninterned symbol is returned so you can redefine it."
  (rename-within-init within-function)
  (let* ((tem (rename-within-renamings-slot within-function))
	 (new (cadr (assoc-equal function-to-rename (cadr tem)))))
    (unless new
      (setq new (make-symbol (format nil "ALTERED-~S-WITHIN-~S"
					 function-to-rename within-function)))
      (push (list function-to-rename new) (cadr tem))
      (rename-within-replace-function new function-to-rename within-function)
      (fset new function-to-rename))
    new))

(defun rename-within-flush (within-function &aux def)
  (setq within-function (unencapsulate-function-spec within-function 'rename-within))
  (setq def (fdefinition (unencapsulate-function-spec within-function '(rename-within))))
  (and (eq (car-safe (fdefinition within-function)) 'macro)
       (setq def (cons 'macro def)))
  ;; don't want to use fdefine, as that would record source file name.
  ;;  So we resort to this kludge.
  (setf (contents (fdefinition-location within-function)) def)
  (setq rename-within-functions
	(delq within-function rename-within-functions)))

))
