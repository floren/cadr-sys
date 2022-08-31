;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11/18/84 04:16:21 by Mly,
;;; Reason: New chaos NCP should fix retransmission lossage
;;;   Please tell me if there are any more problems with this
;;; Multiple host polling improvements
;;; while running on Lisp Machine Twelve from band 4
;;; with System 98.78, CADR 3.10, ZMail 53.19, MIT-Specific 22.5, microcode 309, gc@36.


; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CHSNCP; CHSAUX  "

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
		(WHEN (MEMQ PUNT '(HOST CONN))
		  (REMOVE-CONN CONN)
		  (SETQ CONNECTIONS (DELQ ELEM CONNECTIONS))
		  (DECF OPEN-CONNECTIONS))
		(WHEN (AND HOST (EQ PUNT 'HOST))
		  (DOLIST (C CONNECTIONS)
		    (WHEN (EQ (CAR C) HOST)
		      (WHEN (THIRD C) (REMOVE-CONN (THIRD C)))
		      (SETQ CONNECTIONS (DELQ C CONNECTIONS))
		      (DECF OPEN-CONNECTIONS)))))))))
    ;; Remove host objects with no connections attached to them
    ;; Unwind-protect cleanup -- Flush any connections that remain
    (LOOP FOR (HOST ADDRESS CONN) IN CONNECTIONS
	  WHEN CONN DO (CLOSE-CONN CONN)))) 

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CHSNCP; CHSAUX  "

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
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CHSNCP; CHSAUX  "

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
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CHSNCP; CHSAUX  "

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

(if-in-cadr (load "sys:patch;system-98-79 ncp" :verbose nil :set-default-pathname nil))