;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.10
;;; Reason:
;;;  CHAOS:POLL-HOSTS: Don't hang HOSTAT if prodding unknown hosts.
;;; Written 10-May-23 13:36:29 by ams,
;;; while running on Lisp Machine One from band 7
;;; with Experimental System 100.9, Hacks by AMS 1.0, microcode 323, AMS.



; From file FC: /tree/network/chaos/chsaux.lisp at 10-May-23 13:36:29
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //tree//network//chaos//chsaux"

(DEFUN POLL-HOSTS (HOSTS CONTACT-NAME STREAM HEADER-FUNCTION FORMAT-FUNCTION
		   &KEY IGNORE-STATES (WINDOW-SIZE 1) (TIMEOUT 600.) (WHOSTATE "Poll Hosts")
		   &AUX CONNECTIONS PKT (OPEN-CONNECTIONS 0))
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
		    (WHEN HOST (SETQ PUNT 'HOST)))
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
