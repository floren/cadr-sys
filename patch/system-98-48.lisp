;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 4/18/84 01:07:00 by RpK,
;;; Reason: Supdup: Update documentation: you can get to Internet hosts.
;;; Chaos: SHOW-ROUTING-TABLE of host, SHOW-ROUTING-PATH follows routing info, server
;;;  for DUMP-ROUTING-TABLE protocol
;;; while running on Lisp Machine One from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.44, CADR 3.6, Inconsistent (unreleased patches loaded) ZMail 53.15, MIT-Specific 22.0, microcode 309.



; From file CHSAUX.LISP PS:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "


;;; Routing table format: for N subnets, N*4 bytes of data, holding N*2 words
;;; For subnet n, pkt[2n] has the method; if this is less than 400 (octal), it's
;;; an interface number; otherwise, it's a host which will forward packets to that
;;; subnet.  pkt[2n+1] has the host's idea of the cost.

(DEFUN FORMAT-ROUTING-TABLE-PKT (PKT &OPTIONAL (STREAM STANDARD-OUTPUT) &AUX METHOD COST)
  (FORMAT STREAM "~%Subnet   Method                Cost~%")
  (DOTIMES (I (TRUNCATE (PKT-NBYTES PKT) 4))
    (AND (NOT (ZEROP (SETQ METHOD (AREF PKT (* I 2)))))
	 (< (SETQ COST (AREF PKT (1+ (* I 2)))) (TRUNCATE MAXIMUM-ROUTING-COST 2))
	 (FORMAT STREAM "~3O      ~A~28T~6D~%"
		 I
		 (IF (< METHOD 400) (FORMAT () "Interface ~D" (TRUNCATE METHOD 2))
		   (HOST-DATA METHOD))
		 COST))))

(DEFUN SHOW-ROUTING-TABLE (HOST &OPTIONAL (STREAM STANDARD-OUTPUT))
  (CONDITION-CASE (PKT) (SIMPLE HOST "DUMP-ROUTING-TABLE")
    (SYS:NETWORK-ERROR
     (SEND STREAM :FRESH-LINE)
     (SEND STREAM :STRING-OUT "Network error: ")
     (SEND PKT :REPORT STREAM))
    (:NO-ERROR
     (UNWIND-PROTECT (FORMAT-ROUTING-TABLE-PKT PKT STREAM)
       (RETURN-PKT PKT)))))

(DEFUN SHOW-ROUTING-PATH (&KEY (FROM SI:LOCAL-HOST) TO &AUX METHOD-AS-HOST)
  "Show how packets would most likely from a host to TO.
The required TO argument can be a ChaosNet host/address or a subnet number."
  (OR (AND (NUMBERP TO) (< TO #2R11111111))
      (SETQ TO (LDB 1010 (CHAOS:ADDRESS-PARSE TO))))
  (CONDITION-CASE (PKT) (SIMPLE FROM "DUMP-ROUTING-TABLE")
    (SYS:NETWORK-ERROR
     (SEND TERMINAL-IO :FRESH-LINE)
     (SEND TERMINAL-IO :STRING-OUT "Network error: ")
     (SEND PKT :REPORT TERMINAL-IO))
    (:NO-ERROR
     (UNWIND-PROTECT
	 (LET ((METHOD (AREF PKT (* 2 TO)))
	       (COST (AREF PKT (1+ (* 2 TO)))))
	   (COND ((OR (ZEROP METHOD)
		      ( TO (TRUNCATE (PKT-NBYTES PKT) 4)))
		  (FORMAT T "~&No routing table entry for subnet ~O in ~A."
			  TO (HOST-DATA FROM)))
		 ((< METHOD 400)
		  (FORMAT T "~&Direct path from ~A to subnet ~O at interface ~D."
			  (HOST-DATA FROM) TO (TRUNCATE METHOD 2)))
		 (T
		  (FORMAT T "~&~A will bounce the packet off ~A at cost ~D."
			  (HOST-DATA FROM) (HOST-DATA METHOD) COST)
		  (SETQ METHOD-AS-HOST METHOD))))
       (RETURN-PKT PKT))
     (IF METHOD-AS-HOST (SHOW-ROUTING-PATH :FROM METHOD-AS-HOST :TO TO)))))
  
(DEFUN DUMP-ROUTING-TABLE (&AUX (PKT (GET-PKT))
				(N-SUBNETS (MIN (ARRAY-LENGTH ROUTING-TABLE)
						(TRUNCATE MAX-DATA-WORDS-PER-PKT 2))))
  (DO ((SUBNET 0 (1+ SUBNET))
       (PKT_IDX 0))
      ((= SUBNET N-SUBNETS))
    (SETF (AREF PKT PKT_IDX) (AREF ROUTING-TABLE SUBNET))
    (INCF PKT_IDX) ; deposit cost in next word
    (SETF (AREF PKT PKT_IDX) (AREF ROUTING-TABLE-COST SUBNET))
    (INCF PKT_IDX))
  ;;; set the number of bytes before actually sending....
  (SETF (PKT-NBYTES PKT) (* 4 N-SUBNETS))
  (ANSWER (LISTEN "DUMP-ROUTING-TABLE") PKT))

(ADD-INITIALIZATION "DUMP-ROUTING-TABLE" '(DUMP-ROUTING-TABLE) () 'SERVER-ALIST)

))

; From file SUPDUP.LISP PS:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

(DEFMETHOD (BASIC-NVT :HELP-MESSAGE)
	   (&AUX (FORMAT-ARGS
		   (LIST "~
~&You are using the ~A remote-login program.
To connect to any Chaosnet or Internet host, just type the target host name.
If you want to connect to an Internet host and specify a particular gateway
host, type the gateway host name, an altmode, and the target host name.
If you want to connect to a specific socket on an Internet host, follow
the name of the Internet host by a slash and the socket number in octal.
If you want to connect to a specific connect-name on a Chaosnet host,
follow the name of the Chaosnet host by a slash and the connect name.

Summary:
  host      (for either network)
  gatewayinternet-host
  chaos-host//connect-name
  internet-host//socket-number (octal)
  gatewayinternet-host//socket-number (octal)

At any time you can type the [Network] key to give any of a number of useful
commands.  For descriptions of the available commands, type [Network] [Help].

Connect to host: "
			 PROGRAM-NAME)))
  (DECLARE (SPECIAL FORMAT-ARGS))
  (COND ((NULL CONNECTION)
	 (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
	 (LEXPR-FUNCALL 'FORMAT T FORMAT-ARGS))
	(T
	 (SI:WITH-HELP-STREAM (HELP-STREAM :LABEL "Keyboard system commands")
	   (LEXPR-FUNCALL 'FORMAT HELP-STREAM FORMAT-ARGS)))))

))
