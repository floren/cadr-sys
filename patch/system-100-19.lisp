;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.19
;;; Reason:
;;;  Add support for handling BRD Chaosnet packets; fix some timing issues in Chaosnet code.
;;; Written 6-Jun-23 17:38:37 by ams,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.13, Hacks by AMS 2.0, microcode 323, WIP.



; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:38:37
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RECEIPT (CONN ACK-LEV &OPTIONAL (REC-LEV ACK-LEV))
 (WITHOUT-INTERRUPTS
   (LET ((SENDS (SEND-PKTS CONN))	;(Save array references...)
	 (NEXT NIL)			;Prevent weird screw.
	 (LENGTH (SEND-PKTS-LENGTH CONN)))
     (DO ((PKT SENDS NEXT))			;For each PKT the destination has received...
	 ((OR (NULL PKT) (PKTNUM-< REC-LEV (PKT-NUM PKT))))
;      (SETQ NEXT (PKT-LINK PKT))
       (SETQ NEXT (SETQ SENDS (PKT-LINK PKT)))  ;Two variables only for "clairity"
       (FREE-PKT PKT)
       (DECF LENGTH))
     (SETF (SEND-PKTS CONN) SENDS)
     (SETF (SEND-PKTS-LENGTH CONN) LENGTH)
     (UNLESS SENDS
       (SETF (SEND-PKTS-LAST CONN) NIL)))
   (WHEN (PKTNUM-< (SEND-PKT-ACKED CONN) ACK-LEV)
     (SETF (SEND-PKT-ACKED CONN) ACK-LEV))
   (UPDATE-WINDOW-AVAILABLE CONN)))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:39:08
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RETRANSMISSION (CONN &AUX TIME (INHIBIT-SCHEDULING-FLAG T))
  (WHEN (MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE broadcast-sent-state))	;BV: incl broadcast
    ;; Only if it is open or awaiting a response from RFC or BRD
    (when (null (send-pkts conn))
      (when (pktnum-< (send-pkt-acked conn) (pkt-num-sent conn))
	;;Need to retransmit but no packets on send-list.  Remote end has received all of
	;;our packets but has not acknowledged them all.  Send a SNS to elicit a STS
	(setq inhibit-scheduling-flag nil)
	(let ((pkt (allocate-int-pkt)))
	  (setf (pkt-opcode pkt) sns-op)
	  (setf (pkt-nbytes pkt) 0)
	  (setf (pkt-fwd-count pkt) 0)
	  (transmit-int-pkt-for-conn conn pkt))
	(setq more-retransmission-needed t))	;Indicate that pkts remain unacknowledged
      (return-from retransmission t))				;And return from this function
    ;; Doing this outside the loop can lose
    ;; because then TIME can be less than (PKT-TIME-TRANSMITTED PKT).
    (SETQ TIME (TIME))			;On the other hand, doing it inside the loop loses
    ;;because if there are enough PKTs pending for a particular CONN, it can
    ;;hang because every time it sends one it has to restart from the beginning
    ;;of the list.  So now we deal with the case mentioned above explicitly.
    (BLOCK CONN-DONE
      (DO-FOREVER
	(LET ((INHIBIT-SCHEDULING-FLAG T))
	  (DO* ((PKT (SEND-PKTS CONN) (PKT-LINK PKT))
		(first-pkt-num (and pkt (pkt-num pkt))))
	      ((NULL PKT) (RETURN-FROM CONN-DONE NIL))
	    (COND ((NOT (EQ CONN (PKT-SOURCE-CONN PKT)))
		   (FERROR NIL "~S in SEND-PKTS list for incorrect CONN:
CONN ~S, (PKT-SOURCE-CONN PKT) ~S." PKT CONN (PKT-SOURCE-CONN PKT))))
	    (SETQ MORE-RETRANSMISSION-NEEDED T)
	    (COND ((TIME-LESSP TIME (PKT-TIME-TRANSMITTED PKT)))	;Dont do this one again.
		  (( (TIME-DIFFERENCE TIME (PKT-TIME-TRANSMITTED PKT))
		     (LSH (CONN-RETRANSMISSION-INTERVAL CONN)
			  ;; Retransmit the lowest numbered packet most often
			  (MAX 0 (MIN 5 (1- (pktnum-- (PKT-NUM PKT) first-pkt-num))))))
	      (SETF (PKT-BEING-RETRANSMITTED PKT) T)
	      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	      (TRANSMIT-PKT PKT T)
	      (INCF PKTS-RETRANSMITTED)
	      (SETQ INHIBIT-SCHEDULING-FLAG T)
	      (COND ((EQ (PKT-BEING-RETRANSMITTED PKT) 'FREE)
		     (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)
		     (FREE-PKT PKT))
		    (T (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)))
	      (RETURN NIL))))))		;Must always start from beginning of chain if
					; turned on scheduling, since chain could be invalid
      (PROCESS-ALLOW-SCHEDULE))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:39:29
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN CONN-FINISHED-P (CONN)
  "T unless connection is open but not all our transmissions have been acknowledged."
  (OR (null (send-pkts conn))
     ;( (WINDOW-AVAILABLE CONN) (FOREIGN-WINDOW-SIZE CONN))
      (NEQ (STATE CONN) 'OPEN-STATE)))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:39:46
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RECEIVE-INT-PKT (INT-PKT &AUX (OP (PKT-OPCODE INT-PKT)) CONN ACKN)
  (COND ((NOT (ZEROP (LDB #o0010 (AREF INT-PKT 0))))	;Header version must be zero
	 (FREE-INT-PKT INT-PKT))
	((= OP RUT-OP)
	 (DO ((I FIRST-DATA-WORD-IN-PKT (+ I 2))
	      (N (// (PKT-NBYTES INT-PKT) 4) (1- N))
	      (GATEWAY (PKT-SOURCE-ADDRESS INT-PKT))
	      (N-SUBNETS (ARRAY-LENGTH ROUTING-TABLE))
	      (SUBNET) (COST))
	     ((ZEROP N) (FREE-INT-PKT INT-PKT))
	   (SETQ SUBNET (AREF INT-PKT I) COST (AREF INT-PKT (1+ I)))
	   (WHEN (AND (< SUBNET N-SUBNETS)
		      ( COST (AREF ROUTING-TABLE-COST SUBNET))
		      (NULL (AREF ROUTING-TABLE-TYPE SUBNET)))
	     (SETF (AREF ROUTING-TABLE SUBNET) GATEWAY)
	     (SETF (AREF ROUTING-TABLE-COST SUBNET) COST))))
;;; RPK please check this
;;; Should this say PKT-DEST-ADDRESS instead of INT-PKT-HARDWARE-DEST?
;;; BV: Yes it should.
	((AND (= OP BRD-OP) (ZEROP (PKT-DEST-ADDRESS INT-PKT)))
	 (RECEIVE-BRD INT-PKT))
	(( (PKT-DEST-ADDRESS INT-PKT) MY-ADDRESS)	;Packet to be forwarded
	 (COND ((zerop (pkt-dest-address int-pkt))
		(free-int-pkt int-pkt))
	       ((OR (= (PKT-FWD-COUNT INT-PKT) 17)
		    (> (PKT-NBYTES INT-PKT) MAX-DATA-BYTES-PER-PKT))
		(FREE-INT-PKT INT-PKT)
		(INCF PKTS-OVER-FORWARDED))
	       (T (INCF (PKT-FWD-COUNT INT-PKT))
		  (INCF PKTS-FORWARDED)
		  (TRANSMIT-INT-PKT INT-PKT))))
	(T (RECORD-INT-PKT-HEADER INT-PKT)
	   (AND (BIT-TEST #o200 OP) (INCF DATA-PKTS-IN))
	   (COND
	     ((= OP RFC-OP) (RECEIVE-RFC INT-PKT))
	     ((= OP LOS-OP) (RECEIVE-LOS INT-PKT))
	     ((= OP CLS-OP) (RECEIVE-CLS INT-PKT))
	     ((= OP MNT-OP) (FREE-INT-PKT INT-PKT))
	     ((AND (OR (NULL (SETQ CONN (PKT-DEST-CONN INT-PKT)))
		       ( (PKT-DEST-INDEX-NUM INT-PKT) (LOCAL-INDEX-NUM CONN))
		       (and (not (zerop (foreign-address conn)))	;BV: BRD conn has 0, which matches all
		            ( (PKT-SOURCE-ADDRESS INT-PKT) (FOREIGN-ADDRESS CONN))))
		   (NOT (SETQ CONN (CDR (ASSQ (PKT-DEST-INDEX-NUM INT-PKT)
					      DISTINGUISHED-PORT-CONN-TABLE)))))
	      (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP
				    (IF CONN "You are not connected to this index"
				      "No such index exists")))
	     ((PROG2 (SETF (TIME-LAST-RECEIVED CONN) (TIME))
		     (= OP OPN-OP))
	      (RECEIVE-OPN CONN INT-PKT))
	     ((= OP FWD-OP) (RECEIVE-FWD CONN INT-PKT))
	     ((= OP ANS-OP) (RECEIVE-ANS CONN INT-PKT))
	     ((= OP UNC-OP) (RECEIVE-UNC CONN INT-PKT))
	     ((NOT (OR (= OP SNS-OP) (= OP STS-OP)
		       (= OP EOF-OP) ( OP DAT-OP)))
	      (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Illegal opcode"))
	     ((NOT (= (PKT-SOURCE-INDEX-NUM INT-PKT) (FOREIGN-INDEX-NUM CONN)))
	      (IF (= OP SNS-OP) (FREE-INT-PKT INT-PKT)	;Ignore SNS if not open
		(TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP
				      "That is not your index number for this connection")))
	     ;; Below here can be SNS, STS, EOF, or DAT, all packets having ack fields.
	     ((NOT (EQ (STATE CONN) 'OPEN-STATE))
	      (IF (= OP SNS-OP) (FREE-INT-PKT INT-PKT)	;Ignore SNS if not open
		(TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Connection not open")))
	     (T
	      ;; Below here, this INT-PKT contains a normal acknowledgement field.
	      (SETQ ACKN (PKT-ACK-NUM INT-PKT))	;Acknowledgement field
	      (RECEIPT CONN ACKN)		;Clear receipted packets from send list
	      (COND ((OR (>= OP DAT-OP) (= OP EOF-OP))
		     (RECEIVE-EOF-OR-DAT CONN INT-PKT))
		    ((= OP SNS-OP) (RECEIVE-SNS CONN INT-PKT))
		    ((= OP STS-OP) (RECEIVE-STS CONN INT-PKT))))))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:40:06
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RECEIVE-STS (CONN INT-PKT)
  (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-SECOND-DATA-WORD INT-PKT))
  (RECEIPT CONN (PKT-ACK-NUM INT-PKT) (PKT-FIRST-DATA-WORD INT-PKT))
  (FREE-INT-PKT INT-PKT))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:40:24
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RFC-MEETS-LSN (CONN PKT)
  (SETF (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS PKT))
  (SETF (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM PKT))
  (SETF (FOREIGN-WINDOW-SIZE CONN) (IF (> (PKT-ACK-NUM PKT) MAXIMUM-WINDOW-SIZE)
				       DEFAULT-WINDOW-SIZE
				     (PKT-ACK-NUM PKT)))
  (SETF (PKT-NUM-READ CONN) (PKT-NUM PKT))
  (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM PKT))
  (SETF (PKT-NUM-ACKED CONN) (PKT-NUM PKT))
  (SETF (STATE CONN) 'RFC-RECEIVED-STATE)
  (SETF (READ-PKTS CONN) PKT)
  (SETF (READ-PKTS-LAST CONN) PKT)
  (SETF (PKT-LINK PKT) NIL)
  (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'RFC-RECEIVED-STATE)
  (INTERRUPT-CONN :INPUT CONN))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:40:38
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RECEIVE-OPN (CONN INT-PKT)
  (CASE (STATE CONN)
    (RFC-SENT-STATE (SETF (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM INT-PKT))
		    (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-SECOND-DATA-WORD INT-PKT))
		    (SETF (PKT-NUM-READ CONN) (PKT-NUM INT-PKT))
		    (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM INT-PKT))
		    (SETF (PKT-NUM-ACKED CONN) (PKT-NUM INT-PKT))
		    (SETF (TIME-LAST-RECEIVED CONN) (TIME))
		    (RECEIPT CONN (PKT-ACK-NUM INT-PKT))
		    (SETF (STATE CONN) 'OPEN-STATE)
		    (SETQ RESERVED-INT-PKT INT-PKT)
		    (TRANSMIT-STS CONN 'OPN)
		    (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'OPEN-STATE))
    (OPEN-STATE (COND ((AND (= (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS INT-PKT))
			    (= (FOREIGN-INDEX-NUM CONN)
			       (PKT-SOURCE-INDEX-NUM INT-PKT))
			    (= MY-ADDRESS (PKT-DEST-ADDRESS INT-PKT))
			    (= (LOCAL-INDEX-NUM CONN) (PKT-DEST-INDEX-NUM INT-PKT)))
		       (SETQ RESERVED-INT-PKT INT-PKT)
		       (TRANSMIT-STS CONN 'OPN))
		      (T (TRANSMIT-LOS-INT-PKT INT-PKT
					       LOS-OP
					       "You didn't open this connection"))))
    (OTHERWISE (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Bad state for OPN"))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:41:19
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RESET-ROUTING-TABLE ()
  "Flush out old routing data."
  (SETQ MY-OTHER-SUBNETS NIL)
  (DOTIMES (I (ARRAY-LENGTH ROUTING-TABLE))	;Clear out the routing table
    (SETF (AREF ROUTING-TABLE I) 0)
    (SETF (AREF ROUTING-TABLE-COST I) MAXIMUM-ROUTING-COST)
    (SETF (AREF ROUTING-TABLE-TYPE I) NIL))
  (SETF (AREF ROUTING-TABLE MY-SUBNET) MY-ADDRESS)
  (SETF (AREF ROUTING-TABLE-COST MY-SUBNET) 10.)
  (SI:SELECT-PROCESSOR
    (:CADR
      ;; BV: Send broadcast also on Chaos
      (setf (aref routing-table-type 0) :chaos)
      (SETF (AREF ROUTING-TABLE-TYPE MY-SUBNET) :CHAOS))
    (:LAMBDA
      (SETF (AREF ROUTING-TABLE-TYPE MY-SUBNET) :ETHERNET))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:41:58
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN ENABLE ()
  (SEND BACKGROUND :REVOKE-RUN-REASON)
  (SEND RECEIVER :REVOKE-RUN-REASON)
  (INTERFACE-RESET-AND-ENABLE)
  (SEND BACKGROUND :PRESET 'BACKGROUND)
  (SEND BACKGROUND :RUN-REASON)
  (SEND RECEIVER :PRESET 'RECEIVE-ANY-FUNCTION)
  (SEND RECEIVER :RUN-REASON)
  ;; BV: Enable also BRD reception
  (SETQ *RECEIVE-BROADCAST-PACKETS-P* T)
  (SETQ ENABLE T))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:42:22
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFSELECT ((:PROPERTY CONN NAMED-STRUCTURE-INVOKE))
  (:DESCRIBE (CONN)
    (PRINT-CONN CONN)
    (SI:DESCRIBE-DEFSTRUCT CONN 'CONN))
  (:PRINT-SELF (CONN STREAM IGNORE &OPTIONAL IGNORE)
    (SYS:PRINTING-RANDOM-OBJECT (CONN STREAM)
      (SEND STREAM :STRING-OUT "CHAOS Connection")
      ;; BV: don't try to get host object for address 0
      (LET ((FHOST (if (zerop (foreign-address conn)) "broadcast" (SI:GET-HOST-FROM-ADDRESS (FOREIGN-ADDRESS CONN) :CHAOS)))
	    CONTACT)
	(COND ((SETQ CONTACT (GETF (CONN-PLIST CONN) 'RFC-CONTACT-NAME))
	       (FORMAT STREAM " to ~A ~A" FHOST CONTACT))
	      ((SETQ CONTACT (GETF (CONN-PLIST CONN) 'SERVER-CONTACT-NAME))
	       (FORMAT STREAM " from ~A to ~A server" FHOST CONTACT)))))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:42:34
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN TRANSMIT-PKT (PKT &OPTIONAL ACK-P)
  "Put the pkt on the transmit list, and create a phony transmitter interrupt
  if needed so that the interrupt level will start sending.
If the second arg is T, put an ACK aboard this PKT.
This is a very low level function, called mainly by the following 2 functions.
/(Also called by the retransmitter and forwarder.)"
  (AND (> (PKT-NBYTES PKT) MAX-DATA-BYTES-PER-PKT)
       (FERROR NIL "Attempt to transmit an invalid packet (~S).
The length ~O is greater than the maximum packet size (~O)."
	       PKT (PKT-NBYTES PKT) MAX-DATA-BYTES-PER-PKT))
  (WHEN (and ACK-P
	     (not (= (pkt-opcode pkt) rfc-op))	;BV: from Sys 130
	     (not (= (pkt-opcode pkt) brd-op)))	;BV: don't zap BRD pkts either
    (WITHOUT-INTERRUPTS
      (LET ((CONN (PKT-SOURCE-CONN PKT)))
	(OR CONN (FERROR NIL "~S has null connection." PKT))
	(LET ((ACKN (PKT-NUM-READ CONN)))
	  (SETF (PKT-ACK-NUM PKT) ACKN)
	  (SETF (PKT-NUM-ACKED CONN) ACKN)))))
  (SETF (PKT-TIME-TRANSMITTED PKT) (TIME))
  (SETF (PKT-TIMES-TRANSMITTED PKT) (1+ (PKT-TIMES-TRANSMITTED PKT)))
  (TRANSMIT-INT-PKT (CONVERT-TO-INT-PKT PKT)) )
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:42:46
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN TRANSMIT-INT-PKT (INT-PKT &OPTIONAL (HOST (PKT-DEST-ADDRESS INT-PKT))
                                           (SUBNET (PKT-DEST-SUBNET INT-PKT))
					   (broadcast-if-necessary t)
				 &AUX HARDWARE-P LOCAL-PROC OTHER-LOCAL-PROCS)
  ;;; Simple routing if he is not on my subnet.
  (COND ((AND (NOT (= SUBNET MY-SUBNET))
	      (NOT (MEMQ SUBNET MY-OTHER-SUBNETS)))
	 (AND ( SUBNET (LENGTH ROUTING-TABLE))
	      (SETQ SUBNET 0))
	 (SETQ HOST (AREF ROUTING-TABLE SUBNET))))
  (IF ( (LDB #o1010 HOST) (LENGTH ROUTING-TABLE))
      (FERROR NIL "bad routing table"))
  (SETF (INT-PKT-WORD-COUNT INT-PKT) (1+ (PKT-NWORDS INT-PKT)))
  (SETF (AREF INT-PKT (1- (INT-PKT-WORD-COUNT INT-PKT))) HOST)
  (OR (= (%AREA-NUMBER INT-PKT) CHAOS-BUFFER-AREA)
      (FERROR NIL "Attempt to transmit non-interrupt packet ~A." INT-PKT))
  (AND (BIT-TEST #o200 (PKT-OPCODE INT-PKT)) (SETQ DATA-PKTS-OUT (1+ DATA-PKTS-OUT)))
  (COND ((and (ZEROP HOST)
	      ;; BV: for broadcast pkts, 0 dest is OK (handled below)
	      (not (and broadcast-if-necessary
			(zerop host)
			(zerop (pkt-dest-address int-pkt)))))
	 ;; This means we know no path to the destination.
	 ;; Just give up.
	 (FREE-INT-PKT INT-PKT))
	((= HOST MY-ADDRESS)
	 ;; loop back to myself ... system 98+ handles this a different way
	 (LET ((N-16-BIT-WORDS (+ (CEILING (+ (CHAOS:PKT-NBYTES INT-PKT) 16.) 2) 3)))
	   (SETF (INT-PKT-WORD-COUNT INT-PKT) (1+ N-16-BIT-WORDS))
	   (SETF (INT-PKT-CSR-1 INT-PKT) 0)	; say no CRC-1 error
	   (SETF (INT-PKT-CSR-2 INT-PKT) 0)	; say no CRC-2 error
	   (SETF (INT-PKT-BIT-COUNT INT-PKT)
		 (* (- N-16-BIT-WORDS 3) #o20))	; fill in bit count
	   ;; set the hardware destination
	   (SETF (AREF INT-PKT (- (INT-PKT-WORD-COUNT INT-PKT) 3)) MY-ADDRESS)
	   (WITHOUT-INTERRUPTS
	     (RECEIVE-INT-PKT INT-PKT))))
	(T
	 (ECASE (AREF ROUTING-TABLE-TYPE (LDB #o1010 HOST))
	   (:ETHERNET
	    (COND ((AND (FBOUNDP 'SI:SHARE-MODE-ACTIVE-P)
			(SI:SHARE-MODE-ACTIVE-P))
		   (COND
		     ;; broadcast -- send to everyone
		     ((ZEROP HOST)
		      (SETQ OTHER-LOCAL-PROCS SI:*OTHER-PROCESSORS*)
		      (SETQ HARDWARE-P (EQ SI:*ETHERNET-HARDWARE-CONTROLLER* SI:*MY-OP*)))

		     ;; if he's local, just send to him
		     ((SETQ LOCAL-PROC (UNIX:PROCESSOR-FOR-HOST-IF-ON-MY-NUBUS HOST)))

		     ;; it's definitely going out of this machine - give it to
		     ;; the hardware controller
		     ((EQ SI:*ETHERNET-HARDWARE-CONTROLLER* SI:*MY-OP*)
		      (SETQ HARDWARE-P T))
		     (T
		      (SETQ LOCAL-PROC SI:*ETHERNET-HARDWARE-CONTROLLER*))))
		  (T
		   (SETQ HARDWARE-P T)))

	    (COND ((NOT (NULL HARDWARE-P))
		   (LET ((ETHER-ADDRESS (ETHERNET:GET-ETHERNET-ADDRESS HOST)))
		     (COND ((NOT (NULL ETHER-ADDRESS))
			    (FUNCALL (SI:SELECT-PROCESSOR
				       (:CADR
					 'ETHERNET:SEND-INT-PKT-VIA-UNIBUS-ETHERNET)
				       (:LAMBDA
					 'ETHERNET:SEND-INT-PKT-VIA-MULTIBUS-ETHERNET))
				     INT-PKT
				     ETHERNET:MY-ETHERNET-ADDRESS	;source
				     ETHER-ADDRESS			;destination
				     ETHERNET:CHAOS-ETHERNET-TYPE)
			    (INCF ETHERNET:*ETHERNET-CHAOS-PKTS-TRANSMITTED*)
			    (INCF PKTS-TRANSMITTED))
			   (T
			    (INCF ETHERNET:*ETHERNET-CHAOS-PKTS-NOT-TRANSMITTED-LACKING-ETHERNET-ADDRESS*))))))

	    (DOLIST (OP OTHER-LOCAL-PROCS)
	      (UNIX:TRANSMIT-INT-PKT-TO-SHARING-HOST INT-PKT OP))

	    (IF LOCAL-PROC
		(UNIX:TRANSMIT-INT-PKT-TO-SHARING-HOST INT-PKT LOCAL-PROC))

	    (FREE-INT-PKT INT-PKT))
	   (:CHAOS
	    (SI:SELECT-PROCESSOR
	      (:LAMBDA (FERROR NIL "Trying to use chaosnet hardware")))
	    (WITHOUT-INTERRUPTS
	      (PROG (OLD-TRANSMIT-LIST)
		    (SETQ PKTS-TRANSMITTED (1+ PKTS-TRANSMITTED))
		 LOOP
		    (SETQ OLD-TRANSMIT-LIST (INT-TRANSMIT-LIST))
		    (SETF (INT-PKT-THREAD INT-PKT) OLD-TRANSMIT-LIST)
		    (OR (%STORE-CONDITIONAL INT-TRANSMIT-LIST-POINTER
					    OLD-TRANSMIT-LIST
					    INT-PKT)
			(GO LOOP))
		    (%CHAOS-WAKEUP))))))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:42:59
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN GET-NEXT-PKT (CONN &OPTIONAL (NO-HANG-P NIL) (WHOSTATE "Chaosnet Input")
		     (CHECK-CONN-STATE (NOT NO-HANG-P))
		     &AUX PKT)
  "Return the next input packet from connection CONN.
The packet may contain data, or it may be a CLS, ANS or UNC.
If the next input packet is not are available,
 either wait or return NIL according to NO-HANG-P.
WHOSTATE is what to put in the who-line while we wait, if we wait.
CHECK-CONN-STATE non-NIL says get an error now if connection is in
 an invalid state.  Default is T unless NO-HANG-P.
When you are finished with the data in the packet, use RETURN-PKT
to allow the chaosnet ncp to reuse the packet."
  ;; Loop until we get a packet, decide not to hang, or error out
  (DO-FOREVER
    ;; Check for connection in an erroneous state
    (AND CHECK-CONN-STATE
	 (OR (MEMQ (STATE CONN) '(OPEN-STATE RFC-RECEIVED-STATE ANSWERED-STATE FOREIGN-STATE
					     ;; BV: also this
					     broadcast-sent-state))
	     (IF (EQ (STATE CONN) 'CLS-RECEIVED-STATE)
		 (UNLESS (READ-PKTS CONN)
		   (FERROR 'SYS:CONNECTION-NO-MORE-DATA
			   "Attempt to receive from ~S,
a connection which has been closed by foreign host."
			   CONN))
	       (REPORT-BAD-CONNECTION-STATE CONN "receive from"))))
    ;; Now see if there are any packets we can have
    (WITHOUT-INTERRUPTS
      (SETQ PKT (READ-PKTS CONN))
      (COND (PKT				;Got packet, take off of read list
	     (AND ( UNC-OP (PKT-OPCODE PKT))
		  (SETF (PKT-NUM-READ CONN) (PKT-NUM PKT)))
	     (SETF (READ-PKTS CONN) (PKT-LINK PKT))
	     (COND ((NULL (READ-PKTS CONN))
		    (SETF (READ-PKTS-LAST CONN) NIL))))))
    (AND (NOT (NULL PKT))			;Got packet, acknowledge if necessary
	 (EQ (STATE CONN) 'OPEN-STATE)
	 ( (* 3 (PKTNUM-- (PKT-NUM PKT) (PKT-NUM-ACKED CONN))) (LOCAL-WINDOW-SIZE CONN))
	 (TRANSMIT-STS CONN 'WINDOW-FULL))
    (AND PKT					;Got packet, release from NCP
	 (RELEASE-PKT PKT))
    (AND (OR PKT NO-HANG-P) (RETURN PKT))	;If satisfied, return
    ;; Not satisfied, wait for something interesting to happen
    (PROCESS-WAIT WHOSTATE
		  #'(LAMBDA (X) (OR (READ-PKTS X)
				    (NOT (MEMQ (STATE X) '(OPEN-STATE FOREIGN-STATE
								      ;; BV: also this
								      broadcast-sent-state)))))
		  CONN)))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:43:09
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RECEIVE-CLS (INT-PKT &AUX PKT (INT-FLAG NIL))
  (LET ((CONN (PKT-DEST-CONN INT-PKT)))
    (COND ((NULL CONN)
	   (FREE-INT-PKT INT-PKT))
	  ((eq (state conn) 'BROADCAST-SENT-STATE)
	   ;; BV: ignore (cf MIT AIM 628 sec 4.5
	   )
	  ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
	   (SETQ PKT (CONVERT-TO-PKT INT-PKT))
	   (WITHOUT-INTERRUPTS
	     (FREE-ALL-SEND-PKTS CONN)
	     (FREE-ALL-RECEIVED-PKTS CONN)
	     (SETF (STATE CONN) 'CLS-RECEIVED-STATE)
	     (COND ((NULL (READ-PKTS-LAST CONN))
		    (SETF (READ-PKTS CONN) PKT)
		    (SETQ INT-FLAG T))
		   (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
	     (SETF (READ-PKTS-LAST CONN) PKT)
	     (SETF (PKT-LINK PKT) NIL))
	   (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'CLS-RECEIVED-STATE)
	   (AND INT-FLAG (INTERRUPT-CONN :INPUT CONN)))
	  (T (TRANSMIT-LOS-INT-PKT INT-PKT
				   LOS-OP
				   "You sent a CLS to the wrong kind of connection.")))))
))

; From file FC: /sys/network/chaos/chsncp.lisp at 6-Jun-23 17:43:19
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chsncp"

(DEFUN RECEIVE-ANS (CONN INT-PKT &AUX PKT)
  (COND ((NOT (MEMQ (STATE CONN) '(RFC-SENT-STATE BROADCAST-SENT-STATE)))
	 (FREE-INT-PKT INT-PKT))
	(T (SETQ PKT (CONVERT-TO-PKT INT-PKT))
	   ;; BV: respect the setting in OPEN-BROADCAST-CONNECTION
	   (unless (and (eq (state conn) 'broadcast-sent-state)
			(not (getf (conn-plist conn) 'broadcast-ans-reception-changes-state)))
	     (SETF (STATE CONN) 'ANSWERED-STATE))
	   (SETF (READ-PKTS CONN) PKT)
	   (SETF (PKT-LINK PKT) NIL)
	   (unless (and (eq (state conn) 'broadcast-sent-state)
			(not (getf (conn-plist conn) 'broadcast-ans-reception-changes-state)))
	     (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'ANSWERED-STATE))
	   (INTERRUPT-CONN :INPUT CONN))))
))

; From file FC: /sys/network/chaos/chuse.lisp at 6-Jun-23 17:43:40
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chuse"

(DEFUN OPEN-BROADCAST-CONNECTION (SUBNETS CONTACT-NAME &OPTIONAL (PKT-ALLOCATION 10.)
				  update-ans-state-p
				  &AUX SUBNET-BIT-MAP SUBNET-BIT-MAP-LENGTH)
  "Broadcast a service request from CONTACT-NAME over certain subnets.
PKT-ALLOCATION is the buffering size for unread requests as they come over the net.
The connection returned is in the CHAOS:BROADCAST-SENT-STATE."
  (MULTIPLE-VALUE-SETQ (SUBNET-BIT-MAP SUBNET-BIT-MAP-LENGTH) (SUBNET-BIT-MAP SUBNETS))
  (LET ((CONN (MAKE-CONNECTION)))
    (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN PKT-ALLOCATION MAXIMUM-WINDOW-SIZE)))
    (SETF (FOREIGN-ADDRESS CONN) 0) ; seems ok
    ; (SETF (FOREIGN-INDEX-NUM CONN) FOREIGN-INDEX) ; not sure about this
    (LET ((PKT NIL))
      (UNWIND-PROTECT
	  (PROGN
	    (SETQ PKT (ALLOCATE-PKT))
	    (SETF (PKT-ACK-NUM PKT) SUBNET-BIT-MAP-LENGTH)
	    (SETF (PKT-OPCODE PKT) BRD-OP)
	    (SETF (PKT-LINK PKT) NIL)
	    (SETF (PKT-DEST-ADDRESS PKT) 0)
	    (SETF (PKT-DEST-INDEX-NUM PKT) 0)
	    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
	    (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
	    (SETF (GETF (CONN-PLIST CONN) 'BROADCAST-CONNECTION) T)
	    (SETF (GETF (CONN-PLIST CONN) 'SUBNET-BIT-MAP) SUBNET-BIT-MAP)
	    (SETF (GETF (CONN-PLIST CONN) 'SUBNET-BIT-MAP-LENGTH) SUBNET-BIT-MAP-LENGTH)
	    ;; BV: use standard property to support various printing functions
	    (SETF (GETF (CONN-PLIST CONN) 'rfc-CONTACT-NAME) CONTACT-NAME)
	    ;; BV: note if state should change on receiving ANS
	    (setf (getf (conn-plist conn) 'broadcast-ans-reception-changes-state) update-ans-state-p)
	    (SET-PKT-STRING PKT SUBNET-BIT-MAP CONTACT-NAME)
	    (WITHOUT-INTERRUPTS
	      (SETF (WINDOW-AVAILABLE CONN) 1)
	      (SETF (TIME-LAST-RECEIVED CONN) (TIME))
	      (SETF (STATE CONN) 'BROADCAST-SENT-STATE))
	    (TRANSMIT-PKT PKT ())
	    ;; BV: support standard retransmission mechanisms (see OPEN-CONNECTION)
	    (WITHOUT-INTERRUPTS
	      (SETF (SEND-PKTS CONN) PKT)
	      (SETF (SEND-PKTS-LAST CONN) PKT)
	      (SETF (SEND-PKTS-LENGTH CONN) 1) 
	      (SETQ RETRANSMISSION-NEEDED T)
	      (SETQ PKT NIL)))
	(AND PKT (FREE-PKT PKT)))
    CONN)))
))

; From file FC: /sys/network/chaos/peekch.lisp at 6-Jun-23 17:43:55
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//peekch"

(DEFUN PEEK-CHAOS-PACKET-ITEM (PKT &OPTIONAL (INDENT 0))
  "Returns an item that describes a chaosnet packet.  Mouseable subfields are:
   The host:  Left: Causes info about the host to displayed inferior to the packet.
	      Middle: Causes a static hostat to be displayed inferior to the packet.
  	      Right (menu): Typeout Hostat, Supdup, Telnet, Qsend

Sample output:
Pkt [to ! from] <name> (number){, transmitted <n> times (at <time>)}{, being retransmitted}{, released}{, fowarded <n> times}
    <op> (<number>), <n> bytes, number <n>, acking <n>, source idx <n>, dest idx <n>
    Words from <n>: <wordn> ... <wordn+m>
    String: <string>

Packet: to AI (2026), transmitted 27 times (at 1231232), being retransmitted
 CLS (11), 432 bytes, number 3422, acking 3221, source idx 177777, dest idx 177777
 Words from 0: 123123 12371 1227 272727 272626
 String: /"Now is the time for all good men/"

Packet: from MC (1440), released, forwarded 17 times
 DAT (201), 100 bytes, number 432, acking 102, source idx 123451, dest idx 123441
 Words from 0: 123123 64532
 String: /"FUKT!/"

"
  (LET ((TO-US (AND (ZEROP (PKT-TIMES-TRANSMITTED PKT))
		    (= (PKT-DEST-ADDRESS PKT) MY-ADDRESS)))
	(OTHER-HOST))
    (SETQ OTHER-HOST (IF TO-US
			 (PKT-SOURCE-ADDRESS PKT)
			 (PKT-DEST-ADDRESS PKT)))
    (LIST ()
      (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-PACKET-INSERT-HOSTAT)
	(TV:SCROLL-PARSE-ITEM
	  ':LEADER 4
	  `(:MOUSE-ITEM (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',OTHER-HOST 'TV:ITEM 0 ,INDENT)
			     :DOCUMENTATION "Menu of useful things to do to this host.")
	    :STRING ,(FORMAT NIL "~V@TPacket ~:[to~;from~] ~@[~A ~](~O)"
			     INDENT TO-US
			     ;; BV: Don't try to get name of address 0
			     (if (zerop other-host) 
				 "broadcast"
			       (SI:GET-HOST-FROM-ADDRESS OTHER-HOST ':CHAOS))
			     OTHER-HOST))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'PKT-TIMES-TRANSMITTED (,PKT)
			   NIL (", transmitted ~D times")))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'PKT-TIME-TRANSMITTED (,PKT) NIL (" (at ~O)")))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'PKT-BEING-RETRANSMITTED (,PKT)
			   NIL ("~:[, being retransmitted~;~]")))
	  `(:FUNCTION ,#'PKT-STATUS (,PKT) NIL ("~:[~;, Status: ~@*G~A~]"))
	  (AND TO-US
	       (FORMAT NIL ", fowarded ~D time~:P" (PKT-FWD-COUNT PKT)))))

      ;; Second line
      (LET ((OP (PKT-OPCODE PKT)))
       (TV:SCROLL-PARSE-ITEM
	 (FORMAT NIL
		 "~V@T~A (~O), ~O bytes, number ~O, acking ~O, source idx ~O, dest idx ~O"
		 INDENT
		 (IF ( OP DAT-OP)
		     "Data"
		   (NTH OP OPCODE-LIST))
		 OP
		 (PKT-NBYTES PKT)
		 (PKT-NUM PKT) (PKT-ACK-NUM PKT)
		 (PKT-SOURCE-INDEX-NUM PKT) (PKT-DEST-INDEX-NUM PKT))))
      (TV:SCROLL-PARSE-ITEM (FORMAT NIL "~V@T" INDENT) (PEEK-CHAOS-PKT-WORDS PKT 0 6))
      (TV:SCROLL-PARSE-ITEM (FORMAT NIL "~V@TString: " INDENT) (PEEK-CHAOS-PKT-STRING PKT)))))
))

(eval-when (load)
 (setf (aref chaos:routing-table-type 0) :chaos)
 (setq chaos:*receive-broadcast-packets-p* t))
