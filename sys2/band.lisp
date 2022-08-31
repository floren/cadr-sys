;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-

(DEFVAR BAND-TRANSFER-SERVER-ON ':NOTIFY
  "NIL - don't allow bands to be read or written.  T - do allow.  :NOTIFY - allow, but notify user.")

;Note: because of the way this transfers bytes between the network and
;the disk buffers, if you change the value of QUANTUM it will break; i.e.
;two machines with different values of quantum cannot send to each other.

(DEFUNP BAND-TRANSFER-SERVER (&AUX CONN PKT STR TEM RQB BUF WRITE-P (QUANTUM 17.) PART-NAME
				   PART-BASE PART-SIZE PART-COMMENT SUB-START SUB-N NB TOP
				   (WINDOW 36.))
  (ERRSET
    (UNWIND-PROTECT
      (PROG ()
	(SETQ CONN (CHAOS:LISTEN "BAND-TRANSFER" WINDOW))
	(IF (CHAOS:SYMBOLICS-CONNECTION-P CONN)
	    (RETURN (CHAOS:SYMBOLICS-REJECT CONN))
	  (SETQ STR (CHAOS:PKT-STRING (CHAOS:READ-PKTS CONN)))	;Look at the RFC
	  (LET ((*READ-BASE* 10.))	;RFC is BAND-TRANSFER READ/WRITE band subset size comment
				;subset is NIL or list of rel start and n-blocks
	    (SETQ TEM (READ-FROM-STRING (STRING-APPEND "(" STR ")"))))
	  (AND (NULL BAND-TRANSFER-SERVER-ON)
	       (NOT (MEMBER USER-ID '(NIL "")))
	       (RETURN (CHAOS:REJECT CONN (FORMAT NIL "This machine is in use by ~A" USER-ID))))
	  (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL PART-NAME)
	    (SYS:FIND-DISK-PARTITION (THIRD TEM)))
	  (OR PART-BASE
	      (RETURN (CHAOS:REJECT CONN (FORMAT NIL "No /"~A/" partition here." PART-NAME))))
	  (AND (FOURTH TEM) (SETQ SUB-START (FIRST (FOURTH TEM)) SUB-N (SECOND (FOURTH TEM))))
	  (COND ((STRING-EQUAL (SECOND TEM) "READ")
		 (SETQ WRITE-P NIL)
		 (SETQ PART-COMMENT (PARTITION-COMMENT PART-NAME 0)))
		((STRING-EQUAL (SECOND TEM) "WRITE")
		 (SETQ WRITE-P T)
		 (OR ( (FIFTH TEM) PART-SIZE)
		     (RETURN (CHAOS:REJECT CONN (FORMAT NIL "Partition too small, ~D>~D"
							(FIFTH TEM) PART-SIZE))))
		 (SETQ PART-SIZE (FIFTH TEM))
		 (SETQ PART-COMMENT (STRING (SIXTH TEM))))	;Comment to store later
		(T (RETURN (CHAOS:REJECT CONN "Illegal operation, must be READ or WRITE"))))
	  (AND SUB-START (OR (MINUSP SUB-START) (MINUSP SUB-N) (> (+ SUB-START SUB-N) PART-SIZE))
	       (RETURN (CHAOS:REJECT CONN "Subset outside of partition")))
	  (CHAOS:ACCEPT CONN)
	  (AND (EQ BAND-TRANSFER-SERVER-ON ':NOTIFY)
	       (PROCESS-RUN-FUNCTION "Notify" 'TV:NOTIFY NIL
				     "BAND-TRANSFER-SERVER: ~:[READ~;WRITE~] of ~A partition by ~A"
				     WRITE-P PART-NAME
				     (CHAOS:HOST-DATA (CHAOS:FOREIGN-ADDRESS CONN))))
	  (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':ADD-SERVER CONN "BAND-TRANSFER")
	  (COND ((NOT WRITE-P)			;Send packet containing size, comment
		 (SETQ PART-SIZE (MEASURED-SIZE-OF-PARTITION PART-NAME))
		 (SETQ PKT (CHAOS:GET-PKT))
		 (CHAOS:SET-PKT-STRING PKT (FORMAT NIL "~D ~S" PART-SIZE PART-COMMENT))
		 (CHAOS:SEND-PKT CONN PKT)))
	  (AND SUB-START (SETQ PART-BASE (+ PART-BASE SUB-START)
			       PART-SIZE SUB-N))
	  (COND (WRITE-P (UPDATE-PARTITION-COMMENT PART-NAME "Incomplete Copy" 0)))
	  (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
		BUF (SYS:RQB-BUFFER RQB))
	  (SETQ DISK-ERROR-RETRY-COUNT 20.)	;Try to bypass hardware overrun problem
	  (WIRE-DISK-RQB RQB)
	  (SETQ TOP (+ PART-BASE PART-SIZE))
	  (DO ((BLOCK PART-BASE (+ BLOCK QUANTUM)))
	      (( BLOCK TOP))
	    (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
		 (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
	    (COND ((NOT WRITE-P)		;This can modify pages without setting
		   (DISK-READ-WIRED RQB 0 BLOCK)	; the modified bits, but as long as
						; we dont depend on data after its unwired,
						; it wont hurt.
		   (ARRAY-TO-NET BUF CONN (* QUANTUM PAGE-SIZE 2)))
		  (T (ARRAY-FROM-NET BUF CONN (* QUANTUM PAGE-SIZE 2))
		     (DISK-WRITE-WIRED RQB 0 BLOCK))))
	  (CHAOS:FINISH-CONN CONN)
	  (CHAOS:CLOSE-CONN CONN "Done")
	  (AND WRITE-P (UPDATE-PARTITION-COMMENT PART-NAME PART-COMMENT 0))))
      (AND RQB (SYS:RETURN-DISK-RQB RQB))
      (AND CONN (PROGN (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':DELETE-SERVER CONN)
		       (CHAOS:REMOVE-CONN CONN))))
    NIL))

(DEFUN ARRAY-TO-NET (BUF CONN &OPTIONAL (NHWDS (ARRAY-LENGTH BUF)) (OPCODE 300)
		     &AUX PKT (N (TRUNCATE CHAOS:MAX-DATA-BYTES-PER-PKT 2)))
  (DO ((I 0 (+ I N)))
      (( I NHWDS))
    (SETQ N (MIN (- NHWDS I) N))
    (SETQ PKT (CHAOS:GET-PKT))
    (COPY-ARRAY-PORTION BUF I (+ I N)
			PKT CHAOS:FIRST-DATA-WORD-IN-PKT (+ CHAOS:FIRST-DATA-WORD-IN-PKT N))
    (SETF (CHAOS:PKT-NBYTES PKT) (* N 2))
    (CHAOS:SEND-PKT CONN PKT OPCODE)))

(DEFUN ARRAY-FROM-NET (BUF CONN &OPTIONAL (NHWDS (ARRAY-LENGTH BUF)) PKT PKT-OFFSET
		       &AUX N LIM BUFLIM)
  (DO ((I 0 LIM))
      (( I NHWDS)
       (AND ( I NHWDS)
	    (VALUES PKT PKT-OFFSET)))
    (OR PKT
	(SETQ PKT (CHAOS:GET-NEXT-PKT CONN)
	      PKT-OFFSET CHAOS:FIRST-DATA-WORD-IN-PKT))
    (SETQ N (- (+ CHAOS:FIRST-DATA-WORD-IN-PKT (FLOOR (CHAOS:PKT-NBYTES PKT) 2)) PKT-OFFSET)
	  LIM (+ I N)
	  BUFLIM (MIN NHWDS LIM))
    (COPY-ARRAY-PORTION PKT PKT-OFFSET (SETQ PKT-OFFSET (+ PKT-OFFSET (- BUFLIM I)))
			BUF I BUFLIM)
    (WHEN (= LIM BUFLIM)
      (CHAOS:RETURN-PKT PKT)
      (SETQ PKT NIL))))

(DEFUNP RECEIVE-BAND (FROM-MACHINE FROM-PART TO-PART
		      &OPTIONAL SUBSET-START SUBSET-N-BLOCKS
		      &AUX CONN PKT STR TEM RQB BUF (QUANTUM 17.) (WINDOW 36.)
		      NB TOP PART-BASE ORIG-PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
  "Read the FROM-PART partition from FROM-MACHINE into our partition TO-PART.
If SUBSET-START or SUBSET-N-BLOCKS is specified, they say which part
of the partition to transfer.  They are measured in blocks (or pages).
If a transfer dies, use the last number it printed, times 100.,
as SUBSET-START, to resume where it left off."
  ;; QUANTUM is number of disk blocks we write at once.
  ;; WINDOW is window size for net connection.
  ;; I think it is best if WINDOW is big enough to transfer a whole quantum.
 (OR SUBSET-START (SETQ SUBSET-START 0))
 (UNWIND-PROTECT
   (PROGN
     (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL TO-PART)
       (FIND-DISK-PARTITION-FOR-WRITE TO-PART))
     (WHEN PART-BASE
       (SETQ CONN (CHAOS:CONNECT FROM-MACHINE
				 (FORMAT NIL "BAND-TRANSFER READ ~A ~D"
					 FROM-PART (AND SUBSET-N-BLOCKS
							(LIST SUBSET-START SUBSET-N-BLOCKS)))
				 WINDOW))
       ;; Receive packet containing size and comment
       (SETQ PKT (CHAOS:GET-NEXT-PKT CONN)
	     STR (CHAOS:PKT-STRING PKT))
       (SETQ TEM (LET ((*READ-BASE* 10.)) (READ-FROM-STRING STR)))
       (OR ( TEM PART-SIZE)
	   (RETURN (FORMAT NIL "Does not fit in local partition, ~D>~D" TEM PART-SIZE)))
       (SETQ PART-SIZE TEM)
       (SETQ TEM (STRING-SEARCH-CHAR #/SP STR))
       (SETQ PART-COMMENT (READ-FROM-STRING STR NIL (1+ TEM)))
       (FORMAT T "~&Receiving ~A's ~A into ~A: ~D blocks, ~A~%"
	       FROM-MACHINE FROM-PART TO-PART PART-SIZE PART-COMMENT)
       (CHAOS:RETURN-PKT PKT)
       (SETQ ORIG-PART-BASE PART-BASE)
       (SETQ PART-BASE (+ PART-BASE SUBSET-START)
	     PART-SIZE (- PART-SIZE SUBSET-START))
       (AND SUBSET-N-BLOCKS (SETQ PART-SIZE SUBSET-N-BLOCKS))
       (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" 0)
       (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	     BUF (SYS:RQB-BUFFER RQB))
       (SETQ DISK-ERROR-RETRY-COUNT 20.)	;Try to bypass hardware overrun problem
       (WIRE-DISK-RQB RQB)
       (SETQ TOP (+ PART-BASE PART-SIZE))
       (DO ((BLOCK PART-BASE (+ BLOCK QUANTUM)))
	   (( BLOCK TOP))
	 (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
	      (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
	 (AND ( (SETQ TEM (TRUNCATE (- BLOCK ORIG-PART-BASE) 100.)) N-HUNDRED)
	      (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
	 (ARRAY-FROM-NET BUF CONN (* QUANTUM PAGE-SIZE 2))
	 (DISK-WRITE-WIRED RQB 0 BLOCK))
       (CHAOS:CLOSE-CONN CONN "Done")
       (OR SUBSET-N-BLOCKS (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT 0))))
   (AND RQB (SYS:RETURN-DISK-RQB RQB))
   (AND CONN (CHAOS:REMOVE-CONN CONN)))
 T)

(DEFUNP COMPARE-BAND (FROM-MACHINE FROM-PART TO-PART	
		      &OPTIONAL SUBSET-START SUBSET-N-BLOCKS
		      &AUX CONN PKT STR TEM RQB BUF BUF1 (QUANTUM 17.) NB TOP ORIG-PART-BASE
			   PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
  "Compare the FROM-PART partition from FROM-MACHINE with our partition TO-PART.
If SUBSET-START or SUBSET-N-BLOCKS is specified, they say which part
of the partition to compare.  They are measured in blocks (or pages).
If the operation dies, use the last number it printed, times 100.,
as SUBSET-START, to resume where it left off."
 (OR SUBSET-START (SETQ SUBSET-START 0))
 (UNWIND-PROTECT
   (PROGN
     (MULTIPLE-VALUE (PART-BASE PART-SIZE) (FIND-DISK-PARTITION-FOR-READ TO-PART))
     (SETQ CONN (CHAOS:CONNECT FROM-MACHINE
			       (FORMAT NIL "BAND-TRANSFER READ ~A ~D"
				       FROM-PART (AND SUBSET-N-BLOCKS
						      (LIST SUBSET-START SUBSET-N-BLOCKS)))
			       QUANTUM))
     ;; Receive packet containing size and comment
     (SETQ PKT (CHAOS:GET-NEXT-PKT CONN)
	   STR (CHAOS:PKT-STRING PKT))
     (SETQ TEM (LET ((*READ-BASE* 10.)) (READ-FROM-STRING STR)))
     (OR ( TEM PART-SIZE)
	 (RETURN (FORMAT NIL "Does not fit in local partition, ~D>~D" TEM PART-SIZE)))
     (SETQ PART-SIZE TEM)
     (SETQ TEM (STRING-SEARCH-CHAR #/SP STR))
     (SETQ PART-COMMENT (READ-FROM-STRING STR NIL (1+ TEM)))
     (FORMAT T "~&Comparing ~A with ~A from ~A: ~D blocks, ~A~%"
	     TO-PART FROM-PART FROM-MACHINE PART-SIZE PART-COMMENT)
     (CHAOS:RETURN-PKT PKT)
     (SETQ ORIG-PART-BASE PART-BASE)
     (SETQ PART-BASE (+ PART-BASE SUBSET-START)
	   PART-SIZE (- PART-SIZE SUBSET-START))
     (AND SUBSET-N-BLOCKS (SETQ PART-SIZE SUBSET-N-BLOCKS))
     (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	   BUF (SYS:RQB-BUFFER RQB)
	   BUF1 (MAKE-ARRAY (ARRAY-LENGTH BUF) ':TYPE 'ART-16B))
     (SETQ DISK-ERROR-RETRY-COUNT 20.)		;Try to bypass hardware overrun problem
     (WIRE-DISK-RQB RQB)
     (SETQ TOP (+ PART-BASE PART-SIZE))
     (DO ((BLOCK PART-BASE (+ BLOCK QUANTUM)))
	 (( BLOCK TOP))
       (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
	    (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
       (AND ( (SETQ TEM (TRUNCATE (- BLOCK ORIG-PART-BASE) 100.)) N-HUNDRED)
	    (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
       (ARRAY-FROM-NET BUF CONN (* QUANTUM PAGE-SIZE 2))
       (COND ((DISK-READ-COMPARE-WIRED RQB 0 BLOCK)
	      (COPY-ARRAY-CONTENTS BUF BUF1)
	      (DISK-READ-WIRED RQB 0 BLOCK)
	      (DO ((B BLOCK (1+ B)))
		  ((= B (+ BLOCK QUANTUM)))
		(DO ((I (* (- B BLOCK) 1000) (1+ I))
		     (N (* (1+ (- B BLOCK)) 1000))
		     (NDIFFS 0))
		    ((= I N)
		     (OR (ZEROP NDIFFS)
			 (LET ((SPT (AREF DISK-SECTORS-PER-TRACK-ARRAY 0))
			       (HPC (AREF DISK-HEADS-PER-CYLINDER-ARRAY 0)))
			   (FORMAT T "~&Block ~S (cyl ~O surf ~O sec ~O here, rel ~S) differs in ~D halfwords~%"
				   B (TRUNCATE B (* HPC SPT)) (TRUNCATE (\ B (* HPC SPT)) SPT)
				   (\ B SPT) (- B ORIG-PART-BASE) NDIFFS))))
		  (OR (= (AREF BUF I) (AREF BUF1 I)) (SETQ NDIFFS (1+ NDIFFS))))))))
     (CHAOS:CLOSE-CONN CONN "Done"))
   (AND RQB (SYS:RETURN-DISK-RQB RQB))
   (AND CONN (CHAOS:REMOVE-CONN CONN)))
 T)

(DEFUNP TRANSMIT-BAND (FROM-PART TO-MACHINE TO-PART
		       &OPTIONAL SUBSET-START SUBSET-N-BLOCKS
		       &AUX CONN TEM RQB BUF (QUANTUM 17.) NB TOP
			    PART-BASE ORIG-PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
  "Write the FROM-PART partition on FROM-MACHINE from our partition TO-PART.
If SUBSET-START or SUBSET-N-BLOCKS is specified, they say which part
of the partition to transfer.  They are measured in blocks (or pages).
If a transfer dies, use the last number it printed, times 100.,
as SUBSET-START, to resume where it left off."
 (UNWIND-PROTECT
   (PROGN
     (OR SUBSET-START (SETQ SUBSET-START 0))
     (MULTIPLE-VALUE (PART-BASE PART-SIZE) (FIND-DISK-PARTITION-FOR-READ FROM-PART))
     (SETQ PART-SIZE (MEASURED-SIZE-OF-PARTITION FROM-PART)
	   PART-COMMENT (PARTITION-COMMENT FROM-PART 0))
     (SETQ CONN (CHAOS:CONNECT TO-MACHINE
			       (FORMAT NIL "BAND-TRANSFER WRITE ~A ~D ~D ~S"
				       TO-PART
				       (AND SUBSET-N-BLOCKS
					    (LIST SUBSET-START SUBSET-N-BLOCKS))
				       PART-SIZE PART-COMMENT)))
     (FORMAT T "~&Transmitting ~A to ~A on ~A: ~D blocks, ~A~%"
	     FROM-PART TO-PART TO-MACHINE PART-SIZE PART-COMMENT)
     (SETQ ORIG-PART-BASE PART-BASE)
     (SETQ PART-BASE (+ PART-BASE SUBSET-START)
	   PART-SIZE (- PART-SIZE SUBSET-START))
     (AND SUBSET-N-BLOCKS (SETQ PART-SIZE SUBSET-N-BLOCKS))
     (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	   BUF (SYS:RQB-BUFFER RQB))
     (SETQ DISK-ERROR-RETRY-COUNT 20.)		;Try to bypass hardware overrun problem
     (WIRE-DISK-RQB RQB)
     (SETQ TOP (+ PART-BASE PART-SIZE))
     (DO ((BLOCK PART-BASE (+ BLOCK QUANTUM)))
	 (( BLOCK TOP))
       (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
	    (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
       (AND ( (SETQ TEM (TRUNCATE (- BLOCK ORIG-PART-BASE) 100.)) N-HUNDRED)
	    (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
       (DISK-READ-WIRED RQB 0 BLOCK)	;Modifies pages without setting modified bits.
					;This is ok since it remains wired while we care.
	 (ARRAY-TO-NET BUF CONN (* QUANTUM PAGE-SIZE 2)))
     (CHAOS:FINISH-CONN CONN)
     (CHAOS:CLOSE-CONN CONN "Done"))
   (AND RQB (SYS:RETURN-DISK-RQB RQB))
   (AND CONN (CHAOS:REMOVE-CONN CONN)))
 T)

(ADD-INITIALIZATION "BAND-TRANSFER"
		    '(PROCESS-RUN-FUNCTION
		       "BAND-TRANSFER Server" 'BAND-TRANSFER-SERVER)
		    NIL 'CHAOS:SERVER-ALIST)
