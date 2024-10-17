;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.7
;;; Reason:
;;;  Expunging zmail msg sequences no longer causes a jump to first message
;;;  in the sequence; instead, the user is left at the current message (if
;;;  still present after the expunge, or the one before it if not).
;;; Written 4-Oct-88 19:59:05 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.100, Experimental ZWEI 126.14, Experimental ZMail 74.6, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....)



; From modified file DJ: L.ZMAIL; COMNDS.LISP#596 at 4-Oct-88 19:59:05
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFUN EXPUNGE-ZMAIL-BUFFER (ZMAIL-BUFFER &OPTIONAL (DELETED-MSGS T)
				    &AUX ARRAY INFS (*INTERVAL* *INTERVAL*))
  "Expunge ZMAIL-BUFFER, removing all messages marked as deleted.
If DELETED-MSGS is NIL, only remove messages that have been removed from their owning buffers.
After you expunge a file buffer, you should expunge all temporary buffers
 with DELETED-MSGS = NIL, but this function does not do that for you."
  (COND ((ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
	 (SETQ *INTERVAL* (ZMAIL-DISK-BUFFER-INTERVAL ZMAIL-BUFFER))
	 (SETQ INFS (LOCF (NODE-INFERIORS *INTERVAL*)))
	 (FOREGROUND-BACKGROUND-FINISH ZMAIL-BUFFER)
	 (ZMAIL-BUFFER-DELETE-EXPIRED ZMAIL-BUFFER *DELETE-EXPIRED-MSGS*)
	 (AND *QUERY-BEFORE-EXPUNGE*
	      (ZMAIL-BUFFER-EXPUNGE-QUERY ZMAIL-BUFFER))))
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (DO ((NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (I 0 (1+ I))
       (MSG))
      (( I NMSGS)
       (COND ((AND (PLUSP NMSGS) (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER))
	      ;; Kludge for babyl, make the last line of the file correct maybe.
	      (SEND ZMAIL-BUFFER :UPDATE-MSG-END (AREF ARRAY (1- NMSGS))))
	     ((AND (ZEROP NMSGS) (NOT (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)))
	      ;; A temporary buffer goes away when it becomes empty.
	      (SEND ZMAIL-BUFFER :KILL))))
    ;; For speed, this does not call MSG-GET, which would parse the
    ;; message if need be.  If the message has never been parsed, it
    ;; probably isn't deleted.  Likewise, below it cannot be recent.
    (COND ((OR (EQ (MSG-PARSED-P (SETQ MSG (AREF ARRAY I))) :KILLED)
	       (AND DELETED-MSGS (GET (LOCF (MSG-STATUS MSG)) 'DELETED)))
	   (MSG-POINT-PDL-PURGE MSG (AND (NOT (EQ ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG)))
					 ZMAIL-BUFFER))
	   (IF (EQ ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG))
	       (LET ((REAL-INT (MSG-REAL-INTERVAL MSG)))
		 (UNLESS (EQ REAL-INT (CADR INFS))
		   (ZMAIL-ERROR "Node inferiors messed up, please report to BUG-ZMAIL"))
		 (DELETE-INTERVAL REAL-INT)
		 (UNCACHE-MSG-REFERENCES MSG)
		 ;; Now start really killing it.
		 (WITHOUT-INTERRUPTS
		   (RPLACD INFS (CDDR INFS))
		   (SETF (MSG-PARSED-P MSG) :KILLED)
		   (UNLESS (= (1+ I) NMSGS)
		     (%BLT (ALOC ARRAY (1+ I)) (ALOC ARRAY I)
			   (- NMSGS I 1) 1))
		   (DECF (FILL-POINTER ARRAY))
		   (DECF NMSGS)
		   (DECF I))	;Compensate for increment that DO-step will do.
		 ;; It's no disaster if these don't get done due to an abort.
		 (FLUSH-BP (INTERVAL-FIRST-BP REAL-INT))
		 (FLUSH-BP (INTERVAL-LAST-BP REAL-INT))
		 (LET ((INT (MSG-INTERVAL MSG)))
		   (FLUSH-BP (INTERVAL-FIRST-BP INT))
		   (FLUSH-BP (INTERVAL-LAST-BP INT))))
	     ;; Just remove the message from our array.
	     (WITHOUT-INTERRUPTS
	       (UNLESS (= (1+ I) NMSGS)
		 (%BLT (ALOC ARRAY (1+ I)) (ALOC ARRAY I)
		       (- NMSGS I 1) 1))
	       (DECF (FILL-POINTER ARRAY))
	       (DECF NMSGS)
	       (DECF I))))
	  (T
	   (COND ((EQ ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG))
		  (AND (REMPROP (LOCF (MSG-STATUS MSG)) 'RECENT)
		       (SETF (MSG-TICK MSG) (TICK)))
		  (SETQ INFS (CDR INFS)))))))
  (COND ((EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
	 (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
	 (ZMAIL-SELECT-MSG (COND ((ZEROP (ARRAY-ACTIVE-LENGTH ARRAY)) NIL)
				 ((OR (EQ (MSG-PARSED-P *MSG*) :KILLED)
				      (MSG-GET *MSG* 'DELETED))
				  ;;Expunging shouldn't cause a jump to first message 
				  ;;unless user is already at top of summary window.
				  (if (node-previous (msg-real-interval *msg*))
				      (1- *msg-no*)
				    0))
				 (T *MSG*))
			   T NIL))))

))
