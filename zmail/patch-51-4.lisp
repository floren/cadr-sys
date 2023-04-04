;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 51.4
;;; Reason: Mail file sort predicate bug.
;;; Written 9/21/83 14:56:22 by EB.TFC,
;;; while running on Lisp Machine One from band 8
;;; with System 97.6, CADR 1.0, ZMail 51.2, MIT-Specific 21.0, microcode 257, ZM MIT.



; From file MFILES.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

(DEFUN INSERT-NEW-MAIL (OLD-FILE NEW-FILE &AUX APPEND-P OLD-INT NEW-INT INT-APPEND-P)
  (COND ((GET (LOCF (ZMAIL-BUFFER-OPTIONS OLD-FILE)) ':REVERSE-NEW-MAIL)
	 (REVERSE-ZMAIL-BUFFER NEW-FILE)
	 ;; Select the lowest numbered message
	 (AND (EQ NEW-FILE *ZMAIL-BUFFER*)
	      (LET ((NMSGS (ZMAIL-BUFFER-NMSGS NEW-FILE)))
		(AND ( NMSGS 2)
		     (MUST-REDISPLAY *MSG-WINDOW*
			 (ZMAIL-SELECT-MSG (AREF (ZMAIL-BUFFER-ARRAY NEW-FILE) 0) NIL NIL)))))))
  (SETQ APPEND-P (ZMAIL-BUFFER-APPEND-P OLD-FILE)
	OLD-INT (ZMAIL-DISK-BUFFER-INTERVAL OLD-FILE)
	NEW-INT (ZMAIL-DISK-BUFFER-INTERVAL NEW-FILE))
  (LOCK-ZMAIL-BUFFER (OLD-FILE)
    (LOCK-ZMAIL-BUFFER (NEW-FILE)
      (IF (SETQ INT-APPEND-P (OR APPEND-P (ZEROP (ZMAIL-BUFFER-NMSGS OLD-FILE))))
	  (MULTIPLE-VALUE-BIND (END-LINE PREV-MSG-END-BPS)
	      (SEND OLD-FILE ':LAST-LINE-FOR-APPEND)
	    (IF END-LINE
		(LET ((START-LINE (BP-LINE (INTERVAL-FIRST-BP NEW-INT))))
		  (SETF (LINE-NEXT END-LINE) START-LINE)
		  (SETF (LINE-PREVIOUS START-LINE) END-LINE)
		  (DOLIST (BP PREV-MSG-END-BPS)
		    (MOVE-BP BP START-LINE 0)))
	      (MOVE-BP (INTERVAL-FIRST-BP OLD-INT) (INTERVAL-FIRST-BP NEW-INT)))
	    (MOVE-BP (INTERVAL-LAST-BP OLD-INT) (INTERVAL-LAST-BP NEW-INT)))
	(LET ((START-LINE (BP-LINE (SEND OLD-FILE ':FIRST-MSG-BP))))
	  (LET ((PREV (LINE-PREVIOUS START-LINE))
		(NEW-START-LINE (BP-LINE (INTERVAL-FIRST-BP NEW-INT))))
	    (SETF (LINE-PREVIOUS NEW-START-LINE) PREV)
	    (IF PREV
		(SETF (LINE-NEXT PREV) NEW-START-LINE)
	      (MOVE-BP (INTERVAL-FIRST-BP OLD-INT) NEW-START-LINE 0)))
	  (LET ((NEW-END-LINE (BP-LINE (INTERVAL-LAST-BP NEW-INT))))
	    (AND (ZEROP (LINE-LENGTH NEW-END-LINE))
		 (LINE-PREVIOUS NEW-END-LINE)
		 (SETQ NEW-END-LINE (LINE-PREVIOUS NEW-END-LINE)))
	    (SETF (LINE-NEXT NEW-END-LINE) START-LINE)
	    (SETF (LINE-PREVIOUS START-LINE) NEW-END-LINE))))
      (LET ((NEW-INFS (NODE-INFERIORS NEW-INT))
	    (OLD-INFS (NODE-INFERIORS OLD-INT))
	    LAST-INT FIRST-INT)
	(DOLIST (INT NEW-INFS)
	  (SETF (NODE-SUPERIOR INT) OLD-INT))
	(IF INT-APPEND-P
	    (SETQ LAST-INT (CAR (LAST OLD-INFS))
		  FIRST-INT (CAR NEW-INFS)
		  OLD-INFS (NCONC OLD-INFS NEW-INFS))
	    (SETQ LAST-INT (CAR (LAST NEW-INFS))
		  FIRST-INT (CAR OLD-INFS)
		  OLD-INFS (NCONC NEW-INFS OLD-INFS)))
	(SETF (NODE-INFERIORS OLD-INT) OLD-INFS)
	(COND ((AND LAST-INT FIRST-INT)
	       (LET* ((LAST-INT-END (INTERVAL-LAST-BP LAST-INT))
		      (FIRST-INT-START (INTERVAL-FIRST-BP FIRST-INT))
		      (LAST-INT-END-1 (INTERVAL-LAST-BP (CAR (NODE-INFERIORS LAST-INT))))
		      (MOVE-1-P (BP-= LAST-INT-END LAST-INT-END-1)))
		 (MOVE-BP LAST-INT-END FIRST-INT-START)
		 (AND MOVE-1-P (MOVE-BP LAST-INT-END-1 FIRST-INT-START))) 
	       (SETF (NODE-NEXT LAST-INT) FIRST-INT)
	       (SETF (NODE-PREVIOUS FIRST-INT) LAST-INT))))
      (LET* ((NEW-ARRAY (ZMAIL-BUFFER-ARRAY NEW-FILE))
	     (OLD-ARRAY (ZMAIL-BUFFER-ARRAY OLD-FILE))
	     (NMSGS (ARRAY-ACTIVE-LENGTH NEW-ARRAY))
	     (OLDLEN (ARRAY-ACTIVE-LENGTH OLD-ARRAY))
	     (NEWLEN (+ NMSGS OLDLEN)))
	(AND (< (ARRAY-LENGTH OLD-ARRAY) NEWLEN)
	     (ADJUST-ARRAY-SIZE OLD-ARRAY (TRUNCATE (* NEWLEN 5) 4)))
	(OR APPEND-P
	    ;; If prepending, make space in the array
	    (DO ((I (1- OLDLEN) (1- I))
		 (J (1- NEWLEN) (1- J)))
		((< I 0))
	      (ASET (AREF OLD-ARRAY I) OLD-ARRAY J)))
	(SETF (ARRAY-LEADER OLD-ARRAY 0) NEWLEN)
	(DO ((I 0 (1+ I))
	     (J (IF APPEND-P OLDLEN 0) (1+ J))
	     (MSG))
	    (( I NMSGS))
	  (SETQ MSG (AREF NEW-ARRAY I))
	  ;; It is important that the message be parsed by the inbox buffer, so that UNSEEN
	  ;; properties get put on.  That is why this MSG-PUT is before the :NEW-MSG.
	  (MSG-PUT MSG T 'RECENT)
	  (SEND OLD-FILE ':NEW-MSG MSG)
	  (ASET MSG OLD-ARRAY J))
	;; If this is new mail for a prepending BABYL file, and there is still some old mail
	;; to come in, the last new message won't get a formfeed.  Fix it now.
	(AND (NOT APPEND-P) (ZEROP OLDLEN) (PLUSP NEWLEN)
	     (EQ (ZMAIL-DISK-BUFFER-STATUS OLD-FILE) ':LOADING)
	     (LET* ((MSG (AREF NEW-ARRAY (1- NEWLEN)))
		    (LAST-BP (INTERVAL-LAST-BP OLD-INT))
		    (MSG-LAST-BP (MSG-REAL-END-BP MSG))
		    (AT-END-P (BP-= LAST-BP MSG-LAST-BP)))
	       (SEND OLD-FILE ':UPDATE-MSG-END MSG T)
	       (AND AT-END-P (MOVE-BP LAST-BP (END-LINE MSG-LAST-BP))))))))
  (COND ((EQ NEW-FILE *ZMAIL-BUFFER*)
	 (SELECT-ZMAIL-BUFFER OLD-FILE (EQ NEW-FILE *PRIMARY-ZMAIL-BUFFER*)))
	((EQ OLD-FILE *ZMAIL-BUFFER*)		;*MSG-NO* may need changing
	 (ZMAIL-SELECT-MSG *MSG* T NIL)))
  (MSG-POINT-PDL-FORWARD-ZMAIL-BUFFER NEW-FILE OLD-FILE))

))

; From file COMNDS.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFUN SORT-ZMAIL-BUFFER (ZMAIL-BUFFER MODE FORWARD-P &OPTIONAL NO-BACKGROUND-FINISH)
  (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
       (NOT NO-BACKGROUND-FINISH)
       (FOREGROUND-BACKGROUND-FINISH ZMAIL-BUFFER NIL))
  ;; Parse everything now, just in case.
  (DOMSGS (MSG ZMAIL-BUFFER)
    (ASSURE-MSG-PARSED MSG))
  (LET ((TEMPARRAY (SI:COPY-OBJECT (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))))
    ;; If the sort itself bombs out, we have not clobbered the buffer.
    (COND ((EQ MODE 'MSG-POSITION-LESSP)
	   ;; For sorting by position, record each message's mail file and position in advance.
	   ;; Then comparison is much faster.
	   (DOTIMES (I (LENGTH TEMPARRAY))
	     (SETF (AREF TEMPARRAY I)
		   (LIST (MSG-MAIL-FILE-BUFFER (AREF TEMPARRAY I))
			 (LOCATE-MSG-IN-ZMAIL-BUFFER (AREF TEMPARRAY I)
						     (MSG-MAIL-FILE-BUFFER (AREF TEMPARRAY I)))
			 (AREF TEMPARRAY I))))
	   (FUNCALL (IF FORWARD-P #'STABLE-SORT #'REVERSE-STABLE-SORT)
		    TEMPARRAY 'MSG-POSITION-LESSP-CACHED)
	   (DOTIMES (I (LENGTH TEMPARRAY))
	     (SETF (AREF TEMPARRAY I)
		   (CADDR (AREF TEMPARRAY I)))))
	  (T
	   (FUNCALL (IF FORWARD-P #'STABLE-SORT #'REVERSE-STABLE-SORT)
		    TEMPARRAY MODE))) 
    (COPY-ARRAY-CONTENTS TEMPARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)))
  (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
       (RESPLICE-ZMAIL-BUFFER ZMAIL-BUFFER))
  (COND ((EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
	 (FUNCALL *SUMMARY-WINDOW* ':NEED-FULL-REDISPLAY)
	 (ZMAIL-SELECT-MSG *MSG* NIL NIL))))

))

; From file MFILES.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

(DEFMETHOD (MAIL-FILE-BUFFER :LOADING-DONE) ()
  (AND STREAM (SEND STREAM ':CLOSE))
  (SETQ STREAM NIL)
  ;; If no new mail or new mail is not in yet, wait.
  (COND ((NULL ASSOCIATED-INBOX-BUFFER)
	 (SETQ STATUS NIL))
	((NEQ (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-INBOX-BUFFER) ':AWAITING-SAVE)
	 (SETQ STATUS ':AWAITING-NEW-MAIL))
	(T
	 ;; If new mail all in may need to append it now.
	 (AND (ZMAIL-BUFFER-APPEND-P SELF)
	      (INSERT-NEW-MAIL SELF ASSOCIATED-INBOX-BUFFER))
	 ;; Now ready to save back out.
	 (SETQ STATUS ':SAVING-REQUIRED)
	 (LET ((SORT (GET (LOCF OPTIONS) ':SORT)))
	   (AND SORT (SORT-ZMAIL-BUFFER SELF SORT
					(GET (LOCF OPTIONS) ':APPEND-P) T)))
	 (UNLESS *INHIBIT-BACKGROUND-SAVES*
	   (ZMAIL-BUFFER-BACKGROUND-SAVE SELF))))
  (ZMAIL-BACKGROUND-REQUEST-PUSH (LIST 'ZMAIL-BACKGROUND-PARSE-MSGS SELF 0))
  (AND (NEQ *MSG* ':NO-SELECT)
       (COMPUTE-CURRENT-MSG-NAME)))		;We may now know how many messages

))

; From file MFILES.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

;;; This is the default message loader.  Different formats provide two messages,
;;; :LINE-END-OF-MSG-P and :CANONICAL-LAST-LINE.
;;; :LINE-END-OF-MSG-P is passed a LINE, its LENGTH, a STATE, an EOF flag, and the START.
;;; START is the line on which this message started.
;;; It should return END-IDX and an updated STATE variable.
;;; END-IDX is T if this was the last line, or index of the end of the message
;;; within the line, or :START-NEXT if this line should also start next message, or NIL.
;;; If END-IDX is a number, it is the index of the end of the message's "contents"
;;; but the next message may not start until the following line.
;;; STATE is NIL for the first line of each message.
;;; Aside from this, the meaning of values of STATE is up to the
;;; individual format's :LINE-END-OF-MESSAGE-P method.
;;; :CANONICAL-LAST-LINE is called to make a dummy line for the end of
;;; the file if the normal format requires this.  This will default to
;;; an empty line.

(DEFMETHOD (ZMAIL-DISK-BUFFER :READ-NEXT-MSG) (&OPTIONAL (NMSGS 1) &AUX EOF)
  (LOCK-ZMAIL-BUFFER (SELF)
    (COND ((MEMQ STATUS '(:LOADING-NEW-MAIL :LOADING))
	   (WHEN STREAM
	     (LET* ((START (GET (LOCF OPTIONS) 'NEXT-MSG-START-LINE))
		    (LINE-WAITING-FLAG START))
	       (REMPROP (LOCF OPTIONS) 'NEXT-MSG-START-LINE)
	       (DO ((TEST-FUNCTION (GET-HANDLER-FOR SELF ':LINE-END-OF-MSG-P))
		    (END-LINE) (LINE) (LENGTH) (END-IDX)
		    (MSG-REAL-START-BP) (STATE))	;One piece of state for test function
		   (EOF)
		 (LET ((DEFAULT-CONS-AREA *ZMAIL-MSG-LINE-AREA*))
		   (IF LINE-WAITING-FLAG
		       (SETQ LINE (IF (EQ LINE-WAITING-FLAG T)
				      (LINE-NEXT LINE)
				    LINE-WAITING-FLAG)
			     LINE-WAITING-FLAG NIL)
		     (MULTIPLE-VALUE (LINE EOF)
		       (SEND STREAM ':LINE-IN LINE-LEADER-SIZE))))
		 (COND ((AND EOF (OR (NULL LINE) (ZEROP (LINE-LENGTH LINE))))
			(OR START (RETURN NIL))
			(SETQ LINE (FUNCALL SELF ':CANONICAL-LAST-LINE))))
		 (OR END-LINE (SETQ END-LINE (PROGN
					       (AND (NOT (ZEROP (BP-INDEX LAST-BP)))
						    (INSERT LAST-BP #\CR))
					       (BP-LINE LAST-BP))))
		 (UNLESS (LINE-NEXT LINE) (INSERT-LINE-WITH-LEADER LINE END-LINE))
		 (SETQ LENGTH (LINE-LENGTH LINE))
		 (AND (NULL START)
		      (PLUSP LENGTH)
		      (DO I 0 (1+ I) ( I LENGTH)
			  (OR (MEMQ (AREF LINE I) '(#\SP #\TAB))
			      (RETURN T)))
		      (SETQ START LINE))
		 (MULTIPLE-VALUE (END-IDX STATE)
		   (FUNCALL TEST-FUNCTION ':LINE-END-OF-MSG-P LINE LENGTH STATE EOF START))
		 (UNLESS (EQ END-LINE (LINE-NEXT LINE))
		   ;; :LINE-END-OF-MSG-P made a new line.
		   (SETQ LINE-WAITING-FLAG T))
		 (COND (END-IDX
			(SETQ MSG-REAL-START-BP (CREATE-BP START 0 ':NORMAL))
			(LET ((MSG-REAL-INTERVAL (CREATE-INTERVAL
						   MSG-REAL-START-BP
						   (CREATE-BP (IF (EQ END-IDX ':START-NEXT)
								  LINE
								(LINE-NEXT LINE))
							      0 ':MOVES)))
			      (MSG-INTERVAL (CREATE-INTERVAL
					      (COPY-BP MSG-REAL-START-BP ':NORMAL)
					      (COND ((EQ END-IDX T)
						     (CREATE-BP (LINE-NEXT LINE) 0 ':MOVES))
						    ((EQ END-IDX ':START-NEXT)
						     (CREATE-BP LINE 0 ':MOVES))
						    (T
						     (CREATE-BP LINE END-IDX ':MOVES))))))
			  (SETF (NODE-TICK MSG-INTERVAL) FILE-TICK)
			  (SETF (NODE-TICK MSG-REAL-INTERVAL) FILE-TICK)
			  (ARRAY-PUSH-EXTEND
			    ARRAY
			    (MAKE-MSG REAL-INTERVAL MSG-REAL-INTERVAL
				      INTERVAL MSG-INTERVAL
				      TICK FILE-TICK
				      MAIL-FILE-BUFFER SELF))
			  (DO ((LINE START (LINE-NEXT LINE))
			       (LAST (LINE-NEXT LINE)))
			      ((EQ LINE LAST))
			    (SETF (LINE-NODE LINE) MSG-REAL-INTERVAL))
			  (SETF (NODE-SUPERIOR MSG-INTERVAL) MSG-REAL-INTERVAL)
			  (SETF (NODE-INFERIORS MSG-REAL-INTERVAL) (LIST MSG-INTERVAL))
			  (SETF (NODE-SUPERIOR MSG-REAL-INTERVAL) SELF)
			  (LET ((INFS (NODE-INFERIORS SELF)))
			    (LET ((LAST (CAR (LAST INFS))))
			      (SETF (NODE-PREVIOUS MSG-REAL-INTERVAL) LAST)
			      (COND (LAST
				     (SETF (NODE-NEXT LAST) MSG-REAL-INTERVAL)
				     ;;The last-bp of the previous interval is :MOVES
				     ;;but should have stayed at the start of this one.
				     (LET ((LAST-BP-0 (INTERVAL-LAST-BP LAST))
					   (LAST-BP-1 (INTERVAL-LAST-BP
							(CAR (NODE-INFERIORS LAST)))))
				       (AND (BP-= LAST-BP-0 LAST-BP-1)
					    (MOVE-BP LAST-BP-1 MSG-REAL-START-BP))
				       (MOVE-BP LAST-BP-0 MSG-REAL-START-BP)))))
			    (SETQ INFERIORS
				  (NCONC INFS (NCONS MSG-REAL-INTERVAL)))))
			(COND (( (SETQ NMSGS (1- NMSGS)) 0)
			       (AND (EQ END-IDX ':START-NEXT)
				    (PUTPROP (LOCF OPTIONS) LINE 'NEXT-MSG-START-LINE))
			       (IF LINE-WAITING-FLAG
				   (PUTPROP (LOCF OPTIONS) (LINE-NEXT LINE)
					    'NEXT-MSG-START-LINE))
			       (RETURN NIL)))
			(SETQ START (IF (EQ END-IDX ':START-NEXT) LINE)
			      STATE NIL))))))
	   (IF (AND STREAM (NOT EOF))
	       T
	     (FUNCALL-SELF ':LOADING-DONE)
	     (AND STREAM (PLUSP NMSGS) (NOT *ZMAIL-BACKGROUND-P*)
		  (FUNCALL-SELF ':READ-NEXT-MSG NMSGS)))))))

))