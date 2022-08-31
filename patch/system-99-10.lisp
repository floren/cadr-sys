;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.10
;;;Reason:
;;; time parser accepts ISO formats. eg 1966-10-27 and 1966-Oct-27
;;; Chaos unknown host doesn't blow out when arg not a string
;;; time:*daylight-savings-time-p-function*
;;; new date-print-modes: "yyyy" sequence to go with "yy" sequence: dd//mm//yyyy, etc
;;; inserting characters objects in zmacs
;;; zwei:insert-moving takes optional start and end substring args
;;; macros may expand into declarations (in compiler)
;;; documentation declaration
;;; m-x Add patch records source file version number
;;; #| reader macro eof bug
;;; set-syntax-from-char, set-macro-character, etc, etc hack character objects
;;; reduce bug
;;; Bug reading zmacs register names
;;; :tyoing characters to sheets
;;; sheets clip lozenged strings more winningly
;;; File resume handler doesn't blow out if given new pathname
;;; Activity string returned by NAME server
;;;   Other random cruft
;;; Written 20-Oct-84 10:08:45 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.9, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, Experimental Macsyma 1.0, microcode 320, GC@2.



; From file PATHST.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHST  "

(DEFUN DEFINE-SYS-LOGICAL-DEVICE (&OPTIONAL IGNORE)
  (MAKE-LOGICAL-PATHNAME-HOST "SYS" :WARN-ABOUT-REDEFINITION NIL))

))

(dolist (x fs::*logical-pathname-host-list*)
  (when (get x 'fs:make-logical-pathname-host)
    (setf (get x 'fs:make-logical-pathname-host) '(nil . nil)))
  (si::load-patches-for-logical-pathname-hosts))


; From file TIMPAR.LISP OZ:<L.IO1> OZ:
#10R TIME#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIMPAR  "

;;; ISO formats
;;; 1980-3-15 means 15 March, 1980.
(DEFPATTERN MAIN ((FIXP 4) - (FIXP 1 2) - (FIXP 1 2)) MAIN
	    (YEAR MONTH DATE)
  (SET-MONTH MONTH)
  (SET-DATE DATE)
  (SET-YEAR YEAR))
;;; 1980-MAR-15 means 15 March, 1980.
(DEFPATTERN MAIN ((FIXP 4) - (GET MONTH) - (FIXP 1 2)) MAIN
	    (YEAR MONTH DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE)
  (SET-YEAR YEAR))


))


; From file PATED LISP MLY; MC:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN ADD-PATCH-BUFFER-CHANGED-FUNCTIONS (BUFFER)
  (LET (PROCEED-FLAG)
    (RESECTIONIZE-BUFFER BUFFER)
    (DOLIST (SECTION (NODE-INFERIORS BUFFER))
      (AND (TYPEP SECTION 'SECTION-NODE)
	   (SECTION-NODE-DEFUN-LINE SECTION)
	   (LET ((PATCH-TICK (GET SECTION 'PATCH-TICK)))
	     (> (NODE-TICK SECTION) (OR PATCH-TICK (BUFFER-FILE-READ-TICK BUFFER))))
	   (LET ((NAME (SECTION-NODE-NAME SECTION)))
	     (WHEN (OR PROCEED-FLAG
		       (CASE (FQUERY '(:CHOICES
					(((:PROCEED "Proceed.") #/P)
					 ((:QUIT "Quit") #/Q)
					 . #,FORMAT:Y-OR-N-P-CHOICES))
				     "Add ~S to patch? " NAME)
			 (:PROCEED (SETQ PROCEED-FLAG T))
			 (:QUIT (RETURN-FROM ADD-PATCH-BUFFER-CHANGED-FUNCTIONS :QUIT))
			 ((T) T)))
	       (ADD-PATCH-INTERVAL SECTION NIL T NAME BUFFER))))))
  NIL)


))

; From file PATED LISP MLY; MC:
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN FINISH-PATCH (RELEASE-FLAG)
  (VALIDATE-PATCH-BUFFER)
  (OR *PATCH-BUFFER*
      (BARF "There is no current patch buffer"))
  (LET ((DESCRIPTION (TYPEIN-LINE-MULTI-LINE-READLINE "Description of changes (end with End)")))
    (SETQ DESCRIPTION (STRING-TRIM '(#/NEWLINE) DESCRIPTION))
    (LET ((BP (FORWARD-LINE (INTERVAL-FIRST-BP *PATCH-BUFFER*) 2)))
      (INSERT-MOVING BP ";;; Reason:")
      (INSERT-MOVING BP #/NEWLINE)
      (DO ((START 0 (1+ NEXT-LINE))
	   NEXT-LINE)
	  (())
	(SETQ NEXT-LINE (STRING-SEARCH-CHAR #/NEWLINE DESCRIPTION START))
	(INSERT-MOVING BP ";;;  ")
	(INSERT-MOVING BP DESCRIPTION START NEXT-LINE)
	(INSERT-MOVING BP #/NEWLINE)
	(OR NEXT-LINE (RETURN))))
    (SAVE-BUFFER-IF-NECESSARY *PATCH-BUFFER*)
    (AND (EQ *PATCH-BUFFER* *INTERVAL*)
	 (MUST-REDISPLAY *WINDOW* DIS-TEXT))
    (LET ((ERROR-MESSAGE (IF *PATCH-SYSTEM*
			     (SI::CONSUMMATE-PATCH *PATCH-SYSTEM* *PATCH-NUMBER*
						   DESCRIPTION RELEASE-FLAG)
			   (COMPILE-FILE (BUFFER-PATHNAME *PATCH-BUFFER*))
			   NIL)))
      (COND ((ERRORP ERROR-MESSAGE)
	     (BARF ERROR-MESSAGE))
	    (T
	     (IF (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
		       THEREIS (BUFFER-NEEDS-SAVING-P BUFFER))
		 (FORMAT *QUERY-IO* "~&Don't forget to save your files!")
	       (FORMAT *QUERY-IO* "~&Patch completed."))
	     (SETQ *PATCH-BUFFER* NIL))))))

))

; From file MENU.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MENU  "

(DEFSETF GEOMETRY-FILL-P (GEO) (VALUE)
  `(SETF (GEOMETRY-N-COLUMNS ,GEO) (IF ,VALUE 0 NIL)))

))

; From file CHUSE.LISP OZ:<L.NETWORK.CHAOS> OZ:
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHUSE  "

(DEFUN CHAOS-UNKNOWN-HOST-FUNCTION (NAME)
  (DOLIST (HOST (SI:GET-SITE-OPTION :CHAOS-HOST-TABLE-SERVER-HOSTS))
    (AND (SI:PARSE-HOST HOST T ())		; prevent infinite recursion
	 (WITH-OPEN-STREAM (STREAM (OPEN-STREAM HOST "HOSTAB" :ERROR NIL))
	   (SETQ NAME (STRING NAME))
	   (UNLESS (ERRORP STREAM)
	     (SEND STREAM :LINE-OUT NAME)
	     (SEND STREAM :FORCE-OUTPUT)
	     (DO ((LIST NIL) (RESULT) (DONE)
		  (LINE) (EOF)
		  (LEN) (SP) (PROP))
		 (DONE RESULT)
	       (MULTIPLE-VALUE (LINE EOF) (SEND STREAM :LINE-IN))
	       (IF EOF
		   (SETQ RESULT (WHEN LIST
				  (PUTPROP LIST (STABLE-SORT (GET LIST :HOST-NAMES)
							     #'(LAMBDA (X Y)
								 (< (STRING-LENGTH X)
								    (STRING-LENGTH Y))))
					   :HOST-NAMES)
				  (APPLY #'SI:DEFINE-HOST LIST))
			 DONE T)
		 (SETQ LEN (STRING-LENGTH LINE)
		       SP (STRING-SEARCH-CHAR #/SP LINE 0 LEN))
		 (SETQ PROP (INTERN (SUBSTRING LINE 0 SP) ""))
		 (INCF SP)
		 (CASE PROP
		   (:ERROR (SETQ DONE T))
		   (:NAME
		    (LET ((NAME (SUBSTRING LINE SP LEN)))
		      (OR LIST (SETQ LIST (NCONS NAME)))
		      (PUSH NAME (GET LIST :HOST-NAMES))))
		   ((:SYSTEM-TYPE MACHINE-TYPE)
		    (PUTPROP LIST (INTERN (SUBSTRING LINE SP LEN) "") PROP))
		   (OTHERWISE
		    (LET ((FUNCTION (GET PROP 'HOST-ADDRESS-PARSER)))
		      (OR FUNCTION (SETQ FUNCTION (GET :CHAOS 'HOST-ADDRESS-PARSER)))
		      (PUSH (FUNCALL FUNCTION PROP LINE SP LEN)
			    (GET LIST PROP)))))))
	     (RETURN T))))))

))


; From file TIME.LISP OZ:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFVAR *DAYLIGHT-SAVINGS-TIME-P-FUNCTION* 'DAYLIGHT-SAVINGS-TIME-IN-NORTH-AMERICA-P
  "A function, which when applied to arguments of seconds minutes hours day month year,
will return T if daylight savings time is in effect in the local timezonew at that time.")

(DEFVAR *DEFAULT-DATE-PRINT-MODE* :MM//DD//YY	;perhaps site variable?
  "Defines the default way to print the date. Possible values include:
:DD//MM//YY :MM//DD//YY :DD-MM-YY :DD-MMM-YY :|DD MMM YY| :DDMMMYY :YYMMDD :YYMMMDD
 and similar keywords with YYYY instead of YY.")

(DEFPROP :DD//MM//YYYY "~D//~2,'0D~*~:[//~*~D~]" DATE-FORMAT)		;27/10{/1966}
(DEFPROP :MM//DD//YYYY "~*~D//~0@*~2,'0D~2*~:[//~*~D~]" DATE-FORMAT)	;10/27{/1966}
(DEFPROP :DD-MM-YYYY "~D-~2,'0D~*~:[-~*~D~]" DATE-FORMAT)		;27-10{-1966}
(DEFPROP :DD-MMM-YYYY "~D-~*~A~:[-~*~D~]" DATE-FORMAT)			;27-Oct{-1966}
(DEFPROP :DD/ MMM/ YYYY "~D ~*~A~:[ ~*~D~]" DATE-FORMAT)		;27 Oct{ 1966}
(DEFPROP :DDMMMYYYY "~D~*~A~:[~*~D~]" DATE-FORMAT)			;27Oct{1966}
(DEFPROP :YYYYMMDD "~5*~2,'0D~1@*~2,'0D~0@*~2,'0D" DATE-FORMAT)		;19661027
(DEFPROP :YYYYMMMDD "~3*~:[~*~D~]~2@*~A~0@*~2,'0D" DATE-FORMAT)		;{1966}Oct27
(DEFPROP :YY-MMM-DD "~3*~:[~2,'0D-~]~2@*~A-~0@*~2,'0D" DATE-FORMAT)	;{66-}Oct-27
(DEFPROP :YYYY-MMM-DD "~3*~:[~*~D-~]~2@*~A-~0@*~2,'0D" DATE-FORMAT)	;{1966-}Oct-27
(DEFPROP :YY-MM-DD "~3*~:[~2,'0D-~]~1@*~A-~0@*~2,'0D" DATE-FORMAT)	;{66-}10-27
(DEFPROP :YYYY-MM-DD "~3*~:[~*~D-~]~1@*~A-~0@*~2,'0D" DATE-FORMAT)	;{1966-}10-27

(DEFUN YYYY-YY (YEAR CURRENT-YEAR)
  (IF (= (TRUNCATE (+ YEAR 50.) 100.) (TRUNCATE (+ CURRENT-YEAR 50.) 100.))
      (MOD YEAR 100.)
    YEAR))

(DEFUN PRINT-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
		   &OPTIONAL (STREAM *STANDARD-OUTPUT*)
			     (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*)) 
  "Print time specified on STREAM using date format DATE-PRINT-MODE.
If STREAM is NIL, construct and return a string."
  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH ':SHORT)
				   NIL (MOD YEAR 100.) YEAR)
    (FORMAT STREAM "~? ~2,'0D:~2,'0D:~2,'0D"
	    (OR (GET DATE-PRINT-MODE 'DATE-FORMAT)
		(FERROR NIL "Bad type of DATE-PRINT-MODE: ~S" DATE-PRINT-MODE))
	    DATE-MODE-ARGS
	    HOURS MINUTES SECONDS)))


(DEFUN PRINT-BRIEF-UNIVERSAL-TIME (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*)
						(REF-UT (GET-UNIVERSAL-TIME))
						(DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE REF-DAY REF-MONTH REF-YEAR)
	(DECODE-UNIVERSAL-TIME REF-UT)
      ;; If not same day, print month and day numerically
      (IF (OR ( DAY REF-DAY) ( MONTH REF-MONTH) ( YEAR REF-YEAR))
	  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH :SHORT)
					   (= YEAR REF-YEAR) (YYYY-YY YEAR REF-YEAR) YEAR)
	    (FORMAT STREAM "~? ~2,'0D:~2,'0D"
		    (OR (GET DATE-PRINT-MODE 'DATE-FORMAT)
			(FERROR NIL "Bad type-of DATE-PRINT-MODE: ~S" DATE-PRINT-MODE))
		    DATE-MODE-ARGS
		    HOURS MINUTES))
	;; Always print hours colon minutes, even if same as now
	(FORMAT STREAM "~2,'0D:~2,'0D" HOURS MINUTES)))))


(DEFUN DECODE-UNIVERSAL-TIME (UNIVERSAL-TIME &OPTIONAL TIMEZONE
					     &AUX SECS MINUTES HOURS DAY MONTH YEAR
						  DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P)
  "Given a UNIVERSAL-TIME, decode it into year, month number, day of month, etc.
TIMEZONE is hours before GMT (5, for EST).
DAY and MONTH are origin-1.  DAY-OF-THE-WEEK = 0 for Monday."
  (DECLARE (VALUES SECS MINUTES HOURS DAY MONTH YEAR
		   DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P TIMEZONE))
  (IF TIMEZONE					;explicit timezone means no-dst
      (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	 (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME TIMEZONE))
    ;;Otherwise, decode the time and THEN daylight-adjust it.
    (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME *TIMEZONE*))
    (AND (SETQ DAYLIGHT-SAVINGS-TIME-P
	       (FUNCALL *DAYLIGHT-SAVINGS-TIME-P-FUNCTION*
			SECS MINUTES HOURS DAY MONTH YEAR))
	 ;; See if it's daylight savings time, time-zone number gets smaller if so.
	 (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	   (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME (1- *TIMEZONE*)))))
  (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P
	  (OR TIMEZONE *TIMEZONE*)))


(DEFUN DAYLIGHT-SAVINGS-TIME-P (&REST ARGS)
  "T if daylight savings time would be in effect at specified time in the local timezone."
  (DECLARE (ARGLIST HOURS DAY MONTH YEAR))
  (APPLY *DAYLIGHT-SAVINGS-TIME-P-FUNCTION* 0 0 ARGS))

(DEFUN DAYLIGHT-SAVINGS-TIME-IN-NORTH-AMERICA-P (SECONDS MINUTES HOURS DAY MONTH YEAR)
  "T if daylight savings time would be in effect at specified time in North America."
  (DECLARE (IGNORE SECONDS MINUTES))
  (COND ((OR (< MONTH 4)		;Standard time if before 2 am last Sunday in April
	     (AND (= MONTH 4)
		  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
		    (OR (< DAY LSA)
			(AND (= DAY LSA) (< HOURS 2))))))
	 NIL)
	((OR (> MONTH 10.)		;Standard time if after 1 am last Sunday in October
	     (AND (= MONTH 10.)
		  (LET ((LSO (LAST-SUNDAY-IN-OCTOBER YEAR)))
		    (OR (> DAY LSO)
			(AND (= DAY LSO) ( HOURS 1))))))
	 NIL)
	(T T)))

))

; From file INSERT.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; INSERT  "

(DEFUN ASSURE-STRING (STRING)
  "Coerce STRING into a string, or a fat-string if given a number >= CHAR-CODE-LIMIT"
;character lossage
  (IF (CHARACTERP STRING) (SETQ STRING (CHAR-INT STRING)))
  (COND ((ARRAYP STRING) STRING)
	((AND (NUMBERP STRING) ( STRING CHAR-CODE-LIMIT))
	 (LET ((NEW-STRING (MAKE-ARRAY 1 :TYPE 'ART-FAT-STRING)))
	   (SETF (AREF NEW-STRING 0) STRING)
	   NEW-STRING))
	(T (STRING STRING))))


;>> this should be a little smarter about line breaks.
;>> If we insert a newline at position two of a line, perhaps a
;>> better strategy than leaving the old line with just one character in it
;>> and consing up a whole new array for the n-2 characters left, would be to
;>> put the short part of the line in a new, shorter string and shift the rest
;>> of the line backwards. I should do some metering.
;>> For example, we ===REALLY=== lose big doing something like replacing all the
;>> linefeeds in a file with newlines when the file had no linebreaks in it
;>> originally -- can get this sort of lossage cftping of un*x hosts, etc

;;; Insert the STRING at the BP.
(DEFUN INSERT (BP STRING &OPTIONAL (START 0) END
	       &AUX LINE INDEX LINE-LENGTH FIRST-NEWLINE FIRST-LINE LAST-LINE)
  "Insert a copy of STRING, or the part of it from START to END, into text at BP.
STRING can actually be a string or a character.
START and END are allowed only with strings.
BP is left pointing before the inserted text, unless it is of type :MOVES.
The value is a BP pointing after the inserted text."
  (AND (NOT *BATCH-UNDO-SAVE*) *UNDO-SAVE-SMALL-CHANGES*
       (UNDO-SAVE-NEW-SMALL-CHANGE BP BP))
  (MUNG-BP-INTERVAL BP)
  (SETQ LINE (BP-LINE BP)
	INDEX (BP-INDEX BP)
	LINE-LENGTH (LINE-LENGTH LINE))
;character lossage
  (IF (FIXNUMP STRING) (SETQ STRING (INT-CHAR STRING)))
  (COND ((ARRAYP STRING)
	 (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
	 (SETQ FIRST-NEWLINE (%STRING-SEARCH-CHAR #/NEWLINE STRING START END))
	 (COND ((NULL FIRST-NEWLINE)
		;; The string doesn't have any newlines in it.
		(INSERT-WITHIN-LINE LINE INDEX STRING START END))
	       (T
		;; First, construct the "last" line, which is made up of the last
		;; line of the STRING followed by the part of LINE after INDEX.
		;; The copy the first line of STRING into LINE.
		(LET* ((LAST-NEWLINE (STRING-REVERSE-SEARCH-CHAR #/NEWLINE STRING
								 END START))
		       (ARRAY-TYPE (IF (EQ (ARRAY-TYPE STRING) 'ART-FAT-STRING)
				       'ART-FAT-STRING (ARRAY-TYPE LINE)))
		       (LCHARS (- END LAST-NEWLINE 1)))
		  (COND ((AND (= LAST-NEWLINE (1- END))
			      (ZEROP INDEX))
			 ;; Inserting stuff ending with CR at front of line
			 ;; implies we can just shove down the old line
			 (SETQ LAST-LINE LINE)
			 ;; But then we can't use it as the first line.
			 (SETQ FIRST-LINE (CREATE-LINE ARRAY-TYPE
						       (- FIRST-NEWLINE START)
						       (BP-NODE BP)))
			 (SETF (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS LINE))
			 (AND (LINE-PREVIOUS LINE)
			      (SETF (LINE-NEXT (LINE-PREVIOUS LINE)) FIRST-LINE))
			 (COPY-ARRAY-PORTION STRING START FIRST-NEWLINE
					     FIRST-LINE 0 (ARRAY-LENGTH FIRST-LINE))
			 ;; Transfer bps from the front of LINE to FIRST-LINE.
			 (DOLIST (BP (LINE-BP-LIST LINE))
			   (AND (ZEROP (BP-INDEX BP))
				(EQ (BP-STATUS BP) ':NORMAL)
				(MOVE-BP BP FIRST-LINE 0))))
			(T
			 ;; Otherwise, keep the beginning of the line we are inserting in,
			 ;; and make a new line for the tail end of the string.
			 (SETQ FIRST-LINE LINE)
			 (SETQ LAST-LINE (CREATE-LINE ARRAY-TYPE
						      (+ LCHARS (- LINE-LENGTH INDEX))
						      (BP-NODE BP)))
			 ;; Copy the last line of STRING into LAST-LINE.
			 (COPY-ARRAY-PORTION STRING (1+ LAST-NEWLINE) END
					     LAST-LINE 0 LCHARS)
			 ;; Copy the part of LINE after INDEX into LAST-LINE
			 (COPY-ARRAY-PORTION LINE INDEX LINE-LENGTH
					     LAST-LINE LCHARS (ARRAY-LENGTH LAST-LINE))
			 ;; Figure out whether LINE is being changed at all.
			 (OR (AND (= FIRST-NEWLINE START)
				  (= INDEX LINE-LENGTH))
			     (MUNG-LINE LINE))
			 ;; Copy the first line of STRING into LINE.
			 (SET-LINE-LENGTH LINE (+ INDEX (- FIRST-NEWLINE START)))
			 (OR (EQ ARRAY-TYPE (ARRAY-TYPE LINE))
			     (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING))
			 (COPY-ARRAY-PORTION STRING START FIRST-NEWLINE
					     LINE INDEX (LINE-LENGTH LINE))
			 ;; Relocate buffer pointers.
			 (DOLIST (BP (LINE-BP-LIST LINE))
			   (LET ((I (BP-INDEX BP)))
			     (COND ((OR (> I INDEX)
					(AND (= I INDEX)
					     (EQ (BP-STATUS BP) ':MOVES)))
				    (MOVE-BP BP LAST-LINE (+ (- I INDEX) LCHARS))))))))
		  (DO ((PREV-LINE FIRST-LINE THIS-LINE)
		       (THIS-LINE)
		       (PREV-NEWLINE FIRST-NEWLINE NEWLINE)
		       (NEWLINE)
		       (THE-LINE-BEYOND (LINE-NEXT LINE)))
		      (NIL)
		    (COND ((= PREV-NEWLINE LAST-NEWLINE)
			   ;; We are at the end.
			   (AND THE-LINE-BEYOND
				(SETF (LINE-PREVIOUS THE-LINE-BEYOND) LAST-LINE))
			   (SETF (LINE-NEXT LAST-LINE) THE-LINE-BEYOND)
			   (SETF (LINE-NEXT PREV-LINE) LAST-LINE)
			   (SETF (LINE-PREVIOUS LAST-LINE) PREV-LINE)
			   (RETURN NIL)))
		    (SETQ NEWLINE (%STRING-SEARCH-CHAR #/NEWLINE STRING
						       (1+ PREV-NEWLINE) END))
		    (LET ((LENGTH (- NEWLINE PREV-NEWLINE 1)))
		      (SETQ THIS-LINE (CREATE-LINE (ARRAY-TYPE STRING) LENGTH (BP-NODE BP)))
		      (COPY-ARRAY-PORTION STRING (1+ PREV-NEWLINE) NEWLINE
					  THIS-LINE 0 LENGTH)
		      (SETF (LINE-NEXT PREV-LINE) THIS-LINE)
		      (SETF (LINE-PREVIOUS THIS-LINE) PREV-LINE)))
		  (CREATE-BP LAST-LINE LCHARS)))))
	;; These are for INSERT of a non-string
	((EQ STRING #/NEWLINE)
	 ;; Breaking a line.
	 (COND ((ZEROP INDEX)
		;; Shove down the old line if inserting a CR at its front
		(SETQ FIRST-LINE (CREATE-LINE 'ART-STRING 0 (BP-NODE BP)))
		(SETF (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS LINE))
		(AND (LINE-PREVIOUS LINE)
		     (SETF (LINE-NEXT (LINE-PREVIOUS LINE)) FIRST-LINE))
		(SETF (LINE-NEXT FIRST-LINE) LINE)
		(SETF (LINE-PREVIOUS LINE) FIRST-LINE)
		;; Transfer bps from the front of LINE to FIRST-LINE.
		(DOLIST (BP (LINE-BP-LIST LINE))
		  (AND (ZEROP (BP-INDEX BP))
		       (EQ (BP-STATUS BP) ':NORMAL)
		       (MOVE-BP BP FIRST-LINE 0)))
		(CREATE-BP LINE 0))
	       (T
		;; Otherwise, keep the beginning of the line we are inserting in,
		;; and make a new line for the tail end of the string.
		(SETQ LAST-LINE (CREATE-LINE (ARRAY-TYPE LINE) (- LINE-LENGTH INDEX)
					     (BP-NODE BP)))
		(SETF (LINE-NEXT LAST-LINE) (LINE-NEXT LINE))
		(AND (LINE-NEXT LINE)
		     (SETF (LINE-PREVIOUS (LINE-NEXT LINE)) LAST-LINE))
		(SETF (LINE-NEXT LINE) LAST-LINE)
		(SETF (LINE-PREVIOUS LAST-LINE) LINE)
		;; Copy the part of LINE after INDEX into LAST-LINE
		(COPY-ARRAY-PORTION LINE INDEX LINE-LENGTH
				    LAST-LINE 0 (ARRAY-LENGTH LAST-LINE))
		;; Figure out whether LINE is being changed at all.
		(OR (= INDEX LINE-LENGTH)
		    (MUNG-LINE LINE))
		;; Truncate LINE
		(SET-LINE-LENGTH LINE INDEX)
		;; Relocate buffer pointers.
		(DOLIST (BP (LINE-BP-LIST LINE))
		  (LET ((I (BP-INDEX BP)))
		    (COND ((OR (> I INDEX)
			       (AND (= I INDEX)
				    (EQ (BP-STATUS BP) ':MOVES)))
			   (MOVE-BP BP LAST-LINE (- I INDEX))))))
		(CREATE-BP LAST-LINE 0))))
	;; Insert ordinary character -- code simplified from INSERT-WITHIN-LINE
	((CHARACTERP STRING)
	 (AND (GETF (LINE-PLIST LINE) ':DIAGRAM) (BARF "Diagram line"))
	 (SET-LINE-LENGTH LINE (1+ LINE-LENGTH))
	 (IF (AND ( STRING CHAR-CODE-LIMIT)
		  (NEQ (ARRAY-TYPE LINE) 'ART-FAT-STRING))
	     (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING))
	 ;; Move the characters ahead of the inserting forward.
	 (DO ((LF (1- LINE-LENGTH) (1- LF))
	      (LT LINE-LENGTH (1- LT)))
	     ((< LF INDEX))
	   (SETF (CHAR LINE LT) (CHAR LINE LF)))
	 ;; Insert the new characters into the line.
	 (SETF (CHAR LINE INDEX) STRING)
	 ;; Relocate buffer pointers.
	 (DOLIST (BP (LINE-BP-LIST LINE))
	   (LET ((I (BP-INDEX BP)))
	     (COND ((OR (> I INDEX)
			(AND (= I INDEX)
			     (EQ (BP-STATUS BP) ':MOVES)))
		    (SETF (BP-INDEX BP) (1+ I))))))
	 (MUNG-LINE LINE)
	 (CREATE-BP LINE (1+ INDEX)))
	;; Inserting something random
	(T (INSERT BP (STRING STRING) START END))))

(DEFUN INSERT-MOVING (BP STRING &OPTIONAL (START 0) END)
  "Insert STRING at BP, and relocate BP to point after the inserted text.
STRING can be a string or a character.
The value is a temporary buffer pointer also pointing after the inserted text
but not EQ to BP."
  (LET ((NBP (INSERT BP STRING START END)))
    (MOVE-BP BP NBP)
    NBP))

))


; From file QFCTNS.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFPROP ARGLIST T DEBUG-INFO)
(DEFPROP :ARGLIST ARGLIST DEBUG-INFO)
(DEFPROP VALUES T DEBUG-INFO)
(DEFPROP :VALUES VALUES DEBUG-INFO)
(DEFPROP RETURN-LIST VALUES DEBUG-INFO)
(DEFPROP :RETURN-LIST VALUES DEBUG-INFO)
(DEFPROP FUNCTION-PARENT T DEBUG-INFO)
(DEFPROP INTERPRETED-DEFINITION T DEBUG-INFO)
(DEFPROP DOCUMENTATION T DEBUG-INFO)
;(defprop combined-method-derivation t debug-info)	--
; others include compiler::compiler-arglist

(DEFUN EXTRACT-DECLARATIONS (BODY &OPTIONAL DECLS DOC-STRING-VALID-P ENVIRONMENT &AUX DOC)
  "Extract declarations and documentation string from BODY and return them.
The first value is what is left of BODY after any doc string and decls are removed.
It is BODY missing some number of its initial elements.

The second value is the list of declarations found.
Each element of a DECLARE found in body is a declaration
and goes on this list.  The argument DECLS is the initial
value of this list, and all declarations in BODY are added to that.

The third value is the doc string found in BODY, if there was one.
However, doc strings are only processed if DOC-STRING-VALID-P is non-NIL."
  (DECLARE (VALUES BODY DECLARATIONS DOC-STRING))
  (DO-FOREVER
    (LET (FORM)
      ;; Macro-expand the form, but don't worry if we get an error.
      ;; In that case, we will not see it as a declaration,
      ;; it will get macroexpanded again, and generate a warning then.
      (SETQ FORM (IGNORE-ERRORS
		   (MACROEXPAND (CAR BODY) ENVIRONMENT)))
      (COND ((AND DOC-STRING-VALID-P
		  (STRINGP FORM))
		  ;; We skip any number of strings, but use only the first.
		  (OR DOC (SETQ DOC FORM))
		  ;; If the string is the last thing in the body,
		  ;; don't flush it, since it needs to be the return value.
		  (OR (CDR BODY) (RETURN)))
	    ((EQ (CAR-SAFE FORM) 'DECLARE)
	     ;; hack the documentation declaration specially
	     (COND ((EQ (CADR-SAFE FORM) 'DOCUMENTATION)
		    (SETQ DOC (AND DOC-STRING-VALID-P (OR DOC (CADR-SAFE (CADR FORM))))))
		   ((CDR FORM)
		    ;; We allow any number of DECLAREs, and process them all.
		    (SETQ DECLS (APPEND (CDR FORM) DECLS)))))
	    (T (RETURN)))
      (POP BODY)))
  (VALUES BODY DECLS DOC))

(DEFUN PROCESS-DEFUN-BODY (NAME VARS+BODY &OPTIONAL NO-IMPLICIT-BLOCK)
  "Given the name, and the data, for a DEFUN, return a NAMED-LAMBDA.
NO-IMPLICIT-BLOCK inhibits creation of the automatic BLOCK around the BODY.
This is used for DEFSUBST."
  ;; In case DEFUN was called from compiled code,
  ;; and VARS+BODY is a stack list, copy it.
  (SETQ VARS+BODY (COPY-OBJECT VARS+BODY))
  (LET ((LOCAL-DCL LOCAL-DECLARATIONS)
	(BODY (CDR VARS+BODY))
	(VARS (CAR VARS+BODY))
	DOCUMENTATION TEM)
    ;; Extract any DECLARE from the front of the body and put it into
    ;; the local declarations that are in effect.  Remove the DECLARE from the body.
    (SETF (VALUES BODY LOCAL-DCL DOCUMENTATION)
	  (EXTRACT-DECLARATIONS BODY LOCAL-DCL T))
    (UNLESS NO-IMPLICIT-BLOCK
      (IF (SYMBOLP NAME)
	  (SETQ BODY `((BLOCK ,NAME . ,BODY)))))
    ;; Use some local declarations for making debug-info, if they are present.
    ;; Canonicalize synonyms too.
    (LET ((DEBUG-INFO (LOOP FOR DCL IN LOCAL-DCL
			    WITH TEM
			    WHEN (SETQ TEM (GET (CAR DCL) 'DEBUG-INFO))
			      COLLECT (IF (EQ TEM T) (COPY-TREE DCL)
					(CONS TEM (COPY-TREE (CDR DCL)))))))
      (AND DOCUMENTATION (PUSH `(DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
      ;; Put whatever other local declarations there are
      ;; into a DECLARE at the front of the transformed body.
      (WHEN LOCAL-DCL
	(SETQ TEM (SUBSET-NOT #'(LAMBDA (X) (GET (CAR X) 'DEBUG-INFO))
			      LOCAL-DCL))
	(PUSH `(DECLARE . ,TEM) BODY))
      ;; Make a NAMED-LAMBDA of the appropriate form
      `(NAMED-LAMBDA ,(IF (OR DEBUG-INFO (NOT (SYMBOLP NAME)))
			  (CONS NAME DEBUG-INFO)
			  NAME)
		     ,VARS . ,BODY))))

))

; From file QMISC.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DOCUMENTATION (SYMBOL &OPTIONAL (DOC-TYPE 'FUNCTION))
  "Try to return the documentation string for SYMBOL, else return NIL.
Standard values of DOC-TYPE are: FUNCTION, VARIABLE, TYPE, STRUCTURE and SETF,
but you can put on and retrieve documentation for any DOC-TYPE.
Documentation strings are installed by SETFing a call to DOCUMENTATION."
  (COND ((AND (EQ DOC-TYPE 'VALUE)
	      (GET SYMBOL :DOCUMENTATION)))
	((AND (SYMBOLP SYMBOL)
	      (LET ((DOC-PROP (GET SYMBOL 'DOCUMENTATION-PROPERTY)))
		(CDR (SYS:ASSOC-EQUAL (STRING DOC-TYPE) DOC-PROP)))))
	((AND (EQ DOC-TYPE 'TYPE)
	      (GET SYMBOL 'TYPE-EXPANDER)
	      (DOCUMENTATION (GET SYMBOL 'TYPE-EXPANDER) 'FUNCTION)))
	((AND (EQ DOC-TYPE 'SETF)
	      (GET SYMBOL 'SETF-METHOD)
	      (DOCUMENTATION (GET SYMBOL 'SETF-METHOD) 'FUNCTION)))
	((NEQ DOC-TYPE 'FUNCTION) NIL)
	((SYMBOLP SYMBOL)
	 (IF (FBOUNDP SYMBOL)
	     (DOCUMENTATION (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC SYMBOL)))))
	((CONSP SYMBOL)
	 (IF (FUNCTIONP SYMBOL T)
	     (IF (EQ (CAR SYMBOL) 'MACRO)
		 (DOCUMENTATION (CDR SYMBOL))
	       (OR (CADR (ASSQ 'DOCUMENTATION (DEBUGGING-INFO SYMBOL)))
		   (CADR (ASSQ 'DOCUMENTATION (DEBUGGING-INFO SYMBOL)))
		   (NTH-VALUE 2 (EXTRACT-DECLARATIONS
				  (CDR (LAMBDA-EXP-ARGS-AND-BODY SYMBOL)) NIL T NIL))))
	   (AND (FDEFINEDP SYMBOL)
		(DOCUMENTATION (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC SYMBOL))))))
	((COMPILED-FUNCTION-P SYMBOL)
	 (IF (ASSQ 'COMBINED-METHOD-DERIVATION (DEBUGGING-INFO SYMBOL))
	     ;; its an FEF for a combined method, so do special handling
	     (COMBINED-METHOD-DOCUMENTATION SYMBOL)
	   (OR (CADR (ASSQ 'DOCUMENTATION (DEBUGGING-INFO SYMBOL)))
	       (CADR (ASSQ :DOCUMENTATION (DEBUGGING-INFO SYMBOL))))))))

))

; From file DEFMAC.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

(DEFUN DEFMACRO1 (X TYPE &OPTIONAL ENV)
  (LET (*VARLIST* *VALLIST* OPTIONAL-SPECIFIED-FLAGS DEFMACRO-&BODY-FLAG
	(ARGLIST (CADR X)))
    (WHEN (EQ (CAR-SAFE ARGLIST) '&WHOLE)
      (SETQ ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
	   (MIN-ARGS (CAR ARGS-DATA))
	   (OPT-ARGS (CADR ARGS-DATA)))
     `(LOCAL-DECLARE ((ARGLIST
		       . ,(LOOP FOR TAIL ON (CADR X)
				WHEN (AND (ATOM TAIL) TAIL) RETURN (CADR X)
				UNTIL (EQ (CAR TAIL) '&AUX)
				;; user doesn't want to see &environment...
				WHEN (MEMQ (CAR TAIL) '(&ENVIRONMENT &WHOLE))
				  DO (SETQ TAIL (CDR TAIL))
				  ELSE COLLECT (CAR TAIL))))
       ,@(AND DEFMACRO-&BODY-FLAG
	      `((EVAL-WHEN (EVAL COMPILE LOAD)
		  (DEFMACRO-SET-INDENTATION-FOR-ZWEI ',(CAR X) ',(+ MIN-ARGS OPT-ARGS)))))
       (,TYPE ,(STANDARDIZE-FUNCTION-SPEC (CAR X))
	. ,(LAMBDA-EXP-ARGS-AND-BODY (EXPAND-DEFMACRO X ENV)))))))


(DEFUN EXPAND-DEFMACRO (X ENV)
  (LET (*VARLIST* *VALLIST* OPTIONAL-SPECIFIED-FLAGS DEFMACRO-&BODY-FLAG
	(ARGLIST (CADR X))
	WHOLE-ARG-DATA)
    (WHEN (EQ (CAR-SAFE ARGLIST) '&WHOLE)
      (SETQ WHOLE-ARG-DATA `((,(CADR ARGLIST) *MACROARG*))
	    ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
	   (MIN-ARGS (CAR ARGS-DATA))
	   (MAX-ARGS (CADDR ARGS-DATA))
	   (BODY (CDDR X))
	   DOC-STRING DECLS)
      (MULTIPLE-VALUE-SETQ (BODY DECLS DOC-STRING)
	(EXTRACT-DECLARATIONS BODY NIL T ENV))
      (SETQ DECLS (SUBSET #'(LAMBDA (DECL)
			      (MEMQ (CAR-SAFE DECL) '(ARGLIST RETURN-LIST VALUES)))
			  DECLS))
      (IF DOC-STRING (PUSH `(DOCUMENTATION ,DOC-STRING) DECLS))
      `(NAMED-LAMBDA ,(CAR X) (*MACROARG* &OPTIONAL *MACROENVIRONMENT*)
	,@(IF DECLS `((DECLARE . ,DECLS)))
	*MACROENVIRONMENT*			; Ok not to refer to it.
	,@(COND ((AND DEFMACRO-CHECK-ARGS
		      (NOT (AND (ZEROP MIN-ARGS) (NULL MAX-ARGS))))
		 `((AND ,(COND ((ZEROP MIN-ARGS)
				`(> (LENGTH *MACROARG*)
				    ,(1+ MAX-ARGS)))
			       ((NULL MAX-ARGS)
				`(< (LENGTH *MACROARG*)
				    ,(1+ MIN-ARGS)))
			       (T `(OR (< (LENGTH *MACROARG*)
				          ,(1+ MIN-ARGS))
				       (> (LENGTH *MACROARG*)
					  ,(1+ MAX-ARGS)))))
			(MACRO-REPORT-ARGS-ERROR *MACROARG* ,MIN-ARGS ,MAX-ARGS))))
		(T NIL))
	(LET* (,@OPTIONAL-SPECIFIED-FLAGS
	       ,@WHOLE-ARG-DATA
	       . ,(MAPCAR #'LIST (NREVERSE *VARLIST*) (NREVERSE *VALLIST*)))
	  . ,BODY)))))

))

; From file LMMAC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defmacro with-output-to-string (&environment env (stream string index) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
If STRING is omitted, a new string with no fill pointer is created and returned.
If STRING is supplied, that string's contents are modified destructively,
and the values of BODY's last expression are returned.
If INDEX is supplied, it should be a SETFable accessor which describes
where to find the index to store into STRING, instead of at the end.
The value of INDEX will be updated after the BODY is finished."
  (multiple-value-bind (realbody decls)
      (extract-declarations body nil nil env)
    (if string
	`(let ((,stream (make-string-output-stream ,string ,index)))
	   (declare . ,decls)
	   (unwind-protect
	       (progn . ,realbody)
	     ,(if index `(setf ,index (send ,stream ':get-string-index)))))
      `(let ((,stream (make-string-output-stream)))
	 (declare . ,decls)
	 ,@realbody
	 (get-output-stream-string ,stream)))))

))

; From file RESOUR.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFMACRO USING-RESOURCE (&ENVIRONMENT ENV (VAR RESOURCE-NAME . PARAMETERS) &BODY BODY)
  "Execute BODY with VAR bound to an object allocated from resource RESOURCE-NAME.
PARAMETERS are used in selecting or creating the object,
according to the definition of the resource."
  (MULTIPLE-VALUE-BIND (BODY DECLARATIONS)
      (EXTRACT-DECLARATIONS BODY NIL NIL ENV)
    `(LET ((,VAR NIL))
       ,(IF DECLARATIONS `(DECLARE . ,DECLARATIONS))
       (UNWIND-PROTECT
	   (PROGN
	     (SETQ ,VAR (ALLOCATE-RESOURCE ',RESOURCE-NAME . ,PARAMETERS))
	     . ,BODY)
	 (AND ,VAR (DEALLOCATE-RESOURCE ',RESOURCE-NAME ,VAR))))))

))

;;; moved into 99.12
;; From file RESOUR.LISP OZ:<L.SYS2> OZ:
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

;(DEFMACRO DEFRESOURCE (NAME PARAMETERS &REST OPTIONS)
;  "Define a resource named NAME, with parameters PARAMETERS for constructing objects.
;OPTIONS can specify how to create objects and how to tell when old objects
;can be reused."
;  (LET ((CONSTRUCTOR-FORM NIL) (FINDER-FORM NIL) (MATCHER-FORM NIL) (CHECKER-FORM NIL)
;	(CONSTRUCTOR-FUNCTION NIL) (FINDER-FUNCTION NIL) (MATCHER-FUNCTION NIL)
;	(PARAMETIZER-FUNCTION NIL) (CHECKER-FUNCTION NIL) (INITIAL-COPIES 0)
;	(INITIALIZER-FORM NIL) (INITIALIZER-FUNCTION NIL) (FREE-LIST-SIZE 20.) (PARAMS NIL)
;	(DOCUMENTATION NIL))
;    (OR (CONSP PARAMETERS) (NULL PARAMETERS)
;	(FERROR NIL "~S invalid parameter list" PARAMETERS))
;    (SETQ PARAMS (LOOP FOR P IN PARAMETERS
;		       UNLESS (MEMQ P LAMBDA-LIST-KEYWORDS)
;		       COLLECT (IF (SYMBOLP P) P (CAR P))))
;    ;; if first option is a string, use it as documentation instead
;    (WHEN (STRINGP (CAR OPTIONS))
;      (SETQ DOCUMENTATION (POP OPTIONS)))
;    (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY 'CDDR
;	  DO (CASE KEYWORD
;	       (:CONSTRUCTOR (SETQ CONSTRUCTOR-FORM VALUE))
;	       (:FINDER (SETQ FINDER-FORM VALUE))
;	       (:MATCHER (SETQ MATCHER-FORM VALUE))
;	       (:CHECKER (SETQ CHECKER-FORM VALUE))
;	       (:INITIALIZER (SETQ INITIALIZER-FORM VALUE))
;	       (:INITIAL-COPIES
;		(SETQ INITIAL-COPIES
;		      (COND ((NULL VALUE) 0)
;			    ((NUMBERP VALUE) VALUE)
;			    (T (FERROR NIL ":INITIAL-COPIES ~S - number required"
;				       VALUE)))))
;	       (:FREE-LIST-SIZE
;		(SETQ FREE-LIST-SIZE
;		      (COND ((NULL VALUE) 20.)
;			    ((NUMBERP VALUE) VALUE)
;			    (T (FERROR NIL ":FREE-LIST-SIZE ~S - number required")))))
;	       (OTHERWISE (FERROR NIL "~S illegal option in DEFRESOURCE" KEYWORD))))
;    (OR CONSTRUCTOR-FORM (FERROR NIL "DEFRESOURCE requires the :CONSTRUCTOR option"))
;    ;; Pick function names.  Note that NIL is SYMBOLP.
;    (SETQ CONSTRUCTOR-FUNCTION (IF (SYMBOLP CONSTRUCTOR-FORM) CONSTRUCTOR-FORM
;				 `(:PROPERTY ,NAME RESOURCE-CONSTRUCTOR)))
;    (SETQ FINDER-FUNCTION (IF (SYMBOLP FINDER-FORM) FINDER-FORM
;			    `(:PROPERTY ,NAME RESOURCE-FINDER)))
;    (SETQ MATCHER-FUNCTION (IF (SYMBOLP MATCHER-FORM) MATCHER-FORM
;			     `(:PROPERTY ,NAME RESOURCE-MATCHER)))
;    (SETQ CHECKER-FUNCTION (IF (SYMBOLP CHECKER-FORM) CHECKER-FORM
;			     `(:PROPERTY ,NAME RESOURCE-CHECKER)))
;    (SETQ INITIALIZER-FUNCTION (IF (SYMBOLP INITIALIZER-FORM) INITIALIZER-FORM
;				 `(:PROPERTY ,NAME RESOURCE-INITIALIZER)))
;    (SETQ PARAMETIZER-FUNCTION (IF (AND PARAMETERS (NOT MATCHER-FORM) (NOT FINDER-FORM))
;				   `(:PROPERTY ,NAME RESOURCE-PARAMETIZER)))
;    `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,NAME DEFRESOURCE))
;       ,(IF (NOT (SYMBOLP CONSTRUCTOR-FORM))
;	    `(DEFUN ,CONSTRUCTOR-FUNCTION (IGNORE ,@PARAMETERS)
;	       ,@PARAMS
;	       ,CONSTRUCTOR-FORM))
;       ,(IF (NOT (SYMBOLP FINDER-FORM))
;	    `(DEFUN ,FINDER-FUNCTION (IGNORE ,@PARAMETERS)
;	       ,@PARAMS
;	       ,FINDER-FORM))
;       ,(IF (NOT (SYMBOLP MATCHER-FORM))
;	    `(DEFUN ,MATCHER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
;	       ,@PARAMS
;	       ,MATCHER-FORM))
;       ,(IF (NOT (SYMBOLP CHECKER-FORM))
;	    `(DEFUN ,CHECKER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
;				       ,@PARAMETERS)
;	       ,@PARAMS ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
;	       ,CHECKER-FORM))
;       ,(IF (NOT (SYMBOLP INITIALIZER-FORM))
;	    `(DEFUN ,INITIALIZER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
;	       ,@PARAMS ,(INTERN "OBJECT")
;	       ,INITIALIZER-FORM))
;       ,(IF PARAMETIZER-FUNCTION
;	    `(DEFUN ,PARAMETIZER-FUNCTION ,PARAMETERS
;	       (LIST ,@PARAMS)))
;       (INITIALIZE-RESOURCE ',NAME ',CONSTRUCTOR-FUNCTION ',FINDER-FUNCTION
;			    ',MATCHER-FUNCTION ',CHECKER-FUNCTION
;			    ',PARAMETIZER-FUNCTION ',INITIAL-COPIES ',FREE-LIST-SIZE
;			    ',INITIALIZER-FUNCTION)
;       ,(IF DOCUMENTATION
;	  `(SET-DOCUMENTATION ',NAME 'RESOURCE ,DOCUMENTATION)))))

;))

; From file LMMAC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defmacro with-input-from-string (&environment env (stream string . keyword-args) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
The values of BODY's last expression are returned.
Keywords allowed are :START, :END and :INDEX.
:START and :END can be used to specify a substring of STRING to be read from.
 Eof will then occur when :END is reached.
 If the :END value is NIL, that means the end of STRING.
:INDEX specifies a SETF-able place to store the index of where reading stopped.
 This is done after exit from the body.  It stores the index of the first unread character,
 or the index of eof if that was reached.

Old calling format: (STREAM STRING &OPTIONAL INDEX END), where INDEX serves
 as the value for the :START keyword and for the :INDEX keyword."
  (let (start end index decls realbody)
    (setf (values realbody decls)
	  (extract-declarations body nil nil env))
    (if (keywordp (car keyword-args))
	(setq start (getf keyword-args :start)
	      index (getf keyword-args :index)
	      end (getf keyword-args :end))
      (setq start (car keyword-args)
	    index (car keyword-args)
	    end (cadr keyword-args)))
    `(let ((,stream (make-string-input-stream ,string ,(or start 0) ,end)))
       (declare . ,decls)
       (unwind-protect
	   (progn . ,realbody)
	 ,(if index `(setf ,index (send ,stream :get-string-index)))))))


(DEFMACRO DEFUNP (&ENVIRONMENT ENV FUNCTION-SPEC LAMBDA-LIST &REST BODY)
  "Like DEFUN, but provides an implicit PROG of no variables around the BODY.
So you can use RETURN to return from the function, and use GO.
There is one difference from an ordinary PROG:
the value of the last element of the BODY is returned.
This is so even if it is an atom.  This is like ordinary DEFUNs."
  (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
	(LAST NIL)
	DECLARES DOC)
    (SETQ BODY (COPY-LIST BODY))
    (SETQ LAST (LAST BODY))
    (SETF (VALUES BODY DECLARES DOC)
	  (EXTRACT-DECLARATIONS BODY NIL T ENV))
    (WHEN (NEQ 'RETURN (CAAR-SAFE LAST))
      (SETF (CAR LAST) `(RETURN ,(CAR LAST))))
    `(DEFUN ,FUNCTION-SPEC ,LAMBDA-LIST
       ,DOC
       (DECLARE . ,DECLARES)
       (PROG () . ,BODY))))


;>> moved to 99.12+ pending rethought by mly
;(DEFMACRO CHECK-ARG (ARG-NAME PREDICATE TYPE-STRING &OPTIONAL ERROR-TYPE-NAME)
;  "Generate error if the value of ARG-NAME doesn't satisfy PREDICATE.
;PREDICATE is a function name (a symbol) or an expression to compute.
;TYPE-STRING is a string to use in the error message, such as /"a list/".
;ERROR-TYPE-NAME is a keyword that tells condition handlers what type was desired.
;This macro is somewhat obsolete: you should probably be using CHECK-TYPE instead."
;    (AND (NULL ERROR-TYPE-NAME)
;	 (SYMBOLP PREDICATE)
;	 (SETQ ERROR-TYPE-NAME PREDICATE))
;    `(DO () (,(COND ((SYMBOLP PREDICATE)
;                     `(,PREDICATE ,ARG-NAME))
;                    (T PREDICATE))
;             ,ARG-NAME)
;	 (SETQ ,ARG-NAME
;	       (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
;		       "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
;		       ',ERROR-TYPE-NAME ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))


;(DEFMACRO CHECK-TYPE (ARG-NAME TYPE &OPTIONAL TYPE-STRING)
;  "Generate an error unless (TYPEP ARG-NAME 'TYPE).
;TYPE-STRING is a string to use in the error message, such as /"a list/".
;If you omit it, it will be computed from TYPE's pname."
;  `(DO () ((TYPEP ,ARG-NAME ',TYPE))
;     (SETQ ,ARG-NAME
;	   (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
;		   "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
;		   ',TYPE ,ARG-NAME ',ARG-NAME
;		   ,(OR TYPE-STRING `(TYPE-PRETTY-NAME ',TYPE))))))


(DEFMACRO WITH-OPEN-STREAM (&ENVIRONMENT ENV (STREAM CONSTRUCTION-FORM) &BODY BODY)
  "Execute the BODY with the variable STREAM bound to the value of CONSTRUCTOR-FORM.
On normal exit, close STREAM normally.
On abnormal exit (throwing, errors, etc) close STREAM with argument :ABORT."
  (LET ((GENSYM (GENSYM)))
    (MULTIPLE-VALUE-BIND (REALBODY DECLS)
	(EXTRACT-DECLARATIONS BODY NIL NIL ENV)
      `(LET ((,GENSYM NIL)
	     (.FILE-ABORTED-FLAG. :ABORT))
	 (DECLARE . ,DECLS)
	 (UNWIND-PROTECT
	     (PROGN (SETQ ,GENSYM ,CONSTRUCTION-FORM)
		    (MULTIPLE-VALUE-PROG1 (LET ((,STREAM ,GENSYM))
					    . ,REALBODY)
					  (SETQ .FILE-ABORTED-FLAG. NIL)))
	   (AND ,GENSYM (NOT (ERRORP ,GENSYM))
		(SEND ,GENSYM :CLOSE .FILE-ABORTED-FLAG.)))))))


(DEFMACRO WITH-OPEN-STREAM-CASE (&ENVIRONMENT ENV (STREAM CONSTRUCTION-FORM) &BODY CLAUSES)
  "Use CONSTRUCTOR-FORM to open a stream, using the CLAUSES as in CONDITION-CASE.
The CLAUSES may contain a :NO-ERROR clause which will be executed,
with STREAM bound to the resulting stream, if CONSTRUCTOR-FORM does not get an error.
On normal exit from the :NO-ERROR clause, STREAM is closed normally.
On abnormal exit (throwing, errors, etc) STREAM is closed with argument :ABORT."
  (LET ((GENSYM (GENSYM)))
    (MULTIPLE-VALUE-BIND (REAL DECLS)
	(EXTRACT-DECLARATIONS CLAUSES NIL NIL ENV)
    `(LET ((,GENSYM NIL)
	   (.FILE-ABORTED-FLAG. ':ABORT))
       (DECLARE . ,DECLS)
       (UNWIND-PROTECT
	   (MULTIPLE-VALUE-PROG1
	     (CONDITION-CASE (,STREAM) (SETQ ,GENSYM ,CONSTRUCTION-FORM)
	       . ,REAL)
	     (SETQ .FILE-ABORTED-FLAG. NIL))
	 (AND ,GENSYM (NOT (ERRORP ,GENSYM))
	      (SEND ,GENSYM :CLOSE .FILE-ABORTED-FLAG.)))))))


(DEFMACRO WITH-OPEN-FILE-SEARCH (&ENVIRONMENT ENV
				 (STREAM (OPERATION DEFAULTS AUTO-RETRY)
					 TYPE-LIST-AND-PATHNAME-FORM
					 . OPEN-OPTIONS)
				 &BODY BODY)
  "Open one of several filenames, the same except for the type component.
Binds the variable STREAM to the resulting stream, executes the BODY, then closes the stream.
OPEN-OPTIONS are alternating keywords and values, passed to OPEN.
TYPE-LIST-AND-PATHNAME-FORM is evaluated to get two values:
 a list of pathname types to try, and a base pathname.
The base pathname is merged successively with each type in the list.
This is done using FS:MERGE-PATHNAME-DEFAULTS, with DEFAULTS's value
used as the second argument and the type to be tried as the third argument.
As soon as a merged pathname succeeds in being opened, we execute BODY.
If they all fail, an error is signaled with condition FS:MULTIPLE-FILE-NOT-FOUND.
OPERATION should eval to the name of the calling function; it is used for signaling.
If AUTO-RETRY evals to non-NIL, then the user is asked to type a new
pathname to retry with."
  (LET ((BASE-PATHNAME-VAR (GENSYM))
	(TYPE-LIST-VAR (GENSYM))
	(DEFAULTS-VAR (GENSYM))
	(AUTO-RETRY-VAR (GENSYM)))
    (MULTIPLE-VALUE-BIND (REAL DECLS)
	(EXTRACT-DECLARATIONS BODY NIL NIL ENV)
      `(LET ((,DEFAULTS-VAR ,DEFAULTS)
	     (,AUTO-RETRY-VAR ,AUTO-RETRY))
	 (DECLARE . ,DECLS)
	 (MULTIPLE-VALUE-BIND (,TYPE-LIST-VAR ,BASE-PATHNAME-VAR)
	     ,TYPE-LIST-AND-PATHNAME-FORM
	   (DECLARE . ,DECLS)
	   (FILE-RETRY-NEW-PATHNAME-IF ,AUTO-RETRY-VAR (,BASE-PATHNAME-VAR FS:FILE-ERROR)
	     (WITH-OPEN-STREAM (,STREAM
				(FS:OPEN-FILE-SEARCH ,BASE-PATHNAME-VAR ,TYPE-LIST-VAR
						     ,DEFAULTS-VAR ,OPERATION
						     . ,OPEN-OPTIONS))
	       (DECLARE . ,DECLS)
	       . ,REAL)))))))

(DEFMACRO WITH-CLEANUP-LIST (&ENVIRONMENT ENV NAME-TO-BIND &BODY BODY)
  (MULTIPLE-VALUE-BIND (REAL DECLS)
      (EXTRACT-DECLARATIONS BODY NIL NIL ENV)
    `(LET ((,NAME-TO-BIND (LIST NIL)))
       (DECLARE . ,DECLS)
       (UNWIND-PROTECT
	   (PROGN . ,REAL)
	 (LOOP FOR ELEM IN (CAR ,NAME-TO-BIND)
			   DO (APPLY (CAR ELEM) (CDR ELEM)))))))


(DEFMACRO WITH-LOCK (&ENVIRONMENT ENV (LOCATOR . OPTIONS) &BODY BODY &AUX NORECURSIVE NOERROR)
  "Execute the BODY with a lock locked.
LOCATOR is an expression whose value is the lock status;
it should be suitable for use inside LOCF.
OPTIONS include :NORECURSIVE, do not allow locking a lock already locked by this process."
  ;; Ignore the old :NOERROR option -- it's always that way now.
  (KEYWORD-EXTRACT OPTIONS O () (NORECURSIVE NOERROR) (OTHERWISE NIL))
  (MULTIPLE-VALUE-BIND (REAL DECLS)
      (EXTRACT-DECLARATIONS BODY NIL NIL ENV)
    `(LET* ((.POINTER. (LOCF ,LOCATOR))
	    (.ALREADY.MINE. (EQ (CAR .POINTER.) CURRENT-PROCESS)))
       (DECLARE . ,DECLS)
       (IF (CONSP .POINTER.)
	   (SETQ .POINTER. (CDR-LOCATION-FORCE .POINTER.)))
       (UNWIND-PROTECT
	   (PROGN (IF .ALREADY.MINE.
		      ,(IF NORECURSIVE `(FERROR NIL "Attempt to lock ~S recursively."
						',LOCATOR))
		    ;; Redundant, but saves time if not locked.
		    (OR (%STORE-CONDITIONAL .POINTER. NIL CURRENT-PROCESS)
			(PROCESS-LOCK .POINTER.)))
		  . ,REAL)
	 (UNLESS .ALREADY.MINE.
	   (%STORE-CONDITIONAL .POINTER. CURRENT-PROCESS NIL))))))


(defmacro with-help-stream (&environment env (stream . options) &body bod
			    &aux label width height superior)
  "Execute the BODY with STREAM bound to a stream for printing help text on.
If *TERMINAL-IO* or the specified superior is a window, a special /"help window/"
is popped up and used as STREAM. 
If *TERMINAL-IO* or the specified superior is not a window, it is used directly.
OPTIONS is a list of alternating keywords and values.
 :LABEL's value is a string to use as the label of the help window if one is used.
 :WIDTH's value is a symbol to bind to the width in characters of STREAM.
  It is bound while BODY is executed.
 :HEIGHT's value is a symbol to bind to the height in characters.
 :SUPERIOR's value is a window to use as the superior of the help window."
  (keyword-extract options *with-help-iter* (label width height superior) nil)
  (process-defaults '((label "") (width '*with-help-width*)
		      (height '*with-help-height*) (superior '*terminal-io*)))
  (multiple-value-bind (real decls)
      (extract-declarations bod nil nil env)
    `(let ((.body.function.
	     #'(lambda (,stream &aux ,width ,height)
		 (declare . ,decls)
		 (if (memq :size-in-characters (send ,stream :which-operations))
		     (multiple-value-setq (,width ,height) (send ,stream :size-in-characters))
		   (setq ,width 85. ,height 66.))
		 . ,real)))
     (with-help-stream-1 ,label ,superior .body.function.))))


(defmacro with-input-from-string (&environment env (stream string . keyword-args) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
The values of BODY's last expression are returned.
Keywords allowed are :START, :END and :INDEX.
:START and :END can be used to specify a substring of STRING to be read from.
 Eof will then occur when :END is reached.
 If the :END value is NIL, that means the end of STRING.
:INDEX specifies a SETF-able place to store the index of where reading stopped.
 This is done after exit from the body.  It stores the index of the first unread character,
 or the index of eof if that was reached.

Old calling format: (STREAM STRING &OPTIONAL INDEX END), where INDEX serves
 as the value for the :START keyword and for the :INDEX keyword."
  (let (start end index decls realbody)
    (setf (values realbody decls)
	  (extract-declarations body nil nil env))
    (if (keywordp (car keyword-args))
	(setq start (getf keyword-args :start)
	      index (getf keyword-args :index)
	      end (getf keyword-args :end))
      (setq start (car keyword-args)
	    index (car keyword-args)
	    end (cadr keyword-args)))
    `(let ((,stream (make-string-input-stream ,string ,(or start 0) ,end)))
       (declare . ,decls)
       (unwind-protect
	   (progn . ,realbody)
	 ,(if index `(setf ,index (send ,stream :get-string-index)))))))

(defmacro with-output-to-string (&environment env (stream string index) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
If STRING is omitted, a new string with no fill pointer is created and returned.
If STRING is supplied, that string's contents are modified destructively,
and the values of BODY's last expression are returned.
If INDEX is supplied, it should be a SETFable accessor which describes
where to find the index to store into STRING, instead of at the end.
The value of INDEX will be updated after the BODY is finished."
  (multiple-value-bind (realbody decls)
      (extract-declarations body nil nil env)
    (if string
	`(let ((,stream (make-string-output-stream ,string ,index)))
	   (declare . ,decls)
	   (unwind-protect
	       (progn . ,realbody)
	     ,(if index `(setf ,index (send ,stream ':get-string-index)))))
      `(let ((,stream (make-string-output-stream)))
	 (declare . ,decls)
	 ,@realbody
	 (get-output-stream-string ,stream)))))

))

; From file SETF.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro define-setf-method (&environment env access-function lambda-list &body body)
  "General way to define how to SETF forms starting with ACCESS-FUNCTION.
This form defines a macro which will be invoked by GET-SETF-METHOD-MULTIPLE-VALUE.
The LAMBDA-LIST is matched, DEFMACRO-style, against the form to be SETF'd.
Then the BODY is executed and should produce five values to return from
GET-SETF-METHOD-MULTIPLE-VALUE.
See that function for a description of what the five values mean.
This is more general than DEFSETF because it can decide how to parse
the form to be SETF'd, decide which parts to replace with tempvars, and so on.

A trivial example would be
/(DEFINE-SETF-METHOD CAR (LIST)
  (LET ((TEMPVARS (LIST (GENSYM)))
	(TEMPARGS (LIST LIST))
	(STOREVAR (GENSYM)))
    (VALUES TEMPVARS TEMPARGS (LIST STOREVAR)
	    `(SYS:SETCAR ,(FIRST TEMPVARS) ,STOREVAR)
	    `(CAR ,(FIRST TEMPVARS)))))
which is equivalent to (DEFSETF CAR SETCAR)."
  (multiple-value-bind (real decls doc-string)
      (extract-declarations body nil t env)
    `(progn
       (set-documentation ',access-function 'setf ,doc-string)
       (defmacro (:property ,access-function setf-method) ,lambda-list
	 (declare (function-parent ,access-function define-setf-method)
		  (documentation . ,doc-string)
		  . ,decls)
	 . ,real))))

))

; From file QFCTNS.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN SUBST-EXPAND (SUBST FORM ENVIRONMENT SIMPLE-SUBSTITUTION-OK)
  (LET (ALIST OPTIONAL-FLAG REST-ALREADY-FLAG LAMBDA-LIST BODY FN-NAME)
    ;; Extract the lambda-list, body, and function name from the definition.
    (COND ((EQ (CAR SUBST) 'NAMED-SUBST)
	   (SETQ LAMBDA-LIST (CADDR SUBST) BODY (CDDDR SUBST))
	   (SETQ FN-NAME (COND ((SYMBOLP (CADR SUBST)) (CADR SUBST))
			       (T (CAADR SUBST)))))
	  (T (SETQ LAMBDA-LIST (CADR SUBST) BODY (CDDR SUBST)
		   FN-NAME (CAR FORM))))
    ;; Discard documentation string or declarations from front of body.
    (SETQ BODY (EXTRACT-DECLARATIONS BODY NIL T ENVIRONMENT))
    ;; Provide an implicit PROGN for the body.
    (IF (CDR BODY)
	(SETQ BODY `(PROGN . ,BODY))
      (SETQ BODY (CAR BODY)))
    ;;???? Flush the implicitly generated BLOCK.
    ;; This is a kludge, indeed.
    (AND (EQ (CAR-SAFE BODY) 'BLOCK)
	 (SETQ BODY (CONS 'PROGN (CDDR BODY))))
    ;; Process the lambda list and args to make the alist.
    (DO ((VALS (CDR FORM) (CDR VALS)))
	(NIL)
      ;; We allow only &OPTIONAL and &REST.
      (DO-FOREVER
	(CASE (CAR LAMBDA-LIST)
	  (&OPTIONAL (SETQ OPTIONAL-FLAG T))
	  (&REST (OR REST-ALREADY-FLAG
		     (SETQ VALS (LIST (CONS 'LIST VALS))
			   REST-ALREADY-FLAG T)))
	  (OTHERWISE (RETURN)))
	(POP LAMBDA-LIST))
      ;; All lambda-list keywords aside from &OPTIONAL and &REST are erroneous.
      (AND (MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
	   (RETURN
	     (CONS (CERROR T NIL 'INVALID-FORM
			   "Subst-function ~S contains inappropriate keyword ~A."
			   FN-NAME (CAR LAMBDA-LIST))
		   (CDR FORM))))
      ;; Detect runout of lambda list or of args.
      (COND ((NULL VALS)
	     (COND ((NULL LAMBDA-LIST)
		    (RETURN (IF SIMPLE-SUBSTITUTION-OK
				(SUBLIS ALIST BODY)
			      (SUBLIS-EVAL-ONCE (NREVERSE ALIST) BODY nil nil ENVIRONMENT))))
		   ((NOT OPTIONAL-FLAG)
		    (RETURN (CERROR T NIL 'INVALID-FORM
				    "Too few arguments for ~S."
				    FN-NAME FORM)))))
	    ((NULL LAMBDA-LIST)
	     (RETURN (CERROR T NIL 'INVALID-FORM
			     "Too many arguments for ~S."
			     FN-NAME FORM))))
      ;; Here we have one more arg.  Add it to the alist.
      (PUSH (CONS (COND ((ATOM (CAR LAMBDA-LIST)) (CAR LAMBDA-LIST))
			(T (CAAR LAMBDA-LIST)))
		  (COND (VALS (CAR VALS))
			((ATOM (CAR LAMBDA-LIST)) NIL)
			(T (CADAR LAMBDA-LIST))))
	    ALIST)
      (POP LAMBDA-LIST))))


(DEFUN SUBST-EXPAND-1 (FORM ENVIRONMENT)
  (LET ((SUBST (CAR FORM))
	SIMPLE-SUBSTITUTION-OK)
    (DO-FOREVER
      (COND ((SYMBOLP SUBST)
	     (SETQ SUBST (DECLARED-DEFINITION SUBST)))
	    ((TYPEP SUBST 'COMPILED-FUNCTION)
	     (LET ((DI (DEBUGGING-INFO SUBST)))
	       (SETQ SIMPLE-SUBSTITUTION-OK
		     (NOT (ASSQ ':NO-SIMPLE-SUBSTITUTION DI)))
	       (SETQ SUBST (CADR (ASSQ 'INTERPRETED-DEFINITION DI)))))
	    (T (RETURN))))
    (SUBST-EXPAND SUBST FORM ENVIRONMENT SIMPLE-SUBSTITUTION-OK)))


(DEFUN MACROEXPAND-1 (MACRO-CALL &OPTIONAL ENVIRONMENT
		      &AUX (LOCAL-MACROS (CAR ENVIRONMENT)))
  "Expand MACRO-CALL once and return the result.
Macro calls, uses of SUBSTs, uses of CURRY-BEFORE and CURRY-AFTER,
and uses of functions for which OPEN-CODE-P is true, are all expanded.
The second value is T if there was something to expand.
If SYS:RECORD-MACROS-EXPANDED is non-NIL,
all macro names are pushed on SYS:MACROS-EXPANDED.
The value of *MACROEXPAND-HOOK* (which should behave like FUNCALL)
is used to invoke the expander function."
  (DECLARE (VALUES EXPANSION EXPANDED-FLAG))
  (LET (TM)
    (COND ((ATOM MACRO-CALL) MACRO-CALL)
	  ((NOT (ATOM (CAR MACRO-CALL)))
	   (COND ((EQ (CAAR MACRO-CALL) 'CURRY-AFTER)
		  (VALUES `(,(CADAR MACRO-CALL) ,@(CDR MACRO-CALL) . ,(CDDAR MACRO-CALL))
			  T))
		 ((EQ (CAAR MACRO-CALL) 'CURRY-BEFORE)
		  (VALUES `(,(CADAR MACRO-CALL) ,@(CDDAR MACRO-CALL) . ,(CDR MACRO-CALL))
			  T))
		 ((MEMQ (CAAR MACRO-CALL) '(SUBST CLI:SUBST NAMED-SUBST))
		  (VALUES (FUNCALL *MACROEXPAND-HOOK* 'SUBST-EXPAND-1 MACRO-CALL ENVIRONMENT)
			  T))
		 (T MACRO-CALL)))
	  ((NOT (SYMBOLP (CAR MACRO-CALL)))
	   MACRO-CALL)
	  ((DO ((TAIL LOCAL-MACROS (CDR TAIL)))
	       ((ATOM TAIL))
	     (LET ((FRAME (CAR TAIL)))
	       (SETQ TM
		     (GET-LOCATION-OR-NIL (LOCF FRAME)
					  (LOCF (SYMBOL-FUNCTION (CAR MACRO-CALL)))))
	       (WHEN TM (RETURN TM))))
	   (SETQ TM (CONTENTS TM))
	   (IF (EQ (CAR-SAFE TM) 'MACRO)
	       (LET ((*MACROEXPAND-ENVIRONMENT* ENVIRONMENT)
		     (AINF (ARGS-INFO (CDR TM))))
		 (IF (> (LDB %%ARG-DESC-MAX-ARGS AINF) 1)
		     (VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL ENVIRONMENT) T)
		     (VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL) T)))
	     MACRO-CALL))
	  ((SETQ TM (DECLARED-DEFINITION (CAR MACRO-CALL)))
	   (COND ((TYPEP TM 'COMPILED-FUNCTION)
		  ;; If function is compiled,
		  ;; see if its interpreted defn is recorded.
		  (SETQ TM (ASSQ 'INTERPRETED-DEFINITION (DEBUGGING-INFO TM)))
		  (IF (AND TM (MEMQ (CAADR TM) '(SUBST CLI:SUBST NAMED-SUBST)))
		      (PROGN
			(AND RECORD-MACROS-EXPANDED
			     (NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
			     (PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
			(VALUES (FUNCALL *MACROEXPAND-HOOK* 'SUBST-EXPAND-1 MACRO-CALL ENVIRONMENT)
				T))
		    MACRO-CALL))
		 ((ATOM TM) MACRO-CALL)
		 ((EQ (CAR TM) 'MACRO)
		  (AND RECORD-MACROS-EXPANDED
		       (NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
		       (PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
		  (LET ((*MACROEXPAND-ENVIRONMENT* ENVIRONMENT)
			(AINF (ARGS-INFO (CDR TM))))
		    (IF (> (LDB %%ARG-DESC-MAX-ARGS AINF) 1)
			(VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL ENVIRONMENT)
				T)
		      (VALUES (FUNCALL *MACROEXPAND-HOOK* (CDR TM) MACRO-CALL) T))))
		 ((MEMQ (CAR TM) '(SUBST CLI:SUBST NAMED-SUBST))
		  (AND RECORD-MACROS-EXPANDED
		       (NOT (MEMQ (CAR MACRO-CALL) MACROS-EXPANDED))
		       (PUSH (CAR MACRO-CALL) MACROS-EXPANDED))
		  (VALUES (FUNCALL *MACROEXPAND-HOOK* 'SUBST-EXPAND-1 MACRO-CALL ENVIRONMENT)
			  T))
		 (T MACRO-CALL)))
	  (T MACRO-CALL))))

))

; From file QCOPT.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defcompiler-synonym global:member member-equal)
(defcompiler-synonym global:assoc assoc-equal)
(defcompiler-synonym global:rassoc rassoc-equal)

))

(defun oddp (number) (oddp number))
(defun evenp (number) (evenp number))
(deff throw #'*throw)

(push '(documentation ignore) si::*interpreter-declaration-type-alist*)


; From file QFASL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFASL  "

(DEFUN FASL-OP-ARRAY-PUSH () 
  (LET ((VECTOR (FASL-NEXT-VALUE)))
    (VECTOR-PUSH (FASL-NEXT-VALUE) VECTOR))
  0)

(defprop si::defvar-1 t qfasl-dont-record)
))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFPROP COMPILER-ARGLIST T SI::DEBUG-INFO)


(DEFUN QCOMPILE0 (EXP FUNCTION-TO-BE-DEFINED GENERATING-MICRO-COMPILER-INPUT-P
		  &OPTIONAL (NAME-TO-GIVE-FUNCTION FUNCTION-TO-BE-DEFINED))
  (LET ((EXP1 EXP)
	(VARS ())
	(DEF-TO-BE-SXHASHED)
	(BODY)
	(PDLLVL 0)				;Runtine local pdllvl
	(CALL-BLOCK-PDL-LEVELS)
	(WITHIN-CATCH)
	(ALLGOTAGS)
	(TLEVEL T)
	(P1VALUE T)				;Compiling for value
	(BINDP NIL)				;%BIND not yet used in this frame
	(LVCNT)
	(DROPTHRU T)				;Can drop in if false, flush stuff till tag or
	(MAXPDLLVL 0)				;deepest lvl reached by local pdl

	(ALLVARS)
	(FREEVARS)
	(*LOCAL-FUNCTIONS* *OUTER-CONTEXT-LOCAL-FUNCTIONS*)
	(*FUNCTION-ENVIRONMENT* *OUTER-CONTEXT-FUNCTION-ENVIRONMENT*)
	(*PROGDESC-ENVIRONMENT* *OUTER-CONTEXT-PROGDESC-ENVIRONMENT*)
	(*GOTAG-ENVIRONMENT* *OUTER-CONTEXT-GOTAG-ENVIRONMENT*)
	(LL)
	(TAGOUT)
	(WITHIN-POSSIBLE-LOOP)
	(TLFUNINIT)
	(SPECIALFLAG)
	(MACROFLAG)
	(LOCAL-MAP ())				;names of local variables
	(ARG-MAP ())				;names of arguments
	(LOCAL-FUNCTION-MAP ())			;names of local functions
	(DOCUMENTATION)
	(EXPR-DEBUG-INFO)
	(FAST-ARGS-POSSIBLE T)
	(*BREAKOFF-COUNT* 0)			;no internal functions yet
	(*LEXICAL-CLOSURE-COUNT* 0)
	(VARIABLES-USED-IN-LEXICAL-CLOSURES)
	(MACROS-EXPANDED)			;List of all macros found in this function,
						; for the debugging info.
	(SELF-FLAVOR-DECLARATION)		;(declare (:self-flavor ...))
						; cdr is list of instance variables of flavor
	(SELF-REFERENCES-PRESENT)		;Bound to T if any SELF-REFs are present
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)	;Don't mung ouside value
	(SUBST-FLAG)				;T if this is a SUBST being compiled.
						; Always put interpreted defn in debug info.
	(INHIBIT-SPECIAL-WARNINGS)
	(CLOBBER-NONSPECIAL-VARS-LISTS))
    (BEGIN-PROCESSING-FUNCTION FUNCTION-TO-BE-DEFINED)
    (WHEN (LIST-MATCH-P FUNCTION-TO-BE-DEFINED
			`(:PROPERTY ,IGNORE :NAMED-STRUCTURE-INVOKE))
      (WARN 'OBSOLETE-PROPERTY :IMPLAUSIBLE
	    "NAMED-STRUCTURE-INVOKE, the property name, should not be a keyword."))
    ;; If compiling a macro, compile its expansion function
    ;; and direct lap to construct a macro later.
    (WHEN (EQ (CAR EXP1) 'MACRO)
      (SETQ MACROFLAG T)
      (SETQ EXP1 (CDR EXP1))
      (SETQ DEF-TO-BE-SXHASHED EXP1))
    (UNLESS (MEMQ (CAR EXP1) '(LAMBDA SUBST CLI:SUBST NAMED-LAMBDA NAMED-SUBST))
      (WARN 'FUNCTION-NOT-VALID :FATAL "The definition is not a function at all.")
      (RETURN-FROM QCOMPILE0 NIL))
    (IF (MEMQ (CAR EXP1) '(SUBST NAMED-SUBST CLI:SUBST))
	(SETQ SUBST-FLAG T INHIBIT-SPECIAL-WARNINGS T))
    ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
    (WHEN (MEMQ (CAR EXP1) '(NAMED-LAMBDA NAMED-SUBST))
      (SETQ EXPR-DEBUG-INFO (CDR-SAFE (CADR EXP1))
	    EXP1 (CDR EXP1))
      ;; Debug info that is equivalent to declarations
      ;; should be turned back into declarations, coming before
      ;; declarations made outside of compilation
      ;; but after anything coming from a DECLARE in the body.
      (DOLIST (ELT (REVERSE EXPR-DEBUG-INFO))
	(WHEN (AND (NEQ (CAR ELT) 'DOCUMENTATION)
		   (GET (CAR ELT) 'SI::DEBUG-INFO))
	  (PUSH ELT LOCAL-DECLARATIONS))))
    (SETQ LL (CADR EXP1))			;lambda list.
    (SETQ BODY (CDDR EXP1))
    ;; Record the function's arglist for warnings about recursive calls.
    (OR THIS-FUNCTION-ARGLIST-FUNCTION-NAME
	(SETQ THIS-FUNCTION-ARGLIST-FUNCTION-NAME NAME-TO-GIVE-FUNCTION
	      THIS-FUNCTION-ARGLIST LL))
    ;; Extract documentation string and declarations from the front of the body.
    (MULTIPLE-VALUE-SETQ (BODY LOCAL-DECLARATIONS DOCUMENTATION)
;>> need to pass environment into extract-declarations here
      (EXTRACT-DECLARATIONS BODY LOCAL-DECLARATIONS *FUNCTION-ENVIRONMENT*))
    (SETQ SELF-FLAVOR-DECLARATION
	  (CDR (ASSQ ':SELF-FLAVOR LOCAL-DECLARATIONS)))
    ;; If the user just did (declare (:self-flavor flname)),
    ;; compute the full declaration for that flavor.
    (WHEN (AND SELF-FLAVOR-DECLARATION
	       (NULL (CDR SELF-FLAVOR-DECLARATION)))
      (SETQ SELF-FLAVOR-DECLARATION
	    (CDR (SI::FLAVOR-DECLARATION (CAR SELF-FLAVOR-DECLARATION)))))
    ;; Actual DEFMETHODs must always have SELF-FLAVOR
    (WHEN (EQ (CAR-SAFE FUNCTION-TO-BE-DEFINED) :METHOD)
      (SETQ SELF-REFERENCES-PRESENT T))
    ;; Process &KEY and &AUX vars, if there are any.
    (WHEN (OR (MEMQ '&KEY LL) (MEMQ '&AUX LL))
      ;; Put arglist together with body again.
      (LET ((LAMEXP `(LAMBDA ,LL (DECLARE . ,LOCAL-DECLARATIONS) . ,BODY)))
	;; If there are keyword arguments, expand them.
	(AND (MEMQ '&KEY LL)
	     (SETQ LAMEXP (EXPAND-KEYED-LAMBDA LAMEXP)))
	;; Now turn any &AUX variables in the LAMBDA into a LET* in the body.
	(SETQ LAMEXP (P1AUX LAMEXP))
	;; Separate lambda list and body again.
	(SETQ LL (CADR LAMEXP) BODY (CDDR LAMEXP)))
      ;; Can just pop off the declarations as we have them already from above
      (DO () ((NEQ (CAR-SAFE (CAR BODY)) 'DECLARE))
	(POP BODY)))
    ;; Create the arglist accesible through (arglist foo 'compile)
    (LET ((L NIL))
      (DOLIST (X (CADR EXP1))
	(UNLESS (MEMQ X '(&SPECIAL &LOCAL))
	  (PUSH (COND ((EQ X '&AUX) (RETURN))
		      ((ATOM X) X)		;foo, &optional, etc
		      ((CONSP (CAR X))		;((:foo bar)), ((:foo bar) baz foop), etc
		       (IF (CADR X)
			   (LIST (CAAR X) (CADR X))
			 (CAAR X)))
		      (T			;(foo), (foo bar), (foo bar foop)
		       (IF (CADR X)
			   (LIST (CAR X) (CADR X))
			 (CAR X))))
		L)))
      (SETQ L (NREVERSE L))
      (UNLESS (EQUAL L LL)
	(PUSH (CONS 'COMPILER-ARGLIST L) LOCAL-DECLARATIONS)))
    ;; Now process the variables in the lambda list, after the local declarations.
    (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL LOCAL-DECLARATIONS))
    (COND ((NOT (NULL (CDR BODY)))
	   (SETQ EXP1 `(PROGN . ,BODY)))
	  ((SETQ EXP1 (CAR BODY))))
    (SETQ EXP1 (P1 EXP1))			;Do pass 1 to single-expression body
    (SETQ LVCNT (ASSIGN-LAP-ADDRESSES))
    ;; Now that we know all the variables needed by lexical closures,
    ;; make a list of them and put them into the entries in COMPILER-QUEUE
    ;; for each of those lexical closures.
    (UNLESS (ZEROP *LEXICAL-CLOSURE-COUNT*)
      (SETQ VARIABLES-USED-IN-LEXICAL-CLOSURES
	    (RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES)))
    (OUTF `(MFEF ,FUNCTION-TO-BE-DEFINED ,SPECIALFLAG
		 ,(ELIMINATE-DUPLICATES-AND-REVERSE ALLVARS)
		 ,FREEVARS ,NAME-TO-GIVE-FUNCTION))
    (IF MACROFLAG (OUTF `(CONSTRUCT-MACRO)))
    (OUTF `(QTAG S-V-BASE))
    (OUTF `(S-V-BLOCK))
    (IF (AND SELF-FLAVOR-DECLARATION SELF-REFERENCES-PRESENT)
	(OUTF `(SELF-FLAVOR . ,SELF-FLAVOR-DECLARATION)))
    (OUTF `(QTAG DESC-LIST-ORG))
    (OUTF `(PARAM LLOCBLOCK
		  ,(IF (ZEROP *LEXICAL-CLOSURE-COUNT*)
		       LVCNT
		     (+ LVCNT (* 4 *LEXICAL-CLOSURE-COUNT*) 3
			(LENGTH VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
    (OUTF `(A-D-L))
    (OUTF `(QTAG QUOTE-BASE))
    (OUTF `(ENDLIST))				;Lap will insert quote vector here
    (WHEN (NOT (ZEROP *LEXICAL-CLOSURE-COUNT*))
      (OUTF `(VARIABLES-USED-IN-LEXICAL-CLOSURES
	       . ,(REVERSE (MAPCAR #'(LAMBDA (HOME)
				       (LET ((TEM (VAR-LAP-ADDRESS HOME)))
					 (SELECTQ (CAR TEM)
					   (ARG (CADR TEM))
					   (T (%LOGDPB 1 %%Q-BOXED-SIGN-BIT (CADR TEM))))))
				   VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
    ;; Set up the debug info from the local declarations and other things
    (LET ((DEBUG-INFO NIL) TEM)
      (AND DOCUMENTATION (PUSH `(DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
      (DOLIST (DCL LOCAL-DECLARATIONS)
	(WHEN (SYMBOLP (CAR DCL))
	  (SETQ TEM (GET (CAR DCL) 'SI::DEBUG-INFO))
	  (IF (EQ TEM T) (SETQ TEM (CAR DCL)))
	  (UNLESS (ASSQ TEM DEBUG-INFO)
	    (PUSH (IF (EQ TEM (CAR DCL)) DCL (CONS TEM (CDR DCL))) DEBUG-INFO))))
      ;; Propagate any other kinds of debug info from the expr definition.
      (DOLIST (DCL EXPR-DEBUG-INFO)
	(UNLESS (ASSQ (CAR DCL) DEBUG-INFO)
	  (PUSH DCL DEBUG-INFO)))
      (WHEN (PLUSP *BREAKOFF-COUNT*)		; local functions
	(LET ((INTERNAL-OFFSETS (MAKE-LIST *BREAKOFF-COUNT*)))
	  (OUTF `(BREAKOFFS ,INTERNAL-OFFSETS))
	  (PUSH `(:INTERNAL-FEF-OFFSETS . ,INTERNAL-OFFSETS) DEBUG-INFO)))
      ;; Include the local and arg maps if we have them.
      ;; They were built by ASSIGN-LAP-ADDRESSES.
      (WHEN LOCAL-MAP (PUSH `(LOCAL-MAP ,LOCAL-MAP) DEBUG-INFO))
      (WHEN ARG-MAP (PUSH `(ARG-MAP ,ARG-MAP) DEBUG-INFO))
      (WHEN LOCAL-FUNCTION-MAP (PUSH `(LOCAL-FUNCTION-MAP ,(NREVERSE LOCAL-FUNCTION-MAP))
				     DEBUG-INFO))
      ;; Include list of macros used, if any.
      (WHEN MACROS-EXPANDED
	(LET ((MACROS-AND-SXHASHES
		(MAPCAR #'(LAMBDA (MACRONAME)
			    (LET ((HASH (EXPR-SXHASH MACRONAME)))
			      (IF (OR HASH (CONSP MACRONAME))
				  (LIST MACRONAME HASH)
				MACRONAME)))
			MACROS-EXPANDED)))
	  (IF QC-FILE-RECORD-MACROS-EXPANDED
	      (PROGN
		;; If in QC-FILE, put just macro names in the function
		;; but put the names and sxhashes into the file's list.
		(PUSH `(:MACROS-EXPANDED ,MACROS-EXPANDED) DEBUG-INFO)
		(DOLIST (M MACROS-AND-SXHASHES)
		  (OR (MEMBER-EQUAL M QC-FILE-MACROS-EXPANDED)
		      (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			(PUSH (COPYTREE M) QC-FILE-MACROS-EXPANDED)))))
	    (PUSH `(:MACROS-EXPANDED ,MACROS-AND-SXHASHES)
		  DEBUG-INFO))))
      (AND (OR (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
	       SUBST-FLAG)
	   (PUSH `(INTERPRETED-DEFINITION ,EXP) DEBUG-INFO))
      (WHEN SUBST-FLAG
	(LET* ((ARGS-INFO (ARGS-INFO EXP))
	       (DUMMY-FORM (CONS 'FOO
				 (MAKE-LIST (+ (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)
					       (IF (LDB-TEST %ARG-DESC-EVALED-REST ARGS-INFO)
						   1 0))
					    :INITIAL-ELEMENT '(GENSYM)))))
;>> this is very bogus. The environment should be much hairier. Or should it?
	  (UNLESS (WITH-STACK-LIST (ENV *FUNCTION-ENVIRONMENT*)
		    (EQUAL (SI::SUBST-EXPAND EXP DUMMY-FORM ENV NIL)
			   (SI::SUBST-EXPAND EXP DUMMY-FORM ENV T)))
	    ;; If simple and thoughtful substitution give the same result
	    ;; even with the most intractable arguments,
	    ;; we need not use thoughtful substitution for this defsubst.
	    ;; Otherwise, mark it as requiring thoughtful substitution.
	    (PUSH '(:NO-SIMPLE-SUBSTITUTION T) DEBUG-INFO))))
      ;; Compute the sxhash now, after all displacing macros have been displaced
      (AND MACROFLAG
	   (PUSH `(:EXPR-SXHASH ,(FUNCTION-EXPR-SXHASH DEF-TO-BE-SXHASHED)) DEBUG-INFO))
      ;; If we aren't going to mark this function as requiring a mapping
      ;; table, provide anyway some info that the user declared it wanted one.
      (AND SELF-FLAVOR-DECLARATION (NOT SELF-REFERENCES-PRESENT)
	   (PUSH `(:SELF-FLAVOR ,(CAR SELF-FLAVOR-DECLARATION)) DEBUG-INFO))
      (AND DEBUG-INFO
	   (OUTF `(DEBUG-INFO . ,DEBUG-INFO))))
    (OUTF `PROGSA)
    (P2SBIND LL VARS NIL)			;Can compile initializing code
    (LET ((*LEXICAL-CLOSURE-COUNT* 0))
      (P2 EXP1 'D-RETURN))			;Do pass 2
    (OUTF `(PARAM MXPDL ,(1+ MAXPDLLVL)))
    ALLVARS))

))

; From file PATED.LISP OZ:<L.ZWEI> OZ: (27)
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN ADD-PATCH-INTERVAL (BP1 BP2 IN-ORDER-P DEFUN-NAME BUFFER &AUX NEW-PATCH-BUFFER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (VALIDATE-PATCH-BUFFER)
  (SETQ NEW-PATCH-BUFFER-P (NULL *PATCH-BUFFER*))
  (AND NEW-PATCH-BUFFER-P (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME)))
  (FORMAT *QUERY-IO*
	  "~&Adding ~:[~S~;~A~] to patch file ~A~@[~%(New patch file)~]"
	  (STRINGP DEFUN-NAME) DEFUN-NAME (PATCH-VERSION-DESCRIPTION) NEW-PATCH-BUFFER-P)
  (LET ((BP (INTERVAL-LAST-BP *PATCH-BUFFER*)))
    ;; Put into the patch buffer, making sure the right package and base will be used.
    (MULTIPLE-VALUE-BIND (VARS VALS)
	(SEND BUFFER :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	     (INSERT BP (FORMAT NIL "~%; From file ~A~A~%#~DR ~A#:
/(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE /"~:*~A/")))
/  (COMPILER#:PATCH-SOURCE-FILE ~S

"
				(BUFFER-NAME BUFFER) (BUFFER-VERSION-STRING BUFFER)
				*READ-BASE* *PACKAGE*
				(WHEN (BUFFER-GENERIC-PATHNAME BUFFER)
				  (SEND (BUFFER-GENERIC-PATHNAME BUFFER)
					:STRING-FOR-PRINTING))))))
    (INSERT-INTERVAL BP BP1 BP2 T)
    (INSERT BP "
))
"))
  ;; Mark all sections that the region contains part of
  ;; as having been patched.
  (INTERVAL-LINES (BP1 BP2) (START-LINE END-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (LAST-SECTION))
	((EQ LINE END-LINE))
      (LET ((SECTION (LINE-NODE LINE)))
	(UNLESS (EQ LAST-SECTION SECTION)
	  (PUTPROP SECTION *TICK* 'PATCH-TICK))
	(SETQ LAST-SECTION SECTION)))))

))

; From file READ.LISP OZ:<L.IO> OZ: (435)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#/|-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (PROG ((N 0))
	(GO HOME)
     SHARP
	(CASE (SEND STREAM :TYI)
	  (#/# (GO SHARP))
	  (#/| (INCF N))
	  (#// (SEND STREAM :TYI))
	  ((NIL) (GO BARF)))
     HOME
	(CASE (SEND STREAM :TYI)
	  (#/| (GO BAR))
	  (#/# (GO SHARP))
	  (#// (SEND STREAM :TYI)
	       (GO HOME))
	  ((NIL) (GO BARF))
	  (T (GO HOME)))
     BAR
	(CASE (SEND STREAM :TYI)
	  (#/# (IF (ZEROP N) (RETURN)
		 (DECF N)
		 (GO HOME)))
	  (#/| (GO BAR))
	  (#// (SEND STREAM :TYI)
	       (GO HOME))
	  ((NIL) (GO BARF))
	  (T (GO HOME)))
   BARF (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "The end of file was reached while reading a #/| comment.")))


(DEFUN SET-SYNTAX-FROM-CHAR (TO-CHAR FROM-CHAR &OPTIONAL
			     (TO-READTABLE *READTABLE*) (FROM-READTABLE *READTABLE*))
  "Copy the syntax of FROM-CHAR in FROM-READTABLE to TO-CHAR in TO-READTABLE, Common Lisp style.
Common Lisp has a peculiar definition of just what it is about
the character syntax that can be copied, while other aspects
are supposed to be immutable for a particular character.
This function contains hair to copy only the mutable part of the syntax.
To copy all aspects of the syntax, use COPY-SYNTAX."
; character lossage
  (IF (CHARACTERP TO-CHAR) (SETQ TO-CHAR (CHAR-INT TO-CHAR)))
  (IF (CHARACTERP FROM-CHAR) (SETQ FROM-CHAR (CHAR-INT FROM-CHAR)))
  (LET ((BITS (RDTBL-BITS FROM-READTABLE FROM-CHAR))
	(CODE (RDTBL-CODE FROM-READTABLE FROM-CHAR))
	(FROM-AENTRY (ASSQ FROM-CHAR (RDTBL-MACRO-ALIST FROM-READTABLE)))
	(TO-AENTRY (ASSQ TO-CHAR (RDTBL-MACRO-ALIST TO-READTABLE)))
	SYNTAX-STANDARD)
    ;; Find a character which standardly posesses the syntax that is being copied.
    (DOTIMES (I (ARRAY-DIMENSION INITIAL-COMMON-LISP-READTABLE 1))
      (WHEN (AND (= (RDTBL-BITS INITIAL-COMMON-LISP-READTABLE I) BITS)
		 (= (RDTBL-CODE INITIAL-COMMON-LISP-READTABLE I) CODE))
	(SETQ SYNTAX-STANDARD I)))
    ;; Check for Space, Return, etc. set to Constituent syntax.
    (WHEN (AND (NULL SYNTAX-STANDARD)
	       (EQUAL (GET-SYNTAX-BITS FROM-CHAR INITIAL-COMMON-LISP-READTABLE)
		      (GETF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE) 'ILLEGAL)))
      (SETQ SYNTAX-STANDARD #/A))
    (COND ((OR (NULL SYNTAX-STANDARD) (EQ SYNTAX-STANDARD #/#))
	   ;; Standardize any kind of nonterminating macro.
	   ;; NIL for SYNTAX-STANDARD must mean that, since that is the only kind of syntax
	   ;; that can get used due to Common Lisp actions
	   ;; which isn't present in the standard Common Lisp readtable.
	   (SETQ SYNTAX-STANDARD (IF (= TO-CHAR #/#) #/# NIL)))
	  ;; Standardize any kind of constituent character to an ordinary one.
	  ((OR (ALPHA-CHAR-P SYNTAX-STANDARD)
	       (MEMQ SYNTAX-STANDARD '(#/+ #/- #/. #// #/: #/^ #/_ #/!)))
	   (IF (OR (ALPHA-CHAR-P TO-CHAR)
		   (MEMQ TO-CHAR '(#/+ #/- #/. #// #/: #/^ #/_)))
	       (SETQ SYNTAX-STANDARD TO-CHAR)
	     (IF (MEMQ TO-CHAR '(#/SPACE #/RETURN #/LF #/TAB #/RUBOUT #/BS #/PAGE))
		 (SETQ SYNTAX-STANDARD 'ILLEGAL)
	       (SETQ SYNTAX-STANDARD #/!)))))
    (IF (NULL SYNTAX-STANDARD)
	(SET-SYNTAX-BITS TO-CHAR (GETF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE)
				      'NON-TERMINATING-MACRO)
			 TO-READTABLE)
      (IF (EQ SYNTAX-STANDARD 'ILLEGAL)
	  (SET-SYNTAX-BITS TO-CHAR
			   (GETF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE) 'ILLEGAL)
			   TO-READTABLE)
	(SET-SYNTAX-BITS TO-CHAR
			 (GET-SYNTAX-BITS SYNTAX-STANDARD INITIAL-COMMON-LISP-READTABLE)
			 TO-READTABLE)))
    (SETF (RDTBL-MACRO-ALIST TO-READTABLE)
	  (DELQ TO-AENTRY (RDTBL-MACRO-ALIST TO-READTABLE)))
    (IF FROM-AENTRY
	(PUSH (COPY-TREE FROM-AENTRY) (RDTBL-MACRO-ALIST TO-READTABLE)))))

(DEFUN SET-MACRO-CHARACTER (CHAR FUNCTION &OPTIONAL NON-TERMINATING-P
			    (A-READTABLE *READTABLE*))
  "Set the syntax of CHAR in A-READTABLE to be a macro character that invokes FUNCTION.
NON-TERMINATING-P non-NIL means the macro char is recognized only
 at the start of a token; it can appear unquoted in the middle of a symbol.
FUNCTION is passed the input stream and the character that invoked it.
It should return zero or more values, each of which is an object produced
by the macro.  Zero and one are the most common numbers of values.
More than one may not be accepted in certain contexts.

The function can examine the list of input so far at the current level
 through the special variable XR-LIST-SO-FAR.  It can also be
 :TOPLEVEL if not within list, or :AFTER-DOT if after a dot.

A function can also hairily mung the list so far.
To do this, modify XR-LIST-SO-FAR and return the modified list as
 the only value, after setting the special variable XR-SPLICE-P to T."
; character lossage
  (IF (CHARACTERP CHAR) (SETQ CHAR (CHAR-INT CHAR)))
  (LET ((SYNTAX (GETF (RDTBL-PLIST A-READTABLE)
		      (IF NON-TERMINATING-P 'NON-TERMINTING-MACRO 'MACRO))))
    (UNLESS (AND (CONSP SYNTAX)
		 (FIXNUMP (CAR SYNTAX))
		 (FIXNUMP (CDR SYNTAX)))
      (FERROR NIL "No saved syntax found for defining macro characters."))
    (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
    (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX))
    (LET ((X (ASSQ CHAR (RDTBL-MACRO-ALIST A-READTABLE))))
      (IF (NULL X)
	  (SETF (RDTBL-MACRO-ALIST A-READTABLE)
		(CONS (LIST CHAR FUNCTION NON-TERMINATING-P) (RDTBL-MACRO-ALIST A-READTABLE)))
	(SETF (CDR X) (LIST FUNCTION NON-TERMINATING-P)))))
  T)


(DEFUN GET-MACRO-CHARACTER (CHAR &OPTIONAL (A-READTABLE *READTABLE*))
  "Return the function called for macro character CHAR in readtable A-READTABLE.
Value is NIL if CHAR is not a macro character.
Second value is non-NIL if CHAR does not terminate symbols."
  (DECLARE (VALUES FUNCTION NON-TERMINATING-P))
;character lossage
  (IF (CHARACTERP CHAR) (SETQ CHAR (CHAR-INT CHAR)))
  (LET* ((AENTRY (ASSQ CHAR (RDTBL-MACRO-ALIST A-READTABLE))))
    (VALUES (CADR AENTRY)
	    (CADDR AENTRY))))

(DEFUN SET-DISPATCH-MACRO-CHARACTER (DISP-CHAR SUB-CHAR FUNCTION
				     &OPTIONAL (A-READTABLE *READTABLE*))
  "Set the function run by SUB-CHAR when it is seen following DISP-CHAR.
DISP-CHAR must be a dispatch macro character, such as made by MAKE-DISPATCH-MACRO-CHARACTER.
FUNCTION is the function to be run.  It will receive three arguments:
 the input stream, the sub-character dispatched on, and the numeric argument
 that followed the dispatch character (or NIL if no numeric argument)."
;character lossage
  (IF (CHARACTERP DISP-CHAR) (SETQ DISP-CHAR (CHAR-INT DISP-CHAR)))
  (IF (CHARACTERP SUB-CHAR) (SETQ SUB-CHAR (CHAR-INT SUB-CHAR)))
  (LET* ((AENTRY (ASSQ DISP-CHAR (RDTBL-MACRO-ALIST A-READTABLE)))
	 (SUBAENTRY (ASSQ (CHAR-UPCASE SUB-CHAR) (CDDDR AENTRY))))
    (UNLESS AENTRY
      (FERROR NIL "~@C is not a macro character." DISP-CHAR))
    (UNLESS (EQ (CADR AENTRY) 'XR-DISPATCH-MACRO-DRIVER)
      (FERROR NIL "~@C is not a dispatch macro character." DISP-CHAR))
    (IF SUBAENTRY (SETF (CADR SUBAENTRY) FUNCTION)
      (PUSH (LIST (CHAR-UPCASE SUB-CHAR) FUNCTION) (CDDDR AENTRY)))))

(DEFUN GET-DISPATCH-MACRO-CHARACTER (DISP-CHAR SUB-CHAR
				     &OPTIONAL (A-READTABLE *READTABLE*))
  "Return the function run by SUB-CHAR when encountered after DISP-CHAR.
DISP-CHAR must be a dispatch macro character, such as made by MAKE-DISPATCH-MACRO-CHARACTER.
Value is NIL if SUB-CHAR is not defined (as a subcase of DISP-CHAR)."
  (IF (CHARACTERP DISP-CHAR) (SETQ DISP-CHAR (CHAR-INT DISP-CHAR)))
  (IF (CHARACTERP SUB-CHAR) (SETQ SUB-CHAR (CHAR-INT SUB-CHAR)))
  (LET* ((AENTRY (ASSQ DISP-CHAR (RDTBL-MACRO-ALIST A-READTABLE)))
	 (SUBAENTRY (ASSQ (CHAR-UPCASE SUB-CHAR) (CDDDR AENTRY))))
    (UNLESS AENTRY
      (FERROR NIL "~@C is not a macro character." DISP-CHAR))
    (UNLESS (EQ (CADR AENTRY) 'XR-DISPATCH-MACRO-DRIVER)
      (FERROR NIL "~@C is not a dispatch macro character." DISP-CHAR))
    (AND SUBAENTRY
	 (NOT ( #/0 SUB-CHAR #/9))
	 (CADR SUBAENTRY))))

(DEFUN COPY-SYNTAX (TO-CHAR FROM-CHAR &OPTIONAL
		    (TO-READTABLE *READTABLE*) (FROM-READTABLE *READTABLE*))
  "Copy the syntax of FROM-CHAR in FROM-READTABLE to TO-CHAR in TO-READTABLE.
All aspects of the syntax are copied, including macro definition if any.
However, the meaning as a subdispatch of any dispatch macro characters
is not affected; that is recorded separately under those dispatch macros."
; character lossage
  (IF (CHARACTERP TO-CHAR) (SETQ TO-CHAR (CHAR-INT TO-CHAR)))
  (IF (CHARACTERP FROM-CHAR) (SETQ FROM-CHAR (CHAR-INT FROM-CHAR)))
  (LET ((FROM-AENTRY (ASSQ FROM-CHAR (RDTBL-MACRO-ALIST FROM-READTABLE)))
	(TO-AENTRY (ASSQ TO-CHAR (RDTBL-MACRO-ALIST TO-READTABLE))))
    (SET-SYNTAX-BITS TO-CHAR
		     (GET-SYNTAX-BITS FROM-CHAR FROM-READTABLE)
		     TO-READTABLE)
    (SETF (RDTBL-MACRO-ALIST TO-READTABLE)
	  (DELQ TO-AENTRY (RDTBL-MACRO-ALIST TO-READTABLE)))
    (IF FROM-AENTRY
	(PUSH (COPY-TREE FROM-AENTRY) (RDTBL-MACRO-ALIST TO-READTABLE))))
  T)

))

; From file QRAND.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN MAKE-ARRAY (DIMENSIONS &REST OPTIONS)
  "Create an array of size DIMENSIONS (a number or list of numbers).
The keywords are as follows:
:TYPE - specify array type, controlling type of elements allowed.  Default is ART-Q.
 ART-Q (any elements), ART-Q-LIST (any elements, and the contents looks like a list),
 ART-STRING (elements 0 through 255, printed with quotes),
 ART-FAT-STRING (16 bit unsigned elements, printed with quotes),
 ART-1B (elements 0 and 1), ART-2B (elements 0 through 3), ART-4B, ART-8B, ART-16B,
 ART-32B (elements any fixnum), ART-FLOAT (elements any full-size flonum),
 ART-COMPLEX (elements any number including complex numbers),
 ART-COMPLEX-FLOAT (elements complex numbers composed of two full-size flonums),
 ART-HALF-FIX (16 bit signed fixnum elements),
 ART-FPS-FLOAT ART-COMPLEX-FPS-FLOAT (used with floating point array processor),
 ART-STACK-GROUP-HEAD, ART-REGULAR-PDL, ART-SPECIAL-PDL (parts of stack groups).
:ELEMENT-TYPE - specify array type by specifying Common Lisp
 data type of elements allowed.  For example,
 an :ELEMENT-TYPE of (MOD 4) would get an ART-2B array.
:AREA - specify area to create the array in.
:LEADER-LENGTH - specify number of elements of array leader to make.
:LEADER-LIST - list whose elements are used to initialize the leader.
:FILL-POINTER - specify initial fill pointer value (ARRAY-ACTIVE-LENGTH of the array).
 Requests a leader of length 1 and specifies the contents of the slot.
:INITIAL-ELEMENT - value used to initialize all elements of the array.
:DISPLACED-TO - array, locative or fixnum specifying address of data
 that this array should overlap.
:DISPLACED-INDEX-OFFSET - if displaced to another array, this specifies
 which element of that array should correspond to element 0 of the new one.
:NAMED-STRUCTURE-SYMBOL - if not NIL, specifies a named structure symbol
 to be stored in the array, which should have its named-structure bit set.
:INITIAL-CONTENTS - value is a sequence of sequences of sequences...
 where the leaves are the values to initialize the array from.
 The top level of sequence corresponds to the most slowly varying subscript."
  (DECLARE (ARGLIST DIMENSIONS &KEY ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
		    		    FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET
				    TYPE AREA LEADER-LENGTH LEADER-LIST NAMED-STRUCTURE-SYMBOL
				    ADJUSTABLE))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
	ENTRIES-PER-Q
	LEADER-LIST FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET NAMED-STRUCTURE-SYMBOL
	(AREA NIL) (TYPE 'ART-Q) TYPE-P ELEMENT-TYPE-P
	INITIAL-ELEMENT INITIAL-ELEMENT-P INITIAL-CONTENTS INITIAL-CONTENTS-P
	ARRAY N-DIMENSIONS INDEX-LENGTH LONG-ARRAY-P LEADER-QS DATA-LENGTH LEADER-LENGTH)
    ;; Figure out whether it is old-style.
    (COND ((AND ( LENGTH-OF-OPTIONS 2)
		(OR (NUMBERP (FIRST OPTIONS))
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPES)
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPE-KEYWORDS)))
	   ;; It is old-style.  The first arg is actually AREA.
	   (SETQ AREA DIMENSIONS)
	   (SETQ TYPE (FIRST OPTIONS))
	   (SETQ DIMENSIONS (SECOND OPTIONS))
	   (SETQ DISPLACED-TO (THIRD OPTIONS))
	   (SETQ LEADER-LIST (FOURTH OPTIONS))
	   (SETQ DISPLACED-INDEX-OFFSET (FIFTH OPTIONS))
	   (SETQ NAMED-STRUCTURE-SYMBOL (SIXTH OPTIONS))
	   (IF (NUMBERP LEADER-LIST)
	       (SETQ LEADER-LENGTH LEADER-LIST
		     LEADER-LIST NIL)
	     (SETQ LEADER-LIST (REVERSE LEADER-LIST))))
	  (T
	   ;; It is new-style.
	   (UNLESS (EVENP LENGTH-OF-OPTIONS)
	     (FERROR NIL "Odd-length options list: ~S" OPTIONS))
	   (DO ((O OPTIONS (CDDR O)))
	       ((NULL O))
	     (LET ((VALUE (CADR O)))
	       (CASE (CAR O)
		 (:AREA (SETQ AREA VALUE))
		 (:TYPE (SETQ TYPE-P T)
			(SETQ TYPE (ARRAY-CANONICALIZE-TYPE VALUE)))
		 (:ELEMENT-TYPE (SETQ ELEMENT-TYPE-P T)
		  (SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE)))
		 (:DISPLACED-INDEX-OFFSET
		  (SETQ DISPLACED-INDEX-OFFSET VALUE))
		 (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
		 ((:INITIAL-ELEMENT :INITIAL-VALUE)
		  (SETQ INITIAL-ELEMENT VALUE INITIAL-ELEMENT-P T))
		 (:INITIAL-CONTENTS
		  (SETQ INITIAL-CONTENTS VALUE INITIAL-CONTENTS-P T))
		 (:FILL-POINTER (SETQ LEADER-LENGTH (MAX 1 (OR LEADER-LENGTH 1)))
				(SETQ FILL-POINTER VALUE))
		 (:ADJUSTABLE)
		 (:LEADER-LIST (SETQ LEADER-LIST VALUE))
		 (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
;		 (:OLD-LEADER-LENGTH-OR-LIST (IF (NUMBERP VALUE)
;						 (SETQ LEADER-LENGTH VALUE)
;					       (SETQ LEADER-LIST (REVERSE VALUE))))
		 (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
		 (OTHERWISE
		  (FERROR NIL "~S is not a known MAKE-ARRAY keyword." (FIRST OPTIONS))))))))
    (IF (AND TYPE-P ELEMENT-TYPE-P)
	(FERROR NIL "Both :TYPE and :ELEMENT-TYPE specified."))
    (IF (AND DISPLACED-INDEX-OFFSET (NOT DISPLACED-TO))
	(FERROR NIL "The :DISPLACED-INDEX-OFFSET option specified without :DISPLACED-TO."))
    (IF (AND INITIAL-ELEMENT-P INITIAL-CONTENTS-P)
	(FERROR NIL "Both :INITIAL-ELEMENT and :INITIAL-CONTENTS specified."))
    ;; Process the DIMENSIONS argument.
    (CHECK-TYPE DIMENSIONS (OR FIXNUM LIST))
    (COND ((FIXNUMP DIMENSIONS)
	   (IF (MINUSP DIMENSIONS)
	       (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	   (SETQ N-DIMENSIONS 1
		 INDEX-LENGTH DIMENSIONS))
	  ((OR (NULL DIMENSIONS) (CONSP DIMENSIONS))
	   (DOLIST (DIM DIMENSIONS)
	     (UNLESS (FIXNUMP DIM)
	       (FERROR NIL "The dimension ~S is not a fixnum." DIM))
	     (IF (MINUSP DIM)
		 (FERROR NIL "The negative array dimension ~S is illegal." DIM)))
	   (SETQ N-DIMENSIONS (LENGTH DIMENSIONS))
	   (IF (> N-DIMENSIONS ARRAY-RANK-LIMIT)
	       (FERROR NIL "Arrays may only at most ~D, dimensions, not ~S"
		       ARRAY-RANK-LIMIT N-DIMENSIONS))
	   (SETQ INDEX-LENGTH (APPLY #'TIMES DIMENSIONS))))	;* loses in cold load, TIMES wins.
    ;; Process the DISPLACED-TO argument.
    (CHECK-TYPE DISPLACED-TO (OR NULL FIXNUM ARRAY LOCATIVE))
    ;; See whether this is a "short" or "long" format array.
    (IF (AND (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (NOT DISPLACED-TO))
	(SETQ LONG-ARRAY-P T))
    (OR (FIXNUMP INDEX-LENGTH)
	(FERROR NIL "Attempt to make array too large; total length ~S" INDEX-LENGTH))
    ;; Process the LEADER and NAMED-STRUCTURE-SYMBOL arguments.
    (CHECK-TYPE LEADER-LIST LIST)
    (AND (NULL LEADER-LENGTH) (NOT (NULL LEADER-LIST))
	 (SETQ LEADER-LENGTH (LENGTH LEADER-LIST)))
    (IF (AND LEADER-LENGTH (> (LENGTH LEADER-LIST) LEADER-LENGTH))
	(FERROR NIL "Length of leader initialization list is greater than leader length"))
    (COND (NAMED-STRUCTURE-SYMBOL
	   (IF LEADER-LENGTH
	       (SETQ LEADER-LENGTH (MAX LEADER-LENGTH 2))
	     (UNLESS (= N-DIMENSIONS 1)
	       (FERROR NIL "A named-structure array may not be ~D-dimensional"
		       N-DIMENSIONS))))
	  (LEADER-LENGTH
	   (CHECK-TYPE LEADER-LENGTH (INTEGER 0))))
    (SETQ LEADER-QS (IF LEADER-LENGTH
			(+ 2 LEADER-LENGTH)
		        0))
    ;; Process the TYPE argument.
    (CHECK-ARG TYPE (OR (FIXNUMP TYPE)
			(MEMQ TYPE ARRAY-TYPES)
			(MEMQ TYPE ARRAY-TYPE-KEYWORDS))
	       "an array type")
    (IF (FIXNUMP TYPE)
	;; may be either a small integer, which is shifted over into the type field,
	;; or an already shifted over quantity (such as ART-Q, etc).
	(IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
	    (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
      (IF (FIXNUMP (SYMBOL-VALUE TYPE))
	  (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD (SYMBOL-VALUE TYPE)))
	(SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))))
    (SETQ ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE))
    ;; This is positive if there are 1 or more entries per Q.  It is
    ;; negative if there are more than one Qs per entry.
    (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			  (CEILING INDEX-LENGTH ENTRIES-PER-Q)
			(* INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
    ;; Process the DISPLACED-INDEX-OFFSET argument.
    (CHECK-TYPE DISPLACED-INDEX-OFFSET (OR NULL (FIXNUM 0)))
    (LET ((HEADER-WORD
	    ;; Put in array type and number of dims.
	    (%LOGDPB N-DIMENSIONS %%ARRAY-NUMBER-DIMENSIONS
		     (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
      ;; If there is a leader, set the flag.
      (IF LEADER-LENGTH
	  (SETQ HEADER-WORD (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-WORD)))
      (SETQ HEADER-WORD
	    (COND (DISPLACED-TO
		   ;; Array is displaced; turn on the bit, and the array is 2 long
		   ;; plus one for the index-offset if any.
		   (+ (%LOGDPB 1 %%ARRAY-DISPLACED-BIT HEADER-WORD)
		      (IF DISPLACED-INDEX-OFFSET 3 2)))
		  (LONG-ARRAY-P
		   ;; It is local; if it is a long array, the length is not in the
		   ;; header at all; set the bit instead.
		   (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-WORD))
		  (T
		   ;; It is a short array; the length is in the header.
		   (+ INDEX-LENGTH HEADER-WORD))))
      ;; Create the array.
      (SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-WORD
						  INDEX-LENGTH
						  (OR LEADER-LENGTH 0)
						  AREA
						  (+ (MAX 1 N-DIMENSIONS)
						     LEADER-QS
						     (COND (DISPLACED-TO
							    (IF DISPLACED-INDEX-OFFSET 3 2))
							   (LONG-ARRAY-P
							    (1+ DATA-LENGTH))
							   (T DATA-LENGTH)))
						  )))
    (WHEN (CONSP DIMENSIONS)
      ;; It is a multi-dimensional array.  Fill in the "dope vector".
      ;; If last index varies fastest, put in all but first dimension,
      ;; and the last dimensions come last.
      (DO ((DIMLIST (CDR DIMENSIONS) (CDR DIMLIST))
	   (I (+ N-DIMENSIONS (IF LONG-ARRAY-P 0 -1)) (1- I)))
	  ((NULL DIMLIST))
	(%P-STORE-CONTENTS-OFFSET (CAR DIMLIST) ARRAY I)))
    (COND (DISPLACED-TO
	   ;; It is displaced.  Put information after the dope vector, and after
	   ;; the "long array" word if any.
	   (LET ((IDX (IF LONG-ARRAY-P (1+ N-DIMENSIONS) N-DIMENSIONS)))
	     (%P-STORE-CONTENTS-OFFSET DISPLACED-TO ARRAY IDX)
	     (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ IDX))
	     (COND (DISPLACED-INDEX-OFFSET
		    ;; Index offset feature is in use.
		    ;; Store the index offset in the next Q.
		    (%P-STORE-CONTENTS-OFFSET DISPLACED-INDEX-OFFSET ARRAY (+ IDX 2)))))))
    ;; The leader's initial values were specified.
    (DO ((I 0 (1+ I))
	 (LEADER-LIST LEADER-LIST (CDR LEADER-LIST)))
	((NULL LEADER-LIST))
      (SETF (ARRAY-LEADER ARRAY I) (CAR LEADER-LIST)))
    (AND FILL-POINTER
	 (SETF (FILL-POINTER ARRAY) FILL-POINTER))
    ;; Cretinism associated with make-array, in that the leader list can overlap
    ;; with the name-structure slot, which is how fasd dumps the named-structure-symbol
    ;; So we check for the symbol being t and not smash it in that case
    (WHEN NAMED-STRUCTURE-SYMBOL
      (IF (NULL LEADER-LENGTH)
	  ;; There is no leader; put it in element zero of the body.
	  (SETF (AREF ARRAY 0) NAMED-STRUCTURE-SYMBOL)
	;; There is a leader; use element one of the leader.
	(IF (NEQ NAMED-STRUCTURE-SYMBOL T)
	    (SETF (ARRAY-LEADER ARRAY 1) NAMED-STRUCTURE-SYMBOL)))
      ;; It is a named structure.  Set the flag.
      (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0))
    (AND INITIAL-ELEMENT-P (NOT DISPLACED-TO)
	 (ARRAY-INITIALIZE ARRAY INITIAL-ELEMENT))
    (WHEN INITIAL-CONTENTS-P
      (FILL-ARRAY-FROM-SEQUENCES ARRAY INITIAL-CONTENTS 0 0))
    ;; If there is a fill pointer on an art-q-list array, then it should control
    ;; the length of the list as well.  See array-push and array-pop.
    (WHEN (AND (= N-DIMENSIONS 1)
	       (OR FILL-POINTER (NOT (NULL LEADER-LIST)))
	       ;; The cold load generator's frame builder isn't smart enough for a #, here.
	       (= TYPE #|'#,|# (LDB %%ARRAY-TYPE-FIELD ART-Q-LIST)))
      (OR FILL-POINTER (SETQ FILL-POINTER (CAR LEADER-LIST)))
      (WHEN (AND (FIXNUMP FILL-POINTER)
		 (> FILL-POINTER 0)
		 (< FILL-POINTER (ARRAY-LENGTH ARRAY)))
	(%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- FILL-POINTER)))))
    (VALUES ARRAY DATA-LENGTH)))

))

; From file QRAND.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN ARRAY-TYPE-NULL-ELEMENT (TYPE)
  (CDR (ASSQ (ARRAY-CANONICALIZE-TYPE TYPE)
	     '((ART-ERROR . NIL)
	       (ART-1B . 0)
	       (ART-2B . 0)
	       (ART-4B . 0)
	       (ART-8B . 0)
	       (ART-16B . 0)
	       (ART-32B . 0)
	       (ART-Q . NIL)
	       (ART-Q-LIST . NIL)
	       (ART-STRING . #/ )
	       (ART-STACK-GROUP-HEAD . NIL)
	       (ART-SPECIAL-PDL . NIL)
	       (ART-HALF-FIX . 0)
	       (ART-REG-PDL . NIL)
	       (ART-FLOAT . 0f0)
	       (ART-FPS-FLOAT . 0f0)
	       (ART-FAT-STRING . #/ )
	       (ART-COMPLEX-FLOAT . 0f0+0f0i)
	       (ART-COMPLEX . 0)
	       (ART-COMPLEX-FPS-FLOAT 0f0+0f0i)))))

))

; From file GENRIC.LISP OZ:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun reduce (&functional function sequence &key from-end (start 0) end
	       (initial-value nil initp))
  "Combine the elements of SEQUENCE using FUNCTION, a function of two args.
FUNCTION is applied to the first two elements; then to that result and the third element;
 then to that result and the fourth element; and so on.
START and END restrict the action to a part of SEQUENCE,
 as if the rest of SEQUENCE were not there.  They default to 0 and NIL
 (NIL for END means to the end of SEQUENCE).
If FROM-END is non-NIL, FUNCTION is applied to the last two elements;
 then to the previous element and that result; then to the previous
 element and that result; and so on.
If INITIAL-VALUE is specified, it acts like an extra element of SEQUENCE
 at the end (if FROM-END is non-NIL) or the beginning, in addition to
 the actual elements of the specified part of SEQUENCE.  Then there is
 effectively one more element to be processed.  The INITIAL-VALUE is
 used in the first call to FUNCTION.
If there is only one element to be processed,
 that element is returned and FUNCTION is not called.
If there are no elements (SEQUENCE is of length zero and no INITIAL-VALUE),
 FUNCTION is called with no arguments and its value is returned."
  (if from-end
      (if (arrayp sequence)
	  (do ((index (1- (or end (length sequence))) (1- index))
	       (notfirst initp t)
	       (accum initial-value))
	      ((< index start)
	       (if notfirst accum (funcall function)))
	    (if notfirst
		(setq accum (funcall function (cli:aref sequence index) accum))
	        (setq accum (cli:aref sequence index))))
	(let ((tail-recursion-flag t))
	  (reduce-list-backwards function (nthcdr start sequence)
				 (and end (- end start)) initial-value initp)))
    (do ((index (seq-start sequence start))
	 (end-index (seq-end sequence end))
	 (notfirst initp t)
	 (accum initial-value))
	((eq index end-index)
	 (if notfirst accum (funcall function)))
      (if notfirst
	  (setq accum (funcall function accum (seq-fetch-inc sequence index)))
	(setq accum (seq-fetch-inc sequence index))))))

))

; From file COMD.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFUN VARIABLE-STRING (VAR)
  "Return a string representing the value of ZWEI variable VAR."
  (LET ((*PACKAGE* (SYMBOL-PACKAGE 'FOO))
	(*PRINT-BASE* 10.)
	(*PRINT-RADIX* NIL)
	(*NOPOINT NIL)
	(VAL (SYMBOL-VALUE VAR))
	(TYPE (GET VAR 'VARIABLE-TYPE)))
    (CASE TYPE
      ((:BOOLEAN :KEYWORD :FIXNUM-OR-NIL :FIXNUM :ANYTHING :PIXEL :PIXEL-OR-NIL)
       (PRIN1-TO-STRING VAL))
      (:STRING (STRING VAL))
      (:CHAR (FORMAT NIL "~C" VAL))
      (:CHAR-LIST
       (DO ((VAL VAL (CDR VAL))
	    (STRING (MAKE-STRING (MAX (LENGTH VAL) 10.) :FILL-POINTER 0)))
	   ((NULL VAL) STRING)
	 (VECTOR-PUSH-EXTEND (CAR VAL) STRING))))))

(DEFUN GET-REGISTER-NAME (PROMPT &OPTIONAL PURPOSE &AUX CHAR ECHO-FLAG)
  "Read a register name in the echo area.
Puts PROMPT in the mode line.  Returns a symbol in the utility package.
The ZWEI:TEXT property of that symbol is the text contents.
The ZWEI:POINT property of it is a location saved on it."
  (SETQ CHAR (READ-CHAR-NO-HANG *STANDARD-INPUT*))
  (DO-FOREVER
    (WHEN (NULL CHAR)
      (FORMAT *QUERY-IO* "~&~A " PROMPT)
      (TYPEIN-LINE-ACTIVATE
	(SETQ CHAR (READ-CHAR *STANDARD-INPUT*)))
      (SETQ ECHO-FLAG T))
    (IF ( CHAR #/HELP)
	(RETURN)
      (SETQ CHAR NIL)
      (SEND *QUERY-IO* :CLEAR-SCREEN)
      (FORMAT *QUERY-IO* "You are typing the name of a ZWEI register~A.
A name is just a character sans meta bits; case is ignored."
	      (OR PURPOSE ""))))
  (UNLESS (STRING-CHAR-P CHAR)
    (SIGNAL EH:ABORT-OBJECT))
  (SETQ CHAR (CHAR-UPCASE CHAR))
  (IF ECHO-FLAG (FORMAT *QUERY-IO* "~C" CHAR))
  (MAKE-REGISTER-NAME CHAR))

))


; From file SHWARM.LISP OZ:<L.WINDOW> OZ: (329)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-TYO (SHEET CHAR &OPTIONAL FONT &AUX BASE-ADJ)
  "Output printing or formatting character CHAR on SHEET in FONT.
FONT defaults to the current font of SHEET.
Weird characters are printed in lozenges."
;character lossage
  (IF (CHARACTERP CHAR) (SETQ CHAR (CHAR-INT CHAR)))
  (CHECK-TYPE CHAR (INTEGER 0 (#o400)) "a character")
  (IF ( CHAR #o200)
      (COND ((AND (= CHAR #/NEWLINE) (ZEROP (SHEET-CR-NOT-NEWLINE-FLAG SHEET)))
             (SHEET-CRLF SHEET))
            ((= CHAR #/TAB)
             (SHEET-TAB-1 SHEET))
            ((AND (= CHAR #/BACKSPACE) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
             (SHEET-BACKSPACE-1 SHEET))
            (T
	     (SHEET-DISPLAY-LOZENGED-STRING SHEET
		(STRING (OR (CAR (RASSQ CHAR SI::XR-SPECIAL-CHARACTER-NAMES))
			    (FORMAT NIL "~3O" CHAR))))))
      (PREPARE-SHEET (SHEET)
        (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	    (SHEET-HANDLE-EXCEPTIONS SHEET))
	(COND (FONT
	       (COERCE-FONT FONT SHEET)
	       (SETQ BASE-ADJ (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
	      (T
	       (SETQ FONT (SHEET-CURRENT-FONT SHEET)
		     BASE-ADJ (SHEET-BASELINE-ADJ SHEET))))
        (LET* ((CHAR-WIDTHS (FONT-CHAR-WIDTH-TABLE FONT))
	       (FIT (FONT-INDEXING-TABLE FONT))
	       (WIDTH)
	       (KERN 0)
	       (KERN-TABLE)
	       (XPOS (SHEET-CURSOR-X SHEET))
	       (RIGHT-LIM (SHEET-INSIDE-RIGHT SHEET)))
	  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	      (SETQ RIGHT-LIM (- RIGHT-LIM (SHEET-CHAR-WIDTH SHEET))))
	  (SETQ WIDTH (IF CHAR-WIDTHS
			  (AREF CHAR-WIDTHS CHAR)
			  (FONT-CHAR-WIDTH FONT)))
	  (COND ((> (+ XPOS WIDTH) RIGHT-LIM)
		 (SEND SHEET :END-OF-LINE-EXCEPTION)
		 (SHEET-TYO SHEET CHAR FONT))
		(T
		 (AND (SETQ KERN-TABLE (FONT-LEFT-KERN-TABLE FONT))
		      (SETQ KERN (AREF KERN-TABLE CHAR)))
		 (COND ((NULL FIT)
			(%DRAW-CHAR FONT CHAR (- XPOS KERN)
				    (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ)
				    (SHEET-CHAR-ALUF SHEET)
				    SHEET))
		       ;; Wide character, draw several columns
		       (T
			(DRAW-CHAR FONT CHAR (- XPOS KERN)
				   (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ)
				   (SHEET-CHAR-ALUF SHEET)
				   SHEET)))
		 (SETF (SHEET-CURSOR-X SHEET) (+ XPOS WIDTH)))))))
  CHAR)

(DEFMETHOD (SHEET :TYO) (CH &OPTIONAL FONT)
  (SHEET-TYO SELF CH FONT)
  CH)

))

; From file SHWARM.LISP OZ:<L.WINDOW> OZ: (329)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-DISPLAY-LOZENGED-STRING-INTERNAL (SHEET STRING X0 Y0 XLIM ALUF)
  (LET* ((WIDTH (LOZENGED-STRING-WIDTH STRING))
	 TEM (TRUNCATED 8))
    (WHEN (MINUSP (SETQ TEM (- XLIM (+ WIDTH X0))))
      (SETQ TRUNCATED 4)
      (SETQ WIDTH (+ WIDTH TEM)))
    (IF (> WIDTH 4)
	;; Put the string then the box around it
	(LET ((X1 (+ X0 WIDTH -1))
	      (Y1 (+ Y0 8)))
	  (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING (+ X0 4) (+ Y0 2) X1 NIL
				       (SEND (SHEET-GET-SCREEN SHEET)
					     :PARSE-FONT-DESCRIPTOR FONTS:5X5)
				       ALUF)
	  (%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) ALUF T SHEET)
	  (%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) ALUF T SHEET)
	  (WHEN (PLUSP (SETQ TEM (- WIDTH TRUNCATED)))
	    (%DRAW-RECTANGLE TEM 1 (+ X0 4) Y0 ALUF SHEET)
	    (%DRAW-RECTANGLE TEM 1 (+ X0 4) Y1 ALUF SHEET))
	  (WHEN (EQ TRUNCATED 8)
	    (%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) ALUF T SHEET)
	    (%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) ALUF T SHEET))
	  (1+ X1))
      XLIM)))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro parallel-function-binding-list ((varlist eval-now macroflag) &body body)
  `(prog (vars-left bindframe)
	 ;; Trivial case of empty varlist would lose in code below.
	 (unless ,varlist
	   (go long))  
	 (when (nthcdr #o20 ,varlist)
	   (setq bindframe
		 (mapcan #'(lambda (var)
			     (list* (locf (symbol-function (car var)))
				    (if ,macroflag
					`(macro . ,(expand-defmacro
						     var *interpreter-function-environment*))
				      (if ,eval-now
					  (eval1
					    `(function (lambda . ,(cdr var))))
					`(lambda . ,(cdr var))))
				    nil))
			 ,varlist))
	   (go long))
	 ;; The following code is equivalent to the above mapcar
	 ;; except that the list is constructed on the stack
	 ;; by pushing the elements one by one and fiddling with cdr codes.
	 (with-stack-list (tem nil)
	   ;; BINDFRAME gets a pointer to where the list will go.
	   (setq bindframe tem))
	 ;; Now loop over the varlist, computing and pushing initial values.
	 (setq vars-left ,varlist)
      short-nextvar
	 (when vars-left
	   (%push (locf (symbol-function (caar vars-left))))
	   (%push ,(cond (macroflag
			  ``(macro . ,(expand-defmacro
					(car vars-left) *interpreter-function-environment*)))
			 (eval-now
			  `(eval1
			     `(function (lambda . ,(cdar vars-left)))))
			 (t
			  ``(lambda . ,(cdar vars-left)))))
	   (pop vars-left)
	   (go short-nextvar))
	 ;; Modify cdr-code of last word pushed, to terminate the list.
	 (with-stack-list (tem nil)
	   (%p-dpb-offset cdr-nil %%q-cdr-code tem -1))
      long
	 ;; Here BINDFRAME has the correct variables and values.
	 (return
	   (with-stack-list* (*interpreter-function-environment*
			       bindframe *interpreter-function-environment*)
	     . ,body))))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro zl-parallel-function-binding-list ((varlist ignore macroflag) &body body)
  `(prog (vars-left)
	 ;; Now bind all the prog-variables.
	 ;; DO cannot be used, since the scope of the BINDs would be wrong.
	 (setq vars-left ,varlist)
      bindloop
	 (when vars-left
	   ;; For each symbol, push 2 words on stack:
	   ;; value cell location and new value.
	   (%push (locf (symbol-function (caar vars-left))))
	   (%push ,(if macroflag
		      ``(macro . ,(expand-defmacro (car vars-left) nil))
		      ``(lambda . ,(cdar vars-left))))
	   (pop vars-left)
	   (go bindloop))
	 
	 (setq vars-left ,varlist)
      bindloop1
	 (when vars-left
	   ;; Pop off next symbol and value, and bind them.
	   (%bind (%pop) (%pop))
	   ;; Step down VARS-LEFT just so we pop as many pairs as we pushed.
	   (pop vars-left)
	   (go bindloop1))
	 (return (progn . ,body))))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun macrolet (&quote macro-list &rest body)
  "Execute BODY with macro function definitions as per MACRO-LIST.
Each element of MACRO-LIST looks like (NAME (ARGS...) BODY...).
MACROLET rebinds the function definition of each NAME lexically to
 a macro like the one you would get by doing
 (DEFMACRO NAME (ARGS...) BODY...)."
  (if (eq *interpreter-function-environment* t)
      (zl-parallel-function-binding-list (macro-list t t)
	(eval-body body))
    (gobble-declarations-from-body (vars body)
      (parallel-function-binding-list (macro-list t t)
	(eval-body body)))))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1AUX (LAMBDA)
  (LET (STANDARDIZED AUXVARS AUXLIST NONAUXLIST DECLS BODY)
    (SETQ STANDARDIZED (SI::LAMBDA-EXP-ARGS-AND-BODY LAMBDA))
    (OR (SETQ AUXLIST (MEMQ '&AUX (CAR STANDARDIZED)))
	(RETURN-FROM P1AUX LAMBDA))
    (SETQ AUXVARS (CDR AUXLIST))
    (SETQ NONAUXLIST (LDIFF (CAR STANDARDIZED) AUXLIST))
    (DO ((VARLIST NONAUXLIST (CDR VARLIST))
	 SPECIAL-FLAG)
	((NULL VARLIST)
	 (IF SPECIAL-FLAG
	     (PUSH '&SPECIAL AUXVARS)))
      (COND ((EQ (CAR VARLIST) '&SPECIAL)
	     (SETQ SPECIAL-FLAG T))
	    ((EQ (CAR VARLIST) '&LOCAL)
	     (SETQ SPECIAL-FLAG NIL))))
    (SETQ BODY (CDR STANDARDIZED))
    ;; Take all DECLAREs off the body and put them on DECLS.
    (SETF (VALUES BODY DECLS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL *FUNCTION-ENVIRONMENT*))
    `(LAMBDA ,NONAUXLIST
       ,@(IF DECLS `((DECLARE . ,DECLS)))
       (LET* ,AUXVARS
	 ,@(IF DECLS `((DECLARE . ,DECLS)))
	 . ,BODY))))

))

; From file QFCTNS.LISP OZ:<L.SYS> OZ: (772)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFF UNION-eq #'UNION)
(deff intersection-eq #'intersection)
(deff nunion-eq #'nunion)
(deff nintersection-eq #'nintersection)
))


; From file EVAL.LISP OZ:<L.SYS> OZ: (85)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun self-evaluating-p (form)
  "T if FORM always evaluates to itself."
  (cond ((consp form)
	 (eq (car form) 'quote))
	((symbolp form)
	 (or (null form) (eq form t)
	     (keywordp form)))
	(t t)))

))

; From file FLAVOR.LISP OZ:<L.SYS2> OZ: (282)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN FLAVOR-GET (FLAVOR PROP &OPTIONAL DEFAULT)
  (GETF (FLAVOR-PLIST FLAVOR) PROP DEFAULT))

))

; From file CHSNCP.LISP OZ:<L.NETWORK.CHAOS> OZ: (268)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSNCP  "

(DEFUN RESET-ROUTING-TABLE ()
  "Flush out old routing data."
  (SETQ MY-OTHER-SUBNETS NIL)
  (DOTIMES (I (ARRAY-LENGTH ROUTING-TABLE))	;Clear out the routing table
    (SETF (AREF ROUTING-TABLE I) 0)
    (SETF (AREF ROUTING-TABLE-COST I) MAXIMUM-ROUTING-COST)
    (SETF (AREF ROUTING-TABLE-TYPE I) NIL))
  (SETF (AREF ROUTING-TABLE MY-SUBNET) MY-ADDRESS)
  (SETF (AREF ROUTING-TABLE-COST MY-SUBNET) 10.)
  (SELECT-PROCESSOR
    (:CADR
      (SETF (AREF ROUTING-TABLE-TYPE MY-SUBNET) :CHAOS))
    (:LAMBDA
      (SETF (AREF ROUTING-TABLE-TYPE MY-SUBNET) :ETHERNET))))

))

; From file LMMAC.LISP OZ:<L.SYS2> OZ: (379)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFUN FILE-RETRY-RESUME-HANDLER (ERROR-OBJECT TAG &OPTIONAL NEW-PATHNAME)
  ERROR-OBJECT
  (THROW TAG NEW-PATHNAME))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(defun (:property macrolet p1) (exp)
  (let ((*function-environment* *function-environment*)
	;; If we define it as a local macro, that hides any local function definition.
	(*local-functions* (rem-if #'(lambda (elt) (assq (car elt) (cadr exp)))
				   *local-functions*))
	frame)
    (dolist (elt (reverse (cadr exp)))
      (setq frame (list* (locf (symbol-function (car elt)))
			 `(macro . ,(si::expand-defmacro elt *function-environment*))
			 frame)))
    (push frame *function-environment*)
    `(progn . ,(p1progn-1 (cddr exp)))))

))


; From file CHSAUX.LISP.362 OZ:<L.NETWORK.CHAOS> OZ:NIL
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN USER-ACTIVITY-STRING ()
  (LET ((W (DO ((W TV:SELECTED-WINDOW (SEND W :SUPERIOR))
		(W1 NIL W))
	       ((OR (NULL W) (TYPEP W 'TV:SCREEN)) W1))))
    (OR (IGNORE-ERRORS
	  (TYPECASE W
	    (SUPDUP "Supdup")
	    (ZWEI:ZMACS-FRAME "Zmacs")
	    (TV:PEEK-FRAME "Peek")
	    (ZWEI:CONVERSE-FRAME "Converse")
	    (TV:INSPECT-FRAME "Inspect")
	    (TV:LISP-LISTENER "Lisp")
	    (TELNET "Telnet")
	    (FED:FED-FRAME "Font Edit")
	    (ZWEI:ZMAIL-FRAME "ZMail")))
	SI:LOCAL-HOST-NAME)))

))

;;>> this patch should come as close as possible to the end to avoid warnings lossage
; From file DEFMAC.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

(DEFMACRO DEFMACRO (&ENVIRONMENT ENV &REST X)
  "Define FUNCTION-SPEC as a macro.
When a call to the macro is expanded, the argument list LAMBDA-LIST
is matched against the arguments supplied to the macro.
The variables in it are bound.  Then the BODY is executed,
and the value of the last form in it is the expansion of the macro."
  (DECLARE (ARGLIST FUNCTION-SPEC LAMBDA-LIST &BODY BODY))
  (DEFMACRO1 X 'MACRO ENV))

))

; From file DEFMAC.LISP OZ:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DEFMAC  "

(DEFMACRO DEFLAMBDA-MACRO (&ENVIRONMENT ENV &REST X)
  "Define LAMBDA-MACRO-NAME as a lambda macro.
When a list starting with LAMBDA-MACRO-NAME is encountered as a function,
it is expanded by executing the lambda macro definition
to get a new function to use instead.  The argument list LAMBDA-LIST
is matched against the remainder of the list which is the function.
The variables in it are bound.  Then the BODY is executed,
and the value of the last form in it is the expansion of the macro,
the new function."
  (DECLARE (ARGLIST LAMBDA-MACRO-NAME LAMBDA-LIST &BODY BODY))
  (DEFMACRO1 X 'LAMBDA-MACRO ENV))

))
