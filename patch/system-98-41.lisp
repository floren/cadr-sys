;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 3/09/84 02:55:39 by RMS,
;;; Reason: GET-INTERNAL-RUN-TIME returns time since boot, maybe bignum.
;;; M-X Set Key, etc., will not clobber M-X or Abort.
;;; Unix pathname parsing of :string-for-editor namestrings.
;;; Canonical types used better in pathname defaulting.
;;; ZWEI:EXTENDED-COMMAND-P allows COMTAB arg.
;;; FSM search bugs.  DISK-RESTORE/SAVE arg parsing.
;;; Debugger handles *VALUES* right.  Fix up ERR.
;;; :AFTER-FLIP initialization list.
;;; Parsing complex numbers with signed exponents.
;;; AL font file bugs.  C-Shift-D in editor bug.
;;; while running on Lisp Machine Eighteen from band 6
;;; with System 98.36, CADR 3.6, ZMail 53.12, MIT-Specific 22.0, microcode 306, ZM MIT.


(globalize "APPLYHOOK")

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "


(DEFCONST INTERNAL-TIME-UNITS-PER-SECOND 60.)  ;60 60th of a sec in a second

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFVAR HIGH-TIME-BITS 0)
;; T if  (TIME) was TIME-LESSP than LAST-BOOT-TIME when last checked.
;; Each this changes from T to NIL, (TIME) has wrapped around once.

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFVAR WAS-NEGATIVE NIL)

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFVAR LAST-BOOT-TIME 0 "Value of (TIME) when machine was booted.")

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "


(DEFF GET-INTERNAL-REAL-TIME 'GET-INTERNAL-RUN-TIME)

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFUN GET-INTERNAL-RUN-TIME ()
  "Returns time in 60'ths since last boot.  Can be a bignum."
  (LET ((TIME-DIFF (%POINTER-DIFFERENCE (TIME) LAST-BOOT-TIME)))
    (WHEN (AND (PROG1 WAS-NEGATIVE
		      (SETQ WAS-NEGATIVE (LDB-TEST (BYTE 1 22.) TIME-DIFF)))
	       (NOT WAS-NEGATIVE))
      (INCF HIGH-TIME-BITS))
    (DPB HIGH-TIME-BITS (BYTE 23. 23.) (LDB (BYTE 23. 0) TIME-DIFF))))

;;; One-based lengths of months

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(ADD-INITIALIZATION "Initialize Timebase" 
  '(PROGN (SETQ LAST-BOOT-TIME (TIME) WAS-NEGATIVE NIL HIGH-TIME-BITS 0)
	  (INITIALIZE-TIMEBASE))
  '(:WARM :NOW))

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFUN UPDATE-TIMEBASE (&AUX TIME TICK TOP-9-TIME-BITS INCREMENTAL-TOP-10-TIME-BITS
			(OLD-HOUR *LAST-TIME-HOURS*))
  "Update our information on the current time."
  (COND ((NOT (NULL *LAST-TIME-UPDATE-TIME*))
	 (WITHOUT-INTERRUPTS
	   ;; Put the following code back if the TIME function ever makes any attempt
	   ;; to be even close to 60 cycles.  Also change INITIALIZE-TIMEBASE.
	   ;(SETQ TIME (TIME)
	   ;	 TICK (TRUNC (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) 60.)
	   ;	 *LAST-TIME-UPDATE-TIME*
	   ;	    (LDB 0027 (%24-BIT-PLUS (* 60. TICK) *LAST-TIME-UPDATE-TIME*)))
	   (SETF (VALUES TIME TOP-9-TIME-BITS)
		 (FIXNUM-MICROSECOND-TIME))
	   ;; Don't lose when installing this code,
	   ;; if PREVIOUS-TOP-9-TIME-BITS has not been being updated.
	   (OR PREVIOUS-TOP-9-TIME-BITS
	       (SETQ PREVIOUS-TOP-9-TIME-BITS TOP-9-TIME-BITS))
	   ;; See if we have "missed any ticks" in the low 23. bits;
	   ;; Normally we are supposed to be called frequently enough
	   ;; that bit 23. never increments twice between calls to this function
	   ;; but a long WITHOUT-INTERRUPTS can make that happen.
	   (SETQ INCREMENTAL-TOP-10-TIME-BITS
		 (LSH (- TOP-9-TIME-BITS PREVIOUS-TOP-9-TIME-BITS) 1)
		 PREVIOUS-TOP-9-TIME-BITS TOP-9-TIME-BITS)
	   ;; In the ordinary course of events, we DO notice bit 23 increment
	   ;; because we see the low 23 bits wrap around.
	   ;; So don't count those noticed increments in the "extras".
	   (IF (< TIME *LAST-TIME-UPDATE-TIME*)
	       (DECF INCREMENTAL-TOP-10-TIME-BITS 2))
	   ;; INCREMENTAL-TOP-10-TIME-BITS is now set to twice the number of times
	   ;; that bit 23 has incremented since we last ran, that we didn't notice.
	   ;; Now feed that many increments into bit 22, one by one.
	   ;; When finished with them (if there are any),
	   ;; handle the change in the low 23 bits themselves.
	   (DO (EXIT-THIS-TIME) (())
	     (IF ( INCREMENTAL-TOP-10-TIME-BITS 0)
		 (SETQ TICK (FLOOR (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) 1000000.)
		       EXIT-THIS-TIME T)
	       (SETQ TICK (FLOOR (DPB 1 #o2601 0) 1000000.)))
	     (SETQ *LAST-TIME-UPDATE-TIME*
		   (LDB #o0027 (%MAKE-POINTER-OFFSET
			       DTP-FIX
			       (* 1000000. TICK) *LAST-TIME-UPDATE-TIME*)))
	     (OR (ZEROP TICK)
		 (< (SETQ *LAST-TIME-SECONDS* (+ *LAST-TIME-SECONDS* TICK)) 60.)
		 (< (PROG1 (SETQ *LAST-TIME-MINUTES* (+ *LAST-TIME-MINUTES*
							(FLOOR *LAST-TIME-SECONDS* 60.)))
			   (SETQ *LAST-TIME-SECONDS* (\ *LAST-TIME-SECONDS* 60.)))
		    60.)
		 (< (PROG1 (SETQ *LAST-TIME-HOURS* (+ *LAST-TIME-HOURS*
						      (FLOOR *LAST-TIME-MINUTES* 60.)))
			   (SETQ *LAST-TIME-MINUTES* (\ *LAST-TIME-MINUTES* 60.)))
		    24.)
		 ( (PROG1 (SETQ *LAST-TIME-DAY* (1+ *LAST-TIME-DAY*))
			   (SETQ *LAST-TIME-DAY-OF-THE-WEEK*
				 (\ (1+ *LAST-TIME-DAY-OF-THE-WEEK*) 7))
			   (SETQ *LAST-TIME-HOURS* 0))
		    (MONTH-LENGTH *LAST-TIME-MONTH* *LAST-TIME-YEAR*))
		 ( (SETQ *LAST-TIME-DAY* 1
			  *LAST-TIME-MONTH* (1+ *LAST-TIME-MONTH*))
		    12.)
		 (SETQ *LAST-TIME-MONTH* 1
		       *LAST-TIME-YEAR* (1+ *LAST-TIME-YEAR*)))
	     (IF EXIT-THIS-TIME
		 (RETURN NIL)
	       (DECF INCREMENTAL-TOP-10-TIME-BITS)))
	   (WHEN ( OLD-HOUR *LAST-TIME-HOURS*)
	     ;; If hour has incremented, turn decoded time into a UT
	     ;; using the timezone we were using up to now,
	     ;; use that to decide if we have turned DST on or off,
	     ;; and then re-decode the time.
	     (MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			      *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			      *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
	       (DECODE-UNIVERSAL-TIME
		 (ENCODE-UNIVERSAL-TIME
		   *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
		   *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
		   (IF *LAST-TIME-DAYLIGHT-SAVINGS-P*
		       (1- *TIMEZONE*) *TIMEZONE*))))
	     ;; Update things for GET-INTERNAL-RUN-TIME at least once an hour.
	     (GET-INTERNAL-RUN-TIME))
	   T))
	;This used to call INITIALIZE-TIMEBASE.  However, since that gets called by
	;an initialization it seems best not to get processes into it at the same time.
	(T NIL)))

))

; From file COMG.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "

(DEFUN INSTALL-COMMAND-INTERNAL (COMMAND &OPTIONAL REMEMBER-OLD-P DEINSTALL-P
				 &AUX KEY-LIST)
  (PROMPT-LINE (IF DEINSTALL-P "Key to deinstall:" "Key to get it:"))
  (CLEAR-PROMPTS)
  (ALWAYS-DISPLAY-PROMPTS)
  (DO ((COMTAB *COMTAB*)
       (KEY (INPUT-WITH-PROMPTS STANDARD-INPUT ':MOUSE-OR-KBD-TYI)
	    (INPUT-WITH-PROMPTS STANDARD-INPUT ':TYI)))
      (NIL)
    (PUSH KEY KEY-LIST)
    (RECORD-MINI-BUFFER-VALUE NIL)
    (LET ((OLD-COMMAND (COMMAND-LOOKUP KEY COMTAB)))
      (COND ((AND (PREFIX-COMMAND-P OLD-COMMAND)
		  (NOT *NUMERIC-ARG-P*))
	     (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
	    ((Y-OR-N-P "Install command ~S on~{ ~:@C~}? " COMMAND (REVERSE KEY-LIST))
	     (AND DEINSTALL-P
		  (SETQ COMMAND (MOUSE-MACRO-COMMAND-LAST-COMMAND
				  (COMMAND-LOOKUP KEY COMTAB))))
	     (AND REMEMBER-OLD-P
		  (SET-MOUSE-MACRO-COMMAND-LAST-COMMAND COMMAND
							(COMMAND-LOOKUP KEY COMTAB)))
	     (COMMAND-STORE COMMAND KEY COMTAB)
	     (FORMAT QUERY-IO "~&Installed.")
	     (RETURN NIL))
	    (T (FORMAT QUERY-IO "~&Not installed.")
	       (RETURN NIL)))))
  DIS-NONE)

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (UNIX-PATHNAME-MIXIN :PARSE-NAMESTRING) (IGNORE NAMESTRING
						      &OPTIONAL (START 0) END
						      &AUX DIR NAM TYP DELIM-CHAR
						      DIRSTART DIREND)
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (SETQ START (OR (STRING-SEARCH-NOT-CHAR #\SP NAMESTRING START END) END))
  (SETQ END (1+ (OR (STRING-REVERSE-SEARCH-NOT-CHAR #\SP NAMESTRING END START) (1- START))))
  (SETQ DELIM-CHAR (FUNCALL-SELF ':DIRECTORY-DELIMITER-CHARACTER))
  (LET (I)
    (IF (AND (SETQ I (STRING-SEARCH-CHAR #\SP NAMESTRING START END))
	     (CHAR-EQUAL DELIM-CHAR (AREF NAMESTRING (1- END)))
	     (NOT (STRING-SEARCH-CHAR DELIM-CHAR NAMESTRING START I)))
	(SETQ DIRSTART (STRING-SEARCH-NOT-CHAR #\SP NAMESTRING I END)
	      DIREND END
	      END I)
      (SETQ DIRSTART START
	    DIREND (STRING-REVERSE-SEARCH-CHAR DELIM-CHAR NAMESTRING END START)
	    START (IF DIREND (1+ DIREND) START))))
  ;; Now START..END are the indices around the name and type,
  ;; and DIRSTART..DIREND are the indices around the directory.
  (COND (DIREND
	 (SETQ DIR (LET ((RELATIVE-P T)
			 (DIRIDX DIRSTART)
			 (UP (FUNCALL-SELF ':DIRECTORY-UP-DELIMITER))
			 (NUP NIL)
			 (STRS NIL))
		     (COND ((= (AREF NAMESTRING DIRIDX) DELIM-CHAR)
			    (SETQ RELATIVE-P NIL)
			    (SETQ DIRIDX (STRING-SEARCH-NOT-CHAR
					      DELIM-CHAR NAMESTRING DIRIDX))))
		     (AND DIRIDX (> DIREND DIRIDX)
			  (SETQ STRS (LOOP FOR IDX = DIRIDX THEN JDX
					   AS JDX = (STRING-SEARCH-CHAR
						      DELIM-CHAR NAMESTRING IDX DIREND)
					   COLLECT (SUBSTRING NAMESTRING IDX (OR JDX DIREND))
					   WHILE
					   (AND JDX
						(SETQ JDX
						      (STRING-SEARCH-NOT-CHAR
							DELIM-CHAR NAMESTRING JDX DIREND))))))
		     (AND (STRINGP UP)
			  (DO L STRS (CDR L) (NULL L)
			      (AND (STRING-EQUAL (CAR L) UP)
				   (RPLACA L ':UP))))
		     (AND NUP (SETQ STRS (NCONC NUP STRS)))
		     (COND (RELATIVE-P (CONS ':RELATIVE STRS))
			   ((NULL STRS) ':ROOT)
			   ((NULL (CDR STRS)) (CAR STRS))
			   (T STRS))))))
  (SETQ TYP (STRING-REVERSE-SEARCH-CHAR #/. NAMESTRING END START))
  (IF (EQ TYP START) (SETQ TYP NIL))		;Initial . is part of NAM
  (IF TYP (PSETQ END TYP
		 TYP (SUBSTRING NAMESTRING (1+ TYP) END)))
  (SETQ NAM (AND ( START END) (SUBSTRING NAMESTRING START END)))
  (COND ((EQUAL NAM "'") (SETQ NAM NIL))
	((EQUAL NAM "*") (SETQ NAM ':WILD)))
  (COND ((NULL TYP) (SETQ TYP (AND NAM ':UNSPECIFIC)))
	((EQUAL TYP "'") (SETQ TYP NIL))
	((EQUAL TYP "*") (SETQ TYP ':WILD)))
  (VALUES ':UNSPECIFIC DIR NAM TYP ':UNSPECIFIC))

))

; From file DOC.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN EXTENDED-COMMAND-P (SYMBOL &OPTIONAL (COMTAB *COMTAB*))
  "T if SYMBOL (a DEFCOM name) is reachable thru M-X using COMTAB."
  (DO-NAMED EXTENDED-COMMAND-P
       C COMTAB (COMTAB-INDIRECT-TO C) (NULL C)
    (DOLIST (X (EXTENDED-COMMAND-ALIST C))
      (AND (EQ (CDR X) SYMBOL) (RETURN-FROM EXTENDED-COMMAND-P T)))))

))

; From file SEARCH.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SEARCH  "

(DEFVAR *FSM-CHARACTER-SET-TABLE*	;character  set_number's
	(MAKE-ARRAY '(400 16.) ':TYPE 'ART-1B))

))

; From file SEARCH.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SEARCH  "

(DEFUN BUILD-FSM-CHARACTER-SET (STRING-LIST CASE-DEPENDENT-P &AUX (MAXCHAR 0))
  ;; First pass, get all the alphabetic characters
  (ARRAY-INITIALIZE *FSM-CHARACTER-TABLE* 0)
  (DOLIST (STRING STRING-LIST)
    (DO ((I 0 (1+ I))
	 (LEN (STRING-LENGTH STRING))
	 (CH))
	(( I LEN))
      (OR (LDB-TEST %%FSM-SET (SETQ CH (AREF STRING I)))
	  (COND ((ZEROP (AREF *FSM-CHARACTER-TABLE* (SETQ CH (LDB %%FSM-CHAR CH))))
		 (ASET (SETQ MAXCHAR (1+ MAXCHAR)) *FSM-CHARACTER-TABLE* CH)
		 (AND (NOT CASE-DEPENDENT-P)
		      (OR (AND ( CH #/A) ( CH #/Z))
			  (AND ( CH #/a) ( CH #/z)))
		      (ASET MAXCHAR *FSM-CHARACTER-TABLE* (LOGXOR CH 40))))))))
  ;; Second pass, get the character types for all the sets mentioned
  (ARRAY-INITIALIZE *FSM-CHARACTER-SET-TABLE-16* 0)
  (LET ((LEN (LENGTH *FSM-SEARCH-SET-LIST*)))
    (IF (OR (NOT (BOUNDP '*FSM-SEARCH-SET-TABLE*))
	    (< (ARRAY-LENGTH *FSM-SEARCH-SET-TABLE*) LEN))
	(SETQ *FSM-SEARCH-SET-TABLE* (MAKE-ARRAY LEN))
	(ARRAY-INITIALIZE *FSM-SEARCH-SET-TABLE* NIL)))
  (DOLIST (STRING STRING-LIST)
    (DO ((I 0 (1+ I))
	 (LEN (STRING-LENGTH STRING))
	 (CH))
	(( I LEN))
      (AND (LDB-TEST %%FSM-SET (SETQ CH (AREF STRING I)))
	   (NOT (AREF *FSM-SEARCH-SET-TABLE* (SETQ CH (LDB %%FSM-CHAR CH))))
	   (LET ((*LIST* NIL))
	     (MAP-OVER-FSM-SEARCH-SET CH
	       #'(LAMBDA (SET CH &AUX CHARN)
		   (IF (ZEROP (SETQ CHARN (AREF *FSM-CHARACTER-TABLE* CH)))
		       (ASET 1 *FSM-CHARACTER-SET-TABLE* CH SET)
		     (OR (MEMQ CHARN *LIST*) (PUSH CHARN *LIST*)))))
	     (ASET (NREVERSE *LIST*) *FSM-SEARCH-SET-TABLE* CH)))))
  ;; Now assign character types for all the set intersections
  (DO ((CH 0 (1+ CH))
       (SET-ALIST NIL)
       (MASK) (ENTRY))
      (( CH 400))
    (COND ((NOT (ZEROP (SETQ MASK (AREF *FSM-CHARACTER-SET-TABLE-16* CH))))
	   (COND ((NOT (SETQ ENTRY (ASSQ MASK SET-ALIST)))
		  (PUSH (SETQ ENTRY (CONS MASK (SETQ MAXCHAR (1+ MAXCHAR)))) SET-ALIST)
		  (DO ((SET 0 (1+ SET))
		       (BIT 0001 (+ BIT 0100)))
		      (( SET 16.))
		    (AND (LDB-TEST BIT MASK)
			 (PUSH MAXCHAR (AREF *FSM-SEARCH-SET-TABLE* SET))))))
	   (ASET (CDR ENTRY) *FSM-CHARACTER-TABLE* CH))))
  ;; Finally return the number of character types
  (1+ MAXCHAR))

;;; Apply FUNCTION to all members of a character set,
;;; FUNCTION is caled with SET-NUMBER and character

))

; From file SEARCH.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SEARCH  "

(DEFVAR *FSM-CHARACTER-SET-TABLE-16*	;indirect array to above
	(MAKE-ARRAY 400
		    ':TYPE 'ART-16B
		    ':DISPLACED-TO
		    *FSM-CHARACTER-SET-TABLE*))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DISK-RESTORE-DECODE (PARTITION &AUX LOW-16-BITS HI-16-BITS)
    (COND ((NULL PARTITION)
	   (SETQ LOW-16-BITS 0 HI-16-BITS 0))
	  ((NUMBERP PARTITION)
	   (SETQ LOW-16-BITS (+ #/L (LSH #/O 8)))
	   (SETQ HI-16-BITS (+ #/D (LSH (+ #/0 PARTITION) 8))))
	  ((OR (SYMBOLP PARTITION) (STRINGP PARTITION))
	   (IF (= (STRING-LENGTH PARTITION) 1)
	       (SETQ PARTITION (STRING-APPEND "LOD" PARTITION))
	     (SETQ PARTITION (STRING PARTITION)))
	   (SETQ LOW-16-BITS (+ (CHAR-UPCASE (AR-1 PARTITION 0))
				(LSH (CHAR-UPCASE (AR-1 PARTITION 1)) 8)))
	   (SETQ HI-16-BITS (+ (CHAR-UPCASE (AR-1 PARTITION 2))
			       (LSH (CHAR-UPCASE (AR-1 PARTITION 3)) 8))))
	  (T (FERROR NIL "~S is not a valid partition name." PARTITION)))
    (LIST HI-16-BITS LOW-16-BITS))

))

; From file DLEDIT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DLEDIT  "

(DEFUN SET-CURRENT-BAND (BAND &OPTIONAL (UNIT 0) MICRO-P &AUX RQB LABEL-INDEX DONT-DISPOSE)
  "Specify the LOD band to be used for loading the Lisp system at boot time.
If the LOD band you specify goes with a different microcode,
you will be given the option of selecting that microcode as well.  Usually, do so.

Do PRINT-DISK-LABEL to see what bands are available and what they contain.
UNIT can be a string containing a machine's name, or /"CC/";
then the specified or debugged machine's current band is set.
The last works even if the debugged machine is down.
UNIT can also be a disk drive number; however, it is the disk on
drive zero which is used for booting.

Returns T if the band was set as specified, NIL if not
 (probably because user said no to a query)."
  (SETF (VALUES UNIT DONT-DISPOSE)
	(DECODE-UNIT-ARGUMENT UNIT
	         (FORMAT NIL "(SET-CURRENT-~:[BAND~;MICROLOAD~] ~D)" MICRO-P BAND)))
  (UNWIND-PROTECT
   (PROG ((UCODE-NAME (SELECT SYS:PROCESSOR-TYPE-CODE
			(SI:CADR-TYPE-CODE "MCR")
			(SI:LAMBDA-TYPE-CODE "LMC"))))
    (SETQ RQB (GET-DISK-LABEL-RQB))
    (SETQ BAND (COND ((OR (SYMBOLP BAND) (STRINGP BAND))
		      (STRING-UPCASE (STRING BAND)))
		     (T (FORMAT NIL "~A~D"
				(COND (MICRO-P UCODE-NAME)
				      (T "LOD"))
				BAND))))
    (MULTIPLE-VALUE (NIL NIL LABEL-INDEX)
      (FIND-DISK-PARTITION-FOR-READ BAND RQB UNIT))	;Does a READ-DISK-LABEL
    (OR (STRING-EQUAL (SUBSTRING BAND 0 3)
		      (IF MICRO-P UCODE-NAME "LOD"))
	(FQUERY NIL "The specified band is not a ~A band.  Select it anyway? "
		(IF MICRO-P UCODE-NAME "LOD"))
	(RETURN NIL))
    (PUT-DISK-STRING RQB BAND (COND (MICRO-P 6) (T 7)) 4)
    (IF (NOT MICRO-P)
	(MULTIPLE-VALUE-BIND (NIL MEMORY-SIZE-OF-BAND UCODE-VERSION-OF-BAND)
	    (MEASURED-SIZE-OF-PARTITION BAND UNIT)
	  (LET ((CURRENT-UCODE-VERSION
		  (GET-UCODE-VERSION-FROM-COMMENT (GET-DISK-STRING RQB 6 4) UNIT RQB T))
		(MACHINE-MEMORY-SIZE
		  (MEASURED-SIZE-OF-PARTITION "PAGE" UNIT)))
	    (AND (> MEMORY-SIZE-OF-BAND MACHINE-MEMORY-SIZE)
		 (NOT (FQUERY NIL "~A requires a ~D block PAGE partition, but there is only ~D.  Select ~A anyway? "
			      BAND MEMORY-SIZE-OF-BAND MACHINE-MEMORY-SIZE BAND))
		 (RETURN))
	    (MULTIPLE-VALUE-BIND (BASE-BAND-NAME BASE-BAND-VALID)
		(INC-BAND-BASE-BAND BAND UNIT)
	      (WHEN BASE-BAND-NAME
		(FORMAT T "~%Band ~A is an incremental save with base band ~A."
			BAND BASE-BAND-NAME)
		(UNLESS BASE-BAND-VALID
		  (FORMAT T "~2%It appears that ~A's contents have been changed
 since ~A was dumped.  Therefore, booting ~A may fail to work!"
			  BASE-BAND-NAME BAND BAND)
		  (UNLESS (FQUERY FORMAT:YES-OR-NO-P-OPTIONS "~%Select ~A anyway? "
				  BAND)
		    (RETURN NIL)))))
	    (IF UCODE-VERSION-OF-BAND
		(IF (EQ CURRENT-UCODE-VERSION UCODE-VERSION-OF-BAND)
		    (FORMAT T "~%The new current band ~A should work properly
with the ucode version that is already current." BAND)
		  (LET ((BAND-UCODE-PARTITION
			  (FIND-MICROCODE-PARTITION RQB UCODE-VERSION-OF-BAND)))
		    (IF BAND-UCODE-PARTITION
			(IF (FQUERY NIL "~A goes with ucode ~D, which is not selected.
Partition ~A claims to contain ucode ~D.  Select it? "
				    BAND UCODE-VERSION-OF-BAND
				    BAND-UCODE-PARTITION UCODE-VERSION-OF-BAND)
			    (PUT-DISK-STRING RQB BAND-UCODE-PARTITION 6 4)
			  (UNLESS (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
					  "~2%The machine may fail to boot if ~A is selected
 with the wrong microcode version.  It wants ucode ~D.
Currently ucode version ~D is selected.
Do you know that ~A will run with this ucode? "
					  BAND UCODE-VERSION-OF-BAND
					  CURRENT-UCODE-VERSION BAND)
			    (RETURN NIL)))
		      ;; Band's desired microcode doesn't seem present.
		      (FORMAT T "~%~A claims to go with ucode ~D,
which does not appear to be present on this machine.
It may or may not run with other ucode versions.
Currently ucode ~D is selected."
			      BAND UCODE-VERSION-OF-BAND CURRENT-UCODE-VERSION)
		      (UNLESS (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
				      "~%Should I really select ~A? " BAND)
			(RETURN NIL))))))))
      ;; Here to validate a MCR partition.
      (WHEN (> LABEL-INDEX (- 400 3))
	(FORMAT T "~%Band ~A may not be selected since it is past the first page of the label.
The bootstrap prom only looks at the first page.  Sorry.")
	(RETURN NIL)))
    (WRITE-DISK-LABEL RQB UNIT)
    (RETURN T))
   (RETURN-DISK-RQB RQB)
   (UNLESS DONT-DISPOSE (DISPOSE-OF-UNIT UNIT))))

))

; From file PATHNM.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFUN DEFAULT-PATHNAME (&OPTIONAL DEFAULTS HOST DEFAULT-TYPE DEFAULT-VERSION INTERNAL-P
			 &AUX ELEM PATHNAME HOST-TO-USE CTYPE OTYPE)
  (AND HOST (SETQ HOST (GET-PATHNAME-HOST HOST)))
  (OR DEFAULTS (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((AND DEFAULTS (NLISTP DEFAULTS))
	 (SETQ PATHNAME (PARSE-PATHNAME DEFAULTS)))
	(T
	 (SETQ ELEM (COND ((NOT *DEFAULTS-ARE-PER-HOST*) (ASSQ NIL DEFAULTS))
			  (HOST (ASSQ HOST DEFAULTS))
			  (T (DOLIST (DEFAULT DEFAULTS)	;Last host mentioned
			       (AND (CDR DEFAULT) (RETURN DEFAULT))))))
	 ;; If none better found, take the one for the login machine
	 (OR (CDR ELEM)
	     (SETQ ELEM (OR (ASSQ USER-LOGIN-MACHINE DEFAULTS)
			    (NCONS USER-LOGIN-MACHINE))))
	 ;; If there isn't one already, build a pathname from the host of this one
	 (SETQ HOST-TO-USE (OR HOST (CAR ELEM) (PATHNAME-HOST (CDR ELEM))))
	 (COND ((SETQ PATHNAME (CDR ELEM)))
	       (INTERNAL-P
		(SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST-TO-USE NIL NIL NIL NIL NIL)))
	       (T
		(SETQ PATHNAME (FUNCALL (USER-HOMEDIR HOST-TO-USE) ':NEW-PATHNAME
					':NAME "FOO" ':TYPE *NAME-SPECIFIED-DEFAULT-TYPE*
					':VERSION ':NEWEST))
		(SETF (CDR ELEM) PATHNAME)))))
  ;; If default-type or default-version was given, or the host has changed,
  ;; merge those in.
  (AND (OR (AND HOST (NEQ HOST (PATHNAME-HOST PATHNAME))) DEFAULT-TYPE DEFAULT-VERSION)
       (SETQ HOST (OR HOST (PATHNAME-HOST PATHNAME)))
       (IF INTERNAL-P
	   (AND HOST (SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST NIL NIL NIL NIL NIL)))
	 (SETF (VALUES CTYPE OTYPE) (SEND PATHNAME ':CANONICAL-TYPE))
	 (SETQ PATHNAME (FUNCALL (MAKE-PATHNAME ':HOST HOST ':DEFAULTS NIL)
				 ':NEW-PATHNAME
				 ':DIRECTORY (PATHNAME-DIRECTORY PATHNAME)
				 ':DEVICE (PATHNAME-DEVICE PATHNAME)
				 ':HOST (OR HOST (PATHNAME-HOST PATHNAME))
				 ':NAME (PATHNAME-NAME PATHNAME)
				 ':CANONICAL-TYPE CTYPE
				 ':ORIGINAL-TYPE OTYPE
				 ':VERSION (OR DEFAULT-VERSION (PATHNAME-VERSION PATHNAME))
				 ))))
  PATHNAME)

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFUN SG-EVAL (SG FORM &OPTIONAL REBIND-STREAMS
		SAVED-REGPDL-SEGMENT SAVED-SPECPDL-SEGMENT
		&AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
  "Evaluate FORM in stack group SG and return a list of its values.
This is a high-level function, in that SG's state is preserved.
REBIND-STREAMS = T means execute FORM with streams such as
*STANDARD-INPUT* and *STANDARD-OUTPUT* set to the current value
of *TERMINAL-IO* in the stack group that calls SG-EVAL
instead of whatever they are in stack group SG.
SAVED-REGPDL-SEGMENT and SAVED-SPECPDL-SEGMENT are only for use by SG-EVAL-IN-FRAME."
  (SG-SAVE-STATE SG)
  (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
  (SG-OPEN-CALL-BLOCK SG 0 (IF REBIND-STREAMS 'FH-STREAM-BINDING-EVALER 'FH-EVALER))
  ;; NOTE: SG-FRAME-SINGLE-VALUE knows the position of EH-ERROR among the following
  (SG-REGPDL-PUSH FORM SG)
  (SG-REGPDL-PUSH + SG)
  (SG-REGPDL-PUSH * SG)
  (SG-REGPDL-PUSH // SG)
  (SG-REGPDL-PUSH ++ SG)
  (SG-REGPDL-PUSH ** SG)
  (SG-REGPDL-PUSH //// SG)
  (SG-REGPDL-PUSH +++ SG)
  (SG-REGPDL-PUSH *** SG) 
  (SG-REGPDL-PUSH ////// SG)
  (SG-REGPDL-PUSH *VALUES* SG)
  (SG-REGPDL-PUSH SG SG)
  (SG-REGPDL-PUSH CURRENT-FRAME SG)
  (SG-REGPDL-PUSH EH-ERROR SG)
  (SG-REGPDL-PUSH CURRENT-STACK-GROUP SG)
  (SG-REGPDL-PUSH ERROR-HANDLER-RUNNING SG)
  (SG-REGPDL-PUSH PREV-FH SG)
  (SG-REGPDL-PUSH SAVED-REGPDL-SEGMENT SG)
  (SG-REGPDL-PUSH SAVED-SPECPDL-SEGMENT SG)
  (SG-REGPDL-PUSH ERROR-DEPTH SG)
  (AND REBIND-STREAMS (SG-REGPDL-PUSH *TERMINAL-IO* SG))
  (RUN-SG SG))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFUN SG-APPLY (SG FUNCTION ARGUMENTS &OPTIONAL NO-ERROR-RESTART GOODBYE
		 &AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
  "Apply FUNCTION to ARGUMENTS in stack group SG.
This is a high-level function, in that SG's state is preserved."
  (SG-SAVE-STATE SG)
  (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
  (SG-OPEN-CALL-BLOCK SG 0
		      (IF NO-ERROR-RESTART 'FH-APPLIER-NO-RESTART 'FH-APPLIER))
  (SG-REGPDL-PUSH FUNCTION SG)
  (SG-REGPDL-PUSH ARGUMENTS SG)
  (SG-REGPDL-PUSH + SG)
  (SG-REGPDL-PUSH * SG)
  (SG-REGPDL-PUSH // SG)
  (SG-REGPDL-PUSH ++ SG)
  (SG-REGPDL-PUSH ** SG)
  (SG-REGPDL-PUSH //// SG)
  (SG-REGPDL-PUSH +++ SG)
  (SG-REGPDL-PUSH *** SG) 
  (SG-REGPDL-PUSH ////// SG)
  (SG-REGPDL-PUSH *VALUES* SG)
  (SG-REGPDL-PUSH SG SG)
  (SG-REGPDL-PUSH CURRENT-FRAME SG)
  (SG-REGPDL-PUSH EH-ERROR SG)
  (SG-REGPDL-PUSH CURRENT-STACK-GROUP SG)
  (SG-REGPDL-PUSH ERROR-HANDLER-RUNNING SG)
  (SG-REGPDL-PUSH PREV-FH SG)
  (IF GOODBYE
      (SG-RUN-GOODBYE SG)
    (RUN-SG SG)))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFUN FH-APPLIER (FN ARGS X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
		   X-SG X-FRAME X-ERROR SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH)))
      (*CATCH TAG
	;; Note: no special variables should be bound
	;; outside this point (when we return to the error handler sg).
	(STACK-GROUP-RESUME SG
	  (LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
		(// X//) (//// X////) (////// X//////)
		(*VALUES* XVALUES)
		(EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
		CONDITION-HANDLERS
		CONDITION-DEFAULT-HANDLERS
		;; It's best to heap-cons this to avoid frightening the user.
		(CONDITION-RESUME-HANDLERS
		  (LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			   SI:CATCH-ERROR-RESTART-THROW ,TAG)
			 T CONDITION-RESUME-HANDLERS))
		(*EVALHOOK* NIL)
		(SI:APPLYHOOK NIL)
		(ERRSET-STATUS NIL))
	    (MULTIPLE-VALUE-LIST (APPLY FN ARGS)))))
      ;; This is in case the catch catches.
      (STACK-GROUP-RESUME SG 'LOSE))
    ;; This is reached only if we throw through this frame.
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFUN FH-EVALER (FORM X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
		  X-SG X-FRAME X-ERROR SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH)))
      (*CATCH TAG
	(STACK-GROUP-RESUME
	  SG
	  ;; Note: no special variables should be bound
	  ;; outside this point (when we return to the error handler sg).
	  (LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
		(// X//) (//// X////) (////// X//////) (- FORM)
		(*VALUES* XVALUES)
		(EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
		CONDITION-HANDLERS
		CONDITION-DEFAULT-HANDLERS
		;; It's best to heap-cons this to avoid frightening the user.
		(CONDITION-RESUME-HANDLERS
		  (LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			   SI:CATCH-ERROR-RESTART-THROW ,TAG)
			 T CONDITION-RESUME-HANDLERS))
		(*EVALHOOK* NIL) (SI:APPLYHOOK NIL) (ERRSET-STATUS NIL))
	    (WITH-SELF-VARIABLES-BOUND
	      (MULTIPLE-VALUE-LIST (EVAL FORM))))))
      ;; This is in case the catch catches.
      (STACK-GROUP-RESUME SG 'LOSE))
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN FH-APPLIER-NO-RESTART (FN ARGS IGNORE IGNORE IGNORE IGNORE IGNORE IGNORE IGNORE
			      IGNORE IGNORE IGNORE IGNORE IGNORE IGNORE
			      SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (STACK-GROUP-RESUME SG (MULTIPLE-VALUE-LIST (APPLY FN ARGS)))
    ;; This is reached only if we throw through this frame.
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SI:ERRSET-HANDLER (CONDITION TAG PRINTFLAG)
  (UNLESS (OR ERRSET (SEND CONDITION ':DANGEROUS-CONDITION-P))
    (WHEN PRINTFLAG
      (TERPRI ERROR-OUTPUT)
      (SEND CONDITION ':PRINT-ERROR-MESSAGE CURRENT-STACK-GROUP T ERROR-OUTPUT))
    (*THROW TAG NIL)))

(DEFUN FH-STREAM-BINDING-EVALER (FORM X+ X* X// X++ X** X//// X+++ X*** X////// XVALUES
				 X-SG X-FRAME X-ERROR
				 SG EH-P PREV-FH
				 REGPDL-SEGMENT SPECPDL-SEGMENT ERROR-DEPTH
				 EH-TERMINAL-IO)
  (DECLARE (SPECIAL OLD-TERMINAL-IO OLD-STANDARD-OUTPUT OLD-STANDARD-INPUT))
  (UNWIND-PROTECT
    (LET* ((TAG `("Return to debugger level ~D." ,ERROR-DEPTH))
;	   (OLD-TERMINAL-IO *TERMINAL-IO*) 
;	   (OLD-STANDARD-OUTPUT *STANDARD-OUTPUT*) (OLD-STANDARD-INPUT *STANDARD-INPUT*)
	   WIN-P RESULT)
      (*CATCH TAG
	;; Note: no special variables should be bound
	;; outside this point (when we return to the error handler sg).
	(LET ((+ X+) (++ X++) (+++ X+++) (* X*) (** X**) (*** X***)
	      (// X//) (//// X////) (////// X//////) (- FORM)
	      (*VALUES* XVALUES)
	      (EH-SG X-SG) (EH-FRAME X-FRAME) (EH-ERROR X-ERROR)
	      (*TERMINAL-IO* EH-TERMINAL-IO)
	      (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
	      (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
	      (*QUERY-IO* SI:SYN-TERMINAL-IO)
	      (*ERROR-OUTPUT* SI:SYN-TERMINAL-IO)
	      (*EVALHOOK* NIL) (SI:APPLYHOOK NIL) (ERRSET-STATUS NIL)
	      (RUBOUT-HANDLER NIL)
	      CONDITION-HANDLERS
	      CONDITION-DEFAULT-HANDLERS
	      ;; It's best to heap-cons this to avoid frightening the user.
	      (CONDITION-RESUME-HANDLERS
		(LIST* `((SYS:ABORT ERROR) ,TAG T ,TAG
			 SI:CATCH-ERROR-RESTART-THROW ,TAG)
		       T CONDITION-RESUME-HANDLERS)))
	  (WITH-SELF-VARIABLES-BOUND
	    (SETQ RESULT (MULTIPLE-VALUE-LIST (SI:EVAL-ABORT-TRIVIAL-ERRORS FORM))
		  WIN-P T))))
      (COND (WIN-P
;	     (SETQ *TERMINAL-IO* OLD-TERMINAL-IO
;		   *STANDARD-OUTPUT* OLD-STANDARD-OUTPUT
;		   *STANDARD-INPUT* OLD-STANDARD-INPUT)
	     (STACK-GROUP-RESUME SG RESULT))
	    (T (STACK-GROUP-RESUME SG 'LOSE))))
    (IF REGPDL-SEGMENT (FREE-SAVED-PDL-SEGMENT REGPDL-SEGMENT))
    (IF SPECPDL-SEGMENT (FREE-SAVED-PDL-SEGMENT SPECPDL-SEGMENT))
    (SETF (SG-FOOTHOLD-DATA CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

))

; From file GC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN GC-FLIP-NOW ()
  (WITH-LOCK (GC-FLIP-LOCK)
    (IF (NOT %GC-FLIP-READY) (GC-RECLAIM-OLDSPACE))	;In case not reclaimed already
    (SETQ %PAGE-CONS-ALARM 0 %REGION-CONS-ALARM 0)	;avoid overflow in these fixnums
    (DOLIST (ELEM GC-DAEMON-QUEUE)
      (GC-DAEMON-QUEUE (FIRST ELEM) (SECOND ELEM) 1 1 ELEM))
    (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE)
	(GC-GET-SPACE-SIZES)
      (GC-REPORT			;separate static from exited when exited exists?
	"GC: About to flip.  Dynamic space=~D., Static space=~D., Free space=~D."
	DYNAMIC-SIZE (+ STATIC-SIZE EXITED-SIZE) FREE-SIZE)
      (WITHOUT-INTERRUPTS
	(PROCESS-WAIT "Flip inhibited" #'(LAMBDA () (NOT INHIBIT-GC-FLIPS)))
	;; Perform whatever actions other programs need to do on flips
	(MAPC #'EVAL GC-EVERY-FLIP-LIST)
	(MAPC #'EVAL (PROG1 GC-NEXT-FLIP-LIST
			    (SETQ GC-NEXT-FLIP-LIST GC-SECOND-NEXT-FLIP-LIST
				  GC-SECOND-NEXT-FLIP-LIST NIL)))
	;; Reset the GC scan pointers of all regions, actually only in static and fixed areas
	;; is it necessary.
	(DO REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION) (MINUSP REGION)
	    (STORE (REGION-GC-POINTER REGION) 0))
	;; Invalidate AR-1's cache.
	(SETQ AR-1-ARRAY-POINTER-1 NIL)
	(SETQ AR-1-ARRAY-POINTER-2 NIL)
	;; Don't forget to actually flip! (Change newspace to oldspace in all dynamic areas)
	(%GC-FLIP T)
	;; Deallocate space at the end of the oldspace regions, if we can.
	(DOTIMES (REGION SIZE-OF-AREA-ARRAYS)
	  (IF (= %REGION-SPACE-OLD
		 (%LOGLDB %%REGION-SPACE-TYPE (REGION-BITS REGION)))
	      (DEALLOCATE-END-OF-REGION REGION)))
	(SETQ %GC-GENERATION-NUMBER (1+ %GC-GENERATION-NUMBER))
	(MAPC #'EVAL GC-AFTER-FLIP-LIST)
	(INITIALIZATIONS 'AFTER-FLIP-INITIALIZATION-LIST T))
      (SETQ GC-OLDSPACE-EXISTS T)
      T)))

))

; From file GC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFVAR AFTER-FLIP-INITIZIALIZATION-LIST NIL "Initializations performed after every flip.")

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(IF-IN-LISPM
(DEFMACRO ERRSET (BODY &OPTIONAL (PRINTFLAG T))
  "Execute body, trapping errors.  If no error, return a 1-list of the value of BODY.
If there is an error, return NIL (or at least not a list.)
An error message is printed unless PRINTFLAG is specified and evaluates to NIL."
  (LET ((TAG (GENSYM)))
    `(CATCH-CONTINUATION ',TAG #'(LAMBDA (TEM) (VALUES TEM T)) NIL
       (CONDITION-BIND ((ERROR 'ERRSET-HANDLER ',TAG ,PRINTFLAG))
	  (LIST ,BODY)))))
)

(IF-IN-LISPM 
(DEFMACRO ERR (&OPTIONAL VALUE-FORM FLAG)
    (COND ((OR VALUE-FORM FLAG)
	   `(LET ((.VALUE. ,VALUE-FORM))
	      (DOLIST (H EH:CONDITION-HANDLERS)
		(WHEN (AND (EQ (CAR H) 'ERROR)
			   (EQ (CADR H) 'ERRSET-HANDLER))
		  (*THROW (CADDR H) .VALUE.)))
	      (FERROR "~S" .VALUE.)))
	  (T '(PROGN (DOLIST (H EH:CONDITION-HANDLERS)
		       (WHEN (AND (EQ (CAR H) 'ERROR)
				  (EQ (CADR H) 'ERRSET-HANDLER))
			 (*THROW (CADDR H) NIL)))
		     (ERROR "")))))
)

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun (complex standard-read-function) (stream string &aux complex-start (zero 0))
  stream
  (do ((i 1 (1+ i)))
      ((= i (length string)))
    (when (and (mem '= (aref string i) '(#/+ #/-))
	       (not (alpha-char-p (aref string (1- i)))))
      (return (setq complex-start i))))
  (values
    (normalized-complex
      (cond (complex-start (with-input-from-string (strm string zero complex-start)
			     (xr-read-thing strm)))
	    (t (setq complex-start 0)))
      (with-input-from-string (strm string complex-start (1- (string-length string)))
	(xr-read-thing strm)))
    'complex))

))

; From file FNTCNV.LISP PS:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "

(DEFUN READ-AL-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME
				     &AUX FD ARRAY LINE-HEIGHT PROPORTIONAL MAX-WIDTH)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AL"))
  (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME))
  (SETF (FD-NAME FD) FONTNAME)
  (WITH-OPEN-FILE (STREAM FILENAME '(:IN :FIXNUM))
    (SETQ LINE-HEIGHT (FUNCALL STREAM ':TYI))
    (SETF (FD-LINE-SPACING FD) LINE-HEIGHT)
    (SETF (FD-BLINKER-HEIGHT FD) LINE-HEIGHT)
    (LET ((BASELINE-AND-MAX-WIDTH (FUNCALL STREAM ':TYI)))
      (SETQ PROPORTIONAL (LDB-TEST 1701 BASELINE-AND-MAX-WIDTH))
      (SETF (FD-BASELINE FD) (LDB 1007 BASELINE-AND-MAX-WIDTH))
      (SETF (FD-SPACE-WIDTH FD) (SETQ MAX-WIDTH (LDB 0010 BASELINE-AND-MAX-WIDTH))))
    (SETQ ARRAY (MAKE-ARRAY 1000. ':TYPE 'ART-16B ':LEADER-LIST '(0)))
    (DO CH (FUNCALL STREAM ':TYI) (FUNCALL STREAM ':TYI) (NULL CH)
      (ARRAY-PUSH-EXTEND ARRAY CH)))
  (DO ((CH 0 (1+ CH))
       (CD)
       (CHAR-WIDTH))
      (( CH 200))	;Alto font could have 400 characters, our fonts don't yet
    (SETQ CHAR-WIDTH 0)
    (DO ((IDX CH)
	 (XW))
	(NIL)
      (SETQ IDX (+ IDX (AREF ARRAY IDX)))
      (SETQ XW (AREF ARRAY IDX))
      (IF (ZEROP (PROG1 (LOGAND XW 1) (SETQ XW (FLOOR XW 2))))
	  (SETQ CHAR-WIDTH (+ CHAR-WIDTH 16.)
		IDX XW)
	  (RETURN (SETQ CHAR-WIDTH (+ CHAR-WIDTH XW)))))
    (SETQ CD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE ART-1B
					       :LENGTH (LIST LINE-HEIGHT CHAR-WIDTH))))
    (SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
    (AND (= CH #\SP) (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
    (SETF (CD-CHAR-LEFT-KERN CD) 0)
    (FD-STORE-CD FD CD CH)
    (READ-AL-INTO-FONT-DESCRIPTOR-1 CD ARRAY CH 0))
  (SETF (FD-FILL-POINTER FD) 200)
  ;; Set width of blinker and space fields from the space character.
  (SETF (FD-BLINKER-WIDTH FD) (FD-SPACE-WIDTH FD))
  FD)

))

; From file FNTCNV.LISP PS:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "

(DEFUN WRITE-FONT-DESCRIPTOR-INTO-AL (FD &OPTIONAL FILENAME &AUX ARRAY CARRAY LINE-HEIGHT)
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "AL"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:OUT :FIXNUM))
    (FUNCALL STREAM ':TYO (SETQ LINE-HEIGHT (FD-LINE-SPACING FD)))
    (FUNCALL STREAM ':TYO (DPB (FD-BASELINE FD) 1007 (FD-SPACE-WIDTH FD)))
    (SETQ ARRAY (MAKE-ARRAY 1000. ':TYPE 'ART-16B ':LEADER-LIST '(0))	;Data array
	  CARRAY (MAKE-ARRAY 400 ':TYPE 'ART-16B ':LEADER-LIST '(400)))	;Non self-rel chars
    ;; Store dummy
    (ARRAY-PUSH-EXTEND ARRAY 1)
    (ARRAY-PUSH-EXTEND ARRAY 0)
    (LOOP FOR CH FROM 0 BELOW 200
	  AS CD = (AREF FD CH)
	  WHEN CD
	  DO (LOOP WITH CH = CH WITH (XW HD-XH)
		   WITH CHAR-WIDTH = (CD-CHAR-WIDTH CD)
		   FOR XOFF FROM 0 BY 16. BELOW CHAR-WIDTH
		   DO (SETQ HD-XH (WRITE-AL-COLUMN CD XOFF ARRAY))
		      (ASET (ARRAY-LEADER ARRAY 0) CARRAY CH)
		      (SETQ XW (IF (> (- CHAR-WIDTH XOFF) 16.)
				   (* (SETQ CH (PROG1 (ARRAY-LEADER CARRAY 0)
						      (ARRAY-PUSH-EXTEND CARRAY 0)))
				      2)
				   (1+ (* (- CHAR-WIDTH XOFF) 2))))
		      (ARRAY-PUSH-EXTEND ARRAY XW)
		      (ARRAY-PUSH-EXTEND ARRAY HD-XH))
	  ELSE DO (ASET 0 CARRAY CH))
    (LOOP FOR I FROM 0 BELOW (ARRAY-ACTIVE-LENGTH CARRAY)	;Make self-relative
	  DO (ASET (- (+ (AREF CARRAY I) (ARRAY-ACTIVE-LENGTH CARRAY)) I)
		   CARRAY I))
    (FUNCALL STREAM ':STRING-OUT CARRAY)
    (FUNCALL STREAM ':STRING-OUT ARRAY)
    (FUNCALL STREAM ':CLOSE)
    (FUNCALL STREAM ':TRUENAME)))

))

; From file COMF.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-QUICK-DOCUMENTATION "Prints documentation for the function point is at.
Prints the documentation string of the function which point is inside a call to.
With a numeric argument, reads the name of the function to document
from the mini buffer." ()
    (LET ((NAME (RELEVANT-FUNCTION-NAME (POINT))))
      (IF *NUMERIC-ARG-P*
	  (SETQ NAME (READ-FUNCTION-NAME "Brief Document" NAME T)))
      (IF (NULL NAME) (BARF)
	(LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	  (COND ((NULL DOC)
		 (FORMAT QUERY-IO "~&~S is not documented" NAME))
		(T (IF (FDEFINEDP NAME)
		       (PROGN (SEND STANDARD-OUTPUT ':FRESH-LINE)
			      (PRINT-ARGLIST NAME STANDARD-OUTPUT))
		     (FORMAT T "~S:" NAME))
		   (FORMAT T "~%~A" DOC))))))
    DIS-NONE)

))

; From file COMF.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-BRIEF-DOCUMENTATION "Prints brief documentation for the specified function.
Reads the name of the function from the mini-buffer (the default is
the /"current/" function from the buffer) and prints the first
line of its documentation in the echo area." ()
    (LET ((NAME (READ-FUNCTION-NAME "Brief Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (IF (NULL NAME) (BARF)
	(LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	  (COND ((NULL DOC) (FORMAT QUERY-IO "~&~S is not documented" NAME))
		(T (FORMAT QUERY-IO "~&~S: ~A" NAME
			   (NSUBSTRING DOC 0 (STRING-SEARCH-CHAR #\CR DOC))))))))
    DIS-NONE)

))

; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFCONST INITIALIZATION-KEYWORDS
	  '((SITE SITE-INITIALIZATION-LIST NOW)
	    (SITE-OPTION SITE-OPTION-INITIALIZATION-LIST NOW)
	    (SYSTEM SYSTEM-INITIALIZATION-LIST FIRST)
	    (FULL-GC FULL-GC-INITIALIZATION-LIST)
	    (AFTER-FULL-GC AFTER-FULL-GC-INITIALIZATION-LIST)
	    (AFTER-FLIP AFTER-FLIP-INITIALIZATION-LIST)
	    (ONCE ONCE-ONLY-INITIALIZATION-LIST FIRST)
	    (LOGIN LOGIN-INITIALIZATION-LIST)
	    (LOGOUT LOGOUT-INITIALIZATION-LIST)
	    (WARM WARM-INITIALIZATION-LIST)
	    (COLD COLD-INITIALIZATION-LIST)
	    (BEFORE-COLD BEFORE-COLD-INITIALIZATION-LIST))
  "Alist defining keywords accepted by ADD-INITIALIZATION.
Each element looks like (KEYWORD LIST-VARIABLE-NAME [TIME-TO-RUN])
TIME-TO-RUN should be NOW, FIRST, NORMAL or REDO, or omitted.
It is a default in case the ADD-INITIALIZATION doesn't specify any of them.")

))
