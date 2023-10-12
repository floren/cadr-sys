;;;-*- Mode:LISP; Package:CADR; Base:10 -*-

;;; Save all files on the object machine
(DEFUN SALVAGE-EDITOR ()
  "Save the files in the editor in the other machine, through the debugging cables.
Asks, about each buffer that is modified, whether to save it."
  (PKG-BIND "CADR"
    (QF-CLEAR-CACHE T)				;Make completely certain no garbage
						;in caches screws this!
    (QF-SETUP-Q-FIELDS)
    (DO ((BUFFER-LIST (CC-MEM-READ (1+ (QF-POINTER (QF-SYMBOL 'ZWEI:*ZMACS-BUFFER-LIST*))))
		      (QF-CDR BUFFER-LIST))
	 BUFFER)
	((CC-Q-NULL BUFFER-LIST))
      (SETQ BUFFER (QF-CAR BUFFER-LIST))
      (AND (LET ((NODE-TICK (QF-AR-OR-IR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:NODE-TICK)))
		 (BUFFER-TICK
		   (GET-SLOT-OR-IV BUFFER 'ZWEI:BUFFER-TICK 'ZWEI:FILE-TICK)))
	     (AND (= DTP-FIX (LOGLDB %%Q-DATA-TYPE NODE-TICK))
		  (= DTP-FIX (LOGLDB %%Q-DATA-TYPE BUFFER-TICK))
		  (> (LOGLDB %%Q-POINTER NODE-TICK) (LOGLDB %%Q-POINTER BUFFER-TICK))))
	   (MULTIPLE-VALUE-BIND (BUFFER-NAME FILE-NAME)
	       (SALVAGE-EDITOR-FILE-NAME BUFFER)
	     (AND (FQUERY NIL "Save buffer ~A~@[ on file ~A~]? " BUFFER-NAME FILE-NAME)
		  (SALVAGE-INTERVAL BUFFER
				    (COND (FILE-NAME)
					  (T
					   (FORMAT QUERY-IO "~&Write ~A to file: " BUFFER-NAME)
					   (READLINE QUERY-IO))))))))))

(DEFUN SALVAGE-EDITOR-FILE-NAME (BUFFER &AUX BUFFER-NAME FILE-NAME)
  (DECLARE (RETURN-LIST BUFFER-NAME FILE-NAME))
  (SETQ BUFFER-NAME (WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
		      (CC-Q-PRINT-STRING
			(GET-SLOT-OR-IV BUFFER 'ZWEI:BUFFER-NAME 'ZWEI:NAME))))
  (OR (LET ((FILE-ID (GET-SLOT-OR-IV BUFFER 'ZWEI:BUFFER-FILE-ID 'ZWEI:FILE-ID)))
	(CC-Q-NULL FILE-ID))
      (LET* ((PATHNAME (GET-SLOT-OR-IV BUFFER 'ZWEI:BUFFER-PATHNAME 'ZWEI:PATHNAME))
	     (NAME (GET-INSTANCE-VARIABLE PATHNAME 'FS:STRING-FOR-PRINTING)))
	(IF (CC-Q-NULL NAME)
	    (SETQ FILE-NAME BUFFER-NAME)	;Best we can do if no string cached yet
	    (SETQ FILE-NAME (WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
			      (CC-Q-PRINT-STRING NAME))))))
  (VALUES BUFFER-NAME FILE-NAME))

(DEFUN SALVAGE-ZMAIL ()
  "Save the mail files in the ZMAIL editor in the other machine, through the debugging cables.
Asks, about each buffer that is modified, whether to save it."
  (LET* ((ZM-WINDOW (QF-TYPED-POINTER
		      (CC-MEM-READ (1+ (QF-POINTER (QF-SYMBOL 'ZWEI:*ZMAIL-WINDOW*))))))
	 (closure (get-instance-variable zm-window 'zwei:editor-closure t))
	 (mf-list (qf-symeval-in-closure closure 'zwei:*zmail-buffer-list*)))
    (DO ((LIST MF-LIST (QF-CDR LIST))
	 MAIL-FILE)
	((CC-Q-NULL LIST))
      (SETQ MAIL-FILE (QF-CAR LIST))
      (WHEN (LET ((NODE-TICK (QF-AR-OR-IR-1 MAIL-FILE (GET-DEFSTRUCT-INDEX 'ZWEI:NODE-TICK)))
		  (BUFFER-TICK
		    (GET-SLOT-OR-IV MAIL-FILE 'ZWEI:BUFFER-TICK 'ZWEI:FILE-TICK)))
	      (AND (= DTP-FIX (LOGLDB %%Q-DATA-TYPE NODE-TICK))
		   (= DTP-FIX (LOGLDB %%Q-DATA-TYPE BUFFER-TICK))
		   (> (LOGLDB %%Q-POINTER NODE-TICK) (LOGLDB %%Q-POINTER BUFFER-TICK))))
	(LET ((MAIL-FILE-NAME (LET* ((PATHNAME (GET-INSTANCE-VARIABLE
						 MAIL-FILE 'ZWEI:PATHNAME))
				     (NAME (GET-INSTANCE-VARIABLE
					     PATHNAME 'FS:STRING-FOR-PRINTING)))
				(WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
				  (CC-Q-PRINT-STRING
				    (IF (CC-Q-NULL NAME)
					(GET-INSTANCE-VARIABLE
					  MAIL-FILE 'ZWEI:NAME)
				      NAME))))))
	  (AND (FQUERY NIL "Save mail file ~A? " MAIL-FILE-NAME)
	       (SALVAGE-INTERVAL MAIL-FILE MAIL-FILE-NAME)))))
    (LET ((DRAFT-LIST (qf-symeval-in-closure closure 'zwei:*draft-list*)))
      (DO ((LIST DRAFT-LIST (QF-CDR LIST))
	   DRAFT-MSG)
	  ((CC-Q-NULL LIST))
	(SETQ DRAFT-MSG (QF-CAR LIST))
	(COND ((CC-Q-NULL (GET-SLOT-OR-IV DRAFT-MSG 'ZWEI:DRAFT-MSG-SENT-P 'ZWEI:SENT-P))
	       (LET ((PATHNAME (GET-SLOT-OR-IV DRAFT-MSG 'ZWEI:DRAFT-MSG-PATHNAME
					       'ZWEI:PATHNAME))
		     (SUMMARY-STRING
		       (GET-SLOT-OR-IV DRAFT-MSG 'ZWEI:DRAFT-MSG-SUMMARY-STRING
				       'ZWEI:SUMMARY-STRING)))
		 (IF (CC-Q-NULL PATHNAME)
		     (SETQ PATHNAME NIL)
		   (SETQ PATHNAME (WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
				    (CC-Q-PRINT-STRING
				      (GET-INSTANCE-VARIABLE
					PATHNAME 'FS:STRING-FOR-PRINTING)))))
		 (SETQ SUMMARY-STRING (WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
					(CC-Q-PRINT-STRING SUMMARY-STRING)))
		 (COND ((FQUERY NIL "Save ~A~@[ on ~A~]? " SUMMARY-STRING PATHNAME)
			(COND ((NULL PATHNAME)
			       (FORMAT QUERY-IO "~&Write ~A to file: " SUMMARY-STRING)
			       (SETQ PATHNAME (READLINE QUERY-IO))))
			(SALVAGE-INTERVAL DRAFT-MSG PATHNAME))))))))))

(DEFUN OPEN-SOME-FILE (INITIAL-FILE &REST KEYWORD-ARGS &AUX ACTUAL-FILE)
  "Open INITIAL-FILE, or read another filename if INITIAL-FILE's host is down.
KEYWORD-ARGS are passed to OPEN."
  (SETQ ACTUAL-FILE
	(LOOP FOR FILE = INITIAL-FILE
	      THEN (FS:MERGE-PATHNAME-DEFAULTS
		     (PROGN (FORMAT T "~%Host ~a not available."
				    (FUNCALL HOST :NAME))
			    (FORMAT T "~%Use what pathname instead? (default = ~a) " FILE)
			    (READLINE))
		     FILE)
	      FOR HOST = (FUNCALL (FS:PARSE-PATHNAME FILE) :HOST)
	      UNTIL (CHAOS:HOST-UP-P HOST (* 10. 60.))
	      FINALLY (RETURN FILE)))
  (LEXPR-FUNCALL 'OPEN ACTUAL-FILE KEYWORD-ARGS))

(DEFVAR STRING-CODE :UNBOUND
  "Within SALVAGE-INTERVAL, holds unshifted array type field for ART-STRINGs.")

(DEFVAR FAT-STRING-CODE :UNBOUND
  "Within SALVAGE-INTERVAL, holds unshifted array type field for ART-FAT-STRINGs.")

;;; Write out one file
(DEFUN SALVAGE-INTERVAL (BUFFER FILE-NAME)
  (LET ((STRING-CODE (LDB %%ARRAY-TYPE-FIELD ART-STRING))
	(FAT-STRING-CODE (LDB %%ARRAY-TYPE-FIELD ART-FAT-STRING)))
    (WITH-OPEN-STREAM (STREAM (OPEN-SOME-FILE FILE-NAME '(:OUT)))
      (DO ((LINE-NEXT (GET-DEFSTRUCT-INDEX 'ZWEI:LINE-NEXT 'ARRAY-LEADER))       
	   (LINE (QF-CAR (QF-AR-OR-IR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:INTERVAL-FIRST-BP)))
		 (QF-ARRAY-LEADER LINE LINE-NEXT))
	   (LIMIT (QF-CAR (QF-AR-OR-IR-1 BUFFER
					 (GET-DEFSTRUCT-INDEX 'ZWEI:INTERVAL-LAST-BP)))))
	  (NIL)
	(COND (LINE				;CAN BE NIL IF IT BOMBS ON THE LOSER AND HE RETURNS NIL FROM EH
	       (SALVAGE-LINE LINE STREAM)
	       (FUNCALL STREAM :TYO #\CR)))
	(COND ((OR (NULL LINE) (= LINE LIMIT))
	       (CLOSE STREAM)
	       (FORMAT T "~&Written: ~A~%" (FUNCALL STREAM :TRUENAME))
	       (RETURN NIL)))))))

(DEFUN GET-DEFSTRUCT-INDEX (SYM &OPTIONAL TYPE)
  (CC-GET-DEFSTRUCT-INDEX SYM TYPE))  ;this one really gets it from remote machine.

(COMMENT ;this one assumes slot number the same in local and remote machines
;;; Figure out the index for array-leader or aref generated by defstruct
 (DEFUN GET-DEFSTRUCT-INDEX (SYM TYPE)
  (LET ((DEF (FSYMEVAL SYM)))
    (OR (AND (EQ (CAR DEF) 'NAMED-SUBST)
	     (EQ (CAAR (CDDDR DEF)) TYPE)
	     (CADDR (CADDDR DEF)))
	(FERROR NIL "Unable to get defstruct index for ~S: get help!" SYM)))) )

;Used when something is an array in one version and an instance in another.
(DEFUN GET-SLOT-OR-IV (OBJECT ARRAY-SLOT-ACCESSOR INSTANCE-VARIABLE)
  (LET ((SLOT (CC-GET-DEFSTRUCT-INDEX ARRAY-SLOT-ACCESSOR 'AREF)))
    (IF SLOT (QF-AR-1 OBJECT SLOT)
      (GET-INSTANCE-VARIABLE OBJECT INSTANCE-VARIABLE))))

(DEFUN GET-INSTANCE-VARIABLE (FLAVOR COMPONENT &OPTIONAL NO-ERROR)
  (LET ((IDX (GET-INSTANCE-VARIABLE-INDEX FLAVOR COMPONENT NO-ERROR)))
    (AND IDX (QF-P-CONTENTS (+ (QF-POINTER FLAVOR) IDX)))))

(DEFUN QF-SYMBOL-GET (SYMBOL INDICATOR)
  (DO ((PLIST (CC-MEM-READ (+ (QF-POINTER (QF-SYMBOL SYMBOL)) 3))
	      (QF-CDR (QF-CDR PLIST)))
       (IND (QF-SYMBOL INDICATOR)))
      ((CC-Q-NULL PLIST))
    (AND (= (QF-TYPED-POINTER (QF-CAR PLIST)) IND)
	 (RETURN (QF-CAR (QF-CDR PLIST))))))

(DEFUN QF-%P-CONTENTS-OFFSET (PTR OFFSET)
  (QF-P-CONTENTS (+ (QF-POINTER PTR) OFFSET)))

(DEFUN GET-INSTANCE-VARIABLE-INDEX (INSTANCE COMPONENT &OPTIONAL NO-ERROR)
  (LET* ((FLAVOR-DEFSTRUCT (CC-P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0))
	 (BINDINGS (CC-REF-DEFSTRUCT
		     'SI:FLAVOR-ALL-INSTANCE-VARIABLES
		     FLAVOR-DEFSTRUCT
		     'AREF))
	 (QF-COMPONENT (QF-SYMBOL COMPONENT)))
    (DO ((INDEX 1 (1+ INDEX))
	 (BINDING BINDINGS (QF-CDR BINDING)))
	((QF-NULL BINDING)
	 (AND (NOT NO-ERROR)
	      (FERROR NIL "Unable to find binding of ~S" COMPONENT)))
      (COND ((= QF-COMPONENT
		(QF-CAR BINDING))
	     (RETURN INDEX))))))

(DEFUN SALVAGE-LINE (LINE STREAM)
  (LET ((ARRAY-TYPE-CODE
	  (LDB %%ARRAY-TYPE-FIELD (CC-MEM-READ (LOGLDB %%Q-POINTER LINE)))))
    (COND ((= ARRAY-TYPE-CODE STRING-CODE)
	   (CC-Q-PRINT-STRING LINE NIL STREAM))
	  ((= ARRAY-TYPE-CODE FAT-STRING-CODE)
	   (QF-ARRAY-SETUP (QF-MAKE-Q (QF-POINTER LINE) DTP-ARRAY-POINTER))
	   (DO ((LEN (QF-POINTER (QF-MEM-READ (- QF-ARRAY-HEADER-ADDRESS 2))))
		(ADR QF-ARRAY-DATA-ORIGIN)
		(I 0 (1+ I))
		(CH) (WD)
		(FONT-FLAG 0)
		(FNT))
	       (( I LEN)
		(OR (ZEROP LEN) (ZEROP FONT-FLAG)
		    (FUNCALL STREAM :STRING-OUT "0")))
	     (COND ((ZEROP (LOGAND 1 I))	;Get next word
		    (SETQ WD (QF-MEM-READ ADR)
			  ADR (1+ ADR))))
	     (SETQ CH (LOGAND #o177777 WD)
		   WD (CC-SHIFT WD -16.))
	     (SETQ FNT (LSH CH -8))
	     (COND (( FNT FONT-FLAG)
		    (FUNCALL STREAM :TYO #/)
		    (FUNCALL STREAM :TYO (+ #/0 FNT))
		    (SETQ FONT-FLAG FNT)))
	     (FUNCALL STREAM :TYO (LOGAND CH #o377)))))))
