;;; -*- Mode:LISP; Package:SI; Fonts:CPTFONT; Base:8; Readtable:T -*-
;;; SYS: SITE; NETU, A module of MIT-Specific
;;; Random network-related utilities.

(DEFVAR *MIT-OFFICIAL-HOST-TABLE-SOURCE*
	(FS:PARSE-PATHNAME "MC:SYSNET;HSTMIT >"))

(DEFVAR *MIT-L-SYSTEM-HOST-TABLE-COPY*
	(FS:PARSE-PATHNAME "OZ:OZ:<L.CHAOS>HOSTS.TEXT"))

;;; NOTE: This function determines whether to copy the MC host table based on
;;; NUMERIC VERSION number.  It does not compare write dates.

(DEFUN GET-LATEST-HOST-TABLE (&AUX MC-HT-TN LS-HT-TN (COPY-STATUS :NOT-TESTED))
  (CONDITION-BIND ((FS:FILE-ERROR
		     #'(LAMBDA (COND)
			 (FORMAT T "~&Error in file-handling:~%")
			 (SEND COND :REPORT ERROR-OUTPUT))))
    (IF (NOT (SETQ MC-HT-TN (PROBEF *MIT-OFFICIAL-HOST-TABLE-SOURCE*)))
	 (SETQ COPY-STATUS :NO-MC-HOST-TABLE)
       (SETQ LS-HT-TN (PROBEF *MIT-L-SYSTEM-HOST-TABLE-COPY*))
       (SETQ COPY-STATUS :LOOKED-AT-L-SYS)
       (IF (NOT (OR (NULL LS-HT-TN)
		 (> (SEND MC-HT-TN :VERSION) (SEND LS-HT-TN :VERSION))))
	   (SETQ COPY-STATUS :DIDNT-HAVE-TO)
	 (FORMAT T "~&Copying new host table from ~A ..." MC-HT-TN)
	 (WITH-OPEN-FILE (FROM-MC MC-HT-TN :ERROR T :CHARACTERS T :RAW T)
	   (SETQ COPY-STATUS :OPENED-MC)
	   (WITH-OPEN-FILE (TO-OZ (SEND *MIT-L-SYSTEM-HOST-TABLE-COPY* :NEW-VERSION
					(SEND MC-HT-TN :VERSION))
				  :ERROR T :CHARACTERS T :RAW T :DIRECTION :OUTPUT)
	     (SETQ COPY-STATUS :OPENED-OZ)
	     (FORMAT T " to ~A" (SETQ LS-HT-TN (SEND TO-OZ :TRUENAME)))
	     (STREAM-COPY-UNTIL-EOF FROM-MC TO-OZ)
	     (SETQ COPY-STATUS :DONE))))))
  (SEND *STANDARD-OUTPUT* :FRESH-LINE)
  (SEND *STANDARD-OUTPUT* :STRING-OUT
	(SELECTQ COPY-STATUS
	  (:NOT-TESTED "[Never got to test MC's file]")
	  (:NO-MC-HOST-TABLE "[Theft]")
	  (:LOOKED-AT-L-SYS "[Determined status of the LM source file]")
	  (:OPENED-MC "[MC responded OK]")
	  (:OPENED-OZ "[OZ responded OK]")
	  (:DIDNT-HAVE-TO "[Never had to copy]")
	  (:DONE
	   (FORMAT T "~&So far, so good.~%")
	   (WHEN (Y-OR-N-P "Want to recompile the site files ? ")
	     (MAKE-SYSTEM :SITE :COMPILE))
	   (SEND *STANDARD-OUTPUT* :FRESH-LINE)
	   "[Finished]")))
  (SEND *STANDARD-OUTPUT* :TYO #/CR))

(DEFCONST *TREMONT-BROKEN-FILE* "MC:.DOVR.;.DOVR. BROKEN")
(DEFCONST *TREMONT-NOTICE-FILE* "MC:.DOVR.;.DOVR. NOTICE")

(DEFUN GET-DOVER-NOTICE-REASON (START)
  (ZWEI:POP-UP-EDSTRING START '(:MOUSE)
   '("[Edit Dover Notice] " ZWEI:*MODE-NAME-LIST* "  [End exits, Abort aborts]")
   615. 215. "Type in your notice."))

(DEFUN POST-DOVER-DOWN (&AUX REASON)
  (AND (SETQ REASON (GET-DOVER-NOTICE-REASON "The Dover is down:"))
       (COND ((PROBEF *TREMONT-BROKEN-FILE*)
	      (FORMAT T "~&Somebody has already said the dover was down:~%")
	      (FS:VIEWF *TREMONT-BROKEN-FILE*)
	      (TERPRI)
	      (AND (NOT (PROBEF *TREMONT-NOTICE-FILE*))
		   (FORMAT T "~%There's no notice file.~%"))
	      (WHEN (Y-OR-N-P "Do you want to post your comment in the notice file ? ")
		(POST-DOVER-NOTICE REASON T)))
	     (T (POST-DOVER-NOTICE-IN-FILE REASON *TREMONT-BROKEN-FILE*)
		(POST-DOVER-NOTICE REASON)))))
	 
(DEFUN POST-DOVER-NOTICE (&OPTIONAL (REASON (GET-DOVER-NOTICE-REASON "Note:")) NO-QUERY)
  (AND REASON
       (WHEN (OR NO-QUERY
		 (NOT (PROBEF *TREMONT-NOTICE-FILE*))
		 (PROGN
		   (FORMAT T "~&There already seems to be a notice file:~%")
		   (FS:VIEWF *TREMONT-NOTICE-FILE*)
		   (TERPRI)
		   (Y-OR-N-P "Replace that comment with yours ? ")))
	 (POST-DOVER-NOTICE-IN-FILE REASON *TREMONT-NOTICE-FILE*))))
  
(DEFUN POST-DOVER-NOTICE-IN-FILE (REASON FILE)
  (WITH-OPEN-FILE (Z FILE :DIRECTION :OUTPUT)
    (FORMAT T "~&Posting to ~A...~%" FILE)
    (SEND Z :STRING-OUT REASON)
    (FORMAT Z "~2%          ~A <~A @ ~A> ~\time\~%"
	    FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST USER-ID FS:USER-LOGIN-MACHINE
	    (TIME:GET-UNIVERSAL-TIME))))

(DEFUN DOVER-UP-AGAIN (&OPTIONAL CATCH22)
  (DELETEF *TREMONT-BROKEN-FILE*)
  (IF (NOT CATCH22) (DELETEF *TREMONT-NOTICE-FILE*)
    (POST-DOVER-NOTICE CATCH22 T)))

(DEFUN REMOVE-DOVER-NOTICE () (DELETEF *TREMONT-NOTICE-FILE*))
