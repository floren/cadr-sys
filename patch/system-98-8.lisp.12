;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.8
;;; Reason: Common Lisp EVAL-WHEN changes; Y-OR-N-P and YES-OR-NO-P changes.
;;; *READ-SUPPRESS*.  Potential numbers.  | syntax change.  :: vs #:.
;;; DEFSTRUCT bugs in QC-FILE.  FILE-WRITE-DATE.
;;; Add Patch confirmation bug.  Compile and Load File bug.
;;; Written 12/12/83 19:49:24 by RMS,
;;; while running on Lisp Machine Nine from band 3
;;; with Bad Inconsistently updated System 98.6, CADR 3.1, Experimental ZMail 53.0, MIT-Specific 22.0, microcode 305, ZM MIT.


(globalize "FILE-WRITE-DATE")
(remob 'file-creation-date)
(globalize "*READ-SUPPRESS*")

(load "sys:io;rdtbl")
(load "sys:io;crdtbl")

si:(SETQ INITIAL-READTABLE READTABLE
	 READTABLE (COPY-READTABLE READTABLE)
	 STANDARD-READTABLE READTABLE)
si:(SETQ INITIAL-COMMON-LISP-READTABLE COMMON-LISP-READTABLE
	 COMMON-LISP-READTABLE (COPY-READTABLE COMMON-LISP-READTABLE))

; From file COMC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-BUFFER-FORM (FORM TYPE)
  (DECLARE (SPECIAL COMPILE-PROCESSING-MODE))
  (IF (MEMQ TYPE '(DECLARE RANDOM SPECIAL))
      (EVAL FORM)
    (COMPILER:COMPILE-1 (CADR FORM)
			(APPEND
			  (SELECTQ (CAR FORM)
			    (DEFSUBST '(NAMED-SUBST))
			    (MACRO '(MACRO NAMED-LAMBDA))
			    (DEFUN '(NAMED-LAMBDA)))
			  (CDR (SI:PROCESS-DEFUN-BODY (CADR FORM) (CDDR FORM))))
			(IF (MEMQ (CAR FORM) '(MACRO DEFSUBST))
			    'COMPILER:MACRO-COMPILE
			  COMPILE-PROCESSING-MODE))))
))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN QC-FILE-COMMON (FORM TYPE)
    (COND ((MEMQ TYPE '(SPECIAL DECLARE MACRO))
	   ;; While evaluating the thing, turn off the temporary area, and
	   ;; if this is an EVAL-WHEN (COMPILE), turn off the undo-declarations
	   ;; flag so that macro definitions will really happen.
	   ;;;  NO, don't it screws DEFSTRUCT, which uses EVAL-WHEN (COMPILE LOAD EVAL).
	   ;; YES, do it, since DEFSTRUCT no longer uses EVAL-WHEN.
	   (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
		 (UNDO-DECLARATIONS-FLAG (AND UNDO-DECLARATIONS-FLAG
					      (NOT (EQ TYPE 'DECLARE))))
		 )
	     (OR QC-FILE-IN-CORE-FLAG (EVAL (COPYTREE FORM))))
	   ;; If supposed to compile or fasdump as well as eval, do so.
	   (COND ((EQ TYPE 'SPECIAL) (QC-FILE-FASD-FORM FORM NIL))
		 ((EQ TYPE 'MACRO)
		  (QC-TRANSLATE-FUNCTION (CADR FORM)
					 (DECLARED-DEFINITION (CADR FORM)) 
					 'MACRO-COMPILE
					 (IF QC-FILE-REL-FORMAT 'REL 'QFASL)))))
	  (QC-FILE-IN-CORE-FLAG (QC-FILE-FASD-FORM FORM T))
          (T (QC-FILE-FORM FORM))))

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN FASL-UPDATE-FORM (FORM TYPE)
    (SELECTQ TYPE
      (SPECIAL (FASD-FORM FORM NIL))
      (DECLARE)		;Ignore DECLAREs -- this may not always be right!
      ((DEFUN MACRO)	;Don't compile -- send over whatever is already compiled
        (OR (FDEFINEDP (CADR FORM))
	    (FERROR NIL "You forgot to define ~S" (CADR FORM)))
        (LET ((TEM (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC (CADR FORM)))))
	  (AND (CONSP TEM) (EQ (CAR TEM) 'MACRO) (SETQ TEM (CDR TEM)))
	  (COND ((AND (CONSP TEM) (FUNCTIONP TEM T))
		 (FORMAT ERROR-OUTPUT "~&Compiling ~S~%" (CADR FORM))
		 (COMPILE (CADR FORM))))
	  ;; This works on this bodiless DEFUN by virtue of the fact that FASD-FORM in
	  ;; Optimize mode calls FDEFINITION rather than looking at the form.
	  (FASD-FORM FORM T)))
      (OTHERWISE (FASD-FORM FORM T))))

;(COMPILE-DRIVER form processing-function override-fn) should be used by anyone
;trying to do compilation of forms from source files, or any similar operation.
;It knows how to decipher DECLAREs, EVAL-WHENs, DEFUNs, macro calls, etc.
;It doesn't actually compile or evaluate anything,
;but instead calls the processing-function with two args:
; a form to process, and a flag which is one of these atoms:
;  SPECIAL  -  QC-FILE should eval this and put it in the FASL file.
;		UNDO-DECLARATIONS-FLAG, if on, should stay on for this.
;  DECLARE  -  QC-FILE should eval this.
;  DEFUN    -  QC-FILE should compile this and put the result in the FASL file.
;  MACRO    -  This defines a macro.  QC-FILE should record a declaration
;               and compile it into the FASL file.
;  RANDOM   -  QC-FILE should just put this in the FASL file to be evalled.
;Of course, operations other than QC-FILE will want to do different things
;in each case, but they will probably want to distinguish the same cases.
;That's why COMPILE-DRIVER will be useful to them.

;override-fn gets to look at each form just after macro expansion.
;If it returns T, nothing more is done to the form.  If it returns NIL,
;the form is processed as usual (given to process-fn, etc.).
;override-fn may be NIL.

))

; From file QCFILE.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "


(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN &OPTIONAL COMPILE-TIME-TOO)
  (PROG TOP (FN (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	((ATOM FORM))
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM TOP NIL))
      (SETQ NFORM
	    (WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S at top level"
			     (CAR FORM))
	      (MACROEXPAND-1 FORM)))
      (IF (EQ FORM NFORM) (RETURN)
	(SETQ FORM NFORM)))
    ;; If this was a top-level macro, supply a good guess
    ;; for the function-parent for any DEFUNs inside the expansion.
    (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (COND ((ATOM FORM))
	    ((AND (NEQ FORM OFORM) (SYMBOLP (CADR OFORM)))
	     (PUSH `(FUNCTION-PARENT ,(CADR OFORM)) LOCAL-DECLARATIONS))
	    ((MEMQ (CAR OFORM) '(DEFSTRUCT :DEFSTRUCT))
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (AND (CONSP FORM)
	   (NEQ (CAR FORM) 'EVAL-WHEN)
	   COMPILE-TIME-TOO
	   (FUNCALL PROCESS-FN FORM 'DECLARE))
      (COND ((ATOM FORM))
	    ((EQ (CAR FORM) 'EVAL-WHEN)
	     (OR (AND (OR (NOT (ATOM (CADR FORM))) (NULL (CADR FORM)))	;LISTP eventually
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE))))
		 (FERROR NIL "~S invalid EVAL-WHEN times;
must be a list of EVAL, LOAD, and//or COMPILE."
			     (CADR FORM)))
	     (LET* ((COMPILE (MEMQ 'COMPILE (CADR FORM)))
		    (LOAD (MEMQ 'LOAD (CADR FORM)))
		    (EVAL (MEMQ 'EVAL (CADR FORM)))
		    (EVAL-NOW (OR COMPILE (AND COMPILE-TIME-TOO EVAL))))
	       (DOLIST (FORM1 (CDDR FORM))
		 (IF LOAD
		     (IF EVAL-NOW
			 (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN T)
		       (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN))
		   (IF EVAL-NOW
		       (FUNCALL PROCESS-FN FORM1 'DECLARE))))))
	    ((EQ (SETQ FN (CAR FORM)) 'DEFF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ FN 'DEF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)))
		   (CDDR FORM)))
	    ((EQ FN 'WITH-SELF-ACCESSIBLE)
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)))
		   (CDDR FORM)))
	    ((EQ FN 'PROGN)
	     (MAPC (FUNCTION (LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)))
		   (CDR FORM)))
	    ((MEMQ FN '(MACRO DEFSUBST))
	     (FUNCALL PROCESS-FN FORM 'MACRO))
	    ((MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
				EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT DEFF-MACRO))
	     (FUNCALL PROCESS-FN FORM 'SPECIAL))
	    ((EQ FN 'DECLARE)
	     (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	    ((EQ FN 'COMMENT) NIL)
	    ((EQ FN 'PATCH-SOURCE-FILE)
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)
	     (MAPC (FUNCTION (LAMBDA (FORM)
			       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)))
		   (CDDR FORM))
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING NIL))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO))
	    ((EQ FN 'COMPILER-LET)
	     (EVAL `(LET ,(CADR FORM) (COMPILE-DRIVER '(PROGN 'COMPILE . ,(CDDR FORM))
						      ',PROCESS-FN ',OVERRIDE-FN
						      ,COMPILE-TIME-TOO))))
	    ((EQ FN 'DEFUN)
	     (LET (TEM)
	       (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed defun")
		 (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
	       (COND ((EQ (CDR TEM) (CDR FORM))
		      (IF (NULL (CDDR TEM))
			  (WARN 'MALFORMED-DEFUN ':IMPOSSIBLE
				"Malformed defun ~S" FORM)
			(FUNCALL PROCESS-FN FORM 'DEFUN)))
		     (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)))))
	    (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN RESTORE-SCREEN-FOR-COLD-LOAD-STREAM (&OPTIONAL DONT-ASK)
  (IF (OR DONT-ASK
	  (LET ((QUERY-IO TV:COLD-LOAD-STREAM))
	    (Y-OR-N-P "Restore the screen? ")))
      (BITBLT TV:ALU-SETA
	      (PIXEL-ARRAY-WIDTH (TV:MAIN-SCREEN-AND-WHO-LINE))
	      (PIXEL-ARRAY-HEIGHT (TV:MAIN-SCREEN-AND-WHO-LINE))
	      COLD-LOAD-STREAM-SAVED-SCREEN 0 0
	      (TV:MAIN-SCREEN-AND-WHO-LINE) 0 0)))

))

; From file COME.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COME  "

(DEFCOM COM-COMPILE-AND-EXIT "Compile the buffer and return from top-level" ()
  (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
  (COM-COMPILE-BUFFER)
  (OR (AND (FUNCALL STANDARD-OUTPUT ':INCOMPLETE-P)	;If any compiler messages
	   (NOT (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Exit anyway? "))))
      (*THROW 'EXIT-TOP-LEVEL NIL))
  DIS-NONE)

))

; From file DIRED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN REAP-ONE-FILE (HEAD TAIL N-TO-KEEP STREAM
		      &AUX LAST-VERSION FIRST-DELETION-VERSION (N-VERSIONS 0) THIS-VERSION
		      DELETE-LIST KEEP-LIST)
  (DO LIST HEAD (CDR LIST) (EQ LIST TAIL)
      (SETQ THIS-VERSION (FUNCALL (CAAR LIST) ':VERSION))
      (WHEN (NUMBERP THIS-VERSION)
	(IF (AND LAST-VERSION ( (1+ LAST-VERSION) THIS-VERSION))
	    (SETQ LAST-VERSION NIL N-VERSIONS 0))
	(UNLESS LAST-VERSION (SETQ FIRST-DELETION-VERSION THIS-VERSION))
	(SETQ N-VERSIONS (1+ N-VERSIONS)
	      LAST-VERSION THIS-VERSION)))
  ;; FIRST-DELETION-VERSION is lowest version number to delete.
  ;; That is the bottom of the sequence of consecutive versions
  ;; that ends with the most recent version.
  ;; N-VERSIONS is number of versions that exist, starting with that version.
  (DO ((LIST HEAD (CDR LIST))
       (N-TO-DELETE -1)
       (FILE) (PATHNAME) (VERSION))
      ((EQ LIST TAIL)
       (SETQ DELETE-LIST (NREVERSE DELETE-LIST)
	     KEEP-LIST (NREVERSE KEEP-LIST)))
    (SETQ FILE (CAR LIST)
	  PATHNAME (CAR FILE)
	  VERSION (FUNCALL PATHNAME ':VERSION))
    (IF (EQ VERSION FIRST-DELETION-VERSION)
	(SETQ N-TO-DELETE (- N-VERSIONS N-TO-KEEP)))
    (IF (AND (OR (AND (NUMBERP VERSION) (PLUSP N-TO-DELETE))
		 (MEMBER (FUNCALL PATHNAME ':TYPE) *TEMP-FILE-TYPE-LIST*))
	     (NOT (GET FILE ':DONT-REAP)))
	(PUSH FILE DELETE-LIST)
	(PUSH FILE KEEP-LIST))
    (AND (NUMBERP VERSION)
	 (SETQ N-TO-DELETE (1- N-TO-DELETE))))
  (COND (DELETE-LIST
	 (COND (KEEP-LIST
		(FORMAT STREAM "~&Keeping the following file~P:  (in ~A)~%"
			(LENGTH KEEP-LIST) (FUNCALL (CAAR KEEP-LIST) ':STRING-FOR-DIRECTORY))
		(DOLIST (FILE KEEP-LIST)
		  (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE))))
	 (FORMAT STREAM "~&Deleting the following file~P:~:[ (in ~A)~]~%"
		 (LENGTH DELETE-LIST) KEEP-LIST
		 (FUNCALL (CAAR DELETE-LIST) ':STRING-FOR-DIRECTORY))
	 (DOLIST (FILE DELETE-LIST)
	   (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE))
	 (AND (LET ((QUERY-IO STREAM))
		(Y-OR-N-P "Ok? "))
	      (DOLIST (L DELETE-LIST T)
		(LET ((PATHNAME (CAR L)))
		  (CONDITION-CASE (ERROR)
		      (SEND PATHNAME ':DELETE)
		    (FS:FILE-ERROR
		      (FORMAT STREAM "~&Cannot delete ~A because ~A.~%" PATHNAME ERROR)))))))))

))

; From file ISPELL.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ISPELL  "

(DEFUN CORRECT-SPELLING-1 (WORD RESULT STREAM IN-EDITOR-P)
  "Auxilary function used by CORRECT-SPELLING.  Given a word and the
result given by the spell server, return the correct spelling of a 
given word.   Ask questions of the user on stream STREAM.  If 
IN-EDITOR-P is T,  we will change the buffer to have the correct word in it."
  (COND ((CONSP RESULT)  ;word isn't spelled correctly
	 (IF IN-EDITOR-P (TYPEIN-LINE "Couldn't find it.   Choices listed above."))
	 (LET ((REPLACEMENT
		 (USER-CHOOSE-WORD WORD RESULT STREAM IN-EDITOR-P)))
	   (IF IN-EDITOR-P  ;fix up the screen
	       (SEND STANDARD-OUTPUT ':MAKE-COMPLETE))
	   (AND (NOT (EQUALP REPLACEMENT WORD))
		IN-EDITOR-P
		(REPLACE-WORD-WITH WORD REPLACEMENT
		  (LET ((QUERY-IO STREAM))
		    (Y-OR-N-P "Should I query replace your change as well? "))))))
	((STRING-EQUAL "t" RESULT)   ;;word exists.
	 (IF IN-EDITOR-P (TYPEIN-LINE "Found it."))
	 WORD)
	((STRING-EQUAL "nil" RESULT) ;word is spelled incorrectly
	 (IF IN-EDITOR-P (TYPEIN-LINE "Couldn't find it."))
	 WORD)  ;;leave it alone.
;;(QQUERY NIL  "Type R to replace the word with a word of your choice,
;;or type A to accept the word as it is." (LIST #/r #/R #/a #/A))
;;(IF (< 3 RESULT) (GET-CORRECT-SPELLING-FROM-USER WORD) WORD))
	(T
	 (FERROR "An unparsable result ~A was obtained.
Please send a bug report to BUG-ISPELL." RESULT))))

))

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFUN PROMPT-LINE-Y-OR-N-P (&OPTIONAL STRING &REST FORMAT-ARGS)
  "As the user for Y or N confirmation, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt."
  (DECLARE (:SELF-FLAVOR FED))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (LEXPR-FUNCALL #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (LET ((QUERY-IO PROMPT-WINDOW))
      (Y-OR-N-P NIL))))

))

; From file ALARM.LISP SRC:<L.DEMO> OZ:
#8R HACKS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: DEMO; ALARM  "

(DEFUN REMOVE-ALARM (&OPTIONAL ALARM ALARM-NUMBER CONFIRM)
  "Remove a specific alarm.  Asks the user for confirmation."
  ;;cond-every !
  (COND ((NULL ALARM)
	 (FORMAT QUERY-IO "~%Please type in the name an alarm (or just return to quit).
Valid alarms are ~A." (PRINT-LIST ALARM-TYPE-LIST QUERY-IO))
	 (SETQ ALARM (READLINE QUERY-IO))))
  (COND ((AND (NOT (NULL ALARM)) (NULL ALARM-NUMBER))
	 (FORMAT QUERY-IO "Please type the number of the ~A alarm that you want to be rid of." ALARM)
	 (SETQ ALARM-NUMBER (PARSE-NUMBER (READLINE QUERY-IO)))))
  (COND ((AND (NOT (NULL ALARM)) ( 0 ALARM-NUMBER))
	 (IF (NULL CONFIRM)
	     (SETQ CONFIRM (Y-OR-N-P
			     (FORMAT NIL "Do you really want to remove yourself of alarm number ~A?" ALARM-NUMBER))))
	 (IF CONFIRM (REMOVE-ALARM-INTERNAL ALARM ALARM-NUMBER)))))

))

; From file WORM-TRAILS.LISP SRC:<L.DEMO> OZ:
#8R HACKS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: DEMO; WORM-TRAILS  "

(defun worm-trails ()
  (unwind-protect (progn
		    (if (null *worm-pond*) (initialize-pond))
		    (send *worm-pond* ':expose)
		    (send *worm-pond* ':select)
		    (do ((play-it-again t
					(let ((query-io *worm-pond*))
					  (y-or-n-p "Again? "))))
			((null play-it-again))
		      (send *worm-pond* ':clear-screen)
		      (let ((returnage (wander-around)))
			(send *worm-pond* ':set-cursorpos 0 0 ':character)
			(format *worm-pond*
				"Died of ~A~&Length: ~A~&Moves were ~A~%"
				(first returnage) (second returnage)
				(nreverse (cddr returnage))))))
		  (send *worm-pond* ':deselect)))

))

; From file CONVER.LISP SRC:<L.IO1> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; CONVER  "

(DEFUN CONVERSE-PROFILE (&OPTIONAL (QUERY-IO STANDARD-OUTPUT))
  "Set canonical Converse user options."
  (FORMAT QUERY-IO "~&You are now setting the values of the various Converse Options.
Type Control-Abort to stop doing so.
Note: Your init file is left unchanged.")
  (DOLIST (VARIABLE *Y-OR-N-CONVERSE-OPTIONS*)
    (FORMAT QUERY-IO "~2%The variable ~A has the value ~A and the following documentation: ~&~A~2%"
	    VARIABLE (SYMEVAL VARIABLE) (DOCUMENTATION VARIABLE))
    ;;there are better user interfaces
    (SETQ VARIABLE (Y-OR-N-P "Set this variable to T ? ")))	
  (FORMAT QUERY-IO "~2%There are other variables that normally contain values other than ~
simply NIL or T.~&")
  (COND ((Y-OR-N-P "Would you like to examine them? ")
	 (TERPRI QUERY-IO)
	 (LET ((DOC (Y-OR-N-P "Would you like to see their documentation as well? ")))
	   (DOLIST (VARIABLE *HAIRY-CONVERSE-OPTIONS*)
	     (FORMAT QUERY-IO "~2%Variable: ~A  ~&Value: ~A ~:[ ~;~&Documentation: ~&~A~]"
		     VARIABLE (SYMEVAL VARIABLE) DOC (DOCUMENTATION VARIABLE)))))))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN VIEW-MAIL (&OPTIONAL (USER USER-ID) OLD-MAIL-P (HOST FS:USER-LOGIN-MACHINE)
		  &AUX DIR POSSIBLE-FILES REAL-MAIL-FILE)
  "View someone's mail.  Defaults viewing your own mail.
If OLD-MAIL-P is T, look for an old mail file, as opposed to the latest mail.
You may specify a host either as an optional argument, or as part of the user field."
  (LET ((@-POS (STRING-SEARCH-CHAR #/@ USER))
	(USER (STRING-UPCASE USER))
	(FS:*DEFAULT-PATHNAME-DEFAULTS* NIL))  ;RANDOMNESS
    (COND ((NOT (NULL @-POS))  ;;FIGURE OUT IF USER WANTS A DIFFERENT MACHINE
	   (SETQ HOST (SUBSTRING USER (1+ @-POS)))
	   (SETQ USER (SUBSTRING USER 0 @-POS)))) ;now compute the mail file name.
    (SETQ HOST (SI:PARSE-HOST HOST))
    (SETQ DIR (fs:user-homedir host nil user)) ;;this still isn't right, but...
    (USING-RESOURCE (WINDOW TV:POP-UP-FINGER-WINDOW)
      (SETF (TV:SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
      (SEND WINDOW ':SET-LABEL (FORMAT NIL "Viewing ~A's ~:[new~;old~] mail on ~A"
				       USER OLD-MAIL-P (SEND HOST ':NAME)))
      (SEND WINDOW ':SET-PROCESS CURRENT-PROCESS)
      (TV:WINDOW-CALL (WINDOW :DEACTIVATE)
	(LET ((TERMINAL-IO WINDOW))
	  (SETQ TV:KBD-ESC-TIME NIL)	;Window configuration stable.
	  (IF (NULL OLD-MAIL-P)
	      (SETQ POSSIBLE-FILES (LIST (SEND DIR ':NEW-MAIL-PATHNAME)))
	    (SETQ POSSIBLE-FILES (SEND DIR ':POSSIBLE-MAIL-FILE-NAMES)))
	  (DOLIST (MAIL-FILE POSSIBLE-FILES)
	    (IF (NOT (ERRORP (OPEN MAIL-FILE '(:PROBE))))
		(SETQ REAL-MAIL-FILE MAIL-FILE))) ;we hope this one is ok.
	  (COND ((NULL REAL-MAIL-FILE)
		 (FORMAT WINDOW "~A's mail file appears to be empty. ~2%
Type any character to flush." USER)
		 (SEND WINDOW ':TYI))
	   (T
	    (FS:VIEWF REAL-MAIL-FILE)
	    (SEND WINDOW ':CLEAR-INPUT) ;just to be safe
	    (FORMAT WINDOW
		    "~2%~&Type ~C to delete ~A's mail, or anything else to flush.~@%"
		    #\DELETE USER)
	    (LET ((RESPONSE (SEND WINDOW ':TYI)))
	      (AND (STRING-EQUAL RESPONSE #\DELETE)
		   (LET ((QUERY-IO WINDOW))
		     (YES-OR-NO-P "Do you REALLY want to delete this mail ? "))
		   (SEND REAL-MAIL-FILE ':DELETE)))))))))
  NIL)

))

; From file FQUERY.LISP SRC:<L.IO1> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FQUERY  "

(DEFUN YES-OR-NO-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
  "Ask the user a question he can answer with Yes or No.
Beeps and discards type-ahead.
Passes the arguments to FORMAT.
With no args, asks the question without printing anything but the /"(Yes or No)/".
Returns T if the answer was yes."
  (FQUERY YES-OR-NO-P-OPTIONS
	  (AND FORMAT-STRING
	       (IF (= (AREF FORMAT-STRING (1- (LENGTH FORMAT-STRING))) #\SP)
		   "~&~?" "~&~? "))
	  FORMAT-STRING
	  FORMAT-ARGS))

))

; From file FQUERY.LISP SRC:<L.IO1> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FQUERY  "

(DEFUN Y-OR-N-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
  "Ask the user a question he can answer with Y or N.
Passes the arguments to FORMAT.
With no args, asks the question without printing anything but the /"(Y or N)/".
Returns T if the answer was yes."
  (FQUERY Y-OR-N-P-OPTIONS
	  (AND FORMAT-STRING
	       (IF (= (AREF FORMAT-STRING (1- (LENGTH FORMAT-STRING))) #\SP)
		   "~&~?" "~&~? "))
	  FORMAT-STRING
	  FORMAT-ARGS))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFVAR *READ-SUPPRESS* :UNBOUND
  "T means do not actually intern symbols that are read.
Used by read-time conditionals to skip what is not wanted.")

(FORWARD-VALUE-CELL '*READ-SUPPRESS* 'READ-DONT-INTERN)

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#=-MACRO (STREAM IGNORE &OPTIONAL (LABEL XR-SHARP-ARGUMENT) &AUX THING)
  (COND
    (*READ-SUPPRESS*
     (VALUES))
    ((NOT LABEL)
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "No argument (label number) to #= given."))
    ((ASSQ LABEL XR-LABEL-BINDINGS)
     ; The label is already defined, but we can't tell what it is yet.
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
	     "Label ~S already defined in this expression." LABEL))
    (T ; Go for it and BIND
     (PUSH (CONS LABEL (CAR-LOCATION (NCONS (GENSYM)))) ; Allocate a slot
	   XR-LABEL-BINDINGS)
     (LET ((LABEL-BINDING (ASSQ LABEL XR-LABEL-BINDINGS)))
       (IF (NULL LABEL-BINDING)
	   (FERROR 'SYS:READ-ERROR-1 "Internal error in #= after reading in label's value.")
	 ;; The preceding line should never happen.  By writing into the slot
	 ;; will RPLACD, we also cause other places that referred to the label
	 ;; to get the value, too.
	 (RPLACD (CDR LABEL-BINDING) (SETQ THING (INTERNAL-READ STREAM T NIL T)))
	 ;; If THING has no bindings in it, we can make the binding of THING itself instead of
	 ;; a locative
	 (IF (NOT (CONSP THING))
	     (RPLACD LABEL-BINDING (CDDR LABEL-BINDING)) ; Replace locative with object
	   ;; If there are no bindings as locatives in it
#|	   (IF (NOT (FIND-ANY-THINGS
		      (REMQ (CDR LABEL-BINDING)
			    (LET ((LOC-BINDINGS ()))
			      (DOLIST (BINDING XR-LABEL-BINDINGS LOC-BINDINGS)
				(IF (LOCATIVEP (CDR BINDING))
				    (PUSH (CDR BINDING) LOC-BINDINGS)))))
		      THING))|#
           (RPLACD LABEL-BINDING
		   (CDDR LABEL-BINDING))
	     	   ;; Now we examine the list
	   (NSUBST THING (CDR LABEL-BINDING) THING) ; Substitute for `self'
	   ; Catch the CDR - why does this happen ?
	   (LET ((LAST-CONS (NTHCDR (1- (LENGTH THING)) THING)))
	     (IF (LOCATIVEP (CDR LAST-CONS)) (RPLACD LAST-CONS (CDDR LAST-CONS)))))
	   ; Replace locative with object
	 THING))))
  )

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-##-MACRO (STREAM IGNORE &OPTIONAL (LABEL XR-SHARP-ARGUMENT))
  STREAM ; Not used, we never actually do a READ
  (COND
    (*READ-SUPPRESS* NIL)
    ((NOT LABEL)
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "No argument (label number) to ## given."))
    ((NULL (ASSQ LABEL XR-LABEL-BINDINGS))
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "The ##-label ~S is undefined." LABEL))
    (T
     (CDR (ASSQ LABEL XR-LABEL-BINDINGS))))
  )

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#*-MACRO (STREAM IGNORE &OPTIONAL (LENGTH XR-SHARP-ARGUMENT)
		    &AUX BIT-VECTOR LAST-ELEMENT-READ)
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (SETQ BIT-VECTOR (MAKE-ARRAY (OR LENGTH 10) ':TYPE 'ART-1B ':LEADER-LIST '(0)))
    (DO (CHAR INDEX ERROR-REPORTED) (())
      (SETF (VALUES CHAR INDEX) (XR-XRTYI STREAM NIL T))
      (SELECTOR CHAR CHAR-EQUAL
	((#/0 #/1)
	 (SETQ LAST-ELEMENT-READ (- CHAR #/0))
	 (IF LENGTH
	     (UNLESS (OR (ARRAY-PUSH BIT-VECTOR LAST-ELEMENT-READ)
			 ERROR-REPORTED)
	       (CERROR ':NO-ACTION NIL 'READ-ERROR-1
		       "Number of data bits exceeds specified length in #* bit vector construct.")
	       (SETQ ERROR-REPORTED T))
	   (ARRAY-PUSH-EXTEND BIT-VECTOR LAST-ELEMENT-READ)))
	(T
	 (AND LENGTH
	      ;; ARRAY-PUSH returns () when the fill pointer is at the end of the
	      ;; array.
	      (LOOP WHILE (ARRAY-PUSH BIT-VECTOR LAST-ELEMENT-READ)))
	 (XR-XRUNTYI STREAM CHAR INDEX)
	 (LET ((NVEC (MAKE-ARRAY (LENGTH BIT-VECTOR) ':TYPE ART-1B)))
	   (COPY-ARRAY-CONTENTS BIT-VECTOR NVEC)
	   (RETURN NVEC)))))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#A-MACRO (STREAM IGNORE &OPTIONAL (RANK XR-SHARP-ARGUMENT))
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (IF (AND (FIXNUMP RANK) (PLUSP RANK))
	(LET (DIMENSIONS (SEQUENCES (INTERNAL-READ STREAM T NIL T)))
	  (DO ((DIM 0 (1+ DIM))
	       (STUFF SEQUENCES (ELT STUFF 0)))
	      ((= DIM RANK))
	    (PUSH (LENGTH STUFF) DIMENSIONS))
	  (VALUES (MAKE-ARRAY (NREVERSE DIMENSIONS) ':INITIAL-CONTENTS SEQUENCES)))
      (CERROR ':NO-ACTION NIL 'READ-ERROR-1
	      "~S is not a valid array rank.")
      (INTERNAL-READ STREAM T NIL T)
      NIL)))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFUN XR-#C-MACRO (STREAM IGNORE IGNORE)
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (APPLY 'COMPLEX (INTERNAL-READ STREAM T NIL T))))

(DEFUN XR-#S-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (LET* ((ARGS (INTERNAL-READ STREAM T NIL T))
	   (CONSTRUCTOR
	     (DOLIST (C (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS
			  (GET (CAR ARGS) 'DEFSTRUCT-DESCRIPTION)))
	       (IF (NULL (CDR C)) (RETURN (CAR C))))))
      (IF CONSTRUCTOR
	  (EVAL (CONS CONSTRUCTOR
		      (LOOP FOR (SLOT VALUE) ON (CDR ARGS) BY 'CDDR
			    APPEND `(,SLOT ',VALUE))))
	(CERROR ':NO-ACTION NIL 'READ-ERROR-1
		"~S is not a structure type with a standard keyword constructor." (CAR ARGS))
	NIL))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFUN XR-#+-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (LET ((FEATURE (LET ((PACKAGE PKG-KEYWORD-PACKAGE)
		       (IBASE 10.))
		   (INTERNAL-READ STREAM T NIL T))))	;feature or feature list
    (COND (*READ-SUPPRESS*
	   (VALUES))
	  ((NOT (XR-FEATURE-PRESENT FEATURE))
	   (LET ((*READ-SUPPRESS* T))
	     (INTERNAL-READ STREAM T NIL T))
	   (VALUES))
	  (T
	   (VALUES (INTERNAL-READ STREAM T NIL T))))))

;  #-<FEATURE-FORM> is equivalent to #+(NOT FEATURE-FORM).

(DEFUN XR-#--MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (LET ((FEATURE (LET ((PACKAGE PKG-KEYWORD-PACKAGE)
		       (IBASE 10.))
		   (INTERNAL-READ STREAM T NIL T))))	;feature or feature list
    (COND (*READ-SUPPRESS*
	   (VALUES))
	  ((XR-FEATURE-PRESENT FEATURE)
	   (LET ((*READ-SUPPRESS* T))
	     (INTERNAL-READ STREAM T NIL T))
	   (VALUES))
	  (T
	   (VALUES (INTERNAL-READ STREAM T NIL T))))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#/(-MACRO (STREAM IGNORE
		     &OPTIONAL (LENGTH (UNLESS *READ-SUPPRESS* XR-SHARP-ARGUMENT)))
  (XR-XRUNTYI STREAM #/( 0)
  (LET* ((ELEMENTS (INTERNAL-READ STREAM T NIL T))
	 (VECTOR (MAKE-SEQUENCE 'VECTOR (OR LENGTH (LENGTH ELEMENTS))
				:INITIAL-ELEMENT (CAR (LAST ELEMENTS)))))
    (IF (< (LENGTH VECTOR) (LENGTH ELEMENTS))
	(CERROR ':NO-ACTION NIL 'READ-ERROR-1
		"Elements specified are more than the specified length in #(..) vector construct."))
    (REPLACE VECTOR ELEMENTS)
    VECTOR))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (IF *READ-SUPPRESS*
      (PROGN (READ-DELIMITED-LIST #/ STREAM T) NIL)
    (LET* ((FLAVOR-NAME
	     (LET ((PACKAGE PKG-USER-PACKAGE)) (INTERNAL-READ STREAM T NIL T)))
	   (INSTANCE
	     (LET ((HANDLER (OR (GET FLAVOR-NAME 'READ-INSTANCE)
				(GET-FLAVOR-HANDLER-FOR FLAVOR-NAME ':READ-INSTANCE)))
		   (SELF NIL))
	       (FUNCALL HANDLER ':READ-INSTANCE FLAVOR-NAME STREAM)))
	   (CHAR (TYI STREAM)))
      ;; Make sure that the read-instance function read as much as it was supposed to.
      (IF (EQ CHAR #/)
	  INSTANCE
	(FUNCALL STREAM ':UNTYI CHAR)
	(CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
		"Malformatted #~S... encountered during READ." FLAVOR-NAME)))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "
(DEFUN XR-READ-LIST (STREAM SHOULD-BE-NIL FIFTY)
       SHOULD-BE-NIL ;Ignored.  This would be the string if there was one
       (PROG (LIST THING TYPE END-OF-LIST BP CORRESPONDENCE-ENTRY
	      (INSIDE-COLUMN-0-LIST INSIDE-COLUMN-0-LIST)
	      (THIS-IS-COLUMN-0-LIST (AND READ-CHECK-INDENTATION
					  (EQ XR-XRTYI-PREV-CHAR #\RETURN))))
	     (AND THIS-IS-COLUMN-0-LIST
		  (NOT *READ-SUPPRESS*)
		  (IF (NOT INSIDE-COLUMN-0-LIST)
		      (SETQ INSIDE-COLUMN-0-LIST T)
		    ;; ( in column 0 when not allowed.
		    ;; Report it (but only report each occurrence once).
		    (OR MISSING-CLOSEPAREN-REPORTED
			(PROGN (SETQ MISSING-CLOSEPAREN-REPORTED T)
			       (SIGNAL-PROCEED-CASE (() 'SYS:MISSING-CLOSEPAREN
						     "Open paren found in column zero; missing closeparens assumed.")
				 (:NO-ACTION))))
		    ;; Unread it the char.  The -1 prevents barfage in XR-XRUNTYI, that's all.
		    (XR-XRUNTYI STREAM XR-XRTYI-LAST-CHAR -1)
		    ;; Set a signal for the XR-READ-LIST frame that called us.
		    (SETQ XR-SPLICE-P 'MISSING-CLOSEPAREN)
		    (RETURN NIL)))
	     (SETQ MISSING-CLOSEPAREN-REPORTED NIL)
             (SETQ END-OF-LIST (LOCF LIST))
	     (COND (XR-CORRESPONDENCE-FLAG
		    (FUNCALL STREAM ':UNTYI FIFTY)
		    (SETQ CORRESPONDENCE-ENTRY
			  `(NIL ,(FUNCALL STREAM ':READ-BP) NIL . ,XR-CORRESPONDENCE))
		    (SETQ XR-CORRESPONDENCE CORRESPONDENCE-ENTRY)
		    (FUNCALL STREAM ':TYI)))
	   A (AND XR-CORRESPONDENCE-FLAG
		  (PUSH (FUNCALL STREAM ':READ-BP)
			(CADDR CORRESPONDENCE-ENTRY)))
	     (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     ;; If this is the first element of a list starting in column 0,
	     ;; and it is EVAL-WHEN or something like that,
	     ;; say it is ok for our sublists to start in column 0.
	     (AND THIS-IS-COLUMN-0-LIST
		  (EQ END-OF-LIST (LOCF LIST))
		  (SYMBOLP THING)
		  ;; It is usually dangerous for READ to look at properties of symbols,
		  ;; but this will only happen for the symbol after a paren in column 0
		  ;; and never in lists read in interactively.
		  (GET THING 'MAY-SURROUND-DEFUN)
		  (SETQ INSIDE-COLUMN-0-LIST NIL))
	     (COND ((EQ TYPE 'READER-MACRO)
		    (COND (XR-CORRESPONDENCE-FLAG
			   (FUNCALL STREAM ':UNTYI FIFTY)
			   (SETQ BP (FUNCALL STREAM ':READ-BP))
			   (FUNCALL STREAM ':TYI)))
		    (LET ((XR-LIST-SO-FAR LIST)
			  (XR-SPLICE-P NIL)
			  VALUES)
		      (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
		      (COND ((EQ XR-SPLICE-P 'MISSING-CLOSEPAREN)
			     ;; This means that the reader macro was (
			     ;; and it was unhappy about being at column 0
			     ;; inside another column 0 list.
			     ;; Pretend we saw a ).
			     (AND XR-CORRESPONDENCE-FLAG
				  (RPLACA CORRESPONDENCE-ENTRY LIST)
				  (SETF (CADDR CORRESPONDENCE-ENTRY)
					(NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
			     (RETURN LIST 'LIST))
			    (XR-SPLICE-P
			     (SETQ LIST XR-LIST-SO-FAR)
			     (AND XR-CORRESPONDENCE-FLAG
				  (SETF (CADDR CORRESPONDENCE-ENTRY)
					(FIRSTN (LENGTH LIST)
						(CADDR CORRESPONDENCE-ENTRY))))
			     (SETQ END-OF-LIST
				   (COND ((ATOM LIST) (LOCF LIST))
					 (T (LAST LIST)))))
			    (VALUES
			     (RPLACD END-OF-LIST
				     (SETQ VALUES (COPYLIST VALUES READ-AREA)))
			     (SETQ END-OF-LIST (LAST VALUES))
			     (AND XR-CORRESPONDENCE-FLAG
				  (SETQ XR-CORRESPONDENCE
					`(,(CAR VALUES) ,BP NIL . ,XR-CORRESPONDENCE)))
			     )))
		    (GO A))
		   ((EQ TYPE 'SPECIAL-TOKEN)
		    (COND ((EQ THING 'CLOSE)
			   (AND XR-CORRESPONDENCE-FLAG
				(RPLACA CORRESPONDENCE-ENTRY LIST)
				(SETF (CADDR CORRESPONDENCE-ENTRY)
				      (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
			   (RETURN LIST 'LIST))
			  ((EQ THING 'EOF)
			   (OR (AND READ-CHECK-INDENTATION MISSING-CLOSEPAREN-REPORTED)
			       (SIGNAL-PROCEED-CASE
				   (() 'SYS:READ-LIST-END-OF-FILE
				    "End of file on ~S in the middle of the list ~:S."
				    STREAM LIST)
				 (:NO-ACTION
				   (IF READ-CHECK-INDENTATION
				       (SETQ MISSING-CLOSEPAREN-REPORTED T)))))
			   (RETURN LIST 'LIST))
			  (*READ-SUPPRESS* NIL)
			  ((EQ THING 'CONSING-DOT)
			   (WHEN (NULL LIST)
			     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
				     "A dot was read before any list was accumulated.")
			     (GO A))
			   (GO RDOT))
			  (T (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
			       "The special token ~S was read in the middle of the list ~:S."
			       THING
			       LIST))))
		   (T
		    (RPLACD END-OF-LIST (SETQ END-OF-LIST (NCONS-IN-AREA THING READ-AREA)))
		    (GO A)))
	RDOT (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     (WHEN (EQ TYPE 'SPECIAL-TOKEN)
	       (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
		       "The special token ~S was read after a dot."
		       THING)
	       (GO RDOT))
	     (WHEN (EQ TYPE 'READER-MACRO)
	       (LET ((XR-LIST-SO-FAR ':AFTER-DOT)
		     (XR-SPLICE-P NIL)
		     VALUES)
		 (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
		 (WHEN XR-SPLICE-P
		   (SETQ LIST XR-LIST-SO-FAR)
		   (GO RDOT))
		 (WHEN (NULL VALUES)
		   (GO RDOT))
		 (SETQ THING (CAR VALUES))))
	     (RPLACD END-OF-LIST THING)
      RDOT-1 (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     (COND ((AND (EQ THING 'CLOSE) (EQ TYPE 'SPECIAL-TOKEN))
		    (AND XR-CORRESPONDENCE-FLAG
			 (RPLACA CORRESPONDENCE-ENTRY LIST)
				(SETF (CADDR CORRESPONDENCE-ENTRY)
				      (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
		    (RETURN LIST 'LIST))
		   ((EQ TYPE 'READER-MACRO)
		    (LET ((XR-LIST-SO-FAR ':AFTER-DOT)
			  (XR-SPLICE-P NIL)
			  VALUES)
		      (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
		      (WHEN (OR XR-SPLICE-P (NULL VALUES))
			(GO RDOT-1)))
		    (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
		      "~S was read instead of a close paren (returned by a reader macro)."
		      THING))
		   (T (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
			"~S was read instead of a close paren."
			THING)))
	     ;; Here only if error condition gets handled.
	     (RPLACD END-OF-LIST (LIST (CDR END-OF-LIST) THING))
	     (SETQ END-OF-LIST (LAST END-OF-LIST))
	     (GO A)))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFPROP POTENTIAL-NUMBER XR-READ-SYMBOL STANDARD-READ-FUNCTION)

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN (SHARP-PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
       LAST-CH ;ignored
       (PROG (THING TYPE PK
	      ;; Help un-screw the user if PACKAGE gets set to NIL.
	      (PACKAGE (OR PACKAGE PKG-USER-PACKAGE))
	      (PKG-NAME (SUBSTRING STRING 0 (1- (LENGTH STRING)))))
	     ;; Try to find the package.
	     (UNLESS
	       ;;don't try to find packages if we're not interning -- eg +slime (dis:foo)
	       (OR *READ-SUPPRESS*
		   (SETQ PK (FIND-PACKAGE PKG-NAME NIL)))
	       ;; Package not found.
	       (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
		       "Package ~S does not exist." PKG-NAME))
	     (UNLESS PK
	       (SETQ PK PKG-USER-PACKAGE))
	     (RETURN-READ-STRING STRING)
	     (LET ((PACKAGE PK)
		   (READ-INTERN-FUNCTION
		     (COND ((EQUAL PKG-NAME "")
			    'READ-UNINTERNED-SYMBOL)
			   ((AND (PACKAGE-AUTO-EXPORT-P PK)
				 (PACKAGE-USED-BY-LIST PK))
			    'READ-INTERN-SOFT)
			   (T 'INTERN))))
	       (MULTIPLE-VALUE (THING TYPE)
		 (READ STREAM)))
	     (RETURN-ARRAY PKG-NAME)
	     (RETURN THING TYPE T)))   ;T means we already did RETURN-READ-STRING.

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN (PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
       LAST-CH ;ignored
       (PROG (THING TYPE PK
	      ;; Help un-screw the user if PACKAGE gets set to NIL.
	      (PACKAGE (OR PACKAGE PKG-USER-PACKAGE))
	      INTERNAL-OK ENTIRE-LIST-PREFIXED)
	     ;; Gobble the second colon, if any, and set flag if found.
	     ;; Note that we do not, currently, DO anything with the flag!
	     (MULTIPLE-VALUE-BIND (CH NUM REAL-CH)
		 (XR-XRTYI STREAM NIL T)
	       (IF (= CH #/:)
		   (SETQ INTERNAL-OK T)
		 (IF (= CH #/()
		     (SETQ ENTIRE-LIST-PREFIXED T))
		 (XR-XRUNTYI STREAM REAL-CH NUM)))
	     ;; Try to find the package.
	     (DO ((STRING1 (OR STRING "")))
		 ;;don't try to find packages if we're not interning -- eg +slime (dis:foo)
		 ((OR *READ-SUPPRESS*
		      (SETQ PK (FIND-PACKAGE STRING1 PACKAGE))))
	       ;; Package not found.
	       (SIGNAL-PROCEED-CASE ((PKG) 'SYS:READ-PACKAGE-NOT-FOUND
					       "Package ~S does not exist."
					       STRING1)
		 (:NO-ACTION
		  (RETURN))
		 (:NEW-PACKAGE
		  (LET ((PACKAGE PKG-USER-PACKAGE))
		    (SETQ STRING1 (STRING (READ-FROM-STRING PKG)))))))
	     (UNLESS PK
	       (SETQ PK PKG-USER-PACKAGE))
	     (WHEN STRING (RETURN-READ-STRING STRING))
	     (LET ((PACKAGE PK)
		   (READ-INTERN-FUNCTION
		     (COND ((AND (PACKAGE-AUTO-EXPORT-P PK)
				 (PACKAGE-USED-BY-LIST PK))
			    'READ-INTERN-SOFT)
			   (T 'INTERN))
		     ;; This change may occur only in Common Lisp.
		     #| (IF (OR ENTIRE-LIST-PREFIXED (EQ PK PACKAGE))
			 ;; Here for, e.g., SI: while in SI already.
			 ;; There are things in LOOP which MUST say "SI:" even though
			 ;; loop is loaded into SI on the Lisp machine.
			 ;; Also here for ZWEI:(BP-LINE (POINT));
			 ;; such constructs are not valid Common Lisp
			 ;; so let's keep their meaning the same.
			 READ-INTERN-FUNCTION
		       (IF READ-COLON-ALLOW-INTERNALS
			   'READ-PACKAGE-PREFIX-INTERN
			 'READ-PACKAGE-PREFIX-EXTERNAL-INTERN)) |#))
	       (MULTIPLE-VALUE (THING TYPE)
		 (READ STREAM)))
	     (RETURN THING TYPE T)))   ;T means we already did RETURN-READ-STRING

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-1 (OPTIONS ITEMS CLIP)         ;CLIP means "common lisp, incompatible"-p
  (let* ((description (defstruct-parse-options options CLIP))
         (type-description (get (defstruct-description-type)
                                'defstruct-type-description))
         (name (defstruct-description-name))
         (DOC (AND (STRINGP (CAR ITEMS)) (POP ITEMS)))
         (new-slots (defstruct-parse-items items description))	;now all slots -- mly
         (returns nil))
    ;;Keep the returns from this as close to last as possible
    ;;Evaluate this before everything else
    (AND (defstruct-type-description-defstruct-expander)
         (setq returns (funcall (defstruct-type-description-defstruct-expander) description)))
    (SETQ RETURNS
          (APPEND RETURNS
                  ;;This must be the last returned form, since to compile it
                  ;;might require that the structure already be operable:
                  (IF (DEFSTRUCT-DESCRIPTION-PRINT)
                      (LIST (DEFSTRUCT-DEFINE-PRINTER NAME (DEFSTRUCT-DESCRIPTION-PRINT))))
                  ;;Return the name symbol as our value
                  `(',NAME)))
 #+(and LispM (NOT MIT))
    (push `(record-source-file-name ',name 'defstruct) returns)
 #+(and LispM MIT)
    (push `(eval-when (load eval) (record-source-file-name ',name 'defstruct)) returns)
 #+(AND LISPM MIT)				;not really just mit lispm, but any clisp
    (AND DOC (PUSH `(SETF (DOCUMENTATION ',NAME 'STRUCTURE) ,DOC) RETURNS))
    (let ((alterant (defstruct-description-alterant))
          (size-macro (defstruct-description-size-macro))
          (size-symbol (defstruct-description-size-symbol))
          (predicate (defstruct-description-predicate))
          (copier (defstruct-description-copier)))
      (cond (predicate
             (push (funcall (or (defstruct-type-description-predicate)
                                (defstruct-error
                                  "This DEFSTRUCT type cannot produce a predicate"
                                  (defstruct-description-type) 'in name))
                            description
                            predicate)
                   returns)))
      (cond (copier
             (push
               (let ((copy-fun (defstruct-type-description-copier)))
                 (cond (copy-fun
                        (funcall copy-fun description copier))
                       ((not (= 1 (defstruct-type-description-ref-no-args)))
                        (defstruct-error
                          "This defstruct type cannot produce a copying function"
                          (defstruct-description-type) 'in name))
                       (t (do ((i (1- (defstruct-description-size)) (1- i))
                               (l nil (cons (cons i
                                                  (funcall
                                                    (defstruct-type-description-ref-expander)
                                                    i description 'x))
                                            l)))
                              ((< i 0)
                               `(defun ,copier (x)
                                  ,(invoke-defstruct-constructor-expander
                                     description type-description l nil)))))))
               returns)))
      (cond (alterant
             (defstruct-put-macro alterant 'defstruct-expand-alter-macro)
             (defstruct-putprop-compile-time alterant name 'defstruct-name)))
      (cond (size-macro
             (defstruct-put-macro size-macro 'defstruct-expand-size-macro)
             (defstruct-putprop-compile-time size-macro name 'defstruct-name)))
      (cond (size-symbol
             (push `(defconst ,size-symbol
                      ,(+ (defstruct-description-size)
                          (defstruct-type-description-overhead)))
                   returns))))
    (defstruct-putprop-compile-time name description 'defstruct-description)
    ;;what defstruct returns
    #-(OR LISPM NIL)			;retain eval-when so as not to cause hidden screws
    `(eval-when ,(defstruct-description-eval-when)
       ,.(defstruct-define-ref-macros new-slots description)
       ,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,.returns)
    #+(OR LISPM NIL)			;losing eval-when flushed!! 
    `(PROGN
       ,.(defstruct-define-ref-macros new-slots description)
       ,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,.returns)))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-define-ref-macros (new-slots description)
  (let ((name (defstruct-description-name))
	(returns nil))
    (if (not (defstruct-description-callable-accessors))
	(do ((l new-slots (cdr l))
	     (mname))
	    ((null l))
	  (setq mname (defstruct-slot-description-ref-macro-name (cdar l)))
	  (defstruct-put-macro mname 'defstruct-expand-ref-macro)
	  (defstruct-putprop-compile-time mname (cons name (caar l)) 'defstruct-slot))
      (let* ((type-description
	       (get (defstruct-description-type)
		    'defstruct-type-description))
	     (code (defstruct-type-description-ref-expander))
	     (n (defstruct-type-description-ref-no-args))
	     #+LispM
	     (parent `(,name defstruct))
	     (but-first (defstruct-description-but-first))
	     (default-pointer (defstruct-description-default-pointer)))
	(do ((args nil (cons (gensym) args))
	       (i n (1- i)))
	      ((< i 2)
	       ;;Last arg (if it exists) is name of structure,
	       ;; for documentation purposes.
	       (and (= i 1)
		    (setq args (cons name args)))
	       (let ((body (cons (if but-first
				     `(,but-first ,(car args))
				   (car args))
				 (cdr args))))
		 (and default-pointer
		      (setq args `((,(car args) ,default-pointer)
				   &optional ,@(cdr args))))
		 (setq args (reverse args))
		 (setq body (reverse body))
		 (do ((l new-slots (cdr l))
		      (mname))
		     ((null l))
		   (setq mname (defstruct-slot-description-ref-macro-name
				 (cdar l)))
		   (IF (DEFSTRUCT-SLOT-DESCRIPTION-READ-ONLY (CDAR L))
		       (DEFSTRUCT-PUTPROP-COMPILE-TIME MNAME
						       '(MACRO . UNSETFABLE) 'SETF-METHOD))
		   #+MacLisp 
		   ;;This must come BEFORE the defun. THINK!
		   (defstruct-put-macro mname 'defstruct-expand-ref-macro)
		   (let ((ref (lexpr-funcall
				code
				(defstruct-slot-description-number (cdar l))
				description
				body))
			 (ppss (defstruct-slot-description-ppss (cdar l)))
			 (DOC (DEFSTRUCT-SLOT-DESCRIPTION-DOCUMENTATION (CDAR L))))
		     (push `(#+LispM		defsubst-with-parent
			     #+NIL		defsubst
			     #-(or LispM NIL)	defun
			     ,mname #+LispM ,parent ,args
			     ,DOC
			     ,(if (null ppss) ref `(ldb ,ppss ,ref)))
			   returns))
		   (defstruct-putprop mname
				      (cons name (caar l))
		     'defstruct-slot)))))))
    returns))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-function-parent (sym)
  (values (or (getdecl sym 'defstruct-name)
	      (car (getdecl sym 'defstruct-slot)))
	  'defstruct))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-expand-size-macro (x)
  (let ((description (get-defstruct-description (getdecl (car x) 'defstruct-name))))
    (let ((type-description (or (get (defstruct-description-type)
				     'defstruct-type-description)
				(defstruct-error
				  "Unknown defstruct type"
				  (defstruct-description-type)))))
      (+ (defstruct-description-size)
	 (defstruct-type-description-overhead)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-expand-ref-macro (x)
  (let* ((pair (getdecl (car x) 'defstruct-slot))
	 (description (get-defstruct-description (car pair)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (code (defstruct-type-description-ref-expander))
	 (n (defstruct-type-description-ref-no-args))
	 (args (reverse (cdr x)))
	 (nargs (length args))
	 (default (defstruct-description-default-pointer))
	 (but-first (defstruct-description-but-first)))
    (cond ((= n nargs)
	   (and but-first
		(rplaca args `(,but-first ,(car args)))))
	  ((and (= n (1+ nargs)) default)
	   (setq args (cons (if but-first
				`(,but-first ,default)
				default)
			    args)))
	  (t
	   (defstruct-error
	     "Wrong number of args to an accessor macro" x)))
    (let* ((slot-description 
	     (cdr (or (assq (cdr pair)
			    (defstruct-description-slot-alist))
		      (defstruct-error
			"This slot no longer exists in this structure"
			(cdr pair) 'in (car pair)))))
	    (ref (lexpr-funcall
		   code
		   (defstruct-slot-description-number)
		   description
		   (nreverse args)))
	    (ppss (defstruct-slot-description-ppss)))
      (if (null ppss)
	  ref
	  `(ldb ,ppss ,ref)))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-expand-cons-macro (x)
  (let* ((description (get-defstruct-description (getdecl (car x) 'defstruct-name)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (slot-alist (defstruct-description-slot-alist))
	 (cons-keywords (defstruct-type-description-cons-keywords))
	 (kludge nil)
	 (constructor-description 
	   (cdr (or (assq (car x) (defstruct-description-constructors))
		    (defstruct-error
		      "This constructor is no longer defined for this structure"
		      (car x) 'in (defstruct-description-name)))))
	 (aux nil)
	 (aux-init nil))
     (if (null constructor-description)
	 (setq kludge (defstruct-parse-setq-style-slots (cdr x)
							slot-alist
							cons-keywords
							x))
       (prog (args l)
	     (setq kludge (cons nil nil))
	     (setq args (cdr x))
	     (setq l (car constructor-description))
	  R  (cond ((null l)
		    (if (null args)
			(return nil)
		      (go barf-tma)))
		   ((atom l) (go barf))
		   ((eq (car l) '&optional) (go O))
		   ((eq (car l) '&rest) (go S))
		   ((eq (car l) '&aux) (go A))
		   ((null args) (go barf-tfa)))
	     (defstruct-make-init-dsc kludge
				      (pop l)
	                              (pop args)
				      slot-alist
                                      cons-keywords
                                      x)
             (go R)
          O  (and (null args) (go OD))
             (pop l)
             (cond ((null l) (go barf-tma))
                   ((atom l) (go barf))
                   ((eq (car l) '&optional) (go barf))
                   ((eq (car l) '&rest) (go S))
                   ((eq (car l) '&aux) (go barf-tma)))
             (defstruct-make-init-dsc kludge
                                      (if (atom (car l)) (car l) (caar l))
                                      (pop args)
                                      slot-alist
                                      cons-keywords
                                      x)
             (go O)
          OD (pop l)
             (cond ((null l) (return nil))
                   ((atom l) (go barf))
                   ((eq (car l) '&optional) (go barf))
                   ((eq (car l) '&rest) (go S))
                   ((eq (car l) '&aux) (go A)))
             (or (atom (car l))
                 (defstruct-make-init-dsc kludge
                                          (caar l)
                                          (cadar l)
                                          slot-alist
                                          cons-keywords
                                          x))
             (go OD)
          S  (and (atom (cdr l)) (go barf))
             (defstruct-make-init-dsc kludge
                                      (cadr l)
                                      `(list ,@args)
                                      slot-alist
                                      cons-keywords
                                      x)
             (setq l (cddr l))
             (and (null l) (return nil))
	     (and (atom l) (go barf))
	     (or (eq (car l) '&aux) (go barf))
	  A  (pop l)
	     (cond ((null l) (return nil))
		   ((atom l) (go barf))
		   ((atom (car l))
		    (push (car l) aux)
		    (push (make-empty) aux-init))
		   (t
		    (push (caar l) aux)
		    (push (cadar l) aux-init)))
	     (go A)
	  BARF (defstruct-error
		 "Bad format for defstruct constructor arglist"
		 `(,(car x) ,@(car constructor-description)))
	  BARF-TFA (defstruct-error "Too few arguments to constructor macro" x)
	  BARF-TMA (defstruct-error "Too many arguments to constructor macro" x)))
     (do ((l slot-alist (cdr l)))
	 ((null l))
       (let* ((name (caar l))
	      (slot-description (cdar l))
	      (code (do ((aux aux (cdr aux))
			 (aux-init aux-init (cdr aux-init)))
			((null aux) (defstruct-slot-description-init-code))
		      (and (eq name (car aux)) (return (car aux-init)))))
	      (ppss (defstruct-slot-description-ppss)))
	 (or (and (emptyp code) (null ppss))
	     (let* ((number (defstruct-slot-description-number))
		    (dsc (assoc number (car kludge))))
	       (cond ((null dsc)
		      (setq dsc (list* number nil (make-empty) 0 0 nil))
		      (push dsc (car kludge))))
	       (cond ((emptyp code))
		     ((eq t (cadr dsc)))
		     ((null ppss)
		      (and (emptyp (car (cddr dsc)))
			   (setf (car (cddr dsc)) code)))
		     ((memq name (cadr dsc)))
		     ((and (numberp ppss) (numberp code))
		      (setf (ldb ppss (cadr (cddr dsc))) -1)
		      (setf (ldb ppss (caddr (cddr dsc))) code))
		     (t
		      (push (cons ppss code) (cdddr (cddr dsc)))))))))
     (do ((l (car kludge) (cdr l)))
	 ((null l))
       (rplacd (car l) (defstruct-code-from-dsc (car l))))
     (invoke-defstruct-constructor-expander
       description type-description
       (car kludge) (cdr kludge))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFUN DEFSTRUCT-DEFINE-CONSTRUCTORS (DESCRIPTION)
  (LET ((NAME (DEFSTRUCT-DESCRIPTION-NAME))
	RETURNS)
    (IF (NOT (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS))
	(DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
	  (DEFSTRUCT-PUT-MACRO (CAR CS) 'DEFSTRUCT-EXPAND-CONS-MACRO)
	  (DEFSTRUCT-PUTPROP-COMPILE-TIME (CAR CS) NAME 'DEFSTRUCT-NAME))
      (LET* ((SLOT-ALIST (DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
	     (SIZE (DEFSTRUCT-DESCRIPTION-SIZE))
	     (TYPE-DESCRIPTION (GET (DEFSTRUCT-DESCRIPTION-TYPE) 'DEFSTRUCT-TYPE-DESCRIPTION))
	     (CONS-KEYWORDS (DEFSTRUCT-TYPE-DESCRIPTION-CONS-KEYWORDS))
	     (FL (DEFSTRUCT-TYPE-DESCRIPTION-CONS-FLAVOR))
	     MNAME BODY SYM ARGLIST ARGS REST FROB FROBPPSS X S INIT INIT-LIST BOAP
	     OPT OPTPPSS OPT-SLOT OPTPPSS-SLOT FLAGS PPSS-FLAGS
	     NOPT NOPTPPSS NOPT-SLOT NOPTPPSS-SLOT
	     (F (GENSYM)) (L (GENSYM)) (R (GENSYM)) (D (GENSYM)) (Y (GENSYM))
	     (SL (GENSYM)) (TEM (GENSYM))
	     CW CWN
	     (CONS-WORDS (DO ((V CONS-KEYWORDS (CDR V))
			      R W)
			     ((NULL V) R)
			   ;;will this win everywhere?
			   (SETQ W (INTERN (GET-PNAME (CAR V))))	;in *package*
			   (PUSH W CW)
			   (PUSH (LIST 'QUOTE (CAR V)) CW)
			   (PUSH (GENSYM) CW)
			   (PUSH (LIST W NIL (CAR CW)) R)
			   (PUSH W CWN)))
	     )
	(DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
	  (SETQ MNAME (CAR CS) BOAP T
		ARGLIST () ARGS () REST NIL
		OPT () OPT-SLOT () FLAGS () OPTPPSS () OPTPPSS-SLOT () PPSS-FLAGS ()
		NOPT () NOPT-SLOT () NOPTPPSS () NOPTPPSS-SLOT ())
	  (IF (CDR CS)
	      ;;it's a boa-constructor!
	      (IF (CDDR CS) (DEFSTRUCT-ERROR
			      "DEFSTRUCT constructors can only be specified by arglist"
			      CS)
		(DO ((AL (CADR CS)))
		    (NIL)
		 REQUIRED
		  (SELECTQ (SETQ X (POP AL))
		    (&OPTIONAL (GO OPTIONAL))
		    (&REST (GO REST))
		    (&AUX (GO AUX))
		    (NIL (RETURN))
		    (T (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (PUSH X ARGS)
		       (PUSH X ARGLIST)
		       (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			   (PROGN (PUSH X NOPTPPSS)
				  (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					      (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					NOPTPPSS-SLOT))
			 (PUSH X NOPT)
			 (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			       NOPT-SLOT))
		       (GO REQUIRED)))
		 OPTIONAL
		  (PUSH '&OPTIONAL ARGS)
		  (PUSH '&OPTIONAL ARGLIST)
		 OPT
		  (SELECTQ (SETQ X (POP AL))
		    (&OPTIONAL (GO OPT))
		    (&REST (GO REST))
		    (&AUX (GO AUX))
		    (NIL (RETURN))
		    (T (PUSH X ARGLIST)		     
		       (IF (CONSP X)
			   (IF (CDDR X) (GO ARG-ERROR)
			     (PSETQ X (CAR X) INIT (CADR X)))
			 (SETQ INIT (MAKE-EMPTY)))
		       (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (IF (EMPTYP INIT) (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
		       (IF (EMPTYP INIT)
			   (PROGN
			     (SETQ SYM (GENSYM))
			     (PUSH (LIST X NIL SYM) ARGS)
			     (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
				 (PROGN (PUSH SYM PPSS-FLAGS)
					(PUSH X OPTPPSS)
					(PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						    (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					      OPTPPSS-SLOT))
			       (PUSH SYM FLAGS)
			       (PUSH X OPT)
			       (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				     OPT-SLOT)))
			 (PUSH (LIST X INIT) ARGS)
			 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			     (PROGN (PUSH X NOPTPPSS)
				    (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						(DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					  NOPTPPSS-SLOT))
			   (PUSH X NOPT)
			   (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				 NOPT-SLOT)))
		       (GO OPT)))
		 REST
		  (PUSH '&REST ARGS)
		  (PUSH '&REST ARGLIST)
		  (SELECTQ (SETQ X (POP AL))
		    ((&OPTIONAL &REST &AUX NIL) (GO ARG-ERROR))
		    (T (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (SETQ REST X)
		       (PUSH X ARGS)
		       (PUSH X ARGLIST)
		       (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			   (PROGN (PUSH X NOPTPPSS)
				  (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					      (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					NOPTPPSS-SLOT))
			 (PUSH X NOPT)
			 (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			       NOPT-SLOT))
		       (SELECTQ (SETQ X (POP AL))
			 (&AUX (GO AUX))
			 (NIL (RETURN))
			 (T (GO ARG-ERROR)))))
		 AUX
		  (PUSH '&AUX ARGLIST)
		  (PUSH '&AUX ARGS)
		 OX
		  (SELECTQ (SETQ X (POP AL))
		    ((&OPTIONAL &REST &AUX) (GO ARG-ERROR))
		    (NIL (RETURN))
		    (T (PUSH X ARGLIST)
		       (IF (CONSP X)
			   (IF (CDDR X) (GO ARG-ERROR)
			     (PSETQ X (CAR X) INIT (CADR X)))
			 (SETQ INIT (MAKE-EMPTY)))
		       (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (IF (EMPTYP INIT) (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
		       (IF (EMPTYP INIT) NIL
			 (PUSH (LIST X INIT) ARGS)
			 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			     (PROGN (PUSH X OPTPPSS)
				    (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
						(DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
					  NOPTPPSS-SLOT))
			   (PUSH X NOPT)
			   (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				 NOPT-SLOT)))
		       (GO OX)))
		 ARG-ERROR
		  (DEFSTRUCT-ERROR "Bad defstruct :CONSTRUCTOR argument list" AL 'FOR MNAME)
		 SLOT-ERROR
		  (DEFSTRUCT-ERROR "Invalid defstruct slot-name" X 'WHILE 'DEFINING MNAME)))
	    ;;do this for non-boa-constructors
	    (SETQ BOAP NIL)
	    (PUSH '&KEY ARGLIST)
	    (PUSH '&KEY ARGS)
	    (DOLIST (S SLOT-ALIST)
	      (SETQ X (CAR S) S (CDR S)		;standardize our nomenclature
		    INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S))
	      (IF (EMPTYP INIT)
		  (PROGN
		    (PUSH X ARGLIST)
		    (SETQ SYM (GENSYM))
		    (PUSH (LIST X NIL SYM) ARGS)
		    (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
			(PROGN (PUSH SYM PPSS-FLAGS)
			       (PUSH X OPTPPSS)
			       (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
					   (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				     OPTPPSS-SLOT))
		      (PUSH SYM FLAGS)
		      (PUSH X OPT)
		      (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			    OPT-SLOT)))
		(PUSH (LIST X INIT) ARGS)
		(PUSH (CAR ARGS) ARGLIST)
		(IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S)
		    (PROGN (PUSH X NOPTPPSS)
			   (PUSH (CONS (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
				       (DEFSTRUCT-SLOT-DESCRIPTION-PPSS S))
				 NOPTPPSS-SLOT))
		  (PUSH X NOPT)
		  (PUSH (DEFSTRUCT-SLOT-DESCRIPTION-NUMBER S)
			NOPT-SLOT))))
	    (WHEN CONS-KEYWORDS
	      (SETQ ARGLIST (NCONC (REVERSE CONS-KEYWORDS) (LIST* '&OPTIONAL NIL) ARGLIST))
	      (SETQ ARGS (NCONC (COPYLIST* CONS-WORDS) ARGS))))
	  ;;crunch the args now that we've snarfed them
	  (SETQ ARGLIST (NREVERSE ARGLIST) ARGS (NREVERSE ARGS))
          (SELECTQ FL
	      (:LIST
	       (SETQ INIT-LIST (MAKE-LIST SIZE))
	       (DO ((X INIT-LIST (CDR X))) ((NULL X))
		 (SETF (CAR X) (LIST 'QUOTE NIL)))
	       (DOLIST (X SLOT-ALIST)		;put zero inits where appropriate
		 (IF (DEFSTRUCT-SLOT-DESCRIPTION-PPSS (CDR X)) (SETF (CADR (NTH (CAR X) INIT-LIST)) 0)))
	       (SETQ FROB `((SETF (CADR (NTH (CAR ,SL) ,L))) (CAR ,Y))))
	       (SETQ FROBPPSS `((SETF (LDB (CDAR ,SL) (CADR (NTH (CAAR ,SL) ,L))) (CAR ,Y))))
	      (:ALIST
	       (SETQ INIT-LIST ())
	       (SETQ FROB `((IF (SETQ ,TEM (ASSOC (CAR ,SL) ,L))
				(SETF (CADDR ,TEM) (CAR ,Y))
			      (PUSH (CONS (CAR ,SL) (LIST 'QUOTE (CAR ,Y))) ,L))))
	       (SETQ FROBPPSS `((IF (SETQ ,TEM (ASSOC (CAAR ,SL) ,L))
				    (SETF (LDB (CDAR ,SL) (CADR ,TEM)) (CAR ,Y))
				  (PUSH (CONS (CAAR ,SL) (DPB (CAR ,Y) (CDAR ,SL) 0)) ,L)))))
	      (T (DEFSTRUCT-ERROR
		   "Unknown constructor kind"
		   FL 'IN TYPE-DESCRIPTION)))
	    (SETQ BODY
		  (NCONC (IF INIT-LIST
			     `((SETQ ,L ,INIT-LIST)))
			 #+LISPM				;needed elsewhere??
			 (IF REST
			     `((SETQ ,REST (COPYLIST ,REST))))	;can't trust stack lists
			 (IF (AND (NOT BOAP) CONS-KEYWORDS)
			     `((DO ((,F (LIST ,@CW) (CDDDR ,F)))
				   ((NULL ,F))
				 (WHEN (CAR ,F)
				   (PUSH (CONS (CADR ,F) (CADDR ,F)) ,R)))
			       ,@CWN))				;prevent compiler barfage
			 (IF OPT
			     `((DO ((,F (LIST ,@FLAGS) (CDR ,F))
				    (,SL ',OPT-SLOT (CDR ,SL))
				    (,Y (LIST ,@OPT) (CDR ,Y)))
				   ((NULL ,Y))
				 (WHEN (CAR ,F) ,@FROB))))
			 (IF OPTPPSS
			     `((DO ((,F (LIST ,@PPSS-FLAGS) (CDR ,F))
				    (,SL ',OPTPPSS-SLOT (CDR ,SL))
				    (,Y (LIST ,@OPTPPSS) (CDR ,Y)))
				   ((NULL ,Y))
				 (WHEN (CAR ,F) ,@FROBPPSS))))
			 (IF NOPT
			     `((DO ((,SL ',NOPT-SLOT (CDR ,SL))
				    (,Y (LIST ,@NOPT) (CDR ,Y)))
				   ((NULL ,Y))
				 ,@FROB)))
			 (IF NOPTPPSS
			     `((DO ((,SL ',NOPTPPSS-SLOT (CDR ,SL))
				    (,Y (LIST ,@NOPTPPSS) (CDR ,Y)))
				   ((NULL ,Y))
				 ,@FROBPPSS)))))
	    (DEFSTRUCT-PUTPROP MNAME NAME 'DEFSTRUCT-NAME)
	    (PUSH 
	      `(DEFUN ,MNAME ,ARGS
		 (DECLARE (ARGLIST ,ARGLIST))
		 (LET ((,D (GET-DEFSTRUCT-DESCRIPTION ',NAME))
		       ,L ,TEM ,R)
		   ,@BODY
		   (EVAL 
		     (FUNCALL (DEFSTRUCT-TYPE-DESCRIPTION-CONS-EXPANDER
				(GET (DEFSTRUCT-DESCRIPTION-TYPE ,D) 'DEFSTRUCT-TYPE-DESCRIPTION))
			      ,L
			      ,D
			      ,R))))
	      RETURNS))))
    RETURNS))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-expand-alter-macro (x)
  (let* ((description (get-defstruct-description (getdecl (car x) 'defstruct-name)))
	 (type-description (or (get (defstruct-description-type)
				    'defstruct-type-description)
			       (defstruct-error
				 "Unknown defstruct type"
				 (defstruct-description-type))))
	 (ref-code (defstruct-type-description-ref-expander))
	 (ref-nargs (defstruct-type-description-ref-no-args)))
    (do ((l (car (defstruct-parse-setq-style-slots 
		   (nthcdr (1+ ref-nargs) x)
		   (defstruct-description-slot-alist)
		   nil
		   x))
	    (cdr l))
	 (but-first (defstruct-description-but-first))
	 (body nil)
	 (avars (do ((i 0 (1+ i))
		     (l nil (cons (gensym) l)))
		    ((= i ref-nargs) l)))
	 (vars nil)
	 (vals nil))
	((null l)
	 `((lambda ,avars
	     ,@(if (null vars)
		   body
		   `(((lambda ,vars ,@body) ,.vals))))
	   ,@(do ((i (1- ref-nargs) (1- i))
		  (l `(,(if but-first
			    `(,but-first ,(nth ref-nargs x))
			    (nth ref-nargs x)))
		     (cons (nth i x) l)))
		 ((= i 0) l))))
      (let ((ref (lexpr-funcall ref-code (caar l) description avars)))
	(and (emptyp (car (cddr (car l))))
	     (setf (car (cddr (car l))) ref))
	(let ((code (defstruct-code-from-dsc (car l))))
	  (if (null (cdr l))
	      (push `(setf ,ref ,code) body)
	      (let ((sym (gensym)))
		(push `(setf ,ref ,sym) body)
		(push sym vars)
		(push code vals))))))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN FILE-WRITE-DATE (FILENAME-OR-STREAM)
  "Return file's creation or last write date.  Specify pathname, namestring or file stream."
  (IF (OR (STRINGP FILENAME-OR-STREAM)
	  (SYMBOLP FILENAME-OR-STREAM)
	  (TYPEP FILENAME-OR-STREAM 'PATHNAME))
      (WITH-OPEN-FILE (STREAM FILENAME-OR-STREAM ':DIRECTION NIL)
	(SEND STREAM ':CREATION-DATE))
    (SEND FILENAME-OR-STREAM ':CREATION-DATE)))

))

; From file PATED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFCOM COM-ADD-PATCH "Add the current defun or the region (if any) to the patch buffer.
If there is no patch buffer, ask the user for the system to patch. Then reserve a new
version number, and create a buffer whose pathname is the source file name for the
patch creating that version number.  If there is a region, append it to the end of the
patch buffer; otherwise append the current defun to the end of the patch buffer." ()
  (LET (BP1 BP2 DEFUN-NAME)
    (COND ((WINDOW-MARK-P *WINDOW*)
	   ;; there is a region, use it.
	   (SETQ BP1 (MARK) BP2 (POINT))
	   (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	   (SETQ DEFUN-NAME "the region"))
	  ((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL T))
	   ;; No region, try to get containing defun.
	   (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	   (SETQ DEFUN-NAME (SECTION-NODE-NAME (LINE-NODE (LINE-NEXT (BP-LINE BP1))))))
	  (T
	   (BARF "Unbalanced parentheses or no defuns.")))
    (ADD-PATCH-INTERVAL BP1 BP2 T DEFUN-NAME *INTERVAL*))
  DIS-MARK-GOES)

))

; From file FILES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFCOM COM-COMPILE-AND-LOAD-FILE "Load a file, after compiling it if necessary." ()
  (LET* ((PATHNAME (READ-DEFAULTED-PATHNAME "Compile and load file:"
					    (FUNCALL (FS:DEFAULT-PATHNAME
							 (PATHNAME-DEFAULTS))
						       ':NEW-PATHNAME ':TYPE NIL
						       ':VERSION NIL)
					    ;; If user omits type, don't default it.
					    NIL NIL ':READ T NIL))
	 (SOURCE-PATHNAME (SEND PATHNAME ':SOURCE-PATHNAME))
	 (BUFFER (FIND-FILE-BUFFER SOURCE-PATHNAME)))
    (EDITOR-COMPILE-FILE SOURCE-PATHNAME)
    (FORMAT QUERY-IO "~&~A loaded."
	    (LOAD PATHNAME (IF BUFFER (BUFFER-PACKAGE BUFFER)) NIL NIL T)))
  DIS-NONE)

))

; From file RDDEFS.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; RDDEFS  "

(DEFSTRUCT (READTABLE :ARRAY-LEADER :NAMED
		      (:CONSTRUCTOR MAKE-RDTBL)
		      (:MAKE-ARRAY (:DIMENSIONS (RDTBL-ARRAY-DIMS) :TYPE 'ART-16B))
		      (:DEFAULT-POINTER RDTBL)
		      (:SIZE-MACRO RDTBL-SIZE))
	   RDTBL-FSM					;sacred
	   RDTBL-N-STATES
	   RDTBL-N-BUCKETS
	   RDTBL-STARTING-STATE
	   RDTBL-SLASH-CODE
	   RDTBL-EOF-CODE
	   RDTBL-BREAK-CODE
	   RDTBL-MACRO-ALIST
	   RDTBL-READ-FUNCTION-PROPERTY
	   RDTBL-PLIST
           RDTBL-DEFINITION
	   RDTBL-MAKE-SYMBOL
	   RDTBL-MAKE-SYMBOL-BUT-LAST
           RDTBL-SLASH   ;; Not used.
           RDTBL-WHITESPACE    ;; Not used.
           RDTBL-CIRCLECROSS   ;; Not used.
	   (PTTBL-SPACE			40	)
	   (PTTBL-NEWLINE		215	)
	   (PTTBL-CONS-DOT 		" . "	)
	   (PTTBL-MINUS-SIGN 		#/-	)
	   (PTTBL-DECIMAL-POINT 	#/.	)
	   (PTTBL-SLASH 		#//	)
	   (PTTBL-PRINLEVEL 		"**"	)
	   (PTTBL-PRINLENGTH 		"..."	)
	   (PTTBL-OPEN-RANDOM 		"#<"	)
	   (PTTBL-CLOSE-RANDOM 		">"	)
	   (PTTBL-OPEN-PAREN 		#/(	)
	   (PTTBL-CLOSE-PAREN 		#/)	)
	   (PTTBL-OPEN-QUOTE-STRING 	#/"	)
	   (PTTBL-CLOSE-QUOTE-STRING	#/"	)
	   (PTTBL-OPEN-QUOTE-SYMBOL 	#/|	)
	   (PTTBL-CLOSE-QUOTE-SYMBOL 	#/|	)
	   (PTTBL-PACKAGE-PREFIX        ":"	)
	   (PTTBL-PACKAGE-INTERNAL-PREFIX ":"	)
	   (PTTBL-CHARACTER-PREFIX      "//"   )
	   (PTTBL-CHARACTER-BEFORE-FONT "#"     )
	   (PTTBL-RATIONAL-INFIX        #/\     )
	   (PTTBL-COMPLEX		'("" NIL "i"))
	   (PTTBL-RATIONAL-RADIX	10.	)
	   (PTTBL-OPEN-VECTOR		"#("	)
	   (PTTBL-CLOSE-VECTOR          ")"	)
	   (PTTBL-ARRAY			'("#" :RANK "A" :SEQUENCES))
	   (PTTBL-OPEN-BIT-VECTOR	"#*"	)
	   (PTTBL-UNINTERNED-SYMBOL-PREFIX "#:" )
	   (PTTBL-STRUCTURE		NIL     )  ;Don't know what this should mean yet.
	   RDTBL-ESCAPE-CODE
	   RDTBL-MULTIPLE-ESCAPE-CODE
	   RDTBL-CHARACTER-CODE-ESCAPE-CODE
	   )

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-PNAME-STRING (SYMBOL STREAM FASTP &OPTIONAL NO-PACKAGE-PREFIXES
			   &AUX STRING LEN FSMWINS MUST// TEM)
    (DECLARE (SPECIAL XP-STREAM XP-FASTP XR-EXTENDED-IBASE-P))
    ;; Print a package prefix if appropriate.
    (WHEN (AND *PRINT-ESCAPE* (NOT NO-PACKAGE-PREFIXES)
	       (SYMBOLP SYMBOL))
      (IF (NULL (SYMBOL-PACKAGE SYMBOL))
	  (AND *PRINT-GENSYM*
	       (SEND STREAM ':STRING-OUT
		     (PTTBL-UNINTERNED-SYMBOL-PREFIX READTABLE)))
	(MULTIPLE-VALUE-BIND (PKG-OR-STRING INTERNAL-FLAG)
	    (PKG-PRINTING-PREFIX SYMBOL PACKAGE)
	  (WHEN PKG-OR-STRING
	    (UNLESS (EQUAL PKG-OR-STRING "")
	      (PRINT-PNAME-STRING (IF (STRINGP PKG-OR-STRING) PKG-OR-STRING
				    (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING))
				  STREAM FASTP))
	    (SEND STREAM ':STRING-OUT
		  (IF (AND (NOT (STRINGP PKG-OR-STRING))
			   PACKAGE
			   (ASSOC (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING)
				  (DONT-OPTIMIZE (PKG-REFNAME-ALIST PACKAGE))))
		      ;; Use #: to inhibit an interfering local nickname.
		      "#:"
		    (IF INTERNAL-FLAG
			(PTTBL-PACKAGE-INTERNAL-PREFIX READTABLE)
		      (PTTBL-PACKAGE-PREFIX READTABLE))))))))
    (SETQ STRING (STRING SYMBOL))
    (COND ((NOT *PRINT-ESCAPE*)
	   (PRINT-RAW-STRING STRING STREAM FASTP))
	  (T
	   (SETQ FSMWINS
	    (AND (PLUSP (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING)))
		 (DO ((I 0 (1+ I))
		      (STATE (RDTBL-STARTING-STATE READTABLE))
		      (FSM (RDTBL-FSM READTABLE))
		      (CHAR)
		      (ESCAPE-CODE (RDTBL-ESCAPE-CODE READTABLE))
		      (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE READTABLE))
		      (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE READTABLE)))
		     ((= I LEN)
		      (COND ((NOT (NUMBERP STATE))
			     (DO L (RDTBL-MAKE-SYMBOL READTABLE) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    ((NOT (NUMBERP (SETQ STATE
						 (AR-2 FSM
						       STATE
						       (RDTBL-BREAK-CODE READTABLE)))))
			     (DO L (RDTBL-MAKE-SYMBOL-BUT-LAST READTABLE) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    (T NIL)))
		     (SETQ CHAR (AR-1 STRING I))
		     (COND ((OR (NOT (NUMBERP STATE))	;FSM ran out OR
				(NOT			;Translated char? then fsm loses
				 (= CHAR (RDTBL-TRANS READTABLE CHAR))))
			    (OR MUST//				   ;Must we slash?
				(DO ((I I (1+ I))) ((= I LEN))
				  (LET ((CODE (RDTBL-CODE READTABLE (AR-1 STRING I))))
				    (WHEN (OR (= CODE ESCAPE-CODE)
					      (= CODE MULTIPLE-ESCAPE-CODE)
					      (= CODE CHARACTER-CODE-ESCAPE-CODE))
				      (SETQ MUST// T)
				      (RETURN NIL)))))
			    (RETURN NIL)))
		     (SETQ STATE
			   (AR-2 FSM
				 STATE
				 (COND ((LET ((CODE (RDTBL-CODE READTABLE (AR-1 STRING I))))
					  (OR (= CODE ESCAPE-CODE)
					      (= CODE MULTIPLE-ESCAPE-CODE)
					      (= CODE CHARACTER-CODE-ESCAPE-CODE)))
					(SETQ MUST// T)	;YES: set flag.
					(RDTBL-SLASH-CODE READTABLE))
				       ((AND (NUMBERP BASE) (> BASE 10.)
					     ( #/A CHAR (+ BASE #/A -11.)))
					(CDR (GETF (RDTBL-PLIST READTABLE) 'EXTENDED-DIGIT)))
				       (T
					(RDTBL-CODE READTABLE CHAR))))))))
	   (OR FSMWINS
	       (FUNCALL STREAM ':TYO (PTTBL-OPEN-QUOTE-SYMBOL READTABLE)))
	   (COND ((OR MUST//
		      (AND FSMWINS (NEQ *PRINT-CASE* ':UPCASE)))
		  (DO ((I 0 (1+ I))
		       (ESCAPE-CODE (RDTBL-ESCAPE-CODE READTABLE))
		       (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE READTABLE))
		       (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE READTABLE))
		       (PREV-CHAR 0)
		       CODE)
		      ((= I LEN))
		    (SETQ TEM (AR-1 STRING I))
		    (SETQ CODE (RDTBL-CODE READTABLE TEM))
		    (COND ((OR (= CODE ESCAPE-CODE)
			       (= CODE MULTIPLE-ESCAPE-CODE)
			       (= CODE CHARACTER-CODE-ESCAPE-CODE))
			   (FUNCALL STREAM ':TYO (PTTBL-SLASH READTABLE))
			   (FUNCALL STREAM ':TYO TEM))
			  ((OR (EQ *PRINT-CASE* ':DOWNCASE)
			       (AND (EQ *PRINT-CASE* ':CAPITALIZE)
				    (ALPHANUMERICP PREV-CHAR)))
			   (FUNCALL STREAM ':TYO (CHAR-DOWNCASE TEM)))
			  (T
			   (FUNCALL STREAM ':TYO TEM)))
		    (SETQ PREV-CHAR TEM)))
		 (T (PRINT-RAW-STRING STRING STREAM FASTP)))
	   (OR FSMWINS
	       (FUNCALL STREAM ':TYO (PTTBL-CLOSE-QUOTE-SYMBOL READTABLE)))
	   )))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFVAR READ-INSIDE-MULTIPLE-ESCAPE NIL
  "T if inside vertical bars while reading a token.")

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFUN XR-XRTYI (STREAM &OPTIONAL IGNORE-WHITESPACE NO-CHARS-SPECIAL NO-MULTIPLE-ESCAPES)
  "Read a character from STREAM, processing escapes (// and /) and multiple-escapes (/|).
IGNORE-WHITESPACE non-NIL means skip over whitespace characters.
NO-CHARS-SPECIAL means do not process escapes specially.
NO-MULTIPLE-ESCAPES means do not process multiple-escape characters specially.

The first value is the translated character.
The second is the index for looking in READ's FSM.
The third is the original, nontranslated character.

Has a kludge for IBASE > 10. where letters that should be digits
return the readtable code for EXTENDED-DIGIT rather than their own codes."
  (DECLARE (VALUES TRANSLATED-CHAR FSM-INDEX ACTUAL-CHAR))
  (PROG TOP (CH BITS CODE CH-CHAR)
	(SETQ XR-XRTYI-PREV-CHAR XR-XRTYI-LAST-CHAR)
     L
	(DO-FOREVER
	  (SETQ CH (FUNCALL STREAM ':TYI))
	  (COND ((NULL CH)
		 (RETURN-FROM TOP CH (RDTBL-EOF-CODE READTABLE) CH))
		((AND READ-DISCARD-FONT-CHANGES
		      (EQ CH #/))
		 (IF (EQ #/ (SEND STREAM ':TYI))
		     (RETURN)))
		((NOT (> CH RDTBL-ARRAY-SIZE))
		 (RETURN))))
	(SETQ CH-CHAR (LDB %%CH-CHAR CH))
	(SETQ BITS (RDTBL-BITS READTABLE CH-CHAR))
	(SETQ CODE (RDTBL-CODE READTABLE CH-CHAR))
	(COND ((AND (NOT NO-CHARS-SPECIAL)
		    (NOT NO-MULTIPLE-ESCAPES)
		    (= CODE
		       (RDTBL-MULTIPLE-ESCAPE-CODE READTABLE)))
	       (SETQ READ-INSIDE-MULTIPLE-ESCAPE
		     (IF READ-INSIDE-MULTIPLE-ESCAPE NIL
		       CH-CHAR))
	       (GO L))
	      ((AND (NOT NO-CHARS-SPECIAL)
		    (= CODE
		       (RDTBL-ESCAPE-CODE READTABLE)))
	       (SETQ XR-XRTYI-PREV-CHAR CH)
	       (DO-FOREVER
		 (SETQ CH (FUNCALL STREAM ':TYI))
		 (COND ((AND READ-DISCARD-FONT-CHANGES
			     (EQ CH #/))
			(IF (EQ #/ (SEND STREAM ':TYI))
			    (RETURN)))
		       (T (RETURN))))
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (OR CH
			   (PROGN
			     (CERROR ':NO-ACTION NIL 'SYS:READ-END-OF-FILE
				     "EOF on ~S after a ~S." STREAM
				     (STRING XR-XRTYI-PREV-CHAR))
			     #\SP))
		       (RDTBL-SLASH-CODE READTABLE)
		       CH))
	      ((AND (NOT NO-CHARS-SPECIAL)
		    (= CODE
		       (RDTBL-CHARACTER-CODE-ESCAPE-CODE READTABLE)))
	       (SETQ XR-XRTYI-LAST-CHAR (XR-READ-CIRCLECROSS STREAM))
	       (RETURN XR-XRTYI-LAST-CHAR
		       (RDTBL-SLASH-CODE READTABLE)
		       XR-XRTYI-LAST-CHAR))
	      (READ-INSIDE-MULTIPLE-ESCAPE
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (OR CH
			   (PROGN
			     (CERROR ':NO-ACTION NIL 'SYS:READ-END-OF-FILE
				     "EOF on ~S inside a ~C-quoted token." STREAM
				     READ-INSIDE-MULTIPLE-ESCAPE)
			     #\SP))
		       (RDTBL-SLASH-CODE READTABLE)
		       CH))
	      (T
	       ;; Ordinary character.
	       (COND ((AND IGNORE-WHITESPACE
			   (BIT-TEST 1 BITS))
		      ;; Here if whitespace char to be ignored.
		      (SETQ XR-XRTYI-PREV-CHAR CH)
		      (GO L)))
	       ;; Here for ordinary, significant input char.
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (RDTBL-TRANS READTABLE CH-CHAR)
		       ;; If not doing slashes, caller must not really want the RDTBL-CODE,
		       ;; so return a value which, if passed to XR-XRUNTYI,
		       ;; will prevent barfing.
		       (IF NO-CHARS-SPECIAL 0
			 (IF (AND (NUMBERP IBASE)
				  (> IBASE 10.)
				  ( #/A (CHAR-UPCASE CH) (+ IBASE #/A -11.)))
			     (CDR (GETF (RDTBL-PLIST READTABLE) 'EXTENDED-DIGIT))
			   (RDTBL-CODE READTABLE CH-CHAR)))
		       CH)))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-READ-THING (STREAM)
       (PROG (CH NUM A B STRING (INDEX 0) STLEN REAL-CH ALREADY-RETURNED
		 (READTABLE-FSM (RDTBL-FSM READTABLE))
		 (FNPROP (RDTBL-READ-FUNCTION-PROPERTY READTABLE))
		 (STATE (RDTBL-STARTING-STATE READTABLE))
		 READ-INSIDE-MULTIPLE-ESCAPE)
	     (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM T))
	     (SETQ STATE (AR-2 READTABLE-FSM STATE NUM))
	     (UNLESS (NUMBERP STATE)
	       (LET ((FLAG (CAR STATE))
		     (TODO (CDR STATE)))
		 (SELECTQ FLAG
		   (NO-UNTYI-QUOTE
		    (SETQ A TODO)
		    (SETQ B 'SPECIAL-TOKEN))
		   (LAST-CHAR
		    (MULTIPLE-VALUE (A B)
		      (FUNCALL (GET TODO FNPROP)
			       STREAM NIL CH)))
		   (NO-UNTYI-FUNCTION
		    (IF *READ-SUPPRESS*
			(SETQ A NIL B NIL)
		      (SETQ STRING (GET-READ-STRING))
		      (SETF (FILL-POINTER STRING) 1)
		      (AS-1 CH STRING 0)
		      (MULTIPLE-VALUE (A B ALREADY-RETURNED)
			(FUNCALL (GET TODO FNPROP) STREAM STRING))
		      (UNLESS ALREADY-RETURNED
			(RETURN-READ-STRING STRING))))
		   ((UNTYI-QUOTE UNTYI-FUNCTION)
		    (FERROR 'SYS:READ-ERROR-1
			    "Reader in infinite loop reading character: /"~C/"."
			    REAL-CH))
		   (OTHERWISE
		    (FERROR 'SYS:READ-ERROR-1
			    "The reader found ~S in the finite state machine."
			    FLAG)))
		 (RETURN A B)))
	     (SETQ STRING (GET-READ-STRING))
	     (SETQ STLEN (ARRAY-LENGTH STRING))
	   L (AS-1 CH STRING INDEX)
	     (SETQ INDEX (1+ INDEX))
	     (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
	     (SETQ STATE (AR-2 READTABLE-FSM STATE NUM))
	     (COND ((NUMBERP STATE)
		    (COND ((= INDEX STLEN)
			   (SETQ STLEN (+ 40 STLEN))
			   (ADJUST-ARRAY-SIZE STRING STLEN)
			   (SETQ STRING (FOLLOW-STRUCTURE-FORWARDING STRING))))
		    (GO L)))
	     (LET ((FLAG (CAR STATE))
		   (TODO (CDR STATE)))
	       (SELECTQ FLAG
		 (UNTYI-FUNCTION
		  (XR-XRUNTYI STREAM REAL-CH NUM)
		  (SETF (FILL-POINTER STRING) INDEX)
		  (IF *READ-SUPPRESS*
		      (SETQ A NIL B NIL)
		    (MULTIPLE-VALUE (A B ALREADY-RETURNED)
		      (FUNCALL (GET TODO FNPROP) STREAM STRING))))
		 (LAST-CHAR
		  (SETF (FILL-POINTER STRING) INDEX)
		  (MULTIPLE-VALUE (A B ALREADY-RETURNED)
		    (FUNCALL (GET TODO FNPROP)
			     STREAM STRING CH)))
		 (NO-UNTYI-FUNCTION
		  (SETF (FILL-POINTER STRING) (1+ INDEX))
		  (AS-1 CH STRING INDEX)
		  (IF *READ-SUPPRESS*
		      (SETQ A NIL B NIL)
		    (MULTIPLE-VALUE (A B ALREADY-RETURNED)
		      (FUNCALL (GET TODO FNPROP) STREAM STRING))))
		 (UNTYI-QUOTE
		  (XR-XRUNTYI STREAM REAL-CH NUM)
		  (SETQ A TODO)
		  (SETQ B 'SPECIAL-TOKEN))
		 (NO-UNTYI-QUOTE
		  (SETQ A TODO)
		  (SETQ B 'SPECIAL-TOKEN))
		 (OTHERWISE
		  (FERROR 'SYS:READ-ERROR-1
			  "The reader found ~S in the finite state machine."
			  FLAG)))
	       (UNLESS ALREADY-RETURNED
		 (RETURN-READ-STRING STRING))
	       (RETURN A B))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFUN XR-DOUBLEQUOTE-MACRO (STREAM MATCH)
  (PROG (CH NUM REAL-CH (I 0) (LEN 100) STRING TEM)
	(SETQ STRING (MAKE-ARRAY 100 ':TYPE ART-STRING))
	   ;Should have ':AREA READ-AREA, but leave this out for now because the
	   ;compiler thinks it can use COPYTREE to copy forms out of the temp area.
	(SETQ TEM (RDTBL-SLASH-CODE READTABLE))
      L (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM NIL NIL T))
	(COND ((NULL CH)
	       (CERROR ':NO-ACTION NIL 'SYS:READ-STRING-END-OF-FILE
		       "EOF on ~S in the middle of a string."
		       STREAM STRING)
	       (RETURN ""))
	      ((AND (= (LDB %%CH-CHAR REAL-CH) MATCH)
		    (NOT (= NUM TEM)))
		(ADJUST-ARRAY-SIZE STRING I)
		(RETURN STRING))
	      (T (AS-1 (LDB %%CH-CHAR REAL-CH) STRING I)
		 (SETQ I (1+ I))
		 (COND ((= I LEN)
			(SETQ LEN (+ LEN 40))
			(ADJUST-ARRAY-SIZE STRING LEN)))
		 (GO L)))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFUN XR-#R-MACRO (STREAM IGNORE &OPTIONAL (RADIX XR-SHARP-ARGUMENT))
  (UNLESS (FIXP RADIX)
    (UNLESS *READ-SUPPRESS*
      (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
	      "#R was read with no digits after the #."))
    (SETQ RADIX 10.))
  (LET ((IBASE RADIX))
    (VALUES (INTERNAL-READ STREAM T NIL T))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(ADD-INITIALIZATION 'READ-CHECK-INDENTATION
		    '(SETQ READ-CHECK-INDENTATION NIL INSIDE-COLUMN-0-LIST NIL
			   *COMMON-LISP* NIL *READER-SYMBOL-SUBSTITUTIONS* NIL
			   READ-DONT-INTERN NIL READ-DISCARD-FONT-CHANGES NIL
			   READ-PRESERVE-DELIMITERS NIL 
			   READ-INSIDE-MULTIPLE-ESCAPE NIL
			   READ-INTERN-FUNCTION 'INTERN)
		    '(:WARM))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN SET-SYNTAX-FROM-CHAR (TO-CHAR FROM-CHAR &OPTIONAL
			     (TO-READTABLE READTABLE) (FROM-READTABLE READTABLE))
  "Copy the syntax of FROM-CHAR in FROM-READTABLE to TO-CHAR in TO-READTABLE, Common Lisp style.
Common Lisp has a peculiar definition of just what it is about
the character syntax that can be copied, while other aspects
are supposed to be immutable for a particular character.
This function contains hair to copy only the mutable part of the syntax.
To copy all aspects of the syntax, use COPY-SYNTAX."
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
	     (IF (MEMQ TO-CHAR '(#\SPACE #\RETURN #\LF #\TAB #\RUBOUT #\BS #\PAGE))
		 (SETQ SYNTAX-STANDARD 'ILLEGAL)
	       (SETQ SYNTAX-STANDARD #/!)))))
    (IF (NULL SYNTAX-STANDARD)
	(SET-SYNTAX-BITS TO-CHAR (GET (LOCF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE))
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
	(PUSH (COPYTREE FROM-AENTRY) (RDTBL-MACRO-ALIST TO-READTABLE)))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "


(DEFMETHOD (PRINT-READABLY-MIXIN :PRINT-SELF) (STREAM &REST IGNORE)
  (LET ((BASE 8) (IBASE 8)
	(READTABLE INITIAL-READTABLE)
	(PACKAGE PKG-USER-PACKAGE))
    (SEND STREAM ':STRING-OUT "#")
    (PRIN1 (TYPEP SELF) STREAM)
    (SEND STREAM ':TYO #\SP)
    (DO ((INIT-OPTIONS (SEND SELF ':RECONSTRUCTION-INIT-PLIST) (CDDR INIT-OPTIONS)))
	((NULL INIT-OPTIONS))
      (PRIN1 (CAR INIT-OPTIONS) STREAM)
      (SEND STREAM ':TYO #\SP)
      (PRIN1 (CADR INIT-OPTIONS) STREAM)
      (IF (CDDR INIT-OPTIONS)
	  (SEND STREAM ':TYO #\SP)))
    (SEND STREAM ':TYO #/)))

(DEFMETHOD (PRINT-READABLY-MIXIN :READ-INSTANCE) (FLAVOR STREAM)
  (DO ((IBASE 8) (BASE 8)
       (READTABLE INITIAL-READTABLE)
       (PACKAGE PKG-USER-PACKAGE)
       CH INIT-OPTIONS)
      (())
    ;; Skip past spaces.
    (DO ()
	((NOT (= (SETQ CH (SEND STREAM ':TYI)) #\SP))
	 (SEND STREAM ':UNTYI CH)))
    (IF (= CH #/)
	(RETURN (LEXPR-FUNCALL 'MAKE-INSTANCE FLAVOR INIT-OPTIONS)))
    (SETQ INIT-OPTIONS
	  (LIST* (READ STREAM)
		 (READ STREAM)
		 INIT-OPTIONS))))

))
