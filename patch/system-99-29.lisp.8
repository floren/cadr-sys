;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.33
;;; Written 9-Sep-85 20:55:08 by RMS,
;;; while running on Lisp Machine Twenty-three from band 3
;;; with System 99.27, CADR 4.2, Experimental ZMail 54.4, MIT-Specific 23.0, microcode 320, GC@2.



; From file OZ:KANSAS:<L.ZWEI>BDIRED.LISP.42 at 9-Sep-85 20:55:25
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; BDIRED  "

(DEFMAJOR COM-BDIRED-MODE BDIRED-MODE "BDired"
	  "Setup for editing directory differences" ()
  (PROGN					;Due to ZWEI inversion lossage.
  (IF (TYPEP *INTERVAL* 'FILE-BUFFER)
      (LET ((PATHNAME1 (BUFFER-PATHNAME *INTERVAL*))
	    (PATHNAME2 (BDIRED-ALTERNATE-PATHNAME *INTERVAL*)))
	(IF (NOT (NULL PATHNAME1))
	    (SETQ *BDIRED-PATHNAME1-NAME* (STRING PATHNAME1)))
	(IF (NOT (NULL PATHNAME2))
	    (SETQ *BDIRED-PATHNAME2-NAME* (STRING PATHNAME2))))))
  (SET-COMTAB *MODE-COMTAB*
	      '(#/SP COM-DOWN-REAL-LINE
		#/= COM-DIRED-SRCCOM
		#/? COM-BDIRED-DOCUMENTATION
		#/HELP COM-BDIRED-DOCUMENTATION
		#/C COM-DIRED-COPY
		#/c (0 #/C)
		#/I COM-BDIRED-RESOLVE-INCONSISTENCY
		#/i (0 #/I)
		#/Q COM-BDIRED-EXIT
		#/q (0 #/Q)
		#/R COM-DIRED-RENAME
		#/r (0 #/R)
		#/T COM-BDIRED-TRANSFER
		#/t (0 #/T)
		#/U COM-BDIRED-UNMARK
		#/u (0 #/U)
		#/V COM-DIRED-VIEW-FILE
		#/v (0 #/V)
		#/RUBOUT COM-BDIRED-REVERSE-UNMARK
		#/ABORT COM-BDIRED-ABORT
		#/END COM-BDIRED-EXIT)
	      '())
  (SETQ *MODE-LINE-LIST*
	(APPEND *MODE-LINE-LIST*
		'("  " *BDIRED-PATHNAME1-NAME*
		  "  " *BDIRED-PATHNAME2-NAME*
		  "  (Q to exit)"))))

))

; From file OZ:KANSAS:<L.ZWEI>BDIRED.LISP.42 at 9-Sep-85 20:55:33
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; BDIRED  "

(DEFCOM COM-BDIRED-UNMARK "Un-mark file(s) for transfer or other action." ()
  (DIRED-MAP-OVER-LINES (IF (AND (NOT *NUMERIC-ARG-P*)
				 (OR (NOT (DIRED-LINE-PATHNAME (BP-LINE (POINT))))
				     (EQ (BP-CHAR (BEG-LINE (POINT))) #/SP)))
			    -1
			    *NUMERIC-ARG*)
			#'(LAMBDA (LINE)
			    (MUNG-LINE LINE)
			    (ASET #/SP LINE 0)
			    (BDIRED-UNMARK-TRANSFER (BDIRED-LINE-CFILE LINE)))))

))

; From file OZ:KANSAS:<L.ZWEI>BDIRED.LISP.42 at 9-Sep-85 20:55:37
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; BDIRED  "

(DEFCOM COM-BDIRED-REVERSE-UNMARK "Un-mark previous file(s) for transfer or other action." ()
  (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
  (COM-BDIRED-UNMARK))

))

; From file OZ:KANSAS:<L.ZWEI>BDIRED.LISP.42 at 9-Sep-85 20:55:43
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; BDIRED  "

(DEFCOM COM-BDIRED-HELP "Explain use of DIRED commands" ()
  (FORMAT T "You are in the balance directories editor.  The commands are:
	T	Mark the current file to be transferred.
	P	Mark the current file to be printed on the standard hardcopy device.
	U	Cancel any request marked on the current line, or else the line above.
	Rubout	Move up and cancel any request marked on that line.
	R	Rename this file.  You type the new filename in a mini buffer.
	C	Copy this file.  You type the new filename in a mini buffer.
	Space	Move to the next line.
	  Above commands repeat with a numeric argument,
	  backwards if the argument is negative.	  
	V	View current file.
	Q (or END)  Exit.  The remaining files in the transfer lists will be moved.
	ABORT	Exit without performing any transfers.
	=	SRCCOM this file with the > version.
")
  DIS-NONE)

))

; From buffer coma at 9-Sep-85 21:04:02
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-INSERT-CRS "Insert one or more Returns into the buffer, moving point.
In auto fill mode, if no numeric argument,
fills the line before point as well as inserting a Return.
This might cause another Return to be inserted earlier in the line." ()
  (LET ((POINT (POINT)))
    (SETQ *CURRENT-COMMAND-TYPE* 'INSERT-CR)
    (DOTIMES (I *NUMERIC-ARG*)
      (INSERT-MOVING POINT #/NEWLINE)))
  DIS-TEXT)

))

; From buffer coma at 9-Sep-85 21:04:19
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-THIS-INDENTATION "Insert new line after this one, indent it to under point.
With arg, indent this line to here." ()
  (LET ((BP1 (FORWARD-OVER *BLANKS* (POINT)))
	(BP2 (IF *NUMERIC-ARG-P* (POINT) (INSERT-MOVING (END-LINE (POINT)) #/NEWLINE))))
    (MOVE-BP (POINT) (INDENT-LINE BP2 (BP-INDENTATION BP1))))
  DIS-TEXT)

))

; From buffer coma at 9-Sep-85 21:04:31
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-KILL-LINE "Kill to end of line, or kill an end of line.
If at end of line aside from possible whitespace, kill to beginning of next line.
Otherwise, kill up to the end of the current line.
With a numeric argument, always kill the specified number of lines." ()
  (LET ((POINT (POINT)))
    (COND ((AND (BP-= POINT (INTERVAL-LAST-BP *INTERVAL*)) (PLUSP *NUMERIC-ARG*))
	   (BARF "Attempt to kill past the end of the buffer."))
	  (T
	   (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
	   (COND (*NUMERIC-ARG-P*
		  (KILL-INTERVAL-ARG POINT
				     (BEG-LINE POINT *NUMERIC-ARG* T)
				     *NUMERIC-ARG*)
		  DIS-TEXT)
		 ((END-LINE-P (FORWARD-OVER *BLANKS* POINT))
		  (KILL-INTERVAL POINT (BEG-LINE POINT 1 T) T T)
		  DIS-TEXT)
		 (T
		  (KILL-INTERVAL POINT (END-LINE POINT) T T)
		  (VALUES DIS-LINE (BP-LINE POINT) (BP-INDEX POINT))))))))

))

; From file OZ:KANSAS:<L.ZWEI>COMB.LISP.96 at 9-Sep-85 21:22:22
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(DEFCOM COM-PREFIX-ABORT
  "Same as just Abort." (KM)
  (FUNCALL (COMMAND-LOOKUP #/ABORT *COMTAB*)))

))

; From file OZ:KANSAS:<L.ZWEI>COMB.LISP.96 at 9-Sep-85 21:22:38
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(DEFCOM COM-KILL-COMMENT "Delete any comment on the current line.
If region exists, kill all comments in region." ()
  (IF (WINDOW-MARK-P *WINDOW*)
      (COM-KILL-COMMENTS-IN-REGION)
    (LET ((LEN (LINE-LENGTH (BP-LINE (POINT)))))
      (KILL-COMMENT (BP-LINE (POINT)))
      (OR (= LEN (LINE-LENGTH (BP-LINE (POINT))))
	  (MOVE-BP (POINT) (END-LINE (POINT))))))
  DIS-TEXT)

))

; From file OZ:KANSAS:<L.ZWEI>COMB.LISP.96 at 9-Sep-85 21:22:41
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(DEFCOM COM-KILL-COMMENTS-IN-REGION "Delete any comments within the region." ()
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("Kill Comments" BP1 BP2 T)
      (REGION-LINES (START-LINE STOP-LINE)
	(DO ((LINE START-LINE (LINE-NEXT LINE)))
	    ((EQ LINE STOP-LINE))
	  (KILL-COMMENT LINE)))))
  DIS-TEXT)

))

; From file OZ:KANSAS:<L.ZWEI>COMB.LISP.96 at 9-Sep-85 21:22:55
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(DEFCOM COM-SET-FILL-PREFIX
  "Define Fill Prefix from the current line.
All of the current line up to point becomes the Fill Prefix.
When there is a non-empty fill prefix, any line that fails to start
with the fill prefix is considered a separator of paragraphs.
Fill Region assumes that each non-blank line starts with the prefix
/(which is ignored for filling purposes).  To stop using a Fill Prefix, do
a Set Fill Prefix at the beginning of a line." () 
  (SETQ *FILL-PREFIX* (SUBSTRING (BP-LINE (POINT)) 0 (BP-INDEX (POINT))))
  (FORMAT *QUERY-IO* "~&Fill prefix = ~S" *FILL-PREFIX*)
  DIS-NONE)

))

; From buffer comc at 9-Sep-85 21:31:09
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-EVALUATE-INTO-BUFFER
  "Evaluate a form from the mini-buffer and insert the results into the buffer.
If there are multiple values, each value is printed into the buffer,
with a Return before each one.
A numeric argument means output printed by the evaluation also goes in the buffer." (KM)
  (LET ((FORM (TYPEIN-LINE-MULTI-LINE-READ "Lisp form: (end with ~C)" #/END))
	(STREAM (INTERVAL-STREAM-INTO-BP (POINT))))
    (LET ((VALUES
	    (MULTIPLE-VALUE-LIST
	      (LET ((*STANDARD-OUTPUT* (IF *NUMERIC-ARG-P* STREAM *STANDARD-OUTPUT*)))
		(SI:EVAL-ABORT-TRIVIAL-ERRORS FORM)))))
      (DOLIST (V VALUES)
	(TERPRI STREAM)
	(FUNCALL (OR PRIN1 #'PRIN1)
		 V STREAM)))
    (MOVE-BP (POINT) (SEND STREAM :READ-BP)))
  DIS-TEXT)

))

; From buffer comc at 9-Sep-85 21:36:47
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-MICROCOMPILE-REGION "Microcompile the current region or defun.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled." ()
  (COMPILE-DEFUN-INTERNAL T "Microcompiling" "microcompiled."
			    NIL ;USE-TYPEOUT
			    NIL ;DEFVAR-HACK
			    'COMPILER:MICRO-COMPILE)
  DIS-NONE)

))

; From buffer comc at 9-Sep-85 21:37:18
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-INTERVAL (COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK
			 BP1 &OPTIONAL BP2 IN-ORDER-P
			 (COMPILE-PROCESSING-MODE 'COMPILER:MACRO-COMPILE)
			 &AUX GENERIC-PATHNAME STREAM
			      WHOLE-FILE   ;T if processing the entire file.
			      SI:FDEFINE-FILE-DEFINITIONS)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
Does not print any sort of message saying what is being compiled,
does not know about sectionization.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
PRINT-RESULTS-STREAM is a stream for printing the results of evaluation, or NIL not to print.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is either COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization."
  (DECLARE (SPECIAL COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK COMPILE-PROCESSING-MODE))
  (SETQ GENERIC-PATHNAME (SEND *INTERVAL* :GENERIC-PATHNAME))
  ;; Does not reparse the mode line; we should let the user decide whether to do that.!
  ;; Should not override the user's Set Package if he has done one.
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  ;; Decide whether the entire file is being processed or just a part.
  ;; If the whole file, we want to notice if any function present in the file previously
  ;; is now missing.  If just a part, anything we don't notice now we must assume
  ;; is elsewhere in the file.
  (SETQ WHOLE-FILE
	(AND (BP-= BP1 (INTERVAL-FIRST-BP *INTERVAL*))
	     (BP-= BP2 (INTERVAL-LAST-BP *INTERVAL*))))
  (SETQ STREAM (INTERVAL-STREAM BP1 BP2 T))
  ;; Arrange for first read-error's location to be saved in q-reg ".".
  (REMPROP (MAKE-REGISTER-NAME #/.) 'POINT)
  (LET ((SI:*ALL-FREE-INTERPRETER-VARIABLE-REFERENCES-SPECIAL* T))
    (MULTIPLE-VALUE-BIND (VARS VALS) (SEND *INTERVAL* :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	(WHEN FS:THIS-IS-A-PATCH-FILE
	  ;; If compiling out of the editor buffer of a patch file,
	  ;; make sure the file itself is marked
	  ;; so that Meta-. will behave right.
	  (PUTPROP GENERIC-PATHNAME T :PATCH-FILE))
	;; Bind off this flag -- our stream is not generating font changes
	;; so READ should not try to remove any.
	(LET ((SI:READ-DISCARD-FONT-CHANGES NIL))
	  (FLET ((DO-IT ()
			(COMPILER:COMPILE-STREAM
			  STREAM
			  GENERIC-PATHNAME
			  NIL			;FASD-FLAG
			  (IF (AND COMPILE-P (NOT (EQ COMPILE-P T)))
		;if using user supplied evaluator, avoid any possible macro-expanding, etc
		; in COMPILE-DRIVER.
			      'SIMPLE-COMPILE-INTERVAL-PROCESS-FN
			    'COMPILE-INTERVAL-PROCESS-FN)
			  T			;QC-FILE-LOAD-FLAG
			  NIL			;QC-FILE-IN-CORE-FLAG
			  *PACKAGE*
			  NIL			;FILE-LOCAL-DECLARATIONS
			  NIL			;Unused
			  WHOLE-FILE)))
	    (IF COMPILE-P
		(COMPILER:LOCKING-RESOURCES-NO-QFASL (DO-IT))
	      (DO-IT)))))))
  (OR (NULL GENERIC-PATHNAME)
      (SI:RECORD-FILE-DEFINITIONS GENERIC-PATHNAME SI:FDEFINE-FILE-DEFINITIONS WHOLE-FILE)))

))

; From buffer comc at 9-Sep-85 21:37:53
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN SIMPLE-COMPILE-INTERVAL-PROCESS-FN (FORM)
  (COMPILE-INTERVAL-PROCESS-BASIC-FORM FORM 'RANDOM))

))

; From buffer comc at 9-Sep-85 21:38:13
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFMACRO DO-CHANGED-LISP-BUFFERS ((BUFFER BUFFER-LIST) &BODY BODY)
  "This is careful to not use random buffers."
  `(DOLIST (,BUFFER ,BUFFER-LIST)
     (WHEN (AND (EQ (IF (EQ ,BUFFER *INTERVAL*)
			*MAJOR-MODE*
		      (BUFFER-SAVED-MAJOR-MODE ,BUFFER))
		    'LISP-MODE)
		(NOT (GET ,BUFFER 'SPECIAL-PURPOSE))
		;; Don't consider buffers never modified.
		(> (NODE-TICK ,BUFFER)
		   (BUFFER-FILE-READ-TICK BUFFER)))
       ,@BODY)))

))

; From buffer comc at 9-Sep-85 21:38:16
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-COMPILE-CHANGED-SECTIONS "Compile any sections which have been edited.
Only sections that contain definitions will be compiled.
A numeric arg means ask about each section individually."
	()
  (DO-CHANGED-LISP-BUFFERS (BUFFER *ZMACS-BUFFER-LIST*)
    (SI:FILE-OPERATION-WITH-WARNINGS
      ((AND (BUFFER-FILE-ID BUFFER)
	    (SEND (SEND BUFFER :GENERIC-PATHNAME) :GENERIC-PATHNAME))
       :COMPILE NIL)
      (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
	(COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

))

; From buffer comc at 9-Sep-85 21:38:19
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-TAGS-COMPILE-CHANGED-SECTIONS "Compile any sections in files in tag table which have been edited.
Only sections that contain definitions will be compiled.
A numeric arg means ask about each section individually." ()
  (DO-CHANGED-LISP-BUFFERS (BUFFER (TAG-TABLE-BUFFERS NIL))
    (SI:FILE-OPERATION-WITH-WARNINGS
      ((AND (BUFFER-FILE-ID BUFFER)
	    (SEND (SEND BUFFER :GENERIC-PATHNAME) :GENERIC-PATHNAME))
       :COMPILE NIL)
      (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
	(COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

))

; From buffer comc at 9-Sep-85 21:38:22
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-EVALUATE-CHANGED-SECTIONS "Evaluate any sections which have been edited.
Only sections that contain definitions will be evaluated.
A numeric arg means ask about each section individually."
	()
  (DO-CHANGED-LISP-BUFFERS (BUFFER *ZMACS-BUFFER-LIST*)
    (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*
				      NIL '("Evaluate" "Evaluating" "evaluated.")))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

))

; From buffer comc at 9-Sep-85 21:38:26
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFCOM COM-TAGS-EVALUATE-CHANGED-SECTIONS
  "Evaluate any sections in files in tag table which have been edited.
Only sections that contain definitions will be evaluated.
A numeric arg means ask about each section individually."
  ()
  (DO-CHANGED-LISP-BUFFERS (BUFFER (TAG-TABLE-BUFFERS NIL))
    (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*
				      NIL '("Evaluate" "Evaluating" "evaluated.")))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

))

; From buffer comd at 9-Sep-85 21:43:44
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-SAVE-POSITION-IN-REGISTER "Save the current point in a register.
The register name, a character with no meta bits, is read from the keyboard." ()
  (LET ((Q-REG (GET-REGISTER-NAME "Point to register:")))
    (SAVE-POSITION-IN-REGISTER Q-REG (POINT)))
  DIS-NONE)

))

; From buffer comd at 9-Sep-85 21:43:49
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-JUMP-TO-REGISTER-POSITION "Restore a saved position from a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
    (LET ((PT (GET Q-REG 'POINT)))
      (WHEN (NULL PT)
	(BARF "The register ~A doesn't point anywhere." Q-REG))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
      (MAKE-BUFFER-CURRENT (CDR PT))
      (MOVE-BP (POINT) (CAR PT))))
  DIS-BPS)

))

; From buffer comd at 9-Sep-85 21:43:59
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-MINI-BUFFER-BEEP "Quit out of the mini buffer.
If there is text in the mini buffer, delete it all.
If the mini buffer is empty, quit out of it." ()
  (BEEP)
  (COND (*NUMERIC-ARG-P* DIS-NONE)
	((WINDOW-MARK-P *WINDOW*)
	 (SETQ *MARK-STAYS* NIL)
	 DIS-NONE)
	((BP-= (INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
	 (*THROW 'NON-MINIBUFFER-LEVEL T))
	(T
	 (DELETE-INTERVAL *INTERVAL*)
	 DIS-TEXT)))

))

; From file OZ:KANSAS:<L.ZWEI>COME.LISP.135 at 9-Sep-85 21:57:01
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COME  "

(DEFCOM COM-VARIOUS-QUANTITIES "Performs some operation on some textual unit.
First character following is operation:
  F forward, B backward, K kill, R rubout, X transpose, @ mark region, U uppercase,
  L lowercase, S save, C copy, Z reverse.
Second character following is quantity type:
  C character, W word, S sentence, P paragraph, L line, A atom, - S-expression,
  ( or ) list, D defun, Clear-Screen page separated by Ls, H buffer.
Numeric arguments are obeyed.  ? for help." ()
  (LET (CH MODE-NAME MODE QUANTITY)
    (SETQ CH (INPUT-WITH-PROMPTS *STANDARD-INPUT* ':TYI))
    (PROG ()
       GET-A-MODE
	  (SELECTQ CH
	    (#/?
	     (PRINT-DOC ':FULL 'COM-VARIOUS-QUANTITIES)
	     (FORMAT *QUERY-IO* "~&Type the OPERATION (F, B, K, R, X, @, U, L, S, C or Z)")
	     (DISCARD-LAST-PROMPT)
	     (PRINT-PROMPTS)
	     (SETQ CH (CHAR-UPCASE (INPUT-WITH-PROMPTS *STANDARD-INPUT* ':TYI)))
	     (GO GET-A-MODE))
	    (#/F
	     (SETQ MODE-NAME "Forward"
		   MODE 'COM-QUANTITY-FORWARD))
	    (#/B
	     (SETQ MODE-NAME "Backward"
		   MODE 'COM-QUANTITY-BACKWARD))
	    ((#/D #/K)
	     (SETQ MODE-NAME "Delete"
		   MODE 'COM-QUANTITY-DELETE))
	    (#/R
	     (SETQ MODE-NAME "Rubout"
		   MODE 'COM-QUANTITY-RUBOUT))
	    ((#/X #/T)
	     (SETQ MODE-NAME "Exchange"
		   MODE 'COM-QUANTITY-TWIDDLE))
	    (#/@
	     (SETQ MODE-NAME "Mark"
		   MODE 'COM-QUANTITY-MARK))
	    (#/U
	     (SETQ MODE-NAME "Uppercase"
		   MODE 'COM-QUANTITY-UPPERCASE))
	    (#/L
	     (SETQ MODE-NAME "Lowercase"
		   MODE 'COM-QUANTITY-LOWERCASE))
	    (#/S
	     (SETQ MODE-NAME "Save"
		   MODE 'COM-QUANTITY-SAVE))
	    (#/C
	     (SETQ MODE-NAME "Copy"
		   MODE 'COM-QUANTITY-COPY))
	    (#/Z
	     (SETQ MODE-NAME "Reverse"
		   MODE 'COM-QUANTITY-REVERSE))
	    (OTHERWISE
	     (BARF "Invalid quantity operation")))
       GET-A-QUANTITY
	  (SETQ CH (CHAR-UPCASE (INPUT-WITH-PROMPTS *STANDARD-INPUT* ':TYI)))
	  (SELECTQ CH
	    (#/?
	     (PRINT-DOC ':FULL 'COM-VARIOUS-QUANTITIES)
	     (FORMAT *QUERY-IO* "~&Type quantity name (C, W, S, P, A, L, -, ( or ), D, ~C, or H)"
		     #/CLEAR-SCREEN)
	     (DISCARD-LAST-PROMPT)
	     (PRINT-PROMPTS)
	     (GO GET-A-QUANTITY))
	    (#/C
	     (SETQ MODE-NAME "Character"
		   QUANTITY 'FORWARD-CHAR))
	    (#/W
	     (SETQ MODE-NAME "Word"
		   QUANTITY 'FORWARD-WORD))
	    (#/A
	     (SETQ MODE-NAME "Atom"
		   QUANTITY 'FORWARD-ATOM))
	    (#/S
	     (SETQ MODE-NAME "Sentence"
		   QUANTITY 'FORWARD-SENTENCE))
	    (#/P
	     (SETQ MODE-NAME "Paragraph"
		   QUANTITY 'FORWARD-PARAGRAPH))
	    (#/L
	     (SETQ MODE-NAME "Line"
		   QUANTITY 'FORWARD-LINE))
	    (#/-
	     (SETQ MODE-NAME "S-Expression"
		   QUANTITY 'FORWARD-SEXP))
	    ((#/( #/))
	     (SETQ MODE-NAME "List"
		   QUANTITY 'FORWARD-LIST))
	    (#/D
	     (SETQ MODE-NAME "Defun"
		   QUANTITY 'FORWARD-DEFUN))
	    (#/FF
	     (SETQ MODE-NAME "Page"
		   QUANTITY 'FORWARD-PAGE))
	    (#/H
	     (SETQ MODE-NAME "Buffer"
		   QUANTITY 'FORWARD-BUFFER))
	    (OTHERWISE
	     (BARF "Invalid quantity type")))
	  (LET ((*QUANTITY-MODE* QUANTITY))
	    (FUNCALL MODE)))))

))

; From file OZ:KANSAS:<L.ZWEI>COMF.LISP.103 at 9-Sep-85 22:14:46
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-FROB-LISP-CONDITIONAL "Change CONDs to ANDs or ORs and vice versa.
When changing to COND, point is left in such a place that LF will add another
clause to this condition, and M-) will add another condition.  Also in this case
an argument specifies the number of clauses that are left in the
consequent, the default is 1, i.e. all clauses but the last are assumed to
be for value, and to belong in the antecedent." ()
  (ATOM-WORD-SYNTAX-BIND
    (LET ((POINT (POINT))
	  FIXBP1 FIXBP2)
     (UNWIND-PROTECT
      (LET (COND-BP COND-TYPE UPCASE-P BP)
        (MULTIPLE-VALUE-SETQ (COND-BP COND-TYPE)
	  (FIND-CONTAINING-ATOM POINT '(COND AND OR IF)))	;Locate the COND or AND or OR
        (OR COND-BP (BARF))
        (SETQ UPCASE-P (UPPER-CASE-P (BP-CHARACTER COND-BP)))	;Remember if have to lowercase
        (LET ((START-DEFUN-BP (FORWARD-DEFUN POINT -1 T))
              (END-DEFUN-BP (FORWARD-DEFUN POINT 1 T))
              DEPTH)
	  ;; Parse it all once, then don't even bother checking.
	  (LISP-PARSE-FROM-DEFUN (BP-LINE END-DEFUN-BP) START-DEFUN-BP)
	  ;; Count how many levels down the next defun is from the start of this one.
	  (LET ((*LISP-PARSE-PREPARSED-FLAG* T))
	    (DO ((I -1 (1+ I))
		 (BP3 END-DEFUN-BP (FORWARD-SEXP BP3 -1 NIL 1 START-DEFUN-BP)))
		((NULL BP3) (SETQ DEPTH I))))
	  ;; Insert that many ")"'s just before point, so everything is balanced.
	  ;; These ")"'s lie between FIXBP1 and FIXBP2.  We use that to delete them later.
          (COND ((> DEPTH 0)
                 (LET ((BP (LIKELY-UNBALANCED-POINT (FORWARD-LIST COND-BP -1 NIL 1)
						    END-DEFUN-BP)))
		   (SETQ FIXBP1 (COPY-BP BP :NORMAL)
			 FIXBP2 (COPY-BP BP :MOVES)))
		 (INSERT FIXBP2 #/NEWLINE)
                 (DOTIMES (I DEPTH) (INSERT FIXBP2 #/) ))
                 (INSERT FIXBP2 #/NEWLINE))))
        (COND ((EQ COND-TYPE 'COND)             ;Changing COND to AND or OR
               (LET* ((N (COUNT-LIST-ELEMENTS (FORWARD-LIST COND-BP -1 NIL 1)))
		      (AFTER-COND-SYMBOL (FORWARD-SEXP COND-BP))
		      (FIRST-CLAUSE-LENGTH (COUNT-LIST-ELEMENTS AFTER-COND-SYMBOL)))
		 (AND (> N 3) (BARF "Too many clauses"))
		 (AND (= N 3)
		      (LET ((BP1 (FORWARD-SEXP COND-BP 2)) BP2 BP3)
			(SETQ BP2 (FORWARD-LIST BP1 1 NIL -1 T)
			      BP3 (FORWARD-WORD BP2))
			(OR (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
				 (STRING-EQUAL (BP-LINE BP2) "T" (BP-INDEX BP2) 0
					       (BP-INDEX BP3)))
			    (BARF "Too many clauses"))
			(SETQ BP1 (BACKWARD-OVER '(#/CR #/TAB #/SP) BP1))
			(SETQ BP1 (FORWARD-CHAR BP1 -1))
			(DELETE-INTERVAL BP1 BP3 T)
			(SETQ COND-TYPE (IF (= FIRST-CLAUSE-LENGTH 1) "OR" "IF"))))
		 (AND (> FIRST-CLAUSE-LENGTH 2)
		      (LET* ((BP2 (FORWARD-LIST AFTER-COND-SYMBOL 1 NIL -1 T))
			     (BP3 (FORWARD-SEXP (FORWARD-SEXP BP2 2) -1)))
			(INSERT BP3 "(PROGN ")
			(INSERT (FORWARD-SEXP AFTER-COND-SYMBOL) ")")))
		 (DELETE-INTERVAL COND-BP AFTER-COND-SYMBOL T))
	       (AND (EQ COND-TYPE 'COND)	;Still not determined
		    ;; Check for (COND ((NOT ...)))
		    (LET ((BP1 (FORWARD-LIST COND-BP 1 NIL -2 T)))
		      (LET ((BP2 (FORWARD-WORD COND-BP 1 T)))
			(LET ((WORD (STRING-INTERVAL BP1 BP2)))
			  (COND ((OR (STRING-EQUAL WORD "NULL") (STRING-EQUAL WORD "NOT"))
				 (SETQ BP1 (FORWARD-LIST BP1 -1 NIL 1))
				 (LET ((BP3 (FORWARD-LIST BP1)))
				   (DELETE-INTERVAL (FORWARD-CHAR BP3 -1) BP3 T))
				 (DELETE-INTERVAL BP1 (FORWARD-OVER *BLANKS* BP2) T)
				 (SETQ COND-TYPE "OR"))
				(T
				 (SETQ COND-TYPE "AND")))))))
	       (SETQ BP (FORWARD-OVER *BLANKS* (INSERT COND-BP COND-TYPE)))
	       (LET ((BP1 (FORWARD-LIST BP)))	;Remove a level of parens
		 (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) BP1 T))
	       (DELETE-INTERVAL BP (FORWARD-CHAR BP) T))
	      (T
	       (LET ((BP1 (FORWARD-LIST (FORWARD-LIST (FORWARD-CHAR COND-BP -1))
					-1 NIL -1 T)))
		 (INSERT BP1 #/) )
		 (DO ((N -1 (1+ N))
		      (BP2 BP1 (FORWARD-SEXP BP2 -1))
		      (ARG (COND (*NUMERIC-ARG-P* (- 1 *NUMERIC-ARG*))
				 ((EQ COND-TYPE 'IF) -1)
				 (T 0))))
		     ((BP-= BP2 COND-BP)
		      (COND ((MINUSP (+ ARG N -3))
			     (DELETE-INTERVAL COND-BP (FORWARD-WORD COND-BP) T)
			     (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
						    (INSERT COND-BP "COND")))
			     (INSERT-MOVING BP #/()
			     (COND ((EQ COND-TYPE 'IF)
				    (SETQ BP (FORWARD-SEXP BP 2))
				    (INSERT-MOVING BP ")
 (T"))))
			    (T
			     (SETQ BP (INSERT COND-BP "COND (("))
			     (LET ((BP1 (IF (PLUSP ARG) (FORWARD-LIST BP 1 NIL 1)
					    (FORWARD-SEXP BP (+ ARG N)))))
			       (INSERT BP1 #/) ))
			     (SETQ BP (FORWARD-CHAR BP -1))))
		      (COND ((EQ COND-TYPE 'OR)
			     (INSERT (FORWARD-SEXP BP) #/) )
			     (INSERT-MOVING BP "(NOT "))))))))
        (OR UPCASE-P (DOWNCASE-INTERVAL COND-BP BP T))
        (MOVE-BP POINT (FORWARD-LIST BP -1 NIL (IF (MEMQ COND-TYPE '(IF OR)) 2 1)))
        (COM-INDENT-SEXP)                       ;Regrind changed stuff
        (MOVE-BP POINT (FORWARD-LIST (FORWARD-SEXP POINT) -1 NIL -1 T)))
      (COND (FIXBP1
	     (DELETE-INTERVAL FIXBP1 FIXBP2 T)
	     (FLUSH-BP FIXBP1)
	     (FLUSH-BP FIXBP2))))))
    DIS-TEXT)

))

; From file OZ:KANSAS:<L.ZWEI>COMF.LISP.103 at 9-Sep-85 22:15:58
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFUN DESCRIBE-VARIABLE-INTERNAL (VAR)
  (LET* ((DECL (GETL VAR '(SPECIAL SYS:SYSTEM-CONSTANT)))
	 (BOUND (BOUNDP VAR))
	 (DOC (DOCUMENTATION VAR 'VARIABLE))
	 (STREAM (IF DOC *STANDARD-OUTPUT* *QUERY-IO*)))
    (WHEN (OR DECL BOUND DOC)
      (IF BOUND
	  (FORMAT STREAM "~&~S has a value" VAR)
	(FORMAT STREAM "~&~S is void" VAR))
      (IF (EQ (CAR DECL) 'SPECIAL)
	  (FORMAT STREAM " and is declared special ~:[by file ~A~]"
		  (EQ (CADR DECL) T) (CADR DECL)))
      (IF (EQ (CAR DECL) 'SYS:SYSTEM-CONSTANT)
	  (FORMAT STREAM " and is marked as constant ~:[by file ~A~]"
		  (EQ (CADR DECL) T) (CADR DECL)))
      (IF DOC
	  (FORMAT STREAM "~%~A" DOC))
      T)))

))

; From file OZ:KANSAS:<L.ZWEI>COMG.LISP.41 at 9-Sep-85 22:24:00
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "


(DEFCOM COM-SET-KEY "Install a specified editor command on a specified character.
Two-character sequences starting with prefixes such as Control-X are also allowed;
however, a numeric argument means to regard the prefix character itself as the
character to be redefined." ()
  (LET ((COMMAND (COMPLETING-READ-FROM-MINI-BUFFER "Command to install" *COMMAND-ALIST*)))
    (UNLESS (LISTP COMMAND) (BARF))
    (INSTALL-COMMAND-INTERNAL (CDR COMMAND))))

(DEFCOM COM-DEFINE-CHARACTER "Install a specified editor command on a specified character.
Two-character sequences starting with prefixes such as Control-X are also allowed;
however, a numeric argument means to regard the prefix character itself as the
character to be redefined." ()
  (LET ((COMMAND (COMPLETING-READ-FROM-MINI-BUFFER "Command to install" *COMMAND-ALIST*)))
    (UNLESS (LISTP COMMAND) (BARF))
    (INSTALL-COMMAND-INTERNAL (CDR COMMAND))))

(DEFCOM COM-INSTALL-COMMAND "Install a specified function on a specified editor command character.
The name of the function is read from the mini-buffer (the top of the kill ring
contains the name of the current defun), and a character from the echo area.
Two-character sequences starting with prefixes such as Control-X are also allowed;
however, a numeric argument means to regard the prefix character itself as the
character to be redefined." ()
    (DO (NAME) (NIL)
      (SETQ NAME (READ-FUNCTION-NAME "Name of function to install"
				     (RELEVANT-FUNCTION-NAME (POINT)) NIL 'ALWAYS-READ))
      (AND (OR (FBOUNDP NAME)
	       (FQUERY () "~A is not defined, ok to install anyway? " NAME))
	   (RETURN (INSTALL-COMMAND-INTERNAL NAME)))))

))

; From file OZ:KANSAS:<L.ZWEI>COMG.LISP.41 at 9-Sep-85 22:24:05
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "

(DEFCOM COM-UNDEFINE-CHARACTER "Remove the editor command definition of a character.
Reads the key to undefine using the echo area.
Two-character sequences starting with Control-X are also allowed." ()
  (INSTALL-COMMAND-INTERNAL NIL NIL T))

))

; From file OZ:KANSAS:<L.ZWEI>COMG.LISP.41 at 9-Sep-85 22:24:15
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "

(DEFUN COM-INSTALL-MACRO-INTERNAL (MOUSE-P)
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
	NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ
		 "Name of macro to install (~\lozenged-char\ for last macro defined):"
		 #\Return))
    (COND ((EQ NAME '*EOF*)
	   (SETQ MAC (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)
		 NAME (GENSYM))
	   (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
	  ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
	   (BARF "~A is not a defined macro." NAME)))
    (INSTALL-COMMAND-INTERNAL (MAKE-MACRO-COMMAND NAME MOUSE-P) T)))

))

; From file OZ:KANSAS:<L.ZWEI>COMG.LISP.41 at 9-Sep-85 22:24:25
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "

(DEFUN INSTALL-COMMAND-INTERNAL (COMMAND &OPTIONAL REMEMBER-OLD-P DEINSTALL-P
				 &AUX KEY-LIST)
  (PROMPT-LINE (IF DEINSTALL-P "Key to deinstall:" "Key to get it:"))
  (CLEAR-PROMPTS)
  (ALWAYS-DISPLAY-PROMPTS)
  (DO ((COMTAB *COMTAB*)
       (KEY (INPUT-WITH-PROMPTS *STANDARD-INPUT* :MOUSE-OR-KBD-TYI)
	    (INPUT-WITH-PROMPTS *STANDARD-INPUT* :TYI)))
      (NIL)
    (PUSH KEY KEY-LIST)
    (RECORD-MINI-BUFFER-VALUE NIL)
    (LET ((OLD-COMMAND (COMMAND-LOOKUP KEY COMTAB)))
      (COND ((AND (PREFIX-COMMAND-P OLD-COMMAND)
		  (NOT *NUMERIC-ARG-P*))
	     (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
	    ((Y-OR-N-P "Install command ~S on~{ ~:@C~}? " COMMAND (REVERSE KEY-LIST))
	     ;; Now we know the key sequence, so decide which comtab to really start from.
	     
	     (DO ((COMTAB (SYMEVAL (FQUERY '(:CHOICES (((*ZMACS-COMTAB* "Zmacs") #/Z)
						       ((*STANDARD-COMTAB* "All Zwei editors") #/A)
						       ((*COMTAB* "Just this window") #/W #/T))
						      :TYPE :TYI)
					   "Define for Zmacs, all editors, or just this window? ")))
		  (KEYS KEY-LIST (CDR KEY-LIST)))
		 ((NULL (CDR KEY-LIST))
		  (AND DEINSTALL-P
		       (SETQ COMMAND (MOUSE-MACRO-COMMAND-LAST-COMMAND
				       (COMMAND-LOOKUP KEY COMTAB))))
		  (AND REMEMBER-OLD-P
		       (SET-MOUSE-MACRO-COMMAND-LAST-COMMAND COMMAND
							     (COMMAND-LOOKUP KEY COMTAB)))
		  (COMMAND-STORE COMMAND KEY COMTAB))
	       (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
	     (FORMAT *QUERY-IO* "~&Installed.")
	     (RETURN NIL))
	    (T (FORMAT *QUERY-IO* "~&Not installed.")
	       (RETURN NIL)))))
  DIS-NONE)

))

; From file OZ:KANSAS:<L.ZWEI>COMG.LISP.41 at 9-Sep-85 22:24:45
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "


;;; EMACS compatible macro commands
(DEFCOM COM-START-KEYBOARD-MACRO "Begin defining a keyboard macro.
A numeric argument means append to the previous keyboard macro." ()
  (ASSURE-MACRO-STREAM :MACRO-PUSH)
  (SEND *STANDARD-INPUT* :MACRO-PUSH (+ 2 *NUMERIC-ARG-N-DIGITS*)
	(AND *NUMERIC-ARG-P* (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)))
  DIS-NONE)

(DEFCOM COM-END-KEYBOARD-MACRO "Terminate the definition of a keyboard macro" ()
  (ASSURE-MACRO-STREAM :MACRO-POP)
  (*CATCH 'MACRO-LOOP				;In case no macro running
     (SEND *STANDARD-INPUT* :MACRO-POP (+ 2 *NUMERIC-ARG-N-DIGITS*)
	   (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*)))
  DIS-NONE)

(DEFCOM COM-CALL-LAST-KEYBOARD-MACRO "Repeat the last keyboard macro" ()
  (ASSURE-MACRO-STREAM :MACRO-EXECUTE)
  (SEND *STANDARD-INPUT* :MACRO-EXECUTE NIL (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*))
  DIS-NONE)

(DEFCOM COM-KEYBOARD-MACRO-QUERY "Interactive keyboard macro" ()
  (ASSURE-MACRO-STREAM :MACRO-QUERY)
  (SEND *STANDARD-INPUT* :MACRO-QUERY (+ 2 *NUMERIC-ARG-N-DIGITS*))
  DIS-NONE)

(DEFCOM COM-VIEW-KEYBOARD-MACRO "Typeout the specified keyboard macro.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer.
Just Return means the last one defined, even if temporary." ()
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
	NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ "Name of macro to view (CR for last macro defined):"))
    (COND ((EQ NAME '*EOF*)
	   (SETQ MAC (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)))
	  ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
	   (BARF "~A is not a defined macro." NAME)))
    (DO ((I 0 (1+ I))
	 (LEN (MACRO-LENGTH MAC))
	 (CH))
	((> I LEN))
      (FORMAT T (SELECTQ (SETQ CH (AREF MAC I))
		  (*MOUSE* "Mouse command ~*")
		  (*SPACE* "Macro query ~*")
		  (*RUN* "Repeat ~*")
		  (NIL "Input ~*")
		  (OTHERWISE "~:C "))
	      CH)))
  DIS-NONE)

(DEFCOM COM-NAME-LAST-KEYBOARD-MACRO "Make the last temporary macro permanent.
The new name for the macro is read from the mini-buffer." ()
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET* ((MAC (OR (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)
		  (BARF "There is no previous keyboard macro")))
	 (*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
	 (NAME (TYPEIN-LINE-READ "Name for macro:")))
    (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
  DIS-NONE)

))

; From file OZ:KANSAS:<L.ZWEI>COMG.LISP.41 at 9-Sep-85 22:26:06
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMG  "


(DEFCOM COM-SORT-PAGES
  "Sort the region alphabetically by pages.
A page separator is inserted at the end of the region if there
was not already one there.  For proper results, you may wish to
have an empty line at the beginning of the region, to match the
empty line usually placed at the start of pages." ()
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("Sort" BP1 BP2 T)
      (LET ((NEWDELIM (BEG-LINE-P BP2)))
	(IF NEWDELIM
	    (INSERT-MOVING BP2 (CAR *PAGE-DELIMITER-LIST*)))
	(SORT-INTERVAL-FUNCTIONS #'(LAMBDA (BP)
				     (LOOP
				       WHILE
				       (AND (NOT (BP-= BP (INTERVAL-LAST-BP *INTERVAL*)))
					    (OR (MEMQ (BP-CHAR BP) *PAGE-DELIMITER-LIST*)
						(END-LINE-P BP)))
				       DO
				       (IBP BP))
				     BP)
				 #'(LAMBDA (BP) (FORWARD-PAGE BP 1 T))
				 #'(LAMBDA (BP) BP)
				 #'INTERVAL-WITH-SORT-INTERVAL-LESSP
				 BP1 BP2 T)
	;; If originally was no separator at the end, make it so again.
	(IF NEWDELIM
	    (REGION (BP3 BP4)
	      (DELETE-INTERVAL (FORWARD-CHAR BP4 -1) BP4))))))
  DIS-TEXT)

(DEFVAR *MAKE-KEYBOARD-MACRO-MOVER-COMTAB*)

;;; This returns a function which takes a BP and returns a resultant BP after performing
;;; the given keyboard-macro operation.
(DEFUN MAKE-KEYBOARD-MACRO-MOVER (PROMPT)
  "Returns a function which takes a BP, moves, and returns a BP.
The function is defined to perform the ZWEI commands that you type
while MAKE-KEYBOARD-MACRO-MOVER is running.  Prompts with PROMPT."
  (COM-START-KEYBOARD-MACRO)
  (FORMAT *QUERY-IO* "~&Defining a keyboard macro to ~A~@[; type ~A to finish it~]"
	  PROMPT (KEY-FOR-COMMAND 'COM-END-KEYBOARD-MACRO))
  (LET ((MACRO-ERROR-HOOK #'(LAMBDA ()
			      (*THROW 'EXIT-MAKE-KEYBOARD-MACRO-MOVER :MACRO-ERROR)))
	(MACRO-POP-HOOK #'(LAMBDA ()
			    (*THROW 'EXIT-MAKE-KEYBOARD-MACRO-MOVER T))))
    (AND (EQ (*CATCH 'EXIT-MAKE-KEYBOARD-MACRO-MOVER
	       (SEND SELF :EDIT))
	     :MACRO-ERROR)
	 (*THROW 'ZWEI-COMMAND-LOOP T)))
  (COND ((NOT (BOUNDP '*MAKE-KEYBOARD-MACRO-MOVER-COMTAB*))
	 (SETQ *MAKE-KEYBOARD-MACRO-MOVER-COMTAB* (CREATE-SPARSE-COMTAB 'MACRO-MOVER-COMTAB))
	 (SETF (COMTAB-KEYBOARD-ARRAY *MAKE-KEYBOARD-MACRO-MOVER-COMTAB*)
	       '((-1 . COM-EXIT-KEYBOARD-MACRO-MOVER)))))
  (SET-COMTAB-INDIRECTION *MAKE-KEYBOARD-MACRO-MOVER-COMTAB* *COMTAB*)
  (LET-CLOSED ((OLD-MACRO-PREVIOUS-ARRAY (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)))
    (ARRAY-PUSH-EXTEND OLD-MACRO-PREVIOUS-ARRAY -1)
    (SETF (MACRO-LENGTH OLD-MACRO-PREVIOUS-ARRAY)
	  (1- (MACRO-POSITION OLD-MACRO-PREVIOUS-ARRAY)))
    #'(LAMBDA (BP &AUX (POINT (POINT)) OLD-POINT
	       (MACRO-ERROR-HOOK #'(LAMBDA () (*THROW 'EXIT-KEYBOARD-MACRO-MOVER :MACRO-ERROR))))
	(SETQ OLD-POINT (COPY-BP POINT :NORMAL))
	(MOVE-BP (POINT) BP)
	(UNWIND-PROTECT
	  (PROGN
	    (SEND *STANDARD-INPUT* :MACRO-EXECUTE OLD-MACRO-PREVIOUS-ARRAY 1)
	    (AND (EQ (*CATCH 'EXIT-KEYBOARD-MACRO-MOVER
		       (SEND *WINDOW* :EDIT NIL *MAKE-KEYBOARD-MACRO-MOVER-COMTAB*))
		     :MACRO-ERROR)
		 (*THROW 'ZWEI-COMMAND-LOOP T))
	    (COPY-BP POINT))
	  (MOVE-BP (POINT) OLD-POINT)
	  (FLUSH-BP OLD-POINT)))))

(DEFUN COM-EXIT-KEYBOARD-MACRO-MOVER ()
  (*THROW 'EXIT-KEYBOARD-MACRO-MOVER T))

(DEFCOM COM-SORT-VIA-KEYBOARD-MACROS "Sort the region alphabetically.
You are prompted to enter three keyboard macros.
 one that moves from the beginning of a sort record to the start of its key,
 one that moves from the start of the key to the end of the key,
 one that moves from the end of the key to the end of the containing record.E
End each macro with C-X ).
These macros are used to divide the region up into records and find their sort keys.
Then the records are rearranged to put their sort keys in alphabetical order." ()
  (REGION (BP1 BP2)
    (WITH-BP (FIRST-BP BP1 :NORMAL)
      (WITH-BP (LAST-BP BP2 :MOVES)
	(SETF (WINDOW-MARK-P *WINDOW*) NIL)
	(MOVE-BP (POINT) FIRST-BP)
	(MUST-REDISPLAY *WINDOW* DIS-BPS)
	(LET ((MOVE-TO-KEY-MACRO (MAKE-KEYBOARD-MACRO-MOVER "move to the start of the sort key"))
	      (MOVE-OVER-KEY-MACRO (MAKE-KEYBOARD-MACRO-MOVER "move over the sort key"))
	      (MOVE-TO-NEXT-MACRO (MAKE-KEYBOARD-MACRO-MOVER "move to the end of the record")))
	  (SORT-INTERVAL-FUNCTIONS MOVE-TO-KEY-MACRO MOVE-OVER-KEY-MACRO MOVE-TO-NEXT-MACRO
				   #'INTERVAL-WITH-SORT-INTERVAL-LESSP FIRST-BP LAST-BP T)))))
  DIS-TEXT)

))

; From file OZ:KANSAS:<L.ZWEI>COMH.LISP.14 at 9-Sep-85 22:29:10
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-COMMENT-OUT-REGION "Stick comment start characters at the start of each line in the region.
Adds regardless of any that may already be there.
A numeric argument specifies how many to add. A negative argument species how many to delete.
An argument of U is treated like -1: it deletes single comment starts." ()
  (IF (EQ *NUMERIC-ARG-P* ':CONTROL-U)
      (SETQ *NUMERIC-ARG* (CLI:// *NUMERIC-ARG* -4)))
  (REGION-LINES (START-LINE END-LINE)
    (IF (> *NUMERIC-ARG* 0)
	(LET* ((LEN (LENGTH *COMMENT-BEGIN*))
	       (INSERT (MAKE-STRING (* *NUMERIC-ARG* LEN))))
	  (DOTIMES (I *NUMERIC-ARG*)
	    (COPY-ARRAY-PORTION *COMMENT-BEGIN* 0 LEN
				INSERT (* I LEN) (+ (* I LEN) LEN)))
	  (DO ((LINE START-LINE (LINE-NEXT LINE))
	       (BP (CREATE-BP START-LINE 0)))
	      ((EQ LINE END-LINE))
	    (MOVE-BP BP LINE 0)
	    (UNLESS (EQ (LINE-TYPE LINE) ':BLANK)
	      (INSERT BP INSERT)))
	  (RETURN-ARRAY (PROG1 INSERT (SETQ INSERT NIL)))))
    (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (BP (CREATE-BP START-LINE 0))
	 (BPA (CREATE-BP START-LINE 1)))
	((EQ LINE END-LINE))
      (DOTIMES (I *NUMERIC-ARG*)
	(IF (OR (EQ (LINE-TYPE LINE) ':BLANK)
		(NOT (STRING-EQUAL LINE *COMMENT-BEGIN* :END1 (LENGTH *COMMENT-BEGIN*))))
	    (RETURN)
	    (MOVE-BP BP LINE 0)
	    (MOVE-BP BPA LINE (LENGTH *COMMENT-BEGIN*))
	    (DELETE-INTERVAL BP BPA T)))))
  DIS-TEXT)

))

; From file OZ:KANSAS:<L.ZWEI>COMH.LISP.14 at 9-Sep-85 22:29:15
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-LISP-MATCH-SEARCH "Move to next occurrence of the given pattern of Lisp code.
In matching, differences in whitespace characters are ignored
 except for characters that are quoted or inside strings.
The characters # as an atom in the pattern match any sexp in the buffer.
The characters ... as an atom in the pattern match any number of sexps.
Patterns starting with infrequent characters such as open parentheses
are found much faster.  Those starting with common letters are likely to be slow.
Patterns starting with very infrequent characters are fastest.

A negative argument means search backwards.
An empty pattern string means repeat the last pattern specified." ()
  (LET ((FORM (TYPEIN-LINE-READLINE "Pattern to search for:")))
    (COND ((EQUAL FORM "")
	   (SETQ FORM (OR *LAST-LISP-MATCH-SEARCH-STRING* (BARF "No previous pattern")))
	   (FORMAT *QUERY-IO* "~&Finding ~S" FORM))
	  (T
	   (SETQ *LAST-LISP-MATCH-SEARCH-STRING* FORM)))
    (LET ((BP (LISP-MATCH-SEARCH (POINT) FORM (MINUSP *NUMERIC-ARG*))))
      (UNLESS BP (BARF))
      (MAYBE-PUSH-POINT BP)
      (MOVE-BP (POINT) BP)))
  DIS-BPS)

))

; From file OZ:KANSAS:<L.ZWEI>COMH.LISP.14 at 9-Sep-85 22:29:22
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFUN LISP-MATCH-SEARCH (BP STRING &OPTIONAL REVERSEP FIXUP-P IGNORE LIMIT-BP
			  &AUX (START 0) (END (LENGTH STRING)))
  "Search from BP for Lisp code that matches against STRING.
Matching at any given place is done with LISP-STRING-BUFFER-MATCH.
Differences in whitespace characters are ignored except when quoted or inside strings.
The characters # as an atom in the STRING match any sexp in the buffer.
The characters ... as an atom in the STRING match any number of sexps.

If a match is found, the value is a bp to the end (start, if reverse) of the matching text.
A second value is a bp to the start (end, if reverse) of the matching text.

REVERSEP means search backward from BP; the code matched must end before BP.
 Otherwise, search goes forward from BP.
LIMIT-BP is a place to stop searching; the matched code cannot
 continue past there in forward search or begin before there in backward search.
FIXUP-P says what to do if no match is found.
 T means return the end of the range to be searched
 (either LIMIT-BP or the beginning or end of the interval).
 NIL means return NIL.  Second value is NIL in either case."
  ;; Ignore leading delimiter chars in STRING.
  (DO () ((NOT (= LIST-DELIMITER (LIST-SYNTAX (CHAR STRING START)))))
    (INCF START))
  ;; Strings that start with ... or # are handled specially.
  (COND ((AND (STRING-EQUAL STRING "..." :START1 START :END1 (+ START 3))
	      (OR (= (+ START 3) END)
		  (NOT (MEMQ (LIST-SYNTAX (CHAR STRING (+ START 3)))
			     '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
	 (BARF "A search pattern starting with ... is not meaningful."))
	((AND (STRING-EQUAL STRING "#" :START1 START :END1 (+ START 1))
	      (OR (= (+ START 2) END)
		  (NOT (MEMQ (LIST-SYNTAX (CHAR STRING (+ START 2)))
			     '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
	 (LET ((TAIL-FOUND (LISP-MATCH-SEARCH (FORWARD-SEXP BP 1 T)
					      (SUBSTRING STRING (+ START 3)))))
	   (OR (AND TAIL-FOUND
		    (FORWARD-SEXP TAIL-FOUND -1 NIL 0 BP T T))
	       (AND FIXUP-P LIMIT-BP))))
	(REVERSEP
	 (LET ((BP1 (COPY-BP BP))
	       (FINAL-LIMIT-BP (INTERVAL-FIRST-BP *INTERVAL*))
	       TEM)
	   (DO-FOREVER
	     (SETQ BP1 (ZWEI-SEARCH BP1 (CHAR STRING START) T NIL NIL
				    (OR LIMIT-BP FINAL-LIMIT-BP)))
	     (UNLESS BP1 (RETURN (IF FIXUP-P (OR LIMIT-BP FINAL-LIMIT-BP) NIL)))
	     (IF (SETQ TEM (LISP-STRING-BUFFER-MATCH BP1 BP STRING START))
		 (RETURN BP1 TEM)))))
	(T
	 (LET ((BP1 (COPY-BP BP))
	       (FINAL-LIMIT-BP (INTERVAL-LAST-BP *INTERVAL*))
	       TEM)
	   (DO-FOREVER
	     (SETQ BP1 (DBP (ZWEI-SEARCH BP1 (CHAR STRING START) NIL T NIL LIMIT-BP)))
	     (IF (SETQ TEM (LISP-STRING-BUFFER-MATCH BP1 (OR LIMIT-BP FINAL-LIMIT-BP)
						     STRING START))
		 (RETURN TEM BP1))
	     (IF (OR (BP-= BP1 LIMIT-BP) (BP-= BP1 FINAL-LIMIT-BP))
		 (RETURN (IF FIXUP-P BP1)))
	     (UNLESS (IBP BP1) (RETURN (IF FIXUP-P BP1 NIL))))))))

))

; From file OZ:KANSAS:<L.ZWEI>COMH.LISP.14 at 9-Sep-85 22:29:34
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFUN LISP-STRING-BUFFER-MATCH (START-BP LIMIT-BP PATTERN-STRING &OPTIONAL (START 0) END)
  "Match part of a string against part of an editor buffer, comparing as Lisp code.
The string is PATTERN-STRING; START and END specify the range to use.
The buffer text starts at START-BP.  It will not match past LIMIT-BP.
If there is a match, the value is a bp to the end of the buffer text matched.
Otherwise, the value is NIL.
Differences in whitespace characters are ignored except when quoted or inside strings.
The characters # as an atom in the PATTERN-STRING match any sexp in the buffer.
The characters ... as an atom in the PATTERN-STRING match any number of sexps."
  (UNLESS END (SETQ END (LENGTH PATTERN-STRING)))
  (DO-NAMED OUTER
	    ((I START (1+ I))
	     (BP (COPY-BP START-BP))
	     IN-STRING QUOTED IN-COMMENT IN-ATOM
	     (P-SYN -1))
	    ((= I END) BP)
    (IF (BP-= LIMIT-BP BP)
	(RETURN NIL))
    (LET* ((S-CHAR (CHAR PATTERN-STRING I))
	   (S-SYN (LIST-SYNTAX S-CHAR)))
      ;; S-SYN is this pattern character's syntax.
      ;; P-SYN is the previous significant pattern character's syntax.
      ;; It is LIST-ALPHABETIC iff the last pattern character, not counting delimiters,
      ;; was such as to be part of an atom.  This is the case in which
      ;; at least one delimiter is required in the buffer in order to match.
      (COND (IN-STRING
	     ;; First update the syntactic state.
	     (COND (QUOTED (SETQ QUOTED NIL))
		   ((= S-SYN LIST-DOUBLE-QUOTE)
		    (SETQ IN-STRING NIL))
		   ((= S-SYN LIST-SLASH)
		    (SETQ QUOTED T)))
	     ;; Now always match against buffer.
	     (UNLESS (EQ S-CHAR (BP-CH-CHARACTER BP))
	       (RETURN NIL))
	     (SETQ P-SYN -1)
	     (IBP BP))
	    (IN-COMMENT
	     (IF (EQ S-CHAR #/RETURN) (SETQ IN-COMMENT NIL))
	     ;; Now always match against buffer.
	     (UNLESS (CHAR-EQUAL S-CHAR (BP-CHARACTER BP))
	       (RETURN NIL))
	     (SETQ P-SYN -1)
	     (IBP BP))
	    (QUOTED
	     (SETQ QUOTED NIL)
	     ;; Quoted char, always match against buffer.
	     (UNLESS (EQ S-CHAR (BP-CH-CHARACTER BP))
	       (RETURN NIL))
	     (SETQ P-SYN LIST-ALPHABETIC)
	     (IBP BP))
	    ;; Not in string or comment, not slashified.
	    ((= S-SYN LIST-DELIMITER)
	     ;; Just skip all delimiters in the pattern.
	     (SETQ IN-ATOM NIL))
	    ((AND (NOT IN-ATOM)
		  ( (+ I 3) END)
		  (STRING-EQUAL PATTERN-STRING "..." :START1 I :END1 (+ I 3))
		  (OR (= (+ I 3) END)
		      (NOT (MEMQ (LIST-SYNTAX (CHAR PATTERN-STRING (+ I 3)))
				 '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
	     ;; "..." has been encountered, and its an atom by itself.
	     (DO-FOREVER
	       ;; Try matching the rest of the pattern at one spot.
	       (LET ((TEM (LISP-STRING-BUFFER-MATCH BP LIMIT-BP PATTERN-STRING (+ I 3) END)))
		 (WHEN TEM
		   (RETURN-FROM OUTER TEM)))
	       ;; SKip one more sexp and try again.
	       (SETQ BP (FORWARD-SEXP BP 1 NIL 0 LIMIT-BP NIL T))
	       (UNLESS BP (RETURN NIL))))
	    ((AND (NOT IN-ATOM)
		  ( (+ I 2) END)
		  (STRING-EQUAL PATTERN-STRING "#" :START1 I :END1 (+ I 1))
		  (OR (= (+ I 2) END)
		      (NOT (MEMQ (LIST-SYNTAX (CHAR PATTERN-STRING (+ I 2)))
				 '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
	     ;; "**" has been encountered as an atom in the pattern.
	     ;; SKip it, and skip one sexp in the buffer, then keep matching.
	     (INCF I)
	     (SETQ BP (FORWARD-SEXP BP 1 NIL 0 LIMIT-BP NIL T))
	     (SETQ P-SYN -1)
	     (UNLESS BP (RETURN NIL)))
	    (T
	     ;; Skip all delimiters here in the buffer, if not within an atom.
	     (UNLESS IN-ATOM
	       (DO ((COUNT 0 (1+ COUNT)))  ;Count number of delimiters skipped.
		   ((NOT (= LIST-DELIMITER (LIST-SYNTAX (BP-CHARACTER BP))))
		    (AND (ZEROP COUNT)
			 (= S-SYN LIST-ALPHABETIC)
			 (= P-SYN LIST-ALPHABETIC)
			 (RETURN-FROM OUTER NIL)))
		 (IBP BP)))
	     ;; Set up syntax context of next pattern character.
	     (SELECT S-SYN
	       (LIST-DOUBLE-QUOTE
		(SETQ IN-STRING T))
	       (LIST-SLASH
		(SETQ QUOTED T))
	       (LIST-COMMENT
		(SETQ IN-COMMENT T))
	       (LIST-ALPHABETIC
		(SETQ IN-ATOM T)))
	     (IF (EQ S-CHAR #/.) (SETQ IN-ATOM T))
	     ;; Now always match against buffer.
	     (UNLESS (CHAR-EQUAL S-CHAR (BP-CHARACTER BP))
	       (RETURN NIL))
	     (IBP BP)
	     (SETQ P-SYN S-SYN))))))

))

; From file OZ:KANSAS:<L.ZWEI>COMS.LISP.86 at 9-Sep-85 22:33:49
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-REPLACE-STRING "Replace all occurrences of a given string with another.
Prompts for two string: to replace all FOO's with BAR's, type FOO and BAR.
With no numeric arg, all occurrences after point are replaced.
With numeric arg, that many occurrences are replaced.
If *CASE-REPLACE* is non-null, BAR's initial will be capitalized
if FOO's initial had been (supply it in lower case)." ()
  (LET ((FROM (TYPEIN-LINE-READLINE
		"Replace ~:[all~*~;next ~D~] occurrences ~:[in the region ~]of:"
		*NUMERIC-ARG-P* *NUMERIC-ARG* (NOT (WINDOW-MARK-P *WINDOW*)))))
    (IF (ZEROP (STRING-LENGTH FROM))
	(BARF "The string may not be null.")
      (LET ((TO (LET ((*MINI-BUFFER-DEFAULT-STRING* FROM))
		  (TYPEIN-LINE-READLINE
		    "Replace ~:[all~*~;next ~D~] occurrences ~:[in the region ~]of /"~A/" with:"
		    *NUMERIC-ARG-P* *NUMERIC-ARG* (NOT (WINDOW-MARK-P *WINDOW*)) FROM))))
	(WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
	  (FORMAT *QUERY-IO* "~&~D. replacement~:P made."
		  (REPLACE-STRING (POINT) FROM TO (AND *NUMERIC-ARG-P*
						       *NUMERIC-ARG*))))))
    DIS-TEXT))

))

; From file OZ:KANSAS:<L.ZWEI>COMS.LISP.86 at 9-Sep-85 22:33:53
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-QUERY-REPLACE "Replace string, asking about each occurrence.
Prompts for each string.  If you first give it FOO, then BAR, it
finds the first FOO, displays, and
reads a character.  Space => replace it with BAR and show next FOO.
Rubout => don't replace, but show next FOO.
Comma => replace this FOO and show result, waiting for a
space, R or Altmode.
Period => replace this FOO and exit.  Altmode => just exit.
^ => return to site of previous FOO (actually, pop the point pdl).
W => kill this FOO and enter recursive edit.
R => enter editing mode recursively.  L => redisplay screen.
Exclamation mark => replace all remaining FOOs without asking.
Any other character exits and (except altmode) is read again.
If *CASE-REPLACE* is non-null, BAR's initial will be capitalized
if FOO's initial had been.
If you give a numeric argument, it will not consider FOOs that are not
bounded on both sides by delimiter characters." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (LET ((START-BP (POINT))
	  (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
	(UNLESS (BP-< (POINT) (MARK))
	  (SWAP-BPS (POINT) (MARK)))
	(SETQ END-BP (MARK)))
      (QUERY-REPLACE START-BP END-BP FROM TO *NUMERIC-ARG-P*)))
  DIS-TEXT)

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:09:55
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFMAJOR COM-DIRED-MODE DIRED-MODE "Dired" "Setup for editing a directory" ()
  (PROGN (LET ((PATHNAME (SEND *INTERVAL* :PATHNAME)))
	   (SETQ *DIRED-PATHNAME-NAME* (AND PATHNAME (STRING PATHNAME)))))
  (SET-COMTAB *MODE-COMTAB* '(#/SP COM-DOWN-REAL-LINE
			      #/! COM-DIRED-NEXT-UNDUMPED
			      #/@ COM-DIRED-COMPLEMENT-DONT-DELETE
			      #/# COM-DIRED-COMPLEMENT-DONT-SUPERSEDE
			      #/$ COM-DIRED-COMPLEMENT-NO-REAP-FLAG
			      #/. COM-DIRED-CHANGE-FILE-PROPERTIES
			      #/, COM-DIRED-PRINT-FILE-ATTRIBUTES
			      #/= COM-DIRED-SRCCOM
			      #/? COM-DIRED-DOCUMENTATION
			      #/HELP COM-DIRED-DOCUMENTATION
			      #/A COM-DIRED-APPLY-FUNCTION
			      #/a (0 #/A)
			      #/C COM-DIRED-COPY
			      #/c (0 #/C)
			      #/D COM-DIRED-DELETE
			      #/d (0 #/D)
			      #/C-D COM-DIRED-DELETE
			      #/E COM-DIRED-EDIT-FILE
			      #/e (0 #/E)
			      #/C-SH-E COM-DIRED-EDIT-FILE-TWO-WINDOWS
			      #/F COM-DIRED-FIND-FILE
			      #/f (0 #/F)
			      #/H COM-DIRED-AUTOMATIC
			      #/h (0 #/H)
			      #/K COM-DIRED-DELETE
			      #/k (0 #/K)
			      #/C-K COM-DIRED-DELETE
			      #/L COM-DIRED-LOAD-FILE
			      #/l (0 #/L)
			      #/N COM-DIRED-NEXT-HOG
			      #/n (0 #/N)
			      #/P COM-DIRED-PRINT-FILE
			      #/p (0 #/P)
			      #/Q COM-DIRED-EXIT
			      #/q (0 #/Q)
			      #/R COM-DIRED-RENAME
			      #/r (0 #/R)
			      #/S COM-DIRED-SUBDIRECTORY
			      #/s (0 #/S)
			      #/U COM-DIRED-UNDELETE
			      #/u (0 #/U)
			      #/V COM-DIRED-VIEW-FILE
			      #/v (0 #/V)
			      #/X COM-DIRED-EXECUTE
			      #/x (0 #/X)
			      #/1 COM-NUMBERS
			      #/2 COM-NUMBERS
			      #/3 COM-NUMBERS
			      #/4 COM-NUMBERS
			      #/5 COM-NUMBERS
			      #/6 COM-NUMBERS
			      #/7 COM-NUMBERS
			      #/8 COM-NUMBERS
			      #/9 COM-NUMBERS
			      #/0 COM-NUMBERS
			      #/< COM-DIRED-EDIT-SUPERIOR-DIRECTORY
			      #/RUBOUT COM-DIRED-REVERSE-UNDELETE
			      #/ABORT COM-DIRED-ABORT
			      #/END COM-DIRED-EXIT
			      #/MOUSE-3-1 COM-DIRED-MOUSE-MENU)
	      '(("Automatic" . COM-DIRED-AUTOMATIC)
		("Automatic All Files" . COM-DIRED-AUTOMATIC-ALL)
		("Sort Increasing Reference Date"
		 . COM-DIRED-SORT-BY-INCREASING-REFERENCE-DATE)
		("Sort Decreasing Reference Date"
		 . COM-DIRED-SORT-BY-DECREASING-REFERENCE-DATE)
		("Sort Increasing Creation Date"
		 . COM-DIRED-SORT-BY-INCREASING-CREATION-DATE)
		("Sort Decreasing Creation Date"
		 . COM-DIRED-SORT-BY-DECREASING-CREATION-DATE)
		("Sort Increasing File Name"
		 . COM-DIRED-SORT-BY-INCREASING-FILE-NAME)
		("Sort Decreasing File Name"
		 . COM-DIRED-SORT-BY-DECREASING-FILE-NAME)
		("Sort Increasing Size"
		 . COM-DIRED-SORT-BY-INCREASING-SIZE)
		("Sort Decreasing Size"
		 . COM-DIRED-SORT-BY-DECREASING-SIZE)))
  (SET-MODE-LINE-LIST (APPEND (MODE-LINE-LIST) '("  " *DIRED-PATHNAME-NAME*
						 "     (Q to exit)"))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:10:17
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-SUBDIRECTORY "Insert or remove the files of this subdirectory.
The files in the subdirectory mentioned on this line
are inserted into the DIRED buffer underneath this line.
You can then delete them, rename them, etc.
The subdirectory files are indented one additional space.
If the subdirectory contents are already present in the DIRED buffer,
this command offers to remove them from the buffer.
Removing them from the buffer does not delete the files!
It only makes DIRED stop operating on them.

With an argument, prompts for a wildcarded specification of files in the
directory to insert, rather than inserting them all, which is the default. " ()
  (let* ((line (bp-line (point)))
	 (wild-pathname (getf (line-plist line) 'contents-present))
	 (pathname (getf (line-plist line) ':pathname))
	 directory wild-directory)
    (unless (getf (line-plist line) ':directory)
      (barf "~A is not a directory" pathname))
    (setq directory (send pathname :pathname-as-directory)
	  wild-directory (send directory :new-pathname :name :wild :type :wild :version :wild))
    (cond ;; no arg and presently there => close subdir
	  ((and wild-pathname
		(not *numeric-arg-p*)
		(fquery () "Remove subfiles ~A ?" wild-pathname))
	   (dired-close-line-subdirectory line))
	  ;; arg means selective insert of subdir, deleting old contents if present
	  (*numeric-arg-p*
	   (or wild-pathname (setq wild-pathname wild-directory))
	   (setq wild-pathname (read-directory-name
				 (format nil "Edit which subfiles of directory ~A"
					 wild-directory)
				 wild-pathname))
	   (unless (send wild-directory :pathname-match wild-pathname)
	     (format *query-io* "~&~A does not seem to specify any subfile of ~A"
		     wild-pathname wild-directory)
	     (beep)
	     (if (y-or-n-p "Start a separate dired of ~A ?")
		 (directory-edit wild-pathname t)
	       (return-from com-dired-subdirectory dis-none)))	       
	   (if (getf (line-plist line) 'contents-present)
	       (dired-close-line-subdirectory line))
	   (dired-open-line-subdirectory line wild-pathname))
	  ;; no arg and not there => insert *.*.* subdir
	  (t
	   (dired-open-line-subdirectory line wild-directory))))
  dis-text)

(DEFUN DIRED-OPEN-SUBDIRECTORY (PATHNAME)
  "Add the files in the subdirectory PATHNAME to the dired buffer.
Does nothing if that subdirectory is not itself present.
PATHNAME should be the pathname of the file which is the subdirectory."
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (END-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))) WILD-PATHNAME)
      ((EQ LINE END-LINE) NIL)
    (WHEN (EQ PATHNAME (DIRED-LINE-PATHNAME LINE))
      (UNLESS (SETQ WILD-PATHNAME (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
	(DIRED-OPEN-LINE-SUBDIRECTORY LINE WILD-PATHNAME))
      (RETURN T))))

(DEFUN DIRED-OPEN-LINE-SUBDIRECTORY (LINE WILD-PATHNAME &AUX DIRECTORY)
  (IF (SETQ DIRECTORY (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
      (BARF "Subfiles ~A are already present" DIRECTORY)
    (UNLESS (PATHNAMEP WILD-PATHNAME)
      (SETQ WILD-PATHNAME (SEND (SEND (DIRED-LINE-PATHNAME-OR-BARF LINE)
				      :PATHNAME-AS-DIRECTORY)
				:NEW-PATHNAME :NAME :WILD :TYPE :WILD :VERSION :WILD)))
    (SETQ DIRECTORY (FS:DIRECTORY-LIST WILD-PATHNAME :DELETED :SORTED))
    (LET* ((*BATCH-UNDO-SAVE* T))
      (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	(SETF (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT) WILD-PATHNAME)
	(LET ((NEXT-PLIST (LINE-PLIST (LINE-NEXT LINE)))
	      (STREAM (INTERVAL-STREAM-INTO-BP (CREATE-BP (LINE-NEXT LINE) 0))))
	  (DIRED-INSERT-DIRECTORY DIRECTORY STREAM
				  (1+ (DIRED-LINE-LEVEL LINE)))
	  ;; Restore the plist, now clobbered, of the following line.
	  (SETF (LINE-PLIST (BP-LINE (SEND STREAM :READ-BP))) NEXT-PLIST))))))

(defun dired-close-line-subdirectory (line)
  (let* ((*batch-undo-save* t)
	 (wild-pathname (getf (line-plist line) 'contents-present)))
    (if (null wild-pathname)
	(barf "No subfiles are present")
      (with-read-only-suppressed (*interval*)
	(setf (getf (line-plist line) 'contents-present) nil)
	(do ((line2 (line-next line) (line-next line2))
	     (thislevel (dired-line-level line)))
	    ((let ((linelevel (dired-line-level line2)))
	       (or (null linelevel)
		   ( linelevel thislevel)))
	     (delete-interval (create-bp (line-next line) 0)
			      (create-bp line2 0)
			      t)))))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:10:21
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFVAR *DIRED-SUBDIRECTORY-INDENTATION* 2
  "The number of spaces inserted in front of the files of a subdirectory in dired.")

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:10:32
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-INSERT-DIRECTORY (DIRECTORY STREAM LEVEL)
  "Insert into a DIRED buffer lines describing the files in DIRECTORY.
DIRECTORY is a value returned by FS:DIRECTORY-LIST.
STREAM is a stream outputting into the DIRED buffer.
LEVEL is the depth in subdirectories of these files.
Returns the first inserted line that describes a file."
  ;; Mark all files that are the newest
  (DIRED-COMPUTE-GREATER-THANS (CDR DIRECTORY))
  (DO ((FILES DIRECTORY (CDR FILES))
       (FILE)
       (LINE) (FIRST-FILE-LINE))
      ((NULL FILES)
       FIRST-FILE-LINE)
    (SETQ FILE (CAR FILES))
    (UNLESS (NULL (CAR FILE))
      (IF (GET FILE :DIRECTORY)
	  (LET ((STR (SEND (SEND (SEND (CAR FILE)
				       :NEW-PATHNAME :DEVICE NIL
				       		     ;; Get rid of the version iff this is the newest one.
						     :VERSION (IF (GET FILE ':NEWEST) NIL
								(SEND (CAR FILE) :VERSION)))
				 :PATHNAME-AS-DIRECTORY)
			   :STRING-FOR-DIRECTORY)))
	    ;; STR has the string we want to print instead of the filename.
	    ;; Replace (CAR FILE) with a phony "pathname" that will print as that string.
	    (WITH-STACK-LIST* (FILE1 #'(LAMBDA (&REST IGNORE) STR) (CDR FILE))
	      (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE1 STREAM)))
	(FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE STREAM))
      (SETQ LINE (LINE-PREVIOUS (BP-LINE (SEND STREAM :READ-BP))))
      (INSERT-CHARS (CREATE-BP LINE 5) #/SPACE (* *DIRED-SUBDIRECTORY-INDENTATION* LEVEL))
      ;; Use lower-case "d" to mark already-deleted files.
      (IF (GET FILE ':DELETED)
	  (SETF (CHAR LINE 0) #/d))
      (OR FIRST-FILE-LINE
	  (SETQ FIRST-FILE-LINE LINE))
      (SETF (GETF (LINE-PLIST LINE) 'LEVEL) LEVEL)
      (LOOP FOR (PROP VAL) ON (CDR FILE) BY 'CDDR
	 DO (SETF (GETF (LINE-PLIST LINE) PROP) VAL))
      (SETF (GETF (LINE-PLIST LINE) ':PATHNAME) (CAR FILE)))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:10:55
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-REGENERATE-LINE (LINE &AUX (PLIST (LOCF (LINE-PLIST LINE)))
			      (PATHNAME (GET PLIST :PATHNAME)))
  "Restore the contents of LINE from the data in its properties."
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (LET ((CH (IF (ZEROP (STRING-LENGTH LINE))
		  #/SP
		  (CHAR LINE 0)))
	  (FILE (CONS PATHNAME (CDR PLIST))))
      (SETF (LINE-LENGTH LINE) 0)
      (WITH-OUTPUT-TO-STRING (S LINE)
;	(IF (GET FILE :DIRECTORY)
;	    (LET ((STR (SEND (SEND (SEND (CAR FILE) :PATHNAME-AS-DIRECTORY)
;				   :NEW-PATHNAME :NAME NIL :TYPE NIL :DEVICE NIL)
;			     :STRING-FOR-PRINTING)))
;	      (SEND S :STRING-OUT "      ")
;	      (SEND S :STRING-OUT STR (1+ (STRING-SEARCH-CHAR #/: STR))))
	  (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE S))
;     )
      (OR (GET FILE ':DIRECTORY)
	  ;; Eliminate the Newline which the lister writes.
	  (DECF (LINE-LENGTH LINE)))
      (INSERT-CHARS (CREATE-BP LINE 5) #/SPACE
		    (* *DIRED-SUBDIRECTORY-INDENTATION* (GET FILE 'LEVEL)))
      (SETF (CHAR LINE 0) CH))
    (MUNG-LINE LINE)))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:11:18
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-HELP "Explain use of DIRED commands" ()
  (FORMAT T "You are in the directory editor.  The commands are:
	D (or K, c-D, c-K)  Mark the current file for deletion.
	P	Print the current file on the standard hardcopy device.
	A	Queue this file for function application.
	U	Undelete the current file, or else the file just above the cursor.
		Also used to cancel a Print or Apply function request.
	R	Rename this file.  You type the new filename in a mini buffer.
	C	Copy this file.  You type the new filename in a mini buffer.
	L	Load this file (lisp code or QFASL file).
	Rubout	Undelete file above the cursor.
	Space	Move to the next line.
	  Above commands repeat with a numeric argument,
	  backwards if the argument is negative.	  
	S	Insert the contents of this Subdirectory.
	        The files in the subdirectory are indented ~R additional space~:P.
		By default it inserts all the files of the subdirectory; however
		 by giving this command a numeric argument you will be prompted
		 for a wildcarded pathname specifying a subset of the subdirectory's
		 contents.
		If the subdirectory files are already inserted, then S with no
		 argument command offers to remove them from the display.
		Removing them from the display does NOT delete the files!
	N	Move to the next file with more than ~D versions.
		 (This number /"~:*~D/" is the value of ~S)
	H	Mark excess versions of the current file for deletion.
	Q	Exit.  You will be shown the files to be deleted and asked for
		confirmation.  In this display /":/" means a link, /">/" means
		this is the highest version-number of this file, /"!/" means
		not backed-up, and /"$/" means not to be reaped, please.
	X	Execute.  Perform requested file deletions, etc.,
		but stay in the DIRED buffer afterwards.
	!	Move to the next file that is not backed up on tape.
	@	Complement @ flag (dont-delete)
	#	Complement # flag (dont-supersede)
	$	Complement $ flag (dont-reap)
	,	Print the attributes of a file.  For a source file, the -*- line.
		For a QFASL file, the compilation data and what is recorded
		 of the source file's -*- line.
	.	Change properties of current file.
	E	Edit the current file, or DIRED on subdirectory.
	F	Edit current file or subdirectory, not now, but when you exit.
	C-Sh-E	Edit the current file in another window.  The DIRED remains visible.
		 Enters two window mode if you are in one window mode.
        <       DIRED on the superior directory of this directory.
	V	View the current file (doesn't read it all in).
	=	SRCCOM this file with the > version.

Clicking the right-hand button on the mouse will give you a menu
of commands to operate on the line the mouse is pointing at.

Sorting commands which sort the DIRED buffer:

M-X Sort Increasing File Name
M-X Sort Increasing Creation Date
M-X Sort Increasing Reference Date
M-X Sort Increasing Size
and their counterparts with Decreasing instead of Increasing.
"
	  *DIRED-SUBDIRECTORY-INDENTATION*
	  *FILE-VERSIONS-KEPT* '*FILE-VERSIONS-KEPT*
	  )
  DIS-NONE)

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:12:20
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-PROCESS-FILES ()
  "Perform all the operations requested on files in the DIRED buffer.
Returns T if user typed E or Y or Q, NIL if user typed N."
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (UNDELETEABLE (SEND (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*) :UNDELETABLE-P))
       DELETE-FILES
       UNDELETE-FILES
       FIND-FILES
       PRINT-FILES
       APPLY-FILES
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
       QUERY-RESULT)
      ((EQ LINE LAST-LINE)
       (SETQ DELETE-FILES (NREVERSE DELETE-FILES)
	     UNDELETE-FILES (NREVERSE UNDELETE-FILES)
	     FIND-FILES (NREVERSE FIND-FILES)
	     PRINT-FILES (NREVERSE PRINT-FILES)
	     APPLY-FILES (NREVERSE APPLY-FILES))
       (CATCH 'RETURN-TO-DIRED (PROGN	 
	 (COND ((OR DELETE-FILES UNDELETE-FILES FIND-FILES PRINT-FILES APPLY-FILES)
		(AND DELETE-FILES (DIRED-PRINT-FILE-LIST DELETE-FILES "deleted"))
		(AND UNDELETE-FILES (DIRED-PRINT-FILE-LIST UNDELETE-FILES "undeleted"))
		(AND FIND-FILES (DIRED-PRINT-FILE-LIST FIND-FILES "visited"))
		(AND PRINT-FILES (DIRED-PRINT-FILE-LIST PRINT-FILES "printed"))
		(AND APPLY-FILES (DIRED-PRINT-FILE-LIST APPLY-FILES "processed by function"))
		(COND ((SETQ QUERY-RESULT
			     (DIRED-FILE-QUERY UNDELETEABLE
					       (AND DELETE-FILES "Delete")
					       (AND UNDELETE-FILES "Undelete")
					       (AND FIND-FILES "Visit")
					       (AND PRINT-FILES "Print")
					       (AND APPLY-FILES "Apply function")))
		       (COND (APPLY-FILES
			      ;This crock to fake out read-function-name.
			      ;Mouse would not win particularily.
			      (LET* ((*MINI-BUFFER-REPEATED-COMMAND* '())
				     *DIRED-FUNCTION-TO-APPLY*)
				(MULTIPLE-VALUE-BIND (FNSPEC STRING)
				    (READ-FUNCTION-NAME "Function to apply:" 'COMPILE-FILE)
				  (SETQ *DIRED-FUNCTION-TO-APPLY*
					(COND ((FDEFINEDP FNSPEC) FNSPEC)
					      (T (CONDITION-CASE ()
						     (CLI:READ-FROM-STRING STRING)
						   (SYS:END-OF-FILE
						    (BARF)))))))
				(DIRED-DO-FILE-LIST APPLY-FILES
						    'DIRED-APPLY-FUNCTION NIL))))
		       (AND DELETE-FILES
			    (DIRED-DO-FILE-LIST DELETE-FILES 'DIRED-DELETE-FILE "delete"
						:DELETE-MULTIPLE-FILES
						#'(LAMBDA (LINE)
						    (SETF (GETF (LINE-PLIST LINE) ':DELETED) T))))
		       (AND UNDELETE-FILES
			    (DIRED-DO-FILE-LIST UNDELETE-FILES 'DIRED-UNDELETE-FILE
						"undelete"
						:UNDELETE-MULTIPLE-FILES
						#'(LAMBDA (LINE)
						    (SETF (GETF (LINE-PLIST LINE) ':DELETED) NIL))))
		       (AND FIND-FILES
			    (DIRED-DO-FILE-LIST FIND-FILES 'DIRED-FIND-FILE "visit"))
		       (AND PRINT-FILES
			    (DIRED-DO-FILE-LIST PRINT-FILES 'DIRED-PRINT-FILE "print"))
		       ;; Expunge if desired.
		       (WHEN (EQ QUERY-RESULT :EXPUNGE)
			 (LET ((BLOCKS-FREED 0))
			   ;; Expunge the directory we did DIRED on.
			   (INCF BLOCKS-FREED
				 (FS:EXPUNGE-DIRECTORY
				   (DIRED-BUFFER-DIRECTORY-PATHNAME *INTERVAL*)))
			   ;; Expunge any subdirectories whose contents are listed.
			   (DO ((LINE (LINE-NEXT (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
				      (LINE-NEXT LINE)))
			       ((NULL (LINE-NEXT LINE)))
			     (WHEN (AND (GETF (LINE-PLIST LINE) :DIRECTORY)
					(GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
			       (INCF BLOCKS-FREED
				     (FS:EXPUNGE-DIRECTORY
				       (SEND (DIRED-LINE-PATHNAME LINE)
					     :PATHNAME-AS-DIRECTORY)))))
			   (FORMAT *QUERY-IO* "~&~D blocks freed." BLOCKS-FREED)))
		       ;; If the deleted files are now gone for good,
		       ;; delete their lines from the buffer.
		       ;; Also, flush any U's, A's, F's, or P's.
		       (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
			 (DO ((LINE (LINE-NEXT (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
				    (LINE-NEXT LINE)))
			     ((NULL (LINE-NEXT LINE)))
			   (COND ((= (LENGTH LINE) 0))
				 ((CHAR-EQUAL (CHAR LINE 0) #/D)
				  (IF (OR (EQ QUERY-RESULT :EXPUNGE)
					  (NOT UNDELETEABLE))
				      (DELETE-INTERVAL (BEG-OF-LINE LINE)
						       (BEG-OF-LINE (LINE-NEXT LINE))
						       T)
				    (MUNG-LINE LINE)
				    (SETF (CHAR LINE 0) #/d)))
				 ((CHAR (CHAR LINE 0) #/SP)
				  (MUNG-LINE LINE)
				  (SETF (CHAR LINE 0) #/SP)))))))))
	 (RETURN-FROM DIRED-PROCESS-FILES T))))
    (WHEN (DIRED-LINE-PATHNAME LINE)
      (CASE (CHAR LINE 0)
	(#/D (PUSH LINE DELETE-FILES))
	(#/U (PUSH LINE UNDELETE-FILES))
	(#/F (PUSH LINE FIND-FILES))
	(#/P (PUSH LINE PRINT-FILES))
	(#/A (PUSH LINE APPLY-FILES))))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:12:36
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-DO-FILE-LIST (FILES FUNCTION NAME &OPTIONAL MULTIPLE-FILE-MESSAGE AUXILIARY-FUNCTION
			   &AUX ERR PATHS)
;; Added AUXILIARY-FUNCTION which is called for each file in FILES when the multiple-file
;; path is used.  This is so delete/undelete can pass in a function to update the plist
;; on each line.  1/2/85 KHS.
  (COND ((AND MULTIPLE-FILE-MESSAGE
	      (SEND (DIRED-LINE-PATHNAME (CAR FILES)) :OPERATION-HANDLED-P MULTIPLE-FILE-MESSAGE))
	 (SETQ PATHS (MAPCAR #'DIRED-LINE-PATHNAME FILES))
	 (SETQ ERR (SEND (CAR PATHS) MULTIPLE-FILE-MESSAGE
			 NIL			;error-p
			 PATHS))
	 (AND AUXILIARY-FUNCTION
	      (NOT (ERRORP ERR))
	      (MAPC AUXILIARY-FUNCTION FILES))
	 (AND NAME (ERRORP ERR)
	      (DIRED-REPORT-ERROR NAME "files" ERR))
	 (AND NAME (CONSP ERR)
	      (MAPC #'(LAMBDA (PATHNAME ERROR)
			(AND (ERRORP ERROR)
			     (DIRED-REPORT-ERROR NAME PATHNAME ERROR)))
		    PATHS ERR)))
	(T (DOLIST (LINE FILES)
	     (SETQ ERR (FUNCALL FUNCTION LINE))
	     (AND NAME
		  (ERRORP ERR)
		  (DIRED-REPORT-ERROR NAME (DIRED-LINE-PATHNAME LINE) ERR))))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.308 at 9-Sep-85 23:13:13
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN PARSE-BUG-ARG (WHO)
  (VALUES (STRING-APPEND "BUG-" WHO #/@ *HOST-FOR-BUG-REPORTS*)
	  (LET ((S (FORMAT NIL "In~:[ ~A in~;~*~] ~A, on ~A (~A):~2%"
			       (STRING-EQUAL WHO "LISPM") WHO
			       (SI:SYSTEM-VERSION-INFO)
			       SI:LOCAL-PRETTY-HOST-NAME
			       (machine-type))))
	    ;; Fill to fit within a 75-column line
	    (LOOP WITH LINE-START = 0
		  FOR START = 0 THEN (+ COMMA-POS 2)
		  AS PREV-COMMA-POS = NIL THEN COMMA-POS
		  AS COMMA-POS = (STRING-SEARCH ", " S START)
	       WHEN (> (- (OR COMMA-POS (STRING-LENGTH S)) LINE-START) 72.)
	         UNLESS (NULL PREV-COMMA-POS)
		   DO (SETF (CHAR S (1+ PREV-COMMA-POS)) #/NEWLINE)
		      (when (> prev-comma-pos line-start)
			(SETQ LINE-START (+ PREV-COMMA-POS 2))
			(SETQ COMMA-POS PREV-COMMA-POS))
	       UNTIL (NULL COMMA-POS))
	    S)))

))

; From buffer doc at 10-Sep-85 02:04:39
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFCONST *COM-DOCUMENTATION-ALIST*
	  '((#/C COM-SELF-DOCUMENT)
	    (#/D COM-DESCRIBE-COMMAND)
	    (#/L COM-WHAT-LOSSAGE
	     (SEND *STANDARD-INPUT* :OPERATION-HANDLED-P :PLAYBACK))
	    (#/A COM-APROPOS)
	    (#/U COM-UNDO)
	    (#/V COM-VARIABLE-APROPOS)
	    (#/W COM-WHERE-IS))
  "Alist defining Help options.
Each element is (CHARACTER COMMAND COND-FORM).
The option is available only if COND-FORM evals non-NIL
 (but if the list has only two elements, the option is unconditional).
COMMAND is what is run to do the work.  It should be a DEFCOM name.")

))

; From buffer doc at 10-Sep-85 02:04:48
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFCOM COM-DOCUMENTATION "Run a specified documentation command.
You type a character.  To find out what a certain character does, type C and that character.
To find out what a named command does, type D and the command name.
To find all commands whose names contain a certain substring,
  type A and then the substring.
To find out the last 60 characters you typed, if you are confused, type L.
More advanced options:
   U - Undo; V - run Variable Apropos; W - run Where Is;
   SPACE repeats the previous documentation request, if any." ()
  (DO ((CHAR 0)
       (*IN-COM-DOC-P* T)
       (*REPEAT-DOC-P* NIL))
      (NIL)
    ;; Print a list of available options.
    (FORMAT *QUERY-IO* "~&Help.  Options are ")
    (DOLIST (ELT *COM-DOCUMENTATION-ALIST*)
      (IF (OR (NULL (THIRD ELT))
	      (EVAL (THIRD ELT)))
	  (FORMAT *QUERY-IO* "~C," (CAR ELT))))
    (FORMAT *QUERY-IO* "~\LOZENGED-CHARACTER\,~\LOZENGED-CHARACTER\: " #/SPACE #/HELP)
    ;; Read input chars till we get a valid one.
    (TYPEIN-LINE-ACTIVATE
      (SETQ CHAR (DO ((CHAR (CHAR-UPCASE (READ-CHAR *STANDARD-INPUT*))
			    (CHAR-UPCASE (READ-CHAR *STANDARD-INPUT*))))
		     ((OR (ASSQ CHAR *COM-DOCUMENTATION-ALIST*)
;character lossage
			  (ASSQ (CHAR-INT CHAR) *COM-DOCUMENTATION-ALIST*)
			  (MEMQ CHAR '(#/SP #/HELP)))
		      CHAR)
		   (WHEN (MEMQ CHAR '(#/C-G #/RUBOUT))
		     (SEND *QUERY-IO* :MAKE-COMPLETE)
		     (BARF))
		   (BEEP))))
    (SEND *QUERY-IO* :MAKE-COMPLETE)
    ;; Execute the character we got.
    (COND ((EQL CHAR #/SPACE)
	   (SETQ *REPEAT-DOC-P* T)
	   (SETQ CHAR *COM-DOC-LAST-CHAR*))
	  (T (SETQ *COM-DOC-LAST-CHAR* CHAR)))
    (IF (MEMQ CHAR '(#/? #/HELP))
	(PROGN (FORMAT T "You have entered the Documentation command.~%")
	       (PRINT-DOC :FULL 'COM-DOCUMENTATION *LAST-COMMAND-CHAR*))
      (LET ((FUNCTION (CADR (OR (ASSQ CHAR *COM-DOCUMENTATION-ALIST*)
;character lossage
				(ASSQ (CHAR-INT CHAR) *COM-DOCUMENTATION-ALIST*)))))
	(AND FUNCTION (RETURN (FUNCALL FUNCTION)))))))

))

; From buffer doc at 10-Sep-85 02:05:12
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "


(DEFCOM COM-APROPOS "List commands whose names contain a given string.
Tell on which key(s) each command can be found.
Leading and trailing spaces in the substring are NOT ignored - they
must be matched by spaces in the command name." ()
  (apropos-internal t "Command Apropos. (Substring:)"))

(defcom com-current-mode-apropos
  "List currently accessible commands whose names contain a given string." ()
  (apropos-internal nil "Accessible commands Apropos. (Substring:)"))

(defun apropos-internal (use-any-extended-command-p prompt)
  (multiple-value-bind (function key)
      (get-extended-search-strings prompt)
    (let ((extended-cmd (key-for-command *extended-command-command*))
	  (any-extended-cmd (key-for-command *any-extended-command-command*)))
      (dolist (x *command-alist*)
	(let* ((cmd (cdr x))
	       (name (command-name cmd))
	       (type 'any-extended))
	  (flet ((document-command ()
		   (format t "~&~30,5,2A" name)
		   (print-doc :short cmd)
		   (send *terminal-io* :fresh-line)))
	    (when (funcall function key name)
	      (and (key-for-command cmd) ; just use for predicate here...
		   (setq type 'key))
	      (and (extended-command-p cmd)
		   (setq type 'extended))
	      (case type
		(key
		 (document-command)
		 (when (> (find-command-on-keys cmd 4 "  which can be invoked via: ") 0)
		   (terpri)))
		(extended
		 (document-command)
		 (format t "  which can be invoked via: ~A ~A~%" extended-cmd name))
		(any-extended
		 (when use-any-extended-command-p
		   (document-command)
		   (format t "  which can be invoked via: ~A ~A~%" any-extended-cmd name))))))))))
  (format t "~%Done.~%")
  dis-none)

))
