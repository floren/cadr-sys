;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.26
;;; Reason: rh options. New option: :editing-command.
;;;   tv:stream-mixin operations :read-bp :force-rescan :rescanning-p
;;; setf on cons, list, list* -- by rms
;;; Dribble stream rubout bug
;;; Written 3-Jan-84 22:04:45 by Mly,
;;; while running on Lisp Machine Nine from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.24, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file FQUERY.LISP SRC:<L.IO1> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FQUERY  "

(DEFSELECT READLINE-FQUERY-FUNCTION
  (:READ (STREAM &AUX STRING)
    (SETQ STRING (FUNCALL STREAM ':RUBOUT-HANDLER '((:EDITING-COMMAND #\HELP)	;Just in case
						    (:PROMPT FQUERY-PROMPT)
						    (:DONT-SAVE T))
			  #'FQUERY-READLINE-WITH-HELP STREAM))
    (STRING-TRIM '(#\SP) STRING))
  (:ECHO (ECHO STREAM)
    ECHO STREAM)
  (:MEMBER (STRING LIST)
    (MEM #'STRING-EQUAL STRING LIST)))

))

; From file CONVER.LISP SRC:<L.IO1> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; CONVER  "

(DEFUN QSEND-GET-MESSAGE (&OPTIONAL (STREAM STANDARD-INPUT) IGNORE END-WITH-RETURN-OK)
  (IF (AND (NOT RUBOUT-HANDLER)
	   (MEMQ ':RUBOUT-HANDLER (SEND STREAM ':WHICH-OPERATIONS)))
      (SEND STREAM ':RUBOUT-HANDLER
	       '((:EDITING-COMMAND #\END #\C-Z #\C-C
				   (#\C-M-Y "Yank last msg received")
				   (#\C-M-E "Switch to Converse")))
	       #'QSEND-GET-MESSAGE STREAM NIL END-WITH-RETURN-OK)
      (DO ((MSG (MAKE-ARRAY 100 ':TYPE 'ART-STRING ':LEADER-LIST '(0)))
	   (CH))
	  (NIL) ;;consider efficiency hack for non-control characters
	(SETQ CH (SEND STREAM ':TYI))
	(AND
	  (OR (AND (MEMQ CH '(#\END #\C-Z #\C-C NIL)))
	      (AND END-WITH-RETURN-OK (EQ CH #\RETURN))) ;;why doesn't this work?
	  (RETURN MSG))
	(AND (EQ CH #\C-M-E) (RETURN MSG T))
	(COND ((EQ CH #\C-M-Y)   ;;selectq ify?
	       (LET ((TEXT (LAST-MESSAGE-TEXT)))
		 (IF (NULL TEXT) (SETQ TEXT ""))
		 (STRING-NCONC MSG TEXT)
		 (SEND STREAM ':FORCE-KBD-INPUT TEXT)))
	      ;;normal case for a vanilla character
	      (T (ARRAY-PUSH-EXTEND MSG CH))))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defmacro with-input-editing ((stream rubout-options . brand-s-compatibility-args) &body body)
  "Execute BODY inside of STREAM's :RUBOUT-HANDLER method.
If BODY does input from STREAM, it will be done with rubout processing
if STREAM implements any.
RUBOUT-OPTIONS should be the options for the :RUBOUT-HANDLER message, such as 
 (:NO-INPUT-SAVE T)   -- don't save this batch of input in the history.
 (:FULL-RUBOUT T)     -- return from this construct if rubout buffer becomes empty.
 (:INITIAL-INPUT string) -- start out with that string in the buffer.
 (:INITIAL-INPUT-POINTER n) -- start out with editing pointer n chars from start.
 (:ACTIVATION fn x-args) -- fn is used to test characters for being activators.
	fn's args are the character read followed by the x-args from the option.
        If fn returns non-NIL, the character is an activation.
	It makes a blip (:ACTIVATION char numeric-arg)
	which BODY can read with :ANY-TYI.
 (:DO-NOT-ECHO chars...) -- poor man's activation characters.
	This is like the :ACTIVATION option except that: characters are listed explicitly;
	and the character itself is returned when it is read,
	rather than an :ACTIVATION blip.
 (:COMMAND fn x-args) -- tests like :ACTIVATION, but command chars do a different thing.
	If fn returns non-NIL, the character is a command character.
	The :RUBOUT-HANDLER operation (and therefore the WITH-INPUT-EDITING)
	returns instantly these two values: (:COMMAND char numeric-arg) :COMMAND.
	The input that was buffered remains in the buffer.
 (:PREEMPTABLE token) -- makes all blips act like command chars.
	If the rubout handler encounters a blip while reading input,
	it instantly returns two values: the blip itself, and the specified token.
	Any buffered input remains buffered for the next request for input editing.
 (:EDITING-COMMAND (char doc)...) -- user-implemented /"editing/" commands.
	If any char in the alist is read by the rubout handler,
	it is returned to the caller (that is, to an :ANY-TYI in BODY).
	BODY should process these characters in appropriate ways and keep reading.
 (:PASS-THROUGH chars...) -- makes chars not be treated specially by the rubout
	handler. Useful for getting characters such as  into the buffer.
	Only works for characters with no control, meta, etc bits set.
 (:PROMPT fn-or-string)
	Says how to prompt initially for the input.
	If a string, it is printed; otherwise it is called with two args,
	the stream and a character which is an editing command that says
	why the prompt is being printed.
 (:REPROMPT fn-or-string)
	Same as :PROMPT except used only if the input is reprinted
	for some reason after editing has begun.  The :REPROMPT option
	is not used on initial entry.  If both :PROMPT and :REPROMPT
        are specified, :PROMPT is used on initial entry and :REPROMPT thereafter."
  (let ((keyword (cadr brand-s-compatibility-args)))
    (unless stream (setq stream 'standard-input))
    `(flet ((do-it () . ,body))
       (if (send ,stream ':operation-handled-p ':rubout-handler)
	   ,(if keyword
		`(with-stack-list* (options
				     ',(selectq keyword
					 (:end-activation '(:activation = #/end))
					 ((:line :line-activation)
					  '(:activation memq (#/end #/return))))
				     ,rubout-options)
		   (send ,stream ':rubout-handler options #'do-it))
	      `(send ,stream ':rubout-handler ,rubout-options #'do-it))
	 (do-it)))))

))


; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :RUBOUT-HANDLER) (RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (COND ((> (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
	 (COPY-ARRAY-PORTION RUBOUT-HANDLER-BUFFER (RHB-SCAN-POINTER) (RHB-FILL-POINTER)
			     RUBOUT-HANDLER-BUFFER 0 (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER))
	 (IF (NUMBERP (RHB-TYPEIN-POINTER))
	     (DECF (RHB-TYPEIN-POINTER) (RHB-SCAN-POINTER)))
	 (SETF (RHB-FILL-POINTER) (- (RHB-FILL-POINTER) (RHB-SCAN-POINTER))))
	(T (SETF (RHB-FILL-POINTER) 0)))
  (SETF (RHB-SCAN-POINTER) 0)
  (SETF (RHB-INITIAL-ENTRY) T)
  (*CATCH 'RETURN-FROM-RUBOUT-HANDLER
    (MULTIPLE-VALUE-BIND (PROMPT-STARTING-X PROMPT-STARTING-Y)
	(FUNCALL-SELF ':READ-CURSORPOS)
      (LET ((PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
	(AND PROMPT-OPTION			;Prompt if desired
	     (RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL)))
      (MULTIPLE-VALUE-BIND (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	  (FUNCALL-SELF ':READ-CURSORPOS)
	;; Output any "typeahead"
	(AND (PLUSP (RHB-FILL-POINTER))
	     (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
	(DO ((RUBOUT-HANDLER T)			;Establish rubout handler
	     (RUBOUT-HANDLER-INSIDE T)
	     (RUBOUT-HANDLER-RE-ECHO-FLAG NIL NIL)
	     (RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))
	    (NIL)
	  (*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
	    (CONDITION-CASE (ERROR)
		(RETURN
		 (LET (LIST)
		   (SETQ LIST (MULTIPLE-VALUE-LIST
				(APPLY FUNCTION ARGS)))	;Call READ or whatever.
		   (SETF (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
		   (AND (RHB-TYPEIN-POINTER)
			(> (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER))
			(SETF (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER)))
		   (VALUES-LIST LIST)))
	      (SYS:PARSE-ERROR
	       (TERPRI SELF)
	       (PRINC ">>ERROR: " SELF)
	       (SEND ERROR ':REPORT SELF)
	       (TERPRI SELF)
	       (SETQ RUBOUT-HANDLER-RE-ECHO-FLAG T)
	       (DO () (NIL) (FUNCALL-SELF ':TYI)))))	;If error, force user to rub out
	  ;;Maybe return when user rubs all the way back
	  (AND (ZEROP (RHB-FILL-POINTER))
	       (LET ((FULL-RUBOUT-OPTION (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS)))
		 (WHEN FULL-RUBOUT-OPTION
		   ;; Get rid of the prompt, if any.
		   (SHEET-CLEAR-BETWEEN-CURSORPOSES
		     SELF PROMPT-STARTING-X PROMPT-STARTING-Y
		     (- CURSOR-X LEFT-MARGIN-SIZE) (- CURSOR-Y TOP-MARGIN-SIZE))
		   (SHEET-SET-CURSORPOS SELF PROMPT-STARTING-X PROMPT-STARTING-Y)
		   (RETURN NIL (CADR FULL-RUBOUT-OPTION))))))))))

))


; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFVAR *STREAM-EDITING-COMMAND* NIL)

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "


;This is the "rubout handler" itself.  We call the supplied function
;after binding RUBOUT-HANDLER to T.  That function, which might be READ,
;does its input as usual, but the :ANY-TYI operation acts differently
;because RUBOUT-HANDLER is T.  Specifically, it may invoke :STREAM-RUBOUT-HANDLER.
(DEFMETHOD (EDITOR-STREAM-MIXIN :RUBOUT-HANDLER) (RUBOUT-HANDLER-ARGS FUNCTION
						  &REST ARGS &AUX TEM
						  (*WINDOW* *STREAM-SHEET*)
						  *STREAM-DEFER-OUTPUT-NOT-AT-END*
						  COMMAND-POINT)
  (IF *STREAM-COMMAND-POINT*
      (PROGN
	(MOVE-BP *STREAM-START-BP* *STREAM-BP*)
	(SETQ COMMAND-POINT *STREAM-COMMAND-POINT*)
	(FLUSH-BP COMMAND-POINT)
	(SETQ *STREAM-COMMAND-POINT* NIL))
    (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*))))
  (LET ((PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-ARGS)))
    (WHEN PROMPT-OPTION
      (IF (ARRAYP (CADR PROMPT-OPTION))
	  (PRINC (CADR PROMPT-OPTION) SELF)
	(FUNCALL (CADR PROMPT-OPTION) SELF NIL))
      (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
  (LET ((INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-ARGS)))
	(INITIAL-INPUT-POINTER (CADR (ASSQ ':INITIAL-INPUT-POINTER RUBOUT-HANDLER-ARGS)))
	(*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))	;need this in forward-char
    (WHEN INITIAL-INPUT
      (INSERT *STREAM-START-BP* INITIAL-INPUT)
      (OR COMMAND-POINT
	  (NOT INITIAL-INPUT-POINTER)
	  (SETQ COMMAND-POINT (FORWARD-CHAR *STREAM-START-BP* INITIAL-INPUT-POINTER)))))
  (STREAM-MAYBE-REDISPLAY)
  (*CATCH 'TV:RETURN-FROM-RUBOUT-HANDLER
    (DO ((RUBOUT-HANDLER T)			;Establish rubout handler
	 (*SRE-ACTIVATION-CHARACTER* NIL)
	 (*STREAM-EDITING-COMMAND* (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-ARGS)))
	 (*STREAM-PASS-THROUGH* (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-ARGS)))
	 (*STREAM-COMMAND-HANDLER*
	   (ASSQ ':COMMAND RUBOUT-HANDLER-ARGS))
	 (*STREAM-ACTIVATION-HANDLER*
	   (ASSQ ':ACTIVATION RUBOUT-HANDLER-ARGS))
	 (*STREAM-DO-NOT-ECHO*
	   (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-ARGS)))
	 (*STREAM-PREEMPTABLE* (ASSQ ':PREEMPTABLE RUBOUT-HANDLER-ARGS)))
	(())
      (WITH-BP (START-OF-MSG-BP *STREAM-START-BP* ':NORMAL)
	(WITH-BP (END-OF-MSG-BP *STREAM-START-BP* ':NORMAL)
	  (*CATCH 'RUBOUT-HANDLER
	    (CONDITION-CASE (ERROR)
		(LET ((*SRE-STREAM-BP* *STREAM-BP*)
		      (*SRE-STREAM-START-BP* *STREAM-START-BP*)
		      (*SRE-WINDOW* *STREAM-SHEET*)
		      *SRE-INPUT-END-BP*
		      (*SRE-INPUT-POINT* COMMAND-POINT))
		  (CONDITION-BIND ((ERROR 'STREAM-READ-ERROR-HANDLER))
		    (RETURN
		      (LET (LIST)
			(SETQ LIST (MULTIPLE-VALUE-LIST
				     (APPLY FUNCTION ARGS)))
			(LET ((*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
			  (DELETE-INTERVAL *STREAM-BP*
					   (INTERVAL-LAST-BP *INTERVAL*)))
			(VALUES-LIST LIST)))))
	      (SYS:PARSE-ERROR
	       (LET ((*STREAM-DEFER-OUTPUT-NOT-AT-END* T))
		 (TERPRI SELF)
		 (PRINC ">>ERROR: " SELF)
		 (SEND ERROR ':REPORT SELF)
		 (TERPRI SELF))
	       (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
	       (MOVE-BP END-OF-MSG-BP *STREAM-START-BP*)
	       (MOVE-BP *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	       (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
	       (STREAM-REDISPLAY)
	       (DO () (NIL) (FUNCALL-SELF ':TYI)))))
	  ;; Here if editor throws to RUBOUT-HANDLER
	  ;; to cause the input we have to be read over again.
	  ;; First, delete any error message we got from a previous parsing.
	  (COND ((NOT (BP-= START-OF-MSG-BP END-OF-MSG-BP))
		 (DELETE-INTERVAL START-OF-MSG-BP END-OF-MSG-BP T)
		 (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
		 (STREAM-REDISPLAY T)))
	  ;; Now start over again reading from the front of the input.
	  (MOVE-BP *STREAM-BP* *STREAM-START-BP*)
	  (SETQ COMMAND-POINT NIL)
	  (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)))
      ;; When a rubout or other editing operation is done, throws back to that
      ;; catch to reread the input.  But if the :FULL-RUBOUT option was specified
      ;; and everything was rubbed out, we return NIL and the specified value.
      (AND (BP-= *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	   (SETQ TEM (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-ARGS))
	   (RETURN NIL (CADR TEM))))))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :STREAM-RUBOUT-HANDLER)
	   (&AUX (RUBOUT-HANDLER NIL) CHAR)
  ;; If there is a saved-up activation character, return it from :ANY-TYI now.
  ;; This happens at the end of a rescan of the input.
  (IF *SRE-ACTIVATION-CHARACTER*
      (PROG1 *SRE-ACTIVATION-CHARACTER*
	     (SETQ *SRE-ACTIVATION-CHARACTER* NIL))
    ;; We could just call the editor, but we must pass certain characters (editing-commands)
    ;; that the program doing the read is handling, and we also want to
    ;; save some time for alphabetic characters.
    (IF *SRE-INPUT-POINT*
	(MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
      (SETQ CHAR (FUNCALL-SELF ':ANY-TYI)))
    ;; Just type out and insert self-inserting printing characters
    ;; but not if they are the caller's editing-commands or activations or commands,
    ;; or if they have been redefined in the editor itself.
    (IF (AND (NUMBERP CHAR)
	     (NOT (OR (MEMQ CHAR *STREAM-EDITING-COMMAND*)
		      (SI:ASSQ-CAREFUL CHAR *STREAM-EDITING-COMMAND*)))
	     (NOT (AND *STREAM-COMMAND-HANDLER*
		       (APPLY (CADR *STREAM-COMMAND-HANDLER*)
			      CHAR (CDDR *STREAM-COMMAND-HANDLER*))))
	     (NOT (AND *STREAM-ACTIVATION-HANDLER*
		       (APPLY (CADR *STREAM-ACTIVATION-HANDLER*)
			      CHAR (CDDR *STREAM-ACTIVATION-HANDLER*))))
	     (NOT (MEMQ CHAR *STREAM-DO-NOT-ECHO*))
	     (OR (AND (OR (< CHAR 40)
			  (ALPHA-CHAR-P CHAR))
		      (EQ 'COM-ORDINARILY-SELF-INSERT (COMMAND-LOOKUP CHAR *STREAM-COMTAB*)))
		 (AND (< CHAR 400)
		      (MEMQ CHAR *STREAM-PASS-THROUGH*))))
	(LET ((*WINDOW* *STREAM-SHEET*))
	  (INSERT-MOVING *STREAM-BP* CHAR)
	  (STREAM-IMMEDIATE-OUTPUT
	    (TV:SHEET-TYO *STREAM-SHEET* CHAR))
	  CHAR)
      ;; Otherwise, run the editor till COM-ACTIVATE throws to us,
      ;; then throw to RUBOUT-HANDLER to restart the read using the buffer contents.

      ;; Move editor point to where we are reading.
      (MOVE-BP *STREAM-BP*
	       (OR *SRE-INPUT-POINT* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*))))
      (SETQ *SRE-INPUT-POINT* NIL)
      ;; Update buffer display and window data so editor gets consistent data.
      (STREAM-REDISPLAY T)
      ;; Unread this character so editor will execute it.
      (WHEN CHAR (TV:IO-BUFFER-UNGET (SEND *STREAM-SHEET* ':IO-BUFFER) CHAR))
      ;; Edit.
      (SET-IN-CLOSURE EDITOR-CLOSURE '*EDITOR-STREAM-ACTIVATION-NEEDED* NIL)
      ;; PASS-ON characters throw here
      ;; to return from the :ANY-TYI method.
      (*CATCH 'RETURN-FROM-ANY-TYI
	(LET ((*STREAM-IBEAM-SHOULD-BLINK* NIL)
	      (*INSIDE-EDITOR-STREAM* NIL))
	  (BIND (LOCF (TV:SHEET-MORE-VPOS *STREAM-SHEET*)) NIL)
	  (UNLESS (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	    (SEND SELF ':ENTER-EDITOR)
	    (SETQ *STREAM-IBEAM-SHOULD-BLINK* T)
	    (LET ((IBEAM-BLINKER
		    (CDR (ASSQ 'STREAM-BLINK-IBEAM
			       (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*)))))
	      (WHEN IBEAM-BLINKER
		(STREAM-BLINK-IBEAM IBEAM-BLINKER *STREAM-SHEET* *STREAM-BP* NIL))))
	  (UNWIND-PROTECT
	      (FUNCALL *STREAM-SHEET* ':EDIT EDITOR-CLOSURE)
	    (FUNCALL *STREAM-SHEET* ':EXIT-EDITOR)
	    ;; Put blinker into ordinary stream mode instead of editor mode.
	    (MULTIPLE-VALUE-BIND (X Y) (TV:BLINKER-READ-CURSORPOS *STREAM-BLINKER*)
	      (TV:SHEET-SET-CURSORPOS *STREAM-SHEET* X Y))
	    (FUNCALL *STREAM-BLINKER* ':SET-FOLLOW-P T)	;Make the blinker follow again
	    (TV:BLINKER-SET-VISIBILITY *STREAM-BLINKER*
				       (IF (EQ *STREAM-SHEET* TV:SELECTED-WINDOW)
					   ':BLINK ':ON)))
	  ;; Tell the :RUBOUT-HANDLER method to restart the read.
	  (*THROW 'RUBOUT-HANDLER T))))))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFUN STREAM-PRE-COMMAND-HOOK-1 (CHAR)
  (COND ((OR (MEMQ CHAR *STREAM-EDITING-COMMAND*)
	     (SI:ASSQ-CAREFUL CHAR *STREAM-EDITING-COMMAND*))
	 (SETQ *SRE-INPUT-END-BP* (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))
	 (SETQ *SRE-INPUT-POINT* (COPY-BP *SRE-STREAM-BP*))
	 (MOVE-BP *SRE-STREAM-BP* (INTERVAL-LAST-BP *INTERVAL*))
	 (*THROW 'RETURN-FROM-ANY-TYI CHAR))
	((AND *STREAM-COMMAND-HANDLER*
	      (APPLY (CADR *STREAM-COMMAND-HANDLER*) CHAR (CDDR *STREAM-COMMAND-HANDLER*)))
	 (SEND *SRE-WINDOW* ':SET-*STREAM-COMMAND-POINT*
	       (COPY-BP *SRE-STREAM-BP* ':NORMAL))
	 (MOVE-BP *SRE-STREAM-BP* *SRE-STREAM-START-BP*)
	 (*THROW 'TV:RETURN-FROM-RUBOUT-HANDLER
		 (VALUES
		   `(:COMMAND ,CHAR ,(OR *NUMERIC-ARG* 1))
		   ':COMMAND)))
	((OR (MEMQ CHAR *STREAM-DO-NOT-ECHO*)
	     (AND *STREAM-ACTIVATION-HANDLER*
		  (APPLY (CADR *STREAM-ACTIVATION-HANDLER*)
			 CHAR (CDDR *STREAM-ACTIVATION-HANDLER*))))
	 (SETQ *SRE-ACTIVATION-CHARACTER*
	       (IF (MEMQ CHAR *STREAM-DO-NOT-ECHO*) CHAR
		 `(:ACTIVATION ,CHAR ,(OR *NUMERIC-ARG* 1))))
	 (*THROW 'RUBOUT-HANDLER T)))
  ;; Tell label to change at redisplay after this command finishes,
  ;; so that if this command activates, there will be no change.
  (FUNCALL *WINDOW* ':ENTER-EDITOR)
  ;; Tell the ibeam blinker to start blinking at next redisplay, also.
  (SETQ *STREAM-IBEAM-SHOULD-BLINK* T))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFUN STREAM-COMMAND-HOOK-1 (CHAR)
  (COND ((BP-= (POINT) (INTERVAL-LAST-BP *INTERVAL*))
	 (OR *EDITOR-STREAM-ACTIVATION-NEEDED*
	     (NOT (OR (< CHAR 200)
		      (MEMQ CHAR '(#\TAB #\CR))
		      (AND (< CHAR 400) (MEMQ CHAR *STREAM-PASS-THROUGH*))))
	     (EQ *LAST-COMMAND-TYPE* 'INDENT-NEW-LINE)
	     (COM-ACTIVATE)))			;Automatically activate
	(*EDITOR-STREAM-REQUIRE-ACTIVATION*
	 (SETQ *EDITOR-STREAM-ACTIVATION-NEEDED* T))))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (ZTOP-STREAM-MIXIN :STREAM-RUBOUT-HANDLER) ()
  ;; If everything has been typed out correctly, update the window datastructure
  (AND (< (WINDOW-REDISPLAY-DEGREE *STREAM-SHEET*) DIS-TEXT)
       (FAKE-OUT-TOP-LINE *STREAM-SHEET* (WINDOW-INTERVAL *STREAM-SHEET*)))
  ;; Avoid confusing the :RUBOUT-HANDLER method after a EDITING-COMMAND character.
  (SETQ *SRE-INPUT-END-BP* NIL *SRE-INPUT-POINT* NIL)
  (IF *SRE-ACTIVATION-CHARACTER*
      (PROG1 *SRE-ACTIVATION-CHARACTER*
	     (SETQ *SRE-ACTIVATION-CHARACTER* NIL))
    (SETQ *ZTOP-SG* SYS:%CURRENT-STACK-GROUP)
    (WITH-BP (OLD-STREAM-BP *STREAM-BP* ':NORMAL)
      (LET ((RESUME-INFORMATION (FUNCALL *ZMACS-SG*)))
	(COND ((EQ RESUME-INFORMATION ':RESCAN)
	       (*THROW 'RUBOUT-HANDLER T))
	      ((EQ RESUME-INFORMATION ':KEEP-READING)
	       (MOVE-BP *STREAM-BP* OLD-STREAM-BP)
	       (FUNCALL-SELF ':ANY-TYI))
	      ((EQ (CAR RESUME-INFORMATION) ':EDITING-COMMAND)
	       (SETQ *SRE-INPUT-END-BP* (CADR RESUME-INFORMATION)
		     *SRE-INPUT-POINT* (CADDR RESUME-INFORMATION))
	       (CADDDR RESUME-INFORMATION))
	      ((EQ (CAR RESUME-INFORMATION) ':COMMAND)
	       (SETQ *STREAM-COMMAND-POINT*
		     (COPY-BP *STREAM-BP* ':NORMAL))
	       (MOVE-BP *STREAM-BP* *STREAM-START-BP*)
	       (*THROW 'TV:RETURN-FROM-RUBOUT-HANDLER
		       (VALUES (CADR RESUME-INFORMATION) ':COMMAND)))
	      ((EQ (CAR RESUME-INFORMATION) ':ACTIVATION)
	       (SETQ *SRE-ACTIVATION-CHARACTER*
		     (CADR RESUME-INFORMATION))
	       (*THROW 'RUBOUT-HANDLER T)))))))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (ZTOP-STREAM-MIXIN :PRE-COMMAND-HOOK)
	   (CHAR &AUX
	    (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND *RUBOUT-HANDLER-ARGS*)))
	    (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO *RUBOUT-HANDLER-ARGS*)))
	    (COMMAND-HANDLER (ASSQ ':COMMAND *RUBOUT-HANDLER-ARGS*))
	    (ACTIVATION-HANDLER (ASSQ ':ACTIVATION *RUBOUT-HANDLER-ARGS*)))
  (COND ((OR (MEMQ CHAR EDITING-COMMAND)
	     (SI:ASSQ-CAREFUL CHAR EDITING-COMMAND))
	 (RESUME-ZTOP-SG
	   (PROG1
	     `(:EDITING-COMMAND
		,(COPY-BP (INTERVAL-LAST-BP *INTERVAL*))
		,(COPY-BP (POINT))
		,CHAR)
	     (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*))))
	 (SETQ *CURRENT-COMMAND-TYPE* 'EDITING-COMMAND)
	 (*THROW 'COMMAND-EXECUTE T))
	((AND COMMAND-HANDLER
	      (APPLY (CADR COMMAND-HANDLER) CHAR (CDDR COMMAND-HANDLER)))
	 (RESUME-ZTOP-SG
	   `(:COMMAND
	      (:COMMAND ,CHAR ,(OR *NUMERIC-ARG* 1))))
	 (SETQ *CURRENT-COMMAND-TYPE* 'EDITING-COMMAND)
	 (*THROW 'COMMAND-EXECUTE T))
	((OR (MEMQ CHAR DO-NOT-ECHO)
	     (AND ACTIVATION-HANDLER
		  (APPLY (CADR ACTIVATION-HANDLER)
			 CHAR (CDDR ACTIVATION-HANDLER))))
	 (SETQ *RUBOUT-HANDLER-STATE* ':NORMAL)
	 (SETQ *ZTOP-ACTIVATION-NEEDED* NIL
	       *ZTOP-EDITING* NIL
	       *ZTOP-READING-INPUT* T)
	 (REDISPLAY-MODE-LINE)
	 (RESUME-ZTOP-SG
	   `(:ACTIVATION
	      ,(IF (MEMQ CHAR DO-NOT-ECHO) CHAR
		 `(:ACTIVATION ,CHAR ,(OR *NUMERIC-ARG* 1)))))
	 (SETQ *CURRENT-COMMAND-TYPE* 'ACTIVATE-ZTOP)
	 (*THROW 'COMMAND-EXECUTE T))))

))

; From file STREAM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (ZTOP-STREAM-MIXIN :COMMAND-HOOK) (TYPE &AUX (OLD-STATE *RUBOUT-HANDLER-STATE*))
  (UNLESS (EQ TYPE 'EDITING-COMMAND)
    (AND (ASSQ ':FULL-RUBOUT *RUBOUT-HANDLER-ARGS*) (BP-= *STREAM-START-BP* *STREAM-BP*)
	 (SETQ OLD-STATE ':EDITING TYPE ':FULL-RUBOUT))
    (SETQ *RUBOUT-HANDLER-STATE*
	  (IF (AND (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
		   (OR (NOT *STREAM-ACTIVATION-NEEDED*)
		       (EQ TYPE 'ACTIVATE-ZTOP))
		   (MEMQ TYPE '(SELF-INSERT INSERT-CR ACTIVATE-ZTOP ZTOP-MODE :FULL-RUBOUT)))
	      ':NORMAL
	    ':EDITING))
    (IF (EQ *RUBOUT-HANDLER-STATE* ':EDITING)
	(SETQ *STREAM-ACTIVATION-NEEDED* *ZTOP-REQUIRE-ACTIVATION*)
      (AND (NEQ OLD-STATE ':NORMAL)		;If we were editing
	   (MOVE-BP *STREAM-BP* *STREAM-START-BP*))
      (SETQ *ZMACS-SG* SYS:%CURRENT-STACK-GROUP)
      (RESUME-ZTOP-SG (IF (EQ OLD-STATE ':EDITING) ':RESCAN ':KEEP-READING))
      (AND (NEQ OLD-STATE ':NORMAL)
	   (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS))))
  (SETQ *ZTOP-ACTIVATION-NEEDED* *STREAM-ACTIVATION-NEEDED*)
  (SETQ *ZTOP-EDITING*
	(AND (NOT *ZTOP-ACTIVATION-NEEDED*)
	     (EQ *RUBOUT-HANDLER-STATE* ':EDITING)))
  (SETQ *ZTOP-READING-INPUT* (AND (NOT *ZTOP-ACTIVATION-NEEDED*) (NOT *ZTOP-EDITING*))))

))

; From file RH.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFUN-RH ALTERNATE-RUBOUT-HANDLER ()
  (LET ((CH) (CH-CHAR) (CH-CONTROL-META) (COMMAND)
	(FILL-POINTER (RH-FILL-POINTER))
	(TYPEIN-POINTER (RH-TYPEIN-POINTER))
	(INITIAL-ENTRY (RHB-INITIAL-ENTRY))
	(RUBBED-OUT-SOME? NIL)
	(INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS)))
	(INITIAL-INPUT-POINTER
	  (CADR (ASSQ ':INITIAL-INPUT-POINTER RUBOUT-HANDLER-OPTIONS)))
	(NUMERIC-ARG NIL)
	(NUMERIC-ARG-NEGATIVE NIL))
    (SETF (RHB-INITIAL-ENTRY) NIL)
    ;; Kludge #1.  If this is the first time this rubout handler has been invoked
    ;; in this stream, then we must create the input history.
    (OR (RH-INPUT-RING)
	(WHEN (FBOUNDP 'ZWEI:MAKE-HISTORY)
	  (SETF (RH-INPUT-RING) (RH-MAKE-INPUT-RING))))
    (WHEN INITIAL-ENTRY
      ;; save the previous input on the input history,
      ;; unless the previous read said not to save it.
      (COND ((AND (NOT (RH-DONT-SAVE-FLAG))
		  TYPEIN-POINTER
		  (NEQ INITIAL-ENTRY ':RESTORED)
		  (NOT (ZEROP TYPEIN-POINTER)))
	     ;; only add the contents if it is different from the last entry, and
	     ;; the entry is at least 2 characters long.
	     (SETF (FILL-POINTER RUBOUT-HANDLER-BUFFER) TYPEIN-POINTER)
	     (WHEN (AND (> TYPEIN-POINTER 1)
			(MISMATCH RUBOUT-HANDLER-BUFFER
				  (ZWEI:HISTORY-LATEST-ELEMENT (RH-INPUT-RING))))
	       (ZWEI:PUSH-ON-HISTORY (SUBSEQ RUBOUT-HANDLER-BUFFER 0 TYPEIN-POINTER)
				     (RH-INPUT-RING)))
	     (SETF (FILL-POINTER RUBOUT-HANDLER-BUFFER) FILL-POINTER)))
      ;; Then initialize the typein pointer.
      (SETF (RH-TYPEIN-POINTER) FILL-POINTER)
      (SETQ TYPEIN-POINTER FILL-POINTER)
      ;; Gobble the initial input if any.
      (WHEN (AND INITIAL-INPUT (NEQ INITIAL-ENTRY ':RESTORED))
	(RH-INSERT-STRING INITIAL-INPUT 0 NIL NIL NIL)
	(SETQ RUBBED-OUT-SOME? T FILL-POINTER (RH-FILL-POINTER))
        (SETQ INITIAL-INPUT-POINTER
	      (MAX (MIN (OR INITIAL-INPUT-POINTER TYPEIN-POINTER)
			(LENGTH RUBOUT-HANDLER-BUFFER)) 0))
	(SETF TYPEIN-POINTER INITIAL-INPUT-POINTER
	      (RH-TYPEIN-POINTER) INITIAL-INPUT-POINTER)
 	(RH-SET-POSITION TYPEIN-POINTER))
      ;; Record whether this unit of input should be saved on the history.
      (SETF (RH-DONT-SAVE-FLAG)
	    (OR (CADR (ASSQ ':DONT-SAVE RUBOUT-HANDLER-OPTIONS))
		(CADR (ASSQ ':NO-INPUT-SAVE RUBOUT-HANDLER-OPTIONS))))
      )
    ;;; Can this ever go off? :pass-though now only allows non-bucky. -- mly ;;;
    ;; Kludge #5.  We can't echo or rub out a bucky char or a bli,
    ;; so if the last char inserted was a either of those
    ;; and it did not terminate the input, flush it.
    (AND (NOT (ZEROP TYPEIN-POINTER))
	 (OR (CONSP (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER)))
	     (LDB-TEST %%KBD-CONTROL-META
		       (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER))))
	 (SETQ TYPEIN-POINTER (SETF (RH-TYPEIN-POINTER) (DECF (RH-FILL-POINTER)))))
    ;; Kludge #4.  After resuming a Break, the stream's cursorpos is wrong.
    ;; In fact, the cursor is at the end of the string in that case.
    ;; So, if it is supposed to be elsewhere, move it.
    ;; This condition also avoids wasting time when we are reading typein
    ;; at the end of the string.
    (OR (= FILL-POINTER TYPEIN-POINTER)
	(RH-CURSOR-MOTION TYPEIN-POINTER))
    ;; In case we had to return to the caller with a EDITING-COMMAND char
    ;; while RUBBED-OUT-SOME? was T, make things consistent again
    ;; by causing a rescan now.
    (WHEN (AND (NOT INITIAL-ENTRY)
	       (= (RH-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
      (SETF (RH-SCAN-POINTER) 0)
      (*THROW 'RUBOUT-HANDLER T))
    (*CATCH 'RETURN-CHARACTER
      (WHEN RUBOUT-HANDLER-ACTIVATION-CHARACTER
	(*THROW 'RETURN-CHARACTER
		(PROG1 RUBOUT-HANDLER-ACTIVATION-CHARACTER
		       (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))))
      ;; Read characters.  If an ordinary character is typed and nothing has been rubbed out,
      ;; return immediately.  Otherwise, let all editing operations complete
      ;; before returning. 
      (DO (*LAST-COMMAND-TYPE*
	   *CURRENT-COMMAND-TYPE*
	   *RUBOUT-HANDLER-MARK*
	   (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-OPTIONS)))
	   (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-OPTIONS)))
	   (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS)))
	   (COMMAND-HANDLER
	     (ASSQ ':COMMAND RUBOUT-HANDLER-OPTIONS))
	   (PREEMPTABLE (ASSQ ':PREEMPTABLE RUBOUT-HANDLER-OPTIONS))
	   (ACTIVATION-HANDLER
	     (ASSQ ':ACTIVATION RUBOUT-HANDLER-OPTIONS)))
	  (NIL)
	;; Read a character from the stream after bypassing ourself.
	(SETQ CH (LET ((RUBOUT-HANDLER NIL)) (FUNCALL-SELF ':ANY-TYI)))
	(IF (LISTP CH)
	    (COND ((EQ (CAR CH) 'REDISPLAY-RUBOUT-HANDLER)
		   (SEND SELF ':SET-CURSORPOS
			 PROMPT-STARTING-X PROMPT-STARTING-Y)
		   (SEND SELF ':CLEAR-EOL)
		   (RH-REPRINT-INPUT NIL T))
		  (PREEMPTABLE
		   (SETF (RH-SCAN-POINTER) 0)
		   (*THROW 'RETURN-FROM-RUBOUT-HANDLER
			   (VALUES CH (CADR PREEMPTABLE))))
		  ((AND (EQ (CAR CH) ':MOUSE-BUTTON)
			(EQ (CADR CH) #\MOUSE-3-1))
		   (MOUSE-CALL-SYSTEM-MENU)))
	  (SETQ CH-CHAR (LDB %%KBD-CHAR CH))
	  (SETQ CH-CONTROL-META (LDB %%KBD-CONTROL-META CH))
	  (SETQ COMMAND (UNLESS (AND (ZEROP CH-CONTROL-META)
				     (MEMQ CH PASS-THROUGH))			
			  (ASSQ CH RH-COMMAND-ALIST)))
	  (COND
	    ((AND COMMAND-HANDLER
		  (APPLY (CADR COMMAND-HANDLER) CH (CDDR COMMAND-HANDLER)))
	     (SETF (RH-SCAN-POINTER) 0)
	     (*THROW 'RETURN-FROM-RUBOUT-HANDLER
		     (VALUES
		       `(:COMMAND ,CH ,(* (OR NUMERIC-ARG 1)
					  (IF NUMERIC-ARG-NEGATIVE -1 1)))
		       ':COMMAND)))
	    ((OR (MEMQ CH DO-NOT-ECHO)
		 (AND ACTIVATION-HANDLER
		      (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER))))
	     (RH-SET-POSITION (RH-FILL-POINTER))
	     (LET ((VALUE
		     (IF (MEMQ CH DO-NOT-ECHO) CH
		       `(:ACTIVATION ,CH ,(* (OR NUMERIC-ARG 1)
					     (IF NUMERIC-ARG-NEGATIVE -1 1))))))
	       (COND (RUBBED-OUT-SOME?
		      ;; Why isn't this done in the :RUBOUT-HANDLER method loop?
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER VALUE)
		      (SETF (RH-SCAN-POINTER) 0)
		      (*THROW 'RUBOUT-HANDLER T))
		     (T (*THROW 'RETURN-CHARACTER VALUE)))))
	    ;; Don't touch this character, just return it to caller.
	    ((OR (MEMQ CH EDITING-COMMAND)
		 (SI:ASSQ-CAREFUL CH EDITING-COMMAND))
	     ;; Cause rubout handler rescan next time the user does :TYI.
	     (IF RUBBED-OUT-SOME? (SETF (RH-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
	     (RETURN CH))
	    ;; An standard rh editing command of some sort.  The RUBBED-OUT-SOME bit can only
	    ;; be cleared by entering this function again.  The function is passed the
	    ;; numeric argument, and returns T if we are going to need to throw out (like
	    ;; DIS-ALL in the editor).
	    (COMMAND
	     (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
		   *CURRENT-COMMAND-TYPE* NIL)
	     (SETQ RUBBED-OUT-SOME?
		   (OR (FUNCALL (CDR COMMAND) (* (OR NUMERIC-ARG 1)
						 (IF NUMERIC-ARG-NEGATIVE -1 1)))
		       RUBBED-OUT-SOME?))
	     (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL)
	     ;; If the buffer is empty and the :FULL-RUBOUT option is active, then throw now.
	     ;; This will throw if the user types Rubout or ClearScreen immediately after
	     ;; entering the read function.  It is important that we check for this here
	     ;; and not in RH-DELETE-STRING since some commands, such as Yank-Pop, may
	     ;; temporarily empty the buffer.  It wouldn't be the right thing to throw
	     ;; if the buffer only contained whitespace since it is the responsibility
	     ;; of the caller to discard whitespace when looking for special characters.
	     (COND ((AND (ZEROP (RH-FILL-POINTER))
			 (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
		    ;; This SETF should be done in the :RUBOUT-HANDLER method loop.
		    (SETF (RH-SCAN-POINTER) 0)
		    (*THROW 'RUBOUT-HANDLER T))))
	    ;;Handle Control-number and Control-U specially.
	    ((AND (NOT (ZEROP CH-CONTROL-META))
		  ( #/0 CH-CHAR #/9))
	     (SETQ NUMERIC-ARG (+ (* (OR NUMERIC-ARG 0) 10.) (- CH-CHAR #/0))))
	    ((= CH #\CONTROL-U)
	     (SETQ NUMERIC-ARG (* (OR NUMERIC-ARG 1) 4)))
	    ((AND (NOT (ZEROP CH-CONTROL-META)) (= CH-CHAR #/-))
	     (IF NUMERIC-ARG
		 (FUNCALL-SELF ':BEEP)
	       (SETQ NUMERIC-ARG-NEGATIVE (NOT NUMERIC-ARG-NEGATIVE))))
	    ;; Some other random control character -- beep and ignore
	    ((NOT (ZEROP CH-CONTROL-META))
	     (FUNCALL-SELF ':BEEP)
	     (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))
	    ;; Self-inserting character.  Set RUBBED-OUT-SOME since if we return,
	    ;; we were typing in the middle of the line.  Typing at the end of the
	    ;; line throws to RETURN-CHARACTER.
	    (T (UNLESS NUMERIC-ARG-NEGATIVE
		 (RH-INSERT-CHAR CH (OR NUMERIC-ARG 1) RUBBED-OUT-SOME?)
		 (SETQ RUBBED-OUT-SOME? T))
	       (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
		     *CURRENT-COMMAND-TYPE* NIL
		     *RUBOUT-HANDLER-MARK* NIL)
	       (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))))))))

))

; From file RH.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFINE-RH-COMMAND RH-COM-LIST-COMMANDS (#\CONTROL-HELP) (IGNORE)
  (LET ((RUBOUT-HANDLER NIL))
    (SI:WITH-HELP-STREAM (HELP-WINDOW :LABEL "Input Editor Commands" :WIDTH *WIDTH
				      :SUPERIOR
				      (IF (TYPEP SELF 'SHEET)
					  (SHEET-GET-SCREEN SELF)
					SELF))
      (LET* ((EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-OPTIONS))))
	(FORMAT HELP-WINDOW "Input Editor Commands:~@
		    Control-number and Control-U provide numeric argument.~2%")
	;; Print double column list of commands.
	(RH-PRINT-HELP-DOUBLE-COLUMNS RH-COMMAND-ALIST HELP-WINDOW *WIDTH
				   (APPEND (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS))
					   EDITING-COMMAND))
	(WHEN EDITING-COMMAND
	  (FORMAT HELP-WINDOW "~%Additional commands available right now:~2%")
	  (RH-PRINT-HELP-DOUBLE-COLUMNS EDITING-COMMAND HELP-WINDOW *WIDTH)))
      (TERPRI HELP-WINDOW))))

))

; From file RH.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFUN RH-PRINT-HELP-DOUBLE-COLUMNS (ALIST HELP-WINDOW WIDTH &OPTIONAL EXCEPT)
  (IF (< WIDTH 80.)
      (DOLIST (ELT ALIST)
	(LET ((CH (IF (LISTP ELT) (CAR ELT) ELT))		  
	      DOC)
	  (UNLESS (OR (MEMQ CH EXCEPT)
		      (SI:ASSQ-CAREFUL CH EXCEPT))
	    (SETQ DOC (IF (LISTP ELT)
			  (IF (SYMBOLP (CDR ELT)) (RH-MAKE-COMMAND-NAME (CDR ELT))
			    (CADR ELT))
			"exits the command editor"))
	    (FORMAT HELP-WINDOW "~:C~21T~A~%" CH DOC))))
    (LET (LIST)
      (DOLIST (ELT ALIST)
	(LET ((CH (IF (LISTP ELT) (CAR ELT) ELT))		  
	      DOC)
	  (UNLESS (OR (MEMQ CH EXCEPT)
		      (SI:ASSQ-CAREFUL CH EXCEPT))
	    (SETQ DOC (IF (LISTP ELT)
			  (IF (SYMBOLP (CDR ELT)) (RH-MAKE-COMMAND-NAME (CDR ELT))
			    (CADR ELT))
			"exits the command editor"))
	    (PUSH (CONS CH DOC) LIST))))
      (SETQ LIST (NREVERSE LIST))
      (LET* ((LENGTH (LENGTH LIST))
	     (HALF-LENGTH (CEILING LENGTH 2)))
	(LOOP FOR ELT1 IN LIST
	      FOR ELT2 IN (NTHCDR HALF-LENGTH LIST)
	      DO (FORMAT HELP-WINDOW "~:C~13T~A~37T~:C~58T~A~%"
			 (CAR ELT1) (CDR ELT1) (CAR ELT2) (CDR ELT2)))
	;; If an odd command left over, print it too.
	(IF (ODDP LENGTH)
	    (LET ((ELT (NTH (TRUNCATE LENGTH 2) LIST)))
	      (FORMAT HELP-WINDOW "~:C~13T~A~%"
		      (CAR ELT) (CDR ELT))))))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :RESCANNING-P) ()
  (OR (< (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
      RUBOUT-HANDLER-ACTIVATION-CHARACTER))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :FORCE-RESCAN) ()
  (SETF (RHB-SCAN-POINTER) 0)
  (*THROW 'RUBOUT-HANDLER T))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :READ-BP) ()
  (RHB-SCAN-POINTER))

))

(globalize "DEFSELECT-INCREMENTAL")
; From file STREAM.LISP SRC:<L.WINDOW> OZ:


#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "


;;; Give a single character, or do rubout processing, throws to RUBOUT-HANDLER on editing.
(DEFUN DEFAULT-RUBOUT-HANDLER ()
  (DECLARE (:SELF-FLAVOR STREAM-MIXIN))
  (SETF (RHB-TYPEIN-POINTER) NIL)		;Mark that old rubout handler is in use.
  (WHEN (EQ (RHB-INITIAL-ENTRY) T)
    ;;no point looking for :initial-input-pointer since this rh can't do anything with it
    (LET ((INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS))))
      (WHEN INITIAL-INPUT
	(STRING-NCONC RUBOUT-HANDLER-BUFFER INITIAL-INPUT))))
  (SETF (RHB-INITIAL-ENTRY) NIL)
  (WHEN (= (RHB-SCAN-POINTER) MOST-POSITIVE-FIXNUM)
    (SETF (RH-SCAN-POINTER) 0)
    (*THROW 'RUBOUT-HANDLER T))
  (OR (PROG1 RUBOUT-HANDLER-ACTIVATION-CHARACTER
	     (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))
      (DO ((RUBOUT-HANDLER NIL)
	   (RUBBED-OUT-SOME NIL)
	   (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-OPTIONS)))
	   (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-OPTIONS)))
	   (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS)))
	   (COMMAND-HANDLER
	     (ASSQ ':COMMAND RUBOUT-HANDLER-OPTIONS))
	   (PREEMPTABLE (ASSQ ':PREEMPTABLE RUBOUT-HANDLER-OPTIONS))
	   (ACTIVATION-HANDLER
	     (ASSQ ':ACTIVATION RUBOUT-HANDLER-OPTIONS))
	   CH LEN)
	  (NIL)
	(SETQ CH (FUNCALL-SELF ':ANY-TYI))
	(COND ((AND (CONSP CH) (EQ (CAR CH) 'REDISPLAY-RUBOUT-HANDLER))
	       (SEND SELF ':SET-CURSORPOS PROMPT-STARTING-X PROMPT-STARTING-Y)
	       (SEND SELF ':CLEAR-EOL)
	       (AND (SETQ LEN (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
				  (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
		    (RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
	       (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
		 (FUNCALL-SELF ':READ-CURSORPOS))
	       (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
	      ((LISTP CH)
	       (WHEN PREEMPTABLE
		 (SETF (RHB-SCAN-POINTER) 0)
		 (*THROW 'RETURN-FROM-RUBOUT-HANDLER
			 (VALUES CH (CADR PREEMPTABLE)))))
	      ((AND COMMAND-HANDLER
		    (APPLY (CADR COMMAND-HANDLER) CH (CDDR COMMAND-HANDLER)))
	       (SETF (RHB-SCAN-POINTER) 0)
	       (*THROW 'RETURN-FROM-RUBOUT-HANDLER
		       (VALUES
			 `(:COMMAND ,CH 1)
			 ':COMMAND)))
	      ;; Don't touch this character, just return it to caller.
	      ((OR (MEMQ CH EDITING-COMMAND)
		   (SI:ASSQ-CAREFUL CH EDITING-COMMAND))
	       ;; Cause rubout handler rescan next time the user does :TYI.
	       (IF RUBBED-OUT-SOME (SETF (RH-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
	       (RETURN CH))
	      ;; Is it an editing character?
	      ((AND (NOT (OR (MEMQ CH DO-NOT-ECHO)
			     (AND ACTIVATION-HANDLER
				  (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER)))))
		    (OR (LDB-TEST %%KBD-CONTROL-META CH)
			(AND (MEMQ CH '(#\RUBOUT #\CLEAR-INPUT #\CLEAR-SCREEN #\VT))
			     (NOT (MEMQ CH PASS-THROUGH)))))
	       (COND ((MEMQ CH '(#\CLEAR-SCREEN #\VT))	;Retype buffered input
		      (FUNCALL-SELF ':TYO CH)		;Echo it
		      (IF (= CH #\CLEAR-SCREEN) (FUNCALL-SELF ':CLEAR-SCREEN)
			  (FUNCALL-SELF ':TYO #\CR))
		      (MULTIPLE-VALUE (PROMPT-STARTING-X PROMPT-STARTING-Y)
			(FUNCALL-SELF ':READ-CURSORPOS))
		      (AND (SETQ LEN (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
					 (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
			   (RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
		      (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
			(FUNCALL-SELF ':READ-CURSORPOS))
		      (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
		     ((MEMQ CH '(#\RUBOUT #\M-RUBOUT #\CLEAR-INPUT)) ;Delete some characters
		      (COND ((NOT (ZEROP (SETQ LEN (RHB-FILL-POINTER))))
			     (SETF (RHB-FILL-POINTER)
			       (SETQ LEN (SELECTQ CH
					   (#\RUBOUT (1- LEN))
					   (#\M-RUBOUT (STRING-BACKWARD-WORD
							RUBOUT-HANDLER-BUFFER LEN))
					   (#\CLEAR-INPUT 0))))
			     (SETQ RUBBED-OUT-SOME T)
			     (MULTIPLE-VALUE-BIND (X Y)
				 (FUNCALL-SELF ':COMPUTE-MOTION RUBOUT-HANDLER-BUFFER 0 LEN
				   RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
			       (IF RUBOUT-HANDLER-RE-ECHO-FLAG
				   (SETQ X RUBOUT-HANDLER-STARTING-X Y RUBOUT-HANDLER-STARTING-Y))
			       (MULTIPLE-VALUE-BIND (CX CY) (FUNCALL-SELF ':READ-CURSORPOS)
				 (FUNCALL-SELF ':CLEAR-BETWEEN-CURSORPOSES X Y CX CY))
			       (FUNCALL-SELF ':SET-CURSORPOS X Y)
			       (AND RUBOUT-HANDLER-RE-ECHO-FLAG
				    (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))))))
		     (T (BEEP)))				;Undefined editing character
	       (COND ((AND (ZEROP (RHB-FILL-POINTER))
			   (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
		      (SETF (RHB-SCAN-POINTER) 0)
		      (*THROW 'RUBOUT-HANDLER T))))
	      (T						;It's a self-inserting character
	       ;; If this is first character typed in, re-get starting cursorpos since while
	       ;; waiting for input a notification may have been typed out.
	       (AND (ZEROP (RHB-FILL-POINTER))
		    (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
		      (FUNCALL-SELF ':READ-CURSORPOS)))
	       (COND ((MEMQ CH DO-NOT-ECHO)
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER CH))
		     ((AND ACTIVATION-HANDLER
			   (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER)))
		      (SETQ CH `(:ACTIVATION ,CH 1))
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER CH))
		     (T
		      (FUNCALL-SELF ':TYO CH)
		      (ARRAY-PUSH-EXTEND RUBOUT-HANDLER-BUFFER CH)))
	       (COND (RUBBED-OUT-SOME
		      (SETF (RHB-SCAN-POINTER) 0)
		      (*THROW 'RUBOUT-HANDLER T))
		     (T
		      (SETF (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL)
		      (RETURN CH))))))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :RESTORE-RUBOUT-HANDLER-BUFFER) (STRING &OPTIONAL POINTER)
  (LET ((LENGTH (ARRAY-ACTIVE-LENGTH STRING)))
    (OR ( (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER) LENGTH)
	(ADJUST-ARRAY-SIZE RUBOUT-HANDLER-BUFFER LENGTH))
    (COPY-ARRAY-CONTENTS STRING RUBOUT-HANDLER-BUFFER)
    (SETF (RHB-FILL-POINTER) LENGTH))
  (SETF (RHB-TYPEIN-POINTER) POINTER)
  (FUNCALL-SELF ':REFRESH-RUBOUT-HANDLER)
  (SETF (RHB-SCAN-POINTER) 0)
  (SETF (RHB-INITIAL-ENTRY) ':RESTORED)
  (*THROW 'RUBOUT-HANDLER T))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method list (&rest elts)
  (let ((storevar (gensym)))
    (values nil nil (list storevar)
	    (do ((i 0 (1+ i))
		 (accum)
		 (args elts (cdr args)))
		((null args)
		 (cons 'progn (nreverse accum)))
	      (push (setf-match (car args) `(nth ,i ,storevar)) accum))
	    `(incorrect-structure-setf list . ,elts))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method list* (&rest elts)
  (let ((storevar (gensym)))
    (values nil nil (list storevar)
	    (do ((i 0 (1+ i))
		 (accum)
		 (args elts (cdr args)))
		((null args)
		 (cons 'progn (nreverse accum)))
	      (cond ((cdr args)
		     (push (setf-match (car args) `(nth ,i ,storevar)) accum))
		    (t (push (setf-match (car args) `(nthcdr ,i ,storevar)) accum))))
	    `(incorrect-structure-setf list* . ,elts))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method cons (car cdr)
  (let ((storevar (gensym)))
    (values nil nil (list storevar)
	    `(progn ,(setf-match car `(car ,storevar))
		    ,(setf-match cdr `(cdr ,storevar)))
	    `(incorrect-structure-setf cons ,car ,cdr))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro incorrect-structure-setf (&rest args)
  (ferror nil "You cannot SETF the place ~S~% in a way that refers to its old contents."
	  args))

))


; From file DRIBBL.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DRIBBL  "

(defun dribble-stream-io (op &rest args
			  &aux (old-tio terminal-io)
			       (terminal-io
				 ;; Don't leave terminal-io as the dribble stream
				 ;; so that errors inside here don't bomb out.
				 (if (eq terminal-io dribble-stream)
				     *tv-stream*
				   terminal-io)))
  (declare (special *tv-stream* *file-stream* dribble-stream *rubout-handler-buffer*))
  (selectq op
    ((:tyo :string-out :line-out :fresh-line)
     (lexpr-funcall *tv-stream* op args)
     (lexpr-funcall *file-stream* op args))
    (:tyi
     (prog ()
	   (or rubout-handler (funcall *file-stream* ':send-if-handles ':force-output))
	   (*catch (if rubout-handler 'rubout-handler 'dummy-tag)
	     (let ((ch (funcall *tv-stream* op)))
	       (and rubout-handler (array-push-extend *rubout-handler-buffer* ch))
	       (return ch)))
	   ;;get here if someone threw to rubout-handler
	   ;;reset our buffer and continue the throw
	   (store-array-leader 0 *rubout-handler-buffer* 0)
	   (*throw 'rubout-handler nil)))
    (:untyi
     (funcall *tv-stream* ':untyi (car args))
     (and rubout-handler (plusp (length *rubout-handler-buffer*))
	  (decf (array-leader *rubout-handler-buffer* 0))))
    (:rubout-handler
     (store-array-leader 0 *rubout-handler-buffer* 0)	;reset the buffer
     (prog (vals)
       (cond ((and (funcall *file-stream* ':operation-handled-p ':safe-to-use-p)
		   (not (funcall *file-stream* ':safe-to-use-p)))
	      (format *tv-stream* "~&Dribble stream cannot accept output!")
	      (dribble-end)))
       (setq vals (multiple-value-list
		    ;; Bind terminal-io back to the dribble stream if that's what it was
		    ;; in case the code run inside the rubout handler
		    ;; uses terminal-io.
		    (let ((terminal-io old-tio))
		      (lexpr-funcall *tv-stream* op args))))
       ;; If the stream is having troubles, don't echo to it.
       (cond ((and (funcall *file-stream* ':operation-handled-p ':safe-to-use-p)
		   (not (funcall *file-stream* ':safe-to-use-p)))
	      (format *tv-stream* "~&Dribble stream cannot accept output!")
	      (dribble-end)))
       (funcall *file-stream* ':string-out *rubout-handler-buffer*)
       (funcall *file-stream* ':send-if-handles ':force-output)
       (return-list vals)))
    (:dribble-end
     (format *tv-stream* "~&Closing dribble file.")
     (close *file-stream*)
     (funcall *file-stream* ':send-if-handles ':truename))
    (:notice
     (if (funcall *file-stream* ':send-if-handles ':safe-to-use-p)
	 (lexpr-funcall *tv-stream* ':send-if-handles ':notice args)
       'tv:cold-load-stream))
    (:increment-cursorpos
     (cond ((eq (caddr args) ':character)
	    (dotimes (y-increment (cadr args))
	      (funcall *file-stream* ':tyo #\return))
	    (dotimes (x-increment (car args))
	      (funcall *file-stream* ':tyo #\sp))))
     (lexpr-funcall *tv-stream* op args))
    ((:finish :force-output)
     (lexpr-funcall *file-stream* ':send-if-handles op args)
     (lexpr-funcall *tv-stream* op args))
    (otherwise
     (lexpr-funcall *tv-stream* op args))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :PREEMPTABLE-READ) (OPTIONS FUN &REST ARGS)
  (DO ((TYPEAHEAD OLD-TYPEAHEAD NIL)
       (RESULT) (FLAG))
      (())
    (SETQ OLD-TYPEAHEAD T)
    (UNWIND-PROTECT
      (MULTIPLE-VALUE (RESULT FLAG)
	(LEXPR-FUNCALL-SELF ':RUBOUT-HANDLER (APPEND '((:FULL-RUBOUT :FULL-RUBOUT))
						     (AND (LISTP TYPEAHEAD)
							  `((:INITIAL-INPUT ,(CAR TYPEAHEAD))
							    (:INITIAL-INPUT-POINTER
							      ,(CADR TYPEAHEAD))))
						     OPTIONS)
			    FUN ARGS))
      (AND (EQ OLD-TYPEAHEAD T)
	   (SETQ OLD-TYPEAHEAD NIL)))
    (AND (NEQ FLAG ':FULL-RUBOUT)
	 (RETURN RESULT NIL))
    ;; Determine whether a mouse character caused the full-rubout
    (SETQ RESULT (FUNCALL-SELF ':ANY-TYI-NO-HANG))
    (COND (RESULT
	   (OR (NUMBERP RESULT)
	       (RETURN RESULT ':MOUSE-CHAR))
	   (FUNCALL-SELF ':UNTYI RESULT)))
    (AND (SETQ FLAG (CADR (ASSQ ':FULL-RUBOUT OPTIONS)))
	 (RETURN NIL FLAG))
    ;; Presumably after this point, the user didn't call us with :FULL-RUBOUT
    ;; option, so we should retry. We have to fix up the notion of :PROMPT
    ;; and :REPROMPT first though.
    (LET ((PROMPT (ASSQ ':PROMPT OPTIONS)))
      (COND (PROMPT 
	     (SETQ OPTIONS (REMQ PROMPT OPTIONS))
	     ;This next condition may be unnecessary, but just in case. --kmp
	     (COND ((NOT (ASSQ ':REPROMPT OPTIONS)) 
		    ;; make fake reprompt info. our old prompt should still 
		    ;; be there --kmp
		    (PUSH `(:REPROMPT . ,(CDR PROMPT)) OPTIONS))))))))

))

; From file COLD.LISP SRC:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (COLD-LOAD-STREAM)
(DEFUN COLD-LOAD-STREAM-RUBOUT-HANDLER ()
  (WHEN (= (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) 7777777)
    (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) 0)
    (*THROW 'RUBOUT-HANDLER T))
  (IF COLD-LOAD-STREAM-ACTIVATION-CHARACTER
      (RETURN-FROM COLD-LOAD-STREAM-RUBOUT-HANDLER
	(PROG1 COLD-LOAD-STREAM-ACTIVATION-CHARACTER
	       (SETQ COLD-LOAD-STREAM-ACTIVATION-CHARACTER NIL))))
  (DO ((CH)
       (RUBBED-OUT-SOME)
       (LEN)
       (RUBOUT-HANDLER NIL)
       (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS)))
       (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-OPTIONS)))
       (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-OPTIONS)))
       (COMMAND-HANDLER
	 (ASSQ ':COMMAND RUBOUT-HANDLER-OPTIONS))
       (ACTIVATION-HANDLER
	 (ASSQ ':ACTIVATION RUBOUT-HANDLER-OPTIONS))
       (INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS))))
      (NIL)
    (when initial-input
      (let ((length (length initial-input)))
	(funcall-self ':string-out initial-input)
	(if (< (array-length rubout-handler-buffer) length)
	    (setq rubout-handler-buffer (array-grow rubout-handler-buffer (+ length length))))
	(copy-array-portion initial-input 0 length rubout-handler-buffer 0 length)
	(setf (fill-pointer rubout-handler-buffer ) length)
	(setq initial-input nil)
	;;gross kludge.
	(setq rubout-handler-options (subset-not #'(lambda (x) (eq (car x) ':initial-input))
						 rubout-handler-options))
	(setq rubbed-out-some t)))
    (SETQ CH (FUNCALL-SELF ':TYI))
    (COND ((AND COMMAND-HANDLER
		(APPLY (CADR COMMAND-HANDLER) CH (CDDR COMMAND-HANDLER)))
	     (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) 0)
	     (*THROW 'TV:RETURN-FROM-RUBOUT-HANDLER
		     (VALUES
		       `(:COMMAND ,CH 1)
		       ':COMMAND)))
	  ;; Don't touch this character, just return it to caller.
	  ((OR (MEMQ CH EDITING-COMMAND)
	       (ASSQ-CAREFUL CH EDITING-COMMAND))
	   ;; Cause rubout handler rescan next time the user does :TYI.
	   (IF RUBBED-OUT-SOME
	       (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) 7777777))
	   (RETURN CH))
	  ((AND (NOT (OR (MEMQ CH DO-NOT-ECHO)
			 (MEMQ CH PASS-THROUGH)
			 (AND ACTIVATION-HANDLER
			      (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER)))))
		(OR (LDB-TEST %%KBD-CONTROL-META CH)
		    (MEMQ CH '(#\RUBOUT #\CLEAR-INPUT #\CLEAR-SCREEN #\VT))))
	   (COND
	     ((= CH #\CLEAR)			;CLEAR flushes all buffered input
	      (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 0)
	      (SETQ RUBBED-OUT-SOME T)		;Will need to throw out
	      (FUNCALL-SELF ':TYO CH)		;Echo and advance to new line
	      (FUNCALL-SELF ':TYO #\CR))
	     ((OR (= CH #\FORM) (= CH #\VT))	;Retype buffered input
	      (FUNCALL-SELF ':TYO CH)		;Echo it
	      (IF (= CH #\FORM) (FUNCALL-SELF ':CLEAR-SCREEN) (FUNCALL-SELF ':TYO #\CR))
	      (LET ((PROMPT (CADR (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
				      (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))))
		(AND PROMPT
		     (IF (STRINGP PROMPT)
			 (PRINC PROMPT SELF)
		       (FUNCALL PROMPT SELF CH))))
	      (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
	     ((= CH #\RUBOUT)
	      (COND ((NOT (ZEROP (SETQ LEN (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0))))
		     (SETQ CURSOR-X (MAX 0 (- CURSOR-X CHAR-WIDTH)))
		     (FUNCALL-SELF ':CLEAR-EOL)
		     (STORE-ARRAY-LEADER (SETQ LEN (1- LEN)) RUBOUT-HANDLER-BUFFER 0)
		     (SETQ RUBBED-OUT-SOME T)
		     (COND ((ZEROP LEN)
			    (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
			    (*THROW 'RUBOUT-HANDLER T))))))
	     ((LDB-TEST %%KBD-CONTROL-META CH)
	      (KBD-CONVERT-BEEP)))
	   (COND ((AND (ZEROP (FILL-POINTER RUBOUT-HANDLER-BUFFER))
		       (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
		  (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) 0)
		  (*THROW 'RUBOUT-HANDLER T))))
	  (T						;It's a self-inserting character
	   (COND ((MEMQ CH DO-NOT-ECHO)
		  (SETQ COLD-LOAD-STREAM-ACTIVATION-CHARACTER CH))
		 ((AND ACTIVATION-HANDLER
		       (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER)))
		  (SETQ CH `(:ACTIVATION ,CH 1))
		  (SETQ COLD-LOAD-STREAM-ACTIVATION-CHARACTER CH))
		 (T
		  (IF (LDB-TEST %%KBD-CONTROL-META CH)	;in :pass-through, but had bucky bits
		      (KBD-CONVERT-BEEP)
		    (FUNCALL-SELF ':TYO CH)
		    (ARRAY-PUSH-EXTEND RUBOUT-HANDLER-BUFFER CH))))
	   (COND ((AND (ATOM CH)
		       (LDB-TEST %%KBD-CONTROL-META CH)))	;do nothing
		 (RUBBED-OUT-SOME
		  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
		  (*THROW 'RUBOUT-HANDLER T))
		 (T
		  (STORE-ARRAY-LEADER (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0)
				      RUBOUT-HANDLER-BUFFER 1)
		  (SETQ COLD-LOAD-STREAM-ACTIVATION-CHARACTER NIL)
		  (RETURN CH))))))))

))
