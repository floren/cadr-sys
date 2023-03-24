;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.7
;;; Reason: converse doesn't kill text if recipient not found on any machine
;;;  c-end exits as advertised
;;; random-states print readably
;;; zwei variable documentation
;;; lozenged string pixel tweaking
;;; *print-array* and prinlength in inspector. c-\ is same as menu "set \"
;;; new inspectors use horizontal lisp listener/history. Does this win?
;;; Written 10-Dec-83 14:50:35 by Mly,
;;; while running on Lisp Machine Eighteen from band 4
;;; with Bad Experimental System 98.5, CADR 3.1, Inconsistently updated ZMail 53.2, MIT-Specific 22.0, microcode 305, ZM MIT.




; From file CONVER.LISP SRC:<L.IO1> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; CONVER  "

(DEFUN CONVERSE-SEND-MSG-INTERNAL (DESTINATION MESSAGE &OPTIONAL MAIL-P
				   &AUX USER HOST LOSSAGE)
  (LET ((DEST-LIST (PARSE-COMMAS-INTO-LIST DESTINATION))
	 LOSSAGE-REASON)
    (OR DEST-LIST (SETQ LOSSAGE T))
    (DOLIST (DEST DEST-LIST)
      (MULTIPLE-VALUE (DEST USER HOST)  ;;this now treats each user sperately 
	(PARSE-SINGLE-DEST DEST))
      #| (COND ((AND (NOT (NULL HOST)) (NOT MAIL-P) (NOT (CHAOS:ON-CHAOSNET-P HOST)))
	       (SETQ MAIL-P T)
	       (CONVERSE-PROBLEM
		 (FORMAT NIL "Message for ~A is being mailed, as host ~A is not on the ~
chaosnet." USER (FUNCALL (SI:PARSE-HOST HOST) ':NAME))))) |#
      (COND ((NULL HOST)
	     (SETQ LOSSAGE T)
	     (CONVERSE-PROBLEM
	       (STRING-APPEND "Message not sent because: "
		 (SETQ LOSSAGE-REASON
		       (FORMAT NIL "I could not find /"~A/" logged into any host." USER)))))
	    (T
	     (IF MAIL-P
		 (SEND-MESSAGE-STRING DEST MESSAGE)
	       (SETQ LOSSAGE-REASON (SEND-MSG DEST MESSAGE)
		     LOSSAGE (OR LOSSAGE LOSSAGE-REASON)))))
      (CONVERSE-RECORD-MSG-SENT DEST MESSAGE MAIL-P LOSSAGE-REASON LOSSAGE-REASON T)))
  (NOT LOSSAGE))

))

; From file CONVER.LISP SRC:<L.IO1> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; CONVER  "

(DEFUN CONVERSE-RECORD-MSG-SENT (DEST MESSAGE MAIL-P ERROR-P LOSSAGE-REASON
				 &OPTIONAL INSIDE-CONVERSE-P)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
			(IF (SEND C ':MY-NAME? DEST) (RETURN C)))))
    (IF (NULL CONVERSATION) (SETQ CONVERSATION (SETUP-CONVERSATION DEST)))
    (SEND CONVERSATION ':ADD-MSG
	  (FORMAT NIL
		  "Message ~:[~;NOT ~]~:[sent~;mailed~] to ~A (~\DATIME\)~@[ because~% ~A~]~%~A" 
		  ERROR-P MAIL-P DEST LOSSAGE-REASON MESSAGE))
    (WHEN INSIDE-CONVERSE-P
      ;; move the point to the To: line so user can type there
      (MOVE-BP (POINT) (SEND CONVERSATION ':AFTER-TO-LINE-BP))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT))))

))

; From file PRINT.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN WHICH-OPERATIONS-FOR-PRINT (STREAM &AUX TEM)
  (SETQ TEM (FUNCALL STREAM ':WHICH-OPERATIONS))
  (IF (MEMQ ':PRINT TEM)
      (IF (MEMQ ':STRING-OUT TEM)
	  '(:PRINT :STRING-OUT) '(:PRINT))
    (IF (MEMQ ':STRING-OUT TEM)
	'(:STRING-OUT) '())))

))

(REMPROP 'RANDOM-STATE 'NAMED-STRUCTURE-INVOKE)

; From file NUMER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(DEFSTRUCT (RANDOM-STATE :NAMED-ARRAY (:CONSTRUCTOR MAKE-RANDOM-STATE-1)
			 (:ALTERANT NIL)
			 (:PRINT-FUNCTION
			   #'(LAMBDA (RANDOM-STATE STREAM DEPTH)
			       (LET ((*PRINT-ARRAY* T))
				 (PRINT-NAMED-STRUCTURE 'RANDOM-STATE
					RANDOM-STATE DEPTH STREAM
					(WHICH-OPERATIONS-FOR-PRINT STREAM))))))

    RANDOM-SEED
    RANDOM-POINTER-1
    RANDOM-POINTER-2
    RANDOM-VECTOR)

))

; From file MACROS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MACROS  "

(DEFMACRO DEFVARIABLE (VAR INIT TYPE &BODY BODY)
  "Define a ZWEI option variable, initializing it.
VAR is the variable to be defined.  INIT is an expression to initialize it with.
TYPE is a keyword saying what kind of value the variable should have:
 :BOOLEAN	    T or NIL.
 :KEYWORD	    A symbol in the keyword package.
 :STRING 	    A string.
 :CHAR   	    A character as a fixnum.
 :CHAR-LIST	    A list of characters as fixnums.
 :FIXNUM   	    A fixnum.
 :FIXNUM-OR-NIL     A fixnum or NIL.
 :PIXEL		    A fixnum which is a number of pixels, horizontally.
 :PIXEL-OR-NIL	    A fixnum which is a number of pixels, horizontally, or NIL.
 :SMALL-FRACTION    A small flonum between 0.0s0 and 1.0s0, inclusively.
 :ANYTHING          Any Lisp object.
DOC is a documentation string for the variable.
All ZWEI option variables are reinitialized from their INITs
when ZWEI is initialized."
  (DECLARE (ARGLIST VAR INIT TYPE DOC))
  `(PROGN 'COMPILE
     (DEFINE-VARIABLE ',VAR ,INIT ',TYPE)
     (DEFCONST ,VAR ,INIT ,(CAR BODY))))

))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-VIEW-REGISTER "Display the contents of a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((SYM (GET-REGISTER-NAME "View register:")))
    (FORMAT T "~&REGISTER ~A:" SYM)
    (LET ((BP (CAR (GET SYM 'POINT))))
      (WHEN BP
	(FORMAT T "~& Position: Buffer ~A,~%~8T~A -|- ~A~%"
		(CDR (GET SYM 'POINT))
		(OR (SUMMARIZE-INTERVAL (FORWARD-LINE BP -1 T) BP) "")
		(OR (SUMMARIZE-INTERVAL BP (FORWARD-LINE BP 1 T)) ""))))
    (WHEN (GET SYM 'TEXT)
      (FORMAT T "~& Text:~%~A" (GET SYM 'TEXT)))
    (AND (NULL (PLIST SYM))
	 (FORMAT T "~& No text or saved position assigned.")))
  (FORMAT T "~&")
  DIS-NONE)

))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-JUMP-TO-SAVED-POSITION "Restore a saved position from a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
    (LET ((PT (GET Q-REG 'POINT)))
      (COND ((NULL PT)
	     (BARF "The register ~A doesn't point anywhere." Q-REG))
	    ((NEQ (CDR PT) *INTERVAL*)
	     (BARF "That register ~A doesn't point to this buffer." Q-REG)))
      (MOVE-BP (POINT) (CAR PT))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)))
  DIS-BPS)

))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFUN PRINT-VARIABLE-DOC (VAR &OPTIONAL (STREAM STANDARD-OUTPUT))
  "Print the short doc for ZWEI variable VAR on STREAM.
Adds some leading spaces and a trailing newline."
  (LET ((DOC (DOCUMENTATION VAR 'VARIABLE)))
    (LET ((FIRST-CR (STRING-SEARCH-CHAR #\CR DOC)))
      (FORMAT STREAM "    ~A~&" (IF FIRST-CR
				    (NSUBSTRING DOC 0 FIRST-CR)
				    DOC)))))

))

; From file COMF.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-LONG-DOCUMENTATION "Prints long documentation for the specified symbol or function.
Reads the name of the function or symbol from the mini-buffer
/(the default is the /"current/" function from the buffer).
First comes the arglist of the name as a function, if it is defined,
and then the documentation of the function.
Then, if the name is a symbol, comes the documentation for the name in its other roles." ()
  (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)))))
    (LET ((DOC (DOCUMENTATION NAME 'FUNCTION))
	  (ALL-DOC (AND (SYMBOLP NAME) (GET NAME 'SI:DOCUMENTATION-PROPERTY))))
      (IF (NOT (FDEFINEDP NAME))
	  (FORMAT T "~S:" NAME)
	(SEND STANDARD-OUTPUT ':FRESH-LINE)
	(PRINT-ARGLIST NAME STANDARD-OUTPUT))
      (COND (ALL-DOC
	     (LOOP FOR (KIND STRING) ON ALL-DOC BY 'CDDR
		   UNLESS (EQ KIND 'FUNCTION)
		   DO (FORMAT T "~&~%Documentation of ~S as a ~A:~%~A~%"
			      NAME (STRING-DOWNCASE KIND) STRING)))
	    (DOC
	     (FORMAT T "~&~A" DOC))
	    (T
	     (FORMAT QUERY-IO "~&~S is not documented." NAME)))))
  DIS-NONE)

))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-DESCRIBE-VARIABLE "Print documentation of editor user option variable.
Reads the name of a variable (using completion),
and prints its documentation string." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
	     "Variable name:" *VARIABLE-ALIST* NIL NIL
	     "You are typing the name of a variable to document.")))
    (COND ((EQUAL X "") (BARF))
	  (T (PRINT-VARIABLE (CDR X))
	     (FORMAT T "~A~&"
		     (DOCUMENTATION (CDR X) 'VARIABLE)))))
  DIS-NONE)

))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-SET-VARIABLE "Set editor user option variable, checking type.
Read the name of a variable (with completion), display current value
and documentation, and read a new variable.  Some checking is done
that the variable is the right type." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
	    "Variable name:" *VARIABLE-ALIST* NIL NIL
	    "You are typing the name of a variable to be set."
	    #'(LAMBDA (X)
		   (PRINT-VARIABLE (CDR X))
		   (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
     (AND (EQUAL X "") (BARF))
     (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))
     (PRINT-VARIABLE (CDR X))
     (LET* ((VAR (CDR X))
	    (*MINI-BUFFER-DEFAULT-STRING* (VARIABLE-STRING VAR))
	    (DEFAULT *MINI-BUFFER-DEFAULT-STRING*)
	    (PACKAGE (PKG-FIND-PACKAGE "ZWEI"))
	    (TYPE (GET VAR 'VARIABLE-TYPE))
	    (IBASE 10.) (BASE 10.))
       (SET VAR
	    (SELECTQ TYPE
	      (:CHAR
	       (LET ((V (TYPEIN-LINE-READLINE "New value (one character)")))
		 (OR (= (STRING-LENGTH V) 1) (BARF "~A is not one character." V))
		 (LDB %%CH-CHAR (AREF V 0))))
	      (:CHAR-LIST
	       (LET ((V (TYPEIN-LINE-READLINE-WITH-DEFAULT DEFAULT "New value (a string)")))
		 (DO ((I 0 (1+ I))
		      (RET)
		      (LIM (STRING-LENGTH V)))
		     (( I LIM) (NREVERSE RET))
		   (PUSH (LDB %%CH-CHAR (AREF V I)) RET))))
	      (:STRING
	       (TYPEIN-LINE-READLINE-WITH-DEFAULT DEFAULT "New value (a string)"))
	      ((:PIXEL :FIXNUM)
	       (LET ((V (TYPEIN-LINE-READ "New value (a fixnum)")))
		 (OR (FIXP V) (BARF "~S is not a fixnum." V))
		 V))
	      ((:FIXNUM-OR-NIL :PIXEL-OR-NIL)
	       (LET ((V (TYPEIN-LINE-READ "New value (NIL or a fixnum)")))
		 (OR (FIXP V) (NULL V) (BARF "~S is neither a fixnum not NIL." V))
		 V))
	      (:SMALL-FRACTION
	       (LET ((V (TYPEIN-LINE-READ "New value (a flonum between 0.0 and 1.0")))
		 (OR (FLOATP V) (BARF "~S is not a floating-point number." V))
		 (OR (AND ( V 0.0s0) ( V 1.0s0))
		     (BARF "~S is not between 0.0 and 1.0" V))
		 (SMALL-FLOAT V)))
	      (:BOOLEAN
	       (LET ((V (TYPEIN-LINE-READ "New value (T or NIL)")))
		 (OR (MEMQ V '(T NIL)) (BARF "~S is neither T nor NIL." V))
		 V))
	      (:KEYWORD
	       (LET ((V (TYPEIN-LINE-READ-WITH-DEFAULT DEFAULT "New value (a symbol)")))
		 (OR (SYMBOLP V) (BARF "~S is not a symbol." V))
		 V))
	      (:ANYTHING
	       (TYPEIN-LINE-READ-WITH-DEFAULT DEFAULT "New value"))))))
  DIS-NONE)

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-DISPLAY-LOZENGED-STRING (SHEET STRING)
  "Display STRING on SHEET inside a lozenge.
This is how special characters with no graphic or formatting meaning are output."
  (SETQ STRING (STRING STRING))
  (LET ((WIDTH (LOZENGED-STRING-WIDTH STRING)))
    ;; Make sure there is enough room on the line, if not CRLF and
    ;; hope the sheet isn't too narrow.  Relies on the fact that handling
    ;; of all exceptions leaves you no further to the right than you were
    ;; (usually at the left margin).
    (PREPARE-SHEET (SHEET)
      (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	  (SHEET-HANDLE-EXCEPTIONS SHEET))
      (COND ((> (+ (SHEET-CURSOR-X SHEET) WIDTH)
		(IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
		    (SHEET-INSIDE-RIGHT SHEET)
		    (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-WIDTH SHEET))))
	     (FUNCALL SHEET ':END-OF-LINE-EXCEPTION)))
      (SETF (SHEET-CURSOR-X SHEET)
	    (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL SHEET STRING
			(SHEET-CURSOR-X SHEET) (1+ (SHEET-CURSOR-Y SHEET))
			(SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-ALUF SHEET))))))

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-DISPLAY-LOZENGED-STRING-INTERNAL (SHEET STRING X0 Y0 XLIM ALUF)
  (LET ((WIDTH (LOZENGED-STRING-WIDTH STRING)))
    ;; Put the string then the box around it
    (LET ((X1 (1- (MIN (+ X0 WIDTH) XLIM)))
	  (Y1 (+ Y0 8)))
      (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING (+ X0 4) (+ Y0 2) NIL
				   X1
				   (FUNCALL (SHEET-GET-SCREEN SHEET)
					    ':PARSE-FONT-DESCRIPTOR FONTS:5X5)
				   ALUF)
      (%DRAW-RECTANGLE (- WIDTH 8) 1 (+ X0 4) Y0 ALUF SHEET)
      (%DRAW-RECTANGLE (- WIDTH 8) 1 (+ X0 4) Y1 ALUF SHEET)
      (%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) ALUF T SHEET)
      (%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) ALUF T SHEET)
      (%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) ALUF T SHEET)
      (%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) ALUF T SHEET)
      (1+ X1))))

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-CHARACTER-WIDTH (SHEET CH FONT &AUX TEM)
  "Returns the width of a character, in raster units.
For backspace, it can return a negative number.
For tab, the number returned depends on the current cursor position.
For return, the result is zero.
CH can be NIL; then you get the font's character-width."
  (COERCE-FONT FONT SHEET)
  (COND ((NULL CH) (FONT-CHAR-WIDTH FONT))
	((< CH 200)				;Ordinary printing character
	 (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT)) (AREF TEM CH))
	       (T (FONT-CHAR-WIDTH FONT))))
	((= CH #\CR) 0)				        ;Return
	((= CH #\TAB)				        ;Tab
	 (SETQ TEM (SHEET-TAB-WIDTH SHEET))
	 (- (* (TRUNCATE (+ (SHEET-CURSOR-X SHEET) TEM) TEM) TEM)
	    (SHEET-CURSOR-X SHEET)))
	((AND (= CH #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
	 (MINUS (SHEET-CHAR-WIDTH SHEET)))		;Backspace
	(T						;Misc lozenge character
	 (LET ((TEM (CAR (RASSOC CH SI:XR-SPECIAL-CHARACTER-NAMES))))
	   (LOZENGED-STRING-WIDTH (OR TEM "777"))))))

))

; From file SHWARM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN SHEET-STRING-OUT-EXPLICIT-1 (SHEET STRING START-X Y XLIM YLIM FONT ALU
				    &OPTIONAL (START 0) (END NIL)
				    MULTI-LINE-LINE-HEIGHT
				    &AUX FIT FWT LKT
				    (X START-X))
  "Output STRING on SHEET without using SHEET's cursor, font, etc.
Output starts at cursor position START-X, Y but SHEET's cursor is not moved.
Output stops if x-position XLIM or y-position YLIM is reached.
Font FONT is used, and alu-function ALU.
START and END specify a portion of STRING to be used.
MULTI-LINE-LINE-HEIGHT is how far to move down for Return characters;
 Return also moves back to x-position START-X.
 NIL means output <Return> with a lozenge.
All position arguments are relative to SHEET's outside edges."
  (DECLARE (RETURN-LIST FINAL-X FINAL-INDEX FINAL-Y))
  (COERCE-FONT FONT SHEET)
  (SETQ FIT (FONT-INDEXING-TABLE FONT)
	FWT (FONT-CHAR-WIDTH-TABLE FONT)
	LKT (FONT-LEFT-KERN-TABLE FONT))
  (OR XLIM (SETQ XLIM (SHEET-WIDTH SHEET)))
  (PREPARE-SHEET (SHEET)
    (DO ((I START (1+ I))
	 (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	 (WIDTH (FONT-CHAR-WIDTH FONT))
	 (CH))
	(( I N) (VALUES X Y I))
      (SETQ CH (AREF STRING I))
      (COND ((AND MULTI-LINE-LINE-HEIGHT (= CH #\RETURN))
	     (SETQ X START-X Y (+ Y MULTI-LINE-LINE-HEIGHT))
	     (IF (AND YLIM (> (+ Y MULTI-LINE-LINE-HEIGHT) YLIM))
		 (RETURN X Y I)))
	    (( CH 200)
	     (LET* ((STRING (STRING (OR (CAR (RASSOC CH SI:XR-SPECIAL-CHARACTER-NAMES))
					(FORMAT NIL "~3O" CH))))
		    (NX (+ X (LOZENGED-STRING-WIDTH STRING))))
	       (IF (> NX XLIM) (RETURN X Y I))
	       (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL SHEET STRING
						       X (1+ Y) XLIM ALU)
	       (SETQ X NX)))
	    (T (IF FWT (SETQ WIDTH (AREF FWT CH)))
	       (IF (> (+ X WIDTH) XLIM) (RETURN X Y I))
	       (DRAW-CHAR FONT CH
			  (IF LKT (- X (AREF LKT CH)) X)
			  Y ALU SHEET)
	       (SETQ X (+ X WIDTH)))))))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFCONST INSPECT-PRINLENGTH 1000
  "This value of PRINLENGTH is used while grinding lists in INSPECT.")

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (BASIC-INSPECT :OBJECT-LIST) (LIST)
  (MULTIPLE-VALUE-BIND (STRING-LIST ATOMIC-ITEMS LIST-ITEMS)
      (LET ((PRINLENGTH INSPECT-PRINLENGTH)
	    (PRINLEVEL INSPECT-PRINLEVEL))
	(GRIND-INTO-LIST LIST (TRUNCATE (SHEET-INSIDE-WIDTH) CHAR-WIDTH) T))
    ;; Turn STRING-LIST into a list of elements, one for each line, of the form
    ;; (NIL contents-string atom-item-list line-contains-lozenged-characters-p).
    (DO ((L STRING-LIST (CDR L))
	 (AIS ATOMIC-ITEMS (CDR AIS)))
	((NULL L))
      (LET ((LOZENGED-CHARACTERS
	      (DOTIMES (I (STRING-LENGTH (CAR L)))
		(IF ( (AREF (CAR L) I) 200)
		    (RETURN T)))))
	;; Convert the start and end indices for each atom-item from characters to pixels.
	;; If this line contains no lozenged characters,
	;; this can be done by multiplying.  Otherwise, SHEET-STRING-LENGTH must be used.
	(DOLIST (I (CAR AIS))
	  (SETF (THIRD I) (+ (SHEET-INSIDE-LEFT)
			     (IF LOZENGED-CHARACTERS
				 (SHEET-STRING-LENGTH SELF (CAR L) 0 (THIRD I))
			       (* (THIRD I) CHAR-WIDTH))))
	  (SETF (FOURTH I) (+ (SHEET-INSIDE-LEFT)
			      (IF LOZENGED-CHARACTERS
				  (SHEET-STRING-LENGTH SELF (CAR L) 0 (FOURTH I))
				(* (FOURTH I) CHAR-WIDTH)))))
	(RPLACA L (LIST NIL (CAR L) (CAR AIS) LOZENGED-CHARACTERS))))
    ;; Convert the starting and ending hpos of each list-item from characters to pixels
    ;; Must find the line which the start or end appears on
    ;; and see whether that line had any lozenged characters
    ;; to decide whether a multiplication is sufficient.
    (DOLIST (I LIST-ITEMS)
      (SETF (SECOND I)
	    (+ (SHEET-INSIDE-LEFT)
	       (LET ((LINE-DESC (NTH (THIRD I) STRING-LIST)))
		 (IF (FOURTH LINE-DESC)
		     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (SECOND I))
		   (* (SECOND I) CHAR-WIDTH)))))
      (SETF (FOURTH I)
	    (+ (SHEET-INSIDE-LEFT)
	       (LET ((LINE-DESC (NTH (FIFTH I) STRING-LIST)))
		 (IF (FOURTH LINE-DESC)
		     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (FOURTH I))
		   (* (FOURTH I) CHAR-WIDTH))))))
    (SETQ LIST-ITEMS (SORT LIST-ITEMS
			   #'(LAMBDA (X Y)
			       (COND ((< (THIRD Y) (THIRD X)) T)
				     ((> (THIRD Y) (THIRD X)) NIL)
				     (T (> (SECOND X) (SECOND Y)))))))
    (DO ((LINE (1- (LENGTH STRING-LIST)) (1- LINE))
	 (CURRENT LIST-ITEMS))
	((< LINE 0))
      (DO ()
	  ((OR (NULL CURRENT)
	       ( (THIRD (CAR CURRENT)) LINE)))
	(SETQ CURRENT (CDR CURRENT)))
      (RPLACA (CAR (NTHCDR LINE STRING-LIST)) CURRENT))
    (PROG () (RETURN STRING-LIST ':LIST-STRUCTURE 'INSPECT-LIST-PRINTER))))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFCONST INSPECT-FRAME-ITEM-LIST
     '(("Exit" :VALUE :EXIT
	:DOCUMENTATION "Exit the inspector, returning NIL.")
       ("Return" :VALUE :RETURN
	:DOCUMENTATION "Exit the inspector, returning a value.")
       ("Clear" :VALUE :CLEAR
	:DOCUMENTATION "Remove all objects from the history.")
       ("DeCache" :VALUE :FLUSH-CACHE
	:DOCUMENTATION
	"Delete saved display info.  Useful if you are looking at objects that have changed.")
       ("Modify" :VALUE :MODIFY
	:DOCUMENTATION "Modify a slot by pointing at it then choosing a new value.")
       ("Set \" :VALUE :SET-\
	:DOCUMENTATION "Set the value of the symbol \ by choosing an object."))
  "Menu item-alist for the menu in the INSPECT frame.")

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (INSPECT-FRAME :BEFORE :INIT) (PLIST &AUX IO-BUFFER)
  (LET ((NOI (OR (GET PLIST ':NUMBER-OF-INSPECTORS) 3))
	(NAMES NIL))
    (SETQ IO-BUFFER (MAKE-DEFAULT-IO-BUFFER))
    (SETQ PANES (LIST `(INTERACTOR INTERACTION-PANE :LABEL NIL
				   		    :IO-BUFFER ,IO-BUFFER
						    :MORE-P NIL)
		      `(HISTORY INSPECT-HISTORY-PANE-WITH-MARGIN-SCROLLING
				:IO-BUFFER ,IO-BUFFER
				:SCROLL-BAR 3)
		      `(MENU COMMAND-MENU-PANE
			     :FONT-MAP ,(LIST FONTS:CPTFONT)
			     :ITEM-LIST ,INSPECT-FRAME-ITEM-LIST
			     :IO-BUFFER ,IO-BUFFER)))
    (DOTIMES (I NOI)
      (LET ((NAME1 (INTERN (FORMAT NIL "INSPECTOR-~D" I) "TV")))
	(PUSH `(,NAME1 ,(IF (= I (1- NOI))
			    'INSPECT-PANE-WITH-TYPEOUT
			  'INSPECT-PANE)
		:SCROLL-BAR 3
		:IO-BUFFER ,IO-BUFFER) PANES)
	(PUSH NAME1 NAMES)))
    (SETQ INSPECTORS NAMES)
    (SETQ CONSTRAINTS `((MAIN . ((INTERACTOR-AND-HISTORY MENU . ,(REVERSE NAMES))
				 ((INTERACTOR-AND-HISTORY
				    :HORIZONTAL (:LIMIT (3 NIL :LINES HISTORY)
							0.20s0 :LINES HISTORY)
				    (INTERACTOR HISTORY)
				    ((HISTORY 0.55s0))
				    ((INTERACTOR :EVEN))))
				 ((MENU :ASK :PANE-SIZE))
				 (,@(MAPCAR #'(LAMBDA (NAME1)
						`(,NAME1 :LIMIT (1 30. :LINES)
						  ,(// 0.30s0 (1- NOI))
						  :LINES))
					    (CDR NAMES)))
				  ((,(CAR NAMES) :EVEN))))))))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN INSPECT-COMMAND-LOOP (FRAME &AUX USER IS HISTORY)
  (FUNCALL (SETQ USER (FUNCALL FRAME ':GET-PANE 'INTERACTOR)) ':CLEAR-SCREEN)
  (FUNCALL (CAR (SETQ IS (FUNCALL FRAME ':INSPECTORS))) ':FLUSH-TYPEOUT)
  (FUNCALL USER ':SET-OLD-TYPEAHEAD NIL)
  (SETQ HISTORY (FUNCALL FRAME ':GET-PANE 'HISTORY))
  ;; Flush remnants of modify mode
  (FUNCALL HISTORY ':SET-SENSITIVE-ITEM-TYPES T)
  (DOLIST (I IS)
    (FUNCALL I ':SET-MODIFY-MODE NIL))
  (LET* ((TYPEOUT-WINDOW (FUNCALL FRAME ':TYPEOUT-WINDOW))
	 (TERMINAL-IO TYPEOUT-WINDOW)
	 * ** *** + ++ +++ \
	 *PRINT-ARRAY*
	 (STANDARD-INPUT SI:SYN-TERMINAL-IO)
	 (STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
	 (TV:KBD-INTERCEPTED-CHARACTERS
	   (REMOVE (ASSQ #\BREAK TV:KBD-INTERCEPTED-CHARACTERS)
		   TV:KBD-INTERCEPTED-CHARACTERS))
	 (THING) (TOP-ITEM))
    (DECLARE (SPECIAL \))
    (DO-NAMED INSPECTOR ()
	      (())
      (LET ((ITEMS (FUNCALL HISTORY ':ITEMS))
	    (IW)
	    (IDX))
	(SETQ IDX (ARRAY-ACTIVE-LENGTH ITEMS))
	;; Make sure the inspection windows reflect the state of the history buffer
	(DOLIST (I IS)
	  ;; Update datastructure to reflect current TOP-ITEMs
	  (LET ((DISP (FUNCALL I ':CURRENT-DISPLAY)))
	    (AND DISP (SETF (FOURTH DISP) (FUNCALL I ':TOP-ITEM)))))
	(DOTIMES (I (LENGTH IS))
	  (SETQ IDX (1- IDX))
	  (SETQ IW (NTH I IS))
	  (COND ((< IDX 0)
		 (FUNCALL IW ':SET-CURRENT-DISPLAY
			  (FUNCALL IW ':SETUP
				   `(INSPECT-PRINTER NIL NIL NIL
						     (NIL NIL NIL NIL
							  ,(LABEL-FONT (FUNCALL IW ':LABEL))
							  "Empty"))))
		 (FUNCALL IW ':SET-CURRENT-OBJECT (NCONS NIL)))
		(T (FUNCALL HISTORY ':INSPECT-OBJECT (AREF ITEMS IDX) IW TOP-ITEM NIL T)
		   (SETQ TOP-ITEM NIL)))))
      
      ;; Insure last item in history is on the screen
      (FUNCALL HISTORY ':PUT-LAST-ITEM-IN-WINDOW)
      
      ;; Give *, ** and *** the right values.
      (SETQ *PRINT-ARRAY* NIL)
      (LET* ((ITEMS (FUNCALL HISTORY ':ITEMS))
	     (NITEMS (IF ITEMS (ARRAY-ACTIVE-LENGTH ITEMS) 0)))
	(AND ( NITEMS 1) (SETQ * (AREF ITEMS (- NITEMS 1))))
	(AND ( NITEMS 2) (SETQ ** (AREF ITEMS (- NITEMS 2))))
	(AND ( NITEMS 3) (SETQ *** (AREF ITEMS (- NITEMS 3)))))
      
      ;; Get input.
      ;; Keyboard commands are processed inside this loop.
      ;; Mouse commands exit the loop and go round the outer loop.
      (DO-FOREVER
	(SETQ THING -1)
	(FUNCALL (CAR IS) ':FLUSH-TYPEOUT)
	(FUNCALL FRAME ':SELECT-PANE USER)
	(FUNCALL USER ':FRESH-LINE)
	(OR (FUNCALL USER ':OLD-TYPEAHEAD)
	    (SETQ THING (FUNCALL USER ':ANY-TYI)))
	(UNLESS (NUMBERP THING)
	       ;; Some sort of mouse command, just process
	  (RETURN))
	(SELECTQ THING
	  ((#\C-Z #\ABORT)
	   (SIGNAL EH:ABORT-OBJECT))
	  (#\C-V
	   (FUNCALL (CAR IS) ':SCROLL-TO
		    (- (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)) 2)
		    ':RELATIVE))
	  (#\M-V
	   (FUNCALL (CAR IS) ':SCROLL-TO
		    (- 2 (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)))
		    ':RELATIVE))
	  (#\BREAK
	   (FUNCALL FRAME ':SELECT-PANE (CAR IS))
	   (FUNCALL TERMINAL-IO ':EXPOSE-FOR-TYPEOUT)
	   (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to inspector command loop.")
	     (BREAK 'INSPECT))
	   (FUNCALL TERMINAL-IO ':MAKE-COMPLETE))
	  ;; Clear-Screen decaches.
	  (#\CLEAR-SCREEN
	   (FUNCALL HISTORY ':SET-CACHE NIL)
	   (FUNCALL FRAME ':CLEAR-SCREEN)
	   (FUNCALL FRAME ':REFRESH ':COMPLETE-REDISPLAY))
	  ;; End returns *.
	  (#\END
	   (RETURN-FROM INSPECTOR *))
	  (#\HELP
	   (INSPECT-HELP)
	   (FORMAT TERMINAL-IO "~%Type any character to continue:")
	   (LET ((CH (FUNCALL USER ':ANY-TYI)))
	     (OR (= CH #\SP)
		 (FUNCALL USER ':UNTYI CH))))
	  (#\DELETE
	   (RETURN (FUNCALL HISTORY ':FLUSH-CONTENTS)))
	  ;;set \
	  (#\C-\
	   (FORMAT USER "~&Value to set \ to ")
	   (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
	       (INSPECT-GET-VALUE-FROM-USER USER)
	     (OR PUNT-P (SETQ \ VALUE))))
	  (#\RUBOUT)
	  (#\QUOTE
	   (LET ((TERMINAL-IO USER)
		 FLAG)
	     (FORMAT USER "Eval: ")
	     (MULTIPLE-VALUE (THING FLAG)
	       (FUNCALL USER ':RUBOUT-HANDLER
			'((:FULL-RUBOUT :FULL-RUBOUT)) #'SI:READ-FOR-TOP-LEVEL))
	     (COND ((NEQ FLAG ':FULL-RUBOUT)
		    (SETQ +++ ++ ++ + + THING)
		    (MULTIPLE-VALUE (THING FLAG) (CATCH-ERROR (EVAL THING)))
		    (OR FLAG
			(LET ((PRINLEVEL 3) (PRINLENGTH 5))
			  (PRINT THING USER)))))))
	  (OTHERWISE
	   (LET ((TERMINAL-IO USER)
		 FLAG)
	     (AND ( THING 0) (FUNCALL USER ':UNTYI THING))
	     (MULTIPLE-VALUE (THING FLAG)
	       (FUNCALL USER ':PREEMPTABLE-READ
			'((:FULL-RUBOUT :FULL-RUBOUT)) #'SI:READ-FOR-TOP-LEVEL))
	     (COND ((EQ FLAG ':MOUSE-CHAR) (RETURN))
		   ((NEQ FLAG ':FULL-RUBOUT)
		    (SETQ +++ ++ ++ + + THING)
		    (MULTIPLE-VALUE (THING FLAG) (CATCH-ERROR (EVAL THING)))
		    (OR FLAG
			(RETURN (SETQ THING `(:VALUE ,THING ,HISTORY))))))))))
      (CATCH-ERROR-RESTART (SYS:ABORT "Return to inspector command loop.")
	(COND
	  ((NLISTP THING))
	  ((EQ (CAR THING) ':MENU)
	   (SETF (SECOND THING) (FUNCALL (FOURTH THING) ':EXECUTE (SECOND THING)))
	   (SELECTQ (SECOND THING)
	     (:EXIT (RETURN *))
	     (:RETURN
	      (FORMAT USER "~&Value to return ")
	      (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
		  (INSPECT-GET-VALUE-FROM-USER USER)
		(OR PUNT-P (RETURN VALUE))))
	     (:FLUSH-CACHE
	      (FUNCALL HISTORY ':SET-CACHE NIL))
	     (:MODIFY
	      (SETQ TOP-ITEM (INSPECT-MODIFY-OBJECT USER HISTORY IS)))
	     (:CLEAR
	      (FUNCALL HISTORY ':FLUSH-CONTENTS))
	     (:SET-\
	      (FORMAT USER "~&Value to set \ to ")
	      (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
		  (INSPECT-GET-VALUE-FROM-USER USER)
		(OR PUNT-P (SETQ \ VALUE))))
	     (OTHERWISE (FORMAT USER "~&Unimplemented menu command ~A~%" (SECOND THING)))))
	  (T
	   (COND ((NULL (FIRST THING))
		  ;; Type is NIL -- nothing under mouse
		  (BEEP))
		 ((AND (EQ (FIRST THING) ':LINE-AREA) (EQ (FOURTH THING) #\MOUSE-2-1))
		  ;; Delete from line area
		  (FUNCALL HISTORY ':FLUSH-OBJECT (INSPECT-REAL-VALUE THING)))
		 ((AND (EQ (FOURTH THING) #\MOUSE-2-1)
		       (MEMQ (THIRD THING) IS))
		  ;; Middle click means leave source in one of the windows
		  (LET ((1ST-THING (INSPECT-REAL-VALUE THING))
			(2ND-THING (FUNCALL (THIRD THING) ':CURRENT-OBJECT)))
		    ;; First flush item we will be inspecting
		    (INSPECT-FLUSH-FROM-HISTORY 1ST-THING HISTORY)
		    (INSPECT-FLUSH-FROM-HISTORY 2ND-THING HISTORY)
		    (FUNCALL HISTORY ':APPEND-ITEM 2ND-THING)
		    (FUNCALL HISTORY ':APPEND-ITEM 1ST-THING)))
		 ((EQ (FOURTH THING) #\MOUSE-3-1)
		  ;; Click on right button -- try to find function
		  (SETQ THING (INSPECT-FIND-FUNCTION (INSPECT-REAL-VALUE THING)))
		  (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
		  (FUNCALL HISTORY ':APPEND-ITEM THING))
		 ((KEY-STATE ':HYPER)
		  ;; HYPER means modify the slot we are pointing at.
		  (LET ((TERMINAL-IO (THIRD THING)))
		    (IF (OR (NULL (FIRST THING)) (NULL (GET (FIRST THING) 'SET-FUNCTION)))
			(FORMAT TERMINAL-IO "~&Cannot set this component.")
		      (INSPECT-SET-SLOT THING HISTORY USER))))
		 (T
		  ;; Otherwise inspect the thing we are pointing at.
		  (SETQ THING (INSPECT-REAL-VALUE THING))
		  (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
		  (FUNCALL HISTORY ':APPEND-ITEM THING)))))))))

))

; From file GRIND.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; GRIND  "

(DEFVAR GRINDEF)

))


; From file PATCH.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN PRINT-PATCH (MAJOR-VERSION-NUMBER PATCH-VERSION-DESC)
  (FORMAT T "~&~D.~D ~9T~7A "
	  MAJOR-VERSION-NUMBER
	  (VERSION-NUMBER PATCH-VERSION-DESC)
	  (STRING-APPEND (VERSION-AUTHOR PATCH-VERSION-DESC) ":"))
  (WHEN (VERSION-UNRELEASED PATCH-VERSION-DESC)
    (SEND STANDARD-OUTPUT ':STRING-OUT "(unreleased)  "))
  (TERPRI)
  (TV:PRINT-STRING-WITH-INDENTATION
    STANDARD-OUTPUT
    (VERSION-EXPLANATION PATCH-VERSION-DESC)
    12.))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN COM-HELP (SG ERROR-OBJECT &REST IGNORE)
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  (FORMAT T
	  "You are in the debugger.  If you don't want to debug this error, type ~C.
Otherwise you can evaluate expressions in the context of the error,
examine the stack, and proceed//throw//return to recover.

If you type in a Lisp form, it will be evaluated, and the result printed.
Type ~C or c-Z to get back to top level, or the previous debugger level.
While in the debugger, c-G quits back to the debugger top level.

If you think this error indicates a bug in the Lisp machine system,
use the control-M command.
"
	  #\ABORT #\HELP #\ABORT)

  (FORMAT T "For more help, type a letter to specify a topic:
  I - examining information
  F - selecting stack frames to examine
  S - stepping through the program
  X - resuming execution
  T - transfer to editor, window-oriented debugger, or bug report editor
What topic? ")
  (LET ((CH (CHAR-UPCASE (TYI))))
    (SELECTQ CH
      (#\F
       (FORMAT:OUTPUT T
	 "
Selecting stack frames:

c-N or "
	 (FORMAT:OCHAR #\LINE ':LOZENGED)
	 " goes down a frame, c-P or "
	 (FORMAT:OCHAR #\RETURN ':LOZENGED)
	 " goes up.
m-N and m-P are similar but show args, locals and compiled code.
c-m-N and c-m-P are similar to c-N and c-P, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
m-< and m-> go to the top and bottom of the stack, respectively.
c-S reads a string and searches down the stack for a frame
   calling a function whose name contains that substring.
"))
      (#\I
        (FORMAT T "
Information display:

c-L or ")
	(SEND STANDARD-OUTPUT
	      (IF (SEND STANDARD-OUTPUT ':OPERATION-HANDLED-P ':DISPLAY-LOZENGED-STRING)
		  ':DISPLAY-LOZENGED-STRING
		':STRING-OUT)
	      "Clear-screen")
	(FORMAT T " clears screen and retypes error message.
m-L clears screen and types args, locals and compiled code.
c-B gives a backtrace of function names.
m-B gives a backtrace of function names and argument names and values.
c-m-B is line m-B but shows EVALs, PROGs, CONDs, etc.
c-m-A prints an argument to the current function, and sets * to be that
   argument to let you do more complicated things with it.
   + is set to a locative to that argument, should you want to modify it.
   To specify which argument, type the argument number with Control
   or Meta held down before the c-m-A.
c-m-L is like c-m-A but works on the function's locals rather than the args.
c-m-V is like c-m-A but works on the values this frame is returning.
   (This is useful when you get a trap on exit from function).
c-m-F does likewise for the function itself.
c-A prints the arglist of the function in the current frame.
m-S prints the value in this frame of a special variable you specify.
c-m-S lists all special variable bindings in this frame.

Do (EH-ARG n), (EH-LOC n), (EH-VAL n) or (EH-FUN) to get the value of
an arg, local, value or function-object from an expression being evaluated.
For args and locals, n can be a name or a number.  EH-VAL allows numbers only.
LOCF and SETF on those expressions are also allowed.
"))
      (#\T
        (FORMAT T "
Transfer to other systems:

c-E calls the editor to edit the current function.
c-M enters the editor to send a bug message, and puts the error
  message and a backtrace into the message automatically.
  A numeric argument says how many stack frames to put in the backtrace.
c-m-W switches to the window-based debugger.
"))
      (#\S
       (IF ERROR-HANDLER-RUNNING
	   (FORMAT T "
Stepping commands:

c-X toggles the trap-on-exit flag for the current frame.
m-X sets the trap-on-exit flag for the current frame and all outer frames.
c-m-X clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
c-D proceeds like ~C, but first sets the trap-on-next-function-call flag.
m-D toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with c-X.
" #\RESUME)
	 (FORMAT T "
You cannot use the stepping commands, since you are in examine-only mode
")))
      (#\X
       (FORMAT T "
Commands to continue execution:

")
       (COND (ERROR-HANDLER-RUNNING
	      (FORMAT T "c-Z aborts to previous debugger or other command loop, or to top level.
c-R returns a value or values from the current frame.
c-m-R offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
m-R offers to reinvoke the current frame, letting you alter
   some of the arguments, or use more or fewer arguments.
c-T throws to a specific tag.")
	      (DESCRIBE-PROCEED-TYPES SG ERROR-OBJECT))
	     (T
	      (FORMAT T "You cannot continue execution, since you are in examine-only mode.
~C exits the debugger." #\RESUME))))
      (T (FORMAT T "~%~C is not a known topic" CH)))))

))

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFCONST FED-REGISTER-OP-MENU-ALIST
	  '(("Clear register" :VALUE (:CLEAR-REG))
	    ("Save black" :VALUE (:LOAD-REG))
	    ("Save gray" :VALUE (:LOAD-REG-GRAY))
	    ("Restore to black" :VALUE (:LOAD-BLACK))
	    ("" :NO-SELECT NIL)
	    ("Merge Operations" :NO-SELECT NIL
	     :FONT :MENU-STANDOUT)
	    ("Copy to black" :VALUE (:MERGE-BLACK :COPY))
	    ("Copy to gray" :VALUE (:MERGE-GRAY :COPY))
	    ("Set bits in black" :VALUE (:MERGE-BLACK :SET))
	    ("Set bits in gray" :VALUE (:MERGE-GRAY :SET))
	    ("Clear bits in black" :VALUE (:MERGE-BLACK :CLEAR))
	    ("Clear bits in gray" :VALUE (:MERGE-GRAY :CLEAR))
	    ("Flip bits in black" :VALUE (:MERGE-BLACK :FLIP))
	    ("Flip bits in gray" :VALUE (:MERGE-GRAY :FLIP))))

))

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFMETHOD (REGISTER-PANE :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P
      INHIBIT-CHAR-BOX
      (PROGN (FUNCALL-SELF ':MUST-REDISPLAY REDISPLAY-ALL)
	     (FUNCALL-SELF ':REDISPLAY T))))

))

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFMETHOD (REGISTER-PANE :AFTER :MERGE-CONTENTS) (&REST IGNORE)
  (SETQ INHIBIT-CHAR-BOX NIL)
  (FUNCALL-SELF ':REDISPLAY T))

))

; From file FED.LISP SRC:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFMETHOD (REGISTER-PANE :AFTER :SET-CONTENTS) (&REST IGNORE)
  (SETQ INHIBIT-CHAR-BOX NIL)
  (FUNCALL-SELF ':REDISPLAY T))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN RENAME-FILE (STRING-OR-STREAM NEW-NAME &KEY (ERROR T) QUERY)
  "Rename a file, specified as a pathname, string or I//O stream.
QUERY, if true, means ask before each rename occurs."
  (RENAMEF STRING-OR-STREAM NEW-NAME ERROR QUERY))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN DELETE-FILE (STRING-OR-STREAM &KEY (ERROR T) QUERY)
  "Delete a file, specified as a pathname, string or I//O stream.
QUERY, if true, means to ask before deleting each file."
  (DELETEF STRING-OR-STREAM ERROR QUERY))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN UNDELETE-FILE (STRING-OR-STREAM &KEY (ERROR T) QUERY)
  "Undelete a file, specified as a pathname, string or I//O stream.
Only certain file servers support undeletion.
QUERY, if true, means to ask before undeleting each file."
  (UNDELETEF STRING-OR-STREAM ERROR QUERY))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFMETHOD (PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  LINK-TO
  (LET ((CONDITION
	  (MAKE-CONDITION 'UNKNOWN-OPERATION "Can't create links on file system of ~A."
			  SELF ':CREATE-LINK)))
    (IF ERROR (SIGNAL CONDITION) CONDITION)))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN CREATE-LINK-CHAOS (HOST LINK LINK-TO ERROR-P &AUX HOST-UNIT PKT SUCCESS STRING)
  (FILE-OPERATION-RETRY
    (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT))
    (MULTIPLE-VALUE (PKT SUCCESS STRING)
      (FUNCALL HOST-UNIT ':COMMAND NIL NIL NIL
	       "CREATE-LINK" #\CR (FILE-PRINT-PATHNAME LINK) #\CR
	       (FILE-PRINT-PATHNAME LINK-TO)))
    (UNWIND-PROTECT
	(UNLESS SUCCESS
	  (FILE-PROCESS-ERROR-NEW STRING LINK NIL (NOT ERROR-P) ':CREATE-LINK))
      (CHAOS:RETURN-PKT PKT))))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (ITS-PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  (CREATE-LINK-CHAOS HOST SELF LINK-TO ERROR))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (MULTICS-PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  (CREATE-LINK-CHAOS HOST SELF LINK-TO ERROR))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (LMFS-PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  (CREATE-LINK-CHAOS HOST SELF LINK-TO ERROR))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (LMFILE-PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  (CREATE-LINK-CHAOS HOST SELF LINK-TO ERROR))

))

(LOAD '#FS#:LOGICAL-PATHNAME "SYS: FONTS; HL12BI QFASL >" "FONTS" NIL NIL)
(TV:UPDATE-FONT-MAPS)
