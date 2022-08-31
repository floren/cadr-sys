;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 22-Dec-83 02:30:20 by Mly,
;;; Reason: Reading from editor host (ed-buffer, ed, ed-file) bug.
;;; TV:ADD-SYSTEM-KEY remembers all key bindings to a particular key,
;;;   but only displays most recent. TV:REMOVE-SYSTEM-KEY pops.
;;; WITH-OUTPUT-TO-STRING bug.
;;; {SUB-}APROPOS takes new keywords :BOUNDP and :FBOUNDP
;;;    -- same as :PREDICATE 'BOUNDP etc.
;;; Mouse blips in zwei.  View window says when it's at end
;;; Printing histories and readtables.
;;; Document flavors -- use (DOCUMENTATION FOO-FLAVOR 'DEFFLAVOR) to access.
;;; while running on Lisp Machine One from band 6
;;; with Bad Inconsistently updated System 98.11, CADR 3.2, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 305, ZM MIT.



; From file HOST.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HOST  "

(defun ED-PATHNAME-OPEN (ignored pathname
			 &key &optional (characters t)
			 (direction ':input)
			 (error t)
			 (if-exists ':append)
			 (if-does-not-exist
			   (if (eq direction ':output) ':create
			     ':error))
			 &allow-other-keys)
  "parse OPEN keywords and then call the :REALLY-OPEN method"
  (let ((stream
	  (funcall-self ':REALLY-OPEN pathname characters error
			(eq if-does-not-exist ':create))))
    (if (and (eq direction ':output)
	     (eq if-exists ':append))
	(send stream ':read-until-eof))
    stream))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN ADD-SYSTEM-KEY (CHAR WINDOW-OR-FLAVOR DOCUMENTATION &OPTIONAL (CREATE T))
  "Make typing the System key followed by CHAR select the window WINDOW-OR-FLAVOR.
WINDOW-OR-FLAVOR may be:
  an actual window
  a name of a flavor of window
  a list to evaluate to get a window or a flavor name.
CREATE says whether and how to create a new window if Control-char is pressed.
  It may be:
  T meaning create a window of flavor PROGRAM, or
  the name of a flavor to create, or
  a list to evaluate for effect, to create and select a window.
If CHAR is already defined to select a flavor window, then the old version is
  remembered. To restore the former definition, use (TV:REMOVE-SYSTEM-KEY CHAR)"
  (PUSH (LIST (CHAR-UPCASE CHAR) WINDOW-OR-FLAVOR DOCUMENTATION CREATE)
	*SYSTEM-KEYS*)
  (SETQ *SYSTEM-KEYS* (STABLE-SORTCAR *SYSTEM-KEYS* #'ALPHALESSP)))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN REMOVE-SYSTEM-KEY (CHAR)
  "Remove any definition for CHAR typed after the System key,
or the latest definition if there is more than one."
  (SETQ *SYSTEM-KEYS*
	(DELETE-IF #'(LAMBDA (ELT) (CHAR-EQUAL (CAR ELT) CHAR))
		   *SYSTEM-KEYS* ':COUNT 1)))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFUN USER-ACTIVITY-STRING ()
  (LET ((W (DO ((W TV:SELECTED-WINDOW (SEND W ':SUPERIOR))
		(W1 NIL W))
	       ((OR (NULL W) (TYPEP W 'TV:SCREEN)) W1))))
    (COND ((NULL W) SI:LOCAL-HOST-NAME)
	  ((TYPEP W 'SUPDUP) "Supdup")
	  ((TYPEP W 'ZWEI:ZMACS-FRAME) "Zmacs")
	  ((TYPEP W 'TV:PEEK-FRAME) "Peek")
	  ((TYPEP W 'ZWEI:CONVERSE-FRAME) "Converse")
	  ((TYPEP W 'TV:INSPECT-FRAME) "Inspect")
	  ((TYPEP W 'TV:LISP-LISTENER) "Lisp")
	  ((TYPEP W 'TELNET) "Telnet")
	  ((TYPEP W 'FED:FED-FRAME) "Font Edit")
	  ((IGNORE-ERRORS (TYPEP W 'ZWEI:ZMAIL-FRAME)) "ZMail")
	  (T SI:LOCAL-HOST-NAME))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defmacro with-output-to-string ((stream string index) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
If STRING is omitted, a new string with no fill pointer is created and returned.
If STRING is supplied, that string's contents are modified destructively,
and the values of BODY's last expression are returned.
If INDEX is supplied, it should be a SETFable accessor which describes
where to find the index to store into STRING, instead of at the end.
The value of INDEX will be updated after the BODY is finished."
  (multiple-value-bind (realbody decls)
      (extract-declarations body)
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

; From file QMISC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN APROPOS (SUBSTRING &REST ARGS &KEY &OPTIONAL ((:PACKAGE PKG))
		(INHERITORS NIL) (INHERITED T) DONT-PRINT
		PREDICATE BOUNDP FBOUNDP &ALLOW-OTHER-KEYS)
  "Find all symbols in one or more packages whose names contain SUBSTRING, or
containing each string in it, if SUBSTRING is a list of strings.
If PREDICATE is non-NIL, it is a function to be called with a symbol as arg;
only symbols for which the predicate returns non-NIL will be mentioned.
If BOUNDP is non-NIL, then only bound symbols are mentioned. Likewise FBOUNDP.
The :PACKAGE argument defaults to NIL, meaning do all packages.
The packages which USE that package are processed also, unless :INHERITORS is NIL.
The packages USEd by that package are processed also, unless :INHERITED is NIL.
/(Any other packages which inherit from them also are NOT processed in any case.)
The symbols are printed unless DONT-PRINT is set.
A list of the symbols found is returned."
  (DECLARE (ARGLIST SUBSTRING &KEY &OPTIONAL ((:PACKAGE PKG))
		    (INHERITORS NIL) (INHERITED T)
		    PREDICATE BOUNDP FBOUNDP DONT-PRINT))
  (IF (= (LENGTH ARGS) 1)
      (SETQ PKG (CAR ARGS)))
  (LET (RETURN-LIST
	(APROPOS-PREDICATE PREDICATE)
	(APROPOS-DONT-PRINT DONT-PRINT)
	(APROPOS-SUBSTRING SUBSTRING)
	(APROPOS-BOUNDP BOUNDP)
	(APROPOS-FBOUNDP FBOUNDP))
    (DECLARE (SPECIAL RETURN-LIST APROPOS-PREDICATE APROPOS-SUBSTRING APROPOS-DONT-PRINT
		      APROPOS-BOUNDP APROPOS-FBOUNDP))
    (COND (PKG
	   (MAPATOMS #'APROPOS-1 PKG INHERITED)
	   (AND INHERITORS
		(DOLIST (P (PACKAGE-USED-BY-LIST PKG))
		  (MAPATOMS #'APROPOS-1 P))))
	  (T
	   (DOLIST (P *ALL-PACKAGES*)
	     (MAPATOMS #'APROPOS-1 P))))
    RETURN-LIST))

))

; From file QMISC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN APROPOS-1 (SYMBOL &AUX (P (GET-PNAME SYMBOL)))
  (DECLARE (SPECIAL RETURN-LIST APROPOS-PREDICATE APROPOS-SUBSTRING
		    APROPOS-DONT-PRINT APROPOS-BOUNDP APROPOS-FBOUNDP))
  (COND ((AND (IF (LISTP APROPOS-SUBSTRING)
		  (DOLIST (S APROPOS-SUBSTRING T)
		    (UNLESS (STRING-SEARCH S P) (RETURN NIL)))
		(STRING-SEARCH APROPOS-SUBSTRING P))
	      (OR (NOT APROPOS-BOUNDP) (BOUNDP SYMBOL))
	      (OR (NOT APROPOS-FBOUNDP) (FBOUNDP SYMBOL))
	      (NOT (MEMQ SYMBOL RETURN-LIST))
	      (OR (NULL APROPOS-PREDICATE)
		  (FUNCALL APROPOS-PREDICATE SYMBOL)))
	 (PUSH SYMBOL RETURN-LIST)
	 (OR APROPOS-DONT-PRINT
	     (PROGN
	       ;; Binding the package to NIL forces the package to be printed.
	       ;; This is better than explicitly printing the package, because
	       ;; this way you get the "short" version.
	       (LET ((PACKAGE NIL))
		 (FORMAT T "~%~S" SYMBOL))
	       (AND (FBOUNDP SYMBOL)
		    (FORMAT T " - Function ~:S" (ARGLIST SYMBOL)))
	       (AND (BOUNDP SYMBOL)
		    (COND ((FBOUNDP SYMBOL) (PRINC ", Bound"))
			  (T (PRINC " - Bound"))))
	       (AND (GET SYMBOL 'FLAVOR)
		    (COND ((OR (BOUNDP SYMBOL) (FBOUNDP SYMBOL))
			   (PRINC ", Flavor"))
			  (T (PRINC " - Flavor")))))))))

))

; From file QMISC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN SUB-APROPOS (SUBSTRING STARTING-LIST &KEY PREDICATE BOUNDP FBOUNDP DONT-PRINT)
  "Find all symbols in STARTING-LIST whose names contain SUBSTRING, or
containing each string in it, if SUBSTRING is a list of strings.
If :PREDICATE is set, it should be a function of one arg;
only symbols for which the predicate returns non-NIL are included.
If :BOUNDP is set, then only bound symbols are included. Likewise with FBOUNDP.
The symbols are printed unless :DONT-PRINT is set.
A list of the symbols found is returned."
  (LET (RETURN-LIST
	(APROPOS-PREDICATE PREDICATE)
	(APROPOS-SUBSTRING SUBSTRING)
	(APROPOS-BOUNDP BOUNDP)
	(APROPOS-FBOUNDP FBOUNDP)
	(APROPOS-DONT-PRINT DONT-PRINT))
    (DECLARE (SPECIAL RETURN-LIST APROPOS-PREDICATE APROPOS-BOUNDP APROPOS-FBOUNDP
		      APROPOS-SUBSTRING APROPOS-DONT-PRINT))
    (MAPC 'APROPOS-1 STARTING-LIST)
    RETURN-LIST))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN TYI-WITH-SCROLLING (&OPTIONAL MOUSE-OR-KBD-TYI-P ANY-TYI-P)
  "Read a character, allowing scroll bar commands to be given and executed.
We do not return when scrolling is done, but keep waiting for
some other sort of input.
ANY-TYI-P non-NIL says return anything but scroll commands;
Otherwise MOUSE-OR-KBD-TYI-P non-NIL says return mouse characters;
otherwise ignore them."
  (DO ((CH)) (NIL)
    (COND ((NUMBERP (SETQ CH (FUNCALL STANDARD-INPUT ':ANY-TYI)))
	   (RETURN CH CH))
	  ((ATOM CH))
	  ((EQ (CAR CH) 'SCROLL)
	   (APPLY #'PROCESS-SPECIAL-COMMAND CH))
	  (ANY-TYI-P (RETURN CH))
	  ((AND MOUSE-OR-KBD-TYI-P (EQ (CAR CH) ':MOUSE-BUTTON))
	   (RETURN (THIRD CH) CH)))))

))

; From file DOC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFCOM COM-SELF-DOCUMENT "Print out documentation for the command on a given key." (KM)
  (LET (CHAR)
    (FORMAT QUERY-IO "~&Document command: ")
    (TYPEIN-LINE-ACTIVATE
      (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
	(SETQ CHAR (FUNCALL QUERY-IO ':ANY-TYI))))
    (DO-FOREVER
      (COND ((OR (ATOM CHAR)
		 (AND (EQ (CAR CHAR) ':MOUSE-BUTTON) (SETQ CHAR (THIRD CHAR))))
	     (FORMAT QUERY-IO "~:@C" CHAR)
	     (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)
	     (DOCUMENT-KEY CHAR *COMTAB*)
	     (RETURN))
	    ((EQ CHAR 'SCROLL)
	     (FORMAT QUERY-IO "Mouse-Scroll")
	     (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)
	     (FORMAT T
		     "Mouse Scrolling:
  When the mouse cursor is an up-and-down arrow, near the left edge,
it is in the /"scroll bar/".  Clicking the mouse in the scroll bar
scrolls the text in the window.

When the mouse is near the top or bottom edge and the cursor is a thick arrow,
that too is a place you can scroll, by pushing the mouse against the edge.

In the scroll bar, click left to scroll the line the mouse is on to the
top of the window.  Click right scrolls the same amount in the opposite
direction; the line at the top of the window moves down to the mouse.
Click middle uses the position of the mouse along the edge to choose
a portion of the buffer to view, so that if the mouse is near the bottom
you see something near the end of the file.

A portion of the left edge is thickened to show you what part of the
buffer is currently on the screen.")
	     (RETURN)))))
  DIS-NONE)

))

; From file DISPLA.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "

(DEFUN VIEW-WINDOW-DISPLAY (WINDOW STREAM &OPTIONAL FORCE-P FONTS-P
			    &AUX AT-END-P N-PLINES SHEET
			    LAST-BP PLINE X Y Y-POS)
  (LET ((*WINDOW* WINDOW))
    (SEND *WINDOW* ':SET-BASE-TICK *TICK*)
    (SETQ N-PLINES (WINDOW-N-PLINES WINDOW)
	  LAST-BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
    (AND STREAM (SETQ PLINE (PLINE-OF-POINT NIL WINDOW LAST-BP))
	 (LET ((ISTREAM (INTERVAL-STREAM-INTO-BP LAST-BP FONTS-P)))
	   (DO ((I PLINE (1+ I))
;		(AT-LINE (BP-LINE LAST-BP))
		(LINE) (EOF))
	       (( I N-PLINES))
	     (MULTIPLE-VALUE (LINE EOF)
	       (FUNCALL STREAM ':LINE-IN LINE-LEADER-SIZE))
	     (AND LINE (SEND ISTREAM ':LINE-OUT LINE))
	     (AND EOF (RETURN (SETQ AT-END-P T
				    STREAM NIL))))))
    (MUST-REDISPLAY WINDOW DIS-TEXT)
    (REDISPLAY WINDOW ':POINT NIL NIL FORCE-P)
    (OR STREAM AT-END-P
	(SETQ AT-END-P (FIND-BP-IN-WINDOW WINDOW LAST-BP)))
    (SETQ SHEET WINDOW
	  Y (* N-PLINES (TV:SHEET-LINE-HEIGHT SHEET)))
;  (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET)	;Erase anything left over
;		       (MULTIPLE-VALUE-BIND (NIL HEIGHT)
;			   (FUNCALL SHEET ':LABEL-SIZE)
;			 (- (+ (TV:SHEET-INSIDE-BOTTOM SHEET) HEIGHT) Y))
;		       (TV:SHEET-INSIDE-LEFT SHEET) Y (TV:SHEET-ERASE-ALUF SHEET) SHEET)
    (BIND (LOCF (TV:SHEET-BOTTOM-MARGIN-SIZE SHEET)) 0)
    (AND AT-END-P
	 (MULTIPLE-VALUE (X Y-POS)
	   (FIND-BP-IN-WINDOW-COORDS LAST-BP WINDOW)))
    (COND ((OR (NOT AT-END-P) (NULL Y-POS))
	   (TV:SHEET-LINE-OUT SHEET "--More--" 0 NIL 0 Y)
	   (MULTIPLE-VALUE (X Y-POS)
	     (TV:SHEET-READ-CURSORPOS SHEET)))
	  (AT-END-P (TV:SHEET-LINE-OUT SHEET "--Bottom--" 0 NIL 0 Y)))
    (LET ((BLINKER (WINDOW-POINT-BLINKER WINDOW)))
      (TV:BLINKER-SET-CURSORPOS BLINKER X Y-POS)
      (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK))
    (VALUES AT-END-P STREAM)))

))

; From file DEFS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFSTRUCT (HISTORY :NAMED-ARRAY (:CONC-NAME HISTORY-) (:CONSTRUCTOR MAKE-HISTORY-INTERNAL)
		    (:ALTERANT NIL)
		    (:PRINT-FUNCTION
		      #'(LAMBDA (HISTORY STREAM IGNORE)
			  (SI:PRINTING-RANDOM-OBJECT (HISTORY STREAM :TYPEP :NO-POINTER)
			    (FORMAT STREAM "~a. Length ~S Yank pointer ~S"
				    (HISTORY-NAME HISTORY) (HISTORY-LENGTH HISTORY)
				    (HISTORY-YANK-POINTER HISTORY))))))
  NAME
  (YANK-POINTER NIL)
  (LENGTH 0)
  LIST
  ELEMENT-STRING-FUNCTION
  YANK-METHOD)

))

; From file RDDEFS.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; RDDEFS  "

(DEFSTRUCT (READTABLE :ARRAY-LEADER :NAMED
		      (:CONSTRUCTOR MAKE-RDTBL)
		      (:MAKE-ARRAY (:DIMENSIONS (RDTBL-ARRAY-DIMS) :TYPE 'ART-16B))
		      (:DEFAULT-POINTER RDTBL)
		      (:SIZE-MACRO RDTBL-SIZE)
		      (:PRINT-FUNCTION
			#'(LAMBDA (RDTBL STREAM IGNORE)
			    (SI:PRINTING-RANDOM-OBJECT (RDTBL STREAM :TYPEP)))))

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

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN FLAVOR-HACK-DOCUMENTATION (FLAVOR-NAME)
  (LET* ((DOC (GET (LOCF (FLAVOR-PLIST (GET FLAVOR-NAME 'FLAVOR))) ':DOCUMENTATION))
	 (STRINGS NIL) FOO)
    (IF DOC
	(PROGN
	  (DOLIST (TEM DOC)
	    (AND (STRINGP TEM)
		 (SETQ STRINGS (NCONC STRINGS (NCONS TEM)))))
	  (DOLIST (TEM DOC)
	    (UNLESS (STRINGP TEM)
	      (SETQ STRINGS (NCONC STRINGS (LIST* (IF STRINGS #\RETURN "")
						  (IF FOO "" (SETQ FOO "A "))
						  TEM #\SPACE NIL)))))
	  (IF FOO (NCONC STRINGS (LIST "Flavor.")))
	  (SETF (DOCUMENTATION FLAVOR-NAME 'DEFFLAVOR) (APPLY 'STRING-APPEND STRINGS)))
      (IF (DOCUMENTATION FLAVOR-NAME 'DEFFLAVOR)
	  (SETF (DOCUMENTATION FLAVOR-NAME 'DEFFLAVOR) NIL)))))

))

si:(dolist (x *all-flavor-names*)
     (flavor-hack-documentation x))
