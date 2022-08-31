;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 22-Dec-83 02:30:20 by Mly,
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
CREATE says whether and how to create a new window if Control is
pressed along with CHAR.  It may be:
  T meaning create a window of flavor PROGRAM, or
  the name of a flavor to create, or
  a list to evaluate for effect, to create and select a window."
  (PUSH (LIST (CHAR-UPCASE CHAR) WINDOW-OR-FLAVOR DOCUMENTATION CREATE)
	*SYSTEM-KEYS*)
  (SETQ *SYSTEM-KEYS* (SORTCAR *SYSTEM-KEYS* #'ALPHALESSP)))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-SYS-1 (CH &AUX E W SW MAKENEW FLAVOR-OR-WINDOW)
  (SETQ MAKENEW (LDB-TEST %%KBD-CONTROL CH)
	CH (LDB %%KBD-CHAR CH))
  (COND ((OR (= CH #/?) (= CH #\HELP))
	 (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
	   (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
	   (FUNCALL WINDOW ':SET-LABEL "Keyboard system commands")
	   (WINDOW-CALL (WINDOW :DEACTIVATE)
	     (FORMAT WINDOW
		     "Type ~:@C followed by one of these characters to select the ~
                      corresponding program:~2%" #\SYSTEM)
	     (LET ((LIST (SORTCAR (COPYLIST *SYSTEM-KEYS*) #'ALPHALESSP)) (TEM #\?))
	       (DOLIST (X LIST)
		 (OR (CHAR-EQUAL TEM (SETQ TEM (CAR X)))
		     (FORMAT WINDOW "~&~C~8T~A" TEM (CADDR X)))))		 
	     (FORMAT WINDOW
	       "~2&Type ~:@C control-<character> to create a new window of a particular type.~@
                Type ~:@C after ~:@C to do nothing (if you typed ~:@C by accident).~%~@
		Type a space to flush: " #\SYSTEM #\RUBOUT #\SYSTEM #\SYSTEM)
	     (SETQ KBD-ESC-TIME NIL)		;Let kbd process proceed before we TYI.
	     (FUNCALL WINDOW ':TYI))))
	((SETQ E (ASSQ CH *SYSTEM-KEYS*))
	 ;; Find the most recently selected window of the desired type.
	 ;; If it is the same type as the selected window, make that the
	 ;; least recently selected so as to achieve the cycling-through effect.
	 ;; Otherwise the currently selected window becomes the most recently
	 ;; selected as usual, and esc S will return to it.
	 ;; In any case, we must fake out :MOUSE-SELECT's typeahead action since
	 ;; that has already been properly taken care of and we don't want to snarf
	 ;; any characters already typed after the [SYSTEM] command.
	 (SETQ FLAVOR-OR-WINDOW
	       (COND ((LISTP (SECOND E)) (EVAL (SECOND E)))
		     (T (SECOND E))))
	 (DELAYING-SCREEN-MANAGEMENT	;Inhibit auto selection
	   (COND ((TYPEP FLAVOR-OR-WINDOW 'ESSENTIAL-WINDOW)
		  ;; If the *SYSTEM-KEYS* list has a specific window indicated, use that.
		  (AND (SETQ SW SELECTED-WINDOW) (FUNCALL SW ':DESELECT NIL))
		  (FUNCALL FLAVOR-OR-WINDOW ':MOUSE-SELECT))
		 ((NULL FLAVOR-OR-WINDOW) NIL)  ;NIL means he already did whatever he wanted.
		 ((AND (NOT MAKENEW)
		       (SETQ W (FIND-WINDOW-OF-FLAVOR FLAVOR-OR-WINDOW)))
		  ;; Cycle through other windows of this flavor.
		  (COND ((SETQ SW SELECTED-WINDOW)
			 (FUNCALL SW ':DESELECT NIL)
			 (AND (TYPEP SW FLAVOR-OR-WINDOW)
			      (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SW T))))
		  (FUNCALL W ':MOUSE-SELECT))
		 ((AND (NOT MAKENEW)
		       (SETQ SW SELECTED-WINDOW)
		       (TYPEP (FUNCALL SW ':ALIAS-FOR-SELECTED-WINDOWS) FLAVOR-OR-WINDOW))
		  ;; There is only one window of this flavor, and this is it.
		  (BEEP))
		 ((NULL (FOURTH E)) (BEEP))	;Cannot create
		 ((NLISTP (FOURTH E))
		  ;; Create a new window of this flavor.
		  ;; We create on the default screen.
		  (AND (SETQ SW SELECTED-WINDOW) (FUNCALL SW ':DESELECT NIL))
		  (FUNCALL (MAKE-WINDOW (IF (EQ (FOURTH E) T) FLAVOR-OR-WINDOW (FOURTH E))
					':SUPERIOR DEFAULT-SCREEN)
			   ':MOUSE-SELECT))
		 (T (EVAL (FOURTH E))))))
	(( CH #\RUBOUT) (BEEP)))
  (SETQ KBD-ESC-TIME NIL))

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

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun char-name (char)
  "Returns the standard name of CHAR, as a string; or NIL if there is none.
For example, /"RETURN/" for the character Return.
Only works for characters which are not GRAPHIC-CHAR-P (unlike /"a/", for example."
  (let ((elt (rassq (dont-optimize (%pointer char)) xr-special-character-names)))
    (if elt (symbol-name (car elt)))))

))

; From file QCDEFS.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(DEFUN WARN-ON-ERRORS-CONDITION-HANDLER (CONDITION
					 &AUX
					 (CONDITION-NAMES (SEND CONDITION ':CONDITION-NAMES))
					 (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (IF SI:OBJECT-WARNINGS-OBJECT-NAME
      (PROGN (SI:MAYBE-PRINT-OBJECT-WARNINGS-HEADER)
	     (FORMAT T "~%Warning: ")
	     (LEXPR-FUNCALL 'FORMAT T ERROR-WARNING-ARGS))
    (PRINT-ERROR-WARNING-HEADER))
  (UNLESS EH:ERRSET-STATUS
    (COND ((AND (MEMQ 'SYS:READ-ERROR CONDITION-NAMES)
		(SEND CONDITION ':PROCEED-TYPE-P ':NO-ACTION))
	   (LEXPR-FUNCALL 'WARN 'READ-ERROR ':ERROR (SEND CONDITION ':FORMAT-STRING)
						    (SEND CONDITION ':FORMAT-ARGS))
	   (LET (BP REG)
	     (IF WARN-ON-ERRORS-STREAM
		 (SETQ BP (SEND WARN-ON-ERRORS-STREAM ':SEND-IF-HANDLES ':READ-BP)))
	     (AND BP
		  (SETQ REG (ZWEI:MAKE-REGISTER-NAME #/.))
		  (NOT (GET REG 'ZWEI:POINT))
		  (PROGN
		    (FORMAT T "~&Position of this error saved in ZWEI register /"./".")
		    (ZWEI:SAVE-POSITION-IN-REGISTER REG BP))))
	   ':NO-ACTION)
	  (T
	   (SI:RECORD-WARNING NIL ':ERROR NIL "~A"
			      (LEXPR-FUNCALL 'FORMAT NIL ERROR-WARNING-ARGS))
	   ;; Make a string now, in case the condition object points at data
	   ;; that is in a temporary area.
	   (LEXPR-FUNCALL 'WARN ERROR-WARNING-TYPE ':ERROR (SEND CONDITION ':FORMAT-STRING)
							   (SEND CONDITION ':FORMAT-ARGS))
	   (COND ((AND WARN-ON-ERRORS
		       (NOT (MEMQ 'SYS:PDL-OVERFLOW CONDITION-NAMES))
		       (NOT (SEND CONDITION ':DANGEROUS-CONDITION-P))
		       (NOT (SEND CONDITION ':DEBUGGING-CONDITION-P)))
		  (FORMAT T "~&TO DEBUG THIS, recompile with COMPILER:WARN-ON-ERRORS set to NIL.")
		  'WARN-ON-ERRORS))))))

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

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZWEI :MOUSE-CLICK) (BUTTON X Y &AUX HANDLED-P)
  (COND ((NOT (SEND (SEND SELF ':TOP-OF-EDITOR-HIERARCHY)
		    ':SELF-OR-SUBSTITUTE-SELECTED-P))
	 ;; This frame or whatever is not selected.
	 (TV:MOUSE-SELECT SELF))
	((AND (NOT (EDITOR-WINDOW-SELECTED-P SELF))
	      (OR (= BUTTON #\MOUSE-1-1)
		  *MOUSE-CLICK-ALWAYS-SELECTS*))
	 ;; Frame selected but this editor window is not.  Just switch to it.
	 (COMMAND-BUFFER-PUSH `(SELECT-WINDOW ,SELF))
	 (IF *MOUSE-CLICK-ALWAYS-SELECTS*
	     ;; And maybe also do the command for the mouse button.
	     (COMMAND-BUFFER-PUSH `(:MOUSE-CLICK ,SELF ,(TV:MERGE-SHIFT-KEYS BUTTON) ,X ,Y)))
	 (SETQ HANDLED-P T))
	(T
	 (COMMAND-BUFFER-PUSH `(:MOUSE-CLICK ,SELF ,(TV:MERGE-SHIFT-KEYS BUTTON) ,X ,Y))))
  T)

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFMETHOD (ZWEI :MOUSE-CLICK) (BUTTON X Y &AUX HANDLED-P)
  (COND ((NOT (SEND (SEND SELF ':TOP-OF-EDITOR-HIERARCHY)
		    ':SELF-OR-SUBSTITUTE-SELECTED-P))
	 ;; This frame or whatever is not selected.
	 (TV:MOUSE-SELECT SELF))
	((AND (NOT (EDITOR-WINDOW-SELECTED-P SELF))
	      (OR (= BUTTON #\MOUSE-1-1)
		  *MOUSE-CLICK-ALWAYS-SELECTS*))
	 ;; Frame selected but this editor window is not.  Just switch to it.
	 (COMMAND-BUFFER-PUSH `(SELECT-WINDOW ,SELF))
	 (IF *MOUSE-CLICK-ALWAYS-SELECTS*
	     ;; And maybe also do the command for the mouse button.
	     (COMMAND-BUFFER-PUSH `(:MOUSE-CLICK ,SELF ,(TV:MERGE-SHIFT-KEYS BUTTON) ,X ,Y)))
	 (SETQ HANDLED-P T))
	(T
	 (COMMAND-BUFFER-PUSH `(:MOUSE-CLICK ,SELF ,(TV:MERGE-SHIFT-KEYS BUTTON) ,X ,Y))))
  T)

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFSELECT (PROCESS-SPECIAL-COMMAND UNKNOWN-SPECIAL-COMMAND)
  (REDISPLAY ()
    ;The window is presumably on our list of windows and will get redisplayed
    ;in the normal course of events when buffered input had been processed.
    NIL)
  (SELECT-WINDOW (WINDOW)
    (PROG1 (NEQ WINDOW *WINDOW*)
	   (MAKE-WINDOW-CURRENT WINDOW)))
  (CONFIGURATION-CHANGED ()
    (LET ((FEW (FRAME-EXPOSED-WINDOWS)))
      (UNLESS (MEMQ *WINDOW* FEW)
	(MAKE-WINDOW-CURRENT (CAR FEW))))
    NIL)
  (SCROLL (WINDOW NLINES TYPE)
    (IF (EQ TYPE ':RELATIVE)
	(RECENTER-WINDOW-RELATIVE WINDOW NLINES)
	(RECENTER-WINDOW WINDOW ':START
			 (FORWARD-LINE (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))
				       NLINES T)))
    (UNLESS (EQ WINDOW *WINDOW*)
      ;; Scrolling nonselected window => flush typeout on it
      ;; because the main loop won't do it except for the selected window.
      (PREPARE-WINDOW-FOR-REDISPLAY WINDOW))
    T)
  ;; This is sent if he clicks on the mode line window, etc.
  (:MOUSE-BUTTON (CH WINDOW X Y)
    WINDOW X Y
    (WHEN (= CH #\MOUSE-R)
      (TV:MOUSE-CALL-SYSTEM-MENU)))
  (:MOUSE-CLICK (WINDOW CH *MOUSE-X* *MOUSE-Y*)
    (DECF *MOUSE-X* (TV:SHEET-INSIDE-LEFT (WINDOW-SHEET WINDOW)))
    (DECF *MOUSE-Y* (TV:SHEET-INSIDE-TOP (WINDOW-SHEET WINDOW)))
    (AND (MEMQ ':RECORD (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
	 (FUNCALL STANDARD-INPUT ':RECORD CH))
    (IF *MOUSE-HOOK*
	(FUNCALL *MOUSE-HOOK* WINDOW CH *MOUSE-X* *MOUSE-Y*)
      (IF (NEQ WINDOW *WINDOW*)		;Given in another window,
	  (LET ((*COMTAB* (IF (EQ *WINDOW* *MINI-BUFFER-WINDOW*) *STANDARD-COMTAB* *COMTAB*))
		(*LAST-COMMAND-TYPE* NIL)	;dont confuse mouse mark thing, and
		*CURRENT-COMMAND-TYPE*
		(*WINDOW* WINDOW)
		(*INTERVAL* (WINDOW-INTERVAL WINDOW)))	;temporarily act there (mini-buffer)
	    (PROCESS-COMMAND-CHAR CH))
	(PROCESS-COMMAND-CHAR CH)))
    T)
  ((:TYPEOUT-EXECUTE :EXECUTE) (FUNCTION &REST ARGS)
   (LET ((*MINI-BUFFER-DONT-RECORD* T))
     ;; We would not be able to repeat the command anyway.
     (NOT (APPLY FUNCTION ARGS)))))

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
	  ((AND MOUSE-OR-KBD-TYI-P (EQ (CAR CH) ':MOUSE-CLICK))
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
		 (EQ (CAR CHAR) ':MOUSE-CLICK))
	     (FORMAT QUERY-IO "~:@C" CHAR)
	     (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)
	     (DOCUMENT-KEY CHAR *COMTAB*)
	     (RETURN))
	    ((EQ (CAR CHAR) 'SCROLL)
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
		 (AND (EQ (CAR CHAR) ':MOUSE-CLICK) (SETQ CHAR (THIRD CHAR))))
	     (FORMAT QUERY-IO "~:@C" CHAR)
	     (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)
	     (DOCUMENT-KEY CHAR *COMTAB*)
	     (RETURN))
	    ((EQ (CAR CHAR) 'SCROLL)
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

; From file DOC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN DOCUMENT-KEY (CHAR COMTAB)
  "Print full documentation of character CHAR's definition in COMTAB, on STANDARD-OUTPUT."
  (PROG (TEM PREFIX)
     L  (SETQ TEM (COMMAND-LOOKUP CHAR COMTAB T))
	(COND ((NULL TEM)
	       (FORMAT T " is undefined.~%"))
	      ((SYMBOLP TEM)
	       (IF (NOT (GET TEM 'COMMAND-NAME))
		   (FORMAT T " is ~A, which is not implemented.~%" TEM)
		   (FORMAT T " is ~A, implemented by " (COMMAND-NAME TEM))
		   (FUNCALL STANDARD-OUTPUT ':ITEM 'FUNCTION-NAME TEM)
		   (FORMAT T ":~%")
		   (DO L *COMMAND-HOOK* (CDR L) (NULL L)
		       (LET ((DOCFN (GET (CAR L) 'HOOK-DOCUMENTATION-FUNCTION)))
			 (AND DOCFN
			      (FUNCALL DOCFN TEM CHAR))))
		   (PRINT-DOC ':FULL TEM CHAR)))
	      ((CONSP TEM)
	       (FORMAT T " is an alias for ~@[~:@C ~]~:@C.~%~@[~:@C ~]~:@C"
		       PREFIX (SETQ CHAR (DPB (FIRST TEM) %%KBD-CONTROL-META (SECOND TEM)))
		       PREFIX CHAR)
	       (GO L))
	      ((MACRO-COMMAND-P TEM)
	       (FORMAT T " is a user defined macro named ~A.
With no argument, run the macro with the repeat count in its definition.
With an argument, ignore the repeat count in its definition and use
the argument instead.~%"
		       (SYMEVAL-IN-CLOSURE TEM 'SYMBOL)))
	      ((PREFIX-COMMAND-P TEM)
	       (FORMAT T " is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
Type a subcommand to document (or * for all):~%")
	       (SETQ PREFIX CHAR
		     CHAR (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
			      (FUNCALL STANDARD-INPUT ':TYI)))
	       (FORMAT T "~%~:@C" PREFIX)
	       (COND ((= CHAR #/*)
		      (FORMAT T " has these subcommands:~%")
		      (DOCUMENT-PREFIX-CHAR-TABLE (GET-PREFIX-COMMAND-COMTAB TEM)))
		     (T
		      (FORMAT T " ~:@C" CHAR)
		      (SETQ COMTAB (GET-PREFIX-COMMAND-COMTAB TEM))
		      (GO L))))
	      ((MENU-COMMAND-P TEM)
	       (FORMAT T " is a menu command with the following subcommands:~%")
	       (DO ((L (GET-MENU-COMMAND-COMMANDS TEM) (CDR L))
		    (FLAG T NIL))
		   ((NULL L) (TERPRI))
		 (FORMAT T "~:[, ~]~A" FLAG (CAAR L))))
	      (T (FORMAT T " is garbage!?~%")))))

))

; From file KBDMAC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; KBDMAC  "

(DEFUN MACRO-STREAM-IO-TYI (OP)
  (LET ((CHAR
	  (COND (MACRO-UNTYI (PROG1 MACRO-UNTYI (SETQ MACRO-UNTYI NIL)))
		(MACRO-READING
		 (MACRO-UPDATE-LEVEL)
		 (FUNCALL MACRO-STREAM OP))
		(T (MACRO-TYI OP)))))
    (WHEN (AND (CONSP CHAR) (EQ (CAR CHAR) ':MOUSE-CLICK))
      (SETQ CHAR (THIRD CHAR)))
    (WHEN CHAR
      (WHEN RECORD-ALL-INPUT
	(UNLESS (EQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
	  (SETQ RECORD-INPUT-INTERVAL *INTERVAL*))
	(LET ((INPUT-LIST (GET RECORD-INPUT-INTERVAL 'INPUT-LIST)))
	  (WHEN (OR (NULL INPUT-LIST)
		    ( (LENGTH (CAR INPUT-LIST)) 400.))
	    (PUTPROP RECORD-INPUT-INTERVAL
		     (SETQ INPUT-LIST (CONS (MAKE-ARRAY 400. ':TYPE ART-Q
							':FILL-POINTER 0)
					    INPUT-LIST))
		     'INPUT-LIST))
	  (ARRAY-PUSH (CAR INPUT-LIST) CHAR)))
      (SETF (ARRAY-LEADER MACRO-INPUT-RECORD 1)
	    (\ (1+ (ARRAY-LEADER MACRO-INPUT-RECORD 1))
	       (ARRAY-LENGTH MACRO-INPUT-RECORD)))
      (SETF (AREF MACRO-INPUT-RECORD (ARRAY-LEADER MACRO-INPUT-RECORD 1))
	    CHAR))
    CHAR))

))
