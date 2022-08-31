;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 28-Dec-83 13:23:06 by Mly,
;;; Reason: New FORMAT directive ~\lozenged-string\
;;; Completion success value. Logical pathname completion.
;;; Printing chaos packets. ZWEI:COPY-COMTAB.
;;; Zwei mouse clicks can have bucky bits.
;;; EQUALP on arrays.
;;; while running on Lisp Machine Eighteen from band 7
;;; with Bad Inconsistently updated System 98.16, CADR 3.4, Inconsistently updated ZMail 53.6, MIT-Specific 22.0, microcode 305, ZM MIT.


; From file FORMAT.LISP SRC:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS)
    (AND (CONSP ARG) (EQ (CAR ARG) ':MOUSE-BUTTON) (SETQ ARG (SECOND ARG)))
    (SETQ ARG (CHARACTER ARG))
    (COND ((TV:CHAR-MOUSE-P ARG)
	   (COND ((AND (NOT COLON-FLAG) ATSIGN-FLAG)
		  (OR (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		      (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
		  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "#\")
		  (PRIN1 CHNAME))
		 (T (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
		    (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
		    (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
		    (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
		    (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Mouse-")
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT (NTH (LDB 0003 ARG)
							       '("Left" "Middle" "Right")))
		    (IF (SETQ CHNAME (NTH (SETQ BITS (LDB 0303 ARG))
					  '("" "-Twice" "-Thrice")))
			(FUNCALL STANDARD-OUTPUT ':STRING-OUT CHNAME)
		      (FUNCALL STANDARD-OUTPUT ':TYO #/-)
		      (ENGLISH-PRINT (1+ BITS))
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT "-times")))))
          ((NOT COLON-FLAG)
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   ;; If @ flag or if control bits, we want to use characters' names.
	   (IF (OR ATSIGN-FLAG (NOT (ZEROP BITS)))
	       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (LDB %%KBD-CHAR ARG))))
	   ;; Print an appropriate reader macro if @C.
	   (IF ATSIGN-FLAG (PRINC (IF (OR (NOT (ZEROP BITS)) CHNAME) "#\" "#//")))
	   (IF (NOT (ZEROP BITS))
	       ;; For efficiency, don't send :string-out message just for null string.
	       (FUNCALL STANDARD-OUTPUT
			':STRING-OUT
			(NTH BITS
			     '("" "c-" "m-" "c-m-"
			       "s-" "c-s-" "m-s-" "c-m-s-"
			       "h-" "c-h-" "m-h-" "c-m-h-"
			       "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
	   (IF CHNAME
	       (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		 (LET ((STR (STRING-DOWNCASE CHNAME)))
		   (ASET (CHAR-UPCASE (AREF STR 0)) STR 0)
		   (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
		   (RETURN-ARRAY STR)))
	     (AND ATSIGN-FLAG
		  (NOT (ZEROP BITS))
		  (IF ( #/a (LDB %%KBD-CHAR ARG) #/z)
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT "sh-")
		    (IF (MEMQ (LDB %%KBD-CHAR ARG)
			      '(#/, #\SPACE #/( #/) #/' #// #/` #/@ #/; #/: #/" #/| #/#))
			(TYO (SI:PTTBL-SLASH READTABLE))
		      (SETQ ARG (CHAR-DOWNCASE ARG)))))
	     (FUNCALL STANDARD-OUTPUT ':TYO (LDB %%KBD-CHAR ARG))))
	  (T
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
	   (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
	   (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
	   (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
	   (SETQ ARG (LDB %%KBD-CHAR ARG))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		    (LET ((STR (STRING-DOWNCASE CHNAME)))
		      (ASET (CHAR-UPCASE (AREF STR 0)) STR 0)
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
		      (RETURN-ARRAY STR)))
		  (AND ATSIGN-FLAG (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND ATSIGN-FLAG (< ARG 40) ( ARG #/))
		  (FUNCALL STANDARD-OUTPUT ':TYO ARG)
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
		 ((AND ( #/a ARG #/z)
		       (NOT (ZEROP BITS)))
		  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Shift-")
		  (FUNCALL STANDARD-OUTPUT ':TYO (CHAR-UPCASE ARG)))
                 (T (FUNCALL STANDARD-OUTPUT ':TYO ARG))))))

))

; From file FORMAT.LISP SRC:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFFORMAT LOZENGED-STRING (:ONE-ARG) (STRING PARAMS)
  (SETQ STRING (STRING STRING))
  (IF (AND (SEND STANDARD-OUTPUT ':OPERATION-HANDLED-P ':DISPLAY-LOZENGED-STRING)
	   (DOTIMES (I (STRING-LENGTH STRING) T)
	     (UNLESS ( (AREF STRING I) 200) (RETURN NIL))))	     
      (SEND STANDARD-OUTPUT ':DISPLAY-LOZENGED-STRING STRING)
    (FORMAT-CTL-ASCII STRING PARAMS)))

))


; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN COPY-COMTAB (OLD-COMTAB &OPTIONAL NEW-NAME)
  "Returns a comtab which is a copy of OLD-COMTAB, with name NEW-NAME.
The keyboard array and extended command alist are both copied."
  (LET ((OKBD (COMTAB-KEYBOARD-ARRAY OLD-COMTAB))
	(OMSE (COMTAB-MOUSE-ARRAY OLD-COMTAB)))
    (MAKE-COMTAB
      COMTAB-NAME           NEW-NAME
      COMTAB-KEYBOARD-ARRAY (COND ((LISTP OKBD)
				   (COPYTREE OKBD))
				  ((ARRAYP OKBD)
				   (COPY-ARRAY-CONTENTS OKBD
				     (MAKE-ARRAY (ARRAY-DIMENSIONS OKBD)))))
      COMTAB-MOUSE-ARRAY    (COND ((LISTP OMSE)
				   (COPYTREE OMSE))
				  ((ARRAYP OMSE)
				   (COPY-ARRAY-CONTENTS OMSE
				     (MAKE-ARRAY (ARRAY-DIMENSIONS OMSE)))))
      COMTAB-EXTENDED-COMMANDS (COPYTREE (COMTAB-EXTENDED-COMMANDS OLD-COMTAB))
      COMTAB-INDIRECT-TO    (COMTAB-INDIRECT-TO OLD-COMTAB))))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN COMMAND-LOOKUP (CHAR COMTAB &OPTIONAL NO-INDIRECTION-P)
  "Return the command in COMTAB has for character CHAR.
NO-INDIRECTION-P means do not follow indirections stored
in elements of COMTAB; return the list instead of looking
at the character the list specifies.
The second value is the comtab the command was found in.
This will be COMTAB or a comtab that COMTAB indirects to."
  (DECLARE (RETURN-LIST COMMAND COMTAB))
  (DO ((CTB COMTAB) (CH CHAR)
       (KEYBOARD-ARRAY) (COMMAND))
      (NIL)
    (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB)
	  COMMAND (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
			 (CDR (ASSQ CH KEYBOARD-ARRAY)))
			((TV:CHAR-MOUSE-P CH)
			 (AREF (COMTAB-MOUSE-ARRAY CTB)
			       (MIN (LDB %%KBD-MOUSE-N-CLICKS CH) 1)
			       (LDB %%KBD-MOUSE-BUTTON CH)
			       (LDB %%KBD-CONTROL-META CH)))
			(T
			 (AREF KEYBOARD-ARRAY
			       (LDB %%KBD-CHAR CH)
			       (LDB %%KBD-CONTROL-META CH)))))
    (IF (OR (NOT (CONSP COMMAND)) NO-INDIRECTION-P)
	(AND (OR COMMAND (NULL (SETQ CTB (COMTAB-INDIRECT-TO CTB))))
	     (RETURN COMMAND CTB))
	(SETQ CTB COMTAB
	      CH (%LOGDPB (FIRST COMMAND) %%KBD-CONTROL-META (SECOND COMMAND))))))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN COMMAND-STORE (COMMAND CHAR COMTAB &AUX KEYBOARD-ARRAY)
  "Store COMMAND into COMTAB for character CHAR."
  (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB))
  (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
	 (LET ((ELEMENT (ASSQ CHAR KEYBOARD-ARRAY)))
	   (IF ELEMENT (RPLACD ELEMENT COMMAND)
	       (PUSH (CONS CHAR COMMAND) (COMTAB-KEYBOARD-ARRAY COMTAB)))))
	((TV:CHAR-MOUSE-P CHAR)
	 (ASET COMMAND
	       (COMTAB-MOUSE-ARRAY COMTAB)
	       (MIN (LDB %%KBD-MOUSE-N-CLICKS CHAR) 1)
	       (LDB %%KBD-MOUSE-BUTTON CHAR)
	       (LDB %%KBD-CONTROL-META CHAR)))
	(T
	 (ASET COMMAND
	       KEYBOARD-ARRAY
	       (LDB %%KBD-CHAR CHAR)
	       (LDB %%KBD-CONTROL-META CHAR)))))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN CREATE-COMTAB (&OPTIONAL NAME)
  "Return a new, empty comtab with name NAME."
  (MAKE-COMTAB COMTAB-KEYBOARD-ARRAY (MAKE-ARRAY '(240 16.))
	       COMTAB-NAME NAME
	       COMTAB-MOUSE-ARRAY (MAKE-ARRAY '(2 3 16.))))

))


(LABELS ((ZWEI:COMTAB-HACK (ZWEI:COMTAB &AUX FOO)
			   (WHEN (AND (ZWEI:COMTAB-MOUSE-ARRAY ZWEI:COMTAB)
				      (= (ARRAY-RANK (ZWEI:COMTAB-MOUSE-ARRAY ZWEI:COMTAB))
					 2))
			     (SETQ FOO (MAKE-ARRAY '(2 3 16.)))
			     (DOTIMES (I 2)
			       (DOTIMES (J 3)
				 (SETF (AREF FOO I J 0)
				       (AREF (ZWEI:COMTAB-MOUSE-ARRAY ZWEI:COMTAB) I J))))
			     (SETF (ZWEI:COMTAB-MOUSE-ARRAY ZWEI:COMTAB) FOO))
			   (IF (ZWEI:COMTAB-INDIRECT-TO ZWEI:COMTAB)
			       (ZWEI:COMTAB-HACK (ZWEI:COMTAB-INDIRECT-TO ZWEI:COMTAB)))))
  (DOLIST (X (LIST ZWEI:*SEARCH-MINI-BUFFER-COMTAB*
		   ZWEI:*STRING-SEARCH-CONTROL-H-COMTAB*
		   ZWEI:*MSG-CONTROL-X-COMTAB*
		   ZWEI:*ZMACS-CONTROL-X-COMTAB*
		   ZWEI:*ZMAIL-COMTAB*
		   ZWEI:*MINI-BUFFER-MULTI-LINE-COMTAB*
		   ZWEI:*RECURSIVE-EDIT-COMTAB*
		   ZWEI:*STANDARD-CONTROL-X-COMTAB*
		   ZWEI:*CONTROL-R-COMTAB*
		   ZWEI:*MAIL-CONTROL-X-COMTAB*
		   ZWEI:*STANDARD-COMTAB*
		   ZWEI:*REPLY-COMTAB*
		   ZWEI:*CONVERSE-COMTAB*
		   ZWEI:*STRING-SEARCH-SINGLE-LINE-COMTAB*
		   ZWEI:*ZMACS-COMTAB*
		   ZWEI:*SEARCH-CONTROL-H-COMTAB*
		   ZWEI:*MINI-BUFFER-COMTAB*
		   ZWEI:*COMPLETING-READER-COMTAB*
		   ZWEI:*PATHNAME-READING-COMTAB*
		   ZWEI:*STANDALONE-COMTAB*
		   ZWEI:*MSG-COMTAB*
		   ZWEI:*STREAM-COMTAB*
		   ZWEI:*REPLY-CONTROL-X-COMTAB*
		   ZWEI:*STRING-SEARCH-MINI-BUFFER-COMTAB*))
    (ZWEI:COMTAB-HACK X)))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN RENAME-FILE (STRING-OR-STREAM NEW-NAME &KEY (ERROR T) QUERY)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (VALUES OLD-NAME OLD-TRUENAME NEW-TRUENAME))
  (RENAMEF STRING-OR-STREAM NEW-NAME ERROR QUERY))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN RENAMEF (STRING-OR-STREAM NEW-NAME &OPTIONAL (ERROR-P T) QUERY?)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY?, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR-P is NIL."
  (DECLARE (VALUES OLD-NAME OLD-TRUENAME NEW-TRUENAME))
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
				       (TYPEP STRING-OR-STREAM 'PATHNAME))
				   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
			      (STRING-OR-STREAM FILE-ERROR)
    (LET ((RESULT (WILDCARDED-FILE-OPERATION
		    STRING-OR-STREAM
		    #'PRIMITIVE-RENAME-FILE
		    NIL
		    (PARSE-PATHNAME NEW-NAME) ERROR-P
		    (MAKE-FILE-QUERY-FUNCTION QUERY?))))
      (IF (EQ (CAAR RESULT) (CADAR RESULT))
	  (VALUES (THIRD (CAR RESULT))
		  (FOURTH (CAR RESULT))
		  (FIFTH (CAR RESULT)))
	(VALUES (MAPCAR 'THIRD RESULT)
		(MAPCAR 'FOURTH RESULT)
		(MAPCAR 'FIFTH RESULT))))))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN COMPLETE-CHAOS (HOST PATHNAME STRING OPTIONS
		       &AUX HOST-UNIT PKT FILE-STRING SUCCESS
			    DELETED-P WRITE-P NEW-OK STRING-ORIGIN
			    (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (DOLIST (KEY OPTIONS)
    (SELECTQ KEY
      (:DELETED
       (SETQ DELETED-P T))
      ((:READ :IN)
       (SETQ WRITE-P NIL))
      ((:PRINT :OUT :WRITE)
       (SETQ WRITE-P T))
      (:OLD
       (SETQ NEW-OK NIL))
      (:NEW-OK
       (SETQ NEW-OK T))
      (OTHERWISE
       (FERROR NIL "~S is not a recognized option." KEY ':COMPLETE-STRING))))
  (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT))
  (MULTIPLE-VALUE (PKT SUCCESS FILE-STRING)
    (FUNCALL HOST-UNIT ':COMMAND NIL NIL NIL
	     (FORMAT NIL "COMPLETE~:[ DELETED~]~:[ WRITE~]~:[ NEW-OK~]~%~A~%~A~%"
		     (NOT DELETED-P) (NOT WRITE-P) (NOT NEW-OK)
		     (FILE-PRINT-PATHNAME PATHNAME) STRING)))
  (COND (SUCCESS
	 (OR (SETQ STRING-ORIGIN (STRING-SEARCH-CHAR #\CR FILE-STRING))
	     (FERROR NIL "Illegally formatted string ~S from file server." FILE-STRING))
	 (SETQ SUCCESS (PKG-BIND SI:PKG-KEYWORD-PACKAGE
			 (READ-FROM-STRING FILE-STRING NIL
					   (FILE-CHECK-COMMAND "COMPLETE" FILE-STRING))))
	 (SETQ STRING (SUBSTRING FILE-STRING
				 (SETQ STRING-ORIGIN (1+ STRING-ORIGIN))
				 (STRING-SEARCH-CHAR #\CR FILE-STRING STRING-ORIGIN)))))
  (CHAOS:RETURN-PKT PKT)
  (IF (EQ SUCCESS ':NIL) (SETQ SUCCESS NIL))
  (VALUES STRING SUCCESS))

))

; From file PATHST.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (LOGICAL-PATHNAME :COMPLETE-STRING) (STRING OPTIONS &AUX STRING1 FOO SUCCESS)
  (LET ((TRANSLATED (FUNCALL (PARSE-PATHNAME STRING HOST) ':TRANSLATED-PATHNAME)))
    (SETQ STRING1 (SEND TRANSLATED ':STRING-FOR-HOST))
;    ;; This used to be just :STRING-FOR-PRINTING,
;    ;; but we want to get rid of the 's that NIL components would make.
;    (SETQ STRING1
;	  (IF (NOT (MEMQ (SEND TRANSLATED ':VERSION) '(NIL :UNSPECIFIC)))
;	      (SEND TRANSLATED ':STRING-FOR-PRINTING)
;	    (IF (SEND TRANSLATED ':TYPE)
;		(SEND (SEND TRANSLATED ':NEW-VERSION ':NEWEST) ':STRING-FOR-PRINTING)
;	      (STRING-APPEND (SEND (SEND TRANSLATED ':NEW-PATHNAME
;					 ':NAME NIL ':TYPE NIL)
;				   ':STRING-FOR-PRINTING)
;			     (SEND TRANSLATED ':NAME)))))
;    (SETQ STRING1 (SUBSTRING STRING1 (1+ (STRING-SEARCH-CHAR #/: STRING1))))
    )
  ;; What STRING1 is will match the :STRING-FOR-HOST for many kinds of pathname,
  ;; but not for all.
  (LET (BASE-PATHNAME)
    (CONDITION-CASE ()
	(SETQ BASE-PATHNAME (FUNCALL-SELF ':TRANSLATED-PATHNAME))
      (UNKNOWN-LOGICAL-DIRECTORY
       (SETQ BASE-PATHNAME
	     (FUNCALL (FUNCALL (FUNCALL-SELF ':NEW-DIRECTORY NIL) ':TRANSLATED-PATHNAME)
		      ':NEW-DIRECTORY (FUNCALL-SELF ':DIRECTORY)))))
    (VALUES
      (FUNCALL
	(FUNCALL-SELF ':BACK-TRANSLATED-PATHNAME
		      (PARSE-PATHNAME
			(MULTIPLE-VALUE (FOO SUCCESS)
			  (FUNCALL BASE-PATHNAME
				   ':COMPLETE-STRING
				   STRING1
				   OPTIONS))))
	':STRING-FOR-PRINTING)
      SUCCESS)))

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
      (AND (CONSP CHAR)
	   (EQ (CAR CHAR) ':MOUSE-BUTTON)
	   (SETQ CHAR (SECOND CHAR)))
      (COND ((ATOM CHAR)
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
  (IF (OR (ATOM CHAR)
	  (AND (CONSP CHAR)
	       (EQ (CAR CHAR) ':MOUSE-BUTTON)
	       (SETQ CHAR (SECOND CHAR))))
      (FORMAT T "~&~:@C" CHAR)
    (FORMAT T "~&~S" CHAR))
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
		       PREFIX
		       (SETQ CHAR (%LOGDPB (FIRST TEM) %%KBD-CONTROL-META (SECOND TEM)))
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

; From file CHSNCP.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSNCP  "

(DEFSELECT ((PKT NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (PKT STREAM IGNORE IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (PKT STREAM)
      (FORMAT STREAM "CHAOS packet. PKT-STRING ~S PKT-STATUS ~S"
	      (PKT-STRING PKT) (PKT-STATUS PKT))))
  (:DESCRIBE (PKT)
    (DESCRIBE-DEFSTRUCT PKT 'PKT)
    (DESCRIBE-DEFSTRUCT PKT 'PKT-LEADER)
    (PRINT-PKT PKT)))

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN EQUALP-ARRAY (ARRAY1 ARRAY2)
  (AND (LET ((RANK (ARRAY-RANK ARRAY1)))
	 (DO ((I 1 (1+ I)))
	     ((= I RANK) T)
	   (UNLESS (= (%P-CONTENTS-OFFSET ARRAY1 I) (%P-CONTENTS-OFFSET ARRAY2 I))
	     (RETURN NIL))))
       (LET ((LEN (LENGTH ARRAY1)))
	 (AND (= LEN (LENGTH ARRAY2))
	      (DOTIMES (I LEN T)
		(UNLESS (EQUALP (AR-1-FORCE ARRAY1 I) (AR-1-FORCE ARRAY2 I))
		  (RETURN NIL)))))))

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN ARRAY-ELEMENT-SIZE (ARRAY)
  "Return the number of bits per element of ARRAY."
  (OR (AREF #'ARRAY-BITS-PER-ELEMENT (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0))
      25.))	;Q-type, assume going to use unsigned fixnums.

))
