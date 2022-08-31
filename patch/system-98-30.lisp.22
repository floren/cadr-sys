;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 14-Jan-84 12:58:28 by Mly,
;;; Reason: Copying files in dired.
;;; m-x Find Unbalanced Parentheses does not munge undo info
;;; COMPILE-FILE arguments
;;; Parsing VMS pathnames with a null name. eg ".foo"
;;;   "foo.bar;" gets version :newest
;;; m-x Print Region hacks fonts
;;; tv:graphics-mixin :DRAW-FILLED-IN-CIRCLE fencepost error
;;; Thicken and unthicken font bugs
;;; Setf of cli:aref
;;; while running on Lisp Machine Eighteen from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.29, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN COPY-FILE (PATHNAME-OR-STREAM NEW-NAME
		  &REST OPTIONS
		  &KEY (ERROR T) &ALLOW-OTHER-KEYS)
  "Copy a file, specified as a pathname, string or I//O stream.
CHARACTERS can be T, NIL, meaning the same as in OPEN.
 or it can be :ASK, meaning always ask the user,
 or :MAYBE-ASK meaning ask the user unless the answer is clear,
 or :DEFAULT meaning guess as well as possible but never ask.
Specify BYTE-SIZE to force copying in a certain byte size.
 BYTE-SIZE affects only binary mode copying.
REPORT-STREAM is a stream to output messages to as files are copied.
 If it is NIL, no messages are output.
COPY-CREATION-DATE if NIL means don't copy the file creation date;
 make now be the new file's creation date.
COPY-AUTHOR if NIL means don't copy the author; make you the new file's author.
CREATE-DIRECTORIES says whether to create a directory to be copied into.
 Values are T, NIL and :QUERY (meaning ask the user if the situation comes up).
Values returned:
1) the first value is normally the defaulted pathname to copy to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
4) the fourth value is a mode of copying, or a list of such.
 A mode of copying is a type specifier such as STRING-CHAR or (UNSIGNED-BYTE 8).
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (ARGLIST PATHNAME-OR-STREAM NEW-NAME
		    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
		    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
		    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT))
	   (VALUES TARGET-PATHNAME TARGET-TRUENAME RESULT-PATHNAME COPY-MODE))
  (FORCE-USER-TO-LOGIN)
  (LET ((RESULT
	  (IF (OR (STRINGP PATHNAME-OR-STREAM)
		  (TYPEP PATHNAME-OR-STREAM 'PATHNAME))	;Not a stream
	      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
					  (PATHNAME-OR-STREAM FILE-ERROR)
		(LET ((MERGED-PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME-OR-STREAM)))
		  (APPLY MERGED-PATHNAME
			 ':WILDCARD-MAP #'PRIMITIVE-COPY-FILE
			 ':MAYBE NIL
			 MERGED-PATHNAME (PARSE-PATHNAME NEW-NAME) OPTIONS)))
	    (LET ((TRUENAME (SEND PATHNAME-OR-STREAM ':TRUENAME)))
	      (LIST (APPLY 'PRIMITIVE-COPY-FILE
			   (FILE-PROPERTIES TRUENAME)
			   TRUENAME (PARSE-PATHNAME NEW-NAME) OPTIONS))))))
    (IF (EQ (CAAR RESULT) (CADAR RESULT))
	(VALUES (THIRD (CAR RESULT))
		(FOURTH (CAR RESULT))
		(FIFTH (CAR RESULT))
		(SIXTH (CAR RESULT)))
      (VALUES (MAPCAR 'THIRD RESULT)
	      (MAPCAR 'FOURTH RESULT)
	      (MAPCAR 'FIFTH RESULT)
	      (MAPCAR 'SIXTH RESULT)))))

))

; From file DIRED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-COPY "Copy the file on this line" ()
  (WHEN (GET (LOCF (LINE-PLIST (BP-LINE (POINT)))) ':DELETED)
    (BARF))
  (LET ((FILE (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT)))))
    (LET ((NEWFILE (READ-DEFAULTED-PATHNAME (FORMAT NIL "Pathname to copy ~A to" FILE)
					    FILE))
	  RESULT FILE-PLIST)
      (SETQ RESULT (MULTIPLE-VALUE-LIST (COPY-FILE FILE NEWFILE ':ERROR NIL)))
      (COND ((ERRORP (THIRD RESULT))
	     (FORMAT QUERY-IO "~&Not copied: ~A" (THIRD RESULT)))
	    (T
	     (FORMAT QUERY-IO "~&File copied to ~A" (THIRD RESULT))
	     ;; Save a copy of this file's directory list entry.
	     (SETQ FILE-PLIST (COPYLIST (LINE-PLIST (BP-LINE (POINT)))))
	     (PUTPROP (LOCF FILE-PLIST)
		      (THIRD RESULT)
		      ':PATHNAME)
	     ;; insert a line for the new file.
	     (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	       (MULTIPLE-VALUE-BIND (BP LEVEL)
		   (DIRED-PATHNAME-INSERTION-BP (THIRD RESULT))
		 (COND (BP
			(WITH-BP (SAVE-BP BP ':NORMAL)
			  (INSERT BP #\RETURN)
			  (SETF (LINE-PLIST (BP-LINE SAVE-BP)) FILE-PLIST)
			  (SETF (DIRED-LINE-LEVEL (BP-LINE SAVE-BP))
				(OR LEVEL 0))
			  (DIRED-REGENERATE-LINE (BP-LINE SAVE-BP))))
		       (T
			(FORMAT QUERY-IO ", in a directory not in this display.")))))))))
  DIS-TEXT)

))

; From file DIRED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN EDIT-BUFFERS-REVERT (BUFFER &OPTIONAL IGNORE IGNORE SELECT-P)
  (WITH-READ-ONLY-SUPPRESSED (BUFFER)
    (LET ((*INTERVAL* BUFFER)
	  (OLD-BUFFER)
	  (*BATCH-UNDO-SAVE* T))
      (DOLIST (BUF (HISTORY-LIST (SEND *WINDOW* ':BUFFER-HISTORY)))
	(OR (EQ BUF *INTERVAL*) (RETURN (SETQ OLD-BUFFER BUF))))
      (DELETE-INTERVAL *INTERVAL*)
      (DISCARD-UNDO-INFORMATION *INTERVAL*)
      (LET ((STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-FIRST-BP *INTERVAL*))))
	(FORMAT STREAM "Buffers in ZMACS:~2%")
	(DOLIST (B *ZMACS-BUFFER-LIST*)
	  (COND ((NEQ B *INTERVAL*)
		 (FUNCALL STREAM ':STRING-OUT
			  (IF (BUFFER-NEEDS-SAVING-P B)
			      (IF (EQ B OLD-BUFFER) " S . " " S   ")
			    (IF (EQ B OLD-BUFFER) "   . " "     ")))
		 (FUNCALL STREAM ':STRING-OUT
			  (IF (BUFFER-MODIFIED-P B) " * " "   "))
		 (FUNCALL STREAM ':STRING-OUT (BUFFER-NAME B))
		 (FUNCALL STREAM ':LINE-PUT 'BUFFER B)
		 (FUNCALL STREAM ':TYO #\CR)))))
      (MOVE-BP (POINT) (BEG-LINE (INTERVAL-FIRST-BP *INTERVAL*) 2 T))))
  (IF SELECT-P (MAKE-BUFFER-CURRENT BUFFER)))

))

;;;patched again in 98.32 so punt this one. mly
;; From file COMF.LISP SRC:<L.ZWEI> OZ:
;#8R ZWEI#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "
;
;(DEFCOM COM-FIND-UNBALANCED-PARENTHESES "Find parenthesis error in buffer" ()
;  (LET ((BEG-BP (INTERVAL-FIRST-BP *INTERVAL*))
;	(END-BP (INTERVAL-LAST-BP *INTERVAL*))
;	(POINT (POINT))
;	(OLD-TICK (NODE-TICK *INTERVAL*))
;	(*BATCH-UNDO-SAVE* T)			;so paren insertion & deletion isn't recorded
;	BEG-BP-1 END-BP-1 BP)
;    (UNWIND-PROTECT
;      (PROGN
;	(SETQ BEG-BP-1 (COPY-BP BEG-BP ':MOVES)
;	      END-BP-1 (COPY-BP END-BP ':NORMAL))
;	(INSERT BEG-BP-1 "(
;")
;	(INSERT END-BP-1 "
;)")
;	(IF (SETQ BP (FORWARD-SEXP BEG-BP))
;	    (IF (BP-= BP END-BP)		;All ok
;		(FORMAT QUERY-IO "~&All parens appear balanced.")
;		(MOVE-BP POINT BP)
;		(FORMAT QUERY-IO "~&Probably extra right-paren here."))
;	    (OR (SETQ BP (FORWARD-SEXP END-BP -1))
;		(BARF "Cannot find unbalanced parenthesis"))
;	    (MOVE-BP POINT BP)
;	    (FORMAT QUERY-IO "~&Probably no right-paren for this left-paren.")))
;      (COND (BEG-BP-1
;	     (DELETE-INTERVAL BEG-BP BEG-BP-1 T)
;	     (FLUSH-BP BEG-BP-1)))
;      (COND (END-BP-1
;	     (DELETE-INTERVAL END-BP-1 END-BP T)
;	     (FLUSH-BP END-BP-1)))
;      (SETF (NODE-TICK *INTERVAL*) OLD-TICK)))
;  DIS-BPS)
;
;))


(SETF (DOCUMENTATION 'UNADVISE 'FUNCTION)
      "Remove some or all advice from FUNCTION-SPEC, or from all functions.
With no arguments, all advice is removed.  This is a consequence of these rules:
If FUNCTION-SPEC is non-NIL, advice is removed from that function only.
 Otherwise, advice is removed from all functions if the other args match.
If CLASS is non-NIL, only advice of that class is removed.
If POSITION is non-NIL (a number or name), only advice with that position is removed.")

; From file ADVISE.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defmacro advise (&optional function-spec class name position &body forms)
  "Put advice on FUNCTION-SPEC to perform FORMS.
CLASS is :BEFORE, :AFTER or :AROUND.
NAME is the name for this piece of advice;
any existing piece with the same name and class will be replaced.
POSITION says where to put this advice wrt others of same class;
it is a number, or the name of some other piece of advice to go after,
or NIL meaning put this one first.
If given no arguments, ADVISE returns a list of functions which are presently advised."
  (if (null function-spec) 'advised-functions
    `(advise-1 ',function-spec ',class ',name ',position ',forms)))

))

(SETF (DOCUMENTATION 'FQUERY 'FUNCTION)
      "Ask a multiple-choice question.
FQUERY-FORMAT-STRING and FQUERY-FORMAT-ARGS are used to print the question.
Ending the string with /"? /" is often appropriate.
OPTIONS is a PLIST.  Defined indicators are:
:MAKE-COMPLETE boolean.  Send a :MAKE-COMPLETE message to the stream if it understands it.
:TYPE one of :TYI, :READLINE, :MINI-BUFFER-OR-READLINE.
  It says how the answer is gathered and echoed.
:CHOICES a list of choices.
  A choice is either the symbol :ANY or a list.
  If a list, its car is either a possible return value,
    or a list of a possible return value and how to echo it.
  The remaining things in the list are input items that select that return value.
  For a :READLINE type call, they should be strings.
  For a :TYI type call, they should be characters.
  Example choice (for :READLINE): ((:foo /"Foo/") #\F #\space)
:FRESH-LINE boolean.  Send a :FRESH-LINE to the stream initially.
:CONDITION symbol.  Signalled before asking.
:LIST-CHOICES boolean.  If T, a list of choices is printed after the question.
:BEEP boolean.  If T, we beep before printing the message.
:CLEAR-INPUT boolean.  If T, we discard type-ahead before printing the message.
:SELECT boolean.  Select the window and select back.
:HELP-FUNCTION specifies a function to be called
  if the user types Help.
  It is called with STREAM, CHOICES and TYPE-FUNCTION as arguments.
:STREAM stream or expression.  Specifies the stream to use.
  If it is a symbol (which is not an io-stream) or a list it is evaluated.
  Default is to use QUERY-IO.")

; From file DOCMIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'GETL 'FUNCTION)
 "Find any of the properties in INDICATOR-LIST, on SYMBOL.
Whichever of those properties occurs first in the property list is used.
The value is a pointer to the cell in the property list that points
to the indicator.  The CADR of the value is the property's value.")

))

; From file DOCMIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'ARRAY-ROW-MAJOR-INDEX 'FUNCTION)
  "Return the combined index in ARRAY of the element identified by SUBSCRIPTS.
This value could be used as the second argument of AR-1-FORCE to access that element.")

))

; From file GENRIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(DEFUN MISMATCH (SEQUENCE1 SEQUENCE2 &REST KEYARGS
		 &KEY &OPTIONAL FROM-END TEST TEST-NOT KEY
		 (START1 0) END1 (START2 0) END2)
  "Return index in SEQUENCE1 of first mismatch between it and SEQUENCE2.
Elements are compared one by one, starting with elements at indexes START1 and START2
 and stopping when index 1 reaches END1 or index 2 reaches END2.
If sequences match, value is NIL.  If they match until one is exhausted but not both,
 the value is the index in SEQUENCE1 at which one sequence is exhausted.
TEST is a function of two args to use to compare two elements.
 The elements match when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
FROM-END non-NIL means comparison aligns right ends of the specified
 subsequences and returns one plus the index of the rightmost mismatch."
  (DECLARE (ARGLIST (SEQUENCE1 SEQUENCE2 &KEY FROM-END TEST TEST-NOT KEY
			       (START1 0) END1 (START2 0) END2)))
  (IF FROM-END
      (LEXPR-FUNCALL (IF (AND (ARRAYP SEQUENCE1) (ARRAYP SEQUENCE2))
			 'MISMATCH-ARRAYS-FROM-END
		       'MISMATCH-LISTS-FROM-END)
		     SEQUENCE1 SEQUENCE2 KEYARGS)
    (DO ((INDEX1 (SEQ-START SEQUENCE1 START1))
	 (INDEX2 (SEQ-START SEQUENCE2 START2))
	 (I START1 (1+ I))
	 (STOP1 (SEQ-END SEQUENCE1 END1))
	 (STOP2 (SEQ-END SEQUENCE2 END2)))
	((OR (EQ INDEX1 STOP1) (EQ INDEX2 STOP2))
	 (UNLESS (AND (EQ INDEX1 STOP1) (EQ INDEX2 STOP2))
	   I))
      (UNLESS (EQ (NOT (NULL TEST-NOT))
		  (NOT (FUNCALL (OR TEST-NOT TEST 'EQL)
				(IF KEY
				    (FUNCALL KEY (SEQ-FETCH SEQUENCE1 INDEX1))
				  (SEQ-FETCH SEQUENCE1 INDEX1))
				(IF KEY
				    (FUNCALL KEY (SEQ-FETCH SEQUENCE2 INDEX2))
				  (SEQ-FETCH SEQUENCE2 INDEX2)))))
	(RETURN I))
      (SEQ-INC INDEX1) (SEQ-INC INDEX2))))

))

; From file DOCMIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'LENGTH 'FUNCTION)
  "If LIST-OR-ARRAY is a list, returns the number of elements in LIST-OR-ARRAY
If LIST-OR-ARRAY is an array, returns the active length of the array,
which is the value of the fill-pointer, if any, or else the number of elements
in the array.")

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-FILE (&OPTIONAL INPUT-FILENAME
		     &KEY OUTPUT-FILENAME
		     (SET-DEFAULT-PATHNAME)
		     ((:PACKAGE PACKAGE-SPEC)))
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the
*LOAD-PATHNAME-DEFAULTS*.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE is the package to compile in."
  (QC-FILE (OR INPUT-FILENAME "") OUTPUT-FILENAME
	   NIL NIL PACKAGE-SPEC NIL
	   (NOT SET-DEFAULT-PATHNAME)))

))

(SETF (DOCUMENTATION 'COMPILER:COMPILE-STREAM 'FUNCTION)
"This function does all the /"outer loop/" of the compiler, for file and editor compilation.
Expressions to be compiled are read from INPUT-STREAM.
The caller is responsible for handling any file attributes.
GENERIC-PATHNAME is the file to record information for and use the attributes of.
 It may be NIL if compiling to core.
FASD-FLAG is NIL if not making a QFASL file.
PROCESS-FN is called on each form.
QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
FILE-LOCAL-DECLARATIONS is normally initialized to NIL,
but you can optionally pass in an initializations for it.
COMPILING-WHOLE-FILE-P should be T if you are processing all of the file.")

(SETF (DOCUMENTATION 'SI:COPY-OBJECT-TREE 'FUNCTION)
  "Copy lists, arrays and everything but symbols, for DEPTH levels.
If TEMPORARY-AREAS-ONLY is non-NIL, objects in non-temporary areas are left alone.")


; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN DEFUN-COMPATIBILITY (EXP)
  "Process the cdr of a DEFUN-form, converting old Maclisp formats into modern Lispm ones.
This must be done before the name of the function can be determined with certainty.
The value is an entire form, starting with DEFUN or MACRO.
If no change has been made, the cdr of the value will be EQ to the argument."
  (PROG (FCTN-NAME LL BODY TYPE)
	(SETQ TYPE 'EXPR)
	(SETQ FCTN-NAME (CAR EXP))
	(COND ((NOT (ATOM FCTN-NAME))		;Convert list function specs
	       (COND ((AND (= (LENGTH FCTN-NAME) 2)	;(DEFUN (FOO MACRO) ...)
			   (EQ (SECOND FCTN-NAME) 'MACRO))
		      (SETQ TYPE 'MACRO FCTN-NAME (CAR FCTN-NAME)))
		     ((EQ FCTN-NAME (SETQ FCTN-NAME (STANDARDIZE-FUNCTION-SPEC FCTN-NAME)))
		      (RETURN (CONS 'DEFUN EXP)))))	;Return if no conversion required
	      ((OR (NOT (ATOM (CADR EXP))) (NULL (CADR EXP))) ;Detect a valid DEFUN.
	       (RETURN (CONS 'DEFUN EXP)))
	      ((MEMQ (CADR EXP) '(FEXPR EXPR MACRO))
	       (SETQ TYPE (CADR EXP) EXP (CDR EXP)))
	      ((MEMQ FCTN-NAME '(FEXPR EXPR MACRO))
	       (SETQ TYPE FCTN-NAME FCTN-NAME (CADR EXP) EXP (CDR EXP))))
	;; Here if a new DEFUN has to be constructed
	(SETQ LL (CADR EXP))
	(SETQ BODY (CDDR EXP))
;WEIRD CONVERSION HACK TO UNCONVERT INTERLISP NLAMBDAS THAT WERE PREVIOUSLY CONVERTED
; BY HOLLOWAY'S RANDOM HACKER TO KLUDGY FEXPR'S
	(COND ((AND (EQ TYPE 'FEXPR)
		    (EQUAL LL '(*ARGS*)))
		(SETQ TYPE 'EXPR)
		(SETQ LL (CONS '&QUOTE (CADAAR BODY)))	;LAMBDA LIST OF INTERNAL LAMBDA
		(SETQ BODY (CDDAAR BODY)) ))	;BODY OF INTERNAL LAMBDA
; **END OF THAT HACK**
	(COND ((EQ TYPE 'FEXPR)
	       (SETQ LL (CONS '&QUOTE (CONS '&REST LL))))
	      ((EQ TYPE 'MACRO)
	       (RETURN (CONS 'MACRO (CONS FCTN-NAME (CONS LL BODY)))))
	      ((AND LL (ATOM LL))
	        (SETQ TYPE 'LEXPR
		     LL `(&EVAL &REST *LEXPR-ARGLIST* &AUX (,LL (LENGTH *LEXPR-ARGLIST*))))))
	(RETURN (CONS 'DEFUN (CONS FCTN-NAME (CONS LL BODY))))))

))


; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-HOST) ()
  (LET ((DIR-DELIM (CAR (FUNCALL-SELF ':DIRECTORY-DELIMITERS)))
	(DEV (STRING-OR-WILD DEVICE NIL
			     *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
	(DIR (TENEX-FAMILY-DIRECTORY-NAME))
	(NAM (STRING-OR-WILD NAME NIL
			     *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
	(TYP (STRING-OR-WILD TYPE NIL
			     *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
	(VER (IF VERSION (TENEX-FAMILY-VERSION-STRING) ""))
	(DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~A:~C~A~C~@[~A~]~:[.~]~:[~A~;~*~]~@[~A~]"
	    DEV (CAR DIR-DELIM) DIR (CDR DIR-DELIM)
	    NAM (NOT (OR TYPE VERSION))
	    (MEMQ TYPE '(NIL :UNSPECIFIC)) TYP VER)))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-NAMESTRING)
	   (HOST-SPECIFIED-P NAMESTRING &OPTIONAL (START 0) END)
  (DECLARE (VALUES DEVICE DIRECTORY NAME TYPE VERSION
		   DEVICE-SPECIFIED-P DIRECTORY-SPECIFIED-P NAME-SPECIFIED-P TYPE-SPECIFIED-P
		   VERSION-SPECIFIED-P))
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (LET* ((DIR-DELIM-ALIST (FUNCALL-SELF ':DIRECTORY-DELIMITERS))
	 (ALL-DELIMS (NCONC (MAPCAR #'CAR DIR-DELIM-ALIST) '(#/: #/. #/; #\SP))))
    (DO ((IDX (OR (STRING-SEARCH-NOT-CHAR #\SP NAMESTRING START END) END))
	 (TEM) (TEM1) (DELIM)
	 (DIR-DELIM)
	 (DEV)
	 (DIR) (NAM) (TYP) (VER)
	 DEV-SPECIFIED-P NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P)
	(( IDX END)
	 (IF (EQUAL TYP "") (SETQ TYP ':UNSPECIFIC))
	 (IF (EQUAL NAM "") (SETQ NAM NIL))
	 (SETQ DEV (OR DEV (IF HOST-SPECIFIED-P (SEND SELF ':PRIMARY-DEVICE))))
	 (VALUES DEV DIR NAM TYP VER
		 DEV-SPECIFIED-P DIR NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P))
      (COND ((SETQ DIR-DELIM (CDR (ASSQ (AREF NAMESTRING IDX) DIR-DELIM-ALIST)))
	     (AND DIR
		  (PATHNAME-ERROR IDX "Directory occurs twice in ~A" NAMESTRING))
	     (SETQ IDX (1+ IDX))
	     (DO () (NIL)
	       (MULTIPLE-VALUE (TEM IDX DELIM)
		 (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING
						  (LIST #/. DIR-DELIM) IDX END NIL T))
	       (SETQ DIR (IF (AND (= DELIM DIR-DELIM) (NULL DIR))
			     (LIST TEM)
			   (NCONC DIR (NCONS TEM))))
	       (AND (= DELIM DIR-DELIM) (RETURN))))
	    (T
	     (MULTIPLE-VALUE (TEM IDX DELIM)
	       (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING ALL-DELIMS IDX END T T))
	     (COND ((ASSQ DELIM DIR-DELIM-ALIST)
		    (SETQ IDX (1- IDX)))
		   ((AND (= DELIM #/;) VER)	;Protect against twenex attribute usage
		    (SETQ IDX END)))
	     (IF (MEMBER TEM '("*" #.(MAKE-STRING 1 ':INITIAL-VALUE #\BREAK)))
		 (SETQ TEM ':WILD))
	     (COND ((= DELIM #/:)
		    (AND DEV
			 (PATHNAME-ERROR IDX
				 "Device occurs twice in ~A" NAMESTRING))
		    (SETQ DEV TEM DEV-SPECIFIED-P (1- IDX)))
		   ((= DELIM #/;)
		    (COND ((NULL NAM-SPECIFIED-P)
			   (SETQ NAM TEM TYP ""
				 NAM-SPECIFIED-P (1- IDX) TYP-SPECIFIED-P (1- IDX)))
			  ((NULL TYP-SPECIFIED-P)
			   (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX)))))
		   ((NULL NAM-SPECIFIED-P)
		    (SETQ NAM TEM NAM-SPECIFIED-P (1- IDX))
		    (IF (= DELIM #/.) (SETQ TYP ':UNSPECIFIC)))
		   ((NULL TYP-SPECIFIED-P)
		    (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX))
		    (IF (EQ DELIM #/.) (SETQ VER ':UNSPECIFIC)))
		   ((NULL VER-SPECIFIED-P)
		    (SETQ VER-SPECIFIED-P (1- IDX))
		    (COND ((NULL TEM)
			   (SETQ VER NIL))
			  ((EQUAL TEM "")
			   (SETQ VER ':UNSPECIFIC))
			  ((SETQ TEM1 (NUMERIC-P TEM))
			   (SETQ VER TEM1))
			  ((EQ TEM ':WILD)
			   (SETQ VER ':WILD))
			  ((FUNCALL-SELF ':OLDEST-CHECK TEM)
			   (SETQ VER ':OLDEST))
			  (T (PATHNAME-ERROR IDX
				     "Version must be numeric in ~A" NAMESTRING))))))))))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFWRAPPER (VMS-PATHNAME-MIXIN :PARSE-NAMESTRING) (ARGLIST . BODY)
  `(LET ((NAMESTRING (SECOND ARGLIST)))
     (IF (STRING-SEARCH-CHAR #/ (NTH 1 ARGLIST))
	 (FERROR 'PATHNAME-PARSE-ERROR "Illegal chararacter in ~A" (NTH 1 ARGLIST)))
     (MULTIPLE-VALUE-BIND (DEV DIR NAM TYP VER NIL NIL
			   NAM-SPECIFIED-P TYP-SPECIFIED-P)
	 (PROGN ,@BODY)
       (AND TYP-SPECIFIED-P
	    (MEMQ VER '(NIL :UNSPECIFIC))
	    (< TYP-SPECIFIED-P (LENGTH NAMESTRING))
	    (EQ (AREF NAMESTRING TYP-SPECIFIED-P) #/;)
	    (SETQ VER ':NEWEST))
       (AND NAM-SPECIFIED-P
	    (EQ NAM NIL)
	    ( (AREF NAMESTRING (1- NAM-SPECIFIED-P)) #\)
	    (SETQ NAM :UNSPECIFIC))		  
       (AND (EQUAL DIR '("000000"))
	    (SETQ DIR ':ROOT))
       (VALUES DEV DIR NAM TYP VER))))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFUN STRING-OR-WILD (FIELD &OPTIONAL NO-QUOTE-P SPECIALS REPLACED-BY)
  "Convert FIELD, a pathname component, to a string to appear in a printed representation.
NO-QUOTE-P inhibits insertion of quoting characters;
otherwise, quote characters are inserted and some characters translated:
SPECIALS is a list of characters to be translated,
and REPLACED-BY is an equally-long list of characters to translate them to."
  (COND ((EQ FIELD ':WILD) "*")
	((MEMQ FIELD '(NIL :UNSPECIFIC)) NIL)
	(NO-QUOTE-P (STRING FIELD))
	((QUOTE-COMPONENT-STRING FIELD SPECIALS REPLACED-BY))))

))

; From file MAKSYS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN DESCRIBE-SYSTEM (SYSTEM-NAME &KEY (SHOW-FILES T) (SHOW-TRANSFORMATIONS T) &AUX SYSTEM)
  "Print all about the system named SYSTEM-NAME.
SHOW-FILES is T to give the history of each file in the system, NIL not to,
 or :ASK meaning query the user whether to.
SHOW-TRANSFORMATIONS is similar, for whether to show the transformations
 which MAKE-SYSTEM would execute.
Note that calling DESCRIBE on a system-object prints somewhat lower level information."
  (IF (NULL (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME)))
      (FORMAT T "~&There is no system named ~A.~%" SYSTEM-NAME)
    (SETQ SYSTEM-NAME (SYSTEM-NAME SYSTEM))
    (LET* ((SYSTEM-SOURCE-FILE
	     (GET-SOURCE-FILE-NAME (SYSTEM-SYMBOLIC-NAME SYSTEM) 'DEFSYSTEM))
	   (*FORCE-PACKAGE*
	     (PKG-FIND-PACKAGE (OR (SEND SYSTEM-SOURCE-FILE ':GET ':PACKAGE) "USER"))))
      (WHEN SYSTEM-SOURCE-FILE
	(FORMAT T "~&System ~A~@[ is defined in file ~A~]~%"
		SYSTEM-NAME SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE SYSTEM-SOURCE-FILE)))
    (COND ((SYSTEM-PATCHABLE-P SYSTEM)
	   (FORMAT T "~&~%~A is patchable" SYSTEM-NAME)
	   (MULTIPLE-VALUE-BIND (MAJOR MINOR STATUS)
	       (GET-SYSTEM-VERSION SYSTEM)
	     (LET ((STATUS-NAME (OR (SECOND (ASSQ STATUS SYSTEM-STATUS-ALIST)) STATUS)))
	       (OR (EQUAL STATUS-NAME "")
		   (FORMAT T ", ~A" STATUS-NAME)))
	     (IF MAJOR (FORMAT T ", ~D.~D is loaded" MAJOR MINOR))
	     (FORMAT T ";~%  a typical patch file is ~A~%"
		     (PATCH-SYSTEM-PATHNAME SYSTEM-NAME ':PATCH-FILE (OR MAJOR 1) (OR MINOR 0)
					    ':LISP))
	     (AND MAJOR
		  (FQUERY NIL "Do you want to see the patches for ~A? " SYSTEM-NAME)
		  (PRINT-PATCHES SYSTEM)))))
    (IF (SYSTEM-PACKAGE-DEFAULT SYSTEM)
	(FORMAT T "~& Files in ~A are forcibly read in package ~A.~%"
		SYSTEM-NAME (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
    (WHEN SHOW-FILES
      (FORMAT T "~%Compilation and loading of files in this system:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':RELOAD ':DO-NOT-DO-COMPONENTS
		   ':DESCRIBE ':NO-INCREMENT-PATCH ':NO-RELOAD-SYSTEM-DECLARATION))
    (WHEN SHOW-TRANSFORMATIONS
      (FORMAT T "~%Transformations required to MAKE-SYSTEM now:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':DO-NOT-DO-COMPONENTS ':PRINT-ONLY
		   ':NO-RELOAD-SYSTEM-DECLARATION))
    (LET ((COMPONENTS (SYSTEM-COMPONENT-SYSTEMS SYSTEM)))
      (COND (COMPONENTS
	     (FORMAT T "~2&~A is made up of component system~P "
		     SYSTEM-NAME (LENGTH COMPONENTS))
	     (FORMAT:PRINT-LIST T "~A" COMPONENTS)
	     (WHEN (Y-OR-N-P "Describe the component system~P?" (LENGTH COMPONENTS))
	       (DOLIST (COMPONENT COMPONENTS)
		 (FORMAT T "~2&")
		 (DESCRIBE-SYSTEM COMPONENT ':SHOW-FILES SHOW-FILES
				  ':SHOW-TRANSFORMATIONS SHOW-TRANSFORMATIONS)))))))
  SYSTEM-NAME)

))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-PRINT-REGION "Prints the region on the default hardcopy device." ()
  (REGION (BP0 BP1)
    (LET* ((INTERVAL (MAKE-INTERVAL BP0 BP1))
	   (FONTS (SEND *INTERVAL* ':GET-ATTRIBUTE ':FONTS))
	   (STREAM (IF (ATOM FONTS)
		       (ZWEI:INTERVAL-STREAM INTERVAL)
		     (ZWEI:INTERVAL-STREAM INTERVAL NIL NIL T))))
      (SI:HARDCOPY-STREAM STREAM
			  ':FILE-NAME
			  (FORMAT:OUTPUT NIL
			    "ZWEI Buffer "
			    ;; Print the pathname, or else the buffer name.
			    (PRINC (OR (BUFFER-PATHNAME *INTERVAL*)
				       (BUFFER-NAME *INTERVAL*)))
			  ;; Print last visited version number, if one is recorded.
			  (AND (CONSP (BUFFER-FILE-ID *INTERVAL*))
			       (TYPEP (CAR (BUFFER-FILE-ID *INTERVAL*)) 'FS:PATHNAME)
			       (FORMAT T " (~D)"
				       (SEND (CAR (BUFFER-FILE-ID *INTERVAL*)) ':VERSION))))
			  ':TV-FONTS (IF (ATOM FONTS) (LIST FONTS) FONTS))))
  (FORMAT T " -- Done.")
  DIS-NONE)

))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-DISSOCIATED-PRESS "Dissociate the text in a buffer.
The numeric argument is the number of characters of overlap;
or, if negative, minus the number of words of overlap.
The output appears on the terminal; the buffer is not modified.
To put the output in a buffer, use Execute Command into Buffer
after selecting the buffer that the output should go in." ()
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (LET* ((FORWARD-FUNCTION (IF (MINUSP *NUMERIC-ARG*) 'FORWARD-WORD 'FORWARD-CHAR))
	 (OVERLAP-SIZE (IF *NUMERIC-ARG-P* (ABS *NUMERIC-ARG*) 2))
	 (BUFFER (READ-BUFFER-NAME
		   (FORMAT NIL "Buffer to dissociate: (Overlap = ~D ~:[character~P~;word~P~])"
			  OVERLAP-SIZE (MINUSP *NUMERIC-ARG*) OVERLAP-SIZE)
		   *INTERVAL* NIL))
	 (NLINES (DO ((COUNT 0 (1+ COUNT))
		      (LINE (BP-LINE (INTERVAL-FIRST-BP BUFFER)) (LINE-NEXT LINE))
		      (END-LINE (BP-LINE (INTERVAL-LAST-BP BUFFER))))
		     ((EQ LINE END-LINE) COUNT)))
	 (NLINES-OVER-TEN (FLOOR NLINES 10.))
	 (CURRENT-P (EQ BUFFER *INTERVAL*))
	 (POINT (IF CURRENT-P (POINT) (BUFFER-SAVED-POINT BUFFER)))
	 (*INTERVAL* BUFFER)
	 TENTHS)
    ;; Set up in TENTHS a list of ten lines, distributed at tenths of the buffer.
    (IF (ZEROP NLINES-OVER-TEN)
	(SETQ TENTHS (LIST (INTERVAL-FIRST-BP BUFFER)))
      (DO ((COUNT 0 (1+ COUNT))
	   (LINE (BP-LINE (INTERVAL-FIRST-BP BUFFER)) (LINE-NEXT LINE))
	   (END-LINE (BP-LINE (INTERVAL-LAST-BP BUFFER))))
	  ((EQ LINE END-LINE))
	(IF (ZEROP (\ COUNT NLINES-OVER-TEN))
	    (PUSH LINE TENTHS)))
      (SETQ TENTHS (NREVERSE TENTHS)))
    (DO () (())
      ;; Wrap around if at end; otherwise we might get stuck there.
      (IF (BP-= POINT (INTERVAL-LAST-BP BUFFER))
	  (MOVE-BP POINT (INTERVAL-FIRST-BP BUFFER)))
      ;; Print and advance over a random amount of stuff.
      (LET ((BP (FUNCALL FORWARD-FUNCTION POINT (FIX (SI:RANDOM-IN-RANGE 2 15.)) T)))
	(SEND STANDARD-OUTPUT ':STRING-OUT
	      (STRING-INTERVAL POINT BP T T))
	(MOVE-BP POINT BP))
      ;; Compute the overlap string -- the last few words or characters.
      (LET ((BP (FUNCALL FORWARD-FUNCTION POINT (- OVERLAP-SIZE) T)))
	(LET ((OVERLAP-STRING (STRING-INTERVAL BP POINT T T))
	      (RANDOM-LINE-NUMBER (FIX (SI:RANDOM-IN-RANGE 0 NLINES))))
	  ;; Move to a randomly chosen position in the buffer.
	  ;; Jump immediately to the correct tenth of the buffer,
	  ;; then scan by lines to the chosen line.
	  (MOVE-BP POINT
		   (IF (ZEROP NLINES-OVER-TEN)
		       (INTERVAL-FIRST-BP BUFFER)
		     (NTH (FLOOR RANDOM-LINE-NUMBER NLINES-OVER-TEN) TENTHS))
		   0)
	  (DO ((COUNT (IF (ZEROP NLINES-OVER-TEN)
			  0
			(* (FLOOR RANDOM-LINE-NUMBER NLINES-OVER-TEN) NLINES-OVER-TEN))
		      (1+ COUNT))
	       (LINE (BP-LINE POINT) (LINE-NEXT LINE))
	       (END-COUNT RANDOM-LINE-NUMBER))
	      ((= COUNT END-COUNT)
	       (MOVE-BP POINT LINE (FIX (SI:RANDOM-IN-RANGE 0 (LINE-LENGTH LINE))))))
	  ;; Then search for the overlap string.  At end of buffer, wrap around.
	  (MOVE-BP POINT
		   (OR (SEARCH POINT OVERLAP-STRING)
		       (SEARCH (INTERVAL-FIRST-BP BUFFER) OVERLAP-STRING NIL T)))))))
  DIS-NONE)

))


; From file STREAM.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-FILLED-IN-CIRCLE)
	   (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF))
  (LET ((CENTER-X (+ CENTER-X (SHEET-INSIDE-LEFT)))
	(CENTER-Y (+ CENTER-Y (SHEET-INSIDE-TOP))))
    (PREPARE-SHEET (SELF)
      (DO ((X 0)
	   (F 0)				; F is just x^2. Don't use multiplication!
	   (Y RADIUS))
	  ((> X Y))
	(UNLESS (= X Y)
	  (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1 (- CENTER-X Y) (+ CENTER-Y X)
					 ALU SELF)
	  (UNLESS (ZEROP X)
	    (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1 (- CENTER-X Y) (- CENTER-Y X)
					   ALU SELF)))
	(SETQ F (+ F X X 1) X (1+ X))
	(WHEN ( F Y)
	  (SETQ F (- F Y Y -1) Y (- Y 1))
	  (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X -1) 1
					 (- CENTER-X X -1) (+ CENTER-Y Y 1)
					 ALU SELF)
	  (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X -1) 1
					 (- CENTER-X X -1) (- CENTER-Y Y 1)
					 ALU SELF))))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defsetf cli:aref set-aref)
(defprop cli:aref aloc locf-method)

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN EH-ARG (&OPTIONAL (NAME-OR-NUMBER 0) (ERRORP T))
  "Return the value of specified arg in current frame.
Specify either a number (origin 0) or a symbol (package does not matter)."
  (IF NAME-OR-NUMBER
      (MULTIPLE-VALUE-BIND (VAL1 NIL BARF)
	  (SG-FRAME-ARG-VALUE EH-SG EH-FRAME NAME-OR-NUMBER ERRORP)
	(IF BARF (FORMAT T "~&~A" BARF VAL1) VAL1))
    (MULTIPLE-VALUE-BIND (VAL NIL)
	(SG-LISTIFY-ARGS-AND-LOCALS EH-SG EH-FRAME)
      VAL)))

))

; From file FNTCNV.LISP PS:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "

(DEFUN THICKEN-FONT-DESCRIPTOR (FD &OPTIONAL NEW-NAME &AUX LEN NFD)
  "Given font-descriptor FD, make a new one whose characters are /"thicker/".
NEW-NAME specifies the name to give the new font-descriptor;
by default, a /"B/" is appended to the old name.
The new font descriptor is returned, and the new name is not actually defined."
  (OR NEW-NAME (SETQ NEW-NAME (INTERN (STRING-APPEND (FD-NAME FD) #/B) "FONTS")))
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FD)
	NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-Q :LENGTH LEN)
				  FD-FILL-POINTER (FD-FILL-POINTER FD)
				  FD-NAME NEW-NAME
				  FD-LINE-SPACING (FD-LINE-SPACING FD)
				  FD-BASELINE (FD-BASELINE FD)
				  FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
				  FD-BLINKER-WIDTH (1+ (FD-BLINKER-WIDTH FD))
				  FD-SPACE-WIDTH (1+ (FD-SPACE-WIDTH FD))))
  (DO ((I 0 (1+ I))
       (CD) (NCD))
      (( I LEN))
    (AND (SETQ CD (AREF FD I))
	 (LET ((WIDTH (ARRAY-DIMENSION CD 1))
	       (HEIGHT (ARRAY-DIMENSION CD 0)))
	   (SETQ NCD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-4B
						       :LENGTH (LIST HEIGHT (1+ WIDTH)))
					   CD-CHAR-WIDTH (1+ (CD-CHAR-WIDTH CD))
					   CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
	   (DOTIMES (J HEIGHT)
	     (DOTIMES (I WIDTH)
	       (SETF (AREF NCD J I) (LOGIOR (AREF CD J I) (AREF NCD J I)))
	       (SETF (AREF NCD J (1+ I)) (AREF CD J I))))
	   (ASET NCD NFD I))))
  NFD)

))

; From file FNTCNV.LISP PS:<L.IO1> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; FNTCNV  "

(DEFUN UNTHICKEN-FONT-DESCRIPTOR (FD NEW-NAME &AUX LEN NFD)
  "Given font-descriptor FD, make a new one whose characters are less thick.
NEW-NAME specifies the name to give the new font-descriptor.
The new font descriptor is returned, and the new name is not actually defined."
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FD)
	NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-Q :LENGTH LEN)
				  FD-FILL-POINTER (FD-FILL-POINTER FD)
				  FD-NAME NEW-NAME
				  FD-LINE-SPACING (FD-LINE-SPACING FD)
				  FD-BASELINE (FD-BASELINE FD)
				  FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
				  FD-BLINKER-WIDTH (FD-BLINKER-WIDTH FD)
				  FD-SPACE-WIDTH (FD-SPACE-WIDTH FD)))
  (DO ((I 0 (1+ I))
       (CD) (NCD))
      (( I LEN))
    (AND (SETQ CD (AREF FD I))
	 (LET ((WIDTH (ARRAY-DIMENSION CD 1))
	       (HEIGHT (ARRAY-DIMENSION CD 0)))
	   (SETQ NCD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-4B
						       :LENGTH (LIST HEIGHT WIDTH))
					   CD-CHAR-WIDTH (CD-CHAR-WIDTH CD)
					   CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
	   ;110  100
	   (DOTIMES (J HEIGHT)
	     (DO* ((I (1- WIDTH) (1- I))
		   (RIGHT 0 THIS)
		   (THIS (AREF CD J I) LEFT)
		   LEFT)
		  ((= I 0))
	       (SETQ LEFT (AREF CD J (1- I)))
	       (IF (AND ( LEFT 0) ( THIS 0) (= RIGHT 0))
		   (ASET 0 NCD J I)
		 (ASET (AREF CD J I) NCD J I))))
	   (ASET NCD NFD I))))
  NFD)

))

