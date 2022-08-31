;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.39
;;; Reason: SI:PRINTING-RANDOM-OBJECT includes package prefix if necessary
;;; DESCRIBE recursively indents better
;;; RENAME-PACKAGE accepts a single nickname as well as a list
;;; Define CLI:ATAN
;;; Make FS:*REMEMBER-PASSWORDS* a synonym for FS:RECORD-PASSWORDS-FLAG
;;; Interpreted DO* bug
;;; Declarations in USING-RESOURCE
;;; Metering fixes -- by rms
;;; Patch directories always written/read using "traditional" syntax
;;; Typeout Window bugs -- rms
;;; Can now abort in cold-load stream
;;; Forward *TERMINAL-IO* to TERMINAL-IO
;;; Written 17-Feb-84 04:23:09 by Mly,
;;; while running on Lisp Machine One from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.37, CADR 3.6, ZMail 53.11, MIT-Specific 22.0, microcode 306.



; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defprop let cw-let cw-handler)

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defun cw-let (exp)
  (multiple-value-bind (bound bindlist)
      (cw-parallel-binding-list (cadr exp))
    (let (body)
      (setq all-variables
	    (nunion all-variables
		    (let (all-variables)
		      (setq body (cw-clause (cddr exp)))
		      (dolist (b bound)
			(setq all-variables (delq b all-variables)))
		      all-variables)))
      (if cw-return-expansion-flag
	  (list* (car exp) bindlist body)))))

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defprop si:advise-let cw-let cw-handler)

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defprop si:encapsulation-let cw-let cw-handler)

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defun (let* cw-handler) (exp)
  (multiple-value-bind (bound bindlist)
      (cw-serial-binding-list (cadr exp))
    (let (body)
      (setq all-variables
	    (nunion all-variables
		    (let (all-variables)
		      (setq body (cw-clause (cddr exp)))
		      (dolist (b bound)
			(setq all-variables (delq b all-variables)))
		      all-variables)))
      (if cw-return-expansion-flag
	  (list* 'let* bindlist body)))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-1 (THING)		;An internal subroutine
  (IF (OR (NULL THING)			;Don't recursively describe relatively boring things
	  (NUMBERP THING) (SYMBOLP THING) (STRINGP THING))
      NIL
    (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
    (LET ((*STANDARD-OUTPUT*	;Arrange for indentation by 5 spaces
	    (CLOSURE '(*STANDARD-OUTPUT*)
		     #'(LAMBDA (&REST ARGS)
			 (AND (EQ (SEND *STANDARD-OUTPUT* ':SEND-IF-HANDLES ':READ-CURSORPOS)
				  0)
			      (SEND *STANDARD-OUTPUT* ':STRING-OUT "     "))
			 (LEXPR-FUNCALL *STANDARD-OUTPUT* ARGS)))))
      (DESCRIBE THING T))
    (SEND *STANDARD-OUTPUT* ':FRESH-LINE)))

))

; From file RDDEFS.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; RDDEFS  "

(DEFMACRO PRINTING-RANDOM-OBJECT ((OBJECT STREAM . OPTIONS) &BODY BODY)
  "A macro for aiding in the printing of random objects.
This macro generates a form which:
   1.  Uses the print-table to find the things in which to enclose your randomness.
   2.  (by default) includes the virtual address in the printed representation.
   3.  Obeys PRINT-READABLY
 Options are	:NO-POINTER to suppress the pointer
		:TYPEP princs the typep of the object first.
 		:FASTP <fastp> if the variable happens to be sitting around.

 Example:
 (DEFSELECT ((:PROPERTY HACKER :NAMED-STRUCTURE-INVOKE))
   (:PRINT-SELF (HACKER STREAM IGNORE IGNORE)
     (SI:PRINTING-RANDOM-OBJECT (HACKER STREAM :TYPEP)
       (PRIN1 (HACKER-NAME HACKER) STREAM))))
 ==> #<HACKER /"MMcM/" 6172536765>"
  (LET ((%POINTER T)
	(TYPEP NIL)
	(FASTP NIL))
    (DO ((L OPTIONS (CDR L)))
	((NULL L))
      (SELECTQ (CAR L)
	(:NO-POINTER (SETQ %POINTER NIL))
	(:TYPEP (SETQ TYPEP T))
	(:FASTP (SETQ L (CDR L) FASTP (CAR L)))
	(OTHERWISE (FERROR NIL "~S is an unknown keyword in PRINTING-RANDOM-OBJECT"
			   (CAR L)))))
    `(PROGN
       (AND PRINT-READABLY (PRINT-NOT-READABLE ,OBJECT))
       (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) ,STREAM ,FASTP)
       ,@(AND TYPEP
	      `((PRINT-PNAME-STRING (TYPEP ,OBJECT) ,STREAM ,FASTP)))
       ,@(AND TYPEP BODY
	      `((FUNCALL ,STREAM ':TYO (PTTBL-SPACE READTABLE))))
       ,@BODY
       ,@(AND %POINTER
	      `((FUNCALL ,STREAM ':TYO (PTTBL-SPACE READTABLE))
		(LET ((*PRINT-BASE* 8.)
		      (*PRINT-RADIX* NIL)
		      (*NOPOINT T))
		  (PRINT-FIXNUM (%POINTER ,OBJECT) ,STREAM))))
       (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) ,STREAM ,FASTP)
       ,OBJECT)))

))

; From file DOCMIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION '%DRAW-RECTANGLE 'FUNCTION)
  "Draw a solid rectangle on SHEET using ALU-FUNCTION.
ALU-FUNCTION is typically TV:ALU-IOR, TV:ALU-ANDCA or TV:ALU-XOR.
HEIGHT and WIDTH are the size, and X-BITPOS and Y-BITPOS are the upper left corner.")

))

; From file COMA.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-SET-POP-MARK "Sets or pops the mark.
With no U's, sets the mark at the point, and pushes point onto the point pdl.
With one U, pops the point pdl.
With two U's, pops the point pdl and throws it away" (KM)
  (COND ((OR (NEQ *NUMERIC-ARG-P* ':CONTROL-U)
	     ( *NUMERIC-ARG* 3))
	 (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
	 (MOVE-BP (MARK) (POINT))
	 (SETF (WINDOW-MARK-P *WINDOW*) T)
	 DIS-BPS)
	(( *NUMERIC-ARG* 17.)
	 (MULTIPLE-VALUE-BIND (BP PLINE)
	     (POINT-PDL-POP *WINDOW*)
	   (POINT-PDL-MOVE BP PLINE))
	 DIS-BPS)
	(T
	 (POINT-PDL-POP *WINDOW*)
	 DIS-NONE)))

))

; From file FORMAT.LISP PS:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-ERROR (STRING &REST ARGS)
  (IF (STRINGP CTL-STRING)
      (FERROR NIL "~1{~:}~%~VT~%~3@T/"~A/"~%" STRING ARGS
	      (- CTL-INDEX
		 1
		 (OR (STRING-REVERSE-SEARCH-CHAR #\RETURN CTL-STRING CTL-INDEX)
		     -4))
	      CTL-STRING)
      (FERROR NIL "~1{~:}" STRING ARGS)))

))

(load "SYS:FONTS;5X5 QFASL >" :verbose nil :set-pathname-default nil)
(tv:update-font-maps)

; From file RDDEFS.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; RDDEFS  "

(DEFCONST XR-SPECIAL-CHARACTER-NAMES
  (APPEND '((:NULL . 200) (:NULL-CHARACTER . 200)
	    (:BREAK . 201) (:BRK . 201)
	    (:CLEAR-INPUT . 202) (:CLEAR . 202) (:CLR . 202)
	    (:CALL . 203)
	    (:TERMINAL . 204) (:ESC . 204) (:ESCAPE . 204) (:TERMINAL-ESCAPE . 204)
	    (:MACRO . 205) (:BACK-NEXT . 205) (:BACKNEXT . 205)
	    (:HELP . 206)
	    (:RUBOUT . 207)
	    (:OVERSTRIKE . 210) (:BACKSPACE . 210) (:BS . 210)
	    (:TAB . 211)
	    (:LINE . 212) (:LF . 212) (:LINEFEED . 212) (:LINE-FEED . 212)
	    (:DELETE . 213) (:VT . 213)
	    ;; The keyboard says "CLEAR SCREEN", but it should type out as "PAGE".
	    (:PAGE . 214) (:CLEAR-SCREEN . 214) (:FORM . 214) (:FF . 214)
	    (:RETURN . 215) (:CR . 215) (:NEWLINE . 215)
	    (:QUOTE . 216)
	    (:HOLD-OUTPUT . 217)
	    (:STOP-OUTPUT . 220)
	    (:ABORT . 221)
	    (:RESUME . 222)
	    (:STATUS . 223)
	    (:END . 224)
	    (:ROMAN-I . 225) (:ROMAN-II . 226) (:ROMAN-III . 227) (:ROMAN-IV . 230)
	    (:HAND-UP . 231) (:HAND-DOWN . 232)
	    (:HAND-LEFT . 233) (:HAND-RIGHT . 234)
	    (:SYSTEM . 235) (:NETWORK . 236)

	    (:CENTER-DOT . 0) (:DOWN-ARROW . 1)
	    (:ALPHA . 2) (:BETA . 3) (:AND-SIGN . 4) (:NOT-SIGN . 5)
	    (:EPSILON . 6) (:PI . 7) (:LAMBDA . 10) (:GAMMA . 11) (:DELTA . 12)
	    (:UP-ARROW . 13) (:UPARROW . 13)
	    (:PLUS-MINUS . 14) (:CIRCLE-PLUS . 15)
	    (:INFINITY . 16) (:PARTIAL-DELTA . 17)
	    (:LEFT-HORSESHOE . 20) (:RIGHT-HORSESHOE . 21)
	    (:UP-HORSESHOE . 22) (:DOWN-HORSESHOE . 23)
	    (:UNIVERSAL-QUANTIFIER . 24) (:EXISTENTIAL-QUANTIFIER . 25)
	    (:CIRCLE-X . 26) (:CIRCLE-CROSS . 26) (:TENSOR . 26)
	    (:DOUBLE-ARROW . 27) (:LEFT-ARROW . 30) (:RIGHT-ARROW . 31)
	    (:NOT-EQUAL . 32)(:NOT-EQUALS . 32)
	    (:ALTMODE . 33) (:ALT . 33) (:DIAMOND . 33)
	    (:LESS-OR-EQUAL . 34) (:GREATER-OR-EQUAL . 35) (:EQUIVALENCE . 36)
	    (:OR-SIGN . 37) (:OR . 37)

	    (:SPACE . 40) (:SP . 40)
	    (:INTEGRAL . 177)
	    )
	  (MAPCAR #'(LAMBDA (X) (CONS (CAR X)
				      (DPB 1 %%XR-SPECIAL-CHARACTER-NAMES-MOUSE-BIT
					   (CDR X))))
	      '((:MOUSE-L . 0) (:MOUSE-L-1 . 0) (:MOUSE-L-2 . 10) (:MOUSE-L-3 . 20)
		(:MOUSE-M . 1) (:MOUSE-M-1 . 1) (:MOUSE-M-2 . 11) (:MOUSE-M-3 . 21)
		(:MOUSE-R . 2) (:MOUSE-R-1 . 2) (:MOUSE-R-2 . 12) (:MOUSE-R-3 . 22)
		(:MOUSE-1-1 . 0) (:MOUSE-1-2 . 10)
		(:MOUSE-2-1 . 1) (:MOUSE-2-2 . 11)
		(:MOUSE-3-1 . 2) (:MOUSE-3-2 . 12))))
  "Alist of names of special characters, in the form of symbols in the keyword pkg,
and the character values they correspond to.")

))

; From file DOCMIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'ASH 'FUNCTION)
  "Shift N arithmetically by NBITS. N must be an integer.")

))

; From file PRIMIT.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PRIMIT  "

(DEFUN UPCASE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Uppercasify all characters in the specified interval."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS (BP-= BP1 BP2)
    (UNDO-SAVE-NEW-SMALL-CHANGE BP1 BP2)
    (MUNG-BP-INTERVAL BP1)
    (CHARMAP-PER-LINE (BP1 BP2 NIL)
		      ((MUNG-LINE (CHARMAP-LINE)))
      (LET ((BEFORE (CHARMAP-CHAR)))
	(LET ((AFTER (CHAR-UPCASE BEFORE)))
	  (COND ((NOT (= BEFORE AFTER))
		 (CHARMAP-SET-CHAR AFTER))))))))

))

; From file PRIMIT.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PRIMIT  "

(DEFUN DOWNCASE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Lowercasify all characters in the specified interval."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS (BP-= BP1 BP2)
    (UNDO-SAVE-NEW-SMALL-CHANGE BP1 BP2)
    (MUNG-BP-INTERVAL BP1)
    (CHARMAP-PER-LINE (BP1 BP2 NIL)
		      ((MUNG-LINE (CHARMAP-LINE)))
      (LET ((BEFORE (CHARMAP-CHAR)))
	(LET ((AFTER (CHAR-DOWNCASE BEFORE)))
	  (COND ((NOT (= BEFORE AFTER))
		 (CHARMAP-SET-CHAR AFTER))))))))

))

; From file COMA.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFINE-COMMAND-DOCUMENTATION COM-SELF-INSERT COM ; Not used
  (SELECTQ OP
    (:NAME "Self Insert")
    (:FULL (FORMAT *STANDARD-OUTPUT*
		   "~&Self insertion: ~:C inserts a /"~:[~C~;~\lozenged-char\~]/"."
		   CHAR (CHAR-EQUAL CHAR #\OVERSTRIKE) CHAR))
    (:SHORT (FORMAT STANDARD-OUTPUT "Self insertion"))))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFSIGNAL SYS:NON-POSITIVE-LOG ARITHMETIC-ERROR (NUMBER)
	   "NUMBER was used as the argument to a logarithm function.")

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN RENAME-PACKAGE (PKG NEW-NAME &OPTIONAL NEW-NICKNAMES)
  "Change the name(s) of a package."
  (UNLESS (PACKAGEP PKG)
    (SETF PKG (PKG-FIND-PACKAGE PKG)))
  (UNLESS (CLI:LISTP NEW-NICKNAMES) (SETQ NEW-NICKNAMES (LIST NEW-NICKNAMES)))
  (WITHOUT-INTERRUPTS
    (LET ((TEM (FIND-PACKAGE NEW-NAME)))
      (WHEN (AND TEM (NEQ TEM PKG))
	(FERROR NIL "A package named ~A already exists." NEW-NAME)))
    (DOLIST (NICK NEW-NICKNAMES)
      (LET ((TEM (FIND-PACKAGE NICK)))
	(WHEN (AND TEM (NEQ TEM PKG))
	  (FERROR NIL "A package named ~A already exists." NICK))))
    (SETF (PKG-NAME PKG) NEW-NAME)
    (SETF (PKG-NICKNAMES PKG) NEW-NICKNAMES)))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN ALTER-PACKAGE (NAME &KEY &OPTIONAL NICKNAMES
		      (USE '("GLOBAL")) ((:SIZE IGNORE))
		      SHADOW EXPORT PREFIX-NAME AUTO-EXPORT-P
		      IMPORT SHADOWING-IMPORT IMPORT-FROM
		      RELATIVE-NAMES RELATIVE-NAMES-FOR-ME
		      ((:HASH-INHERITED-SYMBOLS IGNORE))
		      (EXTERNAL-ONLY NIL EXTERNAL-ONLY-P)
		      ((:INCLUDE IGNORE)) ((:COLON-MODE IGNORE)))
  (LET ((PKG (FIND-PACKAGE NAME)))
    (UNLESS PKG (FERROR NIL "Package ~A not found." NAME))
    (UNLESS (CLI:LISTP NICKNAMES) (SETQ NICKNAMES (LIST NICKNAMES)))
    (RENAME-PACKAGE PKG (PKG-NAME PKG) NICKNAMES)
    (UNLESS (OR (NULL PREFIX-NAME) (STRING= PREFIX-NAME NAME)
		(MEM 'STRING= PREFIX-NAME NICKNAMES))
      (FERROR NIL "The prefix name ~A is not a name or nickname of the package." PREFIX-NAME))
    (SETF (PKG-PREFIX-PRINT-NAME PKG) PREFIX-NAME)
    (SHADOW SHADOW PKG)
    (SHADOWING-IMPORT SHADOWING-IMPORT PKG)
    (EXPORT EXPORT PKG)
    (LET ((DESIRED-USE (IF (OR (LISTP USE) (NULL USE))
			   (MAPCAR 'FIND-PACKAGE USE)
			 (LIST (FIND-PACKAGE USE)))))
      (DOLIST (ELT (PKG-USE-LIST PKG))
	(UNLESS (MEMQ ELT DESIRED-USE)
	  (UNUSE-PACKAGE ELT PKG)))
      (USE-PACKAGE DESIRED-USE PKG))
    (IMPORT IMPORT PKG)
    (WHEN IMPORT-FROM
      (DOLIST (NAME (CDR IMPORT-FROM))
	(IMPORT (INTERN (STRING NAME) (CAR IMPORT-FROM)) PKG)))
    (IF (IF EXTERNAL-ONLY-P EXTERNAL-ONLY AUTO-EXPORT-P)
	(PKG-MARK-HAVING-SUBPACKAGES PKG)
      (SETF (PKG-AUTO-EXPORT-P PKG) NIL)
      (SETF (PKG-STORE-FUNCTION PKG) NIL))
    (SETF (PKG-REFNAME-ALIST PKG)
	  (LOOP FOR (NICK . P) IN RELATIVE-NAMES
		COLLECT (CONS (STRING NICK)
			      (FIND-PACKAGE P))))
    ;; First delete any other local nicknames, in any package, for this one.
    (DOLIST (P *ALL-PACKAGES*)
      (SETF (PKG-REFNAME-ALIST P)
	    (CLI:DELETE PKG (PKG-REFNAME-ALIST P) ':KEY 'CDR)))
    ;; Then add the ones that are requested.
    (DOLIST (ELT RELATIVE-NAMES-FOR-ME)
      (PKG-ADD-RELATIVE-NAME (CAR ELT) (CADR ELT) PKG))
    PKG))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defun com-help (sg error-object &rest ignore)
  "Help for using the debugger"
  (tagbody
   again
      (selectq (fquery `(:choices ,com-help-alist
				  :help-function nil)
		       "Help for debugger commands. Choose a topic: ")
	(#\G (format t "

You are in the debugger.  If you don't want to debug this error, type ~C.
Otherwise you can evaluate expressions in the context of the error, examine
the stack, and proceed//throw//return to recover.
  If you type in a Lisp form, it will be evaluated, and the results printed,
using ~A syntax and semantics and base ~D in package ~A. This
evaluation uses the variable environment of the stack frame you are examining.
  Type ~c or ~c to get back to top level, or the previous debugger level.
While in the debugger, ~c quits back to the debugger top level.
  If you think this error indicates a bug in the Lisp machine system, use the
~:c command."
		     #\abort (if *default-common-lisp* "Common Lisp" "traditional")
		     ibase (package-name package)
		     #\abort #\c-Z #\c-G #\c-M))
	(#\I (format t "

~c or ~\lozenged-string\ clears screen and retypes error message.
~c clears screen and types args, locals and compiled code.
~c gives a backtrace of function names.
~c gives a backtrace of function names and argument names and values.
~c is line ~c but shows EVALs, PROGs, CONDs, etc.
~c prints an argument to the current function, and sets * to be that
   argument to let you do more complicated things with it.
   + is set to a locative to that argument, should you want to modify it.
   To specify which argument, type the argument number with Control
   or Meta held down before the ~c.
~c is like ~c but works on the function's locals rather than the args.
~c is like ~c but works on the values this frame is returning.
   (This is useful when you get a trap on exit from function).
~c does likewise for the function itself.
~c prints the arglist of the function in the current frame.
~c prints the value in this frame of a special variable you specify.
~c lists all special variable bindings in this frame.

Use the functions (EH-ARG n), (EH-LOC n), (EH-VAL n) and (EH-FUN) to get the
value of an arg, local, value or function-object respectively from an
expression being evaluated. For args and locals, n can be a name or a number.
EH-VAL allows numbers only. LOCF and SETF on those expressions are also allowed.
" #\c-L "Clear Screen" #\m-L #\c-B #\c-m-B #\m-B #\c-B
  #\c-m-A #\c-m-A #\c-m-L #\c-m-A #\c-m-V #\c-m-A #\c-m-F #\c-A #\m-S #\c-m-S))
	(#\F (format t "

~c or ~\lozenged-char\ goes down a frame, ~c or ~\lozenged-char\ goes up.
~c and ~c are similar but show args, locals and compiled code.
~c and ~c are similar to ~c and ~c, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
~c and ~c go to the top and bottom of the stack, respectively.
~c reads a string and searches down the stack for a frame
   calling a function whose name contains that substring.
" #\c-N #\line #\c-P #\return #\m-N #\m-P #\c-m-N #\c-m-P #\c-N #\c-P #\m-< #\m-> #\c-S))
	(#\S (if error-handler-running
		 (format t "

~c toggles the trap-on-exit flag for the current frame.
~c sets the trap-on-exit flag for the current frame and all outer frames.
~c clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
~c proceeds like ~C, but first sets the trap-on-next-function-call flag.
~c toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with ~c.
" #\c-X #\m-X #\c-m-X #\c-D #\resume #\m-D #\c-X)
	       (format t "

You cannot use the stepping commands, since you are in examine-only mode.
~C exits the debugger." #\resume)))
	(#\P (cond (error-handler-running
		    (format t "

~c aborts to previous debugger or other command loop, or to top level.
~c returns a value or values from the current frame.
~c offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
~c offers to reinvoke the current frame, letting you alter
   some of the arguments, or use more or fewer arguments.
~c throws to a specific tag." #\c-Z #\c-R #\c-m-R #\m-R #\c-T)
		    (describe-proceed-types sg error-object))
		   (t
		    (format t "

You cannot continue execution, since you are in examine-only mode.
~C exits the debugger." #\resume))))
	(#\T (format t "

~c calls the editor to edit the current function.
~c enters the editor to send a bug message, and puts the error
  message and a backtrace into the message automatically.
  A numeric argument says how many stack frames to put in the backtrace.
~c switches to the window-based debugger.
" #\c-E #\c-M #\c-m-W))
	(#\D (com-help-describe-command error-object sg))
	(#\help
	 (format t "~&For additional help in using the debugger, type one of the following:~%")
	 (dolist (x com-help-alist)
	   (unless (eq (third (car x)) t)
	     (format t "~&  ~:C~6T~A" (cadr x) (or (third (car x)) (second (car x))))))
	 (go again))
	(#\abort))))

))

(eval-when (eval compile load) (intern "ATAN" "CLI"))
; From file NUMER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(defun cli:atan (y x)
  "Arctangent in radians of y//x, between - and .  Small flonum arg gets small flonum value."
   (cond ((< y 0) (- (atan (- y) x)))
	 (t (atan y x))))

))

; From file NUMER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(deff atan2 'cli:atan)

))

; From file NUMER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMER  "

(defun atan (y x)
  "Arctangent in radians of y//x, between 0 and 2.  Small flonum arg gets small flonum value."
  (let ((absx (abs (float x)))
	(absy (abs (float y)))
	(temp)
	(temp2)
	(ans -0.004054058))
    (setq temp (// (- absy absx) (+ absy absx))
	  temp2 (* temp temp))
    (do ((l '( 0.0218612288 -0.0559098861  0.0964200441
	      -0.139085335   0.1994653499 -0.3332985605 0.9999993329)
	    (cdr l)))
	((null l))
      (setq ans (+ (* ans temp2) (car l))))
    (setq ans (* ans temp))
    (setq temp (abs ans))
    (cond ((or (>= temp .7855) (< temp .7853))
	   (setq ans (+ ans 0.7853981634)))
	  ((< ans 0) (setq ans (// absy absx)))
	  (t (setq ans (+ (// absx absy) 1.5707963268))))
    (setq temp ans
	  ans (- 3.1415926536 ans))
    (if (>= x 0) (psetq temp ans ans temp))
    (setq temp (* temp 2))
    (if (< y 0) (setq ans (+ ans temp)))
    (if (and (small-floatp x) (small-floatp y))
	(small-float ans)
      ans)))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFVAR *REMEMBER-PASSWORDS* :UNBOUND
  "T => record passwords when the user types them, in case they are useful again.")
(FORWARD-VALUE-CELL '*REMEMBER-PASSWORDS* 'RECORD-PASSWORDS-FLAG)

))

; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFVAR *COMMON-LISP-SYMBOL-SUBSTITUTIONS*
	(COPYTREE
	  '((// . CLI://) 
	    (AR-1 . CLI:AR-1)
	    (AR-1-FORCE . CLI:AR-1-FORCE)
	    (AREF . CLI:AREF)
	    (ASSOC . CLI:ASSOC)
	    (ATAN . CLI:ATAN)
	    (CATCH . CLI:CATCH)
	    (CHARACTER . CLI:CHARACTER)
	    (CLOSE . CLI:CLOSE)
	    (COPY-READTABLE . CLI:COPY-READTABLE)
	    (DEFSTRUCT . CLI:DEFSTRUCT)
	    (DELETE . CLI:DELETE)
	    (ERROR . CLI:ERROR)
	    (EVAL . CLI:EVAL)
	    (EVERY . CLI:EVERY)
	    (FORMAT . CLI:FORMAT)
	    (INTERSECTION . CLI:INTERSECTION)
	    (LAMBDA . CLI:LAMBDA)
	    (LISTP . CLI:LISTP)
	    (MAP . CLI:MAP)
	    (MEMBER . CLI:MEMBER)
	    (NAMED-LAMBDA . CLI:NAMED-LAMBDA)
	    (NAMED-SUBST . CLI:NAMED-SUBST)
	    (NINTERSECTION . CLI:NINTERSECTION)
	    (NLISTP . CLI:NLISTP)
	    (NUNION . CLI:NUNION)
	    (RASSOC . CLI:RASSOC)
	    (READ . CLI:READ)
	    (READ-FROM-STRING . CLI:READ-FROM-STRING)
	    (REM . CLI:REM)
	    (REMOVE . CLI:REMOVE)
	    (SOME . CLI:SOME)
	    (SUBST . CLI:SUBST)
	    (TERPRI . CLI:TERPRI)
	    (THROW . CLI:THROW)
	    (UNION . CLI:UNION)
	    (WARN . CLI:WARN))
	  )
  "Alist used as *READER-SYMBOL-SUBSTITUTIONS* for reading Common Lisp code.")

))

; From file EVAL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do*-internal (x &aux varlist endtest retvals oncep)
  (if (and (car x) (atom (car x)))	;OLD STYLE
      (if (neq interpreter-function-environment t)
	  (ferror nil "Maclisp old-style DO not allowed in Common Lisp.")
	(bind (value-cell-location (car x)) (eval (cadr x)))
	(do-body nil (cadddr x) nil
		 t x (cddddr x)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq interpreter-function-environment t)
	(zl-serial-binding-list (varlist)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x) t))
      (gobble-declarations-from-body (decls-env (cddr x))
	(serial-binding-list (varlist decls-env)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x) t))))))

))

; From file RESOUR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFMACRO USING-RESOURCE ((VAR RESOURCE-NAME . PARAMETERS) &BODY BODY)
  "Execute BODY with VAR bound to an object allocated from resource RESOURCE-NAME.
PARAMETERS are used in selecting or creating the object,
according to the definition of the resource."
  (multiple-value-bind (body declarations)
      (extract-declarations body)
    `(LET ((,VAR NIL))
       ,(if declarations `(declare . ,declarations))
       (UNWIND-PROTECT
	   (PROGN
	     (SETQ ,VAR (ALLOCATE-RESOURCE ',RESOURCE-NAME . ,PARAMETERS))
	     . ,BODY)
	 (AND ,VAR (DEALLOCATE-RESOURCE ',RESOURCE-NAME ,VAR))))))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING-EQUAL (STRING1 STRING2 &REST ARGS)
  "T if STRING1 and STRING2's contents are the same.
Case is ignored in comparing characters unless
ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON is non-NIL.
The keyword arguments allow you to compare only part of a string.
The range of STRING1 to be compared runs from START1 to END1
and the range of STRING2 runs from START2 to END2.
If END1 or END2 omitted or NIL, the end of that string is used."
  (DECLARE (ARGLIST STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2))
  (LET (IDX1 IDX2 LIM1 LIM2)
    (IF (AND (CAR ARGS) (SYMBOLP (CAR ARGS)) (KEYWORDP (CAR ARGS)))
	(SETQ IDX1 (GET (LOCF ARGS) ':START1)
	      IDX2 (GET (LOCF ARGS) ':START2)
	      LIM1 (GET (LOCF ARGS) ':END1)
	      LIM2 (GET (LOCF ARGS) ':END2))
      (LIST-MATCH-P ARGS `(,IDX1 ,IDX2 ,LIM1 ,LIM2)))
    (OR IDX1 (SETQ IDX1 0))
    (OR IDX2 (SETQ IDX2 0))
    (COERCE-STRING-ARG STRING1)
    (COERCE-STRING-ARG STRING2)
    (COND ((OR LIM1 LIM2) 
	   (OR LIM1 (SETQ LIM1 (ARRAY-ACTIVE-LENGTH STRING1)))
	   (OR LIM2 (SETQ LIM2 (ARRAY-ACTIVE-LENGTH STRING2)))
	   (AND (= (SETQ LIM1 (- LIM1 IDX1)) (- LIM2 IDX2))
		(%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 LIM1)))
	  (T (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 NIL)))))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING= (STRING1 STRING2 &REST ARGS)
  "T if STRING1 and STRING2's contents are the same, case being significant.
The keyword arguments allow you to compare only part of a string.
The range of STRING1 to be compared runs from START1 to END1
and the range of STRING2 runs from START2 to END2.
If END1 or END2 omitted or NIL, the end of that string is used."
  (DECLARE (ARGLIST STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2))
  (LET (IDX1 IDX2 LIM1 LIM2
	(ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
    (IF (AND (CAR ARGS) (SYMBOLP (CAR ARGS)) (KEYWORDP (CAR ARGS)))
	(SETQ IDX1 (GET (LOCF ARGS) ':START1)
	      IDX2 (GET (LOCF ARGS) ':START2)
	      LIM1 (GET (LOCF ARGS) ':END1)
	      LIM2 (GET (LOCF ARGS) ':END2))
      (LIST-MATCH-P ARGS `(,IDX1 ,IDX2 ,LIM1 ,LIM2)))
    (OR IDX1 (SETQ IDX1 0))
    (OR IDX2 (SETQ IDX2 0))
    (COERCE-STRING-ARG STRING1)
    (COERCE-STRING-ARG STRING2)
    (COND ((OR LIM1 LIM2) 
	   (OR LIM1 (SETQ LIM1 (ARRAY-ACTIVE-LENGTH STRING1)))
	   (OR LIM2 (SETQ LIM2 (ARRAY-ACTIVE-LENGTH STRING2)))
	   (AND (= (SETQ LIM1 (- LIM1 IDX1)) (- LIM2 IDX2))
		(%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 LIM1)))
	  (T (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 NIL)))))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN ENABLE-STACK-GROUP (THING OFF-OR-ON)
  (COND	((TYPEP THING ':STACK-GROUP))
	((EQ (DATA-TYPE THING) 'DTP-INSTANCE)
	 (LET ((WO (FUNCALL THING ':WHICH-OPERATIONS)))
	   (COND ((MEMQ ':STACK-GROUP WO)
		  (SETQ THING (FUNCALL THING ':STACK-GROUP)))
		 ((MEMQ ':PROCESS WO)
		  (SETQ THING (FUNCALL (FUNCALL THING ':PROCESS) ':STACK-GROUP))))))
	(T (FERROR NIL "Can't meter ~S" THING)))
  (WITHOUT-INTERRUPTS
    (IF (EQ THING %CURRENT-STACK-GROUP)
	(SETQ %MODE-FLAGS
	      (%LOGDPB OFF-OR-ON %%M-FLAGS-METER-ENABLE %MODE-FLAGS))
	(SETF (SI:SG-FLAGS-METER-ENABLE THING) OFF-OR-ON)))
  THING)

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN METER-FIX-SIGNED (BUF INDEX)
  (LET* ((HIGH (AREF BUF (1+ INDEX)))
	 (ANS (DPB HIGH 2010 (LDB 0020 (AREF BUF INDEX)))))
    (IF (LDB-TEST 1001 HIGH)
	(LOGIOR ANS -100000000)
      ANS)))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN METER-FIX-UNSIGNED (BUF INDEX)
  (DPB (AREF BUF (1+ INDEX)) 2011 (LDB 0020 (AREF BUF INDEX))))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN METER-Q (BUF INDEX)
  (LET ((TEMP (AREF BUF (1+ INDEX))))
    (%MAKE-POINTER (LDB 1105 TEMP)
		   (DPB TEMP 2011 (AREF BUF INDEX)))))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN ANALYZE (&REST OPTIONS &AUX RET-INFO INFO (FLUSH-INFO T) CLOSE-STREAM
		(STREAM STANDARD-OUTPUT))
  "Analyze the information recorded by metering.
:STREAM specifies a stream to print the analysis on;
alternatively, :FILE specifies a filename to write it to,
or :BUFFER specifies an editor buffer to write it to.
:ANALYZER specifies a kind of analysis.
/(:TREE is the default; :LIST-EVENTS is also useful).
If you specify :RETURN T, the intermediate data structure is returned.
Passing that data structure as the :INFO argument, you can save much time.
You can also keep the intermediate data structure while metering other computations.

Particular analyzers allow additional keyword arguments.
:TREE handles these:
 :FIND-CALLERS - arg is function spec or list of them;
   say where each of those function was called from and how often.
   This is instead of the ordinary output.
 :STACK-GROUP - arg is stack group or list of them;
   analyze only activities in those stack group.
 :SORT-FUNCTION - passed to SORT-GROUPED-ARRAY-GROUP-KEY.
   Try MAX-CALLS, MAX-RUN-TIME, MAX-REAL-TIME, MAX-PAGE-FAULTS,
   or MAX-RUN-TIME-PER-CALL (all in METER:).
 :SUMMARIZE - arg is function spec of list of them;
   mention only those functions in the output.
 :INCLUSIVE - non-NIL means include time spent within subroutines
   in the times for each function.

Note: to execute something and meter it, use METER:TEST or METER:RUN."
  (DECLARE (ARGLIST &KEY &OPTIONAL ANALYZER STREAM FILE BUFFER RETURN INFO
		    FIND-CALLERS STACK-GROUP SORT-FUNCTION SUMMARIZE INCLUSIVE
		    &ALLOW-OTHER-KEYS))
  (UNWIND-PROTECT
    (LET ((EVENT-TABLE (GET ':TREE 'EVENT-TABLE))
	  (OPT-LIST))
      (DO ((L OPTIONS (CDDR L)))
	  ((NULL L))
	(SELECTQ (FIRST L)
	  (:STREAM (SETQ STREAM (CADR L)))
	  (:FILE (SETQ CLOSE-STREAM T
		       STREAM (OPEN (CADR L) 'OUT)))
	  (:BUFFER (SETQ STREAM (ZWEI:INTERVAL-STREAM (ZWEI:FIND-BUFFER-NAMED (CADR L) T))))
	  (:INFO (SETQ INFO (CADR L) FLUSH-INFO NIL))
	  (:ANALYZER (SETQ EVENT-TABLE (IF (TYPEP (CADR L) 'EVENT-TABLE)
					   (CADR L)
					 (GET (CADR L) 'EVENT-TABLE))))
	  (:RETURN (SETQ RET-INFO (CADR L) FLUSH-INFO NIL))
	  (OTHERWISE (PUTPROP (LOCF OPT-LIST) (CADR L) (CAR L)))))
      (IF (NULL INFO)
	  (DO-OVER-DATA ((MAX-INDEX (ARRAY-DIMENSION EVENT-TABLE 0))
			 (FCTN (SETQ INFO (FUNCALL (EVENT-TABLE-INIT-FUNCTION EVENT-TABLE)
						   OPT-LIST STREAM BUF INDEX)))
			 (EVENT))
			()
	    (SETQ EVENT (COPY-FRAME-TO-STATE BUF INDEX (NEXT-STATE INFO)))
	    (SETQ FCTN (COND ((NULL EVENT) NIL)
			     ((< EVENT MAX-INDEX) (AREF EVENT-TABLE EVENT))
			     (T (CDR (ASSQ EVENT (EVENT-TABLE-EVENTS EVENT-TABLE))))))
	    (IF FCTN
		(FUNCALL FCTN BUF INDEX INFO STREAM))))
      (FUNCALL (EVENT-TABLE-EXIT-FUNCTION EVENT-TABLE) INFO STREAM OPT-LIST)
      (AND RET-INFO INFO))
    (AND FLUSH-INFO INFO (ANALYZE-FREE INFO))
    (AND CLOSE-STREAM STREAM (CLOSE STREAM))))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN COPY-FRAME-TO-STATE (BUF INDEX STATE)
  (LET ((EVENT (AREF BUF INDEX))
	(LENGTH (AREF BUF (1+ INDEX))))
    (COND (( LENGTH 0)
	   (SETF (LOW-REAL-TIME STATE) (AREF BUF (+ INDEX 2)))
	   (SETF (HIGH-REAL-TIME STATE) (AREF BUF (+ INDEX 3)))
	   (SETF (LOW-DISK-TIME STATE) (AREF BUF (+ INDEX 4)))
	   (SETF (HIGH-DISK-TIME STATE) (AREF BUF (+ INDEX 5)))
	   (SETF (PAGE-FAULTS STATE) (METER-FIX-UNSIGNED BUF (+ INDEX 6)))
	   (SETF (STACK-GROUP STATE) (METER-Q BUF (+ INDEX 10)))
	   (SETF (CURRENT-FUNCTION STATE) (METER-Q BUF (+ INDEX 12)))
	   (SETF (STACK-DEPTH STATE) (METER-FIX-UNSIGNED BUF (+ INDEX 14)))
	   EVENT)
	  (T NIL))))


))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFCONST FIND-CLOSEST-SYM-FUN
	  (IF-IN-CADR-ELSE-LAMBDA (INTERN "CC-FIND-CLOSEST-SYM" 'CC)
	    (INTERN "LAM-FIND-CLOSEST-SYM" 'LAM)))
(DEFCONST RACMO
	  (SYMBOL-VALUE (INTERN "RACMO" (IF-IN-CADR-ELSE-LAMBDA 'CC 'LAM))))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN LIST-EVENT-FUNCTION (BUF INDEX INFO STREAM)
  (LET ((REAL-TIME)
	(RUN-TIME)
	(PAGE-FAULTS)
	(NEW-STATE)
	(EVENT (AREF BUF INDEX)))
  (MULTIPLE-VALUE (REAL-TIME RUN-TIME PAGE-FAULTS)
    (STATE-RELATIVE-INFO (BASE-STATE INFO) (SETQ NEW-STATE (NEXT-STATE INFO))))
  (FORMAT STREAM "~&~9D ~9D ~4D ~20A ~20S ~5D "
	  REAL-TIME RUN-TIME PAGE-FAULTS
	  (SG-NAME (STACK-GROUP NEW-STATE))
	  (FUNCTION-NAME (CURRENT-FUNCTION NEW-STATE))
	  (STACK-DEPTH NEW-STATE))
  (SELECT EVENT
    ((%METER-FUNCTION-ENTRY-EVENT %METER-FUNCTION-EXIT-EVENT)
     (FORMAT STREAM "~:[RET ~;CALL~] ~S"
	     (= EVENT %METER-FUNCTION-ENTRY-EVENT)
	     (FUNCTION-NAME (METER-Q BUF (+ INDEX 14.)))))
    (%METER-FUNCTION-UNWIND-EVENT
     (FORMAT STREAM "UNWIND"))
    ((%METER-PAGE-IN-EVENT %METER-PAGE-OUT-EVENT)
     (LET ((VMA (METER-FIX-UNSIGNED BUF (+ INDEX 14.)))
	   (UPC (METER-FIX-UNSIGNED BUF (+ INDEX 16.)))
	   (MICRO-NAME))
       (SETQ MICRO-NAME
	     (FUNCALL FIND-CLOSEST-SYM-FUN
		      (+ RACMO (LDB 0020 UPC))))
       (IF (CONSP MICRO-NAME)
	   (SETQ MICRO-NAME (CAR MICRO-NAME)))
       (FORMAT STREAM "~:[PAGI~;PAGO~]~%~10T~8O (~S) ~S"
	       (= EVENT %METER-PAGE-OUT-EVENT)
	       VMA
	       (AREF #'AREA-NAME (%AREA-NUMBER VMA))
	       (OR MICRO-NAME (LDB 0020 UPC)))))
    (%METER-STACK-GROUP-SWITCH-EVENT
     (FORMAT STREAM "~A" (SG-NAME (METER-Q BUF (+ INDEX 14.)))))
    (OTHERWISE
     (FORMAT STREAM "~&Bad event")))))

))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

(DEFUN SUMMARIZE-TREE (INFO STREAM
		       &AUX STK-GROUP (SORT-FCTN 'MAX-RUN-TIME) ONLY-FOR INCLUSIVE)
  (DECLARE (SPECIAL ONLY-FOR INCLUSIVE))
  (DO ((PL (TREE-INFO-OUTPUT-PLIST INFO) (CDDR PL)))
      ((NULL PL))
    (SELECTQ (CAR PL)
      (:STACK-GROUP (SETQ STK-GROUP (CADR PL)))
      (:SORT-FUNCTION (SETQ SORT-FCTN (CADR PL)))
      (:SUMMARIZE (SETQ ONLY-FOR (FUNCTION-LIST (CADR PL))))
      (:INCLUSIVE (SETQ INCLUSIVE (CADR PL)))
      (OTHERWISE (FERROR NIL "Output option ~S not recognized" (CAR PL)))))
  (USING-RESOURCE (HASH SUMMARY-TABLES)
    (DECLARE (SPECIAL HASH))
    (USING-RESOURCE (SUMM SUMMARY-INFOS)
      (DECLARE (SPECIAL SUMM))
      (CLRHASH HASH)
      (STORE-ARRAY-LEADER 0 SUMM 0)
      (DOLIST (L (TREE-INFO-STACK-GROUPS INFO))
	(COND ((AND (OR (NULL STK-GROUP) (EQ (CAR L) STK-GROUP)
		    (AND (LISTP STK-GROUP) (MEMQ (CAR L) STK-GROUP)))
		(NOT (NULL (CDR L))))
	       (FORMAT STREAM "~&Stack Group: ~A" (SG-NAME (CAR L)))
	       (SUMMARY-BRANCH INFO (CDR L)))))
      (PRINT-SUMMARY-INFORMATION SUMM SORT-FCTN STREAM))))

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN INCREMENT-PATCH-SYSTEM-MAJOR-VERSION (NAME STATUS &AUX VERSION PATCH-MAJOR)
  "Increment the major version of the patchable system NAME, and set status to STATUS.
This modifies the patch directory files of the system."
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME T))
  (COND ((NULL VERSION)
	 (FORMAT T "~&No master directory for system ~A, creating one." NAME)
	 (SETQ VERSION 0)))
  (SETQ VERSION (1+ VERSION)
	PATCH-MAJOR (MAKE-PATCH-MAJOR NAME NAME VERSION VERSION))
  (WITH-OPEN-FILE (FILE (PATCH-SYSTEM-PATHNAME NAME ':SYSTEM-DIRECTORY) '(:WRITE))
    (FORMAT FILE
	    ";;; -*- Mode: Lisp; Package: User; Base: 10.; Common-Lisp: NIL; Patch-File: T -*-
")
    (WRITE-RESPONSIBILITY-COMMENT FILE)
    (LET ((*PRINT-BASE* 10.))
      (PRINT PATCH-MAJOR FILE)))
  (LET ((FIRST-VERS (MAKE-PATCH-VERSION NUMBER 0
					EXPLANATION (FORMAT NIL "~A Loaded" NAME))))
    (WRITE-PATCH-DIRECTORY PATCH-MAJOR (MAKE-PATCH-DIR STATUS STATUS
						       VERSION-LIST (NCONS FIRST-VERS))))
  VERSION)

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN READ-PATCH-DIRECTORY (PATCH-SYSTEM &OPTIONAL NOERROR &AUX DIR)
  "Read in a patch directory file, returning the list-structure representation.
PATCH-SYSTEM is an object of type PATCH-SYSTEM.
The value is described by the defstruct PATCH-DIR.
NOERROR means return NIL rather than get error if patch directory file won't open."
  (CONDITION-CASE-IF NOERROR ()
      (WITH-OPEN-FILE (PATCH-DIR (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
							':VERSION-DIRECTORY
							(PATCH-VERSION PATCH-SYSTEM)))
	(LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE)
	      (*READTABLE* SI:INITIAL-READTABLE))
	  (SETQ DIR (READ PATCH-DIR)))
	(ECASE (CAR DIR)
	  (:EXPERIMENTAL)
	  (:RELEASED)
	  (:BROKEN)
	  (:INCONSISTENT)
	  (:OBSOLETE))
	DIR)
    (FS:FILE-ERROR NIL)))

))

;;patched again in 98.40
;; From file PATCH.LISP PS:<L.SYS2> OZ:
;#8R SYSTEM-INTERNALS#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "
;
;(DEFUN WRITE-PATCH-DIRECTORY (PATCH-SYSTEM PATCH-DIR)
;  "Write out a new patch directory file for PATCH-SYSTEM.
;PATCH-DIR is a list described by the defstruct PATCH-DIR,
;which is the data to write into the file."
;  (LET ((*PRINT-BASE* 10.) (*READ-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE) (*NOPOINT T)
;	(*READTABLE* SI:INITIAL-READTABLE))
;    (WITH-OPEN-FILE (STREAM (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
;						   ':VERSION-DIRECTORY
;						   (PATCH-VERSION PATCH-SYSTEM))
;			       '(:WRITE))
;       (FORMAT STREAM
;	       ";;; -*- Mode: Lisp; Package: User; Base: 10.; Common-Lisp: NIL; Patch-File: T -*-
;;;; Patch directory for ~A version ~D
;"
;	       (PATCH-NAME PATCH-SYSTEM) (PATCH-VERSION PATCH-SYSTEM))
;       (WRITE-RESPONSIBILITY-COMMENT STREAM)
;       (SEND STREAM ':TYO #/()
;       (PRIN1 (PATCH-DIR-STATUS PATCH-DIR) STREAM)
;       (SEND STREAM ':STRING-OUT "
; (")
;       (DOLIST (PATCH (PATCH-DIR-VERSION-LIST PATCH-DIR))
;	 (PRIN1 PATCH STREAM)
;	 (SEND STREAM ':STRING-OUT "
;  "))
;       (SEND STREAM ':STRING-OUT "))"))))
;
;))

(setf (documentation 'si:print-system-modifications 'function)
  "Print descriptions of all loaded patches of the systems in SYSTEM-NAMES, or all systems.")

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro pushnew (value place &rest options)
  "Add ITEM to the front of the list PLACE, if it's not already MEMQ there.
Equivalent to (SETF PLACE (ADJOIN VALUE PLACE OPTIONS...))
but evaluates subforms of PLACE only once."
  (declare (arglist value place &key test test-not key))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method place)
    (let ((val (gensym)))
      (sublis-eval-once (cons `(,val . ,value) (pairlis tempvars tempargs))
			(if options
			    (sublis (list (cons (car storevars)
						`(adjoin ,val ,refform . ,options)))
				    storeform)
			  `(if (memq ,val ,refform)
			       ,refform
			     ,(sublis (list (cons (car storevars)
						  `(cons ,val ,refform)))
				      storeform)))
			t t))))

))

; From file GENRIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun adjoin (item list &key &optional test test-not key)
  "Return either LIST or (CONS ITEM LIST); the latter if no element of LIST matches ITEM.
KEY, if non-NIL, is a function applied ITEM and to each element of LIST
 to get the objects to match.  If KEY is NIL, ITEM and the element itself are used.
TEST is a function passed ITEM (or its key) and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (if (and (null key)
	   (or (eq test 'eq) (eq test #'eq)
	       (and (null test) (null test-not)
		    (or (not (numberp item)) (fixnump item)))))
      (progn (pushnew item list) list)
    (unless (member-1 (if key (funcall key item) item) list test test-not key)
      (push item list))
    list))

))

; From file SRCCOM.LISP PS:<L.IO1> OZ:
#8R SRCCOM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SRCCOM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SRCCOM  "

(DEFUN PRINT-AUTOMATIC-MERGE (FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1
			      FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (PRINT-FILE-SEGMENT FILE-1 *MERGE-LINE-NO* DIFF-LINE-NO-1)
  (WHEN *RECORD-MERGE-BOUNDS-P*
    (SETQ *MERGE-THIS-RECORD* NIL)
    (RECORD-MERGE-BOUND))
  (TERPRI *OUTPUT-STREAM*)
  (SEND *OUTPUT-STREAM* ':LINE-OUT "*** MERGE LOSSAGE ***")
  (PRINT-AUTOMATIC-MERGE-1 FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1)
  (PRINT-AUTOMATIC-MERGE-1 FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (SEND *OUTPUT-STREAM* ':LINE-OUT "*** END OF MERGE LOSSAGE ***")
  (WHEN *RECORD-MERGE-BOUNDS-P*
    (RECORD-MERGE-BOUND)
    (PUSH (NREVERSE *MERGE-THIS-RECORD*) *MERGE-RECORD*))
  (SETQ *MERGE-LINE-NO* SAME-LINE-NO-1))

))

; From file CHOICE.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; CHOICE  "

(DEFUN CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE (WINDOW MSG)
  ;; Returns T if message is "exit", else does variable-changing or special action
  ;; and returns NIL.  msg is either a list that came in whose cadr is
  ;; this window, or it is a regular character; only #\FORM is used.
  (PROG ()
    (COND ((CONSP MSG)
	   (SELECTQ (CAR MSG)
	     (:MOUSE-BUTTON ;Can get these, randomly.
	      )
	     (:CHOICE-BOX
	      (SETQ MSG (SIXTH (THIRD MSG)))	;NIL if done or form to eval
	      (IF (NULL MSG) (RETURN T) (EVAL MSG)))
	     (:VARIABLE-CHOICE
	      (APPLY #'CHOOSE-VARIABLE-VALUES-CHOICE (CDR MSG)))
	     (OTHERWISE (FERROR NIL "~S unknown message from ~S" MSG WINDOW))))
	  ((EQ MSG #\FORM) (FUNCALL WINDOW ':REFRESH)))))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (SETQ MORE-VPOS (AND (SEND SUPERIOR ':MORE-P) (SHEET-DEDUCE-MORE-VPOS SELF))))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :EXPOSE-FOR-TYPEOUT) ()
  ;; This is here so that we don't try to activate ourselves while we are locked,
  ;; so that we don't violate locking order, because activating requires getting
  ;; a lock on our superior
  (FUNCALL-SELF ':ACTIVATE)
  (FUNCALL-SELF ':EXPOSE NIL ':NOOP)
  (OR EXPOSED-P
      ;; If our superior has no screen array, we won't really be exposed.  So wait
      ;; until really exposed to prevent infinite regression
      (PROCESS-WAIT "Typeout Exposed" #'CAR (LOCF (SHEET-EXPOSED-P SELF))))
  (SETQ BOTTOM-REACHED (OR BOTTOM-REACHED 0)
	INCOMPLETE-P T)
  ;;On becoming exposed, also be the selection substitute for an appropriate ancestor.
  (AND (NEQ SELF SELECTED-WINDOW)
       (LET ((TEM (TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR SELF)))
	 (UNLESS (EQ (SEND TEM ':SELECTION-SUBSTITUTE) SELF)
	   (SETQ WINDOW-SUBSTITUTING-FOR TEM)
	   (SETQ PREVIOUS-SUBSTITUTE (SEND TEM ':SELECTION-SUBSTITUTE))
	   (SEND WINDOW-SUBSTITUTING-FOR ':SET-SELECTION-SUBSTITUTE SELF))))
  (FUNCALL-SELF ':HOME-CURSOR)
  (FUNCALL-SELF ':CLEAR-EOL))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :DEACTIVATE) ()
  (WHEN (EQ SELF (SEND WINDOW-SUBSTITUTING-FOR ':ULTIMATE-SELECTION-SUBSTITUTE))
    (SEND WINDOW-SUBSTITUTING-FOR ':REMOVE-SELECTION-SUBSTITUTE SELF
	  (IF (NEQ PREVIOUS-SUBSTITUTE SELF)
	      PREVIOUS-SUBSTITUTE
	    WINDOW-SUBSTITUTING-FOR))))

))

; From file DEFMIC.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DEFMIC  "

(DEFMIC FLOAT 650 (NUMBER OTHER) NIL T)

))


; From file DEFMIC.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DEFMIC  "

(DEFMIC %FLOAT-DOUBLE 652 (LOW HIGH) T)

))

; From file DOCMIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'LDB 'FUNCTION)
  "Load a byte specified by PPSS out of the number WORD.
PPSS is a number whose printed representation in octal, as four digits,
 contains a two-digit position-within-word and a two-digit size.
The position is the number of low bits of WORD after the desired byte.
The size must be less than 25. so that the value fits in a fixnum.")

))

; From file DOCMIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'DPB 'FUNCTION)
 "Deposit VALUE into the byte PPSS of the number WORD, returning a new number.
PPSS is a number whose printed representation in octal, as four digits,
 contains a two-digit position-within-word and a two-digit size.
The position is the number of low bits of WORD after the desired byte.
The size must be less than 25. so that the value fits in a fixnum.")

))

; From file DOCMIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION '%FLOAT-DOUBLE 'FUNCTION)
  "The fixnums HIGH and LOW are concatenated together to produce an
unsigned positive integer with twice as many bits as are used to represent pointers.
A flonum containing an approximation to that value is constructed and returned.")

))

; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYI) (&OPTIONAL IGNORE
							&AUX IDX (INHIBIT-SCHEDULING-FLAG T))
  (COND	((NOT RUBOUT-HANDLER)
	 (IF UNRCHF
	     (PROG1 UNRCHF (SETQ UNRCHF NIL))
	   (DO-FOREVER
	     (COLD-LOAD-STREAM-WAIT-FOR-CHAR)
	     (LET ((CHAR (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))
	       (SELECTQ CHAR
		 (NIL)				;Unreal character
		 (#\BREAK (BREAK "BREAK"))
		 ;; Horrible kludge to make the debugger usable in the cold-load stream
		 ;; How could this reasonably be done?
		 (#\ABORT (IF EH:READING-COMMAND (RETURN CHAR)
			    (SIGNAL EH:ABORT-OBJECT)))
		 (OTHERWISE (RETURN CHAR)))))))
	((> (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0)
	    (SETQ IDX (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1)))
	 (STORE-ARRAY-LEADER (1+ IDX) RUBOUT-HANDLER-BUFFER 1)
	 (AREF RUBOUT-HANDLER-BUFFER IDX))
	(T
	 (COLD-LOAD-STREAM-RUBOUT-HANDLER))))

))

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(FORWARD-VALUE-CELL '*TERMINAL-IO* 'TERMINAL-IO)

))
