;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.40
;;; Reason: type optimization of fixnum
;;; make random character functions into substs
;;; OPEN bug
;;; CLI:*DEFAULT-PATHNAME-DEFAULTS*
;;; copy-readtable rationalized
;;; define logical pathname methods for zmail
;;; printing time/date
;;; define setf method for SEND and LEXPR-SEND
;;; random mouse clicks in window-oriented-debugger! bug
;;; function-parent declaration for defstruct constructors
;;; printing symbols when *PRINT-ESCAPE* is nil still hacks *PRINT-CASE*
;;; Correct quoting for printing character objects, format:ochar ~C
;;; Very important fix for Disassociated Press bug
;;; Compiler optimization for CLI://
;;; Define a few additional canonical file types
;;; LOAD-PATCHES is really silent as requested, uses WITH-SYS-HOST-ACCESSIBLE
;;; Fix a whole bunch of string functions which didn't know about character objects
;;; :NIL when parsing file attribute lists
;;; :NIL when reading values for Zmacs variables
;;; TV:TRACE-VIA-MENUS argument-reading bug
;;; (EVAL-WHEN (CLI:EVAL ...) ...) Sigh.
;;; FORMAT "~D" binds *PRINT-RADIX* to NIL
;;; Debugger prints out missing argument slots
;;; Make Common-Lisp stream input operations know that NIL => *STANDARD-INPUT*
;;; READ-LINE arguments.
;;; Light at the end of the rubout-handling tunnel!!! Recursive rubout-handling now works.
;;; Bugs in complex arithmetic
;;; Written 4-Mar-84 05:22:42 by Mly,
;;; while running on Lisp Machine Two from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.37, CADR 3.6, ZMail 53.12, MIT-Specific 22.0, microcode 306, gc@36.

(eval-when (load compile eval)

(unless ( %microcode-version-number 309.)
  (ferror nil "You ~must have microcode 309 loaded to use this patch.
Use SI:RECEIVE-BAND to copy an appropriate microcode band
  from a machine which already has it, then
/(SI:SET-CURRENT-MICROLOAD <the band which contains 309.>)
Cold boot the machine, and now you will be able to load-patches
happily and un-ferrored.~"))
 )

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defmacro with-input-editing ((stream rubout-options . brand-s-compatibility-args) &body body)
  "Execute BODY inside of STREAM's :RUBOUT-HANDLER method.
If BODY does input from STREAM, it will be done with rubout processing
if STREAM implements any.
RUBOUT-OPTIONS should be the options for the :RUBOUT-HANDLER message, such as 
 (:NO-INPUT-SAVE T)   -- don't save this batch of input in the history.
 (:FULL-RUBOUT flag)  -- return from this construct if rubout buffer becomes empty
	with two values: NIL and flag.
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
        are specified, :PROMPT is used on initial entry and :REPROMPT thereafter.
 (:NONRECURSIVE T)
	Means to ignore previously-specified rubout-handler options and only use the
	options specified to this call to WITH-INPUT-EDITING."
  (let ((keyword (cadr brand-s-compatibility-args)))
    (setq stream (selectq stream
		   ((nil) '*standard-input*)
		   ((t) '*terminal-io*)
		   (t stream)))
    `(flet ((do-it () . ,body))
       (if (send ,stream :operation-handled-p :rubout-handler)
	   ,(if keyword
		`(with-stack-list* (options
				     ',(selectq keyword
					 (:end-activation '(:activation = #/end))
					 ((:line :line-activation)
					  '(:activation memq (#/end #/return))))
				     ,rubout-options)
		   (send ,stream ':rubout-handler options #'do-it))
	      `(send ,stream ':rubout-handler ,rubout-options #'do-it))
	 (let ((rubout-handler nil))
	   (do-it))))))

))

(without-interrupts
  (dolist (pkg *all-packages*)
    (when (eq (nth-value 2 (intern-soft "RUBOUT-HANDLER-OPTIONS" pkg)) pkg)
      (let ((sym (intern "RUBOUT-HANDLER-OPTIONS" pkg)))
	(setf (plist sym) nil)
	(setf (symbol-value sym) nil)
	(fmakunbound sym))))
  (globalize "RUBOUT-HANDLER-OPTIONS"))


; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFVAR RUBOUT-HANDLER-OPTIONS NIL
  "The options supplied as first arg to :RUBOUT-HANDLER operation or to WITH-INPUT-EDITING.")


))

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFSTRUCT (RUBOUT-HANDLER-BUFFER (:TYPE :ARRAY-LEADER)
				  (:MAKE-ARRAY (:LENGTH 1000))
				  (:DEFAULT-POINTER RUBOUT-HANDLER-BUFFER)
				  (:CONC-NAME "RHB-")
				  (:ALTERANT NIL))
  (FILL-POINTER 0)
  (SCAN-POINTER 0)
  (TYPEIN-POINTER 0)
  (DONT-SAVE-FLAG NIL)  ;T means this input should not go on the input ring.
  (INPUT-RING NIL)
  (STATUS NIL))		;nil, :restored, :initial-entry, :rubout, ...

))

(set-in-instance tv:cold-load-stream 'si:rubout-handler-buffer
		 (MAKE-ARRAY #o1000 ':TYPE ART-STRING ':LEADER-LIST '(0 0 NIL)))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (fixnum type-expander) ()
  `(integer ,most-negative-fixnum ,most-positive-fixnum))

(defun (fixnum type-optimizer) (expression)
  `(fixnump ,(cadr expression)))

))

(remprop ':list 'si:subtypes)
(remprop ':list 'si:type-predicate)
(putprop ':list 'cons 'si:type-alias-for)

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun typep-two-args (form &aux opt type pred dtp)
  (cond ((and (caddr form)
	      (or (compiler:quotep (caddr form)) (keywordp (caddr form))))
	 (setq type (if (consp (caddr form))
			(cadr (caddr form))	;(typep foo ':list)
		      (caddr form)))		;(typep foo :list)
	 (flet ((frob (type)
		   (if (and (symbolp type)
			    (setq opt (get type 'type-optimizer)))
		       (funcall opt form)
		     (if (and (symbolp type)
			      (setq opt (get type 'type-alias-for)))
			 `(typep ,(cadr form) ',opt)
		       (cond ((symbolp type)
			      (cond ((setq opt (get type 'type-optimizer))
				     (funcall opt form))
				    ((and (setq pred (get type 'type-predicate))
					  (symbolp pred))
				     `(,pred ,(cadr form)))
				    ((setq dtp (or (rassq type type-of-alist)
						   (rassq type typep-one-arg-alist)))
				     `(= (%data-type ,(cadr form)) ,(car dtp)))
				    ((get type 'si:flavor)
				     `(typep-structure-or-flavor . ,(cdr form)))
				    ((get type 'si:defstruct-description)
				     `(typep-structure-or-flavor . ,(cdr form)))
				    ((class-symbolp type)
				     `(subinstance-of-class-symbol-p ,(cadr form) ',type))
				    (t form)))
			     (t
			      (cond ((setq opt (get (car type) 'type-optimizer))
				     (apply opt form (cdr type)))
				    ((symbolp (setq pred (get (car type) 'type-predicate)))
				     `(,pred ,(cadr form)))
				    (t form))))))))
	   (let ((tem (frob type)))
	     (if (neq tem form)
		 tem
	       (frob (type-canonicalize type))))))
	(t form)))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFSUBST FIXNUM-ARRAYP (OBJECT)
  "T if OBJECT is an array whose type requires all elements to be fixnums."
  (AND (ARRAYP OBJECT)
       (ARRAY-BITS-PER-ELEMENT (%P-LDB %%ARRAY-TYPE-FIELD OBJECT))))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(defsubst graphic-char-p (char)
  "T if CHAR is a graphic character, one which prints as a single glyph.
Things like #\RETURN and #\RESUME and #\CONTROL-A are not graphic."
  ( 0 char 177))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(defsubst string-char-p (char)
  "T if CHAR is a character which ordinary strings can contain.
Note that ART-FAT-STRING arrays can contain additional characters,
for which this function nevertheless returns NIL."
  ( 0 char 377))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(defsubst fat-string-char-p (char)
  "T if CHAR is a charater which a fat string can contain."
  ( 0 char 177777))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst alpha-char-p (char)
  "T if CHAR is alphabetic with no meta bits."
  (and (zerop (ldb %%kbd-control-meta char))
       (or ( #/A (ldb %%ch-char char) #/Z)
	   ( #/a (ldb %%ch-char char) #/z))))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst upper-case-p (char)
  "T if CHAR is an upper case letter with no meta bits."
  (and (zerop (ldb %%kbd-control-meta char))
       ( #/A (ldb %%ch-char char) #/Z)))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst lower-case-p (char)
  "T if CHAR is an upper case letter with no meta bits."
  (and (zerop (ldb %%kbd-control-meta char))
       ( #/a (ldb %%ch-char char) #/z)))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst both-case-p (char)
  "T if CHAR is a character which has upper and lower case forms, with no meta bits.
This is just letters."
  (and (zerop (ldb %%kbd-control-meta char))
       (or ( #/A (ldb %%ch-char char) #/Z)
	   ( #/a (ldb %%ch-char char) #/z))))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst alphanumericp (char)
  "T if CHAR is a letter or digit, with no meta bits."
  (and (zerop (ldb %%kbd-control-meta char))
       (or ( #/0 (ldb %%ch-char char) #/9)
	   ( #/A (ldb %%ch-char char) #/Z)
	   ( #/a (ldb %%ch-char char) #/z))))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst char-code (char)
  "Returns the character code of the character CHAR.
This is sans the font number and meta bits."
  (ldb %%ch-char char))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst char-font (char)
  "Returns the font number of character CHAR."
  (ldb %%ch-font char))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst char-bits (char)
  "Returns the special bits of the character CHAR."
  (ldb %%kbd-control-meta char))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst code-char (code &optional (bits 0) (font 0))
  "Returns a character whose code comes from CODE, bits from BITS and font from FONT.
CODE can be a number or a character.
NIL is returned if it is not possible to have a character object
with the specified FONT and BITS."
  (%make-pointer dtp-character
		 (%logdpb bits %%kbd-control-meta
			  (dpb font %%ch-font code))))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN INT-CHAR (INTEGER)
  "Returns a character whose value corresponds to INTEGER."
  (%MAKE-POINTER DTP-CHARACTER INTEGER))

))

; From file CHARACTER.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun char-int (char)
  "Returns an integer whose value corresponds to CHAR.
On the Lisp machine, this conversion will happen automatically
in most places that an integer can be used."
  (dont-optimize (%pointer char)))

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(defvar *default-date-print-mode* :unbound
  "Defines the default way to print the date. Possible values include:
:DD//MM//YY :MM//DD//YY :DD-MM-YY :DD-MMM-YY :|DD MMM YY| :DDMMMYY :YYMMDD :YYMMMDD")
(forward-value-cell '*default-date-print-mode* 'default-date-print-mode)
))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defsubst %complex-cons (realpart imagpart)
  (let ((object
	  (%allocate-and-initialize dtp-extended-number dtp-header
				    (dpb %header-type-complex
					 %%header-type-field
					 0)
				    0 number-cons-area 3)))
    (setf (complex-real-part object) (+ realpart (* 0 imagpart)))
    (setf (complex-imag-part object) (+ imagpart (* 0 realpart)))
    object))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN OPEN (FILENAME &REST KEYWORD-ARGS)
  "Open a file and return a stream.  FILENAME is a pathname or a string.
DIRECTION is :INPUT, :OUTPUT or NIL for non-data stream.
ERROR if NIL says return an error object rather than getting an error.
CHARACTERS says whether to transfer character data; :DEFAULT means do what the file says.
BYTE-SIZE defaults according to CHARACTERS; :DEFAULT says use file's byte size.
NEW-FILE if non-nil says creating a file is ok; defaults T if output stream
NEW-VERSION says version :NEWEST creates a new version; defaults to NEW-FILE.
OLD-FILE says what to do with existing file; possibilities are
 :ERROR, :REWRITE (or T), :APPEND, :REPLACE (or NIL), :RENAME,
 :RENAME-AND-DELETE, :NEW-VERSION.  Default is (NOT NEW-FILE).
FLAVOR is NIL, :DIRECTORY, :LINK or file-system dependent values.
LINK-TO specifies link target, when you create a file with flavor :LINK.
INHIBIT-LINKS says don't chase a link; open the link itself.
DELETED says it is ok to open deleted but not expunged files.
PRESERVE-DATES says do not alter the files read or write dates.
Other system-specific keywords may be supported for some file systems."
  (DECLARE (ARGLIST FILENAME &KEY &OPTIONAL (DIRECTION ':INPUT) (ERROR T)
		    (CHARACTERS T) BYTE-SIZE NEW-FILE NEW-VERSION OLD-FILE 
		    FLAVOR LINK-TO INHIBIT-LINKS DELETED PRESERVE-DATES
		    &ALLOW-OTHER-KEYS))
  (FORCE-USER-TO-LOGIN)
  (IF (STREAMP FILENAME)
      (SETQ FILENAME (SEND FILENAME ':PATHNAME)))
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME))
  (SETQ LAST-FILE-OPENED FILENAME)
  (IF (OR (NULL KEYWORD-ARGS)			;No args is good args
	  (NOT (NULL (CDR KEYWORD-ARGS))))
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ (GET (LOCF KEYWORD-ARGS) ':ERROR) '(:RETRY :REPROMPT))
				  (FILENAME FILE-ERROR)
        (LEXPR-FUNCALL FILENAME ':OPEN FILENAME KEYWORD-ARGS))
    ;; Old Syntax.
    (DO ((KEYL (IF (AND (CAR KEYWORD-ARGS) (SYMBOLP (CAR KEYWORD-ARGS)))
		   (LIST (CAR KEYWORD-ARGS))
		 (CAR KEYWORD-ARGS))
	       (CDR KEYL))
	 (KEY)
	 (CHARACTERS T)
	 (DIRECTION ':INPUT)
	 (BYTE-SIZE NIL)
	 (ERROR-P T)
	 (ERROR-P-SPECD NIL)
	 (DELETED-P NIL)
	 (TEMPORARY-P NIL)
	 ;; These two are really only useful for machines that do not natively store
	 ;; 8-bit characters.
	 (RAW-P NIL)
	 (SUPER-IMAGE-P NIL)
	 )
	((NULL KEYL)
	 (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR-P '(:RETRY :REPROMPT))
				     (FILENAME FILE-ERROR)
	   ;; Because we don't want to send meaningless keywords to file systems
	   ;; which don't support them, and we don't want to cons...
	   (%ASSURE-PDL-ROOM 19.)			;Worst case
	   (%OPEN-CALL-BLOCK FILENAME 0 4)	;D-RETURN
	   (%PUSH ':OPEN)       (%PUSH FILENAME)
	   (%PUSH ':CHARACTERS) (%PUSH CHARACTERS)
	   (%PUSH ':DIRECTION)  (%PUSH DIRECTION)
	   (COND (BYTE-SIZE     (%PUSH ':BYTE-SIZE)   (%PUSH BYTE-SIZE)))
	   (COND (ERROR-P-SPECD (%PUSH ':ERROR)       (%PUSH ERROR-P)))
	   (COND (DELETED-P     (%PUSH ':DELETED)     (%PUSH DELETED-P)))
	   (COND (TEMPORARY-P   (%PUSH ':TEMPORARY)   (%PUSH TEMPORARY-P)))
	   (COND (SUPER-IMAGE-P (%PUSH ':SUPER-IMAGE) (%PUSH SUPER-IMAGE-P)))
	   (COND (RAW-P	      (%PUSH ':RAW)	    (%PUSH RAW-P)))
	   (%ACTIVATE-OPEN-CALL-BLOCK)))
      (SETQ KEY (CAR KEYL))
      (SELECTOR KEY STRING-EQUAL
	((':IN ':READ) (SETQ DIRECTION ':INPUT))
	((':OUT ':WRITE ':PRINT) (SETQ DIRECTION ':OUTPUT))
	((':BINARY ':FIXNUM) (SETQ CHARACTERS NIL))
	((':CHARACTER ':ASCII) (SETQ CHARACTERS T))
	((':BYTE-SIZE) (SETQ KEYL (CDR KEYL)
			     BYTE-SIZE (CAR KEYL)))
	((':PROBE) (SETQ DIRECTION NIL
			 CHARACTERS NIL
			 ERROR-P-SPECD T
			 ERROR-P NIL))
	((':NOERROR) (SETQ ERROR-P NIL ERROR-P-SPECD T))
	((':ERROR) (SETQ ERROR-P T ERROR-P-SPECD T))
	((':RAW) (SETQ RAW-P T))
	((':SUPER-IMAGE) (SETQ SUPER-IMAGE-P T))
	((':DELETED) (SETQ DELETED-P T))
	((':TEMPORARY) (SETQ TEMPORARY-P T))
	((':BLOCK ':SINGLE) )			;Ignored for compatility with Maclisp
	(OTHERWISE (FERROR NIL "~S is not a known OPEN option" KEY))))))

))

(EVAL-WHEN (COMPILE LOAD EVAL)
  (intern "*DEFAULT-PATHNAME-DEFAULTS*" "CLI")
  (intern "ATAN" "CLI"))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFVAR CLI:*DEFAULT-PATHNAME-DEFAULTS* :UNBOUND
  "These are the defaults pathname defaults as far as Common Lisp programs know them.
The value of this variable is a pathname.
The value cell is kludgily shared with a cell of the alist
stored in GLOBAL:*DEFAULT-PATHNAME-DEFAULTS*.")

(SETQ CLI:*DEFAULT-PATHNAME-DEFAULTS*
      (SI:CDR-LOCATION-FORCE (ASSQ NIL *DEFAULT-PATHNAME-DEFAULTS*)))
(%P-STORE-DATA-TYPE (LOCF CLI:*DEFAULT-PATHNAME-DEFAULTS*) DTP-EXTERNAL-VALUE-CELL-POINTER)

))


; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFVAR *COMMON-LISP-SYMBOL-SUBSTITUTIONS*
	(COPYTREE
	  '((// . CLI://) 
	    (*DEFAULT-PATHNAME-DEFAULTS* . CLI:*DEFAULT-PATHNAME-DEFAULTS*)
	    (AR-1 . CLI:AR-1)
	    (AR-1-FORCE . CLI:AR-1-FORCE)
	    (AREF . CLI:AREF)
	    (ASSOC . CLI:ASSOC)
	    (ATAN . CLI:ATAN)
	    (CATCH . CLI:CATCH)
	    (CHARACTER . CLI:CHARACTER)
	    (CLOSE . CLI:CLOSE)
	    (DEFSTRUCT . CLI:DEFSTRUCT)
	    (DELETE . CLI:DELETE)
	    (ERROR . CLI:ERROR)
	    (EVAL . CLI:EVAL)
	    (EVERY . CLI:EVERY)
;The entry for FORMAT should be removed for system 99.
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

; From file READ.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN COPY-READTABLE (&OPTIONAL (A-READTABLE *READTABLE*) (ANOTHER-READTABLE NIL))
  "Copy a readtable into another readtable, or make a new readtable.
With two arguments, copies the first into the second.
Otherwise makes a new copy of the first readtable.
The first argument defaults to the current readtable.
If the first argument is explicitly supplied as NIL,
 a copy of the unmodified standard Common Lisp syntax is made.."
  (IF (NULL A-READTABLE) (SETQ A-READTABLE COMMON-LISP-READTABLE))
  (LET ((X (ARRAY-DIMENSION A-READTABLE 0))
	(Y (ARRAY-DIMENSION A-READTABLE 1))
	(L (ARRAY-LEADER-LENGTH A-READTABLE)))
    (LET ((NEW-READTABLE (OR ANOTHER-READTABLE
			     (MAKE-ARRAY (LIST X Y)
					 ':TYPE 'ART-16B
					 ':LEADER-LENGTH L))))
      (DOTIMES (I X)
	(DOTIMES (J Y)
	  (SETF (AREF NEW-READTABLE I J) (AREF A-READTABLE I J))))
      (DOTIMES (I L)
	(SETF (ARRAY-LEADER NEW-READTABLE I) (ARRAY-LEADER A-READTABLE I)))
      ;; Certain elements of the leader should not be shared.
      (SETF (RDTBL-MACRO-ALIST NEW-READTABLE)
	    (COPYTREE (RDTBL-MACRO-ALIST NEW-READTABLE)))
      (SETF (RDTBL-PLIST NEW-READTABLE)
	    (COPYLIST (RDTBL-PLIST NEW-READTABLE)))
      (AND (NAMED-STRUCTURE-P A-READTABLE)
	   (MAKE-ARRAY-INTO-NAMED-STRUCTURE NEW-READTABLE))
      NEW-READTABLE)))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (LOGICAL-PATHNAME :MAIL-FILE-FORMAT-COMPUTER) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :INBOX-BUFFER-FLAVOR) LOGICAL-PATHNAME-PASS-ON)

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFUN LOGICAL-HOST-PASS-ON (&REST REST)
  (DECLARE (:SELF-FLAVOR LOGICAL-HOST))
  (APPLY HOST REST))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (LOGICAL-HOST :GMSGS-PATHNAME) LOGICAL-HOST-PASS-ON)

))

(setf (documentation '*read-base* 'variable)
  "Default radix for reading integers.")

(setf (documentation '*print-radix* 'variable)
  "Non-NIL means print a radix specifier when printing an integer.")


; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFUN PRINT-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
		   &OPTIONAL (STREAM STANDARD-OUTPUT)
			     (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*)) 
  "Print time specified on STREAM using date format DATE-PRINT-MODE.
If STREAM is NIL, construct and return a string."
  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH ':SHORT)
				       NIL (MOD YEAR 100.))
    (FORMAT STREAM "~? ~2,'0D:~2,'0D:~2,'0D"
	    (OR (GET DATE-PRINT-MODE 'DATE-FORMAT)
		(FERROR NIL "Bad value of DATE-PRINT-MODE: ~s" DATE-PRINT-MODE))
	    DATE-MODE-ARGS
	    HOURS MINUTES SECONDS)))

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFUN PRINT-DATE (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
		   &OPTIONAL (STREAM STANDARD-OUTPUT))
  "Print the date and time in verbose form on STREAM.
If STREAM is NIL, construct and return a string."
  (SETQ MONTH (MONTH-STRING MONTH)
	DAY-OF-THE-WEEK (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))
  (FORMAT STREAM
	  "~A the ~:R of ~A, ~D/; ~D:~2,'0D:~2,'0D ~A"
	  DAY-OF-THE-WEEK DAY MONTH YEAR (1+ (\ (+ HOURS 11.) 12.)) MINUTES SECONDS
	  (COND ((AND (ZEROP SECONDS)
		      (ZEROP MINUTES)
		      (MEMQ HOURS '(0 12.)))
		 (IF (= HOURS 0) "midnight" "noon"))
		(( HOURS 12.) "pm")
		(T "am"))))

))

; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFUN PRINT-BRIEF-UNIVERSAL-TIME (UT &OPTIONAL (STREAM STANDARD-OUTPUT)
						(REF-UT (GET-UNIVERSAL-TIME))
						(DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE REF-DAY REF-MONTH REF-YEAR)
	(DECODE-UNIVERSAL-TIME REF-UT)
      ;; If not same day, print month and day numerically
      (IF (OR ( DAY REF-DAY) ( MONTH REF-MONTH) ( YEAR REF-YEAR))
	  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH ':SHORT)
					   (= YEAR REF-YEAR) (MOD YEAR 100.))
	    (FORMAT STREAM "~? ~2,'0D:~2,'0D"
		    (OR (GET DATE-PRINT-MODE 'DATE-FORMAT)
			(FERROR NIL "Bad date-print-mode: ~s" DATE-PRINT-MODE))
		    DATE-MODE-ARGS
		    HOURS MINUTES))
	;; Always print hours colon minutes, even if same as now
	(FORMAT STREAM "~2,'0D:~2,'0D" HOURS MINUTES)))))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defmacro defstruct-make-empty () `'%%defstruct-empty%%)

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defmacro defstruct-emptyp (x) `(eq ,x '%%defstruct-empty%%))

))

; From file EHW.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHW  "

(DEFUN PROCESS-SPECIAL-COMMAND (LIST SG ERROR-OBJECT &AUX OPERATION VALUE WINDOW)
  (SETQ OPERATION (FIRST LIST)
	VALUE (SECOND LIST)
	WINDOW (THIRD LIST))
  (COND ((EQ OPERATION ':MENU)
	 (SETQ WINDOW (FOURTH LIST)
	       VALUE (SEND WINDOW ':EXECUTE VALUE))))
  (AND (NEQ OPERATION ':MOUSE-BUTTON)
       (SEND ERROR-HANDLER-WINDOW ':INSPECT-WINDOW-P WINDOW)
       (IF (= (FOURTH LIST) #/MOUSE-1-1)
	   (SETQ OPERATION ':INSPECT)
	 (SETQ OPERATION ':VALUE
		 VALUE (TV:INSPECT-REAL-VALUE LIST))))
  (COND ((AND (EQ OPERATION ':MENU) (MEMQ VALUE '(T NIL-VALUE)))
	 (SEND *STANDARD-OUTPUT* ':LINE-OUT (IF (SETQ VALUE (EQ VALUE T)) "T" "()"))
	 (SETQ OPERATION ':VALUE + VALUE)))
  (COND ((EQ OPERATION ':LINE-AREA)
	 (SETQ CURRENT-FRAME VALUE)
	 (SEND ERROR-HANDLER-WINDOW ':SETUP-FRAME SG CURRENT-FRAME))
	((EQ OPERATION ':MENU)
	 (SEND VALUE SG ERROR-OBJECT))		;Execute a regular menu command
	((EQ OPERATION ':INSPECT)
	 (SEND ERROR-HANDLER-WINDOW ':INSPECT-OBJECT (TV:INSPECT-REAL-VALUE LIST)))
	((MEMQ OPERATION '(:VALUE :FUNCTION STACK-FRAME SPECIAL ARG LOCAL))
	 (SETQ +++ ++ ++ +)
	 (COND ((MEMQ OPERATION '(SPECIAL ARG LOCAL))
		(COND ((MEMQ OPERATION '(ARG LOCAL))
		       (PRIN1 (FIRST VALUE))
		       (LET ((IDX (SECOND VALUE)))
			 (IF (NOT (NUMBERP IDX))
			     (AND (EQUALP IDX "Rest arg")
				  (SETQ VALUE (SG-REST-ARG-VALUE SG CURRENT-FRAME)))
			     (LET ((RP (SG-REGULAR-PDL SG)))
			       (SETQ + (ALOC RP
					     (+ CURRENT-FRAME IDX
						(IF (EQ OPERATION 'ARG) 1
						    (RP-LOCAL-BLOCK-ORIGIN
						      RP CURRENT-FRAME))))))
			     (SETQ VALUE (CAR +)))))
		      (T
		       (SETQ + (PRIN1 VALUE))
		       (SETQ VALUE (SYMEVAL VALUE))))
		(TERPRI))
	       ((EQ OPERATION 'STACK-FRAME)
		(SETQ VALUE (STACK-FRAME-INTO-LIST VALUE SG))))
	 (SETQ *** ** ** * * (PRIN1 VALUE)))
	(T
	 (TV:BEEP))))

))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
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
      ;;; callable, commonlisp-style constructors
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
			 (SETQ INIT (DEFSTRUCT-MAKE-EMPTY)))
		       (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (IF (DEFSTRUCT-EMPTYP INIT) (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
		       (IF (DEFSTRUCT-EMPTYP INIT)
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
			 (SETQ INIT (DEFSTRUCT-MAKE-EMPTY)))
		       (OR (SETQ S (CDR (ASSOC X SLOT-ALIST))) (GO SLOT-ERROR))
		       (IF (DEFSTRUCT-EMPTYP INIT) (SETQ INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S)))
		       (IF (DEFSTRUCT-EMPTYP INIT) NIL
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
		  (DEFSTRUCT-ERROR "Invalid DEFSTRUCT slot-name" X 'WHILE 'DEFINING MNAME)))
	    ;;do this for non-boa-constructors
	    (SETQ BOAP NIL)
	    (PUSH '&KEY ARGLIST)
	    (PUSH '&KEY ARGS)
	    (DOLIST (S SLOT-ALIST)
	      (SETQ X (CAR S) S (CDR S)		;standardize our nomenclature
		    INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE S))
	      (IF (DEFSTRUCT-EMPTYP INIT)
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
		 (DECLARE (ARGLIST ,ARGLIST)
			  (FUNCTION-PARENT ,MNAME DEFSTRUCT))
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

; From file PRINT.LISP PS:<MLY.L> OZ:
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
		     (PTTBL-UNINTERNED-SYMBOL-PREFIX *READTABLE*)))
	(MULTIPLE-VALUE-BIND (PKG-OR-STRING INTERNAL-FLAG)
	    (PKG-PRINTING-PREFIX SYMBOL *PACKAGE*)
	  (MULTIPLE-VALUE (PKG-OR-STRING INTERNAL-FLAG SYMBOL)
	    (LET* ((TEM1 (CAR (RASSQ SYMBOL *READER-SYMBOL-SUBSTITUTIONS*)))
		   (TEM2 (UNLESS TEM1 (CDR (ASSQ SYMBOL *READER-SYMBOL-SUBSTITUTIONS*))))
		   POS IF)
	      (COND ((AND TEM1 (MEMBER (MULTIPLE-VALUE (POS IF)
					 (PKG-PRINTING-PREFIX TEM1 *PACKAGE*))
				       '(NIL "")))
		     (VALUES POS IF TEM1))
		    (TEM2 (MULTIPLE-VALUE (NIL IF POS)
			    (INTERN TEM2 *PACKAGE*))
			  (VALUES POS (EQ IF ':INTERNAL) TEM2))
		    (T (VALUES PKG-OR-STRING INTERNAL-FLAG SYMBOL)))))
	  (WHEN PKG-OR-STRING
	    (UNLESS (EQUAL PKG-OR-STRING "")
	      (PRINT-PNAME-STRING (IF (STRINGP PKG-OR-STRING) PKG-OR-STRING
				    (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING))
				  STREAM FASTP))
	    (SEND STREAM ':STRING-OUT
		  (IF (AND (NOT (STRINGP PKG-OR-STRING))
			   *PACKAGE*
			   (ASSOC (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING)
				  (DONT-OPTIMIZE (PKG-REFNAME-ALIST PACKAGE))))
		      ;; Use #: to inhibit an interfering local nickname.
		      "#:"
		    (IF INTERNAL-FLAG
			(PTTBL-PACKAGE-INTERNAL-PREFIX *READTABLE*)
		      (PTTBL-PACKAGE-PREFIX *READTABLE*))))))))
    (SETQ STRING (STRING SYMBOL))
    (IF (NOT *PRINT-ESCAPE*)
	(SELECTQ *PRINT-CASE*
	  (:DOWNCASE (DOTIMES (I (LENGTH STRING))
		       (SEND STREAM ':TYO (CHAR-DOWNCASE (AREF STRING I)))))
	  (:CAPITALIZE (DO ((LENGTH (LENGTH STRING)) CHAR PREV-LETTER
			    (I 0 (1+ I)))
			   ((= I LENGTH))
			 (SETQ CHAR (AREF STRING I))
			 (COND (( #/A CHAR #/Z)
				(SEND STREAM ':TYO (IF PREV-LETTER (CHAR-DOWNCASE CHAR) CHAR))
				(SETQ PREV-LETTER T))
			       (( #/a CHAR #/z)
				(SEND STREAM ':TYO (IF PREV-LETTER CHAR (CHAR-UPCASE CHAR)))
				(SETQ PREV-LETTER T))
				  (( #/0 CHAR #/9)
				   (SEND STREAM ':TYO CHAR)
				   (SETQ PREV-LETTER T))
				  (T (SEND STREAM ':TYO CHAR)
				     (SETQ PREV-LETTER NIL)))))
	  (T (PRINT-RAW-STRING STRING STREAM FASTP)))
      (SETQ FSMWINS
	    (AND (PLUSP (SETQ LEN (LENGTH STRING)))
		 (DO ((I 0 (1+ I))
		      (STATE (RDTBL-STARTING-STATE *READTABLE*))
		      (FSM (RDTBL-FSM *READTABLE*))
		      (CHAR)
		      (ESCAPE-CODE (RDTBL-ESCAPE-CODE *READTABLE*))
		      (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*))
		      (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*)))
		     ((= I LEN)
		      (COND ((NOT (NUMBERP STATE))
			     (DO L (RDTBL-MAKE-SYMBOL *READTABLE*) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    ((NOT (NUMBERP (SETQ STATE
						 (AREF FSM
						       STATE
						       (RDTBL-BREAK-CODE *READTABLE*)))))
			     (DO L (RDTBL-MAKE-SYMBOL-BUT-LAST *READTABLE*) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    (T NIL)))
		   (SETQ CHAR (AREF STRING I))
		   (COND ((OR (NOT (NUMBERP STATE))	;FSM ran out OR
			      (NOT			;Translated char? then fsm loses
				(= CHAR (RDTBL-TRANS *READTABLE* CHAR))))
			  (OR MUST//				   ;Must we slash?
			      (DO ((I I (1+ I))) ((= I LEN))
				(LET ((CODE (RDTBL-CODE *READTABLE* (AREF STRING I))))
				    (WHEN (OR (= CODE ESCAPE-CODE)
					      (= CODE MULTIPLE-ESCAPE-CODE)
					      (= CODE CHARACTER-CODE-ESCAPE-CODE))
				      (SETQ MUST// T)
				      (RETURN NIL)))))
			  (RETURN NIL)))
		   (SETQ STATE
			 (AREF FSM
			       STATE
			       (COND ((LET ((CODE (RDTBL-CODE *READTABLE* (AREF STRING I))))
					(OR (= CODE ESCAPE-CODE)
					    (= CODE MULTIPLE-ESCAPE-CODE)
					    (= CODE CHARACTER-CODE-ESCAPE-CODE)))
				      (SETQ MUST// T)
				      (RDTBL-SLASH-CODE *READTABLE*))
				     ((AND (NUMBERP *PRINT-BASE*) (> *PRINT-BASE* 10.)
					   ( #\A CHAR (+ *PRINT-BASE* #\A -11.)))
				      (CDR (GETF (RDTBL-PLIST *READTABLE*) 'EXTENDED-DIGIT)))
				     (T
				      (RDTBL-CODE *READTABLE* CHAR))))))))
      (UNLESS FSMWINS (SEND STREAM ':TYO (PTTBL-OPEN-QUOTE-SYMBOL *READTABLE*)))
      (COND ((OR MUST//
		 (AND FSMWINS (NEQ *PRINT-CASE* ':UPCASE)))
	     (DO ((I 0 (1+ I))
		  (ESCAPE-CODE (RDTBL-ESCAPE-CODE *READTABLE*))
		  (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*))
		  (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*))
		  (PREV-CHAR 0)
		  CODE)
		 ((= I LEN))
	       (SETQ TEM (AREF STRING I))
	       (SETQ CODE (RDTBL-CODE *READTABLE* TEM))
	       (COND ((OR (= CODE ESCAPE-CODE)
			  (= CODE MULTIPLE-ESCAPE-CODE)
			  (= CODE CHARACTER-CODE-ESCAPE-CODE))
		      (SEND STREAM ':TYO (PTTBL-SLASH *READTABLE*))
		      (SEND STREAM ':TYO TEM))
		     ((OR (EQ *PRINT-CASE* ':DOWNCASE)
			  (AND (EQ *PRINT-CASE* ':CAPITALIZE)
			       (ALPHANUMERICP PREV-CHAR)))
		      (SEND STREAM ':TYO (CHAR-DOWNCASE TEM)))
		     (T
		      (SEND STREAM ':TYO TEM)))
	       (SETQ PREV-CHAR TEM)))
	    (T (PRINT-RAW-STRING STRING STREAM FASTP)))
      (UNLESS FSMWINS (SEND STREAM ':TYO (PTTBL-CLOSE-QUOTE-SYMBOL *READTABLE*)))
      ))

))

; From file PRINT.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(defun character-needs-quoting-p (char &optional (rdtbl *readtable*))
  "Returns T if CHAR needs to be quoted to be read in as a symbol using readtable RDTBL."
  (let ((state (rdtbl-starting-state rdtbl))
	(fsm (rdtbl-fsm rdtbl))
	(code (rdtbl-code rdtbl char)))
    (if (or (not (numberp state))		;FSM ran out OR
	    ( char (rdtbl-trans rdtbl char))	;Translated char? then fsm loses
	    (= code (rdtbl-escape-code rdtbl))
	    (= code (rdtbl-multiple-escape-code rdtbl))
	    (= code (rdtbl-character-code-escape-code rdtbl)))
	t
      (setq state (aref fsm state (if (and (numberp *print-base*) (> *print-base* 10.)
					   ( #/A char (+ *print-base* (- #/A 11.))))
				      (cdr (getf (rdtbl-plist rdtbl) 'extended-digit))
				    code)))
      (cond ((not (numberp state))
	     (dolist (l (rdtbl-make-symbol rdtbl) t)
	       (and (eq (car state) (car l))
		    (eq (cdr state) (cdr l))
		    (return nil))))
	    ((not (numberp (setq state (aref fsm state (rdtbl-break-code rdtbl)))))
	     (dolist (l (rdtbl-make-symbol-but-last rdtbl) t)
	       (and (eq (car state) (car l))
		    (eq (cdr state) (cdr l))
		    (return nil))))
	    (t t)))))

))

; From file PRINT.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM
		     &OPTIONAL
		     (WHICH-OPERATIONS (WHICH-OPERATIONS-FOR-PRINT STREAM))
		     &AUX NSS (FASTP (MEMQ ':STRING-OUT WHICH-OPERATIONS)))
  (CATCH-CONTINUATION-IF T 'PRINT-OBJECT
      #'(LAMBDA () (FORMAT STREAM "...error printing ")
		(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP :FASTP FASTP))
		(FORMAT STREAM "..."))
      NIL
    (CONDITION-RESUME '((ERROR) :ABORT-PRINTING T ("Give up trying to print this object.")
			CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
      (OR (AND (MEMQ ':PRINT WHICH-OPERATIONS)	;Allow stream to intercept print operation
	       (FUNCALL STREAM ':PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*))
	  (AND *PRINT-CIRCLE*
	       (%POINTERP EXP)
	       (OR (NOT (SYMBOLP EXP))
		   (NOT (SYMBOL-PACKAGE EXP)))
	       ;; This is a candidate for circular or shared structure printing.
	       ;; See what the hash table says about the object:
	       ;; NIL - occurs only once.
	       ;; T - occurs more than once, but no occurrences printed yet.
	       ;;  Allocate a label this time and print #label= as prefix.
	       ;; A number - that is the label.  Print only #label#.
	       (*CATCH 'LABEL-PRINTED
		 (SEND PRINT-HASH-TABLE ':MODIFY-HASH EXP
		       #'(LAMBDA (KEY VALUE KEY-FOUND-P STREAM)
			   KEY KEY-FOUND-P
			   (COND ((NULL VALUE) NIL)
				 ((EQ VALUE T)
				  (LET ((LABEL (INCF PRINT-LABEL-NUMBER))
					(*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM ':TYO #/=)
				    LABEL))
				 (T
				  (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM ':TYO #/#)
				    (*THROW 'LABEL-PRINTED T)))))
		       STREAM)
		 NIL))
	  (TYPECASE EXP
	    (:FIXNUM (PRINT-FIXNUM EXP STREAM))
	    (:SYMBOL (PRINT-PNAME-STRING EXP STREAM FASTP))
	    (:LIST
	     (IF (AND PRINLEVEL ( I-PRINDEPTH PRINLEVEL))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL *READTABLE*) STREAM FASTP)
	       (IF *PRINT-PRETTY*
		   (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		 (PRINT-LIST EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (:STRING
	     (IF ( (ARRAY-ACTIVE-LENGTH EXP) (ARRAY-LENGTH EXP))
		 (PRINT-QUOTED-STRING EXP STREAM FASTP)
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:INSTANCE
	      (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
	    (:ENTITY
	     (IF (MEMQ ':PRINT-SELF (FUNCALL EXP ':WHICH-OPERATIONS))
		 (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*) 
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:NAMED-STRUCTURE
	     (IGNORE-ERRORS
	       (SETQ NSS (NAMED-STRUCTURE-P EXP)))
	     (COND ((AND (SYMBOLP NSS)
			 (OR (GET NSS 'NAMED-STRUCTURE-INVOKE)
			     (GET NSS ':NAMED-STRUCTURE-INVOKE))
			 (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;Named structure that doesn't print itself
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (:ARRAY
	     (PRINT-ARRAY EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))
	    (:SMALL-FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP T))
	    (:FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP NIL))
	    (:BIGNUM
	     (PRINT-BIGNUM EXP STREAM FASTP))
	    (:RATIONAL
	     (PRINT-RATIONAL EXP STREAM FASTP))
	    (:COMPLEX (PRINT-COMPLEX EXP STREAM FASTP))
	    (:CHARACTER
	      (IF (NOT *PRINT-ESCAPE*)
		  (SEND STREAM ':TYO (LDB %%CH-CHAR EXP))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-BEFORE-FONT *READTABLE*))
		(IF (LDB-TEST %%CH-FONT EXP)
		    (LET ((*PRINT-BASE* 10.) (*NOPOINT T))
		      (PRIN1 (LDB %%CH-FONT EXP) STREAM)))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-PREFIX *READTABLE*))
		(LET ((BITS (CHAR-BITS EXP))
		      (CHAR (CHAR-CODE EXP)))
		  (SEND STREAM ':STRING-OUT
			       (NTH BITS
				    '("" "c-" "m-" "c-m-"
				      "s-" "c-s-" "m-s-" "c-m-s-"
				      "h-" "c-h-" "m-h-" "c-m-h-"
				      "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")))
		  ;; If we did get a character's long-name, for whatever reason, use it.
		  (LET ((CHNAME (FORMAT:OCHAR-GET-CHARACTER-NAME CHAR)))
		    (IF CHNAME
			(SEND STREAM ':STRING-OUT CHNAME))
		    ;; Otherwise print the character itself.
		    (AND ( BITS 0)
			 (SI:CHARACTER-NEEDS-QUOTING-P CHAR)
			 (SEND STREAM ':TYO (SI:PTTBL-SLASH *READTABLE*)))
		    (SEND STREAM ':TYO CHAR)))))
	    (:NUMBER
	     (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM *READTABLE*) STREAM FASTP)
	     (PRINT-RAW-STRING (GET-PNAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (SEND STREAM ':TYO (PTTBL-SPACE *READTABLE*))
	     (LET ((*PRINT-BASE* 8))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM *READTABLE*) STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

))

; From file OUTPUT.LISP PS:<L.IO1> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; OUTPUT  "

(defun ochar (char &optional style top-explain minwidth &rest options
		   &aux chname bits char0 char1)
  "Print the character CHAR in a fancy manner on STANDARD-OUTPUT.
STYLE specifies how to print:
 :READ (the default) means print it with #\ in a way that can be read back in;
 :BRIEF means print the character verbatim if it has no control bits,
  otherwise print its name if it's not a graphic char.
 :EDITOR means always use the name except for graphic chars,
  and use verbose forms of prefixes (/"Meta-/" instead of /"M-/").
 :SAIL means use , , etc as prefixes for the control bits
  and never use names.
 :LOZENGED means put the character name in a lozenge if stream can do so,
  unless the character is a graphic character.
  The padding options should not be used with :LOZENGED.
TOP-EXPLAIN if T means add on an explanation of how to type the character
 on the keyboard, using the Top or Greek key, if appropriate.
If MINWIDTH is specified, we pad on the right to that width.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0)."
  (declare (arglist char &optional style top-explain minwidth
		    &key (pad-char #/space) (minpad 0) (tab-period 1)))
  (setq char0
	(if (and (consp char) (eq (car char) ':mouse))
	    (third char) (character char)))
  (setq char1 (ldb %%kbd-char char0))
  (cond ((or minwidth options)
	 (with-stack-list (l (output nil (ochar char0 style top-explain)) nil)
	   (lexpr-funcall 'pad1 l minwidth options)))
	((eq style ':lozenged)
	 (if (and ( char1 200)
		  (send *standard-output* ':operation-handled-p ':display-lozenged-string))
	     (send *standard-output* ':display-lozenged-string
		   (format:output nil (ochar char0 ':editor)))
	   (ochar char0 ':editor))
	 (when top-explain
	   (ochar-explain-top-character char1)))
	((tv:char-mouse-p char0)
	 (cond ((eq style ':read)
		(or (setq chname (ochar-get-character-name char0))
		    (ferror nil "No name known for mouse character ~C" char0))
		(send *standard-output* ':string-out "#\")
		(princ chname))
	       (t (setq bits (ldb %%kbd-control-meta char0))
		  (and (bit-test 8 bits) (send *standard-output* ':string-out "Hyper-"))
		  (and (bit-test 4 bits) (send *standard-output* ':string-out "Super-"))
		  (and (bit-test 1 bits) (send *standard-output* ':string-out "Control-"))
		  (and (bit-test 2 bits) (send *standard-output* ':string-out "Meta-"))
		  (send *standard-output* ':string-out "Mouse-")
		  (send *standard-output* ':string-out (nth (ldb 0003 char0)
							     '("Left" "Middle" "Right")))
		  (if (setq chname (nth (setq bits (ldb 0303 char0))
					'("" "-Twice" "-Thrice")))
		      (send *standard-output* ':string-out chname)
		    (send *standard-output* ':tyo #/-)
		    (english-print (1+ bits))
		    (send *standard-output* ':string-out "-Times")))))
	(t
	  (selectq style
	    (:editor
	      (setq bits (ldb %%kbd-control-meta char0))
	      (and (bit-test 8 bits) (send *standard-output* ':string-out "Hyper-"))
	      (and (bit-test 4 bits) (send *standard-output* ':string-out "Super-"))
	      (and (bit-test 1 bits) (send *standard-output* ':string-out "Control-"))
	      (and (bit-test 2 bits) (send *standard-output* ':string-out "Meta-"))
	      (cond ((setq chname (ochar-get-character-name char1))
		     (let ((str (string-downcase chname)))
		       (aset (char-upcase (aref str 0)) str 0)
		       (send *standard-output* ':string-out str)
		       (return-array str)))
		    ((and (not (zerop bits)) ( #/a char1 #/z))
		     (send *standard-output* ':string-out "Shift-")
		     (send *standard-output* ':tyo (char-upcase char1)))
		    (t (send *standard-output* ':tyo char1))))
	    ((nil :read :brief)
	     (setq bits (char-bits char0))
	     (if (zerop bits) (setq bits nil))
	     ;; In :READ style, get a character name if possible.
	     ;; In :BRIEF style, get one only if there are control bits.
	     (when (or bits (neq style ':brief))
	       (setq chname (ochar-get-character-name char1)))
	     (unless (eq style ':brief) (send standard-output ':string-out "#\"))
	     ;; Now announce the control bits.
	     (if bits (send *standard-output*
			    ':string-out
			    (nth bits
				 '("" "c-" "m-" "c-m-"
				   "s-" "c-s-" "m-s-" "c-m-s-"
				   "h-" "c-h-" "m-h-" "c-m-h-"
				   "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
	     ;; If we did get a character's long-name, for whatever reason, use it.
	     (if chname
		 (let ((str (string-downcase chname)))
		   (if (eq style ':brief)
		       (setf (aref str 0) (char-upcase (aref str 0))))
		   (send *standard-output* ':string-out str)
		   (return-array str))
	       ;; Otherwise print the character itself.
	       ;; In :READ style, print a slash before chars that want it.
	       (and (neq style ':brief)
		    bits
		    ;; If using #\ but using the character, not the name, may need a slash.
		    (if ( #/a char1 #/z)
			(send *standard-output* ':string-out "sh-")
		      (if (si:character-needs-quoting-p char1)
			  (tyo (si:pttbl-slash *readtable*)))))
	       (send *standard-output* ':tyo char1)))
	    (:sail (send *standard-output* ':string-out (nth (ldb %%kbd-control-meta char0)
							     '("" "" "" ""
							       "" "" "" ""
							       "" "" "" ""
							       "" "" "" "")))
		   (and (memq char1 '(#/ #/ #/ #/ #/ #/))
			(send *standard-output* ':tyo #/))
		   (send *standard-output* ':tyo char1)))
	  (and top-explain 
	       (ochar-explain-top-character char1)))))

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
    (DO-FOREVER
      ;; Wrap around if at end; otherwise we might get stuck there.
      (IF (BP-= POINT (INTERVAL-LAST-BP BUFFER))
	  (MOVE-BP POINT (INTERVAL-FIRST-BP BUFFER)))
      ;; Print and advance over a random amount of stuff.
      (LET ((BP (FUNCALL FORWARD-FUNCTION POINT (FIX (SI:RANDOM-IN-RANGE 2 15.)) T)))
	(SEND *STANDARD-OUTPUT* ':STRING-OUT
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
		     (NTH (FLOOR RANDOM-LINE-NUMBER NLINES-OVER-TEN) TENTHS)))
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


;;;;;;;;;;;;;;;;;;;; cli:// compiler optimization ;;;;;;;;;;;;;;;;;;;;
; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(DEFUN ARITHEXP (X &AUX (L (LENGTH (CDR X))) (OP (GET (CAR X) 'TWO-ARGUMENT-FUNCTION)))
  (COND ((NULL OP)
	 (BARF X 'BAD-OP-ARITHEXP 'BARF))
	((= 0 L)
	 (OR (SETQ L (ASSQ OP '((*PLUS . 0) (*DIF . 0) (*TIMES . 1) (*QUO . 1) (%DIV . 1))))
	     (WARN 'BAD-ARITHMETIC ':IMPLAUSIBLE
		   "~S with no arguments." X))
	 (CDR L))
	((= L 1)
	 (COND ((MEMQ (CAR X) '(- -$))
		`(MINUS ,(CADR X)))
	       ((MEMQ (CAR X) '(// //$))
		`(*QUO 1 ,(CADR X)))
	       ((EQ (CAR X) 'CLI://)
		`(%DIV 1 ,(CADR X)))
	       (T (CADR X))))
	((= L 2) `(,OP . ,(CDR X)))
	(T `(,OP (,(CAR X) . ,(BUTLAST (CDR X))) . ,(LAST X)))))
))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(ADD-OPTIMIZER CLI://	ARITH-OPT)

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(DEFUN ARITH-OPT (FORM)
  (IF ( (LENGTH FORM) 2) FORM			;Let ARITHEXP handle this.
    (LOOP FOR ARG IN (CDR FORM)
	  WHEN (OR (NUMBERP ARG)
		   (AND (CONSP ARG)
			(NUMBERP (SETQ ARG (OPTIMIZE ARG T)))))
	       COLLECT ARG INTO WINNERS
	  ELSE COLLECT ARG INTO LOSERS
	  FINALLY
	  (RETURN (COND ((NULL (CDR WINNERS)) FORM)	;Can't hope to optimize.
			((NULL LOSERS) (FOLD-CONSTANTS FORM))	;Easy optimization.
			;; Now we are left with at least two args which are numbers, but at
			;; least one which is not.  Frobbing with divide from here on is
			;; dangerous, eg, (// 5 a 4) must not optimize into (// 1 a).
			((MEMQ (CAR FORM) '(// //$ QUOTIENT CLI://)) FORM)
			;; The only special case left is DIFFERENCE, which treats
			;; its first arg differently.
			((OR (NOT (MEMQ (CAR FORM) '(- -$ DIFFERENCE)))
			     (NUMBERP (CADR FORM)))
			 `(,(CAR FORM) ,(APPLY (CAR FORM) WINNERS) . ,LOSERS))
			(T `(,(CAR FORM) ,@LOSERS ,(APPLY #'+ WINNERS))))))))

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(ADD-OPTIMIZER CLI:ATAN	ARITH-OPT-NON-ASSOCIATIVE)

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(DEFPROP CLI:// NEED-AN-ARG STYLE-CHECKER)

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFCONST *FILE-TYPE-MODE-ALIST*
	  '((:LISP . :LISP)
	    (:TEXT . :TEXT)
	    (:MIDAS . :MIDAS)
	    (:DOC . :TEXT)
	    (:MSS . :TEXT)
	    (:PALX . :MIDAS)
	    (:MAC . :MIDAS)
	    (:TASM . :MIDAS)
	    (:CLU . :PL1)
	    (:PL1 . :PL1))
  "Alist mapping standard pathname type component strings into editor major mode name keywords.")

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-PATCHES (&REST OPTIONS &AUX TEM SOMETHING-CHANGED)
  "Load any new patches for one or more systems.
Options can include these symbols:
 :NOSELECTIVE - don't ask about each patch.
 :SILENT or :NOWARN - don't print out any information on loading patches
   (and also don't ask).
 :VERBOSE - says to print out information about loading each patch.
   This is the default and is only turned off by :silent and :nowarn.
 :UNRELEASED - says to load or consider unreleased patches.
   Once unreleased patches have been loaded, a band may not be dumped.
 :FORCE-UNFINISHED - load all patches that have not been finished yet,
   if they have QFASL files.  This is good for testing patches.
 :NOOP - do nothing
 :SITE - load latest site configuration info.
 :NOSITE - do not load latest site configuration info.
   :SITE is the default unless systems to load patches for are specified.

Options can also include :SYSTEMS followed by a list of systems to load patches for.
One or more names of systems are also allowed.

LOAD-PATCHES returns T if any patches were loaded, otherwise NIL."
  (CATCH-ERROR-RESTART (SYS:REMOTE-NETWORK-ERROR
			 "Give up on trying to load patches.")
    (LET ((SYSTEM-NAMES NIL)			;A-list of systems to load patches for.
	  (SELECTIVE-P T)			;Ask the user.
	  (VERBOSE-P T)				;Tell the user what's going on.
	  (UNRELEASED-P NIL)
	  (SITE-SPECIFIED-P NIL)
	  (SITE-P T)
	  (FORCE-THROUGH-UNFINISHED-PATCHES-P NIL))
      (DO ((OPTS OPTIONS (CDR OPTS)))
	  ((NULL OPTS))
	(SELECTQ (CAR OPTS)
	  (:SYSTEMS
	   (SETQ OPTS (CDR OPTS))
	   (SETQ SYSTEM-NAMES
		 (IF (CONSP (CAR OPTS))
		     (MAPCAR 'GET-PATCH-SYSTEM-NAMED (CAR OPTS))
		   (LIST (GET-PATCH-SYSTEM-NAMED (CAR OPTS)))))
	   (UNLESS SITE-SPECIFIED-P
	     (SETQ SITE-P NIL)))
	  ((:SILENT :NOWARN) (SETQ VERBOSE-P NIL SELECTIVE-P NIL))
	  (:VERBOSE (SETQ VERBOSE-P T))
	  (:SELECTIVE (SETQ SELECTIVE-P T))
	  (:SITE (SETQ SITE-P T SITE-SPECIFIED-P T))
	  (:NOOP NIL)
	  (:NOSITE (SETQ SITE-P NIL SITE-SPECIFIED-P T))
	  (:UNRELEASED (SETQ UNRELEASED-P T))
	  (:NOSELECTIVE (SETQ SELECTIVE-P NIL))
	  (:FORCE-UNFINISHED (SETQ FORCE-THROUGH-UNFINISHED-PATCHES-P T))
	  (OTHERWISE
	    (COND ((AND (OR (SYMBOLP (CAR OPTS)) (STRINGP (CAR OPTS)))
			(SETQ TEM (GET-PATCH-SYSTEM-NAMED (CAR OPTS) T)))
		   (PUSH TEM SYSTEM-NAMES)
		   (UNLESS SITE-SPECIFIED-P
		     (SETQ SITE-P NIL)))
		  (T (FERROR NIL "~S is not a LOAD-PATCHES option and not a system name."
				 (CAR OPTS)))))))
      (WITH-SYS-HOST-ACCESSIBLE
	(LET-IF VERBOSE-P ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
	  (WHEN SITE-P
	    (AND VERBOSE-P
		 (FORMAT T "~%Checking whether site configuration has changed..."))
	    (IF (IF SELECTIVE-P
		    (MAKE-SYSTEM ':SITE ':NO-RELOAD-SYSTEM-DECLARATION)
		  (IF VERBOSE-P
		      (MAKE-SYSTEM ':SITE ':NOCONFIRM ':NO-RELOAD-SYSTEM-DECLARATION)
		    (MAKE-SYSTEM ':SITE ':NOCONFIRM ':NO-RELOAD-SYSTEM-DECLARATION
				 ':SILENT)))
		(SETQ SOMETHING-CHANGED T)
	      (WHEN VERBOSE-P (FORMAT T "  it hasn't."))))
	  (OR SYSTEM-NAMES (SETQ SYSTEM-NAMES PATCH-SYSTEMS-LIST))
	  (LET ((FIRST-SYSTEM T))			; This is the first system being patched.
	    (DOLIST (PATCH SYSTEM-NAMES)
	      (CATCH-ERROR-RESTART (ERROR "Give up on patches for ~A." (CAR PATCH))
		(LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH T))
		       (NEW-VERS (PATCH-DIR-VERSION-LIST PATCH-DIR))
		       (MAJOR (PATCH-VERSION PATCH))
		       PATCHES-NOT-LOADED
		       (CHANGE-STATUS T)		;Ok to change the system status
		       (UNRELEASED-CONSIDERED NIL);T if considering unreleased patches.
		       (PATCH-SKIPPED NIL)	;T if considering patches after skipping one.
		       (PROCEED-FLAG (NOT SELECTIVE-P)))	; Has the user said to proceed?
		  (IF (AND (NULL PATCH-DIR) VERBOSE-P)
		      (FORMAT T "~&Skipping system ~A, whose patch directory cannot be accessed.~%"
			      PATCH)
		    ;; Get list of patches of this system not already loaded.
		    (SETQ PATCHES-NOT-LOADED
			  (CDR (MEMASSQ (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
					NEW-VERS)))
		    ;; Maybe announce the system.
		    (COND ((AND PATCHES-NOT-LOADED VERBOSE-P) ;;verbose and silent no sense
			   (SEND STANDARD-OUTPUT ':FRESH-LINE)
			   (UNLESS FIRST-SYSTEM (TERPRI))
			   (FORMAT T "~&Patches for ~A (Current version is ~D.~D):"
				   (PATCH-NAME PATCH) MAJOR (CAAR (LAST NEW-VERS)))))
		    (DOLIST (VERSION PATCHES-NOT-LOADED)
		      (LET* ((FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH)
							      ':PATCH-FILE
							      (PATCH-VERSION PATCH)
							      (VERSION-NUMBER VERSION)
							      ':QFASL)))
			;; NIL is used to mark patches that are reserved, but not finished.
			;; We can't load any more patches without this one, in order to
			;; make sure that any two systems claiming to be version xx.yy
			;; always have exactly the same set of patches loaded.  Punt.
			;; If someone forgets to finish a patch, we assume a hacker will
			;; eventually see what is happening and fix the directory to unstick
			;; things.  We might at least say the patches are unfinished.
			(UNLESS (VERSION-EXPLANATION VERSION)
			  (COND (VERBOSE-P
			       (FORMAT T "~&There are unfinished patches in ~A."
				       (PATCH-NAME PATCH))))
			  (UNLESS FORCE-THROUGH-UNFINISHED-PATCHES-P
			    (RETURN)))
			(WHEN (VERSION-UNRELEASED VERSION)
			  (COND (VERBOSE-P
				 (FORMAT T "~&There are unreleased patches in ~A."
					 (PATCH-NAME PATCH))))
			  (OR
			    FORCE-THROUGH-UNFINISHED-PATCHES-P 
			    UNRELEASED-P
			    UNRELEASED-CONSIDERED
			    (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			    (AND SELECTIVE-P
				 (WITH-TIMEOUT ((* 5 60. 60.)
						(FORMAT T " -- timed out, No.")
						NIL)
				   (FORMAT T "~&Such patches are subject to change; therefore,
  you should not load them if you are going to dump a band.
  If you are not going to dump a band, it is reasonable
  to load these patches to benefit from the improvements in them.")
				   (SETQ PROCEED-FLAG NIL)
				   (Y-OR-N-P "Consider the unreleased patches? (Automatic No after 5 minutes) ")))
			    (RETURN))
			  (SETQ UNRELEASED-CONSIDERED T))
			(IF VERBOSE-P
			    (PRINT-PATCH (PATCH-VERSION PATCH) VERSION))
			(SELECTQ-EVERY
			  (COND (PROCEED-FLAG)
				(T (WITH-TIMEOUT ((* 5 60. 60.)
						  (FORMAT T " -- timed out, Proceed.")
						  'PROCEED)
				     (FQUERY '(:CHOICES (((T "Yes.") #/Y #/SP #/T #/HAND-UP)
							 ((NIL "No.") #/N #/RUBOUT #/HAND-DOWN)
							 ((PROCEED "Proceed.") #/P)))
					     "Load? (Automatic Proceed after 5 minutes) "))))
			  (NIL
			   ;; "No", don't load any more for this system.
			   ;; Also don't change the status.
			   ;; Except, if we are considering unreleased patches,
			   ;; loading out of order is no worse than loading unreleased
			   ;; patches in the first place, so keep on offering.
			   (SETQ CHANGE-STATUS NIL)
			   (UNLESS (OR FORCE-THROUGH-UNFINISHED-PATCHES-P
				       UNRELEASED-CONSIDERED)
			     (RETURN NIL))
			   (WHEN (EQ VERSION (CAR (LAST PATCHES-NOT-LOADED)))
			     ;; Don't give a spiel about following patches
			     ;; if there are none.
			     (RETURN NIL))
			   (UNLESS (OR PATCH-SKIPPED
				       (EQ (PATCH-STATUS PATCH) ':INCONSISTENT))
			     (FORMAT T "~&If you load any following patches for this system,
  they will be out of sequence, so you must not dump a band.")
			     (SETQ PATCH-SKIPPED T)))
			  (PROCEED
			   ;; "Proceed" with the rest for this system.
			   (SETQ PROCEED-FLAG T))
			  ((T PROCEED)
			   ;; "Yes" or "Proceed", do this one.
			   (SETQ SOMETHING-CHANGED T)
			   ;; Unfinished, unreleased or out of sequence =>
			   ;;  mark system as inconsistent.
			   (WHEN (OR PATCH-SKIPPED
				     (NULL (VERSION-EXPLANATION VERSION))
				     (VERSION-UNRELEASED VERSION))
			     (UNLESS (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			       (SETF (PATCH-STATUS PATCH) ':INCONSISTENT)
			       (FORMAT T "~&~A is now inconsistent; do not dump a band."
				       (PATCH-NAME PATCH))))
			   ;; Avoid error if non ex file, if patch is known to be unfinished.
			   (CONDITION-CASE-IF (NULL (VERSION-EXPLANATION VERSION)) ()
			       (LOAD FILENAME NIL NIL T (NOT VERBOSE-P))	; Don't set default,
			     (FS:FILE-NOT-FOUND
			      (IF VERBOSE-P
				  (FORMAT T "~&File ~A does not exist, ignoring this patch."
					  FILENAME))))
			   (PUSH VERSION (PATCH-VERSION-LIST PATCH))))))
		    (AND CHANGE-STATUS
			 (NEQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			 (LET ((NEW-STATUS (PATCH-DIR-STATUS PATCH-DIR)))
			   (COND ((NEQ (PATCH-STATUS PATCH) NEW-STATUS)
				  (SETQ SOMETHING-CHANGED T)
				  (AND VERBOSE-P
				       (FORMAT T "~&~A is now ~A."
					       (PATCH-NAME PATCH)
					       (FOURTH (ASSQ NEW-STATUS
							     SYSTEM-STATUS-ALIST))))
				  ;; Update the status.
				  (SETF (PATCH-STATUS PATCH) NEW-STATUS))))))))
	      (SETQ FIRST-SYSTEM NIL)))))))
  SOMETHING-CHANGED)

))


;;;;;;;;;;;;;;;;;;;; string functions on characters ;;;;;;;;;;;;;;;;;;;;
; From file string#>  FC: mly\b
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING-APPEND (&REST STRINGS &AUX (LENGTH 0) STRING COERCED (I 0))
  "Append any number of strings (or arrays).  The value is always a newly constructed array.
The value will be the same type of array as the first argument.
Symbols and numbers are coerced into strings."
    (DOLIST (S STRINGS)
      (INCF LENGTH
	    (TYPECASE S
	      (ARRAY (LENGTH S))
	      ((OR FIXNUM CHARACTER) 1)
	      (T (STRING-LENGTH S)))))
    (SETQ STRING (MAKE-ARRAY LENGTH
			     ':TYPE (IF (ARRAYP (CAR STRINGS)) (ARRAY-TYPE (CAR STRINGS))
					'ART-STRING)))
    (DOLIST (S STRINGS)
      (TYPECASE S
	((OR NUMBER CHARACTER)
	 (SETF (AREF STRING I) S)
	 (INCF I 1))
	(T (SETQ COERCED (IF (STRINGP S) S (STRING S)))
	   (COPY-ARRAY-PORTION COERCED 0 (SETQ LENGTH (ARRAY-ACTIVE-LENGTH COERCED))
			       STRING I (INCF I LENGTH)))))
    STRING)

))

; From file string#>  FC: mly\b
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING-NCONC (MUNG &REST STRINGS &AUX LEN FINAL-LEN S2LEN)
  "STRING-NCONC extends the first string and tacks on any number of additional strings.
The first argument must be a string with a fill-pointer.
Returns the first argument, which may have been moved and forwarded,
just like ADJUST-ARRAY-SIZE."
  (SETQ FINAL-LEN (SETQ LEN (FILL-POINTER MUNG)))
  (DOLIST (STR2 STRINGS)
    (SETQ FINAL-LEN (+ FINAL-LEN (STRING-LENGTH STR2))))
  (AND (> FINAL-LEN (ARRAY-LENGTH MUNG))
       (ADJUST-ARRAY-SIZE MUNG FINAL-LEN))
  (DOLIST (STR2 STRINGS)
    (TYPECASE STR2
      ((OR FIXNUM CHARACTER)
       (VECTOR-PUSH STR2 MUNG)
       (INCF LEN 1))
      (T (SETQ STR2 (IF (STRINGP STR2) STR2 (STRING STR2)) S2LEN (ARRAY-ACTIVE-LENGTH STR2))
	 (COPY-ARRAY-PORTION STR2 0 S2LEN MUNG LEN (INCF LEN S2LEN))
	 (SETF (FILL-POINTER MUNG) LEN))))
  MUNG)

))

; From file string#>  FC: mly\b
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING-NREVERSE (STRING &AUX LEN)
  "Destructively modify string by reversing the order of its elements.
Actually, this will work on any one-dimensional array."
  (TYPECASE STRING
    ((OR FIXNUM CHARACTER))
    (T (TYPECASE STRING
	 (ARRAY)
	 (SYMBOL
	  ;; Special treatment to avoid munging symbols
	  (WHEN (SYMBOL-PACKAGE STRING)
	    (FERROR NIL "Illegal to mung the PNAME of an interned symbol."))
	  (SETQ STRING (GET-PNAME STRING)))
	 (T (COERCE-STRING-ARG STRING)))
       (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING))
       (DO ((I 0 (1+ I))
	    (J (1- LEN) (1- J)))
	   ((< J I))
	 (SWAPF (AREF STRING I) (AREF STRING J)))))
  STRING)

))

; From file string#>  FC: mly\b
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN ALPHALESSP (X Y)
  "T if printed representation of X is less than that of Y.
Characters and numbers come before symbols//strings, before random objects, before lists.
Characters and numbers are compared using CHAR<; symbols//strings with STRING-LESSP;
random objecs by printing them(!); lists are compared recursively."
  (COND ((OR (NUMBERP X) (CHARACTERP X)) (OR (NOT (OR (NUMBERP Y) (CHARACTERP Y)))
					     (CHAR< X Y)))
	((OR (NUMBERP Y) (CHARACTERP Y)) NIL)
	((OR (SYMBOLP X) (STRINGP X))
	 (OR (NOT (OR (SYMBOLP Y) (STRINGP Y)))
	     (STRING-LESSP X Y)))
	((OR (SYMBOLP Y) (STRINGP Y)) NIL)
	((ATOM X) (OR (CONSP Y)
		      (STRING-LESSP (FORMAT NIL "~S" X) (FORMAT NIL "~S" Y))))
	((ATOM Y) NIL)
	(T (DO ((X1 X (CDR X1)) (Y1 Y (CDR Y1)))
	       ((NULL Y1))
	     (OR X1 (RETURN T))
	     (AND (ALPHALESSP (CAR X1) (CAR Y1)) (RETURN T))
	     (AND (ALPHALESSP (CAR Y1) (CAR X1)) (RETURN NIL))))))

))

; From file string#>  FC: mly\b
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN ALPHAEQUAL (X Y)
  "T if X and Y print the same, or nearly so.
Exceptions: numbers and characters are compared using CHAR=
and a symbol and its pname compare as equal."
  (TYPECASE X
    ((OR NUMBER CHARACTER)
     (AND (OR (NUMBERP Y) (CHARACTERP Y))
	  (CHAR= X Y)))
    ((OR SYMBOL STRING)
     (AND (OR (SYMBOLP Y) (STRINGP Y))
	  (STRING-EQUAL X Y)))
    (ATOM
     (AND (ATOM Y)
	  (STRING-EQUAL (FORMAT NIL "~S" X) (FORMAT NIL "~S" Y))))
    (T (DO ((X1 X (CDR X1)) (Y1 Y (CDR Y1)))
	   ((NULL X1) (NULL Y1))
	 (OR Y1 (RETURN NIL))
	 (OR (ALPHAEQUAL (CAR X1) (CAR Y1)) (RETURN NIL))))))

))

; From file string#>  FC: mly\b
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN STRING (X)
  "Convert X to a string if possible."
  (TYPECASE X
    (STRING X)
    (SYMBOL (GET-PNAME X))
    ;; this kludginess is due to the fact that (typep x 'string-char) loses on fixnums
    ;; and that string-char-p blows out on non-character non-fixnums
    ((AND (OR FIXNUM CHARACTER)
	  (SATISFIES STRING-CHAR-P))
     (VALUES (MAKE-ARRAY 1 ':TYPE 'ART-STRING ':INITIAL-VALUE X)))
    (INSTANCE
     (SEND X ':SEND-IF-HANDLES ':STRING-FOR-PRINTING))
    (T
     (FERROR NIL "Cannot convert ~S into a string." X))))

))

; From file STRING.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRING  "

(DEFUN CHARACTER (X)
  "Convert X to a character if possible."
  (COND ((NUMBERP X)
	 X)
	((CHARACTERP X)
	 (%POINTER X))
	((AND (STRINGP X) (= (LENGTH X) 1))
	 (AREF X 0))
	((AND (SYMBOLP X) (= (LENGTH (GET-PNAME X)) 1))
	 (AREF (GET-PNAME X) 0))
	(T (FERROR NIL "Cannot convert ~S into a character." X))))

))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN SIMILAR-BUFFER-FILES-WARNING (BUFFER &AUX SAME-NAME SAME-TYPE SAME-EVERYTHING)
  "Warn if any buffer other than BUFFER is visiting the same or a similar file."
  (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
    (AND (NEQ (CDR ELT) BUFFER)
	 (BUFFER-PATHNAME (CDR ELT))
	 (BUFFER-FILE-ID (CDR ELT))
	 (NOT (NODE-SPECIAL-TYPE (CDR ELT)))
	 (IF (EQUALP (SEND (BUFFER-PATHNAME BUFFER) ':STRING-FOR-EDITOR)
		     (SEND (BUFFER-PATHNAME (CDR ELT)) ':STRING-FOR-EDITOR))
	     (RETURN (SETQ SAME-EVERYTHING (CDR ELT)))
	   (IF (EQUALP (SEND (BUFFER-PATHNAME BUFFER) ':NAME)
		       (SEND (BUFFER-PATHNAME (CDR ELT)) ':NAME))
	       (COND ((EQUALP (SEND (BUFFER-PATHNAME BUFFER) ':TYPE)
			      (SEND (BUFFER-PATHNAME (CDR ELT)) ':TYPE))
		      (SETQ SAME-TYPE (CDR ELT)))
		     (T (SETQ SAME-NAME (CDR ELT))))))))
  (IF SAME-EVERYTHING
      (FORMAT *QUERY-IO* "~&Warning: Buffer ~A~&  is also visiting file ~A."
	      (BUFFER-NAME SAME-EVERYTHING) (BUFFER-PATHNAME SAME-EVERYTHING))
    (LET ((LOSER (OR SAME-TYPE SAME-NAME)))
      (IF LOSER
	  (FORMAT *QUERY-IO* "~&Note: Another buffer ~A~&  is visiting file ~A."
		  (BUFFER-NAME LOSER) (BUFFER-PATHNAME LOSER))))))

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN GET-SYSTEM-VERSION (&OPTIONAL (SYSTEM "System"))
  "Returns the major and minor version numbers and status of the system named SYSTEM.
This describes what is currently loaded, not the most recent ones on disk."
  (DECLARE (VALUES MAJOR MINOR STATUS))
  (LET ((PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM T T)))
    (IF PATCH
	(VALUES (PATCH-VERSION PATCH)
		(VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
		(PATCH-STATUS PATCH)))))

))

;;;;;;;;;;;;;;;;;;;; :nil in zmacs and in parsing attribute lists ;;;;;;;;;;;;;;;;;;;;
; From file ZMNEW.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "

(DEFUN SET-ATTRIBUTE (KEYWORD NAMESTRING &OPTIONAL DEFAULT)
  (LET ((INPUT (STRING-TRIM *BLANKS* (TYPEIN-LINE-READLINE "Set ~A:" NAMESTRING)))
	(*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
	(*PRINT-BASE* 10.)
	(*READ-BASE* 10.))
    (SEND *INTERVAL* ':SET-ATTRIBUTE KEYWORD
	  (IF (EQUAL INPUT "")
	      DEFAULT
	    (LET ((TEM (READ-FROM-STRING INPUT)))
	      (IF (EQ TEM ':NIL) NIL TEM)))
	  ':QUERY))
  DIS-NONE)

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN FILE-PARSE-PROPERTY-LIST (STRING &OPTIONAL (START 0) (END (ARRAY-ACTIVE-LENGTH STRING))
				 &AUX PLIST (*READ-BASE* 10.)
				      (*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
				      (*READTABLE* SI:INITIAL-COMMON-LISP-READTABLE)
				      ERROR)
  (AND STRING
       (ARRAYP STRING)
       (= (ARRAY-ELEMENT-SIZE STRING) 8)
       ;; Narrow down to the stuff between the -*-'s
       (SETQ START (STRING-SEARCH "-*-" STRING START END))
       (SETQ END (STRING-SEARCH "-*-" STRING (SETQ START (+ START 3)) END))
       ;; Now parse it.
       (IF (NOT (%STRING-SEARCH-CHAR #/: STRING START END))
	   (SETQ PLIST (LIST ':MODE (READ-FROM-SUBSTRING STRING START END)))
	 (DO ((S START (1+ SEMI-IDX))
	      (COLON-IDX) (SEMI-IDX) (SYM) (ELEMENT NIL NIL) (DONE)
	      (WIN-THIS-TIME NIL NIL))
	     (NIL)
	   (OR (SETQ SEMI-IDX (%STRING-SEARCH-CHAR #/; STRING S END))
	       (SETQ DONE T SEMI-IDX END))
	   (OR (SETQ COLON-IDX (%STRING-SEARCH-CHAR #/: STRING S SEMI-IDX))
	       (RETURN NIL))
	   (IGNORE-ERRORS
	     (OR (SETQ SYM (READ-FROM-SUBSTRING STRING S COLON-IDX))
		 (RETURN NIL))
	     (IGNORE-ERRORS
	       (IF (%STRING-SEARCH-CHAR #/, STRING (SETQ S (1+ COLON-IDX)) SEMI-IDX)
		   (DO ((COMMA-IDX) (ELEMENT-DONE))
		       (NIL)
		     (OR (SETQ COMMA-IDX (%STRING-SEARCH-CHAR #/, STRING S SEMI-IDX))
			 (SETQ ELEMENT-DONE T COMMA-IDX SEMI-IDX))
		     (SETQ ELEMENT
			   (NCONC ELEMENT
				  (NCONS (LET ((TEM (READ-FROM-SUBSTRING STRING S COMMA-IDX)))
					   (IF (EQ TEM ':NIL) NIL TEM)))))
		     (AND ELEMENT-DONE (RETURN NIL))
		     (SETQ S (1+ COMMA-IDX)))
		 (SETQ ELEMENT (LET ((TEM (READ-FROM-SUBSTRING STRING S SEMI-IDX)))
				 (IF (EQ TEM ':NIL) NIL TEM))))
	       (SETQ WIN-THIS-TIME T))
	     (SETQ PLIST (NCONC PLIST (LIST* SYM ELEMENT NIL))))	;Nicer CDR-CODEs
	   (UNLESS WIN-THIS-TIME
	     (SETQ ERROR T))
	   (AND DONE (RETURN NIL)))))
  (VALUES PLIST ERROR))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN PRINT-RANDOM-SEXP (SEXP &OPTIONAL (STREAM STANDARD-OUTPUT))
  (LET ((*PRINT-BASE* 10.)
	(*NOPOINT T) (*PRINT-RADIX* NIL)
	(*READTABLE* SI:INITIAL-READTABLE)
	(*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
    (PRIN1 SEXP STREAM)))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN PRINT-DECIMAL-PROPERTY (PROP STREAM)
  (LET ((*PRINT-BASE* 10.)
	(*NOPOINT T)
	(*PRINT-RADIX* NIL)
	(*READTABLE* SI:INITIAL-READTABLE))
    (PRIN1 PROP STREAM)))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN PARSE-DIRECTORY-BOOLEAN-PROPERTY (STRING START)
  (LET ((TEM (READ-FROM-STRING STRING NIL START)))
    (IF (EQ TEM ':NIL) NIL TEM)))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN (:COMMON-LISP FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
  (VALUES (LIST* '*READTABLE* 'SI:*READER-SYMBOL-SUBSTITUTIONS*
		 'SI:INTERPRETER-FUNCTION-ENVIRONMENT '*NOPOINT
		 NIL)
	  (LIST* (IF VAL SI:COMMON-LISP-READTABLE SI:STANDARD-READTABLE)
		 (IF VAL SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS* NIL)
		 NIL T
		 NIL)))

))

; From file DEFS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFMETHOD (ZMACS-BUFFER :BEFORE :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP NAME)
    (LOOP AS POSSIBLE-NAME =
	     (FORMAT:OUTPUT NIL
	       "*Buffer-"
	       (LET ((*PRINT-BASE* 10.) (*NOPOINT T) (*PRINT-RADIX* NIL))
		 (PRIN1 (INCF *ZMACS-BUFFER-COUNTER*)))
	       "*")
	  WHEN (NULL (FIND-BUFFER-NAMED POSSIBLE-NAME))
	  RETURN (SETQ NAME POSSIBLE-NAME))))

))

; From file PATED.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN CREATE-PRIVATE-PATCH-BUFFER (FILENAME)
  (LET* ((*FIND-FILE-NOT-FOUND-IS-AN-ERROR* NIL))
    (SETQ *PATCH-BUFFER* (FIND-FILE FILENAME NIL)))	;NIL means "don't select this buffer".
  (SETQ *PATCH-SYSTEM* NIL *PATCH-NUMBER* NIL)
  (IF (BP-= (INTERVAL-FIRST-BP *PATCH-BUFFER*)
	    (INTERVAL-LAST-BP *PATCH-BUFFER*))
      (INITIALIZE-PATCH-BUFFER *PATCH-BUFFER* NIL)))

))

; From file METH.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (NODE :GET-ATTRIBUTE) (ATTRIBUTE &OPTIONAL (DEFAULT NIL DEFAULT-SPECIFIED-P))
  (GET (LOCF SI:PROPERTY-LIST) ATTRIBUTE
       (IF DEFAULT-SPECIFIED-P DEFAULT (EVAL (GET ATTRIBUTE 'DEFAULT-ATTRIBUTE-VALUE)))))

))

; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN WRITE-PATCH-DIRECTORY (PATCH-SYSTEM PATCH-DIR)
  "Write out a new patch directory file for PATCH-SYSTEM.
PATCH-DIR is a list described by the defstruct PATCH-DIR,
which is the data to write into the file."
  (LET ((*PRINT-BASE* 10.) (*READ-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE)
	(*NOPOINT T) (*PRINT-RADIX* NIL)
	(*READTABLE* INITIAL-READTABLE))
    (WITH-OPEN-FILE (STREAM (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
						   ':VERSION-DIRECTORY
						   (PATCH-VERSION PATCH-SYSTEM))
			       '(:WRITE))
       (FORMAT STREAM
	       ";;; -*- Mode: Lisp; Package: User; Base: 10.; Common-Lisp: NIL; Patch-File: T -*-
;;; Patch directory for ~A version ~D
"
	       (PATCH-NAME PATCH-SYSTEM) (PATCH-VERSION PATCH-SYSTEM))
       (WRITE-RESPONSIBILITY-COMMENT STREAM)
       (SEND STREAM ':TYO #/()
       (PRIN1 (PATCH-DIR-STATUS PATCH-DIR) STREAM)
       (SEND STREAM ':STRING-OUT "
 (")
       (DOLIST (PATCH (PATCH-DIR-VERSION-LIST PATCH-DIR))
	 (PRIN1 PATCH STREAM)
	 (SEND STREAM ':STRING-OUT "
  "))
       (SEND STREAM ':STRING-OUT "))"))))
))

; From file SYSMEN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFUN TRACE-VIA-MENUS (&OPTIONAL FCN)
  (USING-RESOURCE (TRACE-POP-UP-WINDOW POP-UP-TEXT-WINDOW)
    (USING-RESOURCE (TRACE-POP-UP-MENU TRACE-POP-UP-MENU)
      (FUNCALL TRACE-POP-UP-WINDOW ':SET-LABEL "Trace")
      (FUNCALL TRACE-POP-UP-WINDOW ':SET-SIZE 1000 300)
      (FUNCALL TRACE-POP-UP-WINDOW ':CENTER-AROUND MOUSE-X MOUSE-Y)
      (WINDOW-CALL (TRACE-POP-UP-WINDOW :DEACTIVATE)
	(UNWIND-PROTECT
	  (LET ((BLINKER (CAR (SHEET-BLINKER-LIST TRACE-POP-UP-WINDOW))))
	    (COND ((NULL FCN)
		   ;Make sure blinker is blinking
		   (BLINKER-SET-VISIBILITY BLINKER ':BLINK)
		   (FORMAT TRACE-POP-UP-WINDOW
			   "Type in name of function to be traced or untraced.
  Abort quits.~%")
		   (DO ((*TERMINAL-IO* TRACE-POP-UP-WINDOW) ;for errors, aborts, etc.
			(*STANDARD-INPUT* TRACE-POP-UP-WINDOW)
			(*STANDARD-OUTPUT* TRACE-POP-UP-WINDOW))
		       (NIL)
		     (SETQ FCN (READ))
		     (IF (FDEFINEDP FCN)
			 (RETURN NIL)
			 (FORMAT T " ;not a defined function, try again~%")))))
	    (FUNCALL TRACE-POP-UP-MENU ':MOVE-NEAR-WINDOW TRACE-POP-UP-WINDOW)
	    (DO ((FORM (IF (ATOM FCN) `(TRACE (,FCN)) `(TRACE (:FUNCTION ,FCN))))
		 (CHOICE) (OPTION) (ARG))
		(NIL)
	      ;;Put the current status on the text window
	      (FUNCALL TRACE-POP-UP-WINDOW ':CLEAR-SCREEN)
	      (GRIND-TOP-LEVEL FORM 76 TRACE-POP-UP-WINDOW)	;76 is width in characters
				;Not listening to the keyboard any more, shut off blinker
	      (BLINKER-SET-VISIBILITY BLINKER NIL)
				;Get input from the menu
	      (SETQ CHOICE (FUNCALL TRACE-POP-UP-MENU ':CHOOSE)
		    OPTION (FIRST CHOICE))			      
	      (COND ((NULL CHOICE))	;Try again if outside menu
		    ((EQ OPTION 'UNTRACE)
		     (EVAL `(UNTRACE ,FCN))
		     (RETURN NIL))
		    ((EQ OPTION 'QUIT)
		     (RETURN NIL))
		    ((EQ OPTION 'DO-IT)
		     (EVAL FORM)
		     (RETURN NIL))
		    (T ;; let's make it smarter so that we can undo choices.
		     (let ((previous-entry-position
			     (find-position-in-list (second choice) (second form))))
		       (cond ((and previous-entry-position
				   (or (eq (second choice) ':BREAK)
				       (eq (second choice) ':EXITBREAK))
				   (cddr choice))
			      ;; then we should either remove it from the list if it is
			      ;; the same or just change the argument.
			      (if (eq (third choice) (nth (1+ previous-entry-position)
							  (second form)))
				  ;; remove the choice
				  (if (eq previous-entry-position 0)
				      (setf (second form) (cddr (second form)))
				    (setf (nthcdr previous-entry-position (second form))
					  (nthcdr (+ 2 previous-entry-position)
						  (second form))))
				;; fix the choice
				(setf (nth (1+ previous-entry-position)
					   (second form))
				      (third choice))))
			     ((and previous-entry-position (null option))
			      ;; delete the choice
			      (if (eq previous-entry-position 0)
				  (setf (second form) (cdr (second form)))
				(setf (nthcdr previous-entry-position (second form))
				      (nthcdr (1+ previous-entry-position)
					      (second form)))))
			     ((null option)
			      ;; just add the object
			      (SETF (SECOND FORM)
				    (APPEND (SECOND FORM) (CDR CHOICE))))
			     (T	;Needs an arg, get it
			      (FORMAT TRACE-POP-UP-WINDOW "~2%~A:~%" OPTION)
			      ;; Turn on blinker
			      (BLINKER-SET-VISIBILITY BLINKER ':BLINK)
			      (LET ((*TERMINAL-IO* TRACE-POP-UP-WINDOW)
				    (FLAG))
				(MULTIPLE-VALUE (ARG FLAG)
				  (with-input-editing (*terminal-io*
							'((:full-rubout :full-rubout)
							  (:activation = #/end)))
				    (read-for-top-level *terminal-io*)))
				(UNLESS FLAG
				  ;; if previous entry replace the argument
				  ;; otherwise add on new argument
				  (if (null previous-entry-position)
				      (SETF (SECOND FORM)
					    (APPEND (SECOND FORM) (cdr choice) (ncons ARG)))
				    (setf (nth (1+ previous-entry-position)
					       (second form))
					  arg)))))))))))
	  (FUNCALL TRACE-POP-UP-MENU ':DEACTIVATE))))))

))

;;;;;;;;;;;;;;;;;;;; cli:eval in eval-when, :special/special ;;;;;;;;;;;;;;;;;;;;
; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN &OPTIONAL COMPILE-TIME-TOO)
  (PROG TOP (FN (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	(())
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM TOP NIL))
      (IF (ATOM FORM) (RETURN NIL))
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
	    ((EQ (CAR OFORM) 'DEFSTRUCT)
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (AND (CONSP FORM)
	   (NEQ (CAR FORM) 'EVAL-WHEN)
	   COMPILE-TIME-TOO
	   (FUNCALL PROCESS-FN FORM 'DECLARE))
      (COND ((ATOM FORM)
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ (CAR FORM) 'EVAL-WHEN)
	     (OR (AND (OR (NOT (ATOM (CADR FORM))) (NULL (CADR FORM)))	;LISTP eventually
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL CLI:EVAL LOAD COMPILE))))
		 (FERROR NIL "~S invalid EVAL-WHEN times;
must be a list of EVAL, LOAD, and//or COMPILE."
			     (CADR FORM)))
	     (LET* ((COMPILE (MEMQ 'COMPILE (CADR FORM)))
		    (LOAD (MEMQ 'LOAD (CADR FORM)))
		    (EVAL (or (MEMQ 'EVAL (CADR FORM)) (memq 'cli:eval (cadr form))))
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
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO))
		   (CDDR FORM)))
	    ((EQ FN 'WITH-SELF-ACCESSIBLE)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO))
		   (CDDR FORM)))
	    ((EQ FN 'PROGN)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO))
		   (CDR FORM)))
	    ((MEMQ FN '(MACRO DEFSUBST))
	     (FUNCALL PROCESS-FN FORM 'MACRO))
	    ((MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
				EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT DEFF-MACRO))
	     (FUNCALL PROCESS-FN FORM 'SPECIAL))
	    ((EQ FN 'DECLARE)
	     (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	    ((EQ FN 'PROCLAIM)
	     (FUNCALL PROCESS-FN FORM 'DECLARE))
	    ((EQ FN 'COMMENT) NIL)
	    ((EQ FN 'PATCH-SOURCE-FILE)
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO))
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

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN QC-FILE-FORM (FORM)
  (PROG (TEM FV)
    (COND ((ATOM FORM))
	  ((EQ (CAR FORM) 'COMMENT))		;Delete comments entirely
	  ((EQ (CAR FORM) 'DEFUN)
	   (SETQ TEM (CADR FORM))
	   (SETQ FV (SI:PROCESS-DEFUN-BODY TEM (CDDR FORM)))
	   (COND (QC-FILE-LOAD-FLAG
		     (RPLACA (FUNCTION-CELL-LOCATION TEM) FV)	;In case used interpreted
		     (COMPILE-1 TEM FV)
		     (RETURN (QC-FILE-FASD-FORM FORM T))))
           (QC-TRANSLATE-FUNCTION TEM FV
				  'MACRO-COMPILE
				  (COND (QC-FILE-REL-FORMAT 'REL)
					(T 'QFASL)))
	   (IF (AND *MICROCOMPILE-SWITCH*
		    (GETDECL TEM 'MICROCOMPILE))
	       (QC-TRANSLATE-FUNCTION
		 TEM FV				;Once more, with feeling
		 'MICRO-COMPILE
		 (COND (QC-FILE-REL-FORMAT 'REL)
		       (T 'QFASL))))
	   (RETURN NIL)
           )
	  (QC-FILE-LOAD-FLAG (EVAL FORM)))
    (RETURN (QC-FILE-FASD-FORM FORM T))))

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-DECLARE (DECL-LIST PROCESS-FN)
    (MAPC #'(LAMBDA (DECL)
	      (FUNCALL PROCESS-FN DECL
		       (IF (MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
			   'SPECIAL
			 'DECLARE)))
	  DECL-LIST))

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN EVAL-WHEN (&QUOTE TIMES &REST FORMS &AUX VAL)
  "Process the FORMS only at the specified TIMES.
TIMES is a list which may include COMPILE, EVAL or LOAD.
EVAL means to eval the FORMS if the EVAL-WHEN is processed by the interpreter,
 or to compile and eval them when compiling to core.
LOAD means the compiler when compiling to a file should compile the FORMS
 if appropriate and then make them be executed when the QFASL file is loaded.
COMPILE means the compiler should execute the forms
 at compile time.
/(EVAL LOAD) is equivalent to the normal state of affairs."
    (OR (AND (OR (NOT (ATOM TIMES)) (NULL TIMES))	;LISTP eventually
	     (LOOP FOR TIME IN TIMES ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE CLI:EVAL))))
	(FERROR NIL "~S invalid EVAL-WHEN times;
	must be a list of EVAL, LOAD, and//or COMPILE."
		    TIMES))
    (COND ((OR (MEMQ 'EVAL TIMES) (MEMQ 'CLI:EVAL TIMES))
	   (DOLIST (FORM FORMS) (SETQ VAL (EVAL FORM)))
	   VAL)))

))


; From file PATHNM.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFINE-CANONICAL-TYPE :MAC "MAC")
(DEFINE-CANONICAL-TYPE :TASM "TASM")
(DEFINE-CANONICAL-TYPE :DOC "DOC")
(DEFINE-CANONICAL-TYPE :MSS "MSS")
(DEFINE-CANONICAL-TYPE :TEX "TEX")
(DEFINE-CANONICAL-TYPE :PL1 "PL1")
(DEFINE-CANONICAL-TYPE :CLU "CLU")

))


; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN FILE-GET-PASSWORD (UID HOST &OPTIONAL DIRECTORY-FLAG)
  "Given a username and host, ask for either a password or a username and password.
If DIRECTORY-FLAG is set, we are using directory names, not passwords"
  (DECLARE (VALUES NEW-USER-ID PASSWORD ENABLE-CAPABILITIES))
  (LET* ((LINE (MAKE-STRING 30 ':FILL-POINTER 0))
	 ENABLE-CAPABILITIES CHAR UID-P
	 (HACK (AND (SEND *QUERY-IO* ':OPERATION-HANDLED-P ':CLEAR-BETWEEN-CURSORPOSES)
		    (SEND *QUERY-IO* ':OPERATION-HANDLED-P ':COMPUTE-MOTION)))
	 START-X START-Y)
    (UNLESS DIRECTORY-FLAG
      (SETQ UID (OR (CDR (ASSQ HOST USER-UNAMES)) UID)))
    (LET ((PW (CADR (ASSOC-EQUALP (LIST UID (SEND HOST ':NAME))
				  USER-HOST-PASSWORD-ALIST))))
      (WHEN PW (RETURN-FROM FILE-GET-PASSWORD UID PW)))
    (TAGBODY
	(WHEN HACK (SETQ HACK (MAKE-STRING 30. ':INITIAL-VALUE #/X ':FILL-POINTER 0)))
     RESTART
	(UNLESS DIRECTORY-FLAG
	  (SETQ UID (OR (CDR (ASSQ HOST USER-UNAMES)) UID)))
	(LET ((PW (CADR (ASSOC-EQUALP (LIST UID (SEND HOST ':NAME))
				      USER-HOST-PASSWORD-ALIST))))
	  (WHEN PW (RETURN-FROM FILE-GET-PASSWORD UID PW)))
	(FORMAT *QUERY-IO*
		(IF DIRECTORY-FLAG "~&Type the password for directory ~A on host ~A,
or a directory and password.  /"Directory/" here includes devices as well: "
		  "~&Current login name is ~A ~<~%~:;for host ~A.~>
Type either password or ~<~%~:;loginname<space>password: ~>")
		UID HOST)
	(WHEN HACK (MULTIPLE-VALUE (START-X START-Y) (SEND *QUERY-IO* ':READ-CURSORPOS)))
     L  (SETQ CHAR (SEND *QUERY-IO* ':TYI))
	(COND ((= CHAR #/C-Q)			;quoting character.
	       (VECTOR-PUSH-EXTEND (SEND *QUERY-IO* ':TYI) LINE)
	       (WHEN HACK
		 (VECTOR-PUSH-EXTEND #/X HACK)
		 (SEND *QUERY-IO* ':TYO #/X)))
	      ((= CHAR #/RUBOUT)
	       (WHEN (ZEROP (FILL-POINTER LINE))
		 (GO FLUSH))
	       (VECTOR-POP LINE)
	       (WHEN HACK
		 (VECTOR-POP HACK)
		 (MULTIPLE-VALUE-BIND (X Y)
		     (SEND *QUERY-IO* ':COMPUTE-MOTION HACK 0 NIL
				      START-X START-Y)
		   (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
		     (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES X Y CX CY))
		   (SEND *QUERY-IO* ':SET-CURSORPOS X Y))))
	      ((= CHAR #/CLEAR-INPUT)
	       (GO FLUSH))
	      ((AND (= CHAR #/SPACE)
		    (NOT UID-P))		;allow spaces in passwords
	       (WHEN ENABLE-CAPABILITIES
		 (VECTOR-PUSH-EXTEND #/* LINE 1);make sure we have room for extra element
		 (DOTIMES (I (1- (FILL-POINTER LINE)))
		   (SETF (AREF LINE (1+ I)) (AREF LINE (1- I))))
		 (SETF (AREF LINE 0) #/*)
		 (SETQ ENABLE-CAPABILITIES NIL))
	       (SETQ UID-P T
		     UID LINE
		     LINE (MAKE-STRING 30. ':FILL-POINTER 0))
	       (WHEN HACK
		 (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
		   (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
		 (SEND *QUERY-IO* ':SET-CURSORPOS START-X START-Y))
	       (FORMAT *QUERY-IO* "~A " UID)
	       (WHEN HACK (MULTIPLE-VALUE (START-X START-Y)
			    (SEND *QUERY-IO* ':READ-CURSORPOS))))
	      ((= CHAR #/RETURN)
	       (OR DIRECTORY-FLAG (FILE-HOST-USER-ID UID HOST))
	       (IF RECORD-PASSWORDS-FLAG
		   (PUSH `((,UID ,(SEND HOST ':NAME)) ,LINE) USER-HOST-PASSWORD-ALIST))
	       (WHEN HACK
		 (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
		   (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
		 (SEND *QUERY-IO* ':SET-CURSORPOS START-X START-Y)
		 (RETURN-ARRAY HACK))
	       (FRESH-LINE *QUERY-IO*)
	       (SEND *QUERY-IO* ':SEND-IF-HANDLES ':MAKE-COMPLETE)
	       (RETURN-FROM FILE-GET-PASSWORD UID LINE ENABLE-CAPABILITIES))
	      ((AND (= CHAR #/*)
		    (= (FILL-POINTER LINE) 0))
	       (SETQ ENABLE-CAPABILITIES T))		    
	      (( 0 (CHAR-BITS CHAR)) (BEEP))
	      (T (WHEN HACK
		   (VECTOR-PUSH-EXTEND #/X HACK)
		   (SEND *QUERY-IO* ':TYO #/X))
		 (VECTOR-PUSH-EXTEND CHAR LINE)))
	(GO L)
     FLUSH
	(WHEN HACK
	  (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
	    (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
	  (SEND *QUERY-IO* ':SET-CURSORPOS START-X START-Y)
	  (SETF (FILL-POINTER HACK) 0))
	(WHEN UID-P (RETURN-ARRAY (PROG1 LINE (SETQ LINE UID UID NIL))))
	(FORMAT *QUERY-IO* "Flushed.~&")
	(SETF (FILL-POINTER LINE) 0 UID-P NIL)
	(GO RESTART))))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFCONST *COPY-FILE-KNOWN-TEXT-TYPES* '(:LISP :TEXT :MIDAS :PALX :PATCH-DIRECTORY
					       :INIT :UNFASL :BABYL :XMAIL :MAIL :QWABL :DOC
					       "LPT" "XGP" "ULOAD")
  "Files whose names have these canonical types are normally copied as characters.")

))

; From file FORMAT.LISP PS:<L.IO> OZ:
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
		  (SEND *STANDARD-OUTPUT* ':STRING-OUT "#\")
		  (PRIN1 CHNAME))
		 (T (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
		    (AND (BIT-TEST 8 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Hyper-"))
		    (AND (BIT-TEST 4 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Super-"))
		    (AND (BIT-TEST 1 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Control-"))
		    (AND (BIT-TEST 2 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Meta-"))
		    (SEND *STANDARD-OUTPUT* ':STRING-OUT "Mouse-")
		    (SEND *STANDARD-OUTPUT* ':STRING-OUT (NTH (LDB 0003 ARG)
							      '("Left" "Middle" "Right")))
		    (IF (SETQ CHNAME (NTH (SETQ BITS (LDB 0303 ARG))
					  '("" "-Twice" "-Thrice")))
			(SEND *STANDARD-OUTPUT* ':STRING-OUT CHNAME)
		      (SEND *STANDARD-OUTPUT* ':TYO #/-)
		      (ENGLISH-PRINT (1+ BITS))
		      (SEND *STANDARD-OUTPUT* ':STRING-OUT "-times")))))
          ((NOT COLON-FLAG)
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   ;; If @ flag or if control bits, we want to use characters' names.
	   (IF (OR ATSIGN-FLAG (NOT (ZEROP BITS)))
	       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (LDB %%KBD-CHAR ARG))))
	   ;; Print an appropriate reader macro if @C.
	   (IF ATSIGN-FLAG (PRINC "#\"))
	   (IF (NOT (ZEROP BITS))
	       ;; For efficiency, don't send :string-out message just for null string.
	       (SEND *STANDARD-OUTPUT*
		     ':STRING-OUT
		     (NTH BITS
			  '("" "c-" "m-" "c-m-"
			    "s-" "c-s-" "m-s-" "c-m-s-"
			    "h-" "c-h-" "m-h-" "c-m-h-"
			    "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
	   (IF CHNAME
	       (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		 (LET ((STR (STRING-DOWNCASE CHNAME)))
		   (SETF (AREF STR 0) (CHAR-UPCASE (AREF STR 0)))
		   (SEND *STANDARD-OUTPUT* ':STRING-OUT STR)
		   (RETURN-ARRAY STR)))
	     (AND ATSIGN-FLAG
		  (NOT (ZEROP BITS))
		  (IF ( #/a (LDB %%KBD-CHAR ARG) #/z)
		      (SEND *STANDARD-OUTPUT* ':STRING-OUT "sh-")
		    (IF (SI:CHARACTER-NEEDS-QUOTING-P (LDB %%KBD-CHAR ARG))
			(TYO (SI:PTTBL-SLASH READTABLE))
		      (SETQ ARG (CHAR-DOWNCASE ARG)))))
	     (SEND *STANDARD-OUTPUT* ':TYO (LDB %%KBD-CHAR ARG))))
	  (T
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   (AND (BIT-TEST 8 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Hyper-"))
	   (AND (BIT-TEST 4 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Super-"))
	   (AND (BIT-TEST 1 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Control-"))
	   (AND (BIT-TEST 2 BITS) (SEND *STANDARD-OUTPUT* ':STRING-OUT "Meta-"))
	   (SETQ ARG (LDB %%KBD-CHAR ARG))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		    (LET ((STR (STRING-DOWNCASE CHNAME)))
		      (SETF (AREF STR 0) (CHAR-UPCASE (AREF STR 0)))
		      (SEND *STANDARD-OUTPUT* ':STRING-OUT STR)
		      (RETURN-ARRAY STR)))
		  (AND ATSIGN-FLAG (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND ATSIGN-FLAG (< ARG 40) ( ARG #/))
		  (SEND *STANDARD-OUTPUT* ':TYO ARG)
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
		 ((AND ( #/a ARG #/z)
		       (NOT (ZEROP BITS)))
		  (SEND *STANDARD-OUTPUT* ':STRING-OUT "Shift-")
		  (SEND *STANDARD-OUTPUT* ':TYO (CHAR-UPCASE ARG)))
                 (T (SEND *STANDARD-OUTPUT* ':TYO ARG))))))

))

; From file FORMAT.LISP PS:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-DECIMAL (ARG PARAMS &OPTIONAL (BASE 10.)	;Also called for octal
			   &AUX (*NOPOINT T)
			        (*PRINT-RADIX* NIL)
			        (WIDTH (FIRST PARAMS))
				(PADCHAR (SECOND PARAMS))
				(COMMACHAR (THIRD PARAMS))
				(PLUS-P (AND ATSIGN-FLAG (NUMBERP ARG) (NOT (MINUSP ARG)))))
  (SETQ PADCHAR (COND ((NULL PADCHAR) #/SPACE)
		      ((NUMBERP PADCHAR) PADCHAR)
		      (T (AREF (STRING PADCHAR) 0))))
  (SETQ COMMACHAR (COND ((NULL COMMACHAR) #/,)
			((NUMBERP COMMACHAR) COMMACHAR)
			(T (AREF (STRING COMMACHAR) 0))))
  (AND WIDTH (FORMAT-CTL-JUSTIFY WIDTH
				 (+ (IF (FIXNUMP ARG)
					(+ (LOOP FOR X = (ABS ARG) THEN (FLOOR X BASE)
						 COUNT T
						 UNTIL (< X BASE))
					   (IF (MINUSP ARG) 1 0))
					(FLATC ARG))
				    (IF PLUS-P 1 0)
				    (IF (AND COLON-FLAG (FIXP ARG))
					(FLOOR (1- (FLATC (ABS ARG))) 3)   ;Number of commas
					0))
				 PADCHAR))
  (AND PLUS-P (SEND *STANDARD-OUTPUT* ':TYO #/+))
  (COND ((AND COLON-FLAG (FIXP ARG))
	;; Random hair with commas.  I'm not going to bother not consing.
	 (COND ((MINUSP ARG) (SEND *STANDARD-OUTPUT* ':TYO #/-) (SETQ ARG (- ARG))))
	   (SETQ ARG (NREVERSE (INHIBIT-STYLE-WARNINGS ;Give up!
				 (EXPLODEN ARG))))
	   (DO ((L ARG (CDR L))
		(I 2 (1- I)))
	       ((NULL (CDR L)))
	     (COND ((ZEROP I)
		    (RPLACD L (CONS COMMACHAR (CDR L)))
		    (SETQ I 3 L (CDR L)))))
	   (DOLIST (CH (NREVERSE ARG))
	     (SEND *STANDARD-OUTPUT* ':TYO CH)))
	((TYPEP ARG ':FIXNUM) (SI:PRINT-FIXNUM ARG *STANDARD-OUTPUT*))
	;; This is PRINC rather than PRIN1 so you can have a string instead of a number
	(T (PRINC ARG))))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN GET-FRAME-FUNCTION-AND-ARGS (SG FRAME &AUX FUNCTION NARGS-SPREAD
				    (RP (SG-REGULAR-PDL SG))
				    LEXPR-CALL REST-ARG-EXPECTED REST-ARG-VALUE ANS)
  "Return a list describing what was being executed in frame FRAME in SG.
The car of the list is a function name.
The cdr is a list of arguments (but if the arguments have
been modified, we can only get the latest values)."
  (SETQ FUNCTION (RP-FUNCTION-WORD RP FRAME)
	NARGS-SPREAD (SG-NUMBER-OF-SPREAD-ARGS SG FRAME))
  (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-EXPECTED LEXPR-CALL)
    (SG-REST-ARG-VALUE SG FRAME))
  ;; Analyze the function
  (SETQ FUNCTION (FUNCTION-NAME FUNCTION))
  (OR (SG-FRAME-ACTIVE-P SG FRAME) (BARF "This is not an active frame."))
  ;; Get the spread args.
  (DO ((I NARGS-SPREAD (1- I)))				;Cons them up in reverse order
      ((ZEROP I))
    (SETQ ANS (CONS (AREF RP (+ FRAME I)) ANS)))	;+1 -1
  ;; NCONC the rest arg if any was supplied separately from the regular args
  (AND REST-ARG-EXPECTED
       (SETQ ANS (NCONC ANS (COPY-LIST REST-ARG-VALUE))))
  (CONS FUNCTION ANS))

))

; From file EH.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN SG-NUMBER-OF-SPREAD-ARGS (SG FRAME &AUX
				    (RP (SG-REGULAR-PDL SG))
				    (AP FRAME)
				    (NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))
				    (FUNCTION (RP-FUNCTION-WORD RP AP))
				    ARGS-INFO REST-ARG-P NARGS-EXPECTED)		    
  "Returns the number of spread args present in FRAME in SG.
/"Spread/" args means that the elements of a rest arg normally do not count."
  (WHEN (LEGITIMATE-FUNCTION-P FUNCTION)
    (SETQ ARGS-INFO (ARGS-INFO FUNCTION))
    (SETQ REST-ARG-P (LDB-TEST 2402 ARGS-INFO))
    (SETQ NARGS-EXPECTED (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
  ;; The args that can be asked for are the ones supplied,
  ;; except that FEFs make slots for all args they expect whether supplied or not,
  ;; and if there is a rest arg it any unexpected spread args
  ;; are considered to be part of that.
  (IF (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
      (IF REST-ARG-P NARGS-EXPECTED
	(MAX (1- (RP-LOCAL-BLOCK-ORIGIN RP AP)) NARGS-EXPECTED NARGS-SUPPLIED))
    (IF REST-ARG-P
	(MIN NARGS-SUPPLIED NARGS-EXPECTED)
      NARGS-SUPPLIED)))

))

;;;;;;;;;;;;;;;;;;;; (eq stream nil) => *standard-input* ;;;;;;;;;;;;;;;;;;;;
; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-CHAR (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  "Read one character from STREAM, and return it as a character object.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  RECURSIVE-P
  (LET ((VALUE (SEND (IF (EQ STREAM T) *TERMINAL-IO*
		       (IF (EQ STREAM NIL) *STANDARD-INPUT*
			 STREAM))
		     ':TYI EOF-ERRORP)))
    (IF (NULL VALUE) EOF-VALUE
      (%MAKE-POINTER DTP-CHARACTER VALUE))))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN UNREAD-CHAR (CHAR &OPTIONAL (STREAM *STANDARD-INPUT*))
  "Put CHAR back in STREAM to be read out again as the next input character.
CHAR must be the same character last read from STREAM,
or this may not work or might even signal an error."
  (SEND (IF (EQ STREAM T) *TERMINAL-IO*
	  (IF (EQ STREAM NIL) *STANDARD-INPUT*
	    STREAM))
	':UNTYI (+ 0 CHAR)))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-BYTE (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE)
  "Read one byte from STREAM, and return it.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE."
  (OR (SEND (IF (EQ STREAM T) *TERMINAL-IO*
	      (IF (EQ STREAM NIL) *STANDARD-INPUT*
		STREAM))
	    ':TYI EOF-ERRORP)
      EOF-VALUE))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN PEEK-CHAR (&OPTIONAL PEEK-TYPE (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE
		  RECURSIVE-P)
  "Peek ahead at input from STREAM without discarding it.
The character peeked at is returned as a character object.
If PEEK-TYPE is NIL, peek at the next input character on STREAM,
 but leave it in the input stream so the next input will reread it.
If PEEK-TYPE is T, discard all whitespace chars and peek at first non-whitespace.
 The current readtable says what is whitespace.
Otherwise, discard all chars before the first one that is equal to PEEK-TYPE,
 which should be a number or a character.
EOF-ERRORP and EOF-VALUE are as for READ-CHAR."
  RECURSIVE-P
  (SETQ STREAM (IF (EQ STREAM T) *TERMINAL-IO*
		 (IF (EQ STREAM NIL) *STANDARD-INPUT*
		   STREAM)))
  (COND ((NULL PEEK-TYPE)
	 (LET ((VALUE (SEND STREAM ':TYI)))
	   (IF (NULL VALUE)
	       (IF EOF-ERRORP
		   (FERROR 'SYS:END-OF-FILE-1 "End of file encountered on stream ~S." STREAM)
		 EOF-VALUE)
	     (SEND STREAM ':UNTYI VALUE)
	     (%MAKE-POINTER DTP-CHARACTER VALUE))))
	(T
	 (DO ((WHITESPACE-CODE
		(CDR (GET (LOCF (RDTBL-PLIST READTABLE)) 'WHITESPACE))))
	     (())
	   (LET ((VALUE (SEND STREAM ':TYI)))
	     (IF (NULL VALUE)
		 (IF EOF-ERRORP
		     (FERROR 'SYS:END-OF-FILE-1 "End of file encountered on stream ~S." STREAM)
		   (RETURN EOF-VALUE))
	       (WHEN (COND ((EQ PEEK-TYPE T)
			    (OR ( (LDB %%CH-CHAR VALUE) VALUE)
				( (RDTBL-CODE READTABLE VALUE)
				   WHITESPACE-CODE)))
			   (T
			    (= PEEK-TYPE VALUE)))
		 (SEND STREAM ':UNTYI VALUE)
		 (RETURN (%MAKE-POINTER DTP-CHARACTER VALUE)))))))))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN LISTEN (&OPTIONAL (STREAM *STANDARD-INPUT*))
  "T if input is available on STREAM.
On a noninteractive stream, this is T if not at EOF."
  (SEND (IF (EQ STREAM T) *TERMINAL-IO*
	  (IF (EQ STREAM NIL) *STANDARD-INPUT*
		  STREAM))
	  ':LISTEN))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN CLEAR-INPUT (&OPTIONAL (STREAM *STANDARD-INPUT*))
  "Discard any buffered input on STREAM, if it is an interactive stream."
  (SEND (IF (EQ STREAM T) *TERMINAL-IO*
	  (IF (EQ STREAM NIL) *STANDARD-INPUT*
	    STREAM))
	':CLEAR-INPUT)
  NIL)

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-CHAR-NO-HANG (&OPTIONAL (STREAM *STANDARD-INPUT*)
			  (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  "Read one character from STREAM, and return it as a character object, but don't wait.
On an interactive stream, if no input is currently buffered, NIL is returned.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  RECURSIVE-P
  (CONDITION-CASE-IF (NOT EOF-ERRORP) ()
      (LET ((VALUE (SEND (IF (EQ STREAM T) *TERMINAL-IO*
			   (IF (EQ STREAM NIL) *STANDARD-INPUT*
			     STREAM))
			 ':TYI-NO-HANG T)))
	(IF (NULL VALUE) NIL
	  (%MAKE-POINTER DTP-CHARACTER VALUE)))
    (END-OF-FILE
     EOF-VALUE)))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-LINE (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P
		  				      OPTIONS)
  "Read a line from STREAM and return it as a string.
The string does not include the final Newline character, and is empty if nothing was read.
The second value is T if the line was terminated by EOF.
EOF-ERROR-P says whether an error should be signalled if eof occurs at the start of
 the line. If it is NIL and eof occurs at the start of the line, we return EOF-VALUE and T
RECURSIVE-P is ignored.
If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used."
  (DECLARE (VALUES LINE EOF-FLAG))
  RECURSIVE-P
  (MULTIPLE-VALUE-BIND (STRING EOF-FLAG)
      (READ-DELIMITED-STRING '(#/RETURN #/END)
			     (IF (EQ STREAM T) *TERMINAL-IO*
			       (IF (EQ STREAM NIL) *STANDARD-INPUT*
				 STREAM))
			     EOF-ERROR-P
			     OPTIONS)
    (IF (AND EOF-FLAG (ZEROP (LENGTH STRING)))
	(VALUES EOF-VALUE T)
      (VALUES STRING EOF-FLAG))))

))

; From file QIO.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-DELIMITED-STRING (&OPTIONAL (DELIMITER #/END) (STREAM *STANDARD-INPUT*)
			      EOF RH-OPTIONS (BUFFER-SIZE 100.))
  "Reads input from STREAM until DELIMITER is found; returns a string.
Uses the rubout handler if STREAM supports that.
DELIMITER is either a character or a list of characters.
 (Characters may be fixnums or character objects).
Values are:
 The string of characters read, not including the delimiter
 T if input ended due to end of file
 The delimiter character read (as a fixnum), or NIL if ended at EOF.
EOF if non-NIL means get error on end of file before any input is got.
RH-OPTIONS are passed to WITH-INPUT-EDITING.
BUFFER-SIZE is the size to make the buffer string, initially."
  (DECLARE (VALUES STRING EOF-FLAG DELIMITER))
  (with-stack-list (activation ':activation
			       (if (consp delimiter) 'memq 'eq)
			       delimiter)
    (with-stack-list* (options activation rh-options)
      (with-input-editing (stream options)
	(DO ((BUFFER (MAKE-ARRAY BUFFER-SIZE ':TYPE ART-STRING ':FILL-POINTER 0)))
	    (())
	  (LET ((CH (SEND STREAM (IF RUBOUT-HANDLER ':ANY-TYI ':TYI)
			  (AND (ZEROP (LENGTH BUFFER)) EOF))))
	    (COND ((NULL CH)
		   (RETURN BUFFER T))
		  ((CONSP CH)
		   (WHEN (EQ (CAR CH) ':ACTIVATION)
		     (SEND STREAM ':TYO (CADR CH))
		     (RETURN BUFFER NIL (CADR CH))))
		  ((AND (NOT RUBOUT-HANDLER)
			(IF (CONSP DELIMITER) (MEMQ CH DELIMITER) (EQ CH DELIMITER)))
		   (RETURN BUFFER NIL CH))
		  (T
		   (VECTOR-PUSH-EXTEND CH BUFFER)))))))))

))

;;;;;;;;;;;;;;;;;;;; rubout handler. Yow! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (documentation 'rubout-handler 'variable)
  "Bound to stream which is inside rubout-handler, or NIL if none.")

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(defmethod (stream-mixin :rubout-handler) (options function &rest args)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq ':nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
	(apply function args))
    (let ((rubout-handler-options options))
      (if ( (rhb-fill-pointer) (rhb-scan-pointer))
	  (setf (rhb-fill-pointer) 0)
	(copy-array-portion rubout-handler-buffer (rhb-scan-pointer) (rhb-fill-pointer)
			    rubout-handler-buffer 0 (array-length rubout-handler-buffer))
	(if (numberp (rhb-typein-pointer))
	    (decf (rhb-typein-pointer) (rhb-scan-pointer)))
	(decf (rhb-fill-pointer) (rhb-scan-pointer)))
      (setf (rhb-scan-pointer) 0 (rhb-status) ':initial-entry)
      (*catch 'return-from-rubout-handler
	(let (prompt-starting-x prompt-starting-y
	      rubout-handler-starting-x rubout-handler-starting-y
	      (rubout-handler self)
	      (rubout-handler-inside self)
	      (rubout-handler-re-echo-flag nil)
	      (rubout-handler-activation-character nil))
	  (multiple-value (prompt-starting-x prompt-starting-y) (send self ':read-cursorpos))
	  (setq rubout-handler-starting-x prompt-starting-x
		rubout-handler-starting-y prompt-starting-y)
	  (do-forever
	    (setq rubout-handler-re-echo-flag nil)
	    (*catch 'rubout-handler			;Throw here when rubbing out
	      (condition-case (error)
		  (return
		   (multiple-value-prog1
		     (apply function args)		;Call READ or whatever.
		     (setf (rhb-fill-pointer) (rhb-scan-pointer))
		     (and (rhb-typein-pointer)
			  (> (rhb-typein-pointer) (rhb-fill-pointer))
			  (setf (rhb-typein-pointer) (rhb-fill-pointer)))))
		(sys:parse-error
		 (send self ':fresh-line)
		 (princ ">>ERROR: " self)
		 (send error ':report self)
		 (send self ':fresh-line)
		 (setq rubout-handler-re-echo-flag t)
		 (do-forever (send self ':tyi)))))	;If error, force user to rub out
	    ;;Maybe return when user rubs all the way back
	    (and (zerop (rhb-fill-pointer))
		 (let ((full-rubout-option (assq ':full-rubout rubout-handler-options)))
		   (when full-rubout-option
		     ;; Get rid of the prompt, if any.
		     (sheet-clear-between-cursorposes
		       self prompt-starting-x prompt-starting-y
		       (- cursor-x left-margin-size) (- cursor-y top-margin-size))
		     (sheet-set-cursorpos self prompt-starting-x prompt-starting-y)
		     (return nil (cadr full-rubout-option)))))))))))

))

; From file STREAM.LISP PS:<MLY.L> OZ:
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
	(with-stack-list (initial-input ':initial-input
					(if (consp typeahead) (car typeahead)))
	  (with-stack-list (initial-input-pointer ':initial-input-pointer
					  (if (consp typeahead) (cadr typeahead)))
	    (with-stack-list* (options initial-input
				       initial-input-pointer
				       '((:full-rubout :full-rubout))
				       options)
	      (unless (consp typeahead) (setq options (cddr options)))
	      (lexpr-funcall-self ':rubout-handler options FUN ARGS)))))
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
      (when PROMPT 
	(SETQ OPTIONS (REMQ PROMPT OPTIONS))
	;This next condition may be unnecessary, but just in case. --kmp
	(unless (NOT (ASSQ ':REPROMPT OPTIONS))
	  ;; make fake reprompt info. our old prompt should still 
	  ;; be there --kmp
	  (PUSH `(:REPROMPT . ,(CDR PROMPT)) OPTIONS))))))

))

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :UNTYI) (CH)
  (IF (AND (eq RUBOUT-HANDLER self)
	   ;; RUBOUT-HANDLER added as conjunct 6/1/83
	   ;; to avoid lossage entering editor rubout handler
	   ;; by typing (= 1 2) then stray ) while inside BREAK.
	   ( 1 (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
	   (EQ CH (AREF RUBOUT-HANDLER-BUFFER (1- (RHB-SCAN-POINTER)))))
      (DECF (RHB-SCAN-POINTER))
    (IO-BUFFER-UNGET IO-BUFFER CH)))

))

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(defmethod (stream-mixin :save-rubout-handler-buffer) ()
  (when (eq rubout-handler-inside self)
    ;; Give rubout handler function a chance to put its internal data
    ;; into RUBOUT-HANDLER-BUFFER where we look for it.
    (let ((prop (get stream-mixin-rubout-handler 'save-rubout-handler-buffer)))
      (when prop (funcall prop self)))
    (values (copy-seq rubout-handler-buffer) (rhb-typein-pointer))))

))

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(defmethod (stream-mixin :restore-rubout-handler-buffer) (string &optional pointer)
  (let ((length (array-active-length string)))
    (or ( (array-length rubout-handler-buffer) length)
	(adjust-array-size rubout-handler-buffer length))
    (copy-array-contents string rubout-handler-buffer)
    (setf (rhb-fill-pointer) length))
  (setf (rhb-typein-pointer) pointer)
  (send self ':refresh-rubout-handler)
  (setf (rhb-scan-pointer) 0)
  (setf (rhb-status) ':restored)
  (*throw 'rubout-handler t))

))

; From file RH.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFUN-RH ALTERNATE-RUBOUT-HANDLER ()
  (LET ((CH) (CH-CHAR) (CH-CONTROL-META) (COMMAND)
	(FILL-POINTER (RH-FILL-POINTER))
	(TYPEIN-POINTER (RH-TYPEIN-POINTER))
	(STATUS (RHB-STATUS))
	(RUBBED-OUT-SOME NIL)
	(NUMERIC-ARG NIL)
	(NUMERIC-ARG-NEGATIVE NIL)
	(PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS))
	(INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS)))
	(INITIAL-INPUT-POINTER (CADR (ASSQ ':INITIAL-INPUT-POINTER RUBOUT-HANDLER-OPTIONS))))
    ;;Prompt if desired
    (SETF (RHB-STATUS) NIL)
    (WHEN (MEMQ STATUS '(:INITIAL-ENTRY :RESTORED))
      (WHEN PROMPT-OPTION
	(RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL))
      (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	(SEND SELF ':READ-CURSORPOS))
      ;; Output any "typeahead"
      (WHEN (PLUSP FILL-POINTER)
	(SEND SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
      ;; Kludge #1.  If this is the first time this rubout handler has been invoked
      ;; in this stream, then we must create the input history.
      (WHEN (AND (NOT (RH-INPUT-RING))
		 (FBOUNDP 'ZWEI:MAKE-HISTORY))
	(SETF (RH-INPUT-RING) (RH-MAKE-INPUT-RING)))
      ;; save the previous input on the input history,
      ;; unless the previous read said not to save it.
      (WHEN (AND (NOT (RH-DONT-SAVE-FLAG))
		 TYPEIN-POINTER
		 (EQ STATUS ':INITIAL-ENTRY)
		 (NOT (ZEROP TYPEIN-POINTER)))
	;; only add the contents if it is different from the last entry, and
	;; the entry is at least 2 characters long.
	(SETF (RH-FILL-POINTER) TYPEIN-POINTER)
	(WHEN (AND (> TYPEIN-POINTER 1)
		   (MISMATCH RUBOUT-HANDLER-BUFFER
			     (ZWEI:HISTORY-LATEST-ELEMENT (RH-INPUT-RING))))
	  (ZWEI:PUSH-ON-HISTORY (SUBSEQ RUBOUT-HANDLER-BUFFER 0 TYPEIN-POINTER)
				(RH-INPUT-RING)))
	(SETF (RH-FILL-POINTER) FILL-POINTER))
      ;; Then initialize the typein pointer.
      (SETF (RH-TYPEIN-POINTER) FILL-POINTER
	    TYPEIN-POINTER FILL-POINTER)
      ;; Gobble the initial input if any.
      (WHEN (AND INITIAL-INPUT (EQ STATUS ':INITIAL-ENTRY))
	(RH-INSERT-STRING INITIAL-INPUT 0 NIL NIL NIL)
	(SETQ FILL-POINTER (RH-FILL-POINTER))
	(SETQ TYPEIN-POINTER
	      (MAX (MIN (OR INITIAL-INPUT-POINTER TYPEIN-POINTER)
			(LENGTH RUBOUT-HANDLER-BUFFER))
		   0))
	(SETF (RHB-TYPEIN-POINTER) TYPEIN-POINTER)
	(RH-SET-POSITION TYPEIN-POINTER)
	(SETF (RHB-STATUS) ':RUBOUT RUBBED-OUT-SOME T))
      ;; Record whether this unit of input should be saved on the history.
      (SETF (RH-DONT-SAVE-FLAG)
	    (OR (CADR (ASSQ ':DONT-SAVE RUBOUT-HANDLER-OPTIONS))
		(CADR (ASSQ ':NO-INPUT-SAVE RUBOUT-HANDLER-OPTIONS)))))
;    ;;; Can this ever go off? :pass-though now only allows non-bucky. -- mly
;    ;; Kludge #5.  We can't echo or rub out a bucky char or a blip,
;    ;; so if the last char inserted was a either of those
;    ;; and it did not terminate the input, flush it.
;    (AND (NOT (ZEROP TYPEIN-POINTER))
;	 (OR (CONSP (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER)))
;	     (LDB-TEST %%KBD-CONTROL-META
;		       (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER))))
;	 (SETQ TYPEIN-POINTER (SETF (RH-TYPEIN-POINTER) (DECF (RH-FILL-POINTER)))))
    ;; Kludge #4.  After resuming a Break, the stream's cursorpos is wrong.
    ;; In fact, the cursor is at the end of the string in that case.
    ;; So, if it is supposed to be elsewhere, move it.
    ;; This condition also avoids wasting time when we are reading typein
    ;; at the end of the string.
    (OR (= FILL-POINTER TYPEIN-POINTER)
	(RH-CURSOR-MOTION TYPEIN-POINTER))
    ;; In case we had to return to the caller with a EDITING-COMMAND char
    ;; while RUBBED-OUT-SOME was T, make things consistent again
    ;; by causing a rescan now.
    (WHEN (AND RUBBED-OUT-SOME
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
	(SETQ CH (LET ((RUBOUT-HANDLER NIL)) (SEND SELF ':ANY-TYI)))
	(IF (CONSP CH)
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
			(EQ (CADR CH) #/MOUSE-3-1))
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
	       (COND (RUBBED-OUT-SOME
		      ;; Why isn't this done in the :RUBOUT-HANDLER method loop?
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER VALUE)
		      (SETF (RH-SCAN-POINTER) 0)
		      (*THROW 'RUBOUT-HANDLER T))
		     (T (*THROW 'RETURN-CHARACTER VALUE)))))
	    ;; Don't touch this character, just return it to caller.
	    ((OR (MEMQ CH EDITING-COMMAND)
		 (SI:ASSQ-CAREFUL CH EDITING-COMMAND))
	     ;; Cause rubout handler rescan next time the user does :TYI.
	     (IF RUBBED-OUT-SOME (SETF (RH-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
	     (RETURN CH))
	    ;; An standard rh editing command of some sort.  The RUBBED-OUT-SOME bit can only
	    ;; be cleared by entering this function again.  The function is passed the
	    ;; numeric argument, and returns T if we are going to need to throw out (like
	    ;; DIS-ALL in the editor).
	    (COMMAND
	     (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
		   *CURRENT-COMMAND-TYPE* NIL)
	     (SETQ RUBBED-OUT-SOME
		   (OR (FUNCALL (CDR COMMAND) (* (OR NUMERIC-ARG 1)
						 (IF NUMERIC-ARG-NEGATIVE -1 1)))
		       RUBBED-OUT-SOME))
	     (SETF (RHB-STATUS) (IF RUBBED-OUT-SOME ':RUBOUT))
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
	    ((= CH #/CONTROL-U)
	     (SETQ NUMERIC-ARG (* (OR NUMERIC-ARG 1) 4)))
	    ((AND (NOT (ZEROP CH-CONTROL-META)) (= CH-CHAR #/-))
	     (IF NUMERIC-ARG
		 (SEND SELF ':BEEP)
	       (SETQ NUMERIC-ARG-NEGATIVE (NOT NUMERIC-ARG-NEGATIVE))))
	    ;; Some other random control character -- beep and ignore
	    ((NOT (ZEROP CH-CONTROL-META))
	     (SEND SELF ':BEEP)
	     (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))
	    ;; Self-inserting character.  Set RUBBED-OUT-SOME since if we return,
	    ;; we were typing in the middle of the line.  Typing at the end of the
	    ;; line throws to RETURN-CHARACTER.
	    (T (UNLESS NUMERIC-ARG-NEGATIVE
		 (RH-INSERT-CHAR CH (OR NUMERIC-ARG 1) RUBBED-OUT-SOME)
		 (SETF RUBBED-OUT-SOME T (RHB-STATUS) ':RUBOUT))
	       (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
		     *CURRENT-COMMAND-TYPE* NIL
		     *RUBOUT-HANDLER-MARK* NIL)
	       (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))))))))

))

; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :UNTYI) (CH)
  (IF (EQ RUBOUT-HANDLER SELF)
      (DECF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1))
    (SETQ UNRCHF CH)))

))

; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYI) (&OPTIONAL IGNORE
							&AUX IDX (INHIBIT-SCHEDULING-FLAG T))
  (COND	((NEQ RUBOUT-HANDLER SELF)
	 (IF UNRCHF
	     (PROG1 UNRCHF (SETQ UNRCHF NIL))
	   (DO-FOREVER
	     (COLD-LOAD-STREAM-WAIT-FOR-CHAR)
	     (LET ((CHAR (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))
	       (SELECTQ CHAR
		 (NIL)				;Unreal character
		 (#/BREAK (BREAK "BREAK"))
		 ;; Kludge to make the debugger more usable in the cold-load stream
		 (#/ABORT (IF EH:READING-COMMAND (RETURN CHAR)
			    (SIGNAL EH:ABORT-OBJECT)))
		 (OTHERWISE (RETURN CHAR)))))))
	((> (fill-pointer RUBOUT-HANDLER-BUFFER)
	    (SETQ IDX (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1)))
	 (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) (1+ IDX))
	 (AREF RUBOUT-HANDLER-BUFFER IDX))
	(T
	 (COLD-LOAD-STREAM-RUBOUT-HANDLER))))

))


; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (COLD-LOAD-STREAM)
(DEFUN COLD-LOAD-STREAM-RUBOUT-HANDLER ()
  (declare (special tv:prompt-starting-x tv:prompt-starting-y))
  (WHEN (= (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) MOST-POSITIVE-FIXNUM)
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
       (INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS)))
       (status (array-leader rubout-handler-buffer 2) nil))
      (NIL)
    (setf (array-leader rubout-handler-buffer 2) nil)
    (when (memq status '(:initial-entry :restored))
      (multiple-value (tv:prompt-starting-x tv:prompt-starting-y)
	(send self ':read-cursorpos))
      (let ((prompt-option (assq ':prompt rubout-handler-options)))
	(when prompt-option
	  (tv:rubout-handler-prompt (cadr prompt-option) self nil)))
      (when initial-input
	(let ((length (length initial-input)))
	  (send self ':string-out initial-input)
	  (if (< (array-length rubout-handler-buffer) length)
	      (setq rubout-handler-buffer (array-grow rubout-handler-buffer
						      (+ length length))))
	  (copy-array-portion initial-input 0 length rubout-handler-buffer 0 length)
	  (setf (fill-pointer rubout-handler-buffer) length)
	  (setq rubbed-out-some t))))
    (SETQ CH (SEND SELF ':TYI))
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
	       (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) MOST-POSITIVE-FIXNUM))
	   (RETURN CH))
	  ((AND (NOT (OR (MEMQ CH DO-NOT-ECHO)
			 (MEMQ CH PASS-THROUGH)
			 (AND ACTIVATION-HANDLER
			      (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER)))))
		(OR (LDB-TEST %%KBD-CONTROL-META CH)
		    (MEMQ CH '(#/RUBOUT #/CLEAR-INPUT #/CLEAR-SCREEN #/DELETE))))
	   (COND
	     ((= CH #/CLEAR)			;CLEAR flushes all buffered input
	      (setf (fill-pointer RUBOUT-HANDLER-BUFFER) 0)
	      (SETQ RUBBED-OUT-SOME T)		;Will need to throw out
	      (SEND SELF ':TYO CH)		;Echo and advance to new line
	      (SEND SELF ':TYO #/CR))
	     ((OR (= CH #/FORM) (= CH #/DELETE));Retype buffered input
	      (SEND SELF ':TYO CH)		;Echo it
	      (IF (= CH #/FORM) (SEND SELF ':CLEAR-SCREEN) (SEND SELF ':TYO #/RETURN))
	      (LET ((PROMPT (CADR (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
				      (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))))
		(when prompt (tv:rubout-handler-prompt prompt self ch)))
	      (SEND SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
	     ((= CH #/RUBOUT)
	      (COND ((NOT (ZEROP (SETQ LEN (fill-pointer RUBOUT-HANDLER-BUFFER))))
		     (SETQ CURSOR-X (MAX 0 (- CURSOR-X CHAR-WIDTH)))
		     (SEND SELF ':CLEAR-EOL)
		     (setf (fill-pointer RUBOUT-HANDLER-BUFFER) (decf len))
		     (SETQ RUBBED-OUT-SOME T)
		     (COND ((ZEROP LEN)
			    (setf (array-leader RUBOUT-HANDLER-BUFFER 1) 0)
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
		    (SEND SELF ':TYO CH)
		    (ARRAY-PUSH-EXTEND RUBOUT-HANDLER-BUFFER CH))))
	   (COND ((AND (ATOM CH)
		       (LDB-TEST %%KBD-CONTROL-META CH)))	;do nothing
		 (RUBBED-OUT-SOME
		  (setf (array-leader RUBOUT-HANDLER-BUFFER 1) 0)
		  (*THROW 'RUBOUT-HANDLER T))
		 (T
		  (setf (array-leader RUBOUT-HANDLER-BUFFER 1)
			(fill-pointer RUBOUT-HANDLER-BUFFER))
		  (SETQ COLD-LOAD-STREAM-ACTIVATION-CHARACTER NIL)
		  (RETURN CH))))))))

))

; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :RUBOUT-HANDLER)
		     (options FUNCTION &REST ARGS)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq ':nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
	(apply function args))
    (let ((rubout-handler-options options))
      (setf (fill-pointer RUBOUT-HANDLER-BUFFER) 0)
      (setf (array-leader RUBOUT-HANDLER-BUFFER 1) 0)
      (setf (array-leader rubout-handler-buffer 2) ':initial-entry)
      (let (tv:prompt-starting-x tv:prompt-starting-y)
	(declare (special tv:prompt-starting-x tv:prompt-starting-y))
	(*CATCH 'TV:RETURN-FROM-RUBOUT-HANDLER
	  (DO ((RUBOUT-HANDLER self)		;Establish rubout handler
	       (INHIBIT-SCHEDULING-FLAG T)		;Make sure all chars come here
	       (COLD-LOAD-STREAM-ACTIVATION-CHARACTER NIL))
	      (NIL)
	    (*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
	      (CONDITION-CASE (ERROR)
		  (RETURN (APPLY FUNCTION ARGS))	;Call read type function
		(PARSE-ERROR
		 (send self ':fresh-line)
		 (PRINC ">>ERROR: " SELF)
		 (SEND ERROR ':REPORT SELF)
		 (send self ':fresh-line)
		 (SEND SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER)	;On error, retype buffered
		 (do-forever (SEND SELF ':TYI)))))		;and force user to edit it
	    ;;Maybe return when user rubs all the way back
	    (AND (ZEROP (fill-pointer RUBOUT-HANDLER-BUFFER))
		 (LET ((FULL-RUBOUT-OPTION (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS)))
		   (WHEN FULL-RUBOUT-OPTION
		     ;; Get rid of the prompt, if any.
		     (SEND self ':SET-CURSORPOS tv:PROMPT-STARTING-X tv:PROMPT-STARTING-Y)
		     (SEND self ':CLEAR-EOL)
		     (RETURN NIL (CADR FULL-RUBOUT-OPTION)))))))))))

))


; From file COLD.LISP PS:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :READ-CURSORPOS) (&OPTIONAL (UNITS ':PIXEL)
					       &AUX (X CURSOR-X) (Y CURSOR-Y))
  (IF (EQ UNITS ':CHARACTER)
      (VALUES (CEILING X CHAR-WIDTH)
	      (CEILING Y LINE-HEIGHT))
    (VALUES X Y)))

))

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :ANY-TYI) (&OPTIONAL IGNORE &AUX IDX)
  (COND ((> (RHB-FILL-POINTER) (SETQ IDX (RHB-SCAN-POINTER)))
	 (SETF (RHB-SCAN-POINTER) (1+ IDX))
	 (OR (AREF RUBOUT-HANDLER-BUFFER IDX)
	     (FERROR NIL "EOF on input from a window.")))
	((neq RUBOUT-HANDLER self)
;	 (SETF (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
	 (LET ((CHAR
		 (COND ((KBD-IO-BUFFER-GET IO-BUFFER T))
		       (T
			(FUNCALL-SELF ':NOTICE ':INPUT-WAIT)
			(KBD-IO-BUFFER-GET IO-BUFFER)))))
	   (IF (AND (eq RUBOUT-HANDLER-INSIDE self)
		    (EQ OLD-TYPEAHEAD T)
		    (CONSP CHAR)
		    (NEQ (CAR CHAR) 'REDISPLAY-RUBOUT-HANDLER))
	       ;; If inside the rubout handler in a :PREEMPTABLE-READ
	       ;; and we just got a blip that isn't intended for the rubout handler.
	       (PROGN
		 (MULTIPLE-VALUE-BIND (STRING INDEX)
		     (SEND SELF ':SAVE-RUBOUT-HANDLER-BUFFER)
		   (SETQ OLD-TYPEAHEAD (LIST STRING INDEX)))
		 ;; Save the text, rub it all out, and unread the blip.
		 ;; The :FULL-RUBOUT option will cause the RH to return to the caller
		 ;; who will then read the blip.
		 (FUNCALL-SELF ':UNTYI CHAR)
		 #/CLEAR)
	     CHAR)))
	(T
	 (OR (FUNCALL STREAM-MIXIN-RUBOUT-HANDLER)
	     (FERROR NIL "EOF on input from a window.")))))

))

; From file STREAM.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :ANY-TYI-NO-HANG) (&OPTIONAL IGNORE)
  (if (neq rubout-handler self)
      (KBD-IO-BUFFER-GET IO-BUFFER T)
    (FERROR NIL ":ANY-TYI-NO-HANG from inside a rubout handler.")))

))

; From file RH.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; RH  "

(DEFINE-RH-COMMAND RH-COM-DISPLAY-INTERNAL-STATE (#/CONTROL-META-HELP) (IGNORE)
  (RH-DISPLAY-INFO
    (FORMAT SELF "Terminal I//O:~17T~S~%" TERMINAL-IO)
    (FORMAT SELF "Pointer Values:~17T~
		  Fill pointer = ~D, Scan pointer = ~D, Typein pointer = ~D~%"
	    (RH-FILL-POINTER) (RH-SCAN-POINTER) (RH-TYPEIN-POINTER))
    (FORMAT SELF "Rubout handler buffer size = ~D, Status = ~S~%"
	    (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER) (RHB-STATUS RUBOUT-HANDLER-BUFFER))
    (FORMAT SELF "Options:~17T~S" RUBOUT-HANDLER-OPTIONS)))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :RUBOUT-HANDLER) (options FUNCTION
						  &REST ARGS &AUX TEM
						  (*WINDOW* *STREAM-SHEET*)
						  *STREAM-DEFER-OUTPUT-NOT-AT-END*
						  COMMAND-POINT)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq ':nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
	(apply function args))
    (let ((rubout-handler-options options))
      (IF *STREAM-COMMAND-POINT*
	  (PROGN
	    (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
	    (SETQ COMMAND-POINT *STREAM-COMMAND-POINT*)
	    (FLUSH-BP COMMAND-POINT)
	    (SETQ *STREAM-COMMAND-POINT* NIL))
	(MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*))))
      (LET ((PROMPT-OPTION (ASSQ ':PROMPT rubout-handler-options)))
	(WHEN PROMPT-OPTION
	  (IF (STRINGP (CADR PROMPT-OPTION))
	      (PRINC (CADR PROMPT-OPTION) SELF)
	    (FUNCALL (CADR PROMPT-OPTION) SELF NIL))
	  (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
      (LET ((INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT rubout-handler-options)))
	    (INITIAL-INPUT-POINTER (CADR (ASSQ ':INITIAL-INPUT-POINTER rubout-handler-options)))
	    (*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))	;need this in forward-char
	(WHEN INITIAL-INPUT
	  (INSERT *STREAM-START-BP* INITIAL-INPUT)
	  (OR COMMAND-POINT
	      (NOT INITIAL-INPUT-POINTER)
	      (SETQ COMMAND-POINT (FORWARD-CHAR *STREAM-START-BP* INITIAL-INPUT-POINTER)))))
      (STREAM-MAYBE-REDISPLAY)
      (*CATCH 'TV:RETURN-FROM-RUBOUT-HANDLER
	(DO ((RUBOUT-HANDLER self)			;Establish rubout handler
	     (*SRE-ACTIVATION-CHARACTER* NIL))
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
			  (multiple-value-prog1
			    (APPLY FUNCTION ARGS)
			    (LET ((*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
			      (DELETE-INTERVAL *STREAM-BP*
					       (INTERVAL-LAST-BP *INTERVAL*)))))))
	      (SYS:PARSE-ERROR
	       (LET ((*STREAM-DEFER-OUTPUT-NOT-AT-END* T))
		 (fresh-line SELF)
		 (PRINC ">>ERROR: " SELF)
		 (SEND ERROR ':REPORT SELF)
		 (fresh-line SELF))
	       (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
	       (MOVE-BP END-OF-MSG-BP *STREAM-START-BP*)
	       (MOVE-BP *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	       (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
	       (STREAM-REDISPLAY)
	       (do-forever (FUNCALL-SELF ':TYI)))))
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
	   (SETQ TEM (ASSQ ':FULL-RUBOUT rubout-handler-options))
	   (RETURN NIL (CADR TEM))))))))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
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
    (let ((editing-command (cdr (assq ':editing-command rubout-handler-options)))
	  (command-handler (cdr (assq ':command rubout-handler-options)))
	  (activation-handler (cdr (assq ':activation rubout-handler-options)))
	  (do-not-echo (cdr (assq ':do-not-echo rubout-handler-options)))
	  (pass-through (cdr (assq ':pass-though rubout-handler-options))))
      (IF (AND (NUMBERP CHAR)
	       (NOT (OR (MEMQ CHAR editing-command)
			(SI:ASSQ-CAREFUL CHAR editing-command)))
	       (NOT (AND command-handler
			 (APPLY (car command-handler)
				CHAR (cdr command-handler))))
	       (NOT (AND activation-handler
			 (APPLY (car activation-handler)
				CHAR (cdr activation-handler))))
	       (NOT (MEMQ CHAR DO-NOT-ECHO))
	       (OR (AND (OR (< CHAR 40)
			    (ALPHA-CHAR-P CHAR))
			(EQ 'COM-ORDINARILY-SELF-INSERT
			    (COMMAND-LOOKUP CHAR *STREAM-COMTAB*)))
		   (AND (< CHAR 400)
			(MEMQ CHAR pass-through))))
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
	    (*THROW 'RUBOUT-HANDLER T)))))))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFUN STREAM-PRE-COMMAND-HOOK-1 (CHAR)
  (let ((editing-command (cdr (assq ':editing-command rubout-handler-options)))
	(command-handler (cdr (assq ':command rubout-handler-options)))
	(activation-handler (cdr (assq ':activation rubout-handler-options)))
	(do-not-echo (cdr (assq ':do-not-echo rubout-handler-options))))
    (COND ((OR (MEMQ CHAR editing-command)
	       (SI:ASSQ-CAREFUL CHAR editing-command))
	   (SETQ *SRE-INPUT-END-BP* (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))
	   (SETQ *SRE-INPUT-POINT* (COPY-BP *SRE-STREAM-BP*))
	   (MOVE-BP *SRE-STREAM-BP* (INTERVAL-LAST-BP *INTERVAL*))
	   (*THROW 'RETURN-FROM-ANY-TYI CHAR))
	  ((AND command-handler
		(APPLY (car command-handler) CHAR (cdr command-handler)))
	   (SEND *SRE-WINDOW* ':SET-*STREAM-COMMAND-POINT*
		 (COPY-BP *SRE-STREAM-BP* ':NORMAL))
	   (MOVE-BP *SRE-STREAM-BP* *SRE-STREAM-START-BP*)
	   (*THROW 'TV:RETURN-FROM-RUBOUT-HANDLER
		   (VALUES
		     `(:COMMAND ,CHAR ,(OR *NUMERIC-ARG* 1))
		     ':COMMAND)))
	  ((OR (MEMQ CHAR do-not-echo)
	       (AND activation-handler
		    (APPLY (car activation-handler)
			   CHAR (cdr activation-handler))))
	   (SETQ *SRE-ACTIVATION-CHARACTER*
		 (IF (MEMQ CHAR do-not-echo) CHAR
		   `(:ACTIVATION ,CHAR ,(OR *NUMERIC-ARG* 1))))
	   (*THROW 'RUBOUT-HANDLER T)))
  ;; Tell label to change at redisplay after this command finishes,
  ;; so that if this command activates, there will be no change.
  (send *WINDOW* ':ENTER-EDITOR)
  ;; Tell the ibeam blinker to start blinking at next redisplay, also.
  (SETQ *STREAM-IBEAM-SHOULD-BLINK* T)))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFUN STREAM-COMMAND-HOOK-1 (CHAR)
  (COND ((BP-= (POINT) (INTERVAL-LAST-BP *INTERVAL*))
	 (OR *EDITOR-STREAM-ACTIVATION-NEEDED*
	     (NOT (OR (< CHAR 200)
		      (MEMQ CHAR '(#/TAB #/RETURN))
		      (AND (< CHAR 400)
			   (MEMQ CHAR (cdr (assq ':pass-through rubout-handler-options))))))
	     (EQ *LAST-COMMAND-TYPE* 'INDENT-NEW-LINE)
	     (COM-ACTIVATE)))			;Automatically activate
	(*EDITOR-STREAM-REQUIRE-ACTIVATION*
	 (SETQ *EDITOR-STREAM-ACTIVATION-NEEDED* T))))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFFLAVOR ZTOP-STREAM-MIXIN
	(*ZMACS-SG*
	 *ZTOP-SG*
	 *STREAM-START-BP*
	 (*STREAM-ACTIVATION-NEEDED* NIL)
	 (*RUBOUT-HANDLER-STATE* ':NORMAL))
	()
  (:REQUIRED-FLAVORS EDITOR-STREAM-FROM-WINDOW)
  (:INIT-KEYWORDS :BUFFER)
  (:INITABLE-INSTANCE-VARIABLES *ZMACS-SG*)
  (:GETTABLE-INSTANCE-VARIABLES *STREAM-START-BP*))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (ZTOP-STREAM-MIXIN :BEFORE :RUBOUT-HANDLER) (ignore &REST IGNORE)
  (SETQ *STREAM-ACTIVATION-NEEDED* NIL
	*ZTOP-PACKAGE* PACKAGE
;	rubout-handler-options args	;ARGS used to be the first arg supplied. Flushed.
	*RUBOUT-HANDLER-STATE*
	(IF (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	    ':NORMAL ':EDITING)
	*ZTOP-READING-INPUT* (EQ *RUBOUT-HANDLER-STATE* ':NORMAL)
	*ZTOP-EDITING* (NOT *ZTOP-READING-INPUT*)
	*ZTOP-ACTIVATION-NEEDED* NIL))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (ZTOP-STREAM-MIXIN :PRE-COMMAND-HOOK)
	   (CHAR &AUX
	    (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND rubout-handler-options)))
	    (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO rubout-handler-options)))
	    (COMMAND-HANDLER (cdr (ASSQ ':COMMAND rubout-handler-options)))
	    (ACTIVATION-HANDLER (cdr (ASSQ ':ACTIVATION rubout-handler-options))))
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
	      (APPLY (car COMMAND-HANDLER) CHAR (cdr COMMAND-HANDLER)))
	 (RESUME-ZTOP-SG
	   `(:COMMAND
	      (:COMMAND ,CHAR ,(OR *NUMERIC-ARG* 1))))
	 (SETQ *CURRENT-COMMAND-TYPE* 'EDITING-COMMAND)
	 (*THROW 'COMMAND-EXECUTE T))
	((OR (MEMQ CHAR DO-NOT-ECHO)
	     (AND ACTIVATION-HANDLER
		  (APPLY (car ACTIVATION-HANDLER)
			 CHAR (cdr ACTIVATION-HANDLER))))
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

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (ZTOP-STREAM-MIXIN :COMMAND-HOOK) (TYPE &AUX (OLD-STATE *RUBOUT-HANDLER-STATE*))
  (UNLESS (EQ TYPE 'EDITING-COMMAND)
    (AND (ASSQ ':FULL-RUBOUT rubout-handler-options) (BP-= *STREAM-START-BP* *STREAM-BP*)
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

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFUN STREAM-READ-ERROR-HANDLER (IGNORE &REST IGNORE)
  (when (eq rubout-handler self)
    (INSERT-INTERVAL *SRE-STREAM-BP* *SRE-STREAM-START-BP* *SRE-STREAM-BP*)
    (MUST-REDISPLAY *SRE-WINDOW* DIS-TEXT))
  NIL)

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :AROUND :LISTEN) (CONT MT ARGS)
  (OR (NOT (OR (neq rubout-handler self)
	       (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
      (AROUND-METHOD-CONTINUE CONT MT ARGS)))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :ANY-TYI-NO-HANG) (&OPTIONAL IGNORE)
  (when (eq rubout-handler self)
    (FERROR NIL ":ANY-TYI-NO-HANG while inside RUBOUT-HANDLER"))
  (TV:KBD-IO-BUFFER-GET TV:IO-BUFFER T))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :ANY-TYI) (&OPTIONAL IGNORE)
  (IF (eq rubout-handler self)
      ;; If input is being edited...
      (IF (AND (NULL *SRE-INPUT-POINT*)
	       (NOT (BP-= *STREAM-BP*
			  (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
	  ;; If we have more to fetch from the buffer, just fetch it.
	  (PROG1 (LDB %%CH-CHAR (BP-CHAR *STREAM-BP*))      ;Give buffered character if any
		 (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)	;We are moving point, so...
		 (LET ((*INTERVAL* INTERVAL))
		   (IBP *STREAM-BP*)))
	;; We have to get more input, probably invoking the editor.
	(LET ((*EDITOR-STREAM-ALREADY-KNOWS* T))
	  (SEND SELF ':STREAM-RUBOUT-HANDLER)))
    ;; If input is not being edited
    ;; Then we read it directly, as if were not an editor stream.
    ;; Make sure screen is right before we read it.
    ;; But if came from within the rubout handler, that is already done.
    (OR *EDITOR-STREAM-ALREADY-KNOWS*
	(STREAM-REDISPLAY))
    (IF (TV:KBD-IO-BUFFER-GET TV:IO-BUFFER T)
	(FUNCALL-SELF ':NOTICE ':INPUT-WAIT))
    (TV:KBD-IO-BUFFER-GET TV:IO-BUFFER)))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFMETHOD (EDITOR-STREAM-MIXIN :UNTYI) (CH)
  (IF (eq rubout-handler self)
      (LET ((*INTERVAL* (SEND *STREAM-SHEET* ':INTERVAL)))
	(DBP *STREAM-BP*))
    (TV:IO-BUFFER-UNGET TV:IO-BUFFER CH)))

))

; From file STREAM.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; STREAM  "

(DEFUN STREAM-MAYBE-REDISPLAY ()
  "Maybe redisplay the buffer; then return NIL if caller should update screen explicitly.
This function may redisplay and return T,
may refrain from redisplay and return T.
If explicit updating will work, always does nothing and returns NIL
because explicit updating is certainly fast."
  (DECLARE (:SELF-FLAVOR EDITOR-STREAM-MIXIN))
  (TV:PREPARE-SHEET (*STREAM-SHEET*) NIL)
  (COND (EDITOR-STREAM-DEFER-REDISPLAY
	 ;; If redisplay is deferred, don't do it, but arrange to do it later.
	 ;; Say we did redisplay, to prevent direct output.
	 (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
	 T)
	((NOT (WINDOW-READY-P *STREAM-SHEET* NIL))
	 (FUNCALL *STREAM-SHEET* ':PREPARE-FOR-REDISPLAY)
	 (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
	 (OR *STREAM-DEFER-OUTPUT-NOT-AT-END*
	     (STREAM-REDISPLAY))
	 T)
	((> (WINDOW-REDISPLAY-DEGREE *STREAM-SHEET*) DIS-BPS)
	 (OR *STREAM-DEFER-OUTPUT-NOT-AT-END*
	     (STREAM-REDISPLAY))
	 T)
	((AND (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
	      (neq rubout-handler self))	;Always redisplay on typein
	 ;; Turn off editor blinkers if faking redisplay
	 (DOLIST (BL (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*))
	   (TV:BLINKER-SET-VISIBILITY (CDR BL) NIL))
	 NIL)
	(T
	 (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
	 (OR *STREAM-DEFER-OUTPUT-NOT-AT-END*
	     (STREAM-REDISPLAY))
	 T)))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(COMPILER:MAKE-OBSOLETE PKG-CREATE-PACKAGE "use MAKE-PACKAGE instead")

))

(setf (documentation 'compiler:warn 'function)
  "Record and print a compiler warning.
TYPE describes the particular kind of problem, such as FUNCTION-NOT-VALID.
SEVERITY is a symbol in the keyword package giving a broader classification;
see the source for a list of possible severities.  FORMAT-STRING and ARGS
are used to print the warning.")


; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun fixup-type-properties ()
  (dolist (symbol
	    '(array atom bignum bit bit-vector character closure common
		    compiled-function complex cons double-float entity
		    fat-char fix fixnum flonum float hash-table instance integer keyword ;list
		    locative long-float microcode-function null number
		    package pathname random-state ratio rational readtable real
		    select select-method sequence short-float simple-array simple-bit-vector
		    simple-string simple-vector single-float small-flonum
		    standard-char stream string string-char structure symbol
		    vector))
    (when (get symbol 'type-predicate)
      (setf (get (intern (string symbol) 'keyword) 'type-predicate)
	    (get symbol 'type-predicate)))
    (when (get symbol 'type-optimizer)
      (setf (get (intern (string symbol) 'keyword) 'type-optimizer)
	    (get symbol 'type-optimizer)))
    (when (get symbol 'subtypes)
      (let ((combined (nconc (mapcar #'(lambda (elt) (intern (string elt) 'keyword))
				     (get symbol 'subtypes))
			     (get symbol 'subtypes))))
	(setf (get (intern (string symbol) 'keyword) 'subtypes) combined)
	(setf (get symbol 'subtypes) combined)))
    (when (get symbol 'type-expander)
      (setf (get (intern (string symbol) 'keyword) 'type-expander)
	    (get symbol 'type-expander)))
    (when (get symbol 'type-name)
      (setf (get (intern (string symbol) 'keyword) 'type-name)
	    (get symbol 'type-name))))
  (putprop ':list 'cons 'type-alias-for))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun type-pretty-name-1 (type)
  (cond ((symbolp type)
	 (if (or (get type 'type-name)
		 (string-append-a-or-an
		   (string-subst-char #/space #/- (string-downcase type) nil)))
	     (string-append "a " (string-downcase (format nil "~a" type)))))
	((member type '((integer 0) (float 0)))
	 (string-append "a positive " (string-downcase (symbol-name (car type)))))
	((and (eq (car type) 'or)
	      (dolist (elt (cdr type) t)
		(unless (type-pretty-name-1 elt) (return nil))))
	 (format:output nil
	   (do ((tail (cdr type) (cdr tail)))
	       ((null tail))
	     (unless (cdr tail)
	       (princ "or "))
	     (princ (type-pretty-name-1 (car tail)))
	     (when (cdr tail)
	       (if (cddr tail)
		   (princ ", ")
		 (tyo #/space))))))
	((eq (car type) 'member)
	 (format:output nil
	   (do ((tail (cdr type) (cdr tail)))
	       ((null tail))
	     (unless (cdr tail)
	       (princ "or "))
	     (prin1 (car tail))
	     (when (cdr tail)
	       (if (cddr tail)
		   (princ ", ")
		 (tyo #/space))))))))

))

;;;;;;;;;;;;;;;;;;;; complex canonicalization lossage ;;;;;;;;;;;;;;;;;;;;
; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun %complex (realpart imagpart)
  (let ((object
	  (%allocate-and-initialize dtp-extended-number dtp-header
				    (dpb %header-type-complex
					 %%header-type-field
					 0)
				    0 number-cons-area 3)))
    (setf (complex-real-part object) (+ realpart (* 0 imagpart)))
    (setf (complex-imag-part object) (+ imagpart (* 0 realpart)))
    object))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun numeric-two-arguments (code number1 number2 &aux function)
  (setq function (nth code '(*plus *dif *times *quo = > < *min *max *boole %div)))
  (cond ((and (complexp number1) (complexp number2))
	 (complex-two-arguments code number1 number2))
	((complexp number1)
	 (funcall function number1 (%complex number2 0)))
	((complexp number2)
	 (funcall function (%complex number1 0) number2))
	((floatp number1)
	 (funcall function number1 (float number2)))
	((floatp number2)
	 (funcall function (float number1) number2))
	((small-floatp number1)
	 (funcall function number1 (small-float number2)))
	((small-floatp number2)
	 (funcall function (small-float number1) number2))
	((and (rationalp number1) (rationalp number2))
	 (rational-two-arguments code number1 number2))
	((rationalp number1)
	 (funcall function number1 (rational number2)))
	((rationalp number2)
	 (funcall function (rational number1) number2))
	(t
	 (ferror nil "Arith two-arg op code ~S on ~S and ~S" code number1 number2))))

))

; From file RAT.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun denominator (rational)
  "Return the denominator of RATIONAL.  On integers, this returns 1."
  (check-arg-type rational rational)
  (if (integerp rational) 1
    (rational-denominator rational)))

))

; From file RAT.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun numerator (rational)
  "Return the numerator of RATIONAL.  On integers, this is the identity function."
  (check-arg-type rational :rational)
  (if (integerp rational) rational
    (rational-numerator rational)))

))

(setf (documentation 'nth-value 'function)
  "Returns the VALUE-NUMBER'th (0-based) value of EXP.
Compiles fast when VALUE-NUMBER is a constant.")


; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN INTERN-SOFT (SYM &OPTIONAL PKG &AUX HASH STR)
  "Like INTERN but returns NIL for all three values if no suitable symbol found.
Does not ever put a new symbol into the package."
  (DECLARE (VALUES SYMBOL ACTUALLY-FOUND-FLAG ACTUAL-PACKAGE))
  (COND ((NULL PKG) (SETQ PKG *PACKAGE*))
	((NOT (PACKAGEP PKG)) (SETQ PKG (PKG-FIND-PACKAGE PKG))))
  (IF (STRINGP SYM) (SETQ STR SYM)
    (IF (SYMBOLP SYM) (SETQ STR (GET-PNAME SYM))
      (SETQ STR (STRING SYM))))
  (SETQ HASH (PKG-STRING-HASH-CODE STR))
  (WITHOUT-INTERRUPTS
    (BLOCK INTERN
      ;; Search this package.
      (LET ((LEN (PKG-NUMBER-OF-SLOTS PKG))
	    X Y)
	(DO ((I (\ HASH LEN)))
	    ((NULL (SETQ X (PKG-SLOT-CODE PKG I)))
	     NIL)
	  (AND (PKG-CODE-VALID-P X)
	       (= HASH (PKG-CODE-HASH-CODE X))
	       (EQUAL STR (GET-PNAME (SETQ Y (PKG-SLOT-SYMBOL PKG I))))
	       (RETURN-FROM INTERN Y
			    (IF (PKG-CODE-EXTERNAL-P X)
				':EXTERNAL ':INTERNAL)
			    PKG))
	  (IF (= (INCF I) LEN) (SETQ I 0))))
      ;; Search USEd packages.
      (DOLIST (PKG (PKG-USE-LIST PKG))
	(LET ((LEN (PKG-NUMBER-OF-SLOTS PKG))
	      X Y)
	  (DO ((I (\ HASH LEN)))
	      ((NULL (SETQ X (PKG-SLOT-CODE PKG I)))
	       NIL)
	    (AND (PKG-CODE-VALID-P X)
		 (= HASH (PKG-CODE-HASH-CODE X))
		 (EQUAL STR (GET-PNAME (SETQ Y (PKG-SLOT-SYMBOL PKG I))))
		 (IF (PKG-CODE-EXTERNAL-P X)
		     (RETURN-FROM INTERN Y
				  ':INHERITED
				  PKG)
		   (RETURN)))			;Not inheritable from this package.
	    (IF (= (INCF I) LEN) (SETQ I 0))))))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO DEFVAR (VARIABLE . ARGS)
  "Define a special variable named VARIABLE, and initialize to INITIAL-VALUE if unbound.
Normally, reevaluating the DEFVAR does not change the variable's value.
But in patch files, and if you do C-Shift-E with no region on a DEFVAR,
the variable is reinitialized.  DOCUMENTATION is available if the user
asks for the documentation of the symbol VARIABLE.
If you want your variable to be initially unbound, yet have documentation, 
use :UNBOUND as the initial value."
  (DECLARE (ARGLIST VARIABLE &OPTIONAL INITIAL-VALUE DOCUMENTATION))
  `(PROGN (EVAL-WHEN (COMPILE)
	    (SPECIAL ,VARIABLE))
	  (EVAL-WHEN (LOAD EVAL)
	    (DEFVAR-1 ,VARIABLE . ,ARGS))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO DEFCONST (VARIABLE INITIAL-VALUE . ARGS)
  "Define a special variable which the program won't change but the user may.
It is set unconditionally to the value of INITIAL-VALUE.
DOCUMENTATION is available if the user asks for the documentation of the symbol VARIABLE."
  (DECLARE (ARGLIST VARIABLE INITIAL-VALUE &OPTIONAL DOCUMENTATION))
  `(PROGN (EVAL-WHEN (COMPILE)
	    (SPECIAL ,VARIABLE))
	  (EVAL-WHEN (LOAD EVAL)
	    (DEFCONST-1 ,VARIABLE ,INITIAL-VALUE . ,ARGS))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO DEFCONSTANT (VARIABLE INITIAL-VALUE . ARGS)
  "Define a special variable which will never be changed, and the compiler may assume so.
It is set unconditionally to the value of INITIAL-VALUE.
DOCUMENTATION is available if the user asks for the documentation of the symbol VARIABLE."
  (DECLARE (ARGLIST VARIABLE INITIAL-VALUE &OPTIONAL DOCUMENTATION))
  `(PROGN (EVAL-WHEN (COMPILE)
	    (SPECIAL ,VARIABLE))
	  (EVAL-WHEN (LOAD EVAL)
	    (DEFPROP ,VARIABLE T COMPILER:SYSTEM-CONSTANT))
	  (EVAL-WHEN (LOAD EVAL)
	    (DEFCONST-1 ,VARIABLE ,INITIAL-VALUE . ,ARGS))))

))

; From file RDDEFS.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; RDDEFS  "

(DEFSUBST DECODE-READ-ARG (ARG)
  (COND ((NULL ARG) *STANDARD-INPUT*)
	((EQ ARG T) *TERMINAL-IO*)
	(T ARG)))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method apply (function &rest args)
  (unless (and (consp function)
	       (memq (car function) '(function quote))
	       (eq (length function) 2)
	       (symbolp (cadr function)))
    (ferror 'sys:unknown-setf-reference
	    "In SETF of APPLY, the function APPLYed must be a constant."))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method (cons (cadr function) args))
    (if (eq (cadr function) 'aref)
	(setq storeform
	      `(aset ,(car (last storeform)) . ,(butlast (cdr storeform)))))
    (if (not (eq (car (last storeform)) (car (last tempvars))))
	(ferror 'sys:unknown-setf-reference
		"~S not acceptable within APPLY within SETF." function)
      (values tempvars tempargs storevars
	      `(apply #',(car storeform) . ,(cdr storeform))
	      `(apply #',(car refform) . ,(cdr refform))))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method send (function arg1 &rest args &aux operation)
  (cond ((keywordp arg1) (setq operation arg1))	;(send foo :bar)
	((and (consp arg1)			;(send foo ':bar)
	      (eq (car arg1) 'quote)
	      (symbolp (cadr arg1))
	      (eq (length arg1) 2))
	 (setq operation (cadr arg1)))
	(t (ferror 'unknown-setf-reference "Can only SETF message-sending SENDs.")))
  (let ((tempvars (list* (gensym) (mapcar #'(lambda (ignore) (gensym)) args)))
	(storevar (gensym)))
    (values tempvars (cons function args) (list storevar)
	    (if (eq operation ':get)
		`(send ,(car tempvars) ':putprop
		       ,storevar . ,(cdr tempvars))
	      `(send ,(car tempvars)
		     ;; replace this with a send of :SET operation in new vanilla flavor
		     ',(intern (string-append "SET-" operation) pkg-keyword-package)
		     ,storevar))
	    `(send ,(car tempvars) ,arg1 . ,(cdr tempvars)))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(setf (get 'funcall 'setf-method) (get 'send 'setf-method))
(setf (documentation 'funcall 'setf) (documentation 'send 'setf))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method lexpr-send (function arg1 &rest args &aux operation)
  (cond ((keywordp arg1) (setq operation arg1))	;(send foo :bar)
	((and (consp arg1)			;(send foo ':bar)
	      (eq (car arg1) 'quote)
	      (symbolp (cadr arg1))
	      (eq (length arg1) 2))
	 (setq operation (cadr arg1)))
	(t (ferror 'unknown-setf-reference "Can only SETF message-sending SENDs.")))
  (let ((tempvars (list* (gensym) (mapcar #'(lambda (ignore) (gensym)) args)))
	(storevar (gensym)))
    (values tempvars (cons function args) (list storevar)
	    (if (eq operation ':get)
		`(lexpr-send ,(car tempvars) ':putprop
			     ,storevar . ,(cdr tempvars))
	      `(lexpr-send ,(car tempvars)
			   ;; replace this with a send of :SET operation in new vanilla flavor
			   ',(intern (string-append "SET-" operation) pkg-keyword-package)
			   ,storevar))
	    `(lexpr-send ,(car tempvars) ,arg1 . ,(cdr tempvars)))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defsetf char set-aref)				;could be set-ar-1
(defsetf schar set-aref)			;could be set-ar-1
(defsetf bit set-aref)				;could be set-ar-1
(defsetf sbit set-aref)				;could be set-ar-1
(defsetf svref set-aref)			;could be set-ar-1
(defprop char aloc locf-method)			;could be ap-1
(defprop schar aloc locf-method)		;could be ap-1
(defprop bit aloc locf-method)			;could be ap-1
(defprop sbit aloc locf-method)			;could be ap-1
(defprop svref aloc locf-method)		;could be ap-1

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method apply (function &rest args)
  (unless (and (consp function)
	       (memq (car function) '(function quote))
	       (eq (length function) 2)
	       (symbolp (cadr function)))
    (ferror 'sys:unknown-setf-reference
	    "In SETF of APPLY, the function APPLYed must be a constant."))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method (cons (cadr function) args))
    (if (eq (cadr function) 'aref)
	(setq storeform
	      `(aset ,(car (last storeform)) . ,(butlast (cdr storeform)))))
    (if (not (eq (car (last storeform)) (car (last tempvars))))
	(ferror 'sys:unknown-setf-reference
		"~S not acceptable within APPLY within SETF." function)
      (values tempvars tempargs storevars
	      `(apply #',(car storeform) . ,(cdr storeform))
	      `(apply #',(car refform) . ,(cdr refform))))))

))
