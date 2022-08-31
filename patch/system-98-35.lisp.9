;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 2/02/84 03:00:07 by RMS,
;;; Reason: Select All Buffers as Tag Table: omit special buffers.
;;; Zmacs pathname defaulting.  Zmacs offers to create nonexistent package.
;;; C-X C-F notices if previously nonexistent file now exists.
;;; C-X M flushes undo info; C-U C-X M preserves major mode.
;;; EQUALP on vectors of characters vs strings.
;;; :ITEM-LIST operation bug.
;;; BEEP on non-windows improved.
;;; UNINTERN default arg.
;;; SET-MEMORY-SIZE doc string.  Better swap recommendations.
;;; Fix order of binding of keyword args in compiled code.
;;; Replace uses of FORMAT ~G with ~@*.
;;; while running on Lisp Machine Eighteen from band 4
;;; with System 98.30, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file SECTIO.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFCOM COM-SELECT-ALL-BUFFERS-AS-TAG-TABLE "Select all files currently read in as a tag table.
This causes commands such as Tags Search, Tags Query Replace, and
Tags Compile Changed Sections to look through all files now visited." ()
  (SELECT-FILE-LIST-AS-TAG-TABLE (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
				       AS FILE-ID = (BUFFER-FILE-ID BUFFER)
				       WHEN (OR (EQ FILE-ID T)
						(AND FILE-ID
						     (CONSP FILE-ID)
						     (NOT (NODE-SPECIAL-TYPE BUFFER))))
				       COLLECT (BUFFER-PATHNAME BUFFER))
				 "All buffers visiting files")
  DIS-NONE)

))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN PATHNAME-DEFAULTS (&OPTIONAL (DEFAULTS *PATHNAME-DEFAULTS*) (BUFFER *INTERVAL*)
			  &AUX (MAJOR-MODE (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
					     (SEND BUFFER ':MAJOR-MODE)))
			  TEM)
  "Update DEFAULTS for BUFFER, and return it.
DEFAULTS is a defaults-alist.
We update it by setting the defaults in it
based on BUFFER's visited pathname, or its name."
  (AND (TYPEP BUFFER 'FILE-BUFFER)
       (FS:SET-DEFAULT-PATHNAME
	 (OR (AND (SETQ TEM (GET MAJOR-MODE 'PATHNAME-DEFAULTING-FUNCTION))
		  (FUNCALL TEM DEFAULTS BUFFER))
	     (BUFFER-PATHNAME BUFFER)
	     (LET ((TYPE (CAR (RASSQ (INTERN-SOFT
				       (STRING-UPCASE
					 (SYMEVAL MAJOR-MODE))
				       "KEYWORD")
				     FS:*FILE-TYPE-MODE-ALIST*)))
		   (PN (FUNCALL (FS:DEFAULT-PATHNAME DEFAULTS)
				':NEW-SUGGESTED-NAME
				(BUFFER-NAME BUFFER))))
	       (IF TYPE (FUNCALL PN ':NEW-TYPE TYPE) PN)))
	 DEFAULTS))
  DEFAULTS)

))

; From file ZMNEW.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "


(DEFUN INITIALIZE-BUFFER-PACKAGE (BUFFER)
  "Initialize the BUFFER-PACKAGE of BUFFER from its :PACKAGE attribute."
  (OR (BUFFER-PACKAGE BUFFER)
      (PKG-BIND (OR *DEFAULT-PACKAGE* PACKAGE)
	(CONDITION-BIND ((SYS:PACKAGE-NOT-FOUND 'INITIALIZE-BUFFER-PACKAGE-HANDLER))
	  (SETF (BUFFER-PACKAGE BUFFER)
		(MULTIPLE-VALUE-BIND (VARS VALS)
		    (SEND BUFFER ':ATTRIBUTE-BINDINGS)
		  (PROGV VARS VALS PACKAGE)))))))

(DEFUN INITIALIZE-BUFFER-PACKAGE-HANDLER (CONDITION)
  (WHEN (YES-OR-NO-P "~&Package ~A does not exist.  Create it? " (SEND CONDITION ':NAME))
    ':CREATE-PACKAGE))

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM-LIST) (TYPE LIST &AUX (MAXL 0) N
						  (INSIDE-WIDTH (SHEET-INSIDE-WIDTH)))
  (FUNCALL-SELF ':FRESH-LINE)
  (COND (LIST					;Do nothing if empty list
	 ;; Compute the maximum width of any item, in dots (MAXL).
	 (DOLIST (ITEM LIST)
	   (LET ((STRING (STRING (IF (CONSP ITEM) (CAR ITEM) ITEM))))
	     (SETQ MAXL (MAX (SHEET-STRING-LENGTH SELF STRING) MAXL))))
	 ;; How many items go on each line (except the last)?
	 (SETQ N (MAX (MIN (TRUNCATE INSIDE-WIDTH (+ MAXL (FONT-CHAR-WIDTH CURRENT-FONT)))
			   (LENGTH LIST))
		      1))			;Always print something, even if continuation
	 ;; Now print the items and store the data in the table.
	 ;; Move to a new line when we exhaust a line, and at the end.
	 ;; I counts from 1 thru N on each line.
	 (DO ((I 1 (1+ I))
	      (LIST LIST (CDR LIST))
	      (WIDTH-PER (TRUNCATE INSIDE-WIDTH N)))
	     ((NULL LIST))
	   ;; Actually make this item.
	   (IF (CONSP (CAR LIST))
	       (FUNCALL SELF ':ITEM TYPE (CDAR LIST) "~A" (CAAR LIST))
	       (FUNCALL SELF ':ITEM TYPE (CAR LIST)))
	   ;; Space out for next item, or move to new line.
	   (IF (AND ( I N) (CDR LIST))
	       ;; Not end of line, space out for next item.
	       (MULTIPLE-VALUE-BIND (X Y)
		   (SHEET-READ-CURSORPOS SELF)
		 (SHEET-SET-CURSORPOS SELF
				      (* WIDTH-PER
					 (TRUNCATE (+ (1- WIDTH-PER) X)
						   WIDTH-PER))
				      Y))
	     ;; End of line.
	     (SEND SELF ':TYO #\RETURN)
	     (SETQ I 0)))))
  (TV:MOUSE-WAKEUP)
  NIL)

))

; From file TYPWIN.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :TERPRI) MAYBE-MOVE-BOTTOM-REACHED)

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
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
		(UNLESS (EQUALP (CLI:AR-1-FORCE ARRAY1 I) (CLI:AR-1-FORCE ARRAY2 I))
		  (RETURN NIL)))))))

))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN FIND-FILE (PATHNAME &OPTIONAL (SELECT-P T) QUIETLY (LOAD-P T) DONT-SECTIONIZE
		  &AUX BUFFER STREAM OLD-DESC NEW-DESC)
  "Return a buffer visiting file PATHNAME, reading file in if necessary.
If SELECT-P is T (the default), select the buffer also.
QUIETLY non-NIL means do not print messages about reading the file.
If LOAD-P is NIL, we do not read the file, just create
a buffer supposedly visiting that file (as if the file did not exist).
If there is already a buffer visiting the file, we check to see
if a more recent version of the file exists in the file system
and offer to revert if so.  To avoid this, try FIND-FILE-BUFFER
before you try FIND-FILE.

If DONT-SECTIONIZE is non-NIL, we mark all the buffers
 not to be sectionized.

If PATHNAME has wildcards, we visit all the files specified."
  (IF (STRINGP PATHNAME)
      (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME)))
  (IF (SEND PATHNAME ':WILD-P)
      (SEND PATHNAME ':WILDCARD-MAP 'FIND-FILE-1 NIL NIL
	    (SEND PATHNAME ':VERSION) SELECT-P QUIETLY LOAD-P DONT-SECTIONIZE)
    (SETQ BUFFER (OR (FIND-FILE-BUFFER PATHNAME)
		     (MAKE-INSTANCE 'ZMACS-BUFFER ':NAME NIL)))
    (IF DONT-SECTIONIZE
	(PUTPROP BUFFER T ':DONT-SECTIONIZE)
      (REMPROP BUFFER ':DONT-SECTIONIZE))
    (IF (NULL (BUFFER-FILE-ID BUFFER))
	(IF LOAD-P
	    (REVERT-BUFFER BUFFER PATHNAME
			   (IF DONT-SECTIONIZE 'NOSECTIONIZE T)
			   (AND SELECT-P *FIND-FILE-EARLY-SELECT*) QUIETLY)
	  (SET-BUFFER-PATHNAME PATHNAME BUFFER))
      (COND ((ERRORP (SETQ STREAM (OPEN PATHNAME '(:PROBE))))
	     (AND (NOT (SYMBOLP (BUFFER-FILE-ID BUFFER)))
		  (FORMAT QUERY-IO "~&Note: File ~A has been deleted." PATHNAME)))
	    ((MULTIPLE-VALUE (NEW-DESC OLD-DESC)
	       (STREAM-CHECK-FILE-ID STREAM (BUFFER-FILE-ID BUFFER)))
	     (COND ((AND (SYMBOLP (BUFFER-FILE-ID BUFFER))
			 (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER)))
		    (WHEN (YES-OR-NO-P "The file ~A exists now.  Read it in? " PATHNAME)
		      (REVERT-BUFFER BUFFER PATHNAME
				     (IF DONT-SECTIONIZE 'NOSECTIONIZE T)
				     (AND SELECT-P *FIND-FILE-EARLY-SELECT*))))
		   ((BUFFER-NEEDS-SAVING-P BUFFER)
		    (BEEP)
		    (FORMAT T
			    "Since you last read or wrote ~A
 (~A),
while you've been editing, someone has written a new copy out 
 (~A).
You will lose some work if you are not careful.
I will leave you your old copy instead of reading the new one.
I suggest that you file this out under a different name and then SRCCOM the two files.
Do M-X Revert if you really want the new one.~%" PATHNAME OLD-DESC NEW-DESC))
		   (T
		    (FORMAT T
			    "Since you last read or wrote ~A 
 (~A),
someone else wrote a new version on disk 
 (~A).
Luckily, you haven't edited the buffer since then.
Your old copy is still in the buffer.  " PATHNAME OLD-DESC NEW-DESC)
		    (COND ((FQUERY `(:STREAM ,STANDARD-OUTPUT . ,FORMAT:YES-OR-NO-P-OPTIONS)
				   "Do you want the new version instead? ")
			   (REVERT-BUFFER BUFFER PATHNAME
					  (IF DONT-SECTIONIZE 'NOSECTIONIZE T)
					  (AND SELECT-P *FIND-FILE-EARLY-SELECT*)))))))))
    (SEND BUFFER ':ACTIVATE T)
    (COND (SELECT-P (MAKE-BUFFER-CURRENT BUFFER)))
    BUFFER))

))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(DEFUN STREAM-CHECK-FILE-ID (STREAM FILE-ID &AUX FILE-FILE-ID)
  "If STREAM's :INFO doesn't match FILE-ID, return two strings describing new and old values.
Otherwise return NIL.
The new value is STREAM's :INFO, the old value is FILE-ID."
  (COND ((NULL FILE-ID) NIL)
	((EQUAL FILE-ID (SETQ FILE-FILE-ID (FUNCALL STREAM ':INFO))) NIL)
	(T
	 (VALUES (DESCRIBE-FILE-ID FILE-FILE-ID)
		 (DESCRIBE-FILE-ID FILE-ID)))))

(DEFUN DESCRIBE-FILE-ID (FILE-ID)
  "Given a file-id (the value of the :INFO file stream op) return a string describing it."
  (IF (SYMBOLP FILE-ID) "a new file, not present on disk"
    (FORMAT NIL "~A, created ~@[by ~A at ~]~\TIME\"
	    (CAR FILE-ID)
	    (LET ((PROPS (FUNCALL (CAR FILE-ID) ':PROPERTIES NIL)))
	      (AND (CONSP PROPS) (GET PROPS ':AUTHOR)))
	    (CDR FILE-ID))))

))

; From file SHWARM.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFMETHOD (SCREEN :BEEP) (&OPTIONAL BEEP-TYPE)
  "Beep the beeper."
  BEEP-TYPE  ;We wanted to make this soo hairy, that we punted until we could do it right
  (AND BEEP
       (WITHOUT-INTERRUPTS  ;otherwise might quit out and leave screen complemented
	 (OR (EQ BEEP ':BEEP) (COMPLEMENT-BOW-MODE SELF))
	 (IF (EQ BEEP ':FLASH)
	     (%BEEP 0 BEEP-DURATION)	;Delay same time without making any noise
	   (BEEP BEEP-TYPE 'IGNORE))
	 (OR (EQ BEEP ':BEEP) (COMPLEMENT-BOW-MODE SELF)))))

))

; From file SHWARM.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFUN BEEP (&OPTIONAL BEEP-TYPE (STREAM TERMINAL-IO))
  "Ring the bell and flash the screen.
Works via the :BEEP operation on STREAM if STREAM supports it.
The value of BEEP controls what this function does:
 T means flash the screen and make noise,
 :BEEP means just make noise, :FLASH means just flash.
 NIL means do nothing.
BEEP-TYPE says why the beep is being done.  Standard values are:
 ZWEI:CONVERSE-PROBLEM -- Converse was unable to send a message.
 ZWEI:CONVERSE-MESSAGE-RECEIVED -- A Converse message has come in.
 ZWEI:NO-COMPLETION -- Completion in a minibuffer failed.
 TV:NOTIFY -- A notification cannot be printed on the selected window.
 NIL -- anything else.
BEEP-TYPE does not have any effect, currently,
but you can redefine BEEP to to different things for different beep types."
  (WHEN BEEP
    (IF (MEMQ ':BEEP (FUNCALL STREAM ':WHICH-OPERATIONS))
	(FUNCALL STREAM ':BEEP BEEP-TYPE)
      (%BEEP BEEP-WAVELENGTH BEEP-DURATION))))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN UNINTERN (SYM &OPTIONAL (PKG PACKAGE))
  "Removes (uninterns) any symbol whose pname matches SYM from package PKG.
SYM may be a symbol or a string.
PKG defaults to the current package.
FORCE-FLAG says do not check for name conflicts.
Returns T if a symbol was removed, NIL if there was no such symbol present."
  (REMOB SYM PKG))

(DEFUN REMOB (SYM &OPTIONAL (PKG (SYMBOL-PACKAGE SYM))
		 &OPTIONAL FORCE-FLAG
		 &AUX HASH TEM STR MUST-REPLACE REPLACEMENT-SYM)
  "Removes (uninterns) any symbol whose pname matches SYM from package PKG.
SYM may be a symbol or a string.
PKG defaults to SYM's package
/(in that case, SYM must be a symbol,
and it is the symbol that gets removed).
FORCE-FLAG says do not check for name conflicts.
Returns T if a symbol was removed, NIL if there was no such symbol present."
  (WHEN PKG
    (OR (PACKAGEP PKG) (SETQ PKG (PKG-FIND-PACKAGE PKG)))
    (WHEN (AND (NOT FORCE-FLAG)
	       (MEMQ SYM (PKG-SHADOWING-SYMBOLS PKG)))
      ;; Check for name-conflict being uncovered.
      ;; If there is one, decide what we will do to fix it.
      ;; We can't actually do it until after uninterning our argument.
      (LET ((CONFLICT (CHECK-FOR-NAME-CONFLICT (STRING SYM) PKG T)))
	(WHEN CONFLICT
	  (SETF (VALUES MUST-REPLACE REPLACEMENT-SYM)
		(REPORT-NAME-CONFLICT SYM PKG CONFLICT)))))
    (SETQ HASH (PKG-STRING-HASH-CODE (SETQ STR (STRING SYM))))
    (WITHOUT-INTERRUPTS
      (MULTIPLE-VALUE (SYM TEM)
	(PKG-INTERN-INTERNAL STR HASH PKG))
      (WHEN TEM
	(AND (EQ (SYMBOL-PACKAGE SYM) PKG)
	     (SETF (SYMBOL-PACKAGE SYM) NIL))
	(SETF (PKG-SLOT-CODE PKG TEM) T)
	(SETF (PKG-SLOT-SYMBOL PKG TEM) NIL)
	(SETF (PKG-SHADOWING-SYMBOLS PKG)
	      (DELQ SYM (PKG-SHADOWING-SYMBOLS PKG)))
	(WHEN MUST-REPLACE
	  (INTERN-LOCAL REPLACEMENT-SYM PKG)
	  (PUSH REPLACEMENT-SYM (PKG-SHADOWING-SYMBOLS PKG)))
	T))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST KEYWORDP (SYMBOL)
  "T if SYMBOL belongs to the KEYWORD package."
  (AND (SYMBOLP SYMBOL)
       (EQ (SYMBOL-PACKAGE SYMBOL) PKG-KEYWORD-PACKAGE)))

))

; From file DIRED.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN COM-MAIL-INTERNAL (RE-INIT-P &OPTIONAL WHO WHAT INITIAL-POSITION)
  (FUNCALL *WINDOW* ':FIND-SPECIAL-BUFFER ':MAIL RE-INIT-P "Mail" T ':TEXT)
  (COND (RE-INIT-P				;With no numeric arg, re-initialize the buffer
	 (COM-TEXT-MODE)
	 (TURN-ON-MODE 'MAIL-MODE)
	 (DELETE-INTERVAL *INTERVAL*)
	 (INSERT-MOVING (POINT) "To: ")
	 (AND WHO (INSERT-MOVING (POINT) WHO))
	 (LET ((BP (INSERT (POINT) #\RETURN)))
	   (SETQ BP (INSERT BP *MAIL-HEADER-DELIMITER*))
	   (SETQ BP (INSERT BP #\RETURN))
	   (WHEN WHAT
	     (INSERT-MOVING BP WHAT)
	     (IF INITIAL-POSITION
		 (SETQ BP (FORWARD-CHAR BP (- INITIAL-POSITION (STRING-LENGTH WHAT)) T))))
	   (AND WHO (MOVE-BP (POINT) BP)))
	 ;; RE-INIT-P can be a variable whose value may be a template to use.
	 ;; If so, invoke the template.
	 (AND (NEQ RE-INIT-P T)
	      (BOUNDP RE-INIT-P)
	      (SYMEVAL RE-INIT-P)
	      (FUNCALL (SYMEVAL RE-INIT-P) *INTERVAL* NIL))
	 (DISCARD-UNDO-INFORMATION *INTERVAL*)
	 (NOT-MODIFIED *INTERVAL*)))
  DIS-TEXT)

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN SET-MEMORY-SIZE (NEW-SIZE)
  "Specify how much main memory is to be used, in words.
/(By default, all the memory on the machine is used.)
This is mainly useful running benchmarks with different memory sizes.
If you specify more memory than is present on the machine,
memory board construction starts; in the meantime, the machine crashes."
  (PROG (OLD-SIZE NEWP OLDP)
	(COND ((< NEW-SIZE (+ (SYSTEM-COMMUNICATION-AREA %SYS-COM-WIRED-SIZE) 20000)) ;8K MIN
	       (FERROR NIL "~O is smaller than wired + 8K"  NEW-SIZE)))
    L   (SETQ OLD-SIZE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE))
        (SETQ OLDP (CEILING OLD-SIZE PAGE-SIZE))
        (SETQ NEWP (CEILING NEW-SIZE PAGE-SIZE))
	(COND ((OR (> NEWP (REGION-LENGTH PHYSICAL-PAGE-DATA))
		   (> NEWP (TRUNCATE (* 4 (REGION-LENGTH PAGE-TABLE-AREA)) 9)))
	       (FERROR NIL "~O is bigger than page tables allow"  NEW-SIZE))
	      ((= NEWP OLDP) (RETURN T))
              ((< NEWP OLDP) (GO FLUSH)))
  MORE  (COND ((%DELETE-PHYSICAL-PAGE OLD-SIZE)
               (PRINT (LIST OLD-SIZE "EXISTED"))))
        (%CREATE-PHYSICAL-PAGE OLD-SIZE)
	(SETF (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
	      (+ OLD-SIZE PAGE-SIZE))
        (GO L)

  FLUSH (COND ((NULL (%DELETE-PHYSICAL-PAGE (- OLD-SIZE PAGE-SIZE)))
               (PRINT (LIST (- OLD-SIZE PAGE-SIZE) "DID-NOT-EXIST"))))
	(SETF (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
	      (- OLD-SIZE PAGE-SIZE))
        (GO L)))

))

; From file GC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "


(DEFUN SET-ALL-SWAP-RECOMMENDATIONS (N &OPTIONAL REALLY-ALL)
  "Set all areas to swap in N+1 pages at a time."
  (DOLIST (NAME-OF-AREA AREA-LIST)
    (IF (OR REALLY-ALL (= (AREA-SWAP-RECOMMENDATIONS (SYMEVAL NAME-OF-AREA))
			  LAST-ALL-AREAS-SWAP-RECOMMENDATIONS))
	(SET-SWAP-RECOMMENDATIONS-OF-AREA (SYMEVAL NAME-OF-AREA) N)))
  (SETQ LAST-ALL-AREAS-SWAP-RECOMMENDATIONS N))

(DEFCONST MEMORY-SIZE-SWAP-RECOMMENDATION-ALIST
	  '((320. . 2) (384. . 2) (448. . 3) (512. . 4) (576. . 5) (640. . 5) (704. . 6)
	    (768. . 6) (832. . 7) (896. . 7) (960. . 8) (1024. . 8))
  "Alist of memory size in K versus default area swap recommendation in pages.
Note that number of pages swapped is one greater than the recommendation.")

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN LEXICAL-VAR-P (VAR)
  (DO ((I 0 (1+ I))
       (E COMPILER-LEXICAL-ENVIRONMENT (CDR E)))
      ((NULL E))
    (WHEN (ASSQ VAR (CAR E))
      (RETURN T))))

;Return a reference to VAR as a lexical variable from a higher context,
;or NIL if VAR is not a variable of that sort available now.

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	PSEUDO-KEYNAMES)
    (COND ((MEMQ (CAR LAMBDA-EXP) '(LAMBDA CLI:LAMBDA))
	   (SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP)))
	  (T
	   (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP))))
    (MULTIPLE-VALUE (POSITIONAL-ARGS NIL AUXVARS
		     REST-ARG POSITIONAL-ARG-NAMES
		     KEYKEYS KEYNAMES NIL KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    (SETQ PSEUDO-KEYNAMES (COPYLIST KEYNAMES))
    ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
    ;; and check explicitly whether that has been overridden.
    ;; If the arg is optional
    ;; and the initial value is a constant, we can really init it to that.
    ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
    ;; after all keywords are decoded, we bind the intended variable, in sequence.
    ;; However a var that can shadow something (including any special var)
    ;; must always be replaced with a dummy.
    (DO ((KIS KEYINITS (CDR KIS))
	 (KNS KEYNAMES (CDR KNS))
	 (PKNS PSEUDO-KEYNAMES (CDR PKNS))
	 (KFS KEYFLAGS (CDR KFS)))
	((NULL KNS))
      (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
	    (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	(OR (AND (NULL KEYFLAG)
		 (CONSTANTP KEYINIT)
		 (NOT (ASSQ KEYNAME VARS))
		 (NOT (LEXICAL-VAR-P KEYNAME))
		 (NOT (SPECIALP KEYNAME)))
	    (PROGN (SETF (CAR KIS) 'SI:KEYWORD-GARBAGE)
		   (SETQ PSEUDO-KEYNAME (GENSYM))
		   (SETF (CAR PKNS) PSEUDO-KEYNAME)
		   (PUSH `(,KEYNAME
			   (COND ((EQ ,PSEUDO-KEYNAME SI:KEYWORD-GARBAGE)
				  ,KEYINIT)
				 (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
				    ,PSEUDO-KEYNAME)))
			 KEYCHECKS)))))
    (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
    (SETQ KEYCHECKS (NREVERSE KEYCHECKS))
    
    ;; Add some declarations to inform user of the fact that this compiled
    ;; function still logically wants keyword args.
    ;; The arglist info can be overridden by the user's explicit declaration.
    (COND ((ASSQ 'ARGLIST LOCAL-DECLARATIONS))
	  ((ASSQ ':ARGLIST LOCAL-DECLARATIONS))
	  (T
	   (PUSH `(ARGLIST . ,(LDIFF LAMBDA-LIST AUXVARS)) LOCAL-DECLARATIONS)))

    ;; If the user didn't ask for a rest arg, make one for the
    ;; outer function anyway.
    (OR REST-ARG (SETQ REST-ARG (GENSYM)
		       MAYBE-REST-ARG (LIST '&REST REST-ARG)))
    `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
       (LET* (,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
	      ,@KEYFLAGS)
;       (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;	      (OR (%PERMUTE-ARGS)
;		  (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;			   (CDR ,REST-ARG)
;			   (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;			   ',KEYKEYS)
;			 (%PERMUTE-ARGS)))
;	      ;; If the function really wants the rest arg,
;	      ;; flush the permutation table and its keyword.
;	      ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;	     (T
	 (WHEN ,REST-ARG
	   (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
					,REST-ARG ',KEYKEYS
					,ALLOW-OTHER-KEYS
					(VARIABLE-LOCATION ,(CAR PSEUDO-KEYNAMES))))
	 (LET* ,KEYCHECKS
	   ((LAMBDA ,AUXVARS . ,BODY)))))))

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1LAMBDA (LAMBDA ARGS)
  (LET (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR QUOTEFLAG
	SPECIAL-FLAG SPECIAL-VARS UNSPECIAL-FLAG UNSPECIAL-VARS
	DECLS KEYCHECKS BORDER-VARIABLE PSEUDO-KEYNAMES)
    (SETQ LAMBDA (SI:LAMBDA-EXP-ARGS-AND-BODY (P1AUX LAMBDA)))
    (SETQ ARGLIST (CAR LAMBDA) BODY (CDR LAMBDA))
    (MULTIPLE-VALUE-BIND (NIL NIL NIL REST-ARG
			  NIL KEYKEYS KEYNAMES NIL KEYINITS KEYFLAGS
			  ALLOW-OTHER-KEYS)
	(DECODE-KEYWORD-ARGLIST ARGLIST)
      (WHEN (AND KEYNAMES (NOT REST-ARG))
	(SETQ REST-ARG (GENSYM)))
      (SETQ ARGS1 ARGS)
      (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
	  (NIL)
	(SETQ VAR (CAR ARGLIST1))
	(COND ((NULL ARGLIST1)
	       (RETURN T))
	      ((EQ VAR '&KEY)
	       (PUSH (LIST REST-ARG `(LIST . ,ARGS1)) PROGVARS)
	       (RETURN (SETQ ARGS1 NIL)))
	      ((EQ VAR '&REST)
	       (POP ARGLIST1)
	       (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
	       (RETURN (SETQ ARGS1 NIL)))
	      ((EQ VAR '&OPTIONAL)
	       (SETQ OPTIONAL T))
	      ((EQ VAR '&QUOTE)
	       (SETQ QUOTEFLAG T))
	      ((EQ VAR '&EVAL)
	       (SETQ QUOTEFLAG NIL))
	      ((EQ VAR '&SPECIAL)
	       (SETQ SPECIAL-FLAG T UNSPECIAL-FLAG NIL))
	      ((EQ VAR '&LOCAL)
	       (SETQ SPECIAL-FLAG NIL UNSPECIAL-FLAG T))
	      ((EQ VAR '&FUNCTIONAL))
	      ((MEMQ VAR LAMBDA-LIST-KEYWORDS)
	       (WARN 'BAD-INTERNAL-LAMBDA-KEYWORD ':IMPOSSIBLE
		     "~S is not supported in internal lambdas." VAR))
	      (T (AND SPECIAL-FLAG (PUSH VAR SPECIAL-VARS))
		 (AND UNSPECIAL-FLAG (PUSH VAR UNSPECIAL-VARS))
		 (COND ((SYMBOLP VAR)
			(PUSH (LIST VAR (IF QUOTEFLAG `',(CAR ARGS1)
					  (CAR ARGS1)))
			      PROGVARS))
		       (T
			(COND ((NOT OPTIONAL)
			       (WARN 'BAD-ARGUMENT-LIST ':IMPOSSIBLE
				     "The mandatory argument ~S of an internal lambda ~
  was given a default value."
				     (CAR VAR))))
			(PUSH (LIST (CAR VAR)
				    (COND (ARGS1 (IF QUOTEFLAG `',(CAR ARGS1)
						   (CAR ARGS1)))
					  (T (CADR VAR)))) PROGVARS)))
		 (POP ARGS1))))
      (WHEN KEYNAMES
	(SETQ PSEUDO-KEYNAMES (COPYLIST KEYNAMES))
	;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
	;; and check explicitly whether that has been overridden.
	;; If the initial value is a constant, we can really init it to that.
	;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
	;; after all keywords are decoded, we bind the intended variable, in sequence.
	;; However a var that can shadow something (including any special var)
	;; must always be replaced with a dummy.
	(DO ((KIS KEYINITS (CDR KIS))
	     (KNS KEYNAMES (CDR KNS))
	     (PKNS PSEUDO-KEYNAMES (CDR PKNS))
	     (KFS KEYFLAGS (CDR KFS)))
	    ((NULL KNS))
	  (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
		(KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	    (OR (AND (NULL KEYFLAG)
		     (CONSTANTP KEYINIT)
		     (NOT (ASSQ KEYNAME VARS))
		     (NOT (LEXICAL-VAR-P KEYNAME))
		     (NOT (SPECIALP KEYNAME)))
		(PROGN (SETF (CAR KIS) 'SI:KEYWORD-GARBAGE)
		       (SETQ PSEUDO-KEYNAME (GENSYM))
		       (SETF (CAR PKNS) PSEUDO-KEYNAME)
		       (PUSH `(,KEYNAME
			       (COND ((EQ ,PSEUDO-KEYNAME SI:KEYWORD-GARBAGE)
				      ,KEYINIT)
				     (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
					,PSEUDO-KEYNAME)))
			     KEYCHECKS)))))
	(SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
	(SETQ KEYCHECKS (NREVERSE KEYCHECKS))

	;; BORDER-VARIABLE is a local we put in the binding list
	;; as the easiest way of being able to get a locative to the
	;; slot before the first of our keyword arg locals.
	(SETQ BORDER-VARIABLE (GENSYM))
	(SETQ BODY
	      `((LET* (,BORDER-VARIABLE
		       ,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
		       ,@KEYFLAGS)
		  ,BORDER-VARIABLE
		  (WHEN ,REST-ARG
		    (SI:STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
		      (VARIABLE-LOCATION ,BORDER-VARIABLE)
		      ,REST-ARG ',KEYKEYS
		      ,ALLOW-OTHER-KEYS
		      NIL))
		  (LET* ,KEYCHECKS
		    . ,BODY)))))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (SETF (VALUES BODY DECLS)
	    (EXTRACT-DECLARATIONS-RECORD-MACROS BODY))
      (WHEN SPECIAL-VARS
	(PUSH `(SPECIAL . ,SPECIAL-VARS) DECLS))
      (WHEN UNSPECIAL-VARS
	(PUSH `(UNSPECIAL . ,UNSPECIAL-VARS) DECLS))
      (WHEN DECLS
	(PUSH `(DECLARE . ,DECLS) BODY))
      (P1 `(LET-FOR-LAMBDA ,(NRECONC PROGVARS (IF ARGS1 `((IGNORE (PROGN . ,ARGS1)))))
	     . ,BODY)))))


))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN STORE-KEYWORD-ARG-VALUES (FRAME-POINTER ARGS KEYKEYS ALLOW-OTHER-KEYS
					       &OPTIONAL FIRST-KEYARG-POINTER
					       SPECVAR-LIST)
  (LET ((FIRST-KEYWORD-ARG-INDEX
	  (IF (LOCATIVEP FIRST-KEYARG-POINTER)
	      (PROGN (SETQ FRAME-POINTER FIRST-KEYARG-POINTER) -1)
	    (%P-LDB-OFFSET %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN
			   FRAME-POINTER
			   %LP-ENTRY-STATE))))
    (IF (GET (LOCF ARGS) ':ALLOW-OTHER-KEYS)
	(SETQ ALLOW-OTHER-KEYS T))
    ;; First decode what was specified.
    (DO ((ARGS-LEFT ARGS (CDDR ARGS-LEFT))
	 (FOUND-FLAGS 0))
	((NULL ARGS-LEFT))
      (LET ((KEYWORD (CAR ARGS-LEFT)))
	(DO () (())
	  (LET ((INDEX (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
	    (COND (INDEX
		   (WHEN (ZEROP (LOGAND 1 (ASH FOUND-FLAGS (- INDEX))))
		     (SETQ FOUND-FLAGS
			   (DPB 1 (BYTE 1 INDEX) FOUND-FLAGS))
		     (LET ((SPECVAR (NTH INDEX SPECVAR-LIST)))
		       (IF SPECVAR
			   (SET SPECVAR (CADR ARGS-LEFT)))
		       (%P-STORE-CONTENTS-OFFSET (CADR ARGS-LEFT) FRAME-POINTER
						 (+ 1 INDEX FIRST-KEYWORD-ARG-INDEX))))
		   (RETURN))
		  (ALLOW-OTHER-KEYS (RETURN))
		  (T
		   (SETQ KEYWORD (CERROR ':NEW-KEYWORD NIL 'SYS:UNDEFINED-KEYWORD-ARGUMENT
					 "Keyword arg keyword ~S unrecognized."
					 KEYWORD (CADR ARGS-LEFT)))
		   (OR KEYWORD (RETURN))))))))))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFMETHOD (FUNCTION-ENTRY-ERROR :REPORT) (STREAM)
  (COND
    ((MEMQ 'TOO-FEW-ARGUMENTS CONDITION-NAMES)
     (FORMAT STREAM "Function ~S called with only ~D argument~1@*~P."
	     (FUNCTION-NAME FUNCTION) NARGS))
    ((MEMQ 'TOO-MANY-ARGUMENTS CONDITION-NAMES)
     (FORMAT STREAM "Function ~S called with too many arguments (~D)."
	     (FUNCTION-NAME FUNCTION) NARGS))
    ((MEMQ ' CONDITION-NAMES)
     (FORMAT STREAM "Function ~S called with an argument of bad data type."
	     (FUNCTION-NAME FUNCTION)))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CHECK-ARG (ARG-NAME PREDICATE TYPE-STRING &OPTIONAL ERROR-TYPE-NAME)
  "Generate error if the value of ARG-NAME doesn't satisfy PREDICATE.
PREDICATE is a function name (a symbol) or an expression to compute.
TYPE-STRING is a string to use in the error message, such as /"a list/".
ERROR-TYPE-NAME is a keyword that tells condition handlers what type was desired."
    (AND (NULL ERROR-TYPE-NAME)
	 (SYMBOLP PREDICATE)
	 (SETQ ERROR-TYPE-NAME PREDICATE))
    `(DO () (,(COND ((SYMBOLP PREDICATE)
                     `(,PREDICATE ,ARG-NAME))
                    (T PREDICATE))
             ,ARG-NAME)
	 (SETQ ,ARG-NAME
	       (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
		       "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
		       ',ERROR-TYPE-NAME ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO CHECK-ARG-TYPE (ARG-NAME TYPE &OPTIONAL TYPE-STRING)
  "Generate an error unless (TYPEP ARG-NAME 'TYPE).
TYPE-STRING is a string to use in the error message, such as /"a list/".
If you omit it, it will be computed from TYPE's pname."
  (IF (NULL TYPE-STRING)
      (SETQ TYPE-STRING (STRING-APPEND (TYPE-PRETTY-NAME TYPE))))
  `(DO () ((TYPEP ,ARG-NAME ',TYPE))
     (SETQ ,ARG-NAME
	   (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
		   "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
		   ',TYPE ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO ETYPECASE (OBJECT &BODY CLAUSES)
  "Execute the first clause whose type specifier OBJECT fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of OBJECT.
If the result is T, the rest of that clause is excuted and the values
 of the last form in it are the values of the ETYPECASE form.
If no clause fits, an uncorrectable error is signaled."
  (LET ((SAVE-OBJECT OBJECT))
    (ONCE-ONLY (OBJECT)
      `(COND
	 ,@(LOOP FOR (TYPE . CONSEQUENTS) IN CLAUSES
		 COLLECT `(,(PROGN
			      (MACRO-TYPE-CHECK-WARNING 'TYPECASE TYPE)
			      (COND ;; This clause will eventually be subsumed
				    ;; by the definition of (OR ...) as a type,
				    ;; but for now it is essential.
				    ((AND (LISTP TYPE) (EQ (CAR TYPE) 'OR))
				     `(OR . ,(LOOP FOR XTYPE IN (CDR TYPE)
						   COLLECT `(TYPEP ,OBJECT ',XTYPE))))
				    (T
				     `(TYPEP ,OBJECT ',TYPE))))
			   NIL . ,CONSEQUENTS))
	 (T (FERROR NIL
		    "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
		    '(OR . ,(MAPCAR 'CAR CLAUSES))
		    ,OBJECT ',SAVE-OBJECT
		    ,(STRING-APPEND (TYPE-PRETTY-NAME `(OR . ,(MAPCAR 'CAR CLAUSES))))))))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO CTYPECASE (OBJECT &BODY CLAUSES)
  "Execute the first clause whose type specifier OBJECT fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of OBJECT.
If the result is T, the rest of that clause is excuted and the values
 of the last form in it are the values of the CTYPECASE form.
If no clause fits, a correctable error is signaled.
The user can correct with a new value for OBJECT."
  (LET ((SAVE-OBJECT OBJECT))
    `(BLOCK CTYPECASE-LOOP
       (TAGBODY
      CTYPECASE-LOOP
	 (RETURN-FROM CTYPECASE-LOOP
	   ,(ONCE-ONLY (OBJECT)
	      `(COND
		 ,@(LOOP FOR (TYPE . CONSEQUENTS) IN CLAUSES
			 COLLECT `(,(PROGN
				      (MACRO-TYPE-CHECK-WARNING 'TYPECASE TYPE)
				      (COND ;; This clause will eventually be subsumed
					;; by the definition of (OR ...) as a type,
					;; but for now it is essential.
					((AND (LISTP TYPE) (EQ (CAR TYPE) 'OR))
					 `(OR . ,(LOOP FOR XTYPE IN (CDR TYPE)
						       COLLECT `(TYPEP ,OBJECT ',XTYPE))))
					(T
					 `(TYPEP ,OBJECT ',TYPE))))
				   NIL . ,CONSEQUENTS))
		 (T (SETF ,SAVE-OBJECT
			  (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
				  "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
				  '(OR . ,(MAPCAR 'CAR CLAUSES))
				  ,OBJECT ',SAVE-OBJECT
				  ,(STRING-APPEND
				     (TYPE-PRETTY-NAME `(OR . ,(MAPCAR 'CAR CLAUSES))))))
		    (GO CTYPECASE-LOOP)))))))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO CCASE (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the CCASE.
If no clause matches, a correctable error is signaled.
The user can correct with a new value for TEST-OBJECT."
  (LET (TEST-EXP COND-EXP TYPE-FOR-ERROR)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '*SELECTQ-ITEM*)))
    (SETQ TYPE-FOR-ERROR
	  `(MEMBER . ,(MAPCAN #'(LAMBDA (CLAUSE)
				  (LET ((MATCH (CAR CLAUSE)))
				    (IF (NLISTP MATCH) (LIST MATCH)
				      (COPYLIST MATCH))))
			      CLAUSES)))
    (SETQ COND-EXP
	  `(COND
	     ,@(MAPCAR #'(LAMBDA (CLAUSE)
			   (MACRO-TYPE-CHECK-WARNING 'CCASE (CAR CLAUSE))
			   (COND ((ATOM (CAR CLAUSE))
				  `((EQ ,TEST-EXP ',(CAR CLAUSE)) NIL . ,(CDR CLAUSE)))
				 (T
				  `((MEMQ ,TEST-EXP ',(CAR CLAUSE)) NIL . ,(CDR CLAUSE)))))
		       CLAUSES)
	     (T (SETF ,TEST-OBJECT
		      (CERROR '(:ARGUMENT-VALUE) NIL 'WRONG-TYPE-ARGUMENT
			      "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
			      ',TYPE-FOR-ERROR
			      ,TEST-EXP ',TEST-OBJECT
			      ,(STRING-APPEND (TYPE-PRETTY-NAME TYPE-FOR-ERROR))))
		(GO CCASE-LOOP))))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'CCASE)
    (UNLESS (EQ TEST-EXP TEST-OBJECT)
      (SETQ COND-EXP
	    `(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
	       ,COND-EXP)))
    `(BLOCK CCASE-LOOP
       (TAGBODY
      CCASE-LOOP
         (RETURN-FROM CCASE-LOOP
	   ,COND-EXP)))))

))

; From file LMMAC.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "


(DEFMACRO ECASE (TEST-OBJECT &BODY CLAUSES)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (LET (TEST-EXP COND-EXP TYPE-FOR-ERROR)
    (SETQ TEST-EXP
	  ;; If TEST-OBJECT is an eval-at-load-time,
	  ;; we will treat it as a random expression, which is right.
	  (COND ((OR (ATOM TEST-OBJECT)
		     (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			  (ATOM (CADR TEST-OBJECT))))
		 TEST-OBJECT)
		(T '*SELECTQ-ITEM*)))
    (SETQ TYPE-FOR-ERROR
	  `(MEMBER . ,(MAPCAN #'(LAMBDA (CLAUSE)
				  (LET ((MATCH (CAR CLAUSE)))
				    (IF (NLISTP MATCH) (LIST MATCH)
				      (COPYLIST MATCH))))
			      CLAUSES)))
    (SETQ COND-EXP
	  `(COND
	     ,@(MAPCAR #'(LAMBDA (CLAUSE)
			   (MACRO-TYPE-CHECK-WARNING 'ECASE (CAR CLAUSE))
			   (COND ((ATOM (CAR CLAUSE))
				  `((EQ ,TEST-EXP ',(CAR CLAUSE)) NIL . ,(CDR CLAUSE)))
				 (T
				  `((MEMQ ,TEST-EXP ',(CAR CLAUSE)) NIL . ,(CDR CLAUSE)))))
		       CLAUSES)
	     (T (FERROR NIL
			"The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
			',TYPE-FOR-ERROR
			,TEST-EXP ',TEST-OBJECT
			,(STRING-APPEND (TYPE-PRETTY-NAME TYPE-FOR-ERROR))))))
    (DEAD-CLAUSES-WARNING (CDR COND-EXP) 'ECASE)
    (UNLESS (EQ TEST-EXP TEST-OBJECT)
      (SETQ COND-EXP
	    `(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
	       ,COND-EXP)))
    COND-EXP))

))

; From file QRAND.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN ARRAY-INITIALIZE (ORIGINAL-ARRAY VALUE &OPTIONAL (START 0) END
			 &AUX (ARRAY ORIGINAL-ARRAY) (OFFSET 0))
  "Set all the elements of ARRAY to VALUE, or all elements from START to END.
If END is NIL or not specified, the active length of ARRAY is used."
  (UNLESS END
    (SETQ END (ARRAY-ACTIVE-LENGTH ARRAY)))
  (UNLESS ( 0 START END (ARRAY-LENGTH ARRAY))
    (FERROR NIL "START is ~S and END is ~S, for ~S." START END ARRAY))
  (IF (< END (+ START 30.))
      ;; If number of elements to be hacked is small, just do them.
      (DO ((I START (1+ I))) (( I END))
	(SETF (AR-1-FORCE ARRAY I) VALUE))
    ;; Handle indirect arrays by finding the array indirected to
    ;; and updating the start and end indices if appropriate.
    (DO () ((NOT (ARRAY-INDIRECT-P ARRAY)))
      (AND (ARRAY-INDEX-OFFSET ARRAY)
	   (INCF OFFSET (ARRAY-INDEX-OFFSET ARRAY)))
      (SETQ ARRAY (ARRAY-INDIRECT-TO ARRAY)))
    ;; Handle forwarded arrays.
    (UNLESS (= (%P-DATA-TYPE ARRAY) DTP-ARRAY-HEADER)
      (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY)))
    (LET* ((ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q (%P-LDB %%ARRAY-TYPE-FIELD ORIGINAL-ARRAY)))
	   (BITS-PER-ELEMENT (ARRAY-BITS-PER-ELEMENT (%P-LDB %%ARRAY-TYPE-FIELD ORIGINAL-ARRAY)))
	   (START (+ START OFFSET))
	   (END (+ END OFFSET))
	   (DATA-OFFSET (ARRAY-DATA-OFFSET ARRAY))
	   ;; Compute how many words are in the repeating unit that we replicate with %BLT.
	   ;; This is 1 word unless an element is bigger than that.
	   (BLT-DISTANCE (IF (PLUSP ENTRIES-PER-Q) 1 (- ENTRIES-PER-Q)))
	   ;; This is how many elements it takes to make BLT-DISTANCE words.
	   (Q-BOUNDARY-ELTS (MAX 1 ENTRIES-PER-Q))
	   ;; We must deposit element by element until this element
	   ;; in order to make sure we have a full word of elements stored
	   ;; Beyond this, we can blt entire words.
	   (STOP-ELEMENT-BY-ELEMENT
	     (MIN END
		  (* Q-BOUNDARY-ELTS
		     (1+ (CEILING START Q-BOUNDARY-ELTS)))))
	   ;; We must stop our word-wise copying before this element number
	   ;; to avoid clobbering any following elements which are beyond END.
	   (END-WORD-WISE
	     (MAX START (* Q-BOUNDARY-ELTS (FLOOR END Q-BOUNDARY-ELTS))))
	   ;; Compute index in words, wrt array data, of the first data word
	   ;; that we will not fill up an element at a time.
	   (UNINITIALIZED-DATA-OFFSET
	     (+ DATA-OFFSET
		(* BLT-DISTANCE (CEILING STOP-ELEMENT-BY-ELEMENT Q-BOUNDARY-ELTS))))
	   ;; Compute the length of the data in the array, in Qs, if caller didn't supply it.
	   (DATA-LENGTH
	     (IF (PLUSP ENTRIES-PER-Q)
		 (TRUNCATE END-WORD-WISE ENTRIES-PER-Q)
	       (* END-WORD-WISE (- ENTRIES-PER-Q)))))
      ;; Fill in any elements in an incomplete first word,
      ;; plus one full word's worth.
      ;; We must use the original array to store element by element,
      ;; since the element size of the array indirected to may be different.
      (DO ((I START (1+ I)))
	  ((= I STOP-ELEMENT-BY-ELEMENT))
	(SETF (AR-1-FORCE ORIGINAL-ARRAY (- I OFFSET)) VALUE))
      ;; Now fill in the elements in the incomplete last word.
      (DO ((I END-WORD-WISE (1+ I)))
	  (( I END))
	(SETF (AR-1-FORCE ORIGINAL-ARRAY (- I OFFSET)) VALUE))
      ;; Now copy the data word by word (or by two words for ART-FLOAT!)
      ;; There is no hope of passing %BLT pointers that are GC-safe.
      (IF (PLUSP (- DATA-LENGTH (- UNINITIALIZED-DATA-OFFSET DATA-OFFSET)))
	  (WITHOUT-INTERRUPTS
	    ;; If the array is displaced to a random location, use that location
	    ;; as the data start.  Arrays displaced to other arrays
	    ;; were handled above.
	    (IF (ARRAY-DISPLACED-P ARRAY)
		(SETQ ARRAY (- (%POINTER (ARRAY-INDIRECT-TO ARRAY)) DATA-OFFSET)))
	    (IF BITS-PER-ELEMENT
		;; Numeric array.
		(%BLT (%MAKE-POINTER-OFFSET DTP-FIX ARRAY
					    (- UNINITIALIZED-DATA-OFFSET BLT-DISTANCE))
		      (%MAKE-POINTER-OFFSET DTP-FIX ARRAY UNINITIALIZED-DATA-OFFSET)
		      (- DATA-LENGTH (- UNINITIALIZED-DATA-OFFSET DATA-OFFSET))
		      1)
	      (%BLT-TYPED (%MAKE-POINTER-OFFSET DTP-FIX ARRAY
						(- UNINITIALIZED-DATA-OFFSET BLT-DISTANCE))
			  (%MAKE-POINTER-OFFSET DTP-FIX ARRAY UNINITIALIZED-DATA-OFFSET)
			  (- DATA-LENGTH (- UNINITIALIZED-DATA-OFFSET DATA-OFFSET))
			  1))))))
  ORIGINAL-ARRAY)

))
