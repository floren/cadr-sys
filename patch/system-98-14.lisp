;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.14
;;; Reason: Changes in Common Lisp DEFSTRUCT.
;;; M-X Split Screen bugs.  C-X 4 J.  Undefined major mode bug.
;;; Flash parens forward as well as back.  C-M-B within comment bug.
;;; Editor mouse blips/:MOUSE-OR-KBD-TYI bugs.  Hyper character bugs.
;;; System E gets all the editors now.  Inspector list-modifying bugs.
;;; Reading and printing font #s in character objects.
;;; Written 12/24/83 00:08:21 by RMS,
;;; while running on Lisp Machine Two from band 1
;;; with Bad Inconsistently updated System 98.11, CADR 3.1, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 305, ZM MIT.


; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(DEFMACRO CLI:DEFSTRUCT (OPTIONS &BODY ITEMS)
  "(DEFSTRUCT (<name> . <options>) . <slots>) or (DEFSTRUCT <name> . <slots>)
Options:
  :TYPE specifies the type of data used to represent the structure.
    It also specifies that you do not get something TYPEP can recognize!
    Possible values are VECTOR, (VECTOR <vector-type>) and LIST, as well as the types
    available for non-common-lisp defstruct.
  :NAMED takes no value.  When this accompanies :TYPE,
    you get a phony named structure which has the type stored in it
    but which TYPEP still cannot recognize.
  :CONSTRUCTOR defaults to /"MAKE-<name>/"
    More than one constructor may be specified. The syntax for defining a constructor
    is either: (:CONSTRUCTOR <name>) or (:CONSTRUCTOR <name> <arglist>)
    If no arglist is supplied, a constructor is defined which takes alternating
     slotnames and values as arguments and initializes those slots to those values.
    If an arglist is supplied, then the constructor defined will have this as its
     arglist. Meaningful lambda-list-keywords are &OPTIONAL &REST and &AUX.
     Use &AUX to initialize a slot to a value other then the usual default value.
  :CONC-NAME defaults to <name>. This what to prepend to the names of slots to obtain
    the names of the slot-accessor functions
  :INCLUDE specifies a structure to include as a part of this structure.
  :INITIAL-OFFSET can cause defstruct to skip over that many slots.
  :PREDICATE defaults to <name>-p.  Generates a predicate if possible. Give this option
    a value of NIL if you don't want a predicate.
  :PRINT-FUNCTION The value should be a function of three arguments, the structure to be
    printed, the stream to print it on, and the current print-depth.
  Many other (non-common-lisp, perhaps non-portable) options are available.
    See the documentation for GLOBAL:DEFSTRUCT."
  (DEFSTRUCT-1 OPTIONS ITEMS T))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun defstruct-parse-options (options CLIP)
  (let ((name (if (atom options) options (car options)))
	(type nil)
	(constructors (make-empty))
	(alterant (make-empty))
	(included nil)
	(named-p CLIP)				;structures named by default in common lisp
	(but-first nil)
	(description (make-defstruct-description))
	(OLD))
    (setf (defstruct-description-name) name)
    (WHEN CLIP
      (SETF (DEFSTRUCT-DESCRIPTION-CONC-NAME) (DEFSTRUCT-APPEND-SYMBOLS NAME '-))
      (SETF (DEFSTRUCT-DESCRIPTION-PREDICATE) (MAKE-EMPTY))
      (SETF (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS) T))
    (do ((op) (val) (vals)
	 (options (if (atom options) nil (cdr options))
		  (cdr options)))
	((null options))
      (if (atom (setq op (car options)))
	  (setq vals nil)
	  (setq op (prog1 (car op) (setq vals (cdr op)))))
      (setq val (if (null vals) (make-empty) (car vals)))
      (SETQ OLD NIL)
      ;; If OP is not a keyword, change it to one and come back here.
    AGAIN
      (selectq op
	(:type
	 (if (emptyp val)
	     (defstruct-error
	       "The :TYPE option to DEFSTRUCT must have a value given"
	       name))
	 ;; In Common Lisp, :TYPE implies it is not a true named structure.
	 ;; It may be a phony one (slot allocated for the type, but not
	 ;; marked as a named structure), so if NAMED-P is already :PHONY leave it alone.
	 (IF (AND CLIP (EQ NAMED-P T))
	     (SETQ NAMED-P NIL))
	 (setq type val))
	(:default-pointer
	 (setf (defstruct-description-default-pointer)
	       (if (emptyp val) name val)))
	(:named
	 (or (emptyp val)
	     (defstruct-error
	       "The :NAMED option to DEFSTRUCT doesn't take a value" name))
	 ;; In Common Lisp, :NAMED means just allocate a slot for the name,
	 ;; do not make it a true named structure.
	 (setq named-p (IF CLIP ':PHONY T)))
	(:conc-name
	 (setf (defstruct-description-conc-name)
	       (if (emptyp val)
		   (IF CLIP NIL
		     (defstruct-append-symbols name '-))
		 val)))
	(:print
	 (if (emptyp val)
	     (defstruct-error
	       "The :PRINT option to DEFSTRUCT requires a value"
	       name))
	 (setf (defstruct-description-print) (CONS NIL VALS)))	;clisp nil
	(:PRINT-FUNCTION
	 (AND (EMPTYP VAL)
	      (DEFSTRUCT-ERROR
		"The :PRINT-FUNCTION option to DEFSTRUCT requires a value"
		NAME))
	 (AND (CDR VALS)			;check against using :print syntax
	      (DEFSTRUCT-ERROR
		"The :PRINT-FUNCTION option to DEFSTRUCT takes only one value"))
	 (SETF (DEFSTRUCT-DESCRIPTION-PRINT) (CONS T VALS)))	;clisp t
	(:include
	 (if (emptyp val)
	     (defstruct-error
	       "The :INCLUDE option to DEFSTRUCT requires a value"
	       name))
	 (setq included val)
	 (setf (defstruct-description-include) vals))
	(:predicate
	 (setf (defstruct-description-predicate)
	       (if (emptyp val)
		   (defstruct-append-symbols name '-p)
		   val)))
	(:constructor
	 (cond ((null val)
		(setq constructors nil))
	       (t
		(and (emptyp val)
		     (setq val (defstruct-append-symbols 'make- name)))
		(setq val (cons val (cdr vals)))
		(if (emptyp constructors)
		    (setq constructors (list val))
		    (push val constructors)))))
	(:copier
	 (setf (defstruct-description-copier)
	       (if (emptyp val)
		   (defstruct-append-symbols 'copy- name)
		   val)))
	#-(OR LISPM NIL)
	(:eval-when
	 (and (emptyp val)
	      (defstruct-error
		"The :EVAL-WHEN option to DEFSTRUCT requires a value"
		name))
	 (setf (defstruct-description-eval-when) val))
	(:alterant
	 (setq alterant val))
	(:but-first
	 (if (emptyp val)
	     (defstruct-error
	       "The :BUT-FIRST option to DEFSTRUCT must have a value given"
	       name))
	 (setq but-first val)
	 (setf (defstruct-description-but-first) val))
	(:size-macro
	 (setf (defstruct-description-size-macro)
	       (if (emptyp val)
		   (defstruct-append-symbols name '-size)
		   val)))
	(:size-symbol
	 (setf (defstruct-description-size-symbol)
	       (if (emptyp val)
		   (defstruct-append-symbols name '-size)
		   val)))
	(:callable-accessors
	 (setf (defstruct-description-callable-accessors)
	       (if (emptyp val) t val)))
	(:CALLABLE-CONSTRUCTORS
	 (SETF (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS)
	       (IF (EMPTYP VAL) T VAL)))
	(:property
	 (if (emptyp val)
	     (defstruct-error
	       "The :PROPERTY option to DEFSTRUCT requires a value"
	       name))
	 (push (cons val (if (null (cdr vals)) t (cadr vals)))
	       (defstruct-description-property-alist)))
	(:initial-offset
	 (and (or (emptyp val)
		  (not (fixp val)))
	      (defstruct-error
		"The :INITIAL-OFFSET option to DEFSTRUCT requires a fixnum"
		name))
	 (setf (defstruct-description-initial-offset) val))
	(t
	 (cond ((get op 'defstruct-type-description)
		(or (emptyp val)
		    (defstruct-error
		      "DEFSTRUCT type used as an option with a value"
		      op 'in name))
		(setq type op))
	       (T
		(IF OLD (SETQ OP OLD)
		  (LET ((NEW (DEFSTRUCT-RETRY-KEYWORD OP)))
		    (UNLESS (EQ NEW OP)
		      (SETQ OLD OP OP NEW)
		      (GO AGAIN))))
		(PUSH (CONS NIL (CONS OP (IF (EMPTYP VAL) T VAL))) ;nil flags not explicit
		      (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST)))))))
    (WHEN (AND CLIP (EMPTYP (DEFSTRUCT-DESCRIPTION-PREDICATE)))
      (SETF (DEFSTRUCT-DESCRIPTION-PREDICATE)
	    (AND NAMED-P (DEFSTRUCT-APPEND-SYMBOLS NAME '-P))))	    
    (if (emptyp constructors)
	(setq constructors (list (cons (defstruct-append-symbols 'make- name)
				       nil))))
    (setf (defstruct-description-constructors) constructors)
    (cond ((emptyp alterant)
	   (SETQ ALTERANT (IF CLIP NIL
			   (defstruct-append-symbols 'alter- name)))))
    (setf (defstruct-description-alterant) alterant)
    (WHEN TYPE
      (WHEN (CONSP TYPE)
	(SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) (CADR TYPE))
	(SETQ TYPE (CAR TYPE)))
      (UNLESS (KEYWORDP TYPE)
	(SETQ TYPE (DEFSTRUCT-RETRY-KEYWORD TYPE)))
      (LET ((TYPE-DESCRIPTION (OR (GET TYPE 'DEFSTRUCT-TYPE-DESCRIPTION)
				  (DEFSTRUCT-ERROR
				    "Unknown type in DEFSTRUCT"
				    TYPE 'IN NAME))))
	(if named-p
	    (setq type
		  (or (defstruct-type-description-named-type)
		      (defstruct-error
			"There is no way to make a :NAMED defstruct of this type"
			type 'in name))))))
    (cond (included
	     (let ((d (get-defstruct-description included)))
	       (if (null type)
		   (setq type (defstruct-description-type d))
		 (or (eq type (defstruct-description-type d))
		     (defstruct-error
		       "defstruct types must agree for :INCLUDE option"
		       included 'included 'by name)))
	       (and named-p
		    (NEQ type (defstruct-type-description-named-type
				(or (get type 'defstruct-type-description)
				    (defstruct-error
				      "Unknown type in DEFSTRUCT"
				      type 'in name 'including included))))
		    (defstruct-error
		      ":INCLUDEd defstruct's type isn't a named type"
		      included 'included 'by name))
	       (if (null but-first)
		   (setf (defstruct-description-but-first)
			 (defstruct-description-but-first d))
		 (or (equal but-first (defstruct-description-but-first d))
		     (defstruct-error
		       ":BUT-FIRST options must agree for :INCLUDE option"
		       included 'included 'by name)))))
	  ((null type)
	   (setq type
	     (cond ((EQ NAMED-P ':PHONY)
		    ':PHONY-NAMED-VECTOR)
		   (named-p
		    #+MacLisp-10 ':named-hunk
		    #+Multics ':named-list
		    #+LispM (IF CLIP ':NAMED-VECTOR ':named-array)
		    #+NIL ':extend)
		   (t
		    #+MacLisp-10 ':hunk
		    #+Multics ':list
		    #+LispM (IF CLIP ':VECTOR ':array)
		    #+NIL ':vector)))))
    (let ((type-description (or (get type 'defstruct-type-description)
				(defstruct-error
				  "Undefined defstruct type"
				  type 'in name))))
      (setf (defstruct-description-type) type)
      (setf (defstruct-description-named-p)
	    (eq (defstruct-type-description-named-type) type))
      (OR (DEFSTRUCT-DESCRIPTION-NAMED-P)
	  (NULL (DEFSTRUCT-DESCRIPTION-PRINT))
	  (EMPTYP (DEFSTRUCT-DESCRIPTION-PRINT))
	  (DEFSTRUCT-ERROR
	    ":PRINT or :PRINT-FUNCTION is allowed only for recognizable named structures" NAME))
      (DO ((X (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST) (CDR X)))	;check validity
	  ((NULL X))
	(OR (CAAR X)				;defined explicitly via (:property foo bar)
	    (IF (OR (MEMQ (CADAR X) (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS))
		    (GET (CADAR X) ':DEFSTRUCT-OPTION))	;obsolete form
		(SETF (CAR X) (CDAR X))
	      (DEFSTRUCT-ERROR
		"DEFSTRUCT doesn't understand this option"
		(CAR X) 'IN NAME))))
      (OR (MEMQ ':SUBTYPE (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS))
	  (SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) NIL)))
    description))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "


#+LISPM
(DEFSTRUCT-DEFINE-TYPE :VECTOR			;same as :TYPED-ARRAY
  (:NAMED :PHONY-NAMED-VECTOR)			;except for this
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
			       DESCRIPTION ETC NIL NIL NIL 1 NIL))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION		;ignored
    `(AREF ,ARG ,N)))

#+LISPM
(DEFSTRUCT-DEFINE-TYPE :PHONY-NAMED-VECTOR
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
			       DESCRIPTION ETC NIL NIL NIL 1 T))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION		;ignored
    `(AREF ,ARG ,N))
  (:PREDICATE (DESCRIPTION NAME)
    `(DEFSUBST ,NAME (X)
       (AND (ARRAYP X) (ARRAY-HAS-LEADER-P X)
	    (EQ (ARRAY-LEADER X 1) ',(DEFSTRUCT-DESCRIPTION-NAME))))))

))

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defun lispm-array-for-defstruct (arg
				  cons-init
				  description
				  etc
				  type
				  &OPTIONAL (NAMED-P NIL)
					    (LEADER-P NIL)
					    (TIMES 1)
					    (TYPE-IN-LEADER NIL)
				  &AUX (P (CONS NIL NIL))
				       NO-OP
				       ARRAY-TYPE)
;arg is slot arg
;cons-init is code to initialize the structure per-slot
;description is a structure description
;etc is cons-keyword args/values
;type is the array-type to make
;named-p is t if to make a named structure
;leader-p is t if the data is to be stored in the leader (as in :{named-}array-leader)
;times if the #times for :grouped-array
;type-in-leader is t if the structure-type is to be put in array-leader 1 rather than
; in aref 0
  (defstruct-grok-make-array-args
    (cdr (assq ':make-array (defstruct-description-property-alist)))
    p)
  (defstruct-grok-make-array-args
    (cdr (assq ':make-array etc))
    p)
  (COND (TYPE
	 (PUTPROP P TYPE ':TYPE))
	((SETQ TYPE (CDR (ASSQ ':SUBTYPE ETC)))
	 (PUTPROP P `',(SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE TYPE T)) ':TYPE))
	((SETQ TYPE (DEFSTRUCT-DESCRIPTION-SUBTYPE))
	 (PUTPROP P `',(SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE TYPE T)) ':TYPE)))
  (and named-p (putprop p `',(defstruct-description-name) ':named-structure-symbol))
  (LET* ((S (OR (GET P (IF LEADER-P ':LEADER-LENGTH ':DIMENSIONS)) 0))
	 (SIZE (let ((size (if named-p
			       (1+ (defstruct-description-size))
			     (defstruct-description-size))))
		 (if (numberp times)
		     (MAX S (* size times))
		   `(MAX ,S (* ,size ,times))))))
    (putprop p SIZE (if leader-p ':leader-length ':dimensions)))
  (AND TYPE-IN-LEADER (OR (NOT (GET P ':LEADER-LENGTH))
			  (< (GET P ':LEADER-LENGTH) 2))
       (PUTPROP P 2 ':LEADER-LENGTH))
  (SETQ ARRAY-TYPE (OR (LET ((TYPE (GET P ':TYPE)))
			 (OR (ATOM TYPE)
			     (NEQ (CAR TYPE) 'QUOTE)
			     (SETQ TYPE (CADR TYPE)))
			 (ARRAY-CANONICALIZE-TYPE TYPE))
		       'ART-Q))
  (OR LEADER-P
      (IF (OR (GET P ':INITIAL-ELEMENT)
	      (GET P ':INITIAL-VALUE))
	  (SETQ NO-OP (MAKE-EMPTY))
	(SETQ NO-OP (SELECTQ ARRAY-TYPE
		      ((NIL ART-Q ART-Q-LIST) NIL)
		      ((ART-32B ART-16B ART-8B ART-4B ART-2B ART-1B ART-HALF-FIX
				ART-STRING ART-FAT-STRING)
		       0)
		      ((ART-FLOAT ART-FPS-FLOAT)
		       0.0)
		      (ART-COMPLEX
		       (COMPLEX 0 0))
		      ((ART-COMPLEX-FLOAT ART-COMPLEX-FPS-FLOAT)
		       (COMPLEX 0.0 0.0))
		      (T (MAKE-EMPTY))))))
  ;;make sure that we can store tha named-structure-symbol safely
  (OR (NOT NAMED-P)
      (MEMQ ARRAY-TYPE '(ART-Q ART-Q-LIST ART-SPECIAL-PDL ART-REG-PDL ART-STACK-GROUP-HEAD))
      (GET P ':LEADER-LENGTH)
      (SETQ ARRAY-TYPE 'ART-Q)
      (PUTPROP P 'ART-Q ':TYPE))
  (do ((creator
	 (let ((dims (remprop p ':dimensions)))
	   (do ((l (cdr p) (cddr l)))
	       ((null l))
	     (rplaca l `',(car l)))
	   `(make-array ,(if (null dims) 0 (car dims)) ,@(cdr p))))
       (var (gensym))
       (set-ups nil (if (equal (cdar l) no-op)
			set-ups
		      (PUSH (funcall cons-init (cdar l) var (caar l)) SET-UPS)))
       (l arg (cdr l)))
      ((null l)
       ;; If we want the structure type stored but not a named-structure,
       ;; generate code to store it explicitly.
       (if (and type-in-leader (not named-p))
	   (push `(setf (array-leader ,var 1) ',(defstruct-description-name))
		 set-ups))
       (if set-ups
	   `((lambda (,var)
	       ,@(nreverse set-ups)
	       ,var)
	     ,creator)
	 creator))))

))

; From file SYSMEN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SYSMEN  "

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :MOVE-NEAR-WINDOW) (WINDOW &OPTIONAL (DIMENSIONS '(1 . 1)))
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (FUNCALL WINDOW ':EDGES)
    (LET ((NEW-WIDTH (TRUNCATE (* (CAR DIMENSIONS) (- BOTTOM TOP)) (CDR DIMENSIONS)))
	  (NEW-HEIGHT (- BOTTOM TOP))
	  (SLEFT (SHEET-INSIDE-LEFT SUPERIOR))
	  (SRIGHT (SHEET-INSIDE-RIGHT SUPERIOR))
	  (NTOP TOP)
	  (NBOTTOM BOTTOM)
	  NLEFT NRIGHT NCENTER)
      (COND (( (SETQ NLEFT (- LEFT NEW-WIDTH)) SLEFT)
	     (SETQ NRIGHT LEFT))	;Fits on the left
	    ((< (SETQ NRIGHT (+ RIGHT NEW-WIDTH)) SRIGHT)
	     (SETQ NLEFT RIGHT))	;Fits on the right
	    (T
	     ;; Make it short enough to fit either above or below,
	     ;; and scale the width to match.
	     (SETQ NEW-HEIGHT
		   (MAX (- TOP (SHEET-INSIDE-TOP SUPERIOR))
			(- (SHEET-INSIDE-BOTTOM SUPERIOR) BOTTOM))
		   NEW-WIDTH (TRUNCATE (* (CAR DIMENSIONS) NEW-HEIGHT) (CDR DIMENSIONS)))
	     (SETQ NCENTER (TRUNCATE (+ SLEFT SRIGHT) 2)
		   NLEFT (- NCENTER (TRUNCATE NEW-WIDTH 2))
		   NRIGHT (+ NCENTER (TRUNCATE NEW-WIDTH 2)))
	     (COND ((< (SETQ NBOTTOM (+ BOTTOM NEW-HEIGHT)) (SHEET-INSIDE-BOTTOM SUPERIOR))
		    (SETQ NTOP BOTTOM))
		   (( (SETQ NTOP (- TOP NEW-HEIGHT)) (SHEET-INSIDE-TOP SUPERIOR))
		    (SETQ NBOTTOM TOP))
		   (T (FERROR NIL "Insufficient room to display layout window.")))))
      (FUNCALL-SELF ':SET-EDGES NLEFT NTOP NRIGHT NBOTTOM)))
  (FUNCALL-SELF ':EXPOSE))

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFUN READ-BUFFER-NAME-NEAR-WINDOW (WINDOW PROMPT DEFAULT &OPTIONAL IMPOSSIBLE-IS-OK-P)
  (USING-RESOURCE (W TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS-RESOURCE)
    (FUNCALL W ':CALL-MINI-BUFFER-NEAR-WINDOW WINDOW
	     #'READ-BUFFER-NAME PROMPT DEFAULT IMPOSSIBLE-IS-OK-P
	     (SEND *WINDOW* ':BUFFER-HISTORY))))

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN READ-BUFFER-NAME (PROMPT DEFAULT &OPTIONAL IMPOSSIBLE-IS-OK-P
			 (BUFFER-HISTORY (SEND *WINDOW* ':BUFFER-HISTORY)))
  "Read a buffer name in the mini buffer and return a buffer.
DEFAULT is the default to use; T means use most recent buffer other than the current one.
PROMPT is a string to prompt with; should end in a colon if you want one.
IMPOSSIBLE-IS-OK can be T, NIL or MAYBE.
 T means create a new buffer if name does not complete.
 ZWEI:MAYBE means do so, but user must type Return twice to confirm.
 NIL means don't allow names that don't complete, at all.
BUFFER-HISTORY is a history object containing buffers.
 It defaults to *WINDOW*'s buffer history."
  (AND (EQ DEFAULT T)	; Select most recent buffer other than this one
       (SETQ DEFAULT (PREVIOUS-BUFFER)))
  (AND DEFAULT
       (SETQ PROMPT (STRING-APPEND PROMPT
				   " ("
				   (BUFFER-NAME DEFAULT)
				   " ... C-Shift-F to specify filename)")))
  (AND DEFAULT
       (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* DEFAULT))	;In case of C-Shift-F.
  (LET* ((*READ-BUFFER-KLUDGE* T)
	 (*MINI-BUFFER-DEFAULT-STRING* (if default (BUFFER-NAME DEFAULT)))
	 (*COMPLETING-DELIMS* '(#\SP #/- #/. #/\ #// #/#))
	 (*MINI-BUFFER-VALUE-HISTORY* BUFFER-HISTORY)
	 (NAME (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *ZMACS-BUFFER-NAME-ALIST*
						 IMPOSSIBLE-IS-OK-P)))
    (COND ((EQUAL NAME "") (SETQ NAME DEFAULT))
	  ((CONSP NAME) (SETQ NAME (CDR NAME))))		;Existing buffer
    (COND ((NULL NAME) (BARF))
	  ((STRINGP NAME)
	   (FORMAT QUERY-IO "~&(New Buffer)")
	   (SETQ NAME (CREATE-ONE-BUFFER-TO-GO NAME)))
	  (T
	   (FORMAT QUERY-IO "~&")))
    NAME))	;which by now is a buffer

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-SYS-1 (CH &AUX E W SW MAKENEW FLAVOR-OR-WINDOW SW-ALIAS)
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
	   (SETQ SW SELECTED-WINDOW)
	   (WHEN SW (SETQ SW-ALIAS (SEND SW ':ALIAS-FOR-SELECTED-WINDOWS)))
	   (COND ((TYPEP FLAVOR-OR-WINDOW 'ESSENTIAL-WINDOW)
		  ;; If the *SYSTEM-KEYS* list has a specific window indicated, use that.
		  (AND SW (FUNCALL SW ':DESELECT NIL))
		  (FUNCALL FLAVOR-OR-WINDOW ':MOUSE-SELECT))
		 ((NULL FLAVOR-OR-WINDOW) NIL)  ;NIL means he already did whatever he wanted.
		 ((AND (NOT MAKENEW)
		       (SETQ W (FIND-WINDOW-OF-FLAVOR FLAVOR-OR-WINDOW)))
		  ;; Cycle through other windows of this flavor.
		  (WHEN SW
		    (FUNCALL SW ':DESELECT
			     (IF (TYPEP SW-ALIAS FLAVOR-OR-WINDOW)
				 ':END)))
		  (FUNCALL W ':MOUSE-SELECT))
		 ((AND (NOT MAKENEW) SW
		       (TYPEP SW-ALIAS FLAVOR-OR-WINDOW))
		  ;; There is only one window of this flavor, and this is it.
		  (BEEP))
		 ((NULL (FOURTH E)) (BEEP))	;Cannot create
		 ((NLISTP (FOURTH E))
		  ;; Create a new window of this flavor.
		  ;; We create on the default screen.
		  (AND SW (FUNCALL SW ':DESELECT
				   (IF (TYPEP SW-ALIAS FLAVOR-OR-WINDOW)
				       ':END)))
		  (FUNCALL (MAKE-WINDOW (IF (EQ (FOURTH E) T) FLAVOR-OR-WINDOW (FOURTH E))
					':SUPERIOR DEFAULT-SCREEN)
			   ':MOUSE-SELECT))
		 (T (EVAL (FOURTH E))))))
	(( CH #\RUBOUT) (BEEP)))
  (SETQ KBD-ESC-TIME NIL))

))

; From file SCREEN.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SCREEN  "

(DEFCOM COM-MODIFIED-TWO-WINDOWS "Find a buffer, file or tag in the other window." ()
  (LET (CHAR)
    (FORMAT QUERY-IO "~&Buffer, File, Jump or Tag (B, F, J or T): ")
    (DO ()
	((NEQ (SETQ CHAR (CHAR-UPCASE
			   (TYPEIN-LINE-ACTIVATE
			     (LDB %%CH-CHAR (FUNCALL STANDARD-INPUT ':TYI)))))
	      #\HELP))
      (SEND QUERY-IO ':CLEAR-SCREEN)
      (FORMAT QUERY-IO "~&Four ways to specify the buffer to display in the other window:
B and a buffer name, F and a file name, J and a register name,
or T or Period and the name of a section (as in Meta-Period).  B, F, J or T: "))
    (SELECTQ CHAR
      (#/B (LET ((BUFFER (READ-BUFFER-NAME "Select buffer:" T 'MAYBE)))
	     (SWITCH-WINDOWS)
	     (MAKE-BUFFER-CURRENT BUFFER)
	     DIS-TEXT))
      (#/F (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file:" (PATHNAME-DEFAULTS)
						    NIL NIL ':NEW-OK)))
	     (SWITCH-WINDOWS)
	     (FIND-FILE PATHNAME))
	   (MAYBE-DISPLAY-DIRECTORY ':READ)
	   DIS-TEXT)
      (#/J
       (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
	 (LET ((PT (GET Q-REG 'POINT)))
	   (COND ((NULL PT)
		  (BARF "The register ~A doesn't point anywhere." Q-REG)))
	   (SWITCH-WINDOWS)
	   (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
	   (MAKE-BUFFER-CURRENT (CDR PT))
	   (MOVE-BP (POINT) (CAR PT))))
       DIS-BPS)
      ((#/T #/.)
       (IF *NUMERIC-ARG-P*
	   (PROGN (SWITCH-WINDOWS)
		  (EDIT-NEXT-DEFINITION))
	 (LET ((SPEC
		 (READ-FUNCTION-NAME "Edit function" (RELEVANT-FUNCTION-NAME (POINT))
				     'AARRAY-OK)))
	   (SWITCH-WINDOWS)
	   (EDIT-DEFINITION SPEC)))
       DIS-NONE)
      (OTHERWISE (SEND QUERY-IO ':MAKE-COMPLETE)
		 (BARF)))))

))

; From file ZMNEW.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "

(DEFUN REPARSE-BUFFER-MODE-LINE (BUFFER)
  "Reparse BUFFER's attribute list line and set the buffer's recorded attributes."
  (FS:READ-ATTRIBUTE-LIST BUFFER (INTERVAL-STREAM BUFFER))
  ;; Forget (and thereby override) any Set Package previously done.
  (SETF (BUFFER-PACKAGE BUFFER) NIL)
  (INITIALIZE-BUFFER-PACKAGE BUFFER)
  (SEND BUFFER ':SET-MAJOR-MODE
	(OR (GET-FILE-MAJOR-MODE (OR (FUNCALL BUFFER ':GET-ATTRIBUTE ':MODE)
				     *DEFAULT-MAJOR-MODE*))
	    'FUNDAMENTAL-MODE))
  (LET* (FONTS (*INTERVAL* BUFFER)) ;Must not be bound around the :SET-MAJOR-MODE!
    (SETQ FONTS (SET-BUFFER-FONTS BUFFER))
    (DOLIST (W (SEND BUFFER ':WINDOWS))
      (REDEFINE-FONTS W FONTS
		      (FUNCALL BUFFER ':GET-ATTRIBUTE ':VSP))
      (REDEFINE-WINDOW-OVERPRINTING-FLAG W
					 (SEND BUFFER ':GET-ATTRIBUTE ':BACKSPACE))
      (REDEFINE-WINDOW-TAB-NCHARS W
				  (SEND BUFFER ':GET-ATTRIBUTE ':TAB-WIDTH)))
    (COND ((AND *WINDOW* (EQ BUFFER (WINDOW-INTERVAL *WINDOW*)))
	   (COMPUTE-BUFFER-PACKAGE BUFFER)))))

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN REVERT-FILE-BUFFER (BUFFER PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG
			   &AUX GENERIC-PATHNAME PATHNAME-STRING TRUENAME NEW-MODE)
  (COND ((AND (NULL (BUFFER-FILE-ID BUFFER)) (NULL PATHNAME))
	 (BARF "The buffer ~A is not associated with a file." (BUFFER-NAME BUFFER))))
  (MULTIPLE-VALUE (PATHNAME PATHNAME-STRING)
    (EDITOR-FILE-NAME PATHNAME))
  (COND (CONNECT-FLAG
	 (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
	 (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)))
  (SETQ GENERIC-PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME))
  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
  (WITH-OPEN-FILE-CASE (STREAM PATHNAME)
    (:NO-ERROR
     (SETQ TRUENAME (FUNCALL STREAM ':TRUENAME))
     (WHEN (MEMQ (FUNCALL PATHNAME ':TYPE) '(NIL :UNSPECIFIC))
       (MULTIPLE-VALUE (PATHNAME PATHNAME-STRING)
	 (EDITOR-FILE-NAME
	   (IF (EQUALP (SEND TRUENAME ':NAME) (SEND PATHNAME ':NAME))
	       ;; This is in case user reads FOO > from an ITS, and it is reall FOO BAR.
	       (FUNCALL PATHNAME ':NEW-TYPE
			(FUNCALL TRUENAME ':TYPE))
	     ;; This case if user read FOO BAR from an LMFILE, and truename is FOO|BAR.
	     ;; Or if user reads FOO BAR from an ITS and it is a link to UGH QUUX.
	     PATHNAME))))
     (COND (CONNECT-FLAG
	    (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
	    (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
	    (SIMILAR-BUFFER-FILES-WARNING BUFFER)))
     (OR QUIETLY-FLAG (FORMAT QUERY-IO "~&Reading ~A" TRUENAME))
     (LET ((THIS-VERSION (FUNCALL TRUENAME ':VERSION))
	   (INSTALLED-TRUENAME (FILE-LOADED-TRUENAME TRUENAME))
	   INSTALLED-VERSION)
       (AND INSTALLED-TRUENAME
	    (NUMBERP THIS-VERSION)
	    (NUMBERP (SETQ INSTALLED-VERSION (FUNCALL INSTALLED-TRUENAME ':VERSION)))
	    (NOT QUIETLY-FLAG)
	    ( INSTALLED-VERSION THIS-VERSION)
	    (FORMAT QUERY-IO " (installed version is ~D)" INSTALLED-VERSION)))
     (FS:READ-ATTRIBUTE-LIST BUFFER STREAM)
     ;; Forget (and thereby override) and previouse Set Package in this buffer.
     (SETF (BUFFER-PACKAGE BUFFER) NIL)
     ;; And recompute from latest attribute list.
     (INITIALIZE-BUFFER-PACKAGE BUFFER)
     (UNLESS (SEND BUFFER ':GET-ATTRIBUTE ':MODE)
       (SEND BUFFER ':SET-ATTRIBUTE 
	     ':MODE
	     (OR (CDR (ASSOC (FUNCALL PATHNAME ':CANONICAL-TYPE)
			     FS:*FILE-TYPE-MODE-ALIST*))
		 *DEFAULT-MAJOR-MODE*)))
     (SETQ NEW-MODE (OR (GET-FILE-MAJOR-MODE (FUNCALL BUFFER ':GET-ATTRIBUTE ':MODE))
			'FUNDAMENTAL-MODE))
     (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
       (IF (EQ BUFFER *INTERVAL*)
	   (COMPUTE-BUFFER-PACKAGE BUFFER))
       (AND NEW-MODE
	    (SEND BUFFER ':SET-MAJOR-MODE NEW-MODE)))
     (PRESERVE-BUFFER-POINT (BUFFER)
       (WITH-READ-ONLY-SUPPRESSED (BUFFER)
	 (LET ((*BATCH-UNDO-SAVE* T))		;Don't save all this for undo!
	   (DISCARD-UNDO-INFORMATION BUFFER)
	   (DELETE-INTERVAL BUFFER)
	   (SETF (BUFFER-TICK BUFFER) (TICK))	;For SECTIONIZE-BUFFER
	   (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
	   (LET ((FONTS (SET-BUFFER-FONTS BUFFER))
		 FONTS-P)
	     (SETQ FONTS-P (OR (CDR FONTS) (FUNCALL BUFFER ':GET-ATTRIBUTE ':DIAGRAM)))
	     (WHEN SELECT-FLAG
	       (SEND BUFFER ':ACTIVATE)
	       (MAKE-BUFFER-CURRENT BUFFER)
	       ;; If it is requested, read in the first screenful and then redisplay.
	       (DOTIMES (I (+ 5 (WINDOW-N-PLINES *WINDOW*)))
		 (MULTIPLE-VALUE-BIND (LINE EOFFLG)
		     (FUNCALL STREAM ':LINE-IN LINE-LEADER-SIZE)
		   (WHEN LINE
		     (INSERT-LINE-WITH-LEADER LINE
					      (BP-LINE (INTERVAL-LAST-BP BUFFER))))
		   (IF EOFFLG (RETURN))))
	       (REDISPLAY *WINDOW* ':START (INTERVAL-FIRST-BP BUFFER) NIL))
	     (IF (NOT CONNECT-FLAG)
		 (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
	       (IF (EQ CONNECT-FLAG 'NOSECTIONIZE)
		   (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
		 (SECTIONIZE-FILE-BUFFER BUFFER *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS
					 NIL NIL
					 STREAM FONTS-P))
	       (SET-BUFFER-FILE-ID BUFFER (FUNCALL STREAM ':INFO))
	       (DOLIST (WINDOW (SEND BUFFER ':WINDOWS))
		 (AND FONTS
		      (REDEFINE-FONTS WINDOW FONTS
				      (SEND BUFFER ':GET-ATTRIBUTE ':VSP)))
		 (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
						    (FUNCALL BUFFER ':GET-ATTRIBUTE ':BACKSPACE))
		 (REDEFINE-WINDOW-TAB-NCHARS WINDOW
					     (SEND BUFFER ':GET-ATTRIBUTE ':TAB-WIDTH))))
	     (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
	     (NOT-MODIFIED BUFFER)))))
     (UNLESS QUIETLY-FLAG
       (IF (SEND STREAM ':OPERATION-HANDLED-P ':READ-POINTER)
	   (LET ((NCHARS (SEND STREAM ':READ-POINTER)))
	     (IF (< NCHARS 5000.)
		 (FORMAT QUERY-IO " -- ~D characters." NCHARS)
	       (FORMAT QUERY-IO " -- ~DK characters." (ROUND NCHARS 1024.))))
	 (FORMAT QUERY-IO " -- done."))))
    (FS:FILE-NOT-FOUND
     (WHEN *FIND-FILE-NOT-FOUND-IS-AN-ERROR* (BARF STREAM))
     (OR QUIETLY-FLAG (FORMAT QUERY-IO "(New File)"))
     (LET ((*BATCH-UNDO-SAVE* T))
       (DISCARD-UNDO-INFORMATION BUFFER)
       (DELETE-INTERVAL BUFFER))
     (AND CONNECT-FLAG (SET-BUFFER-FILE-ID BUFFER T))
     (SEND BUFFER ':SET-ATTRIBUTE 
	   ':MODE
	   (OR (CDR (ASSOC (FUNCALL PATHNAME ':CANONICAL-TYPE)
			   FS:*FILE-TYPE-MODE-ALIST*))
	       *DEFAULT-MAJOR-MODE*))
     (SETF (BUFFER-PACKAGE BUFFER) (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* PACKAGE)))
     (LET ((MODE (GET-FILE-MAJOR-MODE (FUNCALL BUFFER ':GET-ATTRIBUTE ':MODE))))
       (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
	 (IF (EQ BUFFER *INTERVAL*) (COMPUTE-BUFFER-PACKAGE BUFFER))
	 (AND MODE (SEND BUFFER ':SET-MAJOR-MODE MODE)))))
    (FS:FILE-ERROR (BARF STREAM)))
  (SETF (BUFFER-TICK BUFFER) (TICK)))		;Buffer is same as file

))

; From file MACROS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MACROS  "

(DEFVARIABLE *FLASH-MATCHING-PAREN-MAX-LINES* 200. :FIXNUM
   "Max number of lines to scan when trying to flash the matching open paren.")

))

; From file DISPLA.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "
(DEFUN BLINK-MATCHING-PAREN (BLINKER WINDOW POINT WINDOW-START-BP
			     &AUX BP X Y OPEN CLOSE)
  (COND ((AND (= (LIST-SYNTAX (SETQ CLOSE (BP-CHAR-BEFORE POINT))) LIST-CLOSE)
	      *FLASH-MATCHING-PAREN*
	      ;; Don't waste time if start of defun is very very far.
	      (< (COUNT-LINES (FORWARD-DEFUN POINT -1 T) POINT T)
		 *FLASH-MATCHING-PAREN-MAX-LINES*)
	      (SETQ BP (FORWARD-SEXP POINT -1 NIL 0 WINDOW-START-BP NIL))
	      (MULTIPLE-VALUE-BIND (NIL SLASHIFIED COMMENT)
		  (LISP-BP-SYNTACTIC-CONTEXT (FORWARD-CHAR POINT -1 T) BP)
		(AND (NOT SLASHIFIED) (NOT COMMENT))))
	 (SETQ OPEN (BP-CH-CHAR BP)) ; (LDB %%CH-CHAR (AREF (BP-LINE BP) INDEX)))
	 ;; checks paren type match if open paren on screen
	 (AND (NEQ (SECOND (ASSQ OPEN *MATCHING-DELIMITER-LIST*)) (LDB %%CH-CHAR CLOSE))
	      (NOT (TV:SHEET-ME-OR-MY-KID-P *WINDOW* *MODE-LINE-WINDOW*))
	      (PROGN (BEEP) (FORMAT QUERY-IO "Non-matching parenthesis.")))
	 ;; NOW move back over singlequotes.
	 (SETQ BP (BACKWARD-LEADING-SINGLE-QUOTES BP WINDOW-START-BP))
	 (SETQ OPEN (BP-CHAR BP))
	 (COND ((PROGN (MULTIPLE-VALUE (X Y) (FIND-BP-IN-WINDOW-COORDS BP WINDOW)) X)
		(LET* ((SHEET (WINDOW-SHEET WINDOW))
		       (FONT (AREF (TV:SHEET-FONT-MAP SHEET) (LDB %%CH-FONT OPEN)))
		       (LKT (FONT-LEFT-KERN-TABLE FONT)))
		  (AND LKT (SETQ X (- X (AREF LKT (LDB %%CH-CHAR OPEN)))))
		  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
		  (WITHOUT-INTERRUPTS
		    (TV:BLINKER-SET-CHARACTER BLINKER FONT (LDB %%CH-CHAR OPEN))
		    (TV:BLINKER-SET-CURSORPOS BLINKER X Y)
		    (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK)) T))))
	((AND (= (LIST-SYNTAX (SETQ OPEN (BP-CH-CHAR POINT))) LIST-OPEN)
	      *FLASH-MATCHING-PAREN*
	      ;; Don't waste time if start of defun is very very far.
	      (< (COUNT-LINES (FORWARD-DEFUN POINT -1 T) POINT T)
		 *FLASH-MATCHING-PAREN-MAX-LINES*)
	      (MULTIPLE-VALUE-BIND (NIL SLASHIFIED COMMENT)
		  (LISP-BP-SYNTACTIC-CONTEXT POINT BP)
		(AND (NOT SLASHIFIED) (NOT COMMENT)))
	      (SETQ BP (FORWARD-SEXP POINT 1 NIL 0
				     ;; Don't look past just below the bottom of the screen.
				     (LET ((END-LINE
					     (PLINE-LINE *WINDOW*
							 (1- (WINDOW-N-PLINES *WINDOW*)))))
				       (AND END-LINE
					    (LINE-NEXT END-LINE)
					    (CREATE-BP (LINE-NEXT END-LINE) 0)))
				     NIL)))
	 (SETQ CLOSE (BP-CHAR-BEFORE BP))
	 ;; checks paren type match if open paren on screen
	 (AND (NEQ (SECOND (ASSQ OPEN *MATCHING-DELIMITER-LIST*)) (LDB %%CH-CHAR CLOSE))
	      (NOT (TV:SHEET-ME-OR-MY-KID-P *WINDOW* *MODE-LINE-WINDOW*))
	      (PROGN (BEEP) (FORMAT QUERY-IO "Non-matching parenthesis.")))
	 ;; Now move past trailing singlequote-like characters.
	 (DO () (( (LIST-SYNTAX (LDB %%CH-CHAR (BP-CHAR BP)))
		    LIST-SINGLE-QUOTE))
	    (IBP BP))
	 (SETQ CLOSE (BP-CHAR-BEFORE BP))
	 (COND ((PROGN (MULTIPLE-VALUE (X Y) (FIND-BP-IN-WINDOW-COORDS (DBP BP) WINDOW)) X)
		(LET* ((SHEET (WINDOW-SHEET WINDOW))
		       (FONT (AREF (TV:SHEET-FONT-MAP SHEET) (LDB %%CH-FONT CLOSE)))
		       (LKT (FONT-LEFT-KERN-TABLE FONT)))
		  (AND LKT (SETQ X (- X (AREF LKT (LDB %%CH-CHAR CLOSE)))))
		  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
		  (WITHOUT-INTERRUPTS
		    (TV:BLINKER-SET-CHARACTER BLINKER FONT (LDB %%CH-CHAR CLOSE))
		    (TV:BLINKER-SET-CURSORPOS BLINKER X Y)
		    (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK)) T))))
	(T (TV:BLINKER-SET-VISIBILITY BLINKER NIL))))

))

; From file LPARSE.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; LPARSE  "

(DEFUN FORWARD-SEXP (BP &OPTIONAL (TIMES 1) FIXUP-P
				  (LEVEL 0) STOP-BP (MOVE-OVER-SINGLEQUOTES-P T) NO-UP-P
			&AUX CH STRCH)
  "Return a bp which is TIMES sexps (list objects) forward from BP.  Comments are ignored.
TIMES may be negative meaning go backward.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case.
LEVEL > 0 means move up that many levels of list structure.
STOP-BP is a place to give up and return if it is reached;
 the value is NIL or STOP-BP.
MOVE-OVER-SINGLEQUOTES-P if T means that a backward motion should
 move back over any singlequote-like characters before the open-paren.
 Forward motion also moves over them after a string, for the sake of #|foo|#.
NO-UP-P means it is forbidden to move up and then down again.
 NIL is returned if that starts to happen."
   (WHEN (LISP-BP-SYNTACTIC-CONTEXT BP)
     ;; BP is within a string.
     ;; Allow motion over sexps within the string,
     ;; but don't allow motion past boundary of this string.
     ;; Now make an exception for the case of what looks like a defun-beginning
     ;; which is inside a string (such as a |# after the previous defun).
     (UNLESS (AND (PLUSP TIMES)
		  (ZEROP (BP-INDEX BP))
		  (= (LIST-SYNTAX (BP-CHAR BP)) LIST-OPEN))
       (SETQ STOP-BP (FORWARD-UP-STRING BP (MINUSP TIMES) NIL))))
   (COND ((ZEROP TIMES) (COPY-BP BP))
	 ((PLUSP TIMES)
	  (LET ((STATE 'NORMAL)  ;STATE is NORMAL, STRING or ALPHABETIC.
		(TIME 0)
		(LAST-BP (OR STOP-BP (INTERVAL-LAST-BP *INTERVAL*))))
	    (CHARMAP-PER-LINE (BP LAST-BP (IF (OR FIXUP-P
						  (AND (EQ STATE 'ALPHABETIC)
						       ( LEVEL 0)
						       (= (1+ TIME) TIMES)))
					      (COPY-BP LAST-BP)
					      NIL))
		     ;; Per-line forms
		     ;; If at start of line and inside some parens,
		     ;; skip over some lines using memoized LISP-PARSE-LINE info.
		     ;; This is an invisible speed-up for the rest of this loop.
		     ((COND ((AND (ZEROP *FIRST-INDEX*) (> LEVEL 0))
			     (MULTIPLE-VALUE (LINE STRCH LEVEL)
			       (LISP-FORWARD-LIST-AUX LINE
						      (AND (EQ STATE 'STRING) STRCH)
						      LEVEL *LAST-LINE*))
			     (SETQ STATE (COND (STRCH 'STRING) (T 'NORMAL)))
			     (SETQ *THIS-IS-THE-LAST-LINE*
				   (EQ LINE *LAST-LINE*)))))
              RESTART
	      (LET ((SYNTAX (LIST-SYNTAX-OPEN-CODED (SETQ CH (CHARMAP-CH-CHAR)))))
		(SELECTQ STATE
		  (ALPHABETIC
		   (SELECT SYNTAX
		     (LIST-ALPHABETIC)
		     (LIST-SINGLE-QUOTE)
		     (LIST-SLASH
		      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))
		     (LIST-COLON
		      (SETQ STATE 'NORMAL))
		     (OTHERWISE
                      (IF ( LEVEL 0)
                          (IF ( (SETQ TIME (1+ TIME)) TIMES)
                              (CHARMAP-RETURN (CHARMAP-BP-BEFORE))))
                      (SETQ STATE 'NORMAL)
		      (GO RESTART))))
		  (STRING
		   (SELECT SYNTAX
		    (LIST-DOUBLE-QUOTE
		      (COND ((= CH STRCH)
			     (IF ( LEVEL 0)
				 (IF ( (SETQ TIME (1+ TIME)) TIMES)
				     (CHARMAP-RETURN
				       ;; Maybe move forward over singlequote-syntax chars
				       ;; for the sake of #|foo|#.
				       (IF MOVE-OVER-SINGLEQUOTES-P
					   (DO ((BP (CHARMAP-BP-AFTER) (IBP BP)))
					       ((OR (NULL BP)
						    (BP-= BP LAST-BP)
						    (NOT (= (LIST-SYNTAX (BP-CHAR BP))
							    LIST-SINGLE-QUOTE)))
						(OR BP (IF FIXUP-P (COPY-BP LAST-BP)))))
					 (CHARMAP-BP-AFTER)))))
			     (SETQ STATE 'NORMAL))))
		     (LIST-SLASH
		      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))))
		  (NORMAL
		   (SELECT SYNTAX
		     (LIST-ALPHABETIC
		      (SETQ STATE 'ALPHABETIC))
		     (LIST-DELIMITER)
		     (LIST-SLASH
		      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL))
		      (SETQ STATE 'ALPHABETIC))
		     (LIST-COMMENT
		      (SETQ INDEX *LAST-INDEX*))
		     (LIST-DOUBLE-QUOTE
		      (SETQ STATE 'STRING STRCH CH))
		     (LIST-SINGLE-QUOTE)
		     (LIST-CLOSE
		      (SETQ LEVEL (1- LEVEL))
		      (COND ((AND NO-UP-P (< LEVEL 0))
			     (CHARMAP-RETURN NIL))
			    (( LEVEL 0)
                             (IF ( (SETQ TIME (1+ TIME)) TIMES)
                                 (CHARMAP-RETURN (CHARMAP-BP-AFTER)))))
                      (SETQ STATE 'NORMAL))
		     (LIST-OPEN
		      (SETQ LEVEL (1+ LEVEL)))
		     (LIST-COLON))))))))
         (T
	  (LET ((STATE 'NORMAL)
                (TIME 0)
		NEW-LINE-FLAG
		(FIRST-BP (OR STOP-BP (INTERVAL-FIRST-BP *INTERVAL*))))
            (RCHARMAP-PER-LINE (BP FIRST-BP
				   (IF (OR FIXUP-P
					   (AND (OR (AND (EQ STATE 'ALPHABETIC) ( LEVEL 0))
						    (AND (EQ STATE
							     'SKIP-LEADING-SINGLE-QUOTES)
							 ( LEVEL 1)))
						(= (1- TIME) TIMES)))
				       (COPY-BP FIRST-BP)
				       NIL))
		     ;; Per-line forms.  Work like those for forward case.
		     ((COND ((AND (NOT *FIRST-LINE-P*) (> LEVEL 0))
			     (MULTIPLE-VALUE (LINE STRCH LEVEL)
			       (LISP-BACKWARD-LIST-AUX LINE
						       (AND (EQ STATE 'STRING) STRCH)
						       LEVEL *LAST-LINE*))
			     (SETQ STATE (COND (STRCH 'STRING) (T 'NORMAL)))
			     (SETQ *THIS-IS-THE-LAST-LINE*
				   (EQ LINE *LAST-LINE*))))
		      (SETQ NEW-LINE-FLAG T))
	      ;; After arriving on a line and processing the implicit Return at the end,
	      ;; skip back to start of any comment on that line.
	      (AND ( #\RETURN (RCHARMAP-CH-CHAR)) 
		   (PROG1 NEW-LINE-FLAG (SETQ NEW-LINE-FLAG NIL))
		   (NOT *FIRST-LINE-P*)
		   (SETQ INDEX (OR (LISP-FIND-COMMENT-START LINE *LAST-INDEX*) INDEX)))
              RESTART
              (COND ((AND (= (LIST-SYNTAX-OPEN-CODED (RCHARMAP-CH-CHAR-BEFORE)) LIST-SLASH)
			  (DO ((SL NIL (NOT SL))
			       (BP (FORWARD-CHAR (RCHARMAP-BP-BEFORE) -1)
				   (FORWARD-CHAR BP -1)))
			      ((OR (NULL BP)
				   ( (LIST-SYNTAX-OPEN-CODED (BP-CH-CHAR BP)) LIST-SLASH))
			       SL)))
		     ;; Odd number of preceding slashes means non-special character
		     (RCHARMAP-DECREMENT)
		     (AND (EQ STATE 'NORMAL)
			  (SETQ STATE 'ALPHABETIC)))
                    (T
                     (LET ((SYNTAX (LIST-SYNTAX-OPEN-CODED (SETQ CH (RCHARMAP-CH-CHAR)))))
                       (SELECTQ STATE
                         (ALPHABETIC
                          (SELECT SYNTAX
                            ((LIST-ALPHABETIC LIST-COLON))
                            (LIST-SINGLE-QUOTE)
			    (OTHERWISE
                             (IF ( LEVEL 0)
                                 (IF ( (SETQ TIME (1- TIME)) TIMES)
                                     (RCHARMAP-RETURN
				      (RCHARMAP-BP-AFTER))))
                             (SETQ STATE 'NORMAL)
                             (GO RESTART))))
                         (STRING
                          (SELECT SYNTAX
                            (LIST-DOUBLE-QUOTE
                             (COND ((= CH STRCH)
                                    (IF ( LEVEL 0)
                                        (IF ( (SETQ TIME (1- TIME)) TIMES)
                                            (RCHARMAP-RETURN
					      (MAYBE-BACKWARD-LEADING-SINGLE-QUOTES
						(RCHARMAP-BP-BEFORE)
						FIRST-BP MOVE-OVER-SINGLEQUOTES-P))))
                                    (SETQ STATE 'NORMAL))))))
                         (NORMAL
                          (SELECT SYNTAX
                            (LIST-ALPHABETIC
                             (SETQ STATE 'ALPHABETIC))
			    (LIST-COMMENT
			     (WHEN *FIRST-LINE-P*
			       (SETQ LEVEL 0)))
                            ((LIST-SLASH LIST-COLON)
                             ;; Crock.
                             (SETQ STATE 'ALPHABETIC))
                            (LIST-DOUBLE-QUOTE
                             (SETQ STATE 'STRING STRCH CH))
                            (LIST-CLOSE
                             (SETQ LEVEL (1+ LEVEL)))
                            (LIST-OPEN
			     (SETQ LEVEL (1- LEVEL))
			     (COND ((AND NO-UP-P (< LEVEL 0) (NOT FIXUP-P))
				    (RCHARMAP-RETURN NIL))
				   ((AND (= (LIST-SYNTAX (RCHARMAP-CH-CHAR-BEFORE)) LIST-COLON)
					 MOVE-OVER-SINGLEQUOTES-P)
				    (SETQ STATE 'ALPHABETIC))
				   (( LEVEL 0)
				    (IF ( (SETQ TIME (1- TIME)) TIMES)
					(RCHARMAP-RETURN
					  (MAYBE-BACKWARD-LEADING-SINGLE-QUOTES
					    (RCHARMAP-BP-BEFORE)
					    FIRST-BP MOVE-OVER-SINGLEQUOTES-P))))))
			    (LIST-COLON))))))))))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "


;;;For things only prepared to deal with fixnums
(DEFMETHOD (STREAM-MIXIN :MOUSE-OR-KBD-TYI) ()
  (DO ((CH)) (NIL)
    (AND (NUMBERP (SETQ CH (FUNCALL-SELF ':ANY-TYI)))
	 (RETURN CH CH))
    (AND (CONSP CH) (EQ (CAR CH) ':MOUSE-BUTTON)
	 (RETURN (SECOND CH) CH))))

(DEFMETHOD (STREAM-MIXIN :MOUSE-OR-KBD-TYI-NO-HANG) ()
  (DO ((CH)) (NIL)
    (AND (OR (NULL (SETQ CH (FUNCALL-SELF ':ANY-TYI-NO-HANG)))
	     (NUMBERP CH))
	 (RETURN CH CH))
    (AND (CONSP CH) (EQ (CAR CH) ':MOUSE-BUTTON)
	 (RETURN (SECOND CH) CH))))

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
  (:MOUSE-BUTTON (CH WINDOW *MOUSE-X* *MOUSE-Y*)
    (IF (NOT (TYPEP WINDOW 'ZWEI))
	(WHEN (= CH #\MOUSE-R)
	  (TV:MOUSE-CALL-SYSTEM-MENU))
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
      T))
  ((:TYPEOUT-EXECUTE :EXECUTE) (FUNCTION &REST ARGS)
   (LET ((*MINI-BUFFER-DONT-RECORD* T))
     ;; We would not be able to repeat the command anyway.
     (NOT (APPLY FUNCTION ARGS)))))

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
	     (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,(TV:MERGE-SHIFT-KEYS BUTTON) ,SELF ,X ,Y)))
	 (SETQ HANDLED-P T))
	(T
	 (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,(TV:MERGE-SHIFT-KEYS BUTTON) ,SELF ,X ,Y))))
  T)

))

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defconst char-bits-limit 20
  "All the special bits in a character must be less than this.
They are Control, Meta, Super and Hyper.")

))

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun code-char (code &optional (bits 0) (font 0))
  "Returns a character whose code comes from CODE, bits from BITS and font from FONT.
CODE can be a number or a character.
NIL is returned if it is not possible to have a character object
with the specified FONT and BITS."
  (%make-pointer dtp-character
		 (%logdpb bits %%kbd-control-meta
			  (dpb font %%ch-font code))))

))

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun set-char-bit (char bit-name new-value)
  "Returns a character like CHAR except that the bit BIT-NAME has value NEW-VALUE in it.
BIT-NAME can be :CONTROL, :META, :SUPER or :HYPER.
NEW-VALUE should be T or NIL."
  (let* ((new-char (%logdpb (if new-value 1 0)
			    (cdr (assq bit-name
				       '((:control . #,%%kbd-control)
					 (:meta . #,%%kbd-meta)
					 (:super . #,%%kbd-super)
					 (:hyper . #,%%kbd-hyper))))
			    char)))
    (if (typep char ':character)
	(int-char new-char)
      new-char)))
))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method %logldb-test (bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int)
    (let ((btemp (gensym))
	  (store (gensym))
	  (itemp (first stores)))
      (values (cons btemp temps)
	      (cons bytespec vals)
	      (list store)
	      `(progn 
		 ,(sublis (list (cons itemp `(%logdpb (if ,store 1 0) ,btemp ,access-form)))
			  store-form)
		 ,store)
	      `(%logldb-test ,btemp ,access-form)))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST %LOGLDB-TEST (PPSS WORD)
  "T if the field specified by PPSS in WORD is not zero.
PPSS is a position (from the right) times 64., plus a size.
Like LDB-TEST except that when SETF'd it does a %LOGDPB rather than a DPB."
  (NOT (ZEROP (%LOGLDB PPSS WORD))))

))

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defsubst char-bit (char bit-name)
  "T if the bit spec'd by BIT-NAME (a keyword) is on in CHAR.
BIT-NAME can be :CONTROL, :META, :SUPER or :HYPER."
  (%logldb-test (cdr (assq bit-name
			   '((:control . #,%%kbd-control)
			     (:meta . #,%%kbd-meta)
			     (:super . #,%%kbd-super)
			     (:hyper . #,%%kbd-hyper))))
		char))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#\-MACRO (STREAM IGNORE &OPTIONAL (BITS XR-SHARP-ARGUMENT))
  (MULTIPLE-VALUE-BIND (NIL NIL CHAR)
      (XR-XRTYI STREAM NIL T)
    (LOGIOR
      (%LOGDPB (OR BITS 0) %%KBD-CONTROL-META 0)
      (IF (NOT (OR ( #/A CHAR #/Z) ( #/a CHAR #/z)))
	  CHAR
	(SEND STREAM ':UNTYI CHAR)
	(PKG-BIND PKG-KEYWORD-PACKAGE
	  (LET ((FROB (INTERNAL-READ STREAM T NIL T)))			;Get symbolic name of character
	    (IF *READ-SUPPRESS* 0			;READ returns NIL in this case; don't bomb.
	      (IF (= (STRING-LENGTH FROB) 1)
		  XR-XRTYI-PREV-CHAR 
		(OR (CDR (ASSQ FROB XR-SPECIAL-CHARACTER-NAMES))
		    (XR-PARSE-KEYBOARD-CHAR FROB)
		    (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
			    "#\~A is not a defined character-name." FROB))))))))))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-PARSE-KEYBOARD-CHAR (SYM)
  (AND (OR (SYMBOLP SYM) (STRINGP SYM))
       (LET ((STRING (IF (STRINGP SYM) SYM (GET-PNAME SYM)))
	     TOP-FLAG GREEK-FLAG SHIFT-FLAG)
	 (LOOP WITH CHAR = 0
	       WITH END = (ARRAY-ACTIVE-LENGTH STRING)
	       WITH TEM = NIL
	       FOR START FIRST 0 THEN (1+ HYPHEN-POS)
	       FOR 1+PREV-HYPHEN-POS = 0 THEN (1+ HYPHEN-POS)
	       FOR HYPHEN-POS = (OR (STRING-SEARCH-CHAR #/- STRING START END) END)
	       DO (LET ((LEN (- HYPHEN-POS 1+PREV-HYPHEN-POS)))
		    (COND ((OR (XR-STR-CMP "CTRL")
			       (XR-STR-CMP "CONTROL"))
			   (SETQ CHAR (DPB 1 %%KBD-CONTROL CHAR)))
			  ((XR-STR-CMP "META")
			   (SETQ CHAR (DPB 1 %%KBD-META CHAR)))
			  ((XR-STR-CMP "HYPER")
			   (SETQ CHAR (%LOGDPB 1 %%KBD-HYPER CHAR)))
			  ((XR-STR-CMP "SUPER")
			   (SETQ CHAR (DPB 1 %%KBD-SUPER CHAR)))
			  ((XR-STR-CMP "GREEK")
			   (SETQ GREEK-FLAG T))
			  ((XR-STR-CMP "FRONT")
			   (SETQ GREEK-FLAG T))
			  ((XR-STR-CMP "TOP")
			   (SETQ TOP-FLAG T))
			  ((OR (XR-STR-CMP "SHIFT")
			       (XR-STR-CMP "SH"))
			   (SETQ SHIFT-FLAG T))
			  ((= 1+PREV-HYPHEN-POS (1- END))
			   (RETURN (GREEKIFY-CHARACTER (AREF STRING 1+PREV-HYPHEN-POS)
						       GREEK-FLAG TOP-FLAG SHIFT-FLAG
						       CHAR)))
			  ((= 1+PREV-HYPHEN-POS (1- HYPHEN-POS))
			   (LET ((TEM (ASSQ (CHAR-UPCASE (LDB %%CH-CHAR (AREF STRING
									  1+PREV-HYPHEN-POS)))
					    '((#/C . %%KBD-CONTROL)
					      (#/M . %%KBD-META)
					      (#/H . %%KBD-HYPER)
					      (#/S . %%KBD-SUPER)))))
			     (IF (NULL TEM)
				 (RETURN NIL)
			       (SETQ CHAR (%LOGDPB 1 (SYMEVAL (CDR TEM)) CHAR)))))
			  ;; See if we have a name of a special character "Return", "SP" etc.
			  ((SETQ TEM
				 (DOLIST (ELEM XR-SPECIAL-CHARACTER-NAMES)
				   (LET ((TARGET (GET-PNAME (CAR ELEM))))
				     (IF (STRING-EQUAL
					   TARGET STRING
					   0 1+PREV-HYPHEN-POS
					   (ARRAY-ACTIVE-LENGTH TARGET) END)
					 (RETURN (CDR ELEM))))))
			   ;; Note: combine with LOGIOR rather than DPB, since mouse
			   ;; characters have the high %%KBD-MOUSE bit on.
			   (RETURN (GREEKIFY-CHARACTER TEM GREEK-FLAG
						       TOP-FLAG SHIFT-FLAG
						       CHAR)))
			  (T (RETURN NIL))))))))

))

; From file COLD.LISP SRC:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(DEFUN KBD-CONVERT-NEW (CH &OPTIONAL (CTL-CHARS-UPPERCASE T))
  (COND ((BIT-TEST 1_15. CH)		;An all-keys-up code, just update shifts mask
	 (COPY-ARRAY-CONTENTS "" KBD-KEY-STATE-ARRAY-16B)	;Mark all keys up
	 (SETQ CH (LDB 0017 CH))	;Get bits for keys or key-pairs still down
	 (SETQ KBD-LEFT-SHIFTS (LOGAND KBD-LEFT-SHIFTS CH)
	       KBD-RIGHT-SHIFTS (LOGAND KBD-RIGHT-SHIFTS CH)
	       KBD-LEFT-SHIFTS		;This is for keys that are down that we thought
	         (LOGIOR		; were up, e.g. caps lock.  Boole 10 is NOR.
		   (LOGAND (BOOLE 10 KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS) CH)
		   KBD-LEFT-SHIFTS)
	      TV:KBD-BUTTONS 0)		;analogous to mouse buttons, set by roman numerals
	 NIL)
	(T (LET* ((KBD-SHIFTS (LOGIOR KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS))
		  (NCH (AREF KBD-NEW-TABLE	;NCH gets translate-table entry
			     (COND ((BIT-TEST 2 KBD-SHIFTS)	;Greek
				    (+ (LOGAND 1 KBD-SHIFTS) 3))
				   ((BIT-TEST 4 KBD-SHIFTS) 2)	;Top
				   ((BIT-TEST 1 KBD-SHIFTS) 1)	;Shift
				   (T 0))
			     (LDB 0007 CH)))
		  (NCH0 (AREF KBD-NEW-TABLE 0 (LDB 0007 CH))))
	     (COND ((BIT-TEST 1_15. NCH)	;Not a real character
		    (COND ((BIT-TEST 1_14. NCH)	;Undefined key, beep if key-down
			   (OR (BIT-TEST 1_8 CH)
			       (KBD-CONVERT-BEEP)))
			  (T			;A shifting key, update KBD-SHIFTS
			    (LET ((BOOLE (IF (BIT-TEST 1_8 CH) 2 7))  ;Bit off, on
				  (BIT (LSH 1 (LOGAND NCH 37))))
			      (IF (BIT-TEST 40 NCH)
				  (SETQ KBD-RIGHT-SHIFTS (BOOLE BOOLE BIT KBD-RIGHT-SHIFTS))
				  (SETQ KBD-LEFT-SHIFTS (BOOLE BOOLE BIT KBD-LEFT-SHIFTS))))))
		    NIL)
		   ((BIT-TEST 1_8 CH)
		    (ASET 0 KBD-KEY-STATE-ARRAY NCH0)
		    (COND ((BIT-TEST 1_9 KBD-SHIFTS)	 ;Mode lock
			   (SELECTQ NCH
			     (#\ROMAN-I (SETQ TV:KBD-BUTTONS (BOOLE 4 TV:KBD-BUTTONS 1)))
			     (#\ROMAN-II (SETQ TV:KBD-BUTTONS (BOOLE 4 TV:KBD-BUTTONS 2)))
			     (#\ROMAN-III (SETQ TV:KBD-BUTTONS (BOOLE 4 TV:KBD-BUTTONS 4))))
			   (SETQ MOUSE-WAKEUP T)))
		    NIL)	 ;Just an up-code
		   ((AND (BIT-TEST 1_9 KBD-SHIFTS)	 ;Mode lock
			 (MEMQ NCH '(#\ROMAN-I #\ROMAN-II #\ROMAN-III)))
		    (ASET 1 KBD-KEY-STATE-ARRAY NCH0)
		    (SETQ TV:KBD-BUTTONS (LOGIOR TV:KBD-BUTTONS
						 (SELECTQ NCH (#\ROMAN-I 1)
							  (#\ROMAN-II 2)
							  (T 4))))
		    (SETQ MOUSE-WAKEUP T)
		    NIL)
		   (T ;A real key depression.  Check for caps-lock.
		    (ASET 1 KBD-KEY-STATE-ARRAY NCH0)
		    (SETQ NCH0 (LDB 0404 KBD-SHIFTS))	;Hyper, Super, Meta, Control bits
		    (IF (AND CTL-CHARS-UPPERCASE
			     (NOT (ZEROP NCH0)))
			(IF ( #/a NCH #/z)
			    (DECF NCH 40)	;Control characters always uppercase,
			  (IF ( #/A NCH #/Z)	;except if Shift is typed they are lowercase.
			      (INCF NCH 40)))
		      ;; Except for control chars for which Shift is reversed,
		      ;; consider the shift-lock key.
		      (AND (BIT-TEST 10 KBD-SHIFTS)	;Caps lock
			   (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
			       (AND ( NCH #/A) ( NCH #/Z) (SETQ NCH (+ NCH 40)))
			     (AND ( NCH #/a) ( NCH #/z) (SETQ NCH (- NCH 40))))))
		    (%LOGDPB NCH0 %%KBD-CONTROL-META NCH)))))))

))

; From file COLD.LISP SRC:<L.WINDOW> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLD  "

(defun kbd-convert-newer (char &optional (ctl-chars-uppercase t))
  (setq char (logand 377 char));strip of source bits
  (cond ((bit-test 1_7 char);is it a second-byte?
	 (cond ((bit-test 1_6 char);up or down code?
		(prog1
		 (multiple-value-bind (soft-char unshifted-soft-char)
		     (new-lookup saved-first-char)
		   (cond ((bit-test 1_15. soft-char)
			  (kbd-bit-15-on soft-char t)
			  nil)
			 (t      ;normal character
			  (aset 1 kbd-key-state-array;set bitmap bit
				unshifted-soft-char)
			  ;A real key depression.  Check for caps-lock.
			  (let ((kbd-shifts (logior kbd-left-shifts kbd-right-shifts)))
			    ;Hyper, Super, Meta, Control bits
			    (SETQ unshifted-soft-char (LDB 0404 KBD-SHIFTS))
			    (IF (AND CTL-CHARS-UPPERCASE
				     (NOT (ZEROP unshifted-soft-char)))
				(IF (<= #/a SOFT-CHAR #/z)
				    (DECF SOFT-CHAR 40)      ;Control characters always uppercase,
				  (IF (<= #/A SOFT-CHAR #/Z)   ;unless  Shift is typed
				      (INCF SOFT-CHAR 40)))
				;; Except for control chars for which Shift is reversed,
				;; consider the shift-lock key.
				(AND (BIT-TEST 10 KBD-SHIFTS)  ;Caps lock
				     (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
					 (AND (>= SOFT-CHAR #/A)
					      (<= SOFT-CHAR #/Z)
					      (SETQ SOFT-CHAR (+ SOFT-CHAR 40)))
				       (AND (>= SOFT-CHAR #/a)
					    (<= SOFT-CHAR #/z)
					    (SETQ SOFT-CHAR (- SOFT-CHAR 40))))))
			    (%LOGDPB unshifted-soft-char %%KBD-CONTROL-META SOFT-CHAR)))))
		 (insure-down-bucky-bit-consistency char)))   ;key-down
	       (t                    ;0: key up
		(multiple-value-bind (soft-char unshifted-soft-char)
		   (new-lookup saved-first-char)
		  (cond ((bit-test 1_15. soft-char)
			 (kbd-bit-15-on soft-char nil)
			 nil)
			(t (aset 0 kbd-key-state-array unshifted-soft-char)))
		  (insure-up-bucky-bit-consistency char)
		  nil))))
	(t (setq saved-first-char (ldb 0007 char));its a first-byte
	   nil)))

))

; From file MOUSE.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; MOUSE  "

(DEFUN MERGE-SHIFT-KEYS (CHAR)
  "Return CHAR with the bits for various shift keys updated.
For example, the %%KBD-HYPER bit of the result will be 1
if a Hyper key is depressed right now."
  (%LOGDPB (IF (KEY-STATE ':HYPER) 1 0)
	   %%KBD-HYPER
	   (DPB (IF (KEY-STATE ':SUPER) 1 0)
		%%KBD-SUPER
		(DPB (IF (KEY-STATE ':META) 1 0)
		     %%KBD-META
		     (DPB (IF (KEY-STATE ':CONTROL) 1 0)
			  %%KBD-CONTROL
			  CHAR)))))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN SET-COMTAB-UPPERCASE-INDIRECTION (COMTAB)
  "Make all lower case chars in COMTAB indirect appropriately.
Those with no meta bits indirect to the corresponding upper case character.
Those with meta bits indirect to the upper case character with Hyper added.
Note: characters which are already defined are not redefined."
  (MAKE-COMTAB-NON-SPARSE COMTAB)
  (LET ((ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB)))
    (DOTIMES (I (ARRAY-DIMENSION ARRAY 1))
       (DO CHAR #/a (1+ CHAR) (> CHAR #/z)
	   (UNLESS (AREF ARRAY CHAR I)
	     (ASET (LIST (IF (ZEROP I) 0 
			   (%LOGLDB %%KBD-CONTROL-META
				    (%LOGDPB 1 %%KBD-HYPER
					     (%LOGDPB I %%KBD-CONTROL-META 0))))
			 (- CHAR 40))
		   ARRAY CHAR I))))))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN COMTAB-CHAR-INDIRECTION (CHAR &OPTIONAL (COMTAB *COMTAB*))
  "Return the character that CHAR indirects to in COMTAB.
This may be CHAR itself, if it is not indirect."
  (DO ((CH CHAR (%LOGDPB (FIRST NCH) %%KBD-CONTROL-META (SECOND NCH)))
       (NCH))
      ((ATOM (SETQ NCH (COMMAND-LOOKUP CH COMTAB T)))
       CH)))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN KEY-FOR-COMMAND (COMMAND &OPTIONAL (COMTAB *COMTAB*)
			STARTING-CHAR STARTING-COMTAB
			SUGGESTED-CHAR &AUX TEM)
  "Return a string describing the character to invoke COMMAND in COMTAB.
Returns NIL if there is no way.
The second value is the comtab that COMMAND was actually found in;
this is COMTAB or one of the comtabs it indirects to.
STARTING-CHAR and STARTING-COMTAB say where, in the sequence
to be searched for COMTAB, to start looking.  This is so you
can use the character and comtab values to resume the search. 
You can use the SUGGESTED-CHAR to save time
by suggesting the place where the command standardly goes."
 (DECLARE (RETURN-LIST STRING CHARACTER COMTAB))
 (OR STARTING-CHAR (SETQ STARTING-CHAR 0))
 (OR STARTING-COMTAB (SETQ STARTING-COMTAB COMTAB))
 (PROG FOUND ()
  (IF SUGGESTED-CHAR
      (MULTIPLE-VALUE-BIND (COMMAND1 COMTAB1)
	  (COMMAND-LOOKUP SUGGESTED-CHAR COMTAB)
	(IF (EQ COMMAND1 COMMAND)
	    (RETURN (FORMAT NIL "~@:C" SUGGESTED-CHAR)
		    SUGGESTED-CHAR COMTAB1))))
  (DO ((CTB STARTING-COMTAB (COMTAB-INDIRECT-TO CTB))
       (STARTING-CHAR STARTING-CHAR 0)
       KEYBOARD-ARRAY LENGTH)
      ((NULL CTB))
    (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB))
    (IF (NOT (ARRAYP KEYBOARD-ARRAY))
	(DOLIST (ELT KEYBOARD-ARRAY)
	  (COND ((< (CAR ELT) STARTING-CHAR))	;Don't ever notice chars before the starting char.
		((AND (EQ (CDR ELT) COMMAND)
		      (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB))
		 (RETURN-FROM FOUND
		   (FORMAT NIL "~@:C" (CAR ELT))
		   (CAR ELT)
		   CTB))
		((AND (EQ (CDR ELT) 'COM-DOCUMENTATION)
		      (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB)
		      (SETQ TEM (RASSQ COMMAND *COM-DOCUMENTATION-ALIST*)))
		 (RETURN-FROM FOUND
		   (FORMAT NIL "~:@C ~:@C" (CAR ELT) (CAR TEM))
		   (CAR ELT)
		   CTB))
		((AND (TYPEP (CDR ELT) 'CLOSURE)	;Redundant but should speed things up.
		      (PREFIX-COMMAND-P (CDR ELT))
		      (SETQ TEM
			    (KEY-FOR-COMMAND COMMAND
					     (GET-PREFIX-COMMAND-COMTAB (CDR ELT))))
		      (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB))
		 (RETURN-FROM FOUND
		   (FORMAT NIL "~:@C ~A" (CAR ELT) TEM)
		   (CAR ELT)
		   CTB))))
      (SETQ LENGTH (ARRAY-DIMENSION KEYBOARD-ARRAY 0))
      (DO ((BITS (LDB %%KBD-CONTROL-META STARTING-CHAR) (1+ BITS))
	   (INCREMENT
	     (IF ARRAY-INDEX-ORDER (ARRAY-DIMENSION KEYBOARD-ARRAY 1)
	       1)))
	  ((= BITS 20))
	(DO ((CH (IF (= BITS (LDB %%KBD-CONTROL-META STARTING-CHAR))
		     (LDB %%KBD-CHAR STARTING-CHAR)
		   0)
		 (+ 1 CH))
	     (OFFSET (IF (= BITS (LDB %%KBD-CONTROL-META STARTING-CHAR))
			 (* INCREMENT (LDB %%KBD-CHAR STARTING-CHAR))
		       0)
		     (+ OFFSET INCREMENT))
	     (PTR (ALOC KEYBOARD-ARRAY 0 BITS)))
	    ((= CH LENGTH))
	  (LET ((THIS-COM (%P-CONTENTS-OFFSET PTR OFFSET))) ;Faster than AREF on 2-dim array!
	    (COND ((AND (EQ THIS-COM COMMAND)
			(IN-THIS-COMTAB-P COMTAB (%LOGDPB BITS %%KBD-CONTROL-META CH) CTB))
		   (RETURN-FROM FOUND
		     (FORMAT NIL "~@:C" (%LOGDPB BITS %%KBD-CONTROL-META CH))
		     (%LOGDPB BITS %%KBD-CONTROL-META CH)
		     CTB))
		  ((AND (EQ THIS-COM 'COM-DOCUMENTATION)
			(IN-THIS-COMTAB-P COMTAB (%LOGDPB BITS %%KBD-CONTROL-META CH) CTB)
			(SETQ TEM (RASSQ COMMAND *COM-DOCUMENTATION-ALIST*)))
		   (RETURN-FROM FOUND
		     (FORMAT NIL "~:@C ~:@C" (%LOGDPB BITS %%KBD-CONTROL-META CH) (CAR TEM))
		     (%LOGDPB BITS %%KBD-CONTROL-META CH)
		     CTB))
		  ((AND (TYPEP THIS-COM 'CLOSURE)	;Redundant but should speed things up.
			(PREFIX-COMMAND-P THIS-COM)
			(SETQ TEM
			      (KEY-FOR-COMMAND COMMAND
					       (GET-PREFIX-COMMAND-COMTAB THIS-COM)))
			(IN-THIS-COMTAB-P COMTAB (%LOGDPB BITS %%KBD-CONTROL-META CH) CTB))
		   (RETURN-FROM FOUND
		     (FORMAT NIL "~:@C ~A" (%LOGDPB BITS %%KBD-CONTROL-META CH) TEM)
		     (%LOGDPB BITS %%KBD-CONTROL-META CH)
		     CTB))))))))))

))

; From file COMTAB.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN COMMAND-CHAR-FROM-NAME (NAME)
  (IF (NUMBERP NAME) NAME
      (LET ((I (STRING-MATCH "MOUSE" NAME)))
	(COND ((NULL I)
	       ;; The name does not start with MOUSE.
	       (LET ((CHAR1 (AREF NAME 0)))
		 (LET ((X (ASSQ CHAR1 '((#/ . 0) (#/ . 1) (#/ . 2) (#/ . 3)))))
		   (COND ((NULL X) CHAR1)
			 (T (%LOGDPB (CDR X) %%KBD-CONTROL-META (AREF NAME 1)))))))
	      (T
	       (MULTIPLE-VALUE-BIND (BUTTON J)
		 (PARSE-NUMBER NAME (1+ I))
		 (LET ((N-CLICKS (PARSE-NUMBER NAME (1+ J))))
		   (COND ((OR (GREATERP 1 BUTTON 3)
			      (GREATERP 1 N-CLICKS 2))
			  (FERROR NIL "Invalid mouse specification ~A" NAME))
			 (T (TV:MAKE-MOUSE-CHAR (1- BUTTON) (1- N-CLICKS)))))))))))

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
			       (LDB %%KBD-MOUSE-BUTTON CH)))
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

; From file DOC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN WALLCHART-COMTAB (COMTAB &OPTIONAL COMMANDS)
  (LET ((TABLE (SYMEVAL COMTAB)))
    (FORMAT T "~|~%Command chart of ~A:~%~%" (MAKE-COMMAND-NAME COMTAB))
    (DO ((LETTER 0 (1+ LETTER)))
	((= LETTER #O237))
      (IF (OR (< LETTER #/a)			;LOWERCASE LETTERS ARE JUST ALIASED.
	      (> LETTER #/z))
	  (DO ((BUCKY 0 (1+ BUCKY)))
	      ((= BUCKY 16.))
	    (LET* ((KEY (%LOGDPB BUCKY %%KBD-CONTROL-META LETTER))
		   (COMMAND (COMMAND-LOOKUP KEY TABLE)))
	      (COND ((AND COMMAND
			  (NEQ COMMAND 'COM-STANDARD)
			  (NEQ COMMAND 'COM-NUMBERS)
			  (NEQ COMMAND 'COM-ORDINARILY-SELF-INSERT)
			  (NEQ COMMAND 'COM-NEGATE-NUMERIC-ARG))
		     (TERPRI)
		     (FUNCALL STANDARD-OUTPUT ':STRING-OUT	;SO ~T WORKS ON ALL STREAMS.
			      (FORMAT NIL "~:C~32,1T~A" KEY (IF (SYMBOLP COMMAND)
								(MAKE-COMMAND-NAME COMMAND)
							      "Extended command")))
		     (SETQ COMMANDS (DELQ COMMAND COMMANDS))))))))
    (TERPRI)
    COMMANDS))

))

; From file DOC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN UNREACHABLE-COMMAND-LIST-INTERNAL (*COMTAB* L &AUX CHAR TEM KBD)
  (DO ((CT *COMTAB* (COMTAB-INDIRECT-TO CT)))
      ((ARRAYP (SETQ KBD (COMTAB-KEYBOARD-ARRAY CT)))))
  (DOTIMES (I (ARRAY-DIMENSION KBD 1))
    (DOTIMES (J (ARRAY-DIMENSION KBD 0))
      (SETQ CHAR (%LOGDPB I %%KBD-CONTROL-META J))
      (SETQ TEM (COMMAND-LOOKUP CHAR *COMTAB* T))
      (COND ((AND TEM (SYMBOLP TEM))
	     (SETQ L (DELQ TEM L)))
	    ((PREFIX-COMMAND-P TEM)
	     (SETQ L (UNREACHABLE-COMMAND-LIST-INTERNAL
		       (GET-PREFIX-COMMAND-COMTAB TEM) L))))))
  (DOLIST (C L)
    (AND (EXTENDED-COMMAND-P C) (SETQ L (DELQ C L))))
  L)

))

; From file DOC.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN DOCUMENT-PREFIX-CHAR-TABLE (COMTAB &AUX KBD CHARS)
  (IF (DO ((CT COMTAB (COMTAB-INDIRECT-TO CT)))
	  ((NULL CT) T)
	(IF (ARRAYP (SETQ KBD (COMTAB-KEYBOARD-ARRAY CT)))
	    (RETURN NIL))
	(SETQ CHARS (UNION CHARS (MAPCAR 'CAR KBD))))
      ;; Every level of comtab we indirect to is a sparse one.
      (DOLIST (CHAR CHARS)
	(PRINT-SHORT-DOC-FOR-TABLE CHAR COMTAB 3))
    (DOTIMES (I (ARRAY-DIMENSION KBD 1))
      (DOTIMES (J (ARRAY-DIMENSION KBD 0))
	(PRINT-SHORT-DOC-FOR-TABLE (%LOGDPB I %%KBD-CONTROL-META J) COMTAB 3)))))

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

; From file MODES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MODES  "

(DEFUN DOCUMENT-PREFIX-CHAR (COMMAND IGNORE OP &AUX COLNUM)
  (SETQ COLNUM (CDR (ASSQ COMMAND '((COM-PREFIX-CONTROL . 1)
				    (COM-PREFIX-META . 2)
				    (COM-PREFIX-CONTROL-META . 3)))))
  (SELECTQ OP
    (:NAME (GET COMMAND 'COMMAND-NAME))
    (:SHORT (FORMAT T "Set the ~[Control~;Meta~;Control-Meta~] prefix." (1- COLNUM)))
    (:FULL (FORMAT T "Set the ~[Control~;Meta~;Control-Meta~] prefix.
Make the next character act as if it were typed with ~[CTRL~;META~;CTRL and META~]
held down, just as if you were on a losing terminal that doesn't
support all of the wonderful keys that we cleverly provide
on these marvelous keyboards.
Type a subcommand to document (or /"*/" for all): " (1- COLNUM) (1- COLNUM))
	   (LET ((CHAR (FUNCALL STANDARD-INPUT ':TYI)))
	     (COND ((= CHAR #/*)
		    (FORMAT T "~2%The following ~[Control~;Meta~;Control-Meta~]- commands are availible:~%" (1- COLNUM))
		    (LET ((N (%LOGDPB COLNUM %%KBD-CONTROL-META 0)))
		      (DO ((I N (1+ I))
			   (LIM (+ N 220)))
			  (( I LIM))
			(PRINT-SHORT-DOC-FOR-TABLE I *COMTAB* 3))))
		   (T (SETQ CHAR (%LOGDPB COLNUM %%KBD-CONTROL-META CHAR))
		      (FORMAT T "~:C~2%" CHAR)
		      (DOCUMENT-KEY CHAR *COMTAB*)))))))

))

; From file PRINT.LISP SRC:<L.IO> OZ:
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
					(BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM ':TYO #/=)
				    LABEL))
				 (T
				  (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM ':TYO #/#)
				    (*THROW 'LABEL-PRINTED T)))))
		       STREAM)
		 NIL))
	  (TYPECASE EXP
	    (:FIXNUM (PRINT-FIXNUM EXP STREAM))
	    (:SYMBOL
	     (PRINT-PNAME-STRING EXP STREAM FASTP))
	    (:LIST
	     (IF (AND PRINLEVEL (>= I-PRINDEPTH PRINLEVEL))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL READTABLE) STREAM FASTP)
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
			 (GET NSS 'NAMED-STRUCTURE-INVOKE)
			 (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;NAMED STRUCTURE THAT DOESN'T PRINT ITSELF
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
;		    (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
;		      (PRINC NSS STREAM))
	    (:ARRAY
	     (IF *PRINT-ARRAY*
		 (IF (AND (= (ARRAY-RANK EXP) 1)
			  (EQ (ARRAY-TYPE EXP) 'ART-1B))
		     (PRINT-BIT-VECTOR EXP STREAM)
		   (IF *PRINT-PRETTY*
		       (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		     (IF (= (ARRAY-RANK EXP) 1)
			 (PRINT-VECTOR EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
		       (PRINT-ARRAY EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	       (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
		 (PRINT-RAW-STRING (GET-PNAME (ARRAY-TYPE EXP)) STREAM FASTP)
		 (DO ((I 0 (1+ I))
		      (RANK (ARRAY-RANK EXP))
		      (DIM))
		     ((= I RANK))
		   (SETQ DIM (ARRAY-DIMENSION EXP I))
		   (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
		   (PRINT-FIXNUM DIM STREAM)))))
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
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-BEFORE-FONT READTABLE))
		(IF (LDB-TEST %%CH-FONT EXP)
		    (LET ((BASE 10.) (*NOPOINT T))
		      (PRIN1 (LDB %%CH-FONT EXP) STREAM)))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-PREFIX READTABLE))
		(IF (CHAR-BIT EXP ':CONTROL)
		    (SEND STREAM ':STRING-OUT "c-"))
		(IF (CHAR-BIT EXP ':META)
		    (SEND STREAM ':STRING-OUT "m-"))
		(IF (CHAR-BIT EXP ':SUPER)
		    (SEND STREAM ':STRING-OUT "s-"))
		(IF (CHAR-BIT EXP ':HYPER)
		    (SEND STREAM ':STRING-OUT "h-"))
		(SEND STREAM ':TYO (LDB %%CH-CHAR EXP))))
	    (:NUMBER
	     (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
	     (PRINT-RAW-STRING (GET-PNAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
	     (LET ((BASE 8))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "


(DEFUN XR-#-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (XR-XRTYI STREAM NIL T)  ;Skip the / that follows.
  (%MAKE-POINTER DTP-CHARACTER
		 (%LOGDPB (OR ARG 0) %%CH-FONT
			  (XR-#\-MACRO STREAM NIL))))

(DEFUN XR-CL-#\-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (%MAKE-POINTER DTP-CHARACTER
		 (%LOGDPB (OR ARG 0) %%CH-FONT
			  (XR-#\-MACRO STREAM NIL))))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN (:LOCATIVE SET-FUNCTION) (ITEM NEW-VALUE IGNORE)
  (RPLACD (SECOND ITEM) NEW-VALUE))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (BASIC-INSPECT :MOUSE-SENSITIVE-ITEM) (X Y)
  (PROG FOUND-ITEM (LILN)
	(MULTIPLE-VALUE-BIND (ITEM TYPE LEFT BWIDTH TOP)
	    (MOUSE-SENSITIVE-ITEM X Y)
	  (COND (ITEM (RETURN-FROM FOUND-ITEM ITEM TYPE LEFT BWIDTH TOP))
		((NOT DISPLAYING-LIST))
		((AND ( Y (SHEET-INSIDE-TOP))
		      (< Y (SHEET-INSIDE-BOTTOM)))
		 ;; No explicit item on this line -- find list structure if it exists
		 (LET ((LINE-NO (+ TOP-ITEM (SHEET-LINE-NO NIL Y))))
		   ;; Starting from this line, work backwards until an enclosing
		   ;; piece of structure is found
		   (OR ( LINE-NO (ARRAY-ACTIVE-LENGTH ITEMS))
		       (DOLIST (LI (FIRST (AREF ITEMS LINE-NO)))
			 (AND (COND ((= LINE-NO (SETQ LILN (THIRD LI)))
				     ;; Entry starts on this line -- within range on right?
				     ( X (SECOND LI)))
				    ((> LINE-NO LILN)
				     ;; Entry starts on some previous line -- so we are ok
				     T))
			      (COND ((= LINE-NO (SETQ LILN (FIFTH LI)))
				     ;; Entry ends on this line, within range on left?
				     (< X (FOURTH LI)))
				    ((< LINE-NO LILN)
				     ;; Entry starts before -- so this is good
				     T))
			      (RETURN-FROM FOUND-ITEM
				(IF (AND (OR MODIFY-MODE (KEY-STATE ':HYPER))
					 (EQ (FIRST LI) ':TOP-LEVEL))
				    NIL
				  (VALUES LI
					  (IF (EQ (FIRST LI) ':TOP-LEVEL)
					      ':LIST-STRUCTURE-TOP-LEVEL
					    ':LIST-STRUCTURE)))))))))))))

))

; From file TSCROL.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TSCROL  "

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW :MOUSE-CLICK) (BUTTON X Y &AUX ITEM TYPE)
  (MULTIPLE-VALUE (ITEM TYPE) (FUNCALL-SELF ':MOUSE-SENSITIVE-ITEM X Y))
  (COND (TYPE
	 (FUNCALL-SELF ':FORCE-KBD-INPUT (LIST TYPE ITEM SELF
					       (MERGE-SHIFT-KEYS BUTTON)))
	 T)))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defun sublis-eval-once-1 (exp alist &optional reuse-flag sequential-flag)
  (if (null alist) exp
    (if (symbolp exp)
	(let ((tem (assq exp alist)))
	  (cond ((null tem) exp)
		((seo-tempvar tem)
		 (incf (seo-count tem))
		 (seo-tempvar tem))
		((eq (seo-count tem) t)
		 (seo-exp tem))
		(t
		 (setf (seo-tempvar tem)
		       (if reuse-flag (car tem) (gensym)))
		 (setf (seo-count tem) 0)
		 (setf (seo-first-use tem)
		       (cons 'progn nil))
		 (let ((e1
			 `(,@(loop for tail on seo-first-uninserted-var
				   until (eq (car tail) tem)
				   do (setf (seo-tempvar (car tail))
					    (if reuse-flag (caar tail) (gensym)))
				   (setf (seo-first-use (car tail)) (seo-first-use tem))
				   collect `(setq ,(seo-tempvar (car tail))
						  ,(if sequential-flag
						       (sublis-eval-once-1
							 (seo-exp (car tail))
							 (ldiff alist tail))
						     (seo-exp (car tail))))
				   finally (setq seo-first-uninserted-var (cdr tail)))
			   (setq ,(seo-tempvar tem)
				 ,(if sequential-flag
				      (sublis-eval-once-1 (seo-exp tem)
							  (ldiff alist (memq tem alist)))
				    (seo-exp tem))))))
		   (setf (cdr (seo-first-use tem)) e1)
		   (seo-first-use tem)))))
      (if (atom exp) exp
	(do ((tail exp (cdr tail))
	     accum)
	    ((atom tail)
	     (nreconc accum tail))
	  (push (sublis-eval-once-1 (car tail) alist reuse-flag sequential-flag)
		accum))))))

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
		 ((CHAR-BIT (FOURTH THING) ':HYPER)
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

; From file STRUCT.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defstruct-define-type :named-list
  (:overhead 1)
  (:cons (arg description etc) :list
    etc			;ignored
    `(list ',(defstruct-description-name) ,.arg))
  (:ref (n description arg)
    description		;ignored
 #+Multics
    `(,(let ((i (\ (1+ n) 4)))
	 (cond ((= i 0) 'car)
	       ((= i 1) 'cadr)
	       ((= i 2) 'caddr)
	       (t 'cadddr)))
      ,(do ((a arg `(cddddr ,a))
	    (i (// (1+ n) 4) (1- i)))
	   ((= i 0) a)))
 #-Multics
    `(nth ,(1+ n) ,arg))
  (:predicate (description name)
    `(defun ,name (x)
       (and
      #-MacLisp-10
	 (not (atom x))
      #+MacLisp-10	;Watch out for hunks!
	 (eq (typep x) 'list)
	 (eq (car x) ',(defstruct-description-name)))))
  (:copier (description name)
    (do ((l `((car x)) (cons `(prog1 (car x) (setq x (cdr x))) l))
	 (i (defstruct-description-size) (1- i)))
	((<= i 1)
	 `(defun ,name (x)
	    (setq x (cdr x))
	    (list ',(defstruct-description-name) ,@l))))))

))

zwei:(compile-flavor-methods editor-top-level editor-stream-from-window
			     ztop-stream-from-window)