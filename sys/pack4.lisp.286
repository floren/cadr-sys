; -*- MODE: LISP; PACKAGE: SI -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFVAR PACKAGE :UNBOUND
  "The current package, the default for INTERN, READ and PRINT.")

;Some special packages live as values of these variables.
(DEFVAR PKG-USER-PACKAGE :UNBOUND
  "The package named USER (the keyword package).")
(DEFVAR PKG-SYSTEM-PACKAGE :UNBOUND
  "The package named SYSTEM.")
(DEFVAR PKG-GLOBAL-PACKAGE :UNBOUND
  "The package named GLOBAL, which all other packages are normally inferiors of.")
(DEFVAR PKG-SYSTEM-INTERNALS-PACKAGE :UNBOUND
  "The package named SYSTEM-INTERNALS or SI.")

(DEFVAR PKG-AREA :UNBOUND
  "The area which packages are consed in.")

;This is the old-style obarray, used only at cold load time
;until this file is loaded.
(DEFVAR OBARRAY)

;Unknown file defaults to STRING of this
; unless this is unbound or NIL.
(DEFVAR PKG-UNKNOWN-FILE-DEFAULTS-TO-PACKAGE)

;Crock to tell QFASL that PACK4 has been loaded (relevant during building the world).
(DEFVAR PKG-IS-LOADED-P)

;Alist from strings to symbols of special functions that appear in package declarations.
(DEFCONST PKG-DECLARATION-NAMES
      '(("SUBPACKAGE" . SUBPACKAGE) ;?
	("SHADOW" . PKG-SHADOW-SYMBOLS)
	("INTERN" . PKG-INTERN-SYMBOLS)
	("BORROW" . PKG-BORROW-SYMBOLS)
	("EXTERNAL" . PKG-EXTERN-SYMBOLS)
	("REFNAME" . PKG-REF-DECL)
	("MYREFNAME" . PKG-MYREFNAME-DECL)
	("FORWARD" . PKG-FORWARD-SYMBOLS)
	("INDIRECT" . PKG-INDIRECT-SYMBOLS)
	("KEYWORD" . PKG-KEYWORD-SYMBOLS)
	("FORWARD-ALIAS" . PKG-FORWARD-ALIAS)
	("INDIRECT-ALIAS" . PKG-INDIRECT-ALIAS)
	("USE" . PKG-USE-PACKAGE)
	("ADVERTISE" . PKG-ADVERTISE-SYMBOLS)))

;;; These have to be created in advance, before reading in PKGDCL, because
;;; they are needed when assigning symbols in the old-style obarray to packages.
;;; This fixes a bug where PKG-FIND-PACKAGE with the CREATE-P option created
;;; the package in the wrong place.
;;; This is a list of lists: (name size . nicknames)
(DEFCONST PACKAGES-WHICH-MAGICALLY-GO-UNDER-SYSTEM
	'( ("CADR" 7000.)
	   ("CHAOS" 1200.)
#+ETHER	   ("ETHER" 2000. "ETH")
	   ("COLOR" 400)
	   ("COMPILER" 2800.)
	   ("FILE-SYSTEM" 1800. "FS")
	   ("QFASL-REL" 300.)
	   ("METER" 300.)
	   ("TV" 4000.)
	   ("EH" 1200.)
	   ("FED" 1000.)
	   ("SYSTEM-INTERNALS" 7000. "SI") ))

;This defines the format of a PACKAGE.
;LMCONS;QF > knows about it!
(DEFSTRUCT (PACKAGE :ARRAY-LEADER :NAMED (:CONSTRUCTOR PKG-MAKE-PACKAGE))
		    ;; (CONSTRUCTOR KLAPAUCIUS)
	   PKG-REFNAME-ALIST			;refname alist.
						;This is a list of pairings of strings
						;and the packages they refer to, in this
						;environment.  There will be at least one
						;entry, (LIST <program-name of pkg> <pkg>),
						;pairing this package with itself.
	   PKG-PROGRAM-NAME			;Name of program this package is a version of.
	   PKG-NAME				;Name of this package.  May be same as above.
	   PKG-SUPER-PACKAGE			;containing package, or nil
	   PKG-SUBPACKAGES			;list of subpackages
	   PKG-LOCKED				;Not allowed to intern new stuff, if T.
	   PKG-DECLARED				;T => this package has been declared.
	   PKG-EXTERNAL-LIST			;List of inherited symbols we can DEFUN,
						;or strings which are their names,
						;or T, meaning ok to redefine anything.
	   PKG-BRING-IN-LIST			;List of commands to create local symbols
						;which point at or are EQ to remote ones.
	   PKG-SHADOW-LIST			;List of symbols specified to be shadowed,
						;or strings which are their names.
	   PKG-FILE-ALIST			;List of files containing code for package.
	   PKG-LOADED				;T => this package has been loaded.
						;:DEFS => the definitions files were loaded.
	   PKG-ADVERTISED-SYMBOLS		;List of symbols which (USE PKG) forwards to,
						;or (BORROW sym) meaning borrow instead, etc.
	   PKG-USER-PACKAGES			;List of packages that USE this package.
	   PKG-NUMBER-OF-SYMBOLS
	   PKG-MAX-NUMBER-OF-SYMBOLS		;Specified size of array.
	   )

(DEFSUBST PKG-NUMBER-OF-SLOTS (PKG)
  (ASH (ARRAY-LENGTH PKG) -1))

;The rest of the package is a 2 by (3/2 * specified size) array,
;whose contents are the interned symbols and their hash codes.
;For each I, (AR-2-REVERSE PKG 0 I) and (AR-2-REVERSE PKG 1 I) are both NIL for an empty slot;
;for a filled slot the first is the hash code and the second is the symbol.
;For a REMOB'd slot, the first is T and the second is NIL.

;Each entry in the PKG-BRING-IN-LIST looks like this:
;(type pkg from-symbol <optional to-symbol>).
;The type is PKG-BORROW-SYMBOL, PKG-FORWARD-SYMBOL or PKG-INDIRECT-SYMBOL,
;and is the function to call to do the work.  Applying FUNCALL to the
;element causes the work to be done.  The to-symbol must not exist for borrowings.

(DEFUN (PACKAGE NAMED-STRUCTURE-INVOKE) (OP &OPTIONAL SELF &REST ARGS)
  (COND ((EQ OP ':WHICH-OPERATIONS)
	 '(:DESCRIBE :PRINT-SELF))
	((EQ OP ':DESCRIBE)
	 (DESCRIBE-PACKAGE SELF))
	((EQ OP ':PRINT-SELF)
	 (LET ((STANDARD-OUTPUT (FIRST ARGS))
	       (SLASHIFY-P (THIRD ARGS)))
	   (IF SLASHIFY-P
	       (SI:PRINTING-RANDOM-OBJECT (SELF STANDARD-OUTPUT)
		 (PRINC "Package ")
		 (PKG-MAP-REFNAMES #'(LAMBDA (NAME CNT)
				       (PRINC NAME)
				       (OR (ZEROP CNT) (PRINC ":")))
				   SELF PACKAGE NIL))
	       (PKG-MAP-REFNAMES #'(LAMBDA (NAME CNT)
				     (PRINC NAME)
				     (OR (ZEROP CNT) (PRINC ":")))
				 SELF PACKAGE NIL))))))

(DEFMACRO PKG-PACKAGE-P (ARG)
  "T if ARG is a package object."
  `(TYPEP ,ARG 'PACKAGE))

(DEFMACRO-DISPLACE PKG-BIND (PKG &BODY BODY)
  "Execute BODY with PKG as current package.  PKG is a package or the name of one."
  (IF (MEMBER PKG '("" "USER"))
      `(LET ((PACKAGE PKG-USER-PACKAGE))	;Optimize most common case.
	 . ,BODY)
      `(LET ((PACKAGE (PKG-FIND-PACKAGE ,PKG)))
	 . ,BODY)))

(DEFUN DESCRIBE-PACKAGE (PKG)
  "Describe thoroughly the package PKG (a package or the name of one).
The only thing not mentioned is what symbols are in the package.
Use MAPATOMS for that."
    (SETQ PKG (PKG-FIND-PACKAGE PKG))
    (FORMAT T "~%Package ~A" (PKG-PROGRAM-NAME PKG))
    (OR (STRING-EQUAL (PKG-NAME PKG) (PKG-PROGRAM-NAME PKG))
	(FORMAT T " (~A)" (PKG-NAME PKG)))
    (DO PKG-1 (PKG-SUPER-PACKAGE PKG) (PKG-SUPER-PACKAGE PKG-1) (NULL PKG-1)
	(FORMAT T ", under ~A" (PKG-NAME PKG-1)))
    (PRINC ".  ")
    (AND (PKG-LOCKED PKG) (PRINC "Locked.  "))
;   (SELECTQ (PKG-LOADED PKG)
;     ((T) (PRINC "Loaded."))
;     (:DEFS (PRINC "Definitions Loaded."))
;     (NIL (PRINC "Not loaded.")))
    (FORMAT T "~&  ~D symbols out of ~D.  Hash modulus=~D.~%"
	    (PKG-NUMBER-OF-SYMBOLS PKG)
	    (PKG-MAX-NUMBER-OF-SYMBOLS PKG)
	    (PKG-NUMBER-OF-SLOTS PKG))
    (COND ((PKG-REFNAME-ALIST PKG)
	   (FORMAT T "Refname alist:~%")
	   (DO L (PKG-REFNAME-ALIST PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~20A~S~%" (CAAR L) (CADAR L)))))
    (COND ((PKG-FILE-ALIST PKG)
	   (FORMAT T "Files:~%")
	   (DO L (PKG-FILE-ALIST PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~A" (CAAR L))
	       (AND (EQ (CADAR L) ':DEFS)
		    (FORMAT T "   (DEFINITIONS)"))
	       (TERPRI))))
    (COND ((PKG-SUBPACKAGES PKG)
	   (FORMAT T "Subpackages:~%")
	   (DO L (PKG-SUBPACKAGES PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~S~%" (CAR L)))))
    (COND ((PKG-ADVERTISED-SYMBOLS PKG)
	   (FORMAT T "Advertised symbols:~%")
	   (DO L (PKG-ADVERTISED-SYMBOLS PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~S~%" (CAR L)))))
    (COND ((PKG-USER-PACKAGES PKG)
	   (FORMAT T "Packages which USE this one:~%")
	   (DO L (PKG-USER-PACKAGES PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~S~%" (CAR L)))))
    (COND ((PKG-BRING-IN-LIST PKG)
	   (FORMAT T "External symbols referred to:~%")
	   (DO L (PKG-BRING-IN-LIST PKG) (CDR L) (NULL L)
	       (COND ((EQ (CAAR L) 'PKG-USE-PACKAGE-EV)
		      (FORMAT T "    Entries of ~A" (CADAR L)))
		     (T
		      (FORMAT T "    ~30A   " (THIRD (CAR L)))
		      (FORMAT T (SELECTQ (CAAR L)
					 (PKG-FORWARD-SYMBOL "-> ")
					 (PKG-INDIRECT-SYMBOL "IND")
					 (PKG-BORROW-SYMBOL "EQ " )))
		      (FORMAT T "  ~A" (SECOND (CAR L)))
		      (AND (FOURTH (CAR L))
			   (FORMAT T ":~A" (FOURTH (CAR L))))))
	       (TERPRI))))
    (COND ((PKG-SHADOW-LIST PKG)
	   (FORMAT T "Shadowed symbols:~%")
	   (DO L (PKG-SHADOW-LIST PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~A~%" (CAR L)))))
    (COND ((EQ (PKG-EXTERNAL-LIST PKG) T)
	   (FORMAT T "Any external symbol may be redefined.~%"))
	  ((PKG-EXTERNAL-LIST PKG)
	   (FORMAT T "External symbols redefined:~%")
	   (DO L (PKG-EXTERNAL-LIST PKG) (CDR L) (NULL L)
	       (FORMAT T "    ~A~%" (CAR L))))))

;;; Create the packages that should initially exist,
;;; and fill them with the appropriate symbols.
(DEFUN PKG-INITIALIZE (&AUX TEM)
       (OR (BOUNDP 'PKG-AREA)
           (MAKE-AREA ':NAME 'PKG-AREA
		      ':REPRESENTATION ':STRUCTURE
		      ':GC ':STATIC
		      ':REGION-SIZE 200000))
       (SETQ PKG-GLOBAL-PACKAGE (PKG-CREATE-PACKAGE 'GLOBAL NIL 4000))
       (SETQ PKG-USER-PACKAGE (PKG-CREATE-PACKAGE 'USER PKG-GLOBAL-PACKAGE 10000))
       (PKG-REF-1 PKG-GLOBAL-PACKAGE "" PKG-USER-PACKAGE)
       (SETQ PKG-SYSTEM-PACKAGE (PKG-CREATE-PACKAGE 'SYSTEM PKG-GLOBAL-PACKAGE 3000))
       (PKG-REF-1 PKG-GLOBAL-PACKAGE "SYS" PKG-SYSTEM-PACKAGE)
       (DO L PACKAGES-WHICH-MAGICALLY-GO-UNDER-SYSTEM (CDR L) (NULL L)
	 (SETQ TEM (PKG-CREATE-PACKAGE (CAAR L) PKG-SYSTEM-PACKAGE (CADAR L)))
	 (PKG-REF-1 PKG-GLOBAL-PACKAGE (CAAR L) TEM)
	 (DO LL (CDDAR L) (CDR LL) (NULL LL)
	   (PKG-REF-1 PKG-GLOBAL-PACKAGE (CAR LL) TEM)
	   (PKG-REF-1 PKG-SYSTEM-PACKAGE (CAR LL) TEM)))
       (LET ((PACKAGE PKG-USER-PACKAGE))
	 (SETQ PKG-SYSTEM-INTERNALS-PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
       (SETF (PKG-LOADED PKG-GLOBAL-PACKAGE) T)
       (SETF (PKG-LOADED PKG-SYSTEM-PACKAGE) T)
       ;(SETF (PKG-EXTERNAL-LIST PKG-USER-PACKAGE) T) ;Warn user if bashing system stuff
       (SETF (PKG-EXTERNAL-LIST PKG-SYSTEM-INTERNALS-PACKAGE) T)

       ;; Read in GLOBAL and SYSTEM   Find each symbol from the file in the old obarray
       ;; and intern it in PKG-GLOBAL-PACKAGE or PKG-SYSTEM-PACKAGE.
       ;; The file contains those atoms which should go in PKG-GLOBAL-PACKAGE.
       ;; It is assumed that we are using the MINI file system at this point.
       ;; Errors in READ are avoided by not giving PACKAGE a value yet.
       (DOLIST (F GLOBAL-PACKAGE-FILE-ALIST)
	 (LET ((PKG
		 (LET ((PACKAGE PKG-USER-PACKAGE))
		   (PKG-FIND-PACKAGE (CADR F))))
	       (ISTREAM (MINI-OPEN-FILE (CAR F) (CADDR F))))
	   (DO ((X))
	       ((EQ PKG (SETQ X (READ ISTREAM PKG))))
	     (OR (ARRAYP (CAR (PACKAGE-CELL-LOCATION X)))	;If not already interned,
		 (RPLACA (PACKAGE-CELL-LOCATION X) NIL))	;Override pkg from cold-load
	     (PKG-INTERN X PKG))))
       (SETQ PACKAGE PKG-USER-PACKAGE)
       ;; Put system variables and system constants on the SYSTEM package.
       (LET ((PACKAGE PKG-SYSTEM-PACKAGE))
	  (MAPC (FUNCTION (LAMBDA (SYMLIST-SYM)
				  (MAPC (FUNCTION PKG-INTERN) (SYMEVAL SYMLIST-SYM))))
		SYSTEM-VARIABLE-LISTS)
	  (MAPC (FUNCTION (LAMBDA (SYMLIST-SYM)
				  (MAPC (FUNCTION PKG-INTERN) (SYMEVAL SYMLIST-SYM))))
		SYSTEM-CONSTANT-LISTS)
	  (MAPC (FUNCTION PKG-INTERN) A-MEMORY-COUNTER-BLOCK-NAMES)
       ;; Any micro-code symbols not already accounted for go on SYSTEM
	  (MAPC (FUNCTION PKG-INTERN) (G-L-P (FUNCTION MICRO-CODE-SYMBOL-NAME-AREA))))
       ;; Now all other system symbols go in the SYSTEM-INTERNALS package, unless
       ;; the cold-load has specified a different place for them to go.
       (MAPATOMS-NR-SYM (FUNCTION (LAMBDA (SYM &AUX SYM1 PKG PKG1)
	   (OR (ARRAYP (SETQ PKG1 (CAR (PACKAGE-CELL-LOCATION SYM))))
					    ;already interned on a package
	       (PROGN (RPLACA (PACKAGE-CELL-LOCATION SYM) NIL)
		      (SETQ PKG (IF PKG1 (PKG-FIND-PACKAGE PKG1 T)
				    PKG-SYSTEM-INTERNALS-PACKAGE))
		      (SETQ SYM1 (INTERN-LOCAL SYM PKG))
		      (COND ((NEQ SYM1 SYM)
			     (PRINC SYM) (PRINC " BUILD-INITIAL-OBARRAY screw")
			     (PRINC ", interned at ") (PRIN1 (%POINTER SYM1))
			     (PRINC ", wanted at ") (PRIN1 (%POINTER SYM))
			     (%HALT))))))))
       T)

(DEFUN PKG-INSTALL NIL
       (PKG-INITIALIZE)
       (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-OBARRAY-PNTR) 'PACKAGE) ;FOR CONSOLE PROGRAM
       (FSET 'INTERN (FSYMEVAL 'PKG-INTERN)))

(DEFUN PKG-CREATE-PACKAGE (NAME &OPTIONAL (SUPER PACKAGE)
			   (SIZE 200)
			   &AUX PKG PROGRAM-NAME TABLE-SIZE
			   ;; Prevent two people from hacking subpackages
			   ;; of one package at one time.
			   (INHIBIT-SCHEDULING-FLAG T)
			   (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
  ;; Default SIZE even if specified as NIL.
  (OR SIZE (SETQ SIZE 200))
  (AND SUPER (SETQ SUPER (PKG-FIND-PACKAGE SUPER)))
  (AND SUPER (PKG-FILE-ALIST SUPER)
       (FERROR NIL "Attempt to create a subpackage of a package, ~A, with files in it."
	       SUPER))
  (COND ((CONSP NAME) (SETQ PROGRAM-NAME (CADR NAME) NAME (CAR NAME)))
	(T (SETQ PROGRAM-NAME NAME)))
  (SETQ NAME (STRING NAME)
	PROGRAM-NAME (STRING PROGRAM-NAME))
  ;; Find a good prime to use as the actual size of the table.
  (SETQ TABLE-SIZE (PKG-GOOD-SIZE SIZE))
  (SETQ PKG (PKG-MAKE-PACKAGE MAKE-ARRAY
			      (:LENGTH
				(IF ARRAY-INDEX-ORDER
				    (LIST TABLE-SIZE 2)
				  (LIST 2 TABLE-SIZE))
				:AREA PKG-AREA)
			      PKG-NAME NAME
			      PKG-PROGRAM-NAME PROGRAM-NAME
			      PKG-SUPER-PACKAGE SUPER
			      PKG-NUMBER-OF-SYMBOLS 0
			      PKG-MAX-NUMBER-OF-SYMBOLS SIZE))
  (AND SUPER (PUSH PKG (PKG-SUBPACKAGES SUPER)))
  (PKG-REF-1 PKG PROGRAM-NAME PKG)
  (AND SUPER (PKG-REF-1 SUPER NAME PKG))
  PKG)

(DEFCONST PKG-GOOD-SIZES
	  '(103 111 123 141 161 203 225 243 263 301 337 357 373
		415 433 445 463 475 521 547 577 631 661 711 747
		1011 1043 1101 1145 1203 1245 1317 1357
		1423 1473 1537 1555 1627 1707 1761
		2033 2077 2131 2223 2275 2353 2447 2535 2631 2721
		3021 3123 3235 3337 3437 3541 3665 3767
		4101 4203 4313 4435 4553 4707
		5037 5201 5331 5477 5667
		6045 6163 6343 6517 6667 7065 7261 7461 7663
		10077 10301 10601 11123 11503 12033 12343 12701 13303 13711
		14321 14733 15343 16011 16465 17155 17657
		20343 21003 21603 22401 23303 24201 25117 26011 27001
		30125 31215 32311 33401 34501 35601 37005
		40207 41527 43001 44315 45713 47301
		51011 52407 54003 55401 57007 60607 62413 64207 66005 67603))

;Given a number of symbols, return a good length of hash table to hold that many.
(DEFUN PKG-GOOD-SIZE (NUMBER-OF-SYMBOLS)
  "Return a good hash table size which is bigger than 5/4 times NUMBER-OF-SYMBOLS."
       (LET ((TEM (TRUNCATE (* NUMBER-OF-SYMBOLS 5) 4)))  ;Allow hash table to become 80% full.
	 (OR (DO L PKG-GOOD-SIZES (CDR L) (NULL L)
	       (AND (> (CAR L) TEM) (RETURN (CAR L))))
	     ;; Beyond the list of good sizes => avoid multiples of small primes.
	     (DO ((N (+ TEM 1 (\ TEM 2)) (+ N 2)))
		 ((NOT (OR (ZEROP (\ N 3))
			   (ZEROP (\ N 5))
			   (ZEROP (\ N 7))
			   (ZEROP (\ N 11.))))
		  N)))))

;This is the normal INTERN function, once this package is installed.
;Value 1 is the interned symbol.
;Value 2 is T if the symbol was already interned.
;Value 3 is the package that the symbol is actually present in.
(DEFUN PKG-INTERN (SYM &OPTIONAL PKG &AUX HASH STR)
  "Intern the string or symbol SYM in package PKG (or the current package).
If the package has a symbol whose pname matches SYM, that symbol is returned.
Otherwise, if SYM is a symbol, it is put in the package and returned.
Otherwise (SYM is a string), a new symbol is constructed, put in the package and returned.

The second value is T if there was already a symbol in the package.

The specified package's superiors are also searched, and the third value
is the package that the symbol was actually found or put in."
  (DECLARE (VALUES SYMBOL ALREADY-INTERNED-FLAG ACTUAL-PACKAGE))
  (PROG INTERN ()
	(COND ((NULL PKG) (SETQ PKG PACKAGE))
	      ((NOT (PKG-PACKAGE-P PKG)) (SETQ PKG (PKG-FIND-PACKAGE PKG))))
	(SETQ HASH (%SXHASH-STRING (SETQ STR (STRING SYM)) 377))
	;; Prevent interrupts in case two people intern symbols with the same pname,
	;; both find that there is no such symbol yet,
	;; and both try to stick them in the obarray simultaneously.
	(WITHOUT-INTERRUPTS
	  (DO ((PKG PKG (PKG-SUPER-PACKAGE PKG)))
	      ((NULL PKG))
	    (MULTIPLE-VALUE-BIND (TEM FOUND)
		(PKG-INTERN-INTERNAL STR HASH PKG)
	      (AND FOUND (RETURN-FROM INTERN TEM T PKG))))
	  (COND ((NOT (SYMBOLP SYM))
		 (AND (STRINGP SYM)
		      (EQ (ARRAY-TYPE SYM) 'ART-FAT-STRING)
		      (SETQ SYM (STRING-REMOVE-FONTS SYM)))
		 (SETQ SYM (MAKE-SYMBOL SYM T))))
	  (OR (CAR (PACKAGE-CELL-LOCATION SYM))
	      (RPLACA (PACKAGE-CELL-LOCATION SYM) PKG))
	  (PKG-INTERN-STORE HASH SYM PKG))
	(RETURN SYM NIL PKG)))

;Find a symbol, if it is already interned, but don't intern it otherwise.
;The values are the same as for INTERN, except that for a symbol
;which is not found all three are NIL.
;Must lock out interrupts because otherwise someone might rehash the
;package while we are scanning through it.
(DEFUN INTERN-SOFT (SYM &OPTIONAL (PKG PACKAGE) &AUX HASH TEM FOUND STR)
  "Like INTERN but return NIL if the package does not have a suitable symbol.
Does not ever put a new symbol into the package."
  (DECLARE (VALUES SYMBOL ACTUALLY-FOUND-FLAG ACTUAL-PACKAGE))
  (OR (PKG-PACKAGE-P PKG) (SETQ PKG (PKG-FIND-PACKAGE PKG)))
  (SETQ HASH (%SXHASH-STRING (SETQ STR (STRING SYM)) 377))
  (DO ((PKG PKG (PKG-SUPER-PACKAGE PKG))
       (INHIBIT-SCHEDULING-FLAG T))
      ((NULL PKG) NIL)
    (MULTIPLE-VALUE (TEM FOUND)
      (PKG-INTERN-INTERNAL STR HASH PKG))
    (AND FOUND (RETURN TEM T PKG))))

;Intern using the current or specified package only.
;The values match those of INTERN.
(DEFUN INTERN-LOCAL (SYM &OPTIONAL (PKG PACKAGE) &AUX HASH TEM FOUND STR)
  "Like INTERN but does not search the superiors of the package PKG.
If no symbol is found in that package itself, SYM or a new symbol
is put into it, even if a superior package contains a matching symbol."
  (DECLARE (VALUES SYMBOL ALREADY-INTERNED-FLAG ACTUAL-PACKAGE))
  (PROG ()
	(OR (PKG-PACKAGE-P PKG) (SETQ PKG (PKG-FIND-PACKAGE PKG)))
	(SETQ HASH (%SXHASH-STRING (SETQ STR (STRING SYM)) 377))
	(WITHOUT-INTERRUPTS
	  (MULTIPLE-VALUE (TEM FOUND)
	    (PKG-INTERN-INTERNAL STR HASH PKG))
	  (AND FOUND (RETURN TEM T PKG))
	  (COND ((NOT (SYMBOLP SYM))
		 (AND (STRINGP SYM)
		      (EQ (ARRAY-TYPE SYM) 'ART-FAT-STRING)
		      (SETQ SYM (STRING-REMOVE-FONTS SYM)))
		 (SETQ SYM (MAKE-SYMBOL SYM T))))
	  (OR (CAR (PACKAGE-CELL-LOCATION SYM))
	      (RPLACA (PACKAGE-CELL-LOCATION SYM) PKG))
	  (PKG-INTERN-STORE HASH SYM PKG))
	(RETURN SYM NIL PKG)))

;Check whether a symbol is present in a given package (not inherited).
;The values match those of INTERN.
(DEFUN INTERN-LOCAL-SOFT (SYM &OPTIONAL (PKG PACKAGE) &AUX HASH TEM FOUND STR)
  "Like INTERN but checks only the specified package and returns NIL if nothing found."
  (DECLARE (VALUES SYMBOL ACTUALLY-FOUND-FLAG ACTUAL-PACKAGE))
  (PROG ()
	(OR (PKG-PACKAGE-P PKG) (SETQ PKG (PKG-FIND-PACKAGE PKG)))
	(SETQ HASH (%SXHASH-STRING (SETQ STR (STRING SYM)) 377))
	(WITHOUT-INTERRUPTS
	  (MULTIPLE-VALUE (TEM FOUND)
	    (PKG-INTERN-INTERNAL STR HASH PKG)))
	(AND FOUND (RETURN TEM T PKG))))

;;; Internals of INTERN.

;Search a given package for a given symbol with given hash code.
;If it is found, return it and the index it was at.
;Otherwise, return NIL NIL.
(DEFUN PKG-INTERN-INTERNAL (STRING HASH PKG
			    &AUX X Y (LEN (PKG-NUMBER-OF-SLOTS PKG))
				 (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
  (DO ((I (\ HASH LEN)))
      ((NULL (SETQ X (AR-2-REVERSE PKG 0 I)))
       NIL)
    (AND (EQ HASH X)				;EQ not =, could be a T here
	 (EQUAL STRING (GET-PNAME (SETQ Y (AR-2-REVERSE PKG 1 I))))
	 (RETURN Y I))
    (SETQ I (1+ I))
    (IF (= I LEN) (SETQ I 0))))

(COMMENT
(DEFUN PKG-HASH-STRING (STRING &OPTIONAL (HASH 0))
  (DO ((I 0 (1+ I))
       (LEN (ARRAY-ACTIVE-LENGTH STRING)))
      (( I LEN)
       (COND ((MINUSP HASH)
	      (LOGXOR HASH -37777777))		;-37777777 = 40000001
	     (T HASH)))
    (SETQ HASH (ROT (LOGXOR HASH (AREF STRING I)) 7)))))

;Store the symbol SYM into the package PKG, given a precomputed hash,
;assuming that no symbol with that pname is present in the package.
;If number of symbols exceeds the maximum (which is less than
;the length of the array by 4/5), we rehash.
;Call only if interrupts are locked out.
(DEFUN PKG-INTERN-STORE (HASH SYM PKG &AUX LEN)
       (COND ((PKG-LOCKED PKG)
	      (FERROR NIL "Interning ~A in locked package ~A" SYM PKG)))
       (SETQ LEN (PKG-NUMBER-OF-SLOTS PKG))
       (DO ((I (\ HASH LEN) (\ (1+ I) LEN))
	    (X))
	   ((OR (NULL (SETQ X (AR-2-REVERSE PKG 0 I)))
		(EQ X 'T))
	    (AS-2-REVERSE HASH PKG 0 I)
	    (AS-2-REVERSE SYM PKG 1 I)))
       (AND (> (SETF (PKG-NUMBER-OF-SYMBOLS PKG)
		     (1+ (PKG-NUMBER-OF-SYMBOLS PKG)))
	       (PKG-MAX-NUMBER-OF-SYMBOLS PKG))
	    (PKG-REHASH PKG)))

;Find a symbol in a package, to borrow it or forward to it.
;Refuses to create a symbol in a package which has subpackages.
(DEFUN PKG-INTERN-LOCAL (SYM &OPTIONAL (PKG PACKAGE) &AUX TEM PKG1 VAL)
    (PROG ()
	  (SETQ PKG (PKG-FIND-PACKAGE PKG))
	  (MULTIPLE-VALUE (VAL TEM PKG1)
	    (FUNCALL (COND ((PKG-SUBPACKAGES PKG) #'INTERN-SOFT) (T #'INTERN)) SYM PKG))
	  (OR PKG1 (FERROR NIL "No symbol ~A exists in package ~A" SYM PKG))
	  (RETURN VAL TEM PKG1)))

;Remove a symbol from a package.  Leaves T as the "hash code" where the
;symbol was, so that PKG-INTERN-INTERNAL will search past that point.
;Put NIL into the package-cell of the symbol so that we know it
;is uninterned.  If the user then interns it someplace else, its package
;cell will then be set to that as it should be.
;Returns T if the symbol was previously interned on that package, NIL if not.
(DEFUN REMOB (SYM &OPTIONAL (PKG (CAR (PACKAGE-CELL-LOCATION SYM))) &AUX HASH TEM STR)
  "Remove (unintern) any symbol whose pname matches SYM from a package.
The package may be specified, or defaults to SYM's package
/(in that case, SYM must be a symbol)."
  (COND ((NOT (NULL PKG))
	 (OR (PKG-PACKAGE-P PKG) (SETQ PKG (PKG-FIND-PACKAGE PKG)))
	 (SETQ HASH (%SXHASH-STRING (SETQ STR (STRING SYM)) 377))
	 (WITHOUT-INTERRUPTS
	   (MULTIPLE-VALUE (SYM TEM)
	     (PKG-INTERN-INTERNAL STR HASH PKG))
	   (COND (TEM
		  (AND (EQ (CAR (PACKAGE-CELL-LOCATION SYM)) PKG)
		       (RPLACA (PACKAGE-CELL-LOCATION SYM) NIL))
		  (AS-2-REVERSE T PKG 0 TEM)
		  (AS-2-REVERSE NIL PKG 1 TEM)
		  T))))))

(DEFUN MAPATOMS (FUNCTION &OPTIONAL (PKG PACKAGE) (SUPERIORS-P T))
  "Call FUNCTION on each symbol in package PKG.
If SUPERIORS-P is supplied as NIL, symbols inherited from
PKG's superiors are not included."
    (SETQ PKG (PKG-FIND-PACKAGE PKG))
    (DO I (1- (PKG-NUMBER-OF-SLOTS PKG)) (1- I) (MINUSP I)
	(AND (NUMBERP (AR-2-REVERSE PKG 0 I))
	     (FUNCALL FUNCTION (AR-2-REVERSE PKG 1 I))))
    (AND SUPERIORS-P
	 (PKG-SUPER-PACKAGE PKG)
	 (MAPATOMS FUNCTION (PKG-SUPER-PACKAGE PKG) T)))

;MAPATOMS over all packages in the world.
(DEFUN MAPATOMS-ALL (FUNCTION &OPTIONAL (TOP-PKG PKG-GLOBAL-PACKAGE))
  "Call FUNCTION on each symbol in TOP-PKG and all packages under it.
TOP-PKG defaults to Global.  Superiors of TOP-PKG are not included."
    (SETQ TOP-PKG (PKG-FIND-PACKAGE TOP-PKG))
    (MAPATOMS FUNCTION TOP-PKG NIL)
    (MAPC (FUNCTION (LAMBDA (PKG) (MAPATOMS-ALL FUNCTION PKG)))
	  (PKG-SUBPACKAGES TOP-PKG)))

;Rehash a package into a larger has table.
(DEFUN PKG-REHASH (PKG &OPTIONAL (SIZE (* 2 (PKG-MAX-NUMBER-OF-SYMBOLS PKG)))
		       &AUX NEW-PKG)
       (SETQ NEW-PKG
	     (PKG-MAKE-PACKAGE MAKE-ARRAY (:LENGTH
					    (IF ARRAY-INDEX-ORDER
						(LIST (PKG-GOOD-SIZE SIZE) 2)
					      (LIST 2 (PKG-GOOD-SIZE SIZE)))
					   :AREA PKG-AREA)))
       (DO I (1- (ARRAY-LEADER-LENGTH PKG)) (1- I) (MINUSP I)
	   (STORE-ARRAY-LEADER (ARRAY-LEADER PKG I) NEW-PKG I))
       (SETF (PKG-NUMBER-OF-SYMBOLS NEW-PKG) 0)
       (SETF (PKG-MAX-NUMBER-OF-SYMBOLS NEW-PKG) SIZE)
       (DO I (1- (PKG-NUMBER-OF-SLOTS PKG)) (1- I) (MINUSP I)
	   (COND ((NUMBERP (AR-2-REVERSE PKG 0 I))
		  (INTERN-LOCAL (AR-2-REVERSE PKG 1 I) NEW-PKG))))
       (STRUCTURE-FORWARD PKG NEW-PKG))

(DEFUN PKG-GOTO (&OPTIONAL (PKG PKG-USER-PACKAGE))	;Go to type-in package.
  "Set the current binding of PACKAGE to the package you specify (by name)."
  (SETQ PKG (PKG-FIND-PACKAGE PKG))
  (AND (PKG-SUBPACKAGES PKG)
       (FERROR NIL "Attempt to PKG-GOTO ~A, which has subpackages" PKG))
  (SETQ PACKAGE PKG))

(DEFUN PKG-GOTO-GLOBALLY (&OPTIONAL (PKG PKG-USER-PACKAGE))
  "Set the global binding of PACKAGE used by new lisp listeners
and by random processes that don't bind PACKAGE."
  (LET ((PACKAGE PACKAGE))
    (SETQ PKG (PKG-GOTO PKG)))
  (PROCESS-RUN-FUNCTION "Set Package" #'(LAMBDA (PKG) (PKG-GOTO PKG)) PKG)
  PKG)
  
(DEFUN PKG-FIND-PACKAGE (THING &OPTIONAL CREATE-P UNDER-PKG)
  "Find or possibly create a package named THING.
THING can be a package; then it is just returned.
It can also be a symbol or string; any package with that as a refname is returned.
 UNDER-PKG is the package to search the refname alist of; default is PACKAGE.
If no such package is found, CREATE-P says what to do:
 NIL means get an error,
 :FIND means return NIL,
 :ASK means create package and return it after getting confirmation,
 T means create package and return it.

If a package is created, it is made a subpackage of UNDER-PKG
 (or of Global if that is NIL).

If THING is a list, its car is a symbol or string giving the package name;
if no package exists, one is always created, and the list is interpreted as
/(NAME SUPERIOR SIZE)."

  ;; If a list was specified, create the package according to that list
  ;; if the package does not exist already.
  (AND (CONSP THING)
       (OR (NOT (PKG-FIND-PACKAGE (CAR THING) ':FIND))
	   (NOT (PKG-DECLARED (PKG-FIND-PACKAGE (CAR THING)))))
       (EVAL `(PACKAGE-DECLARE ,(FIRST THING) ,(SECOND THING) ,(THIRD THING) NIL . ,(CDDDR THING))))
  ;; Get just the package's name.
  (AND (CONSP THING) (SETQ THING (CAR THING)))
  (COND ((PKG-PACKAGE-P THING)
	 THING)
	((OR (SYMBOLP THING)
	     (STRINGP THING))
	 (DO ((X) (PKG (OR UNDER-PKG PACKAGE) (PKG-SUPER-PACKAGE PKG)))
	     ((NULL PKG)
	      ;; Refname not found on the alist(s) we are supposed to search.
	      (COND ((NULL CREATE-P)
		     (FERROR NIL "~A is not a meaningful refname in ~A"
			     THING (OR UNDER-PKG PACKAGE)))
		    ((EQ CREATE-P ':FIND)
		     NIL)
		    ((AND (EQ CREATE-P ':ASK)
			  (NOT (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
				       "~&Package ~A not found.  Create? "
				       THING)))
		     (CERROR T NIL NIL
			     "Please load package ~A declaration file then continue" THING)
		     (PKG-FIND-PACKAGE THING CREATE-P UNDER-PKG))
		    (T (PKG-CREATE-PACKAGE (STRING THING)
					   (OR UNDER-PKG "GLOBAL")))))
	   (AND (SETQ X (ASSOC-EQUALP (STRING THING) (PKG-REFNAME-ALIST PKG)))
		(RETURN (CADR X)))))
	(T (FERROR NIL "~A is not the right type to be a refname" THING))))

;; Takes a function, and destination and origin packages.  Other arguments
;; are whether to abbreviate the package name and a recursion count (initially 0).
;; The function is called on a sequence of strings, which are the refnames to get
;; to that package from the current package.  Doesn't deal with "shadowing"
;; among pkg refnames.
;; The function should take two arguments: the first is the refname
;; itself, and the second is the number of refnames left following
;; this one which must be processed.
;; P is the package being printed, PAKAJE is the package in which
;; the stuff will be read in, PKG is the one being considered at the moment.
;; Warning: ABBREVIATE-MODE is not what it says it is.  The printed representation
;; of a package object sets it to NIL, and PKG-PREFIX (the only other caller)
;; sets it to T.  The effect of the code as it is now is that packages print
;; themselves with their longest name, while package prefixes on symbols in printed
;; output and in QFASL files are the shortest name for the package.  This
;; may not be the most desirable thing.
(DEFUN PKG-MAP-REFNAMES (FCN P PAKAJE ABBREVIATE-P &OPTIONAL (CNT 0) &AUX TEM GOOD)
      (DO ((PKG PAKAJE (PKG-SUPER-PACKAGE PKG)))
	  ((NULL PKG)
	   (COND ((SETQ TEM (PKG-SUPER-PACKAGE P))  ;Can't get to it, first get to superior
		  (PKG-MAP-REFNAMES FCN TEM PAKAJE ABBREVIATE-P (1+ CNT))))
	   (FUNCALL FCN (PKG-NAME P) CNT))
	;; If not abbreviate mode, try very hard to get the package's real name
	;; as the name, since otherwise renaming a package takes a long time
	;; and many recompilations.
	(AND (NOT ABBREVIATE-P)
	     (SETQ TEM (ASSOC-EQUALP (PKG-NAME P) (PKG-REFNAME-ALIST PKG)))
	     (EQ (CADR TEM) P)
	     (RETURN (FUNCALL FCN (CAR TEM) CNT)))
	(DOLIST (PICKAGE (PKG-REFNAME-ALIST PKG))
	  (COND ((EQ (CADR PICKAGE) P)
		 (SETQ GOOD (CAR PICKAGE))
		 (OR ABBREVIATE-P (RETURN)))))
	(AND GOOD (RETURN (FUNCALL FCN GOOD CNT)))))

;;; This is the function which actually does the right thing for PRINT and FASD
;;; The FCN is the same as for PKG-MAP-REFNAMES
;;; What this function does is decide whether a package prefix is required.
;;; If one is required, PKG-MAP-REFNAMES is used to compute what it should be.
;;; A prefix is required if interning relative to the current package returns
;;; a different symbol (either it is shadowed or is in a different package).
;;; For efficiency, a hairier algorithm than that is used.
(DEFUN PKG-PREFIX (SYM FCN &OPTIONAL (PAKAJE PACKAGE))
  (LET ((PKG (CAR (PACKAGE-CELL-LOCATION SYM))))
    (COND ((NULL PKG) )                 ;If uninterned, no prefix
          ((EQ PKG PAKAJE) )            ;If in current package, no prefix
	  ((NULL PAKAJE)		;In prefix-all mode, single-component prefix
	   (PKG-MAP-REFNAMES FCN PKG PKG-GLOBAL-PACKAGE T))
          ((DO ((P (PKG-SUPER-PACKAGE PAKAJE) (PKG-SUPER-PACKAGE P)))
	       ((NULL P) T)		;If not superior to current, must have prefix
	     (AND (EQ P PKG) (RETURN NIL)))
           (PKG-MAP-REFNAMES FCN PKG PAKAJE T))
          ((NEQ (INTERN-SOFT SYM PAKAJE) SYM)   ;Else must actually intern and see if same
           (PKG-MAP-REFNAMES FCN PKG PAKAJE T)))))	;Hmm, shadowed, give prefix

(DEFUN PKG-SHORTEST-NAME (PKG &OPTIONAL (WITH-RESPECT-TO-PKG PKG-GLOBAL-PACKAGE))
  "Return the shortest name PKG can have on the refname alist of WITH-RESPECT-TO-PKG,
or if there is none, return the official name of PKG."
  (LET ((SHORTEST-NAME (PKG-NAME PKG)))
    (DOLIST (ELT (PKG-REFNAME-ALIST WITH-RESPECT-TO-PKG))
      (IF (AND (EQ (CADR ELT) PKG)
	       (NOT (EQUAL (CAR ELT) ""))
	       (< (STRING-LENGTH (CAR ELT))
		  (STRING-LENGTH SHORTEST-NAME)))
	  (SETQ SHORTEST-NAME (CAR ELT))))
    SHORTEST-NAME))

;Add a new entry to FROM-PKG package's refname-alist, which
;specifies that REFNAME means TO-PKG.
;Add at the end so that PKG-MAP-REFNAMES will find the primary name first.
(DEFUN PKG-REF-1 (FROM-PKG REFNAME &OPTIONAL (TO-PKG REFNAME) &AUX TEM)
  (SETQ TO-PKG (PKG-FIND-PACKAGE TO-PKG))
  (SETQ FROM-PKG (PKG-FIND-PACKAGE FROM-PKG))
  (AND (PKG-PACKAGE-P REFNAME) (SETQ REFNAME (PKG-NAME REFNAME)))
  (SETQ REFNAME (STRING REFNAME))
  (IF (SETQ TEM (ASSOC-EQUALP REFNAME (PKG-REFNAME-ALIST FROM-PKG)))
      (SETF (PKG-REFNAME-ALIST FROM-PKG)
	    (DELQ TEM (PKG-REFNAME-ALIST FROM-PKG))))
  (RPLACD (OR (LAST (PKG-REFNAME-ALIST FROM-PKG))
	      (LOCF (PKG-REFNAME-ALIST FROM-PKG)))
	  (CONS `(,REFNAME ,TO-PKG) NIL)))

(DEFUN PKG-REF (REFNAME &OPTIONAL (TO-PKG REFNAME))
       (PKG-REF-1 PACKAGE REFNAME TO-PKG))

;Get a list of all the packages USEd by a given package.
;DEFS-ONLY says to ignore a package which has no DEFS files in it.
;Otherwise, ask the user about each package, and then consider
;similarly all the packages used by that package (recursively, sort of).
(DEFUN PKG-FIND-USED-PACKAGES (PKG DEFS-ONLY &AUX PKGS
				   (CONSIDERED (LIST PKG))
				   (TRY-LIST (LIST PKG)))
    (DO () ((NULL TRY-LIST) PKGS)
	(SETQ PKG (CAR TRY-LIST))
	(POP TRY-LIST)

	(DO BRING-INS (PKG-BRING-IN-LIST PKG) (CDR BRING-INS) (NULL BRING-INS)
	    (LET ((PKG-1 (PKG-FIND-PACKAGE (CADAR BRING-INS))))
		 (COND ((NOT (MEMQ PKG-1 CONSIDERED))
			(PUSH PKG-1 CONSIDERED)
			(COND ((COND (DEFS-ONLY
				      (SOME (PKG-FILE-ALIST PKG-1)
					    #'(LAMBDA (FILESPEC)
					        (MEM #'STRING-EQUAL "DEFS" FILESPEC))))
				     ((Y-OR-N-P
					(FORMAT NIL "Process used package ~S? " PKG-1))))
			       (OR DEFS-ONLY (PUSH PKG-1 TRY-LIST))
			       (PUSH PKG-1 PKGS)))))))))

;Put (PKG-CONTAINED-IN "FOOBAR") at the front of a file
;to declare that it belongs to that package.
;It will cause an error if the file is loaded into the wrong one.
(DEFUN PKG-CONTAINED-IN (STRING)
    (OR (STRING-EQUAL STRING (PKG-PROGRAM-NAME PACKAGE))
	(FERROR NIL "This file belongs in the ~A package" STRING)))

(DEFUN PKG-KILL (PKG &AUX SUPER)
  "Kill a package by removing it from its superior's knowledge."
  (SETQ PKG (PKG-FIND-PACKAGE PKG))
  (AND (PKG-SUBPACKAGES PKG)
       (FERROR NIL "Killing a package ~A which has subpackages" PKG))
  (OR (SETQ SUPER (PKG-SUPER-PACKAGE PKG))
      (FERROR NIL "Killing a root package ~A" PKG))
  (SETF (PKG-SUBPACKAGES SUPER)
	(DELQ PKG (PKG-SUBPACKAGES SUPER)))
  (SETF (PKG-REFNAME-ALIST SUPER)
	(DEL (FUNCTION (LAMBDA (NAME REFSPEC) (EQ NAME (CAR REFSPEC))))
	     (PKG-PROGRAM-NAME PKG) (PKG-REFNAME-ALIST SUPER))))

;The (PACKAGE-DECLARE ...) list that describes a function call is not actually a form.
;It does the declaration both when macro expanded and when evaluated/loaded.
;So the package gets declared if you compile the file or if you load
;either the source or the qfasl.
;The PACKAGE-DECLARE for a package has this form:
;(PACKAGE-DECLARE package-name superpackage-name package-size
;		  nil    ;used to be the file alist.
;		  ....)  ;the body.
;The body should contain only calls to the pseudo-functions listed on the next page.

(DEFMACRO PACKAGE-DECLARE (&REST DECLARATION)
       "Define a package.  Works when compiled and when evaluated.
The BODY is used to extern or shadow symbols."
       (DECLARE (ARGLIST &QUOTE PACKAGE-NAME SUPERIOR-NAME SIZE NIL &REST BODY))
    (PKG-PROCESS-DECLARATION DECLARATION)
    `(PKG-PROCESS-DECLARATION ',DECLARATION))

(DEFUN PKG-PROCESS-DECLARATION (DECLARATION &AUX NAME SUPER SIZE FILE-ALIST BODY TEM
					    (PACKAGE PACKAGE)
					    ;Make sure nothing happens while rehashing, etc.
					    (INHIBIT-SCHEDULING-FLAG T))
    (SETQ NAME (FIRST DECLARATION)
	  SUPER
	  (IF (SECOND DECLARATION)
	      (PKG-FIND-PACKAGE (SECOND DECLARATION))
	    PKG-GLOBAL-PACKAGE)
	  SIZE (OR (THIRD DECLARATION) 1000)
	  FILE-ALIST (FOURTH DECLARATION)
	  BODY (CDDDDR DECLARATION))
    ;; Look for any existing package with the same name and superior,
    ;; and if there is one make this declaration apply to it
    ;; (and make it larger if it isn't as large as this decl says).
    ;; Otherwise, create a package.
    (DO ((L (PKG-SUBPACKAGES SUPER) (CDR L)))
	((NULL L)
	 (SETQ PACKAGE (PKG-CREATE-PACKAGE NAME SUPER SIZE)))
	(COND ((STRING-EQUAL (PKG-NAME (CAR L)) NAME) 
	       (SETQ PACKAGE (COND ((>= (PKG-MAX-NUMBER-OF-SYMBOLS (CAR L)) SIZE)
				    (CAR L))
				   (T (PKG-REHASH (CAR L) SIZE))))
	       (RETURN NIL))))
    (AND (PKG-SUBPACKAGES PACKAGE) FILE-ALIST
	 (FERROR NIL "Attempt to make the nonterminal package ~A contain files." PACKAGE))
    (SETF (PKG-FILE-ALIST PACKAGE) FILE-ALIST)
    ;; Make sure that each file knows what package it is in.    
    (MAPC (IF (NOT (FBOUNDP 'FS:PARSE-PATHNAME))
	      ;; While using MINI, build data structure for when pathnames exist.
	      #'(LAMBDA (FILE-DESC &AUX ELEM PLIST)
		  (OR (SETQ ELEM (ASSOC-EQUALP (CAR FILE-DESC) *COLD-LOADED-FILE-PROPERTY-LISTS*))
		      (PUSH (SETQ ELEM (LIST (CAR FILE-DESC) NIL NIL))
			    *COLD-LOADED-FILE-PROPERTY-LISTS*))
		  (SETQ PLIST (LOCF (THIRD ELEM)))
		  (OR (GET PLIST ':PACKAGE)
		      (PUTPROP PLIST (INTERN (PKG-NAME PACKAGE) PKG-USER-PACKAGE) ':PACKAGE)))
	      #'(LAMBDA (FILE-DESC &AUX PATHNAME)
		  (SETQ PATHNAME (FUNCALL (FS:PARSE-PATHNAME (CAR FILE-DESC))
					  ':GENERIC-PATHNAME))
		  (OR (FUNCALL PATHNAME ':GET ':PACKAGE)
		      (FUNCALL PATHNAME ':PUTPROP
			       (INTERN (PKG-NAME PACKAGE) PKG-USER-PACKAGE)
			       ':PACKAGE))))
	  FILE-ALIST)
    ;; Process the body only if this is the first time this package is declared.
    (COND ((NOT (PKG-DECLARED PACKAGE))
           (MAPC (FUNCTION (LAMBDA (BODYELT)
		     (AND (SETQ TEM (ASS #'STRING-EQUAL (CAR BODYELT) PKG-DECLARATION-NAMES))
			  (RPLACA BODYELT (CDR TEM)))))
		 BODY)
	   (MAPC (FUNCTION EVAL) BODY)
	   (SETF (PKG-DECLARED PACKAGE) T))))

;Here we provide the functions SHADOW, BORROW etc. thet are used in the body of
;a PACKAGE-DECLARE.  We define them as functions PKG-BORROW-SYMBOLS, etc.,
;and then provide symbols SHADOW, BORROW, etc. to go in the package in which
;PACKAGE-DECLAREs are read in, which point at those functions.

;This is what the functions do:
;(SHADOW sym sym sym ...) says that those symbols must be shadowed.
;(INTERN sym sym ...) interns those symbols, ensuring that they appear in either
;			this package or one of its superiors.  Mainly useful if this
;			package has inferiors.
;(EXTERNAL sym sym sym ...) says that those inherited symbols may be externally redefined.
;(FORWARD pkg sym sym sym ...) says that this package should have symbols with
;			those names, whose value, function and plist cells all
;			forward to the similarly named symbols in the specified package.
;(INDIRECT pkg sym sym ...) says that this package should have symbols thus named,
;			whose values are the similarly named symbols from the specified package
;(FORWARD-ALIAS pkg from to) says to make the local symbol named <from> forward to
;			the symbol named <to> in pkg.
;(INDIRECT-ALIAS pkg from to) says to make the local symbol named <from> have as its value
;			the symbol named <to> in pkg.
;(KEYWORD sym sym sym ...) says that the specified symbols should be shared from the
;			USER package.
;(ADVERTISE sym sym sym ...) says that any package that asks to USE this one
;			should forward to those symbols.
;(ADVERTISE (BORROW sym) ...) says that sym should be borrowed instead of forwarded to.
;(ADVERTISE (KEYWORD sym) ...) says that sym should be borrowed from USER
;			in this package and in any package which USEs this package.
;(USE pkg) says that this package should forward (or borrow, as specified) all
;			the ADVERTISEd symbols of pkg.
;(BORROW pkg sym sym sym ...) says that the specified symbols should be borrowed
;			from the specified package.
;			BORROW should be avoided if possible!
;(REFNAME refname pkg)    says that the pair (refname pkg) should go on the refname alist.
;(MYREFNAME package refname) makes refname on package's refname alist refer to
;			the package being declared.
;(SUBPACKAGE pkg refname) may be used to specify a package.  It returns the
;			package which is the binding of the specified refname 
;			searched for starting from the specified package.
;		This feature doesn't appear to have any code to support it.

;; NOW COME THE DEFINITIONS OF THE QUOTED-ARG FUNCTIONS WHICH THOSE SYMBOLS INDIRECT TO.

(DEFUN PKG-FORWARD-SYMBOLS (&QUOTE PKG &REST STRINGS)
    (PKG-BRING-IN-SYMBOLS 'PKG-FORWARD-SYMBOL PKG STRINGS))

(DEFUN PKG-INDIRECT-SYMBOLS (&QUOTE PKG &REST STRINGS)
    (PKG-BRING-IN-SYMBOLS 'PKG-INDIRECT-SYMBOL PKG STRINGS))

(DEFUN PKG-BORROW-SYMBOLS (&QUOTE PKG &REST STRINGS)
    (PKG-BRING-IN-SYMBOLS 'PKG-BORROW-SYMBOL PKG STRINGS))

(DEFUN PKG-KEYWORD-SYMBOLS (&QUOTE &REST STRINGS)
    (PKG-BRING-IN-SYMBOLS 'PKG-BORROW-SYMBOL 'USER STRINGS))

(DEFUN PKG-FORWARD-ALIAS (&QUOTE PKG FROM-SYMBOL TO-SYMBOL)
    (PKG-FORWARD-SYMBOL PKG FROM-SYMBOL TO-SYMBOL)
    (PUSH (LIST 'PKG-FORWARD-SYMBOL PKG FROM-SYMBOL TO-SYMBOL)
	  (PKG-BRING-IN-LIST PACKAGE)))

(DEFUN PKG-INDIRECT-ALIAS (&QUOTE PKG FROM-SYMBOL TO-SYMBOL)
    (PKG-INDIRECT-SYMBOL PKG FROM-SYMBOL TO-SYMBOL)
    (PUSH (LIST 'PKG-INDIRECT-SYMBOL PKG FROM-SYMBOL TO-SYMBOL)
	  (PKG-BRING-IN-LIST PACKAGE)))

(DEFUN PKG-BRING-IN-SYMBOLS (TYPE PKG STRINGS)
    (MAPC (FUNCTION (LAMBDA (STRING)
	      (FUNCALL TYPE PKG STRING STRING)
	      (PUSH (LIST TYPE PKG (STRING STRING))
		    (PKG-BRING-IN-LIST PACKAGE))))
	  STRINGS))

(DEFUN PKG-SHADOW-SYMBOLS (&QUOTE &REST STRINGS)
    (SETF (PKG-SHADOW-LIST PACKAGE)
	  (APPEND STRINGS (PKG-SHADOW-LIST PACKAGE)))
    (MAPC (FUNCTION (LAMBDA (STRING) (INTERN-LOCAL (STRING STRING))))
	  STRINGS))

(DEFUN PKG-INTERN-SYMBOLS (&QUOTE &REST STRINGS)
    (MAPC (FUNCTION (LAMBDA (STRING) (INTERN (STRING STRING))))
	  STRINGS))

(DEFUN PKG-EXTERN-SYMBOLS (&QUOTE &REST STRINGS)
    (SETF (PKG-EXTERNAL-LIST PACKAGE) (APPEND STRINGS (PKG-EXTERNAL-LIST PACKAGE))))

(DEFUN PKG-REF-DECL (&QUOTE REFNAME PKG)
    (PKG-FIND-PACKAGE PKG ':ASK)		;Create the package we refer to, maybe.
    (PKG-REF REFNAME PKG))

(DEFUN PKG-MYREFNAME-DECL (&QUOTE PKG REFNAME)
    (PKG-FIND-PACKAGE PKG ':ASK)		;Create the package we refer to, maybe.
    (PKG-REF-1 PKG REFNAME PACKAGE))

;; Actually make the local symbol FROM forward to the remote TO.
(DEFUN PKG-FORWARD-SYMBOL (PKG FROM &OPTIONAL (TO FROM))
    (PKG-FIND-PACKAGE PKG ':ASK)		;Create the package we refer to, maybe.
    (SETQ TO (PKG-INTERN-LOCAL (STRING TO) PKG))
    (SETQ FROM (INTERN (STRING FROM)))
    (AND (EQ FROM TO) (FERROR NIL "Forwarding the symbol ~S to itself" FROM))
    ;; Forward the value, function, and plist cells.
    (DO I 1 (1+ I) (= I 4)
	(%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE FROM I)
				  DTP-ONE-Q-FORWARD
				  (%MAKE-POINTER-OFFSET DTP-LOCATIVE TO I))))

;; Actually make the local symbol FROM have the remote TO as value.
(DEFUN PKG-INDIRECT-SYMBOL (PKG FROM &OPTIONAL (TO FROM))
    (PKG-FIND-PACKAGE PKG ':ASK)		;Create the package we refer to, maybe.
    (SETQ TO (PKG-INTERN-LOCAL (STRING TO) PKG))
    (SETQ FROM (INTERN (STRING FROM)))
    (AND (EQ FROM TO) (FERROR NIL "Indirecting the symbol ~S to itself" FROM))
    (SET FROM TO))

;; Actually intern the remote NAME locally.
(DEFUN PKG-BORROW-SYMBOL (PKG NAME &OPTIONAL IGNORE)
    (PKG-FIND-PACKAGE PKG ':ASK)		;Create the package we refer to, maybe.
    (INTERN-LOCAL (PKG-INTERN-LOCAL (STRING NAME) PKG)))

;;; The USE and ADVERTISE features.
;;; A package can ADVERTISE several symbols in its declaration,
;;; and another package can refer to all of them by USEing the first package.
;;; If you just advertise a symbol name, that symbol will be forwarded to.
;;; If you advertise (BORROW symbol), it will be borrowed.
;;; If you advertise (KEYWORD symbol), both you and your users will borrow from USER.

(DEFUN PKG-USE-PACKAGE (&QUOTE PKG)
    (PKG-USE-PACKAGE-EV PKG)
    (PUSH (LIST 'PKG-USE-PACKAGE-EV PKG)
	  (PKG-BRING-IN-LIST PACKAGE)))

;Actually forward or borrow the advertised symbols of a specified package.
;Also, put this package on the list of users of that package, so that
;if that package advertises any additional symbols they will be given to this one.
(DEFUN PKG-USE-PACKAGE-EV (PKG)
    (SETQ PKG (PKG-FIND-PACKAGE PKG ':ASK))
    (PUSH PACKAGE (PKG-USER-PACKAGES PKG))
    (PKG-USE-PACKAGE-1 (PKG-ADVERTISED-SYMBOLS PKG) PKG))

(DEFUN PKG-USE-PACKAGE-1 (ADVERTISEMENTS PKG &AUX TEM)
    (DO L ADVERTISEMENTS (CDR L) (NULL L)
	(FUNCALL (COND ((ATOM (CAR L)) 'PKG-FORWARD-SYMBOL)
		       ((SETQ TEM (ASS #'STRING-EQUAL (CAAR L)
				       '((BORROW . PKG-BORROW-SYMBOL)
					 (KEYWORD . PKG-BORROW-SYMBOL)
					 (INDIRECT . PKG-INDIRECT-SYMBOL)
					 (FORWARD . PKG-FORWARD-SYMBOL))))
			(CDR TEM))
		       ((FERROR NIL "Illegal advertisement ~S in ~S while declaring ~S"
				(CAR L) PKG PACKAGE)))
		 PKG
		 (COND ((ATOM (CAR L)) (CAR L))
		       (T (CADAR L))))))

(DEFUN PKG-ADVERTISE-SYMBOLS (&QUOTE &REST STRINGS)
    ;; Convert (... (BORROW FOO BAR) ...) into (... (BORROW FOO) (BORROW BAR) ...), etc.
    (SETQ STRINGS (MAPCAN (FUNCTION (LAMBDA (STR)
			      (COND ((ATOM STR) (LIST STR))
				    (T (MAPCAR (FUNCTION (LAMBDA (MEM)
						   (LIST (CAR STR) MEM)))
					       (CDR STR))))))
			  STRINGS))
    ;; Borrow as keywords any symbols advertised as keywords.
    (DO STRS STRINGS (CDR STRS) (NULL STRS)
	(AND (CONSP (CAR STRS)) (STRING-EQUAL (CAAR STRS) 'KEYWORD)
	     (PKG-BORROW-SYMBOL PKG-USER-PACKAGE (CADAR STRS))))
    ;; Add these symbols to the list of what we advertise,
    (SETF (PKG-ADVERTISED-SYMBOLS PACKAGE) (NCONC STRINGS (PKG-ADVERTISED-SYMBOLS PACKAGE)))
    ;; and give them to any packages already USEing this one.
    (DO PKGS (PKG-USER-PACKAGES PACKAGE) (CDR PKGS) (NULL PKGS)
	(LET ((PKG1 PACKAGE)
	      (PACKAGE (CAR PKGS)))
	     (PKG-USE-PACKAGE-1 STRINGS PKG1))))

(declare (special globalize-fn-pkg globalize-val-pkg))

;Given a symbol, moves it into the GLOBAL package, or to whatever package
;is specified, from all packages under that one.
;All symbols with those names in other packages are forwarded.
;Values, properties and function definitions are all merged from
;those other symbols into these ones.  Multiple values or function
;definitions, such as cannot properly be merged, cause errors.

;Given a string instead of a symbol,
;it takes the symbol from USER (in case it is a keyword), or creates a new one.
(defun globalize (string &optional (into-package "GLOBAL")
			 &aux globalize-fn-pkg globalize-val-pkg sys)
  "Make there be only one symbol with name STRING.
If INTO-PACKAGE is specified, we apply only to that package
and its subpackages."
  (setq into-package (pkg-find-package into-package))
  (and (stringp string) (eq into-package pkg-global-package)
       (intern-soft string pkg-user-package)
       (setq string (intern-soft string pkg-user-package)))
  (globalize-1 (setq sys (intern string into-package))
	       into-package)
  (setf (symbol-package sys) into-package))

;Given a newly created symbol in GLOBAL, makes all symbols
;down below with that name forward to it, after merging in
;their definitions (barfing at multiple definitions).
(defun globalize-1 (global package &aux local)
    (cond ((and (setq local (intern-local-soft global package))
		(neq local global)
		( (%p-ldb-offset %%q-data-type local 3) dtp-one-q-forward))
	   (cond ((boundp local)
		  (and (boundp global)
		       (neq (symeval local) (symeval global))
		       (ferror nil "Multiple values for ~S, in ~A and ~A"
			       global (symbol-package global) (symbol-package local)))
		  (setq globalize-val-pkg package)
		  (set global (symeval local))))
	   (cond ((fboundp local)
		  (and (fboundp global)
		       (neq (fsymeval local) (fsymeval global))
		       (ferror nil "Multiple function definitions for ~S, in ~A and ~A"
			       global (symbol-package global) (symbol-package local)))
		  (setq globalize-fn-pkg package)
		  (fset global (fsymeval local))))
	   (do ((plist (plist local) (cddr plist))) ((null plist))
	     (and (get global (car plist))
		  (neq (get global (car plist)) (cadr plist))
		  (ferror nil "Multiple values for ~S property of ~S" (car plist) global))
	     (putprop global (cadr plist) (car plist)))
	   (do i 1 (1+ i) (= i 4)
	       (%p-store-tag-and-pointer (%make-pointer-offset dtp-locative local i)
					 dtp-one-q-forward
					 (%make-pointer-offset dtp-locative global i)))))
    (mapc (function globalize-1) (circular-list global) (pkg-subpackages package)))

(DEFUN WHERE-IS (PNAME &OPTIONAL (UNDER-PKG PKG-GLOBAL-PACKAGE)
		 &AUX FOUND-IN-PKG FROM-PKGS RETURN-LIST)
  "Find all symbols with a given pname, which packages they are in,
and which packages they are accessible from."
  (DECLARE (SPECIAL RETURN-LIST))
  ;; Given a string, it should probably be uppercased.  But given a symbol copy it exactly.
  (SETQ PNAME (IF (STRINGP PNAME) (STRING-UPCASE PNAME) (STRING PNAME)))
  (FORMAT T "~&")
  ;; Each entry in TABLE is (from-pkg found-in-pkg).  Highest package first.
  (LET ((TABLE (NREVERSE (WHERE-IS-INTERNAL PNAME UNDER-PKG NIL))))
    (IF (NULL TABLE) (FORMAT T "No symbols named ~S exist.~%" PNAME)
	(DO () ((NULL TABLE))
	  (SETQ FOUND-IN-PKG (CADAR TABLE)
		FROM-PKGS (SORT (MAPCAN #'(LAMBDA (X)
					    (COND ((EQ (CADR X) FOUND-IN-PKG)
						   (SETQ TABLE (DELQ X TABLE 1))
						   (NCONS (PKG-NAME (CAR X))))))
					TABLE)
				#'STRING-LESSP))
	  (FORMAT T "~A:~A is accessible from package~P ~{~<~%~10X~2:;~A~>~^, ~}~%"
		      (PKG-NAME FOUND-IN-PKG) PNAME (LENGTH FROM-PKGS) FROM-PKGS))))
  RETURN-LIST)

(DEFUN WHERE-IS-INTERNAL (PNAME PKG TABLE)
  (DECLARE (SPECIAL RETURN-LIST))
  (MULTIPLE-VALUE-BIND (SYM FOUND FOUND-IN-PKG) (INTERN-SOFT PNAME PKG)
    (COND (FOUND
	   (PUSH (LIST PKG FOUND-IN-PKG) TABLE)
	   (OR (MEMQ SYM RETURN-LIST) (PUSH SYM RETURN-LIST)))))
  (DOLIST (SUBPKG (PKG-SUBPACKAGES PKG))
    (SETQ TABLE (WHERE-IS-INTERNAL PNAME SUBPKG TABLE)))
  TABLE)

;; Make a new package like an old one, but suitable for debugging
;; since its externals will be inhibited and will not clobber anything "installed".
;; All subpackages have copies made as well.
;; We don't actually finish the job of creating the package,
;; but just leave the PKG-BRING-IN-LIST, etc., set up as copied.
;; You can alter them all ad lib.
;; Then call PKG-DEBUG-FINISH on the new package to obey whatever those
;; lists have been altered to say.
(DEFUN PKG-DEBUG-COPY (PKG &OPTIONAL NAME SUPER SIZE &AUX NEW-PKG)
    (SETQ NEW-PKG (PKG-CREATE-PACKAGE (OR NAME (STRING-APPEND (PKG-NAME PKG) "-DEBUG"))
				      (OR SUPER (PKG-SUPER-PACKAGE PKG))
				      (OR SIZE (PKG-MAX-NUMBER-OF-SYMBOLS PKG))))
    (SETF (PKG-FILE-ALIST NEW-PKG) (PKG-FILE-ALIST PKG))
    (SETF (PKG-REFNAME-ALIST NEW-PKG) (PKG-REFNAME-ALIST PKG))
    ;; If we have specified a change of superior,
    ;; then any refnames which the new superior has which point to
    ;; the package we are copying must be changed to point at the copy.
    (AND SUPER (MAPC (FUNCTION (LAMBDA (REFPAIR)
		         (AND (EQ (CADR REFPAIR) PKG)
			      (SETF (CADR REFPAIR) NEW-PKG))))
		     (PKG-REFNAME-ALIST SUPER)))
    (SETF (PKG-PROGRAM-NAME NEW-PKG) (PKG-PROGRAM-NAME PKG))
    (SETF (PKG-BRING-IN-LIST NEW-PKG) (PKG-BRING-IN-LIST PKG))
    ;; If this is the uppermost lever of copying,
    ;; all our normal externals should be shadowed instead.
    ;; Otherwise, our superior is someone being debugged
    ;; and he should indeed get our externals.
    (COND (SUPER
	   (SETF (PKG-SHADOW-LIST NEW-PKG)
		 (PKG-SHADOW-LIST PKG))
	   (SETF (PKG-EXTERNAL-LIST NEW-PKG)
		 (PKG-EXTERNAL-LIST PKG)))
	  (T (SETF (PKG-SHADOW-LIST NEW-PKG)
		   (APPEND (PKG-EXTERNAL-LIST PKG)
			   (PKG-SHADOW-LIST PKG)))))
    ;; Copy all subpackages of old package as subpackages of new.
    (MAPC (FUNCTION (LAMBDA (SUBPKG)
	      (PKG-DEBUG-COPY SUBPKG NIL NEW-PKG)))
	  (PKG-SUBPACKAGES PKG)))

(DEFUN PKG-DEBUG-FINISH (PKG)
    (LET ((PACKAGE PKG))
	 (MAPC (FUNCTION INTERN-LOCAL) (PKG-SHADOW-LIST PACKAGE))
	 (MAPC (FUNCTION FUNCALL) (PKG-BRING-IN-LIST PACKAGE)))
    ;; Finish up all subpackages of the new package, also.
    (MAPC (FUNCTION PKG-DEBUG-FINISH)
	  (PKG-SUBPACKAGES PKG)))

(SETQ PKG-IS-LOADED-P T)

