;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.44
;;; Reason: m-x Delete {Non}matching Lines say how many lines they deleted
;;; m-x Create Link
;;; m-x Upper/Lowercase Lisp Code In Region understands that "#\" quotes the next char
;;; Improvements to args to {c-u} c-x c-;
;;; c-g/abort don't beep when they make region go away
;;; :allow-other-keys in flavor init plists
;;; more type randomness
;;; phase returns correct angle
;;; setf method for %logldb
;;; give oddp and evenp function definitions
;;; c-clear-input is a global asynchronous character doing the same
;;;   as terminal clear-input, but perhaps a little faster
;;; terminal key documentation
;;; more multiply matrices featurism
;;; closing serial streams
;;; vector/array-pop do not do any bogus destruction of elements beyond fill-pointer
;;; constraint-frame vs selection-substitutes vs zwei-mini-buffer bug -- rms
;;; Written 21-Mar-84 23:17:36 by Mly,
;;; while running on Lisp Machine Eighteen from band 7
;;; with System 98.37, CADR 3.6, ZMail 53.13, MIT-Specific 22.0, microcode 309, ZM MIT.

(setf (documentation 'time:verify-date 'function)
  "If the day of the week of the date specified by DATE, MONTH, and YEAR
is the same as DAY-OF-THE-WEEK, return NIL; otherwise, return a string that
contains a suitable error message. If YEAR is less than 100, it is shifted
by centuries until it is within 50 years of the present.")

(setf (documentation 'time:*cumulative-month-days-table* 'variable)
  "One-based array of cumulative days per month.")

(setf (documentation 'time:*MONTH-LENGTHS* 'variable)
  "One-based list of lengths of months.")

(when (eq si:site-name :mit)
  (load "SYS:IO;MITSPECIFIC QFASL >" :set-default-pathname nil :verbose nil))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-LIST (L &AUX (*PRINT-CIRCLE* T))
  (FORMAT T "~%~S is a list" L))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-COMPLEX-NUMBER (NUMBER)
  (FORMAT T "~&~S is a complex number with real part ~S and imaginary part ~S."
	  NUMBER (REALPART NUMBER) (IMAGPART NUMBER)))

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-DELETE-MATCHING-LINES "Delete all lines containing the specified string.
Covers from point to the end of the buffer" ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Flush lines containing:"
					  *SEARCH-MINI-BUFFER-COMTAB*)
    (LET ((BP (BEG-LINE (POINT) 0)))
      (DO ((N 0 (1+ N))) (())
	(UNLESS (SETQ BP (FUNCALL FUNCTION BP KEY))
	  (IF (ZEROP N)
	      (BARF "~&No occurences found.")
	    (FORMAT *QUERY-IO* "~&~D line~:P deleted." N))
	  (RETURN NIL))
	(DELETE-INTERVAL (BEG-LINE BP 0)
			 (SETQ BP (BEG-LINE BP 1 T))))))
  DIS-TEXT)

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-DELETE-NON-MATCHING-LINES "Delete all lines not containing the specified string.
Covers from point to the end of the buffer" ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Keep lines containing:"
					  *SEARCH-MINI-BUFFER-COMTAB*)
    (LET ((BP (BEG-LINE (POINT) 0))
	  (NEW-BP))
      (DO ((N 0 (1+ N))) (())
	;; BP points just after the last matching line.
	(SETQ NEW-BP (FUNCALL FUNCTION BP KEY NIL))
	(WHEN (NULL NEW-BP)
	  ;; No more matching lines => delete all after there.
	  (DELETE-INTERVAL BP (INTERVAL-LAST-BP *INTERVAL*) T)
	  (IF (ZEROP N)
	      (FORMAT *QUERY-IO* "~&No lines matched -- rest of buffer killed.")
	    (FORMAT *QUERY-IO* "~&~D line~:P retained." N))
	  (RETURN NIL))
	;; Else delete all from there to beginning of the matching line.
	(DELETE-INTERVAL BP (BEG-LINE NEW-BP 0) T)
	;; Set BP to point after the new matching line.
	(OR (SETQ BP (BEG-LINE NEW-BP 1)) (RETURN NIL)))))
  DIS-TEXT)

))

; From file FILES.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFCOM COM-CREATE-LINK "Creates a link between two files." ()
  (LET* ((LINK (READ-DEFAULTED-PATHNAME
		 "Pathname of link to be created:" *PATHNAME-DEFAULTS*))
	 (TO (READ-DEFAULTED-PATHNAME
	       (FORMAT NIL "Pathname of target of link ~A:" LINK)
	       LINK NIL NIL ':WRITE))
	 (RESULT (FS:CREATE-LINK LINK TO ':ERROR NIL)))
    (IF (ERRORP RESULT)
	(BARF "Cannot create link ~A: ~A" LINK RESULT)
      (FORMAT *QUERY-IO* "~&Link ~A => ~A created." LINK TO)))
  DIS-NONE)

))

; From file COMH.LISP PS:<MLY.L> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFUN LISP-CHANGE-CASE (UP-P &AUX START-BP BP CH SYNTAX)
  (REGION (BP1 BP2)
    (MULTIPLE-VALUE-BIND (STRING-P SLASHIFIED-P COMMENT-P) (LISP-BP-SYNTACTIC-CONTEXT BP1)
      (FLET ((DO-IT () (WITH-UNDO-SAVE ((IF UP-P "Upcase Lisp Code" "Downcase Lisp Code")
					START-BP BP T)
			 (IF UP-P (UPCASE-INTERVAL START-BP BP T)
			   (DOWNCASE-INTERVAL START-BP BP T)))))
	  (SETQ START-BP (COPY-BP BP1) BP (COPY-BP BP1))
	  (DO ()
	      ((BP-= BP BP2)
	       (UNLESS (OR COMMENT-P STRING-P SLASHIFIED-P)
		 (DO-IT)))
	    (IBP BP)
	    (SETQ CH (BP-CH-CHAR BP))
	    (SETQ SYNTAX (LIST-SYNTAX CH))
	    (COND (COMMENT-P
		   (WHEN (= CH #/RETURN)
		     (SETQ COMMENT-P NIL)
		     (MOVE-BP START-BP BP)
		     (IBP START-BP)))
		  (SLASHIFIED-P
		   (SETQ SLASHIFIED-P NIL)
		   (MOVE-BP START-BP BP)
		   (IBP START-BP))
		  ((= SYNTAX LIST-SLASH)
		   (SETQ SLASHIFIED-P BP)
		   (UNLESS STRING-P
		     (DO-IT)))
		  (STRING-P
		   (WHEN (= CH STRING-P)
		     (SETQ STRING-P NIL)
		     (MOVE-BP START-BP BP)
		     (IBP START-BP)))
		  ((OR (= SYNTAX LIST-COMMENT)
		       (= SYNTAX LIST-DOUBLE-QUOTE))
		   (IF (= SYNTAX LIST-COMMENT)
		       (SETQ COMMENT-P T)
		     (SETQ STRING-P CH))
		   (DO-IT))
		  ;; special kludge for Zetalisp #\ #/ equivalence. Sigh
		  ((AND (NOT (OR COMMENT-P STRING-P SLASHIFIED-P))
			(CHAR-EQUAL CH #/\)
			(CHAR-EQUAL (BP-CHAR-BEFORE BP) #/#))
		   (SETQ SLASHIFIED-P T)
		   (DO-IT)))))))
  DIS-TEXT)

))

; From file COMH.LISP PS:<MLY.L> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-COMMENT-OUT-REGION "Stick comment start characters at the start of each line in the region.
Adds regardless of any that may already be there.
A numeric argument specifies how many to add. A negative argument species how many to delete.
An argument of U is treated like -1: it deletes single comment starts." ()
  (IF (EQ *NUMERIC-ARG-P* ':CONTROL-U)
      (SETQ *NUMERIC-ARG* (// *NUMERIC-ARG* -4)))
  (IF (> *NUMERIC-ARG* 0)
      (REGION-LINES (START-LINE END-LINE)
	(LET* ((LEN (LENGTH *COMMENT-BEGIN*))
	       (INSERT (MAKE-STRING (* *NUMERIC-ARG* LEN))))
	  (DOTIMES (I *NUMERIC-ARG*)
	    (COPY-ARRAY-PORTION *COMMENT-BEGIN* 0 LEN
				INSERT (* I LEN) (+ (* I LEN) LEN)))
	  (DO ((LINE START-LINE (LINE-NEXT LINE))
	       (BP (CREATE-BP START-LINE 0)))
	      ((EQ LINE END-LINE))
	    (MOVE-BP BP LINE 0)
	    (UNLESS (MEMQ (LINE-TYPE LINE) '(:BLANK))
	      (INSERT BP INSERT)))
	  (RETURN-ARRAY INSERT)))
    (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
    (COM-UNCOMMENT-OUT-REGION))
  DIS-TEXT)

))

; From file COMH.LISP PS:<MLY.L> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-UNCOMMENT-OUT-REGION "Remove comment start charcaters from the start of each line in the region
which begins with one. A numeric arg specifies now many to remove." ()
  (REGION-LINES (START-LINE END-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (BP (CREATE-BP START-LINE 0))
	 (BPA (CREATE-BP START-LINE 1)))
	((EQ LINE END-LINE))
      (DOTIMES (I *NUMERIC-ARG*)
	(IF (OR (EQ (LINE-TYPE LINE) ':BLANK)
		(NOT (STRING-EQUAL LINE *COMMENT-BEGIN* 0 0
				   (LENGTH *COMMENT-BEGIN*))))
	    (RETURN)
	  (MOVE-BP BP LINE 0)
	  (MOVE-BP BPA LINE (LENGTH *COMMENT-BEGIN*))
	  (DELETE-INTERVAL BP BPA T))))))

))

; From file COMH.LISP PS:<MLY.L> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-LOWERCASE-LISP-CODE-IN-REGION "Lowercase the region, but not strings, comments, etc.
Characters preceded by escape characters are also immune." ()
  (LISP-CHANGE-CASE NIL))

))

; From file COMH.LISP PS:<MLY.L> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMH  "

(DEFCOM COM-UPPERCASE-LISP-CODE-IN-REGION "Uppercase the region, but not strings, comments, etc.
Characters preceded by escape characters are also immune." ()
  (LISP-CHANGE-CASE T))

))

zwei:(set-comtab *standard-comtab*
		 nil
		 '(("Create Link" . com-create-link)
		   ("Uppercase Lisp Code in Region" . com-uppercase-lisp-code-in-region)
		   ("Lowercase Lisp Code in Region" . com-lowercase-lisp-code-in-region)))

; From file ZMACS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN LOAD-FILE-INTO-ZMACS (PATHNAME &OPTIONAL (MERGE-PATHNAME-DEFAULTS-P T))
  (LET (*WINDOW*
	(*MODE-LIST-SYNTAX-TABLE* *LIST-SYNTAX-TABLE*)
	(*PRINT-BASE* *PRINT-BASE*)
	(*READ-BASE* *READ-BASE*)
	(*DEFAULT-COMMON-LISP* *DEFAULT-COMMON-LISP*)
	(PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME)))
    (IF MERGE-PATHNAME-DEFAULTS-P
	(FS:SET-DEFAULT-PATHNAME PATHNAME FS:LOAD-PATHNAME-DEFAULTS))
    (FIND-FILE PATHNAME NIL)))

))


; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(defsubst flavor-unhandled-init-keywords (flavor)
  (get (locf (flavor-plist flavor)) 'unhandled-init-keywords))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN INSTANTIATE-FLAVOR (FLAVOR-NAME INIT-PLIST
		           &OPTIONAL SEND-INIT-MESSAGE-P
				     RETURN-UNHANDLED-KEYWORDS-P ;as second value
				     AREA-TO-CONS-INSTANCE-IN
			   &AUX FL UNHANDLED-KEYWORDS INSTANCE VARS
				   NEW-PLIST)
  "Create and return an instance of the specified FLAVOR-NAME, low level.
INIT-PLIST's CDR is the list of init keywords and their values.
This list will be modified destructively so that any default init plist
keywords (except those that just set instance variables) are on it.
We send a :INIT message only if SEND-INIT-MESSAGE-P is non-nil.
That may further modify the INIT-PLIST.

If RETURN-UNHANDLED-KEYWORDS-P is non-nil, our second value is an
alternating list of keywords and values for those keywords specified in
INIT-PLIST (or in the default init plist) which the flavor doesn't handle.
If RETURN-UNHANDLED-KEYWORDS-P is nil, it is an error if there are any such."
  ;; Trace any chain of alias flavors to a non-alias flavor.
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))
	     "the name of an instantiable flavor, or alias thereof")
  (LET ((TEM (FLAVOR-GET FL ':INSTANTIATION-FLAVOR-FUNCTION)))
    (WHEN TEM
      (SETQ TEM (FUNCALL TEM FL INIT-PLIST))
      (UNLESS (AND (SYMBOLP TEM)
		   (GET TEM 'FLAVOR))
	(FERROR NIL "The INSTANTIATION-FLAVOR-FUNCTION for flavor ~S
returned an invalid value, ~S, not a flavor name." FLAVOR-NAME))
      (SETQ FLAVOR-NAME TEM
	    FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))))
  (WHEN (FLAVOR-GET FL ':ABSTRACT-FLAVOR)
    (FERROR NIL "~S is an abstract flavor (or alias of one) and may not be instantiated."
	    FLAVOR-NAME))
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-TABLE FL) (COMPOSE-METHOD-COMBINATION FL))
  (UNLESS AREA-TO-CONS-INSTANCE-IN
    (SETQ AREA-TO-CONS-INSTANCE-IN
	  (FUNCALL (OR (FLAVOR-GET FL 'INSTANCE-AREA-FUNCTION) 'IGNORE)
		   INIT-PLIST)))
  (LET ((MISSING-KEYWORDS
	  (SUBSET-NOT #'(LAMBDA (KEYWORD) (GET-LOCATION-OR-NIL INIT-PLIST KEYWORD))
		      (FLAVOR-GET FL 'REQUIRED-INIT-KEYWORDS))))
    (WHEN MISSING-KEYWORDS
      (FERROR NIL "Flavor ~S requires init keywords ~S that are missing."
	      FLAVOR-NAME MISSING-KEYWORDS)))
  ;; Make the instance object, then fill in its various fields
  (SETQ INSTANCE (%ALLOCATE-AND-INITIALIZE DTP-INSTANCE DTP-INSTANCE-HEADER
			   FL NIL AREA-TO-CONS-INSTANCE-IN (FLAVOR-INSTANCE-SIZE FL)))
  (SETQ VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
  ;; Default all instance variables to unbound
  (DO ((V VARS (CDR V))
       (I 1 (1+ I)))
      ((NULL V))
    (%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE INSTANCE I)
			      DTP-NULL (CAR V)))
  (setq unhandled-keywords (flavor-unhandled-init-keywords fl))
  (LET ((VAR-KEYWORDS (FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES FL))
	(REMAINING-KEYWORDS (FLAVOR-REMAINING-INIT-KEYWORDS FL)))
    ;; First, process any user-specified init keywords that
    ;; set instance variables.  When we process the defaults,
    ;; we will see that these are already set, and will
    ;; refrain from evaluating the default forms.
    ;; At the same time, we record any init keywords that this flavor doesn't handle.
    (DO ((PL (CDR INIT-PLIST) (CDDR PL))) ((NULL PL))
      (LET ((INDEX (FIND-POSITION-IN-LIST (CAR PL) VAR-KEYWORDS)))
	(COND (INDEX
	       (OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ INDEX)))
		   (SETF (%INSTANCE-REF INSTANCE (1+ INDEX)) (CADR PL))))
	      ((NOT (MEMQ (CAR PL) REMAINING-KEYWORDS))
	       (pushnew (CAR PL) UNHANDLED-KEYWORDS)))))
    ;; Now do all the default initializations, of one sort or other,
    ;; that have not been overridden.
    (DOLIST (D (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL))
      (OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ (CAR D))))
	  (SETF (%INSTANCE-REF INSTANCE (1+ (CAR D))) (EVAL (CADR D)))))
    ;; Now stick any default init plist items that aren't handled by that
    ;; onto the actual init plist.
    (DO ((PL (FLAVOR-REMAINING-DEFAULT-PLIST FL) (CDDR PL))) ((NULL PL))
      (OR (MEMQ-ALTERNATED (CAR PL) (CDR INIT-PLIST))
	  (progn
	    (unless (eq init-plist (locf new-plist))
	      (setq new-plist (cdr init-plist)
		    init-plist (locf new-plist)))
	    (setq new-plist (list* (car pl) (eval (cadr pl)) new-plist))))))
  ;; Complain if any keywords weren't handled, unless our caller
  ;; said it wanted to take care of this.
  (AND (NOT RETURN-UNHANDLED-KEYWORDS-P)
       UNHANDLED-KEYWORDS
       (not (get init-plist ':allow-other-keys))
       (FERROR NIL "Flavor ~S does not handle the init keyword~P ~{~S~^, ~}"
	       FLAVOR-NAME
	       (LENGTH UNHANDLED-KEYWORDS)
	       UNHANDLED-KEYWORDS))
  (IF SEND-INIT-MESSAGE-P
      (SEND INSTANCE ':INIT INIT-PLIST))
  (VALUES INSTANCE UNHANDLED-KEYWORDS))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN COMPOSE-FLAVOR-INITIALIZATIONS (FL &AUX ALIST
				       REMAINING-DEFAULT-PLIST ALL-INITTABLE-IVARS
				       AREA-FUNCTION REQUIRED-INIT-KEYWORDS
				       remaining-init-keywords
				       unhandled-init-keywords)
  (SETQ ALL-INITTABLE-IVARS (MAKE-LIST (LENGTH (FLAVOR-ALL-INSTANCE-VARIABLES FL))
				      ':AREA (IF *JUST-COMPILING* DEFAULT-CONS-AREA
					       BACKGROUND-CONS-AREA)))
  ;; First make the mask saying which ivars can be initted by init kywords.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (LET ((FFL (COMPILATION-FLAVOR FFL)))
      (OR AREA-FUNCTION
	  (SETQ AREA-FUNCTION (FLAVOR-GET FFL ':INSTANCE-AREA-FUNCTION)))
      (SETQ REQUIRED-INIT-KEYWORDS
	    (UNION REQUIRED-INIT-KEYWORDS (FLAVOR-GET FFL ':REQUIRED-INIT-KEYWORDS)))
      (OR (FLAVOR-DEFAULT-HANDLER FL)
	  (SETF (FLAVOR-DEFAULT-HANDLER FL)
		(GET (LOCF (FLAVOR-PLIST FFL)) ':DEFAULT-HANDLER)))
      (DOLIST (IIV (FLAVOR-INITTABLE-INSTANCE-VARIABLES FFL))
	(LET ((INDEX (FIND-POSITION-IN-LIST (CDR IIV) (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	  (AND INDEX
	       (SETF (NTH INDEX ALL-INITTABLE-IVARS)
		     (CAR IIV)))))))
  (setq remaining-init-keywords    
	(SUBSET-NOT #'MEMQ (FLAVOR-ALLOWED-INIT-KEYWORDS FL)
		    (CIRCULAR-LIST ALL-INITTABLE-IVARS)))
  (pushnew ':allow-other-keys remaining-init-keywords)
  (setf	(flavor-remaining-init-keywords fl) remaining-init-keywords)
  ;; Then look at all the default init plists, for anything there
  ;; that initializes an instance variable.  If it does, make an entry on ALIST.
  ;; Any that doesn't initialize a variable, put on the "remaining" list.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (COMPILATION-FLAVOR FFL))
    (DO ((L (GET (LOCF (FLAVOR-PLIST FFL)) ':DEFAULT-INIT-PLIST) (CDDR L))) ((NULL L))
      (LET* ((KEYWORD (CAR L)) (ARG (CADR L))
	     (INDEX (FIND-POSITION-IN-LIST KEYWORD ALL-INITTABLE-IVARS)))
	;; Remove this keyword from the list of required ones,
	;; since it is cannot ever be missing.
	(SETQ REQUIRED-INIT-KEYWORDS
	      (DELQ KEYWORD REQUIRED-INIT-KEYWORDS))
	(IF INDEX
	    ;; This keyword initializes an instance variable,
	    ;; so record an initialization of that variable if none found yet.
	    (OR (ASSQ INDEX ALIST)
		(PUSH (LIST INDEX ARG)
		      ALIST))
	  ;; This keyword does not just initialize an instance variable.
	  (unless (GET (LOCF REMAINING-DEFAULT-PLIST) KEYWORD)
	    (PUTPROP (LOCF REMAINING-DEFAULT-PLIST) ARG KEYWORD))
	  (unless (memq keyword remaining-init-keywords)
	    (pushnew keyword unhandled-init-keywords))
	  ;;(IF (MEMQ KEYWORD (FLAVOR-REMAINING-INIT-KEYWORDS FL))
	  ;;    (OR (GET (LOCF REMAINING-DEFAULT-PLIST) KEYWORD)
	  ;;        (PUTPROP (LOCF REMAINING-DEFAULT-PLIST) ARG KEYWORD))
	  ;;  (FERROR NIL "The flavor ~S has keyword ~S in its default init plist, but doesn't handle it" (FLAVOR-NAME FL) KEYWORD))
	  ))))
  (setf (flavor-unhandled-init-keywords fl) unhandled-init-keywords)
  ;; Then, look for default values provided in list of instance vars.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (COMPILATION-FLAVOR FFL))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES FFL))
      (AND (NOT (ATOM V))
	   ;; When we find one, put it in if there is no init for that variable yet.
	   (LET ((INDEX (FIND-POSITION-IN-LIST (CAR V) (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	     (AND (NOT (ASSQ INDEX ALIST))
		  (PUSH (LIST INDEX
			      (CADR V))
			ALIST))))))
  (IF AREA-FUNCTION
      (PUTPROP (LOCF (FLAVOR-PLIST FL)) AREA-FUNCTION 'INSTANCE-AREA-FUNCTION)
    (REMPROP (LOCF (FLAVOR-PLIST FL)) 'INSTANCE-AREA-FUNCTION))
  (IF REQUIRED-INIT-KEYWORDS
      (PUTPROP (LOCF (FLAVOR-PLIST FL)) REQUIRED-INIT-KEYWORDS 'REQUIRED-INIT-KEYWORDS)
    (REMPROP (LOCF (FLAVOR-PLIST FL)) 'REQUIRED-INIT-KEYWORDS))
  (SETF (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL) ALIST)
  (SETF (FLAVOR-REMAINING-DEFAULT-PLIST FL) REMAINING-DEFAULT-PLIST)
  (SETF (FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES FL) ALL-INITTABLE-IVARS))

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN DEFFLAVOR1 (FLAVOR-NAME INSTANCE-VARIABLES COMPONENT-FLAVORS OPTIONS
		   &AUX FFL ALREADY-EXISTS INSTV IDENTICAL-COMPONENTS
			GETTABLE SETTABLE INITTABLE SPECIAL-IVS
			OLD-SPECIAL-IVS OLD-DEFAULT-HANDLER
			OLD-DEFAULT-INIT-PLIST OLD-LOCAL-IVS OLD-INITTABLE-IVS
			OLD-INIT-KWDS OLD-INSTANCE-AREA-FUNCTION
			OLD-REQUIRED-INIT-KEYWORDS
			INIT-KEYWORDS INCLUDES METH-COMB
			NEW-PLIST (PL (LOCF NEW-PLIST))
			(DEFAULT-CONS-AREA
			  (IF *JUST-COMPILING* DEFAULT-CONS-AREA
			    *FLAVOR-AREA*)))
  (OR *JUST-COMPILING* (RECORD-SOURCE-FILE-NAME FLAVOR-NAME 'DEFFLAVOR))
  (WITHOUT-INTERRUPTS
    (COND ((AND (NOT *JUST-COMPILING*)
		(NOT (MEMQ FLAVOR-NAME *ALL-FLAVOR-NAMES*)))
	   (PUSH FLAVOR-NAME *ALL-FLAVOR-NAMES*)
	   ;; Push on the name without the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (CONS (GET-PNAME FLAVOR-NAME) FLAVOR-NAME))
	   ;; Push on the name with the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (LET ((*PACKAGE* NIL))
				(CONS (FORMAT NIL "~S" FLAVOR-NAME) FLAVOR-NAME)))
	   ;; Array is no longer sorted.
	   (STORE-ARRAY-LEADER NIL *ALL-FLAVOR-NAMES-AARRAY* 1))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
	(FERROR NIL "~S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
	(FERROR NIL "~S, which is not a symbol, was specified as a component flavor" CF)))
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (COND ((AND (SETQ ALREADY-EXISTS (COMPILATION-FLAVOR FLAVOR-NAME))
	      *USE-OLD-FLAVOR-INFO*)
	 (DOLIST (PROP DEFFLAVOR1-PRESERVED-PROPERTIES)
	   (PUTPROP PL (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS)) PROP)
		    PROP))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (DO ((L OPTIONS (CDR L))
       (OPTION) (ARGS))
      ((NULL L))
    (IF (ATOM (CAR L))
	(SETQ OPTION (CAR L) ARGS NIL)
      (SETQ OPTION (CAAR L) ARGS (CDAR L)))
    (SELECTQ OPTION
      (:GETTABLE-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ GETTABLE (UNION GETTABLE (OR ARGS INSTV))))
      (:SETTABLE-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ SETTABLE (UNION SETTABLE (OR ARGS INSTV))))
      ((:INITTABLE-INSTANCE-VARIABLES :INITABLE-INSTANCE-VARIABLES)
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ INITTABLE (UNION INITTABLE (OR ARGS INSTV))))
      (:SPECIAL-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ SPECIAL-IVS (UNION SPECIAL-IVS (OR ARGS INSTV))))
      (:INIT-KEYWORDS
       (SETQ INIT-KEYWORDS (UNION INIT-KEYWORDS ARGS)))
      (:INCLUDED-FLAVORS
       (SETQ INCLUDES (UNION INCLUDES ARGS)))
      (:NO-VANILLA-FLAVOR
       (PUTPROP PL T OPTION))
      (:ORDERED-INSTANCE-VARIABLES
       ;;Don't validate.  User may reasonably want to specify non-local instance
       ;;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
       ;;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (PUTPROP PL (OR ARGS INSTV) ':ORDERED-INSTANCE-VARIABLES))
      (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (PUTPROP PL (UNION (GET PL ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			  (OR ARGS INSTV))
		':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
      (:METHOD-COMBINATION
       (SETQ METH-COMB (NUNION-EQUAL METH-COMB ARGS)))
      (:DEFAULT-nHANDLER
       (PUTPROP PL (CAR ARGS) OPTION))
      ((:REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS
				     :REQUIRED-FLAVORS :REQUIRED-INIT-KEYWORDS)
       (PUTPROP PL (UNION ARGS (GET PL OPTION)) OPTION))
      ((:DOCUMENTATION :DEFAULT-INIT-PLIST :SELECT-METHOD-ORDER :ACCESSOR-PREFIX)
       (PUTPROP PL ARGS OPTION))
      (:ALIAS-FLAVOR
       (PUTPROP PL T ':ALIAS-FLAVOR))
      (:ABSTRACT-FLAVOR
       (PUTPROP PL T ':ABSTRACT-FLAVOR))
      (:INSTANCE-AREA-FUNCTION
       (PUTPROP PL (CAR ARGS) ':INSTANCE-AREA-FUNCTION))
      (:INSTANTIATION-FLAVOR-FUNCTION
       (PUTPROP PL (CAR ARGS) ':INSTANTIATION-FLAVOR-FUNCTION))
      ((:RUN-TIME-ALTERNATIVES :MIXTURE)
       (PUTPROP PL ARGS ':RUN-TIME-ALTERNATIVES)
       (PUTPROP PL 'CHOOSE-RUN-TIME-ALTERNATIVE ':INSTANTIATION-FLAVOR-FUNCTION)
       (PUTPROP PL (MAKE-RUN-TIME-ALTERNATIVE-ALIST FLAVOR-NAME ARGS)
		'RUN-TIME-ALTERNATIVE-ALIST))
      (OTHERWISE (FERROR NIL "~S is not a known DEFFLAVOR option." OPTION))))
  ;; All settable instance variables should also be gettable and inittable.
  (DOLIST (V SETTABLE)
    (OR (MEMQ V GETTABLE)
	(PUSH V GETTABLE))
    (OR (MEMQ V INITTABLE)
	(PUSH V INITTABLE)))
  ;; See whether there are any changes in component flavor structure from last time
  (SETQ IDENTICAL-COMPONENTS
	(AND ALREADY-EXISTS
	     *USE-OLD-FLAVOR-INFO*
	     (EQUAL COMPONENT-FLAVORS (FLAVOR-DEPENDS-ON ALREADY-EXISTS))
	     (EQUAL INCLUDES (FLAVOR-INCLUDES ALREADY-EXISTS))
	     (EQUAL (GET PL ':REQUIRED-FLAVORS)
		    (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS)) ':REQUIRED-FLAVORS))))	
  (AND ALREADY-EXISTS
       (SETQ OLD-SPECIAL-IVS (FLAVOR-SPECIAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-DEFAULT-HANDLER (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
				      ':DEFAULT-HANDLER)
	     OLD-DEFAULT-INIT-PLIST (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
					 ':DEFAULT-INIT-PLIST)
	     OLD-LOCAL-IVS (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INITTABLE-IVS (FLAVOR-INITTABLE-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET ALREADY-EXISTS ':INSTANCE-AREA-FUNCTION)
	     OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET ALREADY-EXISTS ':REQUIRED-INIT-KEYWORDS)
	     OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS ALREADY-EXISTS)))
  ;; If the flavor is being redefined, and the number or order of instance variables
  ;; is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give a warning
  ;; and disconnect from the old FLAVOR defstruct so that old instances will
  ;; retain the old information.  The instance variables can get changed either
  ;; locally or by rearrangement of the component flavors.
  (AND ALREADY-EXISTS
       (IF (AND *USE-OLD-FLAVOR-INFO*
		(EQUAL (GET PL ':ORDERED-INSTANCE-VARIABLES)
		       (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
			    ':ORDERED-INSTANCE-VARIABLES))
		(OR (EQUAL (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
			   INSTANCE-VARIABLES)
		    (EQUAL (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
				   (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS))
			   INSTV))
		(EQ (GET PL ':ALIAS-FLAVOR)
		    (FLAVOR-GET ALREADY-EXISTS ':ALIAS-FLAVOR))
		(OR IDENTICAL-COMPONENTS
		    (EQUAL (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       COMPONENT-FLAVORS INCLUDES)
			   (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       (FLAVOR-DEPENDS-ON ALREADY-EXISTS)
						       (FLAVOR-INCLUDES ALREADY-EXISTS)))))
	   (IF *JUST-COMPILING*
	       (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS NIL)))
	 (IF *JUST-COMPILING*
	     (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS T))
	   (SETQ ALREADY-EXISTS (PERFORM-FLAVOR-REDEFINITION FLAVOR-NAME)))))
  (WHEN (GET PL ':ALIAS-FLAVOR)
    (IF (CDR COMPONENT-FLAVORS)
	(FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		     "This alias flavor has more than one component."))
    (UNLESS COMPONENT-FLAVORS
      (FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		   "This alias flavor has no component to be the alias of."))
    (IF INSTANCE-VARIABLES
	(FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		     "This alias flavor has instance variables; they will be ignored.")))
  ;; Make the information structure unless the flavor already exists.
  (LET ((FL (OR ALREADY-EXISTS
		(AND (NOT *JUST-COMPILING*)
		     (GET FLAVOR-NAME 'UNDEFINED-FLAVOR))
		(MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))
    (SETF (FLAVOR-PACKAGE FL) *PACKAGE*)
    (SETF (FLAVOR-LOCAL-INSTANCE-VARIABLES FL) INSTANCE-VARIABLES)
    (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
    (LET ((OVEC (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL)))
      (SETF (FLAVOR-PLIST FL) NEW-PLIST)
      (IF OVEC (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL) OVEC)))
    (IF GETTABLE
	(SETF (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL) GETTABLE))
    (IF SETTABLE
	(SETF (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL) SETTABLE))
    (IF SPECIAL-IVS
	(SETF (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL) SPECIAL-IVS))
    (SETF (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL)
	  (LOOP FOR V IN INITTABLE COLLECT (CONS (CORRESPONDING-KEYWORD V) V)))
    (SETF (FLAVOR-INIT-KEYWORDS FL) INIT-KEYWORDS)
    (SETF (FLAVOR-INCLUDES FL) INCLUDES)
    ;; This can't be computed for real until flavor composition,
    ;; but this at least contains some of the right ones.
    (SETF (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FL)
	  (FLAVOR-KNOWN-UNMAPPED-INSTANCE-VARIABLES FL))
    ;; First remove old method-combination declarations, then add new ones
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
      (COND ((LOOP FOR DECL IN METH-COMB NEVER (MEMQ (CAR MTE) (CDDR DECL)))
	     (SETF (SECOND MTE) NIL)
	     (SETF (THIRD MTE) NIL))))
    (DOLIST (DECL METH-COMB)
      (LET ((TYPE (CAR DECL)) (ORDER (CADR DECL)) ELEM)
	;; Don't error-check TYPE now, its definition might not be loaded yet
	(DOLIST (MSG (CDDR DECL))
	  (OR (SETQ ELEM (ASSQ MSG (FLAVOR-METHOD-TABLE FL)))
	      (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
	  (SETF (SECOND ELEM) TYPE)
	  (SETF (THIRD ELEM) ORDER))))
    (IF *JUST-COMPILING*
	(COMPILATION-DEFINE-FLAVOR FLAVOR-NAME FL)
      ;; Make this a depended-on-by of its depends-on, or remember to do it later in
      ;; the case of depends-on's not yet defined.
      (DOLIST (COMPONENT-FLAVOR COMPONENT-FLAVORS)
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ FFL (GET COMPONENT-FLAVOR 'FLAVOR))
		 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
		(T (PUSH (CONS COMPONENT-FLAVOR FLAVOR-NAME)
			 *FLAVOR-PENDING-DEPENDS*)))))
      ;; Likewise for its includes
      (DOLIST (INCLUDED-FLAVOR (FLAVOR-INCLUDES FL))
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ FFL (GET INCLUDED-FLAVOR 'FLAVOR))
		 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
		(T (PUSH (CONS INCLUDED-FLAVOR FLAVOR-NAME)
			 *FLAVOR-PENDING-DEPENDS*)))))
      ;; If someone depends on this flavor, which wasn't defined until now, link them up.
      ;; If that flavor was flavor-composed, recompose it now.
      (WITHOUT-INTERRUPTS
	(DOLIST (X *FLAVOR-PENDING-DEPENDS*)
	  (COND ((EQ (CAR X) FLAVOR-NAME)
		 (OR (MEMQ (CDR X) (FLAVOR-DEPENDED-ON-BY FL))
		     (PUSH (CDR X) (FLAVOR-DEPENDED-ON-BY FL)))
		 (SETQ *FLAVOR-PENDING-DEPENDS*
		       (DELQ X *FLAVOR-PENDING-DEPENDS*))))))
      (PUTPROP FLAVOR-NAME FL 'FLAVOR)
      (REMPROP FLAVOR-NAME 'UNDEFINED-FLAVOR)
      ;; Now, if the flavor was redefined in a way that changes the methods but doesn't
      ;; invalidate old instances, we have to propagate some changes.
      (IF (AND ALREADY-EXISTS
	       (NOT IDENTICAL-COMPONENTS))
	  (PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FLAVOR-NAME)
	;; If the methods and instances are ok but other things have changed, notice that too.
	(OR (AND (EQUAL OLD-SPECIAL-IVS
			(FLAVOR-SPECIAL-INSTANCE-VARIABLES FL))
		 (EQUAL OLD-DEFAULT-INIT-PLIST
			(GET (LOCF (FLAVOR-PLIST FL))
			     ':DEFAULT-INIT-PLIST))
		 (EQUAL OLD-LOCAL-IVS
			(FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
		 ;; Get a warning every time, if there is a variable
		 ;; that is globally special but not in a :SPECIAL-INSTANCE-VARIABLES
		 (NOT (DOLIST (IV (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
			;; Elements can be lists (var init)
			(IF (CONSP IV) (SETQ IV (CAR IV)))
			(AND (GET IV 'SPECIAL)
			     (NOT (MEMQ IV (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL)))
			     (RETURN T))))
		 (EQUAL OLD-INITTABLE-IVS
			(FLAVOR-INITTABLE-INSTANCE-VARIABLES FL))
		 (EQUAL OLD-DEFAULT-HANDLER (GET (LOCF (FLAVOR-PLIST FL)) ':DEFAULT-HANDLER))
		 (EQUAL OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET FL ':INSTANCE-AREA-FUNCTION))
		 (EQUAL OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET FL ':REQUIRED-INIT-KEYWORDS))
		 (EQUAL OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS FL)))
	    (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR-NAME)))
      (flavor-hack-documentation flavor-name))
    FLAVOR-NAME))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun typep (object &optional (type nil type-specified-p))
  "T if OBJECT fits the data type specifier TYPE.
An obsolete mode of use is with one argument;
then the value is a type specifier describing OBJECT."
  (declare (arglist object type))
  (let (predicate expander structure-desc (type type) dtp)
    (cond ((not type-specified-p)
	   (setq dtp (%data-type object))
	   ;; Cannot use TYPE-OF, since we must
	   ;; for back-compatibility return keywords.
	   (cond ((= dtp dtp-instance)
		  (%p-contents-offset
		    (instance-flavor object)
		    %instance-descriptor-typename))
		 ((= dtp dtp-array-pointer)
		  (cond ((named-structure-p object))
			((stringp object) ':string)
			(t ':array)))
		 ((= dtp dtp-entity)
		  (class-symbol object))
		 ((= dtp dtp-extended-number) 
		  (select (%p-ldb-offset %%header-type-field object 0)
		    (%header-type-flonum ':flonum)
		    (%header-type-bignum ':bignum)
		    (%header-type-rational ':rational)
		    (%header-type-complex ':complex)
		    (otherwise ':random)))
		 ((cdr (assq dtp typep-one-arg-alist)))
		 (t ':random)))
	  ((setq predicate (get (if (atom type) type (car type)) 'type-predicate))
	   (if (atom type)
	       (funcall predicate object)
	     (apply predicate object (cdr type))))
	  ((setq dtp (or (rassq type type-of-alist) (rassq type typep-one-arg-alist)))
	   (= (%data-type object) (car dtp)))
	  ((setq expander (get (if (atom type) type (car type)) 'type-expander))
	   (typep object
		  (apply expander (if (atom type) nil (cdr type)))))
	  ((progn (unless (symbolp type)
		    (setq type (car type)))
		  (get type 'flavor))
	   (typep-structure-or-flavor object type))
	  ((or (and (setq structure-desc (get type 'defstruct-description))
		    (defstruct-description-named-p structure-desc))
	       (get type 'defstruct-named-p))
	   (typep-structure-or-flavor object type))
	  ((and (symbolp type) (fboundp 'class-symbolp) (class-symbolp type))
	   (and (entityp object)
		(subclass-of-class-symbol-p (class object) type)))
	  (t (typep object (cerror t nil ':wrong-type-arg
				   "~1@*~S is not a type known to TYPEP" 'typep type))))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (nil type-optimizer) (expression) `(progn ,(cadr expression) nil))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (t type-optimizer) (expression) expression `(progn ,(cadr expression) t))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun typep-two-args (form &aux opt type pred dtp)
  (cond ((and (= (length form) 3)
	      (constantp (caddr form)))
	 (setq type (if (consp (caddr form))
			(cadr (caddr form))	;(typep foo ':bar)
		      (caddr form)))		;(typep foo :bar)
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

(defprop nil t si:system-constant)
(defprop t t si:system-constant)

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defsubst invariable-form-p (form)
  (constantp form))

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN SPECIALP (SYMBOL)
  (DOLIST (DECL LOCAL-DECLARATIONS
		;; Here if no local declaration says anything.
		;; Try FILE-(UN)SPECIAL-LIST which reflect global decls in the file.
		(OR (MEMQ SYMBOL FILE-SPECIAL-LIST)
		    (AND (NOT (MEMQ SYMBOL FILE-UNSPECIAL-LIST))
			 (OR ALL-SPECIAL-SWITCH
			     (GET SYMBOL 'SPECIAL)
			     (GET SYMBOL 'SYSTEM-CONSTANT)
			     (MEMQ SYMBOL BARF-SPECIAL-LIST)
			     (MEMQ (SYMBOL-PACKAGE SYMBOL) SPECIAL-PKG-LIST)))))
    (AND (MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
	 (MEMQ SYMBOL (CDR DECL))
	 (RETURN (EQ (CAR DECL) 'SPECIAL)))))
))

; From file QCOPT.LISP PS:<MLY.L> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(DEFUN EQ-TYPEP-1 (FORM TYPE TOPFORM &aux pred)
  (SETQ PRED (OR (CAR (RASSQ TYPE '((STRINGP . STRING) (SYMBOLP . SYMBOL) (CONSP . LIST)
				    (STRINGP . :STRING) (SYMBOLP . :SYMBOL) (listp . :LIST))))
		 (CAR (RASSOC TYPE SI:TYPEP-ONE-ARG-ALIST))
		 (CAR (RASSOC TYPE SI:TYPE-OF-ALIST))))
  (COND ((NULL PRED) TOPFORM)
	((NUMBERP PRED) `(= (%DATA-TYPE ,FORM) ,PRED))
	((SYMBOLP PRED) `(,PRED ,FORM))
	(T TOPFORM)))

))

; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN QC-TRANSLATE-FUNCTION (FUNCTION-SPEC EXP QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE
			      &OPTIONAL (NAME-FOR-FUNCTION FUNCTION-SPEC))
  "Compile one function.  All styles of the compiler come through here.
QC-TF-PROCESSING-MODE should be MACRO-COMPILE or MICRO-COMPILE.
QC-TF-OUTPUT-MODE is used by LAP to determine where to put the compiled code.
 It is COMPILE-TO-CORE for making an actual FEF, QFASL, REL, or
 QFASL-NO-FDEFINE to simply dump a FEF without trying to define a function
EXP is the lambda-expression.
NAME-FOR-FUNCTION is what the fef's name field should say;
 if omitted, FUNCTION-SPEC is used for that too.
In MACRO-COMPILE mode, the return value is the value of QLAPP for the first function."
 (WHEN COMPILER-VERBOSE
   (FORMAT T "~&Compiling ~S" FUNCTION-SPEC))
 (OBJECT-OPERATION-WITH-WARNINGS (NAME-FOR-FUNCTION)
  (LET ((ERROR-MESSAGE-HOOK
	  (LET-CLOSED ((FUNCTION-BEING-PROCESSED NAME-FOR-FUNCTION))
	    #'(LAMBDA () (AND FUNCTION-BEING-PROCESSED
      			      (FORMAT T "Error occurred while compiling ~S"
				      FUNCTION-BEING-PROCESSED)))))
	(COMPILER-QUEUE
	  (NCONS
	    (MAKE-COMPILER-QUEUE-ENTRY
	      FUNCTION-SPEC FUNCTION-SPEC
	      FUNCTION-NAME NAME-FOR-FUNCTION
	      DEFINITION EXP
	      DECLARATIONS LOCAL-DECLARATIONS)))
	(INSIDE-QC-TRANSLATE-FUNCTION T)
	VAL
	THIS-FUNCTION-BARF-SPECIAL-LIST
	VARIABLES-LISTS)
    (DO ((L COMPILER-QUEUE (CDR L))
	 (DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA)
	 FUNCTION-TO-DEFINE
	 COMPILER-LEXICAL-ENVIRONMENT
	 COMPILER-LEXICAL-FUNCTIONS
	 COMPILER-LEXICAL-PROGDESCS
	 COMPILER-LEXICAL-RETPROGDESC
	 COMPILER-LEXICAL-GOTAGS
	 COMPILER-LEXICAL-MACROS
	 (EXP)
	 NAME-FOR-FUNCTION
	 THIS-FUNCTION-ARGLIST
	 THIS-FUNCTION-ARGLIST-FUNCTION-NAME
	 (LOCAL-DECLARATIONS))
	((NULL L))
      (SETF (FILL-POINTER QCMP-OUTPUT) 0)
      (SETQ FUNCTION-TO-DEFINE (COMPILER-QUEUE-ENTRY-FUNCTION-SPEC (CAR L)))
      (SETQ NAME-FOR-FUNCTION (COMPILER-QUEUE-ENTRY-FUNCTION-NAME (CAR L)))
      (SETQ EXP (COMPILER-QUEUE-ENTRY-DEFINITION (CAR L)))
      (SETQ LOCAL-DECLARATIONS (COMPILER-QUEUE-ENTRY-DECLARATIONS (CAR L)))
      (SETQ COMPILER-LEXICAL-ENVIRONMENT (COMPILER-QUEUE-ENTRY-VARIABLES (CAR L)))
      (SETQ COMPILER-LEXICAL-FUNCTIONS (COMPILER-QUEUE-ENTRY-FUNCTIONS (CAR L)))
      (SETQ COMPILER-LEXICAL-PROGDESCS (COMPILER-QUEUE-ENTRY-PROGDESCS (CAR L)))
      (SETQ COMPILER-LEXICAL-RETPROGDESC (COMPILER-QUEUE-ENTRY-RETPROGDESC (CAR L)))
      (SETQ COMPILER-LEXICAL-GOTAGS (COMPILER-QUEUE-ENTRY-GOTAGS (CAR L)))
      (SETQ COMPILER-LEXICAL-MACROS (COMPILER-QUEUE-ENTRY-MACROS (CAR L)))
      (OBJECT-OPERATION-WITH-WARNINGS (NAME-FOR-FUNCTION)
	(CATCH-ERROR-RESTART (ERROR "Give up on compiling ~S" NAME-FOR-FUNCTION)
	  (PUSH (QCOMPILE0 EXP FUNCTION-TO-DEFINE
			   (EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
			   NAME-FOR-FUNCTION)
		VARIABLES-LISTS)
	  (AND PEEP-ENABLE
	       (NEQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
	       (PEEP QCMP-OUTPUT FUNCTION-TO-DEFINE))
	  (COND ((NULL HOLDPROG))
		((EQ QC-TF-PROCESSING-MODE 'MACRO-COMPILE)
		 (SETQ EXP (QLAPP (G-L-P QCMP-OUTPUT) QC-TF-OUTPUT-MODE))
		 (OR VAL (SETQ VAL EXP)))
		((EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
		 (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
		   (MICRO-COMPILE (G-L-P QCMP-OUTPUT) QC-TF-OUTPUT-MODE)))))))
    (DOLIST (VL VARIABLES-LISTS)
      (DOLIST (V VL)
	(COND ((OR (STRING-EQUAL (VAR-NAME V) 'IGNORE)
		   (STRING-EQUAL (VAR-NAME V) 'IGNORED))
	       (OR (ZEROP (VAR-USE-COUNT V))
		   (WARN 'NOT-IGNORED ':IMPLAUSIBLE
			 "The variable ~S is bound and not ignored." (VAR-NAME V))))
	      ((GETF (VAR-DECLARATIONS V) 'IGNORE)
	       (OR (ZEROP (VAR-USE-COUNT V))
		   (WARN 'NOT-IGNORED ':IMPLAUSIBLE
			 "The variable ~S, which is declared to be ignored, was referenced"
			 (VAR-NAME V))))
	      ((NOT (GET (VAR-NAME V) 'IGNORABLE-VARIABLE))
	       (AND (ZEROP (VAR-USE-COUNT V))
		    (EQ (VAR-TYPE V) 'FEF-LOCAL)
		    (IF (GET (VAR-NAME V) 'LOCAL-FUNCTION-NAME)
			(WARN 'NOT-USED ':IMPLAUSIBLE
			      "The local function ~S is never used."
			      (GET (VAR-NAME V) 'LOCAL-FUNCTION-NAME))
		      (WARN 'NOT-USED ':IMPLAUSIBLE
			    "The variable ~S is bound but never used." (VAR-NAME V))))))))
    VAL)))

))

; From file QNEW.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QNEW  "

(defun print-file-warnings (pathname &optional (stream *standard-output*))
  "Output warnings data base for one file to a stream, in machine-readable form."
  (if (stringp pathname)
      (setq pathname (fs:merge-pathname-defaults pathname)))
  (format stream "~&;-*-Mode: Lisp; Package: User; Base: 10. -*-")
  (format stream "~%(SI:RELOAD-FILE-WARNINGS~%  '~S~%  '(" pathname)
  (let ((generic-pathname (if (symbolp pathname) pathname (send pathname ':generic-pathname)))
	(*package* pkg-user-package)
	(*print-base* 10.) (*read-base* 10.)
	(*readtable* initial-readtable)
	file-vars
	file-vals
	(first-operation t))  ;T for the first operation in the operation-alist.
    ;; Get the file's property bindings, but use them only
    ;; when we construct the string which is the text of the warning.
    (multiple-value (file-vars file-vals)
      (and (not (symbolp generic-pathname))
	   (fs:file-attribute-bindings generic-pathname)))
    (filter-warnings generic-pathname)
    (dolist (alist-elt (file-warnings-operation-alist generic-pathname))
      (if first-operation
	  (setq first-operation nil)
	(format stream "~%    "))
      (format stream "(~S NIL" (car alist-elt))
      (dolist (objw (file-warnings-object-alist alist-elt))
	(apply 'format stream "~%     (~S ~S ~S" objw)
	(dolist (w (object-warnings-warnings objw))
	  (multiple-value-bind (nil errorp)
	      (catch-error
		(let ((print-readably t))
		  (print w 'si:null-stream)))
	    (if errorp
		(format stream "~%      (~S ~S ~S /"~~A/" ~S)"
			(first w) (second w) (third w)
			;; Instead of outputting the warning's format-string and args,
			;; run them through format now.  Avoid problems if there is an
			;; object in the args that can't print readably.
			(progv file-vars file-vals
			       (apply 'format nil (fourth w) (nthcdr 4 w))))
	      ;; If we can print the list itself so it will read back, do so.
	      (format stream "~%      ~S" w))))
	(tyo #/) stream))
      (tyo #/) stream)))
  (format stream "))~%"))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defsubst rationalp (x)
  "T if X is an exact rational number (which includes integers)."
  (or (fixnump x)
      (and (= (%data-type x) dtp-extended-number)
	   (memq (%p-ldb-offset %%header-type-field x 0)
		 '(#,%header-type-bignum
		   #,%header-type-rational))
	   t)))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun phase (number)
  "Return the phase of NUMBER, in radians.
This is the angle in the complex plane from the positive real axis
to the ray from the origin through NUMBER.
It is between - (exclusive) and  (inclusive).
For a positive real, this is 0; for a negative real, this is . For 0, it is zero."
  (cond ((realp number)
	 (if (minusp number) pi 0))
	((zerop number) 0)
	(t
	 (atan2 (complex-imag-part number) (complex-real-part number)))))

))



; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defsubst bigp (x)
  "Return T if X is a bignum."
  (and (integerp x) (not (fixnump x))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defsubst ratiop (object)
  "T if OBJECT is a ratio -- a rational number which is not an integer."
  (and (= (%data-type object) dtp-extended-number)
       (= (%p-ldb %%header-type-field object) %header-type-rational)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defsubst flonump (object)
  "T if OBJECT is a full precision flonum."
  (and (floatp object)
       (= (%data-type object) dtp-extended-number)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defsubst subrp (object)
  "T if OBJECT is a compiled or built-in function."
  (or (eq (%data-type object) dtp-u-entry)
      (eq (%data-type object) dtp-fef-pointer)))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defun (caar setf-expand) (form)
  `(car (car ,(cadr form))))
(defun (cadr setf-expand) (form)
  `(car (cdr ,(cadr form))))
(defun (cdar setf-expand) (form)
  `(cdr (car ,(cadr form))))
(defun (cddr setf-expand) (form)
  `(cdr (cdr ,(cadr form))))
(defun (caaar setf-expand) (form)
  `(car (car ,(cadr form))))
(defun (caadr setf-expand) (form)
  `(car (cadr ,(cadr form))))
(defun (cadar setf-expand) (form)
  `(car (cdar ,(cadr form))))
(defun (caddr setf-expand) (form)
  `(car (cddr ,(cadr form))))
(defun (cdaar setf-expand) (form)
  `(cdr (caar ,(cadr form))))
(defun (cdadr setf-expand) (form)
  `(cdr (cadr ,(cadr form))))
(defun (cddar setf-expand) (form)
  `(cdr (cdar ,(cadr form))))
(defun (cdddr setf-expand) (form)
  `(cdr (cddr ,(cadr form))))
(defun (caaaar setf-expand) (form)
  `(car (caaar ,(cadr form))))
(defun (caaadr setf-expand) (form)
  `(car (caadr ,(cadr form))))
(defun (caadar setf-expand) (form)
  `(car (cadar ,(cadr form))))
(defun (caaddr setf-expand) (form)
  `(car (caddr ,(cadr form))))
(defun (cadaar setf-expand) (form)
  `(car (cdaar ,(cadr form))))
(defun (cadadr setf-expand) (form)
  `(car (cdadr ,(cadr form))))
(defun (caddar setf-expand) (form)
  `(car (cddar ,(cadr form))))
(defun (cadddr setf-expand) (form)
  `(car (cdddr ,(cadr form))))
(defun (cdaaar setf-expand) (form)
  `(cdr (caaar ,(cadr form))))
(defun (cdaadr setf-expand) (form)
  `(cdr (caadr ,(cadr form))))
(defun (cdadar setf-expand) (form)
  `(cdr (cadar ,(cadr form))))
(defun (cdaddr setf-expand) (form)
  `(cdr (caddr ,(cadr form))))
(defun (cddaar setf-expand) (form)
  `(cdr (cdaar ,(cadr form))))
(defun (cddadr setf-expand) (form)
  `(cdr (cdadr ,(cadr form))))
(defun (cdddar setf-expand) (form)
  `(cdr (cddar ,(cadr form))))
(defun (cddddr setf-expand) (form)
  `(cdr (cdddr ,(cadr form))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(define-setf-method %logldb (bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int)
    (let ((btemp (gensym))
	  (store (gensym))
	  (itemp (first stores)))
      (values (cons btemp temps)
	      (cons bytespec vals)
	      (list store)
	      `(progn 
		 ,(sublis (list (cons itemp `(%logdpb ,store ,btemp ,access-form)))
			  store-form)
		 ,store)
	      `(%logldb ,btemp ,access-form)))))

))

(eval-when (load compile eval)
  (remprop 'oddp 'compiler:p1)
  (remprop 'evenp 'compiler:p1))

(defun oddp (number)
  "T if NUMBER is odd."
  (oddp number))

(defun evenp (number)
  "T if NUMBER is even."
  (evenp number))


; From file COMB.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "

(DEFCOM COM-BEEP "Beep, and if not given a numeric arg turn off the region." ()
  (AND (MEMQ ':MACRO-ERROR (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
       (FUNCALL STANDARD-INPUT ':MACRO-ERROR))
  (UNLESS (WINDOW-MARK-P *WINDOW*)
    (BEEP))
  (AND *NUMERIC-ARG-P*
       (SETQ *MARK-STAYS* T))
  DIS-NONE)

))

; From file COME.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COME  "

(DEFCOM COM-ABORT-AT-TOP-LEVEL
	"Abort a command that is unfinished or reading arguments.
Aborts minibuffers and recursive edits, and things like C-X M and DIRED.

Actually, this particular definition is the one used at top level only,
and it does nothing except abort any keyboard macro being defined.
When you are actually typing the arguments to a command,
the Abort key has a different definition."
	()
  (IF (WINDOW-MARK-P *WINDOW*)
      (SETQ *MARK-STAYS* NIL)
    (BARF (IF (MEMQ (SEND STANDARD-INPUT ':SEND-IF-HANDLES ':MACRO-LEVEL)
		    '(0 NIL))
	      "Already at top level."
	    "Aborting definition of keyboard macro.")))
  DIS-NONE)

))

; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFCONST KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS
	  '((#/TERMINAL KBD-ESC)
	    (#/SYSTEM KBD-SYS)
	    (#/CONTROL-CLEAR-INPUT KBD-ESC-CLEAR))
  "Default alist of keys handled like Terminal and System.")

))

; From file BASSTR.LISP PS:<MLY.L> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-ESC (&REST IGNORE &AUX CH ARG MINUS FCN ENT TEM)
  "Handle Terminal typed on keyboard"
  (SAVE-IDLE-INFO) ;;but something was probably already clobbered, sigh
  (LET-GLOBALLY ((WHO-LINE-PROCESS CURRENT-PROCESS))
    (WHO-LINE-RUN-STATE-UPDATE)			;Necessary to make above take effect
    (DO-FOREVER
      (SETQ CH (CHAR-UPCASE (KBD-GET-SOFTWARE-CHAR "Terminal-")))
      (COND (( #/0 CH #/9)
	     (SETQ ARG (+ (* (OR ARG 0) 10.) (- CH #/0))))
	    ((= CH #/-) (SETQ MINUS T))
	    (T (RETURN)))))
  (WHO-LINE-RUN-STATE-UPDATE)	;Switch LAST-WHO-LINE-PROCESS back
  (AND MINUS (SETQ ARG (MINUS (OR ARG 1))))
  (COND ((SETQ ENT (ASSQ CH *ESCAPE-KEYS*))
	 (WITHOUT-INTERRUPTS
	   (COND ((MEMQ ':TYPEAHEAD (CDDDR ENT))
		  (KBD-GET-IO-BUFFER)
		  (KBD-SNARF-INPUT SELECTED-IO-BUFFER T)
		  (SETQ KBD-ESC-TIME (TIME)))))
	 (SETQ FCN (SECOND ENT))
	 (AND (CONSP FCN) (SETQ ARG FCN FCN #'EVAL))
	 (COND ((MEMQ ':KEYBOARD-PROCESS (CDDDR ENT))
		(FUNCALL FCN ARG)
		(SETQ KBD-ESC-TIME NIL))
	       (T (PROCESS-RUN-FUNCTION (FORMAT NIL "Handle Terminal-~:C" CH)
					#'(LAMBDA (FCN ARG)
					    (LET ((KBD-LAST-ACTIVITY-TIME
						    KBD-LAST-ACTIVITY-TIME))
					      (FUNCALL FCN ARG))
					    (SETQ KBD-ESC-TIME NIL)
					    (RESTORE-IDLE-INFO))
					FCN ARG))))
	;; quote asynchronous characters
	((OR (AND (SETQ TEM (KBD-GET-IO-BUFFER))
		  (ASSQ CH (GET (LOCF (IO-BUFFER-PLIST TEM))
				':ASYNCHRONOUS-CHARACTERS
				KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)))
	     (ASSQ CH KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS))
	 (UNLESS (IO-BUFFER-FULL-P KBD-IO-BUFFER)
	   (IO-BUFFER-PUT KBD-IO-BUFFER CH)))
  	;((SETQ TEM (ASSQ (DPB 1 %%KBD-CONTROL (LDB %%KBD-CHAR CH))
	;           KBD-STANDARD-ASYNCHRONOUS-CHARACTERS))
	; (APPLY (CADR TEM) (CAR TEM) TV:SELECTED-WINDOW (CDDR TEM)))
	((NEQ CH #/RUBOUT) (BEEP)))
  ;; No unwind-protect needed -- this is for good measure
  (RESTORE-IDLE-INFO))

))

; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-ESC-CLEAR (TEM &OPTIONAL IGNORE)	;terminal clear-input / c-clear-input
  (AND (SETQ TEM (KBD-GET-IO-BUFFER))
       (IO-BUFFER-CLEAR TEM))
  (IO-BUFFER-CLEAR KBD-IO-BUFFER))

))

; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-ESC-HELP (IGNORE &AUX DOC (INDENT 15.))
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
    (SEND WINDOW ':SET-LABEL "Keyboard documentation")
    (WINDOW-MOUSE-CALL (WINDOW :DEACTIVATE)
      (SETQ KBD-ESC-TIME NIL)
      (FORMAT WINDOW "~25TType ~:C followed by:

~:C~VTDo nothing. (Use this if you typed ~:C by accident and want to cancel it.)
0-9, -~VTNumeric argument to following command~%"
	      #/TERMINAL #/RUBOUT INDENT #/TERMINAL INDENT)
      (DOLIST (X *ESCAPE-KEYS*)
	(COND ((NULL (CAR X))
	       (SETQ INDENT 20.)
	       (FORMAT WINDOW "~%~5@TThese are for wizards:~2%"))
	      ((SETQ DOC (EVAL (CADDR X)))
	       (FORMAT WINDOW "~:C" (CAR X))
	       (PRINT-STRING-WITH-INDENTATION
		 WINDOW
		 (IF (ATOM DOC) DOC (CAR DOC))
		 INDENT)
	       (OR (ATOM DOC) (DOLIST (LINE (CDR DOC))
				(PRINT-STRING-WITH-INDENTATION WINDOW LINE INDENT))))))
      (FORMAT WINDOW "~3%~25TKeyboard function keys:

Abort		Throw to command level		Break		Get read-eval-print loop
Control-Abort	To command level immediately	Control-Break	BREAK immediately
Meta-Abort	Throw out of all levels		Meta-Break	Get to error-handler
C-M-Abort	Out of all levels immediately	C-M-Break	Error-handler immediately
Macro		Keyboard macros (in Zmacs)	Stop-Output	(not used)
Terminal	The above commands		Resume		Continue from break//error
System		Select a Program		Call		(not used)
Network		Supdup//Telnet commands		Status		Print Input History
Quote		(not used)			C-Status	Print Kill History
Overstrike	/"backspace/"			Delete		(not used)
Clear-Input	Forget type-in			End		Terminate input
Clear-Screen	Refresh screen			Help		Print documentation
Hold-Output	(not used)			Return		Carriage return
						Line		Next line and indent (Zmacs)
")
      (FORMAT WINDOW "~%Type a space to flush: ")
      (SEND WINDOW ':TYI))))

))

; patched by kab
; From file MATRIX.LISP PS:<L.SYS2> OZ:
#8R MATH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "MATH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MATRIX  "

(DEFUN MULTIPLY-MATRICES (MATRIX-1 MATRIX-2 &OPTIONAL MATRIX-3 &AUX SAVED-MATRIX-3)
  "Multiply matrices MATRIX-1 and MATRIX-2, storing into MATRIX-3 if supplied.
If MATRIX-3 is not supplied, then a new (ART-Q type) array is returned, else
MATRIX-3 must have exactly the right dimensions for holding the result of the multiplication.
Both MATRIX-1 and MATRIX-2 must be either one- or two-diimensional.
The first dimension of MATRIX-2 must equal the second dimension of MATRIX-1, unless MATRIX-1
is one-dimensional, when the first dimensions must match (thus allowing multiplications of the
form VECTOR x MATRIX)"
  (CHECK-ARG MATRIX-1 1-OR-2D-ARRAYP "A one- or two-dimensional array")
  (CHECK-ARG MATRIX-2 1-OR-2D-ARRAYP "A one- or two-dimensional array")
  (CHECK-ARG MATRIX-3 (OR (NULL MATRIX-3) (1-OR-2D-ARRAYP MATRIX-3))
	     "A one- or two-dimensional array or NIL")
  (LET ((DIM-1-1 (IF (= (ARRAY-RANK MATRIX-1) 1) 1 (ARRAY-DIMENSION MATRIX-1 0)))
	(DIM-1-2 (IF (= (ARRAY-RANK MATRIX-1) 1) (ARRAY-DIMENSION MATRIX-1 0)
		   (ARRAY-DIMENSION MATRIX-1 1)))
	(DIM-2-1 (ARRAY-DIMENSION MATRIX-2 0))
	(DIM-2-2 (IF (= (ARRAY-RANK MATRIX-2) 1) 1 (ARRAY-DIMENSION MATRIX-2 1)))
	(DIM-3-1 (WHEN MATRIX-3
		   (IF (= (ARRAY-RANK MATRIX-3) 1) 1 (ARRAY-DIMENSION MATRIX-3 0))))
	(DIM-3-2 (WHEN MATRIX-3
		   (IF (= (ARRAY-RANK MATRIX-3) 1) (ARRAY-DIMENSION MATRIX-3 0)
		     (ARRAY-DIMENSION MATRIX-3 1)))))
    (UNLESS (= DIM-2-1 DIM-1-2)
      (FERROR NIL "The ~~Dx~D matrix ~S and the
~Dx~D matrix ~S cannot be multiplied~"
	      DIM-1-1 DIM-1-2 MATRIX-1 DIM-2-1 DIM-2-2 MATRIX-2))
    (IF MATRIX-3
	(IF (AND (= DIM-1-1 DIM-3-1)
		 (= DIM-2-2 DIM-3-2))
	    ;; We have a destination; see if it's the same as one of the sources,
	    ;; If it is, substitute a temporary for the destination.  We only check
	    ;; EQness, not displacements.
	    (WHEN (LET ((FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-3)))
		    (OR (EQ FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-1))
			(EQ FORWARDED (FOLLOW-STRUCTURE-FORWARDING MATRIX-2))))
	      (SETQ SAVED-MATRIX-3 MATRIX-3
		    MATRIX-3 (MAKE-ARRAY (ARRAY-DIMENSIONS MATRIX-3)
					 ':TYPE (ARRAY-TYPE MATRIX-3))))
	  (FERROR NIL "The ~~Dx~D matrix ~S is not the right size for multiplying the
~Dx~D matrix ~S and the
~Dx~D matrix ~S.~"
		  DIM-3-1 DIM-3-2 MATRIX-3 DIM-1-1 DIM-1-2 MATRIX-1
		  DIM-2-1 DIM-2-2 MATRIX-2))
      ;; we don't make a 1xn matrix here since the user probably wants a vector result.
      (SETQ MATRIX-3 (MAKE-ARRAY (IF (= (ARRAY-RANK MATRIX-1) 1) DIM-2-2 (LIST DIM-1-1 DIM-2-2)))))
    ;; Make indirect arrays to any vectors, so can use ar-2 everywhere below
    (LET ((MAT-1 (IF (= 2 (ARRAY-RANK MATRIX-1)) MATRIX-1
		   (MAKE-ARRAY (LIST DIM-1-1 DIM-2-1) ':TYPE (ARRAY-TYPE MATRIX-1)
			       ':DISPLACED-TO MATRIX-1)))
	  (MAT-2 (IF (= 2 (ARRAY-RANK MATRIX-2)) MATRIX-2
		   (MAKE-ARRAY (LIST DIM-2-1 DIM-2-2) ':TYPE (ARRAY-TYPE MATRIX-2)
			       ':DISPLACED-TO MATRIX-2)))
	  (MAT-3 (IF (= 2 (ARRAY-RANK MATRIX-3)) MATRIX-3
		   (MAKE-ARRAY (LIST DIM-1-1 DIM-2-2) ':TYPE (ARRAY-TYPE MATRIX-3)
			       ':DISPLACED-TO MATRIX-3))))
      ;; Do the actual multiplication
      (DOTIMES (I DIM-1-1)
	(DOTIMES (J DIM-2-2)
	  (SETF (AREF MAT-3 I J)
		(DO ((K 0 (1+ K))
		     (SUM 0 (+ SUM (* (AREF MAT-1 I K) (AREF MAT-2 K J)))))
		    (( K DIM-2-1) SUM)))))
      ;; Try to get rid of any temporary arrays we made
      (WHEN (NEQ MATRIX-3 MAT-3) (RETURN-ARRAY MAT-3))
      (WHEN (NEQ MATRIX-2 MAT-2) (RETURN-ARRAY MAT-2))
      (WHEN (NEQ MATRIX-1 MAT-1) (RETURN-ARRAY MAT-1))
      ;; if we substituted a temporary above, copy the result into the saved
      ;; destination and return the temporary to free storage.
      (WHEN SAVED-MATRIX-3
	(COPY-ARRAY-CONTENTS MATRIX-3 SAVED-MATRIX-3)
	(RETURN-ARRAY MATRIX-3)
	(SETQ MATRIX-3 SAVED-MATRIX-3))))
  MATRIX-3)

))

; From file SERIAL.LISP PS:<L.IO1> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SERIAL  "

(DEFMETHOD (SERIAL-STREAM-MIXIN :CLOSE) (&OPTIONAL ABORT-P)
  (IF (AND (NOT ABORT-P) (VARIABLE-BOUNDP OUTPUT-UNIBUS-CHANNEL) OUTPUT-UNIBUS-CHANNEL)
      (SEND SELF ':FINISH))
  (%UNIBUS-WRITE 764112 (DPB 0 0701 (%UNIBUS-READ 764112)))	;Turn off interrupt
  (IF (VARIABLE-BOUNDP INPUT-UNIBUS-CHANNEL)
      (RETURN-UNIBUS-CHANNEL (PROG1 INPUT-UNIBUS-CHANNEL
				    (VARIABLE-MAKUNBOUND INPUT-UNIBUS-CHANNEL))))
  (IF (VARIABLE-BOUNDP OUTPUT-UNIBUS-CHANNEL)
      (RETURN-UNIBUS-CHANNEL (PROG1 OUTPUT-UNIBUS-CHANNEL
				    (VARIABLE-MAKUNBOUND OUTPUT-UNIBUS-CHANNEL))))
  (IF (VARIABLE-BOUNDP RANDOM-UNIBUS-CHANNEL)
      (RETURN-UNIBUS-CHANNEL (PROG1 RANDOM-UNIBUS-CHANNEL
				    (VARIABLE-MAKUNBOUND RANDOM-UNIBUS-CHANNEL)))))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN VECTOR-POP (ARRAY)
  "Returns the last used element of ARRAY, and decrements the fill pointer.
For an ART-Q-LIST array, the cdr codes are updated so that the overlayed list
no longer contains the element removed. Signals an error if ARRAY is empty
/(has fill-pointer 0)" 
  (LET* ((INHIBIT-SCHEDULING-FLAG T)
	 (INDEX (1- (FILL-POINTER ARRAY)))	;1- because fill-pointer is # active elements
	 (ARRAY-TYPE (AREF #'ARRAY-TYPES (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0)))
	 VAL)
    (WHEN (MINUSP INDEX)
      (FERROR NIL "~S Overpopped" ARRAY))
    (SETQ VAL (AREF ARRAY INDEX))
    (SETF (FILL-POINTER ARRAY) INDEX)
    (WHEN (AND (EQ ARRAY-TYPE 'ART-Q-LIST)
	       (NOT (ZEROP INDEX)))
      ;(SETF (AREF ARRAY INDEX) NIL)		;Flush so not there for GC (ha ha!)
      (%P-DPB CDR-NIL %%Q-CDR-CODE (ALOC ARRAY (1- INDEX))))
    VAL))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFF ARRAY-POP 'VECTOR-POP)

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-CHARACTER (CHARACTER)
  (SETQ CHARACTER (CLI:CHARACTER CHARACTER))
  (FORMAT T "~&~S is a character with code ~D.
Its control-bits are ~D~:[~4*~; (~@[Control~*~]~@[Meta~*~]~@[Super~*~]~@[Hyper~*~])~]. Its font is ~D"
	  CHARACTER (CHAR-CODE CHARACTER) (CHAR-BITS CHARACTER) ( 0 (CHAR-BITS CHARACTER))
	  (CHAR-BIT CHARACTER ':CONTROL) (CHAR-BIT CHARACTER ':META)
	  (CHAR-BIT CHARACTER ':SUPER) (CHAR-BIT CHARACTER ':HYPER)
	  (CHAR-FONT CHARACTER))
  CHARACTER)

))

; From file FRAME.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FRAME  "

(DEFUN CONSTRAINT-FRAME-RECOMPUTE-CONFIGURATION ()
  (DECLARE (:SELF-FLAVOR BASIC-CONSTRAINT-FRAME))
  (PRESERVE-SUBSTITUTE-STATUS SELF
    (LET-GLOBALLY ((RECURSION T))
      (SEND SELF ':DEEXPOSE ':DEFAULT ':NOOP)
      (DOLIST (P EXPOSED-PANES)
	(FUNCALL P ':DEEXPOSE ':DEFAULT ':NOOP)
	;; This assures that old panes won't gratuitously reappear if there is
	;; blank space in the new configuration where a pane used to be.  Note that
	;; this is careful not to touch inferiors that aren't "panes" of this
	;; constraint frame.
	(FUNCALL P ':DEACTIVATE))
      (OR (CONSTRAINT-FRAME-DO-CONSTRAINTS SELF INTERNAL-CONSTRAINTS
					   (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT))
	  (FERROR NIL "Some constraint could not be satisfied"))
      (CONSTRAINT-FRAME-SET-EDGES INTERNAL-CONSTRAINTS NIL)
      (DOLIST (P EXPOSED-PANES) (FUNCALL P ':EXPOSE))
      (SETQ BLANK-RECTANGLES NIL)
      (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	(CONSTRAINT-FRAME-DRAW-BLANK-SPACE))
      (OR (MEMQ SELECTION-SUBSTITUTE EXPOSED-INFERIORS)
	  (SETQ SELECTION-SUBSTITUTE NIL)))))

))

; From file COMD.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFUN EDIT-IN-MINI-BUFFER (&OPTIONAL (COMTAB *MINI-BUFFER-COMTAB*)
				      INITIAL-CONTENTS INITIAL-CHAR-POS MODE-LINE-LIST)
  "Read input using a mini buffer, and return a string.
COMTAB is the comtab to use while in the mini buffer; the default is usually right.
INITIAL-CONTENTS is a string to initialize the mini buffer from
and INITIAL-CHAR-POS is where in that string to start the cursor.
MODE-LINE-LIST is what to display in the mode line during."
  (AND *MINI-BUFFER-COMMAND-IN-PROGRESS*	;Recursive mini-buffers don't work
       (BARF "Mini-buffer entered recursively"))
;Removed because it can go off if a repeated command uses more
;minibuffers that were recorded, for any reason,
;and then you can no longer keep rotating the minibuffer ring with C-M-Y
;after you get back to the command's first minibuffer.
;  ;; Prevent C-M-Y from being confused if we repeated a command
;  ;; previously but are not repeating one now.
;  (OR *MINI-BUFFER-REPEATED-COMMAND*
;      (SETQ *MINI-BUFFER-ENTIRE-REPEATED-COMMAND* NIL))
  (AND *MINI-BUFFER-REPEATED-COMMAND*
       (POP *MINI-BUFFER-REPEATED-COMMAND* INITIAL-CONTENTS)
       (SETQ INITIAL-CHAR-POS (STRING-LENGTH INITIAL-CONTENTS)))
  (MUST-REDISPLAY *MINI-BUFFER-WINDOW* DIS-TEXT)
  ;; Set up the initial contents of the mini buffer, and discard previous undo records.
  (LET ((*INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
	(*BATCH-UNDO-SAVE* T)
	(BP (WINDOW-POINT *MINI-BUFFER-WINDOW*)))
    (DISCARD-UNDO-INFORMATION *INTERVAL*)
    (DELETE-INTERVAL *INTERVAL*)
    (AND INITIAL-CONTENTS
	 (INSERT BP (SETQ INITIAL-CONTENTS (STRING INITIAL-CONTENTS))))
    (AND INITIAL-CHAR-POS
	 (MOVE-BP BP (FORWARD-CHAR BP INITIAL-CHAR-POS))))
  (OR *MINI-BUFFER-DONT-RECORD* (RECORD-MINI-BUFFER-VALUE T))
  (PROG* KLUDGE (VAL SUCCESSFUL
		     (TOP-W (SEND (TV:SHEET-SUPERIOR *MINI-BUFFER-WINDOW*)
				  ':TOP-OF-EDITOR-HIERARCHY))
		     (OSUBST (SEND TOP-W ':SELECTION-SUBSTITUTE)))
    ;; Prevent a delay when mini buffer window is selected
    (SEND (WINDOW-SHEET *MINI-BUFFER-WINDOW*) ':MINI-BUFFER-ENTERED)
    (UNWIND-PROTECT
      (BIND-MODE-LINE MODE-LINE-LIST
	(LET ((*MINI-BUFFER-COMMAND-IN-PROGRESS* *CURRENT-COMMAND*)
	      (PACKAGE PACKAGE)
	      (*COMTAB* COMTAB)
	      (*OUTER-LEVEL-MINI-BUFFER-COMMAND* *MINI-BUFFER-COMMAND*)
	      (INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
	  ;; Actually do the editing.
	  (SETQ VAL (SEND *MINI-BUFFER-WINDOW* ':EDIT))
	  (SETQ *MINI-BUFFER-COMMAND* *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
	  (OR *MINI-BUFFER-DONT-RECORD*
	      (RECORD-MINI-BUFFER-VALUE NIL (STRING-INTERVAL INTERVAL)))
	  ;; If we are repeating a command, re-input any non-mini-buffer
	  ;; characters that followed this mini-buffer.
	  (DOLIST (CHAR (REVERSE (CAR *MINI-BUFFER-REPEATED-COMMAND*)))
	    (SEND TERMINAL-IO ':PUSH-INPUT CHAR))
	  (POP *MINI-BUFFER-REPEATED-COMMAND*)
	  (SETQ SUCCESSFUL T)
	  (RETURN-FROM KLUDGE VAL *MINI-BUFFER-WINDOW* INTERVAL)))
;If we quit out of a minibuffer, tell (:method editor :edit)
;not to push this command on the ring.
;Likewise if this is being run from a mouse click, since then
;*CURRENT-COMMAND* is NIL and we could not reexecute this properly.
      (OR (AND *CURRENT-COMMAND* SUCCESSFUL) (SETQ *MINI-BUFFER-COMMAND* NIL))
      ;; Don't reselect a typeout window that has been popped down.
      (IF (AND OSUBST (NOT (MEMQ OSUBST (TV:SHEET-INFERIORS (TV:SHEET-SUPERIOR OSUBST)))))
	  (SETQ OSUBST (TV:SHEET-SUPERIOR OSUBST)))
      (SEND TOP-W ':SET-SELECTION-SUBSTITUTE OSUBST)
      (UNLESS (EQ *WINDOW* *MINI-BUFFER-WINDOW*)
	(DISAPPEAR-MINI-BUFFER-WINDOW)))))

))


; From file FORMAT.LISP PS:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(defun format-ctl-start-indent-convert (args params)
  (let ((clauses (format-parse-clauses ' nil))
	(indent-convert (or (car params)
			    (send *standard-output* ':send-if-handles ':read-cursorpos
				  				      ':character)
			    0))
	(indent-converted-stream
	  (if indent-convert indent-converted-stream *standard-output*))
	(*standard-output* 'indent-convert-stream))
    (unwind-protect
	(format-ctl-string args (aref clauses 0))
      (format-reclaim-clauses clauses))))
))
