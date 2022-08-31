;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 11/23/83 02:37:30 by rms,
;;; Reason: Initial fixes.
;;; Reason: initial fixes
;;; while running on Lisp Machine Eighteen from band 7
;;; with Experimental System 98.0, CADR 3.0, Experimental ZMail 53.0, MIT-Specific 22.0, microcode 301, ZM MIT.



; From file CLASS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CLASS  "

(DEFUN COPY-CLOSURE (CLOSURE &AUX CLOSURE1)
  "Return a new closure with the same function, variables and initial values as CLOSURE.
However, the new and old closures do not share the same external value cells."
  (CHECK-ARG CLOSURE (OR (ENTITYP CLOSURE) (CLOSUREP CLOSURE)) "an entity or a closure")
  (SETQ CLOSURE1 (%MAKE-POINTER DTP-LIST CLOSURE))
  (IF (AND (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
	   (NULL (CDDR (%MAKE-POINTER DTP-LIST CLOSURE))))
      (%MAKE-POINTER DTP-CLOSURE (COPYLIST CLOSURE1))
    (LET ((ANS (MAKE-LIST DEFAULT-CONS-AREA (LENGTH CLOSURE1))))
      (RPLACA ANS (CAR CLOSURE1))		;CLOSE OVER SAME FCTN
      (DO ((L (CDR CLOSURE1) (CDDR L))
	   (N (CDR ANS) (CDDR N)))
	  ((NULL L) (%MAKE-POINTER (%DATA-TYPE CLOSURE) ANS))
	(RPLACA N (CAR L))			;SAME INTERNAL VALUE CELL
	(LET ((NEW-EXVC (MAKE-LIST DEFAULT-CONS-AREA 1)))
	  (IF (NOT (LOCATION-BOUNDP (CADR L)))
	      (LOCATION-MAKUNBOUND NEW-EXVC)
	    (RPLACA NEW-EXVC (CAR (CADR L))))
	  (RPLACA (CDR N) NEW-EXVC))))))

))

; From file CLASS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CLASS  "

(DEFUN CLOSURE-ALIST (CLOSURE)
  "Return an alist of variables closed over by CLOSURE vs their values they have inside it.
If one of the variables is unbound in the closure,
the corresponding cdr in the alist will also be a DTP-NULL.
Storing into the alist cdr's does not affect the values in the closure."
  (CHECK-ARG CLOSURE (OR (ENTITYP CLOSURE) (CLOSUREP CLOSURE)) "an entity or a closure")
  (IF (AND (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
	   (NULL (CDDR (%MAKE-POINTER DTP-LIST CLOSURE))))
      (LIST (CONS 'LEXICAL-ENVIRONMENT
		  (CADR (%MAKE-POINTER DTP-LIST CLOSURE))))
    (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L))
	 (ANS))
	((NULL L) ANS)
      (SETQ ANS (CONS (CONS (%MAKE-POINTER-OFFSET DTP-SYMBOL (CAR L) -1)
			    NIL)
		      ANS))     ;; Copy (CAADR L) into (CDAR ANS).
      (%BLT-TYPED (CADR L) (CDR-LOCATION-FORCE (CAR ANS)) 1 1) )))

))

; From file CLASS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CLASS  "

;Here are some random functions for poking around in ENTITYs.

(DEFUN CLOSURE-VARIABLES (CLOSURE)
  "Return a list of variables closed over by CLOSURE."
  (CHECK-ARG CLOSURE (OR (ENTITYP CLOSURE) (CLOSUREP CLOSURE)) "an entity or a closure")
  (IF (AND (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
	   (NULL (CDDR (%MAKE-POINTER DTP-LIST CLOSURE))))
      '(LEXICAL-ENVIRONMENT)
    (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L))
	 (ANS NIL (CONS (%MAKE-POINTER-OFFSET DTP-SYMBOL (CAR L) -1) ANS)))
	((NULL L) ANS))))

))

; From file FILES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN DIRECTORY-INPUT-STREAM (DIRECTORY &AUX (DIR DIRECTORY))
  "Return a stream that reads a directory listing of pathname DIRECTORY.
Th stream supports only the :LINE-IN operation."
  (LET-CLOSED ((DIRECTORY DIR)
	       (DIRECTORY-LIST-STREAM NIL)
	       (REREAD-ENTRY))
    (COND ((ERRORP (SETQ DIRECTORY-LIST-STREAM
			 (FS:DIRECTORY-LIST-STREAM DIRECTORY)))
	   (BARF "Error: ~A"  DIRECTORY-LIST-STREAM)))
    (SETQ REREAD-ENTRY (FUNCALL DIRECTORY-LIST-STREAM ':ENTRY))
    (AND REREAD-ENTRY (NULL (CAR REREAD-ENTRY))
	 (SETQ DIRECTORY (GET REREAD-ENTRY ':PATHNAME)))
    #'DIRECTORY-INPUT-STREAM-IO))

))

; From file BASSTR.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(add-system-key #/F 'FED:FED-FRAME "Font Edit")

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (FILE-DATA-STREAM-MIXIN :RENAME) (NEW-NAME &OPTIONAL (ERROR-P T)
					     &AUX SUCCESS STRING)
  (FILE-OPERATION-RETRY
    (SELECTQ STATUS
      ((:OPEN :EOF :SYNC-MARKED :ASYNC-MARKED)
       (MULTIPLE-VALUE (STRING SUCCESS)
	 (FUNCALL-SELF ':COMMAND NIL "RENAME" #\CR (FILE-PRINT-PATHNAME NEW-NAME) #\CR))
       (COND (SUCCESS
	      ;; If there is a second line coming from the file server,
	      ;; it is the new truename.
	      (LET* ((FROM (STRING-SEARCH #\RETURN STRING))
		     TRUENAME-STRING)
		(COND (FROM
		       (SETQ TRUENAME-STRING
			     (SUBSTRING STRING (1+ FROM)
					(STRING-SEARCH #\RETURN STRING (1+ FROM))))
		       (FUNCALL-SELF ':PUTPROP
				     (PARSE-PATHNAME TRUENAME-STRING
						     (SEND (GET SELF ':TRUENAME) ':HOST))
				     ':TRUENAME))))
	      (SETQ SI:PATHNAME NEW-NAME)
	      (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':CLOBBERED)
	      T)
	     (T (FILE-PROCESS-ERROR-NEW STRING SELF NIL (NOT ERROR-P) ':RENAME))))
      (OTHERWISE (FERROR NIL "~S in illegal state for rename." SELF)))))

))

(load "sys:fonts;5x5")

; From file ZMNEW.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "

(DEFUN CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES (PLIST BUFFER)
  (WHEN (AND *UPDATE-PLIST-ON-WRITE-OK*
	     (EQ ':LISP (GET (LOCF PLIST) ':MODE))
	     (BUFFER-PATHNAME BUFFER)
	     (NULL (GET (LOCF PLIST) ':BASE))
	     ;; Don't mess with init files, since they may SETQ BASE.
	     (NEQ (SEND (BUFFER-PATHNAME BUFFER) ':CANONICAL-TYPE) ':INIT))
    ;; insert a base attribute.
    (STORE-ATTRIBUTE-LIST BUFFER (SETQ PLIST (APPEND `(:BASE ,BASE) PLIST)))
    (FORMAT QUERY-IO "~&Updating Base Attribute of File to ~D.~%" BASE)
    T))

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN WRITE-FILE-INTERNAL (PATHNAME &OPTIONAL (BUFFER *INTERVAL*))
  "Save BUFFER in file PATHNAME and mark it as visiting that file."
  ;; Examine the buffer's current mode line.
  ;; If the user has edited in a Fonts: property,
  ;; save the font information, even if he has failed to do
  ;; Reparse Mode Line.
  (LET ((PLIST (FS:FILE-EXTRACT-PROPERTY-LIST (INTERVAL-STREAM BUFFER))))
    (WHEN (CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES PLIST BUFFER)
      (MUST-REDISPLAY-BUFFER BUFFER DIS-TEXT))
    (WITH-OPEN-FILE-RETRY (STREAM (PATHNAME FS:FILE-ERROR) :DIRECTION :OUTPUT)
      (STREAM-OUT-INTERVAL STREAM BUFFER NIL T
			   (OR (GETL (LOCF PLIST) '(:FONTS :DIAGRAM))
			       (SEND BUFFER ':GET-ATTRIBUTE ':FONTS)
			       (SEND BUFFER ':GET-ATTRIBUTE ':DIAGRAM)))
      (CLOSE STREAM)
      (SET-BUFFER-PATHNAME PATHNAME BUFFER)
      (SET-BUFFER-FILE-ID BUFFER (FUNCALL STREAM ':INFO))
      (SETF (BUFFER-TICK BUFFER) (TICK))
      (WHEN *DISCARD-UNDO-INFO-ON-SAVING*
	(DISCARD-UNDO-INFORMATION BUFFER))
      (PRINT-FILE-WRITTEN STREAM))))

))

; From file STREAM.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; STREAM  "

(DEFMETHOD (STREAM-MIXIN :RUBOUT-HANDLER) (RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (COND ((> (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
	 (COPY-ARRAY-PORTION RUBOUT-HANDLER-BUFFER (RHB-SCAN-POINTER) (RHB-FILL-POINTER)
			     RUBOUT-HANDLER-BUFFER 0 (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER))
	 (IF (NUMBERP (RHB-TYPEIN-POINTER))
	     (DECF (RHB-TYPEIN-POINTER) (RHB-SCAN-POINTER)))
	 (SETF (RHB-FILL-POINTER) (- (RHB-FILL-POINTER) (RHB-SCAN-POINTER))))
	(T (SETF (RHB-FILL-POINTER) 0)))
  (SETF (RHB-SCAN-POINTER) 0)
  (SETF (RHB-INITIAL-ENTRY) T)
  (MULTIPLE-VALUE-BIND (PROMPT-STARTING-X PROMPT-STARTING-Y)
      (FUNCALL-SELF ':READ-CURSORPOS)
    (LET ((PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS)))
      (AND PROMPT-OPTION			;Prompt if desired
	   (RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL)))
    (MULTIPLE-VALUE-BIND (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	(FUNCALL-SELF ':READ-CURSORPOS)
      ;; Output any "typeahead"
      (AND (PLUSP (RHB-FILL-POINTER))
	   (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
      (DO ((RUBOUT-HANDLER T)			;Establish rubout handler
	   (RUBOUT-HANDLER-INSIDE T)
	   (RUBOUT-HANDLER-RE-ECHO-FLAG NIL NIL))
	  (NIL)
	(*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
	  (CONDITION-CASE (ERROR)
	      (RETURN (APPLY FUNCTION ARGS))	;Call read type function
	    (SYS:READ-ERROR
	     (TERPRI SELF)
	     (PRINC ">>ERROR: " SELF)
	     (SEND ERROR ':REPORT SELF)
	     (TERPRI SELF)
	     (SETQ RUBOUT-HANDLER-RE-ECHO-FLAG T)
	     (DO () (NIL) (FUNCALL-SELF ':TYI)))))	;If error, force user to rub out
	;;Maybe return when user rubs all the way back
	(AND (ZEROP (RHB-FILL-POINTER))
	     (LET ((FULL-RUBOUT-OPTION (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS)))
	       (WHEN FULL-RUBOUT-OPTION
		 ;; Get rid of the prompt, if any.
		 (SHEET-CLEAR-BETWEEN-CURSORPOSES
		   SELF PROMPT-STARTING-X PROMPT-STARTING-Y
		   (- CURSOR-X LEFT-MARGIN-SIZE) (- CURSOR-Y TOP-MARGIN-SIZE))
		 (SHEET-SET-CURSORPOS SELF PROMPT-STARTING-X PROMPT-STARTING-Y)
		 (RETURN NIL (CADR FULL-RUBOUT-OPTION)))))))))

))

; From file TYPWIN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TYPWIN  "

;;;Mouse-left selects the blinking item, mouse-right pops up a menu near it
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-CLICK) (BUTTON X Y &AUX ITEM)
  (SETQ ITEM (FUNCALL-SELF ':MOUSE-SENSITIVE-ITEM X Y))
  (OR (WHEN ITEM
	(LET ((ITEM-TYPE (ASSQ (TYPEOUT-ITEM-TYPE ITEM) ITEM-TYPE-ALIST)))
	  (WHEN ITEM-TYPE
	    (SELECTQ BUTTON
	      (#\MOUSE-1-1
	       (FUNCALL-SELF ':FORCE-KBD-INPUT
			     (LIST ':TYPEOUT-EXECUTE (CADR ITEM-TYPE)
				   (TYPEOUT-ITEM-ITEM ITEM)))
	       T)
	      (#\MOUSE-3-1
	       (PROCESS-RUN-FUNCTION "Menu Choose" #'TYPEOUT-MENU-CHOOSE
				     MENU (CDDDR ITEM-TYPE) ITEM SELF
				     ;; Compute a label for the menu.
				     (OR (AND (CONSP (THIRD ITEM-TYPE))
					      (CADR (THIRD ITEM-TYPE))
					      (FUNCALL (CADR (THIRD ITEM-TYPE))
						       ITEM))
					 (AND (TYPEP (SECOND ITEM) ':INSTANCE)
					      (OR (SEND (SECOND ITEM) ':SEND-IF-HANDLES
							':STRING-FOR-PRINTING)
						  (SEND (SECOND ITEM) ':SEND-IF-HANDLES
							':NAME)))))
	       T)))))
      ;; Return T unless this is double-right, to inhibit the blip made by default.
      (NEQ BUTTON #\MOUSE-R-2)))

))

; From file SELEV.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SELEV  "


(DEFMACRO SELECT-MATCH (OBJECT . CLAUSES)
  ;; We want ARGLIST to say we have a BODY,
  ;; but don't set up &BODY indentation, because we use the hairier SELECTQ identation.
  (DECLARE (ARGLIST OBJECT &BODY CLAUSES))
  "Execute the first clause whose pattern matches the value of OBJECT.
The syntax is 

   (SELECT-MATCH OBJECT
     (`PATTERN CONDITION CLAUSE-BODY...)
     (`PATTERN CONDITION CLAUSE-BODY...)
     ...
     (`PATTERN CONDITION CLAUSE-BODY...)
     (OTHERWISE CLAUSE-BODY...)

The value of OBJECT is matched against the PATTERNs one at a time until a
match succeeds and the accompanying CONDITION evaluates to non-NIL.
Then the CLAUSE-BODY of that clause is executed and its last expression's
value is returned.

,VARIABLE can appear in a pattern; it matches anything, and the variable
is bound to what it matched for the execution of the CONDITION and CLAUSE-BODY.
If one variable appears twice in a pattern, it must match EQUAL objects
in both occurrences:
    (SELECT-MATCH '(A B C) 
      ((,X B ,X) T 'LOSE)
      ((,X B ,Y) T 'WIN)
      (OTHERWISE 'LOSE-BIG))
returns WIN.  Use ,IGNORE to match anything and not use it."
  (LET* (BOUNDVARS
	 (GENVAR (GENSYM))
	 (CLAUSES (MAPCAR #'SELECT-MATCH-CLAUSE CLAUSES (CIRCULAR-LIST GENVAR))))
    (DECLARE (SPECIAL BOUNDVARS))
    `(LET ((,GENVAR ,OBJECT) . ,BOUNDVARS)
       (COND ,@CLAUSES))))

(DEFMACRO LIST-MATCH-P (LIST PATTERN)
  "T if the value of LIST matches PATTERN.  PATTERN is a backquote expression.
Constant parts of PATTERN are matched against the corresponsing parts of LIST.
Variables preceded by commas are SETQ'd to the corresponding parts of LIST.
If the same variable appears twice, it must match EQUAL objects both times.
Example: (LIST-MATCH-P '(FOO BAR BAR) `(FOO ,X ,X)) returns T and sets X to BAR."
  (LET (BOUNDVARS)
    (DECLARE (SPECIAL BOUNDVARS))
    (SELECT-MATCH-MATCHITEMS PATTERN LIST)))

;Return the COND clause corresponding to one SELECT-MATCH clause.
;Merges any pattern variables used into BOUNDVARS.
(DEFUN SELECT-MATCH-CLAUSE (CLAUSE OBJECTVAR)
  (IF (MEMQ (CAR CLAUSE) '(OTHERWISE :OTHERWISE))
      (CONS 'T (CDR CLAUSE))
    (LET ((PATCOND (SELECT-MATCH-MATCHITEMS (CAR CLAUSE) OBJECTVAR)))
      (CONS (CONS 'AND (IF (EQ (CADR CLAUSE) 'T)
			   (LIST PATCOND)
			 (LIST PATCOND (CADR CLAUSE))))
	    (CDDR CLAUSE)))))

;; MATCHCARCDR evals ARG, tests its car with CAR (a function of one arg)
;; and its cdr with CDR.  The COMPILER:P1 property below takes care
;; of open coding it with very fast code.
(DEFUN MATCHCARCDR (&QUOTE ARG CAR CDR)
  (LET ((ARGVAL (EVAL1 ARG)))
    (AND (CONSP ARGVAL)
	 (FUNCALL CAR (CAR ARGVAL))
	 (FUNCALL CDR (CDR ARGVAL)))))

(DEFUN (MATCHCARCDR COMPILER:P1) (FORM)
  (LET ((CAREXP (MATCHCARCDR-CONVERT-LAMBDA (CADDR FORM))))
    (COND ((EQ (CAR CAREXP) 'EQUAL)
	   `(AND (COMPILER:PUSH-CDR-IF-CAR-EQUAL ,(COMPILER:P1 (CADR FORM))
						 ,(COMPILER:P1 (CADDR CAREXP)))
		 ,(COMPILER:P1 (MATCHCARCDR-CONVERT-LAMBDA (CADDDR FORM)))))
	  ((AND (EQ (CAR CAREXP) 'PROGN)
		(EQ (CAR (CADR CAREXP)) 'SETQ))
	   (COMPILER:P1SETVAR (CADR (CADR CAREXP)))
	   `(AND (COMPILER:PUSH-CDR-STORE-CAR-IF-CONS ,(COMPILER:P1 (CADR FORM))
						      ,(COMPILER:P1 (CADR (CADR CAREXP))))
		 ,(COMPILER:P1 (MATCHCARCDR-CONVERT-LAMBDA (CADDDR FORM)))))
	  (T
	   `(AND (CONSP-OR-POP ,(COMPILER:P1 (CADR FORM)))
		 (PROGN (%PUSH (CARCDR (%POP)))
			(COND (,(COMPILER:P1 CAREXP)
			       ,(COMPILER:P1 (MATCHCARCDR-CONVERT-LAMBDA (CADDDR FORM))))
			      ('T (%POP) 'NIL))))))))

))

; From file SELEV.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SELEV  "

;; Note that MATCHCARCDR-CONVERT-LAMBDA knows exactly what kinds of
;; expressions this function can generate.
(DEFUN SELECT-MATCH-MATCHITEMS (PATT EXPR)
  (DECLARE (SPECIAL BOUNDVARS))
  (COND	((NULL PATT) `(NULL ,EXPR))
	((SYMBOLP PATT)
	 (COND ((EQ PATT 'IGNORE) T)
	       ((MEMBER PATT BOUNDVARS) `(EQUAL ,EXPR ,PATT))
	       (T (PUSH PATT BOUNDVARS)
		  `(PROGN (SETQ ,PATT ,EXPR) T))))
	((EQ (CAR PATT) 'XR-BQ-CONS)
	 `(MATCHCARCDR ,EXPR
		       (LAMBDA (OBJ) ,(SELECT-MATCH-MATCHITEMS (CADR PATT) 'OBJ))
		       (LAMBDA (OBJ) ,(SELECT-MATCH-MATCHITEMS (CADDR PATT) 'OBJ))))
	((EQ (CAR PATT) 'XR-BQ-LIST)
	 (LET ((EXP '(NULL OBJ))
	       (eltmatches (mapcar 'select-match-matchitems (cdr patt) (circular-list 'obj))))
	   (LOOP for eltmatch in (reverse eltmatches)
		 DO (SETQ EXP `(MATCHCARCDR OBJ
					    (LAMBDA (OBJ) ,eltmatch)
					    (LAMBDA (OBJ) ,EXP))))
	   `(MATCHCARCDR ,EXPR . ,(CDDR EXP))))
	((EQ (CAR PATT) 'XR-BQ-LIST*)
	 (LET ((EXP (SELECT-MATCH-MATCHITEMS (CAR (LAST PATT)) 'OBJ)))
	   (LOOP FOR ELT IN (CDR (REVERSE (CDR PATT)))
		 DO (SETQ EXP `(MATCHCARCDR OBJ
					    (LAMBDA (OBJ) ,(SELECT-MATCH-MATCHITEMS ELT 'OBJ))
					    (LAMBDA (OBJ) ,EXP))))
	   `(MATCHCARCDR ,EXPR . ,(CDDR EXP))))
	((EQ (CAR PATT) 'QUOTE)
	 `(EQUAL ,EXPR ',(CADR PATT)))
	((MEMQ (CAR PATT) '(XR-BQ-APPEND XR-BQ-NCONC XR-BQ-VECTOR))
	 (FERROR NIL "Appending, nconcing or vector construction in SELECT-MATCH pattern."))
	(T (FERROR NIL "Unexpected function ~S found in SELECT-MATCH pattern."
		   (CAR PATT)))))

))

; From file CLPACK.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

;This is the normal INTERN function, once packages are installed.
;Value 1 is the interned symbol.
;Value 2 is T if the symbol was already interned.
;Value 3 is the package that the symbol is actually present in.
(DEFUN INTERN (SYM &OPTIONAL PKG &AUX HASH STR)
  "Interns the string or symbol SYM in package PKG (or the current package).
If the package has a symbol whose pname matches SYM, that symbol is returned.
The USEd packages are also searched.
Otherwise, if SYM is a symbol, it is put in the package and returned.
Otherwise (SYM is a string), a new symbol is constructed, put in the package and returned.

The second value is non-NIL if a preexisting symbol was found.
 More specifically, it can be :INTERNAL, :EXTERNAL or :INHERITED.

The third value is the package the symbol was actually found or inserted in.
This may be PKG or one of the packages USEd by PKG."
  (DECLARE (VALUES SYMBOL ALREADY-INTERNED-FLAG ACTUAL-PACKAGE))
  (PROG INTERN ()
	(COND ((NULL PKG) (SETQ PKG *PACKAGE*))
	      ((NOT (PACKAGEP PKG)) (SETQ PKG (PKG-FIND-PACKAGE PKG))))
	(IF (STRINGP SYM) (SETQ STR SYM)
	  (IF (SYMBOLP SYM) (SETQ STR (GET-PNAME SYM))
	    (SETQ STR (STRING SYM))))
	(SETQ HASH (PKG-STRING-HASH-CODE STR))
	;; Prevent interrupts in case two people intern symbols with the same pname,
	;; both find that there is no such symbol yet,
	;; and both try to stick them in the obarray simultaneously.
	(WITHOUT-INTERRUPTS
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
	      (SETQ I (1+ I))
	      (IF (= I LEN) (SETQ I 0))))
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
		       (RETURN)))  ;Not inheritable from this package.
		(SETQ I (1+ I))
		(IF (= I LEN) (SETQ I 0)))))
	  ;; Must install a new symbol.
	  ;; Make a symbol if the arg is not one.
	  (COND ((NOT (SYMBOLP SYM))
		 (AND (STRINGP SYM)
		      (NEQ (ARRAY-TYPE SYM) 'ART-STRING)
		      (SETQ SYM (STRING-REMOVE-FONTS SYM)))
		 (SETQ SYM (MAKE-SYMBOL SYM T))))
	  (RETURN
	    (FUNCALL (OR (PKG-STORE-FUNCTION PKG) 'PKG-INTERN-STORE)
		     HASH SYM PKG)))))

))

; From file CLPACK.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

;Must lock out interrupts because otherwise someone might rehash the
;package while we are scanning through it.
(DEFUN INTERN-SOFT (SYM &OPTIONAL PKG &AUX HASH STR)
  "Like INTERN but returns NIL for all three values if no suitable symbol found.
Does not ever put a new symbol into the package."
  (DECLARE (VALUES SYMBOL ACTUALLY-FOUND-FLAG ACTUAL-PACKAGE EXTERNAL-P))
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
	  (SETQ I (1+ I))
	  (IF (= I LEN) (SETQ I 0))))
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
	    (SETQ I (1+ I))
	    (IF (= I LEN) (SETQ I 0))))))))

))

; From file CLPACK.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN INTERN-LOCAL (SYM &OPTIONAL PKG &AUX HASH TEM FOUND STR)
  "Like INTERN but does not search the superiors of the package PKG.
If no symbol is found in that package itself, SYM or a new symbol
is put into it, even if a superior package contains a matching symbol."
  (DECLARE (VALUES SYMBOL ALREADY-INTERNED-FLAG ACTUAL-PACKAGE))
  (PROG INTERN ()
	(COND ((NULL PKG) (SETQ PKG *PACKAGE*))
	      ((NOT (PACKAGEP PKG)) (SETQ PKG (PKG-FIND-PACKAGE PKG))))
	(IF (STRINGP SYM) (SETQ STR SYM)
	  (IF (SYMBOLP SYM) (SETQ STR (GET-PNAME SYM))
	    (SETQ STR (STRING SYM))))
	(SETQ HASH (PKG-STRING-HASH-CODE STR))
	(WITHOUT-INTERRUPTS
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
	      (SETQ I (1+ I))
	      (IF (= I LEN) (SETQ I 0))))
	  (AND FOUND (RETURN TEM T PKG))
	  (COND ((NOT (SYMBOLP SYM))
		 (AND (STRINGP SYM)
		      (NEQ (ARRAY-TYPE SYM) 'ART-STRING)
		      (SETQ SYM (STRING-REMOVE-FONTS SYM)))
		 (SETQ SYM (MAKE-SYMBOL SYM T))))
	  (RETURN
	    (FUNCALL (OR (PKG-STORE-FUNCTION PKG) 'PKG-INTERN-STORE)
		     HASH SYM PKG)))))

))

(load "sys:fonts;tr12i")
(load "sys:fonts;mouse")
(remprop 'fonts:mouse 'fonts:color-font)

; From file GC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

;Also moves symbols with no value, function defn or plist
;into worthless-symbol-area.
(DEFUN COLLAPSE-DUPLICATE-PNAMES ()
  (DECLARE (SPECIAL TEM-PKG COUNT TOTAL-SIZE SYMBOLS-MOVED))
  (LET ((TEM-PKG (PKG-FIND-PACKAGE '(GC-TEM GLOBAL 50000.) T NIL))
	(SYMBOLS-MOVED 0)
	(COUNT 0)
	(TOTAL-SIZE 0))
    (PAGE-IN-AREA NR-SYM)
    (PAGE-IN-AREA P-N-STRING)
    (MAPATOMS-NR-SYM
      #'(LAMBDA (SYMBOL &AUX TEM)
	  ;; Collapse the pname if this is not the first symbol with this pname.
	  (WHEN (AND (SYMBOL-PACKAGE SYMBOL) ;Otherwise INTERN would side-effect the symbol.
		     ( (%P-DATA-TYPE (GET-PNAME SYMBOL)) DTP-HEADER-FORWARD))
	    (WHEN (AND (NEQ SYMBOL (SETQ TEM (INTERN-LOCAL SYMBOL TEM-PKG)))
		       (NEQ (GET-PNAME SYMBOL) (GET-PNAME TEM)))
	      (LET ((%INHIBIT-READ-ONLY T))
		(STRUCTURE-FORWARD (GET-PNAME SYMBOL) (GET-PNAME TEM)))
	      (INCF TOTAL-SIZE (%STRUCTURE-TOTAL-SIZE (GET-PNAME TEM)))
	      (INCF COUNT 1)))
	  ;; Forward the symbol into worthless-symbol-area if desired.
	  (WHEN (AND ( (%P-DATA-TYPE SYMBOL) DTP-HEADER-FORWARD)
		     ( (%AREA-NUMBER SYMBOL) WORTHLESS-SYMBOL-AREA)
		     (OR (AND (NOT (FBOUNDP SYMBOL))
			      (OR (NOT (BOUNDP SYMBOL))
				  (KEYWORDP SYMBOL))
			      (NULL (PLIST SYMBOL)))
			 (NOT (SYMBOL-PACKAGE SYMBOL))))
	    (LET ((NEW (LET ((DEFAULT-CONS-AREA WORTHLESS-SYMBOL-AREA))
			 (MAKE-SYMBOL (GET-PNAME SYMBOL)))))
	      (%BLT-TYPED SYMBOL NEW (%STRUCTURE-TOTAL-SIZE SYMBOL) 1)
	      (STRUCTURE-FORWARD SYMBOL NEW))
	    (INCF SYMBOLS-MOVED))))
    (PKG-KILL TEM-PKG)
    (RETURN-STORAGE TEM-PKG)
    (CLEAN-UP-STATIC-AREA PKG-AREA)
    (CLEAN-UP-STATIC-AREA P-N-STRING)
    (CLEAN-UP-STATIC-AREA NR-SYM)
    (GC-REPORT "Collapsing duplicate pnames saved ~:D words, for ~:D pnames.
~:D symbols moved to WORTHLESS-SYMBOL-AREA."
	       TOTAL-SIZE COUNT SYMBOLS-MOVED)))

))
