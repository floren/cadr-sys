;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.4
;;; Reason:
;;;  ZWEI's Lisp sectionization didn't understand definitions with
;;;  a package prefix.  (defstruct foo1) worked fine, but
;;;  (zl:defstruct foo2) didn't work.
;;; Written 26-Jul-88 16:21:47 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Cthulhu from band 3
;;; with System 125.19, ZWEI 125.3, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, Experimental Kermit 36.5, microcode 1761, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.ZWEI; SECTIO.LISP#313 at 26-Jul-88 16:21:57
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(defun definition-function-start (line &aux idx end-idx)
  (if (not (char-equal (char line 0) #/())
      0
    (setq end-idx (string-search-set *whitespace-chars* line 1))
    (if (setq idx (string-search-char #/: line 1 end-idx))
	(do ()
	    ((char-not-equal (char line idx) #/:) idx)
	  (incf idx))
      1)))

))

; From modified file DJ: L.ZWEI; SECTIO.LISP#313 at 26-Jul-88 16:22:22
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN (:PROPERTY :LISP GET-SECTION-NAME) (LINE BP &AUX STR SYM ERROR-P
					   IDX start-idx END-IDX (EOF "") NON-FONT-LINE)
  (IF (NOT (AND (> (LENGTH LINE) 1) (CHAR-EQUAL (CHAR LINE 0) #/()))
      (VALUES NIL NIL T)
    (SETQ ERROR-P T)
    (WHEN (AND (setq start-idx (definition-function-start line))
	       (%STRING-EQUAL LINE start-idx "DEF" 0 3)
	       (NOT (%STRING-EQUAL LINE start-idx "DEFPROP " 0 8))
	       (SETQ IDX (STRING-SEARCH-SET *WHITESPACE-CHARS* LINE))
 	       (SETQ IDX (STRING-SEARCH-NOT-SET *WHITESPACE-CHARS* LINE IDX)))
     (SETQ NON-FONT-LINE (STRING-REMOVE-FONTS LINE))
     (dotimes (i 2)
       ;;Kludge -- (defstruct (abc :opt1 xx :opt2 xxx)  parses fine
       ;;          (defstruct (abc :opt1 xx
       ;;                          :opt2 xxx)           doesn't
       ;;If we run out of line on first try and start idx is "(", try again skipping the "("
       (SETQ ERROR-P NIL)
       (CONDITION-CASE ()
	  (MULTIPLE-VALUE-SETQ (SYM END-IDX)
	    (CL:READ-FROM-STRING NON-FONT-LINE NIL EOF :START IDX))
	(:NO-ERROR
	 (cond ((EQ SYM EOF)
		(SETQ ERROR-P T)
		(if (eq (char non-font-line idx) #/()
		    (incf idx)
		  (return)))
	       (t
		(SETQ STR (SUBSTRING NON-FONT-LINE IDX (MIN (LENGTH LINE) END-IDX))))))
	(SYS:PARSE-ERROR
	 (SETQ STR (GET-DEFUN-NAME (MOVE-BP BP LINE 0)))))
       (UNLESS ERROR-P
	 (MULTIPLE-VALUE-SETQ (SYM NIL ERROR-P)
	   (SYMBOL-FROM-STRING STR NON-FONT-LINE NIL SYM))
	 ;;No error, all is cool
	 (return))))
    (WHEN ERROR-P
      (SETQ SYM (CONCATENATE 'STRING
			     (LET ((BUFFER (NODE-TOP-LEVEL-NODE (LINE-NODE LINE))))
			       (IF (safe-get-zwei-buffer-instance-variable BUFFER 'PATHNAME)
				   (LET ((NAME
					   (PATHNAME-NAME (BUFFER-PATHNAME BUFFER))))
				     (IF (CONSP NAME)
					 (APPLY #'STRING-APPEND
						(MAPCAR (LAMBDA (NAME-ELT)
							  (IF (CONSP NAME-ELT)
							      (CAR NAME-ELT) NAME-ELT))
							NAME))
				       (STRING NAME)))
				 (or (safe-get-zwei-buffer-instance-variable BUFFER 'name)
				     "SECTION")))
			     "-"
			     (LET ((START-INDEX (STRING-SEARCH-NOT-CHAR #/( LINE)))
			       (SUBSTRING LINE START-INDEX
					  (AND START-INDEX
					       (STRING-SEARCH-SET *WHITESPACE-CHARS*
								  LINE START-INDEX))))
			     "-"
			     (PRIN1-TO-STRING (INCF *SECTION-COUNT*)))
	    STR SYM))
    (VALUES SYM STR NIL)))

))

; From modified file DJ: L.ZWEI; SECTIO.LISP#313 at 26-Jul-88 16:22:31
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN SYMBOL-FROM-STRING (STR &OPTIONAL LINE OK-TO-ASK SYM &AUX ERROR-P)
  "Given a string STR as found after DEF..., return the name of the object being defined.
LINE is the line that the string was found in.  It is used for
finding the particular defining construct used; this affects the result
since (DEFUN (FOO BAR) defines (:PROPERTY FOO BAR)
while (DEFMETHOD (FOO BAR) defines (:METHOD FOO BAR).
OK-TO-ASK means in certain circumstances
where things are not clear, ask the user.  Otherwise we guess.

The arg can also be an object; then its printed representation is used as the string.

The second value is a canonicalized string for the object
 (maybe the same string specified, maybe not).
The third is T if there was a problem
 in parsing the string (such as unbalanced parens).

You can pass the read-in form of the object as the fourth arg
if you already know it."
  (DECLARE (VALUES SYM STR ERROR-P))
  (IF (ARRAYP STR)
      (UNLESS SYM
	(CONDITION-CASE ()
			(SETQ SYM (CL:READ-FROM-STRING STR))
	  (SYS:PARSE-ERROR (SETQ ERROR-P T))))
    (SETQ SYM STR
	  STR (FORMAT NIL "~S" STR)))
  (COND (ERROR-P
	 (VALUES NIL NIL ERROR-P))
	((SYMBOLP SYM)
	 (VALUES SYM (SYMBOL-NAME SYM)))
	((ATOM SYM)
 	 (VALUES NIL NIL T))
	(T
	 ;; Here SYM is a list.  Certain types of function specs have two ways to
	 ;; type them, with and without the leading type keyword.  Also certain types
	 ;; of functions and other definitions do not follow the standard form
	 ;; of (DEFxxx name options...).  What we do here is to recognize and
	 ;; standardize those cases.  The variables are:
	 ;;	TYPE - the type of function spec or non-function definition
	 ;;	SYM - the function spec or definition name
	 ;;	SPEC - the variant of SYM which appears in the source code
	 ;;	STR - SPEC converted to a string
	 ;; :HANDLER doesn't appear in source files, but gets translated into
	 ;; an appropriate :METHOD here, by analyzing the combined method.
	 ;; :INTERNAL doesn't appear in source files, but might be given as the argument
	 ;; to M-X Disassemble.  The code here just tries not to destroy it.
	 (LET ((TYPE (CAR SYM))
	       DELIM-IDX SPEC)
	   (IF (GET TYPE 'SI:FUNCTION-SPEC-HANDLER)
	       (SETQ SPEC (CDR SYM)
		     STR (DEFINITION-NAME-AS-STRING TYPE SPEC))
	     (SETQ SPEC SYM
		   DELIM-IDX (AND LINE (STRING-SEARCH-SET "( " LINE 1))
		   TYPE (let ((start-idx (definition-function-start line)))
			  (COND ((NULL LINE)
				 :MAYBE-METHOD)
				((%STRING-EQUAL LINE start-idx "DEFMETHOD" 0 9.)
				 :ALWAYS-METHOD)
				((%STRING-EQUAL LINE start-idx "DEFWRAPPER" 0 10.)
				 (SETQ SPEC (LIST (CAR SPEC) :WRAPPER (SECOND SPEC)))
				 :ALWAYS-METHOD)
				((%STRING-EQUAL LINE start-idx "DEFSTRUCT" 0 9.)
				 :DEFSTRUCT)
				((%STRING-EQUAL LINE start-idx "DEFSELECT" 0 9.)
				 :DEFSELECT)
				(T :PROPERTY)))))
	   (OR (SELECTQ TYPE
		 (:INSTANCE-METHOD
		  (AND (BOUNDP (CAR SPEC))
		       (SETQ SYM (FUNCALL (SI:CLASS (SYMBOL-VALUE (CAR SPEC)))
					  :METHOD-FOR (CADR SPEC)))))
		 (:ALWAYS-METHOD
		  (SETQ SYM (CONS ':METHOD SPEC)))
		 ((:METHOD :HANDLER :MAYBE-METHOD)
		  (LET ((FLAVOR (CAR SPEC))
			(MESSAGE (IF (CDDR SPEC) (CADDR SPEC) (CADR SPEC)))
			FL)
		    (COND ((SETQ FL (GET FLAVOR 'SI:FLAVOR)))
			  ((AND (VALIDATE-2-LONG-LIST SPEC) (SI:CLASS-SYMBOLP FLAVOR))
			   (SETQ SYM (FUNCALL (SYMBOL-VALUE FLAVOR) :METHOD-FOR (CADR SPEC))
				 FL T))
			  (OK-TO-ASK
			   (DOLIST (SYMBOL (PACKAGE-LOOKALIKE-SYMBOLS FLAVOR
					      NIL '(SI:FLAVOR)))
			     (IF (FQUERY () "Do you mean ~S? "
					 `(:METHOD ,SYMBOL . ,(CDR SPEC)))
				 (RETURN (SETQ FLAVOR SYMBOL
					       SPEC (CONS FLAVOR (CDR SPEC))
					       FL (GET FLAVOR 'SI:FLAVOR)))))))
		    (COND ((SYMBOLP FL)		;T or NIL
			   (AND (EQ TYPE ':MAYBE-METHOD)
				(VALIDATE-2-LONG-LIST SPEC)
				(SETQ SYM (CONS ':PROPERTY SPEC))))
			  ((FDEFINEDP `(:METHOD . ,SPEC))
			   (SETQ SYM `(:METHOD . ,SPEC)))
			  (OK-TO-ASK
			   (DOLIST (SYMBOL (OR (FIND-COMBINED-METHODS FLAVOR MESSAGE NIL)
					       (SI:FLAVOR-ALL-INHERITABLE-METHODS
						 FLAVOR MESSAGE)))
			     (IF (FQUERY () "Do you mean ~S? " SYMBOL)
				 (RETURN (SETQ SYM SYMBOL))))))))
		 ((:DEFSTRUCT :SPECIAL-FORM)
		  (SETQ SYM (CAR SPEC)
			STR (GET-PNAME SYM)))
		 (:DEFSELECT
		  (SETQ SYM (CAR SPEC))
		  (IF (SYMBOLP SYM)
		      (SETQ STR (GET-PNAME SYM))
		      (MULTIPLE-VALUE-SETQ (SYM STR)
			(SYMBOL-FROM-STRING SYM))))
		 (:PROPERTY
		  (AND (VALIDATE-2-LONG-LIST SPEC)
		       (SETQ SYM (CONS TYPE SPEC))))
		 (:INTERNAL (SETQ SYM (CONS TYPE SPEC))
			    (SETQ STR (DEFINITION-NAME-AS-STRING NIL (CAR SPEC)))))
	       ;; Something we don't understand, make a bogus symbol to use as a property
	       ;; list to remember the location of this definition
	       (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))))
	 (IF (NOT (SYS:VALIDATE-FUNCTION-SPEC SYM))
	     (VALUES NIL NIL T)
	     (VALUES SYM STR)))))

))
