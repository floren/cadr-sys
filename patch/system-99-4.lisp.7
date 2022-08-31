;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.4
;;; Reason: bug in OPEN with :PROBE keyword.
;;; Make symbol || read in properly.
;;; Fix ZWEI:SET-COMTAB-CONTROL-INDIRECTION typo.  Run it again.
;;; FILLARRAY bug.  Lexical scoping of block names and tags bug.
;;; MAKE-LOGICAL-PATHNAME-HOST bug (MLY).
;;; MERGE-PATHNAMES-1 bug (MLY).
;;; Reason: Make symbol || read in properly.
;;; Fix ZWEI:SET-COMTAB-CONTROL-INDIRECTION typo.  Run it again.
;;; FILLARRAY bug.  Lexical scoping of block names and tags bug.
;;; MERGE-PATHNAMES-1 bug (MLY).
;;; Written 9/15/84 01:58:31 by RMS,
;;; while running on Lisp Machine Twenty-four from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.3, CADR 4.0, Experimental ZMail 54.0, MIT-Specific 23.0, microcode 320, GC@2.


; From file PATHNM.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(defun merge-pathnames-1 (pathname
			  &optional defaults
			  &key (default-version nil default-version-specified-p)
			       (default-type nil default-type-specified-p)
			       always-merge-type
			       always-merge-version
			  &aux default new-device new-directory new-name
			       new-type new-version new-otype merge-type-p merge-version-p)
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
If non-NIL DEFAULT-TYPE and DEFAULT-VERSION respecively are used as the defaults for
the type and version components if those components are not supplied by PATHNAME.
Otherwise, these components are defaulted from DEFAULTS in the usual manner.
ALWAYS-MERGE-TYPE and ALWAYS-MERGE-VERSION respectively mean that the version
and type components should always be merged in (from either DEFAULT-TYPE and DEFAULT-VERSION
or from DEFAULTS) even if the relevant component is already specified by PATHNAME."
  (setq pathname (parse-pathname pathname nil defaults))
  (if (null defaults) (setq defaults *default-pathname-defaults*))
  (if (not (typep pathname 'pathname))
      pathname					;Some funny thing.  No defaulting possible.
    (setq default (if (atom defaults)
		      (parse-pathname defaults nil pathname)
		      (default-pathname defaults (pathname-host pathname) nil nil t)))
    ;; Merge the device, directory, and name
    (when (null (pathname-device pathname))
      (setq new-device (pathname-device default)))
    (let ((pdir (pathname-directory pathname))
	  (ddir (pathname-directory default)))
      (cond ((null pdir)
	     (setq new-directory ddir))
	    ((eq (car-safe pdir) :relative)
	     (setq new-directory
		   (merge-relative-directory pdir ddir)))))
    (when (null (pathname-name pathname))
      (setq new-name (pathname-name default)))
    ;; hairyness for merging type and version
    (when (or (null (pathname-type pathname))
	      always-merge-type)
      (setq merge-type-p t)
      (if default-type-specified-p
	  (setq new-type default-type)
	(multiple-value-setq (new-type new-otype) (send default :canonical-type))))
    (when (or (null (pathname-version pathname))
	      always-merge-version)
      (setq new-version (or default-version (pathname-version default)) merge-version-p t))
    (send pathname :new-pathname
		   (if new-device :device) new-device
		   (if new-directory :directory) new-directory
		   (if new-name :name) new-name
		   (if merge-type-p :type) new-type
		   (if new-otype :original-type) new-otype
		   (if merge-version-p :version) new-version)))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-READ-THING (STREAM)
  (PROG (CH NUM A B STRING (INDEX 0) STLEN REAL-CH ALREADY-RETURNED
	 (READTABLE-FSM (RDTBL-FSM *READTABLE*))
	 (FNPROP (RDTBL-READ-FUNCTION-PROPERTY *READTABLE*))
	 (STATE (RDTBL-STARTING-STATE *READTABLE*))
	 READ-INSIDE-MULTIPLE-ESCAPE FOUND-MULTI-ESCAPES)
	(MULTIPLE-VALUE (CH NUM REAL-CH FOUND-MULTI-ESCAPES) (XR-XRTYI STREAM T))
	(SETQ STATE (AREF READTABLE-FSM STATE NUM))
	;; Compensate for bad readtable in system 99.
	;; Detect the case of a whitespace char following a pair of vertical bars.
	(AND (NULL STATE) FOUND-MULTI-ESCAPES
	     (SETQ STATE '(UNTYI-FUNCTION . SYMBOL)))
	(UNLESS (NUMBERP STATE)
	  (LET ((FLAG (CAR STATE))
		(TODO (CDR STATE)))
	    (SELECTQ FLAG
	      (NO-UNTYI-QUOTE
	       (SETQ A TODO)
	       (SETQ B 'SPECIAL-TOKEN))
	      (LAST-CHAR
	       (MULTIPLE-VALUE (A B)
		 (FUNCALL (GET TODO FNPROP)
			  STREAM NIL CH)))
	      (NO-UNTYI-FUNCTION
	       (IF *READ-SUPPRESS*
		   (SETQ A NIL B NIL)
		 (SETQ STRING (GET-READ-STRING))
		 (SETF (FILL-POINTER STRING) 1)
		 (SETF (AREF STRING 0) CH)
		 (MULTIPLE-VALUE (A B ALREADY-RETURNED)
		   (FUNCALL (GET TODO FNPROP) STREAM STRING))
		 (UNLESS ALREADY-RETURNED
		   (RETURN-READ-STRING STRING))))
	      (UNTYI-QUOTE
	       (FERROR 'SYS:READ-ERROR-1
		       "Reader in infinite loop reading character: /"~C/"."
		       REAL-CH))
	      (UNTYI-FUNCTION
	       (IF (NOT FOUND-MULTI-ESCAPES)
		   (FERROR 'SYS:READ-ERROR-1
			   "Reader in infinite loop reading character: /"~C/"."
			   REAL-CH)
		 (XR-XRUNTYI STREAM REAL-CH NUM)
		 (IF *READ-SUPPRESS*
		     (SETQ A NIL B NIL)
		   (MULTIPLE-VALUE (A B)
		     (FUNCALL (GET TODO FNPROP) STREAM "")))))
	      (OTHERWISE
	       (FERROR 'SYS:READ-ERROR-1
		       "The reader found ~S in the finite state machine."
		       FLAG)))
	    (RETURN A B)))
	(SETQ STRING (GET-READ-STRING))
	(SETQ STLEN (ARRAY-LENGTH STRING))
     L  (SETF (AREF STRING INDEX) CH)
	(SETQ INDEX (1+ INDEX))
	(MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
	(SETQ STATE (AREF READTABLE-FSM STATE NUM))
	(COND ((NUMBERP STATE)
	       (COND ((= INDEX STLEN)
		      (SETQ STLEN (+ 32. STLEN))
		      (ADJUST-ARRAY-SIZE STRING STLEN)
		      (SETQ STRING (FOLLOW-STRUCTURE-FORWARDING STRING))))
	       (GO L)))
	(LET ((FLAG (CAR STATE))
	      (TODO (CDR STATE)))
	  (SELECTQ FLAG
	    (UNTYI-FUNCTION
	     (XR-XRUNTYI STREAM REAL-CH NUM)
	     (SETF (FILL-POINTER STRING) INDEX)
	     (IF *READ-SUPPRESS*
		 (SETQ A NIL B NIL)
	       (MULTIPLE-VALUE (A B ALREADY-RETURNED)
		 (FUNCALL (GET TODO FNPROP) STREAM STRING))))
	    (LAST-CHAR
	     (SETF (FILL-POINTER STRING) INDEX)
	     (MULTIPLE-VALUE (A B ALREADY-RETURNED)
	       (FUNCALL (GET TODO FNPROP)
			STREAM STRING CH)))
	    (NO-UNTYI-FUNCTION
	     (SETF (FILL-POINTER STRING) (1+ INDEX))
	     (SETF (AREF STRING INDEX) CH)
	     (IF *READ-SUPPRESS*
		 (SETQ A NIL B NIL)
	       (MULTIPLE-VALUE (A B ALREADY-RETURNED)
		 (FUNCALL (GET TODO FNPROP) STREAM STRING))))
	    (UNTYI-QUOTE
	     (XR-XRUNTYI STREAM REAL-CH NUM)
	     (SETQ A TODO)
	     (SETQ B 'SPECIAL-TOKEN))
	    (NO-UNTYI-QUOTE
	     (SETQ A TODO)
	     (SETQ B 'SPECIAL-TOKEN))
	    (OTHERWISE
	     (FERROR 'SYS:READ-ERROR-1
		     "The reader found ~S in the finite state machine."
		     FLAG)))
	  (UNLESS ALREADY-RETURNED
	    (RETURN-READ-STRING STRING))
	  (RETURN A B))))

))

; From file READ.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-XRTYI (STREAM &OPTIONAL IGNORE-WHITESPACE NO-CHARS-SPECIAL NO-MULTIPLE-ESCAPES)
  "Read a character from STREAM, processing escapes (// and /) and multiple-escapes (/|).
IGNORE-WHITESPACE non-NIL means skip over whitespace characters.
NO-CHARS-SPECIAL means do not process escapes specially.
NO-MULTIPLE-ESCAPES means do not process multiple-escape characters specially.

The first value is the translated character.
The second is the index for looking in READ's FSM.
The third is the original, nontranslated character.
The fourth is T if the character was preceded by one or more
 multi-character escape characters that were passed over.

Has a kludge for *READ-BASE* > 10. where letters that should be digits
return the readtable code for EXTENDED-DIGIT rather than their own codes."
  (DECLARE (VALUES TRANSLATED-CHAR FSM-INDEX ACTUAL-CHAR FOUND-MULTI-ESCAPES))
  (PROG TOP (CH BITS CODE CH-CHAR FOUND-MULTI-ESCAPES)
	(SETQ XR-XRTYI-PREV-CHAR XR-XRTYI-LAST-CHAR)
     L
	(DO-FOREVER
	  (SETQ CH (SEND STREAM (IF RUBOUT-HANDLER ':ANY-TYI ':TYI)))
	  (if (fixnump ch) (setf (char-font ch) 0))
	  (COND ((NULL CH)
		 (RETURN-FROM TOP CH (RDTBL-EOF-CODE *READTABLE*) CH))
		((CONSP CH)
		 (AND (EQ (CAR CH) ':ACTIVATION)
		      ;; Ignore activations except in top-level context.
		      (NOT IGNORE-WHITESPACE)
		      (NOT NO-CHARS-SPECIAL)
		      (NOT NO-MULTIPLE-ESCAPES)
		      (LET ((CH1 (CAR (RDTBL-WHITESPACE *READTABLE*))))
			(RETURN-FROM TOP
			  CH1 (RDTBL-CODE *READTABLE* CH1) CH))))
		((AND READ-DISCARD-FONT-CHANGES
		      (EQ CH #/))
		 (IF (EQ #/ (SEND STREAM ':TYI))
		     (RETURN)))
		((NOT (> CH RDTBL-ARRAY-SIZE))
		 (RETURN))))
	(SETQ CH-CHAR (LDB %%CH-CHAR CH))
	(SETQ BITS (RDTBL-BITS *READTABLE* CH-CHAR))
	(SETQ CODE (RDTBL-CODE *READTABLE* CH-CHAR))
	(COND ((AND (NOT NO-CHARS-SPECIAL)
		    (NOT NO-MULTIPLE-ESCAPES)
		    (= CODE
		       (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*)))
	       ;; Vertical bar.
	       (SETQ FOUND-MULTI-ESCAPES T)
	       (SETQ READ-INSIDE-MULTIPLE-ESCAPE
		     (IF READ-INSIDE-MULTIPLE-ESCAPE NIL
		       CH-CHAR))
	       (GO L))
	      ((AND (NOT NO-CHARS-SPECIAL)
		    (= CODE
		       (RDTBL-ESCAPE-CODE *READTABLE*)))
	       ;; Slash
	       (SETQ XR-XRTYI-PREV-CHAR CH)
	       (DO-FOREVER
		 (SETQ CH (SEND STREAM ':TYI))
		 (COND ((AND READ-DISCARD-FONT-CHANGES
			     (EQ CH #/))
			(IF (EQ #/ (SEND STREAM ':TYI))
			    (RETURN)))
		       (T (RETURN))))
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (OR CH
			   (PROGN
			     (CERROR ':NO-ACTION NIL 'SYS:READ-END-OF-FILE
				     "EOF on ~S after a ~S." STREAM
				     (STRING XR-XRTYI-PREV-CHAR))
			     #/SPACE))
		       (RDTBL-SLASH-CODE *READTABLE*)
		       CH))
	      ((AND (NOT NO-CHARS-SPECIAL)
		    (= CODE
		       (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*)))
	       ;; circlecross
	       (SETQ XR-XRTYI-LAST-CHAR (XR-READ-CIRCLECROSS STREAM))
	       (RETURN XR-XRTYI-LAST-CHAR
		       (RDTBL-SLASH-CODE *READTABLE*)
		       XR-XRTYI-LAST-CHAR))
	      (READ-INSIDE-MULTIPLE-ESCAPE
	       ;; Ordinary character but within vertical bars.
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (OR CH
			   (PROGN
			     (CERROR ':NO-ACTION NIL 'SYS:READ-END-OF-FILE
				     "EOF on ~S inside a ~C-quoted token." STREAM
				     READ-INSIDE-MULTIPLE-ESCAPE)
			     #/SPACE))
		       (RDTBL-SLASH-CODE *READTABLE*)
		       CH))
	      (T
	       ;; Ordinary character.
	       (COND ((AND IGNORE-WHITESPACE (NOT FOUND-MULTI-ESCAPES)
			   (BIT-TEST 1 BITS))
		      ;; Here if whitespace char to be ignored.
		      (SETQ XR-XRTYI-PREV-CHAR CH)
		      (GO L)))
	       ;; Here for ordinary, significant input char.
	       (SETQ XR-XRTYI-LAST-CHAR CH)
	       (RETURN (RDTBL-TRANS *READTABLE* CH-CHAR)
		       ;; If not doing slashes, caller must not really want the RDTBL-CODE,
		       ;; so return a value which, if passed to XR-XRUNTYI,
		       ;; will prevent barfing.
		       (IF NO-CHARS-SPECIAL 0
			 (IF (AND (NUMBERP *READ-BASE*)
				  ( #/A (CHAR-UPCASE CH) (+ *READ-BASE* #/A -11.)))
			     (CDR (GETF (RDTBL-PLIST *READTABLE*) 'EXTENDED-DIGIT))
			   (RDTBL-CODE *READTABLE* CH-CHAR)))
		       CH T)))))

))

; From file COMTAB.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN SET-COMTAB-CONTROL-INDIRECTION (COMTAB)
  "Make EMACS-like indirections in COMTAB.
Tab, Return, etc. are indirected to C-I, C-M.
Meta-anything is indirected to that anything.
Non-ascii control characters indirect to the non-control character.
However, commands that are already defined are not clobbered."
  (MAKE-COMTAB-NON-SPARSE COMTAB)
  (LET ((ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB)))
    ;; Indirect things like Tab to things like control-I
    (DOLIST (CHAR '(#/CR #/LF #/TAB #/BS #/FF #/VT))
      (WHEN (NULL (AREF ARRAY CHAR 0))
	(SETF (AREF ARRAY CHAR 0) (LIST 1 (- CHAR #o100)))))
    ;; Indirect all meta things through the corresponding non-meta thing
    (DO ((I 2 (1+ I))) ((= I 4))
      (DOTIMES (CHAR (ARRAY-DIMENSION ARRAY 0))
	(WHEN (NULL (AREF ARRAY CHAR I))
	  (SETF (AREF ARRAY CHAR I) (LIST (- I 2) CHAR)))))
    ;; Indirect controls other than atsign through underscore to non-controls
    (DOTIMES (CHAR (ARRAY-DIMENSION ARRAY 0))
      (WHEN (AND (NOT ( #/@ CHAR #/_))
		 (NULL (AREF ARRAY CHAR 1)))
	(SETF (AREF ARRAY CHAR 1) (LIST 0 (INT-CHAR CHAR)))))))

))

; From file COMTAB.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

  (SET-COMTAB-CONTROL-INDIRECTION *STANDARD-CONTROL-X-COMTAB*)

))

; From file SEARCH.LISP OZ:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SEARCH  "

	 (SET-COMTAB-CONTROL-INDIRECTION *SEARCH-CONTROL-H-COMTAB*)

))

; From file QMISC.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN FILLARRAY (ARRAY SOURCE)
  "Fill the contents of ARRAY from SOURCE.
If SOURCE is a list, its last element is repeated to fill any part of ARRAY left over.
If SOURCE is an array, elements of ARRAY not filled by SOURCE are left untouched.
If SOURCE is NIL, the array is filled with the default type for the array; this is 0 or NIL.
If ARRAY is NIL, a new list as big as SOURCE is created."
  (LET ((ARRAY (COND ((NULL ARRAY)
		     (SETQ ARRAY
			   (MAKE-ARRAY
			     (COND ((NULL SOURCE) 0)
				   ((CONSP SOURCE) (LENGTH SOURCE))
				   ((ARRAYP SOURCE) (ARRAY-DIMENSIONS SOURCE))
				   (T (FERROR NIL
					      "Unable to default destination array"))))))
		    ((AND (SYMBOLP ARRAY)
			  (FBOUNDP ARRAY)
			  (ARRAYP (FSYMEVAL ARRAY)))
		     (FSYMEVAL ARRAY))
		    (T ARRAY))))
    (CHECK-TYPE ARRAY ARRAY)
    (CHECK-TYPE SOURCE (OR ARRAY LIST))
    (LET ((DEST-NDIMS (ARRAY-RANK ARRAY))
	  (SOURCE-IS-AN-ARRAY-P (ARRAYP SOURCE)))
      (COND (SOURCE-IS-AN-ARRAY-P
	     (LET ((SOURCE-NDIMS (ARRAY-RANK SOURCE)))
	       (COND ((AND (= DEST-NDIMS 1)
			   (= SOURCE-NDIMS 1))
		      ;; One-D array into a one-D array is in microcode!
		      (LET ((N-ELEMENTS (MIN (ARRAY-LENGTH SOURCE)
					     (ARRAY-LENGTH ARRAY))))
			(COPY-ARRAY-PORTION SOURCE 0 N-ELEMENTS ARRAY 0 N-ELEMENTS)))
		     (T
		      ;; Hairy case, some array is multi-dimensional.
		      (USING-RESOURCE (SOURCE-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
			(USING-RESOURCE (DEST-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
			  (DOTIMES (I 10)
			    (ASET 0 SOURCE-INDEX-ARRAY I)
			    (ASET 0 DEST-INDEX-ARRAY I))
			  (LET ((SOURCE-ELEMENTS (ARRAY-LENGTH SOURCE))
				(DEST-ELEMENTS (ARRAY-LENGTH ARRAY)))
			    (DOTIMES (I (MIN SOURCE-ELEMENTS DEST-ELEMENTS))
			      (FILLARRAY-PUT (FILLARRAY-GET SOURCE
							    SOURCE-INDEX-ARRAY
							    SOURCE-NDIMS)
					     ARRAY DEST-INDEX-ARRAY DEST-NDIMS)))))))))
	    ((NULL SOURCE) (COPY-ARRAY-PORTION ARRAY 0 0 ARRAY 0 (ARRAY-LENGTH ARRAY)))
	    (T
	     ;; Source is a list.
	     (COND ((= DEST-NDIMS 1)
		    (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
		      (ASET (CAR SOURCE) ARRAY X)
		      (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))
		   ((= DEST-NDIMS 2)
		    (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
		      (DOTIMES (Y (ARRAY-DIMENSION ARRAY 1))
			(ASET (CAR SOURCE) ARRAY X Y)
			(IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE))))))
		   ((= DEST-NDIMS 3)
		    (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
		      (DOTIMES (Y (ARRAY-DIMENSION ARRAY 1))
			(DOTIMES (Z (ARRAY-DIMENSION ARRAY 2))
			  (ASET (CAR SOURCE) ARRAY X Y Z)
			  (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))))
		   (T
		    (USING-RESOURCE (DEST-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
		      (DOTIMES (I 8)
			(ASET 0 DEST-INDEX-ARRAY I))
		      (DOTIMES (I (ARRAY-LENGTH ARRAY))
			(FILLARRAY-PUT (CAR SOURCE) ARRAY DEST-INDEX-ARRAY DEST-NDIMS)
			(IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE))))))))))
    ARRAY))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1BLOCK (FORM &OPTIONAL ALSO-BLOCK-NAMED-NIL)
  (LET* ((PROGNAME (CADR FORM)) (BODY (CDDR FORM))
	 (RETTAG (GENSYM))
	 (*GOTAG-ENVIRONMENT*)
	 (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*)
;	 (RETPROGDESC
;	   (IF (OR (AND BIND-RETPROGDESC (NEQ PROGNAME 'T))
;		   (EQ PROGNAME 'NIL))
;	       (CAR *PROGDESC-ENVIRONMENT*)
;	     RETPROGDESC))
	 )
    (WHEN (OR (AND ALSO-BLOCK-NAMED-NIL (NEQ PROGNAME 'T))
	      (EQ PROGNAME 'NIL))
      (PUSH (MAKE-PROGDESC :NAME 'NIL
			   :RETTAG RETTAG
			   :NBINDS 0
			   :ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*)
	      *PROGDESC-ENVIRONMENT*))
    (PUSH (MAKE-PROGDESC :NAME PROGNAME
			 :RETTAG RETTAG
			 :NBINDS 0
			 ;; :VARS VARS
			 :ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*)
	  *PROGDESC-ENVIRONMENT*)
    (AND (CDR BODY) (SETQ TLEVEL NIL))
    (SETQ BODY (P1PROGN-1 BODY))
    ;; Push on *GOTAG-ENVIRONMENT* a description of this prog's "return tag",
    ;; a tag we generate and stick at the end of the prog.
    (PUSH (MAKE-GOTAG RETTAG RETTAG NIL (CAR *PROGDESC-ENVIRONMENT*)) *GOTAG-ENVIRONMENT*)
    (SETF (PROGDESC-EXIT-LEXICAL-CLOSURE-COUNT (CAR *PROGDESC-ENVIRONMENT*))
	  *LEXICAL-CLOSURE-COUNT*)
    (LET ((BLOCK
	    `(,(CAR FORM) ,*GOTAG-ENVIRONMENT* ,(CAR *PROGDESC-ENVIRONMENT*) . ,BODY)))
      (IF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG (CAR *PROGDESC-ENVIRONMENT*))
	  (LET ((VARNAME (GENSYM))
		HOME)
	    ;; For a BLOCK name used from internal lambdas,
	    ;; we make a local variable to hold the catch tag
	    ;; (which is a locative pointer to that variable's own slot).
	    ;; The internal lambda accesses this variable via the lexical scoping mechanism.
	    ;; This ensures the proper lexical scoping when there are
	    ;; multiple activations of the same block.
	    (SETQ HOME (VAR-MAKE-HOME VARNAME 'FEF-LOCAL 'FEF-ARG-INTERNAL-AUX
				      NIL 'FEF-QT-EVAL '(FEF-ARG-USED-IN-LEXICAL-CLOSURES)
				      NIL))
	    (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG (CAR *PROGDESC-ENVIRONMENT*))
		  HOME)
	    (PUSH HOME ALLVARS)
	    (VAR-COMPUTE-INIT HOME NIL)
	    (INCF (VAR-USE-COUNT HOME))
	    `(PROGN
	       (SETQ (LOCAL-REF ,HOME) (VARIABLE-LOCATION (LOCAL-REF ,HOME)))
	       (*CATCH (LOCAL-REF ,HOME) ,BLOCK)))
	BLOCK))))

;;; Defines a block with two names, the specified name and NIL.

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1RETURN-FROM (FORM)
  (LET ((PROGDESC #|(IF (NULL (CADR FORM))
		        RETPROGDESC |#
		  (ASSQ (CADR FORM) *PROGDESC-ENVIRONMENT*)))
    (IF (MEMQ PROGDESC *OUTER-CONTEXT-PROGDESC-ENVIRONMENT*)
	`(*THROW ,(TRY-REF-LEXICAL-HOME (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG PROGDESC))
		 ,(IF (= (LENGTH (CDDR FORM)) 1)	;return all values of only form
		      (P1 (THIRD FORM))
		    (LET ((P1VALUE 1))		;else return each value of multiple forms
		      `(VALUES . ,(MAPCAR #'P1 (CDDR FORM))))))
      (LET ((P1VALUE 1))
	`(RETURN-FROM ,(CADR FORM) . ,(MAPCAR #'P1 (CDDR FORM)))))))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN BREAKOFF (X &OPTIONAL LEXICAL &AUX FNAME FNAME-TO-GIVE LOCAL-NAME)
  (MULTIPLE-VALUE-BIND (VARS-NEEDED-LEXICALLY FUNCTIONS-NEEDED-LEXICALLY
			BLOCK-NAMES GO-TAGS)
      (CW-TOP-LEVEL-LAMBDA-EXPRESSION
	X					;form
	(LET ((ACCUM				;variables to check for
		(LOOP FOR HOME IN VARS
		      WHEN (AND (EQ (VAR-TYPE HOME) 'FEF-LOCAL)
				(EQ HOME (ASSQ (VAR-NAME HOME) VARS)))	;Omit shadowed bindings.
		      COLLECT (VAR-NAME HOME))))
	  (DOLIST (ELT *OUTER-CONTEXT-VARIABLE-ENVIRONMENT*)
	    (DOLIST (HOME ELT)
	      (PUSHNEW (VAR-NAME HOME) ACCUM :TEST 'EQ)))
	  ACCUM)
	(MAPCAR #'CAR *LOCAL-FUNCTIONS*)	;functions we're interested in
	*FUNCTION-ENVIRONMENT*)
    (DOLIST (V VARS-NEEDED-LEXICALLY)
      ;; Note: if V is not on VARS, it must come from an outer lexical level.
      ;; That is ok, and it still requires this LAMBDA to be lexical to access it.
      (SETQ LEXICAL T)
      (LET ((TEM (ASSQ V VARS)))
	(WHEN TEM
	  (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
		   (VAR-MISC TEM) :TEST 'EQ))))
    (DOLIST (F FUNCTIONS-NEEDED-LEXICALLY)
      (LET ((TEM (ASSQ F *LOCAL-FUNCTIONS*)))
	(WHEN TEM
	  (SETQ LEXICAL T)
	  (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
		   (VAR-MISC (CADR TEM)) :TEST 'EQ))))
    (DOLIST (B BLOCK-NAMES)
      (LET ((TEM (ASSQ B *PROGDESC-ENVIRONMENT*)))
	(WHEN TEM
	  (SETQ LEXICAL T)
	  (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG TEM) T))))
    (DOLIST (G GO-TAGS)
      (LET ((TEM (SYS:ASSOC-EQUAL G *GOTAG-ENVIRONMENT*)))
	(WHEN TEM
	  (SETQ LEXICAL T)
	  (SETF (GOTAG-USED-IN-LEXICAL-CLOSURES-FLAG TEM) T)
	  (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG (GOTAG-PROGDESC TEM)) T)))))
  (IF (AND (EQ (CAR X) 'NAMED-LAMBDA)
	   (NOT (MEMQ (CADR X) LOCAL-FUNCTION-MAP)))
      (SETQ LOCAL-NAME (CADR X))
    (SETQ LOCAL-NAME *BREAKOFF-COUNT*))
  (SETQ FNAME `(:INTERNAL ,FUNCTION-TO-BE-DEFINED ,*BREAKOFF-COUNT*)
	FNAME-TO-GIVE `(:INTERNAL ,NAME-TO-GIVE-FUNCTION ,LOCAL-NAME))
  (PUSH LOCAL-NAME LOCAL-FUNCTION-MAP)
  (INCF *BREAKOFF-COUNT*)
  (WHEN LEXICAL
    (INCF *LEXICAL-CLOSURE-COUNT*))
  (LET ((SFD SELF-FLAVOR-DECLARATION)
	(LOCAL-DECLS LOCAL-DECLARATIONS))
    ;; Pass along the parent function's self-flavor declaration.
    (IF SFD (PUSH `(:SELF-FLAVOR . ,SFD) LOCAL-DECLS))
    (SETQ COMPILER-QUEUE
	  (NCONC COMPILER-QUEUE
		 (NCONS
		   (MAKE-COMPILER-QUEUE-ENTRY
		     :FUNCTION-SPEC FNAME
		     :FUNCTION-NAME FNAME-TO-GIVE
		     :DEFINITION X
		     :DECLARATIONS LOCAL-DECLS
;;; ******** why use this rather than *variable-environment* ?
		     :VARIABLES (AND LEXICAL (CONS T *OUTER-CONTEXT-VARIABLE-ENVIRONMENT*))
		     :LOCAL-FUNCTIONS (AND LEXICAL *LOCAL-FUNCTIONS*)
		     :PROGDESCS *PROGDESC-ENVIRONMENT*
;		     :RETPROGDESC RETPROGDESC
		     :GOTAGS *GOTAG-ENVIRONMENT*
		     :FUNCTION-ENVIRONMENT *FUNCTION-ENVIRONMENT*
		     )))))
  (LET ((TEM `(BREAKOFF-FUNCTION ,FNAME)))
    (IF LEXICAL `(LEXICAL-CLOSURE ,TEM) TEM)))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1TAGBODY (FORM)
  (LET ((*GOTAG-ENVIRONMENT*)
	(P1VALUE NIL)				;Throw it all away...
	(BODY (CDR FORM))
	(MYPROGDESC (MAKE-PROGDESC :NAME '(TAGBODY) :NBINDS 0)))
    (WHEN (CDR BODY) (SETQ TLEVEL NIL))
    (DOLIST (ELT BODY)
      (WHEN (ATOM ELT)
	(P1TAGAD ELT MYPROGDESC)))
    (SETQ BODY (MAPCAR #'(LAMBDA (STMT)
			   (IF (ATOM STMT)
			       STMT
			       (P1 STMT)))
		       BODY))
    (IF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG MYPROGDESC)
	(LET ((FRAMEWORK
		(P1 `(BLOCK P1TAGBODY
		       (LET (P1TAGBODY P1TAGCATCHTAG)
			 (SETQ P1TAGCATCHTAG (VARIABLE-LOCATION P1TAGCATCHTAG))
			 (TAGBODY
			  P1TAGBODY
			     (SETQ P1TAGBODY
				   (*CATCH P1TAGCATCHTAG
				     (PROGN
				       (CASE P1TAGBODY
					 ((NIL) NIL)
					 . ,(LOOP FOR G IN *GOTAG-ENVIRONMENT*
						  WHEN (GOTAG-USED-IN-LEXICAL-CLOSURES-FLAG G)
						  COLLECT `(,(GOTAG-PROG-TAG G)
							    (GO-HACK ,G))))
				       (RETURN-FROM P1TAGBODY
					 P1TAGBODY-SUBSTITUTE-FOR))))
			     (GO P1TAGBODY))))))
	      TEM)
	  (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG MYPROGDESC)
		(CAR ALLVARS))
	  (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
		   (VAR-MISC (CAR ALLVARS)))
	  ;; Get the generated TAGBODY.
	  (SETQ TEM (CAR (LAST (CAR (LAST FRAMEWORK)))))
	  ;; Get the SETQ.
	  (SETQ TEM (CAR (NLEFT 2 TEM)))
	  ;; Get the RETURN-FROM.
	  (SETQ TEM (CAR (LAST (CAR (LAST (CAR (LAST TEM)))))))
	  (SETF (CAR (LAST TEM))
		`(TAGBODY ,*GOTAG-ENVIRONMENT* . ,BODY))
	  FRAMEWORK)
      `(TAGBODY ,*GOTAG-ENVIRONMENT* . ,BODY))))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1GO (FORM)
  (LET ((GOTAG (SYS:ASSOC-EQUAL (CADR FORM) *GOTAG-ENVIRONMENT*)))
    (IF (MEMQ GOTAG *OUTER-CONTEXT-GOTAG-ENVIRONMENT*)
	`(*THROW ,(TRY-REF-LEXICAL-HOME
		    (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG
		      (GOTAG-PROGDESC GOTAG)))
		 ',(CADR FORM))
      FORM)))

))

; From file OPEN.LISP OZ:<L.IO.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; OPEN  "

(DEFUN OPEN (FILENAME &REST KEYWORD-ARGS)
  "Open a file and return a stream.  FILENAME is a pathname or a string.
DIRECTION is :INPUT, :OUTPUT or :PROBE for non-data stream.
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
  (DECLARE (ARGLIST FILENAME &KEY (DIRECTION :INPUT) (ERROR T)
		    		  (CHARACTERS T) BYTE-SIZE NEW-FILE NEW-VERSION OLD-FILE 
				  FLAVOR LINK-TO INHIBIT-LINKS DELETED PRESERVE-DATES
			     &ALLOW-OTHER-KEYS))
  (FORCE-USER-TO-LOGIN)
  (IF (STREAMP FILENAME)
      (SETQ FILENAME (SEND FILENAME :PATHNAME)))
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME))
  (SETQ LAST-FILE-OPENED FILENAME)
  (IF (OR (NULL KEYWORD-ARGS)			;No args is good args
	  (NOT (NULL (CDR KEYWORD-ARGS))))
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ (GET (LOCF KEYWORD-ARGS) :ERROR) '(:RETRY :REPROMPT))
				  (FILENAME FILE-ERROR)
        (LEXPR-SEND FILENAME :OPEN FILENAME KEYWORD-ARGS))
    ;; Old Syntax.
    (DO ((KEYL (IF (AND (CAR KEYWORD-ARGS) (SYMBOLP (CAR KEYWORD-ARGS)))
		   (LIST (CAR KEYWORD-ARGS))
		 (CAR KEYWORD-ARGS))
	       (CDR KEYL))
	 (KEY)
	 (CHARACTERS T)
	 (DIRECTION :INPUT)
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
	   (COND (SUPER-IMAGE-P (%PUSH :SUPER-IMAGE) (%PUSH SUPER-IMAGE-P)))
	   (COND (RAW-P	      (%PUSH ':RAW)	    (%PUSH RAW-P)))
	   (%ACTIVATE-OPEN-CALL-BLOCK)))
      (SETQ KEY (CAR KEYL))
      (SELECTOR KEY STRING-EQUAL
	((:IN :READ) (SETQ DIRECTION :INPUT))
	((:OUT :WRITE :PRINT) (SETQ DIRECTION :OUTPUT))
	((:BINARY :FIXNUM) (SETQ CHARACTERS NIL))
	((:CHARACTER :ASCII) (SETQ CHARACTERS T))
	((:BYTE-SIZE) (SETQ KEYL (CDR KEYL)
			     BYTE-SIZE (CAR KEYL)))
	((:PROBE) (SETQ DIRECTION NIL
			 CHARACTERS NIL
			 ERROR-P (IF (NOT ERROR-P-SPECD) NIL ERROR-P)
			 ERROR-P-SPECD T))
	((:NOERROR) (SETQ ERROR-P NIL ERROR-P-SPECD T))
	((:ERROR) (SETQ ERROR-P T ERROR-P-SPECD T))
	((:RAW) (SETQ RAW-P T))
	((:SUPER-IMAGE) (SETQ SUPER-IMAGE-P T))
	((:DELETED) (SETQ DELETED-P T))
	((:TEMPORARY) (SETQ TEMPORARY-P T))
	((:BLOCK :SINGLE))			;Ignored for compatility with Maclisp
	(OTHERWISE (FERROR NIL "~S is not a known OPEN option" KEY))))))

))
