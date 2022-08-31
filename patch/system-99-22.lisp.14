;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.22
;;; Reason:
;;;  compiler flet p1 typo
;;;  opeEration-handled-p
;;;  sxhash
;;;  fasloading files with logical pathnames as their generic source
;;;  princ of characters should just write-char if no char-bits
;;;  aborting out of reading zmacs file does not leave the dead
;;;   buffer hanging about --- fix mly spazz
;;;  si::*read-single-colon-allow-internal*
;;;  some symbol-reading improvments
;;;  some reader #-macros improved
;;; Written 13-Feb-85 09:55:47 by Mly,
;;; while running on Lisp Machine Nine from band 3
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.20, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.

(eval-when (eval compile load)
(export (intern "CHAR-MOUSE-P" 'tv) 'tv)
(export (intern "MAKE-MOUSE-CHAR" 'tv) 'tv)
)

; From file OZ:OZ:<MLY.LL>FLAVOR.LISP.3 at 23-Feb-85 19:13:02
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(compiler:make-obsolete with-self-variables-bound "You really shouldn't get using this")
(compiler:make-obsolete with-self-accessible "You really shouldn't get using this")

))

; From file OZ:OZ:<MLY.LL>QCLUKE.LISP.3 at 23-Feb-85 19:20:54
#10R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defprop si::defconst-1 cw-first-arg-quoted cw-handler)

))

; From file OZ:OZ:<MLY.LL>ANALYZE.LISP.1 at 24-Feb-85 05:43:32
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; ANALYZE  "

(defun analyze-definition (analyze-object-name analyze-object-type
			   definition record-function)
  (declare (special analyze-object-name analyze-object-type))
  (and (eq analyze-object-type :function)
       (eq (car-safe definition) 'macro)
       (pop definition))
  (typecase definition
    (compiled-function (analyze-compiled-function definition record-function))
    (si:flavor (analyze-flavor definition record-function))
    (symbol (analyze-list definition record-function))
    (list (funcall (if (eq analyze-object-type :function)
		       #'analyze-lambda
		       #'analyze-list)
		   definition record-function))
    (select-method (analyze-list (%make-pointer dtp-list definition) record-function))
    (closure (analyze-definition analyze-object-name analyze-object-type
				 (closure-function definition) record-function))))

))

; From file OZ:OZ:<MLY.LL>QCP1.LISP.4 at 25-Feb-85 23:54:29
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(defun (:property flet p1) (form)
  (let* ((locals (mapcar #'(lambda (ignore) (gensym)) (cadr form))))
    ;; LOCALS are local variables that really hold the functions.
    (mapc #'(lambda (var fndef)
	      (putprop var (car fndef) 'local-function-name))
	  locals (cadr form))
    ;; P1 will translate any reference to a local function
    ;; into a FUNCALL to the corresponding variable.
    (p1 `(let ,(mapcar #'(lambda (var def)
			   `(,var #'(named-lambda . ,def)))
		       locals (cadr form))
	   (flet-internal ,(mapcar #'(lambda (var def)
				       (list (car def) var `(named-lambda . ,def)))
				   locals (cadr form))
			  . ,(cddr form)))
	t)))					;inhibit optimizations on this pass, since

))

; From file OZ:KANSAS:<L.SYS2>LMMAC.LISP.387 at 26-Feb-85 00:07:40
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFSUBST OPERATION-HANDLED-P (OBJECT &REST OPERATION)
  "Non-NIL if OBJECT has a method defined for OPERATION."
  (LEXPR-FUNCALL OBJECT :OPERATION-HANDLED-P OPERATION))

))

; From file OZ:OZ:<MLY.LL>QRAND.LISP.5 at 26-Feb-85 00:24:59
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; QRAND  "

(DEFUN SXHASH (X &OPTIONAL RANDOM-OBJECT-ACTION)
  "Return a hash code for object X.  EQUAL objects have the same hash code.
The hash code is always a positive fixnum.
Flavor instances and named structures may handle the :SXHASH operation
/(with one arg, passed along from RANDOM-OBJECT-ACTION) to compute their hash codes.
If RANDOM-OBJECT-ACTION is non-NIL, the ultimate default is to use the
object's address to compute a hash code.  This only happens for
objects which cannot be EQUAL unless they are EQ.
If RANDOM-OBJECT-ACTION is NIL, the hash code of an object does not
change even if it is printed out and read into a different system version."
  (MACROLET ((ROT-24-BIT (VALUE BITS)
	       (ONCE-ONLY (VALUE BITS)
		 `(DPB ,VALUE (BYTE (- 24. ,BITS) ,BITS)
		       (LSH ,VALUE (- ,BITS 24.))))))
    (COND ((SYMBOLP X) (%SXHASH-STRING (SYMBOL-NAME X) #o337))
	  ((STRINGP X) (%SXHASH-STRING X #o337))	;Ignores case!
	  ((OR (INTEGERP X) (CHARACTERP X))
	   (IF (MINUSP X) (LOGXOR (LDB 23. X) 1) (LDB 23. X)))
	  ((CONSP X)		;Rotate car by 11. and cdr by 7, but do it efficiently
	   (DO ((ROT 4) (HASH 0) Y (X X))
	       ((ATOM X)
		(OR (NULL X)
		    (SETQ HASH (LOGXOR (ROT-24-BIT (SXHASH X RANDOM-OBJECT-ACTION)
						   (IF (< ROT 4) (+ ROT #o20) (- ROT 4)))
				       HASH)))
		(LOGAND #o37777777 (IF (LDB-TEST (BYTE 1 23.) HASH) (LOGXOR HASH 1) HASH)))
	     (SETQ Y (CAR X) X (CDR X))
	     (UNLESS (< (SETQ ROT (+ ROT 7)) 24.)
	       (SETQ ROT (- ROT 24.)))
	     (SETQ HASH (LOGXOR (ROT-24-BIT
				  (COND ((SYMBOLP Y) (%SXHASH-STRING (SYMBOL-NAME Y) #o337))
					((STRINGP Y) (%SXHASH-STRING Y #o337))
					((OR (INTEGERP X) (CHARACTERP X))
					 (LDB 24. Y))
					(T (SXHASH Y RANDOM-OBJECT-ACTION)))
				  ROT)
				HASH))))
	  ((FLONUMP X) (LOGXOR (%P-LDB-OFFSET #o0027 X 1)
			       (%P-LDB-OFFSET #o2701 X 1)
			       (%P-LDB #o0022 X)))
	  ((AND (TYPEP X 'INSTANCE)
		(SEND X :SEND-IF-HANDLES :SXHASH RANDOM-OBJECT-ACTION)))
	  ((AND (TYPEP X 'NAMED-STRUCTURE)
		(MEMQ :SXHASH (NAMED-STRUCTURE-INVOKE :WHICH-OPERATIONS X)))
	   (NAMED-STRUCTURE-INVOKE :SXHASH X RANDOM-OBJECT-ACTION))
	  ((OR RANDOM-OBJECT-ACTION
	       (SMALL-FLOATP X))
	   (SETQ X (%POINTER X))
	   (LET ((Y (LOGXOR (LDB (- %%Q-POINTER 24.) X)
			    (LSH X (- 24. %%Q-POINTER)))))
	     (LOGAND #o37777777
		     (IF (MINUSP X) (LOGXOR Y 1) Y))))
	  ((ARRAYP X)
	   (ARRAY-ACTIVE-LENGTH X))
	  (T 0))))				;0 for things that can't be read

))

; From file OZ:KANSAS:<L.SYS>QFASL.LISP.462 at 26-Feb-85 01:15:05
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; QFASL  "

(DEFUN FASL-OP-FILE-PROPERTY-LIST ()
  (LET ((PLIST (FASL-NEXT-VALUE)))
    (SETQ FASL-FILE-PLIST PLIST)
    ;; Make the source file really correspond to where things were compiled from.
    (AND FDEFINE-FILE-PATHNAME
	 (LET ((SOURCE-PATHNAME (GETF PLIST :SOURCE-FILE-GENERIC-PATHNAME)))
	   (COND ((AND SOURCE-PATHNAME (NOT (STRINGP FDEFINE-FILE-PATHNAME)))
		  ;; If opened via a logical host, should record with that host in, even if
		  ;; not compiled that way.
		  (UNLESS (TYPEP SOURCE-PATHNAME 'FS::LOGICAL-PATHNAME)
		    (SETQ SOURCE-PATHNAME (SEND FDEFINE-FILE-PATHNAME
						:BACK-TRANSLATED-PATHNAME SOURCE-PATHNAME)))
		  (SETQ FDEFINE-FILE-PATHNAME (SEND SOURCE-PATHNAME :GENERIC-PATHNAME))
		  (SETQ FASL-GENERIC-PLIST-RECEIVER FDEFINE-FILE-PATHNAME)))))
    (DO ((PLIST PLIST (CDDR PLIST)))
	((NULL PLIST))
      (SEND FASL-GENERIC-PLIST-RECEIVER :PUTPROP (CADR PLIST) (CAR PLIST))
;      (WHEN PRINT-LOADED-FORMS
;	(PRINT `(SEND ',FASL-GENERIC-PLIST-RECEIVER :PUTPROP
;		      ',(CADR PLIST) ',(CAR PLIST))))
      (AND ACCUMULATE-FASL-FORMS
	   (PUSH `(SEND ',FASL-GENERIC-PLIST-RECEIVER :PUTPROP
			',(CADR PLIST) ',(CAR PLIST))
		 LAST-FASL-FILE-FORMS))))
  (AND FASLOAD-FILE-PROPERTY-LIST-FLAG (SETQ FASL-RETURN-FLAG T))) ;Cause FASL-WHACK to return

))

; From file OZ:KANSAS:<L.IO>PRINT.LISP.182 at 26-Feb-85 20:46:54
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PPRINT (OBJECT &OPTIONAL STREAM)
  "Print OBJECT on STREAM, with quoting and with extra whitespace to make it look pretty.
Returns zero values."
  (LET ((*PRINT-ESCAPE* T)
	(*PRINT-PRETTY* T))
    (GRIND-TOP-LEVEL OBJECT NIL (DECODE-PRINT-ARG STREAM)))
  (VALUES))

))

; From file OZ:KANSAS:<L.IO>PRINT.LISP.182 at 26-Feb-85 20:50:57
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(defun print-character (char stream fastp
			 &aux (*print-base* 10.) (*print-radix* nil) (*nopoint t))

  (declare (ignore fastp))
  (when *print-escape*
    (send stream :string-out (car (pttbl-character *readtable*)))
    (if (not (zerop (char-font char)))
	(prin1 (char-font char) stream))
    (send stream :string-out (cdr (pttbl-character *readtable*))))
  ;;>> ignore printing font of character when *print-escape* is nil for now...
  (let ((bits (char-bits char))
	(code (char-code char)))
    (send stream :string-out
	  (nth bits
	       '("" "c-" "m-" "c-m-"
		 "s-" "c-s-" "m-s-" "c-m-s-"
		 "h-" "c-h-" "m-h-" "c-m-h-"
		 "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")))
    (cond ((tv:char-mouse-p char)
	   (send stream :string-out "Mouse-")
	   (send stream :string-out (nth (ldb %%kbd-mouse-button char)
					 '("Left-" "Middle-" "Right-")))
	   (prin1 (1+ (ldb %%kbd-mouse-n-clicks char)) stream))
	  (t
	   (let (chname)
	     (if (or *print-escape*
		     ( bits 0))
		 (setq chname (format::ochar-get-character-name code)))
	     (if chname
		 (send stream :string-out chname)
	       (and *print-escape*
		    ( bits 0)
		    (character-needs-quoting-p code)
		    (send stream :tyo (pttbl-slash *readtable*)))
	       (send stream :tyo code)))))))

))

; From file OZ:KANSAS:<L.ZWEI>ZMACS.LISP.520 at 26-Feb-85 21:11:00
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN REVERT-FILE-BUFFER (BUFFER PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG
			   &AUX GENERIC-PATHNAME PATHNAME-STRING TRUENAME NEW-MODE)
  (WHEN (AND (NULL (BUFFER-FILE-ID BUFFER)) (NULL PATHNAME))
    (BARF "The buffer ~A is not associated with a file." (BUFFER-NAME BUFFER)))
  (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
    (EDITOR-FILE-NAME PATHNAME))
  (WHEN CONNECT-FLAG
    (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
    (SETF (BUFFER-PATHNAME BUFFER) PATHNAME))
  (SETQ GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
  (WITH-OPEN-FILE-CASE (STREAM PATHNAME)
    (:NO-ERROR
     (SETQ TRUENAME (SEND STREAM :TRUENAME))
     (WHEN (MEMQ (SEND PATHNAME :TYPE) '(NIL :UNSPECIFIC))
       (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
	 (EDITOR-FILE-NAME
	   (IF (EQUALP (SEND TRUENAME :NAME) (SEND PATHNAME :NAME))
	       ;; This is in case user reads FOO > from an ITS, and it is reall FOO BAR.
	       (SEND PATHNAME :NEW-TYPE (SEND TRUENAME :TYPE))
	     ;; This case if user read FOO BAR from an LMFILE, and truename is FOO|BAR.
	     ;; Or if user reads FOO BAR from an ITS and it is a link to UGH QUUX.
	     PATHNAME))))
     (WHEN CONNECT-FLAG
       (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
       (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
       (SIMILAR-BUFFER-FILES-WARNING BUFFER))
     (WHEN (NOT QUIETLY-FLAG)
       (FORMAT *QUERY-IO* "~&Reading ~A" TRUENAME)
       (LET ((THIS-VERSION (SEND TRUENAME :VERSION))
	     (INSTALLED-TRUENAME (FILE-LOADED-TRUENAME TRUENAME))
	     INSTALLED-VERSION)
	 (AND INSTALLED-TRUENAME
	      (NUMBERP THIS-VERSION)
	      (NUMBERP (SETQ INSTALLED-VERSION (SEND INSTALLED-TRUENAME :VERSION)))
	      ( INSTALLED-VERSION THIS-VERSION)
	      (FORMAT *QUERY-IO* " (installed version is ~D)" INSTALLED-VERSION))))
     (FS:READ-ATTRIBUTE-LIST BUFFER STREAM)
     ;; Forget (and thereby override) and previouse Set Package in this buffer.
     (SETF (BUFFER-PACKAGE BUFFER) NIL)
     ;; And recompute from latest attribute list.
     (INITIALIZE-BUFFER-PACKAGE BUFFER)
     (UNLESS (SEND BUFFER :GET-ATTRIBUTE ':MODE)
       (SEND BUFFER :SET-ATTRIBUTE ':MODE
				   (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
							    FS:*FILE-TYPE-MODE-ALIST*))
				       *DEFAULT-MAJOR-MODE*)))
     (SETQ NEW-MODE (OR (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE ':MODE))
			'FUNDAMENTAL-MODE))
     (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
       (IF (EQ BUFFER *INTERVAL*)
	   (COMPUTE-BUFFER-PACKAGE BUFFER))
       (AND NEW-MODE (SEND BUFFER :SET-MAJOR-MODE NEW-MODE)))
     (PRESERVE-BUFFER-POINT (BUFFER)
       (WITH-READ-ONLY-SUPPRESSED (BUFFER)
	 (LET ((*BATCH-UNDO-SAVE* T))		;Don't save all this for undo!
	   (DISCARD-UNDO-INFORMATION BUFFER)
	   (DELETE-INTERVAL BUFFER)
	   (SETF (BUFFER-TICK BUFFER) (TICK))	;For SECTIONIZE-BUFFER
	   (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
	   (LET ((FONTS (SET-BUFFER-FONTS BUFFER))
		 FONTS-P)
	     (SETQ FONTS-P (OR (CDR FONTS) (SEND BUFFER :GET-ATTRIBUTE ':DIAGRAM)))
	     (WHEN SELECT-FLAG
	       (SEND BUFFER :ACTIVATE)
	       (MAKE-BUFFER-CURRENT BUFFER)
	       ;; If it is requested, read in the first screenful and then redisplay.
	       (DOTIMES (I (+ 5 (WINDOW-N-PLINES *WINDOW*)))
		 (MULTIPLE-VALUE-BIND (LINE EOFFLG)
		     (SEND STREAM :LINE-IN LINE-LEADER-SIZE)
		   (WHEN LINE
		     (INSERT-LINE-WITH-LEADER LINE
					      (BP-LINE (INTERVAL-LAST-BP BUFFER))))
		   (IF EOFFLG (RETURN))))
	       (REDISPLAY *WINDOW* :START (INTERVAL-FIRST-BP BUFFER) NIL))
	     (IF (NOT CONNECT-FLAG)
		 (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
	       (IF (EQ CONNECT-FLAG 'NOSECTIONIZE)
		   (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
		 (SECTIONIZE-FILE-BUFFER BUFFER *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS
					 NIL NIL
					 STREAM FONTS-P))
	       (SET-BUFFER-FILE-ID BUFFER (SEND STREAM :INFO))
	       (DOLIST (WINDOW (SEND BUFFER :WINDOWS))
		 (AND FONTS
		      (REDEFINE-FONTS WINDOW
				      FONTS (SEND BUFFER :GET-ATTRIBUTE ':VSP)))
		 (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
						    (SEND BUFFER :GET-ATTRIBUTE ':BACKSPACE))
		 (REDEFINE-WINDOW-TAB-NCHARS WINDOW
					     (SEND BUFFER :GET-ATTRIBUTE ':TAB-WIDTH))))
	     (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
	     (NOT-MODIFIED BUFFER)))))
     (UNLESS SELECT-FLAG			;else already done above
       (SEND BUFFER :ACTIVATE))
     (UNLESS QUIETLY-FLAG
       (LET ((NCHARS (SEND-IF-HANDLES STREAM :READ-POINTER)))
	 (COND ((NULL NCHARS)
		(FORMAT *QUERY-IO* " -- done."))
	       ((< NCHARS 5000.)
		(FORMAT *QUERY-IO* " -- ~D characters." NCHARS))
	       (T (FORMAT *QUERY-IO* " -- ~DK characters." (ROUND NCHARS 1024.)))))))
    (FS:FILE-NOT-FOUND
     (WHEN *FIND-FILE-NOT-FOUND-IS-AN-ERROR* (BARF STREAM))
     (OR QUIETLY-FLAG (FORMAT *QUERY-IO* "(New File)"))
     (LET ((*BATCH-UNDO-SAVE* T))
       (DISCARD-UNDO-INFORMATION BUFFER)
       (DELETE-INTERVAL BUFFER))
     (AND CONNECT-FLAG (SET-BUFFER-FILE-ID BUFFER T))
     (SEND BUFFER :SET-ATTRIBUTE ':MODE
				 (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
							  FS:*FILE-TYPE-MODE-ALIST*))
				     *DEFAULT-MAJOR-MODE*))
     (SETF (BUFFER-PACKAGE BUFFER) (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*)))
     (LET ((MODE (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE :MODE))))
       (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
	 (IF (EQ BUFFER *INTERVAL*) (COMPUTE-BUFFER-PACKAGE BUFFER))
	 (AND MODE (SEND BUFFER :SET-MAJOR-MODE MODE)))))
    (FS:FILE-ERROR (BARF STREAM)))
  (SETF (BUFFER-TICK BUFFER) (TICK)))		;Buffer is same as file

))

; From file OZ:OZ:<MLY.LL>READ.LISP.2 at 27-Feb-85 03:37:47
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  "

(defun read-error (format-string &rest format-args)
  (apply #'cerror :no-action nil 'read-error-1 format-string format-args))

))

; From file OZ:OZ:<MLY.LL>READ.LISP.2 at 27-Feb-85 03:36:49
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  " 

(defvar *inhibit-reader-symbol-substitution* nil)
(defvar *read-single-colon-allow-internal* t
  "T means that a single colon prefix will find an internal symbol in a package.
:WARN means to print a message on *ERROR-OUTPUT* but still find the symbol.
NIL means to get an error.")

(DEFUN XR-READ-SYMBOL (STREAM STRING)
  (DECLARE (IGNORE STREAM))			;doesn't do any additional reading
  (LET-IF (VARIABLE-BOUNDP *PACKAGE*)
	  ((*PACKAGE* *PACKAGE*))
    (WHEN (VARIABLE-BOUNDP *PACKAGE*)
      (UNLESS (OR *PACKAGE* *READ-SUPPRESS*)
	(READ-ERROR NIL "~S is now ~S, and symbol ~S has no package prefix."
		    '*PACKAGE 'NIL STRING)
	(SETQ *PACKAGE* PKG-USER-PACKAGE)))
    (IF *READ-SUPPRESS*
	(VALUES NIL 'SYMBOL)
      (MULTIPLE-VALUE-BIND (READ FLAG)
	  (FUNCALL READ-INTERN-FUNCTION STRING)
	(DECLARE (IGNORE FLAG))
	(UNLESS *INHIBIT-READER-SYMBOL-SUBSTITUTION*
	  (LET ((SUBST (ASSQ READ (RDTBL-SYMBOL-SUBSTITUTIONS *READTABLE*))))
	    (AND SUBST (SETQ READ (CDR SUBST)))))
	(VALUES READ 'SYMBOL)))))

))

; From file OZ:OZ:<MLY.LL>READ.LISP.2 at 27-Feb-85 03:37:02
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  "

;;; FOO: switches us to the package associated with the string "FOO"
;;; FOO:: is Common Lisp for "allow internal symbols", but we usually do that,
;;;  since *read-single-colon-allow-internal* is usually T
(DEFUN (:PROPERTY PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING IGNORE)
  (PROG (THING PK
	 ;; Help un-screw the user if *PACKAGE* gets set to NIL.
	 (*PACKAGE* (OR *PACKAGE* PKG-USER-PACKAGE))
	 INTERNAL-OK ENTIRE-LIST-PREFIXED)
	;; Gobble the second colon, if any, and set flag if found.
	;; Note that we do not, currently, DO anything with the flag!
	(MULTIPLE-VALUE-BIND (CH NUM REAL-CH)
	    (XR-XRTYI STREAM NIL T)
	  (IF (= CH #/:)
	      (SETQ INTERNAL-OK T)
	    (IF (= CH #/()
		(SETQ ENTIRE-LIST-PREFIXED T))
	    (XR-XRUNTYI STREAM REAL-CH NUM)))
	;; Try to find the package.
	;;don't try to find packages if we're not interning -- eg #+slime (dis:foo)
	(UNLESS *READ-SUPPRESS*
	  (DO ((STRING1 (OR STRING "")))
	      ((SETQ PK (FIND-PACKAGE STRING1 *PACKAGE*)))
	    ;; Package not found.
	    (SIGNAL-PROCEED-CASE ((PKG) 'READ-PACKAGE-NOT-FOUND
					"Package ~S does not exist."
					STRING1)
	      (:NO-ACTION
	       (RETURN))
	      (:NEW-NAME
	       (LET ((*PACKAGE* PKG-USER-PACKAGE))
		 (SETQ STRING1 (STRING (READ-FROM-STRING PKG)))))
	      (:CREATE-PACKAGE
	       (OR (FIND-PACKAGE STRING1 *PACKAGE*)
		   (MAKE-PACKAGE STRING1))))))
	(OR PK (SETQ PK PKG-USER-PACKAGE))
	(WHEN STRING (RETURN-READ-STRING STRING))
	(LET ((*PACKAGE* PK)
	      (*INHIBIT-READER-SYMBOL-SUBSTITUTION* T)
	      (READ-INTERN-FUNCTION (COND ((OR (AND (PKG-AUTO-EXPORT-P PK)
						    (PACKAGE-USED-BY-LIST PK))
					       (PKG-READ-LOCK-P PK))
					   'READ-INTERN-SOFT)
					  ((OR ENTIRE-LIST-PREFIXED (EQ PK *PACKAGE*))
					   ;; Here for, e.g., SI: while in SI already.
					   ;; Also here for ZWEI:(BP-LINE (POINT));
					   ;; such constructs are not valid Common Lisp
					   ;; so let's keep their meaning the same.
					   READ-INTERN-FUNCTION)
					  ((OR INTERNAL-OK
					       (PKG-AUTO-EXPORT-P PK)
					       (EQ *READ-SINGLE-COLON-ALLOW-INTERNAL* T))
					   'INTERN)
					  (T
					   'READ-PACKAGE-PREFIX-EXTERNAL-INTERN))))
	  (SETQ THING (INTERNAL-READ STREAM T NIL T)))
	(RETURN (VALUES THING
			(TYPE-OF THING)
			T))))	;T means we already did RETURN-READ-STRING
))

; From file OZ:OZ:<MLY.LL>READ.LISP.3 at 27-Feb-85 10:02:21
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN (:PROPERTY MULTI-DOT STANDARD-READ-FUNCTION) (STREAM STRING)
  (READ-ERROR "The illegal token ~S was encountered on input by the reader." STRING)
  (XR-READ-SYMBOL STREAM STRING))

;>> this should signal an error with proceed-types to offering to export the symbol
(defun read-package-prefix-external-intern (string)
  (flet ((foo (&rest args)
	   (cond ((eq *read-single-colon-allow-internal* ':warn)
		  (fresh-line *error-output*)
		  (apply #'format *error-output* args))
		 (t (apply #'read-error args)))))
    (multiple-value-bind (sym flag pkg)
	(intern-soft string *package*)
      (case flag
	((nil)
	 (foo "Reference to nonexistent symbol ~S in package ~A using a single colon prefix."
	      string *package*)
	 (intern string pkg))
	(:internal
	 (foo "Reference to non-external symbol ~S in package ~A using a single colon prefix."
	      sym *package*)
	 (values sym flag pkg))
	(t					;:external or :inherited
	 (values sym flag pkg))))))
))

; From file OZ:OZ:<MLY.LL>READ.LISP.3 at 27-Feb-85 10:10:51
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  "

;>> *** CONS ***
(defun xr-#/(-macro (stream ignore &optional length)
  (let ((elements (read-delimited-list #/) stream t)))
    (if *read-suppress*
	nil
      (cond ((and length (plusp length) (null elements))
	     (read-error "The construct #~D() is illegal; at least one element must be given."
			 length)
	     (setq length nil))
	    ((and length (< length (length elements)))
	     (cerror :no-action nil 'read-error-1
		     "Length of elements list supplied, ~D, is more than specified length in #~D(...)"
		     (length elements) length)
	     (setq length nil)))
      (if (null length)
	  (apply #'vector elements)
	(let ((vector (make-array (or length (length elements))
				  :initial-element (car (last elements)))))
	  (replace vector elements)
	  vector)))))

;>> *** CONS ***
(defun xr-#*-macro (stream ignore &optional length)
  (if *read-suppress*
      (progn (internal-read stream t nil t) nil)
    (do ((bit-vector (make-array (or length 32.) :element-type 'bit :fill-pointer 0))
	 elt)
	(())
      (multiple-value-bind (char index actual) (xr-xrtyi stream nil t)
	(case char
	  ((#/0 #/1)
	   (setq elt (if (= char #/0) 0 1))
	   (cond ((null length)
		  (vector-push-extend elt bit-vector))
		 ((vector-push elt bit-vector))
		 (t (read-error
		      "Number of data bits exceeds specified length in /"#*/" bit vector construct.")
		    (vector-push-extend elt bit-vector)
		    (setq length nil))))
	  (t
	   (xr-xruntyi stream actual index)
	   (if (and length (plusp length) (zerop (fill-pointer bit-vector)))
	       (read-error "The construct #~D* is illegal; at least one bit must be given."
			   length))
	   (if length
	       ;; VECTOR-PUSH returns NIL when the fill pointer is at the end of the array.
	       (do () ((vector-push elt bit-vector))))
	   (return (prog1 (copy-seq bit-vector)
			  (return-array bit-vector)
			  (setq bit-vector nil)))))))))


;>> *** CONS ***
;>> ack! this need MUCH better error-reporting when the dimensionalities of the read-in
;>> sequences aren't appropriate or sonsistent.
(defun xr-#a-macro (stream ignore &optional rank)
  (if *read-suppress*
      (progn (internal-read stream t nil t) nil)
    (typecase rank
      (null
       (read-error "An array-rank must be given as an argument to /"#<rank>A/""))
      ((member 0)
       (values (make-array nil :initial-element (internal-read stream t nil t))))
      ((fixnum 0 #.array-rank-limit)
       (let (dimensions (sequences (internal-read stream t nil t)))
	 (do ((dim 0 (1+ dim))
	      (stuff sequences (elt stuff 0)))
	     ((= dim rank))
	   (push (length stuff) dimensions))
	 (values (make-array (nreverse dimensions) :initial-contents sequences))))
      (t
       (read-error "~D is too large an array rank; it must be < ~D." rank array-rank-limit)
       (internal-read stream t nil t)
       nil))))

(defun xr-#c-macro (stream ignore &optional arg)
  (block nil
    (cond (*read-suppress*
	   (internal-read stream t nil t)
	   (return nil))
	  (arg
	   (read-error "An argument, ~D, was given to /"#C/"" arg)))
    (when ( (xr-xrtyi stream t) #/()
      (read-error "/"#C/" must be followed by a list.")
      (return 0))
    (let (n1 n2)
      (setq n1 (internal-read stream t nil t))
      (unless (typep n1 'non-complex-number)
	(read-error "~S cannot be the real component of a complex number" n1)
	(setq n1 0))
      (setq n2 (internal-read stream t nil t))
      (unless (typep n2 'non-complex-number)
	(read-error "~S cannot be the complex component of a complex number" n2)
	(setq n2 0))
      (if (not (null (read-delimited-list #/) stream t)))
	  (read-error "/"#C/" must be followed by a list of exactly 2 numbers"))
      (complex n1 n2))))


))

; From file OZ:OZ:<MLY.LL>READ.LISP.3 at 27-Feb-85 10:10:57
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  "
(defvar *xr-label-bindings*)
(forward-value-cell '*xr-label-bindings* 'xr-label-bindings)


;;;>> Winner of the foobar award for the most gratuitous and misplaced effort of 1985.
;;;>> Am I wasting my time yet?
;(defun xr-#s-macro (stream ignore &optional arg)
;  (block nil
;    (cond (*read-suppress*
;	   (internal-read stream t nil t)
;	   (return nil))
;	  (arg
;	   (read-error "An argument, ~D, was given to /"#S/"" arg)))
;    (when ( (xr-xrtyi stream t) #/()
;      (read-error "~/"#S/" must be followed by a list specifying a structure's type and components.~")
;      (return nil))
;    (let ((name (internal-read stream t nil t))
;	  desc constructor slot-alist long (kludge 0) kwd val)
;      (with-stack-list (args nil)
;	(tagbody
;	    (cond ((not (symbolp name))
;		   (read-error
;		     "The first element of the /"#S(...)/" list, ~S, is not a structure name"
;		     name))
;		  ;; don't use get-defstruct-description as that errs
;		  ((null (setq desc (get name 'defstruct-description)))
;		   (read-error
;		     "~S is not the name of a known structure" name))
;		  ((null (dolist (c (defstruct-description-constructors desc))
;			   (if (or (null (cdr c))
;				   (and (stringp (cadr c)) (null (cddr c))))
;			       (return (setq constructor (car c))))))
;		   (read-error
;		     "The structure-type ~S does not have a constructor ~@
;			  which takes as arguments pairs of alternating slot-names and values"
;		     name))
;		  (t (go win)))
;	 lose
;	    (read-delimited-list #/) stream t)
;	    (return nil)
;	 win
;	    (setq slot-alist (defstruct-description-slot-alist desc))
;	    (if (> (length slot-alist) 60.) (setq long t args nil))
;	 slot
;	    (multiple-value-bind (a b c) (xr-xrtyi stream t)
;	      (if (= a #/))
;		  (go done)
;		(xr-xruntyi stream c b)))
;	    (setq kwd (internal-read stream t nil t))
;	    (cond ((not (symbolp kwd))
;		   (read-error
;		     "~S should have been the name of a structure slot" kwd)
;		   (go lose))
;		  ((not (ass #'string=		;commonlisp braindeath
;			     (setq kwd (intern (symbol-name kwd) pkg-keyword-package))
;			     slot-alist))
;		   (read-error
;		     "~S is not the name of a slot in a ~S structure" kwd name)
;		   (go lose)))
;	    (setq val (internal-read t nil t))
;	    (unless (getf args kwd)
;	      (cond (long
;		     (push val args)		;don't use list* since we nreverse
;		     (push kwd args))
;		    (t
;		     (%push kwd)
;		     (%push val)
;		     (%p-dpb-offset cdr-next %%q-cdr-code args (prog1 kludge (incf kludge)))
;		     (%p-dpb-offset cdr-next %%q-cdr-code args (prog1 kludge (incf kludge)))
;		     (%p-dpb-offset cdr-nil %%q-cdr-code args kludge))))
;	    (go slot)
;	 done
;	    (setq args (if long (nreverse args) (cdr args)))
;	    (return (if (macro-function constructor)
;			(eval (cons constructor args))
;		      (apply constructor args))))))))
))

; From file OZ:OZ:<MLY.LL>READ.LISP.3 at 27-Feb-85 10:23:28
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (LET ((CH (XR-XRTYI STREAM NIL T)))			;Skip the / that follows.
    (IF ( CH #//)
	(READ-ERROR "The character, /"~C/" following /"#/" was not /"///"" ch))
    (XR-CL-#\-MACRO STREAM NIL ARG)))

(defun xr-cl-#\-macro (stream ignore &optional font)
  (if (null font) (setq font 0))
  (if (< font char-font-limit)
      (%make-pointer dtp-character
		     (%logdpb font %%ch-font
			      (xr-#\-macro stream nil nil)))
    (read-error
      "The font number to /"#\/", ~D, is larger than the maximum character font value ~D"
      font (1- char-font-limit))
    #/null))

))

; From file OZ:KANSAS:<L.SYS>CLPACK.LISP.152 at 28-Feb-85 08:23:29
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun pkg-keyword-store (hash sym pkg &aux index)
  (set sym sym)					;make keywords self-evaluate
  (setq index (nth-value 3 (pkg-intern-store hash sym pkg)))
  (setf (pkg-slot-code pkg index)
	(pkg-make-code 1 (pkg-slot-code pkg index)))
  (values sym :external pkg))

))

; From file OZ:KANSAS:<L.SYS>CLPACK.LISP.152 at 28-Feb-85 08:28:22
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun export-all-symbols-of-package-without-checking-for-conflicts (pkg)
  (setq pkg (pkg-find-package pkg))
  (dotimes (index (pkg-number-of-slots pkg))
    (let ((code (pkg-slot-code pkg index)))
      (when (pkg-code-valid-p code)
	(setf (pkg-slot-code pkg index) (pkg-make-code 1 code))))))

))

si::(export-all-symbols-of-package-without-checking-for-conflicts pkg-keyword-package)
si::(setf (pkg-new-symbol-function pkg-keyword-package) 'pkg-keyword-store)
si::(setf (pkg-auto-export-p pkg-keyword-package) t)

