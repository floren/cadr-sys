;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Written 6-Jun-84 19:46:17 by Mly,
;;; Reason: metering warns if data is lost due to exceeding size of disk partition
;;; reading complexnums bug
;;; c-m-x untabify bug with fonts of widely differing widths
;;; while running on Lisp Machine Four from band 2
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.60, CADR 3.6, ZMail 53.17, MIT-Specific 22.1, microcode 309.


(eval-when (load compile eval)
  (globalize (intern "PACKAGE-NAME-CONFLICT" "SI") "SYS")
  (globalize (intern "ZERO-LOG" "SI") "SYS")
  (globalize (intern "NAME-CONFLICT" "SI") "SYS")
  (globalize (intern "ILLEGAL-EXPT" "SI") "SYS"))

; From file METER.LISP PS:<L.IO1> OZ:
#8R METER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "METER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; METER  "

;;; Analysis driver function
(DEFUN ANALYZE (&REST OPTIONS &AUX RET-INFO INFO (FLUSH-INFO T) CLOSE-STREAM
		(STREAM *STANDARD-OUTPUT*))
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
		       STREAM (OPEN (CADR L) :DIRECTION :OUTPUT :CHARACTERS T)))
	  (:BUFFER (SETQ STREAM (ZWEI:INTERVAL-STREAM (ZWEI:FIND-BUFFER-NAMED (CADR L) T))))
	  (:INFO (SETQ INFO (CADR L) FLUSH-INFO NIL))
	  (:ANALYZER (SETQ EVENT-TABLE (IF (TYPEP (CADR L) 'EVENT-TABLE)
					   (CADR L)
					 (GET (CADR L) 'EVENT-TABLE))))
	  (:RETURN (SETQ RET-INFO (CADR L) FLUSH-INFO NIL))
	  (OTHERWISE (PUTPROP (LOCF OPT-LIST) (CADR L) (CAR L)))))
      (IF (ZEROP %METER-DISK-COUNT)
	  (FORMAT STREAM "~&Note: ~Either you have not done METER:RESET, or else the disk metering partition has overflowed!
  If that is the case, then some information has been lost,
  and more recent metering events will not have been recorded.~~%"))
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

; From file NUMDEF.LISP PS:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defsubst %rational-numerator (number)
  (%p-contents-offset number 1))

))

; From file NUMDEF.LISP PS:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defsubst %rational-denominator (number)
  (%p-contents-offset number 2))

))

; From file NUMDEF.LISP PS:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defsubst %complex-real-part (number)
  (%p-contents-offset number 1))

))

; From file NUMDEF.LISP PS:<L.SYS2> OZ:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; NUMDEF  "

(defsubst %complex-imag-part (number)
  (%p-contents-offset number 2))

))

; From file rat#>  FC: mly
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun (complex standard-read-function) (stream string &aux complex-start (zero 0))
  stream
  (do ((i 1 (1+ i)))
      ((= i (length string)))
    (when (and (mem #'= (aref string i) '(#/+ #/-))
	       (not (alpha-char-p (aref string (1- i)))))
      (return (setq complex-start i))))
  (values
    (complex
      (cond (complex-start (with-input-from-string (strm string zero complex-start)
			     (xr-read-thing strm)))
	    (t (setq complex-start 0)))
      (with-input-from-string (strm string complex-start (1- (string-length string)))
	(xr-read-thing strm)))
    'complex))

))


; From file INDENT.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; INDENT  "

(DEFUN TAB-CONVERT (BP-BEFORE BP-AFTER)
  "Convert a tab to the right number of spaces, preserving the font.
BP-BEFORE and BP-AFTER should be temporary BPs to before and after the tab character."
  (LET ((INDENT-BEFORE (BP-VIRTUAL-INDENTATION BP-BEFORE))
	(INDENT-AFTER (BP-VIRTUAL-INDENTATION BP-AFTER))
	(*FONT* (LDB %%CH-FONT (BP-CHAR BP-BEFORE)))
	SPACE NSPACES)
    (IF (BP-STATUS BP-BEFORE)
	(FERROR NIL "~S is not a temporary BP." BP-BEFORE))
    (IF (BP-STATUS BP-AFTER)
	(FERROR NIL "~S is not a temporary BP." BP-AFTER))
    (SETQ SPACE (IN-CURRENT-FONT #\SP))
    (SETQ NSPACES (FLOOR (- INDENT-AFTER INDENT-BEFORE) (FONT-SPACE-WIDTH)))
    (MUNG-BP-LINE-AND-INTERVAL BP-BEFORE)
    (ASET SPACE (BP-LINE BP-BEFORE) (BP-INDEX BP-BEFORE))
    (WHEN  (PLUSP NSPACES)
      (INSERT-CHARS BP-BEFORE SPACE (1- NSPACES))
      (MOVE-BP BP-AFTER (BP-LINE BP-AFTER) (+ (BP-INDEX BP-AFTER) NSPACES -1)))
    BP-AFTER))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun numerator (x)
  "Return the numerator of X, which must be rational.
On integers, this is the identity function."
  (check-type x rational)
  (if (integerp x) x
    (%rational-numerator x)))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun denominator (x)
  "Return the denominator of X, which must be rational.
On integers, this returns 1."
  (check-type x rational)
  (if (integerp x) 1
    (%rational-denominator x)))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun realpart (x)
  "Return the real part of a complex number.  The real part of a real number is itself."
  (check-type x number)
  (if (complexp x)
      (%complex-real-part x)
    x))

))

; From file RAT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defun imagpart (x)
  "Return the imaginary part of a complex number, or 0 if given a real number."
  (check-type x number)
  (if (complexp x)
      (%complex-imag-part x)
    (- x x)))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-NAMESTRING)
	   (HOST-SPECIFIED-P NAMESTRING &OPTIONAL (START 0) END
	    &AUX (WILD-STRINGS `(,(STRING #/BREAK))))
  (DECLARE (VALUES DEVICE DIRECTORY NAME TYPE VERSION
		   DEVICE-SPECIFIED-P DIRECTORY-SPECIFIED-P NAME-SPECIFIED-P TYPE-SPECIFIED-P
		   VERSION-SPECIFIED-P))
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (LET* ((DIR-DELIM-ALIST (SEND SELF :DIRECTORY-DELIMITERS))
	 (ALL-DELIMS (NCONC (MAPCAR #'CAR DIR-DELIM-ALIST) '(#/: #/. #/; #/SP))))
    (DO ((IDX (OR (STRING-SEARCH-NOT-CHAR #/SP NAMESTRING START END) END))
	 (TEM) (TEM1) (DELIM)
	 (DIR-DELIM)
	 (DEV)
	 (DIR) (NAM) (TYP) (VER)
	 DEV-SPECIFIED-P NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P)
	(( IDX END)
	 (IF (EQUAL TYP "") (SETQ TYP :UNSPECIFIC))
	 (IF (EQUAL NAM "") (SETQ NAM NIL))
	 (SETQ DEV (OR DEV (IF HOST-SPECIFIED-P (SEND SELF :PRIMARY-DEVICE))))
	 (VALUES DEV DIR NAM TYP VER
		 DEV-SPECIFIED-P DIR NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P))
      (COND ((SETQ DIR-DELIM (CDR (ASSQ (AREF NAMESTRING IDX) DIR-DELIM-ALIST)))
	     (AND DIR
		  (PATHNAME-ERROR IDX "Directory occurs twice in ~A" NAMESTRING))
	     (INCF IDX)
	     (DO-FOREVER
	       (MULTIPLE-VALUE (TEM IDX DELIM)
		 (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING
						  (LIST #/. DIR-DELIM) IDX END NIL T))
	       (IF (MEMBER TEM WILD-STRINGS) (SETQ TEM :WILD))
	       (SETQ DIR (IF (AND (= DELIM DIR-DELIM) (NULL DIR))
			     (LIST TEM)
			   (NCONC DIR (NCONS TEM))))
	       (AND (= DELIM DIR-DELIM) (RETURN))))
	    (T
	     (MULTIPLE-VALUE (TEM IDX DELIM)
	       (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING ALL-DELIMS IDX END T T))
	     (COND ((ASSQ DELIM DIR-DELIM-ALIST)
		    (SETQ IDX (1- IDX)))
		   ((AND (= DELIM #/;) VER)	;Protect against twenex attribute usage
		    (SETQ IDX END)))
	     (IF (MEMBER TEM WILD-STRINGS) (SETQ TEM :WILD))
	     (COND ((= DELIM #/:)
		    (AND DEV
			 (PATHNAME-ERROR IDX
				 "Device occurs twice in ~A" NAMESTRING))
		    (SETQ DEV TEM DEV-SPECIFIED-P (1- IDX)))
		   ((= DELIM #/;)
		    (COND ((NULL NAM-SPECIFIED-P)
			   (SETQ NAM TEM TYP ""
				 NAM-SPECIFIED-P (1- IDX) TYP-SPECIFIED-P (1- IDX)))
			  ((NULL TYP-SPECIFIED-P)
			   (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX)))))
		   ((NULL NAM-SPECIFIED-P)
		    (SETQ NAM TEM NAM-SPECIFIED-P (1- IDX))
		    (IF (= DELIM #/.) (SETQ TYP :UNSPECIFIC)))
		   ((NULL TYP-SPECIFIED-P)
		    (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX))
		    (IF (EQ DELIM #/.) (SETQ VER :UNSPECIFIC)))
		   ((NULL VER-SPECIFIED-P)
		    (SETQ VER-SPECIFIED-P (1- IDX))
		    (COND ((NULL TEM)
			   (SETQ VER NIL))
			  ((EQUAL TEM "")
			   (SETQ VER :UNSPECIFIC))
			  ((SETQ TEM1 (NUMERIC-P TEM))
			   (SETQ VER TEM1))
			  ((EQ TEM :WILD)
			   (SETQ VER :WILD))
			  ((SEND SELF :OLDEST-CHECK TEM)
			   (SETQ VER :OLDEST))
			  (T (PATHNAME-ERROR IDX
				     "Version must be numeric in ~A" NAMESTRING))))))))))

))

; From file QFCTNS.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN ARGLIST (FUNCTION &OPTIONAL REAL-FLAG &AUX TEM DEBUG-INFO ARG-MAP LOCAL-MAP)
  "Return the argument list of FUNCTION, and its value-list.
FUNCTION may be a function or a function spec.
If REAL-FLAG is T, return the actual argument list, good for compilation, calling, etc.
If REAL-FLAG is COMPILE, return the argument list generated by the compiler, 
 if FUNCTION is compiled. This arglist includes the names of the keys for &KEY arguments,
 if any, and the forms used for defaulting optional args. /"Supplied-p/" args are not included
 If the function is not compiled, this is the same as REAL-FLAG = T
Otherwise, return an argument list intended as documentation for humans.
 This will be the same as if REAL-FLAG were COMPILE, unless there was an explicit
 (DECLARE (ARGLIST ...)) in the defintiion of FUNCTION.
The second value is the value-list, only for documentation for humans.
The third value is NIL, SUBST or MACRO."
  (DECLARE (VALUES ARGLIST VALUES TYPE))
  (TYPECASE FUNCTION
    (SYMBOL
     (OR (GET FUNCTION 'ARGLIST)		;Handles names defined only in the compiler.
	 (ARGLIST (FSYMEVAL FUNCTION) REAL-FLAG)))
    (CONS
     (COND ((mEmQ (CAR FUNCTION) '(LAMBDA cli:lambda))
	    (LDIFF (CADR FUNCTION) (MEMQ '&AUX (CADR FUNCTION))))
	   ((MEMQ (CAR FUNCTION) '(SUBST CLI:SUBST))
	    (VALUES (CADR FUNCTION) NIL 'SUBST))
	   ((MEMQ (CAR FUNCTION) '(NAMED-SUBST NAMED-LAMBDA cli:named-subst cli:named-lambda))
	    (SETQ DEBUG-INFO (DEBUGGING-INFO FUNCTION))
	    (COND ((AND (MEMQ REAL-FLAG '(NIL COMPILE))
			(ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO))
		   (ARGLIST (CADR (ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO)) REAL-FLAG))
		  (T
		   (VALUES
		     (LET ((TEM (OR (IF (EQ REAL-FLAG 'NIL)
					(ASSQ 'ARGLIST DEBUG-INFO))
				    (IF (MEMQ REAL-FLAG '(COMPILE NIL))
					(ASSQ 'COMPILER:COMPILER-ARGLIST DEBUG-INFO)))))
		       (IF TEM (CDR TEM)
			 (LDIFF (CADDR FUNCTION) (MEMQ '&AUX (CADDR FUNCTION)))))
		     (CDR (ASSQ 'VALUES DEBUG-INFO))
		     (AND (mEmQ (CAR FUNCTION) '(NAMED-SUBST cli:named-subst))
			  'SUBST)))))
	   ((MEMQ (CAR FUNCTION) '(CURRY-BEFORE CURRY-AFTER))
	    '(&REST ARGLIST))
	   ((EQ (CAR FUNCTION) 'MACRO)
	    ;; Look for (DECLARE (ARGLIST ...)) type arglist
	    (SETQ DEBUG-INFO (DEBUGGING-INFO (CDR FUNCTION)))
	    (VALUES (CDR (OR (IF (EQ REAL-FLAG 'NIL)
				 (ASSQ 'ARGLIST DEBUG-INFO))
			     (IF (MEMQ REAL-FLAG '(COMPILE NIL))
				 (ASSQ 'COMPILER:COMPILER-ARGLIST DEBUG-INFO))
			     '(NIL . MACRO)))
		    (CDR (ASSQ 'VALUES (DEBUGGING-INFO (CDR FUNCTION))))
		    'MACRO))
	   ((VALIDATE-FUNCTION-SPEC FUNCTION)
	    (ARGLIST (FDEFINITION FUNCTION) REAL-FLAG))
	   (T (FERROR NIL "~S not a recognized function" FUNCTION))))
    (STACK-GROUP
     '(STACK-GROUP-ARG))
    (ARRAY
     (DO ((I (%P-LDB %%ARRAY-NUMBER-DIMENSIONS FUNCTION) (1- I))
	  (L NIL))
	 (( I 0) L)
       (SETQ L (CONS (INTERN (FORMAT NIL "DIM-~D" I) PKG-SYSTEM-INTERNALS-PACKAGE) L))))
    ((OR CLOSURE ENTITY)
     (ARGLIST (CAR (%MAKE-POINTER DTP-LIST FUNCTION)) REAL-FLAG))
    ((OR SELECT-METHOD INSTANCE)
     ;; Can't tell arglist, shouldn't give error though
     '(OP &REST SELECT-METHOD-ARGS-VARY))
    (COMPILED-FUNCTION
     (SETQ DEBUG-INFO (DEBUGGING-INFO FUNCTION))
     (SETQ ARG-MAP (CADR (ASSQ 'COMPILER:ARG-MAP DEBUG-INFO)))
     (SETQ LOCAL-MAP (CADR (ASSQ 'COMPILER:LOCAL-MAP DEBUG-INFO)))
     (VALUES
       (COND ((AND (EQ REAL-FLAG 'NIL)
		   (CDR (ASSQ 'ARGLIST DEBUG-INFO))))
	     ((AND (MEMQ REAL-FLAG '(COMPILE NIL))
		   (CDR (ASSQ 'COMPILER:COMPILER-ARGLIST DEBUG-INFO))))
	     ((SETQ TEM (GET-MACRO-ARG-DESC-POINTER FUNCTION))
	      (DO ((ADL TEM (CDR ADL))
		   (ARGNUM 0 (1+ ARGNUM))
		   (ARGNAME)
		   (OPTIONALP NIL)
		   (QUOTEP NIL)
		   (SPECIAL FEF-LOCAL)
		   (INIT)
		   (INITP T T)
		   (ADLWORD)
		   (ARGLIS NIL))
		  ((NULL ADL)
		   (NREVERSE ARGLIS))
		(SETQ ADLWORD (CAR ADL))
		(SELECT
		  (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
		  (FEF-ARG-REQ
		   (AND OPTIONALP
			(FERROR NIL "Required args after optionals in ~S" FUNCTION)))
		      (FEF-ARG-OPT (OR OPTIONALP (SETQ ARGLIS (CONS '&OPTIONAL ARGLIS)))
				   (SETQ OPTIONALP T))
		      (FEF-ARG-REST (SETQ ARGLIS (CONS '&REST ARGLIS)))
		      (OTHERWISE (RETURN (NREVERSE ARGLIS))))
		(SELECT (MASK-FIELD %%FEF-QUOTE-STATUS ADLWORD)
		  (FEF-QT-QT (OR QUOTEP (SETQ ARGLIS (CONS '&QUOTE ARGLIS)))
			     (SETQ QUOTEP T))
		  (FEF-QT-EVAL (AND QUOTEP (SETQ ARGLIS (CONS '&EVAL ARGLIS)))
			       (SETQ QUOTEP NIL)))
		(SETQ TEM (LDB %%FEF-DES-DT ADLWORD))
		(SETQ TEM (LDB %%FEF-SPECIAL-BIT ADLWORD))	;handle remote some time?
		(WHEN (NEQ TEM SPECIAL)
		  (SETQ SPECIAL TEM)
		  (SETQ ARGLIS (CONS (NTH TEM '(&LOCAL &SPECIAL))
				     ARGLIS)))
		(SETQ ARGNAME (COND ((= (LOGAND ADLWORD %FEF-NAME-PRESENT)
					FEF-NM-YES)
				     (SETQ ADL (CDR ADL))
				     (CAR ADL))
				    (T
				     (SETQ ARGNAME (COND (( (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
							     FEF-ARG-REST)
							  (NTH ARGNUM ARG-MAP))
							 (T (CAR LOCAL-MAP))))
				     (IF (SYMBOLP ARGNAME) ARGNAME (CAR ARGNAME)))))
		(SELECT (MASK-FIELD %%FEF-INIT-OPTION ADLWORD)
		  (FEF-INI-NONE (SETQ INITP NIL))
		  (FEF-INI-NIL (SETQ INIT NIL))
		  (FEF-INI-PNTR
		   (SETQ ADL (CDR ADL))
		   (SETQ INIT (SELECTQ (%P-DATA-TYPE ADL)
				((#.DTP-EXTERNAL-VALUE-CELL-POINTER)
				 (MULTIPLE-VALUE-BIND (SYM CELL-FUNCTION)
				     (DECODE-EVCP (%P-CONTENTS-AS-LOCATIVE ADL))
				   (SELECTQ CELL-FUNCTION
				     (SYMEVAL SYM)
				     (FDEFINITION `(FUNCTION ,SYM))
				     (T `(,CELL-FUNCTION ',SYM)))))
				((#.DTP-SELF-REF-POINTER)
				 (FLAVOR-DECODE-SELF-REF-POINTER
				   (FEF-FLAVOR-NAME FUNCTION)
				   (%P-POINTER ADL)))
				(T `',(CAR ADL)))))
		  (FEF-INI-C-PNTR
		   (SETQ ADL (CDR ADL))
		   (COND ;((= (%P-DATA-TYPE ADL) DTP-EXTERNAL-VALUE-CELL-POINTER)
		         ; (SETQ INIT			;THIS IS A BIT OF A KLUDGE
		         ;       (%FIND-STRUCTURE-HEADER (%P-CONTENTS-AS-LOCATIVE ADL))))
		         ;HOPE IT'S VALUE-CELL-LOCATION
		     ((= (%DATA-TYPE (CAR ADL)) DTP-LOCATIVE)
		      (SETQ INIT (%FIND-STRUCTURE-HEADER (CAR ADL))))
		     ((SETQ INIT (CAAR ADL)))))
		  (FEF-INI-OPT-SA (SETQ ADL (CDR ADL))
				  (SETQ INIT '*HAIRY*))
		  (FEF-INI-COMP-C (SETQ INIT '*HAIRY*))
		  (FEF-INI-EFF-ADR (SETQ ADL (CDR ADL))
				   (SETQ INIT '*HAIRY*))
		  (FEF-INI-SELF (SETQ INIT ARGNAME)))
		(SETQ ARGLIS (CONS (COND (INITP
					  (LIST ARGNAME INIT))
					 (T ARGNAME)) ARGLIS))))
	     (T ;; No ADL.  Use the fast-arg-option to get the general pattern
	      ;;   and the argmap for the names.
	      (LET ((FAST-OPT (%ARGS-INFO FUNCTION))
		    (RES NIL))
		(LET ((MIN-ARGS (LDB %%ARG-DESC-MIN-ARGS FAST-OPT))
		      (MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS FAST-OPT))
		      (EVALED-REST (LDB %%ARG-DESC-EVALED-REST FAST-OPT))
		      (QUOTED-REST (LDB %%ARG-DESC-QUOTED-REST FAST-OPT)))
		  (DOTIMES (I MIN-ARGS)
		    (PUSH (CAAR ARG-MAP) RES)
		    (SETQ ARG-MAP (CDR ARG-MAP)))
		  (OR (= MIN-ARGS MAX-ARGS)
		      (PUSH '&OPTIONAL RES))
		  (DOTIMES (I (- MAX-ARGS MIN-ARGS))
		    (PUSH (CAAR ARG-MAP) RES)
		    (SETQ ARG-MAP (CDR ARG-MAP)))
		  (OR (ZEROP QUOTED-REST)
		      (PUSH '&QUOTE RES))
		  (WHEN (OR (NOT (ZEROP QUOTED-REST)) (NOT (ZEROP EVALED-REST)))
		    (PUSH '&REST RES)
		    (PUSH (CAAR LOCAL-MAP) RES))
		  (NREVERSE RES)))))
       (DEBUG-INFO-VALUES DEBUG-INFO)))
    (MICROCODE-FUNCTION
     (MICRO-CODE-ENTRY-ARGLIST-AREA (%POINTER FUNCTION)))
    (T (FERROR NIL "~S is not a function" FUNCTION))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun type-of (object &aux (dtp (%data-type object)))
  "Returns a type-specifier describing the type OBJECT belongs to.
For example, (TYPE-OF 5) is FIXNUM"
  (cond ((= dtp dtp-instance)
	 (%p-contents-offset
	   (instance-flavor object)
	   %instance-descriptor-typename))
	((= dtp dtp-array-pointer)
	 (cond ((named-structure-p object))
	       ((stringp object) 'string)
	       (t 'array)))
	((= dtp dtp-entity)
	 (class-symbol object))
	((= dtp dtp-extended-number) 
	 (selectq (%p-ldb-offset %%header-type-field object 0)
	   (#.%header-type-flonum 'single-float)
	   (#.%header-type-bignum 'bignum)
	   (#.%header-type-rational 'ratio)
	   (#.%header-type-complex 'complex)
	   (otherwise 'random)))
	((cdr (assq dtp type-of-alist)))
	(t 'random)))

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
	    (PUSH (CREATE-BP LINE 0) TENTHS)))
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
