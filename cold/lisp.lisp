;; -*- Mode: Lisp; Package: SI; Base:10; Readtable:ZL -*-

(setq initial-lisp-symbols '(
 &ALLOW-OTHER-KEYS
 &AUX
 &BODY
 &ENVIRONMENT
 &KEY
 &OPTIONAL
 &REST
 &WHOLE
 ***
 **
 *
 +++
 ++
 +
 -
 //////
 ////
 //=
 cli://						;common -- incompatible
 1+
 1-
 <=
 <
 =
 >=
 >
 *APPLYHOOK*
 *BREAK-ON-WARNINGS*
 *DEBUG-IO*
 cli:*DEFAULT-PATHNAME-DEFAULTS*		;common -- incompatible
 *ERROR-OUTPUT*
 *EVALHOOK*
 *FEATURES*
 *LOAD-VERBOSE*
 *MACROEXPAND-HOOK*
 *MODULES*
 *PACKAGE*
 *PRINT-ARRAY*
 *PRINT-BASE*
 *PRINT-CASE*
 *PRINT-CIRCLE*
 *PRINT-ESCAPE*
 *PRINT-GENSYM*
 *PRINT-LENGTH*
 *PRINT-LEVEL*
 *PRINT-PRETTY*
 *PRINT-RADIX*
 *QUERY-IO*
 *RANDOM-STATE*
 *READ-BASE*
 *READ-DEFAULT-FLOAT-FORMAT*
 *READ-SUPPRESS*
 *READTABLE*
 *STANDARD-INPUT*
 *STANDARD-OUTPUT*
 *TERMINAL-IO*
 *TRACE-OUTPUT*
 ABS
 ACONS
 ACOSH
 ACOS
 ADJOIN
 ADJUST-ARRAY
 ADJUSTABLE-ARRAY-P
 ALPHA-CHAR-P
 ALPHANUMERICP
 AND
 APPEND
 APPLYHOOK
 APPLY
 APROPOS-LIST
 APROPOS
 cli:AREF					;common -- incompatible
 ARRAY-DIMENSION-LIMIT
 ARRAY-DIMENSIONS
 ARRAY-DIMENSION
 ARRAY-DISPLACED-P
 ARRAY-ELEMENT-TYPE
 ARRAY-HAS-FILL-POINTER-P
 ARRAY-IN-BOUNDS-P
 ARRAY-RANK-LIMIT
 ARRAY-RANK
 ARRAY-ROW-MAJOR-INDEX
 ARRAY-TOTAL-SIZE
 ARRAYP
 ARRAY
 ASH
 ASINH
 ASIN
 ASSERT
 ASSOC-IF-NOT
 ASSOC-IF
 cli:ASSOC					;common -- incompatible
 ATANH
 cli:ATAN					;common -- incompatible
 BIGNUM
 BIT-ANDC1
 BIT-ANDC2
 BIT-AND
 BIT-EQV
 BIT-IOR
 BIT-NAND
 BIT-NOR
 BIT-NOT
 BIT-ORC1
 BIT-ORC2
 BIT-VECTOR-P
 BIT-VECTOR
 BIT-XOR
 BIT
 BLOCK
 BOOLE-1
 BOOLE-2
 BOOLE-ANDC1
 BOOLE-ANDC2
 BOOLE-AND
 BOOLE-C1
 BOOLE-C2
 BOOLE-CLR
 BOOLE-EQV
 BOOLE-IOR
 BOOLE-NAND
 BOOLE-NOR
 BOOLE-ORC1
 BOOLE-ORC2
 BOOLE-OR
 BOOLE-SET
 BOOLE-XOR
 BOOLE
 BOTH-CASE-P
 BOUNDP
 BREAK
 BUTLAST
 BYTE-POSITION
 BYTE-SIZE
 BYTE
 CAAAAR
 CAAADR
 CAAAR
 CAADAR
 CAADDR
 CAADR
 CAAR
 CADAAR
 CADADR
 CADAR
 CADDAR
 CADDDR
 CADDR
 CADR
 CALL-ARGUMENTS-LIMIT
 CAR
 CASE
 CATCH
 CCASE
 CDAAAR
 CDAADR
 CDAAR
 CDADAR
 CDADDR
 CDADR
 CDAR
 CDDAAR
 CDDADR
 CDDAR
 CDDDAR
 CDDDDR
 CDDDR
 CDDR
 CDR
 CEILING
 CERROR
 CHAR-BITS-LIMIT
 CHAR-BITS
 CHAR-BIT
 CHAR-CODE-LIMIT
 CHAR-CODE
 CHAR-CONTROL-BIT
 CHAR-DOWNCASE
 CHAR-EQUAL
 CHAR-FONT-LIMIT
 CHAR-FONT
 CHAR-GREATERP
 CHAR-HYPER-BIT
 CHAR-INT
 CHAR-LESSP
 CHAR-META-BIT
 CHAR-NAME
 CHAR-NOT-EQUAL
 CHAR-NOT-GREATERP
 CHAR-NOT-LESSP
 CHAR-SUPER-BIT
 CHAR-UPCASE
 CHAR//=
 CHAR<=
 CHAR<
 CHAR=
 CHAR>=
 CHAR>
 CHARACTERP
 cli:CHARACTER					;common -- incompatible
 CHAR
 CHECK-TYPE
 CIS
 CLEAR-INPUT
 CLEAR-OUTPUT
 cli:CLOSE					;common -- incompatible
 CLRHASH
 CODE-CHAR
 COERCE
 COMMONP
 COMMON
 COMPILE-FILE
 COMPILED-FUNCTION-P
 COMPILED-FUNCTION
 COMPILER-LET
 COMPILE
 COMPLEXP
 COMPLEX
 CONCATENATE
 COND
 CONJUGATE
 CONSP
 CONSTANTP
 CONS
 COPY-ALIST
 COPY-LIST
 COPY-READTABLE
 COPY-SEQ
 COPY-SYMBOL
 COPY-TREE
 COSH
 COS
 COUNT-IF-NOT
 COUNT-IF
 COUNT
 CTYPECASE
 DECF
 DECLARE					;common -- needs hacking
 DECODE-FLOAT
 DECODE-UNIVERSAL-TIME
 DEFCONSTANT
 DEFINE-MODIFY-MACRO
 DEFINE-SETF-METHOD
 DEFMACRO
 DEFPARAMETER
 DEFSETF
 cli:DEFSTRUCT					;common -- incompatible
 DEFTYPE
 DEFUN
 DEFVAR
 DELETE-FILE
 DELETE-IF-NOT
 DELETE-IF
 cli:DELETE					;common -- incompatible
 DENOMINATOR
 DEPOSIT-FIELD
 DESCRIBE
 DIGIT-CHAR-P
 DIGIT-CHAR
 DIRECTORY-NAMESTRING
 DIRECTORY
 DISASSEMBLE
 DO*
 DO-ALL-SYMBOLS
 DO-EXTERNAL-SYMBOLS
 DO-SYMBOLS
 DOCUMENTATION
 DOLIST
 DOTIMES
 DOUBLE-FLOAT-EPSILON
 DOUBLE-FLOAT-NEGATIVE-EPSILON
 DOUBLE-FLOAT
 DO
 DPB
 DRIBBLE
 ECASE
 ED
 EIGHTH
 ELT
 ENCODE-UNIVERSAL-TIME
 ENDP
 ENOUGH-NAMESTRING
 EQL
 EQUALP
 EQUAL
 EQ
 cli:ERROR					;common -- incompatible
 ETYPECASE
 EVAL-WHEN
 EVALHOOK
 EVAL
 EVENP
 cli:EVERY					;common -- incompatible
 EXPORT
 EXPT
 EXP
 FBOUNDP
 FCEILING
 FFLOOR
 FIFTH
 FILE-AUTHOR
 FILE-LENGTH
 FILE-NAMESTRING
 FILE-POSITION
 FILE-WRITE-DATE
 FILL-POINTER
 FILL
 FIND-IF-NOT
 FIND-IF
 FIND-PACKAGE
 FIND-SYMBOL
 FIND
 FINISH-OUTPUT
 FIRST
 FIXNUM
 FLET
 FLOAT-DIGITS
 FLOAT-PRECISION
 FLOAT-RADIX
 FLOAT-SIGN
 FLOATP
 FLOAT
 FLOOR
 FMAKUNBOUND
 FORCE-OUTPUT
 FORMAT
 FOURTH
 FRESH-LINE
 FROUND
 FTRUNCATE
 FUNCALL
 FUNCTIONP
 FUNCTION
 GCD
 GENSYM
 GENTEMP
 GET-DECODED-TIME
 GET-DISPATCH-MACRO-CHARACTER
 GET-INTERNAL-REAL-TIME
 GET-INTERNAL-RUN-TIME
 GET-MACRO-CHARACTER
 GET-OUTPUT-STREAM-STRING
 GET-PROPERTIES
 GET-SETF-METHOD-MULTIPLE-VALUE
 GET-SETF-METHOD
 GET-UNIVERSAL-TIME
 GETF
 GETHASH
 GET
 GO
 GRAPHIC-CHAR-P
 GREATERP
 HASH-TABLE-COUNT
 HASH-TABLE-P
 HASH-TABLE
 HOST-NAMESTRING
 IDENTITY
 IF
 IGNORE
 IMAGPART
 IMPORT
 IN-PACKAGE
 INCF
 INPUT-STREAM-P
 INSPECT
 INT-CHAR
 INTEGER-DECODE-FLOAT
 INTEGER-LENGTH
 INTEGERP
 INTEGER
 INTERNAL-TIME-UNITS-PER-SECOND
 INTERN
 cli:INTERSECTION				;common -- incompatible
 ISQRT
 KEYWORDP
 KEYWORD
 LABELS
 LAMBDA-LIST-KEYWORDS
 LAMBDA-PARAMETERS-LIMIT
 LAMBDA
 LAST
 LCM
 LDB-TEST
 LDB
 LDIFF
 LEAST-NEGATIVE-DOUBLE-FLOAT
 LEAST-NEGATIVE-LONG-FLOAT
 LEAST-NEGATIVE-SHORT-FLOAT
 LEAST-NEGATIVE-SINGLE-FLOAT
 LEAST-POSITIVE-DOUBLE-FLOAT
 LEAST-POSITIVE-LONG-FLOAT
 LEAST-POSITIVE-SHORT-FLOAT
 LEAST-POSITIVE-SINGLE-FLOAT
 LENGTH
 LET*
 LET
 LISP-IMPLEMENTATION-TYPE
 LISP-IMPLEMENTATION-VERSION
 LIST*
 LIST-ALL-PACKAGES
 LIST-LENGTH
 LISTEN
 cli:LISTP					;common -- incompatible
 LIST
 LOAD
 LOCALLY
 LOGANDC1
 LOGANDC2
 LOGAND
 LOGBITP
 LOGCOUNT
 LOGEQV
 LOGIOR
 LOGNAND
 LOGNOR
 LOGNOT
 LOGORC1
 LOGORC2
 LOGTEST
 LOGXOR
 LOG
 LONG-FLOAT-EPSILON
 LONG-FLOAT-NEGATIVE-EPSILON
 LONG-FLOAT
 LONG-SITE-NAME
 LOOP
 LOWER-CASE-P
 MACHINE-INSTANCE
 MACHINE-TYPE
 MACHINE-VERSION
 MACRO-FUNCTION
 MACROEXPAND-1
 MACROEXPAND
 MACROLET
 MAKE-ARRAY
 MAKE-BROADCAST-STREAM
 MAKE-CHAR
 MAKE-CONCATENATED-STREAM
 MAKE-DISPATCH-MACRO-CHARACTER
 MAKE-ECHO-STREAM
 MAKE-HASH-TABLE
 MAKE-LIST
 MAKE-PACKAGE
 MAKE-PATHNAME
 MAKE-RANDOM-STATE
 MAKE-SEQUENCE
 MAKE-STRING-INPUT-STREAM
 MAKE-STRING-OUTPUT-STREAM
 MAKE-STRING
 MAKE-SYMBOL
 MAKE-SYNONYM-STREAM
 MAKE-TWO-WAY-STREAM
 MAKUNBOUND
 MAPCAN
 MAPCAR
 MAPCON
 MAPC
 MAPHASH
 MAPLIST
 MAPL
 cli:MAP					;common -- incompatible
 MASK-FIELD
 MAX
 MEMBER-IF-NOT
 MEMBER-IF
 cli:MEMBER					;common -- incompatible
 MERGE-PATHNAMES
 MERGE
 MINUSP
 MIN
 MISMATCH
 MOD
 MOST-NEGATIVE-DOUBLE-FLOAT
 MOST-NEGATIVE-FIXNUM
 MOST-NEGATIVE-LONG-FLOAT
 MOST-NEGATIVE-SHORT-FLOAT
 MOST-NEGATIVE-SINGLE-FLOAT
 MOST-POSITIVE-DOUBLE-FLOAT
 MOST-POSITIVE-FIXNUM
 MOST-POSITIVE-LONG-FLOAT
 MOST-POSITIVE-SHORT-FLOAT
 MOST-POSITIVE-SINGLE-FLOAT
 MULTIPLE-VALUE-BIND
 MULTIPLE-VALUE-CALL
 MULTIPLE-VALUE-LIST
 MULTIPLE-VALUE-PROG1
 MULTIPLE-VALUE-SETQ
 MULTIPLE-VALUES-LIMIT
 NAME-CHAR
 NAMESTRING
 NBUTLAST
 NCONC
 NIL
 cli:NINTERSECTION				;common -- incompatible
 NINTH
 NOTANY
 NOTEVERY
 NOT
 NRECONC
 NREVERSE
 NSET-DIFFERENCE
 NSET-EXCLUSIVE-OR
 NSTRING-CAPITALIZE
 NSTRING-DOWNCASE
 NSTRING-UPCASE
 NSUBLIS
 NSUBST-IF-NOT
 NSUBST-IF
 NSUBSTITUTE-IF-NOT
 NSUBSTITUTE-IF
 NSUBSTITUTE
 NSUBST
 NTHCDR
 NTH
 NULL
 NUMBERP
 NUMBER
 NUMERATOR
 cli:NUNION					;common  -- incompatible
 ODDP
 OPEN
 OR
 OTHERWISE
 OUTPUT-STREAM-P
 PACKAGE-NAME
 PACKAGE-NICKNAMES
 PACKAGE-SHADOWING-SYMBOLS
 PACKAGE-USE-LIST
 PACKAGE-USED-BY-LIST
 PACKAGEP
 PACKAGE
 PAIRLIS
 PARSE-INTEGER
 PARSE-NAMESTRING
 PATHNAME-DEVICE
 PATHNAME-DIRECTORY
 PATHNAME-HOST
 PATHNAME-NAME
 PATHNAME-TYPE
 PATHNAME-VERSION
 PATHNAMEP
 PATHNAME
 PEEK-CHAR
 PHASE
 PI
 PLUSP
 POP
 POSITION-IF-NOT
 POSITION-IF
 POSITION
 PPRINT
 PRIN1-TO-STRING
 PRIN1
 PRINC-TO-STRING
 PRINC
 PRINT
 PROBE-FILE
 PROCLAIM
 PROG*
 PROG1
 PROG2
 PROGN
 PROGV
 PROG
 PROVIDE
 PSETF
 PSETQ
 PUSHNEW
 PUSH
 QUOTE
 RANDOM-STATE-P
 RANDOM-STATE
 RANDOM
 RASSOC-IF-NOT
 RASSOC-IF
 cli:RASSOC					;common -- incompatible
 RATIONALIZE
 RATIONALP
 RATIONAL
 RATIO
 READ-BYTE
 READ-CHAR-NO-HANG
 READ-CHAR
 READ-DELIMITED-LIST
 cli:READ-FROM-STRING				;common -- incompatible
 READ-LINE
 READ-PRESERVING-WHITESPACE
 READTABLEP
 READTABLE
 cli:READ					;common -- incompatible
 REALPART
 REDUCE
 REM-IF-NOT
 REM-IF
 REMF
 REMHASH
 REMOVE-DUPLICATES
 REMOVE-IF-NOT
 REMOVE-IF
 cli:REMOVE					;common -- incompatible
 REMPROP
 cli:REM					;common -- incompatible
 RENAME-FILE
 RENAME-PACKAGE
 REPLACE
 REQUIRE
 REST
 RETURN-FROM
 RETURN
 REVAPPEND
 REVERSE
 ROTATEF
 ROUND
 RPLACA
 SATISFIES
 SBIT
 SCALE-FLOAT
 SCHAR
 SEARCH
 SECOND
 SEQUENCE
 SET-CHAR-BIT
 SET-DIFFERENCE
 SET-DISPATCH-MACRO-CHARACTER
 SET-EXCLUSIVE-OR
 SET-MACRO-CHARACTER
 SET-SYNTAX-FROM-CHAR
 SETF
 SETQ
 SET
 SEVENTH
 SHADOWING-IMPORT
 SHADOW
 SHIFTF
 SHORT-FLOAT-EPSILON
 SHORT-FLOAT-NEGATIVE-EPSILON
 SHORT-FLOAT
 SHORT-SITE-NAME
 SIGNUM
 SIMPLE-ARRAY
 SIMPLE-BIT-VECTOR-P
 SIMPLE-BIT-VECTOR
 SIMPLE-STRING-P
 SIMPLE-STRING
 SIMPLE-VECTOR-P
 SIMPLE-VECTOR
 SINGLE-FLOAT
 SINH
 SIN
 SIXTH
 SLEEP
 SOFTWARE-TYPE
 SOFTWARE-VERSION
 cli:SOME					;common -- incompatible
 SORT
 SPECIAL-FORM-P
 SPECIAL					;common -- needs hacking
 SQRT
 STABLE-SORT
 STANDARD-CHAR-P
 STANDARD-CHAR
 STEP
 STREAM-ELEMENT-TYPE
 STREAMP
 STREAM
 STRING-CAPITALIZE
 STRING-CHAR-P
 STRING-CHAR
 STRING-DOWNCASE
 STRING-EQUAL
 STRING-GREATERP
 STRING-LEFT-TRIM
 STRING-LESSP
 STRING-NOT-EQUAL
 STRING-NOT-GREATERP
 STRING-NOT-LESSP
 STRING-RIGHT-TRIM
 STRING-TRIM
 STRING-UPCASE
 STRING<=
 STRING<
 STRING=
 STRING>=
 STRING>
 STRINGP
 STRING
 SUBLIS
 SUBSEQ
 SUBSETP
 SUBST-IF-NOT
 SUBST-IF
 SUBSTITUTE-IF-NOT
 SUBSTITUTE-IF
 SUBSTITUTE
 cli:SUBST					;common -- incompatible
 SUBTYPEP
 SVREF
 SXHASH
 SYMBOL-FUNCTION
 SYMBOL-NAME
 SYMBOL-PACKAGE
 SYMBOL-PLIST
 SYMBOL-VALUE
 SYMBOLP
 SYMBOL
 TAGBODY
 TAILP
 TANH
 TAN
 TENTH
 cli:TERPRI					;common -- incompatible
 THE
 THIRD
 THROW
 TIME
 TRACE
 TREE-EQUAL
 TRUENAME
 TRUNCATE
 TYPE-OF
 TYPECASE
 TYPEP
 T
 UNEXPORT
 UNINTERN
 UNION
 UNLESS
 UNREAD-CHAR
 UNSIGNED-BYTE
 UNSPECIAL					;common -- needs hacking
 UNTRACE
 UNUSE-PACKAGE
 UNWIND-PROTECT
 UPPER-CASE-P
 USE-PACKAGE
 USER-HOMEDIR-PATHNAME
 VALUES
 VECTOR-POP
 VECTOR-PUSH-EXTEND
 VECTOR-PUSH
 VECTORP
 VECTOR
 cli:WARN					;common -- incompatible
 WHEN
 WITH-INPUT-FROM-STRING
 WITH-OPEN-FILE
 WITH-OUTPUT-TO-STRING
 WRITE-BYTE
 WRITE-CHAR
 WRITE-LINE
 WRITE-STRING
 WRITE-TO-STRING
 WRITE
 Y-OR-N-P
 YES-OR-NO-P
 ZEROP

))