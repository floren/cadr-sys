
;System SYSTEM-INTERNALS made by AMS at 3/22/23 03:28:00  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; PRODEF LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; RDDEFS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; SGDEFS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; NUMDEF LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; ADVISE LISP >"
  '((:COMPILE NIL
     (SI::ADVISE-FIND-SLOT NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::CLASSS)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." CLASS)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; BAND LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; CHARACTER LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; CLPACK LISP >"
  '((:COMPILE NIL
     (SI::PKG-INITIALIZE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::INITIAL-LISP-SYMBOLS)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; GLOBAL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; SYSTEM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; LISP LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; COLD LISP >"
  '((:COMPILE NIL
     (SI::INITIALIZE-WIRED-KBD-BUFFER NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." TV::TV-SLOT-NUMBER)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; DEFSEL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; DESCRIBE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; DISK LISP >"
  '((:COMPILE NIL
     (SI::CC-DISK-HANDLER NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:CC-DISK-WRITE)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:CC-DISK-READ)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:DBG-SETUP-UNIBUS-MAP)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:PHYS-MEM-READ)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:PHYS-MEM-WRITE)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::READ-LABEL-1)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (SI::DECODE-UNIT-ARGUMENT NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:CC-DISK-INIT)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:CC-DISK-WRITE)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; DLEDIT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; DRIBBL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; ENCAPS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; EVAL LISP >"
  '((:COMPILE NIL
     (LETF NIL NIL
      (COMPILER::VARIABLE-NOT-SYMBOL :IMPOSSIBLE NIL "~S appears in a list of variables to be bound." (SI::TEM (QUOTE LOCF) (CAAR SI::VARS-LEFT)))
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::TEM))
     (LETF* NIL NIL
      (COMPILER::VARIABLE-NOT-SYMBOL :IMPOSSIBLE NIL "~S appears in a list of variables to be bound." (SI::TEM (QUOTE LOCF) (CAAR SI::VARS-LEFT)))
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::TEM))
     (SYSTEM:APPLY-LAMBDA NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::ENVIRONMENT)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; FLAVOR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; FSPEC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; GC LISP >"
  '((:COMPILE NIL
     (GC-ON NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-PRESET "is an obsolete function; Use the :PRESET message")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; GENRIC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; GRIND LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; HASH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; HASHFL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: NETWORK; HOST LISP >"
  '((:COMPILE NIL
     (SI::RESET-NON-SITE-HOSTS NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" FS::LM-HOST-INITIALIZE)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." FS::LOCAL-HOST-ADDED))
     (SI::PARSE-USER-NAME NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::@)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: NETWORK; PACKAGE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; INC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; INFIX LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; LOGIN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; LOOP LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; LTOP LISP >"
  '((:COMPILE NIL
     (LISP-REINITIALIZE NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CLEAR-UNIBUS-MAP)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" UNBREAKON)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::RG-QUAD-SLOT)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::SDU-QUAD-SLOT)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; MACARR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; MAKSYS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; MINI LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; NUMER LISP >"
  '((:COMPILE NIL
     (TAN NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (TAND NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (ASIN NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (ACOS NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (SINH NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (COSH NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (TANH NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (ASINH NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (ACOSH NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X))
     (ATANH NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::X)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; PATCH LISP >"
  '((:COMPILE NIL
     (LOAD-PATCHES NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-RESET "is an obsolete function; Use the :RESET message")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-RESET "is an obsolete function; Use the :RESET message")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; PLANE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; PRINT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; PROCES LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; QIO LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QFASL LISP >"
  '((:COMPILE NIL
     (SI:RECORD-FILE-DEFINITIONS NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-RESET "is an obsolete function; Use the :RESET message")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QFCTNS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QMISC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; HARDCOPY LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QNEW LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QRAND LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; QTRACE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; RAT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; READ LISP >"
  '((:COMPILE NIL
     (SI::XR-#=-MACRO NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::LABEL-BINDING)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; RESOUR LISP >"
  '((:COMPILE NIL
     (DEALLOCATE-RESOURCE NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SI::DEINITIALIZER)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; SELEV LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; SERIAL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; SGFCTN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; SORT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; STEP LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; STREAM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; STRING LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; SYSDCL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; UNFASL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; UNIBUS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; CLMAC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; ANALYZE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; METH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; CLASS LISP >"
  '((:COMPILE NIL)))
