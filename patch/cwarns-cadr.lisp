
;System CADR made by AMS at 3/22/23 10:34:14  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; CADRLP LISP >"
  '((:COMPILE NIL
     (UA::CONS-LAP-ALLOCATE-ARRAYS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." UA::MICRO-CODE-SYMBOL-AREA-SIZE))
     (UA::WRITE-VARIOUS-OUTPUTS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." UA::CONSLP-OUTPUT-PATHNAME))
     (UA::DEFMIC NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." UA::NO-QINTCMP)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." UA::LISP-FUNCTION-P)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." ARGLIST)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; CDMP LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QWMCR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FREAD LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; USYMLD LISP >"
  '((:COMPILE NIL
     (UA::CC-LOAD-MODULE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." UA::RACMO)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." UA::RADMO)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." UA::RAAMO)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; LQFMAC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; LCADMC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; CC LISP >"
  '((:COMPILE NIL
     (CADR::CC-TYPE-IN NIL NIL
      (COMPILER::BREAK-ARG :OBSOLETE NIL "A symbol as the first argument to BREAK is an obsolete construct;
change it to a string before it stops working: ~S" (BREAK CADR::COND-BARF T))
      (COMPILER::BREAK-ARG :OBSOLETE NIL "A symbol as the first argument to BREAK is an obsolete construct;
change it to a string before it stops working: ~S" (BREAK CADR::INPUT-LOSSAGE-GOBBLING))
      (COMPILER::BREAK-ARG :OBSOLETE NIL "A symbol as the first argument to BREAK is an obsolete construct;
change it to a string before it stops working: ~S" (BREAK CADR::INPUT-LOSSAGE-SPACE)))
     (CADR::CC NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" INIT-DL11-UNIBUS-CHANNEL)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (CADR::ILONG-TRIAL NIL NIL
      (COMPILER::BREAK-ARG :OBSOLETE NIL "A symbol as the first argument to BREAK is an obsolete construct;
change it to a string before it stops working: ~S" (BREAK CADR::FOO T)))
     (CADR::CC-CHECK-MAP NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CC-LEVEL-1-MAP-FREQUENCIES)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CC-LEVEL-1-REVERSE-MAP))
     (CADR::CC-RAID NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CC-RAID-REG))
     (CADR::CC-STORE NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CC-RAID-REG))
     (CADR::CC-PRINT-REG-ADR-CONTENTS NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CC-RAID-REG)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; CCGSYL LISP >"
  '((:COMPILE NIL
     (CADR::CC-GETSYL-READ-TOKEN NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." IMPLODE "is an obsolete function; use strings")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; LCADRD LISP >"
  '((:COMPILE NIL
     (CADR::CC-READ-OBUS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (CADR::CC-EXECUTE-R NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (CADR::CC-EXECUTE-W NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (CADR::CC-EXECUTE-LOAD-DEBUG-IR NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (CADR::CC-STEP-MACH NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::SPY-WRITE NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CNSUBW))
     (CADR::SPY-READ NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR::CNSUBR)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; DIAGS LISP >"
  '((:COMPILE NIL
     (CADR::CC-LOOP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-TEST-MACHINE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH))
     (CADR::CC-TEST-VMA-DP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::RAVMA))
     (CADR::CC-TEST-A-MEM-ADDRESSES NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-TEST-PDL-ADDRESSES NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-RUN-TEST-LOOP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-WRITE-ZERO-SPC NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-TEST-SHIFTER NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." IF-FOR-LISPM "is an obsolete function; use the #+LISPM reader macro"))
     (CADR::CC-TEST-LC-AFFECTS-SHIFT NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-SAVED-MICRO-STACK-PTR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-MICRO-STACK))
     (CADR::CC-TEST-DISPATCH NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-SAVED-MICRO-STACK-PTR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-MICRO-STACK))
     (CADR::CC-MEM-TEST-LOOP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-MEM-ZERO NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::DC-CLP-NXM NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::DC-CLP-ADR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::DC-CMD-ADR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::DC-START-ADR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::DC-STS-ADR))
     (CADR::CC-MEM-FILL NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-MEM-FILL-CHECK NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-MEM-TEST-ONE-WORD-TO-DISK NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-DISK-REPEAT-OP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-WRITE-FCN)
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-ADDRESS)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-LAST-CMD)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-LAST-CLP))
     (CADR::CC-CMB-ZAP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-RUN-LOOP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; DMON LISP >"
  '((:COMPILE NIL
     (CADR::CC-RUN-MTEST NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-MODE-REG)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-UPDATE-DISPLAY-FLAG)
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-SAVED-MD)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-SAVED-VMA)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." CADR::CC-MODE-REG))
     (CADR::CC-MTEST-DING-LOOP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-SAVED-VMA)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-SAVED-MD))
     (CADR::CC-TV-SYNC-WRITE-LOOP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-TV-SYNC-READ-LOOP NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::CC-TV-SYNC-READ-LOOP-ENB-RAM NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; LDBG LISP >"
  '((:COMPILE NIL
     (CADR::DBG-RESET NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-UNIBUS-MAP-TO-MD-OK-FLAG))
     (CADR::DBG-WRITE-UNIBUS-MAP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-UNIBUS-MAP-TO-MD-OK-FLAG)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; ZERO LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; CADLD LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; QF LISP >"
  '((:COMPILE NIL
     (CADR::QF-MEM-READ-DISK-COPY NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CC-DISK-LOWCORE))
     (CADR::QF-OBARRAY-NEW-P NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." CADR::PACK)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; CCWHY LISP >"
  '((:COMPILE NIL
     (CADR::CC-MAIL NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." CADR::CC-UPDATE-DISPLAY-FLAG))
     (CADR::WHY-TEXT NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::RAVMA)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-ADDRESS))
     (CADR::CC-CHECK-SERIOUS-ERROR-STATUS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:DBG-ACCESS-PATH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-ADDRESS)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-DISK-DA-DESC))
     (CADR::CC-ANALYZE-SOFTWARE-CRASH NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::RAVMA))
     ((:PROPERTY EH::TRANS-TRAP CADR::ETE-HANDLER) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::RAVMA))
     (CADR::CC-INSTRUCTION-M-SOURCE-REG-ADR NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::RAVMA))
     (CADR::CC-PRINT-ADDRESS-AND-CONTENTS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::RAVMA)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; CCDISK LISP >"
  '((:COMPILE NIL
     (CADR::CC-DISK-CLOBBER NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG"))
     (CADR::READ-LABEL NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CC-DISK-LOWCORE))
     (CADR::FORMAT-DISK-PACK NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-UPDATE-DISPLAY-FLAG))
     (CADR::CC-INITIALIZE-ON-STARTUP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-FULL-SAVE-VALID)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CC-PASSIVE-SAVE-VALID)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; DCHECK LISP >"
  '((:COMPILE NIL
     (CADR::DC-EXEC NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message"))
     (CADR::DC-EXEC-1 NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message"))
     (CADR::DC-EXEC-2 NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message"))
     (CADR::DC-WRITE-READ-TEST NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message"))
     (CADR::DC-READ-TEST NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-CHAR-AVAILABLE "is an obsolete function; use LISTEN or send *TERMINAL-IO* a :LISTEN message"))
     (CADR::DECODE-TRACK NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." KBD-TYI-NO-HANG "is an obsolete function; send *TERMINAL-IO* a :TYI-NO-HANG message
or use READ-CHAR-NO-HANG or READ-BYTE-NO-HANG")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; CHPLOC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: CC; SALVAG LISP >"
  '((:COMPILE NIL)))
