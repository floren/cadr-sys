
;System COMPILER made by AMS at 3/20/23 08:23:47  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; MADEFS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCDEFS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; DISASS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; MA LISP >"
  '((:COMPILE NIL
     (COMPILER::MA-GRUBBLE NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:CC-TYPE-OUT)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CC-UINST-DESC))
     ((:PROPERTY COMPILER::DO-SPECBIND COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-ALUF)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-ALU-SETA)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-OB)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OB-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-MEM-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-A-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     ((:PROPERTY COMPILER::MOVE COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-ALUF)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-ALU-SETA)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-OB)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OB-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-FUNC-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-MEM-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-A-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-ALU-SETM)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-BYTE)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-BYTL-1)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-MROT)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-BYTE-FUNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-BYTE-FUNC-LDB))
     ((:PROPERTY COMPILER::MOVE-LOCATIVE-T COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     ((:PROPERTY COMPILER::JUMP COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-UNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N))
     ((:PROPERTY COMPILER::OPEN-CALL COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-P)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-UNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-ALUF)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-ALU-SETM)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-OB)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OB-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-FUNC-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-PDL-BUFFER-PUSH))
     ((:PROPERTY CALL COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-UNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-P)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N))
     ((:PROPERTY COMPILER::POP-SPECPDL COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     (COMPILER::MA-ASSEMBLE-JUMP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-BYTE)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-A-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-MEM-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-BYTL-1)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-MROT)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N))
     (COMPILER::MA-ASSEMBLE-JUMP-ATOM NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-BYTL)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-MROT)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-UNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N))
     ((:PROPERTY COMPILER::DYNAMIC-STACK-TEST COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-MICRO-STACK)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-A-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-M>A)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N))
     ((:PROPERTY COMPILER::EXIT COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     ((:PROPERTY COMPILER::POP-SPECPDL-AND-EXIT COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     ((:PROPERTY COMPILER::DISCARD-TOP-OF-STACK COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-C-PDL-BUFFER-POINTER-POP))
     ((:PROPERTY COMPILER::ARG-CALL COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     ((:PROPERTY COMPILER::START-LIST COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     ((:PROPERTY COMPILER::START-LIST-AREA COMPILER::MA-ASSEMBLE) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     (COMPILER::MA-REF-M-SIDE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-C-PDL-BUFFER-POINTER-POP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-C-PDL-BUFFER-POINTER)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-C-PDL-BUFFER-INDEX)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-MD))
     (COMPILER::MA-SET-PDL-INDEX-RELATIVE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-ALUF)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-ALU-SUB)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-OB)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OB-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-FUNC-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-PDL-BUFFER-INDEX)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-SRC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-M-SRC-PDL-BUFFER-POINTER)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-A-SRC))
     (COMPILER::MA-NOTE-PDL-WRITE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-C-PI)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-C-PP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-PDL-BUFFER-PUSH))
     (COMPILER::MA-PREPARE-STORE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-PDL-BUFFER-PUSH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-C-PP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-C-PI)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-MD))
     (COMPILER::MA-EMIT-EXIT-REF NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-DISPATCH)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-CONST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-DISP-ADDR))
     (COMPILER::MA-MCLAP-CODE-XFER-TO-SEQ NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-UNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N))
     (COMPILER::MA-MCLAP-SEQ NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-JUMP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-COND)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-JUMP-COND-UNC)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-N)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OP-BYTE)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-FUNC-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-C-PP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-C-PI)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-FUNC-DEST-PDL-BUFFER-PUSH)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; MAOPT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; MC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; MLAP LISP >"
  '((:COMPILE NIL
     (COMPILER::MCLAP NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" CADR:CC-TYPE-OUT)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CC-UINST-DESC))
     (COMPILER::MCLAP-PLUGIN-MM-CALLS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-JUMP-ADDR))
     (COMPILER::MCLAP-EXAMINE-MM-LINKAGES NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-IR-OP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR:CONS-OP-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-ALUF)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-ALU-SETA)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-OB)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-OB-ALU)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-M-MEM-DEST)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." CADR::CONS-IR-A-SRC))
     (COMPILER::GET-UCADR-STATE-LIST NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" UA::UCODE-IMAGE-VERSION)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" UA::UCODE-MODULE-ASSEMBLER-STATE)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" UA::UCODE-IMAGE-MODULE-POINTS)
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." UA::CURRENT-UCODE-IMAGE)
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead"))
     (COMPILER::MA-INITIALIZE-VARIABLES NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCFASD LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCFILE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCP1 LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCP2 LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCOPT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCLUKE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCPEEP LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QCLAP LISP >"
  '((:COMPILE NIL)))
