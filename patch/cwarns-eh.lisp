
;System EH made by AMS at 3/20/23 11:07:25  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: EH; EH LISP >"
  '((:COMPILE NIL
     (EH::SIGNAL-MICROCODE-ERROR NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" EH::MICROCODE-FUNCTION-ARG-VALUES)
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" EH::MAKE-MICROCODE-FUNCTION-FRAME)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: EH; EHF LISP >"
  '((:COMPILE NIL
     ((:PROPERTY SYSTEM:DIVIDE-BY-ZERO EH::MAKE-UCODE-ERROR-FUNCTION) NIL NIL
      (COMPILER::NOT-IGNORED :IMPLAUSIBLE NIL "The variable ~S, which is declared to be ignored, was referenced" EH::ETE)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: EH; EHC LISP >"
  '((:COMPILE NIL
     (EH::COM-PRINT-OPEN-CATCH-FRAMES NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." EH::SG))
     (EH::COM-PRINT-LEXICAL-ENVIRONMENT NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." EH::SG)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: EH; EHW LISP >"
  '((:COMPILE NIL
     ((:METHOD EH::ERROR-HANDLER-TEXT-SCROLL-PANE :COMBINED :DEEXPOSE) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:SCREEN-ARRAY EH::ERROR-HANDLER-TEXT-SCROLL-PANE SELF NIL))
     (EH::COMW-PROCEED NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." EH::SPECIAL-COMMANDS)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: EH; EHBPT LISP >"
  '((:COMPILE NIL)))
