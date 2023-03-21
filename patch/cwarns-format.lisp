
;System FORMAT made by AMS at 3/20/23 08:12:12  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FORMAT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; FQUERY LISP >"
  '((:COMPILE NIL
     (FORMAT::FQUERY-DECODE-OPTIONS NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SELECT)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." SIGNAL-CONDITION))
     ((:SELECT-METHOD FORMAT::READLINE-FQUERY-FUNCTION :READ) NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." STRING))
     (FORMAT::MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." STRING)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; OUTPUT LISP >"
  '((:COMPILE NIL)))
