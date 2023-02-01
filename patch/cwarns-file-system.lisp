
;System FILE-SYSTEM made by LISPM at 1/31/23 18:16:55  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; ACCESS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; PATHNM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; PATHST LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; LOGICAL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: FILE2; PATHNM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: FILE; LMPARS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; OPEN LISP >"
  '((:COMPILE NIL
     (FS:REMOTE-CONNECT NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::ACCESS))
     ((:METHOD PATHNAME :REMOTE-CONNECT) NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::ACCESS)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; BALDIR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; QFILE LISP >"
  '((:COMPILE NIL
     (FS::COMPLETE-CHAOS NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." STRING))
     (FS::HOMEDIR-CHAOS NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::USER))
     ((:METHOD FS::QFILE-DATA-STREAM-MIXIN :RENAME) NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::TRUENAME-STRING)))))
