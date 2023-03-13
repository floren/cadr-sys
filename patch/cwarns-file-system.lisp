
;System FILE-SYSTEM made by LISPM at 3/12/23 19:03:09  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FILE; ACCESS LISP >"
  '((:COMPILE NIL
     (FS::INIT-DORMANT-HOST-GC-PROCESS NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-PRESET "is an obsolete function; Use the :PRESET message")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FILE; PATHNM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FILE; PATHST LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FILE; LOGICAL LISP >"
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
  '#FS::LOGICAL-PATHNAME "SYS: IO; FILE; OPEN LISP >"
  '((:COMPILE NIL
     (FS:REMOTE-CONNECT NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::ACCESS))
     ((:METHOD PATHNAME :REMOTE-CONNECT) NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::ACCESS)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO; FILE; BALDIR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: NETWORK; CHAOS; QFILE LISP >"
  '((:COMPILE NIL
     (FS::COMPLETE-CHAOS NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." STRING))
     (FS::HOMEDIR-CHAOS NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::USER))
     ((:METHOD FS::QFILE-DATA-STREAM-MIXIN :RENAME) NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FS::TRUENAME-STRING)))))
