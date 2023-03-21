
;System System made by AMS at 10/21/22 15:07:23  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; ALARM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; BEEPS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; CAFE LISP >"
  '((:COMPILE NIL
     (HACKS:DRAW-COLOR-INTO-RECTANGLE NIL NIL
      (COMPILER:OBSOLETE :OBSOLETE NIL "~S ~A." COLOR:COLOR-BITBLT "is an obsolete function; use COLOR:RECTANGLE with slightly different args")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; COLXOR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; COLORHACK LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; CROCK LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; DC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; DEUTSC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; DLWHAK LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; DOCTOR LISP >"
  '((:COMPILE NIL
     (HACKS:READIN NIL NIL
      (COMPILER:OBSOLETE :OBSOLETE NIL "~S ~A." EXPLODEC "is an obsolete function; use strings"))
     (HACKS:READWORD NIL NIL
      (COMPILER:OBSOLETE :OBSOLETE NIL "~S ~A." MAKNAM "is an obsolete function; use strings"))
     (HACKS:ANALYZE NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" HACKS:QUIT)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; DOCSCR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; GEB LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; HCEDIT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; MUNCH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; OHACKS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; ORGAN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; QIX LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; ROTATE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; ROTCIR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; WORM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: DEMO; WORM-TRAILS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: IO1; METER LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: IO1; SRCCOM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS:LOGICAL-PATHNAME "SYS: IO1; CONVER LISP >"
  '((:COMPILE NIL
     (ZWEI:QSEND-FORCE-MESSAGE-1 NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" ZWEI:SEND-MESSAGE-STRING))
     (ZWEI:CONVERSE-SEND-MSG-INTERNAL NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" ZWEI:SEND-MESSAGE-STRING)))))
