
;System TV made by LISPM at 10/24/22 05:27:05  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; TVDEFS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; SCRMAN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; SHEET LISP >"
  '((:COMPILE NIL
     (TV:SHEET-CALCULATE-OFFSETS NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET"))
     (TV:SHEET-OVERLAPS-P NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET"))
     (TV:SHEET-OVERLAPS-EDGES-P NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET"))
     (TV:SHEET-OVERLAPS-SHEET-P NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET"))
     (TV:SHEET-WITHIN-P NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET"))
     (TV:SHEET-CONTAINS-SHEET-POINT-P NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; SHWARM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; BASWIN LISP >"
  '((:COMPILE NIL
     ((:METHOD TV::ESSENTIAL-WINDOW :BEFORE :INIT) NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET"))
     (TV::LOWEST-SHEET-UNDER-POINT NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; WHOLIN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; MOUSE LISP >"
  '((:COMPILE NIL
     ((:METHOD TV:BASIC-SCROLL-BAR :SET-SCROLL-BAR-SPEC) NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S is an obsolete calling seqence.
   use (~S ~S~@[ ~*~S ~2:*~S~]) instead." (MAKE-LIST NIL 4) MAKE-LIST 4 NIL :AREA)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; BASSTR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; STREAM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; GRAPHICS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; MENU LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; COMETH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; SYSMEN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; SCRED LISP >"
  '((:COMPILE NIL
     (TV:MOUSE-SET-WINDOW-POSITION NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-X "is an obsolete function; use TV:SHEET-X-OFFSET")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." TV::SHEET-Y "is an obsolete function; use TV:SHEET-Y-OFFSET")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; TYPWIN LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; SCROLL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; TSCROL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; FRAME LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; CHOICE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; CSRPOS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; INSPCT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; RH LISP >"
  '((:COMPILE NIL)))
