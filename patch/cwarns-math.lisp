
;System MATH made by LISPM at 1/31/23 20:36:49  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS2; MATRIX LISP >"
  '((:COMPILE NIL
     (MATH:INVERT-MATRIX NIL NIL
      (SI::BAD-TYPE :IMPLAUSIBLE NIL "The type ~S does not seem to be defined" (ARRAY INTEGER)))
     (MATH:DECOMPOSE NIL NIL
      (SI::BAD-TYPE :IMPLAUSIBLE NIL "The type ~S does not seem to be defined" (ARRAY INTEGER)))
     (MATH:SOLVE NIL NIL
      (SI::BAD-TYPE :IMPLAUSIBLE NIL "The type ~S does not seem to be defined" (ARRAY INTEGER))))))
