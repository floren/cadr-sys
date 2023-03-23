
;System PEEK made by AMS at 3/22/23 07:07:08  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; PEEK LISP >"
  '((:COMPILE NIL
     (TV::PEEK-WHOSTATE NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-WHOSTATE "is an obsolete function; this function is now SI:PROCESS-WAIT-WHOSTATE"))
     ((:METHOD SI:FILE-DATA-STREAM-MIXIN :PEEK-FILE-SYSTEM) NIL NIL
      (COMPILER::SELF-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~S is being referenced by a lexically closed-over function.
This will not, of course, work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" SELF SELF NIL)
      (COMPILER::SELF-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~S is being referenced by a lexically closed-over function.
This will not, of course, work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" SELF SELF NIL)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: NETWORK; CHAOS; PEEKCH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; PEEKFS LISP >"
  '((:COMPILE NIL)))
