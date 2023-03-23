
;System FED made by AMS at 3/22/23 08:33:17  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: IO1; FNTCNV LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; FED LISP >"
  '((:COMPILE NIL
     ((:METHOD FED::GRID-MIXIN :MOUSE-BOOLE-SQUARES) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL FED::BOX-Y-SIZE FED::GRID-MIXIN SELF NIL)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:TOP-MARGIN-SIZE FED::GRID-MIXIN SELF T)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL FED::BOX-X-SIZE FED::GRID-MIXIN SELF T)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:LEFT-MARGIN-SIZE FED::GRID-MIXIN SELF T)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL FED::WINDOW-Y-SIZE FED::GRID-MIXIN SELF T)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL FED::WINDOW-X-SIZE FED::GRID-MIXIN SELF T)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL FED::WINDOW-ARRAY FED::GRID-MIXIN SELF T))
     ((:METHOD FED :AFTER :INIT) NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-PRESET "is an obsolete function; Use the :PRESET message")))))
