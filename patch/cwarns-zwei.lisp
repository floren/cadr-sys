
;System ZWEI made by LISPM at 3/12/23 20:12:34  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; DEFS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; MACROS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; SCREEN LISP >"
  '((:COMPILE NIL
     ((:METHOD ZWEI:ZMACS-FRAME :AFTER :INIT) NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." PROCESS-RESET "is an obsolete function; Use the :RESET message")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMTAB LISP >"
  '((:COMPILE NIL
     ((:METHOD ZWEI:WINDOW :EDIT) NIL NIL
      (COMPILER::SELF-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~S is being referenced by a lexically closed-over function.
This will not, of course, work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" SELF SELF NIL)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; DISPLA LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; FOR LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; INDENT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; INSERT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; METH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; PRIMIT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; NPRIM LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; HISTORY LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; FONT LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; KBDMAC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; SEARCH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMA LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMB LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMD LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COME LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMF LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMG LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMH LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; COMS LISP >"
  '((:COMPILE NIL
     (ZWEI::QUERY-REPLACE NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::CHAR-UPPERCASE-P "is an obsolete function; use UPPER-CASE-P")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; DIRED LISP >"
  '((:COMPILE NIL
     (ZWEI::COM-BUG NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." ZWEI::*ZMAIL-BUG-LIST*))
     (ZWEI::COM-MAIL-INTERNAL NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." ZWEI::*MAIL-HEADER-DELIMITER*))
     (ZWEI::COM-QUIT-COM-MAIL NIL NIL
      (COMPILER::BAD-TYPE-SPECIFICATION :IMPLAUSIBLE NIL "Error expanding type specification ~S for ~S:~%   ~A" (QUOTE ZWEI::STANDALONE-MAIL-OR-DIRED-FRAME) TYPEP #FERROR :PROPERTY-LIST (:TYPE-SPECIFIER ZWEI::STANDALONE-MAIL-OR-DIRED-FRAME) :CONDITION-NAMES (FERROR ERROR CONDITION SI::INVALID-TYPE-SPECIFIER) :FORMAT-STRING "~S is not a known type specifier" :FORMAT-ARGS (ZWEI::STANDALONE-MAIL-OR-DIRED-FRAME)))
     (ZWEI::COM-EXIT-COM-MAIL NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" ZWEI::SEND-MESSAGE)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." ZWEI::*MAIL-HEADER-DELIMITER*))
     (MAIL NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" ZWEI::SEND-MESSAGE-STRING)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; BDIRED LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; DOC LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; FASUPD LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; FILES LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; HOST LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; ISPELL LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; LPARSE LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; MODES LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; MOUSE LISP >"
  '((:COMPILE NIL
     (ZWEI::DEFAULT-MARK-THING NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; PATED LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; PL1MOD LISP >"
  '((:COMPILE NIL
     (ZWEI::PL1-SKIP-COMMENT NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH"))
     (ZWEI::PL1-GET-STRING-FORWARD NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH"))
     (ZWEI::PL1-SKIP-COMMENT-BACKWARD NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH"))
     (ZWEI::PL1-GET-STRING-BACKWARD NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH")))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; POSS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; STREAM LISP >"
  '((:COMPILE NIL
     ((:METHOD ZWEI::EDITOR-STREAM-MIXIN :STREAM-RUBOUT-HANDLER) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" ZWEI::*INSIDE-EDITOR-STREAM* ZWEI::EDITOR-STREAM-MIXIN)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; SECTIO LISP >"
  '((:COMPILE NIL
     (ZWEI::CONTINUE-TAGS-QUERY-REPLACE NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH"))
     (ZWEI::CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." ZWEI::SEARCH "is an obsolete function; use ZWEI:ZWEI-SEARCH"))
     (ZWEI::LIST-METHODS-INTERNAL NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." ZWEI::OBJECT-CLASS)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; ZMNEW LISP >"
  '((:COMPILE NIL
     (ZWEI::CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES NIL NIL
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." ZWEI::DEFAULT)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; ZMACS LISP >"
  '((:COMPILE NIL)))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: ZWEI; ZYMURG LISP >"
  '((:COMPILE NIL
     ((:METHOD ZWEI::ECHO-AREA-WINDOW :COMBINED :DEEXPOSE) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:SCREEN-ARRAY ZWEI::ECHO-AREA-WINDOW SELF NIL))
     ((:METHOD ZWEI:STANDALONE-EDITOR-WINDOW :COMBINED :DEEXPOSE) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:SCREEN-ARRAY ZWEI:STANDALONE-EDITOR-WINDOW SELF NIL))
     ((:METHOD ZWEI::TEMPORARY-MODE-LINE-WINDOW :COMBINED :DEEXPOSE) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:SCREEN-ARRAY ZWEI::TEMPORARY-MODE-LINE-WINDOW SELF NIL))
     ((:METHOD ZWEI::EDITOR-STREAM-WINDOW :COMBINED :DEEXPOSE) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]" NIL TV:SCREEN-ARRAY ZWEI::EDITOR-STREAM-WINDOW SELF NIL)))))
