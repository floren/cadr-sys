
;System ITS-Tape made by AMS at 2/01/23 04:25:58  -*-Mode: Lisp; Package: User; Base: 10.-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: TAPE; PDP10 LISP >"
  '((:COMPILE NIL
     (FS::LM-PLIST-TO-ITS-FILENAMES NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead")
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." GET-FROM-ALTERNATING-LIST "is an obsolete function; use GETF instead"))
     (FS::LOAD-PDP10-DUMP-MAGTAPE NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" FS::STREAM-COPY-PDP10-QFASL-FILE-TO-PDP10-TAPE)))))
