;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 18.1
;;; Reason:
;;;  Fixes to foreign tape format error messages.
;;;  
;;;  1. For Raw-Format, fix calls to signal 'not-supported error;
;;;  :init option is :device-object, not :object.  This was throwing
;;;  into the error handler with a meke-instance error, not the error
;;;  message intended!
;;;  
;;;  2. In ERROR, fix :report method for 'not-supported error;
;;;  had bug in format string.
;;; Written 13-Jan-88 12:55:54 by keith (Keith Corbett) at site LMI
;;; while running on Opus from band 1
;;; with Experimental System 123.174, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental Tape 18.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 8.4, Experimental Laser1+ 2.0, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta I/site/dvi.




; From modified file OPUS: L.TAPE; ERROR.LISP#46 at 13-Jan-88 13:00:12
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "

(defmethod (not-supported :report) (stream)
  (format stream "Sorry, but ~s does not support the operation ~S.~%"
	  device-object operation))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:09
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :read-tape-header) (&rest ignore)
  (signal 'not-supported :device-object self :operation :read-tape-header))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:21
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :write-tape-header) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-tape-header))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:22
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :restore-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :restore-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:26
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :write-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:27
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :write-partition) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-partition))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:28
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :compare-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :compare-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:31
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :beginning-of-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :beginning-of-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:33
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :next-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :next-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:34
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :previous-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :previous-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:36
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :find-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :find-file))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:37
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :find-file-reverse) (&rest ignore)
  (signal 'not-supported :device-object self :operation :find-file-reverse))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:38
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :list-files) (&rest ignore)
  (signal 'not-supported :device-object self :operation :list-files))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:39
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :finish-tape) (&rest ignore)
  (signal 'not-supported :device-object self :operation :finish-tape))

))

; From modified file OPUS: L.TAPE; RAW-FORMAT.LISP#9 at 13-Jan-88 13:06:41
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; RAW-FORMAT  "


(defmethod (raw-format :position-to-append) (&rest ignore)
  (signal 'not-supported :device-object self :operation :position-to-append))

))
