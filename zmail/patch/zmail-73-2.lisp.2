;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 73.2
;;; Reason:
;;;  Further refinement to earlier patch to handling (non-existent) GMSGS
;;;  functionality.  Remove error trapping from host methods for :DO-GMSGS;
;;;  the GMSGS function now handles and prints out network errors. (This is
;;;  more consistent.)  Unfortunately, we cannot easily trap for the rename
;;;  problem for people who inadvertently have *RUN-GMSGS-P* set such that
;;;  they're running GMSGS by accident.
;;; Written 5-Aug-88 15:07:34 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.14, ZWEI 125.11, ZMail 73.1, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, falambka.



; From modified file DJ: L.ZMAIL; MFHOST.LISP#72 at 5-Aug-88 15:07:36
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "


(defun run-gmsgs-p (&optional maybe)
  (CASE *RUN-GMSGS-P*
    (:YES T)
    (:NO NIL)
    (:ONCE-ONLY maybe)
    (t *run-gmsgs-p*)))

))

; From modified file DJ: L.ZMAIL; MFILES.LISP#335 at 5-Aug-88 15:43:29
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

(DEFUN GMSGS (&OPTIONAL (HOST FS:USER-LOGIN-MACHINE) (STREAM *STANDARD-OUTPUT*))
  (IF (STRINGP HOST) (SETQ HOST (SI:PARSE-HOST HOST)))
  (condition-case (error)
      (SEND HOST :DO-GMSGS STREAM)   ;Methods are in MFHOST
    (SYS:NETWORK-ERROR
     (beep)
     (FORMAT *QUERY-IO* "~&GMSGS Error: ~A"
	     (send error :REPORT-STRING)))))

))

; From modified file DJ: L.ZMAIL; MFHOST.LISP#73 at 5-Aug-88 15:43:39
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "

(DEFMETHOD (SI:HOST :DO-GMSGS) (STREAM)
  (MULTIPLE-VALUE-BIND (FILE-NAME UNAME-STRING) (SEND SELF :GMSGS-PATHNAME)
    (WITH-OPEN-STREAM (CSTREAM (CHAOS:OPEN-STREAM SELF
						  (GMSGS-CONTACT-NAME
						    FILE-NAME UNAME-STRING)
						  :DIRECTION :INPUT))
      (STREAM-COPY-UNTIL-EOF CSTREAM STREAM))
    FILE-NAME))

))

; From modified file DJ: L.ZMAIL; MFHOST.LISP#73 at 5-Aug-88 15:43:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "

(DEFMETHOD (SI:HOST-TOPS20-MIXIN :DO-GMSGS) (STREAM &aux result)
  (MULTIPLE-VALUE-BIND (FILE-NAME UNAME-STRING) (SEND SELF :GMSGS-PATHNAME)
    (setq result (CHAOS:SIMPLE SELF (GMSGS-CONTACT-NAME FILE-NAME UNAME-STRING)))
    (FORMAT STREAM "~&~A" (CHAOS:PKT-STRING RESULT))
    (CHAOS:RETURN-PKT RESULT)
    FILE-NAME))

))
