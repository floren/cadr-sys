;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Written 1/29/84 21:29:21 by RpK,
;;; Reason: s-Abort really aborts sending message (i.e., it will not be a candidate for
;;;  the Continue command)
;;; while running on Lisp Machine Eighteen from band 4
;;; with System 98.30, CADR 3.6, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file MAIL.LISP PS:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFCOM COM-ABORT-SEND "Abort sending of this message
If you really want to forget about this message after aborting, use s-Abort. " ()
  (FORMAT QUERY-IO "~&Aborting, use the /"Continue/" command to continue.")
  (*THROW 'SEND-IT NIL))

))

; From file MAIL.LISP PS:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFCOM COM-REALLY-ABORT-SEND "Abort this message, and forget about its draft" ()
  (FORMAT QUERY-IO "~&Aborting.")
  (SETQ *DRAFT-LIST* (DELQ *DRAFT-MSG* *DRAFT-LIST*))
  (*THROW 'SEND-IT NIL))

))

(SET-COMTAB *REPLY-COMTAB* '(#\Super-Abort COM-REALLY-ABORT-SEND))
