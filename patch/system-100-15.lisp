;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.15
;;; Reason:
;;;  New variable UNKNOWN-ADDRESS-FUNCTION; used when encountering unknown Chaos addresses.
;;; Written 6-Jun-23 16:46:26 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.13, Hacks by AMS 2.0, microcode 323, WIP.



; From file FC: /sys/network/chaos/chuse.lisp at 6-Jun-23 16:46:26
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chuse"

(DEFUN CHAOS-UNKNOWN-ADDRESS-FUNCTION (ADDR NETWORK)
  ;; Use the above CHAOS-UNKNOWN-HOST-FUNCTION also for addresses,
  ;; assuming the HOSTAB server is friendly and accepts addresses or names.
  (WHEN (EQ NETWORK :CHAOS)
    (CHAOS-UNKNOWN-HOST-FUNCTION (FORMAT NIL "~O" ADDR))))
))

; From file FC: /sys/network/chaos/chuse.lisp at 6-Jun-23 16:46:36
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chuse"

(SETQ SI::UNKNOWN-ADDRESS-FUNCTION 'CHAOS-UNKNOWN-ADDRESS-FUNCTION)
))

; From file FC: /sys/network/host.lisp at 6-Jun-23 16:47:23
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//host"

(DEFVAR UNKNOWN-ADDRESS-FUNCTION NIL
  "If non-NIL, this should be a function that accepts an address and a network type
and, if possible, defines and returns a host object. Or NIL if it can not.")
))

; From file FC: /sys/network/host.lisp at 6-Jun-23 16:47:34
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//host"

(DEFUN GET-HOST-FROM-ADDRESS (ADDRESS NETWORK &OPTIONAL (UNKNOWN-OK T))
  "Return the host object which has the address ADDRESS on NETWORK, or NIL if none."
  (OR
    (LOOP FOR ELEM IN SI:HOST-ALIST
          WHEN (MEMQ ADDRESS (GETF (SI:HOST-ADDRESSES ELEM) NETWORK))
          RETURN (GET-ALIST-ELEM-HOST ELEM))
    (WHEN (AND UNKNOWN-OK UNKNOWN-ADDRESS-FUNCTION)
      (LET ((F UNKNOWN-ADDRESS-FUNCTION)
            (UNKNOWN-ADDRESS-FUNCTION NIL))     ;Avoid recursion
        (FUNCALL F ADDRESS NETWORK)
        ;; See if it was succesful
        (GET-HOST-FROM-ADDRESS ADDRESS NETWORK NIL)))))
))
