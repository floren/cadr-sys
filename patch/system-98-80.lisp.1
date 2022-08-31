;;; -*- Mode: Lisp; Package: User; Base: 10.; Patch-File: T -*-
;;; Written 11/26/84 14:44:15 by RPK,
;;; Reason: CHAOS:ADDRESS-PARSE returns two values -- the address, and the host.
;;; while running on Lisp Machine Twelve from band 2
;;; with System 98.79, CADR 3.10, ZMail 53.19, MIT-Specific 22.5, microcode 309, gc@36.


; From file CHUSE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (12)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: CHNCP; CHUSE  "

(DEFUN ADDRESS-PARSE (ADDRESS &AUX HOST)
  "Coerce the argument to a chaosnet address.
The argument can be a host name or host object, or an address."
  (DECLARE (VALUES ADDRESS HOST-OBJECT))
  (CONDITION-CASE (ERROR)
      (LET ((ADDRESS (COND ((INTEGERP ADDRESS)
			    ADDRESS)
			   ((AND (TYPEP ADDRESS 'INSTANCE)
				 (SEND (SETQ HOST ADDRESS) :SEND-IF-HANDLES :CHAOS-ADDRESS)))
			   ((AND (SETQ HOST (SI:PARSE-HOST ADDRESS T))
				 (SEND HOST :CHAOS-ADDRESS)))
			   ((AND (STRINGP ADDRESS)
				 (PARSE-NUMBER ADDRESS 0 NIL 8))))))
	(IF ADDRESS (VALUES ADDRESS (OR HOST (SI:GET-HOST-FROM-ADDRESS ADDRESS :CHAOS)))))
    (SYS:UNCLAIMED-MESSAGE NIL)))

))
