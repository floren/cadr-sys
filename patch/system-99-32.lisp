;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.32
;;; Reason:
;;;  Allow supdup/telnet without logging in locally
;;; Written 28-Apr-87 13:37:57 by nick,
;;; while running on Ford Prefect from band 1
;;; with System 99.27, CADR 4.3, Experimental ZMail 54.4, MIT-Specific 23.0, Experimental FILE-Server 10.0, Experimental Local-File 53.0, Experimental Macsyma 6.0, microcode 320, EECS.



; From file OZ:KANSAS:<L.WINDOW>SUPDUP.LISP.276 at 28-Apr-87 13:37:57
#8R SUPDUP#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

(DEFMETHOD (BASIC-NVT :BEFORE :CONNECT) (&REST IGNORE)
  (IF CONNECTION
      (SEND SELF :DISCONNECT)
      #+fascist-bullshit (FS:FORCE-USER-TO-LOGIN)))

))
