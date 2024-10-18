;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for ZWEI version 125.16
;;; Reason:
;;;  Made patch system more robust.
;;; Written 12-Aug-88 18:44:26 by rwk at site Gigamos Cambridge
;;; while running on Harpo from band 3
;;; with Experimental System 126.49, ZWEI 125.15, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.12, SDU ROM 102, kold 4aug88.



; From modified file DJ: L.ZWEI; PATED.LISP#40 at 12-Aug-88 18:44:35
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFCOM COM-START-PATCH "Begin a patch but do not put any code into it yet.
Like the Add Patch command but leaves the patch buffer empty except for its
initial comments." ()
  (VALIDATE-PATCH-BUFFER)
  (when (string-equal "rwk" si::user-id)
    (if (fquery () "Perhaps you really should *test* a patch once in a while, Uncle Bob? ")
	(progn (format *query-io* "Good, aborting this patch.  Please test the sucker on a *fresh* machine!")
	       (return-from com-start-patch dis-none))
      (progn (format *query-io* "OK, Robert, but if you screw up again you're dog meat!")
	     (sleep 3))))
  (WHEN *PATCH-BUFFER*
    (UNLESS (FQUERY () "You are already editing patch ~A.
Do you want to switch to editing some other patch? "
		    (PATCH-VERSION-DESCRIPTION))
      (BARF "You cannot edit two patches at once."))
    (FORMAT *QUERY-IO* "~&Use M-X Resume Patch to go back to editing the previous patch.")
    (SETQ *PATCH-BUFFER* NIL))
  (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME))
  (FORMAT *QUERY-IO* "~&Beginning patch ~A." (PATCH-VERSION-DESCRIPTION))
  DIS-NONE)

))
