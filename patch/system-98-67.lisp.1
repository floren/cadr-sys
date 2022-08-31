;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 6/29/84 03:22:59 by kab,
;;; Reason: SI:PATCH-LOADED-P simply returns NIL if SYSTEM is not loaded.
;;; while running on Lisp Machine Three from band 2
;;; with System 98.64, CADR 3.7, ZMail 53.18, MIT-Specific 22.2, Experimental KAB-Auxiliary 2.0, microcode 309, ZM MIT.



; From file PATCH.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN PATCH-LOADED-P (MAJOR-VERSION MINOR-VERSION &OPTIONAL (SYSTEM "System"))
  "T if specified patch to patchable system SYSTEM is now loaded.
The patch specified is the one with numbers MAJOR-VERSION and MINOR-VERSION.
If the actual loaded major version is greater than MAJOR-VERSION
then the answer is T regardless of MINOR-VERSION, on the usually-true assumption
that the newer system contains everything patched into the older one.
NIL if SYSTEM is not loaded at all."
  (LET* ((PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED SYSTEM T T))
	 (CURRENT-MAJOR-VERSION (PATCH-VERSION PATCH-SYSTEM)))
    (AND PATCH-SYSTEM
	 (OR (> CURRENT-MAJOR-VERSION MAJOR-VERSION)
	     (AND (= CURRENT-MAJOR-VERSION MAJOR-VERSION)
		  ( (OR (VERSION-NUMBER (CAR (PATCH-VERSION-LIST PATCH-SYSTEM))) 0)
		     MINOR-VERSION))))))

))
