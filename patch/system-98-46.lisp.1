;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 4/06/84 02:35:19 by RpK,
;;; Reason: NVT window beeps send SUPDUP:TERMINAL-BEEP as type argument to TV:BEEP.
;;; while running on Lisp Machine One from band 6
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.43, CADR 3.6, ZMail 53.14, MIT-Specific 22.0, microcode 309.



; From file SUPDUP.LISP PS:<L.WINDOW> OZ:
#8R SUPDUP#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SUPDUP  "

(DEFMETHOD (BASIC-NVT :REMOTE-BEEP) ()
  (TV:PREPARE-SHEET (SELF))			;Modular way to deal with output hold.
;  (OR (ZEROP (TV:SHEET-EXCEPTIONS))		;Subject to output holding
;      (TV:SHEET-HANDLE-EXCEPTIONS SELF))
  (COND ((OR (MEMQ (FUNCALL-SELF ':STATUS) '(:EXPOSED :SELECTED))
	     (EQ (FUNCALL-SELF ':DEEXPOSED-TYPEIN-ACTION)
		 ':NOTIFY))		;If he wants notify on type-in, "notify" on beep.
	 (FUNCALL-SELF ':BEEP 'SUPDUP:TERMINAL-BELL))))

))
