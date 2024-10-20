;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for System version 300.6
;;; Reason:
;;;  PRINT-DATE-ONLY: New function.
;;; Written 21-Oct-24 00:51:42 by AMS,
;;; while running on Lisp Machine One from band 3
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 300.5, microcode 323.



; From file OZ: /sys/io1/time.lisp.145 at 21-Oct-24 00:51:54
#10R TIME#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TIME")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //sys//io1//time"

(DEFUN PRINT-DATE-ONLY (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME (get-universal-time))
    (FORMAT STREAM "~2,'0D-~2,'0D-~2,'0D"
	    day
	    (month-string month ':short)
	    (remainder year 100.))))

))
