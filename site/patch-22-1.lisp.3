;;; -*- Mode: Lisp; Package: SYSTEM-INTERNALS; Base: 10.; Patch-File: T -*-
;;; Patch file for MIT-Specific version 22.1
;;; Reason: define basics of hacks:toscanini-uptime-info.
;;;   More sphisticated downtime (holiday) knowledge to follow later.
;;; Written 5/29/84 04:45:26 by LISPM,
;;; while running on Lisp Machine Twenty-three from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.56, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309, ZM MIT.



; From file MITSPECIFIC.LISP PS:<L.SITE> OZ:
#10R HACKS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SITE; MITSPECIFIC  "

(defun toscanini-uptime-info (&optional (stream *standard-output*) (time (time:get-universal-time)))
  (let ((*standard-output* stream))
    (multiple-value-bind (nil m h) (decode-universal-time time)
      (cond (( 12. h 24.)
	     (format t "There are still ~D Hour~P and ~D minute~P to get icecream!"
		     (- 23. h) (- 23. h) (- 60. m) (- 60. m)))
	    ((< h 11.)
	     (format t "Well, you lose. Toscanini's doesn't open again for over ~D hour~P!"
		  (- 11. h)))
	 (t
	  (format t "Yow! Toscanini's opens again in only ~D minute~P"
		  (- 60. m) (- 60. m)))))))

))
