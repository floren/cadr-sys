;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 23.4
;;; Reason:
;;;  Backward-Files and Forward-Files commands didn't really let you use
;;;  the middle button to specify a number of files to skip.
;;; Written 31-May-88 21:37:15 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Djinn from band 2
;;; with Experimental System 124.14, Experimental Local-File 74.0, Experimental File-Server 23.1, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.3, Experimental Lambda-Diag 16.0, microcode 1756, SDU ROM 102.



; From modified file DJ: L.TAPE; TFRAME-COMS.LISP#45 at 31-May-88 21:37:24
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "

(defun Space-Backward-Files (format device n)
  (with-status ("Spacing Backward ~D File~:P ..." n)
    (condition-case ()
	(send format :previous-file device n)
      (tape:physical-beginning-of-tape
       (format *standard-output* "~&At beginning of tape.")))))

(define-command BACKWARD-FILES (control)
  "Space backwards N files.  L: use global numeric arg {M: Enter from Keyboard}"
  :left   (Space-Backward-Files tape:*selected-format* tape:*selected-device* *global-numeric-arg*)
  :middle (let ((number (prompt-and-read :number "~&Number of file to space backward by >> ")))
	    (cond ((typep number '(integer 1))
		   (Space-Backward-Files tape:*selected-format* tape:*selected-device* number))
		  (t
		   (tv:beep)
		   (format t "~&~%Number must be a positive integer!~%"))))
  :documentation "~
This moves the tape backward by files.  If the left mouse button
is used, then the \"global numeric argument\" determined the number 
of files to space over.  If the middle button is used, the number
of files must be specified by the user.")


))

; From modified file DJ: L.TAPE; TFRAME-COMS.LISP#45 at 31-May-88 21:37:30
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "

(define-command FORWARD-FILES (control)
  "Space forward N files.  L: use global numeric argument {M: Enter from Keyboard}"
  :left   (Space-Forward-Files tape:*selected-format* tape:*selected-device* *global-numeric-arg*)
  :middle (let ((number (prompt-and-read :number "~&Number of file to space forward by >> ")))
	    (cond ((typep number '(integer 1))
		   (Space-Forward-Files tape:*selected-format* tape:*selected-device* number))
		  (t
		   (tv:beep)
		   (format t "~&~%Number must be a positive integer!~%"))))
  :documentation "~
This moves the tape forward by files.  If the left mouse button
is used, then the \"global numeric argument\" determined the number 
of files to space over.  If the middle button is used, the number
of files must be specified by the user.")

))
