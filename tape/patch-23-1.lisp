;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 23.1
;;; Reason:
;;;  Still further fixes to tape frame status messages.
;;;  Specifically, say "listing 1 file" not "1 files".
;;;   
;;; Written 27-May-88 17:23:00 by LISPM at site Gigamos Cambridge
;;; while running on Fish food from band 1
;;; with Experimental System 124.10, Experimental Local-File 74.0, Experimental File-Server 23.0, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.0, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.TAPE; TFRAME-COMS.LISP#44 at 27-May-88 17:23:28
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "


(define-command BACKWARD-FILES (control)
  "Space backwards N files.  L: use global numeric arg {M: Enter from Keyboard}"
  :left (with-status ("Spacing Backward ~D File~:P ..." *global-numeric-arg*)
	  (condition-case ()
	      (send tape:*selected-format* :previous-file
		    tape:*selected-device* *global-numeric-arg*)
	    (tape:physical-beginning-of-tape
	     (format *standard-output* "~&At beginning of tape."))))
  :middle (let ((number (prompt-and-read :number "~&Number of file to space backward by >> ")))
	    (if (typep number '(integer 1))
		(with-status ("Spacing Backward ~D File~:P ..." number)
		  (condition-case ()
		      (send tape:*selected-format* :previous-file
			    tape:*selected-device* number)
		    (tape:physical-beginning-of-tape
		     (format *standard-output* "~&At beginning of tape."))))
	      (tv:beep)
	      (format t "~&~%Number must be a positive integer!~%")))
  :documentation "~
This moves the tape backward by files.  If the left mouse button
is used, then the \"global numeric argument\" determined the number 
of files to space over.  If the middle button is used, the number
of files must be specified by the user.")


))

; From modified file DJ: L.TAPE; TFRAME-COMS.LISP#44 at 27-May-88 17:23:32
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "


(Defun Space-Forward-Files (format device n)
  (with-status ("Spacing Forward ~D File~:P ..." n)
    (Send format :next-file device n)))

))

; From modified file DJ: L.TAPE; TFRAME-COMS.LISP#44 at 27-May-88 17:23:36
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "


(define-command FORWARD-FILES (control)
  "Space forward N files.  L: use global numeric argument {M: Enter from Keyboard}"
  :left
    (Space-Forward-Files tape:*selected-format* tape:*selected-device* *global-numeric-arg*)
  :middle
    (let ((number (prompt-and-read :number "~&Number of file to space forward by >> ")))
      (if (typep number '(integer 1))
	  (Space-Forward-Files tape:*selected-format* tape:*selected-device* number)
	(tv:beep)
	(format t "~&~%Number must be a positive integer!~%")))
  :documentation "~
This moves the tape forward by files.  If the left mouse button
is used, then the \"global numeric argument\" determined the number 
of files to space over.  If the middle button is used, the number
of files must be specified by the user.")

))

; From modified file DJ: L.TAPE; TFRAME-COMS.LISP#44 at 27-May-88 17:23:49
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "


(define-command LIST-SOME-FILES (tape-info)
  "L: List N files according to the global numeric argument {M: from keyboard}"
  :left (with-status ("Listing ~D tape file~:P" *global-numeric-arg*)
	  (tape:list-files :number-of-files *global-numeric-arg*))
  :middle (let ((number (prompt-and-read :number "~&Number of files to list >> ")))
	    (typecase number
	      ((integer 1)
	       (with-status ("Listing ~D tape file~:P" number)
		 (send tape:*selected-format*
		       :list-files tape:*selected-device*
		       :number-of-files number)))
	      (t
	       (tv:beep)
	       (format t "~&~%Number must be a positive integer!~%"))))
  :documentation "~
This prints information about a specific number of files on the tape.
If the left mouse button is used, then the \"global numeric argument\" 
is used as the number of files.  If the middle button is used, the
number of files is read from the keyboard.")

))
