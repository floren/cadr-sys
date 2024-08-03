;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 19.1
;;; Reason:
;;;  Fix FIND-FILE commands - was erring out, MATCH-SPEC unbound.
;;; Written 14-Jan-88 13:24:10 by keith (Keith Corbett) at site LMI
;;; while running on Opus from band 1
;;; with Experimental System 123.174, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 8.4, Experimental Laser1+ 2.0, Experimental Tape 19.0, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta I/site/dvi.



; From file OPUS: L.TAPE; TFRAME-COMS.LISP#44 at 14-Jan-88 14:06:36
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-COMS  "

(define-command FIND-FILE (retrieve)
  "Search the tape for a file."
  :left (condition-case ()
	    (let ((plist (send tape:*selected-format* :find-file
			       tape:*selected-device* *file-match*)))
	      (format t "~&Found \"~a\"." (car plist))
	      plist)
	  (tape:logical-end-of-tape
	   (format  t "~&No files matching \"~a\" on tape." *file-match*)
	   (format t "~&*** End of Tape ***~%")))
  :documentation "~
This searches the tape for a file that matches the pathname
specified by the \"file match template\".  If the file is found,
the tape is positioned at the beginning of the file and subsequently
a RESTORE-FILES command can retrieve it.  The file property list
is returned.")

))
