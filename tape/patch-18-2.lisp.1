;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 18.2
;;; Reason:
;;;  Niceness fixes to SELECT-FORMAT. 
;;;  Fix bug that prevented the intended behaviour that the *default-format*
;;;  be the default selected option on menu. [Use (cdr(rass 'string-equal ...))
;;;  to match format names.]
;;;  
;;;  Also call :set-options method with print/read bases set to decimal --
;;;  just nicer.
;;; Written 13-Jan-88 13:46:21 by keith (Keith Corbett) at site LMI
;;; while running on Opus from band 1
;;; with Experimental System 123.174, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental Tape 18.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 8.4, Experimental Laser1+ 2.0, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta I/site/dvi.



; From modified file OPUS: L.TAPE; USER.LISP#95 at 13-Jan-88 13:46:32
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; USER  "
			       
(defun select-format (&optional format-spec &rest init-options)
  "Selects a format to be *selected-format*.  FORMAT-SPEC must be a
format flavor symbol or NIL in which case a menu of available formats
will be popped up.  INIT-OPTIONS case be used to setup the initial
options of the format."
  (let ((flavor (or (cdr (ass 'string-equal format-spec *tape-format-alist*))
		    (let ((alist (mapcar 'cdr *tape-format-alist*)))
		      (tv:mouse-warp (floor (send tv:default-screen :width) 2)
				     (floor (send tv:default-screen :height) 2))
		      (tv:menu-choose alist
				      '(:string "Tape Formats Supported" :font fonts:tr12b)
				      '(:mouse)
				      (or (cdr(rass 'string-equal (car *default-format*)
					       *tape-format-alist*))
					  (car alist)))))))
    (when flavor
      (setq *selected-format* (parse-format flavor))
      (unless (and format-spec (not init-options))
	(let((*print-base* 10.)
	     (*read-base* 10.))
	  (lexpr-send *selected-format* :set-options init-options)))
	*selected-format*)))

))
