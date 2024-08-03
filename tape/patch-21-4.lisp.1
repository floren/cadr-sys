;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 21.2
;;; Reason:
;;;  Another fix to TAPE:SELECT-FORMAT.  Again, use (cdr(RASS...)) to compare
;;;  flavor names in *TAPE-FORMAT-ALIST*.  This time, fix the case that
;;;  SELECT-FORMAT gets called with a flavor name.  
;;;  
;;;  This is Part 1 of fix to bug with right-click on selected format in Tape
;;;  Frame.
;;; Written 14-Mar-88 12:05:56 by keith (Keith M. Corbett) at site LMI
;;; while running on Opus from band 3
;;; with Experimental System 123.199, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 21.1, Experimental Tiger 27.0, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta II/site.



; From modified file OPUS: L.TAPE; USER.LISP#96 at 14-Mar-88 12:13:40
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; USER  "


(defun select-format (&optional format-spec &rest init-options)
  "Selects a format to be *selected-format*.  FORMAT-SPEC must be a
format flavor symbol or NIL in which case a menu of available formats
will be popped up.  INIT-OPTIONS case be used to setup the initial
options of the format."
  (let ((flavor (or (cdr (rass 'string-equal format-spec *tape-format-alist*))
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
