;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 22.1
;;; Reason:
;;;  The si:tape-warm-initialization-list contains a (select-processor) form
;;;  to be evaluated.  Add :falcon to eliminate warning message on warm boot.
;;; Written 13-Apr-88 13:17:12 by pld at site Gigamos Cambridge
;;; while running on Jack Flanders from band 1
;;; with Experimental System 123.232, Experimental Local-File 73.4, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 22.0, microcode 1755, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#208 at 13-Apr-88 13:17:23
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun tapemaster-on-bus? ()
  (select-processor
    (:lambda (neq (determine-tapemaster-owner) :not-on-bus))
    ((:cadr :explorer :falcon))))

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#208 at 13-Apr-88 13:17:36
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(add-initialization "Setup Tapemaster"
		    '(select-processor
		       (:lambda
			 (when (eq (determine-tapemaster-owner) :this-processor)
			   (setup-control-memory)
			   (init)))
		       ((:cadr :explorer :falcon)))
		    '(:now)
		    'si:tape-warm-initialization-list)




))
