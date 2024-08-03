;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 21.4
;;; Reason:
;;;  Part 2 of fix to 'selected format' on tape frame.
;;;  
;;;  Right click calls PROCESS-FORMAT-SELECT, which was finding out the
;;;  default tape format, but not doing anything with it.  Call SELECT-FORMAT
;;;  on the default format, or failing that, on the first one on the alist.
;;; Written 14-Mar-88 12:29:16 by keith (Keith M. Corbett) at site LMI
;;; while running on Opus from band 3
;;; with Experimental System 123.199, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 21.1, Experimental Tiger 27.0, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta II/site.



; From modified file OPUS: L.TAPE; TFRAME-PROCESS.LISP#25 at 14-Mar-88 12:31:39
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-PROCESS  "


(defun process-format-select (blip)
  (let ((tem tape:*selected-format*))
    (ecase (second blip)
      (:select (tape:select-format))
      (:edit-options (tape:set-format-options))
      (:reset tape:(cond (*default-format*
			  (and (lexpr-funcall 'parse-format *default-format*)
			       (lexpr-funcall 'select-format *default-format*)))
			 (*tape-format-alist*
			  (and (parse-format (cdar *tape-format-alist*))
			       (select-format (cdar *tape-format-alist*)))))))
    (unless (eq tem tape:*selected-format*)
      (refresh-format))))

))
