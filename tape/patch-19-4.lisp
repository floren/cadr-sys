;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 19.4
;;; Reason:
;;;  Define error 'invalid-option-value, to signal when an improper VALUE
;;;  has been specified for a proper OPTION.  This was obviously intended.
;;; Written 18-Jan-88 14:01:47 by keith (Keith M. Corbett) at site LMI
;;; while running on Opus from band 2
;;; with Experimental System 123.174, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 8.4, Experimental Laser1+ 2.0, Experimental Tape 19.3, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta I/site/dvi/patches.



; From modified file OPUS: L.TAPE; ERROR.LISP#47 at 18-Jan-88 14:01:54
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defflavor invalid-option-value (object option value)
	   (user-error)
  :gettable-instance-variables
  :initable-instance-variables
  (:required-init-keywords :option :value :object))

))

; From modified file OPUS: L.TAPE; ERROR.LISP#47 at 18-Jan-88 14:01:56
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (invalid-option-value :report) (stream)
  (format stream "Invalid value ~s for option ~a, for object ~S"
	  value option object))

))

; From modified file OPUS: L.TAPE; ERROR.LISP#47 at 18-Jan-88 14:01:58
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(compile-flavor-methods invalid-option-value)

))
