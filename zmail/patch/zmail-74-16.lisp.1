;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.16
;;; Reason:
;;;  Further maintenance for message editing who-line documentation string.
;;;  Thanks to Keith for the suggestion.
;;; Written 8-Nov-88 10:46:04 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.141, Experimental ZWEI 126.28, Experimental ZMail 74.15, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, ) (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: L.ZMAIL; WINDOW.LISP#358 at 8-Nov-88 10:46:04
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(DEFMETHOD (ZMAIL-WINDOW :AROUND :EDIT) (CONT MT ARGS &REST IGNORE)
  (SYS:%BIND (LOCF (TV:BLINKER-DESELECTED-VISIBILITY POINT-BLINKER))
	     :ON)
  (let-globally ((who-line-override-documentation-string
		   #.(string-append "Edit message in buffer; press "
				    #/end
				    " to save changes, or click in summary window to save and jump.")))
    (LET ((*SELECTABLE-MODE-LINE-ELEMENTS* NIL))
      (AROUND-METHOD-CONTINUE CONT MT ARGS))))
))
