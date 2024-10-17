;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.15
;;; Reason:
;;;     Fixes bug in which Initial Process gets an error when user logs out,
;;;  but was editing one of his messages when he last left ZMail.
;;;     Also sets who-line-documentation during msg-editing mode to indicate
;;;  that user can click on a message in the summary window to exit editing
;;;  mode.
;;; Written 7-Nov-88 13:36:51 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.141, Experimental ZWEI 126.28, Experimental ZMail 74.14, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, ) (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: L.ZMAIL; WINDOW.LISP#357 at 7-Nov-88 13:36:51
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(DEFMETHOD (ZMAIL-WINDOW :AROUND :EDIT) (CONT MT ARGS &REST IGNORE)
  (SYS:%BIND (LOCF (TV:BLINKER-DESELECTED-VISIBILITY POINT-BLINKER))
	     :ON)
  (setf (fill-pointer (get 'ZMAIL-SUMMARY-MOUSE :WHO-LINE-DOCUMENTATION)) 0)
  (setf (get 'ZMAIL-SUMMARY-MOUSE :WHO-LINE-DOCUMENTATION)
	(array-push-portion-extend
	  *edit-msg-documentation*
	  "Edit msg with normal Zwei commands; click mouse in summary window to save changes and jump to new msg."))
  (LET ((*SELECTABLE-MODE-LINE-ELEMENTS* NIL))
    (AROUND-METHOD-CONTINUE CONT MT ARGS)))
))

; From modified file DJ: L.ZMAIL; WINDOW.LISP#357 at 7-Nov-88 13:36:58
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(defmethod (zmail-window :around :process-special-command)
	   (cont mt args &rest ignore)	;used to have TYPE arg after ARGS.
  (let ((item (cdr args)))
    (selectq (car-safe item)
      (zwei:summary-mouse
       ;; We really can't handle menu commands when we're composing mail or replying, because
       ;; many of the commands refer to the current messsage, and so on.
       (progn (zmail-select-msg (msg-displayed-index (cadadr item)))
	      (com-quit-zmail-edit)))
      (:menu		; blip type
       (if (fquery '(:type :tyi) "To execute the command /"~A/", ~
 				  you must first return from editing this message.~
				~%Return from editing this message now? " (car (second item)))
	   (progn (cleanup-message-window)
		  (format *query-io* "Click again to execute the /"~A/" command" (car (second item)))
		  (com-quit-zmail-edit))
	 (format *query-io* "Type ~\lozenged-character\ to abort, or ~\lozenged-character\ to save ~
			     and return from editing this message."
		 #/Abort #/End)))
      (read-background-response-queue
       ;;Gross crock!  This command manages to filter through here, and ZWEI doesnt have
       ;; a handler, so it bombs.  Right thing is to ignore it here so it filters
       ;; thru the main loop.  --rg 12/08/86
       nil)
      (otherwise (lexpr-funcall-with-mapping-table cont mt args)))))

))
