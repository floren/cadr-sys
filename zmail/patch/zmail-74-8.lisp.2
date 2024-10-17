;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.8
;;; Reason:
;;;  ZMail now handles mouse clicks more robustly in message editing mode.
;;;  
;;;  If, while editing a message in the bottom (message text) window, a user
;;;  clicks any mouse button in the top (summary) window, the editing is
;;;  saved, :edit mode is exited, and Zmail jumps to the clicked-on message
;;;  line.
;;;  
;;;  If, while editing, the user clicks on a menu command, ZMail warns the
;;;  user that menu commands are not executable from within :edit mode, and
;;;  then fquery's the user as to whether or not to exit :edit mode at that
;;;  time.  If the user responds with "Y", :edit mode is exited, all editing
;;;  to the message is saved in the body of the message, the user is
;;;  presented with a reminder to click again to execute the particular menu
;;;  command, and the top level command loop is re-entered.
;;;  
;;;  If the user responds with "N", then ZMail simply reminds you that the
;;;  <End> and the <Abort> keys may be used to exit an edit session when you
;;;  are finished with the text.
;;; Written 5-Oct-88 02:02:36 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.100, Experimental ZWEI 126.14, Experimental ZMail 74.6, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: L.ZMAIL; WINDOW.LISP#355 at 5-Oct-88 02:03:05
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(defmethod (zmail-window :around :process-special-command)
	   (cont mt args &rest ignore)	;used to have TYPE arg after ARGS.
  (setf (get 'ZMAIL-SUMMARY-MOUSE :WHO-LINE-DOCUMENTATION)
	"Click the mouse on a message line to save editing changes and jump to this message.")
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

; From modified file DJ: L.ZMAIL; COMNDS.LISP#597 at 5-Oct-88 02:03:27
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "


(defun cleanup-message-window ()
  ;; in case user exited via the mouse while a region was highlighted...
  (setf (window-mark-p *window*) nil)
  (send *window* :redisplay dis-all nil nil nil))

))

; From modified file DJ: L.ZMAIL; COMNDS.LISP#597 at 5-Oct-88 02:03:33
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "


(DEFCOM COM-QUIT-ZMAIL-EDIT "Exit editing the message." ()
  (progn (cleanup-message-window)
	 (*THROW 'RETURN-FROM-COMMAND-LOOP T)))

;;; Keyword stuff

;;; List of keywords on, keywords off

))
