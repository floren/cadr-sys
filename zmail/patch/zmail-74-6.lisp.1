;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.6
;;; Reason:
;;;  Fixes ZMail blowout bug caused by clicking in summary window while
;;;  editing a message.  When mouse input is received from outside the
;;;  message editing window, ZMail now gives user the option of exiting from
;;;  the current edit session (via an fquery in the mini-buffer).  
;;;  
;;;  If user declines, Zmail complains of unknown mouse input, and the user
;;;  may continue editing.  If user accepts, Zmail quits the edit BUT DOES
;;;  NOT PROCESS THE MENU COMMAND/SUMMARY WINDOW ACTION INDICATED.  User must
;;;  reclick after escaping from the editing session.  
;;;  
;;;  This behavior is analogous to the ZMail clicking behavior, while sending
;;;  mail, for clicking between original-msg, header, and reply-text panes.
;;;  The first click encountered from outside the current pane simply
;;;  replaces drops cursor at the top of the moused-in window, NOT to the
;;;  actual point clicked in the window.  A second mouse click after the
;;;  first has taken effect is required in order to actually move
;;;  zwei:*point* to the desired place in the pane.
;;; Written 3-Oct-88 00:49:01 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.99, Experimental ZWEI 126.14, Experimental ZMail 74.4, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZMAIL; COMNDS.LISP#595 at 3-Oct-88 00:49:12
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFCOM COM-QUIT-ZMAIL-EDIT "Exit editing the message." ()
  ;; in case user exited via the mouse while a region was highlighted...
  (progn (setf (window-mark-p *window*) nil)
	 (send *window* :redisplay nil nil nil nil)
	 (*THROW 'RETURN-FROM-COMMAND-LOOP T)))

))

; From modified file DJ: L.ZMAIL; WINDOW.LISP#354 at 3-Oct-88 00:49:30
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(defmethod (zmail-window :around :process-special-command)
	   (cont mt args &rest ignore)	;used to have TYPE arg after ARGS.
  (let ((item (cdr args)))
    (selectq (car-safe item)
      ((zwei:summary-mouse :menu)		; blip type
       ;; We really can't handle menu commands when we're composing mail or replying, because
       ;; many of the commands refer to the current messsage, and so on.
       (if (fquery '(:type :tyi)
		   "~&Mouse input outside of edit window not recognized until editing is done.~
		    ~&Do you wish to exit the current editing session now? ")
	   (com-quit-zmail-edit)
	 (barf "Illegal mouse command of type ~A" (car item))))
      (read-background-response-queue
       ;;Gross crock!  This command manages to filter through here, and ZWEI doesnt have
       ;; a handler, so it bombs.  Right thing is to ignore it here so it filters
       ;; thru the main loop.  --rg 12/08/86
       nil)
      (otherwise (lexpr-funcall-with-mapping-table cont mt args)))))

))
