;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 72.1
;;; Reason:
;;;  Eliminates ":MENU is not a valid special editor command" errors when
;;;  clicking on something during draft editing.  Now, user gets a message
;;;  that "You can't use the <xxx> command here."
;;; Written 16-Jun-88 15:57:02 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Brahms' First from band 3
;;; with Experimental System 124.59, Experimental Local-File 74.2, Experimental File-Server 23.1, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.6, Experimental Lambda-Diag 16.1, Tiger 28.0, microcode 1759, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZMAIL; WINDOW.LISP#353 at 20-Jun-88 12:06:11
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; WINDOW  "

(defmethod (zmail-window :around :process-special-command)
	   (cont mt args &rest ignore)	;used to have TYPE arg after ARGS.
  (let ((item (cdr args)))
    (cond ((and (listp item)
		(eq (car item) :menu))		; blip type
	   (let ((name (caadr item)))
	     ;; We really can't handle menu commands when we're composing mail or replying, because
	     ;; many of the commands refer to the current messsage, and so on.
	     (barf "~&You can't use the ~A menu command here." (or name ""))))
	  ((and (listp item)
		(eq (car item) 'read-background-response-queue))
	   ;;Gross crock!  This command manages to filter through here, and ZWEI doesnt have
	   ;; a handler, so it bombs.  Right thing is to ignore it here so it filters
	   ;; thru the main loop.  --rg 12/08/86
	   nil)
	  (t (lexpr-funcall-with-mapping-table cont mt args)))))

))
