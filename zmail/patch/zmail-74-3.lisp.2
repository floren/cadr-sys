;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.3
;;; Reason:
;;;  1. Fix doc string for (ZMAIL). {bletch!}
;;;  
;;;  2. Fix UPDATE-ZMAIL-PROFILE-CHOICE-WINDOW so if Zmail window is not
;;;  found (e.g.  if user killed Zmail), it doesn't blow out.  Now it offers
;;;  to create ZMail window.  This is a big improvement for future patches
;;;  that define profile variables; this typically gets called within a
;;;  patch, and errors are unfortunate.
;;; Written 29-Sep-88 21:06:55 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 1
;;; with Experimental System 126.94, Experimental ZWEI 126.13, Experimental ZMail 74.2, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZMAIL; TOP.LISP#561 at 29-Sep-88 21:06:59
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; TOP  "

(defun find-or-maybe-initialize-zmail-window (&optional reason (timeout (* 60. 60. 3.)) default)
  (or (tv:find-window-of-flavor 'zmail-frame)
      (and (yes-or-no-p-with-timeout timeout default "Start up ZMail~@[ ~A~]?" reason)
	   (prog2 (initialize-zmail) *zmail-window*))))

))

; From modified file DJ: L.ZMAIL; DEFS.LISP#283 at 29-Sep-88 21:07:13
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; DEFS  "


;;; Call this to update the item list of the Profile CVV window.  Needs to be done when patches
;;; call DEFINE-ZMAIL-USER-OPTION.
;;;>>>Then hope patches don't get loaded in world where ZMail is not running
;;;   (maybe user killed it).  So, at least check first.  If user says to make
;;;   a window, do so.

(defun update-zmail-profile-choice-window ()
  "Update the ZMail profile window from the current user option (variables) list.
If ZMail is not up and running, the user is asked whether to start it up (default is no)."
  (let ((zmail (find-or-maybe-initialize-zmail-window "to update profile window")))
    (if zmail
	(prog2 (send
		 (send (symeval-in-closure (send zmail :editor-closure) '*profile-window*)
		       :get-pane 'choose-window)
		 :set-items *zmail-user-option-alist*)
	       t)
      (warn "No ZMail window running -- cannot update profile window"))))

))

; From modified file DJ: L.ZMAIL; TOP.LISP#562 at 29-Sep-88 21:25:33
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; TOP  "

;;; Top level function
(DEFUN ZMAIL (&OPTIONAL SAVED-MAIL-PATHNAME)
  "Calls up a ZMAIL, a mail reading program.
If SAVED-MAIL-PATHNAME is specified it should be either
  - :RELOAD, to reinitialize for user /(reading in mail), or
  - a pathname, to read in a mail file."
  (COND ((memq SAVED-MAIL-PATHNAME '(RELOAD :reload))
	 (INITIALIZE-ZMAIL)
	 (SETQ SAVED-MAIL-PATHNAME NIL)))
  (FS:FORCE-USER-TO-LOGIN)
  (SEND *ZMAIL-WINDOW* :SET-ZMAIL-BUFFER SAVED-MAIL-PATHNAME)
  (SEND *ZMAIL-WINDOW* :SELECT)
  (TV:AWAIT-WINDOW-EXPOSURE)
  T)

))
