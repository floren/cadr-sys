;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 2/09/84 01:39:06 by RMS,
;;; Reason: KBD-ESC-NOTIFICATIONS.
;;; Globalize PKG-KEYWORD-PACKAGE, PKG-SYSTEM-PACKAGE, PACKAGE-EXTERNAL-SYMBOLS.
;;; while running on Lisp Machine Eighteen from band ?
;;; with System 98.34, CADR 3.6, ZMail 53.10, MIT-Specific 22.0, microcode 306.

(globalize 'si:pkg-keyword-package)
(globalize 'si:pkg-system-package)
(globalize 'si:package-external-symbols)

; From file BASSTR.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN KBD-ESC-NOTIFICATIONS (ARG &AUX NN TEM)	;esc n
  "Display notifications in a pop-up window.
With no argument, pending notifications only are printed. If there are none, we say so.
With an argument of T, pending notifications only are printed.
  If there are none, we return immediately.
With an argument of 1, displays all notifications.
With an argument of 2, prints nothing, but marks pending
 notifications as no longer pending."
  ;;do this before selecting new window -- don't get screwed by the wait-to-notify process
  (WITHOUT-INTERRUPTS
    (SETQ NN (APPEND PENDING-NOTIFICATIONS DEFERRED-NOTIFICATIONS)
	  DEFERRED-NOTIFICATIONS NIL
	  PENDING-NOTIFICATIONS NIL))
  (IF (SELECTQ ARG
	((NIL) T)
	((T) NN)
	(1 T)
	(2 (SETQ DEFERRED-NOTIFICATIONS NN)
	   NIL))
      (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
	(SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
	(FUNCALL WINDOW ':SET-LABEL "Notifications -- most recent first")
	(WINDOW-CALL (WINDOW :DEACTIVATE)
	  (SETQ KBD-ESC-TIME NIL)
	  ;;crock to let us catch more notifications while this window is exposed
	  (DO () (())
	    (WITHOUT-INTERRUPTS
	      (SETQ NN (APPEND PENDING-NOTIFICATIONS NN) PENDING-NOTIFICATIONS NIL))
	    (FUNCALL WINDOW ':CLEAR-SCREEN)
	    (FORMAT WINDOW "~:[[There are no pending notifications]~;New notifications:~]~%"
		    NN)
	    (SETQ TEM NOTIFICATION-HISTORY)
	    (IF NN
		(DOLIST (N NN)
		  (TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST N) WINDOW)
		  (FORMAT WINDOW ": ~a~&" (SECOND N))
		  (POP TEM)))	;don't duplicate messages -- unseen messages are also in tem
	    (WHEN ARG
	      (FORMAT WINDOW "~%~:[[No Notifications]~;Previous notifications:~]~%" TEM)
	      (DOLIST (N TEM)
		(TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST N) WINDOW)
		(FORMAT WINDOW ": ~a~&" (SECOND N))))
	    (FORMAT WINDOW "~%Type a space to flush: ")
	    (PROCESS-WAIT "Keyboard" #'(LAMBDA (WINDOW)
					 (OR PENDING-NOTIFICATIONS
					     (NEQ SELECTED-WINDOW WINDOW)
					     (FUNCALL WINDOW ':LISTEN)))
			  WINDOW)
	    (OR PENDING-NOTIFICATIONS
		(NEQ SELECTED-WINDOW WINDOW)
		(FUNCALL WINDOW ':TYI-NO-HANG))
	    (OR PENDING-NOTIFICATIONS (RETURN T)))))))

))
