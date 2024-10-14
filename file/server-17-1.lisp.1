;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 17.1
;;; Reason:
;;;  Hook into file-systems' notification stuff; in particular the disk space warning
;;;   in Local-File 64.1
;;; Written 12-Nov-85 19:11:20 by rpk of LMI Cambridge
;;; while running on Guinea Pig from band 3
;;; with Experimental System 109.10, Experimental Local-File 64.0, Experimental FILE-Server 17.0, Experimental MagTape 3.0, microcode 1303, GC FS LAM.


;; *** Note: ***
;;   You may lose because the buffer has no readtable attribute.
;; *************

; From modified file DJ: L.FILE; SERVER.LISP#168 at 12-Nov-85 19:11:20
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "


(defvar *chaos-file-server-notifier-sleep-time* 300. "Sleep time in seconds")

(defun chaos-file-server-notifier-top-level ()
  (let ((notifiers ()))
    (dolist (flavor (send si:local-host :appropriate-access-flavors))
      (let ((function (get flavor 'notification-for-server)))
	(when function (push function notifiers))))
    (loop
      (dolist (notifier notifiers)
	(let ((message (funcall notifier)))
	  (when message
	    (blast-message-to-file-servers message))))
      (sleep *chaos-file-server-notifier-sleep-time* "QFILE Server Notifier"))))

(add-initialization "QFILE Server Notifier"
		    '(process-run-function '(:name "QFILE Server Notifier" :restart-after-boot t
					     :priority -2)
					   'chaos-file-server-notifier-top-level)
		    '(:enable-services))

))
