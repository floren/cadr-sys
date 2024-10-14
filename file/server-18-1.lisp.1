;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 18.1
;;; Reason:
;;;  (fs:start-disk-space-warner-process) new feature.
;;; Written 30-Jan-86 16:56:22 by GJC (George Carrette) at site LMI Cambridge
;;; while running on Djinn from band 2
;;; with Experimental System 110.12, Experimental Local-File 66.0, Experimental FILE-Server 18.0, Experimental MagTape 4.0, Experimental ZMail 65.1, Experimental Unix-Interface 9.0, microcode 1363.



; From file DJ: L.FILE; SERVER.LISP#180 at 30-Jan-86 16:56:23
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "


(defvar *disk-space-warner-interval* 10. "Interval for warning, in minutes")
(defvar *disk-space-warner-threshold* 800. "Warning level for disk space")
(defvar *disk-space-warner-process* () "The actual process doing the work")

(defun disk-space-warner-function ()
  (do-forever
    (process-wait "disk space watch"
		  #'(lambda ()
		      (< (aref put-usage-array put-free) *disk-space-warner-threshold*)))
    (let ((message (format () "Disk space low !  Only ~D blocks free."
			   (aref put-usage-array put-free))))
      (ignore-errors
	(tv:notify () "~A" message)
	(blast-message-to-file-servers message)))
    (process-sleep (* 60. 60. *disk-space-warner-interval*))))
    
(defun start-disk-space-warner-process ()
  (or *disk-space-warner-process*
      (setq *disk-space-warner-process* (make-process "Disk Space Watch" :priority -2)))
  (SEND *disk-space-warner-process* :PRESET 'DISK-SPACE-WARNER-FUNCTION)
  (PROCESS-RESET-AND-ENABLE *disk-space-warner-process*))
))
