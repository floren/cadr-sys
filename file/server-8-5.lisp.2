;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 5/24/84 06:12:27 by Mly,
;;; Reason: parsing file properties


; From file SERVER.LISP PS:<L.FILE> OZ: (149)
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun change-props-1 (actor fh strings)
  (loop with sym
	for string in strings
	as spacex = (string-search-char #\sp string)
	unless spacex do
	(format conn-stream "~A ~A ERROR STX F Ill formated property spec: ~A" tid fh string)
	nconc (list* (setq sym (si:intern1 (substring string 0 spacex)
					   si:pkg-keyword-package))
		     (server-convert-known-file-property string (1+ spacex) sym)
		     nil)
	into plist
	finally (trap-lossage (error "Change properties")
		      (let ((m (lexpr-funcall actor ':change-properties nil plist)))
			(if (errorp m)
			    (format conn-stream "~A ~A ERROR LOS F ~A" tid fh m)
			  (format conn-stream "~A ~A CHANGE-PROPERTIES" tid fh)))
		  (format conn-stream "~A ~A ERROR SYS F Internal error:~% ~A"
			  tid fh trap-error))))

))
