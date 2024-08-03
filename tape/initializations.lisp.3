;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Final initializations that must be loaded after
;;; all tape support
;;;

;;; The order of the next two initializations is crucial
(add-initialization "Determine available devices"
		    `(progn (setq *available-devices* nil)
			    (dolist (list *tape-device-alist*)
			      (when (funcall (third list))
				(push (second list) *available-devices*))))
		    `(:now)
		    'si:tape-warm-initialization-list)

(add-initialization "Select default device and format"
		    `(progn
		       (setq *selected-device*
			     (cond ((null *available-devices*))
				   ((memq (car *default-device*) *available-devices*)
				    (lexpr-funcall 'parse-device *default-device*))
				   (*available-devices*
				    (parse-device (car *available-devices*)))))
		       (setq *selected-format*
			     (cond (*default-format*
				    (lexpr-funcall 'parse-format *default-format*))
				   (*tape-format-alist*
				    (parse-format (cdar *tape-format-alist*))))))
		    '(:now)
		    'si:tape-warm-initialization-list)
