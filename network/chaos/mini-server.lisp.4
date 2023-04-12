;-*- mode: lisp; package: si; base: 8 -*-

(defun mini-server nil
  (setq chaos:host-down-interval (* 60. 60. 15.))	;15 minutes ... 3 in standard system
  (do (conn pkt op file-name characters-p pathname
       (defaults (fs:parse-pathname "dj:l.sys;")))
      (())
    (ignore-errors
      (condition-bind ((nil #'(lambda (condition-instance)
				(cond ((or (send condition-instance ':debugging-condition-p)
					   (send condition-instance ':dangerous-condition-p))
				       nil)
				      (t (format t "~&error: ~a"
						 (send condition-instance ':report-string))
					 nil)))))
	(unwind-protect
	  (progn
	    (setq conn (chaos:listen "MINI"))
	    (chaos:accept conn)
	    (format t "~&connection open")
	    (do-forever
	      (setq pkt (chaos:get-next-pkt conn))
	      (setq op (chaos:pkt-opcode pkt))
	      (setq file-name (chaos:pkt-string pkt))
	      (setq characters-p (= op 200))
	      (chaos:return-pkt pkt)
	      (setq pathname (fs:merge-pathname-defaults file-name defaults))
	      (format t "~&opening ~a" pathname)
	      (with-open-file-retry (file-stream (pathname sys:network-error fs:file-error)
						 ':characters characters-p)
		(format t " --got it--")
		(let ((response-pkt (chaos:get-pkt)))
		  (chaos:set-pkt-string response-pkt (format nil "~A~%~O"
							     (send (send file-stream ':truename)
								   ':string-for-printing)
							     (funcall file-stream
								      ':creation-date)))
		  (chaos:send-pkt conn response-pkt 202))
		;now send the file
		(let ((outstream (chaos:make-stream conn ':direction ':output
						    ':characters characters-p)))
		  (stream-copy-until-eof file-stream outstream)
		  (funcall outstream ':eof)))))
	  (cond ((not (null conn))
		 (ignore-errors (chaos:close conn))
		 (chaos:remove-conn conn)
		 (setq conn nil)
		 )))))))
