;;; -*- Mode:Lisp;Package:FS;Base:8 -*-
;;; VAX tape utilities

(defun restore-vax-tape (&OPTIONAL directory ufilename &REST options &AUX in-stream filename)
  (or directory
      (setq directory
	    (progn (format T "~%Please type the directory name for these files: ")
		   (readline))))
  (do ()
      ((eq ':EOT (setq filename (or ufilename
				    (read-vax-filename-from-header options)))))
    (SETQ IN-STREAM (LEXPR-FUNCALL #'MAKE-MT-STREAM ':ERROR NIL OPTIONS))
    (with-open-file (os (make-pathname ':DIRECTORY directory
				       ':NAME filename
				       ':TYPE "TEXT")
			'(:write))
      (format T "~%Restoring vax file ~A into file ~A "
	      filename (funcall (funcall os ':TRUENAME) ':STRING-FOR-PRINTING))
      (do ((ch (funcall in-stream ':tyi) (funcall in-stream ':tyi))
	   (base 10.) (ibase 10.) (line-length 0)
	   (counter 1 (1+ counter)))
	  ((null ch))
	(setq ch (ascii-to-lispm ch))
	(cond (( 0 counter 4)
	       (if ( #/0 ch #/9)
		   (setq line-length (+ (* 10. line-length) (- ch #/0)))
		 (format T "~%Ignoring file due to bad record header")
		 (funcall os ':delete)
		 (return)))
	      ((> counter line-length)
	       (cond (( #/0 ch #/9)
		      (funcall in-stream ':untyi ch)
		      (setq counter 0
			    line-length 0))
		     ((string-equal ch "^"))
		     (T (ferror nil "Bad line header encountered."))))
	      (ch (funcall os ':tyo ch)))
	(cond ((and (= line-length counter)
		    ( counter 4))
	       (funcall os ':tyo #\cr))))
      (funcall in-stream ':NEXT-FILE)
      (funcall in-stream ':CLOSE ':RAW))
    (read-eof-file)))

(defun read-vax-filename-from-header (options &AUX in-stream line start)
  (setq in-stream (lexpr-funcall #'MAKE-MT-STREAM ':ERROR NIL options))
  (setq line (funcall in-stream ':LINE-IN))
  (funcall in-stream ':NEXT-FILE)
  (funcall in-stream ':CLOSE ':RAW)
  (if (equal line "") ':EOT
    (substring line (setq start (+ 4 (string-search "HDR1" line)))
	       (string-search-char #\SPACE line start))))


(defun write-vax-tape (filename &OPTIONAL (record-size 120.) &AUX out-stream)
  (setq out-stream (make-mt-stream ':DIRECTION ':OUTPUT ':RECORD-SIZE record-size))
  (with-open-file (in-stream filename)
    (LOOP WITH buff-length 
	  DO
	  (multiple-value-bind (temp-buffer eof)
	      (funcall in-stream ':LINE-IN)
	    (cond ((< (setq buff-length (string-length temp-buffer))
		      record-size)
		   (setq temp-buffer (string-append temp-buffer
						    (make-array (- record-size buff-length)
								':type 'art-string
								':initial-value #\SPACE)))))
	    (funcall out-stream ':STRING-OUT TEMP-BUFFER)
	    (if eof (loop-finish)))))
  (funcall out-stream ':close)
  (mt-write-eof))