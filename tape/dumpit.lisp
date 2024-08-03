;;; -*- Mode:LISP; Package:(DUMPIT :USE ("LISP" "GLOBAL")); Base:10; Readtable:CL -*-

;;; DUMPIT - formatted hex/text dumping routine.

;;; Remember the tape dump routines from the "good old days"... back when
;;; trading foreign tapes -- blocking and deblocking -- were daily, mystical
;;; events...  DUMPIT provides the formatting capability to hex-dump any 8-bit
;;; array object considered as a string of characters.

(in-package 'dumpit :use '("LISP"
			   #+(or LMI TI) "GLOBAL"
			   ))

(export '(dumpit charmap hexmap))

;;; Global/special parameters:

(defconst *default-chars-per-line* 32.)

(defconst *default-left-columns* 8.)

(defconst *default-hex-too* t)

(defconst *nongraphic-mode* nil
  "Non-NIL if formatting to non-graphic (e.g. hardcopy) device, where we
might not get the full machine-specific character representations.")

;;; Machine- or representation-dependent definitions:

(defconst *byte-type-spec* '(mod 256)
  "A type specifier which defines byte size")

#+(or LMI SYMBOLICS TI)
(defun represent-bogus-char()
  (declare(special *nongraphic-mode*))
  (if *nongraphic-mode* #\. 0))
#-(or LMI SYMBOLICS TI)
(defun represent-bogus-char() #\.)
    
(defun printable-char(char)			;Should be truly portable
  "If CHAR is not graphic, return a printable
representation from (REPRESENT-BOGUS-CHAR),
else return CHAR."
  (if (and (graphic-char-p char)
	   (or (not *nongraphic-mode*)
	       (standard-char-p char)))
      char
    (represent-bogus-char)))

;;; Pre-defined dumping formats

(defparameter *formats*
	      '((:hardcopy-standard
		  :nongraphic t
		  :chars-per-line 25
		  :left-columns 4
		  :hex-too t)
		(:screen-standard
		  :chars-per-line 25
		  :left-columns 6
		  :hex-too t)
		(:hardcopy-portrait-text
		  :nongraphic t
		  :chars-per-line 64
		  :left-columns 6
		  :hex-too nil)
		(:hardcopy-landscape-text
		  :nongraphic t
		  :chars-per-line 100
		  :left-columns 6
		  :hex-too nil)
		))

(defparameter *default-format* :screen-standard)

(defun handle-format-error(fmt)
  (let((msg "~s is not a valid DUMPIT format; valid formats are:~% ~s")
       (default-format *default-format*))
    (when (and (not(keywordp fmt))
	       (keywordp default-format))
      (if (and (symbolp fmt)
	       (assoc(intern fmt 'keyword)
		     *formats*))
	  (progn
	    (setq default-format (intern fmt 'keyword))
	    (setq msg (string-append
			msg
			(format nil "~%[Hint: Do you mean ~s ?]~%"
				default-format))))
						;ELSE
	(setq msg (string-append
		    msg 
		    (format nil
			    "~%[Hint: DUMPIT formats are KEYWORDS, e.g. ~s]~%"
			    default-format)))))
    (cerror "Use the default format: ~*~*~s"
	    msg fmt
	    (mapcar #'car *formats*)
	    default-format)
    default-format))

;;; Formatting utility routines

(defun calculate-width-from-format(fmt-list)
  (let((cpl (getf fmt-list :chars-per-line *default-chars-per-line*))
       (lc (getf fmt-list :left-columns *default-left-columns*))
       (h2 (getf fmt-list :hex-too *default-hex-too*)))
    (+ lc
       cpl
       (if h2 (+ 2 (* cpl 2)) 0))))

(defun calculate-format-from-line-width(width &optional (hex-too t))
  (block nil
    (let((cpl (floor (if hex-too (- width 8) width) 3)))
      (when (< cpl 3)
	(ferror nil "Cannot calculate format from arguments."))
      (setq cpl
	  (if (oddp cpl)
	     (if (zerop (mod cpl 5)) cpl
	       (return (calculate-format-from-line-width (1- width))))
	    (cond
	      ((zerop (mod cpl 10)) cpl)
	      ((zerop (mod cpl 4)) cpl)
	      (t (- cpl 2)))))
      (list :chars-per-line cpl
	    :left-columns 6
	    :hex-too t))))

(defun get-format-list(fmt)
  (or
    (typecase fmt
      (null
       (calculate-format-from-line-width
	 (or (send-if-handles standard-output :size-in-characters)
	     80)))
      (list fmt)
      (symbol (cdr (assoc fmt *formats*))))
    (get-format-list (handle-format-error fmt))))

;;; Stream for handling duplicate lines

(defun print-duping-stream(op &optional str &rest rest)
  (declare(special stream laststring dup-msg asabove threshold check-from))
  (selectq op
    (:printdup
     (if (string-equal str laststring :start1 check-from :start2 check-from)
	 (incf asabove)
       (progn
	 (cond
	   ((zerop asabove))
	   ((greaterp asabove threshold)
	    (format stream dup-msg asabove))
	   (t (format stream "~&~a" laststring)))
	 (setq asabove 0)
	 (if (stringp str)
	     (format stream "~&~a" str))))
     (setq laststring str))
    (:flush
     (print-duping-stream :printdup :flushing))
    (:set-dup-msg
     (setq dup-msg str))
    (:check-from
     (setq check-from str))
    (otherwise
     (stream-default-handler stream op str rest))))

(defun make-duping-stream(stream &optional (threshold 1))
  (declare(special stream threshold))
  (let((laststring "")
       (dup-msg "~&<<<~d lines repeated as above>>")
       (asabove 0)
       (check-from 0))
    (declare(special stream laststring dup-msg asabove threshold check-from))
    (closure '(stream laststring dup-msg asabove threshold check-from)
	     'print-duping-stream)))

;;; Main routines

(defun hexmap(a &key
	      (columns 2)
	      (width (array-length a)))
  (let ((len (array-length a)))
    (with-output-to-string (stream)
      (dotimes (j width)
	(if (< j len)
	    (format stream "~v,'0x" columns (char-int (aref a j)))
	  (format stream "~a" (make-string columns :initial-element #\space)))))))

(defun charmap(a &key
	       (len (array-length a))
	       (chars-per-line *default-chars-per-line*)
	       (left-columns *default-left-columns*)
	       (hex-too *default-hex-too*)
	       (skip-dups t)
	       nongraphic)
  (let((*nongraphic-mode* nongraphic)
       (stream (make-duping-stream standard-output))
       str)
    (send stream :check-from left-columns)
    (do* ((start 0 (incf start chars-per-line))
	  (end (min len chars-per-line) (min (+ start chars-per-line) len)))
	 (nil)
      (when (>= start len)
	(send stream :flush)
	(format stream "~&")
	(return))
      (setq str (substring a start end))
      (send stream
	    (if skip-dups :printdup :line-out)
	    (string-append
	      (format nil "~&~vd|" (1- left-columns) start)
	      (if hex-too
		  (string-append (hexmap str :width chars-per-line) "|")
		"")
	      (with-output-to-string(s)
		(do ((i start (incf i)))
		    ((= i end))
		  (format s "~c"
			  (printable-char (aref a i))))))))))

(defun dumpit(arr &optional (fmt *default-format*))
  (apply #'charmap arr (get-format-list fmt)))

#|
;;;Testing routines

(defun make-test-array()
  (let*((len 512) (halflen 256)
	(a (make-array len
		      :element-type *byte-type-spec*
		      :initial-element 0.)))
    (dotimes (i halflen)
      (setf (aref a i) i))
    a))

(defvar *s* nil "test variable")

(eval-when (load)
  (setq *s* (make-test-array)))

(defun tst(fmt)
  (format t "~2%~a" (make-string 80 :initial-element #\x))
  (dumpit *s* fmt))

(defun tsts()
  (dolist (fmt (mapcar #'car *formats*))
    (format t "~%+++ ~a~%" fmt)
    (tst fmt)))

|#
