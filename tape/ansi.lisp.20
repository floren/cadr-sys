;;; -*- Mode:LISP; Package:(ANSI GLOBAL); Readtable:T; Base:10 -*-

#|

Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
licensing and release information.

Read an ANSI-labeled magnetic tape as described by VAX/VMS tape/disk
documentation appendex B.

Note: The complete description is in American National Standard X3.27-1978
      which I did not read.  2/17/85 15:20:08 -George Carrette

Let {} delimit physical markers, [] controller handled markers i.e. tape marks,
and <> delimit software defined markers, and elipses "..." show areas of tape with
data. Then our Multifile/Single Volume tape of interest looks like this:

{BOT}<VOL1><HDR1><HDR2><HDR3>[TM]...1...[TM]<EOF1><EOF2><EOF3>[TM]
           <HDR1><HDR2><HDR3>[TM]...2...[TM]<EOF1><EOF2><EOF3>[TM]
           <HDR1><HDR2><HDR3>[TM]...3...[TM]<EOF1><EOF2><EOF3>[TM]
           <HDR1><HDR2><HDR3>[TM]...N...[TM]<EOF1><EOF2><EOF3>[TM][TM]...scratch...{EOT}

The Begining Of Tape and End Of Tape markers are aluminized areas on the tape.
Searching for the next [TM] marks is an ability of the hardware controller.

The documentation with the KERMIT distribution tape says also that
there may be <EOV1> and <EOV2> labels at the end of the tape, then the double [TM][TM]
so we need to check for this posibility.

|#

;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:25
(defun compute-lispm-char (j)
  (or (cadr (assq j '((#o177 #\rubout)
		      (#o10 #\overstrike)
		      (#o11 #\tab)
		      (#o12 #\line)
		      (#o14 #\page)
		      (#o15 #\return))))
      j))

;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:25
(defun make-ascii-code-array (&aux (a (make-array #o400)))
  (dotimes (j #o400)
    (setf (aref a j) (compute-lispm-char j)))
  a)

(defvar *ascii-code-array* (make-ascii-code-array))

(defun make-ascii-to-lispm-stream (ascii-stream &aux myself)
  (setq myself #'(lambda (operation &optional arg &rest l)
		   (selectq operation
		     (:which-operations '(:tyi))
		     (:tyi
		      (and (setq arg (send ascii-stream :tyi))
			   (aref *ascii-code-array* arg)))
		     (t
		      (stream-default-handler myself operation arg l)))))
  myself)


;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:26
(defmacro with-mt-stream ((var . options) &body body &aux (x (gentemp)))
  (or options (setq options '(:characters nil)))
  ;; bug in Daves new tape software, seems that :close
  ;; operation will search to next file mark, even if
  ;; you have already gotten to the file mark by reading all the data!
  ;; so I'll remove the close here, which is not really needed in
  ;; normal operation anyway.
  ;;  `(let (,x)
  ;; (unwind-protect
  ;;	 (let ((,var (setq ,x (fs:make-mt-stream ,@options))))
  ;;	   ,@body)
  ;;       (and ,x (close ,x)))))
  x
  `(let ((,var (fs:make-mt-stream ,@options)))
     ,@body))


(defun string-to-next-tm ()
  (with-mt-stream (in)
    (with-output-to-string (out)
      (stream-copy-until-eof (make-ascii-to-lispm-stream in) out))))

;; Leaving data in global variables for development and tape diagnostic purposes.

(defvar *s* nil)
(defvar *j* nil)

(defun init-substring (x)
  (setq *s* x)
  (setq *j* 0))

(defun next-substring (amount)
  (substring *s* *j* (setq *j* (+ *j* amount))))

(defun substring-endp ()
  (= *j* (length *s*)))

(defun next-substring-check (should-be &optional new-index)
  (if new-index (setq *j* new-index))
  (let ((is (next-substring (length should-be))))
    (or (string-equal is should-be)
	(ferror nil "Looking for ~S but found ~S" should-be is))))


(defun next-substring-equal (looking-for &optional new-index)
  (if new-index (setq *j* new-index))
  (string-equal looking-for *s* :start2 *j* :end2 (+ *j* (length looking-for))))

(defvar *label* nil)

(defun decode-volume-label ()
  (setq *label* (list 'volume (next-substring 4)
		      'volume-id (next-substring 6)
		      'accessibility (next-substring 1)
		      'implementation-id (next-substring 10))))


(defvar *header* nil)
(defvar *header-1* nil)
(defvar *header-2* nil)


(defun decode-headers ()
  (next-substring-check "HDR1")
  (setq *header-1* (list 'file-id (next-substring 17)
			 'file-set-id (next-substring 6)))
  (next-substring-check "HDR2" 80)
  (setq *header-2* (list 'record-format (next-substring 1)
			 'block-length (next-substring 5)
			 'record-length (next-substring 5)))
  (setq *header* (append *header-1* *header-2*)))

(defun read-eof-marks ()
  (init-substring (string-to-next-tm))
  (next-substring-check "EOF1")
  (dolist (x '(file-id file-set-id))
    (next-substring-check (getf *header-1* x)))
  (next-substring-check "EOF2" 80)
  (dolist (x '(record-format block-length record-length))
    (next-substring-check (getf *header-2* x))))

(defmacro vformat (level string &rest l)
  `(if (verbose-enough ,level)
       (format t ,string ,@l)))

(defun read-header-labels ()
  (init-substring (string-to-next-tm))
  (cond ((substring-endp)
	 nil)
	((next-substring-equal "EOV1")
	 (vformat 2 "~&End of Volume Labels:~%~A~%~A~%"
		  (next-substring 80)
		  (next-substring 80))
	 ())
	('else
	 (decode-headers)
	 t)))

(defvar *verbose* t "A value of NIL or a level 1, 2, or 3, T = 1")

(defun verbose-plist (symbol)
  (vformat 2 "~&~S:~%" symbol)
  (do ((l (symeval symbol) (cddr l)))
      ((null l))
    (vformat 2 "  ~S = ~S~%" (car l) (cadr l))))

(defun verbose-enough (level)
  (and *verbose*
       (or (and (eq *verbose* t) (= level 1))
	   (and (numberp *verbose*) (>= *verbose* level)))))

(defvar *query* t "If T ask about each file on tape")

(defvar *file-id-append* "LM:TMP;" "string to append to file-id before parsing pathname")

(defvar *skip-if-exists* t "If T skip if file exists on disk. If NIL write new version")

(defun restore-tape (&optional &key
		     (verbose nil)
		     (query t)
		     (file-id-append "LM:TMP;")
		     (skip-if-exists t))
  "Read an ANSI labeled tape, restoring it to the filesystem.
The FILE-ID-APPEND is a string to append to the ANSI file-id (which
contains no directory information), or it is a function to call to
process the file-id into a pathname. VERBOSE is a number from 1 to 3"
  (let ((*verbose* verbose)
	(*query* query)
	(*file-id-append* file-id-append)
	(*skip-if-exists* skip-if-exists)
	*s* *j*)
    (vformat 1 "~&Rewinding the tape~%")
    (fs:mt-rewind)
    (restore-tape-1)))

(defun restore-tape-verbose ()
  (restore-tape :verbose t :query t))

(defun restore-tape-1 ()
  (init-substring (string-to-next-tm))
  (decode-volume-label)
  (verbose-plist '*label*)
  (init-substring (nsubstring *s* 80))
  (decode-headers)
  (verbose-plist '*header*)
  (restore-file-data)
  (read-eof-marks)
  (do ()
      ((null (read-header-labels)))
    (verbose-plist '*header*)
    (restore-file-data)
    (read-eof-marks)))

(defvar *record-format-handlers*
	'(("D" restore-from-variable-length-record-stream)
	  ("F" restore-from-fixed-length-record-stream))
  "Alist of string found in RECORD-FORMAT field of header, and function of two
arguments: (magtape-input-stream file-output-stream) to handle restoring data
in that format")


(defun y-or-n-or-proceed (string &rest l)
  (apply #'fquery '(:CHOICES (((T "Yes.") #/y #/t #\SP #\HAND-UP)
			      ((() "No.") #/n #\RUBOUT #\HAND-DOWN)
			      ((:PROCEED "Proceed.") #/p #\RESUME)))
	 string l))
	  
(defun restore-filep (pathname &aux says)
  (cond ((and *skip-if-exists* (probe-file pathname))
	 (vformat 1 "~&Skipping ~A because it exists already~%" pathname)
	 nil)
	((not *query*)
	 (vformat 1 "~&Restoring ~A~%" pathname)
	 t)
	((setq says (y-or-n-or-proceed 	"~&~A : Restore ? " pathname))
	 (if (eq says :proceed) (setq *query* nil))
	 (vformat 1 "~&Ok, restoring ~A~%" pathname)
	 t)
	('else
	 (vformat 1 "~&Ok, skipping ~A~%" pathname))))

(defun restore-file-data ()
  (with-mt-stream (in)
    (let ((pathname (fs:parse-pathname (if (stringp *file-id-append*)
					   (string-append *file-id-append*
							  (getf *header* 'file-id))
					 (funcall *file-id-append* (getf *header* 'file-id))))))
      (cond ((restore-filep pathname)
	     (let ((handler (or (cadr (ass #'string-equal
					   (getf *header* 'record-format)
					   *record-format-handlers*))
				(ferror nil "Record format not handled: ~S"
					(getf *header* 'record-format)))))
	       (with-open-file (out pathname :out)
		 (funcall handler in out))))))))


;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:27
(defun string-translate (string-from start end string-to table)
  end
  (do ((j start (1+ j))
       (tab-dim (length table))
       (k 0 (1+ k))
       (c))
      ((= j end))
    (setq c (aref string-from j))
    (setf (aref string-to k)
	  (if (< c tab-dim)
	      (aref table c)
	    0))))

(defresource simple-string (length)
  :constructor (make-array length :element-type 'string-char))


(defun header-numeric-value (key &aux (string (getf *header* key)))
  (or (parse-number string)
      (ferror nil "Badly formed header ~A: ~S" key string)))

(defun block-length ()
  (header-numeric-value 'block-length))

(defun record-length ()
  (header-numeric-value 'record-length))

(defvar *ignore-bad-rl-field* t
  "If T assume non-numeric record length field is end-of-block")

;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:28
(defun next-variable-record-length (buffer start)
  (cond ((= start (length buffer))
	 (vformat 3 "END-OF-BLOCK "))
	('else
	 (vformat 3 "~A " (substring buffer start (min (+ start 4) (length buffer))))
	 (cond ((or (char= (aref buffer start) #\^) 
		    (char= (aref buffer start) #\@))
		;; #\^ is ansi, #\@ is what we saw on the kermit tape from CMU.
		nil)
	       ((parse-number buffer start (+ start 4)))
	       ;; but in fact we see other garbage too, perhaps due to device
	       ;; driver block cleaning problems. Either on reading or writing
	       ;; the tape.
	       (*ignore-bad-rl-field* nil)
	       ('else
		(cerror "go to next tape block"
			"Badly formed variable record-length field: ~S"
			(substring buffer start (+ start 4))))))))

;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:29
(defun restore-from-variable-length-record-stream (from to)
  (using-resource (buffer simple-string (block-length))
    (do ((tape-block)(b-start)(b-end))
	((null (multiple-value-setq (tape-block b-start b-end) (send from :read-input-buffer))))
      (string-translate tape-block b-start b-end buffer *ascii-code-array*)
      (do ((start 0)(record-length)(end))
	  ((null (setq record-length (next-variable-record-length buffer start))))
	(setq end (+ start record-length))
	(send to :string-out buffer (+ start 4) end)
	(send to :tyo #\return)
	(setq start end))
      (send from :advance-input-buffer))))

;; Copied from LAD: RELEASE-3.TAPE; ANSI.LISP#19 on 2-Oct-86 04:33:29
(defun restore-from-fixed-length-record-stream (from to)
  ;; this is untested but should be obvious enough to be correct.
  (let ((block-length (block-length))
	(record-length (record-length)))
    (using-resource (buffer simple-string block-length)
      (do ((tape-block)(b-start)(b-end))
	  ((null (multiple-value-setq (tape-block b-start b-end) (send from :read-input-buffer))))
	(string-translate tape-block b-start b-end buffer *ascii-code-array*)
	(do ((j 0 (1+ j))
	     (n (floor block-length record-length)))
	    ((= j n))
	  (send to :string-out buffer (* j record-length) (* (1+ j) record-length))
	  (send to :tyo #\return))))))

