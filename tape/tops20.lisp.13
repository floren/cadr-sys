;;; -*- Mode:LISP; Package:(TOPS20 GLOBAL); Base:10; Readtable:T -*-

#|

Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
licensing and release information.

Read a DUMPER format tape. This has been used for our transfers from MIT and UpMail.
Writing this was a whole lot easier (from experience mind you!) than dealing
with DUMPER (and directories and networks and protection etc) on an actual TOPS-20 system.
Exactly why this is so will be known to anybody who has had the chance. -GJC

Our reference for the dumper format was probably some assembly language source
code somebody had. Interesting properties:
 * there arent any tape marks until then end of the whole save set.
 * a 36 bit pdp-10 word is written as 5 8-bytes. (40 bits total)
 * ascii data is in native pdp-10 format, which is 5 7-bit bytes. (35 bits).


The code was first written dealing with everything as 36 bit words or
arrays of these words. Then the handling of logical records of ASCII
data was optimized so as to reduce bignum consing.

|#

(defvar *unhandled-file-types* '("QFASL" "BIN" "EXE")
  "For these types of files we dont handle the data properly to bother restoring")

(defvar *dummy-host*
	(make-instance (get :tops-20 'si:host-flavor)
		       :alist-elem (si:MAKE-HOST-ALIST-ELEM :NAME "TAPE")))

(defvar *query* t)

(defun y-or-n-or-proceed (string &rest l)
  (apply #'fquery '(:CHOICES (((T "Yes.") #/y #/t #\SP #\HAND-UP)
			      ((() "No.") #/n #\RUBOUT #\HAND-DOWN)
			      ((:PROCEED "Proceed.") #/p #\RESUME)))
	 string l))


(defvar *host* "LM")

(defun open-file-from-tops20-spec (x &aux p1 p2 stream ans)
  (setq p1 (fs:parse-pathname x *dummy-host*))
  (setq p2 (send p1 :new-pathname :host *host* :device nil))
  (format t "~&~A => ~A ..." p1 p2)
  (cond ((mem #'string-equal (send p2 :type) *unhandled-file-types*)
	 (format t " file type not handled, not restoring~%"))
	((and (setq ans (block probe-file
			  (condition-bind ((fs:directory-not-found
					     #'(lambda (object) (return-from probe-file object))))
			    (probe-file p2))))
	      (not (errorp ans)))
	 (format t " file exists~%"))
	((or (not *query*)
	     (setq ans (y-or-n-or-proceed " Restore? ")))
	 (and (eq ans :proceed) (setq *query* nil))
	 (fs:create-directory  p2)
	 (setq stream (open p2 :out))
	 (format t " opened: ~A~%" stream)
	 stream)))

(defvar *openf* 'open-file-from-tops20-spec)

(defvar *default-tape-block-size* (* 1024 5) "5 tops-20 pages is the default")

(defun restore-tape (&optional &key
		     (file-opener 'open-file-from-tops20-spec)
		     (query t)
		     (to-host "LM")
		     (tape-block-size *default-tape-block-size*))
  "Note: Tape-Block-size is in PDP-10 words, should usually a multiple of 1024"
  (let ((*openf* file-opener)
	(*query* query)
	(*host* to-host)
	(tape-stream))
    (fs:mt-rewind)
    (restore-tape-1 (setq tape-stream (fs:MAKE-MT-STREAM :DIRECTION :INPUT
							 :RECORD-SIZE (* tape-block-size 5)
							 :CHARACTERS NIL)))
    ;; we dont want an unwind-protect here because closing spaces out to the
    ;; next tape mark, which is usually a lot of tape and not what we want to
    ;; do.
    (and tape-stream (close tape-stream t))))


(defvar *logical-record-size* 518 "in pdp 10 words")
(defvar *logical-record-header-size* 6 "in pdp 10 words")

(defvar *optimize-data-record-handling* t)
      
(defun restore-tape-1 (is &aux file-stream)
  (UNWIND-PROTECT 
      (let ((ary (make-array *logical-record-size*))
	    file-name last-record-type record-type)
	(setq file-stream t)
	(do ()(nil)
	  (read-logical-record-header is ary)
	  (selectq (setq record-type (logical-record-type ary))
	    (data
	     (lmark "file data")
	     (cond ((not file-stream)
		    (throw-away-logical-record-rest is))
		   (*optimize-data-record-handling*
		    (read-map-logical-record-rest
		      is
		      #'(lambda (n)
			  (or (zerop n)
			      (write-ascii-data-word n file-stream)))))
		   ('else
		    (read-logical-record-rest is ary)
		    (dotimes (c (- *logical-record-header-size* *logical-record-header-size*))
		      (let ((n (aref ary (+ c *logical-record-header-size*))))
			;; possibly bogus, but no ascii source files will have
			;; words of nulls in them anyway.
			(or (zerop n) (write-ascii-data-word n file-stream)))))))
	    (tphd
	     (lmark "save-set-header")
	     (throw-away-logical-record-rest is))
	    (flhd
	     (lmark "file header")
	     (read-logical-record-rest is ary)
	     (setq file-name
		   (with-output-to-string (s)
		     (dotimes (c 30)		; no filenames longer than 150 chars i bet.
		       (write-ascii-data-word (aref ary (+ c *logical-record-header-size*)) s))))
	     (setq file-name (substring file-name
					0
					(string-search #/; file-name)))
	     (setq file-stream (funcall *openf* file-name)))
	    (fltr
	     (lmark "file trailer")
	     (throw-away-logical-record-rest is)
	     (and file-stream (close file-stream))
	     (setq file-stream t))
	    (tptr
	     (lmark "tape trailer")
	     (return "done"))
	    (usr
	     (lmark "user directory information")
	     (throw-away-logical-record-rest is))
	    (ctph
	     (lmark "continued saveset")
	     (ferror nil "continued savesets not supported"))
	    (fill
	     (lmark "fill record")
	     (throw-away-logical-record-rest is))
	    (otherwise
	     (cerror "throw away record and continue reading tape"
		     "unknown logical record type ~s" record-type)
	     (throw-away-logical-record-rest is)))
	  (setq last-record-type record-type)))
    (and file-stream
	 (not (eq file-stream t))
	 (close file-stream))))

(defvar *debug* nil "Gives action on each logical record seen, :BREAK, :ERROR, or :VERBOSE")

(defprop lmark t :error-reporter)

(defun lmark (message)
  (selectq *debug*
    (:break (break (string message)))
    (:error (cerror "resume" "~A" message))
    (:verbose (print message))))

(defun read-logical-record (is ary)
  (dotimes (c *logical-record-size*)
    (aset (read-word is) ary c)))

(defun read-logical-record-header (is ary)
  (dotimes (c *logical-record-header-size*)
    (aset (read-word is) ary c)))

(defun read-logical-record-rest (is ary)
  (dotimes (c (- *logical-record-size* *logical-record-header-size*))
    (aset (read-word is) ary (+ c *logical-record-header-size*))))

(defun throw-away-logical-record-rest (is)
  (dotimes (c (- *logical-record-size* *logical-record-header-size*))
    (throw-away-word is)))

(defun read-map-logical-record-rest (is f)
  (dotimes (c (- *logical-record-size* *logical-record-header-size*))
    (funcall f (read-word is))))

(DEFUN throw-away-WORD (IS)
  (SEND IS :TYI)
  (SEND IS :TYI)
  (SEND IS :TYI)
  (SEND IS :TYI)
  (SEND IS :TYI))

(DEFUN READ-WORD (IS &aux C1 C2 C3 C4 C5)
  (SETQ C1 (SEND IS :TYI))
  (setq C2 (SEND IS :TYI))
  (setq C3 (SEND IS :TYI))
  (setq C4 (SEND IS :TYI))
  (setq C5 (SEND IS :TYI))
  (if (and C1 C2 C3 C4 C5)
      (DPB C1 #o3410 (DPB C2 #o2410 (DPB C3 #o1410 (DPB C4 #o0410 C5))))))
    
(defun logical-record-type (ary)
  (let* ((code (if (zerop (aref ary 4))
		  0
		(minus (logior #o-1000 (aref ary 4)))))
	 (ans (nth code '(DATA TPHD FLHD FLTR TPTR USR CTPH FILL))))
    (if (null ans)
	(ferror nil "unknown error type ~s" code))
    ans))
	
(defun write-ascii-data-word (wd stream)
  (dolist (byte '(#o3507 #o2607 #o1707 #o1007 #o0107))
    (let ((c (ldb byte wd)))
      ;; proper check is this: * eat first #\line after #\cr,
      ;; punt after seeing first #\control-c in last word in file.
      ;; but this ignoring certain characters works fine for our source files.
      (cond ((memq c '(#o0 #o12)))
	    ((memq c '(#o10 #o11 #o14 #o15))
	     (send stream :tyo (+ c #o200)))
	    ('else
	     (send stream :tyo c))))))

;; to do:
;; WRITE-QFASL-DATA-WORD i think same as 16B data.
;; WRITE-32B-DATA-WORD, good for TeX font files maybe, EXE files?
;; WRITE-16B-DATA-WORD, good for Alto stuff, fonts I guess.

