;;; -*- Mode:LISP; Package:(TAPE); Base:10; Readtable:CL -*-

;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;
;;;
;;; Format to implement TANALYZ (raw dump) format
;;;
;;;

(in-package 'tape)

(eval-when (eval compile load)
  (defconst *tanalyz-max-kblocks* 32. "Maximum # 1K blocks handled")
  (defconst *tanalyz-max-block-size* (* *tanalyz-max-kblocks* 1024.) "Maximum block size handled")
  )

(defflavor tanalyz-format
	 ((kblocks *tanalyz-max-kblocks*)	;max buffer block size
	  (record-size *tanalyz-max-block-size*)	;expected record size
	  (block-length)			;actual block size found
	  (current-block nil)			;block-length block found
	  (record-length)			;(min record-size block-length)
	  (fileno 0)				;sequential file # encountered
	  (file-stream nil)			;required, not used
	  (output-mode 'verbose)
	  (current-plist nil))
	 (basic-tape-format)
  (:gettable-instance-variables)
  (:settable-instance-variables current-plist))

(defmethod (tanalyz-format :initialize) (&rest init-options)
  (check-attribute-list init-options)
  (unless kblocks
    (setq kblocks *tanalyz-max-kblocks*))
  (unless record-size
    (setq record-size (* kblocks 1024)))
  (unless output-mode
    (setq output-mode 'verbose))
  (unless fileno
    (setq fileno 0))
  (setq current-plist (list 'tanalyz-format))
  (when init-options
    (lexpr-send self :set-options init-options)))

(defmethod (tanalyz-format :set-options) (&rest options)
  (check-attribute-list options)
  (if options
      (do* ((l options (cddr l))
	    (option (car l) (car l))
	    (value (cadr l) (cadr l)))
	   ((null l))
	(case option
	  (:record-size
	   (check-arg value (zerop (remainder value 1024))
		      "a multiple of 1024 bytes")
	   (setq record-size value)
	   (setq kblocks (ceiling (/ record-size 1024))))
	  (:kblocks
	   (check-type value (integer 1 #.*tanalyz-max-kblocks*))
	   (setq kblocks value)
	   (setq record-size (* kblocks 1024)))
	  (t (signal 'invalid-option :object self :option option :value value))))
    (progn
      (tv:choose-variable-values
	`((,(locf kblocks) "Max Blocking Factor (1K byte chunks)" :number)
	  (,(locf output-mode) "Output Mode" :choose (brief verbose full)))
	:label '(:string "Options for the TANALYZ tape format" :font fonts:tr12b))
      (check-type kblocks (integer 1 #.*tanalyz-max-kblocks*))
      (setq record-size (* kblocks 1024))))
  (send self :set-current-plist))

(defmethod (tanalyz-format :open-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :open-file))

(defmethod (tanalyz-format :set-current-plist) ()
  (setf (get current-plist :fileno) fileno)
  (setf (get current-plist :record-size) record-size)
  (setf (get current-plist :characters) t)
  (setf (get current-plist :byte-size) 8)
  current-plist)

(defmethod (tanalyz-format :read-tape-header) (&rest ignore)
  (send self :rewind))

(defmethod (tanalyz-format :write-tape-header) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-tape-header))

(defmethod (tanalyz-format :tape-is-your-format-p) (&rest ignore))

(defmethod (tanalyz-format :restore-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :restore-file))

(defmethod (tanalyz-format :write-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-file))

(defmethod (tanalyz-format :write-partition) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-partition))

(defmethod (tanalyz-format :compare-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :compare-file))

(defmethod (tanalyz-format :beginning-of-file) (device)
  (check-device device)
  (condition-case ()
      (send device :search-filemark-reverse 1 :high)
    (physical-beginning-of-tape
     (setq fileno 0)
     (send self :set-current-plist))))

(defmethod (tanalyz-format :read-file-header) (device &key (reposition t))
  (check-device device)
  (using-resource (header-block si:dma-buffer 1)
    (condition-case ()
	(send device :read-block header-block 1024)
      ((filemark-encountered physical-end-of-tape)
       (signal 'logical-end-of-tape :device-object device))
      (:no-error
       (incf fileno)
       (if reposition (send device :space-reverse 1))
       (send self :set-current-plist)))))

(defmethod (tanalyz-format :next-file) (device &optional (nfiles 1))
  (check-device device)
  (check-type nfiles (integer 1))
  (dotimes (c nfiles) 
    (send device :search-filemark 1 :high)
    (send self :read-file-header device :reposition nil)
    (incf fileno))
  (send device :space-reverse 1)
  (send self :set-current-plist))

(defmethod (tanalyz-format :read-block) (device)
  "Grab the next tape block. Returns substring from block.
DOES NOT trap errors - DIY!"
  (using-resource (block si:dma-buffer kblocks)
    (setq block-length
	  (send device :read-block block record-size))
    (setq record-length (min record-size block-length))
    (setq current-block
	  (si:dma-buffer-string block))
    (substring current-block 0 record-length)))

(defmethod (tanalyz-format :report-file) (device stream &aux mode)
  (setq mode (if stream output-mode 'silent))
  (case mode
    (silent)
    (brief (format stream "~&File #~3,'0d " fileno))
    (t     (format stream "~2%+++ File #~3,'0d +++" fileno)))
  (let((recno 0))
    (condition-case()
	(do*((gotit (send self :read-block device)
		    (send self :read-block device)))
	    ;;do-forever
	    (nil)
	  (incf recno)
	  (case mode
	    (silent
	     (send device :search-filemark 1 :high)
	     (send device :space-reverse 1 :high))
	    (brief
	     (format stream "'~a'" (substring current-block 0
					      (min 64 block-length)))
	     (send device :search-filemark 1 :high)
	     (send device :space-reverse 1 :high))
	    (verbose
	     (format stream "~2&    Rec.  #~d ~d bytes:~2%"
		     recno block-length)
	     (dumpit:dumpit (substring current-block 0
				     (min block-length 100))))
	    (full
	     (format stream "~2&   Rec. #~d ~d bytes:~2%"
		     recno block-length)
	     (dumpit:dumpit (substring current-block 0 block-length))
	     (format stream "~%"))))
      (filemark-encountered
       (case mode
	 ((or silent brief))
	 (t (format stream "~%--- ~d records in file #~d" recno fileno)))))))

(defmethod (tanalyz-format :list-files)
	   (device &key (stream *standard-output*) (number-of-files -1))
  (check-device device)
  (check-type number-of-files (integer))
  (let ((plists)
	(filesread 0))
    (condition-case ()
	(do* ((header (send self :read-file-header device)
		      (send self :read-file-header device)))
	     ;;do-forever
	     (nil)
	  (incf filesread)
	  (send self :report-file device stream)
	  (push current-plist plists)
	  (if (= filesread number-of-files)
	      (return)))
      (logical-end-of-tape
       (and stream
	    (= number-of-files -1)
	    (> filesread 0)
	    (format stream "~2% [End of Tape]~2&"))
       (condition-Case ()
	   (send device :space-reverse 1)
	 (driver-error))))
    (reverse plists)))

(defmethod (tanalyz-format :previous-file) (device &optional (nfiles 1))
  (check-device device)
  (check-type nfiles (integer 1))
  (send self :beginning-of-file device)
  (condition-case()
      (dotimes (times nfiles)
	(send device :space-reverse 1)
	(send device :search-filemark-reverse 1 :high)
	(decf fileno))
    (physical-beginning-of-tape
     (setq fileno 0)))
  (send self :set-current-plist))

(defmethod (tanalyz-format :previous-file) (device &optional (nfiles 1))
  (check-device device)
  (check-type nfiles (integer 1))
  (condition-case()
      (progn
       (send self :beginning-of-file device)
       (dotimes (times nfiles)
	 (send device :space-reverse 1)
	 (send device :search-filemark-reverse 1 :high)
	 (decf fileno)))
    (physical-beginning-of-tape
     (setq fileno 0)))
  (send self :set-current-plist))

(defmethod (tanalyz-format :find-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :find-file))

(defmethod (tanalyz-format :find-file-reverse) (&rest ignore)
  (signal 'not-supported :device-object self :operation :find-file-reverse))

(defmethod (tanalyz-format :finish-tape) (&rest ignore)
  (signal 'not-supported :device-object self :operation :finish-tape))

(defmethod (tanalyz-format :rewind) (device &optional (wait-p t))
  (send device :rewind wait-p)
  (setq fileno 0)
  (send self :set-current-plist)
  fileno)

(defmethod (tanalyz-format :unload) (device)
  (setq fileno 0)
  (send device :unload))

(defmethod (tanalyz-format :position-to-append) (&rest ignore)
  (signal 'not-supported :device-object self :operation :position-to-append))

(define-tape-format tanalyz-format "TANALYZ")

;;; No TANALYZ stream needed or supported...

