;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;
;;;
;;; Format to implement a raw tape stream
;;;
;;;

(defflavor raw-format ((record-size *lmfl-default-record-size*)
		       (file-stream))
	   (basic-tape-format)
  (:gettable-instance-variables))

(defmethod (raw-format :initialize) (&rest init-options)
  (check-attribute-list init-options)
  (unless record-size
    (setq record-size *lmfl-default-record-size*))
  (when init-options
    (lexpr-send self :set-options init-options)))

(defmethod (raw-format :set-options) (&rest options)
  (check-attribute-list options)
  (if options
      (do* ((l options (cddr l))
	    (option (car l) (car l))
	    (value (cadr l) (cadr l)))
	   ((null l))
	(case option
	  (:record-size
	   (check-type value (integer 1024 #.(* 20 1024)))
	   (check-arg value (zerop (remainder value 1024))
		      "a multiple of 1024 bytes")
	   (setq record-size value))
	  (t (signal 'invalid-option :object self :option option :value value))))
    (tv:choose-variable-values
      `((,(locf record-size) "Record Size" :number))
      :label '(:string "Options for the RAW tape format" :font fonts:tr12b))))

(defmethod (raw-format :open-file) (device &key
				    (direction :input)
				    (byte-size :default)
				    (characters :default)
				    plist)
  (check-device device)
  (check-type direction (member :input :output))
  (check-type byte-size (member 8 16 :default))
  (check-type characters (member :default t nil))
  (when file-stream
    (case (send file-stream :status)
      ((:bof :closed))
      (t (close file-stream :abort t))))
  (when plist
    (signal 'protocol-violation
	    :format-string "This is a RAW-FORMAT. It cannot take a plist!"))
  (let* ((*characters (if (eq characters :default)
			  t
			characters))
	 (*byte-size (if (eq byte-size :default)
			 8
		       byte-size)))
    (send device :lock-device)
    (setq file-stream
	  (make-instance (case direction
			   (:input (if (not *characters)
				       'raw-input-stream
				     'raw-input-character-stream))
			   (:output (if (not *characters)
					'raw-output-stream
				      'raw-output-character-stream)))
			 :device device
			 :byte-size *byte-size
			 :record-size record-size
			 :format self))))


(defmethod (raw-format :read-tape-header) (&rest ignore)
  (signal 'not-supported :device-object self :operation :read-tape-header))

(defmethod (raw-format :write-tape-header) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-tape-header))

(defmethod (raw-format :tape-is-your-format-p) (&rest ignore))

(defmethod (raw-format :restore-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :restore-file))

(defmethod (raw-format :write-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-file))

(defmethod (raw-format :write-partition) (&rest ignore)
  (signal 'not-supported :device-object self :operation :write-partition))

(defmethod (raw-format :compare-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :compare-file))

(defmethod (raw-format :beginning-of-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :beginning-of-file))

(defmethod (raw-format :next-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :next-file))

(defmethod (raw-format :previous-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :previous-file))

(defmethod (raw-format :find-file) (&rest ignore)
  (signal 'not-supported :device-object self :operation :find-file))

(defmethod (raw-format :find-file-reverse) (&rest ignore)
  (signal 'not-supported :device-object self :operation :find-file-reverse))

(defmethod (raw-format :list-files) (&rest ignore)
  (signal 'not-supported :device-object self :operation :list-files))

(defmethod (raw-format :finish-tape) (&rest ignore)
  (signal 'not-supported :device-object self :operation :finish-tape))

(defmethod (raw-format :rewind) (device &optional (wait-p t))
  (when file-stream
    (close file-stream :abort t)
    (setq file-stream nil))
  (send device :rewind wait-p))

(defmethod (raw-format :unload) (device)
  (send device :unload)
  (when file-stream
    (close file-stream :abort t)
    (setq file-stream nil)))

(defmethod (raw-format :position-to-append) (&rest ignore)
  (signal 'not-supported :device-object self :operation :position-to-append))

(define-tape-format raw-format "RAW")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Raw stream flavor defs
;;;

(defflavor raw-input-mixin () (tape-stream-mixin))

(defmethod (raw-input-mixin :close) (&optional abort-p)
  (unless (eq status :closed)
    (setq status :closed)
    (when dma-buffer
      (deallocate-resource 'si:dma-buffer dma-buffer))
    (setq dma-buffer nil
	  io-buffer nil)
    (unless abort-p
      (send device :search-filemark 1))
    (send device :unlock-device)))

(defflavor raw-input-character-stream ()
	   (raw-input-mixin si:buffered-input-character-stream))

(defflavor raw-input-stream ()
	   (raw-input-mixin si:buffered-input-stream))

(compile-flavor-methods raw-input-character-stream raw-input-stream)

;;;;;;;;;;;;;;;;;;;;

(defflavor raw-output-mixin () (tape-stream-mixin))

(defmethod (raw-output-mixin :close) (&optional abort-p)
  (unless (eq status :closed)
    (setq status :closed)
    (when dma-buffer
      (deallocate-resource 'si:dma-buffer dma-buffer))
    (setq dma-buffer nil
	  io-buffer nil)
    (unless abort-p
      (send self :force-output)
      (send device :write-filemark))
    (send device :unlock-device)))


(defflavor raw-output-stream ()
	   (raw-output-mixin si:buffered-output-stream))

(defflavor raw-output-character-stream ()
	   (raw-output-mixin si:buffered-output-character-stream))

(compile-flavor-methods raw-output-stream raw-output-character-stream)
