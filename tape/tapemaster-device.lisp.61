;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Device interface for Tapemaster Multibus Tape Controller.
;;;
;;; -dg 8/15/85
;;;

(defconst *tapemaster-device-lock* nil)

(defflavor tapemaster-device ((unit 0)
			      (density :ignore)
			      (skip-bad-blocks t)
			      (use-streaming-mode t))
	   (basic-tape-device)
  :gettable-instance-variables)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setting parameters for tape operation
;;;

(defmethod (tapemaster-device :set-options) (&rest options)
  (check-attribute-list options)
  (if options
      (do* ((l options (cddr l))
	    (option (car l) (car l))
	    (value (cadr l) (cadr l)))
	   ((null l) t)
	(case option
	  (:unit
	   (check-type value (integer 0 8)
		       "in the range of units [0,7] supported by the tapemaster")
	   (setq unit value))
	  (:density)
	  (:skip-bad-blocks
	   (check-type value (member t nil)
		       (format nil "a valid boolean value for the :SKIP-BAD-BLOCKS option to ~A" self))
	   (setq skip-bad-blocks value))
	  (:use-streaming-mode
	   (check-type value (member t nil)
		       (format nil "a valid boolean value for the :USER-STREAMING-MODE option to ~A" self))
	   (setq use-streaming-mode value))
	  (t
	   (signal 'invalid-option :object self :option option :value value))))
    (tv:choose-variable-values
      `((,(locf unit) "Unit" :number)
	(,(locf skip-bad-blocks) "Skip bad blocks on tape" :boolean)
	(,(locf use-streaming-mode) "Use streaming mode when possible" :boolean))
      :label '(:string "Choose Tapemaster-Device options" :font fonts:tr12b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control
;;;

(defmethod (tapemaster-device :initialize) (&rest init-options)
  (when init-options
    (lexpr-send self :set-options init-options)))

(defmethod (tapemaster-device :deinitialize) ()
  (send self :unlock-device))

(defmethod (tapemaster-device :lock-device) (&aux owner)
  (case (setq owner (tm:determine-tapemaster-owner))
    (:not-on-bus (ferror 'device-not-found
			 :format-string "The tapemaster device is not present on the bus."))
    (:no-owner (tm:steal-tapemaster-from-bus))
    (:this-processor
     (unless (or (%store-conditional (locf *tapemaster-device-lock*)
				     nil
				     (cons si:current-process self))
		 (and (eq (car *tapemaster-device-lock*) si:current-process)
		      (eq (cdr *tapemaster-device-lock*) self)))
       (signal-proceed-case (() 'device-unavailable
				:device-type 'tapemaster
				:current-owner *tapemaster-device-lock*
				:proceed-types `(:wait-for-device-free
						  :steal-device-internally))
	 (:wait-for-device-free
	  (process-wait "Wait for free device"
			#'(lambda (process object)
			    (%store-conditional (locf *tapemaster-device-lock*)
						nil
						(cons process object)))
			si:current-process self))
	 (:steal-device-internally
	  (setq *tapemaster-device-lock* (cons si:current-process self))))))
    (t (signal-proceed-case (() 'device-unavailable
				:device-type 'tapemaster
				:current-owner owner
				:proceed-types '(:steal-device-from-other-processor))
	 (:steal-device-from-other-processor
	  (tm:steal-tapemaster-from-bus)
	  (setq *tapemaster-device-lock* (cons si:current-process self)))))))

(defmethod (tapemaster-device :unlock-device ) ()
  (when (and (eq (car *tapemaster-device-lock*) si:current-process)
	     (eq (cdr *tapemaster-device-lock*) self))
    (setq *tapemaster-device-lock* nil)))

(defmethod (tapemaster-device :device-locked-p) ()
  (and (eq (car *tapemaster-device-lock*) si:current-process)
       (eq (cdr *tapemaster-device-lock*) self)))

(defmethod (tapemaster-device :reset) ()
  (tm:setup-control-memory)
  (tm:init))

(defmethod (tapemaster-device :status) ()	;This should be much more hairy
  (tm:controller-status unit))

(defmethod (tapemaster-device :speed-threshold) (record-size)
  record-size
  (* 32 1024))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tape positioning
;;;

(defmethod (tapemaster-device :rewind) (&optional (wait-p t))
  (with-device-locked self
    (tm:rewind unit wait-p)))

(defmethod (tapemaster-device :unload) ()
  (with-device-locked self
    (tm:unload unit (* 60 5))))

(defmethod (tapemaster-device :space) (number-of-records &optional (speed :low))
  (check-type number-of-records (integer 1))
  (check-type speed (member :high :low))
  (with-device-locked self
    (tm:space unit 0 number-of-records (if (eq speed :low) 0 1))))

(defmethod (tapemaster-device :space-reverse) (number-of-records &optional (speed :low))
  (check-type number-of-records (integer 0))
  (check-type speed (member :high :low))
  (with-device-locked self
    (tm:space unit 1 number-of-records (if (eq speed :low) 0 1))))

(defmethod (tapemaster-device :search-filemark) (number-of-filemarks &optional (speed :low))
  (check-type number-of-filemarks (integer 1))
  (check-arg speed (memq speed '(:high :low)) "a valid speed arg (:HIGH or :LOW)")
  (with-device-locked self
    (let ((speed-code (case speed (:high 1) (:low 0))))
      (if (> number-of-filemarks 1)
	  (tm:search-multiple-filemarks number-of-filemarks unit 0 speed-code)
	(tm:search-filemark unit 0 speed-code)))))

(defmethod (tapemaster-device :search-filemark-reverse) (number-of-filemarks &optional (speed :low))
  (check-type number-of-filemarks (integer 1))
  (check-arg speed (memq speed '(:high :low)) "a valid speed arg (:HIGH or :LOW)")
  (with-device-locked self
    (let ((speed-code (case speed (:high 1) (:low 0))))
      (if (> number-of-filemarks 1)
	  (tm:search-multiple-filemarks number-of-filemarks unit 1 speed-code)
	(tm:search-filemark unit 1 speed-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read/Write
;;;

(defmethod (tapemaster-device :optimal-chunk-size) (record-size)
  (check-type record-size (integer 1))
  (* (floor (* (tm:number-of-data-pages) si:page-size 4)
	    (+ record-size 8))
     record-size))

(defmethod (tapemaster-device :read-block) (dma-buffer record-size)
  (check-dma-buffer dma-buffer)
  (check-arg record-size (<= record-size (* (array-length dma-buffer) 4))
	     "less than or equal to the size of the dma-buffer")
  (with-device-locked self
    (tm:direct-read dma-buffer record-size unit 0)))

(defmethod (tapemaster-device :write-block) (dma-buffer record-size)
  (check-arg dma-buffer (eq (named-structure-p dma-buffer) 'si:dma-buffer)
	     "a DMA-BUFFER object")
  (check-arg record-size (<= record-size (* (array-length dma-buffer) 4))
	     "less than or equal to the size of the dma-buffer")
  (let ((tm:*skip-bad-blocks* skip-bad-blocks))
    (with-device-locked self
      (tm:direct-write dma-buffer record-size unit 0))))

(defmethod (tapemaster-device :read-array) (array number-of-records record-size)
  (check-array array number-of-records record-size)
  (check-type number-of-records (integer 0))
  (check-arg record-size (and (typep record-size '(integer 1 65536))
			      (zerop (remainder record-size 1024)))
	     "a record-size that is a multiple of 1024 and between 1024 and 65536")
  (with-device-locked self
    (if use-streaming-mode
	(tm:streaming-read-array
	  array record-size number-of-records unit
	  (if (> (floor (* number-of-records record-size) 1024) 32)
	      1
	    0))
      (using-resource (block si:dma-buffer (ceiling record-size 1024))
	(with-buffer-wired (block (si:dma-buffer-size-in-pages block))
	  (let ((block-array (ecase (array-type array)
			       ((art-8b art-string) (si:dma-buffer-8b block))
			       (art-16b (si:dma-buffer-16b block))))
		(adjusted-record-size (ecase (array-type array)
					((art-8b art-string) record-size)
					(art-16b (/ record-size 2)))))
	    (do* ((count 0 (add1 count))
		  (copy-start 0 (* count adjusted-record-size)))
		 ((= count number-of-records) count)
	      (condition-case (condition)
		  (tm:direct-read block record-size unit 0)
		((filemark-encountered physical-end-of-tape)
		 (signal (type-of condition)
			 :device-type 'tapemaster
			 :unit unit
			 :data-transferred count)))
	      (copy-array-portion block-array 0 adjusted-record-size
				  array copy-start (+ copy-start adjusted-record-size)))))))))
	      

(defmethod (tapemaster-device :write-array) (array number-of-records record-size)
  (check-array array number-of-records record-size)
  (check-type number-of-records (integer 0))
  (check-arg record-size (and (typep record-size '(integer 1 65536))
			      (zerop (remainder record-size 1024)))
	     "a record-size that is a multiple of 1024 and between 1024 and 65536")
  (let ((tm:*skip-bad-blocks* skip-bad-blocks))
    (with-device-locked self
      (if use-streaming-mode
	  (tm:streaming-write-array
	    array record-size number-of-records unit
	    (if (> (floor (* number-of-records record-size) 1024) 32)
		1
	      0))
	(using-resource (block si:dma-buffer (ceiling record-size 1024))
	  (with-buffer-wired (block (si:dma-buffer-size-in-pages block))
	    (let ((block-array (ecase (array-type array)
				 ((art-8b art-string) (si:dma-buffer-8b block))
				 (art-16b (si:dma-buffer-16b block))))
		  (adjusted-record-size (ecase (array-type array)
					  ((art-8b art-string) record-size)
					  (art-16b (/ record-size 2)))))
	      (do* ((count 0 (add1 count))
		    (copy-start 0 (* count adjusted-record-size)))
		   ((= count number-of-records) count)
		(copy-array-portion array copy-start (+ copy-start adjusted-record-size)
				    block-array 0 adjusted-record-size)
		(condition-case (condition)
		    (tm:direct-write block record-size unit 0)
		  ((filemark-encountered physical-end-of-tape)
		   (signal (type-of condition)
			   :device-type 'tapemaster
			   :unit unit
			   :data-transferred count)))))))))))

(defmethod (tapemaster-device :read-to-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (with-device-locked self
    (tm:streaming-read-to-disk disk-unit
      starting-block number-of-blocks record-size unit
      (if (> (floor (* number-of-blocks record-size) 1024) 32) 1 0)
      silent)))

(defmethod (tapemaster-device :write-from-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (let ((tm:*skip-bad-blocks* skip-bad-blocks))
    (with-device-locked self
      (tm:streaming-write-from-disk
	disk-unit
	starting-block number-of-blocks record-size unit
	(if (> (floor (* number-of-blocks record-size) 1024) 32) 1 0)
	silent))))

(defmethod (tapemaster-device :compare-to-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (with-device-locked self
    (tm:streaming-compare-to-disk
      disk-unit
      starting-block
      number-of-blocks
      record-size
      unit
      (if (> (floor (* number-of-blocks record-size) 1024) 32) 1 0)
      silent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other things
;;;

(defmethod (tapemaster-device :write-filemark) (&optional (number-of-filemarks 1))
  (check-type number-of-filemarks (integer 1))
  (with-device-locked self
    (dotimes (c number-of-filemarks)
      (tm:write-filemark unit))))

(define-tape-device tapemaster-device "tr" tm:tapemaster-on-bus?)

