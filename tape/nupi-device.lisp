;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;;
;;; Nupi tape device for explorer.
;;;
;;; - wbg of Chaparral Dallas, 6/86
;;; - dg 11/85
;;;

(DefFlavor NuPI-Device
	 ((unit 6)
	  (density :ignore)
	  (command-block (si:get-nupi-command-block nil)))
	 (Basic-Tape-Device)
  :gettable-instance-variables)


(DefMethod (nupi-device :print-self) (stream ignore slashify)
  (If slashify
      (si:printing-random-object (self stream)
	(format stream "NuPI Unit: ~d" unit))
    (format stream "NuPI Unit ~d" unit))
  )

(DefMethod (nupi-device :set-options) (&rest ignore))

(DefMethod (nupi-device :deinitialize) ()
  )

(DefMethod (nupi-device :lock-device) (&rest ignore))
(DefMethod (nupi-device :unlock-device) (&rest ignore))
(DefMethod (nupi-device :device-locked-p) (&rest ignore) t)

(DefMethod (nupi-device :reset) (&rest ignore)
  (send self :initialize))

(defmethod (nupi-device :status) (&rest ignore))

(defmethod (nupi-device :optimal-chunk-size) (record-size)
  record-size
  )


(defmethod (nupi-device :rewind) (&optional (wait-p t))
  (si:start-nupi-simple-command
    command-block
    (si:nupi-logical-to-physical-unit unit)
    #x20 0 0 nil nil nil)
  (When wait-p
    (si:wait-for-nupi-command command-block "Tape Rewind")
    (si:finish-nupi-command command-block nil))
  )

(defmethod (nupi-device :unload) ()
  (si:simple-nupi-command
    command-block #x21 unit 0 0 nil nil nil "Tape Unload")
  )

(defmethod (nupi-device :speed-threshold) (record-size)
  (declare (ignore record-size))
  -1)


(defmethod (nupi-device :space) (number-of-records &optional (speed :low))
  (check-type number-of-records (integer 1))
  (check-type speed (member :high :low))
  ;; Convert from LMFL (4096 byte) blocks to NuPI (1024 byte) blocks.
  (Let ((number-of-blocks (/ (* number-of-records 4096.) 1024.)))
    (si:simple-nupi-command
      command-block #x23 unit 0 4 number-of-blocks 0 nil "Tape Space"))
  )



(defmethod (nupi-device :space-reverse) (number-of-records &optional (speed :low))
  (check-type number-of-records (integer 1))
  (check-type speed (member :high :low))
  (signal 'tape:driver-error
	  :device-type 'nupi-device
	  :error-code #x23
	  :error-message "Space Reverse operation not available.")
  )


(defmethod (nupi-device :search-filemark) (number-of-filemarks &optional (speed :low))
  (declare (ignore speed))
  (si:simple-nupi-command
    command-block #x27 unit 0 4 number-of-filemarks 0 nil "Tape Search FM")
  )


(DefMethod (nupi-device :Search-Filemark-Reverse) (number-of-filemarks &optional (speed :low))
  (declare (ignore number-of-filemarks speed))
  ;; +++ this command cannot be supported by this device +++
  (signal 'tape:driver-error
     :device-type 'nupi-device
     :error-code  #x27
     :error-message "Search Reverse Filemark operation not available.")
  )


(defmethod (nupi-device :read-block) (dma-buffer record-size)
  (setq record-size 4096)
  (si:nupi-read-from-tape command-block unit 0 record-size dma-buffer 0))


(defmethod (nupi-device :write-block) (dma-buffer record-size)
  (si:nupi-write-to-tape command-block unit 0 record-size dma-buffer 0))


(defmethod (nupi-device :read-array) (array number-of-records record-size)
  (check-arg array (memq (array-type array) `(art-8b art-string art-16b art-32b))
			 "an art-8, art-string, art-16b or art-32b array")
  (let* ((nbytes (* record-size number-of-records))
	 (npages (ceiling nbytes (* si:page-size 4))))
    (Condition-Case (error)
	(using-resource (buffer si:dma-buffer npages)
	  (with-buffer-wired (buffer npages)
	    (si:nupi-read-from-tape
	      command-block unit 0 nbytes buffer 0)
	    (case (array-type array)
	      ((art-8b art-string)
	       (si:copy-array-portion
		 (si:dma-buffer-string buffer) 0 nbytes
		 array 0 nbytes))
	      (art-16b
	       (check-arg record-size (zerop (remainder nbytes 2))
			  "a half-word even record-size for an art-16b array")
	       (si:copy-array-portion
		 (si:dma-buffer-16b buffer) 0 (/ nbytes 2)
		 array 0 (/ nbytes 2)))
	      (art-32b
	       (if (eq (named-structure-p array) 'si:dma-buffer)
		   (copy-array-portion
		     (si:dma-buffer-string buffer) 0 nbytes
		     (si:dma-buffer-string array) 0 nbytes)
		 (ferror nil "Error... cannot copy art-32b array."))))))
      (tape:physical-end-of-tape
       ;; Handle this error and generate a new with the right data transfer value.
       (signal 'tape:physical-end-of-tape
	  :device-type 'nupi-device
	  :unit unit
	  :data-transferred (truncate (Send error :data-transferred) record-size)))))
  )


(defmethod (nupi-device :write-array) (array number-of-records record-size)
  (check-arg array (memq (array-type array) `(art-8b art-string art-16b art-32b))
	     "an art-8, art-string, art-16b or art-32b array")
  (let* ((nbytes (* record-size number-of-records))
	 (npages (ceiling nbytes (* si:page-size 4))))
    (Condition-Case (error)
	(using-resource (buffer si:dma-buffer npages)
	  (case (array-type array)
	    ((art-8b art-string)
	     (si:copy-array-portion
	       array 0 nbytes
	       (si:dma-buffer-string buffer) 0 nbytes))
	    (art-16b
	     (check-arg record-size (zerop (remainder nbytes 2))
			"a half-word even record-size for an art-16b array")
	     (si:copy-array-portion
	       array 0 (/ nbytes 2)
	       (si:dma-buffer-16b buffer) 0 (/ nbytes 2)))
	    (art-32b
	     (if (eq (named-structure-p array) 'si:dma-buffer)
		 (copy-array-portion
		   (si:dma-buffer-string array) 0 nbytes
		   (si:dma-buffer-string buffer) 0 nbytes)
	       (ferror nil "Error... cannot copy art-32b array."))))
	  (with-buffer-wired (buffer npages)
	    (si:nupi-write-to-tape
	      command-block unit 0 nbytes buffer 0)))
      (tape:physical-end-of-tape
       ;; Handle this error and generate a new with the right data transfer value.
       (signal 'tape:physical-end-of-tape
	  :device-type 'nupi-device
	  :unit unit
	  :data-transferred (truncate (Send error :data-transferred) record-size)))))
    )


(Defun Report-Progress (old-progress-count block-count granularity)
  (Let ((progress-count (truncate block-count granularity)))
    (When (> progress-count old-progress-count)
      (Format t " ~d" progress-count))
    progress-count)
  )


(DefParameter *block-transfer-size* 20.)
(DefParameter *streaming-priority* 20.)


(Defun streamer-tape-block-request-complete (tape-command-block blocks-transferred command-name)
  (Condition-Case (error)
      (si:streamer-tape-request-complete tape-command-block command-name)
    (tape:physical-end-of-tape
     ;; Handle this error and generate a new with the right data transfer value.
     (signal 'tape:physical-end-of-tape
	     :device-type 'nupi-device
	     :unit (ldb (byte 8 0) (si:nupi-command-word tape-command-block))
	     :data-transferred (+ blocks-transferred
				  (truncate (Send error :data-transferred) 1024.)))))
  )


(defmethod (nupi-device :write-from-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-arg record-size (zerop (remainder record-size (* si:page-size 4)))
	     "a page-even record-size")
  (let* ((chunk-size (min *block-transfer-size* number-of-blocks))
	 (old-priority (Send si:current-process :priority))
	 number-of-chunks
	 last-chunk-size)
    (multiple-value-bind (a b)
	(floor number-of-blocks chunk-size)
      (setq number-of-chunks (if (zerop b) a (add1 a))
	    last-chunk-size (if (zerop b) chunk-size b)))
    (Unless silent
      (Format t "~&Writing ~d blocks to tape (starting at ~d.) Counting hundreds:"
	      number-of-blocks starting-block))
    (Unwind-Protect
	(using-resource (buffer-1 si:dma-buffer chunk-size)
	  (using-resource (buffer-2 si:dma-buffer chunk-size)
	    (using-resource (disk-command-block si:dma-buffer 1)
	      (setf (si:dma-buffer-named-structure-symbol disk-command-block) 'si:nupi-command-block)
	      (using-resource (tape-command-block si:dma-buffer 1)
		(setf (si:dma-buffer-named-structure-symbol tape-command-block) 'si:nupi-command-block)
		(Let ((transfer-size (* chunk-size si:page-size 4))
		      (address starting-block))
		  (Send si:current-process :Set-Priority *streaming-priority*)
		  (si:nupi-read-from-disk
		    disk-command-block disk-unit address transfer-size buffer-1 0)
		  (si:nupi-write-to-tape-proceed
		    tape-command-block unit 0 transfer-size buffer-1 0)
		  (do ((count 1 (add1 count))
		       (progress-count 0)
		       (last-transfer-size chunk-size)
		       (buffer-1? t (not buffer-1?)))
		      ((> count number-of-chunks)
		       ;; Wait for last tape command to complete.
		       (streamer-tape-block-request-complete tape-command-block
			 (- address starting-block last-transfer-size) "Tape Write"))
		    (incf address chunk-size)
		    (When (= count number-of-chunks)
		      (Setq chunk-size last-chunk-size
			    transfer-size (* chunk-size si:page-size 4)))
		    ;; Read into buffer not used by tape request.
		    (si:nupi-read-from-disk
		      disk-command-block disk-unit address transfer-size
		      (If buffer-1? buffer-2 buffer-1) 0)
		    ;; Wait for tape request to complete.
		    (streamer-tape-block-request-complete tape-command-block
		      (- address starting-block last-transfer-size) "Tape Write")
		    ;; Start next tape request.
		    (si:nupi-write-to-tape-proceed
		      tape-command-block unit 0 transfer-size (If buffer-1? buffer-2 buffer-1) 0)
		    (setq last-transfer-size chunk-size)
		    (Unless silent
		      (Setq progress-count
			    (Report-Progress progress-count (- address starting-block) 100.)))))))))
      (Send si:current-process :Set-Priority old-priority)))
  )


(Defun Compare-Buffers (buffer-a buffer-b size)
  (let ((alphabetic-case-affects-string-comparison t))
    (Unless (%string-equal buffer-a 0 buffer-b 0 size)
      (string-compare buffer-a buffer-b)))
  )

(Defun Buffer-Compare (buffer-a buffer-b size block-number address error-list)
  (Let ((compare
	  (compare-buffers
	    (si:dma-buffer-string buffer-a) (si:dma-buffer-string buffer-b) size)))
    (ZL:If (Null compare)
	error-list
      (format *error-output* "~& Error in record number ~d; Byte: ~d"
	      block-number (abs compare))
      (cons (cons block-number address) error-list)))
  )


(defmethod (nupi-device :compare-to-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (let* ((chunk-size (min *block-transfer-size* number-of-blocks))
	 (old-priority (Send si:current-process :priority))
	 number-of-chunks
	 last-chunk-size
	 error-list)
    (multiple-value-bind (a b)
	(floor number-of-blocks chunk-size)
      (setq number-of-chunks (if (zerop b) a (add1 a))
	    last-chunk-size (if (zerop b) chunk-size b)))
    (Unless silent
      (Format t "~&Comparing ~d blocks to disk. Counting hundreds:" number-of-blocks))
    (unwind-protect
     (using-resource (tape-buffer-1 si:dma-buffer chunk-size)
      (using-resource (tape-buffer-2 si:dma-buffer chunk-size)
       (using-resource (disk-buffer si:dma-buffer chunk-size)
	(using-resource (disk-command-block si:dma-buffer 1)
	  (setf (si:dma-buffer-named-structure-symbol disk-command-block) 'si:nupi-command-block)
	  (using-resource (tape-command-block si:dma-buffer 1)
	    (setf (si:dma-buffer-named-structure-symbol tape-command-block) 'si:nupi-command-block)
	    (Let ((transfer-size (* chunk-size si:page-size 4))
		  (last-transfer-size 0)
		  (address starting-block))
	      (Send si:current-process :Set-Priority *streaming-priority*)
	      (si:nupi-read-from-tape-proceed
		tape-command-block unit 0 transfer-size tape-buffer-1 0)
	      (do ((count 1 (add1 count))
		   (progress-count 0)
		   (buffer-1? t (not buffer-1?)))
		  ((>= count number-of-chunks)
		   (si:nupi-read-from-disk
		     disk-command-block disk-unit address transfer-size disk-buffer 0)
		   (streamer-tape-block-request-complete
		     tape-command-block (- address starting-block) "Tape Read")
		   (Setq error-list
			 (buffer-compare
			   disk-buffer
			   (if buffer-1? tape-buffer-1 tape-buffer-2)
			   transfer-size
			   (* count chunk-size)
			   address
			   error-list)))
		(si:nupi-read-from-disk
		  disk-command-block disk-unit address transfer-size disk-buffer 0)
		(Setq last-transfer-size transfer-size)

		;; Wait for tape request to complete.
		(streamer-tape-block-request-complete
		  tape-command-block (- address starting-block) "Tape Read")

		;; Start next tape read request (into other buffer.)
		(When (= count number-of-chunks)
		  (Setq chunk-size last-chunk-size
			transfer-size (* chunk-size si:page-size 4)))
		(si:nupi-read-from-tape-proceed
		  tape-command-block unit 0 transfer-size (If buffer-1? tape-buffer-2 tape-buffer-1) 0)

		(Setq error-list
		      (buffer-compare
			disk-buffer
			(if buffer-1? tape-buffer-1 tape-buffer-2)
			last-transfer-size
			(* count chunk-size)
			address
			error-list))
		(Unless silent
		  (Setq progress-count (Report-Progress progress-count (- address starting-block) 100.)))
		(incf address chunk-size))))))))
      (Send si:current-process :Set-Priority old-priority))
    (Unless (Null error-list)
      (nreverse error-list) t))
  )

(defmethod (nupi-device :read-to-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-arg record-size (zerop (remainder record-size (* si:page-size 4)))
	     "a page-even record-size")
  (let* ((chunk-size (min *block-transfer-size* number-of-blocks))
	 (old-priority (Send si:current-process :priority))
	 number-of-chunks
	 last-chunk-size)
    (multiple-value-bind (a b)
	(floor number-of-blocks chunk-size)
      (setq number-of-chunks (if (zerop b) a (add1 a))
	    last-chunk-size (if (zerop b) chunk-size b)))
    (Unless silent
      (Format t "~&Reading ~d blocks from tape. Counting hundreds:" number-of-blocks))
    (Unwind-Protect
	(using-resource (buffer-1 si:dma-buffer chunk-size)
	  (using-resource (buffer-2 si:dma-buffer chunk-size)
	    (using-resource (disk-command-block si:dma-buffer 1)
	      (setf (si:dma-buffer-named-structure-symbol disk-command-block) 'si:nupi-command-block)
	      (using-resource (tape-command-block si:dma-buffer 1)
		(setf (si:dma-buffer-named-structure-symbol tape-command-block) 'si:nupi-command-block)
		(Let ((transfer-size (* chunk-size si:page-size 4))
		      (address starting-block))
		  (Send si:current-process :Set-Priority *streaming-priority*)
		  (si:nupi-read-from-tape-proceed
		    tape-command-block unit 0 transfer-size buffer-1 0)
		  (do ((count 1 (add1 count))
		       (progress-count 0)
		       (buffer-1? t (not buffer-1?)))
		      ((> count number-of-chunks)
		       ;; Wait for last tape request to complete.
		       (streamer-tape-block-request-complete
			 tape-command-block (- address starting-block) "Tape Read")
		       (si:nupi-write-to-disk
			 disk-command-block disk-unit address transfer-size
			 (If buffer-1? buffer-1 buffer-2) 0))
		    ;; Wait for tape request to complete.
		    (streamer-tape-block-request-complete
		      tape-command-block (- address starting-block) "Tape Read")
		    (When (= count number-of-chunks)
		      (Setq chunk-size last-chunk-size
			    transfer-size (* chunk-size si:page-size 4)))
		    ;; Start read into other buffer.
		    (si:nupi-read-from-tape-proceed
		      tape-command-block unit 0 transfer-size (If buffer-1? buffer-2 buffer-1) 0)
		    ;; Write completed buffer to disk.
		    (si:nupi-write-to-disk
		      disk-command-block disk-unit address transfer-size
		      (If buffer-1? buffer-1 buffer-2) 0)
		    (incf address chunk-size)
		    (Unless silent
		      (Setq progress-count (Report-Progress progress-count (- address starting-block) 100.)))))))))
      (Send si:current-process :Set-Priority old-priority)))
  )


(defmethod (nupi-device :write-filemark) (&optional (number-of-marks 1))
  (dotimes (j number-of-marks)
    (si:simple-nupi-command
      command-block #x25 unit 0 0 nil nil nil "Tape Write FM")))


(defmethod (nupi-device :erase-tape) ()
  (si:simple-nupi-command
    command-block #x22 unit 0 0 nil nil nil "Tape Erase")
  )



(defmethod (nupi-device :initialize) (&rest ignore)
  ;; BUFFERED       -  set mode to stream (tells formatter to use buffered vs. unbuffered mode).
  ;; SPEED          -  1 = low, 0 & 2 = high, 3 = auto-adjust
  ;; DENSITY        -  0 formatter defaults (Explorer default), 4 QIC-11 (Symbolics & NuMachine)
  ;;                   6 1/2 inch 3200 bpi,  1-3 variations of 1/2 inch
  ;; BLOCK-SIZE     -  Size in bytes of blocks on tape, (24 bit field).
  ;; This command is send to the formatter (controller) rather than to the tape unit itself.
  (let ((buffered T)
	(speed 0)
	(density-code 0)
	(block-size #x200)
	(long-erase T)
	(unload-retension nil)
	(load-retension nil)
	(parms (* 2 (1+ si:%nupi-reserved-b)))
	(buffer (si:dma-buffer-16b command-block)))
    ;; This next word in the parameter block is RESERVED in SCSI spec but NUPI uses it!!
    (aset 12. buffer parms)   ; scsi data length.
    (let ((half-word (dpb (if long-erase 0 1) #o1701	        ; Info for NUPI only.  Not part of 
			  (dpb (if unload-retension 1 0) #o1601	; this SCSI command!!!
			       (dpb (if load-retension 1 0) #o1501 0)))))
      (aset half-word buffer (+ parms 1)))
    ;; From here down we're stuffing params strictly according to SCSI
    ;; halfword    -     SCSI bytes in parameter list
    ;;    3                   2,3
    ;; halfword    -     SCSI bytes in descriptor block
    ;;    4                   0,1    density-code, number-of-blocks MSB
    ;;    5                   2,3    number-of-blocks  (middle, LSB)
    ;;    6                   4,5    reserved, Block Size MSB
    ;;    7                   6,7    Block Size (middle, LSB)
    (aset #x0000 buffer (+ parms 2))
    (let ((half (dpb (if buffered 1 0) #o0401 #x0800)))	 ; 8 is constant=bytes left (block descriptor)
      (aset (dpb speed #o0002 half) buffer (+ parms 3)))
    (aset (dpb density-code #o0004 0) buffer (+ parms 4))
    (aset (dpb (ldb #o2010 block-size) #o1010 0) buffer (+ parms 6))  ; MSB
    (aset (dpb (ldb #o0010 block-size) #o1010	   ; middle & LSB
	       (dpb (ldb #o1010 block-size) #o0010 0))
	  buffer (+ parms 7))
    ;; Send formatter setup command. +++ dma-buffer parameter is just a place holder +++
    (si:simple-nupi-command
      command-block #x41 unit 0 16. command-block 0 nil "Tape Initialize"))
  )


;;; +++ should do better than this +++
(defun nupi-device-present? ()
  (select-processor
    (:explorer t)
    (:cadr)
    (:lambda)))

(define-tape-device nupi-device "nt" nupi-device-present?)
