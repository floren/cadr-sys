;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Testing functions for tape software
;;;
;;; -dg 1/14/85

(defun fill-string-randomly (string)
  (dotimes (c (array-length string))
    (aset (mod c 123) string c)))

(defun test-device (device &optional (record-size 4096))
  (check-arg record-size (zerop (remainder record-size (* si:page-size 4)))
	     "a 1k-byte even record-size")
  (using-resource (outbuf si:dma-buffer 131)
    (using-resource (inbuf si:dma-buffer 131)
      (using-resource (outblock si:dma-buffer (/ record-size si:page-size 4))
	(using-resource (inblock si:dma-buffer (/ record-size si:page-size 4))
	  (with-buffer-wired (inbuf 150)
	    (with-buffer-wired (outbuf 150)
	      (with-buffer-wired (outblock (/ record-size si:page-size 4))
		(with-buffer-wired (inblock (/ record-size si:page-size 4))
		  (format *standard-output* "~&Testing device: ~S with record-size ~D~%"
			  device record-size)
		  (format *standard-output* "~&Setting up device ...")
		  (send device :rewind)
		  (format *standard-output* "done.~2%Testing single block read/write:")
		  (test-single-block-operation device outblock inblock record-size)
		  (format *standard-output* "Testing array read/write operations:")
		  (test-array-operations device outbuf inbuf record-size)
		  (format *standard-output* "Testing disk read/write operations:")
		  (multiple-value-bind (host unit ignore ignore ignore name)
		      (partition-searcher "testing device" 1000 :confirm-write t)
		    (if host
			(test-disk-operations device unit name record-size)
		      (format *standard-output* "~&*** User aborted disk read/write tests. ***~%")))
		  (format *standard-output* "~&~2%Device testing complete.~2%"))))))))))

(defun test-single-block-operation (device outblock inblock record-size)
  (let ((instring (si:dma-buffer-string inblock))
	(outstring (si:dma-buffer-string outblock)))
    (format *standard-output* "~%~5tSetting up buffers ...")
    (fill instring 0)
    (fill-string-randomly outstring)
    (format *standard-output* "done.~%~5TTesting ... ")
    (send device :rewind)
    (send device :write-block outblock record-size)
    (send device :rewind)
    (send device :read-block inblock record-size)
    (if (string-equal outstring instring)
	(format *standard-output* " passed.~2%")
      (format *standard-output* " *** failed ***.~2%"))))

(defun test-array-operations (device outbuffer inbuffer record-size)
  (let* ((outstring (si:dma-buffer-string outbuffer))
	 (out8 (si:dma-buffer-8b outbuffer))
	 (out16 (si:dma-buffer-16b outbuffer))
	 (instring (si:dma-buffer-string inbuffer))
	 (in8 (si:dma-buffer-8b inbuffer))
	 (in16 (si:dma-buffer-16b inbuffer))
	 (number-of-records (floor (string-length outstring) record-size)))
    (format *standard-output* "~%~5TTesting array writing of ~d records (~D bytes each)."
	    number-of-records record-size)
    (format *standard-output* "~%~5TFilling ouput buffer with random data ...")
    (fill-string-randomly outstring)
    (format *standard-output* "done.~%")
    (test-array-transfer
      device outstring outbuffer instring inbuffer number-of-records record-size)
    (test-array-transfer
      device out8 outbuffer in8 inbuffer number-of-records record-size)
    (test-array-transfer
      device out16 outbuffer in16 inbuffer number-of-records record-size)
    ))

(defun test-array-transfer
       (device outarray outbuffer inarray inbuffer number-of-records record-size)
  (let ((outstring (si:dma-buffer-string outbuffer))
	(instring (si:dma-buffer-string inbuffer))
	(number-of-bytes (* number-of-records record-size)))
    (format *standard-output* "~&~5TTesting write of ~A type and read of ~A type arrays ..."
	    (array-type outarray)
	    (array-type inarray))
    (send device :rewind)
    (send device :write-array outarray number-of-records record-size)
    (send device :rewind)
    (send device :read-array inarray number-of-records record-size)
    (ZL:if (string-equal outstring instring
		      :end1 number-of-bytes
		      :end2 number-of-bytes)
	(format *standard-output* " passed.~%")
      (format *standard-output* " *** Failed ***.")
      (format *standard-output* "~&~5TTrying to determine cause of lossage: ")
      (using-resource (block si:dma-buffer (/ record-size si:page-size 4))
      (with-buffer-wired (block (/ record-size si:page-size 4))
	(send device :rewind)
	(format *standard-output* "~&~10TComparing data written by block: ")
	(do ((block-string (si:dma-buffer-string block))
	     (bytes-compared 0 (+ bytes-compared record-size))
	     compare-error)
	    ((or compare-error (= bytes-compared number-of-bytes))
	     (unless compare-error
	       (format *standard-output*
		       "~&~10TIf the :READ-BLOCK test passed, then there is a problem~%~10T~
                        with :READ-ARRAY when given an ~A type array." (array-type inarray))))
	  (send device :read-block block record-size)
	  (write-char #\.)
	  (unless (string-equal outstring block-string
				:start1 bytes-compared
				:end1 (+ bytes-compared record-size)
				:start2 0
				:end2 record-size)
	    (format *standard-output*
		    "~&~10TIf :BLOCK-READ test passed, then there is a problem~%~10T~
                     with :WRITE-ARRAY when given an ~A type array."
		    (array-type outarray))
	    (setq compare-error t))))))))

(defun test-disk-operations (device unit partition record-size)
  (format *standard-output* "~&~5TTesting read/write of 500 disk blocks directly ...")
  (multiple-value-bind (start length name label)
      (si:find-disk-partition partition nil unit)
    label name length
    (send device :rewind)
    (send device :write-from-disk unit (+ start 500) 500 record-size)
    (send device :Rewind)
    (send device :read-to-disk unit start 500 record-size)
    (if (using-resource (rqb1 si:rqb 100 4)
	  (using-resource (rqb2 si:rqb 100 4)
	    (block compare-disk-halves
	      (dotimes (c 5 t)
		(si:disk-read rqb1 unit (+ start (* c 100)))
		(si:disk-read rqb2 unit (+ start 500 (* c 100)))
		(unless (string-equal (si:rqb-8-bit-buffer rqb1)
				      (si:rqb-8-bit-buffer rqb2))
		  (return-from compare-disk-halves nil))))))
	(format *standard-output* " passed.")
      (format *standard-output* " *** failed ***."))))
