;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Nupi support taken from "primitive-io"

;;these can turn into aref and setf after we have "real" art-32b's

(defmacro dma-buffer-aref-32 (array index)
  (Once-Only (index)
    `(dpb (aref (dma-buffer-16b ,array) (1+ (* 2 ,index)))
	  (byte 16 16)
	  (aref (dma-buffer-16b ,array) (* 2 ,index))))
  )

(defmacro dma-buffer-set-aref-32 (array index value)
  (Once-Only (index)
    `(progn
       (aset (ldb (byte 16 0) ,value) (dma-buffer-16b ,array) (* 2 ,index))
       (aset (ldb (byte 16 16) ,value) (dma-buffer-16b ,array) (1+ (* 2 ,index)))
       ,value))
  )

(defsetf dma-buffer-aref-32 dma-buffer-set-aref-32)

;;; Is this good enough?
(Defun DMA-Buffer-P (x)
  (arrayp x))

(Defun %Wired-Status? (page-status)
  (eq (ldb %%pht1-swap-status-code page-status) %pht-swap-status-wired))

(Defun %Page-Wired? (pointer)
  (%Wired-Status? (%page-status pointer)))

;;;
;;; nupi command blocks
;;;

(defun get-nupi-command-block (ignore)
  (let ((command-block (get-dma-buffer 1)))
    (setf (dma-buffer-named-structure-symbol command-block) 'nupi-command-block)
    command-block))

(defun free-nupi-command-block (command-block)
  (free-dma-buffer command-block))

(DefConstant %nupi-command-word 0)
(DefConstant %nupi-status-word 1)
(DefConstant %nupi-scatter-list 2)
(DefConstant %nupi-transfer-count 3)
(DefConstant %nupi-logical-block 4)
(DefConstant %nupi-interrupt-address 5)
(DefConstant %nupi-reserved-a 6)
(DefConstant %nupi-reserved-b 7)

(defmacro nupi-command-word (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-command-word))

(defmacro nupi-status-word (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-status-word))

(defmacro nupi-scatter-list (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-scatter-list))

(defmacro nupi-transfer-count (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-transfer-count))

(defmacro nupi-logical-block (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-logical-block))

(defmacro nupi-interrupt-address (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-interrupt-address))

(defmacro nupi-reserved-a (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-reserved-a))

(defmacro nupi-reserved-b (command-block)
  `(dma-buffer-aref-32 ,command-block %nupi-reserved-b))

(defselect ((nupi-command-block named-structure-invoke))
  (:print-self (array stream ignore ignore)
    (printing-random-object (array stream :typep)
      (When (%page-wired? array)
	(format stream "Wired #x~x; "
		(vadr-to-nubus-phys (dma-buffer-data-vadr array))))
      (format stream "Command: ~a" (or (cadr (assq (ldb (byte 8 24) (nupi-command-word array))
						   nupi-commands))
				       "Unknown"))
      (format stream "; Unit #x~x" (ldb (byte 8 0) (nupi-command-word array)))
      (Let ((status (nupi-status-word array))
	    list)
	(When (ldb-test (byte 1 31) status)
	  (push "Busy" list))
	(When (ldb-test (byte 1 30) status)
	  (push "Complete" list))
	(When (ldb-test (byte 1 29) status)
	  (push "Error" list))
	(when list
	  (format stream "; Status: ")
	  (format:print-list stream "~a" list)))
      
      (Format Stream "; Count ~d." (nupi-transfer-count array))
      (format stream "; Block ~d." (nupi-logical-block array))))
  (:describe (array)
    (format t "~&~S:" array)
    (let ((command-word (nupi-command-word array))
	  (status (nupi-status-word array))
	  (scatter-list (nupi-scatter-list array))
	  (transfer-count (nupi-transfer-count array))
	  (block (nupi-logical-block array))
	  (interrupt-address (nupi-interrupt-address array))
	  (reserved-a (nupi-reserved-a array))
	  (reserved-b (nupi-reserved-b array)))

      (format t "~&Unit #x~x" (ldb (byte 8 0) command-word))
      (format t "~&Spare ~s" (ldb (byte 8 8) command-word))
      (format t "~&Options ~s" (ldb (byte 8 16) command-word))
      (if (ldb-test (byte 1 20) command-word) (format t " Swap-partial-completion-interrupt"))
      (if (ldb-test (byte 1 21) command-word) (format t " device-address-is-physical"))
      (if (ldb-test (byte 1 22) command-word) (format t " SCATTER"))
      (if (ldb-test (byte 1 23) command-word) (format t " Interrupt-enable"))
      (format t "~&Command #x~16r ~a" (ldb (byte 8 24) command-word)
	      (cadr (assq (ldb (byte 8 24) command-word) nupi-commands)))

      (format t "~&Status ~s" status)
      (format t "~&  Busy ~s" (ldb (byte 1 31) status))
      (format t "~&  Complete ~s" (ldb (byte 1 30) status))
      (format t "~&  Error ~s" (ldb (byte 1 29) status))
      (format t "~&  Retries required ~s" (ldb (byte 1 28) status))
      (format t "~&  Aux status available ~s" (ldb (byte 1 27) status))
      (format t "~&  Paging partial completion ~s" (ldb (byte 1 26) status))
      (format t "~&  spare ~s" (ldb (byte 2 24) status))
      (let ((error (ldb (byte 8 16) status)))
	(format t "~&  controller error ~s" error)
	(when (not (zerop error))
	  (format t "  Class: \"~a\"" (nth (ldb (byte 3 21) status) nupi-error-classes))
	  (format t "  ~a" (cadr (assq error nupi-controller-errors)))
	  ))
      
      (let ((error (ldb (byte 8 8) status)))
	(format t "~&  device error ~s" error)
	(when (not (zerop error))
	  (format t "  Class \"~a\"" (nth (ldb (byte 3 13) status) nupi-error-classes))
	  (format t "  ~a " (cadr (assq error nupi-device-errors)))
	  ))
      (format t "~&  spare ~s" (ldb (byte 3 5) status))
      (format t "~&  ECC applied ~s" (ldb (byte 1 4) status))
      (format t "~&  n-retries ~s" (ldb (byte 3 0) status))

      (format t "~&scatter-list #x~16r" scatter-list)

      (format t "~&Transfer count ~d." transfer-count)
      (format t "~&Device block address ~s" block)
      (format t "~&Interrupt address #x~16r" interrupt-address)
      (format t "~&Reserved ~s ~s" reserved-a reserved-b)

      (when (ldb-test (byte 1 22) command-word)	;scatter bit
	(format t "~&Scatter list: ")
	(if (not (= (vadr-to-nubus-phys (%pointer-plus
					  (dma-buffer-data-vadr array)
					  8))
		    scatter-list))
	    (format t "~&   *** warning, scatter list doesn't really point here ***"))
	(do ((scatter-index 8 (+ scatter-index 2))
	     (pages-to-go (floor transfer-count 1024) (1- pages-to-go)))
	    ((zerop pages-to-go))
	  (format t "~&#x~8x ~d."
		  (dma-buffer-aref-32 array scatter-index)
		  (dma-buffer-aref-32 array (1+ scatter-index)))))
      ))
    
  (:which-operations (ignore)
    '(:print-self :which-operations :describe))
  )


(defun fill-in-nupi-command (command-block phys-unit command byte-count disk-address
			     dma-buffer dma-buffer-offset-in-pages
			     &aux n-pages)

  (When (or (not (%page-wired? command-block))
	    (and (dma-buffer-p dma-buffer)
		 (not (%page-wired? dma-buffer))))
    (ferror nil "COMMAND-BLOCK and DMA-BUFFER must be wired."))
  
  (Unless (zerop (ldb (byte 10 0) byte-count))
    (ferror nil "byte-count must be an even number of pages"))

  (setq n-pages (floor byte-count 1024))

  (When (and (dma-buffer-p dma-buffer)
	     (or (> (+ dma-buffer-offset-in-pages n-pages)
		    (dma-buffer-size-in-pages dma-buffer))
		 (> n-pages (floor (- page-size 8) 2))))	;number of scatter entries available
    (ferror nil "transfer request too big"))

  ;; really just need to clear first 8 words
  ;;  can't use array-initialize on 32b array, since it stores DTP-FIX tags
  (array-initialize (dma-buffer-16b command-block) 0)

  (setf (nupi-command-word command-block)
	(+ phys-unit
	   (dpb command (byte 8 24) (if (dma-buffer-p dma-buffer) #x400000 0))))	;scatter flag

  (setf (nupi-scatter-list command-block)
	(If (dma-buffer-p dma-buffer)
	    (vadr-to-nubus-phys (%pointer-plus
				  (dma-buffer-data-vadr command-block)
				  8))
	  (or dma-buffer 0)))

  (setf (nupi-transfer-count command-block) byte-count)
  (setf (nupi-logical-block command-block) disk-address)

  (when (dma-buffer-p dma-buffer)
    (do ((vadr (%pointer-plus (dma-buffer-data-vadr dma-buffer)
			      (* dma-buffer-offset-in-pages page-size))
	       (%pointer-plus vadr page-size))
	 (scatter-entry 8 (+ scatter-entry 2))
	 (pages-to-go n-pages (1- pages-to-go)))
	((zerop pages-to-go))
      (let ((padr (vadr-to-nubus-phys vadr)))
	(setf (dma-buffer-aref-32 command-block scatter-entry) padr)
	(setf (dma-buffer-aref-32 command-block (1+ scatter-entry)) 1024)))))

(defun fill-in-nupi-simple-command (command-block phys-unit command byte-count disk-address
			     dma-buffer dma-buffer-offset-in-pages
			     &aux n-pages)

  (When (or (not (%page-wired? command-block))
	    (and (dma-buffer-p dma-buffer)
		 (not (%page-wired? dma-buffer))))
    (ferror nil "COMMAND-BLOCK and DMA-BUFFER must be wired."))
  
;;;  (Unless (zerop (ldb (byte 10 0) byte-count))
;;;    (ferror nil "byte-count must be an even number of pages"))

  (setq n-pages (floor byte-count 1024))

  (When (and (dma-buffer-p dma-buffer)
	     (or (> (+ dma-buffer-offset-in-pages n-pages)
		    (dma-buffer-size-in-pages dma-buffer))
		 (> n-pages (floor (- page-size 8) 2))))	;number of scatter entries available
    (ferror nil "transfer request too big"))

  ;; really just need to clear first 8 words
  ;;  can't use array-initialize on 32b array, since it stores DTP-FIX tags
  (array-initialize (dma-buffer-16b command-block) 0)

  (setf (nupi-command-word command-block)
	(+ phys-unit
	   (dpb command (byte 8 24) 0)))

  (setf (nupi-scatter-list command-block)
	(Cond ((dma-buffer-p dma-buffer)
	       (vadr-to-nubus-phys (%pointer-plus
				     (dma-buffer-data-vadr command-block)
				     8)))
	      ((Null dma-buffer) 0)
	      (t
	       (si:dma-buffer-set-aref-32 command-block (1+ si:%nupi-reserved-b) dma-buffer)
	       (vadr-to-nubus-phys (%pointer-plus
				     (dma-buffer-data-vadr command-block)
				     8)))))

  (setf (nupi-transfer-count command-block) byte-count)
  (setf (nupi-logical-block command-block) disk-address)
  )

(defun start-nupi-command (command-block phys-unit command byte-count disk-address
			   dma-buffer dma-buffer-offset-in-pages
			   set-modified)
  (wire-wireable-array command-block 0 nil nil nil)
  ;;could arrange to do DONT-BOTHER-PAGING-IN on all pages but first
  (when (dma-buffer-p dma-buffer)
    (wire-wireable-array dma-buffer 0 nil set-modified nil))
  (fill-in-nupi-command
    command-block phys-unit command byte-count disk-address dma-buffer dma-buffer-offset-in-pages)
  (NuPI-Command-Initiate command-block)
  )

(defun start-nupi-simple-command (command-block phys-unit command byte-count disk-address
			   dma-buffer dma-buffer-offset-in-pages
			   set-modified)
  (wire-wireable-array command-block 0 nil nil nil)
  ;;could arrange to do DONT-BOTHER-PAGING-IN on all pages but first
  (when (dma-buffer-p dma-buffer)
    (wire-wireable-array dma-buffer 0 nil set-modified nil))
  (fill-in-nupi-simple-command
    command-block phys-unit command byte-count disk-address dma-buffer dma-buffer-offset-in-pages)
  (NuPI-Command-Initiate command-block)
  )

;;; This should be fixed up for the tape unit to use io-proceed.
(Defun NuPI-Command-Initiate (command-block)
  ;; Do a proceedable initiate if the request is for the tape otherwise
  ;; do the busy wait scheme.
  (If (= (ldb (byte 8 0) (nupi-command-word command-block)) 24)
      (Without-Interrupts
	(aref command-block 0)
	(%nubus-write #xF2 #xE00004
	   (vadr-to-nubus-phys
	     (%pointer-plus command-block (array-data-offset command-block)))))
    (%io-cmd-run command-block))
  )


(Defun NuPI-Command-Complete-P (command-block)
  (ldb-test (byte 1 30.) (nupi-status-word command-block)))

(Defun NuPI-Command-Error-P (command-block)
  (ldb-test (byte 1 29.) (nupi-status-word command-block)))

(Defun NuPI-Check-Status (command-block)
  (Let ((device-status (ldb #o1010 (si:nupi-status-word command-block)))
	(unit (ldb (byte 8 0) (si:nupi-command-word command-block)))
	(data-transferred (si:nupi-transfer-count command-block)))
    (When (NuPI-Command-Error-P command-block)
      (Unless (Memq device-status '(#x4A #x4B #x4C #x4D #x4E #x4F))
	(ferror nil "NuPI error, command block: ~s" command-block)))
    (Selectq device-status
      ((0 #x48 #xC8)   ; no error, SCSI sense available, correctable data error: ignore.
       nil)
      ((#x4C #x4D #x4E #x4F)
       (signal 'tape:filemark-encountered
	       :device-type 'nupi-device
	       :unit unit
	       :data-transferred data-transferred))
      ((#x4A #x4B) ; end of tape, end of recorded media
       (signal 'tape:physical-end-of-tape
	       :device-type 'nupi-device
	       :unit unit
	       :data-transferred data-transferred))))
  )


(Defun wait-for-nupi-command (command-block &optional (command "NuPI Wait"))
  (process-wait command #'NuPI-Command-Complete-P command-block)
  (NuPI-Check-Status command-block))

(Defun finish-nupi-command (command-block dma-buffer)
  (unwire-wireable-array command-block 0 nil)
  (When (dma-buffer-p dma-buffer)
    (unwire-wireable-array dma-buffer 0 nil)))

(defun nupi-logical-to-physical-unit (logical-unit)
  (dpb (ldb (byte 3 1) logical-unit)
       (byte 3 3)
       (ldb (byte 1 0) logical-unit)))

(defun simple-nupi-command (command-block command logical-unit disk-address byte-count
			    dma-buffer dma-buffer-offset-in-pages
			    set-modified &optional (command-name "NuPI Wait"))
  (start-nupi-simple-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      command
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      set-modified)
  (wait-for-nupi-command command-block command-name)
  (finish-nupi-command command-block dma-buffer))

(defun nupi-read-from-disk (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      #x12
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      t)
  (wait-for-nupi-command command-block "Disk Read")
  (finish-nupi-command command-block dma-buffer)
  )

(defun nupi-write-to-disk (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      #x13
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      t)
  (wait-for-nupi-command command-block "Disk Write")
  (finish-nupi-command command-block dma-buffer)
  )

(defun nupi-read-from-tape (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      #x12
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      t)
  (wait-for-nupi-command command-block "Tape Read")
  (finish-nupi-command command-block dma-buffer)
  )
  
(defun nupi-write-to-tape (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      #x13
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      nil)
  (wait-for-nupi-command command-block "Tape Write")
  (finish-nupi-command command-block dma-buffer))



(defun nupi-read-from-tape-proceed (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      #x12
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      t)
  )

(defun nupi-write-to-tape-proceed (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
		      (nupi-logical-to-physical-unit logical-unit)
		      #x13
		      byte-count
		      disk-address
		      dma-buffer
		      dma-buffer-offset-in-pages
		      nil))


(defun streamer-tape-request-complete (command-block command-name)
  (process-wait command-name #'NuPI-Command-Complete-P command-block)
  (NuPI-Check-Status command-block)
  (finish-nupi-command command-block nil)
  )