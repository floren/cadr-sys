;;; -*- Mode:LISP; Package:FS;Base:8 -*-
;;; Tapemaster Tape Drive Controller support.
;;; Documentation for the controller can be found in Computer Products Corporation
;;; document #21010011. This code is based on the July 81 (C) edition.
;;; (c) 1983 Lisp Machine Incorporated
;;; This file contains the constants and other fairly-constant data.
;;;
;;; This version does not do anything tricky with DMA.
;;; Nor does it support separate processes using the same controller.

;;; When debugging, set this to T
(defvar *await-command-completion* ())

(defvar *wait-controller-whostate* "MagTape")
(defvar *wait-unit-whostate* "MT Unit")
(defvar *wait-lock-whostate* "MT Lock")

(defvar *notify-on-retry* t)

;;; SYSTEM CONSTANTS -- COMPILED INTO CODE
;;;  Use #. to get value into compiler  <- fortunately, does not seem to be true.

(eval-when (compile load eval)

;;; These two (the configuration pointer address and the IO address) are set on the board with
;;; dip switches and jumpers.  (See appendix D of the documentation.)

;the #x80 part of this is wired with jumpers, the 6 is implied.  Byte 86 should
; have 1 for 16 bit system.  byte 87 is UNUSED.  Byte 88 is the LOW bits of the
; system configuration block.  89 is the next low bits, then 8A, then 8B is the MSBs. !!!
(defconst tapemaster-system-configuration-pointer-address #x86)	;#x86
(defprop :configuration-pointer tapemaster-system-configuration-pointer-address tm-address)

(defconst tapemaster-io-address #x60)
(defprop :io tapemaster-io-address tm-address)


(defconst tapemaster-system-configuration-block-address #x8000)
(defprop :configuration-block tapemaster-system-configuration-block-address tm-address)

(defconst tapemaster-channel-control-block-address #x9000) ; #x1f0
(defprop :channel-control-block tapemaster-channel-control-block-address tm-address)

(defconst tapemaster-iopb-base-address 0)
(defprop :iopb tapemaster-iopb-base-address tm-address)

(defconst tapemaster-multibus-iopb-address #xa000) ; #x1fc0
(defprop :multibus-iopb tapemaster-multibus-iopb-address tm-address)

(defconst tapemaster-block-buffer #xb000) ; should be safe for at least 10240. byte blocks
(defprop :block-buffer tapemaster-block-buffer tm-address)

(defmacro tm+ (base &rest offsets) `(+ ,(get base 'tm-address) ,@offsets))

(defmacro defoffset-16 (offset8 value rqoffset)
  `(progn ':compile
	  (defconst ,offset8 ,value)
	  (defconst ,rqoffset (// ,value 2))))

;;; IOPB address offsets, figure 4-1 %tmrq's are for offsets inside the RQB
(defoffset-16 %tapemaster-command 0 %tmrq-command)
(defoffset-16 %tapemaster-command-zero 2 %tmrq-command-zero)
(defoffset-16 %tapemaster-control 4 %tmrq-control)
(defoffset-16 %tapemaster-return-count 6 %tmrq-return-count)
(defoffset-16 %tapemaster-buffer-size 10 %tmrq-buffer-size)
(defoffset-16 %tapemaster-records 12 %tmrq-records) ; records/overrun
(defoffset-16 %tapemaster-source 14 %tmrq-source)  ; source/destination 
(defoffset-16 %tapemaster-source-high 16 %tmrq-source-high) ; msb
(defoffset-16 %tapemaster-status 20 %tmrq-status)
(defoffset-16 %tapemaster-interrupt 22 %tmrq-interrupt)      ;      should be 0
(defoffset-16 %tapemaster-link 22 %tmrq-link)      ;      should be 0
(defoffset-16 %tapemaster-interrupt-high 24 %tmrq-interrupt-high) ; msb, should be 0
(defoffset-16 %tapemaster-link-high 24 %tmrq-link-high) ; msb, should be 0

(defconst tapemaster-iopb-length 11. ; can be up to 16. (limited by disk RQB)
  "The number of 16bit quantities in an IOPB.")

;;; Control register byte descriptors, figure 4-2
(defconst %%tapemaster-width 1701)
(defconst %%tapemaster-continuous 1401)
(defconst %%tapemaster-speed 1301)   ; These are the same
(defconst %%tapemaster-density 1301) ; field.
(defconst %%tapemaster-reverse 1201)
(defconst %%tapemaster-bank-select 1001)
(defconst %%tapemaster-bus-lock 701)
(defconst %%tapemaster-link 601)
(defconst %%tapemaster-interrupts 501)
(defconst %%tapemaster-mailbox 401)
(defconst %%tapemaster-tape-select 202)

(defsubst tapemaster-unit-field (unit) ; Calls unit 4 bank 0, unit 0
  (dpb (ldb 201 unit) %%tapemaster-bank-select (lsh (ldb 0002 unit) 2)))

;;; Status register byte descriptors, figure 4-3
;; Drive status
(defconst %%tapemaster-write-protect 101)
(defconst %%tapemaster-formatter-busy 201)
(defconst %%tapemaster-ready 301)
(defconst %%tapemaster-end-of-tape 401)
(defconst %%tapemaster-load-point 501)
(defconst %%tapemaster-online 601)
(defconst %%tapemaster-file-mark 701)
;; Command status
(defconst %%tapemaster-error 1005)
(defconst %%tapemaster-retry 1501)
(defconst %%tapemaster-complete 1601)
(defconst %%tapemaster-entered 1701)

;;; Commands in the command register
(defconst tapemaster-configure #x0)
(defconst tapemaster-set-page #x8)
(defconst tapemaster-nop #x20)
(defconst tapemaster-drive-reset #x90)
(defconst tapemaster-drive-status #x28)
(defconst tapemaster-tape-assign #x74)
(defconst tapemaster-overlapped-rewind #x04)
(defconst tapemaster-read-foreign-tape #x1c)
(defconst tapemaster-rewind #x34)
(defconst tapemaster-offline-and-unload #x38)
(defconst tapemaster-write-filemark #x40)
(defconst tapemaster-search-filemark #x44)
(defconst tapemaster-search-multiple #x94)
(defconst tapemaster-space #x48)
(defconst tapemaster-space-filemark #x70)
(defconst tapemaster-erase #x4c)
(defconst tapemaster-erase-whole-tape #x50)
(defconst tapemaster-direct-read #x2c)
(defconst tapemaster-direct-write #x30)
(defconst tapemaster-direct-edit #x3c)
(defconst tapemaster-buffered-read #x10)
(defconst tapemaster-buffered-write #x14)
(defconst tapemaster-buffered-edit #x18)
(defconst tapemaster-streaming-read #x60)
(defconst tapemaster-streaming-write #x64)
(defconst tapemaster-block-move #x80)
(defconst tapemaster-exchange #x0c)
(defconst tapemaster-short-memory-test #x54)
(defconst tapemaster-long-memory-test #x58)
(defconst tapemaster-controller-confidence-test #x5c)
(defconst tapemaster-test-read-write-timing #x68))
; End of EVAL-WHEN

(defvar tapemaster-read-command tapemaster-direct-read
  "What command the controller gets to read.")

(defvar tapemaster-write-command tapemaster-direct-write
  "What command the controller gets to read.")

;;; The ``inverse'' of this function is not currently provided because commands
;;; need very little of the IOPB to actually work
(defun tm-aset-iopb (array)
  "Store the IOPB into ARRAY, which is usually an RQB."
  (dotimes (offset tapemaster-iopb-length)
    (setf (aref array offset) (tapemaster-read-iopb-16 (* 2 offset)))))

(defsubst tmrqb-status-word (rqb) (aref rqb %tmrq-status))

(defsubst tmrqb-status-eof (rqb) (ldb-test %%tapemaster-filemark (tmrqb-status-word rqb)))
(defsubst tmrqb-status-bof (rqb) (ldb-test %%tapemaster-load-point (tmrqb-status-word rqb)))
(defsubst tmrqb-status-eot (rqb) (ldb-test %%tapemaster-end-of-tape (tmrqb-status-word rqb)))

(defun make-tapemaster-command-list (command-list)
  (cond ((null command-list) nil)
	(t
	 (cons (cons (symeval (car command-list))
		     (string-capitalize-words (substring (string (car command-list)) 11.)))
	       (make-tapemaster-command-list (cdr command-list))))))

(defconst tapemaster-command-list
	  (make-tapemaster-command-list
	    '(tapemaster-configure
	       tapemaster-set-page
	       tapemaster-nop
	       tapemaster-drive-reset
	       tapemaster-drive-status
	       tapemaster-tape-assign
	       tapemaster-overlapped-rewind
	       tapemaster-read-foreign-tape
	       tapemaster-rewind
	       tapemaster-offline-and-unload
	       tapemaster-write-filemark
	       tapemaster-search-filemark
	       tapemaster-search-multiple
	       tapemaster-space
	       tapemaster-space-filemark
	       tapemaster-erase
	       tapemaster-erase-whole-tape
	       tapemaster-direct-read
	       tapemaster-direct-write
	       tapemaster-direct-edit
	       tapemaster-buffered-read
	       tapemaster-buffered-write
	       tapemaster-buffered-edit
	       tapemaster-streaming-read
	       tapemaster-streaming-write
	       tapemaster-block-move
	       tapemaster-exchange
	       tapemaster-short-memory-test
	       tapemaster-long-memory-test
	       tapemaster-controller-confidence-test
	       tapemaster-test-read-write-timing)))

(defun tapemaster-command-as-string (cmd)
  (let ((command-number-and-name (assoc cmd tapemaster-command-list)))
    (cond ((null command-number-and-name)
	   (format nil "Unknown Tapemaster Command #x~16r" cmd))
	  (t
	   (cdr command-number-and-name)))))

(defconst tapemaster-error-list
	  '((#x0 "Everything OK")
	    (#x1 "timed out waiting for expected data busy false")
	    (#x2  "timed out waiting for expected data busy false"
	     "formatter busy and ready true")
	    (#x3  "timed out waiting for expected ready false")
	    (#x4  "timed out waiting for expected ready true")
	    (#x5  "timed out waiting for expected data busy true")
	    (#x6  "memory time out during system memory reference")
	    (#x7  "blank tape was encountered unexpectedly")
	    (#x8  "error in micro diagnostic")
	    (#x9  "unexpected EOT or BOT encountered")
	    (#xa  "hard or soft error; retries didn't help")
	    (#xb  "read overflow or write underflow")
	    ; unused #xc
	    (#xd  "read parity error on controller to transport interface")
	    (#xe  "internal prom checksum error")
	    (#xf  "tape time out"
	     "if you are doing a read; probably the read count is larger than then record"
	     "if you are doing a write; maybe the tape is bad")
	    (#x10 "tape not ready")
	    (#x11 "you tried to write, but the tape doesn't have a write ring")
	    ; unused #x12
	    (#x13  "diagnostic command attempted; but the diagnostic jumper is not in")
	    (#x14  "attempt to link from a command that doesn't allow linking")
	    (#x15  "unexpected file mark during read operation")
	    (#x16  "error in parameter block; usually 0 or too large byte count")
	    ; unused #x17
	    (#x18  "UHE: unidentified hardware error")
	    (#x19  "streaming read or write terminated by operating system or disk")))

(defconst tapemaster-error-table
	  (let ((array (make-array 32. ':area permanent-storage-area)))
	    (dolist (x tapemaster-error-list)
	      (setf (aref array (first x)) (cdr x)))
	    array))

(defun tapemaster-error-as-string (error-number &optional (brief-p t))
  (let ((error-information (aref tapemaster-error-table error-number)))
    (cond ((null error-information)
	   (format nil "Unknown error number #x~16r" error-number))
	  ((null brief-p)
	   (with-output-to-string (stream)
	     (dolist (next-string error-information)
	       (format stream "~&~A" next-string))))
	  (t
	   (first error-information)))))

(defsignal tapemaster-error error (code status-word message)
  "Signalled by failing operations using tape.")

(defun maybe-signal-tapemaster-error
       (status-word &aux (error-code (ldb #.%%tapemaster-error status-word)))
  "Signals a tape error only if the error-code is non-zero."
  (and *notify-on-retry* (ldb-test #.%%tapemaster-retry status-word)
       (progn
	 (format error-output "~&[Magtape: Retried operation.]~%")
	 (tapemaster-print-status status-word error-output)))
  (or  (zerop error-code)
       (signal-condition (make-condition 'tapemaster-error
					 "Tapemaster error ~16RH: ~A"
					 error-code
					 status-word
					 (tapemaster-error-as-string error-code ())))))
