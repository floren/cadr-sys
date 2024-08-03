;;; -*- Mode:LISP; Package:(TM global); Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Tapemaster support for the Lambda
;;;
;;; -dg 8/1/85
;;;
;;; (c) Copyright 1984, Lisp Machines Incorporated.
;;;

(defvar *notify-on-errors* nil)
(defvar *skip-bad-blocks* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tapemaster command opcodes
;;;

(defvar *opcode-alist* ())

(defmacro def-op (command-symbol
		  opcode
		  wait-whostate
		  max-completion-time)
  (let ((cons '#:cons))
    `(progn (defconstant ,command-symbol ,opcode "A Tapemaster command opcode")
	    (let ((,cons (ass '= ,opcode *opcode-alist*)))
	      (if ,cons
		  (rplacd ,cons ',(list (eval max-completion-time) wait-whostate))
		(push '(,opcode ,(eval max-completion-time) ,wait-whostate) *opcode-alist*))))))


;;; Control Status
(def-op %configure #x0 "[TM] Configure" 2)
(def-op %set-page #x8 "[TM] Set Page Register" 1)
(def-op %nop #x20 "[TM] No-op" 1)
(def-op %drive-status #x28 "[TM] Status" 1)
(def-op %tape-assign #x74 "[TM] No-op" 1)	;Documentation says this is a No-op right now.
(def-op %set-retry #x8c "[TM] Set Error Retries" 1)
(def-op %drive-reset #x90 "[TM] Reset Drive" 5)
(def-op %clear-interrrupt #x9c "[TM] Clear Interrupt" 1)

;;; Tape Position
(def-op %overlapped-rewind #x04 "[TM] Overlapped Rewind" (* 5 60))
(def-op %read-foreign-tape #x1c "[TM] Determine Block Size" 5)
(def-op %rewind #x34 "[TM] Rewind" (* 60 5))
(def-op %offline-and-unload #x38 "[TM] Unload" (* 60 5))
(def-op %write-filemark #x40 "[TM] Write Filemark" 5)
(def-op %search-filemark #x44 "[TM] Search Filemark" (* 60 10))
(def-op %space #x48 "[TM] Space" '(lambda (tpb) (* (tpb-records tpb) 3)))
(def-op %erase-fixed-length #x4c "[TM] Fixed Erase" '(lambda (tpb) (* (tpb-records tpb) 2)))
(def-op %erase-tape #x50 "[TM] Erase Tape" (* 60 5))
(def-op %space-filemark #x70 "[TM] Space Filemark" '(lambda (tpb) (* (tpb-records tpb) 3)))
(def-op %search-multiple-filemarks #x94 "[TM] Search Filemark" (* 60 10))

;;; Data Transfer
(def-op %buffered-read #x10 "[TM] Buffered Read" :unused)
(def-op %buffered-write #x14 "[TM] Buffered Write" :unused)
(def-op %buffered-edit #x18 "[TM] Buffered Edit" :unused)
(def-op %direct-read #x2c "[TM] Direct Read" 20)
(def-op %direct-write #x30 "[TM] Direct Write" 20)
(def-op %direct-edit #x3c "[TM] Direct Edit" 5)
(def-op %streaming-read #x60 "[TM] Streaming Read" 20)
(def-op %streaming-write #x64 "[TM] Streaming Write" 20)

;;; Special-commands
(def-op %block-move #x80 "[TM] Block Move" :unused)
(def-op %exchange #x0c "[TM] Exchange" :unused)

;;; Diagnostic Commands
(def-op %short-memory-test #x54 "[TM] Short Mem Test" :unused)
(def-op %long-memory-test #x58 "[TM] Long Mem Test" :unused)

;;; Don't know anything about these (not in manual) -DG
(def-op %controller-confidence-test #x5c "[TM] Controller Confidence Test" (* 60 60))
(def-op %test-read-write-timing #x68 "[TM] Test Read/Write" (* 60 60))

(defun lookup-opcode-info (opcode)
  (declare (values wait-time-in-seconds whostate))
  (values-list (cdr (ass '= opcode *opcode-alist*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Errors
;;;

(defvar *error-code-array* (zl:make-array 27 :type 'art-q))
(defvar *read-data-recovered-errors* ())
(defvar *write-data-recovered-errors* ())

(defmacro def-error (code &body signal-arguments)
  `(progn
     (aset ',signal-arguments *error-code-array* ,code)))

(def-error #x01 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x01
	   :error-message "Tapemaster: internal timeout 1")

(def-error #x02 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x02
	   :error-message "Tapemaster: internal timeout 2")

(def-error #x03 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x03
	   :error-message "Tapemaster: internal timeout 3")

(def-error #x04 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x04
	   :error-message "Tapemaster: internal timeout 4")

(def-error #x05 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x05
	   :error-message "Tapemaster: internal timeout 5")

(def-error #x06 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x06
	   :error-message "Tapemaster: internal timeout 6")

(defflavor blank-tape () (tape:driver-error))

(def-error #x07 blank-tape
	   :device-type tapemaster
	   :error-code #x07
	   :error-message "Tapemaster: The tape is blank...")

(def-error #x08 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x08
	   :error-message "Tapemaster: error during micro-diagnostic")

(defflavor eot-or-bot () (tape:driver-error))
(defflavor eot () (tape:driver-error))
(defflavor bot () (tape:driver-error))

(def-error #x09 eot-or-bot
	   :device-type tapemaster
	   :error-code #x09
	   :error-message "Tapemaster: End-of-tape or Beginning-of-tape was encountered")

(defflavor hard-or-soft-error () (tape:driver-error))

(def-error #x0a hard-or-soft-error
	   :device-type tapemaster
	   :error-code #x0a
	   :error-message "Tapemaster: A hard or soft error was encountered on the tape.")

(defflavor interface-timeout () (tape:driver-error))

(def-error #x0b interface-timeout
	   :device-type tapemaster
	   :error-code #x0b
	   :error-message "Tapemaster: read overflow or write underflow")

(def-error #x0c tape:hardware-error
	   :device-type tapemaster
	   :error-code #x0c
	   :error-message "Tapemaster: an illegal command was issued")

(def-error #x0d tape:hardware-error
	   :device-type tapemaster
	   :error-code #x0d
	   :error-message "Tapemaster: a read parity error occurred between controller and drive")

(def-error #x0e tape:hardware-error
	   :device-type tapemaster
	   :error-code #x0d
	   :error-message "Tapemaster: prom checksum error")

(defflavor time-out-error () (tape:driver-error))

(def-error #x0f time-out-error
	   :device-type tapemaster
	   :error-code #x0f
	   :error-message "Tapemaster: a tape time-out error occurred")

(defflavor tape-not-ready () (tape:driver-error))

(def-error #x10 tape-not-ready
	   :device-type tapemaster
	   :error-code #x10
	   :error-message "Tapemaster: Drive is not ready")

(defflavor write-protected () (tape:driver-error))

(def-error #x11 write-protected
	   :device-type tapemaster
	   :error-code #x11
	   :error-message "Tapemaster: Tape is write protected.")

(def-error #x12 tape:driver-error
	   :device-type tapemaster
	   :error-code #x12
	   :error-message "Tapemaster: unhandled tape error: #x12; call LMI.")

(def-error #x13 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x13
	   :error-message "Tapemaster: diagnostic jumper not installed for diagnostic")

(def-error #x14 tape:driver-error
	   :device-type tapemaster
	   :error-code #x13
	   :error-message "Tapemaster: last command does not allow links")

(defflavor filemark-encountered () (tape:driver-error))

(def-error #x15 filemark-encountered
	   :device-type tapemaster
	   :error-code #x15
	   :error-message "Tapemaster: a filemark was encountered during read")

(def-error #x16 tape:driver-error
	   :device-type tapemaster
	   :error-code #x16
	   :error-message "Tapemaster: error in parameter block")

(def-error #x17 tape:driver-error
	   :device-type tapemaster
	   :error-code #x17
	   :error-message "Tapemaster: the tape drive went offline during a command")

(def-error #x18 tape:hardware-error
	   :device-type tapemaster
	   :error-code #x18
	   :error-message "Tapemaster: unidentifyable hardware error occurred")

(def-error #x19 tape:driver-error
	   :device-type tapemaster
	   :error-code #x19
	   :error-message "Tapemaster: streaming operation terminated by system")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TM Global variables and calculations of boot-dependent constants
;;;
;;;

(defconst *data-error-retry-count* 5
  "The number of retries attemped before signalling a data error
   (Used when an unreadable/unwritable tape block is encountered.)")

(defconst *multibus-io-space-virtual-address* (lsh (ash #o177377000 -1) 1)
  "The lambda virtual address where the multibus I/O space is mapped.")

(defconst *io-registers-address* #x60
  "The tapemaster ATTENTION and RESET registers' base address.")

(defvar *control-memory* nil)

(defvar *base-multibus-page-number* nil
  "The page number of the first multibus page reserved for the tapemaster.")

(defvar *number-of-multibus-pages* nil
  "The number of multibus pages reserve to the tapemaster
   (and potentially mapped to NuBus physical pages.")

(defvar *number-of-control-pages* 2
  "Number of pages (starting at relative page 0) we reserve in the
   mapped tapemaster area for overhead (the rest being data area).")

(defvar *scb* nil
  "The Tapemaster System Configuration Block.")

(defconst *scb-multibus-address* nil
  "The address of the System Configuration Block.")

(defconst *scb-virtual-address* nil
  "The lambda vitrual address of the System Configuration Block.")

(defvar *ccb* nil
  "The Tapemaster Channel Control Block.")

(defconst *ccb-multibus-address* nil
  "The address of the Channel Control Block.")

(defconst *ccb-virtual-address* nil
  "The lambda virtual address of the Channel Control Block.")

(defvar *tpb* nil
  "The Tapemaster Tape Parameter Block.")

(defconst *tpb-multibus-address* nil
  "The address of the Tape Parameter Block.")

(defconst *tpb-virtual-address* nil
  "The lambda virtual adress of the Tape Parameter Block")

(defconst *available* nil
  "T when the tapemaster is available for use.")

(defconst *sdu-version-7-font-table* #x8f0)

(defsubst number-of-data-pages ()
  "The number of 1Kbyte pages available for data (after the SCB and CCB)."
  (- *number-of-multibus-pages*
     *number-of-control-pages*))

(defsubst data-length-in-bytes ()
  "The length in bytes of the tapemaster data area."
  (* (number-of-data-pages) (* si:page-size 4)))

(defsubst data-first-page ()
  "Multibus page number of the first page in the tapemaster mapped
   multibus address space usable for data."
  (+ *base-multibus-page-number*
     *number-of-control-pages*))

(defsubst multibus-page-address (multibus-page)
  "Returns the multibus byte address of the first
   byte in MULTIBUS-PAGE."
  (lsh multibus-page 10))

(defsubst data-starting-address ()
  "Returns the byte address of the first word in the data area."
  (multibus-page-address (data-first-page)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support of Version 7 SDUs
;;;
;;;

(defun sdu-is-version-7-p ()
  (do ((sum 0)
       (adr #xfbfff (1- adr)))
      ((< adr #xf4000)
       (= sum #xae0d))
    (setq sum (logand (+ sum (%multibus-read-8 adr)) #xffff))))

(defsubst lambda-only ()
  (when ( si:processor-type-code si:lambda-type-code)
	(ferror nil "This function is LAMBDA-specific")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System Configuration pointer
;;;

;;; The tapemaster system configuration pointer is at #x86 in multibus
;;;address space on the SDU this structure is mapped to #x84, so the first
;;;two bytes are garbage.

(defstruct (scp
	     :size-symbol
	     (:constructor nil)
	     (:print-function
	       (lambda (struct stream)
		 (si:printing-random-object (struct stream :type)))))
  garbage		; adr #x84-#x85 -- garbage

  bus-width		; adrs #x86 - #x87
			; the "logical bus width": 1= 16bits 0= 8bits
			;  should be the same as the dip switch on the board

  scb-pointer-offset	;adr #x88-#x89
			; Offset part of pointer to the system configuration block (SCB)

  scb-pointer-base	;adr #x8a-#x8b
			; Base part of pointer to SCB
  )

(defconst *scp* (zl:make-array scp-size
			    ':type 'art-16b
			    :named-structure-symbol 'scp
			    :leader-length 3
			    :displaced-to (%pointer-plus si:multibus-virtual-address
							 (/ #x84 4)))
  "System Configuration Pointer for the TAPEMASTER controller")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System Configuration Block
;;;

(defstruct (scb
	     (:constructor nil)
	     :size-symbol
	     (:print-function
	       (lambda (struct stream)
		 (si:printing-random-object (struct stream :type)))))
  must-be-3	     ; This value must be #x03 as required by the controller
  ccb-pointer-offset ; Offset part of pointer to the Channel Control Block (CCB)
  ccb-pointer-base   ; Base part of pointer to the CCB
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Channel Control Block

(defstruct (ccb
	     (:constructor nil)
	     :size-symbol
	     (:print-function
	       (lambda (struct stream)
		 (si:printing-random-object (struct stream :type)))))
  ((channel-control-word     ; Used for interrupt control - should be #x11 for
     (byte 8 0))	     ;  normal operations, #x09 to clear an interrupt

   (gate		     ; Value is 0 tapemaster is ready to accept commands
     (byte 8 8)))	     ;  and #xFF otherwise.

  tpb-pointer-offset	     ; Offset part of pointer to the TAPE-PARMETER-BLOCK (TPB)
  tpb-pointer-base	     ; Base part of pointer to the TPB
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tape Parameter Block
;;;

(defstruct (tpb
	     (:constructor nil)
	     :size-symbol
	     (:print-function
	       (lambda (struct stream)
 		 (si:printing-random-object (struct stream :type)))))
  (command %noop)			; The current command code
  command-hi				; Must be zero

  ((control)				; 16 bit control word consisting of the
					; following fields 
   (unit (byte 2 2))			;  unit on current "BANK"
   (mailbox-interrupts (byte 1 4))	;  use mailbox interrupts
   (interrupt-enable (byte 1 5))	;  select interrupt mode, if enabled
   (link (byte 1 6))			;  if 1, follow link to next command block
   (bus-lock (byte 1 7))		;  if 1, lock bus during whole transfer
   (bank-select (byte 1 8))		;  select which bank of 4 drives each
   (skip-eot (byte 1 9))		;  disregards end-of-tape marker on media
   (reverse (byte 1 10))		;  make operation run in reverse
   (density (byte 1 11))		;  Actually SPEED on Cipher Microstreamer
   (continuous (byte 1 12))		;  leave transport running after command
   (buffered-read-enable (byte 1 13))	;  Random unused feature.
					;  Bit 14 not used.
   (bus-width (byte 1 15))	        ;  1=16bits, 0=8 bits
    )

  return-count				; Usually number of bytes transferred, varies
  buffer-size				; Size if record to be read or written
  records				; Argument for command requiring number of records
					;  over which to operate (i.e. SPACE)
  buffer-pointer-offset			; Offset part of buffer pointer - see Tapemaster manual
  buffer-pointer-base			; Base part of buffer pointer

  ((status)				; 16bit Drive status (low) and Command status (high)

   (write-protect (byte 1 1))		;  Tape is write protected
   (formatter-busy (byte 1 2))		;  Formatter is busy
   (ready (byte 1 3))			;  Selected drive is ready
   (eot (byte 1 4))			;  End of tape detected
   (load-point (byte 1 5))		;  Tape is at load point
   (on-line (byte 1 6))			;  Drive is online
   (file-mark (byte 1 7))		;  A filemark was detected during this operation

   (error (byte 5 8))			;  Error code
   (retry (byte 1 13))			;  A retry had to be done on this command
   (complete (byte 1 14))		;  The command has completed
   (command-seen (byte 1 15))		;  The command has been seen by the controller
    )
  link-offset				;  Offset part of link/interrupt pointer
  link-base)				;  Base part of link/interrupt pointer

(defsubst get-stupid-unit (tpb)
  (dpb (byte 1 2)
       (tpb-bank-select tpb)
       (case (tpb-unit tpb)
	 (0 0)
	 (1 2)
	 (2 1)
	 (3 3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error handling
;;;
;;;

(defun check-for-error (&key no-signal (error-code (tpb-error *tpb*)) error-trace)
  (when (not (zerop error-code))
    (when (> error-code #x19)
      (signal 'tape:driver-error
	      :device-type 'tapemaster
	      :error-code error-code
	      :error-message "Unknown tapemaster error code."))
    (when *notify-on-errors*
      (format *error-output* "~&*** Tapemaster got an error #x~2,VX~@[ during ~A~]~&  Error information: ~S~%"
	      (char-int #\0) error-code
	      error-trace
	      (aref *error-code-array* error-code)))
    (condition-case-if no-signal (a-tape-condition)
	(condition-case (condition)
	    (lexpr-funcall 'signal (aref *error-code-array* error-code))
	  (eot-or-bot (if (zerop (tpb-reverse *tpb*))
			  (signal 'eot
				  :device-type 'tapemaster
				  :error-code (send condition :error-code)
				  :error-message "Physical end of tape encountered")
			(signal 'bot
				:device-type 'tapemaster
				:error-code (send condition :error-code)
				:error-message "Physical beginning of tape encountered")))
	  (tape-not-ready (signal 'tape:tape-not-ready
				  :device-type 'tapemaster
				  :unit (get-stupid-unit *tpb*)))
	  (write-protected
	   (signal 'tape:write-protected
		   :device-type 'tapemaster
		   :unit (get-stupid-unit  *tpb*)))
	  (time-out-error
	   (unless (= (tpb-command *tpb*) %direct-read)
	     (signal-condition condition))))
      (tape:tape-error a-tape-condition))))

(defun find-error-in-streaming-buffer (buffer &optional (index 0))
  (let* ((half-index (/ index 2))
	 (buffer-16 (si:dma-buffer-16b buffer))
	 (gate (dpb 0 (byte 6 10) (aref buffer-16 half-index)))
	 (record-size (aref buffer-16 (+ half-index 1))))
    (if (ldb-test (byte 1 3) gate)
	index
      (find-error-in-streaming-buffer buffer (+ index record-size 8)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multibus mapping
;;;

;;; Hopefully as system 104 matures, these will all be general
;;; system functions.

;; The MULTIBUS map is 1024. words at HEX 18000.  It consists of 22 address bits, an unused bit,
;; and valid.  The entire 20. multibus address space is potentially mapped.

(defsubst read-multibus-mapping-register (page-number)
  "Reads the value in the multibus mapping register for PAGE-NUMBER.
   Note: Bit 23 is always set when mapping is used for this page
   so the real nubus physical page value is in bits 0-21."
  (let ((value 0))
    (dotimes (c 3 value)
      (setq value (dpb (%multibus-read-8 (+ #x18000 (* 4 page-number) c))
		       (byte 8 (* c 8))
		       value)))))

(defsubst write-multibus-mapping-register (page-number data)
  "Writes the multibus mapping register for PAGE-NUMBER as being
   mapped to nubus physical page DATA."
  (dotimes (c 3 data)
    (%multibus-write-8 (+ #x18000 (* 4 page-number) c)
		       (ldb (byte 8 (* c 8)) data))))

(defsubst vadr-to-nubus-physical-page (vadr)
  "Takes a Lambda virtual address and returns the nubus physical page
   that this address currently occupies in memory.
   Note: if the virtual address' page is not wired down, this value
         will not be valid is the page gets swapped out.  Therefore
         the page should be wired down before calling this function."
  (compiler:%nubus-physical-address
    (ldb (byte 14 8) (si:%physical-address vadr))))

(defsubst setup-multibus-map (multibus-map-page virtual-address)
  "Setup the multibus mapping register for MULTIBUS-MAP-PAGE to point
   to the nubus physical page that VIRTUAL-ADDRESS occupies."
  (write-multibus-mapping-register
    multibus-map-page
    (dpb 1 (byte 1 23)			; Set the map-active-for-this-page flag (bit 23)
	 (vadr-to-nubus-physical-page
	   virtual-address))))

(defsubst unmap-multibus-page (multibus-page)
  "Zeros out the multibus map entry for MULTIBUS-PAGE, but
   leaves the mapping enabled bit on, to `reserve' the page."
  (write-multibus-mapping-register multibus-page (dpb 1 (byte 1 23) 0)))

(defun map-dma-buffer (buffer first-page number-of-pages)
  "Maps NUMBER-OF-PAGES multibus pages, starting at FIRST-PAGE, to
   the corresponding nubus physical pages containing BUFFER.
   The first page of BUFFER is not mapped, since it only contains
   non-interesting header information."
  (do ((vadr (si:dma-buffer-data-vadr buffer) (%pointer-plus vadr si:page-size))
       (mmp first-page (1+ mmp))
       (np 1 (1+ np)))
      ((> np number-of-pages) number-of-pages)
    (setup-multibus-map mmp vadr)))

(defun reset-data-area-mapping (&optional
				(starting-page (data-first-page))
				(number-of-pages (number-of-data-pages)))
  "Resets the mapping registers of all multibus pages mapped for tapemaster data.
   (Does not alter the control pages.)"
  (dotimes (c number-of-pages)
    (unmap-multibus-page (+ c starting-page))))

(defmacro with-reset-on-abort (&body body)
  `(condition-case (condition)
       (progn . ,body)
     (sys:abort (init) (signal-condition condition))))

(defmacro unwind-mapping-reset (&body body)
  `(unwind-protect
       (progn ,@body)
     (wait-for-command-complete)
     (reset-data-area-mapping)))

(defmacro with-buffer-mapped ((buffer first-page number-of-pages) &body body)
  `(unwind-protect
       (progn
	 (map-dma-buffer ,buffer ,first-page ,number-of-pages)
	 ,@body)
     (condition-case ()
	 (wait-for-command-complete)
       (sys:abort (tm:init)))
     (reset-data-area-mapping ,first-page ,number-of-pages)))
     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support functions for Tapemaster operation
;;;
;;;

(defun array-data-vadr (array)
  (cond ((eq (named-structure-p array) 'si:dma-buffer)
	 (si:dma-buffer-data-vadr array))
	((array-displaced-p array)
	 (array-data-vadr
	   (%p-contents-offset array (si:array-data-offset array))))
	(t (%pointer-plus (%pointer array) (si:array-data-offset array)))))

(defun reset ()
  "Do a hardware reset of the Tapemaster controller."
  (%p-store-tag-and-pointer
    (+ *multibus-io-space-virtual-address* *io-registers-address* 1)
    0 1))

(defun attention ()
  "Issue a channel attention on the tapemaster board."
  (%p-store-tag-and-pointer
    (+ *multibus-io-space-virtual-address* *io-registers-address*)
    0 1))

(defun gate-open? ()
  "Returns T if the tapemaster gate is open."
  (zerop (ccb-gate *ccb*)))

(defun wait (func timeout whostate error-msg proceed-message)
  (error-restart (tape:wait-timeout proceed-message)
    (unless (process-wait-with-timeout whostate timeout func)
      (signal 'tape:wait-timeout
	      :device-type 'tapemaster
	      :unit (get-stupid-unit  *tpb*)
	      :seconds-waited (ceiling timeout 60)
	      :wait-string error-msg))))

(defun wait-for-command-complete ()
  (multiple-value-bind (timeout whostate)
      (lookup-opcode-info (tpb-command *tpb*))
    (unless (gate-open?)
      (wait 'gate-open?
	    (* 60 (if (numberp timeout) timeout (funcall timeout *tpb*)))
	    whostate
	    (format nil "waiting for \"~A\" command to finish" whostate)
	    "Keep waiting for tapemaster command to finish."))))

(defun command-acknowledged? ()
  (not (zerop (tpb-command-seen *tpb*))))

(defun wait-for-command-acknowledge ()
  (unless (command-acknowledged?)
    (wait 'command-acknowledged?
	  (* 60. 10.)
	  "[TM] Tape Command Ack"
	  "waiting for tapemaster to acknowledge command"
	  "Keep waiting for tapemaster to acknowledge command.")))

;; When we have atomic bus operations, this should be changed to use them.
(defun sieze-gate ()
  (if *available*
      (without-interrupts
	(wait-for-command-complete)
	(setf (ccb-gate *ccb*) #xff))
    (signal 'tape:driver-error
	    :device-type 'tapemaster
	    :error-code 0
	    :error-message
	    "The tapemaster can not be used because of internal problems.  Call LMI.")))

(defun execute-command (command unit reverse density return-count buf-size records
			&optional (multibus-data-address (data-starting-address)))
  (wait-for-command-complete)
  (sieze-gate)
  ;zero from beginning of array to end of what we use
  (copy-array-portion *tpb* 0 0 *tpb* 0 tpb-size)
  (setf (tpb-command *tpb*) command)
  (setf (tpb-unit *tpb*)
	(case (ldb (byte 2 0) unit) (0 0) (3 3) (2 1) (1 2)))
  (setf (tpb-bank-select *tpb*) (ldb (byte 1 2) unit))
  (setf (tpb-reverse *tpb*) reverse)
  (setf (tpb-density *tpb*) density)
  (setf (tpb-bus-width *tpb*) 1)
  (setf (tpb-return-count *tpb*) return-count)
  (setf (tpb-buffer-size *tpb*) buf-size)
  (setf (tpb-records *tpb*) records)
  (setf (tpb-buffer-pointer-offset *tpb*) (ldb (byte 4 0) multibus-data-address))
  (setf (tpb-buffer-pointer-base *tpb*)	(ldb (byte 16. 4) multibus-data-address))
  (setf (ccb-tpb-pointer-offset *ccb*) (ldb (byte 4 0) *tpb-multibus-address*))
  (setf (ccb-tpb-pointer-base *ccb*) (ldb (byte 16 4) *tpb-multibus-address*))
  (attention)
  (wait-for-command-acknowledge))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setting up the tapemaster interface
;;;

(defun get-parameters-from-sys-conf ()
  (cond ((<= (si:%system-configuration-size si:*sys-conf*)
	     si:%system-configuration-multibus-tapemaster-parameter-block)
	 nil)
	(t
	 (setq *tpb-multibus-address*
	       (si:%system-configuration-multibus-tapemaster-parameter-block si:*sys-conf*))
	 (setq *base-multibus-page-number*
	       (si:%system-configuration-tapemaster-base-multibus-map si:*sys-conf*))
	 (setq *number-of-multibus-pages*
	       (si:%system-configuration-tapemaster-multibus-map-size si:*sys-conf*))
	 (and (not (zerop *tpb-multibus-address*))
	      (not (zerop *base-multibus-page-number*))
	      (not (zerop *number-of-multibus-pages*))))))

(defun setup-control-memory ()
  (cond ((not (= si:processor-type-code si:lambda-type-code)))
	((get-parameters-from-sys-conf)
	 (setq *available* t))
	((sdu-is-version-7-p)
	 (setq *tpb-multibus-address* *sdu-version-7-font-table*)
	 (setq *base-multibus-page-number* (- 448 40))
	 (setq *number-of-multibus-pages* 32)
	 (setq *available* t))
	(t 
	 (if ( (si:%system-configuration-tapemaster-owner si:*sys-conf*)
		#o37777777777)
	     (format *error-output* "Warning: can't use half-inch tape with this SDU"))
	 (setq *available* nil)
	 (setq *scb* nil)
	 (setq *ccb* nil)
	 (setq *tpb* nil)
	 (return-from setup-control-memory nil)))
  (setq *scb-multibus-address* (lsh *base-multibus-page-number* 10.))
  (setq *ccb-multibus-address* (lsh (+ *base-multibus-page-number* 1) 10.))
  (unless *control-memory*
    (setq *control-memory* (allocate-resource 'si:dma-buffer 4)))
  (let ((vadr-of-first-page (si:dma-buffer-data-vadr *control-memory*)))
    (setq *scb* (zl:make-array scb-size
			  :type :art-16b
			  :named-structure-symbol 'scb
			  :leader-length 3
			  :displaced-to (setq *scb-virtual-address* vadr-of-first-page)))
    (setq *ccb* (zl:make-array ccb-size
			  :type :art-16b
			  :named-structure-symbol 'ccb
			  :leader-length 3
			  :displaced-to (setq *ccb-virtual-address*
					      (%pointer-plus vadr-of-first-page si:page-size))))
    (setq *tpb* (zl:make-array tpb-size
			  :type :art-16b
			  :named-structure-symbol 'tpb
			  :leader-length 3
			  :displaced-to (setq *tpb-virtual-address*
					      (%pointer-plus si:multibus-virtual-address
							     (/ *tpb-multibus-address* 4)))))))

(defun determine-tapemaster-owner ()
  (let ((lo (aref si:*sys-conf* (* 2 si:%system-configuration-tapemaster-owner)))
	(hi (aref si:*sys-conf* (1+ (* 2 si:%system-configuration-tapemaster-owner)))))
    (cond ((and (= lo 177777) (= hi 177777))
	   :not-on-bus)
	  ((ldb-test (byte 1 15.) hi)
	   (let ((index (si:get-slot-index lo)))
	     (if (= (si:get-slot-index si:rg-quad-slot) index) :this-processor index)))
	  (t
	   :no-owner))))

(defun tapemaster-on-bus? ()
  (select-processor
    (:lambda (neq (determine-tapemaster-owner) :not-on-bus))
    ((:cadr :explorer :falcon))))

(defun steal-tapemaster-from-bus ()
  (aset (dpb 1 (byte 1 15.) 0) si:*sys-conf*
	(1+ (* 2 si:%system-configuration-tapemaster-owner)))
  (aset (si:get-slot-index si:rg-quad-slot) si:*sys-conf*
	(* 2 si:%system-configuration-tapemaster-owner))
  (setup-control-memory)
  (init))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control
;;;

(defun init ()
  "Initialize the Tapemaster controller."
  (lambda-only)
  (unless *available*
    (ferror 'tape:driver-error
	    :device-type 'tapemaster
	    :error-code 0
	    :error-message
	    "The tapemaster is not available for use.  This could be a software error."))
  (array-initialize *control-memory* 0)
  (tape:wire-dma-buffer *control-memory*)
  (map-dma-buffer *control-memory* *base-multibus-page-number* *number-of-control-pages*)
  (reset)
  (array-initialize *scp* 0)
  (setf (scp-bus-width *scp*) 1)		;16 bit bus
  ;point the system-configruation-pointer to the system-configuration-block
  (setf (scp-scb-pointer-offset *scp*) (ldb (byte 4 0) *scb-multibus-address*))
  (setf (scp-scb-pointer-base *scp*) (ldb (byte 16 4) *scb-multibus-address*))
  ;point the system-configuration-block to the channel-control-block
  (setf (scb-must-be-3 *scb*) 3)
  (setf (scb-ccb-pointer-offset *scb*) (ldb (byte 4 0) *ccb-multibus-address*))
  (setf (scb-ccb-pointer-base *scb*) (ldb (byte 16 4) *ccb-multibus-address*))
  ;set up the channel control block
  (setf (ccb-channel-control-word *ccb*) #x11) ;"normal operation"
  (setf (ccb-gate *ccb*) #xff)
  ;make it read all these structures
  (attention)
  (wait-for-command-complete)
  (execute-command %configure 0 0 0 0 0 0)
  (wait-for-command-complete)
  (check-for-error :error-trace 'init))

(defun controller-status (&optional (unit 0) &aux return-list)
  (execute-command %drive-status unit 0 0 0 0 0)
  (wait-for-command-complete)
  (check-for-error :error-trace '(controller-status %drive-status))
  (cond-every ((not (zerop (tpb-write-protect *tpb*)))
	       (push :write-protected return-list))
	      ((not (zerop (tpb-formatter-busy *tpb*)))
	       (push :busy return-list))
	      (t
	       (if (zerop (tpb-ready *tpb*))
		   (push :off-line return-list)
		 (push :on-line return-list)))
	      ((not (zerop (tpb-eot *tpb*)))
	       (push :physical-end-of-tape return-list))
	      ((not (zerop (tpb-load-point *tpb*)))
	       (push :load-point return-list))
	      ((not (zerop (tpb-file-mark *tpb*)))
	       (push :end-of-file return-list))
	      ((not (zerop (tpb-error *tpb*)))
	       (condition-case (condition)
		   (check-for-error :error-trace '(controller-status tape-error))
		 (tape-error
		  (push (cons :error condition) return-list)))))
  return-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tape positioning
;;;

(defun space (unit reverse number-of-blocks density)
  (with-reset-on-abort
    (execute-command %space unit reverse density 0 0 number-of-blocks)
    (wait-for-command-complete)
    (condition-case (condition)
	(check-for-error :error-trace 'space)
      ((eot bot)
       (signal (typecase condition
		 (eot 'tape:physical-end-of-tape)
		 (bot 'tape:physical-beginning-of-tape))
	       :device-type 'tapemaster
	       :unit unit
	       :data-transferred (add1 (- number-of-blocks (tpb-records *tpb*))))))
    number-of-blocks))

(defun space-filemark (unit reverse number-of-blocks density)
  (with-reset-on-abort
    (execute-command %space-filemark unit reverse density 0 0 number-of-blocks)
    (wait-for-command-complete)
    (check-for-error :error-trace 'space-filemark)
    (- number-of-blocks (tpb-records *tpb*) 1)))

(defun search-filemark (unit reverse density)
  (with-reset-on-abort
    (execute-command %search-filemark unit reverse density 0 0 0)
    (wait-for-command-complete)
    (condition-case (condition)
	(check-for-error :error-trace 'search-filemark)
      ((eot bot)
       (signal (typecase condition
		 (bot 'tape:physical-beginning-of-tape)
		 (eot 'tape:physical-end-of-tape))
	       :device-type 'tapemaster
	       :unit unit
	       :data-transferred 0))) 
    (unless (zerop reverse)
      (space unit 0 1 0)
      (wait-for-command-complete)
      (check-for-error :error-trace '(search-filemark reverse)))
    t))

(defun search-multiple-filemarks (number-of-filemarks unit reverse density)
  (with-reset-on-abort
    (execute-command %search-multiple-filemarks unit reverse density 0 0 number-of-filemarks)
    (wait-for-command-complete)
    (condition-case (condition)
	(check-for-error :error-trace 'search-multiple-filemarks)
      ((eot bot)
       (signal (typecase condition
		 (bot 'tape:physical-beginning-of-tape)
		 (eot 'tape:physical-end-of-tape))
	       :device-type 'tapemaster
	       :unit unit
	       :data-transferred 0))) 
    (unless (zerop reverse)
      (space unit 0 1 0)
      (wait-for-command-complete)
      (check-for-error :error-trace '(search-multiple-filemarks reverse)))
    t))

(defun rewind (unit wait?)
  (execute-command %rewind unit 0 0 0 0 0)
  (when wait?
    (wait-for-command-complete)
    (check-for-error :error-trace 'rewind)))

(defun unload (unit seconds-to-wait)
  (execute-command %offline-and-unload unit 0 0 0 0 0)
  (when seconds-to-wait
    (wait-for-command-complete)
    (check-for-error :error-trace 'unload))
  t)

(deff offline 'unload)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Random

(defun write-filemark (unit)
  (execute-command %write-filemark unit 0 0 0 0 0)
  (wait-for-command-complete)
  (condition-case (condition)
      (check-for-error :error-trace 'write-filemark)
    (eot (signal 'tape:physical-end-of-tape
		 :device-type 'tapemaster
		 :unit unit
		 :data-transferred 0))))

(defun erase-fixed-length (unit number-of-blocks)
  (execute-command %erase-fixed-length unit 0 0 0 0 number-of-blocks)
  (wait-for-command-complete)
  (condition-case (condition)
      (check-for-error :error-trace 'erase-fixed-length)
    (eot (signal 'tape:physical-end-of-tape
		 :device-type 'tapemaster
		 :unit unit
		 :data-transferred 0))))

(defun erase-tape (unit)
  (execute-command %erase-tape unit 0 1 0 0 0)
  (wait-for-command-complete)
  (check-for-error :error-trace 'erase-tape)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for read/write operations

(defun disk-process-buffer (op buffer unit address number-of-pages record-size)
  (using-resource (rqb si:rqb number-of-pages 4)
    (unwind-protect
	(let ((rqb-vadr (dpb 0 (byte 8 0) (%pointer-plus (%pointer rqb) #o377)))
	      (end-of-data (* number-of-pages si:page-size)))
	  (si:wire-disk-rqb rqb)
	  (case op
	    (:read (si:disk-read rqb unit address)
		   (%blt rqb-vadr (si:dma-buffer-data-vadr buffer) end-of-data 1)
		   (when (< end-of-data (array-length buffer))
		     (array-initialize
		       (si:dma-buffer-16b buffer)
		       0
		       (* end-of-data 2)
		       (* (min (+ end-of-data (/ record-size 4)) (array-length buffer))
			  2))))
	    (:write (%blt (si:dma-buffer-data-vadr buffer) rqb-vadr end-of-data 1)
		    (si:disk-write rqb unit address))))
      (when rqb (si:unwire-disk-rqb rqb)))
    (values buffer number-of-pages)))

(defsubst disk-read-buffer (buffer unit address number-of-pages record-size)
  (disk-process-buffer :read buffer unit address number-of-pages record-size))

(defsubst disk-write-buffer (buffer unit address number-of-pages record-size)
  (disk-process-buffer :write buffer unit address number-of-pages record-size))

(defun calculate-buffer-sizes-for-disk (number-of-blocks record-size number-of-available-pages)
  (check-arg record-size (zerop (remainder record-size (* si:page-size 4)))
	     "a page-even record-size")
  (let* ((record-size-in-pages (/ record-size (* si:page-size 4)))
	 (total-number-of-records (ceiling number-of-blocks record-size-in-pages))
	 (buffer-size-in-records (floor number-of-available-pages record-size-in-pages))
	 (disk-buffer-size-in-pages (* buffer-size-in-records record-size-in-pages))
	 (tape-buffer-size-in-pages
	   (ceiling (* buffer-size-in-records (+ record-size 8)) (* si:page-size 4)))
	 (number-of-buffers (ceiling number-of-blocks disk-buffer-size-in-pages))
	 (last-buffer-size-in-records
	   (let ((rem (remainder total-number-of-records buffer-size-in-records)))
	     (if (zerop rem) buffer-size-in-records rem)))
	 (last-disk-buffer-size-in-pages
	   (let ((rem (remainder number-of-blocks disk-buffer-size-in-pages)))
	     (if (zerop rem) disk-buffer-size-in-pages rem)))
	 (last-tape-buffer-size-in-pages
	   (ceiling (* last-buffer-size-in-records (+ record-size 8)) (* si:page-size 4))))
    (values total-number-of-records
	    number-of-buffers
	    buffer-size-in-records
	    disk-buffer-size-in-pages
	    tape-buffer-size-in-pages
	    last-buffer-size-in-records
	    last-tape-buffer-size-in-pages
	    last-disk-buffer-size-in-pages)))

(defun calculate-buffer-sizes-for-array (number-of-records record-size number-of-available-pages)
  (check-arg record-size (zerop (remainder record-size (* si:page-size 4)))
	     "a page-even record-size")
  (when (> (+ record-size 8) (* number-of-available-pages 1024.))
    (ferror nil "Record size too big for Multibus DMA buffer space allocated.
See the documentation for the CONFIG program to change this."))
  (let* ((record-size-in-pages (/ record-size (* si:page-size 4)))
	 (buffer-size-in-records (floor number-of-available-pages record-size-in-pages))
	 (tape-buffer-size-in-pages
	   (ceiling (* buffer-size-in-records (+ record-size 8)) (* si:page-size 4)))
	 (number-of-buffers (ceiling number-of-records buffer-size-in-records))
	 (last-buffer-size-in-records
	   (let ((rem (remainder number-of-records buffer-size-in-records)))
	     (if (zerop rem) buffer-size-in-records rem)))
	 (last-tape-buffer-size-in-pages
	   (ceiling (* last-buffer-size-in-records (+ record-size 8)) (* si:page-size 4))))
    (values number-of-buffers
	    buffer-size-in-records
	    tape-buffer-size-in-pages
	    last-buffer-size-in-records
	    last-tape-buffer-size-in-pages)))

(defun check-array-for-streaming (array record-size)
  (check-type array :array)
  (check-arg array (memq (array-type array) '(art-32b art-16b art-8b art-string))
	     "an acceptable array for the TAPEMASTER Driver to handle")
  (when (and (eq (array-type array) 'art-32b)
	     (neq (named-structure-p array) 'si:dma-buffer))
    (signal 'tape:protocol-violation
	    :format-string "ART-32b arrays which are SI:DMA-BUFFER arrays are supported for~
                            array transfer operations"))
  (unless (zerop (remainder record-size 1024))
    (signal 'tape:protocol-violation
	    :format-string "Array transfer operations require a record size divisible by 1024")))

(defun random-numeric-array-info (array)
  (let ((factor (cdr (assq (array-type array) array-elements-per-q))))
    (values factor (floor (array-length array) factor))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Direct Read/Write
;;;

(defvar *interface-timeout-notify* nil)
(defvar *interface-timeout-count* 0)
(defvar *timeout-retry-limit* 50)

(defun direct-read (dma-buffer record-size unit density
		    &optional function &rest args)
  (let ((max-pages (ceiling record-size 1024)))
    (tape:with-buffer-wired (dma-buffer max-pages)
      (with-buffer-mapped (dma-buffer (data-first-page) max-pages)
	(do ((count 0 (add1 count))
	     finished?)
	    (finished? (tpb-records *tpb*))
	  (execute-command %direct-read
			   unit
			   0
			   density
			   0
			   record-size
			   0)
	  (when function 
	    (lexpr-funcall function args))
	  (wait-for-command-complete)
	  (condition-case (condition)
	      (check-for-error :error-trace 'direct-read)
	    (hard-or-soft-error
	     (signal 'tape:bad-tape
		     :device-type 'tapemaster
		     :unit (get-stupid-unit *tpb*)
		     :proceed-types nil))
	    (interface-timeout
	     (incf *interface-timeout-count*)
	     (when *interface-timeout-notify*
	       (format *error-output* "~&Tapemaster: got an interface timeout"))
	     (when (= count *timeout-retry-limit*)
	       (ferror 'tape:hardware-error
		       :device-type 'tapemaster
		       :error-code (send condition :error-code)
		       :error-message
		       (format nil
			       "Tapemaster timed out ~D times in a row during DIRECT-READ."
			       count)))
	     (space unit 1 1 0))
	    ((eot filemark-encountered)
	     (signal (typecase condition
		       (eot 'tape:physical-end-of-tape)
		       (filemark-encountered 'tape:filemark-encountered))
		     :device-type 'tapemaster
		     :unit unit
		     :data-transferred 0))
	    (blank-tape
	     (signal 'tape:blank-tape :device-type 'tapemaster :unit 0))
	    (:no-error (setq finished? t))))))))

(defun direct-write (dma-buffer record-size unit density
		     &optional function &rest args)
  (let ((max-pages (ceiling record-size 1024)))
    (tape:with-buffer-wired (dma-buffer max-pages)
      (with-buffer-mapped (dma-buffer (data-first-page) (ceiling record-size (* si:page-size 4)))
	(do ((count 0 (add1 count))
	     finished?)
	    (finished? (tpb-records *tpb*))
	  (execute-command %direct-write
			   unit
			   0
			   density
			   0
			   record-size
			   0)
	  (when function
	    (lexpr-funcall function args))
	  (wait-for-command-complete)
	  (condition-case (condition)
	      (check-for-error :error-trace 'direct-write)
	    (hard-or-soft-error
	     (zl:if (not *skip-bad-blocks*)
		 (signal 'tape:bad-tape
			 :device-type 'tapemaster
			 :unit (get-stupid-unit *tpb*)
			 :proceed-types nil)
	       (space unit 1 1 0)
	       (erase-fixed-length unit 1)
	       (format t "~&*** Bad tape block on TAPEMASTER unit ~D skipped ***~%" unit)))
	    (interface-timeout
	     (incf *interface-timeout-count*)
	     (when *interface-timeout-notify*
	       (format *error-output* "~&Tapemaster: got an interface timeout"))
	     (when (= count *timeout-retry-limit*)
	       (signal 'tape:hardware-error
		       :device-type 'tapemaster
		       :error-code (send condition :error-code)
		       :error-message
		       (format nil 
			       "Tapemaster timed out ~D times in a row during :DIRECT-WRITE."
			       count)))
	     (space unit 1 1 0))
	    (eot (signal 'tape:physical-end-of-tape
			 :device-type 'tapemaster
			 :unit unit
			 :data-transferred 0))
	    (:no-error (setq finished? t))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Streaming read/write
;;;

(defun %streaming-write (buffer number-of-pages unit density &optional function &rest args)
  (map-dma-buffer buffer (data-first-page) number-of-pages)
  (do ((8b (si:dma-buffer-8b buffer))
       (count 0 (add1 count))
       (maddr (data-starting-address))
       finished?)
      (finished?)
    (execute-command %streaming-write unit 0 density 0 0 0 maddr)
    (when function (lexpr-funcall function args))
    (wait-for-command-complete)
    (condition-case (condition)
	(check-for-error :error-trace '%streaming-write)
      (hard-or-soft-error
       (zl:if (not *skip-bad-blocks*)
	   (signal 'tape:bad-tape
		   :device-type 'tapemaster
		   :unit (get-stupid-unit *tpb*)
		   :proceed-types nil)
	 (space unit 1 1 0)
	 (erase-fixed-length unit 1)
	 (let* ((error-idx (find-error-in-streaming-buffer
			     buffer (- maddr (data-starting-address)))))
	   (aset (dpb (ldb (byte 1 4) (aref 8b error-idx)) (byte 1 4) 1)
		 8b
		 error-idx)
	   (setq maddr (+ (data-starting-address) error-idx)))
	 (format t "~&*** Bad tape block on TAPEMASTER unit ~D skipped ***~%" unit)))
      (interface-timeout
       (let* ((error-idx (find-error-in-streaming-buffer
			   buffer (- maddr (data-starting-address))))
	      (block-complete? (ldb-test (byte 1 2) (aref 8b error-idx))))
	 (zl:if (/= (- maddr (data-starting-address)) error-idx)
	     (incf *interface-timeout-count*
		   (setq count 1))		;First error for this record
	   (incf *interface-timeout-count*)
	   (when (= count *timeout-retry-limit*)
	     (signal 'tape:hardware-error
		     :device-type 'tapemaster
		     :error-code (send condition :error-code)
		     :error-message
		     (format nil 
			     "Tapemaster timed out ~D times in a row during %STREAMING-WRITE."
			     count))))
	 (when *interface-timeout-notify*
	   (format *error-output* "~&Tapemaster: got an interface timeout"))
	 (unless block-complete?
	   (ferror 'tape:driver-error
		   :device-type 'tapemaster
		   :error-code :none
		   :error-message "Tape complete flag was not set after streaming buffer error."))
	 (space unit 1 1 0)
	 ;;; make buffer ready again for retry (careful not to smash last-block bit [4])
	 (aset (dpb (ldb (byte 1 4) (aref 8b error-idx)) (byte 1 4) 1)
	       8b
	       error-idx)
	 (setq maddr (+ (data-starting-address) error-idx))))
      (time-out-error			;rare (perhaps only encountered with Kennedy drives)
       (let* ((error-idx (find-error-in-streaming-buffer
			   buffer (- maddr (data-starting-address))))
	      (block-complete? (ldb-test (byte 1 2) (aref 8b error-idx))))
	 (unless block-complete?
	   (ferror 'tape:driver-error
		   :device-type 'tapemaster
		   :error-code :none
		   :error-message "Tape complete flag was not set after streaming buffer error."))
	 (space unit 1 1 0)
	 ;;; make buffer ready again for retry (careful not to smash last-block bit [4])
	 (aset (dpb (ldb (byte 1 4) (aref 8b error-idx)) (byte 1 4) 1)
	       8b
	       error-idx)
	 (setq maddr (+ (data-starting-address) error-idx))))
      (:no-error (setq finished? t)))))

(defun %streaming-read (buffer number-of-pages unit density &optional function &rest args)
  (map-dma-buffer buffer (data-first-page) number-of-pages)
  (do ((8b (si:dma-buffer-8b buffer))
       (count 0 (add1 count))
       (maddr (data-starting-address))
       finished?)
      (finished?)
    (execute-command %streaming-read unit 0 density 0 0 0 maddr)
    (when function (lexpr-funcall function args))
    (wait-for-command-complete)
    (condition-case (condition)
	(check-for-error :error-trace '%streaming-read)
      (hard-or-soft-error
       (signal 'tape:bad-tape
	       :device-type 'tapemaster
	       :unit (get-stupid-unit *tpb*)
	       :proceed-types nil))
      (blank-tape
       (format *error-output* "~&Tapemaster: Tape might be blank.  Checking further...")
       (let* ((error-idx (find-error-in-streaming-buffer
			   buffer (- maddr (data-starting-address))))
	      (block-complete? (ldb-test (byte 1 2) (aref 8b error-idx))))
	 (unless block-complete?
	   (ferror 'tape:driver-error
		   :device-type 'tapemaster
		   :error-code :none
		   :error-message "Tape complete flag was not set after blank tape error."))
	 ;;;make buffer ready again for retry (careful not to smash last-block bit [4])
	 (aset (dpb (ldb (byte 1 4) (aref 8b error-idx)) (byte 1 4) 1)
	       8b
	       error-idx)
	 (setq maddr (+ (data-starting-address) error-idx))))
      (interface-timeout
       (let* ((error-idx (find-error-in-streaming-buffer
			   buffer (- maddr (data-starting-address))))
	      (block-complete? (ldb-test (byte 1 2) (aref 8b error-idx))))
	 (zl:if (/= (- maddr (data-starting-address)) error-idx)
	     (incf *interface-timeout-count*
		   (setq count 1))			;first error for this record
	   (incf *interface-timeout-count*)
	   (when (= count *timeout-retry-limit*)
	     (signal 'tape:hardware-error
		     :device-type 'tapemaster
		     :error-code (send condition :error-code)
		     :error-message
		     (format nil 
			     "Tapemaster timed out ~D times in a row during %streaming-read."
			     count))))
	 (when *interface-timeout-notify*
	   (format *error-output* "~&Tapemaster: got an interface timeout"))
	 (unless block-complete?
	   (ferror 'tape:driver-error
		   :device-type 'tapemaster
		   :error-code :none
		   :error-message "Tape complete flag was not set after streaming buffer error."))
	 (space unit 1 1 0)
	 ;;;make buffer ready again for retry (careful not to smash last-block bit [4])
	 (aset (dpb (ldb (byte 1 4) (aref 8b error-idx)) (byte 1 4) 1)
	       8b
	       error-idx)
	 (setq maddr (+ (data-starting-address) error-idx))))
      (time-out-error
       (let* ((error-idx (find-error-in-streaming-buffer buffer 0))
	      (block-complete? (ldb-test (byte 1 2) (aref 8b error-idx))))
	 (unless block-complete?
	   (ferror 'tape:driver-error
		   :device-type 'tapemaster
		   :error-code :none
		   :error-message "Tape complete flag was not set after streaming buffer error."))
	 (space unit 1 1 0)
	 ;;; make buffer ready again for retry (careful not to smash last-block bit [4])
	 (aset (dpb (ldb (byte 1 4) (aref 8b error-idx)) (byte 1 4) 1)
	       8b
	       error-idx)
	 (setq maddr (+ (data-starting-address) error-idx))))
      (:no-error (setq finished? t)))))

(defun copy-streaming-buffer-to-array (buffer
				       to-array
				       array-offset
				       last-record-active-size
				       &optional ignore-errors
				       )
  (multiple-value-bind (ignore array-data-length-in-words)
      (random-numeric-array-info to-array)
    (let* ((buffer-16 (si:dma-buffer-16b buffer))
	   (array-data-vadr (array-data-vadr to-array))
	   (array-last-word-vadr (sub1 (+ array-data-vadr array-data-length-in-words))))
      (do* ((buffer-index 0)
	    (array-index array-offset)
	    record-size-in-words
	    data-count-in-words
	    last-block?
	    error?)
	   (last-block? (values to-array (+ array-index data-count-in-words)))
	(setq error? (ldb-test (byte 1 3) (aref buffer-16 (* buffer-index 2)))
	      last-block? (ldb-test (byte 1 4) (aref buffer-16 (* buffer-index 2)))
	      record-size-in-words (/ (aref buffer-16 (add1 (* buffer-index 2))) 4)
	      data-count-in-words (if last-block? last-record-active-size record-size-in-words))
	(cond ((and error? (not ignore-errors))
	       (return-from copy-streaming-buffer-to-array
		 (values to-array
			 (+ array-index data-count-in-words))))
	      ((> (sub1 (+ array-data-vadr array-index data-count-in-words)) array-last-word-vadr)
	       (signal 'tape:protocol-violation
		       :format-string "Destination array for copy is too small."))
	      (t
	       (%blt (+ (si:dma-buffer-data-vadr buffer) buffer-index 2)
		     (+ array-data-vadr array-index)
		     data-count-in-words
		     1)))
	(unless last-block?
	  (incf buffer-index (+ record-size-in-words 2))
	  (incf array-index data-count-in-words))))))
	  
(defun prepare-buffer-for-streaming-write (buffer from record-size number-of-records
					   &optional (word-offset 0))
  (check-array-for-streaming from record-size)
  (multiple-value-bind (ignore array-length-in-words)
      (random-numeric-array-info from)
    (cond ((> (+ (/ (* number-of-records record-size) 4) word-offset) array-length-in-words)
	   (signal 'tape:protocol-violation
		   :format-string "Source array too small for ~d (~D byte) records"
		   :format-args (list number-of-records record-size)))
	  ((> (* number-of-records (+ record-size 8))
	      (array-length (si:dma-buffer-string buffer)))
	   (signal 'tape:protocol-violation
		   :format-string "~D (~D byte) blocks will not fit in buffer"
		   :format-args (list number-of-records record-size))))
    (do* ((count 1 (add1 count))
	  (rsiw (/ record-size 4))
	  (from-vadr (+ (array-data-vadr from) word-offset) (%pointer-plus from-vadr rsiw))
	  (buffer-16 (si:dma-buffer-16b buffer))
	  (buffer-index 0 (+ buffer-index rsiw 2))
	  (buffer-block-vadr (+ (si:dma-buffer-data-vadr buffer) 2)
			     (+ buffer-block-vadr rsiw 2))
	  (madr (+ (data-starting-address) record-size 8) (+ madr record-size 8)))
	 ((> count number-of-records) buffer)
      ; Setup gate
      (aset (dpb (if (= count number-of-records) #2r10001 1) (byte 8 0) 0)
	    buffer-16
	    (* buffer-index 2))
      ; Setup byte count
      (aset record-size buffer-16 (+ (* buffer-index 2) 1))
      ; Setup next block pointer offset
      (aset (ldb (byte 4 0) madr) buffer-16 (+ (* buffer-index 2) 2))
      ; Setup next block pointer base
      (aset (ldb (byte 16 4) madr) buffer-16 (+ (* buffer-index 2) 3))
      (%blt from-vadr buffer-block-vadr rsiw 1))))

(defun prepare-buffer-for-streaming-read (buffer record-size number-of-records)
  (do ((buffer-16 (si:dma-buffer-16b buffer))
       (count 1 (add1 count))
       (buffer-index 0 (+ buffer-index (/ record-size 2) 4))
       (madr (+ (data-starting-address) record-size 8) (+ madr record-size 8)))
      ((> count number-of-records) buffer)
    ; Setup gate
    (setf (aref buffer-16 buffer-index)
	  (dpb (if (= count number-of-records) #2r10001 1) (byte 8 0) 0))
    ; Setup byte count
    (setf (aref buffer-16 (+ buffer-index 1)) record-size)
    ; Setup next block pointer offset
    (setf (aref buffer-16 (+ buffer-index 2)) (ldb (byte 4 0) madr))
    ; Setup next block pointer base
    (setf (aref buffer-16 (+ buffer-index 3)) (ldb (byte 16 4) madr))))

(defun streaming-write-array (from-array record-size number-of-records unit density)
  (multiple-value-bind (number-of-buffers
			buffer-size-in-records
			tape-buffer-size-in-pages
			last-buffer-size-in-records
			last-tape-buffer-size-in-pages)
      (calculate-buffer-sizes-for-array
	number-of-records record-size (number-of-data-pages))
    (using-resource (tape-buffer si:dma-buffer tape-buffer-size-in-pages)
      (tape:with-buffer-wired (tape-buffer tape-buffer-size-in-pages)
	(unwind-mapping-reset
	  (do* ((records-written 0)
		(rsiw (/ record-size 4))
		(count 1 (add1 count))
		(array-word-offset 0 (+ array-word-offset (* buffer-size-in-records rsiw))))
	       ((= count number-of-buffers)
		(prepare-buffer-for-streaming-write
		  tape-buffer from-array record-size
		  last-buffer-size-in-records array-word-offset)
		(condition-case (condition)
		    (%streaming-write
		     tape-buffer last-tape-buffer-size-in-pages unit density)
		  (eot (signal 'tape:physical-end-of-tape
			       :device-type 'tapemaster-device
			       :unit unit
			       :data-transferred
			       (+ records-written
				  (/ (find-error-in-streaming-buffer tape-buffer)
				     (+ record-size 8)))))
		  (:no-error (incf records-written last-buffer-size-in-records))))
	    (prepare-buffer-for-streaming-write
	      tape-buffer from-array record-size buffer-size-in-records array-word-offset)
	    (condition-case (condition)
		(%streaming-write tape-buffer tape-buffer-size-in-pages unit density)
	      (eot (signal 'tape:physical-end-of-tape
			   :device-type 'tapemaster-device
			   :unit unit
			   :data-transferred
			   (+ records-written
			      (/ (find-error-in-streaming-buffer tape-buffer)
				 (+ record-size 8)))))
	      (:no-error (incf records-written buffer-size-in-records)))))))))

(defun streaming-write-from-disk
       (disk-unit first-block length-in-blocks record-size tape-unit density silent)
  (multiple-value-bind (total-number-of-records
			number-of-buffers
			buffer-size-in-records
			disk-buffer-size-in-pages
			tape-buffer-size-in-pages
			last-buffer-size-in-records
			last-tape-buffer-size-in-pages
			last-disk-buffer-size-in-pages)
      (calculate-buffer-sizes-for-disk
	length-in-blocks record-size (min 120 (number-of-data-pages)))
    total-number-of-records
    (unless silent (format *standard-output* "~&Copying will take ~d chunks: " number-of-buffers))
    (unwind-mapping-reset
      (using-resource (tape-buffer si:dma-buffer tape-buffer-size-in-pages)
	(tape:with-buffer-wired (tape-buffer tape-buffer-size-in-pages)
	  (using-resource (disk-buffer si:dma-buffer disk-buffer-size-in-pages)
	    (tape:with-buffer-wired (disk-buffer disk-buffer-size-in-pages)
	      (do* ((blocks-written 0)
		    (count 1 (add1 count))
		    (address first-block (+ address disk-buffer-size-in-pages)))
		   ((= count number-of-buffers)
		    (unless silent (format *standard-output* "~D " count))
		    (prepare-buffer-for-streaming-write
		      tape-buffer
		      (disk-read-buffer disk-buffer disk-unit address
					last-disk-buffer-size-in-pages record-size)
		      record-size
		      last-buffer-size-in-records)
		    (condition-case (condition)
			(%streaming-write
			 tape-buffer last-tape-buffer-size-in-pages tape-unit density)
		      (eot
		       (signal 'tape:physical-end-of-tape
			       :device-type 'tapemaster
			       :unit tape-unit
			       :data-transferred
			       (+ blocks-written
				  (* (/ (find-error-in-streaming-buffer tape-buffer)
					(+ record-size 8))
				     (/ record-size si:page-size 4)))))
		      (:no-error (incf blocks-written last-disk-buffer-size-in-pages))))
		(unless silent (format *standard-output* "~D " count))
		(prepare-buffer-for-streaming-write
		  tape-buffer
		  (disk-read-buffer
		    disk-buffer disk-unit address disk-buffer-size-in-pages record-size)
		  record-size
		  buffer-size-in-records)
		(condition-case (condition)
		    (%streaming-write tape-buffer tape-buffer-size-in-pages tape-unit density)
		  (eot
		   (signal 'tape:physical-end-of-tape
			   :device-type 'tapemaster
			   :unit tape-unit
			   :data-transferred (+ blocks-written
						(* (/ (find-error-in-streaming-buffer tape-buffer)
						      (+ record-size 8))
						   (/ record-size si:page-size 4)))))
		  (:no-error (incf blocks-written disk-buffer-size-in-pages)))))))))))

(defun streaming-read-array (into-array record-size number-of-records unit density)
  (check-array-for-streaming into-array record-size)
  (multiple-value-bind (number-of-buffers
			buffer-size-in-records
			tape-buffer-size-in-pages
			last-buffer-size-in-records
			last-tape-buffer-size-in-pages)
      (calculate-buffer-sizes-for-array
	number-of-records record-size (number-of-data-pages))
    (using-resource (tape-buffer si:dma-buffer tape-buffer-size-in-pages)
      (tape:with-buffer-wired (tape-buffer tape-buffer-size-in-pages)
	(unwind-mapping-reset
	  (do* ((records-read 0)
		(rsiw (/ record-size 4))
		(array-word-offset 0 (+ array-word-offset (* buffer-size-in-records rsiw)))
		(count 1 (add1 count)))
	       ((= count number-of-buffers)
		(prepare-buffer-for-streaming-read
		  tape-buffer record-size last-buffer-size-in-records)
		(condition-case (condition)
		    (%streaming-read
		     tape-buffer last-tape-buffer-size-in-pages unit density)
		  ((eot filemark-encountered)
		   (copy-streaming-buffer-to-array
		     tape-buffer into-array array-word-offset rsiw)
		   (signal (typecase condition
			     (eot 'tape:physical-end-of-tape)
			     (filemark-encountered 'tape:filemark-encountered))
			   :device-type 'tapemaster
			   :unit unit
			   :data-transferred (+ records-read
						(/ (find-error-in-streaming-buffer tape-buffer)
						   (+ record-size 8)))))
		  (:no-error
		   (copy-streaming-buffer-to-array
		     tape-buffer into-array array-word-offset rsiw)
		   (+ records-read last-buffer-size-in-records))))
	    (prepare-buffer-for-streaming-read
	      tape-buffer record-size buffer-size-in-records)
	    (condition-case (condition)
		(%streaming-read
		 tape-buffer tape-buffer-size-in-pages unit density)
	      ((eot filemark-encountered)
	       (copy-streaming-buffer-to-array
		 tape-buffer into-array array-word-offset rsiw)
	       (signal (typecase condition
			 (eot 'tape:physical-end-of-tape)
			 (filemark-encountered 'tape:filemark-encountered))
		       :device-type 'tapemaster
		       :unit unit
		       :data-transferred (+ records-read
					    (/ (find-error-in-streaming-buffer tape-buffer)
					       (+ record-size 8)))))
	      (:no-error
	       (copy-streaming-buffer-to-array
		 tape-buffer into-array array-word-offset rsiw)
	       (incf records-read buffer-size-in-records)))))))))

(defun streaming-read-to-disk
       (disk-unit first-block length-in-blocks record-size tape-unit density silent)
  (multiple-value-bind (total-number-of-records
			number-of-buffers
			buffer-size-in-records
			disk-buffer-size-in-pages
			tape-buffer-size-in-pages
			last-buffer-size-in-records
			last-tape-buffer-size-in-pages
			last-disk-buffer-size-in-pages)
      (calculate-buffer-sizes-for-disk
	length-in-blocks record-size (min 120 (number-of-data-pages)))
    total-number-of-records
    (unless silent (format *standard-output* "~&Copying will take ~d chunks: " number-of-buffers))
    (unwind-mapping-reset
      (using-resource (tape-buffer si:dma-buffer tape-buffer-size-in-pages)
	(tape:with-buffer-wired (tape-buffer tape-buffer-size-in-pages)
	  (using-resource (disk-buffer si:dma-buffer disk-buffer-size-in-pages)
	    (tape:with-buffer-wired (disk-buffer disk-buffer-size-in-pages)
	      (do* ((blocks-read 0)
		    (count 1 (add1 count))
		    (address first-block (+ address disk-buffer-size-in-pages)))
		   ((= count number-of-buffers)
		    (unless silent (format *standard-output* "~D " count))
		    (prepare-buffer-for-streaming-read
		      tape-buffer record-size last-buffer-size-in-records)
		    (condition-case (condition)
			(%streaming-read
			 tape-buffer last-tape-buffer-size-in-pages tape-unit density)
		      ((eot filemark-encountered)
		       (let ((pages-read (* (/ (find-error-in-streaming-buffer tape-buffer)
					       (+ record-size 8))
					    (/ record-size si:page-size 4))))
			 (disk-write-buffer
			   (copy-streaming-buffer-to-array tape-buffer disk-buffer 0 record-size)
			   disk-unit
			   address
			   pages-read
			   record-size)
			 (signal (typecase condition
				   (eot 'tape:physical-end-of-tape)
				   (filemark-encountered 'tape:filemark-encountered))
				 :device-type 'tapemaster
				 :unit tape-unit
				 :data-transferred (+ blocks-read
						      pages-read))))
		      (:no-error
		       (disk-write-buffer
			 (copy-streaming-buffer-to-array
			   tape-buffer disk-buffer 0
			   (let ((rem (* (remainder last-disk-buffer-size-in-pages
						    (/ record-size (* si:page-size 4)))
					 si:page-size)))
			     (if (zerop rem) (/ record-size 4) rem)))
			 disk-unit
			 address
			 last-disk-buffer-size-in-pages
			 record-size)
		       (incf blocks-read last-disk-buffer-size-in-pages))))
		(unless silent (format *standard-output* "~D " count))
		(prepare-buffer-for-streaming-read
		  tape-buffer record-size buffer-size-in-records)
		(condition-case (condition)
		    (%streaming-read tape-buffer tape-buffer-size-in-pages tape-unit density)
		  ((eot filemark-encountered)
		   (let ((pages-read (* (/ (find-error-in-streaming-buffer tape-buffer)
					   (+ record-size 8))
					(/ record-size si:page-size 4))))
		     (disk-write-buffer
		       (copy-streaming-buffer-to-array tape-buffer disk-buffer 0 record-size)
		       disk-unit
		       address
		       pages-read
		       record-size)
		     (signal (typecase condition
			       (eot 'tape:physical-end-of-tape)
			       (filemark-encountered 'tape:filemark-encountered))
			     :device-type 'tapemaster
			     :unit tape-unit
			     :data-transferred (+ blocks-read
						  pages-read))))
		  (:no-error
		   (disk-write-buffer
		     (copy-streaming-buffer-to-array tape-buffer disk-buffer 0 (/ record-size 4))
		     disk-unit
		     address
		     disk-buffer-size-in-pages
		     record-size)
		   (incf blocks-read disk-buffer-size-in-pages)))))))))))

(defun compare-streaming-buffer-to-string (dma-buffer string &optional last-block-size)
  (check-type string string)
  (do* ((buffer-string (si:dma-buffer-string dma-buffer))
	(buffer-index 0)
	(string-index 0)
	(gate (global:aref buffer-string buffer-index)
	      (global:aref buffer-string buffer-index))
	(last-block? (ldb-test (byte 1 4) gate)
		     (ldb-test (byte 1 4) gate))
	(error? (ldb-test (byte 1 3) gate)
		(ldb-test (byte 1 3) gate))
	(record-size (dpb (global:aref buffer-string (+ buffer-index 3))
			  (byte 8 8)
			  (global:aref buffer-string (+ buffer-index 1))))
	(last-record-size (and last-block-size (plusp last-block-size)
			       (1+ (mod (1- last-block-size) record-size))))) ;need positive last-record-size
       ((or last-block? error?)
	(or error?
	    (string-equal buffer-string string
			  :start1 (+ buffer-index 8)
			  :end1 (+ buffer-index 8 (or last-record-size record-size))
			  :start2 string-index
			  :end2 (+ string-index (or last-record-size record-size)))))
    (unless (string-equal buffer-string string
			  :start1 (+ buffer-index 8)
			  :end1 (+ buffer-index 8 record-size)
			  :start2 string-index
			  :end2 (+ string-index record-size))
      (return-from compare-streaming-buffer-to-string nil))
    (incf buffer-index (+ record-size 8))
    (incf string-index record-size)))

(defun streaming-compare-to-disk
       (disk-unit first-block length-in-blocks record-size tape-unit density silent)
  (multiple-value-bind (total-number-of-records
			number-of-buffers
			buffer-size-in-records
			disk-buffer-size-in-pages
			tape-buffer-size-in-pages
			last-buffer-size-in-records
			last-tape-buffer-size-in-pages
			last-disk-buffer-size-in-pages)
      (calculate-buffer-sizes-for-disk
	length-in-blocks record-size (min 120 (number-of-data-pages)))
    total-number-of-records
    (unless silent
      (format *standard-output* "~&Comparison will take ~d chunks: " number-of-buffers))
    (unwind-mapping-reset
      (using-resource (tape-buffer si:dma-buffer tape-buffer-size-in-pages)
	(tape:with-buffer-wired (tape-buffer tape-buffer-size-in-pages)
	  (using-resource (disk-buffer si:dma-buffer disk-buffer-size-in-pages)
	    (tape:with-buffer-wired (disk-buffer disk-buffer-size-in-pages)
	      (do* ((count 1 (add1 count))
		    (blocks-compared 0)
		    unequalp
		    (dbstring (si:dma-buffer-string disk-buffer))
		    (address first-block (+ address disk-buffer-size-in-pages)))
		   ((or (= count number-of-buffers) unequalp)
		    (unless unequalp
		      (unless silent (format *standard-output* "~D " count))
		      (prepare-buffer-for-streaming-read
			tape-buffer record-size	last-buffer-size-in-records)
		      (disk-read-buffer
			disk-buffer disk-unit address last-disk-buffer-size-in-pages record-size)
		      (condition-case (condition)
			  (%streaming-read
			   tape-buffer last-tape-buffer-size-in-pages tape-unit density)
			(eot
			 (if (compare-streaming-buffer-to-string tape-buffer dbstring last-disk-buffer-size-in-pages)
			     (signal 'tape:physical-end-of-tape
				     :device-type 'tapemaster
				     :unit tape-unit
				     :data-transferred
				     (+ blocks-compared
					(* (/ (find-error-in-streaming-buffer tape-buffer)
					      (+ record-size 8))
					   (/ record-size si:page-size 4))))
			   (progn
			     (unless silent
			       (format *standard-output* "[*** Partitions not equal ***]"))
			     nil)))
			(:no-error
			 (if (compare-streaming-buffer-to-string tape-buffer dbstring last-disk-buffer-size-in-pages)
			     (progn (unless silent
				      (format *standard-output* "[Partitions Equal]"))
				    t)
			   (progn
			     (unless silent
			       (format *standard-output* "[*** Partitions not Equal ***]"))
			     nil))))))
		(unless silent (format *standard-output* "~D " count))
		(prepare-buffer-for-streaming-read
		  tape-buffer
		  record-size
		  buffer-size-in-records)
		(disk-read-buffer
		  disk-buffer disk-unit address disk-buffer-size-in-pages record-size)
		(condition-case (condition)
		    (%streaming-read tape-buffer tape-buffer-size-in-pages tape-unit density)
		  (eot
		   (if (compare-streaming-buffer-to-string tape-buffer dbstring)
		       (signal 'tape:physical-end-of-tape
			       :device-type 'tapemaster
			       :unit tape-unit
			       :data-transferred
			       (+ blocks-compared
				  (* (/ (find-error-in-streaming-buffer tape-buffer)
					(+ record-size 8))
				     (/ record-size si:page-size 4))))
		     (progn
		       (unless silent
			 (format *standard-output* "[*** Partitions not equal ***]"))
		     (setq unequalp t))))
		  (:no-error
		   (if (compare-streaming-buffer-to-string tape-buffer dbstring)
		       (incf blocks-compared disk-buffer-size-in-pages)
		     (progn
		       (unless silent
			 (format *standard-output* "[*** Partitions not Equal ***]"))
		       (setq unequalp t)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;
;;; 

(defun raw-tpb ()
  (dotimes (c 11)
    (format *standard-output* "~&~D:~5T~D~15T[~2r]" c (aref *tpb* c) (aref *tpb* c))))

(defun determine-gate-placement (buffer record-size)
  (let ((buf (si:dma-buffer-16b buffer))
	(return-list))
    (dotimes (index (sub1 (array-length buf)) (reverse return-list))
      (let ((contents (aref buf index)))
	(when (and (< contents 256)
		   (= (aref buf (add1 index)) record-size))
	  (push index return-list))))))

(defun analyze-buffer (buffer record-size &optional (stream *standard-output*))
  (format stream "~&[Block]~10T[Gate]~20T[Bytes]~30T[Pointer]~40T[MADR]~50T[VADR]")
  (do* ((buf (si:dma-buffer-8b buffer))
	(count 1 (add1 count))
	(index 0 (+ index record-size 8))
	finished?)
       (finished?)
    (format stream "~&~D~10T~2r~20T~D~30T~D~40T~D~50T~D"
	    count
	    (aref buf index)
	    (+ (lsh (aref buf (+ index 3)) 4)
	       (aref buf (+ index 2)))
	    (+ (data-starting-address) (* index 2))
	    (+ (si:dma-buffer-data-vadr buffer) (/ index 2)))
    (setq finished? (ldb-test (byte 1 4) (aref buf index)))))
  
(defun madr-points-to-buffer-index-p (address buffer 8-bit-index)
  (let* (multibus-page
	 multibus-page-offset
	 (index-vadr (+ (si:dma-buffer-data-vadr buffer) (floor 8-bit-index 4)))
	 (index-virtual-offset (* (remainder index-vadr si:page-size) 4))
	 map-page
	 (index-page
	   (vadr-to-nubus-physical-page index-vadr)))
    (multiple-value-setq (multibus-page multibus-page-offset) (floor address 1024.))
    (setq map-page (ldb (byte 22 0) (read-multibus-mapping-register multibus-page)))
    (values (and (= map-page index-page)
		 (= multibus-page-offset index-virtual-offset))
	    map-page index-page multibus-page-offset index-virtual-offset)))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Verification utilities
;;;
;;;

(defun display-blocks (&key (record-size 4096) (unit 0) (density 0))
  (using-resource (buffer si:dma-buffer 4)
    (do-forever
      (direct-read buffer record-size unit density)
      (send tv:selected-window :clear-screen)
      (send tv:selected-window :string-out (si:dma-buffer-string buffer)))))

(defun fill-buffer (buffer &optional (record-size 4096))
  (let ((buf (si:dma-buffer-8b buffer)))
    (dotimes (c (array-length buf))
      (aset (floor c record-size) buf c))))

(defun buffer-equalp (buffer1 buffer2)
  (string-equal (si:dma-buffer-string buffer1) (si:dma-buffer-string buffer2)))

(defun examine-mapping ()
  (dotimes (c *number-of-multibus-pages*)
    (let* ((value (read-multibus-mapping-register (+ c *base-multibus-page-number*)))
	   (enabled? (ldb-test (byte 1 23) value))
	   (address  (ldb (byte 22 0) value)))
    (format *standard-output* "~&Page: ~d - ~2r [~d:~x] Enabled - ~a"
	    (+ c *base-multibus-page-number*)
	    value address address enabled?))))

(defun compare-disk-to-tape (starting-block number-of-blocks record-size
			     &optional (unit 0) (density 0) &aux error-list)
  (using-resource (tape si:dma-buffer (/ record-size 1024))
    (using-resource (disk si:rqb (/ record-size 1024.) 4)
      (unwind-protect
	  (progn (tape:wire-dma-buffer tape)
		 (si:wire-disk-rqb disk)
		 (do ((count 1 (add1 count))
		      (address starting-block (+ address (/ record-size 1024)))
		      error-list)
		     ((> count (/ number-of-blocks (/ record-size 1024))) (reverse error-list))
		   (si:disk-read disk unit address)
		   (direct-read tape record-size unit density)
		   (let ((val (string-compare (si:rqb-8-bit-buffer disk)
					      (si:dma-buffer-string tape))))
		     (zl:if (zerop val)
			 (write-char #\.)
		       (push (cons count address) error-list)
		       (format *error-output* "~& Error in record number ~d; Byte: ~D"
			       count (abs val))))))
	(si:unwire-disk-rqb disk)
	(tape:unwire-dma-buffer tape))))
  (if error-list (reverse error-list) t))

(defun determine-tm-timing-trade-off (&optional (pages-to-start 40))
  (rewind 0 t)
  (do ((number-of-pages pages-to-start (- number-of-pages 4))
       time0
       time1)
      ((zerop number-of-pages))
    (using-resource (buf si:dma-buffer number-of-pages)
      (tape:with-buffer-wired (buf)
	(let ((beg (time:microsecond-time)))
	  (tm:streaming-write-array buf 4096 (floor (array-length (si:dma-buffer-string buf)) 4096) 0 0)
	  (rewind 0 t)
	  (setq time0 (- (time:microsecond-time) beg))
	  (format *standard-output* "~&Low Speed at ~D pages took: ~d" number-of-pages time0))
	(let ((beg (time:microsecond-time)))
	  (tm:streaming-write-array buf 4096 (floor (array-length (si:dma-buffer-string buf)) 4096) 0 1)
	  (rewind 0 t)
	  (setq time1 (- (time:microsecond-time) beg))
	  (format *standard-output* "~&High speed at ~D pages took: ~d" number-of-pages time1))
	(format *standard-output*
		"~&Time difference was ~D (- means low density faster)." (- time0 time1))))))

(defun clone-tape-by-block (from-unit to-unit record-size &optional (number-of-filemarks-for-eot 2))
  (rewind from-unit t)
  (rewind to-unit t)
  (do ((number-of-pages (ceiling record-size (* si:page-size 4)))
       (filemarks-encountered 0))
      ((= filemarks-encountered number-of-filemarks-for-eot))
    (using-resource (buffer si:dma-buffer number-of-pages)
      (tape:with-buffer-wired (buffer number-of-pages)
	(condition-case ()
	    (direct-read buffer record-size from-unit 1)
	  (tape:filemark-encountered
	   (write-filemark to-unit)
	   (incf filemarks-encountered))
	  (:no-error (setq filemarks-encountered 0)))
	(tm:direct-write buffer record-size to-unit 1)))))

(defun clone-tape (from-unit to-unit record-size &key
		   number-of-files
		   (number-of-filemarks-for-eot 2)
		   (speed :low))
  (format t "~&Rewinding unit ~D ..." from-unit)
  (rewind from-unit t)
  (format t "~&Rewinding unit ~D ..." to-unit)
  (rewind to-unit t)
  (let* ((records-per-buffer (floor (* (number-of-data-pages) si:page-size 4) record-size))
	 (number-of-pages (ceiling (* records-per-buffer (+ record-size 8)) (* si:page-size 4)))
	 (density (ecase speed (:high 1) (:low 0)))
	 (filemarks-encountered 1)
	 (records-this-file 0)
	 (total-record-count 0)
	 (file-count 0)
	 (error))
    (using-resource (buffer si:dma-buffer number-of-pages)
      (tape:with-buffer-wired (buffer number-of-pages)
	  (format t "~&Cloning unit ~d to unit ~d ..." from-unit to-unit)
	  (do-forever
	    (prepare-buffer-for-streaming-read buffer record-size records-per-buffer)
	    (setq error (condition-case (condition)
			    (%streaming-read buffer number-of-pages from-unit density)
			  (filemark-encountered condition)
			  (:no-error nil)))
	    (let ((records (prepare-buffer-for-streaming-write-unaltered buffer record-size)))
	      (unless (zerop records)
		(incf records-this-file records)
		(incf total-record-count records)
		(%streaming-write buffer
				  (ceiling (* (+ record-size 8) records) 1024.)
				  to-unit
				  (if (< records 9) 0 1)))
	      (typecase error
		(null (setq filemarks-encountered 0))
		(filemark-encountered
		 (write-filemark to-unit)
		 (if (zerop records)
		     (incf filemarks-encountered)
		   (setq filemarks-encountered 1))
		 (incf file-count)
		 (format t "~&~5TFile ~D: ~D records" file-count records-this-file)
		 (setq records-this-file 0)
		 (when (or (= filemarks-encountered number-of-filemarks-for-eot)
			   (and number-of-files (= file-count number-of-files)))
		   (format t "~&~D files copied; ~d total records.~2%"
			   file-count total-record-count)
		   (when number-of-files
		     (write-filemark to-unit))
		   (format t "~&Rewinding unit ~D ..." from-unit)
		   (rewind from-unit t)
		   (format t "~&Rewinding unit ~D ..." to-unit)
		   (rewind to-unit t)
		   (return-from clone-tape (values file-count total-record-count))))
		(t (signal-condition error)))))))))

(defun compare-tape-to-tape (from-unit to-unit record-size &key
			     number-of-files
			     (number-of-filemarks-for-eot 2)
			     (speed :low))
  (format t "~&Rewinding unit ~D ..." from-unit)
  (rewind from-unit t)
  (format t "~&Rewinding unit ~D ..." to-unit)
  (rewind to-unit t)
  (let* ((records-per-buffer (floor (* (number-of-data-pages) si:page-size 4) record-size))
	 (number-of-pages (ceiling (* records-per-buffer (+ record-size 8)) (* si:page-size 4)))
	 (density (ecase speed (:high 1) (:low 0)))
	 (filemarks-encountered 0)
	 (records-this-file 0)
	 (total-record-count 0)
	 (file-count 0)
	 (error-count 0)
	 error1 error2)
    (using-resource (buffer-1 si:dma-buffer number-of-pages)
      (using-resource (buffer-2 si:dma-buffer number-of-pages)
	(tape:with-buffer-wired (buffer-1 number-of-pages)
	  (tape:with-buffer-wired (buffer-2 number-of-pages)
	    (unwind-mapping-reset
	      (format t "~&Comparing unit ~d to unit ~d ..." from-unit to-unit)
	      (do ((string-1 (si:dma-buffer-string buffer-1))
		   (string-2 (si:dma-buffer-string buffer-2)))
		  (())
		(prepare-buffer-for-streaming-read buffer-1 record-size records-per-buffer)
		(setq error1 (condition-case (condition)
				 (%streaming-read buffer-1 number-of-pages from-unit density)
			       (filemark-encountered condition)
			       (:no-error nil)))
		(let* ((records-1 (prepare-buffer-for-streaming-write-unaltered buffer-1 record-size))
		       (string-length (* records-1 (+ record-size 8)))
		       records-2)
		  (prepare-buffer-for-streaming-read buffer-2 record-size
						     (cond ((zerop records-1) 1)
							   (error1 (add1 records-1))
							   (t records-1)))
		  (setq error2 (condition-case (condition)
				   (%streaming-read buffer-2 number-of-pages to-unit density)
				 (filemark-encountered condition)
				 (:no-error nil)))
		  (setq records-2
			(prepare-buffer-for-streaming-write-unaltered buffer-2 record-size))
		  (unless (or (zerop records-1)
			      (string-equal string-1 string-2 :end1 string-length :end2 string-length))
		      (incf error-count)
		      (ferror nil "~&Things are not equal..."))
		  (incf records-this-file records-1)
		  (incf total-record-count records-1)
		  (cond ((not (or error1 error2)))
			((not (= records-1 records-2))
			 (ferror nil "Severe Error: Filemarks out of phase between tapes"))
			((and error1 error2
			      (typep error1 'filemark-encountered)
			      (typep error2 'filemark-encountered))
			 (format t "~&~5TCompared file ~D: ~D records" file-count records-this-file)
			 (incf file-count)
			 (setq records-this-file 0)
			 (if (zerop records-1)
			     (incf filemarks-encountered)
			   (setq filemarks-encountered 1))
			 (when (or (= filemarks-encountered number-of-filemarks-for-eot)
				   (and number-of-files (= number-of-files file-count)))
			   (format t "~&Comparison finished: ~D files, ~D records, ~D errors.~2%"
				   file-count total-record-count error-count)
			   (format t "~&Rewinding unit ~D ..." from-unit)
			   (rewind from-unit t)
			   (format t "~&Rewinding unit ~D ..." to-unit)
			   (rewind to-unit t)
			   (return-from compare-tape-to-tape
			     (values file-count total-record-count error-count))))
			(error1 (signal-condition error1))
			(error2 (signal-condition error2))))))))))))

(defun prepare-buffer-for-streaming-write-unaltered (buffer record-size)
  (do ((buffer-8 (si:dma-buffer-8b buffer))
       (index 0 (+ index record-size 8))
       (count 1 (add1 count))
       return)
      (return return)
    (setf (aref buffer-8 index)
	  (cond ((ldb-test (byte 1 4) (aref buffer-8 index))
		 (setq return count)
		 #2r10001)
		((ldb-test (byte 1 3) (aref buffer-8 index))
		 (setq return (sub1 count))
		 #2r10001)
		(t 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializations
;;;
;;;

(add-initialization "Setup Tapemaster"
		    '(select-processor
		       (:lambda
			 (when (eq (determine-tapemaster-owner) :this-processor)
			   (setup-control-memory)
			   (init)))
		       ((:cadr :explorer :falcon)))
		    '(:now)
		    'si:tape-warm-initialization-list)



