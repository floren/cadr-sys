;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-

;;; MagTape definitions.  Mostly copied from RG;MT.
;internal functions:
; MT-PRINT-STATUS  prints current status from hardware.
; EXECUTE-MT-RQB actually does it.  This normally done by microcode for DISK-RQBs.
; MT-WAIT-READY, MT-WAIT-UNIT-READY
; UNIBUS-MAP-MT-RQB,  UNIBUS-UNMAP-MT-RQB.
; MT-RUN rqb command &optional minus-byte-count unit density ibm-mode
; MT-RUN-SIMPLE command unit &optional count.  For commands that dont transfer data.
; MT-SPACE, MT-SPACE-TO-EOF, MT-SPACE-REV, MT-SPACE-REV-TO-BOF, MT-SPACE-TO-APPEND
; MT-REWIND, MT-WRITE-EOF, MT-OFFLINE.

; Normal RQBs are now used for magtape, but
; PRINT-MT-RQB, WIRE-MT-RQB, UNWIRE-MT-RQB, UNIBUS-MAP-MT-RQB and UNIBUS-UNMAP-MT-RQB
; must be used with magtape RQBs.

;; This is also in FSDEFS
(REMPROP 'QUOTIENT-CEILING 'SOURCE-FILE-NAME)
(DEFUN QUOTIENT-CEILING (Y X)
  "The smallest integer greater than or equal to Y//X"
  (// (+ Y (1- X)) X))

(DEFVAR FILE-SYSTEM-PACKAGE (PKG-FIND-PACKAGE "fs"))

(DEFCONST %MT-RQ-DONE-FLAG 0)		;0 RQ ENTERED, -1 COMPLETED
	       ;; These are set up by the requester
(DEFCONST %MT-RQ-COMMAND 1)		;MT COMMAND REGISTER -- ``control'' in Tapemaster
(DEFCONST %MT-BYTE-COUNT 2)		;NEGATIVE BYTE COUNT
(DEFCONST %MT-READ 3)			;ADDITIONAL COMMAND BITS AND STATUS
	      ;; These are stored when the operation completes.
	      ;; The order must agree with the order of the UNIBUS addresses.
(DEFCONST %MT-RQ-STATUS 4)		;MT STATUS REG
(DEFCONST %MT-COMMAND-AFTER 5)		;MT COMMAND REGISTER AFTER XFER
(DEFCONST %MT-BYTE-COUNT-AFTER 6)	;MT RECORD COUNT AFTER
(DEFCONST %MT-RQ-MEM-ADDRESS 7)		;LAST UNIBUS REF ADDR
(DEFCONST %MT-DATA-BUFFER 10)
(DEFCONST %MT-READ-AFTER 11)
(DEFCONST %MT-RQ-COMMAND-REALLY 12)       ;bits are not in the opcode itself

(EVAL-WHEN (COMPILE LOAD EVAL)
;This says that the selected drive is ready.
(DEFSUBST MT-STATUS-READY ()
  (ldb-test %%tapemaster-ready (tapemaster-status)))

;This says that the controller is ready.
(DEFSUBST MT-COMMAND-READY ()
  (tapemaster-controller-ready))

;These are bits in the status stored after a request is executed.

;End of file mark reached when trying to read something.
(DEFSUBST MT-STATUS-EOF ()
 (LDB-TEST %%tapemaster-file-mark (AR-1 RQB %MT-RQ-STATUS)))

;End of tape reached.
(DEFSUBST MT-STATUS-EOT ()
 (LDB-TEST %%tapemaster-end-of-tape (AR-1 RQB %MT-RQ-STATUS)))

;At beginning of tape (rewound, or spaced back that far).
(DEFSUBST MT-STATUS-BOT ()
 (LDB-TEST #|0501|# %%tapemaster-load-point (AR-1 RQB %MT-RQ-STATUS)))

;Any sort of error.
(DEFSUBST MT-STATUS-ERROR ()
 (NOT (ZEROP (LOGAND 17400 (AR-1 RQB %MT-RQ-STATUS)))))
)

;These are the OLD ways of accessing these flags.
(DEFCONST %%MT-STATUS-ILL-COM 1701)
(DEFCONST %%MT-STATUS-EOF 1601)
(DEFCONST %%MT-STATUS-ODD-LENGTH 1501)	;Last word filled with 0s
(DEFCONST %%MT-STATUS-PARITY-ERR 1401)	;parity error, LRC error, or postamble error
(DEFCONST %%MT-STATUS-GRANT-LATE 1301)
(DEFCONST %%MT-STATUS-EOT 1201)
(DEFCONST %%MT-STATUS-RLENGTH 1101)	;RECORD LENGTH ERROR
(DEFCONST %%MT-STATUS-BAD-TAPE 1001)
(DEFCONST %%MT-STATUS-NXM 0701)
(DEFCONST %%MT-STATUS-ON-LINE 0601)
(DEFCONST %%MT-STATUS-BOT 0501)
(DEFCONST %%MT-STATUS-7-TRACK 0401)
(DEFCONST %%MT-STATUS-SETTLE 0301)	;after rewinding
(DEFCONST %%MT-STATUS-WRITE-LOCK 0201)
(DEFCONST %%MT-STATUS-REWINDING 0101)
(DEFCONST %%MT-STATUS-READY 0001)
(DEFCONST %MT-STATUS-ERROR 17400)		 ;Mask for bits which are errors normally

;This is the bit to set to tell the controller to execute the command
;already set up in the various registers.
(DEFCONST %MT-COMMAND-GO 1)

;These are fields to set up in the RQB's %MT-RQ-COMMAND word
;to control a transfer or operation to be done.

(EVAL-WHEN (COMPILE LOAD EVAL)
(DEFSUBST MT-COMMAND-UNIT ()
  (LDB %%tapemaster-tape-select (AR-1 RQB %MT-RQ-COMMAND)))

(DEFSUBST MT-COMMAND-DENSITY ()
  (LDB %%tapemaster-density (AR-1 RQB %MT-RQ-COMMAND)))

(comment
(DEFSUBST MT-COMMAND-XBA-BITS () ; XBA means extended-byte address, seems not be used
  (LDB 0402 (AR-1 RQB %MT-RQ-COMMAND)))
)

(DEFSUBST MT-COMMAND-INTERRUPT-ENABLE ()
  (LDB %%tapemaster-interrupts (AR-1 RQB %MT-RQ-COMMAND)))

;This field's value is one of the command codes below.
(DEFSUBST MT-COMMAND ()
  (AR-1 RQB %MT-RQ-COMMAND-REALLY))  ; the bits and the command can't fit on the TM


;Mag tape command codes.
(DEFCONST %MT-COMMAND-OFFLINE tapemaster-offline-and-unload)
(DEFCONST %MT-COMMAND-READ tapemaster-direct-read)
(DEFCONST %MT-COMMAND-WRITE tapemaster-direct-write)
(DEFCONST %MT-COMMAND-WRITE-EOF tapemaster-write-filemark)
(DEFCONST %MT-COMMAND-SPACE-FOR tapemaster-space)
(DEFCONST %MT-COMMAND-SPACE-REV tapemaster-space) ; SET REVERSE BIT in control reg !
(DEFCONST %MT-COMMAND-WRITE-WITH-EXTENDED-GAP %mt-command-write) ; nothing like it 
(DEFCONST %MT-COMMAND-REWIND tapemaster-rewind)

(DEFCONST MT-COMMAND-NAMES
	  '(%MT-COMMAND-OFFLINE %MT-COMMAND-READ %MT-COMMAND-WRITE
	    %MT-COMMAND-WRITE-EOF %MT-COMMAND-SPACE-FOR %MT-COMMAND-SPACE-REV
	    %MT-COMMAND-WRITE-WITH-EXTENDED-GAP %MT-COMMAND-REWIND))

(DEFCONST %%MT-COMMAND 0103)  ; don't need this
(DEFCONST %%MT-COMMAND-XBA-BITS 0402)	;EXTENDED UNIBUS ADR 17,16
(DEFCONST %MT-COMMAND-INTERRUPT-ENABLE 1_6.) ; or this
(DEFCONST %%MT-COMMAND-READY 0701) ; or this
(DEFCONST %%MT-COMMAND-UNIT %%tapemaster-tape-select)
(DEFCONST %MT-COMMAND-POWER-CLEAR 1_12.) ; don't need this
(DEFCONST %%MT-COMMAND-DENSITY %%tapemaster-density)
(DEFCONST %MT-COMMAND-ERROR 10000) ; might have to change stuff that uses this

(comment
;Unibus register addresses.
(DEFCONST MT-UA-STATUS  772520)
(DEFCONST MT-UA-COMMAND 772522)
(DEFCONST MT-UA-BYTEC   772524)
(DEFCONST MT-UA-CMA     772526)
(DEFCONST MT-UA-BFR     772530)
(DEFCONST MT-UA-DRD     772532)
)
) ;end EVAL-WHEN


;Error reporting.

(DEFUN MT-PRINT-STATUS NIL
  (tapemaster-print-iopb error-output))

;;; These will not be needed
(DEFUN MT-DECODE-STATUS (STATUS)
  (select-processor
    (:cadr
      (CADR:CC-PRINT-SET-BITS STATUS
       '(TAPE-UNIT-READY REWIND-STATUS WRITE-LOCK SETTLE-DOWN
	 SEVEN-TRACK BOT DRIVE-ON-LINE NXM
	 BAD-TAPE RECORD-LENGTH-ERROR EOT BUS-GRANT-LATE
	 PARITY-ERROR NOT-USED END-OF-FILE ILLEGAL-COMMAND)))
    (:lambda (tapemaster-print-status status))))

(DEFUN MT-DECODE-COMMAND (COM)
  (select-processor
    (:cadr
      (CADR:CC-PRINT-SET-BITS COM
       '(GO FCN-0 FCN-1 FCN-2
	 XBA16 XBA17  INT-ENABLE CONTROLLER-READY
	 UNIT-SEL0 UNIT-SEL1 UNIT-SEL2 PARITY-EVEN
	 POWER-CLEAR DENSITY0 DENSITY1 ERROR)))
    (:lambda (tapemaster-print-control com))))

(DEFUN PRINT-MT-RQB (RQB)
  (FORMAT T "~%command before: ~A" (ar-1 rqb %mt-rq-command-really))
  (tapemaster-print-control (AR-1 RQB %MT-RQ-COMMAND))
  (FORMAT T "~%Byte count before ~s" (AR-1 RQB %MT-BYTE-COUNT))
; (FORMAT T "~%command after:")
; (MT-DECODE-COMMAND (AR-1 RQB %MT-COMMAND-AFTER))
  (FORMAT T "~%Status After:")
  (tapemaster-print-status (AR-1 RQB %MT-RQ-STATUS))
  (FORMAT T "~%mem addr after ~S, byte-count-after ~S"
	  (AR-1 RQB %MT-RQ-MEM-ADDRESS) (AR-1 RQB %MT-BYTE-COUNT-AFTER))
  RQB)

;Low-level command execution.
;Initialize the various header words of an RQB, such as %MT-RQ-COMMAND,
;then call EXECUTE-MT-RQB.

(defun sixteen-bit-minus (x) (logand 177777 (1+ (lognot (logand 177777 x)))))

;; This must be used rather than SI:WIRE-DISK-RQB so not to hack the CCW list.
(DEFUN WIRE-MT-RQB (RQB &OPTIONAL (WIRE-P T) SET-MODIFIED
		    &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
			 (LOW (- (%POINTER RQB) (ARRAY-DIMENSION-N 0 RQB) 2))
			 (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
				  (// (ARRAY-LENGTH RQB) 2))))
  (DO LOC (LOGAND LOW (- PAGE-SIZE)) (+ LOC PAGE-SIZE) (>= LOC HIGH)
    (SI:WIRE-PAGE LOC WIRE-P SET-MODIFIED)))

(DEFUN UNWIRE-MT-RQB (RQB)
  (WIRE-MT-RQB RQB NIL))

;;; This doesn't get called by mt-run-simple when you're running the Tapemaster
(DEFUN EXECUTE-MT-RQB (RQB &OPTIONAL SET-MODIFIED &AUX MB-ADR)
  (WIRE-MT-RQB RQB T SET-MODIFIED)
  (SETQ MB-ADR (MULTIBUS-MAP-MT-RQB RQB))  
  (MT-WAIT-READY)
  ; First, wait for the unit
  (tapemaster-execute-command tapemaster-nop (ldb %%tapemaster-tape-select
						  (ar-1 rqb %mt-rq-command))
			      0 0 t)
;  (mt-wait-unit-ready) ; is this really useful, neccessary, or effective ?
  (LET ((BSIZE (MINUS (AR-1 RQB %MT-BYTE-COUNT)))
	(com (ar-1 rqb %mt-rq-command-really)))
    (tapemaster-build-iopb com (ar-1 rqb %mt-rq-command)
			   ;; we can ignore what's at %MT-READ (IBM mode)
			   BSIZE		; already sign-extended
			   MB-ADR)		; not sure if that's the right thing
    ;; This part does read/write until we get DMA working.
    (cond
      ((or (= com tapemaster-buffered-read)
	   (= com tapemaster-direct-read))
       ;; Send the command, wait, and read off the bus
       (tapemaster-execute-command com
				   (dpb 1 %%tapemaster-width (ar-1 rqb %mt-rq-command))
				   bsize
				   MB-ADR
				   t)
       (let ((rc (tapemaster-read-iopb-16 %tapemaster-return-count)))
	 (cond ((and (not (zerop rc))		;if no data xferred, will deal with it later
		     (not (= (tapemaster-read-iopb-16 %tapemaster-records)
			     rc)))
		(ferror nil "~%Record on tape too big, was ~s, allowed ~s"
			(tapemaster-read-iopb-16 %tapemaster-records)
			rc)))))
      ((or (= com tapemaster-buffered-write)
	   (= com tapemaster-direct-write))
       (tapemaster-execute-command com
				   (dpb 1 %%tapemaster-width (ar-1 rqb %mt-rq-command))
				   bsize
				   MB-ADR
				   t))
      (t (tapemaster-channel-attention)))	; Just DO IT
    )
  (tapemaster-wait-controller-ready)
  ;; Update the status
  (setf (ar-1 rqb %mt-rq-status) (tapemaster-read-iopb-16 %tapemaster-status))
  ;; Read the control reg to fake %MT-COMMAND-AFTER
  (setf (ar-1 rqb %mt-command-after) (tapemaster-read-iopb-16 %tapemaster-control))
; (format t "~%#Transferred ~O bytes.#~%" (tapemaster-read-iopb-16 %tapemaster-return-count))
  ;; Update the return count -- it's MINUS, remember
  (setf (ar-1 rqb %mt-byte-count-after)
;	(sixteen-bit-minus
;	  (- (- (ar-1 rqb %mt-byte-count))
;	     (tapemaster-read-iopb-16 %tapemaster-return-count)))
	(tapemaster-read-iopb-16 %tapemaster-return-count))
  (UNWIRE-MT-RQB RQB)
  RQB)

(DEFUN SIGN-EXTEND-16 (X)
  (IF (ZEROP (LOGAND 100000 X)) X (LOGIOR X -100000)))

(DEFVAR PAGE-SIZE-IN-BYTES (* PAGE-SIZE 4.))	;8 bit bytes.

(DEFUN MT-WAIT-READY ()
  "Await controller ready"
  (OR (MT-COMMAND-READY)
      (PROCESS-WAIT "MagTape"
	#'(LAMBDA () (MT-COMMAND-READY))))
  NIL)

(DEFUN MT-WAIT-UNIT-READY ()
  "Await selected unit ready"
  (OR (MT-STATUS-READY)
      (PROCESS-WAIT "MT Unit"
	#'(LAMBDA () (MT-STATUS-READY))))
  NIL)

(defun read-multibus-mapping-register (page-number)
  (let ((ans 0))
    (dotimes (c 3)
      (setq ans (dpb (%multibus-read-8 (+ #16r18000 (* 4 page-number) c))
		     (dpb c 1102 10)
		     ans)))
    ans))

(defun write-multibus-mapping-register (page-number data)
  (dotimes (c 3)
    (%multibus-write-8 (+ #16r18000 (* 4 page-number) c)
		       (ldb (dpb c 1102 10) data)))
  data)

(DEFUN MULTIBUS-MAP-MT-RQB (RQB &OPTIONAL (FIRST-MMP 540)
		    &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
			 (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
				  (// (ARRAY-LENGTH RQB) 2))))
  (DO ((VADR (+ (%POINTER RQB) PAGE-SIZE)
	     (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
       (MMP FIRST-MMP (1+ MMP))
       (NP 0 (1+ NP)))
      ((>= VADR HIGH))
    (COND ((> NP 14.)
	   (FERROR NIL "TOO MANY PAGES")))
    (SETUP-MULTIBUS-MAP MMP VADR))
  (ASH FIRST-MMP 10.))

(DEFUN MULTIBUS-UNMAP-MT-RQB (RQB &OPTIONAL (FIRST-MMP 0)
			   &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
				(LOW (- (%POINTER RQB) (ARRAY-DIMENSION-N 0 RQB) 2))
				(HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
					 (// (ARRAY-LENGTH RQB) 2))))
  (DO ((VADR (+ LOW PAGE-SIZE) (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
       (MMP FIRST-MMP (1+ MMP)))
      ((>= VADR HIGH))
    (WRITE-MULTIBUS-MAPPING-REGISTER MMP 0)))

;;; The MULTIBUS map is 1024. words at HEX 18000.  It consists of 22 address bits, an unused bit,
;;; and valid.  The entire 20. multibus address space is potentially mapped.
(DEFUN SETUP-MULTIBUS-MAP (MULTIBUS-MAP-PAGE VIRTUAL-ADR)
  (WRITE-MULTIBUS-MAPPING-REGISTER MULTIBUS-MAP-PAGE
				   (+ 40000000
				      (COMPILER:%NUBUS-PHYSICAL-ADDRESS
					(LDB 1016 (%PHYSICAL-ADDRESS VIRTUAL-ADR))))))

#|
(DEFUN UNIBUS-MAP-MT-RQB (RQB &OPTIONAL (FIRST-UMP 0)
		    &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
			 (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
				  (// (ARRAY-LENGTH RQB) 2))))
  (DO ((VADR (+ (%POINTER RQB) PAGE-SIZE)
	     (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
       (UMP FIRST-UMP (1+ UMP))
       (NP 0 (1+ NP)))
      ((>= VADR HIGH))
    (COND ((> NP 14.)
	   (FERROR NIL "TOO MANY PAGES")))
    (SETUP-UNIBUS-MAP UMP VADR))
  (+ 140000 (* FIRST-UMP 2000)))

(DEFUN UNIBUS-UNMAP-MT-RQB (RQB &OPTIONAL (FIRST-UMP 0)
			   &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
				(LOW (- (%POINTER RQB) (ARRAY-DIMENSION-N 0 RQB) 2))
				(HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
					 (// (ARRAY-LENGTH RQB) 2))))
  (DO ((VADR (+ LOW PAGE-SIZE) (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
       (UMP FIRST-UMP (1+ UMP)))
      ((>= VADR HIGH))
    (%UNIBUS-WRITE (+ 766140 (* 2 UMP)) 0)))

;;; The Unibus map is 16 words at 766140.  It consists of 14 address bits, write-ok, and valid
;;; It controls locations 140000-177777 (2000 byte locations per page).
(DEFUN SETUP-UNIBUS-MAP (UNIBUS-MAP-PAGE XBUS-ADR)
  (%UNIBUS-WRITE (+ 766140 (* 2 UNIBUS-MAP-PAGE))
		 (+ 140000 (LDB 1016 (%PHYSICAL-ADDRESS XBUS-ADR))))
  (+ 140000 (* UNIBUS-MAP-PAGE 2000) (* 4 (LOGAND 377 XBUS-ADR))))	; returns ubus-adr
|#

;Use MT-RUN to transfer the contents of an RQB.
;Use MT-RUN-SIMPLE to do spacing operations.

(DEFSIGNAL END-OF-TAPE FERROR (UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)
  "Mag tape runs off end of tape.")
(DEFSIGNAL READ-ONLY-TAPE FERROR (UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)
  "No write ring is seated in the tape for writing.")

(DEFVAR MT-RETRY-COUNT 5)
(DEFVAR MT-ATTEMPT-TO-WRITE-WITH-EXTENDED-GAP-COUNT 3)
(DEFUN MT-RUN (RQB COMMAND &OPTIONAL MINUS-BYTE-COUNT (UNIT 0) (DENSITY 0) IBM-MODE)
  (PROG ((RETRIES MT-RETRY-COUNT) (ERROR 0))
   AGAIN
    	(SETF (AREF RQB %MT-RQ-COMMAND) 0)
	(SETF (MT-COMMAND-UNIT) UNIT)
	(SETF (MT-COMMAND-DENSITY) DENSITY)
	(SETF (MT-COMMAND) COMMAND)
	(IF (NULL MINUS-BYTE-COUNT)
	    (SETQ MINUS-BYTE-COUNT
		  (MINUS (ARRAY-LENGTH (RQB-8-BIT-BUFFER RQB)))))
	(AS-1 MINUS-BYTE-COUNT RQB %MT-BYTE-COUNT)
;	(AS-1 (IF IBM-MODE 1_10. 0)
;	      RQB %MT-READ)
	(EXECUTE-MT-RQB RQB (= COMMAND %MT-COMMAND-READ))
	(COND ((MT-STATUS-EOT)
	       (CERROR ':NO-ACTION NIL 'END-OF-TAPE
		       "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
		       UNIT (SELECT-PROCESSOR
			      (:CADR (NTH COMMAND MT-COMMAND-NAMES))
			      (:LAMBDA COMMAND))
		       (- MINUS-BYTE-COUNT) DENSITY IBM-MODE RQB)))
	(IF (NOT (MT-STATUS-ERROR)) (RETURN T)
	  (SETQ ERROR (LDB %%TAPEMASTER-ERROR (AREF RQB %MT-RQ-STATUS)))
	  (MT-SPACE-REV 1 UNIT)
	  (COND
	    ((= COMMAND %MT-COMMAND-WRITE)
	     (IF (= ERROR #X11) ; write ring not there
		 (CERROR () () 'READ-ONLY-TAPE
			 "No write ring on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
			 UNIT (SELECT-PROCESSOR
				(:CADR (NTH COMMAND MT-COMMAND-NAMES))
				(:LAMBDA COMMAND))
			 (- MINUS-BYTE-COUNT) DENSITY IBM-MODE RQB)
	       (WHEN
		 ( RETRIES MT-ATTEMPT-TO-WRITE-WITH-EXTENDED-GAP-COUNT)
		 ; (SETQ COMMAND %MT-COMMAND-WRITE-WITH-EXTENDED-GAP)
		 (FORMAT ERROR-OUTPUT "~&[Retrying write operation]")
		 (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-ERASE 0 1 0 UNIT))))
	    ((= COMMAND %MT-COMMAND-READ) ;compensate for records size
	     (COND ((= ERROR 15.)	 ; that checked for the tape time out error.
		    (SETQ MINUS-BYTE-COUNT (MINUS (TAPEMASTER-DETERMINE-BLOCK-SIZE UNIT)))
		    (MT-SPACE-REV 1 UNIT))
		   ((= ERROR #X15)
		    #|(FORMAT T "End of file reached")|#
		    (MT-SPACE-TO-EOF UNIT)  ;this controller doesnt space over it, so do so.
		    (RETURN T)))) ; EOF
	    (T ; otherwise ...
	     (FORMAT ERROR-OUTPUT "~%MAGTAPE ERROR!")
	     (PRINT-MT-RQB RQB)))
	  (IF (>= (SETQ RETRIES (1- RETRIES)) 0)
	      (GO AGAIN)
	    (CATCH-ERROR-RESTART-EXPLICIT-IF T (MT-ERROR :RETRY "Retry magtape operation.")
	      (FERROR 'MT-ERROR "MagTape operation failed."))
	    (GO AGAIN)))))

(DEFUN MT-RUN-SIMPLE (COMMAND UNIT &OPTIONAL (COUNT 1) RQB)
  (TAPEMASTER-EXECUTE-SIMPLE-COMMAND COMMAND 0 COUNT 0 UNIT RQB)
  T)


;Convenient spacing operations.

;Note: NTIMES fed thru to hardware.  NTIMES of 0 means moby many.
(DEFUN MT-SPACE (&OPTIONAL (NTIMES 1) (UNIT 0) RQB)
  (MT-RUN-SIMPLE %MT-COMMAND-SPACE-FOR UNIT NTIMES RQB))

;(DEFUN MT-SPACE-TO-EOF (&OPTIONAL (UNIT 0) (NTIMES 1))
;  (DOTIMES (C NTIMES)
;    (MT-SPACE 0 UNIT)))	;note 0 arg to MT-SPACE is infinity on UNIBUS controller.

(defun mt-space-to-eof (&optional (unit 0) (ntimes 1))
  (dotimes (c ntimes)
    (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SPACE-FILEMARK 0 10000 0 UNIT)))

(DEFUN MT-SPACE-REV (&OPTIONAL (NTIMES 1) (UNIT 0) RQB) ;might need status
  (DOTIMES (I NTIMES)
    (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SPACE 0 1 1 UNIT RQB)))

;; Reverse through the tape, positioning the tape at the beginning of a file.
;; If SKIP-N-BLOCKS is 0, this positions the tape at the beginning of this file.
;; If SKIP-N-BLOCKS is 1, this positions the tape at the beginning of the previous file, etc.
;; If this reaches the beginning of the tape prematurely, it stops there and returns NIL.
(DEFUN MT-SPACE-REV-TO-BOF (&OPTIONAL (UNIT 0) (SKIP-N-FILES 0) &AUX RQB)
  (UNWIND-PROTECT
      (PROG ()
	(SETQ RQB (GET-DISK-RQB 0))
	L (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SEARCH-FILEMARK 0 1 1 UNIT RQB)
	(COND ((MT-STATUS-EOF)
	       (COND ((ZEROP SKIP-N-FILES)
		      ;; If we stop at an EOF block, we must space forward over it.
		      (MT-SPACE 1 UNIT RQB)
		      (RETURN T)))
	       (DECF SKIP-N-FILES))
	      ((MT-STATUS-BOT)
	       (RETURN (ZEROP SKIP-N-FILES))))
	  (GO L))
    (RETURN-DISK-RQB RQB)))

;; This function attempts to bypass all files on the tape until two
;; consecutive EOFs are found, then positions the tape over the last EOF.
;; The tape is now in a configuration allowing one to append new files.
(DEFUN MT-SPACE-TO-APPEND (&OPTIONAL (UNIT 0) &AUX RQB)
  (UNWIND-PROTECT
    (PROGN
      (SETQ RQB (GET-DISK-RQB 0))
      (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SEARCH-MULTIPLE 0 2 0 UNIT RQB)
      (IF (MT-STATUS-EOT)
	  (CERROR ':NO-ACTION NIL 'END-OF-TAPE
		  "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
		  UNIT '%MT-COMMAND-SPACE-FOR 1 0 NIL RQB)))
    (RETURN-DISK-RQB RQB))
  T)

(DEFUN MT-REWIND (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-REWIND UNIT))

(DEFUN MT-WRITE-EOF (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-WRITE-EOF UNIT))

(DEFUN MT-UNLOAD (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-OFFLINE UNIT))

(DEFUN MT-OFFLINE (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-OFFLINE UNIT))

;; Standard End of Tape handlers

;; This one is useful when you have transporting things which are bigger than
;; a tape.  it just rewinds, lets you reload, and continues.
;; Install it as a handler for END-OF-TAPE, using CONDITION-BIND.
(DEFUN CONTINUING-MT-EOT-HANDLER (CONDITION &AUX (UNIT (SEND CONDITION ':UNIT)))
  (PROG ((STREAM ERROR-OUTPUT))
	(MT-REWIND UNIT)
	(FUNCALL STREAM ':BEEP)
	(FORMAT STREAM "~%>>> MagTape unit ~D reached end of tape <<<~%" UNIT)
	(MT-OFFLINE UNIT)			;This will wait...
     L  (FORMAT STREAM "Please type [Resume] to continue tape operation: ")
	(FUNCALL STREAM ':CLEAR-INPUT)
	(COND ((NOT (CHAR-EQUAL (FUNCALL STREAM ':TYI) #\RESUME))
	       (FUNCALL STREAM ':BEEP)
	       (FUNCALL STREAM ':TYO #\CR)
	       (GO L)))
	(FORMAT STREAM "[Resuming tape operation]~%")
	(RETURN ':NO-ACTION)))


