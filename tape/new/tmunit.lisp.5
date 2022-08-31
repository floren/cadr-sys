;;; -*- Mode:Lisp;Package:Tape;Base:8 -*-
;;; (c) 1984 Lisp Machine Incorporated
;;; TapeMaster unit definition.  Multibus/Lambda hardware

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

)

(EVAL-WHEN (COMPILE LOAD EVAL)
(DEFSUBST MT-COMMAND-UNIT ()
  (LDB %%tapemaster-tape-select (AR-1 RQB %MT-RQ-COMMAND)))

(DEFSUBST MT-COMMAND-DENSITY ()
  (LDB %%tapemaster-density (AR-1 RQB %MT-RQ-COMMAND)))

(DEFSUBST MT-COMMAND-INTERRUPT-ENABLE ()
  (LDB %%tapemaster-interrupts (AR-1 RQB %MT-RQ-COMMAND)))

;This field's value is one of the command codes below.
(DEFSUBST MT-COMMAND ()
  (AR-1 RQB %MT-RQ-COMMAND-REALLY))  ; the bits and the command can't fit on the TM

(comment ;old version
;;; This doesn't get called by mt-run-simple when you're running the Tapemaster
(DEFUN EXECUTE-MT-RQB (RQB &OPTIONAL SET-MODIFIED &AUX MB-ADR)
  (WIRE-MT-RQB RQB T SET-MODIFIED)
  (SETQ MB-ADR (MULTIBUS-MAP-MT-RQB RQB))  
  (MT-WAIT-READY)
  ; First, wait for the unit
  (tapemaster-execute-command tapemaster-nop (ldb %%tapemaster-tape-select
						  (ar-1 rqb %mt-rq-command))
			      0 0 t)
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
		(ferror nil "~%Record on tape too big, was ~O, allowed ~O"
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
  (tm-aset-iopb rqb) ; renders the code below superfluous
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
) ; end comment (old version)

;;; New version with different arglist
(DEFUN TM-EXECUTE-RQB (RQB COM control BYTE-COUNT &OPTIONAL SET-MODIFIED &AUX MB-ADR)
  "Correspondence with EXECUTE-MT-RQB offsets of RQB
COM  %MT-RQ-COMMAND-REALLY (the command code)
CONTROL  %MT-RQ-COMMAND (what goes in the control register, like the unit number)
BYTE-COUNT  %MT-RQ-BYTE-COUNT (positive byte count)"
  (WIRE-MT-RQB RQB T SET-MODIFIED)
  (SETQ MB-ADR (MULTIBUS-MAP-MT-RQB RQB))
  (tapemaster-wait-controller-ready)
  ; First, wait for the unit
  (tapemaster-execute-command tapemaster-nop (ldb %%tapemaster-tape-select CONTROL) 0 0 t)
  (tapemaster-build-iopb com control
			 BYTE-COUNT		; already sign-extended
			 MB-ADR)		; not sure if that's the right thing
  ;; This part does read/write
  (cond
    ((or (= com tapemaster-buffered-read)
	 (= com tapemaster-direct-read))
     ;; Send the command, wait, and read off the bus
     (tapemaster-execute-command com
				 (dpb 1 %%tapemaster-width control)
				 byte-count
				 MB-ADR
				 t)
     (let ((rc (tapemaster-read-iopb-16 %tapemaster-return-count)))
       (cond ((and (not (zerop rc))		;if no data xferred, will deal with it later
		   (not (= (tapemaster-read-iopb-16 %tapemaster-records)
			   rc)))
	      (ferror nil "~%Record on tape too big, was ~O, allowed ~O"
		      (tapemaster-read-iopb-16 %tapemaster-records)
		      rc)))))
    ((or (= com tapemaster-buffered-write)
	 (= com tapemaster-direct-write))
     (tapemaster-execute-command com
				 (dpb 1 %%tapemaster-width control)
				 byte-count
				 MB-ADR
				 t))
    (t (tapemaster-channel-attention)))	; Just DO IT
  (tapemaster-wait-controller-ready)
  ;; Update the status
  (tm-aset-iopb rqb) ; renders the code below superfluous
; (format t "~%#Transferred ~O bytes.#~%" (tapemaster-read-iopb-16 %tapemaster-return-count))
  ;; Update the return count -- it's MINUS, remember
  (UNWIRE-MT-RQB RQB)
  RQB)

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

(DEFUN TM-RUN (RQB COMMAND &OPTIONAL BYTE-COUNT (UNIT 0) (DENSITY 0) IBM-MODE &AUX CONTROL)
  (PROG ((RETRIES MT-RETRY-COUNT) (ERROR 0))
   AGAIN
	(SETF (MT-COMMAND-UNIT) UNIT)
	(SETF (MT-COMMAND-DENSITY) DENSITY)
	(IF (NULL BYTE-COUNT) (SETQ BYTE-COUNT (ARRAY-LENGTH (RQB-8-BIT-BUFFER RQB))))
	(TM-EXECUTE-RQB RQB COMMAND CONTROL BYTE-COUNT (= COMMAND %MT-COMMAND-READ))
	(COND ((MT-STATUS-EOT)
	       (CERROR ':NO-ACTION NIL 'END-OF-TAPE
		       "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
		       UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)))
	(IF (NOT (MT-STATUS-ERROR)) (RETURN T)
	  (SETQ ERROR (LDB %%TAPEMASTER-ERROR (AREF RQB %MT-RQ-STATUS)))
	  (MT-SPACE-REV 1 UNIT)
	  (COND
	    ((= COMMAND TAPEMASTER-WRITE-COMMAND)
	     (IF (= ERROR #X11)			; write ring not there
		 (CERROR () () 'READ-ONLY-TAPE
			 "No write ring on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
			 UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)
	       (WHEN
		 ( RETRIES MT-ATTEMPT-TO-WRITE-WITH-EXTENDED-GAP-COUNT)
		 ; (SETQ COMMAND %MT-COMMAND-WRITE-WITH-EXTENDED-GAP)
		 (FORMAT ERROR-OUTPUT "~&[Retrying write operation]")
		 (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-ERASE 0 1 0 UNIT))))
	    ((= COMMAND TAPEMASTER-READ-COMMAND) ;compensate for records size
	     (COND ((= ERROR 15.)	 ; that checked for the tape time out error.
		    (SETQ BYTE-COUNT (TAPEMASTER-DETERMINE-BLOCK-SIZE UNIT))
		    (TM-SPACE-REV 1 UNIT))
		   ((= ERROR #X15)
		    #|(FORMAT T "End of file reached")|#
		    (TM-SPACE-TO-EOF UNIT)  ;this controller doesnt space over it, so do so.
		    (RETURN T)))) ; EOF
	    (T ; otherwise ...
	     (FORMAT ERROR-OUTPUT "~%MAGTAPE ERROR!")
	     (PRINT-MT-RQB RQB)))
	  (IF (>= (SETQ RETRIES (1- RETRIES)) 0)
	      (GO AGAIN)
	    (CATCH-ERROR-RESTART-EXPLICIT-IF T (MT-ERROR :RETRY "Retry magtape operation.")
	      (FERROR 'MT-ERROR "MagTape operation failed."))
	    (GO AGAIN)))))

(DEFUN TM-RUN-SIMPLE (COMMAND UNIT &OPTIONAL (COUNT 1) RQB)
  (TAPEMASTER-EXECUTE-SIMPLE-COMMAND COMMAND 0 COUNT 0 UNIT RQB)
  T)


;;; Tapemaster Unit definitions
(defflavor tapemaster-unit (status-array (make-array 22. ':type art-16b))
	   (basic-local-mt-unit-mixin) )

(defmethod (tapemaster-unit :status) ()
  (tm-aset-iopb status-array)
  status-array)

(defmethod (tapemaster-unit :status-ready) ()
  (tapemaster-unit-ready number))

(defmethod (tapemaster-unit :command-ready) ()
  (tapemaster-controller-ready))

(defmethod (tapemaster-unit :wait-controller-ready) ()
  )

(defmethod (tapemaster-unit :wait-unit-ready) ()
  )

(defmethod (tapemaster-unit :i-write-buffer) (buffer count density ibm-mode)

  buffer)

(defmethod (tapemaster-unit :i-read-buffer) (buffer count density)

  buffer)

(defmethod (tapemaster-unit :decode-status) (status-array type)
  (selectq type
    (:eot)
    (:eof)
    (:bot)
    (:write-only)
    (:error) ;t/nil
    (:bytes-transferred)
    (:on-line)
    (:error-code) ; a number
    (:bad-tape)

(defmethod (tapemaster-unit :status-eof) (status)
  )

(defmethod (tapemaster-unit :status-eot) (status)
  )

(defmethod (tapemaster-unit :status-bot) (status)
  )

(defmethod (tapemaster-unit :status-error) (status)
  )

(defmethod (tapemaster-unit :odd-bytes-ok-p) () t) ;;; ???
(defmethod (tapemaster-unit :access) () ':tapemaster)

(defmethod (tapemaster-unit :begin-interleaved-rewind) (id)
  (tapemaster-execute-simple-command tapemaster- number)
  )

(defmethod (tapemaster-unit :advance-file) ()
  )

;Note: NTIMES fed thru to hardware.  NTIMES of 0 means moby many.
(DEFUN TM-SPACE (NTIMES UNIT &OPTIONAL RQB)
  (tm-run-simple tapemaster-space UNIT NTIMES RQB))

(defmethod (tapemaster-unit :space) (&optional (ntimes 1) rqb)
  (tm-space ntimes number rqb))

(defun tm-space-to-eof (ntimes unit-number)
  (dotimes (c ntimes)
    (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SPACE-FILEMARK 0 10000 0 UNIT-number)))

(defmethod (tapemaster-unit :space-to-eof) (&OPTIONAL (ntimes 1))
  (tm-space-to-eof ntimes number))

(defun tm-space-rev (ntimes unit &optional rqb)
  (DOTIMES (I NTIMES)
    (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SPACE 0 1 1 UNIT RQB)))

(DEFmethod (tapemaster-unit :SPACE-REV) (&OPTIONAL (NTIMES 1) RQB) ;might need status
  (tm-space-rev ntimes number rqb))

;; Reverse through the tape, positioning the tape at the beginning of a file.
;; If SKIP-N-BLOCKS is 0, this positions the tape at the beginning of this file.
;; If SKIP-N-BLOCKS is 1, this positions the tape at the beginning of the previous file, etc.
;; If this reaches the beginning of the tape prematurely, it stops there and returns NIL.
(DEFUN TM-SPACE-REV-TO-BOF (&OPTIONAL (UNIT 0) (SKIP-N-FILES 0) &AUX RQB)
  (UNWIND-PROTECT
      (PROG ()
	(SETQ RQB (GET-DISK-RQB 0))
	L (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SEARCH-FILEMARK 0 1 1 UNIT RQB)
	(COND ((TMRQB-STATUS-EOF)
	       (COND ((ZEROP SKIP-N-FILES)
		      ;; If we stop at an EOF block, we must space forward over it.
		      (MT-SPACE 1 UNIT RQB)
		      (RETURN T)))
	       (DECF SKIP-N-FILES))
	      ((TMRQB-STATUS-BOT)
	       (RETURN (ZEROP SKIP-N-FILES))))
	  (GO L))
    (RETURN-DISK-RQB RQB)))

;; This function attempts to bypass all files on the tape until two
;; consecutive EOFs are found, then positions the tape over the last EOF.
;; The tape is now in a configuration allowing one to append new files.
(DEFUN TM-SPACE-TO-APPEND (UNIT &AUX RQB)
  (UNWIND-PROTECT
    (PROGN
      (SETQ RQB (GET-DISK-RQB 0))
      (TAPEMASTER-EXECUTE-SIMPLE-COMMAND TAPEMASTER-SEARCH-MULTIPLE 0 2 0 UNIT RQB)
      (IF (TM-STATUS-EOT RQB)
	  (CERROR ':NO-ACTION NIL 'END-OF-TAPE
		  "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
		  UNIT 'TAPEMASTER-SPACE 1 0 NIL RQB)))
    (RETURN-DISK-RQB RQB))
  T)

(defmethod (tapemaster-unit :space-to-append) ()
  (tm-space-to-append number))

(DEFMETHOD (TAPEMASTER-UNIT :REWIND) () (TM-RUN-SIMPLE TAPEMASTER-REWIND NUMBER))

(DEFMETHOD (TAPEMASTER-UNIT :WRITE-EOF) () (TM-RUN-SIMPLE TAPEMASTER-WRITE-FILEMARK NUMBER))

(DEFMETHOD (TAPEMASTER-UNIT :OFFLINE) () (TM-RUN-SIMPLE TAPEMASTER-OFFLINE-AND-UNLOAD NUMBER))


