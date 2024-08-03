;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 26.2
;;; Reason:
;;;  Improve error notifications.
;;;  
;;;  CHECK-FOR-ERROR accepts :ERROR-TRACE argument, something to print if a
;;;  tape error occurs and the trae variable TM:*NOTIFY-ON-ERRORS* is
;;;  non-NIL.  Also add this argument wherever CHECK-FOR-ERROR is called.
;;; Written 21-Sep-88 12:05:15 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Johannes Brahms from band 3
;;; with Experimental System 126.86, Experimental ZWEI 126.10, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:05:32
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:29
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:33
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:38
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:41
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun space-filemark (unit reverse number-of-blocks density)
  (with-reset-on-abort
    (execute-command %space-filemark unit reverse density 0 0 number-of-blocks)
    (wait-for-command-complete)
    (check-for-error :error-trace 'space-filemark)
    (- number-of-blocks (tpb-records *tpb*) 1)))

;; Copied from LAD: RELEASE-3.TAPE; TAPEMASTER-DRIVER.LISP#207 on 30-Mar-87 14:36:01

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:44
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "

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

;; Copied from LAD: RELEASE-3.TAPE; TAPEMASTER-DRIVER.LISP#207 on 30-Mar-87 14:36:02

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:47
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "

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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:50
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun rewind (unit wait?)
  (execute-command %rewind unit 0 0 0 0 0)
  (when wait?
    (wait-for-command-complete)
    (check-for-error :error-trace 'rewind)))

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:52
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun unload (unit seconds-to-wait)
  (execute-command %offline-and-unload unit 0 0 0 0 0)
  (when seconds-to-wait
    (wait-for-command-complete)
    (check-for-error :error-trace 'unload))
  t)

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:56
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun write-filemark (unit)
  (execute-command %write-filemark unit 0 0 0 0 0)
  (wait-for-command-complete)
  (condition-case (condition)
      (check-for-error :error-trace 'write-filemark)
    (eot (signal 'tape:physical-end-of-tape
		 :device-type 'tapemaster
		 :unit unit
		 :data-transferred 0))))

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:06:59
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun erase-fixed-length (unit number-of-blocks)
  (execute-command %erase-fixed-length unit 0 0 0 0 number-of-blocks)
  (wait-for-command-complete)
  (condition-case (condition)
      (check-for-error :error-trace 'erase-fixed-length)
    (eot (signal 'tape:physical-end-of-tape
		 :device-type 'tapemaster
		 :unit unit
		 :data-transferred 0))))

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:07:01
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


(defun erase-tape (unit)
  (execute-command %erase-tape unit 0 1 0 0 0)
  (wait-for-command-complete)
  (check-for-error :error-trace 'erase-tape)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for read/write operations

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:07:03
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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
	    (:no-error (setq finished? t))))))))

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:07:05
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:07:07
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 12:07:10
#10R TM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DRIVER  "


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

))
