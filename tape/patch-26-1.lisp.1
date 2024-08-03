;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 26.1
;;; Reason:
;;;  Tapemaster read/write operations had call to non-existent error flavor.
;;; Written 21-Sep-88 11:35:48 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Johannes Brahms from band 3
;;; with Experimental System 126.86, Experimental ZWEI 126.10, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 11:36:14
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
	(check-for-error)
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

; From modified file DJ: L.TAPE; TAPEMASTER-DRIVER.LISP#209 at 21-Sep-88 11:36:23
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
	(check-for-error)
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
