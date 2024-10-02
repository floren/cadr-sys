;;; -*- Mode:LISP; Package:TAPE; Base:10; Readtable:CL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;


;;; remote tape, to use the RTAPE.c server on a UNIX host.
;;; 17-Apr-86 12:41:55 -George Carrette
;;; Feb 2024 (BV) - various fixes and improvements.

(defflavor rtape-device ((unit 0)
			 (density 1600)
			 (host nil)
			 (stream nil)
			 (record-stream nil)
			 (status (make-rtape-status))
			 (block-size 4096)
			 (probe-id 0)
			 (logged-in-as nil)
			 (read-only nil))
	   (basic-tape-device)
  :gettable-instance-variables)

(defmethod (rtape-device :print-self) (outstream &rest ignore)
  (si:PRINTING-RANDOM-OBJECT (self outstream :type)
      (format outstream "~@[unit ~s~]~:[~; connected to~]~@[ host ~a~]~@[ user ~a~]~:[~; (read-only)~]"
	      unit stream host logged-in-as read-only)))

(defconst rtape-LOGIN-OPCODE		1)
(defconst rtape-MOUNT-OPCODE		2)
(defconst rtape-PROBE-OPCODE		3)
(defconst rtape-READ-OPCODE		4)
(defconst rtape-WRITE-OPCODE		5)
(defconst rtape-REWIND-OPCODE		6)
(defconst rtape-REWIND-SYNC-OPCODE	7)
(defconst rtape-OFFLINE-OPCODE		8)
(defconst rtape-FILEPOS-OPCODE		9)
(defconst rtape-BLOCKPOS-OPCODE	10)
(defconst rtape-WRITE-EOF-OPCODE	12)
(defconst rtape-CLOSE-OPCODE		13)
(defconst rtape-LOGIN-RESPONSE-OPCODE	33)
(defconst rtape-DATA-OPCODE		34)
(defconst rtape-EOFREAD-OPCODE		35)
(defconst rtape-STATUS-OPCODE		36)

(defconst rtape-DLEN		15)		;Note: 16 *including* namelength
(defconst rtape-MAXSTRING	100)

(defconst rtape-operations '(
			     rtape-LOGIN-OPCODE
			     rtape-MOUNT-OPCODE
			     rtape-PROBE-OPCODE
			     rtape-READ-OPCODE
			     rtape-WRITE-OPCODE
			     rtape-REWIND-OPCODE
			     rtape-REWIND-SYNC-OPCODE
			     rtape-OFFLINE-OPCODE
			     rtape-FILEPOS-OPCODE
			     rtape-BLOCKPOS-OPCODE
			     rtape-WRITE-EOF-OPCODE
			     rtape-CLOSE-OPCODE
			     rtape-LOGIN-RESPONSE-OPCODE
			     rtape-DATA-OPCODE
			     rtape-EOFREAD-OPCODE
			     rtape-STATUS-OPCODE))



(defconst rtape-status
	  (list (list 'version 1	"protocol version")
		(list 'probeid 2	"Id in corresponding PROBE")
		(list 'read 3	"Number of blocks read")
		(list 'skipped 3	"Number of blocks skipped")
		(list 'discarded 3	"Numbert of writes discarded")
		(list 'lastop 1	"last opcode received")
		(list 'density 2	"Density in BPI")
		(list 'retries 2	"number of retries in last op")
		(list 'namelength 1	"length of next string")
		(list 'drive RTAPE-DLEN	"Drive name in use" :string)
		(list 'solicited 1/8	"This status was asked for")
		(list 'bot 1/8	"At BOT")
		(list 'pasteot 1/8	"Past EOT")
		(list 'eof 1/8	"Last op reached EOF")
		(list 'nli 1/8	"Not logged in")
		(list 'mounted 1/8	"Tape is mounted")
		(list 'message 1/8	"Error message follows")
		(list 'harderr 1/8	"Hard error encountered")
		(list 'softerr 1/8	"Soft errors encountered")
		(list 'offline 1/8	"Drive is offline")
		(list 'padding 6/8      "used to pad the record to byte boundary")
		(list 'string RTAPE-MAXSTRING "Error message" :string)))


(defun rtape-status-probeid (x)
  (ref-int x 'probeid))

(defun rtape-status-drive (x)
  (substring (ref-string x 'drive) 0 (ref-int x 'namelength)))

(defun rtape-status-message (x)
  (if (ref-flag x 'message)
      (ref-string x 'string t)))

(defconst rtape-status-size
	  (do ((l rtape-status (cdr l))
	       (size 0 (+ (cadar l) size)))
	      ((null l) size)))

(defun lookup-ref (structure name)
  (declare (values index length))
  (do ((l (symeval (named-structure-p structure)) (cdr l))
       (index 0 (+ (cadar l) index)))
      ((null l)
       (ferror nil "cant find index named ~S in ~S" name structure))
    (when (eq (caar l) name)
      (return (values index (cadar l))))))

(defun ref-int (structure name)
  (multiple-value-bind (index length)
      (lookup-ref structure name)
    (cond ((not (or (typep index 'ratio) (typep length 'ratio)))
	   (do ((j 0 (1+ j))
		(number 0 (dpb (aref structure (+ index j)) (byte 8 (* 8 j)) number)))
	       ((= j length)
		number)))
	  ('else
	   (multiple-value-bind (whole-index fraction)
	       (floor index)
	     (if (> (+ fraction length) 1)
		 (ferror nil "fractional reference across byte boundaries no supported"))
	     (ldb (byte (* length 8) (* fraction 8)) (aref structure whole-index)))))))

(defun set-int (structure name value)
  (multiple-value-bind (index length)
      (lookup-ref structure name)
    (cond ((not (or (typep index 'ratio) (typep length 'ratio)))
	   (do ((j 0 (1+ j)))
	       ((= j length)
		value)
	     (setf (aref structure (+ index j)) (ldb (byte 8 (* 8 j)) value))))
	  ('else
	   (multiple-value-bind (whole-index fraction)
	       (floor index)
	     (if (> (+ fraction length) 1)
		 (ferror nil "fractional reference across byte boundaries no supported"))
	     (setf (ldb (byte (* length 8) (* fraction 8)) (aref structure whole-index)) value))))))


(defun ref-flag (x name)
  (not (zerop (ref-int x name))))
	  

(defun ref-string (structure name &optional null-terminated)
  (multiple-value-bind (index length)
      (lookup-ref structure name)
    (do ((s (make-string length))
	 (j 0 (1+ j)))
	((= j length)
	 (cond ((not null-terminated)
		s)
	       ('else
		(substring s 0 (string-search 0 s)))))
      (setf (aref s j) (aref structure (+ index j))))))


(defun ref-describe (structure stream)
  (format stream "~&~S~%" structure)
  (let ((alist (symeval (named-structure-p structure)))
	(max-s 0)
	(max-d 0))
    (dolist (elem alist)
      (setq max-s (max max-s (flatsize (car elem))))
      (setq max-d (max max-d (flatc (caddr elem)))))
    (dolist (elem alist)
      (format stream " ~VS (~VA): " max-s (car elem) max-d (caddr elem))
      (cond ((memq :string elem)
	     (let ((s (ref-string structure (car elem))))
	       (format stream "~S~%" (substring s 0 (string-search 0 s)))))
	    ('else
	     (format stream "~D~%" (ref-int structure (car elem))))))))

(defun make-rtape-status ()
  (make-array rtape-status-size :type 'art-8b :named-structure-symbol 'rtape-status
	      :leader-length 3))


(defselect ((:property rtape-status named-structure-invoke) ignore)
  (:print-self (object stream &rest ignore)
    (format stream "#<RTAPE-STATUS ~A ~A ~{~S~^ ~}>"
	    (rtape-status-drive object)
	    (or (rtape-status-message object) "")
	    (mapcan #'(lambda (flag)
			(if (ref-flag object flag)
			    (list flag)))
		    '(mounted harderr softerr offline))))
  (:describe (object)
    (ref-describe object standard-output)))
    

(defmethod (rtape-device :set-options) (&rest options)
  (cond ((not options)
	 (let ((choose-unit unit)
	       (choose-host host)
	       (read-only-p read-only))
	   (tv:choose-variable-values
	     `((,(locf choose-unit) "Unit" :number)
	       (,(locf choose-host) "Remote Host")
	       (,(locf read-only-p) "Read-Only" :boolean))
	     :label '(:string "Choose Remote-Magtape options" :font fonts:tr12b))
	   (send self :set-options :unit choose-unit :host choose-host
		 :read-only read-only-p)))
	('else
	 (check-attribute-list options)
	 (do ((l options (cddr l)))
	     ((null l))
	   (case (car l)
	     (:host
	      (setq host (and (cadr l) (si:parse-host (cadr l)))))
	     (:unit
	      (setq unit (cadr l)))
	     (:density
	      (or (zerop (cadr l))
		  (setq density (cadr l))))
	     (:read-only
	      (setq read-only (cadr l)))
	     (t
	       (signal 'invalid-option :object self :option (car l) :value (cadr l))))))))


(defmethod (rtape-device :initialize) (&rest init-options)
  (when init-options
    (lexpr-send self :set-options init-options)))


(defvar *rtape-debug* nil)

(defmethod (rtape-device :net-connect) ()
  (send self :net-close)
  (or host
      (ferror nil "no host specified yet"))
  (setq stream (chaos:open-stream host "RTAPE"))
  (setq record-stream (make-instance 'record-stream :stream stream)))

(defmethod (rtape-device :mount-command) ()
;;; command to mount appears to be a string with two lines (possibly)
;;; TYPE REEL DRIVE BLOCKSIZE DENSITY OPTIONS...
;;; ...optional message (for operator?)...
;;; TYPE = "READ" or "WRITE" or "BOTH"
;;; OPTIONS are	"NOREWIND" and "OFFLINE"
  (let ((string (format nil "~A ~A ~D ~D ~D~%~A~%"
			(if read-only "READ" "BOTH")
			"ANY"
			UNIT
			BLOCK-SIZE
			DENSITY
			"")))
    (when *rtape-debug*
      (format t "~&MOUNT ~S" STRING))
    (send record-stream :write rtape-mount-opcode string (length string))
    (send self :probe-status)
    (unless (ref-flag status 'mounted)
      (cond ((not read-only)
	     (cerror "Try mounting the unit READ-ONLY"
		     "Tape failed to mount: ~a"
		     (or (rtape-status-message status)
			 "unknown reason"))
	     (setq read-only t)
	     (send self :mount-command))
	    (t
	     (ferror nil "Tape failed to mount: ~a"
		     (or (rtape-status-message status)
			 "unknown reason")))))))

(defmethod (rtape-device :net-login) (username password)
  (send self :be-connected)
  (when (not logged-in-as)
    (let ((string (format nil "~A ~A" username password)))
      (send record-stream :write rtape-login-opcode string (length string))
      (send record-stream :force-output)
      (let ((reply (send record-stream :opcode)))
	(cond ((not reply)
	       (send self :net-close))
	      ((= reply rtape-login-response-opcode)
	       (let ((error-message (make-string (send record-stream :length))))
		 (send record-stream :read error-message (send record-stream :length))
		 (cond ((zerop (send record-stream :length))
			(setq logged-in-as username))
		     ('else
		      (if (string-search 0 error-message)
			  (setq error-message (substring error-message 0 (string-search 0 error-message))))
		      (format error-output "~&Login failed because ~A~%" error-message)))))
	      ('else
	       (rtape-response-error record-stream "login" rtape-login-response-opcode reply)))))))
	     

(defmethod (rtape-device :read-status) ()
  (send stream :force-output)
  (let ((opcode (send record-stream :opcode)))
    (cond ((not opcode)
	   (send self :net-close)
	   ())
	  ((= opcode rtape-status-opcode)
	   (send record-stream :read status rtape-status-size)
	   (when *rtape-debug*
	     (describe status))
	   t)
	  ('else
	   (rtape-response-error record-stream "read-status" rtape-status-opcode opcode)))))

(defun lookup-rtape-opcode (number)
  (dolist (sym rtape-operations)
    (when (= number (symeval sym)) (return-from lookup-rtape-opcode sym)))
  number)

(defun rtape-response-error (stream doing expected got)
  (declare (eh:error-reporter))
  (cerror "continue" "After ~A expecting ~S but got ~S"
	  doing (lookup-rtape-opcode expected) (lookup-rtape-opcode got))
  (send stream :read-flush))
  

(defmethod (rtape-device :probe-status) ()
  (incf probe-id)
  (send record-stream :start rtape-probe-opcode 2)
  (send record-stream :tyo (ldb (byte 8 0) probe-id))
  (send record-stream :tyo (ldb (byte 8 8) probe-id))
  (do-forever
    (when (not (send self :read-status))
      (ferror nil "Stream closed while reading status: ~S" status))
    (when (= (ref-int status 'probeid) probe-id)
      (return t))))


(defmethod (rtape-device :be-connected) ()
  (when (not RECORD-STREAM)
    (SEND SELF :NET-CONNECT)
    (send self :mount-command)))

(defmethod (rtape-device :net-close) ()
  (setq record-stream nil
	read-only nil)
  (setq probe-id 0)
  (setq logged-in-as nil)
  (when stream
    (close stream)
    (setq stream nil)))

(defmethod (rtape-device :deinitialize) ()
  (send self :net-close))

(defmethod (rtape-device :lock-device) ()
  ())

(defmethod (rtape-device :unlock-device ) ()
  ())

(defmethod (rtape-device :device-locked-p) ()
  ())


(defmethod (rtape-device :reset) ()
  (send self :deinitialize))

(defmethod (rtape-device :speed-threshold) (record-size)
  record-size)

(defun rtape-unimplemented ()
  (declare (eh:error-reporter))
  ;; (cerror "do nothing" "unimplemented")
  ;; Signal something which can be taken care of in other code!
  (signal 'driver-error :device-type 'rtape-device
	  :error-code #x42
	  :error-message "Operation Not Yet Implemented")
  )

;;; Tape positioning
;;;

(defmethod (rtape-device :rewind) (&optional (wait-p t))
  (send self :be-connected)
  (send record-stream :write rtape-rewind-opcode "" 0)
  (if wait-p (send self :probe-status)))

(defmethod (rtape-device :unload) ()
  (send self :be-connected)
  (send record-stream :write rtape-offline-opcode "" 0)
  (send self :probe-status))

(defmethod (rtape-device :space) (number-of-records &optional (speed :low))
  (check-type number-of-records (integer 1))
  (check-type speed (member :high :low))
  (send self :be-connected)
  (let ((s (format nil "~D" number-of-records)))
    (and *rtape-debug* (format t "~&**** Space records: ~d" number-of-records))
    (send record-stream :write rtape-blockpos-opcode s (length s)))
  (and *rtape-debug* (send self :probe-status)))


(defmethod (rtape-device :space-reverse) (number-of-records &optional (speed :low))
  (check-type number-of-records (integer 0))
  (check-type speed (member :high :low))
  (send self :be-connected)
  (cond ((rtape-unimplemented)
	 ;; this causes the server on the 3600 to barf anyway.
	 (let ((s (format nil "~D" (- number-of-records))))
	   (and *rtape-debug* (format t "~&**** Reverse space records: ~d" number-of-records))
	   (send record-stream :write rtape-blockpos-opcode s (length s)))
	 (and *rtape-debug* (send self :probe-status)))))


(defmethod (rtape-device :search-filemark) (number-of-filemarks &optional (speed :low))
  (check-type number-of-filemarks (integer 1))
  (check-arg speed (memq speed '(:high :low)) "a valid speed arg (:HIGH or :LOW)")
  (send self :be-connected)
  (let ((s (format nil "~D" number-of-filemarks)))
    (and *rtape-debug* (format t "~&**** Search filemark: ~d" number-of-filemarks))
    (send record-stream :write rtape-filepos-opcode s (length s)))
  (and *rtape-debug* (send self :probe-status)))

(defmethod (rtape-device :search-filemark-reverse) (number-of-filemarks &optional (speed :low))
  (check-type number-of-filemarks (integer 1))
  (check-arg speed (memq speed '(:high :low)) "a valid speed arg (:HIGH or :LOW)")
  (send self :be-connected)
  (let ((s (format nil "~D" (- number-of-filemarks))))
    (and *rtape-debug* (format t "~&**** Search filemark reverse: ~d" number-of-filemarks))
    (send record-stream :write rtape-filepos-opcode s (length s)))
  (and *rtape-debug* (send self :probe-status)))
  

;;; Read/Write
;;;

(defmethod (rtape-device :optimal-chunk-size) (record-size)
  (check-type record-size (integer 1))
  record-size)

(defmethod (rtape-device :read-block) (dma-buffer record-size)
  (check-dma-buffer dma-buffer)
  (check-arg record-size (<= record-size (* (array-length dma-buffer) 4))
	     "less than or equal to the size of the dma-buffer")
  (send self :be-connected)
  (let ((s (format nil "~D" 1)))
    (when *rtape-debug*
      (format t "~&**** Read Block"))
    (send record-stream :write rtape-read-opcode s (length s))
    (send record-stream :force-output)
    (DO-FOREVER
      (let ((reply (send record-stream :opcode)))
	(cond ((not reply)
	       (send self :net-close)
	       (ferror nil "end of file"))
	      ((= reply rtape-data-opcode)
	       (let ((amount (send record-stream :read (si:dma-buffer-string dma-buffer) record-size)))
		 (when *rtape-debug*
		   (send self :probe-status))
		 (return amount)))
	      ((= reply rtape-eofread-opcode)
	       (send record-stream :read-flush)
	       (when *rtape-debug*
		 (send self :probe-status))
	       (signal 'filemark-encountered
		       :device-type 'rtape
		       :unit unit
		       :data-transferred 0))
	      ((= reply rtape-status-opcode)
	       (SEND SELF :READ-STATUS))
	      ('else
	       (rtape-response-error record-stream "read block" rtape-data-opcode reply)))))))


(defmethod (rtape-device :write-block) (dma-buffer record-size)
  (check-arg dma-buffer (eq (named-structure-p dma-buffer) 'si:dma-buffer)
	     "a DMA-BUFFER object")
  (check-arg record-size (<= record-size (* (array-length dma-buffer) 4))
	     "less than or equal to the size of the dma-buffer")
  (send self :be-connected)
  (when *rtape-debug*
    (format t "~&**** Write Block"))
  (when read-only
    (warn ">>Warning: attempting to write on read-only ~s" self))
  (send record-stream :write rtape-write-opcode (si:dma-buffer-string dma-buffer) record-size)
  (when *rtape-debug*
    (send self :probe-status)))

(defmethod (rtape-device :read-array) (array number-of-records record-size)
  (check-array array number-of-records record-size)
  (check-type number-of-records (integer 0))
  (check-arg record-size (and (typep record-size '(integer 1 65536))
			      (zerop (remainder record-size 1024)))
	     "a record-size that is a multiple of 1024 and between 1024 and 65536")
  (send self :be-connected)
  (using-resource (block si:dma-buffer (ceiling record-size 1024))
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
	    (send self :read-block block record-size)
	  ((filemark-encountered physical-end-of-tape)
	   (signal (type-of condition)
		   :device-type 'rtape
		   :unit unit
		   :data-transferred count)))
	(copy-array-portion block-array 0 adjusted-record-size
			    array copy-start (+ copy-start adjusted-record-size))))))

	      
(defmethod (rtape-device :write-array) (array number-of-records record-size)
  (check-array array number-of-records record-size)
  (check-type number-of-records (integer 0))
  (check-arg record-size (and (typep record-size '(integer 1 65536))
			      (zerop (remainder record-size 1024)))
	     "a record-size that is a multiple of 1024 and between 1024 and 65536")
  (send self :be-connected)
  (when *rtape-debug* (format t "~&**** Write Array"))
  (when read-only
    (warn ">>Warning: attempting to write on read-only ~s" self))
  (dotimes (j number-of-records)
    (send record-stream :write-sub rtape-write-opcode array (* j record-size) record-size))
  (when *rtape-debug*
    (send self :probe-status)))


(defmethod (rtape-device :read-to-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (send self :be-connected)
  (rtape-unimplemented))


(defmethod (rtape-device :write-from-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (send self :be-connected)
  (rtape-unimplemented))


(defmethod (rtape-device :compare-to-disk)
	   (disk-unit starting-block number-of-blocks record-size &key silent)
  (check-unit disk-unit)
  (check-type starting-block (integer 0))
  (check-type number-of-blocks (integer 0))
  (check-type record-size (integer 1))
  (send self :be-connected)
  (rtape-unimplemented))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other things
;;;

(defmethod (rtape-device :write-filemark) (&optional (number-of-filemarks 1))
  (check-type number-of-filemarks (integer 1))
  (send self :be-connected)
  (if *rtape-debug*
      (format t "~&**** Write filemarks: ~D" number-of-filemarks))
  (when read-only
    (warn ">>Warning: attempting to write on read-only ~s" self))
  (dotimes (j number-of-filemarks)
    (send record-stream :write rtape-write-eof-opcode "" 0))
  (if *rtape-debug*
      (send self :probe-status)))

(compile-flavor-methods rtape-device)

(define-tape-device rtape-device "rtape" true)

#|| Evaluate this to setup device menu if not in menu.

(progn (setq *available-devices* nil)
       (dolist (list *tape-device-alist*)
	 (when (funcall (third list))
	   (push (second list)
		 *available-devices*)))
       
       (setq *selected-device*
	     (cond ((null *available-devices*))
		   ((memq (car *default-device*) *available-devices*)
		    (lexpr-funcall 'parse-device *default-device*))
		   (*available-devices*
		    (parse-device (car *available-devices*)))))
       (setq *selected-format*
	     (cond (*default-format*
		    (lexpr-funcall 'parse-format *default-format*))
		   (*tape-format-alist*
		    (parse-format (cdar *tape-format-alist*))))))

||#


;; crufty record stream

(defflavor record-stream
	 (stream
	  (read-length 0)
	  (read-opcode nil)
	  (write-length 0))
	 ()
  (:initable-instance-variables stream))
	  
(defconst *RECMAGIC* (format nil "RECORD STREAM VERSION 1~%"))

(defmethod (record-stream :after :init) (&rest ignored)
  (send stream :string-out *recmagic*)
  (send stream :force-output)
  (do ((j 0 (1+ j))
       (n (length *recmagic*)))
      ((= j n))
    (unless (= (aref *recmagic* j)
	       (or (send stream :tyi) (ferror nil "end of file reading record version")))
      (ferror nil "Character number ~D was bad in reading ~S" *recmagic*))))

(defmethod (record-stream :opcode) ()
  (or read-opcode
      (setq read-opcode
	    (block eof
	      (when (not (zerop read-length))
		(do ()
		    ((zerop read-length))
		  (or (send stream :tyi) (return-from eof nil))
		  (decf read-length)))
	      (let* ((opcode (or (send stream :tyi) (return-from eof nil)))
		     (c (or (send stream :tyi) (ferror nil "EOF after opcode")))
		     (c1 (or (send stream :tyi) (ferror nil "EOF after opcode"))))
		(setq read-length (logior c1 (ash c 8)))
		opcode)))))

(defmethod (record-stream :read-flush) ()
  (do ()
      ((zerop read-length))
    (send stream :tyi)
    (decf read-length))
  (setq read-opcode nil))

(defmethod (record-stream :length) ()
  read-length)

(defmethod (record-stream :read) (string length)
  (cond ((zerop read-length)
	 ())
	('else
	 (let ((n (send stream :string-in t string 0 (min length read-length))))
	   (decf read-length n)
	   (if (zerop read-length) (setq read-opcode nil))
	   read-length))))

(defmethod (record-stream :tyi) (&aux c)
  (cond ((zerop read-length)
	 ())
	((setq c (send stream :tyi))
	 (decf read-length)
	 (if (zerop read-length) (setq read-opcode nil))
	 c)
	('else
	 (ferror nil "record too short"))))


(defmethod (record-stream :start) (op length)
  (if (not (zerop write-length))
      (ferror nil "Unfinished output record"))
  (cond ((null op)
	 (send stream :eof)
	 ())
	('else
	 (send stream :tyo op)
	 (send stream :tyo (logand #o377 (ash length -8)))
	 (send stream :tyo (logand #o377 length))
	 (setq write-length length))))


(defmethod (record-stream :write) (op string length)
  (cond ((not (send self :start op length))
	 ())
	('else
	 (send stream :string-out string 0 length)
	 (setq write-length 0))))

(defmethod (record-stream :write-sub) (op string start length)
  (cond ((not (send self :start op length))
	 ())
	('else
	 (send stream :string-out string start (+ start length))
	 (setq write-length 0))))

(defmethod (record-stream :tyo) (c)
  (if (zerop write-length)
      (ferror nil "Uninitialized output record"))
  (send stream :tyo c)
  (decf write-length))


(defmethod (record-stream :force-output) ()
  (send stream :force-output))

