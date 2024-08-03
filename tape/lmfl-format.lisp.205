;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Support for the LMFL Format
;;;
;;; -dg 9/16/85
;;; -wbg 6/86
;;;

(defconstant *lmfl-default-record-size* 4096)

(defconstant *bytes-per-page* (* si:page-size 4))

(Defun Get-Next-Tape (message format device)
  (terpri) (princ message)
  (send device :rewind)
  (send device :unload)
  (prompt-for-new-tape format device)
  )

;;;
;;; LMFL format flavor definition.
;;;

(defflavor lmfl-format ((record-size)
			(file-stream)
			(tape-modified)
			(current-plist))
	   (basic-tape-format)
  (:gettable-instance-variables)
  (:settable-instance-variables current-plist))


(defmethod (lmfl-format :initialize) (&rest init-options)
  (check-attribute-list init-options)
  (unless record-size
    (setq record-size *lmfl-default-record-size*))
  (setq current-plist nil)
  (when init-options
    (lexpr-send self :set-options init-options)))


(defmethod (lmfl-format :set-options) (&rest options)
  (check-attribute-list options)
  (if options
      (do* ((l options (cddr l))
	    (option (car l) (car l))
	    (value (cadr l) (cadr l)))
	   ((null l))
	(case option
	  (:record-size
	   (check-type value (integer 1024 #.(* 64 1024)))
	   (check-arg value (zerop (remainder value 1024))
		      "a multiple of 1024 bytes")
	   (setq record-size value))
	  (t (signal 'invalid-option :object self :option option :value value))))
    (tv:choose-variable-values
      `((,(locf record-size) "Record Size" :number))
      :label '(:string "Options for the LMFL tape format" :font fonts:tr12b))))


(defmethod (lmfl-format :read-tape-header) (&rest ignore))
(defmethod (lmfl-format :write-tape-header) (&rest ignore))


(defmethod (lmfl-format :tape-is-your-format-p) (device)
  (check-device device)
  (send self :rewind device t)
  (prog1
    (using-resource (header-block si:dma-buffer (/ record-size *bytes-per-page*))
      (send device :read-block header-block record-size)
      (string-equal "LMFL" (si:dma-buffer-string header-block) :end2 4))
    (send self :rewind device)))


(defmethod (lmfl-format :space-to-end-of-this-file) (device plist records-passed)
  (check-device device)
  (check-plist plist)
  (check-type records-passed (or null (integer 0)))
  (do* ((total-records
	  (add1 (ceiling (if (get plist :partition)
			     (* (get plist :size) *bytes-per-page*)
			   (* (or (get plist :length-in-bytes)
				  (get plist :length)
				  (ferror nil "length-in-bytes is NIL!"))
			      (/ (file-byte-size plist) 8)))
			 record-size)))
	(passed records-passed)
	(records-to-space-over (- total-records passed) (- total-records passed)))
       ((condition-case (condition)
	    (progn
	     (Setq current-plist nil)
	     (condition-case ()
		 (select-processor
		  ((:lambda :cadr)
		   (send device :space records-to-space-over
			 (if (> (* records-to-space-over record-size)
				(send device :speed-threshold record-size))
			     :high
			   :low)))
		  (:explorer
		    (send device :search-filemark 1 :high))) ;;doesn't work if filemark is last thing on tape,
                                                             ;;or if physical-end-of-tape is where filemark would be.
	       (tape:filemark-encountered ))
	     t)
	  (physical-end-of-tape
	   (Let ((data-transferred (send condition :data-transferred)))
	     (if (= data-transferred (sub1 records-to-space-over))
		 ;;if end of tape is where filemark would be ignore it for now
		 t
	       (progn
		 (send self :find-continuation-tape device plist)
		 (incf passed data-transferred)
		 nil)))))))
  )


(defmethod (lmfl-format :read-file-header) (device &optional (host-for-parsing si:local-host))
  (check-device device)
  (check-host host-for-parsing)
  (If (Not (Null current-plist))
      current-plist
    (let ((*read-base* 10.))
      (using-resource (header-block si:dma-buffer (/ record-size *bytes-per-page*))
	(condition-case ()
	    (send device :read-block header-block record-size)
	  ((filemark-encountered physical-end-of-tape)
	   (signal 'logical-end-of-tape :device-object device))
	  (:no-error
	   (let ((string (si:dma-buffer-string header-block))
		 plist)
	     (unless (string-equal string "LMFL" :end1 4)
	       (signal 'bad-file-header
		       :format-type 'lmfl
		       :header string))
	     ;;All this is because file plists may have been written out in "random"
	     ;;(not carefully set) readtable:
	     (flet ((read-plist ()
		     (cond ((string-equal string "#!C" :start1 4 :end1 7)
			    (format t "[File header in T.I. format]")
			    (setq plist (read-from-string string nil :no-plist :start 7)))
			   (t
			    (setq plist (read-from-string string nil :no-plist :start 4))))))
	       (let ((*readtable* si:standard-readtable))
		 (condition-case (condition)
		     (read-plist)
		   (si:parse-error
		    (let ((*readtable* si:common-lisp-readtable))
		      (condition-case (condition)
			  (read-plist)
			(si:parse-error
			 (error "Unable to read file property list -- condition was:~&  ~S" condition)))))
		   (t))))
	     (cond ((atom (cdr plist))
		    (signal 'bad-file-header
			    :format-type 'lmfl
			    :header string))
		   (t
		    (setq plist (check-plist-validity plist))
		    (setq current-plist
			  (cons (when host-for-parsing
				  (fs:make-pathname
				    :host      host-for-parsing
				    :device    (getf plist :device)
				    :directory (getf plist :directory)
				    :name      (getf plist :name)
				    :type      (getf plist :type)
				    :version   (getf plist :version)))
				(dolist (elem '(:host :directory :device :name :type :version) plist)
				  (remf plist elem))))))))))))
  )


(defmethod (lmfl-format :restore-partition) (plist device silent)
  (check-plist plist)
  (check-device device)
  (let ((size (get plist :size))
	(comment (or (get plist :comment) (get plist :name))))
    (multiple-value-bind (host unit start ignore ignore name)
	(When (yes-or-no-p "Restore Partition ~s? " comment)
	  (partition-searcher (format nil "for writing partition ~a" comment) size
	     :confirm-write t :default-unit
	     #-CADR tframe:(tframe-default-disk-unit) #+CADR 0)) ;; +++
      (unwind-protect
	  (ZL:if (null host)
	      (progn 
		(format t "~&*** User Aborted restoring partition: ~s ***" comment)
		(send self :space-to-end-of-this-file device plist 0))
	    (si:update-partition-comment name "Incomplete Copy" unit)
	    (do ((first-block start)
		 (blocks size)
		 finished?)
		(finished?)
	      (Setq current-plist nil)
	      (condition-case (condition)
		  (send device :read-to-disk unit first-block blocks record-size :silent silent)
		(physical-end-of-tape
		 (Get-Next-Tape "Partition continued on another tape.  Unloading..." self device)
		 (Let ((dt (send condition :data-transferred)))
		   (incf first-block dt)
		   (decf blocks dt)))
		(:no-error
		 (si:update-partition-comment name (or (get plist :comment) "??? from tape") unit)
		 (condition-case (condition)
		     (send device :search-filemark 1 :high)
		   (physical-end-of-tape))
		 (setq finished? t)))))
	(when unit (si:dispose-of-unit unit))))))


(defmethod (lmfl-format :restore-file) (device &key transform
					(overwrite :never)
					query
					(create-directory :always)
					silent)
  (check-device device)
  (check-type transform (or string pathname compiled-function closure symbol))
  (check-type overwrite (member :query :never :always))
  (check-type create-directory (member :query :never :always :error))
  (let ((chunk-size (floor (send device :optimal-chunk-size record-size)
			   *bytes-per-page*))
	(plist (send self :read-file-header device)))
    (if (get plist :partition)
	(send self :restore-partition plist device silent)
      (let* ((byte-size (file-byte-size plist))
	     (length-in-bytes (or (get plist :length-in-bytes)
				  (get plist :length)))
	     (pathname (determine-restore-file-pathname
			 plist transform overwrite query create-directory silent)))
	(if (null pathname)
	    (send self :space-to-end-of-this-file device plist 0)
	  (with-open-file (outstream pathname
				     :direction :output
				     :byte-size byte-size
				     :characters (get plist :characters))
	    (let ((tmp-plist (copy-list plist))) ;keep plist around so that :find-continuation can use it
	      (dolist (prop '(:length-in-blocks :length-in-bytes :length
						:byte-size :not-backed-up :characters))
		(remprop tmp-plist prop))
	      (lexpr-send outstream :change-properties pathname (cdr tmp-plist)))
	    (using-resource (buffer si:dma-buffer chunk-size)
	      (do* ((chunk-size-in-bytes (* chunk-size si:page-size (/ 32 byte-size)))
		    (record-size-in-bytes (/ record-size (/ byte-size 8)))
		    (buffer-array (case byte-size
				    (8 (si:dma-buffer-string buffer))
				    (16 (si:dma-buffer-16b buffer))))
		    (bytes-to-go length-in-bytes)
		    (bytes-this-transfer (min bytes-to-go chunk-size-in-bytes)
					 (min bytes-to-go chunk-size-in-bytes)))
		   ((zerop bytes-to-go))
		(Setq current-plist nil)
		(condition-case (condition)
		    (send device :read-array
			  buffer-array
			  (ceiling bytes-this-transfer record-size-in-bytes)
			  record-size)
		  (physical-end-of-tape
		   (let* ((bytes-transferred (* (send condition :data-transferred) record-size-in-bytes))
			  (bytes-left (- bytes-this-transfer bytes-transferred)))
		     (send outstream :string-out buffer-array 0 bytes-transferred)
		     (setq tape-modified nil) ;; 
		     (send self :find-continuation-tape device plist)
		     (send device :read-array
			   buffer-array
			   (ceiling bytes-left record-size-in-bytes)
			   record-size)
		     (send outstream :string-out buffer-array 0 bytes-left)))
		   (:no-error
		     (send outstream :string-out buffer-array 0 bytes-this-transfer)))
		(decf bytes-to-go bytes-this-transfer)))
	    (Setq current-plist nil)
	    (condition-case (condition)
		(send device :space 1)
	      ((filemark-encountered physical-end-of-tape)
	       (setq tape-modified nil)))))))))


(defmethod (lmfl-format :write-file-header) (device truename attribute-list)
  (check-device device)
  (check-type truename pathname)
  (check-attribute-list attribute-list)
  (let* ((*print-base* 10.)
	 (*readtable* si:standard-readtable))
    (let* ((plist (cond ((getf attribute-list :partition)
		       attribute-list)
		      (t
		       (nconc (list :device (pathname-device truename)
				    :directory (pathname-directory truename)
				    :name (pathname-name truename)
				    :type (pathname-type truename)
				    :version (pathname-version truename))
			      (let ((x (copy-list attribute-list)))
				(dolist (elem '(:host :directory :device :name :type :version) x)
				  (remf x elem))
				x)))))
	   (string (format nil "LMFL~S" plist)))
      (using-resource (header-block si:dma-buffer (/ record-size *bytes-per-page*))
	(copy-array-contents string (si:dma-buffer-string header-block))
	(Setq current-plist nil)
	(setq tape-modified t)
	(send device :write-block header-block record-size)))))

(defmethod (lmfl-format :write-partition) (partition-name device unit-arg &key
					   silent end start)
  (check-type partition-name string)
  (check-type start (or null (integer 0)))
  (check-type end (or (integer 0) (member t nil)))
  (check-device device)
  (check-type unit-arg (or (integer 0) string closure))
  (si:with-decoded-disk-unit (unit unit-arg "for writing partition")
    (multiple-value-bind (beg length nil name)
	(si:find-disk-partition partition-name nil unit)
      (unless beg
	(ferror 'no-such-partition
		:host (unit-host unit)
		:disk-unit (unit-number unit)
		:partition partition-name))
      (setq start (or start beg)
	    end (cond ((null end)
		       (+ (or (si:measured-from-part-size unit name beg length) length) start))
		      ((integerp end) (+ start end))
		      (t (+ beg length))))
      (unless (and (< start end)
		   (>= start beg)
		   (<= end (+ beg length)))
	(ferror nil "Partition start or end specifications out of bounds."))
      (Setq current-plist nil)
      (using-resource (buffer si:dma-buffer (/ record-size *bytes-per-page*))
	(let ((*print-base* 10.)
	      (plist (list :partition t :name name :size (- end start)
			   :comment (si:partition-comment name unit)
			   :byte-size 16.
			   :host (send (unit-host unit) :name)
			   :host-unit (unit-number unit)
			   :creation-date (time:get-universal-time))))
	  (copy-array-contents
	    (format nil "LMFL~s" plist)
	    (si:dma-buffer-string buffer))
	  (with-device-locked device
	    (condition-case (condition)
		(send device :write-block buffer record-size)
	      (physical-end-of-tape
	       (setq tape-modified nil)
	       (Get-Next-Tape "End of tape while writing partition header. Unloading tape..." self device)
	       (send device :write-block buffer record-size)))))
	(do ((addr start)
	     (blocks-to-write (* (ceiling (- end start) 4.) 4)))
	    ((zerop blocks-to-write))
	  (with-device-locked device
	    (condition-case (condition)
		(progn
		 (setq tape-modified t)
		 (send device :write-from-disk
		       unit addr blocks-to-write record-size :silent silent))
	      (physical-end-of-tape
	       (setq tape-modified nil)
	       (Get-Next-Tape "End of tape while writing partition. Unloading tape..." self device)
	       (Let ((data-transferred (send condition :data-transferred)))
		 (Incf addr data-transferred)
		 (Decf blocks-to-write data-transferred)))
	      (:no-error
	       (setq blocks-to-write 0)
	       (condition-case ()
		   (send device :write-filemark)
		 (physical-end-of-tape))))))))))


(defmethod (lmfl-format :write-file) (device file &key (end-of-tape-action :continue) silent)
  (check-device device)
  (check-type file (or string pathname list))
  (check-type end-of-tape-action (member :continue :error :return))
  (let ((pathname (if (consp file) (fs:parse-pathname (car file)) (fs:parse-pathname file)))
	(properties (if (consp file) (cdr file))))
    (with-open-file (instream pathname :direction :input
			      :characters (or (getf properties :characters) :default))
      (unless properties
	(setq properties (send instream :plist)))
      (block write-file
	(unless silent
	  (format t "~&Writing file: ~a" pathname))
	(Setq current-plist nil)
	(let ((props (check-plist-validity (or properties (send instream :plist))))
	      (byte-factor (/ (file-byte-size instream) 8))
	      (chunk-size (floor (send device :optimal-chunk-size record-size) record-size))
	      (truename (send instream :truename))
	      number-of-records
	      last-record-fill)
	  ;; +++ kludge to indicate byte size actually used to write the file to tape +++
	  ;; +++ problem occurs on unknown file types -- confusion about byte size +++
	  (When (Null (Getf props :byte-size))
	    (nconc props (list :byte-size (file-byte-size instream))))
	  (condition-case (condition)
	      (send self :write-file-header device truename props)
	    (physical-end-of-tape
	     (ecase end-of-tape-action
	       (:error
		(signal 'end-of-tape-writing-header
		   :file-plist (cons nil props)
		   :device device))
	       (:continue
		(setq tape-modified nil)
		(Get-Next-Tape
		  "Physical end of tape.  Continue on next tape.  Unloading..." self device)
		(send self :write-file-header device truename props))
	       (:return
		(return-from write-file
		  (make-condition 'end-of-tape-writing-header
				  :file-plist (cons nil props)
				  :device device))))))
	  (using-resource (buffer si:dma-buffer (* chunk-size (/ record-size *bytes-per-page*)))
	    (multiple-value-bind (a b)
		(floor (* (send instream :length) byte-factor) record-size)
	      (setq number-of-records (if (zerop b) a (add1 a))
		    last-record-fill (/ (if (zerop b) record-size b) byte-factor)))
	    (do* ((record-count 0)
		  (rs (/ record-size byte-factor))
		  (records-this-pass (min (- number-of-records record-count) chunk-size)
				     (min (- number-of-records record-count) chunk-size))
		  (last-bunch (<= (- number-of-records record-count) chunk-size)
			      (<= (- number-of-records record-count) chunk-size))
		  (buffer-array (ecase byte-factor
				  (1 (si:dma-buffer-8b buffer))
				  (2 (si:dma-buffer-16b buffer))))
		  ;; Always use the 8b version of the buffer for writing!
		  (buffer-array-8b (si:dma-buffer-8b buffer)))
		 ((= record-count number-of-records)
		  (condition-case (condition)
		      (send device :write-filemark)
		    (physical-end-of-tape))
		  t)
	      (send instream :string-in nil buffer-array 0
		    (ZL:if (not last-bunch)
			(* records-this-pass rs)
		      ;; stupid format lossage
		      (array-initialize buffer-array 0
					(+ (* (sub1 records-this-pass) rs) last-record-fill)
					(* records-this-pass rs))
		      (+ (* (sub1 records-this-pass) rs) last-record-fill)))
	      (condition-case (condition)
		  (send device :write-array buffer-array-8b records-this-pass record-size)
		(physical-end-of-tape
		 (ecase end-of-tape-action
		   ((:error :return)
		    (let ((cond (make-condition 'end-of-tape-writing-file
				 :file-plist (cons nil props)
				 :device device
				 :bytes-transferred
				      (* (+ record-count (send condition :data-transferred)) rs))))
		      (case end-of-tape-action
			(:return (return-from write-file cond))
			(:error (signal cond)))))
		   (:continue
		    (setq tape-modified nil)
		    (Get-Next-Tape
		      "Physical end of tape encountered.  Continue on next tape.  Unloading..." self device)
		    (let ((records-written (send condition :data-transferred)))
		      (send self :write-file-header
			    device
			    (fs:parse-pathname "lm:continuation.file#0")
			    (let ((bytes-left (- (or (send-if-handles instream :length-in-bytes)
						     (send instream :length))
						 (* (+ record-count records-written) rs))))
			      (list :byte-size (file-byte-size props)
				    :length-in-bytes bytes-left
				    :length-in-blocks (ceiling (* bytes-left byte-factor)
							       *bytes-per-page*)
				    :continuation-properties props)))
		      (using-resource
			  (temp-buffer si:dma-buffer (* (- records-this-pass records-written)
							(/ record-size *bytes-per-page*)))
			(copy-array-portion
			  buffer-array (* records-written rs) (* records-this-pass rs)
			  (case byte-factor
			    (1 (si:dma-buffer-8b temp-buffer))
			    (2 (si:dma-buffer-16b temp-buffer)))
			  0
			  (* (- records-this-pass records-written) rs))
			(send device :write-array
			      (si:dma-buffer-8b temp-buffer)
			      (- records-this-pass records-written)
			      record-size))
		      (incf record-count records-this-pass)))))
		(:no-error
		 (incf record-count records-this-pass)))))))))
  )

;;; Need to do a better job than this. (properties may be in a different order, etc.)
;(defun continuation-properties-equal (plist cplist)
;  (equalp (get cplist :continuation-properties) plist))

(defun continuation-properties-equal (plist cplist)
  (do ((cplist2 (get cplist :continuation-properties) (cddr cplist2)))
      ((null cplist2) t)
    (unless (equalp (cadr cplist2) (get plist (car cplist2)))
      (return nil))))


(defmethod (lmfl-format :find-continuation-tape) (device plist)
  (check-device device)
  (check-plist plist)
  (tv:beep)
  (format t "~&Continued on another tape.  Unloading this tape...")
  (send device :rewind)
  (send device :unload)
  (do () (nil)
    (prompt-for-new-tape self device)
    (let ((cplist (send self :read-file-header device)))
      (if (continuation-properties-equal plist cplist)
	  (return t)
	(When (yes-or-no-p
		"The current tape does not appear to be a continuation of the last tape.  Use it anyway?")
	      (return t)))))
  )


(defmethod (lmfl-format :compare-partition) (device plist silent)
  (check-device device)
  (check-plist plist)
  (let ((part-disk-unit (get plist :host-unit))
	(part-host
	  (or (si:parse-host (get plist :host) t) si:local-host))
	(plist-name (get (Car plist) :name)))
    (multiple-value-bind (host unit start length ignore name)
	(partition-searcher
	  (format nil "for comparing ~s" (get plist :comment))
	  (get plist :size)
	  :default-partition (when (stringp plist-name) plist-name)
	  :default-unit (if (eq (si:parse-host part-host) si:local-host)
			    part-disk-unit
			  (format nil "~A ~D" part-host part-disk-unit))
	  :default-comment (get plist :comment))
      (if (null host)
	  (progn (format t "~&*** User aborted comparison of partition: ~a ***"
			 (or (get plist :comment) (get plist :name)))
		 (send self :space-to-end-of-this-file device plist 0))
	(do ((first start)
	     (blocks (or (si:measured-from-part-size unit name start length) length))
	     Result
	     finished?)
	    (finished? result)
	  (condition-case (condition)
	      (setq result (send device :compare-to-disk
				 unit first blocks record-size :silent silent))
	    (physical-end-of-tape
	     (Get-Next-Tape
	       "Partition continued on another tape.  Unloading this tape..." self device)
	     (Let ((data-transferred (send condition :data-transferred)))
	       (incf first data-transferred)
	       (decf blocks data-transferred)))
	    (:no-error
	     (setq finished? t)
	     (if result
		 (condition-case ()
		     (send device :space 1)
		   ((physical-end-of-tape filemark-encountered)
		    (signal 'logical-end-of-tape :device-object device)))
	       (do (finished?)
		   (finished?)
		 (condition-case (condition)
		     (send device :search-filemark 1 :high)
		   (physical-end-of-tape
		    (prompt-for-new-tape self device))
		   (:no-error (setq finished? t))))))))))))


(defmethod (lmfl-format :compare-file) (device &key transform silent (error-action :return))
  (check-device device)
  (let* ((pl (send self :read-file-header device))
	 (max-chunk (send device :optimal-chunk-size record-size))
	 (pathname (if transform
		       (process-transform transform pl)
		     (car pl)))
	 (length-in-bytes (or (get pl :length-in-bytes)
			      (get pl :length)
			      (get pl :size)	; for partitions
			      (ferror nil "length in bytes is NIL!")))
	 (byte-factor (/ (file-byte-size pl) 8))
	 number-of-chunks
	 last-chunk-size)
    (setq current-plist nil)
    (if (get pl :partition)
	(send self :compare-partition device pl silent)
      (if (not (condition-case (cond)
		   (probef pathname)
		 (fs:directory-not-found)))
	  (let ((cond (make-condition 'compare-source-not-found :source-file pathname)))
	    (send self :space-to-end-of-this-file device pl 0)
	    (case error-action
	      (:return
	       (unless silent
		 (warn (send cond :report nil)))
	       cond)
	      (:error (signal-condition cond))))
	(block really-compare
	  (multiple-value-bind (a b)
	      (floor (* length-in-bytes byte-factor) max-chunk)
	    (setq number-of-chunks (if (zerop b) a (add1 a))
		  last-chunk-size (if (zerop b) max-chunk b))
	    (with-open-file (f pathname
			       :direction :input
			       :characters :default)
	      (using-resource (fbuffer si:dma-buffer (/ max-chunk *bytes-per-page*))
		(using-resource (tbuffer si:dma-buffer (/ max-chunk *bytes-per-page*))
		  (unless silent
		    (format t "~&Comparing \"~a\" ... " pathname))
		  (unless (and (= length-in-bytes
				  (or (get f :length-in-bytes)
				      (get f :length)
				      (ferror nil "file's length in bytes is NIL!")))
			       (= (file-byte-size pl) (file-byte-size f))
			       (= (get pl :creation-date) (get f :creation-date))
			       (eq (get pl :characters) (get f :characters)))
		    (let ((cond (make-condition 'compare-source-changed
						:source-plist (cons (send f :truename)
								    (plist f))
						:file-plist pl)))
		      (unless silent
			(format t "[*** Not Compared ***]"))

		      (send self :space-to-end-of-this-file device pl 0)
		      (case error-action
			(:return (return-from really-compare cond))
			(:error (signal-condition cond)))))
		  (when (zerop length-in-bytes)
		    (Setq current-plist nil)
		    (condition-case (condition)
			(send device :space 1)
		      ((filemark-encountered physical-end-of-tape)))
		    (format t "[Zero Length]")
		    (return-from really-compare pl))
		  (do* ((count 0 (add1 count))
			(records-compared 0)
			(bytes-this-time	;note these are 8-bit bytes
			  (if (= count (sub1 number-of-chunks)) last-chunk-size max-chunk)
			  (if (= count (sub1 number-of-chunks)) last-chunk-size max-chunk))
			(farray (case byte-factor
				  (1 (si:dma-buffer-8b fbuffer))
				  (2 (si:dma-buffer-16b fbuffer))))
			(fstring (si:dma-buffer-string fbuffer))
			(tstring (si:dma-buffer-string tbuffer))
			unequalp)
		       ((or (= count number-of-chunks) unequalp)
			(ZL:if unequalp
			    (let ((cond (make-condition 'compare-error
							:source-file (send f :truename)
							:file-plist pl)))
			      (unless silent
				(format t "[*** Unequal ***]"))
			      (ecase error-action
				(:return
				 (send self :space-to-end-of-this-file device pl records-compared)
				 cond)
				(:error (signal-condition cond))))
			  (unless silent
			    (format t "[Equal]"))
			  (Setq current-plist nil)
			  (condition-case (condition)
			      (send device :space 1)
			    ((filemark-encountered physical-end-of-tape)))
			  pl))
		    (send f :string-in nil farray 0 (/ bytes-this-time byte-factor))
		    (Setq current-plist nil)
		    (condition-case (condition)
			(send device :read-array
			      tstring (ceiling bytes-this-time record-size) record-size)
		      (physical-end-of-tape
		       (let* ((records-read (send condition :data-transferred))
			      (bytes-left (- bytes-this-time (* records-read record-size))))
			 (send self :find-continuation-tape device pl)
			 (ZL:if (string-not-equal fstring tstring
					       :end1 (- bytes-this-time bytes-left)
					       :end2 (- bytes-this-time bytes-left))
			     (setq unequalp t
				   records-compared (+ records-compared records-read))
			   (send device :read-array
				 tstring (ceiling bytes-left record-size) record-size)
			   (unless (string-equal fstring tstring
						 :Start1 (- bytes-this-time bytes-left)
						 :end1 bytes-this-time
						 :end2 bytes-left)
			     (setq unequalp t))
			   (incf records-compared (ceiling bytes-this-time record-size)))))
		      (:no-error
		       (unless (string-equal fstring tstring
					     :start1 0
					     :end1 bytes-this-time
					     :Start2 0
					     :end2 bytes-this-time)
			 (setq unequalp t
			       records-compared
			       (ceiling bytes-this-time record-size)))))))))))))))


(defmethod (lmfl-format :beginning-of-file) (device)
  (check-device device)
  (When (Null current-plist)
    (condition-case ()
	(send device :search-filemark-reverse 1 :high)
      (physical-beginning-of-tape)))
  )


(defmethod (lmfl-format :next-file) (device &optional (nfiles 1))
  (check-device device)
  (check-type nfiles (integer 1))
  (Setq current-plist nil)
  (dotimes (c nfiles) 
    (send device :search-filemark 1 :high)))


(defmethod (lmfl-format :previous-file) (device &optional (nfiles 1))
  (check-device device)
  (check-type nfiles (integer 1))
  (send self :beginning-of-file device)
  (dotimes (times nfiles)
    (send device :space-reverse 1)
    (send device :search-filemark-reverse 1 :high)))


(defmethod (lmfl-format :find-file) (device match)
  (check-device device)
  (check-type match (or list compiled-function closure symbol string pathname))
  (do ((pl (send self :read-file-header device)
	   (send self :read-file-header device)))
      ((tape-file-match match pl)
       pl)
    (send self :space-to-end-of-this-file device pl 0)))


(defmethod (lmfl-format :find-file-reverse) (device match)
  (check-device device)
  (check-type match (or list compiled-function closure symbol string pathname))
  (send self :beginning-of-file device)
  (send self :previous-file device)
  (do ((pl (send self :read-file-header device)
	   (send self :read-file-header device)))
      ((tape-file-match match pl)
       pl)
    (send self :beginning-of-file device)
    (send self :previous-file device)))


(defmethod (lmfl-format :open-file) (device &key
				     (direction :input)
				     (byte-size :default)
				     (characters :default)
				     plist)
  (check-device device)
  (check-type direction (member :input :output))
  (check-type byte-size (member 8 16 :default))
  (check-type characters (member :default t nil))
  (when (eq direction :output)
    (check-plist plist))
  (when file-stream
    (case (send file-stream :status)
      ((:bof :closed))
      (t (close file-stream :abort t))))
  (when (and (eq direction :output) (null plist))
    (signal 'protocol-violation
	    :format-string "LMFL Output stream must have a plist."))
  (let* ((pl (if (eq direction :input) (send self :read-file-header device) plist))
	 (*characters (if (eq characters :default)
			  (get pl :characters)
			(setf (get plist :characters) characters)))
	 (*byte-size (if (eq byte-size :default)
			 (file-byte-size pl)
		       (setf (get pl :byte-size) byte-size))))
    (send device :lock-device)
    (when (eq direction :output)
      (send self :write-file-header device (car pl) (cdr pl)))
    (setq tape-modified (eq direction :output)
	  file-stream (make-instance (case direction
				       (:input (if (not *characters)
						   'lmfl-input-stream
						 'lmfl-input-character-stream))
				       (:output (if (not *characters)
						    'lmfl-output-stream
						  'lmfl-output-character-stream)))
				     :device device
				     :byte-size *byte-size
				     :record-size record-size
				     :format self
				     :pathname (car pl)
				     :property-list (cdr pl)))))
		 
(defmethod (lmfl-format :list-files) (device &key (stream *standard-output*) (number-of-files -1))
  (check-device device)
  (check-type number-of-files (integer))
  (let (list)
    (condition-case ()
	(do (plist
	     byte-size
	     (count 0 (add1 count)))
	    ((= count number-of-files) (reverse list))
	  (setq plist (send self :read-file-header device)
		byte-size (file-byte-size plist))
	  (push plist list)
	  (when stream
	    (if (get plist :partition)
		(format stream "~&Partition: \"~A\" - Length in Blocks: ~D"
			(or (get plist :comment) "Unknown")
			(get plist :size))
	      (format stream "~&~A ~50TByte Size: ~D ~65T- Length in bytes: ~D"
		      (car plist)
		      byte-size
		      (or (get plist :length-in-bytes)
			  (get plist :length)))))
	  (send self :space-to-end-of-this-file device plist 0))
      (logical-end-of-tape
       (Condition-Case ()
	   (send device :space-reverse 1)
	 (driver-error))
       (reverse list)))))

(defmethod (lmfl-format :finish-tape) (device)
  (check-device device)
  (when tape-modified
    (condition-case ()
	(send device :write-filemark)
      (physical-end-of-tape)
      (:no-error
       (Condition-Case ()
	   (send device :space-reverse 1)
	 (driver-error))))
    (setq tape-modified nil)))

(defmethod (lmfl-format :rewind) (device &optional (wait-p t))
  (when file-stream
    (close file-stream :abort t)
    (setq file-stream nil))
  (when tape-modified
    (case (prompt-for-rewind-with-state)
      (:resume (setq tape-modified nil))
      (:save-state (send self :finish-tape device))
      (:enter-debugger
       (ferror nil "Tape state not saved (debug request by user)"))))
  (Setq current-plist nil)
  (send device :rewind wait-p))

(defmethod (lmfl-format :unload) (device)
  (when file-stream
    (close file-stream :abort t)
    (setq file-stream nil))
  (when tape-modified
    (case (prompt-for-rewind-with-state)
      (:resume (setq tape-modified nil))
      (:save-state (send self :finish-tape device))
      (:enter-debugger
       (ferror nil "Tape state not saved (debug request by user)"))))
  (Setq current-plist nil)
  (send device :unload))

(defmethod (lmfl-format :position-to-append) (device)
  (check-device device)
  (Setq current-plist nil)
  (send device :search-filemark 2 :high)
  (send device :space-reverse 1))

(compile-flavor-methods lmfl-format)

(define-tape-format lmfl-format "lmfl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LMFL File streams
;;;

(defflavor lmfl-input-mixin () (tape-stream-mixin))

(defmethod (lmfl-input-mixin :close) (&optional abort-p)
  (send format :set-current-plist nil)
  (unwind-protect
      (or abort-p
	  (memq status '(:eof :closed))
	  (send device :search-filemark 1))
    (setq status :closed)
    (when dma-buffer
      (deallocate-resource 'si:dma-buffer dma-buffer))
    (setq dma-buffer nil
	  io-buffer nil)
    (send device :unlock-device)))

(defflavor lmfl-input-character-stream ()
	   (lmfl-input-mixin si:buffered-input-character-stream))

(defflavor lmfl-input-stream ()
	   (lmfl-input-mixin si:buffered-input-stream))

(compile-flavor-methods lmfl-input-character-stream lmfl-input-stream)

;;;;;;;;;;;;;;;;;;;;

(defflavor lmfl-output-mixin () (tape-stream-mixin))

(defmethod (lmfl-output-mixin :close) (&optional abort-p)
  (unless (eq status :closed)
    (setq status :closed)
    (unwind-protect
	(unless abort-p
	  (send self :force-output)
	  (send device :write-filemark))
      (when dma-buffer
	(deallocate-resource 'si:dma-buffer dma-buffer))
      (setq dma-buffer nil
	    io-buffer nil)
      (send device :unlock-device))))

(defflavor lmfl-output-stream ()
	   (lmfl-output-mixin si:buffered-output-stream))

(defflavor lmfl-output-character-stream ()
	   (lmfl-output-mixin si:buffered-output-character-stream))

(compile-flavor-methods lmfl-output-stream lmfl-output-character-stream)
