;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 17.1
;;; Reason:
;;;  TAPE: Fix continuation of QFASL's.
;;; Written 2-Jun-87 15:59:10 by robert (Robert Putnam) at site LMI Cambridge
;;; while running on Fish food from band 3
;;; with Experimental System 122.7, Experimental Local-File 73.0, Experimental FILE-Server 22.0, Experimental Unix-Interface 11.0, Experimental Tape 17.0, Experimental Tiger 26.0, Experimental KERMIT 33.0, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, microcode 1742, SDU Boot Tape 3.14, SDU ROM 102.



; From modified file DJ: L.TAPE; LMFL-FORMAT.LISP#197 at 2-Jun-87 15:59:18
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; LMFL-FORMAT  "

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

))

; From modified file DJ: L.TAPE; LMFL-FORMAT.LISP#197 at 2-Jun-87 16:00:00
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; LMFL-FORMAT  "

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
				  (2 (si:dma-buffer-16b buffer)))))
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
		  (send device :write-array buffer-array records-this-pass record-size)
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

))
