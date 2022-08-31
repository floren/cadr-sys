;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for FILE-Server version 8.3
;;; Written 17-Jan-84 07:36:08 by Mly,
;;; while running on Lisp Machine Eighteen from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.29, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.0, Experimental FILE-Server 8.0, microcode 306, ZM MIT.


; From file SERVER.LISP PS:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun rfile-server ()
  (if (and server-login
	   (or (null user-id)
	       (zerop (string-length user-id))))
      (trap-lossage (error "Server Login")
	  (progn
	    (login server-login-id si:local-host)
	    (print-server-login-exegesis))))
  (let (tid conn-stream conn alldatas server-openings
	(server-instance (gensym))		;bind em all local....
	(user-id server-login-id)
	(server-protocol-version server-protocol-version)
	(fs:*local-server-via-net* nil))
    (unwind-protect
      (trap-lossage (error "Server Top Level")
	  (*catch 'server-chaos-disappear
	    (setq conn (chaos:listen "FILE"))
	    (when (chaos:symbolics-connection-p conn t)
	      (chaos:symbolics-reject (prog1 conn (setq conn nil)))
	      (ferror nil "Symbolics tried to come in."))
	    (when *lmfs-server-dont-answer-logins*
	      (chaos:reject (prog1 conn (setq conn nil))
			    *lmfs-server-dont-answer-logins*)
	      (ferror nil *lmfs-server-dont-answer-logins*))
	    (let* ((pkt (chaos:read-pkts conn))	;s/b rfc
		   (result (server-parse-rfc pkt)))
	      (cond ((fixp result)
		     (setq server-protocol-version result))
		    (t (chaos:reject (prog1 conn (setq conn nil)) result)
		       (ferror nil result))))
	    (chaos:accept conn)
	    (funcall tv:who-line-file-state-sheet
		     ':add-server conn "FILE" si:current-process
		     'lmfs-peek-server (process-stack-group si:current-process))
	    (setq conn-stream (chaos:make-stream conn))
	    (if *server-shutdown-message* (send-single-shutdown-message conn))
	    (error-restart-loop ((sys:abort error) "Return to server command-reading loop.")
	      (let (pkt op)
		(setq pkt (trap-lossage (error "Server Reading packets")
			      (condition-bind
				((sys:host-stopped-responding
				   #'(lambda (&rest ignore)
				       (*throw 'server-chaos-disappear nil)))
				 (sys:connection-lost
				   #'(lambda (&rest ignore)
				       (*throw 'server-chaos-disappear nil))))
				(chaos:get-next-pkt conn))
			    (ferror 'server-control-conn-network-lossage
				    "Control connection lost")))
		(setq op (chaos:pkt-opcode pkt))
		(cond ((or (= op chaos:eof-op)
			   (= op chaos:cls-op))
		       (funcall conn-stream ':force-output)
		       (chaos:return-pkt pkt)
		       (*throw 'server-chaos-disappear nil))
		      ((not (= op chaos:dat-op))
		       (ferror nil "Unrecognized packet opcode: ~S" op)))
		(let* ((string (chaos:pkt-string pkt))
		       (strings (get-strings-from-pktstring string)))	;nl-delimited strings
		  
		  (if trace-server-enabled
		      (without-interrupts (push (string-append string) server-traces)))
		  (destructuring-bind (tid fh cmd . rest) (parse-cmd-string string)
		    (if *lmfs-server-dont-answer-logins*
			(format conn-stream "~A ~A ERROR HNA F Host not available - ~A "
				tid (or fh "")
				*lmfs-server-dont-answer-logins*)
		      (selectq cmd
			(:login (setq user-id (file-server-login rest)))
			(:open   (file-server-open fh rest (car strings)))
			(:open-for-lispm
			 (lexpr-funcall 'file-server-open-for-lispm
					fh (car strings)
					(let ((base 10.) (ibase 10.)
					      (package si:pkg-user-package)
					      (readtable si:initial-readtable))
					  (read-from-string
					    string nil
					    (+ (string-search-char #\cr string)
					       (length (car strings))
					       2)))))
			(:extended-command
			 (lexpr-funcall 'file-server-extended-command
					fh (car rest) (car strings)
					(let ((base 10.) (ibase 10.)
					      (package si:pkg-user-package)
					      (readtable si:initial-readtable))
					  (read-from-string
					    string nil
					    (+ (string-search-char #\cr string)
					       (length (car strings))
					       2)))))
			(:data-connection (file-server-data-connection fh rest))
			(:undata-connection (file-server-undata-connection fh))
			(:close (file-server-close-connection fh))
			(:filepos (file-server-filepos fh rest))
			(:delete (file-server-delete fh strings))
			(:rename (file-server-rename fh strings))
			(:expunge (file-server-expunge fh strings))
			(:complete (file-server-complete fh rest strings))
			(:continue (file-server-continue fh))
			(:directory (file-server-directory fh rest strings))
			(:change-properties (file-server-change-props fh strings))
			(:create-directory (file-server-create-directory fh strings))
			(:create-link (file-server-create-link fh strings))
			(otherwise (format conn-stream "~A ~A ERROR UKC F Unknown command: ~A"
					   tid (or fh "") cmd)))))
		  (funcall conn-stream ':force-output)
		  (chaos:return-pkt pkt))))))
      (cond (conn
	     (funcall tv:who-line-file-state-sheet ':delete-server conn)
	     (trap-lossage (error "Server Top Level close")
	       (chaos:close-conn conn
			    (or *lmfs-server-dont-answer-logins*
				"Server error")))
	     (chaos:remove-conn conn)))
      (if server-openings
	  (trap-lossage (error "Server finish closing remaining openings")
	      (dolist (opening server-openings)
		(funcall opening ':close ':abort))))
      (trap-lossage (error "Closeout undata")
		    (dolist (data alldatas)
		      (rplaca (server-dataproc-comm-cell
				(get (server-dataproc-comm-sibling data) server-instance))
			      'undata)
		      (rplaca (server-dataproc-comm-cell data) 'undata)
		      (chaos:remove-conn (server-dataproc-comm-conn data)))))))

))

; From file SERVER.LISP PS:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun parse-cmd-string (string &aux answers)
  (let ((nlx (string-search-char #\CR string)))
    (do ((start 0) (lim (or nlx (string-length string))))
	(( start lim) (nreverse answers))
      (if (char-equal (aref string start) #\SP)
	  (progn
	    (push nil answers)
	    (incf start))
	  (let ((endx (or (string-search-char #\SP string start lim) lim)))
	    (push (or (and (loop for x from start below endx finally (return t)
				 unless (memq (aref string x)
					      '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9))
				 return nil)	;parse number is a dead bear
			   (parse-number string start endx))
		      (si:intern1 (substring string start endx) si:pkg-keyword-package))
		  answers)
	    (setq start (1+ endx)))))))

))

; From file SERVER.LISP PS:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-open (fh rest filename &aux answer binp
			 (characters t) direction directionp
			 if-exists if-does-not-exist (byte-size ':default)
			 deleted preserve-dates inhibit-links)
  (let ((losep
	  (*catch 'open-opt-lost
	    (progn
	      (loop for olist on rest
		    do
		    (let ((opt (car olist)))
		      (selectq opt
			(:binary (setq characters nil))
			(:character (setq characters t))
			(:default (setq characters ':default))
			(:read (setq direction ':input directionp t))
			(:write (setq direction ':output directionp t))
			(:probe (setq direction nil directionp t))
			(:probe-directory
			 (setq direction ':probe-directory directionp t))
			(:probe-link
			 (setq direction nil directionp t inhibit-links t))
			(:inhibit-links
			 (setq inhibit-links t))
			((:temporary :raw :super-image))
			(:deleted (setq deleted t))
			(:preserve-dates (setq preserve-dates t))
			(:byte-size
			 (setq byte-size (cadr olist))
			 (pop olist))
			(:if-exists (setq if-exists (cadr olist))
				    (pop olist))
			(:if-does-not-exist (setq if-does-not-exist (cadr olist))
					    (pop olist))
			(t (open-err "UOO F Unknown option: " opt)))))
	      (if (null fh)
		  (if (memq direction '(:input :output))
		      (open-err "ICO F Inconsistent open options for probe opening"))
		;; FHN given. must be real read or write.
		(let* ((comdata (get fh server-instance))
		       (type (selectq (server-dataproc-comm-iotype comdata)
			       (input ':input)
			       (output ':output))))
		    (if (null comdata)
			(open-err "UFH F No open data channel for this file handle: " fh))
		    (if directionp
			(unless (eq direction type)
			  (open-err "ICO F File handle type inconsistent with open mode."))
		      (setq direction type))))
	      (let ((pathname (lmfs-parse-for-server filename)))
		(if (errorp pathname) (open-err "IPS F Bad filename syntax: " pathname))
		(let ((opening
			(open pathname
			      ':direction direction			      
			      ':characters characters
			      ':if-does-not-exist (or if-does-not-exist
						      (selectq direction
							((:input nil) ':error)
							(:output ':create)))
			      ':if-exists (or if-exists
					      (if (memq (pathname-version pathname)
							'(:unspecific :newest))
						  ':new-version ':supersede))
			      ':error nil
			      ':inhibit-links inhibit-links
			      ':deleted deleted
			      ':preserve-dates preserve-dates
			      ':byte-size byte-size)))
		  (if (errorp opening) (*throw 'open-opt-lost (lmfs-error-string opening)))
		  (setq binp
			(selectq characters
			  (:default (not (funcall opening ':characters)))
			  (t (not characters))))
		  (setq answer
			(selectq server-protocol-version
			  (0  
			   (format nil
				   "~D ~A ~D ~S~%~A~%"
				   (funcall (funcall opening ':truename) ':version)
				   (cv-time (funcall opening ':creation-date))
				   (funcall opening ':length)
				   (funcall opening ':send-if-handles ':qfaslp)
				   (server-print-pathname (funcall opening ':truename))))
			  (1
			   (format nil
				   "~A ~D ~S ~S~%~A~%"
				   (time:print-universal-time
				     (funcall opening ':creation-date) nil)
				   (funcall opening ':length)
				   binp		;qfaslp, needed for compatibility
				   (not binp)
				   (server-print-pathname
				     (funcall opening ':truename))))))
		  (if (null direction)
		      (funcall opening ':close)
		    (let ((servi (get fh server-instance)))
		      (push opening server-openings)
		      (setf (server-dataproc-comm-binp servi) binp)
		      (setf (server-dataproc-comm-tid servi) tid)
		      (setf (server-dataproc-comm-opening servi) opening)
		      (rplaca (server-dataproc-comm-cell servi)
			      (if (eq direction ':input) 'read 'write))))
		  nil))))))
    (if (null losep)
	(format conn-stream  "~A ~A OPEN ~A" tid (or fh "") answer)
	(format conn-stream  "~A ~A ERROR ~A" tid (or fh "") losep))))

))

; From file SERVER.LISP PS:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-extended-command (fh command pathname &rest args &aux target)
  ;; Either FH or PATHNAME, but not both, should be non-nil.
  (unless
    ;; This returns t if pathname or stream is not suitable
    (cond ((null fh)
	   (setq target (lmfs-parse-for-server pathname))
	   (when (errorp pathname)
	     (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
	     t))
	  (t
	   (let* ((data (get fh server-instance))
		  (opening (server-dataproc-comm-opening data)))
	     (setq target opening)
	     (when (or (null data)
		       (null opening)
		       (symbolp opening))	;yes, I know NIL is a symbol, thx
	       (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
		       tid fh fh)
	       t))))
    (condition-case (results)
	(multiple-value-list
	 (lexpr-funcall target command args))
      (error
       (format conn-stream "~A ~A ERROR ~A"
	       tid (or fh "") (lmfs-error-string (car results))))
      (:no-error
       (format conn-stream "~A ~A ~A~%"
	       tid (or fh "") command)
       (let ((base 10.) (ibase 10.)
	     (readtable si:initial-readtable)
	     (package si:pkg-user-package))
	 (prin1 results conn-stream))))))

))
