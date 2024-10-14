;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for File-Server version 23.1
;;; Reason:
;;;  Aid server debugging -- store away HOST that originated connection in
;;;  server lossage.  (fs:print-server-lossages) will display it.
;;; Written 27-May-88 17:18:38 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 2
;;; with Experimental System 124.10, Experimental Local-File 74.0, Experimental File-Server 23.0, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.0, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:19:30
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defmacro trap-lossage ((condition-names id) code &body errors)
  (declare (zwei:indentation 0 7 1 3 2 1))
  `(condition-case-if (not lmfs-debug-server) (trap-error)
       ,code
     (,condition-names
      (tv:notify nil "File server got an error.")
      (push (list (cv-time (time:get-universal-time))
		  ',id
		  (send trap-error :report-string)
		  (and (boundp 'conn)
		       conn
		       (si:get-host-from-address (chaos:foreign-address conn) :chaos)))
	    lmfs-server-lossages)
      . ,errors)))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:19:45
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun print-server-lossages (&optional toggle-debug-switch?)
  "Print out server lossage info to *standard-output*.
If toggle-debug-switch? is non-nil, debugging is toggled on or off."
  (cond (toggle-debug-switch?
	 (setq lmfs-debug-server (not lmfs-debug-server))
	 (format t "~&Debugging of server turned ~A" (if lmfs-debug-server "ON" "OFF"))))
  (if (null lmfs-server-lossages)
      (format t "~&No lossages.")
    (loop for (time key err-msg-info host) in lmfs-server-lossages
	  doing
	  (format t "~&~A  ~A~@[  ~S~]~%~5T~S" time key host err-msg-info)
	  (when (send *standard-input* :listen)
	    (let ((c (send *standard-input* :tyi)))
	      (or (= c #/sp) (send *standard-input* :untyi c)))
	    (return nil)))))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:19:55
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

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
	    (catch 'server-chaos-disappear
	      (setq conn (chaos:listen "FILE"))
	      (when (chaos:unwanted-connection-p conn)
		(chaos:unwanted-reject (prog1 conn (setq conn nil)))
		(ferror nil "unwanted connection tried to come in."))
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
	      (send tv:who-line-file-state-sheet
		    :add-server conn "FILE" si:current-process
		    'lmfs-peek-server (process-stack-group si:current-process))
	      (setq conn-stream (chaos:make-stream conn))
	      (if *server-shutdown-message* (send-single-shutdown-message conn))
	      (error-restart-loop ((sys:abort error) "Return to server command-reading loop.")
		(let (pkt op)
		  (setq pkt (trap-lossage (error "Server Reading packets")
				(condition-bind
				  ((sys:host-stopped-responding
				     #'(lambda (&rest ignore)
					 (throw 'server-chaos-disappear nil)))
				   (sys:connection-lost
				     #'(lambda (&rest ignore)
					 (throw 'server-chaos-disappear nil))))
				  (chaos:get-next-pkt conn))
			      (ferror 'server-control-conn-network-lossage
				      "Control connection lost")))
		  (setq op (chaos:pkt-opcode pkt))
		  (cond ((or (= op chaos:eof-op)
			     (= op chaos:cls-op))
			 (send conn-stream :force-output)
			 (chaos:return-pkt pkt)
			 (throw 'server-chaos-disappear nil))
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
			   (apply 'file-server-open-for-lispm
				  fh (car strings)
				  (let ((*read-base* 10.) (*print-base* 10.)
					(*package* si:pkg-user-package)
					(*readtable* si:initial-readtable))
				    (read-from-string
				      string nil
				      (+ (string-search-char #/cr string)
					 (length (car strings))
					 2)))))
			  (:extended-command
			   (apply 'file-server-extended-command
				  fh (car rest) (car strings)
				  (let ((*read-base* 10.) (*print-base* 10.)
					(*package* si:pkg-user-package)
					(*readtable* si:initial-readtable))
				    (read-from-string
				      string nil
				      (+ (string-search-char #/cr string)
					 (length (car strings))
					 2)))))
			  (:data-connection (file-server-data-connection fh rest))
			  (:moby-connection (file-server-moby-connection fh rest))
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
		    (send conn-stream :force-output)
		    (chaos:return-pkt pkt))))))
      (when conn
	(send tv:who-line-file-state-sheet :delete-server conn)
	(trap-lossage (error "Server Top Level close")
	    (chaos:close-conn conn
			      (or *lmfs-server-dont-answer-logins*
				  "Server error")))
	(chaos:remove-conn conn))
      (if server-openings
	  (trap-lossage (error "Server finish closing remaining openings")
	      (dolist (opening server-openings)
		(send opening :close :abort))))
      (trap-lossage (error "Closeout undata")
	  (dolist (data alldatas)
	    (rplaca (server-dataproc-comm-cell
		      (get (server-dataproc-comm-sibling data) server-instance))
		    'undata)
	    (rplaca (server-dataproc-comm-cell data) 'undata)
	    (chaos:remove-conn (server-dataproc-comm-conn data)))))))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:20:10
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-data-top-level (server-instance cell handle)
  (let ((fs:*local-server-via-net* nil))
    (declare (special fs:*local-server-via-net*))
    (trap-lossage (error "File Server Data Connection")
	(do-forever
	  (process-wait "Data Conn Cmd" #'car cell)
	  (let* ((data (get handle server-instance))
		 (celloc (locf (car cell)))
		 (opening (server-dataproc-comm-opening data))
		 (dconn (server-dataproc-comm-conn data))
		 (binp (server-dataproc-comm-binp data)))
	    (selectq (car cell)
	      (undata				;Gute Nacht, O Wesen.
	       (rplaca cell nil)
	       (return nil))
	      
	      ((fpsync wsync)
	       (send-sync-mark dconn)
	       (rplaca cell nil))
	      
	      (directory
	       (server-dataproc-hack-directory data handle)
	       (%store-conditional celloc 'directory nil))
	      (write
	       (if (null opening) (ferror nil "file-server-data-top-level - no opening"))
	       (condition-bind ((no-more-room
				  (let-closed ((server-instance server-instance)
					       (cell1 cell)
					       (handle1 handle))
				    'server-disk-full-handler)))
		 (*catch 'async-abort
		   (do-forever
		     (if (not (eq (car cell) 'write)) (return nil))
		     (let* ((pkt (if (server-window-write-check cell dconn 'write)
				     (chaos:get-next-pkt dconn)
				   (return nil))))
		       (select (chaos:pkt-opcode pkt)
			 (chaos:eof-op
			  (chaos:return-pkt pkt)
			  (setq pkt (if (server-window-write-check cell dconn 'write)
					(chaos:get-next-pkt dconn)
				      (return nil)))
			  (or (= (chaos:pkt-opcode pkt) fs:%qfile-synchronous-mark-opcode)
			      (ferror "Unrecognized Opcode in data server: ~O"
				      (chaos:pkt-opcode pkt)))
			  (chaos:return-pkt pkt)
			  (%store-conditional celloc 'write nil)
			  (return nil))
			 (fs:%qfile-synchronous-mark-opcode
			  (chaos:return-pkt pkt)
			  (%store-conditional celloc 'write nil)
			  (return nil))
			 (fs:%qfile-binary-opcode
			  (unwind-protect
			      (send opening :string-out pkt chaos:first-data-word-in-pkt
				    (+ (truncate (chaos:pkt-nbytes pkt) 2)
				       chaos:first-data-word-in-pkt))
			    (chaos:return-pkt pkt)))
			 (fs:%qfile-character-opcode
			  (unwind-protect
			      (send opening :string-out (chaos:pkt-string pkt)
				    0 (chaos:pkt-nbytes pkt))
			    (chaos:return-pkt pkt)))
			 (otherwise (ferror nil "Unknown pkt opcode: ~O" (chaos:pkt-opcode pkt)))))))))
	      (read
	       (if (null opening) (ferror nil "file-server-data-top-level - no opening"))
	       (do (last eofp) (())
		 (if (server-window-read-check cell dconn) (return nil))
		 (let ((pkt (chaos:get-pkt)))
		   (cond (binp
			  (multiple-value (last eofp)
			    (send opening :string-in nil pkt
				  chaos:first-data-word-in-pkt chaos:max-data-words-per-pkt))
			  (setf (chaos:pkt-opcode pkt) fs:%qfile-binary-opcode)
			  (setf (chaos:pkt-nbytes pkt)
				(* 2 (- last chaos:first-data-word-in-pkt))))
			 (t (multiple-value (last eofp)
			      (send opening :string-in nil (chaos:pkt-string pkt)
				    0 chaos:max-data-bytes-per-pkt))
			    (setf (chaos:pkt-opcode pkt) fs:%qfile-character-opcode)
			    (setf (chaos:pkt-nbytes pkt) last)))
		   (if (plusp (chaos:pkt-nbytes pkt))
		       (chaos:send-pkt dconn pkt (chaos:pkt-opcode pkt))	;don't let SEND dft it
		     (chaos:return-pkt pkt))
		   (cond (eofp
			  (if (server-window-read-check cell dconn) (return nil))
			  (chaos:send-pkt dconn (chaos:get-pkt) chaos:eof-op)
			  (%store-conditional celloc 'read nil)
			  (return nil))))))
	      (moby-server
	       (server-dataproc-hack-moby data handle dconn cell)
	       (%store-conditional celloc 'moby-server nil))
	      (t (ferror nil "Bogus com-cell value: ~S" (car cell)))))))))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:20:20
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun server-dataproc-hack-directory
       (data handle &aux ok (conn (server-dataproc-comm-conn data))
			    (cell (server-dataproc-comm-cell data)))
  (trap-lossage (error "Directory lister toplevel")
     (let* ((conn-stream (chaos:make-stream conn))
	    (arg (server-dataproc-comm-arg data))
	    (path (car arg))
	    (opts (cdr arg)))
       (let ((dirlist (send path :directory-list (cons :noerror opts)))
	     (gopkt (chaos:get-pkt))
	     (dinfo (server-dataproc-comm-dinfo data)))
	 (cond ((errorp dirlist)
		(chaos:set-pkt-string gopkt (car dinfo)
				      "ERROR " (lmfs-error-string dirlist)))
	       (t
		(chaos:set-pkt-string gopkt (car dinfo) "DIRECTORY")))
	 (chaos:send-pkt (cdr dinfo) gopkt)
	 (cond ((not (errorp dirlist))
		(server-dirlist-single (cdar dirlist) nil conn-stream)
		(dolist (file (cdr dirlist))
		  (if (server-window-read-check cell conn 'directory) (return nil))
		  (server-dirlist-single (cdr file) (car file) conn-stream))
		(send conn-stream :tyo #/cr)
		(setq ok t))))
       (send conn-stream :force-output)
       (if ok (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op)))
    (send-data-async-lossage conn "System error during dir list processing" handle)))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:20:32
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun server-dataproc-hack-moby
       (data handle conn cell)  data
  (trap-lossage (error "Moby server toplevel")
   (moby-server-toplevel cell conn)	;defined in moby stuff.
   (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op))
  (send-data-async-lossage conn "System error during moby serving" handle))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:20:38
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-change-props (fh strings)
  (trap-lossage (error "Change properties toplevel")
      (cond ((null fh)
	     (if (not (> (length strings) 0))
		 (format conn-stream "~A  ERROR IRF F No pathname given." tid)
	       (let ((path (lmfs-parse-for-server (car strings))))
		 (if (errorp path)
		     (format conn-stream "~A  ERROR IPS F Syntax error in supplied path: ~A"
			     tid)
		   (change-props-1 path "" (cdr strings))))))
	    (t (let ((data (get fh server-instance)))
		 (if (or (null data) (null (server-dataproc-comm-opening data)))
		     (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
			     tid fh fh)
		   (change-props-1 (server-dataproc-comm-opening data) fh strings)))))
    (format conn-stream "~A ~A ERROR SYS F Internal error:~% ~A" tid (or fh "") trap-error)))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:20:44
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun change-props-1 (actor fh strings)
  (loop with sym
	for string in strings
	as spacex = (string-search-char #/sp string)
	unless spacex do
	(format conn-stream "~A ~A ERROR STX F Ill formated property spec: ~A" tid fh string)
	nconc (list* (setq sym (si:intern1 (substring string 0 spacex)
					   si:pkg-keyword-package))
		     (server-convert-known-file-property string (1+ spacex) sym)
		     nil)
	into plist
	finally (trap-lossage (error "Change properties")
		      (let ((m (apply actor :change-properties nil plist)))
			(if (errorp m)
			    (format conn-stream "~A ~A ERROR LOS F ~A" tid fh m)
			  (format conn-stream "~A ~A CHANGE-PROPERTIES" tid fh)))
		  (format conn-stream "~A ~A ERROR SYS F Internal error:~% ~A"
			  tid fh trap-error))))

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:20:52
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-rename (fh strings)
  (cond ((null fh)				;must be string, delete random file
	 (if (not (= (length strings) 2))
	     (format conn-stream "~A  ERROR IRF F Inconsistent RENAME command options" tid)
	   (let ((path1 (lmfs-parse-for-server (first strings)))
		 (path2 (lmfs-parse-for-server (second strings))))
	     (if (errorp path1)
		 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
	       (if (errorp path2)
		   (format conn-stream "~A  ERROR IPS F Syntax error in rename pathname" tid)
		 (trap-lossage (error "Rename 2 args")
		       (progn
			 (if (null (send path1 :version))
			     (setq path1 (send path1 :new-version :newest)))
			 (if (null (send path2 :version))
			     (setq path2 (send path2 :new-version :newest)))
			 (let ((result (send path1 :rename path2 nil)))
			   (if (errorp result)
			       (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result))
			     (format conn-stream "~A  RENAME" tid))))
		       (format conn-stream "~A  ERROR SYS F System error renaming" tid)))))))
	(t					;rename while open
	 (if (not (= (length strings) 1))
	     (format conn-stream "~A ~A ERROR IRF F Inconsistent rename command options"
		     tid fh)
	   (let ((path (lmfs-parse-for-server (first strings))))
	     (if (errorp path)
		 (format conn-stream "~A ~A ERROR IPS F Syntax error in pathname" tid fh)
	       (let* ((data (get fh server-instance))
		      (opening (server-dataproc-comm-opening data)))
		 (if (or (null data)
			 (null opening)
			 (symbolp opening))	;yes, I know NIL is a symbol, thx
		     (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
			     tid fh fh)
		   (trap-lossage (error "Rename while open")
		         (progn
			   (if (null (send path :version))
			       (setq path (send path :new-version :newest)))
			   (let ((result (send opening :rename path nil)))
			     (if (errorp result)
				 (format conn-stream "~A ~A ERROR ~A" tid fh
					 (lmfs-error-string result))
			       (format conn-stream "~A ~A RENAME~%~A"
				       tid fh
				       (server-print-pathname (send opening :truename))))))
		       (format conn-stream "~A ~A ERROR SYS F System error while renaming"
			       tid fh))))))))))

))

; From file DJ: L.NETWORK.CHAOS; CHSNCP.LISP#408 at 27-May-88 17:22:30
#10R CHAOS#: #!:CL
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSNCP  "

(DEFPARAMETER HOST-DOWN-INTERVAL (* 60. 60. 5))	;  5 minutes

))

; From modified file DJ: L.FILE; SERVER.LISP#198 at 27-May-88 17:23:04
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-expunge (fh strings &aux path result)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given in EXPUNGE command." tid fh))
	((null strings)
	 (format conn-stream "~A  ERROR IRF F No pathname given to EXPUNGE command." tid))
	((cdr strings)
	 (format conn-stream "~A  ERROR IRF F Extra junk given to EXPUNGE command." tid))
	((errorp (setq path (lmfs-parse-for-server (first strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq result (send (send path :new-pathname
					  :name :unspecific
					  :type :unspecific
					  :version :unspecific)
				    :expunge :error nil)))
	 (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
	(t (format conn-stream "~A  EXPUNGE ~D" tid result))))

(defun file-server-create-directory (fh strings &aux path result)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given in CREATE-DIRECTORY command."
		 tid fh))
	((null strings)
	 (format conn-stream "~A  ERROR IRF F No pathname given to CREATE-DIRECTORY command." tid))
	((cdr strings)
	 (format conn-stream "~A  ERROR IRF F Extra junk given to CREATE-DIRECTORY command." tid))
	((errorp (setq path (lmfs-parse-for-server (first strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq result (send path :create-directory :error nil)))
	 (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
	(t (format conn-stream "~A  CREATE-DIRECTORY ~D" tid result))))

(defun file-server-create-link (fh strings &aux path path2 result)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given in CREATE-LINK command."
		 tid fh))
	((null (second strings))
	 (format conn-stream "~A  ERROR IRF F Insufficient arguments given to CREATE-LINK command."
		 tid))
	((cddr strings)
	 (format conn-stream "~A  ERROR IRF F Extra junk given to CREATE-LINK command." tid))
	((errorp (setq path (lmfs-parse-for-server (first strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq path2 (lmfs-parse-for-server (second strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq result (send path :create-link path2 :error nil)))
	 (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
	(t (format conn-stream "~A  CREATE-LINK ~D" tid result))))
))
