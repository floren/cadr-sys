;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 18.2
;;; Reason:
;;;  use the new name CHAOS:UNWANTED-CONNECTION-P
;;; Written 16-Mar-86 13:23:21 by DAWNA at site LMI Cambridge
;;; while running on Lambda Two from band 1
;;; with Experimental System 110.115, Experimental Local-File 66.1, Experimental FILE-Server 18.1, Experimental MagTape 4.1, Experimental ZMail 65.7, Experimental Unix-Interface 9.0, Experimental TCP-Kernel 39.4, Experimental TCP-User 62.4, Experimental TCP-Server 45.5, microcode 1408, SDU ROM 8.



; From modified file DJ: L.FILE; SERVER.LISP#181 at 16-Mar-86 13:23:22
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
	  (*catch 'server-chaos-disappear
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
		       (send conn-stream ':force-output)
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
		  (send conn-stream ':force-output)
		  (chaos:return-pkt pkt))))))
      (when conn
	(send tv:who-line-file-state-sheet ':delete-server conn)
	(trap-lossage (error "Server Top Level close")
		      (chaos:close-conn conn
					(or *lmfs-server-dont-answer-logins*
					    "Server error")))
	(chaos:remove-conn conn))
      (if server-openings
	  (trap-lossage (error "Server finish closing remaining openings")
	      (dolist (opening server-openings)
		(send opening ':close ':abort))))
      (trap-lossage (error "Closeout undata")
		    (dolist (data alldatas)
		      (rplaca (server-dataproc-comm-cell
				(get (server-dataproc-comm-sibling data) server-instance))
			      'undata)
		      (rplaca (server-dataproc-comm-cell data) 'undata)
		      (chaos:remove-conn (server-dataproc-comm-conn data)))))))

))
