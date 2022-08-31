;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for FILE-Server version 8.2
;;; Reason: Disk-full handling.
;;; Written 1/04/84 18:05:56 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.22, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.0, Experimental FILE-Server 8.0, Experimental LFS 3.0, Experimental MagTape 22.0, microcode 306, Xmntl FS.



; From file SERVER.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-data-top-level (server-instance cell handle
				   &aux &special (fs:*local-server-via-net* nil))
  (trap-lossage (error "File Server Data Connection")
   (do () (())
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
	    (do () (())
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
		  (or (= (chaos:pkt-opcode pkt) fs:%file-synchronous-mark-opcode)
		      (ferror "Unrecognized Opcode in data server: ~O"
			      (chaos:pkt-opcode pkt)))
		  (chaos:return-pkt pkt)
		  (%store-conditional celloc 'write nil)
		  (return nil))
		 (fs:%file-synchronous-mark-opcode
		  (chaos:return-pkt pkt)
		  (%store-conditional celloc 'write nil)
		  (return nil))
		 (fs:%file-binary-opcode
		  (unwind-protect
		    (funcall opening ':string-out pkt chaos:first-data-word-in-pkt
			     (+ (truncate (chaos:pkt-nbytes pkt) 2)
				chaos:first-data-word-in-pkt))
		    (chaos:return-pkt pkt)))
		 (fs:%file-character-opcode
		  (unwind-protect
		    (funcall opening ':string-out (chaos:pkt-string pkt)
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
			 (funcall opening ':string-in nil pkt
				  chaos:first-data-word-in-pkt chaos:max-data-words-per-pkt))
		       (setf (chaos:pkt-opcode pkt) fs:%file-binary-opcode)
		       (setf (chaos:pkt-nbytes pkt)
			     (* 2 (- last chaos:first-data-word-in-pkt))))
		    (t (multiple-value (last eofp)
			 (funcall opening ':string-in nil (chaos:pkt-string pkt)
				  0 chaos:max-data-bytes-per-pkt))
		       (setf (chaos:pkt-opcode pkt) fs:%file-character-opcode)
		       (setf (chaos:pkt-nbytes pkt) last)))
	      (if (plusp (chaos:pkt-nbytes pkt))
		  (chaos:send-pkt dconn pkt (chaos:pkt-opcode pkt))	;don't let SEND dft it
		  (chaos:return-pkt pkt))
	      (cond (eofp
		     (if (server-window-read-check cell dconn) (return nil))
		     (chaos:send-pkt dconn (chaos:get-pkt) chaos:eof-op)
		     (%store-conditional celloc 'read nil)
		     (return nil))))))
	 (t (ferror nil "Bogus com-cell value: ~S" (car cell))))))))

))

; From file SERVER.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun server-disk-full-handler (condition)
  (declare (special server-instance cell1 handle1)
	   (unspecial tid))
  (let* ((data (get handle1 server-instance))
	 (celloc (locf (car cell1)))
	 (tid (server-dataproc-comm-tid data))
	 (dconn (server-dataproc-comm-conn data))
	 (cconn (server-dataproc-comm-cconn data)))
    (%store-conditional celloc 'write 'async-mark)
    ;; Send an async pkt on the control connection to advertise our woes.
    (let ((pkt (chaos:get-pkt)))
      (chaos:set-pkt-string
	pkt tid " " handle1 " ERROR NMR R " (send condition ':report-string))
      (chaos:send-pkt cconn pkt %file-asynchronous-mark-opcode))
    ;; Now wait for the control connection to fix us.
    (process-wait "Disk Full"
		  #'(lambda (x) (neq (car x) 'async-mark))
		  cell1)
    (selectq (car cell1)
      (continue    (rplaca cell1 'write)
		   (values ':retry-file-operation nil))
      (async-abort (loop for pkt = (chaos:get-next-pkt dconn)
			 as op = (chaos:pkt-opcode pkt)
			 do (chaos:return-pkt pkt)
			 when (= op fs:%file-synchronous-mark-opcode)
			 return nil)
		   (rplaca cell1 'nil)
		   (*throw 'async-abort nil))
      (otherwise  (ferror nil "Cell in odd state in async recover - ~S"
			  (car cell1))))))

))
