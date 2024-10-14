;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for File-Server version 24.1
;;; Reason:
;;;  File server was assuming any data arriving with a binary opcode is
;;;  in 16 bit chunks.  Chaosnet is perfectly willing to send 8 bit
;;;  binary data...
;;; Written 1-Aug-88 18:11:03 by pld at site Gigamos Cambridge
;;; while running on Maurice Ravel from band 3
;;; with System 125.19, ZWEI 125.7, ZMail 73.0, Local-File 75.1, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, Kenv 7/22/88.



; From modified file DJ: L.FILE; SERVER.LISP#201 at 1-Aug-88 18:11:37
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
			 ((fs:%qfile-binary-opcode fs:%qfile-character-opcode)
			  (unwind-protect
			      (if binp
				  (send opening :string-out pkt chaos:first-data-word-in-pkt
					(+ (truncate (chaos:pkt-nbytes pkt) 2)
					   chaos:first-data-word-in-pkt))
				(send opening :string-out (chaos:pkt-string pkt)
				      0 (chaos:pkt-nbytes pkt)))
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
