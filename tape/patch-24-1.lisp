;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 24.1
;;; Reason:
;;;  Improvements to interface of PARTITION-SEARCHER.
;;;  
;;;  1. When it drags someone into the command loop against their will (maybe
;;;  they mis-typed an argument), give them a message that orients them.
;;;  
;;;  2. Print help note on first entering command loop.  Same after every 8
;;;  bogus keystrokes (they're obviously confused).
;;;  
;;;  3. Print more complete info on current selections.
;;;  
;;;  4. If they have sufficiently specified a partition to select, give them
;;;  a message telling them how to exit.
;;; Written 22-Jul-88 08:41:58 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with System 125.16, ZWEI 125.2, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.0, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, 7/21/88.



; From modified file DJ: L.TAPE; TAPE.LISP#170 at 22-Jul-88 08:41:59
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPE  "

(defun partition-searcher (purpose-string number-of-blocks-needed
			   &key
			   default-partition
			   (default-unit (select-processor (:cadr 0) (:lambda 0) (:explorer nil)))
			   default-comment
			   (interface-stream *terminal-io*)
			   confirm-write)
  "An interface to allow the user to look around for an appropriate
   partition for a particular use described in English in PURPOSE-STRING.
   The selected partition may be located on the system disk, an auxiliary
   disk, or a remote host."
  (declare (values host unit start length label-location name))
  (check-type default-unit (or null (integer 0 8) closure string))
  (check-type default-partition (or null string))
  (with-smooth-abort
    (let ((decoded-unit (if default-unit (get-unit-neatly default-unit purpose-string))))
      (with-abort-disposal (decoded-unit)
	(when decoded-unit
	  (when (and default-partition decoded-unit)
	    (multiple-value-bind (start length label-loc name)
		(si:find-disk-partition default-partition nil decoded-unit)
	      start
	      (when (and (>= length number-of-blocks-needed)
			 (or (null default-comment)
			     (string-equal default-comment
					   (si:partition-comment name decoded-unit)))
			 (with-timeout ((* 60 60 1)
					(format *standard-output* "Timed-out ... Yes")
					t)
			   (y-or-n-p "Use partition ~S on unit ~D of host ~a ~A (1 minute timeout)"
				     default-partition
				     (unit-number default-unit)
				     (unit-host default-unit)
				     purpose-string)))
		(return-from partition-searcher
		  (values (unit-host decoded-unit) decoded-unit start length label-loc name)))))
	  (let ((choice (if decoded-unit
			    (prompt-and-read
			      :string-or-nil
			      "~&Type partition name on ~A (unit ~D) for ~A or ~C to find one >> "
			      (unit-host decoded-unit)
			      (unit-number decoded-unit)
			      purpose-string #\end))))
	    (when choice
	      (multiple-value-bind (start length label-loc name)
		  (si:find-disk-partition choice nil decoded-unit nil confirm-write)
		(cond ((not start)
		       (format t "~&Invalid partition selection: ~S" choice))
		      ((< length number-of-blocks-needed)
		       (format *standard-output* "~&Invalid partition selection (need ~D blocks): ~S"
			       number-of-blocks-needed choice))
		      (t
		       (return-from partition-searcher
			 (values (unit-host decoded-unit)
				 decoded-unit
				 start
				 length
				 label-loc
				 name))))
		(format t "~&~2% --- You have entered the Partition Searcher. Press any key to begin ---")
		(read-char)))))
	(do ((unit (or decoded-unit nil))
	     (host (if decoded-unit (unit-host decoded-unit) si:local-host))
	     partition
	     char
	     (reprint t)
	     (first-time t nil)
	     (bogus-inputs 0)
	     (max-bogus-inputs 8))
	    (())
	  (with-abort-disposal (unit)
	    (when (or reprint (>= bogus-inputs max-bogus-inputs))
	      (send interface-stream :clear-screen)
	      (format t "--- Partition Searcher: Searching for partition ~A ---~%"
		      purpose-string)
	      (when unit
		(print-disk-label unit)
		(format t "~&~2%Selected partition: ~:[<none>~;~:*~A~] - Selected unit: ~A~@[ on ~S~]~2%"
			(fourth partition)
			(unit-number unit)
			(unit-host unit)))
	      (when (and partition unit)
		(format t "~%If you are satisfied with this selection, press ~C to exit.~%" #\end))
	      (setq reprint nil))
	    (when (if (or (>= bogus-inputs max-bogus-inputs) first-time)
		      (setq char #\help
			    bogus-inputs 0)
		    (progn (format t "~&~%Command >> ")
			   (setq char (read-char interface-stream))))
	      (selector char char-equal
		((#^q #^Q #\end)
		 (if (null partition)
		     (when (yes-or-no-p
			     "~&You have not selected a partiton.~%~
                           Do you really want to abort selecting a partition? ")
		       (si:dispose-of-unit unit)
		       (return-from partition-searcher nil))
		   (return-from partition-searcher
		     (lexpr-funcall 'values host unit partition))))
		((#^p #^P)
		 (let* ((string (prompt-and-read :string-or-nil "~& Partition to select >> "))
			(vals (multiple-value-list (si:find-disk-partition string nil unit nil confirm-write))))
		   (cond ((null (car vals))
			  (tv:beep)
			  (format t "~&Invalid partition selection.  Try again."))
			 ((< (second vals) number-of-blocks-needed)
			  (tv:beep)
			  (format t "~&Partition not big enough (need ~D blocks). Try again."
				  number-of-blocks-needed))
			 (t
			  (if (stringp (fourth vals))
			      (setf (fourth vals) (string-upcase (fourth vals))))
			  (setq partition vals
				  reprint t)))))
		((#^u #^U)
		 (let ((nunit (prompt-and-read :number "~& Unit to select >> ")))
		   (if (typep nunit '(not (integer 0 8)))	;There should be a real test here!!!
		       (format t "~&Invalid unit selection (must be and integer [0 7]). Try again.~%")
		     (let ((du (get-unit-neatly (if (eq host si:local-host)
						    nunit
						  (format nil "~A ~D" host nunit))
						"Disk Serving for Tape")))
		       (when du
			 (si:dispose-of-unit unit)
			 (setq unit du
			       partition nil
			       reprint t))))))
		((#^h #^H)
		 (let* ((string (prompt-and-read :string "~& New host >> "))
			(nhost (condition-case () (si:parse-host string) (si:unknown-host-name))))
		   (if (null nhost)
		       (format t "~&Unknown host.  Try again.~%")
		     (let ((du (get-unit-neatly (if (eq nhost si:local-host)
						    0
						  (format nil "~A 0" nhost))
						"Disk Serving for Tape")))
		       (when du
			 (si:dispose-of-unit unit)
			 (setq host nhost
			       unit du
			       partition nil
			       reprint t))))))
		((#^e #^E)
		 (when (yes-or-no-p "Do you really want to edit the disk label for ~A?" host)
		   (with-smooth-abort (si:edit-disk-label unit))
		   (setq reprint t)))
		((#^l #\clear-screen)
		 (setq reprint t))
		((#\help #^?)
		 (format t "~&~%The following commands are available:~%~
                    ~C - Select a partition~%~
                    ~C - Select a new disk unit~%~
                    ~C - Select a new host~%~
                    ~C - Edit disk label for current host and unit~%~
                    ~C - Redisplay (re-reading label)~%~
                    ~C - Quit, returning current selection~%~
                    ~C - Abort, return a selection of NIL~2%"
			 #^p #^u #^h #^e #^l #\end #\abort))
		(t (incf bogus-inputs)
		   (tv:beep))))))))))

))
