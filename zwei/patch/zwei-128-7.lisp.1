;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 128.7
;;; Reason:
;;;  Cleaned CTS reminder feature in SAVE-BUFFER.
;;;  Also added a change-logging facility which is not yet turned on and
;;;  which therefore shouldn't bother anyone.
;;; Written 18-Nov-88 22:11:55 by smh (Steve Haflich) at site Gigamos Cambridge
;;; while running on Alex from band 3
;;; with Experimental System 130.7, Experimental ZWEI 128.6, Experimental ZMail 75.0, Experimental Local-File 77.0, Experimental File-Server 26.0, Experimental Unix-Interface 16.0, Experimental Tape 27.0, Experimental Lambda-Diag 19.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 11/14 Falcon System Loaded.



; From modified file DJ: L.ZWEI; ZMACS.LISP#601 at 18-Nov-88 22:12:56
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defvar *CTS-package-to-mailing-address-alist* NIL
  "An alist keyed on keywordized package names.
If a value is non-NIL, that package will be CTS-controlled and the user will gently
be reminded if he forgets to add CTS comments.  Additionally, if a value is a string,
a summary of changed sections will be mailed using that string as a mail address.")

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#601 at 18-Nov-88 22:12:58
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defun set-cts-control-for-package (package-name &optional (mail-log-address-string 'T))
  "See the variable *CTS-PACKAGE-TO-MAILING-ADDRESS-ALIST*."
  (let* ((name (intern (etypecase package-name
			 (package (package-name package-name))
			 (string (string-upcase package-name))
			 (symbol (symbol-name package-name))) pkg-keyword-package))
	 (entry (assq name *CTS-package-to-mailing-address-alist*)))
    (if entry
	(rplacd entry mail-log-address-string)
      (push (cons name mail-log-address-string) *CTS-package-to-mailing-address-alist*))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#601 at 18-Nov-88 22:13:02
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(DEFUN SAVE-BUFFER (BUFFER &AUX FILE-ID PATHNAME (FILE-FILE-ID  NIL) cts-address)
  "Save BUFFER unconditionally into its visited file, or a file read from the mini buffer."
  (READ-BUFFER-PATHNAME-FOR-SAVING BUFFER)
  (SETQ FILE-ID (BUFFER-FILE-ID BUFFER)
 	PATHNAME (BUFFER-PATHNAME BUFFER))
  (AND (OR (SYMBOLP FILE-ID)
 	   (WITH-OPEN-FILE (S PATHNAME '(:PROBE :ASCII))
 	     (AND (NOT (ERRORP S))
		  (EQUAL (SETQ FILE-FILE-ID (SEND S :INFO)) FILE-ID)))
 	   ;;Sometimes we can get into a situation where the "save it
	   ;;anyway" problem, below, occurs every time we go to save a
	   ;;particular buffer.  So, we allow the user to say, "stop
	   ;;bothering me", allowing the "Proceed" response. 
	   (getf (plist buffer) 'dont-ask-again-about-saving-it-anyway)
	   (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
	     (case
	       (fquery '(:beep t
			       :type :tyi
			       :choices (((T "Yes") #\Y #\hand-up)
					 ((NIL "No") #\N #\hand-down)
					 ((:proceed "Proceed, and don't ask again about this buffer later") #\P)))
		       "When you last read or wrote ~A~@
		      it was ~A,~@
		      but now it is ~A.~@
		      Do you want to save it? "
		       PATHNAME (DESCRIBE-FILE-ID FILE-ID)
		       (IF FILE-FILE-ID (DESCRIBE-FILE-ID FILE-FILE-ID) "deleted"))
	       (NIL NIL)
	       (:PROCEED
		(setf (getf (plist buffer) 'dont-ask-again-about-saving-it-anyway) t)
		t)
	       (T T))))
       ;; The following clause weakly enforces CTS comment annotation for files that belong
       ;; to any system with the :CTS-CONTROLLED declaration. $$$ <17-Nov-88 smh>
       (or (send buffer :get :NOT-CTS-CONTROLLED)
	   (send buffer :get :PATCH-FILE)
	   (not (setq cts-address
		      ;; This is either T, indicating just that the user should be reminded to use CTS,
		      ;; or else a string, which will be used as a mail address to log the changes.
		      (let ((pkg (send buffer :get :package)))
			(and pkg (stringp pkg) (setq pkg (intern (string-upcase pkg) pkg-keyword-package)))
			(or (cdr (assq pkg *CTS-package-to-mailing-address-alist*))
			    (and pkg
				 (setq pkg (find-package pkg))
				 (send pkg :get :CTS-CONTROLLED))
			    (dolist (sys (systems-of-pathname buffer) 'nil)
			      (let ((prop (getf (si::system-plist sys) :CTS-CONTROLLED)))
				(when prop (return prop))))))))
	   ;; At this point we know the buffer is subject to cts control.
	   (and (or (send buffer :get ':CTS-DONE)
		    (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
		      (case (fquery '(:beep t
					    :type :tyi
					    :choices (((T "Yes") #\Y #\hand-up)
						      ((NIL "No") #\N #\hand-down)))
				    "~A belongs to a system with the :CTS-CONTROLLED attribute~@
				but you have not entered any CTS comments since it was last read or written.~@
				Do you want to save it anyway without documenting your changes? "
				    PATHNAME)
			(NIL NIL)
			(T T))))
		;; Send mail logging the changed sections.
		(prog1
		  't
		  (when (stringp cts-address)
		    (let ((changed-sections (let ((*numeric-arg* 2)) (changed-sections-1 buffer))))
		      ;;(format t "~%Finking on you to ~a!" cts-address)
		      (process-run-function
			"CTS Mailer" #'(lambda (adr txt)
					 (errset (mail adr txt) nil))
			cts-address
			(format nil "~&/"~a/" changed by ~:@(~a~) on ~a at ~\time\~%The changed sections were:~{~^~% ~a~}"
				(send buffer :name) user-id (send si:local-host :short-name) (get-universal-time)
				(mapcar #'(lambda (sect) (send sect :defun-line)) changed-sections))))))))
       (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  T)

))
