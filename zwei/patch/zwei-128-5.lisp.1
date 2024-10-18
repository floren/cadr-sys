;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 128.5
;;; Reason:
;;;  When you SAVE-BUFFER for a file belonging to any system with the
;;;  :CTS-CONTROLLED attribitute, and if you haven't inserted at least
;;;  one CTS comment, ZWEI will now remind you to do so.  Enjoy!
;;; Written 17-Nov-88 18:02:24 by smh (Steve Haflich) at site Gigamos Cambridge
;;; while running on Alex from band 3
;;; with Experimental System 130.4, Experimental ZWEI 128.3, Experimental ZMail 75.0, Experimental Local-File 77.0, Experimental File-Server 26.0, Experimental Unix-Interface 16.0, Experimental Tape 27.0, Experimental Lambda-Diag 19.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 11/14 Falcon System Loaded.



; From modified file DJ: L.ZWEI; COMB.LISP#107 at 17-Nov-88 18:03:09
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMB  "


(defun indent-for-cts (symbol-string &aux already-indented)	
  ;;Insert ";;; <symbol> <date> <user> template" at *comment-start*.
  (let* ((line (bp-line (point)))
						;; $$$  <25-Oct-88 saz>
	 (despaced-line (string-trim *whitespace-chars* line))
	 ;; Does this line consist solely of 1 or more tabs
	 ;; and zero or more spaces?
	 (indented-p (and (string-equal "" despaced-line)
			  (setq already-indented t)))
	 (*comment-begin* (string-append
			    (cond ((zerop (string-length line)) ";;; ")
				  (indented-p ";; ")
				  (t "; "))
			    symbol-string)))
    (if (not already-indented)
	(indent-for-comment *point* 1 t nil t)
      (INSERT (point) *COMMENT-BEGIN*))
    (com-end-of-line)				
    (let ((*last-command-char* #\Space))
      (dotimes (i 2) (com-self-insert)))				
    (let ((*last-command-char* #\<))
      (com-self-insert))
      ;; to just print out the date, not the time
    (com-insert-short-date)			
    (let ((*last-command-char* #\Space))		
      (com-self-insert))
    (com-insert-current-user-id)
    (let ((*last-command-char* #\>))		
      (com-self-insert))
    (dotimes (i 2)
      (com-backward-sexp))
    (com-backward))
  ;; $$$ Finally, record that a cts comment was entered so SAVE-BUFFER can warn
  ;; about undocumented changes.  This isn't perfect, but it's something! <17-Nov-88 smh>
  (send *interval* :putprop 't :CTS-DONE))

))

; From modified file DJ: L.ZWEI; DEFS.LISP#169 at 17-Nov-88 18:03:43
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "


(DEFMETHOD (FILE-BUFFER :WRITE-FILE-INTERNAL) (PATH)
  "Save the buffer in file PATH and mark it as visiting that file.
This method is always called when a buffer is written to a file, so it is
a prime candidate for :before and :after daemons."
  "Save BUFFER in file PATHNAME and mark it as visiting that file."
  ;; Examine the buffer's current mode line.
  ;; If the user has edited in a Fonts: property,
  ;; save the font information, even if he has failed to do
  ;; Reparse Mode Line.
  (LET ((PLIST (FS:FILE-EXTRACT-PROPERTY-LIST (INTERVAL-STREAM SELF))))
    (WHEN (CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES PLIST SELF)
      (MUST-REDISPLAY-BUFFER SELF DIS-TEXT))
    (WITH-OPEN-FILE-RETRY (STREAM (PATH FS:FILE-ERROR) :DIRECTION :OUTPUT)
      (STREAM-OUT-INTERVAL STREAM SELF NIL T
			   (OR (GETL (LOCF PLIST) '(:FONTS :DIAGRAM))
			       (SEND SELF :GET-ATTRIBUTE :FONTS)
			       (SEND SELF :GET-ATTRIBUTE :DIAGRAM)))
      (CLOSE STREAM)
      (SET-BUFFER-PATHNAME PATH SELF)
      (SET-BUFFER-FILE-ID SELF (SEND STREAM ':INFO))
      (SETF (BUFFER-TICK SELF) (TICK))
      (send self :remprop :cts-done)		; $$$ cts control <17-Nov-88 smh>
      (WHEN *DISCARD-UNDO-INFO-ON-SAVING*
	(DISCARD-UNDO-INFORMATION SELF))
      (PRINT-FILE-WRITTEN STREAM))))

))

; From modified file DJ: L.ZWEI; METH.LISP#54 at 17-Nov-88 18:07:30
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; METH  "


(DEFMETHOD (FILE-BUFFER :AFTER :NOT-MODIFIED) ()
  (send self :remprop :cts-done)		; $$$ cts control <17-Nov-88 smh>
  (SETQ FILE-TICK *TICK*))

))

; From modified file DJ: L.ZWEI; SECTIO.LISP#317 at 17-Nov-88 18:08:31
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN SYSTEMs-OF-PATHNAME (PATHNAME &OPTIONAL ALREADY-GENERIC)
  "Returns a list of SI::SYSTEM structures for each system PATHNAME is in.
ALREADY-GENERIC non-NIL says assume PATHNAME is already a generic pathname."
  (IF (NULL PATHNAME) '()
    (LET* ((GENERIC-PATHNAME
	     (IF ALREADY-GENERIC PATHNAME
	       (SEND PATHNAME :GENERIC-PATHNAME)))
	   (SYSTEMS (SEND GENERIC-PATHNAME :GET ':SYSTEMS)))
      ;;if it wasn't defined as part of a system, use the SYSTEM system
      (IF (NULL SYSTEMS) '()
	(mapcar #'SI:FIND-SYSTEM-NAMED SYSTEMS)))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#599 at 17-Nov-88 18:10:37
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(DEFUN SAVE-BUFFER (BUFFER &AUX FILE-ID PATHNAME (FILE-FILE-ID  NIL))
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
       (or (send buffer :get :NO-CTS-CONTROL)
	   (dolist (sys (systems-of-pathname *interval*) 't)
	     (when (getf (si::system-plist sys) :cts-controlled)
	       (return 'nil)))
	   (send buffer :get ':CTS-DONE)
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
       (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  T)

))
