;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.8
;;; Reason:
;;;  A change to SAVE-BUFFER pertaining to the handling of saving problems.
;;;  
;;;  When a file appears to have changed behind the user's back, ZMacs says:
;;;  
;;;  When you last read or wrote FOO,
;;;  it was... 
;;;  but now it is... 
;;;  Save it anyway?
;;;  
;;;  Well, sometimes we can get into a situation where the problem occurs
;;;  every time we go to save a particular buffer.  So, now we allow the user
;;;  to answer Y, N, or P for "proceed", which means, "stop bothering me
;;;  about this buffer".
;;; Written 1-Sep-88 15:09:31 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.66, Experimental ZWEI 126.2, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From file DJ: L.ZWEI; ZMACS.LISP#583 at 1-Sep-88 15:09:31
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN SAVE-BUFFER (BUFFER &AUX FILE-ID PATHNAME (FILE-FILE-ID NIL))
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
			 :choices (((T "Yes")  #\Y #\hand-up)
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
       (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  T)

))
