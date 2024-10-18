;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 128.4
;;; Reason:
;;;  Fixed bogus max number of lines calculaton which blew up
;;;  in windows with non-default fonts.
;;; Written 17-Nov-88 17:03:32 by smh (Steve Haflich) at site Gigamos Cambridge
;;; while running on Alex from band 3
;;; with Experimental System 130.4, Experimental ZWEI 128.3, Experimental ZMail 75.0, Experimental Local-File 77.0, Experimental File-Server 26.0, Experimental Unix-Interface 16.0, Experimental Tape 27.0, Experimental Lambda-Diag 19.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 11/14 Falcon System Loaded.



; From modified file DJ: L.ZWEI; ZMACS.LISP#599 at 17-Nov-88 17:03:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-KILL-OR-SAVE-BUFFERS "Put up a choice window offering various buffer operations.
A Control-U prefix prevents modified buffers from being marked by default for saving.
Two Control-U prefixes marks all buffers by default for killing." ()
  (LET* ((max-buffer-name-len (+ (loop for buffer in *zmacs-buffer-list*
				   maximize (string-length (buffer-name buffer)))
				 5))
	 (BUFFER-ALIST
	   (DO ((BUFFER-LIST *ZMACS-BUFFER-LIST* (CDR BUFFER-LIST))
		(RET NIL) (TEM)
		(BUFFER) (FILE-ID))
	       ((NULL BUFFER-LIST) RET)
	     (SETQ BUFFER (CAR BUFFER-LIST))
	     (SETQ TEM (let ((str (make-string max-buffer-name-len))
			     (str2  (STRING-APPEND "  " (BUFFER-NAME BUFFER) "  ")))
			 ;;; $$$ Print a '.' every 5th blank space after pathname. <14nov88 keith>
			 (fillarray str str2)
			 (do ((i (string-length str2) (1+ i)))
			     ((= i max-buffer-name-len) str)
			   (setf (char str i)
				 (if (zerop (remainder i 5)) #/. #/space))))
		   FILE-ID (BUFFER-FILE-ID BUFFER))
	     (SETF (CHAR TEM 0)
		   (COND ((EQ FILE-ID T) #/+)
			 ((BUFFER-READ-ONLY-P BUFFER) #/)
			 ((BUFFER-MODIFIED-P BUFFER) #/*)
			 (T #/SP)))
	     (LET ((BASIC-CHOICES (IF (BUFFER-NEEDS-SAVING-P BUFFER)
				      '((:SAVE T) :KILL :NOT-MODIFIED)
				    '(:SAVE :KILL :NOT-MODIFIED))))
	       (PUSH (LIST BUFFER TEM
			   (IF (AND (BUFFER-PATHNAME BUFFER)
				    (call-editing-type-function (send buffer :major-mode) 'lisp-syntax-p nil))
			       (APPEND BASIC-CHOICES '(:COMPILE))
			     BASIC-CHOICES))
		     RET))))
	CHOICES EXIT-REASON)
    (SETQ BUFFER-ALIST
	  (SORT BUFFER-ALIST (LAMBDA (X Y &AUX STR1 STR2 CH1 CH2)
			       (IF (CHAR= (SETQ CH1 (CHAR (SETQ STR1 (CADR X)) 0))
					  (SETQ CH2 (CHAR (SETQ STR2 (CADR Y)) 0)))
				   (STRING-LESSP STR1 STR2)
				 (< (CASE CH1
				      (#/* 0)
				      (#/+ 1)
				      (#/SP 2)
				      (#/ 3))
				    (CASE CH2
				      (#/* 0)
				      (#/+ 1)
				      (#/SP 2)
				      (#/ 3)))))))
    ;; $$$ Single C-U prevents all SAVE buttons from defaulting to on. <17-Nov-88 smh&saz>
    (if (= *numeric-arg* 4.)			;Was CONTROL-U typed?
	(mapcar #'(lambda (entry)		;If so, brings up the menu with all
		    (setf (third entry)	;buffers marked to be saved.
			  (substitute ':save '(:save t) (third entry) :test #'equal)))
		buffer-alist))
    (if (= *numeric-arg* 16.)			;Was CONTROL-U CONTROL-U typed?
	(mapcar #'(lambda (entry)		;If so, brings up the menu with all
		    (setf (third entry)		;buffers marked to be killed.
			  (substitute '(:kill t) ':kill (third entry))))
		buffer-alist))
    (SETF (VALUES CHOICES EXIT-REASON)
	  (TV:MULTIPLE-CHOOSE "  Buffer" BUFFER-ALIST
			      '((:SAVE "Save" NIL (:NOT-MODIFIED) NIL NIL)
				(:KILL "Kill" NIL (:NOT-MODIFIED) NIL NIL)
				(:NOT-MODIFIED "UnMod"
					       NIL (:SAVE :KILL) NIL NIL)
				(:COMPILE "Compile" NIL NIL NIL NIL))
			      `(:point ,(// (tv:sheet-width *window*) 2)
				       ,(// (tv:sheet-height *window*) 2))
			      (min (length buffer-alist)
				   ;; $$$ Wired in CPTFONT line height. <17-Nov-88 smh&saz>
				   (// (tv:sheet-height *window*)        	;gotta be a better way to say "number
				       14. #+never (tv:sheet-line-height *window*)))))	;of lines displayable on *window*"
    (IF EXIT-REASON
	NIL
      ;; Make sure the current buffer gets done last
      (LET ((ELEM (ASSQ *INTERVAL* CHOICES)))
	(AND ELEM (SETQ CHOICES (NCONC (DELQ ELEM CHOICES) (NCONS ELEM)))))
      (DOLIST (CHOICE CHOICES)
	(LET ((BUFFER (CAR CHOICE)))
	  (IF (MEMQ ':SAVE (CDR CHOICE))
	      (SAVE-BUFFER BUFFER))
	  (IF (MEMQ ':COMPILE (CDR CHOICE))
	      (COMPILE-FILE (BUFFER-PATHNAME BUFFER)
			    :SET-DEFAULT-PATHNAME NIL
			    :PACKAGE (BUFFER-PACKAGE BUFFER)))
	  (IF (MEMQ ':NOT-MODIFIED (CDR CHOICE))
	      (SETF (BUFFER-TICK BUFFER) (TICK)))
	  (IF (MEMQ ':KILL (CDR CHOICE))
	      (KILL-BUFFER BUFFER T))))
      (FORMAT *QUERY-IO* "~&Done.")))
  DIS-NONE)

))
