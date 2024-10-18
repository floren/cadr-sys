;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.21
;;; Reason:
;;;  By default, now Source Compare Changes displays file, then buffer
;;;  contents; this is a more legible ordering corresponding to "before and
;;;  after" changes were made.  The ordering is controlled by the following
;;;  parameter variable, for those who want to reenable the previous
;;;  behaviour:
;;;    
;;;    (defparameter *source-compare-display-file-then-buffer* t
;;;      "Indicates preferences when Source Compare command (e.g., Source Compare Changes)
;;;      is displaying buffer and associated file contents.
;;;    If non-NIL, file contents are displayed before buffer contents.")
;;; Written 18-Oct-88 15:19:21 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.111, Experimental ZWEI 126.20, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Experimental IMicro 20.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; ZMACS.LISP#591 at 18-Oct-88 15:19:25
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defparameter *source-compare-display-file-then-buffer* t
  "Indicates preferences when Source Compare command (e.g., Source Compare Changes)
  is displaying buffer and associated file contents.
If non-NIL, file contents are displayed before buffer contents.")

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#591 at 18-Oct-88 15:19:27
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defun source-compare-changes (buffer)
  (LET* ((NAME (BUFFER-NAME BUFFER))
	 FILE-1 FILE-2)
    (UNWIND-PROTECT
	(progn
	  (let (buff-file file-file)
	    (SETQ buff-file (SRCCOM::MAKE-FILE
			      :FILE-NAME NAME
			      :FILE-TYPE "Buffer"
			      :FILE-STREAM (INTERVAL-STREAM BUFFER)
			      :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
							 (SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE BUFFER)))
						       SI:PKG-KEYWORD-PACKAGE)))
	    (SETQ file-file (SRCCOM::CREATE-FILE (BUFFER-PATHNAME BUFFER)))
	    (if *source-compare-display-file-then-buffer*
		(setq file-1 file-file file-2 buff-file)
	      (setq file-1 buff-file file-2 file-file)))
	  (LET* ((OUTPUT-NAME (FORMAT NIL "*Source Compare Changes of ~A*" NAME))
		 (*STANDARD-OUTPUT*
		   (MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM
		     (FORMAT NIL "*Source Compare Changes of ~A*" NAME) NIL T)))
	    (with-source-compare-parameter-handling file-1 file-2
	      (SRCCOM:DESCRIBE-SRCCOM-SOURCES FILE-1 FILE-2 *STANDARD-OUTPUT*)
	      (COND ((SRCCOM:SOURCE-COMPARE-FILES FILE-1 FILE-2 *STANDARD-OUTPUT*
						  (SRCCOM::QUERY-TYPE))
		     (SETF (BUFFER-TICK BUFFER) (TICK))))	;No changes, unmodify buffer.
	      (LET ((OUTBUF (FIND-BUFFER-NAMED OUTPUT-NAME)))
		(AND OUTBUF (SETF (GET OUTBUF 'SPECIAL-PURPOSE) :SRCCOM-OUTPUT))))
	    (FORMAT T "~&Done.")))
      (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE)))))


))
