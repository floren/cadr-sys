;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.18
;;; Reason:
;;;  With a numeric argument, ZWEI:COM-SOURCE-COMPARE-CHANGES now
;;;  performs a source compare on the current buffer with the
;;;  latest version of its associated file found on disk, without query.
;;; Written 13-Oct-88 16:21:02 by SAZ at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.103, Experimental ZWEI 126.17, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.



; From modified file DJ: L.ZWEI; ZMACS.LISP#589 at 13-Oct-88 16:21:03
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-SOURCE-COMPARE-CHANGES 
  "Compare a buffer vs the buffer's files.  If no changes, clear buffer's modified bit.
The output goes on the screen, and also into a buffer named *Source Compare ...*.
With a numeric argument, perform source compare on the current buffer without asking." ()
  (LET* ((BUFFER (if *numeric-arg-p*
		     *interval*
		   (READ-BUFFER-NAME "Compare changes of buffer" *INTERVAL*)))
	 (NAME (BUFFER-NAME BUFFER))
	 FILE-1 FILE-2)
    (UNWIND-PROTECT
      (PROGN
	(SETQ FILE-1 (SRCCOM::MAKE-FILE
		       :FILE-NAME NAME
		       :FILE-TYPE "Buffer"
		       :FILE-STREAM (INTERVAL-STREAM BUFFER)
		       :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
						  (SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE BUFFER)))
						SI:PKG-KEYWORD-PACKAGE)))
	(SETQ FILE-2 (SRCCOM::CREATE-FILE (BUFFER-PATHNAME BUFFER)))
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
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE))))
  DIS-NONE)

))
