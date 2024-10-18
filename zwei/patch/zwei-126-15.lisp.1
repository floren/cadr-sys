;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.15
;;; Reason:
;;;  Use new SOURCE-COMPARE escape character handling to avoid extreme
;;;  lossage caused by font changes in Meta-X Source Compare and Source
;;;  Compare Changes.  (Making the same change to Source Compare Merge might
;;;  break it even worse.)
;;; Written 7-Oct-88 18:40:25 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.102, Experimental ZWEI 126.14, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; ZMACS.LISP#587 at 7-Oct-88 18:40:31
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defconstant *ignore-font-flag-in-source-compare* '(#/epsilon 1))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#587 at 7-Oct-88 18:40:33
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defmacro with-source-compare-parameter-handling (file-1 file-2 &body body)
  (declare (indentation 2 1))
  `(let ((srccom:*lines-to-print-before* 0.)(srccom:*lines-to-print-after* 0.)
	 (srccom:*escape-character-ignore-flag*
	   (if (or (getf (fs:file-attribute-list (srccom:file-stream ,file-1)) :fonts)
		   (getf (fs:file-attribute-list (srccom:file-stream ,file-2)) :fonts))
	       *ignore-font-flag-in-source-compare*)))
     ,@body))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#587 at 7-Oct-88 18:40:36
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(DEFCOM COM-SOURCE-COMPARE "Compare two files or buffers.
The output goes on the screen, and also into a buffer named *Source Compare ...*." ()
  (LET (FILE-1 FILE-2 NAME-1 NAME-2 KIND DEFAULT)
    (UNWIND-PROTECT
	(PROGN
	  (MULTIPLE-VALUE (FILE-1 NAME-1 Kind DEFAULT)
	    (GET-BUFFER-OR-FILE-FILE "Compare" NIL))
	  (MULTIPLE-VALUE (FILE-2 NAME-2)
	    (GET-BUFFER-OR-FILE-FILE (FORMAT NIL "Compare ~A ~A with" Kind NAME-1)
				     DEFAULT))
	  (LET* ((OUTPUT-NAME (FORMAT NIL "*Source Compare ~A // ~A*" NAME-1 NAME-2))
		 (*STANDARD-OUTPUT* (MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM OUTPUT-NAME NIL T)))
	    (with-source-compare-parameter-handling file-1 file-2
	      (SRCCOM:DESCRIBE-SRCCOM-SOURCES FILE-1 FILE-2 *STANDARD-OUTPUT*)
	      (SRCCOM:SOURCE-COMPARE-FILES FILE-1 FILE-2 *STANDARD-OUTPUT* (SRCCOM::QUERY-TYPE))
	      (LET ((OUTBUF (FIND-BUFFER-NAMED OUTPUT-NAME)))
		(WHEN OUTBUF
		  (SETF (GET OUTBUF 'SPECIAL-PURPOSE) :SRCCOM-OUTPUT))))
	    (FORMAT T "~&Done.")))
      (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE))))
  DIS-NONE)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#587 at 7-Oct-88 18:40:38
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(DEFCOM COM-SOURCE-COMPARE-CHANGES 
  "Compare a buffer vs the buffer's files.  If no changes, clear buffer's modified bit.
The output goes on the screen, and also into a buffer named *Source Compare ...*." ()
  (LET* ((BUFFER (READ-BUFFER-NAME "Compare changes of buffer" *INTERVAL*))
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
