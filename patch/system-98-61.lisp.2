;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 6/05/84 16:50:47 by RMS,
;;; Reason: decoding disk unit args -- what does this do, rms?
;;; Bug in commonlisp member functions
;;; while running on Lisp Machine Eight from band 2
;;; with System 98.48, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309, ZM MIT.



; From file DISK.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DISK  "


(DEFUN DECODE-UNIT-ARGUMENT (UNIT USE &OPTIONAL (CC-DISK-INIT-P NIL) (WRITE-P NIL)
			     &AUX TEM)
  "First value is decoded unit.  Second if T if arg was not already a decoded unit.
If second value is NIL, the caller should call DISPOSE-OF-UNIT eventually."
  (DECLARE (SPECIAL CC-DISK-INIT-P CADR:CC-DISK-LOWCORE CADR:CC-DISK-TYPE))
  (COND ((NUMBERP UNIT) UNIT)			;Local disk
	((AND (STRINGP UNIT) 
	      (STRING-EQUAL UNIT "CC" 0 0 2))
	 (SETQ TEM (STRING-SEARCH-CHAR #/SP UNIT))
	 (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (DECLARE (SPECIAL CC-DISK-UNIT))
	   (COND ((NOT (ZEROP CC-DISK-UNIT))
		  (FERROR NIL "CC can only talk to unit zero")))
	   (COND ((NULL CC-DISK-INIT-P)
		  (CADR:CC-DISK-INIT)
		  ;; Block 2 is part of the disk label!
		  (CADR:CC-DISK-WRITE 1 CADR:CC-DISK-LOWCORE 1) ;Save on blocks 1, 3.
		  (CADR:CC-DISK-WRITE 3 (1+ CADR:CC-DISK-LOWCORE) 1))
		 (T (SETQ CADR:CC-DISK-TYPE T)))   ;Dont try to read garbage label, etc.
	   (CLOSURE '(CC-DISK-UNIT CC-DISK-INIT-P)
		    'CC-DISK-HANDLER)))
	((AND (STRINGP UNIT)			;This makes test data.
	      (STRING-EQUAL UNIT "TEST" 0 0 4))
	 (SETQ TEM (STRING-SEARCH-CHAR #/SP UNIT))
	 (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (declare (special cc-disk-unit))
	   (CLOSURE '(CC-DISK-UNIT)
		    'CC-TEST-HANDLER)))
	((AND (STRINGP UNIT)			;Magtape interface.
	      (STRING-EQUAL UNIT "MT" 0 0 2))
	 (SETQ TEM (STRING-SEARCH-CHAR #/SP UNIT))
	 (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (COND ((NOT (ZEROP CC-DISK-UNIT))
		  (FERROR NIL "MT can only talk to unit zero")))
	   (FS:MAKE-BAND-MAGTAPE-HANDLER WRITE-P)))	;Temporarily, this fctn in RG;MT.
	((STRINGP UNIT)				;Open connection to foreign disk
	 ;; make @lm1 work as well as lm1
	 ;; if a host is stupid enough to have a name like @Losing  then use "@@Losing"
	 (IF (STRING-EQUAL #/@ (SUBSTRING UNIT 0 1))
	     (SETQ UNIT (SUBSTRING UNIT 1)))
	 (LET ((REMOTE-DISK-CONN
		 (CHAOS:CONNECT (SUBSTRING UNIT 0 (SETQ TEM (STRING-SEARCH-CHAR #/SP UNIT)))
				"REMOTE-DISK" 25.))
	       (REMOTE-DISK-STREAM)
	       (REMOTE-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (declare (special remote-disk-conn remote-disk-stream remote-disk-unit))
	   (AND (STRINGP REMOTE-DISK-CONN)
		(FERROR NIL "Cannot connect to ~S: ~A" UNIT REMOTE-DISK-CONN))
	   (SETQ REMOTE-DISK-STREAM (CHAOS:MAKE-STREAM REMOTE-DISK-CONN))
	   (FORMAT REMOTE-DISK-STREAM "SAY Disk being hacked remotely by ~A@~A -- ~A~%"
		   USER-ID (SYMBOLIC-CHAOS-ADDRESS CHAOS:MY-ADDRESS) USE)
	   (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT)
	   (CLOSURE '(REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT)
		    'REMOTE-DISK-HANDLER)))
	(T (VALUES UNIT T))))				;Probably remote disk at higher level

))

; From file DISK.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DISK  "


(DEFUN CC-DISK-HANDLER (OP &REST ARGS)
  (DECLARE (SPECIAL CC-DISK-UNIT CC-DISK-INIT-P CADR:CC-DISK-LOWCORE CADR:CC-DISK-TYPE))
  (SELECTQ OP
    (:READ (LET ((RQB (CAR ARGS)))
	     (LET ((BLOCK (CADR ARGS))
		   (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
	       (DO ((BLOCK BLOCK (1+ BLOCK))
		    (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		    (BUF (RQB-BUFFER RQB))
		    (BUF-IDX -1))
		   ((ZEROP N-BLOCKS))
		 (CADR:CC-DISK-READ BLOCK CADR:CC-DISK-LOWCORE 1)  ;Was 2 - doesn't that clobber the block above LOWCORE?
		 ;; Following code transmogrified from DBG-READ-XBUS and DBG-READ.
		 ;; You don't really think this would be reasonable via DL11 or debug kludge,
		 ;; do you?
		 ;;  Yes it is reasonable, you total fool!  Hacking the disk label is one of 
		 ;;  the most useful things to do.  You really made me do alot of work, and
		 ;;  it was not appreciated one bit.  --HIC
		 (IF (EQ CADR:DBG-ACCESS-PATH 'CADR:BUSINT)
		     (LET ((UBUS-WD-LOC
			     (LSH (CADR:DBG-SETUP-UNIBUS-MAP 17
						   (ASH CADR:CC-DISK-LOWCORE 8)) -1)))
		       (%UNIBUS-WRITE #o766110 0)  ;high unibus adr bit and DBG-NXM-INHIBIT
		       (DOTIMES (W #o400)
			 (%UNIBUS-WRITE #o766114 UBUS-WD-LOC)
			 (AS-1 (%UNIBUS-READ #o766100)
			       BUF
			       (SETQ BUF-IDX (1+ BUF-IDX)))
			 (%UNIBUS-WRITE #o766114 (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC)))
			 (AS-1 (%UNIBUS-READ #o766100)
			       BUF
			       (SETQ BUF-IDX (1+ BUF-IDX)))
			 (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC))))
		     (DO ((ADR (ASH CADR:CC-DISK-LOWCORE 8) (1+ ADR))
			  (WORD)
			  (W 0 (1+ W)))
			 (( W #o400))
		       (SETQ WORD (CADR:PHYS-MEM-READ ADR))
		       (AS-1 (LOGAND #o177777 WORD) BUF (SETQ BUF-IDX (1+ BUF-IDX)))
		       (AS-1 (LDB #o2020 WORD) BUF (SETQ BUF-IDX (1+ BUF-IDX)))))))))
		 
    (:WRITE (LET ((RQB (CAR ARGS)))
	      (LET ((BLOCK (CADR ARGS))
		    (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
		(DO ((BLOCK BLOCK (1+ BLOCK))
		     (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		     (BUF (RQB-BUFFER RQB))
		     (BUF-IDX -1))
		    ((ZEROP N-BLOCKS))
		  (IF (EQ CADR:DBG-ACCESS-PATH 'CADR:BUSINT)
		      (LET ((UBUS-WD-LOC
			      (LSH (CADR:DBG-SETUP-UNIBUS-MAP 17
						    (ASH CADR:CC-DISK-LOWCORE 8)) -1)))
			(%UNIBUS-WRITE #o766110 0)  ;high unibus adr bit and DBG-NXM-INHIBIT
			(DOTIMES (W #o400)
			  (%UNIBUS-WRITE #o766114 UBUS-WD-LOC)
			  (%UNIBUS-WRITE #o766100 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX))))
			  (%UNIBUS-WRITE #o766114 (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC)))
			  (%UNIBUS-WRITE #o766100 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX))))
			  (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC))))
		      (DO ((ADR (ASH CADR:CC-DISK-LOWCORE 8) (1+ ADR))
			   (WORD)
			   (W 0 (1+ W)))
			  (( W #o400))
			(SETQ WORD (DPB (AR-1 BUF (SETQ BUF-IDX (+ 2 BUF-IDX)))
					#o2020
					(AR-1 BUF (1- BUF-IDX))))
			(CADR:PHYS-MEM-WRITE ADR WORD)))
		  ;; If writing label, init some params such as CC:BLOCKS-PER-CYLINDER.
		  (IF (ZEROP BLOCK)
		      (CC:READ-LABEL-1 CADR:CC-DISK-LOWCORE))
	RETRY	  (CADR:CC-DISK-WRITE BLOCK CADR:CC-DISK-LOWCORE 1)
		  (COND ((AND CC-REMOTE-DISK-WRITE-CHECK 
			      (NULL (CADR:CC-DISK-READ BLOCK
						       (1+ CADR:CC-DISK-LOWCORE)
						       1)))
			 (GO RETRY)))		;read it back to let hardware check ECC, etc.
		  ))))

    (:DISPOSE (COND ((NULL CC-DISK-INIT-P)
		     (CADR:CC-DISK-READ 1 CADR:CC-DISK-LOWCORE 1)
		     (CADR:CC-DISK-READ 3 (1+ CADR:CC-DISK-LOWCORE) 1)) ;Restore saved core
		    (T (SETQ CADR:CC-DISK-TYPE NIL))))	;Otherwise read label now that it
							; maybe isnt garbage
    (:UNIT-NUMBER 0)
    (:MACHINE-NAME "via CC")
    (:SAY (FORMAT T "CC-SAY ~A~%" (CAR ARGS)))
    (:HANDLES-LABEL NIL)))

))

; From file GENRIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun member-1 (item list test test-not key)
  ;; Use MEMQ or MEMBER-EQL whenever they will work, since they are microcoded.
  (when (and (null key)
	     (null test-not))
    (cond ((or (eq test 'eq) (eq test #'eq))
	   (return-from member-1 (memq item list)))
	  ((or (null test) (eq test 'eql) (eq test #'eql))
	   (return-from member-1 (member-eql item list)))))
  (do ((tail list (cdr tail)))
      ((null tail))
    (let ((elt (if key (funcall key (car tail)) (car tail))))
      (if (cond (test-not (not (funcall test-not item elt)))
		(test (funcall test item elt))
		(t (eql item elt)))
	  (return tail)))))

))
