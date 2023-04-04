;;; -*-Mode:Lisp;Package:Tape;Base:8-*-
;;; (c) 1984 Lisp Machine Incorporated
;;; RQB allocation functions for local tape software.
;;; Both the CADR and the Lambda need this file loaded, but not a machine using remote tape.
;;; Much of the code comes from MTSTR

(DEFVAR PAGE-SIZE-IN-BYTES (* PAGE-SIZE 4.))	;8 bit bytes.

;; Get a buffer of the correct byte size
(DEFUN MT-GET-RQB-ARRAY FS:(RQB BYTE-SIZE &AUX TYPE)
  FS:
  (COND ((= BYTE-SIZE 20) (RQB-BUFFER RQB))
	((= BYTE-SIZE 10) (RQB-8-BIT-BUFFER RQB))
	((SETQ TYPE (CDR (ASSQ BYTE-SIZE '((4 . ART-4B) (2 . ART-2B) (1 . ART-1B)))))
	 (MAKE-ARRAY (// (* (RQB-NPAGES RQB) PAGE-SIZE-IN-BITS) BYTE-SIZE)
		     ':AREA LOCAL-FILE-SYSTEM-AREA
		     ':TYPE TYPE
		     ':DISPLACED-TO RQB
		     ;; This is a system bug.  Don't try to figure it out.
		     ':DISPLACED-INDEX-OFFSET (* (%P-CONTENTS-OFFSET (RQB-BUFFER RQB) 3)
						 (// 20 BYTE-SIZE))))
	(T (FERROR NIL "~D is an invalid byte size." BYTE-SIZE))))

;; This must be used rather than SI:WIRE-DISK-RQB so not to hack the CCW list.
(DEFUN WIRE-MT-RQB (RQB &OPTIONAL (WIRE-P T) SET-MODIFIED
		    &AUX (LONG-ARRAY-FLAG (%P-LDB SYS:%%ARRAY-LONG-LENGTH-FLAG RQB))
			 (LOW (- (%POINTER RQB) (ARRAY-DIMENSION RQB 0) 2))
			 (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
				  (FLOOR (ARRAY-LENGTH RQB) 2))))
  (DO LOC (LOGAND LOW (- SYS:PAGE-SIZE)) (+ LOC SYS:PAGE-SIZE) (>= LOC HIGH)
    (SI:WIRE-PAGE LOC WIRE-P SET-MODIFIED)))

(DEFUN UNWIRE-MT-RQB (RQB)
  (WIRE-MT-RQB RQB NIL))