;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for MagTape version 14.3
;;; Reason: get-rqb-array
;;; Written 4/25/83 03:56:16 by LMFILE,
;;; while running on Lisp Machine Filecomputer from band 2
;;; with Experimental MIT-Specific 19.0, Experimental System 94.6, Experimental ZMail 50.2, Experimental Local-File 44.0, FILE-Server 6.5, MagTape 14.2, Experimental LFS 2.0, microcode 238, FC.



; From file FSDEFS.LISP SRC:<L.FILE> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE; FSDEFS  "

;; Get a buffer of the correct byte size
(DEFUN GET-RQB-ARRAY (RQB BYTE-SIZE &AUX TYPE)
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

))
