;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 75.1
;;; Reason:
;;;  The disk space description for the root directory used to say
;;;    .... (xxx pages used in )
;;;  but now it says
;;;    .... (xxx pages used in ~;)
;;; Written 28-Jul-88 12:34:34 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 1
;;; with Experimental System 126.2, ZWEI 125.4, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.FILE; FSGUTS.LISP#445 at 28-Jul-88 12:36:07
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSGUTS  "

(DEFUN LMFS-DISK-SPACE-DESCRIPTION (&OPTIONAL DIRECTORY &AUX COMMA (BASE 10.) (*NOPOINT T))
  (WITH-OUTPUT-TO-STRING (STREAM)
    (LOOP FOR SYM IN '(PUT-FREE PUT-RESERVED PUT-USED PUT-UNUSABLE)
	  FOR STRING IN '("Free=" "Reserved=" "Used=" "Unusable=")
	  AS TEM = (AREF PUT-USAGE-ARRAY (SYMEVAL SYM))
	  WHEN (PLUSP TEM)
	    DO (AND COMMA (FUNCALL STREAM :STRING-OUT ", "))
	       (FUNCALL STREAM :STRING-OUT STRING)
	       (SI:PRINT-FIXNUM TEM STREAM)
	       (SETQ COMMA T))
    (when DIRECTORY	;If a directory is given, list pages used in that directory.
      (FORMAT STREAM " (~D page~:P used in ~A)"
	      (+ (MAP-NPAGES (FILE-MAP DIRECTORY))
		 (LOOP FOR FILE IN (READ-DIRECTORY-FILES DIRECTORY)
		       SUM (MAP-NPAGES (FILE-MAP FILE))))
	      (LM-NAMESTRING NIL NIL (or (DIRECTORY-FULL-NAME DIRECTORY) :root) NIL NIL NIL)))))

))
