;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.6
;;; Reason:
;;;  Add :COMPILE-FUN argument to control which compiler to use.
;;; Written 27-Jul-88 22:00:28 by RWK (Robert W. Kerns) at site Gigamos Cambridge
;;; while running on Claude Debussy from band 3
;;; with ZWEI 125.5, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, Experimental System 126.2, microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, Kenv 7/22/88.



; From modified file DJ: L.ZWEI; ZMACS.LISP#578 at 27-Jul-88 22:00:34
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN EDITOR-COMPILE-FILE (PATHNAME &OPTIONAL EVEN-IF-UNCHANGED &key (compile-fun #'compile-file))
  "Compile PATHNAME if it has changed or if EVEN-IF-UNCHANGED.
If PATHNAME is visited in an editor buffer and has changed,
offers to save the buffer first."
  (LET* ((GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
	 JUST-WRITTEN
	 PKG
	 SUCCESS
	 (compile-in-roots-prop (get generic-pathname :compile-in-roots))
	 (package-from-pathname (get generic-pathname :package))
	 (intended-package (cond (package-from-pathname
				  (find-package package-from-pathname *package*))
				 (T *package*)))
	 (package-from-hierarchy-stuff nil))
    (cond ((and compile-in-roots-prop
		(not (cl:member (si:package-root-name intended-package)
				compile-in-roots-prop
				      :test 'string-equal)))
	   (cond ((not (= 1 (length compile-in-roots-prop)))
		  (fsignal "The current heirarchy ~S is not among those acceptable ~s."
			   (si:package-root-name intended-package)
			   compile-in-roots-prop))
		 (t (format *query-io*
			    "  Transferring to hierarchy ~s" (car compile-in-roots-prop))
		    (setq package-from-hierarchy-stuff
			  (find-package (si:pkg-name intended-package)
					(pkg-find-package (car compile-in-roots-prop))))))))
    (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
      (LET ((BUFFER (CDR ELT)))
	(WHEN (EQ (BUFFER-GENERIC-PATHNAME BUFFER)
		  GENERIC-PATHNAME)
	  (SETQ PKG (BUFFER-PACKAGE BUFFER))
	  (AND (BUFFER-NEEDS-SAVING-P BUFFER)
	       (FQUERY () "Save buffer ~A first? " (BUFFER-NAME BUFFER))
	       (PROGN (SETQ JUST-WRITTEN T)
		      (SAVE-BUFFER BUFFER))))))
    (IF (OR EVEN-IF-UNCHANGED
	    JUST-WRITTEN
	    (AND (FILE-HAS-CHANGED PATHNAME)
		 (Y-OR-N-P (FORMAT NIL "File ~A has changed.  Recompile it? " PATHNAME))))
	(UNWIND-PROTECT
	  (PROGN
	    (FORMAT *QUERY-IO* "~&Compiling ~A" PATHNAME)
	    (funcall compile-fun PATHNAME :PACKAGE (cond (package-from-hierarchy-stuff) (t PKG)))
	    (SETQ SUCCESS T))
	  (IF SUCCESS
	      (FORMAT *QUERY-IO* "~&~A compiled." PATHNAME)
	    (FORMAT *QUERY-IO* " -- Compilation aborted."))))))

))
