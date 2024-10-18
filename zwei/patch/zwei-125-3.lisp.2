;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.3
;;; Reason:
;;;  When you were using Dired on a directory one level down from the
;;;  root directory, if you typed the "<" command to edit the directory's
;;;  superior, Dired falsely claimed that there was none.  Now it
;;;  correctly edits the root directory.
;;; Written 25-Jul-88 12:56:47 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Cthulhu from band 3
;;; with System 125.19, ZWEI 125.2, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.1, Lambda-Diag 17.0, Experimental Kermit 36.5, microcode 1761, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.ZWEI; DIRED.LISP#335 at 25-Jul-88 12:56:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-EDIT-SUPERIOR-DIRECTORY
  "Edit the superior directory of the current buffer's directory." ()
  (OR (TYPEP *WINDOW* 'ZMACS-WINDOW) (BARF "Files can only be edited in ZMACS windows."))
  (LET* ((PATHNAME (SEND *INTERVAL* :PATHNAME))
	 (DIRECTORY (SEND PATHNAME :DIRECTORY)))
    (cond ((eq directory :root)
	   (BARF "The root directory has no superior directory."))
	  ((OR (NOT (CONSP DIRECTORY)) (= 1 (LENGTH DIRECTORY)))
	   (DIRECTORY-EDIT
	     (SEND (SEND PATHNAME :DIRECTORY-PATHNAME-AS-FILE) :NEW-PATHNAME
		   :DIRECTORY :root :NAME :WILD :TYPE :WILD :VERSION :WILD)))
	  (t
	   (DIRECTORY-EDIT
	     (SEND (SEND PATHNAME :DIRECTORY-PATHNAME-AS-FILE) :NEW-PATHNAME
		   :DIRECTORY (BUTLAST DIRECTORY)
		   :NAME :WILD :TYPE :WILD :VERSION :WILD))))))

))
