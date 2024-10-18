;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.14
;;; Reason:
;;;  Add Zmacs command Falcon Compile File, and make it available from clicking
;;;    right on a list of buffers.
;;; Written 8-Aug-88 19:27:29 by smh (Steve Haflich) at site Gigamos Cambridge
;;; while running on Harpo from band 3
;;; with Experimental System 126.19, ZWEI 125.13, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.12, SDU ROM 102, kold 4aug88.



; From modified file DJ: L.ZWEI; ZMACS.LISP#579 at 8-Aug-88 19:27:59
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-falcon-COMPILE-FILE "Compile a file." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Compile file:" (PATHNAME-DEFAULTS))))
    (EDITOR-COMPILE-FILE PATHNAME T :compile-fun #'compiler:compile-file-for-falcon))
  DIS-NONE)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#579 at 8-Aug-88 19:28:07
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Compile File for Falcon"
			  TYPEOUT-COMPILE-FILE-for-Falcon NIL "Compile this buffer's source file for the Falcon.")

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#579 at 8-Aug-88 19:28:15
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN TYPEOUT-COMPILE-FILE-for-falcon (BUFFER)
  (UNLESS (BUFFER-PATHNAME BUFFER)
    (BARF "This buffer is not visiting a source file."))
  (EDITOR-COMPILE-FILE (BUFFER-PATHNAME BUFFER) T :compile-fun #'compiler:compile-file-for-falcon)
  DIS-NONE)

(zwei:set-comtab zwei:*zmacs-comtab* () '(("Falcon Compile File" . zwei:com-Falcon-compile-file)))

))
