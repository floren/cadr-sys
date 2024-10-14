;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:10; Patch-File:T -*-
;;; Patch file for Local-File version 74.1
;;; Reason:
;;;  Don't let :length function on a local-file stream bomb out if there
;;;  is no longer a map -- i.e. the stream was closed.
;;; Written 2-Jun-88 10:31:33 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Cthulhu from band 2
;;; with Experimental System 124.15, Experimental Local-File 74.0, Experimental File-Server 23.1, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.4, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8.


; From modified file DJ: L.FILE; FSSTR.LISP#112 at 2-Jun-88 10:32:59
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSSTR  "

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :MAYBE-UPDATE-MAP-BLOCK-SIZE) (CHAR-INDEX)
  (AND MAP					;Don't do anything if stream closed
       (LET ((CHAR-BITS (* CHAR-INDEX BYTE-SIZE)))
	 (WHEN (< (MAP-BLOCK-SIZE MAP MAP-INDEX) CHAR-BITS)
	   (SETF (MAP-BLOCK-SIZE MAP MAP-INDEX) CHAR-BITS)))))

))

; From modified file DJ: L.FILE; FSSTR.LISP#112 at 2-Jun-88 10:33:05
#10R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; FSSTR  "

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :CURRENT-LENGTH-FROM-MAP) ()
  (AND MAP
       (// (MAP-LENGTH MAP) BYTE-SIZE)))

))
