;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.17
;;; Reason:
;;;  "(ed) or (ed nil) simply enters the editor, leaving you in the same state
;;;  as the last time you were in the editor."   ---CLtL, p. 442.  
;;;  
;;;  Previously, both invocations read in the system file TYPES.LISP and moved
;;;  *point* to the deftype for NIL, in that file.
;;; Written 10-Oct-88 18:28:47 by saz at site Gigamos Cambridge
;;; while running on Johannes Brahms from band 3
;;; with Experimental System 126.103, Experimental ZWEI 126.14, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; ZMACS.LISP#588 at 10-Oct-88 18:28:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN ED (&OPTIONAL THING)
  "Select an editor window and look at THING, which
 is normally a function definition to edit.
If THING is T, select a newly created buffer.
If THING is a string or pathname, find that file.
If THING is a list whose CAR is CALLERS-TO-BE-EDITED
 or FUNCTIONS-TO-BE-EDITED,
 make possibilities for them and visit the first one.
If THING is 'ZWEI:RELOAD, reinitialize the editor.
If THING is a function or a function spec, visit that function's definition."
  (when (EQ THING 'RELOAD)
    (return-from ed
      (INITIALIZATIONS '*EDITOR-INITIALIZATION-LIST* T)))
  (LET ((SHEET (FIND-OR-CREATE-IDLE-ZMACS-WINDOW)))
    ;; And select something according to our argument.
    ;; "(ed) or (ed nil) simply enters the editor, leaving you in the same
    ;; state as the last time you were in the editor." --CLtL, p. 442
    (when thing (SEND SHEET :FORCE-KBD-INPUT `(:EXECUTE EDIT-THING ,THING)))
    (SEND SHEET :SELECT)
    (TV:AWAIT-WINDOW-EXPOSURE)))

))
