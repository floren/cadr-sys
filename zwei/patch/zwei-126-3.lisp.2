;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.3
;;; Reason:
;;;  Fix logic in ED and ZWEI:EDIT-THING.  Prevents an error in cold load
;;;  stream caused by datatype error in STRING-EQUAL.
;;;  
;;;    - Keith and SAZ
;;; Reason:
;;;  Fix logic in ED and ZWEI:EDIT-THING.  Prevents an error in cold load
;;;  stream caused by datatype error from STRING-EQUAL.
;;; Written 29-Aug-88 17:53:24 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.66, Experimental ZWEI 126.2, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; ZMACS.LISP#581 at 29-Aug-88 17:53:28
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
    (SEND SHEET :FORCE-KBD-INPUT `(:EXECUTE EDIT-THING ,THING))
    (SEND SHEET :SELECT)
    (TV:AWAIT-WINDOW-EXPOSURE)))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#581 at 29-Aug-88 17:53:30
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(DEFUN EDIT-THING (THING &AUX (*CURRENT-COMMAND* 'ZED))
  "Select buffer or file according to THING.  Used within the editor.
If THING is T, select a newly created buffer.
If it is a string or pathname, find that file.
If it is a list whose CAR is CALLERS-TO-BE-EDITED
 or FUNCTIONS-TO-BE-EDITED,
 make possibilities for them and visit the first one.
If it is a function or function spec, visit that function's definition."
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COND ((EQ THING T)
	 (MAKE-BUFFER-CURRENT (CREATE-ONE-BUFFER-TO-GO)))
	((TYPEP THING '(OR STRING PATHNAME))
	 (FIND-FILE (MAKE-DEFAULTED-PATHNAME (STRING THING) (PATHNAME-DEFAULTS))))
	((and (typep thing 'list)
	      (typep (car-safe thing) '(or symbol string))
	      (case (intern (car thing) :keyword)
		(:callers-to-be-edited
		 (LIST-ZMACS-CALLERS-TO-BE-EDITED "callers" NIL T
						  (SETUP-ZMACS-CALLERS-TO-BE-EDITED (CDR THING)))
		 t)
		(:functions-to-be-edited
		 (LIST-ZMACS-CALLERS-TO-BE-EDITED "functions waiting to be edited" NIL T
						  (SETUP-ZMACS-CALLERS-TO-BE-EDITED (CDR THING)))
		 t)
		(:directory
		 (IF (= (LENGTH (CDR THING)) 1)
		     (DIRECTORY-EDIT (CADR THING))
		   (DIRECTORY-EDIT-MULTIPLE NIL (CDR THING)))
		 t)
		(:mail
		 (APPLY #'ZMACS-COMPOSE-MESSAGE (CDR THING))
		 t)
		(:source-compare-merge
		 (APPLY #'SOURCE-COMPARE-MERGE-1 (CDR THING))
		 t))))
	((SYS:VALIDATE-FUNCTION-SPEC THING)
	 (EDIT-DEFINITION THING))
	((FUNCTIONP THING T)
	 (EDIT-DEFINITION (FUNCTION-NAME THING)))
	(T
	 (let ((*print-length* 15.))
	   (BARF (format nil "Don't know what to do with ~S" THING))))))



))
