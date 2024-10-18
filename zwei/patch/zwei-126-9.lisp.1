;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.9
;;; Reason:
;;;  ZMacs' READ-BUFFER-NAME is supposed to support creation of a new buffer;
;;;  but that feature didn't have a prayer of working, as implemented.  Now
;;;  that works.  Also, improve logic and user messages generally.
;;; Written 8-Sep-88 20:59:22 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.73, Experimental ZWEI 126.8, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; ZMACS.LISP#583 at 8-Sep-88 20:59:23
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN READ-BUFFER-NAME (PROMPT DEFAULT
			 &OPTIONAL
			 IMPOSSIBLE-IS-OK-P
			 (BUFFER-HISTORY (SEND *WINDOW* :BUFFER-HISTORY))
			 &aux default-string)
  "Read a buffer name in the mini buffer and return a buffer.
DEFAULT is the default to use; T means use most recent buffer other than the current one.
PROMPT is a string to prompt with; should end in a colon if you want one.
IMPOSSIBLE-IS-OK can be T, NIL or MAYBE.
 T means create a new buffer if name does not complete.
 ZWEI:MAYBE means do so, but user must type Return twice to confirm.
 NIL means don't allow names that don't complete, at all.
BUFFER-HISTORY is a history object containing buffers.
 It defaults to *WINDOW*'s buffer history."
  (declare (values buffer))
  (if  (EQ DEFAULT T)	; Select most recent buffer other than this one
       (SETQ DEFAULT (PREVIOUS-BUFFER)))
  (setq default-string
	 (typecase default
	   (null nil)
	   (zmacs-buffer (BUFFER-NAME DEFAULT))
	   ((or string symbol) (string default))
	   (t (format nil "~A" default))))
  (SETQ PROMPT (format nil
		       "~A~@[  (~A)~]  <Use Control-Shift-F to specify a filename>"
		       prompt
		       default-string))
  ;;For C-Shift-F, set pathname defaults :
  (if (typep default 'zmacs-buffer)
      (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* default))
  ;;Get name, and find or (maybe) create requested buffer:
  (LET* ((*READ-BUFFER-KLUDGE* T)
	 (*MINI-BUFFER-DEFAULT-STRING* default-string)
	 (*COMPLETING-DELIMS* '(#/Space #/- #/. #/\ #// #/#))
	 (*MINI-BUFFER-VALUE-HISTORY* BUFFER-HISTORY)
	 (NAME (COMPLETING-READ-FROM-MINI-BUFFER
		 PROMPT *ZMACS-BUFFER-NAME-ALIST* IMPOSSIBLE-IS-OK-P)))
    (cond
      ((typep name 'zmacs-buffer) name)		;Got buffer from C-Shift-F
      ((consp name) (cdr name))			;Requested buffer already exists
      (t
       (if (equal name "")			;User pressed <Return>
	   (setq name default-string))
       (or (and name (find-buffer-named name))	;Look for default buffer, maybe
	   (when impossible-is-ok-p		;Create a new buffer
	     (format *query-io* "~&(New Buffer)")
	     (apply 'create-one-buffer-to-go	;Null default? Generate name
		    (if name (ncons name))))
	   ;;Punt with appropriate message:
	   (barf "There is no ~:[default buffer~;buffer named ~:*~A~]" name))))))

))
