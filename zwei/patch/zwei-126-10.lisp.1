;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.10
;;; Reason:
;;;  Refinement to READ-BUFFER-NAME: If DEFAULT arg is not a ZMacs buffer,
;;;  set current pathname defaults from current buffer.  This is better than
;;;  an arbitrary set of defaults, e.g. from previous buffer.  Does not
;;;  affect any existing ZMacs commands -- just some that I'm building.
;;; Written 10-Sep-88 01:32:03 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.76, Experimental ZWEI 126.9, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; ZMACS.LISP#584 at 10-Sep-88 01:32:07
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
      (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* default)
    (pathname-defaults *pathname-defaults* *interval*))
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
