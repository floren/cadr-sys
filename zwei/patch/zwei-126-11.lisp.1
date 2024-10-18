;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.11
;;; Reason:
;;;  Removes current buffer from completing-reader choice list of selectable
;;;  buffers while a zwei:kill-buffer is done.  If user is killing off last
;;;  buffer in existence, the mini-buffer "Killing the current buffer, select
;;;  which other buffer?" prompt will have no (existing) default, and typing
;;;  <Help> will provide no buffer names for mousing or typing. In this case,
;;;  the user types <Return> and lets Zwei create a new buffer, which is then
;;;  selected as the current buffer.
;;;  
;;;  Thanks to Keith for crucial assistance.
;;; Written 22-Sep-88 16:29:57 by saz at site Gigamos Cambridge
;;; while running on Antonio Salieri from band 1
;;; with Experimental System 126.66, Experimental ZWEI 126.2, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZWEI; ZMACS.LISP#585 at 22-Sep-88 16:29:59
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN KILL-BUFFER (BUFFER &OPTIONAL NO-SAVE-P)
  "Kill BUFFER; remove it from the list which can be selected.
Offers to save it if it is a modified file buffer, unless NO-SAVE-P."
  ;; If the buffer is associated with a file and contains changes, offer to write it out.
  (AND (NOT NO-SAVE-P)
       (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (CONSP (BUFFER-FILE-ID BUFFER))
	   (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER))))
       (FQUERY '(:BEEP T
		 :TYPE :READLINE
		 :CHOICES #,FORMAT:YES-OR-NO-P-CHOICES)
	       "Buffer ~A has been modified, save it first? "
	       (BUFFER-NAME BUFFER))
       (SAVE-BUFFER BUFFER))
  ;; If buffer is current, select something else before killing.
  (when (EQ BUFFER *INTERVAL*)
    ;; Cannot select, or be prompted with, the name of the buffer being killed
    (let* ((*zmacs-buffer-name-alist*
	    (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
	   (new-buffer 
	      (SELECT-BUFFER
		"Killing the current buffer, select which other buffer?"
		'MAYBE)))
      (MUST-REDISPLAY *WINDOW* new-buffer)))
  ;; Anybody who refers to this buffer should be redirected.
  (SEND BUFFER :KILL)
  T)

))
