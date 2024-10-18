;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.25
;;; Reason:
;;;  Here we modularize Zwei's COM-INSERT-DATE to call the new function,
;;;  INSERT-FORMATTED-OUTPUT; also, we add the COM-INSERT-CURRENT-USER-ID
;;;  command, which works the same way.
;;;  
;;;  Ideally, the other Meta-X Insert... commands should call the new
;;;  function too (when there's time).
;;; Written 24-Oct-88 14:56:34 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.130, Experimental ZWEI 126.23, Experimental ZMail 74.13, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.



; From modified file DJ: L.ZWEI; COMF.LISP#113 at 24-Oct-88 14:56:41
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(defun insert-formatted-output (printing-function-1 &optional printing-function-2)
  ;;format output of arg1 to point if numeric arg supplied, format output of arg2 if not
  (LET ((STREAM (INTERVAL-STREAM (POINT) (POINT) T)))
    (FUNCALL (IF *NUMERIC-ARG-P* printing-function-1 printing-function-2)
	     STREAM)
    (MOVE-BP (MARK) (POINT))
    (MOVE-BP (POINT) (SEND STREAM :READ-BP))))



))

; From modified file DJ: L.ZWEI; COMF.LISP#113 at 24-Oct-88 14:56:44
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(DEFCOM COM-INSERT-DATE "Print the current date into the buffer.
Calls TIME:PRINT-CURRENT-TIME, or if given an argument TIME:PRINT-CURRENT-DATE" ()
  (insert-formatted-output #'TIME:PRINT-CURRENT-DATE #'TIME:PRINT-CURRENT-TIME)
  DIS-TEXT)

))

; From modified file DJ: L.ZWEI; COMF.LISP#113 at 24-Oct-88 14:56:46
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(defcom com-insert-current-user-id "Print the currently logged in user's name into the buffer at point." ()
  (flet ((upcase-user-id-printer (stream) (format stream "~A" (string-upcase user-id)))
	 (user-id-printer (stream) (format stream "~A" user-id)))
    (insert-formatted-output #'upcase-user-id-printer #'user-id-printer))
  dis-text)


))
