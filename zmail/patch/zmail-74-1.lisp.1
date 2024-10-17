;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.1
;;; Reason:
;;;  Eliminate error in which user tries a c-m-Y while sending a message
;;;  prior to first Get Inbox of ZMail session.
;;; Written 2-Sep-88 11:33:19 by saz at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 1
;;; with Experimental System 126.67, Experimental ZWEI 126.8, Experimental ZMail 74.0, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental Window-Maker 2.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, Lambda/Falcon Development System.



; From modified file DJ: L.ZMAIL; MAIL.LISP#324 at 2-Sep-88 11:33:20
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFCOM COM-ZMAIL-YANK "Insert the message being replied to" ()
  (LET ((CONFIG *WINDOW-CONFIGURATION*))
    (AND *ONE-WINDOW-AFTER-YANK*
	 (EQ CONFIG :REPLY)
	 (SET-MAIL-WINDOW-CONFIGURATION :SEND))
    (if (not *msg*)
	(barf "There is no current message to yank.")
      (progn (INSERT-MSGS-INTO-WINDOW (IF (SEND *DRAFT-HEADER-WINDOW* :EXPOSED-P)
					  *DRAFT-TEXT-WINDOW* *MSG-WINDOW*)
				      (NOT *NUMERIC-ARG-P*))
	     (AND *PRUNE-HEADERS-AFTER-YANKING*
		  (COM-PRUNE-YANKED-HEADERS))
	     (SETQ *CURRENT-COMMAND-TYPE* 'YANK))))
  DIS-TEXT)

))
