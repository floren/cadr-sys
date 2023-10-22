;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.16
;;; Reason:
;;;  SUPDUP: Change overprint default to NIL; so that things work better with ITS.
;;; Written 6-Jun-23 16:55:52 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.13, Hacks by AMS 2.0, microcode 323, WIP.



; From file FC: /sys/window/supdup.lisp at 6-Jun-23 16:55:52
#8R SUPDUP#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SUPDUP")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//window//supdup"

(DEFFLAVOR BASIC-NVT
	   ((ESCAPE-CHAR #/NETWORK)	;Escape character (in Lisp machine character set)
	    (CONNECTION NIL)		;The connection itself
	    (CONNECT-TO NIL)		;Host to connect to (for TYPEIN-TOP-LEVEL)
	    STREAM			;A stream to the above
	    (TERMINAL-STREAM NIL)	;Stream for output. If NIL, (which is the usual case)
					; output to SELF.
	    (OUTPUT-BUFFER (MAKE-STRING #o200 :FILL-POINTER 0))
	    TYPEOUT-PROCESS		;Network to screen
	    TYPEIN-PROCESS		;Keyboard to network
	    (OUTPUT-LOCK NIL)		;Some typeout occurs in TYPEIN-PROCESS
	    (RETURN-TO-CALLER NIL)	;Set to T when :TYPEIN-TOP-LEVEL should return
	    (OVERPRINT NIL)		;NIL means erase chars before outputing.
	    (BLACK-ON-WHITE NIL)
	    (ALIAS-WINDOW NIL)		;Our :ALIAS-FOR-SELECTED-WINDOWS, if non-NIL.
	    PROGRAM-NAME)		;In the "Connect to host" message and help message.
	    ()
  (:REQUIRED-FLAVORS TV:LABEL-MIXIN TV:STREAM-MIXIN TV:SHEET)
  (:GETTABLE-INSTANCE-VARIABLES CONNECTION STREAM OUTPUT-BUFFER ALIAS-WINDOW OVERPRINT)
  (:INITABLE-INSTANCE-VARIABLES ESCAPE-CHAR TYPEIN-PROCESS TYPEOUT-PROCESS PROGRAM-NAME)
  (:SETTABLE-INSTANCE-VARIABLES CONNECT-TO TERMINAL-STREAM BLACK-ON-WHITE OVERPRINT)
  (:REQUIRED-METHODS :CONNECT :GOBBLE-GREETING :NET-OUTPUT :NET-OUTPUT-TRANSLATED)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION ':NOTIFY)
  (:DOCUMENTATION :SPECIAL-PURPOSE "Network virtual terminal windows"))
))

;; Zap existing supdup windows if there are any.
(let ((w (tv:find-window-of-flavor 'supdup:supdup)))
 (when w (send w :set-overprint nil)))