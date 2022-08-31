;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 5/11/84 22:28:13 by RpK,
;;; Reason: Serial stream random unibus channel size is more reasonable...
;;; while running on Lisp Machine Five from band 6
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.48, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309, ZM MIT.



; From file SERIAL.LISP PS:<L.IO1> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SERIAL  "

(DEFMETHOD (SERIAL-STREAM-MIXIN :RESET) (&OPTIONAL (ABORT-OUTPUT :ABORT))
    (SEND SELF :CLOSE ABORT-OUTPUT)		;Get rid of unibus channels if any
    ;; Reset the stupid error flags
    (%UNIBUS-WRITE 764166 (LOGIOR 20 UART-COMMAND))
    (%UNIBUS-WRITE 764166 UART-COMMAND)
    (SERIAL-WRITE-MODE UART-MODES)		;Restore mode registers
    (%UNIBUS-READ 764160)			;Flush buffered input character if any
    (SETQ INPUT-UNIBUS-CHANNEL
	  (GET-UNIBUS-CHANNEL 264 764162 2 764160 2
			      (CEILING INPUT-BUFFER-SIZE PAGE-SIZE))
	  OUTPUT-UNIBUS-CHANNEL
	  (GET-UNIBUS-CHANNEL 264 764162 1 764160 1 
			      (CEILING OUTPUT-BUFFER-SIZE PAGE-SIZE)
			      764166 (LOGAND UART-COMMAND 376))
	  ;;Some UARTS cause interrupts on modem changes.  RANDOM-UNIBUS-CHANNEL absorbs
	  ;; these to prevent microcode from bombing because it cant find anyone to give
	  ;; interrupt to.  "DATA" is CSR itself.
	  RANDOM-UNIBUS-CHANNEL
	  (GET-UNIBUS-CHANNEL 264 764162 4 764162 1 1)) ;; PAGE-SIZE
    (%UNIBUS-WRITE 764112 (DPB 1 0701 (%UNIBUS-READ 764112))))	;Turn on interrupt

))

