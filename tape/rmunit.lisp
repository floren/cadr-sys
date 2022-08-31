;;; -*- Mode:LISP;Package:Tape;Base:8 -*-
;;;

;; Our RFC packet (to contact name MAGTAPE) contains
;; MAGTAPE r devname
;; r = "R" or "W" saying whether to open for read or for read/write.
;; devname = magtape device name.

;; Following that, we get packets with opcode 200 with data to write on tape,
;; or 201 containing commands.

;; 200 is simple: the data in the packet is written.

;; The data is written into records of a fixed size, regardless of packet boundaries.
;; The record size is specified by the set record size command.
;; You can write a smaller record by sending as much data as you want,
;; followed by a force-output command.

;; We do not reply to data packets.
;; Every so often, you should send an output-status inquiry command
;; to find out whether the output data got an error (such as end of tape).
;; All data packets past the error are ignored, and the reply to the
;; output-status command tells you how many packets were handled (not ignored)
;; which will tell you where the error occurred.

;; The error code returned by the output-status inquiry describes the
;; success, or reason for failure, of the output data packets.
;; If the error code is %ereot (end of tape), then the data in the packet
;; that got the error was all written properly, but following packets were ignored.
;; You should send a couple of write tape mark commands to finish the tape.

;; 201 packets are formatted as follows:

;; Four 8-bit bytes, the first of which contains a command code
;; and the next three of which are a transaction code.

;; Arguments to the command occupy four bytes each,
;; and they are turned into numbers by using the earlier bytes
;; as the less significant bits of the number.

;; Command codes are:

(DEFCONSTANT %MAG-COM-READ 0)	; Read records.  Arg is number of records.  Data
				; is sent back in packets of opcode 200, followed
				; by a 201 reply packet.
(DEFCONSTANT %MAG-COM-READ-FILE 1) ; Read file.  Data is sent back in packets of
				; opcode 200, followed by a 201 reply packet.
(DEFCONSTANT %MAG-COM-REWIND 2)	; rewind
(DEFCONSTANT %MAG-COM-DISMOUNT 3) ; dismount
(DEFCONSTANT %MAG-COM-WMARK 4)	; Write tape mark
(DEFCONSTANT %MAG-COM-SFREC 5)	; Skip forward records.  Argument is number of records.
(DEFCONSTANT %MAG-COM-SBREC 6)	; Skip backward records.  Argument is number of records.
(DEFCONSTANT %MAG-COM-SFFIL 7)	; Skip forward files.  Argument is number of files.
(DEFCONSTANT %MAG-COM-SBFIL 10	; Skip backward files.  Argument is number of files.
(DEFCONSTANT %MAG-COM-SEOT 11)  ; Skip to eot.
(DEFCONSTANT %MAG-COM-WGAP 12)  ; Write gap.
(DEFCONSTANT %MAG-COM-SRSIZ 13) ; Specify record size.  Argument is record size.
				; For input, it's a maximum.  40000 (16k bytes) is the
				; highest value allowed.  If you specify something larger,
				; you get %errec and it is ignored.
(DEFCONSTANT %MAG-COM-FOUT 14)  ; Force output.
(DEFCONSTANT %MAG-COM-NOOP 15)	; no-op
(DEFCONSTANT %MAG-COM-OSTAT 16)	; Output status inquiry:
;;    reply's error code describes results of output data packets
;;    since the previous output status inquiry.
;;    The reply has 8 bytes instead of the usual 4.
;;    The second 4 bytes are an encoding (like command arguments)
;;    of the number of non-ignored data packets since the previous output-status inquiry.

;; We send a reply to the 201 packet.  It has opcode 201.

;; The first 8-bit byte is the error code (0 means success).
;;  The next three echo the transaction code.
;; Unless the command was output-status, the reply has only these four bytes.

;; Here are the error codes

(DEFCONSTANT %ersuc 0) 		;Success
(DEFCONSTANT %ersht 1)		;201 packet too short
(DEFCONSTANT %erop  2)		;bad packet opcode
(DEFCONSTANT %ERCMD 3)		;bad command code in 201 packet.
(DEFCONSTANT %EREOF 4)		;End of file encountered when not expected.
				;Note: some commands are supposed to reach eof.
				;They do not report it as an error.
(DEFCONSTANT %ERDAT 5)		;Data error on magtape
(DEFCONSTANT %ERREC 6)		;Record too long
(DEFCONSTANT %ERRND 7)		;Other random system error
(DEFCONSTANT %EROFL 10)		;Magtape is off line.
(DEFCONSTANT %ERNSD 11)		;No such device (error on RFC only).
(DEFCONSTANT %ERDEV 12)		;Device error
(DEFCONSTANT %ERWLK 13)		;Mag tape write locked
(DEFCONSTANT %EREOT 14)		;End of tape.

(DEFVAR *ERROR-MESSAGES*
	#1A("Success" "Command packet too short" "Bad packet opcode" "Bad command code"
	    "Unexpected end of file" "Data error on magtape" "Record too long"
	    "Random system error" "Magtape is offline" "No such device" "Device error"
	    "Magtape write-locked" "End of tape"))

(DEFCONSTANT %MAGTAPE-DATA-OP 200)
(DEFCONSTANT %MAGTAPE-CMD-OP 201)

(DEFUN PUT-COMMAND-IN-PKT (COMMAND TRANSACTION PKT ARGUMENT)
  "Assumes that ARGUMENT is a fixnum."
  (SETF (CHAOS:PKT-OPCODE PKT) %MAGTAPE-CMD-OP)
  (CHAOS:SET-PKT-STRING PKT COMMAND (LDB 0010 TRANSACTION)
			    (LDB 1010 TRANSACTION) (LBD 2010 TRANSACTION)
			    (LDB 0010 ARGUMENT) (LDB 1010 TRANSACTION)
			    (LBD 2010 TRANSACTION) 0))

(DEFUN GET-REPLY-PKT-CODES (PKT FOR-OUTPUT-STATUS)
  (DECLARE (:VALUES ERROR-OP TRANSACTION NDATA-PKTS))
  (LET ((DATA (CHAOS:PKT-STRING PKT)))
    (VALUES (AREF DATA 0)
	    (DPB (AREF DATA 3) 2010 (DPB (AREF DATA 2) 1010 (AREF DATA 1)))
	    (AND FOR-OUTPUT-STATUS
		 (DPB (AREF DATA 6) 2010
			   (DPB (AREF DATA 5) 1010 (AREF DATA 4)))))))

(DEFFLAVOR MAGTAPE-REMOTE-UNIT ((TRANSACTIONS 0)	; transcation ID/count
				CONN			; chaos connection
				CONN-DIR)		; :READ or :READ-WRITE
	   (BASIC-REMOTE-UNIT)
  )


(DEFMETHOD (MAGTAPE-REMOTE-UNIT :PROTOCOL) () ':MAGTAPE)

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :ACTIVE-P) ()
  (AND CONN (SELECTQ (CHAOS:CONN-STATE CONN)
	      (CHAOS:OPN-STATE T)
	      (CHAOS:RFC-SENT-STATE T)
	      (OTHERWISE ()))))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :EXECUTE-COMMAND-IMMEDIATE) (COM ARG)
  "Assumes no packets in the window; leaves no packets in the window.
Returns NIL (net lossage), or error code."
  (LET ((PKT ()))
    (UNWIND-PROTECT
	(PROGN
	  (SETQ PKT (CHAOS:GET-PKT))
	  (INCF TRANSCATIONS)
	  (PUT-COMMAND-IN-PKT COM TRANSACTIONS PKT ARG)
	  (CHAOS:SEND-PKT CONN PKT %MAGTAPE-COM-OP)
	  (SETQ PKT (CHAOS:GET-NEXT-PKT CONN () "MT Command"))
	  (GET-REPLY-PKT-OPCODES PKT ())))))
    
(DEFMETHOD (MAGTAPE-REMOTE-UNIT :REWIND) ()
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-REWIND 0))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :SPACE-TO-APPEND) ()
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-SEOT 0))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :OFFLINE) ()
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-DISMOUNT 0))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :UNLOAD) () (SEND SELF ':OFFLINE))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :WRITE-EOF) ()
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-WMARK 0))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :SPACE) (NTIMES)
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-SFREC NTIMES))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :SPACE-REV) (NTIMES)
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-SBREC NTIMES))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :SPACE-TO-EOF) (NTIMES)
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-SFFIL NTIMES))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :SPACE-REV-TO-BOF) (NTIMES)
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-SBFIL NTIMES))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :WRITE-GAP) ()
  (SEND SELF ':EXECUTE-COMMAND-IMMEDIATE %MAG-COM-WGAP 0))

(DEFMETHOD (MAGTAPE-REMOTE-UNIT :STREAM-FLAVOR) (DIRECTION CHARACTERS FILE-P)
  (SELECTQ DIRECTION
      (:INPUT
       (IF CHARACTERS
	   (IF FILE-P 'REMOTE-FILE-INPUT-CHARACTER-STREAM 'REMOTE-CHARACTER-INPUT-STREAM)
	 (IF FILE-P 'REMOTE-FILE-INPUT-STREAM 'REMOTE-INPUT-STREAM)))
      (:OUTPUT
       (IF CHARACTERS
	   (IF FILE-P 'REMOTE-FILE-OUTPUT-CHARACTER-STREAM 'REMOTE-CHARACTER-OUTPUT-STREAM)
	 (IF FILE-P 'REMOTE-FILE-OUTPUT-STREAM 'REMOTE-OUTPUT-STREAM)))
    
;;; For now it's the only kind of remote unit...
(DEFPROP :MAGTAPE MAGTAPE-REMOTE-UNIT UNIT-FLAVOR)

(DEFFLAVOR MAGTAPE-PROTOCOL-MIXIN (UNIT CONN STREAM) ; is the chaos stream
	   () ; this is a pure mixin
  )
