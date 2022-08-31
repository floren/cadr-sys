;;-*-Mode:Lisp; Package:FILE-SYSTEM; Base: 8 -*-

;Protocol on file server connections:

;Output data to the file is simply sent to the file server
;using packets with opcode FILE-BINARY-OPCODE and FILE-CHARACTER-OPCODE.

;Input data from the file is sent the same way.
;A packet with opcode FILE-EOF-OPCODE and no data signals end of file.

;Aside from data, there are commands and replies.  Commands
;go from the user to the server and replies acknowledge them.
;Commands are sent in packets with opcode FILE-COMMAND-OPCODE
;and replies are sent in packets with opcode FILE-REPLY-OPCODE.
;The data in a command packet is a string which can be read in as a list.
;The first element of the list is the command sequence number.
;The reply to that command will contain the same sequence number.
;The second element of the command list is the command name.
;The following elements' meanings depend on the specific command.

;When a command is done on a file connection
;being used for reading from the file, it is necessary to be
;able to read past the input data in the pipeline to get at
;the command reply.  However this case doesn't actually occur often.

;One kind of command is setting the access pointer.
;When this is done on an output file, the reply can be ignored,
;but when it is done on an input file, the input data in the pipeline
;must be discarded until the reply comes back.

;Unexpected problems such as disk full in the file server
;cause a packet with opcode FILE-LOSSAGE-OPCODE to be sent.
;No further output from the file server can be expected
;until the lossage clears itself up, which can take any
;amount of time.  It is not wise to keep sending input to the
;file server after this, though it won't do much harm;
;it will just swell up the buffers in the chaosnet code.
;Eventually the lossage will clear up and the file server will
;send a FILE-WINNAGE-OPCODE packet.

;The contents of the lossage packet consists of a list
;starting with a keyword giving the type of lossage,
;followed by the PDP-10 IOC error code for it,
;followed by a string for the user,
;followed perhaps by arguments giving details.
;The winnage packet contains no data.

;Unexpected total lossage can cause a CLOSE packet to be sent.
;The contents are the same as those of a temporary lossage packet.


;If a command, reply, or lossage requires a string
;too long for one packet, it is divided into several packets.
;The last packet has the opcode appropriate to the type of
;message, and the preceding packets have opcode FILE-CONTINUED-OPCODE.
;Data packets never need to be continued because the division
;of data among packets is not significant.


;FILE-NEXT-NODE-OPCODE is used to tell REMOTE that the next
;subnode of the file being read has started.  The data in the
;packet is the node name.  Continued packets are used in the
;normal way if the name is very long.

(DEFCONST FILE-BINARY-OPCODE (LOGIOR 100 CHAOS:DAT-OP))
(DEFCONST FILE-CHARACTER-OPCODE CHAOS:DAT-OP)
(DEFCONST FILE-COMMAND-OPCODE (1+ CHAOS:DAT-OP))
(DEFCONST FILE-REPLY-OPCODE (+ 2 CHAOS:DAT-OP))
(DEFCONST FILE-LOSSAGE-OPCODE (+ 3 CHAOS:DAT-OP))
(DEFCONST FILE-CONTINUED-OPCODE (+ 4 CHAOS:DAT-OP))
(DEFCONST FILE-EOF-OPCODE (+ 5 CHAOS:DAT-OP))
(DEFCONST FILE-WINNAGE-OPCODE (+ 6 CHAOS:DAT-OP))
(DEFCONST FILE-NEXT-NODE-OPCODE (+ 7 CHAOS:DAT-OP))

(DEFVAR PKG-FILE-SYSTEM-PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM"))

;A file command is simply a packet with opcode FILE-COMMAND-OPCODE
;containing a string of this format:
;(seqnum commandname arguments)
;The reply looks like
;(seqnum command <stream-exists-p> <value1> <value2> ...<valueN>)
;Command just helps match up commands and replies when debugging.

;Since pathnames are sent around so much, a special syntax is defined for them.
;The syntax is only understood by these streams:
; #P<string-containing-hostname><string-for-host>
;as in #P"LFS""FOO\BAR#69"

;If READ-PATHNAME-HOST is non-NIL, it overrides the hostname which
;follows the #P.  The remote end uses this to stick the remote hostname
;in place of the local hostname from the server, which is what appears in the #P.
;The server end leaves this variable NIL, so that the remote end can
;tell it what hostname to use.
(DEFVAR READ-PATHNAME-HOST NIL)

;This variable, if not NIL, says which host name to output in a #P,
;overriding the one in the pathname being transmitted.
;The remote end uses this to tell the server end which host to use locally.
(DEFVAR SERVER-LOCAL-HOST NIL)

(DEFUN READ-PATHNAME (IGNORE STREAM)
  (LET ((HOST (READ STREAM)))
    (MERGE-PATHNAME-DEFAULTS (PARSE-PATHNAME (READ STREAM) (OR READ-PATHNAME-HOST HOST)))))

;Certain frequently transmitted symbols are encoded as #Snumber.
;This is the list of symbols recognized.  Number is the index in this list.
(DEFVAR STANDARD-SYMBOL-LIST
	`(:CREATION-DATE :REFERENCE-DATE :AUTHOR
	  :LENGTH-IN-BLOCKS :LENGTH-IN-BYTES :BYTE-SIZE :CHARACTERS
	  :TRUENAME :LINK-TO :DELETED :DELETE-PROTECT :SUPERSEDE-PROTECT
	  :PACK-NUMBER :VOLUME-NAME :DUMPED :PDP10-FORMAT :QFASLP
	  :FLAVOR :DIRECTORY :PDIR :STRING :LINK :HARD-LINK :PDP10
	  NIL T STREAM ERROR :INPUT :OUTPUT
	  ;; These are in here so that the first command from REMOTE
	  ;; will use a standard symbol iff REMOTE knows how.
	  OPEN CLOSE NEXT-NODE DIRECTORY-LIST DIRECTORY-LIST-STREAM
	  MULTIPLE-FILE-PLISTS
	  ;; More stream properties.
	  :MUST-EXPLICITLY-CLOSE :LENGTH :INFO :NOT-BACKED-UP :DATE-LAST-EXPUNGE
	  :DONT-DELETE :DONT-SUPERSEDE :DONT-REAP
	  ;; The nonstandard operations that LMFILE offers.
	  :RENAME :DELETE :UNDELETE :EXPUNGE
	  :SET-AUTHOR :SET-CREATION-DATE :FILE-OPERATION
	  ))

;This list is an initial segment of the previous one.
;It says which symbols should actually be encoded.
;The two lists are normally the same.  But when new symbols are added,
;first you add them to the end of the previous list (so they are recognized).
;Then you install new REMOTE and SERVER.  Then you change this list and install again.
(DEFVAR STANDARD-SYMBOL-OUTPUT-LIST
	`(:CREATION-DATE :REFERENCE-DATE :AUTHOR
	  :LENGTH-IN-BLOCKS :LENGTH-IN-BYTES :BYTE-SIZE :CHARACTERS
	  :TRUENAME :LINK-TO :DELETED :DELETE-PROTECT :SUPERSEDE-PROTECT
	  :PACK-NUMBER :VOLUME-NAME :DUMPED :PDP10-FORMAT :QFASLP
	  :FLAVOR :DIRECTORY :PDIR :STRING :LINK :HARD-LINK :PDP10
	  NIL T STREAM ERROR :INPUT :OUTPUT
	  ;; These are in here so that the first command from REMOTE
	  ;; will use a standard symbol iff REMOTE knows how.
	  OPEN CLOSE NEXT-NODE DIRECTORY-LIST DIRECTORY-LIST-STREAM
	  MULTIPLE-FILE-PLISTS
	  ;; More stream properties.
	  :MUST-EXPLICITLY-CLOSE :LENGTH :INFO :NOT-BACKED-UP :DATE-LAST-EXPUNGE
	  :DONT-DELETE :DONT-SUPERSEDE :DONT-REAP
	  ;; The nonstandard operations that LMFILE offers.
	  :RENAME :DELETE :UNDELETE :EXPUNGE
	  :SET-AUTHOR :SET-CREATION-DATE :FILE-OPERATION
	  ))

;SERVER should not encode standard symbols unless it sees REMOTE doing so.
(DEFVAR USE-STANDARD-SYMBOL-ENCODING)

(DEFUN READ-STANDARD-SYMBOL (IGNORE STREAM)
  ;; Set it to T, but get error if it is not bound locally.
  (OR USE-STANDARD-SYMBOL-ENCODING (SETQ USE-STANDARD-SYMBOL-ENCODING T))
  (DO (CH
       (NUMBER 0))
      (())
    (SETQ CH (FUNCALL STREAM ':TYI))
    (IF (<= #/0 CH #/9)
	(SETQ NUMBER (+ (* 10. NUMBER) (- CH #/0)))
      (FUNCALL STREAM ':UNTYI CH)
      (RETURN (NTH NUMBER STANDARD-SYMBOL-LIST)))))

(DEFVAR FILE-READTABLE (COPY-READTABLE SI:INITIAL-READTABLE))

(SET-SYNTAX-#-MACRO-CHAR #/P 'READ-PATHNAME FILE-READTABLE)

(SET-SYNTAX-#-MACRO-CHAR #/S 'READ-STANDARD-SYMBOL FILE-READTABLE)

(DEFMACRO FILE-BIND-FOR-READ (&BODY BODY)
  `(LET ((READTABLE FILE-READTABLE)
	 (IBASE 10.)
	 (PACKAGE PKG-FILE-SYSTEM-PACKAGE))
     . ,BODY))

(DEFMACRO FILE-BIND-FOR-WRITE (&BODY BODY)
  `(LET ((BASE 10.)
	 (PRINLEVEL NIL)
	 (PRINLENGTH NIL)
	 (*NOPOINT T)
	 (PACKAGE PKG-FILE-SYSTEM-PACKAGE))
     . ,BODY))

;Used by both SERVER and REMOTE.

;Commands and replies are read and printed in decimal.  The default package is FS.

(DEFVAR FILE-CONTINUED-PKTS)	;holds continued pkts until main op received.

;extract list from pkt-stream in file-continued-pkts. Strings may be in there too.
(DEFUN FILE-EXTRACT-LIST ()
  (DECLARE (RETURN-LIST LIST PKT-OPCODE))
  (FILE-BIND-FOR-READ
    (READ-FROM-PKT-LIST)))

(DEFUN RETURN-CONTINUED-PKTS ()
  (DOLIST (PKT FILE-CONTINUED-PKTS)
    (COND ((STRINGP PKT))
	  (T (CHAOS:RETURN-PKT PKT))))
  (SETQ FILE-CONTINUED-PKTS NIL))

(LOCAL-DECLARE ((SPECIAL *CHAR-POS*))
(DEFUN READ-FROM-PKT-LIST (&OPTIONAL (EOF-OPTION 'NO-EOF-OPTION)
			   (START-CHAR-POSITION 0)
			   &AUX (*CHAR-POS* START-CHAR-POSITION) PKT-OPCODE)
  (DECLARE (RETURN-LIST CONTENTS PKT-OPCODE))
  (VALUES (PROG1 (READ 'FILE-PKT-STREAM-IN EOF-OPTION)
		 (COND (FILE-CONTINUED-PKTS
			(LET ((PKT (CAR FILE-CONTINUED-PKTS)))
			  (COND ((NOT (STRINGP PKT))
				 (SETQ PKT-OPCODE (CHAOS:PKT-OPCODE PKT))
				 (CHAOS:RETURN-PKT PKT))))
			(SETQ FILE-CONTINUED-PKTS (CDR FILE-CONTINUED-PKTS)))))
	  PKT-OPCODE))

(DEFPROP FILE-PKT-STREAM-IN T SI:IO-STREAM-P)

(DEFUN FILE-PKT-STREAM-IN (OPERATION &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OPERATION ':TYI)
	 (PROG (STRING) TOP
	       (OR FILE-CONTINUED-PKTS
		   (RETURN ARG1))
	       (SETQ STRING (COND ((STRINGP (CAR FILE-CONTINUED-PKTS))
				   (CAR FILE-CONTINUED-PKTS))
				  (T (CHAOS:PKT-STRING (CAR FILE-CONTINUED-PKTS)))))
	       (COND ((>= *CHAR-POS* (ARRAY-ACTIVE-LENGTH STRING))
		      (IF (NOT (STRINGP (CAR FILE-CONTINUED-PKTS)))
			  (CHAOS:RETURN-PKT (CAR FILE-CONTINUED-PKTS)))
		      (IF (NULL (SETQ FILE-CONTINUED-PKTS (CDR FILE-CONTINUED-PKTS)))
			  (RETURN ARG1)
			  (SETQ *CHAR-POS* 0)
			  (GO TOP)))
		     (T (RETURN (PROG1 (AR-1 STRING *CHAR-POS*)
				       (SETQ *CHAR-POS* (1+ *CHAR-POS*))))))))
	((EQ OPERATION ':UNTYI)
	 (SETQ *CHAR-POS* (1- *CHAR-POS*)))
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYI :UNTYI))
	(T (STREAM-DEFAULT-HANDLER 'FILE-PKT-STREAM-IN OPERATION ARG1 REST)))) )

(DEFUN FILE-SEND-MESSAGE (*CONN* OPCODE LIST)
  (LET ((*PKT* (CHAOS:GET-PKT)))
    (DECLARE (SPECIAL *PKT* *CONN*))
    (FILE-BIND-FOR-WRITE
      (PRIN1 LIST 'FILE-PKT-STREAM-OUT)
      (SETF (CHAOS:PKT-NBYTES *PKT*)
	    (ARRAY-ACTIVE-LENGTH (CHAOS:PKT-STRING *PKT*)))
      (CHAOS:SEND-PKT *CONN* *PKT* OPCODE))))
  
(DEFPROP FILE-PKT-STREAM-OUT T SI:IO-STREAM-P)

(DEFUN FILE-PKT-STREAM-OUT (OPERATION &OPTIONAL ARG1 &REST REST &AUX TEM)
  (DECLARE (SPECIAL *PKT* *CONN*))
  (COND ((EQ OPERATION ':TYO)
	 (COND ((NOT (< (ARRAY-ACTIVE-LENGTH (CHAOS:PKT-STRING *PKT*))
			(ARRAY-LENGTH (CHAOS:PKT-STRING *PKT*))))
		(SETF (CHAOS:PKT-NBYTES *PKT*)
		      (ARRAY-ACTIVE-LENGTH (CHAOS:PKT-STRING *PKT*)))
		(CHAOS:SEND-PKT *CONN* *PKT* FILE-CONTINUED-OPCODE)
		(SETQ *PKT* (CHAOS:GET-PKT))))
	 (ARRAY-PUSH (CHAOS:PKT-STRING *PKT*) ARG1)
	 ARG1)
	((EQ OPERATION ':PRINT)
	 (COND ((CONSP ARG1)
		(SI:PRINT-LIST ARG1 (FIRST REST)
			       'FILE-PKT-STREAM-OUT '(:TYO :PRINT))
		T)
	       ((TYPEP ARG1 'PATHNAME)
		(FILE-PKT-STREAM-OUT ':TYO #/#)
		(FILE-PKT-STREAM-OUT ':TYO #/P)
		(PRIN1 (OR SERVER-LOCAL-HOST
			   (FUNCALL (FUNCALL ARG1 ':HOST)
				    ':NAME-AS-FILE-COMPUTER))
		       'FILE-PKT-STREAM-OUT)
		(SI:PRINT-OBJECT (FUNCALL ARG1 ':STRING-FOR-HOST)
				 (FIRST REST)
				 'FILE-PKT-STREAM-OUT NIL)
		T)
	       ((AND (SYMBOLP ARG1)
		     USE-STANDARD-SYMBOL-ENCODING
		     (SETQ TEM (FIND-POSITION-IN-LIST ARG1 STANDARD-SYMBOL-OUTPUT-LIST)))
		(FILE-PKT-STREAM-OUT ':TYO #/#)
		(FILE-PKT-STREAM-OUT ':TYO #/S)
		(PRIN1 TEM 'FILE-PKT-STREAM-OUT)
		T)))				;Returns NIL if not handled.
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYO :PRINT))
	(T (STREAM-DEFAULT-HANDLER 'FILE-PKT-STREAM-OUT OPERATION ARG1 REST))))


;; This is used by OPEN-SERVER-CONNECTION.

(DEFVAR FILE-RFC-STREAM-OUT-PKT-STRING)

(DEFPROP FILE-RFC-STREAM-OUT T SI:IO-STREAM-P)

(DEFUN FILE-RFC-STREAM-OUT (OPERATION &OPTIONAL ARG1 &REST REST &AUX TEM)
  (COND ((EQ OPERATION ':TYO)
	 (OR (ARRAY-PUSH FILE-RFC-STREAM-OUT-PKT-STRING ARG1)
	     (*THROW 'RFC-FAIL NIL)))
	((EQ OPERATION ':PRINT)
	 (COND ((CONSP ARG1)
		(SI:PRINT-LIST ARG1 (FIRST REST)
			       'FILE-RFC-STREAM-OUT '(:TYO :PRINT))
		T)
	       ((TYPEP ARG1 'PATHNAME)
		(FILE-RFC-STREAM-OUT ':TYO #/#)
		(FILE-RFC-STREAM-OUT ':TYO #/P)
		(PRIN1 (OR SERVER-LOCAL-HOST
			   (FUNCALL (FUNCALL ARG1 ':HOST)
				    ':NAME-AS-FILE-COMPUTER))
		       'FILE-RFC-STREAM-OUT)
		(SI:PRINT-OBJECT (FUNCALL ARG1 ':STRING-FOR-HOST)
				 (FIRST REST)
				 'FILE-RFC-STREAM-OUT NIL)
		T)
	       ((AND (SYMBOLP ARG1)
		     USE-STANDARD-SYMBOL-ENCODING
		     (SETQ TEM (FIND-POSITION-IN-LIST ARG1 STANDARD-SYMBOL-OUTPUT-LIST)))
		(FILE-RFC-STREAM-OUT ':TYO #/#)
		(FILE-RFC-STREAM-OUT ':TYO #/S)
		(PRIN1 TEM 'FILE-RFC-STREAM-OUT)
		T)))				;Returns NIL if not handled.
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYO :PRINT))
	(T (STREAM-DEFAULT-HANDLER 'FILE-RFC-STREAM-OUT OPERATION ARG1 REST))))

;;; File stream types: they are INPUT, OUTPUT and PROBE.
;;; The stream type is decided at in server, transmitted to the remote,
;;; which uses it to decide which flavor to use.
;;; If the INITIAL-TRANSMIT-DATA property of the stream type is T,
;;; then the server spontaneously starts transmitting data (an "input" stream).

;; Determining the type of a stream.  (server end only)
(DEFUN DETERMINE-FILE-STREAM-TYPE (STREAM &AUX WO)
  (SETQ WO (FUNCALL STREAM ':WHICH-OPERATIONS))
  (COND ((MEMQ ':STREAM-TYPE WO)
	 (FUNCALL STREAM ':STREAM-TYPE))
	((MEMQ ':TYI WO) 'INPUT)
	((MEMQ ':TYO WO) 'OUTPUT)
	(T 'PROBE)))

(DEFPROP INPUT T INITIAL-TRANSMIT-DATA)

(comment
;;; Compute a list of all the standard stream operations
;;; by mixing together the standard stream types
;;; and asking the mixture for its :which-operations.

(DEFFLAVOR ALL-STREAMS-MIXED ()
  (SI:BUFFERED-INPUT-CHARACTER-STREAM
   SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(DEFMETHOD (ALL-STREAMS-MIXED :DISCARD-OUTPUT-BUFFER) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :WHO-LINE-INFORMATION) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :QFASLP) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :TRUENAME) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :PLIST) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :NEW-OUTPUT-BUFFER) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :SEND-OUTPUT-BUFFER) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :NEXT-INPUT-BUFFER) (&REST IGNORE) ())

(DEFMETHOD (ALL-STREAMS-MIXED :DISCARD-INPUT-BUFFER) (&REST IGNORE) ())

(COMPILE-FLAVOR-METHODS ALL-STREAMS-MIXED)

;List of all stream operations standardly defined.
(DEFCONST STANDARD-STREAM-OPS
  (FUNCALL (MAKE-INSTANCE 'ALL-STREAMS-MIXED) ':WHICH-OPERATIONS))
);end comment

(DEFCONST STANDARD-STREAM-OPS
	  '(:ADVANCE-INPUT-BUFFER :BREAK :CHARACTERS :CLEAR-INPUT 
	    :CLEAR-OUTPUT :CLOSE :DESCRIBE :DIRECTION 
	    :DISCARD-CURRENT-INPUT-BUFFER :DISCARD-CURRENT-OUTPUT-BUFFER 
	    :DISCARD-INPUT-BUFFER :DISCARD-OUTPUT-BUFFER :EOF 
	    :EVAL-INSIDE-YOURSELF :FINISH :FORCE-OUTPUT :FRESH-LINE 
	    :FUNCALL-INSIDE-YOURSELF :GET-HANDLER-FOR :GET-INPUT-BUFFER 
	    :INIT :LAST-CHAR-OUTPUT :LINE-IN :LINE-OUT :LISTEN 
	    :NEW-OUTPUT-BUFFER :NEXT-INPUT-BUFFER :OPERATION-HANDLED-P 
	    :PLIST :PRINT-SELF :QFASLP :READ-INPUT-BUFFER :READ-POINTER 
	    :READ-UNTIL-EOF :REWIND :SEND-CURRENT-OUTPUT-BUFFER 
	    :SEND-IF-HANDLES :SEND-OUTPUT-BUFFER :SET-BUFFER-POINTER 
	    :SET-POINTER :SETUP-NEW-OUTPUT-BUFFER :SETUP-NEXT-INPUT-BUFFER
	    :STREAM-INPUT-BUFFER :STREAM-INPUT-INDEX :STREAM-INPUT-LIMIT 
	    :STREAM-OUTPUT-BUFFER :STREAM-OUTPUT-INDEX 
	    :STREAM-OUTPUT-LIMIT :STRING-IN :STRING-OUT :TRUENAME :TYI 
	    :TYI-NO-HANG :TYIPEEK :TYO :UNTYI :WHICH-OPERATIONS 
	    :WHO-LINE-INFORMATION))
