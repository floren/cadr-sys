;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 51.1
;;; Reason: Bug computing GMSGS pathname.
;;; Written 8/22/83 18:24:50 by RMS,
;;; while running on Lisp Machine Eighteen from band 4
;;; with Experimental System 97.2, CADR 1.0, Experimental ZMail 51.0, MIT-Specific 21.0, microcode 255, ZM MIT.



; From file MFHOST.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST  "

(DEFMETHOD (BABYL-MAIL-FILE-BUFFER :INBOX-BUFFER) (&OPTIONAL NEW-PATHNAME DELETE-P)
  (MAKE-INBOX-BUFFER
    (FUNCALL PATHNAME ':INBOX-BUFFER-FLAVOR)
    (IF NEW-PATHNAME
	(LIST (LIST NEW-PATHNAME NIL DELETE-P))
      (LOOP FOR NEW-PATHNAME
	    IN (IF *RUN-GMSGS-P*
		   (CONS (SEND (ZMAIL-BUFFER-GMSGS-HOST SELF) ':GMSGS-PATHNAME)
			 (GET (LOCF OPTIONS) ':MAIL))
		 (GET (LOCF OPTIONS) ':MAIL))
	    COLLECT (LIST NEW-PATHNAME
			  (FUNCALL NEW-PATHNAME ':NEW-TYPE
				   (STRING-APPEND
				     (FUNCALL NEW-PATHNAME
					      ':ZMAIL-TEMP-FILE-NAME)
				     (FUNCALL NEW-PATHNAME ':TYPE)))
			  T)))
    SELF))

))
