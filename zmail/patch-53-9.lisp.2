;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Written 12/31/83 23:48:06 by RMS,
;;; Reason: Subjects prompted for should really appear in the messages sent.
;;; while running on Lisp Machine Nine from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.17, CADR 3.4, ZMail 53.5, MIT-Specific 22.0, microcode 306, ZM MIT.




; From file MAIL.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFUN GET-SEND-HEADERS (BP1 &OPTIONAL BP2 IN-ORDER-P
			 &AUX LIST PLIST TEM SUBJECT)
  (SETQ LIST (PARSE-HEADERS-INTERVAL BP1 BP2 IN-ORDER-P)
	PLIST (LOCF LIST))
  (COND ((SETQ TEM (GET PLIST 'LOSING-HEADERS))
	 (BARF "Can't grok headers: ~A" TEM))
	((AND (NULL (GET PLIST ':TO))
	      (NULL (GET PLIST ':FTO)))
	 (BARF "There are no /"To/" or /"FTo/" recipients")))
  (WHEN (SETQ TEM (GETL PLIST ':(F)))
    (SETF (FIRST TEM) ':FROM)
    (SETF (SECOND TEM) (PARSE-ADDRESSES (SECOND TEM))))
  (IF (SETQ TEM (GETL PLIST ':(S))) (SETF (FIRST TEM) ':SUBJECT))
  (COND ((NOT (OR (NOT (MEMBER (GET PLIST ':SUBJECT) '(NIL "")))
		  (SELECTQ *REQUIRE-SUBJECTS*
		    ((NIL :INIT) T)
		    (:BUG (NOT *SENDING-BUG-REPORT*))
		    (OTHERWISE NIL))
		  (EQUAL "" (SETQ SUBJECT (TYPEIN-LINE-READLINE "Subject for message (or just Return):")))))
	 (SETQ LIST (LIST* ':SUBJECT SUBJECT LIST))
	 (INSERT-MOVING (ADD-HEADER-FIELD ':SUBJECT NIL NIL) SUBJECT)))
  LIST)

))
