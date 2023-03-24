;;; -*- Mode: Lisp; Package: Tape; Base: 8 -*-
;;; (c) 1984 Lisp Machine Incorporated
;;; Random stuff needed by all tape software (IO, movement).

;; This is also in FSDEFS
(REMPROP 'QUOTIENT-CEILING 'SOURCE-FILE-NAME)
(DEFSUBST QUOTIENT-CEILING (Y X) (CEILING Y X))

(DEFVAR FILE-SYSTEM-PACKAGE (PKG-FIND-PACKAGE "FS"))

;;; > Rest went to WEUNIT...

(DEFUN REWIND (&OPTIONAL (UNIT 0))
  (SEND (PARSE-UNIT UNIT) ':REWIND))

(DEFUN WRITE-EOF (&OPTIONAL (UNIT 0))
  (SEND (PARSE-UNIT UNIT) ':WRITE-EOF))

(DEFUN SPACE (&OPTIONAL (UNIT 0) (NTIMES 1))
  (SEND (PARSE-UNIT UNIT) ':SPACE NTIMES))

(DEFUN SPACE-TO-EOF (&OPTIONAL (UNIT 0) (NTIMES 1))
  (SEND (PARSE-UNIT UNIT) ':SPACE-TO-EOF NTIMES))

(DEFUN SPACE-REV (&OPTIONAL (UNIT 0) (NTIMES 1))
  (SEND (PARSE-UNIT UNIT) ':SPACE-REV NTIMES))

(DEFUN SPACE-REV-TO-BOF (&OPTIONAL (UNIT 0) (SKIP-N-FILES 0))
  (SEND (PARSE-UNIT UNIT) ':SPACE-REV-TO-BOF SKIP-N-FILES))

(DEFUN SPACE-TO-APPEND (&OPTIONAL (UNIT 0))
  (SEND (PARSE-UNIT UNIT) ':SPACE-TO-APPEND))

(DEFUN UNLOAD (&OPTIONAL (UNIT 0))
  (SEND (PARSE-UNIT UNIT) ':UNLOAD))

(DEFUN OFFLINE (&OPTIONAL (UNIT 0))
  (SEND (PARSE-UNIT UNIT) ':OFFLINE))

;;; Old FS functions....

(DEFUN FS:MT-SPACE (&OPTIONAL (NTIMES 1) (UNIT 0))
  (SPACE UNIT NTIMES))

(DEFUN FS:MT-SPACE-TO-EOF (&OPTIONAL (UNIT 0) (NTIMES 1))
  (SPACE-TO-EOF UNIT NTIMES))

(DEFUN FS:MT-SPACE-REV (&OPTIONAL (NTIMES 1) (UNIT 0))
  (SPACE-REV UNIT NTIMES))

;; Reverse through the tape, positioning the tape at the beginning of a file.
;; If SKIP-N-BLOCKS is 0, this positions the tape at the beginning of this file.
;; If SKIP-N-BLOCKS is 1, this positions the tape at the beginning of the previous file, etc.
;; If this reaches the beginning of the tape prematurely, it stops there and returns NIL.
(DEFUN FS:MT-SPACE-REV-TO-BOF (&OPTIONAL (UNIT 0) (SKIP-N-FILES 0))
  (SPACE-REV-TO-BOF UNIT SKIP-N-FILES))

;; This function attempts to bypass all files on the tape until two
;; consecutive EOFs are found, then positions the tape over the last EOF.
;; The tape is now in a configuration allowing one to append new files.
(DEFUN FS:MT-SPACE-TO-APPEND (&OPTIONAL (UNIT 0) &AUX RQB)
  (SPACE-TO-APPEND UNIT))

(DEFUN FS:MT-REWIND (&OPTIONAL (UNIT 0))
  (REWIND UNIT))

(DEFUN FS:MT-WRITE-EOF (&OPTIONAL (UNIT 0))
  (WRITE-EOF UNIT))

(DEFUN FS:MT-UNLOAD (&OPTIONAL (UNIT 0))
  (UNLOAD UNIT))

(DEFUN FS:MT-OFFLINE (&OPTIONAL (UNIT 0))
  (OFFLINE UNIT))

(DEFSIGNAL END-OF-TAPE FERROR (UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)
  "Mag tape runs off end of tape.")

(DEFSIGNAL READ-ONLY-TAPE FERROR (UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)
  "No write ring is seated in the tape for writing.")

;; Standard End of Tape handlers

;; This one is useful when you have transporting things which are bigger than
;; a tape.  it just rewinds, lets you reload, and continues.
;; Install it as a handler for END-OF-TAPE, using CONDITION-BIND.
(DEFUN CONTINUING-MT-EOT-HANDLER (CONDITION &AUX (UNIT (SEND CONDITION ':UNIT)))
  (PROG ((STREAM ERROR-OUTPUT))
	(REWIND UNIT)
	(FUNCALL STREAM ':BEEP)
	(FORMAT STREAM "~%>>> MagTape unit ~D reached end of tape <<<~%" UNIT)
	(OFFLINE UNIT)			;This will wait...
     L  (FORMAT STREAM "Please type [Resume] to continue tape operation: ")
	(SEND STREAM ':CLEAR-INPUT)
	(COND ((NOT (CHAR-EQUAL (FUNCALL STREAM ':TYI) #\RESUME))
	       (FUNCALL STREAM ':BEEP)
	       (FUNCALL STREAM ':TYO #\CR)
	       (GO L)))
	(FORMAT STREAM "[Resuming tape operation]~%")
	(RETURN ':NO-ACTION)))


