;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 1/08/84 19:01:31 by RpK,
;;; Reason: Dired works on subdirectories now
;;;  Fix callers: :STRING-FOR-PRINTING method for logical pathnames and patch version
;;;   description (!).  ~D is no longer equivalent to ~A for non-numbers.
;;; while running on Lisp Machine Eighteen from band 7
;;; with System 98.26, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file DIRED.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-EDIT-FILE "Edit the current file; or DIRED it if it's a directory" ()
  (OR (TYPEP *WINDOW* 'ZMACS-WINDOW) (BARF))
  (WHEN (GET (LOCF (LINE-PLIST (BP-LINE (POINT)))) ':DELETED)
    (BARF))
  (LET* ((LINE (BP-LINE (POINT)))
	 (DIR-P (GET (LOCF (LINE-PLIST LINE)) ':DIRECTORY))
	 (PATHNAME (DIRED-LINE-PATHNAME-OR-BARF LINE)))
    (AND (NOT DIR-P) ; Reversion doesn't make sense for recursive Dired
	 (GET (LOCF (LINE-PLIST LINE)) ':NEWEST)
	 (IF (NULL (SETQ PATHNAME (NEED-TO-REVERT-BUFFER PATHNAME)))
	     (BARF "Re-enter edit command")))
    (IF DIR-P
	(DIRECTORY-EDIT (FUNCALL (FUNCALL PATHNAME ':PATHNAME-AS-DIRECTORY)
				 ':NEW-PATHNAME ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
      (LET ((BUFFER (FIND-FILE-BUFFER PATHNAME)))
	(IF BUFFER (MAKE-BUFFER-CURRENT BUFFER)
	  (FIND-FILE PATHNAME)))
      (LET ((BLURB (KEY-FOR-COMMAND 'COM-SELECT-PREVIOUS-BUFFER
				    *COMTAB* NIL NIL #\C-M-L)))
	(AND (NULL BLURB) (SETQ BLURB (KEY-FOR-COMMAND 'COM-SELECT-BUFFER))
	     (SETQ BLURB (STRING-APPEND BLURB " Return")))
	(AND BLURB
	     (FORMAT QUERY-IO "~&Type ~A to return to DIRED" BLURB))
	DIS-TEXT))))

))

; From file PATED.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN PATCH-VERSION-DESCRIPTION ()
  (IF *PATCH-SYSTEM*
      (FORMAT NIL "~D.~D of ~A"
	      (SI:PATCH-VERSION *PATCH-SYSTEM*) *PATCH-NUMBER*
	      (SI:PATCH-NAME *PATCH-SYSTEM*))
    (BUFFER-NAME *PATCH-BUFFER*)))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-PRINTING) ()
  (LET ((DIR (STRING-OR-WILD DIRECTORY))
	(DEV (LOGICAL-DEVICE-STRING))
	(NAM (LOGICAL-NAME-STRING))
	(TYP (LOGICAL-TYPE-STRING))
	(VER (LOGICAL-VERSION-STRING))  ; can actually be a number
	(DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~A: ~@[~A: ~]~:[~A; ~;~*~]~@[~A~]~@[ ~A~]~@[ ~A~]"
	    (FUNCALL HOST ':NAME-AS-FILE-COMPUTER)
	    DEV
	    (MEMQ DIRECTORY '(NIL :UNSPECIFIC))
	    DIR NAM TYP (IF (NUMBERP VER) (FORMAT () "~D" VER) VER))))

))
