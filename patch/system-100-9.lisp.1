;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for System version 100.9
;;; Reason:
;;;  m-X Finish Patch: Avoid interleaving Reason: lines, and remember that we are done.
;;; Written 10-May-23 13:34:25 by ams,
;;; while running on Lisp Machine One from band 7
;;; with Experimental System 100.8, Hacks by AMS 1.0, microcode 323, AMS.



; From file FC: /tree/zwei/pated.lisp at 10-May-23 13:34:25
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //tree//zwei//pated"

(DEFUN FINISH-PATCH (RELEASE-FLAG)
  (VALIDATE-PATCH-BUFFER)
  (OR *PATCH-BUFFER*
      (BARF "There is no current patch buffer"))
  (LET ((DESCRIPTION (TYPEIN-LINE-MULTI-LINE-READLINE
		       "Description of changes (end with ~C)" #/END)))
    (SETQ DESCRIPTION (STRING-TRIM '(#/NEWLINE) DESCRIPTION))
    (LET ((BP (FORWARD-LINE (INTERVAL-FIRST-BP *PATCH-BUFFER*) 2)))
      (INSERT-MOVING BP ";;; Reason:")
      (INSERT-MOVING BP #/NEWLINE)
      (DO ((START 0 (1+ NEXT-LINE))
	   NEXT-LINE)
	  (())
	(SETQ NEXT-LINE (STRING-SEARCH-CHAR #/NEWLINE DESCRIPTION START))
	(INSERT-MOVING BP ";;;  ")
	(INSERT-MOVING BP DESCRIPTION START NEXT-LINE)
	(INSERT-MOVING BP #/NEWLINE)
	(OR NEXT-LINE (RETURN))))
    (SAVE-BUFFER-IF-NECESSARY *PATCH-BUFFER*)
    (AND (EQ *PATCH-BUFFER* *INTERVAL*)
	 (MUST-REDISPLAY *WINDOW* DIS-TEXT))
    (LET ((ERROR-MESSAGE (IF *PATCH-SYSTEM*
			     (SI::CONSUMMATE-PATCH *PATCH-SYSTEM* *PATCH-NUMBER*
						   DESCRIPTION RELEASE-FLAG)
			   (COMPILE-FILE (BUFFER-PATHNAME *PATCH-BUFFER*))
			   NIL)))
      (COND ((ERRORP ERROR-MESSAGE)
	     (BARF ERROR-MESSAGE))
	    (T
	     (FORMAT *QUERY-IO* "~&~:[Patch completed.~;Don't forget to save your files!~]"
		     (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
			   THEREIS (BUFFER-NEEDS-SAVING-P BUFFER)))
	     (SETQ *PATCH-BUFFER* NIL
		   *PATCH-SYSTEM* NIL))))))
))
