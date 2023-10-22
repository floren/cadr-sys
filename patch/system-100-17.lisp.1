;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.17
;;; Reason:
;;;  CHAOS-UNKNOWN-HOST-FUNCTION: Fix typo so that :MACHINE-TYPE property works again.
;;; Written 6-Jun-23 17:08:02 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.13, Hacks by AMS 2.0, microcode 323, WIP.



; From file FC: /sys/network/chaos/chuse.lisp at 6-Jun-23 17:08:02
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//network//chaos//chuse"

(DEFUN CHAOS-UNKNOWN-HOST-FUNCTION (NAME)
  (DOLIST (HOST (SI:GET-SITE-OPTION :CHAOS-HOST-TABLE-SERVER-HOSTS))
    (AND (SI:PARSE-HOST HOST T ())		; prevent infinite recursion
	 (WITH-OPEN-STREAM (STREAM (OPEN-STREAM HOST "HOSTAB" :ERROR NIL))
	   (SETQ NAME (STRING NAME))
	   (UNLESS (ERRORP STREAM)
	     (SEND STREAM :LINE-OUT NAME)
	     (SEND STREAM :FORCE-OUTPUT)
	     (DO ((LIST NIL) (RESULT) (DONE)
		  (LINE) (EOF)
		  (LEN) (SP) (PROP))
		 (DONE RESULT)
	       (MULTIPLE-VALUE (LINE EOF) (SEND STREAM :LINE-IN))
	       (IF EOF
		   (SETQ RESULT (WHEN LIST
				  (PUTPROP LIST (STABLE-SORT (GET LIST :HOST-NAMES)
							     #'(LAMBDA (X Y)
								 (< (STRING-LENGTH X)
								    (STRING-LENGTH Y))))
					   :HOST-NAMES)
				  (APPLY #'SI:DEFINE-HOST LIST))
			 DONE T)
		 (SETQ LEN (STRING-LENGTH LINE)
		       SP (STRING-SEARCH-CHAR #/SP LINE 0 LEN))
		 (SETQ PROP (INTERN (SUBSTRING LINE 0 SP) ""))
		 (INCF SP)
		 (CASE PROP
		   (:ERROR (SETQ DONE T))
		   (:NAME
		    (LET ((NAME (SUBSTRING LINE SP LEN)))
		      (OR LIST (SETQ LIST (NCONS NAME)))
		      (PUSH NAME (GET LIST :HOST-NAMES))))
		   ((:SYSTEM-TYPE :MACHINE-TYPE)
		    (PUTPROP LIST (INTERN (SUBSTRING LINE SP LEN) "") PROP))
		   (OTHERWISE
		    (LET ((FUNCTION (GET PROP 'HOST-ADDRESS-PARSER)))
		      (OR FUNCTION (SETQ FUNCTION (GET :CHAOS 'HOST-ADDRESS-PARSER)))
		      (PUSH (FUNCALL FUNCTION PROP LINE SP LEN)
			    (GET LIST PROP)))))))
	     (RETURN T))))))
))
