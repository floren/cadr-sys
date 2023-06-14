;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.21
;;; Reason: Change the use of the :LOCAL-INTERNET-DOMAINS site variable.
;;; Written 13-Jun-23 05:08:16 by AMS,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.0, microcode 323.



; From file OZ: /tree/network/host.lisp at 13-Jun-23 05:08:16
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//network//host"

(DEFINE-SITE-VARIABLE LOCAL-INTERNET-DOMAINS :LOCAL-INTERNET-DOMAINS
  "List of domains to which our site belongs.
If a host is specified with one of these domains, 
we try to make the shortest prefix (without the domain) a short-name.
If an unknown host is specified without a domain, we search these
domains for the host.")
))

; From file OZ: /tree/network/host.lisp at 13-Jun-23 05:08:24
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//network//host"

(DEFUN PARSE-HOST (HOST &OPTIONAL NO-ERROR-P (UNKNOWN-OK T)
		   &AUX ELEMENT)
  "Return a host object for name HOST, taken from the HOST-ALIST.
This is the right function to use for making network connections,
but is not right for parsing pathnames.  Use FS:GET-PATHNAME-HOST for that.
HOST can also be a host object already; then it's simply returned.
NO-ERROR-P says just return NIL if there is no such host known.
UNKNOWN-OK says call the UNKNOWN-HOST-FUNCTION (if that's not NIL)
to give it a chance to create a host and add it to the host table."
  (IF (TYPEP HOST 'HOST) HOST
;; This is not useful in the modern world.
;; See CHAOS:CHAOS-UNKNOWN-HOST-FUNCTION for better uses.
;    (LET ((IDX (STRING-SEARCH-CHAR #/. HOST)))
;      (AND IDX
;	   (MEMBER-EQUALP (SUBSTRING HOST (1+ IDX)) LOCAL-INTERNET-DOMAINS)
;	   (SETQ HOST (SUBSTRING HOST 0 IDX))))
    (COND ((AND (SETQ ELEMENT (LOOP FOR ELEMENT IN HOST-ALIST
				    WHEN (MEM #'STRING-EQUAL HOST (HOST-NAME-LIST ELEMENT))
				    RETURN ELEMENT))
		(NOT (NULL (HOST-ADDRESSES ELEMENT))))
	   (GET-ALIST-ELEM-HOST ELEMENT))
	  ((STRING-EQUAL HOST "CHAOS|" :END1 6)
	   (LET ((ADDRESS (PARSE-NUMBER HOST 6 NIL 8)))
	     (OR (GET-HOST-FROM-ADDRESS ADDRESS :CHAOS)
		 (MAKE-UNNAMED-HOST :DEFAULT `(:CHAOS (,ADDRESS))))))
	  ((AND UNKNOWN-OK UNKNOWN-HOST-FUNCTION)
	   (FUNCALL UNKNOWN-HOST-FUNCTION HOST)
	   (PARSE-HOST HOST NO-ERROR-P NIL))
	  (NO-ERROR-P
	   NIL)
	  (T
	   (FERROR 'SYS:UNKNOWN-HOST-NAME "~S is not a known host." HOST)))))
))

; From file OZ: /tree/network/chaos/chuse.lisp at 13-Jun-23 05:09:06
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//network//chaos//chuse"

(DEFUN FIND-SHORTEST-LOCAL-DOMAIN-PREFIX (NAMES
					  &OPTIONAL (DOMAINS SI:LOCAL-INTERNET-DOMAINS))
  "Find the shortest prefix of a local domain"
  (WHEN (AND NAMES DOMAINS)
    (CAR (SORT (MAPCON #'(LAMBDA (NMS)
			   (LET* ((N (CAR NMS))
				  (D (STRING-SEARCH-CHAR #/. N)))
			     (AND D
				  (CLI:MEMBER (SUBSTRING N (1+ D))
					      DOMAINS
					      :TEST #'STRING-EQUAL)
				  (LIST (SUBSTRING N 0 D)))))
		       NAMES)
	       #'(LAMBDA (A B)
		   (< (STRING-LENGTH A) (STRING-LENGTH B)))))))

(DEFUN CHAOS-UNKNOWN-HOST-FUNCTION (NAME)
  "Try to find the host with NAME using the HOSTAB servers.
If NAME does not contain a dot, the :LOCAL-INTERNET-DOMAINS site variable
domains are used to search for it."
  (WHEN (AND SI:LOCAL-INTERNET-DOMAINS
	     ;; If there is no dot in the name
	     (NOT (STRING-SEARCH-CHAR #/. NAME))
             ;; And not only digits
             (CLI:SOME #'(LAMBDA (C) (NOT (DIGIT-CHAR-P C 8))) NAME))
    ;; Try each of the local domains
    (DOLIST (DOM SI:LOCAL-INTERNET-DOMAINS)
      (WHEN (CHAOS-UNKNOWN-HOST-FUNCTION-1 (STRING-APPEND NAME "." DOM))
	(RETURN T))))
  ;; Else (or if it fails) try the name itself
  (CHAOS-UNKNOWN-HOST-FUNCTION-1 NAME))

(DEFUN CHAOS-UNKNOWN-HOST-FUNCTION-1 (NAME)
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
		     (let ((short
			     (find-shortest-local-domain-prefix (get list :host-names))))
		       ;; If there is a candidate short-name, push it
		       (when short
			 (pushnew short (get list :host-names) :test #'string-equal))
		       ;; Sort the names found, to make the shortest one the shortname
		   (SETQ RESULT (WHEN LIST
				  (PUTPROP LIST (STABLE-SORT (GET LIST :HOST-NAMES)
							     #'(LAMBDA (X Y)
								 (< (STRING-LENGTH X)
								    (STRING-LENGTH Y))))
					   :HOST-NAMES)
				  (APPLY #'SI:DEFINE-HOST LIST))
			 DONE T))
		 (SETQ LEN (STRING-LENGTH LINE)
		       SP (STRING-SEARCH-CHAR #/SP LINE 0 LEN))
		 (if (null sp)		;Better safe than sorry
		     (setq done t)		; just forget the whole thing
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
		      (if function
			  ;; Try parsing it as a network address
			  (let ((val (FUNCALL FUNCTION PROP LINE SP LEN)))
			    (when val (PUSH val (GET LIST PROP))))
			  ;; Else it is a generic property: octal number or string
			  (let* ((val (substring line sp))
				 (pval (or (parse-integer val :radix 8 :junk-allowed t)
					   val)))
			    (push pval (get list prop))))))))))
	     (RETURN T))))))
))
