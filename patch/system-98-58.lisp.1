;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 5/24/84 18:14:33 by RpK,
;;; Reason: Peek: Don't show useless subnets in Chaos mode.
;;; SI: PARSE-HOST now tries a HOSTAB server if needed.
;;; while running on Lisp Machine One from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.56, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309.



; From file HOST.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; HOST  "

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
    (LET ((IDX (STRING-SEARCH-CHAR #/. HOST)))
      (AND IDX
	   (MEMBER-EQUALP (SUBSTRING HOST (1+ IDX)) LOCAL-INTERNET-DOMAINS)
	   (SETQ HOST (SUBSTRING HOST 0 IDX))))
    (COND ((AND (SETQ ELEMENT (LOOP FOR ELEMENT IN HOST-ALIST
				    WHEN (MEM #'STRING-EQUAL HOST (HOST-NAME-LIST ELEMENT))
				    RETURN ELEMENT))
		(NOT (NULL (HOST-ADDRESSES ELEMENT))))
	   (GET-ALIST-ELEM-HOST ELEMENT))
	  ((STRING-EQUAL HOST "CHAOS|" :END1 6 :END2 6)
	   (LET ((ADDRESS (PARSE-NUMBER HOST 6 NIL 8)))
	     (OR (GET-HOST-FROM-ADDRESS ADDRESS ':CHAOS)
		 (MAKE-UNNAMED-HOST ':DEFAULT `(:CHAOS (,ADDRESS))))))
	  ((AND UNKNOWN-OK UNKNOWN-HOST-FUNCTION)
	   (FUNCALL UNKNOWN-HOST-FUNCTION HOST)
	   (PARSE-HOST HOST NO-ERROR-P NIL))
	  (NO-ERROR-P
	   NIL)
	  (T
	   (FERROR 'SYS:UNKNOWN-HOST-NAME "~S is not a known host." HOST)))))

))

; From file PEEKCH.LISP PS:<L.WINDOW> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; PEEKCH  "


(DEFUN PEEK-CHAOS-SUBNET-NAME (SUBNET)
  (LET ((STRING 
	  (FORMAT NIL "~:[Direct~;~A~]"
		  ( SUBNET MY-SUBNET)
   ;;this could really be clever and try to save away the info somewhere
		  (LET ((BRIDGE (AREF ROUTING-TABLE SUBNET)))
		    (COND ((AND BRIDGE (NOT (ZEROP BRIDGE)))
			   (HOST-DATA BRIDGE))
			  (T
			   "No Connection"))))))
    (SUBSTRING STRING 0 (MIN 18. (STRING-LENGTH STRING)))))

(DEFUN PEEK-CHAOS-ROUTING-COST (SUBNET)
  (FORMAT NIL "~:[~4D.~]"
	  (= SUBNET MY-SUBNET)   
	  (AREF ROUTING-TABLE-COST SUBNET)))

(DEFUN PEEK-CHAOS (IGNORE)
  "Displays state of all chaos net connections, meters, and routing table"
  (LIST NIL
	(TV:SCROLL-PARSE-ITEM
	  "Chaos connections at "
	  `(:FUNCTION ,#'TIME () NIL ("~O")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () CONN-LIST)
			      #'PEEK-CHAOS-CONN)
	(TV:SCROLL-PARSE-ITEM "Interesting meters")
	(TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () PEEK-A-BOO-LIST)
			      #'(LAMBDA (COUNTER)
				  (TV:SCROLL-PARSE-ITEM
				    `(:STRING ,(STRING COUNTER) 35.)
				    `(:FUNCTION SYMEVAL (,COUNTER) NIL ("~@15A" 10. T)))))
	(TV:SCROLL-PARSE-ITEM '(:STRING "%COUNT-CHAOS-TRANSMIT-ABORTS" 35.)
			   '(:FUNCTION READ-METER (%COUNT-CHAOS-TRANSMIT-ABORTS) NIL
				       ("~@15A" 10. T)))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-PARSE-ITEM "Subnet  Gateway            Cost")
	(TV:SCROLL-MAINTAIN-LIST
	  #'(LAMBDA () 1)
	  #'(LAMBDA (SUBNET)
	      (TV:SCROLL-PARSE-ITEM
		`(:STRING ,(FORMAT NIL "~O" SUBNET) 8.)
		`(:FUNCTION PEEK-CHAOS-SUBNET-NAME (,SUBNET) 16.)
		`(:FUNCTION PEEK-CHAOS-ROUTING-COST (,SUBNET) 12.)))
	  NIL
	  #'(LAMBDA (SUBNET)
	      (LET ((NEW-SUBNET (POSITION-IF-NOT #'ZEROP ROUTING-TABLE :START SUBNET)))
		(VALUES NEW-SUBNET
			(AND NEW-SUBNET (+ NEW-SUBNET 1))
			(NOT NEW-SUBNET)))))))

))
