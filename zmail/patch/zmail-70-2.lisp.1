;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 70.2
;;; Reason:
;;;  Don't barf on addresses like
;;;   @SAIL.STANFORD.EDU:GLS@THINK.COM
;;;  or
;;;   @SAIL.STANFORD.EDU,@RIVERSIDE.SCRC.SYMBOLICS.COM:DCP@QUABBIN.SCRC.SYMBOLICS.COM
;;;  If they appear in < >'s in a header, you will probably have to edit
;;;  out the @...: part in a reply to a message that contains them (if
;;;  you reply to such a message) because many mailers can't deal with
;;;  that sort of address.  If you are replying to a message with an
;;;  address like
;;;   @SAIL.STANFORD.EDU,@SUN.COM:jblow@unixbox.sun.com (Joe Blow)
;;;  you shouldn't have to edit the address; only
;;;   jblow@unixbox.sun.com should appear in the reply header.
;;; Written 27-Jan-87 14:45:11 by RpK (Robert P. Krajewski) at site LMI Cambridge
;;; while running on Cthulhu from band 3
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 121.13, Experimental Lambda-Diag 15.0, Experimental ZMail 70.1, Experimental KERMIT 32.0, Experimental Unix-Interface 10.0, Experimental Local-File 72.0, Experimental FILE-Server 21.0, Experimental Tape 13.0, Experimental Site Data Editor 4.0, Experimental K Bridge Support 1.0, microcode 1730, SDU Boot Tape 3.12, SDU ROM 102, the old ones.



; From modified file DJ: L.ZMAIL; RFC733.LISP#64 at 27-Jan-87 14:45:13
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; RFC733  "

(defsignal parsing-error (ferror) ())

))

; From modified file DJ: L.ZMAIL; RFC733.LISP#64 at 27-Jan-87 14:45:25
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; RFC733  "

(defun parsing-error (format-string &rest format-args)
  (declare (eh:error-reporter))
  (apply #'ferror 'parsing-error format-string format-args))

))

; From modified file DJ: L.ZMAIL; RFC733.LISP#64 at 27-Jan-87 14:45:36
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; RFC733  "

(defun gobble-routing-information (lexemes)
  (if (eq (caar lexemes) 'atsign)
      (let ((lexemes (cdr lexemes))
	    lexeme)
	(flet ((pop ()
		 (when (null lexemes)
		   (parsing-error "Routing information (@...:) ended suddenly."))
		 (pop lexemes lexeme)
		 (car lexeme)))
	  (loop
	    (unless (eq (pop) 'atom)
	      (parsing-error "Expected host name in routing information."))
	    (case (pop)
	      (comma) ; drop through
	      (colon (return-from gobble-routing-information lexemes))
	      (otherwise (parsing-error "Routing information ended illegally")))
	    (unless (eq (pop) 'atsign)
	      (parsing-error "Expected atsign in routing information.")))))
    lexemes))

))

; From modified file DJ: L.ZMAIL; RFC733.LISP#64 at 27-Jan-87 14:45:42
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; RFC733  "

(DEFUN READ-ADDRESS (LEXEMES)
  (declare (values address remaining-lexemes))
  (PROG (HOST-PHRASE)
	(LET ((LEXEMES LEXEMES)
	       ADDRESS ATOM COLON)
	  (DISCARD-COMMENTS)
	  (WHEN (EQ (CAAR LEXEMES) 'COLON)
	    ;; : atom : ...
	    (POP LEXEMES)
	    (DISCARD-COMMENTS)
	    (UNLESS (EQ (CAAR LEXEMES) 'ATOM)
	      (PARSING-ERROR "/"~A/" invalid after colon at start of address."
		      (SECOND (CAR LEXEMES))))
	    (SETQ ATOM (POP LEXEMES))
	    (DISCARD-COMMENTS)
	    (UNLESS (EQ (CAAR LEXEMES) 'COLON)
	      (PARSING-ERROR "No colon after :~A at start of address."
		      (SECOND ATOM)))
	    (POP LEXEMES)
	    (DISCARD-COMMENTS)
	    (MULTIPLE-VALUE (ADDRESS LEXEMES)
	      (READ-ADDRESS LEXEMES))
	    (LET ((IND (INTERN (STRING-UPCASE (SECOND ATOM)) ""))
		  TEM)
	      (DOLIST (ADD ADDRESS)
		(AND (SETQ TEM (GETL (LOCF ADD) '(:NAME)))
		     (SETF (CAR TEM) IND))))
	    (AND (= (LENGTH ADDRESS) 1)
		 (LET ((INT (GET (LOCF (CAR ADDRESS)) :INTERVAL)))
		   (SETF (FIRST INT) (THIRD COLON))))
	    (RETURN (values ADDRESS LEXEMES))))
	;; >> Skip over hairy routing information.  Maybe ZMail could use this later.
	(setq lexemes (gobble-routing-information lexemes))
	;; It is not : atom : address.
	(MULTIPLE-VALUE (HOST-PHRASE LEXEMES)
	  (READ-HOST-PHRASE LEXEMES))
	;; Treat a following comment as the personal name.
	(AND HOST-PHRASE
	     (EQ (CAAR LEXEMES) 'COMMENT)
	     (LET ((PLIST (LOCF HOST-PHRASE))
		   (COMMENT (POP LEXEMES)))
	       (LET ((STRING (SECOND COMMENT)))
		 (PUTPROP PLIST (SUBSTRING STRING 1 (1- (STRING-LENGTH STRING))) :PERSONAL-NAME))
	       ;; Include comment in interval
	       (SETF (SECOND (GET PLIST :INTERVAL)) (FOURTH COMMENT))))
	;; Discard any remaining comments.
	(DISCARD-COMMENTS)
	;; If a colon follows, this is a distribution list.
	(WHEN (EQ (CAAR LEXEMES) 'COLON)
	  (POP LEXEMES)
	  ;; Read a list of addresses, and check for and discard the ";".
	  (MULTIPLE-VALUE-BIND (ADDRESSES LEXEMES)
	      (READ-ADDRESSES LEXEMES)
	    (UNLESS (EQ (CAAR LEXEMES) 'SEMI)
	      (PARSING-ERROR
		      "Distribution list terminated by /"~C/" rather that /";/"."
		      (SECOND (CAR LEXEMES))))
	    (LET ((SEMI (CAR LEXEMES)))
	      (AND HOST-PHRASE
		   (LET* ((ADDRESS NIL)
			  (PLIST (LOCF ADDRESS)))
		     (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :NAME) :DISTRIBUTION-LIST)
		     (SETF (SECOND (GET (LOCF HOST-PHRASE) :INTERVAL)) (THIRD SEMI))
		     (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :INTERVAL) :INTERVAL)
		     (PUTPROP PLIST ADDRESSES :INFERIORS)
		     (PUSH ADDRESS ADDRESSES)))
	      (RETURN (values ADDRESSES (CDR LEXEMES))))))
	;; If a "<" follows, the real thing is within the angle brackets.
	(WHEN (EQ (CAAR LEXEMES) 'LEFT-BRACKET)
	  (POP LEXEMES)
	  ;; Read a list of addresses, and check for and discard the ">".
	  (MULTIPLE-VALUE-BIND (ADDRESSES LEXEMES)
	      (READ-ADDRESSES LEXEMES)
	    (UNLESS (EQ (CAAR LEXEMES) 'RIGHT-BRACKET)
	      (PARSING-ERROR
		      "Bracketed list terminated by /"~C/" rather that /">/"."
		      (SECOND (CAR LEXEMES))))
	    (LET ((RIGHT-BRACKET (CAR LEXEMES)))
	      (AND HOST-PHRASE
		   (IF (= (LENGTH ADDRESSES) 1)
		       ;; For just one address, treat as Fred Foobar <FOO at MIT-AI>
		       (LET ((PLIST (LOCF (CAR ADDRESSES))))
			 (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :NAME) :PERSONAL-NAME)
			 ;; Include phrases and bracket in interval
			 (LET ((INTERVAL (GET PLIST :INTERVAL)))
			   (SETF (FIRST INTERVAL) (FIRST (GET (LOCF HOST-PHRASE) :INTERVAL)))
			   (SETF (SECOND INTERVAL) (THIRD RIGHT-BRACKET))))
		     ;; Otherwise treat as a list
		     (LET* ((ADDRESS NIL)
			    (PLIST (LOCF ADDRESS)))
		       (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :NAME) :BRACKETED-LIST)
		       (SETF (SECOND (GET (LOCF HOST-PHRASE) :INTERVAL)) (THIRD RIGHT-BRACKET))
		       (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :INTERVAL) :INTERVAL)
		       (PUTPROP PLIST ADDRESSES :INFERIORS)
		       (PUSH ADDRESS ADDRESSES)))))
	    ;; Discard the ">".
	    (RETURN (values ADDRESSES (CDR LEXEMES)))))
	(RETURN (values (LIST HOST-PHRASE) LEXEMES))))

))
