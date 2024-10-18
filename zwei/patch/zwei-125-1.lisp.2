;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.1
;;; Reason:
;;;  Zwei's Meta-X Set Base didn't have any default handling, which could
;;;  lead to spurious '*eof* value being your read and print base!  Fix
;;;  defaults here to display default (base 10.), let it be the yank
;;;  default, etc.  Similar for Set Readtable.
;;; Written 15-Jul-88 14:56:25 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with System 125.4, ZWEI 125.0, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.0, Lambda-Diag 17.0, microcode 1761, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; DISPLA.LISP#177 at 15-Jul-88 14:56:33
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "


(defvar *typein-line-eof-value* '*eof*
  "Bind this to specify (and therefore reliably test for) EOF value from TYPEIN-LINE-READs.")

))

; From modified file DJ: L.ZWEI; DISPLA.LISP#177 at 15-Jul-88 14:56:34
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "


(DEFUN TYPEIN-LINE-READ-WITH-DEFAULT (DEFAULT CTL-STRING &REST ARGS &AUX PROMPT)
  "Read an s-expression in the minibuffer; Return terminates; default value DEFAULT.
Prompts by passing ARGS to FORMAT."
  (SETQ PROMPT (IF (NULL ARGS)
		   CTL-STRING
		   (APPLY #'FORMAT NIL CTL-STRING ARGS)))
  (CL:READ (INTERVAL-STREAM (NTH-VALUE 2
			      (EDIT-IN-MINI-BUFFER *MINI-BUFFER-COMTAB*
						   DEFAULT NIL
						   (AND PROMPT (NCONS PROMPT)))))
	   NIL *typein-line-eof-value*))

))

; From modified file DJ: L.ZWEI; DISPLA.LISP#177 at 15-Jul-88 14:56:36
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DISPLA  "


(DEFUN TYPEIN-LINE-MULTI-LINE-READ-WITH-DEFAULT (DEFAULT CTL-STRING &REST ARGS &AUX PROMPT)
  "Read an s-expression in the minibuffer; Return does not terminate; default value DEFAULT.
Prompts by passing ARGS to FORMAT."
  (SETQ PROMPT (IF (NULL ARGS)
		   CTL-STRING
		   (APPLY #'FORMAT NIL CTL-STRING ARGS)))
  (CL:READ (INTERVAL-STREAM (NTH-VALUE 2
			      (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB*
						   DEFAULT NIL
						   (AND PROMPT (NCONS PROMPT)))))
	   NIL *typein-line-eof-value*))


;;;Miscellanous TYPEOUT functions - need more standardization here!!

))

; From modified file DJ: L.ZWEI; ZMNEW.LISP#57 at 15-Jul-88 14:56:57
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "


(DEFCOM COM-SET-BASE "Change the input radix associated with this buffer or file.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well.
The numeric argument is used as the new value,
or else one is read in the minibuffer.  The default is ZWEI:*DEFAULT-BASE*."
  ()
  (SETQ *READ-BASE* (IF *NUMERIC-ARG-P* *NUMERIC-ARG*
		      (LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.)
			    (*mini-buffer-default-string* "10.")
			    (*typein-line-eof-value* 10.))
			(TYPEIN-LINE-READ "Set Base (default is 10):")))
	*PRINT-BASE* *READ-BASE*)
  (format *query-io* "Setting Base to ~10r." *read-base*)
  (SEND *INTERVAL* ':SET-ATTRIBUTE ':BASE *READ-BASE* ':QUERY)
  DIS-NONE)

))

; From modified file DJ: L.ZWEI; ZMNEW.LISP#57 at 15-Jul-88 14:56:59
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "


(DEFCOM COM-SET-READTABLE
  "Change the readtable associated with buffer or file.
Specify a new readtable to be used when reading from this buffer;
for example, evaluating or compiling parts of the buffer, or when
parsing the list syntax of the buffer in ZMACS.
This much does not affect operations on the file itself, only this ZMACS buffer.

To specify a readtable that doesn't already exist, you must exit with
Control-Return, or type Return twice.  Then you must confirm with /"Yes/".

You will also be asked whether to change the attribute list in the text.
If you answer yes, the buffer's first line is modified to say that
it should be read using the new readtable.  This will affect all operations on the
file, once you save the buffer.

Then you will be asked whether to resectionize the buffer.
If you say yes, all the functions definitions in the buffer
will be recorded under symbols in the new package." ()
  (LET (ALIST)
    (DOLIST (RDTBL SI:*ALL-READTABLES*)
      (DOLIST (N (DONT-OPTIMIZE (SI:RDTBL-NAMES RDTBL)))
	(PUSH (CONS N RDTBL) ALIST)))
    (LET* ((default-str (si:rdtbl-short-name *readtable*))
	   (*mini-buffer-default-string* default-str)
	   (RDTBL (COMPLETING-READ-FROM-MINI-BUFFER
		    (format nil "Set readtable (default is ~a) :" default-str)
		    ALIST 'MAYBE)))
      (UNLESS (STRINGP RDTBL) (SETQ RDTBL (CAR RDTBL)))
      (IF (EQUAL RDTBL "")
	  (SETQ RDTBL *READTABLE*)
	(IF (SI:FIND-READTABLE-NAMED RDTBL :FIND)
	    (SETQ *READTABLE* (SI:FIND-READTABLE-NAMED RDTBL))
	  (SETQ RDTBL (STRING-CAPITALIZE-WORDS RDTBL NIL NIL))
	  (IF (NOT (YES-OR-NO-P (FORMAT NIL "Readtable ~A does not exist.  Create? " RDTBL)))
	      (BARF)
	    (SETQ *READTABLE* (COPY-READTABLE *READTABLE*))
	    (SETF (DONT-OPTIMIZE (SI:RDTBL-NAMES *READTABLE*)) (LIST RDTBL)))))))
  (SEND *INTERVAL* ':SET-ATTRIBUTE :READTABLE
				   (DONT-OPTIMIZE (SI:RDTBL-SHORT-NAME *READTABLE*))
				   :QUERY)
  (WHEN (EQ *MAJOR-MODE* 'LISP-MODE)
    (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
			 LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
    (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
			 LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))
  DIS-NONE)

))
