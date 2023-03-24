;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.19
;;; Reason: Zwei commands Make Local Variable and Kill Local Variable.
;;; Define :ELEMENT-TYPE stream operation on all streams.
;;; Define :ANY-TYI on some streams lacking it.
;;; Some new PROMPT-AND-READ options.
;;; Speed up SYMEVAL-GLOBALLY, SETQ-GLOBALLY, etc.
;;; Debugger Meta-L displaying ADI bug.  Add Patch printout bug.
;;; Fix compilation of (LET ((FOO FOO)) ...) when FOO is lexically available.
;;; FIND-PACKAGE (:MYPKG :GLOBAL 300) bug.  (LOCF (FUNCTION FOO)) bug.
;;; Written 12/29/83 01:41:52 by RMS,
;;; while running on Lisp Machine Eighteen from band 7
;;; with Bad Inconsistently updated System 98.16, CADR 3.4, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 306, ZM MIT.

(fundefine 'eh:symeval-globally)
(fundefine 'si:symeval-globally)
(fundefine 'eh:set-globally)
(fundefine 'si:set-globally)
(setplist 'eh:symeval-globally nil)
(setplist 'eh:set-globally nil)

(globalize "SYMEVAL-GLOBALLY")
(globalize "MAKUNBOUND-GLOBALLY")
(globalize "SETQ-GLOBALLY")
(globalize "SET-GLOBALLY")
(globalize "BOUNDP-GLOBALLY")

(remob "UNSIGNED-BYTE" 'fs)
(let ((sym (intern "UNSIGNED-BYTE" 'si)))
  (intern sym 'global)
  (export sym 'global)
  (setf (symbol-package sym) (find-package 'global)))

; From file LOGIN.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "


(DEFUN BOUNDP-GLOBALLY (SYMBOL)
  "T if the global binding of SYMBOL is not unbound.
This is the binding that is in effect outside of rebindings made in this stack group;
the binding seen in any other stack group that does not rebind SYMBOL."
  (MULTIPLE-VALUE-BIND (NIL NIL LOCATION)
      (SYMEVAL-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP 0)
    (LOCATION-BOUNDP LOCATION)))

(DEFUN MAKUNBOUND-GLOBALLY (SYMBOL)
  "Make the global binding of SYMBOL be unbound.
This is the binding that is in effect outside of rebindings made in this stack group;
the binding seen in any other stack group that does not rebind SYMBOL."
  (MULTIPLE-VALUE-BIND (NIL NIL LOCATION)
      (SYMEVAL-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP 0)
    (LOCATION-MAKUNBOUND LOCATION SYMBOL))
  SYMBOL)

(DEFUN SET-GLOBALLY (SYMBOL VALUE)
  "Set the global binding of SYMBOL to VALUE.
This is the binding that is in effect outside of rebindings made in this stack group;
the value seen in any other stack group that does not rebind SYMBOL."
  (EH:SET-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP VALUE 0))

(DEFUN SYMEVAL-GLOBALLY (SYMBOL)
  "Return the global binding of SYMBOL.
This is the value that is in effect outside of rebindings made in this stack group;
the value seen in any other stack group that does not rebind SYMBOL."
  (VALUES (SYMEVAL-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP 0)))

))

; From file LOGIN.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LOGIN  "

(DEFMACRO SETQ-GLOBALLY (&REST VARIABLES-AND-FORMS)
  "Like SETQ but sets the global bindings of the variables, not the current bindings.
It works by doing the SETQ in another process."
  (CONS 'PROGN (LOOP FOR (VAR FORM) ON VARIABLES-AND-FORMS BY 'CDDR
		     COLLECT `(SET-GLOBALLY ',VAR ,FORM))))

(DEFUN (SETQ-GLOBALLY :UNDO-FUNCTION) (FORM &AUX RESULTS)
  (DO L (CDR FORM) (CDDR L) (NULL L)
      (COND ((BOUNDP-GLOBALLY (CAR L))
	     (PUSH `(SET-GLOBALLY ',(CAR L) ',(SYMEVAL-GLOBALLY (CAR L))) RESULTS))
	    (T (PUSH `(MAKUNBOUND-GLOBALLY ',(CAR L)) RESULTS))))
  `(PROGN . ,RESULTS))

))

; From file EHC.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(DEFUN SHOW-ADI (RP IDX)
  "Print the ADI at index IDX in the regular pdl RP"
  (FORMAT T "~2%Additional information supplied with call:")
  (DO ((IDX IDX (- IDX 2)))
      (())
    (LET ((TYPE (LDB %%ADI-TYPE (AR-1 RP IDX)))
	  (MORE-P (%P-LDB %%ADI-PREVIOUS-ADI-FLAG (AP-1 RP (1- IDX)))))
      (SELECT TYPE
	((ADI-RETURN-INFO ADI-USED-UP-RETURN-INFO)
	 (SHOW-MV-SPECS RP IDX))
	(ADI-BIND-STACK-LEVEL
	 (FORMAT T "~% Binding stack level: ~S" (AR-1 RP (1- IDX))))
	(ADI-RESTART-PC
	 (FORMAT T "~% Restart PC on *THROW: ~O" (AR-1 RP (1- IDX))))
	(OTHERWISE
	 (FORMAT T "~% ~S" (NTH TYPE ADI-KINDS))))
      (IF (ZEROP MORE-P) (RETURN)))))

))

; From file STREAM.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; STREAM  "

(DEFMETHOD (CHARACTER-STREAM :ELEMENT-TYPE) () 'CHARACTER)

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (FILE-INPUT-SIGNED-BINARY-STREAM-MIXIN :ELEMENT-TYPE) ()
  `(SIGNED-BYTE ,CURRENT-BYTE-SIZE))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (FILE-INPUT-PHONY-CHARACTER-STREAM-MIXIN :ELEMENT-TYPE) ()
  'CHARACTER)

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (FILE-OUTPUT-PHONY-CHARACTER-STREAM-MIXIN :ELEMENT-TYPE) ()
  'CHARACTER)

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN DECODE-ELEMENT-TYPE (ELEMENT-TYPE BYTE-SIZE)
  (DECLARE (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES))
  (IF (ATOM ELEMENT-TYPE)
      (SELECTQ ELEMENT-TYPE
	(:DEFAULT 
	 (VALUES ':DEFAULT BYTE-SIZE))
	(BIT
	 (VALUES NIL 1))
;No way to find out what byte size was used in this case.
;	(SIGNED-BYTE
;	 (VALUES NIL ':DEFAULT NIL T))
	(UNSIGNED-BYTE
	 (VALUES NIL ':DEFAULT))
	(STRING-CHAR
	 (VALUES T ':DEFAULT))
	(STANDARD-CHAR
	 (VALUES T ':DEFAULT))
	(CHARACTER
	 (VALUES NIL 16. T))
	(T (FERROR 'UNIMPLEMENTED-OPTION "~S is not implemented as an ELEMENT-TYPE."
		   ELEMENT-TYPE)))
    (SELECTQ (CAR ELEMENT-TYPE)
      (UNSIGNED-BYTE
       (VALUES NIL (CADR ELEMENT-TYPE)))
      (SIGNED-BYTE
       (VALUES NIL (CADR ELEMENT-TYPE) NIL T))
      (MOD
       (VALUES NIL (HAULONG (1- (CADR ELEMENT-TYPE)))))
      (T (FERROR 'UNIMPLEMENTED-OPTION "~S is not implemented as an ELEMENT-TYPE."
		 ELEMENT-TYPE)))))

(DEFMETHOD (FILE-BINARY-STREAM-MIXIN :ELEMENT-TYPE) ()
  `UNSIGNED-BYTE)

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFMETHOD (FILE-CHARACTER-STREAM-MIXIN :ELEMENT-TYPE) ()
  'STRING-CHAR)

))

; From file SHEET.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFMETHOD (SHEET :ELEMENT-TYPE) () 'CHARACTER)

))

; From file STREAM.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; STREAM  "

(DEFMETHOD (STREAM :ELEMENT-TYPE) ()
  (LET ((VALUE (SEND SELF ':SEND-IF-HANDLES ':BYTE-SIZE)))
    (IF VALUE `(UNSIGNED-BYTE ,VALUE) 'FIXNUM)))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN STREAM-DEFAULT-HANDLER (FCTN OP ARG1 ARGS &AUX TEM)
  "Subroutine which provides default definition of certain stream operations.
If a stream does not recognize an operation, it may call this function
to have the operation handled.  The stream should return whatever
this function returns to it.  OP should be the operation, FCTN should
be the stream which received the operation, and ARG1 and ARGS should be
the arguments that came with the operation."
  (SELECTQ OP
    ((:TYIPEEK :LISTEN)
     (COND ((SETQ TEM (FUNCALL FCTN ':TYI NIL))
	    (FUNCALL FCTN ':UNTYI TEM)
	    TEM)))
    ((:ANY-TYI :TYI-NO-HANG)
     (FUNCALL FCTN ':TYI ARG1))
    (:ANY-TYI-NO-HANG
     (FUNCALL FCTN ':ANY-TYI ARG1))
    ((:CLEAR-OUTPUT :CLEAR-INPUT :FORCE-OUTPUT :FINISH :CLOSE :EOF)
     NIL)
    (:FRESH-LINE
     (FUNCALL FCTN ':TYO #\CR)
     T)
    ((:STRING-OUT :LINE-OUT)
     (SETQ TEM (STRING ARG1))
     (DO ((LEN (COND ((SECOND ARGS))
		     (T (STRING-LENGTH TEM))))
	  (I (COND ((FIRST ARGS)) (T 0))
	     (1+ I)))
	 ((>= I LEN) NIL)
       (FUNCALL FCTN ':TYO (AR-1 TEM I)))
     (AND (EQ OP ':LINE-OUT)
	  (FUNCALL FCTN ':TYO #\CR)))
    (:LINE-IN
     (LET ((BUF (MAKE-ARRAY 100 ':TYPE ART-STRING
			    ':LEADER-LENGTH (IF (NUMBERP ARG1) ARG1 1))))
       (STORE-ARRAY-LEADER 0 BUF 0)		;Fill pointer
       (VALUES BUF
	       (DO ((TEM (FUNCALL FCTN ':TYI NIL) (FUNCALL FCTN ':TYI NIL)))
		   ((OR (NULL TEM) (= TEM #\CR) (= TEM #\END))
		    (ADJUST-ARRAY-SIZE BUF (ARRAY-ACTIVE-LENGTH BUF))
		    (NULL TEM))
		 (ARRAY-PUSH-EXTEND BUF TEM)))))
    (:STRING-IN
     ;; ARG1 = EOF, (CAR ARGS) = STRING
     (LOOP WITH START = (OR (CADR ARGS) 0)
	   AND END = (OR (CADDR ARGS) (ARRAY-LENGTH (CAR ARGS)))
	   WHILE (< START END)
	   AS CH = (FUNCALL FCTN ':TYI)
	   WHILE CH
	   DO (ASET CH (CAR ARGS) (PROG1 START (INCF START)))
	   FINALLY (AND (ARRAY-HAS-LEADER-P (CAR ARGS))
			(STORE-ARRAY-LEADER START (CAR ARGS) 0))
		   (AND (NULL CH) ARG1 (FERROR 'END-OF-FILE-1 "End of file on ~S." FCTN))
		   (RETURN (VALUES START (NULL CH)))))
    (:STRING-LINE-IN
     ;; ARG1 = EOF, (CAR ARGS) = STRING
     (LOOP WITH START = (OR (CADR ARGS) 0)
	   AND END = (OR (CADDR ARGS) (ARRAY-LENGTH (CAR ARGS)))
	   WHILE (< START END)
	   AS CH = (FUNCALL FCTN ':TYI)
	   WHILE (AND CH (NEQ CH #\RETURN))
	   DO (ASET CH (CAR ARGS) (PROG1 START (INCF START)))
	   FINALLY (AND (ARRAY-HAS-LEADER-P (CAR ARGS))
			(STORE-ARRAY-LEADER START (CAR ARGS) 0))
		   (AND (NULL CH) ARG1 (FERROR 'END-OF-FILE-1 "End of file on ~S." FCTN))
		   (RETURN (VALUES START (NULL CH) (NEQ CH #\RETURN)))))
    (:TYI-NO-HANG (FUNCALL FCTN ':TYI ARG1))
    (:OPERATION-HANDLED-P (MEMQ ARG1 (FUNCALL FCTN ':WHICH-OPERATIONS)))
    (:CHARACTERS T)
    (:ELEMENT-TYPE
     (IF (FUNCALL FCTN ':CHARACTERS) 'CHARACTER
       (LET ((VALUE (FUNCALL FCTN ':SEND-IF-HANDLES ':BYTE-SIZE)))
	 (IF VALUE `(UNSIGNED-BYTE ,VALUE) 'FIXNUM))))
    (:DIRECTION
     (LET ((OPS (FUNCALL FCTN ':WHICH-OPERATIONS)))
       (IF (MEMQ ':TYI OPS)
	   (IF (MEMQ ':TYO OPS) ':BIDIRECTIONAL ':INPUT)
	 (IF (MEMQ ':TYO OPS) ':OUTPUT NIL))))
    (:SEND-IF-HANDLES
     (IF (MEMQ ARG1 (FUNCALL FCTN ':WHICH-OPERATIONS))
	 (LEXPR-FUNCALL FCTN ARG1 ARGS)))
    (OTHERWISE
     (FERROR ':UNCLAIMED-MESSAGE "The stream operation ~S is not supported by ~S"
	     OP FCTN))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN STREAM-ELEMENT-TYPE (STREAM)
  "Return a Common Lisp type describing the objects input or output by STREAM.
This will be either CHARACTER or STRING-CHAR or a subtype of INTEGER."
  (OR (SEND STREAM ':SEND-IF-HANDLES ':ELEMENT-TYPE)
      (IF (SEND STREAM ':CHARACTERS)
	  'CHARACTER
	(LET ((VALUE (SEND STREAM ':SEND-IF-HANDLES ':BYTE-SIZE)))
	  (IF VALUE `(UNSIGNED-BYTE ,VALUE) 'FIXNUM)))))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFMETHOD (BINARY-OUTPUT-STREAM-MIXIN :ELEMENT-TYPE) () '(UNSIGNED-BYTE 8))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFMETHOD (CHARACTER-OUTPUT-STREAM-MIXIN :ELEMENT-TYPE) () 'STRING-CHAR)

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFMETHOD (BINARY-INPUT-STREAM-MIXIN :ELEMENT-TYPE) () '(UNSIGNED-BYTE 8))

))

; From file CHSAUX.LISP SRC:<L.IO> OZ:
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; CHSAUX  "

(DEFMETHOD (CHARACTER-INPUT-STREAM-MIXIN :ELEMENT-TYPE) () 'STRING-CHAR)

))

; From file PATED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFCOM COM-ADD-PATCH "Add the current defun or the region (if any) to the patch buffer.
If there is no patch buffer, ask the user for the system to patch. Then reserve a new
version number, and create a buffer whose pathname is the source file name for the
patch creating that version number.  If there is a region, append it to the end of the
patch buffer; otherwise append the current defun to the end of the patch buffer." ()
  (LET (BP1 BP2 DEFUN-NAME)
    (COND ((WINDOW-MARK-P *WINDOW*)
	   ;; there is a region, use it.
	   (SETQ BP1 (MARK) BP2 (POINT))
	   (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	   (CHECK-INTERVAL-SECTIONS BP1 BP2 T)
	   (SETQ DEFUN-NAME "the region"))
	  ((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
	   ;; No region, try to get containing defun.
	   (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	   (CHECK-INTERVAL-SECTIONS BP1 BP2 T)
	   (SETQ DEFUN-NAME (SECTION-NODE-NAME (LINE-NODE (BP-LINE BP1)))))
	  (T
	   (BARF "Unbalanced parentheses or no defuns.")))
    (ADD-PATCH-INTERVAL BP1 BP2 T DEFUN-NAME *INTERVAL*))
  DIS-MARK-GOES)

))

; From file EHF.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN GET-OWN-SPECIAL-PDL-POINTER (&OPTIONAL (SP (SG-SPECIAL-PDL CURRENT-STACK-GROUP)))
  "Return the current special pdl pointer of the current stack group.."
  (%POINTER-DIFFERENCE (SPECIAL-PDL-INDEX)
		       (ALOC SP 0)))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "


(DEFUN SG-BINDING-POSITION (SG VARIABLE)
  "Return the position in SG's specpdl of the outermost binding of VARIABLE.
Returns NIL if VARIABLE is not bound in stack group SG."
  (DO ((I 0 (1+ I))
       (LIM (1+ (SG-SPECIAL-PDL-POINTER SG)))
       (SP (SG-SPECIAL-PDL SG)))
      ((= I LIM))
    (IF (AND (%P-POINTERP (ALOC SP I))
	     (EQ (%P-CONTENTS-AS-LOCATIVE (ALOC SP I))
		 (VALUE-CELL-LOCATION VARIABLE)))
	(RETURN (1- I)))))

(DEFVAR INSERT-BINDING-IN-CLOSURE-TEMP)

(DEFUN INSERT-BINDING-IN-CLOSURE (CLOSURE VARIABLE BEFORE-VARIABLE &OPTIONAL XVCELL)
  "Put a binding of VARIABLE into CLOSURE, unless there is one already.
Returns T if a new binding was inserted.
The binding's initial value is copied from the current global binding.
If we are running inside CLOSURE, a suitable binding for VARIABLE
is entered in the binding stack so that it is in effect now.
This is done by looking for an existing binding of BEFORE-VARIABLE
and inserting the new binding before it.  BEFORE-VARIABLE should be
a variable that one can assume that CLOSURE would bind and that
nothing else would bind.

If XVCELL is non-NIL, it should be a pointer to a cell
which is used as the closure value cell.  In this case,
the cell's contents are not changed, so they become VARIABLE's value."
  (WITHOUT-INTERRUPTS
    (UNLESS (GET-LOCATION-OR-NIL (%MAKE-POINTER DTP-LIST CLOSURE)
				 (VALUE-CELL-LOCATION VARIABLE))
      (LET ((POSITION (SG-BINDING-POSITION CURRENT-STACK-GROUP BEFORE-VARIABLE))
	    (OVCELL (FOLLOW-CELL-FORWARDING (VALUE-CELL-LOCATION VARIABLE) T)))
	(UNLESS XVCELL
	  (SETQ XVCELL (LIST NIL))
	  ;; Copy the current global value into the new binding cell.
	  (%P-STORE-DATA-TYPE XVCELL (%P-DATA-TYPE OVCELL))
	  (%P-STORE-POINTER XVCELL (%P-POINTER OVCELL)))
	(WHEN POSITION
	  ;; Note: SG-INSERT-SPECIAL-BINDING has no way to update
	  ;; the actual SP pointer we are running with.
	  ;; Therefore, its effect is to push this binding of INSERT-BINDING-IN-CLOSURE-TEMP
	  ;; off the top of the stack and into oblivion.
	  ;; If we did not bind INSERT-BINDING-IN-CLOSURE-TEMP,
	  ;; our binding of INHIBIT-SCHEDULING-FLAG
	  ;; would get moved into oblivion and our binding block would vanish,
	  ;; leaving the stack out of synch.
	  (BIND (LOCF INSERT-BINDING-IN-CLOSURE-TEMP) NIL)
	  (SETF (SG-SPECIAL-PDL-POINTER CURRENT-STACK-GROUP)
		(GET-OWN-SPECIAL-PDL-POINTER))
	  (SG-INSERT-SPECIAL-BINDING CURRENT-STACK-GROUP POSITION
				     (VALUE-CELL-LOCATION VARIABLE) T)
	  (%P-DPB (%P-LDB %%SPECPDL-BLOCK-START-FLAG
			  (ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) (+ POSITION 2)))
		  %%SPECPDL-BLOCK-START-FLAG
		  (ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) POSITION))
	  (%P-DPB 0 %%SPECPDL-BLOCK-START-FLAG
		  (ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) (+ POSITION 2)))
	  (%P-STORE-DATA-TYPE (VALUE-CELL-LOCATION VARIABLE) DTP-FIX)
	  (%P-STORE-POINTER (VALUE-CELL-LOCATION VARIABLE) XVCELL)
	  (%P-STORE-DATA-TYPE (VALUE-CELL-LOCATION VARIABLE)
			      DTP-EXTERNAL-VALUE-CELL-POINTER))
	(SETF (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
	      (LIST* (VALUE-CELL-LOCATION VARIABLE)
		     (%MAKE-POINTER DTP-LOCATIVE XVCELL)
		     (CDR (%MAKE-POINTER DTP-LIST CLOSURE)))))
      T)))

(DEFUN DELETE-BINDING-FROM-CLOSURE (CLOSURE VARIABLE)
  "Remove the binding of VARIABLE from CLOSURE.
If VARIABLE is currently bound, that binding is assumed to come from CLOSURE
and is therefore removed from the binding stack."
  (LET ((POSITION (SG-BINDING-POSITION CURRENT-STACK-GROUP VARIABLE)))
    (WHEN POSITION
      (WITHOUT-INTERRUPTS
	(%P-DPB (%P-LDB %%SPECPDL-BLOCK-START-FLAG
			(ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) POSITION))
		%%SPECPDL-BLOCK-START-FLAG
		(ALOC (SG-SPECIAL-PDL CURRENT-STACK-GROUP) (+ POSITION 2)))
	;; Deleting the binding does not update the actual specpdl pointer
	;; we are running with.  So it has the effect of duplicating this binding
	;; of INSERT-BINDING-IN-CLOSURE-TEMP.
	;; That does no harm BECAUSE this is not the start of a binding block.
	(BIND (LOCF INSERT-BINDING-IN-CLOSURE-TEMP) NIL)
	(SETF (SG-SPECIAL-PDL-POINTER CURRENT-STACK-GROUP)
	      (GET-OWN-SPECIAL-PDL-POINTER))
	(SG-DELETE-SPECIAL-BINDING CURRENT-STACK-GROUP POSITION))))
  (REMPROP (%MAKE-POINTER DTP-LIST CLOSURE)
	   (VALUE-CELL-LOCATION VARIABLE)))

(DEFUN SG-INSERT-SPECIAL-BINDING (SG POSITION BOUND-LOCATION &OPTIONAL CLOSURE-FLAG)
  "Insert a binding for BOUND-LOCATION into the special pdl for SG.
The binding is inserted at position POSITION in the special pdl,
and the data that was at POSITION is moved up.
The new binding is part of the binding block that precedes it.
The old value saved in the binding is the current contents of BOUND-LOCATION.
If CLOSURE-FLAG is non-NIL, the inserted binding is marked as
 /"made by closure entry/" as opposed to /"made by execution of the frame's function/".
**WARNING** unsafe to use on the current stack group
without hairy precautions; see source for EH:INSERT-BINDING-FROM-CLOSURE."
  (RELOCATE-SPECPDL-PORTION SG POSITION 2)
  (%BLT-TYPED BOUND-LOCATION (ALOC (SG-SPECIAL-PDL SG) POSITION) 1 1)
  (SETF (AREF (SG-SPECIAL-PDL SG) (1+ POSITION)) BOUND-LOCATION)
  (%P-STORE-CDR-CODE (ALOC (SG-SPECIAL-PDL SG) POSITION) 0)
  (%P-STORE-CDR-CODE (ALOC (SG-SPECIAL-PDL SG) (1+ POSITION)) 0)
  (%P-DPB (IF CLOSURE-FLAG 1 0) %%SPECPDL-CLOSURE-BINDING
	  (ALOC (SG-SPECIAL-PDL SG) POSITION))
  (%P-DPB (IF CLOSURE-FLAG 1 0) %%SPECPDL-CLOSURE-BINDING
	  (ALOC (SG-SPECIAL-PDL SG) (1+ POSITION))))

(DEFUN SG-DELETE-SPECIAL-BINDING (SG POSITION)
  "Delete one binding from the special pdl for SG.
The binding is deleted at position POSITION in the special pdl,
and the data that was at POSITION is moved down.
**WARNING** unsafe to use on the current stack group
without hairy precautions; see source for EH:DELETE-BINDING-FROM-CLOSURE."
  ;; Restore the binding's saved old value.
  (%BLT-TYPED (ALOC (SG-SPECIAL-PDL SG) POSITION)
	      (AREF (SG-SPECIAL-PDL SG) (1+ POSITION))
	      1 1)
  (RELOCATE-SPECPDL-PORTION SG POSITION -2))

(DEFUN RELOCATE-SPECPDL-PORTION (SG START DISTANCE)
  (CHECK-ARG-TYPE DISTANCE FIXNUM)
  (CHECK-ARG-TYPE START FIXNUM)
  (CHECK-ARG-TYPE SG STACK-GROUP)
  (UNLESS (ZEROP DISTANCE)
    (LET ((SP (SG-SPECIAL-PDL SG))
	  (SPP (SG-SPECIAL-PDL-POINTER SG))
	  (RP (SG-REGULAR-PDL SG)))
      (IF (PLUSP DISTANCE)
	  (%BLT (ALOC SP SPP) (ALOC SP (+ SPP DISTANCE))
		(- SPP START -1) -1)
	(%BLT (ALOC SP (- START DISTANCE)) (ALOC SP START)
	      (- SPP START -1 (- DISTANCE)) 1))
      (LET ((SPBEG (ALOC SP 0)))
	(DOTIMES (I (IF (EQ SG CURRENT-STACK-GROUP)
			(SG-INNERMOST-OPEN SG)
		      (1+ (SG-REGULAR-PDL-POINTER SG))))
	  (IF (AND (%POINTERP (AREF RP I))
		   ( START (%POINTER-DIFFERENCE (AREF RP I) SPBEG) SPP))
	      (SETF (AREF RP I)
		    (%MAKE-POINTER-OFFSET (%DATA-TYPE (AREF RP I))
					  (AREF RP I)
					  DISTANCE)))))
      (INCF (SG-SPECIAL-PDL-POINTER SG) DISTANCE)
      (DO ((FRAME (SG-INNERMOST-OPEN SG) (SG-NEXT-OPEN SG FRAME)))
	  ((NULL FRAME))
	(IF (NOT (ZEROP (RP-ADI-PRESENT RP FRAME)))
	    (DO ((IDX (- FRAME %LP-CALL-BLOCK-LENGTH) (- IDX 2)))
		(())
	      (WHEN (= (LDB %%ADI-TYPE (AR-1 RP IDX)) ADI-BIND-STACK-LEVEL)
		(INCF (AREF RP (1- IDX)) DISTANCE))
	      (IF (ZEROP (%P-LDB %%ADI-PREVIOUS-ADI-FLAG (AP-1 RP (1- IDX))))
		  (RETURN))))))))

))

; From file DEFS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFSUBST BUFFER-SAVED-LOCAL-VARIABLES (BUFFER)
  (GET BUFFER 'SAVED-LOCAL-VARIABLES))

))

; From file DEFS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFVAR *LOCAL-VARIABLES* NIL
  "List of variables given local values in this editor.
These variables are also on *LOCAL-BOUND-VARIABLES*
unless they are bound in the editor closure initially.")
(DEFVAR *LOCAL-BOUND-VARIABLES* NIL
  "List of variables added by user to this editor closure.")

))

; From file ZMACS.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN MAKE-BUFFER-CURRENT (BUFFER &OPTIONAL PRESERVE-BUFFER-HISTORY)
  "Make BUFFER the current ZMACS buffer in the selected window.
PRESERVE-BUFFER-HISTORY non-NIL says do not reorder the buffers for C-M-L, etc."
  (CHECK-ARG-TYPE BUFFER ZMACS-BUFFER)
  ;; Save away the major and minor modes, and turn them off.
  (WHEN *INTERVAL*
    (SETF (BUFFER-SAVED-MODE-LIST *INTERVAL*) *MODE-LIST*)
    (SETF (BUFFER-SAVED-MAJOR-MODE *INTERVAL*) *MAJOR-MODE*)
    (SETF (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)
	  (MAPCAR #'(LAMBDA (V)
		      (CONS V (IF (MEMQ V *LOCAL-BOUND-VARIABLES*)
				  (%P-CONTENTS-AS-LOCATIVE (VALUE-CELL-LOCATION V))
				(LIST (SYMEVAL V)))))
		  *LOCAL-VARIABLES*))
    (UN-SET-MODES)
    ;; Update *ZMACS-BUFFER-LIST*, for C-X C-B,
    ;; and this window's buffer history, for C-M-L.
    (UNLESS PRESERVE-BUFFER-HISTORY
      (UPDATE-BUFFER-HISTORY *INTERVAL* BUFFER)))
  ;; Point the window at this interval, and make it the default interval.
  ;; If called from the two-window commands, the window may already be
  ;; pointing to this buffer, in which case don't change it
  (OR (EQ (WINDOW-INTERVAL *WINDOW*) BUFFER)
      (SEND *WINDOW* ':SET-INTERVAL-INTERNAL BUFFER))
  (SETQ *INTERVAL* BUFFER)
  (UPDATE-BUFFER-NAMES BUFFER)
  ;; Recompute which package READs should be done in.
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  ;; Restore the old major and minor modes.
  (SET-MODES (BUFFER-SAVED-MODE-LIST *INTERVAL*) (BUFFER-SAVED-MAJOR-MODE *INTERVAL*)
	     (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*))
  NIL)

))

; From file MODES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MODES  "

;;; Turn off all modes.  For ZMACS.
(DEFUN UN-SET-MODES ()
  "Turns off all modes that are on."
  (DOLIST (V *LOCAL-BOUND-VARIABLES*)
    (EH:DELETE-BINDING-FROM-CLOSURE *EDITOR* V))
  (DOLIST (V *LOCAL-VARIABLES*)
    (UNLESS (MEMQ V *LOCAL-BOUND-VARIABLES*)
      (SET V (SI:SYMEVAL-GLOBALLY V))))
  (DOLIST (L *MODE-LIST*)
    (MAPC #'EVAL (SECOND L)))
  (SETQ *MODE-NAME-LIST* NIL
	*MODE-LIST* NIL)
  (SETQ *LOCAL-VARIABLES* NIL)
  (SETQ *LOCAL-BOUND-VARIABLES* NIL))

;;; Turn on a saved set of modes.  For ZMACS.
(DEFUN SET-MODES (MODE-LIST MAJOR-MODE &OPTIONAL LOCAL-VARIABLES)
  "Turn on the modes in MODE-LIST, and major mode MAJOR-MODE.
MAJOR-MODE, and the elements of MODE-LIST, are mode symbols, like LISP-MODE."
  (SET-MODES-UNDERSCORE MODE-LIST)
  (SETQ *MAJOR-MODE* MAJOR-MODE)
  (TURN-ON-MODE *MAJOR-MODE*)
  (DOLIST (V LOCAL-VARIABLES)
    (MAKE-LOCAL-VARIABLE (CAR V) NIL (CDR V))
    (SET (CAR V) (CADR V))))

(DEFUN MAKE-LOCAL-VARIABLE (VARIABLE &OPTIONAL (VALUE NIL VALUEP) XVCELL)
  "Make VARIABLE be bound locally in the editor that is running.
If XVCELL is non-NIL, we use it as the closure value cell for the variable
 if the variable does not already have one.
If XVCELL is NIL, then if VALUE was specified, that becomes the
 new value of the variable; otherwise, the value does not change."
  ;; Make sure that our editor closure includes *LOCAL-VARIABLES*
  ;; This is a kludge to get around the fact that in system 98
  ;; all the closure variable lists omit *LOCAL-VARIABLES*.
  ;; The following four lines are not needed after system 98.
  (UNLESS *LOCAL-VARIABLES*
    (EH:INSERT-BINDING-IN-CLOSURE *EDITOR* '*LOCAL-VARIABLES* '*EDITOR*))
  (UNLESS *LOCAL-BOUND-VARIABLES*
    (EH:INSERT-BINDING-IN-CLOSURE *EDITOR* '*LOCAL-BOUND-VARIABLES* '*EDITOR*))
  (UNLESS (MEMQ VARIABLE *LOCAL-VARIABLES*)
    (PUSH VARIABLE *LOCAL-VARIABLES*)
    (IF (EH:INSERT-BINDING-IN-CLOSURE *EDITOR* VARIABLE '*EDITOR* XVCELL)
	(PUSH VARIABLE *LOCAL-BOUND-VARIABLES*)
      (WHEN XVCELL
	(SET VARIABLE (CAR XVCELL)))))
  (UNLESS XVCELL
    (WHEN VALUEP
      (SET VARIABLE VALUE)))
  (WHEN (TYPEP *INTERVAL* 'ZMACS-BUFFER)
    ;; Put this variable, with correct vcell or value,
    ;; on the buffer's local variable list.  Don't wait for a buffer switch.
    (LET ((AELT (ASSQ VARIABLE (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*))))
      (UNLESS AELT
	(SETQ AELT (CONS VARIABLE NIL))
	(PUSH AELT (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)))
      (IF (MEMQ VARIABLE *LOCAL-BOUND-VARIABLES*)
	  (SETF (CDR AELT) (%P-CONTENTS-AS-LOCATIVE (VALUE-CELL-LOCATION VARIABLE)))
	(SETF (CDR AELT) (LIST (SYMEVAL VARIABLE)))))))

(DEFUN KILL-LOCAL-VARIABLE (VARIABLE)
  "Make VARIABLE no longer be bound locally in the editor that is running.
Its value reverts to the global value."
  (WHEN (TYPEP *INTERVAL* 'ZMACS-BUFFER)
    (SETF (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)
	  (CLI:DELETE VARIABLE (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)
		      ':KEY 'CAR)))
  (WHEN (MEMQ VARIABLE *LOCAL-BOUND-VARIABLES*)
    (EH:DELETE-BINDING-FROM-CLOSURE *EDITOR* VARIABLE)
    (SETQ *LOCAL-BOUND-VARIABLES* (DELQ VARIABLE *LOCAL-BOUND-VARIABLES*)))
  (SETQ *LOCAL-VARIABLES* (DELQ VARIABLE *LOCAL-VARIABLES*)))

))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "


(DEFCOM COM-MAKE-LOCAL-VARIABLE "Make editor user option variable local to this buffer.
Reads the name of a variable (with completion)
and makes the variable local to the current buffer
so that if you set it you will not affect any other buffer." ()
  (LET ((VARNAME (COMPLETING-READ-FROM-MINI-BUFFER
		   "Variable name:" *VARIABLE-ALIST* NIL NIL
		   "You are typing the name of a variable to be made local to this buffer."
		   #'(LAMBDA (X)
		       (PRINT-VARIABLE (CDR X))
		       (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
    (UNLESS (CONSP VARNAME) (BARF))
    (MAKE-LOCAL-VARIABLE (CDR VARNAME)))
  DIS-NONE)

(DEFCOM COM-KILL-LOCAL-VARIABLE "Make editor user option variable global in this buffer.
Reads the name of a variable (with completion)
and makes the variable no longer be local to this buffer,
so that this buffer will share the value with most other buffers." ()
  (LET ((VARNAME (COMPLETING-READ-FROM-MINI-BUFFER
		   "Variable name:" *VARIABLE-ALIST* NIL NIL
		   "You are typing the name of a variable to be made local to this buffer."
		   #'(LAMBDA (X)
		       (PRINT-VARIABLE (CDR X))
		       (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
    (UNLESS (CONSP VARNAME) (BARF))
    (KILL-LOCAL-VARIABLE (CDR VARNAME)))
  DIS-NONE)

(DEFCOM COM-LIST-LOCAL-VARIABLES "List editor user option variables local in this buffer." ()
  (DOLIST (VARNAME *LOCAL-VARIABLES*)
    (PRINT-VARIABLE VARNAME)
    (FORMAT T "~A~&" (DOCUMENTATION VARNAME 'VARIABLE)))
  DIS-NONE)

(SET-COMTAB *STANDARD-COMTAB* ()
  (MAKE-COMMAND-ALIST '(COM-MAKE-LOCAL-VARIABLE COM-KILL-LOCAL-VARIABLE COM-LIST-LOCAL-VARIABLES)))
))

; From file COMD.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-SET-VARIABLE "Set editor user option variable, checking type.
Read the name of a variable (with completion), display current value
and documentation, and read a new variable.  Some checking is done
that the variable is the right type." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
	    "Variable name:" *VARIABLE-ALIST* NIL NIL
	    "You are typing the name of a variable to be set."
	    #'(LAMBDA (X)
		   (PRINT-VARIABLE (CDR X))
		   (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
     (AND (EQUAL X "") (BARF))
     (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))
     (PRINT-VARIABLE (CDR X))
     (LET* ((VAR (CDR X))
	    (*MINI-BUFFER-DEFAULT-STRING* (VARIABLE-STRING VAR))
	    (DEFAULT *MINI-BUFFER-DEFAULT-STRING*)
	    (PACKAGE (PKG-FIND-PACKAGE "ZWEI"))
	    (TYPE (GET VAR 'VARIABLE-TYPE))
	    (IBASE 10.) (BASE 10.))
       (SET VAR
	    (SELECTQ TYPE
	      (:CHAR
	       (LET ((V (TYPEIN-LINE-READLINE "New value (one character)")))
		 (OR (= (STRING-LENGTH V) 1) (BARF "~A is not one character." V))
		 (LDB %%CH-CHAR (AREF V 0))))
	      (:CHAR-LIST
	       (LET ((V (TYPEIN-LINE-READLINE-WITH-DEFAULT DEFAULT "New value (a string)")))
		 (DO ((I 0 (1+ I))
		      (RET)
		      (LIM (STRING-LENGTH V)))
		     (( I LIM) (NREVERSE RET))
		   (PUSH (LDB %%CH-CHAR (AREF V I)) RET))))
	      (:STRING
	       (TYPEIN-LINE-READLINE-WITH-DEFAULT DEFAULT "New value (a string)"))
	      ((:PIXEL :FIXNUM)
	       (LET ((V (TYPEIN-LINE-READ "New value (a fixnum)")))
		 (OR (FIXP V) (BARF "~S is not a fixnum." V))
		 V))
	      ((:FIXNUM-OR-NIL :PIXEL-OR-NIL)
	       (LET ((V (TYPEIN-LINE-READ "New value (NIL or a fixnum)")))
		 (OR (FIXP V) (NULL V) (BARF "~S is neither a fixnum not NIL." V))
		 V))
	      (:SMALL-FRACTION
	       (LET ((V (TYPEIN-LINE-READ "New value (a flonum between 0.0 and 1.0")))
		 (OR (FLOATP V) (BARF "~S is not a floating-point number." V))
		 (OR (AND ( V 0.0s0) ( V 1.0s0))
		     (BARF "~S is not between 0.0 and 1.0" V))
		 (SMALL-FLOAT V)))
	      (:BOOLEAN
	       (LET ((V (TYPEIN-LINE-READ "New value (T or NIL)")))
		 (OR (MEMQ V '(T NIL)) (BARF "~S is neither T nor NIL." V))
		 V))
	      (:KEYWORD
	       (LET ((V (TYPEIN-LINE-READ-WITH-DEFAULT DEFAULT "New value (a symbol)")))
		 (OR (SYMBOLP V) (BARF "~S is not a symbol." V))
		 V))
	      (:ANYTHING
	       (TYPEIN-LINE-READ-WITH-DEFAULT DEFAULT "New value"))))
       ;; If variable is local, record its new value in the buffer
       ;; in case the buffer is selected in another window.
       (IF (MEMQ VAR *LOCAL-VARIABLES*)
           (MAKE-LOCAL-VARIABLE VAR (SYMEVAL VAR)))))
  DIS-NONE)

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN VAR-MAKE-HOME (NAME TYPE KIND INIT-SPECS EVAL-TYPE MISC-TYPES &AUX HOME)
    (COND ((NULL (MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST
                                          FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
           (BARF KIND 'BAD-KIND 'BARF)))
    (IF (EQ (SYMBOL-PACKAGE NAME) SI:PKG-KEYWORD-PACKAGE)
	(WARN 'KEYWORD-BOUND ':IMPOSSIBLE
	      "Binding the keyword symbol ~S." NAME))
    (IF (GET NAME 'SYSTEM-CONSTANT)
	(WARN 'SYSTEM-CONSTANT-BOUND ':IMPLAUSIBLE
	      "Binding the system constant symbol ~S." NAME))
    (IF (AND (MEMQ NAME (CDDR SELF-FLAVOR-DECLARATION))
	     (EQ TYPE 'FEF-LOCAL))
	(WARN 'INSTANCE-VARIABLE-BOUND ':IMPLAUSIBLE
	      "Rebinding the instance variable ~S.  The new binding will be local."
	      NAME))
    ;; Rest args interfere with fast arg option except when there are no specials.
    ;; We need to look at this to
    ;;  decide how to process all the AUX variables and can't tell when processing
    ;;  the first one whether the next will be special.
    ;;  In any case, being wrong about this should not be able to produce
    ;;  incorrect code.
    (COND ((EQ KIND 'FEF-ARG-REST)
           (SETQ FAST-ARGS-POSSIBLE NIL)))
    (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
           (AND INIT-SPECS (SETQ FAST-ARGS-POSSIBLE NIL))))
    ;; Detect vars bound to themselves which fail to be special.
    (COND ((AND (EQ NAME (CAR INIT-SPECS))
                (NOT (ASSQ NAME VARS))
		;; If variable is already accessible lexically, it need not be special.
		(DOLIST (FRAME COMPILER-LEXICAL-ENVIRONMENT T)
		  (WHEN (ASSQ NAME FRAME) (RETURN NIL))))
           (MSPL2 NAME)
           (SETQ TYPE 'FEF-SPECIAL)))
    ;; Cons up the variable descriptor.
    ;; Note that INIT-SPECS is not the final value that will go in the INIT slot.
    (SETQ HOME (MAKE-VAR NAME NAME KIND KIND TYPE TYPE
			 INIT INIT-SPECS EVAL EVAL-TYPE MISC MISC-TYPES))
    (SETF (VAR-LAP-ADDRESS HOME)
	  ;; Not the real lap address,
	  ;; but something for P1 to use for the value of the variable
	  (IF (EQ TYPE 'FEF-SPECIAL) NAME `(LOCAL-REF ,HOME)))
    HOME)

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "


(DEFUN (:INTEGER PROMPT-AND-READ-FUNCTION) (OPTION STREAM)
  (LET ((IBASE (OR (AND (CONSP OPTION) (GET OPTION ':INPUT-RADIX)) IBASE))
	(STRING (READLINE-TRIM STREAM)))
    (IF (AND (CONSP OPTION) (GET OPTION ':OR-NIL)
	     (EQUAL STRING ""))
	NIL
      (CONDITION-CASE ()
	  (LET* ((NUMBER (READ-FROM-STRING STRING)))
	    (IF (AND (NUMBERP NUMBER) (INTEGERP NUMBER)) NUMBER
	      (FERROR 'READ-ERROR-1 "That is not an integer.")))
	(END-OF-FILE (FERROR 'READ-ERROR-1 "That is not an integer."))))))

(DEFUN (:SMALL-FRACTION PROMPT-AND-READ-FUNCTION) (OPTION STREAM)
  (LET ((STRING (READLINE-TRIM STREAM)))
    (IF (AND (CONSP OPTION) (GET OPTION ':OR-NIL)
	     (EQUAL STRING ""))
	NIL
      (CONDITION-CASE ()
	  (LET* ((NUMBER (READ-FROM-STRING STRING)))
	    (IF (AND (NUMBERP NUMBER) (REALP NUMBER) ( 0.0 NUMBER 1.0))
		(FLOAT NUMBER)
	      (FERROR 'READ-ERROR-1 "That is not a fraction between 0 and 1.")))
	(END-OF-FILE (FERROR 'READ-ERROR-1 "That is not a fraction between 0 and 1."))))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN (:CHARACTER-LIST PROMPT-AND-READ-FUNCTION) (IGNORE STREAM)
  (CONCATENATE 'LIST (READLINE STREAM)))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "


(DEFUN (:CHOOSE PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION QUERY-IO)
  (LET ((CHOICES (GET OPTION ':CHOICES)))
    (WITH-INPUT-EDITING (QUERY-IO
			  `((:PROMPT ,#'(LAMBDA (&REST ARGS)
					  (APPLY 'PROMPT-AND-READ-PROMPT-FUNCTION ARGS)
					  (FRESH-LINE QUERY-IO)
					  (DO ((CHOICES CHOICES (CDR CHOICES))
					       (I 0 (1+ I)))
					      ((NULL CHOICES))
					    (FORMAT QUERY-IO "~& Type ~D for ~S"
						    I (CAR CHOICES)))
					  (TERPRI QUERY-IO)))
			    (:ACTIVATION MEMQ (#\END #\RETURN))))
      (NTH (READ QUERY-IO)
	   CHOICES))))

(DEFUN (:BOOLEAN PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (IGNORE QUERY-IO)
  (APPLY 'Y-OR-N-P PROMPT-AND-READ-FORMAT-STRING PROMPT-AND-READ-FORMAT-ARGS))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN (:ASSOC PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION QUERY-IO)
  (LET ((CHOICES (GET OPTION ':CHOICES)))
    (WITH-INPUT-EDITING (QUERY-IO
			  `((:PROMPT ,#'(LAMBDA (&REST ARGS)
					  (APPLY 'PROMPT-AND-READ-PROMPT-FUNCTION ARGS)
					  (FRESH-LINE QUERY-IO)
					  (DO ((CHOICES CHOICES (CDR CHOICES))
					       (I 0 (1+ I)))
					      ((NULL CHOICES))
					    (FORMAT QUERY-IO "~& Type ~D for ~S"
						    I (CAAR CHOICES)))
					  (TERPRI QUERY-IO)))
			    (:ACTIVATION MEMQ (#\END #\RETURN))))
      (CDR (NTH (READ QUERY-IO)
		CHOICES)))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN (:CHARACTER PROMPT-AND-READ-NO-RUBOUT-FUNCTION) (OPTION STREAM)
  (PROMPT-AND-READ-PROMPT-FUNCTION STREAM NIL)
  (LET ((CHAR (SEND STREAM ':TYI))
	(STANDARD-OUTPUT STREAM))
    (IF (AND (CONSP OPTION) (GET OPTION ':OR-NIL)
	     (= CHAR #\CLEAR-INPUT))
	(PROGN (PRINC "none") NIL)
      (FORMAT:OCHAR CHAR ':EDITOR)
      CHAR)))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN (:STRING-LIST PROMPT-AND-READ-FUNCTION) (IGNORE STREAM)
  (LET ((STR1 (READLINE STREAM))
	J ACCUM)
    (DO ((I 0))
	(())
      (SETQ J (STRING-SEARCH-CHAR #/, STR1 I))
      (LET ((STR2 (STRING-TRIM " " (SUBSTRING STR1 I J))))
	(UNLESS (EQUAL STR2 "")
	  (PUSH STR2 ACCUM)))
      (UNLESS J (RETURN (NREVERSE ACCUM)))
      (SETQ I (1+ J)))))

))

; From file CLPACK.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN FIND-PACKAGE (NAME &OPTIONAL USE-LOCAL-NAMES-PACKAGE)
  "Returns the package whose name is NAME.
We look first for local names definde in package USE-LOCAL-NAMES-PACKAGE,
then for global package names.  USE-LOCAL-NAMES-PACKAGE is recommended
for use only in parsing package prefixes.
Returns NIL if there is no such package."
  (CHECK-ARG NAME (OR (STRINGP NAME) (CONSP NAME) (SYMBOLP NAME)
		      (PACKAGEP NAME))
	     "a string, symbol, list or package")
  (COND ((PACKAGEP NAME) NAME)
	((CONSP NAME)
	 (OR (FIND-PACKAGE (CAR NAME) USE-LOCAL-NAMES-PACKAGE)
	     (IF (OR (NULL (CDDR NAME))  ;List has length 2 -- cannot be new style.
		     (AND (NULL (CDDDR NAME))  ;Length 3 -- may be old style or new.
			  (OR (CONSP (CADR NAME))  ;so see if it makes sense in old style.
			      (AND (SYMBOLP (CADR NAME))  ;For that, 2nd elt must be a package
				   (FIND-PACKAGE (CADR NAME))))))  ;name, or a list of them.
		 (MAKE-PACKAGE (CAR NAME)
			       (IF (AND (CADR NAME) (ATOM (CADR NAME))) ':SUPER)
			       (CADR NAME)
			       ':USE (IF (CONSP (CADR NAME)) (CADR NAME))
			       (IF (CADDR NAME) ':SIZE) (CADDR NAME))
	       (APPLY 'MAKE-PACKAGE NAME))))
	((OR (STRINGP NAME) (SYMBOLP NAME))
	 (OR (AND USE-LOCAL-NAMES-PACKAGE
		  (DO ((P USE-LOCAL-NAMES-PACKAGE (PKG-SUPER-PACKAGE P T)))
		      ((NULL P))
		    (LET ((ELT (ASS 'EQUAL (STRING NAME)
				    (PKG-REFNAME-ALIST USE-LOCAL-NAMES-PACKAGE))))
		      (WHEN ELT (RETURN (CDR ELT))))))
	     (DOLIST (PKG *ALL-PACKAGES*)
	       (WHEN (OR (EQUAL (PKG-NAME PKG) (STRING NAME))
			 (MEM 'EQUAL (STRING NAME) (PKG-NICKNAMES PKG)))
		 (RETURN PKG)))))))

))

; From file STREAM.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; STREAM  "


(DEFMETHOD (INPUT-STREAM :ANY-TYI) (&OPTIONAL EOF)
  (SEND SELF ':TYI EOF))

(DEFMETHOD (INPUT-STREAM :ANY-TYI-NO-HANG) (&OPTIONAL EOF)
  (SEND SELF ':TYI EOF))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro (function locf-method) (function-spec)
  (if (validate-function-spec function-spec)
      `(fdefinition-location ',function-spec)
    (ferror 'sys:unknown-locf-reference
	    "Cannot SETF a form (FUNCTION x) unless x is a function spec.")))

))

; From file LTOP.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN LISP-REINITIALIZE (&OPTIONAL (CALLED-BY-USER T)
			  &AUX (COLD-BOOT COLD-BOOTING)
			  MUST-ENABLE-TRAPPING)
  ;; Needed until this file recompiled in a world with 25-bit pointers.
  (SET 'A-MEMORY-VIRTUAL-ADDRESS (LSH (ASH A-MEMORY-VIRTUAL-ADDRESS -3) 3))
  (SET 'IO-SPACE-VIRTUAL-ADDRESS (LSH (ASH IO-SPACE-VIRTUAL-ADDRESS -3) 3))
  (SET 'UNIBUS-VIRTUAL-ADDRESS (LSH (ASH UNIBUS-VIRTUAL-ADDRESS -3) 3))

  (SETQ INHIBIT-SCHEDULING-FLAG T)		;In case called by the user
  (SETQ ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)
  ;; If these are set wrong, all sorts of things don't work.
  (SETQ LOCAL-DECLARATIONS NIL FILE-LOCAL-DECLARATIONS NIL
	UNDO-DECLARATIONS-FLAG NIL COMPILER:QC-FILE-IN-PROGRESS NIL)
  ;; Set interpreter state to Zetalisp, top level.
  (SETQ INTERPRETER-ENVIRONMENT NIL INTERPRETER-FUNCTION-ENVIRONMENT T)
  ;; Provide ucode with space to keep EVCPs stuck into a-memory locations
  ;; by closure-binding the variables that forward there.
  (OR (AND (BOUNDP 'AMEM-EVCP-VECTOR) AMEM-EVCP-VECTOR)
      (SETQ AMEM-EVCP-VECTOR
	    (MAKE-ARRAY (+ (LENGTH SYS:A-MEMORY-LOCATION-NAMES) 40 20)  ;20 in case ucode grows.
			':AREA PERMANENT-STORAGE-AREA)))
  (COND ((NOT CALLED-BY-USER)
	 (AND (FBOUNDP 'COMPILER:MA-RESET) ;Unload microcompiled defs, because they are gone!
	      (COMPILER:MA-RESET))	 ; Hopefully manage to do this before any gets called.
	 ;; Set up the TV sync program as soon as possible; until it is set up
	 ;; read references to the TV buffer can get NXM errors which cause a
	 ;; main-memory parity error halt.  Who-line updating can do this.
	 (COND ((BOUNDP 'TV:DEFAULT-SCREEN)
		(COND ((BOUNDP 'TV:SYNC-RAM-CONTENTS)
		       ;;if TV:SET-TV-SPEED has been done in this image,
		       ;;use the results from that.
		       (SETUP-CPT TV:SYNC-RAM-CONTENTS NIL T))
		      (T (SETUP-CPT)))
		(COND ((VARIABLE-BOUNDP TV:MAIN-SCREEN)
		       (SETQ %DISK-RUN-LIGHT
			     (+ (- (* TV:MAIN-SCREEN-HEIGHT
				      TV:(SHEET-LOCATIONS-PER-LINE MAIN-SCREEN))
				   15)
				(TV:SCREEN-BUFFER TV:MAIN-SCREEN)))))
		TV:(SETQ WHO-LINE-RUN-LIGHT-LOC (+ 2 (LOGAND %DISK-RUN-LIGHT 777777)))))
	 ;; Clear all the bits of the main screen after a cold boot.
	 (AND COLD-BOOT (CLEAR-SCREEN-BUFFER IO-SPACE-VIRTUAL-ADDRESS))))
  ;; Do something at least if errors occur during loading
  (OR (FBOUNDP 'FERROR) (FSET 'FERROR #'FERROR-COLD-LOAD))
  (OR (FBOUNDP 'CERROR) (FSET 'CERROR #'CERROR-COLD-LOAD))
  (OR (FBOUNDP 'UNENCAPSULATE-FUNCTION-SPEC)
      (FSET 'UNENCAPSULATE-FUNCTION-SPEC #'(LAMBDA (X) X)))
  (OR (FBOUNDP 'FS:MAKE-PATHNAME-INTERNAL) (FSET 'FS:MAKE-PATHNAME-INTERNAL #'LIST))
  (OR (FBOUNDP 'FS:MAKE-FASLOAD-PATHNAME) (FSET 'FS:MAKE-FASLOAD-PATHNAME #'LIST))
  ;; Allow streams to work before WHOLIN loaded
  (OR (BOUNDP 'TV:WHO-LINE-FILE-STATE-SHEET)
      (SETQ TV:WHO-LINE-FILE-STATE-SHEET #'(LAMBDA (&REST IGNORE) NIL)))
  (UNCLOSUREBIND '(* ** *** + ++ +++ // //// ////// *VALUES* *DEFAULT-COMMON-LISP*))
  (SETQ DEFAULT-CONS-AREA WORKING-STORAGE-AREA)	;Reset default areas.
  (UNCLOSUREBIND '(READ-AREA))
  (SETQ READ-AREA NIL)
  (NUMBER-GC-ON)	;This seems to work now, make it the default
  (SETQ EH:CONDITION-HANDLERS NIL
	EH:CONDITION-DEFAULT-HANDLERS NIL
	EH:CONDITION-RESUME-HANDLERS NIL)
  (UNLESS (VARIABLE-BOUNDP *PACKAGE*)
    (PKG-INITIALIZE))

  (COND ((NOT (BOUNDP 'CURRENT-PROCESS))	;Very first time around
	 (SETQ SCHEDULER-EXISTS NIL
	       CURRENT-PROCESS NIL
	       TV:WHO-LINE-PROCESS NIL
	       TV:LAST-WHO-LINE-PROCESS NIL)
	 (OR (FBOUNDP 'TV:WHO-LINE-RUN-STATE-UPDATE)
	     (FSET 'TV:WHO-LINE-RUN-STATE-UPDATE #'(LAMBDA (&REST IGNORE) NIL)))
	 (KBD-INITIALIZE)))
  (SETQ TV:KBD-LAST-ACTIVITY-TIME (TIME))	; Booting is keyboard activity.
  (INITIALIZE-WIRED-KBD-BUFFER)
  ;now that the "unibus" channel is set up, turn on 60Hz interrupts
  (AND (= PROCESSOR-TYPE-CODE 2)
       (COMPILER:%NUBUS-WRITE #XF8 4 (LOGIOR 40 (COMPILER:%NUBUS-READ #XF8 4))))

  ;;Flush any closure binding forwarding pointers
  ;;left around from a closure we were in when we warm booted.
  (UNCLOSUREBIND '(PRIN1 BASE *NOPOINT FDEFINE-FILE-PATHNAME INHIBIT-FDEFINE-WARNINGS
			 SELF SI:PRINT-READABLY PACKAGE READTABLE
			 EH:ERROR-MESSAGE-HOOK EH:ERROR-DEPTH EH:ERRSET-STATUS))
  (WHEN (VARIABLE-BOUNDP ZWEI:*LOCAL-BOUND-VARIABLES*)
    (UNCLOSUREBIND ZWEI:*LOCAL-BOUND-VARIABLES*))
  (UNCLOSUREBIND '(ZWEI:*LOCAL-VARIABLES* ZWEI:*LOCAL-BOUND-VARIABLES*))
  ;Get the right readtable.
  (OR (VARIABLE-BOUNDP INITIAL-READTABLE)
      (SETQ INITIAL-READTABLE READTABLE
	    READTABLE (COPY-READTABLE READTABLE)
	    STANDARD-READTABLE READTABLE))
  (WHEN (VARIABLE-BOUNDP COMMON-LISP-READTABLE)
    (UNLESS (VARIABLE-BOUNDP INITIAL-COMMON-LISP-READTABLE)
      (SETQ INITIAL-COMMON-LISP-READTABLE COMMON-LISP-READTABLE
	    COMMON-LISP-READTABLE (COPY-READTABLE COMMON-LISP-READTABLE))))

  ;; Initialize the rubout handler.
  (SETQ	RUBOUT-HANDLER NIL TV:RUBOUT-HANDLER-INSIDE NIL)	;We're not in it now

  ;; Initialize the error handler.
  (OR (BOUNDP 'ERROR-STACK-GROUP)
      (SETQ ERROR-STACK-GROUP (MAKE-STACK-GROUP 'ERROR-STACK-GROUP ':SAFE 0)))
  (SETQ %ERROR-HANDLER-STACK-GROUP ERROR-STACK-GROUP)
  (STACK-GROUP-PRESET ERROR-STACK-GROUP 'LISP-ERROR-HANDLER)	;May not be defined yet 
  (SETF (SG-FOOTHOLD-DATA %INITIAL-STACK-GROUP) NIL)	;EH depends on this
  (COND ((AND (FBOUNDP 'LISP-ERROR-HANDLER)
	      (FBOUNDP 'EH:ENABLE-TRAPPING))
	 (IF (NOT (BOUNDP 'EH:ERROR-TABLE))
	     (SETQ MUST-ENABLE-TRAPPING T)
	   ;; Note: if error-table not loaded,
	   ;; we enable trapping after loading it.
	   (FUNCALL ERROR-STACK-GROUP '(INITIALIZE))
	   (EH:ENABLE-TRAPPING))))
  (SETQ EH:ERRSET-STATUS NIL EH:ERROR-MESSAGE-HOOK NIL)	;Turn off possible spurious errset
  (SETQ EH:ERROR-DEPTH 0)

  ;And all kinds of randomness...

  (SETQ TRACE-LEVEL 0)
  (SETQ INSIDE-TRACE NIL)
  (SETQ + NIL * NIL - NIL ;In case of error during first read/eval/print cycle
	// NIL ++ NIL +++ NIL ;or if their values were unprintable or obscene
	** NIL *** NIL)  ;and to get global values in case of break in a non-lisp-listener
  (SETQ //// NIL ////// NIL)
  (SETQ LISP-TOP-LEVEL-INSIDE-EVAL NIL)
  (SETQ %INHIBIT-READ-ONLY NIL)
  (OR (BOUNDP 'PRIN1) (SETQ PRIN1 NIL))
  (SETQ EVALHOOK NIL APPLYHOOK NIL)
  (SETQ IBASE 8 BASE 8 *NOPOINT NIL)
  (SETQ XR-CORRESPONDENCE-FLAG NIL		;Prevent the reader from doing random things
	XR-CORRESPONDENCE NIL)
  (SETQ *RSET T)				;In case any MACLISP programs look at it
  (SETQ FDEFINE-FILE-PATHNAME NIL)
  (SETQ INHIBIT-FDEFINE-WARNINGS NIL)		;Don't get screwed by warm boot
  (SETQ SELF-FLAVOR-DECLARATION NIL)
  (SETQ SELF NIL SELF-MAPPING-TABLE NIL)
  (SETQ SI:PRINT-READABLY NIL)
  (SETQ CHAOS:CHAOS-SERVERS-ENABLED NIL)	;Don't allow botherage from networks
  (IF (BOUNDP 'PKG-USER-PACKAGE)		;If package system is present
      (SETQ PACKAGE PKG-USER-PACKAGE))

  ;; The first time, this does top-level SETQ's from the cold-load files
  (OR (BOUNDP 'ORIGINAL-LISP-CRASH-LIST)	;Save it for possible later inspection
      (SETQ ORIGINAL-LISP-CRASH-LIST LISP-CRASH-LIST))
  (MAPC (FUNCTION EVAL) LISP-CRASH-LIST)
  (SETQ LISP-CRASH-LIST NIL)

  ;Reattach IO streams.  Note that TERMINAL-IO will be fixed later to go to a window.
  (OR (BOUNDP 'SYN-TERMINAL-IO) )
  (COND ((NOT CALLED-BY-USER)
	 (UNCLOSUREBIND '(TERMINAL-IO STANDARD-OUTPUT STANDARD-INPUT
			  QUERY-IO TRACE-OUTPUT ERROR-OUTPUT DEBUG-IO))
	 (SETQ TERMINAL-IO     COLD-LOAD-STREAM
	       STANDARD-OUTPUT SYN-TERMINAL-IO
	       STANDARD-INPUT  SYN-TERMINAL-IO
	       QUERY-IO        SYN-TERMINAL-IO
	       DEBUG-IO        SYN-TERMINAL-IO
	       TRACE-OUTPUT    SYN-TERMINAL-IO
	       ERROR-OUTPUT    SYN-TERMINAL-IO)
	 (FUNCALL TERMINAL-IO ':HOME-CURSOR)))

  (SETQ TV:MOUSE-WINDOW NIL)	;This gets looked at before the mouse process is turned on
  (KBD-CONVERT-NEW 1_15.)	;Reset state of shift keys

  (IF (AND (FBOUNDP 'CADR:CLEAR-UNIBUS-MAP)        ;clear valid bits on unibus map.
	   (= PROCESSOR-TYPE-CODE 1)) ; Prevents randomness
      (CADR:CLEAR-UNIBUS-MAP))		; and necessary if sharing Unibus with PDP11.
					; Do this before SYSTEM-INITIALIZATION-LIST to
					; avoid screwwing ETHERNET code.
  ;; These are initializations that have to be done before other initializations
  (INITIALIZATIONS 'SYSTEM-INITIALIZATION-LIST T)
  ;; At this point if the window system is loaded, it is all ready to go
  ;; and the initial Lisp listener has been exposed and selected.  So do
  ;; any future typeout on it.  But if any typeout happened on the cold-load
  ;; stream, leave it there (clobbering the Lisp listener's bits).  This does not
  ;; normally happen, but just in case we do the set-cursorpos below so that
  ;; if anything strange gets typed out it won't get erased.  Note that normally
  ;; we do not do any typeout nor erasing on the cold-load-stream, to avoid bashing
  ;; the bits of whatever window was exposed before a warm boot.
  (COND (CALLED-BY-USER)
	((FBOUNDP 'TV:WINDOW-INITIALIZE)
	 (MULTIPLE-VALUE-BIND (X Y) (FUNCALL TERMINAL-IO ':READ-CURSORPOS)
	   (FUNCALL TV:INITIAL-LISP-LISTENER ':SET-CURSORPOS X Y))
	 (SETQ TERMINAL-IO TV:INITIAL-LISP-LISTENER)
	 (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':SET-PACKAGE PACKAGE)
	 (FUNCALL TERMINAL-IO ':FRESH-LINE))
	(T (SETQ TV:INITIAL-LISP-LISTENER NIL)	;Not created yet
	   (FUNCALL TERMINAL-IO ':CLEAR-EOL)))

  (AND CURRENT-PROCESS
       (FUNCALL CURRENT-PROCESS ':RUN-REASON 'LISP-INITIALIZE))

  ; prevent screw from things being traced during initialization
  (if (fboundp 'untrace) (untrace))
  (if (fboundp 'breakon) (unbreakon))

  (INITIALIZATIONS 'COLD-INITIALIZATION-LIST)

  (INITIALIZATIONS 'WARM-INITIALIZATION-LIST T)

  (COND ((AND MUST-ENABLE-TRAPPING
	      (BOUNDP 'EH:ERROR-TABLE))
	 ;; Note: this was done here if we just loaded the error table for the first time.
	 (FUNCALL ERROR-STACK-GROUP '(INITIALIZE)) ; Initialize co-routining.
	 (EH:ENABLE-TRAPPING)))

  (SETQ COLD-BOOTING NIL)

  (IF (FBOUNDP 'PRINT-HERALD)
      (PRINT-HERALD)
    (FUNCALL STANDARD-OUTPUT ':CLEAR-EOL)
    (PRINC "Lisp Machine cold load environment, beware!"))

  (AND (BOUNDP 'TIME:*LAST-TIME-UPDATE-TIME*)
       (NULL (CAR COLD-BOOT-HISTORY))
       (SETF (CAR COLD-BOOT-HISTORY) (CATCH-ERROR (LIST SI:LOCAL-HOST
							(GET-UNIVERSAL-TIME)))))

  ;; This process no longer needs to be able to run except for the usual reasons.
  ;; The delayed-restart processes may now be allowed to run
  (COND (CURRENT-PROCESS
	 (FUNCALL CURRENT-PROCESS ':REVOKE-RUN-REASON 'LISP-INITIALIZE)
	 (WHEN WARM-BOOTED-PROCESS
	   (FORMAT T "Warm boot while running ~S.
Its variable bindings remain in effect;
its unwind-protects have been lost.~%" WARM-BOOTED-PROCESS)
	   (WHEN (NOT (OR (EQ (PROCESS-WARM-BOOT-ACTION WARM-BOOTED-PROCESS)
			      'PROCESS-WARM-BOOT-RESTART)
			  (EQ WARM-BOOTED-PROCESS INITIAL-PROCESS)
			  (TYPEP WARM-BOOTED-PROCESS 'SI:SIMPLE-PROCESS)))
	     (IF (YES-OR-NO-P "Reset it?  Answer No if you want to debug it.  ")
		 (RESET-WARM-BOOTED-PROCESS)
	       (FORMAT T "~&Do (SI:DEBUG-WARM-BOOTED-PROCESS) to examine it, or do
/(SI:RESET-WARM-BOOTED-PROCESS) to reset it and let it run again.~%
If you examine it, you will see a state that is not quite the latest one."))))
	 (LOOP FOR (P . RR) IN DELAYED-RESTART-PROCESSES
	       DO (WITHOUT-INTERRUPTS
		    (SETF (PROCESS-RUN-REASONS P) RR)
		    (PROCESS-CONSIDER-RUNNABILITY P)))
	 (SETQ DELAYED-RESTART-PROCESSES NIL)))

  ;; The global value of TERMINAL-IO is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (COND ((AND (NOT CALLED-BY-USER)
	      (FBOUNDP TV:DEFAULT-BACKGROUND-STREAM))
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM)))

  ;; Now that -all- initialization has been completed, allow network servers
  (SETQ CHAOS:CHAOS-SERVERS-ENABLED T))

))
