;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.18
;;; Reason:
;;;  tv:{100,10,9,8,7,6,5.5,0}%-gray Yow!
;;;  Some history-yanking bugs
;;;  Zmacs doc for c-m-x commands
;;;  cl:member bug!
;;;  zwei::revert-file-buffer not activating always
;;;    -- problems when reverting multiple files in multiple zmacs processes
;;;  Better readtable defaulting for lisp mode
;;;  letf, letf*
;;;  More inspector improvements (robustness, closures)
;;;  compile-file accepts keyword :load
;;;  Sheet :clear-rest-of-line got left out of 99
;;;  fixes to char-bit, set-char-bit, code-char, digit-char
;;;  compiler optimization for load-byte, deposit-byte, ^, char-bit, set-char-bit
;;;  compiler::warn => compiler::compiler-warn
;;;  chaos rename broken by mly spazz
;;;  more m-x add patch verbosity
;;;  sys:fef-debugging-info, sys:fef-debugging-info-present-p
;;;  interpreter tagbody bug when lexiclosure created
;;;  fquery :tyi inside rubout-handler
;;; Written 28-Jan-85 00:45:14 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Experimental System 99.13, CADR 4.0, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.



(eval-when (eval compile load)

(let (x)
  (setq x (intern "LETF" 'si))
  (globalize x 'global) (globalize x 'lisp)
  (setq x (intern "LETF*" 'si))
  (globalize x 'global) (globalize x 'lisp))
(globalize (intern "FEF-DEBUGGING-INFO" 'si) 'sys)
(export (intern "100%-GRAY" 'tv) 'tv)
(export (intern "10%-GRAY" 'tv) 'tv)
(export (intern "9%-GRAY" 'tv) 'tv)
(export (intern "8%-GRAY" 'tv) 'tv)
(export (intern "7%-GRAY" 'tv) 'tv)
(export (intern "6%-GRAY" 'tv) 'tv)
(export (intern "5.5%-GRAY" 'tv) 'tv)

; From file QCDEFS.LISP KANSAS:<L.SYS> OZ: (152)
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCDEFS  "

(defmacro defoptimizer (optimizer-name function-to-optimize
			&optional #|| ((&rest optimizes-into)) ||# arglist &body body)
  "(defoptimizer foo-optimizer foo (form)
     (if (eq (cadr form) 'foo)
         `(and (optfoo . ,(cadr form))
               (optfoo2 . (caddr form)))
        form))
OR
/(defoptimizer foo-optimizer foo)"
  (if (null arglist)
      `(add-optimizer-internal ',function-to-optimize ',optimizer-name
			       nil #||',optimizes-into||#)
    `(progn (add-optimizer-internal ',function-to-optimize ',optimizer-name
				    nil #||',optimizes-into||#)
	    (defun ,optimizer-name ,arglist
	      (declare (function-parent ,optimizer-name defoptimizer))
	      . ,body))))

;; until there really is such a thing, this is just sylistic
(defmacro defrewrite (rewriter function-to-rewrite
		      #|| &optional ((&rest rewrites-into)) ||# arglist &body body)
  `(progn (add-optimizer-internal ',function-to-rewrite ',rewriter
				  nil #||',rewrites-into||#)
	  (defun ,rewriter ,arglist
	      (declare (function-parent ,rewriter defrewrite))
	      . ,body)))
	    
;; this is 
(defmacro defcompiler-synonym (function synonym-function)
  "Make the compiler substitute SYNONYM-FUNCTION for FUNCTION when compiling.
eg (defcompiler-synonym plus +)"
  `(defrewrite ,(intern (string-append function "-TO-" synonym-function)) ,function
	       #|| (,synonym-function) ||# (form)
     (cons ',synonym-function (cdr form))))

))

; From file SCRMAN.LISP KANSAS:<L.WINDOW> OZ: (166)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRMAN  "

(defun make-gray (&rest rows)
  "Return a BITBLT array containing a specified gray pattern.
The pattern's size is HEIGHT by WIDTH, and ROWS is a list of bit-arrays
each of which describes one row of the pattern
The array's first dimension will be some multiple of WIDTH
that is suitable for a BITBLT array."
  (let* ((height (length rows))
	 (width (length (car rows)))
	 (rwidth (lcm width 32.))
	 grey)
    (dolist (r (cdr rows))
      (unless (= width (length r))
	(ferror nil "All rows of grey array must be same length")))
    (setq grey (make-pixel-array rwidth height :type art-1b))
    (do ((r rows (cdr r))
	 (y 0 (1+ y)))
	((null r))
      (dotimes (x rwidth)
	(setf (ar-2-reverse grey x y) (bit (car r) (mod x width)))))
    grey))
(eval-when (eval compile)
  (compile 'make-gray))

; From file SCRMAN.LISP KANSAS:<L.WINDOW> OZ: (166)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRMAN  "

(defmacro screen-manager-bit-arrays (screen)
  `(getf (screen-property-list ,screen) 'screen-manager-bit-arrays))

(defmacro using-screen-manager-bit-array ((var sheet) &body body)
  `(let ((.screen. (sheet-screen ,sheet))
	 ,var)
     (unwind-protect
	 (progn
	   (setq ,var (or (without-interrupts (pop (screen-manager-bit-arrays .screen.)))
			  (make-sheet-bit-array
			    .screen. (sheet-width .screen.) (sheet-height .screen.))))
	   . ,body)
       (and ,var
	    (without-interrupts (push ,var (screen-manager-bit-arrays .screen.)))))))

))

)))


; From file SCRMAN.LISP KANSAS:<L.WINDOW> OZ: (166)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRMAN  "

(DEFUN SCREEN-MANAGE-RESTORE-AREA (RECTANGLE-LIST TO-ARRAY X Y ALU &OPTIONAL CLEAR-AREA)
  "Restore contents of rectangles in RECTANGLE-LIST to TO-ARRAY with offsets X and Y.
ALU is the alu-function used in BITBLTing.
CLEAR-AREA non-NIL says just use zeros if no data is remembered;
otherwise, regenerate the contents of SELF and use them.
Return RECTANGLE-LIST sans the rectangles that applied to this window."
  (DECLARE (:SELF-FLAVOR SHEET))
  (COND (BIT-ARRAY
	 (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTANGLE-LIST
						    BIT-ARRAY
						    TO-ARRAY X Y
						    T (OR ALU ALU-SETA)))
	(CLEAR-AREA
	 (SCREEN-MANAGE-CLEAR-AREA RECTANGLE-LIST TO-ARRAY X Y ALU))
	(T
	 ;; If no saved bits, Refresh into a temporary array and use that as the bits
	 (UNWIND-PROTECT
	     (using-screen-manager-bit-array (array self)
	       (SETQ SCREEN-ARRAY ARRAY)
	       (PAGE-IN-PIXEL-ARRAY ARRAY NIL (LIST WIDTH HEIGHT))
	       (SHEET-FORCE-ACCESS (SELF T)
		 (SEND SELF :REFRESH))
	       (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTANGLE-LIST
							  ARRAY
							  TO-ARRAY X Y
							  NIL (OR ALU ALU-SETA)))
;>>>
	   ;; If autoexposured, deexpose inferiors again
	   (SETQ SCREEN-ARRAY NIL)
	   (WITHOUT-SCREEN-MANAGEMENT
	     (DOLIST (I EXPOSED-INFERIORS)
	       (SEND I :DEEXPOSE :DEFAULT :NOOP NIL)))))))

))

; From file SCRMAN.LISP KANSAS:<L.WINDOW> OZ: (166)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRMAN  "

(defvar 10%-gray #.(make-gray #*1000000000
			      #*0001000000
			      #*0000001000
			      #*0000000001
			      #*0010000000
			      #*0000010000
			      #*0000000010
			      #*0100000000
			      #*0000100000
			      #*0000000100))
(defvar 9%-gray #.(make-gray #*10000000000
			     #*00000001000
			     #*00010000000
			     #*00000000001
			     #*00000010000
			     #*00100000000
			     #*00000000010
			     #*00000100000
			     #*01000000000
			     #*00000000100
			     #*00001000000))
(defvar 8%-gray #.(make-gray #*100000000000
			     #*000010000000
			     #*000000001000))
(defvar 7%-gray #.(make-gray #*10000000000000
			     #*00001000000000
			     #*00000000100000
			     #*00000000000010
			     #*00100000000000
			     #*00000010000000
			     #*00000000001000))
(defvar 6%-gray #.(make-gray #*1000000000000000
			     #*0000001000000000
			     #*0000000000001000
			     #*0010000000000000
			     #*0000000010000000
			     #*0000000000000010
			     #*0000100000000000
			     #*0000000000100000))
(defvar 5.5%-gray #.(make-gray #*100000000000000000
			       #*000001000000000000
			       #*000000000010000000
			       #*000000000000000100
			       #*001000000000000000
			       #*000000010000000000
			       #*000000000000100000
			       #*000000000000000001
			       #*000010000000000000
			       #*000000000100000000
			       #*000000000000001000
			       #*010000000000000000
			       #*000000100000000000
			       #*000000000001000000
			       #*000000000000000010
			       #*000100000000000000
			       #*000000001000000000
			       #*000000000000010000))

(defvar 100%-gray #.(make-gray #*1))
(defvar 0%-gray #.(make-gray #*0))

))

; From file HISTORY.LISP KANSAS:<L.ZWEI> OZ: (16)
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HISTORY  "

(DEFUN YANK-AS-TEXT (THING &OPTIONAL
		     (KILL-PREVIOUS (TYPEP *LAST-COMMAND-TYPE* 'HISTORY))
		     LEAVE-POINT-BEFORE)
  "Yank THING into the buffer at point, moving point after it, leaving mark before it.
KILL-PREVIOUS non-NIL means delete the existing contents of the region first;
 this defaults to T if the previous command was a yank command.
LEAVE-POINT-BEFORE non-NIL means put point before the text and mark after.
 This happens anyway if KILL-PREVIOUS is non-NIL and point was
 at the front of the existing region.
An interval that is not a buffer is yanked by inserting its text.
A buffer is yanked by inserting its name!
A string is yanked as its text.
A symbol or list is yanked as it would print, with slashification."
  (IF KILL-PREVIOUS
      (LET ((BP1 (POINT)) (BP2 (MARK)))
	(GET-INTERVAL BP1 BP2 NIL)
	(SETQ LEAVE-POINT-BEFORE (BP-= BP1 (POINT)))
	(LET ((*BATCH-UNDO-SAVE* T)
	      (UNDO-ITEM (CAR (UNDO-STATUS-UNDO-LIST
				(NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*))))))
	  (WHEN UNDO-ITEM
	    ;; Don't use WITH-UNDO-SAVE.  Instead, "re-open" the undo save for the COM-YANK,
	    ;; so that any sequence C-Y M-Y M-Y ... is undone as a unit.
	    (SETF (BP-STATUS (UNDO-ITEM-START-BP UNDO-ITEM)) ':NORMAL)
	    (SETF (BP-STATUS (UNDO-ITEM-END-BP UNDO-ITEM)) ':MOVES))
	  (UNWIND-PROTECT
	      (PROGN
		(DELETE-INTERVAL BP1 BP2 T)
		(MOVE-BP (MARK) BP1)
		(WHEN THING
		  (MOVE-BP (POINT) (INSERT-KILL-RING-THING (MARK) THING))))
	    (UNDO-SAVE-END))))
    (WITH-UNDO-SAVE ("Yank" (POINT) (POINT) T)
      (MOVE-BP (MARK) (POINT))
      (MOVE-BP (POINT) (INSERT-KILL-RING-THING (POINT) THING))))
  (IF LEAVE-POINT-BEFORE (SWAP-BPS (POINT) (MARK))))

))

; From file HISTORY.LISP KANSAS:<L.ZWEI> OZ: (17)
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; HISTORY  "

(DEFUN PUSH-REMOVE-ON-HISTORY (ELEMENT HISTORY)
  "Add ELEMENT to the front of HISTORY, removing it if it appears later on.
Also sets HISTORY's yank-pointer to the front."
  (SETF (HISTORY-LIST HISTORY)
	(CONS ELEMENT (DELQ ELEMENT (HISTORY-LIST HISTORY))))
  (SETF (HISTORY-LENGTH HISTORY) (LENGTH (HISTORY-LIST HISTORY)))
  (SETF (HISTORY-YANK-POINTER HISTORY) 1)
  ELEMENT)

(DEFUN APPEND-REMOVE-ON-HISTORY (ELEMENT HISTORY)
  "Append ELEMENT to the end of HISTORY, removing it if it appears earlier.
Does not affect HISTORY's yank-pointer."
  (SETF (HISTORY-LIST HISTORY)
	(NCONC (DELQ ELEMENT (HISTORY-LIST HISTORY)) (NCONS ELEMENT)))
  (SETF (HISTORY-LENGTH HISTORY) (LENGTH (HISTORY-LIST HISTORY)))
  ELEMENT)
))

; From file DOC.LISP KANSAS:<L.ZWEI> OZ: (75)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFCOM COM-DOCUMENTATION "Run a specified documentation command.
You type a character.  To find out what a certain character does, type C and that character.
To find out what a named command does, type D and the command name.
To find all commands whose names contain a certain substring,
  type A and then the substring.
To find out the last 60 characters you typed, if you are confused, type L.
More advanced options:
   U - Undo; V - run Variable Apropos; W - run Where Is;
   SPACE repeats the previous documentation request, if any." ()
  (DO ((CHAR 0)
       (*IN-COM-DOC-P* T)
       (*REPEAT-DOC-P* NIL))
      (NIL)
    ;; Print a list of available options.
    (FORMAT *QUERY-IO* "~&Help.  Options are ")
    (DOLIST (ELT *COM-DOCUMENTATION-ALIST*)
      (IF (OR (NULL (THIRD ELT))
	      (EVAL (THIRD ELT)))
	  (FORMAT *QUERY-IO* "~C," (CAR ELT))))
    (FORMAT *QUERY-IO* "~\lozenged-character\,~\lozenged-character\: " #/space #/help)
    ;; Read input chars till we get a valid one.
    (TYPEIN-LINE-ACTIVATE
      (SETQ CHAR (DO ((CHAR (CHAR-UPCASE (READ-CHAR *STANDARD-INPUT*))
			    (CHAR-UPCASE (READ-CHAR *STANDARD-INPUT*))))
		     ((OR (MEMQ CHAR '(#/SP #/HELP))
;character lossage
			  (ASS #'CHAR= CHAR *COM-DOCUMENTATION-ALIST*))
		      CHAR)
		   (WHEN (MEMQ CHAR '(#/C-G #/RUBOUT))
		     (SEND *QUERY-IO* :MAKE-COMPLETE)
		     (BARF))
		   (BEEP))))
    (SEND *QUERY-IO* :MAKE-COMPLETE)
    ;; Execute the character we got.
    (COND ((CHAR= CHAR #/SPACE)
	   (SETQ *REPEAT-DOC-P* T)
	   (SETQ CHAR *COM-DOC-LAST-CHAR*))
	  (T (SETQ *COM-DOC-LAST-CHAR* CHAR)))
    (IF (MEMQ CHAR '(#/? #/HELP))
	(PROGN (FORMAT T "You have entered the Documentation command.~%")
	       (PRINT-DOC :FULL 'COM-DOCUMENTATION *LAST-COMMAND-CHAR*))
      (LET ((FUNCTION (CADR (OR (ASSQ CHAR *COM-DOCUMENTATION-ALIST*)
;character lossage
				(ASSQ (CHAR-INT CHAR) *COM-DOCUMENTATION-ALIST*)))))
	(AND FUNCTION (RETURN (FUNCALL FUNCTION)))))))

))

; From file DOC.LISP KANSAS:<L.ZWEI> OZ: (75)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFINE-COMMAND-DOCUMENTATION COM-DOCUMENTATION ()
  (IF (NEQ OP :FULL)
      (IF (EQ OP :NAME)
	  "Documentation"
	(PRINT-DOC OP COM CHAR T))
    (FORMAT T "This prints various sorts of editor documentation.
You type an option -- one character -- saying what kind of documentation you want.
For some options, you then type more; for example, which command to document.

Here are the options available now:~%")
    (DOLIST (ELT *COM-DOCUMENTATION-ALIST*)
      (IF (OR (NULL (THIRD ELT)) (EVAL (THIRD ELT)))
	  (PROGN
	    (FORMAT T "~C -- " (CAR ELT))
	    (PRINT-DOC :SHORT (CADR ELT)))))
    (FORMAT T "~%In addition, ~\lozenged-character\ repeats the previous Help request."
	    #/SPACE)))

))

; From file DOC.LISP KANSAS:<L.ZWEI> OZ: (75)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DOC  "

(DEFUN PRINT-SHORT-DOC-FOR-TABLE (CHAR COMTAB INDENTATION)
  "Document what CHAR does in COMTAB, for subcommands of prefix characters.
It prints one or two lines of stuff, with the given INDENTATION."
  (LET ((X (COMMAND-LOOKUP CHAR COMTAB T)))
    (COND ((MEMQ X '(NIL :UNDEFINED)))		;undefined
	  ((CONSP X))			;alias
	  ((MACRO-COMMAND-P X)
	   (FORMAT T "~&~V@T~:C is a user defined macro.~%" INDENTATION CHAR))
	  ((PREFIX-COMMAND-P X)
	   (FORMAT T "~&~V@T~:C reads another character and dispatches.~%"
		   INDENTATION CHAR))
	  ((NOT (SYMBOLP X)))		;??
	  (T
	   (FORMAT T "~&~V@T~:C is ~A:~%~@T"
		   INDENTATION CHAR (COMMAND-NAME X) (+ 5 INDENTATION))
	   (PRINT-DOC :SHORT X CHAR)))))

))

; From file GENRIC.LISP KANSAS:<L.SYS> OZ: (32)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun cli:member (item list &key test test-not key)
  "Return a tail of LIST whose car is the first element of LIST that matches ITEM.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used.
TEST is a function passed ITEM and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  ;; Use MEMQ whenever that will work, since it is microcoded.
  (block member
    (when (and (null key)
	       (null test-not))
      (cond ((or (eq test 'eq) (eq test #'eq))
	     (return-from member (memq item list)))
	    ((or (null test) (eq test 'eql) (eq test #'eql))
	     (return-from member (sys:member-eql item list)))))
    (do ((tail list (cdr tail)))
	((null tail))
      (let ((elt (if key (funcall key (car tail)) (car tail))))
	(if (cond (test-not (not (funcall test-not item elt)))
		  (test (funcall test item elt))
		  (t (eql item elt)))
	    (return tail))))))

))

; From file OZ:KANSAS:<L.ZWEI>METH.LISP.48 at 30-Jan-1985 03:57:19
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (FILE-BUFFER :AFTER :SET-ATTRIBUTE) (ATTRIBUTE VALUE &OPTIONAL SET-TEXT-TOO)
  (LET ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM SELF))))
    (AND (NOT (EQUAL VALUE (GETF ATTRIBUTES ATTRIBUTE)))
	 ;; Ok, the new value doesn't match what's in the text.
	 (OR (EQ SET-TEXT-TOO T)
	     (AND (EQ SET-TEXT-TOO ':QUERY)
		  (FQUERY NIL "Change the -*- line of the text as well? ")))
	 (PROGN
	   ;; Put the new value in with what we got from the text;
	   ;; if the new value is the default, delete it instead.
	   (IF (OR (MEMQ ATTRIBUTE '(:BASE :MODE :PACKAGE :SYNTAX :READTABLE))
		   (NOT (EQUAL VALUE (EVAL (GET ATTRIBUTE 'DEFAULT-ATTRIBUTE-VALUE)))))
	       (SETF (GETF ATTRIBUTES ATTRIBUTE) VALUE)
	     (REMF ATTRIBUTES ATTRIBUTE)
	     ;; Cause Update Attribute List to forget this one too.
	     (REMF (GETF SI:PROPERTY-LIST 'FS::LAST-FILE-PLIST) ATTRIBUTE))
	   ;; Now we have an attribute list to store in the file.
	   (STORE-ATTRIBUTE-LIST SELF ATTRIBUTES)
	   (MUST-REDISPLAY-BUFFER SELF DIS-TEXT)))))

))

; From file OZ:KANSAS:<L.ZWEI>METH.LISP.48 at 30-Jan-1985 04:32:27
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :TYO) (CH)
  (IF (CHARACTERP CH) (SETQ CH (CHAR-INT CH)))
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (COND ((EQ *FONT-FLAG* T)
	   ;; Character after a ^F.
	   (SETQ *FONT-FLAG* NIL)
	   (COND ((= CH (CHAR-INT #/))
		  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*)
				    (IN-CURRENT-FONT CH **FONT**))))
		    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
		 ((= CH (CHAR-INT #/#))
		  (SETQ *FONT-FLAG* 'DIAG-1))
		 ((= CH (CHAR-INT #/*))
		  (OR (ZEROP (ARRAY-LEADER *FONT-STACK* 0))
		      (SETQ **FONT** (VECTOR-POP *FONT-STACK*))))
		 (T 
		  (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
		  (SETQ **FONT** (- CH #/0)))))
	  ((NULL *FONT-FLAG*)
	   ;; Character in normal text state.
	   (COND ((= CH (CHAR-INT #/))
		  (SETQ *FONT-FLAG* T))
		 (T
		  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*)
				    (IN-CURRENT-FONT CH **FONT**))))
		    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))))
	  ((EQ *FONT-FLAG* 'DIAG-1)
	   ;; Character after a ^F#
	   (SETQ *FONT-FLAG* 'DIAG-2 *STOP-INDEX* 0))
	  ((EQ *FONT-FLAG* 'DIAG-2)
	   (IF (= CH (CHAR-INT #/SPACE))
	       (SETQ *FONT-FLAG* (MAKE-STRING 10. :FILL-POINTER 0))
	     (SETQ *STOP-INDEX* (+ (* *STOP-INDEX* 10.) (- CH #/0)))))
	  ((STRINGP *FONT-FLAG*)
	   (IF (= CH (CHAR-INT #/NEWLINE))
	       (SETQ *INDEX* NIL
		     *FONT-FLAG* (MAKE-INSTANCE (READ-FROM-STRING *FONT-FLAG*)
						:NUMBER-OF-LINES *STOP-INDEX*))
	     (VECTOR-PUSH-EXTEND CH *FONT-FLAG*)))
	  ((TYPEP *FONT-FLAG* 'RESTORABLE-LINE-DIAGRAM-MIXIN)
	   (BLOCK NIL
	     (OR *INDEX*
		 (COND ((< (SETQ *STOP-INDEX* (1- *STOP-INDEX*)) 0)
			(SETQ *INDEX* 0 *FONT-FLAG* (= CH (CHAR-INT #/)))
			(RETURN NIL))
		       (T
			(SETQ *INDEX* (CREATE-LINE ART-STRING 0 NIL))
			(INSERT-LINE-WITH-LEADER *INDEX* *LINE*))))
	     (COND ((= CH (CHAR-INT #/NEWLINE))
		    (SETF (GETF (LINE-PLIST *INDEX*) ':DIAGRAM) *FONT-FLAG*)
		    (SEND *FONT-FLAG* :ADD-LINE *INDEX* *INDEX*)
		    (SETF (LINE-LENGTH *INDEX*) 0)
		    (SETQ *INDEX* NIL))
		   (T
		    (VECTOR-PUSH-EXTEND CH *INDEX*)))))
	  (T (FERROR NIL "~S has a value not understood here" '*FONT-FLAG*)))))

))

; From file OZ:KANSAS:<L.ZWEI>METH.LISP.48 at 30-Jan-1985 04:43:45
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :UNTYI) (CH)
  (IF (CHARACTERP CH) (SETQ CH (CHAR-INT CH)))
  (IF (ZEROP *INDEX*)
      (SETQ *LINE* (LINE-PREVIOUS *LINE*) *INDEX* (LINE-LENGTH *LINE*))
    (IF (CHAR= (IN-CURRENT-FONT CH **FONT**) (CHAR *LINE* (1- *INDEX*)))
	(DECF *INDEX*)
      (SETQ *FONT-FLAG* CH))))

))

; From file OZ:KANSAS:<L.ZWEI>METH.LISP.48 at 30-Jan-1985 04:45:34
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (INTERVAL-STREAM :ELEMENT-TYPE) ()
  'STRING-CHAR)

))

; From file OZ:KANSAS:<L.ZWEI>ZMACS.LISP.519 at 30-Jan-1985 04:55:05
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFUN REVERT-FILE-BUFFER (BUFFER PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG
			   &AUX GENERIC-PATHNAME PATHNAME-STRING TRUENAME NEW-MODE)
  (WHEN (AND (NULL (BUFFER-FILE-ID BUFFER)) (NULL PATHNAME))
    (BARF "The buffer ~A is not associated with a file." (BUFFER-NAME BUFFER)))
  (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
    (EDITOR-FILE-NAME PATHNAME))
  (COND (CONNECT-FLAG
	 (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
	 (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)))
  (SETQ GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
  (WITH-OPEN-FILE-CASE (STREAM PATHNAME)
    (:NO-ERROR
     (SETQ TRUENAME (SEND STREAM :TRUENAME))
     (WHEN (MEMQ (SEND PATHNAME :TYPE) '(NIL :UNSPECIFIC))
       (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
	 (EDITOR-FILE-NAME
	   (IF (EQUALP (SEND TRUENAME :NAME) (SEND PATHNAME :NAME))
	       ;; This is in case user reads FOO > from an ITS, and it is reall FOO BAR.
	       (SEND PATHNAME :NEW-TYPE (SEND TRUENAME :TYPE))
	     ;; This case if user read FOO BAR from an LMFILE, and truename is FOO|BAR.
	     ;; Or if user reads FOO BAR from an ITS and it is a link to UGH QUUX.
	     PATHNAME))))
     (WHEN CONNECT-FLAG
       (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
       (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
       (SIMILAR-BUFFER-FILES-WARNING BUFFER))
     (OR QUIETLY-FLAG (FORMAT *QUERY-IO* "~&Reading ~A" TRUENAME))
     (LET ((THIS-VERSION (SEND TRUENAME :VERSION))
	   (INSTALLED-TRUENAME (FILE-LOADED-TRUENAME TRUENAME))
	   INSTALLED-VERSION)
       (AND INSTALLED-TRUENAME
	    (NUMBERP THIS-VERSION)
	    (NUMBERP (SETQ INSTALLED-VERSION (SEND INSTALLED-TRUENAME :VERSION)))
	    (NOT QUIETLY-FLAG)
	    ( INSTALLED-VERSION THIS-VERSION)
	    (FORMAT *QUERY-IO* " (installed version is ~D)" INSTALLED-VERSION)))
     (FS:READ-ATTRIBUTE-LIST BUFFER STREAM)
     ;; Forget (and thereby override) and previouse Set Package in this buffer.
     (SETF (BUFFER-PACKAGE BUFFER) NIL)
     ;; And recompute from latest attribute list.
     (INITIALIZE-BUFFER-PACKAGE BUFFER)
     (UNLESS (SEND BUFFER :GET-ATTRIBUTE :MODE)
       (SEND BUFFER :SET-ATTRIBUTE :MODE
				   (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
							    FS:*FILE-TYPE-MODE-ALIST*))
				       *DEFAULT-MAJOR-MODE*)))
     (SETQ NEW-MODE (OR (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE :MODE))
			'FUNDAMENTAL-MODE))
     (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
       (IF (EQ BUFFER *INTERVAL*)
	   (COMPUTE-BUFFER-PACKAGE BUFFER))
       (AND NEW-MODE (SEND BUFFER :SET-MAJOR-MODE NEW-MODE)))
     (PRESERVE-BUFFER-POINT (BUFFER)
       (WITH-READ-ONLY-SUPPRESSED (BUFFER)
	 (LET ((*BATCH-UNDO-SAVE* T))		;Don't save all this for undo!
	   (DISCARD-UNDO-INFORMATION BUFFER)
	   (DELETE-INTERVAL BUFFER)
	   (SETF (BUFFER-TICK BUFFER) (TICK))	;For SECTIONIZE-BUFFER
	   (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
	   (LET ((FONTS (SET-BUFFER-FONTS BUFFER))
		 FONTS-P)
	     (SETQ FONTS-P (OR (CDR FONTS) (SEND BUFFER :GET-ATTRIBUTE :DIAGRAM)))
	     (SEND BUFFER :ACTIVATE)
	     (WHEN SELECT-FLAG
	       (MAKE-BUFFER-CURRENT BUFFER)
	       ;; If it is requested, read in the first screenful and then redisplay.
	       (DOTIMES (I (+ 5 (WINDOW-N-PLINES *WINDOW*)))
		 (MULTIPLE-VALUE-BIND (LINE EOFFLG)
		     (SEND STREAM :LINE-IN LINE-LEADER-SIZE)
		   (WHEN LINE
		     (INSERT-LINE-WITH-LEADER LINE
					      (BP-LINE (INTERVAL-LAST-BP BUFFER))))
		   (IF EOFFLG (RETURN))))
	       (REDISPLAY *WINDOW* :START (INTERVAL-FIRST-BP BUFFER) NIL))
	     (IF (NOT CONNECT-FLAG)
		 (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
	       (IF (EQ CONNECT-FLAG 'NOSECTIONIZE)
		   (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
		 (SECTIONIZE-FILE-BUFFER BUFFER *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS
					 NIL NIL
					 STREAM FONTS-P))
	       (SET-BUFFER-FILE-ID BUFFER (SEND STREAM :INFO))
	       (DOLIST (WINDOW (SEND BUFFER :WINDOWS))
		 (AND FONTS
		      (REDEFINE-FONTS WINDOW
				      FONTS (SEND BUFFER :GET-ATTRIBUTE :VSP)))
		 (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
						    (SEND BUFFER :GET-ATTRIBUTE :BACKSPACE))
		 (REDEFINE-WINDOW-TAB-NCHARS WINDOW
					     (SEND BUFFER :GET-ATTRIBUTE :TAB-WIDTH))))
	     (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
	     (NOT-MODIFIED BUFFER)))))
     (UNLESS QUIETLY-FLAG
       (IF (SEND STREAM :OPERATION-HANDLED-P :READ-POINTER)
	   (LET ((NCHARS (SEND STREAM :READ-POINTER)))
	     (IF (< NCHARS 5000.)
		 (FORMAT *QUERY-IO* " -- ~D characters." NCHARS)
	         (FORMAT *QUERY-IO* " -- ~DK characters." (ROUND NCHARS 1024.))))
	 (FORMAT *QUERY-IO* " -- done."))))
    (FS:FILE-NOT-FOUND
     (WHEN *FIND-FILE-NOT-FOUND-IS-AN-ERROR* (BARF STREAM))
     (OR QUIETLY-FLAG (FORMAT *QUERY-IO* "(New File)"))
     (LET ((*BATCH-UNDO-SAVE* T))
       (DISCARD-UNDO-INFORMATION BUFFER)
       (DELETE-INTERVAL BUFFER))
     (AND CONNECT-FLAG (SET-BUFFER-FILE-ID BUFFER T))
     (SEND BUFFER :SET-ATTRIBUTE :MODE
				 (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
							  FS:*FILE-TYPE-MODE-ALIST*))
				     *DEFAULT-MAJOR-MODE*))
     (SETF (BUFFER-PACKAGE BUFFER) (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*)))
     (LET ((MODE (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE :MODE))))
       (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
	 (IF (EQ BUFFER *INTERVAL*) (COMPUTE-BUFFER-PACKAGE BUFFER))
	 (AND MODE (SEND BUFFER :SET-MAJOR-MODE MODE)))))
    (FS:FILE-ERROR (BARF STREAM)))
  (SETF (BUFFER-TICK BUFFER) (TICK)))		;Buffer is same as file

))

; From file OZ:KANSAS:<L.ZWEI>MODES.LISP.138 at 30-Jan-1985 05:20:22
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MODES  "

(DEFCOM COM-PREFIX-META DOCUMENT-PREFIX-CHAR ()
  (PROCESS-PREFIX-COMMAND-CHAR
    (SET-CHAR-BIT (GET-ECHO-CHAR "Meta-" (EQ *LAST-COMMAND-CHAR* #/)) :META T)))

))

; From file OZ:KANSAS:<L.ZWEI>MODES.LISP.139 at 30-Jan-1985 05:42:53
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MODES  "

(DEFMAJOR COM-LISP-MODE LISP-MODE "LISP"
  "Sets things up for editing Lisp code.
Puts Indent-For-Lisp on Tab." ()
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* 'LISP-FIND-COMMENT-START-AND-END)
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-INDENT-FOR-LISP
			      #/RUBOUT COM-TAB-HACKING-RUBOUT
			      #/C-RUBOUT COM-RUBOUT
			      #/M-Z COM-COMPILE-AND-EXIT
			      #/C-M-Z COM-EVALUATE-AND-EXIT
			      #/C-\ COM-JUST-ONE-SPACE)
	      '(("Set Common Lisp" . COM-SET-COMMON-LISP)
		("Set Readtable" . COM-SET-READTABLE)
		;("Set syntax" . com-set-syntax)
		))
  (SETQ *READTABLE* (COMPUTE-BUFFER-READTABLE *INTERVAL*))
  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
		       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
		       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))

))

; From file OZ:KANSAS:<L.ZWEI>ZMNEW.LISP.35 at 30-Jan-1985 05:44:03
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "

(DEFUN COMPUTE-BUFFER-PACKAGE (BUFFER)
  "Set *PACKAGE*, *READ-BASE*, *PRINT-BASE* and *READTABLE*
to the right values for BUFFER."
  (SETQ *PRINT-BASE* (SETQ *READ-BASE* (OR (SEND BUFFER ':GET-ATTRIBUTE ':BASE)
					   *DEFAULT-BASE* *READ-BASE* 8)))
  (SETQ *PACKAGE* (OR (SEND BUFFER ':SEND-IF-HANDLES ':SAVED-PACKAGE)
		      (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*))))
  (SETQ *READTABLE* (COMPUTE-BUFFER-READTABLE BUFFER))
  NIL)

(DEFUN COMPUTE-BUFFER-READTABLE (BUFFER)
  "Return a readtable for use while editing BUFFER"
  (OR (SI:FIND-READTABLE-NAMED (SEND BUFFER :GET-ATTRIBUTE :READTABLE)
			       :FIND)
      (LET* ((DEFAULT '(()))
	     (VALUE (SEND *INTERVAL* :GET-ATTRIBUTE ':SYNTAX DEFAULT)))
	(IF (NEQ VALUE DEFAULT)
	    (SI:FIND-READTABLE-NAMED (SYMBOL-NAME VALUE) ':FIND)))
      *DEFAULT-READTABLE*
      *READTABLE*
      SI:STANDARD-READTABLE))

))

; From file OZ:KANSAS:<L.ZWEI>ZMNEW.LISP.35 at 30-Jan-1985 05:46:58
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ZMNEW  "

(DEFCOM COM-SET-VSP "Set the vertical interline spacing for this buffer.
This is the number of blank rows of pixels between lines of text.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well.
The numeric argument is used as the new value,
or else one is read in the minibuffer.  The default is 2." ()
  (IF *NUMERIC-ARG-P*
      (SEND *INTERVAL* :SET-ATTRIBUTE ':VSP *NUMERIC-ARG* :QUERY)
    (SET-ATTRIBUTE ':VSP "attribute VSP" 2))
  (REDEFINE-FONTS *WINDOW* (WINDOW-FONT-ALIST *WINDOW*)
		  (SEND *INTERVAL* ':GET-ATTRIBUTE ':VSP))
  DIS-NONE)

))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.1 at 30-Jan-1985 06:49:42
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defun pbind (vars-and-vals loc)
  (when vars-and-vals
    `(%bind (,loc ,(caar vars-and-vals))
	   (prog1 ,(cadar vars-and-vals)
		  ,(pbind (cdr vars-and-vals) loc)))))

(defrewrite let-if-expand let-if (form)
  (destructuring-bind (ignore cond vars-and-vals &body body) form
    (cond ((null cond) `(let () . ,body))		;Macros generate this
	  ((eq cond t) `(let ,vars-and-vals . ,body))	;and this
	  (t (multiple-value-bind (body decls)
;>> need to pass environment into extract-declarations here
		 (extract-declarations body local-declarations nil)
	       `(let ()
		  (declare . ,decls)
		  (cond (,cond ,(pbind vars-and-vals 'variable-location)))
		  . ,body))))))

))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.1 at 30-Jan-1985 06:53:42
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defrewrite letf-expand letf (form)
  `(let ()
     ,(pbind (cadr form) 'locf)
     . ,(cddr form)))

(defrewrite letf*-expand letf* (form)
  `(let ()
     ,@(loop for (place value) in (cadr form)
	     collect `(%bind (locf ,place) ,value))
     . ,(cddr form)))

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.90 at 30-Jan-1985 07:08:43
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun letf (&quote places-and-values &rest body)
  "LETF is like LET, except that it it can bind any storage cell
rather than just value cells.
PLACES-AND-VALUES is a list of lists of two elements, the car of each
 of which specifies a location to bind (this should be a form acceptable to LOCF)
 and the cadr the value to which to bind it.
The places are bound in parallel.
Then the body is evaluated sequentially and the values
of the last expression in it are returned.
/(Note that the bindings made by LETF are always /"special/")"
  (prog ((vars-left places-and-values))
     bindloop
   	(when vars-left
	  (%push (eval1 `(locf ,(caar vars-left))))
	  (%push (eval1 (cadar vars-left)))
	  (pop vars-left)
	  (go bindloop))
	(setq vars-left places-and-values)
     bindloop1
	(when vars-left
	  (%bind (%pop) (%pop))
	  (pop vars-left)
	  (go bindloop1))
	(return (eval-body body))))

(defun letf* (&quote places-and-values &rest body)
  "Like LETF except that binding of PLACES-AND-VALUES is done in series."
  (prog ((vars-left places-and-values))
     bindloop
   	(when vars-left
	  (%bind (eval1 `(locf ,(caar vars-left)))
		 (eval1 (cadar vars-left)))
	  (pop vars-left)
	  (go bindloop))

	(return (eval-body body))))

(or (memq 'letf eh::uninteresting-functions)
    (setq eh::uninteresting-functions (list* 'letf 'letf* eh::uninteresting-functions)))

))

; From file INSPCT.LISP KANSAS:<L.WINDOW> OZ: (158)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (BASIC-INSPECT :OBJECT-INSTANCE) (OBJ &AUX (MAXL -1) RESULT FLAVOR)
  (SETQ FLAVOR (SI::INSTANCE-FLAVOR OBJ))
  (OR (TYPEP FLAVOR 'SI:FLAVOR) (SETQ FLAVOR NIL))
  (SETQ RESULT (LIST '("")
		     `("An object of flavor "
		       (:ITEM1 flavor ,flavor ,#'(lambda (f s) (prin1 (si::flavor-name f) s)))
		       ".  Function is "
		       (:ITEM1 FLAVOR-FUNCTION
			       ,(SI::INSTANCE-FUNCTION OBJ)))))
  (LET ((IVARS (IF FLAVOR (SI:FLAVOR-ALL-INSTANCE-VARIABLES FLAVOR)
		   (%P-CONTENTS-OFFSET (%P-CONTENTS-AS-LOCATIVE-OFFSET OBJ 0)
				       %INSTANCE-DESCRIPTOR-BINDINGS))))
    (DO ((BINDINGS IVARS (CDR BINDINGS))
	 (I 1 (1+ I)))
      ((NULL BINDINGS))
      (SETQ MAXL (MAX (FLATSIZE (%FIND-STRUCTURE-HEADER (CAR BINDINGS))) MAXL)))
    (DO ((BINDINGS IVARS (CDR BINDINGS))
	 (SYM)
	 (I 1 (1+ I)))
	((NULL BINDINGS))
      (SETQ SYM (%FIND-STRUCTURE-HEADER (CAR BINDINGS)))
      (PUSH `((:ITEM1 INSTANCE-SLOT ,SYM)
	      (:COLON ,(+ 2 MAXL))
	      ,(IF (= (%P-LDB-OFFSET %%Q-DATA-TYPE OBJ I) DTP-NULL)
		   "void"
		   `(:ITEM1 INSTANCE-VALUE ,(%P-CONTENTS-OFFSET OBJ I))))
	    RESULT)))
  (NREVERSE RESULT))

))

; From file INSPCT.LISP KANSAS:<L.WINDOW> OZ: (158)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (BASIC-INSPECT :OBJECT-LIST) (LIST)
  (MULTIPLE-VALUE-BIND (STRING-LIST ATOMIC-ITEMS LIST-ITEMS)
      (LET ((*PRINT-LENGTH* INSPECT-PRINLENGTH)
	    (*PRINT-LEVEL* INSPECT-PRINLEVEL))
	(condition-bind (((sys:cell-contents-error)
			  #'(lambda (cond)
			      (values :new-value
				      (format nil "#<~S ~O>"
					      (or (q-data-types (send cond :data-type))
						  (send cond :data-type))
					      (%pointer (send cond :address)))))))
	  (GRIND-INTO-LIST LIST (TRUNCATE (SHEET-INSIDE-WIDTH) CHAR-WIDTH) T)))
    ;; Turn STRING-LIST into a list of elements, one for each line, of the form
    ;; (NIL contents-string atom-item-list line-contains-lozenged-characters-p).
    (DO ((L STRING-LIST (CDR L))
	 (AIS ATOMIC-ITEMS (CDR AIS)))
	((NULL L))
      (LET ((LOZENGED-CHARACTERS
	      (DOTIMES (I (STRING-LENGTH (CAR L)))
		(IF ( (CHAR (CAR L) I) #o200)
		    (RETURN T)))))
	;; Convert the start and end indices for each atom-item from characters to pixels.
	;; If this line contains no lozenged characters,
	;; this can be done by multiplying.  Otherwise, SHEET-STRING-LENGTH must be used.
	(DOLIST (I (CAR AIS))
	  (SETF (THIRD I) (+ (SHEET-INSIDE-LEFT)
			     (IF LOZENGED-CHARACTERS
				 (SHEET-STRING-LENGTH SELF (CAR L) 0 (THIRD I))
			       (* (THIRD I) CHAR-WIDTH))))
	  (SETF (FOURTH I) (+ (SHEET-INSIDE-LEFT)
			      (IF LOZENGED-CHARACTERS
				  (SHEET-STRING-LENGTH SELF (CAR L) 0 (FOURTH I))
				(* (FOURTH I) CHAR-WIDTH)))))
	(SETF (CAR L) (LIST NIL (CAR L) (CAR AIS) LOZENGED-CHARACTERS))))
    ;; Convert the starting and ending hpos of each list-item from characters to pixels
    ;; Must find the line which the start or end appears on
    ;; and see whether that line had any lozenged characters
    ;; to decide whether a multiplication is sufficient.
    (DOLIST (I LIST-ITEMS)
      (SETF (SECOND I)
	    (+ (SHEET-INSIDE-LEFT)
	       (LET ((LINE-DESC (NTH (THIRD I) STRING-LIST)))
		 (IF (FOURTH LINE-DESC)
		     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (SECOND I))
		   (* (SECOND I) CHAR-WIDTH)))))
      (SETF (FOURTH I)
	    (+ (SHEET-INSIDE-LEFT)
	       (LET ((LINE-DESC (NTH (FIFTH I) STRING-LIST)))
		 (IF (FOURTH LINE-DESC)
		     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (FOURTH I))
		   (* (FOURTH I) CHAR-WIDTH))))))
    (SETQ LIST-ITEMS (SORT LIST-ITEMS
			   #'(LAMBDA (X Y)
			       (COND ((< (THIRD Y) (THIRD X)) T)
				     ((> (THIRD Y) (THIRD X)) NIL)
				     (T (> (SECOND X) (SECOND Y)))))))
    (DO ((LINE (1- (LENGTH STRING-LIST)) (1- LINE))
	 (CURRENT LIST-ITEMS))
	((< LINE 0))
      (DO ()
	  ((OR (NULL CURRENT)
	       ( (THIRD (CAR CURRENT)) LINE)))
	(SETQ CURRENT (CDR CURRENT)))
      (SETF (CAAR (NTHCDR LINE STRING-LIST)) CURRENT))
    (VALUES STRING-LIST :LIST-STRUCTURE 'INSPECT-LIST-PRINTER)))

))


; From file OZ:KANSAS:<L.WINDOW>INSPCT.LISP.158 at 30-Jan-1985 10:17:36
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFMETHOD (BASIC-INSPECT :OBJECT-CLOSURE) (OBJ &AUX RESULT)
  (SETQ RESULT `("Function is "
		 (:ITEM1 CLOSURE-FUNCTION ,(INSPECT-FUNCTION-FROM (closure-function obj)))))
  (WHEN (ENTITYP OBJ)
    (PUSH '(".  ") RESULT)
    (PUSH `(:ITEM1 TYPE ,(TYPEP OBJ)) RESULT)
    (PUSH '("An object of type ") RESULT))
  (SETQ RESULT (LIST '("") RESULT))
  (let ((bindings (closure-bindings obj)))
    (case (length bindings)
      (0 (push '(("(No bindings. Pretty dull closure, eh?)")) result))
      (1 (push '(("Lexical slots")) result)
	 (loop for x in (caar bindings)
	       for i from 0
	       do (push `((:item1 closure-slot ,i ,#'(lambda (i s) (format s "~2D" i)))
			  (:colon 4)
			  (:item1 closure-value ,x))
			result)))
      (t (LET ((MAXL (loop for x in bindings by 'cddr
			   as h = (%find-structure-header x)
			   maximize (if (symbolp h)
					(case (%pointer-difference x h)
					  ((0 3 4) 0)			;name plist package
					  (1 (flatsize h))		;value
					  (t (+  2 (flatsize h))))	;function
				      0))))
	   (loop for (x y) on bindings by 'cddr
		 do (push `((:item1 closure-slot ,x
				    ,#'(lambda (x s &aux (h (%find-structure-header x)))
					 (if (symbolp h)
					     (format s "~[Pname of ~;~;#'~;~
							Plist of ~;Package of ~]~S"
						     (%pointer-difference x h) h)
					   (prin1 x s))))
			    (:COLON ,(+ 4 MAXL))
			    ,(IF (location-boundp y)
				 `(:ITEM1 CLOSURE-VALUE ,(contents y))
			         "void"))
			  result))))))
    (NREVERSE RESULT))

(DEFUN (:PROPERTY CLOSURE-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (LET ((SLOT (THIRD (SECOND ITEM)))
	(bindings (closure-bindings object)))
    (if (fixnump slot)				;lexiclosure
	(setf (nth slot (caar bindings)) new-value)
      (with-stack-list (s slot)
	(setf (contents (cadr (getl bindings s))) new-value)))))

(remprop 'closure-slot 'only-when-modify)
(setq inspect-prinlength 300.)

(defmethod (basic-inspect :object-locative) (obj)
  `(((:item1 locative-cell "Contents : " princ)
     ,(if (%p-contents-safe-p obj)
	  `(:item1 locative-contents ,(contents obj))
	(print-pointer obj nil)))
    (" Offset " ,(format nil "~D" (%pointer-difference obj (%find-structure-header obj)))
     " into "
     (:item1 ,(data-type (%find-structure-header obj)) ,(%find-structure-header obj)))
    ("%P-Cdr-Code : " ,(symbol-name (nth (%p-cdr-code obj) sys:q-cdr-codes)))
    ("%P-Data-Type: " ,(symbol-name (q-data-types (%p-data-type obj))))
    ("Area        : " ,(format nil "~O, ~S"
			       (%area-number obj) (area-name (%area-number obj))))))

))

; From file OZ:KANSAS:<L.SYS>QCFILE.LISP.323 at 30-Jan-1985 10:40:42
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-FILE (&OPTIONAL INPUT-FILENAME
		     &KEY OUTPUT-FILENAME
		     (SET-DEFAULT-PATHNAME T)
		     ((:PACKAGE PACKAGE-SPEC)) LOAD)
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the standard defaults.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE if non-NIL is the package to compile in.
LOAD means to load the compiled file."
  (QC-FILE (OR INPUT-FILENAME "") OUTPUT-FILENAME
	   LOAD NIL PACKAGE-SPEC NIL
	   (NOT SET-DEFAULT-PATHNAME)))

))

; From file OZ:KANSAS:<L.ZWEI>COMTAB.LISP.321 at 31-Jan-1985 08:57:51
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMTAB  "

(DEFUN DOCUMENT-ANY-EXTENDED-COMMAND (COMMAND CHAR OP)
  (COND ((EQ OP ':NAME) "a prefix for any Zmacs extended commands")
	((MEMQ OP '(:FULL :SHORT))
	 (FORMAT T "May be used to access commands not present in the current comtab~%")
	 (FORMAT T "Completing reads and executes a command from the mini buffer.~%")
	 (WHEN (EQ OP ':FULL)
	   (SETQ COMMAND (GET-ANY-EXTENDED-COMMAND "Type a command to document:"))
	   (UNLESS (EQUAL COMMAND "")
	     (FORMAT T "~:C ~A is implemented by ~S:~%" CHAR (CAR COMMAND) (CDR COMMAND))
	     (PRINT-DOC OP (CDR COMMAND) CHAR))))))

(defprop com-any-extended-command document-any-extended-command documentation-function)

))

; From file OZ:KANSAS:<L.WINDOW>SHWARM.LISP.332 at 1-Feb-85 02:04:38
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHWARM  "

(DEFMETHOD (SHEET :CLEAR-REST-OF-LINE) ()
  (SHEET-CLEAR-EOL SELF))

))

; From file OZ:KANSAS:<L.SYS>QCFILE.LISP.324 at 1-Feb-85 04:11:59
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILER-WARN (TYPE SEVERITY FORMAT-STRING &REST ARGS)
  "Record and print a compiler warning.
TYPE describes the particular kind of problem, such as FUNCTION-NOT-VALID.
SEVERITY is a symbol in the keyword package giving a broader classification;
see the source for a list of possible severities.  FORMAT-STRING and ARGS
are used to print the warning."
  (APPLY #'SI:RECORD-AND-PRINT-WARNING TYPE SEVERITY NIL FORMAT-STRING
	 (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
	   ;; Copy temp area data only; note that ARGS lives in PDL-AREA.
	   ;; on error for nonexistent package refname.
	   (MAPCAR #'(LAMBDA (ARG)
		       (SI:COPY-OBJECT-TREE ARG T 12.))
		   ARGS))))
(deff warn 'compiler-warn)
(make-obsolete warn "use COMPILER-WARN.")

))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.2 at 1-Feb-85 07:38:35
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer load-byte-expand load-byte (form &aux size position)
  (if (and (list-match-p (cddr form) `(,position ,size))
	   (typep position '(fixnum 0 #.(lsh 1 (byte-size %%byte-specifier-position))))
	   (typep size '(fixnum 0 #.(lsh 1 (byte-size %%byte-specifier-size)))))
      `(ldb ,(byte size position) ,(cadr form))
    form))

(defoptimizer deposit-byte-expand deposit-byte (form &aux size position into value)
  (if (and (list-match-p (cdr form) `(,into ,position ,size ,value))
	   (typep position '(fixnum 0 #.(lsh 1 (byte-size %%byte-specifier-position))))
	   (typep size '(fixnum 0 #.(lsh 1 (byte-size %%byte-specifier-size)))))
      ;; Is it possible that so reordering may screw us?
      (if (or (consp into) (consp value))
	  `(progn (%push ,into)
		  (dpb ,value ,(byte size position) (%pop)))
	;; no, it can't
	`(dpb ,value ,(byte size position) ,into))
    form))


(defoptimizer ^-integer-optimzer ^ (form &aux b e)
  (or (and (list-match-p (cdr form) `(,b ,e))
	   (atom b)
	   (case e
	     (1 b)
	     (2 `(* ,b ,b))
	     (3 `(* ,b ,b ,b))
	     (t nil)))
      form))

))


; From file CHARACTER.LISP KANSAS:<L.SYS2> OZ: (21)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defconst *char-bit-alist*
	  `((:control . ,%%kbd-control)
	    (:meta . ,%%kbd-meta)
	    (:super . ,%%kbd-super)
	    (:hyper . ,%%kbd-hyper))
  "Alist of bit names for CHAR-BIT vs byte specifiers to extract those bits from a character.")

(defun char-bit (char bit-name)
  "T if the bit spec'd by BIT-NAME (a keyword) is on in CHAR.
BIT-NAME can be :CONTROL, :META, :SUPER or :HYPER."
  (let ((byte (cdr (assq bit-name *char-bit-alist*))))
    (if byte
	(%logldb-test byte char)
      (ferror nil "~S is not a valid character-bit specifier" bit-name))))

(defun set-char-bit (char bit-name new-value)
  "Returns a character like CHAR except that the bit BIT-NAME has value NEW-VALUE in it.
BIT-NAME can be :CONTROL, :META, :SUPER or :HYPER.
NEW-VALUE should be T or NIL."
  (let ((byte (cdr (assq bit-name *char-bit-alist*))))
    (if byte
	(let* ((new-char (%logdpb (if new-value 1 0) byte char)))
	  (if (typep char 'character)
	      (int-char new-char)
	    new-char))
      (ferror nil "~S is not a valid character-bit specifier" bit-name))))
))

; From file OZ:OZ:<MLY.LL>QCOPT.LISP.2 at 1-Feb-85 08:36:33
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer char-bit-optimizer char-bit (form &aux char name)
  (if (and (list-match-p (cdr form) `(,char ,name))
	   (self-evaluating-p name))
      (let ((byte (cdr (assq name si::*char-bit-alist*))))
	(if byte
	    `(si::%logldb-test ,byte ,char)
	  (compiler-warn 'unknown-char-bit ':implausible
			 "~S is not the name of a bucky bit" name)
	  form))
    form))

;; depends on whether %logdpb on characters continues to make characters.
;(defoptimizer set-char-bit-optimizer set-char-bit (form &aux char name value)
;  (if (and (list-match-p (cdr form) `(,char ,name ,value))
;	   (self-evaluating-p name))
;      (let ((byte (cdr (assq name si::*char-bit-alist*))))
;	(if byte
;	    ;; possible order-of-evaluation problems?
;	    (if (or (consp char) (consp value))
;		`(progn (%push ,char)
;			(%logdpb ,value ,byte (%pop)))
;	        `(%logdpb ,value ,byte ,char))
;	  (compiler-warn 'unknown-char-bit ':implausible
;			 "~S is not the name of a bucky bit" name)
;	  form))
;    form))

))

; From file QFILE.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (358)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFUN RENAME-CHAOS (ACCESS OLD-PATHNAME NEW-PATHNAME ERROR &AUX PKT SUCCESS STRING)
  (DECLARE (VALUES TRUENAME OLD-TRUENAME))
  (FILE-OPERATION-RETRY
    (LET ((HOST-UNIT (SEND ACCESS :GET-HOST-UNIT)))
      (UNWIND-PROTECT
	  (PROGN (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
		   (SEND HOST-UNIT :COMMAND NIL NIL NIL "Rename" "RENAME" #/NEWLINE
			 (FILE-PRINT-PATHNAME OLD-PATHNAME) #/NEWLINE
			 (FILE-PRINT-PATHNAME NEW-PATHNAME) #/NEWLINE))
		 (IF SUCCESS
		     ;; If there is a second line coming from the file server,
		     ;; it is the new truename.
		     (let* ((from (string-search-char #/newline string)) truename)
		       (if (null from) (values new-pathname old-pathname)
			 (let* ((old (string-search-char #/newline string (1+ from)))
				(host (send old-pathname :host)))
			   (setq truename (parse-pathname string host nil (1+ from) old))
			   (if (null old) (values truename old-pathname)
			     (values truename
				     (fs:parse-pathname string host nil (1+ old)))))))
		   (QFILE-PROCESS-ERROR-NEW STRING OLD-PATHNAME NIL (NOT ERROR) :RENAME)))
      (AND PKT (CHAOS:RETURN-PKT PKT))))))

))

; From file PATED.LISP KANSAS:<L.ZWEI> OZ: (31)
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN ADD-PATCH-INTERVAL (BP1 BP2 IN-ORDER-P DEFUN-NAME BUFFER &AUX NEW-PATCH-BUFFER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (VALIDATE-PATCH-BUFFER)
  (SETQ NEW-PATCH-BUFFER-P (NULL *PATCH-BUFFER*))
  (AND NEW-PATCH-BUFFER-P (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME)))
  (FORMAT *QUERY-IO*
	  "~&Adding ~:[~S~;~A~] to patch file ~A~@[~%(New patch file)~]"
	  (STRINGP DEFUN-NAME) DEFUN-NAME (PATCH-VERSION-DESCRIPTION) NEW-PATCH-BUFFER-P)
  (LET ((BP (INTERVAL-LAST-BP *PATCH-BUFFER*)))
    ;; Put into the patch buffer, making sure the right package and base will be used.
    (MULTIPLE-VALUE-BIND (VARS VALS)
	(SEND BUFFER :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	(LET ((TIME:*DEFAULT-DATE-PRINT-MODE* :DD-MMM-YY))
	  (INSERT BP (FORMAT NIL "~%; From ~:[buffer ~A~;file ~:*~A~*~] at ~\datime\~%"
			     (CAR-SAFE (SEND BUFFER :FILE-ID)) BUFFER (GET-UNIVERSAL-TIME))))
	(INSERT BP (FORMAT NIL "#~DR ~A#:
/(~S ((~S (~S ~S)))
/  (~S ~S~2%"
			   *READ-BASE* (PACKAGE-NAME *PACKAGE*)
			   'COMPILER-LET '*PACKAGE* 'PKG-FIND-PACKAGE (PACKAGE-NAME *PACKAGE*)
			   'COMPILER::PATCH-SOURCE-FILE
			   (WHEN (BUFFER-GENERIC-PATHNAME BUFFER)
			     (SEND (BUFFER-GENERIC-PATHNAME BUFFER) :STRING-FOR-PRINTING))))))
    (INSERT-INTERVAL BP BP1 BP2 T)
    (INSERT BP "
))
"))
  ;; Mark all sections that the region contains part of
  ;; as having been patched.
  (INTERVAL-LINES (BP1 BP2) (START-LINE END-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (LAST-SECTION))
	((EQ LINE END-LINE))
      (LET ((SECTION (LINE-NODE LINE)))
	(UNLESS (EQ LAST-SECTION SECTION)
	  (PUTPROP SECTION *TICK* 'PATCH-TICK))
	(SETQ LAST-SECTION SECTION)))))

))

; From file CHARACTER.LISP KANSAS:<L.SYS2> OZ: (21)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun code-char (code &optional (bits 0) (font 0))
  "Returns a character whose code comes from CODE, bits from BITS and font from FONT.
CODE can be a number or a character.
NIL is returned if it is not possible to have a character object
with the specified FONT and BITS."
  (if (and ( 0 bits (1- char-bits-limit))
	   ( 0 font (1- char-font-limit)))
      (%make-pointer dtp-character
		     (%logdpb bits %%kbd-control-meta
			      (dpb font %%ch-font code)))
    nil))
(deff make-char #'code-char)

(defun digit-char (weight &optional (radix 10.) (font 0))
  "Return a character which signifies WEIGHT in radix RADIX, with FONT as specified.
This is always NIL if WEIGHT is  RADIX.
Otherwise, for WEIGHT between 0 and 9, you get characters 0 through 9;
for higher weights, you get letters."
  (if (not ( 0 weight (1- radix))) nil
    (if (not ( 0 font char-font-limit)) nil
      (%make-pointer dtp-character
		     (dpb font %%ch-font (if (< weight 10.)
					     (+ (char-code #/0) weight)
					     (+ (char-code #/A) weight -10.)))))))

))

; From file OZ:KANSAS:<L.SYS2>SGDEFS.LISP.55 at 7-Feb-85 00:18:59
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; SGDEFS  "

(DEFSUBST FEF-DEBUGGING-INFO-PRESENT-P (FEF)
  (LDB-TEST %%FEFHI-MS-DEBUG-INFO-PRESENT (%P-CONTENTS-OFFSET FEF %FEFHI-MISC)))

(DEFUN FEF-DEBUGGING-INFO (FEF) 
  (AND (FEF-DEBUGGING-INFO-PRESENT-P FEF)
       (%P-CONTENTS-OFFSET FEF (1- (%P-LDB %%FEFH-PC-IN-WORDS FEF)))))

(DEFSETF FEF-DEBUGGING-INFO SET-FEF-DEBUGGING-INFO)
(DEFLOCF FEF-DEBUGGING-INFO LOCATE-FEF-DEBUGGING-INFO)

(DEFUN SET-FEF-DEBUGGING-INFO (FEF VALUE)
  (IF (FEF-DEBUGGING-INFO-PRESENT-P FEF)
      (LET ((%INHIBIT-READ-ONLY T))
	(SETF (%P-CONTENTS-OFFSET FEF (1- (%P-LDB %%FEFH-PC-IN-WORDS FEF))) VALUE))
    (FERROR NIL "The FEF ~S has nowhere to put debugging-info" FEF)))
(DEFUN LOCATE-FEF-DEBUGGING-INFO (FEF)
  (IF (FEF-DEBUGGING-INFO-PRESENT-P FEF)
      (%MAKE-POINTER-OFFSET DTP-LOCATIVE (FOLLOW-STRUCTURE-FORWARDING FEF)
					 (1- (%P-LDB %%FEFH-PC-IN-WORDS FEF)))
    (FERROR NIL "The FEF ~S has no debugging-info" FEF)))

))

; From file OZ:KANSAS:<L.SYS>GENRIC.LISP.33 at 4-Feb-85 01:49:16
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun subst-1 (new old tree &optional test invertp key one-arg-predicate)
  (cond ((let ((elt (if key (funcall key tree) tree)))
	   (eq invertp (not (cond (one-arg-predicate (funcall one-arg-predicate elt))
				  (test (funcall test old elt))
				  (t (eql old elt))))))
	 new)
	((atom tree)
	 tree)
	(t
	 (let ((newcar (subst-1 new old (car tree) test invertp key one-arg-predicate))
	       (newcdr (subst-1 new old (cdr tree) test invertp key one-arg-predicate)))
	   (if (and (eql newcar (car tree))
		    (eql newcdr (cdr tree)))
	       tree
	     (cons newcar newcdr))))))

))

; From file OZ:KANSAS:<L.SYS>EVAL.LISP.91 at 4-Feb-85 01:57:37
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun tagbody-internal (body)
  (with-stack-list (tem body nil)
    (with-stack-list (frame 'tagbody tem)
      (with-stack-list* (*interpreter-frame-environment*
			  frame *interpreter-frame-environment*)
	(do ((pc body))
	    (())
	  (cond ((null pc) (return nil))
		((atom pc)
		 (ferror nil "Non-~S atomic cdr in ~S form ~S." 'nil 'tagbody body)
		 (return nil)))
	  (let ((exp (car pc)))
	    (setq pc (cdr pc))
	    (if (atom exp) nil
	      (catch-continuation (cdr (cadr frame))
		  #'(lambda (gotag-pointer) (setq pc (cdr gotag-pointer)))
		  nil
		(with-stack-list (tem1 nil)
		  (setf (cadr tem) (%make-pointer-offset dtp-locative tem1 -1)))
		(eval1 exp)))))))))

(defun apply-lambda (fctn a-value-list)
  (prog (tem)
	(unless (consp fctn) (go bad-function))
   tail-recurse
	(case (car fctn)
	  (curry-after
	   (tagbody
	       (setq tem (cddr fctn))
	       (%open-call-block (cadr fctn) 0 4)
	       (%assure-pdl-room (+ (length tem) (length a-value-list)))

	    loop1
	       (or a-value-list (go loop2))
	       (%push (car a-value-list))
	       (and (setq a-value-list (cdr a-value-list))
		    (go loop1))

	    loop2
	       (or tem (go done))
	       (%push (eval1 (car tem)))
	       (and (setq tem (cdr tem))
		    (go loop2))

	    done
	       (%activate-open-call-block)))
	  (curry-before
	   (tagbody
	       (setq tem (cddr fctn))
	       (%open-call-block (cadr fctn) 0 4)
	       (%assure-pdl-room (+ (length tem) (length a-value-list)))

	    loop1
	       (or tem (go loop2))
	       (%push (eval1 (car tem)))
	       (and (setq tem (cdr tem))
		    (go loop1))

	    loop2
	       (or a-value-list (go done))
	       (%push (car a-value-list))
	       (and (setq a-value-list (cdr a-value-list))
		    (go loop2))

	    done
	       (%activate-open-call-block)))
	  ((lambda named-lambda subst cli:subst named-subst)
;>> Ugh!
	   (let-if (memq (car fctn) '(named-lambda named-subst))
		   ((*interpreter-variable-environment* nil)
		    (*interpreter-function-environment* nil)
		    (*interpreter-frame-environment* nil)
;		    (local-declarations nil)
		    )
	     (let* (optionalf quoteflag tem restf init this-restf specialf
		    (fctn1 (cond ((eq (car fctn) 'named-lambda) (cdr fctn))
				 ((eq (car fctn) 'named-subst) (cdr fctn))
				 (t fctn)))
		    (lambda-list (cadr fctn1))
		    (body (cddr fctn1))
		    (value-list a-value-list)
;		    (local-declarations local-declarations)
		    this-specialp thisval  ;Used by expansion of apply-lambda-bindvar
		    keynames keyinits keykeys keyflags
		    keynames1 keykeys1 keyflags1 (unspecified '(()))
		    allow-other-keys
		    thisvar)
	       (and (cdr body) (stringp (car body)) (pop body))	;and doc string.
	       ;; Make a binding frame to represent any SPECIAL declarations.
	       (with-stack-list* (vars-env nil *interpreter-variable-environment*)
		 ;; If SELF is an instance, and instance vars aren't bound, bind them.
		 (when (and (typep self 'instance)
			    (do ((tail (cdr vars-env) (cdr tail)))
				((atom tail) nil)
			      (and (setq tem
					 (get-lexical-value-cell
					   (car tail)
					   ;; all this to avoid a compiler warning...
					   (locally
					     (declare (special .slots.bound.instance.))
					     (inhibit-style-warnings
					       (locf (symbol-value
						       '.slots.bound.instance.))))))
				   (return (eq (contents tem) self)))))
		   ;;??? Here should take care of special instance variables!!!
		   ;; Probably just omit them, since they were bound when
		   ;; the message was sent, weren't they?
		   (tagbody
		       (setq tem (self-binding-instances))
		    loop
		       (when tem
			 (apply-lambda-bindvar-1 (car tem) (cadr tem) vars-env)
			 (setq tem (cddr tem))
			 (go loop)))
		   (apply-lambda-bindvar-1
		     (locally
		       (declare (special .slots.bound.instance))
		       (inhibit-style-warnings
			 (locf (symbol-value '.slots.bound.instance))))
		     self
		     vars-env))
		 (with-stack-list* (vars-env nil vars-env)
		   ;; Find any declarations at the front of the function body
		   ;; and put them onto VARS-ENV ;;(and LOCAL-DECLARATIONS)
		   ;; Note that any declarations will override instance bindings made
		   (gobble-declarations-internal body vars-env)
		   (with-stack-list* (*interpreter-variable-environment* nil vars-env)
		     (tagbody
		      l
			 (cond ((null value-list) (go lp1))
			       ((or (null lambda-list)
				    (eq (car lambda-list) '&aux)) 
				(cond (restf (go lp1)))
				(return-from apply-lambda
				  (signal-proceed-case
				    ((args)
				     (make-condition 'sys:too-many-arguments
				       "Function ~S called with too many arguments (~D)."
				       fctn (length a-value-list) a-value-list))
				    (:fewer-arguments
				     (apply fctn (append a-value-list args)))
				    (:return-value args)
				    (:new-argument-list (apply fctn args)))))
			       ((eq (car lambda-list) '&key)
				(go key))
			       ((eq (car lambda-list) '&optional)
				(setq optionalf t)
				(go l1))		;Do next value.
			       ((eq (car lambda-list) '&quote)
				(setq quoteflag t)
				(go l1))
			       ((eq (car lambda-list) '&eval)
				(setq quoteflag nil)
				(go l1))
			       ((memq (car lambda-list) '(&special &local))
				(setq specialf (eq (car lambda-list) '&special))
				(go l1))
			       ((memq (car lambda-list) '(&rest &body))
				(setq this-restf t)
				(go l1))		;Do next value.
			       ((memq (car lambda-list) lambda-list-keywords)
				(go l1))
			       ((atom (car lambda-list))
				(setq thisvar (car lambda-list)))
			       ((atom (caar lambda-list))
				(setq thisvar (caar lambda-list))
				;; If it's &OPTIONAL (FOO NIL FOOP),
				;; bind FOOP to T since FOO was specified.
				(when (and optionalf (cddar lambda-list))
				  (and (null (caddar lambda-list)) (go bad-lambda-list))
				  (apply-lambda-bindvar (caddar lambda-list)
							t vars-env specialf)))
			       (t (go bad-lambda-list)))
			 ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
			 ;;  It is in THISVAR.
			 (and (null thisvar) (go bad-lambda-list))
			 (cond (restf
				;; Something follows a &REST arg???
				(go bad-lambda-list))
			       (this-restf	;This IS the &REST arg.
				;; If quoted arg, and the list of values is in a pdl, copy it.
				(and quoteflag
				     (ldb-test %%pht2-map-access-code
					       (area-region-bits (%area-number value-list)))
				     (let ((default-cons-area background-cons-area))
				       (setq value-list (copy-list value-list))))
				(apply-lambda-bindvar thisvar value-list vars-env specialf)
				;; We don't clear out VALUE-LIST
				;; in case keyword args follow.
				(setq this-restf nil restf t)
				(go l1)))
  
			 (apply-lambda-bindvar thisvar (car value-list) vars-env specialf)
			 (pop value-list)
		      l1 (pop lambda-list)
			 (go l)
  
		      key
			 (setf (values nil nil lambda-list nil nil
				       keykeys keynames keyinits keyflags
				       allow-other-keys)
			       (decode-keyword-arglist lambda-list t))
			 ;; Process the special keyword :ALLOW-OTHER-KEYS if present as arg.
			 (if (getf value-list ':allow-other-keys)
			     (setq allow-other-keys t))
  
			 (setq keykeys1 keykeys	;life is tough without LET...
			       keynames1 keynames
			       keyflags1 keyflags)
		      key1
			 (when keykeys1
			   (setq tem (getf value-list (pop keykeys1) unspecified))
			   (setq init (if (eq tem unspecified) (eval1 (car keyinits)) tem))
			   (apply-lambda-bindvar (car keynames1) init vars-env)
			   (if (car keyflags1)
			       (apply-lambda-bindvar (car keyflags1)
						     (neq tem unspecified)
						     vars-env))
			   (pop keynames1)
			   (pop keyflags1)
			   (pop keyinits)
			   (go key1))
			 (do ((x value-list (cddr x))
			      keyword)
			     ((null x))
			   (unless (cdr x)
			     (ferror 'sys:bad-keyword-arglist
				     "No argument after keyword ~S"
				     (car x)))
			   (setq keyword (car x))
			   (setq tem (find-position-in-list keyword keykeys))
			   (unless (or tem allow-other-keys)
			     (do-forever
			       (setq keyword (cerror :new-keyword nil
						     'sys:undefined-keyword-argument
						     "Keyword arg keyword ~S, with value ~S, is unrecognized."
						     keyword
						     (cadr value-list)))
			       (when (and keyword
					  (setq tem (find-position-in-list keyword keykeys)))
				 (interpreter-set (nth tem keynames) (cadr x))
				 (and (setq tem (nth tem keyflags))
				      (interpreter-set tem t))
				 (return)))))
			 ;; Keyword args always use up all the values that are left...
  
			 ;; Here when all values used up.
		      lp1
			 (cond ((null lambda-list) (go ex1))
			       ((memq (car lambda-list) '(&rest &body))
				(and restf (go bad-lambda-list))
				(setq this-restf t)
				(go lp2))
			       ((eq (car lambda-list) '&key)
				(go key))
			       ((memq (car lambda-list) '(&optional &aux))
				(setq optionalf t)	;Suppress too few args error
				(go lp2))
			       ((memq (car lambda-list) '(&special &local))
				(setq specialf (eq (car lambda-list) '&special))
				(go lp2))
			       ((memq (car lambda-list) lambda-list-keywords)
				(go lp2))
			       ((and (null optionalf) (null this-restf))
				(and restf (go bad-lambda-list))
				(return-from apply-lambda
				  (signal-proceed-case
				    ((args)
				     (make-condition 'sys:too-few-arguments
				       "Function ~S called with only ~D argument~1@*~P."
				       fctn (length a-value-list) a-value-list))
				    (:additional-arguments
				     (apply fctn (append a-value-list args)))
				    (:return-value args)
				    (:new-argument-list (apply fctn args)))))
			       ((atom (car lambda-list)) (setq tem (car lambda-list))
							 (setq init nil))
			       ((atom (caar lambda-list))
				(setq tem (caar lambda-list))
				(setq init (eval1 (cadar lambda-list)))
				;; For (FOO NIL FOOP), bind FOOP to NIL since FOO missing.
				(when (cddar lambda-list)
				  (and (null (caddar lambda-list)) (go bad-lambda-list))
				  (apply-lambda-bindvar (caddar lambda-list)
							nil vars-env specialf)))
			       (t (go bad-lambda-list)))
		      lp3
			 (and (null tem) (go bad-lambda-list))
			 (apply-lambda-bindvar tem init vars-env specialf)
			 (and this-restf (setq restf t))
			 (setq this-restf nil)
		      lp2
			 (setq lambda-list (cdr lambda-list))
			 (go lp1)
  
		      ex1
			 ;; Here to evaluate the body.
			 (return-from apply-lambda (eval-body body))
		      bad-lambda-list
			 (setq fctn
			       (cerror :new-function nil 'sys:invalid-lambda-list
				       "~S has an invalid lambda list" fctn))
		      retry
			 (return-from apply-lambda (apply fctn a-value-list)))))))))
	  (macro
	   (ferror 'sys:funcall-macro
		   "Funcalling the macro ~S."
		   (function-name (cdr fctn)))
	   (return-from apply-lambda
	     (eval1 (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list))))))

	;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
	(when (lambda-macro-call-p fctn)
	  (setq fctn (lambda-macro-expand fctn))
	  (go retry))

   bad-function
	;; Can drop through to here for a totally unrecognized function.
	(setq fctn
	      (cerror :new-function nil 'sys:invalid-function
		      "~S is an invalid function." fctn))
	(go retry)

	;; Errors jump out of the inner PROG to unbind any lambda-vars bound with %BIND.
   bad-lambda-list
	(setq fctn
	      (cerror :new-function nil 'sys:invalid-lambda-list
		      "~S has an invalid lambda list" fctn))
   retry
	(and (consp fctn) (go tail-recurse))
	(return (apply fctn a-value-list))

   too-few-args
	(return (signal-proceed-case
		  ((args)
		   (make-condition 'sys:too-few-arguments
				   "Function ~S called with only ~D argument~1@*~P."
				   fctn (length a-value-list) a-value-list))
		  (:additional-arguments
		   (apply fctn (append a-value-list args)))
		  (:return-value args)
		  (:new-argument-list (apply fctn args))))

   too-many-args
	(return (signal-proceed-case
		  ((args)
		   (make-condition 'sys:too-many-arguments
				   "Function ~S called with too many arguments (~D)."
				   fctn (length a-value-list) a-value-list))
		  (:fewer-arguments
		   (apply fctn (append a-value-list args)))
		  (:return-value args)
		  (:new-argument-list (apply fctn args))))))

))


; From file OZ:KANSAS:<L.IO1>FQUERY.LISP.46 at 6-Feb-85 23:16:08
#10R FORMAT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO1; FQUERY  "

(DEFSELECT TYI-FQUERY-FUNCTION
  (:READ (STREAM)
    (DO ((CH)) (())
      (FQUERY-PROMPT STREAM)
      (LET ((RUBOUT-HANDLER NIL))		;in case inside with-input-editing,
	(SETQ CH (READ-CHAR STREAM)))		; don't want character echoed twice.
      (UNLESS (AND (CHAR= CH #/HELP) FQUERY-HELP-FUNCTION)
	(RETURN CH))
      (SEND FQUERY-HELP-FUNCTION STREAM FQUERY-CHOICES 'TYI-FQUERY-FUNCTION)
      (SEND STREAM :FRESH-LINE)))
  (:ECHO (ECHO STREAM)
    (SEND STREAM :STRING-OUT (STRING ECHO)))
  (:MEMBER (CHAR LIST)
; character lossage
    (MEM #'(LAMBDA (X Y) (CHAR-EQUAL X (COERCE Y 'CHARACTER))) CHAR LIST)))

))

