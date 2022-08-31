;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.11
;;; Reason: Fix GLOBAL:GET-UNIVERSAL-TIME.  Arrest-the-process restart bug.
;;; Verify sectionization faster. Automatic methods bug.
;;; Fix scope of SPECIAL declarations in compiler and evaluator.
;;; Changes in Common Lisp spec:
;;;  Values returned by DEFTYPE, DEFVAR, DEFCONST...
;;;  Sequential binding of SETF tempvars.
;;;  DIGIT-CHAR, CHAR-NAME, PUSHNEW, (N)SUBST-IF(-NOT), SUBLIS,
;;;  MAKE-BROADCAST-STREAM, MAKE-STRING-OUTPUT-STREAM, #(...), #*,
;;;  WRITE-CHAR, WRITE-BYTE, ~E, ASSERT,
;;;  PARSE-NAMESTRING/PARSE-PATHNAME, MAKE-PATHNAME, FILE-POSITION.
;;; Written 12/20/83 10:37:23 by RMS,
;;; while running on Lisp Machine Eighteen from band 7
;;; with Bad Inconsistently updated System 98.9, CADR 3.1, Experimental ZMail 53.5, MIT-Specific 22.0, Experimental Local-File 48.0, Experimental FILE-Server 8.0, Experimental LFS 3.0, microcode 305, ZM MIT.


(fset 'get-universal-time 'time:get-universal-time)

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(defmacro with-output-to-string ((stream string index) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
If STRING is omitted, a new string with no fill pointer is created and returned.
If STRING is supplied, that string's contents are modified destructively,
and the values of BODY's last expression are returned.
If INDEX is supplied, it should be a SETFable accessor which describes
where to find the index to store into STRING, instead of at the end.
The value of INDEX will be updated after the BODY is finished."
  (multiple-value-bind (realbody decls)
      (extract-declarations body)
    (if string
	`(let ((,stream (make-string-output-stream ,string ,index)))
	   (declare . ,decls)
	   (unwind-protect
	       (progn . ,realbody)
	     ,(if index `(setf ,index (send ,stream ':get-string-index)))))
      `(let ((,stream (make-string-output-stream)))
	 (declare . ,decls)
	 ,@realbody
	 (get-output-stream-string ,stream)))))

))

; From file TYPES.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defmacro deftype (name arglist &body body)
  "Defines NAME as a data type name for use in TYPEP, etc.
A list starting with NAME, used as a type specifier,
expands by binding the args in ARGLIST and then evaluating the BODY.
The value of BODY should be another type specifier.
Any optional arguments in ARGLIST which do not have default values specified
will be bound to * by default, rather than NIL."
  (let ((argcopy (copylist arglist))
	optionalf)
    (do ((tail argcopy (cdr tail))) ((null tail))
      (cond ((eq (car tail) '&optional)
	     (setq optionalf t))
	    ((memq (car tail) '(&key &rest &aux))
	     (return))
	    ((and optionalf
		  (atom (car tail))
		  (not (memq (car tail) lambda-list-keywords)))
	     (setf (car tail)
		   (list (car tail) ''*)))))
    `(progn
       (defun (,name type-expander) ,argcopy
	 . ,body)
       ',name)))

))

; From file QRAND.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QRAND  "


;;; These are used by DEFVAR and DEFCONST in LMMAC
(DEFUN DEFVAR-1 (&QUOTE SYMBOL &OPTIONAL (VALUE ':UNBOUND) DOCUMENTATION)
  (AND (CONSP SYMBOL) (EQ (CAR SYMBOL) 'QUOTE)
       (SETQ SYMBOL (CADR SYMBOL)))
  (COND ((RECORD-SOURCE-FILE-NAME SYMBOL 'DEFVAR)
	 (PUTPROP SYMBOL (OR FDEFINE-FILE-PATHNAME T) 'SPECIAL)
	 (AND (NEQ VALUE ':UNBOUND)
	      (OR FS:THIS-IS-A-PATCH-FILE (NOT (BOUNDP SYMBOL)))
	      (SET SYMBOL (EVAL1 VALUE)))
	 (SETF (DOCUMENTATION SYMBOL 'VARIABLE) DOCUMENTATION)))
  SYMBOL)

(DEFUN DEFCONST-1 (&QUOTE SYMBOL &EVAL VALUE &OPTIONAL DOCUMENTATION)
  (AND (CONSP SYMBOL) (EQ (CAR SYMBOL) 'QUOTE)
       (SETQ SYMBOL (CADR SYMBOL)))
  (COND ((RECORD-SOURCE-FILE-NAME SYMBOL 'DEFVAR)
	 (PUTPROP SYMBOL (OR FDEFINE-FILE-PATHNAME T) 'SPECIAL)
	 (SET SYMBOL VALUE)
	 (SETF (DOCUMENTATION SYMBOL 'VARIABLE) DOCUMENTATION)))
  SYMBOL)

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


;; Called like SUBLIS, but makes the replacement expressions
;; be evaluated only once and in the same order as they appear in ALIST.
(defun sublis-eval-once (alist exp &optional reuse-flag sequential-flag)
  "Effectively substitute for symbols in EXP according to ALIST, preserving execution order.
Each element of ALIST describes one symbol (the car)
and what it stands for (the cdr).
We replace each symbol with the corresponding value expression,
not with straight textual substitution but so that
the value expression will be evaluated only once.

If SEQUENTIAL-FLAG is non-NIL, the value substituted for each symbol
may refer to the previous symbols substituted for.

This may require the use of temporary variables.
The first use of a symbol would be replaced by a SETQ of the tempvar
to the symbol's corresponding expression.  Later uses would be
replaced with just the tempvar.  A LET to bind the tempvars is
wrapped around the whole expression.

If REUSE-FLAG is non-NIL, the symbols themselves can be used
as their own tempvars when necessary.  Otherwise tempvars are gensymmed."
  (let (constant-alist nonconstant-alist
	value-so-far)
    ;; First, divide replacements up into constant values vs nonconstants.
    ;; Order of evaluation never matters for the constants so we will
    ;; put them in with SUBLIS.
    (dolist (elt alist)
      (let ((tem (if sequential-flag
		     (sublis constant-alist (cdr elt))
		   (cdr elt))))
	(if (constantp tem)
	    (push (if (eq tem (cdr elt)) elt (cons (car elt) tem))
		  constant-alist)
	  (push (list (car elt) 0 tem nil nil)
		nonconstant-alist))))
    ;; The nonconstants must remain in the proper order!
    (setq nonconstant-alist (nreverse nonconstant-alist))
    ;; If the only things not constant are variables,
    ;; then they are ok.
    (when (loop for elt in nonconstant-alist
		always (symbolp (seo-exp elt)))
      (dolist (elt nonconstant-alist)
	(push (cons (car elt)
		    (if sequential-flag (sublis constant-alist (seo-exp elt)) (seo-exp elt)))
	      constant-alist))
      (setq nonconstant-alist nil))
    (setq value-so-far (sublis constant-alist exp))
    (when nonconstant-alist
      ;; Each nonconstant value should be inserted only once, and in correct order.
      ;; SEO-FIRST-UNINSERTED-VAR points to the first one we have not yet inserted.
      ;; All the ones before that have had temporary variables (gensyms) made.
      (let* ((seo-first-uninserted-var nonconstant-alist))
	(setq value-so-far (sublis-eval-once-1 value-so-far nonconstant-alist
					       reuse-flag sequential-flag))
	;; Now stick on evaluations of any values that weren't really used.
	(if seo-first-uninserted-var
	    (setq value-so-far
		  `(multiple-value-prog1
		     ,value-so-far
		     . ,(if sequential-flag
			    (list (sublis-eval-once-1 (caar (last nonconstant-alist))
						      nonconstant-alist
						      reuse-flag t))
			  (mapcar 'seo-exp seo-first-uninserted-var))))))
      ;; If a temp var is not used again after it is set,
      ;; flush the temp var from the code -- just use its value straight.
      (dolist (elt nonconstant-alist)
	(let ((tem (seo-first-use elt)))
	  (when (zerop (seo-count elt))
	    (do ((tail (cdr tem) (cdr tail)))
		((null tail))
	      (when (and (listp (car tail))
			 (eq (caar tail) 'setq)
			 (eq (cadar tail) (seo-tempvar elt)))
		(setf (car tail) (caddar tail))
		(return)))))))
    ;; Now see which temp vars still remain in use,
    ;; and put on a binding for them.
    (let ((tempvars-used
	    (loop for elt in nonconstant-alist
		  when (not (zerop (seo-count elt)))
		  collect (list (seo-tempvar elt) '(compiler:undefined-value)))))
      (if tempvars-used
	  `(let ,tempvars-used ,value-so-far)
	value-so-far))))

(defun sublis-eval-once-1 (exp alist &optional reuse-flag sequential-flag)
  (if (null alist) exp
    (if (symbolp exp)
	(let ((tem (assq exp alist)))
	  (cond ((null tem) exp)
		((seo-tempvar tem)
		 (incf (seo-count tem))
		 (seo-tempvar tem))
		((eq (seo-count tem) t)
		 (seo-exp tem))
		(t
		 (setf (seo-tempvar tem)
		       (if reuse-flag (car tem) (gensym)))
		 (setf (seo-count tem) 0)
		 (setf (seo-first-use tem)
		       (cons 'progn nil))
		 (let ((e1
			 `(,@(loop for tail on seo-first-uninserted-var
				   until (eq (car tail) tem)
				   do (setf (seo-tempvar (car tail))
					    (if reuse-flag (caar tail) (gensym)))
				   (setf (seo-first-use (car tail)) (seo-first-use tem))
				   collect `(setq ,(seo-tempvar (car tail))
						  ,(if sequential-flag
						       (sublis-eval-once-1
							 (seo-exp (car tail))
							 (ldiff alist tail))
						     (seo-exp (car tail))))
				   finally (setq seo-first-uninserted-var (cdr tail)))
			   (setq ,(seo-tempvar tem)
				 ,(if sequential-flag
				      (sublis-eval-once-1 (seo-exp tem)
							  (ldiff alist (memq tem alist)))
				    (seo-exp tem))))))
		   (setf (cdr (seo-first-use tem)) e1)
		   (seo-first-use tem)))))
      (if (atom exp) exp
	(mapcar 'sublis-eval-once-1 exp (circular-list alist)
		(circular-list reuse-flag)
		(circular-list sequential-flag))))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro define-modify-macro (name additional-arglist action-function &optional doc-string)
  "Define a construct which, like INCF, modifies the value of its first argumenty.
NAME is defined so that (NAME place additional-args) expands into
  (SETF place (action-function place additional-args))
except that subforms of place are evaluated only once."
  (let (additional-arg-names)
    (dolist (x additional-arglist)
      (cond ((symbolp x)
	     (if (not (memq x lambda-list-keywords)) (push x additional-arg-names)))
	    (t ; it's a list after &optional
	     (push (car x) additional-arg-names))))
    `(defmacro ,name (place . ,additional-arglist)
       ,doc-string
       (if (symbolp place)
	   ;; Special case this to speed up the expansion process and make better code.
	   `(setq ,place (,',action-function ,place . ,(list . ,additional-arg-names)))
	 (multiple-value-bind (tempvars tempargs storevars storeform refform)
	     (get-setf-method place)
	   (let ((additional-temps (mapcar #'(lambda (ignore) (gensym)) ',additional-arg-names)))
	     (sublis-eval-once (pairlis tempvars tempargs
					(pairlis additional-temps (list . ,additional-arg-names)))
			       (sublis (list (cons (car storevars)
						   (list* ',action-function refform
							  additional-temps)))
				       storeform)
			       t t)))))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro push (value place)
  "Add ITEM to the front of the list PLACE, using CONS.
Works by SETF'ing PLACE."
  (if (symbolp place)
      ;; Special case this to speed up the expansion process and make better code.
      `(setq ,place (cons ,value ,place))
    (multiple-value-bind (tempvars tempargs storevars storeform refform)
	(get-setf-method place)
      (let ((val (gensym)))
	(sublis-eval-once (cons `(,val . ,value) (pairlis tempvars tempargs))
			  (sublis (list (cons (car storevars)
					      `(cons ,val ,refform)))
				  storeform)
			  t t)))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro pop (place &optional into-place)
  "Remove the first element from the list PLACE, and return that element.
Works by SETF'ing PLACE.  If INTO-PLACE is specified, store the
value there as well as returning it."
  (if (and (symbolp place)
	   (symbolp into-place))
      ;; Special case this to speed up the expansion process and make better code.
      (if into-place
	  `(prog1 (setf ,into-place (car ,place)) (setq ,place (cdr ,place)))
	`(prog1 (car ,place) (setq ,place (cdr ,place))))
    (if into-place
	(multiple-value-bind (into-tempvars into-tempargs into-storevars into-storeform)
	    (get-setf-method into-place)
	  (multiple-value-bind (tempvars tempargs storevars storeform refform)
	      (get-setf-method place)
	    (sublis-eval-once (pairlis tempvars tempargs
				       (pairlis into-tempvars into-tempargs))
			      `(prog1 ,(sublis (list (cons (car into-storevars)
							   `(car ,refform)))
					       into-storeform)
				      ,(sublis (list (cons (car storevars)
							   `(cdr ,refform)))
					       storeform))
			      t t)))
      (multiple-value-bind (tempvars tempargs storevars storeform refform)
	  (get-setf-method place)
	(sublis-eval-once (pairlis tempvars tempargs)
			  `(prog1 (car ,refform)
				  ,(sublis (list (cons (car storevars)
						       `(cdr ,refform)))
					   storeform))
			  t t)))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro setf (&rest places-and-values)
  "Sets the value of PLACE to be VALUE.  Allows any number of places and values, like SETQ.
For example, (SETF (AREF A I) NEWVALUE) sets the value of (AREF A I)."
  (declare (arglist place value ...))
  `(progn
     . ,(loop for (place value) on places-and-values by 'cddr
	      collect
	      (multiple-value-bind (tempvars tempargs storevars storeform)
		  (get-setf-method-multiple-value place t)
		(if (and tempvars (symbolp tempvars))
		    ;; Handle case of simple DEFSETF as fast as possible.
		    `(,tempvars ,@(cdr tempargs) ,value)
		  (sublis-eval-once (pairlis tempvars tempargs
					     (list (cons (car storevars) value)))
				    storeform t t))))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro rotatef (&rest places)
  "Rotates the values between all the specified PLACEs.
The second PLACE's value is put into the first PLACE,
the third PLACE's value into the second PLACE, and so on,
and the first PLACE's value is put into the last PLACE."
  (let ((setf-methods
	  (mapcar #'(lambda (place)
		      (multiple-value-list (get-setf-method place)))
		  places)))
    (sublis-eval-once
      (mapcan #'(lambda (setf-method)
		  (pairlis (first setf-method)
			   (second setf-method)))
	      setf-methods)
      `(let (,(car (third (car (last setf-methods))))
	     ,(fifth (car setf-methods)))
	 ,.(loop for i from 1 below (length places)
		 collect (sublis (list (cons (car (third (nth (1- i) setf-methods)))
					     (fifth (nth i setf-methods))))
				 (fourth (nth (1- i) setf-methods))))
	 ,(fourth (car (last setf-methods)))
	 nil)
      t t)))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(defmacro shiftf (&rest places-and-final-value)
  "Copies values into each PLACE from the following one.
The last PLACE need not be SETF'able, as it is only accessed
 to get a value to put in the previous PLACE; it is not set.
The first PLACE's original value is returned as the value of the SHIFTF form."
  (let* ((places (butlast places-and-final-value))
	 (value (car (last places-and-final-value)))
	 (setf-methods
	   (mapcar #'(lambda (place)
		       (multiple-value-list (get-setf-method place)))
		   places)))
    (sublis-eval-once
      (nconc (mapcan #'(lambda (setf-method)
			 (pairlis (first setf-method)
				  (second setf-method)))
		     setf-methods)
	     (list (cons (car (third (car (last setf-methods)))) value)))
      `(prog1
	 ,(fifth (car setf-methods))
	 ,.(loop for i from 1 below (length places)
		 collect (sublis (list (cons (car (third (nth (1- i) setf-methods)))
					     (fifth (nth i setf-methods))))
				 (fourth (nth (1- i) setf-methods))))
	 ,(fourth (car (last setf-methods))))
      t t)))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "


(define-modify-macro incf (&optional (delta 1)) +
		     "Increment PLACE's value by DELTA.")

(define-modify-macro decf (&optional (delta 1)) -
		     "Decrement PLACE's value by DELTA.")

))

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun digit-char (weight &optional (radix 10.) (font 0))
  "Return a character which signifies WEIGHT in radix RADIX, with FONT as specified.
This is always NIL if WEIGHT is  RADIX.
Otherwise, for WEIGHT between 0 and 9, you get characters 0 thru 9;
for higher weights, you get digits."
  (if ( weight radix) nil	;Could the user ever have trouble checking this himself?
    (code-char (if (< weight 10.)
		   (+ #/0 weight)
		 (+ #/A weight -10.))
	       0 font)))

))

; From file CHARACTER.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; CHARACTER  "

(defun char-name (char)
  "Returns the standard name of CHAR, as a string; or NIL if there is none.
For example, /"RETURN/" for the character Return.
Only works for characters which are not GRAPHIC-CHAR-P (unlike /"a/", for example."
  (let ((elt (rassq (dont-optimize (%pointer char)) xr-special-character-names)))
    (if elt (symbol-name (car elt)))))

))

; From file OUTPUT.LISP SRC:<L.IO1> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; OUTPUT  "

(defun ochar (char &optional style top-explain minwidth &rest options
		   &aux chname bits char0 char1)
  "Print the character CHAR in a fancy manner on STANDARD-OUTPUT.
STYLE specifies how to print:
 :READ (the default) means print it with #// or #\ in a way that can be read back in;
 :BRIEF means print the character verbatim if it has no control bits,
  otherwise print its name if it's not a graphic char.
 :EDITOR means always use the name except for graphic chars,
  and use verbose forms of prefixes (/"Meta-/" instead of /"M-/").
 :SAIL means use , , etc as prefixes for the control bits
  and never use names.
 :LOZENGED means put the character name in a lozenge if stream can do so,
  unless the character is a graphic character.
  The padding options should not be used with :LOZENGED.
TOP-EXPLAIN if T means add on an explanation of how to type the character
 on the keyboard, using the Top or Greek key, if appropriate.
If MINWIDTH is specified, we pad on the right to that width.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0)."
  (declare (arglist char &optional style top-explain minwidth
		    &key (pad-char #/ ) (minpad 0) (tab-period 1)))
  (setq char0
	(if (and (consp char) (eq (car char) ':mouse))
	    (third char) (character char)))
  (setq char1 (ldb %%kbd-char char0))
  (cond ((or minwidth options)
	 (lexpr-funcall 'pad1
			(list (output nil (ochar char0 style top-explain)) nil)
			minwidth
			options))
	((eq style ':lozenged)
	 (if (and ( char1 200)
		  (send standard-output ':operation-handled-p ':display-lozenged-string))
	     (send standard-output ':display-lozenged-string
		   (format:output nil (ochar char0 ':editor)))
	   (ochar char0 ':editor))
	 (when top-explain
	   (ochar-explain-top-character char1)))
	((tv:char-mouse-p char0)
	 (cond ((eq style ':read)
		(or (setq chname (ochar-get-character-name char0))
		    (ferror nil "No name known for mouse character ~C" char0))
		(funcall standard-output ':string-out "#\")
		(princ chname))
	       (t (setq bits (ldb %%kbd-control-meta char0))
		  (and (bit-test 8 bits) (funcall standard-output ':string-out "Hyper-"))
		  (and (bit-test 4 bits) (funcall standard-output ':string-out "Super-"))
		  (and (bit-test 1 bits) (funcall standard-output ':string-out "Control-"))
		  (and (bit-test 2 bits) (funcall standard-output ':string-out "Meta-"))
		  (funcall standard-output ':string-out "Mouse-")
		  (funcall standard-output ':string-out (nth (ldb 0003 char0)
							     '("Left" "Middle" "Right")))
		  (if (setq chname (nth (setq bits (ldb 0303 char0))
					'("" "-Twice" "-Thrice")))
		      (funcall standard-output ':string-out chname)
		    (funcall standard-output ':tyo #/-)
		    (english-print (1+ bits))
		    (funcall standard-output ':string-out "-Times")))))
	(t
	  (selectq style
	    (:editor
	      (setq bits (ldb %%kbd-control-meta char0))
	      (and (bit-test 8 bits) (funcall standard-output ':string-out "Hyper-"))
	      (and (bit-test 4 bits) (funcall standard-output ':string-out "Super-"))
	      (and (bit-test 1 bits) (funcall standard-output ':string-out "Control-"))
	      (and (bit-test 2 bits) (funcall standard-output ':string-out "Meta-"))
	      (cond ((setq chname (ochar-get-character-name char1))
		     (let ((str (string-downcase chname)))
		       (aset (char-upcase (aref str 0)) str 0)
		       (funcall standard-output ':string-out str)
		       (return-array str)))
		    ((and (not (zerop bits)) ( #/a char1 #/z))
		     (funcall standard-output ':string-out "Shift-")
		     (funcall standard-output ':tyo (char-upcase char1)))
		    (t (funcall standard-output ':tyo char1))))
	    ((nil :read :brief)
	     (setq bits (ldb %%kbd-control-meta char0))
	     (if (zerop bits)
		 (setq bits nil))
	     ;; In :READ style, get a character name if possible.
	     ;; In :BRIEF style, get one only if there are control bits.
	     (if (or bits (neq style ':brief))
		 (setq chname (ochar-get-character-name char1)))
	     (or (eq style ':brief)
		 (funcall standard-output ':string-out
			  (if (or bits chname) "#\" "#//")))
	     ;; Now announce the control bits.
	     (if bits
		 (funcall standard-output
			  ':string-out
			  (nth bits
			       '("" "c-" "m-" "c-m-"
				 "s-" "c-s-" "m-s-" "c-m-s-"
				 "h-" "c-h-" "m-h-" "c-m-h-"
				 "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
	     ;; If we did get a character's long-name, for whatever reason, use it.
	     (if chname
		 (let ((str (string-downcase chname)))
		   (if (eq style ':brief)
		       (setf (aref str 0) (char-upcase (aref str 0))))
		   (funcall standard-output ':string-out str)
		   (return-array str))
	       ;; Otherwise print the character itself.
	       ;; In :READ style, print a slash before chars that want it.
	       (and (neq style ':brief)
		    bits
		    ;; If using #\ but using the character, not the name, may need a slash.
		    (if ( #/a char1 #/z)
			(funcall standard-output ':string-out "sh-")
		      (if (memq char1
				'(#/, #\space #/( #/) #/' #// #/` #/@ #/; #/: #/" #/| #/#))
			  (tyo (si:pttbl-slash readtable))
			(setq char1 (char-downcase char1)))))
	       (funcall standard-output ':tyo char1)))
	    (:SAIL (funcall standard-output ':string-out (nth (ldb %%kbd-control-meta char0)
							      '("" "" "" ""
								"" "" "" ""
								"" "" "" ""
								"" "" "" "")))
		   (and (memq char1 '(#/ #/ #/ #/ #/ #/))
			(funcall standard-output ':tyo #/))
		   (funcall standard-output ':tyo char1)))
	  (and top-explain 
	       (ochar-explain-top-character char1)))))

))

; From file SETF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro pushnew (value place &rest options)
  "Add ITEM to the front of the list PLACE, if it's not already MEMQ there.
Works by SETF'ing PLACE."
  (declare (arglist value place &key test test-not key))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method place)
    (let ((val (gensym)))
      (sublis-eval-once (cons `(,val . ,value) (pairlis tempvars tempargs))
			`(if ,(if options
				  `(member ,val ,refform . ,options)
				`(memq ,val ,refform))
			     ,refform
			   ,(sublis (list (cons (car storevars)
						`(cons ,val ,refform)))
				    storeform))
			t t))))

))

; From file GENRIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "


(defun nsubst-if (new predicate tree &key &optional key)
  "Destructively replace with NEW every atom or subtree in TREE which satisfies PREDICATE.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (nsubst-1 new nil tree key predicate nil))

(defun nsubst-if-not (new predicate tree &key &optional key)
  "Destructively replace with NEW every atom or subtree in TREE not satisfying PREDICATE.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (nsubst-1 new nil tree key predicate t))

))

; From file GENRIC.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "


(defun subst-if (new predicate tree &key &optional key)
  "Replace with NEW every atom or subtree in TREE which satisfies PREDICATE.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (subst-1 new nil tree key predicate nil))

(defun subst-if-not (new predicate tree &key &optional key)
  "Replace with NEW every atom or subtree in TREE which doesn't satisfy PREDICATE.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (subst-1 new nil tree key predicate t))

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN SUBLIS-1 (ALIST TREE KEY PRED INVERTP DESTRUCTIVEP &AUX TEM)
  (IF (COND ((AND (NULL KEY) (OR (EQ PRED 'EQ) (EQ PRED #'EQ)))
	     (SETQ TEM (ASSQ TREE ALIST)))
	    (T
	     (DOLIST (ELT ALIST)
	       (IF (EQ INVERTP (NULL (FUNCALL PRED (CAR ELT)
					      (IF KEY (FUNCALL KEY TREE) TREE))))
		   (RETURN (SETQ TEM ELT))))))
      (CDR TEM)
    (IF (ATOM TREE) TREE
      (LET ((NEWCAR (SUBLIS-1 ALIST (CAR TREE) KEY PRED INVERTP DESTRUCTIVEP))
	    (NEWCDR (SUBLIS-1 ALIST (CDR TREE) KEY PRED INVERTP DESTRUCTIVEP)))
	(IF (NOT DESTRUCTIVEP)
	    (IF (AND (EQ NEWCAR (CAR TREE))
		     (EQ NEWCDR (CDR TREE)))
		TREE
	      (CONS NEWCAR NEWCDR))
	  (SETF (CAR TREE) NEWCAR)
	  (UNLESS (EQ (CDR TREE) NEWCDR)
	    (SETF (CDR TREE) NEWCDR))
	  TREE)))))

))

; From file QFCTNS.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN SUBLIS (ALIST TREE &KEY &OPTIONAL TEST TEST-NOT KEY)
  "Make multiple replacements in TREE, copying structure as needed.
ALIST specifies the replacements; each element's car is something to replace,
and the cdr is what to replace it with.
Each atom or subtree found anywhere in TREE is compared against each
object to be replaced.  If KEY is non-NIL, it is a function to apply
to that non-list from TREE to get the thing to actually compare.
TEST and TEST-NOT specify how to do comparison.
The value is a predicate which accepts two arguments.
If TEST-NOT is specified, an object is replaced if the predicate returns NIL.
If TEST is specified, an object is replaced if the predicate returns non-NIL."
  (SUBLIS-1 ALIST TREE KEY (OR TEST-NOT TEST 'EQL) (NOT (NULL TEST-NOT)) NIL))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN MAKE-BROADCAST-STREAM (&REST STREAMS)
  "Return an I//O stream which passes all operations to all of the STREAMS.
Thus, output directed to the broadcast stream will go to multiple places."
  (IF (NULL STREAMS) 'NULL-STREAM
    (LET-CLOSED ((BROADCAST-STREAM-STREAMS (COPYLIST STREAMS))
		 (WHICH-OPERATIONS (LOOP WITH WO = (FUNCALL (CAR STREAMS) ':WHICH-OPERATIONS)
					 WITH COPYP = T
					 FOR STREAM IN (CDR STREAMS)
					 DO (LOOP WITH WO2 = (FUNCALL STREAM ':WHICH-OPERATIONS)
						  FOR OP IN WO
						  UNLESS (MEMQ OP WO2)
						    DO (IF COPYP (SETQ WO (COPYLIST WO)))
						       (SETQ COPYP NIL)
						       (SETQ WO (DELQ OP WO)))
					 FINALLY (RETURN WO))))
      (FUNCTION (LAMBDA (&REST ARGS)
		  (COND ((EQ (CAR ARGS) ':WHICH-OPERATIONS) WHICH-OPERATIONS)
			((EQ (CAR ARGS) ':OPERATION-HANDLED-P)
			 (MEMQ (CADR ARGS) WHICH-OPERATIONS))
			((EQ (CAR ARGS) ':SEND-IF-HANDLES)
			 (DO ((L BROADCAST-STREAM-STREAMS (CDR L)))
			     ((NULL (CDR L))	;Last one gets to return multiple values
			      (LEXPR-FUNCALL (CAR L) ':SEND-IF-HANDLES ARGS))
			   (LEXPR-FUNCALL (CAR L) ':SEND-IF-HANDLES ARGS)))
			(T
			 (DO ((L BROADCAST-STREAM-STREAMS (CDR L)))
			     ((NULL (CDR L))	;Last one gets to return multiple values
			      (APPLY (CAR L) ARGS))
			   (APPLY (CAR L) ARGS)))))))))

))

; From file FORMAT.LISP SRC:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN MAKE-STRING-OUTPUT-STREAM (&OPTIONAL STRING START-INDEX EXTRA-ARG)
  "Return a stream that accumulates output in a string.
If STRING is specified, output is STRING-NCONC'd onto it.
Otherwise a new string is created and used;
GET-OUTPUT-STREAM-STRING can be used on the stream to get the accumulated string."
  (IF (STRINGP START-INDEX)
      (LET ((STRING START-INDEX)
	    (START-INDEX EXTRA-ARG))
	(LET-CLOSED ((FORMAT-STRING
		       (OR STRING (MAKE-ARRAY 100 ':TYPE ART-STRING ':FILL-POINTER 0))))
	  (IF START-INDEX
	      (SETF (FILL-POINTER FORMAT-STRING) START-INDEX))
	  'FORMAT-STRING-STREAM))
    (LET-CLOSED ((FORMAT-STRING
		   (OR STRING (MAKE-ARRAY 100 ':TYPE ART-STRING ':FILL-POINTER 0))))
      (IF START-INDEX
	  (SETF (FILL-POINTER FORMAT-STRING) START-INDEX))
      'FORMAT-STRING-STREAM)))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#/(-MACRO (STREAM IGNORE
		     &OPTIONAL (LENGTH (UNLESS *READ-SUPPRESS* XR-SHARP-ARGUMENT)))
  (XR-XRUNTYI STREAM #/( 0)
  (LET* ((ELEMENTS (INTERNAL-READ STREAM T NIL T))
	 (VECTOR (MAKE-SEQUENCE 'VECTOR (OR LENGTH (LENGTH ELEMENTS))
				:INITIAL-ELEMENT (CAR (LAST ELEMENTS)))))
    (IF (AND LENGTH (PLUSP LENGTH) (NULL ELEMENTS))
	(CERROR ':NO-ACTION NIL 'READ-ERROR-1
		"The construct #~D() is illegal; one element is needed."
		LENGTH))
    (IF (< (LENGTH VECTOR) (LENGTH ELEMENTS))
	(CERROR ':NO-ACTION NIL 'READ-ERROR-1
		"Elements specified are more than the specified length in #(..) vector construct."))
    (REPLACE VECTOR ELEMENTS)
    VECTOR))

))

; From file READ.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; READ  "

(DEFUN XR-#*-MACRO (STREAM IGNORE &OPTIONAL (LENGTH XR-SHARP-ARGUMENT)
		    &AUX BIT-VECTOR LAST-ELEMENT-READ)
  (IF *READ-SUPPRESS*
      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
    (SETQ BIT-VECTOR (MAKE-ARRAY (OR LENGTH 10) ':TYPE 'ART-1B ':LEADER-LIST '(0)))
    (DO (CHAR INDEX ERROR-REPORTED) (())
      (SETF (VALUES CHAR INDEX) (XR-XRTYI STREAM NIL T))
      (SELECTOR CHAR CHAR-EQUAL
	((#/0 #/1)
	 (SETQ LAST-ELEMENT-READ (- CHAR #/0))
	 (IF LENGTH
	     (UNLESS (OR (ARRAY-PUSH BIT-VECTOR LAST-ELEMENT-READ)
			 ERROR-REPORTED)
	       (CERROR ':NO-ACTION NIL 'READ-ERROR-1
		       "Number of data bits exceeds specified length in #* bit vector construct.")
	       (SETQ ERROR-REPORTED T))
	   (ARRAY-PUSH-EXTEND BIT-VECTOR LAST-ELEMENT-READ)))
	(T
	 (IF (AND LENGTH (PLUSP LENGTH) (ZEROP (FILL-POINTER BIT-VECTOR)))
	     (CERROR ':NO-ACTION NIL 'READ-ERROR-1
		     "The construct #~D* is illegal; at least one bit must be given."
		     LENGTH))
	 (AND LENGTH
	      ;; ARRAY-PUSH returns () when the fill pointer is at the end of the
	      ;; array.
	      (LOOP WHILE (ARRAY-PUSH BIT-VECTOR LAST-ELEMENT-READ)))
	 (XR-XRUNTYI STREAM CHAR INDEX)
	 (LET ((NVEC (MAKE-ARRAY (LENGTH BIT-VECTOR) ':TYPE ART-1B)))
	   (COPY-ARRAY-CONTENTS BIT-VECTOR NVEC)
	   (RETURN NVEC)))))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "


(DEFUN WRITE-CHAR (CHAR &OPTIONAL (STREAM STANDARD-OUTPUT))
  "Output CHAR to STREAM.  Returns CHAR."
  (FUNCALL (DECODE-PRINT-ARG STREAM) ':TYO (IF (CHARACTERP CHAR) (%POINTER CHAR) CHAR))
  CHAR)  

(DEFUN WRITE-BYTE (BYTE &OPTIONAL (STREAM STANDARD-OUTPUT))
  "Output BYTE to STREAM.  Returns BYTE."
  (FUNCALL (DECODE-PRINT-ARG STREAM) ':TYO BYTE)
  BYTE)  

))

; From file FORMAT.LISP SRC:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-PARSE-COMMAND (ARGS SWALLOW-ARGS)
  (DO ((PARAM-FLAG NIL)		;If T, a parameter has been started in PARAM
       (START CTL-INDEX)	;for error message
       (CH)
       (TEM)
       (SYM)
       (SIGN NIL)		;Sign of parameter currently being constructed.
       (PARAM NIL))		;PARAM is the parameter currently being constructed
      ((>= (SETQ CTL-INDEX (1+ CTL-INDEX)) CTL-LENGTH)
       (SETQ CTL-INDEX (1+ START))
       (FORMAT-ERROR "Command fell off end of control string"))
    (SETQ CH (AREF CTL-STRING CTL-INDEX))
    (AND (>= CH #/a) (<= CH #/z) (SETQ CH (- CH 40))) ;lower-case to upper
    (COND ((AND (>= CH #/0) (<= CH #/9))
	   (SETQ PARAM (+ (* (OR PARAM 0) 10.) (- CH #/0))
		 PARAM-FLAG T))
	  ((= CH #/-)
	   (SETQ SIGN (NOT SIGN)))
	  ((= CH #/+) NIL)
	  ((= CH #/@)
	   (SETQ ATSIGN-FLAG T))
	  ((= CH #/:)
	   (SETQ COLON-FLAG T))
	  ((= CH #/V)
	   (COND ((AND (NULL ARGS) SWALLOW-ARGS)
		  (SETQ CTL-INDEX (1+ CTL-INDEX))
		  (FORMAT-ERROR "No argument for V parameter to use")))
	   (SETQ PARAM (POP ARGS) PARAM-FLAG T))
	  ((= CH #/#)
	   (SETQ PARAM (LENGTH ARGS) PARAM-FLAG T))
	  ((= CH #/')
	   (SETQ PARAM (AREF CTL-STRING (SETQ CTL-INDEX (1+ CTL-INDEX))) PARAM-FLAG T))
	  ((= CH #/,)	;comma, begin another parameter
	   (AND SIGN PARAM (SETQ PARAM (- PARAM)))
	   (ARRAY-PUSH FORMAT-PARAMS PARAM)
	   (SETQ PARAM NIL PARAM-FLAG T SIGN NIL))  ;omitted arguments made manifest by the
					   ;presence of a comma come through as NIL
	  ((= CH #\CR)		;No command, just ignoring a CR
	   (SETQ CTL-INDEX (1+ CTL-INDEX))	;Skip the newline
	   (OR COLON-FLAG	;Unless colon, skip whitespace on the next line
	       (DO () ((OR (>= CTL-INDEX CTL-LENGTH)
			   (NOT (MEMQ (AREF CTL-STRING CTL-INDEX) '(#\SP #\TAB)))))
		 (SETQ CTL-INDEX (1+ CTL-INDEX))))
	   (RETURN 'CRLF ARGS))
	  (T			       ;Must be a command character
	      (SETQ CTL-INDEX (1+ CTL-INDEX))	;Advance past command character
	      (AND SIGN PARAM (SETQ PARAM (- PARAM)))
	      (AND PARAM-FLAG (ARRAY-PUSH FORMAT-PARAMS PARAM))
	      (SETQ PARAM-FLAG NIL PARAM NIL TEM NIL)
	      ;; SYM gets the symbol for the operation to be performed.
	      ;; If SYM is NIL (and maybe otherwise), TEM gets a string
	      ;; which is the operation name as found in the control string.
	      (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		   (COND ((= CH #/\)
			  (LET ((I (STRING-SEARCH-CHAR #/\ CTL-STRING (1+ CTL-INDEX))))
			       (AND (NULL I)
				    (FORMAT-ERROR "Unmatched \ in control string."))
			       (SETQ TEM (NSUBSTRING CTL-STRING CTL-INDEX I))
			       (SETQ CTL-INDEX (1+ I))
			       (LET ((*PACKAGE* FORMAT-PACKAGE)
				     (L (LENGTH TEM)))
				 (IF (OR (%STRING-SEARCH-CHAR #/: TEM 0 L)
					 (%STRING-SEARCH-CHAR #/| TEM 0 L)
					 (%STRING-SEARCH-CHAR #// TEM 0 L))
				     (SETQ SYM (READ-FROM-STRING TEM))
				   (SETQ SYM (INTERN-SOFT (STRING-UPCASE TEM)
							  FORMAT-PACKAGE))))))
			 (T (SETQ SYM (OR (AREF FORMAT-CHAR-TABLE CH)
					  (PROG2 (SETQ TEM (STRING CH))
						 (INTERN-SOFT TEM FORMAT-PACKAGE)
						 (RETURN-ARRAY TEM)))))))
	      (RETURN SYM ARGS)))))

))

; From file FORMAT.LISP SRC:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-HAIRY-E-FORMAT (ORIGINAL-ARG PARAMS)
  (AND (RATIONALP ORIGINAL-ARG) (SETQ ORIGINAL-ARG (FLOAT ORIGINAL-ARG)))
  (IF (NOT (FLOATP ORIGINAL-ARG))
      (FORMAT-CTL-DECIMAL ORIGINAL-ARG (LIST (CAR PARAMS)))
    (PROG* ((WIDTH (CAR PARAMS))
	    (AFTER-DECIMAL (CADR PARAMS))
	    (EXPONENT-DIGITS (THIRD PARAMS))
	    (SCALE (OR (FOURTH PARAMS) 1))
	    (OVERFLOWCHAR (FIFTH PARAMS))
	    (PADCHAR (SIXTH PARAMS))
	    (EXPONENTCHAR (SEVENTH PARAMS))
	    (NEGATIVE (MINUSP ORIGINAL-ARG))
	    WIDTH-AFTER-SIGN-AND-EXPONENT
	    EXPONENT
	    EXTRA-ZERO
	    ARG)
	RETRY
	   (SETF (VALUES ARG EXPONENT) (SI:SCALE-FLONUM (ABS ORIGINAL-ARG)))
	   ;; If user does not specify number of exponent digits, guess.
	   (UNLESS EXPONENT-DIGITS
	     (SETQ EXPONENT-DIGITS
		   (COND ((> (ABS EXPONENT) 99.) 3)
			 ((> (ABS EXPONENT) 9) 2)
			 (T 1))))
	   (SETQ WIDTH-AFTER-SIGN-AND-EXPONENT
		 (AND WIDTH
		      (- (IF (OR NEGATIVE ATSIGN-FLAG) (- WIDTH 1) WIDTH)
			 EXPONENT-DIGITS 2)))
	   (MULTIPLE-VALUE-BIND (BUFER DECIMAL-PLACE)
	       (SI:FLONUM-TO-STRING ARG (SMALL-FLOATP ARG)
				    (AND WIDTH (1- WIDTH-AFTER-SIGN-AND-EXPONENT))
				    (AND AFTER-DECIMAL
					 (IF (PLUSP SCALE)
					     AFTER-DECIMAL
					   (1- AFTER-DECIMAL))))
	     ;; Correct "10.0", caused by carry, into "1.0"
	     (WHEN (= DECIMAL-PLACE 2)
	       (SETF (AREF BUFER 2) (AREF BUFER 1))
	       (SETF (AREF BUFER 1) #/.)
	       (IF (= (AREF BUFER (1- (LENGTH BUFER))) #/0)
		   (DECF (FILL-POINTER BUFER)))
	       (DECF DECIMAL-PLACE)
	       (INCF EXPONENT))
	     (DECF EXPONENT (- SCALE 1))
	     (SETQ EXTRA-ZERO (AND ( SCALE 0)
				   (> WIDTH-AFTER-SIGN-AND-EXPONENT (LENGTH BUFER))))
	     (WHEN WIDTH
	       (WHEN (AND OVERFLOWCHAR
			  (OR (> (LENGTH BUFER) WIDTH-AFTER-SIGN-AND-EXPONENT)
			      (AND (THIRD PARAMS)
				   ( (ABS EXPONENT)
				      (EXPT 10. EXPONENT-DIGITS)))))
		 ;; Does not fit in specified width => print overflow chars.
		 ;; Do not bomb out on an exponent that doesn't fit
		 ;; unless the number of exponent digits was explicitly specified.
		 (RETURN
		   (DOTIMES (I WIDTH)
		     (SEND STANDARD-OUTPUT ':TYO OVERFLOWCHAR))))
	       ;; If exponent needs extra digits but we aren't bombing out,
	       ;; allocate more space to exponent and try again.
	       ;; This way we try to stay within the specified field width
	       ;; by taking away from other things.
	       (DO ((I 1 (1+ I))
		    (X 10. (* X 10.)))
		   ((> X (ABS EXPONENT))
		    (WHEN (> I EXPONENT-DIGITS)
		      (SETQ EXPONENT-DIGITS I)
		      (GO RETRY))))
	       ;; Space left over => print padding.
	       (DOTIMES (I (- WIDTH-AFTER-SIGN-AND-EXPONENT (LENGTH BUFER)
			      (IF EXTRA-ZERO 1 0)))
		 (SEND STANDARD-OUTPUT ':TYO (OR PADCHAR #\SP))))
	     (COND (NEGATIVE (SEND STANDARD-OUTPUT ':TYO #/-))
		   (ATSIGN-FLAG (SEND STANDARD-OUTPUT ':TYO #/+)))
	     (WHEN EXTRA-ZERO
	       (SEND STANDARD-OUTPUT ':TYO #/0))
	     (WHEN (MINUSP SCALE)
	       (SEND STANDARD-OUTPUT ':TYO (SI:PTTBL-DECIMAL-POINT READTABLE))
	       (DOTIMES (I (- SCALE))
		 (SEND STANDARD-OUTPUT ':TYO #/0))
	       (DECF (FILL-POINTER BUFER) (- SCALE)))
	     (DOTIMES (I (1- (LENGTH BUFER)))
	       (WHEN (= I SCALE)
		 (SEND STANDARD-OUTPUT ':TYO (SI:PTTBL-DECIMAL-POINT READTABLE)))
	       (SEND STANDARD-OUTPUT ':TYO
		     (AREF BUFER (IF ( I DECIMAL-PLACE) (1+ I) I))))
	     (SEND STANDARD-OUTPUT ':TYO
		   (OR EXPONENTCHAR
		       (COND ((EQ (NOT (SMALL-FLOATP ARG))
				  (NEQ *READ-DEFAULT-FLOAT-FORMAT* 'SHORT-FLOAT))
			      #/e)
			     ((SMALL-FLOATP ARG) #/s)
			     (T #/f))))
	     (SEND STANDARD-OUTPUT ':TYO
		   (IF (MINUSP EXPONENT) #/- #/+))
	     (LET (ATSIGN-FLAG COLON-FLAG)
	       (FORMAT-CTL-DECIMAL (ABS EXPONENT) (LIST EXPONENT-DIGITS #/0)))
	     (RETURN-ARRAY (PROG1 BUFER (SETQ BUFER NIL)))))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "


(DEFUN PARSE-NAMESTRING (THING &OPTIONAL WITH-RESPECT-TO (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
			 &KEY (START 0) END JUNK-ALLOWED)
  "Parse THING into a pathname and return it.
The same as FS:PARSE-PATHNAME except that that function's args are all positional."
  (PARSE-PATHNAME THING WITH-RESPECT-TO DEFAULTS START END JUNK-ALLOWED))

(DEFUN PARSE-PATHNAME (THING &OPTIONAL WITH-RESPECT-TO (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
		       (START 0) END JUNK-ALLOWED)
  "Parse THING into a pathname and return it.
THING can be a pathname already (it is just passed back),
 a string or symbol, or a Maclisp-style namelist.
WITH-RESPECT-TO can be NIL or a host or host-name;
 if it is not NIL, the pathname is parsed for that host
 and it is an error if the pathname specifies a different host.
If WITH-RESPECT-TO is NIL, then DEFAULTS is used to get the host
 if none is specified.  DEFAULTS may be a host object in this case.
START and END are indices specifying a substring of THING to be parsed.
 They default to 0 for START and NIL (meaning end of THING) for END.
If JUNK-ALLOWED is non-NIL, parsing stops without error if
 the syntax is invalid, and this function returns NIL.
The second value is the index in THING at which parsing stopped.
 If JUNK-ALLOWED is T and there was invalid syntax,
 this is the index of the invalid character."
  (DECLARE (VALUES PARSED-PATHNAME PARSE-END-INDEX))
  (AND WITH-RESPECT-TO
       (SETQ WITH-RESPECT-TO (GET-PATHNAME-HOST WITH-RESPECT-TO)))
  (CONDITION-RESUME '((PATHNAME-ERROR) :NEW-PATHNAME T ("Proceed, supplying a new pathname.")
		      PARSE-PATHNAME-THROW-NEW-PATHNAME)
    (LET ((PARSE-PATHNAME-FLAG JUNK-ALLOWED))
      (CATCH-CONTINUATION 'PARSE-PATHNAME
	  #'(LAMBDA (INDEX-OR-PATHNAME) 
	      (IF (NUMBERP INDEX-OR-PATHNAME)
		  (VALUES NIL (MIN (OR END (LENGTH THING)) INDEX-OR-PATHNAME))
		(VALUES INDEX-OR-PATHNAME START)))
	  NIL
	(COND ((TYPEP THING 'PATHNAME)
	       (AND WITH-RESPECT-TO (NEQ WITH-RESPECT-TO (PATHNAME-HOST THING))
		    (FERROR 'PATHNAME-PARSE-ERROR
			    "Host ~A in ~A does not match ~A"
			    (PATHNAME-HOST THING) THING WITH-RESPECT-TO))
	       (VALUES THING START))
	      ((CONSP THING)
	       (SETQ THING (CANONICALIZE-KLUDGEY-MACLISP-PATHNAME-STRING-LIST THING))
	       (LET (DEVICE DIRECTORY NAME TYPE VERSION HOST)
		 (COND ((LISTP (CAR THING))
			(SETF `((,DEVICE ,DIRECTORY) ,NAME ,TYPE ,VERSION) THING))
		       ((NUMBERP (THIRD THING))
			(SETF `(,NAME ,TYPE ,VERSION ,DEVICE ,DIRECTORY) THING))
		       (T
			(SETF `(,NAME ,TYPE ,DEVICE ,DIRECTORY ,VERSION) THING)))
		 (SETQ HOST (COND ((GET-PATHNAME-HOST DEVICE T))
				  (WITH-RESPECT-TO)
				  ((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
				  (T (DEFAULT-HOST DEFAULTS))))
		 (AND WITH-RESPECT-TO
		      (NEQ WITH-RESPECT-TO HOST)
		      (FERROR 'PATHNAME-PARSE-ERROR
			      "Host ~A in ~A does not match ~A" HOST THING WITH-RESPECT-TO))
		 (VALUES (MAKE-PATHNAME ':HOST HOST
					':DEVICE DEVICE ':DIRECTORY DIRECTORY ':NAME NAME
					':TYPE TYPE ':VERSION VERSION)
			 START)))
	      (T
	       (SETQ THING (STRING THING))
	       (MULTIPLE-VALUE-BIND (HOST-SPECIFIED START END)
		   (PARSE-PATHNAME-FIND-COLON THING START END)
		 ;; If the thing before the colon is really a host,
		 ;; and WITH-RESPECT-TO was specified, then they had better match
		 (AND WITH-RESPECT-TO
		      HOST-SPECIFIED
		      (NEQ WITH-RESPECT-TO HOST-SPECIFIED)
		      ;; Otherwise treat it as a device name
		      (SETQ HOST-SPECIFIED NIL START 0 END NIL))
		 (LET* ((HOST
			  (COND ((GET-PATHNAME-HOST HOST-SPECIFIED T))
				(WITH-RESPECT-TO)
				((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
				(T (DEFAULT-HOST DEFAULTS)))))
		   (MULTIPLE-VALUE-BIND (DEVICE DIRECTORY NAME TYPE VERSION PARSE-END)
		       (FUNCALL (SAMPLE-PATHNAME HOST) ':PARSE-NAMESTRING
				(NOT (NULL HOST-SPECIFIED)) THING START END)
		     (VALUES
		       ;; If device is :NO-INTERN then immeditely return 2nd value, DIRECTORY.
		       ;; this provides a way to bypass as much of this lossage as possible
		       ;; in cases where it doesnt make sense.
		       (COND ((EQ DEVICE ':NO-INTERN)
			      DIRECTORY)
			     (T
			      ;; Otherwise we assume we got the raw forms of everything.
			      (MAKE-PATHNAME-INTERNAL
				HOST DEVICE DIRECTORY NAME TYPE VERSION)))
		       PARSE-END))))))))))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "

(DEFUN MAKE-PATHNAME (&REST OPTIONS
		      &OPTIONAL &KEY
		      (DEFAULTS T)
		      (HOST (IF (EQ DEFAULTS T)
				(DEFAULT-HOST *DEFAULT-PATHNAME-DEFAULTS*)
			      (DEFAULT-HOST DEFAULTS)))
		      &ALLOW-OTHER-KEYS)
  "Create a pathname, specifying components as keyword arguments.
If DEFAULTS is a pathname or a defaults list, the pathname is defaulted from it.
If DEFAULTS is T (the default), the host is defaulted from
*DEFAULT-PATHNAME-DEFAULTS* and the other components are not defaulted at all."
  (DECLARE (ARGLIST &KEY &OPTIONAL (DEFAULTS T)
		    HOST DEVICE RAW-DEVICE DIRECTORY RAW-DIRECTORY
		    NAME RAW-NAME TYPE RAW-TYPE VERSION
		    CANONICAL-TYPE ORIGINAL-TYPE))
  (IF (NOT (SYMBOLP DEFAULTS))
      (MERGE-PATHNAME-DEFAULTS
	(LEXPR-FUNCALL (SAMPLE-PATHNAME HOST)
		       ':NEW-PATHNAME OPTIONS)
	DEFAULTS)
    (LEXPR-FUNCALL (SAMPLE-PATHNAME HOST)
		   ':NEW-PATHNAME OPTIONS)))

))

; From file PROCES.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "

(DEFUN PROCESS-TOP-LEVEL (&OPTIONAL IGNORE)
  (LET ((TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))
    (DO-FOREVER
      (CATCH-ERROR-RESTART (CONDITION "Reset and arrest process ~A."
				      (SEND CURRENT-PROCESS ':NAME))
	(UNWIND-PROTECT
	    (ERROR-RESTART ((SYS:ABORT CONDITION) "Restart process ~A."
			    (SEND CURRENT-PROCESS ':NAME))
	      (APPLY (CAR (PROCESS-INITIAL-FORM CURRENT-PROCESS))
		     (CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS)))
	      (PROCESS-FLUSH-BACKGROUND-STREAM)
	      (PROCESS-WAIT-FOREVER))
	  (PROCESS-FLUSH-BACKGROUND-STREAM)))
      (SEND CURRENT-PROCESS ':ARREST-REASON ':USER)
      (PROCESS-ALLOW-SCHEDULE))))

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO ASSERT (TEST-FORM &OPTIONAL PLACES (FORMAT-STRING "Assertion failed.") &REST ARGS)
  "Signals an error if TEST-FORM evals to NIL.
PLACES are SETF'able things that the user should be able to change when proceeding.
Typically they are things used in TEST-FORM.
Each one becomes a proceed-type which means to set that place.
FORMAT-STRING and ARGS are passed to FORMAT to make the error message."
  (DECLARE (ARGLIST TEST-FORM &OPTIONAL PLACES FORMAT-STRING &REST ARGS))
  `(DO () (,TEST-FORM)
     (SIGNAL-PROCEED-CASE ((VALUE) 'EH:FAILED-ASSERTION
				   ':PLACES ',PLACES
				   ':FORMAT-STRING ,FORMAT-STRING
				   ':FORMAT-ARGS (LIST . ,ARGS))
       . ,(MAPCAR #'(LAMBDA (PLACE)
		      `((,PLACE) (SETF ,PLACE VALUE)))
		  PLACES))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN FILE-POSITION (FILE-STREAM &OPTIONAL NEW-POSITION)
  "Return or set the stream pointer of FILE-STREAM.
With one argument, return the stream pointer of FILE-STREAM, or NIL if not known.
With two argumens, try set the stream pointer to NEW-POSITION
 and return T if it was really possible to do so."
  (IF NEW-POSITION
      (UNLESS (AND (NOT (MEMQ NEW-POSITION '(:START 0)))
		   (SEND FILE-STREAM ':CHARACTERS))
	(SEND FILE-STREAM ':SET-POINTER
	      (SELECTQ NEW-POSITION
		(:START 0)
		(:END (SEND FILE-STREAM ':LENGTH-IN-BYTES))
		(T NEW-POSITION)))
	T)
    (LET ((PTR (SEND FILE-STREAM ':SEND-IF-HANDLES ':READ-POINTER)))
      (UNLESS (AND (NOT (MEMQ PTR '(:START 0)))
		   (SEND FILE-STREAM ':CHARACTERS))
	PTR))))

))

; From file NPRIM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "

(DEFUN SECTIONIZE-FILE-BUFFER (BUFFER &OPTIONAL AARRAY PROPERTY START-NODE END-NODE
			       STREAM HACK-FONTS
                                 &AUX (PACKAGE PACKAGE)
				 (NODE-LIST NIL)
				 (MODE (SEND BUFFER ':MAJOR-MODE))
				 (*INTERVAL* BUFFER) INT-STREAM
				 FIRST-BP LAST-BP ADDED-COMPLETIONS
				 BUFFER-TICK OLD-CHANGED-SECTIONS
				 NODES-TO-REUSE ACTUAL-NEW-NODES
				 START-PREDECESSOR END-FOLLOWER)
  "Compute the sectionization of all or part of BUFFER.
If START-NODE and END-NODE are NIL, the whole buffer is resectionized from scratch.
If they are non-NIL, they should be sections of the buffer;
that portion of the buffer (inclusive!) is resectionized,
reusing any existing nodes if objects with the same names are still present."
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  (SETQ FIRST-BP (INTERVAL-FIRST-BP (OR START-NODE BUFFER))
	LAST-BP (IF END-NODE (COPY-BP (INTERVAL-LAST-BP END-NODE))
		  (INTERVAL-LAST-BP BUFFER)))
  ;; If operating on a specified range of sections,
  ;; from START-NODE to END-NODE inclusive,
  ;; put all those nodes on NODES-TO-REUSE.
  (IF START-NODE
      (DO ((N START-NODE (NODE-NEXT N)))
	  (())
	(PUSH N NODES-TO-REUSE)
	(IF (EQ N END-NODE) (RETURN))))
  (SETQ END-FOLLOWER (IF END-NODE (NODE-NEXT END-NODE))
	START-PREDECESSOR (IF START-NODE (NODE-PREVIOUS START-NODE)))
  ;;Buffer must be a FILE-BUFFER, but need not be a real ZMACS BUFFER.
  (COND (AARRAY
	 (SETQ ADDED-COMPLETIONS (MAKE-ARRAY 100 ':TYPE 'ART-Q-LIST
					         ':LEADER-LENGTH 2 ':LEADER-LIST '(0)))))
  (AND STREAM
       (SETQ INT-STREAM (INTERVAL-STREAM-INTO-BP LAST-BP HACK-FONTS)))
  ;; Make sure the buffer ends with an empty line.
;  (OR (ZEROP (BP-INDEX LAST-BP))
;      (INSERT LAST-BP #\CR))
  (SETQ BUFFER-TICK (BUFFER-TICK BUFFER))
  (TICK)
;;; This is no longer needed for computing the NODE-TICK of sections,
;;; since that can be determined from the text.
;;; But it is still useful for remembering the compile-ticks.
  (OR NODES-TO-REUSE
      (DOLIST (NODE (NODE-INFERIORS BUFFER))
	(PUSH (CONS (SECTION-NODE-NAME NODE)
		    NODE)
	      OLD-CHANGED-SECTIONS)))
  ;; Now scan the buffer and record the definitions.
  (DO* ((LINE (BP-LINE FIRST-BP) (LINE-NEXT LINE))
	(LIMIT (IF (ZEROP (BP-INDEX LAST-BP))	;Line to stop at (may be NIL)
		   (BP-LINE LAST-BP)
		 (LINE-NEXT (BP-LINE LAST-BP))))
	(EOFFLG)
	(BP (COPY-BP FIRST-BP))
	(PREV-NODE-START-BP FIRST-BP)
	(PREV-NODE-DEFUN-LINE NIL)
	(FIRST-NODE-NAME (IF START-NODE "Things deleted" "Buffer header"))
	(PREVIOUS-NODE NIL)
	(ADD-SECTIONS (GET MODE 'EDITING-TYPE))
	(SECTION-P (GET ADD-SECTIONS 'SECTION-P))
	(SECTION-NAME-FUNCTION (GET ADD-SECTIONS 'GET-SECTION-NAME)))
      (NIL)
    ;; If we have a stream, and we are at the limit, read another line.
    (COND ((AND STREAM (EQ LINE LIMIT) (NOT EOFFLG))
	   (MULTIPLE-VALUE (LINE EOFFLG)
			   (FUNCALL STREAM ':LINE-IN LINE-LEADER-SIZE))
	   (IF LINE (SETQ LINE (FUNCALL INT-STREAM ':LINE-OUT LINE)))
	   (SETQ LIMIT (LINE-NEXT LINE))))
    ;; See if the line is the start of a defun.
    ;; If so, record the section that it terminates.
    (WHEN (AND ADD-SECTIONS
	       (OR EOFFLG
		   (EQ LINE LIMIT)
		   (AND LINE (FUNCALL SECTION-P LINE))))
      (LET ((START PREV-NODE-START-BP)
	    END OLD-NODE)
	(IF (OR EOFFLG (EQ LINE LIMIT))
	    (SETQ END LAST-BP)
	  (MOVE-BP BP LINE 0)
	  (SETQ END (COPY-BP BP))
	  ;; Include one blank line before the form in the same section with it.
	  (IF (AND (LINE-PREVIOUS (BP-LINE END))
		   (LINE-BLANK-P (LINE-PREVIOUS (BP-LINE END))))
	      (MOVE-BP END (LINE-PREVIOUS (BP-LINE END)) 0))
	  (SETQ PREV-NODE-START-BP END))
	(UNLESS (AND (NOT (OR EOFFLG (EQ LINE LIMIT)))
		     (OR (BP-= START END)
			 (AND (NOT PREV-NODE-DEFUN-LINE)
			      START-NODE
			      (NOT (EQ START-NODE (CAR (NODE-INFERIORS BUFFER)))))))
	  ;; Now we have decided for certain to create a section ending here.
	  ;; Extract the name of the section that is just being terminated.
	  ;; By now, all the lines that the name runs over must have been read in.
	  (MULTIPLE-VALUE-BIND (SYM STR ERR)
	      (IF PREV-NODE-DEFUN-LINE
		  (FUNCALL SECTION-NAME-FUNCTION PREV-NODE-DEFUN-LINE BP)
		FIRST-NODE-NAME)
	    (WHEN ERR
	      (SETQ SYM "Unknown" STR NIL))
	    (UNLESS ERR
	      (SETQ OLD-NODE (CDR (ASSOC SYM OLD-CHANGED-SECTIONS))))
	    (SETQ PREVIOUS-NODE
		  (ADD-SECTION-NODE START END
				    SYM PREV-NODE-DEFUN-LINE BUFFER PREVIOUS-NODE
				    NIL
;				    (IF OLD-NODE
;					(NODE-TICK OLD-NODE)
;				      (IF STREAM BUFFER-TICK *TICK*))
				    (IF OLD-NODE
					(SECTION-NODE-COMPILE-TICK OLD-NODE)
				      BUFFER-TICK)
				    NODES-TO-REUSE))
	    (IF (MEMQ PREVIOUS-NODE NODES-TO-REUSE)
		(SETQ NODES-TO-REUSE (DELQ PREVIOUS-NODE NODES-TO-REUSE))
	      (PUSH PREVIOUS-NODE ACTUAL-NEW-NODES)
	      (WHEN (AND ADDED-COMPLETIONS (NOT (STRINGP SYM)))
		(SECTION-COMPLETION SYM STR ADDED-COMPLETIONS)
		(UNLESS (SYMBOLP SYM)
		  (SECTION-COMPLETION SYM
				      (DEFINITION-NAME-AS-STRING NIL SYM)
				      ADDED-COMPLETIONS))))
	    (PUSH PREVIOUS-NODE NODE-LIST))))
      (SETQ PREV-NODE-DEFUN-LINE LINE))
    ;; After processing the last line, exit.
    (COND ((OR EOFFLG (EQ LINE LIMIT))
	   (RETURN))))
  ;; If reading a stream, we should not have inserted a CR
  ;; after the eof line.
  (AND STREAM
       (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP BUFFER) -1 T)
			(INTERVAL-LAST-BP BUFFER)
			T))
  ;; Splice the nodes just made in with the nodes
  ;; before START-NODE and after END-NODE
  (LET ((FIRST-NEW-NODE (CAR (LAST NODE-LIST)))
	(FLUSHED-NODES (IF START-NODE NODES-TO-REUSE (NODE-INFERIORS BUFFER))))
    (COND (NODE-LIST
	   (WHEN END-FOLLOWER
	     (SETF (NODE-PREVIOUS END-FOLLOWER) (CAR NODE-LIST))
	     (SETF (NODE-NEXT (CAR NODE-LIST)) END-FOLLOWER))
	   (WHEN START-PREDECESSOR
	     (SETF (NODE-NEXT START-PREDECESSOR) FIRST-NEW-NODE)
	     (SETF (NODE-PREVIOUS FIRST-NEW-NODE) START-PREDECESSOR)))
	  ((AND START-PREDECESSOR END-FOLLOWER)
	   (SETF (NODE-NEXT START-PREDECESSOR) END-FOLLOWER)
	   (SETF (NODE-PREVIOUS END-FOLLOWER) START-PREDECESSOR)))
    ;; Construct the new list of all inferiors of BUFFER.
    ;; Except: if all old nodes were reused, and no new ones made,
    ;; these lists are both still correct.
    (IF (OR FLUSHED-NODES ACTUAL-NEW-NODES)
	(LET (ALL-NODES)
	  (DO ((N (IF END-FOLLOWER (CAR (LAST (NODE-INFERIORS BUFFER)))
		    (CAR NODE-LIST))
		  (NODE-PREVIOUS N)))
	      ((NULL N))
	    (PUSH N ALL-NODES))
	  (SETF (NODE-INFERIORS BUFFER) ALL-NODES)))
    ;; Flush old section nodes that were not reused.
    (DOLIST (NODE FLUSHED-NODES)
      ;; Flush ZMACS-BUFFERS properties for old nodes not reused.
      (WHEN PROPERTY
	(LET ((THE-BUFFER BUFFER)
	      (SYM (SECTION-NODE-NAME NODE)))
	  (OR (STRINGP SYM)
	      (CONDITION-CASE ()
		  (SI:FUNCTION-SPEC-PUTPROP
		   SYM
		   (DEL-IF #'(LAMBDA (DEFN) (EQ (CAR DEFN) THE-BUFFER))
			   (SI:FUNCTION-SPEC-GET SYM PROPERTY))
		   PROPERTY)
		(SYS:INVALID-FUNCTION-SPEC NIL)))))
      (FLUSH-BP (INTERVAL-FIRST-BP NODE))
      (FLUSH-BP (INTERVAL-LAST-BP NODE)))
    ;; Attach ZMACS-BUFFERS properties to the symbols defined herein.
    (WHEN PROPERTY
      (DOLIST (NODE ACTUAL-NEW-NODES)
	(UNLESS (STRINGP (SECTION-NODE-NAME NODE))
	  (CONDITION-CASE ()
	      (SI:FUNCTION-SPEC-PUSH-PROPERTY 
	       (SECTION-NODE-NAME NODE)
	       (CONS BUFFER (SECTION-NODE-DEFUN-LINE NODE))
	       PROPERTY)
	    (SYS:INVALID-FUNCTION-SPEC NIL))))))
  ;; Merge new entries into the aarray.
  (COND (ADDED-COMPLETIONS
	 ;; Copy all the completion entries now, so they all go on one page.
	 (LET ((I (ARRAY-LEADER ADDED-COMPLETIONS 0)))
	   (UNLESS (ZEROP I)
	     (DOTIMES (J I)
	       (SETF (AREF ADDED-COMPLETIONS J)
		     (CONS (STRING-APPEND (CAR (AREF ADDED-COMPLETIONS J)))
			   (CDR (AREF ADDED-COMPLETIONS J)))))
	     ;; Sort them and merge them into the main list.
	     (SORT-COMPLETION-AARRAY ADDED-COMPLETIONS)
	     (MERGE-COMPLETION-AARRAY AARRAY ADDED-COMPLETIONS))))))

))

; From file NPRIM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "


(DEFUN SECTION-SECTIONIZATION-VALID-P (NODE)
  "T if section NODE does not need resectionization to be valid.
In other words, T if you can assume that the boundaries of NODE
do correctly delimit the definition of the object NODE claims to hold."
  (OR ( (SECTION-NODE-SECTIONIZE-TICK NODE)
	 (NODE-TICK NODE))
      ;; If the section breaks around this section are clean,
      ;; and the only section-starting line
      ;; is the one that started this section,
      ;; and that line is unchanged, then we win.
      (AND (OR (NULL (NODE-PREVIOUS NODE))
	       (BP-= (INTERVAL-LAST-BP (NODE-PREVIOUS NODE))
		     (INTERVAL-FIRST-BP NODE)))
	   (OR (NULL (NODE-NEXT NODE))
	       (BP-= (INTERVAL-LAST-BP NODE)
		     (INTERVAL-FIRST-BP (NODE-NEXT NODE))))
	   ;; Don't hack this for Buffer Header nodes.
	   (SECTION-NODE-DEFUN-LINE NODE)
	   (NUMBERP (LINE-TICK (SECTION-NODE-DEFUN-LINE NODE)))
	   ( (SECTION-NODE-SECTIONIZE-TICK NODE)
	      (LINE-TICK (SECTION-NODE-DEFUN-LINE NODE)))
	   (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP NODE))
		      (LINE-NEXT LINE))
		(DEFUN-LINE (SECTION-NODE-DEFUN-LINE NODE))
		(END-LINE (BP-LINE (INTERVAL-LAST-BP NODE)))
		(SECTION-P
		  (GET (GET (SEND (NODE-TOP-LEVEL-NODE NODE) ':MAJOR-MODE)
			    'EDITING-TYPE)
		       'SECTION-P)))
	       ((EQ LINE END-LINE) T)
	     (WHEN (NEQ (EQ LINE DEFUN-LINE)
			(FUNCALL SECTION-P LINE))
	       (RETURN NIL))))))

))

; From file NPRIM.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "


(DEFUN CHECK-INTERVAL-SECTIONS (START-BP &OPTIONAL END-BP IN-ORDER-P)
  "Fix up sectionization  of specified interval, where text has changed.
The specified interval should be all or part of *INTERVAL*."
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
;  (LET ((FOO (NODE-INFERIORS *INTERVAL*))
;	 (INT *INTERVAL*))
;    (DOLIST (N FOO)
;      (IF (NOT (NUMBERP (LINE-TICK (BP-LINE (INTERVAL-FIRST-BP N)))))
;	  (FERROR NIL "First-bp of ~S is deleted.  This bug is on the most-wanted list.
;Please make a detailed report." N))
;      (IF (NOT (NUMBERP (LINE-TICK (BP-LINE (INTERVAL-LAST-BP N)))))
;	  (FERROR NIL "Last-bp of ~S is deleted.  This bug is on the most-wanted list.
;Please make a detailed report." N))))
  (IF (NODE-INFERIORS *INTERVAL*)
      (WHEN (TYPEP (CAR (NODE-INFERIORS *INTERVAL*)) 'SECTION-NODE)
	;; Don't bother trying if the subnodes of *INTERVAL* are not from sectionzation.
	(LET* ((START-NODE (PREVIOUS-CLEAN-SECTION-BREAK (BP-PREV-SUBORDINATE-NODE START-BP)))
	       ;; If END-BP points at the boundary of two nodes,
	       ;; (BP-NODE END-BP) is the following node; but we really don't
	       ;; need to include that node if the break at END-BP is clean.
	       (END-NODE
		 (NEXT-CLEAN-SECTION-BREAK
		   (IF (BP-= START-BP END-BP)
		       START-NODE
		     (LET ((ENDNODE1 (BP-NEXT-SUBORDINATE-NODE END-BP)))
		       (IF (BP-= END-BP (INTERVAL-FIRST-BP ENDNODE1))
			   (NODE-PREVIOUS ENDNODE1)
			 ENDNODE1))))))
	  (IF (OR (EQ START-NODE *INTERVAL*) (EQ END-NODE *INTERVAL*))
	      ;; Can this really happen??
	      (SECTIONIZE-BUFFER *INTERVAL*)
	    ;; No need to do anything if all the nodes are unchanged
	    ;; since last resectionize.
	    (UNLESS (DO ((NODE START-NODE (NODE-NEXT NODE)))
			((NULL NODE) T)
		      (OR ( (SECTION-NODE-SECTIONIZE-TICK NODE)
			     (NODE-TICK NODE))
			  (SECTION-SECTIONIZATION-VALID-P NODE)
			  ;; Unless it is clean and unchanged, resectionize it.
			  (RETURN NIL))
		      (IF (EQ NODE END-NODE) (RETURN T)))
	      (RESECTIONIZE-BUFFER *INTERVAL* START-NODE END-NODE)))))
    (SECTIONIZE-BUFFER *INTERVAL*)))

))

; From file SECTIO.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN (:LISP SECTION-P) (LINE)
  (AND (PLUSP (LENGTH LINE))
       (= (LDB %%CH-CHAR (AREF LINE 0)) #/()))

(DEFUN (:TEXT SECTION-P) (LINE)
  (%STRING-EQUAL LINE 0 ".DEF" 0 4))

(DEFUN (NIL SECTION-P) (IGNORE) NIL)

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN COMPOSE-AUTOMATIC-METHODS (FL)
  ;; Avoid lossage on PROPERTY-LIST-MIXIN while reading this file into the cold load.
  (WHEN (FBOUNDP 'COMPILE-AT-APPROPRIATE-TIME)
    (DOLIST (V (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL))
      (LET* ((VV (CORRESPONDING-KEYWORD V))
	     (METH `(:METHOD ,(FLAVOR-NAME FL) ,VV)))
	(IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
		*JUST-COMPILING*)
	    (COMPILE-AT-APPROPRIATE-TIME
	      FL METH
	      `(NAMED-LAMBDA (,METH) (IGNORE)
		 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
			  (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
		 ,V))
	  (RECORD-SOURCE-FILE-NAME METH))))
    (DOLIST (V (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL))
      (LET* ((SV (INTERN1 (FORMAT NIL "SET-~A" V) PKG-KEYWORD-PACKAGE))
	     (METH `(:METHOD ,(FLAVOR-NAME FL) ,SV)))
	(IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
		*JUST-COMPILING*)
	    (COMPILE-AT-APPROPRIATE-TIME
	      FL METH
	      `(NAMED-LAMBDA (,METH) (IGNORE .NEWVALUE.)
		 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
			  (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
		 (SETQ ,V .NEWVALUE.)))
	  (RECORD-SOURCE-FILE-NAME METH))))))

))

(LOAD '#FS#:LOGICAL-PATHNAME "SYS: FONTS; HL12B QFASL >" "FONTS" NIL NIL)
(TV:UPDATE-FONT-MAPS)


; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN P1 (FORM &OPTIONAL DONT-OPTIMIZE)
  (PROG (TM)
	(OR DONT-OPTIMIZE
	    (SETQ FORM (OPTIMIZE FORM T)))
	(COND
	  ((ATOM FORM)
	   (RETURN (COND ((CONSTANTP FORM) (LIST 'QUOTE FORM))
			 ((SETQ TM (ASSQ FORM VARS))
			  (AND (EQ (VAR-KIND TM) 'FEF-ARG-FREE)
			       (ZEROP (VAR-USE-COUNT TM))
			       (PUSH (VAR-NAME TM) FREEVARS))
			  (VAR-INCREMENT-USE-COUNT TM)
			  (VAR-LAP-ADDRESS TM))
			 ((TRY-REF-SELF FORM))
			 ((SPECIALP FORM)
			  (MAKESPECIAL FORM) FORM)
			 ((TRY-REF-LEXICAL-VAR FORM))
			 (T (MAKESPECIAL FORM) FORM))))
	  ((EQ (CAR FORM) 'QUOTE) (RETURN  FORM))
	  ;; Certain constructs must be checked for here
	  ;; so we can call P1 recursively without setting TLEVEL to NIL.
	  ((NOT (ATOM (CAR FORM)))
	   ;; Expand any lambda macros -- just returns old function if none found
	   (LET ((FCTN (CAR FORM)))
	     (OR (SYMBOLP (CAR FCTN))
		 (WARN 'BAD-FUNCTION-CALLED ':IMPOSSIBLE
		       "There appears to be a call to a function whose CAR is ~S."
		       (CAR FCTN)))
	     (COND ((MEMQ (CAR FCTN) '(LAMBDA NAMED-LAMBDA CLI:LAMBDA CLI:NAMED-LAMBDA))
		    (RETURN (P1LAMBDA FCTN (CDR FORM))))
		   (T ;; Old Maclisp evaluated functions.
		    (WARN 'EXPRESSION-AS-FUNCTION ':VERY-OBSOLETE
			  "The expression ~S is used as a function; use FUNCALL."
			  (CAR FORM))
		    (RETURN (P1 `(FUNCALL . ,FORM)))))))
	  ((NOT (SYMBOLP (CAR FORM)))
	   (WARN 'BAD-FUNCTION-CALLED ':IMPOSSIBLE
		 "~S is used as a function to be called." (CAR FORM))
	   (RETURN (P1 (CONS 'PROGN (CDR FORM)))))
	  ((SETQ TM (ASSQ (CAR FORM) LOCAL-FUNCTIONS))
	   (VAR-INCREMENT-USE-COUNT (CADR TM))
	   (RETURN `(FUNCALL ,(TRY-REF-LEXICAL-HOME (CADR TM))
			     . ,(P1PROGN-1 (CDR FORM)))))
	  ((MEMQ (CAR FORM) '(PROG PROG* :PROG :PROG*)) (RETURN (P1PROG FORM)))
	  ((MEMQ (CAR FORM) '(LET LET* :LET :LET*)) (RETURN (P1LET FORM)))
	  ((MEMQ (CAR FORM) '(BLOCK :BLOCK)) (RETURN (P1BLOCK FORM)))
	  ((MEMQ (CAR FORM) '(TAGBODY :TAGBODY)) (RETURN (P1TAGBODY FORM)))
	  ((MEMQ (CAR FORM) '(%POP :%POP))		;P2 specially checks for this
	   (RETURN FORM)))
	(SETQ TLEVEL NIL)
	;; Check for functions with special P1 handlers.
	(COND ((SETQ TM (GET (CAR FORM) 'P1))
	       (RETURN (FUNCALL TM FORM))))
	(COND ((AND ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
		    (ASSQ (CAR FORM) VARS)
		    (NULL (FUNCTION-P (CAR FORM))))
	       (WARN 'EXPRESSION-AS-FUNCTION ':VERY-OBSOLETE
		     "The variable ~S is used in function position; use FUNCALL."
		     (CAR FORM))
	       (RETURN (P1 (CONS 'FUNCALL FORM)))))
	(RETURN (P1ARGC FORM (GETARGDESC (CAR FORM))))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN MAKE-FREE-VAR-HOME (NAME)
  (MAKE-VAR NAME NAME KIND 'FEF-ARG-FREE TYPE 'FEF-SPECIAL USE-COUNT 0
	    LAP-ADDRESS NAME))

;; For a variable whose scope is ready to begin (it's about to be put on VARS),
;; look for another variable whose scope already ended, to share a slot with.
;; If we find a suitable one, just clobber it in.

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN (MULTIPLE-VALUE-BIND P1) (FORM)
  (LET ((VARIABLES (CADR FORM))
	(VARS VARS) OUTER-VARS
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(M-V-FORM (CADDR FORM))
	(BODY (CDDDR FORM)))
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL))
    (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
    (SETQ OUTER-VARS VARS)
    (SETQ TLEVEL NIL)
    ;; P1 the m-v-returning-form outside the bindings we make.
    (SETQ M-V-FORM (P1 M-V-FORM))
    ;; The code should initialize each variable by popping off the stack.
    ;; The values will be in forward order so we must pop in reverse order.
    (SETQ VARIABLES (MAPCAR #'(LAMBDA (V) `(,V (%POP))) VARIABLES))
    (P1SBIND VARIABLES 'FEF-ARG-INTERNAL-AUX T T THIS-FRAME-DECLARATIONS)
    (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    (SETQ BODY (P1PROGN-1 BODY))
    `(,(CAR FORM) ,VARIABLES ,OUTER-VARS ,VARS ,M-V-FORM . ,BODY)))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN PROCESS-SPECIAL-DECLARATIONS (DECLS)
  (DOLIST (DECL DECLS)
    (IF (EQ (CAR DECL) 'SPECIAL)
	(DOLIST (VARNAME (CDR DECL))
	  (PUSHNEW VARNAME FREEVARS)
	  (PUSH (MAKE-FREE-VAR-HOME VARNAME)
		VARS)))))

))


; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN QCOMPILE0 (EXP FUNCTION-TO-BE-DEFINED GENERATING-MICRO-COMPILER-INPUT-P
		  &OPTIONAL (NAME-TO-GIVE-FUNCTION FUNCTION-TO-BE-DEFINED))
  (PROG (VARS EXP1 DEF-TO-BE-SXHASHED BODY
	 PDLLVL MAXPDLLVL CALL-BLOCK-PDL-LEVELS WITHIN-CATCH
	 ALLGOTAGS TLEVEL P1VALUE BINDP LVCNT
	 DROPTHRU ALLVARS FREEVARS
	 (LOCAL-FUNCTIONS COMPILER-LEXICAL-FUNCTIONS)
	 (LOCAL-MACROS COMPILER-LEXICAL-MACROS)
	 (PROGDESCS COMPILER-LEXICAL-PROGDESCS)
	 (RETPROGDESC COMPILER-LEXICAL-RETPROGDESC)
	 (GOTAGS COMPILER-LEXICAL-GOTAGS)
	 LL TAGOUT TLFUNINIT SPECIALFLAG MACROFLAG
	 LOCAL-MAP ARG-MAP DOCUMENTATION EXPR-DEBUG-INFO
	 FAST-ARGS-POSSIBLE BREAKOFF-COUNT
	 (LEXICAL-CLOSURE-COUNT 0)
	 VARIABLES-USED-IN-LEXICAL-CLOSURES
	 ;; List of all macros found in this function, for the debugging info.
	 MACROS-EXPANDED
	 SELF-FLAVOR-DECLARATION
	 ;; Set to T during pass 1 if any SELF-REFs are present in the function.
	 SELF-REFERENCES-PRESENT
	 (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	 ;; T if this is a SUBST being compiled.  Always put interpreted defn in debug info.
	 SUBST-FLAG
	 INHIBIT-SPECIAL-WARNINGS
	 CLOBBER-NONSPECIAL-VARS-LISTS)
       (SETQ PDLLVL 0)		;RUNTINE LOCAL PDLLVL
       (SETQ DROPTHRU T)	;CAN DROP IN IF FALSE, FLUSH STUFF TILL TAG OR
       (SETQ MAXPDLLVL 0)	;DEEPEST LVL REACHED BY LOCAL PDL
       (SETQ TLEVEL T)
       (SETQ P1VALUE T)
       (SETQ FAST-ARGS-POSSIBLE T)
       (SETQ BREAKOFF-COUNT 0)
       (SETQ EXP1 EXP)
       (BEGIN-PROCESSING-FUNCTION FUNCTION-TO-BE-DEFINED)
       ;; If compiling a macro, compile its expansion function
       ;; and direct lap to construct a macro later.
       (COND ((EQ (CAR EXP1) 'MACRO)
	      (SETQ MACROFLAG T)
	      (SETQ EXP1 (CDR EXP1))
	      (SETQ DEF-TO-BE-SXHASHED EXP1)))
       (OR (MEMQ (CAR EXP1) '(LAMBDA SUBST CLI:LAMBDA
			      NAMED-LAMBDA NAMED-SUBST CLI:NAMED-LAMBDA))
	   (PROGN (WARN 'FUNCTION-NOT-VALID ':FATAL "The definition is not a function at all.")
		  (RETURN NIL)))
       (IF (MEMQ (CAR EXP1) '(SUBST NAMED-SUBST))
	   (SETQ SUBST-FLAG T INHIBIT-SPECIAL-WARNINGS T))
       ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
       (WHEN (MEMQ (CAR EXP1) '(NAMED-LAMBDA CLI:NAMED-LAMBDA NAMED-SUBST))
	 (SETQ EXPR-DEBUG-INFO
	       (AND (NOT (ATOM (CADR EXP1)))
		    (CDADR EXP1))
	       EXP1 (CDR EXP1))
	 ;; Debug info that is equivalent to declarations
	 ;; should be turned back into declarations, coming before
	 ;; declarations made outside of compilation
	 ;; but after anything coming from a DECLARE in the body.
	 (DOLIST (ELT (REVERSE EXPR-DEBUG-INFO))
	   (WHEN (ASSQ (CAR ELT) *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	     (PUSH ELT LOCAL-DECLARATIONS))))
       (SETQ LL (CADR EXP1))	;lambda list.
       (SETQ BODY (CDDR EXP1))
       ;; Record the function's arglist for warnings about recursive calls.
       (OR THIS-FUNCTION-ARGLIST-FUNCTION-NAME
	   (SETQ THIS-FUNCTION-ARGLIST-FUNCTION-NAME NAME-TO-GIVE-FUNCTION
		 THIS-FUNCTION-ARGLIST LL))
       ;; Extract documentation string and declarations from the front of the body.
       (MULTIPLE-VALUE (BODY LOCAL-DECLARATIONS DOCUMENTATION)
	 (EXTRACT-DECLARATIONS BODY LOCAL-DECLARATIONS T))
       (SETQ SELF-FLAVOR-DECLARATION
	     (CDR (ASSQ ':SELF-FLAVOR LOCAL-DECLARATIONS)))
       ;; If the user just did (declare (:self-flavor flname)),
       ;; compute the full declaration for that flavor.
       (AND SELF-FLAVOR-DECLARATION
	    (NULL (CDR SELF-FLAVOR-DECLARATION))
	    (SETQ SELF-FLAVOR-DECLARATION
		  (CDR (SI:FLAVOR-DECLARATION (CAR SELF-FLAVOR-DECLARATION)))))
       ;; Actual DEFMETHODs must always have SELF-FLAVOR, or else
       ;; the flavor system will think they are pre-system-85 methods.
       (AND (CONSP FUNCTION-TO-BE-DEFINED)
	    (EQ (CAR FUNCTION-TO-BE-DEFINED) ':METHOD)
	    (SETQ SELF-REFERENCES-PRESENT T))
       ;; Process &KEY and &AUX vars, if there are any.
       (WHEN (OR (MEMQ '&KEY LL) (MEMQ '&AUX LL))
	 ;; Put arglist together with body again.
	 (LET ((LAMEXP `(LAMBDA ,LL (DECLARE . ,LOCAL-DECLARATIONS) . ,BODY)))
	   ;; If there are keyword arguments, expand them.
	   (AND (MEMQ '&KEY LL)
		(SETQ LAMEXP (EXPAND-KEYED-LAMBDA LAMEXP)))
	   ;; Now turn any &AUX variables in the LAMBDA into a PROG in the body.
	   (SETQ LAMEXP (P1AUX LAMEXP))
	   ;; Separate lambda list and body again.
	   (SETQ LL (CADR LAMEXP) BODY (CDDR LAMEXP)))
	 (DO () ((NOT (AND (CONSP (CAR BODY)) (EQ (CAAR BODY) 'DECLARE))))
	   (POP BODY)))
       ;; Now process the variables in the lambda list, after the local declarations.
       (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL LOCAL-DECLARATIONS))
       (COND ((NOT (NULL (CDR BODY)))
	      (SETQ EXP1 (CONS 'PROGN BODY)))
	     ((SETQ EXP1 (CAR BODY))))
       (SETQ EXP1 (P1 EXP1))		;DO PASS 1 TO SINGLE-EXPRESSION BODY
       (SETQ LVCNT (ASSIGN-LAP-ADDRESSES))
       ;; Now that we know all the variables needed by lexical closures,
       ;; make a list of them and put them into the entries in COMPILER-QUEUE
       ;; for each of those lexical closures.
       (UNLESS (ZEROP LEXICAL-CLOSURE-COUNT)
	 (SETQ VARIABLES-USED-IN-LEXICAL-CLOSURES
	       (RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES)))
       (OUTF (LIST 'MFEF FUNCTION-TO-BE-DEFINED SPECIALFLAG (ELIMINATE-DUPLICATES-AND-REVERSE ALLVARS)
		   FREEVARS NAME-TO-GIVE-FUNCTION))
       (AND MACROFLAG (OUTF '(CONSTRUCT-MACRO)))
       (OUTF '(QTAG S-V-BASE))
       (OUTF '(S-V-BLOCK))
       (IF (AND SELF-FLAVOR-DECLARATION SELF-REFERENCES-PRESENT)
	   (OUTF `(SELF-FLAVOR . ,SELF-FLAVOR-DECLARATION)))
       (OUTF '(QTAG DESC-LIST-ORG))
       (OUTF (LIST 'PARAM 'LLOCBLOCK
		   (IF (ZEROP LEXICAL-CLOSURE-COUNT)
		       LVCNT
		     (+ LVCNT (* 4 LEXICAL-CLOSURE-COUNT) 3
			(LENGTH VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
       (OUTF '(A-D-L))
       (OUTF '(QTAG QUOTE-BASE))
       (OUTF '(ENDLIST))			;LAP WILL INSERT QUOTE VECTOR HERE
       (WHEN VARIABLES-USED-IN-LEXICAL-CLOSURES
	 (OUTF `(VARIABLES-USED-IN-LEXICAL-CLOSURES
		  . ,(REVERSE (MAPCAR #'(LAMBDA (HOME)
					  (LET ((TEM (VAR-LAP-ADDRESS HOME)))
					    (SELECTQ (CAR TEM)
					      (ARG (CADR TEM))
					      (T (%LOGDPB 1 %%Q-BOXED-SIGN-BIT (CADR TEM))))))
				      VARIABLES-USED-IN-LEXICAL-CLOSURES)))))
       ;; Set up the debug info from the local declarations and other things
       (LET ((DEBUG-INFO NIL) TEM)
	 (AND DOCUMENTATION (PUSH `(:DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
	 (DOLIST (DCL *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	   (IF (SETQ TEM (ASSQ (CAR DCL) LOCAL-DECLARATIONS))
	       (IF (NEQ (CAR DCL) (CDR DCL))
		   (PUSH (CONS (CDR DCL) (CDR TEM)) DEBUG-INFO)
		 (PUSH TEM DEBUG-INFO))))
	 ;; Propagate any other kinds of debug info from the expr definition.
	 (DOLIST (DCL EXPR-DEBUG-INFO)
	   (OR (ASSQ (CAR DCL) DEBUG-INFO)
	       (PUSH DCL DEBUG-INFO)))
         (AND (PLUSP BREAKOFF-COUNT)
	      (LET ((INTERNAL-OFFSETS (MAKE-LIST BREAKOFF-COUNT)))
		(OUTF `(BREAKOFFS ,INTERNAL-OFFSETS))
		(PUSH `(:INTERNAL-FEF-OFFSETS . ,INTERNAL-OFFSETS) DEBUG-INFO)))
         ;; Include the local and arg maps if we have them.
         ;; They were built by ASSIGN-LAP-ADDRESSES.
         (AND LOCAL-MAP (PUSH `(LOCAL-MAP ,LOCAL-MAP) DEBUG-INFO))
         (AND ARG-MAP (PUSH `(ARG-MAP ,ARG-MAP) DEBUG-INFO))
	 ;; Include list of macros used, if any.
	 (WHEN MACROS-EXPANDED
	   (LET ((MACROS-AND-SXHASHES
		   (MAPCAR #'(LAMBDA (MACRONAME)
			       (LET ((HASH (EXPR-SXHASH MACRONAME)))
				 (IF (OR HASH (CONSP MACRONAME))
				     (LIST MACRONAME HASH)
				   MACRONAME)))
			   MACROS-EXPANDED)))
	     (IF QC-FILE-RECORD-MACROS-EXPANDED
		 (PROGN
		   ;; If in QC-FILE, put just macro names in the function
		   ;; but put the names and sxhashes into the file's list.
		   (PUSH `(:MACROS-EXPANDED ,MACROS-EXPANDED) DEBUG-INFO)
		   (DOLIST (M MACROS-AND-SXHASHES)
		     (OR (MEMBER M QC-FILE-MACROS-EXPANDED)
			 (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
			   (PUSH (COPYTREE M) QC-FILE-MACROS-EXPANDED)))))
	       (PUSH `(:MACROS-EXPANDED ,MACROS-AND-SXHASHES)
		     DEBUG-INFO))))
	 (AND (OR (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
		  SUBST-FLAG)
	      (PUSH `(INTERPRETED-DEFINITION ,EXP) DEBUG-INFO))
	 (WHEN SUBST-FLAG
	   (LET* ((ARGS-INFO (ARGS-INFO EXP))
		  (DUMMY-FORM (CONS 'FOO
				    (MAKE-LIST (+ (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)
						  (IF (LDB-TEST %ARG-DESC-EVALED-REST ARGS-INFO)
						      1 0))
					       ':INITIAL-ELEMENT '(GENSYM)))))
	     (UNLESS (EQUAL (SI:SUBST-EXPAND EXP DUMMY-FORM)
			    (SI:SUBST-EXPAND EXP DUMMY-FORM T))
	       ;; If simple and thoughtful substitution give the same result
	       ;; even with the most intractable arguments,
	       ;; we need not use thoughtful substitution for this defsubst.
	       ;; Otherwise, mark it as requiring thoughtful substitution.
	       (PUSH '(:NO-SIMPLE-SUBSTITUTION T) DEBUG-INFO))))
	 ;; Compute the sxhash now, after all displacing macros have been displaced
	 (AND MACROFLAG
	      (PUSH `(:EXPR-SXHASH ,(FUNCTION-EXPR-SXHASH DEF-TO-BE-SXHASHED)) DEBUG-INFO))
	 ;; If we aren't going to mark this function as requiring a mapping
	 ;; table, provide anyway some info that the user declared it wanted one.
	 (AND SELF-FLAVOR-DECLARATION (NOT SELF-REFERENCES-PRESENT)
	      (PUSH `(:SELF-FLAVOR ,(CAR SELF-FLAVOR-DECLARATION)) DEBUG-INFO))
	 (AND DEBUG-INFO
              (OUTF `(DEBUG-INFO . ,DEBUG-INFO))))
       (OUTF 'PROGSA)
       (P2SBIND LL VARS NIL)			;CAN COMPILE INITIALIZING CODE
       (LET ((LEXICAL-CLOSURE-COUNT 0))
	 (P2 EXP1 'D-RETURN))			;DO PASS 2
       (OUTF (LIST 'PARAM 'MXPDL (1+ MAXPDLLVL)))
       (RETURN ALLVARS)))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "


(DEFUN P1LET (FORM &OPTIONAL FOR-AUXVARS)
  (LET ((VARS VARS) OUTER-VARS
	(FN (CAR FORM)) (P1VALUE NIL) (BINDP) (BODY) (VLIST)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(ENTRY-LEXICAL-CLOSURE-COUNT LEXICAL-CLOSURE-COUNT))
    (SETQ VLIST (CADR FORM))
    (SETQ BODY (CDDR FORM))
    (IF (EQ FN 'LET-FOR-AUXVARS) (SETQ FN 'LET*))
    ;; Take all DECLAREs off the body.
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL))
    (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
    (SETQ OUTER-VARS VARS)
    ;; Treat parallel binding as serial if it doesn't matter.
    (OR (CDR VLIST) (SETQ FN 'LET*))
    (AND (MEMQ FN '(LET :LET))
	 (DO ((XX VLIST (CDR XX)))
	     ((NULL XX) (SETQ FN 'LET*))
	   ;; Namely, if binding each symbol to NIL, a constant, or itself.
	   (OR (ATOM (CAR XX))
	       (CONSTANTP (CADAR XX))
	       (EQ (CAAR XX) (CADAR XX))
	       (RETURN NIL))))
    ;; Flush rebinding a var to itself if it isn't special
    ;; and range of rebinding is rest of function.
    (AND TLEVEL
	 (SETQ VLIST
	       (SUBSET-NOT #'(LAMBDA (VAR)
			       (AND (NOT (ATOM VAR))
				    (EQ (CAR VAR) (CADR VAR))
				    (EQ (FIND-TYPE (CAR VAR) THIS-FRAME-DECLARATIONS)
					'FEF-LOCAL)
				    (EQ (VAR-TYPE (ASSQ (CAR VAR) VARS)) 'FEF-LOCAL)))
			   VLIST)))
    ;; All the local declarations should be in effect for the init forms.
    (SETQ LOCAL-DECLARATIONS (APPEND THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    ;; &AUX vars should be allowed to inherit special declarations
    ;; since that is what it looks like when you put a DECLARE inside the body.
    (IF FOR-AUXVARS
	(SETQ THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    (SETQ VLIST (P1SBIND VLIST
			 (COND (TLEVEL 'FEF-ARG-AUX)
			       (T 'FEF-ARG-INTERNAL-AUX))
			 (MEMQ FN '(LET :LET))
			 NIL
			 THIS-FRAME-DECLARATIONS))
    ;; Now convert initial SETQs to variable initializations.
    ;; We win only for SETQs of variables bound but with no initialization spec'd,
    ;; which set them to constant values, and only if later vars' inits didn't use them.
    ;; When we come to anything other than a SETQ we can win for, we stop.
    ;; For PROG*, we can't win for a special variable if anyone has called a function
    ;; to do initting, since that function might have referred to the special.
    ;; Even if we don't use tha ADL to init them,
    ;; we avoid redundant settings to NIL.
    (DO ((TEM) (HOME)) (NIL)
      (COND ((MEMBER (CAR BODY) '((SETQ) (:SETQ)))
	     (SETQ BODY (CDR BODY)))
	    ((OR (ATOM (CAR BODY))
		 (ATOM (SETQ TEM (OPTIMIZE (CAR BODY) NIL)))
		 (NOT (MEMQ (CAR TEM) '(SETQ :SETQ)))
		 (NOT (MEMQ (CADR TEM) VLIST))
		 (NOT (CONSTANTP (CADDR TEM)))
		 (AND (SPECIALP (CADR TEM))
		      (OR TLFUNINIT (NOT TLEVEL))
		      (MEMQ FN '(LET* :LET*)))
		 (NOT (ZEROP (VAR-USE-COUNT (SETQ HOME (ASSQ (CADR TEM) VARS))))))
	     (RETURN NIL))
	    (T (SETQ BODY (CONS (CONS 'SETQ (CDDDR TEM)) (CDR BODY)))
	       (RPLACA (MEMQ (CADR TEM) VLIST)
		       `(,(CADR TEM) ,(P1 (CADDR TEM))))
	       ;; For a variable bound at function entry, really set up its init.
	       ;; Other vars (FEF-ARG-INTERNAL-AUX) will be initted by code,
	       ;; despite our optimization, but it will be better code.
	       (AND TLEVEL (EQ (VAR-KIND HOME) 'FEF-ARG-AUX)
		    (SETF (VAR-INIT HOME) `(FEF-INI-PNTR ,(P1 (CADDR TEM))))))))
    
    ;; Now P1 process what is left of the body.
    (AND (CDR BODY) (SETQ TLEVEL NIL))
    (SETQ BODY (P1PROGN-1 BODY))
    `(,FN ,VLIST ,OUTER-VARS ,VARS ,BINDP
      ,ENTRY-LEXICAL-CLOSURE-COUNT ,LEXICAL-CLOSURE-COUNT
      . ,BODY)))

;MEMQ and ASSQ together.  Find the tail of VLIST
;whose CAR or CAAR is VARNAME.

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (LET-FOR-LAMBDA P1) (FORM)
  (LET ((VARS VARS) OUTER-VARS
	(P1VALUE NIL) (BINDP) (BODY) (VLIST)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(ENTRY-LEXICAL-CLOSURE-COUNT LEXICAL-CLOSURE-COUNT))
    ;; Take all DECLAREs off the body.
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
	  (EXTRACT-DECLARATIONS-RECORD-MACROS (CDDR FORM) NIL))
    (SETQ VLIST (P1SBIND (CADR FORM)
			 (COND (TLEVEL 'FEF-ARG-AUX)
			       (T 'FEF-ARG-INTERNAL-AUX))
			 T NIL THIS-FRAME-DECLARATIONS))
    (SETQ OUTER-VARS VARS)
    (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
    ;; Do this here so that the local declarations
    ;; do not affect the init forms.
    (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    ;; Now P1 process what is left of the body.
    (AND (CDR BODY) (SETQ TLEVEL NIL))
    (SETQ BODY (P1PROGN-1 BODY))
    `(LET-FOR-LAMBDA ,VLIST ,THIS-FRAME-DECLARATIONS ,OUTER-VARS ,BINDP
      ,ENTRY-LEXICAL-CLOSURE-COUNT ,LEXICAL-CLOSURE-COUNT
      . ,BODY)))

;; BLOCK and RETURN-FROM.
;; These know how to turn into catches and throws
;; when necessary for general lexical scoping.

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1LAMBDA (LAMBDA ARGS)
  (LET (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR QUOTEFLAG
	SPECIAL-FLAG SPECIAL-VARS UNSPECIAL-FLAG UNSPECIAL-VARS
	DECLS KEYCHECKS BORDER-VARIABLE)
    (SETQ LAMBDA (SI:LAMBDA-EXP-ARGS-AND-BODY (P1AUX LAMBDA)))
    (SETQ ARGLIST (CAR LAMBDA) BODY (CDR LAMBDA))
    (MULTIPLE-VALUE-BIND (NIL NIL NIL REST-ARG
			  NIL KEYKEYS KEYNAMES NIL KEYINITS KEYFLAGS
			  ALLOW-OTHER-KEYS)
	(DECODE-KEYWORD-ARGLIST ARGLIST)
      (WHEN (AND KEYNAMES (NOT REST-ARG))
	(SETQ REST-ARG (GENSYM)))
      (SETQ ARGS1 ARGS)
      (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
	  (NIL)
	(SETQ VAR (CAR ARGLIST1))
	(COND ((NULL ARGLIST1)
	       (RETURN T))
	      ((EQ VAR '&KEY)
	       (PUSH (LIST REST-ARG `(LIST . ,ARGS1)) PROGVARS)
	       (RETURN (SETQ ARGS1 NIL)))
	      ((EQ VAR '&REST)
	       (POP ARGLIST1)
	       (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
	       (RETURN (SETQ ARGS1 NIL)))
	      ((EQ VAR '&OPTIONAL)
	       (SETQ OPTIONAL T))
	      ((EQ VAR '&QUOTE)
	       (SETQ QUOTEFLAG T))
	      ((EQ VAR '&EVAL)
	       (SETQ QUOTEFLAG NIL))
	      ((EQ VAR '&SPECIAL)
	       (SETQ SPECIAL-FLAG T UNSPECIAL-FLAG NIL))
	      ((EQ VAR '&LOCAL)
	       (SETQ SPECIAL-FLAG NIL UNSPECIAL-FLAG T))
	      ((EQ VAR '&FUNCTIONAL))
	      ((MEMQ VAR LAMBDA-LIST-KEYWORDS)
	       (WARN 'BAD-INTERNAL-LAMBDA-KEYWORD ':IMPOSSIBLE
		     "~S is not supported in internal lambdas." VAR))
	      (T (AND SPECIAL-FLAG (PUSH VAR SPECIAL-VARS))
		 (AND UNSPECIAL-FLAG (PUSH VAR UNSPECIAL-VARS))
		 (COND ((SYMBOLP VAR)
			(PUSH (LIST VAR (IF QUOTEFLAG `',(CAR ARGS1)
					  (CAR ARGS1)))
			      PROGVARS))
		       (T
			(COND ((NOT OPTIONAL)
			       (WARN 'BAD-ARGUMENT-LIST ':IMPOSSIBLE
				     "The mandatory argument ~S of an internal lambda ~
  was given a default value."
				     (CAR VAR))))
			(PUSH (LIST (CAR VAR)
				    (COND (ARGS1 (IF QUOTEFLAG `',(CAR ARGS1)
						   (CAR ARGS1)))
					  (T (CADR VAR)))) PROGVARS)))
		 (POP ARGS1))))
      (WHEN KEYNAMES
	;; For each keyword arg, decide whether we need to nit it to KEYWORD-GARBAGE
	;; and check explicitly whether that has been overridden.
	;; If the arg is optional
	;; and the initial value is a constant, we can really init it to that.
	;; Otherwise, we change its KEYINITS element to
	;; KEYWORD-GARBAGE and push a cleanup form on KEYCHECKS.
	(DO ((KIS KEYINITS (CDR KIS))
	     (KNS KEYNAMES (CDR KNS))
	     (KFS KEYFLAGS (CDR KFS)))
	    ((NULL KNS))
	  (LET ((KEYNAME (CAR KNS)) (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	    ;; All optional now.
	    (OR (AND (NULL KEYFLAG)
		     (CONSTANTP KEYINIT))
		(PROGN (RPLACA KIS 'SI:KEYWORD-GARBAGE)
		       (PUSH `(COND ((EQ ,KEYNAME SI:KEYWORD-GARBAGE)
				     (SETQ ,KEYNAME ,KEYINIT))
				    (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))))
			     KEYCHECKS)))))
	(SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
	(SETQ KEYCHECKS (NREVERSE KEYCHECKS))

	;; BORDER-VARIABLE is a local we put in the binding list
	;; as the easiest way of being able to get a locative to the
	;; slot before the first of our keyword arg locals.
	(SETQ BORDER-VARIABLE (GENSYM))

	;; Put our list of variable names onto CLOBBER-NONSPECIAL-VARS-LISTS
	;; so that ASSIGN-LAP-ADDRESSES will clobber out the variables
	;; which are not special with NIL.
	(PUSH KEYNAMES CLOBBER-NONSPECIAL-VARS-LISTS)
	(SETQ BODY
	      `((LET* (,BORDER-VARIABLE
		       ,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) KEYNAMES KEYINITS)
		       ,@KEYFLAGS)
		  ,BORDER-VARIABLE
		  (WHEN ,REST-ARG
		    (SI:STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
		      (VARIABLE-LOCATION ,BORDER-VARIABLE)
		      ,REST-ARG ',KEYKEYS
		      ,ALLOW-OTHER-KEYS
		      ',KEYNAMES))
		  ,@KEYCHECKS
		  . ,BODY))))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (SETF (VALUES BODY DECLS)
	    (EXTRACT-DECLARATIONS-RECORD-MACROS BODY))
      (WHEN SPECIAL-VARS
	(PUSH `(SPECIAL . ,SPECIAL-VARS) DECLS))
      (WHEN UNSPECIAL-VARS
	(PUSH `(UNSPECIAL . ,UNSPECIAL-VARS) DECLS))
      (WHEN DECLS
	(PUSH `(DECLARE . ,DECLS) BODY))
      (P1 `(LET-FOR-LAMBDA ,(NRECONC PROGVARS (IF ARGS1 `((IGNORE (PROGN . ,ARGS1)))))
	     . ,BODY)))))


))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1PROGN (FORM)
  (SETQ TLEVEL NIL)
  (MULTIPLE-VALUE-BIND (BODY THIS-FRAME-DECLARATIONS)
      (EXTRACT-DECLARATIONS-RECORD-MACROS (CDR FORM))
    (LET ((VARS VARS)
	  (LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
      (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
      (LIST* 'PROGN-WITH-DECLARATIONS VARS (P1PROGN-1 BODY)))))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN P1AUX (LAMBDA)
  (BLOCK DONE
    (LET (STANDARDIZED AUXVARS AUXLIST NONAUXLIST DECLS BODY)
      (SETQ STANDARDIZED (SI:LAMBDA-EXP-ARGS-AND-BODY LAMBDA))
      (OR (SETQ AUXLIST (MEMQ '&AUX (CAR STANDARDIZED)))
	  (RETURN-FROM DONE LAMBDA))
      (SETQ AUXVARS (CDR AUXLIST))
      (SETQ NONAUXLIST (LDIFF (CAR STANDARDIZED) AUXLIST))
      (DO ((VARLIST NONAUXLIST (CDR VARLIST))
	   SPECIAL-FLAG)
	  ((NULL VARLIST)
	   (IF SPECIAL-FLAG
	       (PUSH '&SPECIAL AUXVARS)))
	(COND ((EQ (CAR VARLIST) '&SPECIAL)
	       (SETQ SPECIAL-FLAG T))
	      ((EQ (CAR VARLIST) '&LOCAL)
	       (SETQ SPECIAL-FLAG NIL))))
      (SETQ BODY (CDR STANDARDIZED))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (SETF (VALUES BODY DECLS)
	    (EXTRACT-DECLARATIONS-RECORD-MACROS BODY))
      `(LAMBDA ,NONAUXLIST
	 ,@(IF DECLS `((DECLARE . ,DECLS)))
	 (LET ,AUXVARS
	   ,@(IF DECLS `((DECLARE . ,DECLS)))
	   . ,BODY)))))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN (PROGN-WITH-DECLARATIONS P2) (ARGL DEST)
  (LET ((VARS (CAR ARGL)))
    (P2PROGN (CDR ARGL) DEST)))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN P2MULTIPLE-VALUE-BIND (TAIL DEST)
  (LET ((VLIST (CAR TAIL)))
    (LET ((MVTARGET (LENGTH VLIST))
	  (VARS (SECOND TAIL))
	  (MVFORM (FOURTH TAIL)))
      ;; Compile the form to leave N things on the stack.
      ;; If it fails to do so, then it left only one, so push the other N-1.
      (AND (P2MV MVFORM 'D-PDL MVTARGET)
	   (DO ((I 1 (1+ I))) ((= I MVTARGET))
	     (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
      ;; Now pop them off, binding the variables to them.
      ;; Note that the vlist contains the variables
      ;; in the original order,
      ;; each with an initialization of (%POP).
      (P2PBIND VLIST (THIRD TAIL)))
    (LET ((VARS (THIRD TAIL))
	  (BODY (CDDDDR TAIL)))
      (P2PROG12N (LENGTH BODY) DEST BODY))))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN (LET* P2) (ARGL DEST)
  (LET ((VARS (CADR ARGL)))
    (P2LET-INTERNAL VARS (P2SBIND (CAR ARGL) (CADDR ARGL) VARS) ARGL DEST)))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "


(DEFUN (LET P2) (ARGL DEST)
  (LET ((VARS (CADR ARGL)))
    (P2LET-INTERNAL VARS (P2PBIND (CAR ARGL) (CADDR ARGL)) ARGL DEST)))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "


(DEFUN (LET-FOR-LAMBDA P2) (ARGL DEST)
  (LET ((OVARS VARS)
	(VARS VARS)
	(NBINDS (P2PBIND (CAR ARGL) (CADDR ARGL))))
    (PROCESS-SPECIAL-DECLARATIONS (CADR ARGL))
    (P2LET-INTERNAL OVARS NBINDS ARGL DEST)))

;LET-HACK is generated by LET-INTERNAL in case of lexical closures and WITHIN-CATCH.

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

;LET-HACK is generated by LET-INTERNAL in case of lexical closures and WITHIN-CATCH.
(DEFUN (LET-HACK P2) (ARGL DEST)
  (LET ((VARS (CAR ARGL)))
    (P2LET-INTERNAL VARS (CADR ARGL) (CADDR ARGL) DEST T)))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN P2LET-INTERNAL (OVARS NBINDS ARGL DEST &OPTIONAL IGNORE-LEXICAL-CLOSURES)
  (IF (AND WITHIN-CATCH
	   (NOT IGNORE-LEXICAL-CLOSURES)
	   (NEQ (FIFTH ARGL) (SIXTH ARGL)))
      (P2F `(UNWIND-PROTECT
		(LET-HACK ,OVARS ,NBINDS ,ARGL)
	      (DISCONNECT-STACK-CLOSURES ,(FIFTH ARGL) ,(SIXTH ARGL))
	      (UNSHARE-STACK-CLOSURE-VARS ,VARS ,OVARS))
	   DEST)
    (LET* ((VARS (THIRD ARGL))
	   (IBINDP (FOURTH ARGL))
	   (ENTRY-LEXICAL-CLOSURE-COUNT (FIFTH ARGL))
	   (EXIT-LEXICAL-CLOSURE-COUNT (SIXTH ARGL))
	   (BDY (NTHCDR 6 ARGL))
	   (IDEST 'D-PDL)
	   NVALUES
	   M-V-DONE
	   (PROGDESCS PROGDESCS))
      ;; Determine the immediate destination of returns in this prog.
      (AND (MEMQ DEST '(D-IGNORE D-INDS D-RETURN))
	   (NULL M-V-TARGET)
	   (SETQ IDEST DEST))
      ;; If BIND is used within this LET, and it's an internal LET,
      ;; we must push the specpdl index at entry so we can unbind to it later.
      (WHEN (AND IBINDP (NOT (EQ DEST 'D-RETURN)))
	(OUTI '(MISC D-PDL SPECIAL-PDL-INDEX))
	(INCPDLLVL))
      ;; Push a dummy progdesc so that GOs exiting this LET can unbind our specials.
      (PUSH (MAKE-PROGDESC NAME '(LET)
			   PDL-LEVEL PDLLVL
			   NBINDS (IF IBINDP (LIST NBINDS) NBINDS))
	    PROGDESCS)
      (WHEN (AND (EQ M-V-TARGET 'THROW) IBINDP)
	(P2PUSH-CONSTANT 1)
	(OUTI '(MISC D-PDL PDL-WORD)))
      ;; How many words are we supposed to leave on the stack?
      (SETQ NVALUES
	    (COND ((NUMBERP M-V-TARGET) M-V-TARGET)
		  ((EQ IDEST 'D-PDL) 1)
		  (T 0)))
      (UNLESS BDY (SETQ BDY '('NIL)))
      (DO ((TAIL BDY (CDR TAIL)))
	  ((NULL (CDR TAIL))
	   (UNLESS (P2MV (CAR TAIL) IDEST M-V-TARGET)
	     (SETQ M-V-DONE T)))
	(P2 (CAR TAIL) 'D-IGNORE))
      (UNLESS M-V-DONE
	(SETQ NVALUES 1))
      ;; If this is a top-level PROG, we just went to D-RETURN, so we are done.
      (UNLESS (EQ DEST 'D-RETURN)
	;; Unbind any locals that need to be unbound.
	(WHEN (AND (NOT IGNORE-LEXICAL-CLOSURES)
		   ( ENTRY-LEXICAL-CLOSURE-COUNT EXIT-LEXICAL-CLOSURE-COUNT))
	  (P2 `(DISCONNECT-STACK-CLOSURES ,ENTRY-LEXICAL-CLOSURE-COUNT
					  ,EXIT-LEXICAL-CLOSURE-COUNT)
	      'D-IGNORE)
	  (P2 `(UNSHARE-STACK-CLOSURE-VARS ,VARS ,OVARS) 'D-IGNORE))	
	(WHEN (AND (EQ M-V-TARGET 'THROW) IBINDP)
	  (POPPDL NVALUES 1))
	;; Unbind any specials
	(AND IBINDP
	     (OUTPUT-UNBIND-TO-INDEX NVALUES))
	(UNBIND IDEST NBINDS)
	;; Dispose of our value.
	(AND (NEQ DEST IDEST)
	     (NULL M-V-TARGET)
	     (MOVE-RESULT-FROM-PDL DEST))
	;; If we produced multiple values, say we did.
	(IF M-V-DONE (SETQ M-V-TARGET NIL))))))

;These two do not occur in code except as generated by P2PROG-INTERNAL.
;They are almost a kind of macro for use in pass 2.

))

; From file STEP.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-eval (step-form &optional environment)
  (prog ((step-level (1+ step-level)) step-value step-values tem val)
    (when (>= step-level (ARRAY-LENGTH step-array))
      (adjust-array-size step-array (+ 100 step-level))
      (adjust-array-size step-apply-p-array (+ 100 step-level)))
 mc (as-1 step-form step-array step-level)
    (as-1 nil step-apply-p-array step-level)
    (cond ((atom step-form)
           (setq step-values (list (eval step-form)))
           (setq tem 'atom)
           (go rl))
          ((<= step-level step-max)
           (setq tem (step-cmdr step-form nil t)))
          (t (setq tem 'eval)))
    (cond ((step-macro-p step-form)
	   (setq step-form (macroexpand-1 step-form))
           (go mc))
          ((eq tem 'eval)
           (setq step-values (multiple-value-list (evalhook step-form nil nil environment))))
          ((eq tem 'evalhook)
           (setq step-values (multiple-value-list (evalhook step-form #'step-eval nil
							    environment))))
	  ((eq tem 'applyhook)
	   (setq step-values (multiple-value-list (evalhook step-form nil #'step-applyhook
							    environment))))
          ((ferror nil "Unknown function ~S" tem)))
 rl (setq step-value (setq val (car step-values)))
    (cond ((<= step-level step-max)
           (setq tem (step-cmdr step-form step-values (neq tem 'eval))))
          (t (setq tem 'eval)))
    (and (neq step-value val) (return step-value))
 rt (cond ((null (cdr step-values)) (return (car step-values)))
          (t (return-next-value (car step-values))
             (setq step-values (cdr step-values))
             (go rt)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defsubst interpreter-special-in-frame-p (cell frame-interpreter-environment)
  (let ((tem (compiler:undefined-value)))
    (setq tem (get-lexical-value-cell (car frame-interpreter-environment) cell))
    (if tem
	(= (%p-data-type tem) dtp-one-q-forward)
      (cadr (getl (%find-structure-header cell)
		  '(special system-constant))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defmacro gobble-declarations-from-body ((decls-env-var caller-body-exp) &body macro-body)
  `(with-stack-list* (,decls-env-var nil interpreter-environment)
     (when (and (consp (car ,caller-body-exp))
		(eq (caar ,caller-body-exp) 'declare))
       (bind (locf local-declarations) local-declarations)
       (bind (locf interpreter-environment) ,decls-env-var)
       (gobble-declarations-internal ,caller-body-exp ,decls-env-var))
     . ,macro-body))

(defun gobble-declarations-internal (body &optional (env interpreter-environment))
  (dolist (bodyelt body)
    (or (and (consp bodyelt)
	     (eq (car bodyelt) 'declare))
	(return nil))
    (setq local-declarations
	  (append (cdr bodyelt) local-declarations))
    (dolist (decl (cdr bodyelt))
      (when (memq (car decl) '(special unspecial))
	(dolist (var (cdr decl))
	  (setf (car env)
		(list* (value-cell-location var)
		       nil
		       (car env)))
	  (if (eq (car decl) 'special)
	      (let ((slot (locf (cadr (car env)))))
		(%p-store-pointer slot (value-cell-location var))
		(%p-store-data-type slot dtp-one-q-forward))))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun eval (form &optional nohook)
  "Evaluate FORM in traditional (nonlexical) fashion, returning its value(s).
Free variables in FORM must be special."
  (cond ((and *evalhook* (not nohook))
	 (let ((tem *evalhook*)
	       *evalhook* *applyhook*)
	   (funcall tem form '(nil t))))
	((symbolp form)
	 (symeval form))
	((atom form) form)
	(t
	 (let ((interpreter-function-environment t)
	       interpreter-environment)
	   (eval1 form)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defmacro bind-variables-spread ((varlist value-list-exp decls-env) &body body)
 `(prog (vars-left bindframe vals-left thisvarloc)
	(unless ,varlist
	  (go long))  ;Trivial case of empty varlist would lose in code below.
	(when (nthcdr 20 ,varlist)
	  (setq bindframe
		(mapcan #'list*
			,varlist ,value-list-exp
			(circular-list nil)))
	  (go long))
	;; The following code is equivalent to the above mapcar
	;; except that the list is constructed on the stack
	;; by pushing the elements one by one and fiddling with cdr codes.
        (with-stack-list (tem nil)
	  ;; BINDFRAME gets a pointer to where the list will go.
	  (setq bindframe tem))
	;; Now loop over the varlist, computing and pushing initiali values.
        (setq vars-left ,varlist)
	(setq vals-left ,value-list-exp)
      short-nextvar
	(unless vars-left (go short-varsdone))
	(setq thisvarloc (value-cell-location (car vars-left)))
	(%push thisvarloc)
	(%push (car vals-left))
	(pop vars-left)
	(pop vals-left)
	(go short-nextvar)
      short-varsdone
        ;; Modify cdr-code of last word pushed, to terminate the list.
        (with-stack-list (tem nil)
	  (%p-dpb-offset cdr-nil %%q-cdr-code tem -1))
      long
        ;; Here BINDFRAME has the correct variables and values.
        ;; Now for each variable that is supposed to be special
        ;; bind it to its value (as found in BINDFRAME)
        ;; and forward the BINDFRAME slot to the variable's value cell.

	(setq vals-left bindframe)
   bindloop
   	(cond (vals-left
	       (setq thisvarloc (car vals-left))
	       (when (eq thisvarloc (value-cell-location nil))
		 (ferror nil "Attempt to bind NIL"))
	       (when (interpreter-special-in-frame-p thisvarloc ,decls-env)
		 (bind thisvarloc
		       (cadr vals-left))
		 (%p-store-data-type (locf (cadr vals-left))
				     dtp-one-q-forward)
		 (%p-store-pointer (locf (cadr vals-left))
				   thisvarloc))
	       (setq vals-left (cddr vals-left))
	       (go bindloop)))

	(return
	  (with-stack-list* (interpreter-environment bindframe interpreter-environment)
	    . ,body))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun multiple-value-bind (&quote var-list exp &rest body)
  "Evaluate EXP, collecting multiple values, and set the variables to them."
  (let ((val-list (multiple-value-list (eval1 exp))))
    (if (eq interpreter-function-environment t)
	(zl-bind-variables-spread (var-list val-list)
	  (eval-body body))
      (gobble-declarations-from-body (decls-env body)
	(bind-variables-spread (var-list val-list decls-env)
	  (eval-body body))))))

;; Produce code to bind a single variable in a special form.
;; VARIABLE-EXP should be an expression that computes the variable (a symbol)
;; and VALUE-EXP should compute the value for the variable (NOT code to compute the value).

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro bind-variable ((variable-exp value-exp decls-env) &body body)
  `(with-stack-list (frame (value-cell-location ,variable-exp) ,value-exp)
     (when (interpreter-special-in-frame-p (car frame) ,decls-env)
       (bind (car frame)
	     (cadr frame))
       (%p-store-data-type (locf (cadr frame)) dtp-one-q-forward)
       (%p-store-pointer (locf (cadr frame)) (car frame)))
     (with-stack-list* (interpreter-environment frame interpreter-environment)
       . ,body)))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun progn (&quote &rest body)
  "Evaluate all the arguments in order and return the value of the last one.
Multiple values are passed along from that argument's evaluation."
  (gobble-declarations-from-body (decls-env body)
    (eval-body body)))

;;; These functions have hair to implement the correct rules for multiple values

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun with-stack-list (&quote variable-and-elements &rest body)
  "Executes BODY with VARIABLE bound to a temporary list containing ELEMENTS.
In compiled code, the temporary list lives inside the stack, like a &REST argument.
It disappears when the WITH-STACK-LIST is exited.  No garbage is produced.
In interpreted code, this is equivalent to (LET ((VARIABLE (LIST ELEMENTS...))) BODY...)."
  (declare (arglist ((variable . elements) &rest body)))
  (if (eq interpreter-function-environment t)
      (progn
	(bind (value-cell-location (car variable-and-elements))
	      (mapcar 'eval1 (cdr variable-and-elements)))
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (bind-variable ((car variable-and-elements)
		      (mapcar 'eval1 (cdr variable-and-elements))
		      decls-env)
        (eval-body body)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun with-stack-list* (&quote variable-and-elements &rest body)
  "Executes BODY with VARIABLE bound to a temporary list equal to LIST* of ELEMENTS.
The temporary list lives inside the stack, like a &REST argument.
It disappears when the WITH-STACK-LIST is exited.  No garbage is produced."
  (declare (arglist ((variable . elements) &rest body)))
  (if (eq interpreter-function-environment t)
      (progn
	(bind (value-cell-location (car variable-and-elements))
	      (apply 'list* (mapcar 'eval1 (cdr variable-and-elements))))
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (bind-variable ((car variable-and-elements)
		      (apply 'list* (mapcar 'eval1 (cdr variable-and-elements)))
		      decls-env)
	(eval-body body)))))


))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defmacro parallel-binding-list ((varlist decls-env) &body body)
 `(prog (vars-left bindframe vals-left thisvarloc)
	(unless ,varlist
	  (go long))  ;Trivial case of empty varlist would lose in code below.
	(when (nthcdr 20 ,varlist)
	  (setq bindframe
		(mapcan #'(lambda (var)
			    (if (consp var)
				(list* (value-cell-location (car var))
				       (eval1 (cadr var)) nil)
			      (list* (value-cell-location var) nil nil)))
			,varlist))
	  (go long))
	;; The following code is equivalent to the above mapcar
	;; except that the list is constructed on the stack
	;; by pushing the elements one by one and fiddling with cdr codes.
        (with-stack-list (tem nil)
	  ;; BINDFRAME gets a pointer to where the list will go.
	  (setq bindframe tem))
	;; Now loop over the varlist, computing and pushing initiali values.
        (setq vars-left ,varlist)
      short-nextvar
	(unless vars-left (go short-varsdone))
	(setq thisvarloc
	      (value-cell-location
		(if (consp (car vars-left)) (caar vars-left) (car vars-left))))
	(%push thisvarloc)
	(%push (if (consp (car vars-left)) (eval1 (cadar vars-left))))
	(pop vars-left)
	(go short-nextvar)
      short-varsdone
        ;; Modify cdr-code of last word pushed, to terminate the list.
        (with-stack-list (tem nil)
	  (%p-dpb-offset cdr-nil %%q-cdr-code tem -1))
      long
        ;; Here BINDFRAME has the correct variables and values.
        ;; Now for each variable that is supposed to be special
        ;; bind it to its value (as found in BINDFRAME)
        ;; and forward the BINDFRAME slot to the variable's value cell.

	(setq vals-left bindframe)
   bindloop
   	(cond (vals-left
	       (setq thisvarloc (car vals-left))
	       (when (eq thisvarloc (value-cell-location nil))
		 (ferror nil "Attempt to bind NIL"))
	       (when (interpreter-special-in-frame-p thisvarloc ,decls-env)
		 (bind thisvarloc
		       (cadr vals-left))
		 (%p-store-data-type (locf (cadr vals-left))
				     dtp-one-q-forward)
		 (%p-store-pointer (locf (cadr vals-left))
				   thisvarloc))
	       (setq vals-left (cddr vals-left))
	       (go bindloop)))

	(return
	  (with-stack-list* (interpreter-environment bindframe interpreter-environment)
	    . ,body))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defmacro serial-binding-list ((varlist decls-env) &body body)
 `(with-stack-list* (interpreter-environment nil interpreter-environment)
    (prog (bindframe vars-left vals-left thisvar thisval this-specialp)
      (unless ,varlist
	(go trivial))  ;Trivial case of empty varlist would lose in code below.
      (when (nthcdr 20 ,varlist)
	(go long))

      ;; Here if varlist is less than 20 long.
      ;; Construct BINDFRAME on the stack
      ;; by pushing the elements one by one and fiddling with cdr codes.
      (with-stack-list (tem nil)
	;; BINDFRAME gets a pointer to where the list will go.
	(setq bindframe tem))
      ;; Now loop over the varlist, computing and pushing initiali values.
      (setq vars-left ,varlist)
    short-nextvar
      (unless vars-left (go varsdone))
      (setq thisvar (if (symbolp (car vars-left)) (car vars-left)
		      (caar vars-left)))
      (setq this-specialp (interpreter-special-in-frame-p
			    (value-cell-location thisvar) ,decls-env))
      (%push (value-cell-location thisvar))
      (%push (if (consp (car vars-left)) (eval1 (cadar vars-left))))
      (setf (car interpreter-environment) bindframe)
      ;; Modify cdr-code of last word pushed, to terminate the list.
      (with-stack-list (tem nil)
	(%p-dpb-offset cdr-next %%q-cdr-code tem -3)
	(%p-dpb-offset cdr-nil %%q-cdr-code tem -1)
	(setq thisval tem))
      ;; Bind the variable as special, if appropriate.
      (unless thisvar (ferror nil "Attempt to bind NIL"))
      (when this-specialp
	(bind (value-cell-location thisvar) (%p-contents-offset thisval -1))
	(%p-store-data-type (%make-pointer-offset dtp-list thisval -1)
			    dtp-one-q-forward)
	(%p-store-pointer (%make-pointer-offset dtp-list thisval -1)
			  (value-cell-location thisvar)))
      (pop vars-left)
      (go short-nextvar)

    long
      ;; Now loop over the varlist, computing and pushing initial values.
      (setq bindframe (make-list (* 2 (length ,varlist))))
      (setf (car interpreter-environment) bindframe)
      (setq vars-left ,varlist)
      (setq vals-left bindframe)
    long-nextvar
      (unless vars-left (go varsdone))
      (setq thisvar (if (symbolp (car vars-left)) (car vars-left)
		      (caar vars-left)))
      (setq this-specialp (interpreter-special-in-frame-p
			    (value-cell-location thisvar) ,decls-env))
      (setf (car vals-left) (value-cell-location thisvar))
      (setf (cadr vals-left)
	    (if (consp (car vars-left)) (eval1 (cadar vars-left))))
      ;; Bind the variable as special, if appropriate.
      (unless thisvar (ferror nil "Attempt to bind NIL"))
      (when this-specialp
	(bind (value-cell-location thisvar) (cadr vals-left))
	(%p-store-data-type (locf (cadr vals-left))
			    dtp-one-q-forward)
	(%p-store-pointer (locf (cadr vals-left))
			  (value-cell-location thisvar)))
      (pop vars-left)
      (setq vals-left (cddr vals-left))
      (go long-nextvar)

    varsdone
    trivial
      (return
	(progn . ,body)))))


))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun let (&quote varlist &rest body)
  "Binds some variables and then evaluates the BODY.
VARLIST is a list of either variables or lists (variable init-exp).
The init-exps are evaluated, and then the variables are bound.
Then the body is evaluated sequentially and the values
of the last expression in it are returned."
  (if (eq interpreter-function-environment t)
      (zl-parallel-binding-list (varlist)
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (parallel-binding-list (varlist decls-env)
	(eval-body body)))))

(fset 'encapsulation-let #'let)

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun let* (&quote varlist &rest body)
  "Like LET, but binds each variable before evaluating the initialization for the next.
Thus, each variable's initialization can refer to the values of the previous ones."
  (if (eq interpreter-function-environment t)
      (zl-serial-binding-list (varlist)
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (serial-binding-list (varlist decls-env)
	(eval-body body)))))

;;; Support for lexical function definitions (FLET and LABELS).

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun flet (&quote function-list &rest body)
  "Execute BODY with local function definitions as per FUNCTION-LIST.
Each element of FUNCTION-LIST looks like (NAME (ARGS...) BODY...).
FLET rebinds the function definition of each NAME lexically to
 (LAMBDA (ARGS...) BODY...), closed in the environment outside the FLET.
See also LABELS."
  (if (eq interpreter-function-environment t)
      (zl-parallel-function-binding-list (function-list nil nil)
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (parallel-function-binding-list (function-list t nil)
	(eval-body body)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun macrolet (&quote macro-list &rest body)
  "Execute BODY with macro function definitions as per MACRO-LIST.
Each element of MACRO-LIST looks like (NAME (ARGS...) BODY...).
MACROLET rebinds the function definition of each NAME lexically to
 a macro like the one you would get by doing
 (DEFMACRO NAME (ARGS...) BODY...)."
  (if (eq interpreter-function-environment t)
      (zl-parallel-function-binding-list (macro-list t t)
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (parallel-function-binding-list (macro-list t t)
	(eval-body body)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun labels (&quote function-list &rest body)
  "Execute BODY with local function definitions as per FUNCTION-LIST.
Each element of FUNCTION-LIST looks like (NAME (ARGS...) BODY...).
LABELS rebinds the function definition of each NAME lexically to
 (LAMBDA (ARGS...) BODY...), closed in the environment inside the LABELS.
This means that the functions defined by the LABELS can refer to
themselves and to each other.  See also FLET."
  (if (eq interpreter-function-environment t)
      (zl-parallel-function-binding-list (function-list nil nil)
	(eval-body body))
    (gobble-declarations-from-body (decls-env body)
      (parallel-function-binding-list (function-list nil nil)
	;; The values were not evaluated yet.
	;; The binding frame contains the expressions.
	;; Eval them now and store the values in their places.
	(do ((frametail (car interpreter-function-environment) (cddr frametail)))
	    ((null frametail))
	  (setf (cadr frametail)
		(eval1 `(function ,(cadr frametail)))))
	(eval-body body)))))


))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun block (&quote name &rest body)
  "Make nonlocal exit point named NAME for use with RETURN-FROM within BODY.
BODY is evaluated, and the value(s) of the last form in it are returned,
except that if RETURN-FROM is used with our NAME as its argument
during the execution of BODY, control immediately exits from this BLOCK
with values specified by the arguments to RETURN-FROM.
If NAME is NIL, RETURN can also be used to exit this block."
  (check-arg name symbolp "a symbol")
  (enter-block name
    (if (eq interpreter-function-environment t)
	(eval-body body)
      (gobble-declarations-from-body (decls-env body)
	(eval-body body)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun prog (&quote &rest prog-arguments)
  "Old fashioned form that combines a LET, a BLOCK and a TAGBODY.
Usage is (PROG name varlist body...) or (PROG varlist body...).
A non-NIL symbol is interpreted as a NAME; NIL or a cons is a VARLIST.
These two forms of usage are equivalent to
  (BLOCK name
    (BLOCK NIL
      (LET varlist
        (TAGBODY body...))))
or, in the case with no specified NAME,
  (BLOCK NIL
    (LET varlist
      (TAGBODY body...)))
BLOCK establishes the RETURN-point, LET binds the variables,
and TAGBODY executes the body and handles GO tags.
See the documentation of BLOCK, LET and TAGBODY for more information.
PROG is semi-obsolete, but too ancient to be flushed."
  (declare (arglist [progname] varlist &body body))
  (let* ((progname (and (atom (car prog-arguments))
			(car prog-arguments)))
	 (varlist (if progname
		      (second prog-arguments)
		    (first prog-arguments)))
	 (progbody (if progname
		       (cddr prog-arguments)
		     (cdr prog-arguments))))
    (check-arg progname symbolp "a symbol")
    (prog ()
	  (return
	    (if (eq interpreter-function-environment t)
		(enter-block (if (eq progname t) t nil)
		  (enter-block progname
		    (zl-parallel-binding-list (varlist)
		      (tagbody-internal progbody))))
	      (gobble-declarations-from-body (decls-env progbody)
		(parallel-binding-list (varlist decls-env)
		  (enter-block (if (eq progname t) t nil)
		    (enter-block progname
		      (tagbody-internal progbody)
		      (return nil))))))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun prog* (&quote &rest prog-arguments)
  "Old fashioned form that combines a LET*, a BLOCK and a TAGBODY.
PROG* is the same as PROG except that the variables are bound sequentially,
as in LET*, whereas PROG binds them in parallel, like LET."
  (let* ((progname (and (atom (car prog-arguments))
			(car prog-arguments)))
	 (varlist (if progname
		      (second prog-arguments)
		    (first prog-arguments)))
	 (progbody (if progname
		       (cddr prog-arguments)
		     (cdr prog-arguments))))
    (check-arg progname symbolp "a symbol")
    (prog ()
	  (return
	    (if (eq interpreter-function-environment t)
		(enter-block (if (eq progname t) t nil)
		  (enter-block progname
		    (zl-serial-binding-list (varlist)
		      (tagbody-internal progbody))))
	      (gobble-declarations-from-body (decls-env progbody)
		(serial-binding-list (varlist decls-env)
		  (enter-block (if (eq progname t) t nil)
		    (enter-block progname
		      (tagbody-internal progbody)
		      (return nil))))))))))

;; Various sorts of DOs.

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun do-internal (x &aux varlist endtest retvals oncep)
  (if (and (car x) (atom (car x)))	;OLD STYLE
      (if (neq interpreter-function-environment t)
	  (ferror nil "Maclisp old-style DO not allowed in Common Lisp.")
	(bind (value-cell-location (car x)) (eval (cadr x)))
	(do-body nil (cadddr x) nil
		 t x (cddddr x)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq interpreter-function-environment t)
	(zl-parallel-binding-list (varlist)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x)))
      (gobble-declarations-from-body (decls-env (cddr x))
	(parallel-binding-list (varlist decls-env)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x)))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun do*-internal (x &aux varlist endtest retvals oncep)
  (if (and (car x) (atom (car x)))	;OLD STYLE
      (if (neq interpreter-function-environment t)
	  (ferror nil "Maclisp old-style DO not allowed in Common Lisp.")
	(bind (value-cell-location (car x)) (eval (cadr x)))
	(do-body nil (cadddr x) nil
		 t x (cddddr x)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq interpreter-function-environment t)
	(zl-serial-binding-list (varlist)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x)))
      (gobble-declarations-from-body (decls-env (cddr x))
	(serial-binding-list (varlist decls-env)
	  (do-body oncep endtest retvals
		   nil varlist (cddr x)))))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defmacro apply-lambda-bindvar (var value decls-env &optional force-special)
  `(progn
     (setq this-specialp (or ,force-special
			     (interpreter-special-in-frame-p
			       (value-cell-location ,var) ,decls-env)))
     (unless (car interpreter-environment)
       (with-stack-list (tem t)
	 (setf (car interpreter-environment) tem)))
     (%push (value-cell-location ,var))
     (%push ,value)
     ;; Modify cdr-code of last word pushed, to terminate the list.
     (with-stack-list (tem nil)
       (%p-dpb-offset cdr-next %%q-cdr-code tem -3)
       (%p-dpb-offset cdr-nil %%q-cdr-code tem -1)
       (setq thisval tem))
     ;; Bind the variable as special, if appropriate.
     (unless ,var (ferror nil "Attempt to bind NIL"))
     (when this-specialp
       (bind (value-cell-location ,var) (%p-contents-offset thisval -1))
       (%p-store-data-type (%make-pointer-offset dtp-list thisval -1)
			   dtp-one-q-forward)
       (%p-store-pointer (%make-pointer-offset dtp-list thisval -1)
			 (value-cell-location ,var)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "


(defun cl-apply-lambda (fctn a-value-list)
    (prog apply-lambda ()
       (or (consp fctn) (go bad-function))
     tail-recurse
       (cond ((memq (car fctn) '(cli:lambda cli:subst cli:named-lambda cli:named-subst))
	      (let-if (memq (car fctn) '(cli:named-lambda cli:named-subst))
		      ((interpreter-environment nil)
		       (interpreter-function-environment nil)
		       (local-declarations nil))
		;; This won't happen when standard Common Lisp code
		;; interacts with Zetalisp code, but might as well
		;; make (APPLY '(CLI:LAMBDA ...) ...) in Zetalisp work right.
		(let-if (eq interpreter-function-environment t)
			((interpreter-function-environment nil))
		  (let* (optionalf quoteflag tem restf init this-restf specialf
			 (fctn1 (cond ((eq (car fctn) 'cli:named-lambda) (cdr fctn))
				      ((eq (car fctn) 'cli:named-subst) (cdr fctn))
				      (t fctn)))
			 (lambda-list (cadr fctn1))
			 (body (cddr fctn1))
			 (value-list a-value-list)
			 (local-declarations local-declarations)
			 this-specialp thisval  ;Used by expansion of apply-lambda-bindvar
			 keynames keyvalues keyinits keykeys keyoptfs keyflags
			 key-supplied-flags
			 allow-other-keys)
		    (and (cdr body) (stringp (car body)) (pop body))	;and doc string.
		    ;; Make a binding frame to represent any SPECIAL declarations.
		    (with-stack-list* (decls-env nil interpreter-environment)
		      ;; Find any declarations at the front of the function body
		      ;; and put them onto LOCAL-DECLARATIONS and into DECLS-ENV.
		      (gobble-declarations-internal body decls-env)
		      (with-stack-list* (interpreter-environment nil decls-env)
			(prog (thisvar vars-left vals-left)
			  ;; THISVAR is name of argument being processed.
      
			  ;; If SELF is an instance, and its instance vars aren't bound, bind them.
			  (when (and (typep self ':instance)
				     (neq self slots-bound-instance))
			    (tagbody
				(setq tem (self-binding-instances))
			     loop
				(unless tem (go exit))
				(apply-lambda-bindvar-1 (car tem) (cadr tem))
				(setq tem (cddr tem))
				(go loop)
			     exit)
			    (bind (locf slots-bound-instance) self))
		     l    (cond ((null value-list) (go lp1))
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
				 (go l1))		    ;Do next value.
				((memq (car lambda-list) '(&quote &eval))
				 (setq quoteflag (eq (car lambda-list) '&quote))
				 (go l1))
				((memq (car lambda-list) '(&special &local))
				 (setq specialf (eq (car lambda-list) '&special))
				 (go l1))
				((eq (car lambda-list) '&rest)
				 (setq this-restf t)
				 (go l1))		    ;Do next value.
				((memq (car lambda-list) lambda-list-keywords)
				 (go l1))
				((atom (car lambda-list))
				 (setq thisvar (car lambda-list)))
				((atom (caar lambda-list))
				 (setq thisvar (caar lambda-list))
				 ;; If it's &OPTIONAL (FOO NIL FOOP),
				 ;; bind FOOP to T since FOO was specified.
				 (cond ((and optionalf (cddar lambda-list))
					(and (null (caddar lambda-list)) (go bad-lambda-list))
					(apply-lambda-bindvar (caddar lambda-list)
							      t decls-env specialf))))
				(t (go bad-lambda-list)))
			  ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
			  ;;  It is in THISVAR.
			  (and (null thisvar) (go bad-lambda-list))
			  (cond (restf (go bad-lambda-list))	;Something follows a &REST arg???
				(this-restf		;This IS the &REST arg.
				 ;; If quoted arg, and the list of values is in a pdl, copy it.
				 (and quoteflag
				      (ldb-test %%pht2-map-access-code
						(area-region-bits (%area-number value-list)))
				      (let ((default-cons-area background-cons-area))
					(setq value-list (copylist value-list))))
				 (apply-lambda-bindvar thisvar value-list decls-env specialf)
				 ;; We don't clear out VALUE-LIST
				 ;; in case keyword args follow.
				 (setq this-restf nil restf t)
				 (go l1)))
      
			  (apply-lambda-bindvar thisvar (car value-list) decls-env specialf)
			  (pop value-list)
		     l1   (pop lambda-list)
			  (go l)
	
		     key  (setf (values nil nil lambda-list nil nil
					keykeys keynames keyoptfs keyinits keyflags
					allow-other-keys)
				(decode-keyword-arglist lambda-list))
			  ;; Make a list of all required keywords we haven't seen yet.
			  ;; Make alist of (keyword supplied-flag-var supplied-this-time-p)
			  (do ((keyl keykeys (cdr keyl))
			       (flagl keyflags (cdr flagl)))
			      ((null keyl))
			    (and (car flagl)
				 (push (list (car keyl) (car flagl) nil)
				       key-supplied-flags)))
	
			  (setq keyvalues (make-list (length keynames)))
			  ;; Now look at what keyword args were actually supplied.
			  ;; Set up KEYVALUES to contain values corresponding
			  ;; with the variable names in KEYNAMES.
			  (do ((vl value-list (cddr vl))
			       keyword)
			      ((null vl))
			    (or (cdr vl)
				(ferror 'sys:bad-keyword-arglist
					"No argument after keyword ~S"
					(car vl)))
			    (setq keyword (car vl))
			    retry
			    (let ((tem (find-position-in-list keyword keykeys)))
			      (cond (tem
				     (setf (nth tem keyvalues) (cadr vl))
				     (setf (nth tem keyinits) nil)
				     (let ((tem1 (assq keyword key-supplied-flags)))
				       (and tem1 (setf (caddr tem1) t))))
				    ((not allow-other-keys)
				     (setq keyword (cerror ':new-keyword nil
							   'sys:undefined-keyword-argument
							   "Keyword arg keyword ~S, with value ~S, is unrecognized."
							   keyword
							   (cadr vl)))
				     (and keyword (go retry))))))
			  ;; Eval the inits of any keyword args that were not supplied.
			  (do ((kvs keyvalues (cdr kvs))
			       (kis keyinits (cdr kis)))
			      ((null kvs))
			    (and (car kis)
				 (rplaca kvs (eval1 (car kis)))))
			  ;; Bind the supplied-flags of the optional keyword args.
			  ;; Can't use DO here because the bindings must stay around.
		     key1 (cond (key-supplied-flags
				 (apply-lambda-bindvar (cadar key-supplied-flags)
						       (caddar key-supplied-flags)
						       decls-env)
				 (pop key-supplied-flags)
				 (go key1)))
			  ;; If any required keyword args were not specified, barf.
			  ;; Actually bind the keyaord arg variables
			  (setq vals-left keyvalues
				vars-left keynames)
		     keybind
			  (unless vars-left (go lp1))
			  (apply-lambda-bindvar (car vars-left) (car vals-left) decls-env)
			  (pop vars-left)
			  (pop vals-left)
			  (go keybind)
			  ;; Keyword args always use up all the values that are left...
	
			  ;; Here when all values used up.
		     lp1  (cond ((null lambda-list) (go ex1))
				((eq (car lambda-list) '&rest)
				 (and restf (go bad-lambda-list))
				 (setq this-restf t)
				 (go lp2))
				((eq (car lambda-list) '&key)
				 (go key))
				((memq (car lambda-list) '(&optional &aux))
				 (setq optionalf t)		;SUPPRESS TOO FEW ARGS ERROR
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
						      "Function ~S called with only ~D argument~1G~P."
						      fctn (length a-value-list) a-value-list))
				     (:additional-arguments
				      (apply fctn (append a-value-list args)))
				     (:return-value args)
				     (:new-argument-list (apply fctn args)))))
				((atom (car lambda-list)) (setq tem (car lambda-list))
				 (setq init nil))
				((atom (caar lambda-list))
				 (setq tem (caar lambda-list))
				 (setq init (eval (cadar lambda-list)))
				 ;; For (FOO NIL FOOP), bind FOOP to NIL since FOO is missing.
				 (cond ((cddar lambda-list)
					(and (null (caddar lambda-list)) (go bad-lambda-list))
					(apply-lambda-bindvar (caddar lambda-list)
							      nil decls-env specialf))))
				(t (go bad-lambda-list)))
		     lp3  (and (null tem) (go bad-lambda-list))
			  (apply-lambda-bindvar tem init specialf)
			  (and this-restf (setq restf t))
			  (setq this-restf nil)
		     lp2  (setq lambda-list (cdr lambda-list))
			  (go lp1)
    
		     ex1  ;; Here to evaluate the body.
			  (return-from apply-lambda (eval-body body))
		     bad-lambda-list
			  (setq fctn
				(cerror ':new-function nil 'sys:invalid-lambda-list
					"~S has an invalid LAMBDA list" fctn))
		     retry
			  (return-from apply-lambda (apply fctn a-value-list)))))))))
	     ((eq (car fctn) 'macro)
              (ferror 'sys:funcall-macro
		      "Funcalling the macro ~S."
		      (function-name (cdr fctn)))
	      (return-from apply-lambda
			   (eval (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list)))))
	     )

       ;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
       (cond ((lambda-macro-call-p fctn)
	      (setq fctn (lambda-macro-expand fctn))
	      (go retry)))

       bad-function
       ;; Can drop through to here for a totally unrecognized function.
       (setq fctn
	     (cerror ':new-function nil 'sys:invalid-function
		     "~S is an invalid function." fctn))
       (go retry)

       retry
       (and (consp fctn) (go tail-recurse))
       (return (apply fctn a-value-list))))

;DECODE-KEYWORD-ARGLIST

;Given a lambda list, return a decomposition of it and a description
;of all the keyword args in it.
;POSITIONAL-ARGS is the segment of the front of the arglist before any keyword args.
;KEYWORD-ARGS is the segment containing the keyword args.
;AUXVARS is the segment containing the aux vars.
;REST-ARG is the name of the rest arg, if any, else nil.
;POSITIONAL-ARG-NAMES is a list of all positional args
; and the supplied-flags of all optional positional args.
;The rest of the values describe the keyword args.
;There are several lists, equally long, with one element per arg.
;KEYNAMES contains the keyword arg variable names.
;KEYKEYS contains the key symbols themselves (in the keyword package).
;KEYOPTFS contains T for each optional keyword arg, NIL for each required one.
;KEYINITS contains for each arg the init-form, or nil if none.
;KEYFLAGS contains for each arg its supplied-flag's name, or nil if none.
;Finally,
;ALLOW-OTHER-KEYS is T if &ALLOW-OTHER-KEYS appeared among the keyword args.

))

; From file ADVISE.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "


(deff advise-prog #'prog)
(deff advise-setq #'setq)
(deff advise-progn #'progn)
(deff advise-multiple-value-list #'multiple-value-list)
(deff advise-return-list #'return-list)
(deff advise-apply #'apply)
(deff advise-let #'let)
(deff advise-list* #'list*)

))
