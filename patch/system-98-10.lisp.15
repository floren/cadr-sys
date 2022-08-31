;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.10
;;; Reason: READ-DELIMITED-STRING.  NTH-VALUE.  PATCH-LOADED-P.  *DEBUG-IO-OVERRIDE*.
;;; MAKE-PROCESS-QUEUE, etc.  Calling non-CL NAMED-LAMBDAs from CL code.
;;; UNADVISE improvement.  Better pretty names for (INTEGER 0), (FLOAT 0).
;;; MARGIN-SPACE-MIXIN.  :PAST-DATE in CVV windows.
;;; Look in SYS: SITE; foo SYSTEM file without querying.
;;; Interpreted MULTIPLE-VALUE bug.  SHIFTF bug.
;;; Record system major version in sys com area.
;;; Record microcode version so brand S bands can see it.
;;; OPEN: :PROBE-DIRECTORY, :PROBE-LINK.  WITH-OPEN-FILE-SEARCH.
;;; FS:CREATE-LINK.  LINEARIZE-PATHNAME-PLISTS bug.  FULL-GC bug.
;;; Pathname mapping bug.  COPY-FILE takes keyword args.
;;; Flavor redefinition bug.
;;; Add Patch printout bug.  DEFINITION-NAME-AS-STRING bug.
;;; M-X Delete/Undelete/Rename/Copy File improvements.
;;; Written 12/16/83 19:55:43 by RMS,
;;; while running on Lisp Machine Eighteen from band 7
;;; with Bad Inconsistently updated System 98.9, CADR 3.1, Experimental ZMail 53.5, MIT-Specific 22.0, microcode 305, ZM MIT.


(globalize "READ-DELIMITED-STRING")
(globalize "WITH-OPEN-FILE-SEARCH")
(globalize "READLINE-OR-NIL")
(globalize "NTH-VALUE")

; From file ADVISE.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "


(defmacro unadvise (&optional function-spec class position)
  "Remove some or all advice from FUNCTION-SPEC, or from all functions.
With no arguments, all advice is removed.  This is a consequence of these rules:
If FUNCTION-SPEC is non-NIL, advice is removed from that function only.
 Otherwise, advice is removed from all functions if the other args match.
If CLASS is non-NIL, only advice of that class is removed.
If POSITION is non-NIL (a number or name), only advice with that positoin is removed."
  (cond ((null function-spec)
	 `(dolist (fn advised-functions)
	    (unadvise-1 fn ',class ',position)))
	(t `(unadvise-1 ',function-spec ',class ',position))))

(defun unadvise-1 (function-spec &optional class position)
  (setq function-spec (dwimify-arg-package function-spec 'function))
  (and (member function-spec advised-functions) (advise-init function-spec))
  (check-type class (member nil :before :after :around))
  (check-type position (or symbol (integer 0)))
  (let* ((spec1 (unencapsulate-function-spec function-spec 'advise)))
    (dolist (slot-location
	      (if class (list (advise-find-slot spec1 class))
		(list (advise-find-slot spec1 ':before)
		      (advise-find-slot spec1 ':after)
		      (advise-find-slot spec1 ':around))))
      ;; For each slot we are supposed to operate on,
      ;; remove any advice that matches POSITION.
      (cond ((null position)
	     (rplaca slot-location nil))
	    ((numberp position)
	     (let ((preceding (nthcdr position (locf (car slot-location)))))
	       (when (cdr preceding) (rplacd preceding (cddr preceding)))))
	    ((symbolp position)
	     (do ((l (locf (car slot-location)) (cdr l)))
		 ((null l))
	       (and (eq (cadadr (cadr l)) position)
		    (return (rplacd l (cddr l))))))))
    ;; Flush the encapsulation if there is no advice in it.
    (and (null (car (advise-find-slot spec1 ':before)))
	 (null (car (advise-find-slot spec1 ':after)))
	 (null (car (advise-find-slot spec1 ':around)))
	 (let ((olddef (fdefinition (unencapsulate-function-spec spec1 '(advise)))))
	   (cond ((eq (car (fdefinition spec1)) 'macro)
		  (setq olddef (cons 'macro olddef))))
	   (fdefine spec1 olddef)
	   (setq advised-functions (delete function-spec advised-functions))))
    (if compile-encapsulations-flag
	(compile-encapsulations function-spec 'advise))
    nil))

))

; From file TYPES.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun type-pretty-name-1 (type)
  (cond ((symbolp type)
	 (if (or (get type 'type-name)
		 (string-append-a-or-an
		   (string-subst-char #\sp #/-
				      (string-downcase type))))
	     (string-append "a " (string-downcase (format nil "~a" type)))))
	((member type '((integer 0) (float 0)))
	 (string-append "a positive " (string-downcase (symbol-name (car type)))))
	((and (eq (car type) 'or)
	      (dolist (elt (cdr type) t)
		(unless (type-pretty-name-1 elt) (return nil))))
	 (format:output nil
	   (do ((tail (cdr type) (cdr tail)))
	       ((null tail))
	     (unless (cdr tail)
	       (princ "or "))
	     (princ (type-pretty-name-1 (car tail)))
	     (when (cdr tail)
	       (if (cddr tail)
		   (princ ", ")
		 (tyo #\sp))))))
	((eq (car type) 'member)
	 (format:output nil
	   (do ((tail (cdr type) (cdr tail)))
	       ((null tail))
	     (unless (cdr tail)
	       (princ "or "))
	     (prin1 (car tail))
	     (when (cdr tail)
	       (if (cddr tail)
		   (princ ", ")
		 (tyo #\sp))))))))

))

; From file PATCH.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN PATCH-LOADED-P (MAJOR-VERSION MINOR-VERSION &OPTIONAL (SYSTEM "SYSTEM"))
  "T if specified patch to patchable system SYSTEM is now loaded.
The patch specified is the one with numbers MAJOR-VERSION and MINOR-VERSION.
If the actual loaded major version is greater than MAJOR-VERSION
then the answer is T regardless of MINOR-VERSION, on the usually-true assumption
that the newer system contains everything patched into the older one."
  (LET* ((PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED SYSTEM T T))
	 (CURRENT-MAJOR-VERSION (PATCH-VERSION PATCH-SYSTEM)))
    (OR (> CURRENT-MAJOR-VERSION MAJOR-VERSION)
	(AND (= CURRENT-MAJOR-VERSION MAJOR-VERSION)
	     (>= (OR (VERSION-NUMBER (CAR (PATCH-VERSION-LIST PATCH-SYSTEM))) 0)
		 MINOR-VERSION)))))

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun apply-lambda (fctn a-value-list)
    (prog apply-lambda (tem)
       (or (consp fctn) (go bad-function))
       tail-recurse
       (cond ((eq (car fctn) 'curry-after)
	      (prog ()
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
	     ((eq (car fctn) 'curry-before)
	      (prog ()
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
	     ((memq (car fctn) '(cli:lambda cli:named-lambda cli:subst cli:named-subst))
	      (return (cl-apply-lambda fctn a-value-list)))
	     ((memq (car fctn) '(lambda named-lambda subst named-subst))
	      (let* (optionalf quoteflag tem restf init this-restf
		     (fctn (cond ((eq (car fctn) 'named-lambda) (cdr fctn))
				 ((eq (car fctn) 'named-subst) (cdr fctn))
				 (t fctn)))
		     (lambda-list (cadr fctn))
		     (value-list a-value-list)
		     (local-declarations local-declarations)
		     keynames keyvalues keyinits keykeys keyflags
		     key-supplied-flags
		     allow-other-keys)
		(setq fctn (cddr fctn))	;throw away lambda list
		(do () (())
		  (cond ((and (cdr fctn) (stringp (car fctn)))
			 (pop fctn))	;and doc string.
			;; Process any (DECLARE) at the front of the function.
			;; This does not matter for SPECIAL declarations,
			;; but for MACRO declarations it might be important
			;; even in interpreted code.
			((and (not (atom (car fctn)))
			      (memq (caar fctn) '(declare :declare)))
			 (setq local-declarations (append (cdar fctn) local-declarations))
			 (pop fctn))
			(t (return))))
		(prog ()
		  (when (memq (car fctn) '(named-lambda named-subst))
		    (bind (locf interpreter-environment) nil)
		    (bind (locf interpreter-function-environment) t))
		  ;; If SELF is an instance, and its instance vars aren't bound, bind them.
		  (and (typep self ':instance)
		       (neq self slots-bound-instance)
		       (progn (%using-binding-instances (self-binding-instances))
			      (bind (locf slots-bound-instance) self)))
	     l    (cond ((null value-list) (go lp1))
			((or (null lambda-list)
			     (eq (car lambda-list) '&aux)) 
			 (cond (restf (go lp1))
			       (t (go too-many-args))))
			((eq (car lambda-list) '&key)
			 (go key))
			((eq (car lambda-list) '&optional)
			 (setq optionalf t)
			 (go l1))		    ;Do next value.
			((memq (car lambda-list) '(&quote &eval))
			 (setq quoteflag (eq (car lambda-list) '&quote))
			 (go l1))
			((eq (car lambda-list) '&rest)
			 (setq this-restf t)
			 (go l1))		    ;Do next value.
			((memq (car lambda-list) lambda-list-keywords)
			 (go l1))
			((atom (car lambda-list)) (setq tem (car lambda-list)))
			((atom (caar lambda-list))
			 (setq tem (caar lambda-list))
			 ;; If it's &OPTIONAL (FOO NIL FOOP),
			 ;; bind FOOP to T since FOO was specified.
			 (cond ((and optionalf (cddar lambda-list))
				(and (null (caddar lambda-list)) (go bad-lambda-list))
				(bind (value-cell-location (caddar lambda-list)) t))))
			(t (go bad-lambda-list)))
		  ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
		  ;;  It is in TEM.
		  (and (null tem) (go bad-lambda-list))
		  (cond (restf (go bad-lambda-list))	;Something follows a &REST arg???
			(this-restf		;This IS the &REST arg.
			 ;; If quoted arg, and the list of values is in a pdl, copy it.
			 (and quoteflag
			      (ldb-test %%pht2-map-access-code
					(area-region-bits (%area-number value-list)))
			      (let ((default-cons-area background-cons-area))
				(setq value-list (copylist value-list))))
			 (bind (locf (symeval tem)) value-list)
			 ;; We don't clear out VALUE-LIST
			 ;; in case keyword args follow.
			 (setq this-restf nil restf t)
			 (go l1)))
		  (bind (value-cell-location tem) (car value-list))
		  (setq value-list (cdr value-list))
	     l1   (setq lambda-list (cdr lambda-list))
		  (go l)

	     key  (setf (values nil nil lambda-list nil nil
				keykeys keynames nil keyinits keyflags
				allow-other-keys)
			(decode-keyword-arglist lambda-list))
		  ;; Process the special keyword :ALLOW-OTHER-KEYS if present as an arg.
		  (if (get (locf value-list) ':allow-other-keys)
		      (setq allow-other-keys t))
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
		       keyword (found-flags 0))
		      ((null vl))
		    (or (cdr vl)
			(ferror 'sys:bad-keyword-arglist
				"No argument after keyword ~S"
				(car vl)))
		    (setq keyword (car vl))
		    retry
		    (let ((tem (find-position-in-list keyword keykeys)))
		      (cond (tem
			     (when (zerop (logand 1 (ash found-flags (- tem))))
			       (setq found-flags
				     (dpb 1 (byte 1 tem) found-flags))
			       (setf (nth tem keyvalues) (cadr vl))
			       (setf (nth tem keyinits) nil)
			       (let ((tem1 (assq keyword key-supplied-flags)))
				 (and tem1 (setf (caddr tem1) t)))))
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
			 (bind (locf (symeval (cadar key-supplied-flags)))
			       (caddar key-supplied-flags))
			 (pop key-supplied-flags)
			 (go key1)))
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
			((memq (car lambda-list) lambda-list-keywords)
			 (go lp2))
			((and (null optionalf) (null this-restf))
			 (and restf (go bad-lambda-list))
			 (go too-few-args))
			((atom (car lambda-list)) (setq tem (car lambda-list))
			 (setq init nil))
			((atom (caar lambda-list))
			 (setq tem (caar lambda-list))
			 (setq init (eval1 (cadar lambda-list)))
			 ;; For (FOO NIL FOOP), bind FOOP to NIL since FOO is missing.
			 (cond ((cddar lambda-list)
				(and (null (caddar lambda-list)) (go bad-lambda-list))
				(bind (value-cell-location (caddar lambda-list)) nil))))
			(t (go bad-lambda-list)))
	     lp3  (and (null tem) (go bad-lambda-list))
		  (bind (value-cell-location tem) init)
		  (and this-restf (setq restf t))
		  (setq this-restf nil)
	     lp2  (setq lambda-list (cdr lambda-list))
		  (go lp1)

	     ex1  ;; Here to evaluate the body.
		  ;; First bind the keyword args if any.
		  (progv keynames keyvalues
			 (do ((l fctn (cdr l)))
			     ((null (cdr l))
			      (return-from apply-lambda (eval1 (car l))))
			   (eval1 (car l)))))))
	     ((eq (car fctn) 'macro)
              (ferror 'sys:funcall-macro
		      "Funcalling the macro ~S."
		      (function-name (cdr fctn)))
	      (return-from apply-lambda
			   (eval1 (cons fctn (mapcar #'(lambda (arg) `',arg) a-value-list)))))
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

       ;; Errors jump out of the inner PROG to unbind any lambda-vars bound with BIND.

       bad-lambda-list
       (setq fctn
	     (cerror ':new-function nil 'sys:invalid-lambda-list
		     "~S has an invalid LAMBDA list" fctn))
       retry
       (and (consp fctn) (go tail-recurse))
       (return (apply fctn a-value-list))

       too-few-args
       (return (signal-proceed-case
		 ((args)
		  (make-condition 'sys:too-few-arguments
		       "Function ~S called with only ~D argument~1G~P."
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

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFSIGNAL MULTIPLE-FILE-NOT-FOUND FILE-LOOKUP-ERROR (OPERATION PATHNAME PATHNAMES)
	   "None of the files was found in the containing directory.")

))

; From file LMMAC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO WITH-OPEN-FILE-SEARCH ((STREAM (OPERATION DEFAULTS AUTO-RETRY)
					 TYPE-LIST-AND-PATHNAME-FORM
					 . OPEN-OPTIONS)
				 &BODY BODY)
  "Open one of several filenames, the same except for the type component.
Binds the variable STREAM to the resulting stream, executes the BODY, then closes the stream.
OPEN-OPTIONS are alternating keywords and values, passed to OPEN.
TYPE-LIST-AND-PATHNAME-FORM is evaluated to get two values:
 a list of pathname types to try, and a base pathname.
The base pathname is merged successively with each type in the list.
This is done using FS:MERGE-PATHNAME-DEFAULTS, with DEFAULTS's value
used as the second argument and the type to be tried as the third argument.
As soon as a merged pathname succeeds in being opened, we execute BODY.
If they all fail, an error is signaled with condition FS:MULTIPLE-FILE-NOT-FOUND.
OPERATION should eval to the name of the calling function; it is used for signaling.
If AUTO-RETRY evals to non-NIL, then the user is asked to type a new
pathname to retry with."
  (LET ((BASE-PATHNAME-VAR (GENSYM))
	(TYPE-LIST-VAR (GENSYM))
	(DEFAULTS-VAR (GENSYM))
	(AUTO-RETRY-VAR (GENSYM)))
    `(LET ((,DEFAULTS-VAR ,DEFAULTS)
	   (,AUTO-RETRY-VAR ,AUTO-RETRY))
       (MULTIPLE-VALUE-BIND (,TYPE-LIST-VAR ,BASE-PATHNAME-VAR)
	   ,TYPE-LIST-AND-PATHNAME-FORM
	 (FILE-RETRY-NEW-PATHNAME-IF ,AUTO-RETRY-VAR (,BASE-PATHNAME-VAR . FILE-ERROR)
	   (WITH-OPEN-STREAM (,STREAM
			      (FS:OPEN-FILE-SEARCH ,BASE-PATHNAME-VAR ,TYPE-LIST-VAR
						   ,DEFAULTS-VAR ,OPERATION
						   . ,OPEN-OPTIONS))
	     . ,BODY))))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN LOAD-1 (FILE &OPTIONAL PKG NONEXISTENT-OK-FLAG DONT-SET-DEFAULT-P NO-MSG-P)
  ;; Merge everything, defaulting type component to NIL.
  (IF (STREAMP FILE)
      (PROGN
	;; Set the defaults from the pathname we finally opened
	(OR DONT-SET-DEFAULT-P
	    (SET-DEFAULT-PATHNAME (SEND FILE ':PATHNAME) LOAD-PATHNAME-DEFAULTS))
	(CATCH-ERROR-RESTART (ERROR "Give up on loading ~A." (SEND FILE ':PATHNAME))
	  ;; If the file was a character file, read it, else try to fasload it.
	  (FUNCALL (IF (FUNCALL FILE ':CHARACTERS)
		       #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
		   FILE PKG NO-MSG-P)
	  (OR (SEND FILE ':SEND-IF-HANDLES ':TRUENAME) T)))
    (LET ((PATHNAME (PARSE-PATHNAME FILE)))
      (CATCH-ERROR-RESTART (ERROR "Give up on loading ~A." FILE)
	(CONDITION-CASE-IF NONEXISTENT-OK-FLAG ()
	    (WITH-OPEN-FILE-SEARCH (STREAM ('LOAD LOAD-PATHNAME-DEFAULTS
					    (NOT NONEXISTENT-OK-FLAG))
					   (VALUES
					     (LIST (SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE
						     PATHNAME)
						   ':LISP)
					     PATHNAME)
					   ':CHARACTERS ':DEFAULT)
	      ;; Set the defaults from the pathname we finally opened
	      (OR DONT-SET-DEFAULT-P
		  (SET-DEFAULT-PATHNAME (SEND STREAM ':PATHNAME) LOAD-PATHNAME-DEFAULTS))
	      ;; If the file was a character file, read it, else try to fasload it.
	      (FUNCALL (IF (FUNCALL STREAM ':CHARACTERS)
			   #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
		       STREAM PKG NO-MSG-P)
	      (SEND STREAM ':TRUENAME))
	  (MULTIPLE-FILE-NOT-FOUND
	   NIL))))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN OPEN-FILE-SEARCH (BASE-PATHNAME TYPE-LIST DEFAULTS FOR-FUNCTION &REST OPEN-OPTIONS)
  (COND ((NULL (PATHNAME-RAW-TYPE BASE-PATHNAME)))
	((AND (EQ (PATHNAME-RAW-TYPE BASE-PATHNAME) ':UNSPECIFIC)
	      (SEND BASE-PATHNAME ':UNSPECIFIC-TYPE-IS-DEFAULT))
	 ;; If type is really insignificant, replace it with NIL
	 ;; so we will get the same behavior as if it were already NIL.
	 (SETQ BASE-PATHNAME (SEND BASE-PATHNAME ':NEW-TYPE NIL)))
	;; Otherwise, will use only the specified type,
	;; so the elements of TYPE-LIST matter only in how many they are,
	;; and we might as well have only one to avoid wasting time on duplicate opens.
	(T (SETQ TYPE-LIST '(NIL))))
  (DOLIST (TYPE TYPE-LIST
		(FERROR 'FS:MULTIPLE-FILE-NOT-FOUND
			"~S could not find any file related to ~A."
			FOR-FUNCTION BASE-PATHNAME
			(MAPCAR #'(LAMBDA (TYPE)
				    (FS:MERGE-PATHNAME-DEFAULTS
				      BASE-PATHNAME DEFAULTS TYPE))
				TYPE-LIST)))
    (CONDITION-CASE (OPEN-VALUE)
	(APPLY 'OPEN (FS:MERGE-PATHNAME-DEFAULTS
		       BASE-PATHNAME DEFAULTS TYPE)
	       OPEN-OPTIONS)
      (FILE-NOT-FOUND)
      (:NO-ERROR (RETURN OPEN-VALUE)))))

))

; From file MAKSYS.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN FIND-SYSTEM-NAMED (NAME &OPTIONAL NO-ERROR-P LOADED-ONLY)
  "Return the system object whose name is NAME.
NO-ERROR-P says return NIL if no such system, rather than getting error.
LOADED-ONLY says ignore systems whose DEFSYSTEMs have not been executed."
  ;; LOADED-ONLY = SI:FOO used internally to mean
  ;; do reload system source file but don't check SYS: SITE;.
  (IF (TYPEP NAME 'SYSTEM) NAME
      (OR (DOLIST (SYSTEM *SYSTEMS-LIST*)
	    (COND ((TYPEP SYSTEM 'SYSTEM)
		   (AND (OR (STRING-EQUAL NAME (SYSTEM-NAME SYSTEM))
			    (MEM #'STRING-EQUAL NAME (SYSTEM-NICKNAMES SYSTEM)))
			(RETURN SYSTEM)))
		  ((AND (MEMQ LOADED-ONLY '(NIL FOO))
			(STRING-EQUAL NAME SYSTEM))
		   (MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM '(:NOCONFIRM))
		   (LET ((RETRY (FIND-SYSTEM-NAMED NAME T T)))
		     (IF RETRY (RETURN RETRY)
			 (FERROR NIL "~A did not contain a definition of ~A."
				 (SEND (GET-SOURCE-FILE-NAME SYSTEM 'DEFSYSTEM) ':SOURCE-PATHNAME)
				 SYSTEM))))))
	  (AND (NOT LOADED-ONLY)
	       (LET ((PATHNAME (FS:PARSE-PATHNAME (STRING-APPEND "SYS: SITE; " NAME " SYSTEM"))))
		 (WHEN (LOAD PATHNAME ':IF-DOES-NOT-EXIST T ':SET-DEFAULT-PATHNAME NIL
			     ':VERBOSE T)
		   (FIND-SYSTEM-NAMED NAME NO-ERROR-P 'FOO))))
	  (IF NO-ERROR-P NIL
	      (FERROR NIL "System ~S not found" NAME)))))

))

; From file ADVISE.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defun unadvise-within-1 (within-function-spec &optional advised-function class position)
  (if (and within-function-spec advised-function)
      (unadvise-1 `(:within ,within-function-spec ,advised-function) class position)
    (dolist (fn advised-functions)
      (when (and (consp fn)
		 (eq (car fn) ':within)
		 (or (null within-function-spec)
		     (eq within-function-spec (second fn)))
		 (or (null advised-function)
		     (eq advised-function (third fn))))
	(unadvise-1 fn class position)))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFVAR *DEBUG-IO-OVERRIDE* NIL
  "If non-NIL, this is used instead of DEBUG-IO by the debugger.")

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN INVOKE-DEBUGGER-FOR-EH (TERMINAL-IO CELL)
  (LET ((*DEBUG-IO-OVERRIDE* SI:SYN-TERMINAL-IO)
	(CONDITION-PROCEED-TYPES '(:NO-ACTION)))
    (UNWIND-PROTECT
      (LET ((ERROR-DEPTH (1+ ERROR-DEPTH)))
	(INVOKE-DEBUGGER (MAKE-CONDITION 'BREAK)))
      (SETF (CAR CELL) T))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

;; This is the function at the top level in each second level error handler sg.
(DEFUN SECOND-LEVEL-ERROR-HANDLER (SG EH-ERROR &OPTIONAL (IGNORE T)
				   ;; (IGNORE T) is never passed by callers.
				   ;; It forces off the fast arg option
				   ;; which persuades the compiler to bind
				   ;; INHIBIT-SCHEDULING-FLAG at function entry,
				   ;; preventing an abort at beginning of this function.
				   &AUX MSG
				   (INHIBIT-SCHEDULING-FLAG T)
				   (SI:PRINT-READABLY NIL)
				   (PACKAGE SI:PKG-USER-PACKAGE)
				   (DEFAULT-CONS-AREA ERROR-HANDLER-AREA)
				   SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD
				   (ERROR-HANDLER-RUNNING T)
				   (ERROR-HANDLER-REPRINT-ERROR T)
				   (TERMINAL-IO (OR (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						      '*DEBUG-IO-OVERRIDE* SG)
						    (FOLLOW-SYN-STREAM-IN-STACK-GROUP
						      'DEBUG-IO SG)
						    (SYMEVAL-IN-STACK-GROUP 'TERMINAL-IO SG)))
				   (STANDARD-INPUT SI:SYN-TERMINAL-IO)
				   (STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
				   (QUERY-IO SI:SYN-TERMINAL-IO)
				   (DEBUG-IO SI:SYN-TERMINAL-IO)
				   (*DEBUG-IO-OVERRIDE* NIL)
				   ;; In case we want to set CURRENT-PROCESS to nil.
				   (CURRENT-PROCESS CURRENT-PROCESS)
				   CURRENT-FRAME ERROR-LOCUS-FRAME
				   INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
  (UNLESS TERMINAL-IO
    (SETQ MSG "TERMINAL-IO is NIL"))
  (COND ((EQ SG SI:SCHEDULER-STACK-GROUP)
	 (SETQ MSG "Error in the scheduler"))
	((AND (BOUNDP 'TV:KBD-PROCESS)
	      (EQ CURRENT-PROCESS TV:KBD-PROCESS))
	 (SETQ MSG "Error in the keyboard process"))
	((AND (BOUNDP 'TV:MOUSE-PROCESS)
	      (EQ CURRENT-PROCESS TV:MOUSE-PROCESS))
	 (SETQ MSG "Error in the mouse process")))
  ;; Get rid of call to error-handler sg
  (LET ((RP (SG-REGULAR-PDL SG)) (AP (SG-AP SG)))
    (IF (NEQ (AREF RP AP) %ERROR-HANDLER-STACK-GROUP)
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP not found on pdl where expected"))
    (IF ( (RP-DESTINATION RP AP) 0)		;D-IGNORE
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP called with bad destination"))
    (IF ( (SG-REGULAR-PDL-POINTER SG) (1+ AP))
	(FERROR NIL "%ERROR-HANDLER-STACK-GROUP called with wrong number of args"))
    (SETF (SG-IPMARK SG) (SG-NEXT-OPEN SG AP))
    (SETF (SG-AP SG) (SETQ AP (SG-NEXT-ACTIVE SG AP)))
    (SETF (SG-FLAGS-QBBFL SG)			;Must correspond to current frame to work!
	  (RP-BINDING-BLOCK-PUSHED RP AP))
    (DOTIMES (I 5)				;Pop p3zero, function, and arg
      (SG-REGPDL-POP SG))
    ;; Now, if current frame is a foothold, restore to the previous state.  This will
    ;; normally be the case for :BREAK
    (IF (EQ (AREF RP AP) #'FOOTHOLD) (SG-RESTORE-STATE SG 0)))
  ;; Handle weird things like (BREAK): create a condition-object.
  (IF (CONSP EH-ERROR)
      (SETQ EH-ERROR (APPLY 'MAKE-CONDITION EH-ERROR)))
  (SETF (SG-TRAP-TAG SG) EH-ERROR)
  ;; Clear the SG's trap-on-call flag so that our uses of SG-APPLY will not trap.
  ;; The SG-RESTORE-STATE, above, may have restored the flag to 1.
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 0)
  (ASSURE-DISPATCH-SET-UP)
  (ASSURE-FREE-SPACE)
  (AND MSG (USE-COLD-LOAD-STREAM MSG))
  ;; Turn on interrupts if not in cold load stream.
  (UNLESS (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL))
  ;; If not running in the scheduler, give us a run reason in case we died after
  ;; becoming inactive, before getting back to the scheduler.
  (OR (NULL CURRENT-PROCESS)
      (FUNCALL CURRENT-PROCESS ':RUN-REASON CURRENT-STACK-GROUP))
  ;; Try to see if TERMINAL-IO is reasonable and if not fix it.
  (LET ((WO (ERRSET (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS) NIL))
	(ERROR-HANDLER-REPRINT-ERROR NIL))
    (IF (NULL WO) (USE-COLD-LOAD-STREAM "TERMINAL-IO clobbered")
      (COND ((MEMQ ':NOTICE (CAR WO))
	     (DO () (())
	       (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Continue entering the debugger.")
		 (LET (;; :NOTICE can change TERMINAL-IO of a background process
		       (OLD-TIO TERMINAL-IO)
		       ;; Send this message in non-erring stack
		       (WINDOW-BAD (FUNCALL TERMINAL-IO ':NOTICE ':ERROR)))
		   (IF (EQ WINDOW-BAD 'TV:COLD-LOAD-STREAM)
		       (USE-COLD-LOAD-STREAM "window-system problems")
		     (AND (NEQ TERMINAL-IO OLD-TIO)
			  (NOT WINDOW-BAD)
			  (SG-FUNCALL SG #'SET 'TERMINAL-IO TERMINAL-IO))))
		 (RETURN NIL)))))))
  ;; Turn off interrupts if switched to cold load stream.
  (IF (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
      (SETQ INHIBIT-SCHEDULING-FLAG T))
  (IF (VARIABLE-BOUNDP TV:COLD-LOAD-STREAM-OWNS-KEYBOARD)
      (SETQ SAVED-COLD-LOAD-STREAM-OWNS-KEYBOARD TV:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (LET-GLOBALLY-IF (EQ SI:COLD-LOAD-STREAM TERMINAL-IO)
		   ((TV:COLD-LOAD-STREAM-OWNS-KEYBOARD T))
    ;; Setting this causes the previous error to be reprinted if we abort to it.
    (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG CURRENT-STACK-GROUP)
    ;; Give these reasonable values in case of error in the :FIND-CURRENT-FRAME method.
    (SETQ ERROR-LOCUS-FRAME (SG-AP SG)
	  CURRENT-FRAME (SG-AP SG)
	  INNERMOST-VISIBLE-FRAME (SG-AP SG))
    ;; These catches are so that quitting out of the printing of the error message
    ;; leaves you in the error handler at its
    ;; normal command level rather than quitting out of the whole program.
    (*CATCH 'QUIT
      (CATCH-ERROR-RESTART ((ERROR SYS:ABORT) "Abort printing error message, enter debugger.")
	(SETF (VALUES ERROR-LOCUS-FRAME CURRENT-FRAME
		      INNERMOST-VISIBLE-FRAME INNERMOST-FRAME-IS-INTERESTING)
	      (SEND EH-ERROR ':FIND-CURRENT-FRAME SG))
	;; Print the error message, using appropriate package, base, etc.
	(INHERITING-VARIABLES-FROM (SG)
	  (PRINT-CAREFULLY "error message"
	    (SEND STANDARD-OUTPUT ':FRESH-LINE)
	    (SEND EH-ERROR ':PRINT-ERROR-MESSAGE
		  SG NIL STANDARD-OUTPUT))
	  (PRINT-BRIEF-ERROR-BACKTRACE SG EH-ERROR)
	  (SEND EH-ERROR ':MAYBE-CLEAR-INPUT STANDARD-INPUT))))
    ;; Offer any special commands, such as wrong-package correction.
    ;; Then enter the command loop.
    (SEND EH-ERROR ':DEBUGGER-COMMAND-LOOP SG)))

(DEFUN FOLLOW-SYN-STREAM-IN-STACK-GROUP (SYM SG)
  "Evaluate SYM as an i//o stream in stack group SG, and trace synonyms.
That is, if SYM turns out to be TERMINAL-IO-SYN-STREAM,
we get the value of TERMINAL-IO in SG."
  (LOOP AS VAL = (SYMEVAL-IN-STACK-GROUP SYM SG) WITH (PTR) DO
    (COND ((AND (SYMBOLP VAL)
		(= (%P-DATA-TYPE (FUNCTION-CELL-LOCATION VAL))
		   DTP-EXTERNAL-VALUE-CELL-POINTER)
		(= (%POINTER-DIFFERENCE
		     (SETQ PTR (%P-CONTENTS-AS-LOCATIVE (FUNCTION-CELL-LOCATION VAL)))
		     (SETQ PTR (%FIND-STRUCTURE-HEADER PTR)))
		   1)
		(SYMBOLP PTR))
	   (SETQ SYM PTR))
	  (T (RETURN VAL)))))

))

; From file PRODEF.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PRODEF  "

(DEFSTRUCT (PROCESS-QUEUE :NAMED-ARRAY-LEADER (:CONSTRUCTOR MAKE-PROCESS-QUEUE-INTERNAL))
  NAME)
))

; From file PROCES.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PROCES  "


(DEFUN MAKE-PROCESS-QUEUE (NAME SIZE)
  "Makes a process queue whose name is NAME and which can hold SIZE elements.
SIZE matters only in that if more than that many objects are put on the queue
then strict queueing behavior is not guaranteed for objects past the SIZE'th one."
  (MAKE-PROCESS-QUEUE-INTERNAL :NAME NAME
			       :MAKE-ARRAY (:DIMENSIONS (1+ SIZE))))

(DEFUN PROCESS-QUEUE-LOCKER (QUEUE)
  "The process (or other object) which now /"possesses/" QUEUE, a PROCESS-QUEUE."
  (AREF QUEUE 0))

(DEFUN RESET-PROCESS-QUEUE (QUEUE)
  "Removes all processes enqueued on QUEUE, so that it is empty."
  (WITHOUT-INTERRUPTS
    (FILL QUEUE NIL)))

(DEFUN PROCESS-ENQUEUE (QUEUE &OPTIONAL (LOCK-VALUE CURRENT-PROCESS) (WHOSTATE "Lock"))
  "Waits to possess QUEUE in the name of LOCK-VALUE (default is the current process).
Puts LOCK-VALUE at the end of the queue, then waits for it to
reach the front of the queue (to /"possess/" the queue).
Then returns with LOCK-VALUE still in possession of the queue.
WHOSTATE appears in the who line if it is necessary to wait."
  (UNLESS (%STORE-CONDITIONAL (LOCF (AREF QUEUE 0)) NIL (OR LOCK-VALUE CURRENT-PROCESS))
    (WITHOUT-INTERRUPTS
      ;; If the queue is full, wait for there to be room.
      (WHEN (AREF QUEUE (- (LENGTH QUEUE) 2))
	(PROCESS-WAIT WHOSTATE #'(LAMBDA (LOC) (NULL (CONTENTS LOC)))
		      (LOCF (AREF QUEUE (1- (LENGTH QUEUE))))))
      ;; There is room, so put us in the queue.
      (DOTIMES (I (1- (LENGTH QUEUE)))
	(LET ((TEM (LOCF (AREF QUEUE I))))
	  (COND ((%STORE-CONDITIONAL TEM NIL (OR LOCK-VALUE CURRENT-PROCESS))
		 ;; Now wait until we reach the front before returning.
		 (UNLESS (ZEROP I)
		   (PROCESS-WAIT WHOSTATE #'(LAMBDA (SLOT VALUE)
					      (EQ (CONTENTS SLOT) VALUE))
				 (LOCF (AREF QUEUE 0))
				 (OR LOCK-VALUE CURRENT-PROCESS)))
		 (RETURN))
		((EQ (CONTENTS TEM) (OR LOCK-VALUE CURRENT-PROCESS))
		 (FERROR NIL "~S is already enqueued on ~S."
			 (OR LOCK-VALUE CURRENT-PROCESS) QUEUE))))))))

(DEFUN PROCESS-DEQUEUE (QUEUE &OPTIONAL (LOCK-VALUE CURRENT-PROCESS) (ERROR-P T))
  "Assuming that LOCK-VALUE possesses QUEUE, releases possession.
The next thing on the queue will come to the front, or the queue may become empty.
An error occurs if ERROR-P is non-NIL and LOCK-VALUE is not currently
the object at the front of the queue.
LOCK-VALUE defaults to the current process."
  (IF (EQ (OR LOCK-VALUE CURRENT-PROCESS) (PROCESS-QUEUE-LOCKER QUEUE))
      (%BLT-TYPED (ALOC QUEUE 1) (ALOC QUEUE 0)
		  (1- (LENGTH QUEUE)) 1)
    (IF ERROR-P
	(FERROR NIL "~S is not currently locked by ~S."
		QUEUE (OR LOCK-VALUE CURRENT-PROCESS)))))

))

; From file CHOICE.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; CHOICE  "


(DEFPROP :PAST-DATE
	 (TIME:PRINT-UNIVERSAL-TIME READ-PAST-DATE NIL
	  NIL NIL "Click left to input a new date from the keyboard.") 
	 CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN READ-PAST-DATE (STREAM)
  (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME (READLINE-TRIM STREAM) 0 NIL NIL)))
    VAL))

))

; From file BASWIN.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASWIN  "


(DEFFLAVOR MARGIN-SPACE-MIXIN ((SPACE (LIST 0 0 0 0))) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES SPACE)
  (:INITTABLE-INSTANCE-VARIABLES SPACE))

(DEFMETHOD (MARGIN-SPACE-MIXIN :BEFORE :INIT) (IGNORE)
  (COND ((NULL SPACE)
	 (SETQ SPACE '(0 0 0 0)))
	((EQ SPACE T)
	 (SETQ SPACE '(1 1 1 1)))
	((FIXP SPACE)
	 (SETQ SPACE (MAKE-LIST 4 ':INITIAL-ELEMENT SPACE)))
	((ATOM SPACE)
	 (SETQ SPACE '(0 0 0 0)))))

(DEFMETHOD (MARGIN-SPACE-MIXIN :SET-SPACE) (NEW-SPACE)
  (COND ((NULL NEW-SPACE)
	 (SETQ SPACE '(0 0 0 0)))
	((EQ NEW-SPACE T)
	 (SETQ SPACE '(1 1 1 1)))
	((FIXP NEW-SPACE)
	 (SETQ SPACE (MAKE-LIST 4 ':INITIAL-ELEMENT NEW-SPACE)))
	((ATOM NEW-SPACE)
	 (SETQ SPACE '(0 0 0 0)))
	(T (SETQ SPACE NEW-SPACE)))
  (FUNCALL-SELF ':REDEFINE-MARGINS))

(DEFMETHOD (MARGIN-SPACE-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (VALUES (+ LM (FIRST SPACE)) (+ TM (SECOND SPACE))
	  (+ RM (THIRD SPACE)) (+ BM (FOURTH SPACE))))

))

; From file QIO.LISP SRC:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READ-DELIMITED-STRING (&OPTIONAL (DELIMITER #/END) (STREAM STANDARD-INPUT)
			      EOF RUBOUT-HANDLER-OPTIONS (BUFFER-SIZE 100.))
  "Reads input from STREAM until DELIMITER is found; returns a string.
Uses the rubout handler if STREAM supports that.
DELIMITER is either a character or a list of characters.
 (Characters may be fixnums or character objects).
Values are:
 The string of characters read, not including the delimiter
 T if input ended due to end of file
 The delimiter character read (as a fixnum), or NIL if ended at EOF.
EOF if non-NIL means get error on end of file before any input is got.
RUBOUT-HANDLER-OPTIONS are passed to the :RUBOUT-HANDLER operation.
BUFFER-SIZE is the size to make the buffer string, initially."
  (DECLARE (VALUES STRING EOF-FLAG DELIMITER))
  (IF (AND (NOT RUBOUT-HANDLER)
	   (SEND STREAM ':OPERATION-HANDLED-P ':RUBOUT-HANDLER))
      (SEND STREAM ':RUBOUT-HANDLER RUBOUT-HANDLER-OPTIONS
	    'READ-DELIMITED-STRING DELIMITER STREAM EOF RUBOUT-HANDLER-OPTIONS BUFFER-SIZE)
    (DO ((BUFFER (MAKE-ARRAY BUFFER-SIZE ':TYPE ART-STRING ':FILL-POINTER 0)))
	(())
      (LET ((CH (SEND STREAM ':TYI (AND EOF (ZEROP (LENGTH BUFFER))))))
	(COND ((NULL CH)
	       (RETURN BUFFER T))
	      ((COND ((LISTP DELIMITER) (MEM '= CH DELIMITER))
		     (T (= CH DELIMITER)))
	       (RETURN BUFFER NIL CH)))
	(ARRAY-PUSH-EXTEND BUFFER CH)))))

))

; From file PATCH.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(ADD-INITIALIZATION 'RECORD-SYSTEM-VERSION
		    '(SETF (SYSTEM-COMMUNICATION-AREA %SYS-COM-MAJOR-VERSION)
			   (GET-SYSTEM-VERSION))
		    ':BEFORE-COLD)

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun multiple-value (&quote var-list exp)
  "Evaluate EXP, collecting multiple values, and set the variables in VAR-LIST to them.
Returns the first value of EXP."
  (let ((val-list (multiple-value-list (eval1 exp))))
    (do ((vars var-list (cdr vars))
	 (vals val-list (cdr vals)))
	((null vars))
      (when (car vars)
	(if (eq interpreter-function-environment t)
	    (set (car vars) (car vals))
	  (interpreter-set (car vars) (car vals)))))
    (car val-list)))

))

; From file QCP2.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN (NTH-VALUE P2) (TAIL DEST)
  (IF (AND (QUOTEP (CAR TAIL))
	   (TYPEP (CADR (CAR TAIL)) '(INTEGER 0)))
      (IF (ZEROP (CADR (CAR TAIL)))
	  (P2 `(VALUES ,(CADR TAIL)) DEST)
	(P2MV (CADR TAIL) 'D-PDL (1+ (CADR (CAR TAIL))))
	(POPPDL 1 (CADR (CAR TAIL)))
	(MOVE-RESULT-FROM-PDL DEST))
    (P2 `(ELT (MULTIPLE-VALUE-LIST ,(CADR TAIL)) ,(CAR TAIL)) DEST)))

))

; From file QCP1.LISP SRC:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFPROP NTH-VALUE P1EVARGS P1)

))

; From file EVAL.LISP SRC:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun nth-value (value-number &quote exp)
  "Returns the VALUE-NUMBER'th value of EXP.
Compiles fast when VALUE-NUMBER is a constant."
  (nth value-number (multiple-value-list (eval1 exp))))

))

; From file QFILE.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN OPEN-CHAOS (HOST PATHNAME &REST OPTIONS &KEY (DIRECTION ':INPUT) (CHARACTERS T)
		   (ERROR T) (ACCESS-ERROR (NOT ERROR))
		   (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
		   (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
					;; :UNSPECIFIC here is to prevent lossage
					;; writing ITS files with no version numbers.
					'(:NEWEST :UNSPECIFIC))
				  ':NEW-VERSION ':ERROR)
			      IF-EXISTS-P)
		   (IF-DOES-NOT-EXIST
		     (COND ((MEMQ DIRECTION '(:PROBE :PROBE-LINK :PROBE-DIRECTORY))
			    NIL)
			   ((AND (EQ DIRECTION ':OUTPUT)
				 (NOT (MEMQ IF-EXISTS '(:OVERWRITE :TRUNCATE :APPEND))))
			    ':CREATE)
			   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			   ;; for compatibility with the past.
			   ;; A Common-Lisp program would use :PROBE
			   ;; and get NIL as the default for this.
			   (T ':ERROR)))
		   TEMPORARY DELETED RAW SUPER-IMAGE (BYTE-SIZE ':DEFAULT)
		   PRESERVE-DATES INHIBIT-LINKS
		   &AUX HOST-UNIT DATA-CONN PKT SUCCESS STRING NOT-ABORTED
		   PHONY-CHARACTERS SIGN-EXTEND-BYTES
		   (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SI:CCASE DIRECTION
    ((:INPUT :OUTPUT :PROBE-DIRECTORY :PROBE-LINK))
    (:IO (FERROR NIL "Bidirectional file streams are not supported."))
    ((NIL :PROBE) (SETQ DIRECTION NIL)))
  (CHECK-TYPE IF-EXISTS (MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
				:OVERWRITE :APPEND :TRUNCATE :SUPERSEDE NIL))
  (CHECK-TYPE IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
  (WHEN ELEMENT-TYPE-P
    (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
	  (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
  (FILE-OPERATION-RETRY
    (CONDITION-CASE-IF ACCESS-ERROR (ERROR-OBJECT)
        (PROGN
	  (IF (MEMQ DIRECTION '(NIL :PROBE-DIRECTORY :PROBE-LINK))
	      ;;PROBE mode implies no need for data connection
	      (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT))
	    (MULTIPLE-VALUE (DATA-CONN HOST-UNIT)
	      (FUNCALL HOST ':GET-DATA-CONNECTION DIRECTION))))
      (REMOTE-NETWORK-ERROR ERROR-OBJECT)
      (:NO-ERROR
       (UNWIND-PROTECT
	 (PROGN
	   (MULTIPLE-VALUE (PKT SUCCESS STRING)
	     (IF (TYPEP SELF '(OR LMFILE-PARSING-MIXIN LM-PARSING-MIXIN))
		 (FUNCALL HOST-UNIT ':COMMAND NIL
			  (SELECTQ DIRECTION
			    (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			    (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			  NIL "OPEN-FOR-LISPM " #\RETURN
			  (FILE-PRINT-PATHNAME SELF) #\RETURN
			  (LET ((BASE 10.) (*NOPOINT T) (PACKAGE SI:PKG-USER-PACKAGE)
				(READTABLE SI:INITIAL-READTABLE))
			    (AND IF-EXISTS-P (NULL IF-EXISTS)
				 (SETQ OPTIONS (LIST* ':IF-EXISTS ':ERROR OPTIONS)))
			    (AND (NULL IF-DOES-NOT-EXIST)
				 (SETQ OPTIONS (LIST* ':IF-DOES-NOT-EXIST ':ERROR OPTIONS)))
			    (PRIN1-TO-STRING OPTIONS)))
	       (FUNCALL HOST-UNIT ':COMMAND NIL
			(SELECTQ DIRECTION
			  (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			  (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			NIL
			"OPEN " (SELECTQ DIRECTION
				  ((NIL) "PROBE")
				  (:PROBE-DIRECTORY "PROBE-DIRECTORY")
				  (:PROBE-LINK "PROBE INHIBIT-LINKS")
				  (:INPUT "READ")
				  (:OUTPUT "WRITE"))
			" " (SELECTQ CHARACTERS
			      ((NIL) "BINARY")
			      (:DEFAULT "DEFAULT")
			      (T "CHARACTER"))
			(IF (AND (EQ DIRECTION ':OUTPUT)
				 (NEQ IF-EXISTS
				      (IF (MEMQ (PATHNAME-VERSION PATHNAME)
						'(:NEWEST :UNSPECIFIC))
					  ':NEW-VERSION ':SUPERSEDE)))
			    (STRING-APPEND " IF-EXISTS "
					   (IF (EQ IF-EXISTS NIL)
					       ':ERROR
					     IF-EXISTS))
			  "")
			(IF (OR IF-EXISTS-P
				(NEQ IF-DOES-NOT-EXIST
				     (SELECTQ DIRECTION
				       ((:INPUT NIL :PROBE-DIRECTORY :PROBE-LINK) ':ERROR)
				       (:OUTPUT ':CREATE))))
			    (STRING-APPEND " IF-DOES-NOT-EXIST "
					   (IF (EQ IF-DOES-NOT-EXIST NIL)
					       ':ERROR
					     IF-DOES-NOT-EXIST))
			  "")
			(IF INHIBIT-LINKS " INHIBIT-LINKS" "")
			(FORMAT NIL "~:[ BYTE-SIZE ~D~;~*~]~:[~; TEMPORARY~]~:[~; DELETED~]~
				~:[~; RAW~]~:[~; SUPER~]~:[~; PRESERVE-DATES~]~%~A~%"
				(EQ BYTE-SIZE ':DEFAULT) BYTE-SIZE
				TEMPORARY DELETED RAW SUPER-IMAGE PRESERVE-DATES
				(FILE-PRINT-PATHNAME SELF)))))
	   (COND ((NOT SUCCESS)
		  (SETQ NOT-ABORTED T)
		  (SETQ STRING (STRING-APPEND STRING))
		  (AND PKT (CHAOS:RETURN-PKT PKT))
		  (OR (NULL DATA-CONN)
		      (SETF (DATA-STREAM DATA-CONN DIRECTION) NIL))
		  (CONDITION-CASE-IF (NOT IF-DOES-NOT-EXIST)
				     ()
		      (CONDITION-CASE-IF (NOT IF-EXISTS)
					 ()
			  (FILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR) ':OPEN)
			(FILE-ALREADY-EXISTS NIL))
		    (FILE-NOT-FOUND NIL)))
		 (T
		  (LET ((PROPERTIES (READ-FILE-PROPERTY-LIST-STRING STRING "OPEN" PATHNAME)))
		    (CHAOS:RETURN-PKT PKT)
		    (AND (EQ CHARACTERS ':DEFAULT)
			 (SETQ CHARACTERS (GET (LOCF PROPERTIES) ':CHARACTERS)))
		    (UNLESS (OR (EQ BYTE-SIZE ':DEFAULT)
				(GET (LOCF PROPERTIES) ':BYTE-SIZE))
		      (SETF (GET (LOCF PROPERTIES) ':BYTE-SIZE) BYTE-SIZE))
		    (PROG1
		      (MAKE-INSTANCE (SELECTQ DIRECTION
				       (:INPUT
					(IF CHARACTERS
					    'FILE-INPUT-CHARACTER-STREAM
					  (COND (SIGN-EXTEND-BYTES
						 'FILE-INPUT-SIGNED-BINARY-STREAM)
						(PHONY-CHARACTERS
						 'FILE-INPUT-PHONY-CHARACTER-STREAM)
						(T
						 'FILE-INPUT-BINARY-STREAM))))
				       (:OUTPUT
					(IF CHARACTERS
					   'FILE-OUTPUT-CHARACTER-STREAM
					  (IF PHONY-CHARACTERS
					      'FILE-OUTPUT-PHONY-CHARACTER-STREAM
					    'FILE-OUTPUT-BINARY-STREAM)))
				       (T 'FILE-PROBE-STREAM))
				     ':HOST-UNIT HOST-UNIT
				     ':DATA-CONNECTION DATA-CONN
				     ':PROPERTY-LIST PROPERTIES
				     ':PATHNAME PATHNAME)
		      (SETQ NOT-ABORTED T))))))
	 (UNLESS (OR NOT-ABORTED
		     (NULL DATA-CONN)
		     (NULL (SEND HOST-UNIT ':CONTROL-CONNECTION)))
	   ;; Here if aborted out of it and server may have file open.
	   (CONDITION-CASE ()
	       (PROGN
		(AND (EQ DIRECTION ':OUTPUT)
		     (FUNCALL HOST-UNIT ':COMMAND NIL
			      (DATA-OUTPUT-HANDLE DATA-CONN) NIL "DELETE"))
		(MULTIPLE-VALUE-BIND (NIL CLOSE-SUCCESS)
		    (FUNCALL HOST-UNIT ':COMMAND
			     NIL
			     (SELECTQ DIRECTION
			       (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			       (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			     NIL "CLOSE")
		  (WHEN CLOSE-SUCCESS
		    (SELECTQ DIRECTION
		      (:INPUT (READ-UNTIL-SYNCHRONOUS-MARK (DATA-CONNECTION DATA-CONN)))
		      (:OUTPUT (CHAOS:SEND-PKT (DATA-CONNECTION DATA-CONN)
					       (CHAOS:GET-PKT) %FILE-SYNCHRONOUS-MARK-OPCODE)))))
		(FUNCALL HOST-UNIT ':FREE-DATA-CONNECTION DATA-CONN DIRECTION))
	     (SYS:HOST-STOPPED-RESPONDING NIL))))))))

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
      t)))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN CREATE-LINK (LINK LINK-TO &KEY (ERROR T))
  "Create a link, which is specified as a pathname or string, to the file LINK-TO."
  (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
			      (LINK FILE-ERROR)
    (LET ((PATHNAME (MERGE-PATHNAME-DEFAULTS LINK)))
      (SEND PATHNAME ':CREATE-LINK (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
				     (MERGE-PATHNAME-DEFAULTS LINK-TO PATHNAME))
	    ':ERROR ERROR))))

))

; From file PATED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN ADD-PATCH-INTERVAL (BP1 BP2 IN-ORDER-P DEFUN-NAME BUFFER &AUX NEW-PATCH-BUFFER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (VALIDATE-PATCH-BUFFER)
  (SETQ NEW-PATCH-BUFFER-P (NULL *PATCH-BUFFER*))
  (AND NEW-PATCH-BUFFER-P (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME)))
  (FORMAT QUERY-IO
	  "~&Adding ~A to patch file ~A~:[~;~%(New patch file.)~]"
	  DEFUN-NAME (PATCH-VERSION-DESCRIPTION) NEW-PATCH-BUFFER-P)
  (LET ((BP (INTERVAL-LAST-BP *PATCH-BUFFER*)))
    ;; Put into the patch buffer, making sure the right package and base will be used.
    (MULTIPLE-VALUE-BIND (VARS VALS)
	(SEND BUFFER ':ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	     (INSERT BP (FORMAT NIL "~%; From file ~A~%#~DR ~A#:
/(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE /"~:*~A/")))
/  (COMPILER#:PATCH-SOURCE-FILE ~S

"
				(BUFFER-NAME BUFFER) IBASE PACKAGE
				(WHEN (BUFFER-GENERIC-PATHNAME BUFFER)
				  (SEND (BUFFER-GENERIC-PATHNAME BUFFER)
					':STRING-FOR-PRINTING))))))
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
	  ((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL T))
	   ;; No region, try to get containing defun.
	   (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	   (CHECK-INTERVAL-SECTIONS BP1 BP2 T)
	   (SETQ DEFUN-NAME (SECTION-NODE-NAME (LINE-NODE (LINE-NEXT (BP-LINE BP1))))))
	  (T
	   (BARF "Unbalanced parentheses or no defuns.")))
    (ADD-PATCH-INTERVAL BP1 BP2 T DEFUN-NAME *INTERVAL*))
  DIS-MARK-GOES)

))

; From file SECTIO.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFUN DEFINITION-NAME-AS-STRING (TYPE SPEC)
  "Like printing SPEC into a string, but faster, with a few quirks.
The quirk is that if TYPE is non-NIL the printing is done
in the package which the car of SPEC belongs to, if that is possible."
  (LET-IF (AND TYPE (TYPEP (CAR-SAFE SPEC) '(AND SYMBOL (NOT NULL)))
	       (LET ((TEM (SYMBOL-PACKAGE (CAR SPEC))))
		 (AND TEM (NEQ TEM SI:PKG-KEYWORD-PACKAGE)
		      (NEQ TEM SI:PKG-GLOBAL-PACKAGE)
		      (NEQ TEM SI:PKG-SYSTEM-PACKAGE))))
	  ((PACKAGE (SYMBOL-PACKAGE (CAR SPEC))))
    (COND ((AND (SYMBOLP SPEC)
		(LOOP WITH PNAME = (SYMBOL-NAME SPEC)
		      FOR I FROM 0 BELOW (LENGTH PNAME)
		      AS CH = (AREF PNAME I)
		      ALWAYS (OR ( #/A CH #/Z) (EQ CH #/-))))
	   (SYMBOL-NAME SPEC))
	  ((AND (CONSP SPEC)
		(LOOP FOR ELT IN SPEC
		      ALWAYS
		      (AND (SYMBOLP ELT)
			   (LET ((PNAME (SYMBOL-NAME ELT)))
			     (LOOP FOR I FROM 0 BELOW (LENGTH PNAME)
				   AS CH = (AREF PNAME I)
				   ALWAYS (OR ( #/A CH #/Z) (EQ CH #/-)))))))
	   (LET ((STRING (MAKE-ARRAY 40. ':TYPE ART-STRING ':FILL-POINTER 0)))
	     (ARRAY-PUSH STRING #/()
	     (LOOP FOR X IN SPEC
		   FOR POS FROM 0 BY 1
		   AS P = (SYMBOL-PACKAGE X)
		   AS PNAME = (GET-PNAME X)
		   DO (OR (ZEROP POS) (STRING-NCONC STRING #\SP))
		   (IF (AND (NEQ P PACKAGE)
			    (NOT (MEMQ P (PACKAGE-USE-LIST PACKAGE))))
		       (STRING-NCONC STRING
				     (IF (EQ P SI:PKG-KEYWORD-PACKAGE)
					 ""
				       (SI:PKG-SHORTEST-NAME P SI:PKG-GLOBAL-PACKAGE))
				     #/: PNAME)
		     (STRING-NCONC STRING PNAME)))
	     (STRING-NCONC STRING #/))
	     STRING))
	  (T
	   ;; Not all symbols, stay on the safe side
	   (FORMAT:OUTPUT NIL
	     (PRIN1 SPEC))))))

))

; From file EH.LISP SRC:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EH  "

(DEFUN LOAD-ERROR-TABLE ()
  (FORMAT TERMINAL-IO "~&[Loading error table for microcode version ~D]"
	  %MICROCODE-VERSION-NUMBER)
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-DESIRED-MICROCODE-VERSION)
	 %MICROCODE-VERSION-NUMBER)
  (WHEN (EQ SITE-NAME ':MIT)
    ;; Make this word look like a valid 24-bit fixnum with data type
    ;; so that it is possible to select this band from a brand S band.
    (%P-DPB DTP-FIX 3005
	    (LOCF (SYSTEM-COMMUNICATION-AREA %SYS-COM-DESIRED-MICROCODE-VERSION))))
  (SI:WITH-SYS-HOST-ACCESSIBLE
    (LOAD (FUNCALL (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
		   ':NEW-TYPE-AND-VERSION "TBL" %MICROCODE-VERSION-NUMBER)
	  "EH")))

;; Kludge so that the dumped band will look good to a brand S band.
(%P-DPB DTP-FIX 3005
	(LOCF (SYSTEM-COMMUNICATION-AREA %SYS-COM-DESIRED-MICROCODE-VERSION)))

))

; From file PATHNM.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHNM  "


(DEFUN LINEARIZE-PATHNAME-PLISTS ()
  (IF PATHNAME-PLISTS-LINEARIZED-ONCE
      ;; If already been recopied, just reference all of them so they are
      ;; all copied into newspace together.
      (MAPHASH #'(LAMBDA (IGNORE FILE &REST IGNORE)
		   (REFERENCE-ALL (SEND FILE ':PLIST)))
	       FS:*PATHNAME-HASH-TABLE*)
    (SETQ PATHNAME-PLISTS-LINEARIZED-ONCE T)
    (MAPHASH #'(LAMBDA (IGNORE FILE &REST IGNORE)
		 (SEND FILE ':SET-PROPERTY-LIST (COPYTREE (SEND FILE ':PLIST))))
	     FS:*PATHNAME-HASH-TABLE*)))

(DEFUN REFERENCE-ALL (OBJECT)
  (UNLESS (ATOM OBJECT)
    (DO ((TAIL OBJECT (CDR TAIL)))
	((ATOM TAIL))
      (UNLESS (ATOM (CAR TAIL))
	(REFERENCE-ALL (CAR TAIL))))))

))

; From file GC.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN FULL-GC (&OPTIONAL &KEY NO-TEMPORARY-AREAS NO-STATIC-REGIONS NO-RECOPYING DUPLICATE-PNAMES)
  "Do a complete batch-style garbage collection to make minimum size for saved band.
It is best to do this twice in a row to make sure that the used part of memory
is at the bottom of the address space.

If DUPLICATE-PNAMES is T, all equal pnames of symbols are collapsed.
This is only worth doing once for a given system version so it is off
by default.

Unless NO-STATIC-REGIONS is non-NIL, the existing full regions of
WORKING-STORAGE-AREA are marked as static when the GC is done.
Unless NO-TEMPORARY-AREAS is non-NIL, all temporary regions are reset
to achieve minimum band size.
Unless NO-RECOPYING is non-NIL, various structures are recopied after
the GC is done to make sure they are compact (to improve paging efficiency).
GC-IMMEDIATELY uses FULL-GC as a subroutine, supplying T for these three args."
  (PROG ()
	(COND (( (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
		     (GC-GET-COMMITTED-FREE-SPACE T NIL T))
		  (GET-FREE-SPACE-SIZE))
	       (FORMAT QUERY-IO "~&There is probably not enough free space to garbage collect,
unless there is a lot of garbage to be freed.")
	       (OR (Y-OR-N-P "Try garbage collecting anyway? ")
		   (RETURN NIL))))
	;; For extra reduction in size of band, reset all temporary areas.
	;; Do this first, since this may free up other things that they point to.
	(UNLESS NO-TEMPORARY-AREAS
	  (INITIALIZATIONS 'FULL-GC-INITIALIZATION-LIST T)
	  (DO ((AREA FIRST-FULL-GC-AREA (1+ AREA)))
	      ((= AREA SIZE-OF-AREA-ARRAYS))
	    (IF (AREA-TEMPORARY-P AREA)
		(RESET-TEMPORARY-AREA AREA))))
	(WHEN DUPLICATE-PNAMES
	  (COLLAPSE-DUPLICATE-PNAMES))
	(UNLESS NO-STATIC-REGIONS
	  (MAKE-AREA-DYNAMIC WORKING-STORAGE-AREA))
	(WITH-LOCK (GC-FLIP-LOCK)
	  (PROCESS-DISABLE GC-PROCESS)
	  ;; We assume that incremental GC has not been being used,
	  ;; so if oldspace exists, we are already after a (GC-FLIP-NOW).
	  (OR GC-OLDSPACE-EXISTS (GC-FLIP-NOW))
	  ;; Touch all interned symbols and their pnames,
	  ;; to get them in a good order for paging.
	  ;; This is really only necessary if NR-SYM and P-N-STRING are being GC'd.
	  (LET (TEM)
	    (DOLIST (P *ALL-PACKAGES*)
	      (DO-LOCAL-SYMBOLS (SYM P)
		(SETQ TEM (LENGTH (GET-PNAME SYM))))))
	  (GC-RECLAIM-OLDSPACE)
	  (UNLESS NO-RECOPYING
	    (INITIALIZATIONS 'AFTER-FULL-GC-INITIALIZATION-LIST T))
	  (UNLESS NO-STATIC-REGIONS
	    (MAKE-AREA-REGIONS-STATIC WORKING-STORAGE-AREA)))))

))

; From file FLAVOR.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFCONST DEFFLAVOR1-PRESERVED-PROPERTIES
	  '(ADDITIONAL-INSTANCE-VARIABLES
	    ALL-INSTANCE-VARIABLES-SPECIAL
	    COMPILE-FLAVOR-METHODS
	    UNMAPPED-INSTANCE-VARIABLES
	    MAPPED-COMPONENT-FLAVORS
	    INSTANCE-VARIABLE-INITIALIZATIONS
	    ALL-SPECIAL-INSTANCE-VARIABLES
	    ALL-INITTABLE-INSTANCE-VARIABLES
	    REMAINING-DEFAULT-PLIST
	    REMAINING-INIT-KEYWORDS
	    REQUIRED-INIT-KEYWORDS
	    INSTANCE-AREA-FUNCTION))

))

; From file PNMAP.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PNMAP  "

(defun pathname-component-match (pattern sample wild-any wild-one
				 &optional return-specs-flag
				 &aux specs)
  ;; If RETURN-SPECS-FLAG, we return a list of the chars or strings
  ;; that matched the wildcards, in the order they appeared,
  ;; or T if no wildcards but the pattern does match.
  (cond ((eq pattern ':wild)
	 (if return-specs-flag
	     (if (consp sample) sample (list sample))
	   t))
	((symbolp pattern) (eq pattern sample))
	((numberp pattern) (eq pattern sample))
	((listp pattern)
	 (and (listp sample)
	      (= (length pattern) (length sample))
	      (loop for p in pattern for s in sample
		    do
		    (let ((tem
			    (pathname-component-match p s wild-any wild-one
						      return-specs-flag)))
		      (if (null tem) (return nil))
		      (unless (eq tem t)
			(setq specs (append specs tem))))
		    finally (return (or specs t)))))
	((not (stringp sample)) nil)
	(t
	 (do ((p-ptr 0)
	      (p-next)
	      (p-char wild-one)
	      (s-ptr -1)
	      (set (list wild-any wild-one)))
	     (())
	   (setq p-next (string-search-set set pattern p-ptr))
	   (cond ((eq p-char wild-one)
		  (and return-specs-flag ( s-ptr 0)
		       (push (aref sample s-ptr) specs))
		  (setq s-ptr
			(and (string-equal sample pattern (1+ s-ptr) p-ptr
					   (+ 1 s-ptr
					      (- (or p-next (length pattern)) p-ptr))
					   p-next)
			     (1+ s-ptr))))
		 ((null p-next)
		  ;; Stuff at end following a star =>
		  ;;  win if tail of rest of string matches that stuff.
		  (let ((old-s-ptr s-ptr))
		    (setq s-ptr (string-reverse-search pattern sample nil s-ptr p-ptr))
		    (when return-specs-flag
		      (push (substring sample old-s-ptr s-ptr) specs))))
		 (t
		  (let ((old-s-ptr s-ptr))
		    (setq s-ptr (string-search pattern sample s-ptr nil p-ptr p-next))
		    (when return-specs-flag
		      (push (substring sample old-s-ptr s-ptr) specs)))))
	   (unless s-ptr (return nil))
	   (incf s-ptr (- (or p-next (length pattern)) p-ptr))
	   (unless p-next (return (and (= s-ptr (length sample)) (or (nreverse specs) t))))
	   (setq p-char (aref pattern p-next))
	   (setq p-ptr (1+ p-next))))))

))

; From file DIRED.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-COPY "Copy the file on this line" ()
  (WHEN (GET (LOCF (LINE-PLIST (BP-LINE (POINT)))) ':DELETED)
    (BARF))
  (LET ((FILE (DIRED-LINE-PATHNAME-OR-BARF (BP-LINE (POINT)))))
    (LET ((NEWFILE (READ-DEFAULTED-PATHNAME (FORMAT NIL "Pathname to copy ~A to" FILE)
					    FILE))
	  RESULT FILE-PLIST)
      (SETQ RESULT (FS:COPY-FILE FILE NEWFILE ':ERROR NIL))
      (COND ((ERRORP RESULT)
	     (FORMAT QUERY-IO "~&Not copied: ~A" RESULT))
	    ((ERRORP (CAR RESULT))
	     (FORMAT QUERY-IO "~&Not copied: ~A" (CAR RESULT)))
	    (T
	     (FORMAT QUERY-IO "~&File copied to ~A" (CADAR RESULT))
	     ;; Save a copy of this file's directory list entry.
	     (SETQ FILE-PLIST (COPYLIST (LINE-PLIST (BP-LINE (POINT)))))
	     (PUTPROP (LOCF FILE-PLIST)
		      (CADAR RESULT)
		      ':PATHNAME)
	     ;; insert a line for the new file.
	     (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	       (MULTIPLE-VALUE-BIND (BP LEVEL)
		   (DIRED-PATHNAME-INSERTION-BP (CADAR RESULT))
		 (COND (BP
			(WITH-BP (SAVE-BP BP ':NORMAL)
			  (INSERT BP #\RETURN)
			  (SETF (LINE-PLIST (BP-LINE SAVE-BP)) FILE-PLIST)
			  (SETF (DIRED-LINE-LEVEL (BP-LINE SAVE-BP))
				(OR LEVEL 0))
			  (DIRED-REGENERATE-LINE (BP-LINE SAVE-BP))))
		       (T
			(FORMAT QUERY-IO ", in a directory not in this display.")))))))))
  DIS-TEXT)

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN COPY-FILE (PATHNAME-OR-STREAM NEW-NAME
		  &REST OPTIONS
		  &KEY (ERROR T) &ALLOW-OTHER-KEYS)
  "Copy a file, specified as a pathname, string or I//O stream.
CHARACTERS can be T, NIL, meaning the same as in OPEN.
 or it can be :ASK, meaning always ask the user,
 or :MAYBE-ASK meaning ask the user unless the answer is clear,
 or :DEFAULT meaning guess as well as possible but never ask.
Specify BYTE-SIZE to force copying in a certain byte size.
 BYTE-SIZE affects only binary mode copying.
REPORT-STREAM is a stream to output messages to as files are copied.
 If it is NIL, no messages are output.
COPY-CREATION-DATE if NIL means don't copy the file creation date;
 make now be the new file's creation date.
COPY-AUTHOR if NIL means don't copy the author; make you the new file's author.
CREATE-DIRECTORIES says whether to create a directory to be copied into.
 Values are T, NIL and :QUERY (meaning ask the user if the situation comes up)."
  (DECLARE (ARGLIST PATHNAME-OR-STREAM NEW-NAME
		    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
		    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
		    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT)))
  (FORCE-USER-TO-LOGIN)
  (IF (OR (STRINGP PATHNAME-OR-STREAM)
	  (TYPEP PATHNAME-OR-STREAM 'PATHNAME))	;Not a stream
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
				  (PATHNAME-OR-STREAM FILE-ERROR)
        (APPLY (MERGE-PATHNAME-DEFAULTS PATHNAME-OR-STREAM)
	       ':WILDCARD-MAP #'PRIMITIVE-COPY-FILE
	       ':MAYBE (AND (NOT ERROR) '(:NOERROR))
	       NEW-NAME OPTIONS))
    (LIST (APPLY 'PRIMITIVE-COPY-FILE
		 (FILE-PROPERTIES (SEND PATHNAME-OR-STREAM ':TRUENAME))
		 NEW-NAME OPTIONS))))

))

; From file OPEN.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN PRIMITIVE-COPY-FILE (INPUT-PLIST-OR-PATHNAME OUTPUT-SPEC
			    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
			    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
			    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT)
			    &AUX INTYPE INPUT-PLIST INPUT-PATHNAME)
  (IF (NLISTP INPUT-PLIST-OR-PATHNAME)
      (SETQ INPUT-PATHNAME INPUT-PLIST-OR-PATHNAME
	    INPUT-PLIST NIL)
      (SETQ INPUT-PATHNAME (CAR INPUT-PLIST-OR-PATHNAME)
	    INPUT-PLIST INPUT-PLIST-OR-PATHNAME))
  ;; Decide whether to copy as binary file.
  ;; Either do as told, guess from file byte size or type, or ask the user.
  (LET ((CHARACTERS?
	  (SELECTQ CHARACTERS
	    ((T NIL) CHARACTERS)
	    (:ASK (FQUERY NIL "~&Is ~A a text file? " INPUT-PATHNAME))
	    (OTHERWISE
	     ;; At this point we really need to refer to the file's property list,
	     ;; so get it if we were not given it as the first arg.
	     (IF (NULL INPUT-PLIST)
		 (SETQ INPUT-PLIST (FILE-PROPERTIES INPUT-PATHNAME)
		       INPUT-PATHNAME (CAR INPUT-PLIST)))
	     (LET ((BYTE-SIZE (GET INPUT-PLIST ':BYTE-SIZE)))
	       (COND ((MEMQ BYTE-SIZE '(7 8)) T)
		     ((EQ BYTE-SIZE 16.) NIL)
		     ((MEMBER (SETQ INTYPE (SEND INPUT-PATHNAME ':CANONICAL-TYPE))
			      *COPY-FILE-KNOWN-TEXT-TYPES*)
		      T)
		     ((MEMBER INTYPE *COPY-FILE-KNOWN-BINARY-TYPES*) NIL)
		     ((EQ CHARACTERS ':DEFAULT) ':DEFAULT)
		     (T (FQUERY '(:BEEP T) "~&Is ~A a text file? " INPUT-PATHNAME))))))))
    (IF (EQ BYTE-SIZE ':DEFAULT)
	(SETQ BYTE-SIZE (GET INPUT-PLIST ':BYTE-SIZE)))
    (CONDITION-CASE-IF (NOT ERROR) (ERROR)
	(WITH-OPEN-FILE (INSTREAM INPUT-PATHNAME
				  ':DIRECTION ':INPUT
				  ':CHARACTERS CHARACTERS?
				  ':BYTE-SIZE BYTE-SIZE)
	  (LET ((INPUT-TRUENAME (SEND INSTREAM ':TRUENAME)))
	    (CONDITION-BIND ((DIRECTORY-NOT-FOUND
			       #'(LAMBDA (ERROR)
				   (WHEN (IF (EQ CREATE-DIRECTORIES ':QUERY)
					     (PROGN
					       (SEND QUERY-IO ':FRESH-LINE)
					       (SEND ERROR ':REPORT QUERY-IO)
					       (Y-OR-N-P "Create the directory? "))
					   CREATE-DIRECTORIES)
				     (CREATE-DIRECTORY (SEND ERROR ':PATHNAME) ':RECURSIVE T)
				     ':RETRY-FILE-OPERATION))))
	      (WITH-OPEN-FILE (OUTSTREAM (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
					   (MERGE-PATHNAME-DEFAULTS OUTPUT-SPEC INPUT-TRUENAME))
					 ':DIRECTION ':OUTPUT
					 ':CHARACTERS CHARACTERS?
					 ':BYTE-SIZE BYTE-SIZE)
		(IF COPY-AUTHOR
		    (IF COPY-CREATION-DATE
			(SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
			      ':CREATION-DATE (SEND INSTREAM ':CREATION-DATE)
			      ':AUTHOR (OR (SEND INSTREAM ':GET ':AUTHOR)
					   (GET INPUT-PLIST ':AUTHOR)))
		      (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
			    ':AUTHOR (OR (SEND INSTREAM ':GET ':AUTHOR)
					 (GET INPUT-PLIST ':AUTHOR))))
		  (IF COPY-CREATION-DATE
		      (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
			    ':CREATION-DATE (SEND INSTREAM ':CREATION-DATE))))
		(STREAM-COPY-UNTIL-EOF INSTREAM OUTSTREAM)
		(CLOSE OUTSTREAM)
		(WHEN REPORT-STREAM
		  (FORMAT REPORT-STREAM "~&Copied ~A to ~A "
			  INPUT-TRUENAME (SEND OUTSTREAM ':TRUENAME))
		  (IF CHARACTERS?
		      (FORMAT REPORT-STREAM "in character mode.~%")
		    (FORMAT REPORT-STREAM "in byte size ~D.~%"
			    BYTE-SIZE)))
		(LIST INPUT-TRUENAME
		      (SEND OUTSTREAM ':TRUENAME)
		      CHARACTERS?)))))
      ((FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
       ERROR))))

))

; From file FILES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "

(DEFUN COPY-FILE-1 (COPY-MODE COPY-CREATION-DATE?)
  (LET* ((FILE-TYPE-STRING
	   (SELECTQ COPY-MODE
	     (:CHARACTERS "Copy text file")
	     (:BINARY "Copy binary file")
	     (OTHERWISE "Copy file")))
	 (PATHNAME
	   (READ-DEFAULTED-PATHNAME (FORMAT NIL "~A:" FILE-TYPE-STRING)
				    (PATHNAME-DEFAULTS)))
	 (TO-SPEC (READ-UNDEFAULTED-PATHNAME-STRING
		    (FORMAT NIL "Copy file ~A to:" PATHNAME)
		    PATHNAME)))
    (IF (SEND PATHNAME ':WILD-P)
	(LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME)))
	      (TO-PATHNAME (FS:MERGE-PATHNAMES TO-SPEC PATHNAME)))
	  (FORMAT T "~&Files to be copied:~%")
	  (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
	  (WHEN (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Copy them all? "))
	    (DOLIST (ELT DIR)
	      (CONDITION-CASE (ERROR)
		  (COPY-FILE (CAR ELT)
			     (SEND PATHNAME ':TRANSLATE-WILD-PATHNAME TO-PATHNAME (CAR ELT))
			     ':COPY-CREATION-DATE COPY-CREATION-DATE?
			     ':COPY-AUTHOR COPY-CREATION-DATE?
			     ':CHARACTERS COPY-MODE
			     ':REPORT-STREAM STANDARD-OUTPUT)
		((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
		 (FORMAT T "~&Copy failure: ~A" ERROR))))
	    (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
	  (COPY-FILE PATHNAME TO-SPEC
		     ':COPY-CREATION-DATE COPY-CREATION-DATE?
		     ':COPY-AUTHOR COPY-CREATION-DATE?
		     ':CHARACTERS COPY-MODE
		     ':REPORT-STREAM QUERY-IO)
	((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
	 (BARF VALUE))))))

))

; From file FILES.LISP SRC:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "


(DEFCOM COM-DELETE-FILE "Delete a file.
If wildcards are used, many files can be deleted." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Delete file:" (PATHNAME-DEFAULTS))))
    (IF (SEND PATHNAME ':WILD-P)
	(LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME))))
	  (FORMAT T "~&Files to be deleted:~%")
	  (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
	  (WHEN (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Delete them all? "))
	    (DOLIST (ELT DIR)
	      (CONDITION-CASE (ERROR)
		  (SEND (CAR ELT) ':DELETE)
		((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
		 (FORMAT T "~&Deletion failure: ~A" ERROR))))
	    (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
	  (DELETE-FILE PATHNAME)
	((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
	 (BARF VALUE))
	(:NO-ERROR
	 (FORMAT QUERY-IO "~&~A deleted." (CAAR VALUE))))))
  DIS-NONE)

(DEFCOM COM-UNDELETE-FILE "Undelete a file.
If wildcards are used, many files can be undeleted." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Undelete file:" (PATHNAME-DEFAULTS))))
    (IF (SEND PATHNAME ':WILD-P)
	(LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME))))
	  (FORMAT T "~&Files to be undeleted:~%")
	  (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
	  (WHEN (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Undelete them all? "))
	    (DOLIST (ELT DIR)
	      (CONDITION-CASE (ERROR)
		  (SEND (CAR ELT) ':UNDELETE)
		((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
		 (FORMAT T "~&Undeletion failure: ~A" ERROR))))
	    (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
	  (UNDELETE-FILE PATHNAME)
	((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
	 (BARF VALUE))
	(:NO-ERROR
	 (FORMAT QUERY-IO "~&~A undeleted." (CAAR VALUE))))))
  DIS-NONE)

(DEFCOM COM-RENAME-FILE "Rename a file.
If wildcards are used, many files can be renamed." ()
  (LET* ((PATHNAME (READ-DEFAULTED-PATHNAME "Rename file:" (PATHNAME-DEFAULTS)))
	 (TO-SPEC (READ-UNDEFAULTED-PATHNAME-STRING
		    (FORMAT NIL "Rename file ~A to:" PATHNAME)
		    PATHNAME))
	 BUFFERS-CONSIDERED)
    (DECLARE (SPECIAL BUFFERS-CONSIDERED))
    (IF (SEND PATHNAME ':WILD-P)
	(LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME)))
	      (TO-PATHNAME (FS:MERGE-PATHNAMES TO-SPEC PATHNAME)))
	  (FORMAT T "~&Files to be renamed:~%")
	  (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
	  (WHEN (LET ((QUERY-IO STANDARD-OUTPUT))
		  (Y-OR-N-P "Rename them all? "))
	    (DOLIST (ELT DIR)
	      (CONDITION-CASE (ERROR)
		  (SEND (CAR ELT) ':RENAME
			(SEND PATHNAME ':TRANSLATE-WILD-PATHNAME TO-PATHNAME (CAR ELT)))
		((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
		 (FORMAT T "~&Rename failure: ~A" ERROR))
		(:NO-ERROR
		 (RENAME-FILE-1 PATHNAME
				(SEND PATHNAME ':TRANSLATE-WILD-PATHNAME
				      TO-PATHNAME (CAR ELT))))))
	    (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
	  (RENAME-FILE PATHNAME TO-SPEC)
	((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
	 (BARF VALUE))
	(:NO-ERROR
	 (FORMAT QUERY-IO "~&~A renamed~% to ~A." (CAAR VALUE) (CADAR VALUE))
	 (RENAME-FILE-1 (CAAR VALUE) (CADAR VALUE))))))
  DIS-NONE)

(DEFUN RENAME-FILE-1 (INPUT-PATHNAME OUTPUT-PATHNAME)
  (DECLARE (SPECIAL BUFFERS-CONSIDERED))
  ;; Offer to rename a buffer visiting this file, no specific version.
  ;; In order to avoid asking the same question for each file version renamed,
  ;; we record buffers that have been asked about and don't ask a second time.
  (LET ((BUF (FIND-FILE-BUFFER (SEND INPUT-PATHNAME ':NEW-VERSION ':NEWEST))))
    (WHEN (AND BUF (NOT (MEMQ BUF BUFFERS-CONSIDERED)))
      (PUSH BUF BUFFERS-CONSIDERED)
      (IF (FQUERY NIL "~&Rename buffer ~A as well? " BUF)
	  (SET-BUFFER-PATHNAME (SEND OUTPUT-PATHNAME ':NEW-VERSION ':NEWEST) BUF))))
  ;; Offer to rename a buffer visiting this version number specifically.
  (LET ((BUF (FIND-FILE-BUFFER INPUT-PATHNAME)))
    (WHEN (AND BUF (NOT (MEMQ BUF BUFFERS-CONSIDERED)))
      (PUSH BUF BUFFERS-CONSIDERED)
      (WHEN (FQUERY NIL "~&Rename buffer ~A as well? " BUF)
	(SET-BUFFER-PATHNAME OUTPUT-PATHNAME BUF)
	(WHEN (CONSP (BUFFER-FILE-ID BUF))
	  (SETF (BUFFER-FILE-ID BUF)
		(CONS OUTPUT-PATHNAME (CDR (BUFFER-FILE-ID BUF)))))))))

))

zwei:(let ((window (funcall (window-editor-closure (tv:find-window-of-flavor 'zmacs-frame))
			    'eval '*window*)))
       (zwei:push-on-history (send window ':interval)
			     (send window ':buffer-history)))

