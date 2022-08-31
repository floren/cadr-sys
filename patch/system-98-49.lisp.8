;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.49
;;; Reason: Define CLI:CATCH and CLI:THROW right for now.
;;; TRACE-PRINT bug.  ENCAPSULATE inherited SPECIAL decl bug.
;;; STEP bug.
;;; Control characters in ZWEI:COM-STRING-SEARCH
;;; Query Replace, etc., win with region now.
;;; Screen editor Attributes command bug, changing borders.
;;; Window error handler redisplay for Set Arg command.
;;; Source Compare no longer asks "Text or Forms".
;;; TYPE-PRETTY-NAME avoids temp area lossage.
;;; Written 4/21/84 21:35:42 by RMS,
;;; while running on Lisp Machine Seven from band 7
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.46, CADR 3.6, Inconsistent (unreleased patches loaded) ZMail 53.16, MIT-Specific 22.0, microcode 309.



; From file QTRACE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; QTRACE  "

(DEFUN TRACE-PRINT (DEPTH DIRECTION FUNCTION PRINT-ARGS-FLAG EXTRAS-1 EXTRAS-2)
 (DECLARE (SPECIAL ARGLIST VALUES))
 (TERPRI *TRACE-OUTPUT*)
 (DO N (* 2 TRACE-LEVEL) (1- N) (NOT (> N 2))
     (TYO #/SP *TRACE-OUTPUT*))
 (FORMAT *TRACE-OUTPUT* "(~D ~A " DEPTH DIRECTION)
 (PRIN1 FUNCTION *TRACE-OUTPUT*)
 (LET ((STUFF (IF (EQ DIRECTION 'ENTER) ARGLIST VALUES)))
   (COND ((AND STUFF PRINT-ARGS-FLAG)
	  (PRINC ":" *TRACE-OUTPUT*)
	  (DO ((TAIL STUFF (CDR TAIL)))
	      ((ATOM TAIL)
	       (WHEN TAIL
		 (PRINC " . " *TRACE-OUTPUT*)
		 (PRIN1 TAIL *TRACE-OUTPUT*)))
	    (TYO #/SP *TRACE-OUTPUT*)
	    (PRIN1 (CAR TAIL) *TRACE-OUTPUT*)))))
 (COND (EXTRAS-1
	(PRINC "  \\" *TRACE-OUTPUT*)
	(DOLIST (E EXTRAS-1)
	  (PRINC " " *TRACE-OUTPUT*)
	  (PRIN1 (EVAL E) *TRACE-OUTPUT*))))
 (COND (EXTRAS-2
	(PRINC "  ////" *TRACE-OUTPUT*)
	(DOLIST (E EXTRAS-2)
	  (PRINC " " *TRACE-OUTPUT*)
	  (PRIN1 (EVAL E) *TRACE-OUTPUT*))))
 (PRINC ")" *TRACE-OUTPUT*))

))

; From file ENCAPS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ENCAPS  "

(defmacro encapsulate (function-spec outer-function-spec type body &optional extra-debugging-info)
  "Encapsulate the function named FUNCTION-SPEC
with an encapsulation whose body is the value of BODY and whose type is TYPE.
The args are all evaluated, but BODY is evaluated inside some bindings.
OUTER-FUNCTION-SPEC is the function spec the user knows about;
FUNCTION-SPEC itself may be an unencapsulated version of OUTER-FUNCTION-SPEC
so as to cause this encapsulation to go inside other existing ones.

Inside BODY, refer to the variable ENCAPSULATED-FUNCTION to get an object
which you can funcall to invoke the original definition of the function.

FUNCTION-SPEC is redefined with the new encapsulation.
The value returned is the symbol used to hold the original definition.
Within the code which constructs the body, this symbol is the value of COPY."
  `(let* ((default-cons-area background-cons-area)
	  (copy (make-symbol (if (symbolp ,function-spec)
				 (symbol-name ,function-spec)
			       (format nil "~s" ,function-spec))))
	  (defp (fdefinedp ,function-spec))
	  (def (and defp (fdefinition ,function-spec)))
	  (self-flavor-decl (assq ':self-flavor (debugging-info def)))
	  encapsulated-function
	  lambda-list arglist-constructor macro-def)
     ;; Figure out whether we are operating on a macro, and in any case
     ;; compute the lambda list which the encapsulation will use.
     (if defp
	 (setq macro-def (encapsulation-macro-definition def)
	       lambda-list (encapsulation-lambda-list def))
       (setq lambda-list '(&rest .arglist.)))
     (and (symbolp lambda-list)
	  (ferror nil "~S cannot be encapsulated due to hairy arg quoting"
		      ,outer-function-spec))
     (setq arglist-constructor
	   `(encapsulation-list* . ,(cdr (encapsulation-arglist-constructor lambda-list))))
     ;; Copy the original definition, if any, to the copied symbol.
     (and defp (fset copy def))
     ;; Cons up what the body ought to use to call the original definition.
     (setq encapsulated-function (cond (macro-def
					`(encapsulation-macro-definition #',copy))
				       (t `#',copy)))
     (setq def
	   `(,(if (eq interpreter-function-environment t)
		  'named-lambda 'cli:named-lambda)
	      (,,function-spec (encapsulated-definition ,copy ,,type)
	       ,@(if self-flavor-decl (list self-flavor-decl))
	       . ,,extra-debugging-info)
	      ,lambda-list
	      (encapsulation-let ((arglist ,arglist-constructor))
	         (declare (special arglist values))
		 ,,body)))
     ;; If this encapsulation goes inside rename-withins,
     ;; then do any renamings on it.
     (and (memq 'rename-within (cdr (memq ,type encapsulation-standard-order)))
	  (setq def (rename-within-new-definition-maybe ,outer-function-spec def)))
     (and macro-def (setq def (cons 'macro def)))
     (fdefine ,function-spec def nil t)
     copy))


))

; From file ENCAPS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ENCAPS  "


(DEFUN ENCAPSULATION-LAMBDA-LIST (FUNCTION)
  "Return a lambda list good for use in an encapsulation to go around FUNCTION.
The lambda list we return is computed from FUNCTION's arglist."
  (COND ((NULL FUNCTION)
	 '(&REST .ARGLIST.))			;If fn is not defined, NIL is supplied to us.
						;Assume a typical function, since can't know.
	((SYMBOLP FUNCTION)
	 (COND ((FBOUNDP FUNCTION) (ENCAPSULATION-LAMBDA-LIST (FSYMEVAL FUNCTION)))
	       (T '(&REST .ARGLIST.))))
	((CONSP FUNCTION)
	 (SELECTQ (CAR FUNCTION)
	   ((LAMBDA CLI:LAMBDA) (ENCAPSULATION-CONVERT-LAMBDA (CADR FUNCTION)))
	   ((NAMED-LAMBDA CLI:NAMED-LAMBDA)
	    (ENCAPSULATION-CONVERT-LAMBDA (CADDR FUNCTION)))
	   (OTHERWISE '(&REST .ARGLIST.))))
	(T ;A compiled or microcode function
	 (ENCAPSULATION-CONVERT-LAMBDA (ARGLIST FUNCTION T)))))

))

; From file ENCAPS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ENCAPS  "


(DEFUN ENCAPSULATION-CONVERT-LAMBDA (LL
	&AUX EVARG QUARG EVOPT QUOPT EVREST QUREST)
  ;;First determine what types of evalage and quotage are present (set above aux vars)
  (DO ((L LL (CDR L))
       (ITEM)
       (OPTIONALP NIL)
       (QUOTEP NIL)
       (RESTP NIL))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (COND ((EQ ITEM '&AUX)
	   (RETURN NIL))
	  ((EQ ITEM '&EVAL)
	   (SETQ QUOTEP NIL))
	  ((EQ ITEM '&QUOTE)
	   (SETQ QUOTEP T))
	  ((EQ ITEM '&OPTIONAL)
	   (SETQ OPTIONALP T))
	  ((OR (EQ ITEM '&REST) (EQ ITEM '&KEY))
	   (SETQ RESTP T))
	  ((MEMQ ITEM LAMBDA-LIST-KEYWORDS))
	  (RESTP
	   (IF QUOTEP (SETQ QUREST T) (SETQ EVREST T))
	   (RETURN NIL))
	  (OPTIONALP
	   (IF QUOTEP (SETQ QUOPT T) (SETQ EVOPT T)))
	  (T (COND (QUOTEP (SETQ QUARG T))
		   (T (SETQ EVARG T))))))
  ;;Decide how hairy a lambda list is needed
  (COND ((AND (NOT QUARG) (NOT QUOPT) (NOT QUREST))
	 '(&EVAL &REST .ARGLIST.))
	((AND (NOT EVARG) (NOT EVOPT) (NOT EVREST))
	 '(&QUOTE &REST .ARGLIST.))
	(T;;Need a hairy one.
	  (NRECONC
	    (DO ((L LL (CDR L))
	         (LAMBDA-LIST NIL)
		 OPTIONALP
	         (ITEM))
	        ((NULL L) LAMBDA-LIST)
	      (SETQ ITEM (CAR L))
	      (COND ((MEMQ ITEM '(&AUX &REST &KEY))
		     (RETURN LAMBDA-LIST))
		    ((MEMQ ITEM '(&EVAL &QUOTE))
		     (SETQ LAMBDA-LIST (CONS ITEM LAMBDA-LIST)))
		    ((EQ ITEM '&OPTIONAL)
		     (OR OPTIONALP (SETQ LAMBDA-LIST (CONS ITEM LAMBDA-LIST)))
		     (SETQ OPTIONALP T))
		    ((MEMQ ITEM LAMBDA-LIST-KEYWORDS))
		    (OPTIONALP
		     (SETQ LAMBDA-LIST (CONS (LIST (GENSYM) NIL (GENSYM)) LAMBDA-LIST)))
		    (T
		     (SETQ LAMBDA-LIST (CONS (GENSYM) LAMBDA-LIST)))))
	    '(&REST .ARGLIST.)))))

;;; Implement RENAME-WITHIN encapsulations.

;; Rename FUNCTION-TO-RENAME within WITHIN-FUNCTION
;; and make an entry in WITHIN-FUNCTION's encapsulation to record the act.
;; The renamed function is defined by a pointer
;; to the original symbol FUNCTION-TO-RENAME.
;; Return the renamed function name (a symbol).

))

; From file QTRACE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; QTRACE  "

(DEFUN TRACE-1 (SPEC)
  (PROG (BREAK EXITBREAK ENTRYCOND EXITCOND WHEREIN ARGPDL ENTRY EXIT (ARG T) (VALUE T) 
	     STEP (BARFP T) STEPCOND
	     ENTRYVALS EXITVALS MUMBLE FCN SPEC1 TRFCN ERROR
	     (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (COND ((ATOM SPEC)
	   (SETQ FCN SPEC))
	  (T
	   (COND ((EQ (CAR SPEC) ':FUNCTION)
		  (SETQ FCN (CADR SPEC) SPEC (CDR SPEC)))
		 ((ATOM (CAR SPEC))
		  (SETQ FCN (CAR SPEC)))
		 (T (RETURN (LOOP FOR FCN IN (CAR SPEC)
				  NCONC (TRACE-1 `(:FUNCTION ,FCN . ,(CDR SPEC)))))))
	   (DO SPECS (CDR SPEC) (CDR SPECS) (NULL SPECS)
	     (SELECTQ (CAR SPECS)
		(:BREAK (SETQ BARFP SPECS SPECS (CDR SPECS) BREAK (CAR SPECS)))
		(:EXITBREAK (SETQ BARFP SPECS SPECS (CDR SPECS) EXITBREAK (CAR SPECS)))
		(:STEPCOND (SETQ BARFP SPECS SPECS (CDR SPECS) STEPCOND (CAR SPECS)
				 STEP T))
		(:STEP (SETQ STEP T))
		(:ERROR (SETQ ERROR T))
		(:COND (SETQ BARFP SPECS SPECS (CDR SPECS))
		       (SETQ STEPCOND (SETQ EXITCOND (SETQ ENTRYCOND (CAR SPECS)))))
		(:ENTRYCOND (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRYCOND (CAR SPECS)))
		(:EXITCOND (SETQ BARFP SPECS SPECS (CDR SPECS) EXITCOND (CAR SPECS)))
		(:WHEREIN (SETQ BARFP SPECS SPECS (CDR SPECS) WHEREIN (CAR SPECS)))
		(:ARGPDL (SETQ BARFP SPECS SPECS (CDR SPECS) ARGPDL (CAR SPECS)))
		(:ENTRY (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRY (CAR SPECS)))
		(:EXIT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CAR SPECS)))
		(:PRINT (SETQ BARFP SPECS
			      SPECS (CDR SPECS)
			      ENTRY (CONS (CAR SPECS) ENTRY)
			      EXIT (CONS (CAR SPECS) EXIT)))
		(:ENTRYPRINT (SETQ BARFP SPECS SPECS (CDR SPECS)
				   ENTRY (CONS (CAR SPECS) ENTRY)))
		(:EXITPRINT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CONS (CAR SPECS) EXIT)))
		((:ARG :VALUE :BOTH NIL)
		 (AND (EQ (CAR SPECS) ':ARG) (SETQ VALUE NIL))
		 (AND (EQ (CAR SPECS) ':VALUE) (SETQ ARG NIL))
		 (AND (EQ (CAR SPECS) NIL) (SETQ ARG NIL VALUE NIL))
		 (AND ARG (SETQ ENTRYVALS (CDR SPECS)))
		 (AND VALUE (SETQ EXITVALS (CDR SPECS)))
		 (RETURN NIL))
		(OTHERWISE
		 (SETQ MUMBLE (CAR SPECS))
		 (RETURN NIL)))
	     (AND (NULL BARFP) (FERROR NIL "Parameter missing")) )))
    (SETQ FCN (DWIMIFY-ARG-PACKAGE FCN 'FCN))
    (UNTRACE-1 FCN)
    (AND MUMBLE (RETURN (FERROR NIL "Meaningless TRACE keyword: ~S" MUMBLE)))
    (CHECK-ARG ARGPDL SYMBOLP "a symbol")
    (SETQ SPEC1 (UNENCAPSULATE-FUNCTION-SPEC FCN 'TRACE))

    (SETQ TRFCN (ENCAPSULATE SPEC1 FCN 'TRACE
       `(PROG* (,@(AND ARGPDL `((,ARGPDL (CONS (LIST (1+ ,COPY) ',FCN ARGLIST)
					       ,ARGPDL))))
		VALUES
		(,COPY (1+ ,COPY))
		(TRACE-LEVEL (1+ TRACE-LEVEL)))
	       (DECLARE (SPECIAL ,COPY VALUES))
	       ;; End of PROG var list.
	       ,(IF ERROR `(PROGN (LET ((EH:ERROR-DEPTH (1+ EH:ERROR-DEPTH))
					(EH:CONDITION-PROCEED-TYPES '(:NO-ACTION)))
				    (EH:INVOKE-DEBUGGER
				      (MAKE-CONDITION 'EH:TRACE-BREAKPOINT
						      "~S entered" ',FCN)))
				  (RETURN-LIST (MULTIPLE-VALUE-LIST
						 (APPLY ,ENCAPSULATED-FUNCTION ARGLIST))))
		  `(COND ((OR INSIDE-TRACE
			      . ,(AND WHEREIN `((NOT (FUNCTION-ACTIVE-P ',WHEREIN)))))
			  (RETURN-LIST (MULTIPLE-VALUE-LIST
					 (APPLY ,ENCAPSULATED-FUNCTION ARGLIST))))
			 (T (LET ((INSIDE-TRACE T))
			      ,(TRACE-MAYBE-CONDITIONALIZE ENTRYCOND
					 `(TRACE-PRINT ,COPY 'ENTER ',FCN ',ARG
						       ',ENTRY ',ENTRYVALS))
			      ,@(AND BREAK `((AND ,BREAK (LET (INSIDE-TRACE)
					 (BREAK "Entering ~S." ',FCN)))))
			      (SETQ VALUES
				    (LET ((INSIDE-TRACE NIL))
				      (MULTIPLE-VALUE-LIST
					,(IF (AND STEP STEPCOND)
					     ;; conditionally call the stepper.
					     `(IF ,STEPCOND
						  (TRACE-STEP-APPLY
						    ,ENCAPSULATED-FUNCTION
						    ARGLIST)
						(TRACE-APPLY
						  ,ENCAPSULATED-FUNCTION
						  ARGLIST))
					   `(,(IF STEP 'TRACE-STEP-APPLY 'TRACE-APPLY)
					        ,ENCAPSULATED-FUNCTION
					        ARGLIST)))))
			      ,(TRACE-MAYBE-CONDITIONALIZE EXITCOND
					 `(TRACE-PRINT ,COPY 'EXIT ',FCN ',VALUE
						       ',EXIT ',EXITVALS))
			      ,@(AND EXITBREAK
				     `((AND ,EXITBREAK (LET (INSIDE-TRACE)
							 (BREAK "Exiting ~S." ',FCN)))))
			      (RETURN-LIST VALUES))))))))
    (SET TRFCN 0)
    (PUSH FCN TRACED-FUNCTIONS)
    (IF (OR TRACE-COMPILE-FLAG COMPILE-ENCAPSULATIONS-FLAG)
	(COMPILE-ENCAPSULATIONS SPEC1 'TRACE))
    (RETURN (NCONS FCN))))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defun breakon-init (function-spec)
  (let ((default-cons-area background-cons-area)
	(spec1 (si:unencapsulate-function-spec function-spec 'breakon)))
    (cond ((eq spec1 (si:unencapsulate-function-spec spec1 '(breakon)))
	   (si:encapsulate spec1 function-spec 'breakon
			   ;; Must cons the (OR) afresh -- it gets RPLAC'd.
			   `(breakon-this-time ,(list 'or)
					       ,si:encapsulated-function
					       arglist))
	   (push function-spec breakon-functions)))))

))

; From file ENCAPS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ENCAPS  "

(defun rename-within-init (function &aux spec1)
  (setq spec1 (unencapsulate-function-spec function 'rename-within))
  (or (and (fdefinedp spec1)
	   (rename-within-renamings-slot spec1))
      (progn (push function rename-within-functions)
	     (encapsulate spec1 function 'rename-within
			  `(apply ,encapsulated-function arglist)
			  (copytree
			    '((renamings nil))))
	     (if compile-encapsulations-flag
		 (compile-encapsulations spec1 'rename-within))))
  function)

))

; From file STEP.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STEP  "

(defun step-cmdr (form values print-form-p &optional apply-p)
  (declare (special apply-p))
  (prog (ch ch1
	 (standard-input query-io)
	 (standard-output query-io))
    (if step-auto
	(if (eq step-auto 'no-print)
	    (progn (setq step-max (1+ step-level)) (return 'evalhook))))
    (and print-form-p
	 (step-print-form form step-level apply-p))
 pv (do ((l values (cdr l))
         (ch #/ #/))
        ((null l))
      (terpri-if-insufficient-space 80.)
      (tyo #\SP) (tyo ch) (tyo #\SP)
      (print-truncated (car l) 98.))		;Several windows lose if this is 100.
 rd (setq ch1 (if step-auto #\c-N (funcall standard-input ':tyi)))
    (setq ch (char-upcase ch1))
    (cond ((= ch #\CALL) (break "for CALL key."))
          ((= ch #\SP) (setq step-max step-level) (return 'eval))
          ((= ch #\c-U) (setq step-max (max 0 (1- step-level))) (return 'eval))
          ((= ch #\c-N) (setq step-max (1+ step-level)) (return 'evalhook))
          ((= ch #\c-X) (setq step-max -1) (return 'eval))
	  ((and (= ch #\c-A)
		(not apply-p))
	   (setq step-max (1+ step-level)) (return 'applyhook))
          ((= ch #\c-B)
           (break)
           (setq ch 0)
           (as-1 step-form step-array step-level)
           (as-1 apply-p step-apply-p-array step-level)
           (go redis1))
          ((= ch #\c-E)
           (ed)
           (setq ch 10.)
           (go redisplay))
          ((or (= ch #\Clear-Screen) (= ch #\c-L))
           (setq ch 10.)
           (go redisplay))
	  ((= ch #\m-L)
	   (setq ch 10.)
	   (go redis1))
          ((= ch #\c-m-L)
           (setq ch step-level)
           (go redisplay))
          ((or (= ch #\c-G) (= ch #\c-T))
           (setq ch (cond ((= ch #\c-G) #'grind-top-level) (t #'print)))
           (cond ((null values) (funcall ch form))
                 ((do l values (cdr l) (null l)
                    (funcall ch (car l)))))
           (go rd))
	  ((= (ldb %%ch-char ch) '#\HELP)
	   (si:with-help-stream (help-str :label "Stepper help")
	     (terpri help-str)
	     (princ
	       (cond ((null step-values)
		      (if apply-p
			  "You are about to apply the above function to the above arguments."
			"You are about to evaluate the above form."))
		     (t
		      (if apply-p
			  "You have applied a function to arguments
and are about to return the above values."
			"You have evaluated a form and are about to return the above values.")))
	       help-str)
	     (terpri help-str)
	     (princ
"Commands are single characters, usually control, which don't echo:

    C-N	    Proceed to next thing evaled.
    <space> Proceed to next thing evaled at same level.
    C-A     Eval the args without stepping; stop before applying the function.
    C-U	    Proceed to first thing up one level.
    C-X	    Continue without further stepping.
    C-E	    Escape to editor.
    C-T	    Retype current form in full.
    C-G	    Grind current form.
    C-B	    Enter breakpoint, with the following variables bound:
	        STEP-FORM is the form, STEP-VALUES is the list of values,
	        STEP-VALUE is the first value.  If you change these, it wins.
    C-L
    <form>  Clear & show last 10. forms.
    M-L	    Just show last 10. forms (don't clear).
    C-M-L   Clear and show all forms.
    <any LISP form>
	    Will be read and evaluated, and values printed.

Magic flags preceding output:

       Ordinary LISP form
       About to apply a function
       Macro
       Values
       Separates multiple values
"
                    help-str))
           (setq ch 0)
	   ;; No need to redisplay if with-help-stream used a separate window.
	   (if (typep terminal-io 'tv:sheet) (go rd))
	   (go redis1))
	  ((< ch 200)
	   (funcall standard-input ':untyi ch1)
	   (catch-error-restart ((sys:abort error) "Back to STEP command level.")
	     (print
	       (eval-abort-trivial-errors 
		 (cond ((memq ':rubout-handler (funcall standard-input ':which-operations))
			(multiple-value-bind (sexp flag)
			    (funcall standard-input
				     ':rubout-handler
				     '((:full-rubout :full-rubout)
				       (:prompt " Eval: "))
				     #'si:read-for-top-level)
			  (when (eq flag ':full-rubout)
			    (go rd))
			  sexp))
		       ;; If stream has no rubout handler, degrade gracefully.
		       (t
			(funcall standard-output ':string-out " Eval: ")
			(si:read-for-top-level))))))
	   (terpri)
	   (setq ch 0)
	   (go redis1))
          (t (beep)
             (go rd)))
 redisplay 
    (funcall standard-output ':clear-screen)
 redis1 
    (do i (max 0 (- step-level ch)) (1+ i) (> i step-level)
      (step-print-form (ar-1 step-array i) i (aref step-apply-p-array i)))
    (go pv)))

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFUN COM-STRING-SEARCH-INTERNAL (REVERSEP BJP ZJP TOP-P &AUX TEM)
  (UNWIND-PROTECT
    (PROG ((STRING (MAKE-STRING 10 ':FILL-POINTER 0))
	   (ORIG-PT (COPY-BP (POINT))) (FCN 'SEARCH)
	   XCHAR CHAR HACK1 HACK2 ECHOED-P FAILED-P
	   REPEATING-STRING-NOT-ECHOED-FLAG
	   SEARCHING-DONE-AT-LEAST-ONCE)
       REDIS (COND ((NULL (SETQ XCHAR (AND (NOT ECHOED-P)
					   (SEND *STANDARD-INPUT* ':TYI-NO-HANG))))
		    (SETQ ECHOED-P T)			;Started to echo now
		    (TYPEIN-LINE-WITH-REDISPLAY "~:|")
		    (AND BJP (FORMAT *QUERY-IO* "Begin "))
		    (AND ZJP (FORMAT *QUERY-IO* "End "))
		    (AND TOP-P (FORMAT *QUERY-IO* "Top Line "))
		    (AND REVERSEP (FORMAT *QUERY-IO* "Reverse "))
		    (FORMAT *QUERY-IO* (SELECTQ FCN
				       (SEARCH "String search: ")
				       (WORD-SEARCH "Word search: ")
				       (DELIMITED-SEARCH "Delimited search: ")))
		    (FORMAT *QUERY-IO* "~A" STRING)))
	     (AND FAILED-P (GO FAILED))
	     (GO LOP1)
        LOOP (SETQ XCHAR (AND (NOT ECHOED-P)
			      (SEND *STANDARD-INPUT* ':TYI-NO-HANG)))
	LOP1 (SETQ CHAR (OR XCHAR (TYPEIN-LINE-ACTIVATE (TYI-WITH-SCROLLING-AND-MOUSING))))
             (SETQ HACK2 HACK1 HACK1 NIL)
	     (COND ((CHAR-BIT CHAR ':CONTROL)
		    (SETQ CHAR (CHAR-UPCASE (CHAR-CODE CHAR)))
		    (SELECT CHAR
                         (#/B (SETQ BJP T ZJP NIL REVERSEP NIL)
			      (GO REDIS))
                         (#/E (SETQ BJP NIL ZJP T REVERSEP T)
			      (GO REDIS))
			 (#/F (SETQ *CENTERING-FRACTION* 0.0s0 TOP-P T)
			      (GO REDIS))
                         (#/G (SEND *QUERY-IO* ':MAKE-COMPLETE)
			      (WHEN SEARCHING-DONE-AT-LEAST-ONCE
				(SEARCH-RING-PUSH STRING FCN))
                              (BARF))
                         (#/D (MULTIPLE-VALUE (TEM FCN)
				(SEARCH-RING-POP))
			      (COND ((NUMBERP TEM)
				     (SETQ STRING (MAKE-STRING 10 ':FILL-POINTER 1))
				     (SETF (AREF STRING 0) TEM))
				    (T (SETQ STRING TEM)))
			      (GO REDIS))
                         (#/L (GO REDIS))
                         (#/M (IF (NOT (WINDOW-MARK-P *WINDOW*))
				  (BEEP)
				  (REGION (BP1 BP2)
				    (APPEND-TO-ARRAY STRING (STRING-INTERVAL BP1 BP2 T)))
				  (SETF (WINDOW-MARK-P *WINDOW*) NIL)
				  (MUST-REDISPLAY *WINDOW* DIS-MARK-GOES)
				  (REDISPLAY *WINDOW* ':NONE))
			      (GO REDIS))
			 (#/Q (TYPEIN-LINE-ACTIVATE
				(SETQ CHAR (SEND *STANDARD-INPUT* ':TYI)))
			      (SETQ CHAR (LOGAND (IF (LDB-TEST %%KBD-CONTROL CHAR)
						     37 377)
						 CHAR))
			      (GO NORMAL))
                         (#/R (SETQ REVERSEP (NOT REVERSEP))
			      (GO REDIS))
                         (#/S (AND (EQUAL "" STRING)
				   *SEARCH-RING*
				   (SETQ STRING (CAAR *SEARCH-RING*)
					 REPEATING-STRING-NOT-ECHOED-FLAG T
					 FCN (CADAR *SEARCH-RING*)))
			      (LET ((TEM (FUNCALL FCN
                                                  (COND (ZJP (INTERVAL-LAST-BP *INTERVAL*))
                                                        (BJP (INTERVAL-FIRST-BP *INTERVAL*))
                                                        (T (POINT)))
						  STRING
                                                  REVERSEP)))
                                (COND ((NULL TEM)
                                       ;; Next line commented out for Emacs compatibility
                                       ;(BEEP)
				       ;; Comment this BARF instead to stay in search if fail
				       ;; But don't forget to update search default ring
				       (OR (EQUAL "" STRING)
					   (SEARCH-RING-PUSH STRING FCN))
				       (GO FAILED)
                                       )
				      (T (MOVE-BP (POINT) TEM)
					 (MUST-REDISPLAY *WINDOW* DIS-BPS)
					 (AND (WINDOW-READY-P *WINDOW*)	;Minibuffer
					      (REDISPLAY *WINDOW* ':POINT))
					 (SETQ BJP NIL ZJP NIL)
					 (AND TOP-P
					      (SETQ *CENTERING-FRACTION* 0.0s0))
					 (SETQ SEARCHING-DONE-AT-LEAST-ONCE T)
					 (SETQ HACK1 T))))
			      (IF (NULL XCHAR)
				  (GO LOOP)
				  (SETQ ECHOED-P T)
				  (GO REDIS)))
                         (#/U (STORE-ARRAY-LEADER 0 STRING 0)
                              (GO REDIS))
                         (#/V (SETQ FCN 'DELIMITED-SEARCH)
			      (GO REDIS))
			 (#/W (SETQ FCN 'WORD-SEARCH)
                              (GO REDIS))
                         (#/Y (SETQ TEM (CAAR *SEARCH-RING*))
			      (COND ((NULL TEM)
				     (BEEP))
				    ((NUMBERP TEM)
				     (ARRAY-PUSH-EXTEND STRING TEM))
				    (T
				     (APPEND-TO-ARRAY STRING TEM)))
			      (GO REDIS))
			 (OTHERWISE (BEEP)
                                    (GO REDIS))))
		   ((= CHAR #/RUBOUT)
		    (OR (ZEROP (ARRAY-LEADER STRING 0))
			(ARRAY-POP STRING))
		    (GO REDIS))
		   ((= CHAR #/HELP)
		    (DOC-STRING-SEARCH *CURRENT-COMMAND* ':FULL
				       "Search for a specified string.")
		    (SEND *STANDARD-INPUT* ':UNTYI (SEND *STANDARD-INPUT* ':ANY-TYI))
		    (GO REDIS))
		   ((= CHAR #/CLEAR-INPUT)
		    (STORE-ARRAY-LEADER 0 STRING 0)
		    (GO REDIS))
		   ((OR (= CHAR #/) (= CHAR #/END))
		    (OR XCHAR
			(FORMAT *QUERY-IO* "~C" CHAR))
		    (OR (EQUAL "" STRING)
			(SEARCH-RING-PUSH STRING FCN))
		    (OR HACK2
		        (DO ((ARG (ABS *NUMERIC-ARG*) (1- ARG))
			     (KEY (COND ((AND (EQUAL "" STRING)
					      *SEARCH-RING*)
					 (SETQ FCN (CADAR *SEARCH-RING*))
					 (CAAR *SEARCH-RING*))
					(T STRING)))
			     (BP (COND (ZJP (INTERVAL-LAST-BP *INTERVAL*))
				       (BJP (INTERVAL-FIRST-BP *INTERVAL*))
				       (T (POINT)))))
			    (( ARG 0) (MOVE-BP (POINT) BP))
			  (OR (SETQ BP (FUNCALL FCN BP KEY REVERSEP))
			      (GO FAILED))))
		    (MAYBE-PUSH-POINT ORIG-PT)
		    (RETURN DIS-BPS)))
	     (SETQ CHAR (LOGAND 377 CHAR))
      NORMAL (ARRAY-PUSH-EXTEND STRING CHAR)
             (IF XCHAR
		 (GO REDIS)
	       (SETQ ECHOED-P T)		;Started to echo
	       (FORMAT *QUERY-IO* "~C" CHAR)
	       (GO LOOP))
      FAILED (COND (XCHAR			;Typed ahead failing search, force redisplay
		    (SETQ FAILED-P T ECHOED-P T)
		    (GO REDIS)))
	     (WHEN REPEATING-STRING-NOT-ECHOED-FLAG
	       (FORMAT *QUERY-IO* "~A" STRING))
	     (WHEN FAILED-P			;Typed ahead last time
	       (FORMAT *QUERY-IO* ""))
	     (FORMAT *QUERY-IO* " Search failed.")
	     (BARF))
    (SEND *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)))

))

(DEFMACRO CLI:CATCH (TAG &BODY BODY)
  `(*CATCH ,TAG . ,BODY))

(DEFMACRO CLI:THROW (TAG &BODY BODY)
  `(*THROW ,TAG . ,BODY))

(SETF (DOCUMENTATION 'CLI:CATCH 'FUNCTION) (DOCUMENTATION '*CATCH 'FUNCTION))
(SETF (DOCUMENTATION 'CLI:THROW 'FUNCTION) (DOCUMENTATION '*THROW 'FUNCTION))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "


(DEFCOM COM-QUERY-REPLACE "Replace string, asking about each occurrence.
Prompts for each string.  If you first give it FOO, then BAR, it
finds the first FOO, displays, and
reads a character.  Space => replace it with BAR and show next FOO.
Rubout => don't replace, but show next FOO.
Comma => replace this FOO and show result, waiting for a
space, R or Altmode.
Period => replace this FOO and exit.  Altmode => just exit.
^ => return to site of previous FOO (actually, pop the point pdl).
W => kill this FOO and enter recursive edit.
R => enter editing mode recursively.  L => redisplay screen.
Exclamation mark => replace all remaining FOOs without asking.
Any other character exits and (except altmode) is read again.
If *CASE-REPLACE-P* is nonnull, BAR's initial will be capitalized
if FOO's initial had been.
If you give a numeric argument, it will not consider FOOs that are not
bounded on both sides by delimiter characters." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (LET ((START-BP (POINT))
	  (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
	(UNLESS (BP-< (POINT) (MARK))
	  (SWAP-BPS (POINT) (MARK)))
	(SETQ END-BP (MARK)))
      (QUERY-REPLACE START-BP END-BP FROM TO *NUMERIC-ARG-P*)))
  DIS-TEXT)

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFUN QUERY-REPLACE (START-BP END-BP *QUERY-FROM* *QUERY-TO* &OPTIONAL BREAKS
		      &AUX (*CASE-REPLACE-P* *CASE-REPLACE-P*))
  "Query replace *QUERY-FROM* with *QUERY-TO* from START-BP to END-BP.
If *QUERY-FROM* is not all lower case, don't try to preserve case.
BREAKS non-NIL means only consider replacing occurrences surrounded by delimiters."
  ;;If from isn't all lowercase, user probably has something specific in mind
  (AND (DO ((I 0 (1+ I))
	    (LEN (STRING-LENGTH *QUERY-FROM*)))
	   (( I LEN))
	 (AND (CHAR-UPPERCASE-P (AREF *QUERY-FROM* I))
	      (RETURN T)))
       (SETQ *CASE-REPLACE-P* NIL))
  (QUERY-REPLACE-INTERNAL START-BP END-BP *QUERY-FROM* *QUERY-TO* 'QUERY-REPLACE-SEARCH BREAKS))

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "


(DEFUN QUERY-REPLACE-SEARCH (BP TO-BP QUERY-FROM IGNORE &AUX BP1)
  (AND (SETQ BP1 (SEARCH BP QUERY-FROM NIL NIL NIL TO-BP))
       (VALUES BP1 (FORWARD-CHAR BP1 (- (STRING-LENGTH QUERY-FROM))))))

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFUN QUERY-REPLACE-INTERNAL (BP END-BP QUERY-FROM QUERY-TO FUNCTION BREAKS
			       &AUX BP1 BP2 START-BP DO-THE-REST CHAR UCHAR FLAG-1 FLAG-2)
  (BIND-MODE-LINE '("Query Replacing " *QUERY-FROM* " => " *QUERY-TO*)
    (SETQ START-BP (COPY-BP BP)
	  BP1 (COPY-BP BP)
	  BP2 (COPY-BP BP))
    (DO () (NIL)
      (SETQ FLAG-2 FLAG-1 FLAG-1 NIL)
      (COND ((NOT FLAG-2)
	     (MULTIPLE-VALUE (BP2 BP1)
	       (FUNCALL FUNCTION BP2 END-BP QUERY-FROM QUERY-TO))
	     (UNLESS BP2
	       (FORMAT *QUERY-IO* "~&No more occurences.")
	       (RETURN NIL))))
      (COND ((OR FLAG-2
		 (NOT BREAKS)			; If we don't care about breaks, go ahead.
		 (AND				; Both beginning and end must be breaks.
		   (OR (BP-= BP2 END-BP)	; EOB counts as a break.
		       (= (WORD-SYNTAX (BP-CHAR BP2)) WORD-DELIMITER))
		   (OR (BP-= BP1 START-BP)
		       (= (WORD-SYNTAX (BP-CHAR-BEFORE BP1)) WORD-DELIMITER))))
	     ;; Move point after checking delimiters
	     (COND ((NOT FLAG-2)
		    (MOVE-BP BP BP2)
		    (MUST-REDISPLAY *WINDOW* DIS-BPS)))
	     ;; We want to offer this string for replacement.
	     (COND (DO-THE-REST (QREP))
		   (T
		    (REDISPLAY *WINDOW* ':POINT)
		    (REDISPLAY-MODE-LINE)
		    (POINT-PDL-PUSH BP *WINDOW*)
		    (PROG ()
		       GETCHAR
			  (SETQ CHAR (SEND *STANDARD-INPUT* ':TYI))
			  (OR (NUMBERP CHAR) (GO GETCHAR))	;Ignore special request
			  (SETQ UCHAR (CHAR-UPCASE CHAR))
			  (COND ((= UCHAR #/^)
				 (POINT-PDL-POP *WINDOW*)	;Already done once
				 (MULTIPLE-VALUE-BIND (BP1 PLINE)
				     (POINT-PDL-POP *WINDOW*)
				   (MOVE-BP BP BP1)
				   (REDISPLAY-POINT-ON-PLINE BP *WINDOW* PLINE))
				 (MUST-REDISPLAY *WINDOW* DIS-BPS)
				 (REDISPLAY *WINDOW* ':POINT)
				 (GO GETCHAR))
				((= CHAR #/C-R)	;C-R: Recurse.
				 (WITH-BP (BP1-COPY BP1 ':MOVES)
				   (WITH-BP (BP2-COPY BP2 ':NORMAL)
				     (CONTROL-R)
				     (MOVE-BP (POINT) BP2-COPY)
				     (MOVE-BP BP1 BP1-COPY)
				     (IF (BP-< BP2-COPY BP1-COPY)
					 (MOVE-BP BP2 BP1-COPY)
				       (MOVE-BP BP2 BP2-COPY))))
				 (MUST-REDISPLAY *WINDOW* DIS-BPS)
				 (REDISPLAY *WINDOW* ':POINT)
				 (REDISPLAY-MODE-LINE)
				 (GO GETCHAR))
				((MEMQ UCHAR '(#/FF #/C-L))
				 (MUST-REDISPLAY *WINDOW*
						 (IF (= UCHAR #/FF) DIS-ALL
						     (COM-RECENTER-WINDOW)))
				 (REDISPLAY *WINDOW* ':POINT)
				 (GO GETCHAR))
				((MEMQ UCHAR '(#/? #/HELP))
				 (PRINT-DOC ':FULL *CURRENT-COMMAND*)
				 (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT)
				 (REDISPLAY-ALL-WINDOWS)
				 (GO GETCHAR))))
		    (SELECTQ UCHAR
		      (#/SP (QREP))		;Space: Replace and continue.
		      (#/RUBOUT NIL)		;Rubout: Continue.
		      (#/,			;Comma:
		       (QREP)
		       (SETQ FLAG-1 T))
		      ((#/ #/END) (RETURN NIL))	;Altmode: Quit.
		      (#/. (QREP)		;Point: Replace and quit.
		       (RETURN NIL))
		      (#/C-W			;C-W: Delete, and recurse.
		       (DELETE-INTERVAL BP BP1)
		       (MUST-REDISPLAY *WINDOW* DIS-TEXT)
		       (CONTROL-R))
		      (#/! (QREP)		;!: Do this and rest.
		       (SETQ DO-THE-REST T))
		      (OTHERWISE
		       (SEND *STANDARD-INPUT* ':UNTYI CHAR)
		       (RETURN 'ABORTED))))))))))

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-QUERY-EXCHANGE "Query replace two strings with one another at the same time.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*) "exchange")
    (LET ((START-BP (POINT))
	  (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
	(UNLESS (BP-< (POINT) (MARK))
	  (SWAP-BPS (POINT) (MARK)))
	(SETQ END-BP (MARK)))
      (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
					  *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
	(QUERY-REPLACE-LIST START-BP END-BP (LIST FROM TO) (LIST TO FROM)
			    *NUMERIC-ARG-P*))))
  DIS-TEXT)

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "

(DEFCOM COM-MULTIPLE-QUERY-REPLACE "Query replace two sets of strings at the same time.
Strings are read in alternate mini-buffers, ended by a null string.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (MULTIPLE-QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (LET ((START-BP (POINT))
	  (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
	(UNLESS (BP-< (POINT) (MARK))
	  (SWAP-BPS (POINT) (MARK)))
	(SETQ END-BP (MARK)))
      (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
					  *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
	(QUERY-REPLACE-LIST START-BP END-BP FROM-LIST TO-LIST *NUMERIC-ARG-P*))))
  DIS-TEXT)

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "


(DEFUN QUERY-REPLACE-LIST (*BP* END-BP FROM-LIST TO-LIST &OPTIONAL BREAKS
			   &AUX *QUERY-FROM* *QUERY-TO* (*STATE* 0))
  (DECLARE (SPECIAL *BP* *STATE*))
  (QUERY-REPLACE-INTERNAL *BP* END-BP FROM-LIST TO-LIST 'QUERY-REPLACE-SEARCH-LIST BREAKS))

))

; From file COMS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMS  "


(DEFUN QUERY-REPLACE-SEARCH-LIST (BP TO-BP FROM-LIST TO-LIST &AUX TEM)
  (DECLARE (SPECIAL *BP* *STATE*))
  (OR (BP-= BP *BP*) (SETQ *STATE* 0))		;If bp has moved, reset state
  (MULTIPLE-VALUE (*BP* TEM *STATE*)
    (FSM-SEARCH BP FROM-LIST NIL NIL NIL TO-BP *STATE*))
  (COND (*BP*
	 (SETQ *QUERY-FROM* TEM
	       *QUERY-TO* (NTH (FIND-POSITION-IN-LIST TEM FROM-LIST) TO-LIST))
	 (VALUES *BP* (FORWARD-CHAR *BP* (- (STRING-LENGTH TEM)))))))

))

; From file MAIL.LISP PS:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "


(DEFCOM COM-CHANGE-SUBJECT-PRONOUNS "Correct pronouns the subject field (me <-> you)." ()
  (ADD-HEADER-FIELD ':SUBJECT NIL NIL)
  (LET ((*INTERVAL* (CREATE-INTERVAL (COPY-BP (POINT) ':NORMAL)
				     (COPY-BP (BEG-LINE (POINT) 1 T) ':MOVES))))
    (QUERY-REPLACE-LIST (POINT) (INTERVAL-LAST-BP *INTERVAL*)
			*SUBJECT-PRONOUN-FROM-LIST* *SUBJECT-PRONOUN-TO-LIST* T))
  DIS-TEXT)

))

; From file SECTIO.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "


(DEFUN CONTINUE-TAGS-QUERY-REPLACE (RESTART)
  (DO ((BEGINNING RESTART)
       (VAL))
      (NIL)
    ;; Find the next buffer in the list which has an occurrence, select it.
    (DO (BP FOUND-AT) (())
      (SETQ BP (NEXT-FILE-BP BEGINNING))
      (SETQ BEGINNING NIL)
      (WHEN (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
	      (SETQ FOUND-AT (SEARCH BP *TAGS-QUERY-REPLACE-FROM*)))
	(POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
	(MAKE-BUFFER-CURRENT (BP-TOP-LEVEL-NODE BP))
	;; Move bp to just before the occurrence, so we avoid
	;; re-scanning the part of the buffer already searched over.
	(MOVE-BP (POINT) (FORWARD-CHAR FOUND-AT (- (LENGTH *TAGS-QUERY-REPLACE-FROM*))))
	(RETURN)))
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)
    ;; Query replace thru that buffer.
    (SETQ VAL (QUERY-REPLACE (POINT) (INTERVAL-LAST-BP *INTERVAL*)
			     *TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*
			     *TAGS-QUERY-REPLACE-DELIMITED*))
    (AND (EQ VAL 'ABORTED) (RETURN DIS-TEXT))))

))

; From file SECTIO.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(DEFCOM COM-MULTIPLE-QUERY-REPLACE-FROM-BUFFER
	"Perform a multiple query replace from the contents of the specified buffer" ()
  (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
    (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
					*ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
	  FROM-LIST TO-LIST)
      (MULTIPLE-VALUE (FROM-LIST TO-LIST)
	(PARSE-BUFFER-REPLACE-PAIRS T))
      (QUERY-REPLACE-LIST (POINT) (INTERVAL-LAST-BP *INTERVAL*)
			  FROM-LIST TO-LIST *NUMERIC-ARG-P*)))
  DIS-TEXT)

))

; From file SECTIO.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "


(DEFUN CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE (RESTART)
  (DO ((BEGINNING RESTART)
       (*MODE-WORD-SYNTAX-TABLE* (IF (AND *TAGS-QUERY-REPLACE-DELIMITED*
					  (MINUSP *TAGS-QUERY-REPLACE-DELIMITED*))
				*ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
       (VAL))
      (NIL)
    ;; Find and select the next buffer that has an occurrence of any of the strings.
    (DO (BP) (())
      (SETQ BP (NEXT-FILE-BP BEGINNING))
      (SETQ BEGINNING NIL)
      (WHEN (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
	      (DOLIST (STR *TAGS-QUERY-REPLACE-FROM*)
		(IF (SEARCH BP STR) (RETURN T))))
	(POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
	(MAKE-BUFFER-CURRENT (BP-TOP-LEVEL-NODE BP))
	(MOVE-BP (POINT) BP)
	(RETURN)))
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)
    ;; Query replace thru that buffer.
    (SETQ VAL (QUERY-REPLACE-LIST (POINT) (INTERVAL-LAST-BP *INTERVAL*)
				  *TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*
				  *TAGS-QUERY-REPLACE-DELIMITED*))
    (AND (EQ VAL 'ABORTED) (RETURN DIS-TEXT))))

))

; From file ISPELL.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; ISPELL  "


(DEFUN REPLACE-WORD-WITH (WORD REPLACEMENT &OPTIONAL QUERY-REPLACE-P BP1 BP2)
  "Replace WORD located between BP1 and BP2 with REPLACEMENT.
If QUERY-REPLACE-P, than query replace whole buffer as well."
  (PREPARE-WINDOW-FOR-REDISPLAY *WINDOW*)
;;foe bar baz for boo too foo fox foo fi fo
  (IF (NULL BP1)
      (SETQ BP1 (CURRENT-WORD-BP)))
  (IF (NULL BP2)
      (SETQ BP2 (FORWARD-WORD BP1 1 T)))
  (LET ((*CASE-REPLACE-P* T)
	(REPLACEMENT (STRING-DOWNCASE REPLACEMENT))
	(WORD (STRING-DOWNCASE WORD)))  ;for case-replace
    (WITH-UNDO-SAVE ("Spelling correction" BP1 BP2 T)
      (SETQ BP2 (CASE-REPLACE BP1 BP2 REPLACEMENT))
      (MOVE-BP (POINT) BP2))
    (WHEN QUERY-REPLACE-P
      (POINT-PDL-PUSH (COPY-BP (POINT)) *WINDOW*)
      (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*))
      (QUERY-REPLACE (POINT) (INTERVAL-LAST-BP *INTERVAL*)
		     (STRING-DOWNCASE WORD) (STRING-DOWNCASE REPLACEMENT)))))

;for implementing meta-x  Correct spelling
;; must get a bunch of words with theory of com-meta-$ and stuff into
;; chaos packets quickly (we're limited by the size of the packet.

;auxiliary functions for parsing words and spaces into lists and vice versa.


))

; From file COMF.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(DEFCOM COM-QUERY-REPLACE-LET-BINDING "Replace variable of LET with its value.
Point must be after or within the binding to be modified." ()
  (ATOM-WORD-SYNTAX-BIND
   (LET ((POINT (POINT))
	LET-BP BINDING-BP BP1 BP2 FROM TO)
    (OR (SETQ LET-BP (FIND-CONTAINING-ATOM POINT '(LET))) (BARF))
    (DO ((BP (FORWARD-LIST LET-BP 1 NIL -1 T) NBP)
	 (NBP))
	(NIL)
      (OR (SETQ NBP (FORWARD-SEXP BP 1 NIL 0 NIL NIL T)) (BARF))
      (OR (BP-< NBP POINT) (RETURN (SETQ BINDING-BP BP))))
    (SETQ BP1 (FORWARD-LIST BINDING-BP 1 NIL -1 T)
	  BP2 (FORWARD-SEXP BP1)
	  FROM (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-OVER *WHITESPACE-CHARS* BP2)
	  BP2 (FORWARD-SEXP BP1)
	  TO (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-SEXP LET-BP 2)
	  BP2 (FORWARD-SEXP BP1 1 NIL 1))
    (OR *NUMERIC-ARG-P* (PSETQ FROM TO TO FROM))
    (MOVE-BP POINT BP1)
    (LET ((*INTERVAL* (CREATE-INTERVAL BP1 BP2 T)))
      (QUERY-REPLACE POINT (INTERVAL-LAST-BP *INTERVAL*) FROM TO T))))
  DIS-TEXT)

))

; From file COMF.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "


(DEFCOM COM-QUERY-REPLACE-LAST-KILL "Replace top of kill ring with region." ()
  (LET ((POINT (POINT)) (MARK (MARK)))
    (QUERY-REPLACE POINT (INTERVAL-LAST-BP *INTERVAL*)
		   (STRING-INTERVAL (HISTORY-LATEST-ELEMENT *KILL-HISTORY*))
		   (STRING-INTERVAL MARK POINT)))
  DIS-TEXT)


))

; From file SCRED.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRED  "

(DEFUN SCREEN-EDITOR-EDIT-ATTRIBUTES (WINDOW)
  (LET ((BORDERS-P (SEND WINDOW ':OPERATION-HANDLED-P ':SET-BORDERS))
	(LABEL-P (SEND WINDOW ':OPERATION-HANDLED-P ':SET-LABEL))
	(NAME-P (SEND WINDOW ':OPERATION-HANDLED-P ':SET-NAME)))
    (LET* ((CURRENT-FONT-VALUE (FUNCALL WINDOW ':CURRENT-FONT))
	   (OLD-CURRENT-FONT-VALUE CURRENT-FONT-VALUE)
	   (MORE-P-VALUE (FUNCALL WINDOW ':MORE-P))
	   (OLD-MORE-P-VALUE MORE-P-VALUE)
	   (REVERSE-VIDEO-P (FUNCALL WINDOW ':REVERSE-VIDEO-P))
	   (OLD-REVERSE-VIDEO-P REVERSE-VIDEO-P)
	   (VSP (FUNCALL WINDOW ':VSP))
	   (OLD-VSP VSP)
	   (IN-ACTION (FUNCALL WINDOW ':DEEXPOSED-TYPEIN-ACTION))
	   (OLD-IN-ACTION IN-ACTION)
	   (OUT-ACTION (FUNCALL WINDOW ':DEEXPOSED-TYPEOUT-ACTION))
	   (OLD-OUT-ACTION OUT-ACTION)
	   (OTHER-OUT-ACTION)
	   (OLD-OTHER-OUT-ACTION)
	   (CHAR-ALU-FCN (FUNCALL WINDOW ':CHAR-ALUF))
	   (OLD-CHAR-ALU-FCN CHAR-ALU-FCN)
	   (ERASE-ALU-FCN (FUNCALL WINDOW ':ERASE-ALUF))
	   (OLD-ERASE-ALU-FCN ERASE-ALU-FCN)
	   (PRIORITY-VALUE (FUNCALL WINDOW ':PRIORITY))
	   (OLD-PRIORITY-VALUE PRIORITY-VALUE)
	   (SAVE-BITS-VALUE (FUNCALL WINDOW ':SAVE-BITS))
	   (OLD-SAVE-BITS-VALUE SAVE-BITS-VALUE)
	   (LABEL-OR-NAME (COND (NAME-P (FUNCALL WINDOW ':NAME))
				(LABEL-P (FUNCALL WINDOW ':LABEL))))
	   (OLD-LABEL-OR-NAME LABEL-OR-NAME)
	   (BORDERS-SPEC (IF BORDERS-P (FUNCALL WINDOW ':BORDERS)))
	   (OLD-BORDERS-SPEC BORDERS-SPEC)
	   (BORDER-MARGIN-WIDTH-VALUE (IF BORDERS-P (FUNCALL WINDOW ':BORDER-MARGIN-WIDTH)))
	   (OLD-BORDER-MARGIN-WIDTH-VALUE BORDER-MARGIN-WIDTH-VALUE))
      (DECLARE (SPECIAL BORDER-MARGIN-WIDTH-VALUE CURRENT-FONT-VALUE MORE-P-VALUE 
			REVERSE-VIDEO-P BORDERS-SPEC LABEL-OR-NAME VSP IN-ACTION
			CHAR-ALU-FCN ERASE-ALU-FCN PRIORITY-VALUE SAVE-BITS-VALUE
			OUT-ACTION OTHER-OUT-ACTION))
      (IF (CONSP BORDERS-SPEC)		;********************
	  (IF (MEMQ (CAR BORDERS-SPEC) '(NIL :ZERO))
	      (SETQ BORDERS-SPEC 0)
	    (SETQ BORDERS-SPEC (- (FOURTH (FIRST BORDERS-SPEC))
				  (SECOND (FIRST BORDERS-SPEC))))))
      (IF (CONSP LABEL-OR-NAME)
	  (SETQ LABEL-OR-NAME (SIXTH LABEL-OR-NAME)))
      (COND ((NOT (MEMQ OUT-ACTION '(:NORMAL :NOTIFY :PERMIT :ERROR)))
	     (SETQ OTHER-OUT-ACTION OUT-ACTION
		   OLD-OTHER-OUT-ACTION OTHER-OUT-ACTION
		   OUT-ACTION ':OTHER
		   OLD-OUT-ACTION OUT-ACTION)))
      (MULTIPLE-VALUE-BIND (NIL ABORT-P)
	  (*CATCH 'ABORT-EDIT
	    (CHOOSE-VARIABLE-VALUES
	     (MAKE-ATTRIBUTES-LIST WINDOW BORDERS-P LABEL-P NAME-P)
	     ':LABEL (FORMAT NIL "Edit window attributes of ~A." WINDOW)
	     ':MARGIN-CHOICES '("Done" ("Abort" (*THROW 'ABORT-EDIT NIL)))
	     ':FUNCTION 'ATTRIBUTE-EDITOR-HOOK))
	(COND (ABORT-P
	       (BEEP))
	      (T
	       (IF (NEQ CURRENT-FONT-VALUE OLD-CURRENT-FONT-VALUE)
		   (FUNCALL WINDOW ':SET-CURRENT-FONT CURRENT-FONT-VALUE))
	       (IF (NEQ MORE-P-VALUE OLD-MORE-P-VALUE)
		   (FUNCALL WINDOW ':SET-MORE-P MORE-P-VALUE))
	       (IF (NEQ REVERSE-VIDEO-P OLD-REVERSE-VIDEO-P)
		   (FUNCALL WINDOW ':SET-REVERSE-VIDEO-P REVERSE-VIDEO-P))
	       (IF (NEQ VSP OLD-VSP)
		   (FUNCALL WINDOW ':SET-VSP VSP))
	       (IF (NEQ IN-ACTION OLD-IN-ACTION)
		   (FUNCALL WINDOW ':SET-DEEXPOSED-TYPEIN-ACTION IN-ACTION))
	       (COND ((NEQ OUT-ACTION OLD-OUT-ACTION)
		      (FUNCALL WINDOW ':SET-DEEXPOSED-TYPEOUT-ACTION
			       (IF (EQ OUT-ACTION ':OTHER)
				   OTHER-OUT-ACTION
				   OUT-ACTION)))
		     ((AND (EQ OUT-ACTION ':OTHER)
			   (NEQ OTHER-OUT-ACTION OLD-OTHER-OUT-ACTION))
		      (FUNCALL WINDOW ':SET-DEEXPOSED-TYPEOUT-ACTION OTHER-OUT-ACTION)))
	       (IF (NEQ CHAR-ALU-FCN OLD-CHAR-ALU-FCN)
		   (FUNCALL WINDOW ':SET-CHAR-ALUF CHAR-ALU-FCN))
	       (IF (NEQ ERASE-ALU-FCN OLD-ERASE-ALU-FCN)
		   (FUNCALL WINDOW ':SET-ERASE-ALUF ERASE-ALU-FCN))
	       (IF (NEQ PRIORITY-VALUE OLD-PRIORITY-VALUE)
		   (FUNCALL WINDOW ':SET-PRIORITY PRIORITY-VALUE))
	       (IF (NEQ SAVE-BITS-VALUE OLD-SAVE-BITS-VALUE)
		   (FUNCALL WINDOW ':SET-SAVE-BITS SAVE-BITS-VALUE))
	       (IF (NEQ LABEL-OR-NAME OLD-LABEL-OR-NAME)
		   (COND (NAME-P (SEND WINDOW :SET-NAME LABEL-OR-NAME))
			 (LABEL-P (SETF (SIXTH OLD-LABEL-OR-NAME) LABEL-OR-NAME)
				  (SEND WINDOW :SET-LABEL OLD-LABEL-OR-NAME))))
	       (COND (BORDERS-P
		      (IF (NEQ BORDERS-SPEC OLD-BORDERS-SPEC)
			  (FUNCALL WINDOW ':SET-BORDERS BORDERS-SPEC))
		      (IF (NEQ BORDER-MARGIN-WIDTH-VALUE OLD-BORDER-MARGIN-WIDTH-VALUE)
			  (FUNCALL WINDOW ':SET-BORDER-MARGIN-WIDTH
				   BORDER-MARGIN-WIDTH-VALUE))
		      ))))))))

))

; From file EHW.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHW  "

(DEFMETHOD (ERROR-HANDLER-FRAME :SETUP-FRAME) (SG AP &OPTIONAL FORCE-P ARG-CHANGED-FLAG
						     &AUX CODE ARGS LOCALS TEM)
  (OR TV:EXPOSED-P
      ;; If window not exposed, get its bit array in core so setup will go faster
      (SI:PAGE-IN-ARRAY TV:SCREEN-ARRAY))
  (SETQ TEM (ASSQ AP FRAME-ALIST))
  (COND (FORCE-P
	 (SETQ FRAME-ALIST (DELQ TEM FRAME-ALIST))
	 (SETQ TEM NIL)))
  ;; Set stuff up in most interesting order: args, then locals, then code
  (COND (TEM					;Displayed this before
	 (SEND ARGS-WINDOW ':SETUP (THIRD TEM))
	 (SEND LOCALS-WINDOW ':SETUP (FOURTH TEM))
	 (SEND INSPECT-HISTORY-WINDOW ':INSPECT-OBJECT (SECOND TEM) INSPECT-WINDOW))
	(T
	 (MULTIPLE-VALUE (ARGS TEM) (SETUP-ARGS-WINDOW ARGS-WINDOW SG AP))
	 (SETQ LOCALS (SETUP-LOCALS-WINDOW LOCALS-WINDOW SG AP TEM))
	 (SETQ CODE (SETUP-INSPECT-WINDOW INSPECT-WINDOW SG AP INSPECT-HISTORY-WINDOW))
	 (PUSH (LIST AP CODE ARGS LOCALS) FRAME-ALIST)))
  (IF ARG-CHANGED-FLAG
      (SEND STACK-WINDOW ':REFRESH))
  (SEND STACK-WINDOW ':PUT-ITEM-IN-WINDOW AP)
  (SEND STACK-WINDOW ':SET-CURRENT-ITEM AP))

))

; From file EHW.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHW  "

(DEFUN COMW-SET-ARG (SG IGNORE &AUX CHAR)
  (FORMAT T "~&Mouse an argument or local to modify:~%")
  (LET ((OLD-SI-TYPES (SEND ERROR-HANDLER-WINDOW ':SENSITIVE-ITEM-TYPES)))
    (SEND ERROR-HANDLER-WINDOW ':SET-SENSITIVE-ITEM-TYPES '(ARG LOCAL))
    (UNWIND-PROTECT
      (SETQ CHAR (SEND *STANDARD-INPUT* ':ANY-TYI))
      (SEND ERROR-HANDLER-WINDOW ':SET-SENSITIVE-ITEM-TYPES OLD-SI-TYPES)))
  (IF (NOT (AND (CONSP CHAR) (MEMQ (CAR CHAR) '(LOCAL ARG))))
      (FORMAT T "~&That is not an argument or local~%")
    (LET ((IDX (CADADR CHAR)))
      (IF (NOT (NUMBERP IDX))
	  (FORMAT T "~&Cannot set rest arg.")
	(LET ((NEW-OBJ (READ-OBJECT ':EVAL-READ
				    (FORMAT NIL "Value to substitute for ~A: "
					    (CAADR CHAR)))))
	  (LET ((RP (SG-REGULAR-PDL SG)))
	    (ASET NEW-OBJ RP
		  (+ CURRENT-FRAME IDX (IF (EQ (CAR CHAR) 'ARG) 1
					 (RP-LOCAL-BLOCK-ORIGIN RP CURRENT-FRAME))))))))
    (SEND ERROR-HANDLER-WINDOW ':SETUP-FRAME SG CURRENT-FRAME T T)))

))

; From file SRCCOM.LISP PS:<L.IO1> OZ:
#8R SRCCOM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SRCCOM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SRCCOM  "

(DEFUN QUERY-TYPE ()
   "Ask user for the type of compare to do.  Returns value suitable for SOURCE-COMPARE-FILES.
But currently always returns :TEXT because the other mode is useless
and the question is a pain in the neck."
  :TEXT)

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun type-pretty-name (type)
  "Return a string containing a noun phrase describing objects of type TYPE."
  (unless (variable-boundp type-pretty-name-hash-table)
    (setq type-pretty-name-hash-table (make-hash-table :test 'equal :size 500.)))
  (or (gethash type type-pretty-name-hash-table)
      (let ((default-cons-area background-cons-area))
	;; Prevent lossage if TYPE was consed in a temporary area.
	(setq type (copytree type))
	(setf (gethash type type-pretty-name-hash-table)
	      (cond ((symbolp type)
		     (or (get type 'type-name)
			 (string-append-a-or-an
			   (string-subst-char #/space #/-
					      (string-downcase (format nil "~a" type)) nil))))
		    ((and (consp type)
			  (funcall (get (car type) 'type-name-function #'ignore) type)))
		    (t (string-append (format nil "an object of type ~S" type))))))))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defmacro advised-function (before after around inner-function-expression)
 "Expands into the code that executes advice in the proper order."
 (let ((default-cons-area background-cons-area))
  `(advise-prog (values)
		(declare (special values))
     (advise-setq values
		  (advise-multiple-value-list
		    (advise-progn
		      ,@before
		      ,(advise-merge-arounds around inner-function-expression))))
     ,@after
     (advise-return-list values))))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defun grind-print-advice-slot (slot-contents slot-name function width real-io untyo-p)
  (do ((l slot-contents (cdr l)) (i 0 (1+ i)))  ((null l))
    (grind-top-level `(advise ,function ,slot-name ,(cadr (cadar l)) ,i . ,(cddar l))
		     width real-io untyo-p)))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defun advise-update-list (slot-location name position forms)
  (let* ((default-cons-area background-cons-area)
	 preceding (new-unit `(progn ',name . ,forms)))
    (cond ((numberp position)
	   (or (setq preceding (nthcdr position (locf (car slot-location))))
	       (progn (setq preceding (locf (car slot-location)))
		      (do () ((null (cdr preceding)))
			(pop preceding)))))
	  ((and (null name) (null position)))
	  ((or (symbolp position) (null position))
	   (setq preceding (mem #'(lambda (p elt) (eq p (cadadr elt)))
				(or position name) (locf (car slot-location))))))
    ;; If the symbol isn't found, or no position is specified,
    ;; insert new advice at the beginning.
    (or preceding (setq preceding (locf (car slot-location))))
    (push new-unit (cdr preceding))
    ;; If we have a name, delete any old advice with that name.
    (and name
	 (do ((l (locf (car slot-location)) (cdr l))) ((null l))
	   (and (eq (cadadr (cadr l)) name)
		(neq (cadr l) new-unit)
		(return (rplacd l (cddr l))))))
    nil))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

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
	     (if (consp slot-location)
		 (setf (car slot-location) nil)))
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

; From file ENCAPS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ENCAPS  "

(defun encapsulation-body (encapsulation)
  "Given a function made using SI:ENCAPSULATION,
return the object supplied by SI:ENCAPSULATION's caller as the body."
  (if (eq (car encapsulation) 'macro)
      (encapsulation-body (cdr encapsulation))
    (cdddr (cadddr encapsulation))))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defun breakon (&optional function-spec (condition t))
  "Break on entry to FUNCTION-SPEC, if CONDITION evaluates non-NIL.
If called repeatedly for one function-spec with different conditions,
a break will happen if any of the conditions evaluates non-NIL.

With no args, returns a list of function specs that have had
break-on-entry requested with BREAKON."
  (if (null function-spec)
      breakon-functions
    (setq function-spec (dwimify-arg-package function-spec 'function-spec))
    (breakon-init function-spec)
    (setq condition (si:rename-within-new-definition-maybe function-spec condition))
    (let* ((spec1 (si:unencapsulate-function-spec function-spec 'breakon)))
      (uncompile spec1 t)
      (let* ((def (fdefinition spec1))
	     (default-cons-area background-cons-area)
	     ;; Find our BREAKON-THIS-TIME.
	     ;; def looks like:
	     ;;   (named-lambda (foo debugging-info) arglist
	     ;;	    (si:encapsulation-let ((arglist (si:encapsulation-list* arglist)))
	     ;;	       (declare (special arglist))
	     ;;        (breakon-this-time conditions unencapsulated-function arglist)))
	     (defn-data (car (si:encapsulation-body def)))
	     (slot-loc (cadr defn-data)))	;Within that, find ptr to list of conditions.
	(or (member condition (cdr slot-loc)) (push condition (cdr slot-loc)))))
    (if compile-encapsulations-flag
	(compile-encapsulations function-spec 'breakon))
    function-spec))

))

; From file EHC.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHC  "

(defun unbreakon (&optional function-spec (condition t))
  "Remove break on entry to FUNCTION-SPEC, or all functions if no arg.
If CONDITION is specified, we remove only that condition for breaking;
if other conditions have been specified with BREAKON on this function,
the other conditions remain in effect."
  (and function-spec
       (setq function-spec (dwimify-arg-package function-spec 'function-spec)))
  (let* ((spec1 (and function-spec (si:unencapsulate-function-spec function-spec 'breakon))))
    (cond ((null function-spec)
	   (mapc 'unbreakon breakon-functions))
	  ((eq condition t)
	   (fdefine spec1 (fdefinition (si:unencapsulate-function-spec spec1 '(breakon))))
	   (setq breakon-functions (delete function-spec breakon-functions))
	   function-spec)
	  ((neq spec1 (si:unencapsulate-function-spec spec1 '(breakon)))
	   (uncompile spec1 t)
	   (let* ((def (fdefinition spec1))
		  ;; Find our BREAKON-NEXT-TIME.
		  ;; def looks like:
		  ;;   (named-lambda (foo debugging-info) arglist
		  ;;	    (si:encapsulation-let ((arglist (si:encapsulation-list* arglist)))
		  ;;	       (declare (special arglist))
		  ;;        (breakon-this-time conditions unencapsulated-function arglist)))
		  (defn-data (car (si:encapsulation-body def)))
		  (slot-loc (cadr defn-data)))	;Within that, find ptr to list of conditions.
	     (setf (cdr slot-loc)
		   (delete condition (cdr slot-loc)))
	     (cond ((null (cdr slot-loc))
		    (fdefine spec1
			     (fdefinition (si:unencapsulate-function-spec spec1 '(breakon))))
		    (setq breakon-functions (delete function-spec breakon-functions)))
		   (compile-encapsulations-flag
		    (compile-encapsulations function-spec 'breakon))))
	   function-spec))))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "


(defun (advise encapsulation-grind-function) (function def width real-io untyo-p)
  (when def	; Print the advice as calls to advise.
    (if (typep def 'compiled-function)
	(setq def (cadr (assq ':interpreted-definition (debugging-info def)))))
    (let ((body (encapsulation-body def)))
      (when (eq (car (car body)) 'si:displaced)
	(setf (car body) (cadr (car body))))
      (grind-print-advice-slot (cadr (car body)) ':before
			       function width real-io untyo-p)
      (grind-print-advice-slot (caddr (car body)) ':after
			       function width real-io untyo-p)
      (grind-print-advice-slot (cadddr (car body)) ':around
			       function width real-io untyo-p))))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "

(defun advise-init (function-spec)
  (let ((default-cons-area background-cons-area)
	(spec1 (unencapsulate-function-spec function-spec 'advise)))
    (cond ((neq spec1 (unencapsulate-function-spec spec1 '(advise)))
	   (uncompile spec1 t)
	   (let ((body (encapsulation-body (fdefinition spec1))))
	     ;; (car body) looks like:
	     ;;        (advised-function nil nil nil encapsulated-function)
	     (when (eq (car (car body)) 'si:displaced)
	       (setf (car body) (cadr (car body))))))
	  (t
	   (push function-spec advised-functions)
	   (encapsulate spec1 function-spec 'advise
			`(advised-function nil nil nil ,encapsulated-function))))))

))

; From file ADVISE.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; ADVISE  "


(defun advise-find-slot (function-spec class &aux body)
  (uncompile function-spec t)
  (setq body (encapsulation-body (fdefinition function-spec)))
  (nthcdr (cond ((string= class "BEFORE") 1)
		((string= class "AFTER") 2)
		((string= class "AROUND") 3)
		(t (ferror nil "Second argument is ~s, neither BEFORE, AFTER nor AROUND" class)))
	  (car body)))

))
