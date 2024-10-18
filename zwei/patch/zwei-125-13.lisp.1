;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.13
;;; Reason:
;;;  Remove the *FASD-INTERFACE* argument from the various remaining callers. --RWK
;;; Written 8-Aug-88 19:19:26 by smh (Steve Haflich) at site Gigamos Cambridge
;;; while running on Harpo from band 3
;;; with Experimental System 126.18, ZWEI 125.12, ZMail 73.2, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.12, SDU ROM 102, kold 4aug88.



; From modified file DJ: L.ZWEI; COMC.LISP#226 at 8-Aug-88 19:19:30
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-DEFUN-INTERNAL (COMPILE-P MODE-NAME ECHO-NAME
			       &OPTIONAL USE-TYPEOUT DEFVAR-HACK
			       (COMPILER-PROCESSING-MODE
				 '(:MODE COMPILER:MACRO-COMPILE))
			       (*target-computer* 'compiler:lambda-interface)
			       &AUX BP1 BP2 DEFUN-NAME)
  "Compile or evaluate a part of the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
If there is a region, it is used; otherwise the current or following defun is used.
USE-TYPEOUT is passed to COMPILE-PRINT-INTERVAL and controls where information is printed.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (COND ((WINDOW-MARK-P *WINDOW*)
	 (SETQ BP1 (MARK) BP2 (POINT))
	 (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	 (SETQ DEFUN-NAME "Region"))
	((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
	 (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	 (SETQ DEFVAR-HACK T))
	(T
	 (BARF "Cannot find a defun near point.")))
  (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P
			  DEFUN-NAME MODE-NAME ECHO-NAME USE-TYPEOUT DEFVAR-HACK
			  COMPILER-PROCESSING-MODE
			  nil		;already-resectionized-flag
			  *target-computer*))


))

; From modified file DJ: L.ZWEI; COMC.LISP#226 at 8-Aug-88 19:19:38
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "


(DEFUN COMPILE-BUFFER (COMPILE-P MODE-NAME ECHO-NAME
		       &OPTIONAL (COMPILER-PROCESSING-MODE
				   '(:MODE COMPILER:MACRO-COMPILE))
				 (*target-computer* 'compiler:lambda-interface)
		       &AUX BP1 BP2 NAME)
  "Compile or evaluate the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
COMPILE-PROCESSING-MODE is a keyword list.  The :MODE component should be either
 COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (IF *NUMERIC-ARG-P*
      (SETQ BP1 (POINT) BP2 (INTERVAL-LAST-BP *INTERVAL*) NAME "Rest of buffer")
      (SETQ BP1 *INTERVAL* NAME "Buffer"))
  (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P NAME MODE-NAME ECHO-NAME
			  NIL			;USE-TYPEOUT
			  NIL			;DEFVAR-HACK
			  COMPILER-PROCESSING-MODE
			  nil			;already-resectionized-flag
			  *target-computer*)
  DIS-NONE)

))

; From modified file DJ: L.ZWEI; COMC.LISP#226 at 8-Aug-88 19:19:42
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-PRINT-INTERVAL (BP1 BP2 IN-ORDER-P COMPILE-P REGION-NAME MODE-NAME ECHO-NAME
			       &OPTIONAL USE-TYPEOUT DEFVAR-HACK
			       COMPILER-PROCESSING-MODE
			       ALREADY-RESECTIONIZED-FLAG
			       &optional (*target-computer* 'compiler:lambda-interface)
			       &AUX FORMAT-FUNCTION SUCCESS)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
REGION-NAME is a string to print as the name of this whole object,
 or NIL to mention each object's name.
USE-TYPEOUT can be T, NIL, :TYPEOUT or :PROMPT.
  T prints form values and names of objects in typeout window.
 Otherwise, form values appear in the echo area, and 
  :TYPEOUT prints names of objects in typeout window.
  :PROMPT prints names of objects in prompt line.
  NIL prints names of objects in the echo area.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is a keyword list selecting compiler options.  For compatibility,
 a symbol such as COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE is converted to
 (:mode <symbol>)
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowercase past participle and period (/"compiled./")."
  (cond ((null compiler-processing-mode)
	 (setq compiler-processing-mode '(:mode compiler:macro-compile)))
	((symbolp compiler-processing-mode)
	 (setq compiler-processing-mode `(:mode ,compiler-processing-mode))))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS ALREADY-RESECTIONIZED-FLAG
    (CHECK-INTERVAL-SECTIONS BP1 BP2 T))
  (UNDO-SAVE-CURRENT-RANGE)
  (SETQ FORMAT-FUNCTION (CASE USE-TYPEOUT
			  ((T :TYPEOUT) #'(LAMBDA (STRING &REST ARGS)
					    (APPLY 'FORMAT T STRING ARGS)))
			  (:PROMPT #'PROMPT-LINE-MORE)
			  (OTHERWISE #'(LAMBDA (STRING &REST ARGS)
					(APPLY 'FORMAT *QUERY-IO* STRING ARGS)))))
  (IF REGION-NAME
      (FUNCALL FORMAT-FUNCTION "~&~A ~A" MODE-NAME REGION-NAME)
    (FUNCALL FORMAT-FUNCTION "~&~A ~S" MODE-NAME (function-name-from-bp BP1)))
  (UNWIND-PROTECT
    (PROGN
      (COMPILE-INTERVAL COMPILE-P
			(CASE USE-TYPEOUT
			  ((T) T)
			  (T *QUERY-IO*))
			DEFVAR-HACK BP1 BP2 T
			COMPILER-PROCESSING-MODE
			*target-computer*)
      (SETQ SUCCESS T))
    (OR SUCCESS
	(FUNCALL FORMAT-FUNCTION " -- aborted.")))
  (FUNCALL FORMAT-FUNCTION " -- ~A" ECHO-NAME)
  (UPDATE-INTERVAL-COMPILE-TICK BP1 BP2 T))

;; Copied from LAD: RELEASE-3.ZWEI; COMC.LISP#216 on 26-Mar-87 17:35:19

))

; From modified file DJ: L.ZWEI; COMC.LISP#226 at 8-Aug-88 19:19:47
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMC  "

(DEFUN COMPILE-INTERVAL (COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK
			 BP1 BP2 IN-ORDER-P
			 COMPILE-PROCESSING-MODE
			 &optional (*target-computer* 'compiler:lambda-interface)
			 &AUX GENERIC-PATHNAME STREAM
			      WHOLE-FILE   ;T if processing the entire file.
			      SI:FDEFINE-FILE-DEFINITIONS)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
Does not print any sort of message saying what is being compiled,
does not know about sectionization.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
PRINT-RESULTS-STREAM is a stream for printing the results of evaluation, or NIL not to print.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is a keyword list.  The :MODE item should be either
 COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization."
  (DECLARE (SPECIAL COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK COMPILE-PROCESSING-MODE))
  (SETQ GENERIC-PATHNAME (SEND *INTERVAL* :GENERIC-PATHNAME))
  ;; Does not reparse the mode line; we should let the user decide whether to do that.!
  ;; Should not override the user's Set Package if he has done one.
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  ;; Decide whether the entire file is being processed or just a part.
  ;; If the whole file, we want to notice if any function present in the file previously
  ;; is now missing.  If just a part, anything we don't notice now we must assume
  ;; is elsewhere in the file.
  (SETQ WHOLE-FILE
	(AND (BP-= BP1 (INTERVAL-FIRST-BP *INTERVAL*))
	     (BP-= BP2 (INTERVAL-LAST-BP *INTERVAL*))))
  (SETQ STREAM (INTERVAL-STREAM BP1 BP2 T))
  ;; Arrange for first read-error's location to be saved in q-reg ".".
  (REMPROP (MAKE-REGISTER-NAME #/.) 'POINT)
  (LET ((SI:*ALL-FREE-INTERPRETER-VARIABLE-REFERENCES-SPECIAL* T))
    (MULTIPLE-VALUE-BIND (VARS VALS) (SEND *INTERVAL* :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
	(let ((compile-in-roots-prop (get *interval* :compile-in-roots)))
	  (cond ((and (eq compile-p t)
		      compile-in-roots-prop
		      (not (cl:member (si:package-root-name *package*)
				      compile-in-roots-prop
				      :test 'string-equal)))
		 (cond ((not (= 1 (length compile-in-roots-prop)))
			(fsignal "The current heirarchy ~S is not among those acceptable ~s."
				 (si:package-root-name *package*)
				 compile-in-roots-prop))
		       (t (format print-results-stream
				  "  Transferring to hierarchy ~s" (car compile-in-roots-prop))
			  (pkg-goto (si:pkg-name *package*) nil
				    (pkg-find-package (car compile-in-roots-prop))))))))
	(WHEN FS:THIS-IS-A-PATCH-FILE
	  ;; If compiling out of the editor buffer of a patch file,
	  ;; make sure the file itself is marked
	  ;; so that Meta-. will behave right.
	  (PUTPROP GENERIC-PATHNAME T :PATCH-FILE))
	;; Bind off this flag -- our stream is not generating font changes
	;; so READ should not try to remove any.
	(LET ((SI:READ-DISCARD-FONT-CHANGES NIL))
	  (FLET ((DO-IT ()
			(COMPILER:COMPILE-STREAM
			  STREAM
			  GENERIC-PATHNAME
			  NIL			;FASD-FLAG
			  (IF (AND COMPILE-P (NOT (EQ COMPILE-P T)))
		;if using user supplied evaluator, avoid any possible macro-expanding, etc
		; in COMPILE-DRIVER.
			      'SIMPLE-COMPILE-INTERVAL-PROCESS-FN
			    'COMPILE-INTERVAL-PROCESS-FN)
			  T			;QC-FILE-LOAD-FLAG
			  NIL			;QC-FILE-IN-CORE-FLAG
			  *PACKAGE*
			  NIL			;FILE-LOCAL-DECLARATIONS
			  NIL			;Unused
			  WHOLE-FILE
			  *target-computer*)))
	    (IF COMPILE-P
		(COMPILER:LOCKING-RESOURCES-NO-QFASL (DO-IT))
	      (DO-IT)))))))
  (OR (NULL GENERIC-PATHNAME)
      (SI:RECORD-FILE-DEFINITIONS GENERIC-PATHNAME SI:FDEFINE-FILE-DEFINITIONS WHOLE-FILE)))

))
