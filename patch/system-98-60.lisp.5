;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 6/05/84 03:01:55 by Mly,
;;; Reason: Miscompilation of declarations for lambda-lists which have both &key and &aux
;;; while running on Lisp Machine One from band 6
;;; with System 98.49, CADR 3.6, ZMail 53.17, MIT-Specific 22.1, microcode 309.



; From file QCP1.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	PSEUDO-KEYNAMES DECLS)
    (IF (EQ (CAR LAMBDA-EXP) 'LAMBDA)
	(SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP))
      (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP)))	;named-lambda
    (MULTIPLE-VALUE (POSITIONAL-ARGS NIL AUXVARS
		     REST-ARG POSITIONAL-ARG-NAMES
		     KEYKEYS KEYNAMES NIL KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
    (multiple-value (nil decls) (extract-declarations body nil nil))
    (do ((d decls (cdr d)))
	((null d))
      (setf (car d) `(declare ,(car d))))
    ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
    ;; and check explicitly whether that has been overridden.
    ;; If the arg is optional
    ;; and the initial value is a constant, we can really init it to that.
    ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
    ;; after all keywords are decoded, we bind the intended variable, in sequence.
    ;; However a var that can shadow something (including any special var)
    ;; must always be replaced with a dummy.
    (DO ((KIS KEYINITS (CDR KIS))
	 (KNS KEYNAMES (CDR KNS))
	 (PKNS PSEUDO-KEYNAMES (CDR PKNS))
	 (KFS KEYFLAGS (CDR KFS)))
	((NULL KNS))
      (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
	    (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	(OR (AND (NULL KEYFLAG)
		 (CONSTANTP KEYINIT)
		 (NOT (ASSQ KEYNAME VARS))
		 (NOT (LEXICAL-VAR-P KEYNAME))
		 (NOT (SPECIALP KEYNAME)))
	    (PROGN (SETF (CAR KIS) 'SI:KEYWORD-GARBAGE)
		   (SETQ PSEUDO-KEYNAME (GENSYM))
		   (SETF (CAR PKNS) PSEUDO-KEYNAME)
		   (PUSH `(,KEYNAME
			   (COND ((EQ ,PSEUDO-KEYNAME SI:KEYWORD-GARBAGE)
				  ,KEYINIT)
				 (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
				    ,PSEUDO-KEYNAME)))
			 KEYCHECKS)))))
    (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
    (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

    ;; If the user didn't ask for a rest arg, make one for the
    ;; outer function anyway.
    (OR REST-ARG (SETQ REST-ARG (GENSYM)
		       MAYBE-REST-ARG (LIST '&REST REST-ARG)))
    `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
       (LET* (,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
	      ,@KEYFLAGS)
;       (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;	      (OR (%PERMUTE-ARGS)
;		  (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;			   (CDR ,REST-ARG)
;			   (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;			   ',KEYKEYS)
;			 (%PERMUTE-ARGS)))
;	      ;; If the function really wants the rest arg,
;	      ;; flush the permutation table and its keyword.
;	      ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;	     (T
	 (WHEN ,REST-ARG
	   (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
					,REST-ARG ',KEYKEYS
					,ALLOW-OTHER-KEYS
					(VARIABLE-LOCATION ,(CAR PSEUDO-KEYNAMES))))
	 (LET* ,KEYCHECKS
	   ,@decls
	   ((LAMBDA ,AUXVARS . ,BODY)))))))

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defun (:property make-list style-checker) (form)
  (if (= (length form) 3)
      (let ((*print-length* 3) (*print-level* 2))
	(warn 'obsolete ':obsolete "(MAKE-LIST ~S ~S) is an obsolete calling seqence.
   use (MAKE-LIST ~S~@[ :AREA ~S~]) instead." (second form) (third form)
   							 (third form)
							 (if (global:member (second form)
									    '(nil 'nil))
							     nil (second form))))))

))


; From file PATCH.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; PATCH  "

(DEFUN LOAD-PATCHES (&REST OPTIONS &AUX TEM SOMETHING-CHANGED)
  "Load any new patches for one or more systems.
Options can include these symbols:
 :NOSELECTIVE - don't ask about each patch.
 :SILENT or :NOWARN - don't print out any information on loading patches
   (and also don't ask).
 :VERBOSE - says to print out information about loading each patch.
   This is the default and is only turned off by :silent and :nowarn.
 :UNRELEASED - says to load or consider unreleased patches.
   Once unreleased patches have been loaded, a band may not be dumped.
 :FORCE-UNFINISHED - load all patches that have not been finished yet,
   if they have QFASL files.  This is good for testing patches.
 :NOOP - do nothing
 :SITE - load latest site configuration info.
 :NOSITE - do not load latest site configuration info.
   :SITE is the default unless systems to load patches for are specified.

Options can also include :SYSTEMS followed by a list of systems to load patches for.
One or more names of systems are also allowed.

LOAD-PATCHES returns T if any patches were loaded, otherwise NIL."
  (CATCH-ERROR-RESTART (SYS:REMOTE-NETWORK-ERROR
			 "Give up on trying to load patches.")
    (LET ((SYSTEM-NAMES NIL)			;A-list of systems to load patches for.
	  (SELECTIVE-P T)			;Ask the user.
	  (VERBOSE-P T)				;Tell the user what's going on.
	  (UNRELEASED-P NIL)
	  (SITE-SPECIFIED-P NIL)
	  (SITE-P T)
	  (FORCE-THROUGH-UNFINISHED-PATCHES-P NIL))
      (DO ((OPTS OPTIONS (CDR OPTS)))
	  ((NULL OPTS))
	(SELECTQ (CAR OPTS)
	  (:SYSTEMS
	   (SETQ OPTS (CDR OPTS))
	   (SETQ SYSTEM-NAMES
		 (IF (CONSP (CAR OPTS))
		     (MAPCAR 'GET-PATCH-SYSTEM-NAMED (CAR OPTS))
		   (LIST (GET-PATCH-SYSTEM-NAMED (CAR OPTS)))))
	   (UNLESS SITE-SPECIFIED-P
	     (SETQ SITE-P NIL)))
	  ((:SILENT :NOWARN) (SETQ VERBOSE-P NIL SELECTIVE-P NIL))
	  (:VERBOSE (SETQ VERBOSE-P T))
	  (:SELECTIVE (SETQ SELECTIVE-P T))
	  (:SITE (SETQ SITE-P T SITE-SPECIFIED-P T))
	  (:NOOP NIL)
	  (:NOSITE (SETQ SITE-P NIL SITE-SPECIFIED-P T))
	  (:UNRELEASED (SETQ UNRELEASED-P T))
	  (:NOSELECTIVE (SETQ SELECTIVE-P NIL))
	  (:FORCE-UNFINISHED (SETQ FORCE-THROUGH-UNFINISHED-PATCHES-P T))
	  (OTHERWISE
	    (COND ((AND (OR (SYMBOLP (CAR OPTS)) (STRINGP (CAR OPTS)))
			(SETQ TEM (GET-PATCH-SYSTEM-NAMED (CAR OPTS) T)))
		   (PUSH TEM SYSTEM-NAMES)
		   (UNLESS SITE-SPECIFIED-P
		     (SETQ SITE-P NIL)))
		  (T (FERROR NIL "~S is not a LOAD-PATCHES option and not a system name."
				 (CAR OPTS)))))))
      (WITH-SYS-HOST-ACCESSIBLE
	(LET-IF VERBOSE-P ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
	  (WHEN SITE-P
	    (WHEN VERBOSE-P
	      (FORMAT T "~%Checking whether site configuration has changed..."))
	    (IF (IF SELECTIVE-P
		    (MAKE-SYSTEM "SITE" :NO-RELOAD-SYSTEM-DECLARATION)
		  (IF VERBOSE-P
		      (MAKE-SYSTEM "SITE" :NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION)
		      (MAKE-SYSTEM "SITE" :NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION :SILENT)))
		(SETQ SOMETHING-CHANGED T)
	      (WHEN VERBOSE-P (FORMAT T "  it hasn't."))))
	  (OR SYSTEM-NAMES (SETQ SYSTEM-NAMES PATCH-SYSTEMS-LIST))
	  (LET ((FIRST-SYSTEM T))			; This is the first system being patched.
	    (DOLIST (PATCH SYSTEM-NAMES)
	      (CATCH-ERROR-RESTART (ERROR "Give up on patches for ~A." (CAR PATCH))
		(LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH T))
		       (NEW-VERS (PATCH-DIR-VERSION-LIST PATCH-DIR))
		       (MAJOR (PATCH-VERSION PATCH))
		       PATCHES-NOT-LOADED
		       (CHANGE-STATUS T)		;Ok to change the system status
		       (UNRELEASED-CONSIDERED NIL)	;T if considering unreleased patches.
		       (PATCH-SKIPPED NIL)	;T if considering patches after skipping one.
		       (PROCEED-FLAG (NOT SELECTIVE-P)))	; Has the user said to proceed?
		  (IF (AND (NULL PATCH-DIR) VERBOSE-P)
		      (FORMAT T "~&Skipping system ~A, whose patch directory cannot be accessed.~%"
			      (CAR PATCH))
		    ;; Get list of patches of this system not already loaded.
		    (SETQ PATCHES-NOT-LOADED
			  (CDR (MEMASSQ (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
					NEW-VERS)))
		    ;; Maybe announce the system.
		    (WHEN (AND PATCHES-NOT-LOADED VERBOSE-P)	;verbose and silent is nonsense
		      (FRESH-LINE *STANDARD-OUTPUT*)
		      (UNLESS FIRST-SYSTEM (TERPRI))
		      (FORMAT T "~&Patches for ~A (Current version is ~D.~D):"
			      (PATCH-NAME PATCH) MAJOR (CAAR (LAST NEW-VERS))))
		    (DOLIST (VERSION PATCHES-NOT-LOADED)
		      (LET* ((FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH)
							      ':PATCH-FILE
							      (PATCH-VERSION PATCH)
							      (VERSION-NUMBER VERSION)
							      ':QFASL)))
			;; NIL is used to mark patches that are reserved, but not finished.
			;; We can't load any more patches without this one, in order to
			;; make sure that any two systems claiming to be version xx.yy
			;; always have exactly the same set of patches loaded.  Punt.
			;; If someone forgets to finish a patch, we assume a hacker will
			;; eventually see what is happening and fix the directory to unstick
			;; things.  We might at least say the patches are unfinished.
			(UNLESS (VERSION-EXPLANATION VERSION)
			  (WHEN VERBOSE-P
			    (FORMAT T "~&There are unfinished patches in ~A."
				    (PATCH-NAME PATCH)))
			  (UNLESS FORCE-THROUGH-UNFINISHED-PATCHES-P
			    (RETURN)))
			(WHEN (VERSION-UNRELEASED VERSION)
			  (WHEN VERBOSE-P
			    (FORMAT T "~&There are unreleased patches in ~A."
				    (PATCH-NAME PATCH)))
			  (OR
			    FORCE-THROUGH-UNFINISHED-PATCHES-P 
			    UNRELEASED-P
			    UNRELEASED-CONSIDERED
			    (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			    (AND SELECTIVE-P
				 (WITH-TIMEOUT ((* 5 60. 60.)
						(FORMAT T " -- timed out, No.")
						NIL)
				   (FORMAT T "~&Such patches are subject to change; therefore,
  you should not load them if you are going to dump a band.
  If you are not going to dump a band, it is reasonable
  to load these patches to benefit from the improvements in them.")
				   (SETQ PROCEED-FLAG NIL)
				   (Y-OR-N-P "Consider the unreleased patches? (Automatic No after 5 minutes) ")))
			    (RETURN))
			  (SETQ UNRELEASED-CONSIDERED T))
			(WHEN VERBOSE-P
			  (PRINT-PATCH (PATCH-VERSION PATCH) VERSION))
			(SELECTQ-EVERY
			  (COND (PROCEED-FLAG)
				(T (WITH-TIMEOUT ((* 5 60. 60.)
						  (FORMAT T " -- timed out, Proceed.")
						  'PROCEED)
				     (FQUERY '(:CHOICES (((T "Yes.") #/Y #/SP #/T #/HAND-UP)
							 ((NIL "No.") #/N #/RUBOUT #/HAND-DOWN)
							 ((PROCEED "Proceed.") #/P)))
					     "Load? (Automatic Proceed after 5 minutes) "))))
			  (NIL
			   ;; "No", don't load any more for this system.
			   ;; Also don't change the status.
			   ;; Except, if we are considering unreleased patches,
			   ;; loading out of order is no worse than loading unreleased
			   ;; patches in the first place, so keep on offering.
			   (SETQ CHANGE-STATUS NIL)
			   (UNLESS (OR FORCE-THROUGH-UNFINISHED-PATCHES-P
				       UNRELEASED-CONSIDERED)
			     (RETURN NIL))
			   (WHEN (EQ VERSION (CAR (LAST PATCHES-NOT-LOADED)))
			     ;; Don't give a spiel about following patches
			     ;; if there are none.
			     (RETURN NIL))
			   (UNLESS (OR PATCH-SKIPPED
				       (EQ (PATCH-STATUS PATCH) ':INCONSISTENT))
			     (FORMAT T "~&If you load any following patches for this system,
  they will be out of sequence, so you must not dump a band.")
			     (SETQ PATCH-SKIPPED T)))
			  (PROCEED
			   ;; "Proceed" with the rest for this system.
			   (SETQ PROCEED-FLAG T))
			  ((T PROCEED)
			   ;; "Yes" or "Proceed", do this one.
			   (SETQ SOMETHING-CHANGED T)
			   ;; Unfinished, unreleased or out of sequence =>
			   ;;  mark system as inconsistent.
			   (WHEN (OR PATCH-SKIPPED
				     (NULL (VERSION-EXPLANATION VERSION))
				     (VERSION-UNRELEASED VERSION))
			     (UNLESS (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			       (SETF (PATCH-STATUS PATCH) ':INCONSISTENT)
			       (FORMAT T "~&~A is now inconsistent; do not dump a band."
				       (PATCH-NAME PATCH))))
			   ;; Avoid error if non ex file, if patch is known to be unfinished.
			   (CONDITION-CASE-IF (NULL (VERSION-EXPLANATION VERSION)) ()
			       (LOAD FILENAME NIL NIL T (NOT VERBOSE-P))	; Don't set default,
			     (FS:FILE-NOT-FOUND
			      (WHEN VERBOSE-P
				(FORMAT T "~&File ~A does not exist, ignoring this patch."
					FILENAME))))
			   (PUSH VERSION (PATCH-VERSION-LIST PATCH))))))
		    (AND CHANGE-STATUS
			 (NEQ (PATCH-STATUS PATCH) ':INCONSISTENT)
			 (LET ((NEW-STATUS (PATCH-DIR-STATUS PATCH-DIR)))
			   (COND ((NEQ (PATCH-STATUS PATCH) NEW-STATUS)
				  (SETQ SOMETHING-CHANGED T)
				  (WHEN VERBOSE-P
				    (FORMAT T "~&~A is now ~A."
					    (PATCH-NAME PATCH)
					    (FOURTH (ASSQ NEW-STATUS
							  SYSTEM-STATUS-ALIST))))
				  ;; Update the status.
				  (SETF (PATCH-STATUS PATCH) NEW-STATUS))))))))
	      (SETQ FIRST-SYSTEM NIL)))))))
  SOMETHING-CHANGED)

))

; From file INFIX.LISP PS:<L.IO1> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; INFIX  "

(DEFUN INFIX-TOPLEVEL-PARSE (*STANDARD-INPUT* IGNORE IGNORE)
  (LET ((INFIX-TOKEN (INFIX-READ-TOKEN)))
    (INFIX-PARSE -1)))

))

; From file COLOR.LISP PS:<L.WINDOW> OZ:
#8R COLOR#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COLOR")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLOR  "

(DEFMETHOD (COLOR-SCREEN :PARSE-FONT-SPECIFIER) (FD)
  (SETQ FD (TV:SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:COLOR-FONT))
  (cond  ((GET (FONT-NAME FD) 'COLOR-FONT))
	 ((GET (FONT-NAME FD) 'FONTS:COLOR-FONT)
	  (SETQ FD (SYMBOL-VALUE (GET (FONT-NAME FD) 'FONTS:COLOR-FONT))))
	 (T (SETQ FD (MAKE-COLOR-FONT FD))))
  FD)

))

; From file COLOR.LISP PS:<L.WINDOW> OZ:
#8R COLOR#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COLOR")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; COLOR  "

(DEFMETHOD (COLOR-SCREEN :PARSE-FONT-DESCRIPTOR) (FD)
  (SETQ FD (TV:SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:COLOR-FONT))
  (COND  ((GET (FONT-NAME FD) 'COLOR-FONT))
	 ((GET (FONT-NAME FD) 'FONTS:COLOR-FONT)
	  (SETQ FD (SYMBOL-VALUE (GET (FONT-NAME FD) 'FONTS:COLOR-FONT))))
	 (T (SETQ FD (MAKE-COLOR-FONT FD))))
  FD)

))
