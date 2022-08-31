;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.2
;;; Reason: Printing of package objects.
;;; Chaosnet retransmission every 1/2 second.
;;; LET-IF arg evaluation bug.  PROGW now uses global environment.
;;; ERRSET-HANDLER bug.
;;; Written 9/12/84 11:42:34 by RMS,
;;; while running on Lisp Machine Twenty-five from band 5
;;; with Experimental System 99.0, CADR 4.0, Experimental ZMail 54.0, MIT-Specific 23.0, microcode 320, MIT GC@0.



; From file CLPACK.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun package-named-structure-invoke (op pkg &rest args)
  (selectq-with-which-operations op
    (:describe 
     (describe-package pkg))
    (:print-self
     (let ((stream (car args)))
       (if *print-escape*
	   (sys:printing-random-object (pkg stream)
	     (princ "Package " stream)
	     (princ (pkg-name pkg) stream))
	 (princ (pkg-name pkg) stream))))
    ((:get :get-location-or-nil :get-location :getl :putprop :remprop :push-property :plist
	   :plist-location :property-list-location :setplist :set)
     (apply #'property-list-handler op (locf (pkg-plist pkg)) args))))

))

; From file CHSNCP.LISP OZ:<L.NETWORK.CHAOS> OZ:
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSNCP  "

(DEFCONST RETRANSMISSION-INTERVAL 30.)		;  1/2 second

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun let-if (cond &quote var-list &quote &rest body)
  "Perform the bindings in VAR-LIST only if COND is non-NIL; the execute the BODY.
Aside from the presence of COND, LET-IF is just like LET.
The variables are always bound as specials if they are bound;
therefore, strictly speaking only variables declared special should be used."
  (if (not cond)
      (if (eq *interpreter-function-environment* t)
	  (eval-body body)
	(gobble-declarations-from-body (vars-env body)
	  (eval-body body)))
    ;; Cannot use PROGW here; it calls EVAL rather than EVAL1.
    (if (eq *interpreter-function-environment* t)
	(zl-parallel-binding-list (var-list)
	  (eval-body body))
      (gobble-declarations-from-body (vars-env body)
	(parallel-binding-list (var-list vars-env)
	  (eval-body body))))))

))

; From file EH.LISP OZ:<L.SYS2> OZ:
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "

(DEFUN SI:ERRSET-HANDLER (CONDITION TAG PRINTFLAG)
  (UNLESS (OR ERRSET (SEND CONDITION ':DANGEROUS-CONDITION-P))
    (WHEN PRINTFLAG
      (TERPRI ERROR-OUTPUT)
      (SEND CONDITION ':PRINT-ERROR-MESSAGE CURRENT-STACK-GROUP T ERROR-OUTPUT))
    (*THROW TAG NIL)))

))

; From file EVAL.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun progw (vars-and-vals &quote &rest body)
  "Perform bindings from a list of variables and expressions, then execute the BODY.
VARS-AND-VALS is a list of elements like (VARIABLE VALUE-FORM).
The VALUE-FORMs are all evaluated by PROGW, even when compiled.
Note that the value of VARS-AND-VALS is computed each time,
 and always in the global environment.
The variables are always bound as specials if they are bound;
therefore, strictly speaking only variables declared special should be used."
  (do ((vars-and-vals vars-and-vals (cdr vars-and-vals)))
      ((null vars-and-vals)
       (eval-body body))
    (%bind (locf (symbol-value (caar vars-and-vals)))
	   (eval (cadar vars-and-vals)))))

))
