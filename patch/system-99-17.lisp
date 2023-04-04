;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:T; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.17
;;; Reason:
;;;  Bugs in cl:(n)subst (also sped up)
;;;  Compiler check for lexical closure over instance vars (notyetimplementedsigh)
;;;  Could these be the last array-index-order bugs?
;;;   Two small problems in window system code (dg&rg)
;;;  GC flippery (khs)
;;;  Parsing LM pathnames improvements (and a bug fix)
;;;  Methods-calling-interpreted-functions-which-make-special-reference-to-
;;;   variables-which-just-happen-to-have-the-same-name-as-an-instance-
;;;   variables-of-self bug discovered and fixed. (Yow!)
;;; Written 10-Dec-84 03:57:14 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Experimental System 99.13, CADR 4.0, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.


; total spazz by mly
; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (535)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(DEFUN DECODE-CANONICAL-TYPE (CANONICAL-TYPE SYSTEM-TYPE)
  (LET ((PROP (GETF CANONICAL-TYPES CANONICAL-TYPE)))
    (IF (NULL PROP)
	CANONICAL-TYPE
      (LET ((PER-SYSTEM (OR (ASSQ SYSTEM-TYPE PROP) (ASSQ NIL PROP))))
	(VALUES (CADR PER-SYSTEM) (CDR PER-SYSTEM))))))
))


; From file GENRIC.LISP OZ:<MLY.LL> OZ: (6)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun cli:subst (new old tree &key test test-not key)
  "Replace with NEW every atom or subtree in TREE which matches OLD.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used.
TEST is a function passed OLD and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (subst-1 new old tree (or test-not test) (not (null test-not)) key nil))

(defun subst-if (new predicate tree &key key)
  "Replace with NEW every atom or subtree in TREE which satisfies PREDICATE.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (subst-1 new nil tree nil nil key predicate))

(defun subst-if-not (new predicate tree &key key)
  "Replace with NEW every atom or subtree in TREE which doesn't satisfy PREDICATE.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (subst-1 new nil tree nil t key predicate))

(defun subst-1 (new old tree test invertp key one-arg-predicate)
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

(defun nsubst (new old tree &key test test-not key)
  "Destructively replace with NEW every atom or subtree in TREE which matches OLD.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used.
TEST is a function passed OLD and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (if (and (null test-not) (null key) (or (null key) (eq key 'eql) (eq key #'eql)))
      (nsubst-eql new old tree)
    (nsubst-1 new old tree (or test-not test) (not (null test-not)) key nil)))

(defun nsubst-if (new predicate tree &key key)
  "Destructively replace with NEW every atom or subtree in TREE which satisfies PREDICATE.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (nsubst-1 new nil tree nil nil key predicate))

(defun nsubst-if-not (new predicate tree &key key)
  "Destructively replace with NEW every atom or subtree in TREE not satisfying PREDICATE.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (nsubst-1 new nil tree nil t key predicate))

(defun nsubst-eql (new old s-exp)
  (cond ((eql old s-exp) new)
	((atom s-exp) s-exp)
	(t (do ((s s-exp (cdr s))
		(prev nil s))
	       ((atom s)
		(when (eql old s)
		  (setf (cdr prev) new)))
	     (if (atom (car s))
		 (when (eql old (car s))
		   (setf (car s) new))
	       (setf (car s) (nsubst-eql new old (car s)))))
	   s-exp)))

(defun nsubst-1 (new old tree test invertp key one-arg-predicate)
  (cond ((let ((elt (if key (funcall key tree) tree)))
	   (eq invertp (not (cond (one-arg-predicate (funcall one-arg-predicate elt))
				  (test (funcall test old elt))
				  (t (eql old elt))))))
	 new)
	((atom tree)
	 tree)
	(t
	 (do ((tail tree (cdr tail)))
	     (())
	   (setf (car tail) (nsubst-1 new old (car tail) test invertp key one-arg-predicate))
	   (when (atom (cdr tail))
	     (let ((newcdr (nsubst-1 new old (cdr tail) test invertp key one-arg-predicate)))
	       ;; Avoid a RPLACD that could de-cdr-code the list, if it's not needed.
	       (unless (eq (cdr tail) newcdr)
		 (setf (cdr tail) newcdr)))
	     (return tree))))))

))

; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (535)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

(define-canonical-type :lossage "LOSSAGE"
  (:vms "LOS")
  (:unix "LOSS"))

))

; From file QCP1.LISP OZ:<MLY.LL> OZ: (4)
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN VAR-MAKE-HOME (NAME TYPE KIND INIT-SPECS
		      EVAL-TYPE MISC-TYPES THIS-FRAME-DECLARATIONS &AUX HOME)
  (COND ((NULL (MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST
			    FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
	 (BARF KIND 'BAD-KIND 'BARF))
	((KEYWORDP NAME)
	 (WARN 'KEYWORD-BOUND :IMPOSSIBLE
	       "Binding the keyword symbol ~S." NAME))
	((CONSTANTP NAME)
	 (WARN 'SYSTEM-CONSTANT-BOUND :IMPLAUSIBLE
	       "Binding ~S, which is a constant." NAME))
	((MEMQ NAME (CDDR SELF-FLAVOR-DECLARATION))
	 (WARN 'INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE
	       "Binding ~S, which has the same name as an instance variable of flavor ~S"
	       NAME (CAR SELF-FLAVOR-DECLARATION)))
	((AND (EQ NAME 'SELF)
	      SELF-FLAVOR-DECLARATION
	      (EQ TYPE 'FEF-SPECIAL))
	 (WARN 'SELF-BOUND :IMPLAUSIBLE
	       "Rebinding ~S. You may lose!" 'SELF)))
  ;; Rest args interfere with fast arg option except when there are no specials.
  ;; We need to look at this to
  ;;  decide how to process all the AUX variables and can't tell when processing
  ;;  the first one whether the next will be special.
  ;;  In any case, being wrong about this should not be able to produce
  ;;  incorrect code.
  (COND ((EQ KIND 'FEF-ARG-REST)
	 (SETQ FAST-ARGS-POSSIBLE NIL))
	((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
	 (AND INIT-SPECS (SETQ FAST-ARGS-POSSIBLE NIL))))
  ;; Detect vars bound to themselves which fail to be special.
  (WHEN (AND (EQ NAME (CAR INIT-SPECS))
	     (NOT (ASSQ NAME VARS))
	     ;; If variable is already accessible lexically, it need not be special.
	     (DOLIST (FRAME *OUTER-CONTEXT-VARIABLE-ENVIRONMENT* T)
	       (WHEN (ASSQ NAME FRAME) (RETURN NIL))))
    (MSPL2 NAME)
    (SETQ TYPE 'FEF-SPECIAL))
  ;; Cons up the variable descriptor.
  ;; Note that INIT-SPECS is not the final value that will go in the INIT slot.
  (SETQ HOME (MAKE-VAR :NAME NAME :KIND KIND :TYPE TYPE
		       :INIT INIT-SPECS :EVAL EVAL-TYPE :MISC MISC-TYPES
		       :DECLARATIONS (DECLARATIONS-FOR-VARIABLE NAME THIS-FRAME-DECLARATIONS)))
  (IF (AND (EQ TYPE 'FEF-SPECIAL) (GETF (VAR-DECLARATIONS HOME) 'IGNORE))
      (WARN 'NOT-IGNORED :IMPLAUSIBLE
	    "The special variable ~S was declared to be ignored" NAME))
  (SETF (VAR-LAP-ADDRESS HOME)
	;; Not the real lap address,
	;; but something for P1 to use for the value of the variable
	(IF (EQ TYPE 'FEF-SPECIAL) NAME `(LOCAL-REF ,HOME)))
  HOME)

))

; From file QCP1.LISP OZ:<MLY.LL> OZ: (4)
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "OZ:OZ:<MLY.LL>QCP1.."

(defun breakoff (x &optional lexical &aux fname fname-to-give local-name)
  (let ((non-instance-vars)
	(sfd self-flavor-declaration)
	;selfp
	)
    (dolist (home vars)
      (and (eq (var-type home) 'fef-local)
	   ;; Omit shadowed bindings.
	   (eq home (assq (var-name home) vars))
	   (pushnew (var-name home) non-instance-vars)))
    (dolist (elt *outer-context-variable-environment*)
      (dolist (home elt)
	(push (var-name home) non-instance-vars)))
    (multiple-value-bind (vars-needed-lexically functions-needed-lexically
			  block-names go-tags)
	(cw-top-level-lambda-expression
	  x					;form
	  (append (if sfd (list* 'self (cddr sfd)))
		  non-instance-vars)		;variables we're interested in
	  (mapcar #'car *local-functions*)	;functions we're interested in
	  *function-environment*)
      (let (tem w)
	(dolist (v vars-needed-lexically)
	  (cond ((and (memq v (cddr sfd)) (not (memq v non-instance-vars)))
		 (warn 'instance-variable-used-in-internal-lambda :unimplemented
		       "The ~:[~;special ~]instance variable ~S of flavor ~S
is being referenced by a lexically closed-over function.
This will not work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]"
		     (memq v (cadr sfd)) v (car sfd) 'self (prog1 w (setq w t))))
		((and (eq v 'self) sfd)
		 (warn 'self-used-in-internal-lambda :unimplemented
		       "~S is being referenced by a lexically closed-over function.
This will not, of course, work outside of the dynamic scope of ~S.~:[
This problem will be fixed when Mly's brain hurts a little less.~]"
		       'self 'self (prog1 w (setq w t))))
		(t
		 ;; Note: if V is not on VARS, it must come from an outer lexical level.
		 ;; That is ok, and it still requires this LAMBDA to be lexical to access it.
		 (setq lexical t)
		 (setq tem (assq v vars))
		 (when tem
		   (pushnew 'fef-arg-used-in-lexical-closures (var-misc tem) :test #'eq))))))
      (dolist (f functions-needed-lexically)
	(let ((tem (assq f *local-functions*)))
	  (when tem
	    (setq lexical t)
	    (pushnew 'fef-arg-used-in-lexical-closures
		     (var-misc (cadr tem)) :test #'eq))))
      (dolist (b block-names)
	(let ((tem (assq b *progdesc-environment*)))
	  (when tem
	    (setq lexical t)
	    (setf (progdesc-used-in-lexical-closures-flag tem) t))))
      (dolist (g go-tags)
	(let ((tem (assoc-equal g *gotag-environment*)))
	  (when tem
	    (setq lexical t)
	    (setf (gotag-used-in-lexical-closures-flag tem) t)
	    (setf (progdesc-used-in-lexical-closures-flag (gotag-progdesc tem)) t))))))
  (if (and (eq (car x) 'named-lambda)
	   (not (memq (cadr x) local-function-map)))
      (setq local-name (cadr x))
      (setq local-name *breakoff-count*))
  (setq fname `(:internal ,function-to-be-defined ,*breakoff-count*)
	fname-to-give `(:internal ,name-to-give-function ,local-name))
  (push local-name local-function-map)
  (incf *breakoff-count*)
  (when lexical
    (incf *lexical-closure-count*))
  (let ((local-decls local-declarations))
;>> this is already in there.
;    ;; Pass along the parent function's self-flavor declaration.
;    (if sfd (push `(:self-flavor . ,sfd) local-decls))
    (setq compiler-queue
	  (nconc compiler-queue
		 (ncons
		   (make-compiler-queue-entry
		     :function-spec fname
		     :function-name fname-to-give
		     :definition x
		     :declarations local-decls
		     :variables (and lexical (cons t *outer-context-variable-environment*))
		     :local-functions (and lexical *local-functions*)
		     :progdescs *progdesc-environment*
		     :gotags *gotag-environment*
		     :function-environment *function-environment*
		     )))))
  (let ((tem `(breakoff-function ,fname)))
    (if lexical `(lexical-closure ,tem) tem)))

))

;; by dg
; From file FED.LISP#> QL.WINDOW; LAM3:
#8R FED#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFUN DEDUCE-WINDOW-ARRAY-SIZE (&OPTIONAL ARRAY-TYPE)
  "Set WINDOW-ARRAY to an array of type ARRAY-TYPE, with the correct size.
The correct size is enough to record enough boxes of size BOX-X-SIZE
by BOX-Y-SIZE to fill up the window.
Also sets WINDOW-X-SIZE and WINDOW-Y-SIZE to the size values."
  (DECLARE (:SELF-FLAVOR GRID-MIXIN))
  (OR ARRAY-TYPE (SETQ ARRAY-TYPE (ARRAY-TYPE WINDOW-ARRAY)))
  (LET ((LAST-ROW-OF-DOTS
	  (IF (AND (> BOX-X-SIZE MIN-BOX-SIZE)
		   (> BOX-Y-SIZE MIN-BOX-SIZE))
	      2
	      0)))
    (SETQ WINDOW-X-SIZE (TRUNCATE (- (TV:SHEET-INSIDE-WIDTH) LAST-ROW-OF-DOTS) BOX-X-SIZE)
	  WINDOW-Y-SIZE (TRUNCATE (- (TV:SHEET-INSIDE-HEIGHT) LAST-ROW-OF-DOTS) BOX-Y-SIZE))
    (OR (AND (VARIABLE-BOUNDP WINDOW-ARRAY)
	     ( WINDOW-X-SIZE (ARRAY-DIMENSION WINDOW-ARRAY 1))
	     ( WINDOW-Y-SIZE (ARRAY-DIMENSION WINDOW-ARRAY 0)))
	(SETQ WINDOW-ARRAY (MAKE-ARRAY (LIST WINDOW-X-SIZE WINDOW-Y-SIZE)
				       ':TYPE ARRAY-TYPE)))))

))

;; by rg
; From file SCRMAN.LISP#> QL.WINDOW; LAM3:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SCRMAN  "

(DEFUN SCREEN-MANAGE-GRAY-RECTANGLE (RECT ARRAY X Y ALU)
  "Gray the specified rectangle on the specified array.
All graying is relative to (0, 0) on the sheet that the rectangle is on."
  (DECLARE (:SELF-FLAVOR GRAY-DEEXPOSED-RIGHT-MIXIN))
  (LET ((X-OFF (- (RECT-LEFT RECT) (SECOND (RECT-SOURCE RECT))))
	(Y-OFF (- (RECT-TOP RECT) (THIRD (RECT-SOURCE RECT)))))
    (BITBLT (OR ALU CHAR-ALUF)
	    (- (RECT-RIGHT RECT) (RECT-LEFT RECT)) (- (RECT-BOTTOM RECT) (RECT-TOP RECT))
	    GRAY-ARRAY (\ X-OFF (ARRAY-DIMENSION GRAY-ARRAY 1))
	    	       (\ Y-OFF (ARRAY-DIMENSION GRAY-ARRAY 0))
	    ARRAY (+ X (RECT-LEFT RECT)) (+ Y (RECT-TOP RECT)))))

))

;; by khs
; From file GC.LISP KANSAS:<L.SYS2> OZ: (173)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN GC-FLIP-NOW ()
  (WITH-LOCK (GC-FLIP-LOCK)
    (IF (NOT %GC-FLIP-READY) (GC-RECLAIM-OLDSPACE))	;In case not reclaimed already
    (SETQ %PAGE-CONS-ALARM 0 %REGION-CONS-ALARM 0)	;avoid overflow in these fixnums
    (DOLIST (ELEM GC-DAEMON-QUEUE)
      (GC-DAEMON-QUEUE (FIRST ELEM) (SECOND ELEM) 1 1 ELEM))
    (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE)
	(GC-GET-SPACE-SIZES)
      (GC-REPORT			;separate static from exited when exited exists?
	"GC: About to flip.  Dynamic space=~D. Static space=~D. Free space=~D."
	DYNAMIC-SIZE (+ STATIC-SIZE EXITED-SIZE) FREE-SIZE)
      (WITHOUT-INTERRUPTS
	(PROCESS-WAIT "Flip inhibited" #'(LAMBDA () (NOT INHIBIT-GC-FLIPS)))
	;; Perform whatever actions other programs need to do on flips
	(MAPC #'EVAL GC-EVERY-FLIP-LIST)
	(MAPC #'EVAL (PROG1 GC-NEXT-FLIP-LIST
			    (SETQ GC-NEXT-FLIP-LIST GC-SECOND-NEXT-FLIP-LIST
				  GC-SECOND-NEXT-FLIP-LIST NIL)))
	;; Reset the GC scan pointers of all regions, actually only in static and fixed areas
	;; is it necessary.
	(DO ((REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION)))
	    ((MINUSP REGION))
	  (%GC-SCAV-RESET REGION)
	  (STORE (REGION-GC-POINTER REGION) 0))
	;; Invalidate AR-1's cache.
	(SETQ AR-1-ARRAY-POINTER-1 NIL)
	(SETQ AR-1-ARRAY-POINTER-2 NIL)
	;; Don't forget to actually flip! (Change newspace to oldspace in all dynamic areas)
	(%GC-FLIP T)
	;; Deallocate space at the end of the oldspace regions, if we can.
	(DOTIMES (REGION SIZE-OF-AREA-ARRAYS)
	  (IF (= %REGION-SPACE-OLD
		 (%LOGLDB %%REGION-SPACE-TYPE (REGION-BITS REGION)))
	      (DEALLOCATE-END-OF-REGION REGION)))
	(INCF %GC-GENERATION-NUMBER)
	(WHEN GC-AFTER-FLIP-LIST
	  (GC-REPORT
	    "GC: something is using SI::GC-AFTER-FLIP-LIST; please send a bug report.")
	  (MAPC #'EVAL GC-AFTER-FLIP-LIST))
	(INITIALIZATIONS 'AFTER-FLIP-INITIALIZATION-LIST T))
      (SETQ GC-OLDSPACE-EXISTS T)
      T)))

))

; From file GC.LISP KANSAS:<L.SYS2> OZ: (173)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN DEALLOCATE-END-OF-REGION (REGION)
  "Return as much as possible of unused part of region REGION to free pool.
It can then be allocated into other regions."
  (WITHOUT-INTERRUPTS
    (LET* ((FREE-POINTER (REGION-FREE-POINTER REGION))
	   (ORIGIN (REGION-ORIGIN REGION))
	   (SIZE (REGION-LENGTH REGION))
	   (N-QUANTA (TRUNCATE SIZE %ADDRESS-SPACE-QUANTUM-SIZE))
	   ;; MAX 1 below is so we don't truncate a region to zero length.
	   ;; I suspect that that causes crashes.
	   (FIRST-FREE-QUANTUM
	     (MIN N-QUANTA (MAX 1 (CEILING FREE-POINTER %ADDRESS-SPACE-QUANTUM-SIZE)))))
      (UNLESS (= FIRST-FREE-QUANTUM N-QUANTA)	;Less than one quantum is free.
	(DO ((I FIRST-FREE-QUANTUM (1+ I))
	     ;; (truncate origin %address-space-quantum-size), as unsigned number.
	     (ORIGIN-QUANTUM (LSH ORIGIN (- 1 (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE)))))
	    ((= I N-QUANTA))
	  (SETF (AREF #'ADDRESS-SPACE-MAP (+ ORIGIN-QUANTUM I)) 0))
	(SETF (REGION-LENGTH REGION) (* FIRST-FREE-QUANTUM %ADDRESS-SPACE-QUANTUM-SIZE))
	(DEALLOCATE-PAGES (%POINTER-PLUS ORIGIN (REGION-LENGTH REGION))
			  (TRUNCATE (%POINTER-DIFFERENCE SIZE (REGION-LENGTH REGION))
				    PAGE-SIZE))))
    (INVALIDATE-REGION-MAPPING REGION)))

))


;;; khs loses (not cadr-conditionalized)
; From file GC.LISP KANSAS:<L.SYS2> OZ: (173)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

;(DEFUN GC-RECLAIM-OLDSPACE-AREA (AREA)
;  (CHECK-ARG AREA (AND (NUMBERP AREA) ( 0 AREA SIZE-OF-AREA-ARRAYS)) "an area number")
;  (WITHOUT-INTERRUPTS
;    (OR %GC-FLIP-READY
;	(FERROR NIL "You cannot reclaim oldspace now, there may be pointers to it"))
;    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION))
;	 (REGION-TO-FREE)
;	 (PREV-REGION NIL REGION))
;	(())
;     NEXTLOOP					;May GO here to avoid advancing DO variables
;      (AND (MINUSP REGION) (RETURN NIL))
;      (AND (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-OLD)
;	   ;--no longer true--
;	   ;; Free this region unless that would leave the area without any regions
;	   ;; at all, which would lose since there would be no place to remember its bits.
;	   ;; Before freeing, unthread from area's region list.
; 	   (COND ((OR PREV-REGION (NOT (MINUSP (REGION-LIST-THREAD REGION))))
;		  (SETQ REGION-TO-FREE REGION
;			REGION (REGION-LIST-THREAD REGION))
;		  (IF PREV-REGION (STORE (REGION-LIST-THREAD PREV-REGION) REGION)
;		    (STORE (AREA-REGION-LIST AREA) REGION))
;		  (%GC-FREE-REGION REGION-TO-FREE)
;		  (GO NEXTLOOP))
;		 (T
;		  ;; Force region to new space, and reset free,GC pointers.
;		  (CHANGE-REGION-TO-NEW-SPACE REGION)
;		  (SETF (%REGION-FREE-POINTER REGION) 0)
;		  (%GC-SCAV-RESET REGION))))
;      (AND (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-COPY)
;           ;;; Change this region to NEW space so that it can be used for normal
;           ;;; consing
;	   (CHANGE-REGION-TO-NEW-SPACE REGION)))))

(defun change-region-to-new-space (region)
  (setf (region-bits region)
	(%logdpb 0 %%region-scavenge-enable
		 (%logdpb %region-space-new %%region-space-type
			  (%logdpb 1 %%region-oldspace-meta-bit (region-bits region)))))
  (invalidate-region-mapping region))

(defun invalidate-region-mapping (region)
  (do ((ra 0 (+ ra page-size))
       (virtual-address))
      ((> ra (region-length region)))
    (setq virtual-address (%pointer-plus (region-origin region) ra))
    (%change-page-status virtual-address nil nil)))

))

; From file LMPARS.LISP KANSAS:<L.FILE> OZ: (112)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; LMPARS  "

(DEFMETHOD (LM-PARSING-MIXIN :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
	;; Canonicalize list of length 1 into a single string.
	((AND (CONSP SPEC)
	      (STRINGP (CAR SPEC))
	      (NULL (CDR SPEC)))
	 (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
	;; A list of strings is also a structured directory.
	((AND (CONSP SPEC)
	      (LOOP FOR ELT IN SPEC
		    ALWAYS (OR (STRINGP ELT) (MEMQ ELT '(NIL :ROOT :UNSPECIFIC :WILD)))))
	 (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
	((MEMQ SPEC '(NIL :ROOT :UNSPECIFIC :WILD)) SPEC)
	(T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

))

; From file LMPARS.LISP KANSAS:<L.FILE> OZ: (112)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; LMPARS  "

(DEFUN LM-PARSE-NAMESTRING (STRING &OPTIONAL (START 0) END
			    &AUX CHAR STATE TEM TEM1 FIELD-START
			    (DEVICE "DSK") DIRECTORY NAME TYPE VERSION)
  ;; STATE can be T, DOTTED, VERSION, DIRECTORY or NIL.
  ;; NIL is the initial state, and means anything is allowed and nothing is in progress.
  ;; T means that we are in the middle of a name, but nothing else special.
  ;; DOTTED means we have encountered a single period.  TEM is what preceded it.
  ;; DOUBLE-DOTTED means we have encountered "name.name."
  ;;  TEM is the first name and TEM1 is the second.
  ;; DIRECTORY means we have encountered "name . name . name"
  ;;  which can only be the beginning of a directory name,
  ;;  or else that we have encountered a "<".
  ;; VERSION means reading a version number (following a #).
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
  (DO ((INDEX START (1+ INDEX))) (NIL)
    (IF ( INDEX END)
	(SETQ CHAR 'DONE)
      (SETQ CHAR (CHAR STRING INDEX)))
    (COND ((AND (NOT (MEMQ CHAR '(#/SP #/TAB #/. #/: #/; #/# DONE)))
		(OR (NOT (MEMQ CHAR '(#/< #/>)))
		    (MEMQ STATE '(VERSION DOUBLE-DOTTED))))
	   (AND (%STORE-CONDITIONAL (LOCF STATE) NIL T)
		(SETQ FIELD-START INDEX))
	   (COND ((OR (EQ CHAR #//) (EQ CHAR #/))
		  (SETQ INDEX (1+ INDEX))
		  (OR (< INDEX END)
		      (LM-CHAR-ERROR STRING INDEX 'DONE))
		  (SETQ CHAR (CHAR STRING INDEX))
		  (AND ( CHAR #o200)
		       ( CHAR #/TAB)
		       (LM-CHAR-ERROR STRING INDEX CHAR)))))
	  ((EQ CHAR #/<)
	   (COND ((NULL STATE))			;Extraneous whitespace.
		 ((EQ STATE T)
		  (SETQ NAME (LM-FIELD STRING FIELD-START INDEX)))
		 ((EQ STATE 'DOTTED)
		  (AND TEM (SETQ NAME TEM))
		  (SETQ TYPE (LM-FIELD STRING FIELD-START INDEX)))
		 ((EQ STATE 'DOUBLE-DOTTED)
		  (AND TEM (SETQ NAME TEM))
		  (AND TEM1 (SETQ TYPE TEM1))
		  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T)))
		 (T (LM-CHAR-ERROR STRING INDEX CHAR)))
	   (SETQ STATE 'DIRECTORY DIRECTORY NIL)
	   (GO NEW-FIELD))
	  ((MEMQ CHAR '(#/SP #/TAB DONE))
	   (COND ((NULL STATE))			;Extraneous whitespace.
		 ((EQ STATE T)
		  (SETQ NAME (LM-FIELD STRING FIELD-START INDEX) STATE NIL))
		 ((EQ STATE 'DOTTED)
		  (AND TEM (SETQ NAME TEM))
		  (SETQ TYPE (LM-FIELD STRING FIELD-START INDEX) STATE NIL))
		 ((EQ STATE 'DOUBLE-DOTTED)
		  (AND TEM (SETQ NAME TEM))
		  (AND TEM1 (SETQ TYPE TEM1))
		  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T)
			STATE NIL)
		  (COND ((EQ VERSION 0) (SETQ VERSION :NEWEST))
			((EQ VERSION -2) (SETQ VERSION :OLDEST))))
		 ((EQ STATE 'VERSION)
		  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T) STATE NIL))
		 (T (LM-CHAR-ERROR STRING INDEX CHAR))))
	  ((EQ CHAR #/.)
	   (COND ((NULL STATE)			;Could only be :UNSPECIFIC name
		  (SETQ TEM NIL STATE 'DOTTED))
		 ((EQ STATE T)			;Could either be directory or name
		  (SETQ STATE 'DOTTED TEM (LM-FIELD STRING FIELD-START INDEX)))
		 ((EQ STATE 'DOTTED)
		  (OR TEM (LM-CHAR-ERROR STRING INDEX #/.))
		  (SETQ TEM1 (LM-FIELD STRING FIELD-START INDEX)
			STATE 'DOUBLE-DOTTED))
		 ((EQ STATE 'DOUBLE-DOTTED)
		  (OR TEM (LM-CHAR-ERROR STRING INDEX #/.))
		  (SETQ STATE 'DIRECTORY
			DIRECTORY (LIST* TEM TEM1 (LM-FIELD STRING FIELD-START INDEX) NIL)))
		 ((EQ STATE 'DIRECTORY)
		  (SETQ DIRECTORY
			(NCONC DIRECTORY (NCONS (LM-FIELD STRING FIELD-START INDEX)))))
		 ((EQ STATE 'VERSION)
		  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T)
			STATE 'DOTTED))
		 (T (LM-CHAR-ERROR STRING INDEX CHAR)))
	   (GO NEW-FIELD))
	  ((EQ CHAR #/#)
	   (COND ((NULL STATE)
		  (SETQ STATE 'VERSION))
		 ((EQ STATE T)
		  (SETQ NAME (LM-FIELD STRING FIELD-START INDEX) STATE 'VERSION))
		 ((EQ STATE 'DOTTED)
		  (AND TEM (SETQ NAME TEM))
		  (SETQ TYPE (LM-FIELD STRING FIELD-START INDEX) STATE 'VERSION))
		 (T (LM-CHAR-ERROR STRING INDEX CHAR)))
	   (GO NEW-FIELD))
	  ((OR (EQ CHAR #/;) (EQ CHAR #/>))
	   (COND ((EQ STATE T)
		  (SETQ DIRECTORY (LM-FIELD STRING FIELD-START INDEX))
		  (IF (STRING-EQUAL DIRECTORY "~")
		      (SETQ DIRECTORY :ROOT)))
		 ((EQ STATE 'DOTTED)
		  (OR TEM (LM-CHAR-ERROR STRING INDEX CHAR))
		  (SETQ DIRECTORY (LIST TEM (LM-FIELD STRING FIELD-START INDEX))))
		 ((EQ STATE 'DOUBLE-DOTTED)
		  (OR (AND TEM TEM1) (LM-CHAR-ERROR STRING INDEX CHAR))
		  (SETQ DIRECTORY (LIST TEM TEM1 (LM-FIELD STRING FIELD-START INDEX))))
		 ((EQ STATE 'DIRECTORY)
		  (LET ((FIELD (LM-FIELD STRING FIELD-START INDEX)))
		    (IF (AND (NULL DIRECTORY)
			     (EQ FIELD :UNSPECIFIC))
			(SETQ DIRECTORY :ROOT)
			(SETQ DIRECTORY
			      (NCONC DIRECTORY (LIST FIELD))))))
		 (T (LM-CHAR-ERROR STRING INDEX CHAR)))
	   (SETQ STATE NIL))
	  ((EQ STATE T)
	   (SETQ DEVICE (SEND SELF :PARSE-DEVICE-SPEC (LM-FIELD STRING FIELD-START INDEX))
		 STATE NIL))
	  (T (LM-CHAR-ERROR STRING INDEX CHAR)))
    (GO SKIP)
  NEW-FIELD
    (SETQ FIELD-START (1+ INDEX))
  SKIP
    (AND (EQ CHAR 'DONE)
	 (RETURN DEVICE DIRECTORY NAME TYPE VERSION))))

))

; From file LMPARS.LISP KANSAS:<L.FILE> OZ: (112)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; LMPARS  "

(DEFUN LM-CHAR-ERROR (STRING INDEX CHAR)
  (IF (EQ CHAR 'DONE)
      (PATHNAME-ERROR INDEX STRING "Unexpected end of string")
    (PATHNAME-ERROR INDEX STRING "Unexpected character (~:C)" CHAR)))

(DEFUN LM-FIELD (STRING &OPTIONAL (START 0) END VERSION-P DEVICE-P
			&AUX SIZE ARR CHAR)
  (DECLARE (IGNORE DEVICE-P))
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
  (SETQ SIZE (- END START))
  (IF (ZEROP SIZE)
      :UNSPECIFIC
    (SETQ ARR (MAKE-STRING SIZE))
    (DO ((I START (1+ I))
	 (SI 0))
	(( I END)
	 (OR (= SI SIZE)
	     (SETQ ARR (ADJUST-ARRAY-SIZE ARR SI))))
      (UNLESS (MEMQ (SETQ CHAR (CHAR STRING I)) '(#// #/))
	(AND ( CHAR #o200)
	     ( CHAR #/TAB)
	     (LM-CHAR-ERROR STRING I CHAR))
	(ASET (CHAR-UPCASE CHAR) ARR SI)
	(INCF SI)))
    (COND ((STRING-EQUAL ARR "*") :WILD)
	  ((NOT VERSION-P) ARR)
	  ((NUMERIC-P ARR NIL T))
	  ((CDR (SI:ASSOC-EQUAL ARR '((">" . :NEWEST) ("<" . :OLDEST)))))
	  (T (FERROR 'PATHNAME-PARSE-ERROR "Invalid version spec ~S in ~S" ARR STRING)))))

;;; Like LM-FIELD, but doesn't "unquotify", or know about versions.
(DEFUN LM-SPEC (SPEC &AUX LENGTH UPCASE-FLAG CHAR)
  (COND ((STRINGP SPEC)
	 (DOTIMES (I (SETQ LENGTH (LENGTH SPEC)))
	   (AND (> (SETQ CHAR (CHAR SPEC I)) #o177)
		( CHAR #/TAB)
		(LM-CHAR-ERROR SPEC I CHAR))
	   (IF (LOWER-CASE-P CHAR) (SETQ UPCASE-FLAG T)))
	 (COND ((ZEROP LENGTH) :UNSPECIFIC)
	       ((STRING-EQUAL SPEC "*") :WILD)
	       (UPCASE-FLAG (STRING-UPCASE SPEC))
	       (T SPEC)))
	(T SPEC)))
))

; From file LMPARS.LISP KANSAS:<L.FILE> OZ: (112)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE; LMPARS  "

(DEFUN LM-PRINT-COMPONENT (SPEC STREAM &OPTIONAL VERSION-P &AUX TEM)
  (COND ((EQ SPEC :WILD) (SEND STREAM :TYO #/*))
	((NUMBERP SPEC)
	 (LET ((*PRINT-BASE* 10.) (*NOPOINT T) (*PRINT-RADIX* NIL))
	   (SI::PRINT-FIXNUM SPEC STREAM)))
	(VERSION-P
	 (COND ((SETQ TEM (CDR (ASSQ SPEC '((:NEWEST . #/>) (:OLDEST . #/<)))))
		(SEND STREAM :TYO TEM))
	       (T (FERROR "Attempt to print ~S, which is not a valid version." SPEC))))
	((STRINGP SPEC)
	 (DOTIMES (I (ARRAY-ACTIVE-LENGTH SPEC))
	   (AND (MEMQ (SETQ TEM (CHAR SPEC I)) '(#/SP #/TAB
						 #/. #/: #/; #/# #// #/> #/<))
		(SEND STREAM :TYO #//))
	   (SEND STREAM :TYO TEM)))
	(T (FERROR "Attempt to print ~S, which is not a valid component." SPEC))))

))

; From file QMISC.LISP KANSAS:<L.SYS> OZ: (658)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN FIND-CALLERS-OF-SYMBOLS (SYMBOL PKG FUNCTION
				&OPTIONAL (INHERITORS T) (INHERITED T))
  "This is the main driving function for WHO-CALLS and friends.
Looks at all symbols in PKG and USErs (if INHERITORS is T)
and the ones it USEs (if INHERITED is T).
If PKG is NIL, looks at all packages.
Looks at each symbol's function definition and if it
refers to SYMBOL calls FUNCTION with the function name, the symbol used,
and the type of use (:VARIABLE, :FUNCTION, :MISC-FUNCTION,
 :CONSTANT, :UNBOUND-FUNCTION, :FLAVOR,
 or NIL if used in an unknown way in an interpreted function.)
SYMBOL can be a single symbol or a list of symbols.
The symbol :UNBOUND-FUNCTION is treated specially."
  (DECLARE (SPECIAL SYMBOL FUNCTION))
  ;; Sorting first, in order of function definitions, didn't help much when
  ;; tried in the previous generation of this function.
  (WHEN PKG (SETQ PKG (PKG-FIND-PACKAGE PKG)))
  (CHECK-ARG SYMBOL
	     (OR (SYMBOLP SYMBOL)
		 (LOOP FOR SYM IN SYMBOL ALWAYS (SYMBOLP SYM)))
	     "a symbol or a list of symbols")
  (IF (SYMBOLP SYMBOL)
      (SETQ SYMBOL (ADD-SYMBOLS-OPTIMIZED-INTO SYMBOL (LIST SYMBOL)))
    (DOLIST (SYM SYMBOL)
      (SETQ SYMBOL (ADD-SYMBOLS-OPTIMIZED-INTO SYM SYMBOL))))
  (COND (PKG
	 (MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX PKG INHERITED)
	 (AND INHERITORS
	      (DOLIST (P (PACKAGE-USED-BY-LIST PKG))
		(MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX P NIL))))
	(T (DOLIST (P *ALL-PACKAGES*)
	     (MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX P NIL))))
  NIL)

))

; From file QMISC.LISP KANSAS:<L.SYS> OZ: (658)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(defselect (string-io string-io-default-handler)
  (:tyi (&optional eof)
	(if (< *string-io-index* *string-io-limit*)
	    (prog1 (global:aref *string-io-string* *string-io-index*)
		   (incf *string-io-index*))
	  (and eof (ferror 'sys:end-of-file-1 "End of file on ~S." *string-io-stream*))))
  (:read-char (&optional (eof-error-p t) eof-value)
    (if (< *string-io-index* *string-io-limit*)
	(prog1 (char *string-io-string* *string-io-index*)
	       (incf *string-io-index*))
      (if eof-error-p
	  (ferror 'sys:end-of-file-1 "End of file on ~S." *string-io-stream*)
	eof-value)))
  ((:untyi :unread-char) (ignore)
   (if (minusp (decf *string-io-index*))
       (error "Attempt ~S past beginning -- ~S" :unread-char 'string-io)))
  ((:write-char :tyo) (ch)
   (string-io-add-character ch))
  (:string-out (string &optional (start 0) end)
    (or end (setq end (length string)))
    (string-io-add-line string start end))
  (:line-out (string &optional (start 0) end)
    (or end (setq end (length string)))
    (string-io-add-line string start end)
    (string-io-add-character #/Newline))
  (:fresh-line ()
    (and (plusp *string-io-index*)
	 ( (char *string-io-string* *string-io-index*) #/Newline)
	 (string-io-add-character #/Newline)))
  (:read-pointer ()
    *string-io-index*)
  (:set-pointer (ptr)
    (and (neq *string-io-direction* :in)
	 (< ptr *string-io-limit*)
	 (error "Attempt to ~S beyond end of string -- ~S" :set-pointer 'string-io))
    (setq *string-io-index* ptr))
  (:untyo-mark ()
    *string-io-index*)
  (:untyo (mark)
    (setq *string-io-index* mark))
  (:read-cursorpos (&optional (units :pixel))
    (string-io-confirm-movement-units units)
    (let ((string-io-return-index
	    (string-reverse-search-char #/Newline *string-io-string* *string-io-index*)))
      (if string-io-return-index
	  (- *string-io-index* string-io-return-index)
	*string-io-index*)))
  (:increment-cursorpos (x ignore &optional (units :pixel))
    (string-io-confirm-movement-units units)
    (dotimes (i x) (string-io-add-character #/Space)))
  (:constructed-string ()
    ;; Don't change allocated size if we have a fill pointer!
    (if (array-has-fill-pointer-p *string-io-string*)
	(setf (fill-pointer *string-io-string*) *string-io-index*)
      (setq *string-io-string*
	    (adjust-array-size *string-io-string* *string-io-index*)))))

))

(setf (documentation 'listarray 'function)
  "Return a list of the elements of ARRAY, up to index LIMIT.
If LIMIT is NIL, the array size is used; for one-dimensional arrays,
the fill pointer is used if there is one.
Uses GLOBAL:AREF, so will get fixnums out of strings.")

; From file QMISC.LISP KANSAS:<L.SYS> OZ: (658)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN MAKUNBOUND-IN-CLOSURE (CLOSURE PTR &AUX PTR1)
  "Make the symbol or value cell locative PTR unbound in CLOSURE.
More precisely, the binding which is visible within CLOSURE is made unbound.
If CLOSURE does not contain a binding for it, the current binding is made unbound."
  (CHECK-TYPE CLOSURE (OR CLOSURE ENTITY))
  (SETQ PTR1 (ETYPECASE PTR
	       (SYMBOL (LOCF (SYMBOL-VALUE PTR)))
	       (LOCATIVE PTR)))
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (IF (SYMBOLP PTR)
	   (MAKUNBOUND PTR)
	 (LOCATION-MAKUNBOUND PTR)))
    (IF (EQ (CAR L) PTR1)
	(RETURN (LOCATION-MAKUNBOUND (CADR L)))))
  NIL)

))

; From file EVAL.LISP OZ:<MLY.TEM> OZ: (1)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defprop invalid-function t :error-reporter)
(defmacro apply-lambda-bindvar-1 (varloc valloc vars-env)
  `(progn
     (unless (car ,vars-env)
       (with-stack-list (tem1 t)
	 (setf (car ,vars-env) tem1)))
     (%push ,varloc)
     (%push ,valloc)
     ;; Modify cdr-code of last word pushed, to terminate the list.
     ;; Also modify the value, which was pushed as a locative, to be an EVCP.
     (with-stack-list (tem1 nil)
       (%p-dpb-offset dtp-external-value-cell-pointer %%q-data-type tem1 -1)
       (%p-dpb-offset cdr-nil %%q-cdr-code tem1 -1)
       (%p-dpb-offset cdr-next %%q-cdr-code tem1 -2)
       (%p-dpb-offset cdr-next %%q-cdr-code tem1 -3))))

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

; From file COMF.LISP KANSAS:<L.ZWEI> OZ: (100)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-WHERE-AM-I "Print various things about where the point is.
Print the X and Y positions, the octal code for the following character,
the current line number and its percentage of the total file size.
If there is a region, the number of lines in it is printed.
Fast Where Am I prints a subset of this information faster." (KM)
  (REDISPLAY *WINDOW* :POINT NIL NIL T)
  (LET ((POINT (POINT))
	(FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (LET ((POINT-LINES (1- (COUNT-LINES FIRST-BP POINT)))
	  (INTERVAL-LINES (1- (COUNT-LINES FIRST-BP LAST-BP)))
	  (AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
	  (BP-IND (BP-INDENTATION POINT))
	  (SW (FONT-SPACE-WIDTH)))
      (FORMAT *QUERY-IO* "~&X=[~D chars|~D pixels|~:[~S~;~D~] columns] Y=~D ~
			  ~@[Char=#o~O ~]Line=~D(~D%)"
	      (BP-INDEX POINT)
	      BP-IND
	      (ZEROP (\ BP-IND SW))
	      (IF (ZEROP (\ BP-IND SW))
		  (TRUNCATE BP-IND SW)
		  (// (FLOAT BP-IND) SW))
	      (FIND-BP-IN-WINDOW *WINDOW* POINT)
	      (AND (NOT AT-END-P) (BP-CHAR POINT))
	      POINT-LINES
	      (IF (ZEROP INTERVAL-LINES)
		  0
		  (TRUNCATE (* 100. POINT-LINES) INTERVAL-LINES)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
	 (FORMAT *QUERY-IO* " Region has ")
	 (IF (EQ (BP-LINE BP1) (BP-LINE BP2))
	     (FORMAT *QUERY-IO* "~D character~:P." (- (BP-INDEX BP2) (BP-INDEX BP1)))
	     (FORMAT *QUERY-IO* "~D line~:P." (1- (COUNT-LINES BP1 BP2 T))))))
  DIS-NONE)

(DEFCOM COM-FAST-WHERE-AM-I "Quickly print various things about where the point is.
Print the X and Y positions, and the octal code for the following character.
If there is a region, the number of lines in it is printed.
Where Am I prints the same things and more." (KM)
  (REDISPLAY *WINDOW* :POINT NIL NIL T)
  (LET ((POINT (POINT)))
    (LET ((AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
	  (BP-IND (BP-INDENTATION POINT))
	  (SW (FONT-SPACE-WIDTH)))
      (FORMAT *QUERY-IO* "~&X=[~D chars|~D pixels|~:[~S~;~D~] columns] Y=~D~@[ Char=#o~O~]"
	      (BP-INDEX POINT)
	      BP-IND
	      (ZEROP (\ BP-IND SW))
	      (IF (ZEROP (\ BP-IND SW))
		  (TRUNCATE BP-IND SW)
		  (// (FLOAT BP-IND) SW))
	      (FIND-BP-IN-WINDOW *WINDOW* POINT)
	      (AND (NOT AT-END-P) (BP-CHAR POINT)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
	 (FORMAT *QUERY-IO* " Region has ")
	 (IF (EQ (BP-LINE BP1) (BP-LINE BP2))
	     (FORMAT *QUERY-IO* "~D character~:P." (- (BP-INDEX BP2) (BP-INDEX BP1)))
	     (FORMAT *QUERY-IO* "~D line~:P." (1- (COUNT-LINES BP1 BP2 T))))))
  DIS-NONE)
))

; From file DIRED.LISP KANSAS:<L.ZWEI> OZ: (306)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFCOM COM-DIRED-SUBDIRECTORY "Insert or remove the files of this subdirectory.
The files in the subdirectory mentioned on this line
are inserted into the DIRED buffer underneath this line.
You can then delete them, rename them, etc.
The subdirectory files are indented one additional space.
If the subdirectory contents are already present in the DIRED buffer,
this command offers to remove them from the buffer.
Removing them from the buffer does not delete the files!
It only makes DIRED stop operating on them." ()
  (LET ((LINE (BP-LINE (POINT))))
    (IF (NOT (GETF (LINE-PLIST LINE) :DIRECTORY))
	(BARF "~A is not a directory" (GETF (LINE-PLIST LINE) :PATHNAME))
      (DIRED-OPEN-LINE-SUBDIRECTORY (BP-LINE (POINT)))
      DIS-TEXT)))

))

; From file TYPES.LISP KANSAS:<L.SYS> OZ: (71)
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun clear-cached-subtype-info (type)
  (when (variable-boundp *subtypep-hash-table*)
    (if type
	(maphash #'(lambda (key entry)
		     (when (memq type (subtypep-hash-table-element-dependencies entry))
		       (remhash key *subtypep-hash-table*)))
		 *subtypep-hash-table*)
      (clrhash *subtypep-hash-table*)))
  (when (variable-boundp *array-element-type-hash-table*)
    (if type
	(maphash #'(lambda (key entry)
		     (when (memq type (cdr entry))
		       (remhash key *array-element-type-hash-table*)))
		 *array-element-type-hash-table*)
      (clrhash *array-element-type-hash-table*))))

(clear-cached-subtype-info nil)

(defsignal-explicit invalid-type-specifier (ferror) (typespec string)
  :property-list (list :type-specifier typespec)
  :format-string string
  :format-args (list typespec))

))

; From file TYPES.LISP KANSAS:<L.SYS> OZ: (71)
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun typep (object &optional (type nil type-specified-p))
  "T if OBJECT fits the data type specifier TYPE.
An obsolete mode of use is with one argument;
then the value is a type specifier describing OBJECT."
  (declare (arglist object type))
  (let (tem structure-desc dtp
	(type1 (if (consp type) (car type) type)))
    (cond ((not type-specified-p)
	   (setq dtp (%data-type object))
	   ;; Cannot use TYPE-OF, since we must
	   ;; for back-compatibility return keywords.
	   (cond ((= dtp dtp-instance)
		  (%p-contents-offset
		    (instance-flavor object)
		    %instance-descriptor-typename))
		 ((= dtp dtp-array-pointer)
		  (cond ((named-structure-p object))
			((stringp object) :string)
			(t :array)))
		 ((= dtp dtp-entity)
		  (class-symbol object))
		 ((= dtp dtp-extended-number) 
		  (select (%p-ldb-offset %%header-type-field object 0)
		    (%header-type-flonum :flonum)
		    (%header-type-bignum :bignum)
		    (%header-type-rational :rational)
		    (%header-type-complex :complex)
		    (otherwise :random)))
		 ((cdr (assq dtp typep-one-arg-alist)))
		 (t :random)))
	  ((setq dtp (or (rassq type type-of-alist) (rassq type typep-one-arg-alist)))
	   (= (%data-type object) (car dtp)))
	  ((setq tem (get type1 'type-predicate))
	   (if (atom type)
	       (funcall tem object)
	     (apply tem object (cdr type))))
	  ((setq tem (get type1 'type-alias-for))
	   (with-stack-list* (tem tem (cdr type))
	     (if (atom type)
		 (typep object (car tem))
	         (typep object tem))))
	  ((setq tem (get type1 'type-expander))
	   (typep object (apply tem (if (atom type) nil (cdr type)))))
	  ((get type1 'si:flavor)
	   (typep-structure-or-flavor
	     object
	     (dont-optimize (flavor-name (get-flavor-tracing-aliases type1)))))
	  ((or (and (setq structure-desc (get type1 'si::defstruct-description))
		    (defstruct-description-named-p structure-desc))
	       (get type1 'defstruct-named-p))
	   (typep-structure-or-flavor object type1))
	  ((and (symbolp type1) (fboundp 'class-symbolp) (class-symbolp type1))
	   (and (entityp object)
		(subclass-of-class-symbol-p (class object) type1)))
	  (t (typep object (cerror t nil 'invalid-type-specifier
				   "~S is not a valid type specifier" type))))))

))

; From file TYPES.LISP KANSAS:<L.SYS> OZ: (71)
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun compilation-subtypep (x y)
  (declare (values known-to-be-subtype known-whether-is-subtype))
  (multiple-value-bind (known-to-be-subtype known-whether-is-subtype)
      (let ((*use-subtypep-cache-p* nil))
	(subtypep-1 x y ()))
    (values known-to-be-subtype known-whether-is-subtype)))

(defun subtypep-1 (x y dependencies
		   &aux (known-to-be-subtype nil) (known-whether-is-subtype nil) tem elt
		        (cachep *use-subtypep-cache-p*))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (cond ((and (symbolp y) (setq tem (get y 'subtypes)))
	 (if (memq (if (atom x) x (car x)) tem)
	     (return-from subtypep-1 (values t t dependencies))))
	((memq y '(t nil)) (return-from subtypep-1 (values y t dependencies)))
	((and cachep
	      (setq tem (gethash (cons x y) *subtypep-hash-table*))
	      (return-from subtypep-1
		(values (subtypep-hash-table-element-subtypep tem)
			(subtypep-hash-table-element-knownp tem)
			(subtypep-hash-table-element-dependencies tem))))))
  (macrolet ((record-dependency (x)
	       `(and cachep
		     (not (memq ,x *standard-system-type-specifiers*))
		     (if (cli:listp ,x)
			 (dolist (x ,x) (pushnew x dependencies :test #'eq))
		       (pushnew ,x dependencies :test #'eq)))))
   (labels ((record-atomic-dependency (x)
 	      (and cachep
		   (if (atom x)
		       (unless (memq x *standard-system-type-specifiers*)
			 (pushnew x dependencies :test #'eq))
		     (case (car x)
		       ((and or not)
			(dolist (y (cdr x)) (record-atomic-dependency y)))
		       ((satisfies cli:member global:member))
		       ((not (memq x *standard-system-type-specifiers*))
			(pushnew (car x) dependencies :test #'eq)))))))
    (let ((x x) (y y))
      (multiple-value-setq (x dependencies) (type-canonicalize x cachep dependencies))
      (multiple-value-setq (y dependencies) (type-canonicalize y cachep dependencies))
      (cond ((or (null x) (eq y t) (equal x y))
	     (setq known-to-be-subtype t
		   known-whether-is-subtype t))
	    ((eq (car-safe y) 'or)		;(subtypep foo '(or ...))
	     (setq ;; known-to-be-subtype nil
		   known-whether-is-subtype t)
	     (dolist (y (cdr y))
	       (subtypep-3 (t1 t2) x y
		 (if t1 (return (setq known-to-be-subtype t
				      known-whether-is-subtype t))
		   (setq known-whether-is-subtype (and known-whether-is-subtype t2))))))
	    ((eq (car-safe y) 'and)		;(subtypep foo '(and ...))
	     (setq known-to-be-subtype t
		   known-whether-is-subtype t)
	     (dolist (y (cdr y))
	       (subtypep-3 (t1 t2) x y
		 (if t2
		     (setq known-to-be-subtype (and known-to-be-subtype t1))
		   (return (setq known-to-be-subtype nil
				 known-whether-is-subtype nil))))))
	    ((eq (car-safe y) 'not)		;(subtypep foo '(not ...))
	     (multiple-value-bind (t1 t2 tem) (disjoint-typep x (cadr y) dependencies)
	       (setq dependencies tem)
	       (setq known-to-be-subtype t1
		     known-whether-is-subtype (or t2
						  (subtypep-2 x (cadr y))
						  (subtypep-2 (cadr y) x)))))
	    ((eq (car-safe x) 'cli:member)	;(subtypep '(member ...) bar)
	     (setq known-to-be-subtype (loop for z in (cdr x) always (typep z y))
		   known-whether-is-subtype t))
	    ((eq (car-safe x) 'and)		;(subtypep '(and ...) bar)
	     (let ((knownp t))
	       (dolist (x (cdr x))
		 (subtypep-3 (t1 t2) x y
		   (when t1
		     (setq known-to-be-subtype t
			   known-whether-is-subtype t)
		     (return nil))
		   (setq knownp (and knownp t2))))
	       (setq known-whether-is-subtype knownp)))
	    ((eq (car-safe x) 'or)		;(subtypep '(or ...) bar)
	     (let ((val t))
	       (dolist (x (cdr x))
		 (subtypep-3 (t1 t2) x y
		   (unless t2
		     (return nil))
		   (setq val (and val t1))))
	       (setq known-to-be-subtype val
		     known-whether-is-subtype t)))
	    ((eq (car-safe x) 'not)		;(subtypep '(not ...) bar)
	     (multiple-value-bind (nil t2 tem) (disjoint-typep (cadr x) y dependencies)
	       (setq dependencies tem)
	       (setq known-whether-is-subtype (or t2
						  (subtypep-2 (cadr x) y)
						  (subtypep-2 y (cadr x))))))
	    ((eq (car-safe y) 'cli:member))	;(subtypep foo '(member ...))
	    ((eq (car-safe y) 'satisfies))	;(subtypep foo '(satisfies ...))
	    ((eq (car-safe x) 'satisfies))	;(subtypep '(satisfies ...) bar)
	    ((atom y)
	     (setq known-to-be-subtype (atom-subtypep (if (atom x) x (car x)) y)
		   known-whether-is-subtype t))
	    ((atom x)
	     (setq known-whether-is-subtype t))	 
	    (t
	      (unless (setq tem (atom-subtypep (car x) (car y)))
		(setq known-whether-is-subtype t))
	      (if (and tem (setq tem (get (car y) 'subtypep-predicate)))
		  (multiple-value-setq (known-to-be-subtype known-whether-is-subtype dependencies)
		    (funcall tem x y dependencies)))))
      (setq known-whether-is-subtype (not (not known-whether-is-subtype)))
      (setq known-to-be-subtype (not (not known-to-be-subtype)))
      (when cachep
	(setq elt (let ((default-cons-area background-cons-area))
		    (make-subtypep-hash-table-element :subtypep known-to-be-subtype
						      :knownp known-whether-is-subtype
						      :dependencies (copylist dependencies))))
	(setf (gethash (cons x y) *subtypep-hash-table*) elt))))
    (when cachep (setf (gethash (cons x y) *subtypep-hash-table*) elt)))
  (values known-to-be-subtype known-whether-is-subtype dependencies))

))

; From file TYPES.LISP KANSAS:<L.SYS> OZ: (71)
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun array-type-from-element-type (element-type)
  "Returns a symbol, such as ART-4B"
  (let ((default-cons-area background-cons-area))
    (unless (variable-boundp *array-element-type-hash-table*)
      (setq *array-element-type-hash-table* (make-hash-table :test #'equal :size 100.)))
    (unless (variable-boundp *subtypep-hash-table*)
      (setq *subtypep-hash-table* (make-hash-table :test #'equal :size 400.))))
  (let ((*use-subtypep-cache-p* t))
    (array-type-from-element-type-1 element-type)))

(defun compilation-array-type-from-element-type (element-type)
  (let ((*use-subtypep-cache-p* nil))
    (array-type-from-element-type-1 element-type)))

(defun array-type-from-element-type-1 (element-type)
  (cond	((cdr (assoc-equal element-type array-element-type-alist)))
	((and *use-subtypep-cache-p*
	      (car (gethash element-type *array-element-type-hash-table*))))
	(t
	 (multiple-value-bind (canon dependencies)
	     (type-canonicalize element-type *use-subtypep-cache-p* nil)
	   (let ((value (or (cdr (assoc-equal canon array-element-type-alist))
			    (cond ((subtypep-1 canon 'fixnum dependencies)
				   (cond ((subtypep-1 canon 'bit dependencies)
					  'art-1b)	;common case
					 ((subtypep-1 canon '(mod #o10) dependencies)
					  (if (subtypep-1 canon '(mod 4) dependencies)
					      'art-2b 'art-4b))
					 ((subtypep-1 canon '(mod #o200000) dependencies)
					  (if (subtypep-1 canon '(mod #o400) dependencies)
					      'art-8b 'art-16B))
					 ((subtypep-1 canon '(signed-byte #o20) dependencies)
					  'art-half-fix)
					 (t 'art-q)))
				  ((subtypep-1 canon 'cli:character dependencies)
				   (cond ((subtypep-1 canon 'string-char dependencies)
					  'art-string)
					 ((subtypep-1 canon 'fat-char dependencies)
					  'art-fat-string)
					 (t 'art-q)))
				  ((subtypep-1 canon 'float dependencies) 'art-float)
				  ((subtypep-1 canon 'complex dependencies)
				   (if (subtypep-1 canon '(complex float) dependencies)
				       'art-complex-float 'art-complex))
				  (t 'art-q)))))
	     (prog1 value
		    (when *use-subtypep-cache-p*
		      (setq value (cons-in-area value dependencies background-cons-area))
		      (setf (gethash canon *array-element-type-hash-table*) value)
		      (setf (gethash element-type *array-element-type-hash-table*) value))))))))
))