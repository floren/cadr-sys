;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Written 21-May-84 18:53:53 by Mly,
;;; while running on Lisp Machine Eight from band 2
;;; with System 98.48, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309, ZM MIT.

;;; (typep foo 'complex) improvements
;;; realp, (typep foo 'real)
;;; zmacs: c-x j doesn't barf if register points to different buffer
;;; peephole optimizer doesn't hang in infinite loops when hacking infinite loops
;;; lexically interpreted do bug.
;;; lexically interpreted setq
;;; dired: @ on directory doesn't bash display of line
;;; make-string optimizer
;;; unsetfable stuff
;;; defstruct-define-type function-parent declaration bug
;;; random numeric optimizations
;;; (eval-when (load) (export ...)), etc bug
;;; pushnew optimizer


(globalize (intern "INSERT-BINDING-IN-CLOSURE" "EH") "SYS")
(globalize (intern "DELETE-BINDING-FROM-CLOSURE" "EH") "SYS")

; From file FONT.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; FONT  "

(DEFUN INPUT-FONT-NAME (USE-PREVIOUS-P &OPTIONAL (PROMPT "Font ID: ") &AUX NUM)
  "Read a font number from the user.
The user can type a letter (A signifies font 0), an  and a font name,
or mouse left on a character, or mouse right and get a menu of fonts.
USE-PREVIOUS-P means if previous command called this function,
 just return the same number we returned for that command."
  (SETQ *CURRENT-COMMAND-TYPE* 'FONT-CHANGE)
  (IF (AND USE-PREVIOUS-P (EQ *LAST-COMMAND-TYPE* 'FONT-CHANGE))
      *SAVE-FONT-NUM*
    (FORMAT *QUERY-IO* PROMPT)
    (DO (CH ;(IF (AND *NUMERIC-ARG-P* ( 1 *NUMERIC-ARG* 26.)) (+ *NUMERIC-ARG* #/A -1))
	 (ALIST (WINDOW-FONT-ALIST *WINDOW*)))
	(NIL)
      (LET-GLOBALLY ((*GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
		       (IF (NULL ALIST)
			   "Use font of character pointed at."
			 "Use font of character pointed at, click right for menu of fonts.")))
	(UNLESS CH
	  (TYPEIN-LINE-ACTIVATE
	    (SETQ CH (CHAR-UPCASE (send *STANDARD-INPUT* ':MOUSE-OR-KBD-TYI))))))
      (COND ((= CH #/CONTROL-G)
	     (BARF))
	    ((= CH #/)
	     (SETQ NUM (INPUT-FONT-NAME-FROM-MINI-BUFFER))
	     (RETURN NIL))
	    ((= CH #/MOUSE-1-1)
	     (COND ((SETQ CH (MOUSE-CHAR *WINDOW*))
		    (SETQ NUM (LDB %%CH-FONT CH))
		    (RETURN NIL))))
	    ((= CH #/MOUSE-3-1)
	     (COND ((NULL ALIST)
		    (BEEP))
		   ((SETQ CH (TV:MENU-CHOOSE ALIST))
		    (DO ((I 0 (1+ I))		;Have the font itself, but want the number
			 (L (WINDOW-FONT-ALIST *WINDOW*) (CDR L)))
			((EQ (CDAR L) CH) (SETQ NUM I)))
		    (RETURN NIL))))
	    (( #/A CH #/Z)
	     (SETQ NUM (- CH #/A))
	     (RETURN NIL))
	    ((OR (= CH #/HELP) (= CH #/?))
	     (FORMAT *QUERY-IO* "~&Type a font letter, ~
				  or altmode to enter a new font in a mini-buffer, ~@
				  or mouse a character left for its font~:[~;, ~
				  or mouse-right for a menu~].~%" ALIST)
	     (FORMAT *QUERY-IO* PROMPT)
	     (SETQ CH NIL))
	    (T
	     (BEEP) (SETQ CH NIL))))
    (FORMAT *QUERY-IO* "~C (~A)"
	    (+ NUM #/A) (CAR (NTH NUM (WINDOW-FONT-ALIST *WINDOW*))))
    (SETQ *SAVE-FONT-NUM* NUM)))

))


; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (complex type-predicate) (object &optional type)
  (and (complexp object)
       (or (memq type '(nil *))
	   (typep (complex-real-part object) type))))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (complex type-optimizer) (expression &optional (type '*))
  (let ((object (cadr expression)))
    (if (eq type '*) `(complexp ,object)
      (once-only (object)
	`(and (complexp ,object)
	      (typep (complex-real-part ,object) ',type))))))

))

; From file RAT.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defsubst realpart (x)
  "Return the real part of a complex number.  The real part of a real number is itself."
  (check-type x number)
  (if (complexp x)
      (complex-real-part x)
    x))

))

; From file RAT.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RAT  "

(defsubst imagpart (x)
  "Return the imaginary part of a complex number, or 0 if given a real number."
  (check-type number x)
  (if (complexp x)
      (complex-imag-part x)
    (- x x)))

))

; From file TYPES.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; TYPES  "

(defun (:property complex type-predicate) (object &optional type)
  (and (complexp object)
       (or (memq type '(nil *))
	   (typep (complex-real-part object) type))))

))

; From file COMD.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-JUMP-TO-SAVED-POSITION "Restore a saved position from a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
    (LET ((PT (GET Q-REG 'POINT)))
      (WHEN (NULL PT)
	(BARF "The register ~A doesn't point anywhere." Q-REG))
      (MOVE-BP (POINT) (CAR PT))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)))
  DIS-BPS)

))

; From file QCPEEP.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCPEEP  "

(defun peep-one-spot (index)
   (prog function
	 (x x-ptr y z z-ptr tag-skipped-flag
	    y-just-sets-inds
	    y-branch-target-index)
	 (incf peep-one-spot)
	 (or (consp (setq y (aref peep-code-array index))) (return (1+ index)))
	 (when (eq (car y) 'comment)
	   (setf (aref peep-code-array index) nil)
	   (return (1+ index)))
      y-reloaded-reload-z
	 (setq z-ptr (1+ index))

      reload-z
	 ;; Find next non-deleted object (instruction or tag).
	 (do ((length (array-active-length peep-code-array)))
	     (())
	   (if ( z-ptr length)
	       (return (setq tag-skipped-flag t)))
	   (if (setq z (aref peep-code-array z-ptr))
	       (cond ((atom z) (setq tag-skipped-flag t))
		     ((memq (car z) '(restart-tag param))
		      (setq tag-skipped-flag t))
		     ((memq (car z) '(comment no-drop-through)))
		     (t (return))))
	   (incf z-ptr))

      y-reloaded
	 ;; There are a few optimizations that can go on even across a tag.
	 ;; They are the ones that can delete the first insn, not the second.

	 ;; Specifically, delete the first instruction
	 ;; if all it does is set the indicators
	 ;; but what follows does not use them.
	 (setq y-just-sets-inds nil x nil)
	 (and (or (and (memq (car y) '(move car cdr cadr cddr caar cdar))
		       (neq (caddr y) 'pdl-pop)
		       (memq (cadr y) '(d-inds 0 d-ignore)))
		  (member y '((misc d-inds false) (misc d-ignore false)
			      (misc d-inds true) (misc d-ignore true)
			      (move d-pdl pdl-pop)
			      (move d-pdl (lpdl 77)))))
	      (setq y-just-sets-inds t)
	      (not (peep-indicators-used-p z-ptr))
	      (progn (peep-trace set-inds-not-used)
		     (go delete-y)))

	 (when (eq (car y) 'branch)
	   ;; Conditional branch followed by unconditional branch to same place.
	   (and (eq (car z) 'branch)
		(eq (cadr z) 'always)
		(not (fourth y))
		(not (fourth z))
		(eq (fifth y) (fifth z))
		(progn (peep-trace two-branches-to-same-place)
		       (go delete-y)))

	   ;; Branch to .+1, and not popping.
	   (when (and (eq index (setq y-branch-target-index
				      (peep-previous-insn-index-skipping-tags
					(peep-tag-index (fifth y)))))
		      (null (fourth y)))
	     (peep-trace branch-.+1)
	     (go delete-y))

	   (when (eq (cadr y) 'always)
	     ;; See if the code before the branch target
	     ;; matches the code before the branch.
	     (do ((prev-ptr (peep-previous-insn-index index)
			    
			    (peep-previous-insn-index prev-ptr))
		  (target-ptr y-branch-target-index
			      (peep-previous-insn-index target-ptr))
		  (prev-match index prev-ptr)
		  (target-match nil target-ptr))
		 ((or (null prev-ptr)
		      (null target-ptr)
		      (equal (aref peep-code-array prev-ptr) '(no-drop-through))
		      (not (equal (aref peep-code-array prev-ptr)
				  (aref peep-code-array target-ptr))))
		  (unless (= index prev-match)
		    (setq x (aref peep-code-array prev-match))
		    (peep-trace matching-code-precedes-branch-and-target
				(aref peep-code-array
				      (peep-next-insn-index-skipping-tags
					(peep-tag-index (fifth y)))))
		    (let ((oinsn (aref peep-code-array prev-match)))
		      (if (eq (car oinsn) 'branch)
			  (peep-delete-tag-ref (fifth oinsn))))
		    (let ((ntag
			    (peep-updating-index prev-match
			      (peep-updating-index index
				(peep-find-or-insert-tag target-match)))))
		      (setf (aref peep-code-array prev-match)
			    `(branch always nil nil ,ntag))
		      (peep-create-tag-ref ntag))
		    ;; Start deleting this branch
		    ;; so that peep-locally doesn't do this optimization again.
		    (setf (aref peep-code-array index) nil)
		    ;; Perhaps this new branch insn causes optimizations
		    ;; in branches branching to tags that precede it.
		    (peep-previous-insn-index-redo-tags prev-match
							(aref peep-code-array prev-match))
		    (peep-locally (1+ prev-match))
		    (peep-locally prev-match)
		    ;; Finish deleting this branch.
		    (go delete-y)))))

	   ;; Various other optimizations of branch insns
	   ;; that involve looking at the insn following the target tag.
	   (let (target target-ptr x-ptr)
	     (setq target-ptr
		   (peep-next-insn-index-skipping-tags
		     (peep-tag-index (fifth y))))
	     (when target-ptr
	       (setq target (aref peep-code-array target-ptr))
	       (cond ((and (eq (car target) 'branch)
			   (neq target y)	;degenerate case of branch to self
			   (or (eq (cadr target) 'always)
			       (and (eq (cadr target) (cadr y))
				    (eq (caddr target) (caddr y)))))
		      ;; Target of branch is an unconditional branch
		      ;; or a branch testing the same indicator with the same sense.
		      (peep-trace branch-to-branch target)
		      (let ((otag (fifth y)))
			(setf (fifth y) (fifth target))
			(peep-create-tag-ref (fifth y))
			(peep-delete-tag-ref otag))
		      (go y-reloaded))
		     ((and (eq (car target) 'branch)
			   (eq (cadr target) (cadr y))
			   (neq target y)
			   (not (fourth target)))
		      ;;Branch to another branch on same indicator
		      ;;but opposite sense (and not popping).
		      ;;The other branch will never branch if reached from here,
		      ;;so branch to a new tag following it.
		      (peep-trace branch-to-branch-not-taken target)
		      (peep-updating-index index
			(peep-change-branch-to-index y (1+ target-ptr)))
		      (go y-reloaded-reload-z))
		     ((and (eq (car target) 'branch)
			   (eq (cadr y) 'always)
			   (null (fourth target))
			   (eq index
			       (peep-previous-insn-index-skipping-tags
				 (peep-tag-index (fifth target)))))
		      ;; branch [branch .+1]  turns into
		      ;; a branch on the opposite condition, to one after that branch.
		      (peep-trace branch-to-branch-back target)
		      (setf (cadr y) (cadr target))
		      (setf (caddr y) (other (caddr target)))
		      (peep-can-drop-through (1+ index))
		      (peep-updating-index index
			(peep-change-branch-to-index y (1+ target-ptr)))
		      (go y-reloaded-reload-z))
		     ((and (eq (cadr y) 'always)
			   (eq (cadr target) 'd-return)
			   (memq (car target)
				 '(move car cdr caar cadr cdar cddr misc false true call0)))
		      ;; Unconditional branch to a single instruction that returns.
		      (peep-trace branch-to-return target)
		      (setf (aref peep-code-array index) target)
		      (let ((oy y))
			(setq y target)
			(peep-delete-tag-ref (fifth oy)))
		      (go y-reloaded))
		     ((and (eq (cadr target) 'd-inds)
			   (setq x-ptr (peep-previous-insn-index index))
			   (setq x (aref peep-code-array x-ptr))
			   (equal (caddr target) (caddr x))
			   (not (memq (cadr x) '(d-last d-return)))
			   (or (and (memq (car x) set-inds-from-source-insns)
				    (eq (car target) 'move))
			       (and (memq (car x) '(car cdr caar cadr cdar cddr))
				    (eq (car x) (car target)))))
		      ;; Branch to an insn that just sets the inds
		      ;; to what they already were.
		      (peep-trace branch-to-set-inds-already-set target)
		      (peep-updating-index index
			(peep-change-branch-to-index y (1+ target-ptr)))
		      (go y-reloaded-reload-z))))))

	 ;; The rest can happen only if no tag intervenes.
	 (if tag-skipped-flag (return z-ptr))

	 ;; Ok, we have instructions in Y and Z.
	 (cond ((equal y '(no-drop-through))
		;; Delete dead code following a (NO-DROP-THROUGH)
		(if (peep-instruction-p z)
		    (peep-trace dead-code))
		(go delete-z))
	       ((eq (car y) 'branch)
		(cond ((eq (cadr y) 'always)
		       (if (peep-instruction-p z)
			   (peep-trace dead-code))
		       (go delete-z))
		      ((and (eq y-branch-target-index z-ptr)
			    (not (fourth y))
			    (eq (car z) 'branch)
			    (eq (cadr z) 'always))
		       ;; branch .+2 followed by unconditional branch.
		       (peep-trace branch-across-branch)
		       (setf (caddr y) (other (caddr y)))
		       (let ((otag (fifth y)))
			 (setf (fifth y) (fifth z))
			 (setf (fifth z) otag)
			 (go delete-z))))
		(cond ((and (eq (car z) 'branch)
			    (not (fourth y))
			    (eq (cadr y) (cadr z)))
		       ;; Two branches in a row testing the same indicator.
		       (peep-trace two-branches-same-ind)
		       (if (eq (caddr y) (caddr z))
			   ;; same sense => just delete the second one.
			   (go delete-z)
			 ;; opposite sense => second one is really unconditional.
			 (setf (cadr z) 'always)
			 (setf (caddr z) nil)))
		      ;; Branch followed by move.  See if move is to d-inds and is superfluous
		      ((eq (car z) 'move)
		       (and (setq x-ptr (peep-previous-insn-index index))
			    (setq x (aref peep-code-array x-ptr))
			    (memq (car x) set-inds-from-source-insns)
			    (equal (caddr x) (caddr z))
			    (or (and (eq (cadr z) 'd-pdl)
				     ;; Can optimize branch push
				     ;; only if the branch is a branch-or-pop.
				     ;; and the previous insn is also a push or a movem.
				     (or (equal x z)
					 (and (eq (car x) 'movem)
					      (equal (caddr x) (caddr z))))
				     (fourth y))
				(and (memq (cadr z) '(d-ignore d-inds 0))
				     (not (memq (cadr x) '(d-return d-last)))))
			    (progn
			      (peep-trace move-branch-move)
			      (if (eq (cadr z) 'd-pdl)
				  (setf (fourth y) nil))
			      (go delete-z))))))
	       ((eq (car z) 'branch)
		;; check for MOVE D-INDS constant followed by branches.
		;; Decide where the branching will stop
		;; and make one branch straight there.
		;; Then back to Y-RELOADED which may delete the MOVE.
		;; Note: we know that the indicators will get used,
		;; for otherwise the MOVE would already have been deleted, above.
		(let (const)
		  (when (or (and (eq (car y) 'move)
				 (eq (cadr y) 'd-inds)
				 (setq const (caddr y))
				 (consp const)
				 (eq (car const) 'quote-vector)
				 (prog1 (eq (caadr const) 'quote)
					(setq const (cadadr const))))
			    (prog1 (member y '((misc d-inds false)
					       (misc d-inds true)))
				   (setq const (selectq (caddr y) (true t)))))
		    ;; CONST has the actual constant.
		    (peep-trace branch-testing-constant)
		    (let* ((inds-used (peep-indicators-used-p z-ptr))
			   (final-branch (aref peep-code-array inds-used))
			   (indicator (second final-branch))
			   (otag (fifth z))
			   (sense (third final-branch)))
		      (unless (and (eq (cadr z) 'always)
				   (eq (fifth z) (fifth final-branch)))
			(cond ((eq (selectq sense (true t) (false nil))
				   (funcall (selectq indicator
					      (nilind 'null)
					      (atomind 'atom))
					    const))
			       ;; The final branch will go.
			       (setf (fifth z) (fifth final-branch)))
			      (t ;; The final branchgh will not branch.
			       (if (fourth final-branch)
				   ;; Can't skip over a branch that pops.
				   (return z-ptr))
			       (peep-updating-index index
				 (setf (fifth z) (peep-find-or-insert-tag (1+ inds-used))))))
			(setf (cadr z) 'always)
			(setf (caddr z) nil)
			(peep-create-tag-ref (fifth z))
			(peep-delete-tag-ref otag)
			(go y-reloaded-reload-z)))))
		;; Check for push followed by branch-or-pop that will never branch.
		(let (const)
		  (when (and (fourth z)   ;Must be branch-or-pop
			     (or (and (eq (car y) 'move)
				      (eq (cadr y) 'd-pdl)
				      (setq const (caddr y))
				      (consp const)
				      (eq (car const) 'quote-vector)
				      (prog1 (eq (caadr const) 'quote)
					     (setq const (cadadr const))))
				 (prog1 (member y '((misc d-pdl false)
						    (misc d-pdl true)))
					(setq const (eq (caddr y) 'true))))
			     ;; Test that branch will not occur.
			     (neq (selectq (third z) (true t) (false nil))
				  (funcall (selectq (second z)
					     (nilind 'null)
					     (atomind 'atom))
					   const)))
		    (peep-trace branch-or-pop-testing-constant)
		    ;; Change the push to a d-inds, and delete the branch.
		    (setf (aref peep-code-array index)
			  (setq y
				(list* (car y) 'd-inds (cddr y))))
		    (go delete-z))))
	       ((eq (car z) 'pop)
		;; PUSH 0 or PUSH NIL followed by POP.
		(when (member y '((move d-pdl (quote-vector (quote 0)))
				  (move d-pdl (quote-vector (quote nil)))))
		  (peep-trace push-nil-or-0-then-pop)
		  (setf (aref peep-code-array index)
			(setq y
			      (if (equal y '(move d-pdl (quote-vector (quote 0))))
				  `(setzero 0 ,(caddr z))
				`(setnil 0 ,(caddr z)))))
		  (go delete-z)))
	       ((and (or (equal z '(move d-ignore pdl-pop))
			 (and (eq (car z) 'misc)
			      (eq (caddr z) 'poppdl)
			      ( (fourth z) poppdl-max)))
		     (or (equal y '(move d-ignore pdl-pop))
			 (and (eq (car y) 'misc)
			      (eq (caddr y) 'poppdl))))
		;; Two insns in a row that just pop something(s) from the pdl.
		(peep-trace two-pops)
		(let ((total-pops (+ (or (fourth y) 1) (or (fourth z) 1))))
		  (cond (( total-pops poppdl-max)
			 (setf (aref peep-code-array index)
			       (setq y `(misc d-ignore poppdl
					      ,total-pops)))
			 (go delete-z))
			(t
			 (setf (aref peep-code-array index)
			       (setq y `(misc d-ignore poppdl ,poppdl-max)))
			 (setf (aref peep-code-array z-ptr)
			       (setq z `(misc d-ignore poppdl ,(- total-pops poppdl-max))))
			 (return z-ptr)))))
	       ((eq (car z) 'move)
		(cond ((and (eq (car y) 'move)
			    (eq (cadr y) 'd-pdl)
			    (eq (caddr z) 'pdl-pop))
		       ;; Push followed by move from pdl.
		       ;; Transfer the destination into the push,
		       ;; then the pop is not needed.
		       (peep-trace push-then-move-from-pdl)
		       (setf (cadr y) (cadr z))
		       (go delete-z)))
		(and (equal z '(move d-pdl (quote-vector 'nil)))
		     (equal y '(move d-pdl (quote-vector 'nil)))
		     (progn (peep-trace two-push-nils)
			    (return z-ptr)))
		(cond ((equal (caddr y) (caddr z))
		       (cond ((and (eq (cadr z) 'd-inds)
				   (not (memq (cadr y) '(d-last d-return)))
				   (memq (car y) set-inds-from-source-insns))
			      ;; Delete MOVE D-INDS X after something that stores or fetches X.
			      (peep-trace set-inds-already-set)
			      (go delete-z))
			     ((and (eq (cadr z) 'd-pdl)
				   (eq (car y) 'pop))
			      ;; Turn POP X ? MOVE D-PDL X into MOVEM X
			      (peep-trace pop-then-push)
			      (rplaca y 'movem)
			      (go delete-z)))))))
	 (return z-ptr)

      delete-z
	 (or (memq (car z) '(no-drop-through comment))
	     (incf peep-insns-saved))
	 (setf (aref peep-code-array z-ptr) nil)
	 (when (eq (car z) 'branch)
	   (peep-can-drop-through (1+ z-ptr))
	   (peep-delete-tag-ref (fifth z)))
	 (incf z-ptr)
	 ;; If the previous instruction was a NO-DROP-THROUGH
	 ;; we may have turned the branch before it into a branch to .+1.  Check for that.
	 (if (eq (car y) 'no-drop-through)
	     (peep-locally index))
	 (go reload-z)

      delete-y
	 (setf (aref peep-code-array index) nil)
	 (when (eq (car y) 'branch)
	   (peep-can-drop-through (1+ index))
	   (peep-delete-tag-ref (fifth y)))
	 (incf peep-insns-saved)
	 (multiple-value-bind (i f)
	     (peep-previous-insn-index-redo-tags index z)
	   (setq index i)
	   (setq tag-skipped-flag (or f tag-skipped-flag)))
	 (unless index (return z-ptr))
	 (setq y (aref peep-code-array index))
	 ;; If we delete the insn following an unconditional branch,
	 ;; we may have created a branch to .+2 before it,
	 ;; so scan there.
	 (when (and (eq (car y) 'branch)
		    (eq (cadr y) 'always))
	   (peep-locally index))
	 (setq peep-queue (delq y peep-queue))
	 (go y-reloaded-reload-z)))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun do-body (oncep endtest retvals oldp varlist body &optional serial-stepping)
  (enter-block nil
    (do ()
	((and (not oncep) (eval1 endtest))
	 ;; Now evaluate the exit actions.
	 ;; The last one should return its values out of the DO.
	 (eval-body retvals))
      ;; Now execute the body.
      (tagbody-internal body)
      
      ;; Here after finishing the body to step the DO-variables.
      (and oncep (return nil))
      (cond (oldp (if (eq interpreter-function-environment t)
		      (set (car varlist) (eval (caddr varlist)))
		    (interpreter-set (car varlist) (eval1 (caddr varlist)))))
	    (serial-stepping
	     (dolist (elt varlist)
	       (and (consp elt) (cddr elt)
		    (if (eq interpreter-function-environment t)
			(set (car elt) (eval (caddr elt)))
		      (interpreter-set (car elt) (eval1 (caddr elt)))))))
	    (t (do ((vl varlist (cdr vl))
		    (vals (do ((vl varlist (cdr vl))
			       (vals nil (cons (and (consp (car vl)) (cdar vl) (cddar vl)
						    (eval1 (caddar vl)))
					       vals)))	;******* CONS *******
			      ((null vl) (nreverse vals)))
			  (cdr vals)))
		   ((null vl))
		 (cond ((and (consp (car vl)) (cdar vl) (cddar vl))
			(if (eq interpreter-function-environment t)
			    (set (caar vl) (car vals))
			  (interpreter-set (caar vl) (car vals)))))))))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun interpreter-external-value-cell (symbol &aux tem)
  (do ((tail interpreter-environment (cdr tail)))
      ((atom tail)				;assume free references are special
       (or tail (getl symbol '(special system-constant))
	   (var-not-special symbol))
       (%external-value-cell symbol))
    (and (setq tem (get-lexical-value-cell (car tail) (value-cell-location symbol)))
	 (return tem))))

))

; From file EVAL.LISP PS:<MLY.L> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; EVAL  "

(defun setq (&quote &rest symbols-and-values)
  "Given alternating variables and value expressions, sets each variable to following value.
Each variable is set before the following variable's new value is evaluated.
See also PSETQ which computes all the new values and then sets all the variables."
  (prog (val)
     l	(cond ((null symbols-and-values) (return val))
	      ((null (cdr symbols-and-values))
	       (ferror nil "Odd number of arguments to SETQ"))
	      ;; checking for setqing defconstants would make life too hard for hacking
	      ((or (memq (car symbols-and-values) '(t nil))
		   (keywordp (car symbols-and-values)))
	       (ferror nil "Setting ~A is not allowed."
		       (if (keywordp (car symbols-and-values)) "keywords"
			 (car symbols-and-values)))))
	(if (eq interpreter-function-environment t)
	    (set (car symbols-and-values)
		 (setq val (eval (cadr symbols-and-values))))
	  (interpreter-set (car symbols-and-values)
			   (setq val (eval1 (cadr symbols-and-values)))))
	(setq symbols-and-values (cddr symbols-and-values))
	(go l)))

))

; From file OPEN.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; OPEN  "

(DEFUN OPEN (FILENAME &REST KEYWORD-ARGS)
  "Open a file and return a stream.  FILENAME is a pathname or a string.
DIRECTION is :INPUT, :OUTPUT, :PROBE, :PROBE-LINK or :PROBE-DIRECTORY.
ERROR if NIL says return an error object rather than getting an error.
CHARACTERS says whether to transfer character data; :DEFAULT means do what the file says.
BYTE-SIZE defaults according to CHARACTERS; :DEFAULT says use file's byte size.
ELEMENT-TYPE is a type specifier, hacks CHARACTERS and BYTE-SIZE as appropriately.
IF-EXISTS specifies action to take on output wen file already exists.
  One of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE, :OVERWRITE, :TRUNCATE,
  :APPEND, :SUPERSEDE, or NIL.
IF-DOES-NOT-EXIST specifies action to take if file does not exist
  One of :ERROR :CREATE or NIL.
DELETED says it is ok to open deleted but not expunged files.
TEMPORARY says to open a file and mark it as temporary.
ESTIMATED-LENGTH specifies estimated length of file to be written, in bytes.
PRESERVE-DATES says do not alter the files read or write dates.
RAW disables character-set translation, if the server does that.
SUPER-IMAGE disables special treatment of #\rubout in pdp-10 servers.
SUBMIT means to submit this file as a batch job.
FLAVOR is NIL, :DIRECTORY, :LINK or file-system dependent values.
LINK-TO specifies link target, when you create a file with flavor :LINK.
Other system-specific keywords may be supported for some file systems."
  (DECLARE (ARGLIST FILENAME &KEY (DIRECTION :INPUT) (ERROR T)
		    		  (CHARACTERS T) BYTE-SIZE ELEMENT-TYPE
				  IF-EXISTS IF-DOES-NOT-EXIST
				  DELETED TEMPORARY ESTIMATED-LENGTH PRESERVE-DATES
				  RAW SUPER-IMAGE SUBMIT FLAVOR LINK-TO
			     &ALLOW-OTHER-KEYS))
  (FORCE-USER-TO-LOGIN)
  (IF (STREAMP FILENAME)
      (SETQ FILENAME (SEND FILENAME :PATHNAME)))
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME))
  (SETQ LAST-FILE-OPENED FILENAME)
  (IF (OR (NULL KEYWORD-ARGS)			;No args is good args
	  (NOT (NULL (CDR KEYWORD-ARGS))))
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ (GETF KEYWORD-ARGS :ERROR) '(:RETRY :REPROMPT))
				  (FILENAME FILE-ERROR)
        (LEXPR-SEND FILENAME :OPEN FILENAME KEYWORD-ARGS))
    ;; Old Syntax.
    (DO ((KEYL (IF (AND (CAR KEYWORD-ARGS) (SYMBOLP (CAR KEYWORD-ARGS)))
		   (LIST (CAR KEYWORD-ARGS))
		 (CAR KEYWORD-ARGS))
	       (CDR KEYL))
	 (KEY)
	 (CHARACTERS T)
	 (DIRECTION :INPUT)
	 (BYTE-SIZE NIL)
	 (ERROR-P T)
	 (ERROR-P-SPECD NIL)
	 (DELETED-P NIL)
	 (TEMPORARY-P NIL)
	 ;; These two are really only useful for machines that do not natively store
	 ;; 8-bit characters.
	 (RAW-P NIL)
	 (SUPER-IMAGE-P NIL)
	 )
	((NULL KEYL)
	 (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR-P '(:RETRY :REPROMPT))
				     (FILENAME FILE-ERROR)
	   ;; Because we don't want to send meaningless keywords to file systems
	   ;; which don't support them, and we don't want to cons...
	   (%ASSURE-PDL-ROOM 19.)			;Worst case
	   (%OPEN-CALL-BLOCK FILENAME 0 4)	;D-RETURN
	   (%PUSH :OPEN)       (%PUSH FILENAME)
	   (%PUSH :CHARACTERS) (%PUSH CHARACTERS)
	   (%PUSH :DIRECTION)  (%PUSH DIRECTION)
	   (COND (BYTE-SIZE     (%PUSH :BYTE-SIZE)   (%PUSH BYTE-SIZE)))
	   (COND (ERROR-P-SPECD (%PUSH :ERROR)       (%PUSH ERROR-P)))
	   (COND (DELETED-P     (%PUSH :DELETED)     (%PUSH DELETED-P)))
	   (COND (TEMPORARY-P   (%PUSH :TEMPORARY)   (%PUSH TEMPORARY-P)))
	   (COND (SUPER-IMAGE-P (%PUSH :SUPER-IMAGE) (%PUSH SUPER-IMAGE-P)))
	   (COND (RAW-P	        (%PUSH :RAW)	      (%PUSH RAW-P)))
	   (%ACTIVATE-OPEN-CALL-BLOCK)))
      (SETQ KEY (CAR KEYL))
      (SELECTOR KEY STRING-EQUAL
	((:IN :READ) (SETQ DIRECTION :INPUT))
	((:OUT :WRITE :PRINT) (SETQ DIRECTION :OUTPUT))
	((:BINARY :FIXNUM) (SETQ CHARACTERS NIL))
	((:CHARACTER :ASCII) (SETQ CHARACTERS T))
	((:BYTE-SIZE) (SETQ KEYL (CDR KEYL)
			     BYTE-SIZE (CAR KEYL)))
	((:PROBE) (SETQ DIRECTION NIL
			 CHARACTERS NIL
			 ERROR-P-SPECD T
			 ERROR-P NIL))
	((:NOERROR) (SETQ ERROR-P NIL ERROR-P-SPECD T))
	((:ERROR) (SETQ ERROR-P T ERROR-P-SPECD T))
	((:RAW) (SETQ RAW-P T))
	((:SUPER-IMAGE) (SETQ SUPER-IMAGE-P T))
	((:DELETED) (SETQ DELETED-P T))
	((:TEMPORARY) (SETQ TEMPORARY-P T))
	((:BLOCK :SINGLE) )			;Ignored for compatility with Maclisp
	(OTHERWISE (FERROR NIL "~S is not a known OPEN option" KEY))))))

))

; From file DIRED.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN DIRED-REGENERATE-LINE (LINE &AUX (PLIST (LOCF (LINE-PLIST LINE)))
			      (PATHNAME (GET PLIST ':PATHNAME)))
  "Restore the contents of LINE from the data in its properties."
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (LET ((CH (IF (= (STRING-LENGTH LINE) 0) #/SP (AREF LINE 0)))
	  (FILE (CONS PATHNAME (CDR PLIST))))
      (SETF (LINE-LENGTH LINE) 0)
      (WITH-OUTPUT-TO-STRING (S LINE)
;	(IF (GET FILE ':DIRECTORY)
;	    (LET ((STR (SEND (SEND (SEND (CAR FILE) ':PATHNAME-AS-DIRECTORY)
;				   ':NEW-PATHNAME ':NAME NIL ':TYPE NIL ':DEVICE NIL)
;			     ':STRING-FOR-PRINTING)))
;	      (SEND S ':STRING-OUT "      ")
;	      (SEND S ':STRING-OUT STR (1+ (STRING-SEARCH-CHAR #/: STR))))
	  (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE S))
;     )
      (OR (GET FILE ':DIRECTORY)
	  ;; Eliminate the Return which the lister writes.
	  (DECF (LINE-LENGTH LINE)))
      (INSERT-CHARS (CREATE-BP LINE 6) #/SP (GET FILE 'LEVEL))
      (SETF (AREF LINE 0) CH))
    (MUNG-LINE LINE)))

))

; From file COMD.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFCOM COM-JUMP-TO-SAVED-POSITION "Restore a saved position from a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
    (LET ((PT (GET Q-REG 'POINT)))
      (WHEN (NULL PT)
	(BARF "The register ~A doesn't point anywhere." Q-REG))
      (MOVE-BP (POINT) (CAR PT))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)))
  DIS-BPS)

))


; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer make-string make-string-simple-make-array (si:simple-make-array) (form)
  (let* ((loss `(make-array ,(cadr form) :type art-string . ,(cddr form)))
	 (loser (try-to-use-simple-make-array loss)))
    (if (eq loss loser) form loser)))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN DESCRIBE-SYMBOL (SYM)
  (FORMAT T "~%Symbol ~S is in ~:[no~;the ~:*~A~] package." SYM (SYMBOL-PACKAGE SYM))
  (let ((tem nil))
    (dolist (p *all-packages*)
      (multiple-value-bind (s flag) (intern-soft sym p)
	(when (and flag
		   (eq s sym)
		   (not (eq p (symbol-package sym)))
		   (not (memq p (package-used-by-list (symbol-package sym)))))
	  (push p tem))))
    (when tem (format t "~% It is also interned in package~P ~{~A~^, ~}" (length tem) tem)))
  (WHEN (AND (BOUNDP SYM) (NOT (KEYWORDP SYM)))
    (LET ((*PRINT-LEVEL* 2) (*PRINT-LENGTH* 3))
      (FORMAT T "~%The value of ~S is ~S" SYM (SYMBOL-VALUE SYM)))
    (DESCRIBE-1 (SYMBOL-VALUE SYM)))
  (WHEN (FBOUNDP SYM)
    (LET ((*PRINT-LEVEL* 2) (*PRINT-LENGTH* 3))
      (IGNORE-ERRORS
	(FORMAT T "~%The function definition of ~S is ~S: ~S"
		SYM (SYMBOL-FUNCTION SYM) (ARGLIST SYM))))
	 (DESCRIBE-1 (FSYMEVAL SYM)))
  (DO ((PL (SYMBOL-PLIST SYM) (CDDR PL))
       (*PRINT-LEVEL* 2)
       (*PRINT-LENGTH* 3))
      ((NULL PL))
    (FORMAT T "~%~S has property ~S: ~S"
	    SYM (CAR PL) (CADR PL))
    (DESCRIBE-1 (CADR PL)))
  (IF (NOT (OR (BOUNDP SYM) (FBOUNDP SYM) (SYMBOL-PLIST SYM)))
      (FORMAT T "~%It has no value, definition or properties"))
  NIL)

))

;(load "SYS:IO;PNMAP LISP >" :verbose nil :set-default-pathname nil)


;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-PRINTING) ()
;  (LET* ((DEFAULT-CONS-AREA PATHNAME-AREA)
;	 (DIR (IF (ATOM DIRECTORY) (LIST (STRING-OR-WILD DIRECTORY))
;		(MAPCAR 'STRING-OR-WILD DIRECTORY)))
;	 (DEV (LOGICAL-DEVICE-STRING))
;	 (NAM (LOGICAL-NAME-STRING))
;	 (TYP (LOGICAL-TYPE-STRING))
;	 (VER (LOGICAL-VERSION-STRING)))	; can actually be a number
;	(FORMAT NIL "~A: ~@[~A: ~]~:[~{~A; ~}~;~*~]~@[~A~]~@[ ~A~]~@[ ~A~]"
;	    (SEND HOST :NAME-AS-FILE-COMPUTER)
;	    DEV
;	    (MEMBER DIRECTORY '(NIL (:UNSPECIFIC)))
;	    DIR NAM TYP (IF (NUMBERP VER) (FORMAT NIL "~D" VER) VER))))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-DIRECTORY) ()
;  (LET (;(DEFAULT-CONS-AREA PATHNAME-AREA)
;	(DIR (IF (ATOM DIRECTORY) (LIST (STRING-OR-WILD DIRECTORY))
;	       (MAPCAR 'STRING-OR-WILD DIRECTORY)))
;	(DEV (LOGICAL-DEVICE-STRING)))
;    (FORMAT NIL "~@[~A: ~]~:[~{~A;~^ ~}~;~]"
;	    DEV
;	    (MEMBER DIRECTORY '(NIL (:UNSPECIFIC)))
;	    DIR)))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFMETHOD (LOGICAL-PATHNAME :PARSE-NAMESTRING) (IGNORE NAMESTRING &OPTIONAL (START 0) END)
;  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
;  (DO ((I START)
;       (J START (1+ J))
;       CH TEM Q
;       DIR NAM NAMP TYP TYPP VERS)
;      ((> J END)
;       (SETQ DIR (NREVERSE DIR))
;       (WHEN (= (LENGTH DIR) 1)
;	 (SETQ DIR (CAR DIR)))
;       (VALUES :UNSPECIFIC DIR NAM TYP VERS))
;    (SETQ CH (IF (= J END) #/SP (AREF NAMESTRING J)))
;    (COND ((= CH '#/)
;	   (SETQ J (1+ J)))
;	  ((MEMQ CH '(#/; #/: #/ #/ #/SP #/TAB #/.))
;	   (COND ((OR ( I J) (= CH #/) (= CH #/))
;		  (AND (MEM #'= CH '(#/ #/))
;		       (OR ( I J)
;			   (AND ( (1+ J) END)
;				( (AREF NAMESTRING (1+ J)) #/SP)))
;		       (PATHNAME-ERROR J
;			       "An unquoted ~C must be a component unto itself." CH))
;		  (MULTIPLE-VALUE (TEM Q)
;		    (SELECTQ CH
;		      (#/ (VALUES :UNSPECIFIC NIL))
;		      (#/ (VALUES NIL NIL))
;		      (T (UNQUOTE-LOGICAL-STRING NAMESTRING I J))))
;		  (IF (AND (NOT Q) (STRING= TEM "*"))
;		      (SETQ TEM :WILD))
;		  (SELECTQ CH
;		    (#/: NIL)			;Ignore "devices"
;		    (#/; (PUSH TEM DIR))
;		    (OTHERWISE
;		     (COND (VERS)
;			   (TYPP (SETQ VERS (COND ((MEMQ TEM '(:UNSPECIFIC :WILD)) TEM)
;						  ((AND (NOT Q)
;							(COND ((STRING= TEM ">") :NEWEST)
;							      ((STRING= TEM "<") :OLDEST)
;							      ((NUMERIC-P TEM)))))
;						  (T (PATHNAME-ERROR J
;						       "Version not numeric")))))
;			   (NAMP (SETQ TYP TEM TYPP T))
;			   (T (SETQ NAM TEM NAMP T)))))))
;	   (SETQ I (1+ J))))))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFUN UNQUOTE-LOGICAL-STRING (STRING &OPTIONAL (START 0) (END (STRING-LENGTH STRING)))
;  (DO ((I START (1+ I))
;       (NCH 0) (CH)
;       (NEED-COPY NIL))
;      (( I END)
;       (COND ((AND (= START 0) (= I (STRING-LENGTH STRING)) (NOT NEED-COPY))
;	      STRING)				;To avoid consing
;	     ((NOT NEED-COPY)
;	      (SUBSTRING STRING START I))
;	     (T
;	      (DO ((NSTRING (MAKE-STRING NCH))
;		   (J 0)
;		   (K START (1+ K))
;		   CHAR-QUOTED
;		   (CH))
;		  (( K I) (VALUES NSTRING T))
;		(SETQ CH (AREF STRING K))
;		(COND (( CH #/)
;		       (SETF (AREF NSTRING J) (IF CHAR-QUOTED CH (CHAR-UPCASE CH)))
;		       (SETQ CHAR-QUOTED NIL)
;		       (INCF J))
;		      (T (SETQ CHAR-QUOTED T)))))))
;    (SETQ CH (AREF STRING I))
;    (IF (= CH #/)
;	(SETQ NEED-COPY T)
;      (IF ( #/a CH #/z) (SETQ NEED-COPY T))
;      (INCF NCH))))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFMETHOD (LOGICAL-PATHNAME :PARSE-DIRECTORY-SPEC) (SPEC)
;  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
;	;; Canonicalize list of length 1 into a single string.
;	((AND (CONSP SPEC)
;	      (STRINGP (CAR SPEC))
;	      (NULL (CDR SPEC)))
;	 (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
;	;; A list of strings is also a structured directory.
;	((AND (CONSP SPEC)
;	      (LOOP FOR ELT IN SPEC ALWAYS (STRINGP ELT)))
;	 (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
;	((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
;	(T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(defmethod (logical-pathname :translated-pathname) (&aux translations trans frobs)
;  (let ((new-host (send host :host)))
;    (IF (NULL DIRECTORY)
;	(SEND SELF :NEW-PATHNAME :HOST NEW-HOST :DEVICE NIL)
;      (setq translations (send host :translations))
;      (block loser
;	(do-forever
;	  (setq trans translations)
;	 loop
;	  (when (setq frobs (pathname-component-match (caar trans) directory #/* -1 t))
;	    (setq trans (car trans))
;	    (return-from loser nil))
;	  (setq trans (cdr trans))
;	  (when trans (go loop))
;	  (SIGNAL-PROCEED-CASE ((NEWDIR)
;				'UNKNOWN-LOGICAL-DIRECTORY
;				"No translation for directory ~A on host ~A."
;				directory (SEND HOST ':NAME))
;	    (:NEW-LOGICAL-DIRECTORY
;	     (SETQ DIRECTORY (pathname-directory (send self :new-directory newdir))))
;	    (:NEW-DIRECTORY
;	     (LET ((PN (PARSE-PATHNAME NEWDIR NEW-HOST)))
;	       (RETURN (SEND PN :NEW-PATHNAME :DEVICE (OR (SEND PN :DEVICE)
;							  (SEND HOST :DEFAULT-DEVICE))
;			     		      :NAME NAME
;					      :TYPE (SEND SELF :CANONICAL-TYPE)
;					      :VERSION VERSION))))
;	    (:NEW-TRANSLATION
;	     (CHANGE-LOGICAL-PATHNAME-DIRECTORY HOST DIRECTORY NEWDIR))))))
;    (MAKE-PATHNAME :HOST NEW-HOST
;		   :DEVICE (OR (TRANSLATION-PHYSICAL-DEVICE TRANS)
;			       (SEND HOST :DEFAULT-DEVICE))
;		   :DIRECTORY (pathname-translate-wild-component
;				(translation-physical-directory trans)
;				nil
;				frobs
;				(send (send new-host :sample-pathname)	;wild-any
;				      :internal-wild-characters)
;				-1)		;don't care about wild-one
;		   :NAME NAME
;		   :TYPE (SEND SELF :CANONICAL-TYPE)
;		   :VERSION VERSION)))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFMETHOD (LOGICAL-PATHNAME :DIRECTORY-TRANSLATABLE-P) ()
;  (let ((translations (send host :translations)) frobs)
;    (dolist (trans translations)
;      (when (setq frobs (pathname-component-match (car trans) directory #/* -1 t))
;	(return (values (pathname-translate-wild-component
;			  (translation-physical-directory trans)
;			  nil
;			  frobs
;			  (send (send (send host :host) :sample-pathname)
;				:internal-wild-characters)
;			  -1)
;			(translation-physical-device trans)))))))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFMETHOD (LOGICAL-PATHNAME :BACK-TRANSLATED-PATHNAME) (PATHNAME &AUX DEV DIR DEFDEV)
;  (SETQ DEV (SEND PATHNAME :DEVICE)
;	DIR (SEND PATHNAME :DIRECTORY)
;	DEFDEV (SEND HOST :DEFAULT-DEVICE))
;  (DOLIST (TRANS (SEND HOST ':TRANSLATIONS))
;    (when (EQUAL (OR (TRANSLATION-PHYSICAL-DEVICE TRANS) DEFDEV) DEV)
;      (let* ((translation-physical-directory (translation-physical-directory trans))
;	     (frobs (pathname-component-match translation-physical-directory
;					      dir
;					      (send (send (send host :host)
;							  :sample-pathname)
;						    :internal-wild-characters)
;					      -1
;					      t)))
;	(when frobs
;	  (setq dir (pathname-translate-wild-component
;		      (car trans)
;		      nil
;		      frobs
;		      #/*
;		      -1
;		      t)
;		dev :unspecific)))))
;  (SEND PATHNAME :NEW-PATHNAME :HOST HOST
;			       :DEVICE DEV
;			       :DIRECTORY DIR
;			       :TYPE (SEND PATHNAME :CANONICAL-TYPE)))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(defun canonicalize-logical-directory (log dir)
;  (multiple-value-bind (nil ndir nam typ ver)
;      (send (send log :sample-pathname) :parse-namestring nil dir)
;    (if nam (cerror :yes nil nil "Name specified in logical directory specification ~A" dir))
;    (if typ (cerror :yes nil nil "Type specified in logical directory specification ~A" typ))
;    (if ver (cerror :yes nil nil "Version specified in logical directory specification ~A" ver))
;    ndir))

;))

;; From file PATHST.LISP PS:<MLY.L> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "Z: L; PATHST  "

;(DEFUN ADD-LOGICAL-PATHNAME-HOST (LOGICAL-HOST PHYSICAL-HOST TRANSLATIONS
;				  &OPTIONAL DEFAULT-DEVICE
;				  &AUX LOG OLD DEFDEV)
;  "Define a logical host named LOGICAL-HOST, which translates to PHYSICAL-HOST.
;TRANSLATIONS specifies the directory translations to use: each element looks like
; (logical-dir physical-dir), where logical-dir is just a string containing the
; directory name to translate, and physical-dir contains a directory and optionally
; a device, complete with delimiters as appropriate for PHYSICAL-HOST to parse.
;An element in TRANSLATIONS that has NIL instead of a logical-dir specifies
;the default device, to be used as the device when translating directories
;that are not mentioned in TRANSLATIONS.
;DEFAULT-DEVICE can be used to override the default device found in the translations."
;  (IF (SETQ LOG (GET-PATHNAME-HOST LOGICAL-HOST T))
;      (SETQ OLD T)
;      (PUSH (SETQ LOG (MAKE-INSTANCE 'LOGICAL-HOST ':NAME LOGICAL-HOST))
;	    *PATHNAME-HOST-LIST*))
;  (SETQ PHYSICAL-HOST (OR (GET-PATHNAME-HOST PHYSICAL-HOST T) (SI:PARSE-HOST PHYSICAL-HOST)))
;  ;; Here is a bit of a kludge for SI:SET-SITE.  If the physical host is not defined yet,
;  ;; add it now.
;  (PUSHNEW PHYSICAL-HOST *PATHNAME-HOST-LIST* :test 'eq)
;  (SEND LOG ':SET-HOST PHYSICAL-HOST)
;  (IF TRANSLATIONS
;      (SEND LOG ':SET-TRANSLATIONS
;	    (LOOP FOR (LOGICAL-DIRECTORY PHYSICAL-DIRECTORY) IN TRANSLATIONS
;		  WITH DEVICE AND DIRECTORY
;		  DO (LET ((PN (PARSE-PATHNAME PHYSICAL-DIRECTORY PHYSICAL-HOST)))
;		       (setq logical-directory (canonicalize-logical-directory log logical-directory))
;		       (SETQ DIRECTORY (PATHNAME-DIRECTORY PN))
;		       (SETQ DEVICE (PATHNAME-DEVICE PN))
;		       (WHEN (AND LOGICAL-DIRECTORY (MEMQ DIRECTORY '(NIL :UNSPECIFIC)))
;			 (FERROR NIL
;				 "No directory specified in ~A, you probably forgot some delimiter characters."
;				 PHYSICAL-DIRECTORY))
;		       ;; Translation for logical directory NIL specifies the default device.
;		       (WHEN (NULL LOGICAL-DIRECTORY)
;			 (SETQ DEFDEV DEVICE))
;		       ;; Default the default directory to the host's primary device.
;		       (WHEN (NULL DEFDEV)
;			 (SETQ DEFDEV (SEND (SAMPLE-PATHNAME PHYSICAL-HOST) ':PRIMARY-DEVICE))))
;		  WHEN LOGICAL-DIRECTORY
;		       COLLECT (MAKE-LOGICAL-PATHNAME-TRANSLATION
;				 LOGICAL-DIRECTORY LOGICAL-DIRECTORY
;				 PHYSICAL-DEVICE DEVICE
;				 PHYSICAL-DIRECTORY DIRECTORY))))
;  (SEND LOG ':SET-DEFAULT-DEVICE (OR DEFAULT-DEVICE DEFDEV))
;  LOG)

;))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defun get-setf-method-multiple-value (form &optional short-cut &aux tem)
  "Return the canonical five values that say how to do SETF on FORM.
The values are:
* a list of symbols, gensyms, that stand for parts of FORM
* a list of the parts of FORM that they stand for
* a list of symbols, gensyms, that stand for the values to be stored
* an expression to do the storing.  It contains the gensyms described already.
* an expression to refer to the existing value of FORM.
  It differs from FORM in that it has the gensyms replacing the
  parts of FORM that they stand for.
These values give all the information needed to examine and set
 FORM repeatedly without evaluating any of its subforms more than once.

If SHORT-CUT is non-NIL, and if FORM's method of SETFing was defined
by a simple DEFSETF that just gives a function to do the setting,
then we return just two values: the setting function and a replacement FORM
/(differing from FORM by having macros expanded, CADR -> CAR (CDR ...), etc.).
The caller can tell that this case occurred because the first value
is a non-NIL symbol in this case, and is always a list in the normal case."
  (declare (values tempvars tempargs storevars storeform refform))
  (cond ((symbolp form)
	 (let ((g (gensym)))
	   (values nil nil (list g) `(setq ,form ,g) form)))
	((atom form))
	((not (symbolp (car form)))
	 (ferror nil "~S non-symbolic function in SETF." (car form)))
	((or (eq (getdecl (car form) 'setf-method) 'unsetfable)
             (eq (getdecl (car form) 'setf) 'unsetfable))
	 (nosetf form))
	((setq tem (getdecl (car form) 'setf-method))
	 (if (symbolp tem)
	     (if short-cut
		 (values tem form)
	       (let ((gs (mapcar #'(lambda (ignore) (gensym)) (cdr form)))
		     (g (gensym)))
		 (values gs (cdr form) (list g)
			 `(,tem ,@gs ,g)
			 `(,(car form) ,@gs))))
	   (if (eq (cdr tem) 'nosetf)
	       (nosetf form))
	   (call (cdr tem) () form ':optional *macroexpand-environment*)))
	((setq tem (getdecl (car form) 'setf-expand))
	 (get-setf-method-multiple-value (funcall tem form) short-cut))
	((and (fboundp (car form))
	      (arrayp (fsymeval (car form))))
	 (get-setf-method-multiple-value `(aref #',(car form) . ,(cdr form)) short-cut))
	((and (fboundp (car form))
	      (symbolp (fsymeval (car form))))
	 (get-setf-method-multiple-value `(,(fsymeval (car form)) . ,(cdr form)) short-cut))
	((not (eq form (setq form (macroexpand-1 form
						 si:*macroexpand-environment*))))
	 (get-setf-method-multiple-value form short-cut))
	(t (ferror 'sys:unknown-setf-reference
		   "No way known to do SETF of ~S." (car form)))))

))

; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defprop nosetf t :error-reporter)
(defun nosetf (form)
  (ferror 'unknown-locf-reference
	  "SETF is explicitly forbidden on ~S." (car form)))
))



;; From file PATHST.LISP PS:<L.IO> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

;(DEFMETHOD (LOGICAL-HOST :LOGICALLY-BACKTRANSLATE-HOST-DEV-DIR)
;	   (PHYSICAL-HOST PHYSICAL-DEVICE PHYSICAL-DIRECTORY)
;  (WHEN (OR (EQ PHYSICAL-HOST HOST)
;	    (EQ PHYSICAL-HOST SELF))	;SYS:LISPM; sort of things.
;    (DOLIST (TRANS TRANSLATIONS
;		   (IF (AND (EQ PHYSICAL-HOST SELF)
;			    (NOT (ASSOC PHYSICAL-DIRECTORY TRANSLATIONS)))
;		       ;; SYS:RG; should backtranslate to AI:DSK:RG;
;		       (VALUES HOST
;			       (IF (MEMQ PHYSICAL-DEVICE '(NIL :UNSPECIFIC))
;				   DEFAULT-DEVICE
;				 PHYSICAL-DEVICE)
;			       PHYSICAL-DIRECTORY)))
;      (when (EQUAL (OR (TRANSLATION-PHYSICAL-DEVICE TRANS) DEFDEV) DEV)
;	(let* ((translation-physical-directory (translation-physical-directory trans))
;	       (frobs (pathname-component-match translation-physical-directory
;						dir
;						(send (send (send host :host)
;							    :sample-pathname)
;						      :internal-wild-characters)
;						-1
;						t)))
;	  (when frobs
;	    (return self
;		    :unspecific
;		    (pathname-translate-wild-component
;		      (car trans)
;		      nil
;		      frobs
;		      #/*
;		      -1
;		      t))))))))

;))

;; From file PATHST.LISP PS:<L.IO> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

;(DEFUN BACKTRANSLATE-HOST-DIRECTORY (PHYSICAL-HOST PHYSICAL-DEVICE PHYSICAL-DIRECTORY)
;  (DO ((HE *PATHNAME-HOST-LIST* (CDR HE))
;       TRANS-HOST TRANS-DEVICE TRANS-DIRECTORY)
;      ((NULL HE))
;    (MULTIPLE-VALUE (TRANS-HOST TRANS-DEVICE TRANS-DIRECTORY)
;      (SEND (CAR HE) :LOGICALLY-BACKTRANSLATE-HOST-DEV-DIR
;	    	     PHYSICAL-HOST PHYSICAL-DEVICE PHYSICAL-DIRECTORY))
;    (if (and (consp trans-directory) (= (length trans-directory) 1))
;	(setq trans-directory (car trans-directory)))
;    (IF TRANS-HOST
;	(RETURN (VALUES TRANS-HOST TRANS-DEVICE TRANS-DIRECTORY)))))

;))

; From file STRUCT.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; STRUCT  "

(defmacro defstruct-define-type (type &body options)
  "Defines a new type of defstruct structure, with name TYPE.
OPTIONS may include:
 (:CONS (init desc kwds) kind body)
   Body returns code to construct a structure of type TYPE. Init, desc and kwds are bound to
     the initialization defaults, the structure description and an alist of values of
     additional keywords arguments supplied to DEFSTRUCT. Kind is either :LIST or :ALIST.
 (:REF (slot-number desc arg-1 arg-2 ...) body)
   Body returns the contents of slot-number in the structure. desc is bound to the structure
    description. The arg-n's are bound to values supplied to the accessor as arguments.
 (:OVERHEAD slots)
   Reserves a given number of slots as overhead to this structure-type, which are thus
     unavailable for storing structure data.
 :NAMED
   Indicates that this is a named structure type that TYPEP can recognize.
 (:NAMED named-type)
   Defines the associated named structure type to this unnamed TYPE.
 (:CONS-KEYWORDS kwd-1 ...) or (:KEYWORDS kwd-1 ...)
   Defines keywords which may be supplied to a constructor to affect the construction of an
     instance of this structure. These keywords will appear in the kwds alist supplied to the
     :CONS code.
 (:DEFSTRUCT-KEYWORDS kwd-1 ...)
   Defines keywords which may be supplied to DEFSTRUCT to affect the definition of a stucture
     of this TYPE. These keywords will appear in the property-alist slot of the desc supplied
     to the :defstruct code
 (:PREDICATE (desc name) body)
   Body is code which generates a predicate named name for a structure with description desc.
 (:COPIER (desc name) body)
   Body defines code to define a structure-copier named name.
 (:DEFSTRUCT (desc) body)
   Body is run whenever DEFSTRUCT expands a structure of this TYPE. It should return a list
     of forms to be included in the DEFSTRUCT expansion, or else NIL."
  (DO* ((DOC (IF (STRINGP (CAR OPTIONS)) (POP OPTIONS)))
	(options options (cdr options))
	(op) (args)
	(type-description (make-defstruct-type-description))
	(cons-expander nil)
	(ref-expander nil)
	(returns))
       ((null options)
	(SETF (DEFSTRUCT-TYPE-DESCRIPTION-DOCUMENTATION) DOC)
	(or cons-expander
	    (defstruct-error "No :CONS option in DEFSTRUCT-DEFINE-TYPE" type))
	(or ref-expander
	    (defstruct-error "No :REF option in DEFSTRUCT-DEFINE-TYPE" type))
	`(progn 'compile			;not needed in common lisps
		#+LISPM
		(LOCAL-DECLARE ((FUNCTION-PARENT ,TYPE DEFSTRUCT-DEFINE-TYPE))
		  ,cons-expander
		  ,ref-expander
		  ,@returns)
		#-LISPM ,cons-expander
		#-LISPM ,ref-expander
		#-LISPM ,@RETURNS
		(defprop ,type ,type-description defstruct-type-description)
		',TYPE))
    (cond ((atom (setq op (car options)))
	   (setq args nil))
	  (t
	   (setq args (cdr op))
	   (setq op (car op))))
 AGAIN
    (selectq op
      (:cons
	(or (> (length args) 2)
	    (defstruct-error
	      "Bad :CONS option in DEFSTRUCT-DEFINE-TYPE"
	      (car options) 'in type))
	(let ((n (length (car args)))
	      (name (defstruct-append-symbols type "-DEFSTRUCT-CONS")))
	  (or (= n 3)
	      (defstruct-error
		"Bad :CONS option in DEFSTRUCT-DEFINE-TYPE"
		(car options) 'in type))
	  (setf (defstruct-type-description-cons-flavor)
		(defstruct-retry-keyword (cadr args)))
	  (setf (defstruct-type-description-cons-expander) name)
	  (setq cons-expander `(defun ,name ,(car args)
				 ,@(cddr args)))))
      (:ref
	(or (> (length args) 1)
	    (defstruct-error
	      "Bad :REF option in DEFSTRUCT-DEFINE-TYPE"
	      (car options) 'in type))
	(let ((n (length (car args)))
	      (name (defstruct-append-symbols type "-DEFSTRUCT-REF")))
	  (or (> n 2)
	      (defstruct-error
		"Bad :REF option in DEFSTRUCT-DEFINE-TYPE"
		(car options) 'in type))
	  (setf (defstruct-type-description-ref-no-args) (- n 2))
	  (setf (defstruct-type-description-ref-expander) name)
	  (setq ref-expander `(defun ,name ,(car args)
				,@(cdr args)))))
      (:predicate
	(or (> (length args) 1)
	    (defstruct-error
	      "Bad :PREDICATE option in DEFSTRUCT-DEFINE-TYPE"
	      (car options) 'in type))
	(let ((name (defstruct-append-symbols type "-DEFSTRUCT-PREDICATE")))
	  (setf (defstruct-type-description-predicate) name)
	  (push `(defun ,name ,(car args)
		   ,@(cdr args))
		returns)))
      (:copier
	(or (> (length args) 1)
	    (defstruct-error
	      "Bad :COPIER option in DEFSTRUCT-DEFINE-TYPE"
	      (car options) 'in type))
	(let ((name (defstruct-append-symbols type "-DEFSTRUCT-COPIER")))
	  (setf (defstruct-type-description-copier) name)
	  (push `(defun ,name ,(car args)
		   ,@(cdr args))
		returns)))
      (:overhead
	(setf (defstruct-type-description-overhead)
	      (if (null args)
		  (defstruct-error
		    "Bad :OVERHEAD option to DEFSTRUCT-DEFINE-TYPE"
		    (car options) 'in type)
		  (car args))))
      (:named
	(setf (defstruct-type-description-named-type)
	      (if (null args)
		  type
		  (car args))))
      ((:CONS-KEYWORDS :keywords)
	(setf (defstruct-type-description-cons-keywords) args))
      (:DEFSTRUCT-KEYWORDS
       (SETF (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS) ARGS))
      (:defstruct
	(or (> (length args) 1)
	    (defstruct-error
	      "Bad :DEFSTRUCT option in DEFSTRUCT-DEFINE-TYPE"
	      (car options) 'in type))
	(let ((name (defstruct-append-symbols type "-DEFSTRUCT-EXPAND")))
	  (setf (defstruct-type-description-defstruct-expander) name)
	  (push `(defun ,name ,@args) returns)))
      (t
       (let ((new (defstruct-retry-keyword op)))
	 (unless (eq op new)
	   (setq op new)
	   (go AGAIN))
	 (defstruct-error
	   "Unknown option to DEFSTRUCT-DEFINE-TYPE"
	   op 'in type))))))

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer FLOAT FLOAT-OPTIMIZER (INTERNAL-FLOAT) (form)
  (COND ((NULL (CDDR FORM))			;One arg
	 `(INTERNAL-FLOAT ,(CADR FORM)))
	((NUMBERP (CADDR FORM))			;Second arg a number
	 (IF (SMALL-FLOATP (CADDR FORM))
	     (if (numberp (cadr form))
		 (small-float (cadr form))
	       `(SMALL-FLOAT ,(CADR FORM)))
	   (if (numberp (cadr form))
	       (float (cadr form))
	     `(INTERNAL-FLOAT ,(CADR FORM)))))
	(T FORM)))

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer small-float	ARITH-OPT-NON-ASSOCIATIVE)

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defcompiler-synonym short-float small-float)

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFSUBST SHORT-FLOAT (NUMBER)
  "Convert NUMBER to a short float."
  (SMALL-FLOAT NUMBER))

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-PROCLAIM (DECL-LIST PROCESS-FN)
  (MAPC #'(LAMBDA (DECL &AUX X)
	    (SETQ X (EVAL DECL))
	    (IF (NSYMBOLP (CAR X))
		(WARN 'BAD-DECLARATION ':IMPOSSIBLE
		      "An argument of PROCLAIM evaluated to ~S, which is not a valid declaration"
		      DECL)
	      (LET ((S (SYMBOL-NAME (CAR X))))
		(COND ((MEMQ (CAR X) '(SPECIAL UNSPECIAL))
		       (FUNCALL PROCESS-FN DECL 'PROCLAIM))
		      ((EQUAL S "INLINE")
		       )
		      ((EQUAL S "NOTINLINE")
		       )
		      ((EQUAL S "DECLARATION")
		       (PUSHNEW `(,S IGNORE) SI:INTERPRETER-DECLARATION-TYPE-ALIST
				:TEST 'EQUAL :KEY 'CAR))))))
	DECL-LIST))

))

; From file QCFILE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCFILE  "

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN &OPTIONAL COMPILE-TIME-TOO (TOP-LEVEL-P T))
  (PROG TOP (FN (OFORM FORM))
    ;; The following loop is essentially MACROEXPAND,
    ;; but for each expansion, we create an appropriate warn-on-errors message
    ;; containing the name of the macro about to be (perhaps) expanded this time.
    (DO ((NFORM))
	(())
      (IF (AND OVERRIDE-FN
	       (FUNCALL OVERRIDE-FN FORM))
	  (RETURN-FROM TOP NIL))
      (IF (ATOM FORM) (RETURN NIL))
      ;; Don't expand LOCALLY into PROGN here!
      ;; This way, we protect DECLAREs inside the LOCALLY
      ;; from being treated as top-level DECLARE, which would be erroneous.
      ;; The LOCALLY form will just be executed as a random form.
      (IF (EQ (CAR FORM) 'LOCALLY)
	  (RETURN))
      (SETQ NFORM
	    (WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S at top level"
			     (CAR FORM))
	      (MACROEXPAND-1 FORM)))
      (IF (EQ FORM NFORM) (RETURN)
	(SETQ FORM NFORM)))
    ;; If this was a top-level macro, supply a good guess
    ;; for the function-parent for any DEFUNs inside the expansion.
    (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (COND ((ATOM FORM))
	    ((AND (NEQ FORM OFORM) (SYMBOLP (CADR OFORM)))
	     (PUSH `(FUNCTION-PARENT ,(CADR OFORM)) LOCAL-DECLARATIONS))
	    ((EQ (CAR OFORM) 'DEFSTRUCT)
	     (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
		   LOCAL-DECLARATIONS)))
      (AND (CONSP FORM)
	   (NEQ (CAR FORM) 'EVAL-WHEN)
	   COMPILE-TIME-TOO
	   (FUNCALL PROCESS-FN FORM 'DECLARE))
      (COND ((ATOM FORM)
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ (CAR FORM) 'EVAL-WHEN)
	     (OR (AND (CLI:LISTP (CADR FORM))
		      (LOOP FOR TIME IN (CADR FORM)
			    ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE CLI:EVAL))))
		 (FERROR NIL "~S invalid EVAL-WHEN times;
must be a list of EVAL, LOAD, and//or COMPILE."
			     (CADR FORM)))
	     (LET* ((COMPILE (MEMQ 'COMPILE (CADR FORM)))
		    (LOAD (MEMQ 'LOAD (CADR FORM)))
		    (EVAL (OR (MEMQ 'EVAL (CADR FORM)) (MEMQ 'CLI:EVAL (CADR FORM))))
		    (EVAL-NOW (OR COMPILE (AND COMPILE-TIME-TOO EVAL))))
	       (DOLIST (FORM1 (CDDR FORM))
		 (IF LOAD
		     (IF EVAL-NOW
			 (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN T NIL)
		       (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN NIL NIL))
		   (IF EVAL-NOW
		       (FUNCALL PROCESS-FN FORM1 'DECLARE))))))
	    ((EQ (SETQ FN (CAR FORM)) 'DEFF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (FUNCALL PROCESS-FN FORM 'RANDOM))
	    ((EQ FN 'DEF)
	     (COMPILATION-DEFINE (CADR FORM))
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL))
		   (CDDR FORM)))
	    ((EQ FN 'WITH-SELF-ACCESSIBLE)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL))
		   (CDDR FORM)))
	    ((EQ FN 'PROGN)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
		   (CDR FORM)))
	    ((MEMQ FN '(MACRO DEFSUBST))
	     (FUNCALL PROCESS-FN FORM 'MACRO))
	    ((AND TOP-LEVEL-P
		  (MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
			     EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT DEFF-MACRO
			     REQUIRE)))
	     (FUNCALL PROCESS-FN FORM 'SPECIAL))
	    ((EQ FN 'DECLARE)
	     (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	    ((EQ FN 'PROCLAIM)
	     (COMPILE-PROCLAIM (CDR FORM) PROCESS-FN))
	    ((EQ FN 'COMMENT) NIL)
	    ((EQ FN 'PATCH-SOURCE-FILE)
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T)
	     (MAPC #'(LAMBDA (FORM)
		       (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
		   (CDDR FORM))
	     (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
				(SETQ SI:PATCH-SOURCE-FILE-NAMESTRING NIL))
			     PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
	    ((EQ FN 'COMPILER-LET)
	     (EVAL `(LET ,(CADR FORM) (COMPILE-DRIVER '(PROGN . ,(CDDR FORM))
						      ',PROCESS-FN ',OVERRIDE-FN
						      ,COMPILE-TIME-TOO
						      T))))
	    ((EQ FN 'DEFUN)
	     (LET (TEM)
	       (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed DEFUN")
		 (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
	       (COND ((EQ (CDR TEM) (CDR FORM))
		      (IF (NULL (CDDR TEM))
			  (WARN 'MALFORMED-DEFUN ':IMPOSSIBLE
				"Malformed defun ~S" FORM)
			(FUNCALL PROCESS-FN FORM 'DEFUN)))
		     (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL)))))
	    (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defvar test-member-alist
	'((eq . memq)
	  (equal . member)
	  (eql . member-eql)
	  (equalp . member-equalp))
  "Alist of test functions and functions which can be used to check whether any member
of a list satisfies the test. Eg (EQ . MEMQ)")

))

; From file QCOPT.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCOPT  "

(defoptimizer memq memq-eq () (form)
  (or (when (= (length form) 3)
	(let ((item (cadr form))
	      (list (caddr form)))
	  (if (quotep list)
	      (selectq (length (cadr list))
		(0 `(progn ,item nil))
		(1 `(and (,(car (rassq (car form) test-member-alist))
			  ,item ',(car (cadr list)))
			 ',(cadr list)))))))
      form))

))


; From file SETF.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SETF  "

(defmacro pushnew (value place &rest options)
  "Add VALUE to the front of the list PLACE, if it's not already MEMQ there.
Equivalent to (SETF PLACE (ADJOIN VALUE PLACE OPTIONS...))
but evaluates subforms of PLACE only once."
  (declare (arglist value place &key test test-not key))
  (tagbody
      (if (or (not (symbolp place)) (oddp (length options)))
	  ;; stupid optimizer doesn't try to win on this.
	  (go lose)
	(let (test xtest)
	  (do ((x options (cddr x)))
	      ((null x))
	    (cond ((or (eq (car x) ':test)
		       (equal (car x) '':test))
		   (if (not test)
		       ;; note: cannot use (function test) as this would lose inside FLET
		       ;;  redefinition of test
		       (unless (list-match-p (cadr x) `(quote ,test))
			 (return (setq test nil)))))
		  (t (return (setq test nil)))))
	  (if (null options) (setq test 'eql))
	  (cond ((and (symbolp test) (not (null test)))
		 (setq xtest (cdr (assq test compiler:test-member-alist)))
		 (flet ((make-test (item)
			  (if xtest
			      `(,xtest ,item ,place)
			    (let ((x (gensym)))
			      `(do ((,x ,place (cdr ,x)))
				   ((null ,x))
				 (if (,test (car ,x) ,item) (return ,x)))))))
		   (return-from pushnew (if (or (symbolp value) (constantp value))
					    `(progn (or ,(make-test value)
							(push ,value ,place))
						    ,value)
					  (let ((tem (gensym)))
					    `(let ((,tem ,value))
					       (or ,(make-test tem)
						   (push ,tem ,place))
					       ,tem))))))
		;; stupid optimizer doesn't try to hack this either...
		(t (go lose)))))
   lose
      (return-from pushnew
	(multiple-value-bind (tempvars tempargs storevars storeform refform)
	    (get-setf-method place)
	  (let ((val (gensym)))
	    (sublis-eval-once (cons `(,val . ,value) (pairlis tempvars tempargs))
			      (sublis (list (cons (car storevars)
						  `(adjoin ,val ,refform . ,options)))
				      storeform)
			      t t))))))

))
