;;; -*- Mode:Lisp; Readtable:ZL; Package:SI; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.25
;;; Reason:
;;;  Hairy Pace pdl-growing code to copy stack-closure correctly
;;;  Zmacs documentation buglet
;;; Written 11-Mar-85 22:14:20 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 99.23, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.


; From file OZ:OZ:<MLY.LL>EHF.LISP.3 at 14-Mar-85 00:28:48
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHF  "

(defun sg-maybe-grow-pdls (sg &optional (message-p allow-pdl-grow-message)
					regular-room special-room
					inhibit-error
			   &aux (rpp (sg-regular-pdl-pointer sg))
				(rpl (sg-regular-pdl-limit sg))
				(spp (sg-special-pdl-pointer sg))
				(spl (sg-special-pdl-limit sg))
				tem new-size new-limit did-grow)
  "Increase the size of SG's pdls if they are close to being full."
  (unless regular-room (setq regular-room #o2000))
  (unless special-room (setq special-room #o400))
  (when (> (setq tem (+ rpp regular-room)) rpl)
    (setq new-size (max (fix (* pdl-grow-ratio (array-length (sg-regular-pdl sg))))
			(+ tem #o100)))
    (setq new-limit (- new-size #o100))
    (and message-p
	 (format *error-output* "~&[Growing regular pdl of ~S from ~S to ~S]~%"
		 sg rpl new-limit))
    (setf (sg-regular-pdl sg) (sg-grow-pdl sg ':regular-pdl rpp new-size))
    (setf (sg-regular-pdl-limit sg) new-limit)
    (setq did-grow ':regular))
  (when (> (setq tem (+ spp special-room)) spl)
    (setq new-size (max (fix (* pdl-grow-ratio (array-length (sg-special-pdl sg))))
			(+ tem #o100)))
    (setq new-limit (- new-size #o100))
    (and message-p
	 (format *error-output* "~&[Growing special pdl of ~S from ~S to ~S]~%"
		 sg spl new-limit))
    (let ((osp (locf (aref (sg-special-pdl sg) 0))))
      (setf (sg-special-pdl sg) (sg-grow-pdl sg :special-pdl spp new-size))
      (setf (sg-special-pdl-limit sg) new-limit)
      (unless (eq osp (locf (aref (sg-special-pdl sg) 0)))
	(relocate-locatives-to-specpdl (sg-regular-pdl sg)
				       (sg-regular-pdl-pointer sg)
				       osp
				       (locf (aref (sg-special-pdl sg) 0))
				       (sg-special-pdl-pointer sg))))
    (unless did-grow
      (setq did-grow ':special)))
  (when (and did-grow (not inhibit-error))
    (sg-funcall-no-restart sg 'cerror :grow-pdl nil 'pdl-overflow-error
			   			    :pdl-name did-grow)))

))

; From file OZ:OZ:<MLY.LL>EHF.LISP.3 at 14-Mar-85 00:29:05
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHF  "

(defun sg-grow-pdl (sg type pdl-ptr new-size
		    &aux pdl new-pdl tem1 area)
  (setq sg (follow-structure-forwarding sg))	;don't know if this is necessary
  (setq pdl (follow-structure-forwarding (case type
					   (:regular-pdl (sg-regular-pdl sg))
					   (:special-pdl (sg-special-pdl sg))
					   (t (ferror nil "unknown pdl type")))))
  (if ( new-size (array-length pdl))
      ;; Big enough, just adjust limit
      pdl
    (cond ((= (setq area (%area-number pdl)) linear-pdl-area)	;Stupid crock
	   (setq area pdl-area))				; with non-extendable areas
	  ((= area linear-bind-pdl-area)
	   (setq area working-storage-area)))
    (setq new-pdl (make-array new-size :type (array-type pdl)
				       :area area
				       :leader-length (array-leader-length pdl)))
    (%blt-typed (locf (array-leader pdl 0)) (locf (array-leader new-pdl 0))
		(array-leader-length pdl) 1)
    (%blt-typed (locf (aref pdl 0)) (locf (aref new-pdl 0))
		(1+ pdl-ptr) 1)
    (do ((n pdl-ptr (1- n))
	 (to-p (locf (aref new-pdl 0)) (%make-pointer-offset dtp-locative to-p 1))
	 (pdl-base (locf (aref pdl 0))))
	((minusp n))
      (select (%p-data-type to-p)
	((dtp-fix dtp-small-flonum dtp-u-entry))	;The only inum types we should see
	((dtp-header-forward dtp-body-forward)
	 (ferror nil "Already forwarded? -- get help"))
	(otherwise
	 (setq tem1 (%pointer-difference (%p-contents-as-locative to-p) pdl-base))
	 (and ( 0 tem1 pdl-ptr)
	      (%p-store-pointer to-p (locf (aref new-pdl tem1)))))))
    (when (eq type ':regular-pdl)
      (make-copied-stack-closures-refer-to-new-pdl sg pdl new-pdl))
    (structure-forward pdl new-pdl)
    new-pdl))

))

; From file OZ:OZ:<MLY.LL>EHF.LISP.3 at 14-Mar-85 00:29:15
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHF  "

(defun make-copied-stack-closures-refer-to-new-pdl (sg rp new-rp)
  (do ((frame (sg-innermost-active sg) (sg-next-active sg frame)))
      ((null frame))
    (when (ldb-test %%lp-ens-environment-pointer-points-here (rp-entry-word rp frame))
      (if (not (typep (rp-function-word rp frame) 'compiled-function))
	  (ferror nil "I can't deal with this stack"))
      (let* ((local-length (%p-ldb-offset %%fefhi-ms-local-block-length
					  (rp-function-word rp frame)
					  %fefhi-misc))
	     (local-start (+ frame (rp-local-block-origin rp frame)))
	     (frame-begin-vadr (locf (aref rp frame)))
	     (frame-end-vadr (%make-pointer-offset dtp-locative
						   frame-begin-vadr
						   (+ local-start local-length))))
	(if (< local-length 2)
	    (ferror nil "local block is too small"))

	;; First take care of the simple case of a stack closure that
	;; has been copied out by GC-WRITE-TEST
	;;
	;; The stack closure before any funny stuff looks like:
	;;  (n is somewhere in the local block)
	;; This is the closure itself
	;;   n   :  CDR-NEXT   DTP-FEF-POINTER
	;;   n+1 :  CDR-NIL    DTP-LIST n+2
	;;   n+2 :  CDR-NORMAL DTP-LIST n+m     ... the stack-closure-vector
	;;   n+3 :  CDR-ERROR  DTP-LIST next-higher-lexical-environment
	;;     ...
	;; This is the stack-closure-vector
	;;   n+m :  CDR-NEXT   DTP-EVCP pointer-to-arg-or-local
	;;    ...
	;;       :  CDR-NIL    DTP-EVCP pointer-to-arg-or-local
	;;       :             DTP-LIST n+m     ... the stack-closure-vector
	;;       :             DTP-SYMBOL 0     ... this is for disconected stack closure
	;;                                          see below
	;;   END OF LOCAL BLOCK
	;;
	;; If the GC-WRITE-TEST goes off, a structure in the heap gets consed
	;;    x   :  CDR-NORMAL DTP-LIST n+m     ... to the stack-closure-vector
	;;						  still on stack
	;;    x+1 :  CDR-ERROR  DTP-LIST next higher lexical environment
	;;    x+2 :  CDR-NEXT   DTP-FEF-POINTER
	;;    x+3 :  CDR-NIL    DTP-LIST x
	;;
	;; Our job is to track down the pointer back to the stack at X, and move it
	;;  to the new stack.
	;;
	;; When the copy happens, the FEF-POINTER at n and the LIST poitner at n+2
	;; get turned into EVCP's to the respective parts of the heap closure; namely
	;; x+2 and x.
	;;
	;; The EVCP to x+2 is not needed for this stuff.
	;;
	;; So, to copy the stack, we find any EVCP to a word, like x, that points back
	;; to the stack closure vector.

	(do* ((stack-closure-vector-address
		(%p-pointer (locf (aref rp (+ local-start local-length -2)))))
	      (local-number local-start (1+ local-number))
	      (local-loc (locf (aref rp local-number)) (locf (aref rp local-number)))
	      (count local-length (1- count)))
	     ((zerop count))
	  (when (= (%p-data-type local-loc) dtp-external-value-cell-pointer)
	    (let ((final-cell (follow-cell-forwarding local-loc t)))
	      (when (= (%p-pointer final-cell) stack-closure-vector-address)
		(setf (%p-pointer final-cell)
		      (%pointer-plus new-rp
				     (%pointer-difference (%p-pointer final-cell) rp)))))))
	
	;; Now we have to worry about things that have been STACK-CLOSURE-DISCONNECTED
	;;
	;; If you call the stack-closure-disconnect instruction, it first arranges
	;; to copy out the closure itself, so at least we know the
	;; ENVIRONMENT-POINTER-POINTS-HERE bit is on in the call frame.
	;; Then, it makes a copy of the stack-closure-vector in memory, with
	;; EVCP's back to [either the original stack-closure-vector or to
	;;  what it points to]
	;; Usually this instruction will be followed by a STACK-CLOSURE-UNSHARE which
	;; snaps some of the EVCP's.
	;; Finally, it pushes a pointer to the new stack-closure-vector on the
	;; list that is in the last word of the local block.
	;;
	;; So, that list looks like:  ((5 3 4) (2 3 1) ...)
	;;
	;; Each element is another stack-closure-vector formed by a
	;; stack-closure-disconnect.
	;; The elements of the sublists are either real values, or EVCP's back to
	;; the stack.
	;;
	;; We need to find any EVCP's to the stack, and move them to the new stack
	;;
	;; The sub-lists are cdr-coded, by the way.

	(dolist (copied-vector
		  (aref rp (+ frame (rp-local-block-origin rp frame) local-length -1)))
	  (if (null copied-vector) (ferror nil "bad stack-closure-unshare list"))
	  (do ((loc (%pointer copied-vector) (%pointer-plus loc 1)))
	      (())
	    (cond ((and (= (%p-data-type loc) dtp-external-value-cell-pointer)
			( (%pointer-difference (%p-pointer loc) frame-begin-vadr) 0)
			(> (%pointer-difference frame-end-vadr (%p-pointer loc)) 0))
		   (setf (%p-pointer loc)
			 (%pointer-plus new-rp
					(%pointer-difference (%p-pointer loc) rp))))
		  ((= (%p-cdr-code loc) cdr-nil) (return nil)))))
	))))

))

; From file OZ:KANSAS:<L.NETWORK.CHAOS>QFILE.LISP.360 at 12-Apr-85 22:50:16
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; QFILE  "

(DEFMETHOD (QFILE-ACCESS :CREATE-DIRECTORY) (FILE ERROR)
  (DIRECTORY-OPERATION-CHAOS :CREATE-DIRECTORY SELF FILE ERROR "Create Directory"))

))

; From file OZ:KANSAS:<L.ZWEI>ZMACS.LISP.521 at 13-Apr-85 03:14:44
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

(DEFCOM COM-SAVE-ALL-FILES "Offer to write out any changed buffers.
A numeric argument causes the query to be skipped." ()
  (LET ((*QUERY-IO* *STANDARD-OUTPUT*)
	SAVE-WORD-ABBREVS
	BUFFERS-TO-BE-SAVED)
    ;; Ask about each buffer.
    ;; If user says save, and buffer has no file, ask for file now.
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (BUFFER-NEEDS-SAVING-P BUFFER)
	   (OR *NUMERIC-ARG-P*
	       (FQUERY () "Save file ~A ? " (BUFFER-NAME BUFFER)))
	   (PROGN
	     (READ-BUFFER-PATHNAME-FOR-SAVING BUFFER)
	     (PUSH BUFFER BUFFERS-TO-BE-SAVED))))
    ;; Ask, similarly, about saving word abbrevs.
    (SETQ SAVE-WORD-ABBREVS
	  (WORD-ABBREVS-NEED-SAVING-P *NUMERIC-ARG-P*))
    (OR *NUMERIC-ARG-P*
	(AND *WINDOW* (OR SAVE-WORD-ABBREVS BUFFERS-TO-BE-SAVED)
	     (FORMAT T "~&Saving now.")))
    ;; Now save the things the user has already said should be saved.
    (LET ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
      (DOLIST (BUFFER (NREVERSE BUFFERS-TO-BE-SAVED))
	(SAVE-BUFFER BUFFER))
      (AND SAVE-WORD-ABBREVS
	   (COM-WRITE-WORD-ABBREV-FILE-INTERNAL))))
  (AND *WINDOW*
       ;; if numeric arg we only want printing at bottom of window.
       (FORMAT *QUERY-IO* "~&Done.~:[~%~]" *NUMERIC-ARG-P*))
  DIS-NONE)

))

; From file OZ:KANSAS:<L.ZWEI>COMF.LISP.101 at 13-Apr-85 04:15:29
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMF  "

(DEFCOM COM-QUICK-DOCUMENTATION "Prints documentation for the function point is at.
Prints the documentation string of the function which point is inside a call to.
With a numeric argument, reads the name of the function to document
from the mini buffer." ()
  (LET ((NAME (RELEVANT-FUNCTION-NAME (POINT))))
    (IF *NUMERIC-ARG-P*
	(SETQ NAME (READ-FUNCTION-NAME "Brief Document" NAME T)))
    (IF (NULL NAME) (BARF)
      (LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	(IF (NULL DOC)
	    (FORMAT *QUERY-IO* "~&~S is not documented~@[ as a function~]"
		    NAME (CLI:SOME #'CDR (GET NAME 'SI::DOCUMENTATION-PROPERTY)))
	  (IF (FDEFINEDP NAME)
	      (PROGN (SEND *STANDARD-OUTPUT* :FRESH-LINE)
		     (PRINT-ARGLIST NAME *STANDARD-OUTPUT*))
	    (FORMAT T "~S:" NAME))
	  (FORMAT T "~%~A" DOC)))))
  DIS-NONE)

(DEFCOM COM-BRIEF-DOCUMENTATION "Prints brief documentation for the specified function.
Reads the name of the function from the mini-buffer (the default is
the /"current/" function from the buffer) and prints the first
line of its documentation in the echo area." ()
  (LET ((NAME (READ-FUNCTION-NAME "Brief Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (IF (NULL NAME) (BARF)
      (LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
	(IF (NULL DOC) (FORMAT *QUERY-IO* "~&~S is not documented~@[ as a function~]"
			       NAME (CLI:SOME #'CDR (GET NAME 'SI::DOCUMENTATION-PROPERTY)))
	  (FORMAT *QUERY-IO* "~&~S: ~A" NAME
		  (SUBSTRING DOC 0 (STRING-SEARCH-CHAR #/NEWLINE DOC)))))))
    DIS-NONE)

(DEFCOM COM-LONG-DOCUMENTATION "Prints long documentation for the specified symbol or function.
Reads the name of the function or symbol from the mini-buffer
/(the default is the /"current/" function from the buffer).
First comes the arglist of the name as a function, if it is defined,
and then the documentation of the function.
Then, if the name is a symbol, comes the documentation for the name in its other roles." ()
  (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)))))
    (LET ((DOC (DOCUMENTATION NAME 'FUNCTION))
	  (ALL-DOC (AND (SYMBOLP NAME) (GET NAME 'SI::DOCUMENTATION-PROPERTY))))
      (IF (NOT (FDEFINEDP NAME))
	  (FORMAT T "~S:" NAME)
	(SEND *STANDARD-OUTPUT* :FRESH-LINE)
	(PRINT-ARGLIST NAME *STANDARD-OUTPUT*))
      (IF DOC (FORMAT T "~&~A" DOC))
      (COND ((CLI:SOME #'CDR ALL-DOC)
	     (LOOP FOR (KIND . STRING) IN ALL-DOC BY 'CDDR
		   UNLESS (OR (NULL STRING) (STRING= KIND 'FUNCTION))
		     DO (FORMAT T "~2&Documentation of ~S as a ~(~A~):~%~A~%"
				NAME KIND STRING)))
	    ((NOT DOC)
	     (FORMAT *QUERY-IO* "~&~S is not documented." NAME)))))
  DIS-NONE)


))
