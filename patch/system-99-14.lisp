;;; -*- Mode:LISP; Package:USER; Patch-File:T; Base:10; Readtable:T -*-
;;; Patch file for System version 99.14
;;; Reason:
;;;  Improvements to zmacs Add Patch (Buffer) Changed Sections
;;;  EH:
;;;   command arg reading cutesiness
;;;   c-m-a, c-m-l, etc, return "error printing" objects better
;;;   new commands: m-t and c-m-t to look at temporaries pushed on the stack
;;;  trace :cond does right thing
;;;  losing tourism
;;;  plug some packet leakage
;;; Written 01-Dec-84 13:07:52 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with Experimental System 99.11, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, microcode 320, GC@2.


(eval-when (eval compile load)
  (globalize (intern "SG-PLIST" 'si) 'system))

(eval-when (eval load)
  (unless (find-package 'lt)
    (make-package 'language-tools :nicknames '(lt losing-tourist)
		  :prefix-name 'lt :use '(global) :size 500.)
    (rename-package 'global 'global '(zetalisp zl))
    (rename-package 'cli 'lisp '(cl cli))
    (let ((lisp (find-package 'cli)))
      (si::pkg-rehash lisp 2000.)
      (do-local-symbols (s 'global)
	(unless (intern-soft (symbol-name s) lisp) (import s lisp)))
      (import '(nil) lisp)
      (setq si::pkg-lisp-package lisp))))

; From file SGDEFS.LISP KANSAS:<L.SYS2> OZ: (54)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SGDEFS  "

(deff SG-PLIST 'sg-ucode)
(setf (documentation 'sg-plist 'function)
  "Not clear yet. Debugger could use this for communication")

))

; From file SGDEFS.LISP KANSAS:<L.SYS2> OZ: (55)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; SGDEFS  "

(let ((%inhibit-read-only t))
  (setf (nth 6 stack-group-head-leader-qs) 'sg-plist))

(defconst sg-accumulators '(
  SG-AC-K
  SG-AC-S
  SG-AC-J 
  SG-AC-I
  SG-AC-Q
  SG-AC-R
  SG-AC-T
  SG-AC-E
  SG-AC-D
  SG-AC-C 
  SG-AC-B
  SG-AC-A
  SG-AC-ZR
  ))

))

; From file EH.LISP KANSAS:<L.SYS2> OZ: (338)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "

(DEFMACRO PRINT-CAREFULLY (&BODY BODY)
  "If BODY gets an error, print a message saying /"error printing/"."
  `(CONDITION-BIND ((ERROR 'PROCEED-WITH-ABORT-PRINTING))
     . ,BODY))

(DEFUN P-PRIN1-CAREFUL (LOCATIVE &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the contents of LOCATIVE, catching and reporting errors in printing."
  (IF (%P-CONTENTS-SAFE-P LOCATIVE)
      (PRINT-CAREFULLY "printing" (PRIN1 (CONTENTS LOCATIVE) STREAM))
    (SI:PRINTING-RANDOM-OBJECT (NIL STREAM)
      (IF (Q-DATA-TYPES (%P-DATA-TYPE LOCATIVE))
	  (PRINC (Q-DATA-TYPES (%P-DATA-TYPE LOCATIVE)) stream)
	(FORMAT STREAM "Data-type #o~O" (%P-DATA-TYPE LOCATIVE))))))

))

; From file POSS.LISP KANSAS:<L.ZWEI> OZ: (89)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; POSS  "

(DEFUN SECTION-POSSIBILITY (BP SECTION)
  (DECLARE (IGNORE BP))
  (DISPLAY-SECTION SECTION T NIL))

(DEFUN DISPLAY-SECTION (SECTION POINT-PDL-PUSH REDISPLAY-NOW)
  "Move point to the start of SECTION and redisplay.
If POINT-PDL-PUSH, then push (POINT) onto the point-pdl-buffer
if REDISPLAY-NOW, force redisplay once we have moved (POINT)."
  (IF (NOT (MEMQ (NODE-SUPERIOR SECTION) *ZMACS-BUFFER-LIST*))
      (FORMAT *QUERY-IO* "~&The section ~A is in a buffer that has been killed." SECTION)
    (LET* ((SECTION-NODE-DEFUN-LINE (SECTION-NODE-DEFUN-LINE SECTION))
	   (SECTION-BP (AND SECTION-NODE-DEFUN-LINE
			    (IF (NEQ (LINE-TICK SECTION-NODE-DEFUN-LINE) 'DELETED)
				(CREATE-BP SECTION-NODE-DEFUN-LINE 0)
			      (INTERVAL-FIRST-BP SECTION)))))
      (IF (NOT SECTION-BP)
	  (FORMAT *QUERY-IO* "Cannot find section ~A." SECTION)	;unreal section (deleted, eg)
	(IF POINT-PDL-PUSH (POINT-PDL-PUSH (POINT) *WINDOW* T))
	(MAKE-BUFFER-CURRENT (NODE-SUPERIOR SECTION))
	(MOVE-BP (POINT) SECTION-BP)
	(LET ((DIS (RECENTER-WINDOW *WINDOW* :START (BACKWARD-OVER-COMMENT-LINES (POINT) NIL))))
	  (IF REDISPLAY-NOW (REDISPLAY *WINDOW*) DIS))))))
))

; From file PATED.LISP KANSAS:<L.ZWEI> OZ: (29)
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; PATED  "

(DEFUN ADD-PATCH-BUFFER-CHANGED-FUNCTIONS (BUFFER)
  (LET (PROCEED-FLAG)
    (RESECTIONIZE-BUFFER BUFFER)
    (MAKE-BUFFER-CURRENT BUFFER)
    (DOLIST (SECTION (NODE-INFERIORS BUFFER))
      (AND (TYPEP SECTION 'SECTION-NODE)
	   (SECTION-NODE-DEFUN-LINE SECTION)
	   (LET ((PATCH-TICK (GET SECTION 'PATCH-TICK)))
	     (> (NODE-TICK SECTION) (OR PATCH-TICK (BUFFER-FILE-READ-TICK BUFFER))))
	   (LET ((NAME (SECTION-NODE-NAME SECTION)))
	     (DISPLAY-SECTION SECTION NIL T)
	     (WHEN (OR PROCEED-FLAG
		       (CASE (FQUERY '(:CHOICES
					(((:PROCEED "Proceed.") #/P)
					 ((:QUIT "Quit") #/Q #/c-G #/Abort)
					 . #,FORMAT:Y-OR-N-P-CHOICES))
				     "Add ~S to patch? " NAME)
			 (:PROCEED (SETQ PROCEED-FLAG T))
			 (:QUIT (RETURN-FROM ADD-PATCH-BUFFER-CHANGED-FUNCTIONS :QUIT))
			 ((T) T)))
	       (ADD-PATCH-INTERVAL SECTION NIL T NAME BUFFER))))))
  NIL)

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun command-loop (error-sg error-object
		     &aux function sexp 
		     (evalhook nil)
		     special-commands
		     (window-error-handler-old-window nil)
		     io-buffer
		     reading-command)
  (when error-object
    (setq special-commands (send error-object :special-command :which-operations))
    (send error-object :initialize-special-commands))
  (when (setq io-buffer (send *standard-input* :send-if-handles :io-buffer))
    (%bind (locf (tv:io-buffer-output-function io-buffer)) 'io-buffer-output-function)
    (%bind (locf (tv:io-buffer-input-function io-buffer)) nil))
  (inheriting-variables-from (error-sg)  ;Do this every time around the loop in case of setq
    (catch-error-restart ((sys:abort error) "Return to debugger command loop.")
      (catch 'quit
	(show-function-and-args error-sg)
	(warn-about-special-variables error-sg)
	(unless *inhibit-debugger-proceed-prompt*
	  (describe-proceed-types error-sg error-object)))))
  (error-restart (abort "Return to debugger command loop")
    (do ((numeric-arg nil nil)
	 (-)
	 (+ (symeval-in-stack-group '- error-sg))
	 (++ (symeval-in-stack-group '+ error-sg))
	 (+++ (symeval-in-stack-group '++ error-sg))
	 (* (symeval-in-stack-group '* error-sg))
	 (** (symeval-in-stack-group '** error-sg))
	 (*** (symeval-in-stack-group '*** error-sg))
	 (// (symeval-in-stack-group '// error-sg))
	 (//// (symeval-in-stack-group '//// error-sg))
	 (////// (symeval-in-stack-group '////// error-sg))
	 (*values* (symeval-in-stack-group '*values* error-sg)))
	(())
      (inheriting-variables-from (error-sg)  ;Do this every time around the loop in case of setq
	(unless error-handler-running
	  (setq error-depth 1))
	(catch-error-restart ((sys:abort error) "Return to debugger command loop.")
	  (catch 'quit
	    (fresh-line *standard-output*)
	    (dotimes (i error-depth)
	      (send *standard-output* :tyo #/))
	    (do-forever				;This loop processes numeric args
	      ;; Read the next command or sexp, with combined rubout processing.
	      (multiple-value-setq (function sexp)
		(command-loop-read))
	      ;; If it's a character, execute the definition or complain.
	      (cond ((numberp function)
		     (setq numeric-arg
			   (cond ((null numeric-arg)
				  (format t " Argument: ~C"
					  (if (eq function -1) #/- (digit-char function)))
				  function)
				 ((eq function -1)
				  (beep)
				  numeric-arg)
				 (t
				  (format t "~C" (digit-char function))
				  (+ function (* 10. numeric-arg))))))
		    (function
		     (if numeric-arg (tyo #/space))
		     (format t "~C " sexp)
		     (let ((eh-command-char sexp))
		       (return (if (not numeric-arg)
				   (funcall function error-sg error-object)
				   (funcall function error-sg error-object numeric-arg)))))
		    ;; If there was no command, there was a sexp, so eval it.
		    (t
		     (catch 'quit
		       (setq +++ ++ ++ + + -)
		       (let (values)
			 (unwind-protect
			     (setq values (sg-eval-in-frame error-sg (setq - sexp) current-frame t))
			   (push (unless (eq values error-flag) values) *values*))
			 (unless (eq values error-flag)
			   (setq ////// //// //// //)
			   (setq *** ** ** *)
			   (setq // values * (car //))
			   (dolist (value //)
			     (terpri)
			     (print-carefully () (funcall (or prin1 #'prin1) value))))))
		     (return))))))))))
))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun command-loop-read ()
  (prog (char sexp flag function)
    retry
       ;; Read a character.
       (let ((reading-command t))
	 (setq char (send *standard-input* :tyi)))
       ;; Now, if the char is special, echo and return it.
;character lossage
       (cond ((rassq char *proceed-type-special-keys*)
	      (return 'com-proceed-specified-type char))
	     ((rassq char *special-command-special-keys*)
	      (return 'com-special-command char))
	     ((or ( 0 (char-bits char))
		  (command-lookup char))
	      (when (setq function (command-lookup char))
		(if (eq function 'com-number)
		    (setq function (or (digit-char-p (char-code char))
				       -1)))	;minus-sign typed
		(return function char)))
	     ((char= char #/rubout) (go retry)))	;Ignore rubouts
       ;; Otherwise, unread it and read an s-exp instead.
       (send *standard-input* :untyi char)
       (multiple-value (sexp flag)
	 (with-input-editing (*standard-input*  '((:full-rubout :full-rubout)
						  (:activation = #/end)
						  (:prompt " Eval: ")))
	   (si:read-for-top-level)))
       (when (eq flag :full-rubout)
	 (go retry))
       (return nil sexp)))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "


(defun got-values (star minus barf)
  (terpri)
  (if barf (princ barf)
    (setq ////// //// //// // // (list star))
    (setq *** ** ** * * star)
    (setq +++ ++ ++ + + - - minus)
    (print-carefully nil (funcall (or prin1 #'prin1) star))))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "


(defun show-adi (rp idx)
  "Print the ADI at index IDX in the regular pdl RP"
  (format t "~%Additional information supplied with call:")
  (do ((idx idx (- idx 2)))
      (())
    (let ((type (ldb %%adi-type (aref rp idx)))
	  (more-p (%p-ldb %%adi-previous-adi-flag (aloc rp (1- idx)))))
      (select type
	((adi-return-info adi-used-up-return-info)
	 (show-mv-specs rp idx))
	(adi-bind-stack-level
	 (format t "~% Binding stack level: ~S" (aref rp (1- idx))))
	(adi-restart-pc
	 (format t "~% Restart PC on ~S: ~D" 'throw (aref rp (1- idx))))
	(otherwise
	 (format t "~% ~S" (nth type adi-kinds))))
      (if (zerop more-p) (return)))))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "


(defun show-all (sg &aux rp function)
  "Print everything about the current frame, including locals and disassembly."
  (setq rp (sg-regular-pdl sg)
	function (rp-function-word rp current-frame))
  (send *standard-output* :clear-screen)
  ;; Print the header, including the function name
  (format t "Frame address ~D" current-frame)
  (unless (zerop (rp-adi-present rp current-frame))
    (terpri)
    (show-adi rp (- current-frame 4)))
  (terpri)
  (if (typep function 'compiled-function)
      (show-all-macro sg current-frame)
    (show-function-and-args sg)))

(defun show-frame-for-bug-message (sg frame &aux rp function)
  (setq rp (sg-regular-pdl sg)
	function (rp-function-word rp frame))
  (unless (zerop (rp-adi-present rp frame))
    (terpri)
    (show-adi rp (- frame 4)))
  (terpri)
  (if (typep function 'compiled-function)
      (show-all-macro sg frame t)
    (print-function-and-args sg frame t)))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun com-print-variable-frame-value (sg error-object &optional ignore)
  "Print the value of a special variable within the context of the current stack frame.
Prompts for the variable name."
  (declare (ignore error-object))
  (terpri)
  (multiple-value-bind (var full-rubout)
      (with-input-editing (*standard-input* '((:full-rubout :full-rubout)
					      (:activation = #/end)
					      (:prompt "Value in this frame of special variable: ")))
	(si:read-for-top-level))
    (if (eq full-rubout ':full-rubout) (return-from com-print-variable-frame-value nil))
    (multiple-value-bind (value boundflag loc)
	(symeval-in-stack-group var sg current-frame)
      (if boundflag
	  (got-values value loc nil)
	(princ "Void")))))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun com-show-stack-temporaries (sg ignore &optional arg
				   &aux (*print-level* error-message-prinlevel)
				        (*print-length* error-message-prinlength)
				   (rp (sg-regular-pdl sg)))
  "With no argument, show temporary values pushed onto stack by this stack frame.
With an argument of 0, also displays pending open frames, and arguments pushed
 for them (presumably as arguments)
With an argument of -1, displays each word of the regular pdl from the current frame
 to the pdl-pointer. You don't -really- want to do this, do you?"
  (cond ((eq arg -1)
	 (let ((end (sg-regular-pdl-pointer sg))
	       (rp (sg-regular-pdl sg)))
	   (loop for i from (1+ current-frame) below end do
		 (format t "~%~4D: " i)
		 (p-prin1-careful (locf (aref rp i))))))
	(t
	 (loop for this-frame = current-frame then (sg-previous-open sg this-frame)
	       for firstp = t then nil
	       while (and this-frame ( this-frame (sg-regular-pdl-pointer sg)))
	       as function = (rp-function-word rp this-frame)
	       as n-locals = 0 and nargs = 0 and nargs-expected = 0
	       as rest-arg-value = nil and rest-arg-p = nil and lexpr-call = nil
	       until (and (not firstp) (or (eq arg 0)
					   (sg-frame-active-p sg this-frame)
					   (eq function #'foothold)))
	    do (if (eq function #'foothold)
		   (show-foothold sg this-frame)
		 (when (sg-frame-active-p sg this-frame)
		   (when (legitimate-function-p function)
		     (setq nargs-expected
			   (ldb %%arg-desc-max-args (args-info function)))
		     (setq nargs (sg-number-of-spread-args sg this-frame))
		     (multiple-value-setq (rest-arg-value rest-arg-p lexpr-call)
		       (sg-rest-arg-value sg this-frame))
		     (setq n-locals (fef-number-of-locals function))))
		 (let* ((prev-open (sg-previous-open sg this-frame))
			(total (- (if prev-open (- prev-open 4) (sg-regular-pdl-pointer sg))
				  this-frame)))
		   (catch-error (format t "~2&~S" (function-name function)) nil)
		   (when (typep function 'compiled-function)
		     (format t " (P.C. = ~D)" (rp-exit-pc rp this-frame)))
		   (format t "~%Frame index ~D" this-frame)
		   (format t "~%~D arg~:P, ~D local~:P, ~D total in frame"
			   nargs n-locals total)
		   (if (not (zerop (rp-adi-present rp this-frame)))
		       (show-adi rp (- this-frame 4)))
		   (do ((i 0 (1+ i))
			(index (1+ this-frame) (1+ index)))
		       (( i total))
		     (block printed
		       (cond ((< i nargs)
			      (and nargs-expected
				   (= i nargs-expected)
				   (format t "~%  --Extraneous args:--"))
			      (format t "~%  Arg ~D: " i))
			     ((and (= i nargs) rest-arg-p)
			      (format t "~%Rest arg: "))
			     ((and (= i nargs) lexpr-call)
			      (format t "~%Extraneous rest arg: ")
			      (p-prin1-careful (locf rest-arg-value))
			      (return-from printed))
			     ((< i (+ n-locals nargs))
			      (format t "~%Local ~D: " (- i nargs)))
			     (t (format t "~% Temp ~D: " (- i nargs n-locals))))
		       (p-prin1-careful (locf (aref rp index)))))))
	       (terpri)))))

(defun show-foothold (sg frame)
  (let ((end-of-foothold-data (- (sg-previous-open sg frame) 4))
	(rp (sg-regular-pdl sg)))
    (do ((i (1+ frame) (+ i 2))
	 (sg-q si::stack-group-head-leader-qs (cdr sg-q)))
	((> i end-of-foothold-data))
      (format t "~&~30S: " (car sg-q))
      (let ((dtp (ldb (byte (byte-size %%q-data-type) 0) (aref rp (1+ i)))))
	(if (memq (car sg-q) si::sg-accumulators)
	    (format t "#<~:[Data type ~O~;~:*~A~*~] ~O>" (q-data-types dtp) dtp (%pointer (aref rp i)))
	  (setq dtp (%make-pointer dtp (aref rp i)))
	  (p-prin1-careful (locf dtp)))))))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun com-arglist (sg &rest ignore)
  "Returns the arglist for the current stack frame function."
  (let ((function (rp-function-word (sg-regular-pdl sg) current-frame)))
    (multiple-value-bind (arglist values)
	(arglist function nil)
      (format t "~&Argument list for ~S is ~S~@[  ~S~].~%"
	      (function-name function) arglist values)))
  nil)

(defun com-get-arg (sg ignore &optional (arg 0))
  "Sets * to the nth arg to the current function, where n is a numeric argument (default 0)
Also sets + to a locative to the arg."
  (multiple-value-bind (a b barf)
     (sg-frame-arg-value sg current-frame arg nil)
    (got-values a b barf)))

(defun com-get-local (sg ignore &optional (arg 0))
  "Sets * to the nth local of he current function, where n is a numeric argument
/(default 0) Also sets + to a locative to the local."
  (multiple-value-bind (a b barf)
      (sg-frame-local-value sg current-frame arg nil)
    (got-values a b barf)))

(defun com-get-value (sg ignore &optional (arg 0))
  "Sets * to the nth value being returned by the current function, where n is a numeric
argument (default 0) Also sets + to a locative to the value."
  (multiple-value-bind (a b barf)
      (sg-frame-value-value sg current-frame arg nil)
    (got-values a b barf)))

(defun com-get-function (sg ignore &optional ignore)
  "Sets * to the current function, and + to a locative to the it."
  (let ((loc (locf (rp-function-word (sg-regular-pdl sg) current-frame))))
    (got-values (contents loc) loc nil)))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun com-get-stack-temporary (sg ignore &optional (arg 0))
  "Sets * to the nth temporary pushed onto the stack, where is a numeric argument
/(default 0) Also sets + to a locative to the local."
  (multiple-value-bind (a b barf)
      (sg-frame-stack-temporary-value sg current-frame arg nil)
    (got-values a b barf)))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defconst command-dispatch-list
	  '((#/? com-help)
	    (#/help com-help)
	    (#/line com-down-stack)
	    (#/form com-clear-and-show)
	    (#/return com-up-stack)
	    (#/resume com-proceed)
	    (#/abort com-abort)
	    (#/rubout com-flush-numeric-arg)
	    
	    (#/c-- com-number)
	    (#/c-0 com-number 10.)		;control-digits
	    (#/c-A com-arglist)
	    (#/c-B com-short-backtrace)
	    (#/c-C com-proceed)
	    (#/c-D com-proceed-trap-on-call)
	    (#/c-E com-edit-frame-function)
	    (#/c-I com-set-macro-single-step)
	    (#/c-L com-clear-and-show)
	    (#/c-M com-bug-report)
	    (#/c-N com-down-stack)
	    (#/c-P com-up-stack)
	    (#/c-R com-return-a-value)
	    (#/c-S com-search)
	    (#/c-T com-throw)
	    (#/c-X com-toggle-frame-trap-on-exit)
	    (#/c-Z com-top-level-throw)
	    
	    (#/m-- com-number)
	    (#/m-0 com-number 10.)		;meta-digits
	    (#/m-< com-top-stack)
	    (#/m-> com-bottom-stack)
	    (#/m-B full-backtrace)
	    (#/m-D com-toggle-trap-on-call)
	    (#/m-L com-clear-and-show-all)
	    (#/m-N com-down-stack-all)
	    (#/m-P com-up-stack-all)
	    (#/m-R com-reinvoke-new-args)
	    (#/m-S com-print-variable-frame-value)
	    (#/m-T com-show-stack-temporaries)
	    (#/m-X com-set-all-frames-trap-on-exit)
	    
	    (#/c-m-- com-number)
	    (#/c-m-0 com-number 10.)		;control-meta-digits
	    (#/c-m-A com-get-arg)
	    (#/c-m-B full-backtrace-uninteresting)
	    (#/c-m-F com-get-function)
	    (#/c-m-H com-print-frame-handlers)
	    (#/c-m-L com-get-local)
	    (#/c-m-N com-down-stack-uninteresting)
	    (#/c-m-P com-up-stack-uninteresting)
	    (#/c-m-Q com-describe-proceed-types)
	    (#/c-m-R com-return-reinvocation)
	    (#/c-m-S com-print-frame-bindings)
	    (#/c-m-T com-get-stack-temporary)
	    (#/c-m-U com-up-to-interesting)
	    (#/c-m-V com-get-value)
	    (#/c-m-W com-window-error-handler)
	    (#/c-m-X com-clear-all-frames-trap-on-exit)
	    
	    (#/s-A com-proceed-specified-type 26.)
	    )
  "List of elements (character command-symbol) from which EH:COMMAND-DISPATCH-TABLE is initialized.")

(makunbound 'command-dispatch-table)
(assure-dispatch-set-up)

))

; From file EH.LISP KANSAS:<L.SYS2> OZ: (338)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "


(defun eh-stack-temporary (&optional (number 0) (errorp t))
  (multiple-value-bind (val1 nil barf)
      (sg-frame-stack-temporary-value eh-sg eh-frame number errorp)
    (if barf (format *error-output* "~&~A" barf) val1)))

(defun set-eh-stack-temporary (number value)
  (multiple-value-bind (nil loc)
      (sg-frame-stack-temporary-value eh-sg eh-frame number t)
    (and loc (setf (contents loc) value))))


(defun eh-stack-temporary-location (number)
  (multiple-value-bind (nil loc)
      (sg-frame-stack-temporary-value eh-sg eh-frame number t)
    loc))

(defsetf eh-stack-temporary set-eh-stack-temporary)
(deflocf eh-stack-temporary eh-stack-temporary-location)
(deff stack-temporary 'eh-stack-temporary)

))

; From file EH.LISP KANSAS:<L.SYS2> OZ: (338)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "

(DEFUN SG-FRAME-ARG-VALUE (SG FRAME ARGNUM &OPTIONAL (ERRORP T))
  "Return the value and location of arg number ARGNUM in FRAME in SG.
Checks ARGNUM for being in bounds, if the frame is active
/(for an open frame, it is not known how many args there are).
The second value is where the value is located when SG is running;
this may be a symbol value cell, etc., if the arg is special.
ERRORP non-NIL means signal an error if ARGNUM is invalid.
ERRORP NIL means retrun a third value which describes the problem, if any."
  (DECLARE (VALUES VALUE LOCATION BARF))
  (CHECK-TYPE ARGNUM (OR SYMBOL STRING NUMBER))
  (LET* ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
	 (NUM-ARGS (SG-NUMBER-OF-SPREAD-ARGS SG FRAME))
	 (REST-ARG-P (AND (LEGITIMATE-FUNCTION-P FUNCTION)
			  (LDB-TEST #o2402 (ARGS-INFO FUNCTION))))
	 ARG-NAME ERROR-STRING)
    (IF (SYMBOLP ARGNUM)
	(OR (DOTIMES (I NUM-ARGS)
	      (IF (STRING= (STRING (ARG-NAME FUNCTION I)) (STRING ARGNUM))
		  (RETURN (SETQ ARGNUM I))))
	    ;; If this function takes a rest arg and we have
	    ;; specified its name, handle it (it is local number 0).
	    (AND REST-ARG-P
		 (STRING= (STRING (LOCAL-NAME FUNCTION 0)) (STRING ARGNUM))
		 (RETURN-FROM SG-FRAME-ARG-VALUE
		   (IF (CONSP FUNCTION)
		       (VALUES (SG-REST-ARG-VALUE SG FRAME) T)
		       (SG-FRAME-LOCAL-VALUE SG FRAME 0))))))
    (COND ((SYMBOLP ARGNUM)
	   (SETQ ERROR-STRING "No arg named ~S"))
	  ((< ARGNUM 0)
	   (SETQ ERROR-STRING "No argument number ~D, silly!"))
	  (T
	   (SETQ ARG-NAME (ARG-NAME FUNCTION ARGNUM))
	   (WHEN (AND ( ARGNUM NUM-ARGS) (SG-FRAME-ACTIVE-P SG FRAME))
	     (LET ((LOC (NTHCDR (- ARGNUM NUM-ARGS)
				(AND REST-ARG-P (SG-REST-ARG-VALUE SG FRAME)))))
	       (IF LOC
		   (RETURN-FROM SG-FRAME-ARG-VALUE
		     (VALUES (CAR LOC) (LOCF (CAR LOC))))
		 (SETQ ERROR-STRING "Argument number ~D is out of range in current frame"))))))
    (IF ERROR-STRING
	(IF ERRORP
	    (FERROR NIL ERROR-STRING ARGNUM)
	    (VALUES NIL NIL (FORMAT NIL ERROR-STRING ARGNUM)))
      ;; Is this variable bound special in THIS frame?
      (MULTIPLE-VALUE-BIND (START END)
	  (SG-FRAME-SPECIAL-PDL-RANGE SG FRAME)
	(WHEN START
	  (DO ((SP (SG-SPECIAL-PDL SG))
	       (I START (+ 2 I)))
	      (( I END))
	    (AND (EQ ARG-NAME (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I))))
		 ;; Yes, it is, so return its special binding
		 ;; and that binding's location when the SG is running.
		 (RETURN-FROM SG-FRAME-ARG-VALUE
		   (MULTIPLE-VALUE-BIND (VALUE NIL LOCATION)
		       (SYMEVAL-IN-STACK-GROUP ARG-NAME SG FRAME T)
		     (VALUES VALUE LOCATION)))))))
      (VALUES (AREF (SG-REGULAR-PDL SG) (+ FRAME ARGNUM 1))
	      (LOCF (AREF (SG-REGULAR-PDL SG) (+ FRAME ARGNUM 1)))))))

))

; From file EH.LISP KANSAS:<L.SYS2> OZ: (338)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "


(DEFUN SG-FRAME-LOCAL-VALUE (SG FRAME LOCALNUM &OPTIONAL (ERRORP T))
  "Return the value and location of local variable number LOCALNUM in FRAME in SG.
Checks LOCALNUM for being in bounds.
The second value is where the value is located when SG is running;
this may be a symbol value cell, etc., if the arg is special.
ERRORP non-NIL means signal an error if ARGNUM is invalid.
ERRORP NIL means retrun a third value which describes the problem, if any."
  (DECLARE (VALUES VALUE LOCATION BARF))
  (CHECK-TYPE LOCALNUM (OR SYMBOL STRING NUMBER))
  (LET* ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))
	 (NUM-LOCALS (SG-NUMBER-OF-LOCALS SG FRAME))
	 LOCAL-NAME ERROR-STRING)
    (IF (SETQ ERROR-STRING
	      (OR (IF (SYMBOLP LOCALNUM)
		      (DOTIMES (I NUM-LOCALS "No local named ~S")
			(WHEN (STRING= (STRING (LOCAL-NAME FUNCTION I)) (STRING LOCALNUM))
			  (SETQ LOCALNUM I)
			  (RETURN NIL))))
		  (IF (NOT (< -1 LOCALNUM NUM-LOCALS))
		      "Local number ~D is out of range in current frame")))
	(IF ERRORP (FERROR NIL ERROR-STRING LOCALNUM)
	  (VALUES NIL NIL (FORMAT NIL ERROR-STRING LOCALNUM)))
      (SETQ LOCAL-NAME (LOCAL-NAME FUNCTION LOCALNUM))
      ;; Is this variable bound special in THIS frame?
      (MULTIPLE-VALUE-BIND (START END)
	  (SG-FRAME-SPECIAL-PDL-RANGE SG FRAME)
	(WHEN START
	  (DO ((SP (SG-SPECIAL-PDL SG))
	       (I START (+ 2 I)))
	      (( I END))
	    (AND (EQ (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I)))
		     LOCAL-NAME)
		 ;; Yes, it is, so return its special binding
		 ;; and that binding's location when the SG is running.
		 (RETURN-FROM SG-FRAME-LOCAL-VALUE
		   (MULTIPLE-VALUE-BIND (VALUE NIL LOCATION)
		       (SYMEVAL-IN-STACK-GROUP LOCAL-NAME SG FRAME T)
		     (VALUES VALUE LOCATION))))))
	(LET* ((RP (SG-REGULAR-PDL SG))
	       (RPIDX (+ LOCALNUM FRAME (RP-LOCAL-BLOCK-ORIGIN RP FRAME))))
	  (VALUES (AREF RP RPIDX)
		  (LOCF (AREF RP RPIDX))))))))

))

; From file EH.LISP KANSAS:<L.SYS2> OZ: (338)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "


(defun sg-frame-stack-temporary-value (sg frame number &optional (error t))
  "Return the value of the NUMBER'th temporary pushed onto the stack in FRAME.
The second value returned is the location where the temporary is stored."
  (declare (values value location barf))
  (check-type number number)
  (let* ((rp (sg-regular-pdl sg))
	 (function (rp-function-word rp frame))
	 (n-locals 0) (nargs 0))
    (when (sg-frame-active-p sg frame)
      (when (legitimate-function-p function)
	(setq nargs (sg-number-of-spread-args sg frame))
	(setq n-locals (fef-number-of-locals function))))
    (let* ((prev-open (sg-previous-open sg frame))
	   (upper (if prev-open (- prev-open 4) (sg-regular-pdl-pointer sg))))
      ;;>> took out test for ( number 0)
      (if (< (setq number (+ frame n-locals nargs 1 number)) upper)
	  (values (aref rp number) (locf (aref rp number)))
	(let ((string "There are only ~D temporar~:@P in this stack frame"))
	  (if error
	      (ferror nil string (- upper frame n-locals nargs 1))
	    (values nil nil (format nil string (- upper frame n-locals nargs 1)))))))))

))

; From file EH.LISP KANSAS:<L.SYS2> OZ: (338)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EH  "


(DEFUN SG-FRAME-VALUE-VALUE (SG FRAME NUMBER &OPTIONAL CREATE-SLOT)
  "Return the value of the NUMBER'th value being returned by FRAME.
The second value is the location where that value is stored.
CREATE-SLOT means make sure there is a slot for the value to live in;
this makes a difference if FRAME has been asked to return arbitrarily
many values with MULTIPLE-VALUE-LIST."
  (IF CREATE-SLOT
      (CHECK-TYPE NUMBER NUMBER)
      (CHECK-TYPE NUMBER (OR NUMBER NULL)))
  (MULTIPLE-VALUE-BIND (VALUE-LIST TAIL-LOCATION NUM-ALREADY)
      (SG-FRAME-VALUE-LIST SG FRAME)
    (IF (NULL NUMBER)
	VALUE-LIST
      (AND CREATE-SLOT ( NUM-ALREADY NUMBER)
	   (MULTIPLE-VALUE (VALUE-LIST TAIL-LOCATION NUM-ALREADY)
	     (SG-FRAME-VALUE-LIST SG FRAME (1+ NUMBER))))
      (LET ((SLOT (AND ( NUMBER 0) (NTHCDR NUMBER VALUE-LIST))))
	(AND SLOT (VALUES (CAR SLOT)
			  (LOCF (CAR SLOT))))))))

(let ((%inhibit-read-only t)
      (x (assq 'values (debugging-info 'symeval-in-stack-group))))
  (setf (caddr x) 'boundflag)
  (setf (cadddr x) 'location))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (234)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun com-print-frame-bindings (sg error-object &optional count
				 &aux start end (sp (sg-special-pdl sg))
				 self-included special-vars)
  "Lists the names and values of all special variables bound by this frame."
  error-object
  ;; Find range of special pdl for this frame together with
  ;; all uninteresting frames called by it.
  (do ((previous-int (sg-previous-interesting-active sg current-frame
						     innermost-visible-frame))
       (previous current-frame (sg-previous-active sg previous innermost-visible-frame)))
      ((equal previous previous-int))
    (multiple-value-bind (maybe-start maybe-end)
	(sg-frame-special-pdl-range sg previous)
      (setq start maybe-start)
      (and maybe-end (setq end (1+ maybe-end)))))
  (cond (start
	 (if (null count) (format t "~&Names and values of specials bound in this frame:~%"))
	 ;; Now look through the specials bound in this frame.
	 ;; Maybe print one or all, but in any case compute SELF-INCLUDED
	 ;; and SPECIAL-VARS.
	 (do ((i start (+ i 2)))
	     (( i end))
	   (push (symbol-from-value-cell-location (aref sp (1+ i)))
		 special-vars)
	   (if (eq (car special-vars) 'self)
	       (setq self-included i))
	   (when (or (null count) (= count (lsh (- i start) -1)))
	     (format t "~:[~% ~S: ~;~&Value of ~S in this frame:~&   ~]"
		     count (car special-vars))
	     (multiple-value-bind (val error)	;Value
		 (catch-error (funcall (or prin1 #'prin1) (aref sp i)) nil)
	       (cond (error (princ "void"))
		     (count (got-values val (value-cell-location (car special-vars)) nil))))))
	 ;; List the instance variable of SELF also if SELF is bound in this frame.
	 (if (and self-included
		  (typep (aref sp self-included) 'instance)
		  ;; But not if want only one variable and already got it.
		  (or (null count)
		      ( count (lsh (- end start) -1))))
	     (let* ((self-value (aref sp self-included))
		    (self-flavor 
		     (si::instance-flavor self-value))
		    (self-vars (si::flavor-all-instance-variables-slow self-flavor)))
	       (unless count
		 (format t "~2&Non-special instance variables of ~S~&" self-value))
	       (do ((sv self-vars (cdr sv))
		    (count-unspecial (lsh (- end start) -1))
		    (i 1 (1+ i)))
		   ((null sv))
		 (when (and (not (memq (car sv) special-vars))
			    (or (null count)
				(= (1+ count) (incf count-unspecial))))
		   (format t "~:[~% ~S: ~;~&Value of instance variable ~S within ~S:~&   ~]"
			   count (car sv) 'self)
		   (multiple-value-bind (val error)
		       (catch-error (funcall (or prin1 #'prin1)
					     (%instance-ref self-value i)) nil)
		     (cond (error (princ "void"))
			   (count (got-values val (%instance-loc self-value i) nil)))))))))
	(t (format t "~&No specials bound in this frame"))))

))

; From file QTRACE.LISP KANSAS:<L.SYS2> OZ: (151)
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; QTRACE  "

(DEFUN TRACE-1 (SPEC)
  (PROG (BREAK EXITBREAK WHEREIN COND ENTRYCOND EXITCOND STEPCOND ARGPDL ENTRY EXIT
	 (ARG T) (VALUE T) STEP (BARFP T)
	 ENTRYVALS EXITVALS MUMBLE FCN SPEC1 TRFCN ERROR
	 (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
	(IF (ATOM SPEC)
	    (SETQ FCN SPEC)
	  (COND ((EQ (CAR SPEC) ':FUNCTION)
		 (SETQ FCN (CADR SPEC) SPEC (CDR SPEC)))
		((ATOM (CAR SPEC))
		 (SETQ FCN (CAR SPEC)))
		(T (RETURN (LOOP FOR FCN IN (CAR SPEC)
				 NCONC (TRACE-1 `(:FUNCTION ,FCN . ,(CDR SPEC)))))))
	  (DO ((SPECS (CDR SPEC) (CDR SPECS)))
	      ((NULL SPECS))
	    (CASE (CAR SPECS)
	      (:BREAK (SETQ BARFP SPECS SPECS (CDR SPECS) BREAK (CAR SPECS)))
	      (:EXITBREAK (SETQ BARFP SPECS SPECS (CDR SPECS) EXITBREAK (CAR SPECS)))
	      (:STEPCOND (SETQ BARFP SPECS SPECS (CDR SPECS) STEPCOND (CAR SPECS)
			       STEP T))
	      (:STEP (SETQ STEP T))
	      (:ERROR (SETQ ERROR T))
	      (:COND (SETQ BARFP SPECS SPECS (CDR SPECS) COND (CAR SPECS)))
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
	       (IF (MEMQ (CAR SPECS) '(:ARG NIL)) (SETQ VALUE NIL))
	       (IF (MEMQ (CAR SPECS) '(:VALUE NIL)) (SETQ ARG NIL))
	       (AND ARG (SETQ ENTRYVALS (CDR SPECS)))
	       (AND VALUE (SETQ EXITVALS (CDR SPECS)))
	       (RETURN NIL))
	      (OTHERWISE
	       (SETQ MUMBLE (CAR SPECS))
	       (RETURN NIL)))
	    (AND (NULL BARFP) (FERROR NIL "Parameter missing"))))
	(SETQ FCN (DWIMIFY-ARG-PACKAGE FCN 'FCN))
	(UNTRACE-1 FCN)
	(AND MUMBLE (RETURN (FERROR NIL "Meaningless TRACE keyword: ~S" MUMBLE)))
	(CHECK-TYPE ARGPDL SYMBOL)
	(SETQ SPEC1 (UNENCAPSULATE-FUNCTION-SPEC FCN 'TRACE))
	
	(SETQ TRFCN (ENCAPSULATE SPEC1 FCN 'TRACE
	   `(PROG* (,@(AND ARGPDL `((,ARGPDL (CONS (LIST (1+ ,COPY) ',FCN ARGLIST)
						   ,ARGPDL))))
		    (VALUES NIL)
		    (,COPY (1+ ,COPY))
		    (TRACE-LEVEL (1+ TRACE-LEVEL)))
		   (DECLARE (SPECIAL ,COPY VALUES))
		   ;; End of PROG var list.
		   ,(IF ERROR
			`(PROGN (LET ((EH::ERROR-DEPTH (1+ EH::ERROR-DEPTH))
				      (EH::CONDITION-PROCEED-TYPES '(:NO-ACTION)))
				  (EH:INVOKE-DEBUGGER
				    (MAKE-CONDITION 'EH::TRACE-BREAKPOINT
						    "~S entered" ',FCN)))
				(RETURN (APPLY ,ENCAPSULATED-FUNCTION ARGLIST)))
		        `(COND ((OR INSIDE-TRACE
				    ,(AND COND `(NOT ,COND))
				    ,(AND WHEREIN `(NOT (FUNCTION-ACTIVE-P ',WHEREIN))))
				(RETURN (APPLY ,ENCAPSULATED-FUNCTION ARGLIST)))
			       (T (LET ((INSIDE-TRACE T))
				    ,(TRACE-MAYBE-CONDITIONALIZE ENTRYCOND
				       `(TRACE-PRINT ,COPY 'ENTER ',FCN ',ARG
						      ',ENTRY ',ENTRYVALS))
				    ,@(AND BREAK
					   `((AND ,BREAK (LET ((INSIDE-TRACE NIL))
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
					   `((AND ,EXITBREAK
						  (LET ((INSIDE-TRACE NIL))
						    (BREAK "Exiting ~S." ',FCN)))))
				    (RETURN-LIST VALUES))))))))
	(SET TRFCN 0)
	(PUSH FCN TRACED-FUNCTIONS)
	(IF (OR TRACE-COMPILE-FLAG COMPILE-ENCAPSULATIONS-FLAG)
	    (COMPILE-ENCAPSULATIONS SPEC1 'TRACE))
	(RETURN (NCONS FCN))))

))


; From file GC.LISP KANSAS:<L.SYS2> OZ: (172)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN GC-STATUS (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print various statistics about garbage collection."
  (UNLESS %GC-FLIP-READY
    (FORMAT STREAM "~&~:[Incremental~;Batch~] garbage collection now in progress."
	    GC-BATCH-THIS-TIME))
  (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
      (GC-GET-COMMITTED-FREE-SPACE)
    (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE NIL NIL OLD-SIZE)
	(GC-GET-SPACE-SIZES)
      (FORMAT STREAM "~&Dynamic (new+copy) space ~:D, Old space ~:D, Static ~:D,"
	      DYNAMIC-SIZE OLD-SIZE STATIC-SIZE)
      (COND (%GC-FLIP-READY
	     (FORMAT STREAM "~%Free space ~:D, with ~:D needed for garbage collection
 assuming ~D% live data (~S = ~D)."
		     FREE-SPACE
		     (FLOOR (* GC-FLIP-RATIO
			       (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE)))
		     (FLOOR (* GC-FLIP-RATIO 100.))
		     'GC-FLIP-RATIO GC-FLIP-RATIO)
	     (LET ((DISTANCE
		     (- FREE-SPACE
			(FLOOR (* GC-FLIP-RATIO
				  (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE)))))
		   (SAFE-DISTANCE
		     (- FREE-SPACE
			(FLOOR (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
				  (+ COMMITTED-FREE-SPACE
				     COMMITTED-FREE-SPACE-FUDGE)))))
		   (NONINC-DISTANCE
		     (- FREE-SPACE
			(FLOOR (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
				  (+ (GC-GET-COMMITTED-FREE-SPACE T)
				     COMMITTED-FREE-SPACE-FUDGE))))))
	       (COND ((PLUSP DISTANCE)
		      (IF (ASSQ GC-PROCESS ACTIVE-PROCESSES)
			  (FORMAT STREAM "~%A")
			  (FORMAT STREAM "~%If GC is turned on, a"))
		      (FORMAT STREAM " flip will happen in ~:D words." DISTANCE))
		     ((MINUSP NONINC-DISTANCE)
		      (FORMAT STREAM "~%It is ~:D words too late to do garbage collection of any sort."
			      (- NONINC-DISTANCE)))
		     ((MINUSP SAFE-DISTANCE)
		      (FORMAT STREAM
			      "~%It is ~:D words too late to do incremental garbage collection
 but batch (~S, or ~S set to ~S)
 is still safe for ~:D more words."
			      (- SAFE-DISTANCE) 'GC-IMMEDIATELY 'GC-RECLAIM-IMMEDIATELY T
			      NONINC-DISTANCE))
		     (T (FORMAT STREAM "~%A flip should have happened ~:D words ago,
 but it is still safe to turn on GC for ~:D more words."
				(MINUS DISTANCE) SAFE-DISTANCE)))))
	    (T
	     (MULTIPLE-VALUE-BIND (WORK COPYING)
		 (GET-MAX-GC-WORK-REMAINING)
	       (LET ((FREE-SIZE (GET-FREE-SPACE-SIZE)))
		 (FORMAT STREAM "~%Between ~:D and ~:D words of scavenging left to do.
Free space ~:D (of which ~:D might be needed for copying).~:[
Warning: You may require more space for copying than there is freespace, and gc may fail!~;
Ratio scavenging work//free space = ~3F~]"
			 (GET-DIRECT-GC-WORK-REMAINING) WORK FREE-SIZE COPYING
			 (PLUSP (- FREE-SIZE COPYING))
		       (// (FLOAT WORK) (- FREE-SIZE COPYING)))))))))
  (FORMAT STREAM "~&Scavenging during cons ~:[On~;Off~], Idle scavenging ~:[On~;Off~],~%"
	  INHIBIT-SCAVENGING-FLAG INHIBIT-IDLE-SCAVENGING-FLAG)
  (FORMAT STREAM "Automatic garbage collection ~:[Off~;On~].~%"
	  (ASSQ GC-PROCESS ACTIVE-PROCESSES))
  (FORMAT STREAM "GC Flip Ratio ~D, GC Reclaim Immediately ~:[Off~;On~]~%"
	  GC-FLIP-RATIO GC-RECLAIM-IMMEDIATELY)
  (VALUES))

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN EXPAND-MAILING-LISTS (NAMES HOST &AUX RESULT STREAM BAD-ADDRESSES CHAR)
  "Return a list of the recipients of the addresses NAMES at HOST.
Returns either a list of addresses, and a list of bad addresses, or an error instance.
The NAMES should not contain atsigns."
  (DECLARE (VALUES ADDRESSES BAD-ADDRESSES))
  (UNWIND-PROTECT
      (PROGN
	(SETQ STREAM (OPEN-STREAM HOST "EXPAND-MAILING-LIST" :ERROR NIL))
	(IF (ERRORP STREAM) STREAM
	  (DOLIST (NAME NAMES (VALUES (NREVERSE RESULT) (NREVERSE BAD-ADDRESSES)))
	    (SEND STREAM :LINE-OUT NAME)
	    (SEND STREAM :FORCE-OUTPUT)
	    (COND ((CHAR= (SETQ CHAR (SEND STREAM :TYIPEEK)) #/-)
		   (SEND STREAM :LINE-IN)
		   (PUSH NAME BAD-ADDRESSES))
		  ((CHAR= CHAR #/+)
		   (SEND STREAM :LINE-IN)
		   (DO ((LINE (SEND STREAM :LINE-IN T) (SEND STREAM :LINE-IN T)))
		       ((ZEROP (STRING-LENGTH LINE)))
		     (PUSHNEW LINE RESULT :TEST #'STRING-EQUAL)))
		  (T
		   (FERROR () "Unknown character ~C in response" CHAR))))))
    (AND STREAM (NOT (ERRORP STREAM)) (SEND STREAM :CLOSE :ABORT))))

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN RESET-TIME-SERVER (HOST &AUX PKT)
  (SETQ HOST (SI:PARSE-HOST HOST))
  (PRINT-HOST-TIMES (LIST HOST))
  (UNWIND-PROTECT
      (CONDITION-CASE (RESULT)
	  (SETQ PKT (SIMPLE HOST "RESET-TIME-SERVER"))
	(SYS:NETWORK-ERROR
	 (FORMAT *ERROR-OUTPUT* "~&Network error: ")
	 (SEND RESULT :REPORT *ERROR-OUTPUT*)
	 (WRITE-CHAR #/NEWLINE *ERROR-OUTPUT*))
	(:NO-ERROR
	 (FORMAT T "~&Successfully reset the time of ~A." HOST)))
    (AND PKT (RETURN-PKT PKT))))

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (365)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN HOST-UPTIME (HOST &OPTIONAL (STREAM *STANDARD-OUTPUT*) (TIMEOUT 240.) &AUX PKT TIME)
  "Print a human readable time onto stream STREAM.
Returns the uptime (an integer) if host up, NIL if host down."
  (SETQ HOST (SI:PARSE-HOST HOST))
  (UNWIND-PROTECT
      (CONDITION-CASE ()
	  (SETQ PKT (SIMPLE HOST "UPTIME" TIMEOUT))
	(SYS:REMOTE-NETWORK-ERROR
	 (IF STREAM
	     (FORMAT STREAM "Host ~A is apparently not up." HOST))
	 NIL)
	(:NO-ERROR (SETQ TIME (// (DECODE-CANONICAL-TIME-PACKET PKT) 60.))
		   (IF STREAM (TIME:PRINT-INTERVAL-OR-NEVER TIME STREAM))
		   TIME))
    (AND PKT (RETURN-PKT PKT))))

))

; From file SHEET.LISP KANSAS:<L.WINDOW> OZ: (557)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFVAR *WINDOWS-LOCKED-ERROR-QUERY* T
  "T means ask (in cold load stream) what to do about background error with windows locked.
NIL means just wait for user to type Terminal Call or Terminal Meta-Clear-Input.")

(DEFUN WAIT-TILL-SAFE-FOR-ERROR (WINDOW FUNCTION &REST ARGS)
  "Wait until either (APPLY FUNCTION ARGS) is non-NIL or user does Terminal Call.
If user does Terminal Call (and picks WINDOW therein), returns
the symbol COLD-LOAD-STREAM; if FUNCTION returns non-NIL, we return NIL.
If *WINDOWS-LOCKED-ERROR-QUERY* is non-NIL, we immediately
ask user to choose to use cold load stream, unlock window locks, or do nothing.
/"Do nothing/" means we wait, as described above."
  (UNLESS (APPLY FUNCTION ARGS)
    (UNWIND-PROTECT
	(PROGN
	  (WITHOUT-INTERRUPTS (PUSH WINDOW LOCKED-ERROR-WINDOWS))
	  (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
	  (IF *WINDOWS-LOCKED-ERROR-QUERY*
	      ;; Situation has not been resolved yet; query user in cold-load stream
	      (LET (ANSWER)
		(LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
		  (LET ((*QUERY-IO* COLD-LOAD-STREAM))
		    (SEND COLD-LOAD-STREAM :CLEAR-INPUT)
		    (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
		    (SETQ ANSWER
			  (FQUERY '(:CHOICES (((:C "Cold load stream") #/C)
					      ((:U "Clear all locks") #/U)
					      ((:N "Nothing now") #/N)))
				  "How do you want to handle error in process ~A?
You can handle it in the error handler by typing
        C  to use the cold-load stream (like Terminal Call),
        U  to forcibly unlock all windows so a notification can come out 
           (like Terminal Meta-Clear-input)
     or N  to tell it to wait until you do some other thing. "
				  (PROCESS-NAME CURRENT-PROCESS))))
		  (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T))
		(IF (EQ ANSWER :C)
		    ;; Answer C means use cold load stream now.
		    (WITHOUT-INTERRUPTS
		      (SETQ LOCKED-ERROR-WINDOWS
			    (DELQ WINDOW LOCKED-ERROR-WINDOWS 1))))
		;; Answer U means unlock locks, allowing notification now.
		(IF (EQ ANSWER :U) (SHEET-CLEAR-LOCKS))))
	  ;; Wait until either the function supplied to us returns T
	  ;; or someone removes this window from the locked list
	  ;; (which means, telling us to use the cold load stream)
	  (PROCESS-WAIT "Error notify"
			#'(LAMBDA (FUNCTION ARGS WINDOW)
			    (OR (APPLY FUNCTION ARGS)
				(NOT (MEMQ WINDOW LOCKED-ERROR-WINDOWS))))
			FUNCTION ARGS WINDOW)
	  (IF (NOT (MEMQ WINDOW LOCKED-ERROR-WINDOWS))
	      'COLD-LOAD-STREAM))
      (WITHOUT-INTERRUPTS (SETQ LOCKED-ERROR-WINDOWS
				(DELQ WINDOW LOCKED-ERROR-WINDOWS 1))))))

))

; From file BASSTR.LISP KANSAS:<L.WINDOW> OZ: (372)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFCONST KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS
	  '((#/TERMINAL KBD-ESC)
	    (#/SYSTEM KBD-SYS)
	    (#/CONTROL-CLEAR-INPUT KBD-ESC-CLEAR))
  "Default alist of keys handled like Terminal and System.")

))

; From file BASSTR.LISP KANSAS:<L.WINDOW> OZ: (372)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN ADD-ESCAPE-KEY (CHAR FUNCTION &OPTIONAL DOCUMENTATION &REST OPTIONS
		       &AUX C ENTRY BEFORE DURING AFTER (STATE :BEFORE))
  "Add CHAR to the list of actions to be performed and are prefaced by typing TERMINAL.
FUNCTION should be the function to be called when that key is depressed, 
DOCUMENTATION is what to show up when the user types Terminal Help.	
OPTIONS can include either :TYPEAHEAD or :KEYBOARD-PROCESS,
 or :SYSTEM meaning this is a redefinition of system code
 rather than a user overriding the system code."
  (CHECK-TYPE DOCUMENTATION STRING "a valid documentation string")
  (SETQ CHAR (CHAR-UPCASE (GLOBAL:CHARACTER CHAR)))
  (SETQ ENTRY (LIST* CHAR FUNCTION DOCUMENTATION (COPY-LIST OPTIONS)))	;what to store
  (UNLESS (MEMQ ':SYSTEM OPTIONS)
    (PUSH ENTRY *USER-DEFINED-ESCAPE-KEYS*))
  ;;remove character from list.
  (REMOVE-ESCAPE-KEY CHAR)
  ;;logic: before means we haven't found the alphabetics yet, during means we're hacking them
  ;;       now and after means that we are hacking the post alphabetics.
  ;; we also invert the order, so we are really hacking the ones at the end first.
  (DOLIST (ITEM (NREVERSE *ESCAPE-KEYS*))
    (SETQ C (CAR ITEM))
    (AND (EQ STATE ':DURING) (NOT (NULL C)) (NOT (ALPHA-CHAR-P C))
	 (SETQ STATE ':AFTER))
    (AND (EQ STATE ':BEFORE) (NOT (NULL C)) (ALPHA-CHAR-P C)
	 (SETQ STATE ':DURING))
    (CASE STATE
      (:AFTER
       (PUSH ITEM AFTER))
      (:DURING
       (PUSH ITEM DURING))
      (:BEFORE 
       (PUSH ITEM BEFORE))))
  ;; We're all done; now where does that key go?
  (IF (ALPHA-CHAR-P CHAR)
      (PUSH ENTRY DURING)
      (PUSH ENTRY BEFORE))
  (SETQ *ESCAPE-KEYS*				;alphabatize
	(APPEND AFTER (SORTCAR DURING #'ALPHALESSP) BEFORE))
  NIL)

))

; From file BASSTR.LISP KANSAS:<L.WINDOW> OZ: (372)
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; BASSTR  "

(DEFUN ADD-SYSTEM-KEY (CHAR WINDOW-OR-FLAVOR DOCUMENTATION &OPTIONAL (CREATE T))
  "Make typing the System key followed by CHAR select the window WINDOW-OR-FLAVOR.
WINDOW-OR-FLAVOR may be:
  an actual window
  a name of a flavor of window
  a list to evaluate to get a window or a flavor name.
CREATE says whether and how to create a new window if Control-char is pressed.
  It may be:
  T meaning create a window of flavor PROGRAM, or
  the name of a flavor to create, or
  a list to evaluate for effect, to create and select a window.
If CHAR is already defined to select a flavor window, then the old version is
  remembered. To restore the former definition, use (TV:REMOVE-SYSTEM-KEY CHAR)"
  (PUSH (LIST (CHAR-UPCASE (GLOBAL:CHARACTER CHAR)) WINDOW-OR-FLAVOR DOCUMENTATION CREATE)
	*SYSTEM-KEYS*)
  (SETQ *SYSTEM-KEYS* (STABLE-SORTCAR *SYSTEM-KEYS* #'ALPHALESSP)))

))

; From file GENRIC.LISP OZ:<MLY.LL> OZ: (1)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun make-sequence (type size &key (initial-element nil initp))
  "Returns a sequence of SIZE elements, of type TYPE.
Each element is set to INITIAL-ELEMENT.
TYPE must be equivalent to either LIST or some sort of ARRAY.
If the value is a list, it is completely cdr-coded."
  (cond ((eq type 'list)
	 (if initp
	     (make-list size :initial-value initial-element)
	     (make-list size)))
	((or (memq type '(array simple-array vector simple-vector))
	     (and (memq (car-safe type) '(array simple-array vector simple-vector))
		  (eq (cadr type) 't)))
	 (if initp
	     (make-array size :initial-element initial-element)
	     (make-array size)))
	((memq type '(string simple-string))
	 (if initp
	     (make-array size :type art-string :initial-element initial-element)
	     (make-array size :type art-string)))
	(t
	 (let ((xtype (type-canonicalize type nil nil)))
	   (cond
	     ((eq xtype 'list)
	      (make-list size :initial-element initial-element))
	     ((or (memq xtype '(array simple-array))
		  (and (memq (car-safe xtype) '(array simple-array))
			   (memq (cadr xtype) '(t *))))
	      (if initp
		  (make-array size :initial-value initial-element)
		(make-array size)))
	     ((memq (car-safe xtype) '(array simple-array))
	      (if initp
		  (make-array size :element-type (cadr xtype)
				   :initial-element initial-element)
		(make-array size :element-type (cadr xtype))))
	     (t (ferror nil "Invalid sequence type ~S." type)))))))

))

; From file TYPES.LISP OZ:<MLY.LL> OZ: (8)
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

))

; From file QIO.LISP KANSAS:<L.IO> OZ: (216)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

;; BARF! BLETCH! Fuck me HARDER!!!!!
(defun streamp (object)
  "Returns non-NIL if OBJECT is a stream.
This predicate considers the following to be streams:
 Any instance incorporating SI:STREAM or TV:SHEET
 Any function handling either :TYI or :TYO.
 Any symbol with a non-NIL SI:IO-STREAM-P property."
  (or (and (instancep object) (or (typep-structure-or-flavor object 'stream)
				  (typep object 'tv:sheet)))
      ;; Explicit FUNCALLed things that accept messages
      (and (or (closurep object)
	       (entityp object)
	       (and (functionp object)
		    (or (nsymbolp object) (fboundp object))))
	   (arglist object t)
	   (ignore-errors
	     (let ((wo (funcall object :which-operations)))
	       (or (memq :tyo wo)
		   (memq :tyi wo)))))
      (and (symbolp object) (get object 'io-stream-p))))

))

; From file GENRIC.LISP OZ:<MLY.LL> OZ: (3)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun adjoin (item list &key test test-not key)
  "Return either LIST or (CONS ITEM LIST); the latter if no element of LIST matches ITEM.
KEY, if non-NIL, is a function applied ITEM and to each element of LIST
 to get the objects to match.  If KEY is NIL, ITEM and the element itself are used.
TEST is a function passed ITEM (or its key) and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (if (member-1 (if key (funcall key item) item) list test test-not key)
      list
    (cons item list)))

))

; From file EHC.LISP KANSAS:<L.SYS2> OZ: (235)
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; EHC  "

(defun com-reinvoke-new-args (sg ignore &rest ignore)
  "Reinvoke the current function with possibly altered arguments."
  (cond ((null error-handler-running)
	 (format t "You can only examine this stack group, not modify it."))
	((not (sg-frame-active-p sg current-frame))
	 (format t "This frame's args are still being computed;
it cannot be reinvoked since it was never invoked."))
	(t
	 (let* ((form (get-frame-function-and-args sg current-frame))
		(function-name (car form))
		(function (rp-function-word (sg-regular-pdl sg) current-frame))
		(argument-list (cdr form))
		(nargs (length argument-list))
		(args-info (args-info function))
		(args-wanted (ldb %%arg-desc-min-args args-info))
		(rest-flag (ldb-test %%arg-desc-any-rest args-info))
		(max-args (ldb %%arg-desc-max-args args-info))
		new-args
		(*print-level* error-message-prinlevel)
		(*print-length* error-message-prinlength))
	   (format t "~&Reinvoke ~S with possibly altered arguments." function-name)
	   (do ((i 0 (1+ i)))
	       ((unless rest-flag (eq i max-args)))
	     (multiple-value-bind (value flag)
		 (prompt-and-read
		   (let ((keyword (if ( i args-wanted) :eval-read-or-end :eval-read)))
		     (if (< i nargs)
			 (list keyword :default (nth i argument-list))
		         keyword))
		   (if (< i nargs)
		       (if ( i args-wanted)
			   "~&Arg ~D~A, or ~\lozenged-char\ not to change it, or ~C: "
			   "~&Arg ~D~A, or ~\lozenged-char\ not to change it: ")
		       (if ( i args-wanted)
			   "~&Arg ~D~A, or ~*~C: "
			   "~&Arg ~D~A: "))
		   i
		   (format:output nil (display-arg-name " (~A)" function i))
		   #/space #/end)
	       (if (eq flag ':end) (return))
	       (if (eq flag ':default)
		   (prin1 value))
	       (setq new-args (nconc new-args (ncons value)))))
	   (setq form (cons function-name new-args))
	   (when (fquery nil "Reinvoking ~S, OK? " form)
	     (setf (rp-trap-on-exit (sg-regular-pdl sg) innermost-visible-frame) 0)
	     (sg-unwind-to-frame-and-reinvoke sg current-frame form)
	     (leaving-error-handler)
	     (without-interrupts
	       (and error-handler-running
		    (free-second-level-error-handler-sg %current-stack-group))
	       (stack-group-resume sg nil)))))))

))

;; total spazz by mly
;; From file PATHNM.LISP KANSAS:<L.IO.FILE> OZ: (535)
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FILE; PATHNM  "

;(DEFUN DECODE-CANONICAL-TYPE (CANONICAL-TYPE SYSTEM-TYPE)
;  (LET ((PROP (GETF CANONICAL-TYPES CANONICAL-TYPE)))
;    (IF (NULL PROP)
;	(VALUES (STRING CANONICAL-TYPE) NIL)
;      (LET ((PER-SYSTEM (OR (ASSQ SYSTEM-TYPE PROP) (ASSQ NIL PROP))))
;	(VALUES (CADR PER-SYSTEM) (CDR PER-SYSTEM))))))

(define-canonical-type :cadr-microcode "MCR")
(define-canonical-type :cadr-microcode-symbols "SYM")
(define-canonical-type :cadr-microcode-error-table "TBL")
(define-canonical-type :cadr-microcode-locations "LOC")
))