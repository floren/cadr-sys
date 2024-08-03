;;; -*- Mode:LISP; Package:TFRAME; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; TFRAME process support
;;;
;;; -dg 6/10/85
;;;


(defun make-tframe-closure (bindings &optional (init-p t))
  (let ((closure (closure (copylist bindings) #'funcall)))
    (when init-p
      (reset-closure-variables closure))
    closure))

(defun reset-closure-variables (closure)
  (dolist (c (closure-variables closure))
    (set-in-closure closure c (si:eval-special-ok (get c :reset-form)))))

;;; Frame process stuff

(defsignal restart (si:condition restart) () "Restart condition for TFrame process.")

(defun reconfigure-window (window)
  (send window :set-configuration (send window :configuration)))

(defun tframe-frame-process (window &aux (first-time-p t))
  (let ((*package* (pkg-find-package 'tape)))
    (error-restart-loop (restart "Back to Toplevel TFRAME loop")
      (if first-time-p
	  (setq first-time-p nil)
	(reconfigure-window window))
      (let* ((*whole-frame* window)
	     (*tframe-closure* (make-tframe-closure *tframe-closure-variables* nil))
	     (tape:*selected-format* tape:(cond (*default-format*
						 (lexpr-funcall 'parse-format *default-format*))
						(*tape-format-alist*
						 (parse-format (cdar *tape-format-alist*)))))
	     (tape:*selected-device* tape:(cond ((null *available-devices*))
						((memq (car *default-device*) *available-devices*)
						 (lexpr-funcall 'parse-device *default-device*))
						(*available-devices*
						 (parse-device (car *available-devices*))))))
	(reset-closure-variables *tframe-closure*)
	(funcall *tframe-closure* #'tframe-toplevel)))))

(defun tframe-toplevel ()
  (let ((terminal-io (send *whole-frame* :get-pane 'interaction-pane)))
    (tframe-initializations)
    (error-restart-loop (sys:abort "Back to Toplevel TFRAME loop")
      (tframe-toplevel-1-loop))))

;; Copied from LAD: RELEASE-3.TAPE; TFRAME-PROCESS.LISP#25 on 2-Oct-86 03:50:51
(defconst tframe-help-info
	  "You are typing input to ~A.
Click the rightmost mouse button to select a menu of programs and window operations.
Type Control-~C for a list of commands for editing input.
Type ~C ~C for a list of programs.
Type ~C ~C for a list of console operations.

The tape frame has a mode-oriented interface which limits the available
commands and options to the scope of the types of operations meaningful
under a certain context.  The mode can be changed by clicking on the
appropriate mode name under the heading \"MODES\".  The mode can only be
changed when there is no command being processed.

Each of the commands and options are self documenting.  Click right on a
command or click middle on the current value of an option to see the
documentation.

The SELECTED-DEVICE and SELECTED-FORMAT can be modified or replaced by
clicking an appropriate mouse button on it.  The mouse documentation line
at the bottom of the screen provides the appropriate details when the
mouse cursor is placed over the selected format or device.
~2%")

;; Copied from LAD: RELEASE-3.TAPE; TFRAME-PROCESS.LISP#25 on 2-Oct-86 03:50:48
(defun tframe-toplevel-1-loop ()		;stolen from SI:LISP-TOP-LEVEL1
  si:(let (old-package
	   w-pkg
	   (top-level-p t)
	   (tv:rh-help-info tframe:tframe-help-info))
       (FORMAT T "~&;Reading~:[~; at top level~]~@[ in ~A~]."
	       TOP-LEVEL-P (SEND-IF-HANDLES *TERMINAL-IO* :NAME))
       (PUSH NIL *VALUES*)
       (DO ((*READTABLE* *READTABLE*)
	    (LAST-TIME-READTABLE NIL)
	    THROW-FLAG)				;Gets non-NIL if throw to COMMAND-LEVEL (e.g. quitting from an error)
	   (NIL)				;Do forever
	 ;; If *PACKAGE* has changed, set OLD-PACKAGE and tell our window.
	 ;; Conversely, if the window's package has changed, change ours.
	 ;; The first iteration, we always copy from the window.
	 (COND
	   ;; User set the package during previous iteration of DO
	   ;; => tell the window about it.
	   ((AND OLD-PACKAGE (NEQ *PACKAGE* OLD-PACKAGE))
	    (SEND-IF-HANDLES *TERMINAL-IO* :SET-PACKAGE *PACKAGE*)
	    (SETQ OLD-PACKAGE *PACKAGE*))
	   ;; Window's package has been changed, or first iteration through DO,
	   ;; => set our package to the window's -- if the window has one.
	   ((SETQ W-PKG (SEND-IF-HANDLES *TERMINAL-IO* :PACKAGE))
	    (AND (NEQ W-PKG *PACKAGE*)
		 (SETQ *PACKAGE* W-PKG))
	    (SETQ OLD-PACKAGE *PACKAGE*))
	   ;; First time ever for this window => set window's package
	   ;; to the global value of *PACKAGE*.
	   ((NULL OLD-PACKAGE)
	    (SETQ OLD-PACKAGE *PACKAGE*)
	    (SEND-IF-HANDLES *TERMINAL-IO* :SET-PACKAGE *PACKAGE*)))
	 (CHECK-FOR-READTABLE-CHANGE LAST-TIME-READTABLE)
	 (SETQ LAST-TIME-READTABLE *READTABLE*)
	 (SETQ THROW-FLAG T)
	 (CATCH-ERROR-RESTART ((SYS:ABORT DBG:DEBUGGER-CONDITION) "Return to top level in ~A."
			       (OR (SEND-IF-HANDLES *TERMINAL-IO* :NAME) "current process."))
	   (FRESH-LINE)
	   (SETQ +++ ++ ++ + + -)		;Save last three input forms
	   (SETQ - (read-for-top-level nil nil '((:preemptable) (:activation char= #\end))))
	   (unless
	     (when (listp -)
	       (case (car -)
		 (:menu
		  (select (nth 3 -)
		    (tframe:*menu*
		     (let ((values (multiple-value-list (tframe:process-menu-select -))))
		       (push values *values*)
		       (SETQ /// // // / / VALUES)
		       (SETQ *** ** ** * * (CAR /))
		       (DOLIST (VALUE / (dotimes (c 2) (terpri)))
			 (FRESH-LINE)
			 (FUNCALL (OR PRIN1 #'PRIN1) VALUE))))
		    (tframe:*format* (tframe:process-format-select -))
		    (tframe:*mode* (tframe:process-mode-select -))
		    (tframe:*device* (tframe:process-device-select -))
		    (t (ferror nil "~&The mouse blip ~2%~A~2% is going nowhere!!!" -)))
		  t)
		 (:variable-choice
		  (tv:choose-variable-values-process-message tframe:*vars* -)
		  (send *terminal-io* :select) t)
		 (:mouse-button
		  (if (eq (third -) tframe:*vars*)
		      (tframe:document-option-from-blip -)
		    (tv:beep))
		  t)
		 (t nil)))
	     (LET ((LISP-TOP-LEVEL-INSIDE-EVAL T)
		   VALUES)
	       (UNWIND-PROTECT
		   (SETQ VALUES (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
		 ;; Always push SOMETHING -- NIL if evaluation is aborted.
		 (PUSH VALUES *VALUES*))
	       (SETQ /// // // / / VALUES)
	       (SETQ *** ** ** * * (CAR /)))
	     (DOLIST (VALUE / (terpri))
	       (FRESH-LINE)
	       (FUNCALL (OR PRIN1 #'PRIN1) VALUE)))
	   (SETQ THROW-FLAG NIL))
	 (WHEN THROW-FLAG
	   ;; Inform user of return to top level.
	   (FORMAT T "~&;Back to top level~@[ in ~A~]."
		   (SEND-IF-HANDLES *TERMINAL-IO* :NAME))))))

;; Copied from LAD: RELEASE-3.TAPE; TFRAME-PROCESS.LISP#25 on 2-Oct-86 03:50:50
(defun process-menu-select (blip)
  "This function, highlights the choice, evaluates the item's :VALUE (for side effect),
   and then unhighlights the choice.  (Highlighting is UNWIND-PROTECTed)"
  (let* ((item (cadr blip))
	 (str (car (mem #'(lambda (item str)
			    (eq (car item) (tframe-command-name str)))
			item
			(get (car (send *mode* :current-mode)) :commands)))))
    (unwind-protect
	(progn
	  (send *menu* :add-highlighted-item item)
	  ;; Catch device driver errors.
	  (condition-case (error)
	      (ecase (third blip)
		(1 (si:eval-special-ok (tframe-command-left str)))
		(2 (si:eval-special-ok (tframe-command-middle str)))
		(4 (terpri *standard-output*)
		   (format *standard-output*
			   "Documentation for command ~A:~%~5T~~?~"
			   (car item)
			   (or (tframe-command-documentation str)
			       "No additional documentation."))))
	    (tape:driver-error
	     (format t "~&Device Driver: ~a" (Send error :error-message)))))
      (send *menu* :remove-highlighted-item item))))

(defun process-mode-select (blip)
  "This function processes a blip meant for the mode window"
  ;;; the hair here is because this pane may contain more than a simple mode selection
  (let ((mode (get (cadr blip) :value)))
    (typecase mode
      (:cons (case (car mode)
	       (:command (eval `(progn . ,(cdr mode))))
	       (:ignore)
	       (t (ferror nil "Illegal command from Mode Pane: ~2%~A~%" mode))))
      (:symbol (if (memq mode *mode-types*)
		   (send *mode* ':set-mode mode)
		 (ferror "Illegal MODE (probably incorrectly defined) ~2%~A~%"
			 mode)))
      (t (ferror nil "Unhandled input from Mode Pane: ~2%~A~%" blip)))))

(defun process-vars-select (blip)
  "This is calls a window-system pre-fab function for handling variable-values
   blips... it seems to do the right thing."
  (tv:choose-variable-values-process-message *vars* blip))

(defun document-option-from-blip (blip)
  (let* ((y (nth 4 blip))
	 (line-number (truncate (- y (tv:sheet-inside-top *vars*))
				(send *vars* :line-height)))
	 (option (car (aref (send *vars* :items)
			    (+ (send *vars* :top-item) line-number))))
	 (str (or (car (mem #'(lambda (symbol str) (eq symbol (tframe-option-name str)))
			    option
			    (get (caar (send *mode* :highlighted-items)) :options)))
		  (car (mem #'(lambda (symbol str) (eq symbol (tframe-option-name str)))
			    option
			    *global-options*)))))
    (when str
      (format *standard-output* "~&Documentation for option ~A~%~5T~~?~~2%"
	      option
	      (tframe-option-documentation str)))))

(defun process-format-select (blip)
  (let ((tem tape:*selected-format*))
    (ecase (second blip)
      (:select (tape:select-format))
      (:edit-options (tape:set-format-options))
      (:reset tape:(cond (*default-format*
			  (and (lexpr-funcall 'parse-format *default-format*)
			       (lexpr-funcall 'select-format *default-format*)))
			 (*tape-format-alist*
			  (and (parse-format (cdar *tape-format-alist*))
			       (select-format (cdar *tape-format-alist*)))))))
    (unless (eq tem tape:*selected-format*)
      (refresh-format))))

(defun process-device-select (blip)
  (let ((tem tape:*selected-device*))
    (ecase (second blip)
      (:select (tape:select-device))
      (:edit-options (tape:set-device-options))
      (:reset tape:(cond ((null *available-devices*))
			 ((memq (car *default-device*) *available-devices*)
			  (lexpr-funcall 'parse-device *default-device*))
			 (*available-devices*
			  (parse-device (car *available-devices*))))))
    (unless (eq tem tape:*selected-device*)
      (send tem :deinitialize)
      (refresh-device))))

(defun refresh-format ()
  (send *format* :set-item-list
	`((,(type-of tape:*selected-format*)
	   :buttons (:select
		      :edit-options
		      :reset)
	   :documentation "L: Select New Format {M: Edit Options} R: Reset to default"
	   :font fonts:tr12B))))

(defun refresh-device ()
  (send *device* :set-item-list
	`((,(type-of tape:*selected-device*)
	   :buttons (:select
		      :edit-options
		      :reset)
	   :documentation "L: Select New Device {M: Edit Options} R: Reset to default"
	   :font fonts:tr12B))))

(defun tframe-initializations ()
  (send *mode* :set-mode *default-startup-mode*)
  (refresh-device)
  (refresh-format))
