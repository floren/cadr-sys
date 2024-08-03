;;; -*- Mode:LISP; Package:TFRAME; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Macros used in TFRAME system
;;;
;;; -dg 6/18/85
;;;

(declare (special *comment-output*))

(defun fcomment (&rest format-args)
  "This simply lexpr-funcalls format on FORMAT-ARGS with *GRAPH* as the
   steam argument"
  (lexpr-funcall #'format *comment-output* format-args))

(defmacro append-new (list thing)
  "Similar to PUSH-NEW but appends."
  `(unless (memq ,thing ,list)
     (setq ,list (nconc ,list (list ,thing)))))

(defmacro with-status ((string . format-args) &body body)
  "Temporarily makes the status pane (small strip of a window across the middle of the
   screen) display a message, created by passing STRING and FORMAT-ARGS to FORMAT,
   while BODY is being evaluated (in a PROGN).
   After the evaluation of the body, the value of the last form in the body
   is returned (as expected) and reverts the status pane to its previous
   state (thus WITH-STATUS forms can be nested)."
  `(unwind-protect
       (progn (send *status* :show-status (format nil ,string . ,format-args))
	      . ,body)
     (send *status* :clear-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command and Option definition macros
;;;

(defstruct tframe-command
  name
  left
  middle
  mouse-documentation
  documentation)
    
(defun tframe-commands-equalp (a b)
  (eq (tframe-command-name a) (tframe-command-name b)))

(defmacro define-command (name (&rest modes) mouse-documentation . options)
  "Defines a menu-command NAME in menu mode MODE to have the effect FORM.
   Uses the :VALUE keyword for the menu item (the `value' just gets thrown away
   so these comands should be purely side-effect functions).
   MOUSE-DOCUMENTATION is the documentation line the who line displays when the 
   mouse is pointing as the item.

   Options handled:

   :LEFT or :MIDDLE - Each a form to be evaluated when the corresponding button is clicked

   :RIGHT is reserved for long documentation

   :MENU-INIT-FORMS - A list of menu init forms that is appended to the item this command
                      represents.  See Window System Manual: Menu Items.

   :DOCUMENTATION - A string describing in detail what the command does according to
                         any options.  If this options is specified, the MOUSE-DOCUMENTATION
                         string is appended with \"[R: Document Command]\" and clicking the 
                         right mouse button will print the string provided to *standard-output*.

   :EXECUTION-CONSTRAINTS - A list of execution constraints.  This should be generalized
                            a bit more.  The only constraint supported at this time is
                            :TAPE-ALLOCATED."
  `(eval-when (eval load)
     (let ((str (make-tframe-command :name ',name
				     :left ',(getf options :left)
				     :middle ',(getf options :middle)
				     :mouse-documentation ,(string-append mouse-documentation
									  " [R: Document self]")
				     :documentation ,(getf options :documentation))))
       (dolist (mode ',modes)
	 (unless (memq mode *mode-types*)
	   (setq *mode-types* (append *mode-types* (ncons mode))))
	 (let ((old (car (mem 'tframe-commands-equalp str (get mode :commands)))))
	   (setf (get mode :commands)
		 (if old
		     (subst str old (get mode :commands))
		   (nconc (get mode :commands) (ncons str)))))))))

(defstruct tframe-option
  name
  print-name
  default-value
  documentation
  type
  type-args)

(defun tframe-options-equalp (a b)
  (eq (tframe-option-name a) (tframe-option-name b)))

(defmacro define-option (name (&rest modes) print-name
			 default-value
			 (type &rest type-args)
			 documentation)
			 
  `(progn 'compile
	  (eval-when (eval load)
	    (let ((str (make-tframe-option :name ',name
					   :print-name ,print-name
					   :default-value ',default-value
					   :documentation ,documentation
					   :type ',type
					   :type-args ',type-args)))
	      ,(if (null modes)
		   `(let ((old (mem 'tframe-options-equalp str *global-options*)))
		      (setq *global-options*
			    (if old
				(substitute str old *global-options*)
			      (nunion *global-options* (ncons str)))))
		 `(dolist (mode ',modes)
		    (unless (memq mode *mode-types*)
		      (setq *mode-types* (append *mode-types* (ncons mode))))
		    (let ((old (car (mem 'tframe-options-equalp str (get mode :options)))))
		      (setf (get mode :options)
			    (if old
				(subst str old (get mode :options))
			      (nconc (get mode :options) (ncons str)))))))))
	  (define-closed-variable ,name ,default-value ,documentation)))

(defmacro define-closed-variable (symbol &optional reset-form documentation)
  `(progn 'compile
	  (defvar ,symbol :unbound ,documentation)
	  (putprop ',symbol ',reset-form :reset-form)
	  (setq *tframe-closure-variables*
		(nunion *tframe-closure-variables* (ncons ',symbol)))))
