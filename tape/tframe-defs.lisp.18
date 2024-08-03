;;;  -*- Mode:LISP; Package:TFRAME; Base:10; Readtable:CL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;; 
;;;   Various defs for TFrame
;;; 

(defvar *tframe-closure* :unbound)

(defvar *tframe-closure-variables* nil)

(defvar *whole-frame* :unbound)

(defvar *default-menu-font* fonts:medfnb
  "This is the default font for the command menu")

(defconst *mode-types* nil
  "This is the master list of all valid modes for the menus of this utility.")

(defvar *default-startup-mode* 'control
  "The menu mode to start the window in.")

(defvar *default-pop-up-menu-item-font* fonts:hl12b)

(defvar *global-options* ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Closed-over variable definitions
;;;

(define-closed-variable *menu* (send *whole-frame* :get-pane 'menu-pane))
(define-closed-variable *mode* (send *whole-frame* :get-pane 'menu-mode-pane))
(define-closed-variable *vars* (send *whole-frame* :get-pane 'vars-pane))
(define-closed-variable *status* (send *whole-frame* :get-pane 'status-pane))
(define-closed-variable *interaction* (send *whole-frame* :get-pane 'interaction-pane))
(define-closed-variable *format* (send *whole-frame* :get-pane 'format-pane))
(define-closed-variable *device* (send *whole-frame* :get-pane 'device-pane))

;(define-closed-variable *command-menu-alist*
;  (mapcar #'(lambda (mode)
;	      (cons mode
;		    (let ((command-forms))
;		      (dolist (command *tframe-command-list* command-forms)
;			(when (eq (get command :mode) mode)
;			  (setq command-forms
;				(nconc command-forms
;				       (list (get command :menu-item-form)))))))))
;	  *mode-types*))

;(define-closed-variable *option-menu-alist*
;  (mapcar #'(lambda (mode)
;	      (cons mode 
;		    (let ((option-forms))
;		      (dolist (option *tframe-option-list* option-forms)
;			(when (eq (get option :mode) mode)
;			  (setq option-forms
;				(nconc option-forms
;				       `(,(list option
;						(get option :print-name)
;						(get option :type)
;						(si:eval-special-ok
;						  (get option :type-arg)))))))))))
;	  *mode-types*))

(define-closed-variable *comment-output* (send *whole-frame* :get-pane 'drawing-pane))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command Execution Constraints
;;;
;;;

;;;Special commands in MODE menu.
(defvar *special-mode-menu-commands*
	'(("Refresh"
	   :value (:command (send *whole-frame* :whole-refresh))
	   :documentation "Refresh all the windows in the frame"
	   :font fonts:tr12b)
	  ("Restart"
	   :value (:command (signal 'restart :format-string "Restart TFrame"))
	   :documentation "Restart this frame (resets options and commands)")
	  ("QUIT"
	   :value (:command (send (aref tv:previously-selected-windows 0) :select))
	   :documentation "Leave Tape Utility Frame"))
  "Special commands that are added to the modes for selection in the
   Menu Mode pane")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful Utilities
;;;

(defun wild-pathname-from-directory-file (file &optional (defaults (fs:parse-pathname "*.*#*")))
  (send file :new-pathname
	:directory (if (listp (pathname-directory file))
		       (nconc (pathname-directory file) (list (pathname-name file)))
		     (if (eq (pathname-directory file) :root)
			 (pathname-name file)
		       (list (pathname-directory file) (pathname-name file))))
	:name (pathname-name defaults)
	:type (pathname-type defaults)
	:version (pathname-version defaults)))

