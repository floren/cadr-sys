;;; -*- Mode:LISP; Package:TFRAME; Readtable:CL; Base:10 -*-
;;;
;;; This stuff was take out of TFRAME for release 3.0
;;; it will be worked on after the release probably -dg
;;;



(defconst old-tframe-configuration-alist	;next release -dg
	  '((basic-landscape
	      (whole-thing)
	      ((whole-thing :horizontal (:even)
			    (left right)
			    ((left :vertical (80. :characters selectable-item-pane)
				   (selectable-item-pane status-pane interaction-pane)
				   ((selectable-item-pane 30 :lines)
				    (status-pane 1. :lines)
				    (interaction-pane :even)))
			     (right :vertical (:even)
				    (device-pane format-pane menu-mode-pane menu-pane vars-pane)
				    ((format-pane 1. :lines)
				     (device-pane 1. :lines)
				     (menu-mode-pane :ask-window menu-mode-pane :constraint-size)
				     (menu-pane :ask :constraint-size)
				     (vars-pane :even)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Selectable item pane
;;;;
;;; this stuff is for release after 3.0

;(defflavor selectable-item-pane
;	   ((tv:value-array-fill-pointer 0)
;	    (history-alist)
;	    (current-history))
;	   (tv:margin-scroll-mixin
;	    tv:margin-region-mixin
;	    tv:scroll-mouse-mixin
;	    tv:borders-mixin
;	    tv:list-mouse-buttons-mixin
;	    tv:scroll-window
;	    )
;  (:default-init-plist
;    :truncation t
;    :borders 5
;    :scroll-bar 5
;    :label nil
;    :margin-scroll-regions '((:TOP "Top of Interesting Things")
;			     (:BOTTOM "Bottom of Interesting Things")))
;  :gettable-instance-variables
;  :settable-instance-variables)

;(defmethod (selectable-item-pane :before :redisplay) (&rest ignore)
;  (setq tv:display-item (send (cdr current-history) :item-list)))

;(defmethod (selectable-item-pane :after :init) (&rest ignore)
;  (send self :set-current-history "TFrame Default History" nil))

;(defmethod (selectable-item-pane :append-item-top-level) (selectable-item)
;  (check-arg selectable-item (typep selectable-item 'selectable-item)
;	     "a TFRAME:SELECTABLE-ITEM")
;  (let ((inferiors (send (cdr current-history) :inferiors)))
;    (if inferiors
;	(nconc inferiors (ncons selectable-item))
;      (send (cdr current-history) :set-inferiors
;	    (setq inferiors (ncons selectable-item)))))
;  (send self :redisplay))

;(defmethod (selectable-item-pane :add-history) (name)
;  (let ((thing (cons name
;		     (make-instance
;		       'selectable-item
;		       :object name
;		       :print-level 0
;		       :format-string (format nil "-*- Top of History: ~A -*-" name)
;		       :mousable-p t
;		       :type :history))))
;    (if history-alist
;	(push thing history-alist)
;      (setq history-alist (list thing)))
;    thing))

;(defmethod (selectable-item-pane :set-current-history) (name &optional (refresh? t))
;  (let ((obj (ass #'string-equal name history-alist)))
;    (unless obj
;      (setq obj (send self :add-history name)))
;    (setq current-history obj)
;    (setq tv:display-item (send (cdr obj) :item-list))
;    (when refresh?
;      (send self :refresh))))

;(defmethod (selectable-item-pane :reset-history) (&optional name)
;  (let ((history (if name
;		     (ass #'string-equal name history-alist)
;		   current-history)))
;  (send (cdr history) :remove-inferiors)
;  (when (eq history current-history)
;    (setq tv:display-item (send (cdr history) :item-list))
;    (send *items* :refresh))))

;(defun si (&rest args)
;  (lexpr-send *items* args))

;;(defmethod (selectable-item-pane :add-value) (value)
;;  (when (= tv:value-array-fill-pointer (length tv:value-array))
;;    (adjust-array-size tv:value-array (ceiling (* 1.5 (length tv:value-array)))))
;;   (setf (aref tv:value-array tv:value-array-fill-pointer) value)
;;   (incf tv:value-array-fill-pointer)
;;   (values (sub1 tv:value-array-fill-pointer)))
;;
;;(defun create-value-entry (entry)
;;  (let ((item (list :value (send *items* :add-value entry)
;;		    nil (format nil "~A" entry))))
;;    (tv:scroll-parse-item item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Selectable Item definitions
;;;;
;;;; If evolution were this difficult, we wouldn't be here.
;;;;
;;;;                               -dg
;;;;
;;;; This stuff is planned for the next release after 3.0

;(defflavor selectable-item (object
;			    (type)
;			    (mousable-p)
;			    (print-level 0)
;			    (inferiors)
;			    (entry)
;			    (menu-operations)
;			    (format-string)
;			    (random-info)
;			    (saved-inferiors))
;	   ()
;  (:required-init-keywords :object)
;  :gettable-instance-variables
;  :settable-instance-variables
;  :initable-instance-variables)

;(defmethod (selectable-item :after :init) (&rest ignore)
;  (send self :initialize-values))

;(defmethod (selectable-item :initialize-values) (&optional force-p)
;  (when (or force-p (not menu-operations))
;    (send self :recompute-menu-operations))
;  (when (or force-p (not format-string))
;    (send self :recompute-format-string))
;  (when (or force-p (not entry))
;    (send self :recompute-entry)))
  
;(defmethod (selectable-item :recompute-menu-operations) ()
;  (setq menu-operations (determine-menu-operations-for-item self)))

;(defmethod (selectable-item :recompute-format-string) ()
;  (setq format-string
;	(format nil (format nil "~~~DT~~A" print-level) object))
;  (send self :recompute-entry))

;(defmethod (selectable-item :recompute-entry) ()
;  (setq entry
;	(tv:scroll-parse-item
;	  (if mousable-p
;	      `(:mouse (nil :buttons
;			    ((nil :kbd (:selectable-item ,self))
;			     (nil :menu-choose ,(send self :menu-operations))
;			     nil)
;			    :documentation "L: Select this item  M: Menu of operations")
;		       :string ,format-string)
;	    `(:string ,format-string)))))

;(defmethod (selectable-item :print-self) (stream &rest ignore)
;  (format stream "#<Selectable-Item [~S] ~D>" object (%pointer self)))

;(defmethod (selectable-item :menu-call) ()
;  (tv:menu-choose menu-operations))

;(defmethod (selectable-item :item-list) ()
;  (cons nil (cons entry
;		  (mapcar #'(lambda (item)
;			      (send item :item-list))
;			  inferiors))))

;(defmethod (selectable-item :add-inferiors) (new-items &optional (keyword :beginning))
;  (if inferiors 
;      (case keyword
;	(:beginning (nconc new-items inferiors))
;	(:end (nconc inferiors new-items))
;	(:replace (setq inferiors new-items)))
;    (setq inferiors new-items)))

;(defmethod (selectable-item :remove-inferiors) (&optional (items-to-remove))
;  (when inferiors
;    (setq saved-inferiors (or items-to-remove inferiors))
;    (if items-to-remove
;	(dolist (item items-to-remove)
;	  (delq item inferiors))
;      (setq inferiors nil))))

;(defun determine-menu-operations-for-item (item)
;  (let ((fun (get (send item :type) 'menu-alist)))
;    (when fun
;      (cons `(:string ,(format nil "Operations the ~A [~A]"
;			       (or (send item :type) :item)
;			       (send item :object)) :font ,fonts:hl12bi)
;	    (funcall fun item)))))

;(defun (:history menu-alist) (item)
;  `(("Reset"
;     :eval (send *items* :reset-history (send ,item :object))
;     :documentation "Reset this history")))

;(defun (:directory menu-alist) (item)
;  `(("List"
;     :buttons ((nil :eval (progn (insert-directory-list ,item)
;				 (send *items* :redisplay t)))
;	       (nil :no-select)
;	       (nil :eval (progn (remove-directory-list ,item)
;				 (send *items* :redisplay t))))
;     :documentation "L: Insert directory files. R:Remove Directory Files")
;    ("Dired"
;     :eval (dired (wild-pathname-from-directory-file (send ,item :object)))
;     :documentation "Run Dired on this directory in Zmacs")))

;(defun (:file menu-alist) (item)
;  `(("Edit"
;     :buttons ((nil :value (ed (send ,item :object))))
;     :documentation "Edit this file in the editor")
;    ("TFrame info"
;     :buttons ((nil :value (get-tframe-info (send ,item :object)))))))

;(compile-flavor-methods selectable-item)


(define-closed-variable *items* (send *whole-frame* :get-pane 'selectable-item-pane))


;;;; This is targeted for the next release after 3.0. -dg
;;;; Filesystem stuff

;(defun item-directory-list (pathname recursive? print-level)
;  (let ((dirlist (cdr (fs:directory-list pathname))))
;    (mapcar
;      #'(lambda (file)
;	  (if (get file :directory)
;	      (let ((item (make-instance
;			    'selectable-item
;			    :object (car file)
;			    :type :directory
;			    :mousable-p t
;			    :print-level print-level)))
;		(when recursive?
;		  (insert-directory-list item))
;		item)
;	    (make-instance 'selectable-item
;			   :object (car file)
;			   :type :file
;			   :mousable-p t
;			   :print-level print-level)))
;      dirlist)))

;(defun make-directory-item (pathname &key
;			    recursive?
;			    (print-level 1))
;  (make-instance
;    'selectable-item
;    :object pathname
;    :type :directory
;    :mousable-p t
;    :print-level print-level
;    :format-string (format nil "---[Directory list of pathname: ~A]---" pathname)
;    :inferiors (item-directory-list pathname recursive? print-level)))

;(define-command DIRECTORY-LIST filesystem
;  "L: Display a directory on the current host. M: Displays subdirectories"
;  :left (let ((pathname (query-wild-pathname)))
;	  (send *items* :append-item-top-level
;		(make-directory-item pathname)))
;  :middle (let ((pathname (query-wild-pathname)))
;	    (send *items* :append-item-top-level
;		  (make-directory-item pathname
;				       :recursive? t))))

;(defun query-wild-pathname (&optional
;			       (default-pathname (fs:parse-pathname "~;*.*#*" si:local-host)))
;  (let ((pn (prompt-and-read :string-or-nil "Pathname ~A: " default-pathname)))
;    (if pn
;	(fs:merge-pathnames pn default-pathname)
;      default-pathname)))

;(defun insert-directory-list (item)
;  (let ((pn (send item :object)))
;    (send item :set-format-string
;	  (format nil "---[Directory list of pathname: ~A]---" pn))
;    (send item :recompute-entry)
;    (send item :add-inferiors
;	  (or (send item :saved-inferiors)
;	      (item-directory-list pn nil (add1 (send item :print-level)))))))

;(defun remove-directory-list (item)
;  (send item :remove-inferiors)
;  (send item :recompute-format-string))

;(defun process-item-select (item)
;  (case (send item :type)
;    (:file (send *interaction* :force-kbd-input
;		 (format nil "~S" (send item :object))))
;    (t (inspect item))))

