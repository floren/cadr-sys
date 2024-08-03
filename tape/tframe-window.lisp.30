;;;  -*- Mode:LISP; Package:TFRAME; Base:10; Readtable:CL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;; 
;;;  Window system code for the TFrame
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TFRAME
;;;

(defconst tframe-configuration-alist
	  '((basic-landscape
	      (whole-thing)
	      ((whole-thing :horizontal (:even)
			    (left right)
			    ((left :vertical (90 :characters interaction-pane)
				   (vars-pane status-pane interaction-pane)
				   ((status-pane 1. :lines)
				    (vars-pane :ask :constraint-size)
				    (interaction-pane :even)))
			     (right :vertical (:even)
				    (menu-pane menu-mode-pane device-pane format-pane dummy)
				    ((format-pane 1. :lines)
				     (device-pane 1. :lines)
				     (menu-mode-pane :ask-window menu-mode-pane :constraint-size)
				     (menu-pane :ask :constraint-size)
				     (dummy :blank :black :even)))))))
	    (basic-portrait
	      (top middle status-pane interaction-pane)
	      ((top :horizontal (:limit (150)
					:ask-window menu-mode-pane :constraint-size)
		    (menu-mode-pane object-panes)
		    ((menu-mode-pane .5)
		     (object-panes :vertical (:even)
				   (format-pane device-pane)
				   ((format-pane .5)
				    (device-pane :even)))))
	       (middle :horizontal (:ask-window menu-pane :constraint-size)
		       (menu-pane vars-pane)
		       ((menu-pane 30 :characters)
			(vars-pane :even)))
	       (status-pane 1 :lines)
	       (interaction-pane :even)))))

(defflavor tframe ()
	   (tv:process-mixin
	    tv:select-mixin
	    tv:inferiors-not-in-select-menu-mixin
	    tv:alias-for-inferiors-mixin
	    tv:essential-mouse
	    tv:bordered-constraint-frame-with-shared-io-buffer
	    tv:top-box-label-mixin)
  (:default-init-plist
    :save-bits :delayed
    :io-buffer (tv:make-io-buffer #o512 nil 'tv:kbd-default-output-function)
    :process '(tframe-frame-process :regular-pdl-size 16000 :special-pdl-size 2000)
    :borders 1
    :label '(:string "Setting up TFrame..." :font fonts:metsi :centered)
    :configuration (configure-appropriately)
    :panes `((status-pane status-pane)
	     (menu-mode-pane menu-mode-pane)
	     (interaction-pane interaction-pane)
	     (menu-pane menu-pane)
	     (vars-pane vars-pane)
	     (format-pane object-display-pane
			  :label (:string "Selected Format" :font fonts:hl12i :centered))
	     (device-pane object-display-pane
			  :label (:string "Selected Device" :font fonts:hl12i :centered)))
    :constraints tframe-configuration-alist))

(defmethod (tframe :after :init) (ignore)
  (send self :set-selection-substitute
	(send self :get-pane 'interaction-pane))
  (send self :set-label
	`(:string
	   ,(format nil "LMI Tape Utility Frame (V. ~d)" (or (si:get-system-version 'tape) 0))
	   :font fonts:metsi
	   :centered)))

(defmethod (tframe :before :kill) (&rest ignore)
  (send tape:*selected-device* :unlock-device))

(defmethod (tframe :whole-refresh) (&rest ignore)
  (send (send self :get-pane 'interaction-pane) :home-cursor)
  (send self ':refresh))

(defmethod (tframe :before :change-of-size-or-margins) (&rest ignore)
  (send self :set-configuration (configure-appropriately)))

(defun configure-appropriately ()
  (if (= (send tv:main-screen :width) 1024.)
      'basic-landscape
    'basic-portrait))

(compile-flavor-methods tframe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Status pane
;;;

(defflavor status-pane ((status)
			(old-statuses))
	   (tv:notification-mixin
	    tv:window
	    tv:list-mouse-buttons-mixin
	    )
  (:default-init-plist
    :borders 3
    :border-margin-width 3
    :reverse-video-p t
    :blinker-p nil
    :label nil
    :font-map (list fonts:tr12b)))

(defmethod (status-pane :show-status) (string)
  (send self ':clear-screen)
  (and status (push status old-statuses))
  (setq status string)
  (format self "~A" string)
  t)

(defmethod (status-pane :clear-status) ()
  (send self :clear-screen)
  (setq status (pop old-statuses))
  (if status
      (format self "~A" status)
    (send self :clear-screen))
  nil)

(compile-flavor-methods status-pane)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Options pane
;;;
;;; 

(defflavor vars-pane
	   ((current-item-list nil))
	   (tv:text-scroll-window-empty-gray-hack
	    tv:choose-variable-values-pane-mixin
	    tv:basic-choose-variable-values
	    tv:borders-mixin
	    tv:top-box-label-mixin
	    tv:scroll-stuff-on-off-mixin
	    tv:any-tyi-mixin
	    tv:window)
  (:default-init-plist
    :label '(:string "Options and Variables" :font fonts:hl12i :centered)
    :borders 3
    :border-margin-width 3
    :variables nil
    :font-map (list fonts:cptfont)
    :name-font fonts:cptfont
    :value-font fonts:cptfontb
    :unselected-choice-font fonts:tr10
    :selected-choice-font fonts:tr10b
    :stack-group current-stack-group)
  )

(defmethod (vars-pane :constraint-size) (&rest ignore)
  (* (+ (lexpr-funcall 'max
		       (or (mapcar #'(lambda (mode)
				       (length (get mode :options)))
				   *mode-types*)
			   '(0)))
	4)
     (send self :line-height)))

(compile-flavor-methods vars-pane)  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command menu pane
;;;

(defflavor menu-pane 
	   ()
	   (tv:abstract-dynamic-item-list-mixin
	    tv:menu-highlighting-mixin
	    tv:command-menu)
  (:default-init-plist
    :borders 5
    :border-margin-width 3
    :label '(:string "Commands" :font fonts:hl12i :centered :top)
    :default-font *default-menu-font*
    :item-list nil))

(defmethod (menu-pane :update-item-list) (item-list)
  (send self :set-item-list item-list))

(defmethod (menu-pane :constraint-size) (&rest ignore)
  (* (+ (lexpr-funcall 'max
		 (or (mapcar #'(lambda (mode)
				 (length (get mode :commands)))
			     *mode-types*)
		     '(0)))
	4)
     (send self :line-height)))

(compile-flavor-methods menu-pane)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menu mode pane
;;;

(defflavor menu-mode-pane
	   ((current-mode nil))
	   (tv:menu-highlighting-mixin
	    tv:dynamic-multicolumn-mixin
	    tv:abstract-dynamic-item-list-mixin
	    tv:command-menu)
  (:gettable-instance-variables)
  (:default-init-plist
    :borders 5
    :border-margin-width 3
    :label nil
    :font-map (list fonts:tr12b)
    :default-font fonts:tr12b
    :column-spec-list nil))

(defmethod (menu-mode-pane :after :init) (&rest ignore)
  (send self :setup-items)
  (send self :update-item-list))

(defmethod (menu-mode-pane :setup-items) ()
  (multiple-value-bind (modes specials)
      (menu-mode-lists *mode-types*)
    (send self :set-column-spec-list
	  `(("Modes" ',modes :font fonts:hl12i)
	    ("Special Commands" ',specials :font fonts:hl12i)))))

(defmethod (menu-mode-pane :constraint-size) (&rest ignore)
  (* (+ (max (length *mode-types*)
	     (length *special-mode-menu-commands*))
	3)
     (send self :line-height)))

(defmethod (menu-mode-pane :set-mode) (mode)
  (send self :setup-items)
  (let ((mode-item (assoc mode (send self ':item-list))))
    (and current-mode
	 (send self :remove-highlighted-item current-mode))
    (send self :add-highlighted-item mode-item)
    (setq current-mode mode-item)
    (send *menu* :update-item-list
	  (mapcar #'(lambda (str)
		      (list (tframe-command-name str)
			    :value (tframe-command-name str)
			    :documentation (tframe-command-mouse-documentation str)))
		  (get mode :commands)))
    (send *vars* :set-variables
	  (mapcar #'(lambda (option)
		      `(,(tframe-option-name option)
			,(tframe-option-print-name option)
			:documentation
			"Click Left or Right to change this value.  {M: View documentation}"
			,(tframe-option-type option)
			,@(tframe-option-type-args option)))
		  (append (get mode :options) *global-options*)))))

(defun create-menu-mode-item (mode)
  "Given a mode, create a corresponding menu-item that can be used in the
   menu mode selection pane"
  `(,mode
    :value ,mode
    :documentation ,(format nil
			    "Select ~A mode for Commands and Options menus"
			    mode)))

(defun menu-mode-lists (menu-mode-list)
  "Create a list of menu-items (to give to menu command pane) for the available modes."
  (values (mapcar #'create-menu-mode-item
		  menu-mode-list)
	  *special-mode-menu-commands*))

(compile-flavor-methods menu-mode-pane)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interaction pane
;;;

(defflavor interaction-pane
	   ()
	   (tv:notification-mixin
	    tv:list-mouse-buttons-mixin
	    tv:window)
  (:default-init-plist
    :borders 3
    :font-map (list fonts:cptfont)
    :label '(:string "TFrame Interaction Pane" :font fonts:tr12bi)
    :more-p nil
    :deexposed-typeout-action :permit))

(compile-flavor-methods interaction-pane)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Device and Format Object Panes
;;;

(defflavor object-display-pane ()
	   (tv:command-menu)
  (:default-init-plist
    :borders 5
    :border-margin-width 3
    :item-list nil
    :font-map (list fonts:tr12b)))

(compile-flavor-methods object-display-pane)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cruft 
;;;

(TV:ADD-SYSTEM-KEY #\B 'TFRAME "Tape Utility Frame" T)


