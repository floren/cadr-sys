;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 22.4
;;; Reason:
;;;  Tape Frame gave you Portrait configuration if the screen width was 800
;;;  and Landscape configuration otherwise.  Fancy-Landscape, with screen
;;;  width of 896, looks much better with Portrait configuration.  Also,
;;;  the frame didn't reconfigure if you changed screen width.
;;; Written 2-May-88 16:01:12 by pld at site Gigamos Cambridge
;;; while running on Azathoth from band 3
;;; with Experimental System 123.247, Experimental Local-File 73.4, Experimental FILE-Server 22.2, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 22.3, Experimental Serial-IP 1.1, Experimental Tiger 27.1, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8, the old ones.



; From modified file DJ: L.TAPE; TFRAME-WINDOW.LISP#28 at 2-May-88 16:01:24
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-WINDOW  "


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

))

; From modified file DJ: L.TAPE; TFRAME-WINDOW.LISP#28 at 2-May-88 16:01:28
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-WINDOW  "


(defmethod (tframe :before :change-of-size-or-margins) (&rest ignore)
  (send self :set-configuration (configure-appropriately)))

))

; From modified file DJ: L.TAPE; TFRAME-WINDOW.LISP#28 at 2-May-88 16:01:29
#10R TFRAME#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TFRAME")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TFRAME-WINDOW  "


(defun configure-appropriately ()
  (if (= (send tv:main-screen :width) 1024.)
      'basic-landscape
    'basic-portrait))

))
