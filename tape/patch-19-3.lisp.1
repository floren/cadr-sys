;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 19.3
;;; Reason:
;;;  Make *default-dummy-transform-pathname* an instance variable, so
;;;  each instantiation of tape format calculates its own.
;;;  Also put in menu and keywords allowed for :set-options.
;;; Written 18-Jan-88 13:34:08 by keith (Keith M. Corbett) at site LMI
;;; while running on Opus from band 2
;;; with Experimental System 123.174, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 8.4, Experimental Laser1+ 2.0, Experimental Tape 19.2, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta I/site/dvi/patches.



; From modified file OPUS: L.TAPE; TAR-FORMAT.LISP#24 at 18-Jan-88 13:34:20
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAR-FORMAT  "


(defflavor tar-format ((record-size 10240)
		       (file-stream)
		       (op-mode)
		       (ascii-translate? :determine-by-type)
		       (buffer)
		       (buffer-ptr 0)
		       (buffer-fill 0)
		       (eot nil)
		       (tape-state-check)
		       (*default-dummy-transform-pathname* *default-dummy-transform-pathname*))
	   (basic-tape-format)
  :gettable-instance-variables
  (:special-instance-variables *default-dummy-transform-pathname*))

))

; From modified file OPUS: L.TAPE; TAR-FORMAT.LISP#24 at 18-Jan-88 13:48:34
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAR-FORMAT  "


(defmethod (tar-format :set-options) (&rest options)
  (if options
      (do* ((list options (cddr list))
	    (option (car list) (car list))
	    (value (cadr list) (cadr list)))
	   ((null list))
	(case option
	  (:record-size
	   (if (zerop (remainder value 1024))
	       (setq record-size value)
	     (signal 'invalid-option-value
		     :object self
		     :option option
		     :value value)))
	  (:ascii-translate?
	   (if (typep value '(member :determine-by-type :always :query :never))
	       (setq ascii-translate? value)
	     (signal 'invalid-option-value
		     :object self
		     :option option
		     :value value)))
	  (:default-dummy-transform-pathname
	   (if (typep value 'pathname)
	       (setq *default-dummy-transform-pathname* value)
	     (signal 'invalid-option-value
		     :object self
		     :option option
		     :value value)))
	  (t (signal 'invalid-option
		     :object self
		     :option option
		     :value value))))
    (progn
      (setq *default-dummy-transform-pathname*
	    (make-pathname
	      :defaults (or *default-dummy-transform-pathname* (fs:user-homedir))
		:name :wild
		:type :wild
		:version :highest))
      (tv:choose-variable-values
	`((,(locf record-size) "Record Size" :number)
	  (,(locf ascii-translate?) "Perform character set translation?"
	   :choose (:determine-by-type :always :query :never))
	  (*default-dummy-transform-pathname*
	    "Defaults for relative host/directory"
	    :pathname *default-dummy-transform-pathname*))
	:label '(:string "Options for the TAR tape format" :font fonts:tr12b)))))

))
