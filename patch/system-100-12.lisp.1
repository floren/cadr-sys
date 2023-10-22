;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.12
;;; Reason:
;;;  MERGE-PATHNAME-COMPONENTS: Fix typo.
;;; Written 10-May-23 13:49:54 by ams,
;;; while running on Lisp Machine One from band 7
;;; with Experimental System 100.11, Hacks by AMS 1.0, microcode 323, AMS.



; From file FC: /tree/io/file/pathnm.lisp at 10-May-23 13:49:54
#8R FILE-SYSTEM#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //tree//io//file//pathnm"

(defun merge-pathname-components
       (pathname &optional defaults
		 &key (default-version nil default-version-specified-p)
		      (default-type nil default-type-specified-p)
		      (default-name nil default-name-specified-p)
		      always-merge-name always-merge-type always-merge-version
		 &aux default new-device new-directory new-name new-type new-version
		      new-otype merge-name-p merge-type-p merge-version-p)
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
If supplied, DEFAULT-NAME, DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
their components if those components are not supplied by PATHNAME.
Otherwise, these components are defaulted from DEFAULTS in the usual manner.
ALWAYS-MERGE-xxx mean that the the xxx components should *always* be merged in
/(from either DEFAULT-xxx or from DEFAULTS) even if the relevant component is already
specified by PATHNAME."
  (setq pathname (parse-pathname pathname nil defaults))
  (if (null defaults) (setq defaults *default-pathname-defaults*))
  (setq default (if (atom defaults)
		    (parse-pathname defaults nil pathname)
		    (default-pathname defaults (pathname-host pathname) nil nil t)))
  ;; Merge the and device and directory in vanilla fashion
  (when (null (pathname-device pathname))
    (setq new-device (pathname-device default)))
  (let ((pdir (pathname-directory pathname))
	(ddir (pathname-directory default)))
  (cond ((null pdir)
	   (setq new-directory ddir))
	  ((eq (car-safe pdir) ':relative)
	   (setq new-directory
		 (merge-relative-directory pdir ddir)))))
  ;; merge name type and version hirsutely
  (when (or (null (pathname-name pathname))
	    always-merge-name)
    (setq new-name (if default-name-specified-p
			  default-name
			  (pathname-name default))
	  merge-name-p t))
  (when (or (null (pathname-type pathname))
	    always-merge-type)
    (setq merge-type-p t)
    (if default-type-specified-p
	(setq new-type default-type)
	(multiple-value-setq (new-type new-otype) (send default :canonical-type))))
  (when (or (null (pathname-version pathname))
	    always-merge-version)
    (setq new-version (if default-version-specified-p
			  default-version
			  (pathname-version default))
	  merge-version-p t))
  (send pathname :new-pathname
		 (if new-device :device) new-device
		 (if new-directory :directory) new-directory
		 (if merge-name-p :name) new-name
		 (if merge-type-p :type) new-type
		 (if new-otype :original-type) new-otype
		 (if merge-version-p :version) new-version))
))
