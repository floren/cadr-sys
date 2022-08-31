;;; -*- Mode:Lisp; Common-lisp:NIL; Package:User; Base:8; Patch-File:T -*-
;;; Private patches made by Mly
;;; Written 8/08/84 22:00:31 by Mly
;;; while running on Lisp Machine Four from band 6
;;; with Experimental System 99.0, microcode 314, 99x1-8, links!.

; From file CLPACK.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defmacro defpackage (name &body alist-of-options)
  "Defines (creates or alters) a package named NAME.
Each element of ALIST-OF-OPTIONS looks like (OPTION ARGS...)
Options are:
/(:NICKNAMES names...) specifies alternate names for this package.
/(:PREFIX-NAME name) specifies string to print for this package when printing prefixes.
/(:USE packages...) specifies packages for this one to inherit from.
/(:SHADOW names...) specifies names of symbols to shadow in this package.
/(:EXPORT names...) specifies names of symbols to export in this package.
/(:IMPORT symbols...) specifies symbols to import in this package.
/(:IMPORT-FROM package names...) specifies a package and names of symbols
 to import in this package from that package.
/(:SHADOWING-IMPORT symbols...) specifies symbols to import in this package,
 overriding any name conflicts.
/(:RELATIVE-NAMES (name package)...) specifies local nicknames for
 other packages to be in effect in this package.
/(:RELATIVE-NAMES-FOR-ME (package name)...) specifies local nicknames
 for this package to be in effect in other packages.
/(:AUTO-EXPORT-P t-or-nil) non-NIL specifies that all symbols placed in this package
 should be exported automatically at that time.
/(:SIZE int) specifies the number of symbols to allocate space for initially."
  (if (list-match-p name `(quote ,ignore)) (setq name (cadr name)))
  (check-type name (or string symbol))
  `(let ((sym (intern ',name pkg-user-package))
	 (pkg (defpackage-internal ',name ',alist-of-options)))
     (record-source-file-name sym 'defpackage)
     pkg))

))

; From file CLPACK.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(defun defpackage-internal (name alist-of-options)
  ;; this hair deals with the fact that although having more than one :import-from is a
  ;;  perfectly reasonable thing package-wise, a simple keyword call would only see the
  ;;  first one. So we bundle them all up into one call to IMPORT
  (let ((import nil) (l nil))
    (do ((tail alist-of-options (cdr tail))
	 k args)
	((null tail))
      (setq k (car tail))
      (if (keywordp k)
	  (setq args (cadr tail) tail (cdr tail))
	  (psetq k (car k) args (cdr k)))
      (if (list-match-p args `(quote ,ignore)) (setq args (cadr args)))
      (cond ((eq k :import)
	     (dolist (s args)
	       (push s import)))
	    ((eq k :import-from)
	     (let ((p (pkg-find-package (car args) nil)))
	       (dolist (s (cdr args))
		 (push (intern (string s) p) import))))
	    (t (push k l) (push args l))))
    (apply (if (find-package name) #'alter-package #'make-package)
	   name
	   :import (nreverse import)
	   (nreverse l))))

))
