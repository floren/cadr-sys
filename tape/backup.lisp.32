;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;;
;;; Filesystem Backup support for tape software
;;;
;;; -dg 1/13/86
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File logging code
;;;

(defconst *dump-info-list* :unbound)
(defconst *dump-files-list* :unbound)
(defconst *backup-info-consistent* nil
  "If non-NIL, then there are new log files that must be loaded.")
(defvar *last-incremental-backup* nil)
(defvar *last-full-backup* nil)
(defvar *incremental-version-loaded* 0)
(defvar *full-version-loaded* 0)
(defconst *incremental-log-array* (make-array 250 :element-type t))
(defconst *full-log-array* (make-array 100 :element-type t))
(defvar *pathname-component-package*
	(or (si:find-package "TLPCP")
	    (defpackage "TLPCP" (:use) (:size 1000)))
  "Package in which pathname components are interned.")
(defvar *backup-log-format-version* 2)

(defstruct (backup-log (:print-function print-backup-log))
  version
  type
  dump-info-list
  dump-files-list)

(defun print-backup-log (thing str ignore)
  (format str "#<~A Backup-Log ~D>"
	  (backup-log-type thing)
	  (backup-log-version thing)))

(defun test-file-canonical-type (type pathname)
  (string-equal (send pathname :canonical-type) type))

(defun determine-backup-log-source (type log-version)
  (check-type type (member :incremental :full))
  (check-type log-version (integer 1))
  (let* ((files (mapcar 'car
			(cdr
			  (fs:directory-list
			    (fs:parse-pathname (format nil "lm:backup-logs.~A;~D.*#>"
						       type log-version))))))
	 (qfasl (car (mem 'test-file-canonical-type :qfasl files)))
	 (backup-log (car (mem 'test-file-canonical-type :backup-log files))))
    (cond ((and (null qfasl) backup-log)
	   backup-log)
	  ((and (null backup-log) qfasl)
	   qfasl)
	  ((and qfasl
		(>= (send qfasl :version) (if backup-log (send backup-log :version) 0)))
	   qfasl)
	  (backup-log backup-log)
	  (t (ferror nil "No backup log exists for LM:BACKUP-LOGS.~A;~D")))))

(defsubst get-log-array (type)
  (ecase type
    (:incremental *incremental-log-array*)
    (:full *full-log-array*)))

(defun put-log (type number log)
  (let ((array (get-log-array type)))
    (when (> number (array-length array))
      (set (ecase type
	     (:incremental '*incremental-log-array*)
	     (:full '*full-log-array*))
	   (array-grow array (* 2 number))))
    (setf (aref array number) log)))

(defun get-log (type number)
  (let ((array (get-log-array type)))
    (aref array number)))

(defun load-backup-log (type log-number)
  (let ((pathname (determine-backup-log-source type log-number))
	*dump-info-list*
	*dump-files-list*
	(*read-base* 10.))
    (load pathname 'tape)
    (when (= 1 (get *dump-info-list* :log-version))
      (dolist (file *dump-files-list*)
	(setf (nth 1 file) (intern (format nil "~s" (nth 1 file)) *pathname-component-package*))
	(setf (nth 2 file) (intern (format nil "~s" (nth 2 file)) *pathname-component-package*))
	(setf (nth 7 file) (intern (format nil "~S" (nth 7 file)) *pathname-component-package*))))
    (put-log type log-number
	     (make-backup-log :version log-number
			      :type type
			      :dump-info-list *dump-info-list*
			      :dump-files-list *dump-files-list*))))

(defun load-backup-logs (&optional (type :all) force-p)
  (ZL:if (neq type :all)
      (let ((latest-log-version (assess-latest-log-version type)))
	(do ((count 1 (add1 count))
	     finished?)
	    (finished?)
	  (cond ((> count latest-log-version)
		 (setq finished? t))
		((and (get-log type count) (not force-p)))
		(t (load-backup-log type count)))))
    (load-backup-logs :incremental force-p)
    (load-backup-logs :full force-p)))

(defun assess-latest-log-version (type)
  (check-type type (member :incremental :full))
  (let* ((pathname (format nil "lm:backup-logs.~A;*.backup-log#>" type))
	 (files (condition-case ()
		    (cdr (fs:directory-list pathname :no-extra-info))
		  (fs:directory-not-found
		   (format t "~&Creating directory for \"~A\"." pathname)
		   (fs:create-directory pathname)
		   (cdr (fs:directory-list pathname :no-extra-info))))))
    (when files
      (dolist (file files)
	#'(lambda (file)
	    (unless (numberp (read-from-string (send (car file) :name) nil nil))
	      (ferror nil 
		      "There is a file in the directory \"LM:BACKUP-LOGS.~A;\" which should~
                     not be there!~%~ Please remove it and try this operation again!"
		      type))))
      (unless (probef (fs:make-pathname :host si:local-host
					:directory (list "backup-logs" (string type))
					:name (format nil "~d" (length files))
					:type "backup-log"
					:version :newest))
	(ferror nil "There is something wrong with the log file directory \"LM:BACKUP-LOGS.~A\".~%~
                   Please call LMI."
		type)))
    (length files)))

(defun update-log-info (&optional force-p)
  (unless *backup-info-consistent*
    (let ((il (if force-p 0 *incremental-version-loaded*))
	  (fl (if force-p 0 *full-version-loaded*))
	  (iv (assess-latest-log-version :incremental))
	  (fv (assess-latest-log-version :full)))
      (when (> iv il)
	(do ((version (add1 il) (add1 version)))
	    ((> version iv))
	  (load-backup-log :incremental version)
	  (setq *incremental-version-loaded* version)))
      (when (> fv fl)
	(do ((version (add1 fl) (add1 version)))
	    ((> version fv))
	  (load-backup-log :full version)
	  (setq *full-version-loaded* version)))
      (setq *backup-info-consistent* t))))

(defun compile-backup-logs (&optional (type :all) force-p)
  (if (eq type :all)
      (progn (compile-backup-logs :incremental force-p)
	     (compile-backup-logs :full force-p))
    (dolist (file (mapcar 'car
			  (cdr (fs:directory-list
				 (format nil "lm:backup-logs.~A;*.backup-log#>" type)
				 :no-extra-info))))
      (unless (and (null force-p)
		   (probef (send file :new-pathname :canonical-type :qfasl)))
	(format t "~&Compiling backup log: \"~A\" ... " file)
	(compile-file file)
	(format t "done.~%")))))

(defun log-files (file-list host log-file place format user universal-time)
  (check-type file-list cons)
  (check-type host si:host)
  (check-type log-file (or string pathname))
  (check-type place cons)
  (check-type format symbol)
  (check-type user string)
  (check-type universal-time (or integer bignum))
  (let ((*print-base* 10.))
    (with-open-file (f log-file :direction :output :characters t :byte-size 8)
      (format f ";;; -*- Mode: Lisp; Package: tape; Base:10; Readtable:CL -*-~%~
                 ;;;~%;;; Backup log for \"~S\" (format:~A) on host: ~A.~%~
                 ;;; Dumped by ~A on ~\date\.~%;;;~2%"
	      place format host user universal-time)
      (print `(setq *dump-info-list*
		    '(,place
		      :host ,(send host :string-for-printing)
		      :log-file ,(send (send f :truename):string-for-printing)
		      :tape-format ,format
		      :user ,user
		      :time ,universal-time
		      :log-version ,*backup-log-format-version*))
	     f)
      (format f "~&~2%;;; The format for the files is:~%~
                 ;;; (<directory> <name> <type> <version> <characters> <creation-date>~%~
                 ;;;  <byte-size> <author>)~%;;;~2%(setq *dump-files-list*~%'(")
      (dolist (file file-list)
	(format f "(~s ~s ~s ~d ~s ~d ~d ~s)~%"
		(send (car file) :directory)
		(intern (send (car file) :name) *pathname-component-package*)
		(intern (send (car file) :type) *pathname-component-package*)
		(send (car file) :version)
		(get file :characters)
		(get file :creation-date)
		(file-byte-size file)
		(intern (get file :author) *pathname-component-package*)))
      (format f "))~2%;;; End of tape log.~%"))
    (setq *backup-info-consistent* nil)))

(defun find-file-backups (pathname &optional (type :all))
  (if (eq type :all)
      (progn (nconc (find-file-backups pathname :full)
		    (find-file-backups pathname :incremental)))
    (do* (collection
	  (pn (fs:parse-pathname pathname))
	  (count 1 (add1 count))
	  (backup-log (get-log type count)
		      (get-log type count))
	  (dump-info (when backup-log
		       (backup-log-dump-info-list backup-log))
		     (when backup-log
		       (backup-log-dump-info-list backup-log))))
	 ((null backup-log) (reverse collection))
      (dolist (file (backup-log-dump-files-list backup-log))
	(when (backup-log-match pn file)
	  (let ((log-path (pathname-from-backup-file-record file)))
	    (push (cons log-path dump-info) collection)
	    (format t "~&Pathname: \"~A\" -- ~A: ~A~%"
		    log-path (caar dump-info) (cadar dump-info))))))))

(defun pathname-from-backup-file-record (file &optional (host si:local-host))
  (fs:make-pathname :host host
		    :directory (nth 0 file)
		    :name (symbol-name (nth 1 file))
		    :type (symbol-name (nth 2 file))
		    :version (nth 3 file)))

(defun backup-log-match (pathname file)
  (pathname-match (pathname-from-backup-file-record file) pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local file system optimization (for server also)
;;;

(defun slotify-files (file-list &aux alist)
  "Takes a standard file list and returns an alist
   of the form ((<directory> . files) ...) so that we can
   be more efficient in working by directory a la local-file-funcall."
  (do* ((fl file-list (cdr fl))
	(file (if (consp (car fl)) (caar fl) (car fl))
	      (if (consp (car fl)) (caar fl) (car fl)))
	(place (and file (ass #'equal (pathname-directory file) alist))
	       (and file (ass #'equal (pathname-directory file) alist))))
       ((null fl) alist)
    (if place
	(nconc (cdr place)
	       (ncons (list (fs:pathname-name file)
			    (fs:pathname-type file)
			    (fs:pathname-version file))))
      (push (list (pathname-directory file)
		  (list (fs:pathname-name file)
			(fs:pathname-type file)
			(fs:pathname-version file)))
	    alist))))

(defun set-attributes (slot-list attribute-alist)
  (do* ((sl slot-list (cdr sl))
	(dl (car sl) (car sl))
	(dir (and dl (fs:lookup-directory (car dl)))
	     (and dl (fs:lookup-directory (car dl))))
	fobj)
       ((null dl) t)
    (unwind-protect 
	(dolist (file (cdr dl))
	  (setq fobj (lexpr-funcall 'fs:lookup-file (car dl) (append file '(nil nil nil)))) ;REALLY-OPEN NIL
	                                                                                    ;(don't incr OPEN-COUNT)

	  (when fobj
	    (fs:locking (fs:directory-lock dir)
	      (dolist (attribute attribute-alist)
		(fs:locking (fs:file-lock fobj)
		  (fs:set-file-attribute
		    fobj
		    #+CADR
		    ;; In old code, the args are (FILE VAL INDICATOR)
		    (typecase (cdr attribute)
		      ((or compiled-function closure)
		       (funcall (cdr attribute) file))
		      (t (cdr attribute)))
		    (car attribute)
		    #-CADR
		    ;; In new code, it is (FILE INDICATOR VAL)
		    (typecase (cdr attribute)
		      ((or compiled-function closure)
		       (funcall (cdr attribute) file))
		      (t (cdr attribute)))))))))
      (fs:write-directory-files dir))))
	
(defun set-backup-bits (file-list)
  (set-attributes (slotify-files file-list) `((:dumped . t))))

(defun reset-backup-bits (file-list)
  (set-attributes (slotify-files file-list) `((:dumped . nil))))

(defun recursive-file-list-local (&key
				  attribute-match
				  attribute-non-match
				  (directory (fs:dc-root-directory)))
  (check-type directory (and fs:file (satisfies fs:directory?)))
  (do* (return-list
	(list (fs:read-directory-files directory) (cdr list))
	(file (car list) (car list)))
       ((null file) (reverse return-list))
    (if (fs:directory? file)
	(dolist (f (recursive-file-list-local
		     :attribute-match attribute-match
		     :attribute-non-match attribute-non-match
		     :directory file))
	  (push f return-list))
      (when (and (cond (attribute-match
			(equal (fs:file-attribute file (car attribute-match))
			       (cdr attribute-match)))
		       (attribute-non-match
			(not (equal (fs:file-attribute file (car attribute-non-match))
				    (cdr attribute-non-match))))
		       (t t))
		 (> (fs:file-open-count file) -1)
		 (not (fs:file-deleted? file)))
	(push (cons (fs:file-truename file)
		    (fs:lmfs-file-properties file))
	      return-list)))))

(defun list-dumped-files (&optional (directory (fs:dc-root-directory)))
  (recursive-file-list-local :attribute-match '(:dumped . t)
			     :directory directory))

(defun list-new-files (&optional (directory (fs:dc-root-directory)))
  (recursive-file-list-local :attribute-match '(:dumped . nil)
			     :directory directory))

(defun list-all-files (&optional (directory (fs:dc-root-directory)))
  (recursive-file-list-local :directory directory))

(defun recursive-local-file-funcall (function directory to-alter?)
  (check-type directory (and fs:file (satisfies fs:directory?)))
  (print directory)
  (dolist (file (fs:read-directory-files directory)
		(and (when to-alter? (fs:write-directory-files directory))
		     t))
    (if (fs:directory? file)
	(recursive-local-file-funcall function file to-alter?)
      (fs:locking (fs:file-lock file)
	(funcall function file)))))

(defun set-deleted (file)
  (setf (fs:file-deleted? file) t))

(defun reset-dont-delete (file)
  (fs:set-file-attribute file :dont-delete nil))

(defun set-backup-bit (file)
  (fs:set-file-attribute file :dumped t))

(defun reset-backup-bit (file)
  (fs:set-file-attribute file :dumped nil))

(defun set-all-backup-bits-recursive (&optional (directory (fs:dc-root-directory)))
  (recursive-local-file-funcall 'set-backup-bit directory t))

(defun reset-all-backup-bits-recursive (&optional (directory (fs:dc-root-directory)))
  (recursive-local-file-funcall 'reset-backup-bit directory t))

(defun close-file (file)
  (setf (fs:file-open-count file) 0))

(defun close-open-files (&optional (directory (fs:dc-root-directory)))
  (recursive-local-file-funcall 'close-file directory t)
  t)

(defun get-file-plist-from-log-entry (entry)
  (list (fs:make-pathname :host si:local-host
			  :directory (first entry)
			  :name (string (second entry))
			  :type (string (third entry))
			  :version (fourth entry))
	  :characters (nth 4 entry)
	  :creation-date (nth 5 entry)
	  :byte-size (nth 6 entry)
	  :author (string (nth 7 entry))))
	  
(defun set-files-from-backup-log (log-filename)
  (let (*dump-info-list* *dump-files-list*)
    (load log-filename)
    (let ((file-list (mapcar 'get-file-plist-from-log-entry *dump-files-list*)))
      (set-backup-bits file-list))))
