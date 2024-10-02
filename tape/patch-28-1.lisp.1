;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 28.1
;;; Reason:
;;;  In TAPE:FULL-DIRECTORY-LIST, only filter name and type if non-NIL. 
;;;  This makes e.g. (tape:backup-files "lm:~;") equivalent with (tape:backup-files "lm:~;*.*#*")
;;;  which is probably what you want - I do.
;;; Written 23-Aug-24 11:29:13 by BV,
;;; while running on CDR (CADR) from band 3
;;; with Experimental System 100.22, Experimental Local-File 54.0, Experimental FILE-Server 11.0, Experimental Tape 28.0, microcode 323, 230816.



; From file CDR: BV.TAPE; TAPE.LISP#173 at 23-Aug-24 11:29:13
#10R TAPE#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "CDR: BV.TAPE; TAPE.#"

(defun full-directory-list (path &key (inferiors t) (stream *standard-output*) filter-keywords)
  "Returns a list of all files in the directory pointed to by PATH.
   Directory files are NEVER returned, as such files are useless for dumping.
   INFERIORS - if non-nil, the files found according to the name type and version
               components of the original pathname (:WILD, :NEWEST, etc accepted) 
               in any sudirectories are included. (Thus specifying \"<host>:~;*.*#*\"
               would return all files on host.)
   STREAM - if non-nil, the surveying of directories will be commented upon to STREAM.
   FILTER-KEYWORDS - passed to PROCESS-FILTER-KEYWORDS."
  (let* ((pathname (let* ((pn (fs:parse-pathname path))
			  (name (send pn :name))
			  (type (send pn :type))
			  (version (send pn :version)))
		     (cond-every ((and name (neq name :wild))
				  (push name filter-keywords)
				  (push :name filter-keywords))
				 ((and type (neq type :wild))
				  (push type filter-keywords)
				  (push :type filter-keywords))
				 ((numberp version)
				  (push version filter-keywords)
				  (push :version filter-keywords)))
		     (send pn :new-pathname
			   :name :wild
			   :type :wild
			   :version (if (numberp version) :wild version))))
	 (top-list (condition-case (condition)
		       (cdr (fs:directory-list pathname))
		     (fs:file-not-found)))
	 result)
    (when stream
      (format stream "~&Surveying \"~A: ~A\""
	      (send (send pathname :host) :short-name)
	      (send pathname :string-for-directory)))
    (dolist (elem top-list result)
      (cond ((or (get elem :deleted)
		 (and (not inferiors) (get elem :directory))))
	    ((get elem :directory)
	     (setq result
		   (nconc result
			  (full-directory-list
			    (let ((npn (car elem)))
			      (fs:make-pathname
				:host (send npn :host)
				:device (send npn :device)
				:directory (append (typecase (send npn :directory)
						     (string (ncons (send npn :directory)))
						     (cons (send npn :directory)))
						   (ncons (send npn :name)))
				:name (send pathname :name)
				:type (send pathname :type)
				:version (send pathname :version)))
			    :stream stream
			    :filter-keywords filter-keywords))))
	    ((and filter-keywords
		  (not (process-filter-keywords filter-keywords elem))))
	    (t (setq result
		     (nconc result
			    (ncons elem))))))))

))
