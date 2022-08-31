;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Written 1/03/84 19:44:05 by LMFile,
;;; Reason: Accept :DEFAULT as creation byte size.
;;; 25-bit pointer bugs in parsing directories.
;;; OPERATE-ON-PATHLIST-NODE bug.
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.22, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.0, Experimental FILE-Server 8.0, Experimental LFS 3.0, Experimental MagTape 22.0, microcode 306, Xmntl FS.



; From file ANYDIR.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; ANYDIR  "

(defmethod (dir-node :create-subnode)
	   (reason-why-open pathstep &rest keyword-args
			    &key &optional ((:byte-size subnode-byte-size))
			    reinstall-pack reinstall-first-block-number
			    reinstall-dir-info reinstall-length
			    if-exists
			    flavor (characters t) (element-type nil element-type-p)
			    override-protection &allow-other-keys)
  (unlock-node-and-wait-for-disk-space (file-pack node-file))
  (when element-type-p
    (setf (values characters subnode-byte-size)
	  (decode-element-type element-type subnode-byte-size)))
  (setq pathstep (funcall-self 'validate-pathstep pathstep))
  (cond ((and (consp pathstep) (eq (car pathstep) ':property))
	 (lexpr-funcall-self ':create-property-node reason-why-open
			     pathstep keyword-args))
	((and (consp pathstep) (eq (car pathstep) ':subnode-property))
	 (lexpr-funcall-self 'create-subnode-property-pathstep
			     reason-why-open pathstep keyword-args))
	(t
	 (let (old-entry entry)
	   (let ((tem (assq flavor flavor-name-translation-alist)))
	     (and tem
		  (setq flavor (cdr tem))))
	   (or (memq flavor node-flavor-list)
	       (setq flavor nil))
	   (setq subnode-byte-size
		 (if (memq subnode-byte-size '(nil :default))
		     (if characters 8 16.)
		   (validate-byte-size subnode-byte-size)))
	   (setq old-entry (funcall-self 'find-entry pathstep))
	   (setq entry
		 (create-dir-entry flavor subnode-byte-size old-entry keyword-args))
	   (if (eq if-exists ':supersede)
	       (setq pathstep (funcall-self 'inherit-entry-name-and-version
					    pathstep old-entry))
	     (setq pathstep (funcall-self 'inherit-entry-name pathstep old-entry)))
	   (funcall-self 'insert-entry entry pathstep override-protection)
	   ;; Handle putting a lost file back into the directory.
	   ;; We are given the appropriate pointer-info properties to store in the dir.
	   (cond (reinstall-first-block-number
		  (putprop (locf (dir-entry-pointer-info))
			   reinstall-first-block-number
			   ':first-block-number)
		  (putprop (locf (dir-entry-pointer-info))
			   (pack-number-within-volume reinstall-pack)
			   ':pack-number)
		  (multiple-value-bind (del flg)
		      (extract-redundant-dir-flags reinstall-dir-info)
		    (and del (funcall-self 'undelete-subnode entry))
		    (setf (dir-entry-flags) flg))
		  (setf (dir-entry-subnode-flavor)
			(nth (ldb 1010 reinstall-dir-info) node-flavor-list))
		  (setf (dir-entry-byte-size)
			(ldb 0010 reinstall-dir-info))
		  (setf (dir-entry-length) reinstall-length)
		  (or (string-equal (pack-volume-name (file-pack node-file))
				    (pack-volume-name reinstall-pack))
		      (putprop (locf (dir-entry-pointer-info))
			       (pack-volume-name reinstall-pack)
			       ':volume-name))))
	   (funcall-self ':finish-contents)
	   (dir-node-open-subnode reason-why-open entry
				  (list* ':old-entry old-entry
					 keyword-args))))))

))

; From file DIREAD.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; DIREAD  "

(defmethod (name-and-version-dir-node read-dir) ()
  (let (file-ptr-access-array file-ptr-access-index
	(file-ptr-extend-file nil)
	file-ptr-next-word
	(file-ptr-elements-following 0))
    (prog1
      ;; Decide on the format, get the redundant data length,
      ;; and read the data.
      (cond ((= 77777777 (file-fetch-word file-node-offset))
	     (setf (file-contents-length node-file)
		   (file-fetch-word (1+ file-node-offset)))
	     (setq file-ptr-next-word (+ 2 file-node-offset))
	     (read-new-dir-format))
	    (t
	     (setf (file-contents-length node-file) (file-fetch-word file-node-offset))
	     (setq file-ptr-next-word (1+ file-node-offset))
	     (read-old-dir-format)))
      ;; Check that we ended at the right place.
      (let ((directory-data-end (+ (file-contents-length) (file-contents-offset))))
	(or (= file-ptr-next-word directory-data-end)
	    (and (= directory-data-end file-node-offset))	;Empty dir, just created.
	    (ferror nil "Lossage: directory data doesn't end at supposed length"))))))

))

; From file DIREAD.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; DIREAD  "

(defun dir-read-name-entry ()
  (multiple-value-bind (installed-version-number name-length)
		       (file-ptr-fetch-word)
    (and (= installed-version-number 77777777)
	 (setq installed-version-number nil))
    (let ((name (file-ptr-extract-string name-length))
	  versions-list)
      (do (version tem)
	  ((progn (multiple-value (version tem) (file-ptr-fetch-word))
		  (zerop (logior tem version))))
	(push (dir-read-version-entry name version) versions-list))
      (list* name installed-version-number (nreverse versions-list)))))

))

; From file STREAM.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; STREAM  "

(defun operate-on-pathlist-node (directory namepath pathname operation subnode-operation
				 args options)
  (let ((last-step (car (last (append (and (neq directory ':unspecific) directory)
				      namepath))))
	(temp-reason (gen-temp-reason)))
    (if (or (symbolp last-step)	  ;ROOT and SUPERNODE => must open.
	    (and (consp last-step)	  ;(:PROPERTY ...), ditto.
		 (neq (car last-step) ':version))
	    (not subnode-operation))
	(let ((node (lexpr-funcall 'open-node root-node temp-reason
				   directory namepath pathname
				   options)))
	  (unwind-protect
	   (lexpr-funcall node operation args)
	   (funcall node ':remove-reason temp-reason)))
	(let ((supernode (open-node root-node temp-reason
				    (butlast (append directory namepath)) nil
				    pathname)))
	  (unwind-protect
	   (or (lexpr-funcall supernode ':operate-on-subnode-by-name
			      last-step operation subnode-operation args options)
	       (ferror 'file-not-found "File not found."
		       (pathlist-into-pathname
			 (append (send supernode ':standard-pathlist)
				 (list last-step)))))
	   (funcall supernode ':remove-reason temp-reason))))))

))
