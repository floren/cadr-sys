;-*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Lowercase:T -*-

;Full salvager

;walks the entire file structure, opening every file-node.

;T while salvaging.  Suppresses errors on bad data structures.
;The defvar is in DEFS.
;(defvar now-salvaging nil)

(defvar bad-file-starter-list nil)

(defvar bad-free-blocks-list nil)

;Temporarily bound to a list whose elements look like
;(pack new-starts-file-map new-free-map)
;where the maps are arrays that start out as copied
;of the pack's actual maps and are gradually "cleared out"
;toward nothing-starts-files and everything-is-free.
(defvar pack-new-map-alist)

;List of elements of the form (pack first-block-number)
;for all files not pointed to by directories.
;This list is left around after salvaging.
(defvar files-not-pointed-to)

(defun salvage-file-system ()
  (if (not file-system-running)
      (ferror nil "~The file system must be running before you can salvage it!")
    (or (y-or-n-p "Find files overlapping those from the previous salvage? ")
	(setq bad-file-starter-list nil
	      bad-free-blocks-list nil))
    (let (pack-new-map-alist)
      (dolist (pack pack-list)
	(with-pack-lock pack
	  (push (list pack
		      (read-bitmap-into-array
			pack (pack-block-starts-file-map-first-block pack))
		      (read-bitmap-into-array pack (pack-free-map-first-block pack)))
		pack-new-map-alist)))
      ;; Mark all the "bad blocks" in the maps
      ;; so that any file that contains them will trap.
      (dolist (p pack-new-map-alist)
	(let (pack new-starts-file-map new-free-map map-length)
	  (setf (list pack new-starts-file-map new-free-map) p)
	  (setq map-length (* 16. (array-length new-free-map)))
	  (dolist (e bad-file-starter-list)
	    (and (eq (car e) pack)
		 (change-bitmap-bits new-starts-file-map 0 map-length
				     (cadr e) 1 0 t)))
	  (dolist (e bad-free-blocks-list)
	    (and (eq (car e) pack)
		 (change-bitmap-bits new-free-map 0 map-length
				     (cadr e) (caddr e) 1 t)))))
      (funcall root-node 'salvage-all-subnodes 'salvage)
      (setq files-not-pointed-to (find-files-not-pointed-to pack-new-map-alist))
      (fix-files-not-pointed-to))))

;; Find any files marked on disk
;; which have not been found by the tree walk.
(defun find-files-not-pointed-to (pack-new-map-alist &aux accum)
  (dolist (p pack-new-map-alist)
    (let (pack new-starts-file-map)
      (setf (list pack new-starts-file-map) p)
      (dotimes (block (pack-number-of-blocks pack))
	(and (bit-test (lsh 1 (\ block 16.))
		       (aref new-starts-file-map
			     (truncate block 16.)))
	     (push (list pack block) accum)))))
  accum)

(defun fix-files-not-pointed-to (&aux anything-reinserted-flag)
  (dolist (file files-not-pointed-to)
    (let ((pack (car file)) (block (cadr file)))
      (cond ((block-starts-file-p pack block)
	     (format t "~%pack ~S, block ~S starts a file that isn't pointed to.~%"
		     pack block)
	     (let ((pathname (file-get-redundant-pathname pack block))
		   tem fixed)
	       (cond ((eq pathname 'half-dead)
		      (format t "It is half dead.  Flushing it.")
		      (set-block-starts-file-bit pack block 0)
		      (setq fixed t))
		     (pathname
		      (cond ((y-or-n-p "It is file ~A.  Reinsert it in its directory? "
				       (funcall pathname ':string-for-host))
			     (if (errorp (setq tem (file-reinstall pack block pathname)))
				 (format t "Cannot open directory: ~A" tem)
			       (setq anything-reinserted-flag t)
			       (setq fixed t)))))
		     ((y-or-n-p "Its pathname is not determinable.? ")
		      (set-block-starts-file-bit pack block 0)
		      (setq fixed t)))
	       (if fixed
		   (setq files-not-pointed-to
			 (delq file files-not-pointed-to))))))))
  (if anything-reinserted-flag
      (princ "Do another salvage to make sure none of the reinserted files
overlaps any other file
"))
  nil)

(defmethod (node salvage-all-subnodes) (reason) reason ())

(defmethod (dir-node salvage-all-subnodes) (reason &aux (now-salvaging t))
  (funcall-self 'loop-over-directory
		#'(lambda (entry reason)
		    (and (memq ':first-block-number (dir-entry-pointer-info entry))
			 (let ((node
				 (dir-node-open-subnode reason entry
							'(:inhibit-links t
							  :preserve-dates t
							  :for-salvager t))))
			   (cond (node
				  (*catch 'abandon-subnode
				    (funcall node 'salvage-all-subnodes reason))
				  (funcall node ':remove-reason reason))))))
		reason))

(defmethod (file-node :before salvage-all-subnodes) (ignore)
  (prog ((tem (assq (file-pack) pack-new-map-alist))
	salv-error-type
	starts-file-map free-map map-length)
    (setf (list starts-file-map free-map) (cdr tem))
    (setq map-length (* 16. (array-length free-map)))
    (or (errset
	  (change-bitmap-bits starts-file-map 0 map-length
			      (block-number-of-record 0) 1 0)
	  nil)
	(progn
	 (if (block-starts-file-p (file-pack) (block-number-of-record 0))
	     (push (list (file-pack) (block-number-of-record 0))
		   bad-file-starter-list)
	   (push (list (file-pack) (block-number-of-record 0) 1)
		 bad-free-blocks-list))
	 (if (block-starts-file-p (file-pack) (block-number-of-record 0))
	     (progn
	       (format t "~%The node ~S appears in another place in the hierarchy
That is to say, there are two different pointers to it as a subnode.
Repeating the salvage will find the other name.
"
		       node-closure)
	       (*throw 'abandon-subnode t))
	   (format t "~%The first block, ~S, of the node ~S,~%" (block-number-of-record 0) self)
	   (format t "is not marked as starting a file.
If it is part of another file, repeating the salvage
will find that other file.
***Be wary of simply patching this!
"))
	 (return nil)))
    (dolist (mapelt (file-disk-map))
      (setq salv-error-type mapelt)
      (do (first-loser
	   (next-winner (mapelt-data mapelt))
	   (end (+ (mapelt-data mapelt)
		   (- (mapelt-end mapelt) (mapelt-start mapelt)))))
	  (())
	(setq first-loser (find-bitmap-bit free-map 0 map-length
					   next-winner 1))
	(or (and first-loser (< first-loser end))
	    (return))
	(setq next-winner (min end (find-bitmap-bit free-map 0 map-length
						    first-loser 0)))
	(push (list (file-pack) first-loser (- next-winner first-loser))
	      bad-free-blocks-list)
	(format t "~%The file ~S contains blocks ~S through ~S,~%"
		node-closure first-loser (1- next-winner))
	(format t "some of which are either marked free on disk
or part of some other file.
If they are part of another file,
repeating the salvage will find that file.
"))
      (change-bitmap-bits free-map 0 map-length
			  (mapelt-data mapelt)
			  (- (mapelt-end mapelt) (mapelt-start mapelt))
			  1 t))))



;Given the first block number and pack object of a file,
;return the redundant pathname stored in the file.
(defun file-get-redundant-pathname (pack first-block-number)
  (let (node-file half-dead-flag)
    (and (block-starts-file-p pack first-block-number)
	 (setf (values node-file half-dead-flag)
	       (file-setup pack first-block-number "Salvage" t nil nil t)))
    (cond ((and (eq half-dead-flag t)) 'half-dead)
 	  ((and node-file (file-redundant-name-length)
		(< 0 (file-redundant-name-length) 10000.))
	   (let ((string (car (errset
				(file-extract-string (+ (file-map-offset) (file-map-length))
						     (file-redundant-name-length))
				nil))))
	     (and string
		  (let* ((pn1 (parse-pathname string lfs-host))
			 (namelist (funcall pn1 ':name-type-and-version))
			 (pn2 (funcall pn1 ':new-pathname
				       ':directory (butlast namelist))))
		    (funcall pn2 ':new-name-type-and-version (last namelist)))))))))

(defun file-reinstall (pack first-block-number pathname)
  (let* ((pn1 (funcall pathname ':directory-pathname-as-file))
	 (str (open pn1 ':error nil)))
    (if (errorp str)
	str
      (let (length dir-info)
	(let ((node-file (file-setup pack first-block-number "Salvage" t)))
	  (setq length (file-redundant-contents-length)
		dir-info  (file-redundant-dir-info)))
	(funcall (funcall (funcall str ':node)
			  ':create-subnode
			  'salvage
			  (car (funcall pathname ':name-type-and-version))
			  ':reinstall-pack pack
			  ':reinstall-first-block-number first-block-number
			  ':reinstall-length length
			  ':reinstall-dir-info dir-info)
		 ':remove-reason 'salvage)
	t))))
