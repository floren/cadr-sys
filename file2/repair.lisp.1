;;; -*-Mode: Lisp; Package: FILE-SYSTEM; Base: 8; Lowercase:T -*-

;; Repair a directory which was damaged by the bug which caused
;; duplicate entries for a file: both nondeleted, both pointing
;; at the same block number, but one of them having zero length.

(defun repair-dir (pathname)
  (with-open-file (stream (send (parse-namestring pathname) ':directory-pathname-as-file))
    (send (send stream ':node) 'repair-dir)))

(defmethod (name-and-version-dir-node repair-dir) ()
  (dolist (name-entry directory-list)
    (dolist (version-entry (reverse (cddr name-entry)))
      (let ((num-non-deleted (count nil (cdr version-entry) ':key 'dir-entry-deleted)))
	(when (< 1 num-non-deleted)
	  ;; We have found a losing file: more than one nondeleted entry with same version.
	  (if (> num-non-deleted 2)
	      (ferror nil "More than TWO nondeleted entries for ~S.  Help!" (cadr version-entry)))
	  ;; There are only two.
	  (let ((de1 (find nil (cdr version-entry) ':key 'dir-entry-deleted))
		(de2 (find nil (cdr version-entry) ':key 'dir-entry-deleted ':from-end t)))
	    (if (zerop (dir-entry-length de1))
		(psetq de1 de2 de2 de1))
	    ;; de2 should be the zero-length one now.
	    (unless (and (zerop (dir-entry-length de2))
			 (not (zerop (dir-entry-length de1))))
	      (ferror nil "Entries ~S, ~S do not fit expected syndrome." de1 de2))
	    (unless (equal (dir-entry-pointer-info de1) (dir-entry-pointer-info de2))
	      (ferror nil "Entries ~S, ~S do not point at same block." de1 de2))
	    (describe de2)
	    (when (y-or-n-p "Delete it? ")
	      (funcall-self 'delete-entry de2))))))))
