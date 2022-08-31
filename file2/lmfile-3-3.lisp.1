;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for LFS version 3.3
;;; Reason: Fix bug that duplicated dir entries.
;;; Written 1/29/84 01:21:44 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 98.30, CADR 3.5, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.1, Experimental FILE-Server 8.2, Experimental LFS 3.1, Experimental MagTape 22.5, microcode 306, Xmntl FS.



; From file DIREAD.LISP PS:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; DIREAD  "

(defmethod (name-and-version-dir-node store-dir) ()
  (atomic-update-records 0 (file-n-records node-file) t)
  ;; Since we are changing the order of entries,
  ;; the one that used to be at the end may no longer be there.
  ;; So don't try the trick of writing it over at the end.
  (setq dir-last-added-entry nil)
  ;; Say which format of directory dump this is.
  (file-store-word file-node-offset -1 -1)
  (let* (file-ptr-access-array file-ptr-access-index
	 (file-ptr-extend-file t)
	 (file-ptr-next-word (1+ file-node-offset))
	 (file-ptr-elements-following 0))
    ;; Store the word that will eventually contain the total data length.
    (file-ptr-store-word 0 0)
    ;; Write all the dir entries.
    (dolist (name-entry directory-list)
      (dolist (version-entry (cddr name-entry))
 	(dolist (entry (cdr version-entry))
	  (dir-store-entry entry))))
    (dir-finish-store file-node-offset)))

))
