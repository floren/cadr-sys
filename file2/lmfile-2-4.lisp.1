;;; -*- Mode: Lisp; Package: FILE-SYSTEM; Base: 8.; Patch-File: T -*-
;;; Patch file for LFS version 2.4
;;; Reason: Starting directory for dump can specify more than one level of path.
;;; Written 9/30/83 02:33:19 by LMFile,
;;; while running on Lisp Machine Filecomputer from band 4
;;; with MIT-Specific 19.5, System 94.41, ZMail 50.17, Experimental Local-File 44.3, FILE-Server 6.6, MagTape 14.4, Experimental LFS 2.3, microcode 238, FC.



; From file ANYDIR.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; ANYDIR  "

;Given a dir entry, incrementally dump 
;that node or all of the subnodes of that entry to tape.
(defun dir-node-dump-subnodes
       (entry name-pathlist pathrest accept-deleted-nodes max-version
	reason incremental-flag dumper-function
	&aux first-win)
  (declare (special dump-starting-directory))
  max-version
  (and (car dump-starting-directory)
       (eq (funcall-self 'find-entry
			 (funcall self 'validate-pathstep (car dump-starting-directory)))
	   entry)
       (progn (setq first-win t)
	      (setf (car dump-starting-directory) nil)))
  (or (and incremental-flag
	   (memq ':dumped (dir-entry-flags entry)))
      (car dump-starting-directory)
      (let ((dump-starting-directory
	      (and first-win (cdr dump-starting-directory))))
	(with-entry-node-open (node reason entry ':preserve-dates t)
	  (if (dir-entry-dir-p entry)
	      (progn
	       (dir-entry-operate-on-matching-subnodes
		 node entry pathrest
		 name-pathlist accept-deleted-nodes
		 'dir-node-dump-subnodes
		 reason incremental-flag dumper-function)))
	  (funcall dumper-function node)))))

))

; From file DUMP.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER:PATCH-SOURCE-FILE "SYS: FILE2; DUMP  "

(defun dump-file-system (&key &optional full (set-dump-bits t) (unit 0) starting-directory)
  "Dump all files, or all files never dumped, onto magtape.
FULL if non-NIL means dump all files, not just the files not yet dumped.
STARTING-DIRECTORY should be a list of directory names,
forming a path to the first directory to start dumping from.
UNIT is the magtape unit to use.
SET-DUMP-BITS if NIL means do not mark files as dumped.  Default is T."
  (or dump-tree-walking-sg
      (setq dump-tree-walking-sg (make-stack-group "dump tree walk" ':regular-pdl-size 6000)))
  (stack-group-preset dump-tree-walking-sg 'dump-start-tree-walk
		      full %current-stack-group (copylist starting-directory))
  (let (dump-finished
	(dump-tape-unit unit)
	dump-next-node)
    (unwind-protect 
      (progn
	(do () ((dump-one-tape set-dump-bits)))
	(setq dump-finished t))
      (or dump-finished
	  (funcall dump-tree-walking-sg 'abort-dump)))))

))
