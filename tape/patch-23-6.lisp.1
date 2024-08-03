;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 23.6
;;; Reason:
;;;  Allow disk files to have bogus :version properties that are strings --
;;;  we will strip off that property anyway.
;;; Written 1-Jun-88 16:26:11 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 2
;;; with Experimental System 124.15, Experimental Local-File 74.0, Experimental File-Server 23.1, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.4, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8.



; From modified file DJ: L.TAPE; TAPE.LISP#169 at 1-Jun-88 16:27:38
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPE  "

(defconst tape-file-property-type-plist
	  '(:directory (or string list)
	    :name (or string symbol)
	    :type (or string symbol)
	    :version (or string fixnum)
	    :byte-size fixnum
	    :length-in-blocks (integer 0)
	    :length-in-bytes (integer 0)
	    :author string
	    :creation-date (integer 1)
	    :characters symbol)
  "This list of canonical-types for the properties of tape file property lists")

))

; From modified file DJ: L.TAPE; TAPE.LISP#169 at 1-Jun-88 16:27:47
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPE  "

(defun check-plist-validity
       (plist &optional (error-action *error-action-on-imperfect-tape-plists*))
  ;; sometime in the system 99 beta release the filesystem and/or the magtape
  ;; code conspired to put bogus plists on the tape which would cause the filesystem
  ;; to barf when you tried to restore the tape. The magtape code has since been
  ;; corrected to never output bogus plists but we must make sure never the less.
  (let ((newplist (loop for x in plist
			collect (if (and (symbolp x) (not (memq x '(t nil))))
				    (intern (string x) pkg-keyword-package)
				  x))))
    (unless (equal newplist plist)
      (format *error-output* "~&Property list ~S was converted to have all KEYWORD symbols.~%"
	      plist)
      (setq plist newplist)))
  (do ((*print-base* 10.)
       (new-plist)
       (l plist)
       (key)(value)(type))
      ((null l)
       new-plist)
    (setq key (pop l)
	  value (pop l))
    (cond ((and (setq type (getf tape-file-property-type-plist key))
		(not (typep value type)))
	   (select error-action
	     (:warn
	      (cond ((and (not (eq (getf l key plist) plist)) (typep (getf l key) type))
		     ;; this seems to be the only case in fact.
		     (format *error-output* "~&Key ~S had bogus value ~S and was duplicated~%"
			     key value))
		    ('else
		     (format *error-output* "~&Key ~S with bogus value ~S is being ignored~%"
			     key value))))
	     (t
	      (ferror nil "Key ~S with bogus value ~S" key value))))
	  ((eq key :truename))
	  ((eq (getf new-plist key plist) plist)
	   (setf (getf new-plist key) value))
	  ('else
	   (select error-action
	     (:warn
	      (format *error-output* "~&Duplicate key ~S with value ~S being ignored"
		      key value))
	     (t
	      (ferror nil "~&Duplicate key ~S with value ~S" key value)))))))

))
