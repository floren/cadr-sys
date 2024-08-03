;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 23.2
;;; Reason:
;;;  TAPE:WRITE-FILES now handles a FILES list of wild-carded pathnames.
;;; Written 27-May-88 17:35:00 by SAZ at site Gigamos Cambridge
;;; while running on Fish food from band 1
;;; with Experimental System 124.10, Experimental Local-File 74.0, Experimental File-Server 23.0, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.1, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.TAPE; USER.LISP#100 at 27-May-88 17:35:00
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; USER  "

(defun write-files (files &key
		    (format-spec *selected-format*)
		    (device-spec *selected-device*)
		    (end-of-tape-action :continue)
		    silent)
  "Writes some files to tape from disk.  
FILES
	can be file spec or list of file specs.  Each file spec
	must be a pathname (or string to be parsed into a pathname)
	or a file property list.  (See the tape software documentation
	for more details on using file property lists.)  Wildcards are
	acceptable in pathnames and all disk files matching the pathname
	will be spliced into the list where a wildcarded pathname exists.
	AND NOW...files may be a list of wildcarded pathnames.	--saz, 5/27/88
END-OF-TAPE-ACTION
	This determines what to do if the end of the tape is encountered.
	:CONTINUE specifies that the format software should continue to 
	another tape if possible.  :ERROR will cause an error to be signalled."
  (check-type end-of-tape-action (member :continue :error))
  (using-device (device device-spec)
    (using-format (format format-spec)
      (let ((file-list (typecase files
			 ;;handle list of wildcarded pathnames
			 (cons (let ((file-list))	
				 (dolist (file-designator files (reverse file-list))
				   (if (pathname-wild-p file-designator)
				       (setq file-list
					     (append
					       (mapcar #'car
						       (full-directory-list file-designator))
					       file-list))
				     (push (fs:parse-pathname file-designator) file-list)))))
			 ((or string pathname)
			  (let ((pn (fs:parse-pathname files)))
			    (if (pathname-wild-p pn)
				(mapcar #'car
					(full-directory-list (fs:parse-pathname files)))
			      (ncons pn))))
			 (t (ferror nil "Invalid files specifier: ~S" files)))))
	(with-device-locked device
	  (dolist (file file-list)
	    (send format :write-file
		  device
		  file
		  :end-of-tape-action end-of-tape-action
		  :silent silent)))))))

))
