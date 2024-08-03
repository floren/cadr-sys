;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 19.2
;;; Reason:
;;;  Add :silent option to tar-format :write-file method.
;;; Written 14-Jan-88 14:07:50 by keith (Keith Corbett) at site LMI
;;; while running on Opus from band 1
;;; with Experimental System 123.174, Experimental Local-File 73.3, Experimental FILE-Server 22.1, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tiger 27.0, Experimental Site Data Editor 8.4, Experimental Laser1+ 2.0, Experimental Tape 19.0, microcode 1754, SDU Boot Tape 3.14, SDU ROM 8, Beta I/site/dvi.



; From modified file OPUS: L.TAPE; TAR-FORMAT.LISP#23 at 14-Jan-88 14:26:17
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAR-FORMAT  "

(defmethod (tar-format :write-file) (device file
				     &key
				     silent
				     (end-of-tape-action :continue))
  end-of-tape-action				;Tar format doesn't know about continue
  (with-open-file (fstream file :direction :input)
    (let* ((upn (let* ((tpn (send self :get-dummy-pathname (send fstream :truename)))
		       (dir (fs:pathname-directory tpn)))
		  (send tpn :new-directory
			(cons :relative
			      (etypecase dir
				(list dir)
				(string (ncons dir)))))))
	   (plist (list upn
			:byte-size (get fstream :byte-size)
			:characters (get fstream :characters)
			:length-in-bytes (or (get fstream :length-in-bytes) (get fstream :length))
			:mode 0
			:uid 0
			:gid 0
			:creation-date (get fstream :creation-date)
			:link-flag nil
			:link-name nil)))
      (unless silent
	(format *standard-output* "~&Writing file: ~A" (car plist)))
      (send self :write-file-header device plist)
      (send self :read-data-from-stream device fstream))))

))
