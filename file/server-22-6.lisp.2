;;; -*- Mode:Lisp; Readtable:ZL; Package:FILE-SYSTEM; Base:8; Patch-File:T -*-
;;; Patch file for FILE-Server version 22.6
;;; Reason:
;;;  File server was passing on :expunge and :create-directory operations
;;;  using a keyword argument: :error nil.  The operations wanted a simple
;;;  nil...
;;; Written 19-May-88 02:04:41 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Fish food from band 1
;;; with Experimental System 123.283, Experimental Local-File 73.6, Experimental FILE-Server 22.5, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.2, Experimental Lambda-Diag 15.0, Experimental Tape 22.4, microcode 1756, SDU Boot Tape 3.14, SDU ROM 103, patch/experimental.



; From file DJ: L.FILE; SERVER.LISP#195 at 19-May-88 02:04:46
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-expunge (fh strings &aux path result)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given in EXPUNGE command." tid fh))
	((null strings)
	 (format conn-stream "~A  ERROR IRF F No pathname given to EXPUNGE command." tid))
	((cdr strings)
	 (format conn-stream "~A  ERROR IRF F Extra junk given to EXPUNGE command." tid))
	((errorp (setq path (lmfs-parse-for-server (first strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq result (send (send path :new-pathname
					  :name :unspecific
					  :type :unspecific
					  :version :unspecific)
				    :expunge nil)))
	 (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
	(t (format conn-stream "~A  EXPUNGE ~D" tid result))))

))

; From file DJ: L.FILE; SERVER.LISP#195 at 19-May-88 02:04:51
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-create-directory (fh strings &aux path result)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given in CREATE-DIRECTORY command."
		 tid fh))
	((null strings)
	 (format conn-stream "~A  ERROR IRF F No pathname given to CREATE-DIRECTORY command." tid))
	((cdr strings)
	 (format conn-stream "~A  ERROR IRF F Extra junk given to CREATE-DIRECTORY command." tid))
	((errorp (setq path (lmfs-parse-for-server (first strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq result (send path :create-directory nil)))
	 (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
	(t (format conn-stream "~A  CREATE-DIRECTORY ~D" tid result))))

))

; From file DJ: L.FILE; SERVER.LISP#195 at 19-May-88 02:05:14
#8R FILE-SYSTEM#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: FILE; SERVER  "

(defun file-server-create-link (fh strings &aux path path2 result)
  (cond (fh
	 (format conn-stream "~A ~A ERROR IRF File handle given in CREATE-LINK command."
		 tid fh))
	((null (second strings))
	 (format conn-stream "~A  ERROR IRF F Insufficient arguments given to CREATE-LINK command."
		 tid))
	((cddr strings)
	 (format conn-stream "~A  ERROR IRF F Extra junk given to CREATE-LINK command." tid))
	((errorp (setq path (lmfs-parse-for-server (first strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq path2 (lmfs-parse-for-server (second strings))))
	 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
	((errorp (setq result (send path :create-link path2 nil)))
	 (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
	(t (format conn-stream "~A  CREATE-LINK ~D" tid result))))

))
