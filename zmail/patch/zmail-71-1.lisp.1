;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for ZMail version 71.1
;;; Reason:
;;;  The function ZWEI:MAKE-UNIX-ZMAIL-INIT-FILE automates the chore of creating
;;;  ZMail initialization files for a LISPM user with a Unix mail server host.
;;;  
;;;  New version in ZWEI replaces the version formerly in the FTP: package
;;;  (the old version will go away in the next system version).
;;;  
;;;  See the function documentation for details.
;;; Written 5-May-88 19:31:58 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Fish food from band 3
;;; with Experimental System 123.256, Experimental Local-File 73.5, Experimental FILE-Server 22.4, Experimental Unix-Interface 11.0, Experimental KERMIT 34.3, Experimental ZMail 71.0, Experimental Lambda-Diag 15.0, Experimental Tape 22.4, microcode 1756, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZMAIL; MFHOST2.LISP#1 at 5-May-88 19:32:11
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MFHOST2  "


;;;MFHOST2

;;;Like MFHOST, this file is for system-dependent ZMail extensions
;;;and support code.  Unlike MFHOST, this file is in CommonLISP, the
;;;syntax of "real" LISP programmers.

(defun make-unix-zmail-init-file (username host login-dir)
  "This function makes mail initialization files for a ZMail user
 on a LISP machine who also uses a Unix mail server host.
\
For example, I am user GJC, my LISP home directory is \"LAM3:GJC;\", and I
 want to get my mail from the Unix host ANGEL; so I would execute:
\
  (make-unix-zmail-init-file 'gjc 'cap \"lam3:gjc;\")
\
This creates two files, one in the LISP home directory called 'ZMAIL.INIT',
 and one in the Unix home directory called '<user>.bb'.
\
Returns two values, the pathnames of the two ZMail initialization files."
  ;;
  (declare(values zmail-pathname mailfile-pathname))
  ;;
  (multiple-value-bind (real-username fullname home-directory-string)
      (lookup-user-etc-passwd username host)
    (unless (and real-username fullname home-directory-string)
      (warn "Unable to make your Unix-ZMail-INIT-file.")
      (return-from make-unix-zmail-init-file nil))
    (let* ((realhost (si:parse-host host))
	   (realhome (fs:parse-pathname (string-append home-directory-string "/")
					realhost))
	   (mailfile-pathname (fs:merge-pathnames "mail.bb" realhome))
	   (zmail-pathname (send (fs:parse-pathname login-dir)
				 :new-pathname
				 :name "ZMAIL"
				 :type "INIT"
				 :version :highest))
	   (spool-dir (fs:parse-pathname "/usr/spool/mail/" realhost))
	   (spool-file (fs:merge-pathnames 
			 (fs:parse-pathname real-username realhost)
			 spool-dir)))
      ;;So long as there isn't one already...
      (and (or (null (probe-file zmail-pathname))
	       ;;or the caller says it's ok...
	       (yes-or-no-p "~%The ZMail initialization file ~s already exists...~
                             ~&  is it OK to overwrite it?"
			    zmail-pathname))
	   ;;Write out ZMail .init file
	   (with-open-file (stream zmail-pathname :direction :output)
	     (format t "~%Writing out ~s..." zmail-pathname)
	     (let ((*readtable* (si:find-readtable-named "Common-Lisp"))
		   (*package* (find-package "ZWEI"))
		   (*print-base* #o12)
		   (*read-base*  #o12))
	       (format stream ";;-*-Mode:LISP;Package:~A;Base:~10r;ReadTable:~A-*-~%"
		       (package-name *package*)
		       *read-base*
		       (si:rdtbl-short-name si:common-lisp-readtable))
	       (format stream "~&~
                      ~%(login-setq *zmail-startup-file-name* ~S)~
                      ~2%(login-setq *from-user-id* ~S)~
                      ~2%(login-setq *from-host* (si:parse-host ~S))~
                      ~2%(login-setq fs:user-personal-name-first-name-first ~S)~
                      ~2%(login-setq zwei:*zmail-usual-mail-file-directory* ~S)~
                      ~2%(login-setq zwei:*zmail-homedir-real-new-mail-filename* ~S)~2%"
		       (send mailfile-pathname :string-for-printing)
		       real-username
		       (send realhost :name)
		       fullname
		       (send realhome :string-for-printing)
		       (send spool-file :string-for-printing)))))
      ;;So long as there isn't one already...
      (and (or (null (probe-file mailfile-pathname))
	       ;;or the caller says it's ok...
	       (yes-or-no-p "~%The mail file ~s already exists...~
                             ~&  is it OK to overwrite it?"
			    mailfile-pathname))
	   ;;write Babyl file in user's Unix home directory.
	   (with-open-file (stream mailfile-pathname :direction :output)
	     (format t "~%Writing out ~s..." mailfile-pathname)
	     (format stream "Babyl Options:~
                        ~%Append:1~
                        ~%Version:5~
                        ~%Mail: ~A~
                        ~%Owner:~A~
                        ~%Summary-window-format: T~
                        ~%"
		     (send spool-file :string-for-printing)
		     real-username)))
      ;;Return the pathnames we wrote out
      (values zmail-pathname mailfile-pathname))))

(defun lookup-user-etc-passwd (username host)
  (with-open-file (stream (fs:parse-pathname "/etc/passwd" host))
    (do ((st))
	((null (setq st (read-line stream nil))))
      (when (and (string-equal username
			       (substring st 0 (string-search ":" st)))
		 (y-or-n-p "~&Is this your Unix password entry? -->'~A'~% ...?" st))
	(return (parse-user-etc-passwd st))))))

(defun parse-user-etc-passwd (st)
  ;; username:password:uic:gid:Full Name:directory:shell
  (let ((n (string-search ":" st))
	(uname)(fullname)(dir))
    (setq uname (substring st 0 n))
    (setq n (string-search ":" st (1+ n)))	; pass
    (setq n (string-search ":" st (1+ n)))	; uid
    (setq n (string-search ":" st (1+ n)))	; gid
    (setq fullname (substring st (1+ n)
			      (setq n (string-search ":" st (1+ n)))))
    (if (string-search "," fullname)
	(setq fullname (substring fullname 0 (string-search "," fullname))))
    (setq dir (substring st (1+ n) (string-search ":" st (1+ n))))
    (values uname fullname dir)))

))
