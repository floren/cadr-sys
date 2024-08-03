;;;  -*- Mode:LISP; Package:TFRAME; Base:10; Readtable:CL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.
;;; 
;;;   Commands and options for TFRAME
;;; 

;;; wipe out all previous definitions
(eval-when (eval load)
  (mapcar #'(lambda (&quote thing) (setplist thing nil)) *mode-types*)
  (setq *mode-types* nil
	*global-options* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; global options (available in all modes)
;;;

(define-option *global-numeric-arg* ()
  "Global numeric argument"
  1
  (:number)
  "~
Numeric argument common to all modes.  If this option affects
a particular command, it will be documented in the documentation
of the command.")


(define-option *global-pathname-arg* ()
  "Global pathname argument"
  nil
  (:pathname-or-nil)
  "~
Pathname available for all commands.  If this option affects the
operation of a particular command, it will be documented in the
documentation of the command.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tape
;;;

(define-command REWIND/UNLOAD (control tape-info dump backup retrieve)
  "Rewind the storage device.  {M: Unload}"
  :left (with-status ("Rewinding tape...")
	  (tape:rewind))
  :middle (with-status ("Unloading tape...")
	    (tape:unload))
  :documentation "~
This command rewinds the tape to load point if the left mouse button
is used.  If the middle button is used, then the tape is unloaded.
If the tape is unloaded, all subsequent operations will get an error
until another tape is loaded.")


(define-command SPACE-FOR-APPEND (control)
  "Space to the logical end of tape."
  :left (with-status ("Spacing to Logical End of Tape ...")
	  (send tape:*selected-format* :position-to-append tape:*selected-device*))
  :documentation "~
Position the tape so that all subsequent files written to the tape
are appended.")

(define-command BEGINNING-OF-FILE (control)
  "Space to the beginning of the current file."
  :left (with-status ("Spacing to beginning of this file")
	  (send tape:*selected-format* :beginning-of-file tape:*selected-device*))
  :documentation "~
This positions the tape at the beginning of the current file.
It should be used after aborting out of tape operations to ensure
that the formatting software does not lose track of its position
on the tape with repect to files.")

(defun Space-Backward-Files (format device n)
  (with-status ("Spacing Backward ~D File~:P ..." n)
    (condition-case ()
	(send format :previous-file device n)
      (tape:physical-beginning-of-tape
       (format *standard-output* "~&At beginning of tape.")))))

(define-command BACKWARD-FILES (control)
  "Space backwards N files.  L: use global numeric arg {M: Enter from Keyboard}"
  :left   (Space-Backward-Files tape:*selected-format* tape:*selected-device* *global-numeric-arg*)
  :middle (let ((number (prompt-and-read :number "~&Number of file to space backward by >> ")))
	    (cond ((typep number '(integer 1))
		   (Space-Backward-Files tape:*selected-format* tape:*selected-device* number))
		  (t
		   (tv:beep)
		   (format t "~&~%Number must be a positive integer!~%"))))
  :documentation "~
This moves the tape backward by files.  If the left mouse button
is used, then the \"global numeric argument\" determined the number 
of files to space over.  If the middle button is used, the number
of files must be specified by the user.")


(Defun Space-Forward-Files (format device n)
  (with-status ("Spacing Forward ~D File~:P ..." n)
    (Send format :next-file device n)))

(define-command FORWARD-FILES (control)
  "Space forward N files.  L: use global numeric argument {M: Enter from Keyboard}"
  :left   (Space-Forward-Files tape:*selected-format* tape:*selected-device* *global-numeric-arg*)
  :middle (let ((number (prompt-and-read :number "~&Number of file to space forward by >> ")))
	    (cond ((typep number '(integer 1))
		   (Space-Forward-Files tape:*selected-format* tape:*selected-device* number))
		  (t
		   (tv:beep)
		   (format t "~&~%Number must be a positive integer!~%"))))
  :documentation "~
This moves the tape forward by files.  If the left mouse button
is used, then the \"global numeric argument\" determined the number 
of files to space over.  If the middle button is used, the number
of files must be specified by the user.")

(define-command RESET-DEVICE (control)
  "Reset the tape device.  {M: Rewind after resetting}"
  :left (with-status ("Resetting Device: ~a" tape:*selected-device*)
	  (tape:reset-device))
  :middle (prog1 (with-status ("Resetting Device: ~a" tape:*selected-device*)
		   (tape:reset-device))
		 (with-status ("Rewinding tape")
		   (tape:rewind)))
  :documentation "~
This resets the device driver software and the device hardware.
It is typically used if the device seems to be wedged.")


(define-command device-status (control)
  "Return status of the selected tape device."
  :left (tape:device-status)
  :documentation "~
Returns a list of keywords which describe the status
of the selected tape device.  Keywords are intuitively
named and their presence implies boolean truth of the 
condition.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tape info
;;;

(define-command MOUNT-TAPE (tape-info)
  "Determine and select format according to tape.  Print the header"
  :left (with-status ("Mounting tape on ~a" tape:*selected-device*)
	  (tape:mount-tape))
  :documentation "~
Determine the format of a tape already online and at load point
on the selected device.  If the format is supported, then the tape
header (if any) is printed out and the tape positioned at the beginning
of the first file.  If the format is not supported, an error to that
effect will be signalled.")

(define-command LIST-TAPE (tape-info)
  "List all files on the tape."
  :left (with-status ("Listing tape files")
	  (tape:list-files))
  :documentation "~
This prints a description of all files on the tape to *standard-output*.
A list of file property lists is returned representing these files.")

(define-command LIST-SOME-FILES (tape-info)
  "L: List N files according to the global numeric argument {M: from keyboard}"
  :left (with-status ("Listing ~D tape file~:P" *global-numeric-arg*)
	  (tape:list-files :number-of-files *global-numeric-arg*))
  :middle (let ((number (prompt-and-read :number "~&Number of files to list >> ")))
	    (typecase number
	      ((integer 1)
	       (with-status ("Listing ~D tape file~:P" number)
		 (send tape:*selected-format*
		       :list-files tape:*selected-device*
		       :number-of-files number)))
	      (t
	       (tv:beep)
	       (format t "~&~%Number must be a positive integer!~%"))))
  :documentation "~
This prints information about a specific number of files on the tape.
If the left mouse button is used, then the \"global numeric argument\" 
is used as the number of files.  If the middle button is used, the
number of files is read from the keyboard.")

(define-command GET-FILE-PROPERTIES (tape-info)
  "Get plist for current file on tape."
  :left (progn
	  (condition-case ()
	      (send tape:*selected-format* :beginning-of-file tape:*selected-device*)
	    (tape:physical-beginning-of-tape))
	  (prog1
	    (tape:list-files :number-of-files 1 :output-to nil)
	    (condition-case ()
		(send tape:*selected-format* :previous-file tape:*selected-device*)
	      (tape:physical-beginning-of-tape))))
  :documentation "~
This returns a the file property list of the next file on tape,
spacing back to the tape's position before the command was executed.")
  

;;; Dumping

(define-option *default-host* (dump)
  "Default Host"
  si:local-host
  (:host-or-nil)
  "~
This is the host to use for partition operations.
It must be a valid network host or NIL meaning the local
host.")

(define-option *default-disk-unit* (dump)
  "Default disk unit"
  (si:default-disk-unit nil)
  (:number)
  "~
This is the disk drive unit number for partition operations.
It is combined with the \"default host\" option to determine
the exact location of a particular partition specified by name
in the partition operations.")

(defun tframe-default-disk-unit ()
  (if (boundp '*default-disk-unit*)
      *default-disk-unit*
    (si:default-disk-unit nil)))
      

(define-option *write-subdirectories* (dump)
  "Write subdirectories?"
  t
  (:boolean)
  "~
If this is TRUE, then file operations will recursively write
subdirectories as well as the top-level directory specified
by the pathname for the operation.  Otherwise only one directory
level will be considered for dumping.")

(define-option *end-of-tape-action* (dump)
  "End of tape action"
  :continue
  (:choose (:continue :error))
  "~
This determines what should happen if the physical end of tape
is encountered during a dump operation.  If the value of this
option is :CONTINUE, then the format will continue to another
tape if possible.  If the value is :ERROR, a physical-end-of-tape
error will be signalled.  This option holds for all dump operations
that could possibly reach the end of the tape.")

(define-option *verify-files* (DUMP BACKUP RETRIEVE)
  "Verify files"
  t
  (:boolean)
  "~
This determines whether files should be verified after they
are dumped or retrieved.  If files are being dumped, then
all of the files are written, then verified.  If a partition
is being written and it is longer than one tape, then each
tape will be verified before the next one is written.  This
eliminates the waste of time writing subsequent reels if one
reel has a compare error.")


(Defun Sum-File-Lengths (files)
  (Let ((total 0))
    (DoList (file files)
      (Incf total
	    (* (or (get file :length-in-bytes) (get file :length))
	       (/ (tape:file-byte-size file) 8))))
    total)
  )

(Defun Dump-Files (format device pathname subdirectories?)
  (Let ((files
	  (With-Status ("Listing Files to Dump")
	    (tape:full-directory-list pathname
	      :inferiors subdirectories?
	      :stream nil))))
    (DoList (file files)
      (with-status ("Writing File: \"~a\"" (car file))
	(send format :write-file device (car file)
	      :end-of-tape-action *end-of-tape-action*
	      :silent t)))
    (format t "~&Dumped ~:D files (~:D bytes).~%" (length files) (sum-file-lengths files)))
  )


(define-command WRITE-FILES (dump)
  "Write files to tape using the global pathname arg"
  :left
    (Dump-Files tape:*selected-format* tape:*selected-device* *global-pathname-arg* *write-subdirectories*)
  :documentation "~
This command writes files to tape according to a specified
\(optionally wilcarded) pathname.  The pathname is determined 
from the \"global pathname argument\".  Various options
will affect the action of this command as documented.")

(define-command WRITE-PARTITION (dump)
  "Write partition to tape."
  :left (multiple-value-bind (host unit nil nil nil name)
	    (tape:partition-searcher
	      "dumping" 0
	      :default-unit
	      (if (string-equal *default-host* si:local-host)
		  (tframe-default-disk-unit)
		(format nil "~A ~D" *default-host* (tape:unit-number (tframe-default-disk-unit)))))
	  (when host
	    (with-status ("~A[~D]: ~A" host unit name)
	      (tape:write-partition
		name :unit (if (eq host si:local-host)
			       unit
			     (format nil "~A ~D" host (tape:unit-number unit)))))))
  :documentation "~
This writes a partition to tape.  The user will be queried for the
name of the partition to write, optionally allowing the user to
use the partition searcher to find an appropriate partition.
The disk unit to use is the \"default disk unit\" option.
Other options may affect this operation as documented.")

(define-command FINISH-TAPE (dump)
  "Finish tape {M: then Rewind}"
  :left (with-status ("Finishing Tape")
	  (tape:finish-tape))
  :middle (prog1 (with-status ("Finishing Tape")
		   (tape:finish-tape))
		 (with-status ("Rewinding tape")
		   (tape:rewind)))
  :documentation "~
This command finishes the end of the tape ensuring that any
information concerning end of tape is written.  If the middle button
is used, the tape is rewound afterwards.")

(define-command VERIFY-TAPE (dump backup retrieve)
  "Verify the tape."
  :left (progn (with-status ("Rewinding tape")
		 (tape:rewind))
	       (let ((results (tape:compare-files
				:transform
				(when *global-pathname-arg*
				  (fs:make-pathname
				    :host (send (fs:translated-pathname *global-pathname-arg*)
						:host)))))
		     (bad-files))
		 (when results
		   (dolist (file results bad-files)
		     (when (errorp file)
		       (send file :report *interaction*)
		       (push file bad-files))))))
  :documentation "~
This verifies the files on tape against their sources on disk.
The global pathname argument is used to back-translate filenames
to find the source, therefore it must be supplied exactly as it
was for the WRITE-FILES command.  If the file is a partition,
all necessary information may be contained on the tape to determine
the source.  In any event, the user will be asked to confirm the
source or choose another partition to compare against.  This command
can be used in this way to verify that a partition restored from
this tape to a different place.")


;;; Filesystem backup stuff

;; This is because backup of remote-hosts is not yet supported.
;; can you say "kludge"?

(defprop :non-selectable-host
	 (princ)
  tv:choose-variable-values-keyword)

(define-option *backup-host* (BACKUP BACKUP-LOGS)
  "Backup host"
  si:local-host
  (:non-selectable-host)
  "~
This specified the target host for all BACKUP and BACKUP-LOGS
commands.  At this point, BACKUP operations on remote hosts
are not supported.")

(define-option *backup-mode* (BACKUP)
  "Filesystem Backup Mode"
  :incremental
  (:choose (:incremental :full))
  "~
This determines what files in a given domain are to be backed
up.  A value of :FULL means all files in the domain should be
dumped.  :INCREMENTAL means that only those files which have
not been dumped before should be backed up.  Files that have
not previously been backed up are denoted by an excalmation
point (\"!\") to the right of the file length and byte size
in DIRED or LISTF.  Directories typically do not get marked
as backed up.")

(define-option *record-files-as-backed-up* (BACKUP)
  "Record files as backed up"
  t
  (:boolean)
  "~
This determines whether files that are backed up should be marked
as backed up.  For general purposes, this option should be true.")

(defun backup-file-info-generator (&rest ignore)
  (let* ((version (add1 (tape:assess-latest-log-version *backup-mode*)))
	 (machine-name (send si:local-host :short-name))
	 (label-string (format nil "~A-~A-~5,48,d" machine-name *backup-mode* version)))
    (format t "~&Backup label for this tape: ~S" label-string)
    (values label-string
	    (fs:parse-pathname
	      (format nil "~A:BACKUP-LOGS.~A;~D.BACKUP-LOG#1"
		      machine-name
		      *backup-mode*
		      version)))))

(define-command BACKUP-FILESYSTEM (BACKUP)
  "L: Back up the whole local file system. {M: from pathname option}"
  :left (let ((file-list (with-status ("Surveying directories ...")
				 (case *backup-mode*
				   (:incremental (tape:list-new-files))
				   (:full (tape:list-all-files))))))
	  (when file-list
	    (tape:backup-files
	      file-list
	      si:local-host
	      :tape-info-function 'backup-file-info-generator
	      :set-backup-bits *record-files-as-backed-up*
	      :compare *verify-files*)))
  :middle (when *global-pathname-arg*
	    (let ((pathname (fs:parse-pathname *global-pathname-arg*)))
	      (if (neq (send pathname :host) si:local-host)
		  (progn (tv:beep)
			 (format t "~&~%Sorry, remote backups not available yet.~%"))
		(let ((file-list (with-status ("Surveying directories for pathname: ~A" pathname)
				   (tape:full-directory-list
				     pathname
				     :inferiors t
				     :filter-keywords (when (eq *backup-mode* :incremental)
							'(:not-backed-up t))))))
		  (when file-list
		    (tape:backup-files
		      file-list
		      si:local-host
		      :tape-info-function 'backup-file-info-generator
		      :set-backup-bits *record-files-as-backed-up*
		      :compare *verify-files*))))))
  :documentation "~
Backup a filesystem.  If the left button is used, the domain
of files is all files in the filesystem.  If the middle button 
is used, the \"global pathname argument\" must specify a 
wildcarded pathname which is passed to FS:DIRECTORY-LIST to
determine a list of the files to backup.  Various options
will affect the backup as documented.")



;;; Retrieve

(define-option *file-match* (retrieve)
  "File match template"
  (fs:parse-pathname "*;*.*#*" si:local-host)
  (:pathname-or-nil)
  "~
This must be a pathname (optionally wilcarded) which is
used by file match operations (such as FIND-FILE).
It is sent a :pathname-match message with the pathname of
a file on tape (parsed with respect to the same host).
Thus, the lispm pathname parsed from \"*;*.*#*\" will
match all files.")

(define-option *transform* (retrieve)
  "File restore transform"
  nil
  (:any)
  "~
This determines the pathname of files to be restored.
This should be a pathname, something that can be applied to
arguments, or NIL.  If it is NIL, the pathname is derived
from the file property list on tape parsed with respect to the
local host.  If it is a pathname, FS:MERGE-PATHNAMES is used
in which components from the file property list are substituted
for any null components of the transform.  Thus a transform
which is a pathname parsed from \"lamx:bar;\" will cause all
files to be restored to the BAR directory on host LAMX.
If the transform is a function, it must take one argument, the
file property list, and return the pathname to which the file
should be restored. This option is ignored by partition commands.")
  
(define-option *query* (retrieve)
  "Query to restore each file"
  nil
  (:boolean)
  "~
If this is true, the user will be asked whether to restore each
file.  It can be changed in the middle of a restore files command
to allow partial selectivity of file retrieval.")

(define-option *overwrite* (retrieve)
  "Overwrite mode"
  :never
  (:choose (:never :always :query))
  "~
This determines when a file should be overwritten if a file
of the same name type and version already exists.  If this option
is :NEVER, files that already exist are automatically skipped.
If this is :ALWAYS, existing files are always overwritten.
If it is :QUERY, the user will be asked whether to overwite the
file.")

(define-option *create-directory* (retrieve)
  "Create directory"
  :always
  (:choose (:always :query :never :error))
  "~
This option decides what to do if a directory does not exist for
a file to be restored.  :ALWAYS means to create the directory 
automatically.  :NEVER means to skip the file.  :QUERY means ask
the user whether to create the directory or skip the file.
:ERROR causes an FS:DIRECTORY-NOT-FOUND error to be signalled.")

(define-command FIND-FILE (retrieve)
  "Search the tape for a file."
  :left (condition-case ()
	    (let ((plist (send tape:*selected-format* :find-file
			       tape:*selected-device* *file-match*)))
	      (format t "~&Found \"~a\"." (car plist))
	      plist)
	  (tape:logical-end-of-tape
	   (format  t "~&No files matching \"~a\" on tape." *file-match*)
	   (format t "~&*** End of Tape ***~%")))
  :documentation "~
This searches the tape for a file that matches the pathname
specified by the \"file match template\".  If the file is found,
the tape is positioned at the beginning of the file and subsequently
a RESTORE-FILES command can retrieve it.  The file property list
is returned.")

(define-command RESTORE-FILES (retrieve)
  "L: Restore all files from tape {M: Restore N files}"
  :left (condition-case ()
	    (do-forever
	      (when *file-match*
		(send tape:*selected-format* :find-file
		      tape:*selected-device* *file-match*))
	      (send tape:*selected-format* :restore-file
		    tape:*selected-device* 
		    :transform *transform*
		    :query *query*
		    :overwrite *overwrite*
		    :create-directory *create-directory*))
	  (tape:logical-end-of-tape
	   (format t "~&*** End of Tape ***~%")))
  :middle (condition-case ()
	    (dotimes (c *global-numeric-arg*)
	      (when *file-match*
		(send tape:*selected-format* :find-file
		      tape:*selected-device* *file-match*))
	      (send tape:*selected-format* :restore-file
		    tape:*selected-device*
		    :transform *transform*
		    :query *query*
		    :overwrite *overwrite*
		    :create-directory *create-directory*))
;		    :silent *silent*))
	  (tape:logical-end-of-tape
	   (format t "~&*** End of Tape ***~%")))
  :documentation "~
Restore some files (and/or partitions) from the tape.  If the left mouse
button is used, then files are restored until the logical-end-of-tape is
reached.  If the middle button is used, the \"global numeric argument\"
determines how many files to restore.  Other options will affect this
command as documented.")

(define-command install-distribution-tape (retrieve)
  "Install an LMI distribution tape."
  :left (tape:install-distribution-tape)
  :documentation "~
Install an LMI distribution tape.  This is provided
for automatic installation of software release and 
update tapes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backup logs mode
;;;

(define-command load-logs (backup-logs)
  "Load filesystem backup log information"
  :left (tape:load-backup-logs)
  :documentation "~
Load backup information from logs.")

(define-command compile-logs (backup-logs)
  "Compile log files for faster loading."
  :left (tape:compile-backup-logs)
  :documentation "~
Compile all filesystem backup logs into qfasl files
so that they can be loaded faster.")

(define-command find-file-backups (backup-logs)
  "Find all places where a file is backed up. L: use global pathname arg {M: from keyboard}"
  :left (tape:find-file-backups *global-pathname-arg*)
  :middle (tape:find-file-backups
	    (let ((default (fs:merge-pathname-defaults "")))
	      (prompt-and-read
		`(:pathname :defaults ,default)
		(format nil "File match pathname (default ~A) >> "
			default))))
  :documentation "~
Find all backup tapes which contain a specific file.  If
the left button is used, the pathname is determined from
the \"global pathname argument\".  If the middle button is 
used, the pathname is read from the keyboard.
Note: It is important that the LOAD-LOGS command is used
to assure that the latest log information has been loaded.
Otherwise, recently backed up files may not be found.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Self documentation mode.
;;;

(define-option *documentation-format* (self-doc)
  "Self Documentation Format"
  :TEXT
  (:choose (:text :botex))
  "~
Determine the format for the TFrame documentation file
created by the FORMAT-DOCUMENTATION command.  :TEXT format
is straight text that can be printed by any line printer,
mailed to other users, or simply examined in the editor.
:BOTEX format is for internal LMI documentation and will
not generally be useful to the customer.")

(define-command FORMAT-DOCUMENTATION (self-doc)
  "Document TFRAME commands to file (L: global pathname arg {M: Keyboard})"
  :left (let ((pathname (send (fs:parse-pathname *global-pathname-arg* nil
						 (fs:user-homedir-pathname))
			      :new-canonical-type *documentation-format*)))
	  (with-status ("Documenting TFrame to file: ~A" pathname)
	    (document-tframe-to-file pathname *documentation-format*)))
  :middle (let* ((default (send (fs:user-homedir-pathname) :new-pathname
				:name "TFRAME-DOC"
				:canonical-type *documentation-format*))
		 (pathname (prompt-and-read
			     `(:pathname :defaults ,default)
			     (format nil "Documentation output file (default ~A) >> "
				     default))))
	    (with-status ("Documenting TFrame to file: ~A" pathname)
	      (document-tframe-to-file pathname *documentation-format*)))
  :documentation "~
This command writes the online documentation for TFrame commands
to a file suitable for formatting and/or printing.  The format used
for output is determined by the \"Self Documentation Format\" option.
The output file is determined by the \"Global Pathname Argument\"
option if the left mouse button is used.  If the middle button is used,
the user must specify the pathname from the keyboard.  The canonical file
type is always changed according to the format specification.

Currently two formats are supported for output.  The :TEXT format
is raw, with some prettyness added for command-name headers, etc.
:BOTEX format is used internally at LMI, but in the event that BOTEX
is ever released to the field, users may find this a neat way to print
documentation of their own commands and extentions.")

(defvar botex-format-preamble
	"~
@library(patbo)
@library(lisp)
@setpagewidth 6.25in 
@textbodyindent = 0.5in 
@overfullrule 0in
@begin(document)
@baselineskip 13pt 
@parskip 15pt
@parindent = 0in
@defindex vr
~2%")

(defun document-tframe-to-file (pathname format)
  (with-open-file (f pathname :direction :output)
    (ecase format
      (:text (format f "-*- Mode: Text; Base: 10; Package: TFrame -*-~%")
	     (dolist (mode *mode-types*)
	       (format f "~C~%---[TFrame Mode: ~A]---~2%" #\page mode)
	       (ZL:IF (null (get mode :options))
		   (format f "No options defined.~2%")
		 (format f "Options are defined as follows:~2%")
		 (dolist (option (get mode :options))
		   (format f "~A (~S)~%~10T~~?~~2%"
			   (tframe-option-name option)
			   (tframe-option-print-name option)
			   (tframe-option-documentation option))))
	       (ZL:IF (null (get mode :commands))
		   (format f "No commands defined.~2%")
		 (format f "Commands are defined as follows:~2%")
		 (dolist (command (get mode :commands))
		   (format f "~A~%~10T~~?~~2%"
			   (tframe-command-name command)
			   (tframe-command-documentation command)))))
	     (format f "*** End of Tframe Documentation ***~%"))
      (:botex (format f "@comment -*- Mode: Text; Base: 10; Package: TFrame -*-~%")
	      (format f botex-format-preamble)
	      (format f "@subheading Global TFrame options:~2%")
	      (dolist (option *global-options*)
		(format f "@defvar ~A~%~?~%@end defvar~2%"
			(string-downcase (tframe-option-name option))
			(tframe-option-documentation option)))
	      (dolist (mode *mode-types*)
		(format f "@subheading The ~A command mode~2%" mode)
		(ZL:IF (null (get mode :options))
		    (format f "No options defined.~2%")
		  (format f "Options are defined as follows:~2%")
		  (dolist (option (get mode :options))
		    (format f "@defvar ~A~%~?~%@end defvar~2%"
			    (string-downcase (tframe-option-name option))
			    (tframe-option-documentation option))))
		(ZL:IF (null (get mode :commands))
		    (format f "No commands defined.~2%")
		  (format f "Commands are defined as follows:~2%")
		  (dolist (command (get mode :commands))
		    (format f "@b[~A]~2%~?~2%"
			    (tframe-command-name command)
			    (tframe-command-documentation command)))))
	      (format f "@comment *** end of TFrame documentation ***~%@end(document)~%")))))
		
