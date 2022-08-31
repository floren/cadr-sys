;;; -*-Mode: Lisp; Package: FILE-SYSTEM; Base: 8; Lowercase: T -*-

;This file defines operations common to all kinds of directories.

;DIR-NODE itself implements the user-callable operations on directories,
;such as :OPEN-SUBNODE, :CREATE-SUBNODE, :DIRECTORY-STRING,
;and operations passed on by the subnode itself,
;such as RENAME-SUBNODE, CLOSE-SUBNODE, etc.

;The object that specifies a particular subnode of the directory
;is called a PATHSTEP.  Pathsteps can be lists or strings.
;Some formats are standard.  Other formats can be used freely
;by any particular flavor of directory.  The methods of DIR-NODE
;have no knowledge of the format of a pathstep; that knowledge
;belongs entirely to specific kinds of directories.
;Detailed documentation of pathsteps is in LMFILE;PATHNM.

;A PATHLIST is a list of pathsteps, each describing a subnode of
;the node reached by the path so far.

;Every kind of directory has a DIRECTORY-LIST, which is a structure
;whose format depends on the flavor of directory.
;But in any case, that structure contains one DIR-ENTRY for each
;subnode of the directory.
;A DIR-ENTRY is a named structure defined in DEFS.  
;Its components include DIR-ENTRY-NODE, which holds the node-object
;for the subnode when the subnode is open, and DIR-ENTRY-POINTER-INFO,
;which contains the information needed to find the subnode's data
;(such as, a disk address).

;When the directory is initialized, the directory data is read
;from the disk, and DIRECTORY-LIST is set up.  When the directory
;is :FINISHed, the data is written back on the disk.

;Each flavor of directory implements several operations to search and
;modify the DIRECTORY-LIST.  These operations manipulate DIR-ENTRYs
;and pathsteps.  A pathstep contains a filename,
;version, and whatever else the particular flavor of directory wants
;to use to index files by.  These operations include
;FIND-ENTRY, which finds an existing entry
;given a pathstep.
;INSERT-ENTRY and DELETE-ENTRY which insert or remove a given entry
;from the directory-list,
;:BEFORE UNDELETE-SUBNODE, which marks as deleted any other subnodes
;with the same name and version (or whatever), ignoring delete protection.
;REPLACE-ENTRY, which replaces one entry with another.
;READ-DIR, which returns the value of the directory-list instance var
;based on the data stored on disk.
;STORE-DIR, which writes data in the dir file (but does not write it out)
;to record what is in the directory-list.
;LOOP-OVER-DIRECTORY which calls a specified function on each dir-entry
;in the directory-list, and LOOP-OVER-MATCHING-ENTRIES, which also takes
;a mask to select only some of the entries.
;VALIDATE-PATHSTEP which is given a pathstep
;and returns a corrected one after possibly signalling errors,
;FULL-PATHSTEP-TO-STANDARD, which is the inverse of VALIDATE-PATHSTEP;
;it turns a directory-flavor-specific full-pathstep into a standard
;pathstep.  It takes as args the pathstep and a second argument saying
;whether to omit the version number from the standard pathstep.
;REPOSITION-ENTRY, given a dir-entry and a pathstep,
;moves the dir-entry so it comes before the name spec'd in that pathstep.
;OPEN-SUBNODE-PROPERTY-NODE, given reason-why-open, subnode's full-pathstep,
;property pathstep, and open options,
;opens and returns a node representing that subnode's property.
;CREATE-SUBNODE-PROPERTY-NODE, given same args as previous
;except CREATE-SUBNODE options instead of OPEN-SUBNODE options,
;creates a node representing a property of the other subnode.

;The locking discipline is that the subnode is always locked
;before the supernode.  Each place where the supernode sends
;a message to a subnode, it is necessary to show that no
;deadly embrace can result.  This can be done in any of these ways:
; 1) show that the subnode has no reasons-why-open, so nobody
;    else is currently sending it any messages.
; 2) send the message with 'dont-lock.
;    It is necessary to show that use of 'dont-lock causes no lossage.
; 3) show that the subnode is already locked.
;    This is so if we are processing a message sent by the subnode itself,
;    which is a common case.

(defmethod (dir-node :directory-p) () t)

;When a directory node is opened,
;it reads in the directory data from the disk
;to create its directory-list.
;How it does that depends on the type of dir.
(defmethod (dir-node :after init-node) (ignore &key &optional old-entry
					       (inherit-subnodes t)
					       &allow-other-keys)
  (cond ((boundp 'node-file)
	 ;; If the node was not really initted because the pack isn't mounted,
	 ;; don't get an error now.
	 (setq directory-list (funcall-self 'read-dir)
	       open-dir-nodes (cons-in-area self open-dir-nodes background-cons-area)
	       directory-changed nil)
	 ;; Flush all redundant :volume-name pointer info properties.
	 ;; A few were mistakenly created a long time ago.
	 (funcall-self 'loop-over-directory
		       #'(lambda (entry)
			   (let ((vn (get (locf (dir-entry-pointer-info)) ':volume-name)))
			     (cond ((equal vn (pack-volume-name (file-pack)))
				    (remprop (locf (dir-entry-pointer-info)) ':volume-name)
				    (funcall-self 'set-entry-pointer-info
						  entry (dir-entry-pointer-info entry)))))))
	 ;; Make sure we have a :date-last-expunge property
	 ;; and a :earliest-deleted-file property,
	 ;; so changing them later won't require rewriting the entire supernode.
	 (and (null directory-list)
	      (null (funcall-self ':get ':date-last-expunge))
	      (progn (funcall-self 'store-dir)
		     (funcall-self ':putprop ':date-last-expunge 0)
		     (funcall-self ':putprop ':earliest-deleted-file 0)))
	 (let ((old-node (and old-entry
			      (funcall supernode 'open-subnode-entry self old-entry)))
	       (pack (file-pack)))
	   (if (and old-node (null directory-list)
		    (eq (typep self) (typep (funcall old-node ':self)))
		    inherit-subnodes)
	       (progn
		 (setq directory-list
		       (funcall old-node 'make-hard-link-list
				(list (pack-volume-name pack)
				      (pack-number-within-volume pack)
				      (block-number-of-record 0))))
		 (setq directory-changed t)
		 (funcall-self ':finish-contents))
	     (file-write-out)			;In case just created!
	     (push-out-of-core 0 (file-n-records)))))))

(defmethod (dir-node :before really-close) ()
  (setq open-dir-nodes (delq self open-dir-nodes))
  (dir-update-contents nil))

(defmethod (dir-node :always-allow-disk-allocation) () t)

(defmethod (dir-node :before :finish-contents) dir-update-contents)

(defmethod (dir-node :after :finish-contents) (&rest ignore)
  (and (records-modified-p 0 (file-n-records))
       (ferror nil "Some records still not written out"))
  (push-out-of-core 0 (file-n-records)))

(defun dir-update-contents (ignore)
  (declare-flavor-instance-variables (dir-node)
    ;; If data in core has changed, write it into the disk file.
    (if directory-changed
	(funcall-self 'store-dir)
      (funcall-self 'loop-over-directory
		    #'(lambda (entry updater)
			(if (or (dir-entry-changed entry)
				(not (dir-entry-dir-index entry)))
			    (funcall updater 'update-entry entry t)))
		    (funcall-self ':get-handler-for 'update-entry)))
    (setq directory-changed nil)))

;Report that some of the data of a dir entry has changed
;though its length has not changed, so it can be rewritten in place.
;CHANGE-TYPE is either T if only dates and flags have changed,
;:PLIST if the plist data has changed (but its length is the same),
;:FULL if the entry has changed more than that.
(defun dir-entry-mark-changed (entry &optional (change-type t))
  (declare-flavor-instance-variables (dir-node)
    (if (dir-entry-dir-index entry)
	(cond ((eq (dir-entry-changed entry) ':full))
	      ((eq (dir-entry-changed entry) ':plist)
	       (if (eq change-type ':full)
		   (setf (dir-entry-changed entry) ':full)))
	      (t (setf (dir-entry-changed entry) change-type))))))

;This is how the user asks us to open a subnode.
;PATHSTEP says which one.  How it is decoded
;is up to the specific kind of directory through the
;FIND-ENTRY operation, which DIR-NODE does not implement.
;Returns NIL if the specified pathstep does not match an existing subnode.
;May also get an error.
(defmethod (dir-node :open-subnode) (reason-why-open pathstep &rest options
						     &optional &key deleted new-version
						     &allow-other-keys)
  (setq pathstep (funcall-self 'validate-pathstep pathstep))
  (cond ((and (consp pathstep) (eq (car pathstep) ':property))
	 (lexpr-funcall-self ':open-property-node reason-why-open
			     pathstep options))
	((and (consp pathstep) (eq (car pathstep) ':subnode-property))
	 (lexpr-funcall-self 'open-subnode-property-pathstep
			     reason-why-open pathstep options))
	(t
	 (let ((entry (funcall-self 'find-entry pathstep deleted new-version)))
	   (and entry (dir-node-open-subnode reason-why-open entry options))))))

;Though different flavors of directory require different info to
;choose a subnode, and store the data structure differently,
;they are advised to represent the subnode with a DIR-ENTRY structure
;and then use this function to get an open subnode from a dir-entry.
(defun dir-node-open-subnode (reason-why-open entry options)
  (declare-flavor-instance-variables (dir-node)
    (cond ((dir-entry-node entry)
	   ;; dont-lock is safe in this case because
	   ;; :add-reason interlocks with without-interrupts when necessary,
	   ;; so the reasons-why-open list will not be clobbered.
	   ;; At the same time, the node cannot close without sending
	   ;; us a close-subnode-if-no-reasons, and that interlocks with
	   ;; the current operation on the directory.
	   (funcall (dir-entry-node entry)
		    'dont-lock ':add-reason reason-why-open))
	  (t 
	   (let (subnode new-pointer-info)
	     (setq subnode
		   (make-instance (dir-entry-subnode-flavor entry)
				  ':supernode node-closure
				  ':supernode-info entry
				  ':reasons-why-open (list reason-why-open)
				  ':byte-size (dir-entry-byte-size entry)
				  ':length-in-bytes (dir-entry-length)))
	     (setf (values subnode new-pointer-info)
		   (lexpr-funcall subnode 'init-node
				  (dir-entry-pointer-info entry)
				  options))
	     (or (get (locf options) ':preserve-dates)
		 (setf (dir-entry-reference-date entry) (time:get-universal-time)))
	     ;; If initting an external subnode, has its 1st block just been allocated?
	     ;; If so, remember new value.
	     (and new-pointer-info
		  (not (equal new-pointer-info
			      (dir-entry-pointer-info entry)))
		  (progn (funcall-self 'set-entry-pointer-info entry new-pointer-info)
			 (dir-update-contents nil)))
	     (setf (dir-entry-node entry) subnode)
	     subnode)))))

(defmethod (dir-node subnode-pointer-info) (entry)
  (dir-entry-pointer-info entry))

(defmethod (dir-node set-entry-pointer-info) (entry new-pointer-info &optional new-flavor
						    &aux in-link-dirs)
  (and (setq in-link-dirs (get (dir-entry-pointer-info entry) ':in-link-dirs))
       (putprop (locf new-pointer-info) in-link-dirs ':in-link-dirs))
  (setf (dir-entry-pointer-info entry)
	(copy-into-area default-cons-area new-pointer-info))
  (and new-flavor
       (setf (dir-entry-subnode-flavor entry) new-flavor))
  (dir-entry-mark-changed entry ':full))

;Take away a reason for a node to be open.
;Does not close the node!
(defmethod (dir-node :remove-reason) (reason &aux loss-flag)
  (without-interrupts
    (setq loss-flag (not (memq reason reasons-why-open)))
    (or loss-flag
	(setq reasons-why-open (delq reason reasons-why-open)))))

(defmethod (dir-node :before forcibly-close) ()
  (funcall-self 'loop-over-directory
		#'(lambda (entry)
		    (cond ((dir-entry-node entry)
			   (funcall (dir-entry-node entry) 'dont-lock 'forcibly-close)))))
  (setq open-dir-nodes (delq self open-dir-nodes)))

;Create and open a subnode.
(defmethod (dir-node :create-subnode)
	   (reason-why-open pathstep &rest keyword-args
			    &key &optional ((:byte-size subnode-byte-size))
			    reinstall-pack reinstall-first-block-number
			    reinstall-dir-info reinstall-length
			    if-exists
			    flavor (characters t) (element-type nil element-type-p)
			    override-protection &allow-other-keys)
  (unlock-node-and-wait-for-disk-space (file-pack node-file))
  (when element-type-p
    (setf (values characters subnode-byte-size)
	  (decode-element-type element-type subnode-byte-size)))
  (setq pathstep (funcall-self 'validate-pathstep pathstep))
  (cond ((and (consp pathstep) (eq (car pathstep) ':property))
	 (lexpr-funcall-self ':create-property-node reason-why-open
			     pathstep keyword-args))
	((and (consp pathstep) (eq (car pathstep) ':subnode-property))
	 (lexpr-funcall-self 'create-subnode-property-pathstep
			     reason-why-open pathstep keyword-args))
	(t
	 (let (old-entry entry)
	   (let ((tem (assq flavor flavor-name-translation-alist)))
	     (and tem
		  (setq flavor (cdr tem))))
	   (or (memq flavor node-flavor-list)
	       (setq flavor nil))
	   (setq subnode-byte-size
		 (if (memq subnode-byte-size '(nil :default))
		     (if characters 8 16.)
		   (validate-byte-size subnode-byte-size)))
	   (setq old-entry (funcall-self 'find-entry pathstep))
	   (setq entry
		 (create-dir-entry flavor subnode-byte-size old-entry keyword-args))
	   (if (eq if-exists ':supersede)
	       (setq pathstep (funcall-self 'inherit-entry-name-and-version
					    pathstep old-entry))
	     (setq pathstep (funcall-self 'inherit-entry-name pathstep old-entry)))
	   (funcall-self 'insert-entry entry pathstep override-protection)
	   ;; Handle putting a lost file back into the directory.
	   ;; We are given the appropriate pointer-info properties to store in the dir.
	   (cond (reinstall-first-block-number
		  (putprop (locf (dir-entry-pointer-info))
			   reinstall-first-block-number
			   ':first-block-number)
		  (putprop (locf (dir-entry-pointer-info))
			   (pack-number-within-volume reinstall-pack)
			   ':pack-number)
		  (multiple-value-bind (del flg)
		      (extract-redundant-dir-flags reinstall-dir-info)
		    (and del (funcall-self 'undelete-subnode entry))
		    (setf (dir-entry-flags) flg))
		  (setf (dir-entry-subnode-flavor)
			(nth (ldb 1010 reinstall-dir-info) node-flavor-list))
		  (setf (dir-entry-byte-size)
			(ldb 0010 reinstall-dir-info))
		  (setf (dir-entry-length) reinstall-length)
		  (or (string-equal (pack-volume-name (file-pack node-file))
				    (pack-volume-name reinstall-pack))
		      (putprop (locf (dir-entry-pointer-info))
			       (pack-volume-name reinstall-pack)
			       ':volume-name))))
	   (funcall-self ':finish-contents)
	   (dir-node-open-subnode reason-why-open entry
				  (list* ':old-entry old-entry
					 keyword-args))))))

;Create and initialize a new entry and return it.
;Does not insert it in the directory; use the INSERT-ENTRY operation for that.
;It is marked as deleted.
;Undelete it yourself to make the file "really exist".
;Usually that is not done until the data has been written.
;OLD-ENTRY is used for inheritance.

;All the args except the keyword args are assumed valid.
(defun create-dir-entry (flavor byte-size old-entry keyword-args
			 &aux new-flavor characters entry-flags)
  (declare-flavor-instance-variables (dir-node)
    (setq flavor
	  (or flavor
	      (and old-entry
		   (or (dir-entry-dir-p old-entry)
		       (get (dir-entry-subnode-flavor old-entry) 'inheritable))
		   (dir-entry-subnode-flavor old-entry))
	      'text-node))
    (setq characters (get (locf keyword-args) ':characters))
    (setf (values keyword-args new-flavor entry-flags)
	  (lexpr-funcall (make-instance flavor ':supernode node-closure)
			 'default-pointer-info old-entry keyword-args))
    (and new-flavor (setq flavor new-flavor))
    ;; Don't create any nodes if there is a disk space emergency
    ;; on the pack this directory is on.
    (and (< (+ (pack-number-of-available-blocks (file-pack node-file))
	       (pack-number-of-free-blocks (file-pack node-file)))
	    (pack-danger-available-blocks (file-pack node-file)))
	 (process-wait "Diskfull"
		       #'(lambda (pack)
			        (> (pack-number-of-available-blocks pack)
				   (pack-danger-available-blocks pack)))
		       (file-pack node-file)))
    (let ((entry (make-dir-entry)))
      (setf (dir-entry-pointer-info entry) keyword-args)
      (setf (dir-entry-creation-date entry) (time:get-universal-time))
      (setf (dir-entry-reference-date entry) (time:get-universal-time))
      (setf (dir-entry-length entry) 0)
      (setf (dir-entry-byte-size entry) byte-size)
      (setf (dir-entry-subnode-flavor entry) flavor)
      (setf (dir-entry-deleted entry) t)
      (setf (dir-entry-flags entry) entry-flags)
      (and characters
	   (push ':characters (dir-entry-flags entry)))
      entry)))

;Operate on subnodes which are not necessarily open,
;without opening them if they are not already open.
;If a subnode-op is supplied, then a value of nil
;is taken to mean that the subnode was not found
(defmethod (dir-node :operate-on-subnode-by-name) (pathstep node-op subnode-op args
							    &key &optional deleted
							    &allow-other-keys)
 (let (entry)
  (setq pathstep (funcall-self 'validate-pathstep pathstep))
  (if (or (memq (car pathstep) '(:property :subnode-property))
	  (and (setq entry (funcall-self 'find-entry pathstep deleted))
	       (dir-entry-node entry)))
      (let (subnode other-lock (temp-reason (gen-temp-reason)))
	(and (setq subnode
		   (funcall-self ':open-pathstep temp-reason
				 pathstep ':deleted deleted))
	     (unwind-protect (progn
			       (setq other-lock (gobble-another-lock subnode))
			       (lexpr-funcall subnode node-op args))
			     (and other-lock (process-unlock other-lock))
			     (funcall subnode ':remove-reason temp-reason))))
      (and entry
	   (lexpr-funcall-self subnode-op entry args)))))

;Handle operations which open subnodes pass along to us.
;Here, we can freely send messages back to the subnode
;with no fear of deadly embraces, because the subnode
;is already locked by us.

(defmethod (dir-node delete-subnode) (entry &optional no-protection)
  (and (not no-protection)
       (memq ':delete-protect (dir-entry-flags entry))
       (cerror ':no-action nil
	       'dont-delete-flag-set
	       "Old file is delete-protected."
	       (pathlist-into-pathname
		 (send self 'subnode-standard-pathlist entry))))
  (if (dir-entry-deleted entry)
      nil
    (setf (dir-entry-deleted entry) t)
    ;; Update ref date when we delete a file
    ;; so that auto expunge can validly use the ref date
    ;; to decide when it is time to expunge a file.
    (setf (dir-entry-reference-date entry) (time:get-universal-time))
    ;; Cause our :earliest-deleted-file property
    ;; to be the date at which the least-recently-deleted file was deleted.
    (or (funcall supernode 'subnode-get supernode-info ':earliest-deleted-file)
	(funcall supernode 'subnode-putprop supernode-info ':earliest-deleted-file
		 (time:get-universal-time)))
    (dir-entry-mark-changed entry)
    (funcall-self 'update-entry entry))
  t)

;There should be a :before undelete-subnode in the definition
;of each flavor of directory, which deals with deleting
;other subnodes with the same name, version, etc.
(defmethod (dir-node undelete-subnode) (entry)
  (cond ((dir-entry-deleted entry)
	 (setf (dir-entry-deleted entry) nil)
	 (dir-entry-mark-changed entry)
	 (funcall-self 'update-entry entry)
	 ;; Undeletion should undelete all the way up to the root.
	 (funcall-self ':undelete)
	 ;; Undeleting a non-backed-up entry should mark this dir as not backed up.
	 (or (memq ':dumped (dir-entry-flags entry))
	     (progn (funcall-self ':putprop ':not-backed-up t)
		    (funcall supernode ':finish-contents)))
	 t)))

;Change byte size of a subnode of this node.
;Args are the subnode, the new byte size, and the new length in bytes.
(defmethod (dir-node set-subnode-byte-size)
	   (entry new-byte-size length-in-new-bytes)
  (setf (dir-entry-byte-size entry) new-byte-size)
  (setf (dir-entry-length entry) length-in-new-bytes)
  (dir-entry-mark-changed entry))

;Rename a subnode of this node.
;Args are the subnode, the dir to put it under (perhaps this one again),
;and a pathstep to specify the new name.
;The other dir should already be locked.
(defmethod (dir-node rename-subnode) (entry other-dir pathstep)
  (setq pathstep (funcall other-dir 'validate-pathstep pathstep))
  ;; Tell insert-entry to delete the entry from its old position
  ;; after doing all error checks.
  (setq pathstep (funcall other-dir 'insert-entry entry pathstep nil node-closure))
  (funcall other-dir ':finish-contents)
  (funcall-self ':finish-contents)
  pathstep)

;The subnode thinking about closing itself sends us this
;so that we can give it the chance to decide whether it has any reasons
;knowing that we are locked.
(defmethod (dir-node close-subnode-if-no-reasons) (subnode)
  (funcall subnode 'close-if-no-reasons-internal))

(defmethod (dir-node :any-reasons-p) ()
  (or (not (null reasons-why-open))
      (*catch 'any-reasons-p
	(progn
	  (funcall-self 'loop-over-directory
			#'(lambda (entry)
			    (and (dir-entry-node entry)
				 (funcall (dir-entry-node entry) ':any-reasons-p)
				 (*throw 'any-reasons-p t))))
	  nil))))

(defmethod (dir-node :close-if-no-reasons) ()
  (or reasons-why-open
      (funcall-self ':any-reasons-p)
      (funcall supernode 'close-subnode-if-no-reasons node-closure)))

(defmethod (dir-node close-subnode)
	   (entry length &optional (new-pointer-info nil p-info-changed)
		  &aux length-changed)
  (cond (entry
	 (funcall (dir-entry-node entry) 'really-close)
	 (setf (dir-entry-node entry) nil)
	 (setq length-changed ( (dir-entry-length entry) length))
	 (setf (dir-entry-length entry) length)
	 (cond (p-info-changed
		(funcall-self 'set-entry-pointer-info entry new-pointer-info))
	       (length-changed (dir-entry-mark-changed entry)))
	 ;; If we changed anything, update it on disk right now.
	 (funcall-self 'update-entry entry))))

(defmethod (dir-node finish-subnode)
	   (entry length &optional (new-pointer-info nil p-info-changed)
		  &aux length-changed)
  (setq length-changed ( (dir-entry-length entry) length))
  (setf (dir-entry-length entry) length)
  (cond (p-info-changed
	 (funcall-self 'set-entry-pointer-info entry new-pointer-info))
	(length-changed (dir-entry-mark-changed entry)))
  (funcall-self 'update-entry entry))

(defmethod (dir-node reposition-subnode) (entry pathstep)
  (setq pathstep (funcall-self 'validate-pathstep pathstep))
  (funcall-self 'reposition-entry entry pathstep)
  (funcall-self ':finish-contents))

;When an entry is inserted or deleted,
;the flavor of that entry may want to do something.

;For example, a hard-link-node wants to add or remove
;a back pointer each time it is put into or taken out of a directory.

;In any case, it is necessary to tell the subnode that it has
;a different supernode now.  This is what fixes superfile pointers
;when a file is renamed to a different dir.  Because of this,
;renames between dirs must always open the file.

;No locking difficulties in this one,
;since it sends no message to the subnode.
(defmethod (dir-node :after insert-entry) (entry &rest ignore)
  ;; If this entry is not deleted and not dumped,
  ;; mark the directory as not dumped likewise.
  (or (dir-entry-deleted entry)
      (memq ':dumped (dir-entry-flags entry))
      (progn (funcall-self ':putprop ':not-backed-up t)
	     (funcall supernode ':finish-contents)))
  (and (dir-entry-node entry)
       (funcall (dir-entry-node entry) 'new-supernode
		node-closure entry))
  (let ((fn (get (dir-entry-subnode-flavor entry) 'entry-inserted-or-deleted)))
    (and fn (funcall fn entry t)))
  (or (memq ':dumped (dir-entry-flags entry))
      (funcall-self ':putprop ':dumped nil)))

(defmethod (dir-node :after delete-entry) (entry &rest ignore)
  (let ((fn (get (dir-entry-subnode-flavor entry) 'entry-inserted-or-deleted)))
    (and fn (funcall fn entry nil))))

;Add a file to a node's :in-link-dirs.
;Each descriptor is a string which reads in as a list
;which looks like (volume pack first-block-num)
;and identifies a directory which contains a link to this file.
;Descriptors end with Returns.
;Do a :finish soon after calling this.
(defmethod (dir-node subnode-add-in-link) (entry descriptor)
  (let ((base 10.) (*nopoint t))
    (setq descriptor (format:output nil (prin1 descriptor) (terpri))))
  (let ((old-string (or (get (locf (dir-entry-pointer-info entry)) ':in-link-dirs)
			"")))
    (putprop (locf (dir-entry-pointer-info entry))
	     (string-append descriptor old-string)
	     ':in-link-dirs)))

;Removes one instance of a given descriptor from the subnode's :in-link-dirs.
;If there are multiple copies of a descriptor, you must remove them
;multiple times (or set ALL-COPIES to t.
(defmethod (dir-node subnode-delete-in-link) (entry descriptor &optional all-copies)
  (let ((base 10.) (*nopoint t))
    (setq descriptor (format:output nil (prin1 descriptor) (terpri))))
  (let ((old-string (or (get (locf (dir-entry-pointer-info entry)) ':in-link-dirs)
			"")))
    (do (size position) (())
      (setq size (string-length descriptor))
      ;; Find an instance of this descriptor.  Return if none.
      (or (setq position (string-search descriptor old-string))
	  (return))
      ;; Delete the instance we just found.
      (let ((new-string (string-append (substring old-string 0 position)
				       (substring old-string (+ position size)))))
	(if (zerop (string-length new-string))
	    (remprop (locf (dir-entry-pointer-info entry))
		     ':in-link-dirs)
	  (putprop (locf (dir-entry-pointer-info entry))
		   new-string
		   ':in-link-dirs)))
      ;; Maybe one is all we want to delete.
      (or all-copies (return)))))

(defmethod (dir-node subnode-delete-all-in-links) (entry)
  (remprop (locf (dir-entry-pointer-info entry))
	   ':in-link-dirs))

;If we have a subnode which lives in the specified first block number
;and pack, open it.  Otherwise return NIL.
;If we have non-file subnodes, open them and pass the request along.
(defmethod (dir-node open-subnode-given-first-block)
	   (reason pack first-block-number)
  (funcall-self 'loop-over-directory 'dir-open-entry-given-first-block
		reason pack first-block-number))


(defun dir-open-entry-given-first-block (entry reason pack first-block-number)
  (let (object)
    ;; Loop point for each directory entry.
    ;; To find out what kind of subnode, instantiate the flavor 
    ;; but don't send an init-node message, so we don't do any disk ops.
    (setq object (make-instance (dir-entry-subnode-flavor entry)))
    (cond ((funcall object 'file-node-p)
	   ;; It's a file node of some sort.
	   ;; Is it the one we want?
	   (and (string-equal (pack-volume-name pack)
			      (cadr (memq ':volume-name (dir-entry-pointer-info entry))))
		(= (pack-number-within-volume pack)
		   (or (cadr (memq ':pack-number (dir-entry-pointer-info entry))) -1))
		(= first-block-number
		   (or (cadr (memq ':first-block-number (dir-entry-pointer-info entry))) -1))
		(*throw 'open-subnode-given-first-block
			(dir-node-open-subnode reason entry nil))))
	  ;; Handle non-file subnodes.
	  ;; This is commented out because 1) it threatens deadly embrace,
	  ;; and 2) cannot be necessary as long as all directories
	  ;; are file-nodes, which they are.
;	  (t (let ((subnode (dir-node-open-subnode reason entry)))
;	       (unwind-protect
;		 (funcall subnode 'open-subnode-given-first-block
;			  reason pack first-block-number)
;		 (funcall subnode ':remove-reason reason))))
	  )))

;Discard a subnode of this directory, without even bothering
;to delete its blocks (or look at it at all).
;The most we do is mark its first block as not starting a file.
(defmethod (dir-node dike-out-subnode) (pathstep)
  (let* (volume-name pack-number first-block-number pack entry
	 pointer-info)
    (if (typep pathstep 'dir-entry)
	(setq entry pathstep
	      pathstep (dir-entry-full-pathstep entry))
      (setq pathstep
	    (funcall-self 'validate-pathstep pathstep))
      (setq entry (funcall-self 'find-entry pathstep t)))
    (cond (entry
	   (setq pointer-info (dir-entry-pointer-info entry))
	   (setq volume-name
		 (or (get (locf pointer-info) ':volume-name)
		     (pack-volume-name (funcall supernode ':pack))))
	   (setq pack-number (get (locf pointer-info) ':pack-number))
	   (setq first-block-number (or (get (locf pointer-info) ':first-block-number) 0))
	   (cond ((and pack-number (not (zerop first-block-number)))
		  (if (setq pack (find-mounted-pack volume-name pack-number))
		      (and (y-or-n-p "Clear the block-starts-file bit? ")
			   (set-block-starts-file-bit pack first-block-number 0))
		      (ferror nil "Pack ~d, volume ~s not mounted" pack-number volume-name))))
	   (funcall-self 'delete-entry entry)))
    first-block-number))

;When an entry is about to be really deleted,
;if there are any in-links to it,
;rename the entry in place of one of them.
;Value is non-nil if the file migrated, or if there are any hard links
;that ought to preserve the file from deletion even though we can't migrate there now.
(defun dir-migrate-entry (entry in-link-string)
  (with-input-from-string (s in-link-string)
    (do ((temp-reason (gen-temp-reason))
	 preserve-the-file
	 subnode)
	(())
      (let (descriptor othernode pack)
	(or (setq descriptor
		  (let ((ibase 10.) (base 10.)) (read s nil)))
	    (return preserve-the-file))
	(setq pack (find-mounted-pack (car descriptor) (cadr descriptor)))
	(unwind-protect
	  (if (null (setq othernode
			  (open-node-given-first-block temp-reason pack
						       (caddr descriptor))))
	      (funcall-self 'subnode-delete-in-link entry descriptor t)
	    (setq subnode (dir-node-open-subnode temp-reason entry '(:preserve-dates t)))
	    (or (funcall othernode 'superior-p subnode)
		(if (funcall othernode 'replace-hard-link entry subnode)
		    (return t)
		  (funcall-self 'subnode-delete-in-link entry descriptor t))))
	  (and othernode (funcall othernode ':remove-reason temp-reason))
	  (and subnode (funcall subnode ':remove-reason temp-reason)))))))

(defmethod (dir-node replace-hard-link) (entry subnode)
  (*catch 'replace-hard-link
    (funcall-self 'loop-over-directory
      #'(lambda (my-entry other-entry subnode)
	  (and (dir-entries-info-matches my-entry other-entry ':volume-name)
	       (dir-entries-info-matches my-entry other-entry ':pack-number)
	       (dir-entries-info-matches my-entry other-entry ':first-block-number)
	       (eq (dir-entry-subnode-flavor my-entry) 'hard-link-node)
	       (*throw 'replace-hard-link
		       (let* ((pack (file-pack node-file))
			      (volume (pack-volume-name pack))
			      (pack-number (pack-number-within-volume pack))
			      (fbn (block-number-of-record 0))
			      (descriptor (list volume pack-number fbn)))
			 (funcall-self 'replace-entry my-entry other-entry)
			 (funcall subnode 'new-supernode node-closure)
			 (funcall-self 'subnode-delete-in-link other-entry descriptor)
			 (funcall-self ':finish-contents)
			 t))))
      entry subnode)))

(defmethod (dir-node :before replace-entry) (my-entry other-entry)
  (setf (dir-entry-deleted other-entry) (dir-entry-deleted my-entry))
  (setf (dir-entry-reference-date other-entry) (dir-entry-reference-date my-entry))
  (setf (dir-entry-creation-date other-entry) (dir-entry-creation-date my-entry))
  (setf (dir-entry-plist other-entry) (dir-entry-plist my-entry))
  (setf (dir-entry-full-pathstep other-entry) (dir-entry-full-pathstep my-entry))
  (setf (dir-entry-dir-index other-entry) (dir-entry-dir-index my-entry))
  (let ((old-flags (dir-entry-flags other-entry)))
    (setf (dir-entry-flags other-entry) nil)
    (and (memq ':characters old-flags)
	 (push ':characters (dir-entry-flags other-entry)))
    (and (memq ':delete-protect (dir-entry-flags my-entry))
	 (push ':delete-protect (dir-entry-flags other-entry)))
    (and (memq ':supersede-protect (dir-entry-flags my-entry))
	 (push ':supersede-protect (dir-entry-flags other-entry)))))

(defun dir-entries-info-matches (entry1 entry2 pointer-info-name)
  (equal (get (locf (dir-entry-pointer-info entry1)) pointer-info-name)
	 (get (locf (dir-entry-pointer-info entry2)) pointer-info-name)))

(defvar files-being-deleted)

;Really delete all subnodes of this node marked as deleted.
;This method is for the user to call.
;Returns the number of blocks of space freed.
(defmethod (dir-node :expunge) (&optional exempt-recently-deleted-files recursive
				&aux files-being-deleted
				(time-now (time:get-universal-time))
				(blocks-being-freed 0)
				final-blocks-being-freed)
  (and exempt-recently-deleted-files
       (setq exempt-recently-deleted-files (- time-now auto-expunge-interval)))
 (unwind-protect
   (cond ((or (not exempt-recently-deleted-files)
	      (not (funcall-self ':get ':earliest-deleted-file))
	      (< (funcall-self ':get ':earliest-deleted-file)
		 exempt-recently-deleted-files))
	  (let* ((earliest-deleted-file time-now))
	    (declare (special earliest-deleted-file))
	    (funcall-self 'loop-over-directory 'dir-expunge-entry nil
			  exempt-recently-deleted-files recursive)
	    (funcall-self ':putprop ':earliest-deleted-file
			  ;; Never set this to nil even if no deleted files,
			  ;; because if we do, it changes the length
			  ;; and forces a full rewrite of the supernode.
			  earliest-deleted-file)
	    (funcall-self ':putprop ':date-last-expunge
			  (or exempt-recently-deleted-files
			      time-now)))))
   (progn
     (dolist (bunch (file-blocks-to-be-freed))
       (incf blocks-being-freed
	     (cadr bunch)))
     (setq final-blocks-being-freed blocks-being-freed)
     (funcall-self ':finish)
     (incf final-blocks-being-freed (length files-being-deleted))
     (mapc 'file-finish-delete files-being-deleted)))
 final-blocks-being-freed)


;Really delete all the subnodes of this directory,
;whether marked as deleted or not,
;but only if none of them is in open.
;Returns T iff they were deleted.
(defmethod (dir-node expunge-all) (&aux files-being-deleted)
  (if (funcall-self 'subnodes-have-reasons-p t)
      nil
      (unwind-protect
	  (let* ((tem (time:get-universal-time))
		 (earliest-deleted-file tem))
	    (declare (special earliest-deleted-file))
	    (funcall-self 'loop-over-directory 'dir-expunge-entry t)
	    (funcall-self ':putprop ':earliest-deleted-file
			  (and (not (= earliest-deleted-file tem))
			       earliest-deleted-file)))
	(progn (funcall-self ':finish-contents)
	       (mapc 'file-finish-delete files-being-deleted)))
      t))

;ALL-FILES is either T to expunge every file in the dir,
;NIL to expunge all deleted files, or OLD-FILES to expunge
;all deleted files not referenced in the last AUTO-EXPUNGE-INTERVAL time.
;RECURSIVE says expunge all subdirectories.
(defun dir-expunge-entry (entry all-files &optional only-if-deleted-before recursive
				&aux did-expunge)
  ;; We cannot do anything for a node which anyone else might try to use,
  ;; for fear of a deadly embrace.
  (and (or (null (dir-entry-node entry))
	   (null (funcall (dir-entry-node entry)
			  'dont-lock ':any-subnodes-have-reasons-p)))
       ;; We cannot do anything about a file on a non-mounted volume.
       (let ((volume-name (get (locf (dir-entry-pointer-info entry)) ':volume-name)))
	 (or (null volume-name)
	     (find-mounted-pack volume-name 0)))
       ;; Now decide whether this node should be really deleted.
       (if (or (eq all-files t) (dir-entry-deleted entry))
	   (let ((in-link-dirs
		   (get (locf (dir-entry-pointer-info entry)) ':in-link-dirs)))
	     ;; Maybe only flush files not referenced for a while
	     (and (or (null only-if-deleted-before)
		      (> only-if-deleted-before (dir-entry-reference-date entry)))
		  ;; Maybe migrate this node to a different dir.
		  (not (and in-link-dirs
			    (dir-migrate-entry entry in-link-dirs)
			    (progn (funcall-self 'delete-entry entry)
				   (setq did-expunge t))))
		  (let ((subnode (dir-node-open-subnode self entry '(:preserve-dates t)))
			tem)
		    ;; We can send messages to the subnode with no fear of deadly embrace,
		    ;; because we already know nobody else is using it,
		    ;; and nobody else can open it while this dir is locked.
		    (funcall subnode 'expunge-all)
		    (cond ((funcall subnode ':any-subnodes-p)
			   (funcall subnode ':remove-reason self))
			  (t
			   (setq tem (funcall subnode 'really-delete))
			   (setq did-expunge t)
			   (and tem
				(push tem files-being-deleted))
			   (funcall-self 'delete-entry entry))))))
	 ;; Here for entries we are not trying to delete.
	 ;; If it is a directory, expunge it.
	 (and (dir-entry-dir-p entry)
	      recursive
	      (let ((subnode (dir-node-open-subnode self entry '(:preserve-dates t))))
		;; We can send messages to the subnode with no fear of deadly embrace,
		;; because we already know nobody else is using it,
		;; and nobody else can open it while this dir is locked.
		(incf blocks-being-freed
		      (funcall subnode ':expunge only-if-deleted-before recursive))
		(funcall subnode ':remove-reason self)))))
  ;; As we go through the dir, for all deleted files we cannot expunge,
  ;; accumulate the earliest ref date.
  (if (dir-entry-deleted entry)
      (local-declare ((special earliest-deleted-file))
	(and (not did-expunge)
	     (< (dir-entry-reference-date entry) earliest-deleted-file)
	     (setq earliest-deleted-file
		   (dir-entry-reference-date entry))))))

(defmethod (dir-node :any-subnodes-p) ()
  (*catch 'any-subnodes-p
    (funcall-self 'loop-over-directory #'(lambda (entry) entry (*throw 'any-subnodes-p t)))
    nil))

;Return non-nil if this node or any indirect subnode
;has a reason to be open or is locked.
;Never waits for locks on any subnodes.
(defmethod (dir-node :any-subnodes-have-reasons-p) ()
  (or reasons-why-open
      (let ((old-inhibit-scheduling-flag inhibit-scheduling-flag)
	    (inhibit-scheduling-flag t))
	(or (and node-lock (neq node-lock current-process))
	    (funcall-self 'subnodes-have-reasons-p old-inhibit-scheduling-flag)))))

(defmethod (dir-node subnodes-have-reasons-p) (old-inhibit-scheduling-flag)
  (or old-inhibit-scheduling-flag
      (setq inhibit-scheduling-flag nil))
  (*catch 'any-subnodes-have-reasons-p
    (funcall-self 'loop-over-directory
		  #'(lambda (entry)
		      (and (dir-entry-node entry)
			   (funcall (dir-entry-node entry)
				    'dont-lock
				    ':any-subnodes-have-reasons-p)
			   (*throw 'any-subnodes-have-reasons-p t))))))

(defmethod (dir-node subnode-standard-pathlist) (entry)
  (let ((default-cons-area node-caller-area))
    (append (funcall-self ':standard-pathlist)
	    (list (funcall-self 'full-pathstep-to-standard
				(dir-entry-full-pathstep entry)
				(memq ':supersede-protect (dir-entry-flags entry)))))))

(defmethod (dir-node :directory-list-stream) (pathlist &optional options)
  (let ((entire-directory-list
	  (funcall-self ':standard-directory-list pathlist options)))
    (declare (special entire-directory-list))
    (closure '(entire-directory-list)
	     #'(lambda (&optional op arg) op arg
		       (local-declare ((special entire-directory-list))
			 (prog1 (car entire-directory-list)
				(pop entire-directory-list)))))))

;;Note! This unlocks the directory!
(defmethod (dir-node :standard-directory-list) (pathlist &optional options)
  (let* (accumulate-list
	 (accept-deleted-nodes (memq ':deleted options))
	 (temp-reason (gen-temp-reason))
	 (dir-pathlist (funcall-self ':standard-pathlist))
	 (dir-pathname (make-pathname-internal lfs-host ':unspecific
					       (or dir-pathlist '(root))
					       ':unspecific ':unspecific ':unspecific)))
    (declare (special accumulate-list))
    (setq node-lock nil)
    ;; Push an entry for each subnode that deserves one.
    ;; Their order is reversed in the process.
    (funcall node-closure 'dont-lock 'dir-iterate
	     pathlist accept-deleted-nodes ()
	     'dir-node-accumulate-subnodes
	     dir-pathname temp-reason node-caller-area)
    ;; Push another entry, for the directory as a whole (NIL instead of a pathname).
    (let ((default-cons-area node-caller-area))
      (cons (list nil
		  ':settable-file-properties t
		  ':all-properties-settable t
		  ':unsettable-properties lmfile-unsettable-properties
		  ':pathname (funcall dir-pathname ':new-name-type-and-version pathlist)
		  ':physical-volume-free-blocks
		  (do ((packs pack-list (cdr packs))
		       (accum))
		      ((null packs) accum)
		    (let ((pack (car packs)))
		      (push (cons (format nil "~A-~D"
					  (pack-volume-name pack)
					  (pack-number-within-volume pack))
				  (+ (pack-number-of-free-blocks pack)
				     (pack-number-of-available-blocks pack)))
			    accum))))
	    (nreverse accumulate-list)))))

(defmacro with-entry-node-open ((node reason entry . options) &body body)
  `(let ((,node (funcall node-closure 'open-subnode-entry ,reason ,entry . ,options)))
     (and ,node
	  (unwind-protect
	   (progn . ,body)
	   (funcall ,node ':remove-reason ,reason)))))

;This function is called on each matching subnode by operate-on-matching-entries.
;It may push this node's pathname onto ACCUMULATE-LIST,
;and it may also call the operate-on-matching-entries operation recursively.
(defun dir-node-accumulate-subnodes
       (entry name-pathlist pathrest accept-deleted-nodes max-version
	dir-pathname reason area)
  (local-declare ((special accumulate-list))
    (let ((default-cons-area area))
      (or (and pathrest (neq pathrest t))
	  (push (cons (funcall dir-pathname
			       ':new-name-type-and-version
			       name-pathlist)
		      (dir-entry-subnode-plist entry))
		accumulate-list))))
  (and (or (not (numberp max-version))
	   (equal max-version (caddar (last name-pathlist))))
       (dir-entry-dir-p entry)
       pathrest
       (with-entry-node-open (node reason entry ':no-error-if-not-mounted t)
         (dir-entry-operate-on-matching-subnodes
	  node entry pathrest
	  name-pathlist accept-deleted-nodes
	  'dir-node-accumulate-subnodes
	  dir-pathname reason area))))

;A function passed to OPERATE-ON-MATCHING-ENTRIES
;can call this function to continue the tree walk to the 
;next level down in the hierarchy.
;Given an entry, it opens the node and sends OPERATE-ON-MATCHING-ENTRIES to it.
(defun dir-entry-operate-on-matching-subnodes (node entry pathrest name-pathlist
					       accept-deleted-nodes
					       function &rest args)
  (lexpr-funcall node 'dont-lock 'dir-iterate
		 pathrest accept-deleted-nodes
		 (if (memq ':supersede-protect
			   (dir-entry-flags entry))
		     (append (butlast name-pathlist)
			     (list (funcall-self
				    'full-pathstep-to-standard
				    (dir-entry-full-pathstep entry)
				    t)))
		     name-pathlist)
		 function args))

(defmethod (dir-node open-subnode-entry) (reason entry &rest options)
  (and (*catch 'foo
	 (funcall-self 'loop-over-directory
		       #'(lambda (e entry) (and (eq e entry) (*throw 'foo t)))
		       entry))
       (dir-node-open-subnode reason entry options)))

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
	(declare (special dump-starting-directory))
	(with-entry-node-open (node reason entry ':preserve-dates t)
	  (if (dir-entry-dir-p entry)
	      (progn
	       (dir-entry-operate-on-matching-subnodes
		 node entry pathrest
		 name-pathlist accept-deleted-nodes
		 'dir-node-dump-subnodes
		 reason incremental-flag dumper-function)))
	  (funcall dumper-function node)))))

(defmethod (dir-node :tape-dump) (dumper-function &rest args)
  (lexpr-funcall dumper-function node-closure
	   #'(lambda (node tape-stream)
	       (funcall node 'loop-over-directory
			#'(lambda (entry tape-stream)
			    (if (dir-entry-deleted entry)
				nil
			      (print (dir-entry-full-pathstep entry) tape-stream)
			      (if (get (locf (dir-entry-pointer-info entry)) ':link-to)
				  (progn (princ ":link-to " tape-stream)
					 (or (memq ':dumped (dir-entry-flags entry))
					     (push ':dumped (dir-entry-flags entry)))
					 (prin1 (get (locf (dir-entry-pointer-info entry))
						     ':link-to)
						tape-stream)))))
			tape-stream)
	       (terpri tape-stream)
	       1000)
	   args))

;We don't actually try to compare the data on the tape with what
;we wrote about the directory, because it is no disaster if it doesn't match.
;Instead, we mark the directory as dumped if all files now in it are so marked.
(defmethod (dir-node :tape-compare) (tape-stream comparison-plist &optional mark-as-dumped)
  comparison-plist tape-stream
  ;; Make sure dump bits set in files in this dir are written out on disk.
  (funcall-self ':finish)
  (cond ((and mark-as-dumped
	      (not
		(*catch 'some-subnode-undumped
		  (progn
		    (funcall-self 'loop-over-directory
				  #'(lambda (entry)
				      (or (memq ':dumped (dir-entry-flags entry))
					  (dir-entry-deleted entry)
					  (eq (dir-entry-subnode-flavor entry)
					      'hard-link-node)
					  (*throw 'some-subnode-undumped t))))
		    nil))))
	 (funcall-self ':putprop ':not-backed-up nil)
	 t)))

;Plists of subnodes.

(defmethod (dir-node subnode-plist)
	   (entry &optional explicit-properties-only include-truename)
  (if include-truename
      (list* ':truename
	     (pathlist-into-pathname
	       (funcall-self 'subnode-standard-pathlist entry))
	     (dir-entry-subnode-plist entry))
      (dir-entry-subnode-plist entry explicit-properties-only)))

;:NOT-BACKED-UP is actually an exception in that :DUMPED's sense is opposite to it.
;So each use must make an exception for it somehow.
;Nevertheless, having it on this list saves some code.
(defconst entry-flag-property-alist
	  '((:installed . :installed)
	    (:characters . :characters)
	    (:dont-delete . :delete-protect)
	    (:dont-supersede . :supersede-protect)
	    (:not-backed-up . :dumped)))

(defun dir-entry-subnode-plist (entry &optional explicit-properties-only)
  (declare-flavor-instance-variables (dir-node)
    (let ((explicit-props (copy-into-area node-caller-area (dir-entry-plist entry))))
      (or (get (locf explicit-props) ':author)
	  (setq explicit-props (list* ':author (funcall-self ':get ':author)
				      explicit-props)))
      (if explicit-properties-only
	  explicit-props
	(let ((tem
		(list* ':creation-date (copy-into-area node-caller-area
						       (dir-entry-creation-date entry))
		       ':byte-size (dir-entry-byte-size entry)
		       ':reference-date (copy-into-area node-caller-area
							(dir-entry-reference-date entry))
		       ':flavor (car (rassq (dir-entry-subnode-flavor entry)
					    flavor-name-translation-alist))
		       ':length-in-bytes (dir-entry-length entry)
		       ':length-in-blocks (ceiling
					    (ceiling
					      (dir-entry-length entry)
					      (truncate 32. (dir-entry-byte-size entry)))
					    page-size)
		       (append-pointer-info-properties
			 entry
			 explicit-props))))
	  (and (dir-entry-deleted entry)
	       (setq tem (list* ':deleted t tem)))
	  (or (memq ':dumped (dir-entry-flags entry))
	      (setq tem (list* ':not-backed-up t tem)))
	  (dolist (prop entry-flag-property-alist)
	    (and (neq (car prop) ':not-backed-up)
		 (memq (cdr prop) (dir-entry-flags entry))
		 (setq tem (list* (car prop) t tem))))
	  (and (dir-entry-dir-p entry)
	       (setq tem (list* ':directory t tem)))
	  (eliminate-duplicate-properties tem))))))

(defun eliminate-duplicate-properties (plist)
  (do ((l plist (cddr l)))
      ((null l))
    ;; Remprop the n'th property from the remaining properties
    ;; and repeat until we don't find it again.
    (do () ((not (remprop (cdr l) (car l))))))
  plist)

(defconst pointer-info-property-names
	  '(:pack-number
	    :volume-name
	    :link-to))

(defun append-pointer-info-properties (entry plist)
  (do ((l (dir-entry-pointer-info entry) (cddr l)))
      ((null l) plist)
    (and (memq (car l) pointer-info-property-names)
	 (setq plist (list* (car l) (copy-into-area default-cons-area (cadr l))
			    plist)))))

(defmethod (dir-node subnode-get) (entry indicator
				   &optional explicit-properties-only
				   &aux tem)
  (copy-into-area
    node-caller-area
    (if explicit-properties-only
	(or (and (memq indicator pointer-info-property-names)
		 (get (locf (dir-entry-pointer-info entry)) indicator))
	    (get (locf (dir-entry-plist entry)) indicator)
	    (and (eq indicator ':author)
		 (funcall supernode 'subnode-get supernode-info indicator)))
	(selectq indicator
	  (:deleted (dir-entry-deleted entry))
	  (:not-backed-up (null (memq ':dumped (dir-entry-flags entry))))
	  (:creation-date (dir-entry-creation-date entry))
	  (:byte-size (dir-entry-byte-size entry))
	  (:reference-date (dir-entry-reference-date entry))
	  (:flavor (car (rassq (dir-entry-subnode-flavor entry)
			       flavor-name-translation-alist)))
	  (:directory (dir-entry-dir-p entry))
	  (:length-in-bytes (dir-entry-length entry))
	  (:length-in-blocks (ceiling (ceiling
					(dir-entry-length entry)
					(truncate 32. (dir-entry-byte-size entry)))
				      page-size))
	  (t (if (setq tem (assq indicator entry-flag-property-alist))
		 (not (null (memq (cdr tem) (dir-entry-flags entry))))
	       (or (and (memq indicator pointer-info-property-names)
			(get (locf (dir-entry-pointer-info entry)) indicator))
		   (get (locf (dir-entry-plist entry)) indicator)
		   (and (eq indicator ':author)
			(funcall supernode 'subnode-get supernode-info indicator)))))))))

(defmethod (dir-node subnode-putprop) (entry indicator value
				       &optional explicit-properties-only
				       &aux tem)
  (setq indicator (copy-into-area default-cons-area indicator))
  (setq value (copy-into-area default-cons-area value))
  (cond (explicit-properties-only
	 (dir-entry-putprop entry value indicator explicit-properties-only))
	((setq tem (assq indicator entry-flag-property-alist))
	 (if (eq indicator ':not-backed-up)
	     (setq value (not value)))
	 (cond ((neq (null value)
		     (null (memq (cdr tem) (dir-entry-flags entry))))
		(if value
		    (push (cdr tem) (dir-entry-flags entry))
		  (setf (dir-entry-flags entry)
			(delq (cdr tem) (dir-entry-flags entry))))
		(dir-entry-mark-changed entry))))
	(t
	 (selectq indicator
	   (:length-in-blocks nil)
	   (:flavor nil)
	   (:directory nil)
	   (:pack-number nil)
	   (:volume-name nil)
	   (:link-to
	    (if (get (locf (dir-entry-pointer-info entry)) ':link-to)
		(progn
		  (putprop (locf (dir-entry-pointer-info entry))
			   value ':link-to)
		  (dir-entry-mark-changed entry ':full))))
	   (:deleted
	    (if value (funcall-self 'delete-subnode entry)
		(funcall-self 'undelete-subnode entry)))
	   (:creation-date (setf (dir-entry-creation-date entry) value)
	    (dir-entry-mark-changed entry))
	   (:byte-size
	    (cond ((dir-entry-node entry)
		   (funcall (dir-entry-node entry) ':set-byte-size value))
		  (t (setf (dir-entry-byte-size entry) value)
		     (dir-entry-mark-changed entry))))
	   (:reference-date (setf (dir-entry-reference-date entry) value)
	    (dir-entry-mark-changed entry))
	   (:length-in-bytes
	    (cond ((dir-entry-node entry)
		   (funcall (dir-entry-node entry) ':set-length-in-bytes value))
		  (t (setf (dir-entry-length entry) value)
		     (dir-entry-mark-changed entry))))
	   (t (dir-entry-putprop entry value indicator explicit-properties-only))))))

(defun dir-entry-putprop (entry value indicator
			  &optional explicit-properties-only)
  (declare-flavor-instance-variables (dir-node)
    (or (equal value (funcall-self 'subnode-get entry indicator
				   explicit-properties-only))
	;; To check for plists we can't store,
	;; change a copy and check it first, then make it official.
	(let* ((new-plist (copylist (dir-entry-plist entry)))
	       (old-length (funcall-self 'plist-current-length entry)))
	  (putprop (locf new-plist) value indicator)
	  (if (= old-length (plist-length-if-stored new-plist))
	      (dir-entry-mark-changed entry ':plist)
	    (dir-entry-mark-changed entry ':full))
	  (setf (dir-entry-plist entry) new-plist)))))

(defmethod (dir-node subnode-remprop) (entry propname)
  (and (get (locf (dir-entry-plist entry)) propname)
       (progn (remprop (locf (dir-entry-plist entry)) propname)
	      (dir-entry-mark-changed entry ':full))))

(defmethod (dir-node subnode-change-properties) (entry finish-flag &rest properties)
  (do ((l properties (cddr l))) ((null l))
    (funcall-self 'subnode-putprop entry (car l) (cadr l)))
  (and finish-flag (funcall-self ':finish-contents))
  t)
