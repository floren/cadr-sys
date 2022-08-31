;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Lowercase:T -*-

(defflavor local-file-pathname () (lmfile-parsing-mixin meaningful-root-mixin pathname))

;Define the pathname operations which actually operate on the file.

;Here is a common driver for operations on a node specified by a pathlist
;which really only need to access the directory entry for that node,
;and can therefore refrain from opening the node if it is not open already.
;The caller must specify both an operation that can be done to the node itself
;(in case it is already open), and another operation that does the job when
;given to the supernode, in case the node is not actually open.
;ARGS are passed to the operation in either case.
;OPTIONS are those for the OPEN-NODE we must do if the pathlist
;is such that we must open the node to find its supernode.
;
;If SUBNODE-OPERATION is supplied as NIL, always open the specified node.
(defun operate-on-pathlist-node (directory namepath pathname operation subnode-operation
				 args options)
  (let ((last-step (car (last (append (and (neq directory ':unspecific) directory)
				      namepath))))
	(temp-reason (gen-temp-reason)))
    (if (or (symbolp last-step)	  ;ROOT and SUPERNODE => must open.
	    (and (consp last-step)	  ;(:PROPERTY ...), ditto.
		 (neq (car last-step) ':version))
	    (not subnode-operation))
	(let ((node (lexpr-funcall 'open-node root-node temp-reason
				   directory namepath pathname
				   options)))
	  (unwind-protect
	   (lexpr-funcall node operation args)
	   (funcall node ':remove-reason temp-reason)))
	(let ((supernode (open-node root-node temp-reason
				    (butlast (append directory namepath)) nil
				    pathname)))
	  (unwind-protect
	   (or (lexpr-funcall supernode ':operate-on-subnode-by-name
			      last-step operation subnode-operation args options)
	       (ferror 'file-not-found "File not found."
		       (pathlist-into-pathname
			 (append (send supernode ':standard-pathlist)
				 (list last-step)))))
	   (funcall supernode ':remove-reason temp-reason))))))

(defmacro file-error-retry ((error-p) &body body)
  "Execute BODY within a resume handler for :RETRY-FILE-OPERATION and also handle ERROR-P.
If ERROR-P is NIL, a file error in BODY causes us to return an error object."
  (let* ((tag (gensym)))
    `(condition-case-if (not ,error-p) (result)
	 (prog t ()
	    retry
	       (return-from t
		 (catch-continuation-if t ',tag #'(lambda (ignore) (go retry)) nil
		   (with-stack-list* (eh:condition-resume-handlers
				       '((file-error pathname-error)
					 :retry-file-operation t
					 ("retry the operation on the same file.")
					 si:catch-error-restart-throw ,tag)
				       eh:condition-resume-handlers)
		     . ,body))))
       ((file-error pathname-error) result))))

(defmethod (local-file-pathname :delete) (&optional (error-p t))
  (let ((args `(:error ,error-p :preserve-dates t)))
    (file-error-retry (error-p)
      (operate-on-pathlist-node directory (merge-name-type-and-version) self
				':delete 'delete-subnode
				nil args))))

(defmethod (local-file-pathname :undelete) (&optional (error-p t))
  (let ((args `(:error ,error-p :preserve-dates t
		:deleted t)))
    (file-error-retry (error-p)
      (operate-on-pathlist-node directory (merge-name-type-and-version) self
				':undelete 'undelete-subnode
				nil args))))

;NAME-ONLY says that the arg is only the last name component
;and it should be used literally, even preserving its case.
(defmethod (local-file-pathname :rename) (new-name &optional (error-p t) name-only)
  (and (stringp new-name)
       (not name-only)
       (setq new-name (merge-pathname-defaults new-name (default-pathname nil host))))
  (let ((args `(:error ,error-p :preserve-dates t)))
    (file-error-retry (error-p)
      (operate-on-pathlist-node directory (merge-name-type-and-version) self
				':rename nil
				(list new-name) args))))

(defmethod (local-file-pathname :create-link) (to-pathname &key (error t))
  (and (stringp to-pathname)
       (setq to-pathname (merge-pathname-defaults to-pathname (default-pathname nil host))))
  (close (open self ':create t ':flavor ':link
	       ':link-to (funcall to-pathname ':string-for-host)
	       ':error error))
  t)

(defmethod (local-file-pathname :properties) (&optional (error-p t))
  (let ((args `(:error ,error-p :preserve-dates t)))
    (file-error-retry (error-p)
      (cdr (operate-on-pathlist-node directory (merge-name-type-and-version) self
				     ':plist 'subnode-plist
				     '(nil t) args)))))

(defmethod (local-file-pathname :multiple-file-plists) (pathnames &optional options)
  options
  (mapcar #'(lambda (pn)
	      (let ((props (funcall pn ':properties nil)))
		(if props
		    (list* pn ':truename props)
		  pn)))
	  pathnames))

(defmethod (local-file-pathname :create-directory) (&optional &key (error t))
  (let ((file (send self ':directory-pathname-as-file)))
    (close (open file ':if-does-not-exist ':create ':error error ':flavor ':directory))))

(defun directory-op-via-stream (pathname error-p deleted preserve-dates
				operation &rest args)
  (let (stream
	(dirname (funcall pathname ':directory-pathname-as-file)))
    (unwind-protect
      (progn (setq stream (funcall dirname ':open dirname
				   ':preserve-dates preserve-dates
				   ':error error-p ':deleted deleted))
	     (if (errorp stream)
		 stream
	       (lexpr-funcall stream operation args)))
      (and stream (not (errorp stream))
	   (close stream)))))

(defmethod (local-file-pathname :directory-list) (options)
  (directory-op-via-stream self
			  (not (memq ':noerror options))
			  nil t
			  ':file-operation ':standard-directory-list
			  (merge-name-type-and-version)
			  options))

(defmethod (local-file-pathname :directory-list-stream) (&optional options)
  (directory-op-via-stream self
			  (not (memq ':noerror options))
			  nil t
			  ':file-operation ':directory-list-stream
			  (merge-name-type-and-version)
			  options))

(defmethod (local-file-pathname :its-directory-stream) (&optional (error-p t))
  (directory-op-via-stream self error-p nil t
			   ':file-operation ':its-directory-stream))

(defmethod (local-file-pathname :expunge) (&key &optional (error t))
  (directory-op-via-stream self error nil nil
			   ':expunge nil (equal name '("**"))))

(defmethod (local-file-pathname :complete-string) (string options)
  (let ((accept-deleted-nodes (memq ':deleted options))
	dev dir nam typ vers flag completed-pathlist
	dirlength namepath)
    (setf (values dev dir nam typ vers)
	  (funcall-self ':parse-namestring nil string))
    (or dir (setq dir directory))
    (setq dirlength (if (consp dir) (length dir) 0))
    (setf (values completed-pathlist flag)
	  (funcall root-node ':complete-pathlist
		   (append (and (neq dir ':unspecific) dir)
			   (merge-name-type-and-version-1 nam typ vers))
		   accept-deleted-nodes))
    (setq namepath (nthcdr dirlength completed-pathlist))
    (values
      (string-append (funcall host ':name-as-file-computer)
		     ": "
		     (if (and dev (neq dev ':unspecific))
			 (string-append dev ": ")
		       "")
		     (if (neq dir ':unspecific)
			 (pathlist-to-string (firstn dirlength completed-pathlist) t)
		       "")
		     (if (neq dir ':unspecific)
			 (if namepath "; " ";")
		       "")
		     (if namepath (pathlist-to-string namepath t) ""))
      (cond ((eq flag t) ':old)
	    ((eq flag '/?) ':old)
	    (t ':new)))))     

(defmethod (local-file-pathname :undelete-multiple-files) (errorflag pathnames)
  (let ((value))
    (dolist (p pathnames)
      (push (funcall p ':undelete errorflag) value))
    (and (some value 'prog1) value)))

(defmethod (local-file-pathname :delete-multiple-files) (errorflag pathnames)
  (let ((value))
    (dolist (p pathnames)
      (push (funcall p ':delete errorflag) value))
    (and (some value 'prog1) value)))

(defmethod (local-file-pathname :change-properties) (error-p &rest properties)
  (let ((args `(:error ,error-p :preserve-dates t :deleted t)))
    (file-error-retry (error-p)
      (operate-on-pathlist-node directory (merge-name-type-and-version) self
				':change-properties 'subnode-change-properties
				(cons t properties) args))))

(defmethod (local-file-pathname :open) (pathname &rest keyword-args
					&key &optional (error t)
					&allow-other-keys)
  (let ((namepath (merge-name-type-and-version))
	(pathname (or pathname self))
	(error-p error))
    (file-error-retry (error-p)
      (lexpr-funcall 'open-node-stream
		     pathname directory namepath
		     ':volume-name (and (neq device ':unspecific) device)
		     keyword-args))))

;Implement the streams whereby we access file-computer files.

;Given a fully expanded pathlist, open a stream and return it.
;DIRECTION can be ':INPUT, ':OUTPUT, or NIL to return a non-IO object,
; or ':DIRECTORY to get a directory stream.
;OPTIONS are alternating name and value.
;Allowed names include
; :noerror    - T => no error if we fail.
; :byte-size  - specify byte size for this I/O.
;		Also specifies file byte size for new file.
; :create     - NIL => require old node, T or :REQUIRED => always create node,
;		:ALLOWED => use old node if any, else create.
;
;Two keywords relate to character set conversion:
; :characters - T => the caller wants the data bytes to be characters.
; :pdp10-format  - T means caller wants the PDP10 char set,
;		provided that the data are characters.
;If the node is represented in pdp10 format, and the caller
;does not want pdp10 format, and the caller is asking for characters,
;then the stream must convert.
;If the node is not represented in pdp10 format,
;the caller wants pdp10 format, then if the node believes it contains
;characters, the stream must convert its data to pdp10 characters,
;put groups of five into 36-bit words, then extract bytes of the
;specified size from them.  If the node believes it does not contain
;characters, then the bytes of the node must be assembled as many as
;will fit into 36-bit words, then bytes extracted from them.
;
;These rules are based on the assumption that Lisp machine format files
;and Lisp machine callers always know whether they deal with characters,
;whereas pdp10 format files and pdp10 callers should not be believed.
;
;Other keywords are passed on to :OPEN-PATHSTEP or :CREATE-PATHSTEP
;Two which are processed by :OPEN-PATHSTEP are
; :deleted    - T => old deleted files are ok to open.
; :inhibit-links  - T => don't trace a link.  Open the link itself.
;Keywords processed by :CREATE-PATHSTEP include
; :flavor and any pointer-info keyword understood
; by the particular flavor.

;In case of error, our second value is the error condition name.
(defun open-node-stream (pathname directory namepath
			 &rest options)
  (let (node temp-reason creating)
    (setq temp-reason (gen-temp-reason))
    (unwind-protect
      (progn
	(setf (values node creating)
	      (lexpr-funcall 'open-node root-node temp-reason
			     directory namepath pathname options))
	(when node
	  (lexpr-funcall 'make-node-stream node
			 pathname temp-reason creating
			 options)))
      (and node (funcall node ':remove-reason temp-reason)))))

;Given a pathlist and some options and flags,
;open and return two values: the node, and whether it was just created.
;In the pathlist, the symbols ROOT and SUPERNODE are processed here;
;anything else is passed along to the directory node.
;The pathlist is passed in two parts: directory and namepath.
;These two args are effectively appended together,
;except that if we are willing to create a new node
;we are also willing to create all the steps of the namepath,
;but the steps of the directory must always already exist.
(defun open-node (starting-node reason directory namepath pathname &rest options
		  &key &optional direction
		  (create nil create-p)
		  (if-exists (if (eq (pathname-version pathname)
				     ':newest)
				 ':new-version ':error))
		  (if-does-not-exist
		    (cond ((memq direction '(:probe :probe-directory :probe-link))
			   nil)
			  ((and (eq direction ':output)
				(not (memq if-exists '(:overwrite :truncate :append))))
			   ':create)
			  ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			  ;; for compatibility with the past.
			  ;; A Common-Lisp program would use :PROBE
			  ;; and get NIL as the default for this.
			  (t ':error)))
		  deleted 
		  &allow-other-keys
		  &aux prev-node (next-node starting-node)
		  (starting-namepath namepath)
		  creating-flag not-last-step-options
		  probe-directory-flag)
  (declare (return-list node creating-flag))
  (unless (memq direction '(nil :probe :probe-directory :probe-link :input :output))
    (ferror 'inconsistent-options
	    "Invalid direction keyword ~2*~S"
	    pathname ':open direction))
  (unless (memq if-exists '(:error :new-version :rename :rename-and-delete
			    :overwrite :append :truncate :supersede nil))
    (ferror 'inconsistent-options
	    "~2*~S is not valid for :IF-EXISTS."
	    pathname ':open if-exists))
  (unless (memq if-does-not-exist '(:error :create nil))
    (ferror 'inconsistent-options
	    "~2*~S is not valid for :IF-DOES-NOT-EXIST."
	    pathname ':open if-does-not-exist))
  (selectq direction
    (:probe-link
     (setq direction nil
	   options (list* ':inhibit-links t options)))
    (:probe (setq direction nil))
    (:probe-directory
     (setq direction nil
	   probe-directory-flag t)))
  (and (eq directory ':unspecific)
       (setq directory nil))
  (setq not-last-step-options
	`(:deleted ,deleted :flavor :directory))
  (or starting-node
      (ferror 'no-file-system
	      "LMFILE not in operation"
	      pathname))
  (values
    (unwind-protect
      (progn
	(funcall starting-node ':add-reason reason)
	(do ((pathrest (append directory namepath) (cdr pathrest))
	     ;; DOING-DIRECTORY is T while working on the directory,
	     ;; NIL while working on the namepath.
	     (doing-directory t)
	     this-time-old-file-ok this-time-new-file-ok)
	    ((null pathrest)
	     (prog1 next-node
		    ;; Prevent unwind-protect from removing our reason!
		    (setq next-node nil prev-node nil)))
	  (if (eq pathrest namepath)
	      (setq doing-directory nil))
	  (shiftf prev-node next-node nil)
	  ;; Decide whether creation of a new node is ok at this step,
	  ;; and whether use of an old node is ok at this step.
	  ;; On the last step, this is controlled by :create.
	  ;; On earlier steps, an old node is ok and a new one is not.
	  (cond (doing-directory
		 (setq this-time-old-file-ok t this-time-new-file-ok nil))
		(probe-directory-flag
		 (unless (send prev-node ':directory-p)
		   (ferror 'directory-not-found
			   "~2*~A is not a directory."
			   pathname ':open
			   (send
			     (pathlist-into-pathname
			       (send prev-node ':standard-pathlist))
			     ':pathname-as-directory)))
		 (return (shiftf prev-node nil)))
		((eq create ':allowed)
		 (setq this-time-old-file-ok t this-time-new-file-ok t))
		((or (eq create ':required) (eq create t))
		 ;; Creation of the desired node can be required,
		 ;; but supernodes along the way are always ok if already existing.
		 (setq this-time-old-file-ok (not (null (cdr pathrest)))
		       this-time-new-file-ok t))
		(create-p
		 (setq this-time-old-file-ok t this-time-new-file-ok nil))
		(t (setq this-time-old-file-ok
			 (or (cdr pathrest)
			     (neq direction ':output)
			     ;; These all want to LOOK for an existing file.
			     (memq if-exists '(:overwrite :append :truncate :error nil
							  :rename :rename-and-delete)))
			 this-time-new-file-ok
			 (eq if-does-not-exist ':create))))
	  ;; use :open-pathstep or :create-pathstep to open the next pathstep.
	  (setq next-node (or (and this-time-old-file-ok
				   (lexpr-funcall prev-node ':open-pathstep reason
						  (car pathrest)
						  (if (cdr pathrest)
						      not-last-step-options
						      options)))
			      (and this-time-new-file-ok
				   (setq creating-flag t)
				   (lexpr-funcall prev-node ':create-pathstep
						  reason (car pathrest)
						  (if (cdr pathrest)
						      not-last-step-options
						      options)))))
	  (when (and (eq direction ':output)
		     (null (cdr pathrest))
		     (not creating-flag))
	    (selectq if-exists
	      (:error
	       (ferror 'file-already-exists
		       "File ~A already exists."
		       pathname
		       ':open))
	      (nil (return nil))
	      ((:rename :rename-and-delete)
	       (send next-node ':rename "renamed-old-file")
	       (if (eq if-exists ':rename-and-delete)
		   (send next-node ':delete))
	       (send next-node ':remove-reason reason)
	       (setq next-node (lexpr-funcall prev-node ':create-pathstep
					      reason (car pathrest)
					      (if (cdr pathrest)
						  not-last-step-options
						options)))
	       (setq creating-flag t))
	      (:truncate
	       (send next-node ':set-length-in-bytes 0))))	      
	  ;; If we create a directory for this, undelete it right away.
	  (and creating-flag (cdr pathrest) next-node
	       (funcall next-node ':undelete))
	  (or (eq next-node prev-node)
	      (funcall prev-node ':remove-reason reason))
	  (or next-node (return (cond (creating-flag
				       (ferror 'superior-not-directory
					       "Attempt to make subnode of non-directory node ~A."
					       (pathlist-into-pathname
						 (send prev-node ':standard-pathlist))))
				      (doing-directory
				       (ferror 'directory-not-found "Directory ~2*~A not found."
					       pathname ':open
					       (send
						 (pathlist-into-pathname
						   (append (send prev-node ':standard-pathlist)
							   (list (car pathrest))))
						 ':pathname-as-directory)))
				      ((not if-does-not-exist)
				       (return nil))
				      (t
				       (ferror 'file-not-found "File not found for ~A."
					       (pathlist-into-pathname starting-namepath))))))))
	 (and prev-node (funcall prev-node ':remove-reason reason))
	 (and next-node (funcall next-node ':remove-reason reason)))
    creating-flag))

;This operation exists so that other kinds of nodes (such as PDP10-FILE-NODE)
;can supply other streams (such as, to do character code translation).
(defun make-node-stream (node pathname old-reason creating
			      &rest options
			      &key &optional preserve-dates direction
			      &allow-other-keys)
  (let ((stream
	  (lexpr-funcall node 'dont-lock ':make-stream
			 pathname creating
			 options)))
    (cond ((or creating
	       (and (not preserve-dates)
		    (eq direction ':output)))
	   (funcall node ':putprop ':creation-date (time:get-universal-time))
	   (funcall node ':putprop ':author user-id)))
    (funcall node ':remove-reason old-reason)
    stream))

;Given some parameters, create a stream for accessing this node's data.
(defmethod (node :make-stream) (pathname creating
				&key &optional (characters t) pdp10-format
				(element-type nil element-type-p)
				((:byte-size spc-byte-size))
				(direction ':input)
				(if-exists (if (eq (pathname-version pathname)
						   ':newest)
					       ':new-version ':error))
				&allow-other-keys)
  (when element-type-p
    (setf (values characters byte-size)
	  (decode-element-type element-type byte-size)))
  (and (eq pdp10-format ':default)
       (setq pdp10-format nil))
  (and (or pdp10-format (eq characters ':default))
       (setq characters (funcall-self ':characters)))
  (and (eq spc-byte-size ':default)
       (setq spc-byte-size (if pdp10-format 7 byte-size)))
  (and spc-byte-size
       (do () ((memq spc-byte-size '(1 2 4 8 16.)))
	 (setq spc-byte-size (cerror t nil 'bad-byte-size
				     "The byte size ~2*~D is impossible"
				     (pathlist-into-pathname (send self ':standard-pathlist))
				     ':open spc-byte-size))))
  (setq spc-byte-size
	(or spc-byte-size
	    (cond ((not pdp10-format)
		   (if characters 8 16.))
		  (t (if characters 7 16.)))))
  (do () ((memq direction '(:input :output nil :probe-directory :probe-link :probe)))
    (setq direction (cerror ':new-value nil 'inconsistent-options
			    "The direction ~2*~S is not defined."
			    (pathlist-into-pathname (send self ':standard-pathlist))
			    ':open direction)))

  (let ((default-cons-area node-caller-area)
	(stream
	 (make-instance
	   (selectq direction
	     (:INPUT (if (and pdp10-format characters)
			 'fc-pdp10-input-stream
		       (if characters
			   'fc-lispm-character-input-stream
			 'fc-lispm-input-stream)))
	     (:OUTPUT (if (and pdp10-format characters)
			  'fc-pdp10-output-stream
			(if characters
			    'fc-lispm-character-output-stream
			  'fc-lispm-output-stream)))
	     (:PROBE-DIRECTORY 'fc-probe-directory-stream)
	     (t 'fc-probe-stream))
	   ':byte-size spc-byte-size
	   ':must-explicitly-close creating
	   ':node node-closure ':characters characters
	   ':pathname pathname)))
    (and (memq direction '(:input :output))
	 (funcall-self ':add-reason stream))
    (and (eq if-exists ':append)
	 (funcall stream ':set-pointer
		  (funcall-self ':length-in-bytes
				(if pdp10-format 8 spc-byte-size))))
    stream))

(defvar free-fc-cs-bfr nil)

(defun get-fc-cs-bfr ()
  (without-interrupts
    (cond (free-fc-cs-bfr
	   (prog1 free-fc-cs-bfr
		  (setq free-fc-cs-bfr
			(array-leader free-fc-cs-bfr 0))))
	  (t (make-array 2000. ':type 'art-8b ':leader-length 1
			 ':area working-storage-area)))))

(defun free-fc-cs-bfr (bfr)
  (without-interrupts
    (setf (array-leader bfr 0) free-fc-cs-bfr)
    (setq free-fc-cs-bfr bfr)))

;;; This special type of stream is used for probe opens.
;;; It does not keep the node open, just extracts the appropriate info
;;; and remembers it while closing the node.
(defflavor fc-probe-stream (byte-size characters truename plist node length
			    must-explicitly-close)
	   (si:file-stream-mixin)
  :initable-instance-variables
  :gettable-instance-variables)

(defmethod (fc-probe-stream :set-notify-on-close) (value)
  value nil)

(defmethod (fc-probe-stream :after :init) (&rest ignore)
  (setq length
	(convert-length (funcall node ':length-in-bytes)
			(funcall node ':byte-size)
			byte-size))
  (setq truename (pathlist-into-pathname (funcall node ':standard-pathlist)))
  (setq plist (funcall node ':plist))
  (setq node nil))

(defmethod (fc-probe-stream :get) (property)
  (get (locf plist) property))

(defmethod (fc-probe-stream :direction) () nil)

(defmethod (fc-probe-stream :author) ()
  (funcall node ':get 'author))

;; This kind of stream is made for a :PROBE-DIRECTORY open.
;; The truename we report contains our node's name as the directory,
;; with NIL as the other components.
(defflavor fc-probe-directory-stream () (fc-probe-stream))

(defmethod (fc-probe-directory-stream :after :init) (ignore)
  (setq truename (send truename ':pathname-as-directory)))

;The variables TRUENAME and CREATION-DATE are used to remember those things
;after the stream has been closed.
;BYTE-SIZE and CHARACTERS are those of the stream, not the node,
;but they will agree with the node if they were arranged
;to default from the node.
;MUST-EXPLICITLY-CLOSE is T if the node was created for this open.
;It serves two purposes: it tells remote streams that they MUST
;send an explicit close operation over the net, and it tells this stream
;to undelete the file when the stream is closed.
;NOTIFY-ON-CLOSE is an object or function to call if we are forced to close.
;It is passed the symbol :FORCE-CLOSE and the stream being closed.
;This is used to inform the remote machine if a server's stream is closed under it.
(defflavor fc-stream (node byte-size characters must-explicitly-close
			   (notify-on-close nil)
			   (truename nil)
			   (creation-date nil))
	   (si:file-stream-mixin)
  :initable-instance-variables
  (:settable-instance-variables notify-on-close)
  (:gettable-instance-variables node byte-size characters must-explicitly-close))

(defmethod (fc-stream :after :init) (&rest ignore)
  (and node (funcall lfs-host ':add-stream self)))

(defmethod (fc-stream :force-close) ()
  (and notify-on-close (funcall notify-on-close ':force-close self))
  (funcall lfs-host ':remove-stream self)
  (setq node nil))

(defmethod (fc-stream :close) (&optional flag)
  (cond (node
	 ;; Closing a stream that created a node undeletes it, unless we say "abort".
	 (cond ((neq flag ':abort)
		(and must-explicitly-close
		     (funcall node ':undelete))
		(funcall-self ':finish)))
	 (setq truename (funcall-self ':truename))
	 (setq creation-date (funcall-self ':get ':creation-date))
	 (funcall node ':remove-reason self)
	 (setq node nil)))
  (funcall lfs-host ':remove-stream self))

;Dummy, since :CLOSE wants to send this.
(defmethod (fc-stream :finish) ()
  ())

(defmethod (fc-stream :file-operation) (&rest args)
  (lexpr-funcall node args))

(defmethod (fc-stream :length) ()
  (convert-length (funcall node ':length-in-bytes)
		  (funcall node ':byte-size)
		  byte-size))

(defmethod (fc-stream :set-byte-size) (new-byte-size)
  (setq byte-size (validate-byte-size new-byte-size)))

(defmethod (fc-stream :set-creation-date) (new-creation-date)
  (setq creation-date new-creation-date)
  (funcall node ':putprop ':creation-date creation-date))

(defmethod (fc-stream :set-author) (new-author)
  (funcall node ':putprop ':author new-author))

(defmethod (fc-stream :change-properties) (error-p &rest new-plist)
  (lexpr-funcall node ':change-properties error-p new-plist))

(defmethod (fc-stream :set-plist) (new-plist)
  (lexpr-funcall node ':change-properties t new-plist))

(defmethod (fc-stream :author) ()
  (funcall node ':get 'author))

(defmethod (fc-stream :qfaslp) ()
  (and node (funcall node ':qfaslp)))

(defmethod (fc-stream :plist) ()
  (funcall node ':plist))

(defmethod (fc-stream :property-list) ()
  (funcall node ':plist))

(defmethod (fc-stream :get) (property)
  (if node (funcall node ':get property)))

(defmethod (fc-stream :directory-list) (&optional error-p)
  error-p
  (funcall node ':standard-directory-list
	   (funcall si:pathname ':name-type-and-version)))

(defmethod (fc-stream :directory-list-stream) (&optional error-p)
  error-p
  (funcall node ':directory-list-stream
	   (funcall si:pathname ':name-type-and-version)))

(defmethod (fc-stream :its-directory-stream) (&optional error-p)
  error-p
  (funcall node ':its-directory-stream))

(defmethod (fc-stream :expunge) (&optional exempt-recently-deleted-files recursive)
  (funcall node ':expunge exempt-recently-deleted-files recursive))

(defmethod (fc-stream :creation-date) ()
  (or creation-date
      (funcall node ':get ':creation-date)))

(defmethod (fc-stream :truename) ()
  (or truename
      (pathlist-into-pathname (funcall node ':standard-pathlist))))

(defmethod (fc-stream :rename) (new-name &optional (error-p t))
  (condition-case-if (not error-p) (result)
      (funcall node ':rename
	       (if (stringp new-name) (parse-pathname new-name "LFS")
		 new-name))
    ((file-error pathname-error)
     result)))

(defmethod (fc-stream :delete) (&optional (error-p t))
  (condition-case-if (not error-p) (result)
      (funcall node ':delete)
    ((file-error pathname-error)
     result)))

(defmethod (fc-stream :undelete) (&optional (error-p t))
  (condition-case-if (not error-p) (result)
      (funcall node ':undelete)
    ((file-error pathname-error)
     result)))

(defflavor fc-data-stream
	(io-node (ptr 0) (status 0) (buffer (get-fc-cs-bfr)) ind-buffer)
	(fc-stream))

(defmethod (fc-data-stream :after :init) (&rest ignore)
  (setq io-node node))

(defmethod (fc-data-stream :after :close) (&rest ignore)
  (and io-node (funcall io-node ':remove-reason self))
  (and buffer (free-fc-cs-bfr buffer))
  (setq buffer nil))

(defflavor fc-input-stream ()
	   (fc-data-stream)
  (:included-flavors  si:input-pointer-remembering-mixin si:buffered-input-stream))

(defmethod (fc-input-stream :discard-input-buffer) (array)
  array)

(defflavor fc-output-stream ()
	   (fc-data-stream)
  (:included-flavors si:output-pointer-remembering-mixin si:buffered-output-stream))

(defmethod (fc-output-stream :discard-output-buffer) (array)
  array)

(defmethod (fc-output-stream :get-old-data) (array count)
  array count
  (let ((preserve-ptr ptr) (preserve-status status))
    (funcall-self ':next-input-buffer nil)
    (setq ptr preserve-ptr status preserve-status)))

;Lispm data-format streams

(defflavor fc-lispm-stream-mixin (factor) ()
  (:included-flavors fc-data-stream))

(defmethod (fc-lispm-stream-mixin :after :init) (&rest ignore)
  (funcall-self ':set-byte-size byte-size))

(defmethod (fc-lispm-stream-mixin :after :set-byte-size) (ignore)
  (setq factor (truncate 8 (lsh 1 (haulong (1- byte-size)))))
  (setq ind-buffer
	(if (< 4 byte-size 9)
	    buffer
	    (make-array (* (truncate (array-length buffer) 4)
			   (truncate 32. (lsh 1 (haulong (1- byte-size)))))
			':type (or (nth (1- byte-size)
					'(art-1b art-2b art-4b art-4b))
				   'art-16b)
			':displaced-to buffer))))

(defmethod (fc-lispm-stream-mixin :pdp10-format) () nil)

(defmethod (fc-lispm-stream-mixin :set-buffer-pointer) (pointer &aux tem)
  (setf (values ptr status tem)
	(funcall io-node ':lispm-format-pointer pointer factor))
  tem)

(defflavor fc-lispm-input-stream
	() (fc-lispm-stream-mixin fc-input-stream))

;This operation used by :get-old-data on output streams!
(defmethod (fc-lispm-stream-mixin :next-input-buffer) (no-hang-p)
  no-hang-p
  (let (num-bytes real-buffer real-buffer-index)
    (setf (array-leader buffer 0) 0)
    (setf (values ptr status num-bytes real-buffer real-buffer-index)
	  (funcall io-node ':fill-array-lispm-format buffer ptr status byte-size t))
    (or real-buffer (setq real-buffer-index 0))
    (and (not (zerop num-bytes))
	 (values (or real-buffer ind-buffer)
		 real-buffer-index
		 (+ real-buffer-index num-bytes)))))

;;;??? Should be in another file?
(defmethod (node :lispm-format-pointer) (pointer factor)
  (if (zerop factor)
      (values (* 2 pointer) 0 pointer)
    (values (truncate pointer factor) 0 (* factor (truncate pointer factor)))))

(defflavor fc-lispm-character-input-stream ()
	   (fc-lispm-input-stream si:buffered-line-input-stream))

(defflavor fc-lispm-output-stream ((buffer-bytes-used 0))
	(fc-lispm-stream-mixin fc-output-stream))

(defmethod (fc-lispm-output-stream :new-output-buffer) ()
  (values ind-buffer buffer-bytes-used (array-length ind-buffer)))

(defmethod (fc-lispm-output-stream :send-output-buffer) (array end)
  array
  (let (lastbyte)
    (setq buffer-bytes-used (\ end (max factor 1)))
    (cond (( 0 buffer-bytes-used)
	   (setq lastbyte (aref buffer (truncate end factor)))))
    ;; Output all but the last, incomplete, word.
    (setf (values ptr status)
	  (funcall io-node ':output-array-lispm-format
		   buffer ptr status
		   si:output-pointer-base
		   byte-size
		   (if (zerop factor)
		       (* end 2)
		       (truncate end factor))))
    (cond (lastbyte
	   (setf (aref buffer 0) lastbyte)
	   ;; Output whatever we have of the last byte;
	   ;; but since it is not final, do not advance past it
	   ;; in the node, and leave the data in the buffer.
	   (funcall io-node ':output-array-lispm-format
		    buffer ptr status 0 byte-size 1)))))

(defflavor fc-lispm-character-output-stream ()
	   (fc-lispm-output-stream si:line-output-stream-mixin))

;pdp10 character data format

(defflavor fc-pdp10-stream-mixin () ()
  (:included-flavors fc-data-stream))

(defmethod (fc-pdp10-stream-mixin :pdp10-format) () t)

(defmethod (fc-pdp10-stream-mixin :set-buffer-pointer) (pointer &aux tem)
  (setf (values ptr status tem)
	(funcall io-node ':pdp10-format-pointer pointer))
  tem)

(defflavor fc-pdp10-input-stream
	()
	(fc-pdp10-stream-mixin fc-input-stream))

(defmethod (fc-pdp10-stream-mixin :next-input-buffer)
	   (no-hang-p &aux real-buffer real-buffer-index real-buffer-nbytes)
  no-hang-p
  (setf (array-leader buffer 0) 0)
  (setf (values ptr status real-buffer real-buffer-index real-buffer-nbytes)
	(funcall io-node ':fill-array-pdp10-format buffer ptr status t))
  (and (not (zerop (or real-buffer-nbytes (array-active-length buffer))))
       (if real-buffer
	   (values real-buffer real-buffer-index (+ real-buffer-index real-buffer-nbytes))
	 (values buffer 0 (array-active-length buffer)))))

(defmethod (node :pdp10-format-pointer) (pointer)
  (values pointer 0 pointer))

(defflavor fc-pdp10-output-stream ((buffer-bytes-used 0))
	   (fc-pdp10-stream-mixin fc-output-stream))

(defmethod (fc-pdp10-output-stream :new-output-buffer) ()
  (values buffer buffer-bytes-used (array-length buffer)))

(defmethod (fc-pdp10-output-stream :send-output-buffer) (array end)
  array
  (let (tem e0 (e1 0) (e2 0) (e3 0) (e4 0))
    (setq buffer-bytes-used (\ end 5))
    (cond (( 0 buffer-bytes-used)
	   (setq tem (* (truncate end 5) 5))
	   (setq e0 (aref buffer tem))
	   (and (> buffer-bytes-used 1) (setq e1 (aref buffer (+ tem 1))))
	   (and (> buffer-bytes-used 2) (setq e2 (aref buffer (+ tem 2))))
	   (and (> buffer-bytes-used 3) (setq e3 (aref buffer (+ tem 3))))))
    ;; Output all but the last, incomplete, word.
    (setf (values ptr status)
	  (funcall io-node ':output-array-pdp10-format
		   buffer ptr status (truncate end 5)))
    (cond (e0
	   (setf (aref buffer 0) e0)
	   (setf (aref buffer 1) e1)
	   (setf (aref buffer 2) e2)
	   (setf (aref buffer 3) e3)
	   (setf (aref buffer 4) e4)
	   ;; Output whatever we have of the last word;
	   ;; but since it is not final, do not advance past it
	   ;; in the node, and leave the data in the buffer.
	   (funcall io-node ':output-array-pdp10-format
		    buffer ptr status 1)))))

;pdp10 image byte data format

(defflavor fc-pdp10-byte-stream-mixin (ind-buffer) ()
  (:included-flavors fc-data-stream))

(defmethod (fc-pdp10-byte-stream-mixin :after :init) (&rest ignore)
  (funcall-self ':set-byte-size byte-size))

(defmethod (fc-pdp10-byte-stream-mixin :after :set-byte-size) (ignore)
  (setq ind-buffer
	(if (< 4 byte-size 9)
	    buffer
	    (make-array (* (truncate (array-length buffer) 4)
			   (truncate 32. (lsh 1 (haulong (1- byte-size)))))
			':leader-length 1
			':type (or (nth (1- byte-size)
					'(art-1b art-2b art-4b art-4b))
				   'art-16b)
			':displaced-to buffer))))

(defmethod (fc-pdp10-byte-stream-mixin :set-buffer-pointer) (pointer &aux tem)
  (setf (values ptr status tem)
	(funcall io-node ':pdp10-byte-format-pointer pointer byte-size))
  tem)

(defmethod (fc-pdp10-byte-stream-mixin :pdp10-format) () nil)

(defflavor fc-pdp10-byte-input-stream
	()
	(fc-input-stream fc-pdp10-byte-stream-mixin))

(defmethod (fc-pdp10-byte-stream-mixin :next-input-buffer) (no-hang-p)
  no-hang-p
  (setf (array-leader ind-buffer 0) 0)
  (setf (values ptr status)
	(funcall io-node ':fill-array-byte-format ind-buffer ptr status byte-size))
  (and (not (zerop (array-active-length ind-buffer)))
       (values ind-buffer 0
	       (array-active-length ind-buffer))))

(defflavor fc-pdp10-byte-output-stream ()
	   (fc-output-stream fc-pdp10-byte-stream-mixin))

(defmethod (fc-pdp10-byte-output-stream :new-output-buffer) ()
  (values ind-buffer 0 (array-length ind-buffer)))

(defmethod (fc-pdp10-byte-output-stream :send-output-buffer) (array end)
  ;; Output all but the last, incomplete, word.
  (setf (values ptr status)
	(funcall io-node ':output-array-byte-format
		 array ptr status
		 end
		 byte-size)))

(compile-flavor-methods local-file-pathname fc-probe-stream
			fc-lispm-input-stream fc-lispm-character-input-stream
			fc-lispm-output-stream fc-lispm-character-output-stream
			fc-pdp10-input-stream fc-pdp10-output-stream
			fc-pdp10-byte-input-stream fc-pdp10-byte-output-stream)

;Given some parameters, create a stream for accessing this node's data.
(defmethod (dir-node :make-stream)
	   (pathname creating
	    &key &optional (characters t) (direction ':input)
	    ((:byte-size spc-byte-size))
	    viewspec (auto-next-node t)
	    &allow-other-keys)
  (and (eq characters ':default)
       (setq characters (funcall-self ':characters)))
  (and (eq spc-byte-size ':default)
       (setq spc-byte-size byte-size))
  (do () ((memq direction '(:input :output ())))
    (setq direction (cerror ':new-value nil 'inconsistent-options
			    "The direction ~2G~S is not defined."
			    (pathlist-into-pathname (send self ':standard-pathlist))
			    ':open direction)))
  (if (eq direction ':output)
      (ferror 'invalid-operation-for-directory
	      "Directories cannot be opened for output."
	      (pathlist-into-pathname (send self ':standard-pathlist)) ':open))
  (let ((stream
	  (lexpr-funcall
	    'make-instance
	    (selectq direction
	      (:INPUT (if characters
			  'fc-lispm-viewspec-character-input-stream
			'fc-lispm-viewspec-input-stream))
	      (t 'fc-probe-stream))
	    ':byte-size (or spc-byte-size
			    (if characters 8 16.))
	    ':must-explicitly-close creating
	    ':node node-closure ':characters characters
	    ':pathname pathname
	    (and (eq direction ':input)
		 (list ':viewspec viewspec
		       ':auto-next-node auto-next-node)))))
    (and (eq direction ':input)
	 (funcall-self ':add-reason stream))
    stream))

(defmethod (dir-node :fill-array-lispm-format) (&rest ignore) nil)

(defflavor fc-lispm-viewspec-input-stream
	(supernode
	 (remaining-node-entries t) viewspec
	 auto-next-node)
	(fc-lispm-input-stream)
  (:initable-instance-variables viewspec auto-next-node))

(defmethod (fc-lispm-viewspec-input-stream :next-input-buffer) (&optional no-hang-p)
  no-hang-p
  (setf (array-leader buffer 0) 0)
  (setf (values ptr status)
	(funcall io-node ':fill-array-lispm-format buffer ptr status byte-size))
  (if (not (zerop (array-active-length buffer)))
      (values ind-buffer 0
	      (if (zerop factor)
		  (truncate (array-active-length buffer) 2)
		(* (array-active-length buffer) factor)))
    (and auto-next-node
	 (funcall-self ':next-node)
	 (funcall-self ':next-input-buffer))))

(defmethod (fc-lispm-viewspec-input-stream :next-node) ()
  (setq ptr 0 status 0)
  (do (newnode) (())
    (cond ((eq remaining-node-entries t)
	   (setq remaining-node-entries nil)
	   (setq newnode node))
	  ((car remaining-node-entries)
	   (setq newnode
		 (funcall supernode
			  'open-subnode-entry
			  self
			  (pop (car remaining-node-entries)))))
	  (remaining-node-entries
	   (pop remaining-node-entries)
	   (cond ((neq io-node supernode)
		  (funcall supernode ':add-reason self)
		  (funcall io-node ':remove-reason self)
		  (setq io-node supernode)))
	   (setq supernode
		 (if remaining-node-entries
		     (funcall io-node ':supernode)
		   nil)))
	  (t (return nil)))
    (cond (newnode
	   (or (eq io-node node) (funcall io-node ':remove-reason self))
	   (setq io-node newnode)
	   (let ((subnodes (funcall newnode ':directory-entries
				    viewspec (length (and (neq remaining-node-entries t)
							  remaining-node-entries)))))
	     (if (null subnodes)
		 (return (let ((pathlist (funcall io-node ':standard-pathlist)))
			   (make-pathname-internal (funcall si:pathname ':host)
						   nil (or (butlast pathlist) '(root))
						   (last pathlist)
						   nil nil)))
	       (push subnodes remaining-node-entries)
	       (setq supernode newnode)))))))

(defmethod (fc-lispm-viewspec-input-stream :rewind) ()
  (or (eq io-node node)
      (funcall io-node ':remove-reason self))
  (setq io-node node)
  (setq remaining-node-entries t))

(defflavor fc-lispm-viewspec-character-input-stream ()
	   (fc-lispm-viewspec-input-stream si:buffered-line-input-stream))

(compile-flavor-methods fc-lispm-viewspec-input-stream
			fc-lispm-viewspec-character-input-stream)
