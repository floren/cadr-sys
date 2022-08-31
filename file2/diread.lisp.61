;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Lowercase:T -*-

;Reading and storing directories.

;When a directory is opened, the READ-DIR operation is used
;to turn the data on disk into list structure, which becomes
;the value of the DIRECTORY-LIST variable.

;When we FINISH a directory node, the STORE-DIR operation is done
;to write the data in that list structure onto the disk.

;When a file date or flag is updated, call
;DIR-UPDATE-ENTRY to store the changed data into the disk
;without copying the whole dir again, as STORE-DIR would do.

;Those three are the only entry points in this file, and the only way
;directories interface with the actual data on disk.
;However, each flavor of directory node may need its own
;version of the READ-DIR and STORE-DIR  operations.

;;;In an old format directory,
;The data starts with a word giving (redundantly) the length of the data,
;followed by any number of name entries,
;followed by a zero to mark the end.  The length includes the
;words containing the length and the zero, but not the file-node-offset.

;A name entry starts with a word containing the name length in the top 8 bits
;and the installed version number in the bottom 24.
;Then come any number of version entries, followed by a word containing 0.

;A version entry starts with a word containing the version (and 1 in high 8 bits),
;followed by any number of entries, followed by a word containing 0.

;;;A new format directory is simpler.
;It starts with a word containing -1 to distinguish from the old format.
;Then comes a word containing the length of the data (which includes these two words)
;Then come any number of file-entries, followed by a word containing a zero.
;The length includes that word containing zero.

;Each file entry starts with a word whose high 16 bits contain the name length in bytes
;and whose low 16 bits contain the extra flags.
;The extra flags are:
;    1 - always set, so the word cannot be 0.
;    2 - this file is "installed".
;Then comes the name.
;Then comes a word containing the version number in its low 24 bits.
;Then comes an entry.  (A "file-entry" is not the same thing as an "entry").


;Read in the data of a name-and-version-dir-node and return it.
(defmethod (name-and-version-dir-node read-dir) ()
  (let (file-ptr-access-array file-ptr-access-index
	(file-ptr-extend-file nil)
	file-ptr-next-word
	(file-ptr-elements-following 0))
    (prog1
      ;; Decide on the format, get the redundant data length,
      ;; and read the data.
      (cond ((= 77777777 (file-fetch-word file-node-offset))
	     (setf (file-contents-length node-file)
		   (file-fetch-word (1+ file-node-offset)))
	     (setq file-ptr-next-word (+ 2 file-node-offset))
	     (read-new-dir-format))
	    (t
	     (setf (file-contents-length node-file) (file-fetch-word file-node-offset))
	     (setq file-ptr-next-word (1+ file-node-offset))
	     (read-old-dir-format)))
      ;; Check that we ended at the right place.
      (let ((directory-data-end (+ (file-contents-length) (file-contents-offset))))
	(or (= file-ptr-next-word directory-data-end)
	    (and (= directory-data-end file-node-offset))	;Empty dir, just created.
	    (ferror nil "Lossage: directory data doesn't end at supposed length"))))))

(defun read-new-dir-format ()
  (declare-flavor-instance-variables (name-and-version-dir-node)
    (do (namelength extra-flag-bits new-dir-list)
	((zerop (multiple-value (extra-flag-bits namelength) (file-ptr-fetch-two-halves)))
	 (dolist (name-entry new-dir-list)
	   (setf (cddr name-entry) (sortcar (cddr name-entry) 'greaterp)))
	 (nreverse new-dir-list))
      (let (name version name-entry version-entry)
	(setq name (file-ptr-extract-string namelength))
	(or (setq name-entry (ass 'string-equal name new-dir-list))
	    (push (setq name-entry (list name nil))
		  new-dir-list))
	(setq version (file-ptr-fetch-word))
	(or (setq version-entry (assq version (cddr name-entry)))
	    (push (setq version-entry (cons version nil))
		  (cddr name-entry)))
	(push (dir-read-entry `(:version (:name ,name) ,version)
			      extra-flag-bits)
	      (cdr version-entry))))))

(defun read-old-dir-format ()
  (do (name-entries-list)
      ((zerop (file-ptr-fetch-word))
       (nreverse name-entries-list))
    (file-ptr-reprocess-word)
    (push (dir-read-name-entry)
	  name-entries-list)))

;Read and return one name-entry (see "old directory format" above).
(defun dir-read-name-entry ()
  (multiple-value-bind (installed-version-number name-length)
		       (file-ptr-fetch-word)
    (and (= installed-version-number 77777777)
	 (setq installed-version-number nil))
    (let ((name (file-ptr-extract-string name-length))
	  versions-list)
      (do (version tem)
	  ((progn (multiple-value (version tem) (file-ptr-fetch-word))
		  (zerop (logior tem version))))
	(push (dir-read-version-entry name version) versions-list))
      (list* name installed-version-number (nreverse versions-list)))))

;Read and return one version-entry.
(defun dir-read-version-entry (name version-number)
  (let (entries-list)
    (do ()
	((zerop (file-ptr-fetch-word)))
      (file-ptr-reprocess-word)
      (push (dir-read-entry `(:version (:name ,name) ,version-number)) entries-list))
    (list* version-number (nreverse entries-list))))

;Read all the data associated with a directory entry and return it.
;Both the old and new directory formats use one "directory entry" per file,
;but the new format also supplies the "extra flags" which are stored
;separately from the directory entry proper.
;The storage format is:
;Word 0 -- creation date.
;Word 1 -- the reference date.
;Word 2 -- top 8 bits -- flavor number.
;         low 24 bits -- length of plist data.
;Word 3 -- top 8 bits -- byte size
;	   next 8 bits -- flags
;	   low 16 bits -- length of pointer info data.
;Word 4 -- subnode contents length in bytes of specified size.
;Word 5 -- start of pointer info data
;  after that comes the plist data.
(defun dir-read-entry (full-pathstep &optional (extra-flags 0))
 (declare-flavor-instance-variables (dir-node)
  extra-flags
  (let ((entry (make-dir-entry))
	flavor-code plist-length flags ptr-info-length -byte-size- tem)
    (setf (dir-entry-dir-index entry) (- file-ptr-next-word file-node-offset))
    (setf (dir-entry-full-pathstep entry) full-pathstep)
    (setf (dir-entry-creation-date entry) (file-ptr-fetch-date))
    (setf (dir-entry-reference-date entry) (file-ptr-fetch-date))
    (multiple-value (plist-length flavor-code)
      (file-ptr-fetch-word))
    (setf (dir-entry-subnode-flavor entry)
	  (or (nth flavor-code node-flavor-list)
	      (ferror nil "Lossage: subnode flavor code ~D out of range" flavor-code)))
    (multiple-value (tem -byte-size-)
      (file-ptr-fetch-word))
    (or ( -byte-size- 16.)
	(ferror nil "Lossage: byte size ~D in directory too large" -byte-size-))
    (setf (dir-entry-byte-size entry) -byte-size-)
    (setq flags (ldb 2010 tem) ptr-info-length (ldb 20 tem))
    (and (bit-test 1 flags) (setf (dir-entry-deleted entry) t))
    (and (bit-test 2 flags) (push ':delete-protect (dir-entry-flags entry)))
    (and (bit-test 4 flags) (push ':characters (dir-entry-flags entry)))
    (and (bit-test 10 flags) (push ':supersede-protect (dir-entry-flags entry)))
    (and (bit-test 20 flags) (push ':dumped (dir-entry-flags entry)))
    (and (bit-test 40 flags) (push ':installed (dir-entry-flags entry)))
    (setf (dir-entry-length entry) (file-ptr-fetch-bignum))
    (setf (dir-entry-pointer-info entry)
	  (dir-fetch-plist ptr-info-length))
    (setf (dir-entry-plist entry)
	  (dir-fetch-plist plist-length))
    entry)))

;Alter one item in a directory entry in place.
;Takes the dir-entry data structure as argument
;and stores into the directory file certain fixed-size pieces of data,
;using the DIR-ENTRY-DIR-INDEX to figure out where to store them.

;The data we store back are the two file dates, the deleted bit, the flags,
;the byte size, and the length in bytes.

;If the entry has never been output, we add it to the end.
(defmethod (name-and-version-dir-node update-entry) (entry &optional dont-finish-file)
  (cond (directory-changed (funcall-self ':finish))
	((null (dir-entry-dir-index entry))
	 (funcall-self 'store-additional-entry entry)
	 (or dont-finish-file (file-write-out)))
	((eq (dir-entry-changed entry) ':full)
	 (cond ((eq entry dir-last-added-entry)
		(funcall-self 'flush-last-entry entry)
		(funcall-self 'store-additional-entry entry))
	       (t (funcall-self 'store-dir))))
	((dir-entry-changed entry)
	 (file-store-date (+ file-node-offset (dir-entry-dir-index entry))
			  (dir-entry-creation-date entry))
	 (file-store-date (+ 1 file-node-offset (dir-entry-dir-index entry))
			  (dir-entry-reference-date entry))
	 (let ((tem (file-fetch-word (+ 3 file-node-offset (dir-entry-dir-index entry)))))
	   ;; Discard old flags.
	   (setq tem (+ (ldb 0020 tem)
			;; Add back in the new flags.
			(if (dir-entry-deleted entry) 200000 0)
			(if (memq ':delete-protect (dir-entry-flags entry)) 400000 0)
			(if (memq ':characters (dir-entry-flags entry)) 1000000 0)
			(if (memq ':supersede-protect (dir-entry-flags entry)) 2000000 0)
			(if (memq ':dumped (dir-entry-flags entry)) 4000000 0)
			(if (memq ':installed (dir-entry-flags entry)) 10000000 0)))
	   (file-store-word (+ 3 file-node-offset (dir-entry-dir-index entry))
			    tem (dir-entry-byte-size entry))
	   (file-store-bignum (+ 4 file-node-offset (dir-entry-dir-index entry))
			      (dir-entry-length entry))
	   ;; If update-in-place of plist is requested, do it.
	   (cond ((eq (dir-entry-changed entry) ':plist)
		  (let* (file-ptr-access-array file-ptr-access-index
			 (file-ptr-extend-file nil)
			 (file-ptr-next-word (+ 5 file-node-offset
						(dir-entry-dir-index entry)
						(ldb 0020 tem)))
			 (file-ptr-elements-following 0)
			 (expected-plist-end
			   (+ file-ptr-next-word
			      (file-fetch-word (+ 2 file-node-offset
						  (dir-entry-dir-index entry))))))
		    (dir-store-plist (dir-entry-plist entry))
		    (or (= file-ptr-next-word expected-plist-end)
			(progn 
			  (ferror nil "Plist wrong length; directory data clobbered!
Continue to re-store entire directory")
			  (funcall-self 'store-dir)))))))
	 (setf (dir-entry-changed entry) nil)
	 (or dont-finish-file (file-write-out)))))

(defmethod (dir-node plist-current-length) (entry)
  (if (dir-entry-dir-index entry)
      (file-fetch-word (+ 2 (dir-entry-dir-index entry) file-node-offset))
    -1))

(declare (special file-ptr-access-array file-ptr-access-index
		  file-ptr-next-word file-ptr-elements-following))

;;; Store an entire directory.

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

(defun dir-finish-store (dir-writing-start)
 (declare-flavor-instance-variables (dir-node)
  ;; Store the zero word that marks the end of the dir.
  (file-ptr-store-word 0 0)
  ;; Store the length of what we wrote both on disk and in the file-object.
  (file-set-contents-length (- file-ptr-next-word file-node-offset))
  ;; Now mark all the records we wrote in as modified.
  (records-modified (truncate dir-writing-start (file-record-size))
		    (ceiling file-ptr-next-word
			     (file-record-size)))
  (file-store-word (1+ file-node-offset) (- file-ptr-next-word file-node-offset) 0)))

;Always nonzero to indicate not end of directory!
(defun dir-entry-encode-extra-flags (entry) entry 1)

;Store an individual directory entry, starting with its name and version,
;down the current file pointer.
(defun dir-store-entry (entry)
  (declare-flavor-instance-variables (dir-node)
    (file-ptr-store-two-halves
      (dir-entry-encode-extra-flags entry)
      (string-length (cadadr (dir-entry-full-pathstep))))
    (file-ptr-store-string (cadadr (dir-entry-full-pathstep)))
    (file-ptr-store-word (caddr (dir-entry-full-pathstep)) 1)	;Don't let entire word be 0.
    (let (plist-length ptr-info-length)
      (setf (dir-entry-dir-index entry) (- file-ptr-next-word file-node-offset))
      (file-ptr-store-date (dir-entry-creation-date entry))
      (file-ptr-store-date (dir-entry-reference-date entry))
      (file-ptr-store-word (setq plist-length
				 (plist-length-if-stored (dir-entry-plist entry)))
			   (find-position-in-list (dir-entry-subnode-flavor entry)
						  node-flavor-list))
      (file-ptr-store-word (+ (if (dir-entry-deleted entry) 200000 0)
			      (if (memq ':delete-protect (dir-entry-flags entry)) 400000 0)
			      (if (memq ':characters (dir-entry-flags entry)) 1000000 0)
			      (if (memq ':supersede-protect (dir-entry-flags entry)) 2000000 0)
			      (if (memq ':dumped (dir-entry-flags entry)) 4000000 0)
			      (if (memq ':installed (dir-entry-flags entry)) 10000000 0)
			      (setq ptr-info-length
				    (plist-length-if-stored (dir-entry-pointer-info entry))))
			   (dir-entry-byte-size entry))
      (file-ptr-store-bignum (dir-entry-length entry))
      (dir-store-plist (dir-entry-pointer-info entry))
      (dir-store-plist (dir-entry-plist entry))
      ;; Remember that this entry has been stored properly.
      (setf (dir-entry-changed entry) nil))))

;;; Store the redundant dir info in a node.

(defmethod (node store-dir-info) (ignore) nil)

(defmethod (file-node store-dir-info) (file)
  (funcall supernode 'store-subnode-dir-entry supernode-info file))

(defmethod (name-and-version-dir-node store-subnode-dir-entry) (entry node-file)
  (setf (file-redundant-dir-info)
	(+ (if (dir-entry-deleted entry) 200000 0)
	   (if (memq ':delete-protect (dir-entry-flags entry)) 400000 0)
	   (if (memq ':characters (dir-entry-flags entry)) 1000000 0)
	   (if (memq ':supersede-protect (dir-entry-flags entry)) 2000000 0)
	   (if (memq ':dumped (dir-entry-flags entry)) 4000000 0)
	   (dir-entry-byte-size entry)
	   (lsh (find-position-in-list (dir-entry-subnode-flavor entry)
				       node-flavor-list)
		8))))

(defun extract-redundant-dir-flags (redundant-dir-info)
  (declare (return-list deleted other-flag-list))
  (let (flags)
    (and (bit-test 400000 redundant-dir-info) (push ':delete-protect flags))
    (and (bit-test 1000000 redundant-dir-info) (push ':characters-protect flags))
    (and (bit-test 2000000 redundant-dir-info) (push ':supersede-protect flags))
    (and (bit-test 4000000 redundant-dir-info) (push ':dumped flags))
    (values (bit-test 200000 redundant-dir-info) flags)))

;Reading and storing plists (and pointer-info, which is stored the same way).

;A plist is a succession of properties, stretching as long as
;the recorded plist length.

;The first word of a property has four fields.
;The low 20 bits are the length of the property's value as stored, in quarterwords.
;The next 10 bits are the length of the property's name storage, in quarterwords.
;  If the name is encoded, this length is zero as the name is not stored.
;The next two bits and the sign bit, made into a three-bit field,
;  are the value type.  This says how to interpret the value storage.
;  Type codes are
;	0 - value is 32-bit number.
;	1 - value is a string, stored straightforwardly.
;	2 - value is the symbol fs:node.  This is obsolete.
;	3 - value is 32-bit date (a number, but with an offset
;		so the range of values is different from type 0
;		and includes the range of common dates).
;	4 - value is stored as a printed representation.
;		Extract in the string of the recorded length
;		and READ-FROM-STRING it.
;	5 - value is NIL.
;	6 - value is T.
;The remaining 5 bits, in between the value type bits,
;  are the name code.  If this is nonzero, it stands for a particular
;  property name in node-propname-code alist, and the name itself
;  is not stored in the property (and the name length is 0).

;Return the propname code, the propname length, the propval type, the propval length.
(defun file-ptr-prop-decode ()
  (file-ptr-prepare-next-word)
  (let ((highhalf (aref file-ptr-access-array (1+ file-ptr-access-index))))
    (values
      (ldb 1205 highhalf)
      (ldb 0010 highhalf)
      (dpb (ldb 1701 highhalf)
	   0201
	   (ldb 1002 highhalf))
      (aref file-ptr-access-array file-ptr-access-index))))

;Store those four quantities.
(defun dir-store-prop-header (code name-length type val-length)
  (file-ptr-store-two-halves val-length
			     (dpb (lsh type -2) 1701
				  (dpb code 1206
				       (dpb type 1002 name-length)))))

;Cons up a Lisp plist corresponding to the plist stored in a directory entry.
(defun dir-fetch-plist (length)
  (do ((plist-end (+ length file-ptr-next-word))
       plist)
      ((= file-ptr-next-word plist-end) plist)
    (multiple-value-bind (code namelength type valuelength)
	(file-ptr-prop-decode)
      (let ((name (dir-intern-propname code namelength))
	    (value (dir-extract-propval type valuelength)))
	(push value plist)
	(push name plist)))))

;Create a symbol to represent the property name in the property at word pp.
(defun dir-intern-propname (code length)
  (cond ((zerop code)
	 (let ((str (file-ptr-extract-string length)))
	   (or (car (mem 'string-equal
			 str node-frequent-property-names))
	       (intern str si:pkg-user-package))))
	(t (car (rassq code node-propname-code-alist)))))

;Create a Lisp object to represent the value of the property at word pp.
(defun dir-extract-propval (valtype length)
  (cond ((= valtype 0) (file-ptr-fetch-bignum))
	((= valtype 1) (file-ptr-extract-string length))
	((= valtype 2) 'node)
	((= valtype 3)
	 (file-ptr-fetch-date))
	((= valtype 4)
	 (read-from-string (file-ptr-extract-string length)))
	((= valtype 5) nil)
	((= valtype 6) t)))

;Save a Lisp plist as the plist of the directory entry at ep.
;Returns the address of the word after the end of the stored plist data.
(defun dir-store-plist (plist)
  (do ((plist plist (cddr plist)))
      ((null plist))
    (dir-store-property (car plist) (cadr plist))))

;Store one property at location pp in the node.
(defun dir-store-property (propname propval)
  (let ((code (cdr (assq propname node-propname-code-alist)))
	(name-length 0)
	(val-length 0)
	val-printed
	type)
    (cond ((null code)
	   (setq name-length (string-length propname))
	   (setq code 0)))
    (cond ((eq propval t)
	   (setq type 6))
	  ((eq propval nil)
	   (setq type 5))
	  ((eq propval 'node)
	   (setq type 2))
	  ((and (numberp propval) (fixp propval)
		(< file-number-min propval file-date-offset))
	   (setf type 0)
	   (setf val-length 4))
	  ((and (numberp propval) (fixp propval)
		(< file-date-offset propval file-date-max))
	   (setf type 3)
	   (setq val-length 4))
	  ((stringp propval)
	   (setf type 1)
	   (setf val-length (string-length propval)))
	  (t (setq type 4)
	     (setq val-printed
		   (with-output-to-string (foo)
		     (prin1 propval foo)))
	     (setq val-length (string-length val-printed))))
    (dir-store-prop-header code name-length type val-length)
    (if (zerop code) (file-ptr-store-string (string propname)))
    (selectq type
      (0 (file-ptr-store-bignum propval))
      (1 (file-ptr-store-string propval))
      (3 (file-ptr-store-date propval))
      (4 (file-ptr-store-string val-printed)))))

;Given a Lisp plist, compute how many words it would need
;if converted to directory plist format and stored in a dir.
;Checks for various errors such as names too long to store,
;and entire plist too long to store.
(defun plist-length-if-stored (plist)
  (do ((plist-rest plist (cddr plist-rest))
       (length 0))
      ((null plist-rest)
       (cond ((> length node-max-plist-length)
	      (ferror 'invalid-property-value "Plist too long to store in snode")))
       length)
    (setq length (+ length
		    1
		    (cond ((assq (car plist-rest) node-propname-code-alist) 0)
			  (t (cond ((> (string-length (car plist-rest))
				       node-max-propname-length)
				    (ferror 'invalid-property-name
					    "property name ~S too long to store"
					    nil
					    (car plist-rest))))
			     (ceiling (string-length (car plist-rest)) 4)))
		    (cond ((eq (cadr plist-rest) 'node) 0)
			  ((and (numberp (cadr plist-rest))
				(fixp (cadr plist-rest))
				(< file-number-min (cadr plist-rest) file-date-offset))
			   1)
			  ((and (numberp (cadr plist-rest))
				(fixp (cadr plist-rest))
				(< file-date-offset (cadr plist-rest) file-date-max))
			   1)
			  ((stringp (cadr plist-rest))
			   (ceiling (string-length (cadr plist-rest)) 4))
			  ((eq (cadr plist-rest) nil) 0)
			  ((eq (cadr plist-rest) t) 0)
			  (t (ceiling (flatsize (cadr plist-rest)) 4)))))))

;;; More incremental updating of directories.
;;; We can add new entries at the end, in the new format,
;;; and can also discard the last such new entry and add it over again.

(defmethod (name-and-version-dir-node store-additional-entry) (entry)
  ;; If it's an old-format dir, we cannot add individual entries,
  ;; so convert it to new format and then go ahead.
  (if (plusp (file-fetch-word (file-contents-offset)))
      (funcall-self 'store-dir))
  (let* (file-ptr-access-array file-ptr-access-index
	 (file-ptr-extend-file t)
	 ;; Get the directory size from the beginning and that says
	 ;; where to start writing the new entry.
	 (file-ptr-next-word (+ (file-fetch-word (1+ (file-contents-offset)))
				(file-contents-offset)
				-1))	;Take account of the zero at the end.
	 (dir-writing-start file-ptr-next-word)
	 (file-ptr-elements-following 0))
    (setq dir-last-added-entry-index (- file-ptr-next-word (file-contents-offset))
	  dir-last-added-entry entry)
    (atomic-update-records (truncate file-ptr-next-word (file-record-size))
			   (file-n-records))
    (atomic-update-word (1+ file-node-offset))
    (dir-store-entry entry)
    (dir-finish-store dir-writing-start)))

;Discard the last added entry from the dir.
(defmethod (name-and-version-dir-node flush-last-entry) (entry)
  (let* (file-ptr-access-array file-ptr-access-index
	 (file-ptr-extend-file nil)
	 ;; Get the directory size from the beginning and that says
	 ;; where to start writing the new entry.
	 (file-ptr-next-word (+ dir-last-added-entry-index
				(file-contents-offset)))
	 (file-ptr-elements-following 0))
    ;; Record this entry as "not present in the file on disk".
    (setq dir-last-added-entry nil)
    (setf (dir-entry-dir-index entry) nil)
    ;; Modify the file on disk not to contain this entry.
    (atomic-update-word file-ptr-next-word)
    (atomic-update-word (1+ file-node-offset))
    (dir-finish-store file-ptr-next-word)))
