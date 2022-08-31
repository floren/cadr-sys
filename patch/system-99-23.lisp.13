;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Patch-File:T; Base:10; Readtable:ZL -*-
;;; Patch file for System version 99.23
;;; Reason:
;;;  Not all edclarations were getting though compiler's &KEY frobber
;;;  DEFDECL eval-when confusion
;;;  Zmacs diagram bugs
;;;  defsubstify hash-table functions
;;;  si:without-interrupts-hash-table flavor
;;;  more informative hash-table and hash-array printing
;;;  dired "s" command when given a prefix arg prompts for a wildcarded
;;;   subset of files of the subdirectory to include, rather than inserting
;;;   *.*.* which is still the default
;;;  Zmacs rodent-race bugs in mouse-indent-rigidly and read-function-name (khs)
;;; Written 25-Feb-85 23:39:06 by Mly,
;;; while running on Lisp Machine Nine from band 5
;;; with System 99.20, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, microcode 320, GC@2.


(export (intern "WITHOUT-INTERRUPTS-HASH-TABLE" 'si) 'si)

; From file OZ:OZ:<MLY.LL>QCP1.LISP.6 at 28-Feb-85 20:57:20
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	PSEUDO-KEYNAMES)
    (IF (EQ (CAR LAMBDA-EXP) 'LAMBDA)
	(SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP))
      (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP)))	;named-lambda
    (MULTIPLE-VALUE-SETQ (POSITIONAL-ARGS NIL AUXVARS
			  REST-ARG POSITIONAL-ARG-NAMES
			  KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
    (MULTIPLE-VALUE-BIND (NIL DECLS)
;>> need to pass environment into extract-declarations here
	(EXTRACT-DECLARATIONS BODY NIL NIL)
      ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
      ;; and check explicitly whether that has been overridden.
      ;; If the arg is optional
      ;; and the initial value is a constant, we can really init it to that.
      ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
      ;; after all keywords are decoded, we bind the intended variable, in sequence.
      ;; However a var that can shadow something (including any special var)
      ;; must always be replaced with a dummy.
      (DO ((KIS KEYINITS (CDR KIS))
	   (KNS KEYNAMES (CDR KNS))
	   (PKNS PSEUDO-KEYNAMES (CDR PKNS))
	   (KFS KEYFLAGS (CDR KFS)))
	  ((NULL KNS))
	(LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
	      (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	  (OR (AND (NULL KEYFLAG)
		   (CONSTANTP KEYINIT)
		   (NOT (ASSQ KEYNAME VARS))
		   (NOT (LEXICAL-VAR-P KEYNAME))
		   (NOT (SPECIALP KEYNAME)))
	      (PROGN (SETF (CAR KIS) 'SI::KEYWORD-GARBAGE)
		     (SETQ PSEUDO-KEYNAME (GENSYM))
		     (SETF (CAR PKNS) PSEUDO-KEYNAME)
		     (PUSH `(,KEYNAME
			     (COND ((EQ ,PSEUDO-KEYNAME SI::KEYWORD-GARBAGE)
				    ,KEYINIT)
				   (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
				      ,PSEUDO-KEYNAME)))
			   KEYCHECKS)))))
      (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
      (SETQ KEYCHECKS (NREVERSE KEYCHECKS))
  
      ;; If the user didn't ask for a rest arg, make one for the
      ;; outer function anyway.
      (OR REST-ARG (SETQ REST-ARG (GENSYM)
			 MAYBE-REST-ARG (LIST '&REST REST-ARG)))
      `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
	 (DECLARE . ,DECLS)
	 (LET* (,@(MAPCAR #'(LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
		,@KEYFLAGS)
	   (DECLARE . ,DECLS)
	   (WHEN ,REST-ARG
	     (SI::STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
					   ,REST-ARG ',KEYKEYS
					   ,ALLOW-OTHER-KEYS
					   ;; kludgey-compilation-variable-location is just
					   ;; variable-location except that it doesn't the
					   ;; increment the var-use-count of its arg
					   (KLUDGEY-COMPILATION-VARIABLE-LOCATION
					     ,(CAR PSEUDO-KEYNAMES))))
	   (LET* ,KEYCHECKS
	     (DECLARE . ,DECLS)
	     ((LAMBDA ,AUXVARS
		(DECLARE . ,DECLS)
		. ,BODY))))))))

))

; From file OZ:KANSAS:<L.SYS2>LMMAC.LISP.387 at 28-Feb-85 21:05:25
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFMACRO DEFDECL (NAME PROP VALUE)
  "Declare that the PROP property of NAME is VALUE, for GETDECL.
When executed, this makes a property, like DEFPROP.
In file compilation, this makes a declaration, so that GETDECL
done in macros being expanded will see this property."
  `(PROGN
     (EVAL-WHEN (EVAL LOAD)
       (PUTPROP ',NAME ',VALUE ',PROP))
     (EVAL-WHEN (COMPILE)
       (PUTDECL ',NAME ',PROP ',VALUE))))

))

; From file OZ:OZ:<MLY.LL>QFNS.LISP.6 at 28-Feb-85 21:17:04
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(defun declared-definitions ()
  "Cons up an alist of (<function-spec> . <definition>) for all frobs defined in either
LOCAL-DECLARATIONS or FILE-LOCAL-DELCARATIONS."
  (flet ((frob (list result)
	   (dolist (frob list)
	     (and (eq (car frob) 'def)
		  (not (assoc-equal (cadr frob) result))
		  (push (cons (cadr frob) (cddr frob)) result)))
	   result))
    (frob file-local-declarations (frob local-declarations nil))))

))

; From file OZ:KANSAS:<L.ZWEI>FILES.LISP.196 at 28-Feb-85 22:00:35
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; FILES  "


(DEFUN READ-DIRECTORY-NAME (PROMPT PATHNAME &KEY (DEFAULT-NAME :WILD)
						 (DEFAULT-TYPE :WILD)
						 (DEFAULT-VERSION :WILD))
  "Read a pathname to pass to FS:DIRECTORY-LIST.
Prompt with PROMPT, a string probably ending in a colon.
PATHNAME gives the defaults for host, device, directory.
WILDP gives the default used for the other components;
 normally :WILD, but could be NIL."
  (WITH-STACK-LIST (DEFS :DEFAULT-NAME DEFAULT-NAME
		         :DEFAULT-TYPE DEFAULT-TYPE
			 :DEFAULT-VERSION DEFAULT-VERSION)
    (LET ((TYPEIN (APPLY #'READ-UNDEFAULTED-DIRECTORY-STRING PROMPT PATHNAME DEFS)))
      (COND ((EQUAL TYPEIN "")
	     (PUSH-ON-HISTORY PATHNAME *PATHNAME-ARGUMENT-HISTORY*)
	     PATHNAME)
	    (T
	     (SETQ PATHNAME (APPLY #'FS:MERGE-PATHNAME-COMPONENTS TYPEIN PATHNAME DEFS))
	     (PUSH-ON-HISTORY PATHNAME *PATHNAME-ARGUMENT-HISTORY*)
	     PATHNAME)))))


(DEFUN READ-UNDEFAULTED-DIRECTORY-STRING (PROMPT PATHNAME &KEY (DEFAULT-NAME :WILD)
							       (DEFAULT-TYPE :WILD)
							       (DEFAULT-VERSION :WILD))
  "Read a string specifying a pathname to pass to FS:DIRECTORY-LIST.
Prompt with PROMPT, a string probably ending in a colon.
The defaults against which pathname -completion- is made are given by
 the host, device and directory of PATHNAME, with the
 DEFAULT-xxx supplying the other default components.
These defaults are used only for completion -- the returned string
 has not been defaulted against them. For that, use READ-DIRECTORY-NAME"
  (SETQ PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME DEFAULT-NAME
					      :TYPE DEFAULT-TYPE
					      :VERSION DEFAULT-VERSION))
  (SETQ PROMPT (FORMAT NIL "~A (Default is ~A)" PROMPT PATHNAME))
  (LET ((*READING-PATHNAME-DEFAULTS* PATHNAME)
	(*READING-PATHNAME-SPECIAL-TYPE* :WILD)
	(*READING-PATHNAME-SPECIAL-VERSION* :WILD)
	(*READING-PATHNAME-DIRECTION* :READ)
	(*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
    ;; no longer pushes result on *pathname-argument-history*
    ;;  -- that is now caller's responsibility
    (WITH-STACK-LIST (PROMPT PROMPT '(:RIGHT-FLUSH " (Completion)"))
      (STRING-INTERVAL (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB* NIL NIL 
							 PROMPT))))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.307 at 28-Feb-85 23:40:26
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN CLEAR-LINE-PATHNAME (LINE OPERATION)
  (MUNG-LINE LINE)
  (SETF (CHAR LINE 1) OPERATION)
  (SETF (LINE-LENGTH LINE) 5)
  (STRING-NCONC LINE
		(IF (BUFFER-MODIFIED-P (EDIT-BUFFERS-LINE-BUFFER LINE))
		    " * " "   ")
		(BUFFER-NAME (EDIT-BUFFERS-LINE-BUFFER LINE)))
  (REMF (LINE-PLIST LINE) ':PATHNAME))

))

; From file OZ:OZ:<MLY.LL>FONT.LISP.3 at 1-Mar-85 05:09:59
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "OZ:OZ:<MLY.LL>FONT.."

(DEFUN INSERT-DIAGRAM (BP DIAGRAM &REST OPTIONS)
  (AND (SYMBOLP DIAGRAM)
       (SETQ DIAGRAM (APPLY #'MAKE-INSTANCE DIAGRAM OPTIONS)))
  (DO ((I 0 (1+ I))
       (AT-LINE (BP-LINE BP))
       (NUMBER-OF-LINES (SEND DIAGRAM :NUMBER-OF-LINES))
       (LINE))
      (( I NUMBER-OF-LINES) DIAGRAM)
    (MULTIPLE-VALUE (DIAGRAM LINE)
      (APPLY #'MAKE-DIAGRAM-LINE DIAGRAM OPTIONS))
    (INSERT-LINE-WITH-LEADER LINE AT-LINE)))

(DEFUN MAKE-DIAGRAM-LINE (DIAGRAM &REST OPTIONS &AUX LINE)
  (AND (SYMBOLP DIAGRAM)
       (SETQ DIAGRAM (APPLY #'MAKE-INSTANCE DIAGRAM OPTIONS)))
  (SETQ LINE (CREATE-LINE ART-STRING 0 NIL))
  (SETF (GETF (LINE-PLIST LINE) ':DIAGRAM) DIAGRAM)
  (SEND DIAGRAM :ADD-LINE LINE)
  (VALUES LINE DIAGRAM))

))

; From file OZ:KANSAS:<L.SYS2>HASHFL.LISP.31 at 2-Mar-85 04:03:47
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; HASHFL  "

(defsubst gethash (key hash-table &optional default-value)
  "Read the values associated with KEY in HASH-TABLE.
Returns:
 1) The primary value associated with KEY (DEFAULT-VALUE if KEY is not found),
 2) a flag which is T if KEY was found,
 3) a pointer to the list (inside the hash table)
    which holds the key and the associated values
    (NIL if KEY is not found)."
  (declare (values value key-found-flag entry-pointer))
  (send hash-table :get-hash key default-value))

(defsubst sethash (key hash-table value)
  (send hash-table :put-hash key value))

(defsubst puthash (key value hash-table &rest additional-values)
  "Set the values associated with KEY in HASH-TABLE.
The first value is set from VALUE.  If the hash table associates more
than one value with each key, the remaining values are set from ADDITIONAL-VALUES.
Returns: 1) VALUE, 2) the previous value (or NIL),
 3) T if KEY already had an entry in the table,
 4) a pointer to the list (inside the hash table)
    which holds the key and the associated values."
  (declare (values value old-value key-found-flag entry-pointer))
  (lexpr-send hash-table :put-hash key value additional-values))

(defsubst remhash (key hash-table)
  "Delete any entry for KEY in HASH-TABLE.  Return T if there was one."
  (send hash-table :rem-hash key))

(defsubst maphash (function hash-table &rest extra-args)
  "Apply FUNCTION to each item in HASH-TABLE; ignore values.
FUNCTION's arguments are the key followed by the values associated with it,
 followed by the EXTRA-ARGS.
Returns NIL"
  (lexpr-send hash-table :map-hash function extra-args)
  nil)

(defsubst maphash-return (function hash-table &optional (return-function 'list))
  "Apply FUNCTION to each item in HASH-TABLE; apply RETURN-FUNCTION to list of results.
FUNCTION's arguments are the key followed by the values associated with it.
The values returned by FUNCTION are put in a list.
At the end, RETURN-FUNCTION is applied to that list to get the final value."
  (lexpr-send hash-table :map-hash-return function return-function))

(defsubst hash-table-count (hash-table)
  "Returns the number of associations currently stored in HASH-TABLE."
  (send hash-table :filled-entries))

(defsubst clrhash (hash-table &optional ignore)
  "Clear out a hash table; leave it with no entries. Returns HASH-TABLE"
  (send hash-table :clear-hash))

))

; From file OZ:KANSAS:<L.SYS2>HASHFL.LISP.31 at 2-Mar-85 07:03:28
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; HASHFL  "

(defflavor without-interrupts-hash-table ()
	   (hash-table)
  (:documentation "Hashing operations on this hash table happen uninterruptably")
  )

(defwrapper (without-interrupts-hash-table :get-hash) (ignore . body)
  `(without-interrupts
     . ,body))
(defwrapper (without-interrupts-hash-table :put-hash) (ignore . body)
  `(without-interrupts
     . ,body))
(defwrapper (without-interrupts-hash-table :clear-hash) (ignore . body)
  `(without-interrupts
     . ,body))

(compile-flavor-methods without-interrupts-hash-table)

))

; From file OZ:KANSAS:<L.SYS2>HASHFL.LISP.31 at 2-Mar-85 07:04:32
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; HASHFL  "

(defun make-hash-table (&rest options)
  "Create a hash table.  Keyword args are as follows:
COMPARE-FUNCTION: the function for comparing the key against
 keys stored in the table.  Usually EQ or EQUAL.
HASH-FUNCTION: the function to compute a hash code from a key.
 NIL (the default) is for EQ hash tables,
 and SI:EQUAL-HASH is used for EQUAL hash tables.
TEST: Common Lisp way to specify the compare-function.
 It must be EQ, EQL (the default) or EQUAL.
 A suitable hash function will be used automatically.
AREA: area to cons the table in.
SIZE: lower bound for number of entries (this may be rounded up).
 Note that the table cannot actually hold that many keys; this value merely serves
 as an approximation of the expected number of keys.
ACTUAL-SIZE: precise number of entries worth of size to use.
NUMBER-OF-VALUES: number of values to associate with each key (default 1).
 Each PUTHASH can set all the values, and GETHASH retrieves them all.
 Note that SETF of GETHASH can only set one value.
REHASH-FUNCTION: a function which accepts a hash table
 and returns a larger one.
REHASH-THRESHOLD: determines what /"fullness/" will make a growth of the hashtable
 and corresponding rehash necessary.
 Either a flonum between 0 and 1 (default 0.7), meaning that rehash occurs
 if more than that fraction full, or a fixnum, meaning rehash if more than that
 number of slots are filled. If a fixnum, it is automatically proportionally
 increased when the hashtable grows.
REHASH-SIZE: the ratio by which the default REHASH-FUNCTION
 will increase the size of the table.  By default, 1.3.
 This may also be a fixnum, in which case it determines the number of extra slots
 which are added to the hastable's size when it grows."
  (declare (arglist &key (test #'eql) compare-function hash-function
		         area size actual-size number-of-values
			 rehash-threshold rehash-function rehash-size))
  (apply #'make-instance 'hash-table options))

))

; From file OZ:KANSAS:<L.SYS2>HASH.LISP.88 at 2-Mar-85 07:15:58
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; HASH  "

(defsubst hash-table-maximal-fullness (hash-table)
  (values (floor (* (if (floatp (hash-table-rehash-threshold hash-table))
			(hash-table-rehash-threshold hash-table)
		      ;; never a fixnum, as make-hash-array coerces fixnums to floats,
		      ;;  so non-float presumably means the value is NIL
		      0.7s0)
		    (- (hash-table-modulus hash-table) 2)))))

))

; From file OZ:KANSAS:<L.SYS2>HASH.LISP.88 at 2-Mar-85 07:32:44
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; HASH  "

(defselect ((:property hash-array named-structure-invoke))
  (:fasload-fixup (self)
    ;; Force rehash as if due to gc, because hash codes are all wrong now.
    ;; Also fix up cdr codes.
    (setf (hash-table-gc-generation-number self) -1)
    (do ((i 0 (+ i blen))
	 (blen (hash-table-block-length self))
	 (length (array-length self)))
	(( i length))
      (%p-store-cdr-code (locf (cli:aref self (+ i blen -1))) cdr-nil)))
  (:describe (self)
    (describe-defstruct self)
    (format t "~&   Maximum fullness before rehash: ~S" (hash-table-maximal-fullness self)))
  (:print-self (self stream &optional ignore ignore)
    (printing-random-object (self stream :type)
      (print-hash-array self stream nil))))

(defun print-hash-array (hash-array stream for-print-hashtable-p)
  (format stream "~S~:[+~S~;~*~]//~S~@[ :test ~S~]~@[~A~]"
	  (hash-table-fullness hash-array)
	  (zerop (hash-table-number-of-deleted-entries hash-array))
	  (hash-table-number-of-deleted-entries hash-array)
	  (hash-table-maximal-fullness hash-array)
	  (let ((compare-function (hash-table-compare-function hash-array)))
	    (if (or for-print-hashtable-p
		    (and (neq compare-function 'eq) (neq compare-function #'eq)))
		(function-name compare-function)))
	  (and (not for-print-hashtable-p)
	       (hash-table-funcallable-p hash-array)
	       " (Funcallable)")))

))

; From file OZ:KANSAS:<L.SYS2>HASHFL.LISP.31 at 2-Mar-85 07:59:46
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; HASHFL  "

(defmethod (hash-table :print-self) (stream &rest ignore)
  (printing-random-object (self stream :type)
    (print-hash-array hash-array stream t)))

))

; From file OZ:OZ:<MLY.LL>DIRED.LISP.1 at 2-Mar-85 08:09:48
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(defcom com-dired-subdirectory "Insert or remove the files of this subdirectory.
The files in the subdirectory mentioned on this line
are inserted into the DIRED buffer underneath this line.
You can then delete them, rename them, etc.
The subdirectory files are indented one additional space.
If the subdirectory contents are already present in the DIRED buffer,
this command offers to remove them from the buffer.
Removing them from the buffer does not delete the files!
It only makes DIRED stop operating on them.

With an argument, prompts for a wildcarded specification of files in the
directory to insert, rather than inserting them all, which is the default. " ()
  (let* ((line (bp-line (point)))
	 (wild-pathname (getf (line-plist line) 'contents-present))
	 (pathname (getf (line-plist line) ':pathname))
	 directory wild-directory)
    (unless (getf (line-plist line) ':directory)
      (barf "~A is not a directory" pathname))
    (setq directory (send pathname :pathname-as-directory)
	  wild-directory (send directory :new-pathname :name :wild :type :wild :version :wild))
    (cond ;; no arg and presently there => close subdir
	  ((and wild-pathname
		(not *numeric-arg-p*)
		(fquery () "Remove subfiles ~A ?" wild-pathname))
	   (dired-close-line-subdirectory line))
	  ;; arg means selective insert of subdir, deleting old contents if present
	  (*numeric-arg-p*
	   (or wild-pathname (setq wild-pathname wild-directory))
	   (setq wild-pathname (read-directory-name
				 (format nil "Edit which subfiles of directory ~A"
					 wild-directory)
				 wild-pathname))
	   (unless (send wild-directory :pathname-match wild-pathname)
	     (format *query-io* "~&~A does not seem to specify any subfile of ~A"
		     wild-pathname wild-directory)
	     (beep)
	     (if (y-or-n-p "Start a separate dired of ~A ?")
		 (directory-edit wild-pathname t)
	       (return-from com-dired-subdirectory dis-none)))	       
	   (if (getf (line-plist line) 'contents-present)
	       (dired-close-line-subdirectory line))
	   (dired-open-line-subdirectory line wild-pathname))
	  ;; no arg and not there => insert *.*.* subdir
	  (t
	   (dired-open-line-subdirectory line wild-directory))))
  dis-text)

(DEFUN DIRED-OPEN-SUBDIRECTORY (PATHNAME)
  "Add the files in the subdirectory PATHNAME to the dired buffer.
Does nothing if that subdirectory is not itself present.
PATHNAME should be the pathname of the file which is the subdirectory."
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (END-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))) WILD-PATHNAME)
      ((EQ LINE END-LINE) NIL)
    (WHEN (EQ PATHNAME (DIRED-LINE-PATHNAME LINE))
      (UNLESS (SETQ WILD-PATHNAME (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
	(DIRED-OPEN-LINE-SUBDIRECTORY LINE WILD-PATHNAME))
      (RETURN T))))

(DEFUN DIRED-OPEN-LINE-SUBDIRECTORY (LINE WILD-PATHNAME &AUX DIRECTORY)
  (IF (SETQ DIRECTORY (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT))
      (BARF "Subfiles ~A are already present" DIRECTORY)
    (UNLESS (PATHNAMEP WILD-PATHNAME)
      (SETQ WILD-PATHNAME (SEND (SEND (DIRED-LINE-PATHNAME-OR-BARF LINE)
				      :PATHNAME-AS-DIRECTORY)
				:NEW-PATHNAME :NAME :WILD :TYPE :WILD :VERSION :WILD)))
    (SETQ DIRECTORY (FS:DIRECTORY-LIST WILD-PATHNAME :DELETED :SORTED))
    (LET* ((*BATCH-UNDO-SAVE* T))
      (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
	(SETF (GETF (LINE-PLIST LINE) 'CONTENTS-PRESENT) WILD-PATHNAME)
	(LET ((NEXT-PLIST (LINE-PLIST (LINE-NEXT LINE)))
	      (STREAM (INTERVAL-STREAM-INTO-BP (CREATE-BP (LINE-NEXT LINE) 0))))
	  (DIRED-INSERT-DIRECTORY DIRECTORY STREAM
				  (1+ (DIRED-LINE-LEVEL LINE)))
	  ;; Restore the plist, now clobbered, of the following line.
	  (SETF (LINE-PLIST (BP-LINE (SEND STREAM :READ-BP))) NEXT-PLIST))))))

(defun dired-close-line-subdirectory (line)
  (let* ((*batch-undo-save* t)
	 (wild-pathname (getf (line-plist line) 'contents-present)))
    (if (null wild-pathname)
	(barf "No subfiles are present")
      (with-read-only-suppressed (*interval*)
	(setf (getf (line-plist line) 'contents-present) nil)
	(do ((line2 (line-next line) (line-next line2))
	     (thislevel (dired-line-level line)))
	    ((let ((linelevel (dired-line-level line2)))
	       (or (null linelevel)
		   ( linelevel thislevel)))
	     (delete-interval (create-bp (line-next line) 0)
			      (create-bp line2 0)
			      t)))))))

))

; From file OZ:OZ:<MLY.LL>DIRED.LISP.1 at 2-Mar-85 08:20:10
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFVAR *DIRED-SUBDIRECTORY-INDENTATION* 2
  "The number of spaces inserted in front of the files of a subdirectory in dired.")

(DEFCOM COM-DIRED-HELP "Explain use of DIRED commands" ()
  (FORMAT T "You are in the directory editor.  The commands are:
	D (or K, c-D, c-K)  Mark the current file for deletion.
	P	Print the current file on the standard hardcopy device.
	A	Queue this file for function application.
	U	Undelete the current file, or else the file just above the cursor.
		Also used to cancel a Print or Apply function request.
	R	Rename this file.  You type the new filename in a mini buffer.
	C	Copy this file.  You type the new filename in a mini buffer.
	L	Load this file (lisp code or QFASL file).
	Rubout	Undelete file above the cursor.
	Space	Move to the next line.
	  Above commands repeat with a numeric argument,
	  backwards if the argument is negative.	  
	S	Insert the contents of this Subdirectory.
	        The files in the subdirectory are indented ~R one additional space~:P.
		By default it inserts all the files of the subdirectory; however
		 by giving this command a numeric argument you will be prompted
		 for a wildcarded pathname specifying a subset of the subdirectory's
		 contents.
		If the subdirectory files are already inserted, the S with no
		 argument command offers to remove them from the display.
		Removing them from the display does NOT delete the files!
	N	Move to the next file with more than ~S versions.
		 (This number /"~D/" is the value of ~S)
	H	Mark excess versions of the current file for deletion.
	Q	Exit.  You will be shown the files to be deleted and asked for
		confirmation.  In this display /":/" means a link, /">/" means
		this is the highest version-number of this file, /"!/" means
		not backed-up, and /"$/" means not to be reaped, please.
	X	Execute.  Perform requested file deletions, etc.,
		but stay in the DIRED buffer afterwards.
	!	Move to the next file that is not backed up on tape.
	@	Complement @ flag (dont-delete)
	#	Complement # flag (dont-supersede)
	$	Complement $ flag (dont-reap)
	,	Print the attributes of a file.  For a source file, the -*- line.
		For a QFASL file, the compilation data and what is recorded
		 of the source file's -*- line.
	.	Change properties of current file.
	E	Edit the current file, or DIRED on subdirectory.
	F	Edit current file or subdirectory, not now, but when you exit.
	C-Sh-E	Edit the current file in another window.  The DIRED remains visible.
		 Enters two window mode if you are in one window mode.
        <       DIRED on the superior directory of this directory.
	V	View the current file (doesn't read it all in).
	=	SRCCOM this file with the > version.

Clicking the right-hand button on the mouse will give you a menu
of commands to operate on the line the mouse is pointing at.

Sorting commands which sort the DIRED buffer:

M-X Sort Increasing File Name
M-X Sort Increasing Creation Date
M-X Sort Increasing Reference Date
M-X Sort Increasing Size
and their counterparts with Decreasing instead of Increasing.
"
	  *DIRED-SUBDIRECTORY-INDENTATION*
	  *FILE-VERSIONS-KEPT* '*FILE-VERSIONS-KEPT*
	  )
  DIS-NONE)

))

;;; lmi 102.38 khs

; From file OZ:KANSAS:<L.ZWEI>MOUSE.LISP.97 at 2-Mar-85 08:49:56
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFCOM COM-MOUSE-INDENT-RIGIDLY "Track indentation with the mouse.
If there is a region, moves the whole region, else the current line.  Continues until the
mouse is released." (KM)
  (LET ((POINT (POINT))
        (SHEET (WINDOW-SHEET *WINDOW*))
        (START-LINE)
        (END-LINE))
    (COND ((WINDOW-MARK-P *WINDOW*)		;If there is a region, use it
           (REGION (BP1 BP2)
		   (SETQ START-LINE (BP-LINE BP1)
			 END-LINE (BP-LINE BP2))
		   (OR (ZEROP (BP-INDEX BP2))
		       (SETQ END-LINE (LINE-NEXT END-LINE)))))
          (T
	   (SETQ START-LINE (BP-LINE POINT)
		 END-LINE (LINE-NEXT START-LINE))))
    (MULTIPLE-VALUE-BIND (X Y)
        (FIND-BP-IN-WINDOW-COORDS (FORWARD-OVER *BLANKS* (BEG-OF-LINE START-LINE)) *WINDOW*)
    (SEND SHEET :SET-MOUSE-CURSORPOS X Y))
    (PROCESS-WAIT "MOUSE" #'(LAMBDA () (OR (ZEROP TV:MOUSE-LAST-BUTTONS) *MOUSE-P*)))
    (DO ((LAST-X)
	 (LAST-Y)
	 (BP (COPY-BP POINT))
         (DELTA))
	(NIL)
      (MULTIPLE-VALUE (LAST-X LAST-Y) (MOUSE-POSITION))
      (SETQ DELTA (LINE-INDENTATION START-LINE SHEET))
      (MOVE-BP BP START-LINE 0)
      (INDENT-LINE BP (MAX 0 LAST-X) SHEET)
      (SETQ DELTA (- (LINE-INDENTATION START-LINE SHEET) DELTA))
      (OR (= DELTA 0)
          (DO ((LINE START-LINE (LINE-NEXT LINE)))
              ((EQ LINE END-LINE))
            (AND (NEQ LINE START-LINE)
                 (INDENT-LINE (MOVE-BP BP LINE 0)
                              (MAX 0 (+ DELTA (LINE-INDENTATION LINE SHEET))) SHEET))))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT)
      (REDISPLAY *WINDOW* ':POINT)
      (WAIT-FOR-MOUSE LAST-X LAST-Y 8.)
      (WHEN (ZEROP (TV:MOUSE-BUTTONS)) (RETURN))))
  DIS-TEXT)

))

;;; lmi 102.27 khs

; From file OZ:KANSAS:<L.ZWEI>MOUSE.LISP.97 at 2-Mar-85 08:55:16
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; MOUSE  "

(DEFUN READ-FUNCTION-NAME (PROMPT &OPTIONAL DEFAULT MUST-BE-DEFINED STRINGP
				  &AUX EXPLICIT-PACKAGE-P
				  (*MINI-BUFFER-DEFAULT-STRING* DEFAULT)
				  (READ-FUNCTION-NAME-MUST-BE-DEFINED MUST-BE-DEFINED)
				  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
				    *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*)
				  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
				    *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
				    *MOUSE-FONT-CHAR*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
				    *MOUSE-X-OFFSET*)
				  (READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET
				    *MOUSE-Y-OFFSET*))
  "Read a function name using mini buffer or mouse.
PROMPT is a string that goes in the mode line.
DEFAULT is a function spec to return if the user types just Return.
MUST-BE-DEFINED can be T (allow only defined functions), NIL (allow anything)
 or AARRAY-OK (allow anything either defined as a function
 or known as a section by the editor).
STRINGP can be T, NIL, ALWAYS-READ or MULTIPLE-OK.
 T means if user types text, just return a string; don't try to intern it.
 ALWAYS-READ means intern the user's string afresh now;
  don't use the symbol or list recorded in the completion aarray.
 MULTIPLE-OK means it is ok to return more than one possible function
  the user could have meant, if they differ only in their package.

The first value is a list of function specs (only one, unless STRINGP is MULTIPLE-OK).
 If STRINGP is T, this is NIL.
The second value is the string the user typed, sans package prefix.
The third value is T if the user typed a package prefix."
  (DECLARE (VALUES COMPLETIONS STRING EXPLICIT-PACKAGE-P))
  (DECLARE (SPECIAL READ-FUNCTION-NAME-MUST-BE-DEFINED
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
		    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
		    READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
		    READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
		    READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET))
  (AND (EQ MUST-BE-DEFINED T) (SETQ STRINGP 'ALWAYS-READ))
  (SETQ PROMPT (FORMAT NIL "~A~:[:~; (Default: ~S)~]" PROMPT DEFAULT DEFAULT))
  (LET ((NAME
	  (LET ((*POST-COMMAND-HOOK*
		  (APPEND *POST-COMMAND-HOOK* '(READ-FUNCTION-NAME-COMMAND-HOOK)))
		(*MINI-BUFFER-VALUE-HISTORY*
		  *DEFINITION-NAME-HISTORY*))
	    (LET ((*BATCH-UNDO-SAVE* T))
	      (DELETE-INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
	    (UNWIND-PROTECT
		(PROGN (READ-FUNCTION-NAME-COMMAND-HOOK NIL)
		       (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *ZMACS-COMPLETION-AARRAY*
							 (OR (NEQ STRINGP 'ALWAYS-READ)
							     'ALWAYS-STRING)))
	      (READ-FUNCTION-NAME-COMMAND-HOOK T))))
	SYM ERRORP)
    (COND ((EQUAL NAME "")
	   (OR DEFAULT (BARF))
	   (SETQ SYM DEFAULT
		 NAME (IF (SYMBOLP NAME) (SYMBOL-NAME NAME) (PRINC-TO-STRING DEFAULT))))
;	  ((CONSP NAME)
;	   (SETQ SYM (CDR NAME)
;		 NAME (CAR NAME))
;	   (AND (CONSP SYM) (NEQ STRINGP 'MULTIPLE-OK)
;		(SETQ SYM (CAR SYM))))
	  ((EQ STRINGP T)			;If returning a string, don't intern it
	   (SETQ SYM NAME))
	  (T
	   ;; If the string that was specified started with a package prefix,
	   ;; return a flag saying so.
	   ;; SYMBOL-FROM-STRING will flush the prefix from NAME.
	   ;; ** KLUDGE!! **
	   (LET ((NON-LETTER-INDEX
		   (STRING-SEARCH-NOT-SET " ABCDEFGHIJKLMNOPQRSTUVWXYZ-" NAME)))
	     (AND NON-LETTER-INDEX (= (CHAR NAME NON-LETTER-INDEX) #/:)
		  (SETQ EXPLICIT-PACKAGE-P T)))
	   (MULTIPLE-VALUE-SETQ (SYM NAME ERRORP)
	     (SYMBOL-FROM-STRING NAME NIL T))
;	   (AND MULTIPLEP (EQ STRINGP 'MULTIPLE-OK)
;		(SETQ SYM (NCONS SYM)))
	   (IF ERRORP (BARF "Read error"))))
    (AND (EQ MUST-BE-DEFINED T)
	 (NOT (OR (FDEFINEDP SYM)
		  (AND (SYMBOLP SYM)
		       (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST SYM)))))	;ucode entry
	 (OR (DOLIST (SPEC (PACKAGE-LOOKALIKE-SYMBOLS SYM))
	       (AND (FQUERY ()
			    ;; Always print prefix
			    ;; Don't leave *PACKAGE* in keyword during query.
			    (LET ((*PACKAGE* SI:PKG-USER-PACKAGE))
			      (FORMAT NIL "Do you mean ~S? " SPEC)))
		    (RETURN (SETQ SYM SPEC))))
	     (BARF "~S is not defined" SYM)))
    (PUSH-ON-HISTORY SYM *DEFINITION-NAME-HISTORY*)
    (VALUES SYM NAME EXPLICIT-PACKAGE-P)))

))

; From file OZ:OZ:<MLY.LL>FLAVOR.LISP.10 at 2-Mar-85 10:17:15
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; FLAVOR  "

(DEFMACRO MACROCALL (&ENVIRONMENT ENV &REST X)
  (LET ((MACRO (COND ((DECLARED-DEFINITION (CAR X)))
		     ((FDEFINEDP (CAR X))
		      (FDEFINITION (CAR X)))
		     (T (FERROR NIL "Unable to find definition of wrapper ~S at expand time"
				(CAR X))))))
    (COND ((EQ (CAR-SAFE MACRO) 'MACRO)
	   (CALL (CDR MACRO) NIL X :OPTIONAL ENV))
	  ;;--- Temporary code so I can test things in the kludge environment
	  ((AND (SYMBOLP MACRO) (EQ (CAR-SAFE (SYMBOL-FUNCTION MACRO)) 'MACRO))
	   (CALL (CDR (SYMBOL-FUNCTION MACRO)) NIL X :OPTIONAL ENV))
	  (T (FERROR NIL "~S evaluated to ~S, which is not a macro" (CAR X) MACRO)))))

))


; From file OZ:OZ:<MLY.LL>QFNS.LISP.7 at 6-Mar-85 02:50:46
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; QFCTNS  "

(DEFUN DEFF-MACRO (&QUOTE FUNCTION &EVAL DEFINITION)
  "Define FUNCTION with definition DEFINITION, which should be a subst or macro.
If found in a file being compiled, this definition will be in effect
during compilation as well as when the compiled file is loaded.
That is how DEFF-MACRO differs from DEFF."
  (AND UNDO-DECLARATIONS-FLAG
       (COMPILER::FUNCTION-REFERENCED-P FUNCTION)
       (COMPILER::COMPILER-WARN 'MACRO-USED-BEFORE-DEFINED :IMPOSSIBLE
				"The macro ~S was used before it was defined" FUNCTION))
  ;; Put macro definition where it belongs (don't really define it if compiling)
  (COND ((AND (BOUNDP 'UNDO-DECLARATIONS-FLAG) UNDO-DECLARATIONS-FLAG)
	 (WHEN (EQ (CAR-SAFE FUNCTION) ':PROPERTY)
	   (PUTDECL (CADR FUNCTION) (CADDR FUNCTION) DEFINITION))
	 (PUSH `(DEF ,FUNCTION . ,DEFINITION) FILE-LOCAL-DECLARATIONS))
	(T
	 (FDEFINE FUNCTION DEFINITION T)
;	 (IF (SYMBOLP DEFINITION)
;	     (DEFMACRO-COPY-INDENTATION-FOR-ZWEI FUNCTION DEFINITION))
	 ))
  FUNCTION)

))
