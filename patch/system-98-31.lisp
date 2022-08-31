;;; -*- Mode:LISP; Package:USER; Patch-file:T; Base:8; Lowercase:T -*-
;;; Patch file for System version 98.31
;;; Reason: OPEN-CHAOS keyword options
;;; DRIBBLE snarfage of rh buffer
;;; codewalker binding lists
;;; EXPORT of strings
;;; LISP-TOP-LEVEL and BREAK handling of *DEFAULT-COMMON-LISP* change
;;; reader symbol substitutions print dwimily. GLOBAL:AREF gets (intern "AREF" "GLOBAL")
;;; Fed COPY-FONT
;;; DEFRESOURCE function-parent declarations
;;; PRINT-NAMED-STRUCTURE hacks named arrays which don't have DEFSTRUCT definitions
;;; vms parsing braindamage
;;; Written 18-Jan-84 07:31:27 by Mly,
;;; while running on Lisp Machine Eighteen from band 4
;;; with System 98.29, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, Experimental Local-File 48.0, Experimental LFS 3.0, microcode 306, ZM MIT.


; From file QFILE.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QFILE  "

(DEFUN OPEN-CHAOS (HOST PATHNAME &REST OPTIONS &KEY (DIRECTION ':INPUT) (CHARACTERS T)
		   (ERROR T) (ACCESS-ERROR (NOT ERROR))
		   (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
		   (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
					;; :UNSPECIFIC here is to prevent lossage
					;; writing ITS files with no version numbers.
					'(:NEWEST :UNSPECIFIC))
				  ':NEW-VERSION ':ERROR))
		   (IF-DOES-NOT-EXIST
		     (COND ((MEMQ DIRECTION '(:PROBE :PROBE-LINK :PROBE-DIRECTORY))
			    NIL)
			   ((AND (EQ DIRECTION ':OUTPUT)
				 (NOT (MEMQ IF-EXISTS '(:OVERWRITE :TRUNCATE :APPEND))))
			    ':CREATE)
			   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
			   ;; for compatibility with the past.
			   ;; A Common-Lisp program would use :PROBE
			   ;; and get NIL as the default for this.
			   (T ':ERROR)))
		   TEMPORARY DELETED RAW SUPER-IMAGE (BYTE-SIZE ':DEFAULT)
		   PRESERVE-DATES INHIBIT-LINKS
		   &AUX HOST-UNIT DATA-CONN PKT SUCCESS STRING NOT-ABORTED
		   PHONY-CHARACTERS SIGN-EXTEND-BYTES IF-EXISTS-P
		   (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SI:CCASE DIRECTION
    ((:INPUT :OUTPUT :PROBE-DIRECTORY :PROBE-LINK))
    (:IO (FERROR NIL "Bidirectional file streams are not supported."))
    ((NIL :PROBE) (SETQ DIRECTION NIL)))
  (CHECK-TYPE IF-EXISTS (MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
				:OVERWRITE :APPEND :TRUNCATE :SUPERSEDE NIL))
  (CHECK-TYPE IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
  ;; IF-EXISTS-P is T if we need to give the IF-EXISTS to the server.
  (SETQ IF-EXISTS-P
	(NOT (MEMQ IF-EXISTS
		   (SELECTQ (PATHNAME-VERSION PATHNAME)
		     (:NEWEST '(:NEW-VERSION))
		     (:UNSPECIFIC '(:NEW-VERSION :SUPERSEDE))))))
  (WHEN ELEMENT-TYPE-P
    (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
	  (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
  (FILE-OPERATION-RETRY
    (CONDITION-CASE-IF ACCESS-ERROR (ERROR-OBJECT)
        (PROGN
	  (IF (MEMQ DIRECTION '(NIL :PROBE-DIRECTORY :PROBE-LINK))
	      ;;PROBE mode implies no need for data connection
	      (SETQ HOST-UNIT (FUNCALL HOST ':GET-HOST-UNIT))
	    (MULTIPLE-VALUE (DATA-CONN HOST-UNIT)
	      (FUNCALL HOST ':GET-DATA-CONNECTION DIRECTION))))
      (REMOTE-NETWORK-ERROR ERROR-OBJECT)
      (:NO-ERROR
       (UNWIND-PROTECT
	 (PROGN
	   (MULTIPLE-VALUE (PKT SUCCESS STRING)
	     (IF (TYPEP SELF '(OR LMFILE-PARSING-MIXIN LM-PARSING-MIXIN))
		 (FUNCALL HOST-UNIT ':COMMAND NIL
			  (SELECTQ DIRECTION
			    (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			    (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			  NIL "OPEN-FOR-LISPM " #\RETURN
			  (FILE-PRINT-PATHNAME SELF) #\RETURN
			  (LET ((BASE 10.) (*NOPOINT T) (PACKAGE SI:PKG-USER-PACKAGE)
				(READTABLE SI:COMMON-LISP-READTABLE))
			    (AND (EQ DIRECTION ':OUTPUT) (NULL IF-EXISTS)
				 (SETQ OPTIONS (LIST* ':IF-EXISTS ':ERROR OPTIONS)))
			    (AND (NOT IF-EXISTS-P)
				 (GET-LOCATION-OR-NIL (LOCF OPTIONS) ':IF-EXISTS)
				 (PROGN
				   (SETQ OPTIONS (COPYLIST OPTIONS))
				   (REMPROP (LOCF OPTIONS) ':IF-EXISTS)))
			    (AND (NULL IF-DOES-NOT-EXIST)
				 (SETQ OPTIONS (LIST* ':IF-DOES-NOT-EXIST ':ERROR OPTIONS)))
			    (PRIN1-TO-STRING OPTIONS)))
	       (FUNCALL HOST-UNIT ':COMMAND NIL
			(SELECTQ DIRECTION
			  (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			  (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			NIL
			"OPEN " (SELECTQ DIRECTION
				  ((NIL) "PROBE")
				  (:PROBE-DIRECTORY "PROBE-DIRECTORY")
				  (:PROBE-LINK "PROBE INHIBIT-LINKS")
				  (:INPUT "READ")
				  (:OUTPUT "WRITE"))
			" " (SELECTQ CHARACTERS
			      ((NIL) "BINARY")
			      (:DEFAULT "DEFAULT")
			      (T "CHARACTER"))
			(IF (AND (EQ DIRECTION ':OUTPUT)
				 IF-EXISTS-P)
			    (STRING-APPEND " IF-EXISTS "
					   (IF (EQ IF-EXISTS NIL)
					       ':ERROR
					     IF-EXISTS))
			  "")
			(IF (OR IF-EXISTS-P
				(NEQ IF-DOES-NOT-EXIST
				     (SELECTQ DIRECTION
				       ((:INPUT NIL :PROBE-DIRECTORY :PROBE-LINK) ':ERROR)
				       (:OUTPUT ':CREATE))))
			    (STRING-APPEND " IF-DOES-NOT-EXIST "
					   (IF (EQ IF-DOES-NOT-EXIST NIL)
					       ':ERROR
					     IF-DOES-NOT-EXIST))
			  "")
			(IF INHIBIT-LINKS " INHIBIT-LINKS" "")
			(FORMAT NIL "~:[ BYTE-SIZE ~D~;~*~]~:[~; TEMPORARY~]~:[~; DELETED~]~
				~:[~; RAW~]~:[~; SUPER~]~:[~; PRESERVE-DATES~]~%~A~%"
				(EQ BYTE-SIZE ':DEFAULT) BYTE-SIZE
				TEMPORARY DELETED RAW SUPER-IMAGE PRESERVE-DATES
				(FILE-PRINT-PATHNAME SELF)))))
	   (COND ((NOT SUCCESS)
		  (SETQ NOT-ABORTED T)
		  (SETQ STRING (STRING-APPEND STRING))
		  (AND PKT (CHAOS:RETURN-PKT PKT))
		  (OR (NULL DATA-CONN)
		      (SETF (DATA-STREAM DATA-CONN DIRECTION) NIL))
		  (CONDITION-CASE-IF (NOT IF-DOES-NOT-EXIST)
				     ()
		      (CONDITION-CASE-IF (NOT IF-EXISTS)
					 ()
			  (FILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR) ':OPEN)
			(FILE-ALREADY-EXISTS NIL))
		    (FILE-NOT-FOUND NIL)))
		 (T
		  (LET ((PROPERTIES (READ-FILE-PROPERTY-LIST-STRING STRING "OPEN" PATHNAME)))
		    (CHAOS:RETURN-PKT PKT)
		    (AND (EQ CHARACTERS ':DEFAULT)
			 (SETQ CHARACTERS (GET (LOCF PROPERTIES) ':CHARACTERS)))
		    (UNLESS (OR (EQ BYTE-SIZE ':DEFAULT)
				(GET (LOCF PROPERTIES) ':BYTE-SIZE))
		      (SETF (GET (LOCF PROPERTIES) ':BYTE-SIZE) BYTE-SIZE))
		    (PROG1
		      (MAKE-INSTANCE (SELECTQ DIRECTION
				       (:INPUT
					(IF CHARACTERS
					    'FILE-INPUT-CHARACTER-STREAM
					  (COND (SIGN-EXTEND-BYTES
						 'FILE-INPUT-SIGNED-BINARY-STREAM)
						(PHONY-CHARACTERS
						 'FILE-INPUT-PHONY-CHARACTER-STREAM)
						(T
						 'FILE-INPUT-BINARY-STREAM))))
				       (:OUTPUT
					(IF CHARACTERS
					   'FILE-OUTPUT-CHARACTER-STREAM
					  (IF PHONY-CHARACTERS
					      'FILE-OUTPUT-PHONY-CHARACTER-STREAM
					    'FILE-OUTPUT-BINARY-STREAM)))
				       (T 'FILE-PROBE-STREAM))
				     ':HOST-UNIT HOST-UNIT
				     ':DATA-CONNECTION DATA-CONN
				     ':PROPERTY-LIST PROPERTIES
				     ':PATHNAME PATHNAME)
		      (SETQ NOT-ABORTED T))))))
	 (UNLESS (OR NOT-ABORTED
		     (NULL DATA-CONN)
		     (NULL (SEND HOST-UNIT ':CONTROL-CONNECTION)))
	   ;; Here if aborted out of it and server may have file open.
	   (CONDITION-CASE ()
	       (PROGN
		(AND (EQ DIRECTION ':OUTPUT)
		     (FUNCALL HOST-UNIT ':COMMAND NIL
			      (DATA-OUTPUT-HANDLE DATA-CONN) NIL "DELETE"))
		(MULTIPLE-VALUE-BIND (NIL CLOSE-SUCCESS)
		    (FUNCALL HOST-UNIT ':COMMAND
			     NIL
			     (SELECTQ DIRECTION
			       (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
			       (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))
			     NIL "CLOSE")
		  (WHEN CLOSE-SUCCESS
		    (SELECTQ DIRECTION
		      (:INPUT (READ-UNTIL-SYNCHRONOUS-MARK (DATA-CONNECTION DATA-CONN)))
		      (:OUTPUT (CHAOS:SEND-PKT (DATA-CONNECTION DATA-CONN)
					       (CHAOS:GET-PKT) %FILE-SYNCHRONOUS-MARK-OPCODE)))))
		(FUNCALL HOST-UNIT ':FREE-DATA-CONNECTION DATA-CONN DIRECTION))
	     (SYS:HOST-STOPPED-RESPONDING NIL))))))))

))

;; From file PATHNM.LISP PS:<L.FILE2> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

;(defun merge-name-type-and-version-1 (name type version)
;  (let (tem real-name
;	(default-cons-area pathname-area))
;    (setq real-name
;	  (copylist
;	    (cond ((eq name ':wild)
;		   '("*"))
;		  ((eq name ':unspecific)
;		   nil)
;		  (t name))))
;    (cond ((and type
;		(neq type ':unspecific))
;	   (setq tem type)
;	   (and (or (eq tem ':wild) (equal tem '("WILD")))
;		(setq tem "*"))
;	   (let ((last (car (last real-name))))
;	     (if (or (symbolp last) (null last))
;		 (setq real-name (append real-name `((:property ,tem))))
;	       (if (and (equal last "*") (equal tem "*"))
;		   nil
;		 (rplaca (last real-name)
;			 `(:subnode-property ,last ,tem)))))))
;    (and version
;	 (neq version ':unspecific)
;	 (not (atom real-name))
;	 (let ((tem1 (last real-name))
;	       real-version)
;	   (setq real-version
;		 (selectq version
;		   (:oldest ':<)
;		   (:newest ':>)
;		   (:wild ':*)
;		   (:installed ':!)
;		   (:unspecific nil)
;		   (t version)))
;	   (cond ((symbolp (car tem1)))
;		 ((and (consp (car tem1))
;		       (eq (caar tem1) ':property))
;		  (rplaca tem1 (list ':property (cadar tem1) real-version)))
;		 ((and (consp (car tem1))
;		       (eq (caar tem1) ':subnode-property))
;		  (rplaca tem1
;			  `(:subnode-property ,(cadar tem1) ,(caddar tem1) ,real-version)))
;		 (t (rplaca tem1 `(:version ,(car tem1) ,real-version))))))
;    real-name))

;))

;; From file PATHNM.LISP PS:<L.FILE2> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

;(defun decompose-name-type-and-version (pathlist)
;  (cond ((eq pathlist ':wild)
;	 (values ':wild ':wild ':wild))
;	(t
;	 (let ((name pathlist)
;	       type version)
;	   ;; Now correct for the fact that we have to store the type and version
;	   ;; in separate places.
;	   (let ((tem (last name)))
;	     (cond ((atom (car tem)))
;		   ((eq (caar tem) ':version)
;		    (setq version (caddar tem)
;			  name (nconc (butlast name) (list (cadar tem)))))
;		   ((eq (caar tem) ':subnode-property)
;		    (setq version (car (cdddar tem))
;			  type (caddar tem)
;			  name (nconc (butlast name) (list (cadar tem)))))
;		   ((eq (caar tem) ':property)
;		    (setq version (caddar tem)
;			  type (cadar tem)
;			  name (butlast name)))))
;	   (cond ((equal name '("*")) (setq name ':wild)))
;	   (cond ((string-equal type "*")
;		  (setq type ':wild)))
;	   (setq version (selectq version
;			   (:> ':newest)
;			   (:< ':oldest)
;			   (:! ':installed)
;			   (:* ':wild)
;			   (t version)))
;	   (values name type version)))))

;))

;; From file PATHNM.LISP PS:<L.FILE2> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

;(defmethod (lmfile-parsing-mixin :new-pathname)
;	   (&rest options &key &allow-other-keys
;	    &optional (starting-pathname self)
;	    (original-type nil original-type-p)
;	    &aux host-p device-p directory-p name-p type-p version-p
;	    new-host new-device new-directory new-name new-type canonical-type new-version
;	    new-name-really
;	    new-directory-really
;	    new-type-really
;	    new-version-really)
;  (loop for (keyword value) on options by 'cddr
;	do
;	(selectq keyword
;	  (:host (unless host-p (setq new-host value host-p t)))
;	  (:name (unless name-p (setq new-name value name-p t)))
;	  (:raw-name (unless name-p (setq new-name value name-p ':raw)))
;	  (:directory (unless directory-p (setq new-directory value directory-p t)))
;	  (:raw-directory (unless directory-p (setq new-directory value directory-p ':raw)))
;	  (:device (unless device-p (setq new-device value device-p t)))
;	  (:raw-device (unless device-p (setq new-device value device-p ':raw)))
;	  (:type (unless type-p
;		   (if (and (symbolp value) (not (memq value '(nil :unspecific))))
;		       (setq canonical-type value type-p ':canonical)
;		     (setq new-type value type-p t))))
;	  (:raw-type (unless type-p (setq new-type value type-p ':raw)))
;	  (:canonical-type
;	   (unless type-p (setq canonical-type value type-p ':canonical)))
;	  (:version (unless version-p (setq new-version value version-p t)))
;;; All keywords that do NOT require special decoding must go here.
;	  ((:starting-pathname
;	    :original-type :defaults nil)
;	   nil)
;	  (t (ferror nil "Unknown keyword ~s to MAKE-PATHNAME or :NEW-PATHNAME." keyword))))
;  (or host-p (setq new-host (pathname-host starting-pathname)))
;  (setq new-host (get-pathname-host new-host))
;  ;; Turn a specified canonical type into a string (in standard case).
;  (if (eq type-p ':canonical)
;      (multiple-value-bind (preferred all)
;	  (decode-canonical-type canonical-type (send new-host ':system-type))
;	(setq new-type
;	      (let ((alphabetic-case-affects-string-comparison t))
;		(unless original-type-p
;		  (setq original-type (pathname-type starting-pathname)))
;		(if (member original-type all)
;		    original-type
;		  preferred)))))
;  (if (eq (pathname-host starting-pathname) new-host)
;      (progn
;	(or device-p (setq new-device (pathname-raw-device starting-pathname) device-p ':raw))
;	(or directory-p (setq new-directory (pathname-raw-directory starting-pathname) directory-p ':raw))
;	(or name-p (setq new-name (pathname-raw-name starting-pathname) name-p ':raw))
;	(or type-p (setq new-type (pathname-raw-type starting-pathname) type-p ':raw)))
;    ;; Hosts don't match; must convert to standard syntax and reparse.
;    (or device-p (setq new-device (pathname-device starting-pathname)))
;    (or directory-p (setq new-directory (pathname-directory starting-pathname)))
;    (or name-p (setq new-name (pathname-name starting-pathname)))
;    (or type-p (setq new-type (pathname-type starting-pathname))))
;  (or version-p (setq new-version (pathname-raw-version starting-pathname)))
;  ;; Now compute the actual new components from what was specified.
;  (setq new-device (or new-device ':unspecific))
;  (and (stringp new-device)
;       (string-equal new-device "DSK")
;       (setq new-device ':unspecific))
;  (setq new-directory-really
;	(cond ((eq directory-p ':raw) new-directory)
;	      ((stringp new-directory) (expand-pathstring new-directory))
;	      ((eq new-directory ':root) '(root))
;	      ((memq new-directory '(nil :unspecific :wild))
;	       new-directory)
;	      ((consp new-directory) new-directory)
;	      (t (ferror 'pathname-parse-error
;			 "~S is not a valid directory component for ~A."
;			 new-directory host))))
;  (cond ((eq name-p ':raw)
;	 (setq new-name-really new-name))
;	((eq new-name ':wild)
;	 (setq new-name-really ':wild))
;	((stringp new-name)
;	 (setf (values nil nil new-name-really new-type-really new-version-really)
;	       (funcall-self ':parse-namestring nil new-name)))
;	(t
;	 (setq new-name-really new-name)))
;  (setq new-type-really
;	(cond ((eq type-p ':raw) new-type)
;	      ((and (null type-p) new-type-really)
;	       new-type-really)
;	      ((memq new-type '(nil :unspecific :wild))
;	       new-type)
;	      ((string-equal new-type '*) ':wild)
;	      ((mem 'string-equal new-type ignored-types)
;	       ':unspecific)
;	      (t (string new-type))))
;  (when (or (stringp new-version) (symbolp new-version))
;    (unless (null new-version)
;      (setq new-version (intern new-version si:pkg-keyword-package))))
;  (setq new-version-really
;	(selectq new-version
;	  (:> ':newest)
;	  (:< ':oldest)
;	  (:* ':wild)
;	  (:! ':installed)
;	  (t new-version)))
;  (make-pathname-internal new-host
;			  new-device new-directory-really new-name-really
;			  new-type-really new-version-really))

;))

;; From file PATHNM.LISP PS:<L.FILE2> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

;(defmethod (lmfile-parsing-mixin :valid-version-p) (vrs)
;  (or (fixnump vrs)
;      (memq vrs '(nil :unspecific :wild :newest :oldest :installed :> :< :! :*))))

;))

;; From file PATHNM.LISP PS:<L.FILE2> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

;(defun expand-pathstring (pathstring &optional (name-start-index 0))
;  (cond ((or (consp pathstring) (null pathstring)) pathstring)
;	(t
;	 (setq pathstring (string pathstring))
;	 (do (pathlist
;	       previous-step
;	       (len (string-length pathstring))
;	       nextname nextversion propflag
;	       after-root-or-supernode
;	       name-end-index last-nonspace)
;	     (( name-start-index len)
;	      pathlist)
;	   (setq nextname nil nextversion nil)
;	   (cond ((= (aref pathstring name-start-index) #/^)
;		  (setq previous-step 'supernode)
;		  (setq pathlist (nconc pathlist (list previous-step)))
;		  (setq after-root-or-supernode "^")
;		  (setq name-start-index (1+ name-start-index)))
;		 ((= (aref pathstring name-start-index) #/~)
;		  (setq previous-step 'root)
;		  (setq pathlist (nconc pathlist (list previous-step)))
;		  (setq after-root-or-supernode "~")
;		  (setq name-start-index (1+ name-start-index)))
;		 ((and (memq (aref pathstring name-start-index) '(#// #/\))
;		       after-root-or-supernode)
;		  (setq name-start-index (1+ name-start-index))
;		  (setq after-root-or-supernode nil))
;		 ((and (= (aref pathstring name-start-index) #/|)
;		       after-root-or-supernode)
;		  (setq after-root-or-supernode nil))
;		 ((= (aref pathstring name-start-index) #/ )
;		  (setq name-start-index (1+ name-start-index)))
;		 ((memq (aref pathstring name-start-index) '(#/ #/ #/; #/:))
;		  (return pathlist name-start-index))
;		 (t
;		   (and after-root-or-supernode
;			(ferror 'pathname-parse-error
;				"garbage following ~A in pathstring"
;				after-root-or-supernode))
;		   ;; Read a name of subnode or property.
;		   (setq propflag nil)
;		   (cond ((= (aref pathstring name-start-index) #/|)
;			  (setq name-start-index (1+ name-start-index))
;			  (setq propflag t)))
;		   (setq last-nonspace (1- name-start-index))
;		   ;; Find end of name.  Skip over quoted characters.
;		   (do ((i name-start-index (1+ i)))
;		       (( i len)
;			(setq name-end-index len))
;		     (let ((ch (aref pathstring i)))
;		       (cond ((= ch #/)
;			      (setq i (1+ i))
;			      (setq last-nonspace i)
;			      (cond ((= i len)
;				     (pathname-error (1- i)
;					     "Pathstring ends with quote character: ~A"
;					     pathstring))))
;			     ((memq ch pathstring-special-chars)
;			      (return (setq name-end-index i)))
;			     ((= ch #/ ))
;			     (t (setq last-nonspace i)))))
;		   ;; Extract the name.
;		   (setq nextname
;			 (string-left-trim " "
;					   (substring pathstring
;						      name-start-index (1+ last-nonspace))))
;		   (setq name-start-index name-end-index)
;		   (setq nextname (parse-name-and-version nextname
;							  (cond (propflag ':property)
;								(t ':version))))
;		   ;; Don't keep a list starting with :version
;		   ;; if there isn't really a version number.
;		   (and (not propflag) (null (caddr nextname))
;			(setq nextname (cadr nextname)))
;		   ;; Skip over any "/" or "\" separating it from the following name.
;		   (and (< name-start-index len)
;			(or (= (aref pathstring name-start-index) #//)
;			    (= (aref pathstring name-start-index) #/\))
;			(setq name-start-index (1+ name-start-index)))
;		   ;; Add the new step onto the pathlist.
;		   (if (and propflag
;			    (not (symbolp previous-step))
;			    (not (and (consp previous-step)
;				      (eq (car previous-step) ':property))))
;		       (rplaca (last pathlist)
;			       (setq previous-step
;				     `(:subnode-property ,(car (last pathlist))
;							 . ,(cdr nextname))))
;		       (setq pathlist
;			     (nconc pathlist (list (setq previous-step nextname)))))))))))

;))

;; From file PATHNM.LISP PS:<L.FILE2> OZ:
;#8R FILE-SYSTEM#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

;(defun parse-name-and-version (pathstep list-car)
;  (let (name version (len (string-length pathstep))
;        (last-nonspace -1) name-end-index)
;    ;; Find the version-number delimiter "#" if there is one.
;    (do ((i 0 (1+ i)))
;	(( i len)
;	 (setq name-end-index len))
;      (let ((ch (aref pathstep i)))
;	(cond ((= ch #/)
;	       (setq i (1+ i))
;	       (setq last-nonspace i))
;	      ((= ch #/#)
;	       (return (setq name-end-index i)))
;	      ((= ch #/ ))
;	      (t (setq last-nonspace i)))))
;    ;; Extract the name, proper.
;    (setq name (substring pathstep 0 (1+ last-nonspace)))
;    ;; If there is a version number, extract it.
;    (cond ((< name-end-index len)
;	   (setq version
;		 (let ((base 10.) (ibase 10.) (*nopoint t) (package si:pkg-keyword-package))
;		   (read-from-string
;		     (substring pathstep (1+ name-end-index) len))))))
;    (if (eq version ':nil) (setq version nil))
;    (list list-car name version)))

;))


; From file DRIBBL.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DRIBBL  "

(defun dribble-stream-io (op &rest args
			  &aux (old-tio terminal-io)
			       (terminal-io
				 ;; Don't leave terminal-io as the dribble stream
				 ;; so that errors inside here don't bomb out.
				 (if (eq terminal-io dribble-stream)
				     *tv-stream*
				   terminal-io)))
  (declare (special *tv-stream* *file-stream* dribble-stream *rubout-handler-buffer*))
  (selectq op
    ((:tyo :string-out :line-out :fresh-line)
     (lexpr-funcall *tv-stream* op args)
     (lexpr-funcall *file-stream* op args))
    ((:any-tyi :tyi :tyi-no-hang :any-tyi-no-hang)
     (prog ()
	   (or rubout-handler (funcall *file-stream* ':send-if-handles ':force-output))
	   (*catch (if rubout-handler 'rubout-handler 'dummy-tag)
	     (let ((ch (funcall *tv-stream* op)))
	       (and ch rubout-handler
		    (array-push-extend *rubout-handler-buffer*
				       ;; If it's an activation blip, we want the character.
				       (if (listp ch) (cadr ch) ch)))
	       (return ch)))
	   ;;get here if someone threw to rubout-handler
	   ;;reset our buffer and continue the throw
	   (store-array-leader 0 *rubout-handler-buffer* 0)
	   (*throw 'rubout-handler nil)))
    (:untyi
     (funcall *tv-stream* ':untyi (car args))
     (and rubout-handler (plusp (length *rubout-handler-buffer*))
	  (decf (array-leader *rubout-handler-buffer* 0))))
    (:rubout-handler
     (store-array-leader 0 *rubout-handler-buffer* 0)	;reset the buffer
     (prog (vals)
       (cond ((and (funcall *file-stream* ':operation-handled-p ':safe-to-use-p)
		   (not (funcall *file-stream* ':safe-to-use-p)))
	      (format *tv-stream* "~&Dribble stream cannot accept output!")
	      (dribble-end)))
       (setq vals (multiple-value-list
		    ;; Bind terminal-io back to the dribble stream if that's what it was
		    ;; in case the code run inside the rubout handler
		    ;; uses terminal-io.
		    (let ((terminal-io old-tio))
		      (lexpr-funcall *tv-stream* op args))))
       ;; If the stream is having troubles, don't echo to it.
       (cond ((and (funcall *file-stream* ':operation-handled-p ':safe-to-use-p)
		   (not (funcall *file-stream* ':safe-to-use-p)))
	      (format *tv-stream* "~&Dribble stream cannot accept output!")
	      (dribble-end)))
       (funcall *file-stream* ':string-out *rubout-handler-buffer*)
       (funcall *file-stream* ':send-if-handles ':force-output)
       (return-list vals)))
    (:dribble-end
     (format *tv-stream* "~&Closing dribble file.")
     (close *file-stream*)
     (funcall *file-stream* ':send-if-handles ':truename))
    (:notice
     (if (funcall *file-stream* ':send-if-handles ':safe-to-use-p)
	 (lexpr-funcall *tv-stream* ':send-if-handles ':notice args)
       'tv:cold-load-stream))
    (:increment-cursorpos
     (cond ((eq (caddr args) ':character)
	    (dotimes (y-increment (cadr args))
	      (funcall *file-stream* ':tyo #\return))
	    (dotimes (x-increment (car args))
	      (funcall *file-stream* ':tyo #\sp))))
     (lexpr-funcall *tv-stream* op args))
    ((:finish :force-output)
     (lexpr-funcall *file-stream* ':send-if-handles op args)
     (lexpr-funcall *tv-stream* op args))
    (otherwise
     (lexpr-funcall *tv-stream* op args))))

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defun cw-parallel-binding-list (bindlist &aux free-inside bound)
  "Return a list of variables bound by BINDLIST, while recording any variables it uses free.
This is for parallel binding such as is found in PROG and LET."
  (declare (values variables expansion))
  (when (listp bindlist)
    (when cw-return-expansion-flag
      (setq bindlist (mapcar 'copylist bindlist)))
    (dolist (elt bindlist)
      (cond ((or (symbolp elt)
		 (and (listp elt) (null (cdr elt)) (setq elt (car elt))))
	     (push elt bound))
	    ((atom elt))
	    ((listp elt)
	     (if cw-return-expansion-flag
		 (setf (cadr elt)
		       (cw-expression (cadr elt)))
	       (cw-expression (cadr elt)))
	     (setq bound (nunion bound
				 (list (if (listp (car elt))
					   (cadr (car elt))
					 (car elt)))))
	     (setq free-inside
		   (nunion free-inside
			   (let (all-variables)
			     (do ((tail (cddr elt) (cdr tail)))
				 ((null tail))
			       (if cw-return-expansion-flag
				   (setf (car tail)
					 (cw-expression (car tail)))
				 (cw-expression (car tail))))
			     all-variables)))))))
  (dolist (b bound)
    (setq free-inside (delq b free-inside)))
  (setq all-variables (nunion all-variables free-inside))
  (values bound bindlist))

))

; From file QCLUKE.LISP PS:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCLUKE  "

(defun cw-serial-binding-list (bindlist &optional lambda-flag
			       &aux free-inside bound)
  "Return a list of variables bound by BINDLIST, while recording any variables it uses free.
This is for serial binding such as is found in LAMBDAs and PROG*'s.
LAMBDA-FLAG should be T for a LAMBDA arglist, otherwise NIL.
Second value is an expansion of the bindlist, if one is requested."
  (declare (values variables expansion))
  (when (listp bindlist)
    (when cw-return-expansion-flag
      (setq bindlist (mapcar 'copylist bindlist)))
    (dolist (elt bindlist)
      (cond ((and lambda-flag (memq elt lambda-list-keywords)))
	    ((or (symbolp elt)
		 (and (listp elt) (null (cdr elt)) (setq elt (car elt))))
	     (push elt bound))
	    ((atom elt))
	    ((listp elt)
	     (setq all-variables
		   (nunion all-variables
			   (let (all-variables)
			     (if cw-return-expansion-flag
				 (setf (cadr elt)
				       (cw-expression (cadr elt)))
			       (cw-expression (cadr elt)))
			     (dolist (b bound)
			       (setq all-variables (delq b all-variables)))
			     all-variables)))
	     (setq bound (nunion bound
				 (list (if (listp (car elt))
					   (cadr (car elt))
					 (car elt)))))
	     (and lambda-flag
		  (caddr elt)
		  (setq bound (nunion bound (list (caddr elt)))))
	     (unless lambda-flag
	       (setq free-inside
		     (nunion free-inside
			     (let (all-variables)
			       (do ((tail (cddr elt) (cdr tail)))
				   ((null tail))
				 (if cw-return-expansion-flag
				     (setf (car tail)
					   (cw-expression (car tail)))
				   (cw-expression (car tail))))
			       all-variables))))))))
  (dolist (b bound)
    (setq free-inside (delq b free-inside)))
  (setq all-variables (nunion all-variables free-inside))
  (values bound bindlist))

))

; From file CLPACK.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; CLPACK  "

(DEFUN EXPORT (SYMBOLS &OPTIONAL (PKG *PACKAGE*) FORCE-FLAG)
  "Makes SYMBOLS external in package PKG.
If the symbols are not already present in PKG, they are imported first.
Error if this causes a name conflict in any package that USEs PKG.
FORCE-FLAG non-NIL turns off checking for name conflicts, for speed
in the case where you know there cannot be any."
  (SETQ PKG (PKG-FIND-PACKAGE PKG))
  (UNLESS FORCE-FLAG
    (DO-FOREVER
      (LET ((CONFLICTS))
	;; Find all conflicts there are.
	;; Each element of CONFLICTS looks like
	;; (NEW-CONFLICTING-SYMBOL CONFLICT-PACKAGE
	;;   (OTHER-PACKAGE-NAME OTHER-PACKAGE-SYMBOL OTHER-PACKAGE)...)
	(DOLIST (P1 (PKG-USED-BY-LIST PKG))
	  (DOLIST (SYMBOL (IF (CLI:LISTP SYMBOLS) SYMBOLS (LIST SYMBOLS)))
	    (LET ((CANDIDATES
		    (CHECK-FOR-NAME-CONFLICT (IF (SYMBOLP SYMBOL) (GET-PNAME SYMBOL) SYMBOL)
					     P1 NIL SYMBOL PKG)))
	      (WHEN CANDIDATES
		(PUSH (LIST* SYMBOL P1 CANDIDATES) CONFLICTS)))))
	(UNLESS CONFLICTS (RETURN NIL))
	;; Now report whatever conflicts we found.
	(CERROR ':NO-ACTION NIL 'SYMBOL-NAME-CONFLICT
		"Name conflicts created by EXPORT in package ~A:
~:{~S causes a conflict in package ~A.~%~}"
		PKG CONFLICTS))))
  (DOLIST (SYM (IF (CLI:LISTP SYMBOLS) SYMBOLS (LIST SYMBOLS)))
    (UNLESS (SYMBOLP SYM) (SETQ SYM (INTERN SYM PKG)))
    (IMPORT SYM PKG)
    (MULTIPLE-VALUE-BIND (NIL INDEX)
	(PKG-INTERN-INTERNAL (GET-PNAME SYM)
			      (PKG-STRING-HASH-CODE (GET-PNAME SYM))
			      PKG)
      (SETF (PKG-SLOT-CODE PKG INDEX)
	    (PKG-MAKE-CODE 1 (PKG-SLOT-CODE PKG INDEX)))))
  T)

))

; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN LISP-TOP-LEVEL1 (TERMINAL-IO &OPTIONAL (TOP-LEVEL-P T) &AUX OLD-PACKAGE W-PKG)
  "Read-eval-print loop used by lisp listeners.  TERMINAL-IO
is the stream to read and print with."
  (COND ((VARIABLE-BOUNDP PACKAGE)
	 (BIND (LOCF PACKAGE) PACKAGE)))
  (FORMAT T "~&;Reading~@[ at top level~]" TOP-LEVEL-P)
  (IF (SEND TERMINAL-IO ':OPERATION-HANDLED-P ':NAME)
      (FORMAT T " in ~A." (SEND TERMINAL-IO ':NAME))
    (FORMAT T "."))
  (PUSH NIL *VALUES*)
  (DO ((READTABLE STANDARD-READTABLE)
       *READER-SYMBOL-SUBSTITUTIONS*
       (BASE BASE) (IBASE IBASE) (*NOPOINT *NOPOINT)
       (LAST-TIME-DEFAULT-COMMON-LISP '(NIL))
       THROW-FLAG)	;Gets non-NIL if throw to COMMAND-LEVEL (e.g. quitting from an error)
      (NIL)		;Do forever
    ;; If PACKAGE has changed, set OLD-PACKAGE and tell our window.
    ;; Conversely, if the window's package has changed, change ours.
    ;; The first iteration, we always copy from the window.
    (COND ((NOT (VARIABLE-BOUNDP PACKAGE)))
	  ((EQ TERMINAL-IO COLD-LOAD-STREAM))
	  ;; User set the package during previous iteration of DO
	  ;; => tell the window about it.
	  ((AND OLD-PACKAGE (NEQ PACKAGE OLD-PACKAGE))
	   (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':SET-PACKAGE PACKAGE)
	   (SETQ OLD-PACKAGE PACKAGE))
	  ;; Window's package has been changed, or first iteration through DO,
	  ;; => set our package to the window's -- if the window has one.
	  ((SETQ W-PKG (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':PACKAGE))
	   (AND (NEQ W-PKG PACKAGE)
		(SETQ PACKAGE W-PKG))
	   (SETQ OLD-PACKAGE PACKAGE))
	  ;; First time ever for this window => set window's package
	  ;; to the global value of PACKAGE.
	  ((NULL OLD-PACKAGE)
	   (SETQ OLD-PACKAGE PACKAGE)
	   (FUNCALL TERMINAL-IO ':SEND-IF-HANDLES ':SET-PACKAGE PACKAGE)))
    (CHECK-FOR-DEFAULT-COMMON-LISP-CHANGE LAST-TIME-DEFAULT-COMMON-LISP)
    (SETQ LAST-TIME-DEFAULT-COMMON-LISP *DEFAULT-COMMON-LISP*)
    (SETQ THROW-FLAG T)
    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to top level in ~A."
			  (OR (SEND TERMINAL-IO ':SEND-IF-HANDLES ':NAME)
			      "current process."))
      (TERPRI)
      (SETQ +++ ++ ++ + + -)			;Save last three input forms
      (SETQ - (READ-FOR-TOP-LEVEL))
      (LET ((LISP-TOP-LEVEL-INSIDE-EVAL T)
	    VALUES)
	(UNWIND-PROTECT
	    (SETQ VALUES (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
	  ;; Always push SOMETHING -- NIL if evaluation is aborted.
	  (PUSH VALUES *VALUES*))
	(SETQ ////// ////
	      //// //
	      // VALUES)
	(SETQ *** **				;Save first value, propagate old saved values
	      ** *
	      * (CAR //)))
      (DOLIST (VALUE //)
	(TERPRI)
	(FUNCALL (OR PRIN1 #'PRIN1) VALUE))
      (SETQ THROW-FLAG NIL))
    (WHEN THROW-FLAG
      ;; Inform user of return to top level.
      (FORMAT T "~&;Back to top level")
      (IF (SEND TERMINAL-IO ':OPERATION-HANDLED-P ':NAME)
	  (FORMAT T " in ~A." (SEND TERMINAL-IO ':NAME))
	(TYO #/.)))))

))

; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN CHECK-FOR-DEFAULT-COMMON-LISP-CHANGE (LAST-TIME-DEFAULT-COMMON-LISP)
  "Changes the values of READTABLE, *READER-SYMBOL-SUBSTITUTIONS*, *READ-BASE*, *PRINT-BASE*,
and *NOPOINT appropriately if *DEFAULT-COMMON-LISP* is NEQ LAST-TIME-DEFAULT-COMMON-LISP."
  (UNLESS (EQ *DEFAULT-COMMON-LISP* LAST-TIME-DEFAULT-COMMON-LISP)
    (SETQ READTABLE (IF *DEFAULT-COMMON-LISP* COMMON-LISP-READTABLE STANDARD-READTABLE)
	  *READER-SYMBOL-SUBSTITUTIONS*
	            (IF *DEFAULT-COMMON-LISP* *COMMON-LISP-SYMBOL-SUBSTITUTIONS*)
	  LAST-TIME-DEFAULT-COMMON-LISP *DEFAULT-COMMON-LISP*
	  BASE (IF *DEFAULT-COMMON-LISP* 10. (SYMEVAL-GLOBALLY 'BASE))
	  IBASE BASE
	  *NOPOINT (OR *DEFAULT-COMMON-LISP* (SYMEVAL-GLOBALLY '*NOPOINT)))
    (FORMAT T "~&;Now using ~A syntax and semantics and base ~D in package ~A.~%"
	    (IF *DEFAULT-COMMON-LISP* "Common Lisp" "traditional") IBASE
	    (PACKAGE-NAME PACKAGE)))
  *DEFAULT-COMMON-LISP*)

))

; From file LTOP.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; LTOP  "

(DEFUN BREAK (&OPTIONAL &QUOTE FORMAT-STRING &EVAL &REST ARGS
	      &AUX SAVED-BUFFER SAVED-BUFFER-POSITION)
  "Read-eval-print loop for use as subroutine.  Args are passed to FORMAT.
Many variables are rebound, as specified in SI:*BREAK-BINDINGS*."
  (SETQ FORMAT-STRING
	(IF (OR (SYMBOLP FORMAT-STRING) (AND (CONSP FORMAT-STRING)
					     (EQ (CAR FORMAT-STRING) 'QUOTE)
					     (SYMBOLP (CADR FORMAT-STRING))
					     (NULL (CDDR FORMAT-STRING))
					     (SETQ FORMAT-STRING (CADR FORMAT-STRING))))
	    (STRING FORMAT-STRING)
	  (EVAL1 FORMAT-STRING)))
  (UNLESS (OR (EQUAL FORMAT-STRING "")
	      (MEMQ (AREF FORMAT-STRING (1- (LENGTH FORMAT-STRING))) '(#/. #/? #/!)))
    (SETQ FORMAT-STRING (STRING-APPEND FORMAT-STRING #/.)))
  (LET-IF *DEFAULT-COMMON-LISP*
	  ((BASE 10.) (IBASE 10.) (*NOPOINT T))
    (PROGW *BREAK-BINDINGS*
      ;; Deal with keyboard multiplexing in a way similar to the error-handler.
      ;; If we break in the scheduler, set CURRENT-PROCESS to NIL.
      ;; If this is not the scheduler process, make sure it has a run reason
      ;; in case we broke in the middle of code manipulating process data.
      ;; If INHIBIT-SCHEDULING-FLAG is set, turn it off and print a warning.
      (COND ((EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP)
	     (SETQ CURRENT-PROCESS NIL)))
      (AND (NOT (NULL CURRENT-PROCESS))
	   (NULL (FUNCALL CURRENT-PROCESS ':RUN-REASONS))
	   (FUNCALL CURRENT-PROCESS ':RUN-REASON 'BREAK))
      (COND (INHIBIT-SCHEDULING-FLAG
	     (FORMAT T "~%---> Turning off INHIBIT-SCHEDULING-FLAG, you may lose. <---~%")
	     (SETQ INHIBIT-SCHEDULING-FLAG NIL)))
      (AND (MEMQ ':SAVE-RUBOUT-HANDLER-BUFFER (FUNCALL OLD-STANDARD-INPUT
						       ':WHICH-OPERATIONS))
	   (SETF (VALUES SAVED-BUFFER SAVED-BUFFER-POSITION)
		 (FUNCALL OLD-STANDARD-INPUT ':SAVE-RUBOUT-HANDLER-BUFFER)))
      (FORMAT T "~&;Breakpoint ~?  ~:@C to continue, ~:@C to quit.~%"
	      FORMAT-STRING ARGS #\RESUME #\ABORT)
      (LET* ((LAST-TIME-DEFAULT-COMMON-LISP '(NIL))
	     (VALUE
	       (DO-FOREVER
		 (SETQ LAST-TIME-DEFAULT-COMMON-LISP
		       (CHECK-FOR-DEFAULT-COMMON-LISP-CHANGE LAST-TIME-DEFAULT-COMMON-LISP))
		 (TERPRI)
		LOOK-FOR-SPECIAL-KEYS
		 (LET ((CHAR (FUNCALL STANDARD-INPUT ':TYI)))
		   ;; Intercept characters even if otherwise disabled in program
		   ;; broken out of.  Also treat c-Z like ABORT for convenience
		   ;; and for compatibility with the error handler.
		   (AND (= CHAR #\C-Z) (SETQ CHAR #\ABORT))
		   (COND ((AND (BOUNDP 'TV:KBD-STANDARD-INTERCEPTED-CHARACTERS)
			       (ASSQ CHAR TV:KBD-STANDARD-INTERCEPTED-CHARACTERS))
			  (FUNCALL (CADR (ASSQ CHAR TV:KBD-STANDARD-INTERCEPTED-CHARACTERS))
				   CHAR))
			 ((= CHAR #\RESUME)
			  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "[Resume]
")
			  (RETURN NIL))
			 (T (FUNCALL STANDARD-INPUT ':UNTYI CHAR))))
		 (LET ((EH:CONDITION-RESUME-HANDLERS (CONS T EH:CONDITION-RESUME-HANDLERS))
		       (THROW-FLAG T))
		   (CATCH-ERROR-RESTART ((SYS:ABORT ERROR)
					 "Return to BREAK ~?"
					 FORMAT-STRING ARGS)
		     (MULTIPLE-VALUE-BIND (TEM1 TEM)
			 (FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT
								      :FULL-RUBOUT)
								    (:ACTIVATION = #\END))
				  #'READ-FOR-TOP-LEVEL)
		       (COND ((EQ TEM ':FULL-RUBOUT)
			      (GO LOOK-FOR-SPECIAL-KEYS)))
		       (SHIFTF +++ ++ + - TEM1))
		     (COND ((AND (CONSP -) (EQ (CAR -) 'RETURN))
			    (RETURN (EVAL-ABORT-TRIVIAL-ERRORS (CADR -)))))	;(RETURN form) proceeds
		     (LET (VALUES)
		       (UNWIND-PROTECT
			   (SETQ VALUES
				 (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
			 ;; Always push SOMETHING for each form evaluated.
			 (PUSH VALUES *VALUES*))
		       (SETQ ////// ////
			     //// //
			     // VALUES)
		       (SETQ *** **
			     ** *
			     * (CAR //)))
		     (DOLIST (VALUE //)
		       (TERPRI)
		       (FUNCALL (OR PRIN1 #'PRIN1) VALUE))
		     (SETQ THROW-FLAG NIL))
		   (WHEN THROW-FLAG
		     (FORMAT T "~&;Back to Breakpoint ~?  ~:@C to continue, ~:@C to quit.~%"
			     FORMAT-STRING ARGS #\RESUME #\ABORT))))))
	;; Before returning, restore and redisplay rubout handler's buffer so user
	;; gets what he sees, if we broke out of reading through the rubout handler.
	;; If we weren't inside there, the rubout handler buffer is now empty because
	;; we read from it, so leave it alone.  (Used to :CLEAR-INPUT).
	(COND (SAVED-BUFFER
	       (FUNCALL OLD-STANDARD-INPUT ':RESTORE-RUBOUT-HANDLER-BUFFER
			SAVED-BUFFER SAVED-BUFFER-POSITION)))
	VALUE))))

))

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-PNAME-STRING (SYMBOL STREAM FASTP &OPTIONAL NO-PACKAGE-PREFIXES
			   &AUX STRING LEN FSMWINS MUST// TEM)
    (DECLARE (SPECIAL XP-STREAM XP-FASTP XR-EXTENDED-IBASE-P))
    ;; Print a package prefix if appropriate.
    (WHEN (AND *PRINT-ESCAPE* (NOT NO-PACKAGE-PREFIXES)
	       (SYMBOLP SYMBOL))
      (IF (NULL (SYMBOL-PACKAGE SYMBOL))
	  (AND *PRINT-GENSYM*
	       (SEND STREAM ':STRING-OUT
		     (PTTBL-UNINTERNED-SYMBOL-PREFIX READTABLE)))
	(MULTIPLE-VALUE-BIND (PKG-OR-STRING INTERNAL-FLAG)
	    (PKG-PRINTING-PREFIX SYMBOL PACKAGE)
	  (MULTIPLE-VALUE (PKG-OR-STRING INTERNAL-FLAG SYMBOL)
	    (LET* ((TEM1 (CAR (RASSQ SYMBOL *READER-SYMBOL-SUBSTITUTIONS*)))
		   (TEM2 (UNLESS TEM1 (CDR (ASSQ SYMBOL *READER-SYMBOL-SUBSTITUTIONS*))))
		   POS IF)
	      (COND ((AND TEM1 (MEMBER (MULTIPLE-VALUE (POS IF)
					 (PKG-PRINTING-PREFIX TEM1 PACKAGE))
				       '(NIL "")))
		     (VALUES POS IF TEM1))
		    (TEM2 (MULTIPLE-VALUE (NIL IF POS)
			    (INTERN TEM2 PACKAGE))
			  (VALUES POS (EQ IF ':INTERNAL) TEM2))
		    (T (VALUES PKG-OR-STRING INTERNAL-FLAG SYMBOL)))))
	  (WHEN PKG-OR-STRING
	    (UNLESS (EQUAL PKG-OR-STRING "")
	      (PRINT-PNAME-STRING (IF (STRINGP PKG-OR-STRING) PKG-OR-STRING
				    (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING))
				  STREAM FASTP))
	    (SEND STREAM ':STRING-OUT
		  (IF (AND (NOT (STRINGP PKG-OR-STRING))
			   PACKAGE
			   (ASSOC (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING)
				  (DONT-OPTIMIZE (PKG-REFNAME-ALIST PACKAGE))))
		      ;; Use #: to inhibit an interfering local nickname.
		      "#:"
		    (IF INTERNAL-FLAG
			(PTTBL-PACKAGE-INTERNAL-PREFIX READTABLE)
		      (PTTBL-PACKAGE-PREFIX READTABLE))))))))
    (SETQ STRING (STRING SYMBOL))
    (COND ((NOT *PRINT-ESCAPE*)
	   (PRINT-RAW-STRING STRING STREAM FASTP))
	  (T
	   (SETQ FSMWINS
	    (AND (PLUSP (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING)))
		 (DO ((I 0 (1+ I))
		      (STATE (RDTBL-STARTING-STATE READTABLE))
		      (FSM (RDTBL-FSM READTABLE))
		      (CHAR)
		      (ESCAPE-CODE (RDTBL-ESCAPE-CODE READTABLE))
		      (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE READTABLE))
		      (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE READTABLE)))
		     ((= I LEN)
		      (COND ((NOT (NUMBERP STATE))
			     (DO L (RDTBL-MAKE-SYMBOL READTABLE) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    ((NOT (NUMBERP (SETQ STATE
						 (AR-2 FSM
						       STATE
						       (RDTBL-BREAK-CODE READTABLE)))))
			     (DO L (RDTBL-MAKE-SYMBOL-BUT-LAST READTABLE) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    (T NIL)))
		     (SETQ CHAR (AR-1 STRING I))
		     (COND ((OR (NOT (NUMBERP STATE))	;FSM ran out OR
				(NOT			;Translated char? then fsm loses
				 (= CHAR (RDTBL-TRANS READTABLE CHAR))))
			    (OR MUST//				   ;Must we slash?
				(DO ((I I (1+ I))) ((= I LEN))
				  (LET ((CODE (RDTBL-CODE READTABLE (AR-1 STRING I))))
				    (WHEN (OR (= CODE ESCAPE-CODE)
					      (= CODE MULTIPLE-ESCAPE-CODE)
					      (= CODE CHARACTER-CODE-ESCAPE-CODE))
				      (SETQ MUST// T)
				      (RETURN NIL)))))
			    (RETURN NIL)))
		     (SETQ STATE
			   (AR-2 FSM
				 STATE
				 (COND ((LET ((CODE (RDTBL-CODE READTABLE (AR-1 STRING I))))
					  (OR (= CODE ESCAPE-CODE)
					      (= CODE MULTIPLE-ESCAPE-CODE)
					      (= CODE CHARACTER-CODE-ESCAPE-CODE)))
					(SETQ MUST// T)	;YES: set flag.
					(RDTBL-SLASH-CODE READTABLE))
				       ((AND (NUMBERP BASE) (> BASE 10.)
					     ( #/A CHAR (+ BASE #/A -11.)))
					(CDR (GETF (RDTBL-PLIST READTABLE) 'EXTENDED-DIGIT)))
				       (T
					(RDTBL-CODE READTABLE CHAR))))))))
	   (OR FSMWINS
	       (FUNCALL STREAM ':TYO (PTTBL-OPEN-QUOTE-SYMBOL READTABLE)))
	   (COND ((OR MUST//
		      (AND FSMWINS (NEQ *PRINT-CASE* ':UPCASE)))
		  (DO ((I 0 (1+ I))
		       (ESCAPE-CODE (RDTBL-ESCAPE-CODE READTABLE))
		       (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE READTABLE))
		       (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE READTABLE))
		       (PREV-CHAR 0)
		       CODE)
		      ((= I LEN))
		    (SETQ TEM (AR-1 STRING I))
		    (SETQ CODE (RDTBL-CODE READTABLE TEM))
		    (COND ((OR (= CODE ESCAPE-CODE)
			       (= CODE MULTIPLE-ESCAPE-CODE)
			       (= CODE CHARACTER-CODE-ESCAPE-CODE))
			   (FUNCALL STREAM ':TYO (PTTBL-SLASH READTABLE))
			   (FUNCALL STREAM ':TYO TEM))
			  ((OR (EQ *PRINT-CASE* ':DOWNCASE)
			       (AND (EQ *PRINT-CASE* ':CAPITALIZE)
				    (ALPHANUMERICP PREV-CHAR)))
			   (FUNCALL STREAM ':TYO (CHAR-DOWNCASE TEM)))
			  (T
			   (FUNCALL STREAM ':TYO TEM)))
		    (SETQ PREV-CHAR TEM)))
		 (T (PRINT-RAW-STRING STRING STREAM FASTP)))
	   (OR FSMWINS
	       (FUNCALL STREAM ':TYO (PTTBL-CLOSE-QUOTE-SYMBOL READTABLE)))
	   )))

))

; From file FED.LISP PS:<L.WINDOW> OZ:
#8R FED#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FED")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; FED  "

(DEFUN COM-COPY-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No font is selected.")
    (LET ((NEW-FONT (PROMPT-LINE-DEFAULTED-READLINE NIL 'INTERN-FONT-NAME
						    "Copy to (new font name): "))
	  (OLD-FD (FONT-GET-FD CURRENT-FONT))
	  NEW-FD)
      (IF (AND (BOUNDP NEW-FONT)
	       (NOT (FED-Y-OR-N-P "Font ~A exists.  Clobber it? " NEW-FONT)))
	  (BARF "Aborted."))
      (SETF (SYMBOL-VALUE NEW-FONT) (SI:COPY-OBJECT (SYMBOL-VALUE CURRENT-FONT)))
      (SETF (FONT-NAME (SYMBOL-VALUE NEW-FONT)) NEW-FONT)
      (SETQ CURRENT-FONT NEW-FONT)
      ;; Make the new font's FD a copy of the old one, but don't copy name.
      (SETQ NEW-FD (FONT-GET-FD NEW-FONT))
      (COPY-ARRAY-CONTENTS-AND-LEADER OLD-FD NEW-FD)
      (SETF (FD-NAME NEW-FD) NEW-FONT)
      ;; Replace the CD's with copies, too.
      (DOTIMES (I (ARRAY-LENGTH NEW-FD))
	(IF (AREF NEW-FD I)
	    (SETF (AREF NEW-FD I)
		  (SI:COPY-OBJECT (AREF NEW-FD I)))))
      (PUTPROP NEW-FONT NEW-FD 'FONT-DESCRIPTOR)
      (PUTPROP NEW-FONT (SYMBOL-VALUE NEW-FONT) 'FONT-DESCRIBED)
      (UPDATE-FED-EDITED-CHARS)
      (DISPLAY-LABEL))))

))

; From file RESOUR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; RESOUR  "

(DEFMACRO DEFRESOURCE (NAME PARAMETERS &REST OPTIONS)
  "Define a resource named NAME, with parameters PARAMETERS for constructing objects.
OPTIONS can specify how to create objects and how to tell when old objects
can be reused."
  ;; Old format?
  (IF (OR (CONSP NAME) (NULL OPTIONS) (LISTP (CAR OPTIONS)))
      ;; In system 88, crack down on these; make sure they finally get fixed.
      (FERROR NIL "Obsolete form of DEFRESOURCE for ~S~%" NAME)
    (LET ((CONSTRUCTOR-FORM NIL) (FINDER-FORM NIL) (MATCHER-FORM NIL) (CHECKER-FORM NIL)
	  (CONSTRUCTOR-FUNCTION NIL) (FINDER-FUNCTION NIL) (MATCHER-FUNCTION NIL)
	  (PARAMETIZER-FUNCTION NIL) (CHECKER-FUNCTION NIL) (INITIAL-COPIES 0)
	  (INITIALIZER-FORM NIL) (INITIALIZER-FUNCTION NIL) (FREE-LIST-SIZE 20.) (PARAMS NIL))
      (OR (LISTP PARAMETERS) (NULL PARAMETERS)
	  (FERROR NIL "~S invalid parameter list" PARAMETERS))
      (SETQ PARAMS (LOOP FOR P IN PARAMETERS
			 UNLESS (MEMQ P LAMBDA-LIST-KEYWORDS)
			 COLLECT (IF (SYMBOLP P) P (CAR P))))
      (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY #'CDDR
	    DO (SELECTQ KEYWORD
		 (:CONSTRUCTOR (SETQ CONSTRUCTOR-FORM VALUE))
		 (:FINDER (SETQ FINDER-FORM VALUE))
		 (:MATCHER (SETQ MATCHER-FORM VALUE))
		 (:CHECKER (SETQ CHECKER-FORM VALUE))
		 (:INITIALIZER (SETQ INITIALIZER-FORM VALUE))
		 (:INITIAL-COPIES
		  (SETQ INITIAL-COPIES
			(COND ((NULL VALUE) 0)
			      ((NUMBERP VALUE) VALUE)
			      (T (FERROR NIL ":INITIAL-COPIES ~S - number required"
					 VALUE)))))
		 (:FREE-LIST-SIZE
		  (SETQ FREE-LIST-SIZE
			(COND ((NULL VALUE) 20.)
			      ((NUMBERP VALUE) VALUE)
			      (T (FERROR NIL ":FREE-LIST-SIZE ~S - number required")))))
		 (OTHERWISE (FERROR NIL "~S illegal option in DEFRESOURCE" KEYWORD))))
      (OR CONSTRUCTOR-FORM (FERROR NIL "DEFRESOURCE requires the :CONSTRUCTOR option"))
      ;;Pick function names.  Note that NIL is SYMBOLP.
      (SETQ CONSTRUCTOR-FUNCTION (IF (SYMBOLP CONSTRUCTOR-FORM) CONSTRUCTOR-FORM
				   `(:PROPERTY ,NAME RESOURCE-CONSTRUCTOR)))
      (SETQ FINDER-FUNCTION (IF (SYMBOLP FINDER-FORM) FINDER-FORM
			      `(:PROPERTY ,NAME RESOURCE-FINDER)))
      (SETQ MATCHER-FUNCTION (IF (SYMBOLP MATCHER-FORM) MATCHER-FORM
			       `(:PROPERTY ,NAME RESOURCE-MATCHER)))
      (SETQ CHECKER-FUNCTION (IF (SYMBOLP CHECKER-FORM) CHECKER-FORM
			       `(:PROPERTY ,NAME RESOURCE-CHECKER)))
      (SETQ INITIALIZER-FUNCTION (IF (SYMBOLP INITIALIZER-FORM) INITIALIZER-FORM
				   `(:PROPERTY ,NAME RESOURCE-INITIALIZER)))
      (SETQ PARAMETIZER-FUNCTION (IF (AND PARAMETERS (NOT MATCHER-FORM) (NOT FINDER-FORM))
				     `(:PROPERTY ,NAME RESOURCE-PARAMETIZER)))
      `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,NAME DEFRESOURCE))
	 ,(IF (NOT (SYMBOLP CONSTRUCTOR-FORM))
	      `(DEFUN ,CONSTRUCTOR-FUNCTION (IGNORE ,@PARAMETERS)
		 ,@PARAMS
		 ,CONSTRUCTOR-FORM))
	 ,(IF (NOT (SYMBOLP FINDER-FORM))
	      `(DEFUN ,FINDER-FUNCTION (IGNORE ,@PARAMETERS)
		 ,@PARAMS
		 ,FINDER-FORM))
	 ,(IF (NOT (SYMBOLP MATCHER-FORM))
	      `(DEFUN ,MATCHER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
		 ,@PARAMS
		 ,MATCHER-FORM))
	 ,(IF (NOT (SYMBOLP CHECKER-FORM))
	      `(DEFUN ,CHECKER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
					 ,@PARAMETERS)
		 ,@PARAMS ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
		 ,CHECKER-FORM))
	 ,(IF (NOT (SYMBOLP INITIALIZER-FORM))
	      `(DEFUN ,INITIALIZER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
		 ,@PARAMS ,(INTERN "OBJECT")
		 ,INITIALIZER-FORM))
	 ,(IF PARAMETIZER-FUNCTION
	      `(DEFUN ,PARAMETIZER-FUNCTION ,PARAMETERS
		 (LIST ,@PARAMS)))
	 (INITIALIZE-RESOURCE ',NAME ',CONSTRUCTOR-FUNCTION ',FINDER-FUNCTION
			      ',MATCHER-FUNCTION ',CHECKER-FUNCTION
			      ',PARAMETIZER-FUNCTION ',INITIAL-COPIES ',FREE-LIST-SIZE
			      ',INITIALIZER-FUNCTION)))))

))

; From file DOCMIC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; DOCMIC  "

(SETF (DOCUMENTATION 'ARRAY-PUSH 'FUNCTION)
  "Add X as an element at the end of ARRAY.
The fill pointer (leader element 0) is the index of the next element to
be added.  Returns NIL and does no add the element if the array is full;
use ARRAY-PUSH-EXTEND instead if you want the array to grow automatically.")

))

(setf (documentation 'array-pop 'function)
  "Returns the last used element of ARRAY, and decrements the fill pointer.
For an ART-Q-LIST array, the cdr codes are updated so that the overlayed list
no longer contains the element removed. Signals an error if ARRAY is empty
(has fill-pointer 0)")

(setf (documentation 'fillarray 'function)
  "Fill the contents of ARRAY from SOURCE.
If SOURCE is a list, its last element is repeated to fill any part of ARRAY left over.
If SOURCE is an array, elements of ARRAY not filled by SOURCE are left untouched.
If SOURCE is NIL, the array is filled with the default type for the array; this is 0 or NIL.
If ARRAY is NIL, a new list as big as SOURCE is created.")

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM
		     &OPTIONAL
		     (WHICH-OPERATIONS (WHICH-OPERATIONS-FOR-PRINT STREAM))
		     &AUX NSS (FASTP (MEMQ ':STRING-OUT WHICH-OPERATIONS)))
  (CATCH-CONTINUATION-IF T 'PRINT-OBJECT
      #'(LAMBDA () (FORMAT STREAM "...error printing ")
		(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP :FASTP FASTP))
		(FORMAT STREAM "..."))
      NIL
    (CONDITION-RESUME '((ERROR) :ABORT-PRINTING T ("Give up trying to print this object.")
			CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
      (OR (AND (MEMQ ':PRINT WHICH-OPERATIONS)	;Allow stream to intercept print operation
	       (FUNCALL STREAM ':PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*))
	  (AND *PRINT-CIRCLE*
	       (%POINTERP EXP)
	       (OR (NOT (SYMBOLP EXP))
		   (NOT (SYMBOL-PACKAGE EXP)))
	       ;; This is a candidate for circular or shared structure printing.
	       ;; See what the hash table says about the object:
	       ;; NIL - occurs only once.
	       ;; T - occurs more than once, but no occurrences printed yet.
	       ;;  Allocate a label this time and print #label= as prefix.
	       ;; A number - that is the label.  Print only #label#.
	       (*CATCH 'LABEL-PRINTED
		 (SEND PRINT-HASH-TABLE ':MODIFY-HASH EXP
		       #'(LAMBDA (KEY VALUE KEY-FOUND-P STREAM)
			   KEY KEY-FOUND-P
			   (COND ((NULL VALUE) NIL)
				 ((EQ VALUE T)
				  (LET ((LABEL (INCF PRINT-LABEL-NUMBER))
					(BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM LABEL STREAM)
				    (SEND STREAM ':TYO #/=)
				    LABEL))
				 (T
				  (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
				    (SEND STREAM ':TYO #/#)
				    (PRINT-FIXNUM VALUE STREAM)
				    (SEND STREAM ':TYO #/#)
				    (*THROW 'LABEL-PRINTED T)))))
		       STREAM)
		 NIL))
	  (TYPECASE EXP
	    (:FIXNUM (PRINT-FIXNUM EXP STREAM))
	    (:SYMBOL
	     (PRINT-PNAME-STRING EXP STREAM FASTP))
	    (:LIST
	     (IF (AND PRINLEVEL (>= I-PRINDEPTH PRINLEVEL))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL READTABLE) STREAM FASTP)
	       (IF *PRINT-PRETTY*
		   (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
		 (PRINT-LIST EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
	    (:STRING
	     (IF ( (ARRAY-ACTIVE-LENGTH EXP) (ARRAY-LENGTH EXP))
		 (PRINT-QUOTED-STRING EXP STREAM FASTP)
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:INSTANCE
	      (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
	    (:ENTITY
	     (IF (MEMQ ':PRINT-SELF (FUNCALL EXP ':WHICH-OPERATIONS))
		 (FUNCALL EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*) 
	       (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS)))
	    (:NAMED-STRUCTURE
	     (IGNORE-ERRORS
	       (SETQ NSS (NAMED-STRUCTURE-P EXP)))
	     (COND ((AND (SYMBOLP NSS)
			 (OR (GET NSS 'NAMED-STRUCTURE-INVOKE)
			     (GET NSS ':NAMED-STRUCTURE-INVOKE))
			 (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		    (NAMED-STRUCTURE-INVOKE EXP ':PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
		   (T				;NAMED STRUCTURE THAT DOESN'T PRINT ITSELF
		    (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
;		    (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
;		      (PRINC NSS STREAM))
	    (:ARRAY
	     (PRINT-ARRAY EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))
	    (:SMALL-FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP T))
	    (:FLONUM
	     (PRINT-FLONUM EXP STREAM FASTP NIL))
	    (:BIGNUM
	     (PRINT-BIGNUM EXP STREAM FASTP))
	    (:RATIONAL
	     (PRINT-RATIONAL EXP STREAM FASTP))
	    (:COMPLEX (PRINT-COMPLEX EXP STREAM FASTP))
	    (:CHARACTER
	      (IF (NOT *PRINT-ESCAPE*)
		  (SEND STREAM ':TYO (LDB %%CH-CHAR EXP))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-BEFORE-FONT READTABLE))
		(IF (LDB-TEST %%CH-FONT EXP)
		    (LET ((BASE 10.) (*NOPOINT T))
		      (PRIN1 (LDB %%CH-FONT EXP) STREAM)))
		(SEND STREAM ':STRING-OUT (PTTBL-CHARACTER-PREFIX READTABLE))
		(IF (CHAR-BIT EXP ':CONTROL)
		    (SEND STREAM ':STRING-OUT "c-"))
		(IF (CHAR-BIT EXP ':META)
		    (SEND STREAM ':STRING-OUT "m-"))
		(IF (CHAR-BIT EXP ':SUPER)
		    (SEND STREAM ':STRING-OUT "s-"))
		(IF (CHAR-BIT EXP ':HYPER)
		    (SEND STREAM ':STRING-OUT "h-"))
		(SEND STREAM ':TYO (LDB %%CH-CHAR EXP))))
	    (:NUMBER
	     (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
	     (PRINT-RAW-STRING (GET-PNAME (DATA-TYPE EXP))
			       STREAM
			       FASTP)
	     (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
	     (LET ((BASE 8))
	       (PRINT-FIXNUM (%POINTER EXP) STREAM))
	     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP))
	    (T  ;Some random type we don't know about
	     (PRINT-RANDOM-OBJECT EXP STREAM FASTP I-PRINDEPTH WHICH-OPERATIONS))))))
   EXP)

))

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-ARRAY (EXP STREAM FASTP I-PRINDEPTH
		    &optional (which-operations (which-operations-for-print stream)))
  (IF *PRINT-ARRAY*
      (IF (AND (= (ARRAY-RANK EXP) 1)
	       (EQ (ARRAY-TYPE EXP) 'ART-1B))
	  (PRINT-BIT-VECTOR EXP STREAM)
	(IF *PRINT-PRETTY*
	    (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)
	  (IF (= (ARRAY-RANK EXP) 1)
	      (PRINT-VECTOR EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
	    (PRINT-MULTIDIMENSIONAL-ARRAY EXP I-PRINDEPTH STREAM WHICH-OPERATIONS))))
    (PRINTING-RANDOM-OBJECT (EXP STREAM :FASTP FASTP)
      (PRINT-RAW-STRING (GET-PNAME (ARRAY-TYPE EXP)) STREAM FASTP)
      (DO ((I 0 (1+ I))
	   (RANK (ARRAY-RANK EXP))
	   (DIM))
	  ((= I RANK))
	(SETQ DIM (ARRAY-DIMENSION EXP I))
	(FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
	(PRINT-FIXNUM DIM STREAM)))))

))

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-VECTOR (EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
  (FUNCALL STREAM ':STRING-OUT (PTTBL-OPEN-VECTOR READTABLE))
  (DO ((I-PRINLENGTH 0 (1+ I-PRINLENGTH))
       (LENGTH (LENGTH EXP))
       (FIRST T NIL))
      ((= I-PRINLENGTH LENGTH)
       (FUNCALL STREAM ':STRING-OUT (PTTBL-CLOSE-VECTOR READTABLE)))
    (OR FIRST (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE)))
    (PRINT-OBJECT (AREF EXP I-PRINLENGTH) (1+ I-PRINDEPTH) STREAM WHICH-OPERATIONS)
    (AND PRINLENGTH (>= I-PRINLENGTH PRINLENGTH)	;One frob gets printed before test.
	 (PROGN (SEND STREAM ':TYO (PTTBL-SPACE READTABLE))
		(PRINT-RAW-STRING (PTTBL-PRINLENGTH READTABLE) STREAM
				  (MEMQ ':STRING-OUT WHICH-OPERATIONS))
		(SEND STREAM ':TYO (PTTBL-CLOSE-PAREN READTABLE))
		(RETURN NIL)))))

))

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-MULTIDIMENSIONAL-ARRAY (EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
  (DOLIST (ELT (PTTBL-ARRAY READTABLE))
    (COND ((STRINGP ELT) (SEND STREAM ':STRING-OUT ELT))
	  ((EQ ELT ':RANK)
	   (LET ((BASE 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
	     (PRINT-FIXNUM (ARRAY-RANK EXP) STREAM)))
	  ((EQ ELT ':SEQUENCES)
	   (PRINT-ARRAY-CONTENTS EXP 0 0 I-PRINDEPTH STREAM WHICH-OPERATIONS)))))

))

; From file PRINT.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(DEFUN PRINT-NAMED-STRUCTURE (NSS EXP I-PRINDEPTH STREAM WHICH-OPERATIONS)
  (LET ((DESCRIPTION (GET NSS 'DEFSTRUCT-DESCRIPTION)))
    (IF (NOT DESCRIPTION)
	(PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP))
      (FUNCALL STREAM ':STRING-OUT "#S")		;should use printtable
      (LET ((SLOT-ALIST (DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
	    (L (LIST NSS)))
	(DOLIST (S SLOT-ALIST)
	  (LET* ((KWD (INTERN (GET-PNAME (CAR S)) PKG-KEYWORD-PACKAGE))
		 (FUN (DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR S)))
		 (INIT (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE (CDR S)))
		 (VAL (EVAL `(,FUN ,EXP))))	;watch out for macros!
	    (UNLESS (EQUAL VAL INIT)
	      (PUSH KWD L) (PUSH VAL L))))
	(PRINT-LIST (NREVERSE L) I-PRINDEPTH STREAM WHICH-OPERATIONS)))))

))

(setf (documentation 'zwei:defmajor 'function)
  "Define a major mode.
COMMAND-NAME is a symbol, such as COM-MY-MODE.
MODE-SYMBOL is a symbol for the mode, such as MY-MODE.
MODE-NAME is a string for the mode, such as /"My/".
COMMAND-DOCUMENTATION is the doc string for COM-MY-MODE.
COMMAND-OPTIONS is the DEFCOM options list.
BODY is not simply a list of arbitrary forms;
only SETQ, PUSH, ASET, SET-COMTAB, SET-CHAR-SYNTAX,
 SET-SYNTAX-TABLE-INDIRECTION and COMMAND-HOOK forms may be used.
[Actually, one can enclose arbitrary forms using PROGN. No attempt
to undo them is made -- they are just evaluated when the mode is
turned on.]")

(setf (documentation 'time:daylight-savings-time-p 'function)
      "T if daylight savings time would be in effect at specified time in North America.")

(setf (documentation 'time:decode-universal-time-without-dst 'function)
      "Like DECODE-UNIVERSAL-TIME, but always uses standard time.
Even if the time is one at which daylight savings time would be in effect,
the hour and date are computed as for standard time.")


; From file FORMAT.LISP PS:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN INDENT-CONVERT-STREAM (OP &REST ARGS)
  (SELECTQ OP
    (:TYO
     (SEND INDENT-CONVERTED-STREAM ':TYO (CAR ARGS))
     (WHEN (= (CAR ARGS) #\RETURN)
       (DOTIMES (I INDENT-CONVERT)
	 (SEND INDENT-CONVERTED-STREAM ':TYO #\SPACE))))
    (:FRESH-LINE
     (SEND INDENT-CONVERTED-STREAM ':TYO #\RETURN)
     (DOTIMES (I INDENT-CONVERT)
       (SEND INDENT-CONVERTED-STREAM ':TYO #\SPACE)))
    ((:STRING-OUT :LINE-OUT)
     (SI:STREAM-DEFAULT-HANDLER 'INDENT-CONVERT-STREAM OP (CAR ARGS) (CDR ARGS)))
    (:WHICH-OPERATIONS (REMOVE ':PRINT (SEND INDENT-CONVERTED-STREAM ':WHICH-OPERATIONS)))
    (T (LEXPR-FUNCALL INDENT-CONVERTED-STREAM OP ARGS))))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-NAMESTRING)
	   (HOST-SPECIFIED-P NAMESTRING &OPTIONAL (START 0) END)
  (DECLARE (VALUES DEVICE DIRECTORY NAME TYPE VERSION
		   DEVICE-SPECIFIED-P DIRECTORY-SPECIFIED-P NAME-SPECIFIED-P TYPE-SPECIFIED-P
		   VERSION-SPECIFIED-P))
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (LET* ((DIR-DELIM-ALIST (FUNCALL-SELF ':DIRECTORY-DELIMITERS))
	 (ALL-DELIMS (NCONC (MAPCAR #'CAR DIR-DELIM-ALIST) '(#/: #/. #/; #\SP))))
    (DO ((IDX (OR (STRING-SEARCH-NOT-CHAR #\SP NAMESTRING START END) END))
	 (TEM) (TEM1) (DELIM)
	 (DIR-DELIM)
	 (DEV)
	 (DIR) (NAM) (TYP) (VER)
	 DEV-SPECIFIED-P NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P)
	(( IDX END)
	 (IF (EQUAL TYP "") (SETQ TYP ':UNSPECIFIC))
	 (IF (EQUAL NAM "") (SETQ NAM NIL))
	 (SETQ DEV (OR DEV (IF HOST-SPECIFIED-P (SEND SELF ':PRIMARY-DEVICE))))
	 (VALUES DEV DIR NAM TYP VER
		 DEV-SPECIFIED-P DIR NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P))
      (COND ((SETQ DIR-DELIM (CDR (ASSQ (AREF NAMESTRING IDX) DIR-DELIM-ALIST)))
	     (AND DIR
		  (PATHNAME-ERROR IDX "Directory occurs twice in ~A" NAMESTRING))
	     (SETQ IDX (1+ IDX))
	     (DO () (NIL)
	       (MULTIPLE-VALUE (TEM IDX DELIM)
		 (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING
						  (LIST #/. DIR-DELIM) IDX END NIL T))
	       (SETQ DIR (IF (AND (= DELIM DIR-DELIM) (NULL DIR))
			     (LIST TEM)
			   (NCONC DIR (NCONS TEM))))
	       (AND (= DELIM DIR-DELIM) (RETURN))))
	    (T
	     (MULTIPLE-VALUE (TEM IDX DELIM)
	       (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING ALL-DELIMS IDX END T T))
	     (COND ((ASSQ DELIM DIR-DELIM-ALIST)
		    (SETQ IDX (1- IDX)))
		   ((AND (= DELIM #/;) VER)	;Protect against twenex attribute usage
		    (SETQ IDX END)))
	     (IF (MEMBER TEM '("*" #.(MAKE-STRING 1 ':INITIAL-VALUE #\BREAK)))
		 (SETQ TEM ':WILD))
	     (COND ((= DELIM #/:)
		    (AND DEV
			 (PATHNAME-ERROR IDX
				 "Device occurs twice in ~A" NAMESTRING))
		    (SETQ DEV TEM DEV-SPECIFIED-P (1- IDX)))
		   ((= DELIM #/;)
		    (COND ((NULL NAM-SPECIFIED-P)
			   (SETQ NAM TEM TYP ""
				 NAM-SPECIFIED-P (1- IDX) TYP-SPECIFIED-P (1- IDX)))
			  ((NULL TYP-SPECIFIED-P)
			   (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX)))))
		   ((NULL NAM-SPECIFIED-P)
		    (SETQ NAM TEM NAM-SPECIFIED-P (1- IDX))
		    (IF (= DELIM #/.) (SETQ TYP ':UNSPECIFIC)))
		   ((NULL TYP-SPECIFIED-P)
		    (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX))
		    (IF (EQ DELIM #/.) (SETQ VER ':UNSPECIFIC)))
		   ((NULL VER-SPECIFIED-P)
		    (SETQ VER-SPECIFIED-P (1- IDX))
		    (COND ((NULL TEM)
			   (SETQ VER NIL))
			  ((EQUAL TEM "")
			   (SETQ VER ':UNSPECIFIC))
			  ((SETQ TEM1 (NUMERIC-P TEM))
			   (SETQ VER TEM1))
			  ((EQ TEM ':WILD)
			   (SETQ VER ':WILD))
			  ((FUNCALL-SELF ':OLDEST-CHECK TEM)
			   (SETQ VER ':OLDEST))
			  (T (PATHNAME-ERROR IDX
				     "Version must be numeric in ~A" NAMESTRING))))))))))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFWRAPPER (VMS-PATHNAME-MIXIN :PARSE-NAMESTRING) (ARGLIST . BODY)
  `(LET ((NAMESTRING (SECOND ARGLIST)))
     (IF (STRING-SEARCH-CHAR #/ (NTH 1 ARGLIST))
	 (FERROR 'PATHNAME-PARSE-ERROR "Illegal chararacter in ~A" (NTH 1 ARGLIST)))
     (MULTIPLE-VALUE-BIND (DEV DIR NAM TYP VER NIL NIL
			   NAM-SPECIFIED-P TYP-SPECIFIED-P)
	 (PROGN ,@BODY)
       (AND TYP-SPECIFIED-P
	    (MEMQ VER '(NIL :UNSPECIFIC))
	    (< TYP-SPECIFIED-P (LENGTH NAMESTRING))
	    (EQ (AREF NAMESTRING TYP-SPECIFIED-P) #/;)
	    (SETQ VER ':NEWEST))
       (AND NAM-SPECIFIED-P
	    (EQ NAM NIL)
	    ( (AREF NAMESTRING (1- NAM-SPECIFIED-P)) #\)
	    (SETQ NAM :UNSPECIFIC))		  
       (AND (EQUAL DIR '("000000"))
	    (SETQ DIR ':ROOT))
       (VALUES DEV DIR NAM TYP VER))))

))

; From file PATHST.LISP PS:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PATHST  "

(DEFUN STRING-OR-WILD (FIELD &OPTIONAL NO-QUOTE-P SPECIALS REPLACED-BY)
  "Convert FIELD, a pathname component, to a string to appear in a printed representation.
NO-QUOTE-P inhibits insertion of quoting characters;
otherwise, quote characters are inserted and some characters translated:
SPECIALS is a list of characters to be translated,
and REPLACED-BY is an equally-long list of characters to translate them to."
  (COND ((EQ FIELD ':WILD) "*")
	((MEMQ FIELD '(NIL :UNSPECIFIC)) NIL)
	(NO-QUOTE-P (STRING FIELD))
	((QUOTE-COMPONENT-STRING FIELD SPECIALS REPLACED-BY))))

))
