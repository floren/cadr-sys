;;; -*- Mode: Lisp; Base: 8; Package: File-System; Readtable: ZL -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.

;; Old file copying stuff.
;;
;; Entrypoints:
;; FS-COPY-FILE from to &rest options
;; COPY-FILES files to &rest options
;; COPY-DIRECTORY from to &rest options
;; COPY-DIRECTORIES directories to &rest options
;; COPY-SOURCE-FILES-OF-SYSTEM system to &rest options
;; COPY-PATCH-FILES patchable-system to &rest options
;; COPY-INSTALLED-FILES to &rest options

;; The big problem here is that PDP-10 file servers don't know what byte-size their files
;; are suposed to be, and it makes a difference when opening them.  So the hairiest
;; part here is trying to determine this.

;; These types are ASCII files if all else fails.
(DEFVAR CHARACTER-FILE-TYPES '("LISP" "LSP" "TEXT" "TXT" "INIT" "INI" "CMD"
			       "MID" "AST" "LOGIN" "LISPM" "(PDIR)" "DOC" "WLR" "XGP"
			       "-THIS-" "ULOAD" "PROM" "WORDAB" "MAIL" "OMAIL" "RMAIL"))
;; These types are binary (byte-size = 16) if all else fails.
(DEFVAR BINARY-FILE-TYPES '("PRESS"))
;; These types are pdp10 (byte-size = 36) if all else fails.
(DEFVAR PDP10-FILE-TYPES '("KST" "LREC" "FASL" "BIN"))

;;  This variable doesn't seem to be used.
(DEFVAR *DONT-COPY-FILE-TYPES* '("XGP" "FASL" "KST" "LREC" "PRESS" "MAIL" "OMAIL" "RMAIL"))

;; FS-COPY-FILE file destination &rest options
;; This is the basic entry function which crufty MT programs use.
;; <file> is a pathname, string representing one, or a stream.
;;   It may not contain wildcard characters.  If a stream, 
;;   as of now it must handle :NAME, :TYPE, :VERSION, :CREATION-DATE, :QFASLP
;;   :BYTE-SIZE and :CHARACTERS messages even if the output file is fully specified.
;; <destination> is either
;;   another pathname, which is "merged" (by special rules) with the first,
;;   a string containing a colon is interpreted as another pathname,
;;   any other string oe symbol is interpreted as a host name.  This is what's normally
;;   done.  Note that "MT" specifies that you should copy the file onto magtape.
;; options:
;;   :DIRECTORY-LIST <list> is the list FOR THIS FILE handed back from :DIRECTORY-LIST.
;;      This speeds things up quite a bit, because it gives vital information about
;;      byte size and creation date.  If this is present, the pathname given
;;      is assumed to be the truename.
;;   :OUTPUT-DIRECTORY-LIST <list> is the entire directory list for the directory
;;      the copy is going to.  Do not specify this without :DIRECTORY-LIST.
;;      If this is specified, there is no need to probe the output file.
;;   :DEFAULT-BYTE-SIZE <n> Use this bytesize if all else fails
;;   :OVERWRITE <flag>      Copy even if the same file already exists.  Default is NIL.
;;   :VERBOSE <flag>        Print each copy operation and result.  Default is T.
;;   :DELETE <flag>         Delete <file> after copying it to destination.
;;   :AFTER <univ-time>     Only copy the file if it was created after the time specified.
;; Other options are ignored, and are presumably used elsewhere...


;; Returns T if the file was copied normally,
;; :ALREADY-EXISTS if the file wasn't copied because it wasn't necessary,
;; :AFTER if the file wasn't copied due to an :AFTER spec.
;; or an error string returned by an open operation,

(DEFVAR COPY-LOSSES ())
(DEFVAR COPY-OVERS ())

;; Copied from LAD: RELEASE-3.TAPE; COPY.LISP#162 on 2-Oct-86 04:32:25
;renamed from COPY-FILE to avoid conflict with system function.
(DEFUN FS-COPY-FILE (FROM TO &REST OPTIONS &KEY &OPTIONAL OVERWRITE (VERBOSE T) DELETE AFTER
		     DIRECTORY-LIST DEFAULT-BYTE-SIZE
		     OUTPUT-DIRECTORY-LIST (CREATE-DIRECTORY T)
		     &ALLOW-OTHER-KEYS &AUX
		     TRUE-BYTE-SIZE KNOWN-BYTE-SIZE TRUE-CHARACTERS
		     TRUENAME OUTNAME OUTNAME-UNCERTAIN
		     TYPE QFASLP INSTREAM OUTSTREAM (ABORT-FLAG ':ABORT)
		     FROM-IS-STREAM-P TO-DEFAULTS-FROM-STREAM AUTHOR TEM
		     OUT-OPEN-ERROR)
  (*CATCH 'COPY-FILE
    (UNWIND-PROTECT
	(PROG ()
	      (COND ((STRINGP FROM))
		    ((TYPEP FROM 'PATHNAME))
		    ((SI:IO-STREAM-P FROM)
		     (SETQ FROM-IS-STREAM-P T)
		     (SETQ INSTREAM FROM)))
	      
	      ;; If possible, get the byte size from the directory info.
	      (COND (DIRECTORY-LIST
		     (IF (NULL FROM-IS-STREAM-P) (SETQ TRUENAME FROM))
		     (SETF (VALUES OUTNAME OUTNAME-UNCERTAIN TO-DEFAULTS-FROM-STREAM)
			   (DETERMINE-COPY-DESTINATION TO TRUENAME NIL INSTREAM))
		     ;; Punt now if :AFTER specification is not met.
		     (AND AFTER
			  ( (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
			     AFTER)
			  (RETURN ':AFTER))
		     ;; Verbosify after calling DETERMINE-COPY-DESTINATION.
		     (IF VERBOSE (FORMAT T "~%~A~23T ~A~50T"
					 (IF FROM-IS-STREAM-P "" TRUENAME) OUTNAME))
		     ;; If we are sure we know the destination name,
		     ;; and we have an output directory list, check now
		     ;; in case we don't need to copy.
		     (OR TO-DEFAULTS-FROM-STREAM
			 OUTNAME-UNCERTAIN
			 (AND OUTPUT-DIRECTORY-LIST
			      (LET ((DESTEX (COPY-DESTINATION-EXISTS-P
					      OVERWRITE OUTPUT-DIRECTORY-LIST
					      OUTNAME TRUENAME VERBOSE
					      (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
						  (FUNCALL INSTREAM ':CREATION-DATE)))))
				(AND DESTEX (RETURN DESTEX)))))
		     (LET ((CHRLOC (LOCF (GET (LOCF DIRECTORY-LIST) ':CHARACTERS))))
		       (AND CHRLOC (SETQ TRUE-CHARACTERS (CDR CHRLOC))))
		     ;; Take :DIRECTORY-LIST information with a grain of salt...
		     ;; Note that we are assuming here that the files are used for LISPMs...
		     (LET ((POSSIBLE-BYTE-SIZE (GET (LOCF DIRECTORY-LIST) ':BYTE-SIZE)))
		       (AND POSSIBLE-BYTE-SIZE
			    (COND ((EQ POSSIBLE-BYTE-SIZE 7.)
				   (SETQ TRUE-BYTE-SIZE 8.))
				  ((NEQ POSSIBLE-BYTE-SIZE 36.)
				   (SETQ TRUE-BYTE-SIZE POSSIBLE-BYTE-SIZE)))))))
	      
	      ;; Next try opening the file.
	      (COND ((NULL FROM-IS-STREAM-P)
		     (SETQ INSTREAM (OPEN FROM ':CHARACTERS (OR TRUE-CHARACTERS ':DEFAULT)
					  ':BYTE-SIZE (OR TRUE-BYTE-SIZE ':DEFAULT)
					  ':ERROR NIL))
		     (COND ((ERRORP INSTREAM)
			    (AND VERBOSE (FORMAT T "~%~A~50T~A" FROM INSTREAM))
			    (RETURN INSTREAM)))))
	      
	      ;; Punt now if :AFTER specification is not met.
	      (AND AFTER
		   ( (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
			  (FUNCALL INSTREAM ':CREATION-DATE)
			  (FERROR NIL "Bletch!!"))
		      AFTER)
		   (RETURN ':AFTER))
	      
	      (IF (NULL FROM-IS-STREAM-P)
		  (SETQ TRUENAME (FUNCALL INSTREAM :send-if-handles :TRUENAME)))
	      (SETQ QFASLP (FUNCALL INSTREAM ':QFASLP))
	      
	      ;; Now determine the destination if not done already.
	      (IF (OR (NULL OUTNAME) OUTNAME-UNCERTAIN)
		  (PROGN
		    (MULTIPLE-VALUE (OUTNAME TEM TO-DEFAULTS-FROM-STREAM)
		      (DETERMINE-COPY-DESTINATION TO TRUENAME QFASLP INSTREAM))
		    (AND VERBOSE (FORMAT T "~%~A~23T ~A~50T" TRUENAME OUTNAME))))
	      
	      ;; Does the output file already exist?  Is its date the same?
	      ;; Check now if we didn't check before.
	      (AND (NULL TO-DEFAULTS-FROM-STREAM)
		   (OR OUTNAME-UNCERTAIN (NOT OUTPUT-DIRECTORY-LIST))
		   (LET ((DESTEX (COPY-DESTINATION-EXISTS-P
				   OVERWRITE OUTPUT-DIRECTORY-LIST OUTNAME TRUENAME VERBOSE
				   (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
				       (FUNCALL INSTREAM ':CREATION-DATE)))))
		     (when DESTEX
		       (setq abort-flag nil)	;don't close in abort mode.
		       (RETURN DESTEX))))
	      
	      ;; If we knew the byte size before opening the stream, remember that fact.
	      (SETQ KNOWN-BYTE-SIZE TRUE-BYTE-SIZE)
	      
	      (SETQ TYPE (IF (null truename)
			     (FUNCALL INSTREAM ':TYPE)
			   (FUNCALL TRUENAME ':TYPE)))
	      (OR TRUE-BYTE-SIZE
		  ;; If stream knows its proper byte size, believe it.  QFILE streams don't.
		  (AND (SETQ TRUE-BYTE-SIZE (FUNCALL INSTREAM ':SEND-IF-HANDLES ':BYTE-SIZE))
		       ;; If it knows that, it also did :characters :default properly.
		       (PROGN (SETQ TRUE-CHARACTERS (FUNCALL INSTREAM ':CHARACTERS)) T))
		  ;; Otherwise guess.
		  (SETQ TRUE-BYTE-SIZE
			(COND ((or QFASLP
				   (MEMBER TYPE BINARY-FILE-TYPES)
				   (string-search "QFASL" type))
						;temporary kludgery, because QFASLP is ALWAYS nil.
						; I will fix it later. This function should eventualy
						; get thrown away. -dg
			       16.)
			      ((MEMBER TYPE PDP10-FILE-TYPES)
			       9)
			      ((FILE-EXTRACT-ATTRIBUTE-LIST INSTREAM)
			       8)
			      ((OR (MEMQ TYPE '(NIL :UNSPECIFIC))
				   (MEMBER TYPE CHARACTER-FILE-TYPES))
			       8)
			      (DEFAULT-BYTE-SIZE)
			      ((Y-OR-N-P (FORMAT NIL "~%Is ~A a CHARACTER File? " TRUENAME))
			       8)
			      (T 16.))))
	      (OR TRUE-CHARACTERS
		  (SETQ TRUE-CHARACTERS (= TRUE-BYTE-SIZE 8)))
	      (When verbose
		(FORMAT T "~%Byte size ~D, Characters ~S" TRUE-BYTE-SIZE TRUE-CHARACTERS))
	      
	      ;; If stream is open in wrong byte size or with wrong :characters, reopen it.
	      (OR FROM-IS-STREAM-P
		  (AND
		    (OR KNOWN-BYTE-SIZE
			(= TRUE-BYTE-SIZE
			   (OR (FUNCALL INSTREAM ':SEND-IF-HANDLES ':BYTE-SIZE)
			       (IF (FUNCALL INSTREAM ':CHARACTERS) 8 16.))))
		    (EQ TRUE-CHARACTERS (FUNCALL INSTREAM ':CHARACTERS)))
		  (PROGN (PRINC " -- Must reopen stream" *ERROR-OUTPUT*)
			 (CLOSE INSTREAM)
			 (SETQ INSTREAM (OPEN TRUENAME ':ERROR NIL
					      ':BYTE-SIZE TRUE-BYTE-SIZE
					      ':CHARACTERS (= TRUE-BYTE-SIZE 8)))
			 (COND ((ERRORP INSTREAM)
				(AND VERBOSE (FORMAT T "~%~A~50T~A" FROM INSTREAM))
				(RETURN INSTREAM)))))
	      
	      (SETQ AUTHOR
		    (OR (GET (LOCF DIRECTORY-LIST) :AUTHOR)
			(FUNCALL INSTREAM :GET :AUTHOR)
			(IF (NULL FROM-IS-STREAM-P)
			    (DETERMINE-FILE-AUTHOR (FUNCALL INSTREAM :TRUENAME)))
			"Unknown"))
	   OPEN-OUTPUT
	      ;; Do It.
	      (COND ((ERRORP
		       (SETQ OUTSTREAM
			     (COND (TO-DEFAULTS-FROM-STREAM
				    (LEXPR-FUNCALL OUTNAME ':OPEN OUTNAME
						   ':DIRECTION ':OUTPUT
						   ':ERROR (or (typep outname 'mt-filehandle)
							       OUT-OPEN-ERROR)
						   ':CHARACTERS TRUE-CHARACTERS
						   ':BYTE-SIZE TRUE-BYTE-SIZE
						   ':DEFAULTS-FROM-STREAM INSTREAM
						   ':AUTHOR AUTHOR
						   (cond ((memq ':record-size options)
							  `(:record-size ,(get (locf options)
									       ':record-size)))
							 (t nil))))
				   (T
				    (OPEN OUTNAME
					  (COND ((EQ TRUE-BYTE-SIZE 8.)
						 `(:WRITE ,@(IF out-open-error
								() '(:NOERROR))))
						((EQ TRUE-BYTE-SIZE 16.)
						 `(:WRITE :FIXNUM ,@(IF out-open-error
									() '(:NOERROR))))
						(T `(:WRITE ,@(IF out-open-error
								() '(:NOERROR))
							    :BYTE-SIZE ,TRUE-BYTE-SIZE)))
					  )))))
		     (AND CREATE-DIRECTORY
			  (NOT (ERRORP (CREATE-DIRECTORY OUTNAME ':ERROR NIL)))
			  (setq out-open-error t)
			  (GO OPEN-OUTPUT))
		     (AND VERBOSE (FUNCALL OUTSTREAM ':REPORT STANDARD-OUTPUT))
		     (RETURN OUTSTREAM)))
	      
	      ;; This now hacks arbitrary property stuff...
	      (IF TO-DEFAULTS-FROM-STREAM NIL
		(FUNCALL OUTSTREAM ':CHANGE-PROPERTIES NIL
			 ':AUTHOR AUTHOR
			 ':CREATION-DATE (FUNCALL INSTREAM ':GET ':CREATION-DATE))
		(LOOP WITH other-properties = (or directory-list
						  (copylist (funcall instream ':property-list)))
		      AS remove-properties = (funcall outstream ':property-list)
		      THEN (cddr remove-properties)
		      WHILE (and remove-properties other-properties)
		      DO
		      (remprop (locf other-properties) (car remove-properties))
		      FINALLY
		      (dolist (p '(:directory :name :version :type))
			(remprop (locf other-properties) p))
		      FINALLY 
		      (cond (other-properties
			     (lexpr-funcall outstream ':CHANGE-PROPERTIES NIL
					    other-properties)))))
	      (STREAM-COPY-UNTIL-EOF INSTREAM OUTSTREAM NIL)
	      (SETQ ABORT-FLAG NIL))
      
      (unwind-protect
	  (OR (NULL OUTSTREAM) (ERRORP OUTSTREAM)
	      (FUNCALL OUTSTREAM ':CLOSE ABORT-FLAG))
	(OR (NULL INSTREAM) (ERRORP INSTREAM)
	    (PROGN (AND (NOT ABORT-FLAG)
			DELETE
			(FUNCALL INSTREAM ':SEND-IF-HANDLES ':DELETE NIL))
		   (FUNCALL INSTREAM ':CLOSE ABORT-FLAG)))))))

;Three values: first: output-pathname or file-handle; second: T if output-pathname
;might change if QFASLP or INSTREAM were to change.  The third is non-NIL if
;the first value handles defaulting from streams.  In that case, the destination
;stream should be produced simply by sending the first value a :OPEN message
;with :DEFAULTS-FROM-STREAM <stream> keywords.
;In this latter case the file should be opened by directly calling the filehandle
;with an :OPEN message, giving the keywords :DEFAULTS-FROM-STREAM <stream>.
;TRUENAME may be null if source is simply a stream.  In that case it could
; try digging info out of the stream, but we wont worry about that for now.
(DEFUN DETERMINE-COPY-DESTINATION (TO TRUENAME QFASLP INSTREAM)
  (LET (TYPE VERSION PLIST TEM NOT-CERTAIN)
    (AND (SYMBOLP TO)
	 (SETQ TO (GET-PNAME TO)))
    (COND ((TYPEP TO 'PATHNAME))
	  ((typep to 'mt-filehandle))
	  ((string-equal to "mt:")
	   (setq to (parse-pathname to)))
	  ((CAR (ERRSET (GET-PATHNAME-HOST TO) NIL))
	   (SETQ TO (MAKE-PATHNAME ':HOST TO ':DIRECTORY NIL ':NAME NIL)))
	  (T (SETQ TO (PARSE-PATHNAME TO))))
    (COND ((NOT (TYPEP TO 'PATHNAME))
	   (VALUES TO NIL 'DEFAULTS-FROM-STREAM))
	  ((NULL TRUENAME)
	   (LET ((NAME (FUNCALL TO ':NAME)))
	     (COND ((NULL NAME)
		    (SETQ NAME (FUNCALL INSTREAM ':NAME))
		    (AND (LISTP NAME) (NULL (CDR NAME))
			 (SETQ NAME (CAR NAME)))))
	     (COND ((MEMQ (SETQ TYPE (FUNCALL TO ':TYPE)) '(NIL :UNSPECIFIC))
		    (SETQ TYPE (FUNCALL INSTREAM ':TYPE))))
	     (COND ((SYMBOLP (SETQ VERSION (FUNCALL TO ':VERSION)))
		    (SETQ VERSION (FUNCALL INSTREAM ':VERSION))))
	     (VALUES
	       (MAKE-PATHNAME ':HOST (FUNCALL TO ':HOST)
			      ':DEVICE (OR (FUNCALL TO ':DEVICE)
					   (FUNCALL INSTREAM ':DEVICE))
			      ':DIRECTORY (OR (FUNCALL TO ':DIRECTORY)
					      (FUNCALL INSTREAM ':DIRECTORY))
			      ':NAME NAME
			      ':TYPE TYPE
			      ':VERSION VERSION)
	       NIL)))
	  (T
	   ;Make sure wildcards get properly converted, as we should know the 
	   ;correct new name and type from TRUENAME
	   (cond-every
	     ((memq (send to :name) '(:wild :unspecific))
	      (setq to (send to :new-name (send truename :name))))
	     ((memq (send to :type) '(:wild :unspecific))
	      (setq to (send to :new-type (send truename :type)))))
	   (OR (NOT (MEMQ (SETQ TYPE (FUNCALL TO ':TYPE)) '(NIL :UNSPECIFIC)))
	       (NOT (MEMQ (SETQ TYPE (FUNCALL TRUENAME ':TYPE)) '(NIL :UNSPECIFIC)))
	       ;; These do not distinguish types LISP and TEXT.
	       (TYPEP TO 'ITS-PATHNAME)
	       (AND (GET 'LOCAL-FILE-PATHNAME 'SI:FLAVOR)
		    (TYPEP TO 'LOCAL-FILE-PATHNAME))
	       (AND (GET 'REMOTE-LMFILE-PATHNAME 'SI:FLAVOR)
		    (TYPEP TO 'REMOTE-LMFILE-PATHNAME))
	       (SETQ NOT-CERTAIN T))
	   ;;; I excised this so that this function would pass through
	   ;;; :UNSPECIFIC or NIL :TYPE components, since it is not
	   ;;; really correct to change the type.  (and makes files written
	   ;;; to tape be not pathname-equal to their disk file sources.)
	   ;;; I don't know how this will effect other types of file
	   ;;; systems, but right now it needs to work right for ours
	   ;;; and problems with other fs's should be filtered out
	   ;;; before this point. -dg 2/4/85
	   ;;;   vvv this went inside the SETQ above.
	   ;;;   TYPE
	   ;;;   (OR (AND QFASLP "QFASL")
	   ;;;	 (AND INSTREAM
	   ;;;            (SETQ PLIST (FILE-READ-ATTRIBUTE-LIST NIL INSTREAM))
	   ;;;	      (EQ (GET (LOCF PLIST) ':MODE) ':LISP)
	   ;;;	      "LISP")
	   ;;;	 "TEXT")))
	   (OR (NOT (SYMBOLP (SETQ VERSION (FUNCALL TO ':VERSION))))
	       (NOT (SYMBOLP (SETQ VERSION (FUNCALL TRUENAME ':VERSION))))
	       (COND (QFASLP
		      (SETQ PLIST (SI:QFASL-STREAM-PROPERTY-LIST INSTREAM))
		      (FUNCALL INSTREAM ':SET-POINTER 0)
		      (COND ((SETQ TEM (GET (LOCF PLIST) ':QFASL-SOURCE-FILE-UNIQUE-ID))
			     (IF (LISTP TEM)
				 (SETQ VERSION (CAR (LAST TEM)))
			       (SETQ VERSION (FUNCALL TEM ':VERSION))))
			    (T (SETQ VERSION ':NEWEST))))
		     (T (SETQ NOT-CERTAIN T VERSION ':NEWEST))))
	   (LET ((INNAME (FUNCALL TRUENAME ':NAME)))
	     (AND (LISTP INNAME) (NULL (CDR INNAME))
		  (SETQ INNAME (CAR INNAME)))
	     (VALUES
	       (MAKE-PATHNAME ':HOST (FUNCALL TO ':HOST)
			      ':DEVICE (OR (FUNCALL TO ':DEVICE)
					   (FUNCALL TRUENAME ':DEVICE))
			      ':DIRECTORY (OR (FUNCALL TO ':DIRECTORY)
					      (FUNCALL TRUENAME ':DIRECTORY))
			      ':NAME (OR (FUNCALL TO ':NAME)
					 INNAME)
			      ':TYPE TYPE
			      ':VERSION VERSION)
	       NOT-CERTAIN))))))

;Return T if we should not copy this file because an output file already exists.
;Also prints message if appropriate.
(DEFUN COPY-DESTINATION-EXISTS-P (OVERWRITE OUTPUT-DIRECTORY-LIST
				  OUTNAME TRUENAME VERBOSE INDATE)
  (LET (OUTPROBE)
    (AND (NEQ OVERWRITE ':ALWAYS)
	(LET (OUTCRDATE OUTEX)
	  ;; Take note of the fact that an LMFILE pathname with a "type"
	  ;; won't be found as a truename because the truename will have a space.
	  (AND (OR (AND (GET 'LOCAL-FILE-PATHNAME 'SI:FLAVOR)
			(TYPEP OUTNAME 'LOCAL-FILE-PATHNAME))
		   (AND (GET 'REMOTE-LMFILE-PATHNAME 'SI:FLAVOR)
			(TYPEP OUTNAME 'REMOTE-LMFILE-PATHNAME)))
	       (FUNCALL OUTNAME ':TYPE)
	       (SETQ OUTPUT-DIRECTORY-LIST NIL))
	  (IF OUTPUT-DIRECTORY-LIST
	      (PROGN (SETQ OUTEX (ASS #'(LAMBDA (X Y)
					  (AND Y
					       (STRING-EQUAL (FUNCALL X ':STRING-FOR-PRINTING)
							     (FUNCALL Y ':STRING-FOR-PRINTING))))
				      OUTNAME OUTPUT-DIRECTORY-LIST))
		     (SETQ OUTCRDATE (GET (LOCF (CDR OUTEX)) ':CREATION-DATE)))
	    (UNWIND-PROTECT
	     (PROGN
	      (SETQ OUTPROBE (OPEN OUTNAME '(:PROBE)))
	      (COND ((NOT (ERRORP OUTPROBE))
		     (SETQ OUTCRDATE (FUNCALL OUTPROBE ':CREATION-DATE))
		     (SETQ OUTEX T))))
	     (AND OUTPROBE (NOT (ERRORP OUTPROBE))
		  (CLOSE OUTPROBE))))
	  (IF OUTEX
	      (IF (= OUTCRDATE INDATE)
		  (COND ((NOT OVERWRITE)
			 (AND VERBOSE (FUNCALL STANDARD-OUTPUT ':STRING-OUT
					       "[Already Exists]"))
			 ':ALREADY-EXISTS))
		  (COND ((NEQ (PATHNAME-VERSION OUTNAME) ':NEWEST)
			 (AND VERBOSE (FUNCALL STANDARD-OUTPUT ':STRING-OUT
					       "[Different file exists at target]"))
			 (IF TRUENAME (PUSH TRUENAME COPY-LOSSES))
			 ':ALREADY-EXISTS)
			(T (IF TRUENAME (PUSH TRUENAME COPY-OVERS))
			   NIL))))))))

;; This is a really gross kludge...
(DEFVAR *HUMUNGOUS-DIRECTORY-LIST* ())
(DEFUN DETERMINE-FILE-AUTHOR (PATHNAME &AUX ENTRY)
  (COND ((NULL (SETQ ENTRY (ASSQ PATHNAME *HUMUNGOUS-DIRECTORY-LIST*)))
	 (SETQ *HUMUNGOUS-DIRECTORY-LIST*
	       (NCONC (DIRECTORY-LIST (FUNCALL PATHNAME ':NEW-PATHNAME
					       ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
		      *HUMUNGOUS-DIRECTORY-LIST*))
	 (SETQ ENTRY (ASSQ PATHNAME *HUMUNGOUS-DIRECTORY-LIST*))))
  (GET ENTRY ':AUTHOR))

(DEFUN EQ-CAR (X Y)
  (EQ (CAR X) (CAR Y)))

;; All multiple-file copying stuff accepts these options in addition to the others.
;; :COPY-ONLY <list>, where <list> contains some of the following keywords:
;;     :SOURCE  -- copies source "LISP" and "TEXT" files.
;;     :NEWEST  -- copies only the newest versions, where possible...
;;     :QFASL   -- copies "QFASL" files.
;; :SELECTIVE <flag>, asks about each pathname.
;; :CREATE-DIRECTORY (<flag> T), create directory if possible...

(DEFSUBST PARSE-MULTIPLE-FILE-KEYWORDS ()
  (DO ((O OPTIONS (CDDR O)))
      ((NULL O))
    (SELECTQ (CAR O)
      (:COPY-ONLY (SETQ COPY-ONLY (CADR O)))
      (:SELECTIVE (SETQ SELECTIVE (CADR O)))
      (:CREATE-DIRECTORY (SETQ CREATE-DIRECTORY (CADR O))))))

(DEFUNP CHECK-COPY-ONLY (FILE COPY-ONLY &OPTIONAL DIR-LIST TEM VERSION TYPE)
  (IF (SYMBOLP COPY-ONLY) (SETQ COPY-ONLY (LIST COPY-ONLY)))
  (SETQ VERSION (FUNCALL FILE ':VERSION)
	TYPE (FUNCALL FILE ':TYPE))
  (COND ((MEMQ ':NEWEST COPY-ONLY)
	 (AND (SYMBOLP VERSION)
	      (NOT (MEMQ VERSION '(:NEWEST NIL :UNSPECIFIC)))
	      (RETURN NIL))
	 (IF DIR-LIST
	     (OR (CHECK-FILE-NEWEST FILE VERSION DIR-LIST) (RETURN NIL))
	     (AND (NOT (ERRORP (SETQ TEM (OPEN (FUNCALL FILE ':NEW-VERSION ':NEWEST)
						'(:PROBE)))))
		  (NOT (EQUAL VERSION
			      (FUNCALL (FUNCALL TEM ':TRUENAME) ':VERSION)))
		  (RETURN NIL)))))
  (OR (MEMQ ':SOURCE COPY-ONLY) (MEMQ ':QFASL COPY-ONLY)
      (RETURN T))
  (RETURN
    (COND ((MEMBER TYPE '(NIL :UNSPECIFIC "LISP" "TEXT"))
	   (MEMQ ':SOURCE COPY-ONLY))
	  ((EQUAL TYPE "QFASL")
	   (MEMQ ':QFASL COPY-ONLY))
	  ((MEMQ ':SOURCE COPY-ONLY)	;if :SOURCE, copy random things not QFASL.
	   t))))

(DEFUN CHECK-FILE-NEWEST (FILE VERSION DIR-LIST &AUX NAME TYPE TEM)
  (SETQ NAME (FUNCALL FILE ':NAME)
	TYPE (FUNCALL FILE ':TYPE))
  (DO ((D DIR-LIST (CDR D)))
      ((NULL D) T)
    (AND (CAAR D)
	 (EQUAL NAME (FUNCALL (CAAR D) ':NAME))
	 (EQUAL TYPE (FUNCALL (CAAR D) ':TYPE))
	 (NUMBERP (SETQ TEM (FUNCALL (CAAR D) ':VERSION)))
	 (> TEM VERSION)
	 (RETURN NIL))))			;Found a newer one.

(DEFUN COPY-FILES (FILES TO &REST OPTIONS &AUX COPY-ONLY SELECTIVE (CREATE-DIRECTORY T) TEM)
  (PARSE-MULTIPLE-FILE-KEYWORDS)
  (DOLIST (FILE FILES)
    RETRY
    (WHEN (AND (OR (NOT SELECTIVE)
		   (PROGN (FORMAT QUERY-IO "Copy ~A ?" FILE)
			  (Y-OR-N-P)))
	       (OR (NULL COPY-ONLY)
		   (CHECK-COPY-ONLY FILE COPY-ONLY)))
      (WHEN (AND (ERRORP (SETQ TEM (APPLY #'FS-COPY-FILE FILE TO OPTIONS)))
		 (CONDITION-TYPEP TEM '(OR DIRECTORY-NOT-FOUND FILE-NOT-FOUND))
		 CREATE-DIRECTORY)
	(CREATE-DIRECTORY
	  (SEND (SEND (MERGE-PATHNAME-DEFAULTS FILE) :TRANSLATED-PATHNAME) :DIRECTORY))
	(GO RETRY)))))

(defun copy-directory-after-directory (DIR TO AFTER &OPTIONAL &REST OPTIONS
		       &KEY COPY-ONLY SELECTIVE (RECOPY-FILE-ON-EOT T) (copy-subdirectories t)
		       FILTER ONLY-LATEST (SINCE 0)
		       &ALLOW-OTHER-KEYS &aux gobbling)
  (apply 'copy-directory dir to
	 :filter (function (lambda (f)
			     (if gobbling t
			       (let* ((p (car f))
				      (d (send p :directory)))
				 (cond ((if (stringp d)
					    (string-equal after d)
					  (dolist (de d)
					    (if (string-equal after de)
						(return t))))
					(format t "~%Dumping starting now.")
					(setq gobbling t)))))))

	 options))

(DEFUN COPY-DIRECTORY (DIR TO &OPTIONAL &REST OPTIONS
		       &KEY COPY-ONLY SELECTIVE (RECOPY-FILE-ON-EOT T) (copy-subdirectories t)
		       FILTER ONLY-LATEST (SINCE 0) (FUNCTION #'FS-COPY-FILE)
		       &ALLOW-OTHER-KEYS
		       &AUX WHOLE-DIR-LIST OUTPUT-DIR-LIST TAPE-DUMP FILE)
  "A utility for copying directories.
Options:
 :COPY-ONLY     One of NIL (default), :SOURCE, :NEWEST, or :QFASL.
 :SELECTIVE     One of NIL (default) or T. If not NIL, asks whether to copy each element.
 :COPY-SUBDIRECTORIES 
                One of NIL, T (default), or :WILD meaning to use wild name, type, and version.
 :FILTER        One of NIL (default) or a function to be called with the directory
                  element as its single argument.  If value returned by function is 
                  non-NIL, the element is copied.
 :ONLY-LATEST   One of NIL (default) or T. If not NIL, force :NEWEST from the source directory.
 :SINCE date    Copy only files created after date, a string or Universal Time (an integer)."
  (SETQ DIR (PARSE-PATHNAME DIR))
  (SETQ DIR (SEND DIR :NEW-PATHNAME
		  :NAME (IF (MEMQ (SEND DIR :NAME) '(NIL :UNSPECIFIC))
			    :WILD
			  (SEND DIR :NAME))
		  :TYPE (IF (MEMQ (SEND DIR :TYPE) '(NIL :UNSPECIFIC))
			    :WILD
			  (SEND DIR :TYPE))
		  :VERSION (IF ONLY-LATEST :NEWEST
			     (IF (MEMQ (SEND DIR :VERSION) '(NIL :UNSPECIFIC))
				 :WILD
			       (SEND DIR :VERSION)))))
  (UNLESS (TYPEP TO 'MT-FILEHANDLE) (SETQ TO (PARSE-PATHNAME TO)))
  (UNLESS (TYPEP TO '(OR PATHNAME MT-FILEHANDLE))
    (SETQ TO (MERGE-PATHNAME-DEFAULTS DIR :WILD :WILD)))
  (UNLESS (NUMBERP SINCE) (SETQ SINCE (TIME:PARSE-UNIVERSAL-TIME SINCE)))
  (TYPECASE TO
    (MT-FILEHANDLE (SETQ TAPE-DUMP T))  ;must test first, since is a subtype of pathname.
    (PATHNAME
     (DOLIST (PART '(:DIRECTORY :NAME :TYPE))
       (LET ((COMPONENT (send TO PART))) ; Yow -- sending a non-constant message !
	 (IF (MEMQ COMPONENT '(nil :unspecific))
	     (SETQ TO (SEND TO :NEW-PATHNAME PART :WILD)))))
     (SETQ TO (SEND TO :NEW-VERSION :WILD))) ; must always be :WILD for accuracy in dir list
    )
  (SETQ WHOLE-DIR-LIST (DIRECTORY-LIST DIR))
  (IF (AND (NOT TAPE-DUMP)
	   (TYPEP TO 'PATHNAME))
      (ERRSET (SETQ OUTPUT-DIR-LIST (DIRECTORY-LIST TO))
	      NIL))
  (DOLIST (F WHOLE-DIR-LIST)
    (AND (SETQ FILE (CAR F))
	 (NOT (GET F :LINK-TO))
	 (OR (NOT SELECTIVE) (Y-OR-N-P "Copy ~A ?" FILE))
	 (IF (get f :directory)
	     (WHEN copy-subdirectories
	       (let* ((d (send file :directory))
		      (SUBDIRECTORY (cond ((CLI:LISTP D)
					   (APPEND D (NCONS (SEND file :NAME))))
					  ((eq d ':root)
					   (send file :name))
					  (t (LIST D (send file :NAME))))))
		 (APPLY 'copy-directory
			(IF (EQ COPY-SUBDIRECTORIES :WILD)
			    (SEND file :new-pathname
				  :directory SUBDIRECTORY :name :wild :type :wild
				  :version (SEND DIR :VERSION))
			  (SEND DIR :NEW-DIRECTORY SUBDIRECTORY))
			(if (and (not tape-dump) (typep to 'pathname))
			    (SEND TO :NEW-DIRECTORY
				  (let ((olddir (send to :directory))
					(newdir (send file :name)))
				    (cond ((consp olddir)
					   (append olddir (ncons newdir)))
					  ((eq olddir :root)
					   newdir)
					  (t
					   (list olddir newdir)))))
			  TO)
			options)))
	   (WHEN (AND (OR (NULL COPY-ONLY)
			  (CHECK-COPY-ONLY FILE COPY-ONLY WHOLE-DIR-LIST))
		      (> (GET F :CREATION-DATE) SINCE)
		      (OR (NULL FILTER) (FUNCALL FILTER F)))
	     (PROGV (IF RECOPY-FILE-ON-EOT '(*MT-EOT-HANDLER*) NIL)
		    '(COPY-EOT-HANDLER)
	       (DO ((V) (EOT))
		   (())
		 (MULTIPLE-VALUE (V EOT)
		   (*CATCH (IF RECOPY-FILE-ON-EOT :EOT :NEVER)
		     (APPLY FUNCTION FILE TO
			    :DIRECTORY-LIST (CDR F) :OUTPUT-DIRECTORY-LIST OUTPUT-DIR-LIST
			    OPTIONS)))
		 (IF (NULL EOT)
		     (RETURN)
		   (COPY-MOUNT-NEXT-TAPE TAPE-DUMP)))))))))

(DEFUN COMPARE-DIRECTORY  (DIR TO &OPTIONAL &REST OPTIONS
		       &KEY COPY-ONLY SELECTIVE (RECOPY-FILE-ON-EOT T) (copy-subdirectories t)
		       FILTER ONLY-LATEST (SINCE 0) (FUNCTION #'FS-COMPARE-FILE)
		       &ALLOW-OTHER-KEYS)
  (APPLY 'FS-COPY-DIRECTORY DIR TO :FUNCTION FUNCTION OPTIONS))

(DEFUN FS-COMPARE-FILE (F1 F2 &REST OPTIONS &KEY &OPTIONAL (VERBOSE T) AFTER
		     DIRECTORY-LIST	;F1
		     DEFAULT-BYTE-SIZE
		     OUTPUT-DIRECTORY-LIST ;F2
		     &ALLOW-OTHER-KEYS &AUX
		     TRUE-BYTE-SIZE1 KNOWN-BYTE-SIZE1 TRUE-CHARACTERS1
		     TRUENAME1 OUTNAME OUTNAME-UNCERTAIN
		     TYPE QFASLP
		     INSTREAM1 INSTREAM2 (ABORT-FLAG ':ABORT)
		     F1-IS-STREAM-P TO-DEFAULTS-FROM-STREAM TEM)
  (BREAK "INCOMPLETE")
  (*CATCH 'COMPARE-FILE
    (UNWIND-PROTECT
	(PROG ()
	      (COND ((STRINGP F1))
		    ((TYPEP F1 'PATHNAME))
		    ((SI:IO-STREAM-P F1)
		     (SETQ F1-IS-STREAM-P T)
		     (SETQ INSTREAM1 F1)))
	      
	      ;; If possible, get the byte size from the directory info.
	      (COND (DIRECTORY-LIST
		     (IF (NULL F1-IS-STREAM-P) (SETQ TRUENAME1 F1))
		     (SETF (VALUES OUTNAME OUTNAME-UNCERTAIN TO-DEFAULTS-FROM-STREAM)
			   (DETERMINE-COPY-DESTINATION F2 TRUENAME1 NIL INSTREAM1))
		     ;; Punt now if :AFTER specification is not met.
		     (AND AFTER
			  ( (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
			     AFTER)
			  (RETURN ':AFTER))
		     ;; Verbosify after calling DETERMINE-COPY-DESTINATION.
		     (IF VERBOSE (FORMAT T "~%~A~23T ~A~50T"
					 (IF F1-IS-STREAM-P "" TRUENAME1) OUTNAME))
		     ;; If we are sure we know the destination name,
		     ;; and we have an output directory list, check now
		     ;; in case we don't need to copy.
		     (OR TO-DEFAULTS-FROM-STREAM
			 OUTNAME-UNCERTAIN
			 (AND OUTPUT-DIRECTORY-LIST
			      (LET ((DESTEX (COPY-DESTINATION-EXISTS-P
					      NIL OUTPUT-DIRECTORY-LIST
					      OUTNAME TRUENAME1 VERBOSE
					      (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
						  (FUNCALL INSTREAM1 ':CREATION-DATE)))))
				(AND DESTEX (RETURN DESTEX)))))
		     (LET ((CHRLOC (LOCF (GET (LOCF DIRECTORY-LIST) ':CHARACTERS))))
		       (AND CHRLOC (SETQ TRUE-CHARACTERS1 (CDR CHRLOC))))
		     ;; Take :DIRECTORY-LIST information with a grain of salt...
		     ;; Note that we are assuming here that the files are used for LISPMs...
		     (LET ((POSSIBLE-BYTE-SIZE (GET (LOCF DIRECTORY-LIST) ':BYTE-SIZE)))
		       (AND POSSIBLE-BYTE-SIZE
			    (COND ((EQ POSSIBLE-BYTE-SIZE 7.)
				   (SETQ TRUE-BYTE-SIZE1 8.))
				  ((NEQ POSSIBLE-BYTE-SIZE 36.)
				   (SETQ TRUE-BYTE-SIZE1 POSSIBLE-BYTE-SIZE)))))))
	      
	      ;; Next try opening the file.
	      (COND ((NULL F1-IS-STREAM-P)
		     (SETQ INSTREAM1 (OPEN F1 ':CHARACTERS (OR TRUE-CHARACTERS1 ':DEFAULT)
					   ':BYTE-SIZE (OR TRUE-BYTE-SIZE1 ':DEFAULT)
					  ':ERROR NIL))
		     (COND ((ERRORP INSTREAM1)
			    (AND VERBOSE (FORMAT T "~%~A~50T~A" F1 INSTREAM1))
			    (RETURN INSTREAM1)))))
	      
	      ;; Punt now if :AFTER specification is not met.
	      (AND AFTER
		   ( (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
			  (FUNCALL INSTREAM1 ':CREATION-DATE)
			  (FERROR NIL "Bletch!!"))
		      AFTER)
		   (RETURN ':AFTER))
	      
	      (IF (NULL F1-IS-STREAM-P)
		  (SETQ TRUENAME1 (FUNCALL INSTREAM1 :send-if-handles :TRUENAME)))
	      (SETQ QFASLP (FUNCALL INSTREAM1 ':QFASLP))
	      
	      ;; Now determine the destination if not done already.
	      (IF (OR (NULL OUTNAME) OUTNAME-UNCERTAIN)
		  (PROGN
		    (MULTIPLE-VALUE (OUTNAME TEM TO-DEFAULTS-FROM-STREAM)
		      (DETERMINE-COPY-DESTINATION F2 TRUENAME1 QFASLP INSTREAM1))
		    (AND VERBOSE (FORMAT T "~%~A~23T ~A~50T" TRUENAME1 OUTNAME))))
	      
	      ;; Does the output file already exist?  Is its date the same?
	      ;; Check now if we didn't check before.
	      (AND (NULL TO-DEFAULTS-FROM-STREAM)
		   (OR OUTNAME-UNCERTAIN (NOT OUTPUT-DIRECTORY-LIST))
		   (LET ((DESTEX (COPY-DESTINATION-EXISTS-P
				   NIL OUTPUT-DIRECTORY-LIST OUTNAME TRUENAME1 VERBOSE
				   (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
				       (FUNCALL INSTREAM1 ':CREATION-DATE)))))
		     (when DESTEX
		       (setq abort-flag nil)	;don't close in abort mode.
		       (RETURN DESTEX))))
	      
	      ;; If we knew the byte size before opening the stream, remember that fact.
	      (SETQ KNOWN-BYTE-SIZE1 TRUE-BYTE-SIZE1)
	      
	      (SETQ TYPE (IF (null truename1)
			     (FUNCALL INSTREAM1 ':TYPE)
			   (FUNCALL TRUENAME1 ':TYPE)))
	      (OR TRUE-BYTE-SIZE1
		  ;; If stream knows its proper byte size, believe it.  QFILE streams don't.
		  (AND (SETQ TRUE-BYTE-SIZE1 (FUNCALL INSTREAM1 ':SEND-IF-HANDLES ':BYTE-SIZE))
		       ;; If it knows that, it also did :characters :default properly.
		       (PROGN (SETQ TRUE-CHARACTERS1 (FUNCALL INSTREAM1 ':CHARACTERS)) T))
		  ;; Otherwise guess.
		  (SETQ TRUE-BYTE-SIZE1
			(COND ((or QFASLP
				   (MEMBER TYPE BINARY-FILE-TYPES)
				   (string-search "QFASL" type))
						;temporary kludgery, because QFASLP is ALWAYS nil.
						; I will fix it later. This function should eventualy
						; get thrown away. -dg
			       16.)
			      ((MEMBER TYPE PDP10-FILE-TYPES)
			       9)
			      ((FILE-EXTRACT-ATTRIBUTE-LIST INSTREAM1)
			       8)
			      ((OR (MEMQ TYPE '(NIL :UNSPECIFIC))
				   (MEMBER TYPE CHARACTER-FILE-TYPES))
			       8)
			      (DEFAULT-BYTE-SIZE)
			      ((Y-OR-N-P (FORMAT NIL "~%Is ~A a CHARACTER File? " TRUENAME1))
			       8)
			      (T 16.))))
	      (OR TRUE-CHARACTERS1
		  (SETQ TRUE-CHARACTERS1 (= TRUE-BYTE-SIZE1 8)))
	      (FORMAT T "~%Byte size ~D, Characters ~S" TRUE-BYTE-SIZE1 TRUE-CHARACTERS1)
	      
	      ;; If stream is open in wrong byte size or with wrong :characters, reopen it.
	      (OR F1-IS-STREAM-P
		  (AND
		    (OR KNOWN-BYTE-SIZE1
			(= TRUE-BYTE-SIZE1
			   (OR (FUNCALL INSTREAM1 ':SEND-IF-HANDLES ':BYTE-SIZE)
			       (IF (FUNCALL INSTREAM1 ':CHARACTERS) 8 16.))))
		    (EQ TRUE-CHARACTERS1 (FUNCALL INSTREAM1 ':CHARACTERS)))
		  (PROGN (PRINC " -- Must reopen stream" *ERROR-OUTPUT*)
			 (CLOSE INSTREAM1)
			 (SETQ INSTREAM1 (OPEN TRUENAME1 ':ERROR NIL
					      ':BYTE-SIZE TRUE-BYTE-SIZE1
					      ':CHARACTERS (= TRUE-BYTE-SIZE1 8)))
			 (COND ((ERRORP INSTREAM1)
				(AND VERBOSE (FORMAT T "~%~A~50T~A" F1 INSTREAM1))
				(RETURN INSTREAM1)))))
	      
	   OPEN-OUTPUT
	      ;; Do It.
	      (COND ((ERRORP
		       (SETQ INSTREAM2
			     (COND (TO-DEFAULTS-FROM-STREAM
				    (LEXPR-FUNCALL OUTNAME ':OPEN OUTNAME
						   ':DIRECTION ':OUTPUT
						   ':ERROR NIL
						   ':CHARACTERS TRUE-CHARACTERS1
						   ':BYTE-SIZE TRUE-BYTE-SIZE1
						   ':DEFAULTS-FROM-STREAM INSTREAM1
						   (cond ((memq ':record-size options)
							  `(:record-size ,(get (locf options)
									       ':record-size)))
							 (t nil))))
				   (T
				    (OPEN OUTNAME
					  (COND ((EQ TRUE-BYTE-SIZE1 8.)
						 '(:WRITE :NOERROR))
						((EQ TRUE-BYTE-SIZE1 16.)
						 '(:WRITE :FIXNUM :NOERROR))
						(T `(:WRITE :NOERROR
							    :BYTE-SIZE ,TRUE-BYTE-SIZE1)))
					  )))))
		     (AND VERBOSE (FUNCALL INSTREAM2 ':REPORT STANDARD-OUTPUT))
		     (RETURN INSTREAM2)))
	      
	      ;; This now hacks arbitrary property stuff...
	      (IF TO-DEFAULTS-FROM-STREAM NIL
		(FUNCALL INSTREAM2 ':CHANGE-PROPERTIES NIL
			 ':CREATION-DATE (FUNCALL INSTREAM1 ':GET ':CREATION-DATE))
		(LOOP WITH other-properties = (or directory-list
						  (copylist (funcall instream1 ':property-list)))
		      AS remove-properties = (funcall INSTREAM2 ':property-list)
		      THEN (cddr remove-properties)
		      WHILE (and remove-properties other-properties)
		      DO
		      (remprop (locf other-properties) (car remove-properties))
		      FINALLY
		      (dolist (p '(:directory :name :version :type))
			(remprop (locf other-properties) p))
		      FINALLY 
		      (cond (other-properties
			     (lexpr-funcall INSTREAM2 ':CHANGE-PROPERTIES NIL
					    other-properties)))))
	      (STREAM-COPY-UNTIL-EOF INSTREAM1 INSTREAM2 NIL)
	      (SETQ ABORT-FLAG NIL))
      
      (OR (NULL INSTREAM2) (ERRORP INSTREAM2)
	  (FUNCALL INSTREAM2 ':CLOSE ABORT-FLAG))
      (OR (NULL INSTREAM1) (ERRORP INSTREAM1)
	  (FUNCALL INSTREAM1 ':CLOSE ABORT-FLAG)))))

(DEFUN COPY-EOT-HANDLER (&REST IGNORE)
  (*THROW ':EOT NIL))

(DEFUN COPY-MOUNT-NEXT-TAPE (TAPE-DUMP)
  (IF TAPE-DUMP (MT-WRITE-EOF))
  (MT-REWIND)
  (MT-OFFLINE)
  (BEEP)
  (FORMAT T "~&>>> Time to mount a new tape <<<")
  (READLINE))


(DEFUN COPY-DIRS (DIRS FROM TO &REST OPTIONS)
  (DOLIST (DIR DIRS)
    (LEXPR-FUNCALL #'COPY-DIRECTORY (MAKE-PATHNAME ':HOST FROM ':DIRECTORY DIR) TO OPTIONS)))


;;		 Do (SETQ DIRECTORY (FUNCALL DIRECTORY ':NEW-PATHNAME
;;					     ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
;;		    (LEXPR-FUNCALL #'COPY-DIRECTORY DIRECTORY TO OPTIONS)))))

#|
;; Special keyword
;; :SYSTEMS-FILTER <keyword>

(DEFUN COPY-SYSTEM (SYSTEM TO &REST OPTIONS)
  (LEXPR-FUNCALL #'COPY-FILES (SI:SYSTEM-SOURCE-FILES
				SYSTEM (OR (GET (LOCF OPTIONS) ':SYSTEMS-FILTER) ':ALL))
		 TO OPTIONS))

(DEFUN COPY-SYSTEMS (&OPTIONAL SYSTEMS (TO "LM") &REST OPTIONS)
  (OR SYSTEMS (SETQ SYSTEMS SI:*SYSTEMS-LIST*))
  (LET (FILES)
    (DOLIST (SYSTEM SYSTEMS)
      (SETQ FILES (APPEND FILES (SI:SYSTEM-SOURCE-FILES SYSTEM ':ALL))))
    (SETQ FILES (SI:ELIMINATE-DUPLICATES FILES))
    (LEXPR-FUNCALL #'COPY-FILES FILES TO OPTIONS)
    T))

(DEFUN COPY-PATCH-FILES (SYSTEMS FROM TO &REST OPTIONS)
  (IF (NULL SYSTEMS)
      (SETQ SYSTEMS SI:PATCH-SYSTEMS-LIST)
    (SETQ SYSTEMS (LOOP FOR X IN SYSTEMS
			AS SYS = (ASS #'STRING-EQUAL X SI:PATCH-SYSTEMS-LIST)
			WHEN SYS COLLECT SYS
			ELSE DO (FERROR NIL "No patch system ~S" X))))
  (LOOP FOR SYSTEM IN SYSTEMS
	DO (LEXPR-FUNCALL 'COPY-PATCH-FILES-OF-SYSTEM SYSTEM FROM TO OPTIONS)))

(DEFUN COPY-PATCH-FILES-OF-SYSTEM-VERSION (SYSTEM-LIST FROM TO &REST OPTIONS)
  (LET* ((PATCH-SYSTEM (SI:MAKE-PATCH-SYSTEM SI:NAME (CAR SYSTEM-LIST)
					     SI:VERSION (CADR SYSTEM-LIST)))
	 (PATCH-DIR (SI:READ-PATCH-DIRECTORY PATCH-SYSTEM))
	 (FIRST-VERS (FIRST (SI:PATCH-DIR-VERSION-LIST PATCH-DIR))))
    (OR (EQ (SI:VERSION-NUMBER FIRST-VERS) 0)
	(FERROR NIL "Patch directory for ~A messed up: ~S" SYSTEM-LIST FIRST-VERS))
    (SETF (SI:PATCH-STATUS PATCH-SYSTEM) (SI:PATCH-DIR-STATUS PATCH-DIR))
    (SETF (SI:PATCH-VERSION-LIST PATCH-SYSTEM)
	  (REVERSE (SI:PATCH-DIR-VERSION-LIST PATCH-DIR)))
    (LEXPR-FUNCALL 'COPY-PATCH-FILES-OF-SYSTEM PATCH-SYSTEM FROM TO OPTIONS)))
;--not a complete win since some info comes from running system


(DEFUN COPY-PATCH-FILES-OF-SYSTEM (SYSTEM FROM TO &REST OPTIONS
				   &KEY &OPTIONAL NEW-PATCHES-ONLY LATEST-VERSION
				   &ALLOW-OTHER-KEYS)
  (LOOP
	AS NAME = (SI:PATCH-NAME SYSTEM)
	AS PAT-DIR = (SI:SYSTEM-PATCH-DIRECTORY (SI:FIND-SYSTEM-NAMED NAME))
	AS IN-ACTOR = (TRANSLATE (SI:PATCH-DIRECTORY-PATHNAME PAT-DIR) FROM)
	AND OUT-ACTOR = (TRANSLATE (SI:PATCH-DIRECTORY-PATHNAME PAT-DIR) TO)
	AND SAME-DIR-P = (SI:PATCH-DIRECTORY-SAME-DIRECTORY-P PAT-DIR)
	AND PATOM = (SI:PATCH-DIRECTORY-PATCH-ATOM PAT-DIR)
	AND VER = (SI:PATCH-VERSION SYSTEM)
	DO (LEXPR-FUNCALL #'FS-COPY-FILE
			  (FUNCALL IN-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P PATOM
				   ':SYSTEM-DIRECTORY)
			  (FUNCALL OUT-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P PATOM
				   ':SYSTEM-DIRECTORY)
			  OPTIONS)
	(LEXPR-FUNCALL #'FS-COPY-FILE
		       (FUNCALL IN-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P PATOM
				':VERSION-DIRECTORY VER)
		       (FUNCALL OUT-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P PATOM
				':VERSION-DIRECTORY VER)
		       OPTIONS)
	(LOOP WITH MINOR = (OR LATEST-VERSION
			       (CAAR (SI:PATCH-VERSION-LIST SYSTEM)))
	      FOR I FROM MINOR DOWNTO 1
	      DO
	      (AND (EQ (LEXPR-FUNCALL #'FS-COPY-FILE
				      (FUNCALL IN-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P
					       PATOM ':PATCH-FILE VER I "LISP")
				      (FUNCALL OUT-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P
					       PATOM ':PATCH-FILE VER I "LISP")
				      OPTIONS)
		       ':ALREADY-EXISTS)
		   NEW-PATCHES-ONLY
		   (RETURN))
	      (LEXPR-FUNCALL #'FS-COPY-FILE
			     (FUNCALL IN-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P
				      PATOM ':PATCH-FILE VER I "QFASL")
			     (FUNCALL OUT-ACTOR ':PATCH-FILE-PATHNAME NAME SAME-DIR-P
				      PATOM ':PATCH-FILE VER I "QFASL")
			     OPTIONS))
	(RETURN NIL)))

(DEFUN COPY-MICROCODE-FILES (VERSIONS FROM TO &REST OPTIONS)
  (LET ((ACTOR (FS:PARSE-PATHNAME "SYS:UBIN;UCADR")) IN OUT TEM)
    (COND ((MEMQ VERSIONS '(NIL :NEWEST))
	   (SETQ VERSIONS (FUNCALL (PROBEF "SYS:UCADR;UCADR LISP >") ':VERSION)))
	  ((EQ VERSIONS ':CURRENT)
	   (SETQ VERSIONS %MICROCODE-VERSION-NUMBER))
	  ((EQ VERSIONS ':ALL)
	   (COND ((TYPEP (SETQ TEM (FUNCALL ACTOR ':TRANSLATED-PATHNAME))
			 'ITS-PATHNAME)
		  (SETQ VERSIONS
			(LOOP FOR (PATH) IN
			      (NCONC (DIRECTORY-LIST (FUNCALL TEM ':NEW-TYPE "***MCR") ':FAST)
				     (DIRECTORY-LIST (FUNCALL TEM ':NEW-TYPE "***SYM") ':FAST)
				     (DIRECTORY-LIST (FUNCALL TEM ':NEW-TYPE "***TBL") ':FAST))
			      WHEN PATH DO
			      (MULTIPLE-VALUE (NIL TEM)
				(FUNCALL PATH ':TYPE-AND-VERSION))
			      AND COLLECT TEM)))
		 (T (SETQ VERSIONS
			  (LOOP FOR (PATH) IN (DIRECTORY-LIST "SYS:UBIN;UCADR * *")
				WHEN (AND PATH (MEMBER (FUNCALL PATH ':TYPE)
						       '("MCR" "SYM" "TBL")))
				COLLECT (FUNCALL PATH ':VERSION)))))
	   (LOOP FOR (PATH) IN (DIRECTORY-LIST "SYS: UCADR; UCADR LISP *")
		 WHEN PATH COLLECT (FUNCALL PATH ':VERSION) INTO FOO
		 FINALLY (SETQ VERSIONS
			       (SORT (SI:ELIMINATE-DUPLICATES (APPEND FOO VERSIONS)) #'<)))))
    (COND ((NUMBERP VERSIONS)
	   (SETQ VERSIONS (LIST VERSIONS))))
    (LOOP FOR VERSION IN VERSIONS DOING
	  (SETQ IN (TRANSLATE ACTOR FROM)
		OUT (TRANSLATE ACTOR TO))
	  (DOLIST (TYPE '("MCR" "SYM" "TBL"))
	    (LEXPR-FUNCALL #'FS-COPY-FILE (FUNCALL IN ':NEW-TYPE-AND-VERSION TYPE VERSION)
			   (IF (EQUAL TO "MT")
			       "MT:"
			       (FUNCALL OUT ':NEW-TYPE-AND-VERSION TYPE VERSION))
			   ':DEFAULT-BYTE-SIZE (IF (EQUAL TYPE "MCR") 16. 8) OPTIONS))
	  (LET ((SOURCE-ACTOR (FS:PARSE-PATHNAME "SYS:UCADR;UCADR LISP")))
	    (SETQ IN (TRANSLATE SOURCE-ACTOR FROM)
		  OUT (TRANSLATE SOURCE-ACTOR TO))
	    (LEXPR-FUNCALL #'FS-COPY-FILE
			   (FUNCALL IN ':NEW-PATHNAME ':TYPE "LISP" ':VERSION VERSION)
			   (IF (EQUAL TO "MT")
			       "MT:"
			     (FUNCALL OUT ':NEW-PATHNAME ':TYPE "LISP" ':VERSION VERSION))
			   ':DEFAULT-BYTE-SIZE 8
			   OPTIONS)))))


|#