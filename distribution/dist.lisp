;;; -*-Mode:Lisp;Package:FS;Base:8-*-
;;; Here are distribution-related functions that really don't belong in the MT system

(defun make-distribution (&OPTIONAL (host "LM:") (dirs si:*LMI-Standard-Directories*))
  (let ((FS:LM-AUTOMATICALLY-CREATE-DIRECTORIES T))
    (LOOP FOR dir IN dirs
	  AS dir-files = (find-directory-files dir)
	  DO
	  (LOOP FOR (file directory-entry) IN dir-files
		DO (copy-file file host ':DIRECTORY-LIST (cdr directory-entry))))))


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
			   (IF (EQUALP TO "MT")
			       "MT:"
			       (FUNCALL OUT ':NEW-TYPE-AND-VERSION TYPE VERSION))
			   ':DEFAULT-BYTE-SIZE (IF (EQUAL TYPE "MCR") 16. 8) OPTIONS))
	  (LET ((SOURCE-ACTOR (FS:PARSE-PATHNAME "SYS:UCADR;UCADR LISP")))
	    (SETQ IN (TRANSLATE SOURCE-ACTOR FROM)
		  OUT (TRANSLATE SOURCE-ACTOR TO))
	    (LEXPR-FUNCALL #'FS-COPY-FILE
			   (FUNCALL IN ':NEW-PATHNAME ':TYPE "LISP" ':VERSION VERSION)
			   (IF (EQUALP TO "MT")
			       "MT:"
			     (FUNCALL OUT ':NEW-PATHNAME ':TYPE "LISP" ':VERSION VERSION))
			   ':DEFAULT-BYTE-SIZE 8
			   OPTIONS)))))

(DEFUN TRANSLATE (PATH TO-HOST)
  ;; This is excessively simple now.
  (cond ((equalp to-host "mt")
	 (multiple-value-bind (foo bar)
	     (funcall (DEFAULT-PATHNAME *DEFAULT-PATHNAME-DEFAULTS* TO-HOST NIL NIL T)
		  ':parse-namestring
		  t
		  (funcall (funcall path ':translated-pathname) ':string-for-printing))
	   foo	;ignore
	   bar))
	(t
	 (SETQ PATH (FUNCALL PATH ':TRANSLATED-PATHNAME))
	 (MAKE-PATHNAME ':HOST TO-HOST ':DEVICE "DSK" ':DIRECTORY (PATHNAME-DIRECTORY PATH)
		 ':DEVICE (PATHNAME-DEVICE PATH)
		 ':NAME (PATHNAME-NAME PATH) ':TYPE (PATHNAME-TYPE PATH)
		 ':VERSION (PATHNAME-VERSION PATH)))))

;also gobble LISPM; UCADR nnnTBL, MCR, and SYM.
(DEFCONST OTHER-FILES '(
			"CADR1;BUSINT WLR" "CADR1;NEWBAK >" "CADR1;XSPEC >" 
			"CADRDC;DC WLR"
			"CADRIO;IOB WLR"
			"CADRMW;MW WLR"
			"CADRPT;BAKWIR >"
			"CADRTV;LMTV4B WLR" 
			"CADRWD;CADR4 WLR" "CADRWD;ICMEM3 WLR"
			"LCADR;PROMH >" "LCADR;PRAID >"
			"LCADR;MEMD >"
			"LISPM;RECOM >"
			"LISPM1;DCFU ULOAD"
			"LISPM1;MEMD ULOAD"
			"LISPM1;UCADR LOCS"
			"LISPM2;GLOBAL >"
			"LISPM2;SYSTEM >"
			"LISPM2;NUMER >"
			"LISPM;-READ- -THIS-" "LISPM;COLDUT >" "LISPM;COLDUT QFASL"
			"LISPM;QCOM >"
			"LISPM;COLDLD >"
			"LISPM;PKGDCL >"
			"LISPM;SYSDCL >"
			"LMFONT;TVFONT QFASL"
			"LMFONT;20VR QFASL" "LMFONT;25FR3 QFASL"
			"LMFS;FS >" "LMFS;FS QFASL" "LMFS;FS DOC"
			"LMIO1;AS8748 >"  "LMIO1;AS8751 >"
			"LMIO1;CDRIVE >" "LMIO1;CTEST >"
			"LMIO1;PROMP >"
			"LMIO1;UKBD >" "LMIO1;UKBD PROM"
			"LMIO;FILE >"
			"LMIO;RTC >"
			"MOON;CHSNCP >"
			"MOON;DCFU >" 
;			"RG;RG LISPM"
			"SYSENG;HOSTS >"))

(DEFUN COPY-INSTALLED-VERSIONS (TO &OPTIONAL SYSTEMS &REST ARGS &AUX RESULT-FILES)
  (COND ((NULL SYSTEMS) (SETQ SYSTEMS SI:*SYSTEMS-LIST*))
	((NLISTP SYSTEMS) (SETQ SYSTEMS (LIST SYSTEMS))))
  (LET (FILES PLISTED-FILES)
    (DOLIST (SYSTEM SYSTEMS)
      (SETQ FILES (APPEND FILES (SI:SYSTEM-SOURCE-FILES SYSTEM))))
    (SETQ FILES (SI:ELIMINATE-DUPLICATES FILES))
    (SETQ RESULT-FILES
	  (LOOP FOR FILE IN FILES
		DO (SETQ FILE (FUNCALL FILE ':TRANSLATED-PATHNAME))
		COLLECTING (MAKE-PATHNAME ':HOST TO ':DEVICE "DSK"
					  ':DIRECTORY (FUNCALL FILE ':DIRECTORY)
					  ':NAME (FUNCALL FILE ':NAME)
					  ':TYPE (IF (EQ (FUNCALL FILE ':TYPE) ':UNSPECIFIC)
						     "LISP"
						   (FUNCALL FILE ':TYPE))
					  ':VERSION (FUNCALL FILE ':VERSION))))
    (SETQ PLISTED-FILES (MULTIPLE-FILE-PLISTS FILES))
    (LOOP FOR FILE IN PLISTED-FILES
	  DO (LEXPR-FUNCALL #'FS-COPY-FILE
			      (CAR FILE)
			      (NTH (FIND-POSITION-IN-LIST (CAR FILE) FILES)
				   RESULT-FILES)
			      ARGS))))

(DEFVAR SYSTEM-DIRECTORIES '(
"FONTS"
"LCADR"
"LISPM"
"LISPM1"
"LISPM2"
"LMCONS"
"LMDEMO"
"LMFONT"
"LMFS"
"LMIO"
"LMIO1"
"LMPAT"
"LMWIN"
"MOON"
"SYSENG"
"ZMAIL"
"ZWEI"
))

(defvar documentation-directories '(
"LMDOC"
"LMMAN"
"LMWIND"
))

(defvar hardware-directories '(
"CADR1"
"CADRDC"
"CADRIO"
"CADRMW"
"CADRPT"
"CADRTV"
"CADRWD"
))

(DEFUN COPY-WORLD (&OPTIONAL (FROM "FS") (TO "LM") &REST OPTIONS)
  (COND ((YES-OR-NO-P (FORMAT NIL "Copying world from ~s to ~s, OK?" FROM TO))
	 (SETQ OPTIONS (COPYLIST OPTIONS))
	 (OR (GET (LOCF OPTIONS) ':DEFAULT-BYTE-SIZE)
	     (PUTPROP (LOCF OPTIONS) 8 ':DEFAULT-BYTE-SIZE))
  ;	 (LEXPR-FUNCALL #'COPY-PATCH-FILES NIL FROM TO OPTIONS)
  ;	 (LEXPR-FUNCALL #'COPY-MICROCODE-FILES ':CURRENT FROM TO OPTIONS)
  ;	 (LEXPR-FUNCALL #'COPY-MICROCODE-FILES ':NEWEST FROM TO OPTIONS)
	 (LEXPR-FUNCALL #'COPY-FILES OTHER-FILES TO OPTIONS)
	 (LEXPR-FUNCALL #'COPY-SYSTEMS NIL TO OPTIONS)
  ;	 (LEXPR-FUNCALL #'COPY-INSTALLED-VERSIONS TO NIL OPTIONS)
	 )))

(DEFUN COPY-DOCUMENTATION-DIRECTORIES (&OPTIONAL (TO "LM") &REST OPTIONS &AUX (FROM "FS"))
  (LEXPR-FUNCALL #'COPY-DIRS DOCUMENTATION-DIRECTORIES FROM TO OPTIONS))

(DEFUN MAGTAPE-WRITE-SYSTEM-DIRECTORIES ()
  (COPY-DIRS SYSTEM-DIRECTORIES "LM" "MT:"))

(DEFUN DIRED-SYSTEM-DIRECTORIES ()
  (LET ((ACTOR (FUNCALL (FS:DEFAULT-PATHNAME NIL "LM")
			':NEW-PATHNAME ':NAME ':WILD ':VERSION ':WILD ':TYPE ':WILD)))
    (LOOP FOR DIR IN SYSTEM-DIRECTORIES DOING
	  (DIRED (FUNCALL ACTOR ':NEW-DIRECTORY DIR)))))

(DEFUN DIRED-ALL-DIRECTORIES ()
  (LOOP FOR (DIR) IN (ALL-DIRECTORIES SI:LOCAL-HOST)
	WHEN DIR DO
	(DIRED (FUNCALL DIR ':NEW-PATHNAME ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))))

;; Query by directory.
(DEFUN DELETE-FILES-WITH-BAD-CONTENTS ()
  (LOOP FOR (DIR) IN (ALL-DIRECTORIES "LM") DOING
	(SETQ DIR (FUNCALL DIR ':NEW-PATHNAME ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
	WHEN (Y-OR-N-P (FORMAT NIL "Check ~A ? " (FUNCALL DIR ':DIRECTORY))) DO
	(LOOP FOR (FILE) IN (DIRECTORY-LIST DIR ':FAST)
	      WHEN (AND FILE
			(WITH-OPEN-FILE (STREAM FILE ':DIRECTION ':INPUT)
			  (MULTIPLE-VALUE-BIND (STRING START END)
			      (FUNCALL STREAM ':READ-INPUT-BUFFER)
			    (COND ((AND STRING
					(NOT (STRING-SEARCH-NOT-CHAR 0 STRING START
					       (MIN END (+ START 100.)))))
				   (FORMAT T "~%Deleting ~A" FILE)
				   T)))))
	      COLLECT FILE INTO BAD
	      FINALLY (AND BAD
			   (Y-OR-N-P "Do it? ")
			   (FUNCALL (CAR BAD) ':DELETE-MULTIPLE-FILES T BAD)))))

(DEFVAR *LOGICAL-DIRECTORIES* 
  '(("CC" :USUALLY-ALL T :CADR-SPECIFIC T)
    ("CHAOS" :NEVER T)  ; we don't need the host table
    ("CHNCP")
    ("DEMO")
    ("DISTRIBUTION")
    ("DOC" :DOCUMENTATION T)
    ("FILE" :USUALLY-ALL T)
    ("FILE2" :USUALLY-ALL T)
    ("FONTS")	;NOT :just-qfasls! -- mly. There is now equivalence.lisp.
    		; Also, other random useful things could end up in this dir.
    ("IO")
    ("IO1")
    ("ISPELL")
    ("LAMBDA-DIAG")
    ("LAMBDA-UCODE")
;   ("LIB") not created on OZ
    ("MAN" :DOCUMENTATION T)
    ("NETWORK")
    ("PATCH" :USUALLY-ALL T)
    ("PRESS-FONTS" :TYPE "WIDTHS")
    ("SITE" :NEVER T)
    ("SYS")
    ("SYS2")
    ("TAPE" :USUALLY-ALL T)
    ("UBIN")
    ("UCADR")
    ("WINDOW")
    ("WIND" :DOCUMENTATION T)
    ("ZMAIL" :USUALLY-ALL T)
    ("ZWEI"))
  "An alist of SYS logical directories and how they should be copied.")

(DEFUN COPY-OZ-WORLD (TO &OPTIONAL &KEY
		      FILTER (ONLY-LATEST T) (SINCE NIL) (DOCUMENTATION ()) (MODE ':USUAL)
		      &AUX DOC-DIR TYPE)
  "Actually, this can be applied to other hosts, to, as it uses SYS:.
If the mode is :USUAL, qfasls are only copied for directories with patches in them.
If the mode is :ALL-CODE, all code (including QFASL) gets copied.
If documention is T, TEXT files are copied; if :ALL, all documentation is."
  (dolist (LDIR *LOGICAL-DIRECTORIES*)
    (unless (GET LDIR ':NEVER)
      (unless (and (SETQ DOC-DIR (GET LDIR ':DOCUMENTATION)) (NOT DOCUMENTATION))
	(FORMAT T "~%Working on the ~A directory.~%" (CAR LDIR))
	(SETQ TYPE
	      (COND ((get ldir ':type))
		    ((GET LDIR ':JUST-QFASLS)
		     ':QFASL)
		    ((GET LDIR ':USUALLY-ALL)
		     ':WILD)
		    (DOC-DIR (IF (EQ DOC-DIR ':ALL) ':WILD "TEXT"))
		    ((EQ MODE ':ALL-CODE) ':WILD)
		    (T ':LISP)))
	(COPY-DIRECTORY (FS:MAKE-PATHNAME ':HOST "SYS"
					  ':DIRECTORY (FIRST LDIR)
					  ':NAME ':WILD
					  ':TYPE TYPE)
			to
			':filter filter
			':copy-subdirectories ()
			':only-latest only-latest
			':since since)))))

#|	(format t "~&Would copy from: ~A" (FS:MAKE-PATHNAME ':HOST "SYS"
					  ':DIRECTORY (FIRST LDIR)
					  ':NAME ':WILD
					  ':TYPE TYPE))))))|#


(defun (:property :nlstandard tape-restore-transform) (host directory name type version)
  (fs:make-pathname ':host host ':directory (cons "L" (cdr directory))
		    ':name name ':type type ':version version))

;;; To be used with ancient crufty source tapes where SYS:FOO; went to FOO;
(defun (:property :OLD-LMIstandard tape-restore-transform) (host directory name type version)
  (fs:make-pathname ':host host ':directory (LIST "L" DIRECTORY)
		    ':name name ':type type ':version version))