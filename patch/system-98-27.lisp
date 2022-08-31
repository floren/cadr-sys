;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 98.27
;;; Reason: Lmfile pathname defaulting, file host lookup, pathname wildcard mapping bugs.
;;; Mouse clicks in inspector.
;;; READLINE eof arg bug.
;;; MEXP final expansion uses MACROEXPAND-ALL.
;;; REALLY fix common lisp ERROR.
;;; Written 7-Jan-84 06:45:07 by Mly,
;;; while running on Lisp Machine Eighteen from band 7
;;; with System 98.25, CADR 3.4, ZMail 53.9, MIT-Specific 22.0, microcode 306, ZM MIT.



; From file PNMAP.LISP SRC:<L.IO> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PNMAP  "

(defun pathname-translate-wild-component
       (target-pattern data specs wild-any wild-one)
  (cond ((eq target-pattern ':wild) data)
	((or (numberp target-pattern)
	     (symbolp target-pattern)
	     (eq specs t))
	 target-pattern)
	((listp target-pattern)
	 (loop for elt in target-pattern
	       collect
	       (multiple-value-bind (new-elt specs-left)
		   (pathname-translate-component-from-specs
		     elt specs wild-any wild-one)
		 (setq specs specs-left)
		 new-elt)))
	(t (pathname-translate-component-from-specs
	     target-pattern specs wild-any wild-one))))

))

; From file HOST.LISP SRC:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; HOST  "

(DEFMETHOD (BASIC-HOST :READ-INSTANCE) (IGNORE STREAM)
  (LET ((NAM (READ STREAM)))
    (OR (FS:GET-PATHNAME-HOST NAM)
	(PARSE-HOST NAM T))))

))

; From file PATHNM.LISP SRC:<L.FILE2> OZ:
#8R FILE-SYSTEM#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: FILE2; PATHNM  "

(defmethod (lmfile-parsing-mixin :new-pathname)
	   (&rest options &key &allow-other-keys
	    &optional (starting-pathname self)
	    (original-type nil original-type-p)
	    &aux host-p device-p directory-p name-p type-p version-p
	    new-host new-device new-directory new-name new-type canonical-type new-version
	    new-name-really
	    new-directory-really
	    new-type-really
	    new-version-really)
  (loop for (keyword value) on options by 'cddr
	do
	(selectq keyword
	  (:host (unless host-p (setq new-host value host-p t)))
	  (:name (unless name-p (setq new-name value name-p t)))
	  (:raw-name (unless name-p (setq new-name value name-p ':raw)))
	  (:directory (unless directory-p (setq new-directory value directory-p t)))
	  (:raw-directory (unless directory-p (setq new-directory value directory-p ':raw)))
	  (:device (unless device-p (setq new-device value device-p t)))
	  (:raw-device (unless device-p (setq new-device value device-p ':raw)))
	  (:type (unless type-p
		   (if (and (symbolp value) (not (memq value '(nil :unspecific))))
		       (setq canonical-type value type-p ':canonical)
		     (setq new-type value type-p t))))
	  (:raw-type (unless type-p (setq new-type value type-p ':raw)))
	  (:canonical-type
	   (unless type-p (setq canonical-type value type-p ':canonical)))
	  (:version (unless version-p (setq new-version value version-p t)))
;; All keywords that do NOT require special decoding must go here.
	  ((:starting-pathname
	    :original-type :defaults nil)
	   nil)
	  (t (ferror nil "Unknown keyword ~s to MAKE-PATHNAME or :NEW-PATHNAME." keyword))))
  (or host-p (setq new-host (pathname-host starting-pathname)))
  (setq new-host (get-pathname-host new-host))
  ;; Turn a specified canonical type into a string (in standard case).
  (if (eq type-p ':canonical)
      (multiple-value-bind (preferred all)
	  (decode-canonical-type canonical-type (send new-host ':system-type))
	(setq new-type
	      (let ((alphabetic-case-affects-string-comparison t))
		(unless original-type-p
		  (setq original-type (pathname-type starting-pathname)))
		(if (member original-type all)
		    original-type
		  preferred)))))
  (if (eq (pathname-host starting-pathname) new-host)
      (progn
	(or device-p (setq new-device (pathname-raw-device starting-pathname) device-p ':raw))
	(or directory-p (setq new-directory (pathname-raw-directory starting-pathname) directory-p ':raw))
	(or name-p (setq new-name (pathname-raw-name starting-pathname) name-p ':raw))
	(or type-p (setq new-type (pathname-raw-type starting-pathname) type-p ':raw)))
    ;; Hosts don't match; must convert to standard syntax and reparse.
    (or device-p (setq new-device (pathname-device starting-pathname)))
    (or directory-p (setq new-directory (pathname-directory starting-pathname)))
    (or name-p (setq new-name (pathname-name starting-pathname)))
    (or type-p (setq new-type (pathname-type starting-pathname))))
  (or version-p (setq new-version (pathname-raw-version starting-pathname)))
  ;; Now compute the actual new components from what was specified.
  (setq new-device (or new-device ':unspecific))
  (and (stringp new-device)
       (string-equal new-device "DSK")
       (setq new-device ':unspecific))
  (setq new-directory-really
	(cond ((eq directory-p ':raw) new-directory)
	      ((stringp new-directory) (expand-pathstring new-directory))
	      ((eq new-directory ':root) '(root))
	      ((memq new-directory '(nil :unspecific :wild))
	       new-directory)
	      ((consp new-directory) new-directory)
	      (t (ferror 'pathname-parse-error
			 "~S is not a valid directory component for ~A."
			 new-directory host))))
  (cond ((eq name-p ':raw)
	 (setq new-name-really new-name))
	((eq new-name ':wild)
	 (setq new-name-really ':wild))
	((stringp new-name)
	 (setf (values nil nil new-name-really new-type-really new-version-really)
	       (funcall-self ':parse-namestring nil new-name)))
	(t
	 (setq new-name-really new-name)))
  (setq new-type-really
	(cond ((eq type-p ':raw) new-type)
	      ((and (null type-p) new-type-really)
	       new-type-really)
	      ((memq new-type '(nil :unspecific :wild))
	       new-type)
	      ((string-equal new-type '*) ':wild)
	      ((mem 'string-equal new-type ignored-types)
	       ':unspecific)
	      (t (string new-type))))
  (and (stringp new-version)
       (setq new-version (intern new-version si:pkg-keyword-package)))
  (setq new-version-really
	(selectq new-version
	  (> ':newest)
	  (< ':oldest)
	  (* ':wild)
	  ((! :!) ':installed)
	  (t new-version)))
  (make-pathname-internal new-host
			  new-device new-directory-really new-name-really
			  new-type-really new-version-really))

))

; From file INSPCT.LISP SRC:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN INSPECT-COMMAND-LOOP (FRAME &AUX USER IS HISTORY)
  (FUNCALL (SETQ USER (FUNCALL FRAME ':GET-PANE 'INTERACTOR)) ':CLEAR-SCREEN)
  (FUNCALL (CAR (SETQ IS (FUNCALL FRAME ':INSPECTORS))) ':FLUSH-TYPEOUT)
  (FUNCALL USER ':SET-OLD-TYPEAHEAD NIL)
  (SETQ HISTORY (FUNCALL FRAME ':GET-PANE 'HISTORY))
  ;; Flush remnants of modify mode
  (FUNCALL HISTORY ':SET-SENSITIVE-ITEM-TYPES T)
  (DOLIST (I IS)
    (FUNCALL I ':SET-MODIFY-MODE NIL))
  (LET* ((TYPEOUT-WINDOW (FUNCALL FRAME ':TYPEOUT-WINDOW))
	 (TERMINAL-IO TYPEOUT-WINDOW)
	 * ** *** + ++ +++ \
	 *PRINT-ARRAY*
	 (STANDARD-INPUT SI:SYN-TERMINAL-IO)
	 (STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
	 (TV:KBD-INTERCEPTED-CHARACTERS
	   (REMOVE (ASSQ #\BREAK TV:KBD-INTERCEPTED-CHARACTERS)
		   TV:KBD-INTERCEPTED-CHARACTERS))
	 (THING) (TOP-ITEM))
    (DECLARE (SPECIAL \))
    (DO-NAMED INSPECTOR ()
	      (())
      (LET ((ITEMS (FUNCALL HISTORY ':ITEMS))
	    (IW)
	    (IDX))
	(SETQ IDX (ARRAY-ACTIVE-LENGTH ITEMS))
	;; Make sure the inspection windows reflect the state of the history buffer
	(DOLIST (I IS)
	  ;; Update datastructure to reflect current TOP-ITEMs
	  (LET ((DISP (FUNCALL I ':CURRENT-DISPLAY)))
	    (AND DISP (SETF (FOURTH DISP) (FUNCALL I ':TOP-ITEM)))))
	(DOTIMES (I (LENGTH IS))
	  (SETQ IDX (1- IDX))
	  (SETQ IW (NTH I IS))
	  (COND ((< IDX 0)
		 (FUNCALL IW ':SET-CURRENT-DISPLAY
			  (FUNCALL IW ':SETUP
				   `(INSPECT-PRINTER NIL NIL NIL
						     (NIL NIL NIL NIL
							  ,(LABEL-FONT (FUNCALL IW ':LABEL))
							  "Empty"))))
		 (FUNCALL IW ':SET-CURRENT-OBJECT (NCONS NIL)))
		(T (FUNCALL HISTORY ':INSPECT-OBJECT (AREF ITEMS IDX) IW TOP-ITEM NIL T)
		   (SETQ TOP-ITEM NIL)))))
      
      ;; Insure last item in history is on the screen
      (FUNCALL HISTORY ':PUT-LAST-ITEM-IN-WINDOW)
      
      ;; Give *, ** and *** the right values.
      (SETQ *PRINT-ARRAY* NIL)
      (LET* ((ITEMS (FUNCALL HISTORY ':ITEMS))
	     (NITEMS (IF ITEMS (ARRAY-ACTIVE-LENGTH ITEMS) 0)))
	(AND ( NITEMS 1) (SETQ * (AREF ITEMS (- NITEMS 1))))
	(AND ( NITEMS 2) (SETQ ** (AREF ITEMS (- NITEMS 2))))
	(AND ( NITEMS 3) (SETQ *** (AREF ITEMS (- NITEMS 3)))))
      
      ;; Get input.
      ;; Keyboard commands are processed inside this loop.
      ;; Mouse commands exit the loop and go round the outer loop.
      (DO-FOREVER
	(SETQ THING -1)
	(FUNCALL (CAR IS) ':FLUSH-TYPEOUT)
	(FUNCALL FRAME ':SELECT-PANE USER)
	(FUNCALL USER ':FRESH-LINE)
	(OR (FUNCALL USER ':OLD-TYPEAHEAD)
	    (SETQ THING (FUNCALL USER ':ANY-TYI)))
	(UNLESS (NUMBERP THING)
	       ;; Some sort of mouse command, just process
	  (RETURN))
	(SELECTQ THING
	  ((#\C-Z #\ABORT)
	   (SIGNAL EH:ABORT-OBJECT))
	  (#\C-V
	   (FUNCALL (CAR IS) ':SCROLL-TO
		    (- (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)) 2)
		    ':RELATIVE))
	  (#\M-V
	   (FUNCALL (CAR IS) ':SCROLL-TO
		    (- 2 (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)))
		    ':RELATIVE))
	  (#\BREAK
	   (FUNCALL FRAME ':SELECT-PANE (CAR IS))
	   (FUNCALL TERMINAL-IO ':EXPOSE-FOR-TYPEOUT)
	   (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to inspector command loop.")
	     (BREAK 'INSPECT))
	   (FUNCALL TERMINAL-IO ':MAKE-COMPLETE))
	  ;; Clear-Screen decaches.
	  (#\CLEAR-SCREEN
	   (FUNCALL HISTORY ':SET-CACHE NIL)
	   (FUNCALL FRAME ':CLEAR-SCREEN)
	   (FUNCALL FRAME ':REFRESH ':COMPLETE-REDISPLAY))
	  ;; End returns *.
	  (#\END
	   (RETURN-FROM INSPECTOR *))
	  (#\HELP
	   (INSPECT-HELP)
	   (FORMAT TERMINAL-IO "~%Type any character to continue:")
	   (LET ((CH (FUNCALL USER ':ANY-TYI)))
	     (OR (= CH #\SP)
		 (FUNCALL USER ':UNTYI CH))))
	  (#\DELETE
	   (RETURN (FUNCALL HISTORY ':FLUSH-CONTENTS)))
	  ;;set \
	  (#\C-\
	   (FORMAT USER "~&Value to set \ to ")
	   (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
	       (INSPECT-GET-VALUE-FROM-USER USER)
	     (OR PUNT-P (SETQ \ VALUE))))
	  (#\RUBOUT)
	  (#\QUOTE
	   (LET ((TERMINAL-IO USER)
		 FLAG)
	     (FORMAT USER "Eval: ")
	     (MULTIPLE-VALUE (THING FLAG)
	       (FUNCALL USER ':RUBOUT-HANDLER
			'((:FULL-RUBOUT :FULL-RUBOUT)) #'SI:READ-FOR-TOP-LEVEL))
	     (COND ((NEQ FLAG ':FULL-RUBOUT)
		    (SETQ +++ ++ ++ + + THING)
		    (MULTIPLE-VALUE (THING FLAG) (CATCH-ERROR (EVAL THING)))
		    (OR FLAG
			(LET ((PRINLEVEL 3) (PRINLENGTH 5))
			  (PRINT THING USER)))))))
	  (OTHERWISE
	   (LET ((TERMINAL-IO USER)
		 FLAG)
	     (AND ( THING 0) (FUNCALL USER ':UNTYI THING))
	     (MULTIPLE-VALUE (THING FLAG)
	       (FUNCALL USER ':PREEMPTABLE-READ
			'((:FULL-RUBOUT :FULL-RUBOUT)) #'SI:READ-FOR-TOP-LEVEL))
	     (COND ((EQ FLAG ':MOUSE-CHAR) (RETURN))
		   ((NEQ FLAG ':FULL-RUBOUT)
		    (SETQ +++ ++ ++ + + THING)
		    (MULTIPLE-VALUE (THING FLAG) (CATCH-ERROR (EVAL THING)))
		    (OR FLAG
			(RETURN (SETQ THING `(:VALUE ,THING ,HISTORY))))))))))
      (CATCH-ERROR-RESTART (SYS:ABORT "Return to inspector command loop.")
	(COND
	  ((NLISTP THING))
	  ((EQ (CAR THING) ':MOUSE-BUTTON))	;random rodentry
	  ((EQ (CAR THING) ':MENU)
	   (SETF (SECOND THING) (FUNCALL (FOURTH THING) ':EXECUTE (SECOND THING)))
	   (SELECTQ (SECOND THING)
	     (:EXIT (RETURN *))
	     (:RETURN
	      (FORMAT USER "~&Value to return ")
	      (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
		  (INSPECT-GET-VALUE-FROM-USER USER)
		(OR PUNT-P (RETURN VALUE))))
	     (:FLUSH-CACHE
	      (FUNCALL HISTORY ':SET-CACHE NIL))
	     (:MODIFY
	      (SETQ TOP-ITEM (INSPECT-MODIFY-OBJECT USER HISTORY IS)))
	     (:CLEAR
	      (FUNCALL HISTORY ':FLUSH-CONTENTS))
	     (:SET-\
	      (FORMAT USER "~&Value to set \ to ")
	      (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
		  (INSPECT-GET-VALUE-FROM-USER USER)
		(OR PUNT-P (SETQ \ VALUE))))
	     (OTHERWISE (FORMAT USER "~&Unimplemented menu command ~A~%" (SECOND THING)))))
	  (T
	   (COND ((NULL (FIRST THING))
		  ;; Type is NIL -- nothing under mouse
		  (BEEP))
		 ((AND (EQ (FIRST THING) ':LINE-AREA) (EQ (FOURTH THING) #\MOUSE-2-1))
		  ;; Delete from line area
		  (FUNCALL HISTORY ':FLUSH-OBJECT (INSPECT-REAL-VALUE THING)))
		 ((AND (EQ (FOURTH THING) #\MOUSE-2-1)
		       (MEMQ (THIRD THING) IS))
		  ;; Middle click means leave source in one of the windows
		  (LET ((1ST-THING (INSPECT-REAL-VALUE THING))
			(2ND-THING (FUNCALL (THIRD THING) ':CURRENT-OBJECT)))
		    ;; First flush item we will be inspecting
		    (INSPECT-FLUSH-FROM-HISTORY 1ST-THING HISTORY)
		    (INSPECT-FLUSH-FROM-HISTORY 2ND-THING HISTORY)
		    (FUNCALL HISTORY ':APPEND-ITEM 2ND-THING)
		    (FUNCALL HISTORY ':APPEND-ITEM 1ST-THING)))
		 ((EQ (FOURTH THING) #\MOUSE-3-1)
		  ;; Click on right button -- try to find function
		  (SETQ THING (INSPECT-FIND-FUNCTION (INSPECT-REAL-VALUE THING)))
		  (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
		  (FUNCALL HISTORY ':APPEND-ITEM THING))
		 ((CHAR-BIT (FOURTH THING) ':HYPER)
		  ;; HYPER means modify the slot we are pointing at.
		  (LET ((TERMINAL-IO (THIRD THING)))
		    (IF (OR (NULL (FIRST THING)) (NULL (GET (FIRST THING) 'SET-FUNCTION)))
			(FORMAT TERMINAL-IO "~&Cannot set this component.")
		      (INSPECT-SET-SLOT THING HISTORY USER))))
		 (T
		  ;; Otherwise inspect the thing we are pointing at.
		  (SETQ THING (INSPECT-REAL-VALUE THING))
		  (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
		  (FUNCALL HISTORY ':APPEND-ITEM THING)))))))))

))

; From file QIO.LISP PS:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; QIO  "

(DEFUN READLINE (&REST READ-ARGS)
  "Read a line from STREAM and return it as a string.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to :RUBOUT-HANDLER if it is used.

The second value is EOF-OPTION if we exit due to end of file."
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS)
	   (VALUES STRING-OR-EOF-OPTION EOF-FLAG))
  (LET ((OPTIONS NIL))
    ;; This kludge is to let us take a third, optional argument.
    (COND ((> (LENGTH READ-ARGS) 2)
	   (SETQ OPTIONS (THIRD READ-ARGS))
	   (SETQ READ-ARGS (LIST (FIRST READ-ARGS) (SECOND READ-ARGS)))))
    (MULTIPLE-VALUE-BIND (STREAM EOF-OPTION)
	(DECODE-READ-ARGS READ-ARGS)
      (MULTIPLE-VALUE-BIND (STRING EOF)
	  (READ-DELIMITED-STRING '(#\RETURN #\END) STREAM
				 (EQ EOF-OPTION 'NO-EOF-OPTION) OPTIONS)
	(VALUES STRING (IF EOF EOF-OPTION))))))


))

; From file SHEET.LISP PS:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(defun ufm (window)
  (let* ((old-font-map (send window ':font-map))
	 (font-map-font-list (font-map-font-list old-font-map))
	 (current-font (font-map-current-font-name old-font-map))
	 tem ok-if-not-in-map)
    (setq font-map-font-list
	  (mapcar 'update-font font-map-font-list))
    (unless current-font
      (setq current-font (send window ':current-font)))
    (setq current-font (cond ((numberp current-font) current-font)
			     ((setq tem (memq current-font font-map-font-list))
			      (car tem))
			     ((setq tem (position current-font old-font-map))
			      (update-font (aref old-font-map tem)))
			     (t (setq ok-if-not-in-map t)
				(update-font current-font))))
    (send window ':set-current-font current-font))
  (dolist (i (send window ':inferiors))
    (ufm i)))

))

; From file QMISC.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QMISC  "

(DEFUN MEXP (&OPTIONAL &QUOTE FORM)
  "Read-macroexpand-print loop, for seeing how macros expand.
MEXP reads s-expressions and macroexpands each one, printing the expansion.
Type NIL to exit (or Abort)."
    (DO ((TEM FORM NIL))
	(())
      (UNLESS TEM
	(FORMAT T "~2%Macro form ")
	(FUNCALL STANDARD-INPUT ':UNTYI (FUNCALL STANDARD-INPUT ':TYI)));Allow abort to exit
      (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to MEXP input loop.")
	(SETQ TEM (OR TEM (READ-FOR-TOP-LEVEL)))
	(AND (SYMBOLP TEM) (RETURN NIL))
	(DO ((EXP (MACROEXPAND-1 TEM) (MACROEXPAND-1 EXP)))
	    ((EQ EXP TEM))
	  (PRINC "  ")
	  (GRIND-TOP-LEVEL (SETQ TEM EXP)))
	(UNLESS (EQUAL TEM (SETQ TEM (MACROEXPAND-ALL TEM)))
	  (PRINC "  ")
	  (GRIND-TOP-LEVEL TEM))
	)))

))

; From file MAKSYS.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; MAKSYS  "

(DEFUN DESCRIBE-SYSTEM (SYSTEM-NAME &KEY (SHOW-FILES T) (SHOW-TRANSFORMATIONS T) &AUX SYSTEM)
  "Print all about the system named SYSTEM-NAME.
SHOW-FILES is T to give the history of each file in the system, NIL not to,
 or :ASK meaning query the user whether to.
SHOW-TRANSFORMATIONS is similar, for whether to show the transformations
 which MAKE-SYSTEM would execute.
Note that calling DESCRIBE on a system-object prints somewhat lower level information."
  (IF (NULL (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME)))
      (FORMAT T "~&There is no system named ~A.~%" SYSTEM-NAME)
    (SETQ SYSTEM-NAME (SYSTEM-NAME SYSTEM))
    (LET* ((SYSTEM-SOURCE-FILE
	     (GET-SOURCE-FILE-NAME (SYSTEM-SYMBOLIC-NAME SYSTEM) 'DEFSYSTEM))
	   (*FORCE-PACKAGE*
	     (PKG-FIND-PACKAGE (OR (SEND SYSTEM-SOURCE-FILE ':GET ':PACKAGE) "USER"))))
      (WHEN SYSTEM-SOURCE-FILE
	(FORMAT T "~&System ~A~@[ is defined in file ~A~]~%"
		SYSTEM-NAME SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE SYSTEM-SOURCE-FILE)
	(DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE SYSTEM-SOURCE-FILE)))
    (COND ((SYSTEM-PATCHABLE-P SYSTEM)
	   (FORMAT T "~&~%~A is patchable" SYSTEM-NAME)
	   (MULTIPLE-VALUE-BIND (MAJOR MINOR STATUS)
	       (GET-SYSTEM-VERSION SYSTEM)
	     (LET ((STATUS-NAME (OR (SECOND (ASSQ STATUS SYSTEM-STATUS-ALIST)) STATUS)))
	       (OR (EQUAL STATUS-NAME "")
		   (FORMAT T ", ~A" STATUS-NAME)))
	     (IF MAJOR (FORMAT T ", ~D.~D is loaded" MAJOR MINOR))
	     (FORMAT T ";~%  a typical patch file is ~A~%"
		     (PATCH-SYSTEM-PATHNAME SYSTEM-NAME ':PATCH-FILE (OR MAJOR 1) (OR MINOR 0)
					    ':LISP))
	     (AND MAJOR
		  (FQUERY NIL "Do you want to see the patches for ~A? " SYSTEM-NAME)
		  (PRINT-PATCHES SYSTEM)))))
    (IF (SYSTEM-PACKAGE-DEFAULT SYSTEM)
	(FORMAT T "~& Files in ~A are forcibly read in package ~A.~%"
		SYSTEM-NAME (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
    (WHEN SHOW-FILES
      (FORMAT T "~%Compilation and loading of files in this system:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':RELOAD ':DO-NOT-DO-COMPONENTS
		   ':DESCRIBE ':NO-INCREMENT-PATCH ':NO-RELOAD-SYSTEM-DECLARATION))
    (WHEN SHOW-TRANSFORMATIONS
      (FORMAT T "~%Transformations required to MAKE-SYSTEM now:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':DO-NOT-DO-COMPONENTS ':PRINT-ONLY
		   ':NO-RELOAD-SYSTEM-DECLARATION))
    (LET ((COMPONENTS (SYSTEM-COMPONENT-SYSTEMS SYSTEM)))
      (COND (COMPONENTS
	     (FORMAT T " ~A is made up of component system~P "
		     SYSTEM-NAME (LENGTH COMPONENTS))
	     (FORMAT:PRINT-LIST T "~A" COMPONENTS)
	     (WHEN (Y-OR-N-P "Describe the component system~P?" (LENGTH COMPONENTS))
	       (DOLIST (COMPONENT COMPONENTS)
		 (FORMAT T "~2&")
		 (DESCRIBE-SYSTEM COMPONENT ':SHOW-FILES SHOW-FILES
				  ':SHOW-TRANSFORMATIONS SHOW-TRANSFORMATIONS)))))))
  SYSTEM-NAME)

))

; From file FLAVOR.LISP PS:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; FLAVOR  "

(DEFUN DEFFLAVOR1 (FLAVOR-NAME INSTANCE-VARIABLES COMPONENT-FLAVORS OPTIONS
		   &AUX FFL ALREADY-EXISTS INSTV IDENTICAL-COMPONENTS
			GETTABLE SETTABLE INITTABLE SPECIAL-IVS
			OLD-SPECIAL-IVS OLD-DEFAULT-HANDLER
			OLD-DEFAULT-INIT-PLIST OLD-LOCAL-IVS OLD-INITTABLE-IVS
			OLD-INIT-KWDS OLD-INSTANCE-AREA-FUNCTION
			OLD-REQUIRED-INIT-KEYWORDS
			INIT-KEYWORDS INCLUDES METH-COMB
			NEW-PLIST (PL (LOCF NEW-PLIST))
			(DEFAULT-CONS-AREA
			  (IF *JUST-COMPILING* DEFAULT-CONS-AREA
			    *FLAVOR-AREA*)))
  (OR *JUST-COMPILING* (RECORD-SOURCE-FILE-NAME FLAVOR-NAME 'DEFFLAVOR))
  (WITHOUT-INTERRUPTS
    (COND ((AND (NOT *JUST-COMPILING*)
		(NOT (MEMQ FLAVOR-NAME *ALL-FLAVOR-NAMES*)))
	   (PUSH FLAVOR-NAME *ALL-FLAVOR-NAMES*)
	   ;; Push on the name without the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (CONS (GET-PNAME FLAVOR-NAME) FLAVOR-NAME))
	   ;; Push on the name with the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (LET ((PACKAGE NIL))
				(CONS (FORMAT NIL "~S" FLAVOR-NAME) FLAVOR-NAME)))
	   ;; Array is no longer sorted.
	   (STORE-ARRAY-LEADER NIL *ALL-FLAVOR-NAMES-AARRAY* 1))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
	(FERROR NIL "~S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
	(FERROR NIL "~S, which is not a symbol, was specified as a component flavor" CF)))
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (COND ((AND (SETQ ALREADY-EXISTS (COMPILATION-FLAVOR FLAVOR-NAME))
	      *USE-OLD-FLAVOR-INFO*)
	 (DOLIST (PROP DEFFLAVOR1-PRESERVED-PROPERTIES)
	   (PUTPROP PL (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS)) PROP)
		    PROP))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (DO ((L OPTIONS (CDR L))
       (OPTION) (ARGS))
      ((NULL L))
    (IF (ATOM (CAR L))
	(SETQ OPTION (CAR L) ARGS NIL)
	(SETQ OPTION (CAAR L) ARGS (CDAR L)))
    (SELECTQ OPTION
	(:GETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ GETTABLE (UNION GETTABLE (OR ARGS INSTV))))
	(:SETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ SETTABLE (UNION SETTABLE (OR ARGS INSTV))))
	((:INITTABLE-INSTANCE-VARIABLES :INITABLE-INSTANCE-VARIABLES)
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ INITTABLE (UNION INITTABLE (OR ARGS INSTV))))
	(:SPECIAL-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ SPECIAL-IVS (UNION SPECIAL-IVS (OR ARGS INSTV))))
	(:INIT-KEYWORDS
	  (SETQ INIT-KEYWORDS (UNION INIT-KEYWORDS ARGS)))
	(:INCLUDED-FLAVORS
	  (SETQ INCLUDES (UNION INCLUDES ARGS)))
	(:NO-VANILLA-FLAVOR
	  (PUTPROP PL T OPTION))
	(:ORDERED-INSTANCE-VARIABLES
	  ;Don't validate.  User may reasonably want to specify non-local instance
	  ;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
	  ;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (OR ARGS INSTV) ':ORDERED-INSTANCE-VARIABLES))
	(:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (UNION (GET PL ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			     (OR ARGS INSTV))
		   ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
	(:METHOD-COMBINATION
	  (SETQ METH-COMB (NUNION-EQUAL METH-COMB ARGS)))
	(:DEFAULT-HANDLER
	  (PUTPROP PL (CAR ARGS) OPTION))
	((:REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS
	  :REQUIRED-FLAVORS :REQUIRED-INIT-KEYWORDS)
	  (PUTPROP PL (UNION ARGS (GET PL OPTION)) OPTION))
	((:DOCUMENTATION :DEFAULT-INIT-PLIST :SELECT-METHOD-ORDER :ACCESSOR-PREFIX)
	  (PUTPROP PL ARGS OPTION))
	(:ALIAS-FLAVOR
	 (PUTPROP PL T ':ALIAS-FLAVOR))
	(:ABSTRACT-FLAVOR
	 (PUTPROP PL T ':ABSTRACT-FLAVOR))
	(:INSTANCE-AREA-FUNCTION
	 (PUTPROP PL (CAR ARGS) ':INSTANCE-AREA-FUNCTION))
	(:INSTANTIATION-FLAVOR-FUNCTION
	 (PUTPROP PL (CAR ARGS) ':INSTANTIATION-FLAVOR-FUNCTION))
	((:RUN-TIME-ALTERNATIVES :MIXTURE)
	 (PUTPROP PL ARGS ':RUN-TIME-ALTERNATIVES)
	 (PUTPROP PL 'CHOOSE-RUN-TIME-ALTERNATIVE ':INSTANTIATION-FLAVOR-FUNCTION)
	 (PUTPROP PL (MAKE-RUN-TIME-ALTERNATIVE-ALIST FLAVOR-NAME ARGS)
		  'RUN-TIME-ALTERNATIVE-ALIST))
	(OTHERWISE (FERROR NIL "~S is not a known DEFFLAVOR option." OPTION))))
  ;; All settable instance variables should also be gettable and inittable.
  (DOLIST (V SETTABLE)
    (OR (MEMQ V GETTABLE)
	(PUSH V GETTABLE))
    (OR (MEMQ V INITTABLE)
	(PUSH V INITTABLE)))
  ;; See whether there are any changes in component flavor structure from last time
  (SETQ IDENTICAL-COMPONENTS
	(AND ALREADY-EXISTS
	     *USE-OLD-FLAVOR-INFO*
	     (EQUAL COMPONENT-FLAVORS (FLAVOR-DEPENDS-ON ALREADY-EXISTS))
	     (EQUAL INCLUDES (FLAVOR-INCLUDES ALREADY-EXISTS))
	     (EQUAL (GET PL ':REQUIRED-FLAVORS)
		    (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS)) ':REQUIRED-FLAVORS))))	
  (AND ALREADY-EXISTS
       (SETQ OLD-SPECIAL-IVS (FLAVOR-SPECIAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-DEFAULT-HANDLER (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
				      ':DEFAULT-HANDLER)
	     OLD-DEFAULT-INIT-PLIST (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
					 ':DEFAULT-INIT-PLIST)
	     OLD-LOCAL-IVS (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INITTABLE-IVS (FLAVOR-INITTABLE-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET ALREADY-EXISTS ':INSTANCE-AREA-FUNCTION)
	     OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET ALREADY-EXISTS ':REQUIRED-INIT-KEYWORDS)
	     OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS ALREADY-EXISTS)))
  ;; If the flavor is being redefined, and the number or order of instance variables
  ;; is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give a warning
  ;; and disconnect from the old FLAVOR defstruct so that old instances will
  ;; retain the old information.  The instance variables can get changed either
  ;; locally or by rearrangement of the component flavors.
  (AND ALREADY-EXISTS
       (IF (AND *USE-OLD-FLAVOR-INFO*
		(EQUAL (GET PL ':ORDERED-INSTANCE-VARIABLES)
		       (GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
			    ':ORDERED-INSTANCE-VARIABLES))
		(OR (EQUAL (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
			   INSTANCE-VARIABLES)
		    (EQUAL (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
				   (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS))
			   INSTV))
		(EQ (GET PL ':ALIAS-FLAVOR)
		    (FLAVOR-GET ALREADY-EXISTS ':ALIAS-FLAVOR))
		(OR IDENTICAL-COMPONENTS
		    (EQUAL (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       COMPONENT-FLAVORS INCLUDES)
			   (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       (FLAVOR-DEPENDS-ON ALREADY-EXISTS)
						       (FLAVOR-INCLUDES ALREADY-EXISTS)))))
	   (IF *JUST-COMPILING*
	       (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS NIL)))
	 (IF *JUST-COMPILING*
	     (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS T))
	   (SETQ ALREADY-EXISTS (PERFORM-FLAVOR-REDEFINITION FLAVOR-NAME)))))
  (WHEN (GET PL ':ALIAS-FLAVOR)
    (IF (CDR COMPONENT-FLAVORS)
	(FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		     "This alias flavor has more than one component."))
    (UNLESS COMPONENT-FLAVORS
      (FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		   "This alias flavor has no component to be the alias of."))
    (IF INSTANCE-VARIABLES
	(FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS ':IMPOSSIBLE
		     "This alias flavor has instance variables; they will be ignored.")))
  ;; Make the information structure unless the flavor already exists.
  (LET ((FL (OR ALREADY-EXISTS
		(AND (NOT *JUST-COMPILING*)
		     (GET FLAVOR-NAME 'UNDEFINED-FLAVOR))
		(MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))
    (SETF (FLAVOR-PACKAGE FL) PACKAGE)
    (SETF (FLAVOR-LOCAL-INSTANCE-VARIABLES FL) INSTANCE-VARIABLES)
    (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
    (LET ((OVEC (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL)))
      (SETF (FLAVOR-PLIST FL) NEW-PLIST)
      (IF OVEC (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL) OVEC)))
    (IF GETTABLE
	(SETF (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL) GETTABLE))
    (IF SETTABLE
	(SETF (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL) SETTABLE))
    (IF SPECIAL-IVS
	(SETF (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL) SPECIAL-IVS))
    (SETF (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL)
	  (LOOP FOR V IN INITTABLE COLLECT (CONS (CORRESPONDING-KEYWORD V) V)))
    (SETF (FLAVOR-INIT-KEYWORDS FL) INIT-KEYWORDS)
    (SETF (FLAVOR-INCLUDES FL) INCLUDES)
    ;; This can't be computed for real until flavor composition,
    ;; but this at least contains some of the right ones.
    (SETF (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FL)
	  (FLAVOR-KNOWN-UNMAPPED-INSTANCE-VARIABLES FL))
    ;; First remove old method-combination declarations, then add new ones
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
      (COND ((LOOP FOR DECL IN METH-COMB NEVER (MEMQ (CAR MTE) (CDDR DECL)))
	     (SETF (SECOND MTE) NIL)
	     (SETF (THIRD MTE) NIL))))
    (DOLIST (DECL METH-COMB)
      (LET ((TYPE (CAR DECL)) (ORDER (CADR DECL)) ELEM)
	;; Don't error-check TYPE now, its definition might not be loaded yet
	(DOLIST (MSG (CDDR DECL))
	  (OR (SETQ ELEM (ASSQ MSG (FLAVOR-METHOD-TABLE FL)))
	      (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
	  (SETF (SECOND ELEM) TYPE)
	  (SETF (THIRD ELEM) ORDER))))
    (IF *JUST-COMPILING*
	(COMPILATION-DEFINE-FLAVOR FLAVOR-NAME FL)
      ;; Make this a depended-on-by of its depends-on, or remember to do it later in
      ;; the case of depends-on's not yet defined.
      (DOLIST (COMPONENT-FLAVOR COMPONENT-FLAVORS)
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ FFL (GET COMPONENT-FLAVOR 'FLAVOR))
		 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
		(T (PUSH (CONS COMPONENT-FLAVOR FLAVOR-NAME)
			 *FLAVOR-PENDING-DEPENDS*)))))
      ;; Likewise for its includes
      (DOLIST (INCLUDED-FLAVOR (FLAVOR-INCLUDES FL))
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ FFL (GET INCLUDED-FLAVOR 'FLAVOR))
		 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
		(T (PUSH (CONS INCLUDED-FLAVOR FLAVOR-NAME)
			 *FLAVOR-PENDING-DEPENDS*)))))
      ;; If someone depends on this flavor, which wasn't defined until now, link them up.
      ;; If that flavor was flavor-composed, recompose it now.
      (WITHOUT-INTERRUPTS
	(DOLIST (X *FLAVOR-PENDING-DEPENDS*)
	  (COND ((EQ (CAR X) FLAVOR-NAME)
		 (OR (MEMQ (CDR X) (FLAVOR-DEPENDED-ON-BY FL))
		     (PUSH (CDR X) (FLAVOR-DEPENDED-ON-BY FL)))
		 (SETQ *FLAVOR-PENDING-DEPENDS*
		       (DELQ X *FLAVOR-PENDING-DEPENDS*))))))
      (PUTPROP FLAVOR-NAME FL 'FLAVOR)
      (REMPROP FLAVOR-NAME 'UNDEFINED-FLAVOR)
      ;; Now, if the flavor was redefined in a way that changes the methods but doesn't
      ;; invalidate old instances, we have to propagate some changes.
      (IF (AND ALREADY-EXISTS
	       (NOT IDENTICAL-COMPONENTS))
	  (PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FLAVOR-NAME)
	;; If the methods and instances are ok but other things have changed, notice that too.
	(OR (AND (EQUAL OLD-SPECIAL-IVS
			(FLAVOR-SPECIAL-INSTANCE-VARIABLES FL))
		 (EQUAL OLD-DEFAULT-INIT-PLIST
			(GET (LOCF (FLAVOR-PLIST FL))
			     ':DEFAULT-INIT-PLIST))
		 (EQUAL OLD-LOCAL-IVS
			(FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
		 ;; Get a warning every time, if there is a variable
		 ;; that is globally special but not in a :SPECIAL-INSTANCE-VARIABLES
		 (NOT (DOLIST (IV (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
			;; Elements can be lists (var init)
			(IF (CONSP IV) (SETQ IV (CAR IV)))
			(AND (GET IV 'SPECIAL)
			     (NOT (MEMQ IV (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL)))
			     (RETURN T))))
		 (EQUAL OLD-INITTABLE-IVS
			(FLAVOR-INITTABLE-INSTANCE-VARIABLES FL))
		 (EQUAL OLD-DEFAULT-HANDLER (GET (LOCF (FLAVOR-PLIST FL)) ':DEFAULT-HANDLER))
		 (EQUAL OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET FL ':INSTANCE-AREA-FUNCTION))
		 (EQUAL OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET FL ':REQUIRED-INIT-KEYWORDS))
		 (EQUAL OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS FL)))
	    (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR-NAME))))
    FLAVOR-NAME))

))

; From file EHF.LISP PS:<L.WINDOW> OZ:
#8R EH#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; EHF  "

(DEFUN FERROR (SIGNAL-NAME &OPTIONAL FORMAT-STRING &REST ARGS)
  "Report an uncorrectable error, using FORMAT-STRING and ARGS to print the message.
SIGNAL-NAME is a signal name or condition flavor name to be signalled,
or else a condition name to include in the signal, or NIL for none in particular."
  (SIGNAL-CONDITION
    (IF (STRINGP SIGNAL-NAME)
	;; Symbolics calling sequence has no condition; 1st arg is really the format string.
	(FUNCALL 'MAKE-CONDITION 'FERROR ':FORMAT-STRING SIGNAL-NAME
		       ':FORMAT-ARGS (CONS FORMAT-STRING ARGS))
      (LEXPR-FUNCALL 'MAKE-CONDITION SIGNAL-NAME FORMAT-STRING ARGS))
    NIL T))

))
