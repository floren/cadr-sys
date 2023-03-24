;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:10; Patch-File:T -*-
;;; Patch file for System version 99.11
;;; Reason:
;;;  gc-immediately doesn't run pre-full-gc-initializations
;;;  multiple-value-bind compilation
;;;  format temporary string cache
;;;  stupid float printing bug
;;;  copy-value cdr-code lossage. compiler support
;;;  inspector won't highlight self-refs which would make it blow out
;;;  Host tables ALWAYS in base 8
;;;  Zmacs c-) bug
;;; Written 10/28/84 15:17:29 by RMS,
;;; while running on Lisp Machine Twenty-five from band 5
;;; with Experimental System 99.9, CADR 4.0, Experimental ZMail 54.2, MIT-Specific 23.0, microcode 320, GC@2.



; From file GC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN GC-IMMEDIATELY ()
  "Perform a complete garbage collection right away, running in this process.
It is not necessary to turn on automatic GC to use this function."
  (FULL-GC :NO-PRE-GC-INITIALIZATIONS T :NO-STATIC-REGIONS T :NO-RECOPYING T))

))

; From file GC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; GC  "

(DEFUN FULL-GC (&KEY NO-PRE-GC-INITIALIZATIONS NO-STATIC-REGIONS NO-RECOPYING DUPLICATE-PNAMES)
  "Do a complete batch-style garbage collection to make minimum size for saved band.
It is best to do this twice in a row to make sure that the used part of memory
is at the bottom of the address space.

If DUPLICATE-PNAMES is T, all equal pnames of symbols are collapsed.
This is only worth doing once for a given system version so it is off
by default.

Unless NO-STATIC-REGIONS is non-NIL, the existing full regions of
WORKING-STORAGE-AREA are marked as static when the GC is done.
Unless NO-RECOPYING is non-NIL, various structures are recopied after
the GC is done to make sure they are compact (to improve paging efficiency).
GC-IMMEDIATELY uses FULL-GC as a subroutine, supplying T for these three args."
  (WHEN ( (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
	      (GC-GET-COMMITTED-FREE-SPACE T NIL T))
	   (GET-FREE-SPACE-SIZE))
    (FORMAT *QUERY-IO* "~&There is probably not enough free space to garbage collect,
unless there is a lot of garbage to be freed.")
    (UNLESS (Y-OR-N-P "Try garbage collecting anyway? ")
      (RETURN-FROM FULL-GC NIL)))
  (UNLESS NO-PRE-GC-INITIALIZATIONS
    (INITIALIZATIONS 'FULL-GC-INITIALIZATION-LIST T))
  ;; For extra reduction in size of band, reset all temporary areas.
  ;; Do this first, since this may free up other things that they point to.
#|
  This causes lossage since things still point to them!
  (UNLESS NO-TEMPORARY-AREAS
    (DO ((AREA FIRST-FULL-GC-AREA (1+ AREA)))
	((= AREA SIZE-OF-AREA-ARRAYS))
      (IF (AREA-TEMPORARY-P AREA)
	  (RESET-TEMPORARY-AREA AREA))))
|#
  (WHEN DUPLICATE-PNAMES
    (COLLAPSE-DUPLICATE-PNAMES))
  (UNLESS NO-STATIC-REGIONS
    (MAKE-AREA-DYNAMIC WORKING-STORAGE-AREA))
  (WITH-LOCK (GC-FLIP-LOCK)
    (PROCESS-DISABLE GC-PROCESS)
    ;; We assume that incremental GC has not been being used,
    ;; so if oldspace exists, we are already after a (GC-FLIP-NOW).
    (OR GC-OLDSPACE-EXISTS (GC-FLIP-NOW))
    ;; Touch all interned symbols and their pnames,
    ;; to get them in a good order for paging.
    ;; This is really only necessary if NR-SYM and P-N-STRING are being GC'd.
    (LET (TEM)
      (DOLIST (P *ALL-PACKAGES*)
	(DO-LOCAL-SYMBOLS (SYM P)
	  (SETQ TEM (LENGTH (SYMBOL-NAME SYM))))))
    (GC-RECLAIM-OLDSPACE)
    (UNLESS NO-RECOPYING
      (INITIALIZATIONS 'AFTER-FULL-GC-INITIALIZATION-LIST T))
    (UNLESS NO-STATIC-REGIONS
      (MAKE-AREA-REGIONS-STATIC WORKING-STORAGE-AREA))))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (:PROPERTY MULTIPLE-VALUE-BIND P1) (FORM)
  (LET ((VARIABLES (CADR FORM))
	(VARS VARS)
	OUTER-VARS
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(THIS-FRAME-DECLARATIONS NIL)
	(M-V-FORM (CADDR FORM))
	(BODY (CDDDR FORM)))
    (SETF (VALUES BODY THIS-FRAME-DECLARATIONS)
;>> need to pass environment into extract-declarations here
	  (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL NIL))
    (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
    (SETQ OUTER-VARS VARS)
    (SETQ TLEVEL NIL)
    ;; P1 the m-v-returning-form outside the bindings we make.
    (SETQ M-V-FORM (P1 M-V-FORM))
    ;; The code should initialize each variable by popping off the stack.
    ;; The values will be in forward order so we must pop in reverse order.
    (SETQ VARIABLES (MAPCAR #'(LAMBDA (V) `(,V (%POP))) VARIABLES))
    (P1SBIND VARIABLES 'FEF-ARG-INTERNAL-AUX T T THIS-FRAME-DECLARATIONS)
    (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
    ;; Return something with the same sort of arguments a LET has when given to pass 2,
    ;; but with the multiple value producing form as an additional argument at the front.
    `(,(CAR FORM) ,M-V-FORM ,VARIABLES ,OUTER-VARS ,VARS
      . ,(CDDDDR (P1V `(LET () . ,BODY))))))

))

; From file QCP2.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP2  "

(DEFUN (:PROPERTY MULTIPLE-VALUE-BIND P2) (TAIL DEST)
  ;; The first "argument" is the multiple-value producing form.
  (LET ((MVFORM (CAR TAIL))
	(TAIL (CDR TAIL))   ; Remove that, and we have what looks like
			    ; the arguments that a LET would have in pass 2.
	NBINDS)
    (LET* ((VLIST (CAR TAIL))
	   (MVTARGET (LENGTH VLIST))
	   (VARS (SECOND TAIL)))
      ;; Compile the form to leave N things on the stack.
      ;; If it fails to do so, then it left only one, so push the other N-1.
      (AND (P2MV MVFORM 'D-PDL MVTARGET)
	   (DO ((I 1 (1+ I))) ((= I MVTARGET))
	     (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
      (MKPDLLVL (+ PDLLVL MVTARGET))		;say that they are there.
      ;; Now pop them off, binding the variables to them.
      ;; Note that the vlist contains the variables
      ;; in the original order,
      ;; each with an initialization of (%POP).
      (SETQ NBINDS (P2PBIND VLIST (THIRD TAIL)))
      ;; Record that they were popped. -- 9/8/84
      ;; This is needed because P2PBIND binds PDLLVL.
      (MKPDLLVL (- PDLLVL MVTARGET)))
    (P2LET-INTERNAL (SECOND TAIL) NBINDS TAIL DEST)))

))

; From file FORMAT.LISP OZ:<L.IO> OZ:
#8R FORMAT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS)
  (WHEN (EQ (CAR-SAFE ARG) :MOUSE-BUTTON) (SETQ ARG (CADR ARG)))
  (SETQ ARG (CLI:CHARACTER ARG)
	BITS (CHAR-BITS ARG))
  (FLET ((PRINT-BITS (BITS)
	   (AND (BIT-TEST CHAR-HYPER-BIT BITS)
		(SEND *STANDARD-OUTPUT* ':STRING-OUT "Hyper-"))
	   (AND (BIT-TEST CHAR-SUPER-BIT BITS)
		(SEND *STANDARD-OUTPUT* ':STRING-OUT "Super-"))
	   (AND (BIT-TEST CHAR-CONTROL-BIT BITS)
		(SEND *STANDARD-OUTPUT* ':STRING-OUT "Control-"))
	   (AND (BIT-TEST CHAR-META-BIT BITS)
		(SEND *STANDARD-OUTPUT* ':STRING-OUT "Meta-"))))
    (COND ((TV:CHAR-MOUSE-P ARG)
	   (IF (AND (NOT COLON-FLAG) ATSIGN-FLAG)
	       (PRINC "#\"))
	   (PRINT-BITS BITS)
	   (SETF (CHAR-BITS ARG) 0)
	   (IF (AND (NOT COLON-FLAG) ATSIGN-FLAG)
	       (IF (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		   (PRINC CHNAME)
		 (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
	     (SEND *STANDARD-OUTPUT* ':STRING-OUT "Mouse-")
	     (SEND *STANDARD-OUTPUT* ':STRING-OUT (NTH (LDB %%KBD-MOUSE-BUTTON ARG)
						       '("Left" "Middle" "Right")))
	     (IF (SETQ CHNAME (NTH (SETQ BITS (LDB %%KBD-MOUSE-N-CLICKS ARG))
				   '("" "-Twice" "-Thrice")))
		 (SEND *STANDARD-OUTPUT* ':STRING-OUT CHNAME)
	       (SEND *STANDARD-OUTPUT* ':TYO #/-)
	       (ENGLISH-PRINT (1+ BITS))
	       (SEND *STANDARD-OUTPUT* ':STRING-OUT "-times"))))
	  ((NOT COLON-FLAG)
	   ;; If @ flag or if control bits, we want to use characters' names.
	   (IF (OR ATSIGN-FLAG (NOT (ZEROP BITS)))
	       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (CHAR-CODE ARG))))
	   ;; Print an appropriate reader macro if @C.
	   (IF ATSIGN-FLAG (PRINC "#\"))
	   (IF (NOT (ZEROP BITS))
	       ;; For efficiency, don't send :string-out message just for null string.
	       (SEND *STANDARD-OUTPUT*
		     ':STRING-OUT
		     (NTH BITS
			  '("" "c-" "m-" "c-m-"
			    "s-" "c-s-" "m-s-" "c-m-s-"
			    "h-" "c-h-" "m-h-" "c-m-h-"
			    "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
	   (IF CHNAME
	       (PROGN
		 (SETQ CHNAME (SYMBOL-NAME CHNAME))
		 (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE (AREF CHNAME 0)))
		 (DO ((LEN (LENGTH CHNAME))
		      (I 1 (1+ I)))
		     ((= I LEN))
		   (SEND *STANDARD-OUTPUT* :TYO (CHAR-DOWNCASE (AREF CHNAME I)))))
	     (AND ATSIGN-FLAG
		  (NOT (ZEROP BITS))
		  (WHEN ( #/a (CHAR-CODE ARG) #/z)
		    (SEND *STANDARD-OUTPUT* ':STRING-OUT "sh-")
		    (SETQ ARG (CHAR-UPCASE ARG)))
		  (IF (SI:CHARACTER-NEEDS-QUOTING-P (CHAR-CODE ARG))
		      (TYO (SI:PTTBL-SLASH *READTABLE*))))
	     (SEND *STANDARD-OUTPUT* ':TYO (CHAR-CODE ARG))))
	  (T
	   (PRINT-BITS BITS)
	   (SETQ ARG (INT-CHAR (CHAR-CODE ARG)))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (SETQ CHNAME (SYMBOL-NAME CHNAME))
		  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE (AREF CHNAME 0)))
		  (DO ((LEN (LENGTH CHNAME))
		       (I 1 (1+ I)))
		      ((= I LEN))
		    (SEND *STANDARD-OUTPUT* :TYO (CHAR-DOWNCASE (AREF CHNAME I))))
		  (AND ATSIGN-FLAG (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND ATSIGN-FLAG (< ARG #o40) ( ARG #/))
		  (SEND *STANDARD-OUTPUT* ':TYO ARG)
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
		 ((AND ( #/a ARG #/z)
		       (NOT (ZEROP BITS)))
		  (SEND *STANDARD-OUTPUT* ':STRING-OUT "Shift-")
		  (SEND *STANDARD-OUTPUT* ':TYO (CHAR-UPCASE ARG)))
                 (T (SEND *STANDARD-OUTPUT* ':TYO ARG)))))))

))

; From file PRINT.LISP OZ:<L.IO> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(defun scale-flonum (x &aux (short (typep X 'short-float)) tem expt wastoobig)
  (setq expt (truncate (// (float-exponent x) (log 10s0 2s0))))
  (tagbody
      again
	 (if (minusp expt)
	     (setq tem (* x (aref powers-of-10f0-table (- expt))))
	   (setq tem (// x (aref powers-of-10f0-table expt))))
	 (cond ((and ( tem -10s0) (not wastoobig)) (incf expt) (go again))
	       ((> tem -1s0)  (decf expt) (setq wastoobig t) (go again))
	       (t (return-from scale-flonum (values (- (if short (float tem 0s0) tem))
						    expt))))))

))

; From file LMMAC.LISP OZ:<L.SYS2> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; LMMAC  "

(DEFUN COPY-VALUE (TO-CELL FROM-CELL)
  "Copy whatever value is in FROM-CELL into TO-CELL."
  (%P-STORE-CDR-CODE TO-CELL
		     (PROG1 (%P-CDR-CODE FROM-CELL)
			    (%BLT-TYPED FROM-CELL TO-CELL 1 0))))

))

; From file QCP1.LISP OZ:<L.SYS> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; QCP1  "

(DEFUN (:PROPERTY SI:COPY-VALUE P1) (FORM)
  (LET ((TO-CELL (CADR FORM))
	(FROM-CELL (CADDR FORM)))
    (P1 `(%BLT-TYPED ,FROM-CELL ,TO-CELL 1 0))))

))

; From file INSPCT.LISP OZ:<L.WINDOW> OZ:
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; INSPCT  "

(DEFUN PRINT-FEF-INSTRUCTION (PC FEF-AND-PC-IDX *STANDARD-OUTPUT* ITEM-NO
			      &AUX (FEF (FIRST FEF-AND-PC-IDX))
				   (PC-IDX (SECOND FEF-AND-PC-IDX)))
  (SEND *STANDARD-OUTPUT* :STRING-OUT (IF (EQ ITEM-NO PC-IDX) "=> " "   "))
  (LET ((COMPILER:DISASSEMBLE-OBJECT-OUTPUT-FUN
	 #'(LAMBDA (OBJ PREFIX LOC FUN-P)
	     (IF ( (%P-DATA-TYPE LOC) DTP-SELF-REF-POINTER)
		 (SEND *STANDARD-OUTPUT* :ITEM1 (LIST OBJ LOC)
		       (IF FUN-P 'FEF-FUNCTION 'FEF-CONSTANT)
		       'PRINT-FEF-CONSTANT PREFIX)
	       (PRINC PREFIX *STANDARD-OUTPUT*)
	       (PRIN1 OBJ *STANDARD-OUTPUT*)))))
    (AND (NUMBERP PC) (COMPILER:DISASSEMBLE-INSTRUCTION FEF PC))))

))

; From file DISASS.LISP OZ:<L.SYS2> OZ:
#8R COMPILER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "COMPILER")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; DISASS  "

(DEFUN DISASSEMBLE-POINTER (FEF DISP PC &AUX CELL LOC PTR OFFSET TEM)
  (SETQ LOC (%MAKE-POINTER-OFFSET DTP-LOCATIVE FEF DISP))
  (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF DISP) DTP-SELF-REF-POINTER)
	 (MULTIPLE-VALUE-BIND (PTR COMPONENT-FLAVOR-FLAG)
	     (SI:FLAVOR-DECODE-SELF-REF-POINTER 
	       (OR (DISASSEMBLE-CURRENT-FLAVOR FEF PC)
		   (SI:FEF-FLAVOR-NAME FEF))
	       (%P-LDB-OFFSET %%Q-POINTER FEF DISP))
	   (IF (NULL PTR)
	       (SETQ CELL "self-ref-pointer " PTR (%P-LDB-OFFSET %%Q-POINTER FEF DISP))
	     (SETQ CELL (IF COMPONENT-FLAVOR-FLAG "mapping table for " ""))
	     (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
		 (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN PTR CELL LOC T)
	       (FORMAT T "~A~S" CELL PTR)
	       (IF (EQUAL CELL "")
		   (PRINC " in SELF"))))))
	((= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF DISP) DTP-EXTERNAL-VALUE-CELL-POINTER)
	 (SETQ PTR (%FIND-STRUCTURE-HEADER
		     (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF DISP)))
	       OFFSET (%POINTER-DIFFERENCE TEM PTR))
	 (COND ((SYMBOLP PTR)
		(SETQ CELL (NTH OFFSET '("@+0?? "
					 ""
					 "#'"
					 "@PLIST-HEAD-CELL "
					 "@PACKAGE-CELL "))))
	       ((CONSP PTR)
		(SETQ PTR (CAR PTR) CELL "#'"))
	       (T (SETQ CELL "")))
	 (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
	     (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN PTR CELL LOC T)
	   (FORMAT T "~A~S" CELL PTR)))
	(T
	 (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
	     (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN (CAR LOC) "'" LOC NIL)
	   (FORMAT T "'~S" (%P-CONTENTS-OFFSET FEF DISP))))))

))

;;; hacked by mly to what is below
;; From file FORMAT.LISP OZ:<L.IO> OZ:
;#8R FORMAT#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "


;(DEFVAR FORMAT-STRING-BUFFER-RESOURCE NIL)
;(DEFMACRO GET-FORMAT-BUFFER ()
;  `(OR (DO ((VAL)) (())
;	 (SETQ VAL FORMAT-STRING-BUFFER-RESOURCE)
;	 (WHEN (STORE-CONDITIONAL (LOCF FORMAT-STRING-BUFFER-RESOURCE) VAL NIL)
;	   (IF VAL (SETF (FILL-POINTER VAL) 0))
;	   (RETURN VAL)))
;       (MAKE-ARRAY 128 :TYPE 'ART-STRING :FILL-POINTER 0)))
;(DEFMACRO RETURN-FORMAT-BUFFER (VAR)
;  `(SETQ FORMAT-STRING-BUFFER-RESOURCE ,VAR))

;))

;; From file FORMAT.LISP OZ:<L.IO> OZ:
;#8R FORMAT#:
;(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

;(DEFUN FORMAT (STREAM CTL-STRING &REST ARGS)
;  "Format arguments according to a control string and print to a stream.
;/(If the stream is T, *STANDARD-OUTPUT* is used;
; if NIL, a string is returned containing the formatted text.)
;The control string is copied to the stream, but ~ indicates special formatting commands.
;~D  ~mincol,padchar,commacharD   Print number as a decimal integer.
;    ~:D  Print the comma character every three digits.
;    ~@D  Always print the sign.   ~:@D  Both.
;~O  Analogous to ~D, but prints in octal.
;~X  Analogous to ~D, but prints in hex.
;~B  Analogous to ~X, but prints in binary.
;~F  ~w,d,s,overflowchar,padcharF  Print float in nonexponential notation.
;    Multiplies by 10^s before printing if s is specified.
;    Prints in w positions, with d digits after the decimal point.
;    Pads on left with padchar if nec.  If number doesn't fit in w positions,
;    and overflowchar is specified, just fills the w positions with that character.
;~E  ~w,d,e,s,overflowchar,padchar,exptcharE   Print float in exponential notation.
;    Prints in w positions, with e digits of exponent.
;    If s (default is 1) is positive, prints s digits before point, d-s+1 after.
;    If s is zero, prints d digits after the point, and a zero before if there's room.
;    If s is negative, prints d digits after the point, of which the first -s are zeros.
;    If exptchar is specified, it is used to delimit the exponent
;    (instead of /"e/" or whatever.)
;    If overflowchar is specified, then if number doesn't fit in specified width,
;    or if exponent doesn't fit in e positions, field is filled with overflowchar instead.
;~G  Like ~E, but if number fits without exponent, prints without one.
;~$  ~w,x,y,z$ prints a floating-point number with exactly w (default 2) digits to right of
;     decimal, at least x (default 1) to left of decimal, right-justified in field y wide
;     padded with z.  @ print + sign.  : sign to left of padding.
;~R  ~R  Print number as an English cardinal number.
;    ~:R  English ordinal number.   ~@R  Roman numeral.   ~:@R  Old Roman numeral.
;    ~nR  Print number in radix n.  Thus ~8R = ~O, and ~10R = ~D.
;    Extra parameters are as for ~D (~n,mincol,padchar,commacharR).
;~A  Ascii output (PRINC).  Good for printing strings.  ~mincol,colinc,minpad,padcharA.
;    ~@A  Right-justify the string.   ~:A  Make NIL print as ().  ~:@A  Both.
;~S  Analogous to ~A, but uses PRIN1, not PRINC.
;~C  Print a character.  Mouse characters print in standard format.
;    ~C  Actual character, preceded by /"c-/", /"m-/", /"s-/" or /"h-/" if necessary.
;    ~:C  Format effectors print as names.  Names of control bits (/"Control-/") precede.
;    ~@C  Prints the character in READ format, using #\.
;    ~:@C  Like ~:C, but top/front/greek characters are followed by remark, e.g. /" (Top-S)/".
;~*  Ignore an argument.   ~n*  Ignore n arguments.   ~:n*  Back up n arguments (default 1).
;    ~n@* Go to argument n.
;~%  Insert a newline.     ~n%  Insert n newlines.
;~~  Insert a tilde.       ~n~  Insert n tildes.
;~|  Insert a form.        ~n|  Insert n forms.
;    ~:|  Do :CLEAR-SCREEN if the stream supports it, otherwise insert a form.   ~:n|  Similar.
;~<cr>  Ignore a CR and following whitespace in the control string.
;    ~:<cr> Ignore the CR, retain the whitespace.  ~@<cr> Retain the CR, ignore the whitespace.
;~&  Do a :FRESH-LINE.     ~n&  Do a FRESH-LINE, then insert n-1 newlines.
;~^  Terminate processing if no more arguments.  Within ~{...~}, just terminate the loop.
;    ~n;  Terminate if n is zero.  ~n,m;  Terminate if n=m.  ~n,m,p;  Terminate if nmp.
;    ~:^  When within ~:{...~}, ~^ terminates this iteration.  Use ~:^ to exit the loop.
;~T  ~mincol,colincT  Tab to column mincol+p*colinc, for the smallest integer p possible.
;    ~mincol,colinc:T  Same, but tabs in TV pixels rather than characters.
;    ~n@T  Insert n spaces.
;    ~n,colinc@T   Insert n spaces, then move 0 or more up to multiple of colinc.
;~Q  Apply next argument to no arguments.  ~a,b,c,...,zQ  Apply next argument to parameters
;    a,b,c,...z.  In (Q ...) form, apply argument to unevaled parameters.
;~P  Pluralize.  Insert /"s/", unless argument is 1.
;    ~:P  Use previous argument, not next one (i.e. do ~:* first).
;    ~@P  Insert /"y/" if argument is 1, otherwise insert /"ies/".   ~:@P  Both.
;~(  ~(...~)  Force lower case for the output generated within.
;    ~:(...~)  Similar but capitalize each word.
;    ~@(...~)  Similar but capitalize the first word.
;    ~:@(...~)  Similar but force all upper case.
;    ~1(...~)  Force first letter of first word to upper case, leave all else alone.
;~?  Indirect.  Uses up two args; first is a format string, second is args for it.
;    ~@? uses up one arg directly, as a format string, but it operates on
;    the remaining format args and can use them up.
;~<  ~mincol,colinc,minpad,padchar<str0~;str1~;...~;strn~>  Do formatting for all formatting
;    strings strj; then output all strings with padding between them at the ~; points.
;    Each padding point must have at least minpad padding characters.  Subject to that,
;    the total width must be at least mincol, and must be mincol+p*colinc for some p.
;    If str0 is followed by ~:; instead of ~;, then str0 is not normally output, and the
;    ~:; is not a padding point.  Instead, after the total width has been determined,
;    if the text will not fit into the current line of output, then str0 is output before
;    outputting the rest.  (Doesn't work when producing a string.)  An argument n (~:n;)
;    means that the text plus n more columns must fit to avoid outputting str0.  A second
;    argument m (~n,m:;) provides the line width to use instead of the stream's width.
;    ~:<  Also have a padding point at the left.  Hence ~n:<x~> right-justifies x in n columns.
;    ~@<  Also have a padding point at the right.   ~:@<  Both.   Hence ~n:@<x~> centers x.
;~[  ~[str0~;str1~;...~;strn~]  Select.  Argument selects one clause to do.  If argument is not
;    between 0 and n inclusive, then no alternative is performed.  If a parameter is given,
;    then use the parameter instead of an argument.  (The only useful one is /"#/".)
;    If the last string is preceded by ~:;, it is an /"else/" clause, and is processed if
;    no other string is selected.
;    One can also tag the clauses explicitly by giving arguments to ~;.  In this case the
;    first string must be null, and arguments to ~; tag the following string.  The
;    argument is matched against the list of parameters for each ~;.  One can get ranges
;    of tags by using ~:;.  Pairs of parameters serve as inclusive range limits.
;    A ~:; with no parameters is still an /"else/" clause.
;    Example:  ~[~'+,'-,'*,'////;operator~:'A,'Z,'a,'z;letter~:'0,'9;digit~:;other~]
;    will produce /"operator/", /"letter/", /"digit/", or /"other/" as appropriate.
;    ~:[iffalse~;iftrue~]  The argument selects the first clause if nil, the second if non-nil.
;    ~@[str~]  If the argument is non-nil, then it is not swallowed, and str is processed.
;    Otherwise, the nil is swallowed and str is ignored.  Thus ~@[~S~] will PRIN1 a
;    non-null thing.
;~{  ~{str~}  Use str as a format string for each element in the argument.  More generally,
;    the argument is a list of things to be used as successive arguments, and str is used
;    repeatedly as a format string until the arguments are exhausted (or ~^ is used).
;    Within the iteration the commands ~* and ~@* move among the iteration arguments,
;    not among all the arguments given to FORMAT.
;    ~n{str~} repeats the string at most n times.
;    Terminating with ~:} forces str to be processed at least once.
;    ~:{str}  The argument is a list of lists, and each repetition sees one sublist.
;    ~@{str}  All remaining arguments are used as the list.
;    ~:@{str}  Each remaining argument is a list.
;    If the str within a ~{ is empty, then an argument (which must be a string) is used.
;    This argument precedes any that are iterated over as loop arguments.
;~  ~str~ Successive lines within str are indented to align themselves with the column
;    at which str began. ie all text within str will lie to the right of the beginning of str
;In place of a numeric parameter, one may use V, which uses an argument to supply the number;
;or one may use #, which represents the number of arguments remaining to be processed;
;or one may use 'x, which uses the ascii value of x (good for pad characters).
;The control string may actually be a list of intermixed strings and sublists.
;In that case, the strings are printed literally.  The first atom in a sublist should be
;the name of a command, and remaining elements are parameters."
;  (LET-IF (NULL STREAM)
;	  ;; Only bind FORMAT-STRING if STREAM is NIL.  This avoids lossage if
;	  ;; FORMAT with a first arg of NIL calls FORMAT recursively (e.g. if
;	  ;; printing a named structure).
;	  ((FORMAT-STRING (GET-FORMAT-BUFFER)))
;    (LET-IF (STRINGP STREAM)
;	    ((FORMAT-STRING STREAM))
;      (LET ((*STANDARD-OUTPUT* (COND ((OR (NULL STREAM)
;					  (STRINGP STREAM)) 'FORMAT-STRING-STREAM)
;				     ((EQ STREAM T) *STANDARD-OUTPUT*)
;				     (T STREAM)))
;	    (FORMAT-ARGLIST ARGS)
;	    (LOOP-ARGLIST NIL))
;	(*CATCH 'FORMAT-/:^-POINT
;	  (*CATCH 'FORMAT-^-POINT
;            (COND ((STRINGP CTL-STRING)
;		   (FORMAT-CTL-STRING ARGS CTL-STRING))
;		  ((ERRORP CTL-STRING)
;		   (PRINC CTL-STRING))
;		  ((SYMBOLP CTL-STRING)
;		   (FORMAT-CTL-STRING ARGS (GET-PNAME CTL-STRING)))
;		  (T (DO ((CTL-STRING CTL-STRING (CDR CTL-STRING))) ((NULL CTL-STRING))
;		       (IF (STRINGP (CAR CTL-STRING))
;			   (SEND *STANDARD-OUTPUT* ':STRING-OUT (CAR CTL-STRING))
;			 (SETQ ARGS (FORMAT-CTL-LIST ARGS (CAR CTL-STRING)))))))))))
;      ;; Copy returned string out of temporary area and reclaim
;      (WHEN (NULL STREAM)			;return string or nil
;	(PROG1 (SUBSTRING FORMAT-STRING 0)
;	       (RETURN-FORMAT-BUFFER FORMAT-STRING)))))

;))

; From file FORMAT.LISP OZ:<L.IO> OZ: (237)
#10R FORMAT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "FORMAT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; FORMAT  "

(DEFVAR FORMAT-STRING-BUFFER-ARRAY NIL)

(defmacro get-format-buffer (variable make-array)
  `(or (do ((val)) (())
	 (setq val ,variable)
	 (when (%store-conditional (locf ,variable) val nil)
	   (if val (setf (fill-pointer val) 0))
	   (return val)))
       ,make-array))
(defmacro return-format-buffer (array variable)
  `(unless (%store-conditional (locf ,variable) nil ,array)
     (return-storage (prog1 ,array (setq ,array nil)))))

(defun (:select-method format-string-stream :FRESH-LINE) (ignore)
  (WHEN (NOT (OR (NULL FORMAT-STRING)
		 (ZEROP (STRING-LENGTH FORMAT-STRING))
		 (CHAR= (CHAR FORMAT-STRING (1- (STRING-LENGTH FORMAT-STRING))) #/NEWLINE)))
    (VECTOR-PUSH-EXTEND #/NEWLINE FORMAT-STRING)
    T))

(setf (documentation 'format 'function) (documentation 'format 'function))
(DEFUN FORMAT (STREAM CTL-STRING &REST ARGS)
  (LET-IF (NULL STREAM)
	  ;; Only bind FORMAT-STRING if STREAM is NIL.  This avoids lossage if
	  ;; FORMAT with a first arg of NIL calls FORMAT recursively (e.g. if
	  ;; printing a named structure).
	  ((FORMAT-STRING (get-format-buffer format-string-buffer-array
					     (make-string 128. :fill-pointer 0
							       :area format-temporary-area))))
    (LET-IF (STRINGP STREAM)
	    ((FORMAT-STRING STREAM))
      (LET ((*STANDARD-OUTPUT* (COND ((OR (NULL STREAM)
					  (STRINGP STREAM)) 'FORMAT-STRING-STREAM)
				     ((EQ STREAM T) *STANDARD-OUTPUT*)
				     (T STREAM)))
	    (FORMAT-ARGLIST ARGS)
	    (LOOP-ARGLIST NIL))
	(CATCH 'FORMAT-/:^-POINT
	  (CATCH 'FORMAT-^-POINT
            (COND ((STRINGP CTL-STRING)
		   (FORMAT-CTL-STRING ARGS CTL-STRING))
		  ((ERRORP CTL-STRING)
		   (PRINC CTL-STRING))
		  ((SYMBOLP CTL-STRING)
		   (FORMAT-CTL-STRING ARGS (GET-PNAME CTL-STRING)))
		  (T (DO ((CTL-STRING CTL-STRING (CDR CTL-STRING))) ((NULL CTL-STRING))
		       (IF (STRINGP (CAR CTL-STRING))
			   (SEND *STANDARD-OUTPUT* :STRING-OUT (CAR CTL-STRING))
			 (SETQ ARGS (FORMAT-CTL-LIST ARGS (CAR CTL-STRING)))))))))))
      ;; Copy returned string out of temporary area and reclaim
      (WHEN (NULL STREAM)			;return string or nil
	(PROG1 (SUBSTRING FORMAT-STRING 0)
	       (return-format-buffer format-string format-string-buffer-array)))))

))


; From file CHSAUX.LISP OZ:<L.NETWORK.CHAOS> OZ: (362)
#8R CHAOS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFUN GENERATE-HOST-TABLE-1 (INPUT-FILE OUTPUT-FILE)
  (LET ((*PACKAGE* (FIND-PACKAGE "CHAOS"))
	(*READ-BASE* 8) (*PRINT-BASE* 8) (*PRINT-RADIX* t)
	(*READTABLE* SI:STANDARD-READTABLE))
    (WITH-OPEN-FILE (OUTPUT-STREAM OUTPUT-FILE :DIRECTION :OUTPUT :CHARACTERS T)
      (FORMAT OUTPUT-STREAM "~
;;; -*- Mode: LISP;~@[ Package: ~A;~] Base: 8; Readtable:T -*-
;;; *** THIS FILE WAS AUTOMATICALLY GENERATED BY A PROGRAM, DO NOT EDIT IT ***
;;; Host table made from ~A~%"
	      SI:*FORCE-PACKAGE* (SEND (FS:PARSE-PATHNAME INPUT-FILE) :TRUENAME))
      (SI::WRITE-RESPONSIBILITY-COMMENT OUTPUT-STREAM)
      (GENERATE-HOST-TABLE-2 INPUT-FILE OUTPUT-STREAM)
      (WHEN (GET-SITE-OPTION :NON-CHAOS-HOST-TABLE-FILE)
	(GENERATE-HOST-TABLE-2 (GET-SITE-OPTION :NON-CHAOS-HOST-TABLE-FILE)
			       OUTPUT-STREAM)))))

(DEFUN GENERATE-HOST-TABLE-2 (INPUT-FILE OUTPUT-STREAM)
  (WITH-OPEN-FILE (INPUT-STREAM INPUT-FILE :DIRECTION :INPUT :CHARACTERS T)
    (DO ((LINE) (EOF)
	 (I) (J)
	 (NI) (NJ)
	 (HOSTL) (NAMEL) (DELIM))
	(NIL)
      (MULTIPLE-VALUE (LINE EOF)
	(SEND INPUT-STREAM :LINE-IN NIL))
      (AND EOF (RETURN))
      (MULTIPLE-VALUE (I J)
	(PARSE-HOST-TABLE-TOKEN LINE 0))
      (COND ((AND I (STRING-EQUAL LINE "HOST" :START1 I :END1 J))
	     ;; Host name
	     (MULTIPLE-VALUE (NI NJ)
	       (PARSE-HOST-TABLE-TOKEN LINE (1+ J)))
	     (MULTIPLE-VALUE (I J DELIM)
	       (PARSE-HOST-TABLE-TOKEN LINE (1+ NJ)))
	     (SETQ HOSTL (NCONS (SUBSTRING LINE NI NJ)))
	     (IF (CHAR= DELIM #/[)
		 (DO ((L NIL)
		      (I1) (J1))
		     ((CHAR= DELIM #/])
		      (INCF J)
		      (NREVERSE L))
		   (MULTIPLE-VALUE (I1 J1 DELIM)
		     (PARSE-HOST-TABLE-TOKEN LINE (1+ J)))
		   (IF (CHAR= DELIM #/SP)
		       (MULTIPLE-VALUE (I J DELIM)
			 (PARSE-HOST-TABLE-TOKEN LINE (1+ J1)))
		       (SETQ I I1 J J1 J1 I1))
		   (ADD-HOST-TABLE-ADDRESS LINE I1 J1 I J HOSTL))
		 (LET ((I1 I) (J1 J))
		   (IF (= DELIM #/SP)
		       (MULTIPLE-VALUE (I J)
			 (PARSE-HOST-TABLE-TOKEN LINE (1+ J)))
		       (SETQ I I1 J J1 J1 I1))
		   (ADD-HOST-TABLE-ADDRESS LINE I1 J1 I J HOSTL)))
;	     (COND ((OR (GET HOSTL :CHAOS)	;If there were any chaosnet addresses
;			;; Include some popular ARPA sites for speed in SUPDUP/TELNET, etc.
;			(SYS:MEMBER-EQUAL (CAR HOSTL) INCLUDED-NON-CHAOS-HOSTS))
	     (DOTIMES (K 2)
	       (MULTIPLE-VALUE (I J DELIM)
		 (PARSE-HOST-TABLE-TOKEN LINE (1+ J))))
	     (WHEN I
	       (PUTPROP HOSTL (INTERN (SUBSTRING LINE I J) "") :SYSTEM-TYPE))
	     (MULTIPLE-VALUE (I J DELIM)
	       (PARSE-HOST-TABLE-TOKEN LINE (1+ J)))
	     (WHEN I
	       (PUTPROP HOSTL (INTERN (SUBSTRING LINE I J) "") :MACHINE-TYPE))
	     (MULTIPLE-VALUE (I J DELIM)
	       (PARSE-HOST-TABLE-TOKEN LINE (1+ J)))
	     (OR I (SETQ DELIM -1))
	     (SETQ NAMEL (NCONS (CAR HOSTL)))
	     (AND (CHAR= DELIM #/[)
		  (DO () ((CHAR= DELIM #/])
			  (SETQ NAMEL (STABLE-SORT NAMEL
						   #'(LAMBDA (X Y)
						       (< (STRING-LENGTH X)
							  (STRING-LENGTH Y))))))
		    (MULTIPLE-VALUE (I J DELIM)
		      (PARSE-HOST-TABLE-TOKEN LINE (1+ J)))
		    (PUSH (SUBSTRING LINE I J) NAMEL)))
	     (PUTPROP HOSTL NAMEL :HOST-NAMES)
	     (LET ((*PACKAGE* (OR (FIND-PACKAGE SI:*FORCE-PACKAGE*) *PACKAGE*)))
	       (FORMAT OUTPUT-STREAM "(~S ~S~{~%  '~S '~S~})~2%"
		       'SI::DEFINE-HOST (CAR HOSTL) (CDR HOSTL))))))))

))

; From file COMA.LISP KANSAS:<L.ZWEI> OZ: (103)
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "

(DEFCOM COM-SHOW-LIST-START "Displays the start of the list which the point lies after." (KM)
  (LET ((BP (FORWARD-LIST (POINT) -1 NIL 0 NIL T)))
    (IF (NULL BP) (BARF "No list found before point.")
      (LET* ((END-BP (FORWARD-SEXP BP 1 NIL 0))
	     (CLOSE (INT-CHAR (CHAR-CODE (BP-CHARACTER-BEFORE END-BP))))
	     (BALANCE-LENGTH (MAX 1 (- (SEND *TYPEIN-WINDOW* :SIZE-IN-CHARACTERS) 25.))))
	(DO ((INDEX (BP-INDEX BP) (1+ INDEX))
	     (OPEN))
	    ((= (LIST-SYNTAX (SETQ OPEN (CHAR (BP-LINE BP) INDEX))) LIST-OPEN)
	     (TYPEIN-LINE "")		;clear line, since typein-line-more used if paren ok
	     (UNLESS (= (SECOND (ASSQ OPEN *MATCHING-DELIMITER-LIST*)) CLOSE)
	       (PROGN (BEEP) (TYPEIN-LINE "Non-matching parenthesis.~%")))))
	(DO ((STRING (MAKE-STRING 30. :FILL-POINTER 0)
		     (STRING-NCONC STRING (IF (EQ CH #/NEWLINE) "   " CH)))
	     (CH (BP-CH-CHARACTER BP)
;character lossage
		 (COND ((MEMQ (BP-CH-CHAR BP) *WHITESPACE-CHARS*)
			(COND ((AND (CHARACTERP CH)
				    (MEM #'CHAR-EQUAL CH '(#/SP #/NEWLINE #/TAB)))
			       "")
			      ((EQUAL CH "") "")
			      (T (BP-CH-CHARACTER BP))))
		       (T (BP-CH-CHARACTER BP))))
	     (BP (IBP BP) (IBP BP)))
	    ((OR (> (LENGTH STRING) BALANCE-LENGTH)
		 (BP-= BP END-BP))
	     (SETF (FILL-POINTER STRING) (MIN (FILL-POINTER STRING) BALANCE-LENGTH))
	     (TYPEIN-LINE-MORE "~A~:[ ...~] balances this paren" STRING (BP-= BP END-BP))
	     (RETURN-STORAGE (PROG1 STRING (SETQ STRING NIL)))))
	(MOVE-BP (POINT) END-BP))))
  DIS-BPS)

))

; From file CHSAUX.LISP KANSAS:<L.NETWORK.CHAOS> OZ: (363)
#8R CHAOS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "CHAOS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: NETWORK; CHAOS; CHSAUX  "

(DEFSUBST FCL-HOST (ELEM) (CAR ELEM))
(DEFSUBST FCL-CONN1 (ELEM) (CDADR ELEM))	; First CHAOS CONN in an element

(DEFUN FIND-LISPMS-LOGGED-IN-AS-USER (USER
				      &AUX (ELEMS (MAKE-FAST-CONNECTION-LIST
						    (LIST-ALL-NET-MACHINES :LISPM)
						    "FINGER" 1))
				      (HOST-LIST ()))
  "Return a list of (host objects of) lisp machines that USER is logged into."
  (DO ((OLD-TIME (TIME)))
      (NIL)
    (DOLIST (ELEM ELEMS)
      (LET* ((CONN (FCL-CONN1 ELEM))
	     (STATE (AND CONN (STATE CONN))))
	(COND ((NEQ STATE 'RFC-SENT-STATE)	;Still waiting
	       (AND (EQ STATE 'ANSWERED-STATE)	;Got something meaningful
		    (LET* ((PKT (GET-NEXT-PKT CONN))
			   (STR (PKT-STRING PKT)))
		      (AND (STRING-EQUAL STR USER :END1 (STRING-SEARCH-CHAR #/CR STR))
			   (PUSH (FCL-HOST ELEM) HOST-LIST))
		      (RETURN-PKT PKT)))
	       (SETQ ELEMS (DELQ ELEM ELEMS))
	       (WHEN CONN
		 (CLOSE-CONN CONN)
		 (REMOVE-CONN CONN))))))
      (OR ELEMS (RETURN NIL))			; Done with all of them
      (AND (> (TIME-DIFFERENCE (TIME) OLD-TIME) 240.)	;Allow 5 secs for this all
	   (RETURN NIL))
    (PROCESS-WAIT "Finger"
		  #'(LAMBDA (OLD-TIME ELEMS)
		      (OR (> (TIME-DIFFERENCE (TIME) OLD-TIME) 240.)
			  (DO ((ELEMS ELEMS (CDR ELEMS)))
			      ((NULL ELEMS) NIL)
			    (OR (EQ (STATE (FCL-CONN1 (CAR ELEMS))) 'RFC-SENT-STATE)
				(RETURN T)))))
		  OLD-TIME ELEMS))
  ;; Flush all outstanding connections
  (DOLIST (ELEM ELEMS)
    (REMOVE-CONN (FCL-CONN1 ELEM)))
  HOST-LIST)

))
