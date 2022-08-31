;;; -*- Mode:Lisp; Readtable:T; Package:User; Base:8.; Patch-File:T -*-
;;; Patch file for System version 99.31
;;; Reason:
;;;  Change BUG-LISPM to BUG-CADR, add Bounded-I-Search, add AI,MX,BORAX to hostab
;;; Written 8-Dec-86 12:01:02 by Devon,
;;; while running on Marvin from band 1
;;; with System 99.27, CADR 4.3, Experimental ZMail 54.4, MIT-Specific 23.0, Experimental Macsyma 5.0, microcode 320, GC@2 Patched.



; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.309 at 8-Dec-86 12:06:09
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "

(DEFUN BUG (&OPTIONAL (PROGRAM 'CADR) TEXT CALL-EDITOR-ANYWAY)
  "Record a bug in PROGRAM.
If TEXT is omitted, or CALL-EDITOR-ANYWAY is T, an editor window is used.
CALL-EDITOR-ANYWAY can be a number; the cursor is initially positioned
that many characters from the beginning of the string TEXT.
With no arguments, you specify everything with the editor window."
  (MULTIPLE-VALUE-BIND (WHOM WHAT0)
      (PARSE-BUG-ARG PROGRAM)
    (WHEN TEXT
      (IF (NUMBERP CALL-EDITOR-ANYWAY)
	  (SETQ CALL-EDITOR-ANYWAY
		(+ CALL-EDITOR-ANYWAY
		   1 (STRING-LENGTH WHAT0))))
      (SETQ WHAT0 (STRING-APPEND WHAT0 #/NEWLINE TEXT)))
    (MAIL WHOM WHAT0 (OR (NULL TEXT) CALL-EDITOR-ANYWAY))))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.309 at 8-Dec-86 12:06:40
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(DEFUN PARSE-BUG-ARG (WHO)
  (VALUES (STRING-APPEND "BUG-" WHO #/@ *HOST-FOR-BUG-REPORTS*)
	  (LET ((S (FORMAT NIL "In~:[ ~A in~;~*~] ~A, on ~A (~A):~2%"
			       (STRING-EQUAL WHO "CADR") WHO
			       (SI:SYSTEM-VERSION-INFO)
			       SI:LOCAL-PRETTY-HOST-NAME
			       (machine-type))))
	    ;; Fill to fit within a 75-column line
	    (LOOP WITH LINE-START = 0
		  FOR START = 0 THEN (+ COMMA-POS 2)
		  AS PREV-COMMA-POS = NIL THEN COMMA-POS
		  AS COMMA-POS = (STRING-SEARCH ", " S START)
	       WHEN (> (- (OR COMMA-POS (STRING-LENGTH S)) LINE-START) 72.)
	         UNLESS (NULL PREV-COMMA-POS)
		   DO (SETF (CHAR S (1+ PREV-COMMA-POS)) #/NEWLINE)
		      (when (> prev-comma-pos line-start)
			(SETQ LINE-START (+ PREV-COMMA-POS 2))
			(SETQ COMMA-POS PREV-COMMA-POS))
	       UNTIL (NULL COMMA-POS))
	    S)))

))

; From file OZ:KANSAS:<L.ZWEI>DIRED.LISP.309 at 8-Dec-86 12:06:44
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DIRED  "


(DEFCOM COM-BUG "Setup mail buffer for sending a bug report, arg prompts for type" ()
  (LET (WHO WHAT)
    (SETQ WHO (COMPLETING-READ-FROM-MINI-BUFFER
		"Report bug to BUG- (default CADR)"
		(SUBSET #'(LAMBDA (ELT) (NOT (EQUAL (CAR ELT) "Other")))
			*ZMAIL-BUG-LIST*)
		T NIL))
    (IF (CONSP WHO) (SETQ WHO (CAR WHO)))
    (AND (EQUAL WHO "") (SETQ WHO 'CADR))
    (MULTIPLE-VALUE (WHO WHAT)
      (PARSE-BUG-ARG WHO))
    (COM-MAIL-INTERNAL '*DEFAULT-ZMACS-BUG-TEMPLATE* WHO WHAT)))

;;; Create a buffer, put it in text mode, initialize to the right thing, and return.
;;; RE-INIT-P may be T meaning just initialize,
;;; or it can be a variable whose value (if non nil) is a DEFINE-MAIL-TEMPLATE template.

))

; From file OZ:KANSAS:<L.EH>EHF.LISP.228 at 8-Dec-86 12:06:56
#8R EH#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "EH")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: EH; EHF  "


(defmethod (condition :bug-report-recipient-system) ()
  "CADR")					;We leave BUG-LISPM to the slimy world

))

; From file AI: DEVON; ZMACS LISP at 8-Dec-86 12:16:37
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "AI: DEVON; ZMACS"

; Patch I-Search to not go beyond initial bounds of region if we started with one,
; and gave a positive numeric argument.  Thus c-U c-S and c-U c-R are now useful.
; I made ZWEI-SEARCH a tad more tasteful but decided not to gratuituously munge it.
; I added an optional argument to INCREMENTAL-SEARCH to support bounded searching,
; it is a cons of (first-bp . last-bp) of the region in which we want to search.
; It would be a giant pain in the ass to make this run under symbolics, grump.

(DEFUN INCREMENTAL-SEARCH (REVERSE-P &OPTIONAL BOUNDS &AUX (ORIG-PT (COPY-BP (POINT))))
  (SELECT-WINDOW *WINDOW*)
  (SEND #-symbolics *QUERY-IO*
	#+symbolics *TYPEIN-WINDOW* :FRESH-LINE)	;Necessary if in the mini-buffer
  (UNWIND-PROTECT
   (TYPEIN-LINE-ACTIVATE
    (PROG (CHAR			   ; The current command.
	   XCHAR		   ; Upcase version of character
	   MUST-REDIS		   ; T => The echo-area must be completely redisplayed.
	   (P 0)		   ; The stack pointer into *IS-BP*, etc. for input and rubout
           (P1 0)                  ; The pointer for which search we are doing.
                                   ; Can never exceed P.
           SUPPRESSED-REDISPLAY    ; T if the last input char was read before
                                   ; redisplay had a chance to finish.
                                   ;  A G read that way acts like a failing search quit.
	   BP1			   ; Aux BP used for actual searching.
           NEW-BP
           TIME-OUT                ; Set by SEARCH when it times out so we can check input.
           INPUT-DONE              ; An altmode or control char has been seen.
                                   ; Do not look for input any more; just search, then exit.
;	   (TV:KBD-INTERCEPTED-CHARACTERS
;	     (COPYLIST TV:KBD-INTERCEPTED-CHARACTERS))
           )
;	  (SETQ TV:KBD-INTERCEPTED-CHARACTERS
;		(DELQ (ASSQ #/ABORT TV:KBD-INTERCEPTED-CHARACTERS)
;		      TV:KBD-INTERCEPTED-CHARACTERS))
	  (STORE-ARRAY-LEADER 0 *IS-STRING* 0)	   ; Clear out the search string.
          (SETF (AREF *IS-STATUS* 0) T)   ; Initialize the stacks.
          (SETF (AREF *IS-REVERSE-P* 0) REVERSE-P)
          (SETF (AREF *IS-OPERATION* 0) :NORMAL)
          (SETF (AREF *IS-POINTER* 0) 0)
          (SETF (AREF *IS-BP* 0) (COPY-BP (POINT)))
	  (SETQ MUST-REDIS T)	   ; Initially we must redisplay.
	  (GO CHECK-FOR-INPUT)

          ;; Come here if there is input, or nothing to do until there is input.
    INPUT (SETQ SUPPRESSED-REDISPLAY NIL)
	  (AND (WINDOW-READY-P *WINDOW*) 	;In case of minibuffer
	       (REDISPLAY *WINDOW* :POINT))	; Redisplay point position while waiting.
          (IF (= (WINDOW-REDISPLAY-DEGREE *WINDOW*) DIS-NONE)
	      (REDISPLAY-MODE-LINE)		;Update indication of more above or below.
	    (SETQ SUPPRESSED-REDISPLAY T))
	  (IF SUPPRESSED-REDISPLAY
	      (SETQ CHAR (TYI-WITH-SCROLLING NIL T))
	    ;; If must wait for input, make the window's blinker blink
	    ;; even though not selected.
	    (UNWIND-PROTECT
	      (PROGN
		(SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-VISIBILITY :BLINK)
		(SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-DESELECTED-VISIBILITY :BLINK)
		(SETQ CHAR (TYI-WITH-SCROLLING NIL T)))
	      (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-VISIBILITY
		    (IF (EQ *WINDOW* TV:SELECTED-WINDOW)
			:BLINK
		      (TV:SHEET-EXPOSED-P *WINDOW*)))
	      (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-DESELECTED-VISIBILITY :ON)))
	  (WHEN (LISTP CHAR)
	    (SEND *STANDARD-INPUT* :UNTYI CHAR)
	    (SETQ INPUT-DONE T)
	    (GO CHECK-FOR-INPUT))
	  (SETQ XCHAR (CHAR-UPCASE CHAR))
	  (COND ((NOT (OR ( (CHAR-BITS CHAR) 0) (TV:CHAR-MOUSE-P CHAR)
			  (MEMQ CHAR '(#/ #/END #/RUBOUT #/HELP #/ABORT #/CLEAR-INPUT))))
		 (GO NORMAL))
		((MEMQ XCHAR '(#/C-S #/C-R))
		 (PUSH-ISEARCH-STATUS)
		 (SETF (AREF *IS-OPERATION* P) :REPEAT)
		 (LET ((NEW-REVERSE-P (= XCHAR #/C-R)))
		   (COND   ;; In reverse mode, just go to forward.
		     ((NEQ (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
		      (SETF (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
		      (SETQ MUST-REDIS T)
		      (SETF (AREF *IS-OPERATION* P) :REVERSE))
		     ((ZEROP (AREF *IS-POINTER* P))
		      (LET ((STRING (STRING (OR (CAAR *SEARCH-RING*) (BARF)))))
			(COPY-ARRAY-CONTENTS STRING *IS-STRING*)
			(SETF (AREF *IS-POINTER* P) (ARRAY-ACTIVE-LENGTH STRING)))
		      (SETQ MUST-REDIS T))))
		 (GO CHECK-FOR-INPUT))
		((= XCHAR #/C-Q)
		 (LET ((NEW-CH (SEND STANDARD-INPUT :TYI)))
		   (SETQ CHAR (IF (CHAR-BIT NEW-CH :CONTROL)
				  (LOGAND #o37 (CHAR-CODE NEW-CH))
				  (CHAR-CODE NEW-CH))))
		 (GO NORMAL))
		((= CHAR #/HELP)
		 (PRINT-DOC :FULL *CURRENT-COMMAND*)
		 (SEND *STANDARD-INPUT* :UNTYI (SEND *STANDARD-INPUT* :ANY-TYI))
		 (GO INPUT))
		((OR (= XCHAR #/C-G) (= CHAR #/ABORT))
		 (BEEP)
		 (COND ((AND (OR SUPPRESSED-REDISPLAY (NEQ (AREF *IS-STATUS* P) T))
			     (PLUSP P))
			;; G in other than a successful search
			;; rubs out until it becomes successful.
			(SETQ P (DO ((P (1- P) (1- P)))
				    ((EQ (AREF *IS-STATUS* P) T) P)))
			(SETQ P1 (MIN P P1) MUST-REDIS T)
			(GO CHECK-FOR-INPUT))
		       (T
			(MOVE-BP (POINT) (AREF *IS-BP* 0))
			(SEND *QUERY-IO* :MAKE-COMPLETE)
			(RETURN))))
		((OR (= CHAR #/) (= CHAR #/END))
		 (AND (ZEROP P)
		      ;; Call string search, and make self-doc print the right thing there.
		      (LET ((*CURRENT-COMMAND* 'COM-STRING-SEARCH-INTERNAL))
			(RETURN (COM-STRING-SEARCH-INTERNAL REVERSE-P NIL NIL NIL))))
		 (SETQ INPUT-DONE T)
		 (GO CHECK-FOR-INPUT))
		((OR (= CHAR #/RUBOUT) (= CHAR #/CLEAR-INPUT))
		 ;; Clear-input rubs out all the way.  Set P to 1 and let it be decremented.
		 (IF (= CHAR #/CLEAR-INPUT) (SETQ P 1))
		 (COND (( P 0)	   ; If he over-rubbed out,
			(BEEP)	   ;   that is an error.
			(GO CHECK-FOR-INPUT))
		       (T
			;; Rubout pops all of these PDLs.
			(SETQ P (1- P))
			(SETQ P1 (MIN P P1))
			(SETQ MUST-REDIS T)
			(GO CHECK-FOR-INPUT))))
		(T
		 (SEND *STANDARD-INPUT* :UNTYI CHAR)
		 (SETQ INPUT-DONE T)
		 (GO CHECK-FOR-INPUT)))
	  (FERROR NIL "A clause fell through.")

          ;; Normal chars to be searched for come here.
   NORMAL (OR MUST-REDIS (FORMAT *QUERY-IO* "~C" CHAR))
          (PUSH-ISEARCH-STATUS)
          (LET ((IDX (AREF *IS-POINTER* P)))
	    (AND ( IDX (ARRAY-LENGTH *IS-STRING*))
		 (ADJUST-ARRAY-SIZE *IS-STRING* (+ IDX 100)))
	    (SETF (AREF *IS-STRING* IDX) CHAR)
	    (SETF (AREF *IS-POINTER* P) (1+ IDX)))
          (SETF (AREF *IS-OPERATION* P) :NORMAL)
          ;; Come here after possibly processing input to update the search tables
          ;; to search for a while.  First, if necessary and not suppressed
          ;; update the search string displayed in the echo area.
   CHECK-FOR-INPUT
          ;; If there is input available, go read it.
          ;; Otherwise, do work if there is work to be done.
          (AND (NOT INPUT-DONE)
               (SEND *STANDARD-INPUT* :LISTEN)
               (GO INPUT))
          ;; Now do some work for a while, then go back to CHECK-FOR-INPUT.
          (COND (MUST-REDIS
                 (SETQ MUST-REDIS NIL)
		 (FORMAT *QUERY-IO* "~&~:|")
                 (OR (AREF *IS-STATUS* P1) (FORMAT *QUERY-IO* "Failing "))
                 (AND (AREF *IS-REVERSE-P* P) (FORMAT *QUERY-IO* "Reverse "))
		 (AND BOUNDS (FORMAT *QUERY-IO* "Bounded "))
                 (FORMAT *QUERY-IO* "I-Search: ")
                 (STORE-ARRAY-LEADER (AREF *IS-POINTER* P) *IS-STRING* 0)
                 (FORMAT *QUERY-IO* "~A" *IS-STRING*)))
          ;; Now see what sort of state the actual search is in, and what work there is to do.
          ;; P1 points at the level of the table on which we are actually working.
          (SETF BP1 (AREF *IS-BP* P1))
          ;; Display point at the end of the last search level which has succeeded.
          (DO ((P0 P1 (1- P0)))
              ((EQ (AREF *IS-STATUS* P0) T)
               (MOVE-BP (POINT) (AREF *IS-BP* P0))))
          (MUST-REDISPLAY *WINDOW* DIS-BPS)
          (COND ((EQ (AREF *IS-STATUS* P1) :GO)
                 (STORE-ARRAY-LEADER (AREF *IS-POINTER* P1) *IS-STRING* 0)
                 ;; If the level we were working on is still not finished,
                 ;; search at most 100 more lines.  If we find it or the end of the buffer
                 ;; before then, this level is determined and we can work on the next.
                 ;; Otherwise, we remain in the :GO state and do 100 more lines next time.
                 (MULTIPLE-VALUE (NEW-BP TIME-OUT)
                     (ZWEI-SEARCH BP1 *IS-STRING*
                             (AREF *IS-REVERSE-P* P1) NIL 100
			     (FUNCALL (IF (AREF *IS-REVERSE-P* P1) #'CAR #'CDR) BOUNDS)))
                 ;; What happened?
                 (COND (TIME-OUT
                        ;; Nothing determined.  NEW-BP has where we stopped.
                        (MOVE-BP BP1 NEW-BP)
			(DBP BP1))  ;Avoids missing occurrences, if string starts with CR
                       ((NULL NEW-BP)
                        ;; This search was determined to be a failure.
			(OR (AND (MEMQ :MACRO-ERROR
				       (SEND *STANDARD-INPUT* :WHICH-OPERATIONS))
				 (SEND *STANDARD-INPUT* :MACRO-ERROR))
			    (BEEP))
                        (SETF (AREF *IS-STATUS* P1) NIL)
                        (MOVE-BP BP1 (AREF *IS-BP* (1- P1)))
                        (MOVE-BP (POINT) BP1)
                        (SETQ MUST-REDIS T))
                       (T ;; This search level has succeeded.
                        (SETF (AREF *IS-STATUS* P1) T)
                        (MOVE-BP (POINT) NEW-BP)
                        (MOVE-BP BP1 NEW-BP))))
                (( P P1)
                 ;; This level is finished, but there are more pending levels typed ahead.
		 (INCF P1)
                 (SETF (AREF *IS-BP* P1) (SETQ BP1 (COPY-BP BP1)))
                 (STORE-ARRAY-LEADER (AREF *IS-POINTER* P1) *IS-STRING* 0)
                 (COND ((NULL (AREF *IS-STATUS* (1- P1)))
                        (COND ((NEQ (AREF *IS-OPERATION* P1) :REVERSE)
                               ;; A failing search remains so unless we reverse direction.
                               (SETF (AREF *IS-STATUS* P1) NIL))
                              (T ;; If we reverse direction, change prompt line.
                               (SETQ MUST-REDIS T))))
                       ((EQ (AREF *IS-OPERATION* P1) :NORMAL)
                        ;; Normal char to be searched for comes next.
                        ;; We must adjust the bp at which we start to search
                        ;; so as to allow the user to extend the string already found.
                        (MOVE-BP BP1
                                 (FORWARD-CHAR BP1
                                      (COND ((AREF *IS-REVERSE-P* P1)
                                             (COND ((= (ARRAY-ACTIVE-LENGTH *IS-STRING*) 1)
                                                    0)
                                                   (T (ARRAY-ACTIVE-LENGTH *IS-STRING*))))
                                            (T (- 1 (ARRAY-ACTIVE-LENGTH *IS-STRING*))))
                                      T)))))
                ;; If there is nothing left to do, and terminator seen, exit.
                (INPUT-DONE
                 (SEARCH-RING-PUSH
		   ;; Entries on the search ring should have a leader
		   (STRING-NCONC (MAKE-STRING (ARRAY-ACTIVE-LENGTH *IS-STRING*)
					      :FILL-POINTER 0)
				 *IS-STRING*)
		   'ZWEI-SEARCH)
                 (FORMAT *QUERY-IO* "")
		 (MAYBE-PUSH-POINT ORIG-PT)
		 (SELECT-WINDOW *WINDOW*)
                 (RETURN))
                ;; Nothing to do and no terminator, wait for input.
                (T (GO INPUT)))
          (GO CHECK-FOR-INPUT)
	  )
    (SETQ ORIG-PT NIL))
   (PROGN
     (IF ORIG-PT (MOVE-BP (POINT) ORIG-PT))
     (MUST-REDISPLAY *WINDOW* DIS-BPS)
     (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)))
  DIS-BPS)

(DEFCOM COM-INCREMENTAL-SEARCH "Search for character string.
An argument of Control-U invokes Bounded I-Search on the current defun or interval,
otherwise if the region is active Bounded I-Search is invoked on the region.
As characters are typed in the accumulated string is displayed and searched for.
You can use Rubout to cancel characters.
Altmode exits the search (but if search string is empty, it invokes String Search).
Use Q to quote, S to repeat the search with the same string,
R to search backwards.  If S or R is the first character typed, the
previous search string is used again." (KM)
   (INCREMENTAL-SEARCH (< *NUMERIC-ARG* 0) (BOUNDS-FOR-I-SEARCH)))

(DEFCOM COM-REVERSE-INCREMENTAL-SEARCH "Reverse search for character string.
An argument of Control-U invokes Bounded I-Search on the current defun or interval,
otherwise if the region is active Bounded I-Search is invoked on the region.
As characters are typed in the accumulated string is displayed and searched for.
You can use Rubout to cancel characters.
Altmode exits the search (but if search string is empty, it invokes String Search).
Use Q to quote, R to repeat the search with the same string,
S to search forwards.  If S or R is the first character typed, the
previous search string is used again." (KM)
   (INCREMENTAL-SEARCH (>= *NUMERIC-ARG* 0) (BOUNDS-FOR-I-SEARCH)))

(defun bounds-for-i-search ()
  (cond ((> (abs *numeric-arg*) 1)
	 (let ((int (defun-interval (point) 1 nil t t)))
	   (cons (interval-first-bp int)
		 (interval-last-bp int))))
	((window-mark-p *window*)
	 (funcall (if (BP-< (point) (mark)) #'cons #'xcons)
		  (copy-bp (point)) (copy-bp (mark))))
	(t
	 nil)))

; This is just like the EMACS compare windows command, which is potentially much nicer
; than SRCCOM if I can automate advancing over differences as well as SRCCOM does.
; Perhaps a manual mode which moves both window's cursors is what I want, something
; with a command to specify cursor 1 or 2, with 0 for both at once.
; But this will be a funny editing mode, which is a pain.  Oh well, it's a search
; mode like I-Search, we're searching for differences and identical sections.

;;;What it really needs is Super-Mouse-Left to select the window clicked on and
;;; put the point where it was clicked on and start a compare-windows.
;;;Also needed is a pattern-equivalence list on a per-buffer basis.
;;;And of course Super-Mouse-Middle to define simple string-equivalences.

#|
(defmacro typein-line (text)		;was not defined on earlier LMIT sys
  `(send *query-io* :string-out ,text))
|#

#+symbolics
(defmacro char (&rest rest)
	  `(aref ,@rest))

#+symbolics 
(defun frame-exposed-windows ()		;ZMACS is already a kludge.
  (mapcar (lambda (f) (send f :zwei-window))
	  (subset (lambda (w) (typep w 'zmacs-window-pane))
		  (send (window-frame *window*) :exposed-inferiors))))

(DEFCOM COM-COMPARE-WINDOWS
  "Compare two buffers, one in each window, highlighting the differences.
	Currently this is very simple-minded, it merely advances the cursors until
	a mismatch is found.  Aborting advances them to the last point matched.
	Maybe more like SRCCOM eventually." ()
	(let ((windows (frame-exposed-windows)))
	  (unless (= (length windows) 2) (barf "only works in 2 window mode."))
	  (let ((bp1 #-symbolics (send (first windows) :point)
		     #+symbolics (window-point (first windows)))
		(bp2 #-symbolics (send (second windows) :point)
		     #+symbolics (window-point (second windows)))
		(count 0))
	    (unwind-protect
		(typein-line
		      (if (block mismatch
			    (do ((line1 (bp-line bp1) (line-next line1))
				 (index1 (bp-index bp1) 0)
				 (line2 (bp-line bp2) (line-next line2))
				 (index2 (bp-index bp2) 0))
				((null line1)
				 (unless (null line2)
				   (return-from mismatch t)))
			      (do ((length
				     (min (- (line-length line1) index1)
					  (- (line-length line2) index2))
				     (1- length))
				   (char1 index1 (1+ char1))
				   (char2 index2 (1+ char2)))
				  ((zerop length)
				   (unless (and (= char1 (line-length line1))
						(= char2 (line-length line2)))
				     (incf count (- char1 index1))
				     (return-from mismatch t)))
				(unless (= (char line1 char1)
					   (char line2 char2))
				  (incf count (- char1 index1))
				  (return-from mismatch t)))
			      (incf count (1+ (- (line-length line1) index1)))))
			  "Mismatch"
			"No more differences"))
	      (let ((new-bp1 (forward-char bp1 count t))
		    (new-bp2 (forward-char bp2 count t)))
		(move-bp bp1 new-bp1)
		(move-bp bp2 new-bp2)))))
	(setf (window-redisplay-degree (other-window)) dis-bps)
	dis-bps)

; there's a flaky bug in forward-char to a bp at eob+1 which I can't seem to reproduce.

;;; the file AI:Devon;ZMACS ends here

))

; From file AI: DEVON; DEVON LISPM at 8-Dec-86 12:20:25
#10R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "AI: DEVON; DEVON LISPM"

(si:define-host "MIT-AI"
		 :host-names '("AI" "MITAI" "MIT-AI")
		 :machine-type :PDP10
		 :system-type :ITS
		 :arpa '(#o2006)
		 :chaos '(#o3130))
(si:define-host "MIT-MX"
		 :host-names '("KL" "MIT-KL" "MIT-MX" "MIT-PEACEKEEPER" "MX" "PEACEKEEPER")
		 :machine-type :PDP10
		 :system-type :ITS
		 :arpa '(#o1006)
		 :chaos '(#o1440))
(si:define-host "MIT-BORAX"
		 :host-names '("BORAX" "MIT-BORAX")
		 :machine-type :vax-11//750
		 :system-type :unix
		 :lcs '(#o6145)
		 :chaos '(#o15101))
))
