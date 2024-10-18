;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.11
;;; Reason:
;;;  In ZMacs Auto Fill Mode, if typing a space when cursor lay before the
;;;  last word on the line, and the typing of the space caused auto-fill-
;;;  hook to activate, the space would not self-insert.  ZWEI:AUTO-FILL-
;;;  HOOK failed to catch the case in which auto-filling left cursor at
;;;  end of the current line, not on the next one.
;;;  
;;;  Now, if the point ends up on the same line it started on, and 
;;;  (BP-INDEX *POINT*) returns the same thing as (LINE-LENGTH LINE), the
;;;  space character is explicitly re-inserted (it is being deleted in all
;;;  cases).
;;; Written 4-Aug-88 18:11:58 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Brahms' First from band 3
;;; with Experimental System 126.13, ZWEI 125.10, ZMail 73.0, Local-File 75.2, File-Server 24.1, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, falambka.



; From modified file DJ: L.ZWEI; MODES.LISP#164 at 4-Aug-88 18:11:59
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; MODES  "

(DEFUN AUTO-FILL-HOOK (CHAR &AUX BP)
  (SETQ CHAR (CHAR-INT CHAR))
  (AND (MEMQ CHAR *AUTO-FILL-ACTIVATION-CHARACTERS*)
       (NOT *NUMERIC-ARG-P*)
       (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
       (LET ((LINE (BP-LINE (POINT)))
	     (FILL-COLUMN *FILL-COLUMN*))
	 (AND (= CHAR #/CR) (SETQ LINE (LINE-PREVIOUS LINE)))
;	 (AND (PLUSP (STRING-LENGTH *FILL-PREFIX*))
;	      (SETQ FILL-COLUMN (- FILL-COLUMN (STRING-WIDTH *FILL-PREFIX*))))
	 (SETQ BP (DO ((SHEET (WINDOW-SHEET *WINDOW*))		     
		       (LEN (1+ (OR (STRING-REVERSE-SEARCH-NOT-CHAR #/SP LINE) -1)))
		       (POS 0)
		       (CHAR-POS 0 CP)
		       (CP))
		      ((= CHAR-POS LEN) NIL)
		    (SETQ CP (OR (STRING-SEARCH-CHAR #/SP LINE (1+ CHAR-POS)) LEN)
			  POS (TV:SHEET-STRING-LENGTH SHEET LINE CHAR-POS CP NIL NIL POS))
		    (AND (> POS FILL-COLUMN) (> CHAR-POS 0)
			 (RETURN (CREATE-BP LINE CHAR-POS))))))
       (WITH-BP (PT (POINT) :MOVES)		;Save point
	 (MOVE-BP (POINT) BP)
	 (LET ((LINE (BP-LINE BP))
	       (LINE2 NIL))
	   (COND ((OR (END-LINE-P (FORWARD-OVER *BLANKS* BP))
		      (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		      (> (+ (STRING-WIDTH LINE (BP-INDEX BP))
			    (STRING-WIDTH
			      (SETQ LINE2 (LINE-NEXT LINE))))
			 *FILL-COLUMN*)
		      ;; If in Lispish mode and not inside a comment.
		      (AND (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
			   ;(EQ (GET *MAJOR-MODE* 'EDITING-TYPE) :LISP)
			   (> (OR (FIND-COMMENT-START LINE) (1+ (line-length line)))
			      (BP-INDEX BP))))
		  (DELETE-INTERVAL BP (FORWARD-CHAR BP))
		  (LET ((AT-POINT-P (BP-= PT BP)))
		    (MUST-REDISPLAY *WINDOW* (COM-INDENT-NEW-COMMENT-LINE))
		    (AND AT-POINT-P (MOVE-BP PT (POINT)))))
		 (T
		  (MULTIPLE-VALUE-BIND (COMMENT-START COMMENT-END)
		      (FIND-COMMENT-START LINE)
		    ;; Move some text from the end of the line onto the next line.
		    ;; If the next line starts a paragraph,
		    ;; or is a solitary blank line,
		    ;; make a new line to move the text onto.
		    (COND ((AND COMMENT-START
				(< COMMENT-START (BP-INDEX BP)))
			   ;; If we are breaking inside a comment, make a new line
			   ;; unless the next one is also a comment line
			   ;; and the comment starts at the same hpos
			   ;; and with the same comment-starting string.
			   (MULTIPLE-VALUE-BIND (LINE2-COMMENT-START LINE2-COMMENT-END)
			       (AND LINE2 (FIND-COMMENT-START LINE2))
			     (WHEN (OR (NOT LINE2-COMMENT-START)
				       ( (BP-VIRTUAL-INDENTATION
					    (CREATE-BP LINE2 LINE2-COMMENT-START)
					    (WINDOW-SHEET *WINDOW*))
					  (BP-VIRTUAL-INDENTATION
					    (CREATE-BP LINE COMMENT-START)
					    (WINDOW-SHEET *WINDOW*)))
				       ( (- COMMENT-END COMMENT-START)
					  (- LINE2-COMMENT-END LINE2-COMMENT-START))
				       (NOT (STRING-EQUAL LINE LINE2
							  :START1 COMMENT-START
							  :START2 LINE2-COMMENT-START
							  :END1 COMMENT-END
							  :END2 LINE2-COMMENT-END)))
			       (INSERT (CREATE-BP LINE2 0) #/CR)
			       (SETQ LINE2 (LINE-PREVIOUS LINE2)))))
			  ((IF (LINE-BLANK-P LINE2)
			       (NOT (OR (EQ LINE2 (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
					(LINE-BLANK-OR-DIAGRAM-P (LINE-NEXT LINE2))))
			     (BP-AT-PARAGRAPH-TERMINATOR (CREATE-BP LINE2 0)))
			   (unless (char-equal char #/CR) (INSERT (CREATE-BP LINE2 0) #/CR))
			   (SETQ LINE2 (LINE-PREVIOUS LINE2))))
		    ;; Don't include leading blanks in the stuff to move to the next line.
		    (MOVE-BP BP (FORWARD-OVER *BLANKS* BP))
		    ;; IDX is beginning of text to move to next line.
		    (LET ((IDX (BP-INDEX BP))
			  (START-BP (CREATE-BP LINE2 0)))
		      (COND ((AND COMMENT-START
				  (< COMMENT-START (BP-INDEX BP)))
			     ;; If stuff to move down is inside a comment,
			     ;; must move it to inside a comment.
			     (MULTIPLE-VALUE-BIND (ST END)
				 (FIND-COMMENT-START LINE2)
			       (IF END (MOVE-BP START-BP LINE2 END)
				 (INDENT-TO START-BP
					    (BP-INDENTATION (CREATE-BP LINE COMMENT-START)))
				 (MOVE-BP START-BP LINE2 (STRING-LENGTH LINE2))
				 (SETF (VALUES ST END) (FIND-COMMENT-START LINE))
				 (INSERT-MOVING START-BP (NSUBSTRING LINE ST END)))))
			    (T
			     ;; Make sure the following line starts with the fill prefix
			     ;; (in case we just created it and it is empty.)
			     (OR (LOOKING-AT START-BP *FILL-PREFIX*)
				 (INSERT (CREATE-BP LINE2 0) *FILL-PREFIX*))
			     (MOVE-BP START-BP (FORWARD-OVER *BLANKS* START-BP))))
		      ;; Insert the stuff from the previous line.
		      (LET* ((AFTER-BP
			       (INSERT START-BP (NSUBSTRING LINE IDX)))
			     (BACK-ONE-BP (FORWARD-CHAR AFTER-BP -1)))
			;; Make sure there is a space between the new stuff and
			;; what was already on the following line.
			(OR (BP-LOOKING-AT-LIST AFTER-BP *BLANKS*)
			    (BP-LOOKING-AT-LIST BACK-ONE-BP *BLANKS*)
			    (END-LINE-P AFTER-BP)
			    (INSERT AFTER-BP " "))
			;; If it ends a sentence, want two spaces after it.
			(AND (BP-LOOKING-AT-LIST BACK-ONE-BP
						 *FILL-EXTRA-SPACE-LIST*)
			     (NOT (LOOKING-AT AFTER-BP "  "))
			     (NOT (END-LINE-P AFTER-BP))
			     (INSERT AFTER-BP " ")))
		      ;; If Point was within the text moved, keep it with that text.
		      (AND (EQ (BP-LINE PT) (BP-LINE BP))
			   ( (SETQ IDX (- (BP-INDEX PT) IDX)) 0)
			   (MOVE-BP PT LINE2 (+ (BP-INDEX START-BP) IDX))))
		    ;; Delete what we moved from the previous line, plus any blanks before it.
		    (DELETE-INTERVAL (BACKWARD-OVER *BLANKS* BP) (END-OF-LINE LINE))
		    ;; If the text cut left us on the same line we started on, and we were inserting
		    ;; a space, reinsert the trailing space deleted above.
		    (and (char-equal char #/Space)
			 (neq (bp-line pt) line2)
			 (= (bp-index pt) (line-length line))
			 (insert pt #/Space))
		    (MUST-REDISPLAY *WINDOW* DIS-TEXT)))))
	 (MOVE-BP (POINT) PT))))

))
