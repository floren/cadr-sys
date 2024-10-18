;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for ZWEI version 126.28
;;; Reason:
;;;  Zwei's Undo and Redo facilities now take two kinds of numeric arguments:
;;;  Positive Arg (n): Undo/Redo last n changes/Undo's;
;;;  Negative Arg: Undo/Redo till no further Undoing/Redoing is possible.
;;; Written 2-Nov-88 14:34:29 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.138, Experimental ZWEI 126.27, Experimental ZMail 74.13, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, ) (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: L.ZWEI; NPRIM.LISP#40 at 2-Nov-88 14:34:45
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "

(DEFCOM COM-QUICK-UNDO "Undo the last undoable command, no query.
Commands to be undone are remembered for each buffer individually.
If there is a region, undo the last batch of changes that occurred
within the current region.  The region remains so that you can
repeat the command on the same region." (KM)
  (do ((i 0 (incf i))
       (doforever? (and *numeric-arg-p* (minusp *numeric-arg*))))
      ((and (not doforever?)
	    (= i (if *numeric-arg-p* *numeric-arg* 1))))
    (UNDO-UNDO-ITEM (FIND-UNDO-ITEM) (NOT (WINDOW-MARK-P *WINDOW*))))
  DIS-TEXT)

))

; From modified file DJ: L.ZWEI; NPRIM.LISP#40 at 2-Nov-88 14:34:47
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "

(DEFUN FIND-UNDO-ITEM ()
  (UNDO-SAVE-CURRENT-RANGE)
  (LET ((UNDO-ITEM
	  (IF (WINDOW-MARK-P *WINDOW*)
	      (REGION (BP1 BP2)
		(INTERVAL-LAST-UNDO-ITEM BP1 BP2 T))
	    (CAR (UNDO-STATUS-UNDO-LIST
		   (NODE-UNDO-STATUS-OR-NIL (NODE-TOP-LEVEL-NODE *INTERVAL*)))))))
    (SELECTQ UNDO-ITEM
      ((NIL) (BARF "No Undoable changes found in buffer."))
      ((T) (BARF "Conflict: last change in region was not completely within it.")))
    UNDO-ITEM))

))

; From modified file DJ: L.ZWEI; NPRIM.LISP#40 at 2-Nov-88 14:34:49
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "


(DEFCOM COM-QUICK-REDO "Redo the last command undone, no query.
Undone commands are remembered for each buffer individually." ()
  (UNDO-SAVE-CURRENT-RANGE)
  (do ((i 0 (incf i))
       (doforever? (and *numeric-arg-p* (minusp *numeric-arg*)))
       numarg)
      ((cond ((not *numeric-arg-p*)
	      ;; returns t on second time through, normal case
	      (= i 1))
	     ((not doforever?) nil)
	     ((= i *numeric-arg*)
	      (setq numarg t))
	     (t nil))
       (cond ((or numarg (not *numeric-arg-p*)) nil)	;exit the loop, having done all work
	     (t (barf "All recent Undo's redone.  Use the Redo command without numeric arg to redo further."))))
    (LET ((UNDO-STATUS (NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*))))
      (IF (EQ UNDO-STATUS ':DONT) (BARF "No existing redoable changes.")
	(WHEN (OR (NEQ 'EDITING (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
		  (REDO-QUERY UNDO-STATUS *numeric-arg-p*))
	  (LET* ((UNDO-ITEM (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS))))
	    (OR UNDO-ITEM
		(if (and *numeric-arg-p* (not (minusp *numeric-arg*)))
		    (return nil)
		  (BARF "No more changes to Redo.")))
	    (UNDO-ITEM-REDO UNDO-STATUS UNDO-ITEM))))))
  DIS-TEXT)

))

; From modified file DJ: L.ZWEI; NPRIM.LISP#40 at 2-Nov-88 14:34:52
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "


(DEFUN REDO-QUERY (UNDO-STATUS &optional after-many)
  (unless after-many
    (progn
      (FORMAT *QUERY-IO* "~&The last Redoable Undo occurred previous to other modifications.")
      (BEEP)
      (WHEN (Y-OR-N-P "Redo most recent Undo anyway? ")
	(POP (UNDO-STATUS-REDO-LIST UNDO-STATUS))
	T))))

))

; From modified file DJ: L.ZWEI; NPRIM.LISP#40 at 2-Nov-88 14:34:57
#10R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NPRIM  "


(DEFCOM COM-REDO "Redo the last command undone in the current buffer." ()
  (UNDO-SAVE-CURRENT-RANGE)
  (LET* ((UNDO-STATUS (NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*))))
    (IF (EQ UNDO-STATUS ':DONT) (BARF "No Redoable changes exist.")
      (WHEN (OR (NEQ 'EDITING (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
		(REDO-QUERY UNDO-STATUS))
	(LET* ((UNDO-ITEM (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
	       (NAME (UNDO-ITEM-TYPE UNDO-ITEM))
	       SUMMARY)
	  (OR UNDO-ITEM (BARF "No Redoable changes exist."))
	  (FRESH-LINE *QUERY-IO*)
	  (SETQ SUMMARY (SUMMARIZE-UNDO-ITEM UNDO-ITEM))
	  (IF (Y-OR-N-P "Redo ~A (~A)? " (STRING-DOWNCASE NAME) SUMMARY)
	      (UNDO-ITEM-REDO UNDO-STATUS UNDO-ITEM))))))
  DIS-TEXT)

))
