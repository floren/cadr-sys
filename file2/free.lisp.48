
;;; -*-Mode: Lisp; Package: FILE-SYSTEM; Base: 8; -*-

;;; Management of free blocks and the available block table.

;;; The disk contains a free map which contains a bit for each block of the file partition.
;;; This map contains 1 for a block which is free and in the free map.

;;; However, not all blocks not in use are in the free map.
;;; Some are recorded in the available block table for the unit.
;;; Blocks are always allocated from the available table,
;;; and returned to it when a file is deleted.
;;; When the available table gets too full, some of the blocks
;;; in it are transferred to the free map.  When the available table
;;; gets too empty, blocks are transferred to it from the free map.
;;; The free map is always written back before the blocks appear in the available table,
;;; so it is impossible for a block to appear in both the free table and a file.
;;; When a file is deleted, its directory is always written before the blocks are
;;; placed in the available table, for the same reason.

;; (PACK-MINIMUM-AVAILABLE-BLOCKS pack):
;; If there are fewer than this many blocks in the available table, get more from free map.

;; (PACK-MAXIMUM-AVAILABLE-BLOCKS pack):
;; If there are more than this many blocks in the available table, put some in the free map.

;; (PACK-DESIRABLE-AVAILABLE-BLOCKS pack):
;; When we move blocks into or out of the free map, aim for this many
;; in the available table.

;; The available table is actually an N*2 array.
;; It is regarded as a vector of pairs, each pair describing a
;; contiguous run of blocks with the number of the first and the length of the run.

(DEFUN PRINT-AVAILABLE-TABLE (PACK)
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  (LET ((TBL (PACK-AVAILABLE-TABLE PACK)))
    (DO ((I 0 (1+ I))
	 (N (ARRAY-DIMENSION TBL 0)))
	((= I N))
      (LET ((FIRST-BLOCK (AREF TBL I 0))
	    (NUMBER-OF-BLOCKS (AREF TBL I 1)))
	(OR (ZEROP NUMBER-OF-BLOCKS)
	    (FORMAT T "~S-~S  " FIRST-BLOCK (+ FIRST-BLOCK NUMBER-OF-BLOCKS -1)))))))

;Return non-nil if the specifed block is in the available table.
;Specifically, return the index of the available table entry that covers
;the specified block.
(DEFUN BLOCK-AVAILABLE-P (PACK BLOCK)
  (LET* ((TBL (PACK-AVAILABLE-TABLE PACK))
	 (LENGTH (ARRAY-DIMENSION TBL 0)))
    (DOTIMES (I LENGTH)
      (LET ((FIRST-BLOCK (AREF TBL I 0))
	    (NUMBER-OF-BLOCKS (AREF TBL I 1)))
	(IF (AND (>= BLOCK FIRST-BLOCK)
		 (< BLOCK (+ FIRST-BLOCK NUMBER-OF-BLOCKS)))
	    (RETURN I))))))

(DEFUN PRINT-FREE-MAP (PACK)
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  (DO ((FIRST-BIT 0 (+ FIRST-BIT N-BITS))
       (MAP-SIZE (PACK-NUMBER-OF-BLOCKS PACK)) 
       (N-BITS))
      (NIL)
    (MULTIPLE-VALUE (FIRST-BIT N-BITS)
      (FIND-BITMAP-BITS (RQB-BUFFER (PACK-FREE-MAP-RQB PACK)) 0 MAP-SIZE
			FIRST-BIT 1 MAP-SIZE))
    (AND (NULL FIRST-BIT) (RETURN))
    (FORMAT T "~S-~S  " FIRST-BIT (+ FIRST-BIT N-BITS -1))))

;;; Returns 2 values: first block, number-of-blocks actually gotten
;;; Works by first making a pass trying to give what was asked for.
;;; If that fails gives the biggest available.
;;; This can even return 0 as the second value if the disk is full.
(DEFUN ALLOCATE-DISK-BLOCKS (PACK N-BLOCKS-WANTED)
  "Returns two values: first block allocated, number of blocks allocated.
   The number given may be less than the number wanted, even 0 if the disk is full."
  (PROG ALLOCATE-DISK-BLOCKS (FIRST-BLOCK-GOTTEN N-BLOCKS-GOTTEN)
    (WITH-PACK-LOCK PACK
	(AND (< (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK) (PACK-MINIMUM-AVAILABLE-BLOCKS PACK))
	     (> (PACK-NUMBER-OF-FREE-BLOCKS PACK) 0)
	     (REFILL-AVAILABLE-BLOCK-TABLE PACK))
	;; If disk space is below the danger level,
	;; refuse to allocate any except for rewriting a directory.
	(AND (< (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK)
		(PACK-DANGER-AVAILABLE-BLOCKS PACK))
	     (OR (NOT (FILE-TOP-LEVEL-NODE NODE-FILE))
		 (NOT (FUNCALL (FILE-TOP-LEVEL-NODE NODE-FILE)
			       ':ALWAYS-ALLOW-DISK-ALLOCATION)))
	     (RETURN-FROM ALLOCATE-DISK-BLOCKS 0 0))
	(LET ((AVTB (PACK-AVAILABLE-TABLE PACK))
	      (BIGGEST-IDX NIL) (BIGGEST-N -1)
	      (IDX-GOTTEN NIL) N)
	  (DOTIMES (I (ARRAY-DIMENSION AVTB 0))
	    (AND (>= (SETQ N (AREF AVTB I 1)) N-BLOCKS-WANTED)
		 (RETURN (SETQ IDX-GOTTEN I N-BLOCKS-GOTTEN N-BLOCKS-WANTED)))
	    (AND (>= N BIGGEST-N)
		 (SETQ BIGGEST-N N BIGGEST-IDX I)))
	  (OR IDX-GOTTEN
	      (SETQ IDX-GOTTEN BIGGEST-IDX
		    N-BLOCKS-GOTTEN BIGGEST-N))
	  (ASET (SETQ N (- (AREF AVTB IDX-GOTTEN 1) N-BLOCKS-GOTTEN))
		AVTB IDX-GOTTEN 1)
	  (ASET (+ (SETQ FIRST-BLOCK-GOTTEN (AREF AVTB IDX-GOTTEN 0)) N-BLOCKS-GOTTEN)
		AVTB IDX-GOTTEN 0)
	  (SETF (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK)
		(- (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK) N-BLOCKS-GOTTEN))))
    (RETURN FIRST-BLOCK-GOTTEN N-BLOCKS-GOTTEN)))

;;; This function must be called AFTER the directory which used to point to
;;; these blocks has been completely written out to the disk, so that it
;;; is guaranteed that no file points to these blocks even if the system crashes.
;;; Blocks which are locked are not put in the available table;
;;; they are just left "in use".  But this need not be checked for
;;; if we know that no locked blocks are in use in files.
(DEFUN FREE-DISK-BLOCKS (PACK FIRST-BLOCK N-BLOCKS &AUX TEM)
  "Return the specified blocks to the available pool."
  (INCF BLOCKS-BEING-FREED (* (PACK-PAGES-PER-BLOCK PACK) N-BLOCKS))
  (WITH-PACK-LOCK PACK
    ;; Are any of these blocks locked?
    (COND ((AND (OR (PACK-LOCKED-BLOCK-FILES PACK)
		    (PACK-GC-BIT-MAP PACK))
		(< (OR (SETQ TEM (FIND-BITMAP-BIT (PACK-LOCKED-BLOCK-MAP-ARRAY PACK) 0
						  (+ FIRST-BLOCK N-BLOCKS)
						  FIRST-BLOCK 1))
		       (+ FIRST-BLOCK N-BLOCKS))
		   (+ FIRST-BLOCK N-BLOCKS)))
	   ;; If so, free all those BEFORE the first locked one.
	   (OR (= TEM FIRST-BLOCK)
	       (FREE-DISK-BLOCKS PACK FIRST-BLOCK (- TEM FIRST-BLOCK)))
	   ;; Then find the next one that is NOT locked,
	   (SETQ TEM (FIND-BITMAP-BIT (PACK-LOCKED-BLOCK-MAP-ARRAY PACK) 0
				      (+ FIRST-BLOCK N-BLOCKS)
				      TEM 0))
	   ;; and free any more of the specified blocks which follow it.
	   (AND (< TEM (+ FIRST-BLOCK N-BLOCKS))
		(FREE-DISK-BLOCKS PACK TEM (- (+ FIRST-BLOCK N-BLOCKS) TEM))))
	  ;; None of the blocks are locked.
	  (T
	   (ADD-TO-AVAILABLE-TABLE FIRST-BLOCK N-BLOCKS (PACK-AVAILABLE-TABLE PACK) PACK)))))

;;; This function is called to transfer free blocks from the pack's free map to its
;;; available table.  The pack must already be locked.  We guarantee safety by not
;;; altering the available table until the free map has safely been written out.
(DEFUN REFILL-AVAILABLE-BLOCK-TABLE (PACK)
  (LET* ((AVTB (PACK-AVAILABLE-TABLE PACK))
	 ;; Number of free blocks we want to find.
	 (BLOCKS-TO-GET (MIN (PACK-NUMBER-OF-FREE-BLOCKS PACK)
			     (- (PACK-DESIRABLE-AVAILABLE-BLOCKS PACK)
				(PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK))))
	 ;; We construct new available table in ths variable,
	 ;; then when done we copy it back to install it.
	 (NAVTB (MAKE-ARRAY (ARRAY-DIMENSIONS AVTB)))
	 ;; block number of last block (+ 1).
	 (MAX-BLOCK (PACK-NUMBER-OF-BLOCKS PACK)))
    ;; Copy available table into temporary storage, update it there.
    (COPY-ARRAY-CONTENTS AVTB NAVTB)
    (DO ((STARTING-BIT 0 (+ FIRST-BIT N-BITS))
	 (FIRST-BIT) (N-BITS))
	((<= BLOCKS-TO-GET 0)
	 (SETF (PACK-FREE-MAP-SCANNING-INDEX PACK) STARTING-BIT))
      ;; Find next run of free blocks in the free map.
      (MULTIPLE-VALUE (FIRST-BIT N-BITS)
	(FIND-BITMAP-BITS (RQB-BUFFER (PACK-FREE-MAP-RQB PACK)) 0 MAX-BLOCK
			  STARTING-BIT 1 BLOCKS-TO-GET))
      ;; If we reach the end of the free map, start at the beginning.
      ;; Return if there are no free blocks in in the map.
      (IF (NULL FIRST-BIT)
	  (IF (ZEROP STARTING-BIT)
	      (RETURN (SETF (PACK-FREE-MAP-SCANNING-INDEX PACK) 0))
	    (SETQ FIRST-BIT 0 N-BITS 0))
	;;FIRST-BIT and N-BITS are ready to be entered into the available table
	;; in the next free slot, and turned off in the free map.
	(CHANGE-BITMAP-BITS  (RQB-BUFFER (PACK-FREE-MAP-RQB PACK)) 0 MAX-BLOCK
			    FIRST-BIT N-BITS 0)
	(ADD-TO-AVAILABLE-TABLE FIRST-BIT N-BITS NAVTB PACK)
	(DECF (PACK-NUMBER-OF-FREE-BLOCKS PACK) N-BITS)
	(DECF BLOCKS-TO-GET N-BITS)))
    ;; Store back the free map.
    (REWRITE-FREE-MAP PACK)
    ;; Now the blocks can appear in the available table.
    (COPY-ARRAY-CONTENTS NAVTB AVTB)
    (RETURN-ARRAY NAVTB)))

;Add a specified range of blocks to the available table.
;Can be used for deallocating blocks, or for moving them from the free map.
;Tries to find an existing slot in the available table which
;records other available blocks consecutive with these.
(DEFUN ADD-TO-AVAILABLE-TABLE (FIRST-BLOCK N-BLOCKS AVTB PACK)
  (LET ((ZERO-IDX NIL))
    ;; If a GC is going on on this pack,
    ;; all blocks which we are deleting now should be marked
    ;; so they won't be freed while they are in the available table.
    (AND (PACK-GC-BIT-MAP PACK)
	 (NEQ (PACK-GC-BIT-MAP PACK) T)
	 (CHANGE-BITMAP-BITS (PACK-GC-BIT-MAP PACK) 0
			     (PACK-NUMBER-OF-BLOCKS PACK)
			     FIRST-BLOCK N-BLOCKS 1 T))
    (DO-NAMED STORE-INTO-AVAILABLE-TABLE () (NIL)	;Do until successfully stored
      (DOTIMES (I (ARRAY-DIMENSION AVTB 0))
	(LET ((F (AREF AVTB I 0))
	      (N (AREF AVTB I 1)))
	  (COND ((= (+ F N) FIRST-BLOCK)
		 (ASET (+ N N-BLOCKS) AVTB I 1)
		 (RETURN-FROM STORE-INTO-AVAILABLE-TABLE))
		((= (+ FIRST-BLOCK N-BLOCKS) F)
		 (ASET FIRST-BLOCK AVTB I 0)
		 (ASET (+ N N-BLOCKS) AVTB I 1)
		 (RETURN-FROM STORE-INTO-AVAILABLE-TABLE))
		((ZEROP N)
		 (OR ZERO-IDX (SETQ ZERO-IDX I))))))
      (COND (ZERO-IDX
	     (ASET FIRST-BLOCK AVTB ZERO-IDX 0)
	     (ASET N-BLOCKS AVTB ZERO-IDX 1)
	     (RETURN-FROM STORE-INTO-AVAILABLE-TABLE)))
      (FLUSH-AVAILABLE-BLOCK-TABLE PACK T))	;then loop back and try again
    (INCF (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK) N-BLOCKS)))

;;; Transfer blocks from the available table to the free map.
;;; Must be called with the unit locked.
;;; For now, reads and writes the free map one page at a time.
(DEFUN FLUSH-AVAILABLE-BLOCK-TABLE (PACK &OPTIONAL EVERYTHING-P)
  (LET* ((AVTB (PACK-AVAILABLE-TABLE PACK))
	 (BLOCKS-TO-PUT (IF EVERYTHING-P
			    (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK)
			  (MAX 0
			       (- (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK)
				  (PACK-DESIRABLE-AVAILABLE-BLOCKS PACK))))))
    (DO ((IDX (1- (ARRAY-DIMENSION AVTB 0)) (1- IDX)))
	((MINUSP IDX))
      (IF (ZEROP BLOCKS-TO-PUT) (RETURN NIL))
      (LET ((F (AREF AVTB IDX 0))
	    (N (AREF AVTB IDX 1)))
	(COND ((NOT (ZEROP N))
	       (CHANGE-BITMAP-BITS (RQB-BUFFER (PACK-FREE-MAP-RQB PACK))
				   0 (PACK-NUMBER-OF-BLOCKS PACK)
				   F N 1)
	       (ASET 0 AVTB IDX 1)
	       (DECF (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK) N)
	       (INCF (PACK-NUMBER-OF-FREE-BLOCKS PACK) N)
	       (DECF BLOCKS-TO-PUT N)))))
    (REWRITE-FREE-MAP PACK)))

(DEFUN REWRITE-FREE-MAP (PACK)
  (DISK-WRITE (PACK-FREE-MAP-RQB PACK) (PACK-UNIT-NUMBER PACK)
	      (+ (* (PACK-FREE-MAP-FIRST-BLOCK PACK)
		    (PACK-PAGES-PER-BLOCK PACK))
		 (PACK-FIRST-ABS-PAGE-NUMBER PACK))))

;The block-starts-file map is used to remember which blocks contain
;the start of a file.  With this, it is possible to find all files without
;having to parse the node tree structure.

;Timing errors could occur because the block-starts-file map cannot be
;updated at the same time as the directory in which the file is being
;created or deleted.  To prevent these timing errors from causing harm,
;the file is always in the block-starts-file map before it is put in the
;directory and after it is deleted.  At the time of creation or deletion,
;the file's contents mark it as a "half-dead" file (see DEFS) so that
;anyone who looks at it during that time (or after a crash at that time)
;knows he must follow the back pointer to the containing directory
;to find out whether the file is real.

;T if the specified block on the specified pack ought to start a file.
;There is no need to lock the pack, because our access to the map
;is entirely within one aref.
(DEFUN BLOCK-STARTS-FILE-P (PACK BLOCK-NUM)
  (BIT-TEST (LSH 1 (\ BLOCK-NUM 16.))
	    (AREF (PACK-BLOCK-STARTS-FILE-MAP-ARRAY PACK)
		  (TRUNCATE BLOCK-NUM 16.))))

(DEFUN BLOCK-LOCKED-P (PACK BLOCK-NUM)
  (BIT-TEST (LSH 1 (\ BLOCK-NUM 16.))
	    (AREF (PACK-LOCKED-BLOCK-MAP-ARRAY PACK)
		  (TRUNCATE BLOCK-NUM 16.))))

(DEFUN BLOCK-FREE-P (PACK BLOCK-NUM)
  (BIT-TEST (LSH 1 (\ BLOCK-NUM 16.))
	    (AREF (RQB-BUFFER (PACK-FREE-MAP-RQB PACK))
		  (TRUNCATE BLOCK-NUM 16.))))

;;; Set or clear a bit in the pack's file-start map.
;;; This bit says whether a block is the first block of a file.
(DEFUN SET-BLOCK-STARTS-FILE-BIT (PACK BLOCK-NUM NEW-BIT)
  (SET-BITMAP-BIT PACK BLOCK-NUM NEW-BIT
		  (PACK-BLOCK-STARTS-FILE-MAP-FIRST-BLOCK PACK)
		  (PACK-BLOCK-STARTS-FILE-MAP-ARRAY PACK)))

;;; Set or clear the bit which says that a block is locked
;;; (that it is bad and should not be used).
;;; Do not lock a block unless it is in use in a file!
(DEFUN SET-BLOCK-LOCKED-BIT (PACK BLOCK-NUM NEW-BIT
				  &AUX (BLOCK-IS-IN-FILE T))
  (COND ((AND ( NEW-BIT 0)
	      (NOT (BLOCK-LOCKED-P PACK BLOCK-NUM)))
	 ;; Remove this block from the available table if it is in it.
	 (LET ((AVTB (PACK-AVAILABLE-TABLE PACK)))
	   (DOTIMES (I (ARRAY-DIMENSION AVTB 0))
	     (LET ((F (AREF AVTB I 0))
		   (N (AREF AVTB I 1)))
	       (AND (<= F BLOCK-NUM)
		    (< BLOCK-NUM (+ F N))
		    (PROGN ;; Remove this bunch of blocks from the available table
			   (SETF (AREF AVTB I 0) 0)
			   (SETF (AREF AVTB I 1) 0)
			   ;; then put them back in, all but the block we are locking.
			   (OR (= BLOCK-NUM F)
			       (FREE-DISK-BLOCKS PACK F (- BLOCK-NUM F)))
			   (OR (= (1+ BLOCK-NUM) (+ F N))
			       (FREE-DISK-BLOCKS PACK (1+ BLOCK-NUM) (- (+ F N) (1+ BLOCK-NUM))))
			   ;; Don't count this block among the available ones.
			   (DECF (PACK-NUMBER-OF-AVAILABLE-BLOCKS PACK))
			   ;; This block appears not to be in a file.
			   (SETQ BLOCK-IS-IN-FILE NIL))))))
	 ;; Now mark the block as "in use" in the free map.
	 (COND ((BLOCK-FREE-P PACK BLOCK-NUM)
		(SETQ BLOCK-IS-IN-FILE NIL)
		(SET-BLOCK-FREE-BIT PACK BLOCK-NUM 0)))
	 ))
  ;; If we now have a locked block in a file, and didn't before,
  ;; make sure we start looking for such when we delete files.
  (AND BLOCK-IS-IN-FILE
       (NULL (PACK-LOCKED-BLOCK-FILES PACK))
       (NULL (PACK-GC-BIT-MAP PACK))
       (SETF (PACK-LOCKED-BLOCK-FILES PACK) T))
  (SET-BITMAP-BIT PACK BLOCK-NUM NEW-BIT
		  (PACK-LOCKED-BLOCK-MAP-FIRST-BLOCK PACK)
		  (PACK-LOCKED-BLOCK-MAP-ARRAY PACK)))

(DEFUN SET-BLOCK-FREE-BIT (PACK BLOCK-NUM NEW-BIT)
  (INCF (PACK-NUMBER-OF-FREE-BLOCKS PACK)
	(- NEW-BIT (IF (BLOCK-FREE-P PACK BLOCK-NUM) 1 0)))
  (SET-BITMAP-BIT PACK BLOCK-NUM NEW-BIT
		  (PACK-FREE-MAP-FIRST-BLOCK PACK)
		  (RQB-BUFFER (PACK-FREE-MAP-RQB PACK))))

;Change one bit in a map maintained as an array in core.
;The map is changed on disk as well as in the array in core.
;BLOCK-NUM specifies which bit in the map is to be set.
(DEFUN SET-BITMAP-BIT (PACK BLOCK-NUM NEW-BIT MAP-FIRST-BLOCK MAP-ARRAY
		       &AUX (UNIT (PACK-UNIT-NUMBER PACK)) (RQB NIL))
 (WITH-PACK-LOCK PACK
  (LET* ((TEM (AREF MAP-ARRAY (TRUNCATE BLOCK-NUM 16.)))
	 (PPSS (1+ (LSH (\ BLOCK-NUM 16.) 6)))
	 (OLD-BIT (LDB PPSS TEM)))
    (COND ((NOT (= NEW-BIT OLD-BIT))
	   (ASET (DPB NEW-BIT PPSS TEM)
		 MAP-ARRAY (TRUNCATE BLOCK-NUM 16.))
	   (UNWIND-PROTECT
	     (LET ((BITS-PER-PAGE (* 40 PAGE-SIZE))
		   (ARRAY-ELTS-PER-PAGE (* 2 PAGE-SIZE))
		   PAGE-NUMBER MAP-PAGE-NUMBER)
	       (SETQ RQB (GET-DISK-RQB))
	       (SETQ MAP-PAGE-NUMBER (TRUNCATE BLOCK-NUM BITS-PER-PAGE))
	       (SETQ PAGE-NUMBER (+ (PACK-FIRST-ABS-PAGE-NUMBER PACK)
				    (* MAP-FIRST-BLOCK
				       (PACK-PAGES-PER-BLOCK PACK))
				    MAP-PAGE-NUMBER))
	       (COPY-ARRAY-PORTION MAP-ARRAY (* ARRAY-ELTS-PER-PAGE MAP-PAGE-NUMBER)
				   (* ARRAY-ELTS-PER-PAGE (1+ MAP-PAGE-NUMBER))
				   (RQB-BUFFER RQB) 0 ARRAY-ELTS-PER-PAGE)
	       (DISK-WRITE RQB UNIT PAGE-NUMBER))
	     (RETURN-DISK-RQB RQB)))))))

;Make an array and read the contents of a bit map into it.
;The value returned is the array.
;The file-starts-block map is maintained this way.
;No need to lock the pack, because this is done
;before the pack is accessible for use.
(DEFUN READ-BITMAP-INTO-ARRAY (PACK MAP-FIRST-BLOCK &OPTIONAL ARRAY
			       &AUX RQB (UNIT (PACK-UNIT-NUMBER PACK))
			       (ARRAY-ELTS-PER-PAGE (* PAGE-SIZE 2)))
  (OR ARRAY (SETQ ARRAY (MAKE-ARRAY (* (PACK-FREE-MAP-NUMBER-OF-BLOCKS PACK)
				       (PACK-PAGES-PER-BLOCK PACK)
				       ARRAY-ELTS-PER-PAGE)
				    ':TYPE 'ART-16B)))
  (UNWIND-PROTECT
    (PROGN
      (SETQ RQB (GET-DISK-RQB (* (PACK-FREE-MAP-NUMBER-OF-BLOCKS PACK)
				 (PACK-PAGES-PER-BLOCK PACK))))
      (DISK-READ RQB UNIT (+ (PACK-FIRST-ABS-PAGE-NUMBER PACK)
			     (* MAP-FIRST-BLOCK
				(PACK-PAGES-PER-BLOCK PACK))))
      (COPY-ARRAY-CONTENTS (RQB-BUFFER RQB) ARRAY))
    (RETURN-DISK-RQB RQB))
  ARRAY)

;; Clear one of the bitmaps, for initializing a pack.
;; Specify which by supplying the first block number.
;; All bit maps have the same length, on a given pack!
;; There is no PACK object at this time, so all parameters are passed individually.
(DEFUN CLEAR-BITMAP (UNIT MAP-FIRST-BLOCK MAP-N-BLOCKS
		     PACK-FIRST-ABS-PAGE PACK-PAGES-PER-BLOCK
		     &AUX RQB)
  (UNWIND-PROTECT
    (PROGN
      (SETQ RQB (GET-DISK-RQB (* MAP-N-BLOCKS PACK-PAGES-PER-BLOCK)))
      (FILLARRAY (RQB-BUFFER RQB) '(0))
      (DISK-WRITE RQB UNIT (+ (* MAP-FIRST-BLOCK PACK-PAGES-PER-BLOCK)
			      PACK-FIRST-ABS-PAGE)))
    (RETURN-DISK-RQB RQB)))

;;; Primitives for manipulating bit maps.  These are separate functions
;;; only to cut down the hairiness of the code.
;;; The array (MAP) is assumed to be ART-16B.

;;; Change the state of a contiguous range of bits.
;;; Returns the number of bits actually changed, which can be smaller
;;; if some bits lie outside this section of map.  Note that all bits
;;; must change, if they are already in the new state it must be an error,
;;; unless NO-ERROR is set.
(DEFUN CHANGE-BITMAP-BITS (MAP FIRST-BIT-IN-MAP N-BITS-IN-MAP
			   FIRST-BIT-CHANGED N-BITS-CHANGED NEW-VALUE
			   &OPTIONAL NO-ERROR
			   &AUX (BITS-PER-WORD 16.) (WORD-MASK 177777)
			        FUNC1 FUNC2 W)
  ;; Set up BOOLE functions for below depending on whether making 0's or 1's
  (COND ((ZEROP NEW-VALUE)
	 (SETQ FUNC1 2 FUNC2 4))
	((SETQ FUNC1 1 FUNC2 7)))
  ;; Clip the region and make relative to the piece of map given
  (LET ((FIRST-BIT (MAX (- FIRST-BIT-CHANGED FIRST-BIT-IN-MAP) 0))
	(LAST-BIT (MIN (- (+ FIRST-BIT-CHANGED N-BITS-CHANGED) FIRST-BIT-IN-MAP)
		       N-BITS-IN-MAP)))
    (COND ((<= LAST-BIT FIRST-BIT) 0)
	  (T (DO ((I (TRUNCATE FIRST-BIT BITS-PER-WORD) (1+ I))	;I is array index
		  (M (LOGAND (LSH WORD-MASK (\ FIRST-BIT BITS-PER-WORD)) WORD-MASK)
		     WORD-MASK)			;M is mask selecting bits to do in this word
		  (N (+ (- LAST-BIT FIRST-BIT) (\ FIRST-BIT BITS-PER-WORD))
		     (- N BITS-PER-WORD)))	;N is number of bits to go
		 ((<= N 0))
	       (AND (< N BITS-PER-WORD)		;The last word contains fewer bits
		    (SETQ M (LOGAND (LSH WORD-MASK (- N BITS-PER-WORD)) M)))
	       (OR (ZEROP (BOOLE FUNC1 (SETQ W (AREF MAP I)) M))
		   NO-ERROR
		   (FERROR NIL "Bitmap bits already ~D" NEW-VALUE))
	       (ASET (BOOLE FUNC2 W M) MAP I))
	     (- LAST-BIT FIRST-BIT)))))

;;; Find a contiguous set of bits in a certain state.
;;; Returns 2 values: the bit number and the number of bits of the next set.
;;; Returns NIL if it doesn't find any more bits.
;;; N-BITS-IN-MAP assumed to be a multiple of BITS-PER-WORD
;;; MAX-BITS-TO-GET is only approximate
(DEFUN FIND-BITMAP-BITS (MAP FIRST-BIT-IN-MAP N-BITS-IN-MAP
			 STARTING-BIT DESIRED-VALUE MAX-BITS-TO-GET)
  (PROG (FIRST-BIT LAST-BIT)
	;; Find the next bit in the desired state
	(SETQ FIRST-BIT (FIND-BITMAP-BIT MAP FIRST-BIT-IN-MAP N-BITS-IN-MAP
					 STARTING-BIT DESIRED-VALUE))
	(OR FIRST-BIT (RETURN NIL))
	;; Consider only the next MAX-BITS-TO-GET past this point.
	(SETQ N-BITS-IN-MAP (MIN N-BITS-IN-MAP
				 (- (+ MAX-BITS-TO-GET FIRST-BIT)
				    FIRST-BIT-IN-MAP)))
	;; Find the next bit after that which is not in the desired state.
	(SETQ LAST-BIT (FIND-BITMAP-BIT MAP FIRST-BIT-IN-MAP N-BITS-IN-MAP
					FIRST-BIT (- 1 DESIRED-VALUE)))
	(OR LAST-BIT (SETQ LAST-BIT (+ FIRST-BIT-IN-MAP N-BITS-IN-MAP)))
	(RETURN FIRST-BIT (- LAST-BIT FIRST-BIT))))

;; Find the next bit in MAP, past bit number STARTING-BIT, which is set to DESIRED-VALUE.
;; Return NIL if there is none.
(DEFUN FIND-BITMAP-BIT (MAP FIRST-BIT-IN-MAP N-BITS-IN-MAP
			    STARTING-BIT DESIRED-VALUE
			 &AUX (BITS-PER-WORD 16.) (WORD-MASK 177777)
			      FUNC1 FUNC2 W LZ)
  (COND ((ZEROP DESIRED-VALUE)
	 (SETQ FUNC1 2 FUNC2 1))
	((SETQ FUNC1 1 FUNC2 2)))
  (SETQ STARTING-BIT (- STARTING-BIT FIRST-BIT-IN-MAP)
	LZ (\ STARTING-BIT BITS-PER-WORD))
  (DO ((I (TRUNCATE STARTING-BIT BITS-PER-WORD) (1+ I))
       (M (LOGAND (LSH WORD-MASK LZ) WORD-MASK) WORD-MASK)
       (N (+ (- N-BITS-IN-MAP STARTING-BIT) LZ)
	  (- N BITS-PER-WORD)))
      ((<= N 0) NIL)
    (COND ((NOT (ZEROP (BOOLE FUNC1 (SETQ W (AREF MAP I)) M)))	;There are bits here
	   (RETURN
	     (DO ((FIRST-BIT (+ (* I BITS-PER-WORD) FIRST-BIT-IN-MAP LZ) (1+ FIRST-BIT))
		  (M1 (LSH 1 LZ) (LSH M1 1)))
		 ((NOT (ZEROP (BOOLE FUNC1 W M1)))
		  FIRST-BIT)))))
    (SETQ LZ 0)))

;; Return the number of free blocks in a pack's free map.
(DEFUN COUNT-FREE-BLOCKS (PACK &AUX (N-FREE 0))
  (DO ((B (RQB-BUFFER (PACK-FREE-MAP-RQB PACK)))
       (I 0 (1+ I))
       (W))
      ((= I (TRUNCATE (PACK-NUMBER-OF-BLOCKS PACK) 16.)))
    (OR (ZEROP (SETQ W (AREF B I)))
	(COND ((= W 177777) (SETQ N-FREE (+ 16. N-FREE)))
	      ((DO M 1 (LSH M 1) (= M 200000)
		   (AND (BIT-TEST M W) (SETQ N-FREE (1+ N-FREE))))))))
  N-FREE)