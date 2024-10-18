;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.12
;;; Reason:
;;;  To find out if given character was found in a list of delimeters,
;;;  COMPLETE-STRING was getting around "character lossage" by doing 2 MEMQs,
;;;  one on the character, and then on the INT-CHAR of the character.  Using
;;;  (LISP:MEMBER char list :TEST 'CHAR-EQUAL) is simpler and slightly faster.
;;; Written 26-Sep-88 18:18:50 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 1
;;; with Experimental System 126.91, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental ZWEI 126.11, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.



; From modified file DJ: L.ZWEI; COMD.LISP#179 at 26-Sep-88 18:18:51
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; COMD  "

(DEFUN COMPLETE-STRING (STRING ALIST DELIMS &OPTIONAL DONT-NEED-LIST CHAR-POS TRUNC
						      IGNORE-TRAILING-SPACE
			&AUX NCHUNKS CHUNKS CHUNK-DELIMS FILLS CHAMB TEMS RETS
			     RCHUNKS TEM LEN COMPLETED-P CHAR-CHUNK CHAR-OFFSET MAGIC-POS
			     TAIL ONE-BEFORE-TAIL TEMSTRING)
  "Complete a given STRING from an ALIST of strings.
DELIMS is a list of delimiter characters that delimit chunks.
 Each chunk is matched against the chunks of strings in the ALIST.
DONT-NEED-LIST says we don't want all the possibilities, just not NIL.
CHAR-POS is position in STRING to be relocated with new things inserted.
TRUNC says don't complete more than one chunk at end.
IGNORE-TRAILING-SPACE non-NIL says ignore a trailing space character if any.
/
Returns multiple values:
 0) NEWSTRING - the (first) matching completion string.
 1) COMPLETIONS - list of matching completions, a subset of ALIST.
 2) COMPLETED-P - non-NIL if some completion was done;
    value is 'ZWEI:NOSPACE if proper delimiter is already at end of string.
 3) CHAR-POS - the new character position in STRING with new things inserted.
 4) MAGIC-POS - location of first point of ambiguity.
/
For efficiency, if ALIST is an ART-Q-LIST array, it is assumed to be alphabetically
sorted."
  (declare (values newstring completions completed-p char-pos magic-pos))
  (SETQ CHUNKS (MAKE-ARRAY 20. :FILL-POINTER 0)
	CHUNK-DELIMS (MAKE-ARRAY 20. :FILL-POINTER 0))
  (SETQ LEN (STRING-LENGTH STRING))
  (AND IGNORE-TRAILING-SPACE
       (> LEN 0)
       (= (AREF STRING (1- LEN)) #/SP)
       (DECF LEN))
  (DO ((I 0 (1+ I))
       (J 0))
      ((> I LEN))
    (when (if (= I LEN)
	      (SETQ TEM -1)		;Last character delimits a chunk unless it is empty.
	    (lisp:member (setq tem (char string i)) delims :test #'char-equal))
      (AND CHAR-POS (> CHAR-POS J)	;Keep track of relative position
	   (SETQ CHAR-CHUNK (ARRAY-LEADER CHUNKS 0)
		 CHAR-OFFSET (- CHAR-POS J)))
      (VECTOR-PUSH-EXTEND (NSUBSTRING STRING J I) CHUNKS)
      (VECTOR-PUSH-EXTEND TEM CHUNK-DELIMS)
      (SETQ J I)))
  (SETQ NCHUNKS (ARRAY-ACTIVE-LENGTH CHUNKS)
	FILLS (MAKE-ARRAY NCHUNKS)
	TEMS (MAKE-ARRAY NCHUNKS)
	RCHUNKS (MAKE-ARRAY NCHUNKS)
	CHAMB (MAKE-ARRAY NCHUNKS :TYPE 'ART-1B))
  (AND (ARRAYP ALIST)
       (MULTIPLE-VALUE (ALIST TAIL ONE-BEFORE-TAIL)
	 (COMPLETE-STRING-BOUNDS ALIST DELIMS NCHUNKS CHUNKS CHUNK-DELIMS)))
  (AND ONE-BEFORE-TAIL
       (N-CHUNKS-MATCH-P (CAAR ALIST) (CAAR ONE-BEFORE-TAIL) NCHUNKS DELIMS)
       ;; The first and last possibilities are the same, for as many chunks as we need,
       ;; so all in between must also be the same.
       DONT-NEED-LIST
       ;; So if we don't need all the possibilities,
       ;; keep just the first one and the last one.
       (SETQ ALIST (LIST (CAR ALIST) (CAR ONE-BEFORE-TAIL))
	     TAIL NIL))
  (DO ((L ALIST (CDR L))
       (ALL-AMBIG))
      ((EQ L TAIL))
    (COND ((ATOM L))				;Indirect through multiple alists
	  ((NULL (COMPLETE-CHUNK-COMPARE (CAAR L) DELIMS NCHUNKS CHUNKS CHUNK-DELIMS TEMS
					 (AND (NULL RETS) RCHUNKS)))
	   (OR RETS (SETQ CHUNKS RCHUNKS))	;First winner determines case of result
	   (PUSH (CAR L) RETS)		;add to list of partial matches
	   (SETQ ALL-AMBIG DONT-NEED-LIST)
	   (DO ((I 0 (1+ I))
		(FILL))
	       (( I NCHUNKS))
	     (SETQ TEM (AREF TEMS I)
		   FILL (AREF FILLS I))
	     (COND ((NULL FILL)			;First one to complete a chunk
		    (SETF (AREF FILLS I) TEM)	;save for later ones
		    (AND (PLUSP (STRING-LENGTH TEM))
			 (SETQ ALL-AMBIG NIL)))	;This chunk not ambiguous yet
		   ((AND (EQUAL FILL "")
			 (ZEROP (AREF CHAMB I))
			 (NOT (EQUAL TEM "")))
		    ;; If there was an exact match found for this chunk,
		    ;; ignore everything that is NOT an exact match in this chunk.
		    (SETQ ALL-AMBIG NIL)
		    (RETURN NIL))
		   ((AND (EQUAL TEM "")
			 (NOT (AND (EQUAL FILL "")
				   (ZEROP (AREF CHAMB I)))))
		    ;; The first time we find an exact match for this chunk,
		    ;; from now on consider only exact matches for it,
		    ;; and forget anything we concluded about later chunks
		    ;; from completions that were inexact in this chunk.
		    (SETF (AREF FILLS I) "")
		    (SETF (AREF CHAMB I) 0)
		    (DO ((I (1+ I) (1+ I))) ((= I NCHUNKS))
		      (SETF (AREF FILLS I) NIL)
		      (SETF (AREF CHAMB I) 0))
		    (SETQ ALL-AMBIG NIL))
		   (T
		    (SETQ LEN (STRING-LENGTH FILL))
		    (DO ((J 0 (1+ J))
			 (LEN1 (STRING-LENGTH TEM)))
			(( J LEN)
			 (OR (ZEROP LEN)
			     (AND (= I (1- NCHUNKS))
				  (= LEN 1)
				  (MEM '= (AREF FILL 0) DELIMS))
			     (SETQ ALL-AMBIG NIL)))
		      (WHEN (OR ( J LEN1)
				(NOT (CHAR-EQUAL (AREF FILL J) (AREF TEM J))))
			;; Not the same completion, shorten final version
			(ASET (NSUBSTRING FILL 0 J) FILLS I)
			(SETF (AREF CHAMB I) 1)	;Remember this was ambiguous
			(OR (ZEROP J) (SETQ ALL-AMBIG NIL))
			(RETURN NIL))))))
	   ;;If not going to complete and don't need actual list, finish up now.
	   (AND ALL-AMBIG (NULL (AREF FILLS (1- NCHUNKS))) (RETURN NIL)))))
  (WHEN (AND TRUNC (SETQ TEMSTRING (AREF FILLS (1- NCHUNKS))))
    (SETQ LEN (STRING-LENGTH TEMSTRING))
    (AND (ZEROP (AREF CHAMB (1- NCHUNKS)))	;If last chunk wasn't ambigous,
	 (SETQ TRUNC 'NOSPACE))			;shouldn't have delimiter there
    (DO ((I 0 (1+ I)))
	(( I LEN))
      (WHEN (MEM '= (AREF TEMSTRING I) DELIMS)
	(ASET (NSUBSTRING TEMSTRING 0 (1+ I)) FILLS (1- NCHUNKS))
	(SETQ TRUNC 'NOSPACE)			;Already gave a delimiter
	(RETURN NIL))))
  (SETQ TEMSTRING "")
  (DO ((I 0 (1+ I)))
      (( I NCHUNKS))
    (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)	;In case inside chunk not completed,
	 (SETQ CHAR-POS (+ (STRING-LENGTH TEMSTRING) CHAR-OFFSET)))	;relocate
    (SETQ TEMSTRING (STRING-APPEND TEMSTRING (AREF CHUNKS I)))
    (WHEN (AND (SETQ TEM (AREF FILLS I)) (> (STRING-LENGTH TEM) 0))
      (SETQ TEMSTRING (STRING-APPEND TEMSTRING TEM)
	    COMPLETED-P T)
      (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)	;If inside completed chunk,
	   (SETQ CHAR-POS (STRING-LENGTH TEMSTRING))))	;move to end of it
    (OR MAGIC-POS (ZEROP (AREF CHAMB I))		;Remember end of leftmost ambigous chunk
	(SETQ MAGIC-POS (STRING-LENGTH TEMSTRING))))
  (AND COMPLETED-P (EQ TRUNC 'NOSPACE)
       (SETQ COMPLETED-P 'NOSPACE))
  (WHEN (OR (AND (ARRAY-HAS-LEADER-P TEMSTRING)
		 (MINUSP (FILL-POINTER TEMSTRING)))
	    (AND CHAR-POS (MINUSP CHAR-POS))
	    (AND MAGIC-POS (MINUSP MAGIC-POS)))
    (FERROR "Internal error in completion.  Report a bug."))
  (VALUES TEMSTRING (NREVERSE RETS) COMPLETED-P CHAR-POS MAGIC-POS))

))
