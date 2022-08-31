;;; -*- Mode:Lisp;Package:FS;Base:8-*-
;;; ``PDP10'' also means ITS tape format for some of this code.
;;; Ancient functions from RG;MT

;; Mode can be :LIST, :PRINT, or :LOAD
(DEFUN LOAD-PDP10-DUMP-MAGTAPE (&OPTIONAL (MODE ':LOAD) HOST &REST OPTIONS &AUX IS)
  (UNWIND-PROTECT 
    (PROG (PLIST TEM (FIRST-TIME T) ITS-DIR ITS-N1 ITS-N2)
       L  (SETQ IS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':INPUT ':DENSITY 2
				   ':RECORD-SIZE (* 1024. 5) OPTIONS))
	  (COND (FIRST-TIME
		 (STREAM-READ-PDP10-TAPE-HEADER IS)
		 (SETQ FIRST-TIME NIL)))
	  (IF (NULL (MULTIPLE-VALUE (ITS-DIR ITS-N1 ITS-N2)
		      (STREAM-READ-PDP10-DUMP-HEADER IS)))
	      (RETURN T))
	  (SELECTQ MODE
	    (:LIST
	     (FORMAT T "~%~S; ~S ~S" ITS-DIR ITS-N1 ITS-N2)
	     (MT-SPACE-TO-EOF)
	     (GO E0))
	    (:PRINT
	     (FORMAT T "~%~S; ~S ~S" ITS-DIR ITS-N1 ITS-N2)
	     (COND ((EQUAL ITS-N2 "QFASL")
		    (MT-SPACE-TO-EOF)
		    (GO E0))
		   (T (STREAM-COPY-PDP10-ASCII-FILE-FROM-TAPE IS TERMINAL-IO)
		      (GO E0))))
	    (:LOAD
	     (SETQ PLIST (ITS-FILENAMES-TO-LM-PLIST ITS-DIR ITS-N1 ITS-N2))
	     (FORMAT T "~%FILE ~S on tape, type disk file name or CR: " PLIST))
	    (OTHERWISE
	     (FERROR T "~%~S is an unknown mode" MODE)))
	  (IF (> (ARRAY-ACTIVE-LENGTH (SETQ TEM (READLINE))) 0)
	      (SETQ PLIST (PARSE-PATHNAME TEM)))
	  (LET* ((OUTPATH (PATHNAME-FROM-PLIST HOST NIL PLIST)))
	    (WITH-OPEN-FILE (OS OUTPATH ':DIRECTION ':OUTPUT
				':FLAVOR ':PDP10
				':PDP10-FORMAT T
				':CHARACTERS (NOT (EQUAL (SEND OUTPATH ':TYPE) "QFASL")))
	      (COND ((SEND OS ':SEND-IF-HANDLES ':PDP10-FORMAT)
		     (STREAM-COPY-PDP10-FMT-FILE-FROM-TAPE IS OS))
		    ((SEND OS ':CHARACTERS)
		     (STREAM-COPY-PDP10-ASCII-FILE-FROM-TAPE IS OS))
		    (T (STREAM-COPY-PDP10-QFASL-FILE-TO-PDP10-TAPE IS OS)))))
      E0  (CLOSE IS)
	  (GO L))
    (CLOSE IS)))

(DEFUN MT-WRITE-PDP10-FILES (FILES &REST OPTIONS &AUX OS)
  (UNWIND-PROTECT
    (PROG (PLIST (FIRST-TIME T))
	  (SETQ FILES (MAPCAR #'MERGE-PATHNAME-DEFAULTS FILES))
      L	  (COND ((NULL FILES)
		 (MT-WRITE-EOF)
		 (RETURN T)))
	  (SETQ OS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':OUTPUT ':DENSITY 2
				  ':RECORD-SIZE (* 1024. 5) OPTIONS))
	  (COND (FIRST-TIME
		 (STREAM-WRITE-PDP10-TAPE-HEADER OS)
		 (SETQ FIRST-TIME NIL)))
	  (SETQ PLIST (PLIST-VIA-PATHNAME (CAR FILES)))
	  (MULTIPLE-VALUE-BIND (ITS-DIR ITS-N1 ITS-N2)
	      (LM-PLIST-TO-ITS-FILENAMES PLIST)
	    (SETQ ITS-DIR "USERS1")		;Hack for DMS.
	    (STREAM-WRITE-PDP10-DUMP-HEADER OS ITS-DIR ITS-N1 ITS-N2))
	  (WITH-OPEN-FILE (IS (CAR FILES) ':CHARACTERS ':DEFAULT ':PDP10-FORMAT T)
	    (IF (SEND IS ':SEND-IF-HANDLES ':PDP10-FORMAT)
		(STREAM-COPY-PDP10-FMT-FILE-TO-PDP10-TAPE IS OS)
	      (IF (SEND IS ':CHARACTERS)
		  (STREAM-COPY-ASCII-FILE-TO-PDP10-TAPE IS OS)
		(STREAM-COPY-QFASL-FILE-TO-PDP10-TAPE IS OS))))
	  (CLOSE OS)
	  (SETQ FILES (CDR FILES))
	  (GO L))
    (CLOSE OS)))

(DEFUN STREAM-COPY-PDP10-QFASL-FILE-FROM-TAPE (IS OS)
  (PROG (C1 C2 C3 C4 C5)
   L   (SETQ C1 (SEND IS ':TYI)	;JUST GOBBLE HIGH 32 BITS FROM PDP10 WORD.
	     C2 (SEND IS ':TYI)
	     C3 (SEND IS ':TYI)
	     C4 (SEND IS ':TYI)
	     C5 (SEND IS ':TYI))
       (IF (NULL C5) (RETURN NIL))
       (SEND OS ':TYO (DPB C1 1010 C2))
       (SEND OS ':TYO (DPB C3 1010 C4))
       (GO L)))

(DEFUN STREAM-COPY-QFASL-FILE-TO-PDP10-TAPE (IS OS)
  (PROG (C1 C2)
    L	(SETQ C1 (SEND IS ':TYI)
	      C2 (SEND IS ':TYI))
       (IF (NULL C1) (RETURN NIL))
       (IF (NULL C2) (SETQ C2 0))
       (SEND OS ':TYO (LDB 1010 C1))
       (SEND OS ':TYO (LDB 0010 C1))
       (SEND OS ':TYO (LDB 1010 C2))
       (SEND OS ':TYO (LDB 0010 C2))
       (GO L)))

(DEFUN STREAM-COPY-PDP10-FMT-FILE-FROM-TAPE (IS OS &OPTIONAL PDP10WDS)
  (PROG (C1 C2 C3 C4 C5)
    L  (COND ((AND PDP10WDS (ZEROP (SETQ PDP10WDS (1- PDP10WDS))))
	      (RETURN T)))
       (SETQ C1 (SEND IS ':TYI)
	     C2 (SEND IS ':TYI)
	     C3 (SEND IS ':TYI)
	     C4 (SEND IS ':TYI)
	     C5 (SEND IS ':TYI))
       (IF (NULL C5) (RETURN NIL))
       (SEND OS ':TYO (LSH C1 -1))
       (SEND OS ':TYO (DPB C1 0601 (LSH C2 -2)))
       (SEND OS ':TYO (DPB C2 0502 (LSH C3 -3)))
       (SEND OS ':TYO (DPB C3 0403 (LSH C4 -4)))
       (SEND OS ':TYO (DPB C5 0701 (DPB C4 0304 (LSH (LOGAND 77 C5) -1))))
       (GO L)))

(DEFUN STREAM-COPY-PDP10-FMT-FILE-TO-PDP10-TAPE (IS OS)
  (PROG (CH BP WD)
	(SETQ BP 3507 WD 0)
    L	(SETQ CH (SEND IS ':TYI))
        (COND ((NULL CH)
	       (IF (NOT (ZEROP WD)) (STREAM-WRITE-PDP10-WORD OS WD))
	       (RETURN NIL)))
	(SETQ WD (DPB CH BP WD))
	(IF (= BP 0107)
	    (DPB (LSH CH -7) 0001 WD))
	(SETQ BP (- BP 700))
	(COND ((MINUSP BP)
	       (STREAM-WRITE-PDP10-WORD OS WD)
	       (SETQ BP 3507 WD 0)))
	(GO L)))

(DEFUN STREAM-COPY-PDP10-ASCII-FILE-FROM-TAPE (IS OS &OPTIONAL PDP10WDS)
  (PROG (C1 C2 C3 C4 C5 CH)
    L  (COND ((AND PDP10WDS (ZEROP (SETQ PDP10WDS (1- PDP10WDS))))
	      (RETURN T)))
       (SETQ C1 (SEND IS ':TYI)
	     C2 (SEND IS ':TYI)
	     C3 (SEND IS ':TYI)
	     C4 (SEND IS ':TYI)
	     C5 (SEND IS ':TYI))
       (IF (NULL C5) (RETURN NIL))
       (SETQ CH (ASCII-TO-LISPM (LSH C1 -1)))
       (IF CH (SEND OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C1 0601 (LSH C2 -2))))
       (IF CH (SEND OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C2 0502 (LSH C3 -3))))
       (IF CH (SEND OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C3 0403 (LSH C4 -4))))
       (IF CH (SEND OS ':TYO CH))
       (SETQ CH (ASCII-TO-LISPM (DPB C4 0304 (LSH (LOGAND 77 C5) -1))))
       (IF CH (SEND OS ':TYO CH))
       (GO L)))

(DEFUN STREAM-COPY-ASCII-FILE-TO-PDP10-TAPE (IS OS)
  (PROG (CH BP WD)
	(SETQ BP 3507 WD 0)
    L	(SETQ CH (SEND IS ':TYI))
        (COND ((NULL CH)
	       (IF (NOT (ZEROP WD)) (STREAM-WRITE-PDP10-WORD OS WD))
	       (RETURN NIL)))
	(SETQ CH (LISPM-TO-ASCII CH))
	(SETQ WD (DPB CH BP WD)
	      BP (- BP 700))
	(COND ((MINUSP BP)
	       (STREAM-WRITE-PDP10-WORD OS WD)
	       (SETQ BP 3507 WD 0)))
	(COND ((= CH 15)
	       (SETQ WD (DPB 12 BP WD)
		     BP (- BP 700))
	       (COND ((MINUSP BP)
		      (STREAM-WRITE-PDP10-WORD OS WD)
		      (SETQ BP 3507 WD 0)))))
	(GO L)))


(DEFUN STREAM-READ-PDP10-TAPE-HEADER (IS)
  (PROG (HEADER COUNT)
	(SETQ HEADER (STREAM-READ-PDP10-WORD IS))
	(SETQ COUNT (MIN 100 (1- (MINUS (LOGIOR -400000 (LDB 2222 HEADER))))))
	(FORMAT T "~%TAPE HEADER: ~D WORDS" COUNT)
	(DOTIMES (C COUNT)
	  (PRINT (STREAM-READ-PDP10-WORD IS)))
   ;TAPE NUMBER,  TAPE CREATION DATE (SIXBIT), TYPE-DUMP
	))

(DEFUN STREAM-WRITE-PDP10-TAPE-HEADER (OS)
  (STREAM-WRITE-PDP10-WORD OS (PDP10-HALF-WORDS -4 0))
  (DOTIMES (C 3) (STREAM-WRITE-PDP10-WORD OS 0)))

(DEFUN PDP10-HALF-WORDS (LH RH)
  (DPB LH 2222 RH))

(DEFUN STREAM-READ-PDP10-DUMP-HEADER (IS)
  (PROG (HEADER COUNT DIR N1 N2)
	(IF (NULL (SETQ HEADER (STREAM-READ-PDP10-WORD IS)))
	    (RETURN NIL))
	(SETQ COUNT (MIN 100 (1- (MINUS (LOGIOR -400000 (LDB 2222 HEADER))))))
	(FORMAT T "~%FILE HEADER: ~D WORDS" COUNT)
	(SETQ DIR (SIXBIT-TO-LISPM (STREAM-READ-PDP10-WORD IS)))
	(SETQ N1 (SIXBIT-TO-LISPM (STREAM-READ-PDP10-WORD IS)))
	(SETQ N2 (SIXBIT-TO-LISPM (STREAM-READ-PDP10-WORD IS)))
	(DOTIMES (C (- COUNT 3))
	  (STREAM-READ-PDP10-WORD IS))
   ;DIRECTORY, FN1, FN2, PACK NUMBER, CREATION-DATE (DISK FORMAT)
   	(RETURN DIR N1 N2)))

(DEFUN STREAM-WRITE-PDP10-DUMP-HEADER (OS DIR N1 N2)
  (STREAM-WRITE-PDP10-WORD OS (PDP10-HALF-WORDS -6 0))
  (STREAM-WRITE-PDP10-WORD OS (LISPM-TO-SIXBIT DIR))
  (STREAM-WRITE-PDP10-WORD OS (LISPM-TO-SIXBIT N1))
  (STREAM-WRITE-PDP10-WORD OS (LISPM-TO-SIXBIT N2))
  (STREAM-WRITE-PDP10-WORD OS 0)
  (STREAM-WRITE-PDP10-WORD OS -1))

(DEFCONST *ITS-NAME2-TYPES* '("QFASL" "DRW" "PRESS" "BIN"))

(DEFUN ITS-FILENAMES-TO-LM-PLIST (DIR N1 N2)
  (LET ((N2-IS-TYPE (MEMBER N2 *ITS-NAME2-TYPES*))
	(N2-IS-VERSION (NUMERIC-P N2)))
  `(:DIRECTORY ,DIR
    :NAME ,N1
    :TYPE ,(IF N2-IS-TYPE N2 ':UNSPECIFIC)
    :VERSION ,(COND (N2-IS-VERSION) (T ':NEWEST)))))

(DEFUN LM-PLIST-TO-ITS-FILENAMES (PLIST)
  (LET ((DIRECTORY (GET-FROM-ALTERNATING-LIST PLIST ':DIRECTORY))
	(N (GET-FROM-ALTERNATING-LIST PLIST ':NAME))
	(TYPE (GET-FROM-ALTERNATING-LIST PLIST ':TYPE))
	(VERSION (GET-FROM-ALTERNATING-LIST PLIST ':VERSION)))
    (VALUES DIRECTORY N (IF (MEMBER TYPE *ITS-NAME2-TYPES*)
			    TYPE
			    (FORMAT NIL "~D" VERSION)))))

(DEFUN SIXBIT-TO-LISPM (SIX)
  (DO ((F 3606 (- F 600))
       (ANS (MAKE-ARRAY 6 ':TYPE 'ART-STRING ':LEADER-LIST '(0)))
       (CH))
      ((MINUSP F) ANS)
    (IF (ZEROP (SETQ CH (LDB F SIX)))
	(RETURN ANS)
	(ARRAY-PUSH ANS (+ 40 CH)))))

(DEFUN LISPM-TO-SIXBIT (STR)
  (DO ((F 3606 (- F 600))
       (I 0 (1+ I))
       (ANS 0))
      ((MINUSP F) ANS)
    (IF (< I (ARRAY-ACTIVE-LENGTH STR))
	(SETQ ANS (DPB (- (CHAR-UPCASE (AR-1 STR I)) 40) F ANS)))))

(DEFUN STREAM-READ-PDP10-WORD (IS)
  (PROG (C1 C2 C3 C4 C5)
       (SETQ C1 (SEND IS ':TYI)
	     C2 (SEND IS ':TYI)
	     C3 (SEND IS ':TYI)
	     C4 (SEND IS ':TYI)
	     C5 (SEND IS ':TYI))
       (OR C1 C2 C3 C4 C5 (RETURN NIL))
       (RETURN (DPB C1 3410 (DPB C2 2410 (DPB C3 1410 (DPB C4 0410 C5)))))))

(DEFUN STREAM-WRITE-PDP10-WORD (OS WD)
  (SEND OS ':TYO (LDB 3410 WD))
  (SEND OS ':TYO (LDB 2410 WD))
  (SEND OS ':TYO (LDB 1410 WD))
  (SEND OS ':TYO (LDB 0410 WD))
  (SEND OS ':TYO (LDB 0010 WD)))
