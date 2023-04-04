;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-

(DEFVAR CLEARED-BIG-RQB NIL)
(DEFVAR CLEARED-SMALL-RQB NIL)

(DEFMACRO WITH-CLEARED-RQBS (&BODY BODY)
  `(LET (RETURN-RQBS)
     (UNWIND-PROTECT
       (PROGN (COND ((NULL CLEARED-BIG-RQB)
		     (SETQ RETURN-RQBS T)
		     (BIND (LOCF CLEARED-BIG-RQB) (GET-DISK-RQB 10.))
		     (COPY-ARRAY-CONTENTS "" (RQB-BUFFER CLEARED-BIG-RQB))
		     (BIND (LOCF CLEARED-SMALL-RQB) (GET-DISK-RQB 1.))
		     (COPY-ARRAY-CONTENTS "" (RQB-BUFFER CLEARED-SMALL-RQB))))
	      . ,BODY)
       (COND (RETURN-RQBS
	      (RETURN-DISK-RQB CLEARED-BIG-RQB)
	      (RETURN-DISK-RQB CLEARED-SMALL-RQB))))))

(DEFUN CLEAR-BLOCK (UNIT ADDR NPAGES)
  (WITH-CLEARED-RQBS
    (LET* ((N-SMALL (REMAINDER NPAGES 10.))
	   (N-BIG (- NPAGES N-SMALL)))
      (DO ((I N-BIG (1- I))
	   (A ADDR (+ A 10.)))
	  ((ZEROP I))
	(DISK-WRITE CLEARED-BIG-RQB UNIT A))
      (DO ((I N-SMALL (1- I))
	   (A (1- (+ ADDR NPAGES)) (1- A)))
	  ((ZEROP I))
	(DISK-WRITE CLEARED-SMALL-RQB UNIT A)))))

(DEFUN CLEAR-PARTITION (NAME &OPTIONAL (UNIT 0))
  (MULTIPLE-VALUE-BIND (BASE NPAGES)
      (FIND-DISK-PARTITION NAME NIL UNIT)
    (CLEAR-BLOCK UNIT BASE NPAGES)))

(DEFUN CLEAR-PARTITIONS (&REST NAMES)
  (WITH-CLEARED-RQBS
    (DOLIST (N NAMES)
      (CLEAR-PARTITION N 0))))

(DEFUN CLEAR-FILE-SYSTEM ()
  (WITH-CLEARED-RQBS
    (LET ((SIZE (DC-PARTITION-SIZE DISK-CONFIGURATION)))
      (DO ((I 1 (1+ I))
	   (A (DC-PARTITION-BASE DISK-CONFIGURATION) (1+ A)))
	  (( A SIZE))
	(COND ((= (AREF PAGE-USAGE-TABLE I) PUT-FREE)
	       (DISK-WRITE CLEARED-SMALL-RQB LM-UNIT A)))))))