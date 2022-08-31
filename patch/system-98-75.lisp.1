;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patches for System from 99.9
;;; Reason: (REPLACE foo foo ...) bug (99.9) 
;;; Written 14-Oct-84 01:41:54 by Mly,
;;; while running on Lisp Machine Sixteen from band 3
;;; with System 98.70, CADR 3.8, ZMail 53.18, MIT-Specific 22.4, microcode 309, gc@36.


; From file GENRIC.LISP OZ:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; GENRIC  "

(defun replace (into-sequence-1 from-sequence-2 &key (start1 0) end1 (start2 0) end2)
  "Copy all or part of FROM-SEQUENCE-2 into INTO-SEQUENCE-1.
A sequence is either a list or a vector.
START1 and END1 specify the part of FROM-SEQUENCE-2 to be copied.
 They default to 0 and NIL (which means the end of the sequence).
START2 and END2 specify the part of INTO-SEQUENCE-1 to be copied into.
If the subsequence to be copied into is longer than the one to be copied,
 the extra elements of the to-subsequence are left unchanged.
If the two sequences are the same, the data is first copied to a
 intermediate location and then copied back in.
The value is INTO-SEQUENCE-1."
  (if (eq into-sequence-1 from-sequence-2)
      (let* ((n-copy (min (- (or end2 (length from-sequence-2)) start2)
			  (- (or end1 (length into-sequence-1)) start1)))
	     (temp (get-temp-vector n-copy)))
	(replace temp from-sequence-2 :start2 start2 :end2 end2 :end1 n-copy)
	(replace into-sequence-1 temp :end2 n-copy :start1 start1 :end1 end1)
	(setq temp-vector temp))
    (if (and (arrayp into-sequence-1) (arrayp from-sequence-2))
	(let ((n-copy (min (- (or end2 (length from-sequence-2)) start2)
			   (- (or end1 (length into-sequence-1)) start1))))
	  (copy-array-portion from-sequence-2 start2 (+ start2 n-copy)
			      into-sequence-1 start1 (+ start1 n-copy)))
      (let ((store-index (if (arrayp into-sequence-1) start1 (nthcdr start1 into-sequence-1)))
	    (store-end (if end1
			   (if (arrayp into-sequence-1) end1
			     (nthcdr into-sequence-1 end1))
			 (seq-end into-sequence-1)))
	    (fetch-end (seq-end from-sequence-2 end2))
	    (fetch-index (seq-start from-sequence-2 start2)))
	(do ()
	    ((or (eq store-index store-end)
		 (eq fetch-index fetch-end)))
	  (seq-store into-sequence-1 store-index
		     (seq-fetch-inc from-sequence-2 fetch-index))
	  (seq-inc store-index)))))
  into-sequence-1)

))
