;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.7
;;; Reason:
;;;  Fix MLY brain damage.
;;; Written 8-May-23 07:43:31 by AMS,
;;; while running on Lisp Machine One from band 3
;;; with Experimental System 100.6, microcode 323.



; From file OZ: /tree/sys/genric.lisp at 8-May-23 07:43:31
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//genric"

(defmacro key-fetch (key sequence indexvar)
  `(let ((tem (if (fixnump ,indexvar)
		     (cli:aref ,sequence ,indexvar)
		     (car ,indexvar))))
	  (if ,key (funcall ,key tem) tem)))

(defmacro key-fetch-inc (key sequence indexvar)
  `(let ((tem (if (fixnump ,indexvar)
		     (cli:aref ,sequence (prog1 ,indexvar (incf ,indexvar)))
		     (pop ,indexvar))))
	  (if ,key (funcall ,key tem) tem)))
))

;;;>>> Make sure callees (FIND-1, COUNT-1, MISMATCH-1,
;;;>>> MISMATCH-SEQUENCES-FROM-END, SEARCH) get the above macros

; From file OZ: /tree/sys/genric.lisp at 8-May-23 07:43:46
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//genric"

(defun find-1 (item sequence &optional (start 0) end
				       test invertp key from-end one-arg-predicate)
  (declare (values item index))
  (if (and from-end (arrayp sequence))
      (do ((index (1- (or end (length sequence))) (1- index))
	   elt)
	  ((< index start)
	   nil)
	(setq elt (if key (funcall key (cli:aref sequence index))
		    (cli:aref sequence index)))
	(when (eq invertp (not (cond (one-arg-predicate
				      (funcall one-arg-predicate elt))
				     (test
				      (funcall test item elt))
				     (t
				      (eql item elt)))))
	  (return (cli:aref sequence index) index)))
      (do ((index (seq-start sequence start))
	   (i start (1+ i))
	   (stop-index (if (consp end) end (seq-end sequence end)))
	   last-pos elt)
	  ((eq index stop-index)
	   (if last-pos (values elt last-pos)))
	(setq elt (key-fetch-inc key sequence index))
	(when (eq invertp (not (cond (one-arg-predicate
				      (funcall one-arg-predicate elt))
				     (test
				      (funcall test item elt))
				     (t
				      (eql item elt)))))
	  (if from-end
	      (setq last-pos i)
	    (return (values (elt sequence i) i)))))))
))

; From file OZ: /tree/sys/genric.lisp at 8-May-23 07:43:51
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//genric"

(defun count-1 (item sequence &optional (start 0) end test invertp key one-arg-predicate)
  (do ((index (seq-start sequence start))
       (count 0)
       (stop-index (seq-end sequence end))
       elt)
      ((eq index stop-index)
       count)
    (setq elt (key-fetch-inc key sequence index))
    (when (eq invertp (not (cond (one-arg-predicate
				  (funcall one-arg-predicate elt))
				 (test
				  (funcall test item elt))
				 (t
				  (eql item elt)))))
      (incf count))))
))

; From file OZ: /tree/sys/genric.lisp at 8-May-23 07:44:00
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//genric"

(defun mismatch-1 (sequence1 sequence2
		   &optional (start1 0) end1 (start2 0) end2 test invertp key from-end)
  (or end1 (setq end1 (length sequence1)))
  (or end2 (setq end2 (length sequence2)))
  (if from-end
      (funcall (if (and (arrayp sequence1) (arrayp sequence2))
		   #'mismatch-arrays-from-end
	           #'mismatch-sequences-from-end)
	       sequence1 sequence2 start1 end1 start2 end2 test invertp key)
    (or test (setq test #'eql))
    (do ((index1 (seq-start sequence1 start1))
	 (index2 (seq-start sequence2 start2))
	 (i start1 (1+ i))
	 (stop1 (seq-end sequence1 end1))
	 (stop2 (seq-end sequence2 end2)))
	((or (eq index1 stop1) (eq index2 stop2))
	 (unless (and (eq index1 stop1) (eq index2 stop2))
	   i))
      (unless (eq invertp (not (funcall test (key-fetch-inc key sequence1 index1)
					     (key-fetch-inc key sequence2 index2))))
	(return i)))))
))

; From file OZ: /tree/sys/genric.lisp at 8-May-23 07:44:08
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//genric"

(defun mismatch-sequences-from-end (sequence1 sequence2 start1 end1 start2 end2
				    test invertp key)
  (or test (setq test #'eql))
  (let ((compare-length (min (- end1 start1) (- end2 start2))))
    (do ((index1 (seq-start sequence1 (- end1 compare-length)))
	 (index2 (seq-start sequence2 (- end2 compare-length)))
	 (i (- end1 compare-length) (1+ i))
	 (last-mismatch-index1 (unless (= (- end1 start1) (- end2 start2))
				 (- end1 compare-length))))
	((= i compare-length)
	 last-mismatch-index1)
      (unless (eq invertp (not (funcall test (key-fetch-inc key sequence1 index1)
					     (key-fetch-inc key sequence2 index2))))
	(setq last-mismatch-index1 (1+ i))))))
))

; From file OZ: /tree/sys/genric.lisp at 8-May-23 07:44:16
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys//genric"

(defun search (for-sequence-1 in-sequence-2 &key (start1 0) end1 (start2 0) end2
						 from-end test test-not key)
  "Return index in IN-SEQUENCE-2 of first subsequence that matches FOR-SEQUENCE-1.
If no occurrence is found, the value is NIL.
MISMATCH is used to do the matching, with TEST, TEST-NOT and KEY passed along.
START1 and END1 are indices specifying a subsequence of FOR-SEQUENCE-1 to search for.
 The rest of FOR-SEQUENCE-1 might as well not be there.
START2 and END2 are indices specifying a subsequence of IN-SEQUENCE-2 to search through.
 However, the value returned is an index into the entire IN-SEQUENCE-2.
If FROM-END is non-NIL, the value is the index of the LAST subsequence that
 matches FOR-SEQUENCE-1 or the specified part of it.
In either case, the value returned is the index of the beginning of
 the subsequence (of IN-SEQUENCE-2) that matches."
  (let* ((length1 (- (or end1 (length for-sequence-1)) start1))
	 (really-backwards (and from-end (arrayp in-sequence-2)))
	 (pretend-backwards (and from-end (not (arrayp in-sequence-2))))
	 (real-end2 (- (or end2 (length in-sequence-2)) length1 -1))
	 (test (or test-not test #'eql))
	 (invertp (not (null test-not))))
    ;; If REALLY-BACKWARDS, we actually step backwards thru IN-SEQUENCE-2.
    ;; If PRETEND-BACKWARDS, we step forwards but remember the last thing we found.
    (do ((index (seq-start in-sequence-2 start2))
	 (inc (if really-backwards -1 1))
	 (i (if really-backwards (1- real-end2) start2)
	    (+ i inc))
	 last-index-if-from-end
	 (stop-index (if really-backwards (1- start2) (seq-end in-sequence-2 real-end2)))
	 (start-key-1 (key-fetch key for-sequence-1 (seq-start for-sequence-1 start1))))
	((eq index stop-index)
	 last-index-if-from-end)
      (if really-backwards (setq index i))
      (and (eq invertp (not (funcall test start-key-1
				     (key-fetch-inc key in-sequence-2 index))))
	   (not (mismatch-1 for-sequence-1 in-sequence-2 start1 end1 i (+ i length1)
			    test invertp key nil))
	   (if pretend-backwards
	       (setq last-index-if-from-end i)
	     (return i))))))
))
