;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by BV
;;; Reason:
;;;  Copy from LMI.
;;; Written 12-Feb-24 21:31:32 by BV,
;;; while running on CDR (CADR) from band 3
;;; with Experimental System 100.22, Experimental Local-File 54.0, Experimental FILE-Server 11.0, Experimental Tape 27.0, microcode 323, 230816.



; From file OZ: /tree/sys2/string.lisp at 12-Feb-24 21:31:39
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "OZ: //tree//sys2//string"

(defun string-matchp (string1 string2)
  "Like STRING-EQUAL but STRING1 can have wildchar indicators:
`%' = match one character, `*' = match any number of characters from STRING2"
  ;; 17-Mar-86 17:16:08 -gjc
  (IF (or (string-search #\* string1)
	  (string-search #\% string1))
      (string-matchp-1 string1 0 (string-length string1)
		       string2 0 (string-length string2))
    (string-equal string1 string2)))

(defun string-matchp-1 (string1 i1 n1 string2 i2 n2)
  (prog (temp)
	loop
	(if (and (= i1 n1) (= i2 n2)) (return t))
	(if (= i1 n1) (return nil))
	(if (= i2 n2) (go check-star))
	(when (or (char-equal (setq temp (aref string1 i1)) #\%)
		  (char-equal temp (aref string2 i2)))
	  (setq i1 (1+ i1) i2 (1+ i2))
	  (go loop))
	check-star
	(if (char-equal (aref string1 i1) #\*)
	    (cond ((= (1+ i1) n1) (return t))
		  ((= i2 n2) (return nil))
		  ((string-matchp-1 string1 (1+ i1) n1 string2 (1+ i2) n2)
		   (return t))
		  ((string-matchp-1 string1 i1 n1 string2 (1+ i2) n2)
		   (return t))
		  ('else
		   (setq i1 (1+ i1))
		   (go loop)))
	    (return nil))))
))
