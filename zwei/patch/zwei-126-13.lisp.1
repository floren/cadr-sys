;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 126.13
;;; Reason:
;;;  Eliminate gratuitous use of ART-Q-LIST in ZMacs.  Change *FONT-STACK*
;;;  within an interval stream to be an ART-16B (number) array.  Do FIND instead
;;;  of MEMQing down ART-Q-LIST.
;;;  
;;;  I've tested this by opening editor streams on font-ified files with
;;;  HACK-FONTS=T, and all looks well.
;;; Written 27-Sep-88 13:01:20 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 1
;;; with Experimental System 126.92, Experimental ZMail 74.1, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Unix-Interface 14.0, Experimental Tape 25.1, Experimental Lambda-Diag 18.0, Experimental ZWEI 126.12, Experimental IMicro 20.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103.





; From modified file DJ: L.ZWEI; DEFS.LISP#168 at 24-Sep-88 20:09:43
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; DEFS  "

(DEFFLAVOR INTERVAL-STREAM-WITH-FONTS
	   ((*FONT-FLAG* NIL)
	    (**FONT** 0)
	    ;;;Changed *FONT-STACK* from ART-Q-LIST to normal array. -Keith 9/24/88 
	    (*FONT-STACK* (MAKE-ARRAY 50. ':TYPE 'ART-16B ':FILL-POINTER 0)))
	   (INTERVAL-STREAM)
  :INITABLE-INSTANCE-VARIABLES)

))

; From modified file DJ: L.ZWEI; METH.LISP#53 at 24-Sep-88 20:10:37
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :TYI) (&OPTIONAL EOF &AUX CH)
  (COND ((STRINGP *FONT-FLAG*)
	 (SETQ CH (CHAR-INT (CHAR *FONT-FLAG* *INDEX*)))
	 (AND ( (SETQ *INDEX* (1+ *INDEX*)) *STOP-INDEX*)
	      (SETQ *FONT-FLAG* NIL
		    *INDEX* 0
		    *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*) *LAST-INDEX*
				   (LINE-LENGTH *LINE*))))
	 CH)
	((NULL *INDEX*)
	 (AND EOF (ERROR EOF)))
	((EQ *FONT-FLAG* T)
	 (SETQ *FONT-FLAG* NIL)
	 (+ #/0 **FONT**))
	((NUMBERP *FONT-FLAG*)
	 (PROG1 *FONT-FLAG*
		(SETQ *FONT-FLAG* NIL)))
	((< *INDEX* *STOP-INDEX*)
	 (SETQ CH (CHAR *LINE* *INDEX*))
	 (COND (( **FONT** (CHAR-FONT CH))
		(COND ((find (CHAR-FONT CH) *FONT-STACK*)
		       (SETQ **FONT** (VECTOR-POP *FONT-STACK*))
		       (SETQ *FONT-FLAG* #/*))
		      (T
		       (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
		       (SETQ **FONT** (CHAR-FONT CH))
		       (SETQ *FONT-FLAG* T)))
		(CHAR-INT #/))
	       (T
		(SETQ *INDEX* (1+ *INDEX*))
		(IF ( (CHAR-CODE CH) (CHAR-CODE #/))
		    (CHAR-CODE CH)
		  (SETQ *FONT-FLAG* (CHAR-INT CH))))))
	((EQ *LINE* *LAST-LINE*)
	 (SETQ *INDEX* NIL)
	 (AND EOF (ERROR EOF)))
	((ANTICIPATE-FONT-POP))
	(T
	 (SETQ *LINE* (LINE-NEXT *LINE*))
	 (IF (SETQ CH (GETF (LINE-PLIST *LINE*) ':DIAGRAM))
	     (LET* ((STRING (SEND CH :STRING-FOR-FILE *LINE*))
		    (LENGTH (STRING-LENGTH STRING)))
	       (AND (PLUSP LENGTH)
		    (SETQ *FONT-FLAG* STRING
			  *INDEX* 0 *STOP-INDEX* LENGTH)))
	   (SETQ *INDEX* 0 *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
					    *LAST-INDEX*
					  (LINE-LENGTH *LINE*))))
	 (CHAR-INT #/NEWLINE))))

))

; From modified file DJ: L.ZWEI; METH.LISP#53 at 24-Sep-88 20:10:48
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :LINE-IN) (&OPTIONAL SIZE EOF)
  (LET ((RET-LINE)
	(AT-END-P (EQ *LINE* *LAST-LINE*)))
    (COND ((AND (NULL SIZE)
		(NULL *FONT-FLAG*)
		(EQ 0 *INDEX*)
		(NOT AT-END-P))
	   (IF (AND (ZEROP **FONT**)
		    (DOTIMES (I (LINE-LENGTH *LINE*) T)
		      (OR (ZEROP (CHAR-FONT (CHAR *LINE* I)))
			  (RETURN NIL))))
	       ;; Easy case, line is all in font 0 and that font is current.
	       (SETQ RET-LINE *LINE*)
	     ;; Otherwise, look through actual line
	     ;; storing font changes into RET-LINE when needed.
	     (SETQ RET-LINE (MAKE-ARRAY (+ 8. (STRING-LENGTH *LINE*)) :FILL-POINTER 0
					:TYPE ART-STRING))
	     (DOTIMES (I (LINE-LENGTH *LINE*))
	       (LET ((CH (CHAR *LINE* I)))
		 (UNLESS (= (CHAR-FONT CH) **FONT**)
		   (COND ((find (CHAR-FONT CH) *FONT-STACK*)
			  (DO ()
			      ((= (CHAR-FONT CH) **FONT**))
			    (SETQ **FONT** (VECTOR-POP *FONT-STACK*))
			    (VECTOR-PUSH-EXTEND #/ RET-LINE)
			    (VECTOR-PUSH-EXTEND #/* RET-LINE)))
			 (T
			  (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
			  (SETQ **FONT** (CHAR-FONT CH))
			  (VECTOR-PUSH-EXTEND #/ RET-LINE)
			  (VECTOR-PUSH-EXTEND (+ #/0 **FONT**) RET-LINE))))
		 (SETQ CH (CHAR-CODE CH))
		 (IF (= CH (CHAR-CODE #/))
		     (VECTOR-PUSH-EXTEND #/ RET-LINE))
		 (VECTOR-PUSH-EXTEND CH RET-LINE)))
	     ;; Do one or more * now if would otherwise happen after the next few Returns.
	     (DO ()
		 ((NOT (ANTICIPATE-FONT-POP)))
	       (VECTOR-PUSH-EXTEND #/ RET-LINE)
	       (VECTOR-PUSH-EXTEND #/* RET-LINE)
	       (SETQ *FONT-FLAG* NIL)))
	   (SETQ *LINE* (LINE-NEXT *LINE*))
	   (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
				  *LAST-INDEX*
				(LINE-LENGTH *LINE*))))
	  ((NULL *INDEX*)
	   ;; End of file.
	   (SETQ RET-LINE (MAKE-STRING 0))
	   (AND EOF (ERROR EOF)))
	  (T
	   ;; Hard case; do it using our :TYI method.
	   (SETF (VALUES RET-LINE AT-END-P)
		 (STREAM-DEFAULT-HANDLER SELF :LINE-IN SIZE (LIST EOF)))
	   ;; If we are now processing the last line, set *INDEX* to NIL
	   ;; so the next :LINE-IN can get an error if desired.
	   (IF AT-END-P (SETQ *INDEX* NIL))))
    (VALUES RET-LINE AT-END-P)))

))

; From modified file DJ: L.ZWEI; METH.LISP#53 at 24-Sep-88 20:11:41
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; METH  "

(DEFUN ANTICIPATE-FONT-POP ()
  (DECLARE (:SELF-FLAVOR INTERVAL-STREAM-WITH-FONTS))
  (DO ((LINE (LINE-NEXT *LINE*) (LINE-NEXT LINE))
       F)
      ((EQ LINE *LAST-LINE*))
    (OR (GETF (LINE-PLIST LINE) ':DIAGRAM)
	(ZEROP (LINE-LENGTH LINE))
	(RETURN (UNLESS (= **FONT** (SETQ F (CHAR-FONT (CHAR LINE 0))))
		  (WHEN (find F *FONT-STACK*)
		    (SETQ **FONT** (VECTOR-POP *FONT-STACK*))
		    (SETQ *FONT-FLAG* #/*)
		    #/))))))

))
