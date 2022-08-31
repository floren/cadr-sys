;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 51.5
;;; Reason: COPY-MSG and Undo saving bug.
;;; Written 9/21/83 18:14:44 by EB.TFC,
;;; while running on Lisp Machine One from band 8
;;; with Bad System 97.6, CADR 1.0, Inconsistently updated ZMail 51.4, MIT-Specific 21.0, microcode 257, ZM MIT.



; From file MFILES.LISP SRC:<L.ZMAIL> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZMAIL; MFILES  "

(DEFUN COPY-MSG (MSG &AUX NMSG)
  "Return a copy of MSG."
  (SETQ NMSG (MAKE-EMPTY-MSG))
  (SETF (MSG-STATUS NMSG)
	(SOME-PLIST-NOT (CAR (ASSURE-MSG-PARSED MSG)) *PROPERTIES-NOT-COPIED*))
  (SETF (MSG-PARSED-P NMSG) T)
  (LET* ((OLD-LINE (MSG-SUMMARY-LINE MSG))
	 (NEW-LINE (MAKE-SUMMARY-LINE MAKE-ARRAY (:LENGTH (ARRAY-ACTIVE-LENGTH OLD-LINE)))))
    (SETF (MSG-SUMMARY-LINE NMSG) NEW-LINE)
    (COPY-ARRAY-CONTENTS-AND-LEADER OLD-LINE NEW-LINE))
  (SETF (NODE-UNDO-STATUS (MSG-REAL-INTERVAL NMSG)) ':DONT)
  (INSERT-INTERVAL (MSG-END-BP NMSG) (MSG-INTERVAL MSG))
  ;; Mpdify the HEADERS-END-BP property in the MSG-STATUS
  ;; so that it points into the copied text.
  ;; Try to preserve where it points.
  (LET ((HEADERS-END-BP (GET (LOCF (MSG-STATUS MSG)) 'HEADERS-END-BP)))
    (WHEN HEADERS-END-BP
      (DO ((OLINE (BP-LINE (INTERVAL-FIRST-BP (MSG-INTERVAL MSG))) (LINE-NEXT OLINE))
	   (NLINE (BP-LINE (INTERVAL-FIRST-BP (MSG-INTERVAL NMSG))) (LINE-NEXT NLINE)))
	  ((OR (NULL OLINE) (NULL NLINE))
	   (REMPROP (LOCF (MSG-STATUS NMSG)) 'HEADERS-END-BP))
	;; When we find the line in the old interval that the old bp points to,
	;; make the new bp point at the corresponding new line.
	(WHEN (EQ OLINE (CAR HEADERS-END-BP))
	  (PUTPROP (LOCF (MSG-STATUS NMSG)) (CREATE-BP NLINE (BP-INDEX HEADERS-END-BP))
		   'HEADERS-END-BP)
	  (RETURN)))))
  NMSG)

))

(compile-flavor-methods zwei:vms-inbox-buffer)
