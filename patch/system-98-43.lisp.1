;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 3/15/84 03:00:54 by TIM,
;;; Reason: Added new EMACS-compatible ZWEI variable:  Next Screen Context Lines
;;; while running on Lisp Machine Thirty-one from band 3
;;; with System 98.32, CADR 3.6, ZMail 53.10, MIT-Specific 22.0, microcode 306.



; From file MACROS.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; MACROS  "

(DEFVARIABLE *NEXT-SCREEN-CONTEXT-LINES* 1 :FIXNUM
   "The number of lines of overlap for Next Screen and Previous Screen commands")

))

; From file COMA.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "


(DEFCOM COM-NEXT-SCREEN "Move down to display next screenful of text.
With argument, move window down <arg> lines." (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (IF *NUMERIC-ARG-P*
					 *NUMERIC-ARG*
					 (- (WINDOW-N-PLINES *WINDOW*)
					    *NEXT-SCREEN-CONTEXT-LINES*)))
  DIS-NONE)

))

; From file COMA.LISP PS:<L.ZWEI> OZ:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: ZWEI; COMA  "


(DEFCOM COM-PREVIOUS-SCREEN "Move up to display previous screenful of text.
With argument, move window up <arg> lines." (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (IF *NUMERIC-ARG-P*
					 (- *NUMERIC-ARG*)
					 (- *NEXT-SCREEN-CONTEXT-LINES*
					    (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

))
