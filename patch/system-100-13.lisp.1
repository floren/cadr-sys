;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.13
;;; Reason:
;;;  INSPECT: Fix so that it doesn't go to cold-load stream.
;;; Written 30-May-23 17:49:10 by ams,
;;; while running on Lisp Machine One from band 7
;;; with Experimental System 100.12, Hacks by AMS 1.0, microcode 323, AMS.



; From file FC: /sys/window/tscrol.lisp at 30-May-23 17:49:10
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//window//tscrol"

(DEFMETHOD (FUNCTION-TEXT-SCROLL-WINDOW :SETUP) (LIST)
  ;; Label changing should be first -- this may cause redisplay so flush current items too
  (AND ITEMS (STORE-ARRAY-LEADER 0 ITEMS 0))
  (AND ( (LENGTH LIST) 5) (SEND SELF :SET-LABEL (FIFTH LIST)))
  (SEND SELF :SET-PRINT-FUNCTION (FIRST LIST))
  (SEND SELF :SET-PRINT-FUNCTION-ARG (SECOND LIST))
  (SETQ TOP-ITEM (OR (FOURTH LIST) 0))
  (IF (SIXTH LIST)
      (SEND SELF :SET-ITEM-GENERATOR (SIXTH LIST))
    (LET ((ARRAY (OR ITEMS (MAKE-ARRAY (LENGTH (THIRD LIST)) :FILL-POINTER 0))))
      (DOLIST (L (THIRD LIST)) (VECTOR-PUSH-EXTEND L ARRAY))
      (SEND SELF :SET-ITEMS ARRAY)))
  LIST)

))
