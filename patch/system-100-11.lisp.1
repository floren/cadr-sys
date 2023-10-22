;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for System version 100.11
;;; Reason:
;;;  Add support for European timezones, and daylight savings.
;;; Written 10-May-23 13:42:52 by ams,
;;; while running on Lisp Machine One from band 7
;;; with Experimental System 100.7, Hacks by AMS 1.0, microcode 323, AMS.



; From file FC: /tree/io1/time.lisp at 10-May-23 13:42:52
#10R TIME#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TIME")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //tree//io1//time"

(DEFVAR *TIMEZONES* '((0 "GMT" NIL #/Z)			;Greenwich
		      (0 "UT" NIL #/Z)
		      (1 NIL NIL #/A)
		      (2 NIL NIL #/B)
		      (3 NIL "ADT" #/C)
		      (4 "AST" "EDT" #/D)		;Atlantic
		      (5 "EST" "CDT" #/E)		;Eastern
		      (6 "CST" "MDT" #/F)		;Central
		      (7 "MST" "PDT" #/G)		;Mountain
		      (8 "PST" "YDT" #/H)		;Pacific
		      (9 "YST" "HDT" #/I)		;Yukon
		      (10. "HST" "BDT" #/K)		;Hawaiian
		      (11. "BST" NIL #/L)		;Bering
		      (12. NIL NIL #/M)
		      (-1 "CET" NIL #/N)
		      (-2 "EET" "CEST" #/O)
		      (-3 NIL "EEST" #/P)
		      (-4 NIL NIL #/Q)
		      (-5 NIL NIL #/R)
		      (-6 NIL NIL #/S)
		      (-7 NIL NIL #/T)
		      (-8 NIL NIL #/U)
		      (-9 NIL NIL #/V)
		      (-10. NIL NIL #/W)
		      (-11. NIL NIL #/X)
		      (-12. NIL NIL #/Y)
		      (3.5 "NST" NIL -1)		;Newfoundland
		      )
  "List of timezones: offset from gmt, name, daylight-savings-name, military character.")
))

; From file FC: /tree/io1/time.lisp at 10-May-23 13:43:19
#10R TIME#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TIME")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //tree//io1//time"

(DEFUN DAYLIGHT-SAVINGS-TIME-IN-EUROPE-P (SECONDS MINUTES HOURS DAY MONTH YEAR)
  "T if daylight savings time would be in effect at specified time in EU."
  ;;Last Sunday in March to Last Sunday in October, applies all over EU since 1996
  (DECLARE (IGNORE SECONDS MINUTES))
  (COND ((OR (< MONTH 3)
             (AND (= MONTH 3)
                  (LET ((LSM (LAST-SUNDAY-IN-MARCH YEAR)))
                    (OR (< DAY LSM)
                        (AND (= DAY LSM) (< HOURS 2))))))
         NIL)
        ((OR (> MONTH 10.)
             (AND (= MONTH 10.)
                  (LET ((LSO (LAST-SUNDAY-IN-OCTOBER YEAR)))
                    (OR (> DAY LSO)
                        (AND (= DAY LSO) ( HOURS 1))))))
         NIL)
        (T T)))

(DEFUN LAST-SUNDAY-IN-MARCH (YEAR)
  (IF (> YEAR 100.)
      (SETQ YEAR (- YEAR 1900.)))
  ;; This copied from GDWOBY routine in ITS
  (LET ((DOW-BEG-YEAR
          (LET ((B (CL:REM (+ YEAR 1899.) 400.)))
            (CL:REM (- (+ (1+ B) (SETQ B (FLOOR B 4))) (FLOOR B 25.)) 7)))
        (FEB29 (IF (LEAP-YEAR-P YEAR) 1 0)))
    (LET ((DOW-MARCH-31 (CL:REM (+ DOW-BEG-YEAR 89. FEB29) 7)))
      (- 31. DOW-MARCH-31))))
))
