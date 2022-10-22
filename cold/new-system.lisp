;;; -*- Mode:LISP; Package:USER; Base:8; Readtable:ZL -*-

;; Notes on installing new systems on Lisp Machines

;; To load the CADR system (loads the CADR micro assembler, COMPILER
;; also uses this):
;;	(MAKE-SYSTEM 'CADR)
;; It is generally a good idea to always load the CADR system.

;; To assemble new CADR microcode:
;;	(MAKE-SYSTEM 'UCODE)
;; or for boot PROM (and non-split CADR microcode):
;;	(UA:ASSEMBLE "SYS:UCADR;PROMH LISP >")

;; To install new CADR microcode onto a partition:
;;	(SI:LOAD-MCR-FILE "SYS:UBIN;UCADR MCR >" 2)
;; Use SET-CURRENT-MICROLOAD to make a new micro load current; reboot
;; to test.  If all works, it is a good idea to load microcode symbol
;; tables (and then doing a DISK-SAVE):
;;	(CC-LOAD-UCODE-SYMBOLS "SYS:UBIN;UCADR SYM >")

;; To make a completely new system (assuming no QFASL):
;;	(NEW-SYSTEM T)
;; or to just recompile any recently modified files:
;;	(NEW-SYSTEM)
;; If also compiling the the outer system (ZMAIL, CADR
;; disassemble/assembler, etc), pass a T as a third parameter.
;;
;; Compiler messages are stored under SYS: PATCH;.
;;
;; N.B.  This does not increment the System version!

;; To make a cold load, this assumes all of SYSTEM (or at least
;; anything in SI:COLD-LOAD-FILE-LIST, see RECOMPILE-COLD-LOAD-FILES)
;; has been compiled:
;;	(MAKE-SYSTEM 'COLD)
;;	(COLD:MAKE-COLD 8)
;; Use DISK-RESTORE to try out the new cold load band.  Now you can do
;; SI:QLD, and DISK-SAVE.  When you have cold booted the fresh LOD
;; band, you can run LOAD-PATCHES to bring it back up the latest
;; version, and do a DISK-SAVE again.

;; Use SET-CURRENT-BAND to make the final load band current after
;; verifying that it works.

;; To copy between partitions use COPY-DISK-PARTITION.

(DEFUN NEW-SYSTEM (&OPTIONAL (RECOMPILEP T) (OUTERP NIL) &AUX START-TIME END-TIME)
  ;; This does some things twice, it recompiles site files, cold load
  ;; files and the system macro files twice (steps that are in general
  ;; done when recompiling SYSTEM) since this somtimes required when
  ;; you have no QFASLs.
  ;;
  ;; Have lots of extra time on your hands if you do this, this takes
  ;; an excruciating long time...
  (TV:KBD-ESC-MORE 0) 			;Turn off *MORE* processing.
  (SETQ START-TIME (TIME:PRINT-CURRENT-TIME NIL))
  (SI:FULL-GC)  (IF RECOMPILEP (RECOMPILE-AND-LOAD-SYSTEM-MACROS) (LOAD-SYSTEM-MACROS))
  (SI:FULL-GC)  (COMPILE-SITE-FILES RECOMPILEP)
  (SI:FULL-GC)  (COMPILE-RDTBL RECOMPILEP)
  ;; SYSTEM should already include all of these.
  ;;  (SI:FULL-GC)  (RECOMPILE-COLD-LOAD-FILES)
  (SI:FULL-GC)  (COMPILE-WORLD RECOMPILEP)
  ;; I don't know if this is right; UCINIT QFASL hasn't been touched
  ;; since 1982.
  ;;  (SI:FULL-GC)  (COMPILER:WRITE-INITIALLY-MICROCOMPILED-FILE)
  (SI:FULL-GC)  (COMPILE-COLD-LOADER RECOMPILEP)
  ;; Needs to be loaded, but generally no need to recompile all the
  ;; time when spinning a new system.
  (WHEN OUTERP  (SI:FULL-GC)  (COMPILE-SYSTEM 'OUTER-SYSTEM RECOMPILEP))
  (SI:FULL-GC)
  (SETQ END-TIME (TIME:PRINT-CURRENT-TIME NIL))
  (FORMAT T "~%NEW-SYSTEM started on ~A, finished at ~A.~%"
	  START-TIME END-TIME))

;; Hacks for recompiling the SYSTEM

;; SYSTEM expects that these exist as QFASLs before it can be
;; compiled.  See that this matches SYS:SYS;SYSDCL LISP ...

(DEFVAR SYSTEM-MACRO-FILES '("SYS:SYS;SYSDCL"
			     "SYS:SYS2;DEFMAC"		;These are defs files for whole system
			     "SYS:SYS2;LMMAC"
			     "SYS:SYS2;STRUCT"
			     "SYS:SYS2;SETF"
			     "SYS:SYS;TYPES"
			     ))

(DEFUN LOAD-SYSTEM-MACROS ()
  (MAPCAR 'LOAD SYSTEM-MACRO-FILES))

(DEFUN RECOMPILE-AND-LOAD-SYSTEM-MACROS ()
  (QC-FILES SYSTEM-MACRO-FILES T))

(DEFUN COMPILE-SITE-FILES (&OPTIONAL (RECOMPILEP NIL) (RELOADP NIL))
  (COMPILE-SYSTEM 'SITE RECOMPILEP)
  (AND RELOADP
       (SI:UPDATE-SITE-CONFIGURATION-INFO)
       (FS:DEFINE-SYS-LOGICAL-DEVICE)))

(DEFUN RTC-FILE-1 (INPUT OUTPUT)
  (SI:RTC-FILE INPUT))

(SI:DEFINE-SIMPLE-TRANSFORMATION :RTC-COMPILE RTC-FILE-1
  SI:FILE-NEWER-THAN-FILE-P (:LISP) (:QFASL))

(DEFMACRO (:RTC-COMPILE-LOAD SI:DEFSYSTEM-MACRO) (INPUT &OPTIONAL COM-DEP LOAD-DEP COM-COND LOAD-COND)
  `(:FASLOAD (:RTC-COMPILE ,INPUT ,COM-DEP ,COM-COND) ,LOAD-DEP ,LOAD-COND))

(DEFSYSTEM READER
  (:PATHNAME-DEFAULT "SYS:IO;")
  (:PACKAGE SYSTEM-INTERNALS)
  (:MODULE DEFS "RDDEFS")
  (:MODULE READER "READ")
  (:MODULE READ-TABLE-COMPILER "RTC")
  (:MODULE READ-TABLE ("RDTBL" "CRDTBL"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD READER (:FASLOAD DEFS))
  (:SKIP :FASLOAD (:COMPILE READ-TABLE-COMPILER))
  (:RTC-COMPILE-LOAD READ-TABLE (:FASLOAD READ-TABLE-COMPILER)))

(DEFUN COMPILE-RDTBL (&OPTIONAL (RECOMPILEP NIL))
  (UNLESS (PROBEF "SYS:IO;RTC QFASL")
    (QC-FILES '("SYS:IO;RTC")))
  (COMPILE-SYSTEM 'READER RECOMPILEP))

(DEFUN RECOMPILE-COLD-LOAD-FILES ()
  ;; CDR to skip FONTS; CPTFON QFASL.
  (QC-FILES (CDR SI:COLD-LOAD-FILE-LIST)))

;; See that this matches SYS:SYS;SYSDCL LISP...
(DEFVAR SYSTEM-WORLD '(FONTS
		       SYSTEM-INTERNALS
		       FORMAT
		       CHAOS
		       COMPILER
		       FILE-SYSTEM
		       QFASL-REL
		       TIME
		       TV
		       PEEK
		       SUPDUP
		       ZWEI
		       FED
		       COLOR
		       EH
		       PRESS
		       MATH
		       HACKS
		       METER
		       SRCCOM
		       CONVERSE
		       ))

(DEFUN COMPILE-WORLD (&OPTIONAL (RECOMPILEP NIL))
  ;; This does the same as below (hopefully); but does a GC beteween
  ;; each invocation in hope to manage recompiling the whole system
  ;; without running low on virtual memory.
  ;;  (COMPILE-SYSTEM 'SYSTEM RECOMPILEP)
  (IF RECOMPILEP
      (RECOMPILE-AND-LOAD-SYSTEM-MACROS)
      (LOAD-SYSTEM-MACROS))
  (DOLIST (PKG SYSTEM-WORLD)
    (PRINT 'COMPILING...)
    (PRINT PKG)
    (SI:FULL-GC)  (COMPILE-SYSTEM PKG RECOMPILEP)))

(DEFUN COMPILE-COLD-LOADER (&OPTIONAL (RECOMPILEP NIL))
  ;; This needs to be done only if we don't have any QFASL for the
  ;; cold load generator.
  (UNLESS (PROBEF "SYS:COLD;COLDUT QFASL")
    (LOAD "SYS:COLD;COLDPK LISP")
    (QC-FILES '("SYS:COLD;COLDUT")))
  (COMPILE-SYSTEM 'COLD RECOMPILEP))

(DEFSYSTEM OUTER-SYSTEM
  ;; List of systems that we may want when we make a final load band.
  (:COMPONENT-SYSTEMS
   ;; Systems not initially loaded, but done right afterwards
   MIT-SPECIFIC
   CADR
   ZMAIL
   ;; Systems defined elsewhere
   FILE-SYSTEM-UTILITIES ;Consists of: LOCAL-FILE MAGTAPE FILE-SERVER
   ;; Misc. systems used by MAGTAPE and FILE.
   DISTRIBUTION ITS-TAPE VMS-TAPE
   ;;LMFILE-REMOTE
;;   LFS ;LMFILE-SERVER
   ))

(DEFUN COMPILE-OUTER-SYSTEM (&OPTIONAL (RECOMPILEP NIL))
  (COMPILE-SYSTEM 'OUTER-SYSTEM RECOMPILEP))

(DEFUN LOAD-OUTER-SYSTEM (&OPTIONAL (RELOADP NIL))
  (IF RELOADP
      (MAKE-SYSTEM 'OUTER-SYSTEM :RELOAD)
      (MAKE-SYSTEM 'OUTER-SYSTEM)))

(DEFUN QC-FILES (FILE-LIST &OPTIONAL (LOADP NIL))
  (AND (LISTP (CAR FILE-LIST)) (SETQ FILE-LIST (CAR FILE-LIST)))
  (MAPCAR 'PRINT FILE-LIST)
  (PRINT 'COMPILING...)
  (LOOP FOR QFASL-FILE IN FILE-LIST
	AS FILE = (SEND (FS:PARSE-PATHNAME QFASL-FILE) :NEW-TYPE "LISP")
	DO (PRINT FILE)
	DO (QC-FILE FILE)
	FINALLY (WHEN LOADP (MAPCAR 'LOAD FILE-LIST))))

(DEFUN COMPILE-SYSTEM (SYSTEM &OPTIONAL (RECOMPILEP NIL))
  (IF RECOMPILEP
      (MAKE-SYSTEM SYSTEM :RECOMPILE :NOLOAD :NO-INCREMENT-PATCH :DEFAULTED-BATCH)
      (MAKE-SYSTEM SYSTEM   :COMPILE :NOLOAD :NO-INCREMENT-PATCH :DEFAULTED-BATCH))
  ;; Since we use :DEFAULTED-BATCH; we have a CWARNS file to worry
  ;; about.
  (LET* ((PATCH-DIR (SI:PATCH-DIRECTORY-PATHNAME (SI:SYSTEM-PATCH-DIRECTORY (SI:FIND-SYSTEM-NAMED "SYSTEM"))))
	 (OLD-CWARNS-PATHNAME
	   ;; This needs to be the same logic as in SYS2; MAKSYS LISP.
	   (OR (SI:SYSTEM-WARNINGS-PATHNAME-DEFAULT (SI:FIND-SYSTEM-NAMED SYSTEM))
	       (SEND (FS:USER-HOMEDIR) :NEW-PATHNAME
		     :NAME "CWARNS" :TYPE :LISP
		     :VERSION :NEWEST)))
	 (NEW-CWARNS-PATHNAME
	   (SEND PATCH-DIR :NEW-PATHNAME
		 :NAME (FORMAT NIL "CWARNS-~A" SYSTEM) :TYPE :LISP
		 :VERSION :NEWEST)))
    ;; Would be nice if we cared about any old CWARNS -- but we don't.
    (DELETEF NEW-CWARNS-PATHNAME NIL)
    (RENAMEF OLD-CWARNS-PATHNAME NEW-CWARNS-PATHNAME NIL)
    NEW-CWARNS-PATHNAME))
