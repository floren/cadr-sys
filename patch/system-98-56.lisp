;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 5/21/84 05:43:12 by RpK,
;;; Reason: TIME:INITIALIZE-TIMEBASE doesn't try the net if we're :STANDALONE.
;;; CHAOS system has files in a new place.
;;; while running on Lisp Machine Two from band 4
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 98.49, CADR 3.6, ZMail 53.17, MIT-Specific 22.0, microcode 309, gc@36.



; From file TIME.LISP PS:<L.IO1> OZ:
#8R TIME#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TIME")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; TIME  "

(DEFUN INITIALIZE-TIMEBASE (&OPTIONAL UT)
  "Set the clock.
Possible sources of the time include the network, the Lambda SDU clock,
and, failing that, the luser who happens to be around."
  (AND (NULL UT) (NOT (SI:GET-SITE-OPTION :STANDALONE)) *NETWORK-TIME-FUNCTION* 
       (SETQ UT (FUNCALL *NETWORK-TIME-FUNCTION*)))
  (TAGBODY
      (AND (NUMBERP UT) (GO DO-IT))
      ; don't deal with SDU clock until cold load is done ...
      (if-in-lambda
	(SETQ UT (RTC-GET-UNIVERSAL-TIME))
	(IF UT (GO DO-IT)))		; change to GIVE-IT-A-SHOT if the user should be asked
   STRING
      (FORMAT *QUERY-IO* "~&Please type the date and time: ")
      (SETQ UT (READLINE *QUERY-IO*))
      (WHEN (STRING-EQUAL UT "")
	(IF (Y-OR-N-P "Do you want to specify the time or not? ")
	    (GO STRING)
	  (SETQ *LAST-TIME-UPDATE-TIME* NIL)
	  (RETURN-FROM INITIALIZE-TIMEBASE NIL)))
      (CONDITION-CASE (ERROR)
	  (SETQ UT (PARSE-UNIVERSAL-TIME UT 0 NIL T 0))
	(ERROR (SEND ERROR ':REPORT *QUERY-IO*)
	       (GO STRING)))
   GIVE-IT-A-SHOT
      (COND ((NOT (Y-OR-N-P (FORMAT NIL "Time is ~A, OK? " (PRINT-UNIVERSAL-DATE UT NIL))))
	     (GO STRING)))
   DO-IT
      (WITHOUT-INTERRUPTS
	(IF (NOT (NULL *UT-AT-BOOT-TIME*))
	    ;;if we are randomly changing the time while up, mung uptime
	    (SETQ *UT-AT-BOOT-TIME*
		  (+ *UT-AT-BOOT-TIME* (- UT (GET-UNIVERSAL-TIME))))
	  ;;no real surprise: changing at boot time
	  (SETQ *UT-AT-BOOT-TIME* UT))
	(SETF (VALUES *LAST-TIME-UPDATE-TIME* PREVIOUS-TOP-9-TIME-BITS)
	      (FIXNUM-MICROSECOND-TIME))
	(MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			 *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			 *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
	  (DECODE-UNIVERSAL-TIME UT))
	(if-in-lambda (rtc-set-universal-time ut))
	(RETURN-FROM INITIALIZE-TIMEBASE T))))
))

; From file SYSDCL.LISP PS:<L.SYS> OZ:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS; SYSDCL  "

(DEFSYSTEM CHAOS
  (:PACKAGE CHAOS)
  (:MODULE NCP ("SYS: CHNCP; CHSNCP" "SYS: CHNCP; CHUSE"))
  (:MODULE AUX "SYS: CHNCP; CHSAUX")
  (:MODULE TEST "SYS: CHNCP; CHATST")
  (:MODULE EFTP "SYS: CHNCP; EFTP")
  (:COMPILE-LOAD (NCP AUX TEST EFTP))
  (:COMPILE-LOAD (:GENERATE-HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL")))))

))
