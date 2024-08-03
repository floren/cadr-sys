;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 24.2
;;; Reason:
;;;  Rationalize tape software errors: fix typos and mistakes, straighten out
;;;  mis-called arguments.
;;; Written 26-Jul-88 10:53:31 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 2
;;; with System 125.19, ZWEI 125.3, ZMail 73.0, Local-File 75.0, File-Server 24.0, Unix-Interface 13.0, Tape 24.1, Lambda-Diag 17.0, Experimental Kermit 37.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, 7/21/88.



; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:53:37
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "

(defmethod (device-unavailable :report) (stream)
  (etypecase current-owner
    (integer
     (format stream "The ~A device is currently being used by slot #d~d"
	     device-type current-owner))
    (cons
     (format stream "The ~a device is currently being used by the device object: ~S~%~
	             ~9Tin the process: ~S"
	     device-type
	     (cdr current-owner)
	     (car current-owner)))))

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:56:09
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (hardware-error :report) (stream)
  (format stream "Tape hardware error: ~A"
	  error-message))

(compile-flavor-methods hardware-error)

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:56:25
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (driver-error :report) (stream)
  (format stream "Tape device driver error: ~A"
	  error-message))

(compile-flavor-methods driver-error)

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:57:05
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (device-unavailable :case :proceed-asking-user :steal-device-internally)
	   (cont read-func)
  "Steal the device from another process.  *** Be careful ***"
  read-func
  (funcall cont :steal-device-internally))

(defmethod (device-unavailable :case :proceed-asking-user :steal-device-from-other-processor)
	   (cont read-func)
  "Steal the device from another processor."
  read-func
  (funcall cont :steal-device-from-other-processor))

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:55:58
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(compile-flavor-methods device-unavailable)

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:57:46
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (wait-timeout :report) (stream)
  (format stream "The ~a device (unit ~D) timed out after ~A ~A"
	  device-type
	  unit
	  (time:print-interval-or-never seconds-waited nil)
	  wait-string))

(compile-flavor-methods wait-timeout)

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 10:59:00
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (end-of-tape-writing-header  :report) (stream)
  (format stream "End of tape encountered while writing header for file: ~A~&~
                  ~9Ton device ~A"
	  (car file-plist)
	  device))

(compile-flavor-methods end-of-tape-writing-header)

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 11:00:27
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (no-such-partition :report) (report-stream)
  (format report-stream 
	  "There is no partition named ~A on unit ~D of host ~A"
	  partition
	  disk-unit
	  host))

(compile-flavor-methods no-such-partition)

))

; From modified file DJ: L.TAPE; ERROR.LISP#49 at 26-Jul-88 11:04:19
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; ERROR  "


(defmethod (protocol-violation :report) (stream)
  (format stream "Protocol violation: ~A"
	  (lexpr-funcall 'format nil
			 eh:format-string
			 eh:format-args)))

(compile-flavor-methods protocol-violation)

))
