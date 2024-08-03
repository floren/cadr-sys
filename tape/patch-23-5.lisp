;;; -*- Mode:Lisp; Readtable:CL; Package:USER; Base:10; Patch-File:T -*-
;;; Patch file for Tape version 23.5
;;; Reason:
;;;  Tapemaster device doesn't seem to do search-multiple-filemarks in
;;;  reverse direction when at end of tape -- leaves you at end of tape.
;;; Written 1-Jun-88 15:51:11 by pld (Peter L. DeWolf) at site Gigamos Cambridge
;;; while running on Azathoth from band 2
;;; with Experimental System 124.15, Experimental Local-File 74.0, Experimental File-Server 23.1, Experimental Unix-Interface 12.0, Experimental ZMail 72.0, Experimental Tape 23.4, Experimental Lambda-Diag 16.0, microcode 1756, SDU Boot Tape 3.14, SDU ROM 8.



; From file DJ: L.TAPE; TAPEMASTER-DEVICE.LISP#61 at 1-Jun-88 16:02:50
#10R TAPE#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TAPE")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: TAPE; TAPEMASTER-DEVICE  "

(defmethod (tapemaster-device :search-filemark) (number-of-filemarks &optional (speed :low))
  (check-type number-of-filemarks (integer 1))
  (check-arg speed (memq speed '(:high :low)) "a valid speed arg (:HIGH or :LOW)")
  (with-device-locked self
    (let ((speed-code (case speed (:high 1) (:low 0))))
      (if (> number-of-filemarks 1)
	  (tm:search-multiple-filemarks number-of-filemarks unit 0 speed-code)
	(tm:search-filemark unit 0 speed-code)))))

(defmethod (tapemaster-device :search-filemark-reverse) (number-of-filemarks &optional (speed :low))
  (check-type number-of-filemarks (integer 1))
  (check-arg speed (memq speed '(:high :low)) "a valid speed arg (:HIGH or :LOW)")
  (with-device-locked self
    (let ((speed-code (case speed (:high 1) (:low 0))))
      (if (> number-of-filemarks 1)
	  (tm:search-multiple-filemarks number-of-filemarks unit 1 speed-code)
	(tm:search-filemark unit 1 speed-code)))))
))
