;;; -*- Mode: Lisp; Package: SYSTEM-INTERNALS; Base: 10.; Readtable:T -*-

;;; This file contains information on the physical location of the various
;;; lisp machines.
;;; 
;;; For MIT: OZ:SRC:<L.SITE>LMLOCS.LISP.
;;; 
;;; Format is
;;; (name machine-name finger-location (building floor) associated-machine
;;;   site-keyword-overriding-alist)
;;; 
;;; Each machine is the only source of knowledge of where it is; all
;;; programs which print machine locations ask each machine for its
;;; location.
;;; 
;;; To change a machine's location knowledge:
;;; 
;;; (1) Edit this file to correctly list the affected machine's location.
;;; 
;;; (2) Compile this file on a LispM, with (MAKE-SYSTEM 'SITE 'COMPILE)
;;; 
;;; (3) Load it by doing (MAKE-SYSTEM 'SITE).
;;; 
;;; To make this knowledge permanent, do (DISK-SAVE <n>) immediately after
;;; loading the qfasl file.  This will add the new location knowledge to
;;; the band that you loaded the file into.  See the LispM Manual, section
;;; 32.10, for more information on saving and restoring bands.

(DEFCONST MACHINE-LOCATION-ALIST
 '(

   ("MIT-LISPM-1"  "Lisp Machine One"       "907 [Son of CONS] CADR1's Room x6765"
    (MIT-NE43 9) "OZ") 

  )

  "This variable has a list of local lisp machine hosts and their associated finger info.
All lisp machines that are on the chaosnet and are to be considered part of this site
should be on this list.
  The format is:
  (name machine-name finger-location (building floor) associated-machine
   site-keyword-overriding-alist)")

