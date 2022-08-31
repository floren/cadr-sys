;;; -*- Mode: Lisp; Package: ZWEI; Base: 8.; Patch-File: T -*-
;;; Written 3/24/85 04:39:30 by Mly,
;;; Reason: Somebody spazzed in installing 53.20
;;; while running on Lisp Machine Twenty-five from band 2
;;; with System 98.78, CADR 3.10, ZMail 53.19, MIT-Specific 22.5, microcode 309, ZM MIT.

;;; -*- Mode:Lisp; Readtable:ZL; Package:ZWEI; Base:10.; Patch-File:T -*-
;;; Patch file for ZMail version 54.4
;;; Reason:
;;;  Kill the last of the " at "'s
;;; Written 16-Mar-85 20:00:20 by mly,
;;; while running on Lisp Machine Nine from band 7
;;; with System 99.22, CADR 4.2, Experimental ZMail 54.3, MIT-Specific 23.0, Experimental Macsyma 5.5, microcode 320, GC@2.



; From file OZ:KANSAS:<L.ZMAIL>MAIL.LISP.311 at 16-Mar-85 20:02:44
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; MAIL  "

(DEFUN STRING-FROM-HEADER (HEADER FORMAT &AUX (PLIST (LOCF HEADER)) TEM)
  (IF (AND (EQ FORMAT :USE-ORIGINAL)
	   (SETQ TEM (GET PLIST :INTERVAL)))
      (STRING-INTERVAL (FIRST TEM) (SECOND TEM) T)
    (LET ((STRING (GET PLIST :NAME)))
      ;; If talking to the server, strip off any quoting
      (AND (EQ FORMAT :HOST)
	   (QUOTED-RECIPIENT-P STRING)
	   (SETQ STRING (SUBSTRING STRING 1 (1- (STRING-LENGTH STRING)))))
      (LET ((HOST (GET PLIST :HOST))
	    (AT (IF (MEMQ FORMAT '(:SHORT :HOST)) #/@ #/@ #|| " at " ||# )))
	(IF (AND HOST *QUOTE-HOSTS-FOR-XMAILR*)
	    (SETQ STRING (STRING-APPEND STRING AT #/ (CAR HOST) #/))
	  (DO ((HS HOST (CDR HS)))
	      ((NULL HS))
	    (SETQ STRING (STRING-APPEND STRING (IF (CDR HS) "%" AT) (CAR HS))))))
      (AND (MEMQ FORMAT '(:USE-ORIGINAL :INCLUDE-PERSONAL))
	   (SETQ TEM (GET PLIST :PERSONAL-NAME))
	   (PROGN
	     (AND (NOT (QUOTED-RECIPIENT-P TEM))
		  (STRING-SEARCH-SET "/"," TEM)
		  (SETQ TEM (FORMAT:OUTPUT NIL (PRINT TEM))))
	     (SETQ STRING (STRING-APPEND TEM " <" STRING #/>))))
      STRING)))

))

; From file OZ:KANSAS:<L.ZMAIL>DEFS.LISP.273 at 16-Mar-85 20:04:58
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; DEFS  "

(DEFVAR *HEADER-FORMAT-ALIST* '(("Short" :VALUE :SHORT :DOCUMENTATION "No personal names.")
				("Long" :VALUE :LONG :DOCUMENTATION "No personal names.")
				("Include personal" :VALUE :INCLUDE-PERSONAL :DOCUMENTATION
  "Include the user's personal name if any.")
				("Use original" :VALUE :USE-ORIGINAL :DOCUMENTATION
  "Use the exact text of the address from the original."))
  "A list of the header formats available.")

))
