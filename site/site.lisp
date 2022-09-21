;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-
;;; Site declaration for MIT
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFCONST OZ-SYS-PATHNAME-TRANSLATIONS
  '(("CC" "//TREE//CC//")
    ("CHAOS" "//TREE//CHAOS//")
    ("COLD" "//TREE//COLD//")
    ("DEMO" "//TREE//DEMO//")
    ("DISTRIBUTION" "//TREE//DISTRIBUTION//")
    ("DOC" "//TREE//DOC//")
    ("EH" "//TREE//EH//")
    ("FILE" "//TREE//FILE//")
    ("FILE2" "//TREE//FILE2//")
    ("FONTS" "//TREE//FONTS//")
    ("IO" "//TREE//IO//")
    ("IO1" "//TREE//IO1//")
    ("IP" "//TREE//NETWORK//IP//")
    ("ISPELL" "//TREE//ISPELL//")
    ("LAMBDA-DIAG" "//TREE//LAMBDA-DIAG//")
    ("LAMBDA-UCODE" "//TREE//LAMBDA-UCODE//")
    ("LIB" "//TREE//LIB//")
    ("MAN" "//TREE//MAN//")
    ("NETWORK" "//TREE//NETWORK//")
    ("CHNCP" "//TREE//NETWORK//CHAOS//")	; Prefer NETWORK;CHAOS; for back-translation
    ("PATCH" "//TREE//PATCH//")
;;;    ("PRESS-FONTS" "PS:<FONTS>")
    ("SITE" "//TREE//SITE//")
    ("SYS" "//TREE//SYS//")
    ("SYS2" "//TREE//SYS2//")
    ("TAPE" "//TREE//TAPE//")
    ("UBIN" "//TREE//UBIN//")
    ("UCADR" "//TREE//UCADR//")
    ("WINDOW" "//TREE//WINDOW//")
    ("WIND" "//TREE//WIND//")
    ("ZMAIL" "//TREE//ZMAIL//")
    ("ZWEI" "//TREE//ZWEI//")
    ))

(DEFCONST LM-SYS-PATHNAME-TRANSLATIONS
  '(("CC" "<L.CC>")
    ("CHAOS" "<L.CHAOS>")
    ("DEMO" "<L.DEMO>")
    ("DOC" "<L.DOC>")
    ("FILE" "<L.FILE>")
    ("FILE2" "<L.FILE2>")
    ("FONTS" "<L.FONTS>")
    ("IO" "<L.IO>")
    ("IO1" "<L.IO1>")
    ("ISPELL" "<L.ISPELL>")
    ("LAMBDA-DIAG" "<L.LAMBDA-DIAG>")
    ("LAMBDA-UCODE" "<L.LAMBDA-UCODE>")
    ("LIB" "<L.LIB>")
    ("MAN" "<L.MAN>")
    ("PATCH" "<L.PATCH>")
    ("PRESS-FONTS" "<L.PRESS-FONTS>")
    ("SITE" "<L.SITE>")
    ("SYS" "<L.SYS>")
    ("SYS2" "<L.SYS2>")
    ("TAPE" "<L.TAPE>")
    ("UBIN" "<L.UBIN>")
    ("UCADR" "<L.UCADR>")
    ("WINDOW" "<L.WINDOW>")
    ("WIND" "<L.WIND>")
    ("ZMAIL" "<L.ZMAIL>")
    ("ZWEI" "<L.ZWEI>")
    ))

(DEFSITE :MIT
  ;; OZ is where the sources are
  (:SYS-HOST "OZ")
  ;; SYS: translations to use
  (:SYS-HOST-TRANSLATION-ALIST '(("OZ" . OZ-SYS-PATHNAME-TRANSLATIONS)
				 ("LM" . LM-SYS-PATHNAME-TRANSLATIONS)
				 (NIL . LM-SYS-PATHNAME-TRANSLATIONS)))
  ;; These two say how to log in to get error tables, etc.
  (:SYS-LOGIN-NAME "LISPM")
  (:SYS-LOGIN-PASSWORD "LISPM")
  ;; For internet domain addressing, these domains
  ;; are those which the local site belongs to.
  (:LOCAL-INTERNET-DOMAINS '("ARPA"))
  ;; Has a local chaosnet
  (:CHAOS T)
  ;; But no (working) ethernet
  (:ETHER NIL)
  ;; File computers using the chaosnet file server protocol
  (:CHAOS-FILE-SERVER-HOSTS '("OZ"))
  ;; Hosts suspected of supporting time servers
  (:CHAOS-TIME-SERVER-HOSTS '("OZ"))
  ;; Hosts that know about hosts off the chaosnet
  (:CHAOS-HOST-TABLE-SERVER-HOSTS '("OZ"))
  ;; Hosts that have mail servers capable of forwarding mail anyplace
  (:CHAOS-MAIL-SERVER-HOSTS '("OZ"))
  ;; Hosts that have SPELL servers.
  (:SPELL-SERVER-HOSTS '("OZ"))
  ;; Our LISPM host names usually start out as CADR-
  (:USUAL-LM-NAME-PREFIX "CADR-")
  ;; EST
  (:TIMEZONE 5)
  ;; Destination for mail to BUG-FOOBAR
  (:HOST-FOR-BUG-REPORTS "MIT-OZ")
  ;; "Local sites", used by ZMail summary display
  (:LOCAL-MAIL-HOSTS '("OZ"))
  ;; Mail can be sent by COMSAT
  (:COMSAT T)
  ;; But that is not the default
  (:DEFAULT-MAIL-MODE ':CHAOS)
  ;; No GMSGS for collecting system messages
  (:GMSGS NIL)
  ;; No Dover ... either.
  (:DOVER NIL)
  ;; Interpretation of arguments to ESC F
  (:ESC-F-ARG-ALIST '((NIL . :LOGIN) (1 . :LISP-MACHINES)
		      (2 "OZ") 
		      (8 . :ALL-LISP-MACHINES)
		      (0 . :READ)))
  ;; These machines normally have local file systems (potentially of one sort
  ;; or another).  Being on this list means that SI:LOCAL-HOST gets added to
  ;; *pathname-host-list*.  KEY is name as returned by :NAME message to host
  ;; object.  As of system 91, this ought not to matter.
  (:MACHINES-WITH-LOCAL-FILE-SYSTEMS  '(("MIT-LISPM-1")))

  ;for common lisp compatibility

  ;;We are an MIT Lisp Machine.  This option obsolete as of system 98, though.
  (:DEFAULT-MACHINE-TYPE "MIT-CADR")

  ;;This is what Steele says we are
  (:SHORT-SITE-NAME "MIT AI Lab")

  ;;This was as long as I could make it.
  (:LONG-SITE-NAME "Massachusetts Institute of Technology, Artificial Intelligence Laboratory")
  )


