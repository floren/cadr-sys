;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-
;;; Site declaration for MIT
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFCONST OZ-SYS-PATHNAME-TRANSLATIONS
  ;; this pos not used in 99. (fs:make-logical-pathname-host "SYS")
  '(("CC" "OZ:<L.CC>")
    ("CHAOS" "OZ:<L.CHAOS>")
    ("COLD" "OZ:<L.COLD>")
    ("DEMO" "OZ:<L.DEMO>")
    ("DISTRIBUTION" "OZ:<L.DISTRIBUTION>")
    ("DOC" "OZ:<L.DOC>")
    ("FILE" "OZ:<L.FILE>")
    ("FILE2" "OZ:<L.FILE2>")
    ("FONTS" "OZ:<L.FONTS>")
    ("IO" "OZ:<L.IO>")
    ("IO1" "OZ:<L.IO1>")
    ("IP" "OZ:<L.NETWORK.IP>")
    ("ISPELL" "OZ:<L.ISPELL>")
    ("LAMBDA-DIAG" "OZ:<L.LAMBDA-DIAG>")
    ("LAMBDA-UCODE" "OZ:<L.LAMBDA-UCODE>")
    ("LIB" "OZ:<L.LIB>")
    ("MAN" "OZ:<L.MAN>")
    ("NETWORK" "OZ:<L.NETWORK>")
    ("CHNCP" "OZ:<L.NETWORK.CHAOS>")		; Prefer NETWORK;CHAOS; for back-translation
    ("PATCH" "OZ:<L.PATCH>")
    ("PRESS-FONTS" "OZ:<FONTS>")
    ("SITE" "OZ:<L.SITE>")
    ("SYS" "OZ:<L.SYS>")
    ("SYS2" "OZ:<L.SYS2>")
    ("TAPE" "OZ:<L.TAPE>")
    ("UBIN" "OZ:<L.UBIN>")
    ("UCADR" "OZ:<L.UCADR>")
    ("WINDOW" "OZ:<L.WINDOW>")
    ("WIND" "OZ:<L.WIND>")
    ("ZMAIL" "OZ:<L.ZMAIL>")
    ("ZWEI" "OZ:<L.ZWEI>")
    ))

(DEFCONST FC-SYS-PATHNAME-TRANSLATIONS
  '(("CC;" "L\CC;")
    ("CHAOS;" "L\CHAOS;")
    ("DEMO;" "L\DEMO;")
    ("DISTRIBUTION;" "L\DISTRIBUTION;")
    ("DOC;" "L\DOC;")
    ("FILE;" "L\FILE;")
    ("FILE2;" "L\FILE2;")
    ("FONTS;" "L\FONTS;")
    ("IO;" "L\IO;")
    ("IO;FILE;" "L\IO\FILE;")
    ("IO1;" "L\IO1;")
    ("IP;" "L\NETWORK\IP;")
    ("ISPELL;" "L\ISPELL;")
    ("LAMBDA-DIAG;" "L\LAMBDA-DIAG;")
    ("LAMBDA-UCODE;" "L\LAMBDA-UCODE;")
    ("LIB;" "L\LIB;")
    ("MAN;" "L\MAN;")
    ("NETWORK;" "L\NETWORK;")
    ("NETWORK;CHAOS;" "L\NETWORK\CHAOS;")
    ("CHNCP;" "L\NETWORK\CHAOS;")
    ("PATCH;" "L\PATCH;")
    ("PRESS-FONTS;" "L\PRESS-FONTS;")
    ("SITE;" "L\SITE;")
    ("SYS;" "L\SYS;")
    ("SYS2;" "L\SYS2;")
    ("TAPE;" "L\TAPE;")
    ("UBIN;" "L\UBIN;")
    ("UCADR;" "L\UCADR;")
    ("WINDOW;" "L\WINDOW;")
    ("WIND;" "L\WIND;")
    ("ZMAIL;" "L\ZMAIL;")
    ("ZWEI;" "L\ZWEI;")
    ))

(DEFCONST LM-SYS-PATHNAME-TRANSLATIONS
  '(("CC;" "<L.CC>")
    ("CHAOS;" "<L.CHAOS>")
    ("DEMO;" "<L.DEMO>")
    ("DOC;" "<L.DOC>")
    ("DISTRIBUTION;" "<L.DISTRIBUTION>")
    ("FILE;" "<L.FILE>")
    ("FILE2;" "<L.FILE2>")
    ("FONTS;" "<L.FONTS>")
    ("IO;" "<L.IO>")
    ("IO;FILE;" "<L.IO.FILE>")
    ("IO1;" "<L.IO1>")
    ("ISPELL;" "<L.ISPELL>")
    ("LAMBDA-DIAG;" "<L.LAMBDA-DIAG>")
    ("LAMBDA-UCODE;" "<L.LAMBDA-UCODE>")
    ("LIB;" "<L.LIB>")
    ("MAN;" "<L.MAN>")
    ("NETWORK;" "<L.NETWORK>")
    ("NETWORK;CHAOS;" "<L.NETWORK.CHAOS>")
    ("CHNCP;" "<L.NETWORK.CHAOS>")
    ("PATCH;" "<L.PATCH>")
    ("PRESS-FONTS;" "<L.PRESS-FONTS>")
    ("SITE;" "<L.SITE>")
    ("SYS;" "<L.SYS>")
    ("SYS2;" "<L.SYS2>")
    ("TAPE;" "<L.TAPE>")
    ("UBIN;" "<L.UBIN>")
    ("UCADR;" "<L.UCADR>")
    ("WINDOW;" "<L.WINDOW>")
    ("WIND;" "<L.WIND>")
    ("ZMAIL;" "<L.ZMAIL>")
    ("ZWEI;" "<L.ZWEI>")
    ))

(DEFSITE :MIT
  ;; OZ is where the sources are
  (:SYS-HOST "OZ")
  ;; SYS: translations to use
  (:SYS-HOST-TRANSLATION-ALIST '(("FC" . FC-SYS-PATHNAME-TRANSLATIONS)
				 ("OZ" . OZ-SYS-PATHNAME-TRANSLATIONS)
				 ("FS" . LM-SYS-PATHNAME-TRANSLATIONS)
				 ("LM" . LM-SYS-PATHNAME-TRANSLATIONS)
				 (NIL . LM-SYS-PATHNAME-TRANSLATIONS)))
  ;; These two say how to log in to get error tables, etc.
  (:SYS-LOGIN-NAME "LISPM")
  (:SYS-LOGIN-PASSWORD "LISPM")
  ;; Our site-specific system
  (:SITE-SPECIFIC-SYSTEM "MIT-Specific")
  ;; For internet domain addressing, these domains
  ;; are those which the local site belongs to.
  (:LOCAL-INTERNET-DOMAINS '("ARPA"))
; No longer relevant as of system 99.
;  ;; Has a local chaosnet
;  (:CHAOS "MIT")
;  ;; But no (working) ethernet
;  (:ETHER NIL)
  ;; Internet can be reached with Chaos
  (:INTERNET '(("128.31.0.0" :CHAOS "MIT")))
  (:INTERNET-HOST-TABLE "MC: SYSNET; HSTNIC >")
  ;; File computers to go on FS:*PATHNAME-HOST-LIST*.
  ;; The correct name for this site option is :FILE-SERVER-HOSTS as of system 99,
  ;; but the old name is still accepted, and is used here for the sake of earler systems.
  ;; Note that machines not in HSTTBL but known by host table servers
  ;; work EXCEPT when building a new system.
  (:FILE-SERVER-HOSTS '("OZ" "MC"))
  (:CHAOS-FILE-SERVER-HOSTS '("MC" "XX" "OZ" "SPEECH" "EE" "VX" "MATH" "ML"
			      "ALCVAX" "HT" "EDDIE"  "CCC" "CIPG" "DSPG"
			      "VULCAN" "CORWIN" "HTJR" "OBERON" "PFCVAX" "JCF"
			      ;"MIT-MULTICS" Off chaosnet
			      "COGS" "FS" "HOG" "MARIE" "CEZANNE" "DEGAS"
			      "EMS" "HAYDN" "CHARON"
			      "PREP" "PYGMALION" "TALOS" "HEPHAESTUS" "WAIF"
			      ;; What's NU with you ?
			      "NU0" "NU1" "NU2" "NU3" "NU4" "NU5" "NU6" "NU7" "NU8" "NU9"
			      "NU10" "NU11" "NU12" "NU13" "NU14" "NU15" "NU16" "NU17"
			      "SCRC-TENEX" ; not in HSTTBL: "SCRC-COMET" "SCRC-VIXEN"
			      ;; Lisp machines running LMFS -- at MIT
			      ("AP1" :LMFS) ("AP2" :LMFS) ("AP3" :LMFS) ("AP4" :LMFS)
			      ("RB1" :LMFS) ("PI" :LMFS) ("RB2" :LMFS)
			      ("LM15" :LMFS) ("LM23" :LMFS)
			      ("AP5" :LMFS) ("AP6" :LMFS)
			      ("ELVIS" :LMFS) ("JIMI" :LMFS) ("BUDDY" :LMFS) ("JANIS" :LMFS)
			      ("OAK" :LMFS)
			      ;; -- at Symbolics
			      ("POINTER" :LMFS) ; ("NIMBUS" :LMFS) ("HUEY" :LMFS)
			      ("QUABBIN" :LMFS)))
  (:SPECIAL-FILE-HOSTS '((:LMFS ;; Lisp machines running LMFS -- at MIT
			   "AP3" "AP4" "AP5" "AP6" "AP7" "AP8" "RB1" "RB2" "RB3" "RB4" "PI"
			   "LM15" "LM23" "ELVIS" "JIMI" "BUDDY" "JANIS" "OAK" "CROSBY"
			   ;; -- at Symbolics
			   "POINTER" "NIMBUS" "HUEY" "QUABBIN" "RIVERSIDE")))
  ;; Machines running LMFILE that remote hosts should be made for.
  ;; The name here is the name the host will get.
  (:LMFILE-SERVER-HOSTS '("FC"))
  ;; Hosts suspected of supporting time servers
  (:CHAOS-TIME-SERVER-HOSTS '("MC" "OZ" "BYPASS" "PLASMA" "PFCV80" "TOTO" "CHATTER"
			      "LM27" "XX" "EE" "VX" "SCRC" "ALCVAX" "HT" "CCC"))
  ;; Hosts that know about hosts off the chaosnet
  (:CHAOS-HOST-TABLE-SERVER-HOSTS '("MC" "OZ" "XX" "EE" "SCRC-TENEX")) ; "ML"
  ;; Hosts that have mail servers capable of forwarding mail anyplace
  (:CHAOS-MAIL-SERVER-HOSTS '("MC" "XX" "OZ" "EE" "VX")) ; "ML"
  ;; Hosts that will receive mail via the SMTP protocol
  (:CHAOS-SMTP-SERVER-HOSTS '("MC"))
  ;; Hosts that accept mail via the :COMSAT method of sending.
  (:FILE-MAIL-SERVER-HOSTS '("MC" "ML"))
  ;; Hosts that have SPELL servers.
  (:SPELL-SERVER-HOSTS '("OZ" "XX"))
  ;; Prepare for DEC's Brave New World -- per-host default devices
  (:HOST-DEFAULT-DEVICE-ALIST '(("OZ" . "OZ")))
  ;; Access another host's tape drive
  (:DEFAULT-REMOTE-TAPE-PROTOCOL :MAGTAPE)
  ;; Our LISPM host names usually start out as CADR-
  (:USUAL-LM-NAME-PREFIX "CADR-")
  ;; EST
  (:TIMEZONE 5)
  ;; Destination for mail to BUG-FOOBAR
  (:HOST-FOR-BUG-REPORTS "MIT-OZ")
  ;; "Local sites", used by ZMail summary display
  (:LOCAL-MAIL-HOSTS '("MC" "OZ" "ML"))
  ;; Mail can be sent by COMSAT
  (:COMSAT T)
  ;; But that is not the default
  (:DEFAULT-MAIL-MODE ':CHAOS)
  ;; There is a GMSGS server for collecting system messages
  (:GMSGS T)
  ;; Actually, goes to the Internet
  (:ARPA-GATEWAYS '("MC" "ML"))
  ;; Has a Dover
  (:DOVER T)
  ;; It is the default printer.
  (:DEFAULT-PRINTER ':DOVER)
  (:DEFAULT-BIT-ARRAY-PRINTER '(:XGP "MC:.GLPR.;TS7SCN"))
  ;; Interpretation of arguments to ESC F
  (:ESC-F-ARG-ALIST '((NIL . :LOGIN) (1 . :LISP-MACHINES)
		      (2 "OZ") (3 "MC") (4 "EE") (5 "VX") (6 "XX") (7 "SCRC-TENEX")
		      (8 . :ALL-LISP-MACHINES) (9 "PIG" "PREP") (10. "CORWIN" "OBERON")
		      (0 . :READ)))

  (:NON-CHAOS-HOST-TABLE-FILE "SYS: SITE; EXTRA HOSTS")

  ;for common lisp compatibility

  ;;We are an MIT Lisp Machine.  This option obsolete as of system 98, though.
  (:DEFAULT-MACHINE-TYPE "MIT-CADR")

  ;;This is what Steele says we are
  (:SHORT-SITE-NAME "MIT AI Lab")

  ;;This was as long as I could make it.
  (:LONG-SITE-NAME "Massachusetts Institute of Technology, Artificial Intelligence Laboratory")
  )

