;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-
;;; Declarations for SYSTEM's initally loaded
;;; ** (c) Copyright 1984 Massachusetts Institute of Technology **

(DEFSYSTEM SYSTEM
  (:NAME "System")
  (:SHORT-NAME "SYS")
  (:PATCHABLE "SYS: PATCH;")
  (:MODULE ALLDEFS ("SYS: SYS2; DEFMAC"	;at 78, latest 80 WIP
		    "SYS: SYS2; LMMAC"	;at 372, latest 389 WIP
		    "SYS: SYS2; STRUCT"	;at 322, latest 322 OK
		    "SYS: SYS2; SETF"	;at 97, latest 97 OK
		    "SYS: SYS; TYPES")	;at 72, latest 72 FAIL - COLD
	   :PACKAGE SI)
  (:COMPONENT-SYSTEMS FONTS		;OK
		      SYSTEM-INTERNALS
		      FORMAT		;OK
		      CHAOS		;ONE FILE LEFT? EASY?
		      COMPILER
		      FILE-SYSTEM
		      QFASL-REL		;OK
		      TIME		;OK
		      TV
		      PEEK		;OK
		      SUPDUP		;OK
		      ZWEI
		      FED		;OK
		      COLOR		;OK
		      EH
		      PRESS		;EASY
		      MATH		;OK
		      HACKS		;OK
		      METER		;OK
		      SRCCOM		;OK
		      CONVERSE		;OK
		      )
  (:COMPILE-LOAD ALLDEFS)
  (:DO-COMPONENTS (:FASLOAD ALLDEFS)))

(DEFSYSTEM SYSTEM-INTERNALS
  (:PACKAGE SYSTEM-INTERNALS)
  (:NICKNAMES "SI")
  (:MODULE DEFS ("SYS: SYS2; PRODEF"	 ;at 48, latest 49 WIP - MINI
		 "SYS: IO; RDDEFS"	 ;at 62, latest 62 OK - MINI
		 "SYS: SYS2; SGDEFS"	 ;at 54, latest 57 WIP
		 "SYS: SYS2; NUMDEF"))	 ;at 11, latest 12 WIP
  (:MODULE METH "SYS: SYS2; METH")	 ;at 63, latest 63 OK
  (:MODULE CLASS "SYS: SYS2; CLASS")	 ;at 99, lastest 99 OK
  (:MODULE MAIN ("SYS: SYS2; ADVISE"	 ;at 37, latest 38 WIP
		 "SYS: SYS2; BAND"	 ;at 44, latest 44 OK
		 "SYS: SYS2; CHARACTER"	 ;at 21, latest 22 WIP - COLD
		 "SYS: SYS; CLPACK"	 ;at 151, latest 153 WIP - COLD
		 "SYS: WINDOW; COLD"	 ;at 129, latest 129 OK - COLD
		 "SYS: SYS2; DEFSEL"	 ;at 70, latest 70 OK - MINI
		 "SYS: IO; DISK"	 ;at 292, latest 292 OK - MINI
		 "SYS: IO; DLEDIT"	 ;at 52, latest 52 OK
		 "SYS: IO; DRIBBL"	 ;at 36, latest 37 WIP
		 "SYS: SYS2; ENCAPS"	 ;at 27, latest 28 WIP
		 "SYS: SYS; EVAL"	 ;at 78, latest 97 WIP - COLD
		 "SYS: SYS2; FLAVOR"	 ;at 280, latest 283 WIP - MINI
		 "SYS: SYS2; GC"	 ;at 169, latest 174 WIP
		 "SYS: SYS; GENRIC"	 ;at 33, latest 33 OK
		 "SYS: COLD; GLOBAL"	 ;at 644, latest 644 OK - COLD
		 "SYS: IO; GRIND"	 ;at 146, latest 146 OK
		 "SYS: SYS2; HASH"	 ;at 87, latest 89 WIP - MINI
		 "SYS: SYS2; HASHFL"	 ;at 29, latest 33 WIP - MINI
		 "SYS: NETWORK; HOST"	 ;at 121, latest 121 OK - MINI
		 "SYS: NETWORK; PACKAGE" ;at 7, latest 7 OK - MINI
		 "SYS: IO1; INC"	 ;at 8, latest 8 OK
		 "SYS: IO1; INFIX"	 ;at 11, latest 11 OK
		 "SYS: SYS2; LOGIN"	 ;at 87, latest 87 OK - MINI
		 "SYS: SYS2; LOOP"	 ;at 829, latest 829 OK
		 "SYS: SYS; LTOP"	 ;at 495, latest 498 WIP - COLD
		 "SYS: SYS2; MAKSYS"	 ;at 180, latest 180 OK
		 "SYS: IO; MINI"	 ;at 88, latest 90 WIP - COLD
		 "SYS: SYS2; NUMER"	 ;at 60, latest 62 WIP - MINI
		 "SYS: SYS2; PATCH"	 ;at 158, latest 167 WIP
		 "SYS: SYS2; PLANE"	 ;at 32, latest 32 OK
		 "SYS: IO; PRINT"	 ;at 177, latest 183 WIP - COLD
		 "SYS: SYS2; PROCES"	 ;at 157, latest 159 WIP - MINI
		 "SYS: IO; QIO"		 ;at 210, latest 217 WIP - COLD
		 "SYS: SYS; QFASL"	 ;at 461, latest 463 WIP - COLD
		 "SYS: SYS; QFCTNS"	 ;at 770, latest 774 WIP - COLD
		 "SYS: SYS; QMISC"	 ;at 655, latest 659 WIP - MINI
		 "SYS: IO1; HARDCOPY"	 ;at 1, latest 1 OK
		 "SYS: SYS; QNEW"	 ;at 20, latest 20 OK
		 "SYS: SYS; QRAND"	 ;at 408, latest 412 WIP - COLD
		 "SYS: SYS2; QTRACE"	 ;at 151, latest 152 WIP
		 "SYS: SYS2; RAT"	 ;at 46, latest 46 OK
		 "SYS: IO; READ"	 ;at 428, latest 437 WIP - COLD
		 "SYS: SYS2; RESOUR"	 ;at 28, latest 31 WIP - MINI
		 "SYS: SYS2; SELEV"	 ;at 23, latest 24 WIP
		 "SYS: IO1; SERIAL"	 ;at 32, latest 32 OK
		 "SYS: SYS; SGFCTN"	 ;at 57, latest 57 OK - COLD
		 "SYS: SYS; SORT"	 ;at 59, latest 59 OK - MINI
		 "SYS: SYS2; STEP"	 ;at 70, latest 72 WIP
		 "SYS: IO; STREAM"	 ;at 108, latest 111 WIP - MINI
		 "SYS: SYS2; STRING"	 ;at 147, latest 147 OK - COLD
		 "SYS: SYS; SYSDCL"	 ;ignoring
		 "SYS: COLD; SYSTEM"	 ;at 106, latest 106 OK - COLD
		 "SYS: SYS2; UNFASL"	 ;at 19, latest 19 OK
		 "SYS: IO; UNIBUS"	 ;at 25, latest 27 WIP
		 "SYS: SYS2; ANALYZE"	 ;at 17, latest 19 WIP
		 "SYS: SYS2; CLMAC"	 ;at 4, latest 4 OK
		 ))
  (:MODULE RDTBL ("SYS: IO; RDTBL"	;at 167, latest 169 WIP - COLD
		  "SYS: IO; CRDTBL"))	;at 34, latest 35 WIP - COLD
  (:MODULE EXPORT ("SYS: COLD; EXPORT")) ;at 31, latest 31 OK
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:FASLOAD RDTBL)
  (:READFILE EXPORT)
  (:COMPILE-LOAD METH (:FASLOAD DEFS))
  (:COMPILE-LOAD CLASS (:FASLOAD DEFS) (:FASLOAD METH)))

(DEFSYSTEM FONTS
  (:PACKAGE FONTS)
  (:PATHNAME-DEFAULT "SYS: FONTS;")
  (:FASLOAD ("TVFONT" "CPTFONTB" "CPTFONT" "BIGFNT" "TINY" "5X5"
	     "MEDFNT" "MEDFNB" "METS" "METSI" "43VXMS"
	     "HL6" "HL7" "HL10" "HL10B" "HL12" "HL12I" "HL12B" "HL12BI"
	     "TR8" "TR8I" "TR8B" "TR10" "TR10I" "TR10B" "TR10BI"
	     "TR12" "TR12B" "TR12I" "TR12BI"
	     "MOUSE" "SEARCH" "TOG" "ABACUS"))
  (:READFILE ("EQUIVALENCE")))

(DEFSYSTEM CHAOS
  (:PACKAGE CHAOS)
  (:PATHNAME-DEFAULT "SYS: IO;")
  (:MODULE NCP ("CHSNCP" 		;at 265, latest 270 WIP - MINI
		"CHUSE"))		;at 14, latest 14 OK - MINI
  (:MODULE AUX "CHSAUX")		;at 366, latest 366 OK
  (:MODULE TEST "SYS: IO1; CHATST")	;at 67, latest 67 OK
  (:MODULE EFTP "SYS: IO1; EFTP")	;at 39, latest 39 OK
  (:COMPILE-LOAD (NCP AUX TEST EFTP))
  (:COMPILE-LOAD (:GENERATE-HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL")))))

;;; New ethernet
(DEFSYSTEM ETHERNET
  (:NAME "Ethernet")
  (:SHORT-NAME "Ether")
  (:PACKAGE ETHERNET)
  (:PATHNAME-DEFAULT "SYS: IO;")
  (:MODULE MAIN ("SIMPLE-ETHER" "ADDR-RES"))
  (:COMPILE-LOAD (MAIN)))

(DEFSYSTEM SITE
  (:PACKAGE SYSTEM-INTERNALS)
  (:MODULE SITE ("SYS: SITE; SITE" "SYS: SITE; LMLOCS"))
  (LOAD-SITE-FILE (:COMPILE SITE))
  (:MODULE HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL"))
		      :PACKAGE CHAOS)
  (LOAD-SITE-FILE (:COMPILE (:GENERATE-HOST-TABLE HOST-TABLE))))

(DEFSYSTEM TIME
  (:PACKAGE TIME)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:COMPILE-LOAD ("TIME" 		;at 110, latest 110 OK
		  "TIMPAR")))		;at 75, latest 75 OK

(DEFSYSTEM SUPDUP
  (:PACKAGE SUPDUP)
  (:COMPILE-LOAD ("SYS: WINDOW; SUPDUP"))) ;at 277, latest 277 OK

(DEFSYSTEM PRESS
  (:PACKAGE PRESS)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:MODULE RFONTW "RFONTW")		;at 82, latest 82 OK
  (:MODULE PRESS "PRESS")		;at 146, latest 147 WIP
  (:MODULE FONTW "PRESS-FONTS; FONTS WIDTHS >")
  (:COMPILE-LOAD RFONTW)
  (:COMPILE-LOAD PRESS)
;;;---!!! How is PRESS-FONTS; FONTS WIDTHS generated?
  ;; (:LOAD-FONTS-WIDTHS FONTW (:FASLOAD RFONTW))
  )

(DEFSYSTEM FORMAT
  (:PACKAGE FORMAT)
  (:COMPILE-LOAD ("SYS: IO; FORMAT"	  ;at 241, latest 241 OK - MINI
		  "SYS: IO; FORMAT-MACRO" ;at 2, latest 2 OK
		  "SYS: IO1; FQUERY"	  ;at 46, latest 46 OK - MINI
		  "SYS: IO1; OUTPUT")))	  ;at 38, latest 38 OK

(DEFSYSTEM QFASL-REL
  (:PACKAGE QFASL-REL)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:COMPILE-LOAD ("RELLD"		;at 10, latest 10 OK
		  "RELDMP")))		;at 12, latest 12 OK

(DEFSYSTEM COMPILER
  (:PACKAGE COMPILER)
  (:MODULE DEFS ("SYS: SYS; MADEFS"	;at 7, latest 7 OK
		 "SYS: SYS; QCDEFS"))	;at 149, latest 153 WIP
  (:MODULE MAIN ("SYS: SYS2; DISASS"	;at 94, latest 94 OK - MINI
		 "SYS: SYS; MA"		;at 305, latest 305 OK
		 "SYS: SYS; MAOPT"	;at 4, latest 4 OK 
		 "SYS: SYS; MC"		;at 354, latest 354 OK
		 "SYS: SYS; MLAP"	;at 51, latest 51 OK 
		 "SYS: SYS; QCFASD"	;at 248, latest 248 OK
		 "SYS: SYS; QCFILE"	;at 321, latest 324 WIP
		 "SYS: SYS; QCP1"	;at 563, latest 573 WIP
		 "SYS: SYS; QCP2"	;at 261, latest 261 OK
		 "SYS: SYS; QCOPT"	;at 137, latest 137 OK
		 "SYS: SYS; QCLUKE"	;at 26, latest 26 OK
		 "SYS: SYS; QCPEEP"	;at 36, latest 36 OK 
		 "SYS: SYS; QCLAP"))	;at 244, latest 244 OK
  (:COMPILE-LOAD DEFS)			
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:READFILE ("SYS: COLD; DEFMIC"	;at 200, latest 200 OK
	      "SYS: COLD; DOCMIC"))	;at 41, latest 41 OK
  (:FASLOAD ("SYS: SYS; UCINIT")))

(DEFSYSTEM COLOR
  (:PACKAGE COLOR)
  (:COMPILE-LOAD ("SYS: WINDOW; COLOR"))) ;at 69, latest 69 OK

(DEFSYSTEM ZWEI
  (:PACKAGE ZWEI)
  (:PATHNAME-DEFAULT "SYS: ZWEI;")
  (:MODULE DEFS ("DEFS"			;at 155, latest 157 WIP
		 "MACROS"))		;at 148, latest 150 WIP
  (:MODULE SCREEN ("SCREEN"))		;at 467, latest 468 WIP
  (:MODULE MAIN ("COMTAB"		;at 317, latest 322 WIP
		 "DISPLA"		;at 157, latest 159 WIP
		 "FOR"			;at 62, latest 62 OK
		 "INDENT"		;at 104, latest 107 WIP
		 "INSERT"		;at 33, latest 35 WIP
		 "METH"			;at 48, latest 49 WIP
		 "PRIMIT"		;at 175, latest 175 OK
		 "NPRIM"		;at 34, latest 34 OK
		 "HISTORY"		;at 16, latest 18 WIP
		 "FONT"			;at 88, latest 88 OK
		 "KBDMAC"		;at 48, latest 48 OK
		 "SEARCH"		;at 86, latest 86 OK
		 "COMA"			;at 102, latest 106 WIP
		 "COMB"			;at 95, latest 96 WIP
		 "COMC"			;at 204, latest 206 WIP
		 "COMD"			;at 169, latest 170 WIP
		 "COME"			;at 133, latest 135 WIP
		 "COMF"			;at 100, latest 103 WIP
		 "COMG"			;at 40, latest 42 WIP
		 "COMH"			;at 13, latest 14 WIP
		 "COMS"			;at 85, latest 86 WIP
		 "DIRED"		;at 303, latest 311 WIP
		 "BDIRED"		;at 41, latest 42 WIP
		 "DOC"			;at 74, latest 77 WIP
		 "FASUPD"		;at 31, latest 31 OK
		 "FILES"		;at 195, latest 198 WIP
		 "HOST"			;at 20, latest 20 OK
		 "ISPELL"		;at 41, latest 41 OK
		 "LPARSE"		;at 31, latest 31 OK
		 "MODES"		;at 138, latest 139 WIP
		 "MOUSE"		;at 96, latest 98 WIP
		 "PATED"		;at 26, latest 33 WIP
		 "PL1MOD"		;at 14, latest 14 OK
		 "POSS"			;at 89, latest 90 WIP
		 "STREAM"		;at 168, latest 168 OK
		 "SECTIO"		;at 267, latest 273 WIP
		 "ZMNEW"		;at 36, latest 36 OK
		 "ZMACS"))		;at 522, latest 522 OK
  (:MODULE ZYMURG ("ZYMURG"))		;at 42, latest 42 OK
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD SCREEN (:FASLOAD DEFS))
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS SCREEN))
  (:COMPILE-LOAD ZYMURG (:FASLOAD DEFS SCREEN MAIN)))


;;; Font editor
(DEFSYSTEM FED
  (:PACKAGE FED)
  (:MODULE DEFS "SYS: IO1; FNTDEF")	; latest 20, at 20
  (:MODULE MAIN ("SYS: IO1; FNTCNV"	; latest 83, at 83 OK
		 "SYS: WINDOW; FED"	; latest 200, at 200 OK
		 ))
  (:READFILE DEFS)
  (:COMPILE-LOAD MAIN (:READFILE DEFS)))

;;; error handler, debugger
(DEFSYSTEM EH
  (:PACKAGE EH)
  (:PATHNAME-DEFAULT "SYS: EH;")
  (:COMPILE-LOAD ("EH"			;at 336, latest 340 WIP - MINI
		  "EHF" 		;at 225, latest 229 WIP - MINI
		  "EHC"			;at 233, latest 236 WIP - MINI
		  "EHW")))		;at 109, latest 109 OK

(DEFSYSTEM TV
  (:PACKAGE TV)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:MODULE DEFS "TVDEFS")		;at 284, latest 286 WIP
  (:MODULE MAIN ("SCRMAN"		;at 165, latest 166 WIP
		 "SHEET"		;at 557, latest 558 WIP
		 "SHWARM"		;at 328, latest 334 WIP
		 "BASWIN"		;at 563, latest 563 OK
		 "WHOLIN"		;at 90, latest 92 WIP
		 "MOUSE"		;at 247, latest 248 WIP
		 "BASSTR"		;at 372, latest 373 WIP
		 "STREAM"		;at 145, latest 145 OK
		 "GRAPHICS"		;at 1, latest 1 OK
		 "MENU"			;at 104, latest 105 WIP
		 "COMETH"		;at 26, latest 26 OK
		 "SYSMEN"		;at 177, latest 178 WIP
		 "SCRED"		;at 111, latest 112 WIP
		 "TYPWIN"		;at 118, latest 118 OK
		 "SCROLL"		;at 176, latest 176 OK
		 "TSCROL"		;at 73, latest 75 WIP
		 "FRAME"		;at 165, latest 165 OK
		 "CHOICE"		;at 116, latest 116 OK
		 "CSRPOS"		;at 9, latest 10 WIP
		 "INSPCT"		;at 154, latest 159 WIP
		 "RH"))			;at 164, latest 164 OK
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM PEEK
  (:PACKAGE TV)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:MODULE MAIN "PEEK")			  ;at 153, latest 153 OK
  (:MODULE CHAOS "PEEKCH" :PACKAGE CHAOS) ;at 31, latest 31 OK
  (:MODULE FILE "PEEKFS" :PACKAGE FS)	  ;at 10, latest 10 OK

  (:COMPILE-LOAD MAIN)
  (:COMPILE-LOAD CHAOS)
  (:COMPILE-LOAD FILE))

(DEFSYSTEM FILE-SYSTEM
  (:PACKAGE FILE-SYSTEM)
  (:NICKNAMES "FS")
  (:PATHNAME-DEFAULT "SYS: IO;")
  (:MODULE BASIC-PATHNAMES ("ACCESS"	       ;at 9, latest 13 WIP - MINI
			    "PATHNM"))	       ;at 531, latest 538 WIP - MINI
  (:MODULE HOST-PATHNAMES ("PATHST"	       ;at 173, latest 181 WIP - MINI
			   "SYS:FILE2;PATHNM"  ;at 162, latest 163 WIP - MINI
			   "SYS:FILE;LMPARS")) ;at 112, latest 113 WIP - MINI
  (:MODULE FILE-IO ("OPEN"		       ;at 175, latest 180 WIP - MINI
		    "BALDIR"))		       ;at 114, latest 114 OK
  (:MODULE CHAOS-FILE-IO ("SYS: IO; QFILE"))   ;at 353, latest 360 WIP - MINI
  (:COMPILE-LOAD BASIC-PATHNAMES)
  (:COMPILE-LOAD HOST-PATHNAMES (:FASLOAD BASIC-PATHNAMES))
  (:COMPILE-LOAD FILE-IO (:FASLOAD BASIC-PATHNAMES))
  (:COMPILE-LOAD CHAOS-FILE-IO (:FASLOAD HOST-PATHNAMES)))

(DEFSYSTEM MATH
  (:PACKAGE MATH)
  (:COMPILE-LOAD ("SYS: SYS2; MATRIX"))) ;at 26, latest 26

;;; Random use programs and demos
(DEFSYSTEM HACKS
  (:PACKAGE HACKS)
  (:PATHNAME-DEFAULT "SYS: DEMO;")
  (:MODULE DEFS "HAKDEF")		;at 14, latest 14 OK
  (:MODULE MAIN ("ABACUS"		;at 20, latest 20 OK
		 "ALARM"		;at 50, latest 50 OK
		 "BEEPS"		;at 8, latest 8 OK
		 "CAFE"			;at 8, latest 8 OK
		 "COLXOR"		;at 52, latest 52 OK
		 "COLORHACK"		;at 7, latest 7 OK
		 "CROCK"		;at 6, latest 6 OK
		 "DC"			;at 4, latest 4 OK
		 "DEUTSC"		;at 34, latest 34 OK
		 "DLWHAK"		;at 37, latest 37 OK
		 "DOCTOR"		;at 10, latest 10 OK
		 "DOCSCR"		;at 6, latest 6 OK
		 "GEB"			;at 27, latest 27 OK
		 "HCEDIT"		;at 27, latest 27 OK
		 "MUNCH"		;at 14, latest 14 OK
		 "OHACKS"		;at 35, latest 35 OK
		 "ORGAN"		;at 18, latest 18 OK
		 "QIX"			;at 3, latest 3 OK
		 "ROTATE"		;at 5, latest 5 OK
		 "ROTCIR"		;at 5, latest 5 OK
		 "WORM"			;at 8, latest 8 OK
		 "WORM-TRAILS"))	;at 8, latest 8 OK
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:FASLOAD ("TVBGAR" "WORMCH")))	;fonts OK

;;; Source compare
(DEFSYSTEM SRCCOM
  (:PACKAGE SRCCOM)
  (:COMPILE-LOAD ("SYS: IO1; SRCCOM")))	;at 37, latest 37 OK

(DEFSYSTEM METER
  (:PACKAGE METER)
  (:COMPILE-LOAD ("SYS: IO1; METER")))	;at 42, latest 42 OK

;;; Interactive message program
(DEFSYSTEM CONVERSE
  (:PACKAGE ZWEI)
  (:COMPILE-LOAD ("SYS: IO1; CONVER")))	;at 147, latest 147 OK

;;; Systems not initially loaded, but done right afterwards
;;; MIT-Specific definition moved to SYS:SITE;MIT-SPECIFIC.SYSTEM

(DEFSYSTEM CADR
  (:NAME "CADR")
  (:PATHNAME-DEFAULT "SYS:CC;")
  (:PATCHABLE "SYS: PATCH;")
  (:INITIAL-STATUS :RELEASED)
  (:NOT-IN-DISK-LABEL)
  (:PACKAGE CADR)
  (:COMPONENT-SYSTEMS CADR-MICRO-ASSEMBLER CADR-DEBUGGER))

(DEFSYSTEM CADR-DEBUGGER
  (:PACKAGE CADR)
  (:NICKNAMES "CC")
  (:PATHNAME-DEFAULT "SYS: CC;")
  (:MODULE DEFS ("SYS: CC; LQFMAC"	;at 17, latest 17 OK
		 "SYS: CC; LCADMC"))	;at 31, latest 31 OK
  (:MODULE MAIN ("SYS: CC; CC"		;at 50, latest 50 OK
		 "SYS: CC; CCGSYL"	;at 6, latest 6 OK
		 "SYS: CC; LCADRD"	;at 95, latest 95 OK
		 "SYS: CC; DIAGS"	;at 159, latest 159 OK
		 "SYS: CC; DMON"	;at 56, latest 57 WIP EASY
		 "SYS: CC; LDBG"	;at 45, latest 45 OK
		 "SYS: CC; ZERO"	;at 15, latest 15 OK
		 "SYS: CC; CADLD"	;at 8, latest 8 OK
		 "SYS: CC; QF"		;at 126, latest 126 OK
		 "SYS: CC; CCWHY"	;at 12, latest 12 OK
		 "SYS: CC; CCDISK"	;at 106, latest 106 OK
		 "SYS: CC; DCHECK"	;at 7, latest 7 OK
		 "SYS: CC; CHPLOC"	;at 5, latest 5 OK
		 "SYS: CC; SALVAG"))	;at 38, latest 38 OK
  (:COMPILE-LOAD DEFS)
  (:READFILE ("SYS: CC; CADREG"))	;at 4, latest 4 OK
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))	

(DEFSYSTEM CADR-MICRO-ASSEMBLER		;OK
  (:PACKAGE MICRO-ASSEMBLER)
  (:NICKNAMES "UA" "MICRO-ASSEMBLER")
  (:MODULE ASS "SYS: SYS; CADRLP")	;at 152, latest 152 OK
  (:MODULE MAIN ("SYS: SYS; CDMP"	;at 52, latest 52 OK
		 "SYS: SYS; QWMCR"	;at 22, latest 22 OK
		 "SYS: IO; FREAD"	;at 30, latest 30 OK
		 "SYS: SYS2; USYMLD"))	;at 188, latest 188 OK
  (:COMPILE-LOAD ASS)
  (:READFILE ("SYS: COLD; QCOM"		;at 579, latest 583 WIP
	      "SYS: COLD; DEFMIC"	;at 200, latest 200 OK
	      "SYS: SYS; CADSYM")	;at 25, latest 25 OK
	     (:FASLOAD ASS))
  (:COMPILE-LOAD MAIN))

(SET-SYSTEM-SOURCE-FILE "UCODE" "SYS: UCADR; UCODE")

(DEFSYSTEM ZMAIL
  (:NAME "ZMail")
  (:PATHNAME-DEFAULT "SYS: ZMAIL;")
  (:SHORT-NAME "ZM")
  (:PATCHABLE "SYS: PATCH;")
  (:NOT-IN-DISK-LABEL)
  (:PACKAGE ZWEI)
  (:MODULE DEFS "DEFS")
  (:MODULE TV ("MULT" "BUTTON") :PACKAGE TV)
  (:MODULE MAIN ("TOP" "MFILES" "MFHOST" "REFER" "LMFILE" "SYS:FILE;ZMAIL"
		 "SYS:ZMAIL;COMNDS" "MAIL" "WINDOW" "FILTER" "PROFIL" TV))
  (:MODULE COMETH "COMETH")
; (:MODULE PARSE "PARSE")
  (:MODULE RFC733 "RFC733")
  (:MODULE LEX733 "LEX733")
  (:MODULE FONTS "NARROW" :PACKAGE FONTS)
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
; (:COMPILE-LOAD PARSE)
  (:COMPILE-LOAD RFC733
;		 (:FASLOAD PARSE)
		 )
; (:RTC-LOAD LEX733)		;Someday
  (:FASLOAD LEX733)
  (:FASLOAD FONTS)
  (:COMPILE-LOAD COMETH (:FASLOAD DEFS MAIN)))

(defsystem unix
  (:name "Unix-Interface")
  (:short-name "unix")
  (:package unix)
  (:pathname-default "sys:unix;")
  (:patchable nil "unix99-patch")
  (:module config ("sys;lam-config") :package si)
  (:module main ("lamtty" "share-chaos" "iomsg"))
  (:compile-load config)
  (:compile-load main (:fasload config)))

;;; Systems defined elsewhere

(LOOP FOR SYSTEM IN '(:FILE-SYSTEM-UTILITIES :LOCAL-FILE :MAGTAPE :FILE-SERVER)
      DO (SET-SYSTEM-SOURCE-FILE SYSTEM "SYS: FILE; FS"))

(LOOP FOR SYSTEM IN '(:LMFILE-SERVER)
      DO (SET-SYSTEM-SOURCE-FILE SYSTEM "SYS: FILE2; SYSTEM"))

(SET-SYSTEM-SOURCE-FILE ':LFS "SYS: FILE2; SYSTEM")

(SET-SYSTEM-SOURCE-FILE 'COLD "SYS: COLD; COLDPK")

;;; These are the files in the cold load
(DEFCONST COLD-LOAD-FILE-LIST
	  '("SYS: FONTS; CPTFON QFASL >"
	    "SYS: SYS; QRAND QFASL >"
	    "SYS: IO; QIO QFASL >"
;	    "SYS: IO; RDTBL QFASL >"	;done specially
;	    "SYS: IO; CRDTBL QFASL >"	;done specially
	    "SYS: IO; READ QFASL >"
	    "SYS: IO; PRINT QFASL >"
	    "SYS: WINDOW; COLD QFASL >"
	    "SYS: SYS; SGFCTN QFASL >"
	    "SYS: SYS; EVAL QFASL >"
	    "SYS: SYS; TYPES QFASL >"
	    "SYS: SYS; LTOP QFASL >"
	    "SYS: SYS; QFASL QFASL >"
	    "SYS: IO; MINI QFASL >"
	    "SYS: SYS; QFCTNS QFASL >"
	    "SYS: SYS2; STRING QFASL >"
	    "SYS: SYS2; CHARACTER QFASL >"
	    "SYS: SYS; CLPACK QFASL >"
	    "SYS: COLD; GLOBAL QFASL >"
	    "SYS: COLD; SYSTEM QFASL >"))

(DEFCONST LAMBDA-COLD-LOAD-FILE-LIST
	  '("SYS: FONTS; CPTFON QFASL"
	    "SYS: SYS; QRAND QFASL >"
	    "SYS: IO; QIO QFASL >"
;	    "SYS: IO; RDTBL QFASL >"	;done specially
	    "SYS: IO; READ QFASL >"
	    "SYS: IO; PRINT QFASL >"
	    "SYS: WINDOW; COLD QFASL >"
	    "SYS: SYS; SGFCTN QFASL >"
	    "SYS: SYS; EVAL QFASL >"
	    "SYS: SYS; TYPES QFASL >"
	    "SYS: SYS; LTOP QFASL >"
	    "SYS: SYS; QFASL QFASL >"
	    "SYS: IO; ETHER-MINI QFASL >"
	    "SYS: SYS; QFCTNS QFASL >"
	    "SYS: SYS2; STRING QFASL >"
	    "SYS: SYS; CLPACK QFASL >"
	    "SYS: COLD; GLOBAL QFASL >"
	    "SYS: COLD; SYSTEM QFASL >"))

;;; These variables are looked at by the cold load generator, which takes
;;; the translated pathnames and dumps out prototype values into the new
;;; world with those strings suitable for use with MINI.
;;; They are then used before this file gets loaded.
(DEFCONST MINI-FILE-ALIST-LIST
	  '(INNER-SYSTEM-FILE-ALIST REST-OF-PATHNAMES-FILE-ALIST
	    ETHERNET-FILE-ALIST SITE-FILE-ALIST HOST-TABLE-FILE-ALIST))

(DEFCONST INNER-SYSTEM-FILE-ALIST
	  '(("SYS: SYS2; DEFSEL QFASL >" "SI")	;By (resource named-structure-invoke)
	    ("SYS: SYS2; RESOUR QFASL >" "SI")	;By FILLARRAY
	    ("SYS: SYS; QMISC QFASL >" "SI")
	    ("SYS: SYS; SORT QFASL >" "SI")	;Needed by FLAVOR
	    ("SYS: IO; FORMAT QFASL >" "FORMAT")	;ditto
	    ("SYS: IO1; FQUERY QFASL >" "FORMAT")	;Needed by everything in sight
	    ("SYS: SYS2; HASH QFASL >" "SI")	;Needed by FLAVOR,PATHNM
	    ("SYS: SYS2; FLAVOR QFASL >" "SI")	;Needed by PROCES
	    ("SYS: SYS2; HASHFL QFASL >" "SI")	;Make flavors really work.
	    ("SYS: SYS2; PRODEF QFASL >" "SI")	;Definitions for PROCES
	    ("SYS: SYS2; PROCES QFASL >" "SI")
	    ("SYS: SYS2; NUMER QFASL >" "SI")	;SI:EXPT-HARD needed by PROCES
	    ("SYS: EH; EH QFASL >" "EH")
	    ("SYS: EH; EHF QFASL >" "EH")
	    ("SYS: EH; EHC QFASL >" "EH")
	    ("SYS: SYS2; DISASS QFASL >" "COMPILER")	;EH calls subroutines in DISASS
	    ("SYS: IO; DISK QFASL >" "SI")
	    ("SYS: SYS2; LOGIN QFASL >" "SI")	;ditto
	    ("SYS: IO; RDDEFS QFASL >" "SI")	;Load this before trying to read any #\'s
	    ("SYS: NETWORK; HOST QFASL >" "SI")
	    ("SYS: NETWORK; PACKAGE QFASL >" "SI")
	    ("SYS: IO; ACCESS QFASL >" "FS")
	    ("SYS: IO; STREAM QFASL >" "SI")	;Probably needed by any file system
	    ;; PATHNM must be the last file in this list.  It breaks things while cold loading
	    ;; that QLD knows how to fix after this alist is loaded.
	    ("SYS: IO; PATHNM QFASL >" "FS")))

(DEFCONST REST-OF-PATHNAMES-FILE-ALIST
	  '(("SYS: IO; PATHST QFASL >" "FS")
	    ("SYS: FILE2; PATHNM QFASL >" "FS")
	    ("SYS: FILE; LMPARS QFASL >" "FS")
	    ("SYS: IO; OPEN QFASL >" "FS")
	    ("SYS: IO; CHSNCP QFASL >" "CHAOS")
	    ("SYS: IO; CHUSE QFASL >" "CHAOS")
	    ("SYS: IO; QFILE QFASL >" "FS")))

(DEFCONST ETHERNET-FILE-ALIST
	  '(("SYS: IO; SIMPLE-ETHER QFASL >" "ETHERNET")
	    ("SYS: IO; ADDR-RES QFASL >" "ETHERNET")))

(DEFCONST SITE-FILE-ALIST
	  '(("SYS: SITE; SITE QFASL >" "SI")))

(DEFCONST HOST-TABLE-FILE-ALIST
	  '(("SYS: SITE; HSTTBL QFASL >" "CHAOS")
	    ("SYS: SITE; LMLOCS QFASL >" "SI")))
