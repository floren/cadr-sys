;;; -*- Mode:Lisp; Readtable:T; Package:CADR; Base:8.; Patch-File:T -*-
;;; Patch file for CADR version 4.3
;;; Reason:
;;;  Ucode image ENTRY-POINTS-ARRAY length increased to 2000 (octal) to reflect
;;;  increase in length of MICROCODE-SYMBOL-AREA.
;;;  READ-SYM-FILE rewritten to fix bit decay of old DO loops and make code
;;;  comprehensible by mortals.
;;;  UA-DO-DEFMIC fixed in various ways including replacement of usage of STORE
;;;  on areas.
;;; Written 22-Oct-85 14:31:45 by MUSE,
;;; while running on Sarah Bernhardt from band 5
;;; with System 99.27, CADR 4.2, Experimental ZMail 54.4, MIT-Specific 23.0, Experimental Vorpal 17.0, Experimental CAM 10.0, Experimental Camera 12.0, microcode 320, Choreography by Muse.



; From file OZ:KANSAS:<L.SYS2>USYMLD.LISP.187 at 22-Oct-85 14:31:47
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; USYMLD  "

(DEFSTRUCT (UCODE-IMAGE :ARRAY :NAMED (:ALTERANT NIL))
   UCODE-IMAGE-VERSION			;version # of this microcode
   UCODE-IMAGE-MODULE-POINTS		;List of ucode-module structures, "most recent" first
					; these give modules that were loaded and the state
					; of load after each so that it is possible to unload
					; a module, etc. (In push-down fashion. All modules
					; loaded since that module must also be unloaded, etc)
   UCODE-IMAGE-MODULE-LOADED		;A tail of ucode-image-module-points, which is
					;  the list of modules actually loaded now.
   UCODE-IMAGE-TABLE-LOADED		;The concentationation of the ucode-tables for
					;  the modules loaded.
   UCODE-IMAGE-ASSEMBLER-STATE		;Assembler state after main ASSEMBLY
   (UCODE-IMAGE-CONTROL-MEMORY-ARRAY	;Data as loaded into control memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-CONTROL-MEMORY));size-of-hardware-control-memory
   (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY	;Data as loaded into dispatch memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-DISPATCH-MEMORY))
   (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE ;1 -> this a-mem location part of ucode-image
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-A-MEMORY ':TYPE 'ART-1B))
   (UCODE-IMAGE-A-MEMORY-ARRAY		;data as loaded into a-memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-A-MEMORY))
   (UCODE-IMAGE-ENTRY-POINTS-ARRAY	;Image of the stuff that normally gets main memory
    (MAKE-ARRAY 2000 ':LEADER-LIST '(577)));with fill-pointer
					; First 600 locs are entries for misc 
					;  insts 200-777.
					; Next 200 are for micro-code-entries 
					;  (specified via micro-code-entry pseudo in CONSLP)
					; Rest are entry points to microcompiled fctns.
   (UCODE-IMAGE-SYMBOL-ARRAY		;CONSLP symbols. Alternating symbol, type, value
    (MAKE-ARRAY 3000 ':FILL-POINTER 0))
   )

))

; From file OZ:KANSAS:<L.SYS2>USYMLD.LISP.187 at 22-Oct-85 14:33:40
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; USYMLD  "

(DEFUN READ-SYM-FILE (VERSION &OPTIONAL
		      (IMAGE CURRENT-UCODE-IMAGE) &AUX
		      (FILENAME (IF (NUMBERP VERSION)
				    (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
					  :NEW-TYPE-AND-VERSION "SYM" VERSION)
				  VERSION)))
  (WITH-OPEN-FILE (STREAM FILENAME :DIRECTION :INPUT)
    (SYM-HEADER-DISPATCH IMAGE STREAM)))

(DEFUN READ-OCTAL (STREAM &AUX (*READ-BASE* 8.)) (READ STREAM))

(DEFUN SYM-HEADER-DISPATCH (IMAGE STREAM &OPTIONAL ITEM)
  ;; Chug along till we get to a header...
  (UNLESS ITEM
    (LOOP DO (SETQ ITEM (READ-OCTAL STREAM))
	  UNTIL (MINUSP ITEM)))
  (SELECTQ ITEM
    (-1 NIL) ;; End of file.
    (-2 (SETUP-SYMBOL-ARRAY IMAGE STREAM))
    (-4 (UCODE-IMAGE-STORE-ASSEMBLER-STATE (READ-OCTAL STREAM) IMAGE)
	(SYM-HEADER-DISPATCH IMAGE STREAM))
    (OTHERWISE (FERROR NIL "~O is not a valid block item" ITEM))))

(DEFUN SETUP-SYMBOL-ARRAY (IMAGE STREAM)
  (LOOP WITH SYM-ARRAY = (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)
	INITIALLY (SETF (FILL-POINTER SYM-ARRAY) 0)
	FOR SYM = (READ-OCTAL STREAM)
	UNTIL (AND (NUMBERP SYM) (MINUSP SYM)) ;; Next header.
	FOR TYPE = (READ-OCTAL STREAM)
	FOR VAL = (READ-OCTAL STREAM)
	DO
	(VECTOR-PUSH-EXTEND SYM SYM-ARRAY 1000)
	(VECTOR-PUSH-EXTEND TYPE SYM-ARRAY 1000)
	(VECTOR-PUSH-EXTEND VAL SYM-ARRAY 1000)
	FINALLY (SYM-HEADER-DISPATCH IMAGE STREAM SYM)))

))

; From file OZ:KANSAS:<L.SYS2>USYMLD.LISP.187 at 22-Oct-85 14:34:46
#8R MICRO-ASSEMBLER#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS2; USYMLD  "

(DEFUN UA-DO-DEFMIC (NAME OPCODE ARGLIST LISP-FUNCTION-P NO-QINTCMP 
  		  &AUX FUNCTION-NAME INSTRUCTION-NAME MICRO-CODE-ENTRY-INDEX NARGS)
  (IF (ATOM NAME)
      (SETQ FUNCTION-NAME NAME INSTRUCTION-NAME NAME)
    (SETQ FUNCTION-NAME (CAR NAME) INSTRUCTION-NAME (CDR NAME)))
  (IF (NULL OPCODE)
      (SETQ OPCODE (OR (GET INSTRUCTION-NAME 'QLVAL) (UA-ASSIGN-MICRO-ENTRY NAME))))
  (PUTPROP INSTRUCTION-NAME OPCODE 'QLVAL)
  (SETQ NARGS (SI:ARGS-INFO-FROM-LAMBDA-LIST ARGLIST))
  (WHEN (OR (BIT-TEST NARGS %ARG-DESC-QUOTED-REST)
	    (BIT-TEST NARGS %ARG-DESC-EVALED-REST)
	    (BIT-TEST NARGS %ARG-DESC-INTERPRETED)
	    (BIT-TEST NARGS %ARG-DESC-FEF-QUOTE-HAIR)
	    (AND (NOT NO-QINTCMP)
		 (NOT (= (LDB %%ARG-DESC-MAX-ARGS NARGS)
			 (LDB %%ARG-DESC-MIN-ARGS NARGS)))))
    (FERROR NIL "~%The arglist of the function ~s, ~s, is too hairy to microcompile.
ARGS-INFO = ~O~%"
	    NAME ARGLIST NARGS))
  (WHEN LISP-FUNCTION-P
    (SETQ MICRO-CODE-ENTRY-INDEX (ALLOCATE-MICRO-CODE-ENTRY-SLOT FUNCTION-NAME))
    (SETF (SYSTEM:MICRO-CODE-ENTRY-ARGLIST-AREA MICRO-CODE-ENTRY-INDEX) ARGLIST)
    (SETF (SYSTEM:MICRO-CODE-ENTRY-ARGS-INFO-AREA MICRO-CODE-ENTRY-INDEX) NARGS))
  (UNLESS NO-QINTCMP
    (PUTPROP INSTRUCTION-NAME (LDB %%ARG-DESC-MAX-ARGS NARGS) 'QINTCMP)
    (UNLESS (EQ FUNCTION-NAME INSTRUCTION-NAME)
      (PUTPROP FUNCTION-NAME (LDB %%ARG-DESC-MAX-ARGS NARGS) 'QINTCMP))))

))
