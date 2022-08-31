;;; -*- Mode:LISP; Package:SI; Readtable:ZL; Base:10 -*-
;;; These symbols are put onto the SYSTEM package, which is a subpackage
;;; of GLOBAL and has SYSTEM-INTERNALS and COMPILER as subpackages.
;;; All of the symbols from SYSTEM-CONSTANT-LISTS and SYSTEM-VARIABLE-LISTS
;;; are on it as well.
;;; Also, any symbol in MICRO-CODE-SYMBOL-NAME-AREA that doesn't go on
;;; GLOBAL gets put on SYSTEM.

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCONST INITIAL-SYSTEM-SYMBOLS '(

;;; Be SURE to leave a SPACE before all symbols, because the Maclisp reader b.d.g.
 %BIND
 %1d-aloc					;
 %1d-aref					;brand s ar-1-force
 %1d-aset					;
 *BOOLE
 *BREAK-BINDINGS*
 *LOGAND
 *LOGIOR
 *LOGXOR
 *MAX
 *MIN
 ACTIVE-PROCESSES
 ALL-PROCESSES
 APPLY-LAMBDA
 ASSOC-EQL
 ASSOC-EQUAL
 ASSOC-EQUALP
 CLOCK-FUNCTION-LIST
;COMMAND-LEVEL					;for ABORT key
 CONSTANTS-PAGE
 DECLARED-DEFINITION				;More COMPILER vs SI problems
 DECODE-KEYWORD-ARGLIST
 DEFUN-COMPATIBILITY				;If you expect DEFUN to work
 DELETE-BINDING-FROM-CLOSURE
 DWIMIFY-ARG-PACKAGE
 DWIMIFY-PACKAGE
 DWIMIFY-PACKAGE-0
 EVAL1
 EXTRACT-DECLARATIONS
 FDEFINE-FILE-PATHNAME
 FDEFINITION-LOCATION				;not in GLOBAL; use LOCF
 FILE-LOCAL-DECLARATIONS			;Used by COMPILER and SI
 FIND-POSITION-IN-LIST-EQL
 FIND-POSITION-IN-LIST-EQUAL
 FSYMEVAL-IN-ENVIRONMENT
 FUNCALL-WITH-MAPPING-TABLE-INTERNAL
 FUNCTION-PARENT
 FUNCTION-SPEC-HANDLER
 GET-MACRO-ARG-DESC-POINTER			;These used by compiler.
 HEADER-TYPE-FEF 
 INTERPRETED-DEFINITION
 INSERT-BINDING-IN-CLOSURE
 LAMBDA-MACRO-CALL-P
 LAMBDA-MACRO-EXPAND
 LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL
 LISP-ERROR-HANDLER
 LIST-PRODUCT
 LIST-SUM
 M-EQ
 MACROS-EXPANDED
 MACRO-IN-ENVIRONMENT-P
 MAKE-OBSOLETE
 MEMBER-EQUAL					;same as GLOBAL:MEMBER
 MEMBER-EQUALP
 PAGE-BAND-FLAGS				;Brand S area name; QF expects it to exist.
 QUOTE-EVAL-AT-LOAD-TIME
 RASSOC-EQL
 RASSOC-EQUAL
 RASSOC-EQUALP
 READ-AREA
 RECORD-MACROS-EXPANDED
 RESET-TEMPORARY-AREA
 SCHEDULER-STACK-GROUP
 SELF-BINDING-INSTANCES
 SELF-FLAVOR-DECLARATION
 SELF-REF
 STANDARDIZE-FUNCTION-SPEC
 STORE-KEYWORD-ARG-VALUES
 SYSTEM-CONSTANT
 UNDO-DECLARATIONS-FLAG				;Used by MACRO to communicate with QC-FILE.
 VALIDATE-FUNCTION-SPEC
 WITH-HELP-STREAM
 WITH-SELF-ACCESSIBLE

;;; Declarations

;these two are here so that brand S compatible code will not blow out trying ot export
; from sys. Perhaps they should be supported?
 downward-function
 downward-funarg
;array-register
;array-register-1d

;;; Stuff part of cold-load-stream and used by TV.

 COLD-LOAD-STREAM
 KBD-CONVERT-TO-SOFTWARE-CHAR
 KBD-GET-HARDWARE-CHAR
 KBD-HARDWARE-CHAR-AVAILABLE

;;; FEF hacking.
; see also fef stuff below defined in SYS2; SGDEFS
 FEF-INSTRUCTION
 FEF-INSTRUCTION-LENGTH
 FEF-LIMIT-PC
 FEF-DEBUGGING-INFO
 FEF-DEBUGGING-INFO-PRESENT-P
 FEF-NAME

;;; Shared between LFL which is in COMPILER and stuff in SI
 GET-FILE-LOADED-ID
 SET-FILE-LOADED-ID

;;; Processor code variables
 CADR-TYPE-CODE
 LAMBDA-TYPE-CODE
 PROCESSOR-TYPE-CODE

;;; Addresses of funny parts of virtual memory.
 A-MEMORY-VIRTUAL-ADDRESS
 IO-SPACE-VIRTUAL-ADDRESS
 UNIBUS-VIRTUAL-ADDRESS

;;; "Entries" to DISK
 CLEAR-DISK-FAULT
 DISK-READ
 DISK-READ-COMPARE
 DISK-WRITE
 FIND-DISK-PARTITION
 FIND-DISK-PARTITION-FOR-READ
 FIND-DISK-PARTITION-FOR-WRITE
 GET-DISK-FIXNUM
 GET-DISK-RQB
 GET-DISK-STRING
 MEASURED-SIZE-OF-PARTITION
 PAGE-IN-AREA
 PAGE-IN-ARRAY
 PAGE-IN-PIXEL-ARRAY
 PAGE-IN-REGION
 PAGE-IN-STRUCTURE
 PAGE-IN-WORDS
 PAGE-OUT-AREA
 PAGE-OUT-ARRAY
 PAGE-OUT-PIXEL-ARRAY
 PAGE-OUT-REGION
 PAGE-OUT-STRUCTURE
 PAGE-OUT-WORDS
 PARTITION-COMMENT
 POWER-UP-DISK
 PUT-DISK-FIXNUM
 PUT-DISK-STRING
 RETURN-DISK-RQB
 RQB-8-BIT-BUFFER
 RQB-BUFFER
 RQB-NPAGES
 UPDATE-PARTITION-COMMENT

;;; Macros and functions for (compiler and other sorts of) warnings.
 FILE-OPERATION-WITH-WARNINGS
 OBJECT-OPERATION-WITH-WARNINGS
 RECORD-WARNING
 RECORD-UNDEFINED-WARNING

;;; Symbols defined by SYS2;SGDEFS.  These should be in SYSTEM just like those
;;; symbols defined by QCOM.
 SG-NAME SG-REGULAR-PDL SG-REGULAR-PDL-LIMIT SG-SPECIAL-PDL SG-SPECIAL-PDL-LIMIT
 SG-INITIAL-FUNCTION-INDEX
 SG-UCODE SG-TRAP-TAG SG-RECOVERY-HISTORY SG-FOOTHOLD-DATA
 SG-STATE SG-CURRENT-STATE SG-FOOTHOLD-EXECUTING-FLAG SG-PROCESSING-ERROR-FLAG
 SG-PROCESSING-INTERRUPT-FLAG SG-SAFE SG-INST-DISP SG-IN-SWAPPED-STATE
 SG-SWAP-SV-ON-CALL-OUT SG-SWAP-SV-OF-SG-THAT-CALLS-ME
 SG-PREVIOUS-STACK-GROUP SG-CALLING-ARGS-POINTER SG-CALLING-ARGS-NUMBER
 SG-TRAP-AP-LEVEL SG-REGULAR-PDL-POINTER SG-SPECIAL-PDL-POINTER SG-AP SG-IPMARK
 SG-TRAP-MICRO-PC
;SG-ERROR-HANDLING-SG
;SG-INTERRUPT-HANDLING-SG
 SG-SAVED-QLARYH SG-SAVED-QLARYL SG-SAVED-M-FLAGS SG-FLAGS-QBBFL
 SG-FLAGS-CAR-SYM-MODE SG-FLAGS-CAR-NUM-MODE SG-FLAGS-CDR-SYM-MODE SG-FLAGS-CDR-NUM-MODE
 SG-FLAGS-DONT-SWAP-IN SG-FLAGS-TRAP-ENABLE SG-FLAGS-MAR-MODE SG-FLAGS-PGF-WRITE
 SG-FLAGS-METER-ENABLE SG-FLAGS-TRAP-ON-CALL
 SG-AC-K SG-AC-S SG-AC-J SG-AC-I SG-AC-Q SG-AC-R SG-AC-T SG-AC-E SG-AC-D
 SG-AC-C SG-AC-B SG-AC-A SG-AC-ZR SG-AC-2 SG-AC-1 SG-VMA-M1-M2-TAGS SG-SAVED-VMA SG-PDL-PHASE
 REGULAR-PDL-SG SPECIAL-PDL-SG
 RP-CALL-WORD RP-EXIT-WORD RP-ENTRY-WORD RP-FUNCTION-WORD
 RP-DOWNWARD-CLOSURE-PUSHED RP-ADI-PRESENT RP-DESTINATION RP-DELTA-TO-OPEN-BLOCK
 RP-DELTA-TO-ACTIVE-BLOCK RP-MICRO-STACK-SAVED RP-PC-STATUS RP-BINDING-BLOCK-PUSHED RP-EXIT-PC
 RP-NUMBER-ARGS-SUPPLIED RP-LOCAL-BLOCK-ORIGIN RP-TRAP-ON-EXIT

 FEF-INITIAL-PC FEF-NO-ADL-P FEF-FAST-ARGUMENT-OPTION-P FEF-SPECIALS-BOUND-P
 FEF-LENGTH FEF-FAST-ARGUMENT-OPTION-WORD FEF-BIT-MAP-P FEF-BIT-MAP
 FEF-NUMBER-OF-LOCALS FEF-ADL-ORIGIN FEF-ADL-LENGTH

;;; Standard fundamental error flavors
 WARNING
 PROCEED-WITH-VALUE-MIXIN
 AUTOMATIC-ABORT-DEBUGGER-MIXIN
 NO-ACTION-MIXIN

;;; Standard error flavors.
 ARITHMETIC-ERROR
 BAD-ARRAY-MIXIN
 BAD-CONNECTION-STATE
 CELL-CONTENTS-ERROR
 CONNECTION-ERROR
 END-OF-FILE
 LOCAL-NETWORK-ERROR
 NETWORK-ERROR
 PACKAGE-ERROR
 PACKAGE-NOT-FOUND
 READ-END-OF-FILE
 READ-ERROR
 REMOTE-NETWORK-ERROR
 UNBOUND-VARIABLE

;;; A few signal-names, not advertised, but here for communication with signalers.
 BAD-CONNECTION-STATE-1
 CONNECTION-ERROR-1
 END-OF-FILE-1
 READ-ERROR-1

;;; Standard error condition names.
 ABORT
 AREA-OVERFLOW
 ARRAY-HAS-NO-LEADER
 ARRAY-WRONG-NUMBER-OF-DIMENSIONS
 BAD-ARRAY-ERROR
 BAD-ARRAY-TYPE
 BAD-CDR-CODE
 BAD-DATA-TYPE-IN-MEMORY
 BAD-INTERNAL-MEMORY-SELECTOR-ARG
 BAD-KEYWORD-ARGLIST
 BIGNUM-NOT-BIG-ENOUGH-DPB
 BITBLT-DESTINATION-TOO-SMALL
 BREAK-CONDITION
 BREAKPOINT
 CALL-TRAP
 CONNECTION-CLOSED
 CONNECTION-LOST
 CONNECTION-NO-MORE-DATA
 CONNECTION-REFUSED
 CONS-IN-FIXED-AREA
 CONS-ZERO-SIZE
 DATA-TYPE-SCREWUP
 DISK-ERROR
 DIVIDE-BY-ZERO
 DRAW-OFF-END-OF-SCREEN
 DRAW-ON-UNPREPARED-SHEET
 EXIT-TRAP
 EXTERNAL-SYMBOL-NOT-FOUND
 FAILED-ASSERTION
 FILL-POINTER-NOT-FIXNUM
 FIXNUM-OVERFLOW
 FLOATING-EXPONENT-OVERFLOW
 FLOATING-EXPONENT-UNDERFLOW
 FUNCALL-MACRO
 HOST-NOT-RESPONDING-DURING-CONNECTION
 HOST-STOPPED-RESPONDING
 IALLB-TOO-SMALL
 ILLEGAL-EXPT					;includes what was zero-to-negative-power
 ILLEGAL-INSTRUCTION
 INVALID-FORM
 INVALID-FUNCTION
 INVALID-FUNCTION-SPEC
 INVALID-LAMBDA-LIST
 LOCK-TIMEOUT
 MAR-BREAK
 MICRO-CODE-ENTRY-OUT-OF-RANGE
 MISSING-CLOSEPAREN
 MVR-BAD-NUMBER
 NAME-CONFLICT					;what slime call package-name-conflict
;NEGATIVE-SQRT
 NETWORK-RESOURCES-EXHAUSTED
 NO-MAPPING-TABLE
 NO-MAPPING-TABLE-1
 NO-SERVER-UP
 NUMBER-ARRAY-NOT-ALLOWED
 PACKAGE-ERROR
 PACKAGE-NOT-FOUND
 PACKAGE-NOT-FOUND-1
 PARSE-ERROR
 PARSE-FERROR
 PDL-OVERFLOW
 PRINT-NOT-READABLE
 PRINT-READABLY
 PRINTING-RANDOM-OBJECT
 READ-LIST-END-OF-FILE
 READ-PACKAGE-NOT-FOUND
 READ-STRING-END-OF-FILE
 READ-SYMBOL-END-OF-FILE
 REDEFINITION
 REGION-TABLE-OVERFLOW
 RPLACD-WRONG-REPRESENTATION-TYPE
 SELECT-METHOD-BAD-SUBROUTINE-CALL
 SELECT-METHOD-GARBAGE-IN-SELECT-METHOD-LIST
 SELECTED-METHOD-NOT-FOUND
 SELF-NOT-INSTANCE
 SG-RETURN-UNSAFE
 STACK-FRAME-TOO-LARGE
 STEP-BREAK
 STREAM-CLOSED
 STREAM-INVALID
 SUBSCRIPT-OUT-OF-BOUNDS
;SYMBOL-NAME-CONFLICT
 THROW-EXIT-TRAP
 THROW-TAG-NOT-SEEN
 TOO-FEW-ARGUMENTS
 TOO-MANY-ARGUMENTS
 UNBOUND-CLOSURE-VARIABLE
 UNBOUND-INSTANCE-VARIABLE
 UNBOUND-LOCAL-VARIABLE				;Never signaled, at the present time.
 UNBOUND-SYMBOL
 UNBOUND-VARIABLE
 UNCLAIMED-MESSAGE
 UNDEFINED-FUNCTION
 UNDEFINED-KEYWORD-ARGUMENT
 UNKNOWN-ADDRESS
 UNKNOWN-HOST-NAME
 UNKNOWN-LOCF-REFERENCE
 UNKNOWN-SETF-REFERENCE
 VIRTUAL-MEMORY-OVERFLOW
 WRITE-IN-READ-ONLY
 WRONG-STACK-GROUP-STATE
 WRONG-TYPE-ARGUMENT
 ZERO-ARGS-TO-SELECT-METHOD
 ZERO-LOG
 ZERO-TO-NEGATIVE-POWER				;subsumed by sys:illegal-expt

))
