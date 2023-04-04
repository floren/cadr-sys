; -*- Mode:LISP; Base:8; Readtable:T -*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Elements in Q-CORRESPONDING-VARIABLE-LIST are symbols whose values in maclisp are lists
;;;   all of whose members are system contants.  These system constants have maclisp values
;;;   and are made to have the identical values in lisp machine lisp.
(DEFCONST Q-CORRESPONDING-VARIABLE-LISTS '(
  AREA-LIST
  Q-CDR-CODES
  Q-DATA-TYPES
  Q-HEADER-TYPES
  Q-LISP-CONSTANTS
  ;RTB-RTB-BITS
  ;RTB-RTS-BITS
  ;RTB-RTO-OPS 
  ;RTB-MISC
  ;RTM-OPS
  ;READTABLE-%%-BITS 
  ARRAY-TYPES
  HEADER-FIELDS
  ARG-DESC-FIELDS
  NUMERIC-ARG-DESC-FIELDS
  FEF-NAME-PRESENT
  FEF-SPECIALNESS
  FEF-ARG-SYNTAX
  FEF-INIT-OPTION
  FEFHI-FIELDS
  FEF-QUOTE-STATUS 
  FEF-FUNCTIONAL
  ARRAY-FIELDS
  ARRAY-LEADER-FIELDS
  ARRAY-MISCS
  Q-REGION-BITS
  SELF-REF-POINTER-FIELDS
  SYSTEM-CONSTANT-LISTS
  SYSTEM-VARIABLE-LISTS
  SCRATCH-PAD-VARIABLES
  FASL-GROUP-FIELDS
  FASL-OPS 
  FASL-TABLE-PARAMETERS
  FASL-CONSTANTS
  FASL-CONSTANT-LISTS
  FEFH-CONSTANTS 
  FEFHI-INDEXES
  STACK-GROUP-HEAD-LEADER-QS
  SG-STATES
  SPECIAL-PDL-LEADER-QS
  REG-PDL-LEADER-QS 
  SG-STATE-FIELDS
  SG-INST-DISPATCHES
  SYSTEM-COMMUNICATION-AREA-QS
  PAGE-HASH-TABLE-FIELDS 
  Q-FIELDS MICRO-STACK-FIELDS
  M-FLAGS-FIELDS
  M-ERROR-SUBSTATUS-FIELDS 
  SPECPDL-FIELDS
  LINEAR-PDL-FIELDS
  LINEAR-PDL-QS
  HARDWARE-MEMORY-SIZES 
  DISK-RQ-LEADER-QS
  DISK-RQ-HWDS
  DISK-HARDWARE-SYMBOLS
  UNIBUS-CHANNEL-QS
  UNIBUS-CSR-BITS
  CHAOS-BUFFER-LEADER-QS
  CHAOS-HARDWARE-SYMBOLS
  INSTANCE-DESCRIPTOR-OFFSETS
  METER-EVENTS
  METER-ENABLES
  ADI-KINDS
  ADI-STORING-OPTIONS
  ADI-FIELDS
  ))

;;; Elements in SYSTEM-CONSTANT-LISTS are symbols whose maclisp and lisp machine
;;; values are lists of symbols which should get system-constant property for the compiler.
;;; Normally should be very close to Q-CORRESPONDING-VARIABLES-LISTS
(DEFCONST SYSTEM-CONSTANT-LISTS '(
  AREA-LIST
  Q-CDR-CODES
  Q-DATA-TYPES
  Q-HEADER-TYPES
  Q-LISP-CONSTANTS
  ;RTB-RTB-BITS
  ;RTB-RTS-BITS
  ;RTB-RTO-OPS
  ;RTB-MISC
  ;RTM-OPS
  ;READTABLE-%%-BITS
  ARRAY-TYPES
  HEADER-FIELDS	;Not HEADER-TYPES
  ARG-DESC-FIELDS
  NUMERIC-ARG-DESC-FIELDS
  FEF-NAME-PRESENT
  FEF-SPECIALNESS
  FEF-ARG-SYNTAX
  FEF-INIT-OPTION
  FEFHI-FIELDS
  FEF-DES-DT
  FEF-QUOTE-STATUS 
  FEF-FUNCTIONAL
  ARRAY-FIELDS
  ARRAY-LEADER-FIELDS
  Q-REGION-BITS
  SELF-REF-POINTER-FIELDS
  ARRAY-MISCS					;ARRAY-MISCS should be flushed someday
  SYSTEM-CONSTANT-LISTS				;Some things look at sublists of these
  SYSTEM-VARIABLE-LISTS	; two
  ;SCRATCH-PAD-VARIABLES
  ;SCRATCH-PAD-POINTERS
  ;SCRATCH-PAD-PARAMETERS
  ;SCRATCH-PAD-TEMPS 
  FASL-GROUP-FIELDS
  FASL-OPS
  FASL-TABLE-PARAMETERS
  FASL-CONSTANTS
  FASL-CONSTANT-LISTS
  FEFH-CONSTANTS
  FEFHI-INDEXES
  STACK-GROUP-HEAD-LEADER-QS
  SG-STATES
  SPECIAL-PDL-LEADER-QS
  REG-PDL-LEADER-QS 
  SG-STATE-FIELDS
  SG-INST-DISPATCHES
  SYSTEM-COMMUNICATION-AREA-QS
  PAGE-HASH-TABLE-FIELDS
  Q-FIELDS MICRO-STACK-FIELDS
  M-FLAGS-FIELDS
  M-ERROR-SUBSTATUS-FIELDS 
  SPECPDL-FIELDS
  LINEAR-PDL-FIELDS
  LINEAR-PDL-QS
  HARDWARE-MEMORY-SIZES 
  DISK-RQ-LEADER-QS
  DISK-RQ-HWDS
  DISK-HARDWARE-SYMBOLS
  UNIBUS-CHANNEL-QS
  UNIBUS-CSR-BITS
  CHAOS-BUFFER-LEADER-QS
  CHAOS-HARDWARE-SYMBOLS
  INSTANCE-DESCRIPTOR-OFFSETS
  METER-EVENTS
  METER-ENABLES
  A-MEMORY-ARRAY-SYMBOLS
  ADI-KINDS
  ADI-STORING-OPTIONS
  ADI-FIELDS
  ))

;;; Like above but get declared SPECIAL rather than SYSTEM-CONSTANT
(DEFCONST SYSTEM-VARIABLE-LISTS '(
  A-MEMORY-LOCATION-NAMES
  M-MEMORY-LOCATION-NAMES
  ))

;;; These get declared SYSTEM-CONSTANT and get their Maclisp values shipped over.
(DEFCONST Q-LISP-CONSTANTS '(
  PAGE-SIZE
  AREA-LIST
  Q-DATA-TYPES
  SIZE-OF-AREA-ARRAYS
  LENGTH-OF-ATOM-HEAD 
  %ADDRESS-SPACE-MAP-BYTE-SIZE
  %ADDRESS-SPACE-QUANTUM-SIZE
  A-MEMORY-VIRTUAL-ADDRESS
  IO-SPACE-VIRTUAL-ADDRESS
  UNIBUS-VIRTUAL-ADDRESS
  ARRAY-ELEMENTS-PER-Q
  ARRAY-BITS-PER-ELEMENT
  %FEF-HEADER-LENGTH
  %LP-CALL-BLOCK-LENGTH 
  %LP-INITIAL-LOCAL-BLOCK-OFFSET
  A-MEMORY-COUNTER-BLOCK-NAMES
  SYSTEM-CONSTANT-LISTS
  SYSTEM-VARIABLE-LISTS
  ))

(DEFCONST HARDWARE-MEMORY-SIZES '(
  SIZE-OF-HARDWARE-CONTROL-MEMORY
  SIZE-OF-HARDWARE-DISPATCH-MEMORY 
  SIZE-OF-HARDWARE-A-MEMORY
  SIZE-OF-HARDWARE-M-MEMORY 
  SIZE-OF-HARDWARE-PDL-BUFFER
  SIZE-OF-HARDWARE-MICRO-STACK 
  SIZE-OF-HARDWARE-LEVEL-1-MAP
  SIZE-OF-HARDWARE-LEVEL-2-MAP 
  SIZE-OF-HARDWARE-UNIBUS-MAP
  ))

;;;; Data on how to set up the initial areas in the cold load.

;;; See also AREA-LIST, which is in QCOM because microassembly refers to it.

;;; These areas are encached in the pdl buffer.
(DEFCONST PDL-BUFFER-AREA-LIST '(
  LINEAR-PDL-AREA				;Main pdl
  PDL-AREA					;Pdls for misc stack groups
  ))

;;; Note that at present all areas up through address-space-map must be wired.
;;; The reason is that when the microcode starts up it straight-maps that
;;; amount of virtual memory, without checking separately for each page.
;;; It would lose big if one of those straight-mapped pages got swapped out.
;;; Exceptions: unused portions of page-table-area and physical-page-data get unwired
(DEFCONST WIRED-AREA-LIST '(
  RESIDENT-SYMBOL-AREA				;No good reason
  SYSTEM-COMMUNICATION-AREA			;For console, micro interrupt, etc.
  SCRATCH-PAD-INIT-AREA				;Load micro code variables upon startup
  MICRO-CODE-SYMBOL-AREA			;No good reason, actually
  REGION-ORIGIN					;Used by page fault handler
  REGION-LENGTH					;Used by page fault handler
  REGION-BITS					;Used by page fault handler
  REGION-FREE-POINTER				;Used by DISK-SAVE, etc.
						; Not likely to be swapped out!
  PAGE-TABLE-AREA				;Used by page fault handler
  PHYSICAL-PAGE-DATA				;Used by page fault handler
  ADDRESS-SPACE-MAP				;Used by page fault handler
  ))

;;; Areas to be set up read only by cold load
(DEFCONST READ-ONLY-AREA-LIST '(
  SCRATCH-PAD-INIT-AREA
  MICRO-CODE-SYMBOL-AREA
  SUPPORT-ENTRY-VECTOR
  CONSTANTS-AREA
  INIT-LIST-AREA
  P-N-STRING
  MICRO-CODE-SYMBOL-NAME-AREA
  MACRO-COMPILED-PROGRAM
  ))

;;; COLD-LOAD-AREA-SIZES is in QCOM, since writing out a microassembly refers to it.

;;; Default region size is 16K
(DEFCONST COLD-LOAD-REGION-SIZES '(
  WORKING-STORAGE-AREA		#o1000000
  MACRO-COMPILED-PROGRAM	#o400000
  P-N-STRING 			#o400000
  NR-SYM			#o200000
  PDL-AREA			#o200000
  PROPERTY-LIST-AREA		#o200000
  PERMANENT-STORAGE-AREA	#o400000
  ))

;;; In the cold-load, areas have only one region, so you can only use one
;;; representation type per area.  These are the list areas, the rest are structure areas.
(DEFCONST LIST-STRUCTURED-AREAS '(
  SYSTEM-COMMUNICATION-AREA
  SCRATCH-PAD-INIT-AREA
  MICRO-CODE-SYMBOL-AREA
  PAGE-TABLE-AREA
  PHYSICAL-PAGE-DATA
  REGION-ORIGIN
  REGION-LENGTH
  REGION-BITS
  REGION-FREE-POINTER
  REGION-GC-POINTER
  REGION-LIST-THREAD
  AREA-NAME
  AREA-REGION-LIST
  AREA-REGION-SIZE
  AREA-REGION-BITS
  AREA-MAXIMUM-SIZE  
  SUPPORT-ENTRY-VECTOR
  CONSTANTS-AREA
  MICRO-CODE-ENTRY-AREA
  MICRO-CODE-ENTRY-NAME-AREA
  MICRO-CODE-ENTRY-ARGS-INFO-AREA
  MICRO-CODE-ENTRY-MAX-PDL-USAGE
  MICRO-CODE-ENTRY-ARGLIST-AREA
  MICRO-CODE-SYMBOL-NAME-AREA
  INIT-LIST-AREA PROPERTY-LIST-AREA
; OBT-TAILS
  ))

;;; not including fixed areas
(DEFCONST STATIC-AREAS '(
  INIT-LIST-AREA
  PERMANENT-STORAGE-AREA
  P-N-STRING CONTROL-TABLES
  NR-SYM
  MACRO-COMPILED-PROGRAM 
  ))

;;; Don't put FUNCTION around the symbols in here -- that means if you
;;; redefine the function the microcode does not get the new definition,
;;; which is not what you normally want.  Saying FUNCTION makes it a couple
;;; microseconds faster to call it.  Not all of these data are actually
;;; used; check the microcode if you want to know.
(DEFCONST SUPPORT-VECTOR-CONTENTS '(
  'PRINT
  'CALL-NAMED-STRUCTURE
  'DEFSTRUCT-DESCRIPTION
  'APPLY-LAMBDA
  'EQUAL
  'PACKAGE
  'EXPT-HARD
  'NUMERIC-ONE-ARGUMENT
  'NUMERIC-TWO-ARGUMENTS
  '"unbound"
  'INSTANCE-HASH-FAILURE
  'INSTANCE-INVOKE-VECTOR
  'EQUALP
  'EQUALP-ARRAY
  ))

;;; Contents of constants page
(DEFCONST CONSTANTS-PAGE '(NIL T 0 1 2 3 4 5 6 7 8 9 10. -1 -2 -3 -4))

(DEFCONST SCRATCH-PAD-VARIABLES '(
  SCRATCH-PAD-POINTERS
  SCRATCH-PAD-PARAMETER-OFFSET 
  SCRATCH-PAD-PARAMETERS
  SCRATCH-PAD-TEMP-OFFSET
  SCRATCH-PAD-TEMPS
  ))

(DEFCONST SCRATCH-PAD-POINTERS '(
  INITIAL-TOP-LEVEL-FUNCTION
  ERROR-HANDLER-STACK-GROUP 
  CURRENT-STACK-GROUP
  INITIAL-STACK-GROUP
  LAST-ARRAY-ELEMENT-ACCESSED
  ))

(DEFCONST SCRATCH-PAD-PARAMETER-OFFSET #o20)

(GLOBAL:WHEN (GLOBAL:> (GLOBAL:LENGTH SCRATCH-PAD-POINTERS) SCRATCH-PAD-PARAMETER-OFFSET) 
  (BARF 'BARF 'SCRACH-PAD-PARAMETER-OFFSET 'BARF))

(DEFCONST SCRATCH-PAD-PARAMETERS '(
  ERROR-TRAP-IN-PROGRESS
  DEFAULT-CONS-AREA 
  BIND-CONS-AREA
  LAST-ARRAY-ACCESSED-TYPE
  LAST-ARRAY-ACCESSED-INDEX 
  INVOKE-MODE
  INVISIBLE-MODE 
  CDR-ATOM-MODE
  CAR-ATOM-MODE
  ACTIVE-MICRO-CODE-ENTRIES
  ))

(DEFCONST SCRATCH-PAD-TEMP-OFFSET #o20)

(GLOBAL:WHEN (GLOBAL:> (GLOBAL:LENGTH SCRATCH-PAD-PARAMETERS) SCRATCH-PAD-TEMP-OFFSET)
  (BARF 'BARF 'SCRATCH-PAD-TEMP-OFFSET 'BARF))

(DEFCONST SCRATCH-PAD-TEMPS '(
  LAST-INSTRUCTION
  TEMP-TRAP-CODE
  LOCAL-BLOCK-OFFSET 
  SCRATCH-/#-ARGS-LOADED
  TEMP-PC
  SPECIALS-IN-LAST-BLOCK-SLOW-ENTERED
  ))

;;; The documentation that used to be here has been moved to LMDOC;FASLD >

(GLOBAL:PROCLAIM '(GLOBAL:SPECIAL FASL-TABLE FASL-GROUP-LENGTH FASL-GROUP-FLAG FASL-RETURN-FLAG))

(DEFCONST FASL-GROUP-FIELD-VALUES '(
  %FASL-GROUP-CHECK		#o100000 
  %FASL-GROUP-FLAG		#o40000
  %FASL-GROUP-LENGTH		#o37700
  FASL-GROUP-LENGTH-SHIFT	-6
  %FASL-GROUP-TYPE		#o77
  %%FASL-GROUP-CHECK		#o2001
  %%FASL-GROUP-FLAG		#o1701
  %%FASL-GROUP-LENGTH		#o0610
  %%FASL-GROUP-TYPE		#o0006
  ))

(DEFCONST FASL-GROUP-FIELDS (GET-ALTERNATE FASL-GROUP-FIELD-VALUES))
(ASSIGN-ALTERNATE FASL-GROUP-FIELD-VALUES)

(DEFCONST FASL-OPS '(
  FASL-OP-ERR
  FASL-OP-NOOP
  FASL-OP-INDEX
  FASL-OP-SYMBOL
  FASL-OP-LIST
  FASL-OP-TEMP-LIST
  FASL-OP-FIXED
  FASL-OP-FLOAT
  FASL-OP-ARRAY
  FASL-OP-EVAL
  FASL-OP-MOVE 
  FASL-OP-FRAME
  FASL-OP-LIST-COMPONENT
  FASL-OP-ARRAY-PUSH
  FASL-OP-STOREIN-SYMBOL-VALUE 
  FASL-OP-STOREIN-FUNCTION-CELL
  FASL-OP-STOREIN-PROPERTY-CELL 
  FASL-OP-FETCH-SYMBOL-VALUE
  FASL-OP-FETCH-FUNCTION-CELL 
  FASL-OP-FETCH-PROPERTY-CELL
  FASL-OP-APPLY
  FASL-OP-END-OF-WHACK 
  FASL-OP-END-OF-FILE
  FASL-OP-SOAK
  FASL-OP-FUNCTION-HEADER
  FASL-OP-FUNCTION-END 
  FASL-OP-NULL-ARRAY-ELEMENT
  FASL-OP-NEW-FLOAT
  FASL-OP-UNUSED10 
  FASL-OP-UNUSED11
  FASL-OP-UNUSED12
  FASL-OP-QUOTE-POINTER
  FASL-OP-S-V-CELL 
  FASL-OP-FUNCELL
  FASL-OP-CONST-PAGE
  FASL-OP-SET-PARAMETER
  FASL-OP-INITIALIZE-ARRAY 
  FASL-OP-CHARACTER
  FASL-OP-UNUSED1
  FASL-OP-UNUSED2 
  FASL-OP-UNUSED3
  FASL-OP-UNUSED4
  FASL-OP-UNUSED5  
  FASL-OP-UNUSED6
  FASL-OP-STRING
  FASL-OP-STOREIN-ARRAY-LEADER 
  FASL-OP-INITIALIZE-NUMERIC-ARRAY
  FASL-OP-REMOTE-VARIABLE
  FASL-OP-PACKAGE-SYMBOL
  FASL-OP-EVAL1
  FASL-OP-FILE-PROPERTY-LIST
  FASL-OP-REL-FILE
  FASL-OP-RATIONAL
  FASL-OP-COMPLEX
  FASL-OP-LARGE-INDEX
  FASL-OP-STOREIN-SYMBOL-CELL
  ))
(ASSIGN-VALUES FASL-OPS 0)

(DEFCONST FASL-TABLE-PARAMETERS '(
  FASL-NIL
  FASL-EVALED-VALUE
  FASL-TEM1
  FASL-TEM2
  FASL-TEM3 
  FASL-SYMBOL-HEAD-AREA 
  FASL-SYMBOL-STRING-AREA 
  FASL-OBARRAY-POINTER
  FASL-ARRAY-AREA 
  FASL-FRAME-AREA
  FASL-LIST-AREA
  FASL-TEMP-LIST-AREA 
  FASL-UNUSED
  FASL-UNUSED2
  FASL-UNUSED3 
  FASL-UNUSED6
  FASL-UNUSED4
  FASL-UNUSED5
  ))
(ASSIGN-VALUES FASL-TABLE-PARAMETERS 0)

(DEFCONST FASL-CONSTANTS '(
  LENGTH-OF-FASL-TABLE
  FASL-TABLE-WORKING-OFFSET
  ))

(DEFCONST FASL-CONSTANT-LISTS '(
  FASL-GROUP-FIELDS
  FASL-OPS
  FASL-TABLE-PARAMETERS
  FASL-CONSTANTS
  ))

(DEFCONST FASL-TABLE-WORKING-OFFSET #o40)

(GLOBAL:COND ((GLOBAL:> (GLOBAL:LENGTH FASL-TABLE-PARAMETERS) FASL-TABLE-WORKING-OFFSET)
	      (IOC V)
	      (GLOBAL:PRINT 'FASL-TABLE-PARAMETER-OVERFLOW)))

;;; People call this you know, don't go randomly deleting it!
(DEFUN FASL-ASSIGN-VARIABLE-VALUES ()
  ())  ;I guess what this used to do is done at top level in this file