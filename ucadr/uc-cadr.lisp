;-*-Mode:Midas-*-
;NOTE: THIS FILE FOLLOWS UC-PARAMETERS AND HAS FIRST I-MEM CODE.

(SETQ UC-CADR '(
;also note: A-LOWEST-DIRECT-VIRTUAL-ADDRESS  holds the lowest direct mapped
; virtual address, normally LOWEST-A-MEM-VIRTUAL-ADDRESS.  But it can be set lower
; ie if you are using the new color TV board you need 128K of direct mapped space
; below that to reference the video buffer.
(ASSIGN LOWEST-A-MEM-VIRTUAL-ADDRESS 176776000)	;MUST BE 0 MODULO SIZE OF A-MEM
(ASSIGN LOWEST-IO-SPACE-VIRTUAL-ADDRESS 177000000)  ;BEGINING OF X-BUS IO SPACE
(ASSIGN LOWEST-UNIBUS-VIRTUAL-ADDRESS 177400000)    ;END OF X-BUS, BEGINNING OF UNIBUS

;Compare with these after clearing the sign bit of the address
;(which is done since that bit is meaningless in the map on a CADR).
(ASSIGN INTERNAL-LOWEST-A-MEM-VIRTUAL-ADDRESS 76776000)    ;MUST BE 0 MODULO SIZE OF A-MEM
(ASSIGN INTERNAL-LOWEST-IO-SPACE-VIRTUAL-ADDRESS 77000000) ;BEGINING OF X-BUS IO SPACE
(ASSIGN INTERNAL-LOWEST-UNIBUS-VIRTUAL-ADDRESS 77400000)   ;END OF X-BUS, BEGINNING OF UNIBUS

(ASSIGN CHAOS-CSR-ADDRESS 77772060)		;UNIBUS 764140
(ASSIGN DISK-REGS-ADDRESS-BASE 77377774)	;XBUS ADDRESS 17377774

(ASSIGN DISK-READ-COMMAND 0)
(ASSIGN DISK-WRITE-COMMAND 11)
(ASSIGN DISK-READ-COMPARE-COMMAND 10)
(ASSIGN DISK-RECALIBRATE-COMMAND 10001005)

(ASSIGN TV-REGS-ADDRESS-BASE 77377760)		;XBUS ADDRESS 17377760
 ;IN REGISTER 0, BIT 3 IS INTERRUPT ENABLE, BIT 4 IS INTERRUPT FLAG
(ASSIGN DISK-RUN-LIGHT-VIRTUAL-ADDRESS 77051763)	;XBUS ADDRESS

(ASSIGN MICROSECOND-CLOCK-HARDWARE-VIRTUAL-ADDRESS 77772050)
(ASSIGN MICROSECOND-CLOCK-PHYSICAL-ADDRESS 77772050)  ;Unibus (764120)

(ASSIGN MOUSE-HARDWARE-VIRTUAL-ADDRESS 77772042)   ;Unibus 764104 Y, 764106 X

(ASSIGN BEEP-HARDWARE-VIRTUAL-ADDRESS 77772044)	   ;Unibus 764110

(ASSIGN INTERRUPT-STATUS-HARDWARE-VIRTUAL-ADDRESS 77773020)
		;Unibus address 766040 (interrupt status)
(ASSIGN CLEAR-INTERRUPT-HARDWARE-VIRTUAL-ADDRESS  77773021) ;Unibus address 766042
(ASSIGN INTERRUPT-CONTROL-HARDWARE-VIRTUAL-ADDRESS 77773020) ;Unibus address 766040

(ASSIGN UNIBUS-MAP-VIRTUAL-BASE-ADDRESS 77773060)	;Base of the unibus map

;(DISPATCH ADVANCE-INSTRUCTION-STREAM) TO GET NEXT HALFWORD
(ASSIGN ADVANCE-INSTRUCTION-STREAM
	(PLUS (PLUS (PLUS DISPATCH-ADVANCE-INSTRUCTION-STREAM 
			  (BYTE-FIELD 1 31.)) ;NEEDFETCH BIT
		    LOCATION-COUNTER)
	      D-ADVANCE-INSTRUCTION-STREAM))


;;; INITIALIZATION

(LOCALITY I-MEM)

ZERO	(JUMP ZERO HALT-CONS)		;WILD TRANSFER TO ZERO

;This is location 1.  Enter here if virtual memory is valid.
BEG	((M-ZERO) SETZ)				;DON'T GET SCREWED BY CLOBBERED LOC 2@A
	(JUMP BEG0000)

;Enter here from the PROM.  Virtual memory is not valid yet.
(LOC 6)
PROM	(JUMP-NOT-EQUAL-XCT-NEXT Q-R A-ZERO PROM)    ;These 2 instructions duplicate the prom
       ((Q-R) ADD Q-R A-MINUS-ONE)
;;; Decide whether to restore virtual memory from saved band on disk, i.e.
;;; whether this is a cold boot or a warm boot.  If the keyboard has input
;;; available, and the character was RETURN (rather than RUBOUT), it's a warm boot.
	(CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) (A-CONSTANT 17772045))		;Unibus address 764112 (KBD CSR)
	(JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 5) MD	;If keyboard is not ready,
		COLD-BOOT)			; assume we are supposed to cold-boot
	(CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) (A-CONSTANT 17772040))		;Unibus address 764100 (KBD LOW)
	((MD) (BYTE-FIELD 6 0) MD)		;Get keycode
	(JUMP-EQUAL MD (A-CONSTANT 46) COLD-BOOT)	;This is cold-boot if key is RUBOUT
	((MD) (A-CONSTANT 46))			;Standardize mode.  Mostly, set to NORMAL speed
	(CALL-XCT-NEXT PHYS-MEM-WRITE)		;40 is PROM-DISABLE, 2 is NORMAL speed.
       ((VMA) (A-CONSTANT 17773005))		;Unibus 766012
	(JUMP BEG0000)


;PUSHJ HERE FOR FATAL ERRORS, E.G. THINGS THAT CAN'T HAPPEN.
;ALSO FOR THINGS WHICH DON'T HAVE ERROR-TABLE ENTRIES YET.
  (MICRO-CODE-ILLEGAL-ENTRY-HERE)	;FILL IN UNUSED ENTRIES IN 
					; MICRO-CODE-SYMBOL-AREA
ILLOP	(POPJ HALT-CONS)		;Halt with place called from in lights


;; (%WRITE-INTERNAL-PROCESSOR-MEMORIES CODE ADR D-HI D-LOW)
;;   CODE SELECTS WHICH MEMORY GETS WRITTEN. 1 -> I, 2 -> D, 4 -> A/M . 
;;    (THIS IS A SUBSET OF THE CODE USED IN MCR FILES).
XWIPM (MISC-INST-ENTRY %WRITE-INTERNAL-PROCESSOR-MEMORIES)
	((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)
	((M-1) DPB C-PDL-BUFFER-POINTER (BYTE-FIELD 10 30) A-1)  ;M-1 GETS 32 BITS DATA
	((M-2) (BYTE-FIELD 20 10) C-PDL-BUFFER-POINTER-POP)      ;M-2 GETS REST BEYOND THAT
	((M-A) Q-POINTER C-PDL-BUFFER-POINTER-POP)		;ADDRESS
	((M-B) Q-POINTER C-PDL-BUFFER-POINTER-POP)		;CODE
	(JUMP-EQUAL M-B (A-CONSTANT 1) XWIPM-I)
	(JUMP-EQUAL M-B (A-CONSTANT 2) XWIPM-D)
	(CALL-NOT-EQUAL M-B (A-CONSTANT 4) TRAP)
   (ERROR-TABLE BAD-INTERNAL-MEMORY-SELECTOR-ARG M-B)
	(JUMP-LESS-THAN M-A (A-CONSTANT 40) XWIPM-M)
	((OA-REG-LOW) DPB M-A OAL-A-DEST A-ZERO)
	((A-GARBAGE) M-1)
	(JUMP XFALSE)

XWIPM-M ((OA-REG-LOW) DPB M-A OAL-M-DEST A-ZERO)
	((M-GARBAGE) M-1)
	(JUMP XFALSE)

XWIPM-D ((OA-REG-LOW) DPB M-A OAL-DISP A-ZERO)
	(DISPATCH A-1 WRITE-DISPATCH-RAM)
	(JUMP XFALSE)

XWIPM-I ((OA-REG-LOW) DPB M-A OAL-JUMP A-ZERO)
	(WRITE-I-MEM A-2 M-1)
	(JUMP XFALSE)

;; Give this an offset into the IO part of the XBUS, not an XBUS address.
XXBR (MISC-INST-ENTRY %XBUS-READ)
	(DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
    (ERROR-TABLE ARG-POPPED 0 PP)
	((VMA-START-READ) ADD C-PDL-BUFFER-POINTER-POP	;XBUS word addr
		(A-CONSTANT LOWEST-IO-SPACE-VIRTUAL-ADDRESS))
XUBR0	(CHECK-PAGE-READ)		;Mustn't check for sequence breaks since
	(JUMP-XCT-NEXT RETURN-M-1)	;on some devices reading has side effects and if
       ((M-1) READ-MEMORY-DATA)		;a sequence break occurred we would read it twice

XUBR (MISC-INST-ENTRY %UNIBUS-READ)
	(DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
    (ERROR-TABLE ARG-POPPED 0 PP)
	((VMA-START-READ) (BYTE-FIELD 17. 1) C-PDL-BUFFER-POINTER-POP	;UBUS word addr
		(A-CONSTANT LOWEST-UNIBUS-VIRTUAL-ADDRESS))
	(JUMP XUBR0)

;; %XBUS-WRITE-SYNC w-loc w-val delay s-loc s-mask s-val
;; Waits for (LOGAND (%XBUS-READ s-loc) s-mask) to not-equal s-val, then
;; to equal s-val.  Then it loops 'delay' number of times and writes
;; w-val into w-loc.  This is intended for such things as color-map hacking.
XXBWS (MISC-INST-ENTRY %XBUS-WRITE-SYNC)
	(CALL GET-32-BITS)		;S-VAL
	((M-2) M-1)
	(CALL GET-32-BITS)		;S-MASK
	((VMA) (BYTE-FIELD 18. 0) C-PDL-BUFFER-POINTER-POP	;S-LOC
		(A-CONSTANT LOWEST-IO-SPACE-VIRTUAL-ADDRESS))
XXBWS1	((VMA-START-READ) VMA)
	(CHECK-PAGE-READ)
	((M-3) AND READ-MEMORY-DATA A-1)
	(JUMP-EQUAL M-3 A-2 XXBWS1)
XXBWS2	((VMA-START-READ) VMA)
	(CHECK-PAGE-READ)
	((M-3) AND READ-MEMORY-DATA A-1)
	(JUMP-NOT-EQUAL M-3 A-2 XXBWS2)
	((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)	;DELAY
XXBWS3	(JUMP-NOT-EQUAL-XCT-NEXT M-1 A-ZERO XXBWS3)
       ((M-1) SUB M-1 (A-CONSTANT 1))
	;drop into XXBW

;; See comments on %XBUS-READ above.
XXBW (MISC-INST-ENTRY %XBUS-WRITE)
	(CALL GET-32-BITS)		;M-1 gets value to write
	(DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
		(ERROR-TABLE ARGTYP FIXNUM PP 0)
	((WRITE-MEMORY-DATA) M-1)
	((VMA-START-WRITE M-T) ADD C-PDL-BUFFER-POINTER-POP	;Return random fixnum in M-T
		(A-CONSTANT LOWEST-IO-SPACE-VIRTUAL-ADDRESS))
	(CHECK-PAGE-WRITE)
	(POPJ)

XUBW (MISC-INST-ENTRY %UNIBUS-WRITE)
	(DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
		(ERROR-TABLE ARGTYP FIXNUM PP 1)
	((M-T WRITE-MEMORY-DATA) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;WORD TO WRITE
;;; IF THIS IS MADE CONTINUABLE, THIS WILL HAVE TO BE FIXED
	(DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
		(ERROR-TABLE ARGTYP FIXNUM PP 0)
	((M-A) (BYTE-FIELD 17. 1) C-PDL-BUFFER-POINTER-POP)	;UBUS WORD ADDR
	((VMA-START-WRITE) ADD M-A (A-CONSTANT LOWEST-UNIBUS-VIRTUAL-ADDRESS))
	(CHECK-PAGE-WRITE)
	(POPJ)


;;; %STORE-CONDITIONAL pointer, old-val, new-val
;;; This is protected against interrupts, provided that the value you
;;; are storing does not point at the EXTRA-PDL, and that the location
;;; is guaranteed never to contain a pointer to old-space (i.e. it
;;; only points to static areas.)  This is always protected against
;;; sequence breaks (other macrocode processes).
XSTACQ (MISC-INST-ENTRY %STORE-CONDITIONAL) ;args are pointer, old-val, new-val
	((M-A) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;new
	((M-B) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;old
	((M-1) Q-DATA-TYPE PDL-TOP)
	(CALL-NOT-EQUAL M-1 (A-CONSTANT (EVAL DTP-LOCATIVE)) TRAP)
    (ERROR-TABLE ARGTYP LOCATIVE PP 0)
;Won't interrupt between reading out the data here
	((VMA-START-READ) C-PDL-BUFFER-POINTER-POP) ;pntr
	(CHECK-PAGE-READ-NO-INTERRUPT)
	(DISPATCH TRANSPORT-READ-WRITE READ-MEMORY-DATA)
	((M-1) Q-TYPED-POINTER READ-MEMORY-DATA)
	(JUMP-NOT-EQUAL M-B A-1 XFALSE)		;Return NIL if old-val was wrong
	((WRITE-MEMORY-DATA-START-WRITE)	;Otherwise, store new-val
		SELECTIVE-DEPOSIT
		READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER A-A)
;and writing the replacement data here
	(CHECK-PAGE-WRITE)
	(POPJ-AFTER-NEXT GC-WRITE-TEST)
       ((M-T) A-V-TRUE)


;;; Read microsecond clock into M-2  (preserve A-TEM1)
READ-MICROSECOND-CLOCK
	((VMA-START-READ) (A-CONSTANT MICROSECOND-CLOCK-HARDWARE-VIRTUAL-ADDRESS))
					;Unibus 764120
	(CHECK-PAGE-READ-NO-INTERRUPT)
	((M-2) READ-MEMORY-DATA)
	((VMA-START-READ) ADD VMA (A-CONSTANT 1))	;Unibus 764122
	(CHECK-PAGE-READ-NO-INTERRUPT)
	(POPJ-AFTER-NEXT
		(M-2) DPB READ-MEMORY-DATA (BYTE-FIELD 20 20) A-2)
       (NO-OP)

;the following two routines should be combined into the above one.  But be careful,
; registers are very touchy.

read-microsecond-clock-into-md
	((VMA-START-READ) (A-CONSTANT MICROSECOND-CLOCK-HARDWARE-VIRTUAL-ADDRESS))
				;Microsecond clock (764120)
	(CHECK-PAGE-READ-NO-INTERRUPT)
	((A-TEM1) READ-MEMORY-DATA)		;Stash low word
	((VMA-START-READ) ADD VMA (A-CONSTANT 1))
	(ILLOP-IF-PAGE-FAULT)			;Map should be set up, don't bash A-TEM1
	((WRITE-MEMORY-DATA) DPB READ-MEMORY-DATA (BYTE-FIELD 20 20) A-TEM1)
	(popj)

(ASSIGN MICROSECOND-CLOCK-UNIBUS-ADDRESS 764120)
;Read the time in microseconds, and put it in A-LAST-USEC-TIME.
READ-USEC-TIME
	((A-TEM2) MD)
	((VMA-START-READ) (A-CONSTANT (PLUS LOWEST-UNIBUS-VIRTUAL-ADDRESS
					    MICROSECOND-CLOCK-UNIBUS-ADDRESS)))
	(CHECK-PAGE-READ-NO-INTERRUPT)
	((A-LAST-USEC-TIME) MD)
	((VMA-START-READ) M+1 VMA)
	(CHECK-PAGE-READ-NO-INTERRUPT)
	(POPJ-AFTER-NEXT
	 (A-LAST-USEC-TIME) DPB MD (BYTE-FIELD 20 20) A-LAST-USEC-TIME)
       ((MD) A-TEM2)

XHALT (MISC-INST-ENTRY %HALT)
	(JUMP HALT-CONS XFALSE)		;CONTINUING RETURNS NIL

))