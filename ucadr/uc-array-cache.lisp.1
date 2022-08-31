

    (ERROR-TABLE RESTART XAR-1-CACHED-1)
XAR-1-CACHED-1 (MISC-INST-ENTRY AR-1-CACHED-1)
	(DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
		  Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 1 XAR-1-CACHED-1)
	((M-Q) Q-POINTER PDL-POP)
    (ERROR-TABLE RESTART XAR-1-CACHED-OOB-1)
	(JUMP-NOT-EQUAL PDL-TOP A-AR-1-ARRAY-POINTER-1 XAR-1-CACHE-INIT-1)
;; This array is the one most recently referenced.
;; Discard array pointer from pdl, at same time get saved array type in M-B.
	((M-B) SETA A-AR-1-ARRAY-HEADER-1 PDL-POP)
	(CALL-GREATER-OR-EQUAL M-Q A-AR-1-ARRAY-LENGTH-1 TRAP)
    (ERROR-TABLE SUBSCRIPT-OOB M-Q M-S XAR-1-CACHED-OOB-1 PP)
;; Reference the array, getting data addr in M-E.
	(DISPATCH-CALL-XCT-NEXT (LISP-BYTE %%ARRAY-TYPE-FIELD) M-B
	 ARRAY-TYPE-REF-DISPATCH)
    (ERROR-TABLE BAD-ARRAY-TYPE M-B)
       ((M-E) A-AR-1-ARRAY-ADDRESS-1)
	(POPJ)

;; This array is not the last one referenced.
;; Decode it, and remember in case it is referenced again.
XAR-1-CACHE-INIT-1
	(CALL-XCT-NEXT ARRAY-DECODE-1-A)
       ((M-A) Q-TYPED-POINTER PDL-POP)
;; Must not record a displaced array, as for them the subscript
;; must be altered to do the access.
	(JUMP-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-B XAR-1-X)
	((A-AR-1-ARRAY-ADDRESS-1) M-E)
	((A-AR-1-ARRAY-POINTER-1) DPB M-A Q-TYPED-POINTER
	 (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
	((A-AR-1-ARRAY-LENGTH-1) M-S)
	(JUMP-XCT-NEXT XAR-1-X)
       ((A-AR-1-ARRAY-HEADER-1) M-B)

    (ERROR-TABLE RESTART XAR-1-CACHED-2)
XAR-1-CACHED-2 (MISC-INST-ENTRY AR-1-CACHED-2)
	(DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
		  Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 1 XAR-1-CACHED-2)
	((M-Q) Q-POINTER PDL-POP)
    (ERROR-TABLE RESTART XAR-1-CACHED-OOB-2)
	(JUMP-NOT-EQUAL PDL-TOP A-AR-1-ARRAY-POINTER-2 XAR-1-CACHE-INIT-2)
;; This array is the one most recently referenced.
;; Discard array pointer from pdl, at same time get saved array type in M-B.
	((M-B) SETA A-AR-1-ARRAY-HEADER-2 PDL-POP)
	(CALL-GREATER-OR-EQUAL M-Q A-AR-1-ARRAY-LENGTH-2 TRAP)
    (ERROR-TABLE SUBSCRIPT-OOB M-Q M-S XAR-1-CACHED-OOB-2 PP)
;; Reference the array, getting data addr in M-E.
	(DISPATCH-CALL-XCT-NEXT (LISP-BYTE %%ARRAY-TYPE-FIELD) M-B
	 ARRAY-TYPE-REF-DISPATCH)
    (ERROR-TABLE BAD-ARRAY-TYPE M-B)
       ((M-E) A-AR-1-ARRAY-ADDRESS-2)
	(POPJ)

;; This array is not the last one referenced.
;; Decode it, and remember in case it is referenced again.
XAR-1-CACHE-INIT-2
	(CALL-XCT-NEXT ARRAY-DECODE-1-A)
       ((M-A) Q-TYPED-POINTER PDL-POP)
;; Must not record a displaced array, as for them the subscript
;; must be altered to do the access.
	(JUMP-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-B XAR-1-X)
	((A-AR-1-ARRAY-ADDRESS-2) M-E)
	((A-AR-1-ARRAY-POINTER-2) DPB M-A Q-TYPED-POINTER
	 (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
	((A-AR-1-ARRAY-LENGTH-2) M-S)
	(JUMP-XCT-NEXT XAR-1-X)
       ((A-AR-1-ARRAY-HEADER-2) M-B)
