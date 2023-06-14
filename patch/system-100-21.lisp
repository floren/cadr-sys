;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 100.21
;;; Reason:
;;;  ZWEI: C-q is now bound to "Quoted Insert."  C-\ is bound to "Just One Space".
;;; Written 14-Jun-23 16:06:27 by ams,
;;; while running on Lisp Machine One from band 2
;;; with Experimental System 100.13, Hacks by AMS 2.0, microcode 323, WIP.



; From file FC: /sys/zwei/comtab.lisp at 14-Jun-23 16:06:27
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "FC: //sys//zwei//comtab"

(DEFUN INITIALIZE-STANDARD-COMTABS ()
  (SETQ *STANDARD-COMTAB*
         (SET-COMTAB 'STANDARD-COMTAB
	  '((0 #o200) COM-ORDINARILY-SELF-INSERT
	    #/BS COM-ORDINARILY-SELF-INSERT
	    #/C-F COM-FORWARD
	    #/C-B COM-BACKWARD
	    #/C-N COM-DOWN-REAL-LINE
	    #/C-P COM-UP-REAL-LINE
	    #/C-V COM-NEXT-SCREEN
	    #/M-V COM-PREVIOUS-SCREEN
	    #/C-M-V COM-SCROLL-OTHER-WINDOW
	    #/C-A COM-BEGINNING-OF-LINE
	    #/C-E COM-END-OF-LINE
	    #/M-R COM-MOVE-TO-SCREEN-EDGE
	    #/M-< COM-GOTO-BEGINNING
	    #/M-> COM-GOTO-END
	    #/C-SP COM-SET-POP-MARK
	    #/C-@ COM-SET-POP-MARK
	    #/M-SP COM-PUSH-POP-POINT-EXPLICIT
	    #/C-M-SP COM-MOVE-TO-PREVIOUS-POINT
	    #/CR COM-INSERT-CRS
	    #/C-O COM-MAKE-ROOM
	    #/C-M-O COM-SPLIT-LINE
	    #/M-O COM-THIS-INDENTATION
	    #/M-^ COM-DELETE-INDENTATION
	    #/C-M-^ COM-DELETE-INDENTATION
	    #/C-D COM-DELETE-FORWARD
	    #/RUBOUT COM-RUBOUT
	    #/C-RUBOUT COM-TAB-HACKING-RUBOUT
	    #/C-K COM-KILL-LINE
	    #/CLEAR COM-CLEAR
	    #/BREAK COM-BREAK
	    #/M-W COM-SAVE-REGION
	    #/C-W COM-KILL-REGION
	    #/C-M-W COM-APPEND-NEXT-KILL
	    #/C-Y COM-YANK
	    #/M-Y COM-YANK-POP
	    #/C-L COM-RECENTER-WINDOW
	    #/FF COM-COMPLETE-REDISPLAY
	    #/C-M-! COM-COMPLETE-REDISPLAY
	    #/C-U COM-UNIVERSAL-ARGUMENT
	    #/C-- COM-NEGATE-NUMERIC-ARG
	    #/M-- COM-NEGATE-NUMERIC-ARG
	    #/M-C-- COM-NEGATE-NUMERIC-ARG
	    #/S-- COM-NEGATE-NUMERIC-ARG
	    #/S-C-- COM-NEGATE-NUMERIC-ARG
	    #/S-M-- COM-NEGATE-NUMERIC-ARG
	    #/S-M-C-- COM-NEGATE-NUMERIC-ARG
	    #/H-- COM-NEGATE-NUMERIC-ARG
	    #/C-- COM-NEGATE-NUMERIC-ARG
	    #/H-M-- COM-NEGATE-NUMERIC-ARG
	    #/H-M-C-- COM-NEGATE-NUMERIC-ARG
	    #/H-S-- COM-NEGATE-NUMERIC-ARG
	    #/H-S-C-- COM-NEGATE-NUMERIC-ARG
	    #/H-S-M-- COM-NEGATE-NUMERIC-ARG
	    #/H-S-M-C-- COM-NEGATE-NUMERIC-ARG
	    (#/C-0 10.) COM-NUMBERS
	    (#/M-0 10.) COM-NUMBERS
	    (#/C-M-0 10.) COM-NUMBERS
	    (#/S-0 10.) COM-NUMBERS
	    (#/S-C-0 10.) COM-NUMBERS
	    (#/S-M-0 10.) COM-NUMBERS
	    (#/S-M-C-0 10.) COM-NUMBERS
	    (#/H-0 10.) COM-NUMBERS
	    (#/H-C-0 10.) COM-NUMBERS
	    (#/H-M-0 10.) COM-NUMBERS
	    (#/H-M-C-0 10.) COM-NUMBERS
	    (#/H-S-0 10.) COM-NUMBERS
	    (#/H-S-C-0 10.) COM-NUMBERS
	    (#/H-S-M-0 10.) COM-NUMBERS
	    (#/H-S-M-C-0 10.) COM-NUMBERS
	    #/C-T COM-EXCHANGE-CHARACTERS
	    #/M-T COM-EXCHANGE-WORDS
	    #/C-M-T COM-EXCHANGE-SEXPS
	    #/M-F COM-FORWARD-WORD
	    #/M-B COM-BACKWARD-WORD
	    #/M-K COM-KILL-SENTENCE
	    #/M-D COM-KILL-WORD
	    #/M-RUBOUT COM-BACKWARD-KILL-WORD
	    #/M-@ COM-MARK-WORD
	    #/C-M-F COM-FORWARD-SEXP
	    #/C-M-N COM-FORWARD-LIST
	    #/C-M-B COM-BACKWARD-SEXP
	    #/C-M-P COM-BACKWARD-LIST
	    #/C-M-K COM-KILL-SEXP
	    #/C-M-RUBOUT COM-BACKWARD-KILL-SEXP
	    #/C-M-@ COM-MARK-SEXP
	    #/C-M-/) COM-FORWARD-UP-LIST
	    #/C-M-/( COM-BACKWARD-UP-LIST
	    #/C-M-U COM-BACKWARD-UP-LIST
	    #/C-M-[ COM-BEGINNING-OF-DEFUN
	    #/C-M-] COM-END-OF-DEFUN
	    #/C-M-A COM-BEGINNING-OF-DEFUN
	    #/C-M-E COM-END-OF-DEFUN
	    #/C-M-D COM-DOWN-LIST
	    #/C-/( COM-FIND-UNBALANCED-PARENTHESES
	    #/C-/) COM-SHOW-LIST-START
	    #/M-/( COM-MAKE-/(/)
	    #/M-/) COM-MOVE-OVER-/)
	    #/M-] COM-FORWARD-PARAGRAPH
	    #/M-[ COM-BACKWARD-PARAGRAPH
	    #/M-H COM-MARK-PARAGRAPH
	    #/M-E COM-FORWARD-SENTENCE
	    #/M-A COM-BACKWARD-SENTENCE
	    #/C-G COM-BEEP
	    #/TAB COM-INSERT-TAB
	    #/C-M-TAB COM-INDENT-FOR-LISP
	    #/C-TAB COM-INDENT-DIFFERENTLY
	    #/LF COM-INDENT-NEW-LINE
	    #/C-M-Q COM-INDENT-SEXP
	    #/C-/; COM-INDENT-FOR-COMMENT
	    #/M-/; COM-INDENT-FOR-COMMENT
	    #/C-M-/; COM-KILL-COMMENT
	    #/M-N COM-DOWN-COMMENT-LINE
	    #/M-P COM-UP-COMMENT-LINE
	    #/M-Q COM-FILL-PARAGRAPH
	    #/M-G COM-FILL-REGION
	    #/C-/\ COM-JUST-ONE-SPACE
	    #/M-/\ COM-DELETE-HORIZONTAL-SPACE
	    #/M-CR COM-BACK-TO-INDENTATION
	    #/M-M COM-BACK-TO-INDENTATION
	    #/C-M-CR COM-BACK-TO-INDENTATION
	    #/C-M-M COM-BACK-TO-INDENTATION
	    #/M-U COM-UPPERCASE-WORD
	    #/M-L COM-LOWERCASE-WORD
	    #/M-C COM-UPPERCASE-INITIAL
	    #/C-M-/\ COM-INDENT-REGION
	    #/M-FF COM-INSERT-FF
	    #/M-TAB COM-INSERT-TAB
	    #/M-S COM-CENTER-LINE
	    #/M-= COM-COUNT-LINES-REGION
	    #/C-= COM-FAST-WHERE-AM-I
	    #/C-S COM-INCREMENTAL-SEARCH
	    #/C-R COM-REVERSE-INCREMENTAL-SEARCH
	    #/M- COM-EVALUATE-MINI-BUFFER
	    #/C- COM-COMPILE-REGION
	    #/C-SH-C COM-COMPILE-REGION
	    #/C-SH-E COM-EVALUATE-REGION
	    #/M-SH-E COM-EVALUATE-REGION-VERBOSE
	    #/C-M-SH-E COM-EVALUATE-REGION-HACK
;	    #/C-? COM-SELF-DOCUMENT
	    #/M-? COM-SELF-DOCUMENT
	    #/C-M-? COM-DOCUMENTATION
	    #/HELP COM-DOCUMENTATION
	    #/C-HELP COM-DOCUMENTATION
	    #/C-Q COM-QUOTED-INSERT
	    #/M-X COM-EXTENDED-COMMAND
	    #/C-M-X COM-ANY-EXTENDED-COMMAND
	    #/C-< COM-MARK-BEGINNING
	    #/C-> COM-MARK-END
	    #/M-LF COM-INDENT-NEW-COMMENT-LINE
	    #/C-% COM-REPLACE-STRING
	    #/M-% COM-QUERY-REPLACE
	    #/C-M-H COM-MARK-DEFUN
	    #/C-M-R COM-REPOSITION-WINDOW
	    #/M-/' COM-UPCASE-DIGIT
	    #/C-SH-S COM-LISP-MATCH-SEARCH
	    #/C-Z COM-QUIT
;	    #/END COM-QUIT
	    #/ABORT COM-ABORT-AT-TOP-LEVEL
	    #/C-M-& COM-FROB-LISP-CONDITIONAL
	    #/C-M-$ COM-FROB-DO
	    #/C-SH-A COM-QUICK-ARGLIST
	    #/C-SH-D COM-QUICK-DOCUMENTATION
	    #/M-SH-D COM-LONG-DOCUMENTATION
	    #/C-SH-V COM-DESCRIBE-VARIABLE-AT-POINT
	    #/C-J COM-CHANGE-FONT-CHAR
	    #/M-J COM-CHANGE-FONT-WORD
	    #/C-M-J COM-CHANGE-DEFAULT-FONT
	    #/C-SH-J COM-CHANGE-FONT-REGION
	    #/M-SH-J COM-CHANGE-ONE-FONT-REGION
	    #/M-/# COM-TEXT-JUSTIFIER-CHANGE-FONT-WORD
	    #/M-_ COM-TEXT-JUSTIFIER-UNDERLINE-WORD
	    #/M-$ COM-CORRECT-WORD-SPELLING
	    #/C-M-/# COM-GOTO-CHARACTER
	    #/M-SH-P COM-QUICK-PRINT-BUFFER
	    #/C-SH-U COM-QUICK-UNDO
	    #/C-SH-R COM-QUICK-REDO
	    #/MOUSE-1-1 COM-MOUSE-MARK-REGION
	    #/MOUSE-1-2 COM-MOUSE-MOVE-REGION
	    #/MOUSE-2-1 COM-MOUSE-MARK-THING
	    #/MOUSE-2-2 COM-MOUSE-KILL-YANK
	    )
	  (MAKE-COMMAND-ALIST
	   '(;; COM*:
	     COM-INSTALL-COMMAND COM-SET-KEY COM-ARGLIST
	     COM-COUNT-LINES COM-COUNT-WORDS COM-COUNT-CHARACTERS
	     COM-QUERY-REPLACE COM-REPLACE-STRING
	     COM-HOW-MANY COM-COUNT-OCCURRENCES COM-OCCUR COM-LIST-MATCHING-LINES
	     COM-KEEP-LINES COM-DELETE-NON-MATCHING-LINES
	     COM-FLUSH-LINES COM-DELETE-MATCHING-LINES
	     COM-COMPILE-REGION COM-COMPILE-BUFFER COM-EVALUATE-REGION COM-EVALUATE-BUFFER
	     COM-VIEW-REGISTER COM-LIST-REGISTERS COM-KILL-REGISTER
	     COM-LIST-VARIABLES COM-VARIABLE-APROPOS COM-DESCRIBE-VARIABLE COM-SET-VARIABLE
	     COM-GRIND-DEFINITION COM-GRIND-EXPRESSION COM-EVALUATE-INTO-BUFFER COM-TRACE
	     COM-VIEW-DOVER-QUEUE COM-VIEW-MAIL
	     COM-ATOM-QUERY-REPLACE COM-FORMAT-CODE COM-MULTIPLE-QUERY-REPLACE
	     COM-MULTIPLE-QUERY-REPLACE-FROM-BUFFER COM-QUERY-EXCHANGE
	     COM-QUERY-REPLACE-LAST-KILL COM-QUERY-REPLACE-LET-BINDING
	     COM-FIND-UNBALANCED-PARENTHESES
	     COM-MACRO-EXPAND-EXPRESSION COM-MACRO-EXPAND-EXPRESSION-ALL
	     COM-UNDO COM-REDO COM-DISCARD-UNDO-INFORMATION
	     COM-FILL-LONG-COMMENT COM-KILL-COMMENTS-IN-REGION
	     COM-SORT-LINES COM-SORT-PARAGRAPHS COM-SORT-VIA-KEYBOARD-MACROS
	     COM-EXECUTE-COMMAND-INTO-BUFFER COM-INSERT-DATE COM-DISASSEMBLE
	     COM-UPPERCASE-LISP-CODE-IN-REGION COM-LOWERCASE-LISP-CODE-IN-REGION
	     COM-PRINT-BUFFER COM-PRINT-REGION COM-PRINT-ALL-BUFFERS COM-DISSOCIATED-PRESS
	     COM-MAKE-LOCAL-VARIABLE COM-KILL-LOCAL-VARIABLE COM-LIST-LOCAL-VARIABLES
	     ;; DOC:
	     COM-LIST-COMMANDS COM-APROPOS COM-WHERE-IS COM-DESCRIBE-COMMAND
	     COM-GENERATE-WALLCHART
	     ;; FILES:
	     COM-INSERT-FILE COM-WRITE-REGION COM-APPEND-TO-FILE COM-PREPEND-TO-FILE
	     COM-VIEW-FILE COM-LIST-FILES COM-PRINT-FILE
	     COM-RENAME-FILE COM-DELETE-FILE COM-UNDELETE-FILE
	     COM-COPY-FILE COM-COPY-TEXT-FILE COM-COPY-BINARY-FILE
	     COM-LOAD-FILE
	     ;; DIRS:
	     COM-LIST-ALL-DIRECTORY-NAMES COM-VIEW-DIRECTORY COM-EXPUNGE-DIRECTORY
	     COM-VIEW-LOGIN-DIRECTORY
	     ;; MODES:
	     COM-LISP-MODE COM-TEXT-MODE COM-FUNDAMENTAL-MODE COM-PL1-MODE COM-BOLIO-MODE
	     COM-TEX-MODE COM-TECO-MODE COM-MACSYMA-MODE
	     COM-ELECTRIC-PL1-MODE COM-ATOM-WORD-MODE COM-EMACS-MODE COM-OVERWRITE-MODE
	     COM-ANY-BRACKET-MODE COM-AUTO-FILL-MODE COM-WORD-ABBREV-MODE
	     COM-INSERT-WORD-ABBREVS COM-KILL-ALL-WORD-ABBREVS COM-LIST-WORD-ABBREVS
	     COM-DEFINE-WORD-ABBREVS COM-EDIT-WORD-ABBREVS COM-LIST-SOME-WORD-ABBREVS
	     COM-WRITE-WORD-ABBREV-FILE COM-READ-WORD-ABBREV-FILE COM-MAKE-WORD-ABBREV
	     COM-EDIT-TAB-STOPS COM-MIDAS-MODE COM-RETURN-INDENTS-MODE
	     COM-ELECTRIC-SHIFT-LOCK-MODE COM-ELECTRIC-FONT-LOCK-MODE
             ;; FONT, KBDMAC, DIRED
	     COM-SET-FONTS
	     COM-INSTALL-MACRO COM-INSTALL-MOUSE-MACRO
	     COM-VIEW-KEYBOARD-MACRO COM-NAME-LAST-KEYBOARD-MACRO
	     ))))
  (SETQ *STANDARD-CONTROL-X-COMTAB*
	(SET-COMTAB 'STANDARD-CONTROL-X-COMTAB
		    '(#/C-G COM-PREFIX-BEEP
		      #/C-D COM-DISPLAY-DIRECTORY
		      #/C-N COM-SET-GOAL-COLUMN
		      #/C-P COM-MARK-PAGE
		      #/C-X COM-SWAP-POINT-AND-MARK
		      #/G COM-OPEN-GET-REGISTER
		      #/X COM-PUT-REGISTER
		      #/S COM-SAVE-POSITION
		      #/J COM-JUMP-TO-SAVED-POSITION
		      #/L COM-COUNT-LINES-PAGE
		      #/RUBOUT COM-BACKWARD-KILL-SENTENCE
		      #/C-/; COM-COMMENT-OUT-REGION
		      #/; COM-SET-COMMENT-COLUMN
		      #/. COM-SET-FILL-PREFIX
		      #/F COM-SET-FILL-COLUMN
		      #/C-U COM-UPPERCASE-REGION
		      #/C-L COM-LOWERCASE-REGION
		      #/C-O COM-DELETE-BLANK-LINES
		      #/C-I COM-INDENT-RIGIDLY
		      #/= COM-WHERE-AM-I
		      #/[ COM-PREVIOUS-PAGE
		      #/] COM-NEXT-PAGE
		      #/H COM-MARK-WHOLE
		      #/C-C COM-QUIT
		      #/C-J COM-CHANGE-FONT-REGION
		      #/( COM-START-KBD-MACRO
		      #/) COM-END-KBD-MACRO
		      #/E COM-CALL-LAST-KBD-MACRO
		      #/Q COM-KBD-MACRO-QUERY
		      #/ COM-REPEAT-MINI-BUFFER-COMMAND
		      #/C-T COM-EXCHANGE-LINES
		      #/T COM-EXCHANGE-REGIONS
		      #/# COM-TEXT-JUSTIFIER-CHANGE-FONT-REGION
		      #/_ COM-TEXT-JUSTIFIER-UNDERLINE-REGION
		      #/C-M-SP COM-MOVE-TO-DEFAULT-PREVIOUS-POINT
		      #/HELP COM-DOCUMENT-CONTAINING-PREFIX-COMMAND
		      #/ABORT COM-PREFIX-ABORT
		      )))
  (SET-COMTAB-CONTROL-INDIRECTION *STANDARD-CONTROL-X-COMTAB*)
  (SET-COMTAB *STANDARD-COMTAB*
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *STANDARD-CONTROL-X-COMTAB*)))
  (SETQ *COMPLETING-READER-COMTAB*
	(SET-COMTAB 'COMPLETING-READER-COMTAB
		    '(#/ COM-COMPLETE
		      #/SP COM-SELF-INSERT-AND-COMPLETE
		      #/. COM-SELF-INSERT-AND-COMPLETE
		      #/C-? COM-LIST-COMPLETIONS
		      #/C-Q COM-QUOTED-INSERT
		      #/HELP COM-DOCUMENT-COMPLETING-READ
		      #/C-// COM-COMPLETION-APROPOS
		      #/CR COM-COMPLETE-AND-EXIT
		      #/C-G COM-MINI-BUFFER-BEEP
		      #/ABORT COM-RECURSIVE-EDIT-ABORT
		      #/C-CR COM-COMPLETE-AND-EXIT
		      #/END COM-COMPLETE-AND-EXIT-IF-UNIQUE
		      #/MOUSE-1-1 COM-MOUSE-END-OF-MINI-BUFFER
		      #/MOUSE-3-1 COM-MOUSE-LIST-COMPLETIONS
		      #/C-SH-Y COM-YANK-DEFAULT-STRING
		      #/C-M-Y COM-YANK-PREVIOUS-INPUT
		      #/M-SH-Y COM-POP-MINI-BUFFER-HISTORY
		      #/C-SH-F COM-SPECIFY-FILE-BUFFER
		      #/C-Z :UNDEFINED
		      #/M-Z :UNDEFINED
		      #/C-M-Z :UNDEFINED)))
  (SET-COMTAB-INDIRECTION *COMPLETING-READER-COMTAB* *STANDARD-COMTAB*)
  (SETQ *CONTROL-R-COMTAB*
	(SET-COMTAB 'CONTROL-R-COMTAB
		    '(#/C- COM-EXIT-CONTROL-R
		      #/END COM-EXIT-CONTROL-R
		      #/ABORT COM-EXIT-CONTROL-R)))
  (SET-COMTAB-INDIRECTION *CONTROL-R-COMTAB* *STANDARD-COMTAB*)
  (SETQ *RECURSIVE-EDIT-COMTAB*
	(SET-COMTAB 'RECURSIVE-EDIT-COMTAB
		    '(#/C- COM-EXIT-CONTROL-R
		      #/END COM-EXIT-CONTROL-R
		      #/C-G COM-RECURSIVE-EDIT-BEEP
		      #/ABORT COM-RECURSIVE-EDIT-ABORT)))
  (SET-COMTAB-INDIRECTION *RECURSIVE-EDIT-COMTAB* *STANDARD-COMTAB*)
  (SETQ *STANDALONE-COMTAB*
	(SET-COMTAB 'STANDALONE-COMTAB
		    '(#/END COM-QUIT
		      #/ABORT COM-STANDALONE-ABORT
		      #/C- COM-QUIT)))
  (SET-COMTAB-INDIRECTION *STANDALONE-COMTAB* *STANDARD-COMTAB*)
  )
))