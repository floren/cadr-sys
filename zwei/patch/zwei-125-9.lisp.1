;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.9
;;; Reason:
;;;  Improve ZMacs' spelling checker commands - particularly how they ensure
;;;  that the spelling checker is loaded.  Create the SPELL package if needed.
;;;  Etc.
;;; Written 1-Aug-88 18:24:40 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.6, ZWEI 125.7, ZMail 73.0, Local-File 75.1, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, Experimental KMC-SYSTEM 3.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, falambka.



; From modified file DJ: L.ZWEI; NEW-ISPELL.LISP#8 at 1-Aug-88 18:25:14
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NEW-ISPELL  "


(defun assure-spell-system-loaded(&optional error-p)
  "Try to make sure the spelling checker is loaded.
Won't signal an error unless ERROR-P is non-NIL."
  (unless (fboundp 'spell-word)
    (if (not (si:find-system-named :ISPELL (not error-p)))
	(barf "The ISPELL system is not accessible")
      (if (not (y-or-n-p-with-timeout (* 2. 60. 60.) T "Load the ISPELL (spelling checker) system?"))
	  (barf "ISPELL is not loaded.")
	(progn
	  (unless (find-package "SPELL")
	    (make-package "SPELL" :use "GLOBAL"))
	  (make-system :ISPELL :noconfirm)
	  (unless (fboundp 'spell-word)
	    (barf "Error: The spelling checker is not defined.")))))))

))

; From modified file DJ: L.ZWEI; NEW-ISPELL.LISP#8 at 1-Aug-88 18:25:18
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NEW-ISPELL  "

  
(defcom com-correct-word-spelling
	"Check the spelling of the word before point.
The status of the word will be reported in the echo area."
  ()
  (assure-spell-system-loaded)
  (let* ((bp1 (forward-word (forward-word (forward-char (point) -1 t) 1 t) -1 t))
	 (bp2 (forward-word bp1 1 t))
	 (word (string-interval bp1 bp2)))
    (format *query-io* "Checking the spelling of ~s --" word)
    (zwei-correct-spelling bp1 bp2)
    )
  dis-none)

))

; From modified file DJ: L.ZWEI; NEW-ISPELL.LISP#8 at 1-Aug-88 18:25:35
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; NEW-ISPELL  "


(defcom com-correct-spelling
	"Run the spelling checker on the buffer or the region.
Each word not found in the dictionary will be displayed, and
your choices for proceeding will be listed in the echo area."
	()
  (assure-spell-system-loaded)
  (let ((bp1 (interval-first-bp *interval*))
	(bp2 (interval-last-bp *interval*)))
    (when (window-mark-p *window*)
      (setq bp1 (copy-bp (mark)) bp2 (copy-bp (point)))
      (order-bps bp1 bp2))
    (setf (window-mark-p *window*) nil)
    (must-redisplay *window* dis-mark-goes)
    (setq bp1 (copy-bp bp1))
    (setq bp2 (copy-bp bp2 :moves))
    (do-named for-each-line
	      (end-of-line-bp
		line bp3
		)
	      (())
      (setq line (bp-line bp1))
      (setq end-of-line-bp (create-bp line
				      (if (eq line (bp-line bp2))
					  (bp-index bp2)
					(string-length line))
				      :moves))
      (do-named for-a-line
		()
		(())
	;;skip to first alphabetic character
	(do-named find-next-word
		  ()
		  (())
	  (when (bp-= bp1 end-of-line-bp)
	    (return-from find-next-word nil))
	  (let ((char (char-code (aref line (bp-index bp1)))))
	    (when (or (<= #/A char #/Z)
		      (<= #/a char #/z))
	      (return-from find-next-word nil))
	    (incf (bp-index bp1))))
	(setq bp3 (copy-bp bp1))
	;;find end of alphabetic string
	(do-named find-last-letter-of-word
		  ()
		  (())
	  (when (bp-= bp3 end-of-line-bp)
	    (return-from find-last-letter-of-word nil))
	  (let ((char (char-code (aref line (bp-index bp3)))))
	    (when (not (or (<= #/A char #/Z)
			   (<= #/a char #/z)))
	      (return-from find-last-letter-of-word nil)))
	  (incf (bp-index bp3)))
	(when (= (bp-index bp1) (bp-index bp3))
	  (return-from for-a-line nil))
	;;now check it
	(let ((choices (inhibit-style-warnings (spell-word line (bp-index bp1) (bp-index bp3)))))
	  (when (or (null choices)
		    (consp choices))
	    (setq bp3 (copy-bp bp3 :moves))
	    (fix-word bp1 bp3 choices)
	    (flush-bp bp3)))
	(move-bp bp1 bp3))
      ;;now move bp1 to next line
      (when (eq line (bp-line bp2))
	(return-from for-each-line nil))
      (move-bp bp1 (line-next line) 0)
      (flush-bp end-of-line-bp)
      )
    (flush-bp bp2)
    )
  dis-none)

))
