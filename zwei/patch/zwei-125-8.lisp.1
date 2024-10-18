;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZWEI version 125.8
;;; Reason:
;;;  In List Tag Buffers, if there's no current search context (e.g., we're
;;;  at end of buffers) we weren't printing the search string at all.  Now,
;;;  print it at bottom of display.  (Even though it can't be used for
;;;  anything, the user may want to see it to verify that the search was
;;;  working ok.)
;;; Written 1-Aug-88 14:18:19 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Breaking Glass from band 3
;;; with Experimental System 126.6, ZWEI 125.7, ZMail 73.0, Local-File 75.1, File-Server 24.0, Unix-Interface 13.0, Tape 24.2, Lambda-Diag 17.0, microcode 1762, SDU Boot Tape 3.14, SDU ROM 103, falambka.



; From modified file DJ: L.ZWEI; SECTIO.LISP#315 at 1-Aug-88 14:18:20
#8R ZWEI#: 
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; SECTIO  "

(defcom com-list-tag-buffers "Display buffers and search context associated with a tag table.
  If there is numeric (or universal) arg, use current tag table.
  Else, ask which tag table to display buffers for.
  Displays item-list of buffer names you can select." ()
  (let ((output *query-io*)
	tag-table tag-table-name
	buffers current-buffer
	search-string search-key-p)
    (multiple-value (buffers tag-table) (tag-table-buffers nil *numeric-arg-p*))
    (if (null buffers) (BARF "No buffers."))
    (setq output *standard-output*)
    (setq tag-table-name (or (car (rassq tag-table *zmacs-tag-table-alist*))
			     (format nil "~S" tag-table)))
    ;;This is the buffer, if any, in path of current search
    (setq current-buffer (send tag-table :get 'current-pathname))
    ;;We may have a search string associated with the tag table.
    (setq search-string  (zmacs-tags-search-key-string))
    ;;We <may> also have a search context, i.e. we're not at end of buffers with search
    (setq search-key-p   (and current-buffer search-string))
    (format output "~&Tag table~@[ ~A~]~@[ ~*(the current tag table)~]~@[ - ~*search context is established.~]~2&"
	    tag-table-name
	    (eq tag-table *zmacs-current-tag-table*)
	    search-key-p)
    (format output "~&Buffers read in:")
    (let* ((current-buffer-mark-char #/+)
	   (max-size (- (send output :size-in-characters) 6.))
	   (max-buffer-string-len (- max-size 6))
	   (max-buffer-name-len (find-maximum-buffer-name-length max-buffer-string-len buffers)))
      (format output "~%   ~a" (make-string max-buffer-name-len :initial-element #\-))
      (when search-key-p
	(format output "  Search string:"))
      (dolist (buffer buffers)
	(let*((currentp  (eq buffer current-buffer))
	      (buffer-mark-char (if currentp current-buffer-mark-char #/space))
	      (buffer-string (name-for-display buffer max-buffer-name-len)))
	  (format output "~& ~c " buffer-mark-char)
	  (send output :item 'zmacs-buffer buffer "~A" buffer-string)
	  (when (and search-key-p currentp)
	    (let*((extra (+ 2 (- max-buffer-name-len (string-length buffer-string))))
		  (leftover (- max-size max-buffer-name-len extra))
		  (search-string (and (plusp leftover)
				      (zmacs-tags-search-key-string-for-display leftover))))
	      (when search-string (format output "~V,@T{~A}" extra search-string))))
	  (terpri output)))
      (terpri output)
      (when current-buffer
	(format output "~& ~c  means current search buffer." current-buffer-mark-char))
      (cond
	(search-key-p (format output "~&{ } shows current search string."))
	(search-string (format output "~&{~A} was last search string." search-string)))
      (terpri output)))
  dis-none)

))
