;;; -*- Mode: LISP; Package: CHAOS; BASE: 8 -*-

;;much of this used to be in sys:io;chsaux

;;chaosnet hacks that MIT people sometimes use

;;; utility for these kludges.
(DEFUN QUICK-SERVER-STRING (HOSTS CONTACT-NAME &OPTIONAL (ARGS "") (TIMEOUT 300.)
			    		       &AUX HOST STREAM)
  "Given that a server exists for CONTACT-NAME at all of the hosts in HOSTS, try to
return a string that that quickie server provides, orNIL if there is none.
If ARGS is provided, it is send in the initial RFC as the data to the server.
Timeout is the time in sixtieths of a second to wait before giving up."
  (DO-FOREVER
    (SETQ HOST (OR HOSTS (UP-HOSTS HOSTS 1 TIMEOUT)))
    (COND ((NULL HOST)
	     NIL)
	    (T
	     (SETQ STREAM (OPEN-STREAM (CAR HOST)
				       (STRING-APPEND CONTACT-NAME #/SPACE ARGS)
				       :TIMEOUT TIMEOUT
				       :ERROR NIL
				       :ASCII-TRANSLATION T	;usually right
				       :DIRECTION :INPUT))
	     (IF (ERRORP STREAM)
		 (SETQ HOSTS (CDR HOSTS))
	       (RETURN (WITH-OPEN-STREAM (STREAM STREAM)
			 (WITH-OUTPUT-TO-STRING (S)
			   (STREAM-COPY-UNTIL-EOF STREAM S)))))))))
	    
;;;; Random server and user routines

(DEFVAR *LIMERICK-HOSTS* '("mc") "List of hosts that have a limerick server.")

;;; The infamous LIMERICK getter
(DEFUN LIMERICK (&OPTIONAL (ARGS ""))
  "Print a limerick obtained from one of the limerick server hosts in chaos:*limerick-hosts*
If ARGS is provided, it is assumed to be the number of the limerick requested."
  (AND (NUMBERP ARGS) (SETQ ARGS (FORMAT NIL "~D" ARGS))) 
  (QUICK-SERVER-STRING *LIMERICK-HOSTS* "LIMERICK" ARGS))

(DEFVAR *BYE-HOSTS* '("XX" "OZ") "List of hosts that have a bye server.")

(DEFUN BYE ()
  "Get an interesting statement about life from one of the servers in chaos:*bye-hosts*"
  (QUICK-SERVER-STRING *BYE-HOSTS* "BYE"))

(DEFUN TINGLE (&OPTIONAL (USE-CMU-P T))
  "Get a tingle quotation from a host."
  (IGNORE USE-CMU-P) ;;can't do it anywhere else yet
  (QUICK-SERVER-STRING '("MC")
		       "NAME"
		       "tingle%cmu-cs-a"))

(DEFVAR *YOW-HOSTS* '("CCC") "Lists of hosts that have a YOW server.")

(DEFUN YOW ()
  "Yow!! Get an offical Zippy the Pinhead quotation from any of the servers in chaos:*yow-hosts*"
  (QUICK-SERVER-STRING *YOW-HOSTS* "YOW"))

(DEFVAR *LSC-HOSTS* '("EE"))
(DEFUN LSC (&OPTIONAL (ARGS ""))
  "Get the MIT-LSC movie schedule for the current week, or a different week if ARGS are given."
  (QUICK-SERVER-STRING *LSC-HOSTS* "LSC" ARGS))



;;;;open the door!  (maybe someday marty will make this work.)
;;(DEFUN HACK-DOOR (COMMAND)
;;  "Open the door of the 9th floor of the AI lab.  Only works if AI's chaosnet is up."
;;  (CONDITION-CASE (RESULT)
;;      (SIMPLE "AI" (STRING-APPEND "DOOR " COMMAND))
;;    (SYS:REMOTE-NETWORK-ERROR
;;      (TV:NOTIFY NIL "Failed trying to hack the door:~%~A" RESULT))
;;    (SYS:UNCLAIMED-MESSAGE
;;     (TV:NOTIFY NIL
;;		"Can't even try to answer the door because MIT-AI isn't on the chaosnet."))
;;    (:NO-ERROR (RETURN-PKT RESULT) T)))

hacks:
(defun toscanini-uptime-info (&optional (stream *standard-output*) (time (time:get-universal-time)))
  (let ((*standard-output* stream))
    (multiple-value-bind (nil m h) (decode-universal-time time)
      (cond (( 12. h 24.)
	     (format t "There are still ~D Hour~P and ~D minute~P to get icecream!"
		     (- 23. h) (- 23. h) (- 60. m) (- 60. m)))
	    ((< h 11.)
	     (format t "Well, you lose. Toscanini's doesn't open again for over ~D hour~P!"
		  (- 11. h)))
	 (t
	  (format t "Yow! Toscanini's opens again in only ~D minute~P"
		  (- 60. m) (- 60. m)))))))