;;; -*- Mode:Lisp;Package:NETI;Base:10; Readtable: T -*-
;;; This is SYS: NETWORK; REGIONS

(DEFMACRO GET-REGION (S) `(GET ,S 'NET-REGION))

(DEFUN DEFINE-NET-REGION (REGION-NAME SPEC)
  (UNLESS (EQUAL SPEC (GET REGION-NAME 'NET-REGION-SPEC))
    (SETF (GET REGION-NAME 'NET-REGION-SPEC) SPEC)
    (SETF (GET-REGION REGION-NAME)
	  (LET ((PROCESS-FUNCTION (GET (CAR SPEC) 'REGION-SPEC-INITIALIZER)))
	    (IF PROCESS-FUNCTION (CONS (CAR SPEC) (MAPCAR PROCESS-FUNCTION (CDR SPEC)))
	      SPEC)))))

(DEFUN DEFAULT-IN-REGION-FUNCTION (NETWORK NETWORK-ADDRESS REGION)
  (AND (EQ NETWORK (CAR REGION))
       (CLI:MEMBER NETWORK-ADDRESS (CDR REGION) :TEST (GET NETWORK 'ADDRESS-REGION-TEST))))

(DEFUN (:PROPERTY :CHAOS ADDRESS-REGION-TEST) (ADDRESS SUBNET)
  (= (LDB 1010 ADDRESS SUBNET)))

(DEFUN IN-NETWORK-REGIONS-P (NETWORK NETWORK-ADDRESS REGION)
  (CLI:MEMBER NETWORK-ADDRESS (CDR REGION) :TEST
	      #'(LAMBDA (ADDRESS REGION)
		  (IN-NETWORK-REGION-P NETWORK NETWORK-ADDRESS REGION))))

(DEFPROP :REGIONS IN-NETWORK-REGIONS-P IN-REGION-FUNCTION)

(DEFUN (:PROPERTY :EXCEPT IN-REGION-FUNCTION) (NETWORK NETWORK-ADDRESS REGION)
  (NOT (IN-NETWORK-REGION-P NETWORK NETWORK-ADDRESS REGION)))

(DEFUN IN-NETWORK-REGION-P (NETWORK NETWORK-ADDESS REGION-NAME)
  (LET ((REGION (GET REGION-NAME 'NET-REGION)))
    (FUNCALL (OR (GET (CAR REGION) 'IN-REGION-FUNCTION)
		 'DEFAULT-IN-REGION-FUNCTION)
	     NETWORK NETWORK-ADDRESS REGION)))