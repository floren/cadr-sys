;;; -*- Mode:LISP; Package:TAPE; Readtable:CL; Base:10 -*-
;;;
;;; Remote tape device support
;;;
;;; -dg 2/18/86
;;;

(defmacro defmethod-forward ((flavor method))
  `(defmethod (,flavor ,method) (&rest arguments)
     (send self :forward-simple-operation ,method arguments)))

(defflavor remote-tape-device (unit
			       density
			       (device-type)
			       (host)
			       (stream)
			       (buffer))
	   (basic-tape-device)
  (:gettable-instance-variables device-type stream host buffer)
  (:required-instance-variables :device-type :host))

(defmethod (remote-tape-device :allocate-buffer) (size-in-pages)
  (unless (and buffer
	       (>= (si:dma-buffer-size-in-pages buffer) size-in-pages))
    (when buffer
      (deallocate-resource 'si:dma-buffer buffer))
    (allocate-resource 'si:dma-buffer size-in-pages)))

(defmethod (remote-tape-device :forward-simple-operation) (operation &rest args)
  (unless stream
    (ferror nil "REMOTE-TAPE: No stream to forward operation to..."))
  (format stream ":OPERATION ~S ~S "
	  operation args)
  (send stream :force-output)
  (let ((return-code (read stream)))
    (ecase return-code
      (:values (values-list (read stream)))
      (:error (signal-condition (read stream))))))

(defmethod-forward (remote-tape-device :unit))
(defmethod-forward (remote-tape-device :density))
(defmethod-forward (remote-tape-device :set-options))

(defmethod (remote-tape-device :initialize) (&rest init-options)
  (setq stream (network:open-remote-tape-server host))
  (lexpr-send self :set-options init-options))

(defmethod (remote-tape-device :deinitialize) ()
  (format stream ":finished ")
  (send stream :force-output)
  (close stream)
  (send self :unlock-device))

(defmethod (remote-tape-device :lock-device) ())

(defmethod (remote-tape-device :unlock-device) ())

(defmethod (remote-tape-device :device-locked-p) ()
  (and stream t))

(defmethod-forward (remote-tape-device :reset))
(defmethod-forward (remote-tape-device :status))
(defmethod-forward (remote-tape-device :speed-threshold))
(defmethod-forward (remote-tape-device :space))
(defmethod-forward (remote-tape-device :space-reverse))
(defmethod-forward (remote-tape-device :search-filemark))
(defmethod-forward (remote-tape-device :search-filemark-reverse))
(defmethod-forward (remote-tape-device :optimal-chunk-size))

(defmethod (remote-tape-device :read-block) (dma-buffer record-size)
  (format stream ":read-block ~D " record-size)
  (send stream :force-output)
  (let ((reply (read stream)))
    (ecase reply
      (:error (signal-condition (read stream)))
      (:data (let ((data-fill (read stream)))
	       (when (zerop data-fill)
		 (ferror nil "Block from remote tape device had 0 bytes!!!"))
	       (send stream :string-in t (si:dma-buffer-string dma-buffer) 0 data-fill)
	       data-fill)))))

(defmethod (remote-tape-device :write-block) (dma-buffer record-size)
  (format stream ":write-block ~D " record-size)
  (send stream :string-out (si:dma-buffer-string dma-buffer) 0 record-size)
  (let ((reply (read stream)))
    (ecase reply
      (:error (signal-condition (read stream)))
      (:values (lexpr-funcall 'values (read stream))))))

;(defmethod (remote-tape-device :read-array) (array number-of-records record-size)
;  (format stream ":read-array ~D ~D " number-of-records record-size)
;  (send stream :force-output)
;  (let ((reply (read stream)))
;    (ecase reply
;      (:error (signal-condition (read stream)))
;      (:data (let ((buffer (if (memq (array-type array) '(art-8b art-string))
;			       array
;			     (copy-array-contents


(define-tape-device remote-tape-device "RM" true)

(defun remote-tape-server (stream)
  ;; do the right stuff here.
  stream)

;; code to hook this into chaos protocols:

(net:define-network-function (net:open-remote-tape-server :chaos) (host)
  (chaos:open-stream host "LRTDP"))

(defun chaos-remote-tape-server ()
  (with-open-stream (stream (chaos:open-stream nil "LRTDP"))
    (remote-tape-server stream)))

(si:add-initialization "LRTDP"
		       '(process-run-function "LRTDP server" 'chaos-remote-tape-server)
		       ()
		       'chaos:Server-alist)







