;-*-Mode: Lisp; Package: FILE-SYSTEM; Base: 8-*-

;This file defines functions which allow a node to change itself into
;a different flavor of node without anybody noticing.

;First, you construct the new node flavor instance with
;open-replacement-node.
;Then, you transfer into it the old data (this is situation-dependent).
;Then, do the replacement, by calling replace-node.

;When this returns, the directory is already written out
;containing the new flavor of this node, and its new pointer info.
(defun replace-node (new-node)
  (setq node-node new-node)
  (funcall new-node 'report-to-supernode))

(defmethod (node report-to-supernode) ()
  (funcall supernode 'set-subnode-byte-size supernode-info
	   byte-size length-in-bytes))

(defun open-replacement-node (flavor &rest keyword-args)
 (declare-flavor-instance-variables (node)
  (let (subnode new-flavor new-pointer-info)
    (setf (values keyword-args new-flavor)
	  (funcall (make-instance flavor ':supernode supernode)
		   'default-pointer-info supernode-info keyword-args))
    (setq subnode
	  (make-instance (or new-flavor flavor)
			 ':supernode supernode
			 ':supernode-info supernode-info
			 ':reasons-why-open reasons-why-open
			 ':byte-size byte-size
			 ':length-in-bytes length-in-bytes))
    (multiple-value (subnode new-pointer-info)
      (funcall subnode 'init-node keyword-args '(:inhibit-links t)))
    (funcall supernode 'set-entry-pointer-info supernode-info
	     new-pointer-info
	     (funcall subnode ':flavor))
    subnode)))

;Applications of flavor-changing:

;Change a string-node into a text-node (if it gets too big).

(defun change-to-text-node ()
 (declare-flavor-instance-variables (string-node)
  (let ((newnode (open-replacement-node 'text-node)))
    (funcall newnode ':output-array-lispm-format text-string 0 0 0 0)
    (replace-node newnode))))
