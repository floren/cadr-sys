; -*- Mode:LISP; Package:ZWEI; Base:8 -*- 
;;; Mail file support for local file system
; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFMETHOD (FS:LM-PATHNAME :MAIL-FILE-FORMAT-COMPUTER) (IGNORE)
  'BABYL-MAIL-FILE)

(DEFMETHOD (FS:LM-PATHNAME :POSSIBLE-RMAIL-FILES) ()
  (LIST (FUNCALL-SELF ':NEW-PATHNAME ':NAME "ZMAIL" ':TYPE "BABYL" ':VERSION 0)))

(DEFMETHOD (FS:LM-PATHNAME :POSSIBLE-MAIL-FILE-FLAVORS) ()
  '(BABYL-MAIL-FILE))

(DEFMETHOD (FS:LM-PATHNAME :NEW-MAIL-PATHNAME) ()
  (FUNCALL-SELF ':NEW-PATHNAME ':NAME "ZMAIL" ':TYPE "TEXT" ':VERSION 0))

(DEFMETHOD (FS:LM-PATHNAME :ZMAIL-TEMP-FILE-NAME) ()
  "_ZMAIL_")

(DEFMETHOD (FS:LM-PATHNAME :NEW-MAIL-FILE-FLAVOR) ()
  'ITS-NEW-MAIL-FILE)