;; $Id: auth.cl,v 1.2 2006/07/30 17:29:42 dancy Exp $

(in-package :user)

(defun authenticate-user (user pass)
  (util.pam:with-pam (pam "smtp" user)
    (util.pam:pam-authenticate pam :password pass)))
