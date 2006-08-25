;; $Id: auth.cl,v 1.3 2006/08/25 16:20:31 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :pam))

(defun authenticate-user (user pass)
  (util.pam:with-pam (pam "smtp" user)
    (util.pam:pam-authenticate pam :password pass)))
