;; $Id: auth.cl,v 1.1 2005/09/23 16:26:37 dancy Exp $

;; XXX -- This would be much better if we had a PAM API.

(in-package :user)

(defun authenticate-user (user pass)
  (block nil
    (let ((pwent (getpwnam user)))
      (if (null pwent)
	  (return))
      
      (string= (pwent-passwd pwent)
	       (crypt pass (pwent-passwd pwent))))))
