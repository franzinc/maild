(in-package :user)

(defun get-mailer-by-id (id)
  (let ((entry (find id *mailers* :test #'eq :key #'first)))
    (if (null entry)
	(error "get-mailer-by-id: No mailer matches ~S!" id))
    entry))

(defun mark-recip-with-suitable-mailer (recip)
  (block nil
    (if (recip-type recip) ;; special recip
	(return))
    (when (not (local-domain-p (recip-addr recip)))
      (setf (recip-mailer recip) :smtp)
      (return))
    (dolist (mailer *mailers*)
      (when (funcall (third mailer) (recip-addr recip))
	(setf (recip-mailer recip) (first mailer))
	(return)))
    (when (null (recip-mailer recip))
      ;; No mailer accepted the address.  Convert it into an
      ;; error recip.
      (setf (recip-type recip) :error)
      (setf (recip-errmsg recip) "No such mailbox"))))

(defun any-mailer-matches-p (addr)
  (dolist (mailer *mailers*)
    (if (funcall (third mailer) addr)
	(return t))))

;; returns the run-as user as a second value
(defun make-delivery-command-for-recip (recip q)
  (let ((mailer (get-mailer-by-id (recip-mailer recip))))
    (values (funcall (fourth mailer) (recip-addr recip) q)
	    (fifth mailer))))

;;;; built-in mailers.

(defun lookup-addr-in-passwd (thing)
  (let ((addr (make-parsed-and-unparsed-address thing)))
    (and (local-domain-p addr)
	 (getpwnam (string-downcase (emailaddr-user addr))))))

(defun deliver-local-command (addr q)
  (list "/usr/bin/procmail" 
	"-Y"
	"-f"
	(emailaddr-orig (rewrite-local-envelope-sender (queue-from q)))
	"-d"
	(string-downcase (emailaddr-user addr))))


