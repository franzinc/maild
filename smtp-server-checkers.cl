(in-package :user)

;; utils
(defun add-smtp-mail-from-checker (name func)
  (setf *smtp-mail-from-checkers*
    (nconc *smtp-mail-from-checkers*
	   (list (list name func)))))

(defun add-smtp-rcpt-to-checker (name func)
  (setf *smtp-rcpt-to-checkers*
    (nconc *smtp-rcpt-to-checkers*
	   (list (list name func)))))

(defun add-smtp-data-pre-checker (name func)
  (setf *smtp-data-pre-checkers*
    (nconc *smtp-data-pre-checkers*
	   (list (list name func)))))

;; default checkers for the smtp server

;; MAIL FROM:

(defun smtp-mail-from-blacklist-checker (ip addr)
  (declare (ignore ip))
  (if (sender-blacklisted-p (emailaddr-orig addr))
      (values :err (format nil "~A... Access denied"
			   (emailaddr-orig addr)))
    :ok))

(defun smtp-mail-from-domain-required-checker (ip addr)
  (declare (ignore ip))
  (if (and *sender-domain-required* 
	   (not (emailnullp addr))
	   (null (emailaddr-domain addr)))
      (values :err (format nil "~A... Sender domain required"
			   (emailaddr-orig addr)))
    :ok))

(defun smtp-mail-from-domain-checker (ip from)
  (declare (ignore ip))
  (block nil
    (let ((domain (emailaddr-domain from)))
      (if (null domain)
	  (return :ok))
      (let ((exists (domain-exists-p domain)))
	(when (eq exists :unknown)
	  (return 
	    (values :transient "Domain resolution error")))
	
	(when (null exists)
	  (return
	    (values :err "Sender domain must exist")))
	
	:ok))))

;; RCPT TO:

(defun smtp-rcpt-to-relay-checker (ip from type recip recips)
  (declare (ignore from recips))
  (if (or (eq type :local) 
	  (relaying-allowed-p ip))
      :ok
    (values :err (format nil "~A... Relaying denied" (emailaddr-orig recip)))))
	  
(defun relaying-allowed-p (cliaddr)
  (dolist (check *relay-access*)
    (if (addr-in-network-p cliaddr (parse-addr check))
	(return t))))

(defun smtp-rcpt-to-dns-blacklist-checker (ip from type recip recips)
  (declare (ignore type recips))
  (block nil
    (let ((domain (connection-dns-blacklisted-p ip)))
      (if (null domain)
	  (return :ok))
      (maild-log "(~A/~A/~A) blacklisted by ~A"
		 (socket:ipaddr-to-dotted ip) 
		 (emailaddr-orig from)
		 (emailaddr-orig recip)
		 domain)
      (ecase *dns-blacklisted-response-type*
	(:transient
	 (return 
	   (values :transient 
		   (format nil "Please try again later (~A)" domain))))
	(:permanent
	 (return 
	   (values :err 
		   (format nil "Blacklisted by ~A" domain))))))))
