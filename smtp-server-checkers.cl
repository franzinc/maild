;; $Id: smtp-server-checkers.cl,v 1.8 2003/07/08 18:05:25 layer Exp $

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

(defun add-smtp-connection-checker (name func)
  (setf *smtp-connection-checkers*
    (nconc *smtp-connection-checkers*
	   (list (list name func)))))

;; default checkers for the smtp server

;; Initial connection

(defun smtp-connection-blacklist-checker (ip)
  (let ((bl (connection-blacklisted-p ip)))
    (if* (null bl)
       then
	    :ok
       else
	    (values :err 
		    (format nil "~A (~A)" *blacklisted-response* bl)))))

(defun smtp-connection-reverse-dns-checker (ip)
  (block nil
    (if (not *reverse-dns-required*)
	(return :ok))
    (let ((exists (domain-exists-p 
		   (concatenate 'string (flip-ip ip) ".in-addr.arpa"))))
      (when (eq exists :unknown)
	(return 
	  (values :transient "Domain resolution error")))
      
      (when (null exists)
	(return
	  (values :err "Client reverse DNS record must exist")))
      
      :ok)))


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
  (declare (ignore recips))
  (block nil
    (if (eq type :local)
	(return :ok))

    (dolist (checker *relay-checkers*)
      (if (funcall checker ip from recip)
	  (return-from smtp-rcpt-to-relay-checker :ok)))
    
    (values :err (format nil "~A... Relaying denied" (emailaddr-orig recip)))))
	  
(defun relaying-allowed-p (cliaddr from recip)
  (declare (ignore from recip))
  (dolist (check *relay-access*)
    (if (addr-in-network-p cliaddr (parse-addr check))
	(return t))))

(defun smtp-rcpt-to-dns-blacklist-checker (ip from type recip recips)
  (declare (ignore type recips))
  (block nil
    ;; check exceptions first
    (let ((orig (emailaddr-orig recip)))
      (dolist (string *dns-blacklist-recipient-exceptions*)
	(if (equalp string orig)
	    (return-from smtp-rcpt-to-dns-blacklist-checker :ok))))
    
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
	   (values
	    :transient
	    (let ((default-msg
		      (format nil "Please try again later (~A)" domain)))
	      (if* *dns-blacklist-temp-failure-response*
		 then (or (ignore-errors
			   (format nil *dns-blacklist-temp-failure-response*
				   domain))
			  default-msg)
		 else default-msg)))))
	(:permanent
	 (return 
	   (values
	    :err
	    (let ((default-msg (format nil "Blacklisted by ~A" domain)))
	      (if* *dns-blacklist-failure-response*
		 then (or (ignore-errors
			   (format nil *dns-blacklist-failure-response*
				   domain))
			  default-msg)
		 else default-msg)))))))))
