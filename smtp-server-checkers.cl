;; copyright (C) 2003 Franz Inc, Oakland, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: smtp-server-checkers.cl,v 1.15 2005/09/22 04:02:57 dancy Exp $

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

(defun add-smtp-data-checker (name func)
  (setf *smtp-data-checkers*
    (nconc *smtp-data-checkers*
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
    (if (or (not *reverse-dns-required*)
	    (relaying-allowed-p ip nil nil))
	(return :ok))
    
    ;; When doing :ptr lookups, dns-query automatically flips the address
    ;; and adds .in-addr.arpa for us.
    (let ((res (dns-record-exists-p (socket:ipaddr-to-dotted ip) :ptr)))
      (when (eq res :unknown)
	(return 
	  (values :transient "Domain resolution error")))
      
      (when (or (null res) (eq res :nxdomain))
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
      (let ((res (valid-email-domain-p domain)))
	(when (eq res :unknown)
	  (return 
	    (values :transient 
		    (format nil "Domain (~A) resolution error" domain))))
	(when (null res)
	  (return
	    (values :err (format nil "Sender domain (~A) must resolve" domain))))
	
	:ok))))

;; RCPT TO:

(defun smtp-rcpt-to-relay-checker (sess ip from type recip recips)
  (declare (ignore recips))
  (block nil
    (if (eq type :local)
	(return :ok))

    (if (or (session-auth-user sess)
	    (relaying-allowed-p ip from recip))
	(return :ok))
    
    (if *client-authentication*
	(return (values :err "Authentication required")))
    
    (values :err (format nil "~A... Relaying denied" (emailaddr-orig recip)))))

(defun check-relay-access (cliaddr from recip)
  (declare (ignore from recip))
  (dolist (check *relay-access*)
    (if (addr-in-network-p cliaddr (parse-addr check))
	(return t))))

  

(defun smtp-rcpt-to-dns-blacklist-checker (sess ip from type recip recips)
  (declare (ignore type recips sess))
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
