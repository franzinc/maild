(in-package :user)

(eval-when (compile load eval)
  (require :acldns))

;; Modelled after the glibc resolver which uses the search path first
;; if there is no dot in the domain name.  
(defun useful-dns-query (domain &rest rest &key (search t) &allow-other-keys)
  (macrolet ((flags (respform)
	       (let ((respvar (gensym)))
		 `(let ((,respvar ,respform))
		    (if (eq (type-of ,respvar) 'dns-response)
			(dns-response-flags ,respvar)
		      (fourth ,respvar))))))
    (when (not *dns-configured*)
      ;; since we rely on *dns-domain* and *domain-search-list*
      (configure-dns :auto t) 
      ;; Avoid using old data.
      (setf socket::*stale-entry-remove-time* 0))

    (let ((search-list (copy-list *domain-search-list*))
	  res)
      (if *dns-domain*
	  (pushnew *dns-domain* search-list :test #'equalp))
      (if (and search (null (position #\. domain)))
	  (dolist (searchdom search-list)
	    (setf res 
	      (multiple-value-list 
	       (apply #'dns-query (cons 
				   (concatenate 'string domain "." searchdom)
				   rest))))
	    ;; Only loop if there's a definitely negative answer.
	    (if (not (member :no-such-domain (flags res)))
		(return-from useful-dns-query (values-list res)))))
      (apply #'dns-query (cons domain rest)))))


;; Only follows one CNAME lookup.   If there is any more than that,
;; the sender's domain has a really jacked up setup.

;; possible answers
;;  t -- yes, there exists a record of that type.
;;  nil -- no record of that type exists
;;  :nxdomain -- the domain itself doesn't exist
;;  :unknown -- couldn't get any answers.
(defun dns-record-exists-p (domain type &key (try-cname t))
  (block nil
    (let ((resp (useful-dns-query domain :decode nil :type type)))
      (if (null resp)
	  (return :unknown))
      (let ((flags (dns-response-flags resp))
	    (answer (dns-response-answer resp)))
	(cond 
	 ((member :nameserver-internal-error flags)
	  (return :unknown))
	 ((member :no-such-domain flags)
	  (return :nxdomain))
	 ((null answer)
	  (return nil)) ;; no records of that type for that name
	 ((member :cname answer
		  :test #'eq :key #'dns-rr-type)
	  (if (not try-cname)
	      (return nil)
	    ;; There should only be one cname answer.
	    (return (dns-record-exists-p (dns-rr-answer (first answer))
				       type :try-cname nil))))
	 (t
	  t))))))
  
;; A valid email domain is one that has an MX record or an A record
;; or a CNAME to an MX or A record.

;; possible answers:  
;;  t -- there is either an MX or A record for that domain
;;  nil -- there is neither an MX nor A record for that domain
;          (possibly because the domain does not exist at all)
;; :unknown -- couldn't get answers
(defun valid-email-domain-p (domain)
  (block nil
    (let ((res (dns-record-exists-p domain :mx)))
      (cond
       ((eq res t)
	(return t))
       ((eq res :nxdomain)
	(return nil))
       ((eq res :unknown)
	(return :unknown)))
      (setf res (dns-record-exists-p domain :a))
      (cond
       ((eq res t)
	(return t))
       ((eq res :nxdomain)
	(return nil))
       ((eq res :unknown)
	(return :unknown)))
      nil)))

;; Requires properly set up forward and reverse DNS
(defun compute-fqdn ()
  (let ((mainip (useful-dns-query (gethostname)))
	fqdn)
    (if (null mainip)
	(error "Could not resolve our hostname (~A) via DNS" (gethostname)))
    (setf fqdn (dns-ipaddr-to-hostname mainip))
    (if (null fqdn)
	(error "Could not do a reverse DNS lookup on our IP address (~A)"
	       (ipaddr-to-dotted mainip)))
    fqdn))


;; If there are DNS troubles, the user can set *fqdn* in the config file.
(defun fqdn ()
  (if *fqdn*
      *fqdn*
    (setf *fqdn* (compute-fqdn))))


    