(in-package :user)

;; Default address rewriting rules.  

(defun this-host-p (addr)
  (or (null (emailaddr-domain addr))
      (member (emailaddr-domain addr)
	      (append (list (short-host-name) (fqdn)) *host-aliases*)
	      :test #'equalp)))

(defun should-masquerade-p (addr)
  (and 
   *masquerade-as* 
   (this-host-p addr) 
   (not (member (emailaddr-user addr) *exposed-users* :test #'equalp))))

;; smtp

(defun rewrite-smtp-canonicalize (addr)
  (block nil
    (if (emailnullp addr)
	(return addr))
    
    (if (should-masquerade-p addr)
	(setf addr
	  (parse-email-addr
	   (concatenate 'string (emailaddr-user addr) "@" *masquerade-as*))))
    ;; Tack on our fqdn if we're still domainless.
    (if (null (emailaddr-domain addr))
	(setf addr 
	  (parse-email-addr
	   (concatenate 'string (emailaddr-user addr) "@" (fqdn)))))
    addr))
    

(defun rewrite-smtp-envelope-sender (addr)
  (rewrite-smtp-canonicalize addr))

(defun rewrite-smtp-header-sender (addr)
  (rewrite-smtp-canonicalize addr))

(defun rewrite-smtp-envelope-recip (addr)
  (rewrite-smtp-canonicalize addr))

(defun rewrite-smtp-header-recip (addr)
  (rewrite-smtp-canonicalize addr))


;; local 

;; Rewrite senders/recips during local delivery
(defun rewrite-local-sender-or-recip (addr)
  (block nil
    (if (null *strip-domain-for-local-delivery*)
	(return addr))
    (if (string= (emailaddr-domain addr) *strip-domain-for-local-delivery*)
	(parse-email-addr (emailaddr-user addr))
      addr)))

(defun rewrite-local-envelope-sender (addr)
  (rewrite-local-sender-or-recip addr))

(defun rewrite-local-header-sender (addr)
  (rewrite-local-sender-or-recip addr))

;; Just use the user part.
(defun rewrite-local-envelope-recip (addr)
  (parse-email-addr (emailaddr-user addr)))

(defun rewrite-local-header-recip (addr)
  (rewrite-local-sender-or-recip addr))
 


;;; To, From, Cc.
;;; removes Bcc.
(defun rewrite-headers (headers type)
  ;; Bcc is always removed from the headers, regardless of the rewrite
  ;; type.
  (setf headers (remove-header "Bcc:" headers))
  
  (if (eq type :norewrite)
      (return-from rewrite-headers headers))
  (let (senderfunc recipfunc)
    (ecase type
      (:local
       (setf senderfunc #'rewrite-local-header-sender)
       (setf recipfunc #'rewrite-local-header-recip))
      (:smtp
       (setf senderfunc #'rewrite-smtp-header-sender)
       (setf recipfunc #'rewrite-smtp-header-recip)))
    
    (mapcar #'(lambda (h)
		(cond
		 ((recip-header-p h)
		  (rewrite-header h recipfunc))
		 ((sender-header-p h)
		  (rewrite-header h senderfunc))
		 (t
		  h))) 
	    headers)))

(defun rewrite-header (string rewritefunc)
  (let ((colonpos (position #\: string)))
    (if (null colonpos)
	(error "Invalid header passed to rewrite-header"))
    (incf colonpos)
    (let ((rewritten (rewrite-header-help string rewritefunc :pos colonpos)))
      (if rewritten
	  (concatenate 'string (subseq string 0 colonpos) rewritten)
	string))))

		 
(defun rewrite-header-help (string rewritefunc &key (pos 0) 
						    (max (length string)))
  (block nil
    (let ((addrlist (parse-address-list 
		     (emailaddr-lex string :pos pos :max max))))
      (if (null addrlist)
	  (return nil))
      (rewrite-addrspec addrlist rewritefunc)
      (with-output-to-string (s) 
	(print-address-list addrlist s)))))

(defun rewrite-addrspec (thing rewritefunc)
  (cond
   ((addrspec-p thing)
    (let ((ea (make-emailaddr :user (addrspec-user thing)
			      :domain (addrspec-domain thing))))
      (setf ea (funcall rewritefunc ea))
      (setf (addrspec-user thing) (emailaddr-user ea))
      (setf (addrspec-domain thing) (emailaddr-domain ea))))
   ((listp thing)
    (dolist (item thing)
      (rewrite-addrspec item rewritefunc)))))
    
    
