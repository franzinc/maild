(in-package :user)

;;; Functions to locate/categorize recipients

;; see aliases.cl for alias-rhs and alias-exp info as well.
(defstruct recip
  type ;; :prog, :file, :error.  nil means normal
  addr ;; parsed
  file ;; for :file and :prog recips
  prog-user ;; for :prog recips
  errmsg ;; for :error recips
  expanded-from ;; string  (information only)
  owner ;; nil means just use the original envelope sender
  status) 

(defun recip-printable (r)
  (let ((type (recip-type r)))
    (if (null type)
	(emailaddr-orig (recip-addr r))
      (ecase type
	(:prog
	    (with-output-to-string (s)
	      (write-char #\| s)
	      (if (recip-prog-user r)
		  (format s "(~A)" (recip-prog-user r)))
	      (write-string (recip-file r) s)))
	(:file
	 (recip-file r))
	(:error
	 (format nil "\":error:~A\"" (recip-errmsg r)))))))
  

(defun local-domain-p (address)
  (let ((domain (emailaddr-domain address)))
    (or (null domain) 
	(member domain 
		(append (list (short-host-name) (fqdn)) 
			*host-aliases*
			*localdomains*)
		:test #'equalp))))
  
;; Domain part is assumed to have been checked already.
(defun lookup-recip-in-passwd (orig-address)
  (let ((address (if (stringp orig-address)
		     (parse-email-addr orig-address)
		   orig-address)))
    (if (null address)
	(error "lookup-recip-in-passwd: Invalid address: ~A" orig-address))
    (let ((pwent (getpwnam (string-downcase (emailaddr-user address)))))
      (if (null pwent)
	  nil
	(list (make-recip :addr address))))))

(defun get-recipient-disposition (orig-address)
  (block nil
    (let ((addr (if (stringp orig-address)
		    (parse-email-addr orig-address)
		  orig-address))
	  recips)
      (if (null addr)
	  (error "get-recipient-disposition: Invalid address: ~S" orig-address))
      (if (not (local-domain-p addr))
	  (return :non-local))
      (dolist (func *local-recip-checks* :local-unknown)
	(setf recips (funcall func addr))
	(if (and recips (= (length recips) 1) 
		 (eq (recip-type (first recips)) :error))
	    (return
	      (values
	       :error
	       (if (recip-errmsg (first recips)) 
		   (recip-errmsg (first recips))
		 "Invalid user"))))
	(if recips
	    (return :local-ok))))))

(defun alias-exp-to-recip (exp)
  (let ((rhs (alias-exp-rhs exp)))
    (make-recip :type (alias-rhs-type rhs)
		:addr (alias-rhs-addr rhs)
		:file (alias-rhs-file rhs)
		:prog-user (alias-rhs-prog-user rhs)
		:errmsg (alias-rhs-errmsg rhs)
		:expanded-from (alias-rhs-expanded-from rhs)
		:owner (alias-exp-owner exp))))

(defmacro alias-exps-to-recips (exps)
  `(mapcar #'alias-exp-to-recip ,exps))

(defun lookup-recip-in-aliases (orig-address)
  (let ((address (if (stringp orig-address)
		     (parse-email-addr orig-address)
		   orig-address)))
    (if (null address)
	(error "lookup-recip-in-aliases: Invalid address: ~S" orig-address))
    (let ((exp (expand-alias address)))
      (if (null exp)
	  nil
	(alias-exps-to-recips exp)))))

(defmacro mailing-list-p (exp)
  `(> (length ,exp) 1))

;; returns a list of recip structs w/ no duplicates.
(defun expand-addresses (addrs sender)
  (let ((exclude-sender t)
	(sender-exp (lookup-recip-in-aliases sender)))
    (cond 
     ((mailing-list-p sender-exp)
      (setf exclude-sender nil))
     ((null sender-exp)
      (setf sender-exp sender))
     ((recip-type (first sender-exp)) ;; prog/file/whatnot expansion
      (setf exclude-sender nil))
     (t
      (setf sender-exp (first sender-exp))))
    
    (let (recips)
      (dolist (addr addrs)
	(if (not (local-domain-p addr))
	    ;; don't bother trying to expand non-local recips.
	    (push (make-recip :addr addr) recips)
	  ;; else
	  (let ((expansion (lookup-recip-in-aliases addr)))
	    (if* expansion
	       then
		    (when (and (mailing-list-p expansion)
			       exclude-sender
			       (member sender-exp expansion
				       :test #'same-recip-p))
		      (if *debug*
			  (maild-log "Removing ~A from expansion of ~A"
				     (recip-printable sender-exp)
				     (emailaddr-orig addr)))
		      (setf expansion (delete sender-exp expansion
					   :test #'same-recip-p)))
		    (setf recips (nconc recips expansion))
	       else
		    (push (make-recip :addr addr) recips)))))
      (delete-duplicates recips :test #'same-recip-p))))

;; Should be called on post-expansion recip structs.
(defun same-recip-p (recip1 recip2)
  (block nil
    (let ((type1 (recip-type recip1))
	  (type2 (recip-type recip2)))
      (if (not (eq type1 type2))
	  (return nil))
      (when (null type1) ;; regular recips
	(let* ((addr1 (recip-addr recip1))
	       (addr2 (recip-addr recip2))
	       (local1 (local-domain-p addr1))
	       (local2 (local-domain-p addr2)))
	  ;; If the user parts don't match, definite non-match
	  (if (not (equalp (emailaddr-user addr1) (emailaddr-user addr2)))
	      (return nil))
	  ;; If they're both local, then we have a match.
	  (if (and local1 local2)
	      (return t))
	  ;; If both are non-local, compare the domain parts
	  (if (and (not local1) (not local2))
	      (return (equalp (emailaddr-domain addr1) 
			      (emailaddr-domain addr2))))
	  ;; One local, one non-local.  Definite non-match
	  (return nil)))
      ;; other recip types
      (ecase type1
	(:prog
	    (and
	     (string= (recip-file recip1) (recip-file recip2))
	     (equalp (recip-prog-user recip1) (recip-prog-user recip2))))
	(:file
	 (string= (recip-file recip1) (recip-file recip2)))))))
  


