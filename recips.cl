(in-package :user)

;;; Functions to locate/categorize recipients

(defun local-domain-p (address)
  (let ((domain (emailaddr-domain address)))
    (or (null domain) 
	(member domain 
		(append (list (short-host-name) (fqdn)) 
			*host-aliases*
			*localdomains*)
		:test #'equalp))))
  
;; Domain part is assumed to have been checked already.
(defun lookup-recip-in-passwd (address)
  (getpwnam (string-downcase (emailaddr-user address))))


(defun get-recipient-disposition (addr)
  (block nil
    (if (not (local-domain-p addr))
	(return :non-local))
    ;; Check for blacklisted recipients listed in the aliases file first.
    (multiple-value-bind (found errmsg)
	(lookup-recip-in-aliases addr)
      (if (eq found :error)
	  (return (values :error (if errmsg errmsg "Invalid user")))))
    ;; Run other checkers 
    (dolist (func *local-recip-checks* :local-unknown)
      (multiple-value-bind (found errmsg)
	  (funcall func addr)
	(if (eq found :error)
	    (return (values :error (if errmsg errmsg "Invalid user"))))
	(if found
	    (return :local-ok))))))

;;; This is an attempt to improve on the virtusertable idea from
;;; sendmail.  virtusertable is simply another less-functional aliases
;;; file... so I'm expanding the function of the aliases file to work
;;; like virtusertable.  The aliases are first checked using the fully
;;; qualified address (assuming the sender specified one).  If that
;;; finds something, it'll be used.  Otherwise, a lookup is done w/o
;;; the domain portion.  This should be acceptable because this
;;; function is only called when we've determined that a "local"
;;; domain name was specified.

(defun lookup-recip-in-aliases (address &key parsed)
  (multiple-value-bind (exp errmsg)
      (lookup-recip-in-aliases-help address :parsed parsed)
    (if exp
	(values exp errmsg)
      (multiple-value-bind (exp errmsg)
	  (lookup-recip-in-aliases-help (emailaddr-user address) 
					:parsed parsed)
	(values exp errmsg)))))
    
(defun lookup-recip-in-aliases-help (addr &key parsed)
  (block nil
    (let ((exp (expand-alias addr :parsed parsed)))
      (if (/= (length exp) 1)
	  (return exp))
      (let ((exp2 (first exp)))
	(if parsed
	    (setf exp2 (emailaddr-orig exp2)))
	(if (prefix-of-p ":error:" exp2)
	    (return (values :error
			    (subseq exp2 #.(length ":error:")))))
	(return exp)))))
      
      
	
	
    
    
    
    
