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
  owner) ;; nil means just use the original envelope sender

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

;; this stuff below needs to be updated to use the new stuff from
;; aliases.cl

(defun lookup-recip-in-aliases (orig-address &key parsed)
  (let ((address (if (stringp orig-address)
		     (parse-email-addr orig-address)
		   orig-address)))
    (if (null address)
	(error "lookup-recip-in-aliases: Invalid address: ~S" orig-address))
    (multiple-value-bind (exp errmsg)
	(lookup-recip-in-aliases-help address :parsed parsed)
      (if exp
	  (values exp errmsg)
	(multiple-value-bind (exp errmsg)
	    (lookup-recip-in-aliases-help (emailaddr-user address) 
					  :parsed parsed)
	  (values exp errmsg))))))
    
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

;; Takes a parsed email addr and returns a list of recip structs.

(defun expand-recip (addr expanded-from)
  (block nil
    (if (not (local-domain-p addr))
	(return (list (make-recip :addr addr :owner owner))))
    (let ((expansion (lookup-recip-in-aliases addr :parsed t))
	  res)
      (if (null expansion)
	  (return (list (make-recip :addr addr :owner owner)))) 
      (dolist (exp expansion)
	(if (or (not (local-domain-p exp))
		(equalp (emailaddr-user recip) (emailaddr-user exp)))
	    (push 
	     exp
		  
		  res)
	  (setf res (append res (expand-recip exp)))))
      res)))

;; Duplicate-free
(defun expand-recips (recips)
  (let (res)
    (dolist (recip recips)
      (setf res (append res (expand-recip recip))))
    (dedupe-recips res)))

(defun dedupe-recips (recips)
  (let (seen res)
    (dolist (recip recips)
      (if* (not (member recip seen :test #'emailaddr=))
	 then
	      (push recip res)
	      (push recip seen)))
    res))
