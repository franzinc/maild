(in-package :user)

;;; Functions to locate/categorize recipients

(defstruct recip
  type ;; :prog, :file, :error, :bad, :include   nil means normal
  orig ;; unparsed
  addr ;; parsed
  file ;; for :file and :prog recips
  prog-user ;; for :prog recips
  errmsg ;; for :error and :bad recips
  escaped ;; don't expand any further
  expanded-from ;; string  (information only)
  owner ;; nil means just use the original envelope sender
  status) 

(defun error-recip-p (r)
  (eq (recip-type r) :error))

(defun bad-recip-p (r)
  (eq (recip-type r) :bad))

(defun include-recip-p (r)
  (eq (recip-type r) :include))

(defun file-recip-p (r)
  (eq (recip-type r) :file))

(defun prog-recip-p (r)
  (eq (recip-type r) :prog))


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
	(:include
	 (format nil ":include:~A" (recip-file r)))
	(:error
	 (format nil ":error:~A" (recip-errmsg r)))
	(:bad
	 (format nil ":bad:~A" (recip-orig r)))))))
  

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

;; accepts non-special recip structs as arg as well
(defun get-recipient-disposition (addr)
  (block nil
    (if (recip-p addr)
	(if (recip-type addr)
	    (error "get-recipient-disposition doesn't work on special recips")
	  (setf addr (recip-addr addr))))
	
    (if (not (local-domain-p addr))
	(return :non-local))
    (let (recips)
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

(defun lookup-recip-in-aliases (orig-address)
  (let ((address (if (stringp orig-address)
		     (parse-email-addr orig-address)
		   orig-address)))
    (if (null address)
	(error "lookup-recip-in-aliases: Invalid address: ~S" orig-address))
    (expand-alias address)))

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

;;; recipient parsing stuff.  

;; Works with specials, atoms, and quoted strings.
(defun tokens-begin-with-p (char tokens)
  (let ((token (first tokens)))
    (when tokens
      (cond
       ((atom-token-p token)
	(char= (schar (second token) 0) char))
       ((quoted-string-token-p token)
	(char= (schar (second token) 1) char))
       ((some-special-token-p token char)
	t)
       (t
	nil)))))

(defun parse-recip (tokens)
  (if (stringp tokens)
      (setf tokens (emailaddr-lex tokens)))
  (let* ((string (tokens-to-string tokens :strip-trailing-white t))
	 (recip (make-recip :orig string)))
    (cond
     ;; file recips
     ((tokens-begin-with-p #\/ tokens)
      (setf (recip-type recip) :file)
      (setf (recip-file recip) string))

     ;; include recips
     ((prefix-of-p ":include:" string)
      (setf (recip-type recip) :include)
      (setf (recip-file recip) (subseq string #.(length ":include:"))))
       
     ;; error recips
     ((prefix-of-p ":error:" string)
      (setf (recip-type recip) :error)
      (setf (recip-errmsg recip) (subseq string #.(length ":error:")))
      (if (string= (recip-errmsg recip) "")
	  (setf (recip-errmsg recip) "Invalid user")))
       
     ;; program recips
     ((tokens-begin-with-p #\| tokens)
      (setf (recip-type recip) :prog)
      (multiple-value-bind (found whole prog-user prog)
	  (match-regexp "^|(\\([^)]+\\))\\(.*\\)" string)
	(declare (ignore whole))
	(if* found
	   then
		(setf (recip-file recip) prog)
		(setf (recip-prog-user recip) prog-user)
	   else
		(setf (recip-file recip) (subseq string 1)))))
     
     ;; regular email recip
     (t 
      (when (tokens-begin-with-p #\\ tokens)
	(pop tokens)
	(setf (recip-escaped recip) t))
      
      
      (setf tokens (nreverse tokens))
      
      ;; dump trailing whitespace (for next test)
      (while (whitespace-token-p (first tokens))
	     (pop tokens))

      ;; gross hacks to make "group" mailboxes work.
      (when (some-special-token-p (first tokens) #\;)
	(pop tokens))
      
      (setf tokens (nreverse tokens))

      (when (member-if #'(lambda (tok) (some-special-token-p tok #\:)) tokens)
	;; disregard everything up to (and including) the colon
	(while (and tokens (not (some-special-token-p (first tokens) #\:)))
	       (pop tokens))
	(pop tokens)) ;; pop the colon
      
      (multiple-value-bind (addrspec remaining-tokens)
	  (parse-address tokens)
	(if* (or remaining-tokens (null addrspec) 
		 (not (eq (first addrspec) :address)))
	   then
		(setf (recip-type recip) :bad)
		(setf (recip-errmsg recip) "Invalid address")
	   else
		;; good address.
		(setf (recip-addr recip)
		  (mailbox-to-emailaddr (second addrspec)))))))
      recip))


(defun parse-recip-list (input &key (pos 0) allow-null)
  (block nil
    (setf input (emailaddr-lex input :pos pos))
    (let ((tokens-lists (sep-tokens-by-comma input))
	  recips)
      (dolist (tokens tokens-lists)
	;; trim leading whitespace
	(if (whitespace-token-p (first tokens))
	    (pop tokens))
	(let* ((recip (parse-recip tokens))
	       (addr (if (null (recip-type recip)) (recip-addr recip))))
	  (cond
	   ((recip-type recip) ;; other special types
	    ) ;; no checks
	   
	   ;; check for <>
	   ((and (null (emailaddr-user addr)) (null (emailaddr-domain addr)) 
		 (not allow-null))
	    (setf (recip-type recip) :bad)
	    (setf (recip-errmsg recip) "Invalid address"))
	   
	   ;; check for @domain.com
	   ((and (null (emailaddr-user addr)) (emailaddr-domain addr))
	    (setf (recip-type recip) :bad)
	    (setf (recip-errmsg recip) "User address required")))
	  (push recip recips)))
      recips)))

;; for messages coming in on stdin
(defun get-good-recips-from-string (string &key (pos 0) verbose)
  (let (good-recips)
    (dolist (recip (parse-recip-list string :pos pos))
      (cond
       ((bad-recip-p recip)
	(format t "~A... ~A~%" (recip-orig recip) (recip-errmsg recip)))
       ((include-recip-p recip)
	(format t "~A... Cannot mail directly to :include:s~%" 
		(recip-orig recip)))
       ((error-recip-p recip)
	(format t "~A... Cannot mail directly to :error:s~%" 
		(recip-orig recip)))
       ((file-recip-p recip)
	(format t "~A... Cannot mail directly to files~%" 
		(recip-orig recip)))
       ((error-recip-p recip)
	(format t "~A... Cannot mail directly to programs~%" 
		(recip-orig recip)))
       (t
	(multiple-value-bind (disp msg)
	    (get-recipient-disposition recip)
	  (ecase disp
	    ((:non-local :local-ok)
	     (if verbose
		 (format t "~A... OK~%" (recip-orig recip)))
	     (push recip good-recips)) ;; accepted
	    (:error 
	     (format t "~A... ~A~%" (recip-orig recip) msg))
	    (:local-unknown
	     (format t "~A... User unknown~%" (recip-orig recip))))))))
    good-recips))
