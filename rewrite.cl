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
    
;;;; stuff user may want to modify
;;;;

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

;; end user modifiable area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns values: senderfunc recipfunc
(defun get-rewrite-funcs (type)
  (ecase type
    (:local
     (values #'rewrite-local-header-sender #'rewrite-local-header-recip))
    (:smtp
     (values #'rewrite-smtp-header-sender #'rewrite-smtp-header-recip))))

;;; To, From, Cc.
;;; removes Bcc.
(defun rewrite-headers (headers type)
  ;; Bcc is always removed from the headers, regardless of the rewrite
  ;; type.
  (setf headers (remove-header "Bcc:" headers))
  
  (if (eq type :norewrite)
      (return-from rewrite-headers headers))
  (multiple-value-bind (senderfunc recipfunc)
      (get-rewrite-funcs type)
    (let (newheaders h nextline)
      (while headers
	     (setf h (pop headers))
	     (when (or (recip-header-p h) (sender-header-p h))
	       (while (setf nextline (pop headers))
		      (if (or (= 0 (length nextline))
			      (not (whitespace-p (schar nextline 0))))
			  (return)) ;; break
		      (setf h (header-unfold h nextline)))
	       ;; get here if nextline wasn't there.. or if it was
	       ;; the beginning of a new header
	       (if nextline 
		   (push nextline headers))
	       
	       ;; We now have an unfolded header.  Process it.
	       (cond
		((recip-header-p h)
		 (rewrite-header h recipfunc))
		((sender-header-p h)
		 (rewrite-header h senderfunc))
		(t
		 (error "This should never happen")))
	       
	       ;; returns a list of lines.  push them onto 
	       
	       (let ((newlines (header-fold h)))
		 (while (> (length newlines) 1)
			(push (pop newlines) newheaders))
		 (setf h (pop newlines))))
	     
	     (push h newheaders))
      (nreverse newheaders))))
	       
	       

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
    (multiple-value-bind (addrlist remainder)
	(parse-address-list (emailaddr-lex string :pos pos :max max))
      (if (null addrlist)
	  (return nil)) ;; indicate no rewrite done
      ;; sanity check
      (if (not (eq (first addrlist) :address-list))
	  (error "rewrite-header-help: Expected an :address-list!"))
      (dolist (addr (second addrlist))
	(rewrite-addrlist-entry addr rewritefunc))
      (with-output-to-string (s) 
	(print-address-list addrlist s)
	(print-token-list remainder s)))))


(defun rewrite-addrlist-entry (thing rewritefunc)
  ;; sanity check
  (if (or (not (listp thing))
	  (not (eq (first thing) :address)))
      (error "rewrite-addrlist-entry: Got something other than :address"))
  (setf thing (second thing))
  (cond
   ((groupspec-p thing)
    (dolist (mb (second (groupspec-mailbox-list thing)))
      (rewrite-mailbox mb rewritefunc)))
   ((eq (first thing) :mailbox)
    (rewrite-mailbox thing rewritefunc))
   (t
    (error "rewrite-addrlist-entry: Unexpected data: ~S" thing))))

;; modifies addrspec in 'as'
(defun rewrite-addrspec (as rewritefunc)
  ;; sanity check
  (if (not (addrspec-p as))
      (error "non-addrspec passwd to rewrite-addrspec"))
  (let ((ea (make-emailaddr :user (addrspec-user as)
			    :domain (addrspec-domain as))))
    (setf ea (funcall rewritefunc ea))
    (setf (addrspec-user as) (emailaddr-user ea))
    (setf (addrspec-domain as) (emailaddr-domain ea))))

(defun rewrite-mailbox (mb rewritefunc)
  (if (not (eq (first mb) :mailbox))
      (error "non-mailbox ~S passed to rewrite-mailbox" mb))
  (cond 
   ((nameaddr-p (second mb))
    (rewrite-addrspec (angle-addr-to-addrspec (nameaddr-angleaddr (second mb)))
		      rewritefunc))
   ((addrspec-p (second mb))
    (rewrite-addrspec (second mb) rewritefunc))
   (t
    (error "rewrite-mailbox: Unexpected data: ~S~%" mb))))

