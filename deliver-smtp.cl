(in-package :user)

;;; XXX -- all calls to outline need to be wrapped in a timeout.

(defun deliver-smtp (recip q)
  (block nil
    (let ((buf (make-string *maxlinelen*))
	  (domain (emailaddr-domain recip))
	  errmsg)
      (multiple-value-bind (sock mxname status)
	  (connect-to-mx domain)
	(when (eq status :no-such-domain)
	  (setf errmsg (format nil "~A: Host not found" domain))
	  (maild-log "~A" errmsg)
	  (return (values :fail errmsg)))
	(when (null sock)
	  (setf errmsg 
	    (format nil 
		    "Failed to connect to any mail exchanger for domain ~A"
		    domain))
	  (maild-log "~A" errmsg)
	  (return (values :transient errmsg)))
	(unwind-protect
	    (let ((sender (emailaddr-orig (rewrite-smtp-envelope-sender
					   (queue-from q)))))
	      (multiple-value-bind (res response)
		  (get-smtp-greeting sock buf mxname)
		(if (not (eq res :ok))
		    (return (values res response))))
	      (multiple-value-bind (res response)
		  (smtp-say-hello sock buf mxname)
		(if (not (eq res :ok))
		    (return (values res response))))
	      (multiple-value-bind (res response)
		  (smtp-send-mail-from sock buf mxname sender)
		(if (not (eq res :ok))
		    (return (values res response))))

	      ;; when this is improved, there may be more than one
	      ;; recipient..  As long as one of them is accepted, proceed
	      ;; w/ the transaction. Bounces for the others will need
	      ;; to be handled.
	      (multiple-value-bind (res response)
		  (smtp-send-rcpt-to sock buf mxname (emailaddr-orig recip))
		(if (not (eq res :ok))
		    (return (values res response))))
	   
	      (multiple-value-bind (res response)
		  (smtp-send-data sock buf mxname q)
		(if (not (eq res :ok))
		    (return (values res response))))
	      
	      (maild-log "Successful SMTP delivery to ~A (mx: ~A)" 
			 (emailaddr-orig recip)
			 mxname)
	      :delivered)
	  ;; cleanup forms
	  (if sock 
	      (ignore-errors
	       ;; try to be polite
	       (smtp-finish sock buf mxname)
	       (close sock))))))))

(defun smtp-finish (sock buf mxname)
  (outline sock "QUIT")
  (get-smtp-reply sock buf *quittimeout* mxname))
  
(defun smtp-send-data (sock buf mxname q)
  (block nil
    (outline sock "DATA")
    (multiple-value-bind (status line)
	(get-smtp-reply sock buf *datainitiationtimeout* mxname)
      (if (not (eq status :intermediate-ok))
	  (return (values status line))))
    (handler-case (write-message-to-stream sock q :smtp :smtp t)
      (error (c)
	(let ((report 
	       (format nil "Error ~A while sending message data to ~A" 
		       c mxname)))
	  (maild-log "~A" report)
	  (return (values :transient report)))))
    (outline sock ".")
    (get-smtp-reply sock buf *dataterminationtimeout* mxname)))

(defun smtp-send-rcpt-to (sock buf mxname to)
  (if (string= to "<>")
      (setf to ""))
  (outline sock "RCPT TO:<~A>" to)
  (get-smtp-reply sock buf *rcptcmdtimeout* mxname))

(defun smtp-send-mail-from (sock buf mxname sender)
  (if (string= sender "<>")
      (setf sender ""))
  (outline sock "MAIL FROM:<~A>" sender)
  (get-smtp-reply sock buf *mailcmdtimeout* mxname))

(defun smtp-say-hello (sock buf mxname)
  (outline sock "HELO ~A" (fqdn))
  (get-smtp-greeting sock buf mxname))

(defun get-smtp-greeting (sock buf mxname)
  (get-smtp-reply sock buf *greetingtimeout* mxname))

(defun get-smtp-reply (sock buf timeout mxname)
  (let (line char)
    (loop
      (setf line (smtp-get-line sock buf timeout))
      (cond
       ((eq line :longline)
	(maild-log "Got excessively long response from ~A while reading the SMTP greeting" mxname)
	(return (values :transient line)))
       ((eq line :eof)
	(maild-log "Remote SMTP server ~A hung up on us" mxname)
	(return (values :transient "<disconnect>")))
       ((eq line :timeout)
	(maild-log "Timeout while waiting for SMTP greeting from ~A" mxname)
	(return (values :transient "<timeout>")))
       ((stringp line)
	nil)
       (t
	(error "Unexpected return value from smtp-get-line: ~S" line)))
      (if* (< (length line) 4)
	 then
	      (maild-log "Got short reply line from ~A: ~A" mxname line)
	      (return (values :transient line)))
      (setf char (schar line 3))
      (case char
	(#\-  ;; line will be continued.. ignore
	 nil) 
	(#\space ;; final reply line.  Use this for processing the code
	 (return (smtp-response-disposition line mxname)))
	(t
	 (maild-log "Got weird reply line from ~A: ~A" mxname line)
	 (return (values :transient line)))))))
	 
(defun smtp-response-disposition (line mxname)
  (block nil
    (multiple-value-bind (found whole code)
	(match-regexp "^\\([0-9][0-9][0-9]\\) " line)
      (declare (ignore whole))
      (if* (not found)
	 then
	      (maild-log "Got weird reply line from ~A: ~A" mxname line)
	      (return (values :transient line)))
      (case (schar code 0)
	(#\2
	 (return (values :ok line)))
	(#\3
	 (return (values :intermediate-ok line)))
	(#\4
	 (return (values :transient line)))
	(#\5
	 (return (values :fail line)))
	(t
	 (maild-log "Unexpected response code from ~A: ~A" mxname line)
	 (return (values :transient line)))))))


(defun connect-to-mx (domain)
  (block nil
    (let ((mxs (get-good-mxs domain)))
      (if (eq mxs :no-such-domain)
	  (return (values nil nil :no-such-domain)))
      (dolist (mx mxs)
	(let (sock)
	  (handler-case 
	      (setf sock (make-socket :remote-host (car mx) 
				      :remote-port *smtp-port*))
	    (error (c)
	      (maild-log "Error ~A while connecting to ~A" c (cdr mx))
	      nil))
	  (if sock
	      (return (values sock (cdr mx) :ok))))))))

;; return a list of (addr . name)
(defun get-good-mxs (domain)
  (let (res)
    (multiple-value-bind (best ttl others)
	(useful-dns-query domain :type :mx)
      (declare (ignore ttl))
      (if (eq best :no-such-domain)
	  (return-from get-good-mxs :no-such-domain))
      (let ((mxs (cons best others)))
	(if (null best)  ;; use the A record if there's no MX record
	    (setf mxs (list (list domain nil 0))))
	(dolist (mx mxs)
	  (complete-mx-info mx)
	  (if (second mx)
	      (push (cons (second mx) (first mx)) res)))))
    (nreverse res))) ;; order matters

;; do a name lookup if (second info) is nil
;; XXX -- this is bogus because a host could have multiple addresses and
;; lookup-hostname only returns one.
(defun complete-mx-info (info)
  (when (null (second info))
    (setf (second info) (ignore-errors (lookup-hostname (first info))))
    (if (null (second info))
	(maild-log "Couldn't get address of MX ~A" (first info)))))


	
	
	