(in-package :user)

;; XXX --  Need a check to make sure that we never try to connect to
;; ourself.

(defun deliver-smtp (recip q &key verbose)
  (block nil
    (let ((buf (make-string *maxlinelen*))
	  (domain (emailaddr-domain (recip-addr recip)))
	  (sender (emailaddr-orig 
		   (rewrite-smtp-envelope-sender (recip-owner recip))))
	  (recip-printable (emailaddr-orig (recip-addr recip)))
	  errmsg)
      (multiple-value-bind (sock mxname status)
	  (connect-to-mx domain :verbose verbose)
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
	;; Socket ready.
	(unwind-protect
	    (with-socket-timeout (sock :write *datatimeout*)
	      (multiple-value-bind (res response)
		  (get-smtp-greeting sock buf mxname :verbose verbose)
		(if (not (eq res :ok))
		    (return (values res response))))
	      (multiple-value-bind (res response)
		  (smtp-say-hello sock buf mxname :verbose verbose)
		(if (not (eq res :ok))
		    (return (values res response))))
	      (multiple-value-bind (res response)
		  (smtp-send-mail-from sock buf mxname sender :verbose verbose)
		(if (not (eq res :ok))
		    (return (values res response))))

	      ;; when this is improved, there may be more than one
	      ;; recipient..  As long as one of them is accepted, proceed
	      ;; w/ the transaction. Bounces for the others will need
	      ;; to be handled.
	      (multiple-value-bind (res response)
		  (smtp-send-rcpt-to sock buf mxname recip-printable
				     :verbose verbose)
		(if (not (eq res :ok))
		    (return (values res response))))
	   
	      (multiple-value-bind (res response)
		  (smtp-send-data sock buf mxname q :verbose verbose)
		(if (not (eq res :ok))
		    (return (values res response))))
	      
	      (maild-log "Successful SMTP delivery to ~A (mx: ~A)" 
			 recip-printable mxname)
	      :delivered)
	  ;; cleanup forms
	  (if sock 
	      (ignore-errors
	       ;; try to be polite.  But don't wait too long
	       (mp:with-timeout (30)
		 (smtp-finish sock buf mxname :verbose verbose))
	       (close sock :abort t))))))))

(defun smtp-finish (sock buf mxname &key verbose)
  (if verbose
      (format t ">>> QUIT~%"))
  (outline sock "QUIT")
  (get-smtp-reply sock buf *quittimeout* mxname :verbose verbose))
  
(defun smtp-send-data (sock buf mxname q &key verbose)
  (block nil
    (if verbose
	(format t ">>> DATA~%"))
    (outline sock "DATA")
    (multiple-value-bind (status line)
	(get-smtp-reply sock buf *datainitiationtimeout* mxname 
			:verbose verbose)
      (if (not (eq status :intermediate-ok))
	  (return (values status line))))
    (handler-case (write-message-to-stream sock q :smtp :smtp t)
      (error (c)
	(let (report)
	  (if (and (eq (type-of c) 'socket-error)
		   (eq (stream-error-identifier c) :write-timeout))
	      (setf report
		(format nil "Message transmission stalled while talking to ~A"
			mxname))
	    (setf report
	      (format nil "Error ~A while sending message data to ~A" 
		      c mxname)))
	  (maild-log "~A" report)
	  (return (values :transient report)))))
    (if verbose
	(format t ">>> .~%"))
    (outline sock ".")
    (get-smtp-reply sock buf *dataterminationtimeout* mxname 
		    :verbose verbose)))

(defun smtp-send-rcpt-to (sock buf mxname to &key verbose)
  (if (string= to "<>")
      (setf to ""))
  (let ((string (format nil "RCPT TO:<~A>" to)))
    (if verbose 
	(format t ">>> ~A~%" string))
    (outline sock "~A" string))
  (get-smtp-reply sock buf *rcptcmdtimeout* mxname :verbose verbose))

(defun smtp-send-mail-from (sock buf mxname sender &key verbose)
  (if (string= sender "<>")
      (setf sender ""))
  (let ((string (format nil "MAIL FROM:<~A>" sender)))
    (if verbose 
	(format t ">>> ~A~%" string))
    (outline sock "~A" string))
  (get-smtp-reply sock buf *mailcmdtimeout* mxname :verbose verbose))

(defun smtp-say-hello (sock buf mxname &key verbose)
  (let ((string (format nil "HELO ~A" (fqdn))))
    (if verbose
	(format t ">>> ~A~%" string))
    (outline sock "~A" string))
  (get-smtp-greeting sock buf mxname :verbose verbose))

(defun get-smtp-greeting (sock buf mxname &key verbose)
  (get-smtp-reply sock buf *greetingtimeout* mxname :verbose verbose))

(defun get-smtp-reply (sock buf timeout mxname &key verbose)
  (let (line char)
    (loop
      (setf line (smtp-get-line sock buf timeout))
      (if verbose
	  (format t "~A~%" line))
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


(defun connect-to-mx (domain &key verbose)
  (block nil
    (let ((mxs (get-good-mxs domain)))
      (if (eq mxs :no-such-domain)
	  (return (values nil nil :no-such-domain)))
      (dolist (mx mxs)
	(let (sock)
	  (handler-case 
	      (progn
		(if verbose
		    (format t "Connecting to ~A...~%" (cdr mx)))
		(setf sock (make-socket :type :hiper
					:remote-host (car mx) 
					:remote-port *smtp-port*)))
	    (error (c)
	      (if verbose
		  (format t "..failed: ~A~%" c))
	      (maild-log "Error ~A while connecting to ~A" c (cdr mx))
	      nil))
	  (if* sock
	     then
		  (if verbose
		      (format t "..connected.~%"))
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


	
	
	