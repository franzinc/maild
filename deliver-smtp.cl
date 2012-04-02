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
;; $Id: deliver-smtp.cl,v 1.26 2007/05/18 16:21:56 dancy Exp $

(in-package :user)

;; XXX --  Need a check to make sure that we never try to connect to
;; ourself.


;; Makes a list like:
;; ((domain1 (recip1 recip2)) (domain2 (recip3 recip4)) ...)
(defun group-smtp-recips-by-domain (recips)
  (let (domains)
    (dolist (recip recips)
      (let* ((domain (emailaddr-domain (recip-addr recip)))
	     (group (find domain domains :key #'car :test #'equalp)))
	(if (null group)
	    (push (list domain (list recip)) domains)
	  (push recip (second group)))))
    domains))

(defstruct smtp-delivery
  unprocessed-recips
  in-process-recips
  good-recips
  failed-recips
  transient-recips)

;; Caller determines which recips are smtp recips.
(defun deliver-smtp (deliv q &key verbose)
  (dolist (domaingroup (group-smtp-recips-by-domain 
			(smtp-delivery-unprocessed-recips deliv)))
    (let ((domain (first domaingroup))
	  (recips (second domaingroup)))
      (dolist (ownergroup (group-recips-by-owner recips))
	(let* ((sender (first ownergroup))
	       (recips (second ownergroup))
	       (newdeliv (make-smtp-delivery :unprocessed-recips recips)))
	  (multiple-value-bind (status msg)
	      (deliver-smtp-help domain sender newdeliv q :verbose verbose)
	    (ecase status
	      (:transient
	       (dolist (recip (smtp-delivery-unprocessed-recips newdeliv))
		 (push (list recip msg)
		       (smtp-delivery-transient-recips deliv))))
	      (:fail
	       (dolist (recip (smtp-delivery-unprocessed-recips newdeliv))
		 (push (list recip msg)
		       (smtp-delivery-failed-recips deliv))))
	      (:ok
	       ;; sanity check
	       (if (smtp-delivery-unprocessed-recips newdeliv)
		   (error "deliver-smtp-help returned :ok but there are still unprocessed recips"))))
	    ;; :transient and :fail stats may still have the failed-recips
	    ;; and transient-recips slots filled in.. so this is common
	    ;; to all
	    (setf (smtp-delivery-good-recips deliv)
	      (nconc (smtp-delivery-good-recips deliv)
		     (smtp-delivery-good-recips newdeliv)))
	    (setf (smtp-delivery-failed-recips deliv)
	      (nconc (smtp-delivery-failed-recips deliv)
		     (smtp-delivery-failed-recips newdeliv)))
	    (setf (smtp-delivery-transient-recips deliv)
	      (nconc (smtp-delivery-transient-recips deliv)
		     (smtp-delivery-transient-recips newdeliv)))))))))


(defun deliver-smtp-help (domain sender deliv q &key verbose)
  (block nil
    (let ((buf (make-string *maxlinelen*))
	  (sender (emailaddr-orig (rewrite-smtp-envelope-sender sender)))
	  (msg-size (queue-smtp-size q))
	  server-supports-size
	  errmsg)
      (setf (queue-status q)
	(format nil "Connecting to mail exchanger for ~a" domain))
      (update-queue-file q)

      (multiple-value-bind (sock mxname status)
	  (connect-to-mx domain :verbose verbose)
	(when (eq status :no-such-domain)
	  (setf errmsg (format nil "~A: Host not found" domain))
	  (maild-log-and-print verbose "~A" errmsg)
	  (return (values :fail errmsg)))
	(when (eq status :servfail)
	  (setf errmsg (format nil "~A: DNS lookup error" domain))
	  (maild-log-and-print verbose "~A" errmsg)
	  (return (values :transient errmsg)))
	(when (null sock)
	  (setf errmsg 
	    (format nil 
		    "Failed to connect to any mail exchanger for domain ~a: ~a"
		    domain status))
	  (maild-log-and-print verbose "~A" errmsg)
	  (return (values :transient errmsg)))

	;; Socket ready.
	(unwind-protect
	    (progn
	      (setf (queue-status q)
		(format nil "Communicating with ~a" mxname))
	      (update-queue-file q)
	      
	      (multiple-value-bind (res response)
		  (get-smtp-greeting sock buf mxname :verbose verbose)
		(when (not (eq res :ok))
		  (setf response 
		    (format nil "~a said ~a when we connected."
			    mxname response))
		  (return (values res response))))
	      
	      ;; HELO
	      (multiple-value-bind (res response supports-size max-size)
		  (smtp-say-hello sock buf mxname :verbose verbose)
		(if* (eq res :ok)
		   then (setf server-supports-size supports-size)
			(if (and max-size msg-size (> msg-size max-size))
			    (return 
			      (values :fail 
				      (format nil "~
Message size exceeds recipient server's max (~d)" max-size))))
		   else (return (values res response))))
	      
	      ;; MAIL FROM:
	      (multiple-value-bind (res response)
		  (smtp-send-mail-from sock buf mxname sender 
				       (if (and server-supports-size msg-size)
					   msg-size)
				       :verbose verbose)
		(if (not (eq res :ok))
		    (return (values res response))))

	      ;; RCPT TO:
	      (let (recip)
		(loop
		  (setf recip (pop (smtp-delivery-unprocessed-recips deliv)))
		  (if (null recip)
		      (return)) ;; break from loop
		  (multiple-value-bind (res response)
		      (smtp-send-rcpt-to sock buf mxname 
					 (recip-printable recip)
					 :verbose verbose)
		    ;; individual recipients may suffer transient or
		    ;; permanent errors which do not affect the rest of
		    ;; the session.
		    
		    (ecase res
		      (:ok
		       (push recip (smtp-delivery-in-process-recips deliv))
		       ;; Add an auto-whitelist entry so we can easily
		       ;; get return mail from the recipient.  Only
		       ;; do this for local senders (i.e., when local-domain-p
		       ;; returns true).
		       (if (and *greylisting-enabled*
				*greylist-whitelist-recips* 
				(not (string= sender "<>"))
				(local-domain-p sender))
			   (whitelist (recip-printable recip) 
				      sender
				      *greylist-whitelist-recips*
				      "auto-whitelist-outgoing")))
		      (:transient
		       (push (list recip response) 
			     (smtp-delivery-transient-recips deliv)))
		      (:fail
		       (push (list recip response) 
			     (smtp-delivery-failed-recips deliv)))))))
	      
	      ;; Make sure there was at least one accepted recip
	      (if (null (smtp-delivery-in-process-recips deliv))
		  (return :ok))
	      
	      ;; DATA
	      (multiple-value-bind (res response)
		  (smtp-send-data sock buf mxname q :verbose verbose)
		(ecase res
		  (:ok
		   (let (recip)
		     (while (setf recip 
			      (pop (smtp-delivery-in-process-recips deliv)))
		       (push recip (smtp-delivery-good-recips deliv))
			    
		       (maild-log-and-print 
			verbose
			"Successful SMTP delivery to ~A (mx: ~A)" 
			(recip-printable recip)  
			mxname)))
		   (return :ok))
		  ((:transient :fail)
		   (setf (smtp-delivery-unprocessed-recips deliv)
		     (smtp-delivery-in-process-recips deliv))
		   ;; not really necessary
		   (setf (smtp-delivery-in-process-recips deliv) nil)
		   (return (values res response))))))

	  ;; cleanup forms
	  (when sock 
	    (ignore-errors (smtp-finish sock buf mxname :verbose verbose))
	    (ignore-errors (close sock))
	    (ignore-errors (close sock :abort t))))))))

(defun smtp-finish (sock buf mxname &key verbose)
  (smtp-tx-rx "QUIT" sock buf *quittimeout* mxname :verbose verbose))
  
(defun smtp-send-data (sock buf mxname q &key verbose)
  (block nil
    (multiple-value-bind (status line)
	(smtp-tx-rx "DATA" sock buf *datainitiationtimeout* mxname 
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
	  (maild-log-and-print verbose "~A" report)
	  (return (values :transient report)))))
    
    (smtp-tx-rx "." sock buf *dataterminationtimeout* mxname 
		:verbose verbose :data-term t)))

(defun smtp-send-rcpt-to (sock buf mxname to &key verbose)
  (if (string= to "<>")
      (setf to ""))
  (smtp-tx-rx (format nil "RCPT TO:<~A>" to)
	      sock buf *rcptcmdtimeout* mxname :verbose verbose))

(defun smtp-send-mail-from (sock buf mxname sender size &key verbose)
  (if (string= sender "<>")
      (setf sender ""))
  (let ((cmd (format nil "MAIL FROM:<~A>" sender)))
    (if size
	(setf cmd (format nil "~a SIZE=~d" cmd size)))
    (smtp-tx-rx cmd sock buf *mailcmdtimeout* mxname :verbose verbose)))

;; Returns
;;  t for ok, nil for not-okay
;;  t or nil, to indicate if the server supports SIZE
;;  server-reported max message size (or nil if not reported)
(defun smtp-say-ehlo (sock buf mxname &key verbose)
  (let (supports-size max-size)
    (multiple-value-bind (res response)
	(smtp-tx-rx (format nil "EHLO ~A" (fqdn)) 
		    sock buf *greetingtimeout* mxname :verbose verbose)
      (when (eq res :ok)
	(multiple-value-bind (found whole value)
	    (match-re "^250-SIZE(?: (\\d+))?" response :multiple-lines t)
	  (declare (ignore whole))
	  (when found
	    (setf supports-size t)
	    (when value
	      (setf max-size (parse-integer value))
	      ;; Some servers say "SIZE 0".
	      (if (eq max-size 0)
		  (setf max-size nil)))))
	(values t supports-size max-size)))))

;; Returns
;;   status keyword
;;   response string (or nil if ok)
;;   t if the server supports the SIZE extension
;;   the max message size allowed (or nil if not reported)
(defun smtp-say-hello (sock buf mxname &key verbose)
  ;; try EHLO first.
  (multiple-value-bind (ok supports-size max-size)
      (smtp-say-ehlo sock buf mxname :verbose verbose)
    (when ok
      (return-from smtp-say-hello (values :ok nil supports-size max-size))))
  ;; No go on EHLO.  Revert to HELO
  (multiple-value-bind (res response)
      (smtp-tx-rx (format nil "HELO ~A" (fqdn)) 
		  sock buf *greetingtimeout* mxname :verbose verbose)
    (values res response)))

(defun get-smtp-greeting (sock buf mxname &key verbose)
  (get-smtp-reply sock buf *greetingtimeout* mxname :verbose verbose))

(defun smtp-tx-rx (tx sock buf timeout mxname &key verbose data-term)
  (when verbose 
    (format t ">>> ~A~%" tx)
    (finish-output))
  (with-socket-timeout (sock :write timeout)
    (outline sock "~a" tx))
  
  (multiple-value-bind (status response)
      (get-smtp-reply sock buf timeout mxname :verbose verbose)
    (if data-term 
	(setf tx "[message data]"))
    (setf response 
      (format nil "When we said: ~a, ~a said:~%~a" tx mxname response))
    (values status response)))

(defun get-smtp-reply (sock buf timeout mxname &key verbose)
  (let (line char lines)
    (loop
      (setf line (smtp-get-line sock buf timeout))
      (if verbose
	  (format t "~A~%" line))
      (cond
       ((eq line :longline)
	(maild-log-and-print
	 verbose 
	 "Got excessively long response from ~A while reading the SMTP greeting"
	 mxname)
	(return (values :transient line)))
       ((eq line :eof)
	(maild-log-and-print 
	 verbose "Remote SMTP server ~A hung up on us" mxname)
	(return (values :transient "<unexpected disconnect>")))
       ((eq line :timeout)
	(maild-log-and-print
	 verbose "Timeout while waiting for SMTP greeting from ~A" mxname)
	(return (values :transient "<communication timeout>")))
       ((stringp line)
	nil)
       (t
	(error "Unexpected return value from smtp-get-line: ~S" line)))
      (if* (< (length line) 4)
	 then
	      (maild-log-and-print
	       verbose "Got short reply line from ~A: ~A" mxname line)
	      (return (values :transient line)))
      (setf char (schar line 3))
      (push line lines)
      (case char
	(#\-  ;; more lines follow
	 nil)
	(#\space ;; final reply line.  Use this for processing the code
	 (return (smtp-response-disposition 
		  (list-to-delimited-string (nreverse lines) #\newline)
		  mxname)))
	(t
	 (maild-log-and-print
	  verbose "Got weird reply line from ~A: ~A" mxname line)
	 (return (values :transient line)))))))
	 
(defun smtp-response-disposition (line mxname)
  (block nil
    (multiple-value-bind (found whole code)
	(match-re "^(\\d{3})[ -]" line)
      (declare (ignore whole))
      (if* (not found)
	 then (maild-log "Got weird reply from ~A: ~A" mxname line)
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
    (let ((mxs (get-good-mxs domain))
	  errs)
      (if (or (eq mxs :no-such-domain) (eq mxs :servfail))
	  (return (values nil nil mxs)))
      (flet ((process-error (hostname reason)
	       (push (format nil "~a: ~a" hostname reason) errs)
	       (if verbose
		   (format t "..failed: ~A~%" reason))
	       (maild-log "Error while connecting to ~A: ~a" hostname reason)
	       nil))
	
	(dolist (mx mxs)
	  (let (sock)
	    (handler-case 
		(progn
		  (if verbose
		      (format t "Connecting to ~A...~%" (cdr mx)))
		  (setf sock (make-socket :type :hiper
					  :remote-host (car mx) 
					  :remote-port 25)))
	      (socket-error (c)
		(process-error (cdr mx) (strerror (stream-error-code c))))
	      (error (c)
		(process-error (cdr mx) (format nil "~a" c))))

	    (if* sock
	       then (if verbose
			(format t "..connected.~%"))
		    (return-from connect-to-mx (values sock (cdr mx) :ok)))))
	
	;; no luck.
	(values nil nil (list-to-delimited-string errs #\,))))))
	

;; return a list of (addr . name)
(defun get-good-mxs (domain &key verbose)
  (let (res)
    (multiple-value-bind (best ttl others disp)
	(dns-query domain :type :mx :search t
		   :ignore-cache *ignore-dns-cache*)
      (declare (ignore ttl))
      (if (member :no-such-domain disp)
	  (return-from get-good-mxs :no-such-domain))
      (if (member :nameserver-internal-error disp)
	  (return-from get-good-mxs :servfail))
      (let ((mxs (cons best others)))
	(if (null best)  ;; use the A record if there's no MX record
	    (setf mxs (list (list domain nil 0))))
	(dolist (mx mxs)
	  (complete-mx-info mx :verbose verbose)
	  (if (second mx)
	      (push (cons (second mx) (first mx)) res)))))
    (nreverse res))) ;; order matters

;; do a name lookup if (second info) is nil
;; XXX -- this is bogus because a host could have multiple addresses and
;; lookup-hostname only returns one.
(defun complete-mx-info (info &key verbose)
  (when (null (second info))
    (setf (second info) (ignore-errors (lookup-hostname (first info))))
    (if (null (second info))
	(maild-log-and-print 
	 verbose "Couldn't get address of MX ~A" (first info)))))
