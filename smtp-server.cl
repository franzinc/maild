(in-package :user)

;; handles an SMTP session

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (use-package :socket))

(defstruct session
  sock ;; might be a regular stream if program was started with -bs option
  dotted ;; remote host as a dotted string
  (buf (make-string *maxlinelen*))
  helo ;; did the client already say HELO ?
  from
  to ;; a list
  verbose
  fork)


(defun smtp-server-daemon (&key queue-interval)
  (let ((pid (fork)))
    (case pid
      (0 ;; child
       (detach-from-terminal)
       (maild-log "SMTP server starting")
       (if (and queue-interval (> queue-interval 0))
	   (queue-process-daemon queue-interval))
       (smtp-server)
       (exit 1 :quiet t)) ;; just in case
      (-1
       (error "Ack! smtp-server-daemon fork failed"))
      (t ;; parent
       )))) ;; just return

(defun smtp-server ()
  (parse-connections-blacklist)
  
  (let ((port *smtp-port*)
	(ent (getservbyname "smtp" "tcp"))
	sock)
    (if ent
	(setf port (servent-port ent)))
    (handler-case 
	(setf sock (make-socket :local-port port 
				:local-host *smtp-ip*
				:type :hiper
				:connect :passive
				:reuse-address t))
      (error (c)
	(maild-log "~A" c)
	(maild-log "Exiting.")
	(return-from smtp-server)))
    
    (unwind-protect
	(loop
	  (let ((newsock (ignore-errors (accept-connection sock))))
	    (if newsock
		(mp:process-run-function "SMTP session" #'do-smtp 
					 newsock))))
      (ignore-errors (close sock)))))

(defmacro smtp-remote-host (sock)
  (let ((socksym (gensym)))
    `(let ((,socksym ,sock))
       (if (socketp ,socksym)
	   (remote-host ,socksym)
	 #.(dotted-to-ipaddr "127.0.0.1")))))

(defmacro smtp-remote-dotted (sock)
  (let ((socksym (gensym)))
    `(let ((,socksym ,sock))
       (if (socketp ,socksym)
	   (ipaddr-to-dotted (remote-host ,socksym))
	 "localhost"))))

    
(defun do-smtp (sock &key fork verbose)
  (unwind-protect
      (handler-case
	  (with-socket-timeout (sock :write *datatimeout*)
	    (let* ((dotted (smtp-remote-dotted sock))
		   (sess (make-session :sock sock :dotted dotted
				       :fork fork :verbose verbose))
		   bl cmd)
	
	      (maild-log "SMTP connection from ~A" dotted)
	      
	      (setf bl (connection-blacklisted-p (smtp-remote-host sock)))
	
	      (when bl
		(outline sock "500 ~A (~A)" *blacklisted-response* bl)
		(maild-log "Rejecting blacklisted SMTP client (~A)"
			   dotted)
		(return-from do-smtp))
	      
	      (outline sock "220 ~A Allegro mail server ready" (fqdn)) ;; Greet
	      (loop
		(setf cmd 
		  (smtp-get-line (session-sock sess) (session-buf sess) 
				 *cmdtimeout*))
		(case cmd
		  (:timeout 
		   (maild-log "Closing idle SMTP connection from ~A" dotted)

		   (outline sock "421 Control connection idle for too long")
		   (return :timeout)) 
		  (:eof
		   ;; client closed the connection or something.
		   (return :eof))
		  (:longline
		   (maild-log "SMTP client ~A sent excessively long command"
			      dotted)
		   (outline sock "500 Line too long."))
		  (t
		   (if (eq (process-smtp-command sess cmd) :quit)
		       (return :quit)))))))
	(error (c)
	  (maild-log "SMTP server error: ~A" c)))
    ;; cleanup forms
    (maild-log "Closing SMTP session with ~A" (smtp-remote-dotted sock))
    (if (socketp sock)
	(ignore-errors (close sock :abort t)))))


(defparameter *smtpcmdlist*
    '(("helo" smtp-helo)
      ("quit" smtp-quit)
      ("noop" smtp-noop)
      ("rset" smtp-reset)
      ("mail" smtp-mail)
      ("rcpt" smtp-rcpt)
      ("data" smtp-data)))

(defun process-smtp-command (sess cmd)
  (block nil
    (let ((sock (session-sock sess))
	  cmdinfo)
      (if* (or (< (length cmd) 4)
	       (null (setf cmdinfo (find (subseq cmd 0 4) *smtpcmdlist*
					 :key #'first :test #'equalp))))
	 then
	      (outline sock "500 5.5.1 Command unrecognized: ~S"
		       cmd)
	      (return nil))
      (funcall (second cmdinfo) sess (subseq cmd 4)))))

(defun reset-smtp-session (sess)
  (setf (session-from sess) nil)
  (setf (session-to sess) nil))
  
;; What good is this?
(defun smtp-helo (sess text)
  (let ((sock (session-sock sess)))
    (if* (session-helo sess)
       then
	    (outline sock "503 5.0.0 ~a Duplicate HELO"
		     (fqdn))
       else
	    (setf (session-helo sess) text)
	    (outline sock "250 ~a Hi there." (fqdn)))))
    
(defun smtp-quit (sess text)
  (declare (ignore text))
  (outline (session-sock sess) "221 2.0.0 ~a closing connection" (fqdn))
  :quit)

(defun smtp-noop (sess text)
  (declare (ignore text))
  (outline (session-sock sess) "250 2.0.0 OK"))


(defun smtp-reset (sess text)
  (declare (ignore text))
  (reset-smtp-session sess)
  (outline (session-sock sess) "250 2.0.0 Reset state"))

(defun smtp-mail (sess text)
  (block nil
    (let ((sock (session-sock sess)))
      (if* (session-from sess)
	 then
	      (outline sock "503 5.5.0 Sender already specified")
	      (return))
      ;; parse the command line
      (multiple-value-bind (matched whole addr)
	  (match-regexp "^\\b+from:\\b*\\(..*\\)\\b*$" text :case-fold t)
	(declare (ignore whole))
	(when (not matched)
	  (outline sock "501 5.5.2 Syntax error in command")
	  (return))
	(let ((addr (parse-email-addr addr :allow-null t)))
	  (when (null addr)
	    (outline sock "553 5.3.0 Invalid address")
	    (maild-log "Client from ~a: MAIL ~A: Invalid address"
		       (session-dotted sess) text)
	    (return))
	  
	  ;; Call checkers
	  (dolist (checker *smtp-mail-from-checkers*)
	    (multiple-value-bind (status string)
		(funcall (second checker) (smtp-remote-host sock) addr)
	      (ecase status
		(:err
		 (outline sock "551 ~A" string)
		 (maild-log "Checker ~S rejected MAIL FROM:~A. Message: ~A"
			    (first checker) (emailaddr-orig addr)
			    string)
		 (return-from smtp-mail))
		(:transient
		 (outline sock "451 ~A" string)
		 (maild-log 
		  "Checker ~S temporarily failed MAIL FROM:~A. Message: ~A"
		  (first checker) (emailaddr-orig addr) string)
		 (return-from smtp-mail))
		(:ok
		 ))))
	  
	  (setf (session-from sess) addr)
	  (outline sock "250 2.1.0 ~A... Sender ok" (emailaddr-orig addr)))))))

(defun smtp-rcpt (sess text)
  (block nil
    (let ((sock (session-sock sess))
	  disp errmsg)
      (when (>= (length (session-to sess)) *maxrecips*)
	(outline sock "452 4.5.3 Too many recipients")
	(maild-log "Client from ~A: Too many recipients"
		   (session-dotted sess))
	(return))
      (when (null (session-from sess))
	(outline sock "503 5.0.0 Need MAIL before RCPT")
	(return))
      (multiple-value-bind (matched whole addrstring)
	  (match-regexp "^\\b+to:\\b*\\(..*\\)\\b*$" text :case-fold t)
	(declare (ignore whole))
	(when (not matched)
	  (outline sock "501 5.5.2 Syntax error in command")
	  (return))
	(let ((addr (parse-email-addr addrstring)))
	  (when (null addr)
	    (outline sock "553 5.3.0 Invalid address")
	    (maild-log "Client from ~A: rcpt to:~A... Invalid address"
		       (session-dotted sess)
		       addrstring)
	    (return))
	  
	  (multiple-value-setq (disp errmsg)
	    (get-recipient-disposition addr))
	  (ecase disp
	    (:error
	     (outline sock "550 ~a... ~a" 
		      (emailaddr-orig addr) errmsg)
	     (maild-log "client from ~A: rcpt to:~A... blacklisted recip"
			(session-dotted sess)
			(emailaddr-orig addr))
	     (return))
	    (:local-unknown
	     (outline sock "550 5.1.1 ~a... User unknown"
		      (emailaddr-orig addr))
	     (maild-log "Client from ~A: rcpt to:~A... User unknown"
			(session-dotted sess)
			(emailaddr-orig addr))
	     (return))
	    (:local-ok
	     (setf disp :local))
	    (:non-local
	     (setf disp :remote)))
	  
	  ;; Run checkers
	  (dolist (checker *smtp-rcpt-to-checkers*)
	    (multiple-value-bind (status string)
		(funcall (second checker) 
			 (smtp-remote-host sock)
			 (session-from sess)
			 disp
			 addr
			 (session-to sess))
	      (ecase status
		(:err
		 (outline sock "550 ~A" string)
		 (maild-log "Checker ~S rejected RCPT TO:~A. Message: ~A"
			    (first checker) (emailaddr-orig addr)
			    string)
		 (return-from smtp-rcpt))
		(:transient
		 (outline sock "451 ~A" string)
		 (maild-log 
		  "Checker ~S temporarily failed RCPT TO:~A. Message: ~A"
		  (first checker) (emailaddr-orig addr) string)
		 (return-from smtp-rcpt))
		(:ok
		 ))))
	  
	  (push addr (session-to sess))
	  (outline sock "250 2.1.5 ~a... Recipient ok" 
		   (emailaddr-orig addr)))))))
 
(defun smtp-data (sess text)
  (declare (ignore text))
  (let* ((sock (session-sock sess))
	 (dotted (session-dotted sess))
	 q status headers msgsize err rejected)
    ;; Sanity checks first
    (if (null (session-from sess))
	(return-from smtp-data 
	  (outline sock "503 5.0.0 Need MAIL command")))
    (if (null (session-to sess))
	(return-from smtp-data 
	  (outline sock "503 5.0.0 Need RCPT (recipient)")))

    ;; Run pre-checkers
    (dolist (checker *smtp-data-pre-checkers*)
      (multiple-value-bind (status string)
	  (funcall (second checker) (smtp-remote-host sock) 
		   (session-from sess) (session-to sess))
	(ecase status
	  (:err
	   (outline sock "551 ~A" string)
	   (maild-log "Checker ~S rejected DATA phase initiation. Message: ~A"
		      (first checker) string)
	   (return-from smtp-data))
	  (:transient
	   (outline sock "451 ~A" string)
	   (maild-log 
	    "Checker ~S temporarily failed DATA phase initiation. Message: ~A"
	    (first checker) string)
	   (return-from smtp-data))
	  (:ok
	   ))))
    
    (with-new-queue (q f err (session-from sess))
      (outline sock "354 Enter mail, end with \".\" on a line by itself")
      (handler-case 
	  (multiple-value-setq (status headers msgsize)
	    (read-message-stream sock f :smtp t))
	(socket-error (c)
	  (if (eq (stream-error-identifier c) :read-timeout)
	      (maild-log "Client from ~A stopped transmitting." dotted)
	    (maild-log "Client from ~A: socket error: ~A" dotted c))
	  (return-from smtp-data :quit))
	(error (c)
	  (maild-log "Unexpected error during read-message-stream: ~A" c)
	  (setf err :transient)
	  (return))) ;; break from with-new-queue
      
      (ecase status
	(:eof
	 (maild-log "Client ~A disconnected during SMTP data transmission"
		    dotted)
	 (return-from smtp-data :quit))
	(:dot  ;;okay
	 ))

      ;; Run through checkers.
      (when (and (not err) (not rejected))
	(multiple-value-bind (res text checker)
	    (check-message-checkers q headers msgsize)
	  (ecase res
	    (:ok
	     ) ;; all is well
	    (:reject
	     (maild-log "Rejecting message from ~A" 
			(emailaddr-orig (session-from sess)))
	     (maild-log "Checker ~S said: ~A" checker text)
	     
	     (outline sock "552 ~A" text)
	     (setf rejected t))
	    (:transient
	     (maild-log "Checker ~s reported a transient error: ~A"
			checker text)
	     (setf err :transient)))))
      
      (when (and (not err) (not rejected))
	;; This finalizes and unlocks the queue item.
	(queue-finalize q (session-to sess) headers (smtp-remote-host sock))))
    
    ;; At this point, the queue file is saved on disk.. or it has been
    ;; deleted due to an error/rejection in processing.
    
    ;; Rejected messages have already been reported to the client.
    ;; Report transient errors now.
    
    (when (or (eq err :transient)
	      (and (listp err) (eq (first err) :transient)))
      (outline 
       sock "451 Requested action aborted: local error in processing"))
    
    ;; If we have a good message, report it to the client and
    ;; begin delivering it.

    (when (and (not err) (not rejected))
      (maild-log "Accepted SMTP message from client ~A: from ~A to ~A"
		 dotted
		 (emailaddr-orig (session-from sess))
		 (list-to-delimited-string
		  (mapcar #'emailaddr-orig (session-to sess))
		  #\,))
      (outline sock "250 2.0.0 ~A Message accepted for delivery"
	       (queue-id q))
      
      ;; Deliver in another process
      (if (and (session-fork sess) (not (session-verbose sess)))
	  (let ((pid (fork)))
	    (case pid
	      (0 ;; child
	       (detach-from-terminal)
	       (queue-process-single (queue-id q) :wait t)
	       (exit 0 :quiet t))
	      (-1  ;; error
	       (error "Fork failed!")))
	    ;; parent.. just keep on truckin'
	    )
	;; else.. we must be running in daemon mode already..
	(mp:process-run-function 
	    (format nil "Processing id ~A" (queue-id q)) 
	  #'queue-process-single (queue-id q))))
    
    (reset-smtp-session sess)))
