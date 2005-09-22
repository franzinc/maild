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
;; $Id: smtp-server.cl,v 1.32 2005/09/22 04:02:57 dancy Exp $

(in-package :user)

;; handles an SMTP session

(eval-when (compile load eval)
  (require :osi)
  (require :ssl)
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
  fork
  ssl
  auth-user)

(defstruct statistics
  (stats-start (get-universal-time))
  (num-connections 0)
  connections-rejected-permanently ;; list of checker/count pairs
  connections-rejected-temporarily ;; list of checker/count pairs
  (connections-accepted 0)
  senders-rejected-permanently ;; list of checker/count pairs
  senders-rejected-temporarily ;; list of checker/count pairs
  recips-rejected-permanently ;; list of checker/count pairs
  recips-rejected-temporarily ;; list of checker/count pairs
  messages-pre-rejected-permanently ;; list of checker/count pairs
  messages-pre-rejected-temporarily ;; list of checker/count pairs
  messages-rejected-permanently ;; list of checker/count pairs
  messages-rejected-temporarily ;; list of checker/count pairs
  (mails-accepted 0))

(defparameter *smtp-server-stats* (make-statistics))

(defmacro inc-smtp-stat (slot)
  (let ((accessor (intern (format nil "~A-~A" 'statistics slot))))
    `(without-interrupts
       (incf (,accessor *smtp-server-stats*)))))

(defmacro get-smtp-stat (slot)
  (let ((accessor (intern (format nil "~A-~A" 'statistics slot))))
    `(without-interrupts
       (,accessor *smtp-server-stats*))))

(defmacro inc-checker-stat (slot checkername)
  (let ((tmp (gensym))
	(checker (gensym))
	(accessor (intern (format nil "~A-~A" 'statistics slot))))
    `(without-interrupts
       (let* ((,checker ,checkername)
	      (,tmp (member ,checker (,accessor *smtp-server-stats*) :key #'first :test #'string=)))
	 (if ,tmp
	     (incf (cdr (first ,tmp)))
	   (push (cons ,checker 1) (,accessor *smtp-server-stats*)))))))

  

(defun smtp-server-daemon (&key queue-interval)
  (flet ((startup ()
	   (maild-log "Allegro Maild ~A SMTP server starting"
		      *allegro-maild-version*)
	   (if (and queue-interval (> queue-interval 0))
	       (queue-process-daemon queue-interval))
	   (smtp-server)))
    (if* *debug*
       then (startup)
	    (loop (sleep 200000))
       else (let ((pid (fork)))
	      (case pid
		(0 ;; child
		 (detach-from-terminal)
		 (startup)
		 
		 (exit 1 :quiet t)) ;; just in case
		(-1
		 (error "Ack! smtp-server-daemon fork failed"))
		(t ;; parent
		 ;; just return
		 ))))))

(defvar *rep-server-started* nil)

(defun smtp-server ()
  (when (and *rep-start-server*
	     (not *rep-server-started*))
    (maild-log "Starting REP server")
    
    (setq excl::*trace-timestamp* t)
    (setq *trace-output*
      (open "/tmp/maild.trace" :direction :output
	    :if-does-not-exist :create
	    :if-exists :append))    

    (mp:process-run-function "rep server"
      'start-rep-server *rep-server-port* 'maild-log)
    (setq *rep-server-started* t))
  
  (parse-connections-blacklist)

  (let ((port *smtp-port*)
	(ent (getservbyname "smtp" "tcp"))
	sockets sock sslsock)
    (if ent
	(setf port (servent-port ent)))
    (handler-case 
	(progn
	  (setf sock (make-socket :local-port port 
				  :local-host *smtp-ip*
				  :type :hiper
				  :connect :passive
				  :reuse-address t))
	  (if *ssl-support*
	      (setf sslsock (make-socket :local-port 465
					 :local-host *smtp-ip*
					 :type :hiper
					 :connect :passive
					 :reuse-address t))))
      (error (c)
	(maild-log "~A" c)
	(maild-log "Exiting.")
	(return-from smtp-server)))

    (push sock sockets)
    (if sslsock (push sslsock sockets))
    
    
    (if (probe-file *stats-file*)
	(if* (world-or-group-writable-p *stats-file*)
	   then
		(maild-log 
		 "Not reading ~A since it is world or group writable"
		 *stats-file*)
		(setf *smtp-server-stats* (make-statistics))
	   else
		(with-open-file (f *stats-file*)
		  (setf *smtp-server-stats* (read f))))
      ;; no file. start fresh
      (setf *smtp-server-stats* (make-statistics)))
       
    (start-webserver)
  
    (unwind-protect
	(loop
	  (let* ((ready (first (mp:wait-for-input-available sockets)))
		 (newsock (ignore-errors (accept-connection ready))))
	    (when newsock
	      (mp:process-run-function "SMTP session" 
		#'do-smtp 
		newsock :ssl (eq ready sslsock)))))
      (ignore-errors (close sock)
		     (if sslsock (close sslsock))))))

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

(defun start-tls-common (sess)
  (setf (session-ssl sess) t)
  (setf (session-sock sess)
    (socket:make-ssl-server-stream 
     (session-sock sess)
     :certificate *ssl-certificate-file*
     :key *ssl-key-file*)))
  
    
(defun do-smtp (sock &key fork verbose ssl)
  (unwind-protect
      (handler-case
	  (with-socket-timeout (sock :write *datatimeout*)
	    (let* ((dotted (smtp-remote-dotted sock))
		   (sess (make-session :sock sock :dotted dotted
				       :ssl ssl
				       :fork fork :verbose verbose))
		   cmd)

	      (if ssl
		  (setf sock (start-tls-common sess)))
	      
	      (maild-log "~ASMTP connection from ~A" 
			 (if ssl "SSL " "")
			 dotted)
	      
	      (inc-smtp-stat num-connections)
	      
	      (parse-connections-blacklist)
	      
	      ;; Run through checkers.
	      (dolist (checker *smtp-connection-checkers*)
		(multiple-value-bind (status string)
		    (funcall (second checker) (smtp-remote-host sock))
		  (ecase status
		    (:err
		     (outline sock "500 ~A" string)
		     (maild-log 
		      "Checker ~S rejected connection from ~A. Message: ~A"
		      (first checker) dotted string)
		     (inc-checker-stat connections-rejected-permanently (first checker))
		     (return-from do-smtp))
		    (:transient
		     (outline sock "400 ~A" string)
		     (maild-log 
		      "Checker ~S temporarily rejected connection from ~A. Message: ~A"
		      (first checker) dotted string)
		     (inc-checker-stat connections-rejected-temporarily (first checker))
		     (return-from do-smtp))
		    (:ok
		     ))))
	      
	      (inc-smtp-stat connections-accepted)
	      
	      ;; Greet
	      (outline sock "220 ~A Allegro Maild ~A ready" 
		       (fqdn) *allegro-maild-version*)

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
    (when (socketp sock)
      (ignore-errors (update-smtp-stats)) ;; stats file dir might not exist
      (ignore-errors (close sock :abort t)))))


(defparameter *smtp-stats-lock* nil)

;; don't do this if we're running with -bs
(defun update-smtp-stats ()
  (without-interrupts 
    (if (null *smtp-stats-lock*)
	(setf *smtp-stats-lock* (mp:make-process-lock))))
  (mp:with-process-lock (*smtp-stats-lock*)
    (with-os-open-file (f *stats-file* (logior *o-wronly* *o-creat* *o-trunc*) 
			  #o600)
      (write *smtp-server-stats* :stream f))))


(defparameter *smtpcmdlist*
    '(("helo" smtp-helo)
      ("ehlo" smtp-ehlo)
      ("starttls" smtp-starttls)
      ("quit" smtp-quit)
      ("noop" smtp-noop)
      ("auth" smtp-auth)
      ("rset" smtp-reset)
      ("mail" smtp-mail)
      ("rcpt" smtp-rcpt)
      ("data" smtp-data)))

(defun process-smtp-command (sess cmdline)
  (let* ((sock (session-sock sess))
	 (spacepos (or (position #\space cmdline) (length cmdline)))
	 (cmd (subseq cmdline 0 spacepos))
	 (cmdinfo (find cmd *smtpcmdlist* :key #'first :test #'equalp)))
    
    (if* (null cmdinfo)
       then
	    (outline sock "500 5.5.1 Command unrecognized: ~S"
		     cmdline)
	    nil
       else
	    (funcall (second cmdinfo) sess (subseq cmdline spacepos)))))

(defun reset-smtp-session (sess)
  (setf (session-from sess) nil)
  (setf (session-to sess) nil))

(defun smtp-helo (sess text)
  (smtp-helo-common sess text :helo))

(defun smtp-ehlo (sess text)
  (smtp-helo-common sess text :ehlo))

(defun smtp-helo-common (sess text type)
  (block nil
    (let ((sock (session-sock sess)))
      (when (session-helo sess)
	(outline sock "503 5.0.0 ~a Duplicate HELO"
		 (fqdn))
	(return))

      (when (and *helo-must-match-ip*
		 (not (string= (session-dotted sess) "127.0.0.1")))
	(setf text (string-left-trim '(#\space) text))
	
	(multiple-value-bind (first ttl rest flags)
	    (dns-query text :search t :ignore-cache *ignore-dns-cache*)
	  (declare (ignore ttl))
	  (when (member :no-such-domain flags)
	    (if* (eq 't *helo-must-match-ip*)
	       then (outline sock "501 Invalid domain name")
		    (maild-log
		     "Rejected HELO ~A from client ~A (invalid domain)"
		     text (session-dotted sess))
		    (inc-checker-stat connections-rejected-permanently
				      "HELO domain checker")
		    (return :quit)
	       else ;; log only
		    (maild-log
		     "NOTE: HELO ~A from client ~A (invalid domain)"
		     text (session-dotted sess))))
	  
	  (when (null first)
	    (if* (eq 't *helo-must-match-ip*)
	       then (outline sock "401 Unable to resolve domain")
		    (maild-log "Temporarily rejected client from ~A ~
because we couldn't resolve the name supplied in the HELO command (~A)" 
			       (session-dotted sess) text)
		    (inc-checker-stat connections-rejected-temporarily
				      "HELO domain checker")
		    (return :quit)
	       else ;; log only
		    (maild-log "NOTE: couldn't resolve the name supplied ~
in the HELO command (~A) from client ~A"
			       text (session-dotted sess))))
	  
	  (let ((addrs (append (list first) rest)))
	    (when (not (member (smtp-remote-host sock) addrs))
	      (if* (eq 't *helo-must-match-ip*)
		 then ;; do it
		      (outline sock
			       "501 HELO domain must match your IP address")
		      (maild-log
		       "Rejected HELO ~A from client ~A (No IP match)"
		       text (session-dotted sess))
		      (inc-checker-stat connections-rejected-permanently
					"HELO domain checker")
		      (return :quit)
		 else ;; log only
		      (maild-log
		       "NOTE: HELO ~A from client ~A (No IP match)"
		       text (session-dotted sess)))))))
      
      (setf (session-helo sess) text)

      (ecase type
	(:helo
	 (outline sock "250 ~a Hi there." (fqdn)))
	(:ehlo
	 (let (res)
	   (push (format nil "~a Hi there." (fqdn)) res)

	   (if (and *ssl-support* (not (session-ssl sess)))
	       (push "STARTTLS" res))

	   (when *client-authentication*
	     (if (or (not *client-auth-requires-ssl*)
		     (and *client-auth-requires-ssl* (session-ssl sess)))
		 (push 
		  (format nil "AUTH ~a"
			  (list-to-delimited-string 
			   (mapcar #'car *sasl-mechs*) 
			   #\space))
		  res)))
	     
	   (setf res (nreverse res))
	   (while (> (length res) 1)
	     (outline sock "250-~a" (pop res)))
	   (outline sock "250 ~a" (pop res))))))))

(defun smtp-starttls (sess text)
  (declare (ignore text))
  (let ((sock (session-sock sess)))
    (if* (or (not *ssl-support*) (session-ssl sess))
       then
	    (outline sock "500 STARTTLS not available")
       else
	    (setf (session-helo sess) nil)
	    (outline sock "220 Ready for TLS")
	    (maild-log "Starting TLS for ~a" (session-dotted sess))
	    (start-tls-common sess))))
      
(defun smtp-quit (sess text)
  (declare (ignore text))
  (outline (session-sock sess) "221 2.0.0 ~a closing connection" (fqdn))
  :quit)

(defun smtp-noop (sess text)
  (declare (ignore text))
  (outline (session-sock sess) "250 2.0.0 OK"))

(defun smtp-auth (sess text)
  (block nil
    (let ((sock (session-sock sess))
	  mechname initial mechfunc)
      (when (not *client-authentication*)
	(outline sock "500 Command unavailable")
	(return))
      
      (when (and *client-auth-requires-ssl* (not (session-ssl sess)))
	(outline sock "538 Encryption required for requested authentication mechanism")
	(return))
      
      (when (session-auth-user sess)
	(outline sock "503 5.5.0 Already Authenticated")
	(return))

      (let ((tmp 
	     (delimited-string-to-list (string-trim '(#\space) text) #\space)))
	(setf mechname (first tmp))
	(setf initial (second tmp)))
	
      (when (null mechname)
	(outline sock "501 5.5.2 AUTH mechanism must be specified")
	(return))
      
      (setf mechfunc (cdr (assoc mechname *sasl-mechs* :test #'equalp)))
      (when (null mechfunc)
	(outline sock "504 5.3.3 AUTH mechanism ~a not available"
		 mechname)
	(return))

      (multiple-value-bind (user pass)
	  (funcall mechfunc sess initial)
	(when (or (null user) (not (authenticate-user user pass)))
	  (maild-log "~a: Auth failure (user=~a)"
		     (session-dotted sess) user)
	  (sleep 5)
	  (outline sock "535 5.7.0 authentication failed")
	  (return))
	
	(setf (session-auth-user sess) user)

	(maild-log "~a: User ~a authenticated" (session-dotted sess) user)
	(outline sock "235 2.0.0 OK Authenticated")))))
    

(defun smtp-reset (sess text)
  (declare (ignore text))
  (reset-smtp-session sess)
  (outline (session-sock sess) "250 2.0.0 Reset state"))

(defun smtp-mail (sess text)
  (block nil
    (let ((sock (session-sock sess)))
      (when (and *helo-must-match-ip* (not (session-helo sess)))
	(outline sock "503 5.0.0 Polite people say HELO first")
	(return))

      (when (session-from sess)
	(outline sock "503 5.5.0 Sender already specified")
	(return))

      (when (and (eq *client-authentication* :required)
		 (null (session-auth-user sess)))
	(outline sock "530 Authentication required")
	(return))
      
      ;; parse the command line
      (multiple-value-bind (matched whole addr auth)
	  (match-regexp "^\\b+from:\\b*\\(\\B\\B*\\)\\b*\\(auth=\\B+\\|\\)$"
			text :case-fold t)
	(declare (ignore whole auth))
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
		 
		 (inc-checker-stat senders-rejected-permanently (first checker))
		 (return-from smtp-mail))
		(:transient
		 (outline sock "451 ~A" string)
		 (maild-log 
		  "Checker ~S temporarily failed MAIL FROM:~A. Message: ~A"
		  (first checker) (emailaddr-orig addr) string)
		 (inc-checker-stat senders-rejected-temporarily (first checker))
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
	  (match-regexp "^\\b+to:\\b*\\(\\B\\B*\\)\\b*$" text :case-fold t)
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
	     (maild-log "client from ~A: rcpt to:~A... error recip"
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
			 sess
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
		 (inc-checker-stat recips-rejected-permanently (first checker))
		 (return-from smtp-rcpt))
		(:transient
		 (outline sock "451 ~A" string)
		 (maild-log 
		  "Checker ~S temporarily failed RCPT TO:~A. Message: ~A"
		  (first checker) (emailaddr-orig addr) string)
		 (inc-checker-stat recips-rejected-temporarily (first checker))
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
	   (inc-checker-stat messages-pre-rejected-permanently (first checker))
	   (return-from smtp-data))
	  (:transient
	   (outline sock "451 ~A" string)
	   (maild-log 
	    "Checker ~S temporarily failed DATA phase initiation. Message: ~A"
	    (first checker) string)
	   (inc-checker-stat messages-pre-rejected-temporarily (first checker))
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

      (dolist (checker *smtp-data-checkers*)
	(multiple-value-bind (status string)
	    (funcall (second checker) (smtp-remote-host sock) 
		     (session-from sess) (session-to sess)
		     msgsize headers (queue-datafile q))
	  (ecase status
	    (:err
	     (outline sock "551 ~A" string)
	     (maild-log "Checker ~S rejected message data. Message: ~A"
			(first checker) string)
	     (inc-checker-stat messages-rejected-permanently (first checker))
	     (return-from smtp-data))
	    (:transient
	     (outline sock "451 ~A" string)
	     (maild-log 
	      "Checker ~S temporarily failed message. Message: ~A"
	      (first checker) string)
	     (inc-checker-stat messages-rejected-temporarily (first checker))
	     (return-from smtp-data))
	    (:ok
	     ))))

      
      ;; Run through non-smtp-specific checkers.
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
	     (inc-checker-stat messages-rejected-permanently checker)
	     
	     (setf rejected t))
	    (:transient
	     (maild-log "Checker ~s reported a transient error: ~A"
			checker text)
	     (inc-checker-stat messages-rejected-temporarily checker)
	     (setf err :transient)))))
      
      (when (and (not err) (not rejected))
	;; This finalizes and unlocks the queue item.
	(queue-finalize q (session-to sess) headers (smtp-remote-host sock)
			:date t)))
    
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

      (inc-smtp-stat mails-accepted)
      
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
