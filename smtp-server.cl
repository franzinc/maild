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
;; $Id: smtp-server.cl,v 1.46 2007/07/23 20:23:06 dancy Exp $

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
  auth-user
  rcpt-to-delay)

(defstruct statistics
  (stats-start (get-universal-time))
  (num-connections 0)
  connections-rejected-permanently ;; list of checker/count pairs
  connections-rejected-temporarily ;; list of checker/count pairs
  (connections-accepted 0)
  (transactions-started 0)
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
    #-smp-macros
    `(without-interrupts
       (incf (,accessor *smtp-server-stats*)))
    #+smp-macros `(incf-atomic (,accessor *smtp-server-stats*))
    ))

(defmacro get-smtp-stat (slot)
  (let ((accessor (intern (format nil "~A-~A" 'statistics slot))))
    `(,accessor *smtp-server-stats*)))

(defmacro inc-checker-stat (slot checkername)
  (let ((tmp (gensym))
	(checker (gensym))
	(accessor (intern (format nil "~A-~A" 'statistics slot))))
    `(#-smp-macros without-interrupts 
      #+smp-macros with-delayed-interrupts 
      ;;mm 2012-02 SMP-NOTE This can only be done with a lock in smp
      ;;           since we are basically doing a pushnew here.
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
	   (when *pid-file*
	     (with-open-file (f *pid-file*
			      :external-format :latin1
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	       (format f "~d~%" (excl.osi:getpid))))
	   (unwind-protect (smtp-server)
	     (if *pid-file*
		 (ignore-errors (delete-file *pid-file*))))))
	     
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

  (let (sockets sock sslsock)
    (handler-case 
	(progn
	  (setf sock (make-socket :local-port *smtp-port*
				  :local-host *smtp-ip*
				  :type :hiper
				  :connect :passive
				  :reuse-address t))
	  (if *ssl-support*
	      (setf sslsock (make-socket :local-port *smtps-port*
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
		(with-open-file (f *stats-file* :external-format :latin1)
		  (setf *smtp-server-stats* (read f))))
      ;; no file. start fresh
      (setf *smtp-server-stats* (make-statistics)))
       
    (start-webserver)
  
    (unwind-protect
	(loop
	  (let* ((ready (first (mp:wait-for-input-available sockets))))
	    (multiple-value-bind (newsock err)
		(ignore-errors (accept-connection ready))
	      (if* newsock
		 then #+ignore
		      (maild-log "accept-connection ~a fd ~a"
				 newsock
				 (excl::stream-input-handle newsock))
		      (mp:process-run-function "SMTP session" 
			#'do-smtp 
			newsock :ssl (eq ready sslsock))
		 else (maild-log "accept-connection failed: ~a" err)))))
      ;; cleanup forms
      (ignore-errors (close sock))
      (if sslsock (ignore-errors (close sslsock))))))

(defmacro smtp-remote-host (sock)
  (let ((socksym (gensym)))
    `(let ((,socksym ,sock))
       (if (socketp ,socksym)
	   (remote-host ,socksym)
	 0))))

(defmacro smtp-remote-dotted (sock)
  (let ((socksym (gensym)))
    `(let ((,socksym ,sock))
       (if (socketp ,socksym)
	   (ipaddr-to-dotted (remote-host ,socksym))
	 "[local]"))))

(defun start-tls-common (sess)
  (setf (session-ssl sess) t)
  (setf (session-sock sess)
    (socket:make-ssl-server-stream 
     (session-sock sess)
     :certificate *ssl-certificate-file*
     :key *ssl-key-file*)))
  
(defmacro with-smtp-err-handler ((sock) &body body)
  (let ((s (gensym)))
    `(let ((,s ,sock))
       (block nil
	 (handler-bind 
	     ((excl::ssl-error 
	       #'(lambda (c)
		   (maild-log "~a: ~a" (smtp-remote-dotted ,s) c)
		   (return)))
	      (socket-error 
	       #'(lambda (c)
		   (maild-log "~a: ~a" 
			      (smtp-remote-dotted ,s) 
			      (excl.osi:strerror (stream-error-code c)))
		   (return)))
	      (errno-stream-error 
	       #'(lambda (c)
		   (if* (not (eq (stream-error-code c) *epipe*))
		      then (report-unexpected-smtp-error c)
		      else (maild-log "~a: Broken pipe" (smtp-remote-dotted ,s)))
		   (return)))
	      (error 
	       #'(lambda (c)
		   (report-unexpected-smtp-error c)
		   (return))))
	   ,@body)))))

(defun report-unexpected-smtp-error (c)
  (maild-log "SMTP server error: ~A" c)
  (maild-log "Begin backtrace~%")
  (let ((backtrace (with-output-to-string (s) (zoom s))))
    (dolist (line (delimited-string-to-list backtrace #\newline))
      (maild-log "  ~a" line))
    (maild-log "End backtrace")))

(defun do-smtp (sock &key fork verbose ssl)
  (with-smtp-err-handler (sock)
    (unwind-protect
	(with-socket-timeout (sock :write *datatimeout*)
	  (let* ((dotted (smtp-remote-dotted sock))
		 (sess (make-session :sock sock :dotted dotted
				     :ssl ssl
				     :fork fork :verbose verbose
				     :rcpt-to-delay
				     *rcpt-to-negative-initial-delay*
				     ))
		 cmd)

	    (if ssl
		(setf sock (start-tls-common sess)))
	      
	    (maild-log "~ASMTP connection from ~A" 
		       (if ssl "SSL " "")
		       dotted)
	      
	    (inc-smtp-stat num-connections)
	      
	    (parse-connections-blacklist)
	      
	    ;; Run through checkers.
	    (when (not *postpone-checkers*)
	      (if (null (run-connection-checkers sock))
		  (return-from do-smtp)))
	      
	    (inc-smtp-stat connections-accepted)

	    (when (and *greet-pause* 
		       (not (trusted-client-p (smtp-remote-host sock))))
	      (sleep *greet-pause*)
	      (when (and (listen sock) (peek-char nil sock nil))
		;; Pre-greeting traffic received.
		(let ((strict nil))
		  (if* (not strict)
		     then (maild-log "Pre-greeting traffic from ~a" dotted)
		     else (maild-log "Dropping connection from ~a due to pre-greeting traffic" dotted)
			  (outline sock "554 Terminating connection due to protocol violation")
			  ;; RFC2821 says that we must wait continue to respond
			  ;; to commands until the client says QUIT.  But if the
			  ;; client is going to violate the protocol, so are we.
			  (return-from do-smtp)))))
	      
	    ;; Greet
	    (outline sock "220 ~A Allegro Maild ~A ready" 
		     (fqdn) *allegro-maild-version*)

	    (loop
	      ;; bug17189.
	      ;; Make sure that 'sock' always has the correct current
	      ;; socket.  The STARTTLS command might change it.
	      (setf sock (session-sock sess))
	      
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
      ;; cleanup forms
      (maild-log "Closing SMTP session with ~A)" 
		 (smtp-remote-dotted sock))
      (when (socketp sock)
	(ignore-errors (update-smtp-stats)) ;; stats file dir might not exist
	(ignore-errors (close sock))
	(ignore-errors (close sock :abort t))))))

;; Returns 't' if accepted, nil if rejected.
(defun run-connection-checkers (sock &key postponed)
  (let ((dotted (smtp-remote-dotted sock)))
    (dolist (checker *smtp-connection-checkers* t)
      (multiple-value-bind (status string)
	  (funcall (second checker) (smtp-remote-host sock))
	(ecase status
	  (:err
	   (inc-checker-stat connections-rejected-permanently (first checker))
	   (outline sock "500 ~A" string)
	   (maild-log 
	    "~aChecker ~S rejected connection from ~A. Message: ~A"
	    (if postponed "Postponed " "") (first checker) dotted string)
	   (return))
	  (:transient
	   (inc-checker-stat connections-rejected-temporarily (first checker))
	   (outline sock "400 ~A" string)
	   (maild-log 
	    "~aChecker ~S temporarily rejected connection from ~A. Message: ~A"
	    (if postponed "Postponed " "") (first checker) dotted string)
	   (return))
	  (:sufficient
	   (return t))
	  (:ok
	   ))))))

(defparameter *smtp-stats-lock* (mp:make-process-lock))

;; don't do this if we're running with -bs
(defun update-smtp-stats ()
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

      (setf text (string-left-trim '(#\space) text))

      (when (and *helo-must-match-ip*
		 (not (string= (session-dotted sess) "127.0.0.1")))
	
	(multiple-value-bind (first ttl rest flags)
	    (dns-query text :search t :ignore-cache *ignore-dns-cache*)
	  (declare (ignore ttl))
	  (when (member :no-such-domain flags)
	    (if* (eq 't *helo-must-match-ip*)
	       then (inc-checker-stat connections-rejected-permanently
				      "HELO domain checker")
		    (outline sock "501 Invalid domain name")
		    (maild-log
		     "Rejected HELO ~A from client ~A (invalid domain)"
		     text (session-dotted sess))
		    (return :quit)
	       else ;; log only
		    (maild-log
		     "NOTE: HELO ~A from client ~A (invalid domain)"
		     text (session-dotted sess))))
	  
	  (when (null first)
	    (if* (eq 't *helo-must-match-ip*)
	       then (inc-checker-stat connections-rejected-temporarily
						      "HELO domain checker")
		    (outline sock "401 Unable to resolve domain")
		    (maild-log "Temporarily rejected client from ~A ~
because we couldn't resolve the name supplied in the HELO command (~A)" 
			       (session-dotted sess) text)
		    (return :quit)
	       else ;; log only
		    (maild-log "NOTE: couldn't resolve the name supplied ~
in the HELO command (~A) from client ~A"
			       text (session-dotted sess))))
	  
	  (let ((addrs (append (list first) rest)))
	    (when (not (member (smtp-remote-host sock) addrs))
	      (if* (eq 't *helo-must-match-ip*)
		 then ;; do it
		      (inc-checker-stat connections-rejected-permanently
					"HELO domain checker")
		      (outline sock
			       "501 HELO domain must match your IP address")
		      (maild-log
		       "Rejected HELO ~A from client ~A (No IP match)"
		       text (session-dotted sess))
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

	   (if (and *maxmsgsize* (> *maxmsgsize* 0))
	       (push (format nil "SIZE ~d" *maxmsgsize*) res))
	   
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

;; Should be compliant with RFC1869, except more liberal w/ spaces.
(defun parse-mail-cmd-parameter (string)
  (when (not (match-re "^\\s*$" string))
    (multiple-value-bind (matched whole keyword equal value)
	(match-re "^\\s*([a-zA-Z0-9][a-zA-Z0-9-]*)(=([^= ]*))?" string
		  :return :index)
      (declare (ignore equal))
      (if* matched
	 then (values t 
		      (subseq string (car keyword) (cdr keyword))
		      (subseq string (car value) (cdr value))
		      (subseq string (cdr whole)))
	 else :syntax))))
  
(defun parse-mail-cmd-parameters (string)
  (let (res)
    (loop
      (multiple-value-bind (status keyword value rest)
	  (parse-mail-cmd-parameter string)
	(if (null status)
	    (return))
	(if (eq status :syntax)
	    (return-from parse-mail-cmd-parameters :syntax))
	(push (cons keyword value) res)
	(setf string rest)))
    (nreverse res)))

;; Returns
;; from (or nil if error)
;; err string
;; incoming message size (if supplied)
(defun parse-mail-cmd (string)
  (multiple-value-bind (matched whole from rest)
      (match-re "^\\s+from:\\s*(\\S+)(.*)$" string :case-fold t)
    (declare (ignore whole))

    (if* (not matched)
       then (return-from parse-mail-cmd
	      (values nil "501 5.5.2 Syntax error in command")))
    
    ;; Now check for extension parameters which we support.
    (let ((params (parse-mail-cmd-parameters rest))
	  size)
      (if* (eq params :syntax)
	 then (values nil "501 5.5.2 Syntax error in command")
	 else (dolist (param params)
		(let ((keyword (car param))
		      (value (cdr param)))
		  (if* (equalp keyword "auth")
		     then t ;; just ignore
		   elseif (equalp keyword "size")
		     then (setf size (ignore-errors (parse-integer value)))
		     else (return-from parse-mail-cmd
			    (values 
			     nil 
			     (format nil "555 5.5.4 ~a parameter unrecognized"
				     keyword))))))
	      (values from nil size)))))

(defun smtp-mail (sess text)
  (block nil
    (let ((sock (session-sock sess)))
      (when (and *helo-must-match-ip* (not (session-helo sess)))
	(outline sock "503 5.0.0 Polite people say HELO first")
	(return))

      (when (session-from sess)
	(inc-checker-stat senders-rejected-permanently "protocol violation checker")
	(outline sock "503 5.5.0 Sender already specified")
	(return))

      (when (and (eq *client-authentication* :required)
		 (null (session-auth-user sess)))
	(outline sock "530 Authentication required")
	(return))
      
      ;; parse the command line
      (multiple-value-bind (addr errstring expected-size)
	  (parse-mail-cmd text)
	(when (null addr)
	  (inc-checker-stat senders-rejected-permanently 
			    "command syntax checker")
	  (outline sock "~a" errstring)
	  (maild-log "Client from ~a: Syntax error: MAIL ~a" (session-dotted sess) text)
	  (return))
	
	(setf addr (parse-email-addr addr :allow-null t))

	(when (null addr)
	  (inc-checker-stat senders-rejected-permanently 
			    "email address syntax checker")
	  (outline sock "553 5.3.0 Invalid address")
	  (maild-log "Client from ~a: MAIL ~A: Invalid address"
		     (session-dotted sess) text)
	  (return))
	
	(when (and expected-size *maxmsgsize* 
		   (> *maxmsgsize* 0) 
		   (> expected-size *maxmsgsize*))
	  (outline sock "~
552 5.2.3 Message size exceeds fixed maximum message size (~d)" 
		   *maxmsgsize*)
	  (return))
	
	(inc-smtp-stat transactions-started)
	
	;; Call checkers
	(when (not *postpone-checkers*)
	  (if (null (run-mail-from-checkers sock addr))
	      (return-from smtp-mail)))
	
	(setf (session-from sess) addr)
	(outline sock "250 2.1.0 ~A... Sender ok" (emailaddr-orig addr))))))

(defun run-mail-from-checkers (sock addr &key postponed)
  (dolist (checker *smtp-mail-from-checkers* t)
    (multiple-value-bind (status string)
	(funcall (second checker) (smtp-remote-host sock) addr)
      (ecase status
	(:err
	 (inc-checker-stat senders-rejected-permanently (first checker))
	 (outline sock "551 ~A" string)
	 (maild-log "~aChecker ~S rejected MAIL FROM:~A. Message: ~A"
		    (if postponed "Postponed " "")
		    (first checker) (emailaddr-orig addr)
		    string)
	 (return))
	(:transient
	 (inc-checker-stat senders-rejected-temporarily (first checker))
	 (outline sock "451 ~A" string)
	 (maild-log 
	  "~aChecker ~S temporarily failed MAIL FROM:~A. Message: ~A"
	  (if postponed "Postponed " "")
	  (first checker) (emailaddr-orig addr) string)
	 (return))
	(:sufficient
	 (return t))
	(:ok
	 )))))
	  

(defun smtp-rcpt (sess text)
  (block nil
    (let ((sock (session-sock sess))
	  disp errmsg)
      (when (>= (length (session-to sess)) *maxrecips*)
	(inc-checker-stat recips-rejected-temporarily "recipient count checker")
	(outline sock "452 4.5.3 Too many recipients")
	(maild-log "Client from ~A: Too many recipients"
		   (session-dotted sess))
	(return))
      (when (null (session-from sess))
	(inc-checker-stat recips-rejected-permanently "protocol violation checker")
	(outline sock "503 5.0.0 Need MAIL before RCPT")
	(return))
      (multiple-value-bind (matched whole addrstring)
	  (match-regexp "^\\b+to:\\b*\\(\\B\\B*\\)\\b*$" text :case-fold t)
	(declare (ignore whole))
	(when (not matched)
	  (inc-checker-stat recips-rejected-permanently "command syntax checker")
	  (outline sock "501 5.5.2 Syntax error in command")
	  (return))
	(let ((addr (parse-email-addr addrstring)))
	  (when (null addr)
	    (inc-checker-stat recips-rejected-permanently "email address syntax checker")
	    (outline sock "553 5.3.0 Invalid address")
	    (maild-log "Client from ~A: rcpt to:~A... Invalid address"
		       (session-dotted sess)
		       addrstring)
	    (return))
	  
	  (multiple-value-setq (disp errmsg)
	    (get-recipient-disposition addr))
	  (ecase disp
	    (:error
	     (smtp-rcpt-to-delay sess)
	     (inc-checker-stat recips-rejected-permanently ":error recip checker")
	     (outline sock "550 ~a... ~a" 
		      (emailaddr-orig addr) errmsg)
	     (maild-log "client from ~A: rcpt to:~A... error recip"
			(session-dotted sess)
			(emailaddr-orig addr))
	     (return))
	    (:local-unknown
	     (smtp-rcpt-to-delay sess)
	     (inc-checker-stat recips-rejected-permanently "unknown recip checker")
	     (outline sock "550 5.1.1 ~a... User unknown"
		      (emailaddr-orig addr))
	     (maild-log "Client from ~A: rcpt to:~A... User unknown"
			(session-dotted sess)
			(emailaddr-orig addr))
	     (return))
	    ((:local-ok :non-local)
	     ))
	  
	  ;; Run checkers
	  (when (not *postpone-checkers*)
	    (if (null (run-rcpt-to-checkers sock sess addr disp))
		(return-from smtp-rcpt)))
	  
	  (push addr (session-to sess))
	  (outline sock "250 2.1.5 ~a... Recipient ok" 
		   (emailaddr-orig addr)))))))

(defun run-rcpt-to-checkers (sock sess addr disp &key postponed)
  (case disp
    (:local-ok
     (setf disp :local))
    (:non-local
     (setf disp :remote)))
  
  (dolist (checker *smtp-rcpt-to-checkers* t) 
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
	 (smtp-rcpt-to-delay sess)
	 (inc-checker-stat recips-rejected-permanently (first checker))		 
	 (outline sock "550 ~A" string)
	 (maild-log "~aChecker ~S rejected RCPT TO:~A. Message: ~A"
		    (if postponed "Postponed " "")
		    (first checker) (emailaddr-orig addr)
		    string)
	 (return))
	(:transient
	 (smtp-rcpt-to-delay sess)
	 (inc-checker-stat recips-rejected-temporarily (first checker))
	 (outline sock "451 ~A" string)
	 (maild-log 
	  "~aChecker ~S temporarily failed RCPT TO:~A. Message: ~A"
	  (if postponed "Postponed " "")
	  (first checker) (emailaddr-orig addr) string)
	 (return))
	(:sufficient
	 (return t))
	(:ok
	 )))))


(defun smtp-rcpt-to-delay (sess)
  (sleep (session-rcpt-to-delay sess))
  (setf (session-rcpt-to-delay sess) (* (session-rcpt-to-delay sess) 2)))

(defmacro with-session-reset-on-unwind ((sess) &body body)
  `(unwind-protect (progn ,@body)
     (reset-smtp-session ,sess)))
  
(defun smtp-data (sess text)
  (declare (ignore text))
  (let* ((sock (session-sock sess))
	 (dotted (session-dotted sess))
	 q status headers err complaint err-string smtp-size)
    (declare (ignore-if-unused err-string))
    ;; Check for protocol violations.
    (if (null (session-from sess))
	(return-from smtp-data 
	  (outline sock "503 5.0.0 Need MAIL command")))
    (if (null (session-to sess))
	(return-from smtp-data 
	  (outline sock "503 5.0.0 Need RCPT (recipient)")))

    (with-session-reset-on-unwind (sess)
      ;; Run pre-checkers
      (when (not *postpone-checkers*)
	(if (null (run-pre-data-checkers sock sess))
	    (return-from smtp-data)))
	
      
      (with-new-queue (q f err (session-from sess) (smtp-remote-host sock))
	(outline sock "354 Enter mail, end with \".\" on a line by itself")
	(handler-case 
	    (multiple-value-setq 
		(status headers complaint err-string smtp-size)
	      (read-message-stream sock f :smtp t))
	  (socket-error (c)
	    (if* (eq (stream-error-identifier c) :read-timeout)
	       then (maild-log "Client from ~A stopped transmitting." dotted)
	       else (maild-log "Client from ~A: socket error: ~A" dotted c))
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
	
	(when complaint
	  (inc-checker-stat messages-rejected-permanently complaint)
	  (outline sock "552 ~a" complaint)
	  (maild-log "~a: Message data rejected: ~a" dotted complaint)
	  (return-from smtp-data))
	
	(queue-prefinalize q (session-to sess) headers
			   :metoo *metoo*
			   :smtp-size smtp-size
			   :helo (session-helo sess)
			   )
	
	;; Run through smtp-specific checkers.
	(dolist (checker *smtp-data-checkers*)
	  (multiple-value-bind (status string)
	      (funcall (second checker) sess q)
	    (ecase status
	      ((:err :reject)
	       (inc-checker-stat messages-rejected-permanently (first checker))
	       (outline sock "551 ~A" string)
	       (maild-log "Checker ~S rejected message data. Message: ~A"
			  (first checker) string)
	       (return-from smtp-data))
	      (:transient
	       (inc-checker-stat messages-rejected-temporarily (first checker))
	       (outline sock "451 ~A" string)
	       (maild-log 
		"Checker ~S temporarily failed message. Message: ~A"
		(first checker) string)
	       (return-from smtp-data))
	      (:ok
	       ))))

	;; Run postponed checkers.
	(if (and *postpone-checkers* (null (run-postponed-checkers sock sess)))
	    (return-from smtp-data))
	
	;; Run through non-smtp-specific checkers.
	(multiple-value-bind (res text checker)
	    (check-message-checkers q)
	  (ecase res
	    (:ok
	     ) ;; all is well
	    (:reject
	     (inc-checker-stat messages-rejected-permanently checker)
	     (maild-log "Rejecting message from ~A" 
			(emailaddr-orig (session-from sess)))
	     (maild-log "Checker ~S said: ~A" checker text)
	     (outline sock "552 ~A" text)
	     (return-from smtp-data))
	    (:transient
	     (inc-checker-stat messages-rejected-temporarily checker)
	     (maild-log "Checker ~s reported a transient error: ~A"
			checker text)
	     (outline sock "452 ~a" text)
	     (return-from smtp-data))))
	
	;; This finalizes and unlocks the queue item.
	(queue-finalize q (session-to sess) headers :date t :metoo *metoo*))
      ;; outside of with-new-queue.
      
      ;; At this point, the queue file is saved on disk.. or it has been
      ;; deleted due to an error/rejection in processing.
    
      ;; Rejected messages have already been reported to the client.
      ;; Report transient errors now.
    
      (when (or (eq err :transient)
		(and (listp err) (eq (first err) :transient)))
	(outline 
	 sock "451 Requested action aborted: local error in processing")
	(return-from smtp-data))
    
      ;; We have a good message.  Report it to the client and begin
      ;; delivering it.

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
	  #'queue-process-single (queue-id q))))))

(defun run-pre-data-checkers (sock sess &key postponed)
  (dolist (checker *smtp-data-pre-checkers* t)
    (multiple-value-bind (status string)
	(funcall (second checker) sess
		 (smtp-remote-host sock) (session-from sess) 
		 (session-to sess))
      (ecase status
	(:err
	 (inc-checker-stat messages-pre-rejected-permanently (first checker))
	 (outline sock "551 ~A" string)
	 (maild-log "~aChecker ~S rejected DATA phase initiation. Message: ~A"
		    (if postponed "Postponed " "")
		    (first checker) string)
	 (return))
	(:transient
	 (inc-checker-stat messages-pre-rejected-temporarily (first checker))
	 (outline sock "451 ~A" string)
	 (maild-log 
	  "~aChecker ~S temporarily failed DATA phase initiation. Message: ~A"
	  (if postponed "Postponed " "")
	  (first checker) string)
	 (return))
	(:sufficient
	 (return t))
	(:ok
	 )))))

(defun run-postponed-checkers (sock sess)
  (block nil
    (if (null (run-connection-checkers sock :postponed t))
	(return))
    (if (null (run-mail-from-checkers sock (session-from sess) :postponed t))
	(return))
    (dolist (addr (session-to sess))
      (if (null (run-rcpt-to-checkers sock sess addr 
				      (get-recipient-disposition addr) 
				      :postponed t))
	  (return-from run-postponed-checkers)))
    (if (null (run-pre-data-checkers sock sess :postponed t))
	(return))
    
    t))
