(in-package :user)

(defun trusted-user-p (us)
  (member us *trusted-users* :test #'equalp))

;; returns (values parsed-from-address from-name real-user)
(defun compute-sender-info (user-fromaddr user-gecos)
  (let* ((uid (getuid)) ;; the real uid
	 (pwent (getpwuid uid))
	 (auth-warn t)
	 gecos parsedfromaddr )
    (if (null pwent)
	(error "I can't figure out who you are! (uid=~D)" uid))
    
    ;; It is okay for gecos to end up being nil.  This means
    ;; that no human-friendly name will be in the From: 
    ;; header.
    (if* (null user-fromaddr)
       then
	    (setf user-fromaddr (pwent-name pwent))
	    (setf gecos (pwent-gecos pwent))
	    (setf auth-warn nil))
    
    (setf parsedfromaddr 
      (parse-email-addr user-fromaddr :allow-null t))
    (if (null parsedfromaddr)
	(error "~A is not a valid address" user-fromaddr))
    
    ;; don't need to look up gecos info if the fromaddr
    ;; is not local.. or if user-gecos is specified.
    (if (and (local-domain-p parsedfromaddr) (null user-gecos))
	(let ((newpwent (string-downcase (getpwnam (emailaddr-user parsedfromaddr)))))
	  (if* newpwent
	     then
		  (setf gecos (pwent-gecos newpwent))
		  (if (string= (pwent-name pwent) (pwent-name newpwent))
		      (setf auth-warn nil)))))

    (if user-gecos
	(setf gecos user-gecos))
    
    (values 
     parsedfromaddr 
     gecos 
     (if (trusted-user-p (pwent-name pwent))
	 nil
       auth-warn)
     (pwent-name pwent))))
  

(defun send-from-stdin (recips &key (dot t) gecos from)
  (multiple-value-bind (fromaddr gecos authwarn realuser)
      (compute-sender-info from gecos)
    (let ((q (make-queue-file fromaddr recips))
	  status headers msgsize)
      (with-open-file (f (queue-datafile q)
		       :direction :output
		       :if-does-not-exist :create)
	(fchmod f #o0600)
	(multiple-value-setq (status headers msgsize)
	  (read-message-stream t f :dot dot))

	(if (not (member status '(:eof :dot)))
	    (error "got status ~s from read-message-stream" status))
	(finish-output f)
	(fsync f)

	(if authwarn
	    (setf headers 
	      (append headers 
		      (list (make-x-auth-warning-header realuser fromaddr)))))
	(queue-init-headers q headers (dotted-to-ipaddr "127.0.0.1")
			    :date t
			    :add-from t 
			    :from-gecos gecos))
	
      (if (and *maxmsgsize* (> *maxmsgsize* 0) (>= msgsize *maxmsgsize*))
	  (error "Message exceeded max message size (~D)" *maxmsgsize*))
      (queue-unlock q)
      ;; XXX -- this might need changing.
      ;; I think sendmail goes through and processes local recips
      ;; and returns immediate errors (dead.letter).. then forks
      ;; to handle the rest.. which may potentially be slower.
      ;; For now, we're going to fork for any type of message.
      (let ((pid (if *debug* 0 (fork))))
	(if (= pid 0)
	    (queue-process-single (queue-id q) :wait t))))))
		  

(defun read-message-stream (s bodystream &key smtp dot)
  (let ((count 0)
	(doingheaders t)
	(firstline t)
	(buffer (make-string *maxdatalinelen*))
	lastincomplete
	headers)
    (if smtp 
	(setf dot t))
    (loop
      (multiple-value-bind (endpos maxed)
	  (read-message-stream-line s buffer
				    :timeout (if smtp *datatimeout*))
	(if (eq endpos :eof)
	    (return (values :eof (reverse headers) count)))
	
	(if (and dot
		 (not lastincomplete) 
		 (= endpos 1) 
		 (char= (schar buffer 0) #\.))
	    (return (values :dot (reverse headers) count)))

	(incf count endpos)
	
	(if* doingheaders
	   then
		(cond
		 ((and firstline 
		       (not (valid-header-line-p buffer endpos :strict t)))
		  (setf doingheaders nil)
		  (read-message-stream-write-body 
		   bodystream buffer endpos count 
		   :nonewline maxed
		   :smtp smtp))
		 ((= endpos 0) ;; blank line
		  (setf doingheaders nil)
		  (if (null headers)
		      (write-char #\newline bodystream)))
		 ((valid-header-line-p buffer endpos)
		  (push (subseq buffer 0 endpos) headers))
		 (t ;; non-header
		  (setf doingheaders nil)
		  (read-message-stream-write-body 
		   bodystream buffer endpos count
		   :nonewline maxed
		   :smtp smtp)))
		
		(setf firstline nil)
	   else
		;; doing body.
		(read-message-stream-write-body 
		 bodystream buffer endpos count
		 :nonewline maxed
		 :smtp smtp))
	
	(setf lastincomplete maxed)))))
		

(defun read-message-stream-write-body (s buffer endpos count 
				       &key nonewline smtp)
  (when (or (null *maxmsgsize*) (<= *maxmsgsize* 0) (< count *maxmsgsize*))
    (write-string buffer s 
		  :start 
		  (if (and smtp 
			   (>= endpos 2) 
			   (string= (subseq buffer 0 2) ".."))
		      1 0)
		  :end endpos)
    (if (null nonewline)
	(write-char #\newline s))))

	
(defun read-message-stream-line (s buffer &key timeout)
  (with-socket-timeout (s :read timeout)
    (read-message-stream-line-inner s buffer)))


(defun read-message-stream-line-inner (s buffer)
  (let ((pos 0)
	(max (length buffer))
	lastchar
	char)
    (loop
      (when (>= pos max)
	(unread-char lastchar s)
	(return (values (1- pos) :max)))
      
      (setf char (read-char s nil nil))
      
      ;;; if we're in the middle of a line, treat EOF as EOL
      (if (null char)
	  (return (if (= pos 0) :eof pos)))
      
      (when (eq char #\linefeed)
	(if (and (eq lastchar #\return) (> pos 0))
	    (decf pos))
	(return pos))
      
      (setf (schar buffer pos) char)
      (setf lastchar char)
      (incf pos))))
