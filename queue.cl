(in-package :user)

(defstruct queue 
  id
  (ctime (get-universal-time)) ;; when this message was queued
  (status "Reading message data...")
  from
  recips ;; remaining recipients to be processed (list of recip structs)
  orig-recips ;; parsed email addrs, pre-alias expansion
  headers
  valid) ;; not valid until the data file has been written

(defmacro queue-filename-from-id (id)
  `(concatenate 'string *queuedir* "/qf" ,id))

(defmacro queue-lockfile-from-id (id)
  `(concatenate 'string *queuedir* "/qf" ,id ".lck"))


(defmacro queue-filename (queue)
  `(queue-filename-from-id (queue-id ,queue)))

(defmacro queue-datafile (queue)
  `(concatenate 'string *queuedir* "/df" (queue-id ,queue)))


;; The sequence file holds the last used sequence number.
(defun create-queue-id ()
  (let ((seqnum 0)
	(lockfile (format nil "~A/.seq.lck" *queuedir*)))
    (with-lock-file (lockfile nil :wait t)
      (with-os-open-file (f (format nil "~A/.seq" *queuedir*)
			    (logior *o-rdwr* *o-creat*) #o0600)
	(if (> (file-length f) 0)
	    ;; read last sequence number.
	    (setf seqnum (parse-integer (file-contents f))))
	(incf seqnum)
	(file-position f 0)
	(format f "~d" seqnum)))
    (format nil "~12,'0d" seqnum)))


(defun make-queue-file (from)
  (let ((status :try-again)
	id lockfile qfile q)
    (while (eq status :try-again)
      (setf id (create-queue-id))
      (setf lockfile (queue-lockfile-from-id id))
      (setf qfile (queue-filename-from-id id))
      (when (lock-file lockfile) 
	(unwind-protect
	    (when (null (probe-file qfile))
	      (setf q (make-queue :id id :from from))
	      (update-queue-file q) ;; write it out
	      (setf status :ok))
	  ;; cleanup forms
	  (when (eq status :try-again)
	    (delete-file lockfile)))))
    q))
  

(defun update-queue-file (queue)
  (let ((filename (queue-filename queue))
	(tmpfile (format nil "~A/tempqf~A" *queuedir* (queue-id queue)))
	(*print-pretty* t)) ;; change to nil after debugging
    (with-open-file (f tmpfile 
		     :direction :output
		     :if-does-not-exist :create)
      (fchmod f #o0600)
      (write queue :stream f)
      (finish-output f)
      (fsync f))
    (rename tmpfile filename)))

      
(defun remove-queue-file (q)
  (if (probe-file (queue-filename q))
      (delete-file (queue-filename q)))
  (if (probe-file (queue-datafile q))
      (delete-file (queue-datafile q)))
  (queue-unlock q))


;; recips is a list of email addresses or a list of recip structs
(defun queue-finalize (q recips headers cliaddr &key date add-from from-gecos)
  (let ((emailaddrs 
	 (if (emailaddr-p (first recips))
	     recips
	   (mapcar #'recip-addr recips))))
    
    (setf (queue-orig-recips q) emailaddrs)
    (setf (queue-recips q) 
      (expand-addresses emailaddrs (queue-from q))))
  
  ;; Add in any necessary headers.
  (setf (queue-headers q)
    (cons 
     (make-received-header cliaddr (queue-id q) (queue-orig-recips q))
     headers))
  (if (null (locate-header "Message-Id:" headers))
      (setf (queue-headers q)
	(append (queue-headers q) 
		(list (make-message-id-header (queue-id q))))))
  (if (and date (null (locate-header "Date:" headers)))
      (setf (queue-headers q)
	(append (queue-headers q) 
		(list (make-date-header)))))
  (if (and add-from (null (locate-header "From:" headers)))
      (setf (queue-headers q)
	(append (queue-headers q) 
		(list (make-from-header (queue-from q) from-gecos)))))
  
  (setf (queue-valid q) t)
  (update-queue-file q))

;; reads a queue file.  Doesn't lock.
(defun queue-read (id)
  (with-open-file (f (queue-filename-from-id id))
    (read f)))

(defun queue-lock (id)
  (lock-file (queue-lockfile-from-id id)))

(defun queue-unlock (q)
  (let ((lockfile (queue-lockfile-from-id (queue-id q))))
    (if (probe-file lockfile)
	(delete-file lockfile))))

(defun queue-locked-p (q)
  (let ((lockfile (queue-lockfile-from-id (queue-id q))))
    (probe-file lockfile)))

(defun queue-exists-p (id)
  (probe-file (queue-filename-from-id id)))
1
;; locks and reads.  Returns nil if it couldn't get a lock.
(defun queue-get (id)
    (if (null (queue-lock id))
	nil
      ;; got lock
      (queue-read id)))

;; write out and unlock
(defun queue-put (q) 
  (update-queue-file q)
  (queue-unlock q))

(defmacro with-locked-queue ((qvar id noexistform) &body body)
  (let ((lockfilevar (gensym))
	(idvar (gensym)))
    `(let* ((,idvar ,id)
	    (,lockfilevar (queue-lockfile-from-id ,idvar)))
       (with-lock-file (,lockfilevar :locked)
	 (if (not (queue-exists-p ,idvar))
	     ,noexistform
	   (let ((,qvar (queue-read ,idvar)))
	     ,@body))))))

;; 'q' and 'errvar' should be variables that are already in scope.  
;; shame on me.
(defmacro with-new-queue ((q streamvar errvar from) &body body)
  `(let (,streamvar)
     (setf ,q (make-queue-file ,from))
     (unwind-protect
	 (block nil
	   (handler-case 
	       (setf ,streamvar 
		 (open (queue-datafile ,q) 
		       :direction :output
		       :if-does-not-exist :create))
	     (error (c)
	       (maild-log "Failed to open queue datafile ~A: ~A"
			  (queue-datafile q) c)
	       (setf ,errvar (list :transient c))
	       (return)))
	   
	     ;; file is open now.  Make sure it always gets closed.
	     (with-already-open-file (,streamvar)
	       (fchmod ,streamvar #o0600) 
	       ,@body))
       ;; cleanup forms
       (queue-unlock ,q)
       (if (not (queue-valid ,q))
	   (remove-queue-file ,q)))))
