(in-package :user)

(defstruct queue 
  id
  ctime ;; when this message was queued
  status
  from
  recips ;; remaining recipients to be processed
  headers
  )

(defmacro queue-filename-from-id (id)
  `(concatenate 'string *queuedir* "/qf" ,id))

(defmacro queue-lockfile-from-id (id)
  `(concatenate 'string *queuedir* "/qf" ,id ".lck"))


(defmacro queue-filename (queue)
  `(queue-filename-from-id (queue-id ,queue)))

(defmacro queue-datafile (queue)
  `(concatenate 'string *queuedir* "/df" (queue-id ,queue)))


(defun queue-expand-aliases (recips)
  (let (expandedrecips)
    (dolist (recip recips)
      (if (local-domain-p recip)
	  (let ((expanded (lookup-recip-in-aliases recip :parsed t)))
	    (setf expandedrecips
	      (nconc expandedrecips
		     (if expanded 
			 expanded 
		       (list recip)))))
	(push recip expandedrecips)))
    (queue-dedupe-recips expandedrecips)))
    
(defun queue-dedupe-recips (recips)
  (let (seen res)
    (dolist (recip recips)
      (if* (not (member recip seen :test #'emailaddr=))
	 then
	      (push recip res)
	      (push recip seen)))
    res))

;; Returns the locked queue struct
(defun make-queue-file (from recips)
  (let ((prefix (format nil "~A/tf" *queuedir*))
	id
	tmpfile
	queue)
    (loop
      (multiple-value-bind (f filename)
	  (mkstemp (concatenate 'string prefix "XXXXXX"))
	(setf tmpfile filename)
	(close f)
	(setf id (subseq filename (length prefix)))
	
	;; We've safely selected a possible id.  Make sure it's not used
	;; on any qf.  
	(if (not (queue-exists-p id))
	    (return)) ;; break from the loop
	
	(delete-file filename)))
    
    ;; we now have a tf file which has put a claim on our id.
    ;; Now create the regular lockfile so that we don't have
    ;; to worry about the file be processed while it's still
    ;; being created.
    (queue-lock id)
    (delete-file tmpfile)

    (setf queue
      (make-queue 
       :id id
       :ctime (get-universal-time)
       :status "Fresh"
       :from from
       :recips (queue-expand-aliases recips)))
    (update-queue-file queue)
    queue))
    

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
    (rename-file tmpfile filename)))

      
(defun remove-queue-file (q)
  (if (probe-file (queue-filename q))
      (delete-file (queue-filename q)))
  (if (probe-file (queue-datafile q))
      (delete-file (queue-datafile q)))
  (queue-unlock q))


(defun queue-init-headers (queue headers cliaddr &key date add-from from-gecos)
  (setf (queue-headers queue)
    (cons (make-received-header cliaddr (queue-id queue))
	  headers))
  (if (null (locate-header "Message-Id:" headers))
      (setf (queue-headers queue)
	(append (queue-headers queue) 
		(list (make-message-id-header (queue-id queue))))))
  (if (and date (null (locate-header "Date:" headers)))
      (setf (queue-headers queue)
	(append (queue-headers queue) 
		(list (make-date-header)))))
  (if (and add-from (null (locate-header "From:" headers)))
      (setf (queue-headers queue)
	(append (queue-headers queue) 
		(list (make-from-header (queue-from queue) from-gecos)))))
  
  (update-queue-file queue))

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
       (with-lock-file-nowait (,lockfilevar :locked)
	 (if (not (queue-exists-p ,idvar))
	     ,noexistform
	   (let ((,qvar (queue-read ,idvar)))
	     ,@body))))))


(defun queue-process-single (id &key wait (if-does-not-exist :error))
  (let ((res (with-locked-queue (q id :noexist)
	       (queue-process-single-help q :wait wait))))
    (case res
      (:locked
	(if *debug*
	    (format t "Queue ID ~A is locked.~%" id))
	:locked)
      (:noexist
       (ecase if-does-not-exist
	 (:error 
	  (error "queue-process-single: Queue ID ~A does not exist" id))
	 (:ignore
	  :noexist)))
      (t
       res))))
  
(defun queue-process-single-help (q &key wait)
  (block nil
    (maild-log "Processing queue id ~A" (queue-id q))
    ;; Sanity check
    (if (not (probe-file (queue-datafile q)))
	(error "Queue id ~A doesn't have a data file!" (queue-id q)))
    
    
    ;; XXX --  This really needs work.   We want to take advantage of 
    ;; multiple recips per SMTP session... and also need to avoid
    ;; sending more than one timeout bounce if there are multiple
    ;; undelivered-to recips left.
    (dolist (recip (queue-recips q))
      (setf (queue-status q)
	(format nil "Working on delivery to ~A" (emailaddr-orig recip)))
      ;; XXX -- will probably want to make this only happen
      ;; every so often
      (update-queue-file q)

      (multiple-value-bind (status response)
	  (if (local-domain-p recip)
	      (deliver-local (emailaddr-user recip) q)
	    (deliver-smtp recip q))
	(case status
	  (:delivered
	   (setf (queue-recips q) (remove recip (queue-recips q))))
	  (:fail
	   (maild-log "delivery to ~A failed: ~A" (emailaddr-orig recip)
		      response)
	   (maild-log "Sending bounce to ~A" (emailaddr-orig (queue-from q)))
	   (create-bounce q (list recip) response :wait wait)
	   (setf (queue-recips q) (remove recip (queue-recips q))))
	  (t
	   (maild-log "deliver status for ~a is ~s." 
		      (emailaddr-orig recip) status)
	   (if response
	       (maild-log "Error message is: ~A" response))))))
    
    ;; Done processing recips.
    
    (when (queue-undeliverable-timeout-p q)
      (maild-log "Bouncing queue id ~A due to queue timeout" (queue-id q))
      (create-bounce q (queue-recips q) 
		     (format nil "Could not deliver message for ~D days"
			     *bounce-days*))
      (remove-queue-file q)
      (return))
    
    (when (null (queue-recips q))
      ;; Everything has been delivered.  Clean up
      (maild-log "Completed final delivery for queue id ~A" 
		 (queue-id q))
      (remove-queue-file q) ;; unlocks as well
      (return))
    
    (setf (queue-status q)
      "some failed recipients")
    (update-queue-file q)
    (maild-log "Terminating processing of queue id ~A" (queue-id q))))


(defun queue-undeliverable-timeout-p (q)
  (> (get-universal-time)
     (+ (queue-ctime q) (* *bounce-days* 86400))))
      

(defun get-all-queue-ids ()
  (let* ((prefix (concatenate 'string *queuedir* "/qf"))
	 (prefixlen (length prefix))
	 res)
    (dolist (p (directory (concatenate 'string prefix "*")))
      ;; skip .lck files.
      (if (string/= "lck" (pathname-type p))
	  (push (subseq (namestring p) prefixlen) res)))
    res))

(defun queue-list ()
  (dolist (id (get-all-queue-ids))
    (let ((q (ignore-errors (queue-read id))))
      (when q
	(format t 
		"~%Id: ~A~A~% Date: ~A~% Status: ~A~% Sender: ~A~% Remaining recips: ~A~%"
		id
		(if (queue-locked-p q) " (LOCKED)" "")
		(ctime (queue-ctime q))
		(queue-status q)
		(emailaddr-orig (queue-from q))
		(list-to-delimited-string
		 (mapcar #'emailaddr-orig (queue-recips q))
		 #\,))
	(if (not (probe-file (queue-datafile q)))
	    (format t "Queue id ~A doesn't have a data file!~%" id))))))


(defun queue-process-all ()
  (dolist (id (get-all-queue-ids))
    (handler-case (queue-process-single id :if-does-not-exist :ignore)
      (error (c)
	(maild-log "Got error ~A while processing queue id ~A"
		   c id)))))

