(in-package :user)

(defun queue-process-single (id &key wait verbose (if-does-not-exist :error))
  (let ((res (with-locked-queue (q id :noexist)
	       (queue-process-single-help q :wait wait :verbose verbose))))
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
  
(defun queue-process-single-help (q &key wait verbose)
  (block nil
    (maild-log "Processing queue id ~A" (queue-id q))
    ;; Sanity check
    (if (not (probe-file (queue-datafile q)))
	(error "Queue id ~A doesn't have a data file!" (queue-id q)))
    
    ;; XXX --  This really needs work.   We want to take advantage of 
    ;; multiple recips per SMTP session... and also need to avoid
    ;; sending more than one timeout bounce if there are multiple
    ;; undelivered-to recips left.
    (with-output-to-string (errmsgs)
      (dolist (recip (queue-recips q))
	(setf (queue-status q)
	  (format nil "Working on delivery to ~A" (emailaddr-orig recip)))
	;; XXX -- May want to move this outside the loop.
	(update-queue-file q)

	(multiple-value-bind (status response)
	    (if (local-domain-p recip)
		(deliver-local (emailaddr-user recip) q :verbose verbose)
	      (deliver-smtp recip q :verbose verbose))
	  (case status
	    (:delivered
	     (setf (queue-recips q) (remove recip (queue-recips q))))
	    (:fail
	     (maild-log "delivery to ~A failed: ~A" (emailaddr-orig recip)
			response)
	     (maild-log "Sending bounce to ~A" (emailaddr-orig (queue-from q)))
	     (create-bounce q (list recip) response :wait wait)
	     (setf (queue-recips q) 
	       (delete recip (queue-recips q))))
	    (t
	     (maild-log "delivery status for ~a is ~s." 
			(emailaddr-orig recip) status)
	     (if* response
		then
		     (format errmsgs "~A: ~A~%" (emailaddr-orig recip)
			     response)
		     (maild-log "Error message is: ~A" response))))))
      
      ;; Done processing recips. 
      (when (null (queue-recips q))
	;; Everything has been delivered.  Clean up
	(maild-log "Completed final delivery for queue id ~A" (queue-id q))
	(remove-queue-file q) 
	(return))
      
      (setf (queue-status q) (get-output-stream-string errmsgs))
      (update-queue-file q)
      
      (when (queue-undeliverable-timeout-p q)
	(maild-log "Bouncing queue id ~A due to queue timeout" (queue-id q))
	(create-bounce q (queue-recips q) 
		       (format nil "Could not deliver message for ~D days"
			       *bounce-days*))
	(remove-queue-file q)
	(return))
      
      (maild-log "Terminating processing of queue id ~A" (queue-id q)))))


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
	(format t "~%Id: ~A" id)
	(if (queue-locked-p q)
	    (format t " (LOCKED)"))
	(if (not (queue-valid q))
	    (format t " (Incomplete)"))
	(terpri)
	(if (not (probe-file (queue-datafile q)))
	    (format t "Queue id ~A doesn't have a data file!~%" id))
	(format t " Date queued: ~A~%" (ctime (queue-ctime q)))
	(format t " Sender: ~A~%" (emailaddr-orig (queue-from q)))
	(format t " Remaining recips: ~A~%"
		(list-to-delimited-string
		 (mapcar #'emailaddr-orig (queue-recips q))
		 #\,))
	(format t " Status: ~A~%" (queue-status q))))))


(defun queue-process-all (&key verbose)
  (dolist (id (get-all-queue-ids))
    (handler-case (queue-process-single id :if-does-not-exist :ignore
					:verbose verbose)
      (error (c)
	(maild-log "Got error ~A while processing queue id ~A"
		   c id)))))

(defun queue-process-daemon (interval)
  ;; sanity check
  (if (or (null interval) (<= interval 0))
      (error "queue-process-daemon: Invalid interval: ~S" interval))
  (mp:process-run-function "Queue daemon"
    #'(lambda ()
	(maild-log "Queue process internal: ~D seconds" interval)
	(loop
	  (queue-process-all)
	  (sleep interval)))))

