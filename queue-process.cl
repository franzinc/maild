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
    
    ;; XXX --  This needs work.   We want to take advantage of 
    ;; multiple recips per SMTP session.
    (let (failed-recips recip-addr recip-printable recip-type)
      (dolist (recip (queue-recips q))
	(setf recip-addr (recip-addr recip))
	(setf recip-printable (recip-printable recip))
	(setf recip-type (recip-type recip))
	(if (null (recip-owner recip))
	    (setf (recip-owner recip) (queue-from q)))
	(setf (queue-status q)
	  (format nil "Working on delivery to ~A" recip-printable))
	(setf (recip-status recip) "Attempting delivery")

	;; XXX -- May want to move this outside the loop.
	(update-queue-file q)
	  
	(multiple-value-bind (status response)
	    (if (or (member recip-type '(:file :prog))
		    (local-domain-p recip-addr))
		(deliver-local recip q :verbose verbose)
	      (deliver-smtp recip q :verbose verbose))
	  (case status
	    (:delivered
	     (setf (queue-recips q) (delete recip (queue-recips q))))
	    (:fail
	     (maild-log "delivery to ~A failed: ~A" 
			recip-printable response)
	     (setf (recip-status recip)
	       (format nil "~A: ~A" recip-printable response))
	     (push recip failed-recips)
	     (setf (queue-recips q) 
	       (delete recip (queue-recips q))))
	    ;; Everything else is treated as a transient problem
	    (t
	     (maild-log "delivery status for ~a is ~s." 
			recip-printable status)
	     (when response
	       (setf (recip-status recip)
		 (format nil "~A: ~A" recip-printable response))
	       (maild-log "Error message is: ~A" response))))))
      
      ;; See if we need to send bounces
      (if failed-recips
	  (bounce q failed-recips :wait wait))
	
      ;; Done processing recips. 
      (when (null (queue-recips q))
	;; Everything has been delivered.  Clean up
	(maild-log "Completed final delivery for queue id ~A" (queue-id q))
	(remove-queue-file q) 
	(return))
      
      (when (queue-undeliverable-timeout-p q)
	(maild-log "Bouncing queue id ~A due to queue timeout" (queue-id q))
	(bounce q (queue-recips q) :wait wait :undeliverable t)
	(remove-queue-file q)
	(return))
      
      (setf (queue-status q) "Will try during next queue run")
      (update-queue-file q)
      
      (maild-log "Terminating processing of queue id ~A" (queue-id q)))))

(defun queue-undeliverable-timeout-p (q)
  (> (get-universal-time)
     (+ (queue-ctime q) (* *bounce-days* 86400))))
      

;; XXX -- todo: sort the directory list so that queue items
;; will be processed in the order which they were created.
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
		 (mapcar #'recip-printable (queue-recips q))
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

