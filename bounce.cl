(in-package :user)

(defun create-bounce (oldq failed-recips err &key wait)
  ;; If this is a double-bounce, send the message to postmaster.
  (let* ((original-sender (queue-from oldq))
	 (bounce-to original-sender)
	 (subject "Subject: Returned mail"))
    (when (emailnullp original-sender)
      (setf bounce-to (parse-email-addr "postmaster"))
      (setf subject "Subject: Postmaster notify"))
    (let ((q (make-queue-file (parse-email-addr "<>" :allow-null t)
			      (list bounce-to))))
      (with-open-file (f (queue-datafile q)
		       :direction :output
		       :if-does-not-exist :create)
	(fchmod f #o0600)
	(write-line
	 "   ----- The following addresses had permanent fatal errors -----"
	 f)
	(dolist (recip failed-recips)
	  (write-line (emailaddr-orig recip) f))
	(write-line "" f)
	(write-line "   ----- Transcript of session follows -----" f)
	(write-line err f)
	(write-line "" f)
      
	(write-line 
	 "   ----- Original message follows -----" f)
	(write-message-to-stream f oldq :norewrite :noclose t)
      
	(finish-output f)
	(fsync f)
      
	(queue-init-headers 
	 q 
	 (list subject  (format nil "To: ~a" (emailaddr-orig bounce-to)))
	 (dotted-to-ipaddr "127.0.0.1")
	 :date t
	 :add-from t 
	 :from-gecos "Mail Delivery Subsystem"))
    
      (queue-unlock q)
      (let ((bounce-proc (mp:process-run-function "Sending bounce" 
			   #'queue-process-single (queue-id q))))
	(if wait
	    (wait-for-process 
	     bounce-proc 
	     (format nil "Waiting for bounce message to be sent to ~A"
		     (emailaddr-orig bounce-to))))))))

