(in-package :user)

;;; XXX -- message timeout bounce should look somewhat different.
;;;   it needs to complain about not being able to send.. and show
;;    the reasons why.
(defun create-bounce (oldq failed-recips err &key wait)
  ;; If this is a double-bounce, send the message to postmaster.
  (let* ((original-sender (queue-from oldq))
	 (bounce-to original-sender)
	 (subject "Subject: Returned mail")
	 q errstatus)
    (when (emailnullp original-sender)
      (setf bounce-to (parse-email-addr "postmaster"))
      (setf subject "Subject: Postmaster notify"))
    (with-new-queue (q f errstatus (parse-email-addr "<>" :allow-null t))
      (write-line
       "   ----- The following addresses had permanent fatal errors -----"
       f)
      (dolist (recip failed-recips)
	(write-line (emailaddr-orig recip) f))
      (write-line "" f)
      (write-line "   ----- Transcript of session follows -----" f)
      (write-line err f)
      (write-line "" f)
      
      (write-line "   ----- Original message follows -----" f)
      (write-message-to-stream f oldq :norewrite :noclose t)
      
      (finish-output f)
      (fsync f)
      
      (queue-finalize 
       q (list bounce-to)
       (list subject  (format nil "To: ~a" (emailaddr-orig bounce-to)))
       (dotted-to-ipaddr "127.0.0.1")
       :date t
       :add-from t 
       :from-gecos "Mail Delivery Subsystem"))
    
    (when errstatus
      ;; It's bad news if a bounce doesn't build properly.
      (error (second errstatus)))
    
    (let ((bounce-proc (mp:process-run-function "Sending bounce" 
			 #'queue-process-single (queue-id q))))
      (if wait
	  (wait-for-process 
	   bounce-proc 
	   (format nil "Waiting for bounce message to be sent to ~A"
		   (emailaddr-orig bounce-to)))))))
