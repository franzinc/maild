(in-package :user)

;; Makes something like:
;; ((owner1 recip1 recip2) (owner2 recip3 recip4) (owner3 recip5))
(defun group-failed-recips-by-owner (failed-recips)
  (let (owners owner)
    (dolist (recip failed-recips)
      (setf owner (member (recip-owner recip) owners 
			  :key #'car :test #'emailaddr=))
      (if (null owner)
	  (setf owners (cons (cons (recip-owner recip) (list recip)) owners))
	(push recip (cdr (car owner)))))
    owners))

(defun bounce (oldq failed-recips &key wait undeliverable)
  (dolist (entry (group-failed-recips-by-owner failed-recips))
    (bounce-inner oldq (car entry) (cdr entry) 
		  :wait wait :undeliverable undeliverable)))


(defun bounce-inner (oldq bounce-to failed-recips &key wait undeliverable)
  ;; If this is a double-bounce, send the message to postmaster.
  (let* ((subject "Subject: Returned mail")
	 q errstatus)
    (when (emailnullp bounce-to)
      (setf bounce-to (parse-email-addr "postmaster"))
      (setf subject "Subject: Postmaster notify"))
    (with-new-queue (q f errstatus (parse-email-addr "<>" :allow-null t))
      (write-line
       "   ----- The following addresses had permanent fatal errors -----"
       f)
      (dolist (recip failed-recips)
	(write-recip-expansion recip f))
      (write-line "" f)
      (write-line "   ----- Transcript of session follows -----" f)
      (dolist (recip failed-recips)
	(write-line (recip-status recip) f)
	(if undeliverable
	    (format f "Could not deliver message for ~D days" *bounce-days*)))
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

(defun write-recip-expansion (recip stream)
  (write-line (emailaddr-orig (recip-addr recip)) stream)
  (dolist (exp (recip-expanded-from recip))
    (format stream "  ... expanded from ~A~%" (emailaddr-orig exp))))
