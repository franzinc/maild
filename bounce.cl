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
;; $Id: bounce.cl,v 1.14 2006/03/21 18:12:45 dancy Exp $

(in-package :user)

(defun bounce (oldq failed-recips &key wait undeliverable)
  (dolist (entry (group-recips-by-owner failed-recips))
    (bounce-inner oldq (first entry) (second entry) 
		  :wait wait :undeliverable undeliverable)))


(defun bounce-inner (oldq bounce-to failed-recips &key wait undeliverable)
  ;; If this is a double-bounce, send the message to postmaster.
  (let* ((subject "Subject: Returned mail")
	 q errstatus)
    (when (emailnullp bounce-to)
      (setf bounce-to (parse-email-addr "postmaster"))
      (setf subject "Subject: Postmaster notify"))
    (with-new-queue (q f errstatus (parse-email-addr "<>" :allow-null t))
      
      (if undeliverable
	  (format f "----- Could not deliver message for ~D days -----~%~%"
		  *bounce-days*))
      
      (write-line
       "----- The following addresses had permanent fatal errors -----"
       f)
      
      (dolist (recip failed-recips)
	(write-recip-expansion recip f))
      (terpri f)

      (write-line "----- Transcript of session follows -----" f)
      (dolist (recip failed-recips)
	(write-line (recip-status recip) f))
      (terpri f)
      (terpri f)
      
      (write-line "----- Original message follows -----" f)
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
  (write-line (recip-printable recip) stream)
  (dolist (exp (recip-expanded-from recip))
    (format stream "  ... expanded from ~A~%" (emailaddr-orig exp))))
