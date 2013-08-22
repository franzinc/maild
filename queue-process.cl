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
;; $Id: queue-process.cl,v 1.21 2006/10/02 20:24:46 dancy Exp $

(in-package :user)


; send-from-stdin, :operator
; queue-processing-thread, :operator
; smtp-data, :operator
; bounce-inner, :operator
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
    (maild-log-and-print verbose "Processing queue id ~A" (queue-id q))
    ;; Sanity checks
    (when (not (queue-valid q))
      (maild-log-and-print 
       verbose "Queue id ~A is incomplete.  Removing." (queue-id q))
      (remove-queue-file q)
      (return))
    (when (not (probe-file (queue-datafile q)))
      (maild-log-and-print
       verbose "Queue id ~A doesn't have a data file.  Removing." (queue-id q))
      (remove-queue-file q)
      (return))

    (let (failed-recips recip-addr recip-printable status response smtp-recips)
      (dolist (recip (queue-recips q))
	(setf recip-addr (recip-addr recip))
	(setf recip-printable (recip-printable recip))
	(if (null (recip-owner recip))
	    (setf (recip-owner recip) (queue-from q)))
	
	(if* (or (special-recip-p recip) (local-domain-p recip-addr))
	   then	(mark-recip-with-suitable-mailer recip)
		(setf (queue-status q) 
		  (format nil "Attempting delivery to ~a"
			  (recip-printable recip)))
		(update-queue-file q)
		
		(if* (error-recip-p recip)
		   then	(setf status :fail)
			(setf response (recip-errmsg recip))
		   else (handler-case 
			    (multiple-value-setq (status response)
			      (deliver-local recip q :verbose verbose))
			  (error (c)
			    (setf status :transient)
			    (setf response (format nil "~a" c)))))

		(case status
		  (:delivered
		   (setf (queue-recips q) (delete recip (queue-recips q))))
		  (:fail
		   (maild-log-and-print
		    verbose
		    "delivery to ~A failed: ~A" recip-printable response)
		   (setf (recip-status recip)
		     (format nil "~A: ~A" recip-printable response))
		   (push recip failed-recips)
		   (setf (queue-recips q) 
		     (delete recip (queue-recips q))))
		  ;; Everything else is treated as a transient problem
		  (t
		   (maild-log-and-print
		    verbose "delivery status for ~a is ~s." 
		    recip-printable status)
		   (if* response
		      then (setf (recip-status recip) response)
			   (maild-log-and-print 
			    verbose "Error message is: ~A" response)
		      else (setf (recip-status recip) 
			     (format nil 
				     "Unknown transient error.  Check logs.")))))
	   else
		(push recip smtp-recips)))
      
      ;; handle smtp recips now.  Unfortunate duplication of code
      ;; here.. but not too terrible.
      (let ((deliv (make-smtp-delivery :unprocessed-recips smtp-recips)))
	(deliver-smtp deliv q :verbose verbose)
	(dolist (recip (smtp-delivery-good-recips deliv))
	  (setf (queue-recips q)
	    (delete recip (queue-recips q))))
	(dolist (entry (smtp-delivery-transient-recips deliv))
	  (let ((recip (first entry))
		(response (second entry)))
	    (setf (recip-status recip)
	      (format nil "~A: ~A" (recip-printable recip) response))
	    (maild-log "Delivery deferred: ~A" (recip-status recip))))
	(dolist (entry (smtp-delivery-failed-recips deliv))
	  (let ((recip (first entry))
		(response (second entry)))
	    (setf (recip-status recip)
	      (format nil "~A: ~A" (recip-printable recip) response))
	    (maild-log "Delivery failed: ~A" (recip-status recip))
	    (push recip failed-recips)
	    (setf (queue-recips q) 
	      (delete recip (queue-recips q))))))
      
      ;; See if we need to send bounces
      (if failed-recips
	  (bounce q failed-recips :wait wait))
	
      ;; Done processing recips. 
      (when (null (queue-recips q))
	;; Everything has been delivered.  Clean up
	(maild-log-and-print
	 verbose "Completed final delivery for queue id ~A" (queue-id q))
	(remove-queue-file q) 
	(return))
      
      (when (queue-undeliverable-timeout-p q)
	(maild-log-and-print
	 verbose "Bouncing queue id ~A due to queue timeout" (queue-id q))
	(bounce q (queue-recips q) :wait wait :undeliverable t)
	(remove-queue-file q)
	(return))
      
      (setf (queue-status q) "Queued")
      (update-queue-file q)
      
      (maild-log-and-print
       verbose "Terminating processing of queue id ~A" (queue-id q)))))

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
    (sort res #'string<)))

(defun queue-list ()
  (let ((ids (get-all-queue-ids)))
    (when (null ids)
      (format t "~A is empty~%" *queuedir*)
      (return-from queue-list))
    (dolist (id ids)
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
	  (format t " Last update: ~a~%" (ctime (queue-mtime q)))
	  (format t " Sender: ~A~%" (emailaddr-orig (queue-from q)))
	  (format t " Remaining recips:~%")
	  (dolist (recip (queue-recips q))
	    (format t "  ~A~%   ~A~%"
		    (recip-printable recip)
		    (if (recip-status recip)
			(recip-status recip)
		      "")))
	  (format t " Status: ~A~%" (queue-status q)))))))

(defstruct queue-processing-job
  id
  verbose
  )

(defstruct (queue-processor-state (:include synchronizing-structure))
  (job-queue (make-instance 'mp:queue))
  threads
  )

(defparameter *queue-processor-state* (make-queue-processor-state))

;; start-queue-processing-threads, :operator
(defun queue-processing-thread ()
  (let ((job-queue (queue-processor-state-job-queue *queue-processor-state*)))
    (loop
      (catch 'fail
	(let* ((job (mp:dequeue job-queue :wait t :whostate "Waiting for work"))
	       (id (queue-processing-job-id job))
	       (verbose (queue-processing-job-verbose job)))
	  (handler-bind 
	      ((error 
		#'(lambda (c)
		    (maild-log "Got error ~A while processing queue id ~A"
			       c id)
		    (let ((backtrace (with-output-to-string (s) (zoom s))))
		      (dolist (line (delimited-string-to-list backtrace #\newline))
			(maild-log "  ~a" line))
		      (maild-log "End backtrace"))
		    (throw 'fail nil))))

	    (queue-process-single id :if-does-not-exist :ignore :verbose verbose)))))))

;; queue-process-all, :operator
(defun start-queue-processing-threads (count)
  "Safe to call multiple times"
  (with-locked-structure (*queue-processor-state*)
    (when (null (queue-processor-state-threads *queue-processor-state*))
      (let (threads)
	(dotimes (n count)
	  (let* ((name (format nil "Queue processor ~a" n))
		 (proc (mp:process-run-function name #'queue-processing-thread)))
	    (setf (mp:process-keeps-lisp-alive-p proc) nil)
	    
	    (push proc threads)))
	
	(setf (queue-processor-state-threads *queue-processor-state*) threads)))))

;; (:internal queue-process-daemon 0), :operator
;; main, :operator
(defun queue-process-all (&key verbose (max *queue-max-threads*))
  (start-queue-processing-threads max)
  
  (let ((job-queue (queue-processor-state-job-queue *queue-processor-state*)))
    
    ;; Queue up all the work
    (dolist (id (get-all-queue-ids))
      (mp:enqueue job-queue (make-queue-processing-job :id id :verbose verbose)))
    
    ;; Now wait for the job queue to drain.
    
    (while (not (mp:queue-empty-p job-queue))
      (mp:process-sleep 10 "Waiting for remaining queue processing threads to complete"))))

;; (flet smtp-server-daemon startup), :operator
(defun queue-process-daemon (interval)
  ;; sanity check
  (if (or (null interval) (<= interval 0))
      (error "queue-process-daemon: Invalid interval: ~S" interval))
  (let ((proc
	 (mp:process-run-function "Queue daemon"
	   #'(lambda ()
	       (maild-log "Queue process interval: ~D seconds" interval)
	       (loop
		 (queue-process-all)
		 (mp:process-sleep interval "Waiting for next queue processing interval")
		 )))))
    (setf (mp:process-keeps-lisp-alive-p proc) nil)))    	 
