(in-package :user)

;; Reject spammy email.

(defparameter *spamc-cmd* 
    (vector "/usr/bin/spamc" "spamc" "-c"))

(defparameter *spamc-user* "spamassn")

(defparameter *reject-threshold* 10)


;; Returns :ok, :reject, or :transient
(defun spamassn-check (q)
  (block nil
    (multiple-value-bind (output errput status)
	(send-message-to-program q *spamc-cmd* 
				 :rewrite :local :run-as *spamc-user*)
      (declare (ignore status))
      (if errput
	  (maild-log "spamc stderr: ~A" errput))
      (when (null output)
	(maild-log "spamc generated no output.")
	(return :transient))
      (multiple-value-bind (found whole score)
	  (match-regexp "^\\([.0-9-]+\\)/" output)
	(declare (ignore whole))
	(when (not found)
	  (maild-log "spamc generated unexpected output: ~A" output)
	  (return :transient))
	(setf score (parse-fp score))
	(when (>= score *reject-threshold*)
	  (if *debug*
	      (maild-log "Rejecting message with spam score ~A from ~A"
			 score
			 (emailaddr-orig (queue-from q))))
	  (return :reject))
	:ok))))

