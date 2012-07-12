#! /fi/cl/9.0/bin/mlisp -#C

(eval-when (compile eval load)
  (require :osi)
  (use-package :excl.osi))

(defvar *to*
    (format nil "~a@franz.com" (pwent-name (getpwuid (getuid)))))

(defun verify-emails (spoolfile number)
  ;; Things to verify:
  ;;   Body is "body$i"
  ;;   Subject is "subject$i"
  ;;   To is "$USER@franz.com"
  (with-open-file (in spoolfile)
    (let ((state :looking-for-start)
	  line
	  subject
	  to
	  body
	  (ht (make-hash-table :test #'equalp :size 1001)))
      (loop
	(setq line (read-line in nil in))
	(when (eq line in) (setq state :eof))
	;;(format t "line:~a~%" line)
	(ecase state
	  (:eof
	   (if* (and subject to body)
	      then (setf (gethash subject ht) (cons to body))
	      else (error "EOF in middle of message (~a ~a ~a)"
			  subject to body))
	   (return))
	  (:looking-for-start
	   (when (and subject to body)
	     (setf (gethash subject ht) (cons to body)))
	   (setq subject nil to nil body nil)
	   (when (not (match-re "^From .*" line :return nil))
	     (error "failed to find start of message"))
	   (setq state :looking-for-header))
	  (:looking-for-header
	   (multiple-value-bind (found whole header val)
	       (match-re "^(To|Subject): (.*)" line)
	     (declare (ignore whole))
	     (when found
	       (if* (string= "To" header)
		  then (setq to val)
		elseif (string= "Subject" header)
		  then (setq subject val))
	       (when (and subject to)
		 (setq state :looking-for-body-separator)))))
	  (:looking-for-body-separator
	   (when (string= "" line)
	     (setq state :looking-for-body)))
	  (:looking-for-body
	   (setq body line)
	   (setq state :eat-space))
	  (:eat-space
	   (if* (string= "" line)
	      then (setq state :looking-for-start)
	      else (error "didn't find space")))))
      
      ;; check the hash table of messages
      (or (= number (hash-table-count ht))
	  (error "Number of emails did not match (expected ~d, got ~d)."
		 number (hash-table-count ht)))
      (maphash (lambda (subject rest)
		 (let ((to (car rest))
		       (body (cdr rest)))
		   (or (string= *to* to)
		       (error "To did not match: ~s." to))
		   (multiple-value-bind (matched whole n)
		       (match-re "^subject([0-9]+)$" subject)
		     (declare (ignore whole))
		     (when (not matched)
		       (error "Subject did not match: ~s." subject))
		     (setq n (parse-integer n))
		     (or (string= (format nil "body~d" n)
				  body)
			 (error "Body did not match: ~s." body)))))
	       ht)))
  0)

(sys:with-command-line-arguments
    (("v" :short verbose))
    (rest)
  (or (= 2 (length rest))
      (error "usage: spoolfile number-of-emails"))
  (when verbose
    (format t "spoolfile=~s, n=~s~%"
	    (first rest) (second rest)))
  (exit (handler-case
	    (verify-emails
	     (first rest)
	     (parse-integer (second rest)
			    :junk-allowed nil))
	  (error (c) 
	    (format t "Error: ~a~%" c)
	    1))
	:quiet t))
