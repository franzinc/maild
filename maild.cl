(in-package :user)

(defun main (&rest args)
  (if (probe-file "/etc/maild.cl")
      (load "/etc/maild.cl" :verbose nil))
  (let ((prgname (pop args)))
    (if (null args)
	(error "~a: recipients must be specified on the command line."
	       prgname))
    (with-command-line-arguments 
	(("F" :short fullname :required)
	 ("f" :short from :required)
	 ("i" :short ignoredot nil)
	 ("b" :short runmode :required)
	 ("o" :short options :required))
      (recips :command-line-arguments args)
      
      (when runmode
	(cond
	 ((string= runmode "d")
	  (error "Daemon mode not done yet"))
	 ((string= runmode "s")
	  ;; Run in stdin/stdout smtp mode
	  (do-smtp *terminal-io* :fork t)
	  (exit 0 :quiet t))
	 ((string= runmode "p")
	  (queue-list)
	  (exit 0 :quiet t))
	 (t
	  (error "-b~A option invalid" runmode)))) 
      
      
      (if (null recips)
	  (error "~a: recipients must be specified on the command line."
		 prgname))
      
      (if (not (probe-file *queuedir*))
	  (error "Queue directory ~A doesn't exist!" *queuedir*))
  
      (let (parsed-recips)
	(dolist (reciplist recips)
	  (multiple-value-bind (parsedlist badlist cruft)
	      (parse-email-addr-list reciplist)
	    (setf parsed-recips (nconc parsed-recips parsedlist))
	    (dolist (bad badlist)
	      (format t "~A... ~A~%" (first bad) (second bad)))
	    (if (string/= cruft "")
		(format t "~A... Ignored~%" cruft))))
	
	(setf parsed-recips (weed-bogus-local-recips parsed-recips))
	
	(if parsed-recips
	    (send-from-stdin parsed-recips
			     :dot (if ignoredot nil t)
			     :gecos fullname
			     :from from)
	  (error "~a: No valid recipients specified." prgname))))))


(defun weed-bogus-local-recips (recips)
  (let (goodrecips)
    (dolist (recip recips)
      (multiple-value-bind (disp errmsg)
	  (get-recipient-disposition recip)
	(cond
	 ((eq disp :local-unknown)
	  (format t "~A... User unknown~%" (emailaddr-orig recip)))
	 ((eq disp :error)
	  (format t "~A... ~A~%" (emailaddr-orig recip) errmsg))
	 (t
	  (push recip goodrecips)))))
    (nreverse goodrecips)))
	  

;; s = seconds
;; m = minutes (default if no tag specified)
;; h = hours
;; d = days
;; w = weeks
(defun parse-queue-interval (string)
  (let ((max (length string))
	(pos 0)
	(seconds 0)
	char)
    (loop
      (when (>= pos max)
	(return))
      (multiple-value-bind (value newpos)
	  (parse-integer string :start pos :junk-allowed t)
	(when (null value)
	  (error "Invalid time spec: ~A" string))
	;; find out what the units are
	(when (>= newpos max)
	  ;; no characters follow.  Default to minutes
	  (incf seconds (* 60 value))
	  (return))
	(setf char (schar string newpos))
	(case char
	  (#\s
	   (incf seconds value))
	  (#\m
	   (incf seconds (* 60 value)))
	  (#\h
	   (incf seconds (* 3600 value)))
	  (#\d
	   (incf seconds (* 86400 value)))
	  (#\w
	   (incf seconds (* (* 86400 7) value)))
	  (t
	   (error "Invalid time unit: '~A'" char)))
	(setf pos (1+ newpos))))
    seconds))
