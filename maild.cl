(in-package :user)


(defun main (&rest args)
  (if (and (probe-file "/etc/maild.cl") 
	   (verify-root-only-file "/etc/maild.cl"))
      (load "/etc/maild.cl" :verbose nil))
  ;; sanity check.
  (if (not (probe-file *queuedir*))
      (error "Queue directory ~A doesn't exist!" *queuedir*))
  (verify-root-only-file *queuedir*)
  (let ((prgname (pop args)))
    ;; contend w/ with-command-line-arguments 
    (if (null args)
	(error "~a: recipients must be specified on the command line."
	       prgname))
    (with-command-line-arguments 
	(("F" :short fullname :required)
	 ("f" :short from :required)
	 ("i" :short ignoredot nil)
	 ("b" :short runmode :required)
	 ("o" :short options :required)
	 ("q" :short processqueue :optional)
	 ("v" :short verbose nil))
      (recips :command-line-arguments args)
      
      (establish-signal-handlers)

      (when processqueue
	(if (stringp processqueue)
	    (setf processqueue (parse-queue-interval processqueue))
	  (setf processqueue 0)))
      
      (when runmode
	(cond
	 ((string= runmode "d")
	  (verify-real-user-is-root)
	  (smtp-server-daemon :queue-interval processqueue)
	  (exit 0 :quiet t)) ;; parent gets here.
	 ((string= runmode "s")
	  (do-smtp *terminal-io* :fork t :verbose verbose)
	  (exit 0 :quiet t))
	 ((string= runmode "p")
	  (verify-real-user-is-root)
	  (queue-list)
	  (exit 0 :quiet t))
	 (t
	  (error "-b~A option invalid" runmode)))) 
      
      (when processqueue
	(verify-real-user-is-root)
	(queue-process-all :verbose verbose)
	(exit 0 :quiet t))
	  
      
      (if (null recips)
	  (error "~a: recipients must be specified on the command line."
		 prgname))
      
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
			     :from from
			     :verbose verbose)
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

(defun maild-signal-handler (sig tee)
  (declare (ignore tee))
  (format t "Maild terminating...~%")
  ;;(exit (+ 128 sig) :quiet t))
  (excl::mp-safe-exit (+ 128 sig) :quiet t))

(defun establish-signal-handlers ()
  (dolist (sig `(,*sigint* ,*sigterm*))
    (set-signal-handler sig #'maild-signal-handler)))
