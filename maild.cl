(in-package :user)

(defparameter *configfile* "/etc/maild.cl")

(defun main (&rest args)
  (setf *load-verbose* nil)

  (if (and (probe-file *configfile*) 
	   (verify-root-only-file *configfile*))
      (load *configfile*))
  
  ;; sanity check.
  (if (not (probe-file *queuedir*))
      (error "Queue directory ~A doesn't exist!" *queuedir*))
  (verify-root-only-file *queuedir*)

  (let ((prgname (pop args)))
    (when (string= (basename prgname) "mailq")
      (verify-real-user-is-root)
      (queue-list)
      (exit 0 :quiet t))
    
    (if (null args)
	(error "Recipient names must be specified"))
    (with-command-line-arguments 
	(("F" :short fullname :required-companion)
	 ("f" :short from :required-companion)
	 ("i" :short ignoredot nil)
	 ("b" :short runmode :required-companion)
	 ("o" :short options :required-companion :allow-multiple-options)
	 ("q" :short processqueue :optional-companion)
	 ("v" :short verbose nil)
	 ("t" :short grab-recips nil))
      (cmdline-recips :command-line-arguments args)
      
      (establish-signal-handlers)

      ;; process options.  Ignores ones which we don't implement
      (dolist (option options)
	(cond
	 ((string= option "i")
	  (setf ignoredot t))))
      
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
	 ((string= runmode "v")
	  (verify-cmdline-addrs cmdline-recips)
	  (exit 0 :quiet t))
	 ((string= runmode "i")
	  ;; ignore.
	  (exit 0 :quiet t))
	 (t
	  (error "-b~A option invalid" runmode)))) 
      
      (when processqueue
	(verify-real-user-is-root)
	(queue-process-all :verbose verbose)
	(exit 0 :quiet t))

      (if (and (not grab-recips) (null cmdline-recips))
	  (error "Recipient names must be specified"))

      (let (good-recips)
	(dolist (string cmdline-recips)
	  (setf good-recips 
	    (nconc good-recips
		   (get-good-recips-from-string string))))
	
	(if (or good-recips grab-recips)
	    (send-from-stdin good-recips
			     :dot (if ignoredot nil t)
			     :gecos fullname
			     :from from
			     :grab-recips grab-recips
			     :verbose verbose)
	  (error "~a: No valid recipients specified." prgname))))))



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

;; XXX -- sendmail shows the expansions.  Will do that
;; later.
(defun verify-cmdline-addrs (strings)
  (dolist (string strings)
    (get-good-recips-from-string string :verbose t)))

