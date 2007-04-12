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
;; $Id: maild.cl,v 1.26 2007/04/12 17:15:02 dancy Exp $

(in-package :user)

(defparameter *configfile* "/etc/maild.cl")

(eval-when (compile eval load)
;; useful when telnet'ing in:
  (require :trace))

(defun main (&rest args)
  (setf *load-verbose* nil)

  ;; Ensure sane umask.
  (excl.osi:umask #o22)
  
  (let ((prgname (pop args)))
    (when (string= (basename prgname) "mailq")
      (verify-real-user-is-root)
      (queue-list)
      (exit 0 :quiet t))
    ;; Pretend to support 'newaliases'
    (when (string= (basename prgname) "newaliases")
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
	 ("t" :short grab-recips nil)
	 ("m" :short metoo nil)
	 ("C" :short alt-config-file :required-companion))
      (cmdline-recips :command-line-arguments args)

      (when alt-config-file
	(verify-real-user-is-root)
	(if (not (probe-file alt-config-file))
	    (error "Configuration file ~A not found." alt-config-file))
	(setf *configfile* alt-config-file))
      
      ;; Load the configuration now.
      (if (and (probe-file *configfile*) 
	       (verify-root-only-file *configfile*))
	  (load *configfile*))

      ;; sanity checks
      (if (not (probe-file *queuedir*))
	  (error "Queue directory ~A doesn't exist!" *queuedir*))
      (verify-root-only-file *queuedir*)

      (if (and *ssl-support* (null *ssl-certificate-file*))
	  (error "*ssl-certificate-file* must be set when *ssl-support* is enabled"))
      (if (and *client-authentication* 
	       *client-auth-requires-ssl* 
	       (null *ssl-support*))
	  (error "*ssl-support must be enabled when *client-auth-requires-ssl* is enabled"))
      
      (establish-signal-handlers)

      ;; process options.  Ignores ones which we don't implement
      (dolist (option options)
	(cond
	 ((string= option "i")
	  (setf ignoredot t))
	 ((string= option "M")
	  (setf metoo t))))
      
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
	 ((string= runmode "D")
	  (verify-real-user-is-root)
	  (setf *debug* t)
	  (smtp-server-daemon :queue-interval processqueue))
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
	(queue-process-all :verbose verbose :max 1)
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
			     :metoo metoo
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
  (maild-log "Maild terminating on signal ~d" sig)
  (excl::mp-safe-exit (+ 128 sig) :quiet t)
  t) ;; for good measure

(defun establish-signal-handlers ()
  (dolist (sig `(,*sigint* ,*sigterm* ,*sighup*))
    (set-signal-handler sig #'maild-signal-handler)))

;; XXX -- sendmail shows the expansions.  Will do that
;; later.
(defun verify-cmdline-addrs (strings)
  (dolist (string strings)
    (get-good-recips-from-string string :verbose t)))

