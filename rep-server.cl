;; $Id: rep-server.cl,v 1.1 2004/11/15 04:09:13 layer Exp $

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rep server

;; The REP server is a Lisp thread that listens for connections.  When it
;; receives one, it starts a read-eval-print loop on that stream.

(defvar *logger-function* nil)

(defun start-rep-server (port &optional logger-function)
  (when logger-function (setq *logger-function* logger-function))
  (loop
    (let ((socket (socket:make-socket :connect :passive :local-port port
				      ;; Only accept connections from local
				      ;; host:
				      :local-host "127.0.0.1"
				      :reuse-address t)))
      (unwind-protect
	  (loop
	    (let ((connection (socket:accept-connection socket))
		  from)
	      (handler-case
		  (progn
		    (setq from (or (socket:ipaddr-to-hostname
				    (socket:remote-host connection))
				   (socket:ipaddr-to-dotted
				    (socket:remote-host connection))))
		    (rep-log "REP: new connection from ~a" from)
		    (format connection "
WARNING: do not use :exit or (exit).  You will exit the entire process.
         Use ~s to quit.~&"
			    '(quit))
		    (force-output connection) 
		    (mp:process-run-function
		     "telnet session"
		     'start-telnet-session connection from))
		(error ()
		  (ignore-errors (close connection))))))
	(ignore-errors (close socket))))))

(defvar *in-telnet-session* nil)

(defun start-telnet-session (s from)
  (unwind-protect
      (catch 'end-telnet-session
	(let ((*in-telnet-session* t))
	  (tpl:start-interactive-top-level
	   s 'tpl:top-level-read-eval-print-loop nil)))
    (ignore-errors (close s)))
  (rep-log "REP: closing connection from ~a" from))

(defun quit ()
  (throw 'end-telnet-session nil))

(defvar *exit-wrapped* nil)

(when (not *exit-wrapped*)
  (flet ((msg ()
	   (format t "Use ~s instead of exit.~%" '(quit))))
    (def-fwrapper exit-wrapper (&optional status &rest args)
      (declare (ignore args))
      (if* *in-telnet-session*
	 then (msg)
	 else (call-next-fwrapper)))
  
    (fwrap 'excl:exit :rep 'exit-wrapper)
    (fwrap 'tpl::exit-command :rep 'exit-wrapper))) 

(defun rep-log (format-string &rest args)
  (when *logger-function*
    (apply *logger-function* format-string args)))
