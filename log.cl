;; $Id: log.cl,v 1.2 2003/07/08 18:05:24 layer Exp $

(in-package :user)

(defparameter *log-opened* nil)

(defun maild-log (&rest args)
  (if* (null *log-opened*)
     then
	  (openlog "maild"
		   (logior *log-pid* (if *debug* *log-perror* 0))
		   *log-mail*)
	  (setf *log-opened* t))
  (syslog (logior *log-mail* *log-info*) 
	  "~?"
	  (first args) (rest args)))

	  

