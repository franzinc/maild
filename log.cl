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
;; $Id: log.cl,v 1.6 2004/11/10 15:51:53 layer Exp $

(in-package :user)

(defparameter *log-opened* nil)

(defun maild-log (&rest args)
  (when (null *log-opened*)
    (openlog "maild" (logior *log-pid* (if *debug* *log-perror* 0)) *log-mail*)
    (setf *log-opened* t))
  
  (if* (first args)
     then (syslog (logior *log-mail* *log-info*) "~?" (first args) (rest args))
     else ;; Just in case someone calls maild-log instead of
	  ;; maild-log-and-print:
	  (syslog (logior *log-mail* *log-info*) "BUG: maild-log: args=~s"
		  args)))

(defun maild-log-and-print (verbose &rest args)
  (when verbose
    (if* (first args)
       then (format t "~?" (first args) (rest args))
	    (write-char #\newline)
	    (force-output)
       else (maild-log "BUG: maild-log-and-print: args=~s" args)))
  (apply #'maild-log args))
