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
;; $Id: www.cl,v 1.6 2007/05/18 16:21:56 dancy Exp $

(in-package :user)

;; Report information via web

(eval-when (compile load eval)
  (require :aserve)
  (use-package :net.html.generator))

(defparameter *www-port* nil)
(defparameter *www-interface* nil)
(defparameter *www-wserver* nil)
(defparameter *www-logfile* nil)
(defparameter *www-logstream* nil)

(defun enable-webserver (&key (port 2525) (interface "127.0.0.1") logfile)
  (setf *www-port* port)
  (setf *www-interface* interface)
  (setf *www-logfile* logfile))

(defun start-webserver ()
  (when *www-port*
    (maild-log "Starting webserver")
    (handler-case 
	(setf *www-wserver* (net.aserve:start :port *www-port* :host *www-interface*))
      (error (c)
	(maild-log " Failed: ~A" c)
	(return-from start-webserver)))
    (when *www-logfile*
      (setf *www-logstream* (open *www-logfile* 
				  :direction :output
				  :if-does-not-exist :always-append
				  :if-exists :always-append))
      (let ((dvh (net.aserve:wserver-default-vhost *www-wserver*)))
	(setf (net.aserve:vhost-log-stream dvh) *www-logstream*)
	(setf (net.aserve:vhost-error-stream dvh) *www-logstream*)))
    (net.aserve:publish :path "/" :server *www-wserver* :function 'webserver-info
			:content-type "text/html")))

(defmacro webserver-show-checker (header stat)
  (if (not (stringp header))
      (error "webserver-show-checker: header should be a string"))
  (let ((chk (gensym))
	(data (gensym)))
    `(let ((,data (get-smtp-stat ,stat)))
       (when ,data
	 (html
	  (:ul
	   (:li ,header)
	   (:ul
	    (dolist (,chk ,data)
	      (html
	       (:li "By " (:princ-safe (car ,chk)) ": " (:princ (cdr ,chk))))))))))))
    
     

(defun webserver-info (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (html 
       (:html
	(:head (:title "Allegro Maild " (:princ-safe *allegro-maild-version*)))
	(:body
	 (:h3 "Allegro Maild " (:princ-safe *allegro-maild-version*))
	 "Statistics collection started at "
	 (:princ-safe (ctime (get-smtp-stat stats-start))) :br
	 (:ul
	  (:li "Client connections made: " (:princ (get-smtp-stat num-connections)))
	  (:li "Client connections accepted: " (:princ (get-smtp-stat connections-accepted))))
	 (:ul
	  (:li "Transactions started: " (:princ (get-smtp-stat transactions-started))))
	 
	 (webserver-show-checker "Client connections rejected temporarily..." connections-rejected-temporarily)
	 (webserver-show-checker "Client connections rejected permanently..." connections-rejected-permanently)
	 (webserver-show-checker "Senders rejected temporarily..." senders-rejected-temporarily)
	 (webserver-show-checker "Senders rejected permanently..." senders-rejected-permanently)
	 (webserver-show-checker "Recipients rejected temporarily..." recips-rejected-temporarily)
	 (webserver-show-checker "Recipients rejected permanently..." recips-rejected-permanently)
	 (webserver-show-checker "Messages pre-rejected (before receiving message text) temporarily..." messages-pre-rejected-temporarily)
	 (webserver-show-checker "Messages pre-rejected (before receiving message text) permanently..." messages-pre-rejected-permanently)
	 (webserver-show-checker "Messages rejected (after receiving message text) temporarily..." messages-rejected-temporarily)
	 (webserver-show-checker "Messages rejected (after receiving message text) permanently..." messages-rejected-permanently)
	 
	 (:ul
	  (:li "Emails accepted for delivery: " (:princ (get-smtp-stat mails-accepted))))))))))

	       


