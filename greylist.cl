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
;; $Id: greylist.cl,v 1.8 2003/07/23 20:28:46 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :mysql))

(defparameter *greylist-configfile* nil)
(defparameter *greylist-db-host* "localhost")
(defparameter *greylist-db-name* "greylist")
(defparameter *greylist-db-user* "greylist")
(defparameter *greylist-db-password* "unset")
;; :opt-in or :opt-out.  
(defparameter *greylist-operation-mode* :opt-out)

;; times are in seconds

;; Unrecognized triples must wait at least this long before success
(defparameter *greylist-delay* (* 30 60)) ;; 30 minutes
;; Unrecognized triples must retry before this amount of time.
(defparameter *greylist-delay-lifetime* (* 4 3600)) ;; 4 hours
;; Good records can be idle for this amount of time.
(defparameter *greylist-lifetime* (* 60 86400)) ;; 60 days

(defparameter *greylist-db* nil)

(defparameter *greylist-lock* nil)

(defstruct triple
  ip
  from 
  to
  createtime
  blockexpire
  expire
  blocked
  passed)


(defun enable-greylist (configfile)
  (setf *greylist-configfile* configfile)
  (add-smtp-rcpt-to-checker "Greylist checker" 'greylist-rcpt-to-checker)
  (add-smtp-data-pre-checker "Greylist checker" 'greylist-data-pre-checker))

  
(defun ensure-greylist-db ()
  (without-interrupts
    (when (null *greylist-lock*)
      (setf *greylist-lock* 
	(mp:make-process-lock :name "Greylist database lock"))))

  (mp:with-process-lock (*greylist-lock*)
    (if (null *greylist-db*)
	(load *greylist-configfile* :verbose nil))
    
    ;; See if we have a broken connection.
    (when (and *greylist-db* 
	       (not (dbi.mysql:mysql-connected *greylist-db*)))
      (maild-log "greylist: Disconnecting broken mysql connection")
      (dbi.mysql:disconnect :db *greylist-db*)
      (setf *greylist-db* nil))
    
    (when (null *greylist-db*)
      (maild-log "greylist: Establishing mysql connection")
      (setf *greylist-db* 
	(dbi.mysql:connect :database *greylist-db-name*
			   :host *greylist-db-host*
			   :user *greylist-db-user*
			   :password *greylist-db-password*)))))

(defun greylist-check-common (now ip from to)
  (block nil
    (let ((triple (greylist-lookup-triple now ip from to)))
      (when (> (triple-blockexpire triple) now) 
	;; block is still in place
	(incf (triple-blocked triple)) ;; up the counter
	(maild-log "Greylist in place for (~A/~A/~A).  Lifts at ~A.  New blocked count: ~D"
		   (ipaddr-to-dotted ip) 
		   (triple-from triple) 
		   (triple-to triple)
		   (ctime (triple-blockexpire triple))
		   (triple-blocked triple))
	(update-triple triple)
	(return 
	  (values :transient "Please try again later")))

      ;; Must be a whitelisted triple.  Update stats and let it through.
      (incf (triple-passed triple))
      (setf (triple-expire triple) (+ now *greylist-lifetime*))
      
      (maild-log "Whitelist in place for (~A/~A/~A).  Expires at ~A.  Blocked/Passed: ~d/~d"
		 (ipaddr-to-dotted ip) 
		 (triple-from triple) 
		 (triple-to triple)
		 (ctime (triple-expire triple))
		 (triple-blocked triple)
		 (triple-passed triple))
      
      (update-triple triple)
      
      :ok)))

(defun greylist-db-init ()
  (handler-case
      (progn
	(ensure-greylist-db)
	:ok)
    (error (c)
      (maild-log "Failed to establish connection to greylist database: ~A"
		 c)
      (values :transient "Please try again later"))))
  

;; Should return
;; :ok -- ready to check greylist
;; :skip -- no need to check greylist.. accept the message
;; (:transient ...) -- something wrong connecting to database

(defun greylist-init (ip from to)
  ;; Hosts that are allowed to relay through us aren't subject
  ;; to greylisting.
  (dolist (checker *relay-checkers*)
    (when (funcall checker ip from to)
      (maild-log "Client from ~A:  Auto-whitelisted relay client."
		 (ipaddr-to-dotted ip))
      (return-from greylist-init :skip)))
  
  (let ((res (multiple-value-list (greylist-db-init))))
    (ecase (first res)
      (:transient
       (return-from greylist-init (values-list res)))
      (:ok)))
  
  (setf to (emailaddr-orig to))
  
  (ecase *greylist-operation-mode*
    (:opt-out
     (if* (greylist-recipient-excluded-p to)
	then
	     (maild-log "Greylist: ~A is in the opt-out list" to)
	     :skip
	else
       :ok))
    (:opt-in
     (if (greylist-recipient-included-p to)
	 :ok
       :skip))))

(defun greylist-mailer-daemon-addr-p (addr)
  (or (emailnullp addr)
      (equalp "postmaster" (emailaddr-user addr))))
  
(defmacro with-greylist ((ip from to) &body body)
  (let ((res (gensym)))
    `(let ((,res (multiple-value-list (greylist-init ,ip ,from ,to))))
       (ecase (first ,res)
	 (:ok
	  ,@body)
	 (:skip
	  :ok)
	 (:transient
	  (values-list ,res))))))


(defun greylist-rcpt-to-checker (ip from type to recips)
  (declare (ignore type recips))
  (block nil
    ;;(maild-log "greylist: checking...")
    (with-greylist (ip from to)
      ;; mail from mailer daemon is checked later 
      (when (greylist-mailer-daemon-addr-p from)
	(return :ok))
    
      (greylist-check-common (get-universal-time)
			     ip
			     (emailaddr-orig from)
			     (emailaddr-orig to)))))


(defun greylist-data-pre-checker (ip from tos)
  (block nil
    ;;(maild-log "greylist-data-pre-checker...")
    (with-greylist (ip from (first tos))
      (when (not (greylist-mailer-daemon-addr-p from))
	(return :ok)) ;; we already passed the rcpt to test

      (let ((now (get-universal-time))
	    res)
	(dolist (to tos :ok)
	  (setf res 
	    (multiple-value-list 
	     (greylist-check-common now ip (emailaddr-orig from)
				    (emailaddr-orig to))))
	
	  (if (not (eq (first res) :ok))
	      (return-from greylist-data-pre-checker (values-list res))))
      
	;; We're about to say okay.. but before we do, expire the
	;; corresponding records.  (special case for mail from <>).
	(dolist (to tos)
	  (let ((triple (greylist-lookup-triple now ip (emailaddr-orig from)
						(emailaddr-orig to))))
	    (greylist-delete-triple triple)))
      
	:ok))))


;;;;;;;; DB stuff

(defmacro greysql (&rest rest)
  ;; gah!
  `(mp:with-process-lock (*greylist-lock*)
     (dbi.mysql:sql ,@rest :db *greylist-db*)))


(defun greylist-lookup-triple (now ip from to)
  (block nil
    ;; not necessary for MySQL
    ;;(setf from (string-downcase from))
    ;;(setf to (string-downcase to))
    
    (let ((res 
	   (greysql
	    (format nil "select createtime, blockexpire, expire, blocked, passed from triples where ip=~A and sender=~S and receiver=~S"
		    ip
		    (dbi.mysql:mysql-escape-sequence from)
		    (dbi.mysql:mysql-escape-sequence to)))))
      
      (when (null res)
	(maild-log "Making new triple.")
	(return (make-fresh-triple now ip from to)))
      
      (setf res (first res))
      (let ((createtime (first res))
	    (blockexpire (second res))
	    (expire (third res))
	    (blocked (fourth res))
	    (passed (fifth res)))
	;; If we just read an expired record, don't use it.
	(when (> now expire)
	  (maild-log "Read expired record, so making a new triple")
	  (return (make-fresh-triple now ip from to)))

	(make-triple :ip ip
		     :from from
		     :to to
		     :createtime createtime
		     :blockexpire blockexpire
		     :expire expire
		     :blocked blocked
		     :passed passed)))))
	

(defun make-fresh-triple (now ip from to)
  (make-triple :ip ip 
	       :from from 
	       :to to
	       :createtime now 
	       :blockexpire (+ now *greylist-delay*)
	       :expire (+ now *greylist-delay-lifetime*)
	       :blocked 0
	       :passed 0))
	  
(defun update-triple (triple)
  (mp:with-process-lock (*greylist-lock*)
    (greysql "lock tables triples write")
    (greylist-delete-triple triple)
    (greylist-insert-triple triple)
    (greysql "unlock tables")))
  

(defun greylist-insert-triple (triple)
  (mp:with-process-lock (*greylist-lock*) 
    (dbi.mysql:insert-db ((ip (triple-ip triple))
			  (sender (triple-from triple) :text)
			  (receiver (triple-to triple) :text)
			  (createtime (triple-createtime triple))
			  (blockexpire (triple-blockexpire triple))
			  (expire (triple-expire triple))
			  (blocked (triple-blocked triple))
			  (passed (triple-passed triple)))
			 :table "triples"
			 :db *greylist-db*)))

(defun greylist-delete-triple (triple)
  (greysql
   (format nil "delete from triples where ip=~A and sender=~S and receiver=~S"
	   (triple-ip triple)
	   (dbi.mysql:mysql-escape-sequence (triple-from triple))
	   (dbi.mysql:mysql-escape-sequence (triple-to triple)))))

(defun greylist-recipient-excluded-p (to)
  (if (greysql 
       (format nil "select receiver from optout where receiver=~S"
	       (dbi.mysql:mysql-escape-sequence to)))
      t
    nil))

(defun greylist-recipient-included-p (to)
  (if (greysql 
       (format nil "select receiver from optin where receiver=~S"
	       (dbi.mysql:mysql-escape-sequence to)))
      t
    nil))


