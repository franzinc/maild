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
;; $Id: greylist.cl,v 1.16 2004/07/14 22:49:34 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :mysql))

(defparameter *greylist-db-host* "localhost")
(defparameter *greylist-db-name* "greylist")
(defparameter *greylist-db-user* "greylist")
(defparameter *greylist-db-password* "unset")
;; :opt-in or :opt-out.  
(defparameter *greylist-operation-mode* :opt-out)
(defparameter *greylist-ip-whitelist* '("127.0.0.1"))


;; times are in seconds

;; Unrecognized triples must wait at least this long before success
(defparameter *greylist-delay* (* 30 60)) ;; 30 minutes
;; Unrecognized triples must retry before this amount of time.
(defparameter *greylist-delay-lifetime* (* 4 3600)) ;; 4 hours
;; Good records can be idle for this amount of time.
(defparameter *greylist-lifetime* (* 60 86400)) ;; 60 days

;; Change to t to make the greylisting checks happen
;; after the message data is received.  This may improve interoperability
;; with weird clients.  However, this has a potentially undesirable side
;; effect.  When this option is turned off, a message arriving for
;; several recipients will have each recipient deferred or accepted 
;; individually.  With this option turned on, if any one recipient should
;; be deferred, the message is deferred for all recipients.
(defparameter *greylist-after-data-received* nil)

(defparameter *greylist-db* nil)

(defparameter *greylist-lock* nil)

(defparameter *greylist-ip-whitelist-parsed* nil)


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
  (load configfile :verbose nil)
  (if* (not *greylist-after-data-received*)
     then
	  (add-smtp-rcpt-to-checker 
	   "Greylist checker" 'greylist-rcpt-to-checker)
	  (add-smtp-data-pre-checker 
	   "Greylist checker" 'greylist-data-pre-checker)
     else
	  (add-smtp-data-checker "Greylist checker" 'greylist-data-checker))
  (setf *greylist-ip-whitelist-parsed* nil)
  (dolist (ip *greylist-ip-whitelist*)
    (push (parse-addr ip) *greylist-ip-whitelist-parsed*)))
  
(defun ensure-greylist-db ()
  (without-interrupts
    (when (null *greylist-lock*)
      (setf *greylist-lock* 
	(mp:make-process-lock :name "Greylist database lock"))))

  (mp:with-process-lock (*greylist-lock*)
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
    
    (ecase *greylist-operation-mode*
      (:opt-out
       (when (greylist-recipient-excluded-p to)
	 (maild-log "Greylist: ~A is in the opt-out list" to)
	 (return :ok)))
      (:opt-in
       (when (not (greylist-recipient-included-p to))
	 (return :ok))))
    
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
  ;; Check ip whitelist.
  (dolist (net *greylist-ip-whitelist-parsed*)
    (when (addr-in-network-p ip net)
      (maild-log "Client from ~A:  Manually whitelisted client." 
		 (ipaddr-to-dotted ip))
      (return-from greylist-init :skip)))
  
  ;; Hosts that are allowed to relay through us aren't subject
  ;; to greylisting.   Most (all?) relay checkers ignore the 'to'
  ;; parameter so this won't cause problems when there are really
  ;; multiple recipients (i.e., it is unlikely that this function
  ;; will return different values depending on the 'to' parameter).
  (when (relaying-allowed-p ip from to)
    (maild-log "Client from ~A:  Auto-whitelisted relay client."
	       (ipaddr-to-dotted ip))
    (return-from greylist-init :skip))
  
  (multiple-value-bind (res string)
      (greylist-db-init) ;; check below needs the db
    (when (not (eq res :ok))
      (return-from greylist-init (values res string))))
      
  (when (greylist-whitelisted-sender-p from to)
    (maild-log "Client from ~A:  Manually whitelisted sender: ~A" 
	       (ipaddr-to-dotted ip)
	       (emailaddr-orig from))
    (return-from greylist-init :skip))
  
  :ok)


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


(defun greylist-mailer-daemon-addr-p (addr)
  (or (emailnullp addr)
      (equalp "postmaster" (emailaddr-user addr))))
  

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


;; If any of the triples would delay, then the entire message is
;; delayed.  However, all triples are checked anyway.. to make sure
;; their greylists are updated properly (otherwise there will be a
;; very long cascading delay as each of the recipients of the message
;; are delayed and then whitelested, one at a time).
(defun greylist-data-checker-common (ip from tos)
  (with-greylist (ip from (first tos))
    (let ((now (get-universal-time))
	  (ok t)
	  res
	  return-values-list)
      
      (dolist (to tos)
	(setf res (multiple-value-list 
		   (greylist-check-common now ip (emailaddr-orig from)
					  (emailaddr-orig to))))
	;; the return values from the first failed check are used.
	(when (and ok (not (eq (first res) :ok)))
	  (setf ok nil)
	  (setf return-values-list res)))
    
      (if (not ok)
	  (return-from greylist-data-checker-common 
	    (values-list return-values-list)))
      
      ;; Reach here if all triples were :ok.  Before reporting :ok,
      ;; check to see if this is a message from <>.  If it is,
      ;; immediately expire the corresponding triples since messages
      ;; from <> are considered one-time error messages. 
      (if (greylist-mailer-daemon-addr-p from)
	  (dolist (to tos)
	    (let ((triple (greylist-lookup-triple now ip (emailaddr-orig from)
						  (emailaddr-orig to))))
	      (greylist-delete-triple triple))))
      
      :ok)))


(defun greylist-data-checker (ip from tos size headers datafile)
  (declare (ignore size headers datafile))
  (greylist-data-checker-common ip from tos))

;; Same as greylist-data-checker.. except it always says okay if
;; the message is not from mailer daemon.
(defun greylist-data-pre-checker (ip from tos)
  (if (not (greylist-mailer-daemon-addr-p from))
      :ok
    (greylist-data-checker-common ip from tos)))


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
    
    ;; First, remove any cruft.
    (greysql (format nil "delete from triples where expire<=~D" now))
    
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

(defun greylist-whitelisted-sender-p (from to)
  (>= 
   (caar 
    (greysql
     (format nil "select count(*) from whitelist where 
       (sender = ~S or sender = '*@~A')
       and
       (receiver=~S or receiver='*')"
	     (dbi.mysql:mysql-escape-sequence (emailaddr-orig from))
	     (dbi.mysql:mysql-escape-sequence (emailaddr-domain from))
	     (dbi.mysql:mysql-escape-sequence (emailaddr-orig to)))))
   1))
