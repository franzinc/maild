(in-package :user)

(eval-when (compile load eval)
  (require :mysql))

(defparameter *greylist-configfile* nil)
(defparameter *greylist-db-name* "greylist")
(defparameter *greylist-db-user* "greylist")
(defparameter *greylist-db-password* "unset")

;; times are in seconds

;; Unrecognized triples must wait at least this long before success
(defparameter *greylist-delay* (* 30 60)) ;; 30 minutes
;; Unrecognized triples must retry before this amount of time.
(defparameter *greylist-delay-lifetime* (* 4 3600)) ;; 4 hours
;; Good records can be idle for this amount of time.
(defparameter *greylist-lifetime* (* 60 86400)) ;; 60 days

(defparameter *greylist-localhost* nil) ;; for testing

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
  (when (null *greylist-lock*)
    (setf *greylist-lock* 
      (mp:make-process-lock :name "Greylist database lock")))
  
  (when (null *greylist-db*)
    (load *greylist-configfile* :verbose nil)
    (setf *greylist-db* 
      (dbi.mysql:connect :database *greylist-db-name*
			 :user *greylist-db-user*
			 :password *greylist-db-password*))))

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

(defun greylist-init (ip)
  (block nil
    ;; localhost is not subject to greylisting normally.
    (when (and (= ip #.(dotted-to-ipaddr "127.0.0.1"))
	       (not *greylist-localhost*))
      (maild-log "Client from localhost.  Always whitelisted")
      (return :skip))
    
    (handler-case
	(progn
	  (ensure-greylist-db)
	  :ok)
      (error (c)
	(maild-log "Failed to establish connection to greylist database: ~A"
		   c)
	(return
	  (values :transient "Please try again later"))))))
  

(defun greylist-rcpt-to-checker (ip from type to recips)
  (declare (ignore type recips))
  (block nil
    (maild-log "greylist: checking...")
    
    (let ((res (multiple-value-list (greylist-init ip))))
      (ecase (first res)
	(:ok
	 )
	(:skip
	 (return :ok))
	(:transient
	 (return (values-list res)))))
    
    ;; mail from:<> is checked later 
    (when (emailnullp from) ;; <>
      (maild-log "greylist: mail is from <> so check will happen after DATA")
      (return :ok))
    
    (greylist-check-common (get-universal-time)
			   ip
			   (emailaddr-orig from)
			   (emailaddr-orig to))))


(defun greylist-data-pre-checker (ip from tos)
  (block nil
    (maild-log "greylist-data-pre-checker...")
    
    (let ((res (multiple-value-list (greylist-init ip))))
      (ecase (first res)
	(:ok
	 )
	(:skip
	 (return :ok))
	(:transient
	 (return (values-list res)))))
    
    (when (not (emailnullp from))
      ;;(maild-log " not checking because mail is not from <>")
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
      
      :ok)))


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
    (greylist-delete-triple triple)
    (greylist-insert-triple triple)))
  

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



