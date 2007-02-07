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
;; $Id: queue.cl,v 1.25 2007/02/07 01:29:10 dancy Exp $

(in-package :user)

(defstruct queue 
  id
  (ctime (get-universal-time)) ;; when this message was queued
  (status "Reading message data...")
  client-address
  from
  recips ;; remaining recipients to be processed (list of recip structs)
  orig-recips ;; parsed email addrs, pre-alias expansion
  headers
  valid) ;; not valid until the data file has been written

(defmacro queue-filename-from-id (id)
  `(concatenate 'string *queuedir* "/qf" ,id))

(defmacro queue-lockfile-from-id (id)
  `(concatenate 'string *queuedir* "/qf" ,id ".lck"))


(defmacro queue-filename (queue)
  `(queue-filename-from-id (queue-id ,queue)))

(defmacro queue-datafile (queue)
  `(concatenate 'string *queuedir* "/df" (queue-id ,queue)))


;; The sequence file holds the last used sequence number.
(defun create-queue-id ()
  (let ((seqnum 0)
	(lockfile (format nil "~A/.seq.lck" *queuedir*)))
    (with-lock-file (lockfile nil :wait t)
      (with-os-open-file (f (format nil "~A/.seq" *queuedir*)
			    (logior *o-rdwr* *o-creat*) #o0600)
	(if (> (file-length f) 0)
	    ;; read last sequence number.
	    (setf seqnum (parse-integer (file-contents f))))
	(incf seqnum)
	(file-position f 0)
	(format f "~d" seqnum)))
    (format nil "~12,'0d" seqnum)))

(defun make-queue-file (from cliaddr)
  (let ((status :try-again)
	id lockfile qfile q)
    (while (eq status :try-again)
      (setf id (create-queue-id))
      (setf lockfile (queue-lockfile-from-id id))
      (setf qfile (queue-filename-from-id id))
      (when (lock-file lockfile) 
	(unwind-protect
	    (when (null (probe-file qfile))
	      (setf q (make-queue :id id :from from :client-address cliaddr))
	      (update-queue-file q) ;; write it out
	      (setf status :ok))
	  ;; cleanup forms
	  (when (eq status :try-again)
	    (delete-file lockfile)))))
    q))
  

(defun update-queue-file (queue)
  (let ((filename (queue-filename queue))
	(tmpfile (format nil "~A/tempqf~A" *queuedir* (queue-id queue)))
	(*print-pretty* t)) ;; change to nil after debugging
    (with-open-file (f tmpfile 
		     :direction :output
		     :if-does-not-exist :create)
      (fchmod f #o0600)
      (write queue :stream f)
      (finish-output f)
      (fsync f))
    (rename tmpfile filename)))

      
(defun remove-queue-file (q)
  (if (probe-file (queue-filename q))
      (delete-file (queue-filename q)))
  (if (probe-file (queue-datafile q))
      (delete-file (queue-datafile q)))
  (queue-unlock q))

;; convenience functions.
(defun queue-append-header (q header)
  (setf (queue-headers q) (nconc (queue-headers q) (list header))))

;; removes all existing instances and appends the new one.
;; not used in the main program but might be used in user's *extra-headers-func*.
(defun queue-replace-header (q header)
  (let ((colonpos (position #\: header))
	headername)
    (if (null colonpos)
	(error "queue-replace-header: Invalid header: ~A~%" header))
    (setf headername (subseq header 0 (1+ colonpos))) ;; include the colon in the name
    (setf (queue-headers q) (remove-header headername (queue-headers q)))
    (queue-append-header q header)))

;; also available for *extra-headers-func*
(defun queue-locate-header (q header)
  (locate-header header (queue-headers q)))

;; recips is a list of email addresses or recip structs
(defun queue-prefinalize (q recips headers &key metoo)
  (let ((emailaddrs (if* (emailaddr-p (first recips))
		       then recips
		       else (mapcar #'recip-addr recips))))
    
    (setf (queue-orig-recips q) emailaddrs)
    (setf (queue-recips q) 
      (expand-addresses emailaddrs (queue-from q) :metoo metoo)))
  
  (setf (queue-headers q)
    (cons 
     (make-received-header (queue-client-address q) (queue-id q))
     (copy-list headers))))

(defun queue-finalize (q recips headers &key date add-from from-gecos metoo)
  (if (null (queue-orig-recips q))
      (queue-prefinalize q recips headers :metoo metoo))
  
  (if (null (locate-header "Message-Id:" headers))
      (queue-append-header q (make-message-id-header (queue-id q))))
  (if (and date (null (locate-header "Date:" headers)))
      (queue-append-header q (make-date-header)))
  (if (and add-from (null (locate-header "From:" headers)))
      (queue-append-header q (make-from-header (queue-from q) from-gecos)))
  
  (if *extra-headers-func*
      (funcall *extra-headers-func* q))
  
  (setf (queue-status q) "Ready for delivery")
  (setf (queue-valid q) t)
  (update-queue-file q)
  
  (maild-log "~A: msgid=~A, from=~A, to=~A"
	     (queue-id q)
	     (get-message-id (queue-headers q))
	     (emailaddr-orig (queue-from q))
	     (list-to-delimited-string 
	      (mapcar #'recip-printable (queue-recips q)) #\,)))

;; reads a queue file.  Doesn't lock.
(defun queue-read (id)
  (with-open-file (f (queue-filename-from-id id))
    (read f)))

(defun queue-lock (id)
  (lock-file (queue-lockfile-from-id id)))

(defun refresh-queue-lock (q)
  (maild-log "Refreshing lockfile for qf~A" (queue-id q))
  (refresh-lock-file (queue-lockfile-from-id (queue-id q))))

(defun queue-unlock (q)
  (let ((lockfile (queue-lockfile-from-id (queue-id q))))
    (if (probe-file lockfile)
	(delete-file lockfile))))

(defun queue-locked-p (q)
  (let ((lockfile (queue-lockfile-from-id (queue-id q))))
    (probe-file lockfile)))

(defun queue-exists-p (id)
  (probe-file (queue-filename-from-id id)))

;; locks and reads.  Returns nil if it couldn't get a lock.
(defun queue-get (id)
    (if (null (queue-lock id))
	nil
      ;; got lock
      (queue-read id)))

;; write out and unlock
(defun queue-put (q) 
  (update-queue-file q)
  (queue-unlock q))

;;

(defun queue-lock-refresher (q)
  (loop
    (sleep *queue-lock-refresh-interval*)
    (refresh-queue-lock q)))

;; Macrology.

(defmacro with-queue-lock-refresher ((q) &body body)
  (let ((process (gensym)))
    `(let ((,process (mp:process-run-function "Queue lock refresher"
		       #'queue-lock-refresher ,q)))
       (unwind-protect
	   (progn
	     ,@body)
	 ;; cleanup
	 (kill-and-reap-process ,process)))))

(defmacro with-locked-queue ((qvar id noexistform) &body body)
  (let ((lockfilevar (gensym))
	(idvar (gensym)))
    `(let* ((,idvar ,id)
	    (,lockfilevar (queue-lockfile-from-id ,idvar)))
       (with-lock-file (,lockfilevar :locked)
	 (if (not (queue-exists-p ,idvar))
	     ,noexistform
	   (let ((,qvar (queue-read ,idvar)))
	     (with-queue-lock-refresher (,qvar)
	       ,@body)))))))

;; 'q' and 'errvar' should be variables that are already in scope.  
(defmacro with-new-queue ((q streamvar errvar from cliaddr) &body body)
  `(let (,streamvar)
     (setf ,q (make-queue-file ,from ,cliaddr))
     (unwind-protect
	 (with-queue-lock-refresher (,q)
	   (block nil
	     (handler-case 
		 (setf ,streamvar 
		   (open (queue-datafile ,q) 
			 :direction :output
			 :if-does-not-exist :create))
	       (error (c)
		 (maild-log "Failed to open queue datafile ~A: ~A"
			    (queue-datafile q) c)
		 (setf ,errvar (list :transient c))
		 (return)))
	   
	     ;; file is open now.  Make sure it always gets closed.
	     (with-already-open-file (,streamvar)
	       (fchmod ,streamvar #o0600) 
	       ,@body)))
       ;; cleanup forms
       (queue-unlock ,q)
       (if (not (queue-valid ,q))
	   (remove-queue-file ,q)))))
