(in-package :user)

(defstruct queue 
  id
  (ctime (get-universal-time)) ;; when this message was queued
  (status "Reading message data...
")
  from
  recips ;; remaining recipients to be processed
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


(defun queue-expand-aliases (recips)
  (let (expandedrecips)
    (dolist (recip recips)
      (if (local-domain-p recip)
	  (let ((expanded (lookup-recip-in-aliases recip :parsed t)))
	    (setf expandedrecips
	      (nconc expandedrecips
		     (if expanded 
			 expanded 
		       (list recip)))))
	(push recip expandedrecips)))
    (queue-dedupe-recips expandedrecips)))
    
(defun queue-dedupe-recips (recips)
  (let (seen res)
    (dolist (recip recips)
      (if* (not (member recip seen :test #'emailaddr=))
	 then
	      (push recip res)
	      (push recip seen)))
    res))

;; Returns the locked queue struct
(defun make-queue-file (from recips)
  (let ((prefix (format nil "~A/tf" *queuedir*))
	id
	tmpfile
	queue)
    (loop
      (multiple-value-bind (f filename)
	  (mkstemp (concatenate 'string prefix "XXXXXX"))
	(setf tmpfile filename)
	(close f)
	(setf id (subseq filename (length prefix)))
	
	;; We've safely selected a possible id.  Make sure it's not used
	;; on any qf.  
	(if (not (queue-exists-p id))
	    (return)) ;; break from the loop
	
	(delete-file filename)))
    
    ;; we now have a tf file which has put a claim on our id.
    ;; Now create the regular lockfile so that we don't have
    ;; to worry about the file be processed while it's still
    ;; being created.
    (queue-lock id)
    (delete-file tmpfile)

    (setf queue
      (make-queue 
       :id id
       :from from
       :recips (queue-expand-aliases recips)))
    (update-queue-file queue)
    queue))
    

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
    (rename-file tmpfile filename)))

      
(defun remove-queue-file (q)
  (if (probe-file (queue-filename q))
      (delete-file (queue-filename q)))
  (if (probe-file (queue-datafile q))
      (delete-file (queue-datafile q)))
  (queue-unlock q))


(defun queue-init-headers (queue headers cliaddr &key date add-from from-gecos)
  (setf (queue-headers queue)
    (cons (make-received-header cliaddr (queue-id queue))
	  headers))
  (if (null (locate-header "Message-Id:" headers))
      (setf (queue-headers queue)
	(append (queue-headers queue) 
		(list (make-message-id-header (queue-id queue))))))
  (if (and date (null (locate-header "Date:" headers)))
      (setf (queue-headers queue)
	(append (queue-headers queue) 
		(list (make-date-header)))))
  (if (and add-from (null (locate-header "From:" headers)))
      (setf (queue-headers queue)
	(append (queue-headers queue) 
		(list (make-from-header (queue-from queue) from-gecos)))))

  (setf (queue-valid queue) t)
  (update-queue-file queue))

;; reads a queue file.  Doesn't lock.
(defun queue-read (id)
  (with-open-file (f (queue-filename-from-id id))
    (read f)))

(defun queue-lock (id)
  (lock-file (queue-lockfile-from-id id)))

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

(defmacro with-locked-queue ((qvar id noexistform) &body body)
  (let ((lockfilevar (gensym))
	(idvar (gensym)))
    `(let* ((,idvar ,id)
	    (,lockfilevar (queue-lockfile-from-id ,idvar)))
       (with-lock-file-nowait (,lockfilevar :locked)
	 (if (not (queue-exists-p ,idvar))
	     ,noexistform
	   (let ((,qvar (queue-read ,idvar)))
	     ,@body))))))

;; Don't look at this!
(defmacro with-new-queue ((q streamvar statusvar from recips) &body body)
  `(let (,streamvar)
     (setf ,q (make-queue-file ,from ,recips))
     (unwind-protect
	 (block nil
	   (handler-case 
	       (setf ,streamvar 
		 (open (queue-datafile ,q) 
		       :direction :output
		       :if-does-not-exist :create))
	     (error (c)
	       (maild-log "Failed to open queue datafile ~A: ~A"
			  (queue-datafile q) c)
	       (setf ,statusvar :transient)
	       (return)))
	   
	   ;; file is open now.  Make sure it always gets closed.
	   (with-already-open-file (,streamvar)
	     (fchmod f #o0600) 
	     ,@body))
       ;; cleanup forms
       (queue-unlock ,q)
       (if (not (queue-valid ,q))
	   (remove-queue-file ,q)))))
