(in-package :user)

;; default *deliver-local-command*
(defun deliver-local-command (user queue)
  (list "/usr/bin/procmail" 
	"-Y"
	"-f"
	(emailaddr-orig (rewrite-local-envelope-sender (queue-from queue)))
	"-d"
	user))

(defstruct wmts-async
  stream
  queue
  rewrite-type
  smtp
  noclose
  (gate (mp:make-gate nil))
  (status :ok)) ;; :ok or an error condition

;;; called when a recipient has been determined to be a local user
;;; that exists.  Also includes program and file recipients.
(defun deliver-local (recip q &key verbose)
  (let* ((type (recip-type recip))
	 (file (recip-file recip))
	 (user (if (recip-addr recip) (emailaddr-user (recip-addr recip))))
	 (res (multiple-value-list
	       (cond
		((null type)
		 (if verbose
		     (format t "~A... Connecting to local...~%" user))
		 (deliver-to-program-help 
		  (funcall *deliver-local-command* user q) q
		  :user user))
		((eq type :file)
		 (if verbose
		     (format t "~A... Writing to file...~%" file))
		 (deliver-to-file file q))
		((eq type :prog)
		 (if verbose
		     (format t "~A... Connecting to prog...~%" file))
		 (deliver-to-program file (recip-prog-user recip) q))))))
    (when verbose
      (format t "~A... " (recip-printable recip))
      (case (first res)
	(:delivered
	 (format t "Sent~%"))
	(:transient
	 (format t "Transient error~%"))
	(t
	 (format t "Error~%"))))
    
    (values-list res)))

       
;; Since with-stream-lock only locks out other processes, we need to
;; do internal locking as well.
(defvar *file-delivery-locks* nil)

;; Blocks until the lock is acquired.
(defun get-file-delivery-lock (file)
  (without-interrupts
    (if (null *file-delivery-locks*)
	(setf *file-delivery-locks* (make-hash-table :test #'equal 
						     :values nil))))
  (loop
    (without-interrupts
      (if (null (gethash file *file-delivery-locks*))
	  (progn
	    (setf (gethash file *file-delivery-locks*) t)
	    (return t))))
    (sleep 1)))

(defun put-file-delivery-lock (file)
  (mp:without-scheduling
    (remhash file *file-delivery-locks*)))

(defmacro with-file-delivery-lock ((file) &body body)
  (let ((filevar (gensym)))
    `(let ((,filevar ,file))
       (get-file-delivery-lock ,filevar)
       (unwind-protect
	   (progn 
	     ,@body)
	 (put-file-delivery-lock ,filevar)))))
  

(defun deliver-to-file (file q)
  (verify-security file :writable-file-okay t)
  (with-file-delivery-lock (file)
    (with-os-open-file (stream file (logior *o-wronly* *o-creat* *o-append*)
			       #o0600)
      (with-stream-lock (stream)
	(format stream "From ~A  ~A~%" 
		(emailaddr-orig (queue-from q)) (ctime))
	(write-message-to-stream stream q :local :noclose t)
	(terpri stream)
	(maild-log "Delivered to file ~A" file)
	:delivered))))

(defun deliver-to-program (cmdline run-as q)
  (deliver-to-program-help (split-regexp "\\b+" cmdline) q
			   :run-as (if run-as 
				       run-as
				     *local-delivery-user*)))


(defun deliver-to-program-help (prglist q &key (run-as *local-delivery-user*)
					       (rewrite :local)
					       user)
  (block nil
    (let* ((prgvec (coerce (cons (car prglist) prglist) 'vector))
	   (prgname (svref prgvec 0)))
      (verify-root-only-file prgname)
      (multiple-value-bind (output errput status writerstatus)
	  (send-message-to-program q prgvec :rewrite rewrite
				   :run-as run-as)
	(if output
	    (maild-log "~A stdout: ~A~%" prgname output))
	(if errput
	    (maild-log "~A stderr: ~A~%" prgname errput))
	(when (/= status 0)
	  (maild-log "~A exited w/ status: ~D" prgname status)
	  (return (values :transient errput)))
	(when (not (eq writerstatus :ok))
	  ;; the writer logs its own errors.
	  (return (values :transient (format nil "~A" writerstatus))))
	(maild-log "Successful local delivery to ~A" 
		   (if user user prgname))
	:delivered))))


;; run-as should be nil or a string like "root" or "dancy".
;; Returns values:
;;  output, errput, exit code
(defun send-message-to-program (q prg &key (rewrite :norewrite) run-as)
  (let (uid gid initgroups-user dir async)
    (when run-as
      (let ((pwent (getpwnam (string-downcase run-as))))
	(if (null pwent)
	    (error "send-message-to-program: user ~S doesn't exist" run-as))
	(setf uid (pwent-uid pwent))
	(setf gid (pwent-gid pwent))
	(setf initgroups-user run-as)
	(setf dir (pwent-dir pwent)))) 
    (with-pipe (readfrom writeto) 
      (setf async (make-wmts-async :stream writeto
				   :queue q
				   :rewrite-type rewrite))
      (mp:process-run-function "message text generator"
	#'write-message-to-stream-async async)
      (multiple-value-bind (output errput status)
	  (command-output
	   prg
	   :uid uid
	   :gid gid
	   :initgroups-user initgroups-user
	   :directory dir
	   :input readfrom
	   :whole t)
	(close readfrom)
	;; wait for writer to finish
	(mp:process-wait "Waiting for message text generator to finish"
			 #'mp:gate-open-p (wmts-async-gate async))
	(values output errput status (wmts-async-status async))))))


(defun write-message-to-stream (stream q rewrite-type 
				&key smtp noclose)
  (if (null (queue-headers q))
      (error "write-message-to-stream: queue-headers is null.  This can't be right"))
  (with-socket-timeout (stream :write *datatimeout*)
    (macrolet ((endline () `(if smtp 
				(progn
				  (write-char #\return stream)
				  (write-char #\linefeed stream))
			      (write-char #\newline stream))))
      ;; headers might span lines.  Need to handle the EOL characters
      ;; correctly.
      (let (char)
	(dolist (header (rewrite-headers (queue-headers q) rewrite-type))
	  (dotimes (n (length header))
	    (setf char (schar header n))
	    (if (eq char #\newline)
		(endline)
	      (write-char char stream)))
	  (endline)))

      ;; write the header boundary.
      (endline)

      (with-open-file (f (queue-datafile q))
	(let (char (freshline t))
	  (while (setf char (read-char f nil nil))
	    (if* (char= char #\newline)
	       then
		    (endline)
		    (setf freshline t)
	       else
		    (if (and freshline smtp (char= char #\.))
			(write-char char stream)) ;; double-up the dot
		    (write-char char stream)
		    (setf freshline nil))))))

    (finish-output stream)

    (if (not (or smtp noclose))
	(close stream :abort t)))) ;; EOF


(defun write-message-to-stream-async (async)
  (let ((stream (wmts-async-stream async))
	(queue (wmts-async-queue async))
	(rewrite-type (wmts-async-rewrite-type async))
	(smtp (wmts-async-smtp async))
	(noclose (wmts-async-noclose async)))
    (handler-case 
	(write-message-to-stream stream queue rewrite-type
				 :smtp smtp
				 :noclose noclose)
      (t (c)
	(maild-log "write-message-to-stream error: ~A" c)
	(setf (wmts-async-status async) c)
	;; Gotta do this, otherwise the listening party won't know
	;; what the deal is.
	(ignore-errors (close stream :abort t)))))
  (mp:open-gate (wmts-async-gate async)))
