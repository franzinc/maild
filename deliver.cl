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


(defun local-delivery-type (user)
  (if (< (length user) 1)
      :normal ;; not all that normal, really.
    (case (schar user 0)
      (#\|
       :to-program)
      (#\/
       :to-file)
      (t
       :normal))))

;;; called when a recipient has been determined to be a local user
;;; that exists.  Also includes program and file recipients.
(defun deliver-local (user q)
  (ecase (local-delivery-type user)
    (:normal
     (deliver-to-program-help (funcall *deliver-local-command* user q) q
			      :user user))
    (:to-file
     (deliver-to-file user q))
    (:to-program
     (deliver-to-program (subseq user 1) q))))

;; Since with-stream-lock only locks out other processes, we need to
;; do internal locking as well.
(defvar *file-delivery-locks* nil)

;; Blocks until the lock is acquired.
(defun get-file-delivery-lock (file)
  (mp:without-scheduling
    (if (null *file-delivery-locks*)
	(setf *file-delivery-locks* (make-hash-table :test #'equal 
						     :values nil))))
  (loop
    (mp:without-scheduling
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

;; (dancy)/usr/bin/id  means run /usr/bin/id as user 'dancy'
(defun deliver-to-program (cmdline q)
  (let ((run-as *program-alias-user*))
    ;; Check for special syntax for user to run the program as.
    (multiple-value-bind (found whole user realcmdline)
	(match-regexp "^(\\([^)]+\\))\\(.*\\)" cmdline)
      (declare (ignore whole))
      (when found
	;; Make sure the user exists.
	(if (null (getpwnam (string-downcase user)))
	    (error 
	     "Alias left hand side: ~A: User ~A doesn't exist"
	     cmdline user))
	(setf run-as user)
	(setf cmdline realcmdline)))
    (deliver-to-program-help (split-regexp "\\b+" cmdline) 
			     q :run-as run-as)))

(defun deliver-to-program-help (prglist q &key (run-as *local-delivery-user*)
					       (rewrite :local)
					       user)
  (block nil
    (let* ((prgvec (coerce (cons (car prglist) prglist) 'vector))
	   (prgname (svref prgvec 0)))
      (verify-security prgname)
      (multiple-value-bind (output errput status writerstatus)
	  (send-message-to-program q prgvec :rewrite rewrite
				   :run-as run-as)
	(if output
	    (maild-log "~A stdout: ~A~%" prgname output))
	(if errput
	    (maild-log "~A stderr: ~A~%" prgname errput))
	(when (/= status 0)
	  (maild-log "~A exited w/ status: ~D" prgname status)
	  (return :transient))
	(when (not (eq writerstatus :ok))
	  ;; the writer logs its own errors.
	  (return :transient))
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

(defun write-message-to-stream (stream queue rewrite-type 
				&key smtp noclose)
  (if (null (queue-headers queue))
      (error "write-message-to-stream: queue-headers is null.  This can't be right"))
  (with-socket-timeout (stream :write *datatimeout*)
    (macrolet ((endline () `(if smtp 
				(progn
				  (write-char #\return stream)
				  (write-char #\linefeed stream))
			      (write-char #\newline stream))))
      (dolist (header (rewrite-headers (queue-headers queue) rewrite-type))
	(write-string header stream)
	(endline))

      ;; write the header boundary.
      (endline)

      (with-open-file (f (queue-datafile queue))
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
	(maild-log "write-message-to-stream got error ~A" c)
	(setf (wmts-async-status async) c)
	;; Gotta do this, otherwise the listening party won't know
	;; what the deal is.
	(ignore-errors (close stream :abort t)))))
  (mp:open-gate (wmts-async-gate async)))
