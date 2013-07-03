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
;; $Id: deliver.cl,v 1.20 2008/04/09 16:45:42 dancy Exp $

(in-package :user)

(defstruct wmts-async
  stream
  queue
  rewrite-type
  smtp
  noclose
  add-mbox-from
  (gate (mp:make-gate nil))
  (status :ok)) ;; :ok or an error condition

;;; called when a recipient has been determined to be a local user
;;; that exists.  Also includes program and file recipients.
;;; Called by queue-process-single-help
(defun deliver-local (recip q &key verbose)
  (let* ((type (recip-type recip))
	 (file (recip-file recip))
	 (res (multiple-value-list
	       (cond
		((null type) ;; regular recipient
		 (deliver-via-mailer recip q :verbose verbose))
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

;; Called by deliver-local
(defun deliver-via-mailer (recip q &key verbose)
  (when verbose
    (format t "~A... Connecting to ~A...~%" 
	    (emailaddr-orig (recip-addr recip)) (recip-mailer recip)))
  (multiple-value-bind (cmdlist run-as)
      (make-delivery-command-for-recip recip q)
    (deliver-to-program-help cmdlist q run-as :user 
			     (emailaddr-orig (recip-addr recip)))))


;; Since with-stream-lock only locks out other processes, we need to
;; do internal locking as well.
(defvar *file-delivery-locks* (make-hash-table :test #'equal 
					       :values nil))

;; Blocks until the lock is acquired.
(defun get-file-delivery-lock (file)
  (loop
    (#-smp-macros without-interrupts
     #+smp-macros with-delayed-interrupts
     ;;mm 2012-02 SMP-NOTE This will not be sufficient in smp.
     ;;   Need to use a lock or wish for rfe10479.
      (if (null (gethash file *file-delivery-locks*))
	  (progn
	    (setf (gethash file *file-delivery-locks*) t)
	    (return t))))
    (sleep 1)))

(defun put-file-delivery-lock (file)
  (#-smp-macros mp:without-scheduling
   #+smp-macros with-delayed-interrupts
   ;;mm 2012-02 The wrapper may be unnecessary since remhash is atomic.
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
	(write-message-to-stream stream q :local :noclose t :add-mbox-from t
				 :escape-mbox-from t)
	(terpri stream)
	(maild-log "Delivered to file ~A" file)
	:delivered))))

(defun deliver-to-program (cmdline run-as q)
  (deliver-to-program-help (split-regexp "\\b+" cmdline) q
			   (if run-as run-as *program-alias-user*)))


(defun deliver-to-program-help (prglist q run-as &key (rewrite :local)
						      user)
  (block nil
    (let* ((prgvec (coerce (cons (car prglist) prglist) 'vector))
	   (prgname (svref prgvec 0)))
      (multiple-value-bind (status output errput writerstatus)
	  (send-message-to-program q prgvec :rewrite rewrite
				   :run-as run-as)
	(if output
	    (maild-log "~A stdout: ~A" prgname output))
	(if errput
	    (maild-log "~A stderr: ~A" prgname errput))
	(when (/= status 0)
	  (maild-log "~A exited w/ status: ~D" prgname status)
	  (return (values :transient (or errput output))))
	(when (not (eq writerstatus :ok))
	  ;; the writer logs its own errors.
	  (return (values :transient (format nil "~A" writerstatus))))
	(maild-log "Successful local delivery to ~A" 
		   (if user user prgname))
	:delivered))))


;; run-as should be nil or a string like "root" or "dancy".
;; Returns values:
;;  exit code, output, errput, writer status
(defun send-message-to-program (q prg &key (rewrite :norewrite) run-as
					   (add-mbox-from t))
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
				   :rewrite-type rewrite
				   :add-mbox-from add-mbox-from))
      (let ((proc (mp:process-run-function "message text generator"
		    #'write-message-to-stream-async async)))
	(setf (mp:process-keeps-lisp-alive-p proc) nil))
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
	(values status output errput (wmts-async-status async))))))


(defun write-message-to-stream (stream q rewrite-type 
				&key smtp noclose add-mbox-from
				     escape-mbox-from
				     max-lines)
  (if (null (queue-headers q))
      (error "write-message-to-stream: queue-headers is null.  This can't be right"))
  
  (let ((line-count 0))
    (with-socket-timeout (stream :write *datatimeout*)
      (macrolet ((endline () 
		   `(progn
		      (if* smtp
			 then (write-char #\return stream)
			      (write-char #\linefeed stream)
			 else (write-char #\newline stream))
		      (if max-lines
			  (incf line-count)))))

	(when add-mbox-from
	  (format stream "From ~A  ~A" (emailaddr-orig (queue-from q)) (ctime))
	  (endline))
      
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

	(with-open-file (f (queue-datafile q) :external-format :latin1)
	  (let ((buf (make-array 1024 :element-type 'character))
		(freshline t)
		count got-eol)
	    (declare (dynamic-extent buf))
	    (loop 
	      (multiple-value-setq (count got-eol)
		(get-line f buf))
	      (if (null count)
		  (return))
	      
	      (if* (and max-lines (>= line-count max-lines))
		 then (write-string "...Truncated..." stream)
		      (endline)
		      (return))
	      
	      (when freshline
		(if (and escape-mbox-from (>= count 5) (prefixp "From " buf))
		    (write-char #\> stream))
		(if (and smtp (>= count 1) (prefixp "." buf))
		    (write-char #\. stream)))
	    
	      (write-string buf stream :end count)

	      (setf freshline got-eol)
	    
	      (if freshline
		  (endline))))))

      (finish-output stream)

      (if (not (or smtp noclose))
	  (close stream :abort t))))) ;; EOF


(defun write-message-to-stream-async (async)
  (let ((stream (wmts-async-stream async))
	(queue (wmts-async-queue async))
	(rewrite-type (wmts-async-rewrite-type async))
	(smtp (wmts-async-smtp async))
	(noclose (wmts-async-noclose async))
	(add-mbox-from (wmts-async-add-mbox-from async)))
    (handler-case 
	(write-message-to-stream stream queue rewrite-type
				 :smtp smtp
				 :noclose noclose
				 :add-mbox-from add-mbox-from)
      (t (c)
	(maild-log "write-message-to-stream error: ~A" c)
	(setf (wmts-async-status async) c)
	;; Gotta do this, otherwise the listening party won't know
	;; what the deal is.
	(ignore-errors (close stream :abort t)))))
  (mp:open-gate (wmts-async-gate async)))
