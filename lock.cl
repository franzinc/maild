;; $Id: lock.cl,v 1.3 2003/07/08 18:05:24 layer Exp $

(in-package :user)


(defun lock-file (filename &key wait)
  (let ((whostate (format nil "Waiting on ~A" filename)))
    (loop
      (if (lock-file-help filename)
	  (return t))
      (if (not wait)
	  (return nil))
      (sleep 1 whostate))))
  
(defun lock-file-help (filename)
  (handler-case
      (let ((f (os-open filename (logior *o-excl* *o-creat* *o-wronly*) 
			#o0600)))
	(format f "~D~%" (getpid))
	(close f)
	t)
    (syscall-error (e)
      (if (= (syscall-error-errno e) *eexist*) 
	  nil
	(error e)))))

(defmacro with-lock-file ((filename nolockform &key wait) &body body)
  (let ((filenamevar (gensym))
	(lockresvar (gensym))
	(waitvar (gensym)))
    `(let* ((,filenamevar ,filename)
	    (,waitvar ,wait)
	    (,lockresvar (lock-file ,filenamevar :wait ,waitvar)))
       (if* (null ,lockresvar) ;; couldn't get the lock
	  then
	       (if ,waitvar
		   (error "with-lock-file: lock-file returned nil when it shouldn't have")
		 (progn
		   ,nolockform))
	  else
	       (unwind-protect
		   (progn
		     ,@body)
		 ;; It's okay for the body to delete the lockfile
		 (if (probe-file ,filenamevar)
		     (delete-file ,filenamevar)))))))
