(in-package :user)


(defun lock-file (filename &key wait)
  (loop
    (if (lock-file-help filename)
	(return t))
    (if (not wait)
	(return nil))
    (sleep 1)))
  
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
    
(defmacro with-lock-file-nowait ((filename nolockform) &body body)
  (let ((filenamevar (gensym))
	(lockresvar (gensym)))
    `(let* ((,filenamevar ,filename)
	    (,lockresvar (lock-file ,filenamevar)))
       (if (null ,lockresvar)
	   ,nolockform
	 (unwind-protect
	     (progn
	       ,@body)
	   ;; It's okay for the body to delete the lockfile
	   (if (probe-file ,filenamevar)
	       (delete-file ,filenamevar)))))))
