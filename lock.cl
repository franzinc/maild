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
;; $Id: lock.cl,v 1.5 2003/09/30 17:56:42 dancy Exp $

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
  (when (stale-lockfile-p filename)
    (maild-log "Removing stale lockfile ~A" filename)
    (ignore-errors (delete-file filename)))
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

(defun refresh-lock-file (filename)
  (utime filename nil (get-universal-time)))


(defun stale-lockfile-p (filename)
  (let ((mtime (ignore-errors (file-write-date filename)))
	(now (get-universal-time)))
    (and mtime  (> now (+ mtime *queue-lock-timeout*)))))
    
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
