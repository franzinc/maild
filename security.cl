(in-package :user)

(defun world-or-group-writable-p (file &key sb)
  (let ((sb (if sb sb (stat file))))
    (values
     (/= 0 (logand #o22 (stat-mode sb)))
     sb))) ;; in case there are other tests to be done

(defun verify-security (file &key writable-file-okay)
  (if (world-or-group-writable-p (dirname file))
      (error 
       "~A is in a world writable directory.  Aborting for security reasons"
       file))
  (let ((sb (stat file)))
    (if (and (not writable-file-okay)
	     (world-or-group-writable-p file :sb sb))   
	(error "~A is a world writable file.  Aborting for security reasons"
	       file))
    sb))

(defun verify-root-only-file (file)
  (let ((sb (verify-security file)))
    (if (/= 0 (stat-uid sb))
	(error "File ~A isn't owned by root.  Aborting for security reasons" 
	       file))
    sb))

(defun verify-real-user-is-root ()
  (if (/= (getuid) 0)
      (error "Permission denied")))
