(in-package :user)

(defun world-writable-p (file)
  (/= 0 (logand 2 (stat-mode (stat file)))))

(defun verify-security (file &key writable-file-okay)
  (if (world-writable-p (dirname file))
      (error 
       "~A is in a world writable directory.  Aborting for security reasons"
       file))
  (if (and (not writable-file-okay)
	   (world-writable-p file))   
      (error "~A is a world writable file.  Aborting for security reasons"
	     file)))

