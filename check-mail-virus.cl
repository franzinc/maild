(in-package :user)

;; return codes:
;;  0 -- okay
;;  1 -- transient
;;  2 -- reject

(defparameter *vcheckdir* "/var/spool/vcheck")
(defparameter *user* "mailnull")
(defparameter *ripmime* "/usr/local/bin/ripmime")
(defparameter *uvscan* "/usr/local/bin/uvscan")

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi))

(defun usage ()
  (error "Directory ~a must exist and be owned by user ~a~%"
	  *vcheckdir* *user*))

(defmacro with-temp-dir ((dir mode) &body body)
  (let ((dirsym (gensym))
	(modesym (gensym)))
    `(let ((,dirsym ,dir)
	   (,modesym ,mode))
       (unwind-protect 
	   (progn
	     (make-directory ,dirsym ,modesym)
	     ,@body)
	 ;;cleanup forms
	 (delete-directory-and-files ,dirsym :if-does-not-exist :ignore)))))
				     
(defun main (&rest args)
  (declare (ignore args))
  
  (if (not (probe-file *vcheckdir*))
      (usage))
  
  (let ((pwent (getpwnam *user*))
	sb tempdir)
	
    (when (null pwent)
      (format t "User ~a doesn't exist" *user*)
      (exit 1 :quiet t))
    
    (setf sb (stat *vcheckdir*))
    (if (/= (stat-uid sb) (pwent-uid pwent))
	(usage))
    
    ;; If running as root, become mailnull.
    (when (= (geteuid) 0)
      (setgid (pwent-gid pwent))
      (initgroups *user* (pwent-gid pwent))
      (setuid (pwent-uid pwent)))
    
    (setf tempdir 
      (format nil "~a/~a.~a" *vcheckdir* (getpid) (get-universal-time)))

    ;; eep
    (umask 0)

    (with-temp-dir (tempdir #o0700) 
      (let ((res (run-shell-command 
		  (vector *ripmime*
			  *ripmime*
			  "-i" "-" ;; read from stdin
			  "-d" tempdir))))
	(if (/= res 0)
	    (error "ripmime exited with code ~a" res)))
      (let ((res (run-shell-command
		  (vector *uvscan*
			  *uvscan*
			  "--secure"
			  tempdir))))
	
	(case res
	  (0 
	   (exit 0 :quiet t))
	  (13
	   (exit 2 :quiet t)) ;; reject message
	  (t
	   (error "uvscan exited with code ~a" res)))))))

(defun build ()
  (compile-file-if-needed "check-mail-virus.cl")
  (generate-executable "check-mail-virus" '("check-mail-virus.fasl")))
