(in-package :user)

;; For sendmail compatibilty.
;; XXX -- not done yet.. and I'm considering dumping it.

(defparameter *virtusers* nil)

(defstruct virtusers
  (hash (make-hash-table :test #'equalp))
  (mtime 0))

(defun ensure-virtusers ()
  (if (null *virtusers*)
      (setf *virtusers* (make-virtusers)))
  (when (probe-file *virtusersfile*)
    (let ((mtime (file-write-date *virtusersfile*)))
      (when (> mtime (virtusers-mtime *virtusers*))
	(if *debug* (maild-log "Reparsing virtusers"))
	(setf (virtusers-hash *virtusers*) (parse-virtusers-file))
	(setf (virtusers-mtime *virtusers*) mtime)))))

;; The format is more strict than the aliases file.  No continuation
;; lines are allowed.  blank lines and lines that begin with '#' are comments.
;; key and value are separated by whitespace.  value part must be a valid
;; single email address.  
(defun parse-virtusers-file ()
  (with-open-file (f *virtusersfile*)
    (let ((linenum 0)
	  (hash (make-hash-table :test #'equalp))
	  addr
	  line)
      (while (setf line (read-line f nil nil))
	(incf linenum)
	(when (and (> (length line) 0)
		   (char/= (schar line 0) #\#)
		   (match-regexp "\\B" line))
	  (multiple-value-bind (found whole key value)
	      (match-regexp "^\\(\\B+\\)\\b+\\(.*\\)$" line)
	    (declare (ignore whole))
	    (if (not found)
		(error "~A line ~A: invalid format: ~A" 
		       *virtusersfile* linenum line))
	    (if (and (> (length value) 0) 
		     (member (schar value 0) '(#\| #\/)))
		(error "~A is an invalid right-hand-side in a virtusers file"
		       value))
	    (setf addr (parse-email-addr value))
	    (if (null addr)
		(error "~A line ~A: invalid right hand side: ~A"
		       *virtusersfile* linenum value))
	    (setf (gethash key hash) addr))))
      hash)))

;; addr can be a string or an emailaddr.
;; This should only be called for addresses in *localdomains*.
(defun lookup-virtuser (addr) 
  (block nil
    (ensure-virtusers)
    (let ((addrstring (if (emailaddr-p addr) 
			  (emailaddr-orig addr)
			addr))
	  (addr (if (emailaddr-p addr)
		    addr
		  (parse-email-addr addr))))
      ;; try specific match first
      (let ((expansion (gethash addrstring (virtusers-hash *virtusers*))))
	(if expansion
	    (return expansion)))
      ;; try inexact match if we have a domain name part.
      (if (null (emailaddr-domain addr))
	  (return))
      
      (let ((expansion (gethash (format nil "@~A" (emailaddr-domain addr))
				(virtusers-hash *virtusers*))))
	(if expansion
	    (return expansion)))
      nil)))
	  
	  
    
    
  
  

