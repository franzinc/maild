;; $Id: smtp.cl,v 1.3 2003/07/08 18:05:25 layer Exp $

(in-package :user)

;; Stuff common to the smtp server and client

;; handles Newline or CR/LF EOL convention.
(defun smtp-get-line (sock buf timeout)
  (let ((pos 0)
	longline
	lastchar
	char)
    ;; with-timeout is simpler here than with-socket-timeout and error
    ;; handling
    (mp:with-timeout (timeout :timeout)
      (loop
	(setf char (ignore-errors (read-char sock nil nil)))
	(if (null char)
	    (return :eof))
	
	(when (eq char #\linefeed)
	  (if longline
	      (return :longline))
	  (if (and (eq lastchar #\return) (> pos 0))
	      (decf pos))
	  (return (subseq buf 0 pos)))
	
	(if* (not longline)
	   then
		(setf (schar buf pos) char)
		(incf pos))
	(if (>= pos *maxlinelen*)
	    (setf longline t))
	(setf lastchar char)))))

