;; $Id: checkers.cl,v 1.3 2003/07/08 18:05:24 layer Exp $

(in-package :user)

;; Checks that are run after a message body is received but before it
;; is accepted.  
;; Checkers are called with the following arguments:
;;    headers  : a list of the message headers 
;;    size : the message size in bytes 
;;    datafile : path to file which contains the message body data.
;; 
;; Checkers should return:
;;
;;   :ok   if the checker is satisfied.
;;   :transient   if the checker can't currently accept the message but
;;                might later.
;;   :reject    if the message is to be rejected.
;;
;;   for :transient and :reject, a second value can be returned.  It
;;   should be a text string will will be returned to the client.


;; This is the function that calls the checkers.  It returns
;; these values:
;;  1) status code (:ok, :transient, :reject)
;;  2) error text  (if :transient or :reject)
;;  3) name of checker (if :transient or :reject)


(defun check-message-checkers (q headers size)
  (dolist (checker *message-data-checkers* :ok)
    (multiple-value-bind (res text)
	(funcall (second checker) headers size (queue-datafile q))
      (case res
	(:reject
	 (if (null text)
	     (setf text "Message rejected"))
	 (return (values :reject text (first checker))))
	(:transient
	 (if (null text)
	     (setf text "(no additional details"))
	 (return (values :transient text (first checker))))
	(:ok
	 ) ;; so far so good
	(t
	 ;; something unexpected
	 (setf text 
	   (format nil "Unexpected return code from ~A: ~S"
		   (first checker) res))
	 (maild-log "~A" text)
	 (return (values :transient text (first checker))))))))

(defun message-size-checker (headers size datafile)
  (declare (ignore headers datafile))
  (if (and *maxmsgsize* (> *maxmsgsize* 0) (>= size *maxmsgsize*))
      (values :reject 
	      (format nil "5.2.3 Message exceeds maximum fixed size (~D)"
		      *maxmsgsize*))
    :ok))

(defun hop-count-checker (headers size datafile)
  (declare (ignore size datafile))
  (if (> (count-received-headers headers) *maximum-hop-count*)
      (values :reject "5.4.6 Too many hops")
    :ok))

(defun external-checker (prglist headers datafile)
  (block nil
    (let ((pwent (getpwnam *external-checker-user*)))
      (when (null pwent)
	(maild-log "external-checker: User ~a does not exist"
		   *external-checker-user*)
	(return :transient))
      (with-pipe (readfrom writeto)
	(mp:process-run-function "external checker message text generator"
	  #'external-checker-msgwriter writeto headers datafile)
	(multiple-value-bind (output errput status)
	    (command-output
	     (coerce (cons (first prglist) prglist) 'vector)
	     :directory "/tmp"
	     :input readfrom
	     :whole t
	     :uid (pwent-uid pwent)
	     :gid (pwent-gid pwent)
	     :initgroups-user *external-checker-user*)
	  (close readfrom)
	  (values status output errput))))))

(defun external-checker-msgwriter (writestream headers datafile)
  (dolist (h headers)
    (write-line h writestream))
  (write-line "" writestream)
  (with-open-file (f datafile)
    (let (char)
      (while (setf char (read-char f nil nil))
	     (write-char char writestream))))
  (close writestream))

;; utility for config file
(defun add-checker (name function)
  (setf *message-data-checkers* 
    (nconc *message-data-checkers* (list (list name function)))))

	   
	   
