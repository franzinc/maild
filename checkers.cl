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
;; $Id: checkers.cl,v 1.6 2003/09/19 17:30:34 dancy Exp $

(in-package :user)

;; Checks that are run after a message body is received but before it
;; is accepted.  These checkers are used for messages that came in from
;; SMTP or stdin... and are primarily intended for checkers that want to
;; verify the message data itself.  If you want something that only runs
;; after a message has been received via the SMTP DATA command, use
;; an smtp-data-checker (see smtp-server-checkers.cl).
;;
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
	     (setf text "(no additional details)"))
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

	   
	   
