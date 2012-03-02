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
;; $Id: mailer.cl,v 1.6 2005/12/20 00:39:40 dancy Exp $

(in-package :user)

;; Called by make-delivery-command-for-recip
(defun get-mailer-by-id (id)
  (let ((entry (find id *mailers* :test #'eq :key #'first)))
    (if (null entry)
	(error "get-mailer-by-id: No mailer matches ~S!" id))
    entry))

(defmacro mailer-recip-lookup-func (mailer)
  `(third ,mailer))

;; recip should be a recip struct
;; called by mark-recip-with-suitable-mailer and any-mailer-matches-p
(defun lookup-recip-in-mailer (mailer recip)
  (block nil
    (let ((addr (recip-addr recip)))
      (if (funcall (mailer-recip-lookup-func mailer) addr)
	  (return recip))
      ;; Maybe it's an extended address
      (multiple-value-bind (addr extension)
	  (unextended-address addr)
	(if (null addr)
	    (return nil))
	;; It is.  Alter the recip struct.
	(setf (recip-extension recip) extension)
	(setf (recip-addr recip) addr)
	recip))))
  
;; called by queue-process-single-help
(defun mark-recip-with-suitable-mailer (recip)
  (block nil
    (if (recip-type recip) ;; special recip
	(return))
    (when (not (local-domain-p (recip-addr recip)))
      (setf (recip-mailer recip) :smtp)
      (return))
    (dolist (mailer *mailers*)
      (when (lookup-recip-in-mailer mailer recip)
	(setf (recip-mailer recip) (first mailer))
	(return)))
    (when (null (recip-mailer recip))
      ;; No mailer accepted the address.  Convert it into an
      ;; error recip.
      (setf (recip-type recip) :error)
      (setf (recip-errmsg recip) "No such mailbox"))))

;; addr is a parsed address.
;; called by: quick-verify-recip
(defun any-mailer-matches-p (addr)
  (dolist (mailer *mailers*)
    (if (lookup-recip-in-mailer mailer (make-recip :addr addr))
	(return t))))

;; returns the run-as user as a second value.
;; Called by deliver-via-mailer
(defun make-delivery-command-for-recip (recip q)
  (let ((mailer (get-mailer-by-id (recip-mailer recip))))
    (values (funcall (fourth mailer) recip q)
	    (fifth mailer))))

;;;; built-in mailers.

(defun lookup-addr-in-passwd (thing)
  (let ((addr (make-parsed-and-unparsed-address thing)))
    (and (local-domain-p addr)
	 (getpwnam (string-downcase (emailaddr-user addr))))))

(defun deliver-local-command (recip q)
  (list "/usr/bin/procmail" 
	"-Y"
	"-a"
	(if (recip-extension recip) (recip-extension recip) "")
	"-f"
	(emailaddr-orig (rewrite-local-envelope-sender (queue-from q)))
	"-d"
	(string-downcase (emailaddr-user (recip-addr recip)))))

(defun test-deliver-local-command (recip q)
  (declare (ignore recip q))
  (list
   ;; Yes, this is a build-time expansion of test/mailer.sh's pathname
   #.(namestring (merge-pathnames "test/mailer.sh"))
   *test-mode-mailbox*
   ))
