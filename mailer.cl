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
;; $Id: mailer.cl,v 1.3 2003/07/08 18:15:52 layer Exp $

(in-package :user)

(defun get-mailer-by-id (id)
  (let ((entry (find id *mailers* :test #'eq :key #'first)))
    (if (null entry)
	(error "get-mailer-by-id: No mailer matches ~S!" id))
    entry))

(defun mark-recip-with-suitable-mailer (recip)
  (block nil
    (if (recip-type recip) ;; special recip
	(return))
    (when (not (local-domain-p (recip-addr recip)))
      (setf (recip-mailer recip) :smtp)
      (return))
    (dolist (mailer *mailers*)
      (when (funcall (third mailer) (recip-addr recip))
	(setf (recip-mailer recip) (first mailer))
	(return)))
    (when (null (recip-mailer recip))
      ;; No mailer accepted the address.  Convert it into an
      ;; error recip.
      (setf (recip-type recip) :error)
      (setf (recip-errmsg recip) "No such mailbox"))))

(defun any-mailer-matches-p (addr)
  (dolist (mailer *mailers*)
    (if (funcall (third mailer) addr)
	(return t))))

;; returns the run-as user as a second value
(defun make-delivery-command-for-recip (recip q)
  (let ((mailer (get-mailer-by-id (recip-mailer recip))))
    (values (funcall (fourth mailer) (recip-addr recip) q)
	    (fifth mailer))))

;;;; built-in mailers.

(defun lookup-addr-in-passwd (thing)
  (let ((addr (make-parsed-and-unparsed-address thing)))
    (and (local-domain-p addr)
	 (getpwnam (string-downcase (emailaddr-user addr))))))

(defun deliver-local-command (addr q)
  (list "/usr/bin/procmail" 
	"-Y"
	"-f"
	(emailaddr-orig (rewrite-local-envelope-sender (queue-from q)))
	"-d"
	(string-downcase (emailaddr-user addr))))


