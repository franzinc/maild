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
;; $Id: blacklist.cl,v 1.8 2008/07/31 02:28:28 dancy Exp $

(in-package :user)

;; email blacklists of various types.

;; Connection blacklist.  Connections made from these hosts will
;; result in a rude message being sent, followed by sudden disconnect.

;; XXX people might want a way to add exceptions.

(defparameter *blacklist-connections-parsed* nil)

(defun parse-connections-blacklist ()
  (setf *blacklist-connections-parsed* nil)
  (dolist (addr *blacklist-connections*)
    (push 
     (if (ip-addr-or-mask-p addr) 
	 (parse-addr addr) 
       (concatenate 'string "." addr))
     *blacklist-connections-parsed*)))

;; So we can differentiate between domain names
;; and ip.addr/netmask style addresses.
(defun ip-addr-or-mask-p (string)
  ;; should be all digits, dots, and slashes
  (match-regexp "^[0-9./]+$" string))
  

(defun connection-blacklisted-p (remote)
  ;; Collect reverse address info if available
  (let ((reverse (dns-ipaddr-to-hostname remote)))
    (dolist (bl *blacklist-connections-parsed*)
      (if* (network-address-p bl) 
	 then
	      (if (addr-in-network-p remote bl)
		  (return (ipaddr-to-dotted remote)))
	 else
	      ;; compare domain strings
	      (if (and reverse (within-blacklisted-domain-p reverse bl))
		  (return reverse))))))

;; true if 'blacklisted' is the suffix of 'domain'
(defun within-blacklisted-domain-p (domain blacklisted)
  (setf domain (concatenate 'string "." domain))
  (let ((startpos (- (length domain) (length blacklisted))))
    (if (< startpos 0)
	nil
      (equalp blacklisted (subseq domain startpos)))))	
      
;; MAIL FROM: blacklist

;; XXX Will want to improve this to support regular expressions
;; and whatnot.
(defun sender-blacklisted-p (sender)
  (dolist (addr *blacklist-from*)
    (if (equalp addr sender)
	(return t))))

;;;;  DNS-based blacklisting

;; Treats non-responses as not-blacklisted.
(defun connection-dns-blacklisted-p (ip)
  ;; Never blacklist trusted clients
  (when (not (trusted-client-p ip))
    (let ((flipped (flip-ip ip)))
      (dolist (item *dns-blacklists* nil)
	;; item == "hostname" or ("hostname" . ipaddr)
	(let* ((check-return-code (and (consp item)
				       (stringp (car item))
				       (stringp (cdr item))))
	       (domain (if* check-return-code
			  then (car item)
			  else item))
	       (blacklist-result (when check-return-code
				   (socket:dotted-to-ipaddr
				    (cdr item)
				    :errorp nil)))
	       result)
	  (when (and (or (not check-return-code)
			 ;; If a blacklist result code was specified and it
			 ;; didn't resolve, then ignore this dnsbl entry.
			 blacklist-result)
		     (setq result
		       (socket:dns-query
			(concatenate 'string flipped "." domain)
			:ignore-cache *ignore-dns-cache*))
		     (or (null blacklist-result)
			 (eql blacklist-result result)))
	    (return domain)))))))

;;; DNS Whitelisting (dnswl.org)

(defun dnswl-lookup (ip)
  (let* ((lookup (concatenate 'string (flip-ip ip) ".list.dnswl.org"))
	 (res (socket:dns-query lookup)))
    (when res
      (multiple-value-bind (x x category score)
	  (socket:ipaddr-to-dotted res :values t)
	(declare (ignore x))
	(let ((txt (socket:dns-query lookup :type :txt)))
	  (if txt
	      (setf txt (subseq (first txt) 0 (position #\space (first txt)))))
	  (values category score txt))))))
    
    
