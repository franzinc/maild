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
;; $Id: spf.cl,v 1.1 2005/06/15 20:36:09 dancy Exp $

(in-package :user)

;; prefixes:
;; - fail 
;; ~ softfail
;; + pass  (default prefix if unspecified)
;; ? neutral

;; Mechanisms are matching predicates.  If there's a match, do what the
;; prefix says.  If there is no match, move to the next.  If nothing
;; matching, result is neutral.

;; returns from check_host:
;; neutral.. which is the same as none.
;; pass: client is authorized to inject mail. 
;; fail: client is not authorized to inject.
;;    should result in a 550 reply that lists the reason for the failure.
;;  failure reasons:
;;   Not Permitted  (include explanation if possible)
;;   Malformed Domain
;;   Domain Does Not Exist

;; softfail:  Probably bogus.. perhaps mark the message and send it on.
;;            or subject it to further scrutiny.
;; none: no SPF records were provided by the domain.  (we use nil)
;; temperror: reject w/ 450.  Good for DNS lookup problems.
;; permerror: reject w/ 550. A PermError result means that the
;;  domain's published records couldn't be correctly interpreted for
;;  this "Mail From" identity.  Syntax error or whatnot.. loops..

;; TODOs:
;; *Make sure that include directives don't include loops.

;; ip is a numeric ip (will need to change later to support ipv6).
;; domain is a string.
;; sender is a parsed email addr.
(defun spf-check-host (ip domain sender)
  (block nil
    (if (not (fqdn-p domain))
	(return (values :fail "Malformed Domain")))
    
    (let ((spf (spf-lookup domain)))
      (if (symbolp spf)
	  (return spf))
      (let ((terms (split-regexp "\\b+" spf)))
	(pop terms) ;; skip header.
	
	(dolist (term terms)
	  ;; check for a prefix.

(defun spf-prefix (string)
  (let ((char (char string 0)))
    (case char
      (#\+
       (values :pass 1))
      (#\-
       (values :fail 1))
      (#\?
       (values :neutral 1))
      (#\~
       (values :soft-fail 1))
      (t
       (values :pass 0)))))


(defun spf-record-p (string)
  (let ((elts (split-regexp "\\b+" string)))
    (and elts (string= "v=spf1" (first elts)))))
  

;; Returns the spf string or a status keyword.. or nil
(defun spf-lookup (domain)
  (block nil
    (let ((txts (spf-lookup-help domain)))
      (if (symbolp txts)
	  (return txts))
      (setf txts (remove-if-not #'spf-record-p txts))
      (if (null txts)
	  (return))
      (when (/= (length txts) 1)
	(maild-log "Domain ~a has multple SPF records." domain)
	(return :perm-error))
      (first txts))))
  

;; Return a list of TXT records, or a status keyword
(defun spf-lookup-help (domain)
  (block nil
    (multiple-value-bind (txts ttl rest status)
	(socket:dns-query domain :type :txt)
      (declare (ignore ttl))
      (if (member :no-such-domain status)
	  (return :no-such-domain))
      (when (member :error status)
	(maild-log "DNS error while looking up TXT for domain ~A" domain)
	(return :temp-error))
      (if (null txts)
	  (return))
      (setf txts (cons txts rest))
      ;; Each TXT record is a list of strings.  
      (let (res)
	(dolist (txt txts)
	  (push
	   (with-output-to-string (s)
	     (dolist (string txt)
	       (write-string string s)))
	   res))
	res))))
