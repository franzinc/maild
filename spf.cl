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
;; $Id: spf.cl,v 1.2 2005/06/16 18:25:26 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :aserve))

;; Implementation based on 
;; http://www.ozonehouse.com/mark/spf/draft-lentczner-spf-00.txt


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
	  )))))
	  
	  
(defun spf-record-p (string)
  (let ((elts (split-regexp "\\b+" string)))
    (and elts (equalp "v=spf1" (first elts)))))
  

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

;;; parsing stuff

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

(defun spf-parse-mechanism (string)
  (multiple-value-bind (action pos)
      (spf-prefix string)
    ))

(defun spf-parse-all (string pos max)
  (declare (ignore max))
  (match-regexp "^all$" string :start pos :case-fold t))

(defun spf-parse-include (string pos max)
  (when (match-regexp "^include" string :start pos :case-fold t)
    (incf pos #.(length "include"))
    (if (>= pos max)
	(error "Syntax error. include mechanism requires colon"))
    (let ((char (char string pos)))
      (if (char/= char #\:)
	  (error "Syntax error. include mechanism requires colon")))
    (incf pos)
    (spf-parse-domain-spec string pos max sender domain ip host ourdomain)
    ))

      
    


(defun spf-parse-ip4 (string pos max)
  (when (match-regexp "^ip4" string :start pos :case-fold t)
    (incf pos 3)
    (if (or (>= pos max)
	    (char/= (char string pos) #\:))
	(error "Syntax error in ip4 term.  colon is required"))
    (incf pos)
    (multiple-value-bind (network newpos)
	(spf-parse-dotted-ip4 string pos max)
      (if (null network)
	  (error "Syntax error in ip4 term. ip4-network is required"))
      (setf pos newpos)
      (let ((length (spf-parse-cidr-length string pos max)))
	(if (null length)
	    (setf length 32))
	(if (> length 32)
	    (error "Syntax error in ip4 term. Invalid CIDR length: ~A"
		   length))
	(values network length)))))
  

;; Signals an error on bogus IP address
(defun spf-parse-dotted-ip4 (string pos max)
  (if (>= pos max)
      nil
    (multiple-value-bind (found whole a b c d)
	(match-regexp "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" string :start pos)
      (when found
	(flet ((convert (string)
		 (let ((val (parse-integer string)))
		   (if (or (< val 0) (> val 255))
		       (error "Invalid IP address"))
		   val)))
	  (values (+ (ash (convert a) 24)
		     (ash (convert b) 16)
		     (ash (convert c) 8)
		     (convert d))
		  (+ pos (length whole))))))))

(defun spf-parse-dual-cidr-length (string pos max)
  (block nil
    (if (>= pos max)
	(return))
    (multiple-value-bind (ip4len newpos)
	(spf-parse-cidr-length string pos max)
      (if (null ip4len)
	  (return))
      (setf pos newpos)
      (multiple-value-bind (ip6len newpos)
	  (spf-parse-cidr-length string pos max)
	(if ip6len
	    (values ip4len ip6len newpos)
	  (values ip4len nil pos))))))

(defun spf-parse-cidr-length (string pos max)
  (if (>= pos max)
      nil
    (multiple-value-bind (matched whole digits)
	(match-regexp "^/\\([0-9]+\\)" string :start pos)
      (if matched
	  (let ((len (parse-integer digits)))
	    ;; Check lower bound.. but not upper bound since
	    ;; caller may be interested in either ip4 or ip6
	    ;; addresses
	    (if (< len 0)
		(error "Invalid CIDR length"))
	    (values len (+ pos (length whole))))))))

;; Macro stuff

(defmacro spf-parse-domain-spec (&rest args)
  `(spf-parse-macro-string ,@args :exclude-slash t))

(defun spf-parse-macro-string (string pos max sender domain ip host ourdomain
		     &key exclude-slash)
  (let ((string
	 (with-output-to-string (s)
	   (while (< pos max)
	     (let ((char (char string pos)))
	       (cond
		((and exclude-slash (char= char #\/))
		 (return))
		((char= char #\%)
		 (multiple-value-bind (res newpos)
		     (spf-parse-macro-expand string pos max sender domain
					     ip host ourdomain)
		   (setf pos newpos)
		   (write-string res s)))
		(t
		 (write-char char s)
		 (incf pos))))))))
    (values string pos)))
  

(defun spf-parse-macro-expand (string pos max sender domain ip host ourdomain)
  (macrolet ((check-len ()
	       `(if (>= pos max)
		    (error "Syntax error. Incomplete macro"))))
    
    (block nil
      (if (match-regexp "^%%" string :start pos)
	  (return (values "%" (+ pos 2))))
      (if (match-regexp "^%_" string :start pos)
	  (return (values " " (+ pos 2))))
      (if (match-regexp "^%-" string :start pos)
	  (return (values "%20" (+ pos 2))))
      (if (not (match-regexp "^%{" string :start pos))
	  (return (values "%" (1+ pos))))
      ;; start of a macro.
      (incf pos 2)
      (check-len)
      
      (let ((macro (char string pos)))
	(incf pos)
	(multiple-value-bind (use reverse pos)
	    (spf-parse-transformer string pos max)
	  (check-len)
	  ;; check for optional delimiters.
	  (multiple-value-bind (found delim)
	      (match-regexp "^[.+,/_=-]*" string :start pos)
	    (declare (ignore found))
	    (if (string= delim "")
		(setf delim ".")
	      (incf pos (length delim)))
	    (check-len)
	    (if (char/= (char string pos) #\})
		(error "Syntax error. Missing }"))
	    (incf pos)
	    
	    (let ((res 
		   (case (char-downcase macro)
		     (#\h "deprecated")
		     (#\s (emailaddr-orig sender))
		     (#\l (emailaddr-user sender))
		     (#\o (emailaddr-domain sender))
		     (#\d domain)
		     ((#\c #\i) (ipaddr-to-dotted ip))
		     (#\p host)
		     (#\v "in-addr") ;; ipv6 not supported yet
		     (#\r ourdomain)
		     (#\t 
		      (format nil "~d" 
			      (universal-to-unix-time (get-universal-time))))
		     (t "unknown"))))
	      
	      (setf res (spf-transform res delim use reverse))
	      
	      (if (upper-case-p macro)
		  (setf res (net.aserve:uriencode-string res)))
	      
	      (values res pos))))))))

(defun spf-transform (string delim use reverse)
  ;; split
  (let ((res (delimited-string-to-list string delim)))
    ;; reverse
    (if reverse
	(setf res (nreverse res)))
    ;; reduce
    (while (> (length res) use)
      (setf res (cdr res)))
    ;; rebuild
    (list-to-delimited-string res #\.)))

  
(defun spf-parse-transformer (string pos max)
  (if (>= pos max)
      nil
    (multiple-value-bind (found whole digits reverse)
	(match-regexp "^\\([0-9]*\\)\\(r\\|\\)" string :start pos 
		      :case-fold t)
      (if found
	  (values (if (string= digits "") 128 (parse-integer digits))
		  (if (string= reverse "") nil :reverse)
		  (+ pos (length whole)))
	(values 128 nil pos)))))
