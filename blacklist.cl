;; $Id: blacklist.cl,v 1.4 2003/07/08 18:05:24 layer Exp $

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
  (let ((flipped (flip-ip ip)))
    (dolist (domain *dns-blacklists* nil)
      (if (socket:dns-query (concatenate 'string flipped "." domain))
	  (return domain)))))


