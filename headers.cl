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
;; $Id: headers.cl,v 1.16 2007/02/07 01:29:10 dancy Exp $

(in-package :user)

;; Header info table.. mostly stolen from sendmail's conf.c
(defstruct hdrinfo
  name ;; including the colon
  type) ;; list

(defmacro mkfromhdr (name)
  `(make-hdrinfo :name (concatenate 'string ,name ":") :type '(:from)))

(defmacro mktohdr (name)
  `(make-hdrinfo :name (concatenate 'string ,name ":") :type '(:to)))


(defparameter *hdrinfos*
    (list
     (mkfromhdr "resent-sender")
     (mkfromhdr "resent-from")
     (mkfromhdr "resent-reply-to")
     (mkfromhdr "sender")
     (mkfromhdr "from")
     (mkfromhdr "reply-to")
     (mkfromhdr "errors-to")
     (mkfromhdr "return-receipt-to")
     
     (mktohdr "to")
     (mktohdr "resent-to")
     (mktohdr "cc")
     (mktohdr "resent-cc")
     (mktohdr "bcc")
     (mktohdr "resent-bcc")
     (mktohdr "apparently-to")))


(defstruct header
  buffer
  (current-line-len 0))

(defparameter *newline-string* (make-string 1 :initial-element #\newline))

(defun add-header-word (h word &key nospace)
  (if* (null (header-buffer h))
     then
	  (setf (header-buffer h) word)
	  (setf (header-current-line-len h) (length word))
     else
	  (if* (> (+ (if nospace 0 1) 
		     (length word) 
		     (header-current-line-len h)) 
		  78)
	     then
		  (let ((nextline (concatenate 'string "    " word)))
		    (setf (header-buffer h)
		      (concatenate 'string 
			(header-buffer h) 
			*newline-string*
			nextline))
		    (setf (header-current-line-len h)
		      (length nextline)))
	     else
		  (setf (header-buffer h)
		    (concatenate 'string
		      (header-buffer h)
		      (if nospace "" " ")
		      word))
		  (incf (header-current-line-len h)
			(+ (if nospace 0 1) (length word))))))
  


(defun make-x-auth-warning-header (realuser fromaddr)
  (let ((h (make-header)))
    (dolist (word (list 
		   "X-Authentication-Warning:"
		   (format nil "~A:" (fqdn))
		   realuser "set" "sender" "to"
		   (if (emailaddr-p fromaddr) 
		       (emailaddr-orig fromaddr)
		     fromaddr)
		   "using" "-f"))
      (add-header-word h word))
    (header-buffer h)))
  
(defun make-date-header ()
  (concatenate 'string "Date: " (datetime)))

;; It is okay for gecos to be nil..
(defun make-from-header (addr gecos)
  (let ((h (make-header))
	(addr (if* (emailnullp addr) 
		 then *mailer-daemon*
		 else (emailaddr-orig addr))))
    (add-header-word h "From:")
    (if* gecos
       then (add-header-word h (quote-if-necessary gecos))
	    (add-header-word h (format nil "<~A>" addr))
       else (add-header-word h addr))
    (header-buffer h)))

(defun make-message-id-header (id)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore-if-unused second minute hour date month year day daylight-p zone))
    (format nil "Message-Id: <~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d.~a@~A>"
	    year month date hour minute second
	    id
	    (fqdn))))

(defun get-message-id (headers)
  (multiple-value-bind (h pos)
      (locate-header "Message-ID:" headers)
    (if* (null h)
       then "<none>"
       else (subseq h pos))))

(defun make-received-header (id helo cliaddr esmtp auth-user ssl)
  (let ((h (make-header)))
    (add-header-word h "Received:")
    (when helo 
      ;; Received via SMTP
      (add-header-word h "from")
      (add-header-word h helo)
      (add-header-word h (format nil "(~@[~a ~][~a])" 
				 (ipaddr-to-hostname cliaddr)
				 (ipaddr-to-dotted cliaddr))))
    (add-header-word h "by")
    (add-header-word h (fqdn))
    (add-header-word h 
		     (format nil "(Allegro Maild ~A)" *allegro-maild-version*))
    (when (and esmtp (or auth-user ssl))
      (add-header-word h "with")
      (add-header-word h (if* (and ssl auth-user)
			    then "ESMTPSA"
			  elseif ssl
			    then "ESMTPS"
			  elseif auth-user
			    then "ESMTPA"
			    else (error "This can never happen"))))
      
    (add-header-word h "id")
    (add-header-word h (format nil "~A;" id))
    (add-header-word h (datetime))
    (header-buffer h)))

(defun datetime ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore-if-unused second minute hour date month year day zone))
    (if daylight-p
	(decf zone))
    (let ((zonesign (if (<= zone 0) "+" "-")))
      (format nil "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~a~4,'0d"
	      (svref *abbrev-days-of-week* day)
	      date
	      (svref *abbrev-months* month)
	      year
	      hour minute second
	      zonesign
	      (* (abs zone) 100)))))

;;; nil means it's all whitespace (or null) past startpos
(defun position-of-first-nonspace-character (string &optional (startpos 0))
  (position-if-not 
   #'(lambda (char) (member char '(#\space #\tab)))
   string
   :start startpos))

(defun header-unfold (curr next)
  (let ((startpos (position-of-first-nonspace-character next)))
    (if startpos
	(concatenate 'string curr " " (subseq next startpos))
      curr)))

(defun header-fold (string)
  (let ((words (split-regexp "\\b+" string))
	(h (make-header)))
    (dolist (word words)
      (add-header-word h word))
    (split-regexp *newline-string* (header-buffer h))))


(defun valid-header-name-p (string stoppos)
  (let (code)
    (dotimes (i stoppos t)
      (setf code (char-code (schar string i)))
      (if (or (< code 33) (> code 126))
	  (return nil)))))

(defun valid-header-line-p (string endpos &key strict)
  (block nil
    ;; check for continuation-lineness
    (if (and (not strict) (>= endpos 1) (whitespace-p (schar string 0)))
	(return t))
    (let ((colonpos (position #\: string :end endpos)))
      (if (null colonpos)
	  (return nil))
      (valid-header-name-p string colonpos))))

;; Returns the first matching header.  
(defun locate-header (header headers)
  (dolist (h headers nil)
    (when (prefix-of-p header h)
      (let* ((aftercolonpos (1+ (position #\: h)))
	     (pos (position-of-first-nonspace-character h aftercolonpos)))
	(return 
	  (values h (if pos pos aftercolonpos)))))))


;; Removes all instances
(defun remove-header (header headers)
  (remove-if #'(lambda (h) (prefix-of-p header h)) headers))


;; Returns the position of the header data
(defun recip-header-p (header)
  (dolist (hi *hdrinfos*)
    (if (and (member :to (hdrinfo-type hi))
	     (prefix-of-p (hdrinfo-name hi) header))
	(return (length (hdrinfo-name hi))))))

(defun sender-header-p (header)
  (dolist (hi *hdrinfos*)
    (if (and (member :from (hdrinfo-type hi))
	     (prefix-of-p (hdrinfo-name hi) header))
	(return (length (hdrinfo-name hi))))))
    

(defun count-received-headers (headers)
  (count-if #'(lambda (h) (prefix-of-p "Received:" h)) headers))
