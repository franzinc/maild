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
;; $Id: utils.cl,v 1.9 2003/07/08 18:15:53 layer Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :acldns)
  (require :osi)
  (use-package :excl.osi))

(defparameter *outline-crlf* t)
(defparameter *outline-flush* t)

;; never call outline w/ a first argument that is anything
;; but a format string.  This macro checks for that situation
;; to be safe.
(defmacro outline (stream format-string &rest args)
  (let ((ressym (gensym))
	(streamsym (gensym)))
    (when (not (stringp format-string))
      (error "Ack!! format-string is not a string constant: ~s."
	     format-string))
    `(let ((,streamsym ,stream)
	   (,ressym (format nil ,format-string ,@args)))
       (write-string ,ressym ,streamsym)
       (if *outline-crlf*
	   (write-char #\return ,streamsym))
       (write-char #\newline ,streamsym)
       (if *outline-flush*
	   (force-output ,streamsym)))))

(defparameter *abbrev-days-of-week* 
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *abbrev-months*
    #(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun" 
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))


(defmacro with-my-temp-file ((streamvar filenamevar format) &body body)
  `(multiple-value-bind (,streamvar ,filenamevar)
       (mkstemp ,format)
     (declare (ignore-if-unused ,streamvar ,filenamevar))
     (unwind-protect
	 (progn
	   ,@body)
       (if (streamp ,streamvar) (close ,streamvar)))))

(defmacro whitespace-p (char)
  (let ((charsym (gensym)))
    `(let ((,charsym ,char))
       (or (char= ,charsym #\space) (char= ,charsym #\tab)))))

;; Case insensitive
(defun prefix-of-p (prefix whole)
  (block nil
    (let ((lenwhole (length whole))
	  (lenprefix (length prefix)))
      (if (< lenwhole lenprefix)
	  (return nil))
      (equalp prefix (subseq whole 0 lenprefix)))))

(defun wait-for-process (proc reason)
  (mp:process-wait 
   reason
   #'(lambda (p) (not (mp:process-active-p p))) proc))

(defun skipwhite (string pos &key vertical)
  (let ((spacechars '(#\space #\tab))
	(max (length string))
	char)
    (if vertical
	(setf spacechars (nconc spacechars '(#\newline #\return))))
    (loop
      (if (>= pos max)
	  (return pos))
      (setf char (schar string pos))
      (if (not (member char spacechars))
	  (return pos))
      (incf pos))))

(defun parse-fp (string)
  (block nil
    (let ((max (length string))
	  (value 0)
	  (pos 0)
	  (nths 10)
	  (wholepart t)
	  char charval negate)
      (if (string= string "")
	  (return (values 0 0)))
      (when (char= (schar string 0) #\-)
	(setf negate t)
	(incf pos))
      (loop
	(if (>= pos max)
	    (return))
	(setf char (schar string pos))
	(if (digit-char-p char)
	    (setf charval (- (char-code char) 48)))
	(cond 
	 ((and wholepart (digit-char-p char))
	  (setf value (+ (* value 10) charval)))
	 ((and wholepart (char= char #\.))
	  (setf wholepart nil))
	 ((not (digit-char-p char))
	  (return)) ;; break out of loop
	 (t
	  ;; fractional part
	  (setf value (+ value (/ charval nths)))
	  (setf nths (* 10 nths))))
	(incf pos))
      (if negate
	  (setf value (- value)))
      (values (coerce value 'float) pos))))
	
(defmacro socketp (thing)
  `(typep ,thing 'socket:socket))

;; Modeled after the glibc resolver which uses the search path first
;; if there is no dot in the domain name.  
(defun useful-dns-query (domain &rest rest &key (search t) &allow-other-keys)
  (macrolet ((flags (respform)
	       (let ((respvar (gensym)))
		 `(let ((,respvar ,respform))
		    (if (eq (type-of ,respvar) 'dns-response)
			(dns-response-flags ,respvar)
		      (fourth ,respvar))))))
    ;; since we rely on *dns-domain* and *domain-search-list*
    (configure-dns :auto t) 
    ;; Avoid using old data.
    (setf socket::*stale-entry-remove-time* 0)
    (let ((search-list (copy-list *domain-search-list*))
	  res)
      (if *dns-domain*
	  (pushnew *dns-domain* search-list :test #'equalp))
      (if (and search (null (position #\. domain)))
	  (dolist (searchdom search-list)
	    (setf res 
	      (multiple-value-list 
	       (apply #'dns-query (cons 
				   (concatenate 'string domain "." searchdom)
				   rest))))
	    ;; Only loop if there's a definitely negative answer.
	    (if (not (member :no-such-domain (flags res)))
		(return-from useful-dns-query (values-list res)))))
      (apply #'dns-query (cons domain rest)))))


;; t -> definitely yes
;; nil -> definitely no
;; :unknown -> no sure [didn't get an answer or something]

(defun domain-exists-p (domain)
  (let ((resp (useful-dns-query domain :decode nil :type :any)))
    (cond 
     ((null resp)
      :unknown)
     ((member :no-such-domain (dns-response-flags resp))
      nil)
     (t
      t))))

(defmacro with-already-open-file ((f) &body body)
  `(when ,f
     (block nil
       (unwind-protect (progn ,@body)
	 (if ,f
	     (close ,f))))))
  

;; Requires properly set up forward and reverse DNS
(defun compute-fqdn ()
  (let ((mainip (useful-dns-query (gethostname)))
	fqdn)
    (if (null mainip)
	(error "Could not resolve our hostname (~A) via DNS" (gethostname)))
    (setf fqdn (dns-ipaddr-to-hostname mainip))
    (if (null fqdn)
	(error "Could not do a reverse DNS lookup on our IP address (~A)"
	       (ipaddr-to-dotted mainip)))
    fqdn))


;; If there are DNS troubles, the user can set *fqdn* in the config file.
(defun fqdn ()
  (if *fqdn*
      *fqdn*
    (setf *fqdn* (compute-fqdn))))

(defun short-host-name ()
  (if *short-host-name*
      *short-host-name*
    (let* ((hostname (gethostname))
	   (dot-pos (position #\. hostname)))
      (setf *short-host-name* (subseq hostname 0 dot-pos)))))
    

(defmacro with-socket-timeout ((sock type timeout) &body body)
  (let* ((types '((:read . excl::stream-read-timeout)
		  (:write . excl::stream-write-timeout)))
	 (accessor (cdr (assoc type types))))
    (if (null accessor)
	(error "with-socket-XXXX-timeout: type must be :read or :write"))
    (let ((sockvar (gensym))
	  (timeoutvar (gensym))
	  (origvar (gensym)))
      `(let ((,sockvar ,sock)
	     (,timeoutvar ,timeout)
	     ,origvar)
	 (if (socketp ,sockvar)
	     (progn
	       (setf ,origvar (,accessor ,sockvar))
	       (setf (,accessor ,sockvar) ,timeoutvar)))
	 (unwind-protect
	     (progn ,@body)
	   (if (socketp ,sockvar)
	       (setf (,accessor ,sockvar) ,origvar)))))))

(defun flip-ip (ip)
  (ipaddr-to-dotted
   (logior (ash (logand ip #xff000000) -24)
	   (ash (logand ip #x00ff0000) -8)
	   (ash (logand ip #x0000ff00) 8)
	   (ash (logand ip #x000000ff) 24))))
