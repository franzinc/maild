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
;; $Id: utils.cl,v 1.18 2006/03/30 23:46:51 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
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

;; Parse floating point string.
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
  `(or (typep ,thing 'socket:socket)
       (typep ,thing 'excl::ssl-server-stream)))


(defmacro with-already-open-file ((f) &body body)
  `(when ,f
     (block nil
       (unwind-protect (progn ,@body)
	 (if ,f
	     (close ,f))))))
  

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

(defun relaying-allowed-p (addr from to)
  (dolist (checker *relay-checkers*)
    (if (funcall checker addr from to)
	(return t))))

;; Return true if string is a fully qualified domain name.
;; which basically means, return true if there is at least one
;; dot in string.
(defun fqdn-p (string)
  (find #\. string))

;;; 

(defun process-dead-p (process)
  #+(version= 6 2)(null (mp:process-implementation process))
  #+(version>= 7 0)(not (mp:process-alive-p process)))

(defun wait-for-process-to-die (process)
  (mp:process-wait 
   (format nil "Waiting for ~A to die" process)
   #'process-dead-p process))

(defun kill-and-reap-process (p)
  (mp:process-kill p)
  (wait-for-process-to-die p))

;;  

;; Returns two values:
;;  (1) # of characters in line (excluding newline).  will be nil on EOF.
;;  (2) true if a newline was read.

(defun get-line (stream buf)
  (declare (optimize (speed 3))
	   (type (simple-array character (*)) buf))
  (let ((count 0)
	(max (length buf))
	newline char)
    (declare (fixnum count max))
    (while (and (< count max) (setf char (read-char stream nil nil)))
      (when (char= char #\newline)
	(setf newline t)
	(return))
      
      (setf (aref buf count) char)
      (incf count))
    
    (if (and (= count 0) (null newline))
	(setf count nil)) ;; EOF
    
    (values count newline)))
