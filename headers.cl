(in-package :user)

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

;; XXX - Should add quoting stuff in case the gecos part
;; has special characters..
;; It is okay for gecos to be nil..
(defun make-from-header (addr gecos)
  (let ((h (make-header))
	(addr (if (emailnullp addr) 
		  *mailer-daemon*
		(emailaddr-orig addr))))
    (add-header-word h "From:")
    (if* gecos
       then
	    (add-header-word h gecos)
	    (add-header-word h (format nil "<~A>" addr))
       else
	    (add-header-word h addr))
    (header-buffer h)))

(defun make-message-id-header (id)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore-if-unused second minute hour date month year day daylight-p zone))
    (format nil "Message-Id: <~D~D~D~D~D~D.~a@~A>"
	    year month date hour minute second
	    id
	    (fqdn))))


(defun make-received-header (cliaddr id recips)
  (let ((h (make-header)))
    (add-header-word h "Received:")
    (add-header-word h "from")
    (let ((hostname (ipaddr-to-hostname cliaddr)))
      (if* hostname
	 then
	      (add-header-word h hostname)
	      (add-header-word h (format nil "(~A)" (ipaddr-to-dotted cliaddr)))
	 else
	      (add-header-word h (ipaddr-to-dotted cliaddr))))
    (add-header-word h "by")
    (add-header-word h (fqdn))
    (add-header-word h "(Allegro maild)")
    (add-header-word h "(for")
    (dolist (recip recips)
      (add-header-word h (format nil "<~A>" (emailaddr-orig recip))))
    (add-header-word h ")" :nospace t)
    (add-header-word h "id")
    (add-header-word h (format nil "~A;" id))
    (add-header-word h (datetime))
    (header-buffer h)))

(defun datetime ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore-if-unused second minute hour date month year day daylight-p zone))
    (let ((zonesign (if (<= zone 0) "+" "-")))
      (format nil "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~a~4,'0d"
	      (svref *abbrev-days-of-week* day)
	      date
	      (svref *abbrev-months* month)
	      year
	      hour minute second
	      zonesign
	      (* (abs zone) 100)))))

;; Not used.
(defun parse-message-headers (filename)
  (let (res)
    (with-open-file (f filename)
      (loop
	(multiple-value-bind (line colonpos)
	    (get-unfolded-header f)
	  (if (null line)
	      (return))
	  (push (cons (subseq line 0 colonpos)
		      (subseq line (1+ colonpos)))
		res))))
    (reverse res)))
    
;; Returns nil at end of headers or end of file.
;; Used by parse-message-headers (therefore, unused)
(defun get-unfolded-header (f)
  (block nil
    (let ((line (read-line f nil nil))
	  colonpos peek contline startpos)
      (if (null line)
	  (return nil))
      (setf colonpos (position #\: line))
      (if (or (null colonpos) (not (valid-header-name-p line colonpos)))
	  (return nil))
      (loop
	;; see if we need to read in a continuation line.
	(setf peek (peek-char nil f nil nil))
	(if (not (member peek '(#\space #\tab)))
	    (return (values line colonpos)))
	;; Read the continuation line
	(setf contline (read-line f nil nil))
	(if (null contline)
	    (error "get-unfolded-line: This should never happen!"))
	(setf startpos (position-of-first-nonspace-character contline))
	;; if startpos is null, we've just read a continuation
	;; line that consists of all white space.  This is
	;; allowed by RFC822 but not by RFC2822.  
	(if startpos
	    (setf line 
	      (concatenate 'string line " " (subseq contline startpos))))))))

;;; nil means it's all whitespace (or null) past startpos
(defun position-of-first-nonspace-character (string &optional (startpos 0))
  (position-if-not 
   #'(lambda (char) (member char '(#\space #\tab)))
   string
   :start startpos))


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
    (if (prefix-of-p header h)
	(return h))))

;; Removes all instances
(defun remove-header (header headers)
  (remove-if #'(lambda (h) (prefix-of-p header h)) headers))

;; Returns the position of the header data
(defun recip-header-p (header)
  (cond 
   ((or (prefix-of-p "To:" header) (prefix-of-p "Cc:" header))
    3)
   ((prefix-of-p "Bcc:" header)
    4)
   (t
    nil)))

(defun sender-header-p (header)
  (if (prefix-of-p "From:" header)
      5))

(defun count-received-headers (headers)
  (count-if #'(lambda (h) (prefix-of-p "Received:" h)) headers))
