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

(defun unquote-quoted-string (string)
  (let ((outstring (make-string (length string)))
	(outpos 0)
	(inpos 0)
	(max (length string))
	quote
	char)
    (while (< inpos max)
      (setf char (schar string inpos))
      (cond
       (quote
	(setq quote nil)
	(setf (schar outstring outpos) char)
	(incf outpos)
	(incf inpos))
       ((char= char #\")
	(incf inpos))
       ((char= char #\\)
	(incf inpos)
	(setf quote t))
       (t
	(setf (schar outstring outpos) char)
	(incf outpos)
	(incf inpos))))
    (subseq outstring 0 outpos)))

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
    (let (res)
      (if (and search (null (position #\. domain)))
	  (dolist (searchdom *domain-search-list*)
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
  
	     