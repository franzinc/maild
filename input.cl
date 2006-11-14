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
;; $Id: input.cl,v 1.16 2006/11/14 23:09:08 dancy Exp $

(in-package :user)

(defun trusted-user-p (us)
  (member us *trusted-users* :test #'equalp))

;; returns (values parsed-from-address from-name real-user)
(defun compute-sender-info (user-fromaddr user-gecos)
  (let* ((uid (getuid)) ;; the real uid
	 (pwent (getpwuid uid))
	 (auth-warn t)
	 gecos parsedfromaddr )
    (if (null pwent)
	(error "I can't figure out who you are! (uid=~D)" uid))
    
    ;; It is okay for gecos to end up being nil.  This means
    ;; that no human-friendly name will be in the From: 
    ;; header.
    (if* (null user-fromaddr)
       then
	    (setf user-fromaddr (pwent-name pwent))
	    (setf gecos (pwent-gecos pwent))
	    (setf auth-warn nil))
    
    (setf parsedfromaddr 
      (parse-email-addr user-fromaddr :allow-null t))
    (if (null parsedfromaddr)
	(error "~A is not a valid address" user-fromaddr))
    
    ;; don't need to look up gecos info if the fromaddr
    ;; is not local.. or if user-gecos is specified.
    (if (and (local-domain-p parsedfromaddr) (null user-gecos))
	(let ((newpwent 
	       (getpwnam (string-downcase (emailaddr-user parsedfromaddr)))))
	  (if* newpwent
	     then
		  (setf gecos (pwent-gecos newpwent))
		  (if (string= (pwent-name pwent) (pwent-name newpwent))
		      (setf auth-warn nil)))))

    (if user-gecos
	(setf gecos user-gecos))
    
    (values 
     parsedfromaddr 
     gecos 
     (if (trusted-user-p (pwent-name pwent))
	 nil
       auth-warn)
     (pwent-name pwent))))
  

;; 'recips' is a (possibly empty) list of recip structs
(defun send-from-stdin (recips &key (dot t) gecos from verbose 
				    grab-recips metoo)
  (multiple-value-bind (fromaddr gecos authwarn realuser)
      (compute-sender-info from gecos)
    (let (q errstatus)
      (with-new-queue (q f errstatus fromaddr *localhost*)
	;; body doesn't execute if datafile open failed.
	(multiple-value-bind (status headers)
	    (read-message-stream *standard-input* f :dot dot)
	  (if (not (member status '(:eof :dot)))
	      (error "got status ~s from read-message-stream" status))
	  
	  (if grab-recips
	      (setf recips 
		(append recips (grab-recips-from-headers headers))))
	  
	  (if (null recips)
	   (error "No recipient addresses found in header"))
	  
	  (if authwarn
	      (setf headers 
		(append headers 
			(list (make-x-auth-warning-header realuser fromaddr)))))
	  (queue-prefinalize q recips headers :metoo metoo)

	  ;; Run message checkers.
	  (multiple-value-bind (res text checker)
	      (check-message-checkers q)
	    (declare (ignore checker))
	    (ecase res
	      (:ok 
	       ) ;; all is well
	      ((:transient :reject)
	       (error "Message rejected: ~A" text))))

	  
	  ;; This marks the message as complete.
	  (queue-finalize q recips headers 
			  :date t
			  :add-from t 
			  :from-gecos gecos
			  :metoo metoo)))
      
      (when errstatus
	;; somethin' went wrong.  It should already have been logged.
	;; Report it to the user and bail out.
	(error (second errstatus)))
      
      ;; XXX -- this might need changing.
      ;; I think sendmail goes through and processes local recips
      ;; and returns immediate errors (dead.letter).. then forks
      ;; to handle the rest.. which may potentially be slower.
      ;; For now, we're going to fork for any type of message.
      
      ;; Verbosity implies no fork.
      (let ((pid (if (or verbose *debug*) 0 (fork))))
	(if* (= pid 0)
	   then
		(if (and (not *debug*) (not verbose))
		    (detach-from-terminal))
		(queue-process-single (queue-id q) :wait t :verbose t))))))

;; Works right even if there are multiple To:, Cc: or Bcc: headers.
;; Works on folded headers now too.
(defun grab-recips-from-headers (headers)
  (let (good-recips pos h nextline)
    (while headers
	   (setf h (pop headers))
	   (when (recip-header-p h)
	     (while (setf nextline (pop headers))
		    (if (or (= 0 (length nextline))
			    (not (whitespace-p (schar nextline 0))))
			(return)) ;; break
		    (setf h (header-unfold h nextline)))
	     ;; get here if nextline wasn't there.. or if it was
	     ;; the beginning of a new header
	     (if nextline 
		 (push nextline headers))
	     
	     (setf pos (recip-header-p h))
	     (when pos
	       (setf good-recips
		 (nconc good-recips 
			(get-good-recips-from-string h :pos pos))))))
    good-recips))

(defun read-message-stream (s bodystream &key smtp dot)
  (let ((res (multiple-value-list 
	      (read-message-stream-inner s bodystream :smtp smtp :dot dot))))
    (finish-output bodystream)
    (fsync bodystream) ;; Try to make sure the data file is really on disk.
    (values-list res)))
  

(defun read-message-stream-inner (s bodystream &key smtp dot)
  (let ((count 0)
	(doingheaders t)
	(firstline t)
	(buffer (make-string *maxdatalinelen*))
	lastincomplete
	headers)
    (if smtp 
	(setf dot t))
    (loop
      (multiple-value-bind (endpos maxed)
	  (read-message-stream-line s buffer
				    :timeout (if smtp *datatimeout*))
	(if (eq endpos :eof)
	    (return (values :eof (reverse headers) count)))
	
	(if (and dot
		 (not lastincomplete) 
		 (= endpos 1) 
		 (char= (schar buffer 0) #\.))
	    (return (values :dot (reverse headers) count)))

	(incf count endpos)
	
	(if* doingheaders
	   then
		(cond
		 ;; Special case.  Messages that begin w/ a Unix
		 ;; mailbox "From " separator have the separator 
		 ;; stripped.
		 ((and firstline (>= endpos 5) 
		       (string= (subseq buffer 0 5) "From "))
		  ;; Just ignore it
		  )
		 ((and firstline 
		       (not (valid-header-line-p buffer endpos :strict t)))
		  (setf doingheaders nil)
		  (read-message-stream-write-body 
		   bodystream buffer endpos count 
		   :nonewline maxed
		   :smtp smtp))
		 ((= endpos 0) ;; blank line
		  (setf doingheaders nil)
		  (if (null headers)
		      (write-char #\newline bodystream)))
		 ((valid-header-line-p buffer endpos)
		  (push (subseq buffer 0 endpos) headers))
		 (t ;; non-header
		  (setf doingheaders nil)
		  (read-message-stream-write-body 
		   bodystream buffer endpos count
		   :nonewline maxed
		   :smtp smtp)))
		
		(setf firstline nil)
	   else
		;; doing body.
		(read-message-stream-write-body 
		 bodystream buffer endpos count
		 :nonewline maxed
		 :smtp smtp))
	
	(setf lastincomplete maxed)))))
		

(defun read-message-stream-write-body (s buffer endpos count 
				       &key nonewline smtp)
  (when (or (null *maxmsgsize*) (<= *maxmsgsize* 0) (< count *maxmsgsize*))
    (write-string buffer s 
		  :start 
		  (if (and smtp 
			   (>= endpos 2) 
			   (string= (subseq buffer 0 2) ".."))
		      1 0)
		  :end endpos)
    (if (null nonewline)
	(write-char #\newline s))))

	
(defun read-message-stream-line (s buffer &key timeout)
  (with-socket-timeout (s :read timeout)
    (read-message-stream-line-inner s buffer)))


(defun read-message-stream-line-inner (s buffer)
  (let ((pos 0)
	(max (length buffer))
	lastchar
	char)
    (loop
      (when (>= pos max)
	(unread-char lastchar s)
	(return (values (1- pos) :max)))
      
      (setf char (read-char s nil nil))
      
      ;;; if we're in the middle of a line, treat EOF as EOL
      (if (null char)
	  (return (if (= pos 0) :eof pos)))
      
      (when (eq char #\linefeed)
	(if (and (eq lastchar #\return) (> pos 0))
	    (decf pos))
	(return pos))
      
      (setf (schar buffer pos) char)
      (setf lastchar char)
      (incf pos))))
