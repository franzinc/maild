;; $Id: sasl.cl,v 1.1 2005/09/22 04:02:57 dancy Exp $

(in-package :user)

;; Functions that speak the two simple SASL mechanisms.

(defparameter *sasl-mechs* 
    '(("LOGIN" . sasl-login-mech)
      ("PLAIN" . sasl-plain-mech)))
  

(defun sasl-login-mech (sess initial)
  "If successful, returns username and password strings.
   Otherwise, returns:
      NIL: base64 decoding problem.
      <keyword>: smtp-get-line problem."
  (declare (ignore initial))
  (block nil
    (let ((sock (session-sock sess))
	  (buf (session-buf sess))
	  input username password)
      (outline sock "334 ~a" (string-to-base64-string "Username:"))
      
      (setf input (smtp-get-line sock buf *cmdtimeout*))
      (if (not (stringp input))
	  (return input))
      (setf username (ignore-errors (base64-string-to-string input)))
      (when (null username)
	(outline sock "501 5.5.4 cannot decode AUTH parameter ~a" input)
	(return))

      (outline sock "334 ~a" (string-to-base64-string "Password:"))
      
      (setf input (smtp-get-line sock buf *cmdtimeout*))
      (if (not (stringp input))
	  (return input))
      (setf password (ignore-errors (base64-string-to-string input)))
      (when (null password)
	(outline sock "501 5.5.4 cannot decode AUTH parameter ~a" input)
	(return))
      
      (values username password))))

(defun sasl-plain-mech (sess initial)
  "If successful, returns username and password strings.
   Otherwise, returns:
      NIL: base64 decoding problem.
      <keyword>: smtp-get-line problem."
  (block nil
    (let ((sock (session-sock sess))
	  (buf (session-buf sess))
	  input nullpos1 nullpos2 decoded username password)
      
      (if* initial
	 then
	      (setf input initial)
	 else
	      (outline sock "334 ")
	      (setf input (smtp-get-line sock buf *cmdtimeout*))
	      (if (not (stringp input))
		  (return input)))
      
      (setf decoded (ignore-errors (base64-string-to-string input)))
      (when (or (null decoded) (/= 2 (count #\null decoded)))
	(outline sock "501 5.5.4 cannot decode AUTH parameter ~a" input)
	(return))

      (setf nullpos1 (position #\null decoded))
      (setf nullpos2 (position #\null decoded :start (1+ nullpos1)))
      (setf username (subseq decoded (1+ nullpos1) nullpos2))
      (setf password (subseq decoded (1+ nullpos2)))
      
      (values username password))))
