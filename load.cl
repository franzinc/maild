(in-package :user)

(defparameter *source-files*
    '(
      "utils" "security" "lex"
      "emailaddr" "log" "ipaddr" "blacklist" 
      "recips" "aliases" "headers" "rewrite" 
      "lock" "smtp" "queue" "mailer" "deliver" "deliver-smtp" 
      "queue-process" "checkers"
      "input"
      "smtp-server" "smtp-server-checkers" "greylist"
      "bounce" 
      "maild"))


(defun compile-sources (&key load)
  (let ((excl::*break-on-warnings* t))
    (with-compilation-unit ()
      (dolist (file *source-files*)
	(compile-file-if-needed (concatenate 'string file ".cl"))
	(if load (load (concatenate 'string file ".fasl")))))))
  
(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (use-package :socket))

(eval-when (load eval)
  (load "config.cl")
  (compile-sources :load t))

(defun build ()
  (compile-sources)
  (generate-executable 
   "maild" 
   (append '("config.cl" :acldns :locale)
	   (mapcar #'(lambda (f) 
		       (concatenate 'string f ".fasl"))
		   *source-files*)))
  (chmod "maild/maild" #o4555)) ;; setuid
