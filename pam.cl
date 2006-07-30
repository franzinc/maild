;; $Id: pam.cl,v 1.1 2006/07/30 17:35:04 dancy Exp $

(defpackage :util.pam
  (:use :lisp :excl)
  (:export
   #:with-pam
   #:pam-start
   #:pam-end
   #:pam-authenticate
   #:set-pam-fail-delay
   ))

(in-package :util.pam)

;; AIX has libpam, but not in shared library form.
#+(or windows tru64 aix)
(error "No PAM support on this platform")

#-(or hpux macosx)
(if* (probe-file "/lib/libpam.so.0")
   then (load "libpam.so.0" :foreign t)
   else (load "libpam.so"))
#+hpux
(load "libpam.sl")
#+macosx
(load "libpam.dylib")


;; return values. 
(defconstant *pam-success* 0)

#+(or linux freebsd4 macosx)
(eval-when (compile load eval)
(defconstant *pam-auth-err* 7)
(defconstant *pam-cred-insufficient* 8)
(defconstant *pam-authinfo-unavail* 9)
(defconstant *pam-user-unknown* 10)
(defconstant *pam-maxtries* 11)
(defconstant *pam-conv-err* 19)
)

#-(or linux freebsd4 macosx)
(eval-when (compile load eval)
(defconstant *pam-auth-err* 9)
(defconstant *pam-cred-insufficient* 11)
(defconstant *pam-authinfo-unavail* 12)
(defconstant *pam-user-unknown* 13)
(defconstant *pam-maxtries* 8)
(defconstant *pam-conv-err* 6)
)

;; item types
(defconstant *pam-service* 1)
(defconstant *pam-user* 2)
(defconstant *pam-tty* 3)
(defconstant *pam-rhost* 4)
(defconstant *pam-conv* 5)
(defconstant *pam-ruser* 8)
(defconstant *pam-user-prompt* 9)

;; message styles
(eval-when (compile load eval)
(defconstant *pam-prompt-echo-off* 1)
(defconstant *pam-prompt-echo-on* 2)
(defconstant *pam-error-msg* 3)
(defconstant *pam-text-info* 4)
)

(defclass pam ()
  ((handle :accessor handle :initarg :handle :initform nil)
   (conversation :accessor conversation :initarg :conversation)
   (data :accessor data :initarg :data :initform nil)
   (id :accessor id)
   (fail-delay :accessor fail-delay :initform (* 2 1000 1000))
   (user :accessor user :initform nil)
   (password :accessor password :initform nil)))

(defstruct pam-message
  style
  message)

(defstruct pam-response
  response
  (code 0))
  

(ff:def-foreign-type pam-handle-t (* :void))

#|
struct pam_conv {
    int (*conv)(int num_msg, const struct pam_message **msg,
		struct pam_response **resp, void *appdata_ptr);
    void *appdata_ptr;
};
|#

(ff:def-foreign-type pam-conv
    (:struct
     (conv (* :void))
     (appdata (* :void))))

(ff:def-foreign-type pam-msg
    (:struct
     (style :int)
     (msg (* :char))))

(ff:def-foreign-type pam-resp
    (:struct
     (resp (* :char))
     (retcode :int))) ;; currently unused.  zero expected

;; functions

(ff:def-foreign-call (pam_start "pam_start")
    ((service-name (* :char))
     (user (* :char))
     (conversation (* :void))
     (handle-ptr (* pam-handle-t)))
  :strings-convert t)


(ff:def-foreign-call (pam_end "pam_end")
    ((handle pam-handle-t)
     (status :int)))

(ff:def-foreign-call (pam_set_item "pam_set_item")
    ((handle pam-handle-t)
     (type :int)
     (item (* :void)))
  :strings-convert t)


(ff:def-foreign-call (pam_get_item "pam_get_item")
    ((handle pam-handle-t)
     (type :int)
     (item (* (* :void)))))

(ff:def-foreign-call (pam_strerror "pam_strerror")
    ((handle pam-handle-t)
     (errnum :int))
  :returning ((* :char)))

#+linux
(ff:def-foreign-call (pam_fail_delay "pam_fail_delay")
    ((handle pam-handle-t)
     (microsec :unsigned-int)))

(ff:def-foreign-call (pam_authenticate "pam_authenticate")
    ((handle pam-handle-t)
     (flags :int)))

(ff:def-foreign-call (pam_setcred "pam_setcred")
    ((handle pam-handle-t)
     (flags :int)))

(ff:def-foreign-call (pam_acct_mgmt "pam_acct_mgmt")
    ((handle pam-handle-t)
     (flags :int)))

(ff:def-foreign-call (pam_chauthtok "pam_chauthtok")
    ((handle pam-handle-t)
     (flags :int)))

(ff:def-foreign-call (pam_open_session "pam_open_session")
    ((handle pam-handle-t)
     (flags :int)))

(ff:def-foreign-call (pam_close_session "pam_close_session")
    ((handle pam-handle-t)
     (flags :int)))

(ff:def-foreign-call (pam_putenv "pam_putenv")
    ((handle pam-handle-t)
     (string (* :char)))
  :strings-convert t)

(ff:def-foreign-call (pam_getenv "pam_getenv")
    ((handle pam-handle-t)
     (name (* :char)))
  :strings-convert t
  :returning ((* :char)))

;; Returned memory must be free()'d.
;; Returns a NULL-terminated array of character pointers.
(ff:def-foreign-call (pam_getenvlist "pam_getenvlist")
    ((handle pam-handle-t))
  :returning ((* (* :char))))

;;;;;;;;;;;;;;;;;;;;;

#+ignore
(ff:defun-foreign-callable pam-delay-callback ((status :int)
					       (microsecs :unsigned-int)
					       (appdata-ptr (* :void)))
  (declare (:unwind 0)
	   (ignore appdata-ptr))

  (when (/= status *pam-success*)
    ;; convert to seconds and sleep
    (sleep (/ microsecs 1000000))))
     
;; responses is free()'d by the PAM library, so we need to allocate it
;; using malloc().
(ff:defun-foreign-callable pam-conversation-callback 
    ((msg-count :int)
     (msgs (* :void))
     (resps (* :void))
     (id :int))
  (declare (:unwind *pam-conv-err*))

  (let ((msgs-ftype '(:array (* pam-msg) 1))
	(pam (locate-pam-object id))
	style messages)
    
    (dotimes (n msg-count)
      (setf style (ff:fslot-value-typed msgs-ftype :c msgs n '* 'style))
      ;; convert style to something lisp-friendly if possible.
      (setf style (case style 
		    (#.*pam-prompt-echo-off* :prompt-echo-off)
		    (#.*pam-prompt-echo-on* :prompt-echo-on)
		    (#.*pam-error-msg* :error)
		    (#.*pam-text-info* :text)
		    (t style)))
      
      (push
       (make-pam-message 
	:style style
	:message (native-to-string
		  (ff:fslot-value-typed msgs-ftype :c msgs n '* 'msg)))
       messages))
    
    (setf messages (nreverse messages))

    (let ((responses (funcall (conversation pam) messages (data pam) 
			      :password (password pam))))
      (if* (null responses)
	 then *pam-conv-err*
       elseif (/= (length responses) msg-count)
	 then (error "pam conversation function ~a returned the wrong number of responses (wanted: ~d, got: ~d"
		     (conversation pam) msg-count (length responses))
	 else (setf (ff:fslot-value-typed '(* :void) :c resps)
		(pam-convert-responses responses))
	      *pam-success*))))


(ff:def-foreign-call (strdup "strdup") ((string (* :char)))
  :strings-convert t
  :returning :foreign-address)

(ff:def-foreign-call (calloc "calloc") ((num :unsigned-int) 
					(size :unsigned-int))
  :returning :foreign-address)

(defun pam-convert-responses (responses)
  (let ((buf (calloc (length responses) (ff:sizeof-fobject 'pam-resp)))
	(n 0)
	(resp-ftype '(:array pam-resp 1)))
    (if* (zerop buf)
       then (error "Memory allocation error")
       else (dolist (response responses)
	      (setf (ff:fslot-value-typed resp-ftype :c buf n 'resp)
		(strdup (if* (pam-response-p response)
			   then (pam-response-response response)
			   else response)))
	      (setf (ff:fslot-value-typed resp-ftype :c buf n 'retcode)
		(if* (pam-response-p response)
		   then (pam-response-code response)
		   else 0))
	      (incf n))
	    buf)))

(defparameter *pam-conversation-callback-addr* nil)
#+ignore
(defparameter *pam-delay-callback-addr* nil)

(defun initialize-pam-conversation-callback ()
  (setf *pam-conversation-callback-addr*
    (ff:register-foreign-callable 'pam-conversation-callback :reuse t))
  #+ignore
  (setf *pam-delay-callback-addr*
    (ff:register-foreign-callable 'pam-delay-callback :reuse t)))

  
(if* (null *pam-conversation-callback-addr*)
   then (initialize-pam-conversation-callback)
	(pushnew #'initialize-pam-conversation-callback 
		 excl::*system-restart-actions*))

(defmacro with-success (form &body body)
  (let* ((res (gensym))
	 (funcname (first form))
	 (pam (second form))
	 (handle (gensym)))
    (setf form (cddr form))
    `(let* ((,handle (handle ,pam))
	    (,res (,funcname ,handle ,@form)))
       (if* (= ,res *pam-success*)
	  then ,@body
	  else (error "~a failed: ~a"
		      (quote ,funcname)
		      (pam_strerror ,handle ,res))))))

(defparameter *pam-objects* nil)
(defparameter *pam-objects-lock* (mp:make-process-lock))

(defun register-pam-object (pam)
  (mp:with-process-lock (*pam-objects-lock*)
    (let ((pos (position nil *pam-objects*)))
      (if* pos
	 then (setf (nth pos *pam-objects*) pam)
	      pos
	 else (setf *pam-objects* (nconc *pam-objects* (list pam)))
	      (1- (length *pam-objects*))))))

(defun deregister-pam-object (pam)
  (mp:with-process-lock (*pam-objects-lock*)
    (let ((pos (position pam *pam-objects*)))
      (if* (null pos)
	 then (error "deregister-pam-object: object not found.  This should never happen!"))
      (setf (nth pos *pam-objects*) nil))))

(defun locate-pam-object (id)
  (mp:with-process-lock (*pam-objects-lock*)
    (find id *pam-objects* :key #'id)))

(defmacro with-pam ((var &rest args) &body body)
  `(let ((,var (pam-start ,@args)))
     (unwind-protect
	 (progn ,@body)
       (pam-end ,var))))

(defun pam-start (service-name user 
		  &key (conversation 'pam-default-conversation)
		       data)
  (let* ((ftype '(:struct (ptr pam-handle-t)))
	 (handle-holder (ff:allocate-fobject ftype :foreign-static-gc))
	 (conv (ff:allocate-fobject 'pam-conv :foreign-static-gc))
	 (pam (make-instance 'pam 
		:conversation conversation :data data))
	 (id (register-pam-object pam))
	 res)

    (setf (id pam) id)
    
    (setf (ff:fslot-value-typed 'pam-conv :foreign-static-gc conv 'conv)
      *pam-conversation-callback-addr*)
    (setf (ff:fslot-value-typed 'pam-conv :foreign-static-gc conv 'appdata)
      id)

    (if* (null user)
       then (setf user 0)
       else (setf (user pam) user))
    
    ;; pam_start copies the information out of 'conv'.
    (setf res (pam_start service-name user conv handle-holder))
    (if* (= res *pam-success*)
       then (setf (handle pam)
	      (ff:fslot-value-typed ftype :foreign-static-gc 
				    handle-holder 'ptr))
	    pam
       else (deregister-pam-object pam)
	    (error "pam_start failed: ~a" (pam_strerror 0 res)))))

(defmethod pam-end ((pam pam) &optional (status *pam-success*))
  (let ((handle (handle pam)))
    (when handle
      (with-success (pam_end pam status)
	(setf (handle pam) nil)
	(deregister-pam-object pam)
	t))))

(defmethod pam-authenticate ((pam pam) &key (flags 0) password)
  (if password
      (setf (password pam) password))
  
  (let* ((handle (handle pam))
	 (res (pam_authenticate handle flags)))
    (if* (= res *pam-success*)
       then t
       else ;; fail delay
	    (sleep (/ (fail-delay pam) (* 1000 1000)))
	    (values nil 
		    (case res
		      (#.*pam-auth-err* :auth-err)
		      (#.*pam-cred-insufficient* :cred-insufficient)
		      (#.*pam-authinfo-unavail* :authinto-unavail)
		      (#.*pam-user-unknown* :user-unknown)
		      (#.*pam-maxtries* :max-tries)
		      (t res))))))

;; Currently the style is ignored since we don't have any
;; API for turning on/off echo.
(defun pam-default-conversation (messages data &key password)
  (declare (ignore data))
  (let (responses)
    (dolist (message messages)
      (if* (and password (eq (pam-message-style message) :prompt-echo-off))
	 then (push password responses)
	 else (write-string (pam-message-message message))
	      (finish-output)
	      (push (read-line) responses)))
    (nreverse responses)))

(defmethod set-pam-fail-delay ((pam pam) microseconds)
  ;; On linux, override the default delay since we'll handle
  ;; it in lisp.
  #+linux
  (with-success (pam_fail_delay pam 0)
    t)
  (setf (fail-delay pam) microseconds))

;;;; TESTING. 

(defun test (&optional user password)
  (with-pam (pam "login" user)
    (set-pam-fail-delay pam (* 1 1000 1000))
    (multiple-value-bind (success code)
	(pam-authenticate pam :password password)
      (if* success
	 then t
	 else code))))

(defun test-constants-convo (messages data &key password)
  (declare (ignore password))
  (let (resps)
    (dolist (message messages)
      (ecase (pam-message-style message)
	(:prompt-echo-on ;; roughly translates to "user"
	 (ecase data
	   (0 ;; test unknown user
	    (push "asdfsadf" resps))
	   (1 ;; test known user
	    (push "dancy" resps))))
	(:prompt-echo-off ;; roughly translates to "password"
	 (push "testing" resps))))
    (nreverse resps)))

(defun test-constants ()
  (with-pam (pam "login" nil :data 0 :conversation 'test-constants-convo)
    (set-pam-fail-delay pam 0)
    (multiple-value-bind (success code)
	(pam-authenticate pam)
      (if success
	  (error "unexpected success when testing unknown user"))
      (if* (eq code :auth-err)
	 then (format t "This platform returns :auth-err for unknown users~%")
       elseif (not (eq code :user-unknown))
	 then (error "Unexpected code ~s when testing unknown user" code))))
  (with-pam (pam "login" nil :data 1 :conversation 'test-constants-convo)
    (set-pam-fail-delay pam 0)
    (multiple-value-bind (success code)
	(pam-authenticate pam)
      (if success
	  (error "unexpected success when testing known user"))
      (if (not (eq code :auth-err))
	  (error "Unexpected code ~s when testing known user" code)))))
