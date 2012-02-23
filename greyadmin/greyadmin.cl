(in-package :user)

(defparameter *configfile* "/etc/greyadmin.cl")

(defparameter *port* 2526)
(defparameter *run-as* "root")
(defparameter *domain* "unconfigured.domain")
(defparameter *pop-auth-server* nil)
;; trailing slash is required
(defparameter *libdir* "/usr/lib/greyadmin/")
;; super user
(defparameter *admin-user* nil)
(defparameter *admin-password* nil)

(defparameter *greylist-db-host* "localhost")
(defparameter *greylist-db-name* "greylist")
(defparameter *greylist-db-user* "greylist")
(defparameter *greylist-db-password* "password-not-set-yet")
(defparameter *greylist-operation-mode* :opt-out)
(defparameter *greylist-lifetime* (* 60 86400))

(defparameter *greylist-db* nil)

(defvar *greylist-lock*
  ;;mm 2012-02 We use defvar because this lock should only be created once.
  (mp:make-process-lock :name "Greylist database lock"))


(eval-when (compile load eval)
  (require :aserve)
  (require :webactions)
  (use-package :net.aserve)
  (use-package :net.html.generator)
  (require :osi)
  (use-package :excl.osi)
  (require :mysql)
  (use-package :dbi.mysql)
  (require :socket)
  (require :pam))


(defmacro greysql (&rest rest)
  ;; gah!
  `(mp:with-process-lock (*greylist-lock*)
     (sql ,@rest :db *greylist-db*)))



(defun debug-main ()
  (main "greyadmin" "-d"))

(defun main (&rest args)
  (pop args)
  (with-command-line-arguments 
      (("d" :short debug nil))
    (rest :command-line-arguments args)
    (declare (ignore rest))

    ;; config file is required
    (load *configfile* :verbose debug)
    
    (when debug
      (setf *libdir* "")
      (setf *port* 2527))
    
    (init-webserver)

    (let ((pid (if debug 0 (fork))))
      (cond
       ((not (numberp pid))
	(error "fork failed"))
       ((= pid 0)
	;; child
	(when (not debug)
	  (detach-from-terminal))
	(loop (sleep 86400)))
       (t
	;; parent
	(exit 0 :quiet t))))))

(defun init-webserver ()
  (let ((pwent (getpwnam *run-as*)))
    (if (null pwent)
	(error "User ~A doesn't exist" *run-as*))
    
    (greyadmin-log "starting webserver on port ~s" *port*)
    
    (start :port *port* 
	   :setuid (pwent-uid pwent)
	   :setgid (pwent-gid pwent))
    
    ;; so that db connection errors will be noticed.
    (ensure-greylist-db)
    
    (webaction-project "greylistadmin"
		       :project-prefix "/"
		       :destination *libdir*
		       :index "login"
		       :map '(("login" "login.clp")
			      ("checklogin" check-login)
			      ("super" "super.clp" (:redirect t))
			      ("impersonate" impersonate)
			      ("menu" "menu.clp" (:redirect t))
			      ("update" update)
			      ("whitelist" quick-whitelist-triple)
			      ("modify-personal-whitelist" modify-personal-whitelist)))))

;; no output is generated unless an authenticated session
;; is in use or public argument is supplied.
(def-clp-function ga_with-wrapper (req ent args body) 
  (let ((sess (websession-from-req req)))
    (if (or 
	 ;; public area
	 (assoc "public" args :test #'equal) 
	 ;; superuser can get anywhere
	 (websession-variable sess "super")
	 ;; authenticated user in non-superonly area.
	 (and (null (assoc "superonly" args :test #'equal)) 
	      (websession-variable sess "user")))
	(html
	 (:html
	  (:head 
	   (:title "Greylist administration"))
	  ((:body :if* (assoc "login" args :test #'equal) :onload "document.loginform.login.focus()")
	   (:h3 "Greylist administration")
	   (emit-clp-entity req ent body)))))))

(defmacro with-socket ((var &key remote-host remote-port) &body body)
  `(let (,var)
     (unwind-protect
	 (progn
	   (setf ,var (socket:make-socket :remote-host ,remote-host
					  :remote-port ,remote-port))
	   ,@body)
       (if ,var
	   (close ,var :abort t)))))

(defun pop-response-ok (line)
  (and line
       (>= (length line) 3)
       (string= (subseq line 0 3) "+OK")))

(defun pop-send-line (string stream)
  (write-string string stream)
  (write-char #\return stream)
  (write-char #\newline stream)
  (finish-output stream))
	      
(defun check-pop-password (user pw)
  (block nil
    (when *pop-auth-server*
      (with-socket (sock :remote-host *pop-auth-server* :remote-port 110)
	(let (res)
	  (if (not (pop-response-ok (read-line sock)))
	      (return))
	  (pop-send-line (format nil "USER ~A" user) sock)
	  (if (not (pop-response-ok (read-line sock)))
	      (return))
	  (pop-send-line (format nil "PASS ~A" pw) sock)
	  (setf res (pop-response-ok (read-line sock)))
	  ;; try to be polite
	  (pop-send-line "QUIT" sock)
	  (read-line sock)
	  res)))))

(defun check-password (user pw)
  (util.pam:with-pam (pam "login" user)
    (if* (util.pam:pam-authenticate pam :password pw)
       then t
       else (check-pop-password user pw))))


(defun check-login (req ent)
  (declare (ignore ent))
  (block nil
    (let* ((login (request-query-value "login" req))
	   (pw (request-query-value "pw" req))
	   (sess (websession-from-req req)))

      ;; Check for super user access
      (when (and *admin-user* *admin-password* 
		 (string= login *admin-user*)
		 (string= pw *admin-password*))
	(setf (websession-variable sess "error") nil)
	(setf (websession-variable sess "super") t)
	(return "super"))
      (when (or (null login) (null pw) (not (check-password login pw)))
	(setf (websession-variable sess "error") "Invalid login")
	(return "login"))
      (setf (websession-variable sess "error") nil)
      (setf (websession-variable sess "user") login)
      (setf (websession-variable sess "greylisting")
	(determine-greylist-status (user-address login)))
      "menu")))

(defun impersonate (req ent)
  (declare (ignore ent))
  (block nil
    (let* ((login (request-query-value "login" req))
	   (sess (websession-from-req req)))
      (if (null (websession-variable sess "super"))
	  (return "login"))
      (setf (websession-variable sess "error") nil)
      (setf (websession-variable sess "user") login)
      (setf (websession-variable sess "greylisting")
	(determine-greylist-status (user-address login)))
      "menu")))
  

(defun user-address (user)
  (if (find #\@ user)
      user
    (format nil "~A@~A" user *domain*)))

(def-clp-function ga_user-address (req ent args body) 
  (declare (ignore ent args body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user")))
    (html
     (:princ-safe (user-address user)))))

;; t means on, nil means off
(defun determine-greylist-status (addr)
  (ecase *greylist-operation-mode*
    (:opt-out
     ;; if user is found in the optout table, then greylisting
     ;; is off.
     (if (greylist-recipient-excluded-p addr)
	 nil
       t))
    (:opt-in
     ;; if user is found in the optin table, then greylisting
     ;; is on.
     (if (greylist-recipient-included-p addr)
	 t
       nil))))

(defun update (req ent)
  (declare (ignore ent))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (greylisting (request-query-value "greylisting" req))
	 (addr (user-address user)))

    (if (null user)
	(return-from update "login"))
    
    (cond
     ((string= greylisting "on")
      (turn-on-greylisting addr))
     ((string= greylisting "off")
      (turn-off-greylisting addr)))
    
    (setf (websession-variable sess "greylisting")
      (determine-greylist-status addr))
    
    (setf (websession-variable sess "error")
      "Settings updated")
    
    "menu"))

(def-clp-function ga_num-blocked-triples (req ent args body) 
  (declare (ignore ent body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (if (assoc "user" args :test #'equal)
		   (user-address user))))
    (html 
     (:princ (num-blocked-triples :addr addr)))))

(def-clp-function ga_num-passed-triples (req ent args body) 
  (declare (ignore ent body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (if (assoc "user" args :test #'equal)
		   (user-address user))))
    (html 
     (:princ (num-passed-triples :addr addr)))))

(def-clp-function ga_num-suspected-spams (req ent args body) 
  (declare (ignore ent body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (if (assoc "user" args :test #'equal)
		   (user-address user))))
    (html 
     (:princ (num-suspected-spams :addr addr)))))

(defun make-url (base alist)
  (concatenate 'string base "?" (query-to-form-urlencoded alist)))

(def-clp-function ga_list-delayed-triples (req ent args body)
  (declare (ignore args body))
  (let* ((wa (webaction-from-ent ent))
	 (sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user))
	 (entries (get-delayed-triples addr)))
    (when entries
      (html
       (:b "Deliveries currently being delayed for "
	   (:princ-safe addr)) 
       :br :br
       ((:table :border 1)
	(:tr (:td (:b "Sending mail server")) 
	     (:td (:b "Sender (click to allow through on next delivery attempt)"))
	     (:td (:b "Unblocks at")))
	(dolist (thing entries)
	  (html
	   (:tr
	    (:td (:princ (socket:ipaddr-to-dotted (first thing))))
	    (:td 
	     ((:a :href (make-url (locate-action-path wa "whitelist" sess)
				  `(("ip" . ,(first thing))
				    ("sender" . ,(second thing)))))
	      (:princ-safe (second thing))))
	    (:td (:princ-safe (ctime (third thing))))))))))))

(def-clp-function ga_list-unused-autowhitelisted (req ent args body)
  (declare (ignore args body ent))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user))
	 (entries (get-unused-autowhitelisted addr)))
    (when entries
      (html
       :br
       (:b "Auto-whitelisted but never-subsequently-received deliveries for "
	   (:princ-safe addr)) 
       :br :br
       ((:table :border 1)
	(:tr (:td (:b "Sending mail server")) 
	     (:td (:b "Sender"))
	     (:td (:b "Blocked attempts")))
	(dolist (thing entries)
	  (html
	   (:tr
	    (:td (:princ (socket:ipaddr-to-dotted (first thing))))
	    (:td (:princ-safe (second thing)))
	    (:td (:princ (third thing)))))))))))



(defun quick-whitelist-triple (req ent)
  (declare (ignore ent))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user))
	 (sender (request-query-value "sender" req)))
    (greysql
     (format nil "update triples set blockexpire=0 where receiver=~S and sender=~S"
	     (mysql-escape-sequence addr)
	     (mysql-escape-sequence sender)))
    "menu"))

(def-clp-function ga_lifetime-days (req ent args body)
  (declare (ignore args body req ent))
  (html (:princ (ceiling (/ *greylist-lifetime* 86400.0)))))

(def-clp-function ga_dump-personal-whitelist (req ent args body)
  (declare (ignore ent args body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user))
	 (entries (mapcar #'car (greysql (format nil "select sender from whitelist where receiver=~S order by sender" (mysql-escape-sequence addr))))))
    (if (null entries)
	(setf entries '("<no entries>")))
    (html 
     ((:form :action "modify-personal-whitelist" :method :post)
      ((:table :border 0)
       (:tr
	(:td
	 ((:select :name "senders" :multiple t :size 10)
	  (dolist (entry entries)
	    (html (:option (:princ-safe entry)))))
	 :br
	 ((:input :type :submit :value "Remove selected" :name "remove")))
	((:td :valign :center)
	 "Sender address to add:" :br
	 ((:input :type :edit :name "sender" :size 50))
	 :br
	 ((:input :type :submit :name "add" :value "<< Add")))))))))

(defun multiple-request-query-value (key req)
  (mapcar #'cdr
	  (remove-if-not (lambda (thing) (equal (car thing) key)) 
			 (request-query req))))
      
    

(defun modify-personal-whitelist (req ent)
  (declare (ignore ent))
  (block nil
    (let* ((sess (websession-from-req req))
	   (user (websession-variable sess "user"))
	   (addr (user-address user))
	   (remove (request-query-value "remove" req))
	   (sender (request-query-value "sender" req))
	   (senders (multiple-request-query-value "senders" req)))
      
      (if (null user)
	  (return "login"))
      
      (when remove
	(when (null senders)
	  (setf (websession-variable sess "error")
	    "You must select addresses to remove from the list")
	  (return "menu"))
	
	(dolist (sender senders)
	  (greysql
	   (format nil
		   "delete from whitelist where receiver=~S and sender=~S"
		   (mysql-escape-sequence addr)
		   (mysql-escape-sequence sender))))
	
	(setf (websession-variable sess "error")
	  "Personal whitelist updated.")
	(return "menu"))
      
      ;; assume that an add is desired..  The user might have submitted
      ;; the form by hitting the enter key in their browser, in which
      ;; case the 'add' submit button will not have been triggered..
      ;; so we don't explicitly check for it.
      
      (setf sender (string-trim '(#\space) sender))
      (when (string= sender "")
	(setf (websession-variable sess "error")
	  "You must specify the sender email address which you want to add")
	(return "menu"))
      
      (greysql 
       (format nil 
	       "insert into whitelist (sender, receiver, source) values (~S, ~S, 'user')" 
	       (mysql-escape-sequence sender) 
	       (mysql-escape-sequence addr)))
      
      (setf (websession-variable sess "error")
	"Personal whitelist updated.")
      
      "menu")))


;; db stuff

(defun ensure-greylist-db ()

  (mp:with-process-lock (*greylist-lock*)

    ;; See if we have a broken connection.
    (when (and *greylist-db* 
	       (not (mysql-connected *greylist-db*)))
      (disconnect :db *greylist-db*)
      (setf *greylist-db* nil))
    
    (when (null *greylist-db*)
      (setf *greylist-db* 
	(connect :database *greylist-db-name*
			   :host *greylist-db-host*
			   :user *greylist-db-user*
			   :password *greylist-db-password*)))))


(defun greylist-recipient-excluded-p (to)
  (ensure-greylist-db)
  (if (greysql 
       (format nil "select receiver from optout where receiver=~S"
	       (mysql-escape-sequence to)))
      t
    nil))

(defun greylist-recipient-included-p (to)
  (ensure-greylist-db)
  (if (greysql 
       (format nil "select receiver from optin where receiver=~S"
	       (mysql-escape-sequence to)))
      t
    nil))

(defun turn-on-greylisting (addr)
  (ensure-greylist-db)
  (ecase *greylist-operation-mode*
    (:opt-out
     (greysql 
      (format nil "delete from optout where receiver=~S"
	      (mysql-escape-sequence addr))))
    (:opt-in
     (greysql 
      (format nil "insert into optin set receiver=~S"
	      (mysql-escape-sequence addr))))))

(defun turn-off-greylisting (addr)
  (ensure-greylist-db)
  (ecase *greylist-operation-mode*
    (:opt-out
     (greysql 
      (format nil "insert into optout set receiver=~S"
	      (mysql-escape-sequence addr))))
    (:opt-in     
     (greysql 
      (format nil "delete from optin where receiver=~S"
	      (mysql-escape-sequence addr))))))

(defun num-blocked-triples (&key addr)
  (caar 
   (greysql 
    (with-output-to-string (s)
      (write-string "select sum(blocked) from triples" s)
      (if addr
	  (format s " where receiver=~S" (mysql-escape-sequence addr)))))))

(defun num-passed-triples (&key addr)
  (caar 
   (greysql 
    (with-output-to-string (s)
      (write-string "select sum(passed) from triples" s)
      (if addr
	  (format s " where receiver=~S" (mysql-escape-sequence addr)))))))

(defun num-suspected-spams (&key addr)
  (caar 
   (greysql 
    (with-output-to-string (s)
      (write-string "select count(*) from triples where passed=0" s)
      (if addr
	  (format s " and receiver=~S" (mysql-escape-sequence addr)))))))

;; Return triples that are currently in the blocking period.
(defun get-delayed-triples (addr)
  (let ((now (get-universal-time)))
    (greysql 
     (format nil "select ip, sender, blockexpire from triples where receiver=~S and blockexpire>~D and expire>~D and passed=0" 
	     (mysql-escape-sequence addr)
	     now now))))

;; Return triples that are not in the blocking period but which haven't
;; passed any messages
(defun get-unused-autowhitelisted (addr)
  (let ((now (get-universal-time)))
    (greysql
     (format nil "select ip, sender, blocked from triples where receiver=~S and blockexpire<~D and expire>~D and passed=0"
	     (mysql-escape-sequence addr)
	     now now))))

;;;;;;;
;; debugging

(defparameter *log-opened* nil)

(defun greyadmin-log (format-string &rest format-args)
  (when (null *log-opened*)
    (openlog "greyadmin" *log-pid* *log-mail*)
    (setf *log-opened* t))
  
  (syslog (logior *log-mail* *log-info*)
	  (handler-case
	      (apply #'format nil format-string format-args)
	    (error (c)
	      (format nil "error in format string ~s: ~a" format-string c)))))

;;;;;;;

;;;;TODO: for 9.0 turn all the without-* calls into locks.
#+(version= 8 2)
(setq excl::*warn-smp-usage* nil)

(defun build ()
  (handler-case (compile-file-if-needed "greyadmin.cl")
    (warning (c)
      (error c)))
  (generate-executable "greyadmin" '("greyadmin.fasl" :locale)))
