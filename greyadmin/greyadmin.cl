(in-package :user)

(defparameter *configfile* "/etc/greyadmin.cl")

(defparameter *port* 2526)
;; need root to use shadow passwords
(defparameter *run-as* "root")
(defparameter *domain* "unconfigured.domain")
;; trailing slash is required
(defparameter *libdir* "/usr/local/lib/greyadmin/")

(defparameter *greylist-db-host* "localhost")
(defparameter *greylist-db-name* "greylist")
(defparameter *greylist-db-user* "greylist")
(defparameter *greylist-db-password* "password-not-set-yet")
(defparameter *greylist-operation-mode* :opt-out)

(defparameter *greylist-db* nil)

(defparameter *greylist-lock* nil)


(eval-when (compile load eval)
  (require :aserve)
  (require :webactions)
  (use-package :net.aserve)
  (use-package :net.html.generator)
  (require :osi)
  (use-package :excl.osi)
  (require :mysql)
  (use-package :dbi.mysql))

(defun main (&rest args)
  (declare (ignore args))
  (init-webserver)
  (let ((pid (fork)))
    (cond
     ((not (numberp pid))
      (error "fork failed"))
     ((= pid 0)
      ;; child
      (detach-from-terminal)
      (loop (sleep 86400)))
     (t
      ;; parent
      (exit 0 :quiet t)))))


(defun init-webserver ()
  ;; config file is required
  (load *configfile* :verbose nil)
  
  (let ((pwent (getpwnam *run-as*)))
    (if (null pwent)
	(error "User ~A doesn't exist" *run-as*))
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
			      ("menu" "menu.clp")
			      ("update" update)))))

;; no output is generated unless an authenticated session
;; is in use or public argument is supplied.
(def-clp-function ga_with-wrapper (req ent args body) 
  (let ((sess (websession-from-req req)))
    (if (or (assoc "public" args :test #'equal)
	    (websession-variable sess "user"))
	(html
	 (:html
	  (:head (:title "Greylist administration"))
	  (:body
	   (:h3 "Greylist administration")
	   (emit-clp-entity req ent body)))))))

(defun check-password (user pw)
  (let ((ent (getpwnam user)))
    (when ent
      (or (string= (crypt pw (pwent-passwd ent))
		   (pwent-passwd ent))
	  (check-shadow-password user pw)))))
      

(defun check-shadow-password (user pw)
  (when (shadow-passwd-supported-p)
    (let ((ent (getspnam user)))
      (when ent
	(string= (crypt pw (spwd-passwd ent)) 
		 (spwd-passwd ent))))))
  

(defun check-login (req ent)
  (declare (ignore ent))
  (block nil
    (let* ((login (request-query-value "login" req))
	   (pw (request-query-value "pw" req))
	   (sess (websession-from-req req)))
      (when (not (check-password login pw))
	(setf (websession-variable sess "error") "Invalid login")
	(return "login"))
      (setf (websession-variable sess "user") login)
      (setf (websession-variable sess "greylisting")
	(determine-greylist-status (user-address login)))
      "menu")))

(defun user-address (user)
  (format nil "~A@~A" user *domain*))

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
  (declare (ignore ent args body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user)))
    (html 
     (:princ (num-blocked-triples addr)))))

(def-clp-function ga_num-passed-triples (req ent args body) 
  (declare (ignore ent args body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user)))
    (html 
     (:princ (num-passed-triples addr)))))

(def-clp-function ga_num-suspected-spams (req ent args body) 
  (declare (ignore ent args body))
  (let* ((sess (websession-from-req req))
	 (user (websession-variable sess "user"))
	 (addr (user-address user)))
    (html 
     (:princ (num-suspected-spams addr)))))
      


  



;; from greylist.cl (with some mods)

(defun ensure-greylist-db ()
  (without-interrupts
    (when (null *greylist-lock*)
      (setf *greylist-lock* 
	(mp:make-process-lock :name "Greylist database lock"))))

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





(defmacro greysql (&rest rest)
  ;; gah!
  `(mp:with-process-lock (*greylist-lock*)
     (sql ,@rest :db *greylist-db*)))

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

(defun num-blocked-triples (addr)
  (caar 
   (greysql 
    (format nil "select sum(blocked) from triples where receiver=~S"
	    (mysql-escape-sequence addr)))))

(defun num-passed-triples (addr)
  (caar 
   (greysql 
    (format nil "select sum(passed) from triples where receiver=~S"
	    (mysql-escape-sequence addr)))))

(defun num-suspected-spams (addr)
  (caar 
   (greysql 
    (format nil "select count(*) from triples where receiver=~S and passed=0"
	    (mysql-escape-sequence addr)))))

;;;;;;;

(defun build ()
  (compile-file-if-needed "greyadmin.cl")
  (generate-executable "greyadmin" '("greyadmin.fasl")))
