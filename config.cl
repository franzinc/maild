(in-package :user)

(defparameter *debug* t)

(defparameter *aliasesfile* "/etc/aliases")
(defparameter *virtusersfile* "/etc/mail/virtusers")

;; If this is nil, it is determined by calling (gethostname)
(defparameter *short-host-name* nil)

;; If this is nil, the fully qualified domain name of this
;; host will be determined automatically, if possible.
(defparameter *fqdn* nil)

;; Other names by which this host is known (Besides *fqdn* and
;; *short-host-name*).
(defparameter *host-aliases* nil)

;; Domains for which we receive mail. Add your domain name here.
;; *short-host-name*, *fqdn*, and *host-aliases* are included
;; implicitly so you don't need to add them.
(defparameter *localdomains* '("franz.com"))

;; List of IP addresses or networks.
(defparameter *relay-access* '("127.0.0.1" "192.132.95.0/24"))

;; Can be a list of IP addresses and masks.. or domain names.
;; If a domain like example.com is supplied, then connections
;; from mailhost.example.com will be blacklisted as well.  Comparing
;; domains will require reverse DNS mapping enforcement to be turned
;; on to work properly.
(defparameter *blacklist-connections* nil)

;; This is the message that is sent immediately after connection from
;; a blacklisted client.  
(defparameter *blacklisted-response* "We do not accept mail from you")


;; Addresses to reject during the MAIL FROM: transaction
(defparameter *blacklist-from* '("big@boss.com"))

;; If non-nil, then envelope senders in the SMTP session must have
;; a domain name part.
(defparameter *sender-domain-required* nil)

;; These functions will be called in order until one of them
;; returns true.  They are used to determine if a potential 
;; local recipient address should be accepted.  Order matters since
;; this is also used to check for recipients which should generate
;; an error message.  Currenly, this can only be done in the aliases
;; file, so that check should happen first.
(defparameter *local-recip-checks*
    '(lookup-recip-in-aliases lookup-recip-in-passwd))

(defparameter *queuedir* "/var/spool/maild")

;; Users who can use the -f command line argument without generating
;; an X-Authentication-Warning header in the outgoing message.
(defparameter *trusted-users* '("root" "daemon"))

;; SMTP-outgoing unqualified addresses are augmented with this 
;; domain part.
(defparameter *masquerade-as* "franz.com")

;; Local users for whom we do not use masquerading.
(defparameter *exposed-users* '("root"))

;; If set, during local delivery, any addresses with this string as
;; their domain part will have their domain part stripped.
(defparameter *strip-domain-for-local-delivery* "franz.com")

;; nil or 0 means no max.
(defparameter *maxmsgsize* nil) 

;; Bounce undeliverable messages after *bounce-days* days.
(defparameter *bounce-days* 5)

;; Maximum number of "Received" headers that may be found in a message
;; before we assume there's a mail loop and bounce the message.
(defparameter *maximum-hop-count* 17)

;; User to run local delivery programs as
(defparameter *local-delivery-user* "root")

;; User program aliases (e.g.,  majordomo: |/home/majordomo/wrapper..) run
;; as (by default.. can be overridden in the aliases file by specifying the
;; program with "|(user)/program/path"  syntax.
(defparameter *program-alias-user* "mailnull")


;; List of checkers to be called after a message body has been
;; received.  A checker entry is a list with two elements.  The first
;; element is a string which describes the checker.  The second
;; element is the checker function (or a symbol naming the function.
;; All listed checkers must return :ok before the message is accepted.
;; see checker.cl for more details.
(defparameter *message-data-checkers* 
    '(("Message size checker" message-size-checker)
      ("Hop count checker" hop-count-checker)))

;;;;;;;;
;;; Stuff that will need to be moved out to generalize the program.

(defun my-deliver-local-command (user queue)
  (list
   "/usr/local/sbin/deliver-via-spamc" 
   "-f"
   (emailaddr-orig (rewrite-local-envelope-sender (queue-from queue)))
   "-d"
   user))

(defparameter *deliver-local-command* 'my-deliver-local-command)

(defparameter *extra-headers-func* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only used if "smtp" entry can't be found in /etc/services and its 
;; associates.
(defparameter *smtp-port* 25) 
(defparameter *smtp-ip* nil) ;; address to bind socket
(defparameter *maxlinelen* 2048) ;; Max SMTP command line length.  including CR, but not LF
(defparameter *maxrecips* 100)
(defparameter *mailer-daemon* "MAILER-DAEMON")


;; Minimum recommended timeouts from RFC2821
;;
;; idle timeout for clients
(defparameter *cmdtimeout* (* 5 60)) 

;; read/write timeout for blocks of data during message headers/body
;; reception/transmission.  Also used as a write timeout for any
;; reponses sent to clients and other SMTP servers during delivery.
(defparameter *datatimeout* (* 3 60)) 

;; How long to wait for the SMTP connection greeting and the response
;; to the HELO command (when delivering mail via SMTP)
(defparameter *greetingtimeout* (* 5 60))

;; How long to wait for the response to a MAIL FROM: command (when 
;; delivering mail via SMTP)
(defparameter *mailcmdtimeout* (* 5 60))

;; How long to wait for the response to a RCPT TO: command (when delivering
;; mail via SMTP)
(defparameter *rcptcmdtimeout* (* 5 60))

;; How long to wait for the response to a DATA command (when delivering
;; mail via SMTP)
(defparameter *datainitiationtimeout* (* 2 60))

;; How long to wait for the response to the termination of a DATA command
;; (via "." on a blank line, when delivering mail via SMTP)
(defparameter *dataterminationtimeout* (* 10 60))

;; How long to wait for the response to a QUIT command (when delivering
;; mail via SMTP)
(defparameter *quittimeout* (* 2 60))

;; not really a maximum.. it's just the buffer size... but it must
;; be bigger than the largest anticipated header name.
(defparameter *maxdatalinelen* 2048)

