;; $Id: config.cl,v 1.38 2006/07/05 15:19:41 dancy Exp $

(in-package :user)

(defparameter *debug* nil)

(defparameter *ssl-support* nil)
;; Must be set if *ssl-support* is true
(defparameter *ssl-certificate-file* "/etc/maild.crt")
;; Can be nil if *ssl-certificate-file* contains the key
(defparameter *ssl-key-file* "/etc/maild.crt")

;; *client-authentiation*
;;  If nil, authentication is disabled.
;;  If :relay, then authentication is required before mail can be relayed
;;     through this server
;;  If :required, then authentication is required before any mail transaction.
;;     This is okay for mail hosts that are only used by clients for injecting
;;     mail.  But it's probably a bad idea for mailhosts that receive mail
;;     from other hosts (unless those other hosts have been configured with
;;     the proper authentication parameters).
(defparameter *client-authentication* nil)

;; Since, for best password database interoperability, maild only
;; supports cleartext auth mechanisms, it is a good idea to require the
;; use of SSL before allowing authentication to occur.  Needless to say,
;; If this is enabled, then *ssl-support* needs to be enabled as well.
(defparameter *client-auth-requires-ssl* t)
 
(defparameter *aliases-file* "/etc/aliases")

(defparameter *stats-file* "/var/state/maild")

;; If this is nil, the fully qualified domain name of this
;; host will be determined automatically, if possible.  *fqdn*
;; is what is used during the HELO transaction when delivering mail
;; via SMTP.
(defparameter *fqdn* nil)

;; Other names by which this host is known (Besides *fqdn*,
;; (gethostname), and *short-host-name*).  This is used in the default
;; rewriting rules (for masquerading).
(defparameter *host-aliases* '("localhost"))

;; Domains for which we receive mail. Add your domain name here.
;; (gethostname), *short-host-name*, *fqdn*, and *host-aliases* are
;; included implicitly so you don't need to add them.
(defparameter *localdomains* nil)

;; List of functions to call to determine if the client is allowed to
;; relay through this server.  Function is called with arguments:
;;  1) ip address of client 
;;  2) envelope sender
;;  3) envelope recipient
;; The function should return true (non-nil) if relaying is allowed,
;; otherwise nil.  The first function that returns true will
;; terminate the checking.
(defparameter *relay-checkers* '(check-relay-access))

;; List of IP addresses or networks.  This is used by the default
;; relay checker "check-relay-access"
(defparameter *relay-access* '("127.0.0.1"))


;; Addresses to reject during the MAIL FROM: transaction
(defparameter *blacklist-from* '("big@boss.com"))

;; Can be a list of IP addresses and masks.. or domain names.
;; If a domain like example.com is supplied, then connections
;; from mailhost.example.com will be blacklisted as well.  
(defparameter *blacklist-connections* nil)

;; This is the message that is sent immediately after connection from
;; a blacklisted client.  
(defparameter *blacklisted-response* "We do not accept mail from you")

;;;;; DNS-based blacklisting stuff

(defparameter *dns-blacklists* nil)
;; can be :transient or :permanent
(defparameter *dns-blacklisted-response-type* :transient)
;; List of recipients that are never subject to DNS blacklisting
(defparameter *dns-blacklist-recipient-exceptions* nil)
;; Message to give if connection is DNS blacklisted, when 
;; *dns-blacklisted-response-type* is :permanent.  The value should be a
;; string acceptable to `format', which takes one argument, a string which
;; was the host that blacklisted the connection.  A value of `nil' means
;; using the default of "Blacklisted by ~A".
(defparameter *dns-blacklist-failure-response* nil)
;; Similar to *dns-blacklist-failure-response*, except used when 
;; *dns-blacklisted-response-type* is :transient.  
(defparameter *dns-blacklist-temp-failure-response* nil)

;;;;;;



;; If non-nil, then envelope senders in the SMTP session must have
;; a domain name part.
(defparameter *sender-domain-required* nil)

;; If non-nil, then a reverse DNS lookup on the client's IP
;; address must succeed before mail is accepted.
(defparameter *reverse-dns-required* nil)

;; If the value is t, then the result of a DNS lookup on the name
;; specified in the HELO command must match the IP address of the connected
;; client.  If non-nil and not t, then just log connections that would be
;; rejected if the value of this variable were t.  Use of this option
;; is discouraged.  Plenty of legitimate sending hosts will not pass
;; this test.
(defparameter *helo-must-match-ip* nil)


(defparameter *queuedir* "/var/spool/maild")

;; Users who can use the -f command line argument without generating
;; an X-Authentication-Warning header in the outgoing message.
(defparameter *trusted-users* '("root" "daemon"))

;; SMTP-outgoing unqualified addresses are augmented with this 
;; domain part.
(defparameter *masquerade-as* nil)

;; Local users for whom we do not use masquerading.
(defparameter *exposed-users* '("root"))

;; If set, during local delivery, any addresses with this string as
;; their domain part will have their domain part stripped.
(defparameter *strip-domain-for-local-delivery* nil)

;; nil or 0 means no max.
(defparameter *maxmsgsize* nil) 

;; Bounce undeliverable messages after *bounce-days* days.
(defparameter *bounce-days* 5)

;; Maximum number of "Received" headers that may be found in a message
;; before we assume there's a mail loop and bounce the message.
(defparameter *maximum-hop-count* 17)

;; User program aliases (e.g.,  majordomo: |/home/majordomo/wrapper..) run
;; as (by default.. can be overridden in the aliases file by specifying the
;; program with "|(user)/program/path"  syntax.
(defparameter *program-alias-user* "mailnull")

;; This can be a string that is used to separate an email address from
;; its extension.  When an address is being looked up in the aliases
;; or via mailer checks, the full address is checked first.  If there
;; are no hits, then the address that up to (but not including)
;; *address-extension-delimiter* is checked.  For example, if
;; *address-extension-delimiter* is "+" and an email is addressed to
;; root+smith, then the aliases file will be scanned for root+smith
;; first.  If there is no root+smith alias, then the search
;; recommences for just "root". 
(defparameter *address-extension-delimiter* nil)


;; List of checkers to be called when the SMTP MAIL command has
;; been issued by the client.  Each entry should be in
;; ("checker name" checker-function-symbol)  form.

;; The functions are passed two values:  
;; The IP address of the client and the sender specified 
;; in the MAIL command (in parsed emailaddr struct format)

;; The functions should return either :ok, :transient, or :err.  For
;; the latter two, an additional value can be returned.  It will be
;; used as part of the response string.

(defparameter *smtp-mail-from-checkers* 
    '(("Blacklisted sender checker" smtp-mail-from-blacklist-checker)
      ("Sender domain required checker" smtp-mail-from-domain-required-checker)
      ("Sender domain must resolve checker" smtp-mail-from-domain-checker)))

;; Same idea as the above.  Checkers are called with
;; client ip address, sender, recip-type, new recipient, existing recipients.
;; (all email addresses are passed in parsed emailaddr struct form).
;; recip-type will be :local or :remote.
;; The checkers are called after built-in blacklist (via aliases) and unknown
;; user checks are done.

(defparameter *smtp-rcpt-to-checkers* 
    '(("Relay checker" smtp-rcpt-to-relay-checker)
      ("DNS blacklist checker" smtp-rcpt-to-dns-blacklist-checker)))

(defparameter *rcpt-to-negative-initial-delay* 5)

;; Same idea as above.  Checkers are called with 
;; client ip address, sender, recips (all email addresses parsed).
;; This is called just before the DATA command responds with the 
;; normal go-ahead (354 Enter mail...).  
(defparameter *smtp-data-pre-checkers* nil)

;; These checkers are called after the client has sent the CRLF.CRLF
;; message terminator, but before a response code is sent. Checkers
;; are called with:  client ip address, sender, recips (all email addressed
;; parsed), message size, message headers, message data filename.
(defparameter *smtp-data-checkers* nil)

;; Initial connection checkers.  Checkers are called with the 
;; client ip address.
(defparameter *smtp-connection-checkers* 
    '(("Blacklist checker" smtp-connection-blacklist-checker)
      ("Reverse DNS checker" smtp-connection-reverse-dns-checker)))


;; List of checkers to be called after a message body has been
;; received.  A checker entry is a list with two elements.  The first
;; element is a string which describes the checker.  The second
;; element is the checker function (or a symbol naming the function.
;; All listed checkers must return :ok before the message is accepted.
;; see checker.cl for more details.
(defparameter *message-data-checkers* 
    '(("Message size checker" message-size-checker)
      ("Hop count checker" hop-count-checker)))

;; User to run external checkers as.
(defparameter *external-checker-user* "mailnull")

;; Function that will be called when message a message is first received
;; which can be used to add additional headers to the message.  The
;; function will be called w/ one argument, the "queue" structure.
(defparameter *extra-headers-func* nil)


;; See MAILERS.txt for information on mailers.
(defparameter *mailers*
    '((:local ;; keyword iden
       "Unix mailbox" 
       lookup-addr-in-passwd
       deliver-local-command
       "root")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only used if "smtp" entry can't be found in /etc/services.
(defparameter *smtp-port* 25) 
(defparameter *smtp-ip* nil) ;; address to bind socket
;; Max SMTP command line length.  including CR, but not LF
(defparameter *maxlinelen* 2048) 
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

;;;;;;;;;;;;;;;

(defparameter *queue-lock-timeout* (* 15 60)) ;; 15 minutes
(defparameter *queue-lock-refresh-interval* (* 10 60)) ;; every 10 minutes
(defparameter *queue-max-threads* 10) ;; max number of delivery threads
    
;;;;;;;;;;;;;;;

(defparameter *rep-start-server* nil)
(defparameter *rep-server-port* 9567)

(defparameter *ignore-dns-cache* nil)
