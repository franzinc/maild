;; $Id: config.cl,v 1.22 2003/07/09 16:15:27 dancy Exp $

(in-package :user)

(defparameter *debug* nil)

(defparameter *aliases-file* "/etc/aliases")

(defparameter *stats-file* "/var/state/maild")

;; If this is nil, it is determined by calling (gethostname)
(defparameter *short-host-name* nil)

;; If this is nil, the fully qualified domain name of this
;; host will be determined automatically, if possible.
(defparameter *fqdn* nil)

;; Other names by which this host is known (Besides *fqdn* and
;; *short-host-name*).
(defparameter *host-aliases* '("localhost"))

;; Domains for which we receive mail. Add your domain name here.
;; *short-host-name*, *fqdn*, and *host-aliases* are included
;; implicitly so you don't need to add them.
(defparameter *localdomains* nil)

;; List of functions to call to determine if the client is allowed to
;; relay through this server.  Function is called with arguments:
;;  1) ip address of client 
;;  2) envelope sender
;;  3) envelope recipient
;; The function should return true (non-nil) if relaying is allowed,
;; otherwise nil.  The first function that returns true will
;; terminate the checking.
(defparameter *relay-checkers* '(relaying-allowed-p))

;; List of IP addresses or networks.  This is used by the default
;; relay checker "relaying-allowed-p"
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

;;;;; DNSNL stuff

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
      ("Sender domain required checker" 
       smtp-mail-from-domain-required-checker)
      ("Sender domain exists checker" smtp-mail-from-domain-checker)))

;; Same idea as the above.  Checkers are called with
;; client ip address, sender, recip-type, new recipient, existing recipients.
;; (all email addresses are passed in parsed emailaddr struct form).
;; recip-type will be :local or :remote.
;; The checkers are called after built-in blacklist (via aliases) and unknown
;; user checks are done.

(defparameter *smtp-rcpt-to-checkers* 
    '(("Relay checker" smtp-rcpt-to-relay-checker)
      ("DNS blacklist checker" smtp-rcpt-to-dns-blacklist-checker)))

;; Same idea as above.  Checkers are called with 
;; client ip address, sender, recips (all email addresses parse).
;; This is called just before the DATA command responds with the 
;; normal go-ahead (354 Enter mail...).  
(defparameter *smtp-data-pre-checkers* nil)

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

