(push "192.168.0.0/24" *relay-access*) ;; all local hosts

(push "foo.com" *localdomains*)
(setq *masquerade-as* "foo.com")
(setq *strip-domain-for-local-delivery* "foo.com")

(enable-webserver ;; http://localhost:3666 for stats
  ;; firewall prevents outside access:
 :interface "0.0.0.0" :port 3666)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; anti-spam measures:

;; for now, just testing:
(setq *helo-must-match-ip* :log-only)

(setq *reverse-dns-required* nil)

;; DNS blacklisting:
(push "sbl.spamhaus.org" *dns-blacklists*)
(push "cbl.abuseat.org" *dns-blacklists*)
(setq *dns-blacklisted-response-type* :permanent)
(setq *dns-blacklist-failure-response* "Blacklisted by ~A.")

;; Greylisting:
(enable-greylist "/etc/maild-greylist.cl")
