;; configuration options for maild's greylist feature.

;; This file should be made mode 0600 so that folks can't
;; get at the password.  

(setq *greylist-db-user* "greylist") ;; see greyadmin.cl
(setq *greylist-db-password* "...change me....")

;; 25 hours.  There are some stupid IPs out there that only
;; reattempt delivery once a day
(setq *greylist-delay-lifetime* (* 25 3600)) 

(setq *greylist-delay* (* 18 60))
