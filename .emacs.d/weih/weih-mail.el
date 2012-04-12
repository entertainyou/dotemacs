(require 'smtpmail)
(require 'notmuch)

(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
(setq smtpmail-default-smtp-server "smtp.opera.com") ; set before loading library
(setq smtpmail-smtp-server "smtp.opera.com")
(setq smtpmail-local-domain "opera.com")
(setq smtpmail-sendto-domain "opera.com")
(setq smtpmail-mail-address "weih@opera.com")
(setq user-full-name "HUANG Wei")
(setq user-mail-address "weih@opera.com")

(setq smtpmail-debug-info t) ; only to debug problems
;; (setq smtpmail-auth-credentials  ; or use ~/.authinfo
;;       '(("smtp.opera.com" 587 "weih" "")))


(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; (setq smtpmail-starttls-credentials
;;       '(("YOUR SMTP HOST" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))

(provide 'weih-mail)