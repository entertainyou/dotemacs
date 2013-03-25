;; (require 'smtpmail)
;; (require 'notmuch)

;; (setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
;; (setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
;; (setq smtpmail-default-smtp-server "smtp.opera.com") ; set before loading library
;; (setq smtpmail-smtp-server "smtp.opera.com")
;; (setq smtpmail-local-domain "opera.com")
;; (setq smtpmail-sendto-domain "opera.com")
;; (setq smtpmail-mail-address "weih@opera.com")
;; (setq user-full-name "HUANG Wei")
;; (setq user-mail-address "weih@opera.com")

;; (setq smtpmail-debug-info t) ; only to debug problems
;; ;; (setq smtpmail-auth-credentials  ; or use ~/.authinfo
;; ;;       '(("smtp.opera.com" 587 "weih" "")))


;; ;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; ;; (setq smtpmail-starttls-credentials
;; ;;       '(("YOUR SMTP HOST" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))

;; (provide 'weih-mail)



(require 'weih-common)
(defvar gnus-summary-buffer)
(defvar gnus-inhibit-images 'nil)
(if (not (fboundp 'gnus-blocked-images))
    (defun gnus-blocked-images () nil))

(try-require 'notmuch
	     (define-key notmuch-hello-mode-map (kbd "g") 'notmuch-hello-poll-and-update)
	     (define-key notmuch-hello-mode-map (kbd "G") 'self-insert-command)
	     (define-key notmuch-hello-mode-map (kbd "n") 'widget-forward)
	     (define-key notmuch-hello-mode-map (kbd "p") 'widget-backward)
	     (setq notmuch-search-oldest-first 'nil))

(setq message-auto-save-directory "~/.emacs.d/Mail/drafts")

(setq mail-host-address "opera.com")

(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

(try-require 'bbdb
	     (define-key message-mode-map (kbd "<C-tab>") 'bbdb-complete-mail)
	     (setq bbdb-complete-mail-allow-cycling t))

(provide 'weih-mail)
