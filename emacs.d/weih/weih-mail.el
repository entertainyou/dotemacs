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

(defun open-review-url ()
  "Open the review for the current notmuch search view"
  (interactive "")
  (if (eq major-mode 'notmuch-search-mode)
      (progn
        (notmuch-search-show-thread)
        (if (search-forward-regexp "https://critic\.oslo\.osa/r/[0-9]+" nil t)
            (let ((review-url (match-string 0)))
              (message "Found review url: %s" review-url)
              (browse-url review-url))
          (message "No review url found"))
        (notmuch-kill-this-buffer))
    (message "Not in notmuch-search-mode buffer.")))

(try-require 'notmuch
 	     (define-key notmuch-hello-mode-map (kbd "g") 'notmuch-poll-and-refresh-this-buffer)
	     (define-key notmuch-hello-mode-map (kbd "G") 'self-insert-command)
	     (define-key notmuch-hello-mode-map (kbd "n") 'widget-forward)
	     (define-key notmuch-hello-mode-map (kbd "p") 'widget-backward)
             (define-key notmuch-search-mode-map (kbd "r") 'open-review-url)
	     (setq notmuch-search-oldest-first 'nil))

(setq message-auto-save-directory "~/.emacs.d/Mail/drafts")

(setq mail-host-address "opera.com")

(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

(defun browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
  Default to the URL around or before point.  The strings in
  variable `browse-url-firefox-arguments' are also passed to
  Firefox.

  When called interactively, if variable
  `browse-url-new-window-flag' is non-nil, load the document in a
  new Firefox window, otherwise use a random existing one.  A
  non-nil interactive prefix argument reverses the effect of
  `browse-url-new-window-flag'.

  If `browse-url-firefox-new-window-is-tab' is non-nil, then
  whenever a document would otherwise be loaded in a new window, it
  is loaded in a new tab in an existing window instead.

  When called non-interactively, optional second argument
  NEW-WINDOW is used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (window-args (if (browse-url-maybe-new-window new-window)
                          (if browse-url-firefox-new-window-is-tab
                              '("-new-tab")
                            '("-new-window"))))
         (ff-args (append browse-url-firefox-arguments window-args (list url)))
         (process-name (concat "firefox " url))
         (process (apply 'start-process process-name nil
                         browse-url-firefox-program ff-args) )) ))
;; (try-require 'bbdb
;; 	     (define-key message-mode-map (kbd "<C-tab>") 'bbdb-complete-mail)
;; 	     (setq bbdb-complete-mail-allow-cycling t))

(provide 'weih-mail)
