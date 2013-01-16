(defvar opera-bts-mode nil
  "Mode variable for opera bts minor mode.")
(make-variable-buffer-local 'opera-bts-mode)

(defvar *opera-bts-projects* '("QIANG" "HUGE" "OMS" "MOBILE" "OMO")
  "Opera BTS projects")

(defun opera-bts-browse-at-point ()
  (interactive)
  (opera-bts-browse (thing-at-point 'symbol)))

(defun opera-bts-browse (ticket)
  (browse-url (concat "https://bugs.opera.com/browse/" ticket)))

(defun opera-bts-mode-on ()
  (if (not (assq 'opera-bts-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(opera-bts-mode " Opera BTS") minor-mode-alist)))
  (dolist (project *opera-bts-projects*)
    (message (concat (format "%s" project) "-[0-9]*")))
  (local-set-key (kbd "C-c t") 'opera-bts-browse-at-point))

(defun opera-bts-mode (&optional arg)
  "Opera BTS mode."
  (interactive "P")
  (setq opera-bts-mode
        (if (null arg)
            (not opera-bts-mode)
          (> (prefix-numeric-value arg) 0)))
  (if opera-bts-mode
      (opera-bts-mode-on)))

(global-set-key (kbd "C-c t") 'opera-bts-browse-at-point)
(provide 'weih-bts)
