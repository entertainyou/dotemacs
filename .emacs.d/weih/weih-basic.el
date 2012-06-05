;; HUANG Wei's emacs setting
;; (setq user-full-name "entertainyou")
;; (setq user-mail-address "grubbyfans@gmail.com")

(setq major-mode 'text-mode)

(setq fill-column 80)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;; (require 'color-theme)
;; (color-theme-initialize)
;; (require 'color-theme-blackboard)

;; (when (require 'solarized-theme nil t)
;;   (load-theme 'solarized-dark t)
;; )

;; (when (require 'tango-2 nil t)
;;   (load-theme 'tango-2 t)
;;   )

(load-theme 'tango-dark t)
;; (color-theme-blackboard)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(menu-bar-mode -1)
(tool-bar-mode -1)

(scroll-bar-mode -1)
(setq initial-scratch-message "")

(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'animate)

(setq tab-width 4)
;;; hightlight mark region
;; (transient-mark-mode t)

;;; highlight matching parenthsis
(show-paren-mode t)

(global-linum-mode t)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-message t)
(setq kill-ring-max 200)

;;; change buffer to reflect with modification on disk
(global-auto-revert-mode 1)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(when (require 'yasnippet nil t)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/snippets")
  (global-set-key (kbd "M-/") 'yas/expand)
  (global-set-key (kbd "<S-iso-lefttab>") 'yas/prev-field)
)

(column-number-mode t)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg))))
  (message "line(s) are duplicated"))

(global-set-key (kbd "C-c f") 'find-grep)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-c l") 'goto-line)
;; (global-set-key (kbd "C-c m") 'menu-bar-mode)
(global-set-key (kbd "C-c m") 'notmuch)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c q") 'query-replace-regexp)
(global-set-key (kbd "C-c r") 'replace-regexp)

(global-set-key [(f2)] 'emacs-lisp-byte-compile)


(require 'highlight-symbol)
(global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c n") 'highlight-symbol-next)
(global-set-key (kbd "C-c p") 'highlight-symbol-prev)

(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "C-c =") 'enlarge-window)
(global-set-key (kbd "C-c -") 'shrink-window)

(global-set-key (kbd "C-c y") 'yas/expand)

(global-set-key (kbd "C-x C-j") 'dired-jump)

;; (add-to-list 'load-path "~/.emacs.d/weih/icicles")

;; (require 'icicles)
;; (setq icicle-show-Completions-initially-f  t)
;; (icy-mode t)

(require 'rainbow-mode)
(rainbow-mode t)

(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)

(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Cyberpunk variant of timer `blink-cursor-timer'. OVERWRITES original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )

(blink-cursor-mode)

(defun echo-full-path (arg)
  "Echo buffer's full-path"
  (interactive "p")
  (message buffer-file-name)
  )

(global-set-key (kbd "C-c e") 'echo-full-path)

(setq-default indent-tabs-mode nil)

;; (add-to-list 'load-path "~/.emacs.d/weih/expand-region.el/")
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; handle "same name" buffers
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-buffer-name-style 'forward)

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

;; (defun move-line-up (n)
;;   "Move the current line up by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) -1 (- n))))

;; (defun move-line-down (n)
;;   "Move the current line down by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) 1 n)))

;; (global-set-key (kbd "<M-up>") 'move-line-up)
;; (global-set-key (kbd "<M-down>") 'move-line-down)

;; ignore case when reading a file name
(setq read-file-name-completion-ignore-case t)

;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)


(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; (add-hook 'autoconf-mode-hook   'flyspell-prog-mode)
;; (add-hook 'autotest-mode-hook   'flyspell-prog-mode)
;; (add-hook 'c++-mode-hook        'flyspell-prog-mode)
;; (add-hook 'c-mode-hook          'flyspell-prog-mode)
;; (add-hook 'cperl-mode-hook      'flyspell-prog-mode)
;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; (add-hook 'makefile-mode-hook   'flyspell-prog-mode)
;; (add-hook 'nxml-mode-hook       'flyspell-prog-mode)
;; (add-hook 'python-mode-hook     'flyspell-prog-mode)
;; (add-hook 'sh-mode-hook         'flyspell-prog-mode)

;; If you want to check the result each time before repeating, then
;; `C-x e e e...'.
;; If you want to repeat only N times, then `C-u N C-x e'.
;; If you want to repeat forever or until error, then `C-u 0 C-x e'.

;; <shift>-<F8>  to start recording
;; <shift>-<F8>  again to stop recording
;; <F8>          to call it

(defun my-toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun my-toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; start/stop recording a keyboard macro
(global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)

;; execute the most recent keyboard macro
(global-set-key (kbd "<f8>") 'call-last-kbd-macro)

;; auto-save every 100 input events
(setq auto-save-interval 100)

;; auto-save after 15 seconds idle time
(setq auto-save-timeout 15)


;; always use copying to create backup files (don't clobber symlinks)
(setq backup-by-copying t)

(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; load generic modes which support e.g. batch files
(require 'generic-x)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(when (require 'dired-isearch nil t)
  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
  (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
  (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
  (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)
)

(require 'drag-stuff)
(drag-stuff-global-mode)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(provide 'weih-basic)

