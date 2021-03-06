;; HUANG Wei's emacs setting

(require 'weih-common)
(setq user-full-name "HUANG Wei")
(setq user-mail-address "weih@opera.com")

;; (defvar *require-not-found-message* "[require] %s not present")

;; (defun require-not-found (package)
;;   (message *require-not-found-message* package))

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(setq major-mode 'text-mode)

(setq fill-column 80)

(try-require 'twilight-anti-bright-theme
             (load-theme 'twilight-anti-bright t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq initial-scratch-message "")

(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'animate)

(setq tab-width 4)

;;; highlight matching parenthsis
(show-paren-mode t)

(setq enable-recursive-minibuffers t)

(global-linum-mode t)

(setq x-select-enable-clipboard t)
(setq inhibit-startup-message t)
(setq kill-ring-max 200)

;;; change buffer to reflect with modification on disk
(global-auto-revert-mode 1)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(try-require 'yasnippet
             (yas-global-mode t)
             ;; (yas/load-directory "/home/weih/.emacs.d/snippets")
             (global-set-key (kbd "M-/") 'yas/expand)
             (define-key yas-keymap (kbd "<S-iso-lefttab>") 'yas/prev-field)
             (setq yas-also-auto-indent-first-line t)
             (try-require 'auto-yasnippet))

(column-number-mode t)

(global-set-key (kbd "C-c f") 'find-grep)
(global-set-key (kbd "C-c j") 'package-list-packages)
(global-set-key (kbd "C-c J") 'package-list-packages-no-fetch)

(try-require 'duplicate-thing
	     (global-set-key (kbd "C-c d") 'duplicate-thing))

(global-set-key (kbd "C-c l") 'goto-line)
;; (global-set-key (kbd "C-c m") 'menu-bar-mode)
(global-set-key (kbd "C-c m") 'notmuch)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c q") 'query-replace-regexp)
(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

(global-set-key [(f2)] 'emacs-lisp-byte-compile)

(try-require 'highlight-symbol
             (global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
             (global-set-key (kbd "C-c h n") 'highlight-symbol-next)
             (global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
             (global-set-key (kbd "C-c n") 'highlight-symbol-next)
             (global-set-key (kbd "C-c p") 'highlight-symbol-prev))

(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "C-c =") 'enlarge-window)
(global-set-key (kbd "C-c -") 'shrink-window)

(global-set-key (kbd "C-c y") 'yas/expand)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(try-require 'icicles
             (icy-mode t)
             (define-key global-map (kbd "C-c b") 'icicle-bookmark))

(try-require 'rainbow-mode
             (rainbow-mode t))

(try-require 'iedit
             (define-key global-map (kbd "C-;") 'iedit-mode))

(defvar blink-cursor-colors (list "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)

(defun blink-cursor-timer-function ()
  "Cyberpunk variant of timer `blink-cursor-timer'. OVERWRITES original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count)))
  (internal-show-cursor nil (not (internal-show-cursor-p))))

(blink-cursor-mode)

(defun print-message-and-keyring (output-to-buffer message)
  "Echo buffer's full-path"
  (interactive "P")
  (let ((standard-output (if output-to-buffer (current-buffer) t)))
    (princ message)
    (kill-new message)
    nil))

(defun echo-filepath (output-to-buffer)
  "Echo buffer's full-path"
  (interactive "P")
  (print-message-and-keyring output-to-buffer (or buffer-file-name default-directory)))

(defun echo-filename (output-to-buffer)
  "Echo buffer's name"
  (interactive "P")
  (print-message-and-keyring output-to-buffer
                             (or (and buffer-file-name (file-name-nondirectory buffer-file-name)) (buffer-name))))

(global-set-key (kbd "C-c e") 'echo-filepath)
(global-set-key (kbd "C-c C-e") 'echo-filename)

(setq-default indent-tabs-mode nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; handle "same name" buffers
(try-require 'uniquify
             (setq uniquify-buffer-name-style 'post-forward))

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

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-file-name-transforms '((".*" "~/.saves/\\1" t)))       ; use versioned backups

(try-require 'dired-isearch
             (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
             (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
             (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
             (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp))

(try-require 'drag-stuff
    (drag-stuff-global-mode))

(try-require 'ace-jump-mode
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))


;; (when (require 'undo-tree nil t)
;;   (global-undo-tree-mode)
;;   )

(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Make subword mode global
(global-subword-mode t)

;; (set-face-attribute
;;  'default nil :font "Andale Mono 15")

;; (set-face-attribute
;;  'default nil :font "Liberation Mono 14")

;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "WenQuanYi Zen Hei Mono" :size 16)))

;; (when (require 'guru nil t)
;;   (guru-global-mode))

;; load generic modes which support e.g. batch files
;; (require 'generic-x)

(mapc (lambda (package) (try-require package)) '(dired-single awk-it midnight generic-x))

(setq debug-on-error t)

;; (icomplete-mode t)

;; (global-rainbow-delimiters-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to,but not including ARGth occurrence of CHAR.")

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(winner-mode t)
(add-hook 'text-mode-hook (lambda () (flyspell-mode t)))

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; (set-default-font "Liberation Mono-13")

;; (setq window-system-default-frame-alist
;;       '((x (font "Liberation Mono 14"))))

(add-to-list 'default-frame-alist '(font . "Liberation Mono-13"))

(try-require 'keyfreq
             (keyfreq-mode t)
             (keyfreq-autosave-mode t))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)

(try-require 'ace-jump-buffer
             (global-set-key (kbd "C-c b") 'ace-jump-buffer))

(try-require 'increase-number
             (global-set-key (kbd "C-c +") 'increment-integer-at-point)
             (global-set-key (kbd "C-c -") 'decrement-integer-at-point))

(try-require 'anzu
             (global-anzu-mode +1))

(try-require 'grep-a-lot
             (grep-a-lot-setup-keys))

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-c k") 'kill-this-buffer)

(try-require 'whitespace
             (setq whitespace-line-column 110)
             (setq whitespace-style '(face empty tabs lines-tail trailing))
             (global-whitespace-mode))

(provide 'weih-basic)
