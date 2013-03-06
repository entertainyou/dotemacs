;; HUANG Wei's emacs setting
(setq user-full-name "HUANG Wei")
(setq user-mail-address "weih@opera.com")

(defvar *require-not-found-message* "[require] %s not present")

(defun require-not-found (package)
  (message *require-not-found-message* package))

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq major-mode 'text-mode)

(setq fill-column 80)

;; (load-theme 'tango-dark t)
;; (when (require 'twilight-anti-bright 'nil t)
;;   (load-theme 'twilight-anti-bright t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'twilight-anti-bright t)

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

;; (global-unset-key (kbd "M-/"))
(if (require 'yasnippet nil t)
  ;; (yas/initialize)
  (progn
    (yas-global-mode t)
    ;; (yas/load-directory "/home/weih/.emacs.d/snippets")
    (global-set-key (kbd "M-/") 'yas/expand)
    (define-key yas-keymap (kbd "<S-iso-lefttab>") 'yas/prev-field)
    (setq yas-also-auto-indent-first-line t)
    (require 'auto-yasnippet nil t))
  (require-not-found 'yasnippet))


(column-number-mode t)

(global-set-key (kbd "C-c f") 'find-grep)

(if (require 'duplicate-thing nil t)
  (global-set-key (kbd "C-c d") 'duplicate-thing)
  (require-not-found 'duplicate-thing))

(global-set-key (kbd "C-c l") 'goto-line)
;; (global-set-key (kbd "C-c m") 'menu-bar-mode)
(global-set-key (kbd "C-c m") 'notmuch)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c q") 'query-replace-regexp)
(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

(global-set-key [(f2)] 'emacs-lisp-byte-compile)

(if (require 'highlight-symbol nil t)
    (progn
      (global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
      (global-set-key (kbd "C-c h n") 'highlight-symbol-next)
      (global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
      (global-set-key (kbd "C-c n") 'highlight-symbol-next)
      (global-set-key (kbd "C-c p") 'highlight-symbol-prev))
  (require-not-found 'highlight-symbol))

(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "C-c =") 'enlarge-window)
(global-set-key (kbd "C-c -") 'shrink-window)

(global-set-key (kbd "C-c y") 'yas/expand)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(if (require 'icicles)
;; (setq icicle-show-Completions-initially-f  t)
    (progn
      (icy-mode t)
      (define-key global-map (kbd "C-c b") 'icicle-bookmark))
  (require-not-found 'icicles))

(if (require 'rainbow-mode nil t)
    (rainbow-mode t)
  (require-not-found 'rainbow-mode))

(if (require 'iedit)
    (define-key global-map (kbd "C-;") 'iedit-mode)
  (require-not-found 'iedit))

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

(defun echo-file-name (output-to-buffer)
  "Echo buffer's full-path"
  (interactive "P")
  (let ((standard-output (if output-to-buffer (current-buffer) t)))
    (prin1 buffer-file-name)
    nil))

(global-set-key (kbd "C-c e") 'echo-file-name)

(setq-default indent-tabs-mode nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; handle "same name" buffers
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-buffer-name-style 'post-forward)

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

(if (require 'dired-isearch nil t)
  (progn
    (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
    (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
    (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
    (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp))
  (require-not-found 'dired-isearch))

(if (require 'drag-stuff nil t)
    (drag-stuff-global-mode)
  (require-not-found 'drag-stuff))

(if (require 'ace-jump-mode nil t)
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (require-not-found 'ace-jump-mode))

(if (require 'dired-single nil t)
    nil
  (require-not-found 'dired-single))

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

(if (require 'awk-it nil t)
    nil
  (require-not-found 'awk-it))

(if (require 'midnight nil t)
    nil
  (require-not-found 'midnight))

(setq debug-on-error t)

;; (icomplete-mode t)

(global-rainbow-delimiters-mode)

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

(if (require 'keyfreq nil t)
    (progn
     (keyfreq-mode t)
     (keyfreq-autosave-mode t))
    (require-not-found 'keyfreq))

(provide 'weih-basic)
