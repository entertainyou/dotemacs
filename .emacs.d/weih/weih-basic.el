;; HUANG Wei's emacs setting

(setq user-full-name "entertainyou")
(setq user-mail-address "grubbyfans@gmail.com")

(setq major-mode 'text-mode)

(setq fill-column 80)

(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-blackboard)

(color-theme-blackboard)

(menu-bar-mode -1)
(tool-bar-mode -1)

(scroll-bar-mode -1)
(setq initial-scratch-message "")

(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'animate)

(setq tab-width 4)
;;; hightlight mark region
(transient-mark-mode t)

;;; highlight matching parenthsis
(show-paren-mode t)

(global-linum-mode t)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-message t)
(setq kill-ring-max 200)

;;; change buffer to reflect with modification on disk
(global-auto-revert-mode 1)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

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

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c m") 'menu-bar-mode)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c r") 'query-replace-regexp)

(global-set-key [(f2)] 'emacs-lisp-byte-compile)


;; (require 'highlight-symbol)
;; (global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-c h n") 'highlight-symbol-next)
;; (global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
;; (global-set-key (kbd "C-c n") 'highlight-symbol-next)
;; (global-set-key (kbd "C-c p") 'highlight-symbol-prev)

(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c =") 'enlarge-window)
(global-set-key (kbd "C-c -") 'shrink-window)

(global-set-key (kbd "C-c y") 'yas/expand)

(add-to-list 'load-path "/home/weih/.emacs.d/icicles")
(require 'icicles)
(setq icicle-show-Completions-initially-f  t)
(icy-mode t)

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

(add-to-list 'load-path "~/.emacs.d/expand-region.el/")
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

(provide 'weih-basic)

