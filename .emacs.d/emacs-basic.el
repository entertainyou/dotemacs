
(setq user-full-name "entertainyou")
(setq user-mail-address "grubbyfans@gmail.com")



;; (require 'color-theme-bespin)
;; (setq default-major-mode 'text-mode)
(setq major-mode 'text-mode)

;; (setq default-fill-column 80)
(setq fill-column 80)
;; (if window-system
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-blackboard)
(color-theme-blackboard)
;; (color-theme-bespin)
;; )
(tool-bar-mode -1)
;; (menu-bar-mode -1)

;; F10 for menu
;; (scroll-bar-mode t)
(scroll-bar-mode -1)
(setq initial-scratch-message "")

(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.temp/backup")))
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.temp/auto-save" t)))
(setq auto-save-list-file-prefix "~/.emacs.temp/auto-save-list/.save-")
(setq backup-by-copying t)
(setq make-backup-file t)

;; (global-set-key [(meta ?/)] 'hippie-expand)
;; (setq hippie-expand-try-functions-list
;;   '(
;;   try-expand-line ; 补全当前行
;;   try-expand-line-all-buffers
;;   try-expand-list ; 补全一个列表
;;   try-expand-list-all-buffers
;;   try-expand-dabbrev ; 搜索当前 buffer
;;   try-expand-dabbrev-visible ; 搜索当前可见窗口
;;   try-expand-dabbrev-all-buffers ; 搜索所有 buffer
;;   try-expand-dabbrev-from-kill ; 从 kill-ring 中搜索
;;   try-complete-file-name ; 文件名匹配
;;   try-complete-file-name-partially ; 文件名部分匹配
;;   try-complete-lisp-symbol ; 补全 lisp symbol
;;   try-complete-lisp-symbol-partially ; 部分补全 elisp symbol
;;   try-expand-whole-kill
;;   )
;;   )

(display-time-mode 1)

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)
;; (setq display-time-format "%m %d %H:%M")
(setq display-time-format "%X")
;; (setq default-major-mode 'text-mode)

;; (setq font-lock-maximum-decoration t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq uniquify-buffer-name-style 'forward)
(mouse-avoidance-mode 'animate)

;; (setq default-fill-column 60)
;; (setq default-tab-width 4)
(setq tab-width 4)

(blink-cursor-mode -1)
(transient-mark-mode 1)
(show-paren-mode 1)
(mouse-wheel-mode t)

(setq visible-bell nil)
;; (setq next-line-add-newlines t)

(global-linum-mode t)
(setq mouse-yank-at-point t)			
(setq x-select-enable-clipboard t)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(setq kill-ring-max 200)

(setq apropos-do-all t)

;; (global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;; (setq frame-title-format "%b %F")
(setq frame-title-format '("" "[%b] - Emacs " emacs-version))
;; (server-start)
;; (setq icon-map-list '(x-gtk-stock-map))				
(global-auto-revert-mode 1)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/emhacks")
(require 'tabbar)
(tabbar-mode t)

;; (global-set-key [(control shift tab)] 'tabbar-backward-tab)
;; (global-set-key [(control tab)]       'tabbar-forward-tab)
;; (global-set-key (kbd "C-S-iso-lefttab") 'tabbar-backward-tab)

(global-set-key [C-S-iso-lefttab] 'tabbar-backward-tab)
(global-set-key [C-tab] 'tabbar-backward-tab)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's tabbar-buffer-groups.
 This function group all buffers into 3 groups:
 Those Dired, those user buffer, and those emacs buffer.
 Emacs buffer are those starting with “*”."
  (list
   (cond
	((string-equal "*" (substring (buffer-name) 0 1))
	 "Emacs Buffer"
	 )
	(
	 (eq major-mode 'dired-mode)
	 "Dired"
	 )
	(t
	 "User Buffer"
	 )
	))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)



;; shell
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c/")

(require 'yasnippet)

;; (load-file "~/.emacs.d/yasnippet/yasnippet.el")
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(column-number-mode t)
(require 'highlight-tail)
;; (setq highlight-tail-colors
;; 	  '(("black" . 0)
;; 		("#bc2525" . 25)
;; 		("black" . 66)))

(setq highlight-tail-colors
	  '(("blue" . 0)
		("red" . 40)
		("blue" . 80)
		("black" . 100)))


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
;; (global-set-key [(f10)] 'menu-bar-mode)
(global-set-key [(f2)] 'emacs-lisp-byte-compile)

(global-set-key (kbd "C-c m") 'menu-bar-mode)
;; (global-set-key (kbd "C-c c") 'emacs-lisp-byte-compile)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c e") 'eval-buffer)
;; (global-set-key (kbd "C-c s") 'shell)

(global-set-key (kbd "C-c t") 'highlight-tail-mode)
(global-set-key (kbd "C-c a") 'auto-complete-mode)


(global-set-key (kbd "C-c p") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c o") 'find-file)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-c u") 'emacs-uptime)


(require 'highlight-symbol)
(global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)


(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)


;; Use ibuffer mode
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; (fset 'nextline-and-delete
;;    "\C-n\C-d")
(setq echo-keystrokes -1)
(provide 'emacs-basic)
