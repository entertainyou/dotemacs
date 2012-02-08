(require 'cc-mode)

(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)

(defun my-c-mode-common-hook()
  ;; (hs-minor-mode t)
  (subword-mode t)
  (paredit-mode t)
  ;; (gtags-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "awk")
  (c-set-offset 'inline-open 0)
  ;; (textmate-mode)
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

(defun my_java_hook ()
  (paredit-mode t)
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'java-mode-hook 'my_java_hook)



(add-hook 'gtags-select-mode-hook
  '(lambda ()
     ;; (setq hl-line-face 'underline)
     ;; (hl-line-mode 1)
     ;; (lcaol (kbd "<return>") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "<return>") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
))

(add-to-list 'load-path "~/.emacs.d/weih/auto-complete-1.3.1")
;; (load-file "~/.emacs.d/weih/auto-complete-1.3.1/auto-complete-config.el")
(require 'auto-complete-config "~/.emacs.d/weih/auto-complete-1.3.1/auto-complete-config.el")


(setq ac-sources (append ac-sources '(ac-source-gtags)))
(setq ac-use-fuzzy t)
(ac-config-default)

(global-auto-complete-mode t)

(global-set-key (kbd "M-/") 'yas/expand)
(global-set-key (kbd "<S-iso-lefttab>") 'yas/prev-field)
(global-font-lock-mode t)

(dolist (map (list lisp-mode-map emacs-lisp-mode-map lisp-interaction-mode-map
                   awk-mode-map java-mode-map
                    c-mode-base-map))
  (define-key map (kbd "RET") 'newline-and-indent)
  (define-key map (kbd "<return>") 'newline-and-indent))

(global-set-key (kbd "C-c g f") 'gtags-find-file)
(global-set-key (kbd "C-c g t") 'gtags-find-tag)
(global-set-key (kbd "C-c g T") 'gtags-find-tag-other-window)

(global-set-key (kbd "C-c g s") 'gtags-find-symbol)
(global-set-key (kbd "C-c g r") 'gtags-find-rtag)
(global-set-key (kbd "C-c g g") 'gtags-find-with-grep)
(global-set-key (kbd "C-c g p") 'gtags-find-pattern)
(global-set-key (kbd "C-c g b") 'gtags-pop-stack)

(setq mode-compile-expert-p t)
(setq mode-compile-reading-time 0)

(wrap-region-global-mode t)
(require 'magit)

(add-to-list 'load-path "~/.emacs.d/weih/mark-multiple.el")
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)

(require 'rename-sgml-tag)
(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; (require 'js2-rename-var)
;; (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

(provide 'weih-prog)