(require 'cc-mode)

(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)

(setq gtags-select-buffer-single t)

(global-font-lock-mode t)

(defun remove-ac-yasnippet()
  (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
  )

(defun my-c-mode-common-hook()
  ;; (hs-minor-mode t)
  (subword-mode t)
  (which-function-mode t)
  ;; (paredit-mode t)
  ;; (gtags-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "awk")
  (c-set-offset 'inline-open 0)
  (hide-ifdef-mode)
  ;; (rainbow-delimiters-mode)
  ;; (textmate-mode)
  ;; (setq ac-sources (append '(ac-source-gtags) ac-sources))
  )

(add-hook 'emacs-lisp-mode-hook 'remove-ac-yasnippet)
(add-hook 'c-mode-common-hook 'remove-ac-yasnippet)
(add-hook 'c++-mode-common-hook 'remove-ac-yasnippet)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-hook ()
  ;; (paredit-mode t)
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'java-mode-hook 'my-java-hook)

(add-hook 'gtags-select-mode-hook
  '(lambda ()
     ;; (setq hl-line-face 'underline)
     (hl-line-mode 1)
     ;; (lcaol (kbd "<return>") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "<return>") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
     (define-key gtags-select-mode-map (kbd "n") 'next-line)
     (define-key gtags-select-mode-map (kbd "p") 'previous-line)
))

;; (add-to-list 'load-path "~/.emacs.d/weih/auto-complete-1.3.1")
;; ;; (load-file "~/.emacs.d/weih/auto-complete-1.3.1/auto-complete-config.el")
;; (require 'auto-complete-config "~/.emacs.d/weih/auto-complete-1.3.1/auto-complete-config.el")

(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (setq ac-ignore-case 'smart)
  (setq ac-sources (append ac-sources '(ac-source-gtags)))
  (setq ac-use-fuzzy t)
  (global-auto-complete-mode t)
)

;; ;; this conflicts with auto complete, RET to choose current choice.
;; (dolist (map (list lisp-mode-map emacs-lisp-mode-map lisp-interaction-mode-map
;;                    awk-mode-map java-mode-map
;;                     c-mode-base-map))
;;   (define-key map (kbd "RET") 'newline-and-indent)
;;   (define-key map (kbd "<return>") 'newline-and-indent))

;; (global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "<return>") 'newline-and-indent)
(define-key global-map (kbd "RET") 'newline-and-indent)

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

;; (wrap-region-global-mode t)
(when (require 'magit nil t)
  (global-set-key (kbd "C-c z") 'magit-blame-mode)
  )


;; (add-to-list 'load-path "~/.emacs.d/weih/mark-multiple.el")
;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; (require 'mark-more-like-this)
;; (global-set-key (kbd "C-<") 'mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mark-next-like-this)
;; (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)

;; (require 'rename-sgml-tag)
;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; (require 'js2-rename-var)
;; (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

(when (require 'autopair nil t)
  (autopair-global-mode)
)


(add-to-list 'auto-mode-alist '("\\.inc\\'" . c-mode))

;; (setq scheme-program-name "gsi -:d-")
;; (setq scheme-program-name "guile")
;; (setq scheme-program-arguments "")

;; (require 'xscheme)

;; (require 'geiser-install)

(setq inferior-lisp-program "/usr/bin/sbcl")
(when (require 'slime-autoloads nil t)
  (slime-setup '(slime-fancy)))

(define-derived-mode bream-mode java-mode
  "bream mode"
  (setq mode-name "bream")
  ;; (font-lock-add-keywords 'bream-mode '(("\\<modifies\\>" . 'font-lock-warning-face)))
  )

(add-to-list 'auto-mode-alist '("\\.bream\\'" . bream-mode))

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; (require 'sgml nil t)
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region)
  )


;; TODO: remove the PATH.
(load-file "~/.emacs.d/weih/lyskom-all-0.48.elc")
;; (load-file "./lyskom-all-0.48.el")


(add-hook 'pike-mode-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))



(provide 'weih-prog)
