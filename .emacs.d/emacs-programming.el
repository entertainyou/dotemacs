

(require 'cc-mode)
;; (c-set-offset 'inline-open 0)
;; (c-set-offset 'friend '-)
;; (c-set-offset 'substatement-open 0)

(autoload 'gtags-mode "gtags" "" t)

(defun my-c-mode-common-hook()
  ;; (hs-minor-mode t)
  (subword-mode t)
  (gtags-mode t)
  (c-set-style "awk")
  ;; (setq tab-width 4 indent-tabs-mode nil)
  ;; (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  ;; (define-key c-mode-base-map [(return)] 'newline-add-indent)
  ;; (define-key c-mode-base-map [(f7)] 'compile)
  ;; (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  ;; (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;; (setq c-macro-shrink-window-flag t)
  ;; (setq c-macro-preprocessor "cpp")
  ;; (setq c-macro-cppflags " ")
  ;; (setq c-macro-prompt-flag t)

  ;; (setq abbrev-mode t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c++-mode-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (c-set-style "stroustrup")
)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
(setq autopair-blink nil)

(defun skeleton-pair-func()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
			       (?, " ")
			       (?\; " ")))
  (setq skeleton-pair t)
  (local-set-key (kbd ",") 'skeleton-pair-insert-maybe)
)

(add-hook 'c-mode-common-hook 'skeleton-pair-func)
(add-hook 'emacs-lisp-mode-hook 'skeleton-pair-func)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(global-auto-complete-mode t)
(setq ac-use-fuzzy t)
(ac-config-default)
;; (require 'auto-complete)
(require 'yasnippet)
(setq ac-sources (append ac-sources '(ac-source-dictionary)))

(global-set-key (kbd "M-/") 'yas/expand)
(global-font-lock-mode t)
(font-lock-add-keywords 'c-mode
			'(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
			  ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
			  ("\\((\\|)\\)" . font-lock-keyword-face)
			  ("{\\|}" . font-lock-keyword-face)
			  ("\\[\\|\\]" . font-lock-keyword-face)
			  (";" . font-lock-keyword-face)
			  ("," . font-lock-keyword-face)
			  ;; ("(\\|)" . yellow-face)
			  )
			)

;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;; (hl-line-mode t)

(require 'align)
(global-set-key "\C-x\C-j" 'align)

(require 'generic-x)

;; newline and ident
(dolist (map (list lisp-mode-map emacs-lisp-mode-map lisp-interaction-mode-map
                   awk-mode-map java-mode-map
                    c-mode-base-map))
  (define-key map (kbd "RET") 'newline-and-indent)
  (define-key map (kbd "<return>") 'newline-and-indent))

;; Emacs template

(require 'template)
(template-initialize)


(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
;; (global-set-key "\C-cc" 'mode-compile)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)

(define-key c-mode-map (kbd "C-c g t") 'gtags-find-tag)
(define-key c-mode-map (kbd "C-c g s") 'gtags-find-symbol)
(define-key c-mode-map (kbd "C-c g r") 'gtags-find-rtag)
(define-key c-mode-map (kbd "C-c g g") 'gtags-find-with-grep)

(setq mode-compile-expert-p t)
(setq mode-compile-reading-time 0)
(setq cc-default-compiler-options "-Wall -g")

(provide 'emacs-programming)
