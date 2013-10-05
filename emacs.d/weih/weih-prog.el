(require 'weih-common)

(require 'cc-mode)

(try-require 'gtags
             (autoload 'gtags-mode "gtags" "" t)
             (setq gtags-select-buffer-single t))

(global-font-lock-mode t)

;; (defun remove-ac-yasnippet()
;;   (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;;   )

(defun my-c-mode-common-hook ()
  ;; (hs-minor-mode t)
  (subword-mode t)
  ;; (which-function-mode t)
  ;; (paredit-mode t)
  ;; (gtags-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "awk")
  (c-set-offset 'inline-open 0)
  (hide-ifdef-mode)
  (flyspell-prog-mode)
  ;; (rainbow-delimiters-mode)
  ;; (textmate-mode)
  ;; (setq ac-sources (delq 'ac-source-gtags ac-sources))
  )

(defun lisp-mode-hook ()
  (setq smartparens-mode nil)
  (paredit-mode))
;; (add-hook 'emacs-lisp-mode-hook 'remove-ac-yasnippet)
;; (add-hook 'c-mode-common-hook 'remove-ac-yasnippet)
;; (add-hook 'c++-mode-common-hook 'remove-ac-yasnippet)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'scheme-mode-hook 'lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hook)

(defun my-java-hook ()
  ;; (paredit-mode t)
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (flyspell-prog-mode)
  ;; (setq ac-sources (delq 'ac-source-gtags ac-sources))
  (c-set-offset 'substatement-open 0))

(add-hook 'java-mode-hook 'my-java-hook)

(defun my-lisp-hook ()
  (flyspell-prog-mode))

(add-hook 'lisp-mode-hook 'my-lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)

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

(try-require 'auto-complete-config
	     (ac-config-default)
             (ac-flyspell-workaround)
	     (setq ac-ignore-case 'smart)
	     ;; (setq ac-sources (append ac-sources '(ac-source-gtags)))
	     (setq ac-use-fuzzy t)
	     (global-auto-complete-mode t))

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
(try-require 'magit
	     (global-set-key (kbd "C-c z") 'magit-blame-mode)
	     (add-hook 'magit-log-edit-mode-hook
		       (lambda ()
			 (set-fill-column 72)
			 (auto-fill-mode t))))

(try-require 'git-commit-mode)

;; (add-to-list 'load-path "~/.emacs.d/weih/mark-multiple.el")
;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(try-require 'multiple-cursors
	     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this) 
	     (global-set-key (kbd "C->") 'mc/mark-next-like-this) 
	     (global-set-key (kbd "C-M-m") 'mc/mark-more-like-this)) 

;; (require 'rename-sgml-tag)
;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; (require 'js2-rename-var)
;; (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

;; (try-require 'autopair
;; 	     (autopair-global-mode))

(try-require 'smartparens
             (smartparens-global-mode t)
             (setq sp-ignore-modes-list
                   (delete 'minibuffer-inactive-mode sp-ignore-modes-list)))
;; (if (require 'smartparens nil t)
;;     (smartparens-global-mode t)
;;   (require-not-found 'smartparens))

;; (setq scheme-program-name "gsi -:d-")
;; (setq scheme-program-name "guile")
;; (setq scheme-program-arguments "")

;; (require 'xscheme)

;; (require 'geiser-install)

;; (setq inferior-lisp-program "/usr/bin/clisp")
(setq inferior-lisp-program "/usr/bin/sbcl")
(try-require 'slime-autoloads
	     (slime-setup))

(define-derived-mode bream-mode java-mode
  "bream mode"
  (setq mode-name "bream")
  (font-lock-add-keywords 'bream-mode '(("\\<modifies\\>" . 'font-lock-warning-face))))

(mapcar
 (lambda (l)
   (add-to-list 'auto-mode-alist l))
  '(("\\.bream\\'" . bream-mode)
   ("\\.php\\'" . php-mode)
   ("\\.scheme\\'" . scheme-mode)
   ("\\.inc\\'" . c-mode)))

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; (require 'sgml nil t)
(try-require 'expand-region
	     (global-set-key (kbd "C-=") 'er/expand-region)
	     (global-set-key (kbd "C--") 'er/contract-region))

;; TODO: remove the PATH.
;; (load-file "~/.emacs.d/weih/lyskom-all-0.48.elc")
;; (load-file "./lyskom-all-0.48.el")


;; (add-hook 'pike-mode-hook
;;           (lambda()
;;             (local-set-key (kbd "C-c <right>") 'hs-show-block)
;;             (local-set-key (kbd "C-c <left>")  'hs-hide-block)
;;             (local-set-key (kbd "C-c <up>")    'hs-hide-all)
;;             (local-set-key (kbd "C-c <down>")  'hs-show-all)
;;             (hs-minor-mode t)))
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)


(c-add-style "WebKit" '("Google"
                        (c-basic-offset . 4)
                        (c-offsets-alist . ((innamespace . 0)
                                            (access-label . -)
                                            (case-label . 0)
                                            (member-init-intro . +)
                                            (topmost-intro . 0)))))



(dir-locals-set-class-variables
 'opera-src
 '((c-mode . ((c-file-style . "Google")))
   (c++-mode . ((c-file-style . "Google")))))
(dir-locals-set-class-variables
 'webkit-src
 '((c-mode . ((c-file-style . "WebKit")))
   (c++-mode . ((c-file-style . "WebKit")))))

(dir-locals-set-directory-class "~/work/" 'opera-src)
(dir-locals-set-directory-class
 "~/work/chromium/src/third_party/WebKit/" 'webkit-src)

(defmacro project-specifics (name &rest body)
  (declare (indent 1))
  `(progn
     (add-hook 'find-file-hook
               (lambda ()
                 (when (string-match-p ,name (buffer-file-name))
                   ,@body)))
     (add-hook 'dired-after-readin-hook
               (lambda ()
                 (when (string-match-p ,name (dired-current-directory))
                   ,@body)))))

;; (project-specifics "projects/zombietdd"
;;                    (set (make-local-variable 'slime-js-target-url) "http://localhost:3000/")
;;                    (ffip-local-patterns "*.js" "*.jade" "*.css" "*.json" "*.md"))

(project-specifics "work/mobile"
  (setq compile-command "make -C ~/work/mobile/android -j 16 all PRODUCT=oupeng ANDROID_GOMA_WRAPPER=/usr/bin/ccache BUILD_CHROMIUM=NO"))

(try-require 'ac-slime
             (add-hook 'slime-mode-hook 'set-up-slime-ac)
             (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
             (eval-after-load "auto-complete"
               '(add-to-list 'ac-modes 'slime-repl-mode)))

(try-require 'insert-shebang
             (add-hook 'find-file-hook 'insert-shebang))

(provide 'weih-prog)
