;; HUANG Wei's emacs setting


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.

(setq load-path (cons "~/.emacs.d/" load-path))
(setq load-path (cons "~/.emacs.d/weih/" load-path))

;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(require 'weih-basic "~/.emacs.d/weih/weih-basic.el")
(require 'weih-prog "~/.emacs.d/weih/weih-prog.el")
(require 'weih-mail "~/.emacs.d/weih/weih-mail.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "7cced48b557e24937f437e59c7f6a6cea5ace4e603377beb5067d0b2c27b4b7d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
