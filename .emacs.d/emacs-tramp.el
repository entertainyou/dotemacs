
(require 'tramp)
;; (setq tramp-default-method "ssh")
;; ;; (add-to-list 'tramp-default-method-alist '("" "john" "ssh"))
;; (add-to-list 'tramp-default-method-alist '("cnode01" "" "ssh"))
;; (add-to-list 'tramp-default-method-alist
;; 			 '("\\`localhost\\'" "\\`root\\'" "su"))

;; (setq tramp-default-user "root")

;; (add-to-list 'tramp-default-user-alist
;;                   '("ssh" ".*\\.somewhere\\.else\\'" "john"))

(provide 'emacs-tramp)
