(require 'weih-common)

;; (try-require 'org2blog
;;     (setq org2blog/wp-blog-alist
;;           '(("entertainyou"
;;              ;; :url "http://entertainyou.oicp.net/wordpress/xmlrpc.php"
;;              :url "http://entertainyou.us/wordpress/xmlrpc.php"
;;              :username "weih"
;;              ))))
;; (add-to-list 'load-path "~/o-blog/")
;; (try-require 'o-blog)
;; (try-require 'org)
;; (try-require 'org-export)
;; (provide 'weih-blog)

(try-require 'org-octopress
             (setq org-octopress-directory-top       "~/source/octopress/source")
             (setq org-octopress-directory-posts     "~/source/octopress/source/_posts")
             (setq org-octopress-directory-org-top   "~/source/octopress/source")
             (setq org-octopress-directory-org-posts "~/source/octopress/source/blog")
             (setq org-octopress-setup-file          "~/.emacs.d/weih/setupfile.org"))

(provide 'weih-blog)
