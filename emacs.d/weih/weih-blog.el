(require 'weih-common)

;; (try-require 'org2blog
;;     (setq org2blog/wp-blog-alist
;;           '(("entertainyou"
;;              ;; :url "http://entertainyou.oicp.net/wordpress/xmlrpc.php"
;;              :url "http://entertainyou.us/wordpress/xmlrpc.php"
;;              :username "weih"
;;              ))))
(add-to-list 'load-path "~/o-blog/")
(try-require 'o-blog)
(try-require 'org)
(try-require 'org-export)
(provide 'weih-blog)
