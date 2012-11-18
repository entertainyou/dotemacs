(if (require 'org2blog nil t)
    (setq org2blog/wp-blog-alist
          '(("entertainyou"
             ;; :url "http://entertainyou.oicp.net/wordpress/xmlrpc.php"
             :url "http://entertainyou.us/wordpress/xmlrpc.php"
             :username "weih"
             )))
  (require-not-found 'org2blog))

(provide 'weih-blog)
