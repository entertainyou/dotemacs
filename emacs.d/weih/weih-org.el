(add-hook 'org-load-hook
          (setcdr (assoc "\\.x?html?\\'" org-file-apps) "firefox %s"))

(provide 'weih-org)
