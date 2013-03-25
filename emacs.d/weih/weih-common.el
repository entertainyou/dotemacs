(defmacro try-require (package-name &rest body)
  "If PACKAGE-NAME can be found, require PACKAGE-NAME, return t
and do BODY, else report PACKAGE-NAME can not be found and return nil"
  `(if (require ,package-name nil t)
       (progn
         ,@body
         t)
     (progn
       (message "require package %s failed" ,package-name)
       nil)))

(provide 'weih-common)
