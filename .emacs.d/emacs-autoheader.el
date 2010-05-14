;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      emacs-autoheader.el
;;                
;; Description:   
;;                
;; Copyright (C) 2010,  entertainyou
;; Author:        entertainyou <grubbyfans@gmail.com>
;; Created at:    Sat Mar 27 22:30:24 2010
;;                
;; Modified by:   entertainyou <grubbyfans@gmail.com>
;; Modified at:   Fri May 14 16:21:10 2010
;;                
;; Status:        Experimental, do not distribute.
;; Update count:  1
;;                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (require 'auto-header)
(require 'auto-header)
(setq header-full-name "entertainyou")
(setq header-email-address "grubbyfans@gmail.com")
(setq header-update-on-save '(filename
                              modified
                              counter
                              copyright
                              ))
(setq header-field-list '(
                          filename
                          blank
                          description
                          blank
                          copyright
                          author
                          created
                          blank
                          modified_by
                          modified
                          blank
                          status
                          update
                          blank
                          ))

(provide 'emacs-autoheader)
