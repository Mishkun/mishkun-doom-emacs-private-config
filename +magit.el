;;; +magit.el -*- lexical-binding: t; -*-

(map! :map magit-mode-map
      :leader
      (:prefix "g"
      :desc "Refresh magit buffer" "r" #'magit-refresh))

; magit restore workaround with persp-mode.el
(persp-def-buffer-save/load
 :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
 :save-vars '(default-directory)
 :load-function #'(lambda (savelist &rest _funcs)
                      (let* ((vars-list (car (cddr savelist)))
                             (buf-dir (alist-get 'default-directory vars-list)))
                        (magit-status buf-dir))
                    )
 )

; open magit buffer in project
(map! :leader
      (:prefix "g"
       :desc "open magit for project" "p" (cmd! (counsel-projectile-switch-project "v"))))

;; remove outdated default 50 max length
(setq git-commit-summary-max-length 68)
