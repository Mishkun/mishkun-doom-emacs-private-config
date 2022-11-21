;;; +startrek.el -*- lexical-binding: t; -*-
;;;
(setq startrek--task-script "~/.dotfiles/zshell/bin/st_getIssue")
(setq startrek--task-statuses-alist '(("open" . "TODO")
                                      ("new" . "TODO")
                                      ("analyse" . "TODO")
                                      ("inProgress" . "NEXT")))

(defun startrek-append-task-as-org-mode-heading (key)
  "Loads startrek task and appends its title and description"
  (interactive "sProvide task key:")
  (let* ((task (startrek--load-task key))
         (summary (gethash "summary" task))
         (description (gethash "description" task))
         (link (replace-regexp-in-string (regexp-quote "st-api.yandex-team.ru/v2/issues") "st.yandex-team.ru" (gethash "self" task)))
         (status-key (gethash "status" task))
         (status (assoc status-key startrek--task-statuses-alist)))
    (insert (format "%s [[%s][%s]] %s\n%s" (or status "TODO") link key summary (or description "")))))

(defun startrek--load-task (key)
  "Loads startrek task using handy shell script i wrote"
  (json-parse-string
    (shell-command-to-string (concat startrek--task-script " " key))))
