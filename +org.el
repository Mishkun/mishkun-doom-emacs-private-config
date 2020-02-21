;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; Org configuration

;; Mark the completion time of TODOs
(setq org-log-done 'time)
;; and also log time
(setq org-log-done-with-time t)
(setq org-log-note-clock-out t)

;; add yandex-tracker link abbrev
(after! org
  (pushnew! org-link-abbrev-alist
            '("st" . "https://st.yandex-team.ru/%s"))
  (pushnew! org-link-abbrev-alist
            '("pr" . "https://github.com/YandexClassifieds/mobile-autoru-client-android/pull/%s")))


;; disable annoying Table of Content on export
(setq org-export-with-toc nil)

;; Org-agenda setup
(setq org-agenda-files '("~/org" "~/org/projects"))
;; Add my custom dashboard
(defun list-projects (arg &rest params)
    (mapcar (lambda (x)
              `(todo "TODO|WAIT" ,(append `((org-agenda-files '(,x)) (org-agenda-overriding-header ,x))
                                          params)))
            arg))

(setq org-agenda-custom-commands
      `(("O" "Overview"
         ;; Calendar agenda for time-bounded tasks
         ,(append
         '((agenda "" ((org-agenda-span 8)
                      (org-agenda-start-day "0d")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
         (list-projects (directory-files "~/org/projects" :match-regexp ".*\.org$")
                        '(org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
        ("P" "Projects"
         ,(list-projects (directory-files "~/org/projects" :match-regexp ".*\.org$")))))

;; deft setup
(setq deft-directory "~/org/deft")

;; org-roam setup
(use-package! org-roam
      :after org
      :hook (org-mode . org-roam-mode)
      :custom
      (org-roam-directory "~/org/deft"))

;; keybindings
(map! :leader
      (:prefix "o"
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))
        :desc "Org-brain" "B" #'org-brain-visualize)
     (:prefix "n"
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))))


(map! :map org-mode-map
      :localleader
      (:prefix ("r" . "refile")
               "s" #'org-sort))
