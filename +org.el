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
            '("pr" . "https://github.com/YandexClassifieds/mobile-autoru-client-android/pull/%s"))
  (pushnew! org-link-abbrev-alist
            '("download" . "file://~/org/.attach/%s")))


;; Roam setup
(setq org-roam-directory "~/org/deft")

(defun open-roam-index ()
  (interactive)
  (find-file  (concat (file-name-as-directory org-roam-directory) "index.org")))

;; Add roam keybinding to open index file
(map! :map org-mode-map
      :localleader
      (:prefix ("m" . "roam")
       "x" #'open-roam-index))

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


;; keybindings
(map! :leader
      (:prefix "o"
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))
        :desc "Org-brain" "B" #'org-brain-visualize)
     (:prefix "n"
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))))


(map! :map org-mode-map
      :localleader
      ;; Add sort to kb
      (:prefix ("r" . "refile")
       "s" #'org-sort)
      ;; rebind m c c to toggle clock instead of canceling
      (:prefix ("c" . "clock")
       "c" #'+org/toggle-clock
       "C" #'org-clock-cancel))
