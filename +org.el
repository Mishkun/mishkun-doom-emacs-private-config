;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; Org configuration

;; Keywords
(setq org-todo-keywords '(
                          ;; General TODOS
                          (sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")
                          ;; Subtasks
                          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                          ;; To-reads
                          (sequence "TOREAD(r)" "REVIEW(v)" "ARCHIV(a)")))

;; Mark the completion time of TODOs
(setq org-log-done 'time)
;; and also log time
(setq org-log-done-with-time t)
(setq org-log-note-clock-out t)

;; add yandex-tracker link abbrev
(after! org
  (pushnew! org-link-abbrev-alist
          '("st" . "https://st.yandex-team.ru/%s")))


(setq org-export-with-toc nil)

;; Org-agenda setup
(setq org-agenda-files '("~/org" "~/org/projects"))
;; Add my custom dashboard
(setq org-agenda-custom-commands
      '(("O" "Overview"
         ;; TODo's with the A priority are shown on top. these i need to do in first place
         ((tags-todo "+PRIORITY=\"A\"" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                        (org-agenda-overriding-header "High-priority unfinished tasks:")))
          ;; Calendar agenda for time-bounded tasks
          (agenda "" ((org-agenda-span 8)
                      (org-agenda-start-day "-1d")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
          ;; PR-review requests
          (todo "TODO|WAIT" ((org-agenda-files '("~/org/review-pr.org"))
                      (org-agenda-overriding-header "PR's to review:")))
          (tags-todo "-PRIORITY=\"A\"-pr-toread" ((org-agenda-overriding-header "Other tasks:")))
          ))))

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
      (:prefix ("r" . "refile")
               "s" #'org-sort))
