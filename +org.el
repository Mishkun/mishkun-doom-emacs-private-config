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

;; Org-journal setup
(setq org-journal-file-format
      "%Y%m%d.org")

;; Org-agenda setup
(setq org-agenda-files '("~/org" "~/org/projects"))
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

;; keybindings
(map! :leader
      (:prefix "o"
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))
        :desc "Org-brain" "B" #'org-brain-visualize)
     (:prefix "n"
        ;; remove annoying deft keybinding
        ;; TODO: remove this after https://github.com/hlissner/doom-emacs/pull/2390 is accepted to the upstream
       "d" nil
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))
       )
)
