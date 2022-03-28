;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; Org configuration

(setq org-directory "~/org/")
(setq org-archive-location "~/org/archive/%s_archive::")
;; Mark the completion time of TODOs
(setq org-log-done 'time)
;; and also log time
(setq org-log-done-with-time t)
(setq org-log-note-clock-out t)

(use-package! org-duration
  :config
  (setq org-duration-units
        `(("min" . 1)
          ("h" . 60)
          ("d" . ,(* 60 8))
          ("w" . ,(* 60 8 5))
          ("m" . ,(* 60 8 5 4))
          ("y" . ,(* 60 8 5 4 11))))
  (org-duration-set-regexps))


;; add yandex-tracker link abbrev
(after! org
  (pushnew! org-link-abbrev-alist
            '("st" . "https://st.yandex-team.ru/%s"))
  (pushnew! org-link-abbrev-alist
            '("pr" . "https://github.com/YandexClassifieds/mobile-autoru-client-android/pull/%s"))
  (pushnew! org-link-abbrev-alist
            '("download" . "file://~/org/.attach/%s"))
(pushnew! org-link-abbrev-alist
            '("tg" . "https://t.me/%s"))
  (setq org-todo-keywords
        '((sequence  "TODO(t)" "MAYBE(m)" "PROJ(p)" "NEXT(n)" "WAIT(w)"  "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
(setq org-capture-templates
      '(("z" "my task" entry (file+headline "~/org/inbox.org" "Inbox") "* TODO %? %^G" :prepend t)
        ("m" "maybe" entry (file+headline "~/org/inbox.org" "Inbox") "* MAYBE %? %^G" :prepend t))))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?" :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n
* TODO Распланировать день
SCHEDULED: <%<%Y-%m-%d %a>>
- [ ] Перенести заметки из apple notes в дневной листинг
- [ ] Проверить актуальность проектов
- [ ] Прочитать всю почту
- [ ] Закрыть задачи на вчера
- [ ] Запланировать задачи на день и их effort
"))))

;; Roam setup
(setq org-roam-directory "~/org/roam")
(setq org-roam-index-file "~/org/roam/index.org")
(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)
        ("r" "ref" plain
":ROAM_REFS: %^{ref}
:END:
#+title: ${title} by %^ref
%?"
         :if-new (file "%<%Y%m%d%H%M%S>-${slug}.org") :unnarrowed t)))


(defun open-roam-index ()
  (interactive)
  (find-file  (concat (file-name-as-directory org-roam-directory) "index.org")))

;; Add roam keybinding to open index file
(map! :map org-mode-map
      :localleader
      (:prefix ("m" . "roam")
       "x" #'open-roam-index))


;; add intelij-like items swap
(map! :map org-mode-map
      "s-<up>" #'org-move-subtree-up
      "s-<down>" #'org-move-subtree-down)

;; disable annoying Table of Content on export
(setq org-export-with-toc nil)

;; enable auto-fill in org-mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Org-agenda setup

;; Use org-ql
(use-package! org-ql)
(setq org-agenda-files '("~/org" "~/org/projects" "~/org/roam" "~/org/roam/daily"))

;; Add my custom dashboard
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                (todo . " %i %-12:c %-6e")
                                (tags . " %i %-12:c")
                                (search . " %i %-12:c")))

(setq org-agenda-custom-commands
      '(("A" "Today's agenda"
         ((agenda "" ((org-agenda-span 2)
                      (org-agenda-entry-types '(:deadline :scheduled))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-start-day "0d")))))
        ("B" "Backlog mode"
         ((agenda "" ((org-agenda-span 8)))
          (org-ql-block '(and (todo "WAIT")
                              (not (scheduled)))
                        ((org-ql-block-header "Tasks waiting:")))
          (org-ql-block '(or (and (ancestors (todo "PROJ"))
                                  (todo "NEXT"))
                             (and (todo "PROJ")
                                  (descendants (or (todo "NEXT" "WAIT")
                                                    (scheduled)))))
                        ((org-ql-block-header "Next tasks in projects:")))
          (org-ql-block '(and (todo "NEXT" "TODO")
                              (not (scheduled))
                              (not (ancestors (todo))))
                        ((org-ql-block-header "Next tasks:")))))
        ("W" "Review Mode"
         ((agenda "" ((org-agenda-span 8)))
          (org-ql-block '(todo "WAIT")
                        ((org-ql-block-header "Tasks waiting:")))
          (org-ql-block '(or (and (todo "PROJ")
                                  (not (descendants (or (todo "NEXT")
                                                        (scheduled)))))
                             (and (todo "WAIT")
                                  (not (scheduled))
                                  (ancestors (todo "PROJ"))))
                        ((org-ql-block-header "Stuck projects:")))
          (org-ql-block '(and (todo "TODO")
                              (not (ancestors (todo)))
                              (not (effort)))
                        ((org-ql-block-header "Wait for estimate:")))
          (org-ql-block '(and (todo "PROJ")
                              (not (ancestors (todo))))
                        ((org-ql-block-header "Projects:")))
          (org-ql-block '(and (todo "MAYBE")
                              (not (ancestors (todo))))
                        ((org-ql-block-header "MAYBE?")))))))


;; deft setup
(setq deft-directory "~/org/deft")

;; calendar
;;
(defun my-open-calendar ()
  "opens my calendar window"
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    )))
;; First day of the week
(setq calendar-week-start-day 1)
;;

;; org-babel timing code blocks
(progn
  (defun gjg/time-call (time-call &rest args)
    (let ((start-time (float-time))
          (result (apply time-call args)))
      (message "Function call took %f seconds" (- (float-time) start-time))
      result))
  (advice-add 'org-babel-execute-src-block :around #'gjg/time-call))

;; archive file
(defun org-archive-file ()
  (interactive)
  (let* ((current-file-name (buffer-name (buffer-base-buffer)))
         (pattern (car (split-string org-archive-location ":")))
         (new-file-name (format pattern current-file-name)))
  (doom/move-this-file new-file-name)))


;; wordcount
(defun count-words-in-org-subtree ()
  (interactive)
  (save-excursion
    (org-mark-subtree) ;mark the whole subtre
    (forward-line 1)
    (let ((nchars (- (mark) (point))))
      (deactivate-mark) ;clear the region
      (message "%d" nchars))))

;; keybindings
(map! :leader
      (:prefix "o"
        :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O")))
     (:prefix "n"
      :desc "Org agenda overview" "A" (lambda () "Open my configured agenda view" (interactive) (org-agenda nil "O"))
      (:prefix "r"
       :desc "Roam index" "x" #'open-roam-index)))


(map! :map org-mode-map
      :localleader
      ;; Add sort to kb
      (:prefix ("r" . "refile")
       "s" #'org-sort)
      (:prefix "s" "c" #'count-words-in-org-subtree)
      "A" #'org-archive-file
      ;; rebind m c c to toggle clock instead of canceling
      (:prefix ("c" . "clock")
       "c" #'+org/toggle-clock
       "C" #'org-clock-cancel))
