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

;; org-roam setup
(use-package! org-roam
      :after org
      :hook (org-mode . org-roam-mode)
      :custom
      (org-roam-directory "~/org/deft")
      (org-roam-link-representation 'title))


;; This is copypasta from the org-roam to resolve issues with async building of
;; caches on small databases
(defun org-roam-build-sync ()
  (interactive)
      (setq org-roam-cache (let ((backward-links (make-hash-table :test #'equal))
            (forward-links (make-hash-table :test #'equal))
            (file-titles (make-hash-table :test #'equal)))
        (cl-labels ((org-roam--find-files
                     (dir)
                     (if (file-exists-p dir)
                         (let ((files (directory-files dir t "." t))
                               (dir-ignore-regexp (concat "\\(?:"
                                                          "\\."
                                                          "\\|\\.\\."
                                                          "\\)$"))
                               result)
                           (dolist (file files)
                             (cond
                              ((file-directory-p file)
                               (when (not (string-match dir-ignore-regexp file))
                                 (setq result (append (org-roam--find-files file) result))))
                              ((and (file-readable-p file)
                                    (string= (file-name-extension file) "org"))
                               (setq result (cons (file-truename file) result)))))
                           result)))
                    (org-roam--parse-content
                     (file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (with-current-buffer (current-buffer)
                         (org-element-map (org-element-parse-buffer) 'link
                           (lambda (link)
                             (let ((type (org-element-property :type link))
                                   (path (org-element-property :path link))
                                   (start (org-element-property :begin link)))
                               (when (and (string= type "file")
                                          (string= (file-name-extension path) "org"))
                                 (goto-char start)
                                 (let* ((element (org-element-at-point))
                                        (content (or (org-element-property :raw-value element)
                                                     (buffer-substring
                                                      (or (org-element-property :content-begin element)
                                                          (org-element-property :begin element))
                                                      (or (org-element-property :content-end element)
                                                          (org-element-property :end element))))))
                                   (list :from file
                                         :to (file-truename (expand-file-name path org-roam-directory))
                                         :content (string-trim content))))))))))
                    (org-roam--process-items
                     (items)
                     (mapcar
                      (lambda (item)
                        (pcase-let ((`(:from ,p-from :to ,p-to :content ,content) item))
                          ;; Build forward-links
                          (let ((links (gethash p-from forward-links)))
                            (if links
                                (puthash p-from
                                         (if (member p-to links)
                                             links
                                           (cons p-to links)) forward-links)
                              (puthash p-from (list p-to) forward-links)))
                          ;; Build backward-links
                          (let ((contents-hash (gethash p-to backward-links)))
                            (if contents-hash
                                (if-let ((contents-list (gethash p-from contents-hash)))
                                    (let ((updated (cons content contents-list)))
                                      (puthash p-from updated contents-hash)
                                      (puthash p-to contents-hash backward-links))
                                  (progn
                                    (puthash p-from (list content) contents-hash)
                                    (puthash p-to contents-hash backward-links)))
                              (let ((contents-hash (make-hash-table :test #'equal)))
                                (puthash p-from (list content) contents-hash)
                                (puthash p-to contents-hash backward-links))))))
                      items))
                    (org-roam--extract-title
                     (buffer)
                     (with-current-buffer buffer
                       (org-element-map
                           (org-element-parse-buffer)
                           'keyword
                         (lambda (kw)
                           (when (string= (org-element-property :key kw) "TITLE")
                             (org-element-property :value kw)))
                         :first-match t))))
          (let ((org-roam-files (org-roam--find-files org-roam-directory)))
            (mapcar #'org-roam--process-items
                    (mapcar #'org-roam--parse-content org-roam-files))
            (mapcar (lambda (file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (when-let ((title (org-roam--extract-title (current-buffer))))
                          (puthash file title file-titles))))
                    org-roam-files)))
        (list
         :forward forward-links
         :backward backward-links
         :titles file-titles)))
     (message "Org-roam cache built!"))

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
