;;; +publish.el -*- lexical-binding: t; -*-

(use-package! org-element)

(defun sitemap-entry (entry style project)
  (unless (directory-name-p entry)
    (let ((abstract (car (let ((filename (org-publish--expand-file-name entry project)))
                           (with-temp-buffer
                             (insert-file-contents-literally filename)
                             (org-element-map (org-element-parse-buffer) 'special-block
                               (lambda (block)
                                 (let ((name (org-element-property :type block))
                                       (begin (org-element-property :contents-begin block))
                                       (end (org-element-property :contents-end block)))
                                   (pcase name
                                     ("abstract" (buffer-substring-no-properties begin end))
                                     (_ nil)))))))))
          (title (org-publish-find-title entry project)))
      (cond
       (abstract (format "[[file:%s][%s]] :: %s" entry title abstract))
       (t (format "[[file:%s][%s]]" entry title)))
      )))

(let* ((project-base-dir (or (getenv "BLOG_ROOT") "/Users/themishkun/projects/tinkering/themishkun-blog"))
       (project-publish-dir (concat project-base-dir "/pages")))
  (setq org-publish-project-alist
        `(("blog"
           :base-directory ,project-base-dir
           :publishing-directory ,project-publish-dir
           :base-extension "org"
           :recursive t
           :auto-sitemap t
           :sitemap-format-entry sitemap-entry
           :sitemap-sort-files chronologically
           :sitemap-style list
           :section-numbers nil
           :with-author nil
           :with-date nil
           :with-toc nil
           :html-head "<link id=\"pagestyle\" rel=\"stylesheet\" type=\"text/css\" href=\"../attachments/org.css\"/>"
           :html-preamble auto
           :html-postamble t
           :html-postamble-format (("en" "<a class=\"telegram-button button\" href=\"https://t.me/izpodshtorki\" target=\"_blank\"><i></i><span>Подпишись и обсуждай в Telegram</span></a>")))

          ("attachments"
           :base-directory ,(concat project-base-dir "/attachments")
           :publishing-directory ,(concat project-publish-dir "/attachments")
           :publishing-function org-publish-attachment
           :base-extension ".*"
           :recursive t)
          ("website" :components ("blog" "attachments")))))
