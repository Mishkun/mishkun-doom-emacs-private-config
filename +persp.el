;;; +persp.el -*- lexical-binding: t; -*-

(setq agenda-workspaces '("agenda" "zettelkasten"))

(defun workspace-open-agenda-setup ()
    "open or select a frame with agenda combo"
  (interactive)
  (condition-case nil (mapc #'+workspace-load agenda-workspaces) (error nil))
  ;; Close other workspaces
  ;; (when (not (seq-contains-p #'(lambda (x) (= x +workspace-current-name)) agenda-workspaces))
  ;;   (+workspace-delete (+workspace-current-name)))
  (+workspace-switch (car agenda-workspaces)))
