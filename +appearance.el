;;; +appearance.el -*- lexical-binding: t; -*-

;; Fonts setup
(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-unicode-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Helvetica"))

;; Color theme choose
(setq doom-theme 'doom-one-light)

;; Absolute line numbers
(setq display-line-numbers-type t)

;; Startup as fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(after! writeroom-mode
  (progn
    ;; disable mixed-pitch and all of the changes to the font altogether
    (setq +zen-mixed-pitch-modes '())
    (setq +zen-text-scale 0)
    (setq writeroom-width 110)
    (message "Zen loaded with %s %s" +zen-mixed-pitch-modes +zen-text-scale)
    ))
