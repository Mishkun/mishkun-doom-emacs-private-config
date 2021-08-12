;;; +appearance.el -*- lexical-binding: t; -*-

;; Fonts setup
(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-unicode-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Helvetica"))

;; Color theme choose
(setq doom-theme 'doom-one)

;; Absolute line numbers
(setq display-line-numbers-type t)

;; Startup as fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
