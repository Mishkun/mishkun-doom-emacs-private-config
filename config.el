;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Mikhail Levchenko"
      user-mail-address "mishkun.lev@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-unicode-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Helvetica"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;
;; MY CONFIG
;;

;; Startup as fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set Russian imput method by default
(load! "+russian.el")

(load! "+org.el")

(setq lsp-haskell-process-path-hie "hie")

;; Add vimish fast scroll to ivy
(map! :after ivy :map ivy-minibuffer-map
      "C-u" #'ivy-scroll-down-command
      "C-d" #'ivy-scroll-up-command)

;; TAB in counsel buffer faster
(map! :after counsel :map counsel-find-file-map
      "RET" #'ivy-alt-done)

;; My keyboard has combined SPC and Super in one key, so I keep closing windows
;; with Super-W instead of moving them with SPC w l
(map! :g "s-w" nil)

;; FIXME this is before I figure out how to use .gpg
(add-to-list 'auth-sources '"~/.authinfo")

;; Plantuml executable configuration to brew
;;(setq plantuml-executable-path "/usr/local/bin/plantuml")
;;(setq plantuml-default-exec-mode 'executable)
