;;; +gpg.el -*- lexical-binding: t; -*-

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Mikhail Levchenko"
      user-mail-address "mishkun.lev@gmail.com")

;; FIXME this is before I figure out how to use .gpg
(add-to-list 'auth-sources '"~/.authinfo")
