;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
(load! "+gpg.el")
(load! "+russian.el")
(load! "+org.el")
(load! "+appearance.el")
(load! "+magit.el")
(load! "+mappings.el")
(load! "+persp.el")
(load! "+publish.el")

(use-package! protobuf-mode
  :defer-incrementally t)
