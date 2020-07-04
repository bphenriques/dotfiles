;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Bruno Henriques"
      user-mail-address "4727729+bphenriques@users.noreply.github.com")

;; Theming
(load! "+utils")
(load! "+theme")
(load! "+macos")
(load! "+org-capture")
(load! "+org-roam")

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t)                      ; By default while in insert all changes are one big blob. Be more granular
