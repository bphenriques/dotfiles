;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Bruno Henriques"
      user-mail-address "john@doe.com")

;; Theming
(load! "+utils")
(load! "+theme")
(load! "+macos")
(load! "org-capture")


(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t)                      ; By default while in insert all changes are one big blob. Be more granular


;
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)
